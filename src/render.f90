module fortrat_render
  use iso_fortran_env, only: real64, output_unit
  use fortrat_types
  implicit none

  integer, parameter :: MAX_COLS = 300
  integer, parameter :: MAX_ROWS = 100

  type :: cell_t
    character(len=1) :: ch    = ' '
    character(len=16):: color = ''
    logical          :: bold  = .false.
  end type

  type(cell_t), save :: cur_frame(MAX_COLS, MAX_ROWS)
  type(cell_t), save :: prv_frame(MAX_COLS, MAX_ROWS)
  logical,      save :: first_frame = .true.

contains

  subroutine render_reset_frame()
    first_frame = .true.
  end subroutine

  subroutine render_clear(w, h)
    integer, intent(in) :: w, h
    integer :: c, r
    do r = 1, h
      do c = 1, w
        cur_frame(c,r)%ch    = ' '
        cur_frame(c,r)%color = ''
        cur_frame(c,r)%bold  = .false.
      end do
    end do
  end subroutine

  subroutine render_set(c, r, ch, color, bold, w, h)
    integer,          intent(in) :: c, r, w, h
    character(len=1), intent(in) :: ch
    character(len=*), intent(in) :: color
    logical,          intent(in) :: bold
    if (c < 1 .or. c > w .or. r < 1 .or. r > h) return
    ! Don't overwrite bold (node) cells with edge chars
    if (cur_frame(c,r)%bold .and. .not. bold) return
    cur_frame(c,r)%ch    = ch
    cur_frame(c,r)%color = trim(color)
    cur_frame(c,r)%bold  = bold
  end subroutine

  ! Bresenham line — draw edge between two node positions
  subroutine render_edge(x0, y0, x1, y1, color, w, h)
    integer,          intent(in) :: x0, y0, x1, y1, w, h
    character(len=*), intent(in) :: color
    integer      :: cx, cy, dx, dy, sx, sy, err, e2
    character(len=1) :: ch

    dx  =  abs(x1-x0);  sx = merge(1, -1, x0 < x1)
    dy  = -abs(y1-y0);  sy = merge(1, -1, y0 < y1)
    err = dx + dy
    cx  = x0;  cy = y0

    do
      ! Pick character based on local slope
      if (dx == 0 .or. abs(dy) > abs(dx)*2) then
        ch = '|'
      else if (dy == 0 .or. abs(dx) > abs(dy)*2) then
        ch = '-'
      else if ((sx > 0 .and. sy > 0) .or. (sx < 0 .and. sy < 0)) then
        ch = '\'
      else
        ch = '/'
      end if
      ! Skip cells too close to endpoints (node area)
      if (.not. (cx == x0 .and. cy == y0) .and. &
          .not. (cx == x1 .and. cy == y1)) then
        call render_set(cx, cy, ch, color, .false., w, h)
      end if
      if (cx == x1 .and. cy == y1) exit
      e2 = 2 * err
      if (e2 >= dy) then; err = err + dy; cx = cx + sx; end if
      if (e2 <= dx) then; err = err + dx; cy = cy + sy; end if
    end do
  end subroutine

  ! Draw a node sigil at position
  subroutine render_node(cx, cy, lbl, color, selected, w, h)
    integer,          intent(in) :: cx, cy, w, h
    character(len=*), intent(in) :: lbl, color
    logical,          intent(in) :: selected
    integer :: i, llen, start_col
    character(len=12) :: display
    llen = min(len_trim(lbl), 10)  ! cap at 10 chars
    if (selected) then
      display = '['//lbl(1:llen)//']'
      llen = llen + 2
    else
      display = lbl(1:llen)
    end if
    ! Centre the label on cx
    start_col = cx - llen/2
    do i = 1, llen
      call render_set(start_col + i - 1, cy, display(i:i), color, selected, w, h)
    end do
  end subroutine

  ! Draw text string at position (truncated to fit)
  subroutine render_text(c, r, text, color, bold, w, h)
    integer,          intent(in) :: c, r, w, h
    character(len=*), intent(in) :: text, color
    logical,          intent(in) :: bold
    integer :: i, len_t
    len_t = min(len_trim(text), w - c + 1)
    do i = 1, len_t
      call render_set(c + i - 1, r, text(i:i), color, bold, w, h)
    end do
  end subroutine

  ! Flush frame — build entire output as one buffer, write atomically (no flicker)
  subroutine render_flush(w, h)
    use fortrat_tui, only: fortrat_write_buf, fortrat_flush
    use iso_c_binding
    integer, intent(in) :: w, h
    ! Frame buffer: enough for clear + all cells
    ! Each cell: ~20 bytes (cursor pos + color + char + reset)
    ! Max cells: 300*100 = 30000 * 20 = 600KB — use allocatable
    character(len=1), allocatable :: fbuf(:)
    integer :: fbuf_size, fpos
    integer :: c, r, cl
    character(len=32) :: pos_seq
    integer :: pos_len

    fbuf_size = 9 + w * h * 16   ! home seq + per cell: color+bold+char+reset ~16 bytes
    allocate(fbuf(fbuf_size))
    fpos = 0

    ! Start with: hide cursor, home cursor only (no clear — prevents scroll)
    call fbuf_append(fbuf, fpos, char(27)//'[?25l', 6)  ! hide cursor
    call fbuf_append(fbuf, fpos, char(27)//'[H',    3)  ! cursor home row 1 col 1

    ! Write all cells — including spaces — so previous frame is fully overwritten
    do r = 1, h
      ! Move to start of row
      write(pos_seq, '(a,i0,a,i0,a)') char(27)//'[', r, ';1H'
      pos_len = len_trim(pos_seq)
      call fbuf_append(fbuf, fpos, pos_seq(1:pos_len), pos_len)

      do c = 1, w
        ! Color
        if (len_trim(cur_frame(c,r)%color) > 0) then
          cl = len_trim(cur_frame(c,r)%color)
          call fbuf_append(fbuf, fpos, cur_frame(c,r)%color(1:cl), cl)
        else
          call fbuf_append(fbuf, fpos, char(27)//'[0m', 4)
        end if
        ! Bold
        if (cur_frame(c,r)%bold) then
          call fbuf_append(fbuf, fpos, char(27)//'[1m', 4)
        end if
        ! Character
        call fbuf_append(fbuf, fpos, cur_frame(c,r)%ch, 1)
        prv_frame(c,r) = cur_frame(c,r)
        if (fpos > fbuf_size - 64) exit
      end do
      ! Reset at end of row
      call fbuf_append(fbuf, fpos, char(27)//'[0m', 4)
      if (fpos > fbuf_size - 64) exit
    end do

    ! Single write call — atomic, no flicker
    call fortrat_write_buf(fbuf, int(fpos, c_int))
    call fortrat_flush()
    deallocate(fbuf)
    first_frame = .false.
  end subroutine

  subroutine fbuf_append(buf, pos, str, n)
    character(len=1), intent(inout) :: buf(:)
    integer,          intent(inout) :: pos
    character(len=*), intent(in)    :: str
    integer,          intent(in)    :: n
    integer :: i
    do i = 1, n
      pos = pos + 1
      if (pos <= size(buf)) buf(pos) = str(i:i)
    end do
  end subroutine

  ! ── Main graph pane render ──
  subroutine render_graph_pane(graph, state, w, h, row_off)
    type(lex_graph_t), intent(in) :: graph
    type(app_state_t), intent(in) :: state
    integer,           intent(in) :: w, h, row_off
    integer      :: i, cx, cy, vis_idx
    character(len=16) :: col
    logical      :: selected, dimmed
    integer      :: visible_nodes(MAX_NODES), n_vis
    ! Build visible node index list
    n_vis = 0
    do i = 1, graph%n_nodes
      if (graph%nodes(i)%active) then
        n_vis = n_vis + 1
        visible_nodes(n_vis) = i
      end if
    end do

    ! Draw edges first
    do i = 1, graph%n_edges
      if (graph%edges(i)%src == 0) cycle
      if (.not. graph%nodes(graph%edges(i)%src)%active) cycle
      if (.not. graph%nodes(graph%edges(i)%tgt)%active) cycle
      cx = nint(graph%nodes(graph%edges(i)%src)%x)
      cy = nint(graph%nodes(graph%edges(i)%src)%y) + row_off
      call render_edge( &
        cx, cy, &
        nint(graph%nodes(graph%edges(i)%tgt)%x), &
        nint(graph%nodes(graph%edges(i)%tgt)%y) + row_off, &
        GREEN_DIM, w, h)
    end do

    ! Draw nodes
    vis_idx = 0
    do i = 1, graph%n_nodes
      if (.not. graph%nodes(i)%active) cycle
      vis_idx = vis_idx + 1
      cx = nint(graph%nodes(i)%x)
      cy = nint(graph%nodes(i)%y) + row_off
      col   = ns_color(graph%nodes(i)%ns_group)
      selected = (vis_idx == state%cursor_idx .or. i == state%selected_idx)
      dimmed   = len_trim(state%search_query) > 0 .and. &
                 index(graph%nodes(i)%id, trim(state%search_query)) == 0
      if (dimmed) col = GREEN_DIM
      call render_node(cx, cy, trim(graph%nodes(i)%label), col, selected, w, h)
    end do
  end subroutine

  ! ── Inspect pane ──
  subroutine render_inspect_pane(graph, state, col_off, w, h)
    type(lex_graph_t), intent(in) :: graph
    type(app_state_t), intent(in) :: state
    integer,           intent(in) :: col_off, w, h
    integer           :: row, i, idx, vis_idx, n_out, pane_w
    character(len=ID_LEN)   :: out_ids(64)
    character(len=3)  :: sigil

    pane_w = w - col_off
    row = 2

    idx = 0
    vis_idx = 0
    do i = 1, graph%n_nodes
      if (.not. graph%nodes(i)%active) cycle
      vis_idx = vis_idx + 1
      if (vis_idx == state%cursor_idx) then; idx = i; exit; end if
    end do
    if (state%selected_idx > 0) idx = state%selected_idx

    if (idx == 0) then
      call render_text(col_off, row,   'C     NO NODE SELECTED', GREEN_DIM, .false., w, h)
      call render_text(col_off, row+2, '      HJKL  : navigate', GREEN_DIM, .false., w, h)
      call render_text(col_off, row+3, '      ENTER : inspect',  GREEN_DIM, .false., w, h)
      call render_text(col_off, row+4, '      /     : search',   GREEN_DIM, .false., w, h)
      call render_text(col_off, row+5, '      TAB   : toggle ns',GREEN_DIM, .false., w, h)
      call render_text(col_off, row+6, '      C     : community',GREEN_DIM, .false., w, h)
      call render_text(col_off, row+7, '      Q     : quit',     GREEN_DIM, .false., w, h)
      return
    end if

    sigil = KIND_SIGIL(graph%nodes(idx)%kind)(1:3)
    call render_text(col_off, row, 'SUBROUTINE INSPECT('//sigil//')', GREEN_BR, .true., w, h)
    row = row + 1
    call render_text(col_off, row, repeat('-', pane_w), GREEN_DIM, .false., w, h)
    row = row + 1
    call render_text(col_off, row, trim(graph%nodes(idx)%id), GREEN_BR, .true., w, h)
    row = row + 2

    call render_text(col_off, row, 'COMMON /LEXDATA/', GREEN_DIM, .false., w, h)
    row = row + 1
    call render_text(col_off, row, '  KIND     '//sigil, GREEN, .false., w, h)
    row = row + 1
    call render_text(col_off, row, '  NS_GROUP '//ns_name(graph%nodes(idx)%ns_group), GREEN, .false., w, h)
    row = row + 1
    if (graph%nodes(idx)%ns_group == NS_COMMUNITY) then
      call render_text(col_off, row, '  ORIGIN   EXTERNAL', YELLOW, .false., w, h)
      row = row + 1
    end if
    row = row + 1

    ! Description with word-wrap
    if (len_trim(graph%nodes(idx)%doc) > 0 .and. row < h - 3) then
      block
        integer  :: dlen, lw, pos, npos
        character(len=DOC_LEN) :: doc
        doc  = trim(graph%nodes(idx)%doc)
        dlen = len_trim(doc)
        lw   = pane_w - 6
        pos  = 1
        call render_text(col_off, row, 'C     DESCRIPTION', GREEN_DIM, .false., w, h)
        row = row + 1
        do while (pos <= dlen .and. row < h - 3)
          npos = min(pos + lw - 1, dlen)
          call render_text(col_off, row, 'C     '//doc(pos:npos), GREEN_DIM, .false., w, h)
          pos = npos + 1
          row = row + 1
        end do
        row = row + 1
      end block
    end if

    ! Fields
    if (graph%nodes(idx)%n_fields > 0 .and. row < h - 3) then
      call render_text(col_off, row, 'C     FIELDS', GREEN_DIM, .false., w, h)
      row = row + 1
      do i = 1, min(graph%nodes(idx)%n_fields, 8)
        if (graph%nodes(idx)%fields(i)%required) then
          call render_text(col_off, row, '  '//trim(graph%nodes(idx)%fields(i)%name), GREEN_BR, .false., w, h)
        else
          call render_text(col_off, row, 'C '//trim(graph%nodes(idx)%fields(i)%name), GREEN, .false., w, h)
        end if
        row = row + 1
        if (row >= h - 2) exit
      end do
      row = row + 1
    end if

    ! Outbound refs
    if (row < h - 4) then
      n_out = 0
      do i = 1, graph%n_edges
        if (graph%edges(i)%src == idx) then
          n_out = n_out + 1
          if (n_out <= 4) out_ids(n_out) = graph%nodes(graph%edges(i)%tgt)%id
        end if
      end do
      if (n_out > 0) then
        call render_text(col_off, row, 'C     CALL/REF', GREEN_DIM, .false., w, h)
        row = row + 1
        do i = 1, min(n_out, 4)
          call render_text(col_off, row, '  CALL '//trim(out_ids(i)), GREEN, .false., w, h)
          row = row + 1
          if (row >= h - 2) exit
        end do
      end if
    end if

    call render_text(col_off, h-1, 'END SUBROUTINE INSPECT', GREEN_DIM, .false., w, h)
  end subroutine

  ! ── Column ruler ──
  subroutine render_ruler(w, h)
    integer, intent(in) :: w, h
    character(len=80) :: ruler
    ruler = 'C23456789012345678901234567890123456789012345678901234567890123456789072'
    call render_text(1, 1, ruler(1:min(w,72)), GREEN_DIM, .false., w, h)
  end subroutine

  ! ── Pane header bar ──
  subroutine render_header(w, h, divider_col)
    integer,           intent(in) :: w, h, divider_col
    integer           :: i
    character(len=w)  :: line
    character(len=32) :: left_hdr, right_hdr

    line = repeat('-', w)
    left_hdr  = '-[FORTRAT:GRAPH]'
    right_hdr = '-[INSPECT]'
    line(1:len_trim(left_hdr))   = left_hdr
    line(divider_col:divider_col) = '+'
    line(divider_col+1:divider_col+len_trim(right_hdr)) = right_hdr(1:len_trim(right_hdr))

    call render_text(1, 2, line, GREEN_DIM, .false., w, h)

    ! Vertical divider
    do i = 3, h - 1
      call render_set(divider_col, i, '|', GREEN_DIM, .false., w, h)
    end do
  end subroutine

  ! ── Status bar ──
  subroutine render_status(state, graph, w, h)
    type(app_state_t), intent(in) :: state
    type(lex_graph_t), intent(in) :: graph
    integer,           intent(in) :: w, h
    character(len=w)  :: bar
    character(len=32) :: left_part, right_part, mode_str
    character(len=16) :: n_str, e_str
    integer           :: n_vis, n_edg, i

    select case(state%mode)
    case(MODE_GRAPH);   mode_str = '[GRAPH]'
    case(MODE_SEARCH);  mode_str = '[SEARCH]'
    case(MODE_INSPECT); mode_str = '[INSPECT]'
    case(MODE_LOADING); mode_str = '[LOADING]'
    case default;       mode_str = '[?]'
    end select

    n_vis = 0
    do i = 1, graph%n_nodes
      if (graph%nodes(i)%active) n_vis = n_vis + 1
    end do
    n_edg = graph%n_edges

    write(n_str, '(i0)') n_vis
    write(e_str, '(i0)') n_edg

    left_part  = ' FORTRAT '//trim(mode_str)//' '//trim(state%status_msg)
    right_part = ' N='//trim(n_str)//' E='//trim(e_str)//' '

    bar = repeat(' ', w)
    bar(1:min(len_trim(left_part),w)) = left_part(1:min(len_trim(left_part),w))
    bar(w-len_trim(right_part)+1:w)   = right_part

    call render_text(1, h, bar, GREEN, .true., w, h)
  end subroutine

  ! ── Loading screen ──
  subroutine render_loading(state, w, h)
    type(app_state_t), intent(in) :: state
    integer,           intent(in) :: w, h
    integer :: r

    r = 2
    call render_text(1, r, 'C23456789012345678901234567890123456789012345678901234567890123456789072', GREEN_DIM, .false., w, h)
    r = r + 2
    call render_text(7, r, 'FORTRAT V1.0  (FORMERLAB 2026)', GREEN_BR, .true., w, h)
    r = r + 1
    call render_text(7, r, 'COPYRIGHT (C) FORMERLAB. ALL RIGHTS RESERVED.', GREEN_DIM, .false., w, h)
    r = r + 2
    call render_text(7, r, 'IMPLICIT NONE',                            GREEN, .false., w, h); r=r+1
    call render_text(7, r, 'CHARACTER*(256) QUERY',                    GREEN, .false., w, h); r=r+1
    call render_text(7, r, 'INTEGER N_NODES, N_EDGES',                 GREEN, .false., w, h); r=r+1
    call render_text(7, r, 'LOGICAL COMMUNITY_FLAG',                   GREEN, .false., w, h); r=r+1
    call render_text(7, r, 'DATA COMMUNITY_FLAG /.FALSE./',            GREEN, .false., w, h); r=r+2
    call render_text(7, r, 'CALL FETCH_LEXICONS(ATPROTO_REPO, GRAPH)', GREEN, .false., w, h); r=r+2
    call render_text(7, r, 'C     STATUS:',                            GREEN_DIM, .false., w, h); r=r+1
    call render_text(7, r, '>> '//trim(state%progress),               GREEN_BR, .false., w, h)
  end subroutine

  ! Helper
  function ns_name(ns) result(s)
    integer, intent(in) :: ns
    character(len=20)   :: s
    select case(ns)
    case(NS_APP_BSKY);    s = 'app.bsky'
    case(NS_COM_ATPROTO); s = 'com.atproto'
    case(NS_CHAT_BSKY);   s = 'chat.bsky'
    case(NS_TOOLS_OZONE); s = 'tools.ozone'
    case default;         s = 'community'
    end select
  end function

end module fortrat_render
