program fortrat
  use iso_fortran_env, only: real64, output_unit
  use fortrat_types
  use fortrat_tui
  use fortrat_simulate
  use fortrat_render
  use fortrat_lexparse
  implicit none

  type(lex_graph_t) :: graph
  type(app_state_t) :: state
  integer           :: key, cols, rows, frame_skip
  logical           :: dirty

  ! ── Init state ──
  state%groups      = .true.     ! all ns groups visible
  state%groups(NS_COMMUNITY) = .false.
  state%mode        = MODE_LOADING
  state%status_msg  = 'READY'

  ! ── Get terminal size ──
  call tui_get_term_size(cols, rows)
  state%term_w   = max(cols, 80)
  state%term_h   = max(rows, 24)
  state%inspect_w = 42
  state%graph_w   = state%term_w - state%inspect_w - 1
  state%graph_h   = state%term_h - 3

  ! ── Fetch and build graph (plain text output, no raw mode yet) ──
  call build_graph(graph, state%community, show_progress)

  ! ── Init simulation ──
  write(*, '(a)') 'Settling simulation...'
  flush(output_unit)
  call apply_filters(graph, state%groups)
  call sim_init(graph, state%graph_w, state%graph_h)
  call sim_prewarm(graph, state%graph_w, state%graph_h, 40)
  write(*, '(a)') 'Done. Starting TUI...'
  flush(output_unit)

  ! ── Now enter raw mode and take over the screen ──
  call tui_enter_raw_mode()
  call render_reset_frame()

  state%mode     = MODE_GRAPH
  state%status_msg = 'READY'
  frame_skip = 0

  ! ── Main event loop ──
  do while (.true.)
    dirty = .false.

    ! ── Simulation tick (only while alpha > 0) ──
    if (alpha > 0.0d0) then
      call sim_tick(graph, state%graph_w, state%graph_h)
      dirty = .true.
    end if

    ! ── Read input (non-blocking) ──
    key = tui_read_key()
    if (key /= 0) then
      call handle_key(key, graph, state)
      dirty = .true.
    end if

    ! ── Render only when something changed, or every 8 frames as keepalive ──
    frame_skip = frame_skip + 1
    if (dirty .or. frame_skip >= 8) then
      frame_skip = 0
      call render_clear(state%term_w, state%term_h)
      call render_ruler(state%term_w, state%term_h)
      call render_header(state%term_w, state%term_h, state%graph_w + 1)
      call render_graph_pane(graph, state, state%graph_w, state%term_h, 2)
      call render_inspect_pane(graph, state, state%graph_w + 2, state%term_w, state%term_h)
      call render_status(state, graph, state%term_w, state%term_h)
      call render_flush(state%term_w, state%term_h)
    end if

    ! ── Frame sleep: faster while settling, slower when idle ──
    if (alpha > 0.0d0) then
      call usleep_f(100000)   ! 100ms while sim running (~10fps)
    else
      call usleep_f(50000)    ! 50ms when idle — responsive to keys
    end if
  end do

contains

  subroutine show_progress(msg)
    character(len=*), intent(in) :: msg
    state%progress = trim(msg)
    write(*, '(a)') trim(msg)
    flush(output_unit)
  end subroutine

  subroutine handle_key(key, graph, state)
    integer,           intent(in)    :: key
    type(lex_graph_t), intent(inout) :: graph
    type(app_state_t), intent(inout) :: state
    integer :: vis_count, vis_idx, i
    character(len=1) :: ch

    ! Count visible nodes
    vis_count = 0
    do i = 1, graph%n_nodes
      if (graph%nodes(i)%active) vis_count = vis_count + 1
    end do

    ch = achar(key)

    ! Quit
    if (ch == 'q' .or. ch == 'Q') then
      call tui_exit_raw_mode()
      stop
    end if

    ! Search mode: accumulate query
    if (state%mode == MODE_SEARCH) then
      if (key == 13) then  ! Enter
        state%mode = MODE_GRAPH
        state%status_msg = 'QUERY: '//trim(state%search_query)
      else if (key == 27) then  ! ESC
        state%search_query = ''
        state%mode = MODE_GRAPH
        state%status_msg = 'READY'
      else if (key == 127 .or. key == 8) then  ! Backspace
        i = len_trim(state%search_query)
        if (i > 0) state%search_query(i:i) = ' '
      else if (key >= 32 .and. key < 127) then
        i = len_trim(state%search_query) + 1
        if (i <= len(state%search_query)) then
          state%search_query(i:i) = ch
        end if
      end if
      return
    end if

    ! Navigation
    if (ch == 'j' .or. key == -2) then  ! down
      state%cursor_idx = min(state%cursor_idx + 1, max(vis_count, 1))
      state%status_msg = 'NAV'
    else if (ch == 'k' .or. key == -1) then  ! up
      state%cursor_idx = max(state%cursor_idx - 1, 1)
      state%status_msg = 'NAV'
    else if (ch == 'l' .or. key == -4) then  ! right/next
      state%cursor_idx = min(state%cursor_idx + 5, max(vis_count, 1))
    else if (ch == 'h' .or. key == -3) then  ! left/prev
      state%cursor_idx = max(state%cursor_idx - 5, 1)

    ! Select
    else if (key == 13) then  ! Enter
      if (state%cursor_idx > 0) then
        vis_idx = 0
        do i = 1, graph%n_nodes
          if (.not. graph%nodes(i)%active) cycle
          vis_idx = vis_idx + 1
          if (vis_idx == state%cursor_idx) then
            state%selected_idx = i
            state%mode = MODE_INSPECT
            state%status_msg = 'INSPECT: '//trim(graph%nodes(i)%id)
            exit
          end if
        end do
      end if

    ! Escape — deselect
    else if (key == 27) then
      state%selected_idx = 0
      state%mode = MODE_GRAPH
      state%status_msg = 'READY'

    ! Search
    else if (ch == '/') then
      state%search_query = ''
      state%mode = MODE_SEARCH
      state%status_msg = 'SEARCH — type, ENTER confirm, ESC cancel'

    ! Tab — cycle one ns group on/off
    else if (key == 9) then
      block
        integer :: g
        do g = 1, 5
          if (.not. state%groups(g)) then
            state%groups(g) = .true.
            write(state%status_msg, '(a,i0,a)') 'NS GROUP ', g, ' ON'
            go to 99
          end if
        end do
        state%groups(5) = .false.
        state%status_msg = 'COMMUNITY OFF'
        99 continue
      end block
      call apply_filters(graph, state%groups)
      call sim_init(graph, state%graph_w, state%graph_h)
      call sim_prewarm(graph, state%graph_w, state%graph_h, 40)

    ! c — toggle community
    else if (ch == 'c' .or. ch == 'C') then
      state%community = .not. state%community
      state%groups(NS_COMMUNITY) = state%community
      call apply_filters(graph, state%groups)
      write(state%status_msg, '(a)') merge('COMMUNITY ON ', 'COMMUNITY OFF', state%community)

    end if
  end subroutine

end program fortrat

! usleep wrapper via ISO C binding
subroutine usleep_f(microseconds)
  use iso_c_binding
  implicit none
  integer, intent(in) :: microseconds
  interface
    subroutine c_usleep(us) bind(c, name='usleep')
      import c_int
      integer(c_int), value :: us
    end subroutine
  end interface
  call c_usleep(int(microseconds, c_int))
end subroutine
