module fortrat_tui
  use iso_c_binding
  use iso_fortran_env, only: output_unit
  use fortrat_types
  implicit none

  logical, save :: raw_mode_active = .false.

  interface
    function fortrat_enter_raw() bind(c, name='fortrat_enter_raw') result(r)
      import c_int
      integer(c_int) :: r
    end function

    function fortrat_exit_raw() bind(c, name='fortrat_exit_raw') result(r)
      import c_int
      integer(c_int) :: r
    end function

    subroutine fortrat_winsize(cols, rows) bind(c, name='fortrat_winsize')
      import c_int
      integer(c_int), intent(out) :: cols, rows
    end subroutine

    subroutine fortrat_clear_screen() bind(c, name='fortrat_clear_screen')
    end subroutine

    subroutine fortrat_write_at(row, col, str, len) bind(c, name='fortrat_write_at')
      import c_int, c_char
      integer(c_int), value         :: row, col, len
      character(kind=c_char, len=1), intent(in) :: str(len)
    end subroutine

    subroutine fortrat_flush() bind(c, name='fortrat_flush')
    end subroutine

    function fortrat_read_key(buf) bind(c, name='fortrat_read_key') result(n)
      import c_int, c_char
      character(c_char), intent(out) :: buf(4)
      integer(c_int) :: n
    end function
  end interface

contains

  subroutine tui_enter_raw_mode()
    integer(c_int) :: rc
    rc = fortrat_enter_raw()
    raw_mode_active = .true.
    call fortrat_clear_screen()
  end subroutine

  subroutine tui_exit_raw_mode()
    integer(c_int) :: rc
    if (.not. raw_mode_active) return
    rc = fortrat_exit_raw()
    raw_mode_active = .false.
    write(*, '(a)', advance='no') SHOW_CUR // RESET
    write(*, *)
    flush(output_unit)
  end subroutine

  subroutine tui_get_term_size(cols, rows)
    integer, intent(out) :: cols, rows
    integer(c_int) :: c, r
    call fortrat_winsize(c, r)
    cols = int(c)
    rows = int(r)
    if (cols <= 0) cols = 120
    if (rows <= 0) rows = 40
  end subroutine

  ! Non-blocking key read. Returns:
  !   0        = nothing available
  !   1..127   = ASCII char
  !  -1        = up arrow
  !  -2        = down arrow
  !  -3        = left arrow
  !  -4        = right arrow
  !  -5        = ESC (bare)
  function tui_read_key() result(key)
    integer                :: key
    character(c_char)      :: buf(4)
    integer(c_int)         :: n

    n = fortrat_read_key(buf)
    if (n <= 0) then
      key = 0; return
    end if

    ! ESC sequence
    if (ichar(buf(1)) == 27 .and. n >= 3) then
      if (buf(2) == '[') then
        select case(buf(3))
        case('A'); key = -1   ! up
        case('B'); key = -2   ! down
        case('C'); key = -4   ! right
        case('D'); key = -3   ! left
        case default; key = -5
        end select
        return
      end if
    end if
    if (ichar(buf(1)) == 27 .and. n == 1) then
      key = -5; return
    end if
    key = ichar(buf(1))
  end function

  ! Move cursor to 1-based (col, row)
  subroutine tui_move(col, row)
    integer, intent(in) :: col, row
    character(len=16)   :: buf
    write(buf, '(a,i0,a,i0,a)') ESC//'[', row, ';', col, 'H'
    write(*, '(a)', advance='no') trim(buf)
  end subroutine

end module fortrat_tui
