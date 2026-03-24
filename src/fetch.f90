module fortrat_fetch
  use iso_c_binding
  use fortrat_types
  implicit none

  ! libcurl constants
  integer(c_long), parameter :: CURLOPT_URL             = 10002
  integer(c_long), parameter :: CURLOPT_WRITEFUNCTION   = 20011
  integer(c_long), parameter :: CURLOPT_WRITEDATA       = 10001
  integer(c_long), parameter :: CURLOPT_USERAGENT       = 10018
  integer(c_long), parameter :: CURLOPT_FOLLOWLOCATION  = 52
  integer(c_long), parameter :: CURLOPT_SSL_VERIFYPEER  = 64
  integer(c_long), parameter :: CURLOPT_HTTPHEADER      = 10023
  integer(c_long), parameter :: CURLOPT_TIMEOUT         = 30
  integer(c_long), parameter :: CURLOPT_CONNECTTIMEOUT  = 10
  integer(c_long), parameter :: CURL_GLOBAL_ALL         = 3

  type(c_ptr), save :: curl_handle = c_null_ptr

  ! Dynamic buffer for HTTP response
  integer, parameter          :: MAX_HTTP_BUF = 8 * 1024 * 1024  ! 8 MB
  character(len=1), save, target :: http_buf(MAX_HTTP_BUF)
  integer,          save      :: http_buf_len = 0

  interface
    function curl_global_init(flags) bind(c, name='curl_global_init') result(r)
      import c_long, c_int
      integer(c_long), value :: flags
      integer(c_int)         :: r
    end function

    function curl_easy_init() bind(c, name='curl_easy_init') result(h)
      import c_ptr
      type(c_ptr) :: h
    end function

    function curl_easy_setopt_ptr(handle, option, param) &
        bind(c, name='curl_easy_setopt') result(r)
      import c_ptr, c_long, c_int
      type(c_ptr),     value :: handle
      integer(c_long), value :: option
      type(c_ptr),     value :: param
      integer(c_int)         :: r
    end function

    function curl_easy_setopt_long(handle, option, param) &
        bind(c, name='curl_easy_setopt') result(r)
      import c_ptr, c_long, c_int
      type(c_ptr),     value :: handle
      integer(c_long), value :: option, param
      integer(c_int)         :: r
    end function

    function curl_easy_setopt_funptr(handle, option, param) &
        bind(c, name='curl_easy_setopt') result(r)
      import c_ptr, c_long, c_int, c_funptr
      type(c_ptr),     value :: handle
      integer(c_long), value :: option
      type(c_funptr),  value :: param
      integer(c_int)         :: r
    end function

    function curl_easy_perform(handle) bind(c, name='curl_easy_perform') result(r)
      import c_ptr, c_int
      type(c_ptr), value :: handle
      integer(c_int)     :: r
    end function

    subroutine curl_easy_cleanup(handle) bind(c, name='curl_easy_cleanup')
      import c_ptr
      type(c_ptr), value :: handle
    end subroutine

    function curl_slist_append(list, str) bind(c, name='curl_slist_append') result(r)
      import c_ptr, c_char
      type(c_ptr),       value       :: list
      character(c_char), intent(in)  :: str(*)
      type(c_ptr)                    :: r
    end function

    subroutine curl_slist_free_all(list) bind(c, name='curl_slist_free_all')
      import c_ptr
      type(c_ptr), value :: list
    end subroutine
  end interface

contains

  ! Write callback called by curl for each received chunk
  function write_callback(ptr, size, nmemb, userdata) &
      bind(c) result(written)
    type(c_ptr),        value :: ptr, userdata
    integer(c_size_t),  value :: size, nmemb
    integer(c_size_t)         :: written
    integer(c_size_t)         :: nbytes
    character(len=1), pointer :: src(:)
    integer :: i, start
    ! userdata intentionally unused — curl requires the argument
    if (.false.) written = transfer(userdata, written)

    nbytes  = size * nmemb
    written = nbytes

    call c_f_pointer(ptr, src, [int(nbytes)])
    start = http_buf_len + 1
    do i = 1, int(nbytes)
      if (http_buf_len >= MAX_HTTP_BUF) exit
      http_buf_len = http_buf_len + 1
      http_buf(http_buf_len) = src(i)
    end do
  end function

  subroutine fetch_init()
    integer(c_int) :: rc
    rc = curl_global_init(CURL_GLOBAL_ALL)
    curl_handle = curl_easy_init()
    rc = curl_easy_setopt_long(curl_handle, CURLOPT_FOLLOWLOCATION, 1_c_long)
    rc = curl_easy_setopt_long(curl_handle, CURLOPT_SSL_VERIFYPEER, 0_c_long)
    rc = curl_easy_setopt_long(curl_handle, CURLOPT_TIMEOUT,        30_c_long)
    rc = curl_easy_setopt_long(curl_handle, CURLOPT_CONNECTTIMEOUT, 10_c_long)
  end subroutine

  ! Fetch a URL, return response as allocatable string
  subroutine fetch_url(url, response, ok)
    character(len=*),              intent(in)  :: url
    character(len=:), allocatable, intent(out) :: response
    logical,                       intent(out) :: ok
    integer(c_int)   :: rc
    type(c_funptr)   :: cbfun
    character(len=len_trim(url)+1, kind=c_char), target :: curl_url
    character(len=12, kind=c_char),              target :: ua_str
    integer :: i

    http_buf_len = 0
    ok = .false.

    ! Build C strings
    do i = 1, len_trim(url)
      curl_url(i:i) = url(i:i)
    end do
    curl_url(len_trim(url)+1:len_trim(url)+1) = c_null_char

    ua_str = 'fortrat/1.0' // c_null_char

    cbfun = c_funloc(write_callback)
    rc = curl_easy_setopt_ptr(curl_handle, CURLOPT_URL, &
         c_loc(curl_url))
    rc = curl_easy_setopt_funptr(curl_handle, CURLOPT_WRITEFUNCTION, cbfun)
    rc = curl_easy_setopt_ptr(curl_handle, CURLOPT_USERAGENT, &
         c_loc(ua_str))

    rc = curl_easy_perform(curl_handle)
    if (rc /= 0) return

    allocate(character(len=http_buf_len) :: response)
    do i = 1, http_buf_len
      response(i:i) = http_buf(i)
    end do
    ok = .true.
  end subroutine

  subroutine fetch_cleanup()
    if (c_associated(curl_handle)) call curl_easy_cleanup(curl_handle)
  end subroutine

end module fortrat_fetch
