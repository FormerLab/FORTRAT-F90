module fortrat_types
  use iso_fortran_env, only: int32, int64, real64
  implicit none

  integer, parameter :: MAX_NODES    = 512
  integer, parameter :: MAX_EDGES    = 4096
  integer, parameter :: MAX_FIELDS   = 64
  integer, parameter :: ID_LEN       = 128
  integer, parameter :: LABEL_LEN    = 32
  integer, parameter :: DOC_LEN      = 256
  integer, parameter :: KIND_LEN     = 16

  ! Namespace group IDs
  integer, parameter :: NS_APP_BSKY    = 1
  integer, parameter :: NS_COM_ATPROTO = 2
  integer, parameter :: NS_CHAT_BSKY   = 3
  integer, parameter :: NS_TOOLS_OZONE = 4
  integer, parameter :: NS_COMMUNITY   = 5

  ! Lexicon kinds
  integer, parameter :: KIND_RECORD       = 1
  integer, parameter :: KIND_QUERY        = 2
  integer, parameter :: KIND_PROCEDURE    = 3
  integer, parameter :: KIND_SUBSCRIPTION = 4
  integer, parameter :: KIND_TOKEN        = 5
  integer, parameter :: KIND_OBJECT       = 6
  integer, parameter :: KIND_UNKNOWN      = 7

  character(len=4), parameter :: KIND_SIGIL(7) = &
    [ 'REC ', 'QRY ', 'SUB ', 'EVT ', 'TOK ', 'OBJ ', '??? ' ]

  ! ANSI color escape codes (phosphor green palette)
  character(len=*), parameter :: ESC        = achar(27)
  character(len=*), parameter :: CSI        = ESC // '['
  character(len=*), parameter :: RESET      = ESC // '[0m'
  character(len=*), parameter :: GREEN_BR   = ESC // '[92m'   ! bright green — primary
  character(len=*), parameter :: GREEN      = ESC // '[32m'   ! mid green
  character(len=*), parameter :: GREEN_DIM  = ESC // '[2;32m' ! dim green
  character(len=*), parameter :: YELLOW     = ESC // '[33m'   ! amber — community
  character(len=*), parameter :: CYAN       = ESC // '[36m'   ! chat.bsky
  character(len=*), parameter :: MAGENTA    = ESC // '[35m'   ! tools.ozone
  character(len=*), parameter :: WHITE      = ESC // '[97m'
  character(len=*), parameter :: BLACK_BG   = ESC // '[40m'
  character(len=*), parameter :: GREEN_BG   = ESC // '[42m'
  character(len=*), parameter :: BOLD       = ESC // '[1m'
  character(len=*), parameter :: REVERSE    = ESC // '[7m'
  character(len=*), parameter :: CLEAR_SCR  = ESC // '[2J'
  character(len=*), parameter :: HIDE_CUR   = ESC // '[?25l'
  character(len=*), parameter :: SHOW_CUR   = ESC // '[?25h'

  type :: lex_field_t
    character(len=ID_LEN)    :: name    = ''
    character(len=KIND_LEN)  :: ftype   = ''
    character(len=ID_LEN)    :: ref     = ''
    logical                  :: required = .false.
  end type

  type :: lex_node_t
    character(len=ID_LEN)    :: id        = ''
    character(len=LABEL_LEN) :: label     = ''
    integer                  :: ns_group  = NS_COMMUNITY
    integer                  :: kind      = KIND_UNKNOWN
    character(len=DOC_LEN)   :: doc       = ''
    integer                  :: n_fields  = 0
    type(lex_field_t)        :: fields(MAX_FIELDS)
    logical                  :: active    = .false.   ! visible in current filter
    ! Force simulation state
    real(real64)             :: x = 0.0d0, y = 0.0d0
    real(real64)             :: vx = 0.0d0, vy = 0.0d0
    real(real64)             :: fx = -1.0d0           ! -1 means unpin
  end type

  type :: lex_edge_t
    integer   :: src = 0    ! index into nodes array
    integer   :: tgt = 0
    logical   :: is_union = .false.
  end type

  type :: lex_graph_t
    integer           :: n_nodes = 0
    integer           :: n_edges = 0
    type(lex_node_t)  :: nodes(MAX_NODES)
    type(lex_edge_t)  :: edges(MAX_EDGES)
  end type

  ! Application state
  integer, parameter :: MODE_LOADING = 0
  integer, parameter :: MODE_GRAPH   = 1
  integer, parameter :: MODE_SEARCH  = 2
  integer, parameter :: MODE_INSPECT = 3
  integer, parameter :: MODE_ERROR   = 4
  integer, parameter :: MODE_HELP    = 5

  type :: app_state_t
    integer                    :: mode         = MODE_LOADING
    integer                    :: cursor_idx   = 1     ! 1-based into visible nodes
    integer                    :: selected_idx = 0     ! 0 = none
    character(len=ID_LEN)      :: search_query = ''
    logical                    :: groups(5)             ! visible ns groups
    logical                    :: community    = .false.
    character(len=128)         :: status_msg   = 'READY'
    character(len=256)         :: progress     = ''
    character(len=512)         :: error_msg    = ''
    integer                    :: term_w       = 120
    integer                    :: term_h       = 40
    integer                    :: graph_w      = 78    ! cols for graph pane
    integer                    :: inspect_w    = 42    ! cols for inspect pane
    integer                    :: graph_h      = 36    ! rows for graph area
  end type

contains

  ! NS group → ANSI color string
  function ns_color(ns) result(col)
    integer, intent(in) :: ns
    character(len=16)   :: col
    select case(ns)
    case(NS_APP_BSKY);    col = GREEN_BR
    case(NS_COM_ATPROTO); col = YELLOW
    case(NS_CHAT_BSKY);   col = CYAN
    case(NS_TOOLS_OZONE); col = MAGENTA
    case default;         col = WHITE
    end select
  end function

end module fortrat_types
