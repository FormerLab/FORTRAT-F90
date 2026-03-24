module fortrat_lexparse
  use json_module, only: json_core, json_value, json_file, &
                         json_CK => json_CK, &
                         json_LK => json_LK
  use fortrat_types
  use fortrat_fetch
  implicit none

  ! Convenience alias — CK is the character kind json-fortran uses
  integer, parameter :: CK = json_CK

  character(len=*), parameter :: TREE_URL = &
    'https://api.github.com/repos/bluesky-social/atproto/git/trees/main?recursive=1'
  character(len=*), parameter :: RAW_BASE = &
    'https://raw.githubusercontent.com/bluesky-social/atproto/main/'

  integer, parameter :: MAX_PATHS = 256
  character(len=256) :: lex_paths(MAX_PATHS)
  integer            :: n_paths = 0

contains

  function resolve_ref(base, ref) result(target)
    character(len=*), intent(in)  :: base, ref
    character(len=ID_LEN)         :: target
    character(len=ID_LEN)         :: r
    integer :: hash_pos

    target = ''
    if (len_trim(ref) == 0) return
    if (ref(1:1) == '#') then; target = base; return; end if

    r = ref
    if (len_trim(r) > 4 .and. r(1:4) == 'lex:') r = r(5:)
    hash_pos = index(r, '#')
    if (hash_pos > 0) r = r(1:hash_pos-1)
    if (len_trim(r) == 0 .or. index(r, '.') == 0) then
      target = base; return
    end if
    target = trim(r)
  end function

  function detect_nsgroup(id) result(ns)
    character(len=*), intent(in) :: id
    integer :: ns
    if     (len_trim(id) >= 9  .and. id(1:9)  == 'app.bsky.')    then; ns = NS_APP_BSKY
    else if(len_trim(id) >= 12 .and. id(1:12) == 'com.atproto.') then; ns = NS_COM_ATPROTO
    else if(len_trim(id) >= 10 .and. id(1:10) == 'chat.bsky.')   then; ns = NS_CHAT_BSKY
    else if(len_trim(id) >= 13 .and. id(1:13) == 'tools.ozone.') then; ns = NS_TOOLS_OZONE
    else;                                                                ns = NS_COMMUNITY
    end if
  end function

  function detect_kind(type_str) result(k)
    character(len=*), intent(in) :: type_str
    integer :: k
    select case(trim(type_str))
    case('record');       k = KIND_RECORD
    case('query');        k = KIND_QUERY
    case('procedure');    k = KIND_PROCEDURE
    case('subscription'); k = KIND_SUBSCRIPTION
    case('token');        k = KIND_TOKEN
    case('object');       k = KIND_OBJECT
    case default;         k = KIND_UNKNOWN
    end select
  end function

  function short_label(id) result(lbl)
    character(len=*),    intent(in) :: id
    character(len=LABEL_LEN)        :: lbl
    integer :: i
    lbl = ''
    do i = len_trim(id), 1, -1
      if (id(i:i) == '.') then
        lbl = id(i+1:min(len_trim(id), i+LABEL_LEN))
        return
      end if
    end do
    lbl = id(1:min(len_trim(id), LABEL_LEN))
  end function

  ! Get a string from json_core by path, copy to fixed-len var
  subroutine jc_get_str(jc, root, path, val, found)
    type(json_core),   intent(inout) :: jc
    type(json_value),  pointer       :: root
    character(len=*),  intent(in)    :: path
    character(len=*),  intent(out)   :: val
    logical,           intent(out)   :: found
    character(kind=CK, len=:), allocatable :: tmp
    logical(json_LK) :: f

    val   = ''
    found = .false.
    call jc%get(root, path, tmp, f)
    if (.not. f) return
    if (.not. allocated(tmp)) return
    val   = tmp(1:min(len(tmp), len(val)))
    found = len_trim(val) > 0
  end subroutine

  ! Parse GitHub tree JSON — collect lexicon file paths
  subroutine parse_tree(json_str, paths, n)
    character(len=*),   intent(in)  :: json_str
    character(len=256), intent(out) :: paths(:)
    integer,            intent(out) :: n
    type(json_core)   :: jc
    type(json_value), pointer :: root, tree_arr, item
    character(kind=CK, len=:), allocatable :: path_val_ck
    character(len=256) :: path_val
    integer   :: n_tree, i
    logical(json_LK)   :: f

    n = 0
    call jc%parse(root, json_str)
    if (jc%failed()) then
      call jc%clear_exceptions(); return
    end if

    call jc%get(root, 'tree', tree_arr, f)
    if (.not. f .or. .not. associated(tree_arr)) then
      call jc%destroy(root); return
    end if

    n_tree = jc%count(tree_arr)
    do i = 1, n_tree
      call jc%get_child(tree_arr, i, item)
      if (.not. associated(item)) cycle
      call jc%get(item, 'path', path_val_ck, f)
      if (.not. f .or. .not. allocated(path_val_ck)) cycle
      path_val = path_val_ck

      if (index(path_val, 'lexicons/') == 0) cycle
      if (len_trim(path_val) < 5) cycle
      if (path_val(len_trim(path_val)-4:len_trim(path_val)) /= '.json') cycle

      n = n + 1
      if (n > size(paths)) then; n = n - 1; exit; end if
      paths(n) = trim(path_val)
    end do

    call jc%destroy(root)
  end subroutine

  ! Parse one lexicon JSON file, add node + edges to graph
  subroutine parse_lexicon(json_str, graph, community)
    character(len=*),  intent(in)    :: json_str
    type(lex_graph_t), intent(inout) :: graph
    logical,           intent(in)    :: community
    type(json_core)   :: jc
    type(json_value), pointer :: root, defs_node
    character(len=ID_LEN)  :: lex_id, type_str, desc_str
    logical  :: found
    integer  :: node_idx, i
    logical(json_LK) :: f

    call jc%parse(root, json_str)
    if (jc%failed()) then
      call jc%clear_exceptions(); return
    end if

    call jc_get_str(jc, root, 'id', lex_id, found)
    if (.not. found) then
      call jc%destroy(root); return
    end if

    ! Skip duplicates
    do i = 1, graph%n_nodes
      if (trim(graph%nodes(i)%id) == trim(lex_id)) then
        call jc%destroy(root); return
      end if
    end do
    if (graph%n_nodes >= MAX_NODES) then
      call jc%destroy(root); return
    end if

    graph%n_nodes = graph%n_nodes + 1
    node_idx = graph%n_nodes
    graph%nodes(node_idx)%id       = trim(lex_id)
    graph%nodes(node_idx)%label    = short_label(lex_id)
    graph%nodes(node_idx)%ns_group = merge(NS_COMMUNITY, detect_nsgroup(lex_id), community)
    graph%nodes(node_idx)%active   = .true.
    graph%nodes(node_idx)%kind     = KIND_UNKNOWN

    ! defs.main.type
    call jc_get_str(jc, root, 'defs.main.type', type_str, found)
    if (found) graph%nodes(node_idx)%kind = detect_kind(type_str)

    ! defs.main.description
    call jc_get_str(jc, root, 'defs.main.description', desc_str, found)
    if (found) graph%nodes(node_idx)%doc = trim(desc_str)

    ! Walk defs for ref edges
    call jc%get(root, 'defs', defs_node, f)
    if (f .and. associated(defs_node)) then
      call collect_refs(jc, defs_node, lex_id, graph)
    end if

    call jc%destroy(root)
  end subroutine

  ! Recursively walk json_value tree collecting ref edges
  recursive subroutine collect_refs(jc, node, lex_id, graph)
    type(json_core),           intent(inout) :: jc
    type(json_value), pointer, intent(in)    :: node
    character(len=*),          intent(in)    :: lex_id
    type(lex_graph_t),         intent(inout) :: graph
    type(json_value), pointer :: child
    character(kind=CK, len=:), allocatable :: sval
    character(len=ID_LEN) :: type_str, ref_str, target
    integer :: i, n
    logical(json_LK) :: f

    if (.not. associated(node)) return

    ! Check type field at this node
    call jc%get(node, 'type', sval, f)
    if (f .and. allocated(sval)) then
      type_str = sval
      if (trim(type_str) == 'ref') then
        call jc%get(node, 'ref', sval, f)
        if (f .and. allocated(sval)) then
          ref_str = sval
          target = resolve_ref(lex_id, ref_str)
          if (len_trim(target) > 0 .and. trim(target) /= trim(lex_id)) then
            call add_edge(graph, lex_id, target, .false.)
          end if
        end if
      end if
    end if

    ! Recurse into children
    n = jc%count(node)
    do i = 1, n
      call jc%get_child(node, i, child)
      if (associated(child)) call collect_refs(jc, child, lex_id, graph)
    end do
  end subroutine

  subroutine add_edge(graph, src_id, tgt_id, is_union)
    type(lex_graph_t), intent(inout) :: graph
    character(len=*),  intent(in)    :: src_id, tgt_id
    logical,           intent(in)    :: is_union
    integer :: i, src_idx, tgt_idx

    if (graph%n_edges >= MAX_EDGES) return
    src_idx = 0; tgt_idx = 0
    do i = 1, graph%n_nodes
      if (trim(graph%nodes(i)%id) == trim(src_id)) src_idx = i
      if (trim(graph%nodes(i)%id) == trim(tgt_id)) tgt_idx = i
    end do
    if (src_idx == 0 .or. tgt_idx == 0) return
    do i = 1, graph%n_edges
      if (graph%edges(i)%src == src_idx .and. &
          graph%edges(i)%tgt == tgt_idx) return
    end do
    graph%n_edges = graph%n_edges + 1
    graph%edges(graph%n_edges)%src      = src_idx
    graph%edges(graph%n_edges)%tgt      = tgt_idx
    graph%edges(graph%n_edges)%is_union = is_union
  end subroutine

  subroutine build_graph(graph, include_community, progress_cb)
    type(lex_graph_t), intent(inout) :: graph
    logical,           intent(in)    :: include_community
    interface
      subroutine progress_cb(msg)
        character(len=*), intent(in) :: msg
      end subroutine
    end interface
    character(len=:), allocatable :: response
    logical :: ok
    integer :: i
    character(len=512) :: msg

    call fetch_init()
    call progress_cb('Fetching atproto lexicon tree...')
    call fetch_url(TREE_URL, response, ok)
    if (.not. ok) then
      call progress_cb('ERROR: Could not reach GitHub API'); return
    end if

    call parse_tree(response, lex_paths, n_paths)
    deallocate(response)
    write(msg, '(a,i0,a)') 'Found ', n_paths, ' lexicon files'
    call progress_cb(trim(msg))

    graph%n_nodes = 0
    graph%n_edges = 0

    do i = 1, n_paths
      call fetch_url(RAW_BASE//trim(lex_paths(i)), response, ok)
      if (ok) then
        call parse_lexicon(response, graph, include_community)
        deallocate(response)
      end if
      if (mod(i, 20) == 0) then
        write(msg, '(a,i0,a,i0)') 'Loading: ', i, '/', n_paths
        call progress_cb(trim(msg))
      end if
    end do

    write(msg, '(a,i0,a,i0,a)') &
      'Ready: ', graph%n_nodes, ' nodes, ', graph%n_edges, ' edges'
    call progress_cb(trim(msg))
    call fetch_cleanup()
  end subroutine

  subroutine apply_filters(graph, groups)
    type(lex_graph_t), intent(inout) :: graph
    logical,           intent(in)    :: groups(5)
    integer :: i
    do i = 1, graph%n_nodes
      graph%nodes(i)%active = groups(graph%nodes(i)%ns_group)
    end do
  end subroutine

end module fortrat_lexparse
