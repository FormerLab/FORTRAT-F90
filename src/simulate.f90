module fortrat_simulate
  use iso_fortran_env, only: real64
  use fortrat_types
  implicit none

  real(real64), parameter :: REPULSION    = -20.0d0   ! many-body strength
  real(real64), parameter :: LINK_DIST    =   6.0d0   ! target edge length (chars)
  real(real64), parameter :: LINK_STR     =   0.35d0
  real(real64), parameter :: CLUSTER_STR  =   0.06d0  ! x-axis namespace clustering
  real(real64), parameter :: CENTER_STR   =   0.02d0
  real(real64), parameter :: COLLIDE_R    =   4.0d0   ! collision radius (chars)
  real(real64), parameter :: ALPHA_DECAY  =   0.02d0
  real(real64), parameter :: VELOCITY_DEC =   0.6d0   ! velocity decay per tick

  real(real64), save, public :: alpha = 1.0d0

contains

  ! Assign initial positions scattered by ns_group
  subroutine sim_init(graph, w, h)
    type(lex_graph_t), intent(inout) :: graph
    integer,           intent(in)    :: w, h
    integer  :: i, ns
    real(real64) :: cx, seed

    alpha = 1.0d0

    do i = 1, graph%n_nodes
      ns = graph%nodes(i)%ns_group
      ! Cluster x-center for each ns group
      cx = (dble(ns) / 6.0d0) * dble(w)
      ! Pseudo-random scatter using node index
      seed = dble(mod(int(i,int64) * 2654435769_int64, 100000_int64)) / 100000.0d0
      graph%nodes(i)%x  = cx + (seed - 0.5d0) * dble(w) * 0.15d0
      graph%nodes(i)%y  = dble(h) * 0.5d0 + (seed - 0.3d0) * dble(h) * 0.4d0
      graph%nodes(i)%vx = 0.0d0
      graph%nodes(i)%vy = 0.0d0
    end do
  end subroutine

  subroutine sim_tick(graph, w, h)
    type(lex_graph_t), intent(inout) :: graph
    integer,           intent(in)    :: w, h
    real(real64) :: ax(MAX_NODES), ay(MAX_NODES)
    real(real64) :: dx, dy, dist, force, cx
    integer      :: i, j, src, tgt, ns
    real(real64) :: alpha_k

    alpha_k = alpha

    ax = 0.0d0
    ay = 0.0d0

    ! ── Many-body repulsion (O(n²) — fine for ~200 nodes) ──
    do i = 1, graph%n_nodes
      if (.not. graph%nodes(i)%active) cycle
      do j = i+1, graph%n_nodes
        if (.not. graph%nodes(j)%active) cycle
        dx   = graph%nodes(j)%x - graph%nodes(i)%x
        dy   = graph%nodes(j)%y - graph%nodes(i)%y
        dist = sqrt(dx*dx + dy*dy) + 0.01d0
        force = REPULSION * alpha_k / (dist * dist)
        ax(i) = ax(i) - force * dx / dist
        ay(i) = ay(i) - force * dy / dist
        ax(j) = ax(j) + force * dx / dist
        ay(j) = ay(j) + force * dy / dist
      end do
    end do

    ! ── Link forces ──
    do i = 1, graph%n_edges
      src = graph%edges(i)%src
      tgt = graph%edges(i)%tgt
      if (src == 0 .or. tgt == 0) cycle
      if (.not. graph%nodes(src)%active) cycle
      if (.not. graph%nodes(tgt)%active) cycle
      dx   = graph%nodes(tgt)%x - graph%nodes(src)%x
      dy   = graph%nodes(tgt)%y - graph%nodes(src)%y
      dist = sqrt(dx*dx + dy*dy) + 0.01d0
      force = (dist - LINK_DIST) / dist * LINK_STR * alpha_k
      ax(src) = ax(src) + force * dx
      ay(src) = ay(src) + force * dy
      ax(tgt) = ax(tgt) - force * dx
      ay(tgt) = ay(tgt) - force * dy
    end do

    ! ── Namespace cluster + center forces ──
    do i = 1, graph%n_nodes
      if (.not. graph%nodes(i)%active) cycle
      ns = graph%nodes(i)%ns_group
      cx = (dble(ns) / 6.0d0) * dble(w)
      ax(i) = ax(i) + (cx - graph%nodes(i)%x) * CLUSTER_STR * alpha_k
      ay(i) = ay(i) + (dble(h)*0.5d0 - graph%nodes(i)%y) * CENTER_STR * alpha_k
    end do

    ! ── Collision avoidance (skip when alpha is low — saves O(n²)) ──
    if (alpha_k > 0.1d0) then
    do i = 1, graph%n_nodes
      if (.not. graph%nodes(i)%active) cycle
      do j = i+1, graph%n_nodes
        if (.not. graph%nodes(j)%active) cycle
        dx   = graph%nodes(j)%x - graph%nodes(i)%x
        dy   = graph%nodes(j)%y - graph%nodes(i)%y
        dist = sqrt(dx*dx + dy*dy) + 0.01d0
        if (dist < COLLIDE_R) then
          force = (COLLIDE_R - dist) / dist * 0.5d0
          ax(i) = ax(i) - force * dx
          ay(i) = ay(i) - force * dy
          ax(j) = ax(j) + force * dx
          ay(j) = ay(j) + force * dy
        end if
      end do
    end do
    end if

    ! ── Integrate velocities ──
    do i = 1, graph%n_nodes
      if (.not. graph%nodes(i)%active) cycle
      if (graph%nodes(i)%fx >= 0.0d0) then
        ! Pinned node
        graph%nodes(i)%x  = graph%nodes(i)%fx
        graph%nodes(i)%vx = 0.0d0
        graph%nodes(i)%vy = 0.0d0
        cycle
      end if
      graph%nodes(i)%vx = (graph%nodes(i)%vx + ax(i)) * VELOCITY_DEC
      graph%nodes(i)%vy = (graph%nodes(i)%vy + ay(i)) * VELOCITY_DEC
      graph%nodes(i)%x  =  graph%nodes(i)%x + graph%nodes(i)%vx
      graph%nodes(i)%y  =  graph%nodes(i)%y + graph%nodes(i)%vy
      ! Clamp to grid
      graph%nodes(i)%x = max(3.0d0, min(dble(w)-4.0d0, graph%nodes(i)%x))
      graph%nodes(i)%y = max(1.0d0, min(dble(h)-1.0d0, graph%nodes(i)%y))
    end do

    ! ── Cool down ──
    alpha = alpha * (1.0d0 - ALPHA_DECAY)
    if (alpha < 0.0005d0) alpha = 0.0d0  ! fully settled
  end subroutine

  ! Pre-warm: run N ticks with high alpha to settle initial layout
  subroutine sim_prewarm(graph, w, h, n_ticks)
    type(lex_graph_t), intent(inout) :: graph
    integer,           intent(in)    :: w, h, n_ticks
    integer :: i
    alpha = 1.0d0
    do i = 1, n_ticks
      call sim_tick(graph, w, h)
    end do
    ! Leave a little heat so it finishes settling live, but not too much
    if (alpha > 0.05d0) alpha = 0.05d0
  end subroutine

end module fortrat_simulate
