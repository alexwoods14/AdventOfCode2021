program day12
  use omp_lib
  implicit none

  double precision :: stime, etime
  character*10, allocatable, dimension(:) :: input
  character*5, allocatable, dimension(:) :: caveids, origcaveids
  logical, allocatable, dimension(:,:) :: connections
  character*5, allocatable, dimension(:) :: stack
  character*10 :: A
  character*100 :: route

  integer :: ierr, i, j, N, M, ENDN, STARTN, loc, sPtr, num, maxpaths


  stime = omp_get_wtime()
  
  connections = .false. 
  caveids = ''
  stack = ''
  sPtr = 0

  open(1,file="input.txt")

  N=0 ! rows
  M=0 ! cols

  do
    read(1,*,iostat=ierr) A
    if(ierr.ne.0) exit
    N=N+1
  end do
  rewind(1) ! back to top

  allocate(input(N))
  allocate(stack(N+5))
  allocate(caveids(N))
  allocate(connections(N,N))

  ! create pointer for each one
  j = 1
  do i = 1, N
    read(1,*) input(i)
  end do

  call construct_connections()
  
  num = 0
  call paths(stack, sum(findloc(caveids,"start")))
  write(*,*) "Part 1:", num
  etime = omp_get_wtime()
  write(*,*) "Took:", etime-stime, "seconds"
  
 
  num = 0
  stack = ''
  sPtr = 0
  call pathspart2(stack, sum(findloc(caveids,"start")))

  write(*,*) "Part 2:", num
  etime = omp_get_wtime()
  write(*,*) "Took:", etime-stime, "seconds"
  
  deallocate(input)
  deallocate(caveids)
  deallocate(connections)
  deallocate(stack)

contains

  recursive subroutine paths(stack, curr) 
    character*5, allocatable, dimension(:) :: stack
    integer :: id, curr
    integer :: next
    integer :: i

    id = curr

    sPtr = sPtr + 1
    stack(sPtr) = caveids(id)

    if( caveids(id) == "end") then
      num = num + 1
!      write(*,fmt='(10A6)') stack
    else
    do i = 1, N 
      if (connections(i,id) == .true.) then ! if connected
        if(sum(findloc(stack, caveids(i))) == 0 .or. is_cap(caveids(i)(1:1))) then
            call paths(stack, i)
            stack(sPtr) = ''
            sPtr = sPtr -1
        end if
      end if
    end do
    end if

  end subroutine paths
  
  recursive subroutine pathspart2(stack, curr) 
    character*5, allocatable, dimension(:) :: stack
    integer :: id, curr
    integer :: next
    integer :: i

    id = curr

    sPtr = sPtr + 1
    stack(sPtr) = caveids(id)

    if( caveids(id) == "end") then
      num = num + 1
!      write(*,*) stack
    else
    do i = 1, N 
      if (connections(i,id) == .true.) then ! if connected
        if(sum(findloc(stack, caveids(i))) == 0 .or. is_cap(caveids(i)(1:1)) .or. (.not.  aretwolowercase(stack) .and. caveids(i) /= "start" .and. caveids(i) /= "end")) then
            call pathspart2(stack, i)
            stack(sPtr) = ''
            sPtr = sPtr -1
        end if
      end if
    end do
    end if

  end subroutine pathspart2
  
  function aretwolowercase(stack)
    character*5, allocatable, dimension(:) :: stack
    integer :: i
    logical :: aretwolowercase

    aretwolowercase = .false.
    do i = 1, count(caveids /= '')
      if (caveids(i) /= "start" .and. caveids(i) /= "end" .and. .not. is_cap(caveids(i)(1:1))) then
        aretwolowercase = aretwolowercase .or. count(stack == caveids(i)) >= 2
      end if
    end do

  end function aretwolowercase

  function is_cap(c)
    character :: c
    logical :: is_cap
    is_cap = ichar(c) < 97
  end function is_cap
  
  subroutine construct_connections()
    integer ::  fromloc, toloc
    character*5 :: from, to

    do i = 1, N
      A = input(i)
      loc = index(A, '-')
      from = trim(A(:loc-1))
      to = trim(A(loc+1:))
      if(sum(findloc(caveids, from)) == 0) then
        caveids(j) = from
        j = j+1
      end if

      if(sum(findloc(caveids, to)) == 0) then
        caveids(j) = to
        j = j+1
      end if
    end do

    do i = 1, N
      A = input(i)
      loc = index(A, '-')
      from = trim(A(:loc-1))
      fromloc = sum(findloc(caveids, from))
      to = trim(A(loc+1:))
      toloc = sum(findloc(caveids, to))

      connections(fromloc, toloc) = .true.
      connections(toloc, fromloc) = .true.
    end do
    
  end subroutine construct_connections

end program day12






