program day15
  use omp_lib
  use binary_heap
  implicit none

  double precision :: stime, etime
  integer, allocatable, dimension(:,:) :: map, map2
 
  integer :: ierr, N, M, i, j
  character :: A


  open(1,file="input.txt")
  N=0 ! rows
  M=0 ! cols
  do
    read(1,'(A)',advance="no",iostat=ierr) A
    if (ierr.NE.0) exit
    if(A .NE. ' ') M=M+1
  end do
  rewind 1
  do
    read(1,*,iostat=ierr)
    if(ierr.ne.0) exit
    N=N+1
  end do
  rewind 1 ! back to top
 
  allocate(map(M,N))
  do i = 1, N
    read(1,'(100i1)') map(:,i)
  end do
  ! timing logic not I/O
  stime = omp_get_wtime()



  call reset_heap()
  write(*,*) "part 1:", shortestpath(map)
  etime = omp_get_wtime()
  write(*,*) "took:", etime-stime, "seconds"

!  write(*,'(50i1)') map2
  allocate(map2(M*5, N*5))
  map2 = 0
  do i = 0, 4
    do j = 0, 4
      map2(1+i*M:(i+1)*M,1+j*N:(j+1)*N) = mod(map(:,:) + (i+j) - 1, 9) + 1
    end do
  end do

  call reset_heap()
  
  write(*,*) "part 2:", shortestpath(map2)
  etime = omp_get_wtime()
  write(*,*) "took:", etime-stime, "seconds"

  deallocate(map, map2)

contains

  function shortestpath(map)
    integer, allocatable, dimension(:,:) :: map
    logical, allocatable, dimension(:,:) :: inserted
    integer, dimension(RANK) :: curr
    integer :: shortestpath
    integer :: dist, x, y
    
    curr = (/0, 1, 1/)
    allocate(inserted(ubound(map,1), ubound(map,2)))
    inserted = .false.

    do while(curr(2) /= ubound(map,1) .or. curr(3) /= ubound(map,2))
      dist = curr(1)
      x = curr(2)
      y = curr(3)
      if(x < ubound(map,1) .and. .not. inserted(x+1, y)) then
        call insert(dist + map(x+1, y), x+1, y)
        inserted(x+1, y) = .true.
      end if
      if(y < ubound(map,2) .and. .not. inserted(x, y+1)) then
        call insert(dist + map(x, y+1), x, y+1)
        inserted(x, y+1) = .true.
      end if
      if(x > 1 .and. .not. inserted(x-1, y)) then
        call insert(dist+ map(x-1, y), x-1, y)
        inserted(x-1, y) = .true.
      end if
      if(y > 1 .and. .not. inserted(x, y-1)) then
        call insert(dist + map(x, y-1), x, y-1)
        inserted(x, y-1) = .true.
      end if
      curr = popmin()
    end do
    deallocate(inserted)
    shortestpath = curr(1)
  end function shortestpath
end program day15
