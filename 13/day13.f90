program day13
  use omp_lib
  implicit none

  integer, allocatable, dimension(:,:) :: coords, folds
  double precision :: stime, etime
  logical, allocatable, dimension(:,:) :: map
  character, allocatable, dimension(:,:) :: readable

  integer :: ierr, N, M, i, k, minx, miny
  character :: A
  character*20 :: FMT

  stime = omp_get_wtime()

  open(1,file="input.txt")
  N=0

  do
    read(1,*,iostat=ierr) A
    if(A == 'f') exit
    N=N+1
  end do

  M = 1
  do
    read(1,*,iostat=ierr)
    if(ierr /= 0) exit
    M=M+1
  end do
  rewind(1) ! back to top

  allocate(coords(2,N))
  ! (location, dim)
  allocate(folds(2,M))

  do i = 1, N
    read(1, *) coords(:,i)
  end do
  read(1, *) 
  
  minx = huge(minx)
  miny = huge(miny)

  do i = 1, M  
    !read(1, *) line
    read(1,fmt="(""fold along "",A,""=""i)") A, k
    if(A=='x') then
      folds(2,i) = 1
      minx = min(minx,k)
    elseif(A=='y') then
      folds(2,i) = 2
      miny = min(miny,k)
    end if
    folds(1,i) = k
  end do
  !write(*,fmt="(""fold along "",A,""=""i)") 'x', 2

  allocate(map(0:maxval(coords(1,:)),0:maxval(coords(2,:))))
  map = .false.
  do i = 1, N
    map(coords(1,i), coords(2,i)) = .true.
  end do
!  write(*,fmt='(11l1)') map

  call fold(map, folds(1,1), folds(2,1))

  write(*,*) "Part 1", count(map)
  etime = omp_get_wtime()
  write(*,*) "Time =", etime-stime, "s"

  do i = 2, M
    call fold(map, folds(1,i), folds(2,i))
  end do

  allocate(readable(0:minx-1,0:miny-1))
  readable = '.'
  where (map(0:minx-1,0:miny-1)==.true.) readable = '#'

  WRITE(FMT,'("(", I0, "A1)")') minx
  write(*,FMT) readable
  write(*,*) "Part 2"
  etime = omp_get_wtime()
  write(*,*) "Time =", etime-stime, "s"

  deallocate(map)
  deallocate(folds)
  deallocate(coords)
  deallocate(readable)

contains 
  subroutine fold(map, l, d)
    logical, allocatable, dimension(:,:) :: map
    integer :: l, d, i
    if(d == 1) then
      do i = l+1, min(ubound(map, d), l*2)
        map(2*l-i,:) = map(2*l-i,:) .or. map(i,:)
      end do
      map(l:,:) = .false.
    elseif(d == 2) then
      do i = l+1, min(ubound(map, d), l*2)
        map(:,2*l-i) = map(:,2*l-i) .or. map(:,i)
      end do
      map(:,l:) = .false.
    end if

  end subroutine fold


end program day13
