program day9
  use omp_lib
  implicit none

  double precision :: stime, etime
  integer, allocatable, dimension(:,:) :: map, newmap
  logical, allocatable, dimension(:,:) :: lowpoint
  integer, allocatable, dimension(:) :: sizes

  integer :: ierr, N, M, i, j, k, nchanges
  character :: A

  stime = omp_get_wtime()

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
  rewind(1) ! back to top
 
  allocate(map(0:M+1, 0:N+1))
  allocate(lowpoint(M, N))
  map = huge(i)
  do i = 1, N
    read(1,'(100i1)') map(1:M,i)
  end do

  lowpoint = .false.
  where(map(1:M,1:N) < min(map(1:M,0:N-1), &
                           map(1:M,2:N+1), &
                           map(0:M-1,1:N), &
                           map(2:M+1,1:N))) &
        lowpoint = .true.

  write(*,*) "part 1:", sum(map(1:M,1:N) + 1, lowpoint)
  etime = omp_get_wtime()
  write(*,*) "took:", etime-stime, "seconds"


  k = -1
  do j = 1, N
    do i = 1, M
      if (lowpoint(i,j)) then
        map(i,j) = k
        k=k-1
      end if
    end do
  end do
  
  allocate(newmap(0:M+1, 0:N+1))

  newmap = map
  nchanges = 1
  do while(nchanges /= 0 )
    where(map(1:M,1:N) < 9)
      newmap(1:M,1:N)= min(map(1:M,1:N), &
                           map(1:M,0:N-1), &
                           map(1:M,2:N+1), &
                           map(0:M-1,1:N), &
                           map(2:M+1,1:N))
    end where

    nchanges = count(newmap /= map)
    map = newmap
  end do
  
  allocate(sizes(k+1:-1))
  do j = 1, N
    do i = 1, M
      if (map(i,j) < 9) sizes(map(i,j)) = sizes(map(i,j)) + 1
    end do
  end do


  call qsort(sizes, lbound(sizes, 1), ubound(sizes, 1))
  write(*,*) "part 2:", product(sizes(ubound(sizes,1)-2:))
  etime = omp_get_wtime()
  write(*,*) "took:", etime-stime, "seconds"

  deallocate(newmap)
  deallocate(map)
  deallocate(lowpoint)
  deallocate(sizes)

contains
  recursive subroutine qsort(array, low, high)
    integer, allocatable, dimension(:), intent(inout) :: array
    integer, intent(in) :: low, high
    integer :: pivot

    if (low < high) then
      call partition(array, low, high, pivot)
      call qsort(array, low, pivot-1)
      call qsort(array, pivot+1, high)
    end if
  
  end subroutine qsort

  subroutine partition(array, low, high, pi)
    integer, allocatable, dimension(:), intent(inout) :: array
    integer, intent(in) :: low, high
    integer, intent(out) :: pi
    integer :: pivot, ret, i, j, temp

    pivot = array(high)
    i=low-1
    do j = low, high-1
      if (array(j) < pivot) then
        i = i + 1
        temp = array(i) 
        array(i) = array(j)
        array(j) = temp
      end if
    end do
    temp = array(high)
    array(high) = array(i+1)
    array(i+1) = temp

    pi = i+1

  end subroutine partition
end program day9
