program day7
  use omp_lib
  implicit none

  integer, allocatable, dimension(:) :: input
  integer(kind=8), dimension(0:8) :: days, new
  double precision :: stime, etime

  integer :: ierr, N, i, mean, fuel, j, k, minfuel
  character :: A

  stime = omp_get_wtime()

  open(1,file="input.txt")
  N=1

  do
    read(1,'(A)',advance="no",iostat=ierr) A
    if (ierr.NE.0) exit
    if(A .EQ. ',') N=N+1
  end do
  rewind(1) ! back to top

  allocate(input(N))
  read(1, *) input

  ! part 1
  call qsort(input, 1, N)
  write(*,*) "part 1:", sum(abs(input - median(input)))
  etime = omp_get_wtime()
  write(*,*) "took:", etime-stime, "seconds"


  ! part 2
  mean = NINT(real(sum(input))/size(input))
  minfuel = huge(minfuel)
  fuel = 0
  do i = -1, 1 ! median is 464.56 but min value is actually 464 not 465 as mean evaluates to
    do j = 1, N
      fuel = fuel + part2move(input(j), mean+i)
    end do
    minfuel = min(minfuel,fuel)
  end do

  write(*,*) "part 2:", minfuel
  etime = omp_get_wtime()
  write(*,*) "took:", etime-stime, "seconds"

  deallocate(input)


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

  function median(array)
    integer, allocatable, dimension(:) :: array
    integer :: N, median

    N = size(array)
    if(MOD(N,2) == 0) then
      median = int((array(N/2) + array(N/2 + 1))/2.d0)
    else
      median = array(ceiling(N/2.d0))
    end if
  end function median

  function part2move(src, dest)
    integer :: src, dest, part2move, i
    part2move = abs(dest-src)*(abs(dest-src) + 1) / 2;
  end function part2move

end program day7
