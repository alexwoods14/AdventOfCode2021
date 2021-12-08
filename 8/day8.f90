program day8
  use omp_lib
  implicit none

  character*7, allocatable:: input(:, :), output(:, :)
  character :: A
  double precision :: stime, etime
  integer, dimension(0:9) :: segcounts

  integer :: ierr, N, i, j, length

  stime = omp_get_wtime()

  open (unit = 1, file = "testinput.txt")

  N=0
  do
    read(1,*,iostat=ierr)
    if(ierr.ne.0) exit
    N=N+1
  end do
  rewind(1)
  allocate(input(10, N))
  allocate(output(4, N))

  do i = 1, N
    read(1, *) input(:, i), A, output(:, i)
  end do

  segcounts = 0
  do i = 1, N
    do j = 1, 4
      length = len(trim(output(j,i)))
      segcounts(length) = segcounts(length) + 1
    end do
  end do
  write(*,*) "part 1:", sum((/ segcounts(2), segcounts(4), segcounts(3), segcounts(7)/))






  deallocate(input)
  deallocate(output)

end program day8
