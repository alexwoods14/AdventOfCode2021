program day6
  use omp_lib
  implicit none

  integer, allocatable, dimension(:) :: input
  integer(kind=8), dimension(0:8) :: days, new
  double precision :: stime, etime

  integer :: ierr, N, i
  character :: A

  stime = omp_get_wtime()

  open(1,file="input.txt")
  N=1

  ! also shamlessly stolen from u/autid due to still figuring out fortran inputs
  do
    read(1,'(A)',advance="no",iostat=ierr) A
    if (ierr.NE.0) exit
    if(A .EQ. ',') N=N+1
  end do
  rewind(1) ! back to top

  allocate(input(N))
  read(1, *) input

  new = 0
  days = 0


  do i = 0, 8
    days(i) = count(input == i)
  end do

  do i = 1, 256
    days = cshift(days, 1)
    days(6) = days(6) + days(8)
  end do

  write(*,*) sum(days)

  etime = omp_get_wtime()

  write(*,*) "time =", etime-stime, "s"


end program day6
