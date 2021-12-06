program day6
  implicit none

  integer, allocatable, dimension(:) :: input
  integer(kind=8), dimension(0:8) :: days, new

  integer :: ierr, N, i
  character :: A

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
    new(0:7) = days(1:8)
    new(6) = new(6) + days(0)
    new(8) = days(0)
    days = new
  end do

  write(*,*) sum(days)



end program day6
