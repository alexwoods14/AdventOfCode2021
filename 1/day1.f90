program day1

  implicit none

  integer, parameter :: N = 2000
  integer, dimension(N) :: input
  integer :: i, p1total, p2total

  open (unit = 1, file = "input.txt")

  p1total = 0
  p2total = 0

  do i = 1,size(input)
      read(1,*) input(i)
  end do

  WRITE(*,'(A,I0)') "Part 1: ", COUNT(INPUT(1:N-1).LT.INPUT(2:N))
  WRITE(*,'(A,I0)') "Part 2: ", COUNT(INPUT(1:N-3).LT.INPUT(4:N))


end program day1
