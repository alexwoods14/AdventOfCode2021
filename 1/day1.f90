program day1

  implicit none

  integer, dimension(2000) :: input
  integer :: i, p1total, p2total

  open (unit = 1, file = "input.txt")

  p1total = 0
  p2total = 0

  do i = 1,size(input)
      read(1,*) input(i)
      if(i >= 2 .and. input(i) > input(i-1)) p1total = p1total + 1
      if(i >= 4 .and. sum(input(i-2:i)) > sum(input(i-3:i-1))) p2total = p2total + 1
  end do

  write (*,*) "part 1:", p1total
  write (*,*) "part 2:", p2total
  

end program day1
