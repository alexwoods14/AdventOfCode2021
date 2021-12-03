program day2

  implicit none

  integer, parameter :: N = 1000
  integer, dimension(N) :: magnitude
  character*15, dimension(N) :: direction
  integer :: i, horizontal, vertical, aim


  open (unit = 1, file = "input.txt")

  do i = 1, N
    read(1,*) direction(i), magnitude(i)
  end do

  horizontal = 0
  vertical = 0

  do i = 1, N
    if (direction(i) == "forward") horizontal = horizontal + magnitude(i)
    if (direction(i) == "up") vertical = vertical - magnitude(i)
    if (direction(i) == "down") vertical = vertical + magnitude(i)
  end do

  write (*,*) "part1:", horizontal*vertical
  
  

  horizontal = 0
  vertical = 0
  aim = 0

  do i = 1, N
    if (direction(i) == "forward") then
      horizontal = horizontal + magnitude(i)
      vertical = vertical + aim*magnitude(i)
    end if
    if (direction(i) == "up") aim = aim - magnitude(i)
    if (direction(i) == "down") aim = aim + magnitude(i)
  end do

  write (*,*) "part2:", horizontal*vertical


end program day2
