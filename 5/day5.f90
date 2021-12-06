program day5

  implicit none

  integer, allocatable, dimension(:, :) :: start, finish, grid, grid2
  integer :: i, j, N, ierr, cross, sx, sy, fx, fy, x, y
  character*2 :: ignore


  open (unit = 1, file = "input.txt")


  ! IO allocation and input shamelessly stolen from reddit user u/autid
  N=0
  do
    read(1,*,iostat=ierr)
    if(ierr.ne.0) exit
    N=N+1
  end do
  rewind(1)

  allocate(start(2,N),finish(2,N))

  do i = 1, N
    read(1,*) start(:,i), ignore, finish(:,i)
  end do


  cross = 0
  allocate(grid(0:max(maxval(start(1,:)),maxval(finish(1,:))), &
                0:max(maxval(start(2,:)),maxval(finish(2,:)))))
  allocate(grid2(0:max(maxval(start(1,:)),maxval(finish(1,:))), &
                0:max(maxval(start(2,:)),maxval(finish(2,:)))))

  grid = 0
  do i = 1, N
    sx=min(start(1,i), finish(1,i))
    fx=max(start(1,i), finish(1,i))
    sy=min(start(2,i), finish(2,i))
    fy=max(start(2,i), finish(2,i))
    if(sx == fx) then
      grid(sx,sy:fy) = grid(sx,sy:fy) + 1
    elseif(sy == fy) then
      grid(sx:fx,sy) = grid(sx:fx,sy) + 1
    else
      x = start(1,i)
      y = start(2,i)
      do j = 0, fx-sx
        grid2(x+sign(j,finish(1,i)-x),y+sign(j,finish(2,i)-y)) = &
          grid2(x+sign(j,finish(1,i)-x),y+sign(j,finish(2,i)-y)) + 1
      end do
    end if
  end do
  write (*,*) "part 1:", count(grid >= 2)
  write (*,*) "part 2:", count((grid+grid2) >= 2)

end program day5
