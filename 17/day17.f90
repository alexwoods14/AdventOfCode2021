program day1
  use omp_lib

  implicit none

  double precision :: stime, etime
!  integer, parameter :: minY = -10, maxY = -5, minX = 20, maxX = 30
  integer, parameter :: minY = -114, maxY = -75, minX = 153, maxX = 199
  integer :: i, u, v, s, s2, maxs, xv, sx, countin, steps, tempv
  logical :: used(1000)

  countin = 0
  stime = OMP_get_wtime()
  maxs = 0
  do u = 1, 1000
    s = u*(u+1)/2
    ydown:do v = u+10, u, -1
      s2 = v*(v+1)/2
      if(s-s2 >= minY .and. s-s2 <= maxY) then
        maxs = s
        steps = u+v!+1
        xveloc: do xv = 0, steps+(maxX-minX)
          sx = xv*(xv+1)/2 - (xv-steps)*(xv-steps+1)/2
          if (xv < steps) sx = xv*(xv+1)/2
          if (sx >= minX .and. sx <= maxX) then
            countin = countin+1
!            write(*,'(I0,",",I0)') xv, u
            cycle
          end if
        end do xveloc
        exit ydown
      end if
      end do ydown
  end do

  write(*,'(A, I0)') "Part 1: ", maxs
  etime = OMP_get_wtime()
  write(*,'(A, F5.3," ms")') "Part 1: ", (etime-stime)*1000
  
  write(*,'(A, I0)') "Part 2: ", countin
  etime = OMP_get_wtime()
  write(*,'(A, F5.3," ms")') "Part 1: ", (etime-stime)*1000
  
  do u = 0, -10000, -1
    tempv = u
    s = 0
!    do steps = 1, 1000
    steps = 0
    used = .false.
    do while(s >= minY)
      steps = steps + 1
      s = s + tempv
      tempv = tempv - 1
    !if(u == 0) write(*,*) "s=", s, "steps=", steps

      if(s >= minY .and. s <= maxY) then
!      write(*,*) "u=", u, "s=", s, "steps=", steps
      do xv = 0, steps+(maxX-minX)+10000
        sx = xv*(xv+1)/2 - (xv-steps)*(xv-steps+1)/2
        if (xv < steps) sx = xv*(xv+1)/2
          if (sx >= minX .and. sx <= maxX .and. .not. used(xv)) then
            used(xv) = .true.
            countin = countin+1
!            write(*,'(I0,",",I0)') xv, u
            cycle
          end if
      end do 
    end if
    end do
  end do



  write(*,'(A, I0)') "Part 2: ", countin
  etime = OMP_get_wtime()
  write(*,'(A, F5.3," ms")') "Part 1: ", (etime-stime)*1000



end program day1
