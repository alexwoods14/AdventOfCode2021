program day4

  implicit none

  integer, parameter :: N = 100, L=5
  integer, dimension(N) :: input
  logical, dimension(N) :: bingo
  integer :: a, i, j, called
  integer, dimension(L,L,N) :: boards
  logical, dimension(L,L,N) :: drawn
  integer, dimension(2) :: loc
  !integer, dimension(L,N) :: inputs

  open (unit = 1, file = "input.txt")

  read(1,*) input
  
  do i = 1, N
    read(1,*)
    do j = 1,L
      read(1,*) boards(:,j,i)
    end do
  end do

  drawn = .false.
  bingo = .false.

  main: do a = 1, size(input)
    called = input(a)
    do i = 1, N
      loc = findloc(boards(:,:,i), called)
      if(sum(loc) /= 0 ) then
        drawn(loc(1),loc(2),i) = .true.
      end if
      do j = 1,L
        if (.not. bingo(i) .and. (all(drawn(j,:,i) == .true.) .or. all(drawn(:,j,i) == .true.))) then
          if( all(bingo == .false.) ) then
            write(*,*) i, "is first to bingo with score: ", &
              sum(boards(:,:,i), .not. drawn(:,:,i))*called
          end if

          bingo(i) = .true.
        end if
        if ( all(bingo)) then
            write(*,*) i, "is last to bingo with score: ", &
              sum(boards(:,:,i), .not. drawn(:,:,i))*called
          exit main
        end if
      end do
    end do
  end do main



!  write(*,*) boards(:,:,1)
!  write(*,*) boards(:,:,2)
! write(*,*) boards(:,:,3)
  
!  write(*,*)
!  loc = findloc(boards(:,:,3), 7)
!  if(sum(loc) /= 0 ) write(*,*) boards(loc(1),loc(2),3)

end program day4
