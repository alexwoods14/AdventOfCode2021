program day3

  implicit none

  integer, parameter :: N = 1000, L=12
  character (len=L) :: working
  character (len=L), dimension(N) :: input
  integer, dimension(L,N) :: inputs
  integer :: i, j, ogr, csr, com
  integer, dimension(L) :: counts, gr, er
  logical, dimension(N) :: keep

  open (unit = 1, file = "input.txt")

  counts(:) = 0

  do i = 1, N
    read(1,*) working
    input(i) = working
    do j = 1, L
    read(working(j:j),*) inputs(j,i)
    end do
  end do

!  write (*,*) inputs(:,3)

  do j = 1,L
    counts(j) = count(inputs(j,:) == 1)
  end do

  gr(:) = 0
  er(:) = 0
  where (counts >= N/2)
    gr = 1
    er = 0
  elsewhere (counts < N/2)
    gr = 0
    er = 1
  end where

  write(*,*) "part 1", bin_to_dec(gr) * bin_to_dec(er)

  keep(:) = .true.
  o: do j = 1, L
    if (count(keep==.true.) == 1) exit o
    counts(j)=count(inputs(j,:) == 1 .and. keep)
    com = 0
    if (counts(j) >= count(keep==.true.)/real(2)) com = 1
    do i = 1, N
      if(keep(i) .and. inputs(j,i) /= com) keep(i) = .false.
    end do
  end do o

  ogr = bin_to_dec(inputs(:,findloc(keep, .true.)))

  keep(:) = .true.
  c: do j = 1, L
    if (count(keep==.true.) == 1) exit c
    counts(j)=count(inputs(j,:) == 1 .and. keep)
    com = 0
    if (counts(j) < count(keep==.true.)/real(2)) com = 1
    do i = 1, N
      if(keep(i) .and. inputs(j,i) /= com) keep(i) = .false.
    end do
  end do c

  csr = bin_to_dec(inputs(:,findloc(keep, .true.)))


  write(*,*) "part 2", csr*ogr
  
contains
  integer function bin_to_dec(bin)
    integer, dimension(L), intent(in) :: bin
    integer :: dec
    dec = 0
    do i = 1,L
      dec = dec + bin(i)*(2**(L-i))
    end do
    bin_to_dec = dec

  end function bin_to_dec


end program day3
