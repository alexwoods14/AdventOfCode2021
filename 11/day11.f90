program day11
  use omp_lib
  implicit none

  double precision :: stime, etime
  integer, allocatable, dimension(:,:) :: grid
  logical, allocatable, dimension(:,:) :: flashed

  integer :: ierr, i, j, N, M, flashes, flashcount
  character :: A
  integer :: steps, part2step, part1flashes
  logical :: changed, allflash

  steps = 100

  stime = omp_get_wtime()

  open(1,file="input.txt")

  N=0 ! rows
  M=0 ! cols

  do
    read(1,'(A)',advance="no",iostat=ierr) A
    if (ierr.NE.0) exit
    if(A .NE. ' ') M=M+1
  end do
  rewind 1
  do
    read(1,*,iostat=ierr)
    if(ierr.ne.0) exit
    N=N+1
  end do
  rewind(1) ! back to top

  allocate(grid(0:M+1,0:N+1))
  allocate(flashed(M,N))

  do i = 1, N
    read(1,'(10I1)') grid(1:M,i)
  end do
    
!  write(*,fmt='(10i1)') grid(1:M,1:N)

  flashcount = 0
  allflash = .false.

  i = 1
  do while (.not. allflash)
    flashed = .false.
    grid = grid + 1

    flashes = 0
    changed = .true.

    do while (changed) ! while at least 1 flash
      where (grid(1:M,1:N) > 9 .and. .not. flashed)
        flashed = .true.
        grid(0:M-1,1:N)   = grid(0:M-1,1:N)   + 1 ! left
        grid(2:M+1,1:N)   = grid(2:M+1,1:N)   + 1 ! right
        grid(1:M,0:N-1)   = grid(1:M,0:N-1)   + 1 ! up
        grid(1:M,2:N+1)   = grid(1:M,2:N+1)   + 1 ! down
        grid(0:M-1,0:N-1) = grid(0:M-1,0:N-1) + 1 ! left, up
        grid(0:M-1,2:N+1) = grid(0:M-1,2:N+1) + 1 ! left, down
        grid(2:M+1,0:N-1) = grid(2:M+1,0:N-1) + 1 ! right, up
        grid(2:M+1,2:N+1) = grid(2:M+1,2:N+1) + 1 ! right, down
      end where
      changed = flashes /= count(flashed) ! did any flash this step?
      flashes = count(flashed)
    end do
    where (flashed) grid(1:M,1:N) = 0  ! reset to 0 if flashed
    flashcount = flashcount + flashes

    allflash = all(flashed)
    if(allflash) part2step = i
    if(i == 100) part1flashes = flashcount

    i = i + 1
  end do
  


  write(*,*) "Part 1:", part1flashes
  etime = omp_get_wtime()
  write(*,*) "Took:", etime-stime, "seconds"
  
  write(*,*) "Part 2:", part2step
  etime = omp_get_wtime()
  write(*,*) "Took:", etime-stime, "seconds"



  deallocate(grid)
end program day11
