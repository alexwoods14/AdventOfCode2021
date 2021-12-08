program day8
  use omp_lib
  implicit none

  character*7, allocatable:: input(:, :), output(:, :)
  character, dimension(1) :: A
  character*7 :: work
  character*7, dimension(0:9) :: numbers
  double precision :: stime, etime

  integer, dimension(0:9) :: segcounts

  integer :: ierr, N, i, j, length, k, l, total, int_out, val

  stime = omp_get_wtime()

  open (unit = 1, file = "input.txt")

  N=0
  do
    read(1,*,iostat=ierr)
    if(ierr.ne.0) exit
    N=N+1
  end do
  rewind(1)
  allocate(input(10, N))
  allocate(output(4, N))

  do i = 1, N
    read(1, *) input(:, i), A, output(:, i)
  end do

  ! part 1
  segcounts = 0
  do i = 1, N
    do j = 1, 4
      length = len(trim(output(j,i)))
      segcounts(length) = segcounts(length) + 1
    end do
  end do
  write(*,*) "part 1:", sum((/ segcounts(2), segcounts(4), segcounts(3), segcounts(7)/))
  etime = omp_get_wtime()
  write(*,*) "took:", etime-stime, "seconds"


  ! 0 1 2 3 4 5 6 7 8 9

  ! 6 2 5 5 4 5 6 3 7 6
  numbers = "#"
  total = 0
  do i = 1, N
    do k = 2, 7
      do j = 1, 10
        work = input(j, i)
        length = len(trim(work))
        if(length /= k) cycle

        if(length == 2) numbers(1) = work
        if(length == 3) numbers(7) = work
        if(length == 4) numbers(4) = work
        if(length == 7) numbers(8) = work
        
        if(length == 5) then ! 2, 3, 5
          if(in_both(work, numbers(1)) == 2) then ! 3
            numbers(3) = work
          else
            if(in_both(work, numbers(4)) == 3) then
              numbers(5) = work
            else
              numbers(2) = work
            end if
          end if
        end if

        if(length == 6) then ! 0, 6, 9
          if(in_both(work, numbers(5)) == 5) then
            if(in_both(work, numbers(1)) == 2) then ! 9
              numbers(9) = work
            else
              numbers(6) = work
            end if
          else
            numbers(0) = work
          end if
        end if

      end do
    end do

    val = -1
    int_out = 0
    do j = 1, 4
      work = output(j, i)
      do k = 0, 9
        if(equal(work, numbers(k))) val = k
      end do
      int_out = int_out + val * 10 ** (4-j)
    end do
!    write(*,*) int_out
    total = total + int_out
    
  end do

  write(*,*) "part 2:",  total
  etime = omp_get_wtime()
  write(*,*) "took:", etime-stime, "seconds"
  
  deallocate(input)
  deallocate(output)

  contains
    function equal(a, c)
      character*7 :: a, b, c
      logical :: equal
      integer :: i, j
      b = c
      equal = .true.
      if(len(trim(a)) .ne. len(trim(b))) then
        equal = .false.
      else
        outer: do i = 1, len(trim(a))
          check: do j = 1, len(trim(a))
            if (a(i:i) == b(j:j)) then
              b(j:j) = "X"
              exit check
            end if
            if (j == len(trim(a))) then
              equal = .false.
              exit outer
            end if
          end do check
        end do outer
      end if

    end function equal
    
    function in_both(a, c)
      character*7 :: a, b, c
      integer :: in_both
      integer :: i, j
      in_both = 0
      b = c
      outer: do i = 1, len(trim(a))
        check: do j = 1, len(trim(b))
          if (a(i:i) == b(j:j)) then
            b(j:j) = "X"
            in_both = in_both + 1
            exit check
          end if
        end do check
      end do outer

    end function in_both

end program day8
