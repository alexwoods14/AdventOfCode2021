program day10
  use omp_lib
  implicit none

  double precision :: stime, etime
  character, dimension(4) :: start, fin
  integer, dimension(4) :: scores1, scores2
  character, dimension(110) :: total
  character, allocatable, dimension(:,:) :: input


  integer :: ierr, i, j, N, M, loc, k, p1score
  integer(kind=8), allocatable, dimension(:) :: p2score
  character :: A

  start  = (/ '(', '[', '{', '<' /)
  fin    = (/ ')', ']', '}', '>' /)
  scores1 = (/ 3, 57, 1197, 25137 /)
  scores2 = (/  1,   2,   3,   4 /)

  stime = omp_get_wtime()

  open(1,file="input.txt")

  N = 0

  do
    read(1,*,iostat=ierr)
    if(ierr.ne.0) exit
    N=N+1
  end do
  rewind(1) ! back to top

  allocate(input(110,N))
  allocate(p2score(N))

  do i = 1, N
    read(1,'(110A1)') input(:,i)
  end do
  

  p1score = 0
  p2score = 0
  do i = 1, N 
    total = ''
    k = 1
    word:do j = 1, sum(len_trim(input(:,i)))
      loc = sum(findloc(start, input(j,i)))
      if (loc /= 0) then
        total(k) = input(j,i)
        k=k+1
      else
        loc = sum(findloc(fin, input(j,i)))
        if(loc == sum(findloc(start, total(k-1)))) then
          total(k-1) = ''
          k = k-1
        else
          p1score = p1score + scores1(loc)
          total=''
          exit word
        end if
      end if
    end do word
    if(sum(len_trim(total)) /= 0) then
      do j = 1, sum(len_trim(total))
        k = k - 1
        loc = sum(findloc(start, total(k)))
        p2score(i) = p2score(i)*5 + scores2(loc)
      end do
    end if
  end do

  call qsort(p2score, int(lbound(p2score,1),kind=8), int(ubound(p2score, 1),kind=8))
  loc = count(p2score > 0)

  write(*,*) "Part 1:", p1score
  etime = omp_get_wtime()
  write(*,*) "Took:", etime-stime, "seconds"
  
  write(*,*) "Part 2:", p2score( loc + ceiling((N-loc)/2.d0)  )
  etime = omp_get_wtime()
  write(*,*) "Took:", etime-stime, "seconds"



  deallocate(input)
  deallocate(p2score)
 
contains
  recursive subroutine qsort(array, low, high)
    integer(kind=8), allocatable, dimension(:), intent(inout) :: array
    integer(kind=8), intent(in) :: low, high
    integer(kind=8) :: pivot

    if (low < high) then
      call partition(array, low, high, pivot)
      call qsort(array, low, pivot-1)
      call qsort(array, pivot+1, high)
    end if
  
  end subroutine qsort

  subroutine partition(array, low, high, pi)
    integer(kind=8), allocatable, dimension(:), intent(inout) :: array
    integer(kind=8), intent(in) :: low, high
    integer(kind=8), intent(out) :: pi
    integer(kind=8) :: pivot, ret, i, j, temp

    pivot = array(high)
    i=low-1
    do j = low, high-1
      if (array(j) < pivot) then
        i = i + 1
        temp = array(i) 
        array(i) = array(j)
        array(j) = temp
      end if
    end do
    temp = array(high)
    array(high) = array(i+1)
    array(i+1) = temp

    pi = i+1

  end subroutine partition
end program day10
