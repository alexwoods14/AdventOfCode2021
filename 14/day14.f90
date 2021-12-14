program day14
  use omp_lib
  implicit none

  double precision :: stime, etime  ! timing calls

  character*2, allocatable, dimension(:) :: states
  character, allocatable, dimension(:) :: insert, chars
  integer, allocatable, dimension(:,:) :: transitions
  logical, allocatable, dimension(:,:) :: charidx  ! N*num of chars  :: true if char occurs in that state
  integer(kind=8), allocatable, dimension(:) :: counts, newcounts, charcounts
  integer :: ierr, N, i, j, steps
  character :: A
  character*25 :: template

  stime = omp_get_wtime()
  open(1,file="input.txt")
  N=0
  read(1,*) template ! input
  read(1,*) ! blank line
  ! calculate how many rules and allocate as such
  do
    read(1, * ,iostat=ierr)
    if (ierr.NE.0) exit
    N=N+1
  end do
  rewind(1)
  allocate(states(N))
  allocate(insert(N))
  allocate(counts(N))
  allocate(newcounts(N))
  allocate(transitions(3,N)) ! each state transforms into 2 different states + character of which it generates
  
  read(1,*)
  read(1,*)
  do i = 1, N
    read(1, *) states(i), A, insert(i)
  end do

  call init_character_counts(chars, charcounts, charidx, states)
  call generate_transitions(transitions, states, insert, chars)
  call init_counts(counts, template, charcounts, chars)

  steps= 40
  do i = 1, 10
    newcounts = 0
    do j = 1, N
      newcounts(transitions(1,j)) = newcounts(transitions(1,j)) + counts(j)
      newcounts(transitions(2,j)) = newcounts(transitions(2,j)) + counts(j)
      charcounts(transitions(3,j)) = charcounts(transitions(3,j)) + counts(j)
    end do
    counts = newcounts
  end do

  etime = omp_get_wtime()

  write(*,*) "Part 1:", maxval(charcounts) - minval(charcounts)
  write(*,*) "time =", etime-stime, "s"

  do i = 11, steps
    newcounts = 0
    do j = 1, N
      newcounts(transitions(1,j)) = newcounts(transitions(1,j)) + counts(j)
      newcounts(transitions(2,j)) = newcounts(transitions(2,j)) + counts(j)
      charcounts(transitions(3,j)) = charcounts(transitions(3,j)) + counts(j)
    end do
    counts = newcounts
  end do
  
  write(*,*) "Part 2:", maxval(charcounts) - minval(charcounts)
  etime = omp_get_wtime()
  write(*,*) "time =", etime-stime, "s"

  etime = omp_get_wtime()

  deallocate(states)
  deallocate(insert)
  deallocate(counts)
  deallocate(newcounts)
  deallocate(transitions)
  deallocate(chars)
  deallocate(charcounts)
  deallocate(charidx)
contains
  subroutine generate_transitions(transitions, states, insert, chars)
    integer, allocatable, dimension(:,:) :: transitions
    character*2, allocatable, dimension(:) :: states
    character, allocatable, dimension(:) :: insert, chars
    integer :: i, locl, locr
    character*2 :: nl, nr

    do i = 1, N
      nl = states(i)
      nr = states(i)
      nl(2:2) = insert(i)
      nr(1:1) = insert(i)
      locl = sum(findloc(states, nl))
      locr = sum(findloc(states, nr))
!      write(*,*) states(i), " -> ", insert(i), "  --->  ", nl, ":", nr, locl, locr
      transitions(1,i) = locl
      transitions(2,i) = locr
      transitions(3,i) = sum(findloc(chars, insert(i)))
    end do
  end subroutine generate_transitions

  subroutine init_counts(counts, template, charcounts, chars)
    integer(kind=8), allocatable, dimension(:) :: counts, newcounts, charcounts
    character, allocatable, dimension(:) :: chars
    character*25 :: template
    integer :: i
    
    charcounts = 0
    counts = 0
    charcounts(sum(findloc(chars, template(1:1))))=charcounts(sum(findloc(chars, template(1:1))))+1
    do i = 1, len_trim(template) - 1
      counts(sum(findloc(states, template(i:i+1))))=counts(sum(findloc(states, template(i:i+1))))+1
      charcounts(sum(findloc(chars, template(i+1:i+1))))=charcounts(sum(findloc(chars, template(i+1:i+1))))+1
    end do
  end subroutine init_counts
  
  subroutine init_character_counts(chars, charcounts, charidx, states)
    character*2, allocatable, dimension(:) :: states
    character, allocatable, dimension(:) :: temp, chars
    logical, allocatable, dimension(:,:) :: charidx
    integer(kind=8), allocatable, dimension(:) :: charcounts
    integer :: i, j, k
    allocate(temp(N))
    j = 1
    do i = 1, N
      do k = 1, 2
      if (sum(findloc(temp,states(i)(k:k))) == 0) then
          temp(j) = states(i)(k:k)
          j = j + 1
        end if
      end do
    end do
    allocate(chars(j-1))
    chars(:) = temp(:j-1)
    deallocate(temp)

    allocate(charcounts(j-1))
    allocate(charidx(N, j-1))

    charidx = .false.
    do i = 1, j
      do k = 1, N
      if (states(k)(1:1) == chars(i)) charidx(k,i) = .true.
      if (states(k)(2:2) == chars(i)) charidx(k,i) = .true.
      enddo
    end do

    !    write(*,*) chars
!    write(*,fmt='(16L3)') charidx


  end subroutine init_character_counts
end program day14
