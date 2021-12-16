program day16
  use omp_lib
  implicit none

  double precision :: stime, etime  ! timing calls
  integer :: i, N, ierr, versionsum
  integer(kind=8) :: res(3)
  character :: A
  character, allocatable :: packet(:)


  open(1,file="input.txt")
  N=0
  do
    read(1, '(A)', advance="no" ,iostat=ierr) A
    if (ierr.NE.0) exit
    N=N+1
  end do
  rewind(1)

  ! write data to packet, auto expand hex to binary with method
  allocate(packet(N*4))
  packet = '0'
  do i = 1, N
    read(1, '(A)', advance="no" ,iostat=ierr) A
    packet(1 + (i-1)*4:i*4) = hextobin(A)
  end do  
  close(1)

  stime = omp_get_wtime()
  versionsum = 0
  versionsum = versionsum + bintodec(packet(1:3))

  res = compute(int(1, kind=8))

  write(*,*) "Part 1:", res(2)
  etime = omp_get_wtime()
  write(*,*) "Took :", etime-stime, "s"

  
  write(*,*) "Part 2:", res(3)
  etime = omp_get_wtime()
  write(*,*) "time =", etime-stime, "s"
  

  deallocate(packet)
contains
  ! res(end index, version sum, return value)
  recursive function compute(start) result(res)
    integer(kind=8) :: res(3), returned(3)
    integer(kind=8) :: start
    integer :: pactype, j, pacver
    integer :: lentypeid, lenarg
    integer(kind=8) :: val, i, versum


    i = start
    pacver = bintodec(packet(i:i+2))
    i = i+3
    pactype = bintodec(packet(i:i+2))
    i = i+3

    versum = pacver
    val = 0
    if(pactype >= 5) val = -1 ! -1 used to avoid comparing first literal to nothing
    if(pactype == 1) val = 1 ! multiplication
    if(pactype == 2) val = huge(val) ! minimum
    if(pactype == 3) val = 10-huge(val) ! max



    if(pactype == 4) then !literal
      versum = pacver
      j = 0
      do while(bintodec(packet(i:i)) /= 0)
        i = i + 5
        j = j + 1
      end do
      val = 0
      do i = 0, j
        val = val + 2**(4*(j-i))*bintodec(packet(start+7+i*5:start+7+i*5+3))
      end do

      i = start + 6 + (j+1)*5

    else
      lentypeid = bintodec(packet(i:i)) ! 0 is length of subpacket
                                        ! 1 is num of subpackets
      i = i + 1 ! inc after type
      if(lentypeid == 0) then ! total length = lenarg
        lenarg = bintodec(packet(i:i+14))
        i = i + 15
        j = i
        do while (i /= j + lenarg)
          returned = compute(i)
          i = returned(1)
          versum = versum + returned(2)
          call updateval(pactype, val, returned(3))
        end do
      else ! lenarg number of packets
        lenarg = bintodec(packet(i:i+10))
        i = i + 11
        do j = 1, lenarg
          returned = compute(i)
          i = returned(1)
          versum = versum + returned(2)
          call updateval(pactype, val, returned(3))
        end do
      end if
    end if
    res(1) = i
    res(2) = versum
    res(3) = val
  end function compute
          
 
  subroutine updateval(id, val, returnedval)
    integer :: id
    integer(kind=8) :: val, returnedval
    select case(id)
      case(0)
        val = val+returnedval
      case(1)
        val = val*returnedval
      case(2)
        val = min(val, returnedval)
      case(3)
        val = max(val, returnedval)
      case(5)
        if(val == -1) then
          val = returnedval
        else
          if (val > returnedval) then
            val = 1
          else
            val = 0
          end if
        end if
      case(6)
        if(val == -1) then
          val = returnedval
        else
          if (val < returnedval) then
            val = 1
          else
            val = 0
          end if
        end if
      case(7)
        if(val == -1) then
          val = returnedval
        else
          if (val == returnedval) then
            val = 1
          else
            val = 0
          end if
        end if

    end select
  end subroutine updateval

  function hextobin(H)
    character :: H
    character*4 :: bin
    character, dimension(4) :: hextobin
    select case(H)
      case('0') 
        bin = "0000"
      case('1')
        bin = "0001"
      case('2')
        bin = "0010"
      case('3')
        bin = "0011"
      case('4')
        bin = "0100"
      case('5')
        bin = "0101"
      case('6')
        bin = "0110"
      case('7')
        bin = "0111"
      case('8')
        bin = "1000"
      case('9')
        bin = "1001"
      case('A')
        bin = "1010"
      case('B')
        bin = "1011"
      case('C')
        bin = "1100"
      case('D')
        bin = "1101"
      case('E')
        bin = "1110"
      case('F')
        bin = "1111"
    end select
    hextobin(1) = bin(1:1)
    hextobin(2) = bin(2:2)
    hextobin(3) = bin(3:3)
    hextobin(4) = bin(4:4)
  end function hextobin

  function bintodec(B) result(dec)
    character, dimension(:) :: B
    integer :: dec, length, i
    length = size(B)

    dec = 0
    do i = 1, length
      dec = dec + (ichar(B(i))-48)*2**(length-i)
    end do

  end function bintodec

end program day16
