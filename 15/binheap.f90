module binary_heap
  implicit none

  integer, parameter :: RANK = 3

  integer, dimension(RANK,5000) ::  heap
  integer :: heap_size = 0


contains
  subroutine reset_heap()
    heap_size = 0
    heap = 0
  end subroutine reset_heap
  function parent(i)
    integer :: i, parent
    parent = i/2
  end function parent
  
  function left(i)
    integer :: i, left
    left = 2*i
  end function left
  
  function right(i)
    integer :: i, right
    right = 2*i+1
  end function right

  subroutine swap(i, j)
    integer :: i, j
    integer, dimension(RANK) ::  temp
    temp(:) = heap(:,i)
    heap(:,i) = heap(:,j)
    heap(:,j) = temp(:)
  end subroutine swap

  subroutine insert(dist, x, y)
    integer :: dist, x, y, i
    integer, dimension(RANK) :: toins
    toins(:) = (/dist, x, y /)
    ! insert at end
    heap_size = heap_size + 1
    i = heap_size
    heap(:,heap_size) = toins(:)
    do while(i /= 1 .and. heap(1,parent(i)) > heap(1, i))
      call swap(i, parent(i))
      i = parent(i)
    end do 


  end subroutine insert

  function popmin()
    integer, dimension(RANK) :: popmin
    popmin = heap(:,1)
    heap(:,1) = heap(:, heap_size)
    call reheap(1)
    heap_size = heap_size - 1
  end function popmin
  
  recursive subroutine reheap(root)
    integer :: root
    integer :: l, r, smallest

    l = left(root)
    r = right(root)
    smallest = root
    if(l <= heap_size .and. heap(1,l) < heap(1,smallest)) smallest = l
    if(r <= heap_size .and. heap(1,r) < heap(1,smallest)) smallest = r
    

    if(smallest /= root) then
      call swap(root, smallest)
      call reheap(smallest)
    end if

  end subroutine reheap

end module binary_heap
