! Copyright (C) 2012 Tim Leslie, Breakaway Consulting Pty. Ltd.

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation; either version 2
! of the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
program sparse

  implicit none

  call main(10000)

contains

  subroutine main(n)

    integer, intent(in) :: n

    integer :: Ap (n+1), symbolic, numeric, filenum, status
    double precision :: control (20), info (90), x (n), b (n), y(n)
    integer, allocatable :: Ai(:)
    double precision, allocatable :: Ax(:)
    double precision :: A(n,n)

    integer :: i, j

    ! Set up an arbitrary sparse matrix in dense format
    A = 0.0d0
    do i=1,n
       A(i,i) = 2.0d0*i                    ! Diagonal terms
       A(i,mod(i+3, n) + 1) = -1.5d0*(i-3) ! Off-diagonal terms
    enddo
    
    ! Convert the matrix to sparse format
    call dense_to_sparse(A, n, Ap, Ai, Ax)

    ! Set default control parameters
    call umf4def(control)
    ! Factorise A to allow for repeated solutions of Ax = b
    call umf4sym (n, n, Ap, Ai, Ax, symbolic, control, info)
    call check_info(info, "umf4sym")
    call umf4num (Ap, Ai, Ax, symbolic, numeric, control, info)
    call check_info(info, "umf4num")

    ! Set up an arbitrary RHS b with known solution y
    do i=1,n
       y(i) = 3.0d0*i
    enddo
    b = matmul(A, y)

    ! Solve Ax = b (first parameter is UMFPACK_A = 0)
    call umf4sol (0, x, b, numeric, control, info)
    call check_info(info, "umf4sol")
    print *, maxval(abs(x - y))

    ! Solve Ax = b (first parameter is UMFPACK_A = 0)
    call umf4solr (0, Ap, Ai, Ax, x, b, numeric, control, info)
    call check_info(info, "umf4solr")
    print *, maxval(abs(x - y))

    ! Deallocate sparse format arrays. This can be done earlier
    ! if umf4sol is being used (as opposed to umf4solr).
    deallocate(Ai)
    deallocate(Ax)

  end subroutine main

  subroutine dense_to_sparse(A, n, Ap, Ai, Ax)
    integer, intent(in) :: n
    double precision, intent(in) :: A(n,n)
    integer, intent(out) :: Ap(n+1)
    integer, allocatable, intent(out) :: Ai(:)
    double precision, allocatable, intent(out) :: Ax(:)
    
    integer :: i, j, nz, p

    if (allocated(Ai) .or. allocated(Ax)) then
       print *, "Ai and Ax must be unallocated on entro to dense_to_sparse()"
       stop 1
    endif
    
    nz = count(A /= 0.0d0)
    allocate(Ai(nz))
    allocate(Ax(nz))

    p = 1 ! Index into the Ai and Ax arrays (1 based)
    do j=1,n ! Iterate over each column
       Ap(j) = p - 1 ! Save the starting index for this column in 0 based format
       do i=1,n ! Iterate over the rows, looking for non zero entries
          if (A(i,j) /= 0.0d0) then
             Ai(p) = i - 1  ! Zero based
             Ax(p) = A(i,j)
             p = p + 1
          endif
       end do
    end do
    Ap(n+1) = p - 1 ! Save the final index in 0 based format

    ! Check that the final index points to the nz'th entry
    if (Ap(n+1) /= nz) then 
       print *, "Something went wrong"
       print *, "Ap(n+1) = ", Ap(n+1), " /= nz = ", nz
    endif

  end subroutine dense_to_sparse

  subroutine check_info(info, umf_name)
    double precision, intent(in) :: info(:)
    character (len=*), intent(in) :: umf_name

    if (info(1) /= 0.0d0) then
       print *, "Error in ", umf_name, ": info = ", info(1)
       stop 1
    endif
  end subroutine check_info

end program sparse
