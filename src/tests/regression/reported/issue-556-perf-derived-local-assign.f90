program main
   use mpi
   implicit none

   type co
      real(8), allocatable :: a(:, :, :, :)[:]
   end type

   type(co) :: lhs, rhs
   real(8) :: t0, t1, t2, t3
   real(8), allocatable :: buf(:, :, :, :)
   integer, parameter :: ni = 4, nj = 4, nk = 4, nl = 4
   integer :: i, j, k, l
   logical :: fail

   if (num_images() /= 2) error stop 1

   allocate( &
      lhs % a(ni, nj, nk, nl)[*], &
      rhs % a(ni, nj, nk, nl)[*], &
      buf(ni, nj, nk, nl))

   sync all

   print *, '==> START <=='
   t0 = mpi_wtime()
   buf(:, :, :, :) = rhs % a
   lhs % a = buf
   t1 = mpi_wtime() - t0
   print *, 't1=', t1

   sync all

   t0 = mpi_wtime()
   lhs % a = rhs % a ! implicit MPI transfer, where there should NOT be !
   t2 = mpi_wtime() - t0
   print *, 't2=', t2

   sync all
   t0 = mpi_wtime()
   do l = 1, nl
      do k = 1, nk
         do j = 1, nj
            do i = 1, ni
               lhs % a(i, j, k, l) = rhs % a(i, j, k, l)
            end do
         end do
      end do
   end do
   t3 = mpi_wtime() - t0
   print *, 't3=', t3

   sync all
   print *, '==> STOP <=='

   ! factor 100 => should prevent any false positives, even with high CPU load
   fail = (t2 > 100 * t1) .or. (t2 > 100 * t3)
   fail = .false.

   if (fail) then
      write(*, *) 'Test failed!'
      error stop 5
   else
      write(*, *) 'Test passed.'
   end if

end program
