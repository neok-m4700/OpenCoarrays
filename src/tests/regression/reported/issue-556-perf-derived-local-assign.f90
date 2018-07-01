program main
   use mpi
   implicit none

   type co
      real(8), allocatable :: a(:, :, :)[:]
   end type

   type(co) :: lhs, rhs
   real(8) :: t0, t1, t2, t3
   real(8), allocatable :: buf(:, :, :)
   integer, parameter :: ni = 8, nj = 8, nk = 8
   integer :: i, j, k, me, nimg
   logical :: fail, c1, c2, c3
   character(5) :: prefix

   me = this_image()
   nimg = num_images()
   write(prefix, '(i1a1i1a2)') me, '/', nimg, ': '

   if (nimg /= 2) error stop 1

   allocate( &
      lhs % a(ni, nj, nk)[*], &
      rhs % a(ni, nj, nk)[*], &
      buf(ni, nj, nk))

   ! call random_number(rhs % a)
   rhs % a = reshape([(me * 100 + i, i = 1, ni * nj * nk)], [ni, nj, nk])
   sync all

   print *, prefix, '==> START <=='
   lhs % a = 0.
   t0 = mpi_wtime()
   buf(:, :, :) = rhs % a
   lhs % a = buf
   t1 = mpi_wtime() - t0
   print *, prefix, 't1=', t1
   c1 = any(abs(lhs % a - buf) > epsilon(0.))

   sync all

   ! this bit shows the issue, sending data to myself over MPI is a poor choice !
   lhs % a = 0.
   t0 = mpi_wtime()
   lhs % a = rhs % a ! implicit MPI transfer, where there should NOT be !
   t2 = mpi_wtime() - t0
   print *, prefix, 't2=', t2
   c2 = any(abs(lhs % a - buf) > epsilon(0.))

   sync all
   lhs % a = 0.
   t0 = mpi_wtime()
   do k = 1, nk
      do j = 1, nj
         do i = 1, ni
            lhs % a(i, j, k) = rhs % a(i, j, k)
         end do
      end do
   end do
   t3 = mpi_wtime() - t0
   print *, prefix, 't3=', t3
   c3 = any(abs(lhs % a - buf) > epsilon(0.))
   
   sync all
   print *, prefix, '==> STOP <=='

   fail = fail .or. c1 .or. c2 .or. c3
   ! fail = fail .or. (t2 > 100 * t1) .or. (t2 > 100 * t3) !  <== still no improvement

   if (fail) then
      write(*, *) 'Test failed!'
      write(*, *) 'c1=', c1, ' c2=', c2, ' c3=', c3
      error stop 5
   else
      write(*, *) 'Test passed.'
   end if

end program
