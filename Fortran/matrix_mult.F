c ----------------------------------------------------------------------
      subroutine matrix_vector_mul_fortran(y, A, x, m, n)
c ----------------------------------------------------------------------
      implicit none
      integer m, n, i, j
      real x(n), y(m), A(m,n)
      i=0
      j=0
c calculate y=Ax, where a is an m by n maxrix and x&y are 1 by n vectors
      do i=1,m
         do j=1,n
c order???? not sure
            y(j) = y(j)+(A(j,i)*x(j)
         end do
      end do
c cheating & fortran 90 specific
c      y=matmul(A,x)
      return y
      end
