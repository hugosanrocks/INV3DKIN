       subroutine conv(x,lx,h,lh,y,ly)

       !1D CONVOLUTION SUBROUTINE

       implicit none
       !Dimension of vectors
       integer, intent(inout) :: LX, LH, LY
       !Vectors to be convolved
       real, intent(inout) :: x(lx), h(lh), y(ly)
       !Counters
       integer i,j

       DO i=1,LY
       Y(i)=0.0
       enddo

       DO i=1,LX
        DO j=1,Lh
         Y(i+j-1)=Y(i+j-1)+X(i)*h(j)
        ENDDO
       ENDDO


       end subroutine conv
