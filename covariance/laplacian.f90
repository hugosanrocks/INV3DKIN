
        subroutine laplacian(green_mesh)

        !COMMON VARIABLES
        IMPLICIT NONE
        INCLUDE 'green.h'
        TYPE (mesh) :: green_mesh

        integer i, j, k, iunit
        real laplace(green_mesh%msub,green_mesh%msub)
        real lambda
        integer hyp


        laplace(:,:) = 1.

        !Five point laplacian operator [0 1 0;1 -4 1;0 1 0]
        do i=1,green_mesh%msub
          laplace(i,i) = -4.
        enddo

        do i=1,5
         print *, laplace(i,1:5)
        enddo

        endsubroutine laplacian
