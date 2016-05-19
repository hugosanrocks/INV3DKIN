
        subroutine laplacian(green_mesh)

        !COMMON VARIABLES
        IMPLICIT NONE
        INCLUDE 'green.h'
        TYPE (mesh) :: green_mesh

        integer i, j, k, iunit
        real laplace(green_mesh%msub,green_mesh%msub)
        real lambda
        integer hyp


        green_mesh%la(:,:) = 0.

        !Five point laplacian operator [0 1 0;1 -4 1;0 1 0]
        do i=1,green_mesh%msub
          green_mesh%la(i,i) = 4.
        enddo
        

        endsubroutine laplacian
