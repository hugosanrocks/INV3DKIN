      subroutine read_grad(green_mesh)

      implicit none
      ! Define all the variables needed to read models and
      ! associated gradient, variables in ../include/green.h
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      ! Variables needed only here
      real, dimension(:), allocatable :: g
      integer :: n, i, j, k, m
      
      allocate(g(green_mesh%interp_i*green_mesh%ncomp*green_mesh%msub))
      ! Flush array
      g(:)=0.


       !Rearrange gradient info into a 1D vector
       k = 1
       do i=1,green_mesh%msub          !number of subfaults  
         n = (i-1)*green_mesh%ncomp
         do j=1,green_mesh%ncomp       !number of components
          do m=1,green_mesh%interp_i   !number of time samples
           g(k) = green_mesh%tottrac(m,j+n)
           k = k + 1
          enddo
         enddo
       enddo

      call model_d(g,green_mesh%grad2,green_mesh%interp_i,green_mesh%msub,green_mesh%slipm)


      end subroutine read_grad
