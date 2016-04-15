      subroutine new_grad(green_mesh)

      implicit none
!     Define all the variables needed to read models and
!     associated gradient, variables in ../include/green.h
      INCLUDE 'green.h'
!     optim type strcuture
      TYPE (mesh) :: green_mesh

!     Variables needed only here
      integer :: n, i, j, k, m  ! dimension of the problem
      real,dimension(:),allocatable :: g
      real lambda


        !compose the slip for convolutions (x,y,z)!
        call model_c(green_mesh%model2,green_mesh%model,green_mesh%interp_i,green_mesh%msub,green_mesh%slipm)
        print *, green_mesh%model2(3500), green_mesh%model(3500), 'model dc'
        allocate(g(green_mesh%msub*green_mesh%ncomp*green_mesh%interp_i))

         call forward(green_mesh)
         print *, green_mesh%syn(120,45), 'syn for'

         !Compute residuals for all stations and components
         call residual(green_mesh)

         do i = 1,green_mesh%msub
           green_mesh%tfft_i = i
           call adj_trac(green_mesh)
         enddo

       green_mesh%grad2(:)=0.
       g(:)=0.
       !Rearrange model and gradient info in a vector
       k = 1
       do i=1,green_mesh%msub          !number of subfaults  
         !jump inside yoffe.src file, every column is a subfault
         !slip module
         n = (i-1) * green_mesh%ncomp
         do j=1,green_mesh%ncomp
          do m=1,green_mesh%interp_i
           g(k) = green_mesh%tottrac(m,j+n)
           k = k + 1
          enddo
         enddo
       enddo

      !Rearrange gradient into a 1D vector
      call model_d(g,green_mesh%grad2,green_mesh%interp_i,green_mesh%msub,green_mesh%slipm)
      !do i=1,green_mesh%modelsize2
      !  write(82,*) green_mesh%grad2(i)
      !enddo

       !ADDITIONAL MODEL TERM
       !Estimate cost from model term and gradient contribution
       ! call model1d(green_mesh)
       ! call model_edge(green_mesh)
       !  call model_time(green_mesh)
       ! print *, 'costa', green_mesh%costa!, 'cost time', green_mesh%costm
       ! green_mesh%costa = green_mesh%costa + green_mesh%lam*green_mesh%costm
       ! green_mesh%grad2(:) = green_mesh%grad2(:) + green_mesh%lam*green_mesh%gradad(:)
       !do i=1,green_mesh%modelsize2
       !  write(81,*) green_mesh%model2(i)
       !  write(83,*) green_mesh%gradad(i)
       !enddo

      end subroutine new_grad
