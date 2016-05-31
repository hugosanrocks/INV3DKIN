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
 
        allocate(g(green_mesh%msub*green_mesh%ncomp*green_mesh%interp_i))

         call forward(green_mesh)

         !Compute residuals for all stations and components
         call residual(green_mesh)

!IN FREQUENCY DOMAIN
!         do i = 1,green_mesh%msub
!           green_mesh%tfft_i = i
!           call adj_trac(green_mesh)
!         enddo
!FREQUENCY DOMAIN

!      TIME DOMAIN GRADIENT COMPUTATION
       call conadjtime(green_mesh)


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
       !TIKONOV Term
       !call model_pri(green_mesh)
       !print *, 'prior', green_mesh%costm
       !Estimate cost from model term and gradient contribution
       !call modeltimer(green_mesh)
       !call model1d(green_mesh)    !space correlation
       !call model_edge(green_mesh)  !edge effect
       !call modeltime(green_mesh)  !time correction
       !print *, 'costa', green_mesh%costa, 'cost corr', green_mesh%costm
       green_mesh%costa = green_mesh%costa !+ &
!  &    green_mesh%lam1*green_mesh%costm
!       green_mesh%grad2(:) = green_mesh%grad2(:) + &
!  &    green_mesh%lam1*green_mesh%gradad(:)
       !call modeltimer(green_mesh)  !time correction
       !print *, 'costa', green_mesh%costa, 'cost time', green_mesh%costm
       !green_mesh%costa = green_mesh%costa + green_mesh%lam2*green_mesh%costm
       !green_mesh%grad2(:) = green_mesh%grad2(:) + green_mesh%lam2*green_mesh%gradad(:)
       print *, 'Cost: ', green_mesh%costa

      ! do i=1,green_mesh%modelsize2
      !   write(81,*) green_mesh%model2(i)
      !   write(83,*) green_mesh%gradad(i)
      ! enddo

      end subroutine new_grad
