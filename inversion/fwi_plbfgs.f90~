     subroutine fwi_plbfgs(green_mesh,optim)

      implicit none
!     Define all the variables needed to read models and
!     associated gradient, variables in ../include/green.h
      INCLUDE 'green.h'
!     optim type strcuture
      TYPE (mesh) :: green_mesh
      include 'optim_type.h'
      type (optim_type) :: optim

!     Variables needed only here
      integer :: n, i, j, k, l, win(green_mesh%msub), cont                                        ! dimension of the problem
      real,dimension(:),allocatable :: grad_preco, model, grad     ! current gradient
      character*4 :: FLAG                                          ! communication FLAG 

!        Save memory for current model, current gradient, preco gradient
         allocate(grad_preco(green_mesh%modelsize2))
         allocate(model(green_mesh%modelsize2))
         allocate(grad(green_mesh%modelsize2))

!####### Initialize values ################################        
         n=green_mesh%modelsize2 ! dimension
         FLAG='INIT'             ! first flag
         optim%conv=1e-8         ! tolerance for the stopping criterion
         optim%print_flag=1      ! print info in output files 
         optim%debug=.false.     ! level of details for output files
         optim%l=20

         open(36,file='front.data',status='old',action='read')
         do i=1,green_mesh%msub
           read(36,*) win(i)
         enddo


         if (green_mesh%for_opt .ne. 1) then
         !Arrange slip rate model in 1D vector
         l = 1
         do i=1,green_mesh%msub
          if (win(i) .ne. 1) then
           cont=1+(i-1)*2*green_mesh%interp_i
           !Decompose slip vector along stk and along dip = 2 directions
            do j=1,2
             do k=1,green_mesh%interp_i
              model(l) = green_mesh%model2(cont)
              grad(l)  = green_mesh%grad2(cont)
              l = l + 1
             enddo
            enddo
          else
          endif
         enddo          
         endif


         if (green_mesh%for_opt .ne. 1) then
         !Arrange slip rate model in 1D vector
         l = 1
         do i=1,green_mesh%msub
          if (win(i) .ne. 1) then
           cont=1+(i-1)*2*green_mesh%interp_i
print *, 'back mod',cont, 'cut mod',l, i
           !Decompose slip vector along stk and along dip = 2 directions
            do j=1,2
             do k=1,green_mesh%interp_i
!              model(l) = green_mesh%model2(cont)
!              grad(l)  = green_mesh%grad2(cont)
              l = l + 1
             enddo
            enddo
          else
          endif
         enddo
         endif



!maybe needed
!         call read_grad(green_mesh)

!
!       green_mesh%grad2(:) = green_mesh%grad2(:) + &
!  &    green_mesh%lam1*green_mesh%gradad(:)


         grad_preco(:) = green_mesh%grad2(:)

!#########################################################

  !----------------------------------------------------!
  ! optimization loop: while convergence not reached or!
  ! linesearch not failed, iterate                     !
  !----------------------------------------------------
  !call model_check(green_mesh)

!
   do while ((FLAG.ne.'CONV').and.(FLAG.ne.'FAIL'))
     call PLBFGS(n,green_mesh%model2,green_mesh%costa,green_mesh%grad2,grad_preco,optim,FLAG)
     !call model_check(green_mesh)
     if(FLAG.eq.'GRAD') then
        call new_grad(green_mesh)
     endif
   enddo

      !Write last slip-rate aproximation
      call write_model(green_mesh,green_mesh%model2,green_mesh%modelsize2)
      call write_syn(green_mesh)



      deallocate(grad_preco,model,grad)
      end subroutine fwi_plbfgs
