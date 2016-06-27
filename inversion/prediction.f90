     subroutine prediction(green_mesh)

      implicit none
!     Define all the variables needed to read models and
!     associated gradient, variables in ../include/green.h
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

!     Variables needed only here
      integer :: n, iunit, i, j, k, l, cont                                        ! dimension of the problem
      integer :: predsize
      real,dimension(:),allocatable :: grad_preco, model, grad     ! current gradient
      character*4 :: FLAG                                          ! communication FLAG 


        green_mesh%idsub(:) = 0              !Order of subfaults to predict

        !Read form file and identify the subfaults where
        !the prediction will be done
        cont = 0
        j = 0
        do i=1,green_mesh%msub
          if (green_mesh%win(i,1) .le. green_mesh%dowin) then
             cont = cont + 1
          else
             j = j + 1
             green_mesh%idsub(j) = i
          endif
        enddo
print *, cont, 'cont'
                 !subf * comp * samples
        predsize = cont * 2 * (green_mesh%interp_i)
print *, predsize, 'predsize'

        allocate(model(predsize),grad(predsize))

!===================================================!
!     Forward and adjoint using the full model
!===================================================!
      call forward(green_mesh)
      call adjoint(green_mesh)
      call read_grad(green_mesh)
       call modeltimer(green_mesh)
       print *, 'Cost: ', green_mesh%costa, 'Cost time: ',green_mesh%costm
       green_mesh%lam1=0.05
       green_mesh%costa = green_mesh%costa + &
  &    green_mesh%lam1*green_mesh%costm
       green_mesh%grad2(:) = green_mesh%grad2(:) + &
  &    green_mesh%lam1*green_mesh%gradad(:)
       print *, 'costa', green_mesh%costa
       print *, green_mesh%costa, 'INITIAL COST'
!===================================================!

      !Arrange slip rate model in 1D vector
      !Remove subfaults not to update
      l = 1
      do i=1,green_mesh%msub
        if (green_mesh%win(i,1) .le. green_mesh%dowin) then
          cont=1+(i-1)*2*green_mesh%interp_i
!          print *, i, cont, l
          !Remove subfaults not to be updated
          do j=1,2
            do k=1,green_mesh%interp_i
              model(l) = green_mesh%model2(cont)
              grad(l) =  green_mesh%grad2(cont)
              cont = cont + 1
              l = l + 1
            enddo
          enddo
        else
        endif
      enddo

      call plbfgs_prediction(model,grad,predsize,green_mesh)

      !Arrange slip rate model in 1D vector
      !Insert back the subfaults that were removed
      l = 1
      do i=1,green_mesh%msub
        if (green_mesh%win(i,1) .le. green_mesh%dowin) then
          cont=1+(i-1)*2*green_mesh%interp_i
!          print *, 'back mod',cont, 'cut mod',l, i
          !Decompose slip vector along stk and along dip = 2 directions
          do j=1,2
            do k=1,green_mesh%interp_i
              green_mesh%model2(cont) = model(l)
              green_mesh%grad2(cont) = grad(l)
              cont = cont + 1
              l = l + 1
            enddo
          enddo
        else
          cont = 1+(i-1)*2*green_mesh%interp_i
!          print *, 'back mod',cont, 'from  mod',cont, i
          !Decompose slip vector along stk and along dip = 2 directions
          do j=1,2
            do k=1,green_mesh%interp_i
              green_mesh%model2(cont) = green_mesh%model2(cont)
              green_mesh%grad2(cont) = green_mesh%grad2(cont)
              cont = cont + 1
            enddo
          enddo
        endif
      enddo

      !Write last slip-rate aproximation
      call write_model(green_mesh,green_mesh%model2,green_mesh%modelsize2)
 print *, 'wrote model'
      call write_syn(green_mesh)

       deallocate(model,grad)
      end subroutine prediction






     subroutine plbfgs_prediction(model,grad,predsize,green_mesh)

      implicit none
!     Define all the variables needed to read models and
!     associated gradient, variables in ../include/green.h
      INCLUDE 'green.h'
!     optim type strcuture
      TYPE (mesh) :: green_mesh
      include 'optim_type.h'
      type (optim_type) :: optim

!     Variables needed only here
      integer :: n, i, j, k                                        ! dimension of the problem
      integer, intent(inout) :: predsize
      real,dimension(:),allocatable :: grad_preco                  ! current gradient
      character*4 :: FLAG                                          ! communication FLAG 
      real, intent(inout) :: model(predsize), grad(predsize)


!        Save memory for current model, current gradient, preco gradient
         allocate(grad_preco(predsize))

!####### Initialize values ################################        
         n=predsize              ! dimension
         FLAG='INIT'             ! first flag
         if (green_mesh%dowin .le. 1) then
         optim%conv=4e-02         ! tolerance for the stopping criterion
         elseif (green_mesh%dowin .gt. 1) then
         optim%conv=9e-02
         endif
         optim%print_flag=1      ! print info in output files 
         optim%debug=.false.     ! level of details for output files
         optim%l=20
         optim%niter_max=50

         grad_preco(:) = grad(:)

!#########################################################

  !----------------------------------------------------!
  ! optimization loop: while convergence not reached or!
  ! linesearch not failed, iterate                     !
  !----------------------------------------------------
  !call model_check(green_mesh)


   do while ((FLAG.ne.'CONV').and.(FLAG.ne.'FAIL'))
     call PLBFGS(n,model,green_mesh%costa,grad,grad_preco,optim,FLAG)
     if(FLAG.eq.'GRAD') then
        call new_gradpredict(model,grad,predsize,green_mesh)
     endif
   enddo



      deallocate(grad_preco)
      end subroutine plbfgs_prediction






      subroutine new_gradpredict(model,grad,predsize,green_mesh)

      implicit none
!     Define all the variables needed to read models and
!     associated gradient, variables in ../include/green.h
      INCLUDE 'green.h'
!     optim type strcuture
      TYPE (mesh) :: green_mesh

!     Variables needed only here
      integer :: n, i, j, k, l, cont, m             ! dimension of the problem
      integer, intent(inout) :: predsize
      real,dimension(:),allocatable :: g
      real lambda
      real, intent(inout) :: model(predsize), grad(predsize)


      !Arrange slip rate model in 1D vector
      !Insert back the subfaults that were removed
      l = 1
      do i=1,green_mesh%msub
        if (green_mesh%win(i,1) .le. green_mesh%dowin) then
          cont=1+(i-1)*2*green_mesh%interp_i
!          print *, 'back mod',cont, 'cut mod',l, i
          !Decompose slip vector along stk and along dip = 2 directions
          do j=1,2
            do k=1,green_mesh%interp_i
              green_mesh%model2(cont) = model(l)
              green_mesh%grad2(cont) = grad(l)
              cont = cont + 1
              l = l + 1
            enddo
          enddo
        else
          cont = 1+(i-1)*2*green_mesh%interp_i
          !Decompose slip vector along stk and along dip = 2 directions
          do j=1,2
            do k=1,green_mesh%interp_i
              green_mesh%model2(cont) = green_mesh%model2(cont)
              green_mesh%grad2(cont) = green_mesh%grad2(cont)
              cont = cont + 1
            enddo
          enddo
        endif
      enddo

        !compose the slip for convolutions (x,y,z)!
        call model_c(green_mesh%model2,green_mesh%model,green_mesh%interp_i,green_mesh%msub,green_mesh%slipm)
        allocate(g(green_mesh%msub*green_mesh%ncomp*green_mesh%interp_i))

         call forward(green_mesh)
         !Compute residuals for all stations and components
         call residual(green_mesh)
         !TIME DOMAIN GRADIENT COMPUTATION
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
       call modeltimer(green_mesh)
       print *, 'Cost: ', green_mesh%costa, 'Cost time: ',green_mesh%costm
       green_mesh%lam1=0.05
       green_mesh%costa = green_mesh%costa + &
  &    green_mesh%lam1*green_mesh%costm
       green_mesh%grad2(:) = green_mesh%grad2(:) + &
  &    green_mesh%lam1*green_mesh%gradad(:)
       print *, 'costa', green_mesh%costa

      !Arrange slip rate model in 1D vector
      !Remove subfaults not to update
      l = 1
      do i=1,green_mesh%msub
        if (green_mesh%win(i,1) .le. green_mesh%dowin) then
          cont=1+(i-1)*2*green_mesh%interp_i
          !Remove subfaults not to be updated
          do j=1,2
            do k=1,green_mesh%interp_i
              model(l) = green_mesh%model2(cont)
              grad(l) =  green_mesh%grad2(cont)
              cont = cont + 1
              l = l + 1
            enddo
          enddo
        else
        endif
      enddo

      deallocate(g)
      end subroutine new_gradpredict
