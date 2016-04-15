      subroutine write_model(green_mesh,model,vecl)

      INCLUDE 'green.h'
!     optim type strcuture
      TYPE (mesh) :: green_mesh

!     Variables needed only here
      integer, intent(inout) :: vecl
      ! vecl = dimension of the problem
      real, intent(inout) :: model(vecl)                   ! current gradient
      real y(439*3*1)
!     Variables needed only here
      integer :: iunit, iunit2, m, i, j, k


      call model_c(green_mesh%model2,green_mesh%model,green_mesh%interp_i,green_mesh%msub,green_mesh%slipm)


       iunit2=44
       OPEN(iunit2,FILE=green_mesh%out//'model.out',&
  &         status='unknown')

       iunit=22        
       OPEN(iunit,FILE=green_mesh%dat//'slip_xyz.ascii',&
  &         status='unknown')

       !Rearrange model info into a 1D vector
       k = 1
       do i=1,green_mesh%msub     !number of subfaults
         do j=1,green_mesh%ncomp
          do m=1,green_mesh%interp_i
           green_mesh%slipr(m,j) = green_mesh%model(k) !3D coordinates slip-rate
           k = k + 1
          enddo
         enddo
         do m=1,green_mesh%interp_i
           write(iunit,*) green_mesh%slipr(m,:)
         enddo
       enddo

       write(iunit2,*) green_mesh%model(:)
       close(iunit2)
       close(iunit)


      end subroutine write_model



      subroutine read_model(green_mesh)

      INCLUDE 'green.h'
!     optim type strcuture
      TYPE (mesh) :: green_mesh

!     Variables needed only here
      integer :: iunit


       iunit=22
       OPEN(iunit,FILE=green_mesh%out//'model.out',&
  &         status='unknown')


           read(22,*) green_mesh%model


       close(iunit)


      end subroutine read_model

