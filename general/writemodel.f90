      subroutine write_model(green_mesh,model,vecl)

      implicit none
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


      !call model_c(green_mesh%model2,green_mesh%model,green_mesh%interp_i,green_mesh%msub,green_mesh%slipm)


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
           write(iunit2,*) green_mesh%model(k)
           k = k + 1
          enddo
         enddo
         do m=1,green_mesh%interp_i
           write(iunit,*) green_mesh%slipr(m,:)
         enddo
       enddo


        
       close(iunit2)
       close(iunit)


      end subroutine write_model





      subroutine read_modelf(green_mesh)

      INCLUDE 'green.h'
!     optim type strcuture
      TYPE (mesh) :: green_mesh
      integer :: iunit, iunit2, m, i, j, k


      call coor_trans(green_mesh)
      green_mesh%model(:) = 0.
      green_mesh%model2(:) = 0.


!     Variables needed only
       iunit=22
       OPEN(iunit,FILE=green_mesh%dat//'modelpri.dat',&
  &         status='old',action='read')

       !Rearrange model info into a 1D vector
       k = 1
       do i=1,green_mesh%msub     !number of subfaults
         do m=1,green_mesh%interp_i
           read(iunit,*) green_mesh%slipr(m,:)
         enddo
         do j=1,green_mesh%ncomp
          do m=1,green_mesh%interp_i
           green_mesh%model(k) = green_mesh%slipr(m,j)
           k = k + 1
          enddo
         enddo
       enddo
       close(iunit)

      call model_d(green_mesh%model,green_mesh%model2,green_mesh%interp_i,green_mesh%msub,green_mesh%slipm)


      end subroutine read_modelf




      subroutine read_model(green_mesh)

      implicit none
      INCLUDE 'green.h'
!     optim type strcuture
      TYPE (mesh) :: green_mesh

!     Variables needed only here
      integer :: iunit, i, j, k, m, jump

      !Flush the array used to read prior model (slip-rate)
      green_mesh%modelp(:) = 0.

      jump = 151 - 75         !to be changed

       iunit=22
       OPEN(iunit,FILE=green_mesh%dat//'modelpri.dat',&
  &         status='old',action='read')

       k = 1
       do i=1,green_mesh%msub     !number of subfaults
         do j=1,green_mesh%ncomp
          do m=1,green_mesh%syn_sam               !only first time window used
           read(iunit,*) green_mesh%modelp(k)
           k = k + 1
          enddo
           k = k + jump - 1
         enddo
       enddo

       !Set prediction here


       close(iunit)


      end subroutine read_model




      subroutine read_modelpri(green_mesh)

      implicit none
      INCLUDE 'green.h'
!     optim type strcuture
      TYPE (mesh) :: green_mesh

!     Variables needed only here
      integer :: k, i, j, iunit, m, iunit2


       iunit=22
       OPEN(iunit,FILE=green_mesh%dat//'modelpri.dat',&
  &         status='old')

       k = 1
       do i=1,green_mesh%msub     !number of subfaults
         do j=1,green_mesh%ncomp
          do m=1,green_mesh%interp_i
           read(iunit,*) green_mesh%modelp(k)
           k = k + 1
          enddo
         enddo
       enddo

       close(iunit)


      end subroutine read_modelpri


