
!--------------------------------------------------------
!     3D Kinematic Seismic Source Inversion
!     by an adjoint-state method
!--------------------------------------------------------
!------------- FORWARD SUBROUTINE -----------------------
!
!     This program:
!
!     2) Forward:
!
!      2.1) Using the sliprate module for each subfault -> green_mesh%slipr
!           and the pseudo-Green's functions            -> green_mesh%traction
!           computes the synthetic seismograms.
!
!----------------------------------------------------------------------
!    Author:
!    Hugo S. Sanchez Reyes 29/06/15
!    Universite de Grenoble 1 "Joseph Fourier"
!    Supervisors: J. Tago, V.M. Cruz-Atienza, V. Hjorleifsdottir, 
!                 L. Metivier, J. Virieux
!---------------------------------------------------------------------

      subroutine forward(green_mesh)

!----------------------Definition of variables--------------------------------------

      implicit none
      !COMMON variables
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

!     Variables needed only by this program
      integer i, j, k, m, iunit
      real start, fini, progres

       !Flush the array of synthetics
       green_mesh%syn(:,:) = 0.


       !Read traciton in time domain
       call read_time(green_mesh)
       !USED ONLY TO CHECK WRITING AND READING
       !do i=1,green_mesh%interp_i
       ! write(123,*) green_mesh%tractionvec(i,22:24)
       !enddo

       k=1
       do i=1,green_mesh%msub          !number of subfaults  
         !Arrange model (slip)
         do j=1,green_mesh%ncomp
          do m=1,green_mesh%interp_i
           print *, j+(i-1)*3, m, k
           green_mesh%slip(m,j+(i-1)*3) = green_mesh%model(k)
           k = k + 1
          enddo
         enddo
       enddo



       !Read tractions in the frequency domain
       call read_fft(green_mesh)

       call cpu_time(start)
       k = 1
       !write(*,'(a)')'----------100%'
       do i=1,green_mesh%msub          !number of subfaults  
         green_mesh%tfft_i = i
         !Arrange model (slip)
         do j=1,green_mesh%ncomp
          do m=1,green_mesh%interp_i
           green_mesh%slipr(m,j) = green_mesh%model(k)
           k = k + 1
          enddo
         enddo
         call slipfft(green_mesh)
        !Computation of synthetic velocity records
         call syn_velo(green_mesh)
         !Progress bar *********!         
         !progres = mod(i,(green_mesh%msub/10))
         !if (progres .eq. 0) then 
         !call progressbar(i)
         !endif
         !This part is not necessary for the forward problem!
       enddo
       call cpu_time(fini)
       !write(*,*)
       !print *, 'time for synthetics:', fini-start


!*********************************************************
!      Low-pass Filter the synthetic seismograms if needed
!      (NOT WORKING YET)
!      call filter_trac(green_mesh)
!*********************************************************

!      iunit=15
!      !Write information about synthetics
!      OPEN(iunit,file=green_mesh%dat//'syn.info',status='unknown')
!      write(iunit,*) green_mesh%interp_i
!      write(iunit,*) green_mesh%nsta, green_mesh%ncomp
!      close(iunit)
      

      end subroutine forward




      subroutine slipfft (green_mesh)!,n,nc,slipf)

      implicit none
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      include "fftw3.f"

      integer i, j, k, cont
      integer*8 plan_forward, plan_backward
      !double precision in(green_mesh%lensyn)
      !double complex out(green_mesh%lensynf)
      complex out(green_mesh%lensynf)
      real in(green_mesh%lensyn)

      call sfftw_plan_dft_r2c_1d_ ( plan_forward, green_mesh%lensyn, in, out,&
     &  FFTW_ESTIMATE )

      in(:)=0.

        do k = 1, green_mesh%ncomp
          do j = 1, green_mesh%interp_i
            in(j) = green_mesh%slipr(j,k)
          enddo

        !do j = 1, green_mesh%interp_i
        !  write(16,*) j, in(j)
        !enddo
        !execute the plan to transform the IN data to
        !the OUT FFT coefficients.
        call sfftw_execute_ ( plan_forward )

        green_mesh%slipf(:,k)=out(:)

     !USED TO CHECK BACK FFT VALUES
     ! call sfftw_plan_dft_c2r_1d_ ( plan_backward, green_mesh%lensyn, out, in,&
     !&  FFTW_ESTIMATE )
     ! call dfftw_execute_ ( plan_backward )
     ! do j=1,green_mesh%lensyn
     ! write(16,*) j,in(j)/dble(green_mesh%lensyn)
     ! enddo
     ! call dfftw_destroy_plan_ ( plan_backward )

      enddo

      call sfftw_destroy_plan_ ( plan_forward )

      return
      end subroutine slipfft
