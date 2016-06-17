!--------------------------------------------------------
!     3D Kinematic Seismic Source Inversion
!     by an adjoint-state method
!--------------------------------------------------------
!------------- RESIDUALS SUBROUTINE -----------------------
!
!     This program:
!
!    3) Adjoint:
!
!     3.2) Estimate the residuals (r=syn-obs) at each station 
!          and compnent. Both, synthetics and observed seismograms
!          are reversed in time t'=T-t
!
!----------------------------------------------------------------------
!    Author:
!    Hugo S. Sanchez Reyes 29/06/15
!    Universite de Grenoble 1 "Joseph Fourier"
!    Supervisors: J. Tago, V.M. Cruz-Atienza, V. Hjorleifsdottir, 
!                 L. Metivier, J. Virieux
!---------------------------------------------------------------------

      subroutine residual(green_mesh)

      !COMMON VARIABLES
      implicit none
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      integer i, j, k


      !Flush residual array
      green_mesh%res(:,:)=0.d0
      
      !Flush the cost function before computation
      green_mesh%costa=0.d0

      !ALL residuals have same length      
!      green_mesh%res(:,:) = green_mesh%syn(:,:) - green_mesh%obs(:,:)

      
      !Invert for different time windoes
      k = 1
      do i=1,green_mesh%nsta
        do j=1,green_mesh%ncomp
          green_mesh%res(1:green_mesh%samwin(i,2),k) = &
  &       green_mesh%syn(1:green_mesh%samwin(i,2),k) - green_mesh%obs(1:green_mesh%samwin(i,2),k)
          k = k + 1
        enddo
      enddo

      !Multiply by data covariance matrix (Weighting factors)
      call prod_res(green_mesh)                                                                           !lenobs = syn_i

      !Print only if you want to check
      !call write_residual(green_mesh)

      !Transform residuals to Frequency domain
      !turn off this if convolutions are done in tme
      !call resi_fft(green_mesh)

      end subroutine residual


      subroutine read_obs (green_mesh)

  USE INIT_BUTTERWORTH_MOD
  USE FILTFILT_BUTTERWORTH_MOD

      !COMMON VARIABLES
      implicit none
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh
      TYPE (butter) :: butt

      integer ii, jj, k, iunit, p
      integer nrec, nstep
      real dt

      p=1

       do ii=1,green_mesh%nsta
        !Loop over 3 cartesian components
        do jj=1,green_mesh%ncomp
         write(green_mesh%sta,'(I3.3)') ii
         write(green_mesh%comp,'(I1.1)') jj
         !Unit to read observations
         OPEN(iunit,FILE=green_mesh%dat//'obs_S'//green_mesh%sta//'_C'//green_mesh%comp,&
    &    status='unknown')
         do k=1,green_mesh%lenobs               !syn_i = lenobs    !2 june
          read(iunit,*) green_mesh%obs(k,p)     !syn_i = interp_i  !31 may
         enddo
         close(iunit)
        p=p+1
        enddo !loop over 3 components
       enddo  !loop over number of stations

       
       if (green_mesh%optf .eq. 1) then
          !-------------------------------------------------------------------
          !OPTIONAL FILTER APPLIED TO GREEN'S FUNCTIONS
          butt%order = 2
          butt%fc = 1.
          nstep = green_mesh%lenobs
          nrec  = green_mesh%stcomp
          dt    = green_mesh%slipdt
          !-------------------------------------------------------------------
          !Parameters needed
       do ii=1,green_mesh%stcomp
          !Asign the time series to be filtered
          green_mesh%tseries(:) = green_mesh%obs(:,ii)
          !Initialize the butterworth filter
          CALL INIT_BUTTERWORTH(butt, dt)
          ! Apply the butterworth filter
          CALL FILTFILT_BUTTERWORTH(green_mesh%tseries, butt, nstep)
          !copy_tseries(:,2) = tseries(:)
          green_mesh%obs(:,ii) = green_mesh%tseries(:)
          ! Print the resultsprint input and filtered signals to output file
          !-----------------------------------------------------------------
          ! END OF OPTIONAL FILTER APPLIED TO GREEN'S FUNCTIONS
          !-----------------------------------------------------------------
      enddo
      else
       print *, ' Observation not filtered'
      endif

       call weight_cov(green_mesh)

      end subroutine read_obs



      subroutine resi_fft (green_mesh)

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

        do k = 1, green_mesh%stcomp
        in(:)=0.
          do j = 1, green_mesh%interp_i
            in(j) = green_mesh%res(j,k)
          enddo

        !do j = 1, green_mesh%lensyn
        !  write(16,*) j, in(j)
        !enddo
        !execute the plan to transform the IN data to
        !the OUT FFT coefficients.

        call sfftw_execute_ ( plan_forward )

        !do j = 1, green_mesh%lensynf
        !  write(16,*) j, out(j)
        !enddo


     !USED TO CHECK BACK FFT VALUES
     ! call sfftw_plan_dft_c2r_1d_ ( plan_backward, green_mesh%lensyn, out, in,&
     !&  FFTW_ESTIMATE )
     ! call sfftw_execute_ ( plan_backward )
     ! do j=1,green_mesh%lensyn
     ! write(16,*) j,in(j)/dble(green_mesh%lensyn)
     ! enddo
     ! call sfftw_destroy_plan_ ( plan_backward )
     ! stop

      green_mesh%resif(:,k) = out(:)

      enddo

      call sfftw_destroy_plan_ ( plan_forward )

      return
      end subroutine resi_fft






      subroutine weight_cov(green_mesh)

      implicit none
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

 !     real,dimension(:),allocatable :: x, y
      integer i,j,k
      integer iunit
      real    weights(3)


      green_mesh%cd(:,:)=0.

      !Read weights given for each recording
      k=1
      iunit=22
      open(iunit,file=green_mesh%dat//'weights.dat',status='old',action='read')
      do i=1,green_mesh%nsta
       read(iunit,*) weights
       do j=1,3
        green_mesh%cd(k,k) = weights(1+(j-1))
        k=k+1
       enddo
      enddo
      close(iunit)


      endsubroutine weight_cov



      subroutine prod_res(green_mesh)

      !COMMON VARIABLES
      implicit none
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      integer i, j, k, l, iunit, mm
      real weights(3), cost, costa, al, be
      real, dimension(:), allocatable :: resvec, vec
      real, dimension(:,:), allocatable :: weightm, resvect

      !MKL matrix multiplication coefficients
      al = 1.d0
      be = 0.d0
      costa = 0.

      !All recordings with weight from file (Cd matrix)
      if (green_mesh%weig .eq. 2) then
       iunit = 22
       open(iunit,file=green_mesh%dat//'weights.dat',status='old',action='read')
       k = 1
       do i=1,green_mesh%nsta
        mm = green_mesh%samwin(i,2)
        read(iunit,*) weights
        allocate(weightm(mm,mm),resvect(1,mm))
        allocate(resvec(mm),vec(mm))
        resvec(:) = 0.
        vec(:) = 0.
        weightm(:,:) = 0.
        do j=1,green_mesh%ncomp
            !Multiplication of matrices 
            resvec(:) = green_mesh%res(1:mm,k)
            do l=1,mm
               weightm(l,l) = weights(j)
            enddo
            call sgemm('N','N',mm,1,mm,al,        &
      &       weightm,mm,resvec,mm,be,vec,mm)
            resvect(1,:) = vec(:)
            call sgemm('N','N',1,1,mm,al,        &
      &       resvect,1,vec,mm,be,cost,mm)
            !Cumulative cost
            costa = costa + cost
            !Residuals weighted
            green_mesh%res(1:mm,k) = vec(:)
          k = k + 1
        enddo
       deallocate(weightm,resvec,resvect,vec)
       enddo
       close(iunit)
      !All recordings with weight = 1
      elseif (green_mesh%weig .eq. 1) then 
       call fcost(green_mesh%res,green_mesh%stcomp,green_mesh%syn_i,costa)
       !print *, 'm3', m3
      else 
       print *, 'Wrong weighting value: Check dat/weights.info'
      endif


      green_mesh%costa = costa
      !print *, green_mesh%costa, 'costa'

      endsubroutine prod_res



