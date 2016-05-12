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
      integer i


      !Flush residual array
      green_mesh%res(:,:)=0.d0

      !Flush the cost function before computation
      green_mesh%costa=0.d0

      green_mesh%res(:,:) = green_mesh%syn(:,:) - green_mesh%obs(:,:)
    
      !Multiply by data covariance matrix (Weighting factors)
      call prod_res(green_mesh)
      
      !Time reverse for adjoint force
      call order_inv(green_mesh%res,green_mesh%interpadj_i,green_mesh%stcomp)
 
      !Print only if you want to check
      !call write_residual(green_mesh)

      !Transform to the frequency domain
      call resi_fft(green_mesh)

      end subroutine residual


      subroutine read_obs (green_mesh)

      !COMMON VARIABLES
      implicit none
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      integer ii, jj, k, iunit, p

      p=1

       do ii=1,green_mesh%nsta
        !Loop over 3 cartesian components
        do jj=1,green_mesh%ncomp
         write(green_mesh%sta,'(I3.3)') ii
         write(green_mesh%comp,'(I1.1)') jj
         !Unit to read observations
         OPEN(iunit,FILE=green_mesh%dat//'obs_S'//green_mesh%sta//'_C'//green_mesh%comp//'',&
    &    status='unknown')
       !  do k=1,green_mesh%interp_i
          read(iunit,*) green_mesh%obs(1:green_mesh%interp_i,p)
       !  enddo
         close(iunit)
        p=p+1
        enddo !loop over 3 components
       enddo  !loop over number of stations

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

      integer i,j,k
      real m1(green_mesh%stcomp), m2(green_mesh%stcomp), m3
      real matr(green_mesh%interp_i,green_mesh%stcomp), m4(green_mesh%stcomp)

      !Value of misfit (cost)
      m3=0.
      matr(:,:) = 0.

      !All recordings with weight from file (Cd matrix)
      if (green_mesh%weig .eq. 2) then
       do k=1,green_mesh%interp_i
        m1(:) = 0.
        !res' * cd'
        do i=1,green_mesh%stcomp
         do j=1,green_mesh%stcomp
          m1(i) = m1(i) + green_mesh%res(k,j)*green_mesh%cd(j,i)
         enddo
        enddo
        !print *, 'm1', m1(45)
        m2(:) = 0.
        !cd * res
        do i=1,green_mesh%stcomp
         do j=1,green_mesh%stcomp
          m2(i) = m2(i) + green_mesh%cd(i,j)*green_mesh%res(k,j)
         enddo
        enddo
        !print *, 'm2', m2(45)
        m4(:)=0.
        do i=1,green_mesh%stcomp
         m4(i) = green_mesh%cd(i,i)*m2(i)
        enddo
        matr(k,:) = m4(:)
        !res'*cd'*cd*res
        do i=1,green_mesh%stcomp
          m3 = m3 + m2(i)*m1(i)
        enddo
       enddo
       green_mesh%res(:,:) = matr(:,:)
      !All recordings with weight = 1
      elseif (green_mesh%weig .eq. 1) then 
       m3 = 0.
       call fcost(green_mesh%res,green_mesh%stcomp,green_mesh%interp_i,m3)
       !print *, 'm3', m3
      else 
       print *, 'Wrong weighting value: Check dat/weights.info'
      endif


      green_mesh%costa = m3


      endsubroutine prod_res



