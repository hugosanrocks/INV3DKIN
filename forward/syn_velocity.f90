!--------------------------------------------------------
!     3D Kinematic Seismic Source Inversion
!     by an adjoint-state method
!--------------------------------------------------------
!------------- SYNTHETIC VELOCITY RECORDINGS SUBROUTINE -
!
!     This program:
!
!      1) Uses the slip rate model (3 components) from 
!         green_mesh%slipr
!      2) Reads the pseudo-Green's functions 
!         (unitary traction) TRACT_SXXX.bin.
!      3) Convolves these two vectors for each station
!         according to the forward formulation.
!      4) Stack the corresponding components of these
!         convolutions to estimate the synthetic
!         records at each station SXXX and component CX
!      5) Writes the resulting synthetics
!
!----------------------------------------------------------------------
!    Author:
!    Hugo S. Sanchez Reyes 29/06/15
!    Universite de Grenoble 1 "Joseph Fourier"
!    Supervisors: J. Tago, V.M. Cruz-Atienza, V. Hjorleifsdottir, 
!                 L. Metivier, J. Virieux
!---------------------------------------------------------------------

      subroutine  syn_velo(green_mesh)!,syn)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh



       !Time delay due to origin time green_mesh%ot
       green_mesh%delays=green_mesh%ot/(green_mesh%slipdt)

 
     
       call confft(green_mesh)




      end subroutine syn_velo


      subroutine read_time(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh

       integer bit, iunit, i,j,k,l
       integer*8 reclent

       ! Bytes used for each element of traction vector (!frequency)
!       INQUIRE(iolength=bit) green_mesh%tracf(1,1)
 !      print *, bit, 'bit'
       bit=1
       ! Total bytes used for traction matrix (frequency)
       reclent=bit*green_mesh%ncomp*green_mesh%trac_i*green_mesh%stcomp*green_mesh%msub

       iunit=16
       OPEN(iunit,file='dat/TRACT_time.bin',status='unknown',&
 &          form='unformatted',ACCESS='DIRECT',recl=reclent)
         read(iunit,rec=1) green_mesh%tractionvec(:,:)
         !read(iunit,rec=green_mesh%tfft_i) green_mesh%tracf(:,:)
       close(iunit)

      end subroutine read_time



      subroutine contime(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh
       ! FFT Library

      integer i, j, k, ii, jj, kk, mm, cont, m
      real scalfac
      real, dimension(:), allocatable :: x, y, z

      scalfac=green_mesh%moment/(green_mesh%msub)

      allocate(x(green_mesh%interp_i),y(green_mesh%trac_i),z(green_mesh%lensyn))

      cont = 0
       do i=1,green_mesh%msub
        do j=1,green_mesh%nsta
        do m=1,green_mesh%ncomp
         do k=1,green_mesh%ncomp   !Loop over 3 traction components
          ii = m + (i-1)*3
          cont = k + (m-1)*3 + (j-1)*9 + (i-1)*green_mesh%stcomp*green_mesh%ncomp
          jj = m + (j-1)*3
          kk = m + (k-1)*3 + (j-1)*green_mesh%ncomp*green_mesh%ncomp+(i-1)*green_mesh%stcomp*3
          mm = k + (j-1)*3
!          used to check elements to be convolved
!          print *, 'slip', ii, 'conv tract', kk, 'syn', mm, 'contime'
          x(:) = green_mesh%slip(:,ii)  !ii = ii
          y(:) = green_mesh%tractionvec(:,kk) !cont = kk   jj= mm
!         !Convolve slip rate model with the Green functions
          call conv(x,green_mesh%interp_i,y,green_mesh%trac_i,&
  &                z,green_mesh%lensyn)
!         !Discrete convolution term
          z(:)=z(:)*green_mesh%slipdt*scalfac
!         !Stack the three convolutions X, Y, Z
         green_mesh%syn(:,mm)=green_mesh%syn(:,mm)+z(green_mesh%delays+1:green_mesh%delays+green_mesh%samwin(j,green_mesh%wininv)) !interp_i = syn_i  changed
                                                                                                                   !samwin = syn_i  2 june
         enddo     !Loop over 3 traction components
        enddo
       enddo
      enddo

      deallocate(x,y,z)
      endsubroutine contime




      subroutine read_fft(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh

       integer bit, iunit, i
       integer*8 reclent

       ! Bytes used for each element of traction vector (frequency)
       INQUIRE(iolength=bit) green_mesh%tracf(1,1)
       ! Total bytes used for traction matrix (frequency)
       reclent=bit*green_mesh%ncomp*green_mesh%lensynf*green_mesh%stcomp*green_mesh%msub

       iunit=16
       OPEN(iunit,file='dat/TRACT_fft.bin',status='unknown',&
 &          form='unformatted',ACCESS='DIRECT',recl=reclent)
         read(iunit,rec=1) green_mesh%tracf(:,:)
         !read(iunit,rec=green_mesh%tfft_i) green_mesh%tracf(:,:)
         !do i=1,green_mesh%lensynf
         !   write(15,*) green_mesh%tracf(i,1)
         !enddo
       close(iunit)



      end subroutine read_fft


      subroutine confft(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh
       ! FFT Library
       include "fftw3.f"

!      double complex in1(green_mesh%lensynf), in2(green_mesh%lensynf), inh(green_mesh%lensynf)
!      double precision outr(green_mesh%lensyn)
      complex in1(green_mesh%lensynf), in2(green_mesh%lensynf), inh(green_mesh%lensynf)
      real outr(green_mesh%lensyn)

      integer*8 plan_backward
      integer i, j, k, ii, jj, cont, m

!  Set up a plan, and execute the plan to transform the IN data to
!  the OUT FFT coefficients.

       ii = green_mesh%ncomp*green_mesh%stcomp
       jj = 1 
       do i=1,green_mesh%nsta             !stations
         do j=1,green_mesh%ncomp          !slip components
          do k=1,green_mesh%ncomp         !traction components
!           slip component in1, traction component in2
            in1(:) = green_mesh%slipf(:,j)
            in2(:) = green_mesh%tracf(:,&
  &         j+(k-1)*green_mesh%ncomp+((i-1)*(green_mesh%ncomp**2))+(green_mesh%tfft_i-1)*ii) 
            inh(:) = in1(:) * in2(:)      !convolution
            cont = k+(i-1)*3
!            print *, j,  j+(k-1)*green_mesh%ncomp+((i-1)*(green_mesh%ncomp**2))+(green_mesh%tfft_i-1)*ii, cont, 'confreq'
            call backfft(green_mesh,inh,green_mesh%lensynf,&
  &                      green_mesh%lensyn,outr,cont)
          enddo
          jj = jj + 1
         enddo
       enddo

      return
      end subroutine confft




      subroutine backfft(green_mesh,outr,nc,n,inh,cont)

      implicit none
      ! COMMON VARIABLES
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh
      !Dimension of vectors
      integer, intent(inout) :: nc, n, cont
      !Vectors to be convolved
!      double complex, intent(inout) :: outr(nc)
!      double precision, intent(inout) :: inh(n)
      complex, intent(inout) :: outr(nc)
      real, intent(inout) :: inh(n)

      ! FFT Library
      include "fftw3.f"

      integer*8 plan_backward
      integer i
      real scalfac

      call sfftw_plan_dft_c2r_1d_ ( plan_backward, n, outr, inh,&
     &  FFTW_ESTIMATE )

      call sfftw_execute_ ( plan_backward )

      !For homogeneous media, scalfac has the same mu for all subfaults
      !evereything done in preprocess stage
      !scalfac=green_mesh%moment/(green_mesh%mu*green_mesh%msub)
      scalfac=green_mesh%moment/(green_mesh%msub)
      
      !print *, green_mesh%delays+1, green_mesh%delays+green_mesh%interp_i,'check'
      green_mesh%syn(:,cont) = green_mesh%syn(:,cont) + &
  &                 (inh(green_mesh%delays+1:green_mesh%delays+green_mesh%interp_i)*scalfac*green_mesh%slipdt)/dble(n)
      
      call sfftw_destroy_plan_ ( plan_backward )



      return
      endsubroutine backfft
