!--------------------------------------------------------
!     3D Kinematic Seismic Source Inversion
!     by an adjoint-state method
!--------------------------------------------------------
!------------- ADJOINT TRACTION SUBROUTINE --------------
!
!     This program:
!
!    3) Adjoint:
!
!      1) Takes the residuals at every station and component
!      2) Reads the pseudo Green functions 
!      3) Convolves this two vectors for each station
!         according to the adjoint formulation
!      4) Stacks the total traction for all stations
!         and all components.
!      5) Writes the resulting total traction due to
!         the residuals at ATXXXXX.bin
!
!----------------------------------------------------------------------
!    Author:
!    Hugo S. Sanchez Reyes 29/06/15
!    Universite de Grenoble 1 "Joseph Fourier"
!    Supervisors: J. Tago, V.M. Cruz-Atienza, V. Hjorleifsdottir, 
!                 L. Metivier, J. Virieux
!---------------------------------------------------------------------


      subroutine  adj_trac(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh

       !local variables
       REAL, DIMENSION(:), ALLOCATABLE :: X, Y, Z
       INTEGER ii, jj, i, j, k, ki, iunit, ijcomp, ijsub, ijsta
       INTEGER reclent, p, adjjump, start, fin, kx

!======================================================

       !units where to read the info required and write
       !the resulting traction
       iunit=15

       !'lent' length of traction vector
       INQUIRE(iolength=reclent) green_mesh%traction

       !Samples of delay due to the origin time of gaussian force
       green_mesh%delays=green_mesh%ot/green_mesh%slipdt

       !Flush the array
       green_mesh%synt(:,:) = 0.
!       green_mesh%tottrac(:,:) = 0.

!       call conadjtime(green_mesh)

!       do i=1,green_mesh%interp_i
!       write(89,*) green_mesh%tottrac(i,210)
!       enddo

       call conadjfft(green_mesh)

       fin = green_mesh%tfft_i * green_mesh%ncomp
       start = fin - 2
       !print *, start, fin, green_mesh%tfft_i
       !Save the total traction
       ki=green_mesh%interp_i+green_mesh%delays
       do i=1,green_mesh%interp_i
        green_mesh%tottrac(i,start:fin) = green_mesh%synt(ki,:)
        ki=ki-1
        kx=kx-1
       enddo

  
      end subroutine adj_trac






      subroutine conadjtime(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh
       ! FFT Library

      integer i, j, k, ii, jj, kk, mm, cont, m, p
      real scalfac
      real, dimension(:), allocatable :: x, y, z
      real, dimension(:,:), allocatable :: tottrac

      scalfac=green_mesh%moment/(green_mesh%msub)

      allocate(x(green_mesh%interp_i),y(green_mesh%interp_i),z(green_mesh%lensyn))
      allocate(tottrac(green_mesh%interp_i,green_mesh%msub*green_mesh%ncomp))

      tottrac(:,:) = 0.

      cont = 1
       do i=1,green_mesh%msub
        do j=1,green_mesh%nsta
        do m=1,green_mesh%ncomp
         do k=1,green_mesh%ncomp   !Loop over 3 traction components
          ii = m + (j-1)*3
          jj = m + (j-1)*3
          kk = k + (j-1)*3 + (j-1)*green_mesh%ncomp*green_mesh%ncomp+(i-1)*green_mesh%stcomp*3
          mm = k + (i-1)*3
!          used to check elements to be convolved
!          print *, 'res', ii, 'conv tract', cont, 'adj', mm, 'time'
          x(:) = green_mesh%res(:,ii) 
          y(:) = green_mesh%tractionvec(:,cont) 
!         !Convolve slip rate model with the Green functions
          call conv(x,green_mesh%interp_i,y,green_mesh%interp_i,&
  &                z,green_mesh%lensyn)
!         !Discrete convolution term
          z(:)=z(:)*green_mesh%slipdt*scalfac
!          do p=1,green_mesh%interp_i
!           write(15,*) z(green_mesh%delays+p)
!          enddo
!         !Stack the three convolutions X, Y, Z
         tottrac(:,mm)= tottrac(:,mm)+z(green_mesh%delays+1:green_mesh%delays+green_mesh%interp_i)
         cont = cont + 1
         enddo     !Loop over 3 traction components
        enddo
       enddo
      enddo

      k = green_mesh%interp_i
      do i=1,green_mesh%interp_i
       green_mesh%tottrac(i,:) = tottrac(k,:)
       k = k - 1
      enddo


      deallocate(x,y,z)
      deallocate(tottrac)
      endsubroutine conadjtime









      subroutine conadjfft(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh

       include "fftw3.f"

!      double complex in1(green_mesh%lensynf), in2(green_mesh%lensynf), inh(green_mesh%lensynf)
!      double precision outr(green_mesh%lensyn)
      complex in1(green_mesh%lensynf), in2(green_mesh%lensynf), inh(green_mesh%lensynf)
      real outr(green_mesh%lensyn)

      integer*8 plan_backward
      integer i, j, k, jj, ii, cont, m, kk

!  Set up a plan, and execute the plan to transform the IN data to
!  the OUT FFT coefficients.

       kk=1+((green_mesh%tfft_i-1)*green_mesh%stcomp*green_mesh%ncomp)
       ii = green_mesh%ncomp*green_mesh%stcomp
       jj = 1 
       do i=1,green_mesh%nsta       !stations
         do j=1,green_mesh%ncomp    !slip components
          do k=1,green_mesh%ncomp   !traction components
!            print *, 'comp res',j+(i-1)*green_mesh%ncomp,&
!     &      'comp tract',kk,&
!     &      'tot tract',k+(green_mesh%tfft_i-1)*green_mesh%ncomp
!                      slip component, traction component, synthetic tract
         in1(:) = green_mesh%resif(:,j+(i-1)*green_mesh%ncomp)
         in2(:) = green_mesh%tracf(:,kk)
         !do m=1,green_mesh%lensynf
         !write(222,*) m,in1(m)
         !write(333,*) m,in2(m)
         !enddo
         inh(:) = in1(:) * in2(:) 
         cont = k
         call back_fft(green_mesh,inh,green_mesh%lensynf,green_mesh%lensyn,outr,cont)
!         print *, 'res', j+(i-1)*green_mesh%ncomp, 'trac', kk, 'syn', cont, 'freq'
          kk = kk + 1
          enddo
          jj = jj + 1
         enddo
       enddo
!
!  Terminate.
!
      return
      end subroutine conadjfft




      subroutine back_fft(green_mesh,outr,nc,n,inh,cont)

      implicit none
       !COMMON VARIABLES
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh
      !Dimension of vectors
      integer, intent(inout) :: nc, n, cont
      !Vectors to be convolved
!      double complex, intent(inout) :: outr(nc)
!      double precision, intent(inout) :: inh(n)
      complex, intent(inout) :: outr(nc)
      real, intent(inout) :: inh(n)
      real scalfac

      include "fftw3.f"

      integer*8 plan_backward
      integer i, p

       !For homogeneous media, scalfac has the same mu for all subfaults
       !evereything done in preprocess stage
       !scalfac=green_mesh%moment/(green_mesh%mu*green_mesh%msub)
       scalfac=green_mesh%moment/(green_mesh%msub)

       !print *, scalfac, 'adj'

      call sfftw_plan_dft_c2r_1d_ ( plan_backward, n, outr, inh,&
     &  FFTW_ESTIMATE )

      call sfftw_execute_ ( plan_backward )
      !do i=1,green_mesh%lensyn
      !write(444,*)i, inh(i)/dble(n)
      !enddo
      !print *, cont, green_mesh%tfft_i
      green_mesh%synt(:,cont) = green_mesh%synt(:,cont) + (inh(:)*green_mesh%slipdt*scalfac/dble(n))
!          do p=1,green_mesh%interp_i
!           write(16,*) inh(p+green_mesh%delays)*green_mesh%slipdt*scalfac/dble(n)
!          enddo

      !do i=1,3
      !print *, green_mesh%synt(500,i), inh(500)/dble(n)
      !enddo

      call sfftw_destroy_plan_ ( plan_backward )

      return
      endsubroutine back_fft


