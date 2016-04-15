!--------------------------------------------------------
!     3D Kinematic Seismic Source Inversion
!     by an adjoint-state method
!--------------------------------------------------------
!------------- ADJOINT SUBROUTINE -----------------------
!
!     This program:
!
!    3) Adjoint:
!
!     3.2) Estimate the residuals (r=syn-obs) at each station 
!          and compnent. Both, synthetics and observed seismograms
!          are reversed in time t'=T-t
!     3.4) The residuals at the recievers are retropropagated to
!          the fault and the total traction at the fault is 
!          estimated by convolving the pseudo-Green's functions
!          with the residuals (adjoint formulation).
!
!----------------------------------------------------------------------
!    Author:
!    Hugo S. Sanchez Reyes 29/06/15
!    Universite de Grenoble 1 "Joseph Fourier"
!    Supervisors: J. Tago, V.M. Cruz-Atienza, V. Hjorleifsdottir, 
!                 L. Metivier, J. Virieux
!---------------------------------------------------------------------


      subroutine adjoint(green_mesh)

!----------------------Definition of variables--------------------------------------

      implicit none

      !COMMON VARIABLES
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      !local variables
      integer i
      real start, fini


         !read observed seismograms at stations
         call read_obs(green_mesh)

         !Compute residuals for all stations and components
         call residual(green_mesh)
         print *, 'Finish residual estimation'
!         stop
         green_mesh%tottrac(:,:)=0.

         call cpu_time(start)
         do i = 1,green_mesh%msub
           !Jump inside traction files for m-subfaults
           green_mesh%tfft_i = i
           call adj_trac(green_mesh)
         enddo
         call cpu_time(fini)
         print *, 'Time for adjoint:', fini-start

         !do i=1,green_mesh%lensyn
         !write(100,*) i,green_mesh%tottrac(i,:)
         !enddo
         
      end subroutine adjoint
