!----------------------------------------------------------------------
!
!     Inverse problem for 3D kinematic source inversion
!     by an adjoint-state method approach
!
!     ------------- Procedure info ---------------------------
!
!     This program:
!
!     1) Preprocess:
!
!      1.1) Reads the: 
!              - focal mechanism information.
!              - simulation (GEODG3D) information.
!              - slip rate model information (AXITRA).
!      1.2) Using this information, the program 
!           performs a coordinate transformation
!           slip rate (x,y,z).
!      1.3) Prepares the stress tensor by:
!             - Reading files P_CX TAU_*_CX SIGMA_**_CX.
!             - Rearranging this files into a unique file
!               for each station and component.
!             - Interpolating at the stress tensor at 
!               sliprate sampling rate.
!      1.4) Estimating the unitary traction vector at each 
!           staion and component.
!   
!     2) Forward:
!
!      2.1) Using the sliprate model and unitary traction
!           vector, computes the synthetic seismograms.
!
!    3) Adjoint:
!     
!     3.1) First, read the adjoint information (samples, etc.)
!     3.2) Estimate the residuals (r=syn-obs) at each station 
!          and compnent. Both, synthetics and observed seismograms
!          are reversed in time t'=T-t
!     3.4) The residuals at the recievers are retropropagated to
!          the fault and the total traction at the fault is 
!          estimated by convolving the untary traction vector
!          with the residuals (adjoint formulation).
!
!    4) fwi_pstd:
!
!     4.1) Searchs for a sliprate model that minimizes the 
!          cost function (L2). The steepest descent method
!          (PSTD) is used inside a loop, where the updating 
!          parameter to compute is the gradient (total
!          traction of adjoint problem).
!----------------------------------------------------------------------
!    Hugo S. Sanchez Reyes 29/05/15
!    Universite de Grenoble 1 "Joseph Fourier"
!    Supervisors: J. Tago, V.M. Cruz-Atienza, V. Hjorleifsdottir, J. Virieux
!---------------------------------------------------------------------

      subroutine initializeadj(green_mesh)

!----------------------Definition of variables--------------------------------------
      !COMMON VARIABLES
      implicit none
      !Define all the variables needed to read stresses
      ! and calculate the tractions associated "Green's
      ! functions", variables in include/green.h
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh


      !Estimate the tractions (adjoint problem) using as 
      !forces the velocity residuals at the recievers
      !lensynt = dimension of adjoint traction vectors

      !Save memory for traction arrays, total traction (gradient), cost and residuals
      allocate(green_mesh%tottrac(green_mesh%interp_i,green_mesh%ncomp*green_mesh%msub))
      !allocate(green_mesh%tracad(green_mesh%lensyn,green_mesh%ncomp*green_mesh%msub))
      allocate(green_mesh%cost(green_mesh%ncomp*green_mesh%nsta))
      allocate(green_mesh%res(green_mesh%syn_i,green_mesh%stcomp))
      !interp_i = syn_i           changed

      !FREQUENCY DOMAIN ARRAYS
      !Residual in frequency domain
      !allocate(green_mesh%resif(green_mesh%lensynf,green_mesh%stcomp))
      !allocate(green_mesh%synt(green_mesh%lensyn,green_mesh%ncomp))

      !Read simulation, sliprate and adjoint problem info
      call read_adjinfo(green_mesh)


      end subroutine initializeadj
