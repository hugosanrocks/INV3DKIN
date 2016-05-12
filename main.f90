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
#include "precis.h"

      program INV3DKIN

!----------------------Definition of variables--------!
      ! COMMON VARIABLES
      IMPLICIT NONE
      ! Define all the GLOBAL variables needed
      ! variables inside include/green.h
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh
      ! Optimization TOOL_BOX
      include 'optim_type.h'
      type (optim_type) :: optim
      !INCLUDE 'varprec.h'
      !TYPE (var_type) :: vartype
      !MYFLOAT var1
      !Variables needed only by this program

      integer i, j, k, iunit, fwiopt, m
!------------------------------------------------------!
      ! Timming variables
      real :: start, fini

      ! Set working directories
      ! Directory containing input data files P_C* TUA_C* SIGMA_**_C*
      green_mesh%dat='dat/'
      ! Directory containing output data files
      green_mesh%out='out/'

      !var_type%var1=1.d0
      !print *, var_type%var1

!=====================================================!
!=====================================================!
      ! ONLY FOR GEODG3D OUTPUT FILES
      ! UNCOMMENT IF YOU WISH TO DO PREPROCESS

      ! Prepare pseudo Green functions as needed
      !call preprocess(green_mesh)
!===================================================!
!===================================================!
      ! INITIAL STEPS

      ! Read information, time samples, focal info
      ! reciever's geometry, optim. strategy, etc.
      call read_info(green_mesh)
      ! Initialize arrays
      call initialize(green_mesh)
      ! Estimate slip vector direction
      call coor_trans(green_mesh)
!==================================================!
      ! INITAIL FORWARD AND ADJOINT MODELING

      ! Initialize model covariance matrix
!      call exp_covar(green_mesh)
!      call time_corr(green_mesh)
!      call edge(green_mesh)

      ! First iteration (slip-rate = vitesse.out*vslip)
      green_mesh%iter=0
      write(green_mesh%iter_i,'(I5.5)') green_mesh%iter

      ! Compute the first synthetics associated to 
      ! initial model (vitesse.out*vslip)
      call forward(green_mesh)

      IF (green_mesh%debug .eqv. .true.) THEN
      ! Write first synthetics to check
      call write_debug(green_mesh)
      ENDIF

      ! Estimate the tractions (adjoint problem) using as 
      ! forces the velocity residuals at the recievers
      ! lensynt = dimension of adjoint traction vectors
      call initializeadj(green_mesh)

      call adjoint(green_mesh)

      ! Use any optimization strategy to follow the -gradient 
      ! direction to find a model (solution)
      call cpu_time(start)
      call fwi_option(green_mesh,optim)
      call cpu_time(fini)
      print *, 'Time used for inversion: ', fini-start,'seconds'

!      call ortogonal(green_mesh)

      ! Destroy all the memory used
      call destroy_arrays(green_mesh)

      end program INV3DKIN



