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

!  USE INIT_BUTTERWORTH_MOD
!  USE FILTFILT_BUTTERWORTH_MOD



      program PREP_GEODG

!----------------------Definition of variables--------------------------------------
      !COMMON VARIABLES
      implicit none
      !Define all the variables needed to read stresses
      ! and calculate the tractions associated "proc's
      ! functions", variables in include/proc.h
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh
      !Optimization TOOL_BOX
      include 'optim_type.h'
      type (optim_type) :: optim
!      include 'varprec.h'
!      type (var_type) :: var
      MYFLOAT var1

!     Variables needed only by this program
      integer i, j, k, iunit, fwiopt, m
!------------------------------------------------------

      real :: start, stop

!     Set working directories
     
!     Directory containing input data files P_C* TUA_C* SIGMA_**_C*
      green_mesh%dat='dat/'
!     Directory containing output data files
      green_mesh%out='out/'

      var1=1.d0
      print *, var1


!-----------------------------------------------------
      ! ONLY FOR GEODG3D OUTPUT FILES

      !Prepare pseudo proc functions as needed by the
      !inversion
       
      !call cpu_time(start)
      !Read general information and prepare stress tensor files
      call preprocess(green_mesh)
      !call cpu_time(stop)
      !print *, stop-start
!----------------------------------------------------


      end program PREP_GEODG
