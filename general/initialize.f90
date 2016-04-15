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

      subroutine initialize(green_mesh)

!----------------------Definition of variables--------------------------------------
      !COMMON VARIABLES
      implicit none
      !Define all the variables needed to read stresses
      ! and calculate the tractions associated "Green's
      ! functions", variables in include/green.h
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh
     
      !Change debug = .false. not to write output files
      green_mesh%debug = .false. 


!Coordinate transformation (strike,dip,rake) to (x,y,z)
      allocate(green_mesh%fault(green_mesh%msub,3))

      !slipmod = matrix containing slip module for each subfualt
      allocate(green_mesh%slipmod(green_mesh%slipsam,green_mesh%msub))
      !slipr = matrix with (x,y,z) slip components for only 1 subfault
      allocate(green_mesh%slipr(green_mesh%interp_i,green_mesh%ncomp))
      !tractionvec = matrix containing all traction vectors for only 1 subfault
      allocate(green_mesh%tractionvec(&
  &   green_mesh%interp_i*green_mesh%nsta*green_mesh%ncomp,green_mesh%ncomp))

      !Forward variables
      !lensyn = dimension of synthetic vectors
      !lensynf = dimension of synthetics in frequency
      !stcomp = total number of synthetics
      green_mesh%lensyn=green_mesh%interp_i+green_mesh%interp_i-1
      green_mesh%lensynf = (green_mesh%lensyn / 2) + 1
      green_mesh%stcomp=green_mesh%nsta*green_mesh%ncomp

      !tracf = traction matraix containing all traction vectors (frequency)
      allocate(green_mesh%tracf(green_mesh%lensynf,&
  &   green_mesh%stcomp*green_mesh%ncomp*green_mesh%msub))
      !slipf = matrix containing slip vectors (frequency) for only 1 subfault
      allocate(green_mesh%slipf(green_mesh%lensynf,green_mesh%ncomp))
      !syn = matrix to store synthetic seismograms at receivers (time)
!      allocate(green_mesh%syn(green_mesh%lensyn,green_mesh%stcomp))
      allocate(green_mesh%syn(green_mesh%interp_i,green_mesh%stcomp))
      !model = 1D vector containing slip histories, used for OPTIMIZATION
      green_mesh%modelsize=green_mesh%interp_i*green_mesh%ncomp*green_mesh%msub
      green_mesh%modelsize2=green_mesh%interp_i*2*green_mesh%msub
      allocate(green_mesh%model(green_mesh%modelsize))
      allocate(green_mesh%model2(green_mesh%modelsize2))
      allocate(green_mesh%grad2(green_mesh%modelsize2))

      allocate(green_mesh%gradad(green_mesh%modelsize2))
      !allocate(green_mesh%tracad(green_mesh%lensyn, green_mesh%msub*2))

      allocate(green_mesh%slipr2(green_mesh%interp_i,green_mesh%msub*2))

      !syn = matrix to store observed seismograms at receivers (time)
      allocate(green_mesh%obs(green_mesh%interp_i,green_mesh%stcomp))
      allocate(green_mesh%cd(green_mesh%stcomp,green_mesh%stcomp))

      allocate(green_mesh%cm(green_mesh%msub,green_mesh%msub))
      allocate(green_mesh%ce(green_mesh%msub,green_mesh%msub))
      allocate(green_mesh%rtimes(green_mesh%msub),green_mesh%rsamp(green_mesh%msub))


      end subroutine initialize




      subroutine write_debug(green_mesh)

      !COMMON VARIABLES
      implicit none
      ! Define all the GLOBAL variables
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      integer iunit

      IF (green_mesh%debug_i .eq. 1) then
      GO TO 111
      ELSEIF (green_mesh%debug_i .eq. 2) then
      GO TO 222
      ELSEIF (green_mesh%debug_i .eq. 3) then
      GO TO 333
      ENDIF

      iunit = 15

      open(iunit,file='debug_data.ascii',status='unknown')

 111   write(iunit,*) green_mesh%rt, green_mesh%ot
       write(iunit,*) green_mesh%nsta, green_mesh%ncomp, green_mesh%msub
       write(iunit,*) green_mesh%simsam, green_mesh%simt, green_mesh%simdt
       write(iunit,*) green_mesh%interp_i
       write(iunit,*) 'Focal mechanism', green_mesh%stk, green_mesh%dip, green_mesh%rak
       write(iunit,*) 'Fault area', green_mesh%lenstk,'X',green_mesh%lendip,'km2'
       write(iunit,*) 'Subfault area', green_mesh%stk_s,'X',green_mesh%dip_s,'km2'
       write(iunit,*) 'No. of subfault', green_mesh%nsubf
       write(iunit,*) 'Seismic Moment', green_mesh%moment
       write(iunit,*) 'Slip rate samples and slipdt', &
  &                   green_mesh%slipsam, green_mesh%slipdt

      GO TO 999


 222  call write_syn(green_mesh)
      GO TO 999

 333  write(iunit,*) 'Normal vector', green_mesh%vnorm
      write(iunit,*) 'Slip vector', green_mesh%vslip
      write(iunit,*) 'Strike vector', green_mesh%vstk
      GO TO 999

      

 999  return
      end subroutine write_debug


      subroutine destroy_arrays(green_mesh)

      !COMMON VARIABLES
      implicit none
      ! Define all the GLOBAL variables
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      deallocate(green_mesh%rtimes,green_mesh%rsamp)
      deallocate(green_mesh%gradad)
!      deallocate(green_mesh%tracad)
      deallocate(green_mesh%slipr2)
      deallocate(green_mesh%fault)
      deallocate(green_mesh%tracf,green_mesh%slipf)
      deallocate(green_mesh%slipmod,green_mesh%model,green_mesh%model2)!
      deallocate(green_mesh%tractionvec,green_mesh%syn,green_mesh%slipr)
      deallocate(green_mesh%cost)
      deallocate(green_mesh%res,green_mesh%tottrac)
      deallocate(green_mesh%resif)
      deallocate(green_mesh%obs,green_mesh%cd,green_mesh%cm,green_mesh%ce)
      deallocate(green_mesh%grad2)
     
      end subroutine destroy_arrays
