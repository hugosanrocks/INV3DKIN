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


      !1 Coordinate transformation (strike,dip,rake) to (x,y,z)
      allocate(green_mesh%fault(green_mesh%msub,3))
      !2 slipmod = matrix containing slip module for each subfualt
      allocate(green_mesh%slipmod(green_mesh%interp_i,green_mesh%msub))
      !interp_i = simsam    changed

      !1D vectors used for optimization toolbox
      green_mesh%modelsize=green_mesh%interp_i*green_mesh%ncomp*green_mesh%msub
      green_mesh%modelsize2=green_mesh%interp_i*2*green_mesh%msub   !in 2D along dip and strike

      !3 4 5
      allocate(green_mesh%model(green_mesh%modelsize))
      allocate(green_mesh%model2(green_mesh%modelsize2))
      allocate(green_mesh%grad2(green_mesh%modelsize2))

      !slipr = matrix with (x,y,z) slip components for only 1 subfault
      !6
      allocate(green_mesh%slipr(green_mesh%interp_i,green_mesh%ncomp))
      !7 Matrix with slip-rate time history in 3D for all subfaults
      allocate(green_mesh%slip(green_mesh%interp_i,green_mesh%ncomp*green_mesh%msub))

      !8 tractionvec = matrix containing all traction vectors for only 1 subfault
      allocate(green_mesh%tractionvec(green_mesh%trac_i,&
  &            green_mesh%nsta*green_mesh%ncomp*green_mesh%ncomp*green_mesh%msub))

      !Forward variables
      !lensyn = dimension of synthetic vectors
      !lensynf = dimension of synthetics in frequency
      !stcomp = total number of synthetics
      green_mesh%lensyn=green_mesh%interp_i+green_mesh%trac_i-1
      green_mesh%stcomp=green_mesh%nsta*green_mesh%ncomp

!=====TERMS USED IF CONVOLUTION PERFORMED IN THE FREQUENCY DOMAIN============!
!      green_mesh%lensynf = (green_mesh%lensyn / 2) + 1
      !tracf = traction matraix containing all traction vectors (frequency)
!      allocate(green_mesh%tracf(green_mesh%lensynf,&
!  &   green_mesh%stcomp*green_mesh%ncomp*green_mesh%msub))
!     !slipf = matrix containing slip vectors (frequency) for only 1 subfault
!      allocate(green_mesh%slipf(green_mesh%lensynf,green_mesh%ncomp))
      !syn = matrix to store synthetic seismograms at receivers (time)
!============================================================================!

      !9
      allocate(green_mesh%syn(green_mesh%syn_i,green_mesh%stcomp))   !interp_i = syn_sam
      !interp_i = syn_i          changed

      !10
      allocate(green_mesh%gradad(green_mesh%modelsize2))
      !allocate(green_mesh%tracad(green_mesh%lensyn, green_mesh%msub*2))
      !11
      allocate(green_mesh%slipr2(green_mesh%interp_i,green_mesh%msub*2))

      !syn = matrix to store observed seismograms at receivers (time)
      !12
      allocate(green_mesh%obs(green_mesh%syn_i,green_mesh%stcomp))
      !interp_i = syn_i          changed

      !13
      allocate(green_mesh%cd(green_mesh%stcomp,green_mesh%stcomp))
      !14
      allocate(green_mesh%ce(green_mesh%msub,green_mesh%msub))
      !15
      allocate(green_mesh%cm(green_mesh%msub,green_mesh%msub))
      !16
      allocate(green_mesh%ct(green_mesh%slipsam,green_mesh%slipsam))
      !17
      allocate(green_mesh%rtimes(green_mesh%msub))
      !18
      allocate(green_mesh%rsamp(green_mesh%msub))
      !19
      allocate(green_mesh%diag(green_mesh%msub*green_mesh%interp_i))
      !20
      allocate(green_mesh%la(green_mesh%msub,green_mesh%msub))
      !21
      allocate(green_mesh%modelp(green_mesh%modelsize))
      !22
      allocate(green_mesh%model2p(green_mesh%modelsize2))
      !23
      allocate(green_mesh%tseries(green_mesh%syn_i))
      !interp_i = syn_i           filter the observations

      !24
      allocate(green_mesh%samwin(green_mesh%stcomp,2))

      end subroutine initialize




      subroutine write_debug(green_mesh)

      !COMMON VARIABLES
      implicit none
      ! Define all the GLOBAL variables
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      integer iunit

      iunit = 25
      open(iunit,file=green_mesh%out//'debug_data.ascii',status='unknown')

      IF (green_mesh%debug_i .eq. 1) then
      GO TO 111
      ELSEIF (green_mesh%debug_i .eq. 3) then
      GO TO 333
      ENDIF

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
       close(iunit)
      GO TO 999

 333  write(iunit,*) 'Normal vector', green_mesh%vnorm
      write(iunit,*) 'Slip vector', green_mesh%vslip
      write(iunit,*) 'Strike vector', green_mesh%vstk
      close(iunit)
      GO TO 999

 999  return
      end subroutine write_debug


      subroutine destroy_arrays(green_mesh)

      !COMMON VARIABLES
      implicit none
      ! Define all the GLOBAL variables
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      deallocate(green_mesh%fault)       !1
      deallocate(green_mesh%slipmod)     !2
      deallocate(green_mesh%model)       !3
      deallocate(green_mesh%model2)      !4
      deallocate(green_mesh%grad2)       !5
      deallocate(green_mesh%slipr)       !6
      deallocate(green_mesh%slip)        !7
      deallocate(green_mesh%tractionvec) !8
      deallocate(green_mesh%syn)         !9
      deallocate(green_mesh%gradad)      !10
      deallocate(green_mesh%slipr2)      !11
      deallocate(green_mesh%obs)         !12
      deallocate(green_mesh%cd,green_mesh%ce,green_mesh%cm)
      deallocate(green_mesh%ct)          !13,14,15,16
      deallocate(green_mesh%rtimes)      !17
      deallocate(green_mesh%rsamp)       !18
      deallocate(green_mesh%diag)        !19
      deallocate(green_mesh%la)
      deallocate(green_mesh%modelp)
      deallocate(green_mesh%model2p)
      deallocate(green_mesh%tseries)
      deallocate(green_mesh%samwin)

      !From initialadj
      deallocate(green_mesh%cost)
      deallocate(green_mesh%res)
      deallocate(green_mesh%tottrac)
      
      !Frequency domain arrays
!      deallocate(green_mesh%resif)
!      deallocate(green_mesh%tracad)
!      deallocate(green_mesh%tracf,green_mesh%slipf)


      end subroutine destroy_arrays
