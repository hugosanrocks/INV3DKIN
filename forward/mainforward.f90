!--------------------------------------------------------
!     3D Kinematic Seismic Source Inversion
!     by an adjoint-state method
!--------------------------------------------------------
!------------- MAIN FORWARD MODELING PROGRAM ------------
!
!     This program:
!
!     2) Forward:
!
!      2.1) Using the sliprate module for each subfault -> green_mesh%slipr
!           and the pseudo-Green's functions            -> green_mesh%traction
!           computes the synthetic seismograms.
!
!----------------------------------------------------------------------
!    Author:
!    Hugo S. Sanchez Reyes 29/06/15
!    Universite de Grenoble 1 "Joseph Fourier"
!    Supervisors: J. Tago, V.M. Cruz-Atienza, V. Hjorleifsdottir, 
!                 L. Metivier, J. Virieux
!---------------------------------------------------------------------

      program main_forward

!----------------------Definition of variables--------------------------------------
      !COMMON VARIABLES
      implicit none
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

!     Variables needed only by this program
      integer iunit
!------------------------------------------------------

!     Set working directories
!     Directory containing input data files P_C* TUA_C* SIGMA_**_C*
      green_mesh%dat='dat/'
!     Directory containing output data files
      green_mesh%out='out/'

      !Read general information and prepare stress tensor files
      call read_info(green_mesh)

      ! Option of forward modeling, from modulus (1) or 3D vector (2)
      iunit=15
      !Write information about synthetics
      OPEN(iunit,file=green_mesh%dat//'syn.info',status='unknown')
      read(iunit,*) green_mesh%for_opt
      close(iunit)


      call initialize(green_mesh)

      if (green_mesh%for_opt .eq. 1) then
      call coor_trans(green_mesh)
      elseif (green_mesh%for_opt .eq. 2) then
      call read_model(green_mesh)
      print *, 'read ready'
      else
      write(*,*) 'Wrong forward option, check dat/syn.info'
      endif

      !Compute the forward problem and compute first synthetics
      !asociated with the first model (0 ZEROS)
      !lensyn = dimension of synthetic vectors
      call forward(green_mesh)
      
      call write_syn(green_mesh)

      call write_model(green_mesh,green_mesh%model2,green_mesh%modelsize2)




      deallocate(green_mesh%fault)
      deallocate(green_mesh%tracf,green_mesh%slipf)
      deallocate(green_mesh%slipmod,green_mesh%model)!
      deallocate(green_mesh%tractionvec,green_mesh%syn,green_mesh%slipr)

      end program main_forward
