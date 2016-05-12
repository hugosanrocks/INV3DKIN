      subroutine read_info(green_mesh)

      IMPLICIT NONE
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      integer iunit, fwiopt, niter_max
      REAL pi/3.1415926535897932/

      ! Directory containing input data files P_C* TUA_C* SIGMA_**_C*
      green_mesh%dat='dat/'
      ! Directory containing output data files
      green_mesh%out='out/'

      iunit=10
      ! Read simulation information

      print *, 'Reading simul.info'
      open(iunit,file=green_mesh%dat//'simul.info',&
  &        status='old',action='read')
       read(iunit,*) green_mesh%rt, green_mesh%ot
       read(iunit,*) green_mesh%nsta, green_mesh%ncomp, green_mesh%msub
       read(iunit,*) green_mesh%simsam, green_mesh%simt, green_mesh%simdt
       read(iunit,*) green_mesh%stress_opt
       read(iunit,*) green_mesh%interp_i
       read(iunit,*) green_mesh%prog
      close(iunit)

      !Read focal mechanism information 
      open(iunit,file=green_mesh%dat//'focal.info',&
  &        status='old',action='read')
       read(iunit,*) green_mesh%stk, green_mesh%dip, green_mesh%rak
       read(iunit,*) green_mesh%lenstk, green_mesh%lendip
       read(iunit,*) green_mesh%stk_s, green_mesh%dip_s
      close(iunit)
      green_mesh%nsubf=green_mesh%lenstk*green_mesh%lendip/(green_mesh%stk_s*green_mesh%dip_s)

      ! Convert angles to radiants
      green_mesh%stk=green_mesh%stk*(2.0*pi/360.0)
      green_mesh%dip=green_mesh%dip*(2.0*pi/360.0)
      green_mesh%rak=(green_mesh%rak*(2.0*pi/360.0))

      ! Read slip rate point source information
      open(iunit,file=green_mesh%dat//'sliprate.info',status='old',action='read')
       read(iunit,*) green_mesh%slipsam, green_mesh%slipt
       read(iunit,*) green_mesh%moment, green_mesh%mu
      close(iunit)
      green_mesh%slipdt=green_mesh%slipt/green_mesh%slipsam

      IF (green_mesh%debug .eqv. .true.) THEN
        ! Write simulation information to check
        green_mesh%debug_i = 1
        call write_debug(green_mesh)
      ENDIF

      !Read the option selected
      iunit=12
      open(iunit,file=green_mesh%dat//'fwioption.info',status='unknown')
      read(iunit,*) fwiopt
      read(iunit,*) niter_max
      read(iunit,*) green_mesh%weig
      close(iunit)


      end subroutine read_info
