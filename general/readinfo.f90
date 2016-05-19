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

      iunit=20
      ! Read simulation information

      WRITE(6, *) '================================================='
      write(6,*) ' Reading input information '
      WRITE(6, *) '================================================='

      write(6,*) ' Reading Green s functions '
      open(iunit,file=green_mesh%dat//'simul.info',&
  &        status='old',action='read')
       read(iunit,*) green_mesh%rt, green_mesh%ot
       read(iunit,*) green_mesh%nsta, green_mesh%ncomp, green_mesh%msub
       read(iunit,*) green_mesh%simsam, green_mesh%simt, green_mesh%simdt
       read(iunit,*) green_mesh%stress_opt
       read(iunit,*) green_mesh%interp_i, green_mesh%trac_i
       read(iunit,*) green_mesh%prog
      close(iunit)

      iunit=21
      !Read focal mechanism information 
      write(6,*) ' Reading focal mechanism '
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

      iunit = 22
      ! Read slip rate point source information
      write(6,*) ' Reading inital model '
      open(iunit,file=green_mesh%dat//'sliprate.info',status='old',action='read')
       read(iunit,*) green_mesh%slipsam, green_mesh%slipt
       read(iunit,*) green_mesh%moment, green_mesh%mu
      close(22)
      green_mesh%slipdt=green_mesh%slipt/green_mesh%slipsam

      IF (green_mesh%debug .eqv. .true.) THEN
        ! Write simulation information to check
        green_mesh%debug_i = 1
        call write_debug(green_mesh)
      ENDIF

      iunit = 23
      !Read the option selected
      write(6,*) ' Reading optimization options '
      open(iunit,file=green_mesh%dat//'fwioption.info',status='unknown')
      read(iunit,*) fwiopt
      read(iunit,*) niter_max
      read(iunit,*) green_mesh%weig
      close(iunit)

      iunit = 24
      !Write information about synthetics
      write(6,*) ' Reading inforamtion for synthetics '
      OPEN(iunit,file=green_mesh%dat//'syn.info',status='unknown')
      read(iunit,*) green_mesh%for_opt
      !For inversion ==== for_opt = 1
      read(iunit,*) green_mesh%syn_sec, green_mesh%syn_sam
      close(iunit)

      WRITE(6, *) '================================================='
      write(6,*) ' Finish reading input information '
      WRITE(6, *) '================================================='


      end subroutine read_info
