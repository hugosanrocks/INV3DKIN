!--------------------------------------------------------
!     3D Kinematic Seismic Source Inversion
!     by an adjoint-state method
!--------------------------------------------------------
!------------- WRITE SYNTHETIC SEISMOGRAMS SUBROUTINE ---
!
!     This program:
!
!     Writes into ASCII files the corresponding synthetic
!     seismograms to each station and every component
!
!     THIS SUBROUTINE IS INDEPENDENT OF THE INVERSION
!----------------------------------------------------------------------
!    Author:
!    Hugo S. Sanchez Reyes 29/06/15
!    Universite de Grenoble 1 "Joseph Fourier"
!    Supervisors: J. Tago, V.M. Cruz-Atienza, V. Hjorleifsdottir, 
!                 L. Metivier, J. Virieux
!---------------------------------------------------------------------

       subroutine write_syn(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh

       !local variables
       INTEGER i, ii, jj, k, iunit, iunit2, iunit3
       INTEGER start, fini, j
       REAL    t, dt     
       REAL    tini, tfin, maxamp, maxamp2

      call read_obs(green_mesh)

      !Unit to open files
      iunit=22
      iunit2=23
      iunit3=24

      dt = green_mesh%slipdt
      start= 1!green_mesh%delays+1
      fini = green_mesh%syn_i   !interp_i = syn_i    changed    green_mesh%delays+green_mesh%interp_i

      k=1
      do ii=1,green_mesh%nsta
       write(green_mesh%sta,'(I3.3)') ii
       open(iunit3,file=green_mesh%out//'syn_S'//green_mesh%sta//'.win',&
    &  status='unknown')
       write(iunit3,*) '# NUM_WIN = ', 2
       write(iunit3,*) '1',real(green_mesh%samwin(ii,1:2))*dt,'0'
       write(iunit3,*) '2',real(green_mesh%samwin(ii,2:3))*dt,'0'
       close(iunit3)
       open(iunit3,file=green_mesh%out//'syn_S'//green_mesh%sta//'.win.qual',&
    &  status='unknown')
       write(iunit3,*) '# NUM_WIN = ', 2
       write(iunit3,*) '# i win_start win_end Tshift CC dlnA'
       write(iunit3,*) '1',real(green_mesh%samwin(ii,1:2))*dt,'0.00000    1.00000    0.00000'
       write(iunit3,*) '2',real(green_mesh%samwin(ii,2:3))*dt,'0.00000    1.00000    0.00000'
       close(iunit3)
       do jj=1,green_mesh%ncomp
         t = 0.
!        write(green_mesh%sta,'(I3.3)') ii
        write(green_mesh%comp,'(I1.1)') jj
        open(iunit2,file=green_mesh%out//'syn_S'//green_mesh%sta//'_C'//green_mesh%comp//'.head',&
    &   status='unknown')
        write(iunit2,*) '# NPTS = ', green_mesh%samwin(ii,2) 
        open(iunit3,file=green_mesh%out//'obs_S'//green_mesh%sta//'_C'//green_mesh%comp//'.head',&
    &   status='unknown')
        write(iunit3,*) '# NPTS = ', 875  !changed 
       !Write synthetic seismogram ASCII file (used to check)
        open(iunit,file=green_mesh%out//'syn_S'//green_mesh%sta//'_C'//green_mesh%comp//'.ascii',&
    &   status='unknown')
        do j=start,green_mesh%samwin(ii,2)
         write(iunit,*) t, green_mesh%syn(j,k)
         t = t + dt
        enddo
       close(iunit) !Close writing units
       !Maximum values in the plot
       maxamp  = maxval(abs(green_mesh%syn(1:green_mesh%samwin(ii,2),k)))
       maxamp2 = maxval(abs(green_mesh%obs(:,k)))
       write(iunit3,*) '# PLOT_MAX = ', 1.3*maxamp2
       write(iunit3,*) '# T_START = ',  0.d0
       write(iunit3,*) '# T_END = ',    15.0
       close(iunit3)
       write(iunit2,*) '# PLOT_MAX = ', 1.3*maxamp
       write(iunit2,*) '# T_START = ',  0.d0
       write(iunit2,*) '# T_END = ',    t-dt
       close(iunit2)

       k=k+1
       enddo
      enddo
      


      end subroutine write_syn
