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
       INTEGER i, ii, jj, k, iunit, start, fini, j
      

      !Unit to open files
      iunit=12

      start= 1!green_mesh%delays+1
      fini = green_mesh%syn_i   !interp_i = syn_i    changed    green_mesh%delays+green_mesh%interp_i

      k=1
      do ii=1,green_mesh%nsta
       do jj=1,green_mesh%ncomp
        write(green_mesh%sta,'(I3.3)') ii
        write(green_mesh%comp,'(I1.1)') jj
        !Write synthetic seismogram ASCII file (used to check)
        open(iunit,file=green_mesh%out//'syn_S'//green_mesh%sta//'_C'//green_mesh%comp//'.ascii',&
    &   status='unknown')
        do j=start,fini
         write(iunit,*) green_mesh%syn(j,k)
        enddo
       close(iunit) !Close writing units
       k=k+1
       enddo
      enddo
      


      end subroutine write_syn
