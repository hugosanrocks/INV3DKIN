      subroutine read_infop(proc_mesh)

         IMPLICIT NONE
         INCLUDE 'proc.h'
         TYPE (mesh) :: proc_mesh

         integer iunit
        
         !Directory containing input data files P_C* TUA_C* SIGMA_**_C*
         proc_mesh%dat='dat/'
         !Directory containing output data files
         proc_mesh%out='out/'

         iunit=10

         !Read reciever's geometry
         print *, 'Reading simul.info'
         open(iunit,file=proc_mesh%dat//'simul.info',status='old',action='read')
         read(iunit,*) proc_mesh%rt, proc_mesh%ot
         read(iunit,*) proc_mesh%nsta, proc_mesh%ncomp, proc_mesh%msub
         read(iunit,*) proc_mesh%simsam, proc_mesh%simt, proc_mesh%simdt
         read(iunit,*) proc_mesh%stress_opt
         print *, 'Simulation dt', proc_mesh%simdt
         close(iunit)

         !Read focal mechanism information 
         open(iunit,file=proc_mesh%dat//'focal.info',status='old',action='read')
         read(iunit,*) proc_mesh%stk, proc_mesh%dip, proc_mesh%rak
         close(iunit)
         print *, 'Focal mechanism', proc_mesh%stk, proc_mesh%dip, proc_mesh%rak

         !Read slip rate point source information
         open(iunit,file=proc_mesh%dat//'sliprate.info',status='old',action='read')
         read(iunit,*) proc_mesh%slipsam, proc_mesh%slipt
         read(iunit,*) proc_mesh%moment, proc_mesh%mu
         close(iunit)
         print *, 'Seismic Moment', proc_mesh%moment
         proc_mesh%slipdt=proc_mesh%slipt/proc_mesh%slipsam
         print *, 'Slip rate samples and slipdt', proc_mesh%slipsam, proc_mesh%slipdt


      end subroutine read_infop
