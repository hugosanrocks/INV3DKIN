      subroutine preprocess(proc_mesh)

      implicit none

      !Define all the variables needed to read stresses and calculate the tractions
      !associated "Green's functions", variables in include/green.h
      INCLUDE 'proc.h'
      TYPE (mesh) :: proc_mesh

      !Variables needed only by this program
      integer i, j, k, m, mjump, iunit
      real :: start, stop


      WRITE(6 ,*) ''
      WRITE(6, *) '================================================='
      WRITE(6, *) ' PREPROCESS, PREPARING PSEUDO-GREEN S FUNCTIONS  '
      WRITE(6, *) '================================================='

      !Read reciever's geometry
      call read_infop(proc_mesh)

      !Coordinate transformation (strike,dip,rake) to (x,y,z)
      call norm_vec(proc_mesh)

!------------------FOR EACH STATION AND EACH COMPONENT----------------
      !The files P TAU_P TAU_PP SIGMA_.. are rearranged into only one
      !binary file stress_input.bin
      WRITE(*,*) 'Reading stress input files '

      !Number of samples in each file to be read (simulation time samples)

      !6 files with simsam = samples in simulation
      allocate(proc_mesh%stsi(proc_mesh%simsam,6))
      allocate(proc_mesh%stinterp(proc_mesh%simsam,6))
      allocate(proc_mesh%tractionv(proc_mesh%simsam,3))

      !Subfaults's positions array
      allocate(proc_mesh%fault(proc_mesh%msub,3))
      allocate(proc_mesh%mus(proc_mesh%msub))

      !Time series array used to filtered Green's functions
      allocate(proc_mesh%tseries(proc_mesh%simsam))

      !Assign medium properties to each subfault (mu, lambda)
      call id_mu(proc_mesh)

      proc_mesh%tfft_i = 1
      call cpu_time(start)

      !Read stress data form GEODG3D (stress_opt .eq. 1)
      if (proc_mesh%stress_opt .eq. 1) then
      write(6,*) 'Input stress data comes from GEODG3D'
      elseif (proc_mesh%stress_opt .eq. 2) then
      write(6,*) 'Input stress data comes from AXITRA'
      elseif (proc_mesh%stress_opt .eq. 3) then
      write(6,*) 'Input stress data comes from DWN-IBEM'
      else
      write(6,*) 'Wrong option for input1=GEODG3D, 2=AXITRA, 3=DWN-IBEM'
      endif

      do m=1,proc_mesh%msub          !number of subfaults
       print *, 'Subfault no: ', m 
       proc_mesh%mjump=(m-1)*proc_mesh%simsam*proc_mesh%nsta   
       !jump inside DG input files P TAU_* SIGMA_*
         proc_mesh%sta_i=1
         do i=1,proc_mesh%nsta
          proc_mesh%comp_i=1
          do k=1,proc_mesh%ncomp
           write(proc_mesh%sta,'(I3.3)') i
           write(proc_mesh%comp,'(I1.1)') k
             if (proc_mesh%stress_opt .eq. 1) then
              call mul_src(proc_mesh)
              !Interpolate solution form GEODG3D every slip dt
              call interpolation(proc_mesh)     
             elseif (proc_mesh%stress_opt .eq. 2) then !From AXITRA
              call stress_axi(proc_mesh)
              call interp_axi(proc_mesh)
             elseif (proc_mesh%stress_opt .eq. 3) then !From DWN-IBEM
              call stress_dwn(proc_mesh)
              call interp_dwn(proc_mesh)
             else
              print *, 'Wrong option to read stress input files'
             endif
           !4) Construction of stress tensor from stress_input.bin and
           !estimation of tractions, they are save into TRACT_STXXXCX
           call traction(proc_mesh)
           proc_mesh%tfft_i = proc_mesh%tfft_i + 1
           proc_mesh%comp_i=proc_mesh%comp_i+1
          enddo
          proc_mesh%sta_i=proc_mesh%sta_i+1
         enddo
      enddo
      call cpu_time(stop)
      write(6,*) '*****************************************'
      write(6,*) 'Time used for preprocess: ', stop-start, 'seconds'
      write(6,*) '*****************************************'
      WRITE(6, *) '================================================='
      WRITE(6, *) ' PREPROCESS FINISHED, FILES  "dat/TRACT_SXXX_CX" '
      WRITE(6, *) '================================================='

     deallocate(proc_mesh%fault,proc_mesh%mus)
     deallocate(proc_mesh%tractionv)
     deallocate(proc_mesh%stsi,proc_mesh%stinterp)
     deallocate(proc_mesh%tseries)
     end subroutine preprocess
