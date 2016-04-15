         subroutine stress_dwn(proc_mesh)


  USE INIT_BUTTERWORTH_MOD
  USE FILTFILT_BUTTERWORTH_MOD


         IMPLICIT NONE
         INCLUDE 'proc.h'
         TYPE (mesh) :: proc_mesh

         integer*8 i, n, cont, reclen, nfile, reclenx
         integer iunit, iunit2, k, icont, ii, mfault
         real kreal, jump
         real, dimension(:), allocatable :: vecread
         real, dimension(:,:), allocatable :: sti
         !------------------------------------------
         !     variables declaration for butterworth
         !-------------------------------------------
           TYPE(butter) :: butt
           REAL, DIMENSION(:), POINTER :: tseries
           REAL, DIMENSION(:,:), POINTER :: copy_tseries
           REAL, DIMENSION(:), POINTER :: tseries_total
           INTEGER nstep
           REAL dt, tbegin, t
           INTEGER rec_sel, nrec
         !-----------------------------

         proc_mesh%stsi(:,:) = 0.d0   !Flush the stress input array
         n=proc_mesh%simsam           !time samples in Green's functions
         allocate(vecread(n))         !1D array used to read input stress files

         !Jump of samples inside the files
         icont=proc_mesh%mjump/(n)
         mfault=(icont/(proc_mesh%nsta))+1     !Number of subfault used to identify
                                               !respective mu (medium property)

         !Byte length of each stress input element TAU TAU' TAU'' SIGMA..
         INQUIRE(iolength=reclen) vecread(1)

         do nfile=1,6  ! six files to read per source-station
           !Choose the file name SIGMA_**_C*
          if (nfile .eq. 1) then
            proc_mesh%file_s='SIGMA_XX_C'//proc_mesh%comp//''
          elseif(nfile .eq. 2) then
            proc_mesh%file_s='SIGMA_YY_C'//proc_mesh%comp//''
          elseif(nfile .eq. 3) then
            proc_mesh%file_s='SIGMA_ZZ_C'//proc_mesh%comp//''
          elseif(nfile .eq. 4) then
            proc_mesh%file_s='SIGMA_XY_C'//proc_mesh%comp//''
          elseif(nfile .eq. 5) then
            proc_mesh%file_s='SIGMA_XZ_C'//proc_mesh%comp//''
          elseif(nfile .eq. 6) then
            proc_mesh%file_s='SIGMA_YZ_C'//proc_mesh%comp//''
          endif
          !Open the file to be read TAU TAU' TAU'' SIGMA..
          iunit2=15
          OPEN(iunit2,FILE=proc_mesh%dat//proc_mesh%file_s,&
    &        FORM='UNFORMATTED',STATUS='UNKNOWN',&
    &        ACCESS='DIRECT',recl=reclen*n)
          !Read the stress component for each subfault, station, component
          read(iunit2,rec=proc_mesh%sta_i+icont) vecread
          !-------------------------------------------------------------------
          !OPTIONAL FILTER APPLIED TO GREEN'S FUNCTIONS
          !-------------------------------------------------------------------
          !Parameters needed
          butt%order = 2
          butt%fc = 4.
          nstep = proc_mesh%simsam
          nrec  = proc_mesh%stcomp
          dt    = proc_mesh%simdt
          !Asign the time series to be filtered
          proc_mesh%tseries(:) = vecread(:)
          !Initialize the butterworth filter
          CALL INIT_BUTTERWORTH(butt, dt)
          ! Apply the butterworth filter
          CALL FILTFILT_BUTTERWORTH(proc_mesh%tseries, butt, nstep)
!          copy_tseries(:,2) = tseries(:)
          vecread(:) = proc_mesh%tseries(:)
          ! Print the resultsprint input and filtered signals to output file
          !!OPEN(UNIT=1,FILE='test_butterworth.txt',STATUS='UNKNOWN')
          !!t=tbegin
          !!DO ii = 1,nstep
          !! WRITE(1,'(E12.5,x,E12.5,x,E12.5)') t, tseries_total(ii), copy_tseries(ii,2)
          !! t=t+dt
          !!ENDDO
          !!CLOSE(unit=1)
          !!PRINT *,' '
          !!PRINT *,'Results in file test_butterworth.txt :)'
          !-----------------------------------------------------------------
          ! END OF OPTIONAL FILTER APPLIED TO GREEN'S FUNCTIONS
          !-----------------------------------------------------------------
          proc_mesh%stsi(:,nfile) = vecread(:)        
          close(iunit2)
         enddo !END OF CYCLE OVER THE 6 STRESS COMPONENT FILES TO BE OPENED

       !Scale the stress tensor (FOR FINITE FAULT)
       !Divide the stress tensor by the corresponding mu
       proc_mesh%stsi(:,:)=proc_mesh%stsi(:,:)/proc_mesh%mus(mfault)

      !Deallocate memory used
      deallocate(vecread)
      endsubroutine stress_dwn
