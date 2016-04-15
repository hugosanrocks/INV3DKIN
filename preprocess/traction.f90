      subroutine  traction(proc_mesh)

!( m, n, mtx, vec, res )

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!=======================================================================================
         IMPLICIT NONE
         INCLUDE 'proc.h'
         TYPE (mesh) :: proc_mesh

         INTEGER ii, jj, i, cont, cont2, m, n, iunit
         INTEGER  reclen, reclent, iunit2
         INTEGER ijsub, ijcomp, ijump, icont
         INTEGER nn, nnc
         REAL t_slip
         REAL, DIMENSION(:,:), ALLOCATABLE :: tracvec


!       ijsub = (proc_mesh%mjump / (proc_mesh%simsam*proc_mesh%nsta)) * &
!    &          (proc_mesh%ncomp * proc_mesh%interp_i)

     
       !Jump to write next component
!       ijcomp = (proc_mesh%comp_i - 1) * proc_mesh%interp_i

       allocate(tracvec(3,proc_mesh%interp_i))

        !not used anymore
!       icont=proc_mesh%mjump/(proc_mesh%simsam*proc_mesh%nsta)*proc_mesh%ncomp


!      Byte length of stress_input.bin vectors stsi (6 elements)
!      and traction vector (3 elements)
       INQUIRE(iolength=reclent) proc_mesh%traction

!      PSEUDO GREE'S FUNCTIONS
!      Binary file to store Tractions asociated to stationXXX and componentX
!!       iunit=11
!!       OPEN(iunit,FILE=proc_mesh%dat//'TRACT_S'//proc_mesh%sta//'.bin',&
!!    &  status='unknown',FORM='UNFORMATTED',&
!!    &  ACCESS='DIRECT',recl=proc_mesh%interp_i*12)!reclent)

       m=3                     !rows of stress tensor
       n=3                     !columns of stress tensor and elements of v normal

       t_slip=0.d0             !used to check the time of interpolation
       do i=1,proc_mesh%interp_i
         !print *, i + ijcomp + ijsub
         t_slip=t_slip+proc_mesh%slipdt
         ! stress_opt = 1 Data comes from GEODG3D
         if (proc_mesh%stress_opt .eq. 1) then
            !INPUT DATA COMES FROM GEODG3D
            !Froming the stress tensor from TAU TAU' TAU''
            proc_mesh%stso(1,1)=proc_mesh%stinterp(i,1)+proc_mesh%stinterp(i,2)
            proc_mesh%stso(2,2)=proc_mesh%stinterp(i,1)+proc_mesh%stinterp(i,3)
            proc_mesh%stso(3,3)=proc_mesh%stinterp(i,1)-proc_mesh%stinterp(i,2)- &
  &                           proc_mesh%stinterp(i,3)
           !Symmetry of stress tensor
           proc_mesh%stso(1,2)=proc_mesh%stinterp(i,4)
           proc_mesh%stso(1,3)=proc_mesh%stinterp(i,5)
           proc_mesh%stso(2,3)=proc_mesh%stinterp(i,6)
           proc_mesh%stso(2,1)=proc_mesh%stso(1,2)
           proc_mesh%stso(3,1)=proc_mesh%stso(1,3)
           proc_mesh%stso(3,2)=proc_mesh%stso(2,3)
         !stress_opt = 2 Data files come from AXITRA
         elseif (proc_mesh%stress_opt .eq. 2) then
           !INPUT DATA COMES FROM AXITRA
           !Forming the stress tensor from TAU TAU' TAU''
           proc_mesh%stso(1,1)=proc_mesh%stinterp(i,1)
           proc_mesh%stso(2,2)=proc_mesh%stinterp(i,4)
           proc_mesh%stso(3,3)=proc_mesh%stinterp(i,6)
           !Symmetry of stress tensor
           proc_mesh%stso(1,2)=proc_mesh%stinterp(i,2)
           proc_mesh%stso(1,3)=proc_mesh%stinterp(i,3)
           proc_mesh%stso(2,3)=proc_mesh%stinterp(i,5)
           proc_mesh%stso(2,1)=proc_mesh%stso(1,2)
           proc_mesh%stso(3,1)=proc_mesh%stso(1,3)
           proc_mesh%stso(3,2)=proc_mesh%stso(2,3)
         elseif (proc_mesh%stress_opt .eq. 3) then
           !INPUT DATA COMES FROM DWN IBEM
           proc_mesh%stso(1,1)=proc_mesh%stinterp(i,1)
           proc_mesh%stso(2,2)=proc_mesh%stinterp(i,2)
           proc_mesh%stso(3,3)=proc_mesh%stinterp(i,3)
           !Symmetry of stress tensor
           proc_mesh%stso(1,2)=proc_mesh%stinterp(i,4)
           proc_mesh%stso(1,3)=proc_mesh%stinterp(i,5)
           proc_mesh%stso(2,3)=proc_mesh%stinterp(i,6)
           proc_mesh%stso(2,1)=proc_mesh%stso(1,2)
           proc_mesh%stso(3,1)=proc_mesh%stso(1,3)
           proc_mesh%stso(3,2)=proc_mesh%stso(2,3)
         else
           print *, 'Wrong option to read input files'
         endif

         !write(88,*) proc_mesh%stso(1,:), proc_mesh%stso(2,2), proc_mesh%stso(2,3), proc_mesh%stso(3,3)
!        Compute the traction vector sigma*vnorm=stso*vnorm
!        ensuring that is zero before computing the multiplication
         proc_mesh%traction(:)=0.d0
         do cont=1,m
          do cont2=1,n
           proc_mesh%traction(cont)=proc_mesh%traction(cont) +&
  &            proc_mesh%stso(cont,cont2)*proc_mesh%vnorm(cont2)
          enddo
         enddo
         tracvec(:,i)=proc_mesh%traction(:)
       enddo

       !Save traction in frequency domain
       nn=proc_mesh%interp_i + proc_mesh%interp_i -1
       nnc=(nn /2) + 1
       !print *, nn,nnc !CHECK size of arrays
       !do i=1,proc_mesh%interp_i
       ! write(555,*)i, tracvec(:,i)
       !enddo
       !Save traction in the frequency domain
       call tractfft(proc_mesh,tracvec,nn,nnc,proc_mesh%tfft_i)


       deallocate(tracvec)
       end subroutine traction