      subroutine  read_traction(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh

       !Variables needed only by this subroutine
       INTEGER i, k, iunit
       INTEGER reclent, ii, jj
       INTEGER ijcomp, ijsub

       !Units to be used to read and write
       iunit=15

       !'lent' length of traction vector
       INQUIRE(iolength=reclent) green_mesh%traction

       !Jump for each subfault
       ijsub=(green_mesh%mjump-1)*green_mesh%interp_i*green_mesh%ncomp

       !Loop over number of stations and components 
       k=1                         !synthetic number!
       do ii=1,green_mesh%nsta
        do jj=1,green_mesh%ncomp
         write(green_mesh%sta,'(I3.3)') ii
         write(green_mesh%comp,'(I1.1)') jj
         !Jump to write next component
         ijcomp = (jj - 1) * green_mesh%interp_i
         !Unit containing unitary traction vector
         !at sation SXXX and component CX
         OPEN(iunit,&
  &      FILE=green_mesh%dat//'TRACT_S'//green_mesh%sta//'.bin',&
  &      status='unknown',FORM='UNFORMATTED',&
  &      ACCESS='DIRECT',recl=reclent)
          !store traction vector at variable 'y'
          do i=1,green_mesh%interp_i
           read(iunit,rec=i+ijcomp+ijsub) green_mesh%tractionvec(k,:)
           write(22,*) green_mesh%tractionvec(k,:)
           k = k + 1
          enddo
        enddo
       enddo
       


      end subroutine read_traction
