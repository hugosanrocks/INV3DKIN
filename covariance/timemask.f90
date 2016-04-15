       subroutine time_mask(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh

       real, dimension(:,:), allocatable :: matrix, f
       real, dimension(:), allocatable :: t
       real dt
       integer i, j, iunit, tsam, sub, ind(288), record, reclen


       dt = green_mesh%slipdt
       tsam = green_mesh%slipsam
       sub = green_mesh%msub

       allocate(matrix(sub,sub),t(tsam),f(tsam,sub))

       matrix(:,:) = 0.
       f(:,:) = 1.

       t(1) = 0.
       do i=2,tsam
        t(i) = t(i-1) + dt
       enddo

       ind(:) = green_mesh%rsamp(:)

       do j=1,sub
        do i=ind(j),tsam
         f(i,j) = exp(-1. * (t(i) - t(ind(j))) / 2. )
        enddo
       enddo

       open(iunit,file=green_mesh%dat//'timemask.dat',status='unknown',&
  &         form='unformatted',access='direct',action='write',recl=sub*sub)
       do j=1,tsam
       do i=1,sub
        matrix(i,i) = f(j,i)
       enddo
       write(iunit,rec=j) matrix
       enddo      

       deallocate(matrix,t,f)
       endsubroutine time_mask


