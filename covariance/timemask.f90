       subroutine time_mask(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh

       real, dimension(:,:), allocatable :: matrix, f
       real, dimension(:), allocatable :: t
       real dt, lambda
       integer i, j, iunit, tsam, sub, ind(288), record, reclen, k, nuc(4)


       dt = green_mesh%slipdt
       tsam = green_mesh%slipsam
       sub = green_mesh%msub
       lambda = 0.1
!may 31
       !nuc(1)=210
       !nuc(2)=211
       !nuc(3)=186
       !nuc(4)=187

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
         f(i,j) = exp(-1. * (t(i) - t(ind(j))) / lambda )  !0.1 bueno !4 malo
        enddo
       enddo
       !f(:,210) = 1.
       !f(:,211) = 1.
       !f(:,186) = 1.
       !f(:,187) = 1.
       !ind(210) = 0
       !ind(211) = 0
       !ind(186) = 0
       !ind(187) = 0

!       do j=1,4
!        do i=1,tsam
!         f(i,nuc(j)) = exp(-1. * (t(i) - t(ind(nuc(j)))) / 0.04 )  !0.1 bueno !4 malo
!        enddo
!       enddo
!       do i=1,875
!        write(875,*) f(i,186)
!       enddo

       open(iunit,file=green_mesh%dat//'timemask.dat',status='unknown',&
  &         form='unformatted',access='direct',action='write',recl=sub*sub)
       k=1
       do j=1,tsam
       do i=1,sub
        matrix(i,i) = f(j,i)
        green_mesh%diag(k) = f(j,i)
        write(99,*) green_mesh%diag(k)
        k=k+1
       enddo
       write(iunit,rec=j) matrix
       enddo      

       !do i=1,sub
       ! do k=1,sub
       !  write(666,*) matrix(i,k)
       ! enddo
       !enddo
       
       deallocate(matrix,t,f)
       endsubroutine time_mask


