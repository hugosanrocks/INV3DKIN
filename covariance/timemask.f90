       subroutine time_mask(green_mesh)

       !COMMON VARIABLES
       IMPLICIT NONE
       INCLUDE 'green.h'
       TYPE (mesh) :: green_mesh

       real, dimension(:,:), allocatable :: matrix, f
       real, dimension(:), allocatable :: t
       real dt, lambda
       integer i, j, iunit, tsam, sub, ind(green_mesh%msub), record, reclen, k, nuc(9)

       iunit = 33
       dt = green_mesh%slipdt
       tsam = green_mesh%interp_i
       sub = green_mesh%msub
       lambda = 0.1

!may 31
       nuc(1)=210
       nuc(2)=211
       nuc(3)=212
       nuc(4)=185
       nuc(5)=186
       nuc(6)=187
       nuc(7)=234
       nuc(8)=235
       nuc(9)=236
       do i=1,green_mesh%msub
         if (green_mesh%rsamp(i) .le. 12) then
            green_mesh%rsamp(i) = 1
         endif
       enddo


       allocate(matrix(sub,sub),t(tsam),f(tsam,sub))

       matrix(:,:) = 0.
       f(:,:) = 1.

       t(1) = 0.
       do i=2,tsam
        t(i) = t(i-1) + dt
       enddo

       !Constant travel time   VS_MAX
       ind(:) = green_mesh%rsamp(:)
       !Estimated eikonal
       open(iunit,file=green_mesh%dat//'ttimes.info',status='old',&
  &         action='read')
       !read(iunit,*) ind(:)
       !print *, ind
       close(iunit)

       do j=1,sub
        do i=ind(j),tsam
         f(i,j) = exp(-1. * (t(i) - t(ind(j))) / lambda )  !0.1 bueno !4 malo
        enddo
       enddo

       !Nucleation is not penalize at first approach
!       do i=1,9
!       f(:,nuc(i)) = 1.
!       ind(nuc(i)) = 1
!       enddo
       !ind(185) = 12
       !ind(186) = 6

 !      do j=1,9
 !       do i=1,tsam
 !        f(i,nuc(j)) = exp(-1. * (t(i) - t(ind(nuc(j)))) / 0.04 )  !0.1 bueno !4 malo
 !       enddo
 !      enddo
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
       close(iunit)

       !do i=1,sub
       ! do k=1,sub
       !  write(666,*) matrix(i,k)
       ! enddo
       !enddo
       
       deallocate(matrix,t,f)
       endsubroutine time_mask


