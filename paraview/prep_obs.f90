     subroutine prep_paraview(nx,nz,dx,dz,nxz,it,yoffe)

     integer, intent(inout) :: nx, nz, nxz, it
     integer i, j, k
     real, intent(inout) :: dx, dz, yoffe(nxz)
     real vec(nz,nx)
     character*4 buffer

      write(buffer,'(I4.4)') it
      open(141,file='fileobs'//buffer//'.vtk',status='unknown')


!      write(141,*) "# vtk DataFile Version 2.0"   ! standard header
!      write(141,*) "Volume example" ! description
!      write(141,*) "DATASET STRUCTURED_POINTS"     ! it's a binary file
 
!      write(141,*) "DIMENSIONS", nx, nz, 1           ! this is the grid dimensions
!      write(141,*) "ASPECT_RATIO 1 1 1"
!      write(141,*) "ORIGIN 0 0 0"  ! our origin is 0,0,0
!      write(141,*) "POINT_DATA 1000"
!      write(141,*) "SCALARS VX1" ! save ONE scalar field, named as the variable name (the last "1" is the dim of scalar field)
!      write(141,*) "LOOKUP_TABLE default" ! standard lookup table (don't touch)

    k=1
    do i=1,8
    do j=1,16
    vec(i,j) = yoffe(k) ! dump to vtk file
    k=k+1
    enddo
    enddo

!    do i=1,20
      write(141,*) yoffe
!    enddo

    close(141)

    end subroutine prep_paraview
