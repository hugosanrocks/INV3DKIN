     subroutine prep_paraview(nx,nz,dx,dz,nxz,it,yoffe)

     integer, intent(inout) :: nx, nz, nxz, it
     integer i, j, k
     real, intent(inout) :: dx, dz, yoffe(nxz)
     real vec(nz,nx)
     character*4 buffer

      write(buffer,'(I4.4)') it
      open(141,file='file'//buffer//'.vtk',status='unknown')

    k=1
    do i=1,nz
    do j=1,nx
    vec(i,j) = yoffe(k) ! dump to vtk file
    k=k+1
    enddo
    enddo

      write(141,*) yoffe

    close(141)

    end subroutine prep_paraview
