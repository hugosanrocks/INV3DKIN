      program prog

      implicit none
      integer nx, nz, nxz, ndt, i, it, j, it1
      real dt, dx, dz
      real, dimension(:,:), allocatable :: src
      real, dimension(:), allocatable :: srcvec, vec
      real t

      nx=16
      nz=8
      dx=1.
      dz=1.
      nxz=nx*nz
      dt=25.0/1024.0
      ndt=1024
   
      allocate(src(ndt,nxz),srcvec(nxz),vec(nx))

      open(12,file='source.src',status='unknown')
      do i=1,ndt
       read(12,*) src(i,:)
      enddo
      close(12)
      print *, 'Read finish!'

      it1=1
      t = 0.
      do i=1,45
       it = i + ((i -1)* 10)
       do j=1,nxz
       print *, i, j
       srcvec(j) = src(it,j)
       enddo
       call prep_paraview(nx,nz,dx,dz,nxz,it1,srcvec)
       it1=it1+1
      enddo


      deallocate(src,srcvec)
      end program prog


