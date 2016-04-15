      program prog

      implicit none
      integer nx, nz, nxz, ndt, i, it, j, it1, k, orden(128)
      real dt, dx, dz
      real, dimension(:,:), allocatable :: src
      real, dimension(:), allocatable :: srcvec, vec, srcor
      real t, tfin
      integer jump, m

      nx = 16
      nz = 8
      dx = 1
      dz = 1
      ndt = 1024
      nxz = nx*nz
      tfin = 25.
      dt = tfin / dble(ndt)
   
      allocate(src(ndt*nxz,3),srcvec(nxz),vec(nxz),srcor(nxz))

      k=128
      m=1
      do i=1,8
        do j=1,16
         orden(m)=k-(i)*16+j
         m=m+1
        enddo
      enddo
      print *, orden

      open(12,file='slip_xyz.ascii',status='unknown')
      do i=1,ndt*nxz
       read(12,*) src(i,:)
      enddo
      close(12)
      print *, 'Read is ready'

      it1=1
      t = 0.
      do i= 1,45
       it = i + ((i -1)* 10)
       jump = it
       do j=1,nxz
       jump = jump + ndt
       srcvec(j) = src(jump,2)  !meaningful component
       !print *, jump
       enddo
       do j=1,nxz
       srcor(orden(j)) = srcvec(j)
       enddo
       !print *, it, it1
       call prep_paraview(nx,nz,dx,dz,nxz,it1,srcor)
       it1=it1+1
      enddo


      deallocate(src,srcvec,vec,srcor)
      end program prog


