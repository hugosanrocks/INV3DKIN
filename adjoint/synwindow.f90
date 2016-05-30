      subroutine windows(green_mesh)

      !COMMON VARIABLES
      implicit none
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      integer i, iunit, j, samwin(green_mesh%nsta,2), k
      real*4  twin(green_mesh%nsta,2), reswin(green_mesh%syn_i,green_mesh%stcomp)

      iunit = 22

      open(iunit,file=green_mesh%dat//'windows.info',status='old',&
  &        action='read')

      do i= 1,green_mesh%nsta
        read(iunit,*) j, twin(i,1:2)
        samwin(i,1:2) = int(twin(i,1:2)/green_mesh%slipdt) + 1
      enddo
      close(iunit)


      k = 1
      do i = 1, green_mesh%nsta
        do j = 1, green_mesh%ncomp
           reswin(1:green_mesh%syn_i,k) = green_mesh%res(samwin(i,1):samwin(i,2),k)
           k = k + 1
        enddo
      enddo



      endsubroutine windows
