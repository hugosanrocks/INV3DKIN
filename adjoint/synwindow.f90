      subroutine windows(green_mesh)

      !COMMON VARIABLES
      implicit none
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      integer i, iunit, j, k
      real*4  twin(green_mesh%nsta,2), reswin(green_mesh%interp_i,green_mesh%stcomp)

      iunit = 22

      open(iunit,file=green_mesh%dat//'windows.info',status='old',&
  &        action='read')

      do i= 1,green_mesh%nsta
        read(iunit,*) j, twin(i,1:2)
        green_mesh%samwin(i,1:2) = int(twin(i,1:2)/green_mesh%slipdt) + 1
      enddo
      close(iunit)


      endsubroutine windows
