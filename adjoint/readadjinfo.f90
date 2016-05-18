      subroutine read_adjinfo(green_mesh)

      IMPLICIT NONE
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh

      integer*8 i, n, cont, reclen, nfile, reclenx
      integer iunit
        
      iunit=15

      ! Read adjoint problem information needed
      write(6,*) ' Reading adjoint information '
      !open(iunit,file=green_mesh%dat//'syn.info',status='unknown')
        !read(iunit,*) green_mesh%interpadj_i
        !read(iunit,*) green_mesh%nsta, green_mesh%ncomp
      !close(iunit)
       green_mesh%interpadj_i = green_mesh%interp_i

      end subroutine read_adjinfo
