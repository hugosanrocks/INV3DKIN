      subroutine initwin(green_mesh)

!----------------------Definition of variables--------------------------------------
      !COMMON VARIABLES
      implicit none
      !Define all the variables needed to read stresses
      ! and calculate the tractions associated "Green's
      ! functions", variables in include/green.h
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh


      !1D vectors used for optimization toolbox
      green_mesh%interp_i = green_mesh%interp_i + green_mesh%mext
      print *, green_mesh%interp_i, 'interp_i'
      green_mesh%modelsize = green_mesh%interp_i*green_mesh%ncomp*green_mesh%msub
      green_mesh%modelsize2 = green_mesh%interp_i*2*green_mesh%msub   !in 2D along dip and strike

      green_mesh%lensyn=green_mesh%interp_i+green_mesh%trac_i-1
      green_mesh%stcomp=green_mesh%nsta*green_mesh%ncomp
      !from iniadj
      allocate(green_mesh%tottrac(green_mesh%interp_i,green_mesh%ncomp*green_mesh%msub))
      allocate(green_mesh%syn(green_mesh%lenobs,green_mesh%stcomp))   !interp_i = syn_sam    31 may
      !2 slipmod = matrix containing slip module for each subfualt
      allocate(green_mesh%slipmod(green_mesh%interp_i,green_mesh%msub))
      !3 4 5
      allocate(green_mesh%model(green_mesh%modelsize))
      allocate(green_mesh%model2(green_mesh%modelsize2))
      allocate(green_mesh%grad2(green_mesh%modelsize2))
      !slipr = matrix with (x,y,z) slip components for only 1 subfault
      !6
      allocate(green_mesh%slipr(green_mesh%interp_i,green_mesh%ncomp))
      !7 Matrix with slip-rate time history in 3D for all subfaults
      allocate(green_mesh%slip(green_mesh%interp_i,green_mesh%ncomp*green_mesh%msub))
      !10
      allocate(green_mesh%gradad(green_mesh%modelsize2))
      !11
      allocate(green_mesh%slipr2(green_mesh%interp_i,green_mesh%msub*2))
      allocate(green_mesh%modelp(green_mesh%modelsize))
      !22
      allocate(green_mesh%model2p(green_mesh%modelsize2))
      allocate(green_mesh%diag(green_mesh%msub*green_mesh%interp_i))

      end subroutine initwin




      subroutine destroywin(green_mesh)

!----------------------Definition of variables--------------------------------------
      !COMMON VARIABLES
      implicit none
      !Define all the variables needed to read stresses
      ! and calculate the tractions associated "Green's
      ! functions", variables in include/green.h
      INCLUDE 'green.h'
      TYPE (mesh) :: green_mesh


      deallocate(green_mesh%tottrac)
      deallocate(green_mesh%syn)
      deallocate(green_mesh%slipmod)
      deallocate(green_mesh%model)
      deallocate(green_mesh%model2)
      deallocate(green_mesh%grad2)
      deallocate(green_mesh%slipr)
      deallocate(green_mesh%slip)
      deallocate(green_mesh%gradad)
      deallocate(green_mesh%slipr2)
      deallocate(green_mesh%modelp)
      deallocate(green_mesh%model2p)
      deallocate(green_mesh%diag)


      endsubroutine destroywin
