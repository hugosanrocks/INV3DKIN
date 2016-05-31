        subroutine coor_trans(green_mesh)

        !COMMON VARIABLES
         IMPLICIT NONE
         INCLUDE 'green.h'
         TYPE (mesh) :: green_mesh

         !Variables needed by this subroutine
         INTEGER i, j, k, l, iunit, mem(1,2), n1, n2
         REAL pi/3.1415926535897932/
         REAL v3(2), vec3(1,3), vec2(1,2), matt(2,3), matt2(3,2), vect(1,3), val(1,1)
         REAL res(1,2), vs3(1,3), vm(3,2), al, be

         !SAME AS AXITRA
         !****************************************************************!
         ! - STRIKE : strike of the fault measured clockwise from North. 
         !   DIP : dip of the fault. 
         !   RAKE : direction of slip of the hanging wall relatively to 
         !   the foot wall. It is measured counterclockwise from the 
         !   strike direction. If faulting is right-lateral (like the San 
         !   Andreas or the North Anatolian), rake=0.
         !****************************************************************!

         !Add 90 degrees to rake to be consistent with AXITRA
         green_mesh%rak=green_mesh%rak+pi

         !Unitary slip vector
         !Same as AXITRA coordinate system
         green_mesh%vslip(1) = (cos(green_mesh%rak)*cos(green_mesh%stk)) + &   !-1.0
  &      sin(green_mesh%rak)*cos(green_mesh%dip)*sin(green_mesh%stk)
         green_mesh%vslip(2) =( cos(green_mesh%rak)*sin(green_mesh%stk)) - & 
  &      (sin(green_mesh%rak)*cos(green_mesh%dip)*cos(green_mesh%stk))
         green_mesh%vslip(3) = -1.0*sin(green_mesh%rak)*sin(green_mesh%dip)  !-1 z para arriba

         !Fault unitary normal vector used for transformation (Stein p.218 eq.2)
         !Same as AXITRA coordinate system
         green_mesh%vnorm(1)=-1.0*sin(green_mesh%dip)*sin(green_mesh%stk) ! 1 x para el este
         green_mesh%vnorm(2)=     sin(green_mesh%dip)*cos(green_mesh%stk)
         green_mesh%vnorm(3)=-1.0*cos(green_mesh%dip)

         !Unitary strike vector
         green_mesh%vstk(1)=cos(green_mesh%stk)
         green_mesh%vstk(2)=sin(green_mesh%stk)
         green_mesh%vstk(3)=0.

         !Rotate vectors to be consistent with mesh GEODG3D
         call rotate_z(green_mesh%vnorm)
         call rotate_z(green_mesh%vslip)
         call rotate_z(green_mesh%vstk)

         !Unitary dip vector
         green_mesh%vdip(1)= green_mesh%vstk(2)*green_mesh%vnorm(3)-green_mesh%vnorm(2)*green_mesh%vstk(3)
         green_mesh%vdip(2)= -1.*(green_mesh%vstk(1)*green_mesh%vnorm(3)-green_mesh%vnorm(1)*green_mesh%vstk(3))
         green_mesh%vdip(3)= green_mesh%vstk(1)*green_mesh%vnorm(2)-green_mesh%vnorm(1)*green_mesh%vstk(2)

         green_mesh%slipm(1,:) = green_mesh%vstk(:)
         green_mesh%slipm(2,:) = green_mesh%vdip(:)

         !Decompose slip vector along strike and dip
         green_mesh%vslip2(:)=0.
         do i=1,2
          do j=1,3
           green_mesh%vslip2(i)=green_mesh%vslip2(i) +green_mesh%slipm(i,j)*green_mesh%vslip(j)
          enddo
         enddo

         !Print only to check decompositon along strike and dip
         write(6,*) ' Slip vector    :', green_mesh%vslip
         write(6,*) ' Strike vector  :', green_mesh%vstk
         write(6,*) ' Dip vector     :', green_mesh%vdip
         write(6,*) ' 2D Slip vector :', green_mesh%vslip2


         !Read the slip-rate initial model (modulus of vector)
         iunit=30
         open(iunit,file=green_mesh%dat//'vitesse.out',status='unknown',action='read')
         do i=1,green_mesh%interp_i      !slipsam = interp_i
          read(iunit,*) green_mesh%slipmod(i,:)
         enddo
         close(iunit)

         !Arrange slip rate model in 1D vector
         l = 1
         do i=1,green_mesh%msub
         !Decompose slip vector along stk and along dip = 2 directions
          do j=1,2
           do k=1,green_mesh%interp_i
            green_mesh%model2(l) = green_mesh%slipmod(k,i)*green_mesh%vslip2(j)
            l = l + 1
           enddo
          enddo
         enddo


        !Change slip vector model from along stk and dip to (x,y,z)
        call model_c(green_mesh%model2,green_mesh%model,green_mesh%interp_i,green_mesh%msub,green_mesh%slipm)
        

         endsubroutine coor_trans

