         subroutine model1d(green_mesh)

         IMPLICIT NONE
         INCLUDE 'green.h'
         TYPE(mesh) :: green_mesh

         integer mem(1,2), i, j, k, l, n1, n2, p
         real val(1,1), vec2(1,2), vec3(1,3), vect(1,3), matt(2,3)
         real matt2(3,2)

         !Flush the array
         green_mesh%slipr2(:,:) = 0.d0


         mem(1,1) = 1
         mem(1,2) = 1+green_mesh%interp_i
 
         do k=1,green_mesh%interp_i
          do i=1,green_mesh%msub
            n1 = mem(1,1) + green_mesh%Interp_i*2*(i-1)+(k-1)
            n2 = mem(1,2) + green_mesh%interp_i*2*(i-1)+(k-1)
            vec2(1,1) = green_mesh%model2(n1)
            vec2(1,2) = green_mesh%model2(n2)
            green_mesh%slipr2(k,i) = vec2(1,1)
            green_mesh%slipr2(k,i+green_mesh%msub) = vec2(1,2)
          enddo
         enddo

         call cov_mod(green_mesh)

         !Return additional gradient to 1D array (interp_i*msub*2)
         !=======================================!
         green_mesh%gradad(:) = 0.d0
         do k=1,green_mesh%interp_i
          do i=1,green_mesh%msub
           n1 = mem(1,1) + green_mesh%Interp_i*2*(i-1)+(k-1)
            n2 = mem(1,2) + green_mesh%interp_i*2*(i-1)+(k-1)
            vec2(1,1) = green_mesh%slipr2(k,i)
            vec2(1,2) = green_mesh%slipr2(k,i+green_mesh%msub)
            green_mesh%gradad(n1) = vec2(1,1)
            green_mesh%gradad(n2) = vec2(1,2)
          enddo
         enddo


         endsubroutine model1d
