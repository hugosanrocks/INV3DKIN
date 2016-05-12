         subroutine cov_time(green_mesh)

         IMPLICIT NONE
         INCLUDE 'green.h'
         TYPE(mesh) :: green_mesh

         integer m, n, i, j, mm
         real vec(green_mesh%interp_i,1), res(green_mesh%interp_i,1)
         real cres(1,1)
         real al, be, cost, vec2(1,green_mesh%interp_i)

         al = 1.d0
         be = 0.d0
         mm = green_mesh%interp_i
         green_mesh%costm = 0.

         !Flush the array
         res(:,:) = 0.d0

          do j=1,green_mesh%msub*2
           vec(1:green_mesh%interp_i,1) = green_mesh%slipr2(:,j)
           vec2(1,:) = vec(:,1) 
            !Multiplication of matrices 
            call sgemm('N','N',mm,1,mm,al,        &
      &       green_mesh%ct,mm,vec,mm,be,res,mm)
             !Multiplication of matrices 
            call sgemm('N','N',1,1,mm,al,        &
      &       vec2,1,res,mm,be,cres,mm)
           ! print *, 'cres', cres
           green_mesh%costm = green_mesh%costm + cres(1,1)
           green_mesh%slipr2(:,j) = res(:,1)
          enddo


         !used only to check
         !do i=1,green_mesh%msub
         !write(33,*) vec(i,1)
         !enddo





         endsubroutine cov_time
