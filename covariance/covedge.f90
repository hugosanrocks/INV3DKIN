         subroutine cov_edge(green_mesh)

         IMPLICIT NONE
         INCLUDE 'green.h'
         TYPE(mesh) :: green_mesh

         integer m, n, i, j, mm, k
         real vec(green_mesh%msub,1), res(green_mesh%msub,1)
         real cres(1,1)
         real al, be, cost, vec2(1,green_mesh%msub)

         al = 1.d0
         be = 0.d0
         mm = green_mesh%msub
         green_mesh%costm = 0.

         !Flush the array
         res(:,:) = 0.d0

         do i=1,2
          m=1+(i-1)* mm
          n=1+(i-1)* mm + mm - 1
          print *, m, n, i
          do j=1,green_mesh%interp_i
           vec(1:green_mesh%msub,1) = green_mesh%slipr2(j,m:n)
           vec2(1,:) = vec(:,1) 
            !Multiplication of matrices 
            call sgemm('N','N',mm,1,mm,al,        &
      &       green_mesh%ce,mm,vec,mm,be,res,mm)
             !Multiplication of matrices 
            call sgemm('N','N',1,1,mm,al,        &
      &       vec2,1,res,mm,be,cres,mm)
            green_mesh%costm = green_mesh%costm + cres(1,1)
           green_mesh%slipr2(j,m:n) = res(1:mm,1)
           !print *, cres, 'cres'
          enddo
         enddo
         !print *, green_mesh%costm, 'costmm'

         !used only to check
         do i=1,green_mesh%msub
         write(33,*) vec(i,1)
         enddo
         !print *, 'model edge cost', green_mesh%costm





         endsubroutine cov_edge
