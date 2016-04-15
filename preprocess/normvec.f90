        subroutine norm_vec(proc_mesh)

        !COMMON VARIABLES
         IMPLICIT NONE
         INCLUDE 'proc.h'
         TYPE (mesh) :: proc_mesh

         !Variables needed by this subroutine
         INTEGER i, j, iunit, iunit2, reclen
         REAL pi/3.1415926535897932/
         integer cont, cont2
         real nor(3)
         real vec(3), vec2(3), vstk(3), vdip(3), vslip(3)
         real conma(3,3)

         !Rotation matrix around Z
         conma(1,1)=cos(-1.*pi/2.)
         conma(1,2)=-sin(-1.*pi/2.)
         conma(1,3)=0.
         conma(2,1)=sin(-1.*pi/2.)
         conma(2,2)=cos(-1.*pi/2.)
         conma(2,3)=0.
         conma(3,1)=0.
         conma(3,2)=0.
         conma(3,3)=1.


      !Read focal mechanism information 
      open(iunit,file=proc_mesh%dat//'focal.info',&
  &        status='old',action='read')
       read(iunit,*) proc_mesh%stk, proc_mesh%dip, proc_mesh%rak
       close(iunit)

         !Convert angles to radiants
         print *, proc_mesh%stk, proc_mesh%dip
         proc_mesh%stk=proc_mesh%stk*(2.0*pi/360.0)
         proc_mesh%dip=proc_mesh%dip*(2.0*pi/360.0)
         proc_mesh%rak=(proc_mesh%rak+180.)*(2.0*pi/360.0)

         !Fault unitary normal vector used for transformation (Stein p.218 eq.2)
         !Same as AXITRA
         proc_mesh%vnorm(1)=-1.0*sin(proc_mesh%dip)*sin(proc_mesh%stk) ! 1 x para el este
         proc_mesh%vnorm(2)=     sin(proc_mesh%dip)*cos(proc_mesh%stk)
         proc_mesh%vnorm(3)=-1.0*cos(proc_mesh%dip)

         !Unitary slip vector
         proc_mesh%vslip(1) = (cos(proc_mesh%rak)*cos(proc_mesh%stk)) + &   !-1.0
  &      sin(proc_mesh%rak)*cos(proc_mesh%dip)*sin(proc_mesh%stk)
         proc_mesh%vslip(2) =( cos(proc_mesh%rak)*sin(proc_mesh%stk)) - &
  &      (sin(proc_mesh%rak)*cos(proc_mesh%dip)*cos(proc_mesh%stk))
         !Because of the orientation on the mesh
         proc_mesh%vslip(3) = -1.0*sin(proc_mesh%rak)*sin(proc_mesh%dip)  !-1 z para arriba

         print *, 'norm no trans', proc_mesh%vnorm
         print *, 'slip no trans', proc_mesh%vslip

         !Rotate the normal vector around the Z axis
         nor(:)=0.
         do cont=1,3
          do cont2=1,3
           nor(cont)=nor(cont) +conma(cont,cont2)*proc_mesh%vnorm(cont2)
          enddo
         enddo

         print *, 'Normal vector to fault plane', nor

         proc_mesh%vnorm(:)=nor


         print *, 'vnorm trans', proc_mesh%vnorm
         print *, 'vslip trans', proc_mesh%vslip

         return
         endsubroutine norm_vec
