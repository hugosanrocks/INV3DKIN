       subroutine hypo(green_mesh)

        !COMMON VARIABLES
        IMPLICIT NONE
        INCLUDE 'green.h'
        TYPE (mesh) :: green_mesh

        integer i, j, k, iunit
        real dist(green_mesh%msub)
        real lambda
        real expp(green_mesh%msub), al, be, hypol(1,3)


        al=1.
        be=0.
        !SAME AS Mathilde Radiguet THESIS EQ (3.3) 
        !Controling parameters of covariance matrix
        lambda = 1000.     !distance of exponential decay

        green_mesh%fault(:,:)=0.
        green_mesh%cm(:,:)=0.
        dist(:) = 0.
print *, 'ini'

         !File where to read the subfault positions
         iunit=10
         open(iunit,file=green_mesh%dat//'fault.pos',status='old',&
 &            action='read',access='DIRECT',recl=green_mesh%msub*4*green_mesh%ncomp)
         !Read subfault positions (x,y,z)
         read(iunit,rec=1) green_mesh%fault(:,:)
         close(iunit)
print *, 'fault read'

       !For instance we locate manually the hypoceter, set at subfault 211
       hypol(1,:) = green_mesh%fault(211,:)


       !Distance from each subfault center to the hypocenter
       do i=1,green_mesh%msub
         dist(i) = sqrt( (hypol(1,1)-green_mesh%fault(i,1))**2 +&
  &                (hypol(1,2)-green_mesh%fault(i,2))**2 +&
  &                (hypol(1,3)-green_mesh%fault(i,3))**2  )*1000.
         expp(i) =  exp(-1.* (dist(i) / lambda) )
       enddo
print *, 'cycle'

       !Maximum penalization
       expp(211) = 1.
       expp(187) = 1.
       expp(235) = 1.


       !set to global variable
       green_mesh%diag2(:) = expp(:)
print *, 'diag'

       
       endsubroutine hypo
