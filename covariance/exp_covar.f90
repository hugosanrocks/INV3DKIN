        subroutine exp_covar(green_mesh)

        !COMMON VARIABLES
        IMPLICIT NONE
        INCLUDE 'green.h'
        TYPE (mesh) :: green_mesh

        integer i, j, k, iunit
        real dist(green_mesh%msub,green_mesh%msub)
        real lambda, lambda0, sigmam, factor
        integer hyp

        !SAME AS Mathilde Radiguet THESIS EQ (3.3) 
        !Controling parameters of covariance matrix
        lambda = 3000.!500.     !9 km
        lambda0 = 1500.     !3 km
        sigmam = 0.1    !0.5 meters
        factor = ( sigmam * (lambda / lambda0 ) )**2
        hyp = 210

        green_mesh%fault(:,:)=0.
        green_mesh%cm(:,:)=0.
        dist(:,:) = 0.

         !File where to read the subfault positions
         iunit=10
         open(iunit,file=green_mesh%dat//'fault.pos',status='old',&
 &            action='read',access='DIRECT',recl=green_mesh%msub*4*green_mesh%ncomp)
         !Read subfault positions (x,y,z)
         read(iunit,rec=1) green_mesh%fault(:,:)
         close(iunit)
         !Used to check the subfault positions
         do j=1,green_mesh%msub
           write(77,*) green_mesh%fault(j,:)
         enddo

       !Estimate distance and correlation matrices
       do i=1,green_mesh%msub
        do j=1,green_mesh%msub
         dist(i,j) = sqrt( (green_mesh%fault(j,1)-green_mesh%fault(i,1))**2 +& 
  &                (green_mesh%fault(j,2)-green_mesh%fault(i,2))**2 +&
  &                (green_mesh%fault(j,3)-green_mesh%fault(i,3))**2  )*1000.
         green_mesh%cm(i,j) = factor * exp(-1.* (dist(i,j) / lambda) )
          if ((dist(i,j) .lt. 1510) .and. (i .ne. j)) then
            green_mesh%la(i,j) = -1.
          endif
        enddo
       enddo

       green_mesh%la(:,:) = green_mesh%la(:,:) / 1500.**2
        do i=1,288
         do j=1,288
          write(33,*) green_mesh%la(i,j)
         enddo
        enddo


       !Used only to check values of distance and covariance
       k=1
       do i=1,green_mesh%msub
        do j=1,green_mesh%msub
       !  write(88,*) dist(i,:)
         write(999,*) green_mesh%cm(i,j)
       !  k=k+1
        enddo
        enddo

       open(111,file='dat/corr.dat',status='unknown')
       do i=1,green_mesh%msub
        read(111,*) green_mesh%cm(i,:)
       enddo
       close(111)



       !Travel times
       !TO BE CHANGED FOR VARIABLE VS VELOCITY
       green_mesh%rtimes(:) = dist(hyp,:) / 4620. !4620 MAX velo siv1
       do i=1,green_mesh%msub
       green_mesh%rsamp(i) = floor( (dist(hyp,i) / 4620. ) / green_mesh%slipdt )
       write(31,*) green_mesh%rsamp(i)
       enddo
       

       call time_mask(green_mesh)


       endsubroutine exp_covar
