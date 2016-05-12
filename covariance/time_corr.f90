        subroutine time_corr(green_mesh)

        !COMMON VARIABLES
        IMPLICIT NONE
        INCLUDE 'green.h'
        TYPE (mesh) :: green_mesh

        integer i, j, k, iunit
        real dist(green_mesh%interp_i,green_mesh%interp_i)
        real lambda, lambda0, sigmam, factor, t(green_mesh%interp_i)
        integer hyp

        !SAME AS Mathilde Radiguet THESIS EQ (3.3) 
        !Controling parameters of covariance matrix
        lambda = 2. !1. * green_mesh%slipdt    !time distance of corr
        !lambda0 = 0.04     !3 km
        !sigmam = 0.02    !0.5 meters
        !factor = ( sigmam * (lambda / lambda0 ) )**2
        !hyp = 210
        green_mesh%ct(:,:)=0.

        !distance in time
        dist(:,:) = 0.

         !File where to read the subfault positions
        t(1) = 0.
        do i=2,green_mesh%interp_i
         t(i) = t(i-1) + green_mesh%slipdt
       enddo

         !Used to check the subfault positions
         !do j=1,green_mesh%interp_i
         !  write(77,*) t(j)
         !enddo

       !Estimate distance and correlation matrices
       do i=1,green_mesh%interp_i
        do j=1,green_mesh%interp_i
         dist(i,j) = sqrt( (t(i)- t(j) )**2. )
         green_mesh%ct(i,j) =  exp(-1.* (dist(i,j) / lambda) )
        enddo
       enddo

       !Used only to check values of distance and covariance
       do i=1,green_mesh%interp_i
        do j=1,green_mesh%interp_i
         write(991,*) green_mesh%ct(i,j)
        enddo
        enddo


       green_mesh%ct(:,:) = 0.
       open(222,file='dat/ct.dat',status='unknown')
       do i=1,green_mesh%interp_i
        read(222,*) green_mesh%ct(i,:)
       enddo
       close(222)

       !Used only to check values of distance and covariance
       !do i=1,green_mesh%interp_i
       ! do j=1,green_mesh%interp_i
       !  write(991,*) green_mesh%ct(i,j)
       ! enddo
       ! enddo

       endsubroutine time_corr
