       TYPE mesh

       SEQUENCE

       CHARACTER (LEN=4) dat, out                     		  ! Working directories
       REAL*4, DIMENSION(:,:), POINTER ::  slipmod         	  ! Slip rate modulus
       
       !Synthetics, adjoint traction, residuals and observations
       REAL*4, DIMENSION(:,:),POINTER :: syn, synt, res 
       REAL*4, DIMENSION(:,:),POINTER :: obs, slip                ! Arrays to save traces

       !Focal mechanism
       REAL*4 stk, dip, rak                                 	  ! Focal mechanism
       REAL*4 lenstk, lendip, stk_s, dip_s			  ! Fault and subfault lengths (strike and dip)

       REAL*4 vnorm(3), vslip(3), traction(3)                  	  ! Normal & slip unitary vectors
       REAL*4 vslip2(2), vstk(3), vdip(3), slipm(2,3)         	  ! slip vector along stk and dip, strike vector, dip vector, proj. matrix

       REAL*4, DIMENSION(:,:),POINTER :: fault                    ! Position of each subfault

       REAL*4 simt, simdt, slipt, slipdt                 	  ! Simulation t and dt (Green's functions), slip t and dt (to find)
       REAL*4, DIMENSION(:),POINTER :: cost, diag, diag2             	  ! Cost function
       REAL*4 costa, costm, lam1, lam2, lam3                   	  ! Total accumulative cost

       REAL*4 moment, mu, rt, ot, syn_sec                     	  ! seismic moment, shear modulus, rise time, origin time

       INTEGER*4 nsta, ncomp, msub, nsubf, lensyn, lensynf,lenobs ! Number of: stations, components and subfaults, length of traces
       INTEGER*8 stcomp, sta_i, comp_i, mjump, prog            	  ! Counter on stations, components and staXcomp, jump inside files, prog bar

       INTEGER*8 simsam, slipsam, interp_i, trac_i, syn_i
       INTEGER*4 interpadj_i      	  			  ! Number of samples of: simulation, slip, forward interpolation, adjoint records
       INTEGER*4 delays, iter                              	  ! Number of samples of delay (due to origin time), number of  iteration
       INTEGER*4 modelsize, modelsize2                      	  ! number of samples in the model to optimize


       INTEGER*4, DIMENSION(:), POINTER :: synsam                 ! rupture sample time

       CHARACTER (LEN=3) :: sta, sub                      	  ! Variables to write file names
       CHARACTER (LEN=1) :: comp                         	  ! Variables to write file names
       CHARACTER (len=5) :: iter_i

       REAL*4, DIMENSION(:,:), POINTER :: slipr, tracadj, tottrac 
       REAL*4, DIMENSION(:,:), POINTER :: tractionvec 
								  ! Arrays to save current sliprate and gradient

       REAL*4, DIMENSION(:), POINTER :: model,modelp,model2
       REAL*4, DIMENSION(:), POINTER :: model2p,grad, grad2
       REAL*4, DIMENSION(:), POINTER :: gradad, rtimes            ! 1D arrays used by TOOLBOX to optmize
       REAL*4, DIMENSION(:,:), POINTER :: slipr2, tracad          ! 1D arrays used by TOOLBOX to optmize
       INTEGER*4, DIMENSION(:), POINTER :: rsamp                  ! rupture sample time

       INTEGER*4, DIMENSION(:,:), POINTER :: samwin               ! samples of time window


       !DOUBLE COMPLEX, DIMENSION (:,:), POINTER :: tracf, slipf  ! Arrays for fft conversion (slip and traction)
       COMPLEX, DIMENSION (:,:), POINTER :: tracf, slipf, resif
       INTEGER tfft_i                                             ! Counter used by fftw3

       INTEGER debug_i, stress_opt, for_opt, optf, optm, mext           ! Debug options, origin of stress files, forward option
       LOGICAL debug         


       REAL*4, DIMENSION(:,:),POINTER :: cd, cm, ce, ct, la       ! Arrays to covariance matrices
       INTEGER weig
       REAL*4, DIMENSION(:),POINTER :: tseries                    ! Subfault's positions (x,y,z)
       INTEGER wininv                                             ! Number of the time window to forward
       INTEGER*4, DIMENSION(:), POINTER :: idsub, win

       END TYPE mesh

!---------------------
! Butterworth filter
!---------------------
TYPE butter

SEQUENCE

   ! Order of the butterworth filter
   INTEGER order

   ! Cutoff frequency
   REAL fc

   ! Filter coefficients
   REAL C(5,10)

   ! Coefficients defining the filter memory
   REAL D(2,10)

   ! Group delay in seconds
   REAL Tg

   ! Number of required 2nd order sections
   INTEGER NSections


END TYPE butter

