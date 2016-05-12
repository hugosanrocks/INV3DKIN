       TYPE mesh

       SEQUENCE

       CHARACTER (LEN=4) dat, out                     		  ! Working directories
       REAL*4, DIMENSION(:,:), POINTER ::  slipmod         	  ! Slip rate modulus
       
       !Synthetics, adjoint traction, residuals and observations
       REAL*4, DIMENSION(:,:),POINTER :: syn, synt, res, obs      ! Arrays to save traces

       !Focal mechanism
       REAL*4 stk, dip, rak                                 	  ! Focal mechanism
       REAL*4 lenstk, lendip, stk_s, dip_s			  ! Fault and subfault lengths (strike and dip)

       REAL*4 vnorm(3), vslip(3), traction(3)                          	  ! Normal & slip unitary vectors
       REAL*4 vslip2(2), vstk(3), vdip(3), slipm(2,3)         	  ! slip vector along stk and dip, strike vector, dip vector, proj. matrix

       REAL*4, DIMENSION(:,:),POINTER :: fault                    ! Position of each subfault

       REAL*4 simt, simdt, slipt, slipdt                 	  !Simulation t and dt (Green's functions), slip t and dt (to find)
       REAL*4, DIMENSION(:),POINTER :: cost, diag               	  ! Cost function
       REAL*4 costa, costm, lam1, lam2, lam3                   	  ! Total accumulative cost

       REAL*4 moment, mu, rt, ot                           	  ! seismic moment, shear modulus, rise time, origin time

       INTEGER*4 nsta, ncomp, msub, nsubf, lensyn, lensynf        ! Number of: stations, components and subfaults, length of traces
       INTEGER*8 stcomp, sta_i, comp_i, mjump, prog            	  ! Counter on stations, components and staXcomp, jump inside files, prog bar

       INTEGER*8 simsam, slipsam, interp_i, interpadj_i      	  ! Number of samples of: simulation, slip, forward interpolation, adjoint records
       INTEGER*4 delays, iter                              	  ! Number of samples of delay (due to origin time), number of  iteration
       INTEGER*4 modelsize, modelsize2                      	  ! number of samples in the model to optimize

       CHARACTER (LEN=3) :: sta, sub                      	  ! Variables to write file names
       CHARACTER (LEN=1) :: comp                         	  ! Variables to write file names
       CHARACTER (len=5) :: iter_i

       REAL*4, DIMENSION(:,:), POINTER :: slipr, tracadj, tottrac, tractionvec 
								  ! Arrays to save current sliprate and gradient

       REAL*4, DIMENSION(:), POINTER :: model,model2, grad, grad2, gradad, rtimes              ! 1D arrays used by TOOLBOX to optmize
       REAL*4, DIMENSION(:,:), POINTER :: slipr2, tracad                       ! 1D arrays used by TOOLBOX to optmize
       INTEGER*4, DIMENSION(:), POINTER :: rsamp                               ! rupture sample time

       !DOUBLE COMPLEX, DIMENSION (:,:), POINTER :: tracf, slipf  ! Arrays for fft conversion (slip and traction)
       COMPLEX, DIMENSION (:,:), POINTER :: tracf, slipf, resif
       INTEGER tfft_i                                             ! Counter used by fftw3

       INTEGER debug_i, stress_opt, for_opt                       ! Debug options, origin of stress files, forward option
       LOGICAL debug         


       REAL*4, DIMENSION(:,:),POINTER :: cd, cm, ce, ct                  ! Arrays to covariance matrices
       INTEGER weig

       END TYPE mesh
