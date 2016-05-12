       TYPE mesh

       SEQUENCE

       CHARACTER (LEN=4) dat, out                     	! Working directories
       REAL*4, DIMENSION(:,:), POINTER ::  slipmod         	! Slip rate modulus
       REAL*4, DIMENSION(:,:),POINTER :: stacoor           	! Stress elements read

         !Synthetics, adjoint traction and residuals
         REAL*4, DIMENSION(:,:),POINTER :: syn, synt, res, obs    	! Stress elements read

         !Focal mechanism
         REAL*4 stk, dip, rak                               	! Focal mechanism
         REAL*4 lenstk, lendip, stk_s, dip_s			! Fault & subfault lengths

         REAL*4 vnorm(3), vslip(3)                          	! Normal & slip unitary vectors
         REAL*4 vslip2(2), vstk(3), vdip(3), slipm(2,3)         ! slip along stk and dip, strike vector, dip vector, transformation matrix
         REAL*4 stso(3,3), traction(3)                      	!Stress tensor and traction vector
         REAL*4 simdt, tr, slipt, slipdt, simt                 !Simulation dt, rise time, slip final time, slip dt, simulation final time
         REAL*4, DIMENSION(:),POINTER :: cost               	! Cost function
         REAL*4 nsubf
         real*4 costa                                        	! Total accumulative cost
         REAL*4 moment, mu, rt, ot                           	! seismic moment, rise time, origin time
         INTEGER*4 ncomp, msub, nsta, scomp, lensyn, lensynf            ! Number of stations, components and subfaults, length of synthetics

         INTEGER*8 stcomp, sta_i, comp_i, mjump, prog            	! Counter on stations, components and staXcomp

         INTEGER*8 simsam, slipsam, interp_i, interpadj_i      ! Number of samples of simulation, slip, forward interpolation, adjoint records
         INTEGER*4 delays, iter                              	! Number of samples of delay, iteration
         INTEGER*4 modelsize, modelsize2                      		! FWI modelsize
         CHARACTER (LEN=3) :: sta, sub                      	! Variables to write file names
         CHARACTER (LEN=1) :: comp                         	! Variables to write file names
         CHARACTER (LEN=20) :: FILE_S, FILE_OPT
         Character (len=5) :: iter_i

         REAL*4, DIMENSION(:,:), POINTER :: stsi, stinterp
         REAL*4, DIMENSION(:,:), POINTER :: slipr, tracadj

         REAL*4, DIMENSION(:,:), POINTER :: tottrac, tractionv
         REAL*4, DIMENSION(:), POINTER :: model,model2, cx, ch, cz

         REAL*4, DIMENSION(:,:), POINTER :: tractionvec
         !DOUBLE COMPLEX, DIMENSION (:,:), POINTER :: tracf, slipf
         COMPLEX, DIMENSION (:,:), POINTER :: tracf, slipf, resif


         INTEGER tfft_i

         INTEGER debug_i, stress_opt
         LOGICAL debug         

       END TYPE mesh
