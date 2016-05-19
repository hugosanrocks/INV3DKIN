include ../install/make.hugo-PC.inc
#

all : FWI_INV3DKIN FORWARD PREPROC_GEODG

BASE_DIR        =      ../..
INV_BASE_DIR    =      $(BASE_DIR)/INV3DKIN
PREPROC_DIR     =      $(INV_BASE_DIR)/preprocess
INV_GENERAL_DIR =      $(INV_BASE_DIR)/general
FILTER_DIR      =      $(INV_BASE_DIR)/filtro/test
FWI_INV_DIR     =      $(INV_BASE_DIR)/inversion
COV_INV_DIR     =      $(INV_BASE_DIR)/covariance
FOR_INV_DIR     =      $(INV_BASE_DIR)/forward
ADJ_INV_DIR     =      $(INV_BASE_DIR)/adjoint
INV_INC_DIR     =      $(INV_BASE_DIR)/include
INV_EXE_DIR     =      $(INV_BASE_DIR)/bin

VPATH = $(BASE_DIR):$(INV_BASE_DIR):$(PREPROC_DIR):$(FILTER_DIR):$(INV_GENERAL_DIR):$(INV_EXE_DIR):$(COV_INV_DIR):$(FWI_INV_DIR):$(FOR_INV_DIR):$(ADJ_INV_DIR)

FFLAGS    = -I. -I $(INV_INC_DIR) -I $(IOPTIM) $(FOPTION)
FFLAGS2   = -openmp

#first : test_Connect1D

COV_OBJ     =  covariance.o model1d.o covmodel.o exp_covar.o edge.o time_corr.o covedge.o modeledge.o timemask.o modeltime.o covtime.o covtimer.o modeltimer.o laplacian.o covtiko.o modeltiko.o modelpri.o covpri.o

FILTER_OBJ  =  init_butterworth.o filtfilt_butterworth.o

#matrix.o not used now

GENERAL_OBJ = orderinv.o linreg.o conv.o ortogonal.o rotate.o modeldecomp.o inv_mat.o

PREPROC_OBJ = preprocess.o readinfop.o mul_src.o stress_axi.o interpolation.o stress_dwn.o interp_axi.o interp_dwn.o traction.o normvec.o tract_fft.o medium.o

OPTIM_OBJ = fcost.o fwi_option.o fwi_pstd.o fwi_lbfgs.o fwi_plbfgs.o fwi_pnlcg.o

INIT_OBJ = initialize.o initializeadj.o readadjinfo.o modelcheck.o

MAIN_OBJ  = main.o

FORWARD_OBJ = readinfo.o coortrans.o forward.o syn_velocity.o write_syn.o progress.o

ADJOINT_OBJ = adjoint.o residual.o write_residual.o adjtraction.o read_traction.o read_obs2.o

MODEL_GRAD_OBJ = writemodel.o newgrad.o readgrad.o

ALL_OBJ = $(INIT_OBJ) $(PREPROC_OBJ) $(GENERAL_OBJ) $(OPTIM_OBJ) $(MAIN_OBJ) $(FORWARD_OBJ) $(MODEL_GRAD_OBJ) $(ADJOINT_OBJ) $(COV_OBJ) $(FILTER_OBJ)

FOR_OBJ = $(PREPROC_OBJ) $(FILTER_OBJ) mainforward.o writemodel.o $(INIT_OBJ) $(FORWARD_OBJ) $(GENERAL_OBJ)

PRE_OBJ = $(PREPROC_OBJ) $(FILTER_OBJ) linreg.o prep.o rotate.o

.SUFFIXES: .f90 .o
.f90.o:
	$(F90) -openmp -c  $(FFLAGS) $<

.SUFFIXES: .c .o
.c.o:
	$(CC) -c $(C_OPTION) $(FFLAGS) $<

PREPROC_GEODG : $(PRE_OBJ)
	$(F90) $(FFLAGS) -o $(INV_EXE_DIR)/$@ $(PRE_OBJ) $(LIBS2) -openmp

FORWARD : $(FOR_OBJ)
	$(F90) $(FFLAGS) -o $(INV_EXE_DIR)/$@ $(FOR_OBJ) $(LIBS2) -openmp

FWI_INV3DKIN : $(ALL_OBJ)
	$(F90) $(FFLAGS) -o $(INV_EXE_DIR)/$@ $(ALL_OBJ) $(LIBS) $(LIBS2) $(LIBS3)

objs : $(ALL_OBJ)

$(ALL_OBJ): $(INV_INC_DIR)/precis.h $(INV_INC_DIR)/fftw3.f $(INV_INC_DIR)/green.h $(INV_INC_DIR)/varprec.h

clean:
	rm -f *.o  *~ ../bin/FWI_INV3DKIN ../bin/FORWARD ../bin/PREPROC_GEODG

