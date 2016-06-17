cd ../filtro/test/
rm *mod
ifort init_butterworth.f90 butterworth_filter.f90 filtfilt_butterworth.f90 main.f90 -I ../../include -o test
cp *mod ../../obj/
cd ../../obj/
make
