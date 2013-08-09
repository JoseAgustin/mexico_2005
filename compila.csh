#!/bin/csh
#
#  Compila Programs
cd $PWD
cd 01_pob
ifort -o cel2.exe -O2 -axAVX -fp-model precise celda_pob2.f90
cd ..
cd 02_aemis
ifort -o ASpatial.exe -O2 -axAVX -fp-model precise area_espacial.f90
cd ..
cd 03_movilspatial
ifort -O2 -axAVX -fp-model precise suma_carretera.f90 -o carr.exe
ifort -O2 -axAVX -fp-model precise suma_vialidades.f90 -o vial.exe
ifort -O2 -axAVX -fp-model precise agrega.f90 -o agrega.exe
cd ..
cd 04_temis
ifort -O2 -axAVX -fp-model precise  atemporal.f90 -o Atemporal.exe
cd ..
cd 05_semisM
ifort -O3 -axAVX -fp-model precise movil_spatial_sn2.f90 -o MSpatial_sn2.exe
ifort -O3 -axAVX -fp-model precise movil_spatial.f90 -o MSpatial.exe
cd ..
cd 06_temisM
ifort -O3 -axAVX -fp-model precise movil_temp.f90 -o Mtemporal.exe
cd ..
cd 07_puntual
ifort -O3 -axAVX -fp-model precise t_puntal.f90 -o Puntual.exe
cd ..
cd 08_spec
ifort -O3 -axAVX -fp-model precise agg_a.f90 -o spa.exe
ifort -O3 -axAVX -fp-model precise agg_m.f90 -o spm.exe
ifort -O3 -axAVX -fp-model precise agg_p.f90 -o spp.exe
cd ..
cd 09_pm25spec
ifort -O3 -axAVX -fp-model precise pm25_speci_a.f90 -o spm25a.exe
ifort -O3 -axAVX -fp-model precise pm25_speci_m.f90 -o spm25m.exe
ifort -O3 -axAVX -fp-model precise pm25_speci_p.f90 -o spm25p.exe
cd ..
cd 10_storage
set FC=`nc-config --fc`
set FFL=`nc-config --fflags`
set FLL=`nc-config --flibs`
$FC' '$FFL' '$FLL' '-O2 -axAVX -fp-model precise guarda2bio_nc4.f90 -o radm2bio4.exe
cd ..
cd 12_biogenic
ifort -O3 -axAVX -fp-model precise  btemporal.f90 -o Btemporal.exe

