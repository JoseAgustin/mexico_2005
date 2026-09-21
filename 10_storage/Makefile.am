include $(top_srcdir)/common.am

# Este programa usa la interfaz Fortran de NetCDF
AM_FCFLAGS += $(NETCDF_FCFLAGS)
LDADD       = $(NETCDF_LIBS)

bin_PROGRAMS       = radm2bio4
radm2bio4_SOURCES  = guarda2_nc2005.f90
