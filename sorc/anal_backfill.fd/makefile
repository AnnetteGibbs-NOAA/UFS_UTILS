SHELL=	/bin/sh

CMD= a.out

OBJS = driver.o

$(CMD):	$(OBJS)
	$(FCOMP) $(FFLAGS) -o $(CMD) $(OBJS) $(IP_LIB4) $(SP_LIB4) $(W3NCO_LIB4) $(NETCDF_LDFLAGS_F) -g -traceback

driver.o:  driver.f90
	$(FCOMP) $(FFLAGS) -c driver.f90

clean:
	rm -f *.o *.mod ${CMD}

install:
	mv $(CMD) ../../exec
