# Fortran Music Manager Makefile

FC = gfortran
FCFLAGS = -O2 -Wall -fPIC
FCFLAGS_DEBUG = -g -O0 -Wall -fPIC -fcheck=all -fbacktrace
LDFLAGS = -lfcgi -lsqlite3

# Source files
MODULES = env_module.o db_module.o auth_module.o mp3_module.o template_module.o
MAIN = fortran_fcgi.f90

# Output
TARGET = fortran_fcgi

all: $(TARGET)

$(TARGET): $(MODULES) $(MAIN)
	$(FC) $(FCFLAGS) -o $@ $(MODULES) $(MAIN) $(LDFLAGS)

# Debug build with full runtime checks
debug: FCFLAGS = $(FCFLAGS_DEBUG)
debug: clean $(MODULES) $(MAIN)
	$(FC) $(FCFLAGS_DEBUG) -o $(TARGET) $(MODULES) $(MAIN) $(LDFLAGS)

env_module.o: env_module.f90
	$(FC) $(FCFLAGS) -c $<

db_module.o: db_module.f90
	$(FC) $(FCFLAGS) -c $<

auth_module.o: auth_module.f90 env_module.o
	$(FC) $(FCFLAGS) -c $<

mp3_module.o: mp3_module.f90
	$(FC) $(FCFLAGS) -c $<

template_module.o: template_module.f90
	$(FC) $(FCFLAGS) -c $<

clean:
	rm -f *.o *.mod $(TARGET)

.PHONY: all clean debug

