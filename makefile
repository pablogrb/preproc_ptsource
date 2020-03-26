FC = pgf90

# FLAGS = -O3 -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio
FLAGS = -g -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio

TARGET = preproc_ptsource

MODULES = \
class_uam_iv.o \
lcpgeo.o \
pspgeo.o
PROGRAMS = \
preproc_ptsource.o

OBJECTS = $(MODULES) $(PROGRAMS)

preproc_ptsource: $(OBJECTS)
	$(FC) $^ -o $@ $(FLAGS)

%.mod: %.f90
	$(FC) -c $^ $(FLAGS)

%.o: %.f90
	$(FC) -c $^ $(FLAGS)

.PHONY: clean

clean:
	rm -f $(OBJECTS) *.mod
