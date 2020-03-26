!	------------------------------------------------------------------------------------------
!	UAM-IV File Module
!	------------------------------------------------------------------------------------------

MODULE class_UAM_IV
IMPLICIT NONE

! 	Public variables
	PUBLIC :: UAM_IV

! 	Boundary parameter derived type structure
	TYPE :: UAM_BC_PAR
! 		Parameter
		INTEGER :: iedge							! Edge number (1=west, 2=east, 3=south, 4=north)
		INTEGER :: ncell							! Number of cells
		INTEGER, ALLOCATABLE :: iloc(:,:)			! Index of first cell modeled (4,max(ncell))(?)
! 		Concentrations
		REAL, ALLOCATABLE :: bc_conc(:,:,:,:)		! Boundary concentration array
													! (cell, layer, hour, species)
	END TYPE

!	UAM-IV Derived Type Structure					! Input files
	TYPE :: UAM_IV

! 		Header
		CHARACTER(LEN=256) :: in_file				! Input filename
		INTEGER :: unit								! Input unit
		CHARACTER(LEN=4), DIMENSION(10) :: fname	! Name array
		CHARACTER(LEN=10) :: ftype					! File type
		CHARACTER(LEN=4), DIMENSION(60) :: note		! Note array
		INTEGER :: nseg,nspec,idate,jdate			! Species number and dates
		REAL :: begtim, endtim						! Hours
		INTEGER :: iutm,nx,ny						! UTM zone, horizontal grid
		INTEGER :: nz,nzlo,nzup						! Vertical grid
		REAL :: hts,htl,htu							! Others ???
		INTEGER :: i1,j1,nx1,ny1					! Others ???
		REAL :: orgx,orgy,utmx,utmy,dx,dy			! Grid origin and spacing

!		Timing
		INTEGER :: update_times = 24				! Number of emissions data points per file
													! Assumes 24 hours, change as necessary

! 		Species
		CHARACTER(LEN=4), ALLOCATABLE :: spname(:,:)	! Species name array
		CHARACTER(LEN=10), ALLOCATABLE :: c_spname(:)	! Species name array (in single char)
		LOGICAL lhdrspec, l3d, lbndry, lptsrc		! Filetype logicals
! 		l3d may not be necessary

!		Time variant headers
		INTEGER, ALLOCATABLE :: ibgdat(:), iendat(:)! Beggining and end date of emission records
		REAL, ALLOCATABLE :: nbgtim(:), nentim(:)	! Beggining and end time of emission records

!		AIRQUALITY Type
!		3D concentration array
		REAL, ALLOCATABLE :: conc(:,:,:,:,:)		! Concentration array
													! (col, row, layer, hour, species)

!		EMISSIONS Type
!		2D emissions array
		REAL, ALLOCATABLE :: aemis(:,:,:,:)			! Emissions array
													! (col, row, hour, species)
!		PTSOURCE Type
! 		Stack Parameters
		INTEGER :: nstk								! Number of stacks
		REAL, ALLOCATABLE :: xstk(:), ystk(:)		! Stack location
		REAL, ALLOCATABLE :: hstk(:), dstk(:)		! Stack height and diameter
		REAL, ALLOCATABLE :: tstk(:), vstk(:)		! Stack temperature and velocity

! 		Stack Emissions
		INTEGER, ALLOCATABLE :: icell(:,:),jcell(:,:)	! idum in the CAMx manual
		INTEGER, ALLOCATABLE :: kcell(:,:)			! Ignored, except as flag for OSAT
		REAL, ALLOCATABLE :: flow(:,:)				! Stack flow rate (m3 /hr)
		REAL, ALLOCATABLE :: plmht(:,:)				! Effective plume height override (m)
		REAL, ALLOCATABLE :: ptemis(:,:,:)			! Species point emission rate (mol/time 
													! period for gases, g/time period for
													! aerosols) update_times x nstk x nspec

!		BOUNDARY Type
! 		Boundary type array
		TYPE(UAM_BC_PAR), DIMENSION(4) :: bc		! bc parameter derived typer array

!		Type functions
		CONTAINS
			PROCEDURE :: read => read_uamfile
			PROCEDURE :: write => write_uamfile
			PROCEDURE :: spindex => fl_spindex

	END TYPE UAM_IV

! 	Public methods
	PUBLIC :: read_uamfile, write_uamfile

! 	Private methods
	PRIVATE :: read_open_file, read_header, read_species
	PRIVATE :: read_grid_conc
	PRIVATE :: read_grid_emis
	PRIVATE :: read_stack_param, read_stack_emis
	PRIVATE :: read_bound_param

	PRIVATE :: write_open_file, write_header, write_species
	PRIVATE :: write_grid_conc
	PRIVATE :: write_stack_param, write_stack_emis

CONTAINS

!	------------------------------------------------------------------------------------------
!	Public Methods
!	------------------------------------------------------------------------------------------
!		File IO

	SUBROUTINE read_uamfile(fl,in_file,unit)

		CLASS(UAM_IV), INTENT(INOUT) :: fl
		CHARACTER(LEN=256), INTENT(IN), OPTIONAL :: in_file
		INTEGER, INTENT(IN), OPTIONAL :: unit

! 		Open the file
		CALL read_open_file(fl,in_file,unit)
! 		Read the header
		CALL read_header(fl)
! 		Read the species names
		CALL read_species(fl)

! 		Call different methods according to file type
		SELECT CASE (fl%ftype)
		CASE ('AVERAGE   ','AIRQUALITY')
! 			Read the 3D concentration grid
			CALL read_grid_conc(fl)
		CASE ('EMISSIONS ')
! 			Read the 2D emissions grid
			CALL read_grid_emis(fl)
		CASE ('PTSOURCE  ')
!	 		Read the stack parameters
			CALL read_stack_param(fl)
! 			Read the emission records
			CALL read_stack_emis(fl)
		CASE ('BOUNDARY  ')
! 			Read the boundary parameters
			CALL read_bound_param(fl)
! 			Read the boundary concentrations
			CALL read_bound_conc(fl)
		CASE DEFAULT
			WRITE(0,*) 'class_UAM_IV error: Not a valid file type'
			CALL EXIT(0)
		END SELECT

! 		Close the file
		CALL close_file(fl)

	END SUBROUTINE read_uamfile

	SUBROUTINE write_uamfile(fl,in_file,unit)

		CLASS(UAM_IV), INTENT(INOUT) :: fl
		CHARACTER(LEN=256), INTENT(IN), OPTIONAL :: in_file
		INTEGER, INTENT(IN), OPTIONAL :: unit

! 		Open the file
		CALL write_open_file(fl,in_file,unit)
! 		Write the header
		CALL write_header(fl)
! 		Write the species names
		CALL write_species(fl)

! 		Call different methods according to file type
		SELECT CASE (fl%ftype)
		CASE ('AVERAGE   ','AIRQUALITY')
! 			Write the 3D concentration grid
			CALL write_grid_conc(fl)
		CASE ('EMISSIONS ')
! 			Write the 2D emissions grid
			CALL write_grid_emis(fl)
		CASE ('PTSOURCE  ')
!	 		Write the stack parameters
			CALL write_stack_param(fl)
! 			Write the emission records
			CALL write_stack_emis(fl)
		CASE ('BOUNDARY  ')
! 			Write the boundary parameters
			CALL write_bound_param(fl)
! 			Write the boundary concentrations
			CALL write_bound_conc(fl)
		CASE DEFAULT
			WRITE(0,*) 'class_UAM_IV error: Not a valid file type'
			CALL EXIT(0)
		END SELECT

! 		Close the file
		CALL close_file(fl)

	END SUBROUTINE write_uamfile

!	------------------------------------------------------------------------------------------
!		Utilities

	FUNCTION fl_spindex(fl,spec) result(index)

		CLASS(UAM_IV), INTENT(IN) :: fl
		CHARACTER(LEN=*), INTENT(IN) :: spec
		INTEGER :: index,i_sp

!		Test for allocation of variable
		IF (.NOT. ALLOCATED(fl%c_spname)) THEN
			WRITE(0,*) 'The species list was not allocated'
			WRITE(0,*) 'Make sure that the file header or full file has been read first'
			CALL EXIT(1)
		END IF

		index = 0
!		Use c_spname as a lookup table
		DO i_sp = 1, fl%nspec
			IF (TRIM(spec) == TRIM(fl%c_spname(i_sp))) THEN
				index = i_sp
				EXIT
			END IF
		END DO

		IF (index == 0) THEN
			WRITE(0,*) 'The species ', spec, ' was not found in the species list'
			CALL EXIT(1)
		END IF

	END FUNCTION fl_spindex

! 	SUBROUTINE clone_header(fl_inp, fl_out)

! 		TYPE(UAM_IV), INTENT(IN) :: fl_inp
! 		TYPE(UAM_IV), INTENT(INOUT) :: fl_out

! !		Build the output file header using the header of the first file and the user dates
! 		fl_out%ftype  = fl_inp%ftype
! 		fl_out%update_times = fl_inp%update_times
! !		Header 1
! 		fl_out%fname  = fl_inp%fname
! 		fl_out%note   = fl_inp%note
! 		fl_out%nseg   = fl_inp%nseg
! 		fl_out%nspec  = fl_inp%nspec
! 		fl_out%idate  = fl_inp%idate
! 		fl_out%begtim = fl_inp%begtim
! 		fl_out%jdate  = fl_inp%jdate
! 		fl_out%endtim = fl_inp%endtim
! !		Header 2
! 		fl_out%orgx = fl_inp%orgx
! 		fl_out%orgy = fl_inp%orgy
! 		fl_out%iutm = fl_inp%iutm
! 		fl_out%utmx = fl_inp%utmx
! 		fl_out%utmy = fl_inp%utmy
! 		fl_out%dx   = fl_inp%dx
! 		fl_out%dy   = fl_inp%dy
! 		fl_out%nx   = fl_inp%nx
! 		fl_out%ny   = fl_inp%ny
! 		fl_out%nz   = fl_inp%nz
! 		fl_out%nzlo = fl_inp%nzlo
! 		fl_out%nzup = fl_inp%nzup
! 		fl_out%hts  = fl_inp%hts
! 		fl_out%htl  = fl_inp%htl
! 		fl_out%htu  = fl_inp%htu
! !		Header 3
! 		fl_out%i1  = fl_inp%i1
! 		fl_out%j1  = fl_inp%j1
! 		fl_out%nx1 = fl_inp%nx1
! 		fl_out%ny1 = fl_inp%ny1

! 	END SUBROUTINE

	SUBROUTINE inquire_header(fl,in_file,unit)

		TYPE(UAM_IV), INTENT(INOUT) :: fl
		CHARACTER(LEN=256), INTENT(IN), OPTIONAL :: in_file
		INTEGER, INTENT(IN), OPTIONAL :: unit

! 		Open the file
		CALL read_open_file(fl,in_file,unit)
! 		Read the header
		CALL read_header(fl,.TRUE.)
! 		Read the species names
		CALL read_species(fl,.TRUE.)
! 		Close the file
		CALL close_file(fl)

	END SUBROUTINE inquire_header

!	------------------------------------------------------------------------------------------
!	File Opening
!	------------------------------------------------------------------------------------------

	SUBROUTINE read_open_file(fl,in_file,unit)

		TYPE(UAM_IV), INTENT(INOUT) :: fl
		CHARACTER(LEN=256), INTENT(IN), OPTIONAL :: in_file
		INTEGER, INTENT(IN), OPTIONAL :: unit

!		Check for optionals
		IF (PRESENT(in_file)) THEN
			fl%in_file = in_file
		END IF
		IF (PRESENT(unit)) THEN
			fl%unit = unit
		END IF

! 		Open the files
		IF (PRESENT(unit)) THEN
			OPEN (UNIT=fl%unit,FILE=TRIM(fl%in_file),FORM='UNFORMATTED',STATUS='OLD')
		ELSE
			OPEN (NEWUNIT=fl%unit,FILE=TRIM(fl%in_file),FORM='UNFORMATTED',STATUS='OLD')
		END IF
		WRITE(*,*) 'Opened file: ', TRIM(fl%in_file)

	END SUBROUTINE read_open_file

	SUBROUTINE write_open_file(fl,in_file,unit)

		TYPE(UAM_IV), INTENT(INOUT) :: fl
		CHARACTER(LEN=256), INTENT(IN), OPTIONAL :: in_file
		INTEGER, INTENT(IN), OPTIONAL :: unit

!		Check for optionals
		IF (PRESENT(in_file)) THEN
			fl%in_file = in_file
		END IF
		IF (PRESENT(unit)) THEN
			fl%unit = unit
		END IF

! 		Open the files
		IF (PRESENT(unit)) THEN
			OPEN (UNIT=fl%unit,FILE=TRIM(fl%in_file),FORM='UNFORMATTED',STATUS='NEW')
		ELSE
			OPEN (NEWUNIT=fl%unit,FILE=TRIM(fl%in_file),FORM='UNFORMATTED',STATUS='NEW')
		END IF
		WRITE(*,*) 'Opened file: ', TRIM(fl%in_file)

	END SUBROUTINE write_open_file

	SUBROUTINE close_file(fl)

		TYPE(UAM_IV), INTENT(IN) :: fl

! 		Close the file
		CLOSE (fl%unit)
		WRITE(*,*) 'Closed file: ', TRIM(fl%in_file)

	END SUBROUTINE close_file

!	------------------------------------------------------------------------------------------
!	Headers
!	------------------------------------------------------------------------------------------

	SUBROUTINE read_header(fl,silent)

		TYPE(UAM_IV), INTENT(INOUT) :: fl
		INTEGER :: i
		CHARACTER(LEN=41) :: h1format, h2format
		LOGICAL, INTENT(IN), OPTIONAL :: silent
		LOGICAL :: l_silent

!		Check for optionals
		IF (.NOT. PRESENT(silent)) THEN
			l_silent = .FALSE.
		ELSE
			l_silent = silent
		END IF

! 		Set the format strings
		h1format='(10a1,60a1,/,i2,1x,i3,1x,i6,f6.0,i6,f6.0)'
		h2format='(2(f16.5,1x),i3,1x,4(f16.5,1x),5i4,3f7.0)'

! 		Read the first header
		READ (fl%unit) fl%fname,fl%note,fl%nseg,fl%nspec,fl%idate,fl%begtim,fl%jdate,&
			&fl%endtim
		! WRITE(*,h1format) fl%fname,fl%note,fl%nseg,fl%nspec,fl%idate,fl%begtim,fl%jdate,&
		! 	&fl%endtim
		WRITE(fl%ftype,'(10a1)') (fl%fname(i),i=1,10)
		WRITE(*,*) 'File type is ',fl%ftype

! 		Read the second header
		READ (fl%unit) fl%orgx,fl%orgy,fl%iutm,fl%utmx,fl%utmy,fl%dx,fl%dy,fl%nx,fl%ny,fl%nz,&
			&fl%nzlo,fl%nzup,fl%hts,fl%htl,fl%htu
		! WRITE(*,h2format) fl%orgx,fl%orgy,fl%iutm,fl%utmx,fl%utmy,fl%dx,fl%dy,fl%nx,fl%ny,fl%nz,&
		! 	&fl%nzlo,fl%nzup,fl%hts,fl%htl,fl%htu
		READ (fl%unit) fl%i1,fl%j1,fl%nx1,fl%ny1

!		Print the header to terminal
		IF (.NOT. l_silent) THEN
			WRITE(*,h1format) fl%fname,fl%note,fl%nseg,fl%nspec,fl%idate,fl%begtim,fl%jdate,&
				&fl%endtim
			WRITE(*,*) 'File type is ',fl%ftype
			WRITE(*,h2format) fl%orgx,fl%orgy,fl%iutm,fl%utmx,fl%utmy,fl%dx,fl%dy,fl%nx,fl%ny,fl%nz,&
				&fl%nzlo,fl%nzup,fl%hts,fl%htl,fl%htu
		END IF

	END SUBROUTINE read_header

	SUBROUTINE write_header(fl)

		TYPE(UAM_IV), INTENT(IN) :: fl
		CHARACTER(LEN=41) :: h1format, h2format

! 		Set the format strings
		h1format='(10a1,60a1,/,i2,1x,i3,1x,i6,f6.0,i6,f6.0)'
		h2format='(2(f16.5,1x),i3,1x,4(f16.5,1x),5i4,3f7.0)'

! 		Write the first header
		WRITE(fl%unit) fl%fname,fl%note,fl%nseg,fl%nspec,fl%idate,fl%begtim,fl%jdate,&
			&fl%endtim
		WRITE(*,h1format) fl%fname,fl%note,fl%nseg,fl%nspec,fl%idate,fl%begtim,fl%jdate,&
			&fl%endtim
		WRITE(*,*) 'File type is ',fl%ftype

! 		Write the second header
		WRITE(fl%unit) fl%orgx,fl%orgy,fl%iutm,fl%utmx,fl%utmy,fl%dx,fl%dy,fl%nx,fl%ny,fl%nz,&
			&fl%nzlo,fl%nzup,fl%hts,fl%htl,fl%htu
		WRITE(*,h2format) fl%orgx,fl%orgy,fl%iutm,fl%utmx,fl%utmy,fl%dx,fl%dy,fl%nx,fl%ny,fl%nz,&
			&fl%nzlo,fl%nzup,fl%hts,fl%htl,fl%htu
		WRITE(fl%unit) fl%i1,fl%j1,fl%nx1,fl%ny1

	END SUBROUTINE write_header

!	------------------------------------------------------------------------------------------

	SUBROUTINE read_species(fl,silent)

		TYPE(UAM_IV), INTENT(INOUT) :: fl
		INTEGER :: i,j
		LOGICAL, INTENT(IN), OPTIONAL :: silent
		LOGICAL :: l_silent

!		Check for optionals
		IF (.NOT. PRESENT(silent)) THEN
			l_silent = .FALSE.
		ELSE
			l_silent = silent
		END IF

! 		Allocate memory for the species arrays
		IF (ALLOCATED(fl%spname)) DEALLOCATE(fl%spname)
		ALLOCATE(fl%spname(10,fl%nspec))
		IF (ALLOCATED(fl%c_spname)) DEALLOCATE(fl%c_spname)
		ALLOCATE(fl%c_spname(fl%nspec))

! 		Read the species records
		READ (fl%unit) ((fl%spname(i,j),i=1,10),j=1,fl%nspec)
		! WRITE(fl%c_spname,'(10a1)') ((fl%spname(i,j),i=1,10),j=1,fl%nspec)
		DO i = 1, fl%nspec
			WRITE(fl%c_spname(i),'(10A1)') (fl%spname(j,i),j=1,10)
		END DO

!		Print the species list to terminal
		IF (.NOT. l_silent) THEN
			WRITE(*,*) fl%c_spname
		END IF
! 		WRITE(*,'(10a1)') ((fl%spname(i,j),i=1,10),j=1,fl%nspec)

	END SUBROUTINE read_species

	SUBROUTINE write_species(fl)

		TYPE(UAM_IV), INTENT(IN) :: fl
		INTEGER :: i,j

!		Test for variable allocation, don't write an empty variable
		IF (.NOT. ALLOCATED(fl%c_spname)) THEN
			WRITE(0,*) 'The species list was not allocated'
			CALL EXIT(1)
		END IF

! 		Write the species records
		WRITE(fl%unit) ((fl%spname(i,j),i=1,10),j=1,fl%nspec)
		WRITE(*,*) fl%c_spname
! 		WRITE(*,'(10a1)') ((fl%spname(i,j),i=1,10),j=1,fl%nspec)

	END SUBROUTINE write_species

!	------------------------------------------------------------------------------------------
!	AIRQUALITY 3D Grid
!	------------------------------------------------------------------------------------------

	SUBROUTINE read_grid_conc(fl)

		TYPE(UAM_IV), INTENT(INOUT) :: fl

		INTEGER :: i_hr, i_sp, i_nz, i_nx, i_ny
		INTEGER :: ione = 1
		CHARACTER(LEN=4) :: temp_spname(10)
		INTEGER :: j
		INTEGER :: io_status = 0
! 		Format strings
		CHARACTER(LEN=17) :: hformat

		hformat = '(5x,2(i10,f10.2))'

! 		Allocate the header arrays
		IF (ALLOCATED(fl%ibgdat) .AND. ALLOCATED(fl%iendat)) DEALLOCATE(fl%ibgdat,fl%iendat)
		ALLOCATE(fl%ibgdat(fl%update_times), fl%iendat(fl%update_times))
		IF (ALLOCATED(fl%nbgtim) .AND. ALLOCATED(fl%nentim)) DEALLOCATE(fl%nbgtim,fl%nentim)
		ALLOCATE(fl%nbgtim(fl%update_times), fl%nentim(fl%update_times))

! 		Allocate the 3D concentration array
		IF (ALLOCATED(fl%conc)) DEALLOCATE(fl%conc)
		ALLOCATE(fl%conc(fl%nx,fl%ny,fl%nz,fl%update_times,fl%nspec))

! 		Loop over hours
		DO i_hr = 1,fl%update_times ! Update times is default 24
! 			Read the section header
			READ (fl%unit,IOSTAT=io_status) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)
! 			Check for EOF
			IF (io_status .LT. 0) THEN
				fl%update_times = i_hr - 1
				EXIT
			END IF
! 			Output the section header to screen
			WRITE(*,hformat) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)

! 			Loop though species
			DO i_sp = 1, fl%nspec
! 			Ouput species names to terminal for sanity
! 			WRITE(*,*) 'Reading ', fl%c_spname(i_sp)
! 				Loop through layers
				DO i_nz = 1,fl%nz
					READ (fl%unit) ione, (temp_spname(j),j=1,10), &
					& ((fl%conc(i_nx, i_ny, i_nz, i_hr, i_sp),i_nx=1, fl%nx), i_ny=1, fl%ny)
				END DO
			END DO
		END DO

	END SUBROUTINE read_grid_conc

	SUBROUTINE write_grid_conc(fl)

		TYPE(UAM_IV), INTENT(INOUT) :: fl

		INTEGER :: i_hr, i_sp, i_nz, i_nx, i_ny
		INTEGER :: ione = 1
		INTEGER :: j
! 		Format strings
		CHARACTER(LEN=17) :: hformat

		hformat = '(5x,2(i10,f10.2))'

!		Test for variable allocation, don't write an empty variable
		IF (.NOT. ALLOCATED(fl%conc)) THEN
			WRITE(0,*) 'The concentration array was not allocated'
			CALL EXIT(1)
		END IF

! 		Loop over hours
		DO i_hr = 1,fl%update_times ! Update times is default 24
! 			Write the section header
			WRITE(fl%unit) fl%ibgdat(i_hr), fl%nbgtim(i_hr), fl%iendat(i_hr), fl%nentim(i_hr)
! 			Output the section header to screen
			WRITE(*,hformat) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)

! 			Loop though species
			DO i_sp = 1, fl%nspec
! 			Ouput species names to terminal for sanity
			WRITE(*,*) 'Writing ', fl%c_spname(i_sp)
! 				Loop through layers
				DO i_nz = 1,fl%nz
					WRITE(fl%unit) ione, (fl%spname(j,i_sp),j=1,10), &
					& ((fl%conc(i_nx, i_ny, i_nz, i_hr, i_sp),i_nx=1, fl%nx), i_ny=1, fl%ny)
				END DO
			END DO
		END DO

	END SUBROUTINE write_grid_conc

!	------------------------------------------------------------------------------------------
!	EMISSIONS 2D Grid
!	------------------------------------------------------------------------------------------

	SUBROUTINE read_grid_emis(fl)

		TYPE(UAM_IV), INTENT(INOUT) :: fl

		INTEGER :: i_hr, i_sp, i_nx, i_ny
		INTEGER :: ione = 1
		CHARACTER(LEN=4) :: temp_spname(10)
		INTEGER :: j
		INTEGER :: io_status = 0
! 		Format strings
		CHARACTER(LEN=17) :: hformat

		hformat = '(5x,2(i10,f10.2))'

! 		Allocate the header arrays
		ALLOCATE(fl%ibgdat(fl%update_times), fl%iendat(fl%update_times))
		ALLOCATE(fl%nbgtim(fl%update_times), fl%nentim(fl%update_times))

! 		Allocate the 2D concentration array
		ALLOCATE(fl%aemis(fl%nx,fl%ny,fl%update_times,fl%nspec))

! 		Loop over hours
		DO i_hr = 1,fl%update_times ! Update times is default 24
! 			Read the section header
			READ (fl%unit,IOSTAT=io_status) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)
! 			Check for EOF
			IF (io_status .LT. 0) THEN
				fl%update_times = i_hr - 1
				EXIT
			END IF
! 			Output the section header to screen
			WRITE(*,hformat) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)

! 			Loop though species
			DO i_sp = 1, fl%nspec
! 				Ouput species names to terminal for sanity
! 				WRITE(*,*) 'Reading ', fl%c_spname(i_sp)
				READ (fl%unit) ione, (temp_spname(j),j=1,10), &
				& ((fl%aemis(i_nx, i_ny, i_hr, i_sp),i_nx=1, fl%nx), i_ny=1, fl%ny)
			END DO
		END DO

	END SUBROUTINE read_grid_emis

	SUBROUTINE write_grid_emis(fl)

		TYPE(UAM_IV), INTENT(INOUT) :: fl

		INTEGER :: i_hr, i_sp, i_nx, i_ny
		INTEGER :: ione = 1
		INTEGER :: j
! 		Format strings
		CHARACTER(LEN=17) :: hformat

		hformat = '(5x,2(i10,f10.2))'

!		Test for variable allocation, don't write an empty variable
		IF (.NOT. ALLOCATED(fl%aemis)) THEN
			WRITE(0,*) 'The emissions array was not allocated'
			CALL EXIT(1)
		END IF

! 		Loop over hours
		DO i_hr = 1,fl%update_times ! Update times is default 24
! 			Read the section header
			WRITE(fl%unit) fl%ibgdat(i_hr), fl%nbgtim(i_hr), fl%iendat(i_hr), fl%nentim(i_hr)

! 			Output the section header to screen
			WRITE(*,hformat) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)

! 			Loop though species
			DO i_sp = 1, fl%nspec
! 				Ouput species names to terminal for sanity
! 				WRITE(*,*) 'Reading ', fl%c_spname(i_sp)
				WRITE(fl%unit) ione, (fl%spname(j,i_sp),j=1,10), &
				& ((fl%aemis(i_nx, i_ny, i_hr, i_sp),i_nx=1, fl%nx), i_ny=1, fl%ny)
			END DO
		END DO

	END SUBROUTINE write_grid_emis

!	------------------------------------------------------------------------------------------
!	PTSOURCE parameters and emissions
!	------------------------------------------------------------------------------------------

	SUBROUTINE read_stack_param(fl)

		TYPE(UAM_IV), INTENT(INOUT) :: fl
		INTEGER :: i_stk
		INTEGER :: ione = 1
! 		CHARACTER(LEN=20) :: stkformat

! 		Set the format strings
! 		stkformat = '(2(f16.5,1x),4e14.7)'

! 		Read the number of stacks
		READ (fl%unit) ione,fl%nstk
		WRITE(*,*) ione, fl%nstk, TRIM(fl%in_file)

! 		Allocate the stack parameter arrays
		ALLOCATE(fl%xstk(fl%nstk), fl%ystk(fl%nstk))
		ALLOCATE(fl%hstk(fl%nstk), fl%dstk(fl%nstk))
		ALLOCATE(fl%tstk(fl%nstk), fl%vstk(fl%nstk))

! 		Read the stack parameter records
		READ (fl%unit) (fl%xstk(i_stk),fl%ystk(i_stk),fl%hstk(i_stk),fl%dstk(i_stk),&
			&fl%tstk(i_stk),fl%vstk(i_stk),i_stk=1,fl%nstk)

! 		DO i_stk = 1,fl%nstk
! 			WRITE(*,stkformat) fl%xstk(i_stk),fl%ystk(i_stk),fl%hstk(i_stk),&
! 				&fl%dstk(i_stk),fl%tstk(i_stk),fl%vstk(i_stk)
! 		END DO

	END SUBROUTINE read_stack_param

	SUBROUTINE write_stack_param(fl)

		TYPE(UAM_IV), INTENT(IN) :: fl
		INTEGER :: i_stk
		INTEGER :: ione = 1
! 		CHARACTER(LEN=20) :: stkformat

! 		Set the format strings
! 		stkformat = '(2(f16.5,1x),4e14.7)'

!		Test for variable allocation, don't write an empty variable
		IF (.NOT. ALLOCATED(fl%xstk) .OR. &
			.NOT. ALLOCATED(fl%ystk) .OR. &
			.NOT. ALLOCATED(fl%hstk) .OR. &
			.NOT. ALLOCATED(fl%dstk) .OR. &
			.NOT. ALLOCATED(fl%tstk) .OR. &
			.NOT. ALLOCATED(fl%vstk) ) THEN
			WRITE(0,*) 'One or more stack parameter vector was not allocated'
			CALL EXIT(1)
		END IF

! 		Write the number of stacks
		WRITE(fl%unit) ione,fl%nstk
		WRITE(*,*) ione, fl%nstk, TRIM(fl%in_file)

! 		Write the stack parameter records
		WRITE(fl%unit) (fl%xstk(i_stk),fl%ystk(i_stk),fl%hstk(i_stk),fl%dstk(i_stk),&
			&fl%tstk(i_stk),fl%vstk(i_stk),i_stk=1,fl%nstk)

! 		DO i_stk = 1,fl%nstk
! 			WRITE(*,stkformat) fl%xstk(i_stk),fl%ystk(i_stk),fl%hstk(i_stk),&
! 				&fl%dstk(i_stk),fl%tstk(i_stk),fl%vstk(i_stk)
! 		END DO

	END SUBROUTINE write_stack_param

!	------------------------------------------------------------------------------------------

	SUBROUTINE read_stack_emis(fl)

		TYPE(UAM_IV), INTENT(INOUT) :: fl

		INTEGER :: i_hr, i_stk, i_sp
		INTEGER :: ione = 1
		CHARACTER(LEN=4) :: temp_spname(10)
		INTEGER :: j
		INTEGER :: io_status = 0
! 		Format strings
		CHARACTER(LEN=17) :: hformat

		hformat = '(5x,2(i10,f10.2))'

! 		Allocate the header arrays
		ALLOCATE(fl%ibgdat(fl%update_times), fl%iendat(fl%update_times))
		ALLOCATE(fl%nbgtim(fl%update_times), fl%nentim(fl%update_times))

! 		Allocate the stack description arrays
		ALLOCATE(fl%icell(fl%update_times,fl%nstk))
		ALLOCATE(fl%jcell(fl%update_times,fl%nstk))
		ALLOCATE(fl%kcell(fl%update_times,fl%nstk))
		ALLOCATE(fl%flow(fl%update_times,fl%nstk),fl%plmht(fl%update_times,fl%nstk))

! 		Allocate the emissions array
		ALLOCATE(fl%ptemis(fl%update_times,fl%nstk,fl%nspec))

! 		Loop over hours
		DO i_hr = 1,fl%update_times	! Update times is default 24
! 			Read the section header
			READ (fl%unit,IOSTAT=io_status) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)
! 			Check for EOF
			IF (io_status .LT. 0) THEN
				fl%update_times = i_hr - 1
				EXIT
			END IF
! 			Output the section header to screen
			WRITE(*,hformat) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)

! 			Read the stack number
			READ (fl%unit) ione, fl%nstk
! 			Read the point source descriptions
			READ (fl%unit) (fl%icell(i_hr,i_stk),fl%jcell(i_hr,i_stk),fl%kcell(i_hr,i_stk),&
				fl%flow(i_hr,i_stk),fl%plmht(i_hr,i_stk),i_stk=1,fl%nstk)

! 			Loop though species
			DO i_sp = 1, fl%nspec
				READ (fl%unit) ione, (temp_spname(j),j=1,10), (fl%ptemis(i_hr,i_stk,i_sp),&
					&i_stk=1,fl%nstk)
			END DO
		END DO

	END SUBROUTINE read_stack_emis

	SUBROUTINE write_stack_emis(fl)

		TYPE(UAM_IV), INTENT(IN) :: fl

		INTEGER :: i_hr, i_stk, i_sp
		INTEGER :: ione = 1
		INTEGER :: j
! 		Format strings
		CHARACTER(LEN=17) :: hformat

		hformat = '(5x,2(i10,f10.2))'

!		Test for variable allocation, don't write an empty variable
		IF (.NOT. ALLOCATED(fl%icell) .OR. &
			.NOT. ALLOCATED(fl%jcell) .OR. &
			.NOT. ALLOCATED(fl%kcell) .OR. &
			.NOT. ALLOCATED(fl%flow)  .OR. &
			.NOT. ALLOCATED(fl%plmht) ) THEN
			WRITE(0,*) 'One or more stack parameter vector was not allocated'
			CALL EXIT(1)
		ELSE IF (.NOT. ALLOCATED(fl%ptemis)) THEN
			WRITE(0,*) 'The stack emissions array was not allocated'
			CALL EXIT(1)
		END IF

! 		Loop over hours
		DO i_hr = 1,fl%update_times	! Update times is default 24
! 			Write the section header
			WRITE(fl%unit) fl%ibgdat(i_hr), fl%nbgtim(i_hr), fl%iendat(i_hr), fl%nentim(i_hr)
! 			Output the section header to screen
			WRITE(*,hformat) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)

! 			Write the stack number
			WRITE(fl%unit) ione, fl%nstk
! 			Write the point source descriptions
			WRITE(fl%unit) (fl%icell(i_hr,i_stk),fl%jcell(i_hr,i_stk),fl%kcell(i_hr,i_stk),&
				&fl%flow(i_hr,i_stk),fl%plmht(i_hr,i_stk),i_stk=1,fl%nstk)

! 			Loop though species
			DO i_sp = 1, fl%nspec
				WRITE(fl%unit) ione, (fl%spname(j,i_sp),j=1,10), (fl%ptemis(i_hr,i_stk,i_sp),&
					&i_stk=1,fl%nstk)
			END DO
		END DO

	END SUBROUTINE write_stack_emis

!	------------------------------------------------------------------------------------------
!	BOUNDARY definitions and concentrations
!	------------------------------------------------------------------------------------------

	SUBROUTINE read_bound_param(fl)

		TYPE(UAM_IV), INTENT(INOUT) :: fl

		INTEGER :: i_bd, ii_bd, i_nd
		INTEGER :: ione = 1

		DO i_bd = 1, 4

! 			Read the cell numbers
			READ (fl%unit) ione, fl%bc(i_bd)%iedge, fl%bc(i_bd)%ncell

! 			Debug output
! 			WRITE(*,*) fl%bc(i_bd)%iedge, fl%bc(i_bd)%ncell

! 			Rewind the file one record
			BACKSPACE(fl%unit)

! 			Allocate the location array
			ALLOCATE(fl%bc(i_bd)%iloc(4,fl%bc(i_bd)%ncell))

! 			Re-read the record, this time reading the location array
			READ (fl%unit) ione, ione, ione, &
				& ((fl%bc(i_bd)%iloc(ii_bd,i_nd),ii_bd=1,4),i_nd=1,fl%bc(i_bd)%ncell)

! 			Debug output
! 			WRITE(*,*) fl%bc(i_bd)%iloc

		END DO

	END SUBROUTINE read_bound_param

	SUBROUTINE write_bound_param(fl)

		TYPE(UAM_IV), INTENT(IN) :: fl

		INTEGER :: i_bd, ii_bd, i_nd
		INTEGER :: ione = 1

! 		Loop through boundaries to write iloc
		DO i_bd = 1,4

! 			Write the boundary parameters and the location array
			WRITE(fl%unit) ione, fl%bc(i_bd)%iedge, fl%bc(i_bd)%ncell, &
				& ((fl%bc(i_bd)%iloc(ii_bd,i_nd),ii_bd=1,4),i_nd=1,fl%bc(i_bd)%ncell)

		END DO

	END SUBROUTINE write_bound_param

!	------------------------------------------------------------------------------------------

	SUBROUTINE read_bound_conc(fl)

		TYPE(UAM_IV), INTENT(INOUT) :: fl

		INTEGER :: i_hr, i_sp, i_bd, i_nz, i_nd
		INTEGER :: ione =1
		CHARACTER(LEN=4) :: temp_spname(10)
		INTEGER :: temp_iedge
		INTEGER :: j
		INTEGER :: io_status = 0
! 		Format strings
		CHARACTER(LEN=17) :: hformat

		hformat = '(5x,2(i10,f10.2))'

! 		Allocate the header arrays
		ALLOCATE(fl%ibgdat(fl%update_times), fl%iendat(fl%update_times))
		ALLOCATE(fl%nbgtim(fl%update_times), fl%nentim(fl%update_times))

! 		Allocate the boundary concentration array
		DO i_bd = 1,4
			ALLOCATE(fl%bc(i_bd)%bc_conc(fl%bc(i_bd)%ncell,fl%nz,fl%update_times,fl%nspec))
		END DO

! 		Hour loop
		DO i_hr = 1,fl%update_times
! 			Read the section header
			READ (fl%unit,IOSTAT=io_status) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)
! 			Check for EOF
			IF (io_status .LT. 0) THEN
				fl%update_times = i_hr - 1
				EXIT
			END IF
! 			Output the section header to screen
			WRITE(*,hformat) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)

! 			Loop though species
			DO i_sp = 1,fl%nspec
! 			Ouput species names to terminal for sanity
! 			WRITE(*,*) 'Reading ', fl%c_spname(i_sp)
! 				Boundary edge loop
				DO i_bd = 1, 4
! 					Read the boundary concentrations
					READ (fl%unit) ione, (temp_spname(j),j=1,10), temp_iedge,&
						&((fl%bc(i_bd)%bc_conc(i_nd,i_nz,i_hr,i_sp),&
						&i_nz=1,fl%nz),i_nd=1,fl%bc(i_bd)%ncell)
				END DO
			END DO
		END DO

	END SUBROUTINE read_bound_conc

	SUBROUTINE write_bound_conc(fl)

		TYPE(UAM_IV), INTENT(IN) :: fl

		INTEGER :: i_hr, i_sp, i_bd, i_nz, i_nd
		INTEGER :: ione =1
		INTEGER :: j
! 		Format strings
		CHARACTER(LEN=17) :: hformat

		hformat = '(5x,2(i10,f10.2))'

! 		Hour loop
		DO i_hr = 1,fl%update_times
! 			Write the section header
			WRITE(fl%unit) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)
! 			Output the section header to screen
			WRITE(*,hformat) fl%ibgdat(i_hr), fl%nbgtim(i_hr),&
				&fl%iendat(i_hr), fl%nentim(i_hr)

! 			Loop though species
			DO i_sp = 1,fl%nspec
! 			Ouput species names to terminal for sanity
! 			WRITE(*,*) 'Reading ', fl%c_spname(i_sp)
! 				Boundary edge loop
				DO i_bd = 1, 4
! 					Write the boundary concentrations
					WRITE(fl%unit) ione, (fl%spname(j,i_sp),j=1,10), i_bd,&
						&((fl%bc(i_bd)%bc_conc(i_nd,i_nz,i_hr,i_sp),&
						&i_nz=1,fl%nz),i_nd=1,fl%bc(i_bd)%ncell)
				END DO
			END DO
		END DO

	END SUBROUTINE write_bound_conc

END MODULE
