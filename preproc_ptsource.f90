PROGRAM preproc_ptsource

USE class_UAM_IV
IMPLICIT NONE

	! ------------------------------------------------------------------------------------------
	! Purpose:
	! 	Converts the point source parameters and emissions from the UPB - GIA inventory in CSV
	! 	format to the CAM 6.20 forma
	! Inputs
	! 	Point source parameter file
	! 	Point source emissions file
	! Outputs
	! 	Point source emissions file in CAMx 6.20 forma
	! By:
	! 	Pablo Garcia
	! 	pablogrb@gmail.com
	! 	UPB - GIA
	! 	2020-03
	! This program requires a F08 compatible compiler
	! ------------------------------------------------------------------------------------------
	! Error coodes
	!	0 = File IO
	!	1 = Memory allocation

	! ------------------------------------------------------------------------------------------
	! Declarations

	! IO
	CHARACTER(LEN=256) :: ptsource_param			! Point source parameter file
	INTEGER :: param_unit
	CHARACTER(LEN=256) :: ptsource_emiss			! Point source emission file
	INTEGER :: emiss_unit
	CHARACTER(LEN=256) :: ptsource_out				! CAMx emissions file

	! UAM IV output file
	TYPE(UAM_IV) :: fl_out							! CAMx format ptsource output file

	! CAMx Control
	CHARACTER(LEN=60) :: Run_Message				! Run message (also known as note)
	INTEGER :: Time_Zone							! Time Zone (5 = EST)
	INTEGER :: emiss_date							! Date for the emissions file
	INTEGER :: frames = 1							! Number of data frames (24, one for each hour)
	REAL :: dt = 24.								! Duration of each data frame (1., one hour)
	INTEGER :: nstk									! Number of stacks
	INTEGER :: nspec								! Number of inventory species

	! Projection parameters
	CHARACTER(LEN=10) :: Map_Projection				! (LAMBERT,POLAR)
	INTEGER :: UTM_Zone
	REAL :: POLAR_Longitude_Pole					! deg (west<0,south<0)
	REAL :: POLAR_Latitude_Pole						! deg (west<0,south<0)
	REAL :: LAMBERT_Center_Longitude				! deg (west<0,south<0)
	REAL :: LAMBERT_Center_Latitude					! deg (west<0,south<0)
	REAL :: LAMBERT_True_Latitude1					! deg (west<0,south<0)
	REAL :: LAMBERT_True_Latitude2					! deg (west<0,south<0, can be same as
													!      LAMBERT_True_Latitude1)

	! Grid parameters
	REAL :: Master_Origin_XCoord					! km or deg, SW corner of cell (1,1)
	REAL :: Master_Origin_YCoord					! km or deg, SW corner of cell (1,1)
	REAL :: Master_Cell_XSize						! km or deg
	REAL :: Master_Cell_YSize						! km or deg
	INTEGER :: Master_Grid_Columns					! grid cells in E-W direction
	INTEGER :: Master_Grid_Rows						! grid cells in N-S direction

	! ptsource parameter vectors
	INTEGER, ALLOCATABLE :: camx_id_par(:)			! ptsource id code from the inventory
	REAL, ALLOCATABLE :: pt_lat(:), pt_lon(:)		! latitude and longitude of each source

	! emiss data
	INTEGER, ALLOCATABLE :: camx_id_emi(:)			! ptsource id code from the inventory
	INTEGER, ALLOCATABLE :: emi_hr(:)					! Hour for the emission, must match the i_hr index

	! Control
	INTEGER :: arg_num
	CHARACTER(LEN=2) :: arg_switch
	LOGICAL :: file_exists
	INTEGER :: alloc_stat
	INTEGER :: io_stat
	INTEGER :: i, i_stk, i_nsp, i_dfr
	CHARACTER(LEN=10) :: str_dummy

	! Namelist IO
	CHARACTER(LEN=256) :: ctrlfile					! Control namelist
	INTEGER :: nml_unit								! Control file unit
	NAMELIST /CAMx_Control/ Run_Message, Time_Zone, emiss_date, nstk, nspec
	NAMELIST /file_io/ ptsource_param, ptsource_emiss, ptsource_out
	NAMELIST /projection/ Map_Projection, UTM_Zone, POLAR_Longitude_Pole, POLAR_Latitude_Pole, &
						& LAMBERT_Center_Longitude, LAMBERT_Center_Latitude, &
						& LAMBERT_True_Latitude1, LAMBERT_True_Latitude2
	NAMELIST /grid/ Master_Origin_XCoord, Master_Origin_YCoord, Master_Cell_XSize, Master_Cell_YSize, &
						& Master_Grid_Columns, Master_Grid_Rows

	! ------------------------------------------------------------------------------------------
	! Entry point
	! ------------------------------------------------------------------------------------------

	! Command line argument capture
	arg_num = COMMAND_ARGUMENT_COUNT()
	IF (arg_num .EQ. 0) THEN
		ctrlfile = 'preproc_ptsource.nml'
	ELSEIF (arg_num .NE. 2) THEN
		WRITE(0,'(A)') 'Bad argument number'
		CALL EXIT(0)
	ELSE
	! 	Capture the argument type
		CALL GET_COMMAND_ARGUMENT(1,arg_switch)
		IF (arg_switch .NE. '-f') THEN
			WRITE(0,'(A)') 'Bad argument type'
			CALL EXIT(0)
		ELSE
			CALL GET_COMMAND_ARGUMENT(2,ctrlfile)
		END IF
	END IF
	! Check if the file exists
	INQUIRE(FILE=TRIM(ctrlfile), EXIST=file_exists)
	IF (file_exists) THEN
		WRITE(6,'(2A)') 'Using the control file ', TRIM(ctrlfile)
	ELSE
		WRITE(0,'(3A)') 'Control file ', TRIM(ctrlfile), ' does not exist'
		CALL EXIT(0)
	END IF

	! Read the namelist
	OPEN(NEWUNIT=nml_unit, FILE=ctrlfile, FORM='FORMATTED', STATUS='OLD', ACTION='READ')
	READ(nml_unit,NML=CAMx_Control)
	READ(nml_unit,NML=file_io)
	READ(nml_unit,NML=projection)
	READ(nml_unit,NML=grid)
	CLOSE(nml_unit)

	! ------------------------------------------------------------------------------------------
	! Build the first file header
	fl_out%ftype = "PTSOURCE"
	DO i = 1, 10
		fl_out%fname(i) = fl_out%ftype(i:i)
	END DO
	DO i = 1, 60
		fl_out%note(i) = Run_Message(i:i)
	END DO
	fl_out%nseg 	= Time_Zone
	fl_out%nspec	= nspec
	fl_out%idate 	= emiss_date
	fl_out%begtim 	= 0.
	fl_out%jdate 	= emiss_date
	fl_out%endtim 	= 24.
	! Build the second file header
	SELECT CASE (Map_Projection)
	CASE ('LAMBERT')
		fl_out%nzlo = 2
		fl_out%orgx = LAMBERT_Center_Longitude
		fl_out%orgy = LAMBERT_Center_Latitude
	CASE ('POLAR')
		fl_out%nzlo = 3
		fl_out%orgx = POLAR_Longitude_Pole
		fl_out%orgy = POLAR_Latitude_Pole
	CASE DEFAULT
		WRITE(0,'(A)') 'Not a valid projection type'
		CALL EXIT(0)
	END SELECT
	fl_out%iutm = 0
	fl_out%utmx = Master_Origin_XCoord
	fl_out%utmy = Master_Origin_YCoord
	fl_out%dx 	= Master_Cell_XSize*1000.
	fl_out%dy 	= Master_Cell_YSize*1000.
	fl_out%nx 	= Master_Grid_Columns
	fl_out%ny 	= Master_Grid_Rows
	fl_out%nz 	= 1
	fl_out%nzup = 0
	fl_out%hts 	= LAMBERT_True_Latitude1
	fl_out%htl 	= LAMBERT_True_Latitude2
	fl_out%htu 	= 1
	! Build the third header
	fl_out%i1 	= 1
	fl_out%j1 	= 1
	fl_out%nx1 	= Master_Grid_Columns
	fl_out%ny1 	= Master_Grid_Rows
	! Set the stack number
	fl_out%nstk = nstk

	! ------------------------------------------------------------------------------------------
	! Check if the ptsource parameter file exists
	INQUIRE(FILE=TRIM(ptsource_param), EXIST=file_exists)
	IF (.NOT. file_exists) THEN
		WRITE(0,'(A)') 'Point source parameter file ', TRIM(ptsource_param), ' does not exist'
		CALL EXIT(0)
	END IF

	! Read the ptsource parameter file
	OPEN(NEWUNIT=param_unit, FILE=TRIM(ptsource_param),STATUS='OLD')
	! Skip column headers
	READ(param_unit,*)

	! Allocate the parameter vectors
	ALLOCATE(camx_id_par(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)
	ALLOCATE(pt_lat(fl_out%nstk), pt_lon(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)
	ALLOCATE(fl_out%xstk(fl_out%nstk), fl_out%ystk(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)
	ALLOCATE(fl_out%hstk(fl_out%nstk), fl_out%dstk(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)
	ALLOCATE(fl_out%tstk(fl_out%nstk), fl_out%vstk(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)

	! Read the parameter file while converting the lat lon to projection coordinates
	DO i_stk = 1, fl_out%nstk

		READ(param_unit,*,IOSTAT=io_stat) camx_id_par(i_stk), pt_lat(i_stk), pt_lon(i_stk),&
						& fl_out%hstk(i_stk), fl_out%dstk(i_stk),&
						& fl_out%tstk(i_stk), fl_out%vstk(i_stk)
		IF ( io_stat > 0 ) THEN
			WRITE(0,'(A)') 'Error reading stack parameter file'
			CALL EXIT(0)
		ELSE IF ( io_stat < 0 ) THEN
			WRITE(0,'(A,I3,A,I3)') 'Unexpected end of parameter file, expected ', fl_out%nstk,&
								&  ' point sources, failed while reading no. ', i_stk
			CALL EXIT(0)
		END IF

		! Switch by projection type
		SELECT CASE (Map_Projection)
		CASE ('LAMBERT')
			CALL lcpgeo(0,LAMBERT_Center_Latitude,LAMBERT_Center_Longitude,LAMBERT_True_Latitude1, LAMBERT_True_Latitude2,&
					&	fl_out%xstk(i_stk),fl_out%ystk(i_stk),pt_lon(i_stk),pt_lat(i_stk))
		CASE ('POLAR')
			CALL pspgeo(0,POLAR_Longitude_Pole,POLAR_Latitude_Pole,fl_out%xstk(i_stk),fl_out%ystk(i_stk),pt_lon(i_stk),pt_lat(i_stk))
		CASE DEFAULT
			WRITE(0,'(A)') 'Not a valid projection type'
			CALL EXIT(0)
		END SELECT

	END DO

	! ------------------------------------------------------------------------------------------
	! Check if the ptsource emissions file exists
	INQUIRE(FILE=TRIM(ptsource_emiss), EXIST=file_exists)
	IF (.NOT. file_exists) THEN
		WRITE(0,'(A)') 'Point source emissions file ', TRIM(ptsource_param), ' does not exist'
		CALL EXIT(0)
	END IF

	! Read the ptsource parameter file
	OPEN(NEWUNIT=emiss_unit, FILE=TRIM(ptsource_emiss),STATUS='OLD')
	! Allocate the species names vectors
	ALLOCATE(fl_out%c_spname(fl_out%nspec), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)
	ALLOCATE(fl_out%spname(10,fl_out%nspec), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)
	! Read the the species list
	READ(emiss_unit,*) str_dummy, str_dummy, (fl_out%c_spname(i_nsp), i_nsp = 1, fl_out%nspec)
	! Diagnostic output of species list
	! WRITE(*,*) (fl_out%c_spname(i_nsp),i_nsp=1,fl_out%nspec)
	! Write to the species array
	DO i_nsp = 1, fl_out%nspec
		DO i = 1,10
			! This produces a compiler warning -Wcharacter-truncation, but runs fine
			fl_out%spname(i,i_nsp) = fl_out%c_spname(i_nsp)(i:i)
		END DO
	END DO
	! Diagnostic output of species list
	! WRITE(*,'(10A1)') ((fl_out%spname(i,i_nsp),i=1,10),i_nsp=1,fl_out%nspec)

	! Allocate the emiss vectors
	ALLOCATE(camx_id_emi(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)
	ALLOCATE(emi_hr(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)

	! Allocate the time variant headers
	fl_out%update_times = frames
	ALLOCATE(fl_out%ibgdat(frames), fl_out%iendat(frames), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)
	fl_out%ibgdat = emiss_date
	fl_out%iendat = emiss_date
	ALLOCATE(fl_out%nbgtim(frames), fl_out%nentim(frames), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)

	! Allocate the stack emissions arrays
	ALLOCATE(fl_out%icell(frames,nstk),fl_out%jcell(frames,nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)
	ALLOCATE(fl_out%kcell(frames,nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)
	ALLOCATE(fl_out%flow(frames,nstk),fl_out%plmht(frames,nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)
	ALLOCATE(fl_out%ptemis(frames,nstk,nspec), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat)

	! Set values for the empty/control stack emission arrays
	fl_out%icell = 0
	fl_out%jcell = 0
	fl_out%kcell = 0
	fl_out%flow  = 0.
	fl_out%plmht = 0.

	! Read each data frame
	DO i_dfr = 1, frames
		! Sanity output
		WRITE(6,'(A,I2)') 'Working on hour ', i_dfr

		! Write the time variant header
		fl_out%nbgtim = 0. + dt*REAL(i_dfr - 1)
		fl_out%nentim = 0. + dt*REAL(i_dfr)

		! Read each stack record
		DO i_stk = 1, fl_out%nstk
			
			READ(emiss_unit,*,IOSTAT=io_stat) camx_id_emi(i_stk), emi_hr(i_stk),&
											& (fl_out%ptemis(i_dfr, i_stk, i_nsp), i_nsp = 1, fl_out%nspec)

			IF ( io_stat > 0 ) THEN
				WRITE(0,'(A)') 'Error reading stack emissions file'
				CALL EXIT(0)
			ELSE IF ( io_stat < 0 ) THEN
				WRITE(0,'(A,I3,A,I2)') 'Unexpected end of emissions file while trying to read stack ', i_stk, ' at hour ', i_dfr
				CALL EXIT(0)
			END IF

		END DO

		! Check for param vector consistency
		IF ( .NOT. ALL(camx_id_par .EQ. camx_id_emi)) THEN
			WRITE(0,'(A)') "Stack list inconsistency"
			WRITE(0,'(A)') "Each data frame must have the same stak list as the stack parameter file"
			WRITE(0,'(A,I2)') "Failed while reading data frame ", i_dfr
			CALL EXIT(2)
		END IF
		IF ( .NOT. ALL(emi_hr .EQ. i_dfr)) THEN
			WRITE(0,'(A)') "Inconsistent hour information"
			WRITE(0,'(A)') "All stacks whithin a data frame must have the same hour information"
			WRITE(0,'(A,I2)') "Failed while reading data frame ", i_dfr
		END IF

	END DO

	! ------------------------------------------------------------------------------------------
	! Check if the output file exists
	INQUIRE(FILE=TRIM(ptsource_out), EXIST=file_exists)
	IF ( file_exists ) THEN
		WRITE(0,'(A)') 'Output point source file ', TRIM(ptsource_out), ' exists. Will not overwrite'
		CALL EXIT(0)
	END IF

	! Write the output file
	CALL write_uamfile(fl_out, ptsource_out)

END PROGRAM preproc_ptsource

!	------------------------------------------------------------------------------------------
!	Subroutines and functions
!	------------------------------------------------------------------------------------------

SUBROUTINE check_alloc_stat(alloc_stat)

	INTEGER, INTENT(IN) :: alloc_stat

	IF ( alloc_stat .NE. 0 ) THEN
		WRITE(0,'(A)') 'Error allocating parameter vectors, check available memory'
		CALL EXIT(1)
	END IF

END SUBROUTINE check_alloc_stat
