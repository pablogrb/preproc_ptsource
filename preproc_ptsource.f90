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
	!	0 = IO Error

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

	! ptsource parameter vectors
	INTEGER, ALLOCATABLE :: camx_id(:)				! ptsource id code from the inventory
	REAL, ALLOCATABLE :: pt_lat(:), pt_lon(:)		! latitude and longitude of each source
	! REAL, ALLOCATABLE :: xstk(:), ystk(:)			! Stack location
	! REAL, ALLOCATABLE :: hstk(:), dstk(:)			! Stack height and diameter
	! REAL, ALLOCATABLE :: tstk(:), vstk(:)			! Stack temperature and velocity

	! Control
	INTEGER :: arg_num
	CHARACTER(LEN=2) :: arg_switch
	LOGICAL :: file_exists
	INTEGER :: alloc_stat
	INTEGER :: io_stat
	INTEGER :: i_stk

	! Namelist IO
	CHARACTER(LEN=256) :: ctrlfile					! Control namelist
	INTEGER :: nml_unit								! Control file unit
	NAMELIST /file_io/ ptsource_param, ptsource_emiss, ptsource_out
	NAMELIST /projection/ Map_Projection, UTM_Zone, POLAR_Longitude_Pole, POLAR_Latitude_Pole, &
						& LAMBERT_Center_Longitude, LAMBERT_Center_Latitude, &
						& LAMBERT_True_Latitude1, LAMBERT_True_Latitude2

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
		WRITE(6,'(3A)') 'Control file ', TRIM(ctrlfile), ' does not exist'
		CALL EXIT(0)
	END IF

	! Read the namelist
	OPEN(NEWUNIT=nml_unit, FILE=ctrlfile, FORM='FORMATTED', STATUS='OLD', ACTION='READ')
	READ(nml_unit,NML=file_io)
	READ(nml_unit,NML=projection)
	CLOSE(nml_unit)

	! ------------------------------------------------------------------------------------------
	! Check if the ptsource parameter file exists
	INQUIRE(FILE=TRIM(ptsource_param), EXIST=file_exists)
	IF (.NOT. file_exists) THEN
		WRITE(0,'(A)') 'Point source parameter file ', TRIM(ptsource_param), ' does not exist'
		CALL EXIT(0)
	END IF

	! Read the ptsource parameter file
	OPEN(NEWUNIT=param_unit, FILE=TRIM(ptsource_param),STATUS='OLD')
	! Read the number of point sources
	READ(param_unit,*) fl_out%nstk, fl_out%nspec
	WRITE(6,'(A,I3)') 'Number of point sources: ', fl_out%nstk
	WRITE(6,'(A,I3)') 'Number species: ', fl_out%nspec
	! Skip column headers
	READ(param_unit,*)

	! Allocate the parameter vectors
	ALLOCATE(camx_id(fl_out%nstk), STAT=alloc_stat)
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
		READ(param_unit,*,IOSTAT=io_stat) camx_id(i_stk), pt_lat(i_stk), pt_lon(i_stk),&
						& fl_out%hstk(i_stk), fl_out%dstk(i_stk),&
						& fl_out%tstk(i_stk), fl_out%vstk(i_stk)
		IF ( io_stat > 0 ) THEN
			WRITE(0,'(A)') 'Error reading stack parameter file'
			CALL EXIT(0)
		ELSE IF ( io_stat < 0 ) THEN
			WRITE(0,'(A,I3,A,I3)') 'Unexpected end of file, expected ', fl_out%nstk, ' point sources, failed while reading no. ', i_stk
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
		END SELECT
	END DO

	! ------------------------------------------------------------------------------------------
	! Check if the ptsource emissions file exists
	INQUIRE(FILE=TRIM(ptsource_emiss), EXIST=file_exists)
	IF (.NOT. file_exists) THEN
		WRITE(0,'(A)') 'Point source emissions file ', TRIM(ptsource_param), ' does not exist'
		CALL EXIT(0)
	END IF

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
