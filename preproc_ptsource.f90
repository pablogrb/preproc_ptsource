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
	! ------------------------------------------------------------------------------------------

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


	! Argument control
	INTEGER :: arg_num
	CHARACTER(LEN=2) :: arg_switch
	LOGICAL :: file_exists

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
		WRITE(0,'(2A)') 'Using the control file ', TRIM(ctrlfile)
	ELSE
		WRITE(0,'(3A)') 'Control file ', TRIM(ctrlfile), ' does not exist'
		CALL EXIT(0)
	END IF

	! Read the namelist
	OPEN(NEWUNIT=nml_unit, FILE=ctrlfile, FORM='FORMATTED', STATUS='OLD', ACTION='READ')
	READ(nml_unit,NML=file_io)
	READ(nml_unit,NML=projection)
	CLOSE(nml_unit)

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
	WRITE(0,'(A,I3)') 'Number of point sources: ', fl_out%nstk
	WRITE(0,'(A,I3)') 'Number species: ', fl_out%nspec

END PROGRAM preproc_ptsource