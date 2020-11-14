PROGRAM preproc_ptsource

USE class_UAM_IV
IMPLICIT NONE

	! ------------------------------------------------------------------------------------------
	! Purpose:
	! 	Converts the point source parameters and emissions from the UPB - GIA inventory in CSV
	! 	format to the CAM 6.20 format
	! Inputs
	! 	Point source parameter file
	! 	Point source emissions file
	! Outputs
	! 	Point source emissions file in CAMx 6.20 format
	! By:
	! 	Pablo Garcia
	! 	pablogrb@gmail.com
	! 	UPB - GIA
	! 	2020-03
	! This program requires a F08 compatible compiler
	! ------------------------------------------------------------------------------------------
	! Error codes
	!	0 = File IO
	!	1 = Memory allocation

	! ------------------------------------------------------------------------------------------
	! Declarations

	! IO
	CHARACTER(LEN=256) :: ptsource_param			! Point source parameter file
	INTEGER :: param_unit
	CHARACTER(LEN=256) :: ptsource_emis			! Point source emision file
	INTEGER :: emis_unit
	CHARACTER(LEN=256) :: conv_f_matrix				! Conversion matrix for scaling, mw, and mech
	INTEGER :: conv_f_unit
	CHARACTER(LEN=256) :: ptsource_out				! CAMx emissions file

	! UAM IV output file
	TYPE(UAM_IV) :: fl_out							! CAMx format ptsource output file

	! CAMx Control
	CHARACTER(LEN=60) :: Run_Message				! Run message (also known as note)
	INTEGER :: Time_Zone							! Time Zone (5 = EST)
	INTEGER :: emis_date							! Date for the emissions file
	INTEGER :: frames = 24							! Number of data frames (24, one for each hour)
	REAL :: dt = 1.									! Duration of each data frame (1., one hour)
	INTEGER :: nstk									! Number of stacks
	INTEGER :: i_nspec								! Number of inventory species

	REAL :: conv_fact								! Unit conversion factor, emissions must be in g/h
													! E.g use 1000 to convert an inventory in kg/h to g/h

	! Conversion matrix
	INTEGER :: nspec								! Number of output species
	CHARACTER(LEN=16), ALLOCATABLE :: s_inp_spec(:)	! Species array of the inventory file
	CHARACTER(LEN=16), ALLOCATABLE :: s_mat_spec(:)	! Species array of the conv matrix file
	CHARACTER(LEN=10), ALLOCATABLE :: s_out_spec(:)	! Species array of the output
	REAL, ALLOCATABLE :: scale_factors(:)			! Scale factor vector
	REAL, ALLOCATABLE :: molecular_weights(:)		! Vector of molecular weights
	REAL, ALLOCATABLE :: conv_matrix(:,:)			! Linear transformation matrix for species

	REAL, ALLOCATABLE :: v_inp_emis(:)				! Emmision rate vector as read from emision file (size i_nspec)
	REAL, ALLOCATABLE :: v_out_emis(:)				! emision rate vector after linear transform (size nspec)

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

	! emis data
	INTEGER, ALLOCATABLE :: camx_id_emi(:)			! ptsource id code from the inventory
	INTEGER, ALLOCATABLE :: emi_hr(:)				! Hour for the emision, must match the i_hr index

	! Control
	INTEGER :: arg_num
	CHARACTER(LEN=2) :: arg_switch
	LOGICAL :: file_exists
	INTEGER :: alloc_stat
	INTEGER :: io_stat
	INTEGER :: i, i_stk, i_nsp, i_dfr, i_spi, i_spo
	CHARACTER(LEN=10) :: str_dummy
	CHARACTER(LEN=4)  :: str_refmt

	! Namelist IO
	CHARACTER(LEN=256) :: ctrlfile					! Control namelist
	INTEGER :: nml_unit								! Control file unit
	NAMELIST /CAMx_Control/ Run_Message, Time_Zone, emis_date, nstk, i_nspec, conv_fact
	NAMELIST /file_io/ ptsource_param, ptsource_emis, conv_f_matrix, ptsource_out
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
	! Read the conversion matrix
	! Check if the conversion matrix file exists
	INQUIRE(FILE=TRIM(conv_f_matrix), EXIST=file_exists)
	IF (.NOT. file_exists) THEN
		WRITE(0,'(A)') 'Conversion matrix file ', TRIM(conv_f_matrix), ' does not exist'
		CALL EXIT(0)
	END IF

	! Read the conversion matrix file
	OPEN(NEWUNIT=conv_f_unit, FILE=TRIM(conv_f_matrix),STATUS='OLD')
	! Read the number of output species
	READ(conv_f_unit,*,IOSTAT=io_stat) str_dummy, nspec
	CALL check_io_stat(io_stat, 'number of species', 'conversion matrix')
	! WRITE(*,'(I3)') nspec

	! Allocate the species vectors
	ALLOCATE(s_inp_spec(i_nspec), s_mat_spec(i_nspec), s_out_spec(nspec), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'species vectors')

	! Read the output species list
	READ(conv_f_unit,*,IOSTAT=io_stat) str_dummy, str_dummy, (s_out_spec(i_spo), i_spo=1,nspec)
	CALL check_io_stat(io_stat, 'species list', 'conversion matrix')
	! WRITE(*,*) s_out_spec

	! Read the scale factor vector
	ALLOCATE(scale_factors(nspec), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'scale factor vectors')
	READ(conv_f_unit,*,IOSTAT=io_stat) str_dummy, str_dummy, (scale_factors(i_spo), i_spo=1,nspec)
	CALL check_io_stat(io_stat, 'scale factor', 'conversion matrix')
	! WRITE(*,*) scale_factors

	! Read the species names, molecular weights and linear transformation matrix
	! Allocate memory to molecular weights and lintrans matrix
	ALLOCATE(molecular_weights(i_nspec), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'molecular weights')
	ALLOCATE(conv_matrix(i_nspec,nspec), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'conversion matrix')

	DO i_spi = 1, i_nspec
		READ(conv_f_unit,*,IOSTAT=io_stat) s_mat_spec(i_spi), molecular_weights(i_spi), (conv_matrix(i_spi,i_spo), i_spo=1,nspec)
		CALL check_io_stat(io_stat, 'conversion matrix', 'conversion matrix')
	END DO

	! Close the conversion matrix file
	CLOSE(conv_f_unit)
	
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
	fl_out%idate 	= emis_date
	fl_out%begtim 	= 0.
	fl_out%jdate 	= emis_date
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
	fl_out%utmx = Master_Origin_XCoord*1000.
	fl_out%utmy = Master_Origin_YCoord*1000.
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
	CALL check_alloc_stat(alloc_stat, 'stack codes')
	ALLOCATE(pt_lat(fl_out%nstk), pt_lon(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat,'stack lat and lon')
	ALLOCATE(fl_out%xstk(fl_out%nstk), fl_out%ystk(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'stack projection coordinates')
	ALLOCATE(fl_out%hstk(fl_out%nstk), fl_out%dstk(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'stack heights and diameters')
	ALLOCATE(fl_out%tstk(fl_out%nstk), fl_out%vstk(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'stack temperatures and flows')

	! Read the parameter file while converting the lat lon to projection coordinates
	DO i_stk = 1, fl_out%nstk

		READ(param_unit,*,IOSTAT=io_stat) camx_id_par(i_stk), pt_lat(i_stk), pt_lon(i_stk),&
						& fl_out%hstk(i_stk), fl_out%dstk(i_stk),&
						& fl_out%tstk(i_stk), fl_out%vstk(i_stk)
		CALL check_io_stat(io_stat, 'stack parameters', 'stack parameter')
		! IF ( io_stat > 0 ) THEN
		! 	WRITE(0,'(A)') 'Error reading stack parameter file'
		! 	CALL EXIT(0)
		! ELSE IF ( io_stat < 0 ) THEN
		! 	WRITE(0,'(A,I3,A,I3)') 'Unexpected end of parameter file, expected ', fl_out%nstk,&
		! 						&  ' point sources, failed while reading no. ', i_stk
		! 	CALL EXIT(0)
		! END IF

		! Switch by projection type
		SELECT CASE (Map_Projection)
		CASE ('LAMBERT')
			! Convert Lat-Lon to LAMBERT
			CALL lcpgeo(0,LAMBERT_Center_Latitude,LAMBERT_Center_Longitude,LAMBERT_True_Latitude1, LAMBERT_True_Latitude2,&
					&	fl_out%xstk(i_stk),fl_out%ystk(i_stk),pt_lon(i_stk),pt_lat(i_stk))
		CASE ('POLAR')
			! Convert Lat-Lon to POLAR
			CALL pspgeo(0,POLAR_Longitude_Pole,POLAR_Latitude_Pole,fl_out%xstk(i_stk),fl_out%ystk(i_stk),pt_lon(i_stk),pt_lat(i_stk))
		CASE DEFAULT
			WRITE(0,'(A)') 'Not a valid projection type'
			CALL EXIT(0)
		END SELECT
		! Geodetic routines ouputs projection coordinates in km, CAMx requires meters
		fl_out%xstk(i_stk) = fl_out%xstk(i_stk)*1000
		fl_out%ystk(i_stk) = fl_out%ystk(i_stk)*1000

	END DO
	! Close the stack parameter file
	CLOSE(param_unit)

	! ------------------------------------------------------------------------------------------
	! Check if the ptsource emissions file exists
	INQUIRE(FILE=TRIM(ptsource_emis), EXIST=file_exists)
	IF (.NOT. file_exists) THEN
		WRITE(0,'(A)') 'Point source emissions file ', TRIM(ptsource_param), ' does not exist'
		CALL EXIT(0)
	END IF

	! Read the ptsource parameter file
	OPEN(NEWUNIT=emis_unit, FILE=TRIM(ptsource_emis),STATUS='OLD')
	! Allocate the species names vectors
	ALLOCATE(fl_out%c_spname(fl_out%nspec), fl_out%spname(10,fl_out%nspec), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'species name vectors')
	
	! Read the the species list
	READ(emis_unit,*,IOSTAT=io_stat) str_dummy, str_dummy, (s_inp_spec(i_nsp), i_nsp = 1, i_nspec)
	CALL check_io_stat(io_stat, 'species list', 'emissions')
	! Compare the inventory species list with the conversion matrix input species list
	DO i_spi = 1, i_nspec
		IF ( s_inp_spec(i_spi) .NE. s_mat_spec(i_spi) ) THEN
			WRITE(*,*) 'Species ', i_spi, ' mismatch'
			WRITE(*,*) 'Inventory species is: ', s_inp_spec(i_spi)
			WRITE(*,*) 'Matrix input species is: ', s_mat_spec(i_spi)
			CALL EXIT(1)
		END IF
	END DO

	! Write to the human readable species array
	fl_out%c_spname = s_out_spec
	! Write to the species array
	DO i_nsp = 1, fl_out%nspec
		DO i = 1,10
			WRITE(str_refmt,'(4A)') fl_out%c_spname(i_nsp)(i:i)
			fl_out%spname(i,i_nsp) = str_refmt
		END DO
	END DO
	! Diagnostic output of species list
	! WRITE(*,'(10A1)') ((fl_out%spname(i,i_nsp),i=1,10),i_nsp=1,fl_out%nspec)

	! Allocate the emis vectors
	ALLOCATE(camx_id_emi(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'stack codes')
	ALLOCATE(emi_hr(fl_out%nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'emission vector from emissions file')

	! Allocate the time variant headers
	fl_out%update_times = frames
	ALLOCATE(fl_out%ibgdat(frames), fl_out%iendat(frames), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'date header vectors')
	fl_out%ibgdat = emis_date
	fl_out%iendat = emis_date
	ALLOCATE(fl_out%nbgtim(frames), fl_out%nentim(frames), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'time header vectors')

	! Allocate the stack emissions arrays
	ALLOCATE(fl_out%icell(frames,nstk),fl_out%jcell(frames,nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'i and j cell arrays')
	ALLOCATE(fl_out%kcell(frames,nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'kcell array')
	ALLOCATE(fl_out%flow(frames,nstk),fl_out%plmht(frames,nstk), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'flow and plume height arrays')
	ALLOCATE(fl_out%ptemis(frames,nstk,nspec), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'emission array')

	! Allocate the intermediate vectors for transformations
	ALLOCATE(v_inp_emis(i_nspec), v_out_emis(nspec), STAT=alloc_stat)
	CALL check_alloc_stat(alloc_stat, 'intermediate emission vectors for transformations')

	! Set values for the empty/control stack emision arrays
	fl_out%icell = 0
	fl_out%jcell = 0
	fl_out%kcell = 0
	fl_out%flow  = 0.
	fl_out%plmht = 0.

	! Read each data frame
	DO i_dfr = 1, frames

		! Write the time variant header
		fl_out%nbgtim(i_dfr) = 0. + dt*REAL(i_dfr - 1)
		fl_out%nentim(i_dfr) = 0. + dt*REAL(i_dfr)

		! Sanity output
		WRITE(6,'(A,I2)') 'Working on hour ', i_dfr

		! Read each stack record
		DO i_stk = 1, fl_out%nstk
			
			READ(emis_unit,*,IOSTAT=io_stat) camx_id_emi(i_stk), emi_hr(i_stk),&
											& (v_inp_emis(i_nsp), i_nsp = 1, i_nspec)
			IF ( io_stat > 0 ) THEN
				WRITE(0,'(A)') 'Error reading stack emissions file'
				CALL EXIT(0)
			ELSE IF ( io_stat < 0 ) THEN
				WRITE(0,'(A,I3,A,I2)') 'Unexpected end of emissions file while trying to read stack ', i_stk, ' at hour ', i_dfr
				CALL EXIT(0)
			END IF
			
			! Convert units
			v_inp_emis = v_inp_emis * conv_fact

			! Convert to mole using the molecular weigth vector
			v_inp_emis = v_inp_emis / molecular_weights

			! Use a linear transformation to speciate for the model species list
			v_out_emis = MATMUL(v_inp_emis, conv_matrix)

			! Scale the emissions using the scaling factor vector
			v_out_emis = v_out_emis * scale_factors
			
			! Assign the converted output to the output file object
			fl_out%ptemis(i_dfr,i_stk,:) = v_out_emis

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
	! Close the stack emissions file
	CLOSE(emis_unit)

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

SUBROUTINE check_alloc_stat(alloc_stat, target)
IMPLICIT NONE

	INTEGER, INTENT(IN) :: alloc_stat
	CHARACTER(LEN=*), INTENT(IN) :: target

	IF ( alloc_stat .NE. 0 ) THEN
		WRITE(0,'(A,A,A)') 'Error allocating ', target, ' check available memory'
		CALL EXIT(1)
	END IF

END SUBROUTINE check_alloc_stat

SUBROUTINE check_io_stat(io_stat, target_v, target_f)
IMPLICIT NONE

	INTEGER, INTENT(IN) :: io_stat
	CHARACTER(LEN=*), INTENT(IN) :: target_v
	CHARACTER(LEN=*), INTENT(IN) :: target_f

	IF ( io_stat > 0 ) THEN
		WRITE(0,'(A,A,A,A,A)') 'Error reading ', target_v,' from ', target_f, ' file'
		CALL EXIT(0)
	ELSE IF ( io_stat < 0 ) THEN
		WRITE(0,'(A,A,A)') 'Unexpected end of ', target_f,' file'
		CALL EXIT(0)
	END IF

END SUBROUTINE
