PROGRAM preproc_ptsource

IMPLICIT NONE

!	------------------------------------------------------------------------------------------
!	Purpose:
!		Converts the point source parameters and emissions from the UPB - GIA inventory in CSV
!		format to the CAM 6.20 forma
!	Inputs
!		Point source parameter file
!		Point source emissions file
!	Outputs
!		Point source emissions file in CAMx 6.20 forma
!	By:
!		Pablo Garcia
!		pablogrb@gmail.com
!		UPB - GIA
!		2020-03
!	------------------------------------------------------------------------------------------

!	------------------------------------------------------------------------------------------
!	Declarations

!	IO
	CHARACTER(LEN=256) :: ctrlfile					! Control namelist
	CHARACTER(LEN=256) :: ptsource_param			! Point source parameter file
	CHARACTER(LEN=256) :: ptsource_emiss			! Point source emission file
	CHARACTER(LEN=256) :: ptsource_out				! CAMx emissions file

!	Projection parameters
	CHARACTER(LEN=10) :: Map_Projection				! (LAMBERT,POLAR)
	INTEGER :: UTM_Zone
	REAL :: POLAR_Longitude_Pole					! deg (west<0,south<0)
	REAL :: POLAR_Latitude_Pole						! deg (west<0,south<0)
	REAL :: LAMBERT_Center_Longitude				! deg (west<0,south<0)
	REAL :: LAMBERT_Center_Latitude					! deg (west<0,south<0)
	REAL :: LAMBERT_True_Latitude1					! deg (west<0,south<0)
	REAL :: LAMBERT_True_Latitude2					! deg (west<0,south<0, can be same as
													!      LAMBERT_True_Latitude1)

!	Namelist IO
	INTEGER :: nml_unit
	NAMELIST /file_io/ ptsource_param, ptsource_emiss, ptsource_out
	NAMELIST /projection/ Map_Projection, UTM_Zone, POLAR_Longitude_Pole, POLAR_Latitude_Pole, &
						& LAMBERT_Center_Longitude, LAMBERT_Center_Latitude, &
						& LAMBERT_True_Latitude1, LAMBERT_True_Latitude2

!	------------------------------------------------------------------------------------------
!	Entry point
!	------------------------------------------------------------------------------------------

END PROGRAM preproc_ptsource