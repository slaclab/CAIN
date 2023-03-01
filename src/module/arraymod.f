	MODULE ARRAYMOD
	  IMPLICIT NONE
	  TYPE ARRAY
	    INTEGER        TYPE    !  0:undef, 1:real(8), 2:character
	    CHARACTER(16)  NAME    !  must match with MCHAR in namelength.h
	    INTEGER        NC      !  number of characters in NAME
	    INTEGER        RANK
	    INTEGER, POINTER:: DIM(:,:) ! DIM(3,RANK) lower_bound/upper_bound/size
	    REAL(8), POINTER:: VAL(:)   ! vales in 1-dimensional form
	    INTEGER, POINTER:: LC(:,:)  ! LC(2,:)  first and last position in
	                                ! global string stack GSTR
	                                ! String not filled yet if LC(1,*)=0
	                                ! Number of character is LC(2,*)-LC(1,*)+1 
	                                ! If 0, zero length.
	  END TYPE ARRAY

	  INTEGER, PARAMETER:: MARRAY=100
	  INTEGER          NARRAY
	  TYPE(ARRAY)      ARR(MARRAY)

	  INTEGER, PARAMETER:: MGSTR=100000
	  CHARACTER(MGSTR)  GSTR
	  INTEGER          NGSTR        ! Last position used.

	  INTEGER, PARAMETER:: MAXRECURS=4
	  INTEGER, PARAMETER:: MGSTR2=10000
	  CHARACTER(MGSTR2)  GSTR2(MAXRECURS)
	  INTEGER          NGSTR2(MAXRECURS)
	  INTEGER          EVALNEST,EVCMPLNEST,EVALLAST

	END MODULE ARRAYMOD