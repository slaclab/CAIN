	SUBROUTINE FMTCHECK(FMT,IRTN)
C  Check if the format is enclosed by ()
C  IRTN=0:  not enclosed.
C       1:  enclosed.
C      >1:  error
	IMPLICIT NONE
	INTEGER IRTN
	CHARACTER(*) FMT
	INTEGER N,I,L1,L2
	N=LEN(FMT)
	IRTN=100
	IF(N.LE.0) RETURN
	L1=0
	DO I=1,N
	  IF(FMT(I:I).NE.' ') THEN
	    IF(FMT(I:I).EQ.'(') L1=1
	    GOTO 100
	  ENDIF
	ENDDO
	RETURN
100   L2=0
	DO I=N,1,-1
	  IF(FMT(I:I).NE.' ') THEN
	    IF(FMT(I:I).EQ.')') L2=1
	    EXIT
	  ENDIF
	ENDDO
	IF(L1.EQ.L2) THEN
	  IRTN=L1
	ELSE
	  IRTN=100
	ENDIF
	RETURN
	END