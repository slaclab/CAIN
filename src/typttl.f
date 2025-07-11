      SUBROUTINE TYPTTL(LR,KIN,LLOST,ITYPE,TEXT,NC)
C  Generates a character string of particle selection
C  Maximum possible number of characters is 81, perhaps.
C  Len(TEXT)>=100 for safety
      IMPLICIT NONE
      INTEGER LR(2),KIN(3),LLOST,ITYPE(3),NC
      CHARACTER*(*) TEXT
      CHARACTER*8 LKIN(3)/'Photon','Electron','Positron'/
      INTEGER NCKIN(3)/6,8,8/
      CHARACTER*6 LTYPE(3)/'normal','incoh.','test'/
      INTEGER NCTYP(3)/6,6,4/
      INTEGER I,NC1
      NC=0
      TEXT=' '
      IF(LR(1).NE.0.AND.LR(2).EQ.0) THEN
	  NC1=NC+12
        TEXT(NC+1:NC1)='RIGHT-going,'
      ELSEIF(LR(1).EQ.0.AND.LR(2).NE.0) THEN
	  NC1=NC+11
        TEXT(NC+1:NC1)='LEFT-going,'
      ELSEIF(LR(1).NE.0.AND.LR(2).NE.0) THEN
	  NC1=NC+11
        TEXT(NC+1:NC1)='RIGHT+LEFT,'
	ELSE
	  NC1=0
      ENDIF
	NC=NC1
      DO 200 I=1,3
        IF(KIN(I).NE.0) THEN
	    NC1=NC+NCKIN(I)+1
          TEXT(NC+1:NC1)=' '//LKIN(I)
          NC=NC1
        ENDIF
 200  CONTINUE
	IF(LLOST.NE.0) THEN
	  NC1=NC+5
	  TEXT(NC+1:NC1)=' Lost'
	  NC=NC1
	ENDIF
      IF(ITYPE(1).NE.0.OR.ITYPE(2).NE.0.OR.ITYPE(3).NE.0) THEN
        DO 260 I=1,3
          IF(ITYPE(I).GE.1) THEN
	      NC1=NC+NCTYP(I)+1
            TEXT(NC+1:NC1)=' '//LTYPE(I)
            NC=NC1
          ENDIF
 260    CONTINUE
        NC1=NC+10
        TEXT(NC+1:NC1)=' particles'
        NC=NC1
      ENDIF
      RETURN
      END


