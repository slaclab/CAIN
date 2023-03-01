      SUBROUTINE APSOFF(TEXT,NC)
C  Eliminate outermost apostrophe pairs.
C  NC: number of characters after taking off apostrophes.
C  Note that a string like
C        'x=a','y=b'
C  is returned with a (presumably) unwanted form
C         x=a','y=b
C
      IMPLICIT NONE
      CHARACTER*(*) TEXT
      INTEGER NC,NC0,I,I1,I2,IAP
      CHARACTER*1 APOS(2)/"'",'"'/
C
      NC0=LEN(TEXT)
      NC=NC0
      IF(NC0.LE.1) RETURN
      DO 200 I=1,NC0-1
        IF(TEXT(I:I).EQ.APOS(1)) THEN
	    IAP=1
          I1=I
          GOTO 220
	  ELSEIF(TEXT(I:I).EQ.APOS(2)) THEN
	    IAP=2
          I1=I
          GOTO 220
        ELSEIF(TEXT(I:I).NE.' ') THEN
          RETURN    !  start with non-apos
        ENDIF
 200  CONTINUE
      RETURN    !  blanck string
 220  DO 240 I=NC0,I1+1,-1
        IF(TEXT(I:I).EQ.APOS(IAP)) THEN
          I2=I
          GOTO 260
        ELSEIF(TEXT(I:I).NE.' ') THEN
          RETURN    !  start with apos but end with non-apos
        ENDIF
 240  CONTINUE
      RETURN
 260  DO 280 I=I1+1,I2-1
        TEXT(I-I1:I-I1)=TEXT(I:I)
 280  CONTINUE
      DO 300 I=I2,NC0+I1
        TEXT(I-I1:I-I1)=' '
 300  CONTINUE
      NC=I2-I1-1
      RETURN
      END


      
