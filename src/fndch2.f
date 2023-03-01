      SUBROUTINE FNDCH2(TEXT,N1,N2,CH,N,IRTN)
C  Search for a character CH starting 
C  from column N1 upto N2 in the string TEXT.
C  CH must not be '()[]{}
C  Inside ' ' and parenthesis is not searched.
C  Whether the type of parenthesis matches or not
C  is not checked.
C  Assume the column N1 is not in ' '.
C  If N1 is in parenthesis, search ends at
C  the closing pair.
C    IRTN=0:  found at column N
C         1:  not found
C
      IMPLICIT NONE
      INTEGER N1,N2,N,IRTN
      CHARACTER*(*) TEXT
      CHARACTER*1 CH
      INTEGER LVL,INAPOS,CATCOD,CC
	CHARACTER(1) APOS(2)/"'",'"'/,CH1
C
      IRTN=1
      IF(N1.GT.N2) RETURN
      N=N1
      LVL=0
      INAPOS=0
 200  CH1=TEXT(N:N)
     	IF(INAPOS.EQ.0) THEN
	  IF(CH1.EQ.APOS(1)) THEN
		  INAPOS=1
	  ELSEIF(CH1.EQ.APOS(2)) THEN
		  INAPOS=2
	  ELSE
          IF(LVL.EQ.0.AND.CH1.EQ.CH) THEN
            IRTN=0
            RETURN
          ENDIF
	    CC=CATCOD(CH1)
          IF(CC.GE.11.AND.CC.LE.13) THEN
            LVL=LVL+1
          ELSEIF(CC.GE.21.AND.CC.LE.23) THEN
            LVL=LVL-1
            IF(LVL.LT.0) GOTO 300
          ENDIF
	  ENDIF
	ELSE
	  IF(CH1.EQ.APOS(INAPOS)) INAPOS=0
	ENDIF
      IF(N.LT.N2) THEN
        N=N+1
        GOTO 200
      ENDIF
 300  RETURN
      END
