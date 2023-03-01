      SUBROUTINE FINDCH(L1,N1,L2,N2,LINE,NCHAR,CH,
     %   L,N,IRTN)
C  Search for a character CH starting 
C  from (line,column)=(L1,N1) upto (L2,N2).
C  CH must not be '"()[]{}
C  Inside a pair of apostrophies and inside
C  parenthesis is not searched.
C  Whether the type of parenthesis matches or not
C  is not checked.
C  Assume (L1,N1) is not in ' '.
C  If (L1,N1) is in parenthesis, search ends at
C  the closing pair.
C    IRTN=0:  found at (L,N)
C         1:  not found
C
      IMPLICIT NONE
      INTEGER L1,N1,L2,N2,NCHAR(L2),L,N,IRTN
      CHARACTER*(*) LINE(L2)
      CHARACTER*1 CH
      INTEGER LVL,INAPOS,CATCOD,CC,NN2
	CHARACTER(1) APOS(2)/"'",'"'/,CH1
C
      IF(L1.GT.L2.OR.(L1.EQ.L2.AND.N1.GT.N2)) THEN
        IRTN=1
        RETURN
      ENDIF
      L=L1
      N=N1
      LVL=0
      INAPOS=0
 200  CH1=LINE(L)(N:N)
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
      IF(L.NE.L2) THEN
        NN2=NCHAR(L)
      ELSE
        NN2=N2
      ENDIF
      IF(N.LT.NN2) THEN
        N=N+1
      ELSE
 220    IF(L.LT.L2) THEN
          L=L+1
          IF(NCHAR(L).EQ.0) GOTO 220
          N=1
        ELSE
          GOTO 300
        ENDIF
      ENDIF
      GOTO 200
 300  IRTN=1
      RETURN
      END
