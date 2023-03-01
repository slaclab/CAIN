      FUNCTION CATCOD(CH)
      IMPLICIT NONE
      CHARACTER*1 CH
      INTEGER CATCOD,K
      CHARACTER*1 KKO(3)/'(','[','{'/,KKC(3)/')',']','}'/,
     %    APOS/''''/
C
      IF(CH.EQ.' ') THEN
        CATCOD=0
      ELSEIF(CH.GE.'A'.AND.CH.LE.'Z'.OR.CH.GE.'a'.AND.CH.LE.'z') THEN
        CATCOD=1
      ELSEIF(CH.GE.'0'.AND.CH.LE.'9') THEN
        CATCOD=2
      ELSEIF(CH.EQ.'='.OR.CH.EQ.'<'.OR.CH.EQ.'>') THEN
        CATCOD=7
      ELSEIF(CH.EQ.',') THEN
        CATCOD=8
      ELSEIF(CH.EQ.';') THEN
        CATCOD=9
      ELSEIF(CH.EQ.APOS) THEN
        CATCOD=10
      ELSE
        DO 200 K=1,3
          IF(CH.EQ.KKO(K)) THEN
            CATCOD=10+K
            RETURN
          ENDIF
 200    CONTINUE
        DO 220 K=1,3
          IF(CH.EQ.KKC(K)) THEN
            CATCOD=20+K
            RETURN
          ENDIF
 220    CONTINUE
        CATCOD=99
      ENDIF
      RETURN
      END
