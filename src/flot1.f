C******************** FLOT1 **********************************
      FUNCTION FLOT1(IA,IK,IRTN)
      IMPLICIT NONE
      INTEGER IK,IRTN
      REAL*8 FLOT1
      CHARACTER*1 IA(IK)
      INTEGER IE,IDEC,I,J,IPAK1,IRTN0,LK
      REAL*8 R,TEN,SIG,S
      CHARACTER*1 NUM(10)/'0','1','2','3','4','5','6','7','8','9'/
      REAL*8 TEN0/10D0/
      IRTN=0
      FLOT1=0.0
      R=0.0
      IE=0
      TEN=10.0
      IDEC=0
      SIG=1.
      DO 20 I=1,IK
      IF(IA(I).EQ.' ') GO TO 20
      IF(IA(I).EQ.'+') GO TO 20
      IF(IA(I).EQ.'-') GO TO 5
      IF(IA(I).EQ.'E'.OR.IA(I).EQ.'e'.OR.IA(I).EQ.'D'.OR.
     %     IA(I).EQ.'d') GO TO 25
      IF(IA(I).NE.'.') GO TO 10
      IDEC=-1
      TEN=1.0D0
      GO TO 20
    5 SIG=-1.0D0
      GO TO 20
   10 DO 15 J=1,10
   15 IF(IA(I).EQ.NUM(J)) GO TO 16
      IRTN=1
      RETURN
   16 S=J-1D0
      IF(IDEC.NE.0) S=S*TEN0**IDEC
      R=R*TEN+S
      IF(IDEC.NE.0) IDEC=IDEC-1
   20 CONTINUE
   21 FLOT1=R*SIG*TEN0**IE
      RETURN
   25 LK=IK-I
      IE=IPAK1(IA(I+1),LK,IRTN0)
      IF(IRTN0.EQ.0) GOTO 21
      IRTN=IRTN+1000*IRTN0
      RETURN
      END
C******************** IPAK1 ***************************************
      FUNCTION IPAK1(IA,IK,IRTN)
      IMPLICIT NONE
      INTEGER IPAK1,IK,IRTN
      CHARACTER*1 IA(IK)
      INTEGER J,IS,L,K
      CHARACTER*1 NAM(10)/'0','1','2','3','4','5','6','7','8','9'/
      J=0
      IRTN=0
      IS=1
      DO 30 L=1,IK
      IF(IA(L).EQ.'+'.OR.IA(L).EQ.' ') GOTO 30
      IF(IA(L).NE.'-') GO TO 5
      IS=-1
      GOTO 30
    5 DO 10 K=1,10
   10 IF(IA(L).EQ.NAM(K)) GOTO 20
      IRTN=1
      GOTO 30
   20 J=J*10+K-1
   30 CONTINUE
      IPAK1=J*IS
      RETURN
      END
