      SUBROUTINE BBOUT(NP,IFLG,XY,XYCM,LEL,R00,CHU0,SHU0,EL,
     %   FXY,LOUT)
C Choose the method for particles outside
      IMPLICIT NONE
      INTEGER NP,IFLG(NP),LEL,LOUT(3)
      REAL*8 XY(2,NP),XYCM(2),R00,CHU0,SHU0,EL,FXY(2,NP)
      INTEGER N
      REAL*8 R002,EL2,RR
C
      R002=R00**2
      EL2=EL**2
      DO 200 N=1,3
        LOUT(N)=0
 200  CONTINUE
      DO 300 N=1,NP
        IF(IFLG(N).LE.0) GOTO 300
        FXY(1,N)=XY(1,N)-XYCM(1)
        FXY(2,N)=XY(2,N)-XYCM(2)
        IF(FXY(1,N)**2+FXY(2,N)**2.GT.R002) THEN
C Harmonic expansion
          IFLG(N)=2
        ELSE
          IF(LEL.EQ.1) THEN
            IFLG(N)=1
          ELSE
            IF(LEL.GE.0) THEN
              RR=(FXY(1,N)/CHU0)**2+(FXY(2,N)/SHU0)**2
            ELSE
              RR=(FXY(2,N)/CHU0)**2+(FXY(1,N)/SHU0)**2
            ENDIF
            IF(RR.LE.EL2) THEN
              IFLG(N)=1
            ELSE
C Elliptic expansion
              IFLG(N)=3
            ENDIF
          ENDIF
        ENDIF
        LOUT(IFLG(N))=1
 300  CONTINUE
      RETURN
      END
