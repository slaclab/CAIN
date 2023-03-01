      SUBROUTINE BBKCK(L,NP,XY,FXY,NOUT,MX,MY,Q,LVEC,IFLG)
	USE BBPKCM
      IMPLICIT NONE
      INTEGER L,NP,NOUT,MX,MY,LVEC,IFLG(NP)
      REAL*8 XY(2,NP),FXY(2,NP),Q(MX,MY,2)
C      INCLUDE 'include/bbpkcm.h'
      INTEGER L2,N,LOUT(3)
      REAL*8 DSUM,DCX,DCY,X00(2)
C
      L2=3-L
      NOUT=0
      DO 200 N=1,NP
        IF(XY(1,N).LT.XYMIN(1,L2).OR.XY(1,N).GT.XYMAX(1,L2).OR.
     %     XY(2,N).LT.XYMIN(2,L2).OR.XY(2,N).GT.XYMAX(2,L2)) THEN
          IFLG(N)=1
          NOUT=NOUT+1
        ELSE
          IFLG(N)=0
        ENDIF
 200  CONTINUE
      CALL BBKCK0(NP,XY,FXY,NXY(1),NXY(2),DXY(1,L2),DXY(2,L2),
     %    XYMIN(1,L2),
     %    MMX,MMY,PHI(1,1,L2),DPHI(1,1,1,L2),IFLG)
      IF(NOUT.EQ.0) RETURN
      IF(LEL.EQ.0) THEN
        DO 220 N=1,NP
          IF(IFLG(N).GT.0) THEN
            FXY(1,N)=0
            FXY(2,N)=0
          ENDIF
 220    CONTINUE
      ELSE
        IF(LBBEXP(L2).EQ.0) THEN
          CALL BBXPND(L2,MX,MY,Q,LVEC)
          LBBEXP(L2)=1
        ENDIF
        CALL BBOUT(NP,IFLG,XY,XYCM(1,L2),LEL,R00(L2),CHU0,SHU0,
     %       EL(L2),FXY,LOUT)
        DSUM=DXY(1,L2)+DXY(2,L2)
        DCX=ACOULX*DSUM
        DCY=ACOULY*DSUM
        X00(1)=0.5D0*(NXY(1)+1)*DXY(1,L2)
        X00(2)=0.5D0*(NXY(2)+1)*DXY(2,L2)
        IF(LOUT(1).NE.0)
     %    CALL BBCOUL(NXY(1),NXY(2),DXY(1,L2),DXY(2,L2),X00,MX,MY,
     %    Q(1,1,L2),NCOULX,NCOULY,DSUM,DCX,DCY,COULF,NP,IFLG,FXY)
        IF(LOUT(2).NE.0)
     %    CALL BBHARM(NMOM,R00(L2),QMOM(0,1,L2),NP,IFLG,FXY)
        IF(LOUT(3).NE.0)
     %    CALL BBELLP(NMOM,LEL,EU0,EL(L2),QMOM(0,2,L2),NP,IFLG,FXY)
c        CALL BBKKMM(NP,IFLG,XY,XYCM(1,L2),NXY(1),NXY(2),
c     %       DXY(1,L2),DXY(2,L2),
c     %       MMOM,NMOM,LEL,R00(L2),EU0,CHU0,SHU0,EL(L2),QMOM(0,1,L2),
c     %       MX,MY,Q(1,1,L2),FXY)
      ENDIF
      RETURN
      END
