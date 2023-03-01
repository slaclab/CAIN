      SUBROUTINE EXTFEV(LLLE,LLLB,EB,EXB,C0,OMSQ,OM,CV1,CV2,CE,CB)
C  One special case not treated, i.e,
C   exactly cross field with exactly the same strenth B=E.
      IMPLICIT NONE
      REAL*8 EB(3,2),EXB(3),C0,OMSQ(2),OM(2),CV1(3,2),CV2(3,2),
     %    CE(2),CB(2)
      LOGICAL LLLE,LLLB
      INTEGER I,J
      REAL*8 C1E,C1B,C1,EDOTB
      IF(LLLE.AND.LLLB) THEN
        EDOTB=EB(1,1)*EB(1,2)+EB(2,1)*EB(2,2)+EB(3,1)*EB(3,2)
        EXB(1)=EB(2,1)*EB(3,2)-EB(3,1)*EB(2,2)
        EXB(2)=EB(3,1)*EB(1,2)-EB(1,1)*EB(3,2)
        EXB(3)=EB(1,1)*EB(2,2)-EB(2,1)*EB(1,2)
        C1E=EB(1,1)**2+EB(2,1)**2+EB(3,1)**2
        C1B=-(EB(1,2)**2+EB(2,2)**2+EB(3,2)**2)
        IF(ABS(EDOTB).LE.MAX(C1E,-C1B)*1D-8) EDOTB=0
      ELSE
        EDOTB=0
        EXB(1)=0
        EXB(2)=0
        EXB(3)=0
        IF(LLLE) THEN
          C1E=EB(1,1)**2+EB(2,1)**2+EB(3,1)**2
          C1B=0
        ELSE
          C1B=-(EB(1,2)**2+EB(2,2)**2+EB(3,2)**2)
          C1E=0
        ENDIF
      ENDIF
      C1=C1E+C1B
      IF(EDOTB.EQ.0) THEN
        IF(C1.GE.0) THEN
          OMSQ(1)=C1
          OMSQ(2)=0
        ELSE
          OMSQ(1)=0
          OMSQ(2)=C1
        ENDIF
        C0=ABS(C1)
      ELSE
        C0=SQRT(C1**2+4*EDOTB**2)
        IF(C1.GE.0) THEN
          OMSQ(1)=(C0+C1)/2
          OMSQ(2)=-EDOTB**2/OMSQ(1)
        ELSE
          OMSQ(2)=-(C0-C1)/2
          OMSQ(1)=-EDOTB**2/OMSQ(2)
        ENDIF
      ENDIF
      DO 280 J=1,2
        OM(J)=SQRT(ABS(OMSQ(J)))
        CB(J)=OMSQ(J)-C1B
        CE(J)=OMSQ(J)-C1E
        DO 270 I=1,3
          CV1(I,J)=OMSQ(J)*EB(I,1)+EDOTB*EB(I,2)
          CV2(I,J)=OMSQ(J)*EB(I,2)-EDOTB*EB(I,1)
 270    CONTINUE
 280  CONTINUE
      RETURN
      END
C-------------------------------------------------------------------
      SUBROUTINE EXFTMT(EB,C0,CB,CE,EXB,CV1,CV2,EP,MAT)
      IMPLICIT NONE
      REAL*8 EB(3,2),C0,CB(2),CE(2),EXB(3),CV1(3,2),CV2(3,2),
     %   EP(0:3),MAT(0:3,2,2)
      INTEGER I,J,K
      REAL*8 PE,PB,PCV2(3),C1
      REAL*8 EMASS/0.51099906D6/
C
      PE=EB(1,1)*EP(1)+EB(2,1)*EP(2)+EB(3,1)*EP(3)
      PB=EB(1,2)*EP(1)+EB(2,2)*EP(2)+EB(3,2)*EP(3)
      DO 300 J=1,2
        MAT(0,1,J)=CB(J)*EP(0)-EXB(1)*EP(1)-EXB(2)*EP(2)-EXB(3)*EP(3)
        MAT(0,2,J)=CV1(1,J)*EP(1)+CV1(2,J)*EP(2)+CV1(3,J)*EP(3)
        PCV2(1)=EP(2)*CV2(3,J)-EP(3)*CV2(2,J)
        PCV2(2)=EP(3)*CV2(1,J)-EP(1)*CV2(3,J)
        PCV2(3)=EP(1)*CV2(2,J)-EP(2)*CV2(1,J)
        DO 200 I=1,3
          MAT(I,1,J)=EXB(I)*EP(0)+CE(J)*EP(I)+EB(I,1)*PE+EB(I,2)*PB
          MAT(I,2,J)=CV1(I,J)*EP(0)+PCV2(I)
 200    CONTINUE
        C1=C0*EMASS
        IF(J.EQ.2) C1=-C1
        DO 240 I=0,3
          DO 220 K=1,2
            MAT(I,K,J)=MAT(I,K,J)/C1
 220      CONTINUE
 240    CONTINUE
 300  CONTINUE
      RETURN
      END
C--------------------------------------------------------------------
      SUBROUTINE EXTFAA(OM,TAU,A,DA)
      IMPLICIT NONE
      REAL*8 OM(2),TAU,A(2,2),DA(2,2)
      REAL*8 CH,SH,CO,SI
      IF(OM(1).EQ.0) THEN
        A(1,1)=TAU
        A(2,1)=TAU**2/2
        DA(1,1)=1
      ELSE
        SH=SINH(OM(1)*TAU/2)
        CH=COSH(OM(1)*TAU/2)
        A(1,1)=2*SH*CH/OM(1)
        A(2,1)=2*(SH/OM(1))**2
        DA(1,1)=1+2*SH**2
      ENDIF
      IF(OM(2).EQ.0) THEN
        A(1,2)=TAU
        A(2,2)=TAU**2/2
        DA(1,2)=1
      ELSE
        SI=SIN(OM(2)*TAU/2)
        CO=COS(OM(2)*TAU/2)
        A(1,2)=2*SI*CO/OM(2)
        A(2,2)=2*(SI/OM(2))**2
        DA(1,2)=1-2*SI**2
      ENDIF
      DA(2,1)=A(1,1)
      DA(2,2)=A(1,2)
      RETURN
      END
C--------------------------------------------------------------------------
      SUBROUTINE EXTFDA(OM,TAU,DA)
      IMPLICIT NONE
      REAL*8 OM(2),TAU,DA(2,2)
      REAL*8 CH,SH,CO,SI
      IF(OM(1).EQ.0) THEN
        DA(1,1)=1
        DA(2,1)=TAU
      ELSE
        SH=SINH(OM(1)*TAU/2)
        CH=COSH(OM(1)*TAU/2)
        DA(1,1)=1+2*SH**2
        DA(2,1)=2*SH*CH/OM(1)
      ENDIF
      IF(OM(2).EQ.0) THEN
        DA(1,2)=1
        DA(2,2)=TAU
      ELSE
        SI=SIN(OM(2)*TAU/2)
        CO=COS(OM(2)*TAU/2)
        DA(1,2)=1-2*SI**2
        DA(2,2)=2*SI*CO/OM(2)
      ENDIF
      RETURN
      END
