      SUBROUTINE BBFKER(NXY0,DXY0,XYMIN0,NMOM0,
     %               LEL1,R001,EL1,U001,IRTN)
	USE BBPKCM
      IMPLICIT NONE
C      INCLUDE 'include/bbpkcm.h'
      INTEGER NXY0(2),NMOM0,LEL1,IRTN
      REAL*8 DXY0(2,2),XYMIN0(2,2),R001(2),EL1(2),U001
      INTEGER I,J,L
      REAL*8 DXY1(2),X,Y,F,F1,AVR,PHI1,PHI2,RW
      INTEGER NCALL/0/,NXY00(2),NMOM00
      REAL*8 RAT,RAT1,RAT2,RAT00
      SAVE NCALL,NXY00,RAT00,NMOM00
      REAL*8 PI/3.14159 26535 89793 238D0/,PIH
c      F(X,Y)=-0.5D0*(X/DXY(1))*(Y/DXY(2))
c     %        *(LOG((X**2+Y**2)/(DXY(1)*DXY(2)))
c     %        +(X/Y)*ATAN(Y/X)+(Y/X)*ATAN(X/Y))
      REAL*8 A1,A2,AX1,AX2,AY1,AY2,CX,CY,AA
      F(X,Y)=A1*(X*Y*LOG(A2*(X**2+Y**2))
     %           +X**2*ATAN(Y/X)+Y**2*ATAN(X/Y))
c      F(X,Y)=A1*(X*Y*LOG(A2*(X**2+Y**2))
c     %           +(X**2-Y**2)*ATAN(Y/X)+Y**2*PIH)
C
      CALL CPUTIM('BBFKER',1)
      NCALL=NCALL+1
      IF(NXY0(1).GT.MMX.OR.NXY0(2).GT.MMY) GOTO 900
      RAT1=DXY0(1,1)/DXY0(2,1)
      RAT2=DXY0(1,2)/DXY0(2,2)
      IF(ABS(RAT1-RAT2).GE.1D-6) GOTO 920
      RAT=(RAT1+RAT2)/2
      NMOM=MIN(MMOM,NMOM0)
      NFX=NXY0(1)*2
      NFY=NXY0(2)*2
      DO 200 J=1,2
        NXY(J)=NXY0(J)
        DXY1(J)=DXY0(J,1)
        DO 180 L=1,2
          DXY(J,L)=DXY0(J,L)
          XYMIN(J,L)=XYMIN0(J,L)
          XYMAX(J,L)=XYMIN(J,L)+NXY(J)*DXY(J,L)
 180    CONTINUE
 200  CONTINUE
C
      IF(NCALL.NE.1) THEN
        IF(NXY0(1).EQ.NXY00(1).AND.NXY0(2).EQ.NXY00(2).
     %     AND.RAT.EQ.RAT00.AND.NMOM0.EQ.NMOM00) GOTO 300
      ENDIF
      RAT00=RAT
      NXY00(1)=NXY0(1)
      NXY00(2)=NXY0(2)
      NMOM00=NMOM0
C
      PIH=PI/2D0
      A2=1D0/(DXY1(1)*DXY1(2))
      A1=-0.5D0*A2
      AX1=NXY(1)+0.5D0
      AX2=NXY(1)-0.5D0
      AY1=NXY(2)+0.5D0
      AY2=NXY(2)-0.5D0
      AA=1D0/(NFX*NFY)
      AVR=0
      DO 10 J=1,NFY-1
*VOCL LOOP,NOVREC
        DO 20 I=1,NFX-1
c          F1=F((NXY(1)-I+0.5D0)*DXY1(1),(NXY(2)-J+0.5D0)*DXY1(2))
c     %      -F((NXY(1)-I-0.5D0)*DXY1(1),(NXY(2)-J+0.5D0)*DXY1(2))
c     %      -F((NXY(1)-I+0.5D0)*DXY1(1),(NXY(2)-J-0.5D0)*DXY1(2))
c     %      +F((NXY(1)-I-0.5D0)*DXY1(1),(NXY(2)-J-0.5D0)*DXY1(2))
          F1=F((AX1-I)*DXY1(1),(AY1-J)*DXY1(2))
     %      -F((AX2-I)*DXY1(1),(AY1-J)*DXY1(2))
     %      -F((AX1-I)*DXY1(1),(AY2-J)*DXY1(2))
     %      +F((AX2-I)*DXY1(1),(AY2-J)*DXY1(2))
          KER(I,J)=F1
          AVR=AVR+F1
 20     CONTINUE
 10   CONTINUE
      AVR=AVR*AA
      DO 40 J=1,NFY-1
*VOCL LOOP,NOVREC
        DO 30 I=1,NFX-1
          KER(I,J)=KER(I,J)-AVR
 30     CONTINUE
 40   CONTINUE
*VOCL LOOP,NOVREC
      DO 50 I=1,NFX
        KER(I,NFY)=DCMPLX(0.D0,0.D0)
 50   CONTINUE
*VOCL LOOP,NOVREC
      DO 60 J=1,NFY
        KER(NFX,J)=DCMPLX(0.D0,0.D0)
 60   CONTINUE
      CALL ZFFT2(KER,2*MMX,NFX,NFY,-1,-1)
      CY=PI*(NFY-2)/NFY
      CX=PI*(NFX-2)/NFX
      DO 110 J=1,NFY
        PHI1=(J-1)*CY
*VOCL LOOP,NOVREC
        DO 120 I=1,NFX
          PHI2=PHI1+(I-1)*CX
          KER(I,J)=KER(I,J)*DCMPLX(COS(PHI2),SIN(PHI2))*AA
120     CONTINUE
110   CONTINUE
C  Table for direct Coulomb force
C  When the horizontal and vertical distances between a given point
C  and a bin center is smaller than (ACOULX*(DX+DY),ACOULY*(DX+DY)),
C  the table is refered to. The table dimension is
C  (0:NCOULX,0:NCOULY). Only the first quadrant is in the table.
C  The table depends on DX/DY ratio only. The length is scaled
C  by (DX+DY).
      NCOULX=MMX
      NCOULY=MMY
      ACOULX=3
      ACOULY=3
      CALL BBCTAB(RAT,1D0,NCOULX,NCOULY,ACOULX,ACOULY,COULF)
C
 300  DO 320 L=1,2
        XYCM(1,L)=XYMIN(1,L)+NXY(1)*DXY(1,L)/2D0
        XYCM(2,L)=XYMIN(2,L)+NXY(2)*DXY(2,L)/2D0
 320  CONTINUE
      IF(NMOM.LT.0) THEN
        LEL=0
      ELSE
        RW=RAT*NXY(1)/NXY(2)
        IF(RW.GE.0.8.AND.RW.LE.1.25D0) THEN
          LEL=1
          DO 340 L=1,2
            R00(L)=SQRT((NXY(1)*DXY(1,L))**2
     %                 +(NXY(2)*DXY(2,L))**2)/2
 340      CONTINUE
        ELSE
          LEL=2
          IF(RW.LE.1) LEL=-2
          EU0=SQRT((RW+1)/ABS(RW-1))
          U00=LOG(EU0)
          CHU0=(EU0+1/EU0)/2
          SHU0=SINH(U00)
          DO 400 L=1,2
            R00(L)=MAX(NXY(1)*DXY(1,L),NXY(2)*DXY(2,L))
            EL(L)=SQRT(ABS((NXY(1)*DXY(1,L))**2
     %                    -(NXY(2)*DXY(2,L))**2)/2)
 400      CONTINUE
        ENDIF
      ENDIF
      LEL1=LEL
      DO 420 L=1,2
        R001(L)=R00(L)
        EL1(L)=EL(L)
 420  CONTINUE
      U001=U00
      IRTN=0
      GOTO 1000
 900  IRTN=1000
      print *, " bbfker nxy0(1)= ", nxy0(1), " mmx= ", mmx,
     &       " nxy0(2)= ", nxy0(2), " mmy= ", mmy    
      GOTO 1000
 920  IRTN=1001
      GOTO 1000
 1000 CALL CPUTIM('BBFKER',2)
      RETURN
      END

