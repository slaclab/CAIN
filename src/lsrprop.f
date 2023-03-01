      SUBROUTINE LSRPROP(LSRDT1,Z0,WL,RANGE,ZMAX,TDL,IRTN)
C   Laser field propagation in vacuum.
C   Assume perfectly coherent beam.
C   Needed for interpreting laser file data for ORDER=XY
C
C   Input
C     The 2D field E(x,y) on the plane z=z0 has been stored in LSRDT1
C     Original mesh
C        xymin = LSRDT1%VALMM(1,J)   (J=1 for x, 2 for y)
C        xymax = LSRDT1%VALMM(2,J)
C        dxy   = LSRDT1%DVAL(J)
C        nxy   = LSRDT1%NVAL(J)
C           (Note that J=3 is undefined in the input)
C     Field data
C        abs(E)^2 = LSRDT1%PTP(*)
C           normalized as power density (W/m^2)
C        cmplx phase = LSRDT1%PTF(*)
C   This subroutine converts the data into the 3D form as if the file
C   data is given in the format ORDER=XYZ.
C   Output is overwritten on LSRDT1. Adopt twist coordinate (see lasrdata.f).
C   LSRDT1%VALMM, DVAL and NVAL are filled in the twist coordinate.
C   The range of z is (Z0-ZMAX,Z0+ZMAX).
C   RANGE will be filled in this routine. This is not on the twist
C   coordinate.

C
	USE LASRDATA
	IMPLICIT NONE
	INCLUDE 'include/ctrlcm.h'
c	INCLUDE 'include/lasrcm.h'
	TYPE(LASERDATA) LSRDT1
	INTEGER IRTN
	REAL(8) Z0,WL,RANGE(2,0:3),ZMAX,TDL(2)
	INTEGER ITYP,NXY(2),NXYZ(3),ISTAT,N,I,J,L,IZ
	REAL(8) DXY(2),XYMM(2,2),ZFOC(2),BFOC(2),SIGFOC(2),EMIT(2),
     %   DXYZ(3),ZR0(2),ZR,XYZ00(3),SIGR(2),XYZMM(2,3),TH
	COMPLEX(8), ALLOCATABLE:: E(:,:)
	INTEGER IDEBUG1/1/

	IRTN=0
	IF(LSRDT1%ITYPE.NE.TYPEXY) RETURN
	NXY(1:2)=LSRDT1%NVAL(1:2)
	DXY(1:2)=LSRDT1%DVAL(1:2)
	DO L=1,2
	  XYMM(1:2,L)=LSRDT1%VALMM(1:2,L)
	ENDDO
	ALLOCATE(E(NXY(1),NXY(2)),STAT=ISTAT)
	IF(ISTAT.NE.0) THEN
	  IRTN=1000
	  WRITE(MSGFL,100)
100     FORMAT(' (SUBR.LSRPROP) Temporary memory allocation for laser',
     %    ' field failed.')
        RETURN
	ENDIF
	N=0
	DO J=1,NXY(2)
	  DO I=1,NXY(1)
	    N=N+1
	    TH=LSRDT1%PTF(N)
	    E(I,J)=SQRT(LSRDT1%PTP(N))*DCMPLX(COS(TH),SIN(TH))
	  ENDDO
	ENDDO
C      Now the input 2D data have been copied to E, XYMM, NXY and DXY.

C   Compute the position of focus and Rayleigh length in order to
C   estimate the required mesh region
	CALL LSRFOCUS(E,NXY,DXY,XYMM,Z0,ZMAX,WL,
     %     XYZ00(1:2),ZFOC,BFOC,SIGFOC,EMIT,IDEBUG1,MSGFL,IRTN)
	IF(IRTN.NE.0) THEN
	  WRITE(MSGFL,110) IRTN
110     FORMAT(' (SUBR.LSRPROP) Laser file data wrong or program ',
     %    'error',/,'   Emittance imaginary?  Rtn code ',I4)
	  IRTN=1003
	  GOTO 800
	ENDIF
C  Define z-mesh
	XYZ00(3)=(ZFOC(1)+ZFOC(2))/2
	ZR0(1:2)=BFOC(1:2)*MAX(1D0,TDL(1:2))
C       Take into account the later dilution by TDL factor
	ZR=SQRT(ZR0(1)*ZR0(2))
	DO L=1,2
	  SIGR(L)=SIGFOC(L)*SQRT(1+(ZR+ABS(ZFOC(L)))**2/BFOC(L)**2)
	ENDDO	  
	DXYZ(3)=0.2D0*ZR    !  zmesh = Rayleigh_length(approx) /5
	DO I=1,2
	  J=2*I-3
	  RANGE(I,3)=Z0+J*ZMAX    !  RANGE in laser coordinate
	  XYZMM(I,3)=RANGE(I,3)-XYZ00(3)  !  XYZMM in twist coordinate
	  IF(ABS(XYZMM(I,3)).GT.ZR) THEN
	    XYZMM(I,3)=XYZMM(I,3)*SQRT(ZR/ABS(XYZMM(I,3)))
	  ENDIF
	ENDDO
	NXYZ(3)=INT((XYZMM(2,3)-XYZMM(1,3))/DXYZ(3))+2
	IF(MOD(NXYZ(3),2).EQ.0) NXYZ(3)=NXYZ(3)+1
	DXYZ(3)=(XYZMM(2,3)-XYZMM(1,3))/(NXYZ(3)-1)
	N=NXYZ(3)
C  Define (x,y) mesh
	DO L=1,2
	  DXYZ(L)=0.1D0*SIGR(L)              !  xymesh =(approx) sigma/10
	  XYZMM(1,L)=-6*SIGR(L)
        XYZMM(2,L)=+6*SIGR(L)      !   xy region = +-4*sigma
	  NXYZ(L)=INT((XYZMM(2,L)-XYZMM(1,L))/DXYZ(L))+1
	  IF(MOD(NXYZ(L),2).EQ.0) NXYZ(L)=NXYZ(L)+1
C            number of transverse mesh is odd
	  DXYZ(L)=(XYZMM(2,L)-XYZMM(1,L))/(NXYZ(L)-1)
	  N=N*NXYZ(L)
	ENDDO
	IF(IDEBUG1.NE.0) THEN
		WRITE(MSGFL,120)
     %     (XYZ00(L),BFOC(L),ZFOC(L),EMIT(L),SIGFOC(L),L=1,2),
     %     (XYZMM(1,L),L=1,3),(XYZMM(2,L),L=1,3),(DXYZ(L),L=1,3),
     %     (NXYZ(L),L=1,3),(SIGR(L),L=1,2),XYZ00(3),ZR,N
120     FORMAT(' (LSRPROP) Field property estimated by LASERFOSUS',/,
     %     2('   XY00=',1PD12.4,'  Beta=',1PD12.4,'  ZFOC=',1PD12.4,
     %     ' Emit=',1PD12.4, ' Sigma=',1PD12.4,/),
     %   '   Mesh region (twist coordinate)',
     %                           T35,6X,'xi',11X,'eta',10X,'zeta',/,
     %     T5,'min',T35,1P3D13.4,/,
     %     T5,'max',T35,1P3D13.4,/,
     %     T5,'mesh size',T35,1P3D13.4,/,
     %     T5,'number of mesh',T35,3I13,/,
     %     T5,'reference size',T35,1P2D12.4,/,
     %     T5,'zeta-center',T35,1PD12.4,/,
     %     T5,'near-far border',T35,1PD12.4,/,
     %     T5,'total number of mesh point ',T35,I10)
	  IF(MSGDEST.GE.2) CALL FILEECHO(1)
      ENDIF
	IF(N.GE.3000000) THEN
	  IRTN=1005
	  WRITE(MSGFL,130) N
130     FORMAT(' (SUBR.LSRPROP) Required mesh point for laser field ',
     %   'table =',I10,' is too many.')
	  GOTO 800
	ENDIF

C   Re-allocate LSRDT1
	DEALLOCATE(LSRDT1%PTP,LSRDT1%PTF,STAT=ISTAT)
	LSRDT1%FDEFINED=.FALSE.
	IF(ISTAT.NE.0) THEN
	  IRTN=1004
	  WRITE(MSGFL,140)
140     FORMAT(' (SUBR.LSRPROP) Memory de-allocation for laser',
     %    ' field failed.')
        GOTO 800
	ENDIF
	ALLOCATE(LSRDT1%PTP(N),STAT=ISTAT)
	IF(ISTAT.NE.0) THEN
	  IRTN=1006
	  WRITE(MSGFL,160)
160     FORMAT(' (SUBR.LSRPROP) Memory allocation for laser',
     %    ' field failed.')
        GOTO 800
	ENDIF
	LSRDT1%ITYPE=TYPEXYZ   !   ORDER=XYZ
	LSRDT1%NDIM=3
	LSRDT1%TWISTCOORD=.TRUE.
	LSRDT1%NDATA=N
	LSRDT1%XYZ00=XYZ00
	LSRDT1%ZR=ZR
	LSRDT1%SIGR=SIGR
	DO L=1,3
	  LSRDT1%VALMM(1:2,L)=XYZMM(1:2,L)
	  LSRDT1%DVAL(L)=DXYZ(L)
	  LSRDT1%NVAL(L)=NXYZ(L)
	ENDDO
	DO L=1,2
C             RANGE(*,3) already defined
	  DO I=1,2
	    RANGE(I,L)=XYZ00(L)+XYZMM(I,L)
     %      *MAX(1D0,(XYZMM(1,3)/ZR)**2,(XYZMM(2,3)/ZR)**2)
	  ENDDO
	ENDDO

C   Fill the new table
	CALL LSRPROP0(E,NXY,DXY,XYMM,Z0,WL,ZR,XYZ00,SIGR,
     %   LSRDT1%PTP,NXYZ,DXYZ,XYZMM,IDEBUG1,IRTN)
	IF(IRTN.NE.0) THEN
	  WRITE(MSGFL,100)
	  GOTO 800
	ENDIF
	IF(IDEBUG1.NE.0) CALL PRTLSRDT(LSRDT1,MSGFL,MSGDEST)

800	DEALLOCATE(E,STAT=ISTAT)
	RETURN
	END

	SUBROUTINE NFORFFT(N)
C   Find an integer close to input N (>=2) and better for FFT
C   Output N is either  2^m or 3*2^m
C      0.8165 < (output N)/(input N) < 1.2245
	IMPLICIT NONE
	INTEGER N
	INTEGER N0
	REAL(8) X

	N0=N
	N=1
100   IF(N.LT.N0) THEN
	  N=N*2
	  GOTO 100
	ENDIF
	IF(N.EQ.N0) RETURN
	X=DFLOAT(N0)/DFLOAT(N)
	IF(X.LE.0.6124D0) THEN
	  N=N/2
	ELSEIF(X.LE.0.8666D0) THEN
	  N=(N/4)*3
	ENDIF
	RETURN
	END