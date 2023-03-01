	MODULE LASRDATA
        IMPLICIT NONE
	  INTEGER,PARAMETER:: MLSR=10
C  Laser profile data from file
C  NLSRFL :  number of files. At most 2.
C  LSRDT(i,l):  data for i-th file of l-th laser
C               i=1,NLSRFL(l)
C  NDIM   :  Number of independent variable
C  ITYPE  :  Type of variable combination.
C            See TABTYPE
C            NDIM  is the number of characters in
C            in string TABTYPE(ITYPE)
C  NVAL(j) : j=1,NDIM.  Number of abscissa for j-th
C            variable
C  NDATA     number of dapa points (product of NVAL's)
C  VMM(1,j),VMM(2,j):  the first and last abscissa
C  DVAL(j) : grid interval for j-th variable.
C            DVAL(j)=(VMM(2,j)-VMM(1,j))/NVAL(j)
C  TWISTCOORD: flag if twist coordinate is used
C  NVDEFINED:  flag if n-vector data stored.
C  FDEFINED:  flag if phase data stored.
C  PTP     : pointer for power density data
C            aligned as in the array form
C              PTP(NVAL(1),NVAL(2),...)
C  PTPNV   : pointer for n-vector data
C              PTPNV(2 or 3,NVAL(1),NVAL(2),...)
	  INTEGER,PARAMETER:: MAXNDIM=4, MAXTABTYPE=6
C       MAXNDIM is the maximum length of the character string TABTYPE.
C       This is equal to the maximum dimension of the table.
        TYPE LASERDATA
			REAL(8) VALMM(2,MAXNDIM), DVAL(MAXNDIM)
			REAL(8), POINTER :: PTP(:),PTPNV(:,:),PTF(:)
			INTEGER NDIM,ITYPE
			INTEGER NVAL(MAXNDIM),NDATA
			LOGICAL NVDEFINED,FDEFINED,TWISTCOORD
	    REAL(8) ZR,XYZ00(3),SIGR(2)
C           When TWISTCOORD=.true., use the re-scaled coordinate defined by
C             \zeta = XYZ00(3) + \zeta_{twist}*f
C             \xi = XY00(1)+ \xi_{twist}*f^2
C             \eta= XY00(2)+ \eta_{twist}*f^2
C                     f = max(1, abs(\zeta_{twist})/ZR)
C                       = max(1, sqrt[abs(\zeta-XYZ00(3))/ZR])
C           VALMM and DVAL are the min, max, and interval in the twist coordinate.
C           For this coordinate to be effective, the focusing parameters
C           (position of the focal point and the Rayleigh length) must
C           be close in x- and y-plane.
        END TYPE

	  INTEGER NLSRFL(MLSR)
	  TYPE(LASERDATA) LSRDT(2,MLSR)
	
	  INTEGER, PARAMETER:: TYPEL=1, TYPEXYZ=2, TYPERZ=3,
     %      TYPELXYZ=4, TYPELRZ=5, TYPEXY=6
	  CHARACTER*(MAXNDIM) TABTYPE(MAXTABTYPE)/
     %             'L','XYZ','RZ','LXYZ','LRZ','XY'/
C      L indicates ct-z
	  INTEGER NCTABTYPE(MAXTABTYPE)/1,3,2,4,3,2/
C          number of characters in TABTYPE
	  INTEGER NDIMTRANS(MAXTABTYPE)/0,2,1,2,1,2/
C          number of `transverse characters' (X or Y or R) in TABTYPE

	  CONTAINS
	    SUBROUTINE PRTLSRDT(LSRDT1,MSGFL,MSGDEST)
	    IMPLICIT NONE
	    TYPE(LASERDATA) LSRDT1
	    INTEGER MSGFL,MSGDEST
	    INTEGER I,J,NDIM,NZ,IZ,IR,ISTAT,ITY,K,NK,IV(2)
	    REAL(8) PMAX
	    REAL(8), ALLOCATABLE:: ZZ(:),PINT(:,:)
	    CHARACTER(19) TEXT(MAXTABTYPE)/
     %     'F(t-z)dt','F(x,y,z)dxdy','F(r,z)2pi*rdr',
     %     'F(t-z,x,y,z)dtdxdy','F(t-z,r,z)2pi*rdrdt','F(x,y)dxdy'/
	    INTEGER NCTEXT(MAXTABTYPE)/8,12,13,18,19,10/

	    PMAX=0
	    DO I=1,LSRDT1%NDATA
	      PMAX=MAX(PMAX,LSRDT1%PTP(I))
	    ENDDO
	    NDIM=LSRDT1%NDIM
	    ITY=LSRDT1%ITYPE
	    WRITE(MSGFL,100) TABTYPE(ITY)
100       FORMAT(' ---------- Laser Data from File  ORDER=',A,' ------')
          WRITE(MSGFL,120) (TABTYPE(ITY)(I:I),I=1,NDIM)
120       FORMAT(T20,4(6X,A1,5X))
          WRITE(MSGFL,140) 'Range min.',(LSRDT1%VALMM(1,I),I=1,NDIM)
	    WRITE(MSGFL,140) 'Range max.',(LSRDT1%VALMM(2,I),I=1,NDIM)
	    WRITE(MSGFL,140) 'Mesh size',(LSRDT1%DVAL(I),I=1,NDIM)
	    WRITE(MSGFL,160) '# of points',(LSRDT1%NVAL(I),I=1,NDIM)
140       FORMAT(1X,A,T20,1P4D12.4)
160       FORMAT(1X,A,T20,4I12)
          WRITE(MSGFL,170) LSRDT1%NDATA,PMAX
170       FORMAT(1X,'Total number of data points',T35,I10,/,
     %       1X,'Maximum of the power density data',T35,1PD12.4)
	    IF(LSRDT1%NVDEFINED) WRITE(MSGFL,180) 
     %       (NDIMTRANS(ITY)+1)*LSRDT1%NDATA
180       FORMAT(1X,'Total number of n-vector data',T35,I10)
	    IF(LSRDT1%FDEFINED) WRITE(MSGFL,190) LSRDT1%NDATA
190       FORMAT(1X,'Total number of phase data',T35,I10)
          IF(LSRDT1%TWISTCOORD) THEN
	      WRITE(MSGFL,200) (LSRDT1%XYZ00(I),I=1,3),LSRDT1%ZR,
     %        (LSRDT1%SIGR(I),I=1,2)
200         FORMAT(' Use twist coordinate defined by',/,
     %        T5,'Origin of (xi,eta,zeta)',T35,1P3D12.4,/,
     %        T5,'Near-far border. zeta=zeta0+-',T35,1PD12.4,/,
     %        T5,'Reference size in near region',T35,1P2D12.4)
	    ENDIF
	    NZ=1
	    IZ=0
	    IR=0
	    DO I=1,NDIM
	      IF(TABTYPE(ITY)(I:I).EQ.'Z') THEN
	        NZ=LSRDT1%NVAL(I)
	        IZ=I
	      ELSEIF(TABTYPE(ITY)(I:I).EQ.'R') THEN
	        IR=I
	      ENDIF
	    ENDDO
	    ALLOCATE(ZZ(NZ),PINT(NZ,0:2),STAT=ISTAT)
	    IF(ISTAT.NE.0) THEN
	      WRITE(MSGFL,300)
300         FORMAT(' --- Memory allocation failed in power integration.'
     %        ,/,'      (This is for printing only. skipped.)')
	    ELSE
            CALL LSRFLPINT(LSRDT1,IZ,IR,NZ,NK,IV,ZZ,PINT)
	      WRITE(MSGFL,310) TEXT(ITY)(1:NCTEXT(ITY))
310         FORMAT(1X,'Integrated quantities on z=const.',/,
     %           20X,'Pint = ',A)
	      IF(NK.EQ.0) THEN
	        WRITE(MSGFL,330)
330           FORMAT(5X,'     z(m)         Pint   ')
            ELSE
	        WRITE(MSGFL,340) (TABTYPE(ITY)(IV(K):IV(K)),K=1,NK)
340           FORMAT(5X,'     z(m)         Pint   '
     %          2('   rms',A,' (m) ',:))
	      ENDIF
	      DO I=1,NZ
	        WRITE(MSGFL,360) ZZ(I),(PINT(I,K),K=0,NK)
360           FORMAT(5X,1PD12.4,1X,1P3D12.4)
            ENDDO
	      DEALLOCATE(ZZ,PINT,STAT=ISTAT)
	    ENDIF
	    IF(MSGDEST.GE.2) CALL FILEECHO(1)
	    END SUBROUTINE PRTLSRDT

	    SUBROUTINE LSRFLPINT(LSRDT1,IZ,IR,NZ,NK,IV,ZZ,PINT)
C           Integrate the power over L,X,Y,R for constant Z.
          IMPLICIT NONE
	    TYPE(LASERDATA) LSRDT1
	    INTEGER IZ,IR,NZ,NK,IV(2)
	    REAL(8) ZZ(NZ),PINT(NZ,0:2)
	    INTEGER NN(3),II(3),NII(3),JV(2)
	    INTEGER NJ,J,I,JR,N,N1,NNZ,IIZ,K
	    REAL(8) DVOL,F1,TWF,VMIN(2),DV(2),VV(0:2)
	    REAL(8) PI/3.141592653589793238D0/
C           I: index in TABTYPE(LSRDT1%ITYPE)
C           J: index of integration variables     (at most 3, x,y,t-z)
C           K: index of variables to compute rms  (at most 2, x,y)
C          II(J): integration index for j-th variable

	    J=0
	    JR=0
	    K=0
	    N=1
	    DVOL=1
	    NNZ=1
	    DO I=1,LSRDT1%NDIM
	      IF(I.EQ.IZ) THEN
	        NNZ=N
	      ELSE
	        J=J+1
	        NN(J)=N
	        NII(J)=LSRDT1%NVAL(I)
	        IF(I.EQ.IR) THEN
	          JR=J
	          DVOL=DVOL*LSRDT1%DVAL(I)**2*2*PI
	        ELSE
	          DVOL=DVOL*LSRDT1%DVAL(I)
	        ENDIF
	        IF(TABTYPE(LSRDT1%ITYPE)(I:I).NE.'L') THEN
	          K=K+1
	          IV(K)=I
	          JV(K)=J
	          VMIN(K)=LSRDT1%VALMM(1,I)
	          DV(K)=LSRDT1%DVAL(I)
	        ENDIF
	      ENDIF
	      N=N*LSRDT1%NVAL(I)
	    ENDDO
	    NJ=J
	    NK=K
	    PINT=0
	    VV(0)=1
	    II=1
200	    J=1
220       IF(II(J).GE.NII(J)) THEN
            II(J)=1
            J=J+1
	      IF(J.GT.NJ) GOTO 240
	      GOTO 220
	    ELSE
	      II(J)=II(J)+1
          ENDIF
	    N=1
	    F1=1
	    DO J=1,NJ
	      N=N+(II(J)-1)*NN(J)
	      IF(J.EQ.JR) THEN
	        IF(II(J).EQ.1) THEN
	          F1=0.25D0
	        ELSEIF(II(J).EQ.NII(J)) THEN
	          F1=0.5D0*(II(J)-1.5D0)
	        ELSE
	          F1=II(J)-1
	        ENDIF
	      ENDIF
	    ENDDO
	    IF(NK.NE.0) THEN
	      DO K=1,NK
	        VV(K)=(VMIN(K)+DV(K)*(II(JV(K))-1))**2
	      ENDDO
	    ENDIF
	    DO IIZ=1,NZ
	      N1=N+NNZ*(IIZ-1)
	      DO K=0,NK
	        PINT(IIZ,K)=PINT(IIZ,K)+VV(K)*F1*LSRDT1%PTP(N1)
	      ENDDO
	    ENDDO
	    GOTO 200
240	    DO IIZ=1,NZ
	      TWF=1
	      IF(IZ.NE.0) THEN
	        ZZ(IIZ)=LSRDT1%VALMM(1,IZ)+LSRDT1%DVAL(IZ)*(IIZ-1)
	        IF(LSRDT1%TWISTCOORD) THEN
	          TWF=MAX(1D0,ABS(ZZ(IIZ))/LSRDT1%ZR)
	          ZZ(IIZ)=ZZ(IIZ)*TWF
	        ENDIF
	      ELSE
	        ZZ(IIZ)=0
	      ENDIF
	      IF(NK.NE.0.AND.PINT(IIZ,0).GT.0) THEN
	        DO K=1,NK
	         PINT(IIZ,K)=SQRT(MAX(0D0,PINT(IIZ,K)/PINT(IIZ,0)))*TWF**2
	        ENDDO
	      ENDIF
	      PINT(IIZ,0)=PINT(IIZ,0)*DVOL*TWF**4
	    ENDDO
	  RETURN
	  END SUBROUTINE LSRFLPINT
	
	END MODULE LASRDATA