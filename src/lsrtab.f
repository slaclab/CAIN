	SUBROUTINE LSRTAB(LSR,TXYZ,TFAC,SFAC,VECN,IRTN)
C  Interpolate the tabulated laser profile data
C  Input
C    LSR   laser ID number
C    TXYS  position in laser coordinate
C  Output
C    TFAC  power density factor of time profile
C    SFAC  power density factor of spatial profile
C       If one of these is not defined, it is set to 1.
C       If both is given in one table, TFAC is set to it and
C       SFAC is set to 1.
C    VECN  propagation direction
C    IRTN  0 :  normal. POW defined.
C          1 :  normal. POW and VECN defined.
C         10 :  outside the laser fireld region  
C
	USE LASRDATA
	IMPLICIT NONE
	INTEGER LSR,IRTN
	REAL*8 TXYZ(0:3),TFAC,SFAC,VECN(3)
	INCLUDE 'include/lasrcm.h'
	INTEGER I,J,K,N,L,ID(MAXNDIM),II(MAXNDIM),NDIM,NN,ITYPE
	REAL*8 P1(MAXNDIM),P2(MAXNDIM),F(MAXNDIM),F1,NORM,TWF
	REAL*8 FF(0:2**MAXNDIM-1),FNV(0:2**MAXNDIM-1,3),R,FMAX,FMIN
	CHARACTER*1 CH
	LOGICAL LNV,CALR,LTWIST

	NDIM=0
	TFAC=1
	SFAC=1
	CALR=.FALSE.
	DO 400 K=1,NLSRFL(LSR)
	  ITYPE=LSRDT(K,LSR)%ITYPE
	  LTWIST=LSRDT(K,LSR)%TWISTCOORD
	  NDIM=LSRDT(K,LSR)%NDIM
	  LNV=LSRDT(K,LSR)%NVDEFINED
	  NN=NDIMTRANS(ITYPE)+1
	  DO 200 J=1,NDIM
	    CH=TABTYPE(ITYPE)(J:J)
	    IF(CH.EQ.'L') THEN
	      F(J)=TXYZ(0)-TXYZ(3)
	    ELSEIF(CH.EQ.'X') THEN
	      F(J)=TXYZ(1)
	    ELSEIF(CH.EQ.'Y') THEN
	      F(J)=TXYZ(2)
	    ELSEIF(CH.EQ.'Z') THEN
	      F(J)=TXYZ(3)
	      IF(LTWIST) THEN
	        F(J)=F(J)-LSRDT(K,LSR)%XYZ00(3)
	        TWF=1
	        IF(ABS(F(J)).GT.LSRDT(K,LSR)%ZR) THEN
	          TWF=SQRT(ABS(F(J))/LSRDT(K,LSR)%ZR)
	          F(J)=F(J)/TWF
	        ENDIF
	      ENDIF
	    ELSEIF(CH.EQ.'R') THEN
	      IF(.NOT.CALR) THEN
	        R=SQRT(TXYZ(1)**2+TXYZ(2)**2)
	        CALR=.TRUE.
	      ENDIF
C             called at most once. not a waste of time.
            F(J)=R
	    ENDIF
200     ENDDO
        DO 220 J=1,NDIM
	    IF(LTWIST) THEN
	      CH=TABTYPE(ITYPE)(J:J)
	      IF(CH.EQ.'X') THEN
	        F(J)=(F(J)-LSRDT(K,LSR)%XYZ00(1))/TWF**2
	      ELSEIF(CH.EQ.'Y') THEN
	        F(J)=(F(J)-LSRDT(K,LSR)%XYZ00(2))/TWF**2
	      ENDIF
	    ENDIF
C                    Allow half-mesh extrapolation
          FMIN=LSRDT(K,LSR)%VALMM(1,J)-0.5D0*LSRDT(K,LSR)%DVAL(J)
	    FMAX=LSRDT(K,LSR)%VALMM(2,J)+0.5D0*LSRDT(K,LSR)%DVAL(J)
	    IF(F(J).LT.FMIN.OR.F(J).GT.FMAX) THEN
	      IF(ITYPE.EQ.TYPEXYZ.OR.ITYPE.EQ.TYPERZ) THEN
	        SFAC=0
	      ELSE
	        TFAC=0
	      ENDIF
	      GOTO 800
	    ENDIF
	    P1(J)=(F(J)-LSRDT(K,LSR)%VALMM(1,J))/LSRDT(K,LSR)%DVAL(J)
	    ID(J)=MAX(0,MIN(LSRDT(K,LSR)%NVAL(J)-2,INT(P1(J))))
	    P1(J)=P1(J)-ID(J)
	    P2(J)=1-P1(J)
220     CONTINUE
	  DO 240 I=1,NDIM
	    II(I)=0
240     CONTINUE
	  II(1)=-1
	  DO 300 J=0,2**NDIM-1
	    II(1)=II(1)+1
	    I=1
	    DO WHILE (II(I).GT.1)
	      II(I)=0
	      I=I+1
	      II(I)=II(I)+1
	    ENDDO
	    N=ID(NDIM)+II(NDIM)
		  IF(NDIM.GE.2) THEN
	      DO 260 L=NDIM-1,1,-1
	        N=N*LSRDT(K,LSR)%NVAL(L)+(ID(L)+II(L))
260         CONTINUE
	    ENDIF
	    FF(J)=LSRDT(K,LSR)%PTP(N+1)
	    IF(LNV) THEN
	      DO 280 L=1,NN
	        FNV(J,L)=LSRDT(K,LSR)%PTPNV(L,N+1)
280         CONTINUE 
          ENDIF
300     CONTINUE
        DO 360 J=NDIM,1,-1
	    N=2**(J-1)
	    DO 340 I=0,N-1
	      FF(I)=FF(I)*P2(J)+FF(I+N)*P1(J)
	      IF(LNV) THEN
	        NORM=0
	        DO 320 L=1,NN
	          FNV(I,L)=FNV(I,L)*P2(J)+FNV(I+N,L)*P1(J)
	          NORM=NORM+FNV(I,L)**2
320           CONTINUE
              NORM=SQRT(NORM)
	        DO 330 L=1,NN
	          FNV(I,L)=FNV(I,L)/NORM
330           CONTINUE
            ENDIF
340       CONTINUE
360     CONTINUE
        FF(0)=MAX(0D0,FF(0))
        IF(ITYPE.EQ.TYPEXYZ.OR.ITYPE.EQ.TYPERZ) THEN
	    SFAC=SFAC*FF(0)
	  ELSE
          TFAC=TFAC*FF(0)
	  ENDIF
	  IF(LNV) THEN
	    IF(NN.EQ.3) THEN
            DO 380 L=1,NN
	        VECN(L)=FNV(0,L)
380         CONTINUE
          ELSE
	      VECN(3)=FNV(0,2)
		  IF(.NOT.CALR) THEN
	        R=SQRT(TXYZ(1)**2+TXYZ(2)**2)
	        CALR=.TRUE.
	      ENDIF
	      IF(R.EQ.0) THEN
	        VECN(1)=0
	        VECN(2)=0
	      ELSE
	        VECN(1)=FNV(0,1)*TXYZ(1)/R
	        VECN(2)=FNV(0,1)*TXYZ(2)/R
	      ENDIF
	    ENDIF
        ENDIF
400   CONTINUE
      IRTN=0
	IF(LNV) IRTN=1
      RETURN
800   IRTN=10
      RETURN
	END
