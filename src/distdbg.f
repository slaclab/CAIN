	SUBROUTINE DISTDBG(K,X,XMAX)
C  Plot a histogram of any parameter for debug purpose
C  When you want to make a hitogram of the value X (assume >=0),
C  estimate the maximum possible X=Xmax (over estimation)
C  and call this subroutine as
C        CALL DISTDBG(k,X,Xmax)
C  1<=k<=10. Different k makes different histogram.
C  (Xmax at the first call for eack k is stored and is not used 
C  at later calls)
C  Then, at the end (subroutine STOPCAIN) its histogram
C  will be generated on file distdbg.tdr.
	IMPLICIT NONE
	INTEGER, PARAMETER:: MBIN=100000, MK=10
	TYPE BINCOUNT
	  INTEGER NCALL
	  REAL(8) XMAX
	  INTEGER, POINTER:: NN(:)
	END TYPE BINCOUNT
	TYPE(BINCOUNT) BIN(MK)

	INTEGER K
	REAL(8) X,XMAX
	
	INTEGER K1
	REAL(8) DX
	INTEGER NCALL/0/
	SAVE NCALL,BIN
	INTEGER I,IMAX,NMAX,II,J,MERGE,NEX
	
	IF(NCALL.EQ.0) THEN
	  DO K1=1,MK
	    BIN(K1)%NCALL=0
	  ENDDO
	ENDIF
	IF(K.GT.0) THEN
	  IF(K.GT.MK) RETURN
	  NCALL=NCALL+1
	  IF(BIN(K)%NCALL.EQ.0) THEN
	    ALLOCATE(BIN(K)%NN(0:MBIN+1))
	    BIN(K)%NN=0
	    BIN(K)%XMAX=XMAX
	  ENDIF
	  BIN(K)%NCALL=BIN(K)%NCALL+1
	  IF(X.GE.0) THEN
	    IF(X.GT.BIN(K)%XMAX) THEN
	      I=MBIN+1
	    ELSE
	      I=MAX(1,MIN(MBIN,INT(X/BIN(K)%XMAX*MBIN)+1))
	    ENDIF
	  ELSE
	    I=0
	  ENDIF
	  BIN(K)%NN(I)=BIN(K)%NN(I)+1
	ELSE
	  IF(NCALL.EQ.0) RETURN
	  OPEN(98,file='distdbg.tdr')
	  DO K1=1,MK
	    IF(BIN(K1)%NCALL.EQ.0) CYCLE
	    WRITE(98,100) K1,BIN(K1)%NCALL,BIN(K1)%NN(0),
     %       BIN(K1)%NN(MBIN+1)
100       FORMAT('(  K=',I2,'  NCALL=',I8,'  out side range ',2I8)
	    NMAX=0
	    DO I=1,MBIN
	      IF(BIN(K1)%NN(I).NE.0) IMAX=I
	      NMAX=MAX(NMAX,BIN(K1)%NN(I))
	    ENDDO
	    IF(NMAX.EQ.0) CYCLE
	    DX=BIN(K1)%XMAX/MBIN
	    IF(NMAX.LE.BIN(K1)%NCALL/20.OR.IMAX.GE.200) THEN
	      MERGE=MIN(1000,MAX(BIN(K1)%NCALL/(20*NMAX)+2,IMAX/200+2))
	      II=0
	      NMAX=0
	      DO I=1,IMAX,MERGE
	        DO J=1,MIN(MERGE-1,MBIN-I)
	          BIN(K1)%NN(I)=BIN(K1)%NN(I)+BIN(K1)%NN(I+J)
	        ENDDO
	        II=II+1
	        BIN(K1)%NN(II)=BIN(K1)%NN(I)
	        NMAX=MAX(NMAX,BIN(K1)%NN(II))
	      ENDDO
	      IMAX=II
	      DX=DX*MERGE
	    ENDIF
	    NEX=1
	    IF(IMAX*DX.GE.1D3.OR.IMAX*DX.LE.1D-3) THEN
	      NEX=INT(LOG10(IMAX*DX)+1000)-1000
	      DX=DX/10D0**NEX
	    ENDIF
	    WRITE(98,200) IMAX*DX,NINT(1.05*NMAX),K1
200       FORMAT(' NEWFRAME; SET FONT DUPLEX',/,
     %    ' SET LIMIT X 0 ',1PD10.3,' Y 1 ',I8,/,
     %    ' SET SCALE Y LOG',/,
     %    ' TITLE TOP SIZE 2.0 ',1H',I2,'th Parameter',1H')
	    IF(NEX.NE.1) THEN
	      WRITE(98,220) NEX
220         FORMAT(' TITLE 6.0 1.6 SIZE 1.9 ',1H','102',I3,'3',1H',/,
     %             ' CASE ',                  1H','  X',3X,'X',1H')
	    ENDIF
	    WRITE(98,240) ((I-0.5D0)*DX,BIN(K1)%NN(I),I=1,IMAX)
240       FORMAT(4(1PD11.4,I6,';'))
          WRITE(98,260)
260       FORMAT(' JOIN 1 ')
          WRITE(98,300) '3.0','All',BIN(K1)%NCALL
          WRITE(98,300) '5.0','X<0',BIN(K1)%NN(0)
	    WRITE(98,300) '7.0','X>Xmax',BIN(K1)%NN(MBIN+1)
300       FORMAT(' TITLE ',A,' 0.6 SIZE 1.6 ',1H',A,': ',I8,1H')
          DEALLOCATE(BIN(K1)%NN)
        ENDDO
        CLOSE(98)
      ENDIF
	RETURN
	END
