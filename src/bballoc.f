      SUBROUTINE BBALLOC(MBBXY1,IRTN)
	USE BBCOM
	USE BBPKCM
	IMPLICIT NONE
	INTEGER MBBXY1,IRTN
	INTEGER,PARAMETER:: MEX=11
	INTEGER I,M,N

	N=2
	M=MEX
	DO I=1,MEX-1
	  M=I
	  IF(N.GE.MBBXY1) EXIT
	  N=2*N
	ENDDO
	MBBXY1=N
	MXY=MBBXY1
	MMX=MBBXY1
	MMY=MBBXY1

	ALLOCATE(BBQ(MXY**2*2),BBWORK((2*MXY)**2),STAT=IRTN)
	IF(IRTN.NE.0) RETURN

	ALLOCATE(KER(2*MMX,2*MMY),QF(2*MMX,2*MMY),PHIF(2*MMX,2*MMY),
     %  PHI(MMX,MMY,2),DPHI(3,MMX,MMY,2),COULF(2,(1+MMX)*(1+MMY)),
     %  QWORK(8*MMX*MMY),STAT=IRTN)
      IF(IRTN.NE.0) THEN
	  DEALLOCATE(BBQ,BBWORK,STAT=IRTN)
	  RETURN
	ENDIF
	RETURN
	END

	SUBROUTINE BBDEALLOC
	USE BBCOM
	USE BBPKCM
	IMPLICIT NONE
	INTEGER ISTAT

	DEALLOCATE(BBQ,BBWORK,STAT=ISTAT)

	DEALLOCATE(KER,QF,PHIF,PHI,DPHI,COULF,QWORK,STAT=ISTAT)

	RETURN
	END

