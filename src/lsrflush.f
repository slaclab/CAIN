	SUBROUTINE LSRFLUSH(LSR,REGION,NZ,ZZ,PTOT)
C  Numerical integration of laser flush energy.
C  Used only for parameter printing. Not used for tracking.
C  Must be called before any Lorentz transformation after laser definition.
C  Output: PTOT in Joule
	IMPLICIT NONE
	INTEGER LSR,NZ
	REAL*8 REGION(2,0:3),ZZ(NZ),PTOT(NZ)
	INTEGER IZ,IX,IY,IT,IX1,IY1,IT1,ISTAT,ND
	REAL*8 TXYS(0:3),Z,PD,V(3,3),PD0,OMG,SUM,F1,DT,DX,DY,
     %   FACT,FACX,FACY
	INTEGER MM,MAXDIV
	PARAMETER (MM=50,MAXDIV=10)
	REAL*8, ALLOCATABLE :: FF(:,:,:)
	INCLUDE 'include/ctrlcm.h'
	INCLUDE 'include/cnstcm.h'
	
	CALL CPUTIM('LSRFLUSH',1)
	ALLOCATE(FF(0:MM,0:MM,0:MM), STAT=ISTAT)
	IF(ISTAT.NE.0) THEN
	  IF(MSGLVL.GE.0) THEN
	    WRITE(MSGFL,100)
100       FORMAT(' (SUBR.LSRFLUSH) Allocation error.')
        ENDIF
	  GOTO 600
	ENDIF
	DT=(REGION(2,0)-REGION(1,0))/MM
	DX=(REGION(2,1)-REGION(1,1))/MM
	DY=(REGION(2,2)-REGION(1,2))/MM
	DO 550 IZ=1,NZ
	  IF(NZ.EQ.1) THEN
	    Z=0.5D0*(REGION(1,3)+REGION(2,3))
	  ELSE
	    Z=REGION(1,3)+(REGION(2,3)-REGION(1,3))/(NZ-1)*(IZ-1)
	  ENDIF
	  ZZ(IZ)=Z
C   integrate along z=const
C   start from a coarse mesh
	  SUM=0
	  DO 300 IT=0,MM
	    DO 280 IX=0,MM
	      DO 260 IY=0,MM
	        TXYS(0)=REGION(1,0)+DT*IT+Z
	        TXYS(1)=REGION(1,1)+DX*IX
	        TXYS(2)=REGION(1,2)+DY*IY
	        TXYS(3)=Z
	        CALL LSRCOORD(LSR,TXYS,1)
	        CALL LSRGEO(LSR,TXYS,FF(IT,IX,IY),PD0,V,OMG,0)
	        SUM=SUM+FF(IT,IX,IY)
260         CONTINUE
280       CONTINUE
300     CONTINUE
	  IF(SUM.LE.0) GOTO 580
	  PTOT(IZ)=0
	  DO 500 IT=0,MM-1
	    DO 480 IX=0,MM-1
	      DO 460 IY=0,MM-1
	        F1=(FF(IT+1,IX+1,IY+1)+FF(IT,IX+1,IY+1)+FF(IT+1,IX,IY+1)
     %           +FF(IT,IX,IY+1)+FF(IT+1,IX+1,IY)+FF(IT,IX+1,IY)
     %           +FF(IT+1,IX,IY)+FF(IT,IX,IY))/8D0
	        ND=MAX(1,MIN(MAXDIV,NINT(SQRT(F1/SUM*MM**3))))
	        IF(ND.GE.2) THEN
	          F1=0
	          DO 440 IT1=0,ND
	            FACT=1D0
	            IF(IT1.EQ.0.OR.IT1.EQ.ND) FACT=0.5D0
	            DO 420 IX1=0,ND
	              FACX=1D0
	              IF(IX1.EQ.0.OR.IX1.EQ.ND) FACX=0.5D0
	              DO 400 IY1=0,ND
	                TXYS(0)=REGION(1,0)+DT*(IT*ND+IT1)/ND+Z
	                TXYS(1)=REGION(1,1)+DX*(IX*ND+IX1)/ND
	                TXYS(2)=REGION(1,2)+DY*(IY*ND+IY1)/ND
	                TXYS(3)=Z
	                FACY=1D0
	                IF(IY1.EQ.0.OR.IY1.EQ.ND) FACY=0.5D0
	                CALL LSRCOORD(LSR,TXYS,1)
	                CALL LSRGEO(LSR,TXYS,PD,PD0,V,OMG,0)
	                F1=F1+PD*FACT*FACX*FACY
400                 CONTINUE
420               CONTINUE
440             CONTINUE
	          F1=F1/ND**3
	        ENDIF
	        PTOT(IZ)=PTOT(IZ)+F1
460         CONTINUE
480       CONTINUE
500     CONTINUE
	  PTOT(IZ)=PTOT(IZ)*DT*DX*DY/CVEL
c	  call tst(mm,ff,iz)
550   CONTINUE

580   DEALLOCATE(FF, STAT=ISTAT)
600	CALL CPUTIM('LSRFLUSH',2)
	RETURN
	END
	
	subroutine tst(mm,ff,iz)
	implicit none
	integer mm,iz
	real*8 ff(0:mm,0:mm,0:mm)
	real*8 aaa(0:100)
	integer it,ix,iy

	do 200 it=0,mm
	  aaa(it)=0
	  do 180 ix=0,mm
	    do 160 iy=0,mm
            aaa(it)=aaa(it)+ff(it,ix,iy)
160       continue
180     continue
200   continue
	write(6,300) iz
300   format(' IZ=',i5)
      write(6,320) (aaa(it),it=0,mm)
320   format(1p5d11.3)
	return
	end
