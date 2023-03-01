      SUBROUTINE PRBLMAG(BLNAME,P00,LCOMB,FILE,MSGFL,IRTN)
	USE BEAMLN
	IMPLICIT NONE
	CHARACTER(*) BLNAME
	INTEGER LCOMB,FILE,MSGFL,IRTN
	REAL(8) P00
	INTEGER IDBL,JJ,JJ1,ISTAT,J,I,I1,IREP,TYP,LNEXT,N,NCMB,NC
	REAL(8) APERT1(2),X1,X2
	CHARACTER(140) TEXT
	INTEGER MAGTYPE
	REAL(8) PI/3.141592653589793238D0/,CVEL/2.99792458D8/
	CHARACTER(6) TTL(5)/'Marker','Drift','Bend','QUAD','Comb.B'/
	INTEGER, ALLOCATABLE:: ID(:),NCOMB(:),NCOUNT(:)
C   IREP 1: marker, 2: drift, 3: bend, 4: quad, 5: combined bend,

	ALLOCATE(ID(NMAG),NCOMB(NMAG),NCOUNT(NMAG),STAT=ISTAT)
	IF(ISTAT.NE.0) GOTO 900
	WRITE(FILE,100) BLNAME
100   FORMAT(' *** Magnets in the Beamline ',A,' ***',/,
     %    '      [ ]: number of appearance')
	DO IREP=1,5
	  N=0
	  CALL STARTBL(BLNAME,IDBL,APERT1)
	  IF(IDBL.LE.0) GOTO 910
200	  CALL NEXTMAG(JJ,APERT1)
220     IF(JJ.LT.0) THEN
	    GOTO 930
	  ELSEIF(JJ.EQ.0) THEN
		  GOTO 300
	  ENDIF
	  TYP=MAGTYPE(MAG(JJ))
	  NCMB=1
	  LNEXT=0
	  IF(LCOMB.NE.0.AND.TYP.GE.3.AND.IREP.GE.3.AND.
     %    ABS(MAG(JJ)%TEDGE(1)+MAG(JJ)%TEDGE(2)).LE.1D-8) THEN
C            note: bend magnet can be combined only if entrance and 
C            exit edge angles compensate each other.
	    LNEXT=1
240	    CALL NEXTMAG(JJ1,APERT1)
          IF(JJ1.LT.0) THEN
	      JJ=JJ1
	      GOTO 930
          ELSEIF(JJ1.GT.0) THEN
	      IF(JJ1.EQ.JJ) THEN
	        NCMB=NCMB+1
				  GOTO 240
	      ELSEIF(MAGTYPE(MAG(JJ1)).EQ.1) THEN
				  GOTO 240
	      ENDIF
	    ENDIF
	    GOTO 250
	  ENDIF
250	  IF(TYP.NE.IREP) GOTO 280
	  I1=0
	  IF(N.NE.0) THEN
	    DO I=1,N
	      IF(ID(I).EQ.JJ.AND.NCOMB(I).EQ.NCMB) THEN
	        I1=I
	        EXIT
	      ENDIF
	    ENDDO
	  ENDIF
	  IF(I1.EQ.0) THEN
	    N=N+1
	    I1=N
	    ID(I1)=JJ
	    NCOMB(I1)=NCMB
	    NCOUNT(I1)=0
	  ENDIF
	  NCOUNT(I1)=NCOUNT(I1)+1
280	  IF(LNEXT.NE.0) THEN
	    JJ=JJ1
		  GOTO 220
	  ENDIF
	  GOTO 200
300     IF(N.EQ.0) CYCLE
        WRITE(FILE,310) TTL(IREP)
310     FORMAT(' --- ',A,' ---')
	  IF(IREP.EQ.1) THEN
	    WRITE(FILE,330) (MAG(ID(I))%NAME,NCOUNT(I),I=1,N)
330       FORMAT(5(1X,A,'[',I3,'] '))
        ELSEIF(IREP.EQ.2) THEN
	    WRITE(FILE,340) (MAG(ID(I))%NAME,NCOUNT(I),
     %       MAG(ID(I))%LENGTH%X,I=1,N)
340       FORMAT(3(' name           L(m)    '),/,
     %        3(1X,A,'[',I3,']',F8.4,2X))
        ELSE
          TEXT=' name             L(m)   ang(rad)   K1(1/m) '//
     %         '   edge(rad,rad)  rot(deg)'
	    NC=70
	    IF(P00.NE.0) THEN
	      TEXT(NC+1:NC+16)="  B(T)   B'(T/m)"
	      NC=NC+16
	    ENDIF
	    WRITE(FILE,'(A)') TEXT(1:NC)
	    DO I=1,N
	      IF(NCOMB(I).EQ.1) THEN
              TEXT=MAG(ID(I))%NAME
	      ELSE
	        WRITE(TEXT,'(I1,"*",A)') NCOMB(I),MAG(ID(I))%NAME
	      ENDIF
	      NC=MAXMAGNAME+2
		    WRITE(TEXT(NC+1:),360) NCOUNT(I),
     %        MAG(ID(I))%LENGTH%X*NCOMB(I),
     %        MAG(ID(I))%ANGLE*NCOMB(I),MAG(ID(I))%K1%X*NCOMB(I),
     %        (MAG(ID(I))%TEDGE(J),J=1,2),
     %        MAG(ID(I))%ROTATE*180/PI
360         FORMAT('[',I3,']',F8.4,F10.6,F10.5,2F10.6,F6.1)
            NC=NC+59
	      IF(P00.NE.0.AND.MAG(ID(I))%LENGTH%X.NE.0) THEN
	        X1=P00*MAG(ID(I))%ANGLE/(CVEL*MAG(ID(I))%LENGTH%X)
	        X2=P00*MAG(ID(I))%K1%X/(CVEL*MAG(ID(I))%LENGTH%X)
	        WRITE(TEXT(NC+1:NC+16),370) X1,X2
370           FORMAT(F8.5,F8.3)
              NC=NC+16
	      ENDIF
	      WRITE(FILE,'(1X,A)') TEXT(1:NC)
          ENDDO
        ENDIF
	ENDDO
	IRTN=0
	GOTO 1000

900   IRTN=1000
	WRITE(MSGFL,905)
905   FORMAT(' (SUBR.PRBLMAG) Allocation error.')
	RETURN
910	IRTN=1001
	WRITE(MSGFL,915) BLNAME
915   FORMAT(' (SUBR.PRBLMAG) Beamline "',A,'" does not exist.')
      GOTO 1000
930	IRTN=1003
	WRITE(MSGFL,935) JJ
935   FORMAT(' (SUBR.PRBLMAG) Call NEXTMAG error. code=',I3)
      GOTO 1000
1000  DEALLOCATE(ID,NCOMB,NCOUNT,STAT=ISTAT)
      RETURN
	END

	FUNCTION MAGTYPE(M)
	USE BEAMLN
	IMPLICIT NONE
	INTEGER MAGTYPE
	TYPE(MAGNET) M
	IF(M%K1%X.NE.0) THEN
	  IF(M%ANGLE.NE.0) THEN
	    MAGTYPE=5
	  ELSE
	    MAGTYPE=4
	  ENDIF
	ELSEIF(M%ANGLE.NE.0) THEN
	  MAGTYPE=3
	ELSEIF(M%LENGTH%X.NE.0) THEN
	  IF(M%ROTATE.NE.0) THEN
	    MAGTYPE=4
	  ELSE
	    MAGTYPE=2
	  ENDIF
	ELSE
	  MAGTYPE=1
	ENDIF
	RETURN
	END