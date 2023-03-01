	SUBROUTINE BLOPTICS(LPER,NAM,BAI,BAF,EPI,EPF,IRTN)
C  Store optics of the beamline NAM
C  Input
C    LPER        If =1, periodicity assumed.
C    NAM         beamline name
C    BAI(2,2)    Twiss param and eta at entrance  (bx,ax,by,ay)
C    EPI(4)      Eta at entrance  (ex,epx,ey,epy)
C                (output if LPER>=1)
C  Output
C    BAF(2,2),EPF(4)  Twiss param and eta at exit
C    IRTN     0   normal
C           102   Beamline unstable (when LPER=1)
C           103   Invalid Twiss param at entrance (when LPER=0)
C          1001   Beamline nonexisting
C          1012   nesting level too deep
C          1013   recursive citation of beamline name
C
	USE BEAMLN
	IMPLICIT NONE
	INTEGER LPER,IRTN
	CHARACTER(*) NAM
	REAL(8) BAI(2,2),BAF(2,2),EPI(4),EPF(4)
C       beta, alpha, eta, eta' for x and y, initial and final
      INCLUDE 'include/ctrlcm.h'
	INTEGER II,JJ,NN,N,I,J,IREP,IREP0,IWORK(4),ISTAT
	REAL*8 T(6,6),T0(6,6),S,B1,A1,G1,E1(4),TUNE(2),CMU,SMU,T4(4,4),
     %   WORK(4),APERT1(2)
	CHARACTER(80) ERR
	REAL(8) PI/3.141592653589793238D0/

      CALL CPUTIM('BLOPTICS',1)
	CALL STARTBL(NAM,II,APERT1)
	IF(II.LE.0) GOTO 910
	NN=BL(II)%NEXP
	IF(BL(II)%LTWISS) THEN
	  DEALLOCATE(BL(II)%TWISS,BL(II)%TMAT,STAT=ISTAT)
	  BL(II)%LTWISS=.FALSE.
	ENDIF
	ALLOCATE(BL(II)%TWISS(MTWISS,0:NN),BL(II)%TMAT(6,6,0:NN),
     %  STAT=ISTAT)
	IF(ISTAT.NE.0) GOTO 940
	BL(II)%LTWISS=.TRUE.
	CALL MAGSET(II,ERR)
	IF(ERR.NE.' ') GOTO 950

	IREP0=1
	IF(LPER.EQ.0) IREP0=2
	DO IREP=IREP0,2
	  CALL UNIT8(T0,6,6)
	  IF(IREP.EQ.2) THEN
	    BL(II)%TMAT(1:6,1:6,0)=T0(1:6,1:6)
	    DO J=1,2
	      TUNE(J)=0
	      DO I=1,2
	        BAF(I,J)=BAI(I,J)
	      ENDDO
	    ENDDO
	    DO I=1,4
	      EPF(I)=EPI(I)
	    ENDDO
	    IF(BAI(1,1).LE.0.OR.BAI(1,2).LE.0) GOTO 930
	  ENDIF
	  DO N=0,NN
	    IF(N.EQ.0) GOTO 300
	    JJ=BL(II)%MAGID(N)
C--    compute T-matrix
	    CALL TMATRIX(MAG(JJ)%LENGTH%X,MAG(JJ)%ANGLE,MAG(JJ)%K1%X,
     %       MAG(JJ)%ROTATE,MAG(JJ)%TEDGE,T,IRTN)
	    IF(IRTN.NE.0) THEN
	      IRTN=1009
            IF(IRTN.EQ.1) THEN
	       IF(MSGLVL.GE.0) WRITE(MSGFL,260) MAG(JJ)%NAME(1:MAG(JJ)%NC)
260          FORMAT(' (SUBR.BLOPTICS) Coupled element "',A,'" found. ',
     %          ' CAIN not ready.')
	      ELSEIF(IRTN.GE.100) THEN
	       IF(MSGLVL.GE.0) WRITE(MSGFL,280) MAG(JJ)%NAME(1:MAG(JJ)%NC)
280          FORMAT(' (SUBR.BLOPTICS) BLOPTICS not ready for element "',
     %          A,'"')
	      ENDIF
            GOTO 1000
	    ENDIF
C--     multiply T-matrix or successive calculation of Twiss params
          CALL MATMUL(6,T,6,T0,6,T0,6)
	    IF(IREP.EQ.2) THEN
	      BL(II)%TMAT(1:6,1:6,N)=T0(1:6,1:6)
	      DO J=1,2
	        I=0
	        IF(J.EQ.2) I=2
	        G1=(1+BAF(2,J)**2)/BAF(1,J)
	        B1=T(I+1,I+1)**2*BAF(1,J)-2*T(I+1,I+1)*T(I+1,I+2)*BAF(2,J)
     %            +T(I+1,I+2)**2*G1
	        A1=-T(I+1,I+1)*T(I+2,I+1)*BAF(1,J)
     %          +(1+2*T(I+1,I+2)*T(I+2,I+1))*BAF(2,J)
     %          -T(I+1,I+2)*T(I+2,I+2)*G1
	        TUNE(J)=TUNE(J)+ATAN2(T(I+1,I+2),
     %           T(I+1,I+1)*BAF(1,J)-T(I+1,I+2)*BAF(2,J))/(2*PI)
	        BAF(1,J)=B1
	        BAF(2,J)=A1
	      ENDDO
	      CALL MATVEC(T,6,4,EPF)
	      DO I=1,4
	        EPF(I)=EPF(I)+T(I,6)
	      ENDDO
	    ENDIF
C--    store
300       IF(IREP.EQ.2) THEN
	      DO J=1,2
	        BL(II)%TWISS(1+5*(J-1),N)=BAF(1,J)
	        BL(II)%TWISS(2+5*(J-1),N)=BAF(2,J)
	        BL(II)%TWISS(3+5*(J-1),N)=EPF(2*J-1)
	        BL(II)%TWISS(4+5*(J-1),N)=EPF(2*J)
	        BL(II)%TWISS(5+5*(J-1),N)=TUNE(J)
	      ENDDO
	    ENDIF
	  ENDDO
C--     Solve eigenvalue
	  IF(IREP.EQ.1) THEN
          DO J=1,2
	      I=0
	      IF(J.EQ.2) I=2
	      CMU=0.5D0*(T0(I+1,I+1)+T0(I+2,I+2))
	      IF(ABS(CMU).GE.1D0) THEN
	        IRTN=10
	        GOTO 920
	      ENDIF
	      SMU=SQRT(1-CMU**2)
	      BAI(1,J)=T0(I+1,I+2)/SMU
	      BAI(2,J)=0.5D0*(T0(I+1,I+1)-T0(I+2,I+2))/SMU
C	      TUNE(J)=ATAN2(SMU,CMU)/(2*PI)
	    ENDDO
	    DO I=1,4
	      EPI(I)=-T0(I,6)
	      DO J=1,4
	        T4(I,J)=T0(I,J)
	      ENDDO
	      T4(I,I)=T4(I,I)-1
	    ENDDO
	    CALL DLUDCP(T4,4,4,IWORK,WORK,IRTN)
	    CALL DLUBKS(T4,4,4,IWORK,EPI)
	    CYCLE
	  ENDIF
      ENDDO

      IRTN=0
	GOTO 1000
910   IRTN=1001
	IF(MSGLVL.GE.0) WRITE(MSGFL,915) NAM
915   FORMAT(' (SUBR.BLOPTICS) Beamline "',A,'" does not exist.')
      GOTO 1000
920   IRTN=102
	IF(MSGLVL.GE.0) WRITE(MSGFL,925) NAM
925   FORMAT(' (SUBR.BLOPTICS) Beamline "',A,'" unstable.')
      GOTO 990
930   IRTN=103
	IF(MSGLVL.GE.0) WRITE(MSGFL,935) NAM
935   FORMAT(' (SUBR.BLOPTICS) Invalid Twiss param for beamline "',
     %     A,'".')
      GOTO 990
940   IRTN=1004
	IF(MSGLVL.GE.0) WRITE(MSGFL,945)
945   FORMAT(' (SUBR.BLOPTICS) Memory allocation error.')
      GOTO 990
950   IRTN=1005
	IF(MSGLVL.GE.0) WRITE(MSGFL,'(A)') ERR
      GOTO 990
990   IF(BL(II)%LTWISS) THEN
	  DEALLOCATE(BL(II)%TWISS,BL(II)%TMAT,STAT=ISTAT)
	  BL(II)%LTWISS=.FALSE.
	ENDIF	
1000  CALL CPUTIM('BLOPTICS',2)
      RETURN
	END

	SUBROUTINE PRBLOPTICS(NAM,FILE,IRTN)
C  Print optics of the beamline NAM in file FILE
C  Optics must be calculated by BLOPTICS command
C  Input
C    NAM         beamline name
C    FILE         output file unit number
C  Output
C    IRTN     0   normal
C          1000   File open error
C          1001   Beamline nonexisting
C          1002   Beamline unstable (when LCYC=1)
C          1003   Invalid Twiss para at entrance (when LCYC=0)
C          1011   STARTBL not called yet
C          1012   nesting level too deep
C          1013   recursive citation of beamline name
C
	USE BEAMLN
	IMPLICIT NONE
	INTEGER FILE,IRTN
	CHARACTER(*) NAM
      INCLUDE 'include/ctrlcm.h'
	INTEGER II,JJ,I,N,NN
	REAL*8 APERT1(2)
	CHARACTER(38) TEXT
	REAL(8) PI/3.141592653589793238D0/

	CALL STARTBL(NAM,II,APERT1)
	IF(II.LE.0) GOTO 910
	NN=BL(II)%NEXP
	IF(NN.LE.0) GOTO 920
	IF(.NOT.BL(II)%LTWISS) GOTO 920
	WRITE(FILE,100) NAM
100   FORMAT(" ***** Linear Optics of Beamline ",A," *****",
     %    "  (at magnet exits)",/,
     %    " name    length  ang(rad)  k1   rot(deg)    s(m) ",
     %    "  Bx(m)     Ax    Ex(m)    Ex'     Nux  ",
     %    "  By(m)     Ay    Ey(m)    Ey'     Nuy   T56(m)")
      DO N=0,NN
	  IF(N.EQ.0) THEN
	    TEXT=' (ent)'
        ELSE
	    JJ=BL(II)%MAGID(N)
	    WRITE(TEXT,220) MAG(JJ)%NAME,MAG(JJ)%LENGTH%X,MAG(JJ)%ANGLE,
     %    MAG(JJ)%K1%X,MAG(JJ)%ROTATE*180/PI
220       FORMAT(A8,0PF8.4,0PF8.4,0PF8.4,0PF6.1)
        ENDIF
	  WRITE(FILE,240) TEXT,BL(II)%SBL(N),(BL(II)%TWISS(I,N),I=1,10),
     %    BL(II)%TMAT(5,6,N)
240     FORMAT(A,0PF10.4,2(0PF8.3,0PF8.4,0PF8.4,0PF8.5,0PF8.5),0PF8.5)
      ENDDO

      IRTN=0
	GOTO 1000
910   IRTN=1001
	IF(MSGLVL.GE.0) WRITE(MSGFL,915) NAM
915   FORMAT(' (SUBR.PRBLOPTICS) Beamline "',A,'" does not exist.')
      GOTO 1000
920   IRTN=1002
	IF(MSGLVL.GE.0) WRITE(MSGFL,925) NAM
925   FORMAT(' (SUBR.PRBLOPTICS) Optics for beamline "',A,'" not ',
     %   'ready. Use BLOPTICS command.')
      GOTO 1000
	
1000  RETURN
	END

