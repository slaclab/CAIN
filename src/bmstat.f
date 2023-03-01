      SUBROUTINE BMSTAT(N1,N20,LNP,INCP,LLOST,FILE)
	USE BEAMCM
C  Print beam statistics for N1<=N<=N20.
C  If N20=0, adopt N20=NP.
C  If LNP=0, print only the number of particles.
C         1, standard (average, rms)
C         2, plus max,min.
C  If INCP>0, print only the incoherent pair particles
C             Otherwise, normal particles only. 
C  If LLOST>0, print only the lost particles detected by
C         loss monitor (LOST=2)
      IMPLICIT NONE
      INTEGER N1,N20,LNP,INCP,LLOST,FILE
C      INCLUDE 'include/beamcm.h'
      INTEGER N2,N,I,L,K,J,LSPIN,NC,IFFLAG
      INTEGER NPP(3,2),LNPP(2)
      REAL*8 ANN(3,2),TXYSAV(0:3,3,2),TXYSRM(0:3,3,2),
     %    EPAV(0:3,3,2),EPRM(0:3,3,2),SPINAV(0:3,3,2),
     %    TXYSMM(2,0:3,3,2),EPMM(2,0:3,3,2)
      CHARACTER*8 KIN(3)/'photon','electron','positron'/
      CHARACTER*5 LLRR(2)/'Right','Left'/
      CHARACTER*25 LSPN
      CHARACTER*1024 TEXT
      REAL*8 FLMX/1D60/,FLMN/-1D60/
C
      CALL CPUTIM('BMSTAT',1) 
      IF(N20.EQ.0) THEN
        N2=NP
      ELSE
        N2=N20
      ENDIF
      IF(N2-N1.LT.0) GOTO 900
      IF(INCP.EQ.0) THEN
        TEXT=' '
        NC=1
      ELSE
        TEXT=' incoherent '
        NC=12
      ENDIF
	IF(LLOST.NE.0) THEN
	  TEXT(NC+1:NC+5)='lost '
	  NC=NC+5
	ENDIF
      LSPIN=IFFLAG('SPIN')
      DO 230 L=1,2
        DO 220 K=1,3
          NPP(K,L)=0
          ANN(K,L)=0
          IF(LNP.GE.1) THEN
            DO 200 I=0,3
              TXYSAV(I,K,L)=0
              TXYSRM(I,K,L)=0
              EPAV(I,K,L)=0
              EPRM(I,K,L)=0
              TXYSMM(1,I,K,L)=FLMX
              TXYSMM(2,I,K,L)=FLMN
              EPMM(1,I,K,L)=FLMX
              EPMM(2,I,K,L)=FLMN
 200        CONTINUE
            IF(LSPIN.NE.0) THEN
              DO 210 I=1,3
                SPINAV(I,K,L)=0
 210          CONTINUE
            ENDIF
          ENDIF
 220    CONTINUE
 230  CONTINUE
      DO 270 N=N1,N2
	  IF(LLOST.EQ.0) THEN
	    IF(LOST(N).NE.0) GOTO 270
	  ELSE
	    IF(LOST(N).NE.2) GOTO 270
	  ENDIF
        IF(INCP.EQ.0) THEN
          IF(PNAME(N).NE.'    ') GOTO 270
        ELSE
          IF(PNAME(N)(1:1).NE.'I') GOTO 270
        ENDIF
        K=KIND(N)
        L=1
        IF(EP(3,N).LT.0) L=2
        NPP(K,L)=NPP(K,L)+1
        ANN(K,L)=ANN(K,L)+WGT(N)
        IF(LNP.GE.1) THEN
          DO 240 I=0,3
            TXYSAV(I,K,L)=TXYSAV(I,K,L)+WGT(N)*TXYS(I,N)
            EPAV(I,K,L)=EPAV(I,K,L)+WGT(N)*EP(I,N)
 240      CONTINUE
          IF(LNP.GE.2) THEN
            DO 250 I=0,3
              TXYSMM(1,I,K,L)=MIN(TXYSMM(1,I,K,L),TXYS(I,N))
              TXYSMM(2,I,K,L)=MAX(TXYSMM(2,I,K,L),TXYS(I,N))
              EPMM(1,I,K,L)=MIN(EPMM(1,I,K,L),EP(I,N))
              EPMM(2,I,K,L)=MAX(EPMM(2,I,K,L),EP(I,N))
 250        CONTINUE
          ENDIF
          IF(LSPIN.NE.0) THEN
            DO 260 I=1,3
              SPINAV(I,K,L)=SPINAV(I,K,L)+WGT(N)*SPIN(I,N)
 260        CONTINUE
          ENDIF
        ENDIF
 270  CONTINUE
      DO 290 L=1,2
        LNPP(L)=0
        DO 280 K=1,3
          IF(NPP(K,L).NE.0) LNPP(L)=1
 280    CONTINUE
 290  CONTINUE
C
      IF(LNP.LE.0) GOTO 600
      DO 420 L=1,2
        IF(LNPP(L).EQ.0) GOTO 420
        DO 400 K=1,3
          IF(ANN(K,L).NE.0) THEN
            DO 360 I=0,3
              TXYSAV(I,K,L)=TXYSAV(I,K,L)/ANN(K,L)
              EPAV(I,K,L)=EPAV(I,K,L)/ANN(K,L)
 360        CONTINUE
            IF(LSPIN.NE.0) THEN
              DO 380 I=1,3
                SPINAV(I,K,L)=SPINAV(I,K,L)/ANN(K,L)
 380          CONTINUE
              SPINAV(0,K,L)=SQRT(SPINAV(1,K,L)**2
     %             +SPINAV(2,K,L)**2+SPINAV(3,K,L)**2)
            ENDIF
          ENDIF
 400    CONTINUE
 420  CONTINUE
C
      DO 450 N=N1,N2
	  IF(LLOST.EQ.0) THEN
	    IF(LOST(N).NE.0) GOTO 450
	  ELSE
	    IF(LOST(N).NE.2) GOTO 450
	  ENDIF
        IF(INCP.EQ.0) THEN
          IF(PNAME(N).NE.'    ') GOTO 450
        ELSE
          IF(PNAME(N)(1:1).NE.'I') GOTO 450
        ENDIF
        K=KIND(N)
        L=1
        IF(EP(3,N).LT.0) L=2
        DO 440 I=0,3
          TXYSRM(I,K,L)=TXYSRM(I,K,L)
     %       +WGT(N)*(TXYS(I,N)-TXYSAV(I,K,L))**2
          EPRM(I,K,L)=EPRM(I,K,L)
     %       +WGT(N)*(EP(I,N)-EPAV(I,K,L))**2
 440    CONTINUE
 450  CONTINUE
C
      DO 480 L=1,2
        IF(LNPP(L).EQ.0) GOTO 480
        DO 470 K=1,3
          IF(ANN(K,L).NE.0) THEN
            DO 460 I=0,3
              TXYSRM(I,K,L)=SQRT(TXYSRM(I,K,L)/ANN(K,L))
              EPRM(I,K,L)=SQRT(EPRM(I,K,L)/ANN(K,L))
 460        CONTINUE
          ENDIF
 470    CONTINUE
 480  CONTINUE
C
      DO 580 L=1,2
        IF(LNPP(L).EQ.0) GOTO 580
        DO 560 K=1,3
          IF(ANN(K,L).EQ.0) GOTO 560
          WRITE(FILE,500) LLRR(L),TEXT(1:NC),KIN(K),NPP(K,L),
     %           ANN(K,L)
 500      FORMAT(' +++',A,'-going',A,A,
     %      I10,' macro particles   ',1PD10.3,' real')
          WRITE(FILE,510) (TXYSAV(I,K,L),I=0,3),
     %       (TXYSRM(I,K,L),I=0,3)
 510      FORMAT(' Average (t,x,y,s)',T30,1P4D10.3,' m',/,
     %      ' R.m.s.  (t,x,y,s)',T30,1P4D10.3,' m')
          IF(LNP.GE.2) THEN
            WRITE(FILE,520) ((TXYSMM(J,I,K,L),I=0,3),J=1,2)
 520        FORMAT(' Min     (t,x,y,s)',T30,1P4D10.3,' m',/,
     %       ' Max     (t,x,y,s)',T30,1P4D10.3,' m')
          ENDIF
          WRITE(FILE,530) (EPAV(I,K,L),I=0,3),(EPRM(I,K,L),I=0,3)
 530      FORMAT(' Average (En,Px,Py,Ps)',T30,1P4D10.3,' eV',/,
     %      ' R.m.s.  (En,Px,Py,Ps)',T30,1P4D10.3,' eV')
          IF(LNP.GE.2) THEN
            WRITE(FILE,540) ((EPMM(J,I,K,L),I=0,3),J=1,2)
 540        FORMAT(' Min     (En,Px,Py,Ps)',T30,1P4D10.3,' eV',/,
     %      ' Max     (En,Px,Py,Ps)',T30,1P4D10.3,' eV')
          ENDIF
          IF(LSPIN.NE.0) THEN
            LSPN='Spin    (|S|,Sx,Sy,Ss)'
            IF(K.EQ.1) LSPN='Stokes (|Xi|,Xi1,Xi2,Xi3)'
            WRITE(FILE,550) LSPN,(SPINAV(I,K,L),I=0,3)
 550        FORMAT(' ',A,T30,0P4F10.5)
          ENDIF
 560    CONTINUE
 580  CONTINUE
      GOTO 1000
C
 600  IF(LNPP(1).EQ.0.AND.LNPP(2).EQ.0) THEN
        WRITE(FILE,610) TEXT(1:NC)
 610    FORMAT(' +++ No particles under the condition:',/,
     %     '   "',A,'"')
      ELSE
        WRITE(FILE,620) TEXT(1:NC)
 620    FORMAT(' +++ Number of ',A,' particles +++ macro(real) ',
     %     'gamma, e-, e+')
        DO 640 L=1,2
          IF(LNPP(L).NE.0) THEN
            WRITE(FILE,630) LLRR(L),(NPP(K,L),ANN(K,L),K=1,3)
 630        FORMAT(' ',A,'-going  ',3(I8,'(',1PD9.3,')'))
          ENDIF
 640    CONTINUE
      ENDIF
      GOTO 1000
C
 900  WRITE(FILE,905)
 905  FORMAT(' No beam')
      GOTO 1000
C
 1000 CALL CPUTIM('BMSTAT',2)
      RETURN
      END
