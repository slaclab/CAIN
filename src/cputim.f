      SUBROUTINE CPUTIM(NAME,K)
C  Measure lap time
C    NAME:  character string defining a stack of lap time
C           accumulation. (e.g., a subroutine name)
C           (length <= 12 characters)
C    K   :  integer. 1 or 2 or 3.
C  CALL CPUTIM(name,1)   (Re)start clock for the stack "name"
C  CALL CPUTIM(name,2)   Add the time increment since the
C           last call of the same stack with K=1.
C  CALL CPUTIM(name,3)   Print the stack "name"
C                        (Does not reset the clock)
C  CALL CPUTIM('  ',3)   Print out the contents of all the
C           stacks.
C  CALL CPUTM2(name,T)   Return the time T for the stack "name"
C                        Real*8. sec. (Does not reset the clock)
C  CALL CPUTIMD(name,K,n)  For debug. Same as CPUTIM except that
C                        NCALL is returned by n. (only when
C                        IDEBUG>=2)
C Clock subroutine
C    call clock1(t):  returns cputime in seconds (t:real*8)
C
      IMPLICIT NONE
      INTEGER MN,K
      PARAMETER (MN=200)
      CHARACTER*(*) NAME
      INTEGER IENT,I,KK,NCAL1,NCALD
      INTEGER NCAL(MN),ON(MN),INAME
      REAL*8 TT,T1,TIM(MN),TIM0(MN),TEMP(MN)
      CHARACTER*12 NAM(MN)
      INCLUDE 'include/ctrlcm.h'
      INTEGER NCALL/0/,NN
      SAVE NCALL,NN,TIM,TIM0,NAM,NCAL,ON
C
      IENT=1
      KK=K
      GOTO 100
      ENTRY CPUTIMD(NAME,K,NCALD)
      IENT=2
      KK=K
      GOTO 100
      ENTRY CPUTM2(NAME,TT)
      IENT=3
      KK=3
      TT=0
 100  IF(NCALL.EQ.0) THEN
        DO 200 I=1,MN
          NCAL(I)=0
          TIM(I)=0
          TIM0(I)=0
          ON(I)=0
 200    CONTINUE
        NN=0
      ENDIF
      NCALL=NCALL+1
      IF(KK.LE.0.OR.KK.GE.4) RETURN
      I=INAME(NAME,NN,NAM,MN)
C       I=-1: name=' '
C       I=0 : new name
C       I>=1:  name = NAM(I)
      IF(I.LT.0.AND.KK.EQ.3) GOTO 400
      IF(I.LT.0) RETURN
      IF(I.EQ.0) THEN
        IF(KK.EQ.1) THEN
          NN=NN+1
          I=NN
          NAM(I)=NAME
        ELSE
          RETURN
        ENDIF
      ENDIF
      IF(IDEBUG.GE.2) THEN
        IF(KK.EQ.1) THEN
          WRITE(MSGFL,220) NAME,NCAL(I)+1
 220      FORMAT(' Call   "',A,'".  Ncall=',I10)
        ELSEIF(KK.EQ.2) THEN
          WRITE(MSGFL,240) NAME,NCAL(I)+1
 240      FORMAT(' Return "',A,'".  Ncall=',I10)
        ENDIF
        IF(IENT.EQ.2) NCALD=NCAL(I)+1
      ENDIF
      IF(KK.EQ.1) THEN
        CALL CLOCK1(TIM0(I))
        ON(I)=1
      ELSE
        IF(ON(I).EQ.1) THEN
          CALL CLOCK1(T1)
          T1=TIM(I)+(T1-TIM0(I))
        ENDIF
        IF(KK.EQ.2) THEN
          IF(ON(I).EQ.1) THEN
            TIM(I)=T1
            NCAL(I)=NCAL(I)+1
            ON(I)=0
          ENDIF
        ELSE
          IF(ON(I).EQ.1) THEN
            NCAL1=NCAL(I)+1
          ELSE
            T1=TIM(I)
            NCAL1=NCAL(I)
          ENDIF
          IF(IENT.EQ.1.OR.IENT.EQ.2) THEN
            WRITE(MSGFL,300) NAM(I),T1,NCAL1
 300        FORMAT(' ---- CPU time "',A,'"  ',F10.3,' sec',
     %         ' Ncall',I10)
          ELSE
            TT=T1
          ENDIF
        ENDIF
      ENDIF
      RETURN
 400  IF(NN.EQ.0) RETURN
C       add pending ones
      CALL CLOCK1(T1)
      DO 420 I=1,NN
        IF(ON(I).EQ.1) THEN
          TEMP(I)=TIM(I)+(T1-TIM0(I))
        ELSE
          TEMP(I)=TIM(I)
        ENDIF
 420  CONTINUE
      WRITE(MSGFL,440) (NAM(I),TEMP(I),NCAL(I)+ON(I),I=1,NN)
 440  FORMAT(' ---- CPU time (seconds, # of calls) ----',/,
     % (3(2X,A,F9.3,I9,:)))
      RETURN
      END
C--------------------------------------
      FUNCTION INAME(NAME,NN,NAM,MN)
      IMPLICIT NONE
      INTEGER NN,MN,INAME
      CHARACTER*(*) NAME,NAM(MN)
      INTEGER I1
C
      IF(NAME.EQ.' ') THEN
        INAME=-1
      ELSE
        IF(NN.EQ.0) THEN
          INAME=0
        ELSE
          DO 200 I1=1,NN
            IF(NAME.EQ.NAM(I1)) THEN
              INAME=I1
              RETURN
            ENDIF
 200      CONTINUE
          INAME=0
        ENDIF
      ENDIF
      RETURN
      END
