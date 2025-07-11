      SUBROUTINE SLVEQM(T1,IS,SS)
	USE BEAMCM
	USE BBCOM
      IMPLICIT NONE
      INTEGER IS
      REAL*8 T1,SS(2)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/beamcm.h'
C      INCLUDE 'include/bbcom.h'
      INCLUDE 'include/cfqedcm.h'
      INCLUDE 'include/cnstcm.h'
      INTEGER L,L2,N,I,NDIV,KIN,IOUT,IRTN1
      REAL*8 XYS0(3),XYS1(3),EP0(0:3),EP1(0:3),
     %   FXYS1(3),FXYS0(3),
     %   DT,DS,SGN,EMASS2,ERXY(2),ER,
     %   BBFLD(3,2),BBFLD0(3,2),BBFL1(3,2),EXTFXY(3),FF(3),
     %   ANOM1,ANOME
      EXTERNAL ANOME
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
      REAL*8 ERMIN0/1D-3/,ERMIN
      REAL*8 dotfld
      REAL*8 fldabs
      REAL*8 fldmax(2)/-1d20,-1d20/
      save fldmax
C      ERMIN: maximum error in x and y in one time step in units
C             of mesh size of on-coming beam (the mesh size of the
C             co-moving beam might be undefined)
C
      CALL CPUTIM('SLVEQM',1)
	IF(EQMERR.GT.0) THEN
        ERMIN=EQMERR
C               Defined by the variable EqmErr
	ELSE
        ERMIN=ERMIN0
C               This seems to be too accurate. 1.0 is normally enough.
	ENDIF
      DS=SS(2)-SS(1)
      EMASS2=EMASS**2
c      print *, " slveqm before do 400 "," fldbe= ", fldbe,
c     &        " zfsgm= ",zfsgm
      DO 400 N=1,NP
c       print *, " slveqm do 400 n= ",n," kind(n)= ",kind(n)," is= ",is,
c     &     " isbin(n)= ",isbin(n)," lbbfin(n)= ",lbbfin(n)
        IF(ISBIN(N).NE.IS) GOTO 400
        IF(LBBFIN(N).NE.0) GOTO 400
        IF(EP(3,N).GE.0) THEN
          L=1
          SGN=1
        ELSE
          L=2
          SGN=-1
        ENDIF
        L2=3-L
c        print *, " slveqm do 400 n= ",n," kind(n)= ",kind(n)," is= ",is,
c     &   " l2= ",l2," bbflg(l2)= ",bbflg(l2)
        IF(BBFLG(L2).EQ.0) GOTO 400
        KIN=KIND(N)
        IF(KIN.EQ.1) THEN
          IF(LCOHP.LE.0) GOTO 400
C    Beam field on photon for coherent-pair calculation
          DT=T1-TXYS(0,N)
          DO 160 I=1,3
            XYS1(I)=TXYS(I,N)+EP(I,N)/EP(0,N)*DT
 160      CONTINUE
          CALL BBKICK0(L,DS,1,TXYS(1,N),EP(0,N),BBFLD0,FF,IOUT,NXY,
     &             BBQ,is)
          CALL BBKICK0(L,DS,1,XYS1,EP(0,N),BBFLD,FF,IOUT,NXY,BBQ,is)
          DO 170 I=1,3
            FLD(I,1,N)=FLD(I,1,N)+0.5D0*(BBFLD0(I,1)+BBFLD(I,1))
            FLD(I,2,N)=FLD(I,2,N)+0.5D0*(BBFLD0(I,2)+BBFLD(I,2))
 170      CONTINUE
          GOTO 690
        ENDIF
        DT=T1-TXYS(0,N)
        DO 180 I=1,3
          XYS0(I)=TXYS(I,N)
 180    CONTINUE
        DO 190 I=0,3
          EP0(I)=EP(I,N)
 190    CONTINUE
        DO 200 I=1,3
          EXTFXY(I)=CHARGE(KIN)*(FLD(I,1,N)
     %      +(EP0(I2(I))*FLD(I3(I),2,N)
     %       -EP0(I3(I))*FLD(I2(I),2,N))/EP0(0))
 200    CONTINUE
C         EXTFXY: force due to external field in V/m
C-- Determine time step size
        CALL BBKICK0(L,DS,KIN,XYS0,EP0,BBFLD0,FF,IOUT,NXY,BBQ,is)
c        write(msgfl,101) n,is,
c     &       sqrt(bbfld0(1,1)**2+bbfld0(2,1)**2)
 101    format(' n= ',i10,' is= ',i10,' |bbfld0|=  ',1pd10.3)
        DO 210 I=1,3
          FXYS0(I)=EXTFXY(I)+FF(I)
          EP1(I)=EP0(I)+FXYS0(I)*DT
 210    CONTINUE
        EP1(0)=SQRT(EMASS2+EP1(1)**2+EP1(2)**2+EP1(3)**2)
        DO 230 I=1,3
          XYS1(I)=XYS0(I)+(EP0(I)/EP0(0)+EP1(I)/EP1(0))/2*DT
 230    CONTINUE
        CALL BBKICK0(L,DS,KIN,XYS1,EP1,BBFLD,FF,IOUT,NXY,BBQ,is)
c        write(msgfl,105) n,is,
c     &       sqrt(bbfld(1,1)**2+bbfld(2,1)**2)
 105    format(' n= ',i10,' is= ',i10,' |bbfld|=  ',1pd10.3)
        DO 240 I=1,3
          FXYS1(I)=EXTFXY(I)+FF(I)
 240    CONTINUE
        DO 250 I=1,2
          ERXY(I)=((FXYS1(I)-FXYS0(I))/EP1(0)*DT**2/4)/BBDXY(I,L2)
 250      CONTINUE
        ER=MAX(ABS(ERXY(1)),ABS(ERXY(2)))
C          Error in one step is proportional to 1/NDIV**3
C          Total error is to 1/NDIV**2
        NDIV=MAX(1,NINT(SQRT(ER/ERMIN)))
c        print *, " n= ",n," is= ",is," ndiv= ",ndiv
        IF(NDIV.EQ.1) THEN
C-- One step is enough
          DO 270 I=1,3
            BBFL1(I,1)=0.5D0*(BBFLD0(I,1)+BBFLD(I,1))
            BBFL1(I,2)=0.5D0*(BBFLD0(I,2)+BBFLD(I,2))
            EP1(I)=EP0(I)+(FXYS0(I)+FXYS1(I))/2*DT
 270      CONTINUE
          EP1(0)=SQRT(EMASS2+EP1(1)**2+EP1(2)**2+EP1(3)**2)
          DO 280 I=1,3
            XYS0(I)=XYS0(I)+(EP0(I)/EP0(0)+EP1(I)/EP1(0))/2*DT
 280      CONTINUE
          DO 290 I=0,3
            EP0(I)=EP1(I)
 290      CONTINUE
        ELSEIF(NDIV.LE.12) THEN
C-- NDIV steps
          CALL TRAPZ2(NDIV,XYS0,EP0,DT,FXYS0,BBFLD0,EXTFXY,
     %       BBFL1,L,DS,KIN,NXY,BBQ,is)
        ELSE
C-- Runge-Kutta
          CALL RNGKTT(XYS0,EP0,DT,EXTFXY,
     %       BBFL1,ERMIN,BBDXY(1,L2),L,DS,KIN,NXY,BBQ,IRTN1,is)
        ENDIF
C--
        DO 620 I=1,3
          FLD(I,1,N)=FLD(I,1,N)+BBFL1(I,1)
          FLD(I,2,N)=FLD(I,2,N)+BBFL1(I,2)
 620   CONTINUE
       fldabs=sqrt(fld(1,l2,n)**2+fld(2,l2,n)**2)
       dotfld=(fld(1,l2,n)*ep0(1)+fld(2,l2,n)*ep0(2))/fldabs
     &       /sqrt(ep0(1)**2+ep0(2)**2)
       if(fldabs.gt.fldmax(l2)) then
          fldmax(l2)=fldabs
          write(msgfl,103) n,is,l2,charge(kin),ep0(0),fldabs,t1
 103      format(' n= ',i10,' is= ',i4,' l2= ',i3,
     &         ' q= ',1pd8.1,' ep0(0)= ', 1pe11.3,
     &         ' |fld|=  ',1pe11.3,
     &         ' t1= ',1pe11.3)
       endif
c       if(is.eq.6.and.l2.eq.2) then
c          write(msgfl,113) n,is,l2,charge(kin),ep0(0),ep0(3),
c     &     fldabs,dotfld,t1
c 113      format(' n= ',i10,' is= ',i4,' l2= ',i3,' q= ',1pd8.1,
c     &         ' ep0(0)= ', 1pe11.3,' ep0(3)= ', 1pe11.3,
c     &         ' |fld|=  ',1pe11.3,' dotfld= ',1pe11.3,
c     &           ' t1= ',1pe11.3)
c       endif
        IF(ISPIN.GE.1) THEN
          ANOM1=ANOME(FLD(1,1,N),EP(0,N),2)
          CALL BMTEQ(EP(0,N),CHARGE(KIN),MASS(KIN),ANOM1,
     %       FLD(1,1,N),DT,SPIN(1,N))
        ENDIF
C-- Store new variables. 
C    Extrapolate back to the initial time by a straight line.
C    Effects of the external field have already taken into account
C    so that only straight line extrapolation is enough at ENDPUSH
C    for particles with LBBFIN=1.
        DO 640 I=0,3
          EP(I,N)=EP0(I)
 640    CONTINUE
        DO 660 I=1,3
          TXYS(I,N)=XYS0(I)-DT*EP(I,N)/EP(0,N)
 660    CONTINUE
C--
 690    LBBFIN(N)=1
 400  CONTINUE
      CALL CPUTIM('SLVEQM',2)
      RETURN
      END

