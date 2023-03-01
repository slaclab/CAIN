	SUBROUTINE NLCPGN(N,LSR,OMG,EV,PD,DT,PMAXCP1,
     %   NPH1,WGT1,PROB,PE2,PG2,SPE2,STK2,IRTN)
	USE BEAMCM
	USE LASRDATA
      IMPLICIT NONE
	INCLUDE 'include/lasrcm.h'
      INTEGER N,LSR,NPH1,IRTN
	REAL(8) OMG,EV(3,3),PD,DT,PMAXCP1,WGT1,PROB,PE2(0:3),PG2(0:3),
     %  SPE2(3),STK2(3)

	INTEGER I
	REAL(8) ABSP,ABSP2,HE1,HE2

      INCLUDE 'include/ctrlcm.h'

      IF(ISPIN.GE.1) THEN
        HE1=EP(1,N)*SPIN(1,N)+EP(2,N)*SPIN(2,N)+EP(3,N)*SPIN(3,N)
        ABSP=SQRT(EP(1,N)**2+EP(2,N)**2+EP(3,N)**2)
        HE1=HE1/ABSP
      ENDIF
!     Begin Change by Li Dongguo March.11.2003 
      CALL NLCPGN0(EP(0:3,N),OMG,EV,HE1,STKSLS(1:3,LSR),
     %  PD,DT,PMAXCP1,ISPIN,
     %  NPH1,PE2,HE2,PG2,STK2,PROB,WGT1,IRTN)

!     End Change by Li Dongguo March.11.2003
!     Old Version before Change March.11.2003
!        CALL NLCPGN0(EP(0,N),OMG,EV(1,3),HE1,STKSLS(2,LSR),
!          PD,DT,PMAXCP,ISPIN,
!          NPH1,PE2,HE2,PG2,HG2,PROB,WGT1,IRTN)

      IF(IRTN.NE.0) GOTO 900
      PMMCP=MAX(PMMCP,PROB)
C      ----- for circular polarization till next ++++++
      IF(NPH1.GE.1) THEN
        IF(ISPIN.GE.1) THEN
          ABSP2=SQRT(PE2(1)**2+PE2(2)**2+PE2(3)**2)
          DO I=1,3
            SPE2(I)=HE2*PE2(I)/ABSP2
          ENDDO
!          STK2(1)=0
!          STK2(2)=HG2
!          STK2(3)=0
        ENDIF
      ENDIF
      IF(ISPIN.GE.1.AND.(NPH1.EQ.0.OR.WGT1.LT.1)) THEN
        DO I=1,3
          SPIN(I,N)=HE1*EP(I,N)/ABSP
        ENDDO
      ENDIF
c              +++++++++++++++++++++++++++
	RETURN
900   RETURN
	END