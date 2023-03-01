      SUBROUTINE WRFMTFC(FILE,XX,NXX,CHBUF,FMT,ERR)
C  Print XX(i) (i=1,NXX) by analysing fortran format FMT
C  Recognized descriptors
C     X,T,A,I,F,E,D,G,P,/,:
	USE FLCHTYP
	IMPLICIT NONE
	INCLUDE 'include/evchcod.h'
	INTEGER FILE,NXX
	CHARACTER(*) CHBUF,FMT,ERR
	TYPE(FLCHTYPE) XX(NXX)
	INTEGER, PARAMETER:: MLVL=10
	INTEGER NREP(MLVL),IREP(MLVL),ILVL(MLVL),LRT,IRT,LVLRT,LPEND,IRTN
	INTEGER NC,LVL,KP,I,NX,IC,KK(3,2),N,LNN,NN,NN1,IDEC,NC1,NCH1,
     %     LNNC(2),NC2,IOS
	INTEGER, PARAMETER:: MCHLINE=1024
	CHARACTER(MCHLINE) LINE
	CHARACTER(30) FMT1    !  20 needed
	INTEGER NCHLINE
	INTEGER LCHCOD,IPAK1
	CHARACTER(1) CH

	NC=LEN(FMT)
	IF(NC.LE.1) GOTO 955
	DO I=NC,1,-1
	  IF(FMT(I:I).NE.' ') THEN
	    NC=I
	    EXIT
	  ENDIF
	ENDDO
	LVL=0
	NX=0
	NCHLINE=0
	KP=0
	LNN=0
	LINE=' '
	ERR=' '
	LRT=0
	I=0
100   I=I+1
110	IF(I.GT.NC) GOTO 905
	IC=LCHCOD(FMT(I:I))
	IF(LVL.EQ.0.AND.IC.NE.C_OPENPAR1) GOTO 955
	IF(IC.EQ.C_QTE.OR.IC.EQ.C_DBLQTE) THEN
	  LPEND=0
	  DO WHILE(I.LT.NC)
	    I=I+1
	    IF(LCHCOD(FMT(I:I)).EQ.IC) THEN
	      IF(LPEND.EQ.0) THEN
	        LPEND=1
	      ELSE
	        IF(NCHLINE.GE.MCHLINE) GOTO 900
	        NCHLINE=NCHLINE+1
	        LINE(NCHLINE:NCHLINE)=FMT(I:I)
	        LPEND=0
	      ENDIF
	    ELSE
	      IF(LPEND.EQ.0) THEN
	        IF(NCHLINE.GE.MCHLINE) GOTO 900
	        NCHLINE=NCHLINE+1
	        LINE(NCHLINE:NCHLINE)=FMT(I:I)
	      ELSE
	        I=I-1
	        GOTO 100
	      ENDIF
	    ENDIF
	  ENDDO
	  GOTO 910
	ENDIF
	IF(IC.EQ.C_BLANCK) GOTO 100
	IF(IC.EQ.C_OPENPAR1) THEN
	  IF(LVL.GE.MLVL) GOTO 945
	  LVL=LVL+1
	  IF(LVL.LE.2.AND.LRT.EQ.0) THEN
C           position to return when FMT end reached
	    IF(LNN.EQ.0) THEN
	      IRT=I
	    ELSE
	      IRT=LNNC(1)
	    ENDIF
	    LVLRT=LVL-1
	  ENDIF
	  ILVL(LVL)=I
	  IF(LNN.EQ.0) THEN
	    NREP(LVL)=1
	  ELSEIF(NN.GE.1) THEN
	    NREP(LVL)=NN
	    LNN=0
	  ELSE
	    GOTO 930
	  ENDIF
	  IREP(LVL)=0
	  GOTO 100
	ELSEIF(IC.EQ.C_CLOSPAR1) THEN
	  IF(LNN.NE.0) GOTO 915
	  IF(LVL.EQ.1) THEN
	    IF(NCHLINE.NE.0) THEN
		    WRITE(FILE,'(A)') LINE(1:NCHLINE)
	      NCHLINE=0
	      LINE=' '
	    ELSE
	      WRITE(FILE,'()')
	    ENDIF
	  ENDIF
	  IREP(LVL)=IREP(LVL)+1
	  IF(IREP(LVL).GE.NREP(LVL)) THEN
	    LVL=LVL-1
	    IF(LVL.EQ.0) THEN
C	      KP=0
C              The above lines, which is commented out, would clear the 
C              scale shifter P when format end is reached.
	      IF(NX.LT.NXX) THEN
	        LRT=1
	        LVL=LVLRT
	        I=IRT
	        GOTO 110
	      ELSE
	        GOTO 820
	      ENDIF
	    ENDIF
	  ELSE
	    I=ILVL(LVL)
	  ENDIF
	  GOTO 100
	ELSEIF(IC.GE.C_NUM.AND.IC.LE.C_NUM+9.OR.
     %         IC.EQ.C_PLUS.OR.IC.EQ.C_MINUS) THEN
	  IF(LNN.NE.0) GOTO 915
	  CALL GETFMTNUM(FMT(1:NC),I,KK,IDEC,IRTN)
	  IF(IDEC.NE.0.OR.IRTN.EQ.102) GOTO 920
	  IF(IRTN.NE.0) GOTO 925
	  IF(I.EQ.NC) GOTO 905
	  LNN=1
	  LNNC(1)=KK(1,1)
	  LNNC(2)=KK(2,1)
		NN=KK(3,1)
	  GOTO 100
	ELSEIF(IC.EQ.C_COMMA) THEN
	  IF(LNN.NE.0) GOTO 915
	  GOTO 100
	ELSEIF(IC.EQ.C_DIV) THEN
	  IF(LNN.NE.0) GOTO 915
	  IF(NCHLINE.NE.0) THEN
		  WRITE(FILE,'(A)') LINE(1:NCHLINE)
	    NCHLINE=0
	    LINE=' '
	  ELSE
	    WRITE(FILE,'()')
	  ENDIF
	  GOTO 100
	ELSEIF(IC.EQ.C_COLON) THEN
	  IF(LNN.NE.0) GOTO 915
	  IF(NX.GE.NXX) GOTO 800
	  GOTO 100
	ELSE
	  CH=FMT(I:I)
	  CALL TOUPPER(CH)
	  IF(CH.EQ.'P') THEN
	    IF(LNN.EQ.0) GOTO 940
	    KP=NN
	    LNN=0
	    GOTO 100
	  ELSEIF(CH.EQ.'F'.OR.CH.EQ.'E'.OR.CH.EQ.'D'.OR.CH.EQ.'G'.OR.
     %    CH.EQ.'I'.OR.CH.EQ.'A') THEN
	    IF(NX.GE.NXX) GOTO 800
	    IF(I.EQ.NC) GOTO 905
	    I=I+1
	    CALL GETFMTNUM(FMT(1:NC),I,KK,IDEC,IRTN)
	    IF(IRTN.NE.0) GOTO 925
	    IF(I.EQ.NC) GOTO 905
	    IF(LNN.NE.0) THEN
	      LNN=0
	      IF(NN.LE.0) GOTO 930
	      NN1=NN
	    ELSE
	      NN1=1
	    ENDIF
C           Create format statement
	    FMT1='('
	    NC1=1
C           Scale factor P
	    IF(KP.NE.0) THEN
	      WRITE(FMT1(NC1+1:NC1+6),'(I4,"P,")') KP
	      NC1=NC1+6
	    ENDIF
C           E,I,F, etc
	    NC1=NC1+1
	    FMT1(NC1:NC1)=CH
C           Field width
	    IF(KK(1,1).EQ.0) THEN
            IF(CH.EQ.'I') THEN
	        KK(3,1)=12
	        KK(1,1)=1
	      ELSEIF(CH.NE.'A') THEN
	        KK(3,1)=25
	        KK(1,1)=1
	      ENDIF
	    ENDIF
	    IF(KK(1,2).EQ.0) THEN
	      IF(CH.EQ.'F'.OR.CH.EQ.'E'.OR.CH.EQ.'D'.OR.CH.EQ.'G') THEN
	        KK(3,2)=16
	        KK(1,2)=1
	      ENDIF
	    ENDIF
	    IF(KK(1,1).NE.0) THEN
	      WRITE(FMT1(NC1+1:NC1+3),'(I3)') KK(3,1)
	      NC1=NC1+3
	    ENDIF
	    IF(KK(1,2).NE.0) THEN
	      WRITE(FMT1(NC1+1:NC1+4),'(".",I3)') KK(3,2)
	      NC1=NC1+4
	    ENDIF
	    NC1=NC1+1
	    FMT1(NC1:NC1)=')'
	    IF(CH.NE.'A') NC2=KK(3,1)
	    DO N=1,NN1
	      IF(NX.GE.NXX) GOTO 800
	      NX=NX+1
	      IF(CH.EQ.'A') THEN
	        IF(XX(NX)%L.NE.2) GOTO 935
	        IF(KK(1,1).EQ.0) THEN
	          NC2=XX(NX)%C(2)-XX(NX)%C(1)+1
	          IF(NC2.LE.0) CYCLE
	        ELSE
	          NC2=KK(3,1)
	        ENDIF
	      ELSE
	        IF(XX(NX)%L.NE.1) GOTO 935
	      ENDIF
	      NCH1=NCHLINE+NC2
            IF(NCH1.GT.MCHLINE) GOTO 900
	      IF(CH.EQ.'A') THEN
	        WRITE(LINE(NCHLINE+1:NCH1),FMT1,IOSTAT=IOS) 
     %          CHBUF(XX(NX)%C(1):XX(NX)%C(2))
	      ELSEIF(CH.EQ.'I') THEN
	        WRITE(LINE(NCHLINE+1:NCH1),FMT1,IOSTAT=IOS) NINT(XX(NX)%X)
	      ELSE
	        WRITE(LINE(NCHLINE+1:NCH1),FMT1,IOSTAT=IOS) XX(NX)%X
	      ENDIF
	      IF(IOS.NE.0) GOTO 960
	      NCHLINE=NCH1
	    ENDDO
	    GOTO 100
	  ELSEIF(CH.EQ.'X') THEN
	    IF(LNN.NE.0) THEN
	      IF(NN.LE.0) GOTO 930
	      LNN=0
	    ELSE
	      GOTO 940
	    ENDIF
	    NCHLINE=NCHLINE+NN
	    IF(NCHLINE.GT.MCHLINE) GOTO 900
	    GOTO 100
	  ELSEIF(CH.EQ.'T') THEN
	    IF(LNN.NE.0) GOTO 915
	    I=I+1
	    CALL GETFMTNUM(FMT(1:NC),I,KK,IDEC,IRTN)
	    IF(IRTN.EQ.102.OR.IDEC.NE.0) GOTO 920
	    IF(IRTN.NE.0) GOTO 925
	    IF(KK(1,1).EQ.0) GOTO 940
	    IF(KK(3,1).LE.0) GOTO 930
	    NCHLINE=KK(3,1)-1
	    IF(NCHLINE.GT.MCHLINE) GOTO 900
	    GOTO 100
	  ELSE
	    GOTO 950
	  ENDIF
	ENDIF
	GOTO 905
800   IF(NCHLINE.NE.0) WRITE(FILE,'(A)') LINE(1:NCHLINE)
820   ERR=' '
	RETURN

900   ERR='The line to be printed too long.'
	RETURN
905   ERR='Unexpected end of format statement.'
	RETURN
910   ERR='Missing closing apos in format statement.'
	RETURN
915   ERR='Unidentified number "'//FMT(LNNC(1):LNNC(2))//
     %       '" in format statement.'
	RETURN
920   ERR='Invalid decimal point in format statement.'
	RETURN
925   ERR='Invalid number string in format statement.'
	RETURN
930   ERR='Invalid non-positive number in format statement.'
	RETURN
935   ERR='Data type mismatch between format statement and data.'
	RETURN
940   ERR='Missing number in format statement.'
	RETURN
945   ERR='( ) too deeply nested in format statement.'
	RETURN
950   ERR='Invalid character "'//FMT(I:I)//'" in format statement.'
	RETURN
955   ERR='Format statement not enclosed by ()'
	RETURN
960   ERR='Invalid format '//FMT1
	RETURN
	END

      SUBROUTINE GETFMTNUM(A,I,KK,IDEC,IRTN)
C  Get number like mm.nn for analysis of format statement
C  Input
C   A    Format string
C   I    Start location in A
C  Output
C   I    Last position of the number
C   KK(3,2)   KK(*,1): mm part, KK(*,2): nn part
C        KK(1,*) :  start location of the part (0 if not found)
C        KK(2,*) :  end location of the part
C        KK(3,*) :  the integer contained
C   IDEC Location of the decimal point (0 if not found)
C   IRTN
	IMPLICIT NONE
	CHARACTER(*) A
	INTEGER I,KK(3,2),IDEC,IRTN
	INCLUDE 'include/evchcod.h'
	INTEGER NC,J,IC,II(2,2),K
	INTEGER LCHCOD,IPAK1

	NC=LEN(A)
	J=I-1
	I=NC
	IDEC=0
	KK=0
	K=1
C        K=1 before decimal point, 2 after.
	DO WHILE (J.LT.NC)
	  J=J+1
	  IC=LCHCOD(A(J:J))
	  IF(IC.EQ.C_BLANCK) CYCLE
	  IF(IC.EQ.C_PLUS.OR.IC.EQ.C_MINUS) THEN
	    IF(KK(1,1).EQ.0) THEN
	      KK(1,1)=J
	      KK(2,1)=J
	    ELSE
	      GOTO 910
	    ENDIF
	  ELSEIF(IC.EQ.C_DEC) THEN
	    IF(K.GE.2) GOTO 920
	    K=K+1
	    IDEC=J
	  ELSEIF(IC.GE.C_NUM.AND.IC.LE.C_NUM+9) THEN
	    IF(KK(1,K).EQ.0) KK(1,K)=J
	    KK(2,K)=J
	  ELSE
	    I=J-1
	    EXIT
	  ENDIF
	ENDDO
	DO K=1,2
	  IF(KK(1,K).EQ.0) CYCLE
	  KK(3,K)=IPAK1(A(KK(1,K):KK(2,K)),KK(2,K)-KK(1,K)+1,IRTN)
	  IF(IRTN.NE.0) GOTO 930
	ENDDO
	IRTN=0
	RETURN
910   IRTN=101        ! invalid +-
	RETURN
920   IRTN=102        ! invalid decimal point
	RETURN
930   IRTN=103        ! invalid numberical string
	RETURN
	END
