	SUBROUTINE BLKRHS0(TEXT,MITEM,NITEM,ITEM,IRTN)
C  Separate items delimited by comma
C  Expect a form
C        ( a, b, c, ...)
C  Returns the position ITEM(*,i) for first and last characters.
C  ", ', (, [, {  are recognized.
C   ( Two successive 's or "s under corresponding quotes are not
C     understood as one ' or ", but so long as they appear as
C     a pair, the resulting ITEM(*,*) will be correct.)
C  The outermost pair of ( ) can be absent.
	IMPLICIT NONE
	INTEGER MITEM,NITEM,ITEM(2,MITEM),IRTN
	CHARACTER(*) TEXT
	INCLUDE 'include/ctrlcm.h'
	INCLUDE 'include/evchcod.h'
	INTEGER INAPOS,NC,I,LVL,I0,I1,J,KK,II0,II1,JKK,J0,J1,KKK
	CHARACTER(1) APOS(2)/"'",'"'/,COMMA/','/
	INTEGER, PARAMETER:: ML=50
	INTEGER KAKKO(ML)
	INTEGER LCHCOD

	NITEM=0
	NC=LEN(TEXT)
	IRTN=0
	IF(NC.LE.0) RETURN
C  Check if the whole string is enclosed by parens
	II0=0
      DO I=1,NC
	  IF(TEXT(I:I).NE.' ') THEN
	    IF(II0.EQ.0) II0=I
	    II1=I
	  ENDIF
	ENDDO
	IF(II0.EQ.0) RETURN
	J0=LCHCOD(TEXT(II0:II0))
	J1=LCHCOD(TEXT(II1:II1))
	JKK=C_CLOSPAR1-C_OPENPAR1
	IF((J0.GE.C_OPENPAR1.AND.J0.LE.C_CLOSPAR3).OR.
     %   (J1.GE.C_OPENPAR1.AND.J1.LE.C_CLOSPAR3)) THEN
	  IF(J1-J0.NE.JKK) GOTO 930
	  II0=II0+1
	  II1=II1-1
	  IF(II1.LT.II0) RETURN
	ENDIF
	LVL=0
	I0=0
	INAPOS=0
	DO I=II0,II1
	  KKK=0
	  IF(INAPOS.NE.0) THEN
	    IF(TEXT(I:I).EQ.APOS(INAPOS)) INAPOS=0
	  ELSE
	    KK=LCHCOD(TEXT(I:I))
	    IF(KK.EQ.C_QTE) THEN
	      INAPOS=1
	    ELSEIF(KK.EQ.C_DBLQTE) THEN
	      INAPOS=2
	    ELSEIF(KK.GE.C_OPENPAR1.AND.KK.LE.C_OPENPAR3) THEN
	      LVL=LVL+1
	      KAKKO(LVL)=KK
	    ELSEIF(KK.GE.C_CLOSPAR1.AND.KK.LE.C_CLOSPAR3) THEN
	      IF(LVL.EQ.0) GOTO 900
	      IF(KK-JKK.NE.KAKKO(LVL)) GOTO 900
	      LVL=LVL-1
	    ELSEIF(KK.EQ.C_COMMA.AND.LVL.EQ.0) THEN
	      KKK=1
	    ENDIF
	  ENDIF
	  IF(KKK.EQ.0.AND.TEXT(I:I).NE.' ') THEN
	    IF(I0.EQ.0) I0=I
	    I1=I
	  ENDIF
	  IF(I.EQ.II1) THEN
	    IF(LVL.NE.0) GOTO 910
	    IF(INAPOS.NE.0) GOTO 900
	    IF(I0.NE.0) KKK=1
	  ENDIF
	  IF(KKK.NE.0) THEN
	    IF(NITEM.GE.MITEM) GOTO 920
	   	NITEM=NITEM+1
	    IF(I0.EQ.0) THEN
	      ITEM(1,NITEM)=1
	      ITEM(2,NITEM)=0
	    ELSE
	      ITEM(1,NITEM)=I0
	      ITEM(2,NITEM)=I1
	    ENDIF
	    I0=0
	  ENDIF
	ENDDO
	IRTN=0
	RETURN
900	IRTN=1000
	IF(MSGLVL.GE.0) WRITE(MSGFL,905) TEXT
905   FORMAT(' (SUBE.BLKRHS0) Closing quote does not match in',/,
     %     '"',A,'"')
	GOTO 990
910	IRTN=1010
	IF(MSGLVL.GE.0) WRITE(MSGFL,915) TEXT
915   FORMAT(' (SUBE.BLKRHS0) Missing closing parenthesis in',/,
     %     '"',A,'"')
	GOTO 990
920	IRTN=1020
	IF(MSGLVL.GE.0) WRITE(MSGFL,925) TEXT
925   FORMAT(' (SUBE.BLKRHS0) Too many items in',/,'   "',A,'"')
	GOTO 990
930	IRTN=1030
	IF(MSGLVL.GE.0) WRITE(MSGFL,935) TEXT
935   FORMAT(' (SUBE.BLKRHS0) Parens do not match in',/,'   "',A,'"')
	GOTO 990
990   RETURN
	END

