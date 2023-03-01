      FUNCTION LCHCOD(CH)
C  Character code
C   0   blanck
C   1-3     opening parenthesis
C   4-6     closing parenthesis
C   7       '  (apostrophe)
C   10-14   binary operator
C   15      &  (and)
C   16      |  (or)
C   18      ,  comma
C   19      .  decimal point
C   20-29   number
C   30-35   alphabet for exponent indication
C   40-62   other upper-case alphabet
C   70-92   other lower-case alphabet
C   99      other characters which may appear in variable names,
C           defined by the subr.EVINIT.
C  100      other characters which must not.
C  Thus,
C   30-99  can appear as the first character of a variable name.
C   20-99  can appear as a non-first character of a variable name.
C   100    cannot be used at any place in expressions.
C
      IMPLICIT NONE
      CHARACTER*1 CH
      INTEGER LCHCOD
      INCLUDE 'include/chspcm.h'
	INCLUDE 'include/evchcod.h'

      INTEGER I
      DO 10 I=0,MCHNM
        IF(CH.EQ.CHNM(I)) THEN
          LCHCOD=I
          RETURN
        ENDIF
 10   CONTINUE
      IF(NCHSP.GE.1) THEN
        DO 20 I=1,NCHSP
          IF(CH.EQ.CHSP(I)) THEN
            LCHCOD=C_VAR
            RETURN
          ENDIF
 20     CONTINUE
      ENDIF
      LCHCOD=C_OTHER
      RETURN
      END
