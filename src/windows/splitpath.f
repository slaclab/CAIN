	SUBROUTINE SPLITPATH(FULLNAME,PATH,NCPATH,NAME,NCNAME,EXT,NCEXT)
! Split fullpath name into  path+name+extension
!  FULLNAME   Leading and trailing blanck spaces are ignored.
!  PATH    Includes derive_name:\ and the last \ (or /)
!  NAME    Does not include extension.
!          (When the input FULLNAME is a directory, NAME will be the last directory name.)
!  EXT     Extension including the leading period.
!          (When there are more than one period, the last one is considered
!           to be the extension identifier.)
	IMPLICIT NONE
	INTEGER NCPATH,NCNAME,NCEXT
	CHARACTER*(*) FULLNAME,PATH,NAME,EXT
	INTEGER NC0,NC,IE,IDDIR,I0,I
!
	NC0=LEN(FULLNAME)
	IE=0
	IDDIR=0
	I0=0
	NC=0
	DO 200 I=NC0,1,-1
	  IF(FULLNAME(I:I).NE.' ') THEN
	    IF(NC.EQ.0) NC=I
	    I0=I
	  ENDIF
	  IF(FULLNAME(I:I).EQ.'.') THEN
		IF(IE.EQ.0.AND.IDDIR.EQ.0) IE=I
	  ELSEIF(FULLNAME(I:I).EQ.'\'.OR.FULLNAME(I:I).EQ.'/') THEN
		IF(IDDIR.EQ.0) THEN
		  IDDIR=I
	    ENDIF
	  ENDIF
200	CONTINUE
      NCPATH=0
	NCNAME=0
	NCEXT=0
	PATH=''
	NAME=''
	EXT=''
      IF(NC.EQ.0) RETURN
	IF(IDDIR.NE.0) THEN
	  PATH=FULLNAME(I0:IDDIR)
	  NCPATH=IDDIR-I0+1
	  I0=IDDIR+1
	ENDIF
	IF(IE.EQ.0) THEN
	  NAME=FULLNAME(I0:NC)
	  NCNAME=NC-I0+1
	ELSE
	  NAME=FULLNAME(I0:IE-1)
	  NCNAME=IE-I0
	  NCEXT=NC-IE+1
	  EXT=FULLNAME(IE:NC)
	ENDIF
	RETURN
	END

