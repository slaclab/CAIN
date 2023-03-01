C Standard format of beam data
      INTEGER LENSTDFMT
      PARAMETER (LENSTDFMT=253)
C        data length according to STDFMT (note: particle name is 4 byte)
C  KIND,GEN,PNAME,WGT,(TXYS(I),I=0,3),(EP(I),I=0,3),(SPIN(I),I=1,3)
      CHARACTER*27 STDFMT/'(1X,I1,I6,1X,A,1P12D20.12)'/
      CHARACTER(198), PARAMETER:: STDTTL=
     %'(13H!K   GEN NAME,7X,6HWeight,7X,8X,4HT(m),16X,4HX(m),16X,'//
     %'4HY(m),16X,4HS(m),16X,5HE(eV),14X,8HPx(eV/c),12X,8HPy(eV/c),'//
     %'12X,8HPs(eV/c),15X,2HSx,18X,2HSy,18X,2HSs,9X)'
C Short format
C  KIND,GEN,PNAME,WGT,(TXYS(I),I=0,3),(EP(I),I=0,3),(SPIN(I),I=1,3)
      CHARACTER*60 SHTFMT/
     %'(1X,I1,I2,1X,A4,1PD10.3,1X,1P4D10.3,1X,1P4D10.3,1X,0P3F7.4)'/
      CHARACTER(198), PARAMETER:: SHTTTL=
     %'(9H!K G NAME,3X,6HWeight,3X,3X,4HT(m),6X,4HX(m),6X,4HY(m),'//
     %'6X,4HS(m),5X,6HEn(eV),3X,8HPx(eV/c),2X,8HPy(eV/c),2X,'//
     %'8HPs(eV/c),4X,2HSx,5X,2HSy,5X,2HSs,2X)'
C Mathematica format
      CHARACTER*43 MATHFMT/
     %'(1H{,I1,1H,,I5,1H,,A,12(1H,,1PD19.12),2H},)'/
      CHARACTER(203), PARAMETER:: MATHTTL=
     %'(13H(*K  GEN NAME,7X,6HWeight,7X,8X,4HT(m),16X,4HX(m),16X,'//
     %'4HY(m),16X,4HS(m),16X,5HE(eV),14X,8HPx(eV/c),12X,8HPy(eV/c),'//
     %'12X,8HPs(eV/c),15X,2HSx,18X,2HSy,18X,2HSs,7X,2H*))'
