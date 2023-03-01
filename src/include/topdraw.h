      INTEGER MLMODE,MCOLR
      PARAMETER (MLMODE=9,MCOLR=8)
      CHARACTER*7 COLOR(0:MCOLR)/'WHITE','WHITE','RED','YELLOW','GREEN',
     %     'CYAN','BLUE','MAGENTA','BLACK'/
      CHARACTER*15 PATTRN(0:MLMODE)/
     %  '100','100','.2 .15','.1 .08','.05 .04',
     %  '.2 .15 .01 .2','.1 .08 .01 .1',
     %  '.05 .04 .01 .05','.02 .09','.015 .05'/
C     0 or 1:solid, 2:long dash, 3:medium dash, 4:short dash,
C     5:long dot-dash, 6:medium dot-dash, 7:short dot-dash,
C     8:dots, 9:fine dots
C
