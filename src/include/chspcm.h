C   See include/evchcod.h to change this table
      INTEGER MCHNM,MCHSP
      PARAMETER (MCHNM=102,MCHSP=100)
      CHARACTER*1 CHSP(MCHSP)
      INTEGER NCHSP
      CHARACTER*1 CHNM(0:MCHNM)/
     %  ' ', '(', '[', '{', ')', ']', '}', "'", '"', ' ',
     %  '+', '-', '*', '/', '^', '&', '|', '=', '>', '<',
     %  ' ', ' ', ' ', ' ', ' ', '$', ':', ' ', ',', '.',
     %  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
     %  'E', 'D', 'Q', 'e', 'd', 'q', ' ', ' ', ' ', ' ',
     %  'A', 'B', 'C', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
     %  'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U', 'V', 'W',
     %  'X', 'Y', 'Z', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
     %  'a', 'b', 'c', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
     %  'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'v', 'w',
     %  'x', 'y', 'z'/  
      COMMON/CHSPCM/NCHSP
      COMMON/CHSPCM2/CHSP