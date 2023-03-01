C   See include/chspcm.h to change this table
      INTEGER, PARAMETER::
     %  C_BLANCK=0,                                ! blanck
     %  C_OPENPAR1=1, C_OPENPAR2=2, C_OPENPAR3=3,  ! opening parenthsis
     %  C_CLOSPAR1=4, C_CLOSPAR2=5, C_CLOSPAR3=6,  ! closing parenthesis
     %  C_QTE=7, C_DBLQTE=8,                       ! quote
     %  C_PLUS=10, C_MINUS=11, C_STAR=12, C_DIV=13, C_HAT=14, ! binary operator
     %  C_AND=15, C_OR=16,                         ! logical operator
     %  C_EQ=17, C_LARGE=18, C_SMALL=19,           ! relational operator
     %  C_DOLLAR=25, C_COLON=26,                   ! dollar, colon
     %  C_COMMA=28,                                ! comma
     %  C_DEC=29,                                  ! decimal point
     %  C_NUM=30,                                  ! number
     %  C_EXP=40,                                  ! characters for exponent
     %  C_UALPHA=50,                               ! other uppercase alphabet
     %  C_LALPHA=80,                               ! other lowercase alphabet
     %  C_VAR=109,                                 ! others allowed in variable name
     %  C_OTHER=110                                ! others
