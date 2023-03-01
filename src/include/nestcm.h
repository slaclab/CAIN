      INTEGER, PARAMETER:: MNEST_TYPE=4
      INTEGER, PARAMETER:: 
     %          NEST_PUSH=1, NEST_IF=2, NEST_DO=3, NEST_TRANS=4
C          Must be from 1 to MNEST_TYPE
      CHARACTER(9) LOPNAM(MNEST_TYPE)/'PUSH','IF','DO','TRANSPORT'/
      CHARACTER*12 ENDNAM(MNEST_TYPE)/'ENDPUSH','ENDIF','ENDDO',
     %     'ENDTRANSPORT'/

      INTEGER, PARAMETER:: MNEST=10
      INTEGER NEST(MNEST),ICNEST(MNEST),NREP(MNEST),IREP(MNEST),
     %    NESTST(MNEST),IDDOVAR(2,MNEST),DOVAR(2,MNEST),
     %    NESTLV,NESTRT,INPUSH
      COMMON/NESTCM/NEST,ICNEST,NREP,IREP,NESTST,IDDOVAR,DOVAR,
     %    NESTLV,NESTRT,INPUSH

C  NEST=1: PUSH,  =2: IF,  =3: DO,  =4: TRANSPORT
C  NESTRT=0: came from the preceeding statement
C         1: came by jump from a command.
C       jump from IF to ELSE
C       jump from ENDPUSH back to PUSH     
C       jump from ENDDO back to DO
C       jump from DO to ENDDO for loop exit
C       jump from ENDTRANSPORT to TRANSPORT
C  NESTST: status of nest
C    IF:  1: IF started
C         2: ELSEIF found
C         3: ELSE found
C    DO:  1: DO started
C         2: CYCLE found
C         3: EXIT found
C         4: DO detected loop end
C  IDDOVAR: DO control variable.
C          IDDOVAR(1,*)  Variable ID. (scalar or array)
C                        (0 for DO of WHILE/REPEAT type)
C          IDDOVAR(2,*)  Array index (1-d form). (scalar if 0)
C          DOVAR(1,*)    initial value
C          DOVAR(2,*)    increment per step. (Number of steps is 
C                        NREP(*). Same as DO REPEAT type.)
