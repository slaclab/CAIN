      INTEGER MMCMD
      PARAMETER (MMCMD=41)
      CHARACTER*16 CMD(MMCMD)/'ALLOCATE','SET','ARRAY','FLAG','BEAM',
     %  'LASER','LASERQED','EXTERNALFIELD','CFQED','LUMINOSITY',
     %  'BBFIELD','PPINT',
     %  'DRIFT','LORENTZ','PUSH','ENDPUSH',
     %  'MAGNET','BEAMLINE','BLOPTICS','MATCHING',
     %  'TRANSPORT','ENDTRANSPORT','CLEAR',
     %  'IF','ELSEIF','ELSE','ENDIF',
     %  'DO','CYCLE','EXIT','ENDDO',
     %  'PRINT','WRITE','PLOT','FILE','HEADER','STORE','RESTORE',
     %  'DEBUGFLAG','STOP','END'/
      INTEGER NCCMD(MMCMD)   !   filled in initlz.f
      COMMON/CMDNAMCM/NCCMD
C   0 indicates no operand accepted
      INTEGER OPACCEPT(MMCMD)/1,1,1,1,1,
     %   1,1,1,1,1,
     %   1,1,
     %   1,1,1,1,
     %   1,1,1,1,
     %   1,0,1,
     %   1,1,0,0,
     %   1,0,0,0,
     %   1,1,1,1,1,1,1,
     %   1,0,0/
