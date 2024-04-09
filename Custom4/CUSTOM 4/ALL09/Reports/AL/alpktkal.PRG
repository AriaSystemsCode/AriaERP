*:***************************************************************************
*: Program file  : ALPKTKAL.PRG
*: Program desc. : Custom Pick ticket form for ALL Fashions
*: TRACK NO      : C201296.exe,C201295.122  {T20101201.0018}
*: System        : Aria4XP
*: Module        : AL
*: Developer     : Mariam Mazhar(MMT)
*:***************************************************************************
* Modifications:
*:***************************************************************************
IF llOGFltCh
  lcTmpWshp = loogscroll.gfTempName()
  IF USED(lcTmpWshp)
    USE IN (lcTmpWshp)
  ENDIF
  IF !USED('CUTPICK')
    =gfOpenTable('CUTPICK','CUTORD')
  ENDIF
  IF !USED('POSLN')
    =gfOpenTable('POSLN','POSLN')
  ENDIF

  SELECT (lcTmpOrdL)
  lcSetRel = SET("Relation")
  DIMENSION  laFlStru[1,18]
  lnFldNo = AFIELDS(laFlStru)

  DIMENSION laFlStru[lnFldNo +2,18]
  laFlStru[lnFldNo +1,1] = 'mShip'
  laFlStru[lnFldNo +1,2] = 'M'
  laFlStru[lnFldNo +1,3] = 10
  laFlStru[lnFldNo +1,4] = 0

  laFlStru[lnFldNo +2,1] = 'mShip1'
  laFlStru[lnFldNo +2,2] = 'M'
  laFlStru[lnFldNo +2,3] = 10
  laFlStru[lnFldNo +2,4] = 0

  STORE ' ' TO  laFlStru[lnFldNo +1,7],laFlStru[lnFldNo +1,8],;
    laFlStru[lnFldNo +1,9],laFlStru[lnFldNo +1,10],;
    laFlStru[lnFldNo +1,11],laFlStru[lnFldNo +1,12],;
    laFlStru[lnFldNo +1,13],laFlStru[lnFldNo +1,14],;
    laFlStru[lnFldNo +1,15],laFlStru[lnFldNo +1,16]
  STORE 0 TO    laFlStru[lnFldNo +1,17] ,laFlStru[lnFldNo +1,18]



  STORE ' ' TO  laFlStru[lnFldNo +2,7],laFlStru[lnFldNo +2,8],;
    laFlStru[lnFldNo +2,9],laFlStru[lnFldNo +2,10],;
    laFlStru[lnFldNo +2,11],laFlStru[lnFldNo +2,12],;
    laFlStru[lnFldNo +2,13],laFlStru[lnFldNo +2,14],;
    laFlStru[lnFldNo +2,15],laFlStru[lnFldNo +2,16]
  STORE 0 TO    laFlStru[lnFldNo +2,17] ,laFlStru[lnFldNo +2,18]

  IF TYPE('lcRpSortBy') = 'U' AND TYPE('lcRpPrtBy') = 'U'
    =gfCrtTmp(lcTmpWshp,@laFlStru,"PikTkt + Order + cGrupDetal + STR(LineNo , 6)",lcTmpWshp,.T.)
  ELSE
*-- IF Print By Picking Ticket Number, the sorting will be as is it.
    IF lcRpPrtBy = 'P'
      IF lcRpSortBy = 'S'
        =gfCrtTmp(lcTmpWshp,@laFlStru,"PikTkt + Order + cGrupDetal + STYLE",lcTmpWshp,.T.)
      ELSE
        =gfCrtTmp(lcTmpWshp,@laFlStru,"PikTkt + Order + cGrupDetal + STR(LineNo , 6)",lcTmpWshp,.T.)
      ENDIF
    ELSE
      IF lcRpSortBy = 'S'
        =gfCrtTmp(lcTmpWshp,@laFlStru,"Account + PikTkt + Order + cGrupDetal + STYLE",lcTmpWshp,.T.)
      ELSE
        =gfCrtTmp(lcTmpWshp,@laFlStru,"Account + PikTkt + Order + cGrupDetal + STR(LineNo , 6)",lcTmpWshp,.T.)
      ENDIF
    ENDIF
  ENDIF
  SELECT (lcTmpOrdL)
  LOCATE
  SCAN
    SCATTER MEMO MEMVAR
    m.mship  = ''
    m.mShip1 = ''
    lnCnter = 1
    IF gfSeek('2'+m.Order+STR(m.lineno,6),'CUTPICK')
      SELECT CUTPICK
      SCAN REST WHILE TRANCD+ORDER+CORDLINE = '2'+m.Order+STR(m.lineno,6)
        =gfSeek('PP'+CUTPICK.Ctktno+'0001'+CUTPICK.STYLE+CUTPICK.ctktlineno+'2','POSLN')
        SELECT * FROM POSLN INTO CURSOR 'TMPSHP' ORDER BY shipno
        SELECT 'TMPSHP'
        SCAN  
          IF !EMPTY(TMPSHP.shipno) AND (!(TMPSHP.shipno $ m.mship) AND !(TMPSHP.shipno $ m.mShip1))
            IF lnCnter <= 3
              m.mship = m.mship +  IIF(EMPTY(m.mship),'',CHR(13)+CHR(10))+ TMPSHP.shipno
              lnCnter = lnCnter + 1
            ELSE
              m.mShip1 = m.mShip1 +  IIF(EMPTY(m.mShip1),'',CHR(13)+CHR(10))+ TMPSHP.shipno
              lnCnter = lnCnter + 1
            ENDIF
          ENDIF
        ENDSCAN
      ENDSCAN
    ENDIF
    INSERT INTO (lcTmpWshp) FROM MEMVAR
  ENDSCAN
  lcTmpOrdL = lcTmpWshp
  SELECT(lcTmpOrdL)
  SET RELATION TO &lcSetRel.
ENDIF