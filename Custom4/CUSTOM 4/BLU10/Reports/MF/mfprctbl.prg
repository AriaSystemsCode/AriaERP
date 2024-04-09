*:----------------------------------------------------------------
*: Program file  : MFPRCTBL.PRG for BLU10 Blue Berry
*: Program desc. : Print Cut Ticket Form.
*: Date          : 02/15/2007
*: System        : Aria 4XP.
*: Module        : Manufactring - (MF)
*: Developer     : Waleed Hamed - (WLD)
*: Tracking Num  : C200748 CONVERT FROM C101721
*: Ticket Number : T20070108.0125
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : 1. MF/OutPut/CutTkt
*:----------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------
*: Example             : DO MFPRCTBL
*:----------------------------------------------------------------
*--- Array to hold structure of temp files.
PRIVATE laFileStru
lcCtPck   = loOGScroll.gfTempName()
lcCtPckBL = loOGScroll.gfTempName()
lcScaleBL = loOGScroll.gfTempName()
lcMainFBL = loOGScroll.gfTempName()

loOGScroll.cCROrientation='L'
*--- Start Main
llDummy = lfGetReady()   &&--- Function to create Temp Files.
llDummy = lfGropData()   &&--- Funciotn to group each 14 size in one line
llDummy = lfGrpAlloc()   &&--- Group each 14 size in one line for Allo.
llDummy = lfSetRel()     &&--- Function to set and clear relation.

*--- End Main

*:----------------------------------------------------------------
*: Name       : lfGetReady      Due To :C#101721
*: Developer  : Waleed Hamed - (WLD)
*: Date       : 02/15/2007
*: Purpose    : Function to Create Temp Files.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfGetReady()
*:----------------------------------------------------------------
FUNCTION lfGetReady

  =gfOpenTable('CutPick','CutPick','SH','CutPick')

  SELECT CutPick
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,18]
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SCALE'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 3
  laFileStru[lnFileStru,4] = 0
  FOR lnCount = 7 TO 16
    STORE SPACE(1) TO laFileStru[lnFileStru,lnCount]
  ENDFOR
  STORE 0 TO laFileStru[lnFileStru, 17],laFileStru[lnFileStru, 18]

  gfCrtTmp(lcCtPck,@laFileStru,"TranCd+cTktNo+Style+Order",lcCtPck,.F.)

  *---- lcCtPckBL
  SELECT CutPick
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+6,18]
  FOR lnIndex  = 9 TO 14
    lcIndex = ALLTRIM(STR(lnIndex,2))
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'Qty'+lcIndex
    laFileStru[lnFileStru,2] = 'N'
    laFileStru[lnFileStru,3] = 6
    laFileStru[lnFileStru,4] = 0
    FOR lnCount = 7 TO 16
      STORE SPACE(1) TO laFileStru[lnFileStru,lnCount]
    ENDFOR
    STORE 0 TO laFileStru[lnFileStru, 17],laFileStru[lnFileStru, 18]
  ENDFOR

  gfCrtTmp(lcCtPckBL,@laFileStru,"trancd+ctktno+style+PADL(cTktLineNo,6)",lcCtPckBL,.F.)

  *--- lcScaleBL
  lnFileStru = 0
  DIMENSION laFileStru[16,18]
  laFileStru=''
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'STYLE'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 19
  laFileStru[lnFileStru,4] = 0
  FOR lnCount = 7 TO 16
    STORE SPACE(1) TO laFileStru[lnFileStru,lnCount]
  ENDFOR
  STORE 0 TO laFileStru[lnFileStru, 17],laFileStru[lnFileStru, 18]

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Line'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  FOR lnCount = 7 TO 16
    STORE SPACE(1) TO laFileStru[lnFileStru,lnCount]
  ENDFOR
  STORE 0 TO laFileStru[lnFileStru, 17],laFileStru[lnFileStru, 18]

  FOR lnIndex = 1 TO 14
    lcIndex = ALLTRIM(STR(lnIndex,2))
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'Sz'+lcIndex
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 5
    laFileStru[lnFileStru,4] = 0
    FOR lnCount = 7 TO 16
      STORE SPACE(1) TO laFileStru[lnFileStru,lnCount]
    ENDFOR
    STORE 0 TO laFileStru[lnFileStru, 17],laFileStru[lnFileStru, 18]
  ENDFOR

  gfCrtTmp(lcScaleBL,@laFileStru,"Style+Line",lcScaleBL,.F.)

  *--- (lcMainFBL)
  SELECT(lcMainF)
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+12,18]
  FOR lnIndex  = 9 TO 14
    lcIndex = ALLTRIM(STR(lnIndex,2))
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'Qty'+lcIndex
    laFileStru[lnFileStru,2] = 'N'
    laFileStru[lnFileStru,3] = 6
    laFileStru[lnFileStru,4] = 0
    FOR lnCount = 7 TO 16
      STORE SPACE(1) TO laFileStru[lnFileStru,lnCount]
    ENDFOR
    STORE 0 TO laFileStru[lnFileStru, 17],laFileStru[lnFileStru, 18]
  ENDFOR
  FOR lnIndex  = 9 TO 14
    lcIndex = ALLTRIM(STR(lnIndex,2))
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'Ord'+lcIndex
    laFileStru[lnFileStru,2] = 'N'
    laFileStru[lnFileStru,3] = 7
    laFileStru[lnFileStru,4] = 0
    FOR lnCount = 7 TO 16
      STORE SPACE(1) TO laFileStru[lnFileStru,lnCount]
    ENDFOR
    STORE 0 TO laFileStru[lnFileStru, 17],laFileStru[lnFileStru, 18]
  ENDFOR
  gfCrtTmp(lcMainFBL,@laFileStru,"PO+cWareCode+Style+Dyelot+STR(LineNo,6)+NoteFlag",lcMainFBL,.F.)

  *:----------------------------------------------------------------
  *: Name       : lfGropData      Due To :C#101721
  *: Developer  : Waleed Hamed - (WLD)
  *: Date       : 02/15/2007
  *: Purpose    : Function to group each 14th size in one line.
  *:----------------------------------------------------------------
  *: Calls      : None.
  *:----------------------------------------------------------------
  *: Parameters : None.
  *:----------------------------------------------------------------
  *: Returns    : None.
  *:----------------------------------------------------------------
  *: Example    : = lfGropData()
  *:----------------------------------------------------------------
FUNCTION lfGropData

  SELECT (lcMainF)
  lcSetSkip = SET('SKIP')
  SET SKIP TO
  GOTO TOP
  DO WHILE !EOF()
    lcCut      = PO
    lcStyle    = ALLTRIM(SUBSTR(STYLE,1,LEN(ALLTRIM(STYLE))-3))
    lcWareCode = cWareCode
    =gfSeek(lcCut+lcWareCode+lcStyle)
    SCATTER MEMVAR MEMO
    m.Style  = lcStyle
    lnCnt    = 0
    lnOldCnt = 0
    lnLineNo = 0
    m.Line   = 0
    FOR lnInd = 1 TO 14
      lcStr    = ALLTRIM(STR(lnInd,2))
      m.Qty&lcStr = 0
      m.Ord&lcStr = 0
    ENDFOR
    SCAN REST WHILE PO+cWareCode+STYLE+Dyelot+NoteFlag = ;
        lcCut+lcWareCode+lcStyle
      SCATTER MEMVAR MEMO
      SELECT (lcMainFBL)
      IF &lcMainf..NoteFlag $ 'ST'
        APPEND BLANK
        GATH MEMVAR MEMO
        REPLACE NoteFlag WITH &lcMainf..NoteFlag,;
          notes    WITH &lcMainf..Notes
      ELSE
        lnOldCnt = lnOldCnt+lnCnt
        lcScle = ALLTRIM(SUBSTR(&lcMainf..STYLE,LEN(ALLTRIM(&lcMainf..STYLE))-2))
        lnCnt  = IIF(gfSeek('S'+lcScle,'SCALE'),SCALE.CNT,0)
        FOR lnInd = lnOldCnt+1 TO lnOldCnt+lnCnt
          SELECT (lcMainF)
          lcStr    = ALLTRIM(STR(lnInd,2))
          lcGetFrm = ALLTRIM(STR(lnInd-lnOldCnt))
          m.Qty&lcStr = Qty&lcGetFrm
          m.Ord&lcStr = Ord&lcGetFrm
          SELECT (lcMainFBL)
          IF MOD(lnInd,14) = 1
            REPLACE  TotQty  WITH lfCalcTot('Q'),;
              TotOrd  WITH lfCalcTot('')
            APPEND BLANK
            lnLineNo = lnLineNo + 1
            GATH MEMVAR MEMO
            REPLACE LINENO WITH lnLineNo,;
              STYLE  WITH lcStyle
            FOR lnI = 1 TO 14
              lcStr1    = ALLTRIM(STR(lnI,2))
              REPLACE Qty&lcStr1 WITH  0,;
                Ord&lcStr1 WITH  0
            ENDFOR
          ENDIF
          lcFrm = IIF(MOD(lnInd-lnOldCnt,lnCnt)=0,ALLTRIM(STR(lnCnt)),;
            ALLTRIM(STR(MOD(lnInd-lnOldCnt,lnCnt))))
          lcIn  = IIF(MOD(lnInd,14)=0,'14',ALLTRIM(STR(MOD(lnInd,14))))
          REPLACE Qty&lcIn WITH m.Qty&lcFrm,;
            Ord&lcIn WITH m.Ord&lcFrm
          lnOldAls = SELECT(0)
          SELECT(lcScaleBL)
          IF MOD((VAL(lcStr)-1),14) = 0 .OR. !gfSeek(lcStyle)
            m.Line = m.Line + 1
            APPEND BLANK
            REPLACE STYLE WITH lcStyle,;
              LINE  WITH STR(m.Line,6)
          ENDIF
          lcToRep = ALLTRIM(EVAL('SCALE.SZ'+lcGetFrm))
          IF VAL(lcStr) > 14
            LOCATE REST WHILE STYLE = lcStyle FOR EMPTY(SZ14)
            lcTo = ALLTRIM(STR((VAL(lcStr)-14)))
            REPLACE SZ&lcTo WITH lcToRep
          ELSE
            REPLACE SZ&lcStr WITH lcToRep
          ENDIF
          SELECT(lnOldAls)
        ENDFOR
        SELECT (lcMainFBL)
        REPLACE  TotQty  WITH lfCalcTot('Q'),;
          TotOrd  WITH lfCalcTot('')
      ENDIF
    ENDSCAN
    lnCnt    = 0
    lnOldCnt = 0
    m.LineNo = 0
  ENDDO
  SELECT (lcMainF)
  SET SKIP TO &lcSetSkip.

  *:----------------------------------------------------------------
  *: Name       : lfSetRel      Due To :C#101721
  *: Developer  : Waleed Hamed - (WLD)
  *: Date       : 02/15/2007
  *: Purpose    : Set relation.
  *:----------------------------------------------------------------
  *: Calls      : None.
  *:----------------------------------------------------------------
  *: Parameters : None.
  *:----------------------------------------------------------------
  *: Returns    : None.
  *:----------------------------------------------------------------
  *: Example    : = lfSetRel()
  *:----------------------------------------------------------------
FUNCTION lfSetRel

  SELECT (lcCtPckBL)
  SET FILTER TO TotQty <> 0
  GOTO TOP
  SELECT(lcMainF)
  GOTO TOP
  SELECT(lcMainFBL)
  SET RELATION TO
  IF llRPrtAlo
    SET RELATION TO "1"+PO+STYLE+PADL(LINENO,6) INTO (lcCtPckBL) ADDITIVE
    SET SKIP TO (lcCtPckBL)
  ENDIF
  SET RELATION TO STYLE+STR(LINENO,6) INTO (lcScaleBL) ADDITIVE
  SET RELATION TO PO+cwarecode+ALLTRIM(STYLE) INTO (lcMainF) ADDITIVE
  SET FILTER TO TotQty <> 0
  GOTO TOP

  *:----------------------------------------------------------------
  *: Name       : lfGrpAlloc      Due To :C#101721
  *: Developer  : Waleed Hamed - (WLD)
  *: Date       : 02/15/2007
  *: Purpose    : Function to group allocation .
  *:----------------------------------------------------------------
  *: Calls      : None.
  *:----------------------------------------------------------------
  *: Parameters : None.
  *:----------------------------------------------------------------
  *: Returns    : None.
  *:----------------------------------------------------------------
  *: Example    : = lfGrpAlloc()
  *:----------------------------------------------------------------
FUNCTION lfGrpAlloc

  *--- CutPick    *--- Index == trancd+ctktno+style
  SELECT(lcMainFBL)
  GOTO TOP
  SCAN FOR  !gfSeek('1'+PO+ALLTRIM(SUBSTR(STYLE,1,LEN(ALLTRIM(STYLE))-3)),lcCtPckBL)
    lcCut = PO

    lcStyle = ALLTRIM(STYLE)

    SELECT CutPick
    =gfSeek('1'+lcCut+lcStyle)

    SCAN REST WHILE TranCd+cTktNo+STYLE = '1'+lcCut+lcStyle
      SCATTER MEMVAR
      m.Style  = lcStyle
      m.Scale = ALLTRIM(SUBSTR(STYLE,LEN(ALLTRIM(STYLE))-2))
      INSERT INTO(lcCtPck) FROM MEMVAR
    ENDSCAN
    SELECT (lcCtPck)
    =gfSeek('1'+lcCut+lcStyle)

    DO WHILE !EOF()
      lcOrd   = ORDER
      lcCut   = cTktNo
      lcStyle = STYLE

      SCATTER MEMVAR MEMO
      m.Style  = lcStyle
      lnCnt    = 0
      lnOldCnt = 0
      lnLineNo =0

      =SEEK('1'+lcCut+lcStyle+lcOrd)
      SCAN REST WHILE trancd+ctktno+STYLE+ORDER = '1'+lcCut+lcStyle+lcOrd
        IF &lcMainf..NoteFlag $ 'ST'
          APPEND BLANK
          GATH MEMVAR MEMO
          REPLACE NoteFlag WITH &lcMainf..NoteFlag,;
            notes    WITH &lcMainf..Notes
        ELSE
          lnOldCnt = lnOldCnt+lnCnt
          lcScle = SCALE

          lnCnt  = IIF(gfSeek('S'+lcScle,'SCALE'),SCALE.CNT,0)
          FOR lnInd = lnOldCnt+1 TO lnOldCnt+lnCnt

            SELECT (lcCtPck)
            SCAT MEMVAR MEMO
            lcStr    = ALLTRIM(STR(lnInd,2))
            lcGetFrm = ALLTRIM(STR(lnInd-lnOldCnt))
            m.Qty&lcStr = Qty&lcGetFrm
            lnOldAls = SELECT(0)
            m.TotQty    = m.TotQty + Qty&lcGetFrm
            SELECT (lcCtPckBL)
            IF MOD(lnInd,14) = 1
              REPLACE  TotQty  WITH lfCalcTot('Q')
              APPEND BLANK
              lnLineNo = lnLineNo + 1
              GATH MEMVAR MEMO
              REPLACE cTktLineNo WITH STR(lnLineNo,6),;
                STYLE  WITH lcStyle
              FOR lnI = 1 TO 14
                lcStr1    = ALLTRIM(STR(lnI,2))
                REPLACE Qty&lcStr1 WITH  0
              ENDFOR
            ENDIF
            lcFrm = IIF(MOD(lnInd-lnOldCnt,lnCnt)=0,ALLTRIM(STR(lnCnt)),;
              ALLTRIM(STR(MOD(lnInd-lnOldCnt,lnCnt))))
            lcIn  = IIF(MOD(lnInd,14)=0,'14',ALLTRIM(STR(MOD(lnInd,14))))
            REPLACE Qty&lcIn WITH m.Qty&lcFrm
          ENDFOR
          SELECT (lcCtPckBL)
          REPLACE  TotQty  WITH lfCalcTot('Q')
        ENDIF
      ENDSCAN
    ENDDO
  ENDSCAN

  *:----------------------------------------------------------------
  *: Name       : lfUsrVld      Due To :C#101721
  *: Developer  : Waleed Hamed - (WLD)
  *: Date       : 02/15/2007
  *: Purpose    : User defined function to calles from FRX.
  *:----------------------------------------------------------------
  *: Calls      : None.
  *:----------------------------------------------------------------
  *: Parameters : None.
  *:----------------------------------------------------------------
  *: Returns    : None.
  *:----------------------------------------------------------------
  *: Example    :  = lfUsrVldFn('lfBlSum','MFPRCTBL')
  *:----------------------------------------------------------------
FUNCTION lfBlSum
  PARAMETER lcDummy

  PRIVATE lnAlias
  lnAlias = SELECT(0)
  lcTempNAm = loOGScroll.gftempName()

  USE gcWorkDir+(lcMainFBL) IN 0 AGAIN ALIAS (lcTempNAm)
  SELECT (lcTempNAm)
  INDEX ON PO+cWareCode+STYLE+STR(LINENO,6)+Dyelot+NoteFlag TAG (lcTempNAm)
  =gfSeek(&lcMainFBL..PO)
  DIME laSum[15]
  laSum = 0
  *-- Sum size quantities for the budget lines for each warehouse.
  SUM Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,;
    Qty9,Qty10,Qty11,Qty12,Qty13,Qty14,TotQty WHILE PO=&lcMainFBL..PO ;
    FOR NoteFlag = 'N' .AND. cWareCode+TranCD = &lcMainFBL..cWareCode+'1' TO ARRAY laSum
  USE IN (lcTempNAm)
  SELECT (lnAlias)
  RETURN ''
  *--- End.

  *:----------------------------------------------------------------
  *: Name       : lfCalcTot      Due To :C#101721
  *: Developer  : Waleed Hamed - (WLD)
  *: Date       : 02/15/2007
  *: Purpose    : Function to Calculate Table.
  *:----------------------------------------------------------------
  *: Calls      : None.
  *:----------------------------------------------------------------
  *: Parameters : None.
  *:----------------------------------------------------------------
  *: Returns    : None.
  *:----------------------------------------------------------------
  *: Example    : = lfCalcTot()
  *:----------------------------------------------------------------
FUNCTION lfCalcTot
  PARAMETER lcType

  PRIVATE lnToRet
  lnToRet = 0

  FOR lnSzScl = 1 TO 14
    lcSzScl = ALLTRIM(STR(lnSzScl))
    lnToRet = lnToRet + IIF(lcType='Q',Qty&lcSzScl,Ord&lcSzScl)
  ENDFOR
  RETURN lnToRet


