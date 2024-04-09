*:----------------------------------------------------------------
*: Program file        : MFPRCTBL.PRG  FOR
*: Program description : Print Cut Ticket Form.
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Manufactring - (MF)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101721
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : 1. MF/OutPut/CutTkt
*:----------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------
*: Example             : DO MFPRCTBL
*:----------------------------------------------------------------
*:B#803419,1 SSH Incorect Total
*:B#803866,1 AMH 12/03/2000 Print all allocatoin information of C/T.
*:----------------------------------------------------------------
*--- Array to hold structure of temp files.
PRIVATE laFileStru
*B#803866,1 AMH Get temp. name of temp table for CUTPICK [Start]
lcCtPck = gfTempName()
*B#803866,1 AMH [End]

*--- Start Main
llDummy = lfGetReady()   &&--- Function to create Temp Files.
llDummy = lfGropData()   &&--- Funciotn to group each 14 size in one line
llDummy = lfGrpAlloc()   &&--- Group each 14 size in one line for Allo.
llDummy = lfSetRel()     &&--- Function to set and clear relation.
*--- End Main

*:----------------------------------------------------------------
*: Name       : lfGetReady      Due To :C#101721
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 23/03/00
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

*B#803866,1 AMH Create tamp. table for CUTPICK 
*B#803866,1 with indexed by trancd+ctktno+style+order [Start]
SELECT CutPick
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SCALE'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 3
laFileStru[lnFileStru,4] = 0
CREATE TABLE (gcWorkDir+lcCtPck) FROM ARRAY laFileStru
INDEX ON TranCd+cTktNo+Style+Order TAG (lcCtPck)
*B#803866,1 AMH [End]

*---- lcCtPckBL
SELECT CutPick
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+6,4]
FOR lnIndex  = 9 TO 14
  lcIndex = ALLTRIM(STR(lnIndex,2))
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Qty'+lcIndex
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
ENDFOR
CREATE TABLE (gcWorkDir+lcCtPckBL) FROM ARRAY laFileStru
INDEX ON trancd+ctktno+style+PADL(cTktLineNo,6) TAG (lcCtPckBL)

*--- lcScaleBL
lnFileStru = 0
DIMENSION laFileStru[16,4]
laFileStru=''
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'STYLE'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 19
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Line'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

FOR lnIndex = 1 TO 14
  lcIndex = ALLTRIM(STR(lnIndex,2))
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Sz'+lcIndex
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0
ENDFOR

CREATE TABLE (gcWorkDir+lcScaleBL) FROM ARRAY laFileStru
INDEX ON Style+Line TAG (lcScaleBL)
*--- (lcMainFBL)
SELECT(lcMainF)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+12,4]
FOR lnIndex  = 9 TO 14
  lcIndex = ALLTRIM(STR(lnIndex,2))
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Qty'+lcIndex
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
ENDFOR
FOR lnIndex  = 9 TO 14
  lcIndex = ALLTRIM(STR(lnIndex,2))
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Ord'+lcIndex
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 7
  laFileStru[lnFileStru,4] = 0
ENDFOR
CREATE TABLE (gcWorkDir+lcMainFBL) FROM ARRAY laFileStru
INDEX ON CUTTKT+cWareCode+Style+Dyelot+STR(LineNo,6)+NoteFlag TAG (lcMainFBL)


*:----------------------------------------------------------------
*: Name       : lfGropData      Due To :C#101721
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 23/03/00
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
*B#803866,1 AMH clear set skip [Start]
lcSetSkip = SET('SKIP')
SET SKIP TO
*B#803866,1 AMH [End]
GOTO TOP
DO WHILE !EOF()
  lcCut      = CutTkt
  lcStyle    = ALLTRIM(SUBSTR(STYLE,1,LEN(ALLTRIM(Style))-3))
  lcWareCode = cWareCode
  =SEEK(lcCut+lcWareCode+lcStyle)
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
  SCAN REST WHILE CUTTKT+cWareCode+Style+Dyelot+NoteFlag = ;
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
      lcScle = ALLTRIM(SUBSTR(&lcMainf..Style,LEN(ALLTRIM(&lcMainf..Style))-2))
      lnCnt  = IIF(SEEK('S'+lcScle,'SCALE'),SCALE.Cnt,0)
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
          REPLACE LineNo WITH lnLineNo,;
                  Style  WITH lcStyle
          FOR lnI = 1 TO 14
            lcStr1    = ALLTRIM(STR(lnI,2))
            REPLACE Qty&lcStr1 WITH  0,;
                    Ord&lcStr1 WITH  0
          ENDFOR
        ENDIF
        *:B#803419,1 SSH Incorect Total
        *lcFrm = IIF(MOD(lnInd,8)=0,'8',ALLTRIM(STR(MOD(lnInd,8))))
        *B#803866,1 AMH Incorect Qty [Start]
        *lcFrm = IIF(MOD(lnInd,lnCnt)=0,ALLTRIM(STR(lnCnt)),ALLTRIM(STR(MOD(lnInd,lnCnt))))
        lcFrm = IIF(MOD(lnInd-lnOldCnt,lnCnt)=0,ALLTRIM(STR(lnCnt)),;
                    ALLTRIM(STR(MOD(lnInd-lnOldCnt,lnCnt))))
        *B#803866,1 AMH [End]
        *:B#803419,1 SSH [END]
        lcIn  = IIF(MOD(lnInd,14)=0,'14',ALLTRIM(STR(MOD(lnInd,14))))
        REPLACE Qty&lcIn WITH m.Qty&lcFrm,;
                Ord&lcIn WITH m.Ord&lcFrm
        lnOldAls = SELECT(0)
        SELECT(lcScaleBL)
        IF MOD((VAL(lcStr)-1),14) = 0 .OR. !SEEK(lcStyle)
          m.Line = m.Line + 1
          APPEND BLANK
          REPLACE STYLE WITH lcStyle,;
                  Line  WITH STR(m.Line,6)
        ENDIF
        lcToRep = ALLTRIM(EVAL('SCALE.SZ'+lcGetFrm))
        IF VAL(lcStr) > 14
          LOCATE REST WHILE Style = lcStyle FOR EMPTY(SZ14)
          lcTo = ALLTRIM(STR((VAL(lcStr)-14)))
          REPLACE SZ&lcTo WITH lcToRep
        ELSE
          REPLACE SZ&lcStr WITH lcToRep
        ENDIF
        SELECT(lnOldAls)
      ENDFOR
      *:B#803419,1 SSH Incorect Total
      SELECT (lcMainFBL)
      REPLACE  TotQty  WITH lfCalcTot('Q'),;
               TotOrd  WITH lfCalcTot('')    
      *:B#803419,1 SSH [END]
    ENDIF
  ENDSCAN
  lnCnt    = 0
  lnOldCnt = 0
  m.LineNo = 0
ENDDO
*B#803866,1 AMH restore set skip [Start]
SELECT (lcMainF)
SET SKIP TO &lcSetSkip.
*B#803866,1 AMH [End]

*:----------------------------------------------------------------
*: Name       : lfSetRel      Due To :C#101721
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 23/03/00
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
  SET RELATION TO "1"+cuttkt+style+PADL(LineNo,6) INTO (lcCtPckBL) ADDITIVE
  SET SKIP TO (lcCtPckBL)
ENDIF
SET RELATION TO STYLE+STR(LineNo,6) INTO (lcScaleBL) ADDITIVE
SET RELATION TO CUTTkt+cwarecode+ALLTRIM(STYLE) INTO (lcMainF) ADDITIVE
SET FILTER TO TotQty <> 0
GOTO TOP

*:----------------------------------------------------------------
*: Name       : lfGrpAlloc      Due To :C#101721
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 23/03/00
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
SCAN FOR  !SEEK('1'+CutTkt+ALLTRIM(SUBSTR(STYLE,1,LEN(ALLTRIM(STYLE))-3)),lcCtPckBL)
  lcCut = CutTkt

  *B#803866,1 AMH [Start]
  *lcStyle = ALLTRIM(SUBSTR(STYLE,1,LEN(ALLTRIM(STYLE))-3))
  lcStyle = ALLTRIM(STYLE)
  *B#803866,1 AMH [End]

  SELECT CutPick
  =SEEK('1'+lcCut+lcStyle)

  *B#803866,1 AMH collecting data from CUTPICK to lcCtPck [Start]
  SCAN REST WHILE TranCd+cTktNo+Style = '1'+lcCut+lcStyle
    SCATTER MEMVAR
    m.Style  = lcStyle
    m.Scale = ALLTRIM(SUBSTR(Style,LEN(ALLTRIM(Style))-2))
    INSERT INTO(lcCtPck) FROM MEMVAR
  ENDSCAN
  SELECT (lcCtPck)
  =SEEK('1'+lcCut+lcStyle)
  *B#803866,1 AMH [End]

  DO WHILE !EOF()
    lcOrd   = Order
    lcCut   = cTktNo

    *B#803866,1 AMH the style already trim [Start]
    *lcStyle = ALLTRIM(SUBSTR(STYLE,1,LEN(ALLTRIM(STYLE))-3))
    lcStyle = STYLE
    *B#803866,1 AMH [End]

    SCATTER MEMVAR MEMO
    m.Style  = lcStyle
    lnCnt    = 0
    lnOldCnt = 0
    lnLineNo =0

    *B#803866,1 AMH modifay seek and scan for new index [Start]
    *=SEEK('1'+lcCut+lcStyle)
    =SEEK('1'+lcCut+lcStyle+lcOrd)
    *SCAN REST WHILE trancd+ctktno+style = '1'+lcCut+lcStyle;
              FOR   Order = lcOrd
    SCAN REST WHILE trancd+ctktno+style+order = '1'+lcCut+lcStyle+lcOrd
    *B#803866,1 AMH [End]

      IF &lcMainf..NoteFlag $ 'ST'
        APPEND BLANK
        GATH MEMVAR MEMO
        REPLACE NoteFlag WITH &lcMainf..NoteFlag,;
                notes    WITH &lcMainf..Notes
      ELSE
        lnOldCnt = lnOldCnt+lnCnt

        *B#803866,1 AMH the scale existing in lcCtPck [Start]
        *lcScle = ALLTRIM(SUBSTR(Style,LEN(ALLTRIM(Style))-2))
        lcScle = Scale
        *B#803866,1 AMH [End]

        lnCnt  = IIF(SEEK('S'+lcScle,'SCALE'),SCALE.Cnt,0)
        FOR lnInd = lnOldCnt+1 TO lnOldCnt+lnCnt

          *B#803866,1 AMH select lcCtPck [Start]
          *SELECT CutPick
          SELECT (lcCtPck)
          *B#803866,1 AMH [End]

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
                     Style  WITH lcStyle
            FOR lnI = 1 TO 14
              lcStr1    = ALLTRIM(STR(lnI,2))
              REPLACE Qty&lcStr1 WITH  0
            ENDFOR
          ENDIF
          *:B#803419,1 SSH Incorect Total
          *lcFrm = IIF(MOD(lnInd,8)=0,'8',ALLTRIM(STR(MOD(lnInd,8))))
          *B#803866,1 AMH Incorect Qty [Start]
          *lcFrm = IIF(MOD(lnInd,lnCnt)=0,ALLTRIM(STR(lnCnt)),ALLTRIM(STR(MOD(lnInd,lnCnt))))
          lcFrm = IIF(MOD(lnInd-lnOldCnt,lnCnt)=0,ALLTRIM(STR(lnCnt)),;
                      ALLTRIM(STR(MOD(lnInd-lnOldCnt,lnCnt))))
          *B#803866,1 AMH [End]
          *:B#803419,1 SSH [END]
          lcIn  = IIF(MOD(lnInd,14)=0,'14',ALLTRIM(STR(MOD(lnInd,14))))
          REPLACE Qty&lcIn WITH m.Qty&lcFrm
        ENDFOR
        *:B#803419,1 SSH Incorect Total
        SELECT (lcCtPckBL)
        REPLACE  TotQty  WITH lfCalcTot('Q')
        *:B#803419,1 SSH [END]
      ENDIF      
    ENDSCAN
  ENDDO
ENDSCAN

*:----------------------------------------------------------------
*: Name       : lfUsrVld      Due To :C#101721
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 23/03/00
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
lcTempNAm = gftempName()
USE gcWorkDir+(lcMainFBL) IN 0 AGAIN ALIAS (lcTempNAm)
SELECT (lcTempNAm)
INDEX ON CUTTKT+cWareCode+Style+STR(LineNo,6)+Dyelot+NoteFlag TAG (lcTempNAm)
=SEEK(&lcMainFBL..CutTkt)
DIME laSum[15]
laSum = 0
*-- Sum size quantities for the budget lines for each warehouse.
SUM Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,;
    Qty9,Qty10,Qty11,Qty12,Qty13,Qty14,TotQty WHILE CUTTKT=&lcMainFBL..CUTTKT ;
    FOR NoteFlag = 'N' .AND. cWareCode+TranCD = &lcMainFBL..cWareCode+'1' TO ARRAY laSum
USE IN (lcTempNAm)
SELECT (lnAlias)
RETURN ''
*--- End.

*:----------------------------------------------------------------
*: Name       : lfCalcTot      Due To :C#101721
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 23/03/00
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


*:----------------------------------------------------------------
*: Name       : lfErase      Due To :C#101721
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 23/03/00
*: Purpose    : Function to Clear Temp FIle.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfErase()
*:----------------------------------------------------------------
FUNCTION lfErase

*B#803866,1 AMH clear temp file lcCtPck [Start]
IF USED(lcCtPck)
  USE IN &lcCtPck
  ERASE &gcWorkDir.&lcCtPck+'.DBF'
  ERASE &gcWorkDir.&lcCtPck+'.CDX'
ENDIF
*B#803866,1 AMH [End]

USE IN IIF(USED(lcCtPckBL),lcCtPckBL,0)
ERASE &gcWorkDir.&lcCtPckBL+'.DBF'
ERASE &gcWorkDir.&lcCtPckBL+'.CDX'

USE IN IIF(USED(lcScaleBL),lcScaleBL,0)
ERASE &gcWorkDir.&lcScaleBL+'.DBF'
ERASE &gcWorkDir.&lcScaleBL+'.CDX'

USE IN IIF(USED(lcMainFBL),lcMainFBL,0)
ERASE &gcWorkDir.&lcMainFBL+'.DBF'
ERASE &gcWorkDir.&lcMainFBL+'.CDX'
*--- End Program.