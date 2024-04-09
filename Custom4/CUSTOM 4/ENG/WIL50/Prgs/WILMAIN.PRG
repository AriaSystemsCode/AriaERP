*:***************************************************************************
*: Program file       : WILMAIN
*: Program description: Wilson Main Program
*: Module             : Main Systems (SY)
*: Developer          : Saber A.Razek (SAB)
*: Tracking Job Number: C201556.EXE
*: Date               : 05/03/2013
*:***************************************************************************
*Modifications:
*:***************************************************************************

PARAMETER loFormSet, lcEvntFun, lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*!*************************************************************
*! Name      : lfAddShw2PO
*! Developer : Saber A Razek (SAB)
*! Date      : 02-17-2013
*! Purpose   : Add Show in PO column to Activity Grid
*!*************************************************************
FUNCTION lfAddShw2PO
LPARAMETERS loGrdTasks, lcActiveMode

WITH loGrdTasks
  .ReadOnly = !INLIST(lcActiveMode, 'A', 'E') &&.F.
  LOCAL lnColCnt, lcColCnt
  lnColCnt = .ColumnCount + 1
  lcColCnt = ALLTRIM(STR(lnColCnt))
  .ColumnCount = lnColCnt
  .Column&lcColCnt..ControlSource = 'lshw2po'
  IF TYPE('loFormSet.Ariaform1.Ariacontainer1.grdTasks.Column'+lcColCnt+'.Ariacheckbox1') <> 'O'
    .Column&lcColCnt..AddObject('Ariacheckbox1','Ariacheckbox')
  ENDIF   
  .Column&lcColCnt..CurrentControl ='Ariacheckbox1'
  .Column&lcColCnt..Sparse = .F.
  .Column&lcColCnt..Ariacheckbox1.Caption = ''  
  .Column&lcColCnt..Ariacheckbox1.Visible = INLIST(lcActiveMode, 'A', 'E') &&.T.
  .Column&lcColCnt..Header1.Caption = "Send in PO"
  .Column&lcColCnt..ColumnOrder = 3
  FOR lnColIndx = 3 TO .ColumnCount-1
    lcColIndx = ALLTRIM(STR(lnColIndx))
    .Column&lcColIndx..ColumnOrder = lnColIndx+1
  ENDFOR
  
ENDWITH
  
ENDFUNC
*-- end of lfAddShw2PO.


*!*************************************************************
*! Name      : lfEnaShw2PO
*! Developer : Saber A Razek (SAB)
*! Date      : 02-17-2013
*! Purpose   : Enabled Show in PO column in Activity Grid and make it read write
*!*************************************************************
FUNCTION lfEnaShw2PO
LPARAMETERS loGrdTasks, lcActiveMode

WITH loGrdTasks
  .ReadOnly = !INLIST(lcActiveMode, 'A', 'E')
  FOR lnColIndx = 1 TO .ColumnCount
    lcColIndx = ALLTRIM(STR(lnColIndx))
    .Column&lcColIndx..ReadOnly = INLIST(lcActiveMode, 'A', 'E')
  ENDFOR
  lcColCnt = ALLTRIM(STR(.ColumnCount))
  .Column&lcColCnt..ReadOnly = !INLIST(lcActiveMode, 'A', 'E')
  .Column&lcColCnt..Enabled = .T.
  .Column&lcColCnt..Ariacheckbox1.ReadOnly = !INLIST(lcActiveMode, 'A', 'E')
  .Column&lcColCnt..Ariacheckbox1.Enabled = .T.
  .Column&lcColCnt..Ariacheckbox1.Visible = INLIST(lcActiveMode, 'A', 'E')
ENDWITH

ENDFUNC
*-- end of lfEnaShw2PO.


*!*************************************************************
*! Name      : lfSavShw2PO
*! Developer : Saber A Razek (SAB)
*! Date      : 02-17-2013
*! Purpose   : Update lShw2PO field from grdTasks to PMPTHDT/PMPRJDT Table
*!*************************************************************
FUNCTION lfSavShw2PO
LPARAMETERS loGrdTasks, lcForm

LOCAL lcActTmp, lcGridTmp, lcAlias, lnTmpRecNo, lnRecNo
lcGridTmp = loGrdTasks.RecordSource
lcActTmp  = IIF((UPPER(lcForm) = 'MFPATHA'), "PMPTHDT", "PMPRJDT")
lcAlias = SELECT(0)

SELECT (lcGridTmp)
lnTmpRecNo = RECNO()
SELECT (lcActTmp)
lnRecNo = RECNO()

IF (UPPER(lcForm) = 'MFPATHA')
  SELECT (lcActTmp)
  SCAN
    =gfReplace('')
  ENDSCAN
ELSE
  SELECT (lcGridTmp)
  SCAN
    SELECT (lcActTmp)  
    LOCATE FOR CPRJ_TYP+CPRJ_ID+COPRT_CTG+COPRT_ID = &lcGridTmp..CPRJ_TYP+&lcGridTmp..CPRJ_ID+&lcGridTmp..COPRT_CTG+&lcGridTmp..COPRT_ID
    IF FOUND()
      REPLACE LSHW2PO WITH &lcGridTmp..LSHW2PO
      =gfReplace('')
    ENDIF
  ENDSCAN
ENDIF

SELECT (lcGridTmp)
IF BETWEEN(lnTmpRecNo, 1, RECCOUNT())
  GOTO lnTmpRecNo
ENDIF
SELECT (lcActTmp)
IF BETWEEN(lnRecNo, 1, RECCOUNT())
  GOTO lnRecNo
ENDIF
SELECT (lcAlias)

ENDFUNC
*-- end of lfSavShw2PO.