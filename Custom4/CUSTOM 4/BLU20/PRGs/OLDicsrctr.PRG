*:*************************************************************************
*:  Program file : ICSRCTR.PRG
*:  Program desc.: Serialized Inventory Control Screen
*:         System: Aria 4XP
*:      Developer: Khalid Mohi El-Din Mohamed
*:           Date: 05/30/2006
*:      Reference: *C131527,1
*:************************************************************************
*: Passed Parameters  :lcIRTran    => 'I' Issue
*:                                    'R' Receive
*:                     lcTranTyp   => '6' Receive PO
*:                     lcTranTyp   => '0' Return Authorization
*:                     lcSessArr   => Array of Session Numbers
*:                     llDontAsk   => To Ask before to generate or not
*:*************************************************************************
*: Modifications      :
*B609779,1 MMT 12/25/2011 Wrong message statement in custom serialization program[T20110608.0020]
*:*************************************************************************

*WSH [Start]
*PARAMETERS lcIRTran, lcTranTyp, lcSessionNo
PARAMETERS lcIRTran, lcTranTyp, lcSessArr, llDontAsk
*WSH [End]

**************
ON ERROR DO gfEHan WITH ERROR(), MESSAGE(), ;
    MESSAGE(1), SYS(16), LINENO(), SYS(102), ;
    SYS(100), SYS(101), LASTKEY(), ALIAS(), ;
    SYS(5), SYS(12), SYS(6), SYS(2003), ;
    WONTOP(), SYS(2011), SYS(2018), SET("CURSOR")
**************

PUBLIC laScopExpr, lcSelTran
PRIVATE lcAllVar
lcAllVar  = "lcTmpSyInv,lcTmpDet,lcTmpDet2,lcTmpDet3,lcTmpDet4,lcStyHdr,llCalGrid,llExternal,lnMajLen,"+;
            "lnNonMajSt,lnNonMajLen,llDontAsk"
lcSelTran = 'A'

DIMENSION laAllVar[1,1], laScopExpr[1]
STORE '' TO laAllVar, laScopExpr
STORE 0 TO lnMajLen,lnNonMajSt,lnNonMajLen

=gfSubStr(lcAllVar,@laAllVar,',')

LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
  PRIVATE &laAllVar[lnI,1].
ENDFOR

STORE IIF(TYPE('lcTranTyp') $ 'LU', .T.,.F.) TO llCalGrid
STORE IIF(TYPE('lcTranTyp') $ 'LU', .F.,.T.) TO llExternal

lcIRTran   = IIF(TYPE('lcIRTran') <> 'C','',lcIRTran) 
lcTranTyp  = IIF(TYPE('lcTranTyp') <> 'C','',lcTranTyp) 

*WSH [Start]
*lcSessionNo = IIF(TYPE('lcSessionNo') <> 'C','',lcSessionNo)
*WSH [End]

lcStyHdr     = ALLTRIM(gfItemMask('HI','','0001'))
lcTmpSyInv   = gfTempName()
lcTmpDet     = gfTempName()
lcTmpDet2    = gfTempName() && 2nd index in table lcTmpDet
lcTmpDet3    = gfTempName() && 3rd index in table lcTmpDet
lcTmpDet4    = gfTempName() && 4th index in table lcTmpDet

*-- If the program is called from the transactions and the answer of the message was yes
*-- then the program will geneate the serial numbers for each piece and dispaly the screen.
*-- if No, then the program will add records to the custom file INVSERNO without serial 
*-- numbers. The user will assign the serial number when runs the screen as stand alone.

*-- Would you like to genrate serial numbers now?
*llGenSrNow = llExternal AND gfModalGen('TRM42266B42002','DIALOG') = 1
*B609779,1 MMT 12/25/2011 Wrong message statement in custom serialization program[T20110608.0020][Start]
*IF !llDontAsk AND llExternal AND gfModalGen('TRM42266B42002','DIALOG') = 2
IF !llDontAsk AND llExternal AND gfModalGen('TRM42280B42002','DIALOG') = 2
*B609779,1 MMT 12/25/2011 Wrong message statement in custom serialization program[END]
  RETURN
ENDIF

*WSH [Start]
*llGenSrNow = llExternal
*WSH [End]

*!*  IF !llCalGrid AND !llGenSrNow 
*!*    *-- Open tables
*!*    =lfOpenTable()
*!*    *-- To add a record for each piece in the custom table with serial number 
*!*    =lfAddInvSR(.F.,lcSessionNo,lcIRTran,lcTranTyp, .F.)
*!*    =gfCloseTable('StyInvJl')
*!*    =gfCloseTable('Scale')
*!*    =gfCloseTable('Style')
*!*    RETURN
*!*  ENDIF

*WSH [Start]
*DO FORM (oAriaApplication.ScreenHome+"ICSRCTR.SCX") WITH lcIRTran, lcTranTyp, lcSessionNo
DO FORM (oAriaApplication.ScreenHome+"ICSRCTR.SCX") WITH lcIRTran, lcTranTyp, lcSessArr
*WSH [End]

*!*****************************************************************************************
*! Name      : lfSRInit
*! Developer : Khalid Mohi El-Din
*! Date      : 05/30/2006
*! Purpose   : Init fuction
*!*****************************************************************************************
FUNCTION lfSRInit
LPARAMETERS loFormSet

SET MULTILOCKS ON

*-- Open tables
=lfOpenTable()

*-- Add needed properties
=lfAddPro(loFormSet)

*-- Create temporary files
=lfCrtTmpFl(loFormSet)
*-- End of lfSRInit

DECLARE loFormSet.laPanelObj[3,6]
STORE '' TO loFormSet.laPanelObj
loFormSet.laPanelObj[1,1] = 'pbScop'
loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"SCOPE.bmp"
loFormSet.laPanelObj[1,3] = 'mScope'
loFormSet.laPanelObj[1,4] = 'Scope'
loFormSet.laPanelObj[1,5] = 'Scope'
loFormSet.laPanelObj[1,6] = 'S'

loFormSet.laPanelObj[2,1] = 'pbGenSerNo'
loFormSet.laPanelObj[2,2] = oAriaApplication.BitMapHome+"GENERAT.BMP"
loFormSet.laPanelObj[2,3] = 'mGenSerNo'
loFormSet.laPanelObj[2,4] = 'Generate Serial Numbers'
loFormSet.laPanelObj[2,5] = 'Generate Serial Numbers'
loFormSet.laPanelObj[2,6] = 'V'

loFormSet.laPanelObj[3,1] = 'pbSelSerNo'
loFormSet.laPanelObj[3,2] = oAriaApplication.BitMapHome+"OPEN.bmp"
loFormSet.laPanelObj[3,3] = 'mSelSerNo'
loFormSet.laPanelObj[3,4] = 'Select Serial Numbers'
loFormSet.laPanelObj[3,5] = 'Select Serial Numbers'
loFormSet.laPanelObj[3,6] = 'V'

*WSH [Start]
*WITH loFormSet.AriaForm1
*  IF loFormSet.llExternal AND loFormSet.lcTranTypS = '6'
*    *.cmdGenLbl.Visible = .F.
*    *.cmdSelectLbl.Visible = .F.
*    .lblScanSerNo.Visible = .F.
*    .txtSerNo.Visible = .F.
*  ENDIF
*ENDWITH
*WSH [End]

*!*************************************************************
*! Name      : lfAddPro
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : function to Add properties to the FormSet.
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfAddPro
LPARAMETERS loFormSet

LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
  IF LEFT(laAllVar[lnI,1],2) = 'la'
    LOCAL lnRow,lnCol,lcACopy
    lnRow = ALEN(laAllVar[lnI,1],1)
    lnCol = ALEN(laAllVar[lnI,1],2)
    loFormSet.AddProperty(laAllVar[lnI,1]+'['+ALLTRIM(STR(lnRow))+;
                                          ','+ALLTRIM(STR(lnCol))+']')
    lcACopy = '=ACOPY(' + laAllVar[lnI,1] + ',loFormSet.' + laAllVar[lnI,1] + ')'
    &lcACopy.
  ELSE
    loFormSet.AddProperty(laAllVar[lnI,1],EVALUATE(laAllVar[lnI,1]))
  ENDIF
ENDFOR
*--end of lfAddPro.

*!*************************************************************
*! Name      : lfOpenTable
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To open the required tables
*!*************************************************************
FUNCTION lfOpenTable

*WSH [Start]
=gfOpenTable(oAriaApplication.DataDir+'INVSERNO',oAriaApplication.DataDir+'SESSION','SH')
=gfOpenTable(oAriaApplication.DataDir+'SEQUENCE',oAriaApplication.DataDir+'CSEQ_TYPE','SH')
*WSH [End]

=gfOpenTable(oAriaApplication.DataDir+'ICSTYSER',oAriaApplication.DataDir+'CSTYLE','SH')
=gfOpenTable(oAriaApplication.DataDir+'StyleUPC',oAriaApplication.DataDir+'StyleUPC','SH')
=gfOpenTable(oAriaApplication.DataDir+'StyInvJl',oAriaApplication.DataDir+'MfgOpr','SH')
=gfOpenTable(oAriaApplication.DataDir+'Scale',oAriaApplication.DataDir+'Scale','SH')
=gfOpenTable(oAriaApplication.DataDir+'Style',oAriaApplication.DataDir+'Style','SH')

*-- end of lfOpenTable

*!*************************************************************
*! Name      : lfCrtTmpFl
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To create temporary files
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfCrtTmpFl
LPARAMETERS loFormSet

LOCAL lnCount, laIndex
PRIVATE laFileStru

SELECT StyInvJl
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+4,18]

laFileStru[lnFileStru+1,1] = 'lMark'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lPrint'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'cTranDesc'
laFileStru[lnFileStru+3,2] = 'C'
laFileStru[lnFileStru+3,3] = '30'
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'cScale'
laFileStru[lnFileStru+4,2] = 'C'
laFileStru[lnFileStru+4,3] = '3'
laFileStru[lnFileStru+4,4] = 0

FOR lnCount = 1 TO 4
  STORE '' TO laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
              laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
              laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
              laFileStru[lnFileStru+lnCount,16]
  STORE 0 TO  laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
ENDFOR

DIMENSION laIndex[1,2]
laIndex = ''

*WSH [Start]
*laIndex[1,1] = 'cSession+cTrCode+Style+cWareCode'
laIndex[1,1] = 'cSession+cTrCode+cTrType+cIrType+STR(LineNo,6)+Style+cWareCode'
*WSH [End]

laIndex[1,2] = loFormSet.lcTmpSyInv

=gfCrtTmp(loFormSet.lcTmpSyInv,@laFileStru,@laIndex)

laFileStru = ''
DIMENSION laFileStru[16,18]
laFileStru[1,1] = 'lPrint'
laFileStru[1,2] = 'L'
laFileStru[1,3] = 1
laFileStru[1,4] = 0
laFileStru[2,1] = 'cStyle'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 19
laFileStru[2,4] = 0
laFileStru[3,1] = 'cSize'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 5
laFileStru[3,4] = 0
laFileStru[4,1] = 'cSerialNo'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 8
laFileStru[4,4] = 0
laFileStru[5,1] = 'nQty'
laFileStru[5,2] = 'N'
laFileStru[5,3] = 6
laFileStru[5,4] = 0
laFileStru[6,1] = 'cReference'
laFileStru[6,2] = 'C'
laFileStru[6,3] = 30
laFileStru[6,4] = 0
laFileStru[7,1] = 'cSizeNo'
laFileStru[7,2] = 'C'
laFileStru[7,3] = 1
laFileStru[7,4] = 0
laFileStru[8,1] = 'cSession'
laFileStru[8,2] = 'C'
laFileStru[8,3] = 6
laFileStru[8,4] = 0
laFileStru[9,1] = 'cStatus'
laFileStru[9,2] = 'C'
laFileStru[9,3] = 1
laFileStru[9,4] = 0
laFileStru[10,1] = 'LineNo'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 6
laFileStru[10,4] = 0
laFileStru[11,1] = 'cRecStat'
laFileStru[11,2] = 'C'
laFileStru[11,3] = 1
laFileStru[11,4] = 0
laFileStru[12,1] = 'cIRType'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 1
laFileStru[12,4] = 0
laFileStru[13,1] = 'cWareCode'
laFileStru[13,2] = 'C'
laFileStru[13,3] = 6
laFileStru[13,4] = 0
laFileStru[14,1] = 'nLineNo'
laFileStru[14,2] = 'N'
laFileStru[14,3] = 6
laFileStru[14,4] = 0
laFileStru[15,1] = 'cTrCode'
laFileStru[15,2] = 'C'
laFileStru[15,3] = 6
laFileStru[15,4] = 0
laFileStru[16,1] = 'cTrType'
laFileStru[16,2] = 'C'
laFileStru[16,3] = 6
laFileStru[16,4] = 0

lnCount = 0
FOR lnCount = 1 TO 16
  STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
              laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
              laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
              laFileStru[lnCount,16]
  STORE 0 TO  laFileStru[lnCount,17],laFileStru[lnCount,18]
ENDFOR

DIMENSION laIndex[4,2]
laIndex = ''

*WSH [Start]
*laIndex[1,1] = 'cSession+cStyle+STR(nLineNo,6)+cSizeNo+cSerialNo'
laIndex[1,1] = 'cSession+cStyle+STR(nLineNo,6)+cTrCode+cIrType+cSizeNo+STR(LineNo,6)'
*WSH [End]

laIndex[1,2] = loFormSet.lcTmpDet

*WSH [Start]
*laIndex[2,1] = 'cSerialNo+cIRType+cStyle+cSizeNo'
laIndex[2,1] = 'cSerialNo+cStyle+cSize+cIRType'
*WSH [End]

laIndex[2,2] = loFormSet.lcTmpDet2
laIndex[3,1] = 'cRecStat+cSerialNo'
laIndex[3,2] = loFormSet.lcTmpDet3

*WSH [Start]
laIndex[4,1] = 'cStyle+cSize+cWareCode+cSerialNo'
laIndex[4,2] = loFormSet.lcTmpDet4
*WSH [End]

=gfCrtTmp(loFormSet.lcTmpDet,@laFileStru,@laIndex)
SELECT(loFormSet.lcTmpDet)
SET ORDER TO (loFormSet.lcTmpDet)

SELECT (loFormSet.lcTmpSyInv)
*-- end of lfCrtTmpFl

*!*************************************************************
*! Name      : lfBuildGrd
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To build grid columns
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfBuildGrd
LPARAMETERS loFormSet

*WSH [Start]
LOCAL lnAlias
lnAlias = SELECT(0)
*WSH [End]

PRIVATE lcTmpFile, lcTmpDetail
lcTmpFile = loFormSet.lcTmpSyInv
WITH loFormSet.ariaform1.grdTransactions 
  .RecordSource = ''
  .ColumnCount = 18
  .RecordSource = lcTmpFile
  .Columns(1).Header1.Caption = 'Mark'
  .Columns(1).WIDTH = 30
  .Columns(1).CONTROLSOURCE = lcTmpFile +'.lMark'
  .Columns(1).CurrentControl = "Ariacheckbox1"

  .Columns(2).Header1.Caption = 'Print'
  .Columns(2).WIDTH = 30
  .Columns(2).CONTROLSOURCE = lcTmpFile +'.lPrint'
  .Columns(2).CurrentControl = "Ariacheckbox1"

  .Columns(3).Header1.Caption = 'Trans No.'
  .Columns(3).WIDTH = 60
  .Columns(3).CONTROLSOURCE = lcTmpFile +'.cTrCode'
  .Columns(3).ReadOnly = .T.

  .Columns(4).Header1.Caption = 'Transaction Type'
  .Columns(4).WIDTH = 150
  .Columns(4).CONTROLSOURCE = lcTmpFile +'.cTranDesc'
  .Columns(4).ReadOnly = .T.
  
  .Columns(5).Header1.Caption = 'Type'
  .Columns(5).WIDTH = 30
  .Columns(5).CONTROLSOURCE = lcTmpFile +'.cIRType'
  .Columns(5).ReadOnly = .T.

  .Columns(6).Header1.Caption = 'Session #'
  .Columns(6).WIDTH = 60
  .Columns(6).CONTROLSOURCE = lcTmpFile +'.cSession'
  .Columns(6).ReadOnly = .T.

  .Columns(7).Header1.Caption = loFormSet.lcStyHdr
  .Columns(7).WIDTH = 120
  .Columns(7).CONTROLSOURCE = lcTmpFile +'.Style'
  .Columns(7).ReadOnly = .T.
  
  .Columns(8).Header1.Caption = 'Transaction Date'
  .Columns(8).WIDTH = 100
  .Columns(8).CONTROLSOURCE = lcTmpFile +'.dTrDate'
  .Columns(8).ReadOnly = .T.

  .Columns(9).Header1.Caption = 'Qty1 '
  .Columns(9).Header1.Alignment = 1     && Right
  .Columns(9).WIDTH = 60
  .Columns(9).CONTROLSOURCE = lcTmpFile +'.nStk1'
  .Columns(9).ReadOnly = .T.

  .Columns(10).Header1.Caption = 'Qty2 '
  .Columns(10).Header1.Alignment = 1     && Right
  .Columns(10).WIDTH = 60
  .Columns(10).CONTROLSOURCE = lcTmpFile +'.nStk2'
  .Columns(10).ReadOnly = .T.

  .Columns(11).Header1.Caption = 'Qty3 '
  .Columns(11).Header1.Alignment = 1     && Right
  .Columns(11).WIDTH = 60
  .Columns(11).CONTROLSOURCE = lcTmpFile +'.nStk3'
  .Columns(11).ReadOnly = .T.

  .Columns(12).Header1.Caption = 'Qty4 '
  .Columns(12).Header1.Alignment = 1     && Right
  .Columns(12).WIDTH = 60
  .Columns(12).CONTROLSOURCE = lcTmpFile +'.nStk4'
  .Columns(12).ReadOnly = .T.

  .Columns(13).Header1.Caption = 'Qty5 '
  .Columns(13).Header1.Alignment = 1     && Right
  .Columns(13).WIDTH = 60
  .Columns(13).CONTROLSOURCE = lcTmpFile +'.nStk5'
  .Columns(13).ReadOnly = .T.

  .Columns(14).Header1.Caption = 'Qty6 '
  .Columns(14).Header1.Alignment = 1     && Right
  .Columns(14).WIDTH = 60
  .Columns(14).CONTROLSOURCE = lcTmpFile +'.nStk6'
  .Columns(14).ReadOnly = .T.

  .Columns(15).Header1.Caption = 'Qty7 '
  .Columns(15).Header1.Alignment = 1     && Right
  .Columns(15).WIDTH = 60
  .Columns(15).CONTROLSOURCE = lcTmpFile +'.nStk7'
  .Columns(15).ReadOnly = .T.

  .Columns(16).Header1.Caption = 'Qty8 '
  .Columns(16).Header1.Alignment = 1     && Right
  .Columns(16).WIDTH = 60
  .Columns(16).CONTROLSOURCE = lcTmpFile +'.nStk8'
  .Columns(16).ReadOnly = .T.

  .Columns(17).Header1.Caption = 'Reference'
  .Columns(17).WIDTH = 120
  .Columns(17).CONTROLSOURCE = lcTmpFile +'.Reference'
  .Columns(17).ReadOnly = .T.

  .Columns(18).Header1.Caption = 'Location'
  .Columns(18).WIDTH = 80
  .Columns(18).CONTROLSOURCE = lcTmpFile +'.cWareCode'
  .Columns(18).ReadOnly = .T.

ENDWITH

*WSH [Start]
SELECT (lcTmpFile)
LOCATE
*WSH [End]

lcTmpDetail = loFormSet.lcTmpDet
WITH loFormSet.ariaform1.grdTranDetail
  .ColumnCount = 6
  .RecordSource = ''
  .RecordSource = lcTmpDetail
  .Columns(1).Header1.Caption = 'Print'
  .Columns(1).WIDTH = 30
  .Columns(1).CONTROLSOURCE = lcTmpDetail +'.lPrint'
  .Columns(1).CurrentControl = "Ariacheckbox1"

  .Columns(2).Header1.Caption = loFormSet.lcStyHdr
  .Columns(2).WIDTH = 180
  .Columns(2).CONTROLSOURCE = lcTmpDetail +'.cStyle'
  .Columns(2).ReadOnly = .T.
  
  .Columns(3).Header1.Caption = 'Size'
  .Columns(3).WIDTH = 50
  .Columns(3).CONTROLSOURCE = lcTmpDetail +'.cSize'
  .Columns(3).ReadOnly = .T.

  .Columns(4).Header1.Caption = 'Qty '
  .Columns(4).Header1.Alignment = 1     && Left
  .Columns(4).WIDTH = 60
  .Columns(4).CONTROLSOURCE = lcTmpDetail +'.nQty'
  .Columns(4).Alignment = 1     && Left
  .Columns(4).ReadOnly = .T.

  .Columns(5).Header1.Caption = 'Serial Number'
  .Columns(5).WIDTH = 150
  .Columns(5).CONTROLSOURCE = lcTmpDetail +'.cSerialNo'
  .Columns(5).ReadOnly = .T.

  .Columns(6).Header1.Caption = 'Reference'
  .Columns(6).WIDTH = 300
  .Columns(6).CONTROLSOURCE = lcTmpDetail +'.cReference'
  .Columns(6).ReadOnly = .T.
ENDWITH

*WSH [Start]
SELECT (lcTmpDetail)
LOCATE

SELECT (lnAlias)
*WSH [End]

*-- end of lfBuildGrd

*!*************************************************************
*! Name      : lfChgMode
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : To handle the modes of the screen
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!             lcScrMode : 'S' Select, 'V' View, 'A' Add 
*!*************************************************************
FUNCTION lfChgMode
LPARAMETERS loFormSet, lcScrMode

IF EMPTY(lcScrMode) OR lcScrMode = 'S'
  loFormSet.AriaForm1.grdTransactions.RecordSource = ''
  loFormSet.AriaForm1.grdTranDetail.RecordSource   = ''
ELSE
  *-- To build the header and detail grids
  =lfBuildGrd(loFormSet)
ENDIF

loFormSet.oToolBar.ChangeButtonStatus('pbScop', IIF(lcScrMode = 'A', 'DISABLED', 'ENABLED'))
loFormSet.oToolBar.ChangeButtonStatus('pbGenSerNo', IIF(lcScrMode = 'A', 'ENABLED', 'DISABLED'))
loFormSet.oToolBar.cmdAdd.ToolTipText  = IIF(lcScrMode $ 'A', 'Save modifications', 'Add new record')
loFormSet.oToolBar.cmdExit.ToolTipText = IIF(lcScrMode $ 'S ', 'Close Current Form', 'Undo Changes')
*-- end of lfChgMode

*!*************************************************************
*! Name      : lfvScope
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To display the option grid
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfvScope
LPARAMETERS loFormSet

IF loFormSet.llCalGrid

  lcDataSess = loFormSet.DatasessionID

  oGetItemMask = CREATEOBJECT('GetItemMask')
  lnMajSeg   = oGetItemMask.Do('SM')  && No. of major segments.
  DIMENSION laMajSegs[1,1]
  oGetItemMask.Do(@laMajSegs)
  STORE 0  TO lnNonMajSt
  STORE "" TO lcNonMajPi,lcNonMajTl
  
  *WSH [Start]
  STORE 'A' TO lcSelTran
  *WSH [End]
  
  *-- Loop Around Non Major elements.
  FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
    IF laMajSegs[lnI,1] $ 'CF'
      lcFree_Clr = laMajSegs[lnI,1]
      lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
      lnSupMajSt = lnNonMajSt
      lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                   laMajSegs[lnI,3],;
                   lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
      lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                   PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                   lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    ENDIF                     
    
    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
      EXIT
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
  ENDFOR    && end Loop Around Non Major elements.
  lcColorTlt =  ALLTRIM(lcNonMajTl)
  lcStyTitle = gfItemMask('HM') 
  STORE "" TO lcNonMajPi,lcNonMajTl,lcRPTrans
  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  
  lcExpr1 = gfOpGrid('ICSTCTR',.T.,.F.,.F.,.T.,.T.)
  SET DATASESSION TO lcDataSess 
  loFormSet.llCalGrid = .F.
  
  =lfGetItemMask(loFormSet)
  
  *-- Collect Data based on option grid criteria
  IF lcExpr1 <> '.F.'
    
    *WSH [Start]
    loFormSet.AriaForm1.grdTransactions.RecordSource = ''
    loFormSet.AriaForm1.grdTranDetail.RecordSource   = ''
    *WSH [End]
    
    SELECT(loFormSet.lcTmpSyInv)
    ZAP
    SELECT(loFormSet.lcTmpDet)
    
    *WSH [Start]
    SET KEY TO
    *WSH [End]
    
    ZAP
    
    =lfColData(loFormSet)
    IF !EOF(loFormSet.lcTmpSyInv)
      loFormSet.ChangeMode("A")
    ENDIF
  ENDIF
  
  *WSH [Start]
  *loFormSet.oToolBar.ChangeButtonStatus('pbScop','ENABLED')
  *WSH [End]
  
ENDIF  
*-- end of lfvScope

*!*************************************************************
*! Name      : lfColData
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : Collect data from based on option grid selection criteria.
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfColData
LPARAMETERS loFormSet

PRIVATE lcForExp, lcStyFile, lcTranFile
lcForExp = ".T."

LOCAL llSelStyle, llSelTran

*WSH [Start]
*-- CSESSION+STYLE+SZ1+CIRTYPE+STR(LINENO,6)+CSER_NUM
*=gfOpenTable(oAriaApplication.DataDir+'INVSERNO',oAriaApplication.DataDir+'SESSION','SH')
SELECT INVSERNO
=gfSetOrder("SESSION")
*WSH [End]

*-- Style Range
STORE .F. TO llSelStyle, llSelTran
lcValue = lfGetFilter("STYLE.CSTYMAJOR")
IF !EMPTY(lcValue)
  lcStyFile  = lcValue 
  IF !EMPTY(lcStyFile) AND USED(lcStyFile) AND RECCOUNT(lcStyFile)> 0 
     llSelStyle = .T.
  ENDIF
ENDIF

*-- Style Color
lcValue = lfGetFilter("SUBSTR(STYINVJL.STYLE,lnNonMajSt,lnColorLen)")
IF !EMPTY(lcValue)
  lcForExp   = lcForExp + ;
               " AND  SUBSTR(Style,loFormSet.lnNonMajSt,loFormSet.lnNonMajLen) $ '" + lcValue + "'"
ENDIF

*-- Style Division
lcValue = lfGetFilter("STYINVJL.CDIVISION")
IF !EMPTY(lcValue)
  lcForExp   = lcForExp + " AND  Style.cDivision $ '" + lcValue + "'"
ENDIF

*-- Style Group
lcValue = lfGetFilter("STYINVJL.CSTYGROUP")
IF !EMPTY(lcValue)
  lcForExp   = lcForExp + " AND  Style.cStyGroup $ '" + lcValue + "'"
ENDIF

*-- Date Range
lcValue = lfGetFilter("STYINVJL.DTRDATE")
IF !EMPTY(lcValue)
  lnSpPos = ATC('|', lcValue)
  lcDate1 = DTOS(CTOD(IIF(lnSpPos > 0, SUBSTR(lcValue, 1, lnSpPos - 1), ALLTRIM(lcValue))))
  lcDate2 = DTOS(CTOD(IIF(lnSpPos > 0, SUBSTR(lcValue, lnSpPos + 1), "")))
  IF !EMPTY(lcDate2)
    IF lcSelTran <> 'T'
      lcForExp   = lcForExp  + " AND BETWEEN(DTOS(STYINVJL.DtrDate), '" + lcDate1 + "', '" + lcDate2 + "')"
    ELSE
      lcForExp   = lcForExp  + " AND BETWEEN(DTOS(RETAUTH.RaDate), '" + lcDate1 + "', '" + lcDate2 + "')"
    ENDIF
  ENDIF
ENDIF

*-- Transaction Type
*-- All|Receive PO|Invoice Sales Order|Return Authorization|Credit Memo|Physical Inventory|
*-- Inventory Adjustment~A|R|I|T|C|P|D
DO CASE
  *-- By Receive PO
  CASE lcSelTran = 'R'
    lcForExp = lcForExp + " AND StyInvJl.cTrType = '6'"

    *-- PO #
    lcValue = lfGetFilter("POSHDR.PO")
    IF !EMPTY(lcValue)
      lcTranFile  = lcValue 
      llSelTran = !EMPTY(lcTranFile) AND USED(lcTranFile) AND RECCOUNT(lcTranFile)> 0 
    ENDIF
    
  *-- By Invoice
  CASE lcSelTran = 'I'
    lcForExp = lcForExp + " AND StyInvJl.cTrType = '3'"
    *-- Invoice #
    lcValue = lfGetFilter("INVHDR.INVOICE")
    IF !EMPTY(lcValue)
      lcTranFile  = lcValue 
      llSelTran = !EMPTY(lcTranFile) AND USED(lcTranFile) AND RECCOUNT(lcTranFile)> 0 
    ENDIF
    
  *-- By Return Authorization
  CASE lcSelTran = 'T'
    *-- Return Authorization
    lcValue = lfGetFilter("RETAUTH.RANO")
    IF !EMPTY(lcValue)
      lcTranFile  = lcValue 
      llSelTran = !EMPTY(lcTranFile) AND USED(lcTranFile) AND RECCOUNT(lcTranFile)> 0 
    ENDIF
    
  *-- By Physical Inventory
  CASE lcSelTran = 'P'
    
    *WSH [Start]
    *lcForExp = lcForExp + " AND StyInvJl.cTrType = '2' AND StyInvJl.cIRType = 'R'"
    lcForExp = lcForExp + " AND StyInvJl.cTrType = '2'"
    *WSH [End]
    
  *-- By Credit Memo
  CASE lcSelTran = 'C'
    lcForExp = lcForExp + " AND StyInvJl.cTrType = '7'"
    *-- Invoice #
    lcValue = lfGetFilter("RETHDR.CRMEMO")
    IF !EMPTY(lcValue)
      lcTranFile  = lcValue 
      llSelTran = !EMPTY(lcTranFile) AND USED(lcTranFile) AND RECCOUNT(lcTranFile)> 0 
    ENDIF
    
  *-- By Inventory Adjustment
  CASE lcSelTran = 'D'
    
  *WSH [Start]
    lcForExp = lcForExp + " AND StyInvJl.cTrType = '1'"
  
  *-- By Void Invoice
  CASE lcSelTran = 'V'
    lcForExp = lcForExp + " AND StyInvJl.cTrType = '4'"
  
  *-- By Void Credit Memo
  CASE lcSelTran = 'M'
    lcForExp = lcForExp + " AND StyInvJl.cTrType = '8'"
  *WSH [End]
    
  *-- By All
  CASE lcSelTran = 'A'
    
    *-- '1' Inventory Adjustment, '2' Physical Inventory, '3' Sales Order Invoice
    *-- '6' Receive PO, '7' Credit Memo   
    
    *WSH [Start]
    *lcForExp = lcForExp + " AND (StyInvJl.cTrType $ '1367' OR (StyInvJl.cTrType = '2' AND StyInvJl.cIRType = 'R'))"
    lcForExp = lcForExp + " AND StyInvJl.cTrType $ '1234678'"
    *WSH [End]
    
ENDCASE

IF llSelStyle AND llSelTran
  lcForExp = lcForExp + " AND SEEK(cTrCode,lcTranFile)"
ENDIF

*WSH [Start]
IF llSelTran
  llSelStyle = .F.
ENDIF
*WSH [End] [Start]

loFormSet.AriaForm1.LockScreen = .T.

DO CASE 
  *-- Case select Styles
  CASE llSelStyle
    =lfSelBySty(loFormSet, lcForExp)
    
  *-- Case by Transaction
  CASE llSelTran
    IF lcSelTran = 'T'
      =lfSelByRA(loFormSet, lcForExp)
    ELSE  
      =lfSelByTrn(loFormSet, lcForExp)
    ENDIF
    
  *-- Case by All
  
  *WSH [Start]
  *CASE !llSelStyle AND !llSelTran
  OTHERWISE
  *WSH [End]
  
    *-- we will use the custom file for the lookup screen which will hold all the styles
    *-- that were marked to maintain the seril number and loop through the StyInvJl
    =lfSelByAll(loFormSet, lcForExp)
    
ENDCASE

loFormSet.AriaForm1.LockScreen = .F.
*-- end of lfColData

*!*************************************************************
*! Name      : lfSelBySty
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : Case select styles from option grid
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!             lcForExp  : For expression
*!*************************************************************
FUNCTION lfSelBySty
LPARAMETERS loFormSet, lcForExp

LOCAL lnCnt

*WSH [Start]
IF lcSelTran $ 'AT'
  IF !USED('RALINE')
    =gfOpenTable(oAriaApplication.DataDir+'RALINE',oAriaApplication.DataDir+'RALINES','SH')
  ENDIF
  IF !USED('RETAUTH')
    =gfOpenTable(oAriaApplication.DataDir+'RETAUTH',oAriaApplication.DataDir+'RETAUTH','SH')
  ENDIF

  SELECT RALINE
  =gfSetOrder("RALINES")

  SELECT RETAUTH
  =gfSetOrder("RETAUTH")
ENDIF
*WSH [End]

SELECT StyInvJl
gfSETORDER("STYINVJL")

SELECT (lcStyFile)
SCAN
  lcStyle = SUBSTR(cStyMajor,1,loFormSet.lnMajLen)
  
  *WSH [Start]
  *IF gfSeek(lcStyle,'StyInvJl')
  IF !lfChkMaitSR(lcStyle)
    LOOP
  ENDIF
  
  =gfSeek(lcStyle, 'Style')
  =gfSeek('S'+Style.Scale, 'Scale')
  
  WAIT WINDOW 'Style : ' + lcStyle NOWAIT
  *WSH [End]
  
  IF lcSelTran <> 'T' AND gfSeek(lcStyle,'StyInvJl')
    SELECT StyInvJl
    SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrDate)+cTrCode+STR(LineNo,6) = lcStyle
      
      *WSH [Start]
      *=gfSeek(Style,'Style')
      *=gfSeek('S'+Style.Scale,'Scale')
      *WAIT WINDOW 'Style : ' + Style NOWAIT
      *IF !(&lcForExp) OR !lfChkMaitSR(SUBSTR(Style,1,loFormSet.lnMajLen))
      IF !(&lcForExp)
      *WSH [End]
      
        LOOP
      ENDIF
      
      STORE .F. TO llMark
      
      *-- Get data from Master InvSerNo table
      =lfFromMastInv(loFormSet)
      
    ENDSCAN
  ENDIF
  
  *WSH Get Return Authorization Lines... [Start]
  IF lcSelTran $ 'AT'
    llIgnore = .T.
    
    SELECT INVSERNO
    =gfSetOrder("TRNCODE")
    
    SELECT RALINE
    IF gfSeek(lcStyle)
      lcOldKey = SPACE(29)
      lcOldRa  = SPACE(6)
      
      SCAN REST WHILE STYLE+RANO+CRA_LINNO = lcStyle
        IF lcOldRa <> RALINE.RaNo
          =gfSeek(RANO, "RETAUTH")
          lcOldRa = RALINE.RaNo
        ENDIF
        
        IF !(&lcForExp)
          LOOP
        ENDIF
        
        IF lcOldKey <> RALINE.RaNo+RALINE.Style+RALINE.cRa_LinNo
          *-- To add a record in the header (1st grid)
          =lfAddHdrRec(loFormSet, '0')
          
          llMark   = .F.
          lcOldKey = RALINE.RaNo+RALINE.Style+RALINE.cRa_LinNo
        ENDIF
        
        *-- Get RA Line details...
        =lfGetRaSty(loFormSet)
      ENDSCAN
    ENDIF
    llIgnore = .F.
  ENDIF
  *WSH [End]
  
ENDSCAN

SELECT(loFormSet.lcTmpSyInv)
LOCATE
SELECT(loFormSet.lcTmpDet)

*WSH [Start]
SET KEY TO EVALUATE(loFormSet.lcTmpSyInv+'.cSession')+EVALUATE(loFormSet.lcTmpSyInv+'.Style')+;
           STR(EVALUATE(loFormSet.lcTmpSyInv+'.LineNo'),6)+EVALUATE(loFormSet.lcTmpSyInv+'.cTrCode')+EVALUATE(loFormSet.lcTmpSyInv+'.cIrType')
*WSH [End]

LOCATE

*-- To build the header and detail grids
=lfBuildGrd(loFormSet)

*-- To enable and disbale controls
=lfAdjControl(loFormSet,'')

WAIT CLEAR
*-- end of lfSelBySty

*!*************************************************************
*! Name      : lfGetRaSty
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : Get an Ra Lines for a Style from InvSerNo, or Raline
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfGetRaSty
LPARAMETERS loFormSet

*-- If RA Line found in the INVSERNO file..
SELECT INVSERNO
IF gfSeek(RALINE.RaNo+'R'+RALINE.Style)
  SCAN REST WHILE cTrCode+CIRTYPE+STYLE+SZ1+CWARECODE+CSER_NUM+STR(LINENO,6) = ;
                  RALINE.RaNo+'R'+RALINE.Style ;
            FOR nLineNo = INT(VAL(RALINE.cRa_LinNo))
    
    FOR lnCnt = 1 TO Scale.Cnt
      lcCnt = STR(lnCnt,1)
      IF Sz1 == Scale.Sz&lcCnt
        EXIT
      ENDIF
    ENDFOR
    
    IF !llMark
      IF !EMPTY(cSer_Num)
        REPLACE lMark WITH .T., lPrint WITH .T. IN SELECT(loFormSet.lcTmpSyInv)
        llMark = .T.
      ENDIF  
    ENDIF
    
    *-- To add a record in the detail for each serial number (2nd grid)       
    =lfAddDetRec(loFormSet, InvSerNo.Style, InvSerNo.Sz1, InvSerNo.cSer_Num,;
                 '', lcCnt, InvSerNo.cSession, InvSerNo.LineNo, 'M',;
                 InvSerNo.cIRType, InvSerNo.cWareCode, InvSerNo.nLineNo, InvSerNo.cTrCode, InvSerNo.cTrType)
  ENDSCAN

*-- RA Line not found in the INVSERNO file..
ELSE
  FOR lnCnt = 1 TO Scale.Cnt
    lcCnt    = ALLTRIM(STR(lnCnt))
    lnLineNo = 0
    
    FOR lnJ = 1 TO RALINE.Qty&lcCnt
      lnLineNo = lnLineNo + 1
      
      *-- To add a record in the detail for each serial number (2nd grid)       
      =lfAddDetRec(loFormSet, RALINE.Style, Scale.Sz&lcCnt, '',;
                   '', lcCnt, SPACE(6), lnLineNo, 'N',;
                   'R', RETAUTH.cWareCode, INT(VAL(RALINE.cRa_LinNo)), RALINE.RaNo)
    ENDFOR
  ENDFOR
ENDIF

*!*************************************************************
*! Name      : lfSelByTrn
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : Case select transaction from option grid
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!             lcForExp  : For expression
*!*************************************************************
FUNCTION lfSelByTrn
LPARAMETERS loFormSet, lcForExp

LOCAL lnCnt

*--ctrcode+coprcode+clotno+ctrtype+style+cwarecode
SELECT StyInvJl
gfSETORDER("MFGOPR")

SELECT (lcTranFile)
SCAN
  lcTranNo = KeyExp
  IF gfSeek(lcTranNo,'StyInvJl')
    SELECT StyInvJl
    SCAN REST WHILE cTrCode+cOprCode+cLotNo+cTrType+Style+cWareCode = lcTranNo
      =gfSeek(Style,'Style')
      =gfSeek('S'+Style.Scale,'Scale')
      
      WAIT WINDOW 'Style : ' + Style NOWAIT
      IF !(&lcForExp) OR !lfChkMaitSR(SUBSTR(Style,1,loFormSet.lnMajLen))
        LOOP
      ENDIF
      STORE .F. TO llMark
      
      *-- Get data from Master InvSerNo table
      =lfFromMastInv(loFormSet)
    ENDSCAN
  ENDIF
ENDSCAN

SELECT(loFormSet.lcTmpSyInv)
LOCATE
SELECT(loFormSet.lcTmpDet)

*WSH [Start]
SET KEY TO EVALUATE(loFormSet.lcTmpSyInv+'.cSession')+EVALUATE(loFormSet.lcTmpSyInv+'.Style')+;
           STR(EVALUATE(loFormSet.lcTmpSyInv+'.LineNo'),6)+EVALUATE(loFormSet.lcTmpSyInv+'.cTrCode')+EVALUATE(loFormSet.lcTmpSyInv+'.cIrType')
*WSH [End]

LOCATE 

*-- To build the header and detail grids
=lfBuildGrd(loFormSet)

*-- To enable and disbale controls
=lfAdjControl(loFormSet,'')

WAIT CLEAR
*-- end of lfSelByTrn

*!*************************************************************
*! Name      : lfSelByRA
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : Case select by RA
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!             lcForExp  : For expression
*!*************************************************************
FUNCTION lfSelByRA
LPARAMETERS loFormSet, lcForExp

LOCAL lnCnt, lcOldIndex
lcOldIndex = ORDER('InvSerNo')

*WSH [Start]
IF !USED('RALINE')
  =gfOpenTable(oAriaApplication.DataDir+'RALINE',oAriaApplication.DataDir+'RALINE','SH')
ENDIF
IF !USED('RETAUTH')
  =gfOpenTable(oAriaApplication.DataDir+'RETAUTH',oAriaApplication.DataDir+'RETAUTH','SH')
ENDIF

SELECT RALINE
=gfSetOrder("RALINE")

SELECT RETAUTH
=gfSetOrder("RETAUTH")

llIgnore = .T.
*WSH [End]

SELECT InvSerNo
=gfSetOrder('TRNCODE')

SELECT (lcTranFile)
SCAN
  lcTranNo = KeyExp
  
  *WSH [Start]
  *IF !gfSeek(lcTranNo, 'InvSerNo')
  *  =lfAddRA(loFormSet, lcTranNo, .F.)
  *ELSE
  *  STORE .F. TO llMark
  *  
  *  SELECT InvSerNo
  *  =gfSeek(lcTranNo + 'R')
  *  SCAN REST WHILE cPack_Ra+CIRTYPE+STYLE+SZ1+CWARECODE+CSER_NUM+STR(LINENO,6) = ;
  *                  lcTranNo + 'R'
  IF gfSeek(lcTranNo, "RETAUTH")
    SELECT RALINE
    =gfSeek(RETAUTH.RaNo)
    SCAN REST WHILE RANO+STYLE+CRA_LINNO = RETAUTH.RaNo
  *WSH [End]
  
      =gfSeek(Style,'Style')
      =gfSeek('S'+Style.Scale,'Scale')
      
      *WSH [Start]
      *IF !(&lcForExp)
      IF !(&lcForExp) OR !lfChkMaitSR(SUBSTR(Style,1,loFormSet.lnMajLen))
      *WSH [End]
      
        LOOP
      ENDIF
      
      WAIT WINDOW 'Style: ' + Style NOWAIT
      
      *WSH [Start]
      *FOR lnCnt = 1 TO Scale.Cnt
      *  lcCnt = STR(lnCnt,1)
      *  IF Sz1 == Scale.Sz&lcCnt
      *    EXIT
      *  ENDIF
      *ENDFOR 
      *
      *IF !llMark
      *  IF !EMPTY(cSer_Num)
      *    REPLACE lMark WITH .T., lPrint WITH .T. IN SELECT(loFormSet.lcTmpSyInv)
      *    llMark = .T.
      *  ENDIF  
      *ENDIF
      *
      **-- To add a record in the detail for each serial number (2nd grid)       
      *=lfAddDetRec(loFormSet, InvSerNo.Style, InvSerNo.Sz1, InvSerNo.cSer_Num,;
      *             '', lcCnt, InvSerNo.cSession, InvSerNo.LineNo, 'M',;
      *             InvSerNo.cIRType, InvSerNo.cWareCode, InvSerNo.nLineNo, InvSerNo.cPack_Ra)
      
      *-- To add a record in the header (1st grid)
      =lfAddHdrRec(loFormSet, '0')
      
      llMark   = .F.
      =lfGetRaSty(loFormSet)
      *WSH [End]
      
    ENDSCAN
  ENDIF
ENDSCAN

*WSH [Start]
llIgnore = .F.
*WSH [End]

SELECT InvSerNo
=gfSetOrder(lcOldIndex)

SELECT(loFormSet.lcTmpSyInv)
LOCATE
SELECT(loFormSet.lcTmpDet)

*WSH [Start]
SET KEY TO EVALUATE(loFormSet.lcTmpSyInv+'.cSession')+EVALUATE(loFormSet.lcTmpSyInv+'.Style')+;
           STR(EVALUATE(loFormSet.lcTmpSyInv+'.LineNo'),6)+EVALUATE(loFormSet.lcTmpSyInv+'.cTrCode')+EVALUATE(loFormSet.lcTmpSyInv+'.cIrType')
*WSH [End]

LOCATE 

*-- To build the header and detail grids
=lfBuildGrd(loFormSet)

*-- To enable and disbale controls
=lfAdjControl(loFormSet,'')

WAIT CLEAR
*-- end of lfSelByRA

*!*************************************************************
*! Name      : lfSelByAll
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : Case select all transactions
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!             lcForExp  : For expression
*!*************************************************************
FUNCTION lfSelByAll
LPARAMETERS loFormSet, lcForExp

LOCAL lnCnt

*WSH [Start]
IF lcSelTran $ 'AT'
  IF !USED('RALINE')
    =gfOpenTable(oAriaApplication.DataDir+'RALINE',oAriaApplication.DataDir+'RALINES','SH')
  ENDIF
  IF !USED('RETAUTH')
    =gfOpenTable(oAriaApplication.DataDir+'RETAUTH',oAriaApplication.DataDir+'RETAUTH','SH')
  ENDIF

  SELECT RALINE
  =gfSetOrder("RALINES")

  SELECT RETAUTH
  =gfSetOrder("RETAUTH")
ENDIF
*WSH [End]

SELECT StyInvJl
gfSETORDER("STYINVJL")

SELECT ICStySer
gfSeek('')

SCAN FOR lSrMaintan
  lcStyle = SUBSTR(cStyMajor,1,loFormSet.lnMajLen)
  IF gfSeek(lcStyle, 'StyInvJl')
    SELECT StyInvJl
    SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrDate)+cTrCode+STR(LineNo,6) = lcStyle              
      =gfSeek(Style,'Style')
      =gfSeek('S'+Style.Scale,'Scale')
      
      WAIT WINDOW 'Style : ' + Style NOWAIT
      
      IF !(&lcForExp)
        LOOP
      ENDIF
      
      STORE .F. TO llMark
      
      *-- Get data from Master InvSerNo table
      =lfFromMastInv(loFormSet)
      
    ENDSCAN
  ENDIF
ENDSCAN

SELECT(loFormSet.lcTmpSyInv)
LOCATE
SELECT(loFormSet.lcTmpDet)

*WSH [Start]
SET KEY TO EVALUATE(loFormSet.lcTmpSyInv+'.cSession')+EVALUATE(loFormSet.lcTmpSyInv+'.Style')+;
           STR(EVALUATE(loFormSet.lcTmpSyInv+'.LineNo'),6)+EVALUATE(loFormSet.lcTmpSyInv+'.cTrCode')+EVALUATE(loFormSet.lcTmpSyInv+'.cIrType')
*WSH [End]

LOCATE

*-- To build the header and detail grids
=lfBuildGrd(loFormSet)

*-- To enable and disbale controls
=lfAdjControl(loFormSet,'')

WAIT CLEAR
*-- end of lfSelByAll

*!*************************************************************
*! Name      : lfFromMastInv
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To get the data from master InvSerNo
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfFromMastInv
LPARAMETERS loFormSet

*WSH [Start]
LOCAL llFound
llFound = .F.

*-- To add a record in the header (1st grid)
=lfAddHdrRec(loFormSet,StyInvJl.cTrType)
*WSH [End]

IF gfSeek(StyInvJl.cSession+StyInvJl.Style,'InvSerNo')
  
  *WSH [Start]
  **-- To add a record in the header (1st grid)
  *=lfAddHdrRec(loFormSet,StyInvJl.cTrType)
  *WSH [End]
  
  SELECT InvSerNo
  
  *WSH [Start]
  *SCAN REST WHILE CSESSION+STYLE+SZ1+CIRTYPE+STR(LINENO,6)+CSER_NUM = ;
  *                StyInvJl.cSession+StyInvJl.Style FOR StyInvJl.LineNo = nLineNo
  SCAN REST WHILE CSESSION+STYLE+SZ1+CIRTYPE+STR(LINENO,6)+CSER_NUM = ;
                  StyInvJl.cSession+StyInvJl.Style FOR cIrType = StyInvJl.cIrType AND nLineNo = StyInvJl.LineNo
    
    llFound = .T.
  *WSH [End]
    
    WAIT WINDOW 'Style/Size/Serial Number : ' + Style+"/"+Sz1+"/"+cSer_Num NOWAIT
    
    FOR lnCnt = 1 TO Scale.Cnt
      lcCnt = STR(lnCnt,1)
      IF Sz1 == Scale.Sz&lcCnt
        EXIT
      ENDIF
    ENDFOR 
    
    IF !llMark
      IF !EMPTY(cSer_Num)
        REPLACE lMark WITH .T., lPrint WITH .T. IN SELECT(loFormSet.lcTmpSyInv)
        llMark = .T.
      ENDIF  
    ENDIF
    
    *-- To add a record in the detail for each serial number (2nd grid)
    
    *WSH [Start]             
    *=lfAddDetRec(loFormSet, InvSerNo.Style, InvSerNo.Sz1, InvSerNo.cSer_Num,;
                 StyInvJl.Reference, lcCnt, InvSerNo.cSession, InvSerNo.LineNo, 'M',;
                 InvSerNo.cIRType, InvSerNo.cWareCode, InvSerNo.nLineNo)
    =lfAddDetRec(loFormSet, InvSerNo.Style, InvSerNo.Sz1, InvSerNo.cSer_Num,;
                 StyInvJl.Reference, lcCnt, InvSerNo.cSession, InvSerNo.LineNo, 'M',;
                 InvSerNo.cIRType, InvSerNo.cWareCode, InvSerNo.nLineNo, StyInvJl.cTrCode, StyInvJl.cTrType)
    *WSH [End]
    
  ENDSCAN
  
*WSH [Start]
*ELSE
*  *-- To add a record in the header (1st grid)
*  =lfAddHdrRec(loFormSet,StyInvJl.cTrType)
ENDIF

SELECT STYINVJL
IF !llFound
*WSH [End]
  
  lnCnt   = 0
  FOR lnCnt = 1 TO Scale.Cnt
    lcCnt = STR(lnCnt,1)
    lnLineNo = 0
    IF nStk&lcCnt <> 0
      *-- To add records to the master serialized inventory file for each piece                   
      FOR lnCnt1  = 1 TO ABS(nStk&lcCnt)
        lnLineNo = lnLineNo + 1
        lcSzCode   = Scale.Sz&lcCnt
        lcSerNum = ''
        WAIT WINDOW "Adding Style/Size: " + Style+"/"+lcSzCode NOWAIT
        
        *-- To add a record in the detail for each serial number (2nd grid)
        
        *WSH [Start]
        *=lfAddDetRec(loFormSet, StyInvJl.Style, lcSzCode, lcSerNum, StyInvJl.Reference,;
                     lcCnt, StyInvJl.cSession, lnLineNo, 'N', StyInvJl.cIRType,;
                     StyInvJl.cWareCode, StyInvJl.LineNo)
        =lfAddDetRec(loFormSet, StyInvJl.Style, lcSzCode, lcSerNum, StyInvJl.Reference,;
                     lcCnt, StyInvJl.cSession, lnLineNo, 'N', StyInvJl.cIRType,;
                     StyInvJl.cWareCode, StyInvJl.LineNo, StyInvJl.cTrCode, StyInvJl.cTrType)
        *WSH [End]
        
      ENDFOR
    ENDIF
    SELECT StyInvJl
  ENDFOR
ENDIF
WAIT CLEAR
*-- end of lfFromMastInv

*!*************************************************************
*! Name      : lfGetFilter
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To get the selected options in the OG.
*!*************************************************************
*! Parameters: lcFilter : Option grid element
*!*************************************************************
FUNCTION lfGetFilter
LPARAMETERS lcFilter

lnPOS = ASCAN(laScopExpr,lcFilter) 
IF lnPos > 0
  lnPOS    = ASUBSCRIPT(laScopExpr,lnPos,1)
  lcReturn = laScopExpr[lnPOS,6]    
ELSE
  lcReturn = ""     
ENDIF
RETURN lcReturn

*!*************************************************************
*! Name      : lfvldType
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To clear the options
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfVldType
CLEARread()
lcSelTran = lcRpTrans 
*-- end of lfVldType

*!*************************************************************
*! Name      : lfCreatExp
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : function to copy the fixed filter from option grid
*!*************************************************************
FUNCTION lfCreatExp

*--Copy the fixed filter array to a public array in order to be
*--available after closing the optiion grid
=ACOPY(loOGScroll.laOGFxFlt , laScopExpr)
*-- end of lfCreatExp

*!*************************************************************
*! Name      : lfSRShow
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To be called in the change 
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!         lcSessNo  : Receive or Issue Session No
*!         lcIRTrn   : 'R' Received, 'I' Issue 
*!         lcTrnTyp  : '6' Receive PO
*!         lcTrnTyp  : '7' Credit Memo
*!*************************************************************
FUNCTION lfSRShow

*WSH [Start]
*PARAMETERS loFormSet,lcSessNo, lcIRTrn, lcTrnTyp
PARAMETERS loFormSet, lcTrnTyp
*WSH [End]

*-- To get the major/nonmajor segments
=lfGetItemMask(loFormSet)

*WSH [Start]
LOCAL lnI
FOR lnI = 1 TO ALEN(loFormSet.laSessNos,1)
  lcIRTrn   = loFormSet.laSessNos[lnI,1]
  lcSessNo  = loFormSet.laSessNos[lnI,2]
  lcTrnCode = loFormSet.laSessNos[lnI,3]
*WSH [End]
  
  DO CASE
    
    *-- Special handling for the the Return Authorization
    CASE lcTrnTyp = '0'
      =lfAddRA(loFormSet, lcTrnCode, .F.)
    
    *WSH [Start]  
    **-- Case of PO Receiving
    *CASE lcTrnTyp $ '6'
    *  =lfAddInvSR(loFormSet, lcSessNo, lcIRTrn, lcTrnTyp, .T.)
    *
    *-- Case of Invoice Sales Order
    *CASE lcTrnTyp $ '3'
    *  =lfAddInvSR(loFormSet, lcSessNo, 'I', lcTrnTyp, .F.)
    *
    *-- Case of PO Receiving, Inventory Adjustment and Credit Memo
    CASE lcTrnTyp $ '617'
      =lfAddInvSR(loFormSet, lcSessNo, lcIRTrn, lcTrnTyp, .F.)
    
    *-- Case of Physical Inventory
    CASE lcTrnTyp $ '2'
      =lfAddInvSR(loFormSet, lcSessNo, 'I', lcTrnTyp, .T., 'S')
      =lfAddInvSR(loFormSet, lcSessNo, 'R', lcTrnTyp, .F.)
      
    *-- Case of Invoice Sales Order
    CASE lcTrnTyp $ '3'
      =lfAddInvSR(loFormSet, lcSessNo, 'I', lcTrnTyp, .F.)
      
    *-- Void Invoice
    CASE lcTrnTyp $ '4'
      =lfAddInvSR(loFormSet, lcSessNo, 'R', lcTrnTyp, .T., 'T', '3')
      
    *-- Void Credit Memo
    CASE lcTrnTyp $ '8'
      =lfAddInvSR(loFormSet, lcSessNo, 'I', lcTrnTyp, .T., 'T', '7')
    *WSH [End]
    
  ENDCASE
  
*WSH [Start]
ENDFOR

SELECT (loFormSet.lcTmpSyInv)
LOCATE
SELECT (loFormSet.lcTmpDet)
LOCATE

IF loFormSet.llExternal AND !(lcTrnTyp $ '48') AND !EOF(loFormSet.lcTmpSyInv)
  loFormSet.ChangeMode("A")
ENDIF
*WSH [End]

*-- To build the header and detail grids
=lfBuildGrd(loFormSet)

*-- To enable and disbale controls
=lfAdjControl(loFormSet,lcTrnTyp)
*-- end of lfSRShow

*!*************************************************************
*! Name      : lfAutoGen
*! Developer : Wael M. ABo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : Automatic Generate Serial Numbers
*!*************************************************************
*! Example   : =lfAutoGen()
*!*************************************************************
FUNCTION lfAutoGen
LPARAMETERS loFormSet

loFormSet.AriaForm1.LockScreen = .T.

LOCAL lnAlias, lcHdrRec, lcDetOrd, lcDetKey, lcDetOrd, lcSerNum, llFoundH, llFoundD, lcSeekKey
lnAlias  = SELECT(0)
lcTmpOrd = ORDER(loFormSet.lcTmpDet)
lcHdrRec = IIF(EOF(loFormSet.lcTmpSyInv), 0, RECNO(loFormSet.lcTmpSyInv))
lcTmpRec = IIF(EOF(loFormSet.lcTmpDet), 0, RECNO(loFormSet.lcTmpDet))
llFoundH = .F.
llFoundD = .F.

SELECT (loFormSet.lcTmpDet)
lcTmpKey = SET("Key", 1)

SET KEY TO
SET ORDER TO (loFormSet.lcTmpDet)

SELECT (loFormSet.lcTmpSyInv)
LOCATE
SCAN REST FOR cIRType = 'R' AND lMark
  llFoundH  = .T.
  lcSeekKey = EVALUATE(loFormSet.lcTmpSyInv + '.cSession') +;
              EVALUATE(loFormSet.lcTmpSyInv + '.Style')    +;
              STR(EVALUATE(loFormSet.lcTmpSyInv + '.LineNo'), 6)+;
              EVALUATE(loFormSet.lcTmpSyInv+'.cTrCode')+;
              EVALUATE(loFormSet.lcTmpSyInv+'.cIrType')
  
  SELECT (loFormSet.lcTmpDet)
  =SEEK(lcSeekKey)
  SCAN REST WHILE cSession+cStyle+STR(nLineNo,6)+cTrCode+cIrType+cSizeNo+STR(LineNo,6) = lcSeekKey ;
            FOR   EMPTY(cSerialNo)
    REPLACE cSerialNo WITH gfSequence('CSER_NUM','','','') ,;
            lPrint    WITH .T.
    
    llFoundD = .T.
  ENDSCAN
  
  SELECT (loFormSet.lcTmpSyInv)
  IF llFoundD
    REPLACE lPrint WITH .T.
  ENDIF
ENDSCAN

IF !llFoundH
  *-- 'Please mark the received Styles you want to serialize.'
  =gfModalGen('INM42275B42001','DIALOG')
ELSE
  IF !llFoundD
    *-- 'All selected Style pieces are serialized. No Serial Numbers generated.'
    =gfModalGen('INM42276B42001','DIALOG')
  ENDIF
ENDIF

SELECT (loFormSet.lcTmpSyInv)
IF !EMPTY(lcHdrRec)
  GOTO (lcHdrRec)
ENDIF

SELECT (loFormSet.lcTmpDet)
SET ORDER TO (lcTmpOrd)
SET KEY TO (lcTmpKey)
IF !EMPTY(lcTmpRec)
  GOTO (lcTmpRec)
ENDIF

loFormSet.AriaForm1.LockScreen = .F.

SELECT (lnAlias)
RETURN
*-- end of lfAutoGen

*!*************************************************************
*! Name      : lfAddInvSR
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To add records in the serial inventory file
*!*************************************************************
*! Parameters: loFormSet   : Reference to the formset
*!         lcSessNo    : Receive or Issue Session No
*!         lcIRTrn     : 'R' Received, 'I' Issue 
*!         lcTrnTyp    : '6' Receive PO
*!         llGetSrNo   : .T. Get Serial Numbers to be used    && WSH
*!         lcGetBy     : 'S' Get by Style, 'T' By Transaction && WSH
*!         lcGetTrType : Transaction Type of the Transaction to get Serials from
*!*************************************************************
FUNCTION lfAddInvSR

*WSH [Start
*PARAMETERS loFormSet, lcSessNo, lcIRTrn, lcTrnTyp, llGenSrNo
PARAMETERS loFormSet, lcSessNo, lcIRTrn, lcTrnTyp, llGetSrNo, lcGetBy, lcGetTrType
*WSH [End]

*WSH [Start]
LOCAL llGetPack
llGetPack = .F.

IF lcTrnTyp = '3' AND 'AL' $ oAriaApplication.CompanySetupModules
  =gfOpenTable(oAriaApplication.DataDir+'ORDLINE',oAriaApplication.DataDir+'ORDLINE','SH')
  =gfOpenTable(oAriaApplication.DataDir+'INVLINE',oAriaApplication.DataDir+'INVLINE','SH')
  =gfOpenTable(oAriaApplication.DataDir+'PACK_HDR',oAriaApplication.DataDir+'ORDERPCK','SH')
  =gfOpenTable(oAriaApplication.DataDir+'ICSRPACK',oAriaApplication.DataDir+'ICSRPACK','SH')
ENDIF
*WSH [End]

PRIVATE lnAlias
LOCAL lnCnt, lcCnt, lnCnt1, llFound

*WSH [Start]
*=gfOpenTable(oAriaApplication.DataDir+'INVSERNO',oAriaApplication.DataDir+'SESSION','SH')
SELECT INVSERNO
lcOrder = ORDER()

IF llGetSrNo
  =gfSetOrder(IIF(lcGetBy = "S", "STYLE", "TRNCODE"))
ELSE
  =gfSetOrder("SESSION")
ENDIF

PRIVATE lcSRCursor
lcSRCursor = gfTempName()
*WSH [End]

SELECT StyInvJl
=gfSETORDER("SESSION")

*WSH [Start]
*IF gfSeek(lcSessNo)
IF gfSeek(lcSessNo+lcIRTrn+lcTrnTyp)
*WSH [End]

   SCAN REST WHILE CSESSION+CIRTYPE+CTRTYPE+CTRCODE = lcSessNo+lcIRTrn+lcTrnTyp
     *-- Check if the style is marked to maintain serial numbers
     IF !lfChkMaitSR(SUBSTR(Style,1,loFormSet.lnMajLen))
       LOOP
     ENDIF
     
     =gfSeek(Style,'Style')
     =gfSeek('S'+Style.Scale,'Scale')
     
     *WSH [Start]
     *IF llGenSrNo
     *WSH [End]
     
     *-- To add a record in the header (1st grid)
     =lfAddHdrRec(loFormSet, lcTrnTyp)
     
     *WSH [Start]
     *ENDIF
     
     llGetPack = .F.
     IF lcTrnTyp = '3' AND 'AL' $ oAriaApplication.CompanySetupModules
       =gfSEEK(STYINVJL.cTrCode, "INVLINE")
       IF !EMPTY(INVLINE.Order) AND gfSEEK("O" + INVLINE.Order, "ORDLINE")
         IF gfSEEK(ORDLINE.Order+ORDLINE.Store, 'PACK_HDR')
           CREATE CURSOR (lcSRCursor) (Size C(5), cSerialNo C(8))
           INDEX ON Size TAG (lcSRCursor)
           SET ORDER TO (lcSRCursor)
           
           SELECT PACK_HDR
           SCAN REST WHILE ORDER+STORE+PACK_NO = ORDLINE.Order+ORDLINE.Store
             IF gfSEEK(PACK_HDR.Pack_No, 'ICSRPACK')
               SELECT ICSRPACK
               SCAN REST WHILE PACK_NO+STR(CART_NO,4)+STYLE+SZ1+SERIAL_UPC+STR(LINENO,6) = PACK_HDR.Pack_No;
                         FOR   Style = StyInvJL.Style AND !EMPTY(Serial_UPC)
                 
                 =gfReplace("Invoice WITH STYINVJL.cTrCode")
                 
                 SELECT (lcSRCursor)
                 APPEND BLANK
                 REPLACE Size      WITH ICSRPACK.SZ1,;
                         cSerialNo WITH ICSRPACK.Serial_UPC
                 llGetPack = .T.
               ENDSCAN
             ENDIF
           ENDSCAN
           SELECT ICSRPACK
           =gfTableUpdate()
         ENDIF
       ENDIF
     ENDIF
     *WSH [End]
     
     lnCnt = 0
     FOR lnCnt = 1 TO Scale.Cnt
        lcCnt = STR(lnCnt,1)
        lnLineNo = 0
        
        *WSH [Start]
        lcSzCode = Scale.Sz&lcCnt
        
        IF llGetPack
          SELECT (lcSRCursor)
          SET KEY TO lcSzCode
          LOCATE
        ENDIF
        SELECT StyInvJl
        
        WAIT WINDOW "Adding Style/Size: " + Style+"/"+lcSzCode NOWAIT
        
        IF llGetSrNo
          =lfGetSerials(loFormSet, lcSRCursor, lcGetBy,;
                        IIF(lcGetBy = 'S',;
                            STYINVJL.Style+lcSzCode,;
                            STYINVJL.cTrCode+lcGetTrType+IIF(lcIRTrn = "I", "R", "I")+STYINVJL.Style+lcSzCode))
        ENDIF
        
        SELECT StyInvJl
        *WSH [End]
        
        IF nStk&lcCnt <> 0
          *-- To add records to the master serialized inventory file for each piece                   
          FOR lnCnt1  = 1 TO ABS(nStk&lcCnt)
             lnLineNo = lnLineNo + 1
             
             *WSH [Start]
             *lcSzCode = Scale.Sz&lcCnt
             *lcSerNum = IIF(llGenSrNo, gfSequence('CSER_NUM','','',''), '')
             *IF llGenSrNo
             IF llGetSrNo OR llGetPack
               SELECT (lcSRCursor)
               lcSerNum = IIF(EOF(), '', cSerialNo)
               IF !EOF()
                 SKIP
               ENDIF
             ELSE
               lcSerNum = ''
             ENDIF
             
             SELECT StyInvJl
             IF llGetSrNo OR llGetPack
             *WSH [End]
               
               WAIT WINDOW "Serial Number: " + lcSerNum NOWAIT
               
             *WSH [Start]
             *ELSE
             *  WAIT WINDOW "Adding Style/Size: " + Style+"/"+lcSzCode NOWAIT
             *ENDIF
             *WSH [End]
               
               *-- To add a record to the master serila No. file.
               =lfAddToMast(loFormSet,StyInvJl.cSession,lcSerNum,StyInvJl.cIRType,StyInvJl.Style,;
                            lcSzCode,StyInvJl.cWareCode, lnLineNo, StyInvJl.LineNo, StyInvJl.cTrCode, StyInvJl.cTrType)
               
             *WSH [Start]
             *IF llGenSrNo
             *  *-- To add a record in the detail for each serial number (2nd grid)
             *  =lfAddDetRec(loFormSet, StyInvJl.Style, lcSzCode, lcSerNum, StyInvJl.Reference,;
             *             lcCnt, StyInvJl.cSession, lnLineNo,'N',StyInvJl.cIRType,;
             *             StyInvJl.cWareCode, StyInvJl.LineNo)
             *ENDIF
             ENDIF
             
             =lfAddDetRec(loFormSet, StyInvJl.Style, lcSzCode, lcSerNum, StyInvJl.Reference,;
                          lcCnt, StyInvJl.cSession, lnLineNo,IIF(llGetSrNo OR llGetPack, 'M', 'N'),StyInvJl.cIRType,;
                          StyInvJl.cWareCode, StyInvJl.LineNo, StyInvJl.cTrCode, StyInvJl.cTrType)
             *WSH [End]
             
          ENDFOR
        ENDIF
        SELECT StyInvJl
     ENDFOR
   ENDSCAN
   
   *WSH [Start]
   *SELECT InvSerNo
   *LOCATE
   *IF !EOF()
   *  =gfTableUpdate()
   *  IF llGenSrNo
   *    loFormSet.ariaform1.cmdPrintLbl.Enabled = .T.
   *  ENDIF
   *ENDIF
   IF llGetSrNo OR llGetPack
     SELECT InvSerNo
     =gfTableUpdate()
   ENDIF
   *WSH [End]
   
ENDIF

*WSH [Start]
SELECT InvSerNo
=gfSetOrder(lcOrder)
*WSH [End]

SELECT(loFormSet.lcTmpSyInv)
LOCATE
SELECT(loFormSet.lcTmpDet)
LOCATE 

WAIT CLEAR
*-- end of lfAddInvSR

*!*************************************************************
*! Name      : lfGetSerials
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : Function to get Serial numbers received
*!*************************************************************
*! Example   : =lfGetSerials()
*!*************************************************************
FUNCTION lfGetSerials
LPARAMETERS loFormSet, lcSerialCurs, lcGetBy, lcSeekKey

LOCAL lcIrType, lnAlias, lcSerNum, lcSeekKey, lnTotQty
lnAlias  = SELECT(0)
lcIrType = EVALUATE(loFormSet.lcTmpSyInv + '.cIRType')

CREATE CURSOR (lcSerialCurs) (cSerialNo C(8))

SELECT INVSERNO
IF gfSeek(lcSeekKey)
  *-- Case we need available Serial Numbers for a Style...
  IF lcGetBy = 'S'
    lcStyle = Style
    
    DO WHILE !EOF() AND Style = lcStyle
      lcSerNum  = cSer_Num
      lcSeekKey = Style+Sz1+cWareCode+cSer_Num
      lnTotQty  = 0
      
      SCAN REST WHILE Style+Sz1+cWareCode+cSer_Num+cIRType+STR(LINENO,6) = lcSeekKey FOR IIF(lcIrType = 'I', !EMPTY(cSession), .T.)
        lnTotQty = lnTotQty + IIF(cIRType = 'I', -TotQty, TotQty)
      ENDSCAN
      
      IF !EMPTY(lcSerNum) AND lnTotQty > 0
        SELECT (lcSerialCurs)
        APPEND BLANK
        REPLACE cSerialNo WITH lcSerNum
      ENDIF
      SELECT INVSERNO
    ENDDO
  
  *-- Case we need Serial Numbers from a Specific Transaction #...
  ELSE
    SCAN REST WHILE CTRCODE+CTRTYPE+CIRTYPE+STYLE+SZ1+CWARECODE+CSER_NUM+STR(LINENO,6) = lcSeekKey
      IF !EMPTY(CSER_NUM)
        SELECT (lcSerialCurs)
        APPEND BLANK
        REPLACE cSerialNo WITH INVSERNO.CSER_NUM
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

SELECT (lcSerialCurs)
LOCATE

SELECT (lnAlias)
RETURN
*-- end of lfGetSerials

*!*************************************************************
*! Name      : lfChkMaitSR
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To check if the style is marked to maintain serial #
*!*************************************************************
*! Parameters: lcStyMaj  : Style Major
*!*************************************************************
FUNCTION lfChkMaitSR
LPARAMETERS lcStyMaj
RETURN gfSeek(lcStyMaj, 'ICSTYSER') AND ICSTYSER.lSrMaintan

*!*************************************************************
*! Name      : lfAddHdrRec
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To add a record from StyinvJl
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!         lcTrnTyp  : '1' Inventory Adjustment
*!         lcTrnTyp  : '2' Physical Inventory
*!         lcTrnTyp  : '3' Sales Order Invoice
*!         lcTrnTyp  : '6' Receive PO
*!         lcTrnTyp  : '7' Credit Memo
*!*************************************************************
FUNCTION lfAddHdrRec
LPARAMETERS loFormSet, lcTrnTyp

PRIVATE lnAlias
LOCAL lnCnt
lnAlias = SELECT()

SELECT StyInvJl
SCATTER MEMVAR
m.cScale = Scale.Scale

*WSH [Start]
*IF loFormSet.llGenSrNow
*  STORE .T. TO m.lMark, m.lPrint
*ENDIF
IF loFormSet.llExternal
  STORE .T. TO m.lMark
ENDIF
*WSH [End]

DO CASE
  *-- Inventory Adjustment
  CASE lcTrnTyp = '1'
    m.cTranDesc = 'Inventory Adjustment/Transfer'
    
  *-- Physical Inventory
  CASE lcTrnTyp = '2'
    m.cTranDesc = 'Physical Inventory'
    
  *-- Sales Order Invoice
  CASE lcTrnTyp = '3'
  
  *WSH [Start]
    *m.cTranDesc = 'Sales Order Invoice'
    m.cTranDesc = 'Invoice'
    
  *-- Void Invoice
  CASE lcTrnTyp = '4'
    m.cTranDesc = 'Void Invoice'
  *WSH [End]
    
*!*      FOR lnCnt = 1 TO Scale.Cnt
*!*        lcCnt = STR(lnCnt,1)
*!*        m.nStk&lcCnt = BS(m.nStk&lcCnt)
*!*      ENDFOR
    
  *-- Receive PO
  CASE lcTrnTyp = '6'
    m.cTranDesc = 'PO Receiving'
    
  *-- Credit Memo
  CASE lcTrnTyp = '7'
    
  *WSH [Start]
    *m.cTranDesc = 'Credit Memo'
    m.cTranDesc = 'Return Merchandise'
    
  *-- Void Credit Memo
  CASE lcTrnTyp = '8'
    m.cTranDesc = 'Void Credit Memo'
    
  *-- Return Authorization
  CASE lcTrnTyp = '0'
    m.cSession  = ''
    m.cTranDesc = 'Return Authorization'
    m.cIRType   = 'R'
    m.cWareCode = RETAUTH.cWareCode
    m.dTrDate   = RETAUTH.RaDate
    m.cTrCode   = RALINE.RaNo
    m.Reference = ''
    m.LineNo    = INT(VAL(RALINE.cRa_LinNo))
    m.Style     = RALINE.Style
    
    FOR lnI = 1 TO 8
      lcI = ALLTRIM(STR(lnI))
      m.nStk&lcI = RALINE.Qty&lcI
    ENDFOR
  *WSH [End]
  
ENDCASE

SELECT (loFormSet.lcTmpSyInv)
APPEND BLANK
GATHER MEMVAR

SELECT (lnAlias)
*-- end of lfAddHdrRec

*!*************************************************************
*! Name      : lfAddToMast
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To add a record to the master serila No. file.
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!             lcSessNo  : Session No.
*!             lcSerNo   : Serial Number
*!             lcIRType  : 'R' Receive, 'I' Issue
*!             lcStyle   : Style Code
*!             lcSize    : Size Code
*!             lcWareCode: WareHouse Code
*!             lnLineN   : Line No
*!*************************************************************
FUNCTION lfAddToMast
PARAMETERS loFormSet,lcSessNo,lcSerNo, lcIRType, lcStyle, lcSize, lcWareCode, lnLineN,;
            lnStyLinNo, lcTrCode, lcTrType
PRIVATE lnAlias
lnAlias = SELECT()
SELECT InvSerNo
APPEND BLANK 
*=gfAppend()
*=gfReplace([cSession  WITH lcSessNo,]+;
           [cSer_Num  WITH lcSerNum,]+;
           [cIRType   WITH lcIRType,]+;
           [Style     WITH lcStyle,]+;
           [Sz1       WITH lcSize,]+;
           [LineNo    WITH lnLineN,]+;
           [cWareCode WITH lcWareCode,]+;
           [TotQty    WITH 1])
REPLACE cSession  WITH lcSessNo,;
        cSer_Num  WITH lcSerNo ,;
        cIRType   WITH lcIRType,;
        Style     WITH lcStyle,;
        Sz1       WITH lcSize,;
        LineNo    WITH lnLineN,;
        cWareCode WITH lcWareCode,;
        TotQty    WITH 1,;
        nLineNo   WITH lnStyLinNo,;
        cTrCode   WITH IIF(TYPE('lcTrCode') = 'C', lcTrCode, ''),;
        cTrType   WITH IIF(TYPE('lcTrType') = 'C', lcTrType, '')
=gfAdd_Info('InvSerNo')
=gfReplace()
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfDetHdrRec
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To add a record for each piece
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!             lcStyle   : Style Code
*!             lcSize    : Size Code
*!             lcSerNo   : Serial Number
*!             lcRef     : Reference
*!             lcSizeNo  : Size Number
*!             lcSessNo  : Session No.
*!             lnLineN   : Line No
*!             lcRecStat : 'N' New, 'M' Modified
*!             lcIRType  : 'I' Issue, 'R' Receive
*!             lcWareCode: WareHouse Code
*!             lnStyLinNo: Style line number
*!             lcTrCode  : Transaction Number
*!*************************************************************
FUNCTION lfAddDetRec
LPARAMETERS loFormSet, lcStyle, lcSize, lcSerNo, lcRef, lcSizeNo,;
            lcSessNo , lnLineN, lcRecStat, lcIRType, lcWareCode,lnStyLinNo, lcTrCode, lcTrType
PRIVATE lnAlias
lnAlias = SELECT()
SELECT(loFormSet.lcTmpDet)
APPEND BLANK
REPLACE lPrint     WITH !EMPTY(lcSerNo),;
        cStyle     WITH lcStyle  ,;
        cSize      WITH lcSize   ,;
        nQty       WITH 1        ,;
        cSerialNo  WITH lcSerNo  ,;
        cReference WITH lcRef    ,;
        cSizeNo    WITH lcSizeNo ,;
        cSession   WITH lcSessNo ,;
        LineNo     WITH lnLineN   ,;
        cStatus    WITH IIF(EMPTY(cSerialNo),'E',''),;
        cRecStat   WITH IIF(EMPTY(cSerialNo),lcRecStat,'O'),;
        cIRType    WITH lcIRType ,;
        cWareCode  WITH lcWareCode,;
        nLineNo    WITH IIF(TYPE('lnStyLinNo') = 'N', lnStyLinNo, 0),;
        cTrCode    WITH IIF(TYPE('lcTrCode') = 'C', lcTrCode, ''),;
        cTrType    WITH IIF(TYPE('lcTrType') = 'C', lcTrType, '')

SELECT (lnAlias)
*-- end of lfAddDetRec

*!*************************************************************
*! Name      : lfAddRA
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To get the RA records
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!             lcRANumber: RA Number
*!*************************************************************
FUNCTION lfAddRA
LPARAMETERS loFormSet, lcRANumber, llGenSrNo

IF !USED('RALINE')
  =gfOpenTable(oAriaApplication.DataDir+'RALINE',oAriaApplication.DataDir+'RALINE','SH')
ENDIF
IF !USED('RETAUTH')
  =gfOpenTable(oAriaApplication.DataDir+'RETAUTH',oAriaApplication.DataDir+'RETAUTH','SH')
ENDIF

*WSH [Start]
SELECT RALINE
=gfSetOrder("RALINE")

SELECT RETAUTH
=gfSetOrder("RETAUTH")
*WSH [End]

IF loFormSet.llExternal
  lcForExp = ".T."
ENDIF
 
SELECT RETAUTH
=gfSeek(lcRANumber)

SELECT RaLine
=gfSeek(lcRANumber)
SCAN
  =gfSeek(Style, 'Style')
  =gfSeek('S'+Style.Scale, 'Scale')

  IF !(&lcForExp)
    LOOP
  ENDIF
  
  *-- To add a record in the header (1st grid)
  =lfAddHdrRec(loFormSet, '0')
  
  lnCnt = 0
  FOR lnCnt = 1 TO Scale.Cnt
    lcCnt = STR(lnCnt,1)
    lnLineNo = 0
    lcSzCode = Scale.Sz&lcCnt
    WAIT WINDOW "Adding Style/Size: " + Style+"/"+lcSzCode NOWAIT
    
    IF Qty&lcCnt > 0
      *-- To add records to the master serialized inventory file for each piece                   
      FOR lnCnt1  = 1 TO Qty&lcCnt
        lnLineNo = lnLineNo + 1             
        lcSerNum = ''
        
        IF llGenSrNo
          *-- To add a record to the master serila No. file.
          =lfAddToMast(loFormSet, '', lcSerNum, 'R', Style, lcSzCode, cWareCode, lnLineNo, LineNo, RETAUTH.RANO)
        ENDIF
        
        *-- To add a record in the detail for each serial number (2nd grid) 
        =lfAddDetRec(loFormSet, Style, lcSzCode, lcSerNum, '',lcCnt, '',;
                     lnLineNo, 'N', 'R', RETAUTH.cWareCode, INT(VAL(cRa_LinNo)), RETAUTH.RANO)
      ENDFOR
    ENDIF
  ENDFOR
ENDSCAN
WAIT CLEAR

*!*************************************************************
*! Name      : lfAdjControl
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To enable and disable controls
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfAdjControl
LPARAMETERS loFormSet, lcTrnTyp

WITH loFormSet.ariaform1
  .grdTransactions.Enabled = .T. 
  .grdTranDetail.Enabled = .T.
  .grdTransactions.SETALL('READONLY',.T.,'COLUMN')
  .grdTransactions.Refresh
  
  .grdTransactions.Columns(2).ReadOnly = .F.
  .grdTranDetail.Columns(1).ReadOnly = .F.    
  DO CASE
    CASE lcTrnTyp = '6'
      .grdTransactions.Columns(1).ReadOnly = .T.
      .grdTransactions.Refresh
      .grdTranDetail.Refresh
  ENDCASE

ENDWITH
*-- end of lfAdjControl

*!*************************************************************
*! Name      : lfHdrAftRwChg
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To filter the detail records based on the header
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfHdrAftRwChg
LPARAMETERS loFormSet

PRIVATE lnAlias, llStatus
llStatus = .F.

lnAlias = SELECT()
SELECT(loFormSet.lcTmpSyInv)
=gfSeek('S'+cScale,'Scale')
WITH loFormSet.ariaform1.grdTransactions
  .Columns(9).Header1.Caption = Scale.Sz1
  .Columns(10).Header1.Caption = Scale.Sz2
  .Columns(11).Header1.Caption = Scale.Sz3
  .Columns(12).Header1.Caption = Scale.Sz4
  .Columns(13).Header1.Caption = Scale.Sz5
  .Columns(14).Header1.Caption = Scale.Sz6
  .Columns(15).Header1.Caption = Scale.Sz7
  .Columns(16).Header1.Caption = Scale.Sz8 
ENDWITH  

SELECT(loFormSet.lcTmpDet)
SET KEY TO EVALUATE(loFormSet.lcTmpSyInv+'.cSession')+EVALUATE(loFormSet.lcTmpSyInv+'.Style')+;
           STR(EVALUATE(loFormSet.lcTmpSyInv+'.LineNo'),6)+EVALUATE(loFormSet.lcTmpSyInv+'.cTrCode')+EVALUATE(loFormSet.lcTmpSyInv+'.cIrType')
LOCATE
LOCATE REST WHILE cSession+cStyle+STR(nLineNo,6)+cTrCode+cIrType+cSizeNo+STR(LineNo,6) = ;
                  EVALUATE(loFormSet.lcTmpSyInv+'.cSession')+;
                  EVALUATE(loFormSet.lcTmpSyInv+'.Style') FOR !EMPTY(cSerialNo)

loFormSet.ariaform1.grdTransactions.Columns(1).ReadOnly = FOUND()
LOCATE

*WSH [Start]
IF loFormSet.ActiveMode = 'A' AND EVALUATE(loFormSet.lcTmpSyInv + '.lMark')
  loFormSet.oToolBar.ChangeButtonStatus('pbSelSerNo', 'ENABLED')
ELSE
  loFormSet.oToolBar.ChangeButtonStatus('pbSelSerNo', 'DISABLED')
ENDIF
*WSH [End]

=lfDetAftRwChg(loFormSet)

*loFormSet.ariaForm1.cmdGenLbl.Enabled = .T.
*loFormSet.ariaForm1.cmdSelectLbl.Enabled = .T.
SELECT(lnAlias)
*-- end of lfHdrAftRwChg

*!*************************************************************
*! Name      : lfDetAftRwChg
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To enable/disable the scan serial number textbox
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfDetAftRwChg
LPARAMETERS loFormSet

loFormSet.ariaform1.txtSerNo.Enabled = !loFormSet.llExternal AND ;
                      EVALUATE(loFormSet.lcTmpSyInv+'.lMark') AND ;
                      EVALUATE(loFormSet.lcTmpDet+'.cStatus') = 'E'

loFormSet.ariaform1.txtSerNo.Value    = EVALUATE(loFormSet.lcTmpDet+'.cSerialNo')

*WSH [Start]
loFormSet.ariaform1.txtSerNo.OldValue = loFormSet.ariaform1.txtSerNo.Value
*WSH [End]

loFormSet.ariaform1.grdTranDetail.Refresh
*-- end of lfDetAftRwChg

*!*************************************************************
*! Name      : lfvScanSrNo
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To validate the serial number
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!             lcSerNo   : Serial Number
*!             lcType    : 'R' Receive, 'I' Issue
*!*************************************************************
FUNCTION lfvScanSrNo
PARAMETERS loFormSet, lcSerNo, lcType

LOCAL lcAlias, lcOldIndex, lcSeekKey, lcOldSeek, lnQty, lcTmpKey, lnRecNo, lnReturn
lcAlias    = SELECT()
lcOldIndex = ORDER('INVSERNO')
lnQty      = 0
lcSeekKey  = lcSerNo && + EVALUATE(loFormSet.lcTmpDet+'.cStyle') + EVALUATE(loFormSet.lcTmpDet+'.cSize')
lnRecNo    = RECNO(loFormSet.lcTmpDet)
lnReturn   = -1

*WSH [Start]
*llFound    = .F.
lcOldSeek  = EVALUATE(loFormSet.lcTmpDet+'.cSerialNo') && + EVALUATE(loFormSet.lcTmpDet+'.cStyle') + EVALUATE(loFormSet.lcTmpDet+'.cSize')

IF gfSeek('CSER_NUM  ', 'SEQUENCE') AND INT(VAL(lcSerNo)) >= SEQUENCE.nSeq_No
  *-- 'There is no received quantity for serial number: ' + lcSerNo 
  =gfModalGen('INM42270B42001', 'DIALOG', lcSerNo)
  RETURN 0
ENDIF
*WSH [End]

SELECT(loFormSet.lcTmpDet)

*WSH [Start]
lcTmpKey  = SET("Key", 1)
SET KEY TO
*WSH [End]

SET ORDER TO (loFormSet.lcTmpDet2)

*WSH [Start]
*IF SEEK(lcSerNo+EVALUATE(loFormSet.lcTmpDet+'.cIRType'))
*  =gfModalGen('INM42267B42001','DIALOG', lcSerNo + '|' + EVALUATE(loFormSet.lcTmpDet+'.cStyle')+'/'+EVALUATE(loFormSet.lcTmpDet+'.cSize'))
*  SET ORDER TO (loFormSet.lcTmpDet)
*  IF BETWEEN(lnRecNo,1,RECCOUNT())
*    GOTO lnRecNo
*  ENDIF
*  
*  =lfDetAftRwChg(loFormSet)
*  RETURN 0
*ELSE
*  SET ORDER TO (loFormSet.lcTmpDet)
*  IF BETWEEN(lnRecNo,1,RECCOUNT())
*    GOTO lnRecNo
*  ENDIF
*ENDIF
*
SELECT InvSerNo
=gfSetOrder('SERIALNO')

*-- Check if the old Serial number cannot be changed...
SELECT(loFormSet.lcTmpDet)
IF !EMPTY(cSession) AND !EMPTY(SUBSTR(lcOldSeek,1,8))
  IF SEEK(lcOldSeek)
    SCAN REST WHILE cSerialNo+cStyle+cSize+cIRType = lcOldSeek FOR cRecStat <> 'O' AND !EMPTY(cSession)
      lnQty = lnQty + IIF(cIRType = 'R', nQty, -nQty)
    ENDSCAN
  ENDIF
  
  SELECT InvSerNo
  IF gfSeek(lcOldSeek)
    SCAN REST WHILE CSER_NUM+STYLE+SZ1+CIRTYPE+STR(LINENO,6) = lcOldSeek FOR !EMPTY(cSession)
      lnQty = lnQty + IIF(cIRType = 'R', TotQty, -TotQty)
    ENDSCAN
  ENDIF
  
  IF lcType = 'R'
    IF lnQty <= 0
      *-- 'Current Serial Number: ð has been issued. Cannot change it.'
      =gfModalGen('INM42277B42001', 'DIALOG', SUBSTR(lcOldSeek,1,8) + '|issued')
      lnReturn = 0
    ENDIF
  ELSE
    IF lnQty > 0
      *-- 'Current Serial Number: ð has been received again. Cannot change it.'
      =gfModalGen('INM42277B42001', 'DIALOG', SUBSTR(lcOldSeek,1,8) + '|received again')
      lnReturn = 0
    ENDIF
  ENDIF
ENDIF

IF lnReturn = -1 AND EMPTY(lcSerNo)
  lnReturn = 1
ENDIF

*-- Check if the new Serial number is available or not...
SELECT(loFormSet.lcTmpDet)
IF lnReturn = -1
  lnReturn = 1
  lnQty    = 0
  
  IF SEEK(lcSeekKey)
    SCAN REST WHILE cSerialNo+cStyle+cSize+cIRType = lcSeekKey FOR cRecStat <> 'O' AND IIF(lcType = 'I', !EMPTY(cSession), .T.)
      lnQty = lnQty + IIF(cIRType = 'R', nQty, -nQty)
    ENDSCAN
  ENDIF
*WSH [End]
  
  SELECT InvSerNo
  IF gfSeek(lcSeekKey)
    
    *WSH [Start]
    *SCAN REST WHILE CSER_NUM+STYLE+SZ1+CIRTYPE+STR(LINENO,6) = lcSeekKey ;
              FOR nLineNo = EVALUATE(loFormSet.lcTmpDet+'.nLineNo')
    SCAN REST WHILE CSER_NUM+STYLE+SZ1+CIRTYPE+STR(LINENO,6) = lcSeekKey ;
              FOR IIF(lcType = 'I', !EMPTY(cSession), .T.)
    *WSH [End]
    
      lnQty = lnQty + IIF(cIRType = 'R', TotQty, -TotQty)
      
      *WSH [Start]
      *llFound = .T.
      *WSH [End]
      
    ENDSCAN
  ENDIF
  
  IF lcType = 'I'
    IF lnQty <= 0
      *WAIT WINDOW 'There is no available quantity for serial # : ' + lcSerNo
      =gfModalGen('INM42268B42001','DIALOG', lcSerNo)
      
      *WSH [Start]
      *RETURN 0
      lnReturn = 0
      *WSH [End]
      
    ENDIF
  ELSE
    
    *WSH [Start]
    *IF !llFound
    *  *WAIT WINDOW 'There is no received quantity for serial number: ' + lcSerNo 
    *  =gfModalGen('INM42270B42001','DIALOG', lcSerNo)
    *  RETURN 0
    *ELSE
    *WSH [End]
    
    IF lnQty > 0
      *WAIT WINDOW 'Serial number: ' + lcSerNo + ' already exist in the stock. Cannot proceed.'
      =gfModalGen('INM42271B42001','DIALOG', lcSerNo)
      
      *WSH [Start]
      *RETURN 0
      lnReturn = 0
      *WSH [End]
      
    ENDIF
    
    *WSH [STart]
    *ENDIF
    *WSH [End]
    
  ENDIF

*WSH [Start]
  *SELECT(loFormSet.lcTmpDet)
  *SET ORDER TO (loFormSet.lcTmpDet)
  *SET KEY TO (lcTmpKey)
  *IF BETWEEN(lnRecNo,1,RECCOUNT())
  *  GOTO lnRecNo
  *ENDIF
  *REPLACE cSerialNo WITH lcSerNo
  *
  *IF !EOF()
  *  SKIP
  *  =lfDetAftRwChg(loFormSet)
  *  RETURN 0
  *ENDIF
ENDIF

SELECT(loFormSet.lcTmpDet)
SET ORDER TO (loFormSet.lcTmpDet)
SET KEY TO (lcTmpKey)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF

IF lnReturn = 1
  REPLACE cSerialNo WITH lcSerNo
  
  IF !EOF()
    SKIP
    IF EOF()
      SKIP -1
    ELSE
      =lfDetAftRwChg(loFormSet)
      lnReturn = 0
    ENDIF
  ENDIF
ENDIF
*WSH [End]

SELECT (lcAlias)

*WSH [Start]
RETURN lnReturn
*WSH [End]

*!*************************************************************
*! Name      : lfSaveSrNo
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To save the serial #
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfSaveSrNo
LPARAMETERS loFormSet

SELECT(loFormSet.lcTmpDet)
SET ORDER TO (loFormSet.lcTmpDet3)
*-- To update the new records
IF SEEK('N')
  SCAN REST WHILE cRecStat+cSerialNo = 'N'
    WAIT WINDOW 'Updating Style/Size/Serial Number : ' + cStyle+"/"+cSize+"/"+cSerialNo NOWAIT
    =lfAddToMast(loFormSet,cSession,cSerialNo,cIRType,cStyle,;
                   cSize,cWareCode, LineNo, nLineNo, cTrCode, cTrType)
  ENDSCAN
ENDIF

SELECT InvSerNo
=gfSetOrder('SESSION')

SELECT(loFormSet.lcTmpDet)
*-- To update the modified serial #
IF SEEK('M')
  SCAN REST WHILE cRecStat+cSerialNo = 'M' FOR !EMPTY(cSerialNo)
    WAIT WINDOW 'Updating Style/Size/Serial Number : ' + cStyle+"/"+cSize+"/"+cSerialNo NOWAIT
    SELECT InvSerNo
    
    *WSH [Start]
    *IF gfSeek(EVALUATE(loFormSet.lcTmpDet+'.cSession')+EVALUATE(loFormSet.lcTmpDet+'.cStyle')+;
            EVALUATE(loFormSet.lcTmpDet+'.cSize')+EVALUATE(loFormSet.lcTmpDet+'.cIRType')+;
            STR(EVALUATE(loFormSet.lcTmpDet+'.LineNo'),6))
    IF gfSeek(EVALUATE(loFormSet.lcTmpDet+'.cSession')+EVALUATE(loFormSet.lcTmpDet+'.cStyle')+;
            EVALUATE(loFormSet.lcTmpDet+'.cSize')+EVALUATE(loFormSet.lcTmpDet+'.cIRType')+;
            STR(EVALUATE(loFormSet.lcTmpDet+'.LineNo'),6)+EVALUATE(loFormSet.lcTmpDet+'.cTrCode'))
    *WSH [End]
      
      =gfDelete('')
      SELECT(loFormSet.lcTmpDet)
      =lfAddToMast(loFormSet,cSession,cSerialNo,cIRType,cStyle,;
                   cSize,cWareCode, LineNo, nLineNo, cTrCode, cTrType)
      
    ENDIF
  ENDSCAN
ENDIF 
WAIT CLEAR 
SELECT InvSerNo
LOCATE

IF !EOF()
  =gfTableUpdate()
ENDIF
 
*=gfCloseTable('INVSERNO')

SELECT(loFormSet.lcTmpSyInv)
ZAP
SELECT(loFormSet.lcTmpDet)
ZAP
SET ORDER TO (loFormSet.lcTmpDet)

*!*************************************************************
*! Name      : lfHdrPrint
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To check/uncheck the details
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfHdrPrint
LPARAMETERS loFormSet

PRIVATE lnAlias
lnAlias = SELECT()

*WSH [Start]
loFormSet.AriaForm1.LockScreen = .T.

LOCAL lnTmpRec, lnTmpOrd, lnTmpKey
lnTmpRec = IIF(EOF(loFormSet.lcTmpDet), 0, RECNO(loFormSet.lcTmpDet))
lnTmpOrd = ORDER(loFormSet.lcTmpDet)
*WSH [End]

SELECT(loFormSet.lcTmpDet)

*WSH [Start]
lnTmpKey = SET("Key", 1)
*WSH [End]

*WSH [Start]
*IF SEEK (EVALUATE(loFormSet.lcTmpSyInv+'.cSession')+EVALUATE(loFormSet.lcTmpSyInv+'.Style'))
*  REPLACE REST lPrint WITH  EVALUATE(loFormSet.lcTmpSyInv+'.lPrint') ;
*          WHILE cSession+cStyle = EVALUATE(loFormSet.lcTmpSyInv+'.cSession')+EVALUATE(loFormSet.lcTmpSyInv+'.Style');
*          FOR !EMPTY(cSerialNo)
LOCAL lcSeekKey
lcSeekKey = EVALUATE(loFormSet.lcTmpSyInv+'.cSession')+;
            EVALUATE(loFormSet.lcTmpSyInv+'.Style')+;
            STR(EVALUATE(loFormSet.lcTmpSyInv+'.LineNo'),6)+;
            EVALUATE(loFormSet.lcTmpSyInv+'.cTrCode')+;
            EVALUATE(loFormSet.lcTmpSyInv+'.cIrType')

IF SEEK(lcSeekKey)
  REPLACE REST lPrint WITH  EVALUATE(loFormSet.lcTmpSyInv+'.lPrint') ;
          WHILE cSession+cStyle+STR(nLineNo,6)+cTrCode+cIrType+cSizeNo+STR(LineNo,6) = lcSeekKey ;
          FOR !EMPTY(cSerialNo)
*WSH [End]

ENDIF

*WSH [Start]
SET ORDER TO (lnTmpOrd)
SET KEY TO (lnTmpKey)
IF !EMPTY(lnTmpRec)
  GOTO (lnTmpRec)
ENDIF

loFormSet.AriaForm1.LockScreen = .F.
*WSH [End]

SELECT(lnAlias)
*-- end of lfHdrPrint

*!*************************************************************
*! Name      : lfDetPrint
*! Developer : Wael M Abo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : To check/uncheck the details
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfDetPrint
LPARAMETERS loFormSet

LOCAL lnAlias
lnAlias = SELECT(0)

SELECT (loFormSet.lcTmpDet)
IF EMPTY(cSerialNo)
  REPLACE lPrint WITH .F.
ENDIF

SELECT (loFormSet.lcTmpSyInv)
REPLACE lPrint WITH IIF(EVALUATE(loFormSet.lcTmpDet + '.lPrint'), .T., lPrint)

loFormSet.AriaForm1.Refresh()

SELECT (lnAlias)
*-- end of lfDetPrint

*!*************************************************************
*! Name      : lfGetItemMask
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To get the major/nomajor segments
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfGetItemMask
LPARAMETERS loFormSet

LOCAL oGetItemMask, lcMjrPct
oGetItemMask = CREATEOBJECT('GetItemMask')
lcMjrPct     = oGetItemMask.Do('PM','','0001') &&Major picture
loFormSet.lnMajLen = LEN(lcMjrPct)

DIMENSION laStySeg[1,1]
=oGetItemMask.Do(@laStySeg,'','0001')
FOR lnCnt = 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    lnClrStr = laStySeg[lnCnt , 4]
    lnClrLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR

loFormSet.lnNonMajSt  = lnClrStr
loFormSet.lnNonMajLen = lnClrLen

RETURN
*--end of lfGetItemMask

*!*************************************************************
*! Name      : lfPrintLbl
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To print labels
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfPrintLbl
LPARAMETERS loFormSet

PRIVATE laFileStru

lcTmpLbl = gfTempName()
laFileStru = ''
DIMENSION laFileStru[7,18]
laFileStru[1,1] = 'cStyle'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 12
laFileStru[1,4] = 0
laFileStru[2,1] = 'cColor'
laFileStru[2,2] = 'C'
laFileStru[2,3] = '20'
laFileStru[2,4] = 0
laFileStru[3,1] = 'cSize'
laFileStru[3,2] = 'C'
laFileStru[3,3] =  '5'
laFileStru[3,4] = 0
laFileStru[4,1] = 'cUPC'
laFileStru[4,2] = 'C'
laFileStru[4,3] =  '13'
laFileStru[4,4] = 0
laFileStru[5,1] = 'cSerNo'
laFileStru[5,2] = 'C'
laFileStru[5,3] =  '8'
laFileStru[5,4] = 0
laFileStru[6,1] = 'cFabNam'
laFileStru[6,2] = 'C'
laFileStru[6,3] =  '40'
laFileStru[6,4] = 0
laFileStru[7,1] = 'cFurOrg'
laFileStru[7,2] = 'C'
laFileStru[7,3] =  '20'
laFileStru[7,4] = 0

lnCount = 0
FOR lnCount = 1 TO 7
  STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
              laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
              laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
              laFileStru[lnCount,16]
  STORE 0 TO  laFileStru[lnCount,17],laFileStru[lnCount,18]
ENDFOR
=gfCrtTmp(lcTmpLbl,@laFileStru)

SELECT(loFormSet.lcTmpDet)

*WSH [Start]
*lcKey = EVALUATE(loFormSet.lcTmpSyInv+'.cSession')+EVALUATE(loFormSet.lcTmpSyInv+'.Style')
lcKey = EVALUATE(loFormSet.lcTmpSyInv+'.cSession')+;
        EVALUATE(loFormSet.lcTmpSyInv+'.Style')+;
        STR(EVALUATE(loFormSet.lcTmpSyInv+'.LineNo'),6)+;
        EVALUATE(loFormSet.lcTmpSyInv+'.cTrCode')
        EVALUATE(loFormSet.lcTmpSyInv+'.cIrType')
*WSH [End]

IF SEEK (lcKey)
  
  *WSH [Start]
  *SCAN REST WHILE cSession+cStyle = lcKey FOR lPrint
  SCAN REST WHILE cSession+cStyle+STR(nLineNo,6)+cTrCode+cIrType+cSizeNo+STR(LineNo,6) = lcKey FOR lPrint
  *WSH [End]
  
    =gfSeek(cStyle,'Style')
    =gfSeek(cStyle+cSizeNo,'StyleUPC')
    SELECT(lcTmpLbl)
    INSERT INTO (lcTmpLbl) (cStyle, cColor, cSize, cUPC, cSerNo, cFabNam, cFurOrg) ;
                 VALUES    (SUBSTR(EVALUATE(loFormSet.lcTmpDet+'.cStyle'),1,loFormSet.lnMajLen),;
                            gfCodDes(SUBSTR(EVALUATE(loFormSet.lcTmpDet+'.cStyle'),loFormSet.lnNonMajSt,loFormSet.lnNonMajLen), 'COLOR'),;
                            EVALUATE(loFormSet.lcTmpDet+'.cSize'),;
                            StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,;
                            EVALUATE(loFormSet.lcTmpDet+'.cSerialNo'), Style.cFabNam, Style.cFurOrg)
  ENDSCAN
ENDIF          

SELECT(lcTmpLbl)
LOCATE
IF EOF()
  RETURN
ENDIF

PRIVATE loOGScroll

*-- Create a Dummy Object of the Option Grid...
LOCAL lnDataSess, lcDevice, lcClassDir, oOptionGrid
lnDataSess = SET("Datasession")
lcDevice   = oAriaApplication.gcDevice
oAriaApplication.gcDevice = 'PRINTER'

lcClassDir   = ADDBS(oAriaApplication.ClassDir)
oOptionGrid  = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
loOGScroll   = oOptionGrid.OptionGrid.oHost
lcOGPlatForm = ''
loOgScroll.lcOGPlatForm  = ''
loOgScroll.lcOGLastForm  = 'ICSRLBL'
loOGScroll.llPrintPDF = .F.

DIMENSION loOGScroll.laSelFile[1,3]
loOGScroll.laSelFile = ''

SELECT(lcTmpLbl)
LOCATE

=gfDispRe('ICSRLBL')

SET DATASESSION TO (lnDataSess)

oOptionGrid = .NULL.
RETURN

*!*************************************************************
*! Name      : FileExist
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To return the existance of the file
*!*************************************************************
*! Parameters: cFileToCheckFor 
*!*************************************************************
FUNCTION FileExist
LPARAMETERS cFileToCheckFor

RETURN loOGScroll.FileExist(cFileToCheckFor)

*!*************************************************************
*! Name      : lfvSRSelect
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : Valid function for the Select Serial Numbers button
*!*************************************************************
*! Parameters: loFormSet - Reference to the formset
*!*************************************************************
FUNCTION lfvSRSelect
LPARAMETERS loFormSet, lcIRType

DIMENSION laSizes[1]

LOCAL lnAlias, lnRecNo, lnTmpRec, lcTmpOrd, lcTmpKey, lcSerOrd, lnI, lcI
LOCAL lcStyle, lcSerNum, lcSeekKey, lnTotQty, lnSizeIndx, llFound

lnAlias  = SELECT(0)
lcStyle  = EVALUATE(loFormSet.lcTmpDet + '.cStyle')
lnTmpRec = IIF(!EOF(loFormSet.lcTmpDet), RECNO(loFormSet.lcTmpDet), 0)
lcTmpOrd = ORDER(loFormSet.lcTmpDet)

*--Check if all Serial numbers are assigned...
SELECT (loFormSet.lcTmpDet)
LOCATE FOR EMPTY(cSerialNo)
IF !FOUND()
  *-- 'Serial Numbers are assigned for all issued/received Style:' + lcStyle
  =gfModalGen('INM42273B42001','DIALOG', IIF(lcIRType = 'I', 'issued', 'received') + '|' + lcStyle)
  
  SELECT (loFormSet.lcTmpDet)
  IF !EMPTY(lnTmpRec)
    GOTO (lnTmpRec)
  ENDIF
  
  SELECT (lnAlias)
  RETURN
ENDIF

WAIT WINDOW 'Searching for available Serial Numbers for Style:' + lcStyle NOWAIT

loFormSet.AriaForm1.LockScreen = .T.

SELECT (loFormSet.lcTmpDet)
lcTmpKey = SET("Key", 1)

SET KEY TO
SET ORDER TO (loFormSet.lcTmpDet4)
SET RELATION TO 

SELECT INVSERNO
lcSerOrd = ORDER()
=gfSetOrder("STYLE")

*-- Get Sizes
=gfSeek(lcStyle, 'Style')
=gfSeek('S' + Style.Scale, 'Scale')

DIMENSION laSizes[Scale.cnt]

FOR lnI = 1 TO Scale.cnt
  lcI = ALLTRIM(STR(lnI))
  laSizes[lnI] = EVALUATE('Scale.Sz' + lcI)
  
  DIMENSION laSource&lcI.[1]
  DIMENSION laTarget&lcI.[1]
  STORE '' TO laSource&lcI., laTarget&lcI.
ENDFOR

*-- Get Source Serial Numbers from Master file...
SELECT INVSERNO
IF gfSeek(lcStyle)
  DO WHILE !EOF() AND Style = lcStyle
    lcSerNum  = cSer_Num
    lcSeekKey = Style+Sz1+cWareCode+cSer_Num
    lnTotQty  = 0
    
    SCAN REST WHILE Style+Sz1+cWareCode+cSer_Num+cIRType+STR(LINENO,6) = lcSeekKey FOR IIF(lcIrType = 'I', !EMPTY(cSession), .T.)
      lnTotQty = lnTotQty + IIF(cIRType = 'I', -TotQty, TotQty)
    ENDSCAN
    
    SELECT (loFormSet.lcTmpDet)
    =SEEK(lcSeekKey)
    SCAN REST WHILE cStyle+cSize+cWareCode+cSerialNo = lcSeekKey FOR cRecStat <> 'O' AND IIF(lcIrType = 'I', !EMPTY(cSession), .T.)
      lnTotQty = lnTotQty + IIF(cIRType = 'I', -nQty, nQty)
    ENDSCAN
    SELECT INVSERNO
    
    IF !EMPTY(lcSerNum) AND IIF(lcIRType = 'I', lnTotQty > 0, lnTotQty = 0)
      lnSizeIndx = ASCAN(laSizes, SUBSTR(lcSeekKey, 20, 5))
      
      *--Add this serial number to the source array...
      =lfAdd2Source(lnSizeIndx, lcSerNum)
    ENDIF
  ENDDO
ENDIF

*-- Get New Source Serial Numbers from temp file...
SELECT INVSERNO
=gfSetOrder("SERIALNO")

SELECT (loFormSet.lcTmpDet)
=SEEK(lcStyle)
DO WHILE !EOF() AND cStyle = lcStyle
  lcSerNum  = cSerialNo
  lcSeekKey = cStyle+cSize+cWareCode+cSerialNo
  lnTotQty  = 0
  llFound   = .F.
  
  SCAN REST WHILE cStyle+cSize+cWareCode+cSerialNo = lcSeekKey FOR cRecStat <> 'O' AND IIF(lcIrType = 'I', !EMPTY(cSession), .T.)
    lnTotQty = lnTotQty + IIF(cIRType = 'I', -nQty, nQty)
    llFound  = .T.
  ENDSCAN
  
  IF llFound AND !EMPTY(lcSerNum) AND IIF(lcIRType = 'I', lnTotQty > 0, lnTotQty = 0)
    lnSizeIndx = ASCAN(laSizes, SUBSTR(lcSeekKey, 20, 5))
    lcSizeIndx = ALLTRIM(STR(lnSizeIndx))
    
    SELECT INVSERNO
    llFound = gfSeek(lcSerNum+SUBSTR(lcSeekKey, 1, 19)+SUBSTR(lcSeekKey, 20, 5))
    
    SELECT (loFormSet.lcTmpDet)
    IF !llFound
      *--Add Serial Number to the source array...
      =lfAdd2Source(lnSizeIndx, lcSerNum)
    ENDIF
  ENDIF
ENDDO

SELECT INVSERNO
=gfSetOrder(lcSerOrd)

SELECT (loFormSet.lcTmpDet)
SET ORDER TO (lcTmpOrd)
SET KEY TO (lcTmpKey)
IF !EMPTY(lnTmpRec)
  GOTO (lnTmpRec)
ENDIF

loFormSet.AriaForm1.LockScreen = .F.

WAIT CLEAR

*--Check if no Serial Numbers found...
llFound = .F.
FOR lnI = 1 TO ALEN(laSizes)
  lcI = ALLTRIM(STR(lnI))
  IF !EMPTY(laSource&lcI.[1])
    llFound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llFound
  *-- No available Serial Numbers found for Style: x
  =gfModalGen('INM42272B42001','DIALOG', lcStyle)
  SELECT (lnAlias)
  RETURN
ENDIF

*-- Run the Mover Screen
llUpdate = .F.
DO FORM (oAriaApplication.ScreenHome + "ICSRSEL.SCX") WITH 'Style: ' + ALLTRIM(lcStyle)

loFormSet.AriaForm1.LockScreen = .T.

*--Update the Temp File with selected Serial #(s)
SELECT (loFormSet.lcTmpDet)
LOCATE
*SET ORDER TO
IF llUpdate
  WAIT WINDOW 'Updating selected Serial Numbers for Style:' + lcStyle NOWAIT
  
  *-- Loop for each size
  DO WHILE !EOF()
    lcSize = cSize
    lnI    = ASCAN(laSizes, lcSize)
    lcI    = ALLTRIM(STR(lnI))
    lnJ    = 1
    
    *-- Loop to update all pieces for the current Style/Size
    DO WHILE lnJ <= ALEN(laTarget&lcI,1)
      IF EOF() OR cSize # lcSize
        EXIT
      ENDIF
      IF EMPTY(laTarget&lcI.[lnJ])
        lnJ = lnJ + 1
        LOOP
      ENDIF
      IF !EMPTY(cSerialNo)
        SKIP
        LOOP
      ENDIF
      
      REPLACE cSerialNo WITH laTarget&lcI.[lnJ]
      SKIP
      
      laTarget&lcI.[lnJ] = ''
      lnJ = lnJ + 1
    ENDDO
    
    *-- Skip all remaining pieces for the updated Style/Size
    DO WHILE !EOF() AND cSize = lcSize
      SKIP
    ENDDO
  ENDDO
  
  WAIT CLEAR
ENDIF

SELECT (loFormSet.lcTmpDet)
SET ORDER TO (lcTmpOrd)
SET KEY TO (lcTmpKey)
IF !EMPTY(lnTmpRec)
  GOTO (lnTmpRec)
ENDIF

loFormSet.AriaForm1.LockScreen = .F.

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfAdd2Source
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : Function to add a serial number to the Source Array
*!*************************************************************
*! Example   : =lfAdd2Source()
*!*************************************************************
FUNCTION lfAdd2Source
LPARAMETERS lnSizeIndx, lcSerNum

LOCAL lcSizeIndx, lnRows
lcSizeIndx = ALLTRIM(STR(lnSizeIndx))
lnArrRows  = ALEN(laSource&lcSizeIndx) + IIF(EMPTY(laSource&lcSizeIndx.[1]), 0, 1)

DIMENSION laSource&lcSizeIndx.[lnArrRows]
laSource&lcSizeIndx.[lnArrRows] = lcSerNum
