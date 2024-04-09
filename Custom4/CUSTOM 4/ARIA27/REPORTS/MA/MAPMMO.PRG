*:***************************************************************************
*: Program file  : MAPMMO
*: Program desc. : Print Material Manufacturing Order
*: For Report    : MAPMMO.FRX,MFPRTCSA.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MAterial
*: Developer     : WAB - Walid Abd El Wahab
*: Date			 : 02/10/2000
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetCodes, lfEndGroup, lfSum, lfGetTitle, lfStGroup, lfvOMsg,
*:                 lfShiftArr,lfvPrtCs
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MAPMMO
*:***************************************************************************
*:Enh. # 301375
*:***************************************************************************
*:B128486,1 MMR 07/29/2005 fix bug OF REPEATING item IN BOM REPORT.
*:***************************************************************************
DIMENSION laCompAdd[6]                 && Array to hold company addresses
DIMENSION laShpAdr[5]
DIMENSION laSum[9]                     && Array to sum size quantities
STORE SPACE(0) TO laCompAdd,lcSt, lcStyTitle, lcShpName, laShpAdr, lcOper ,;
      lclotNo,lcClrLD
STORE .F. TO llLogo , llEndGroup
lcCostF   = gfTempName()
lcMGroup = 'CUTTKT'
*cMfgOrdNo
DECLARE laColor[1,2]
  
*-- If we will print the cost sheet
IF llrPrtCs
  STORE SPACE(0) TO lcCostItm, lcHead1, lcHead2
  STORE SPACE(0) TO  lcPattrn,lcStyMaj
  lcInGroup = 'CUTTKT+TYP'
  *-- Create array to hold the cost element titles.
  DIMENSION laCost[8,2]
  laCost[1,1]  = 'M_CTTYPE1 '
  laCost[2,1]  = 'M_CTTYPE2 '
  laCost[3,1]  = 'M_CTTYPE3 '
  laCost[4,1]  = 'M_CTTYPE4 '
  laCost[5,1]  = 'M_CTSLBL1 '
  laCost[6,1]  = 'M_CTSLBL2 '
  laCost[7,1]  = 'M_CTSLBL3 '
  laCost[8,1]  = 'M_CTSLBL4 '
  =gfGetMemvar(@laCost,gcAct_Comp)
  *-- create the temporary file to print the cost sheet from.
  SELECT CTKTBOM
  COPY STRUCTURE TO (gcWorkDir+lcCostF)
  SELE 0
  USE (gcWorkDir+lcCostF) EXCL
  INDEX ON CUTTKT+Typ+cCatgTyp+Item+IClr TAG (lcCostF)
ENDIF
lcBOMTit  = ALLTRIM(gfGetMemvar('M_MMOLNLBL',gcAct_Comp))
lcBOMTit  = IIF(RIGHT(lcBOMTit,1) ='#', lcBOMTit,lcBOMTit+'#')
lcTime   = TIME()                       && Variable to hold the Time
*-- Temporary file of the MMO report .
lcMainF = gfTempName()
*-- Get the system setting of dyelot (Y/N)

*HDM[START] Use material setup not ic setup
*llDyelot = (gfGetMemvar('M_DYELOT',gcAct_Comp)='Y')
llDyelot = (gfGetMemvar('M_MATDYE  ',gcAct_Comp)='Y')
*hdm [end]
SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
* Get the company addresses
laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)
DO lfShiftArr WITH laCompAdd

*-- Set necessary relations



*-- Create the temporary file based on mmfgordd  and add two fields to handle
*-- notes
SELECT MMFGORDD
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,4]

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'NoteFlag'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Notes'
laFileStru[lnFileStru,2] = 'M'
laFileStru[lnFileStru,3] = 0
laFileStru[lnFileStru,4] = 0

Create Table (gcWorkDir+lcMainF) FROM ARRAY laFileStru
INDEX ON cMfgOrdNo+cWareCode+cFabric+Color+Dyelot+NoteFlag TAG (lcMainF)

SELECT MMFGORDH
*-- Add the status filter to the expression returned from the report generator
lcRpExp = '(' + lcRpExp + ') .AND. ' + IIF(lcstatus='L',"Status $'CHOA'",;
          "Status = lcStatus")

SCAN FOR &lcRpExp
  lcWareCode = SPACE(8)
  lcFabric   = SPACE(7)
  lcColor    = SPACE(6)
  SELECT MMFGORDD
  
  IF SEEK(MMFGORDH.cMfgOrdNo)
    SCAN WHILE cMfgOrdNo = MMFGORDH.cMfgOrdNo FOR TranCD='1'
      SCATTER MEMVAR
      m.NoteFlag = 'N'
      m.Notes    = ''
      INSERT INTO (lcMainF) FROM MEMVAR
      *-- Get last fabric in the MMO to put the notes lines directly after it.
      IF lcfabric + lcColor < cFabric + Color
        lcFabric  = cFabric
        lcColor   = Color
      ENDIF

      *HDM [Start] Comment this part as it should be after this scan part
      **-- We will print notes with each warehouse.
      *IF cWareCode # lcWareCode .AND. !EMPTY(lcWareCode)
      *  m.cWareCode = lcWareCode
      *  lcWareCode  = cWareCode
      *  m.cFabric   = lcFabric
      *  m.Color     = lcColor
      *  m.Dyelot    = CHR(255)
      *  M.QTY1      = 0
      *  M.TOTQTY    = 0
      *  *-- Get the FABRIC note.
      *  IF llRPrtSn .AND. SEEK('G'+MMFGORDH.cFabric,'NotePad')
      *    m.NoteFlag = 'S'
      *    m.Notes    = Notepad.MNotes
      *    INSERT INTO (lcMainF) FROM MEMVAR
      *  ENDIF
      *  *-- Get the M.M.O. note.
      *  IF llrPrtMmon  .AND. SEEK('I'+m.cMfgOrdNo,'NotePad')
      *    m.NoteFlag = 'T'
      *    m.Notes    = Notepad.MNotes
      *    INSERT INTO (lcMainF) FROM MEMVAR
      *  ENDIF
      *ENDIF
      *HDM [End]

      IF EMPTY(lcWareCode)
        lcWareCode = cWareCode
        lcFabric   = cFabric
        lcColor    = Color 
      ENDIF
    ENDSCAN
    m.cWareCode = lcWareCode
    m.cFabric   = lcFabric
    m.Color     = lcColor 
    m.Dyelot    = CHR(255)
    m.nmfgtotqty = 0
    *-- Print notes for last warehouse.
    IF llRPrtSn .AND. SEEK('G'+MMFGORDH.cFabric,'NotePad')
      m.NoteFlag = 'S'
      m.Notes    = Notepad.MNotes
      INSERT INTO (lcMainF) FROM MEMVAR
    ENDIF
    IF llrPrtMmon  .AND. SEEK('O'+m.cMfgOrdNo,'NotePad')
      m.NoteFlag = 'T'
      m.Notes    = Notepad.MNotes
      INSERT INTO (lcMainF) FROM MEMVAR
    ENDIF
    go top in (lcMainF)
  ENDIF
  *-- If we will print cost sheet , insert cost sheet records in the temprorary file.
  IF llrPrtCs .AND. SEEK('T'+MMFGORDH.cMfgOrdNo,'CTKTBOM')
    SELECT CTKTBOM
    SCAN WHILE cImTyp+CutTkt = 'T'+MMFGORDH.cMfgOrdNo FOR cCatgTyp $ 'FT'
      SCATTER MEMVAR
      INSERT INTO (lcCostF) FROM MEMVAR
    ENDSCAN
  ENDIF
 
ENDSCAN  && End loop of MMFGORDH file.




SELECT (lcMainF)
*HDM [Start] Include the color in the relation to get the data at color level
*SET RELATION TO cFabric INTO Fabric ADDITIVE
SET RELATION TO cFabric+color INTO Fabric ADDITIVE
*HDM [End]
SET RELATION TO cMfgOrdNo INTO MMFGORDH ADDITIVE


lcLogoPic = gfTempName()
IF SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')
  llLogo = .T.
  lcObj_Id = OBJLINK.cObject_ID
  *-- Make cursor contain one field and one record holding the company logo
  SELECT gobject;
   FROM Objects         ;
   WHERE Objects.cobject_id = lcObj_Id ;
   INTO CURSOR (lcLogoPic)
ENDIF
lcOgPlatForm = 'WINDOW'
SELECT (lcMainF)

=lfOptProg()

DO gfDispRe WITH EVALUATE('lcFormNam')

SET RELATION TO

*-- Print the cost sheet if required.
IF llrPrtCs  .AND. !EOF(lcCostF)
  
  SELECT (lcCostF)
  SET RELATION TO LEFT(item,7)+iclr INTO Fabric ADDITIVE
  SET RELATION TO cMfgOrdNo INTO MMFGORDH ADDITIVE
  *--
  lcTmp=lcOGTmpForm
  lcOGTmpForm=''
  *--
  DO gfDispRe WITH 'MFPRCSA'

  *--
  lcOGTmpForm=lcTmp
  *--
ENDIF
USE IN (lcMainF)
ERASE (gcWorkDir+lcMainF)
IF USED(lcCostF)
  USE IN (lcCostF)
  ERASE (gcWorkDir+lcCostF)
ENDIF

*!*************************************************************
*! Name      : lfvOMsg
*! Developer : WAB - Walid Abd El Wahab
*! Date      : 02/10/2000
*! Purpose   : To Open the Optional message screen
*!*************************************************************
*! Called from : MFPRTCT.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOMsg()
*!*************************************************************
FUNCTION lfvOMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcOMsg1'   && 1st. line Variable
laOptMsg[1,2] = 75          && Line length
laOptMsg[2,1] = 'lcOMsg2'   && 1st. line Variable
laOptMsg[2,2] = 75          && Line length
laOptMsg[3,1] = 'lcOMsg3'   && 1st. line Variable
laOptMsg[3,2] = 75          && Line length

= gfOptMsg('laOptMsg')      && Call Function to write optional message.

*!*************************************************************
*! Name      : lfvPrtCs
*! Developer : WAB - Walid Abd El Wahab
*! Date      : 02/10/2000
*! Purpose   : Valid function of the print cost sheet setting on the option
*!             grid to enable or disable the setting of print cost on cost sheet.
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrtCs()
*!*************************************************************
FUNCTION lfvPrtCs
*-- Get the position of the print cost setting in the array to enable or 
*-- disable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LLRPCost'),1)
laOGObjCnt[lnPos] = llrPrtCs
= lfOGShowGet('LLRPCost')

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : WAB - Walid Abd El Wahab
*! Date      : 02/10/2000
*! Purpose   : When function of the option grid.
*!*************************************************************
*! Called from : MFPRTCT.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LLRPCost'),1)
laOGObjCnt[lnPos] = llrPrtCs
= lfOGShowGet('llRpCost')  

*!*************************************************************
*! Name        : lfShiftArr
*! Developer   : WAB - Walid Abd El Wahab
*! Date        : 02/10/2000
*! Purpose     : Function to Pack the passed array
*!*************************************************************
*! Calls       : 
*!              Procedures : None
*!              Functions  : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : DO lfShiftArr WITH laCompAdd
*!*************************************************************
FUNCTION lfShiftArr

PARAMETERS laArray
PRIVATE lnAlen,lnCount, lnC
* Get length of the array
lnALen = ALEN(laArray,1)
* check each element of the array if it is empty
FOR lnCount = 1 TO lnALen
  IF EMPTY(laArray[lnCount])
    * If any element is empty shift down the later elements
    FOR lnC = lnCount TO lnALen-1
      laArray[lnC]=laArray[lnC+1]
    ENDFOR
    laArray[lnAlen]=''
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfGetCodes
*! Developer : WAB - Walid Abd El Wahab
*! Date      : 02/10/2000
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Called from : MFPRCTA.FRX
*!*************************************************************
*! Calls       : gfGetAdr(), lfShiftArr()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetCodes()
*!*************************************************************

FUNCTION lfGetCodes

llEndGroup = .F.
*-- Get the suitable title due to status to be printed on the .FRX
DO CASE
  CASE MMFGORDH.STATUS='O'
    lcSt = 'OPEN'
  CASE MMFGORDH.STATUS='H'
    lcSt = 'HOLD'
  CASE MMFGORDH.STATUS='A'
    lcSt = 'ACTUAL'
  CASE MMFGORDH.STATUS='C'
    lcSt = 'COMPLETED'
ENDCASE
*-- Get the warehouse addresses
IF SEEK(&lcMainF..cwarecode,'WAREHOUS')
  lcShpName   = WAREHOUS.cDesc
  laShpAdr[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  laShpAdr[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  laShpAdr[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  laShpAdr[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  laShpAdr[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
  DO lfShiftArr WITH laShpAdr
ELSE
  STORE SPACE(0) TO lcShpName, laShpAdr
ENDIF

RETURN ''

*!*************************************************************
*! Name      : lfGetClr
*! Developer : WAB - Walid Abd El Wahab
*! Date      : 02/10/2000
*! Purpose   : get color long description
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Color long name
*!*************************************************************
*! Example     : = lfGetClr()
*!*************************************************************
FUNCTION lfGetClr
PRIVATE  lcClrDesc
laColor[1,1] = 'CLRLNAME'
laColor[1,2] = 'lcClrLD'  
lcClrDesc  = gfCodDes(Color, 'COLOR')
=gfRltFld(Color, @laColor, 'COLOR')
RETURN lcClrLD


*!*************************************************************
*! Name      : lfEndGroup
*! Developer : WAB - Walid Abd El Wahab
*! Date      : 02/10/2000
*! Purpose   : To state that if we would print the word "Continued" 
*!             and to initialize some variables.
*!*************************************************************
*! Called from : MFPRCTA.FRX, MFPRTCSA,FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup
* Set this variable .T. to don't print the word "CONTINUED"
llEndGroup = .T.
RETURN ''

*!*************************************************************
*! Name      : lfGetTitle
*! Developer : WAB - Walid Abd El Wahab
*! Date      : 02/10/2000
*! Purpose   : To get the cost element title.
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetTitle()
*!*************************************************************
FUNCTION lfGetTitle
PRIVATE lcSetEx
lcCostItm = ''
lcSetEx = SET('EXACT')
SET EXACT ON
lnAlias = SELECT(0)
SELECT (lcCostF)

*-- the LACOST array has 10 rows, 5 for category type, and 5 for the 
*-- coresponding titles.  so, get the row of the cost element and add 5 to 
*-- the row number to get the cost element title row number.
IF ASCAN(laCost,cCatgTyp) # 0
  lcCostItm = laCost[ASUBSCRIPT(laCost,ASCAN(laCost,cCatgTyp),1)+4,2]
ENDIF
lcHead1   = ''
lcHead2   = ''
IF cCatgTyp = 'T'
  lcHead1   = '                  DATE                 QTY      DATE     DIRECT'
  lcHead2   = 'VENDOR  ORDERD PO #  ORDRD RECVD. SHIPD.'
ELSE 
  IF cCatgTyp = 'F'
    lcHead1   = '[-------------------- PULLED -------------------]  TOTAL'
  ENDIF
ENDIF

SET EXACT &lcSetEx
SELECT (lnAlias)
RETURN ''

*!*************************************************************
*! Name      : lfStGroup
*! Developer : WAB - Walid Abd El Wahab
*! Date      : 02/10/2000
*! Purpose   : To Initialize the variable llEndGroup
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStGroup()
*!*************************************************************
FUNCTION lfStGroup
llEndGroup = .F.
RETURN ''
*!*************************************************************
*! Name      : lfGetOp
*! Developer : WAB - Walid Abd El Wahab
*! Date      : 02/10/2000
*! Purpose   : to be done on the starting of page, used in lot cost 
*!             sheet form 
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetOp()
*!*************************************************************
FUNCTION lfGetOp
*:B128486,1 MMR 07/29/2005 fix bug OF REPEATING item IN BOM REPORT.[Start]
lnOld=ALIAS()
SELECT MMFGORDH
SEEK EVAL(lcCostF+'.cuttkt')
*:B128486,1 MMR[End]
lcStyMaj   = MMFGORDH.cFabric
lcPattrn = ''
*:B128486,1 MMR 07/29/2005 fix bug OF REPEATING item IN BOM REPORT.[Start]
SELECT (lnOld)
*:B128486,1 MMR.[End]