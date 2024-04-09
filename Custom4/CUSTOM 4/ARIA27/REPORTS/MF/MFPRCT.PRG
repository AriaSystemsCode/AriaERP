*:***************************************************************************
*: Program file  : MFPRCT
*: Program desc. : Print CUTTING TICKETS
*: For Report    : MFPRCTA.FRX,MFPRTCSA.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Developer     : AHMED MOHAMMED MOHAMMED
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetCodes, lfEndGroup, lfSum, lfGetTitle, lfStGroup, lfvOMsg,
*:                 lfShiftArr,lfvPrtCs
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFPRTCT
*:***************************************************************************
*:Modifications:
*B802039,1 AMM 03/24/1999 Fix some bugs in the form.
*B802534,1 RAMY 09/09/1999 Update the 3rd group in the .FRX to print all
*                          DYELOTs for the same color not only the first one.
*B802660,1 RAMY 10/03/1999 Add variable that holds the .FRX name, added in (SYDREPRT) , (SYREPUVR)
*E301338,1 AMM 10/25/1999 Call PO bill of Material after printing the PO form
*C200095,1 RAMY 10/25/1999 Make a custome CutTkt form and custom CS for Mexx
*B603214,1 RAMY 11/07/1999 Fix the problem of printing addetion line when printing notes 
*B603214,1                 [updated in the FRX]
*C101696,1 RAMY 11/29/1999 Make a custome CutTkt form for Elien Fisher
*B803250,1 SSH  23/05/2000 Prevet print sizes desc incase of Extended size scale.
*B803250,1 SSH  23/05/2000 Modify Frx by adding display condition.
*C101885,1 SSH  05/06/2000 Support printing @,Say Forms
*B803322,1 SSH  05/06/2000 Print Div Long Name If Exist Instead of Company NAme
*C101931,1 ABD  08/31/2000 Make a custome CutTkt form for Fresh Produce.
*C102114,1 AAN  02/25/2001 Adding a logical variable to indicate whether to 
*C102114,1                 print the call the gfdispre from the optional 
*C102114,1                 program or from the main program.
*B605017,1 KHM  10/11/2001 Fix the bug of printing to a file when the report's
*B605017,1                 mode is graphics.
*B605222,1 KHM 12/20/2001  Fix the bug of not displaying the width and vendor
*B605222,1                 when printing the bill of material.
*C102730,1 ALB 11/25/2002  Adding a function to get the color segment.
*:*********************************************************************************************

DIMENSION laCompAdd[6]                 && Array to hold company addresses
DIMENSION laShpAdr[5]
DIMENSION laSum[9]                     && Array to sum size quantities
STORE SPACE(0) TO laCompAdd,lcSt, lcStyTitle, lcShpName, laShpAdr, lcOper
STORE .F. TO llLogo , llEndGroup
lcCostF   = gfTempName()
lcMGroup = 'CUTTKT'

*C102114,1 AAN 02/25/2001 [START] initialize this variable to check if print
*C102114,1                the report from the main or from the optional program.
llPoDisp = .T.
*C102114,1 AAN [END]

*B803250,1 SSH  23/05/2000 Variable to indicate if Extended Size Scale .
llMScale   = gfGetMemVar('M_USEEXSSC')
*B803250,1 SSH  23/05/2000 [End]
*C101696,1 RAMY Define some new variable [START]
DIMENSION laVenAdr[5]
STORE SPACE(0) TO lcVenName
*C101696,1 RAMY [END]
*-- If we will print the cost sheet
IF llrPrtCs
  STORE SPACE(0) TO lcCostItm, lcHead1, lcHead2
  *E301338,1 AMM Initialize variable to use instead of fields
  STORE SPACE(0) TO lcLotNo , lcStyMaj , lcPattrn 
  *E301338,1 AMM Adjust the inner group of the BOM report
  lcInGroup = 'CUTTKT+TYP'
  *E301338,1 AMM end
  IF !USED('Fabric')
    =gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
  *B605222,1 KHM 12/20/2001 (Begin) Setting the appropriate order.
  ELSE
    SET ORDER TO TAG Fabric IN Fabric
  *B605222,1 KHM 12/20/2001 (End)
  ENDIF
  *-- Create array to hold the cost element titles.
  DIMENSION laCost[10,2]
  laCost[1,1]  = 'M_CMTYPE1 '
  laCost[2,1]  = 'M_CMTYPE2 '
  laCost[3,1]  = 'M_CMTYPE3 '
  laCost[4,1]  = 'M_CMTYPE4 '
  laCost[5,1]  = 'M_CMTYPE5 '
  laCost[6,1]  = 'M_CMSLBL1 '
  laCost[7,1]  = 'M_CMSLBL2 '
  laCost[8,1]  = 'M_CMSLBL3 '
  laCost[9,1]  = 'M_CMSLBL4 '
  laCost[10,1] = 'M_CMSLBL5 '

  =gfGetMemvar(@laCost,gcAct_Comp)

  *-- create the temporary file to print the cost sheet from.
  SELECT CTKTBOM
  COPY STRUCTURE TO (gcWorkDir+lcCostF)
  SELE 0
  USE (gcWorkDir+lcCostF) EXCL
  INDEX ON CUTTKT+Typ+cCatgTyp+Item+IClr TAG (lcCostF)
ENDIF
*E301338,1 AMM Change the variable name to fit running PO BOM form
*lcTitle  = ALLTRIM(gfGetMemvar('M_PRDLNLBL',gcAct_Comp))
*lcTitle  = IIF(RIGHT(lcTitle,1) ='#', lcTitle,lcTitle+'#')
lcBOMTit  = ALLTRIM(gfGetMemvar('M_PRDLNLBL',gcAct_Comp))
lcBOMTit  = IIF(RIGHT(lcBOMTit,1) ='#', lcBOMTit,lcBOMTit+'#')
*E301338,1 AMM end
lcTime   = TIME()                       && Variable to hold the Time
*-- Temporary file of the cutting ticket report.
lcMainF = gfTempName()
*-- Get the system setting of dyelot (Y/N)
llDyelot = (gfGetMemvar('M_DYELOT',gcAct_Comp)='Y')
SELECT SYCCOMP
SEEK gcAct_Comp
*B803322,1 SSH  05/06/2000 Print Div Long Name If Exist Instead of Company NAme
DECLARE laDivLNam[1,2]
STORE ''  TO laDivLNam , lcDivLNam
laDivLNam[1,1] = 'DIVLNAME'
laDivLNam[1,2] = 'lcDivLNam'
*B803322,1 SSH  [END]
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
lcStyTitle = gfItemMask('HI')


*-- Set necessary relations
SELECT OBJLINK
SET RELATION TO Objlink.cobject_id INTO Objects ADDITIVE

SELECT STYLE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

SELECT CUTPICK
SET RELATION TO 'O'+ Cutpick.order INTO Ordhdr ADDITIVE

SELECT CUTTKTH
SET RELATION TO 'S'+ Cuttkth.style INTO Objlink ADDITIVE

*-- Create the temporary file based on CUTTKTL and add two fields to handle
*-- notes
SELECT CUTTKTL
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
*C101696,1 RAMY Add element to the array that holds the temp. lines 

*DIMENSION laFileStru[lnFileStru+2,4]
DIMENSION laFileStru[lnFileStru+3,4]

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

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'nYield'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 7
laFileStru[lnFileStru,4] = 3
*C101696,1 RAMY [END]


Create Table (gcWorkDir+lcMainF) FROM ARRAY laFileStru
INDEX ON CUTTKT+cWareCode+Style+Dyelot+NoteFlag TAG (lcMainF)

SELECT CUTTKTH
*-- Add the status filter to the expression returned from the report generator

*C101931,1 ABD Change the Select status way from  inlist to be mover and default Open. [Begin]
*lcRpExp = '(' + lcRpExp + ') .AND. ' + IIF(lcstatus='L',"Status $'CHOA'",;
*          "Status = lcStatus")
lcRpExp = '(' + lcRpExp + ')'+'.AND. Status $ lcStatus'
SELECT STYLE
lcSaveOrde = Order()
*-- Add new relation if user select by style.
set order to Cstyle
SELECT CUTTKTH
SET RELATION TO Cuttkth.Style INTO STYLE ADDITIVE
GOTO TOP
*-- Store empty to vendor & M. Opration expertion.
STORE "" TO lcVenExp , lcMopExp
*C101931,1 ABD  [End]

*C101931,1 ABD Cut expration from lcRpExp for cust form for FP. [Begin]
IF ALLTRIM(lcFormName) = 'MFPRCTFP'
  =lfUsrVldFn('lfGetExp','MFPRCTFP')
ENDIF
*C101931,1 ABD  [End]

SCAN FOR &lcRpExp
  lcWareCode = SPACE(8)
  lcStyle    = SPACE(20)
  SELECT CUTTKTL
   *C101931,1 ABD Cut expration from lcRpExp for cust FP. [Begin]
    IF ALLTRIM(lcFormName) = 'MFPRCTFP'                 ;
       .AND. (!EMPTY (lcVenExp) .OR. !EMPTY (lcMopExp)) ;
       .AND. !lfUsrVldFn('lfvMop_ApV','MFPRCTFP')
      LOOP
    ENDIF
  *C101931,1 ABD  [End]
  
  IF SEEK(CUTTKTH.CutTkt)
    SCAN WHILE CutTkt = CutTktH.CutTkt FOR TranCD='1'
      SCATTER MEMVAR
      m.NoteFlag = 'N'
      m.Notes    = ''
      INSERT INTO (lcMainF) FROM MEMVAR
      *-- Get last style in the cuttkt to put the notes lines directly after it.
      IF lcStyle < Style
        lcStyle     = Style
      ENDIF
      *-- We will print notes with each warehouse.
      IF cWareCode # lcWareCode .AND. !EMPTY(lcWareCode)
        m.cWareCode = lcWareCode
        lcWareCode  = cWareCode
        m.Style     = lcStyle
        m.Dyelot    = CHR(255)
        M.QTY1      = 0
        M.TOTQTY    = 0
        *-- Get the style note.
        IF llRPrtSn .AND. SEEK('F'+CUTTKTH.Style,'NotePad')
          m.NoteFlag = 'S'
          m.Notes    = Notepad.MNotes
          INSERT INTO (lcMainF) FROM MEMVAR
        ENDIF
        *-- Get the cutting ticket note.
        IF llRPrtCtn .AND. SEEK('I'+m.CutTkt,'NotePad')
          m.NoteFlag = 'T'
          m.Notes    = Notepad.MNotes
          INSERT INTO (lcMainF) FROM MEMVAR
        ENDIF
      ENDIF
      IF EMPTY(lcWareCode)
        lcWareCode = cWareCode
        lcStyle    = Style
      ENDIF
    ENDSCAN
    m.cWareCode = lcWareCode
    m.Style     = lcStyle
    m.Dyelot    = CHR(255)
    *-- Print notes for last warehouse.
    IF llRPrtSn .AND. SEEK('F'+CutTktH.Style,'NotePad')
      m.NoteFlag = 'S'
      m.Notes    = Notepad.MNotes
      INSERT INTO (lcMainF) FROM MEMVAR
    ENDIF
    IF llRPrtCtn .AND. SEEK('I'+m.CutTkt,'NotePad')
      m.NoteFlag = 'T'
      m.Notes    = Notepad.MNotes
      INSERT INTO (lcMainF) FROM MEMVAR
    ENDIF
    go top in (lcMainF)
  ENDIF
  *-- If we will print cost sheet , insert cost sheet records in the temprorary file.
  IF llrPrtCs .AND. SEEK('M'+CUTTKTH.CutTkt,'CTKTBOM')
    SELECT CTKTBOM
    SCAN WHILE cImTyp+CutTkt = 'M'+CutTktH.CutTkt FOR cCatgTyp $ 'FTS'
      SCATTER MEMVAR
      INSERT INTO (lcCostF) FROM MEMVAR 
    ENDSCAN
  ENDIF
 
ENDSCAN  && End loop of CUTTKTH file.

*C101931,1 ABD remove the new relation if user select by style. [Begin]
SELECT STYLE 
SET ORDER TO &lcSaveOrde
SELECT CUTTKTH
SET RELATION OFF INTO STYLE
*C101931,1 ABD  [End]

SELECT (lcMainF)
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO "1"+cuttkt+IIF(&lcMainF..NoteFlag='N',style,SPACE(20)) INTO Cutpick ADDITIVE
SET RELATION TO CutTkt INTO CUTTKTH ADDITIVE

IF llRPrtAlo
  SET SKIP TO CUTPICK
ENDIF

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

*B605017,1 KHM  10/11/2001 (Begin) It should be WINDOWS.
*lcOgPlatForm = 'WINDOW'
lcOgPlatForm = 'WINDOWS'
*B605017,1 KHM  10/11/2001 
SELECT (lcMainF)

*C200095,1 RAMY 10/25/99 [start] Add this line to check if there is an optional program or not
=lfOptProg()
*C200095,1 RAMY 10/25/99 [end]

*B802660,1 RAMY change this line to print useing the variable that holds the FRX name [start]
*DO gfDispRe WITH 'MFPRCTA'
*C101885,1 SSH  05/06/2000  [Begin] Support printing @,Say Forms
*DO gfDispRe WITH EVALUATE('lcFormNam')
llNoRec= .T.
lcPrgName  = lcFormNam
llIsAparel = lfIsApparl(@lcPrgName)
IF llIsAparel
  =gfSetDevice()
  DO EVAL('lcPrgName')
  IF !llNoRec
    DO ENDREPORT
  ENDIF
ELSE
  *C102114,1 AAN 02/25/2001 [Start].
  *DO gfDispRe WITH EVALUATE('lcFormNam')
  IF llPoDisp
    DO gfDispRe WITH EVALUATE('lcFormNam')
  ENDIF
  *C102114,1 AAN 02/25/2001 [End].
ENDIF
*C101885,1 SSH[End]
*B802660,1 RAMY [end]

SET RELATION TO


*C101931,1 ABD Dont print the cost sheet for Customer FP. [Begin]
IF ALLTRIM(lcFormName) # 'MFPRCTFP'
  *C101931,1 ABD [END]
  *-- Print the cost sheet if required.
  IF llrPrtCs  .AND. !EOF(lcCostF)
    SELECT (lcCostF)
    SET RELATION TO LEFT(item,7)+iclr INTO Fabric ADDITIVE
    SET RELATION TO CutTkt INTO CUTTKTH ADDITIVE
    *B802660,1 RAMY Add these lines to store the old .FRX name [start]
    lcOldTmp = lcOGTmpForm
    lcOGTmpForm = ""
    lcOldOptProg = lcOptProg
    *B802660,1 RAMY [end]
  
    *C200095,1 RAMY 10/25/99 [start] Add these lines to check if there is custom cost sheet or not
    *DO gfDispRe WITH 'MFPRTCSA'
    *lcFrxNam = lfGetForm('CT')
    
    lcFrxNam = SUBSTR(lcFormNam , 7,2)
    lcFrxNam = 'MFPRCS' + lcFrxNam
    IF FILE(gcRepHome + 'MF\' + lcFrxNam + '.FRX')
      llPrnAgan = .T.
	  *C102114,1 AAN 02/25/2001 [Start].
      *DO gfDispRe WITH EVALUATE('lcFrxNam')
      IF llPoDisp
        DO gfDispRe WITH EVALUATE('lcFrxNam')
      ENDIF
      *C102114,1 AAN 02/25/2001 [End].
    ELSE
      *C101885,1 SSH  05/06/2000 Support printing @,Say Forms
      IF llIsAparel
        =gfSetDevice()
        IF FILE(gcRepHome + 'MF\' + lcFrxNam + '.FXP')
          DO EVAL('lcFrxNam')
          IF !llNoRec
            DO ENDREPORT
          ENDIF
        ENDIF
      ELSE
      *C101885,1 SSH[END].

        *C102114,1 AAN 02/25/2001 [Start].        
        *DO gfDispRe WITH 'MFPRCSA'
        IF llPoDisp
          DO gfDispRe WITH 'MFPRCSA'
        ENDIF
        *C102114,1 AAN 02/25/2001 [End].
      *C101885,1 SSH  05/06/2000 Support printing @,Say Forms      
      ENDIF
      *C101885,1 SSH[END].
    ENDIF
    *C200095,1 RAMY 10/25/99 [END]
    
    *B802660,1 RAMY Add these lines to retrive the old .FRX name [start]
    lcOGTmpForm = lcOldTmp
    lcOptProg   = lcOldOptProg
    *B802660,1 RAMY [end]
  ENDIF
  *B605222,1 KHM 12/20/2001 (Begin) If printing the BOM restore the order
  IF llrPrtCs  
    SET ORDER TO TAG cFabric IN Fabric
  ENDIF  
  *B605222,1 KHM 12/20/2001   
  *C101931,1 ABD End for if Statement for Cutomer FP[Begin]
ENDIF
*C101931,1 ABD [END] 

*C101696,1 RAMY Clear the temp files created to the form EF
IF RIGHT(lcFormName, 2) = UPPER('EF')
  =lfUsrVldFn('lfClsFiles','MFPRCTEF')
ENDIF
*C101696,1 RAMY [end]

USE IN (lcMainF)
ERASE (gcWorkDir+lcMainF)
IF USED(lcCostF)
  USE IN (lcCostF)
  ERASE (gcWorkDir+lcCostF)
ENDIF

*!*************************************************************
*! Name      : lfGetCodes
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
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

*B803322,1 SSH  05/06/2000 Print Div Long Name If Exist Instead of Company NAme
= gfRltFld(CutTktH.cDivision,@laDivLNam,'CDIVISION')
*B803322,1 SSH[END]
llEndGroup = .F.
*-- Get the suitable title due to status to be printed on the .FRX
DO CASE
  CASE CUTTKTH.STATUS='O'
    lcSt = 'OPEN'
  CASE CUTTKTH.STATUS='H'
    lcSt = 'HOLD'
  *khalid
  *CASE CUTTKTH.STATUS='X'
  *  lcSt = 'CANCELLED'
  CASE CUTTKTH.STATUS='X'
    lcSt = 'CANCELLED'
  CASE CUTTKTH.STATUS='S'
    lcSt = 'CLOSED'
  *khalid
  CASE CUTTKTH.STATUS='A'
    lcSt = 'ACTUAL'
  *B802039,1 AMM fill the variable in case of status complete.
  CASE CUTTKTH.STATUS='C'
    lcSt = 'COMPLETED'
  *B802039,1 AMM end
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
*! Name        : lfShiftArr
*! Developer   : AHMED MOHAMMED IBRAHIM (AMM)
*! Date        : 07/15/1998
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
*! Name      : lfEndGroup
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
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
*! Name      : lfSum
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
*! Purpose   : To sum size quantities for each warehouse.
*!*************************************************************
*! Called from : MFPRCTA.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSum()
*!*************************************************************
FUNCTION lfSum
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT CUTTKTL
=SEEK(CutTktH.CutTkt)
laSum = 0
*-- Sum size quantities for the budget lines for each warehouse.
SUM Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty WHILE CUTTKT=CUTTKTH.CUTTKT ;
    FOR cWareCode+TranCD=&lcMainF..cWareCode+'1' TO ARRAY laSum
SELECT (lnAlias)
RETURN ''

*!*************************************************************
*! Name      : lfGetTitle
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
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
  lcCostItm = laCost[ASUBSCRIPT(laCost,ASCAN(laCost,cCatgTyp),1)+5,2]
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
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
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
*! Name      : lfvOMsg
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
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
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
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
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
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
X = .F.
IF X
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LLRPCost'),1)
  laOGObjCnt[lnPos] = llrPrtCs
  = lfOGShowGet('llRpCost')

*C101931,1 ABD Change the Select status way from  inlist to be mover and default Open. [Begin]
ENDIF
DECLARE laRpSource[6] , laRpTarget[1]
STORE 'Open'     TO laRpSource[1]
STORE 'Hold'     TO laRpSource[2]
STORE 'Actual'   TO laRpSource[3]
STORE 'Closed'   TO laRpSource[4]
STORE 'Canceled' TO laRpSource[5]
STORE 'Complete' TO laRpSource[6]
STORE 'Open'     TO laRpTarget[1]
lcStatus = 'O'
*C101931,1 ABD [End] 

*!*************************************************************
*! Name      : lfGetOp
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
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
*E301338,1 AMM set the variables to display on the .FRX
lcStyMaj = CUTTKTH.style
lcPattrn = CUTTKTH.pattern
*E301338,1 AMM end

*!*************************************************************
*! Name      : lfGetTit
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Get the MFG order title.
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : lcTit
*!*************************************************************
*! Example     : = lfGetTit()
*!*************************************************************
FUNCTION lfGetTit

PRIVATE lcTit
lcTit = ALLTRIM(gfGetMemvar('M_PRDLNLBL',gcAct_Comp))
lcTit  = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit+' #')
RETURN lcTit
*!*************************************************************
*! Name      : lfsrStyle
*! Developer : ABDOU ELGENDI        - (ABD)
*! Date      : 08/24/2000
*! Purpose   : Rise change order of style file in range browse screen.
*! Reference : C101931,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrStyle()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrStyle
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    SET ORDER TO Cstyle IN STYLE
    GOTO TOP
  CASE lcParm = 'R'
    SET ORDER TO Style IN STYLE
    GOTO TOP
ENDCASE
*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : ABDOU ELGENDI        - (ABD)
*! Date      : 08/24/2000
*! Purpose   : Evaluate Status expression.
*! Reference : C101931,1
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
*
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

= gfMover(@laRpSource,@laRpTarget,'Select Cutting Ticket Status',.T.,'')  && call mover function.
lcStatus = ' '

*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcStatus = lcStatus + IIF(laRpTarget[lnI] = 'Open'        ,'O',;
                              IIF(laRpTarget[lnI] = 'Hold'    ,'H',;
                              IIF(laRpTarget[lnI] = 'Actual'  ,'A',;
                              IIF(laRpTarget[lnI] = 'Closed'  ,'S',;
                              IIF(laRpTarget[lnI] = 'Canceled','X',;
                              IIF(laRpTarget[lnI] = 'Complete','C',''))))))
  ENDFOR  && end Loop to make Status expression.
ENDIF

*-- if empty of status is like select all available values.
lcStatus = IIF(EMPTY(lcStatus),'OHASXC',ALLTRIM(lcStatus))

*-- End of lfvOStatus.
*!*************************************************************

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Albert Raif (ALB)
*! Date      : 11/24/2002
*! Purpose   : Get Color Length and Non major/free Length
*! Reference : C102730 
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************

FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcNonMajTl = ''
lcNonMajPi = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.
