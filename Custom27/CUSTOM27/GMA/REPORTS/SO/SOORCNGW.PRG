*:***************************************************************************
*: Program file  : SOORCNGW      "FROM SOORCNGW"  C119796
*: Program desc. : Customer Order confirmation Report for GMA (web form)
*: For Report    : (SOORCNGW.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mohamed Shokry Mohamed (MHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO Soorcngw
*:***************************************************************************
*: This Program is due to C119796
*:***************************************************************************
*:Modifications
*:B122707,1 NNA 05/06/2004 Fix Bug that the report not show the Currency Code, So it Fixed in Prg. and FRX.
*:B125318,1 BWA 12/29/2004 Fix Bugs in the report.
*:B126803,1 ASH 03/14/2005 Fix bug 'Type Mismatch'.
*:***************************************************************************
*:           

lcTime     = TIME()                     && Variable to hold the Time
lnLastRec  = 0                          && Record No. Of last record in order group.
lcStore    = ''
lcDivLName = ''
*-- Note Variables [begin]
lcOrdsNum=''
lcTitle    = ''                         && Title of Note. 
lcNotes    = ''                         && Notes.
llPrintBox = .F.

*llNoRec= .T.
*lcPrgName  = lcFormName
*llIsAparel = lfIsApparl(@lcPrgName)
*C101403,1 (End)

llEndGroup = .F.                        && Flag to know if we are at the end of the Group
llPrntBoth = .T.  && Flag to know we print both line notes and notepad.
llAprvFlag = .F.                        && Flag to print approval 
llTitle    = .T.                        && Flag to print Detail header.

*lcTmpIndx = 'ACCOUNT + STORE + CTERMCODE + SHIPVIA + CUSTPO + DTOS(COMPLETE)'

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  lcStyTitle = gfItemMask('HI')        && Title of the style.
  lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.
  lcObj_Id  = ''                       && Object Identification in Objlink file.
  *-- if this company have a logo, put it in temp. file and then use it in .FRX
  IF SEEK('*' + 'LOGO' , 'OBJLINK') AND ;
     SEEK(OBJLINK.cObject_ID,'OBJECTS')
    = lfGetLogo()  && Function to Fill the temp. With company Logo.
  ENDIF
ENDIF
*--E301168 HDM create temp file for ordline file [Start]
lcTempOrd = gfTempName()
*--E301168 HDM create temp file for ordline file [End]
*--- Relation between opened files [begin]
*-- Note that the files was opened in Rep. Gen.
SELECT ORDHDR
SET RELATION TO cordtype+ order INTO Ordline ADDITIVE

*--E301168 HDM create temp file for ordline file [Start]
DIMENSION laTempStru[1]
SELECT ORDLINE

lnFldsLen = AFIELDS(laTempStru)

*B122707,1 NNA (Begin) Increase The Dimension To Add A new field that will Hold The currency Code
*DIMENSION laTempStru[lnFldsLen + 17,4]
DIMENSION laTempStru[lnFldsLen + 18,4]
*B122707,1 NNA (End)

laTempStru[lnFldsLen + 1,1] = "UpcSize1"
laTempStru[lnFldsLen + 1,2] = "C"
laTempStru[lnFldsLen + 1,3] = 7
laTempStru[lnFldsLen + 1,4] = 0
  
laTempStru[lnFldsLen + 2,1] = "UpcSize2"
laTempStru[lnFldsLen + 2,2] = "C"
laTempStru[lnFldsLen + 2,3] = 7
laTempStru[lnFldsLen + 2,4] = 0

laTempStru[lnFldsLen + 3,1] = "UpcSize3"
laTempStru[lnFldsLen + 3,2] = "C"
laTempStru[lnFldsLen + 3,3] = 7
laTempStru[lnFldsLen + 3,4] = 0

laTempStru[lnFldsLen + 4,1] = "UpcSize4"
laTempStru[lnFldsLen + 4,2] = "C"
laTempStru[lnFldsLen + 4,3] = 7
laTempStru[lnFldsLen + 4,4] = 0

laTempStru[lnFldsLen + 5,1] = "UpcSize5"
laTempStru[lnFldsLen + 5,2] = "C"
laTempStru[lnFldsLen + 5,3] = 7
laTempStru[lnFldsLen + 5,4] = 0

laTempStru[lnFldsLen + 6,1] = "UpcSize6"
laTempStru[lnFldsLen + 6,2] = "C"
laTempStru[lnFldsLen + 6,3] = 7
laTempStru[lnFldsLen + 6,4] = 0

laTempStru[lnFldsLen + 7,1] = "UpcSize7"
laTempStru[lnFldsLen + 7,2] = "C"
laTempStru[lnFldsLen + 7,3] = 7
laTempStru[lnFldsLen + 7,4] = 0

laTempStru[lnFldsLen + 8,1] = "UpcSize8"
laTempStru[lnFldsLen + 8,2] = "C"
laTempStru[lnFldsLen + 8,3] = 7
laTempStru[lnFldsLen + 8,4] = 0

laTempStru[lnFldsLen + 9,1] = "SkuSize1"
laTempStru[lnFldsLen + 9,2] = "C"
laTempStru[lnFldsLen + 9,3] = 7
laTempStru[lnFldsLen + 9,4] = 0

laTempStru[lnFldsLen + 10,1] = "SkuSize2"
laTempStru[lnFldsLen + 10,2] = "C"
laTempStru[lnFldsLen + 10,3] = 7
laTempStru[lnFldsLen + 10,4] = 0

laTempStru[lnFldsLen + 11,1] = "SkuSize3"
laTempStru[lnFldsLen + 11,2] = "C"
laTempStru[lnFldsLen + 11,3] = 7
laTempStru[lnFldsLen + 11,4] = 0

laTempStru[lnFldsLen + 12,1] = "SkuSize4"
laTempStru[lnFldsLen + 12,2] = "C"
laTempStru[lnFldsLen + 12,3] = 7
laTempStru[lnFldsLen + 12,4] = 0

laTempStru[lnFldsLen + 13,1] = "SkuSize5"
laTempStru[lnFldsLen + 13,2] = "C"
laTempStru[lnFldsLen + 13,3] = 7
laTempStru[lnFldsLen + 13,4] = 0

laTempStru[lnFldsLen + 14,1] = "SkuSize6"
laTempStru[lnFldsLen + 14,2] = "C"
laTempStru[lnFldsLen + 14,3] = 7
laTempStru[lnFldsLen + 14,4] = 0

laTempStru[lnFldsLen + 15,1] = "SkuSize7"
laTempStru[lnFldsLen + 15,2] = "C"
laTempStru[lnFldsLen + 15,3] = 7
laTempStru[lnFldsLen + 15,4] = 0

laTempStru[lnFldsLen + 16,1] = "SkuSize8"
laTempStru[lnFldsLen + 16,2] = "C"
laTempStru[lnFldsLen + 16,3] = 7
laTempStru[lnFldsLen + 16,4] = 0

laTempStru[lnFldsLen + 17,1] = "Sku"
laTempStru[lnFldsLen + 17,2] = "C"
laTempStru[lnFldsLen + 17,3] = 16
laTempStru[lnFldsLen + 17,4] = 0

*B122707,1 NNA (Begin) A new Field in the Temp file To have the OrdHdr.cCurrCode
laTempStru[lnFldsLen + 18,1] = "CurrCode"
laTempStru[lnFldsLen + 18,2] = "C"
laTempStru[lnFldsLen + 18,3] = 3
laTempStru[lnFldsLen + 18,4] = 0
*B122707,1 NNA (End)

CREATE TABLE (gcWorkDir+lcTempOrd) FROM ARRAY laTempStru

SELECT (lcTempOrd)
INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) TAG (lcTempOrd)
SET ORDER TO TAG (lcTempOrd)

SELECT ORDHDR

lnLenSku = SKUTMPL.Len1

SELECT SKUTMPL

=SEEK('S')
lnLenSku = SKUTMPL.Len1
 
SELECT ORDHDR
IF !EMPTY(lcRpExp)
  lcRpExp = lcRpExp + [ AND ]
ENDIF
IF lcRpOrdSta ="A"

  *B122707,1 NNA 05/06/2004 (Begin) Show the order status [Bid] also
  *lcRpExp = lcRpExp + [!(ORDHDR.Status $ "BX")]
  lcRpExp = lcRpExp + [!(ORDHDR.Status $ "X")]  
  *B122707,1 NNA

ELSE
  lcRpExp = lcRpExp + [(ORDHDR.Status $ lcRpOrdSta)]
ENDIF

*--order type

IF lcRpOrdTyp !="A"
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + [ AND ]
  ENDIF
  lcRpExp = lcRpExp + [(ORDHDR.CordType $ lcRpOrdTyp)]
ENDIF

IF !EMPTY(lcRpAcc)
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + [ AND ]
  ENDIF
  lcRpExp = lcRpExp + [(ORDHDR.ACCOUNT = lcRpAcc)]
ENDIF

*--
SCAN FOR &lcRpExp
  SELE ORDLINE
  SCAN REST WHILE CORDTYPE+ORDER = ORDHDR.CORDTYPE+ORDHDR.ORDER
    SCATT MEMVAR MEMO
	*----------
    STORE '' TO m.SKUSize1 , m.SKUSize2 , m.SKUSize3 , m.SKUSize4 , m.SKUSize5 ,;
	         m.SKUSize6 , m.SKUSize7 , m.SKUSize8 ,m.SKU

    *B122707,1 NNA (Begin) Reset The Memory Variable that Hold The Currency Code (m.CurrCode)
    *STORE '' TO m.UpcSize1 , m.UpcSize2 , m.UpcSize3 , m.UpcSize4 , m.UpcSize5 ,;
	         m.UpcSize6 , m.UpcSize7 , m.UpcSize8 
    STORE '' TO m.UpcSize1 , m.UpcSize2 , m.UpcSize3 , m.UpcSize4 , m.UpcSize5 ,;
	         m.UpcSize6 , m.UpcSize7 , m.UpcSize8 ,m.CurrCode
    *-- get the Currency Code from the order Header To insert it in the Temp File
    m.CurrCode = ORDHDR.cCurrCode
    *B122707,1 NNA (End)

    =SEEK('S' + m.Scale,"SCALE")
    FOR lnCount = 1 TO Scale.Cnt
	    lcCount = STR(lnCount , 1)
        IF SEEK(m.Style + lcCount , "StyleUPC")
          m.UPCSize&lcCount= StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
        ENDIF
    ENDFOR
    
    IF SEEK('S' + OrdLine.Account + m.Style,'SPCK_LIN')
      SELECT SPCK_LIN
	  m.SKU=LEFT(Pack_Id,lnLenSku)
      lnCount = 1
	  SCAN FOR Type + Account + Style + Pack_ID= 'S' + OrdLine.Account + m.Style
		lcCount = STR(lnCount , 1)
	    m.SkuSize&lcCount = AllTrIM(SUBSTR(PACK_ID,lnlensku,Len(Pack_ID)))
    	lnCount = 1 + lnCount
	  ENDSCAN     
    ENDIF    
	
	*----------    
    INSERT INTO (lcTempOrd) FROM MEMVAR
  ENDSCAN
ENDSCAN

GO TOP
SELECT ORDHDR
SET RELATION OFF INTO ORDLINE

SELECT ORDHDR
*SET RELATION TO cordtype+ order INTO Ordline ADDITIVE
SET RELATION TO cordtype+ order INTO (lcTempOrd) ADDITIVE

SET RELATION TO cwarecode INTO Warehous ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
*SELECT ORDLINE
SELECT (lcTempOrd)
SET FILTER TO TotQty != 0
SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO style INTO Style ADDITIVE

SELECT OBJLINK_A
SET RELATION TO cobject_id INTO OBJECTS_A ADDITIVE
*--- Relation between opened files [end]

IF llFrTime
  llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.
  DECLARE laCompAdd[5,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
  laCompAdd = ''                    && Array to hold the Company address
  laSoldTo = ''                     && Array to hold the Sold To address
  laShipTo = ''                     && Array to hold the Ship To address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'

  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK gcAct_Comp
  lcCompName = cCom_Name
  lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
  *----E500320,1 NAD [Begin] Get the company fax no. to print it 
  lcCompFax  = cCom_fax              && Variable to hold the Company Fax
  *----E500320,1 NAD [END]
  lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
  *-- Load Company address.
  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  *= lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  *-- Get company Address [end].
ENDIF

*lcSkipExpr = [Ordline]    && Expression we skip to later.
lcSkipExpr  = [&lcTempOrd]

*-- lcNoteLns : Name of Temp. Loop File which is used to print both line notes 
*--           : and notepad from notepad file.
*--           : note that this name and temp. file is created 
*--           : one for every optional grid seasson run.
lcNoteLns = IIF(EMPTY(lcNoteLns),gfTempName(),lcNoteLns)

*-- if you don't find temp. file, create it if you have both types of notes.
*IF !USED(lcNoteLns) AND llPrntBoth
IF !USED(lcNoteLns) 
  CREATE CURSOR (lcNoteLns)  (cRecord C(2))
  INDEX ON cRecord TAG (lcNoteLns) OF (gcWorkDir+lcNoteLns)
  FOR lnI = 1 TO 2
    APPEND BLANK
   REPLACE cRecord WITH "N"+ALLTRIM(STR(lnI))
  ENDFOR
ENDIF

SELECT (lcTempOrd)
SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
lcSkipExpr = [&lcTempOrd,&lcNoteLns]

*B125318,1 BWA 12/29/2004 Fix the bug of not printing the PO# in the FRX.[START]
SET RELATION TO "2" + ORDER + STR(LINENO,6) INTO CUTPICK ADDITIVE
*B125318,1 BWA 12/29/2004.[END]

*-- if we are in case of print both types of notes.  [begin]

*-- Select Master report file.
SELECT ORDHDR
SET SKIP TO &lcSkipExpr

DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp


SELECT ORDHDR

SET RELATION TO

IF USED(lcTempOrd)
  SELECT(lcTempOrd)
  USE
  ERASE (gcWorkDir+lcTempOrd+'.DBF')
  ERASE (gcWorkDir+lcTempOrd+'.CDX')
ENDIF
*-- end of Report code.

*!*************************************************************
*! Name      : lfsChOrder
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Rise change order flag, in range browse screen.
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
*! Example   : =lfsChOrder()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
*
FUNCTION lfsChOrder
PARAMETERS lcParm
DO CASE 
  CASE lcParm = 'S'
    SELECT ORDHDR
    DO CASE
      *--FIX ALL AND CHANGE lcRpOrdTyp
      CASE lcRpOrdTyp = "A"
         SET FILTER TO IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
           STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
           ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) 

         LOCATE
         
      CASE lcRpOrdTyp = "O" 
         SET FILTER TO (CORDTYPE + ORDER = "O") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
            STATUS = lcRpOrdSta) 
         
            LOCATE FOR CORDTYPE+ORDER = "O"

      CASE lcRpOrdTyp = "C"
         SET FILTER TO (CORDTYPE + ORDER = "C") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
            STATUS = lcRpOrdSta) 
         
         LOCATE FOR CORDTYPE+ORDER = "C"

      CASE lcRpOrdTyp = "T" 
         SET FILTER TO (CORDTYPE + ORDER = "T") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
         STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
         ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) 
         LOCATE FOR CORDTYPE+ORDER = "T"

    ENDCASE                  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET FILTER TO
    llClearSel = .F.
ENDCASE
*-- end of lfsChOrder.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
lcOldVal = EVALUATE(SYS(18))

*-- end of lfwOldVal.
*!*************************************************************
*! Name      : lfvType
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Transaction Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvType()
*!*************************************************************
*
FUNCTION lfvType

IF lcRpOrdTyp $ "TC"
  lcRpEDIFlt = "B"
  CLEAR READ 
ELSE
  *B126803,1 ASH 03/14/2005 (Begin) Fix bug 'Type Mismatch'.
  *IF (laOldVal = 3) .OR. (laOldVal = 2)
  IF (lcOldVal = 3) .OR. (lcOldVal = 2)
  *B126803,1 ASH 03/14/2005 (End)
    lcRpEDIFlt = "B"
    CLEAR READ 
  ENDIF  
ENDIF
llClearSel = .T.
*-- end of lfvType.

*!*************************************************************
*! Name      : lfvOrdSta
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Order Status Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvOrdSta()
*!*************************************************************

FUNCTION lfvOrdSta

llClearSel = .T.

*-- end of lfvOrdSta.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm

SELECT Customer
SET ORDER TO Customer
LOCATE
*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfGetLogo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Function to Save the company logo in temp. file 
*!             which is used after this to print the logo for company.
*!*************************************************************
*! Called from : SORDCON.PRG
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLogo()
*!*************************************************************
FUNCTION lfGetLogo
llLogo = .T.
lcLogoPic = gfTempName()
lcObj_Id = OBJLINK.cObject_ID
*-- Select general field which have company logo.
SELECT gobject;
 FROM Objects         ;
 WHERE Objects.cobject_id = lcObj_Id ;
 INTO CURSOR (lcLogoPic)
*-- end of lfGetLogo.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : SORDCON.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.
*!*************************************************************
*! Name      : lfAprvFlag
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Function that control appearance of 
*!           : Approval objects in SORDCONA.FRX
*!*************************************************************
*! Called from : [Option Grid] "Approval Object" in Header Band.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Logical
*!*************************************************************
*! Example     : = lfAprvFlag()
*!*************************************************************
FUNCTION lfAprvFlag
llAprvFlag = !EMPTY(OrdHdr.Approval)
RETURN llAprvFlag
*-- end of lfAprvFlag.

*!*************************************************************
*! Name        : lfGetNotes
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : SORDCONA.FRX [Variable lcDum in the report]
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : lfBoxPrn,lfNoteHead,lfNoteData
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes

lcNotes    = lfNoteData()     && Note Data.

IF !EMPTY(lcNotes)
  lcTitle    = lfNoteHead()     && Title of the note (Line Note OR NotePad).
ELSE
  lcTitle    =""
ENDIF

llPrintBox = !EMPTY(lcTitle)  && If it's .T. Report Print box around notes.
llTitle    = RECNO(lcTempOrd) != lnLastRec
RETURN ''
*-- end of lfGetNotes.

*!*************************************************************
*! Name        : lfNoteHead
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose     : Function to fill the approparate Note Title.
*!             : ("Line Notes" OR "Order NotePad" OR "Contract NotePad") .
*!*************************************************************
*! Called from : lfGetNotes Function.
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfNoteHead()
*!*************************************************************
FUNCTION lfNoteHead
lcNoteHead = ''
*-- If you have order lines.
IF ORDHDR.LastLine > 0
  *-- if you print both notes.
  IF llPrntBoth
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE 
       CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      
        lcNoteHead = 'Line ' + ALLTRIM(STR(&lcTempOrd..LineNo)) + ' Notepad'
        
      CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))
           
      
        
        *lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType = 'O','Order Notepad','Contract Notepad'),'')
        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType='O','Order Notepad',IIF(OrdHdr.cOrdType='T','EDI Temporary Order Notepad','Contract Notepad')),'')

    ENDCASE
  ELSE && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE
      CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      
        lcNoteHead = 'Line ' + ALLTRIM(STR(&lcTempOrd..LineNo)) + ' Notepad'


      CASE llRpOrdNot AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

        
        *lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType = 'O','Order Notepad','Contract Notepad'),'')

        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType='O','Order Notepad',IIF(OrdHdr.cOrdType='T','EDI Temporary Order Notepad','Contract Notepad')),'')
    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteHead
*-- end of lfNoteHead

*!*************************************************************
*! Name        : lfNoteData
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 20/10/2003
*! Purpose     : Function to fill the approparate Note Data Field in report.
*!*************************************************************
*! Called from : lfGetNotes Function.
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfNoteData()
*!*************************************************************
FUNCTION lfNoteData
lcNoteData  = ''
lcPrntNote = ''
*-- If you have order lines.
IF ORDHDR.LastLine > 0
  *-- if you print both notes.
  IF llPrntBoth
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE
      CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))

        For lnNotLine = 1 To MEMLINES(&lcTempOrd..Note_Mem)
          lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = lcPrntNote


      CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))
        For lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
    ENDCASE
  ELSE  && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE

      CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))

        lcNoteData  =  ALLTRIM(&lcTempOrd..Note_Mem)
        For lnNotLine = 1 To MEMLINES(&lcTempOrd..Note_Mem)
          lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = lcPrntNote
      CASE llRpOrdNot AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

        For lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteData
*-- end of lfNoteData.
*!*************************************************************
*! Name        : lfHeadVar
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose     : Function to fill the approparate data for report header.
*!*************************************************************
*! Called from : SORDCONA.FRX [Header Band]
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : lfSolSpAdr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfHeadVar()
*!*************************************************************
FUNCTION lfHeadVar
lcAlias = ALIAS()   && Save Current alias.
llEndGroup = .F.    && Start of new Group.
= lfSolSpAdr()      && Call Function that fill header data [SoldTo and ShipTo]
SELECT (lcAlias)    && Restore before function alias.
RETURN ''
*-- end of lfHeadVar.

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Function to Get the Sold to Address, Ship to Address,
*!           : the Description of the Ship Via, Season,
*!           : Special Instructions, and Terms.
*!*************************************************************
*! Called from : lfHeadVar Function
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : gfRltFld, gfCodDes, gfGetAdr, lfAdrShift.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
FUNCTION lfSolSpAdr

lnSavAlias = SELECT(0)

lcStore = &lcTempOrd..Store

= gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcShipVia = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
lcSeason  = gfCodDes(ORDHDR.Season  , 'SEASON'   )
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')

SELECT CUSTOMER
IF ORDHDR.MULTI = 'Y'
  *= SEEK('S' + OrdLine.Account + OrdLine.Store , "CUSTOMER")
  = SEEK('S' + &lcTempOrd..Account + &lcTempOrd..Store , "CUSTOMER")
  
  IF ALLTRIM(ORDHDR.ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ENDIF  
ENDIF

lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

= lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

*-- IF alternate ship to address
IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else

  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcStore = lcStore + '  Dist. Center : ' + Customer.Store
  ENDIF

  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

  IF TYPE('lcCurrKey') = 'C'
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF

ENDIF    && End of IF
= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
SELECT (lnSavAlias)
*-- end of lfSolSpAdr.


*!*************************************************************
*! Name      : lfLastRec
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Calculate last Record in order details.
*!*************************************************************
*! Called from : [SORDCONA.FRX, ORDER GROUP HEADER BAND] 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : NULL
*!*************************************************************
*! Example     : = lfLastRec()
*!*************************************************************
FUNCTION lfLastRec
PRIVATE lcThAlias,lnThRec,lcThStore
lcThAlias = ALIAS()           && Save Current Alias.
SELECT (lcTempOrd)

lnThRec = RECNO(lcTempOrd)    && Save Current record #.
lcThStore = Store
LOCATE REST FOR ( cordtype+order+store+style+STR(lineno,6) > OrdHdr.cordtype + OrdHdr.order + lcThStore)
IF (order != OrdHdr.order) OR (store != lcThStore)
  SKIP -1
ENDIF   

lnLastRec = RECNO(lcTempOrd)

IF BETWEEN(lnThRec,1,RECCOUNT(lcTempOrd))
  GO lnThRec IN (lcTempOrd)    && Restore Record #
ELSE
  GO TOP IN (lcTempOrd)    && Restore Record #
ENDIF

IF lnLastRec <= 0
  lcOrdsNum = ''
ENDIF


SELECT (lcThAlias)            && Restore Alias.
RETURN ''
*-- end of lfLastRec.
*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Function to Update the End of Group flag 
*!*************************************************************
*! Called from : SORDCONA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup
llEndGroup = .T.   && We are in the end of the group (i.e : Order end.)
llTitle    = .T.
RETURN '    '
*-- end of lfEndGroup.

*!**************************************************************************
*! Name      : lfvAccount
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Validation function for the Customer Account field
*!**************************************************************************
*! Called from : Customer Account field [Option Grid]
*!**************************************************************************
*! Calls       : CusBrowM()
*!**************************************************************************
*! Example     : =lfvAccount()
*!**************************************************************************
*
FUNCTION lfvAccount
PRIVATE lcObjVal
lcObjVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

*-- Avoid recurresion clear read.
IF !(lcObjVal == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER
  
  lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field
  *IF The user want to Browse or if the Account he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
    llObjRet = CusBrowM(@lcObjVal , '' , 'M')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
    &lcObjName = lcObjVal
  ENDIF    && End of IF

*-- If the user changed the stored account (old value)   
  
  SET ORDER TO &lcCustOrd
  SELECT(lnAlsNo)

ENDIF  
*-- End of lfvAcct.

*!**************************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/10/2003
*! Purpose   : Save any Get Object old value
*!**************************************************************************
*! Example   : =lfwOldVal()
*!**************************************************************************
*!
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))
*-- End of lfwOldVal.
