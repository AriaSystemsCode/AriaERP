*:***************************************************************************
*: Program file  : MFOPMG
*: Program desc. : Operations Management Report
*: For Report    : MFOPMGA.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Modules       : MF,PO,MA
*: Developer     : AHMED MOHAMMED MOHAMMED
*: Date          : 07/28/98
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetCodes, lfShiftArr, lfwOGWhen, lfwOldVal, lfvCont, 
*:                 lfGetTit, lfsrvTrans
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFOPMG
*:***************************************************************************
*: Mod.    :
*:B602304,1 AMM 12/13/1998 Print the operation data fron the MFGOPHD not from 
*:B602304,1 AMM            the codes file.
*:B802150,1 AMM 04/29/1999 1- Adjust the date filter 
*:B802150,1 AMM            2- Adjust the Ship to address in case of printing PO
*:B802635,1 AMM 10/25/1999 Print Style Dyelot if any
*:B802793,1 AMM 11/17/1999 Print cost if the user has cost previlage only
*:B605721,1 KHM 03/21/2002 Fix the bug of wrong price in the FRX.
*:B125607,1 BWA 01/04/2005 Fix the bug of printing duplicate vendor address.
*:B129132,1 EIH 09/15/2005 Fix the bug when preview multiple cut tickets which have multiple vendors The vendor address
*:B129132,1 EIH 09/15/2005 Is wrong (the same address of first cut tickets).
*:***************************************************************************

DIMENSION laFrom[5], laTo[5], laCompAdd[6], laFOprD[7], laTOprD[7]
DIMENSION laFromOpr[7,2], laToOpr[7,2]
STORE SPACE(0) TO laFrom,laTo,laCompAdd,lcFDesc,lcTDesc,lcFName,lcTName,;
                  lcOpr, lcStyTitle, laOldVal

*B125607,1 BWA 01/04/2005 Add new variable hold the Contcode.[START]
STORE SPACE(0) TO lcContCode
*B125607,1 BWA 01/04/2005.[END]

*B129132,1 EIH 09/15/2005 Add new variable hold the cut ticket number.[Begin]
STORE SPACE(0) TO lcCutTikNo
*B129132,1 EIH 09/15/2005 [END]



llLogo = .F.
* Two arrays to get operation data from the codes file
laFromOpr[1,1] = 'GLACCOUNT'
laFromOpr[1,2] = 'laFOprD[1]'
laFromOpr[2,1] = 'COPERSEQ'
laFromOpr[2,2] = 'laFOprD[2]'
laFromOpr[3,1] = 'LINHOUSE'
laFromOpr[3,2] = 'laFOprD[3]'
laFromOpr[4,1] = 'CCONTCODE'
laFromOpr[4,2] = 'laFOprD[4]'
laFromOpr[5,1] = 'CCONTNAME'
laFromOpr[5,2] = 'laFOprD[5]'
laFromOpr[6,1] = 'LMFGOPR'
laFromOpr[6,2] = 'laFOprD[6]'
laFromOpr[7,1] = 'LEADTIME'
laFromOpr[7,2] = 'laFOprD[7]'

*B602304,1 AMM start, Comment out not used any more.
*laToOpr[1,1] = 'GLACCOUNT'
*laToOpr[1,2] = 'laTOprD[1]'
*laToOpr[2,1] = 'COPERSEQ'
*laToOpr[2,2] = 'laTOprD[2]'
*laToOpr[3,1] = 'LINHOUSE'
*laToOpr[3,2] = 'laTOprD[3]'
*laToOpr[4,1] = 'CCONTCODE'
*laToOpr[4,2] = 'laTOprD[4]'
*laToOpr[5,1] = 'CCONTNAME'
*laToOpr[5,2] = 'laTOprD[5]'
*laToOpr[6,1] = 'LMFGOPR'
*laToOpr[6,2] = 'laTOprD[6]'
*laToOpr[7,1] = 'LEADTIME'
*laToOpr[7,2] = 'laTOprD[7]'
*B602304,1 AMM end

lcTime   = TIME()                       && Variable to hold the Time
lcLogoPic = gfTempName()
*B602304,1 AMM Open MFGOPRHD again with that alias to get the target operation data from
lcTrgtOp  = gfTempName()
lcTOpr    = SPACE(0)
*B602304,1 AMM end

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

lcStyTitle = gfItemMask('HI')
lcMaj      = gfItemMask('PM')             && Get the major of the style
lnMajSize  = LEN(lcMaj)                   && Length of the major
lcMainF    = gfTempName()
*B802793,1 AMM Get the cost previlage of the user
llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
*B802793,1 AMM end
SELECT OBJLINK
SET RELATION TO Objlink.cobject_id INTO Objects ADDITIVE

DO CASE
  CASE lcImTyp = 'M'
    IF !USED('CUTTKTH')
      =gfOpenFile(gcDataDir+'CUTTKTH','CUTTKTH','SH')
    ELSE
      SELECT CUTTKTH
      SET ORDER TO CUTTKTH
    ENDIF
  CASE lcImTyp = 'I'
    IF !USED('POSHDR')
      =gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
    ELSE
      SELECT POSHDR
      SET ORDER TO POSHDR
    ENDIF
  CASE lcImTyp = 'T'
    IF !USED('MMFGORDH')
      SELECT 0
      USE (gcDataDir+'MMFGORDH') ORDER TAG MMFGORDH
    ELSE
      SELECT MMFGORDH
      SET ORDER TO MMFGORDH
    ENDIF
ENDCASE

SELECT STYLE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

IF SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')
  llLogo = .T.
  lcObj_Id = OBJLINK.cObject_ID
  *-- Make cursor contain one field and one record holding the company logo
  SELECT gobject;
   FROM Objects         ;
   WHERE Objects.cobject_id = lcObj_Id ;
   INTO CURSOR (lcLogoPic)
ENDIF

* Create the temporary file
SELECT MFGOPRDT
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cTarget'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0


CREATE TABLE (gcWorkDir+lcMainF) FROM ARRAY laFileStru
*INDEX ON cTktNo+cOprCode+cTarget+cLotNo+TranCd+cTrgOpr+cTrgLot TAG (lcMainF)
INDEX ON cTktNo+cOprCode+cTarget+cLotNo+Item+TranCd+cTrgOpr+cTrgLot TAG (lcMainF)

* Add the filter of status
IF lcStatus <> "L"
   lcRpExp = '(' +lcRpExp+ ") .AND. ("+lcFile+".Status = '"+lcStatus+"')"
ENDIF

SELECT MFGOPRHD
DO CASE
  CASE lcImTyp = 'M' .OR. lcImTyp = 'T'
    SET RELATION TO cTktNo INTO (lcFile) ADDITIVE
  CASE lcImTyp = 'I'
    SET RELATION TO 'P'+cTktNo INTO (lcFile) ADDITIVE
ENDCASE
*B802150,1 AMM Set relation to let the date filter works.
SET RELATION TO cImTyp+cTktNo+cOprCode INTO MFGOPRDT ADDITIVE
*B802150,1 AMM end

* Fill the temporary file with required data
SCAN FOR &lcRpExp
  lcTrgOpr = SPACE(6)
  *-- Get next operation title
  lnRecNo = RECNO()
  * Get next operation sequence number =(current Operation Seq+1)
  lcOprSeq  = PADL(ALLTRIM(STR(VAL(cOperSeq)+1)),2,'0')
  =SEEK(lcImTyp+MFGOPRHD.cTktNo)
  *B602304,1 AMM Adjust the locate to fit if the user put a leading zero
  *LOCATE REST FOR cOperSeq=lcOprSeq
  LOCATE REST FOR PADL(ALLTRIM(cOperSeq),2,'0') = lcOprSeq
  *B602304,1 AMM end
  IF FOUND()
    lcTrgOpr = cOprCode
  ENDIF
  GOTO (lnRecNo)
  
  IF SEEK(lcImTyp+cTktNo+cOprCode,'MFGOPRDT')
    SELECT MFGOPRDT
    SCAN WHILE cImTyp+cTktNo+cOprCode= lcImTyp+MFGOPRHD.cTktNo+MFGOPRHD.cOprCode ;
         FOR TranCd='1' .and. IIF(EMPTY(lcRpLot),.T.,cLotNo=lcRpLot)
      m.cTarget = lcTrgOpr
      SCATTER MEMVAR MEMO
      INSERT INTO (lcMainF) FROM MEMVAR
    ENDSCAN
  ENDIF

ENDSCAN
SET RELATION TO
*B602304,1 AMM Use the file again to get the target operation data from.
USE (gcDataDir+'MFGOPRHD') AGAIN ALIAS (lcTrgtOp) IN 0 ORDER TAG MFGOPRHD
*B602304,1 AMM end

SELECT (lcMainF)
GO TOP
SET RELATION TO cTktNo INTO (lcFile) ADDITIVE 
SET RELATION TO Item INTO STYLE ADDITIVE
SET RELATION TO cimtyp+cTktNo+cOprCode INTO MFGOPRHD ADDITIVE
* Make this relation to get the unit cost of each element.
*B802793,1 AMM Adjust the relation
*SET RELATION TO cimtyp+ctktno+SPACE(6)+'1'+Item+Color+SPACE(25)+cOprCode INTO BOMLINE ADDITIVE
SET RELATION TO cimtyp+ctktno+SPACE(6)+'1'+Item+Color+cOprCode INTO BOMLINE ADDITIVE
*B802793,1 AMM end
SET RELATION TO 'S'+LEFT(Item,lnMajSize) INTO Objlink ADDITIVE
*B602304,1 AMM Set relation to get target operation data
SET RELATION TO cimtyp+cTktNo+cTarget INTO (lcTrgtOp) ADDITIVE
*B602304,1 AMM end

* Display the report
DO gfDispRe WITH (lcFormName)


SELECT (lcMainF)
SET RELATION TO
USE 
ERASE (gcWorkDir+lcMainF+'.DBF')
ERASE (gcWorkDir+lcMainF+'.CDX')
IF USED(lcTrgtOp)
  USE IN (lcTrgtOp)
ENDIF


*!*************************************************************
*! Name      : lfGetCodes
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Called from : MFOPMGA.FRX
*!*************************************************************
*! Calls       : gfRltFld(), gfGetAdr(), lfShiftArr()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetCodes()
*!*************************************************************
FUNCTION lfGetCodes
* If the operation changed, get the new codes.

*B125607,1 BWA 01/04/2005 Fix the bug of printing duplicate vendor address.[START]
*IF lcOpr # &lcMainF..cOprCode

*B129132,1 EIH 09/15/2005 Add check for the cut ticket number .[Begin]
*IF (lcOpr # &lcMainF..cOprCode) .OR. (lcContCode # &lcMainF..cContCode)
IF (lcOpr # &lcMainF..cOprCode) .OR. (lcContCode # &lcMainF..cContCode) .OR.  (lcCutTikNo # &lcMainF..ctktno)
  lcCutTikNo = &lcMainF..ctktno
  *B129132,1 EIH 09/15/2005 [END]
  
  lcContCode = &lcMainF..cContCode
  *B125607,1 BWA 01/04/2005.[END]

  lcOpr = &lcMainF..cOprCode
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  * Get the main operation name and addresses
  lcFDesc   = gfCodDes(&lcMainF..cOprCode, 'MFGCODE')
  =gfRltFld(&lcMainF..cOprCode , @laFromOpr, 'MFGCODE')
  *B602304,1 AMM Don't get operation data from the codes file, get it from 
  *B602304,1 AMM MFGOPRHD instead
  *IF !EMPTY(&lcMainF..cTarget)
  *  lcTDesc   = gfCodDes(&lcMainF..cTarget, 'MFGCODE')
  *  =gfRltFld(&lcMainF..cTarget , @laToOpr, 'MFGCODE')
  *ELSE
  *  STORE SPACE(0) TO lcTDesc, laTOprD
  *ENDIF 
  IF !EMPTY(&lcMainF..cTarget) .AND. !EOF(lcTrgtOp)
    lcTDesc = gfCodDes(&lcMainF..cTarget, 'MFGCODE')
    lcTOpr  = EVAL(lcTrgtOp+'.cContCode')
    lcTName = EVAL(lcTrgtOp+'.cContName')
  ELSE
    STORE SPACE(0) TO lcTDesc, lcTOpr, lcTName
  ENDIF
  *B602304,1 AMM end

  *-- Search the contractor code in the vendor file
  *B602304,1 AMM Get the vendor data if the operation is InHouse = 'NO'
  *IF SEEK(ALLTRIM(MFGOPRHD.cContCode),'APVENDOR')
  IF !MFGOPRHD.lInHouse .AND. SEEK(ALLTRIM(MFGOPRHD.cContCode),'APVENDOR')  
  *B602304,1 AMM end
    lcFName   = APVENDOR.CVenComp
    * Get the vendor addresses
    laFrom[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
    laFrom[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
    laFrom[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
    laFrom[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
    laFrom[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
    DO lfShiftArr WITH laFrom
  ELSE
    STORE SPACE(0) TO laFrom,lcFName
    *B602304,1 AMM Get the contractor name entered by cost sheet program
    lcFName  = MFGOPRHD.cContName
    *B602304,1 AMM end
  ENDIF
  
  *--If this is last operation, get the warehouse addresses
  *B602304,1 AMM start, that array not used any more, use this variable instead
  *IF EMPTY(laTOprD[4])
  IF EMPTY(lcTOpr)
  *B602304,1 AMM end
    *B802150,1 AMM Adjust the seek exp. in case of seeking in POSHDR file
    *IF SEEK(&lcMainF..CTKTNo,lcFile) .AND. SEEK(&lcFile..cWareCode,'WareHous')
    IF SEEK(IIF(lcImTyp='I','P','')+&lcMainF..CTKTNo,lcFile) .AND. SEEK(&lcFile..cWareCode,'WareHous')
    *B802150,1 AMM end
      lcTDesc = '** None **'
      *B602304,1 AMM start, That array not used any more, use this variable instead
      *laTOprD[4] = WareHous.cWareCode
      lcTOpr  = WareHous.cWareCode
      *B602304,1 AMM end
      lcTName = Warehous.cDesc
      laTo[1] = gfGetAdr('Warehous' , '' , '' , '' , 1)
      laTo[2] = gfGetAdr('Warehous' , '' , '' , '' , 2)
      laTo[3] = gfGetAdr('Warehous' , '' , '' , '' , 3)
      laTo[4] = gfGetAdr('Warehous' , '' , '' , '' , 4)
      laTo[5] = gfGetAdr('Warehous' , '' , '' , '' , 5)
      DO lfShiftArr WITH laTo
    ENDIF
  ELSE
    *B602304,1 AMM Get the vendor data from the vendor file in case of 
    *B602304,1 AMM InHouse=No only.
    *IF SEEK(ALLTRIM(laTOprD[4]),'APVENDOR')
    IF !&lcTrgtOp..lInHouse .AND. SEEK(ALLTRIM(lcTOpr),'APVENDOR')
    *B602304,1 AMM end
      lcTName   = APVENDOR.CVenComp
      * Get the vendor addresses
      laTo[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
      laTo[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
      laTo[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
      laTo[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
      laTo[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
      DO lfShiftArr WITH laTo
    ELSE
      STORE SPACE(0) TO laTo,lcTName
      *B602304,1 AMM If InHouse = Yes, get the contractor name entered by 
      *B602304,1 AMM Cost sheet program
      lcTName  = MFGOPRHD.cContName
      *B602304,1 AMM end
    ENDIF
  ENDIF
  SELECT (lnAlias)
ENDIF
RETURN ''

*!*************************************************************
*! Name      : lfShiftArr
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Collapse thhe elements of the array
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfShiftArr()
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
*! Name      : lfwOGWhen
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : When function of the option grid
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : gfRltFld(), gfCodDes(), gfGetAdr(), lfShiftArr()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen
* check the current module to specify the tables which will be used.
DO CASE
  CASE GCACT_APPL='MF'
    lcImTyp = 'M'
    lcFile  = 'CUTTKTH'
  CASE GCACT_APPL='PO'
    lcImTyp = 'I'
    lcFile  = 'POSHDR'
  CASE GCACT_APPL='MA'
    lcImTyp = 'T'
    lcFile  = 'MMFGORDH'
ENDCASE

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Store the old value.
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvCont
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Valid function of the contractor setting in the option grid
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : gfApVnBrow()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCont()
*!*************************************************************
FUNCTION lfvCont

PRIVATE lcVar, lcObj
lnAlias = SELECT(0)
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(  ( EVALUATE(SYS(18)) )  )   && Varible to hold the current field value
SELECT APVENDOR
SET ORDER TO TAG VenCode 
*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj )
  IF !SEEK(lcObj , 'APVENDOR'))
    =gfApVnBrow(@lcObj)
    IF !EMPTY(lcObj)
      &lcVar = lcObj      && Update the field
    ELSE
      &lcVar = laOldVal
    ENDIF
  ENDIF
ELSE
  IF !SEEK(lcObj , 'APVENDOR'))
    *WAIT WINDOW "This contractor doesn't exist in the vendor file !!" 
    =gfModalGen('TRM00001B00000','DIALOG','This contractor')
  ENDIF
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGetTit
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Get title to be displayed (PO or cuttkt or MFG order)
*!*************************************************************
*! Called from : The option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : lcTit
*!*************************************************************
*! Example     : = lfGetTit()
*!*************************************************************
FUNCTION lfGetTit

PRIVATE lcTit
DO CASE
  CASE GCACT_APPL='PO'
    lcTit = 'Purchase Order'
  CASE GCACT_APPL='MA'
    lcTit = 'MFG Order'
  CASE GCACT_APPL='MF'
    lcTit = ALLTRIM(gfGetMemvar('M_PRDLNLBL',gcAct_Comp))  
ENDCASE
lcTit  = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit+'#')
RETURN lcTit

*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvTrans()
*!*************************************************************
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
ENDCASE
