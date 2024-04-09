*:**************************************************************************
*: Program file  : ARMATI
*: Program desc. : Material Invoice Form.
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 06/11/2002
*: Reference     : N000390
*:**************************************************************************
*: Functions     : lfOptMsg , lfwOldVal , lfvAccount , lfvInvNo , lfAdrShift
*:                 lfSolSpAdr , lfEndGroup , lfInvSet , lfCalcTax , lfEndLine
*:                 lfEndGrRtn , lfGetPath , lfGetFrx , lfGetSign , lfPrtLNotes
*:                 lfPrtINotes
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Modifications:
*:B606791,1 SSE 12/18/2002 Fix bug of running the report if MA module is installed only.
*:E126684,1 TMI 03/03/2005 Remove zeros in the right of the decimal point.
*:**************************************************************************
*-- lcTypInv  && Variable that is hold the name of the optinal message memo file.
*-- lcTypInv = 'InvMessg'

IF EMPTY(lcRpMsg1) .AND. EMPTY(lcRpMsg2) .AND. EMPTY(lcRpMsg3)
  IF FILE(gcDataDir+lcTypInv+'.MEM')
    RESTORE FROM gcDataDir+lcTypInv+'.MEM' ADDITIVE
  ENDIF
ENDIF

llMulCurr = gfGetMemVar('llMulCurr',gcAct_Comp)
lcCurrPost = "LEFT "                              && Default Value.
IF llMulCurr
  IF !USED('SYCINT')
    =gfOpenFile(gcsyshome + "SYCINT" , "CCONTCODE" , 'SH')
  ENDIF
  =SEEK(SycComp.cCont_Code,'SYCINT')
  lcCurrPost = SycInt.cCurrency
  IF !USED('SYCCURR')
    =gfOpenFile(gcsyshome + "SYCCURR" , "CCURRCODE" , 'SH')
  ENDIF
ENDIF

llarpinv = .T.            && llarpinv it will be a commen variable.

llPrntInst = TYPE('lcPrntInst') = 'C' .AND. lcPrntInst='Y'
lcTime = TIME()          && Variable to hold the Time
llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
             'OBJECTS') , .T. , .F.)        && Flag to know if we are to print the Company Logo
STORE 0 TO TAXABLE , NTAXABLE , lnTaxable , lnLines
STORE '' TO lcStrToPrn
STORE .F. TO llPrtSku , llLineFlag , llInvFlag
STORE '' TO lcNotes , lcNotesTtl ,  lcLNoteTtl , lcLNotes ,lcINoteTtl , lcINotes
STORE .F. TO llLPrtNote , llIPrtNote

lcDCCode = ''

lcPrgName  = lcFormName
llIsAparel = lfIsApparl(@lcPrgName)
lcSolTName = ''        && Variable to hold the Sold to name
lcShpTName = ''        && Variable to hold the Ship to name
lcShipVia = ''         && Variable to hold the Ship Via Description
lcTerms = ''           && Variable to hold the Terms Description
lcSpkLin = ''          && Variable to hold the Size # of the SKU
llEndGroup = .F.       && Flag to know if we are at the end of the Group
lcDivLName = ''        && Variable to hold the Division long name

lcFacName  = ''
lcTmpDbt = gfTempName()

lcInvPrtUp = gfTempName()
CREATE CURSOR (lcInvPrtUp) (cMatInv C(6))
INDEX ON cMatInv TAG cMatInv of (gcWorkDir+lcInvPrtUp)

DECLARE laCompAdd[6,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2], laFactor[5,1]
laFactor = ''

laCompAdd = ''          && Array to hold the Company address
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

=gfOpenFile(gcsyshome+'SYCCOMP',gcsyshome+'CCOMP_id','SH')
=gfOpenFile(gcsyshome+'SYCFACT',gcsyshome+'cfaccode','SH')
=gfOpenFile(gcDataDir+"MInvLine","MInvLine",'SH', @LCInvLine_A, .T.)

llTax      = (gfGetMemVar('M_TAX') = 'Y')
lcTaxDesc = gfGetMemVar('M_TAX_DESC')
lcTaxMeth = gfGetMemVar('M_TAX_METH')
lcDunsNo  = gfGetMemVar('XDUNS')

SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name             && Variable to hold the Company Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
lcCompFax = cCom_Fax               && Variable to hold the Company Fax
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6] = TRANSFORM(lcCompPhon , lcPhonPict)
lcCompFax = TRANSFORM(lcCompFax , lcPhonPict)  && Fax No. Pic
=lfAdrShift('laCompAdd')

SET ORDER TO TAG MInvLine IN MInvLine
CREATE CURSOR (lcTmpDbt) (CFILE_NUM C(1))
IF llPrntInst .OR. llRpInvNot 
  IF !llIsAparel
    INSERT INTO   (lcTmpDbt) VALUES('1')
    IF llRpInvNot
      INSERT INTO (lcTmpDbt) VALUES('2')
    ENDIF
    IF llPrntInst
      INSERT INTO (lcTmpDbt) VALUES('3')
    ENDIF
    SELECT (lcTmpDbt)
    INDEX ON CFILE_NUM TAG CFILE_NUM of (lcTmpDbt)

    SELECT MInvHdr
    SET RELATION TO '' INTO (lcTmpDbt)
    SELECT (lcTmpDbt)
    SET RELATION TO IIF(CFILE_NUM = '3', MInvHdr.cMatInv, '*') INTO ARMInstD
    SET RELATION TO IIF(CFILE_NUM = '1', MInvHdr.cMatInv, '*') INTO MInvLine ADDITIVE
  ELSE
    SELECT MInvHdr
    SET RELATION TO MInvHdr.cMatInv INTO MInvLine ADDITIVE
  ENDIF    
ELSE
  SELECT MInvHdr
  SET RELATION TO MInvHdr.cMatInv INTO MInvLine ADDITIVE
ENDIF

SELECT MInvLine
SET ORDER TO TAG Fabric IN Fabric
SET RELATION TO IIF(!EMPTY(MInvLine.AltFabric) , MInvLine.AltFabric + MInvLine.AltColor,MInvLine.Fabric) INTO Fabric

SET ORDER TO TAG CUSTOMER IN CUSTOMER
SELECT MInvHdr

SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account,'S' + Account + Store) INTO CUSTOMER ADDITIVE
IF !llIsAparel
  SET SKIP TO MInvLine
ENDIF
SELECT MInvHdr
SET RELATION TO 'O' + MInvHdr.cMorder INTO MaSoHdr ADDITIVE

IF llPrntInst .OR. llRpInvNot
  IF !llIsAparel
    SET SKIP TO (lcTmpDbt) , MInvLine , ARMInstD
  ENDIF
ELSE
  IF !llIsAparel
    SET SKIP TO MInvLine
  ENDIF
ENDIF

lcRpExp = lcRpExp + IIF (!EMPTY(lcRpExp) .AND. lcFactrInv <> 'B' ,;
          ' .AND. ' , '') + IIF(lcFactrInv = 'F' , '!EMPTY(MInvHdr.cFacCode)' ,;
          IIF(lcFactrInv = 'N' , 'EMPTY(MInvHdr.cFacCode)' , ''))

lcRpExp = lcRpExp + " AND MInvHdr.STATUS <> 'V'"

lcPrnComp = IIF(TYPE('llPrntComp') = 'L' , IIF(llPrntComp , 'Y' , 'N') , 'Y')
llPrntComp = lcPrnComp  = "Y"
llPrnFact  = IIF(TYPE('llFactor ') = 'L', llFactor, .F.)

llLineNote = llRpInvNot
llNotePad  = llRpInvLNt
SELECT MInvHdr

lnFrmPos=AT("ARMATI",lcFormName)+6 
IF !lfGetPath()
  *-- Message "This form does not exist.Please check your company information settings."
  =gfModalGen('TRM40170B00000','DIALOG','This form')          
  RETURN
ENDIF 

=lfOptProg() 

IF llIsAparel
  IF FILE(lcPrgName +'.FXP')
    DO EVAL('lcPrgName')
  ELSE
    * Message "Form 'XX' does not exist.Please check your company information settings."   
    =gfModalGen('TRM40170B00000','DIALOG',"Form '" +SUBSTR(lcPrgName,lnFrmPos) + "'" )            
    RETURN
  ENDIF
  
  IF !llNoRec
    DO ENDREPORT
  ENDIF
ELSE
  lcSavPAdv = _PADVANCE
  IF !lfGetFrx()
    * Message "Form 'XX' does not exist.Please check your company information settings." 
    =gfModalGen('TRM40170B00000','DIALOG',"Form '" +SUBSTR(lcFormName,lnFrmPos) + "'" )          
    RETURN
  ENDIF

  IF llarpinv
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  ENDIF
  _PADVANCE = lcSavPAdv
  
ENDIF  

SELECT (lcInvPrtUp)
SCAN 
  SELECT MInvHdr
  =SEEK(EVAL(lcInvPrtUp+'.cMatInv'))
  REPLACE PrtFlag WITH 'P'
ENDSCAN  
USE IN (lcInvPrtUp)

SELECT MInvHdr
SET SKIP TO
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO
SELECT Fabric
SET RELATION TO
SELECT MInvLine
SET RELATION TO
IF USED(lcTmpDbt)
  SELECT (lcTmpDbt)
  SET RELATION TO
  USE
ENDIF
ERASE (gcWorkDir + (lcTmpDbt)+'.*')

*!*************************************************************
*! Name      : lfOptMsg
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfOptMsg

PRIVATE laOptMsg

DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 2nd. line Variable

laOptMsg[1,2] = 75                && Line length

IF EMPTY(lcRpMsg1) .AND. EMPTY(lcRpMsg2) .AND. EMPTY(lcRpMsg3)

  IF FILE(gcDataDir+lcTypInv+'.MEM')
    RESTORE FROM gcDataDir+lcTypInv+'.MEM' ADDITIVE
  ENDIF

  =gfOptMsg('laOptMsg')

  SET MEMOWIDTH TO 75              && the length of the memo field.
  SAVE TO gcDataDir+lcTypInv+'.MEM' ALL LIKE lcRpMsg*

ELSE
  =gfOptMsg('laOptMsg')
ENDIF

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
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
*
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
*! Purpose   : Validation function for the Account field
*!*************************************************************
*! Called from : Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvAccount

PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
*! Purpose   : Validation function for the cMatInv number field
*!*************************************************************
*! Called from : cMatInv number field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvInvNo

PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

lcInvHdTag = ORDER('MInvHdr')
lcCstmrTag = ORDER('CUSTOMER')
SET ORDER TO TAG MInvHdr IN MInvHdr
SET ORDER TO TAG CUSTOMER IN CUSTOMER

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'MInvHdr'))
  
  lcBrFields = "cMatInv :R :H= 'Invoice' , " +;
               "Printed = IIF(PrtFlag = 'P' , 'Yes' , 'No') :R :H= 'Printed' , " +;
               "InvDate :R :H= 'Date' , " +;
               "Account :R :H= 'Account' , " +;
               "cMorder :R :H= 'Order' , " +;
               "CustPO  :R :H= 'Reference' , " +;
               "CUSTOMER.BTName :R :H= 'Bill to' , " +;
               "Rep1    :R :H= 'Sales Rep.' , " +;
               "Ship    :R :H= 'Pieces' , " +;
               "ShipAmt :R :H= 'Merchandise'"
  
  lcFile_Ttl = 'Receivable invoices' 
  
  SELECT MInvHdr
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]
  
  IF gfBrows('' , 'cMatInv' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE    && Else
    &lcObjName = laOldVal
  ENDIF    && End of IF
  
  SET RELATION OFF INTO CUSTOMER
ENDIF    && End of IF

*IF The MInvHdr file did not have an active index
IF EMPTY(lcInvHdTag)
  SET ORDER TO 0 IN MInvHdr
ELSE    && Else
  SET ORDER TO TAG (lcInvHdTag) IN MInvHdr
ENDIF    && End of IF

*IF The CUSTOMER file did not have an active index
IF EMPTY(lcCstmrTag)
  SET ORDER TO 0 IN CUSTOMER
ELSE    && Else
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Haytham El_Sheltawi
*! Date      : 01/15/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPINVA.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
*--HDM =>FOR lnCount = 1 TO 5
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Haytham El_Sheltawi
*! Date      : 01/15/1998
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Terms
*!*************************************************************
*! Called from : ARPINVA.FRX
*!*************************************************************
*! Calls       : gfRltFld() , gfCodDes() , gfGetAdr() , lfAdrShift()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfSolSpAdr

PRIVATE lnInvHdRec , lnInvLnRec , lnLineRec

lnInvHdRec = IIF(EOF('MInvHdr') , 0 , RECNO('MInvHdr'))
lnInvLnRec = IIF(EOF('MInvLine') , 0 , RECNO('MInvLine'))

*-- (LCTMPDBT) , ARMInstD
IF USED(lcTmpDbt)
  lnTmpDbt = IIF(EOF(lcTmpDbt) , 0 , RECNO(lcTmpDbt))
  lnARINSTMD = IIF(EOF('ARMInstD') , 0 , RECNO('ARMInstD'))
ELSE
  lnTmpDbt   = 0
  lnARINSTMD = 0
ENDIF
*HAYTHAR ADD [END]
lnLineRec = IIF(EOF('MInvLine') , 0 , RECNO('MInvLine'))
lnHrRc    = IIF(EOF('MInvHdr') , 0 , RECNO('MInvHdr'))
COUNT TO lnLines WHILE MInvLine.cMatInv = MInvHdr.cMatInv
IF lnInvLnRec > 0
  GO (lnLineRec) IN MInvLine
ENDIF
IF lnHrRc > 0
  GO (lnHrRc) IN MInvHdr
ENDIF

IF !EMPTY(MInvHdr.CFACCODE)
  =SEEK(MInvHdr.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

llEndGroup = .F.
=gfRltFld(MInvHdr.cDivision , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(MInvHdr.ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(MInvHdr.cTermCode , 'CTERMCODE')

lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

SELECT MInvHdr
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GOTO RECNO()
ENDIF
SELECT CUSTOMER

IF MaSoHdr.Alt_ShpTo
  lcShpTName  = MaSoHdr.STName
  laShipTo[1] = MaSoHdr.cAddress1
  laShipTo[2] = MaSoHdr.cAddress2
  laShipTo[3] = MaSoHdr.cAddress3
  laShipTo[4] = MaSoHdr.cAddress4
  laShipTo[5] = MaSoHdr.cAddress5
ELSE    && Else

  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    *-- DC Code
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF
  
  lcShpTName  = IIF(MInvHdr.STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))
  laShipTo[1] = IIF(MInvHdr.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(MInvHdr.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(MInvHdr.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(MInvHdr.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(MInvHdr.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF lnCUSRec <> 0 
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
ENDIF    && End of IF

=lfAdrShift('laShipTo')

*-- Restore the old record pointer in MInvHdr
SELECT MInvHdr

IF lnTmpDbt <> 0
  GO lnTmpDbt IN (lcTmpDbt)
ENDIF
IF lnARINSTMD <> 0
  GO lnARINSTMD IN ARMInstD
ENDIF
*HAYTHAR ADD [END]

*Hossam ENDIF

*-- Restore the old record pointer in MInvLine
IF lnInvLnRec = 0
  GO BOTTOM IN MInvLine
  IF !EOF('MInvLine')
    SKIP IN MInvLine
  ENDIF
ELSE
  GO lnInvLnRec IN MInvLine
ENDIF

RETURN ''

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Haytham El_Sheltawi
*! Date      : 01/15/1998
*! Purpose   : Function to Update the End of Group flag and to update
*!             the PrtFlag field in the MInvHdr file if the divice is not
*!             Screen
*!*************************************************************
*! Called from : ARPINVA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfEndGroup
llEndGroup = .T.

*If the Device is not Screen
IF gcDevice <> 'SCREEN'
  INSERT INTO (lcInvPrtUp) (cMatInv) VALUES (MInvHdr.cMatInv)

ENDIF    && End of IF

RETURN ''

*!*************************************************************
*! Name      : lfInvSet
*! Developer : Haytham El_Sheltawi
*! Date      : 08/19/1998
*! Purpose   : Set function for the cMatInv number option in case
*!             of In Range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfInvSet

PARAMETERS lcParm

IF lcParm = 'S'
  
  lcInvHdTag = ORDER('MInvHdr')
  lcCstmrTag = ORDER('CUSTOMER')
  SET ORDER TO TAG MInvHdr IN MInvHdr
  SET ORDER TO TAG CUSTOMER IN CUSTOMER
  
ELSE
  
  *IF The MInvHdr file did not have an active index
  IF EMPTY(lcInvHdTag)
    SET ORDER TO 0 IN MInvHdr
  ELSE    && Else
    SET ORDER TO TAG (lcInvHdTag) IN MInvHdr
  ENDIF    && End of IF
  
  *IF The CUSTOMER file did not have an active index
  IF EMPTY(lcCstmrTag)
    SET ORDER TO 0 IN CUSTOMER
  ELSE    && Else
    SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
  ENDIF    && End of IF
  
ENDIF
*------------------------
FUNCTION lfCalcTax
*lnDetLinNo = 1
lnTaxable = lnTaxable + (MInvHdr.TAX_RATE * MInvLine.PRICE)
RETURN lnTaxable

*!*************************************************************
*! Name      : lfPrtNotes
*! Developer : Hossam Eldin Mahmoud El Etreby(HDM)
*! Date      : 01/28/1999
*! Purpose   : Function to Evaluate Notes To be Printed 
*! Returns   : Printed Notes
*!*************************************************************
*! Called from : ARPINVA,ARPINVZ .FRX (Notes Expression)
*!*************************************************************


FUNCTION lfPrtNotes
PRIVATE lcReturn

DO CASE

  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2' .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' ;
       .AND. SEEK('N' + MInvHdr.cMatInv , 'NOTEPAD')

    FOR lnLoop = 1 TO MEMLINES(NOTEPAD.mNotes)
      IF MLINE(NOTEPAD.mNotes , lnLoop) = CHR(13)
        lcNotes    = ALLTRIM(NOTEPAD.mNotes)
      ENDIF
    ENDFOR
    lcNotesTtl = 'Invoice Notes'
    lcNotes    = ALLTRIM(NOTEPAD.mNotes)
  CASE llRpInvLNt .AND. !EMPTY(MInvLine.note_mem) .AND. LEFT(ALLTRIM(STRTRAN(MInvLine.note_mem,CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(MInvLine.note_mem)
  
  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*!*************************************************************
*! Name      : lfPrtLNotes
*! Developer : Hossam Eldin Mahmoud El Etreby(HDM)
*! Date      : 02/15/1999
*! Purpose   : Function to Evaluate Line Notes Only To be Printed 
*!*************************************************************
*! Returns   : Printed Notes
*!*************************************************************
*! Called from : ARPINVA.FRX (Dos Format)
*!*************************************************************
*
FUNCTION lfPrtLNotes
PRIVATE lcReturn

DO CASE
  CASE llRpInvLNt .AND. !EMPTY(MInvLine.note_mem) .AND. LEFT(ALLTRIM(STRTRAN(MInvLine.note_mem,CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(MInvLine.note_mem)
  
  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*!*************************************************************
*! Name      : lfPrtINotes
*! Developer : Hossam Eldin Mahmoud El Etreby(HDM)
*! Date      : 02/15/1999
*! Purpose   : Function to Evaluate cMatInv Notes Only To be Printed 
*!*************************************************************
*! Returns   : Printed Notes
*!*************************************************************
*! Called from : ARPINVA.FRX (Dos Format)
*!*************************************************************
*
FUNCTION lfPrtINotes
PRIVATE lcReturn

DO CASE
  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2' .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Material Invoice Notes'
    lcNotes    = SUBSTR(ALLTRIM(NOTEPAD.mNotes),1,5)

  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*!*************************************************************
*! Name        : lfEndLine
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 08/24/1999
*! Purpose     : To get the Record count of the MInvLine.DBF
*!*************************************************************
*! Called from : ARPINVL.FRX
*!*************************************************************
*! Example     : lfEndLine
*!*************************************************************
FUNCTION lfEndLine
PRIVATE lcAlias
*lnInvRecNo      && Variable to hold the number of the last cMatInv Lines per
                 && invoice and the variable in Syrepuvr.dbf
lcAlias = ALIAS()
SELECT (lcInvLine_A)
SET ORDER TO TAG MInvLine DESCENDING
=SEEK(MInvHdr.cMatInv,lcInvLine_A)
lnInvRecNo = IIF(EOF(lcInvLine_A) , 0 , RECNO(lcInvLine_A))
SET ORDER TO TAG MInvLine ASCENDING
SELECT (lcAlias)
RETURN ''

*!*************************************************************
*! Name        : lfEndGrRtn
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 08/24/1999
*! Purpose     : To get this variable <llEndGroup> its initial value
*!******************************************************************
*! Called from : ARPINVL.FRX  <DOS>
*!******************************************************************
*! Example     : lfEndGrRtn
*!******************************************************************
FUNCTION lfEndGrRtn

llEndGroup = .F.
RETURN ''

*!*************************************************************
*! Name      : lfGetPath						
*! Developer : Nader Anis
*! Date      : 06/07/2000
*! Purpose   : Check If the file exist or not..
*!*************************************************************
*! Example	 : =lfGetPath
*!*************************************************************
FUNCTION lfGetPath

IF !EMPTY(lcOptProg)
  lcPath = ALLTRIM(lcOptProg)
  lcPath = IIF(FILE(gcRepHome+lcPath+'.FXP'),;
                   gcRepHome+lcPath,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcPath+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcPath,;
  				            gcRepHome+LEFT(lcPath,2)+'\'+lcPath)) 
   
  RETURN FILE(ALLTRIM(lcpath) +'.FXP')  
ENDIF  


*!*************************************************************
*! Name      : lfGetFrx						
*! Developer : Nader Anis
*! Date      : 06/07/2000
*! Purpose   : Check If the file exist or not.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Example	 : =lfGetFrx
*!*************************************************************
*
FUNCTION lfGetFrx
IF RAT('\',lcFormName)=0
  lcFrxPath= IIF(FILE(gcRepHome+lcFormName+'.FRX') ;
  				     .OR. FILE(gcRepHome+lcFormName+'.LBX'),;
  				     gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
ENDIF  				   
RETURN FILE(lcFrxPath +'.FRX') .OR. FILE(lcFrxPath+'.LBX')  				   

*!**************************************************************************
*! Name      : lfGetSign
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 05/07/2001
*! Purpose   : Get the symbol of the used curr.
*!**************************************************************************
*! Example   : = lfGetSign()
*!**************************************************************************
FUNCTION lfGetSign
PRIVATE lcSign

lcSign = SPACE(3)
lcSign = IIF(SEEK(MInvHdr.CCURRCODE,'SYCCURR'),SYCCURR.cCurrSmbl,lcSign)

RETURN lcSign
*-- End of lfGetSign

*!**************************************************************************
*! Name      : lfChkModls
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2002
*! Purpose   : Check if module MA is installed in your company
*!**************************************************************************
*! Example   :  =lfChkModls()
*!**************************************************************************
*B606791,1
FUNCTION lfChkModls
*-- llogtrmnat:- Variable Terminate The Option Grid.
IF !('MA' $ gcCmpModules)
  *-- Message No.    : 42083
  *-- Message Text   : XX module is not installed. Cannot proceed.    
  *-- Button No.     :  00000   
  *-- Message Button : OK
  =gfModalGen('TRM42083B00000','DIALOG','Material')
  llOgTrmnat = .T.
  CLEAR READ
  RETURN
ENDIF
*-- End Of lfChkModls.

*:**************************************************************************
*:* Name        : lfRmZr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/03/2005
*:* Purpose     : Remove zeros in the right of the decimal point
*:***************************************************************************
*:* Called from : ARMATIA.FRX
*:***************************************************************************
FUNCTION lfRmZr
PARAMETERS lnValue
PRIVATE lcValue,lnI
lcValue = ALLTRIM(STR(lnValue,15,5))+' '
FOR lnI = 1 TO 5
  lcValue = STRTRAN(lcValue,'0 ','  ')
ENDFOR
lcValue = LEFT(lcValue,LEN(lcValue)-1)
lcValue = IIF(VAL(lcValue)=0,'',PADL(lcValue,18))
RETURN  lcValue
*-- end of lfRmZr.