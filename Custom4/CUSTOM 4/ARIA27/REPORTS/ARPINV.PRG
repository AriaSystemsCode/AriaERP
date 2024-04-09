*:************************************************************************
*: Procedure file: ARPINV.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Haytham el_Sheltawi
*:      Copyright (c) 
*:  Last modified: 01/21/1998
*:
*:  Procs & Fncts: lfGetForm
*:               : lfOptMsg
*:               : lfwOldVal
*:               : lfvAccount
*:               : lfvInvNo
*:               : lfAdrShift
*:               : lfSolSpAdr
*:               : lfEndGroup
*:               : lfNotePad
*:               : lfNoteRec
*:
*:  Documented    01/21/1998
*:          E300782,1 This program was coded by HS for the Enhancement
*:                    Comany Settings
*:************************************************************************
*:Modi. 09/16/1998 By HDM To run both At/Say and .Frx Invoice forms According to
*:E301090,10 HDM 12/31/1998 Prevent Printing Void Invoices
*:C101385,1 ADEL 01/24/1999 Put the SKUs validation and supress functions in this program.
*:E301138,1 HDM 02/14/1999  Make the Standard Invoice form works in dos as well as windows
*:B801927,1 HDM 02/10/1999  Adjust browse fields of invoice 'inlist'
*:B801935,1 HDM 02/10/1999  Change totals font to BOLD
*:B802114,1 TAK 03/31/1999  Change to print the dist. centr addr isted of store addres if 
*:B802114,1 TAK             there is a dist. center asign to the store.
*:C101480,1 ADEL 03/24/1999 1)Get the distribution center address if found.
*:C101480,1                 2)Call the optional program function to be used in calling
*:C101480,1                    any external program doing something not found in this
*:C101480,1                    program.
*:B802171,1 HDM 04/20/1999  We Should Include DBA If found in customer file
*:                          Print Invoice Date before ship date in standered Invoice
*:C101535,1 HDM 05/26/1999  WE HAVE TO GET DC Code as well
*:C101535,1 HDM 05/26/1999  Custom Invoice Form for  CARIBOU LTD.
*                           ARPINVCL.FRX & OPT. PROG. ARGETSCA.PRG
*:B802298,1 HDM 05/30/1999  Invoice forms should Display Factor information
*:                          If found in invhdr file
*:C101651,1 BWA 08/29/1999 We need to convert standard Invoice form "K" from A26 to A27,
*:                          this form will be added in A27 standard version named form "L" thus we modify, 
*:                          functions in the main program to get this form work under the main program .
*:B802661,1 BWA 10/02/1999  Fix the bug of Invoice form "L" that doesn't print Factor information or
*:                          "Remit To" address for the factored invoices in the .FRX itself.
*:B603281,1 BWA 11/11/1999  Creat new option in the option grid of the invoice hold the print status of
*:                          the invoices if we want to print it or not (the work in the SYRPUVR.DBF).
*:                          and fix the bug of change the flag on the invoice when printing in (Fresh Invoice)
*:B802849,1 BWA 12/05/1999  Fix bug of increasing the font of the invoice form "A" in the FRX itself
*:B803009,1 BWA 02/01/2000  Fix bug of The invoices are printed with a wrong amount to pay.
*:B803058,1 ADEL 02/22/2000 Fix the bug of "Alias  'any invoice no entered in the OG' not found" as the variable
*:B803058,1                 lcInvoice used in any invoice form will overwrite lcInvoice used as a temp
*:B803058,1                 file name in this program.
*:B603469,1 BWA 27/02/2000  Fix the bug of Invocie form A does not print the merchandise charges although they 
*:B603469,1                 are included in the total amount to pay. (The fix in the FRX.)
*:B603650,1 BWA 05/17/2000 Fix the bug of Title not centered in the form [Fix in FRX ].
*:B803285,1 NAD 06/04/2000 1-Fix bug the address cuts the box after the line where it prints.  (Form A) (Fix in Frx)
*:B803285,1                2-Fix bug the form prints the Page Footer in a new page.
*:B803285,1                 (Adjust the page length to be 66 rows not 88 rows ) (Form A) (Fix in frx Dos.)
*:B603498,1 NAD 06/07/2000 Fix bug File does not exist if the form doesn't exist in the system.
*:B802619,1 WMA 07/03/2000 change the second number that is labeled "phone :" into the company fax number,
*:B802619,1                and change label to "Fax #:".  (Form A)
*:B603750,1 BWA 07/17/2000 Fix the bug of invoice program does not save the optional message that entered in the option grid.
*:B603372,1 BWA 08/08/2000 Fix the bug of when making invoice for consolidated customer for multi store sales order
*:B603372,1                The SOLD TO and SHIP TO print empty addresses.
*:B803326,1 BWA 08/10/2000 Fix the bug of showing the sku description even if there is no quantity for the size.[Invoice form A FRX]
*:B603894,1 BWA 09/14/2000 Wrong message in the optional message for specific setting . 
*:C101986,1 BWA 09/20/2000 initial variable to check if print the report 
*                          from the main or from the optional program.
*:B803722,1 BWA 10/10/2000 Fix the bug of printing empty line if the sku is empty in form L [Fix in FRX]
*:C200139,1 BWA 12/20/2000 Modify the conditions by add to it the check of the right record of the notepad.
*:B604168,1 BWA 02/01/2001 Dimension the array hold the scales to hold all the scales in the scale file.
*:E301612,1 BWA 05/07/2001 Add the currency sign to total charge in case of multy currency to invoice form A. 
*:B804142,1 BWA 05/14/2001 Fix bug Invoice form [A] prints the company address even if we have that setup parameter of "Print company address" set to NO.[Fix in frx]
*:B606472,1 ADEL 10/20/02 Fix the bug of not printing the sku when sku header is less than 8 chrs.
*:E302209,1 KHM 07/16/2003 Add the tax reference.
*:E302198,1 ALB 08/17/2003 change tax rate to print 99.999 formate in the FRX
*:B120016,1 ABD 10/21/2003 Print the Invoice from the invoice screen in case the invoice was printed or not.
*:C122585,1 BWA 08/12/2004 Add salesrep file to be open with the files.
*:C126052,1 BWA 03/16/2005 Add global variables hold the postion and length of the style , color and Scale.
*:B608571,1 ASH 05/22/2008 Fix error while printing invoices if CRM is installed. (T20080521.0014)
*:******************************************************************************************


*:B603750,1 BWA 07/17/2000 The name of the file hold the optional message lines.[START]
*-- lcTypInv  && Variable that is hold the name of the optinal message memo file.
*-- lcTypInv = 'InvMessg'

*B603894,1 BWA 09/14/2000 Comment this lines because it is duplicated in the lfOptMsg function.[START]
*IF FILE(gcDataDir+lcTypInv+'.MEM')
*  RESTORE FROM gcDataDir+lcTypInv+'.MEM' ADDITIVE
*ENDIF

*C126052,1 BWA 03/16/2005 Add global variables hold the postion and length of the style , color and Scale.[START]
STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl
=lfChkStrct() && Get the length of the style , color and scale.
*C126052,1 BWA 03/16/2005.[END]

IF EMPTY(lcRpMsg1) .AND. EMPTY(lcRpMsg2) .AND. EMPTY(lcRpMsg3)
  IF FILE(gcDataDir+lcTypInv+'.MEM')
    RESTORE FROM gcDataDir+lcTypInv+'.MEM' ADDITIVE
  ENDIF
ENDIF
*B603894,1 [END]
*B603750,1 [END]

*E301612,1 BWA 05/07/2001 Add the currency sign to total charge in case of multy currency to invoice form A. [START]
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
*E301612,1 [END]

*C101986,1 BWA 09/20/2000 initial variable to check if print the report 
*                         from the main or from the optional prorgram.[START]
llarpinv = .T.            && llarpinv it will be a commen variable.
*C101986,1 [END]

*B120016,1 ABD - Print the Invoice from the invoice screen in case the invoice was printed or not. [Begin]
IF !(TYPE('lcProgName') = 'U') .AND. TYPE('lcProgName') = 'C' .AND. lcProgName = 'ARDINV'
  lcRpPrSt  = ''
ENDIF
*B120016,1 ABD - [End]

*C101651,1 BWA 08/29/1999 We need to create array hold the size and the scales for thr form "L" 
*B604168,1 BWA 02/01/2001 Dimension the array hold the scales to hold all the scales in the scale file.[START]
lcScalCont = 0          &&  Scale Counter
lcScaleExp = ""         &&  Scale Expression
lcScalfl = SELECT(0)
SELECT SCALE
*B608571,1 ASH 05/22/2008 (Begin) For some reason the scan for is not working after installing the CRM and adding the new index 'CRM1'
*SCAN FOR Type+Scale+Prepak = 'S' AND lcScaleExp # SCALE.SCALE 
=SEEK('S')
SCAN WHILE Type+Scale+Prepak = 'S' 
  IF lcScaleExp # SCALE.SCALE 
*B608571,1 ASH 05/22/2008 (End)  
    lcScalCont = lcScalCont + 1
    lcScaleExp = SCALE.SCALE
  ENDIF
ENDSCAN
SELECT(lcScalfl)

*DIMENSION laScale[4,9]
DIMENSION laScale[lcScalCont,9]
*B604168,1 [END]

STORE SPACE(0) TO laScale
lcOldInv = SPACE(6)
*C101651,1 BWA 08/29/1999 [END]

llPrntInst = TYPE('lcPrntInst') = 'C' .AND. lcPrntInst='Y'
lcTime = TIME()          && Variable to hold the Time
llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
             'OBJECTS') , .T. , .F.)        && Flag to know if we are to print the Company Logo
*--HDM
STORE 0 TO TAXABLE , NTAXABLE , lnTaxable , lnLines
STORE '' TO lcStrToPrn
STORE .F. TO llPrtSku , llLineFlag , llInvFlag
STORE '' TO lcNotes , lcNotesTtl ,  lcLNoteTtl , lcLNotes ,lcINoteTtl , lcINotes
STORE .F. TO llLPrtNote , llIPrtNote

*--HDM C101535,1 [Start]Get Major & non Major Length
lnMajor  = LEN(gfItemMask('PM'))
lnNMajor = LEN(gfItemMask('PN'))
lcDCCode = ''
*--HDM C101535,1 [End]

lcPrgName  = lcFormName
llIsAparel = lfIsApparl(@lcPrgName)
DIME laSku[8]
STORE '' TO laSku
*--HDM
lcSolTName = ''        && Variable to hold the Sold to name
lcShpTName = ''        && Variable to hold the Ship to name
lcShipVia = ''         && Variable to hold the Ship Via Description
lcTerms = ''           && Variable to hold the Terms Description
lcSpkLin = ''          && Variable to hold the Size # of the SKU
llEndGroup = .F.       && Flag to know if we are at the end of the Group
lcDivLName = ''        && Variable to hold the Division long name
*llHedNote = .F.        && Flag to know if we are to Print the Invoice Note pad after this record

*--HDM B802298,1[Start] Define Variable to hold Factor Code
lcFacName  = ''
*--HDM B802298,1[End]

lcTmpDbt = gfTempName()

*B803009,1 Fix bug of The invoices are printed with a wrong amount to pay. [START]
*B803009,1 Create a temp file to hold the invoice number of the printed
*B803009,1 invoices. This file is used after printing to update the Print Flag field 
*B803009,1 in INVHDR file.
*B803058,1 (Begin) Remark the following lines and change the temp file name.
*B803058,1 because the lcInvoice name is used by some custom forms.
*lcInvoice = gfTempName()
*CREATE CURSOR (lcInvoice) (Invoice C(6))
*INDEX ON Invoice TAG Invoice of (gcWorkDir+lcInvoice)
lcInvPrtUp = gfTempName()
CREATE CURSOR (lcInvPrtUp) (Invoice C(6))
INDEX ON Invoice TAG Invoice of (gcWorkDir+lcInvPrtUp)
*B803058,1 (End)
*B803009,1 BWA 02/01/2000  [END]

*--B802298,1[Start] HDM New array holding Factor address
*DECLARE laCompAdd[6,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
DECLARE laCompAdd[6,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2], laFactor[5,1]
laFactor = ''
*--B802298,1[End]

laCompAdd = ''          && Array to hold the Company address
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

*C101480,1 (Begin) Open the SYCCOMP file.
=gfOpenFile(gcsyshome+'SYCCOMP',gcsyshome+'CCOMP_id','SH')
*C101480,1 (End)

*--HDM B802298,1 [Start] OPEN SYCFACT FILE
=gfOpenFile(gcsyshome+'SYCFACT',gcsyshome+'cfaccode','SH')
*--HDM B802298,1 [End]

*C101651,1 BWA 08/29/1999 We need to reopren the InvLine to get the recored number in a variable. 
=gfOpenFile(gcDataDir+"InvLine","InvLine",'SH', @LCInvLine_A, .T.)
*C101651,1 BWA [END]

llTax      = (gfGetMemVar('M_TAX') = 'Y')
lcTaxDesc = gfGetMemVar('M_TAX_DESC')
lcTaxMeth = gfGetMemVar('M_TAX_METH')

*E302209,1 KHM 07/16/2003 (Begin) Add the tax reference.
lcTaxRefDs = ALLTRIM(gfGetMemVar('M_TAX_REFE'))
*E302209,1 KHM 07/16/2003 (End)

*Ren
lcDunsNo  = gfGetMemVar('XDUNS')
*Ren end

*B606472,1 Get to know if company is extended size scale. [Begin]
llExtSize = gfGetMemVar('M_USEEXSSC')
*B606472,1 Get to know if company is extended size scale. [End]

SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name             && Variable to hold the Company Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
*B802619,1 WMA [Begin]
lcCompFax = cCom_Fax               && Variable to hold the Company Fax
*B802619,1 WMA [End]
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6] = TRANSFORM(lcCompPhon , lcPhonPict)
*B802619,1 WMA [Begin]
lcCompFax = TRANSFORM(lcCompFax , lcPhonPict)  && Fax No. Pic
*B802619,1 WMA [End]
=lfAdrShift('laCompAdd')

SET ORDER TO TAG INVLINE IN INVLINE
*Bassem
CREATE CURSOR (lcTmpDbt) (CFILE_NUM C(1))
IF llPrntInst .OR. llRpInvNot 
  IF !llIsAparel
    *Bassem
    *CREATE CURSOR (lcTmpDbt) (CFILE_NUM C(1))
    INSERT INTO   (lcTmpDbt) VALUES('1')
    IF llRpInvNot
      INSERT INTO (lcTmpDbt) VALUES('2')
    ENDIF
    IF llPrntInst
      INSERT INTO (lcTmpDbt) VALUES('3')
    ENDIF
    SELECT (lcTmpDbt)
    INDEX ON CFILE_NUM TAG CFILE_NUM of (lcTmpDbt)

    SELECT INVHDR
    SET RELATION TO '' INTO (lcTmpDbt)
    SELECT (lcTmpDbt)
    SET RELATION TO IIF(CFILE_NUM = '3', INVHDR.Invoice, '*') INTO ARINSTMD
    SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.Invoice, '*') INTO INVLINE ADDITIVE

  ELSE
    SELECT INVHDR
    SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE
  ENDIF    
ELSE
  SELECT INVHDR
  SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE
ENDIF

SELECT INVLINE
SET ORDER TO TAG STYLE IN STYLE
SET RELATION TO IIF(!EMPTY(INVLINE.ALTSTYLE) , INVLINE.ALTSTYLE ,INVLINE.Style) INTO STYLE
SET ORDER TO TAG SPCKLINS IN SPCK_LIN
SET RELATION TO "S" + INVLINE.Account + INVLINE.Style INTO SPCK_LIN ADDITIVE

*--HDM
*SET SKIP TO Spck_lin
*--HDM

SELECT STYLE
SET ORDER TO TAG SCALE IN SCALE
SET RELATION TO 'S' + Scale INTO SCALE

SELECT CUSTOMER
SET ORDER TO TAG CUSTOMER
*SET ORDER TO TAG SKUTMPL IN SKUTMPL
*SET RELATION TO 'S' + SkuTmpl INTO SKUTMPL
SELECT INVHDR

*B603372,1 BWA 08/08/2000 Fix the bug of when making invocie for consolidated customer for multi store sales order
*B603372,1                The SOLD TO and SHIP TO print empty addresses.[START]
*SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
*B603372,1 [END]

IF !llIsAparel
  SET SKIP TO INVLINE , SPCK_LIN
ENDIF
SELECT INVHDR
*--B602494,1 HDM[start]
SET RELATION TO 'O' + Invhdr.order INTO Ordhdr ADDITIVE
*--B602494,1 HDM[end]

IF llPrntInst .OR. llRpInvNot
  IF !llIsAparel
    SET SKIP TO (lcTmpDbt) , INVLINE , ARINSTMD
  ENDIF
ELSE
  IF !llIsAparel
    SET SKIP TO INVLINE
  ENDIF
ENDIF

lcRpExp = lcRpExp + IIF (!EMPTY(lcRpExp) .AND. lcFactrInv <> 'B' ,;
          ' .AND. ' , '') + IIF(lcFactrInv = 'F' , '!EMPTY(INVHDR.cFacCode)' ,;
          IIF(lcFactrInv = 'N' , 'EMPTY(INVHDR.cFacCode)' , ''))

*--HDM E301090,10 Prevent Printing Void Invoices[start]
lcRpExp = lcRpExp + " AND INVHDR.STATUS <> 'V'"
*--HDM E301090,10 Prevent Printing Void Invoices[end]

lcPrnComp = IIF(TYPE('llPrntComp') = 'L' , IIF(llPrntComp , 'Y' , 'N') , 'Y')
*llPrnFact = IIF(TYPE('llFactor ') = 'L', llFactor, .T.)
llPrntComp = lcPrnComp  = "Y"
llPrnFact  = IIF(TYPE('llFactor ') = 'L', llFactor, .F.)

llLineNote = llRpInvNot
llNotePad  = llRpInvLNt
SELECT INVHDR

*C101480,1 Also Add the optional program function to be used in getting 
*C101480,1 anything special not found in this program(arpinv).

*B603498,1 (Start) IF the file exist do the program.
lnFrmPos=AT("ARPINV",lcFormName)+6 
IF !lfGetPath()
  * Message "This form does not exist.Please check your company information settings."
  =gfModalGen('TRM40170B00000','DIALOG','This form')          
  RETURN
ENDIF 
*B603498,1 (End)

=lfOptProg() 
*C101480,1 (End)

*=lfPrtNotes()
IF llIsAparel
  *B603498,1 (Start) IF the file exist do the program.
  IF FILE(lcPrgName +'.FXP')
  *B603498,1 (End)
    DO EVAL('lcPrgName')
  *B603498,1 (Start)  Display the message
  ELSE
    * Message "Form 'XX' does not exist.Please check your company information settings."   
    =gfModalGen('TRM40170B00000','DIALOG',"Form '" +SUBSTR(lcPrgName,lnFrmPos) + "'" )            
    RETURN
  ENDIF
  *B603498,1 (End)
  
  IF !llNoRec
    DO ENDREPORT
  ENDIF
ELSE
  lcSavPAdv = _PADVANCE
  *B603498,1 (Start)  IF the file exist do the report.
  IF !lfGetFrx()
    * Message "Form 'XX' does not exist.Please check your company information settings." 
    =gfModalGen('TRM40170B00000','DIALOG',"Form '" +SUBSTR(lcFormName,lnFrmPos) + "'" )          
    RETURN
  ENDIF
  *B603498,1 (End)

  *C101986,1 BWA 09/20/2000 Check if print the report from the main program or the optional prorgram.[START]
  *  DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  IF llarpinv
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  ENDIF
  *C101986,1 [END]

  _PADVANCE = lcSavPAdv
  
ENDIF  

*B803009,1 BWA 02/01/2000  Fix bug of The invoices are printed with a wrong amount to pay. [START]
*B803058,1 (Begin) Remark the following lines and change the temp file name.
*SELECT (lcInvoice)
*SCAN 
*  SELECT INVHDR
*  =SEEK(EVAL(lcInvoice+'.INVOICE'))
*  REPLACE PrtFlag WITH 'P'
*ENDSCAN  
*USE IN (lcInvoice)
SELECT (lcInvPrtUp)
SCAN 
  SELECT INVHDR
  =SEEK(EVAL(lcInvPrtUp+'.INVOICE'))
  REPLACE PrtFlag WITH 'P'
ENDSCAN  
USE IN (lcInvPrtUp)
*B803058,1 (End)
*B803009,1 BWA 02/01/2000  [END]

SELECT INVHDR
SET SKIP TO
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO
SELECT STYLE
SET RELATION TO
SELECT INVLINE
SET RELATION TO
IF USED(lcTmpDbt)
  SELECT (lcTmpDbt)
  SET RELATION TO
  USE
ENDIF
ERASE (gcWorkDir + (lcTmpDbt)+'.*')
*!*************************************************************
*! Name      : lfGetForm
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
*! Purpose   : Function to get the Current Form name
*!*************************************************************
*! Called from : Option Grid [Default value for the variable lcFormName]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Form name
*!*************************************************************
*
*FUNCTION lfGetForm
*PRIVATE lnPrevAl

*-- Hossam [START]
*RETURN 'ARPINVA'

*lnPrevAl = SELECT(0)

*IF !USED('FORMCDHD')
*  USE (gcDataDir + 'FORMCDHD') IN 0 ORDER TAG CURFORM
*ENDIF
*SELECT FORMCDHD

*IF SEEK ('ARPINV')
*  lcCurform = UPPER(ALLTRIM(cFormMaj) + ALLTRIM(cCurForm))
*ENDIF

*RESTORE FROM MEMO mFormSets ADDITIVE

*FOR lnCount = 1 TO ALEN(laSettings,1)

* &laSettings[lnCount, 1] = laSettings[lnCount, 2]

*ENDFOR


*SELECT (lnPrevAl)

*RETURN lcCurform
*RETURN 'ARPINVZ'
*-- Hossam [end]

*!*************************************************************
*! Name      : lfRpWhen
*! Developer : Haytham El_Sheltawi
*! Date      : 01/21/1998
*! Purpose   : Option Grid When Function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfGetMemVar() , lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
*FUNCTION lfRpWhen

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

*B603894,1 BWA 09/14/2000 Check if the variables for the optional message from the setting is empty or not .[START]
IF EMPTY(lcRpMsg1) .AND. EMPTY(lcRpMsg2) .AND. EMPTY(lcRpMsg3)
*B603894,1 [END]

  *B603750,1 BWA 07/17/2000 Check if the memo file has lines of the optional message or not to restore them.[START]
  IF FILE(gcDataDir+lcTypInv+'.MEM')
    RESTORE FROM gcDataDir+lcTypInv+'.MEM' ADDITIVE
  ENDIF
  *B603750,1 [END]

  =gfOptMsg('laOptMsg')

  *B603750,1 BWA 07/17/2000 Save the lines of the optional message to the memo file.[START]
  SET MEMOWIDTH TO 75              && the length of the memo field.
  SAVE TO gcDataDir+lcTypInv+'.MEM' ALL LIKE lcRpMsg*
  *B603750,1 [END]

*B603894,1 BWA 10/14/2000 If the variables for the optional message is not empty show it.[START]
ELSE
  =gfOptMsg('laOptMsg')
ENDIF
*B603894,1 [END]

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
*! Purpose   : Validation function for the Invoice number field
*!*************************************************************
*! Called from : Invoice number field [Option Grid]
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

lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))
  
  lcBrFields = "Invoice :R :H= 'Invoice' , " +;
               "Printed = IIF(PrtFlag = 'P' , 'Yes' , 'No') :R :H= 'Printed' , " +;
               "InvDate :R :H= 'Date' , " +;
               "Account :R :H= 'Account' , " +;
               "Order   :R :H= 'Order' , " +;
               "CustPO  :R :H= 'Reference' , " +;
               "CUSTOMER.BTName :R :H= 'Bill to' , " +;
               "Rep1    :R :H= 'Sales Rep.' , " +;
               "Ship    :R :H= 'Pieces' , " +;
               "ShipAmt :R :H= 'Merchandise'"
  
  lcFile_Ttl = 'Receivable invoices' 
  
  SELECT INVHDR
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]
  
  IF gfBrows('' , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE    && Else
    &lcObjName = laOldVal
  ENDIF    && End of IF
  
  SET RELATION OFF INTO CUSTOMER
ENDIF    && End of IF

*IF The INVHDR file did not have an active index
IF EMPTY(lcInvHdTag)
  *B804142,1 BWA 05/14/2001 [START]
  *SET ORDER TO  IN INVHDR
  SET ORDER TO 0 IN INVHDR
  *B804142,1 BWA 05/14/2001 [END]
ELSE    && Else
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
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

PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec

lnInvHdRec = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))
lnInvLnRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
lnPakLnRec = IIF(EOF('SPCK_LIN') , 0 , RECNO('SPCK_LIN'))

*HAYTHAR ADD [BEGIN]
*-- (LCTMPDBT) , ARINSTMD
IF USED(lcTmpDbt)
  lnTmpDbt = IIF(EOF(lcTmpDbt) , 0 , RECNO(lcTmpDbt))
  lnARINSTMD = IIF(EOF('ARINSTMD') , 0 , RECNO('ARINSTMD'))
ELSE
  lnTmpDbt   = 0
  lnARINSTMD = 0
ENDIF
*HAYTHAR ADD [END]
lnLineRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
lnHrRc    = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))
COUNT TO lnLines WHILE INVLINE.INVOICE = INVHDR.INVOICE
IF lnInvLnRec > 0
  GO (lnLineRec) IN INVLINE
ENDIF
IF lnHrRc > 0
  GO (lnHrRc) IN INVHDR
ENDIF

*-- HDM B802298,1 [Start] Fill laFactor with factor address
IF !EMPTY(INVHDR.CFACCODE)
  =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF
*-- HDM B802298,1 [End]



llEndGroup = .F.
=gfRltFld(INVHDR.cDivision , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')

lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

*IF ORDHDR.Alt_ShpTo is .T.
*--HDM
SELECT INVHDR
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GOTO RECNO()
ENDIF
SELECT CUSTOMER
*--HDM

IF ORDHDR.Alt_ShpTo
  *SELECT ORDHDR
  lcShpTName  = ORDHDR.STName
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
ELSE    && Else

  *--HDM C101535,1[Start] We have to first get the DC Code not Shipto code
  *-- B802171,1 [Start] We Should Include DBA If found in customer file
  *lcShpTName  = CUSTOMER.STName
  *lcShpTName  = IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA)
  *-- B802171,1 [End]
  *--HDM C101535,1[End]
  
  *B802114,1 (Start) read the adress of the distrbution center.
  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    *-- DC Code
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF
  *B802114,1 (End).
  *--HDM C101535,1[Start]
  
  *B603372,1 BWA 08/08/2000 Fix the bug of when making invoice for consolidated customer for multi store sales order
  *B603372,1                The SOLD TO and SHIP TO print empty addresses.[START]
  *lcShpTName  = IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA)
  lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))
  *B603372,1 [END]

  *--HDM C101535,1[End]
  
  *B603372,1 BWA 08/08/2000 Fix the bug of when making invoice for consolidated customer for multi store sales order
  *B603372,1                The SOLD TO and SHIP TO print empty addresses.[START]
  *laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  *laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  *laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  *laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  *laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
  
  laShipTo[1] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))
  *B603372,1 [END]

  *B802114,1 (Start) put the file pointer back to the store because the only information
  *B802114,1         we need from the DC record is the addresss.
  IF lnCUSRec <> 0 
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
  *B802114,1 (End).
ENDIF    && End of IF

=lfAdrShift('laShipTo')

*HossamIF llRpInvLNt .OR. llRpInvNot
*Hossam  SELECT (lcDumFile)
  *-- Restore the old record pointer in INVHDR
*Hossam  IF lnInvHdRec = 0
*Hossam    GO BOTTOM IN INVHDR
*Hossam    SKIP IN INVHDR
*Hossam  ELSE
*Hossam    GO lnInvHdRec IN INVHDR
*Hossam  ENDIF
*HossamELSE

  SELECT INVHDR

*HAYTHAR ADD [BEGIN]
*-- (LCTMPDBT) , ARINSTMD
IF lnTmpDbt <> 0
  GO lnTmpDbt IN (lcTmpDbt)
ENDIF
IF lnARINSTMD <> 0
  GO lnARINSTMD IN ARINSTMD
ENDIF
*HAYTHAR ADD [END]

*Hossam ENDIF

*-- Restore the old record pointer in INVLINE
IF lnInvLnRec = 0
  GO BOTTOM IN INVLINE
  IF !EOF('INVLINE')
    SKIP IN INVLINE
  ENDIF
ELSE
  GO lnInvLnRec IN INVLINE
ENDIF

*-- Restore the old record pointer in SPCK_LIN
IF lnPakLnRec = 0
  GO BOTTOM IN SPCK_LIN
  IF !EOF('SPCK_LIN')
    SKIP IN SPCK_LIN
  ENDIF
ELSE
  GO lnPakLnRec IN SPCK_LIN
ENDIF

*-- (LCTMPDBT) , ARINSTMD
RETURN ''

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Haytham El_Sheltawi
*! Date      : 01/15/1998
*! Purpose   : Function to Update the End of Group flag and to update
*!             the PrtFlag field in the INVHDR file if the divice is not
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
*  REPLACE INVHDR.PrtFlag WITH 'P'

*B803009,1 BWA 02/01/2000  Fix bug of The invoices are printed with a wrong amount to pay. [START]
  *B803058,1 (Begin) Remark the following line and change the temp name file.
  *INSERT INTO (lcInvoice) (INVOICE) VALUES (INVHDR.INVOICE)
  INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  *B803058,1 (End)
*B803009,1 BWA 02/01/2000  [END]

ENDIF    && End of IF

RETURN ''

*!*************************************************************
*! Name      : lfInvSet
*! Developer : Haytham El_Sheltawi
*! Date      : 08/19/1998
*! Purpose   : Set function for the invoice number option in case
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
  
  lcInvHdTag = ORDER('INVHDR')
  lcCstmrTag = ORDER('CUSTOMER')
  SET ORDER TO TAG INVHDR IN INVHDR
  SET ORDER TO TAG CUSTOMER IN CUSTOMER
  
ELSE
  
  *IF The INVHDR file did not have an active index
  IF EMPTY(lcInvHdTag)
    SET ORDER TO 0 IN INVHDR
  ELSE    && Else
    SET ORDER TO TAG (lcInvHdTag) IN INVHDR
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
lnTaxable = lnTaxable + (INVHDR.TAX_RATE * INVlINE.PRICE)
RETURN lnTaxable

*!***************************************************************
*! Name : lpPrtSku.
*! Auth : Timour Abdalla Khalil.
*! Date : 07/20/95.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the style/color Skus for a specific account.
*G000000,1 TAK 07/20/95.
*!***************************************************************
FUNCTION lfPrtSku
PRIVATE lnPrevAl
STORE ' ' TO lcStrToPrn

*B803326,1 BWA 08/10/2000 Fix the bug of showing wrong sku.[START]
STORE '' TO laSku
*B803326,1 [END]

IF ! SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  llPrtSku = .F.
  RETURN .F.
ENDIF
lnPrevAl = SELECT (0)
SELECT Spck_Lin
IF !EMPTY(PACK_ID)
  lnI = 1
  *=SEEK('S'+Style.Scale,'Scale')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF

  *B606472,1 (Begin) Begin printing from the fisrt chr after the sku header.
  *SSE 
  *=SEEK(TYPE+ACCOUNT+STYLE,'SPCK_HDR')
  IF llExtSize
    =SEEK(TYPE+ACCOUNT+SUBSTR(Style,1,LEN(ALLTRIM(Style))-4),'SPCK_HDR')
  ELSE
    =SEEK(TYPE+ACCOUNT+STYLE,'SPCK_HDR')
  ENDIF
    
  lnDime1 = MIN(lnDime1,LEN(ALLTRIM(SPCK_HDR.SKU)))
  *B606472,1 (End)
  
  *DIME laSku[8]
  *laSku = SPACE(16)
   SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku[lnX]=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN
  lnI = 1
  =SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  DO WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    lcStrToPrn = 'SKU # ' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = ;
             'S'+InvLine.Account+InvLine.Style .AND. !EOF()
      lcI = STR(lnI,1)
      *lcStrToPrn = lcStrToPrn + Scale.Sz&lcI+':'+laSku(lnI) + ' '
      *lcSku    = lcSku + ALLTRIM(SUBSTR(SPCK_LIN.PACK_ID,LEN(ALLTRIM(SPCK_HDR.PACK_ID))+1,16))
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
  ENDDO  
ENDIF
SELECT (lnPrevAl)
IF EMPTY(lcStrToPrn)
  STORE '' TO laSku
  llPrtSku = .F.
  RETURN .F.
ELSE
  llPrtSku = .T.
  RETURN .T.
ENDIF


*SELECT Spck_Lin
*lnArrLen = 0
*IF !EMPTY(PACK_ID)
*  SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style
*    lnArrLen = lnArrLen + 1
*    lcSku    = lcSku + ALLTRIM(SUBSTR(SPCK_LIN.PACK_ID,LEN(ALLTRIM(SPCK_HDR.PACK_ID))+1,16))
*  ENDSCAN
*ENDIF


*!*************************************************************
*! Name      : lfvSkuPck
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/24/1999
*! Purpose   : Function to valid the shus option in the option grid.
*!*************************************************************
*! Called from : Option Grid    [Print sku/pack]
*!*************************************************************
*! Refer to C101385
*!*************************************************************

FUNCTION lfvSkuPck

CLEAR READ
llRpSkuSize = (llRpPrtSku)


*!*************************************************************
*! Name      : lfSuprSKU
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/24/1999
*! Purpose   : Function to determine wheather the skus option in 
*!           : the option grid will appear or not .
*!*************************************************************
*! Called from : Suppress field in the FORMCDDT file.
*!*************************************************************
*! Refer to C101385
*!*************************************************************

FUNCTION lfSuprSKU

*-- If we do not want the SKUs options to be browsed in the option grid
*-- Flase the two SKUs variables to be used in the Forms programs.
IF !(TYPE('lcPrntSKU') = 'C' .AND. lcPrntSKU='Y')
  STORE .F. TO llRpPrtSku,llRpSkuSize
ENDIF
RETURN (TYPE('lcPrntSKU') = 'C' .AND. lcPrntSKU='Y')

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

*C200139,1 BWA 12/20/2000 Modify the conditions by add to it the check of the right record of the notepad.[START]
*  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2' .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*'

  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2' .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' ;
       .AND. SEEK('C' + INVHDR.Invoice , 'NOTEPAD')
*C200139,1 [END]

    FOR lnLoop = 1 TO MEMLINES(NOTEPAD.mNotes)
      IF MLINE(NOTEPAD.mNotes , lnLoop) = CHR(13)
        lcNotes    = ALLTRIM(NOTEPAD.mNotes)
      ENDIF
    ENDFOR
    lcNotesTtl = 'Invoice Notes'
    lcNotes    = ALLTRIM(NOTEPAD.mNotes)
  CASE llRpInvLNt .AND. !EMPTY(Invline.note_mem) .AND. LEFT(ALLTRIM(STRTRAN(Invline.note_mem,CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(Invline.note_mem)
  
  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*!*************************************************************
*! Name      : lfPrtLNotes
*! Developer : Hossam Eldin Mahmoud El Etreby(HDM)
*! Date      : 02/15/1999
*! Purpose   : Function to Evaluate Line Notes Only To be Printed 
*! Returns   : Printed Notes
*!           Due to E301138,1
*!*************************************************************
*! Called from : ARPINVA.FRX (Dos Format)
*!*************************************************************

FUNCTION lfPrtLNotes
PRIVATE lcReturn

DO CASE
  CASE llRpInvLNt .AND. !EMPTY(Invline.note_mem) .AND. LEFT(ALLTRIM(STRTRAN(Invline.note_mem,CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(Invline.note_mem)
  
  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)


*!*************************************************************
*! Name      : lfPrtLNotes
*! Developer : Hossam Eldin Mahmoud El Etreby(HDM)
*! Date      : 02/15/1999
*! Purpose   : Function to Evaluate Invoice Notes Only To be Printed 
*! Returns   : Printed Notes
*!           Due to E301138,1
*!*************************************************************
*! Called from : ARPINVA.FRX (Dos Format)
*!*************************************************************

FUNCTION lfPrtINotes
PRIVATE lcReturn

DO CASE
  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2' .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Invoice Notes'
    lcNotes    = SUBSTR(ALLTRIM(NOTEPAD.mNotes),1,5)

  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)


*!*************************************************************
*! Name        : lfSclSze
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 08/24/1999
*! Purpose     : To get the scale and the size
*!*************************************************************
*! Called from : ARPINVL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : C101651
*!*************************************************************
*! Example     : lfSclSze
*!*************************************************************
FUNCTION lfSclSze

PRIVATE lcAlias  , lcX , lnCountSc , lcKey , lnSclCnt , lcScaleSz
lcOldInv   = INVHDR.INVOICE
lcAlias = ALIAS()
SELECT InvLine
lcKey = Invoice+STR(LineNo,6)  
SEEK InvHdr.Invoice
lnSclCnt = 0

*B604168,1 BWA 02/01/2001 Dimension the array hold the scales to hold all the scales in the scale file.[START]
*SCAN WHILE Invoice = InvHdr.Invoice  .AND. lnSclCnt < 5
STORE SPACE(0) TO laScale
SCAN WHILE Invoice = InvHdr.Invoice  .AND. lnSclCnt < lcScalCont + 1
*B604168,1 [END]

  IF ASCAN(laScale,INVLINE.SCALE) = 0 .AND. SEEK('S'+INVLINE.SCALE,'SCALE')
      lnSclCnt = lnSclCnt + 1
      laScale[lnSclCnt,1] = SCALE.Scale
      For lnCountSc = 1 TO 8 
        lcX = STR(lnCountSc,1)
        laScale[lnSclCnt,lnCountSc+1] = SCALE.SZ&lcX
      ENDFOR
  ENDIF
ENDSCAN    
SEEK(lcKey)     
SELECT (lcAlias)
RETURN ''

*!*************************************************************
*! Name        : lfEndLine
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 08/24/1999
*! Purpose     : To get the Record count of the InvLine.DBF
*!*************************************************************
*! Called from : ARPINVL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : C101651
*!*************************************************************
*! Example     : lfEndLine
*!*************************************************************
FUNCTION lfEndLine
PRIVATE lcAlias
*lnInvRecNo      && Variable to hold the number of the last Invoice Lines per
                 && invoice and the variable in Syrepuvr.dbf
lcAlias = ALIAS()
SELECT (lcInvLine_A)
SET ORDER TO TAG InvLine DESCENDING
=SEEK(InvHdr.Invoice,lcInvLine_A)
lnInvRecNo = IIF(EOF(lcInvLine_A) , 0 , RECNO(lcInvLine_A))
SET ORDER TO TAG InvLine ASCENDING
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
*! Passed Parameters : None
*!******************************************************************
*! Return      : None
*!******************************************************************
*! Refer to    : C101651
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
*! Ref       : B603498,1
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Example	      	 : =lfGetPath
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
*! Ref       : B603498,1
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Example	      	 : =lfGetFrx
*!*************************************************************
FUNCTION lfGetFrx
IF RAT('\',lcFormName)=0
  lcFrxPath= IIF(FILE(gcRepHome+lcFormName+'.FRX') ;
  				     .OR. FILE(gcRepHome+lcFormName+'.LBX'),;
  				     gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
ENDIF  				   
RETURN FILE(lcFrxPath +'.FRX') .OR. FILE(lcFrxPath+'.LBX')  				   

*E301612,1 BWA 05/07/2001 Add the currency sign to total charge in case of multy currency to invoice form A. [START]
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
lcSign = IIF(SEEK(INVHDR.CCURRCODE,'SYCCURR'),SYCCURR.cCurrSmbl,lcSign)

RETURN lcSign
*-- End of lfGetSign
*E301612,1 [END]

*C126052,1 BWA 03/16/2005 Add global variables hold the postion and length of the style , color and Scale.[START]
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/16/2005
*! Purpose   : Get the Style , Scale and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*C126052,1 BWA 03/16/2005.[END]