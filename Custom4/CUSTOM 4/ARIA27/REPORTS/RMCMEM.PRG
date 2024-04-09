*:***************************************************************************
*: Program file  : RmCMem
*: Program desc. : Credit Memo
*: For Report    : RmCMemA.FRX
*: System        : Aria Advantage Series.
*: Module        : Return Merchandise (RM)
*: Developer     : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe,
*:               : gfRltFld, gfCodDes,gfGetMemVar,gfOptMsg,gfBrows.
*:               : lfGetLogo,lfAdrShift,lfSolSpAdr,lfHeadVar,lfSaveFlag,
*:               : lfEndGroup,lfwRepWhen,lfFormName,lfvCrMemo,lfvAccount,
*:               : lfvWareHs,lfvOptMsg,lfwOldVal,lfClearRep.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : All [[IF llFrTime]] Blocks executed one time in the option grid seasson.
*:         : and if user selection opens any temp. files that may be used later
*:         : in another selection I take this file open to use untill user choice
*:         : is to press < Close > button, to save more time. 
*:***************************************************************************
*: Example : DO RmCMem
*:***************************************************************************
*: This Program is due to E300846 ...
*:***************************************************************************
*: Modifications :
*:B802214,1 BWA  05/04/1999 Fix the bug  of the report Tax title
*:B802266,1 HDM  06/03/1999 Add Set function to force in range go to top of file
*:C101584,1 ADEL  07/22/1999 Run a selected form from form codes instead of
*:C101584,1 					always calling form A
*:B603247,1 MAB 10/28/1999 Remove a strange print prices option found in grid
*:B603247,1                Note this option affect print size scales only.
*:C200108,1 RAMY 12/19/1999 Custome Form For F&G
*:C101751,1 HDM  01/13/2000 Custom Form For TOCCA
*:B603396,1 HDM  01/24/2000 Exclude collecting lines for any other trans. except
*:                          the received lines
*:B603598,1 RAMY 04/30/2000 Add the update of the 'FLAG' field after printing
*:B803332,1 BWA 06/19/2000 Fix the bug of not printing PST tax if the country is canada.[FRX]
*:B603712,1 RAMY 07/04/2000 Eleminate the spaces in the form to add more lines
*                           NOTE: the modification is in the .FRX file only
*:C102212,1 ADEL 34/19/2001 Add Canadian HST tax report.(FRX) also add PST as it doesn't exist.
*:C200274,1 RAE 02/05/2002 Customized Credit Memo form for england
*:C200377,1 BWA 08/07/2002 Add variables to hold the length and postion of color and style.
*:E301988,1 BWA 08/19/2002 Change the header of the Credit Memo to Be Credit Note in case the Country is England [Fix in FRX].
*:E302057,1 BWA 12/08/2002 Add the Tax refrence in case ENG.
*:B129694,1 NNA 09/22/2005 Fix bug that the form does not print Currency Sign
*:***************************************************************************
lcTime = TIME()          && Variable to hold the Time
llTitle    = .T.         && Flag to print Detail header.
llEndGroup = .F.         && Flag to detect end of the Group.
lcDivLName = ''

*B129694,1 NNA 09/22/2005 (Begin) Add the currency sign to the total credit Amount on the form. 
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
*B129694,1 NNA (End)

*E302057,1 BWA 12/08/2002 Add the Tax refrence in case ENG.[START]
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
*E302057,1 BWA 12/08/2002.[END]

*C200274,1 RAE [START] 
lcTax_Rate = ''          && Hole the percentage rate of tax
lcVatNo    = ''          && Hold the VAT number
lcCompFax  = ''          && Hold the company Fax
*C200274,1 RAE [END]

*C101751,1 [Start] Check if we are going to display tocca
*                  Custome format
*                  If So raise a flag tell that also
*                  declare variables to hold Vendor# and DUNS# 
IF TYPE('lcTocca')='C' AND ALLTRIM(lcTocca) ='Y'
  llTocca = .T.
  STORE '' TO lcVendCD , lcDunsNo , lcDept
ELSE
  llTocca = .F.
ENDIF
*C101751,1 [End]

*C200377,1 BWA 08/072002 Add variables to hold the length and postion of color and style.[START]
                  *--Section to get the style and color length --*
*--THE COLOR LENGTH
STORE 0 TO lnClrLnR1 , lnClrPosR1
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnR1  = LEN(laItemSeg[lnCount,3])
    lnClrPosR1 = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
STORE 0 TO lnLenthR1
lnLenthR1 = LEN(gfItemMask('PM'))

               *--End Section to get the Style/Color Lengths --*

*--Initial variable to check if print the report from the main or from the optional prorgram.
llRmMemo = .T.            && llarpinv it will be a commen variable.

*C200377,1 BWA 08/07/2002.[END]

*-- if it's first time you run option Grid, i.e: you have unknown variables.

*C200108,1 RAMY Add he condition to check if we print the f&g form or not, that privent the data 
*               collection in the main program and replace it with the Optional Progaram [start]
*hdm [start] temp.
*IF TYPE('lcColec') <> 'C' .AND. lcColec = 'N'
IF TYPE('lcColec') <> 'C'
*hdm [End] temp.
  IF llFrTime
    llTax      = gfGetMemVar('M_TAX') = 'Y'  && This company use taxes.
    *B802214,1 BWA Get the tax description title from the setups file.
    IF llTax
      lcTaxDsc = gfGetMemVar('M_TAX_DESC')
      lcTaxDsc = IIF(EMPTY(lcTaxDsc),'GST',lcTaxDsc )
    ENDIF
    *B802214,1 BWA  end
  
    lcStyTitle = gfItemMask('HI')            && Title of the style.
    lnMajorLen = LEN(gfItemMask("PM"))       && Style major length.
    lcObj_Id   = ''                           && Object Identification in Objlink file. 
  
    *-- if this company have a logo, put it in temp. file and then use it in .FRX
    
    IF SEEK('*' + 'LOGO' , 'OBJLINK') AND ;
      SEEK(OBJLINK.cObject_ID,'OBJECTS')
      = lfGetLogo()  && Function to Fill the temp. With company Logo.
    ENDIF

    lcRetLine = gfTempName()      && Name of temp. File have lines data.    
    *-- Add Two fields to file structure.
    SELECT RetLine
    = AFIELDS(laFileStru)
    lnFileStru = ALEN(laFileStru,1) 
  
    DIMENSION laFileStru[lnFileStru+2, 4]
    lnFileStru = lnFileStru + 2
  
    laFileStru[lnFileStru - 1 ,1] = 'lEndLine'
    laFileStru[lnFileStru - 1 ,2] = 'L'
    laFileStru[lnFileStru - 1 ,3] = 1
    laFileStru[lnFileStru - 1 ,4] = 0

    laFileStru[lnFileStru  ,1] = 'mNotes'
    laFileStru[lnFileStru  ,2] = 'M'
    laFileStru[lnFileStru  ,3] = 1
    laFileStru[lnFileStru  ,4] = 0
  
    CREATE TABLE (gcWorkDir+lcRetLine) ;
           FROM ARRAY laFileStru

    INDEX ON CRMEMO TAG (lcRetLine)
    SELECT (lcRetLine)
    SET RELATION TO style INTO Style ADDITIVE

  ENDIF  && End if llFrTime block

  *-- if you find previous data in the file [Not first time you run the report.]
  IF RECCOUNT(lcRetLine) != 0
    SELECT (lcRetLine)
    ZAP
  ENDIF

  *-- Fill Temp. Lines file with data. 
  SELECT RetHdr
  SCAN FOR &lcRpExp
    SELECT RetLine
    *B603396,1 [Start] Scan only for Transaction code = '2'==> Receiving
    *SCAN FOR CrMemo = RetHdr.CrMemo
    SCAN FOR CrMemo = RetHdr.CrMemo AND CRET_TRNCD = '2'
    *B603396,1 [End]
      SCATTER MEMVAR MEMO
      SELECT (lcRetLine)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDSCAN
    SELECT (lcRetLine)
    REPLACE lEndLine WITH .T.   && End lines for every group.
  
    *-- if user want to print notepad and there is notepad for this CrMemo 
    IF llRpOrdNot AND SEEK('R' + RetHdr.CrMemo , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      SELECT (lcRetLine)
      APPEND BLANK
      REPLACE CrMemo   WITH RetHdr.CrMemo ,;
              mNotes   WITH ALLTRIM(NOTEPAD.mNotes) ,;
              lEndLine WITH .T.       
    ENDIF  && End if 
  ENDSCAN  && Endscan

  *--- Relations between opened files [begin]
  SELECT RetHdr
  SET RELATION TO cwarecode INTO Warehous ADDITIVE
  SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                      'S' + Account + Store) INTO CUSTOMER ADDITIVE

  SET RELATION TO CRMemo INTO (lcRetLine) ADDITIVE

  SELECT STYLE
  SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
  *--- Relations between opened files [end]

  IF llFrTime
    llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.
    DECLARE laCompAdd[6,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2], laMemoData[3,2]
        
    laCompAdd  = ''            && Array to hold the Company address
    laSoldTo   = ''            && Array to hold the Sold To address
    laShipTo   = ''            && Array to hold the Ship To address
    laMemoData = ''            && Array to hold (R/A # , Order #, Invoice #) 

    laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
    laDivLName[1,2] = 'lcDivLName'
  
    *-- Get company Address [begin].
    SELECT SYCCOMP
    SEEK gcAct_Comp
    lcCompName = cCom_Name
    lcCompPhon = cCom_Phon             && Variable to hold the Company Phone

    *C200274,1 RAE [START] 
    lcCompFax = cCom_Fax               && Variable to hold the Company Fax
    *C200274,1 RAE [END]

    lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
    *-- Load Company address.
    laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
    laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
    laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
    laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
    laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)    
    laCompAdd[6] = "Phone #:" + TRANSFORM(lcCompPhon , lcPhonPict) 
    = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.

    *C101751,1[start] Variable to hold DUNS#
    lcDunsNo  = gfGetMemVar('XDUNS')
    *C101751,1[end]

    
    *-- Get company Address [end].

  ENDIF  && End if llFrTime block

  lcSkipExpr = [(lcRetLine)]  && Expression we skip to later

  SELECT RETHDR
  SET SKIP TO &lcSkipExpr
ENDIF &&C200108,1 RAMY [end]

*C101584,1 Check if the report is an @ SAY report. If it is, run the program
*C101584,1 instead of calling gfDispRe()
*DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
*C200108,1 RAMY Call the function that will check if we have optional program or not [start]P

=lfOptProg()
*C200108,1 RAMY [END]
PRIVATE lcPrgName
lcPrgName  = lcFormName
llIsAparel = lfIsApparl(@lcPrgName)
llNoRec    = .F.
IF llIsAparel
  DO EVAL('lcPrgName')
  IF !llNoRec
    DO ENDREPORT
  ENDIF
ELSE

  *C200377,1 BWA 07/25/2002 Add variable to check if print from the main or from the optional program.[START]
  *DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  IF llRmMemo
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  ENDIF
  *C200377,1 BWA 07/25/2002.[END]

ENDIF
*C101584,1 end
*C200108,1 RAMY [start]
IF RIGHT(lcFormName , 2) = 'FG'
  =lfUsrVldFn('lfSetCnt','RMCMEMFG')
ENDIF
*C200108,1 RAMY [end]
*B603598,1 RAMY Add the loop to update the 'FLAG' field [start]
SELECT RETHDR
IF gcDevice <> 'SCREEN'
  SCAN FOR &lcRpExp
    REPLACE FLAG WITH 'Y'
  ENDSCAN
ENDIF
*B603598,1 RAMY [end]

*-- end of Report code.

*-- Function section
*-------------------------------------------
*!*************************************************************
*! Name      : lfGetLogo
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
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
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.

*!*************************************************************
*! Name        : lfHeadVar
*! Developer   : Mohamed Badran (MAB)
*! Date        : 03/25/1998
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
lcAlias = ALIAS()
llEndGroup = .F.
= lfSolSpAdr()  && Calculate [Sold To and Ship To Data]

*-- Calculate (R/A # , Order #, Invoice #) data [Begin]
laMemoData[1,1] = IIF(!EMPTY(RetHdr.RaNo),'R/A #','')
laMemoData[1,2] = RetHdr.RaNo

laMemoData[2,1] = IIF(!EMPTY(RetHdr.Order),'Order #','')
laMemoData[2,2] = RetHdr.Order

laMemoData[3,1] = IIF(!EMPTY(RetHdr.Invoice),'Invoice #','')
laMemoData[3,2] = RetHdr.Invoice

*-- Shift empty data
FOR lnMemoData = 1 TO 3
  IF EMPTY(laMemoData[lnMemoData,1])
    = ADEL(laMemoData,lnMemoData)
  ENDIF
ENDFOR    && End of FOR Loop

*-- Replace logical items with ''
FOR lnMemoData = 1 TO 6
  IF TYPE('laMemoData[lnMemoData]') $ "UL"
    laMemoData[lnMemoData] = ''
  ENDIF
ENDFOR    && End of FOR Loop

*-- Add ":" to the end of 2nd and 3rd titles.
FOR lnMemoData = 2 TO 3
  IF !EMPTY(laMemoData[lnMemoData,1])
    laMemoData[lnMemoData,1] = laMemoData[lnMemoData,1] + ;
                               SPACE(16 - LEN(laMemoData[lnMemoData,1])) + ":"
  ENDIF
ENDFOR     && End of FOR Loop
*-- Calculate (R/A # , Order #, Invoice #) data [End]

SELECT (lcAlias)
RETURN ''
*-- end of lfAdrShift.

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
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
= gfRltFld(RetHdr.CDivision , @laDivLName , 'CDIVISION')  && Get Division.
lcTerms   = gfCodDes(RetHdr.CTERMCODE , 'CTERMCODE')        && Get Terms.

SELECT CUSTOMER
lcSolTName = BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
= lfAdrShift('laSoldTo')

lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.

*C101751,1 [Start] Get the Vendor # from Customwer file
IF llTocca
  lcVendCD = Customer.CCUSVEND
  lcDept = IIF(!EMPTY(RetHdr.INVOICE) AND SEEK(RetHdr.INVOICE,'INVHDR'),INVHDR.DEPT,'')

ENDIF
*C101751,1[End]
*-- end of lfSolSpAdr.

*!*************************************************************
*! Name        : lfGetNotes
*! Developer   : Mohamed Badran (MAB)
*! Date        : 03/25/1998
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : SORDCONA.FRX [Variable lcDum in the report]
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetNotes()
*!*************************************************************
FUNCTION lfSaveFlag
llTitle =  !EVALUATE(lcRetLine+'.lEndLine')
RETURN ''
*-- end of lfGetNotes.

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
*! Purpose   : Function to Update the End of Group flag and to update
*!             the Flag field in the ORDHDR file if the divice is not
*!             Screen.
*!*************************************************************
*! Called from : SORDCONA.FRX [GROUP FOOTER BAND]
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

*IF The Divice is not Screen
IF gcDevice <> 'SCREEN'
  REPLACE RetHdr.Flag WITH 'Y'
ENDIF    && End of IF
RETURN '    '
*-- end of lfEndGroup.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
SET ORDER TO RETHDR   IN RETHDR    && To use it to validate CrMemo    in option grid.
SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate Account   in option grid.
SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate Warehouse in option grid.
*C101751,1 [Start]
IF TYPE('lcTocca')='C' AND ALLTRIM(lcTocca) ='Y'
  llInvOp = gfOpenFile(gcDataDir+'INVHDR' , 'INVHDR' , 'SH')
ENDIF
*C101751,1 [End]
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfFormName
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
*! Purpose   : Function to get the Form name
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Form name
*!*************************************************************
*! Example     : = lfFormName()
*!*************************************************************
*C101584,1 This function is not called anymore
*FUNCTION lfFormName
*RETURN 'RMCMEMA'
*-- end of lfFormName.

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
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
*! Example     : = lfvOptMsg()
*!*************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length

= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- end of lfvOptMsg.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
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
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvCrMemo
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
*! Purpose   : Validation function for the CrMemo field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCrMemo()
*!*************************************************************
FUNCTION lfvCrMemo
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND !SEEK(lcObj , 'RETHDR')))

  SELECT RETHDR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "CRMEMO    :R :H= 'CrMemo#' , "    +;
               "PRINTED = IIF(Flag = 'Y' , 'Yes' , 'No') :R :H= 'Printed' , " +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "CRDATE    :R :H= 'Credit Date',"+;
               "REASON    :R :H= 'Reason' ,"     +;
               "CDIVISION :R :H= 'Division' ,"   +;
               "CTERMCODE :R :H= 'Terms' ,"      +;
               "PIECES    :R :H= 'Pieces' ,"    +;
               "AMOUNT    :R :H= 'Amount ' ,"    +; 
               "OTHER     :R :H= 'Other ',"  +; 
               "TAX_AMT   :R :H= 'Tax Amount' "
  
  lcFile_Ttl = "Credit Memo ..."
  lcBrowCond = [FOR STATUS != "V"]
          
  = gfBrows(lcBrowCond,'CRMEMO','laTemp')
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvCrMemo.


*!*************************************************************
*! Name      : lfvAccount
*! Developer : Mohamed Badran (MAB)
*! Date      : 04/12/1998
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
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
*-- end of lfvAccount.

*!*************************************************************
*! Name      : lfvWareHs
*! Developer : Haytham El_Sheltawi
*! Date      : 04/12/1998
*! Purpose   : Validation function for the Warehouse Code field
*!*************************************************************
*! Called from : Warehouse Code field [Option Grid]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvWareHs

PRIVATE lcObjName , lcObjVal

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Warehouse he entered is not in the
*file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvWareHs.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/28/1998
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close >
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
*-- Close temp. opended files, if it used.
IF USED(lcRetLine)
  USE IN (lcRetLine)
  ERASE (gcWorkDir+lcRetLine+'.DBF')
  ERASE (gcWorkDir+lcRetLine+'.CDX')
ENDIF
*C101751,1 [Start] Close INVHDR file if Tocca Format
IF TYPE('lcTocca')='C' AND ALLTRIM(lcTocca) ='Y' AND USED('INVHDR')
  USE IN INVHDR
ENDIF
*C101751,1 [End]

*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfChkSysDy
*! Developer : AHMED AMER
*! Date      : 08/20/98
*! Purpose   : To check if the system uses dyelot or not
*!*************************************************************
*! Called from : Option grid (Variable llDyelot)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : = lfChkSysDy()
*!*************************************************************

FUNCTION lfChkSysDy

RETURN (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')

*!*************************************************************
*! Name      : lfSSet
*! Developer : HOSSAM EL ETREBY
*! Date      : 06/03/1999
*! Purpose   : Set function for in range
*!*************************************************************
*! Called from : Option grid (Variable Credit MEMO)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical
*!*************************************************************
*! Example     : =lfSSet()
*!*************************************************************
*!DUE TO B802266,1
*!*************************************************************

FUNCTION lfSSet

PARAMETER lcParam
GO TOP IN RETHDR

*!***************************************************************************
*!* Name        : lfGetSign
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/22/2005
*!* Purpose     : Get the symbol of the used currancy.
*!***************************************************************************
*!* Called from : the Report Frx
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfGetSign()
*!***************************************************************************
*!129694,1
FUNCTION lfGetSign
PRIVATE lcSign

lcSign = SPACE(3)
lcSign = IIF(SEEK(RETHDR.CCURRCODE,'SYCCURR'),SYCCURR.cCurrSmbl,lcSign)

RETURN lcSign
*-- End of Function .

