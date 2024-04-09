*:***************************************************************************
*: Program file  : ARGROSSP
*: Program desc. : Gross profit
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : Account receivable (AR)
*: Developer     : AHMED AMER (AHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARGROSSP
*:***************************************************************************
*B802059,1 03/24/1999 MAB Fix the following bugs
*B802059,1            1- Date Range does not save old value
*B802059,1            2- If you directly enter a valid style range you get 
*B802059,1               'No records to display message ...' 
*B802059,1            3- Variable lcSubTitl not found (Sort by Class) 
*B603115,1 08/17/1999 Walid(WAM) Add the multi currency to this report .
*B603184,1 BWA 10/05/1999 Fix the bug of sorting by Class doesn't work 
*B802659,1 BWA 22/11/1999 Fix the bug of the advance buttom and the title of the
*                         invoice date screen.
*B603315,1 BWA 05/12/1999 Fix the bug of show return transactions in case we 
*                         choose "Include Return -> Yes" in option grid. And Print
*                         descreption of the class sorting in the subtotaL
*B603387,1 SAM 01/12/2000 Fix printing *****.** in Gross Profit Report
*B603387,1                for returns if not foreign currency 
*B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.
*B802858,1 BWA 03/05/2000 Fix the bug of the OK bottom get an error Massage.
*C101712,1 BWA 03/20/2000 A custom process to calculate the gross and the net amount.
*C101795,1 BWA 04/05/2000 Convert GMA100 gross profit report to A27.
*B603656,1 BWA 05/25/2000 Modifications on the calculations for the custom process
*B603656,1                for the customized Gross profit report. for (Fresh Perduce).
*B803410,1 BWA 07/13/2000 Fix the bug of not printing item description.
*B603704,1 BWA 07/25/2000 The program run very slow.
*E301432,1 BWA 07/26/2000 Add a new filter to the criteria, which is posting date.
*B603799,1 BWA 08/06/2000 Fix the bug of the report print the voided credit memo.
*C101926,1 BWA 08/10/2000 A custom process for International Sourcing Group to get
*C101926,1                the cost from the style file not from the invline file.
*B603713,5 NAD 10/02/2000 Increase the picture of the Price Field.
*B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*B603970,1 BWA 10/19/2000 Add wait window to show the transaction no. while printing.
*B804038,1 AMM 02/25/2001 Fix the bug of displaying an empty message if no records upon printing.
*B604360,1 BWA 04/22/2001 Fix bug "lfvaccount" not found.
*B604674,1 BWA 07/16/2001 Fix the bug of Advanced button get error when the company isn't link to Gl.
*B604755,1 BWA 08/01/2001 Fix the bug Grand total gives Zero value when sorting by salesrep.
*B605440,1 NAD 01/29/2001 Fix Bugs in the realese .  
*B605654,1 SSE 03/12/2002 Fix bug of asteriks in Net Amount, Profit%, Grand Total by adding a decimal options.
*B605902,1 SSE 10/16/2002 Fix bug of shifting Net Amount under 'RETURNS WITHOUT INVOICES'.
*B606789,1 SSE 12/18/2002 Fix bug of not including trade discount in case of return merchandise.
*B606830,1 SSE 12/31/2002 Fix bug of not including trade discount for return if invoice is out of date range .
*B606944,1 WAB 02/10/2003 Fix the bug of not shopwing some customer in the summary report while srorting by Account 
*B607372,1 ABD 07/16/2003 Fix bug wrong numbers in while change the sort by.
*:***************************************************************************
*-- ldRPPLDate Variable that hold From date value
*-- ldRPPHDate Variable that hold to   date value
*-- lcRPFrSty  Variable that hold From style value
*-- lcRPToSty  Variable that hold To   style value
*-- lcRPFrAcc  Variable that hold From Account value
*-- lcRPToAcc  Variable that hold To   Account value
*-- lcRPSeason Variable that hold Season
*-- lcRPDiv    Variable that hold Division

*C101926,1 BWA 08/10/2000 Call the trigger to check the value of llIntrSG variable [START]
* "INTSGCOST" >> International Sourcing Group style cost.
llIntrSG = .F.
=gfDoTriger('ARGROSSP',PADR('INTSGCOST',10))
*C101926,1 [END]

*B603704,1 BWA 06/25/2000 Initial variables to use them in the filter.[START]
STORE SPACE(0) TO lcTrue , lcFalse
*B603704,1 [END]

ldLDate   = ldRPPLDate
ldHDate   = ldRPPHDate

*B802059,1 ==> 2- get proper XLStyle and XHStyle [Begin]
lnMajorLen = LEN(gfItemMask("PM"))
*XLSTYLE   = lcRPFrSty
*XHSTYLE   = lcRPToSty

XLSTYLE   = PADR(lcRPFrSty,lnMajorLen)
XHSTYLE   = PADR(lcRPToSty,lnMajorLen)
*B802059,1 ==> 2- get proper XLStyle and XHStyle [End  ]

XLACCOUNT = lcRPFrAcc
XHACCOUNT = lcRPToAcc
lcSeason  = lcRPSeason
lcDiv     = lcRPDiv

*-- Getting class. postion in Fixed filtter array to get all classes that
*-- user selected in option grid
*-- XTCS variable that hold selected classes
lnClassPo = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CLASS'),1)
XTCS = laOGFxFlt[lnClassPo,6]

*-- lcSlsRep  variable that hold sales rep.
*-- XDT       variable that showes how to print invoice (by Ship date OR Inv. date)
*-- lcReport  variable that showes if print report in summary or detail
*-- lcIncRet  variable that showse if returns is included or not
*-- lcSort    variable that showes the report sort 
*-- XGrdTotO  variable that showes printing grand total or not
*-- lcOTitle  variable that hold the optional title
*-- lcSubGros variable showes deducting returns from gross sales or not

lcSlsRep  = lcRPSalRep
XDT       = lcRPInvBy
lcReport  = lcRPSumDet
lcIncRet  = IIF(llRPIncRet,'Y','N')
lcSort    = lcRPSortBy
XGrdTotO  = IIF(llRPGrnTot,'Y','N')
llInclRet = llRPIncRet
lcOTitle  = lcRPTitle
lcSubGros = IIF(llRPSubRet,'Y','N')

*C101795,1 Variable hold the salesrep name. [START]
lcNamSrep = SPACE(0)
*C101795,1 [END]

DO lpFilesRela

*--initilize filters on file.
XFILTER = "INVHDR->Status<>'V'"
IF llInclRet 
  *B603799,1 BWA 08/06/2000 Fix the bug of the report print the voided credit memo.[START]
  *XRFILTER= ".T."
  XRFILTER= "RETHDR.Status<>'V'"
  *B603799,1 [END]
ENDIF

*E301432,1 BWA 07/26/2000 Add a new filter to the criteria, which is posting date.[START]
=lfvPostDat()
*E301432,1 [END]

*--Set and build the filters due to grid var.
DO lpSetFilter
lcSFilt=' '

*--Creat a temp file.
INVLTEMP = gfTEMPNAME()
DO lpCreateTmp

WAIT WINDOW 'Building the invoices records...' NOWAIT
*-- Build the invoice records.
SELE INVLINE
IF ! EMPTY(XLSTYLE)
  lcSetNear=SET('NEAR')
  SET NEAR ON
  SEEK XLSTYLE
  IF EOF()
    WAIT CLEAR
    *B804038,1 AMM change the wait window to dialoge message
    *WAIT WINDOW 'No invoices were found for the selected styles! UNABLE TO PROCEED.' NOWAIT
    =gfModalGen('TRM00052B00000','DIALOG')
    SET DEVICE TO SCREEN
    *B804038,1 AMM end
    
    SET NEAR &lcSetNear
    RETURN
  ENDIF
  SET NEAR &lcSetNear
  lcSFilt='REST WHILE STYLE<=XHSTYLE '
ELSE
  IF !EMPTY(XHSTYLE)  
    lcSFilt='REST WHILE STYLE<=XHSTYLE '
  ENDIF
ENDIF


*B603704,1 BWA 06/25/2000 use the invline tag to make the scan mush faster.[START]
IF EMPTY(XLSTYLE) AND EMPTY(XHSTYLE)
  SET ORDER TO Invline
ENDIF
*B603704,1 [END]

SCAN &lcSFilt FOR &XFILTER
  WAIT WINDOW 'Collecting invoice # : '+Invoice NOWAIT
  
  *C101712,1 Modify the Temp. with the gross price field [START]
  *SCATTER FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,TOTQTY,COST TO laInvRec

  *C101926,1 BWA 08/10/2000 Get the cost from style file if it for customer INT20.
  *SCATTER FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,GROS_PRICE,TOTQTY,COST TO laInvRec
  IF llIntrSG
    SCATTER FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,GROS_PRICE,TOTQTY,STYLE.TOTCOST TO laInvRec
  ELSE
    SCATTER FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,GROS_PRICE,TOTQTY,COST TO laInvRec
  ENDIF
  *C101926,1 [END]
  *C101712,1 [END]

  SELE &INVLTEMP
  APPE BLANK
  
  *C101712,1 Modify the Temp. with the gross price field [START]
  *GATHER FROM laInvRec FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,TOTQTY,COST
  GATHER FROM laInvRec FIELDS INVOICE,ACCOUNT,STYLE,LINENO,PRICE,GROS_PRICE,TOTQTY,COST
  *C101712,1 [END]
  
  *B603115,1 [start]
  *REPLACE TYPE  WITH 'INV',;
          CODE  WITH INVLINE->INVOICE,;
          DATE  WITH INVLINE->INVDATE,;             
          NAME  WITH CUSTOMER->BTNAME,;
          CLASS WITH CUSTOMER->CLASS,;
          DISCPCNT  WITH INVHDR->DISCPCNT,;
          TRDE_DISC WITH INVHDR->TRDE_DISC 

  REPLACE TYPE  WITH 'INV',;
          CODE  WITH INVLINE->INVOICE,;
          DATE  WITH INVLINE->INVDATE,;             
          NAME  WITH CUSTOMER->BTNAME,;
          CLASS WITH CUSTOMER->CLASS,;
          DISCPCNT  WITH INVHDR->DISCPCNT,;
          TRDE_DISC WITH INVHDR->TRDE_DISC ,;          
          Ccurrcode WITH INVHDR.Ccurrcode  ,;
          Nexrate   WITH INVHDR.Nexrate     ,;
          Ncurrunit WITH INVHDR.Ncurrunit
          
  *C101795,1 Replace the salesrep field in the Temp. file with the salesrep name. [START]
  REPLACE REP1  WITH INVHDR.REP1
  *C101795,1 [END]
  
  *B603115,1 [end..]
ENDSCAN

SELE &INVLTEMP 

IF llRetModu AND llInclRet  
  WAIT WINDOW 'Building the returns records...' NOWAIT
  *-- Build the returns records.
  INDEX ON Invoice+STR(RECNO(),7) TAG &INVLTEMP
  SELE RETLINE
  IF ! EMPTY(XLSTYLE)
    lcOldNear = SET("NEAR")
    SET NEAR ON    
    =SEEK(XLSTYLE)
    SET NEAR &lcOldNear
  ENDIF
  
  *B603315,1 BWA 05/12/1999 Fix the bug of show return transactions[START]
  XRFILTER = STRTRAN(XRFILTER,"INVHDR.cCurrcode","RETHDR.cCurrcode")
  *B603315,1 BWA 05/12/1999 [END]
  
  SCAN &lcSFilt FOR &XRFILTER
    WAIT WINDOW 'Collecting credit memo # : '+Crmemo NOWAIT
    lcInvoice=Invoice
    DIMENSION laInvRec[6]

    *C101712,1 Modify the Temp. with the gross price field [START]
    *SCATTER FIELDS ACCOUNT,STYLE,LINENO,PRICE,TOTQTY,COST TO laInvRec
    SCATTER FIELDS ACCOUNT,STYLE,LINENO,GROS_PRICE,PRICE,TOTQTY,COST TO laInvRec
    *C101712,1 (End)
    
    SELE &INVLTEMP 
    
    *B606830,1 Always seek for invoice in Invoice Header file. [Begin] 
    *IF !SEEK(lcInvoice)
    IF !SEEK(lcInvoice,'InvHdr')
    *B606830,1 Always seek for invoice in Invoice Header file. [End] 

      lcInvoice='ZZZZZZ'      && Returns without invoice or for invoices
                              && not in date range are holding the ZZZZZZ.
    ENDIF
    *B603387,1   [Start] 
    *B603387,1      Fix printing *****.** in Gross Profit Report
    *B603387,1      for returns if currency not the first.
    *B603387,1      added the currency information in REPLACE.
    *B603387,1      Add New Funcion (lfGetFCurr)To Display Foreign From Base Currency
    *APPE BLANK
    *GATHER FROM laInvRec FIELDS ACCOUNT,STYLE,LINENO,PRICE,TOTQTY,COST   
    *REPLACE TYPE WITH 'RET',;
            INVOICE WITH lcInvoice,;
            CODE  WITH RETLINE->CRMEMO      ,;
            DATE  WITH RETLINE->CRDATE      ,;             
            NAME  WITH CUSTOMER->BTNAME     ,;
            CLASS WITH CUSTOMER->CLASS      
     
    =SEEK(RETLINE.ACCOUNT+RETLINE.CRMEMO,'RETHDR')
    APPE BLANK

    *C101712,1 Modify the Temp. with the gross price field [START]
    *GATHER FROM laInvRec FIELDS ACCOUNT,STYLE,LINENO,PRICE,TOTQTY,COST
    GATHER FROM laInvRec FIELDS ACCOUNT,STYLE,LINENO,GROS_PRICE,PRICE,TOTQTY,COST
    *C101712,1 (End)
            
    REPLACE TYPE WITH 'RET',;
            INVOICE WITH lcInvoice,;
            CODE  WITH RETLINE->CRMEMO      ,;
            DATE  WITH RETLINE->CRDATE      ,;             
            NAME  WITH CUSTOMER->BTNAME     ,;
            CLASS WITH CUSTOMER->CLASS      ,;
            Ccurrcode WITH RETHDR.Ccurrcode ,;
            Nexrate   WITH RETHDR.Nexrate   ,;
            Ncurrunit WITH RETHDR.Ncurrunit
    
    *B606789,1 Include the trade discount. [Begin] 
    REPLACE Trde_Disc WITH IIF(SEEK(lcInvoice,'InvHdr'),InvHdr.Trde_Disc,0)
    *B606789,1 Include the trade discount. [End] 

  *C101795,1 Replace the salesrep field in the Temp. file with the salesrep name. [START]
  REPLACE REP1  WITH RetHdr.SalesRep1
  *C101795,1 [END]
  
  *B603387,1   [End]        
  
  ENDSCAN
ENDIF

SELE &INVLTEMP 
IF RECCOUNT() = 0
  WAIT CLEAR
  *B804038,1 AMM Use the new function of messages instead of new one.
  *= gfDialog( 'I', 'No records have been selected for the report.' )
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  *B804038,1 AMM end
  RETURN
ENDIF

*--Clear relations
SELE INVLINE
SET RELA TO

IF llRetModu
  SELE RETLINE
  SET RELA TO
ENDIF

*-- Indexing file.
Z = RECCOUNT()
WAIT WINDOW 'Sorting '+STR(Z,6)+' records for the report ' NOWAIT

SELE &INVLTEMP 
DO CASE
  CASE lcSort='A'
    INDEX ON Account+Invoice+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    lcHBreak='ACCOUNT'
  CASE lcSort='I'
    INDEX ON Invoice+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    lcHBreak='INVOICE'
  CASE lcSort='S'
    INDEX ON Style+Invoice+Type+Code+STR(RECNO(),7) TAG &INVLTEMP    
    lcHBreak='STYLE'
  CASE lcSort='C'
    INDEX ON Class+Invoice+Account+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    lcHBreak='CLASS'
    
  *C101795,1 New sort with salesrep . [START]
  CASE lcSort='R'
    INDEX ON REP1+Invoice+Account+Style+Type+Code+STR(RECNO(),7) TAG &INVLTEMP
    lcHBreak='REP1'
  *C101795,1 [END]
ENDCASE

WAIT CLEAR
R_WIDTH = 'W'

SET DEVICE TO PRINT
DO lpPrint
DO ENDREPORT
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lpFilesRela
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Create files relation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpFilesRela
*!*************************************************************

PROCEDURE lpFilesRela

SELECT INVLINE
SET RELATION TO INVOICE INTO INVHDR,'M'+ACCOUNT INTO CUSTOMER

*C101795,1 Relation to the style for the style group. [START]
SET RELATION TO STYLE INTO STYLE ADDITIVE
*C101795,1 [END]

IF llRetModu
  SELECT RETLINE
  SET RELATION TO ACCOUNT+CRMEMO INTO RETHDR,'M'+ACCOUNT INTO CUSTOMER
  SET RELATION TO STYLE INTO STYLE ADDITIVE
ENDIF

*!*************************************************************
*! Name      : lpSetFilter
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : reconstruct filter expression
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpSetFilter
*!*************************************************************

PROCEDURE lpSetFilter

*B603704,1 BWA 06/25/2000 Get the variables there value .[START]
lcTrue = IIF(XDT='S' , ".AND. INVHDR.SHIPDATE <=ldHDate" , ".AND. INVHDR.INVDATE <=ldHDate" )
lcFalse = IIF(XDT='S', ".AND. BETWEEN(INVHDR.SHIPDATE,ldLDate,ldHDate)", ".AND. BETWEEN(INVHDR.INVDATE,ldLDate,ldHDate)")
*B603704,1 [END]

IF !EMPTY(ldHDate)
  *B603704,1 BWA 06/25/2000 Change the filter to make it more optimize.[START] 
  *XFILTER=XFILTER+ IIF(EMPTY(ldLDate),;
  *".AND.IIF(XDT='S',INVHDR->SHIPDATE,INVHDR->INVDATE)<=ldHDate",;
  *".AND.BETWEEN(IIF(XDT='S',INVHDR->SHIPDATE,INVHDR->INVDATE),ldLDate,ldHDate)")
  
  XFILTER=XFILTER+ IIF(EMPTY(ldLDate),lcTrue,lcFalse)
  *B603704,1 [END]

  IF llInclRet
    XRFILTER=XRFILTER+ ".AND. BETWEEN(CRDATE,ldLDate,ldHDate)"
  ENDIF 
ENDIF

IF ! EMPTY(XLACCOUNT)
  XFILTER = XFILTER + ".AND. BETWEEN(ACCOUNT,XLACCOUNT,XHACCOUNT)"
  IF llInclRet
    XRFILTER= XRFILTER + ".AND. BETWEEN(ACCOUNT,XLACCOUNT,XHACCOUNT)"
  ENDIF
ENDIF

IF ! EMPTY(lcSeason)
  XFILTER = XFILTER + " .AND.INVHDR->SEASON=lcSeason"
  IF llInclRet
    XRFILTER= XRFILTER + ".AND. Style.Season=lcSeason"
  ENDIF
ENDIF

IF ! EMPTY(lcSlsRep)
  XFILTER = XFILTER + ".AND.INVHDR->REP1=lcSlsRep"
  IF llRetModu AND llInclRet  
    XRFILTER= XRFILTER+ ".AND.RETHDR->SALESREP1=lcSlsRep"  
  ENDIF
ENDIF

IF ! EMPTY(lcDiv)
  XFILTER = XFILTER + ".AND.INVHDR->cDIVISION=lcDiv"
  IF llRetModu AND llInclRet  
    XRFILTER= XRFILTER + ".AND. RETHDR->cDIVISION=lcDiv"
  ENDIF
ENDIF

IF ! EMPTY(ALLTRIM(XTCS)) 
  XFILTER = XFILTER + ".AND.TRIM(CUSTOMER->CLASS)$XTCS"
  IF llInclRet  
    XRFILTER= XRFILTER + ".AND.TRIM(CUSTOMER->CLASS)$XTCS" 
  ENDIF
ENDIF

*B603115,1 add the INVHDR.cCurrcode to the filter [start] 
IF ! EMPTY(laOGFxFlt[2,6])
  XFILTER = XFILTER + IIF(EMPTY(XFILTER), "" , " .AND. " ) + "INVHDR.cCurrcode = ALLTRIM(laOGFxFlt[2,6])"
  IF llRetModu AND llInclRet  
    *B603315,1 BWA 05/12/1999 Fix the bug of show return transactions[START]  
    *XRFILTER= XFILTER + IIF(EMPTY(XFILTER), "" , " .AND. " ) + " INVHDR.cCurrcode = ALLTRIM(laOGFxFlt[2,6]) "  
    XRFILTER= XRFILTER + IIF(EMPTY(XRFILTER), "" , " .AND. " ) + " INVHDR.cCurrcode = ALLTRIM(laOGFxFlt[2,6]) "
    *B603315,1 BWA 05/12/1999 Fix the bug of show return transactions[START]    
  ENDIF
ENDIF
*B603115,1 add the INVHDR.cCurrcode to the filter [end..] 

*C101795,1 Add the condition of the style group to the filter. [START]
IF !EMPTY(lcStyGrp)
  XFILTER = XFILTER + ".AND. STYLE.cStyGroup=lcStyGrp"
  IF llInclRet  
    XRFILTER= XRFILTER + " .AND. STYLE.cStyGroup=lcStyGrp"
  ENDIF  
ENDIF
*C101795,1 [END]

*!*************************************************************
*! Name      : lpCreateTmp
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Craete temp. file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCreateTmp
*!*************************************************************

PROCEDURE lpCreateTmp
*B603115,1 add CCURRCODE , Nexrate , Ncurrunit to the tmp file [start]
*CREATE TABLE &gcWorkDir.&INVLTEMP ;
  (TYPE  C(3)  , DATE D       , CODE  C(6)   , ACCOUNT C(5) ,;
   NAME C(30)  , CLASS C(2)   , INVOICE C(6) , STYLE C(19)  ,;
   LINENO N(6) , PRICE N(8,2) , TOTQTY N(6)  , COST N(10,2) ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) )           
   
*B603184,1 BWA 10/05/1999 Fix the bug of sorting by Class by change field class from 2 to 6 C [start]
*CREATE TABLE &gcWorkDir.&INVLTEMP ;
  (TYPE  C(3)  , DATE D       , CODE  C(6)   , ACCOUNT C(5) ,;
   NAME C(30)  , CLASS C(2)   , INVOICE C(6) , STYLE C(19)  ,;
   LINENO N(6) , PRICE N(8,2) , TOTQTY N(6)  , COST N(10,2) ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CCURRCODE  C(3) , Nexrate N(9,4) , Ncurrunit N(4,0) )           
 
*C101712,1 Modify the Temp. with the gross price field [START]
*CREATE TABLE &gcWorkDir.&INVLTEMP ;
  (TYPE  C(3)  , DATE D       , CODE  C(6)   , ACCOUNT C(5) ,;
   NAME C(30)  , CLASS C(6)   , INVOICE C(6) , STYLE C(19)  ,;
   LINENO N(6) , PRICE N(8,2) , TOTQTY N(6)  , COST N(10,2) ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CCURRCODE  C(3) , Nexrate N(9,4) , Ncurrunit N(4,0) )           

*C101795,1 Modify the Temp. with the salesrep field. [START]
*CREATE TABLE &gcWorkDir.&INVLTEMP ;
  (TYPE  C(3)  , DATE D       , CODE  C(6)   , ACCOUNT C(5) ,;
   NAME C(30)  , CLASS C(6)   , INVOICE C(6) , STYLE C(19)  ,;
   LINENO N(6) , PRICE N(9,2) , GROS_PRICE N(9,2) , TOTQTY N(6)  , COST N(10,2) ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CCURRCODE  C(3) , Nexrate N(9,4) , Ncurrunit N(4,0) )
*B603713,5 NAD 02/10/2000 (Start) Increase the price field width.
*CREATE TABLE &gcWorkDir.&INVLTEMP ;
  (TYPE  C(3)  , DATE D       , CODE  C(6)   , ACCOUNT C(5) ,;
   NAME C(30)  , CLASS C(6)   , INVOICE C(6) , STYLE C(19)  ,;
   LINENO N(6) , PRICE N(9,2) , GROS_PRICE N(9,2) , TOTQTY N(6)  , COST N(10,2) ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CCURRCODE  C(3) , Nexrate N(9,4) , Ncurrunit N(4,0),;
   REP1 C(3))

CREATE TABLE &gcWorkDir.&INVLTEMP ;
  (TYPE  C(3)  , DATE D       , CODE  C(6)   , ACCOUNT C(5) ,;
   NAME C(30)  , CLASS C(6)   , INVOICE C(6) , STYLE C(19)  ,;
   LINENO N(6) , PRICE N(12,2) , GROS_PRICE N(12,2) , TOTQTY N(8)  , COST N(10,2) ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CCURRCODE  C(3) , Nexrate N(9,4) , Ncurrunit N(4,0),;
   REP1 C(3))
*B603713,5 NAD 02/10/2000 (End)
*C101712,1 [END]
*C101795,1 [END]

*B603184,1 BWA 10/05/1999 [END]
*B603115,1 add CCURRCODE , Nexrate , Ncurrunit to the tmp file [end..]
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************

FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************

FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'

      lnClrPo    = laMajSeg[lnI,4]

      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
  
      lcNonMajPi = laMajSeg[lnI,3]

      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
  
      EXIT
  
    ELSE
      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*!*************************************************************
*! Name      : lfchkRet
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Check existence of RM module
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfchkRet()
*!*************************************************************

FUNCTION lfchkRet

RETURN ('RM' $ gcCMpModules)

*!*************************************************************
*! Name      : lfvIncRet
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate include returns option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvIncRet()
*!*************************************************************

FUNCTION lfvIncRet

lnSubRetPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPSubRet'),1)
laOGObjCnt[lnSubRetPo] = llRPIncRet
llRPSubRet = IIF(llRPIncRet,llRPSubRet,.F.)
= lfOGShowGet('llRPSubRet')

*!*************************************************************
*! Name      : lfvSumDet
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate Print summary or Detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSumDet()
*!*************************************************************

FUNCTION lfvSumDet

lnGrnTotPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPGrnTot'),1)
laOGObjCnt[lnGrnTotPo] = lcRPSumDet = 'S'
llRPGrnTot = .F.
= lfOGShowGet('llRPGrnTot')

*!*************************************************************
*! Name      : lfMajPic
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*!*************************************************************
*! Name      : lfvSalesRep
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate sales rep.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSalesRep()
*!*************************************************************

FUNCTION lfvSalesRep

lcRepCode = VARREAD()

*B605440,1 NAD 01/29/2001 Fix Bugs in the realese (Start) .  
*IF LASTKEY() = 13 AND !MDOWN()
* IF !EMPTY(&lcRepCode) AND  !SEEK(&lcRepCode, "SALESREP")
*B605440,1 NAD 01/29/2001 Fix Bugs in the realese (End)  .  

IF '?' $ &lcRepCode OR  !EMPTY(&lcRepCode) AND  !SEEK(&lcRepCode, "SALESREP")
  
    XREPCODE = &lcRepCode
    DO REPCHK WITH XREPCODE, .T.
    &lcRepCode = XREPCODE
*B605440,1 NAD 01/29/2001 Fix Bugs in the realese (Start) .  
  *ENDIF
*ELSE
*  &lcRepCode = ''
*B605440,1 NAD 01/29/2001 Fix Bugs in the realese (End)  .  
ENDIF

*!*************************************************************
*! Name      : lfvDateRng
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************

FUNCTION lfvDateRng
*B802059,1 ==> 1- Date Range does not save old value

PRIVATE ldFrom,ldTo

*B802059,1 ==> 1- Save old value to date ranges [Begin] 
*STORE {} TO ldFrom,ldTo
ldFrom = ldRPPLDate
ldTo   = ldRPPHDate
*B802059,1 ==> 1- Save old value to date ranges [End  ] 

*B802659,1 BWA 22/11/1999 Fix the bug of the title of the invoice date screen. [START]
*lcTitle = 'Prod. order comp. date'
lcTitle = 'Invoice Date'
*B802659,1 BWA 22/11/1999 [END]


*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]

ldRPPLDate = ldFrom
ldRPPHDate = ldTo
*-- end of lfvDateRng.

*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbDateOk()
*!*************************************************************

FUNCTION lfvpbDateOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfvObjRng
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Showes range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvObjRng()
*!*************************************************************

FUNCTION lfvObjRng

PARAMETERS lcObj

lcTitle = lcObj

DO CASE 
  CASE UPPER(lcObj) = "ACCOUNT"
    STORE lcRPFrAcc  TO lcFrom,lcOldFrm
    STORE lcRPToAcc  TO lcTo,lcOldTo
    STORE "@! XXXXX" TO lcPict
  CASE UPPER(lcObj) = "STYLE"
    STORE lcRPFrSty  TO lcFrom,lcOldFrm
    STORE lcRPToSty  TO lcTo,lcOldTo
    STORE lcMajPic   TO lcPict
ENDCASE


*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\ObjRng.SPR')
DO ObjRng.Spx
*B603955,1 ABD - [End]

DO CASE 
  CASE UPPER(lcObj) = "ACCOUNT"
    STORE lcFrom TO lcRPFrAcc
    STORE lcTo   TO lcRPToAcc 
  CASE UPPER(lcObj) = "STYLE"
    STORE lcFrom TO lcRPFrSty
    STORE lcTo   TO lcRPToSty
ENDCASE  
 
*!*************************************************************
*! Name      : lfvObj
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate Account OR style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvObj()
*!*************************************************************

FUNCTION lfvObj

*-- this is the validation of from_item

lcRngVarNa = VARREAD()
lcRngVarVa = &lcRngVarNa.

IF !EMPTY(lcRngVarVa)
  DO CASE
    CASE UPPER(lcObj) = "ACCOUNT"
      &lcRngVarNa = lfvAccount()
    CASE UPPER(lcObj) = "STYLE"
      &lcRngVarNa = lfvStyle()
  ENDCASE  
ENDIF

SHOW GET lcRngVarNa

*!*************************************************************
*! Name      : lfvStyle
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************

FUNCTION lfvStyle

lcStyle = lcRngVarNa

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ENDIF

SET ORDER TO lcTag IN STYLE

RETURN &lcStyle

*!*************************************************************
*! Name      : lfOldVal
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Saving old value
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOldVal()
*!*************************************************************

FUNCTION lfOldVal

lcObjNam = VARREAD()

DO CASE
  CASE UPPER(lcObjNam) = "LCFROM"
    lcOldFrm = &lcObjNam
  CASE UPPER(lcObjNam) = "LCTO"
    lcOldTo = &lcObjNam
ENDCASE    

*!*************************************************************
*! Name      : lfvpbObjOk
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate range screen's ok button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbObjOk()
*!*************************************************************

FUNCTION lfvpbObjOk

*-- this is the validation of to_item

IF !EMPTY(lcFrom) .OR. !EMPTY(lcTo)
  IF EMPTY(lcTo) 
    lcTo = lcFrom
  ENDIF
  IF lcFrom > lcTo
    WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
   _CUROBJ = OBJNUM(lcFrom)
  ELSE
    DO CASE
      CASE UPPER(lcObj) = "ACCOUNT"
        lcRPFrAcc = lcFrom
        lcRPToAcc = lcTo
      CASE UPPER(lcObj) = "STYLE"
        lcRPFrSty = lcFrom
        lcRPToSty = lcTo
    ENDCASE
    CLEAR READ
  ENDIF
ELSE
  CLEAR READ  
ENDIF

*!*************************************************************
*! Name      : lfvpbObjCa
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate range screen's Cancel button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbObjCa()
*!*************************************************************

FUNCTION lfvpbObjCa

*-- this is the validation of to_item

STORE lcOldFrm TO lcFrom
STORE lcOldTo  TO lcTo

DO CASE
  CASE UPPER(lcObj) = "ACCOUNT"
    lcRPFrAcc = lcFrom
    lcRPToAcc = lcTo
  CASE UPPER(lcObj) = "STYLE"
    lcRPFrSty = lcFrom
    lcRPToSty = lcTo
ENDCASE
CLEAR READ

*!*************************************************************
*! Name      : lpPrint
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Print report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************

PROCEDURE lpPrint

*B603656,1 BWA New Variable hold the return value of the gfdotriger() to check if the process exist or not.[START]
*PRIVATE llCallGfam
PRIVATE llCallGfam,llRetuV
*B603656,1 [END]
llCallGfam=EMPTY(laOGFxFlt[2,6]) OR (!EMPTY(laOGFxFlt[2,6]) AND lcRpCurr <> 'F')

SELECT &INVLTEMP
GO TOP

*B603656,1 BWA 05/25/2000 Modifications on the calculations for the custom process for the customized sales j. for (Fresh Perduce).
*C101712,1 A custome process for EF to calculate the gross and the net amount.[START]
llRetuV = .F.
=gfDoTriger('ARGROSSP',PADR('SALESRELAT',10))
*C101712,1 [END]
*B603656,1 [END]

ROW     = 99
PAGENO  = 0
R_TITLE = 'GROSS PROFIT BY '+lcHBreak
BREAK   = &lcHBreak

*-- Variables Initialization.
STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5        &&Subtotals
STORE 0 TO XGTOT1,XGTOT2,XGTOT3,XGTOT4,XGTOT5   &&Grandtotlals

XTITLE = SPACE(1)
XTIME  = TIME()
PAGENO = 0
ROW    = 99
XAcName= ' '

CLEAR TYPEAHEAD

DO WHILE .T.

   *B603970,1 BWA 10/19/2000 Add wait window to show the transaction no. while printing.[START]
   WAIT WINDOW 'Printing Transaction # : '+ &INVLTEMP..Invoice NOWAIT
   *B603970,1 [END]

   IF ROW >= 53
     PAGENO = PAGENO+1
     DO RPT_HDR WITH 'ARGROSSP'+lcReport,lcOTitle,R_WIDTH
     IF lcReport='S' .OR. ! llInclRet
     
       *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]
       *@ 05,00 SAY 'ACCOUNT ...... NAME ......... INVOICE ' + PADR(lcMajTtl,22) + 'PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %'
       *B603713,5 NAD 02/10/2000 (Start) Adjust the header after increasing the price field.
       *@ 05,00 SAY 'ACC. ...... NAME ......... INVOICE ' + PADR(lcMajTtl,20) + ' PIECES  S. PRICE   GROSS AMT      NET AMT       COST       PROFIT    PROF.%'
        @ 05,00 SAY 'ACC. ...... NAME ......... INVOICE ' + PADR(lcMajTtl,20) + ' PIECES    S. PRICE   GROSS AMT       NET AMT       COST       PROFIT  PROF.%'
       *B603713,5 NAD (End)
     ELSE
       *@ 05,00 SAY 'ACCOUNT ...... NAME ..... TYP INV/CRD ' + PADR(lcMajTtl,22) + 'PIECES  S. PRICE   GROSS AMT    NET AMT       COST     PROFIT   PROF. %'
       *B603713,5 NAD 02/10/2000 (Start) Adjust the header after increasing the price field.
       *@ 05,00 SAY 'ACC.  ...... NAME ..... TYP INV/CRD ' + PADR(lcMajTtl,20) + 'PIECES S. PRICE    GROSS AMT      NET AMT       COST       PROFIT     PROF.%'
        @ 05,00 SAY 'ACC.  ...... NAME ..... TYP INV/CR ' + PADR(lcMajTtl,20) + 'PIECES   S. PRICE     GROSS AMT       NET AMT       COST       PROFIT  PROF.%'
       *B603713,5 NAD 02/10/2000 (End)
       *B603169,1 BWA 03/05/2000 [END]
      
     ENDIF
     @ 06,00 SAY REPLICATE('=',132)
     ROW = 07
   ENDIF

   DO lpSubTotals   &&SubTotals

   STORE 0 TO XRTOT1,XRTOT2,XRTOT3,XRTOT4,XRTOT5    &&RetSubTot

   IF EOF() .OR. (llInclRet .AND. lcSort='I' .AND. Invoice='ZZZZZZ')
     EXIT  
   ENDIF

   IF ROW >=53
     ROW = 99
     LOOP
   ENDIF
    
   IF !(llInclRet .AND. lcReport='D' .AND. lcSort<>'I' .AND. Invoice='ZZZZZZ')
     IF lcReport = 'D'
       DO lpDetail
     ELSE
       DO lpSummary
     ENDIF
   ENDIF

   *--Print returns without invoice.
   IF llInclRet .AND. lcSort<>'I' .AND. Invoice='ZZZZZZ' 
     DO lpRetNOInv  
   ENDIF

   xTot1 = xTot1 + xRTot1 
   xTot2 = xTot2 + xRTot2 
   xTot3 = xTot3 + xRTot3 
   xTot4 = xTot4 + xRTot4 

ENDDO

*--Print returns without invoice case of invoice sort.
IF llInclRet .AND. lcSort='I' .AND. Invoice='ZZZZZZ'
  DO lpRetNOInv
  xGTot1 = xGTot1 + xRTot1 
  xGTot2 = xGTot2 + xRTot2  &&B800179,1 MFM 09/20/95 Added.
  xGTot3 = xGTot3 + xRTot3 
  xGTot4 = xGTot4 + xRTot4  &&B800179,1 MFM 09/20/95 Added.
ENDIF
*---- End main report loop ---

*B604755,1 BWA 08/01/2001 Fix the bug Grand total gives Zero value when sorting by salesrep.[START]
IF xTot1 > 0
  XGTOT1 = XGTOT1 + XTOT1
  XGTOT2 = XGTOT2 + XTOT2
  XGTOT3 = XGTOT3 + XTOT3
  XGTOT4 = XGTOT4 + XTOT4
  XGTOT8 = XGTOT8 + XTOT8
  XGTOT9 = XGTOT9 + XTOT9
  STORE 0 TO XTOT1 , XTOT2 , XTOT3 , XTOT4 , XTOT5 , XTOT8 , XTOT9
ENDIF
*B604755,1 BWA 08/01/2001 [END]

*-- GRAND TOTAL part.
ROW=ROW+1
@ ROW,00 SAY REPLICATE('=',132)
ROW = ROW+1
@ ROW,000 SAY '* GRAND TOTAL *'

*B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]
*@ ROW,058 SAY XGTOT1  PICTURE '99999999'
*@ ROW,077 SAY XGTOT2  PICTURE '99999999.99'
*@ ROW,088 SAY XGTOT3  PICTURE '99999999.99'
*@ ROW,099 SAY XGTOT4  PICTURE '99999999.99'

*B605654,1 Shift the price grand total. [Begin]
*@ ROW,054 SAY XGTOT1  PICTURE '99999999'
IF lcReport='S' .OR. ! llInclRet
  @ ROW,054 SAY XGTOT1  PICTURE '99999999'
ELSE
  @ ROW,053 SAY XGTOT1  PICTURE '99999999'
ENDIF  
*B605654,1 Shift the price grand total. [End]

*B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
*@ ROW,072 SAY XGTOT2  PICTURE '999999999.99'
*@ ROW,085 SAY XGTOT3  PICTURE '999999999.99'
*@ ROW,097 SAY XGTOT4  PICTURE '99999999.99'

*B605654,1 Allow to print decimals. [Begin]
*@ ROW,074 SAY XGTOT2  PICTURE '9999999999.99'
*@ ROW,088 SAY XGTOT3  PICTURE '999999999.99'
*@ ROW,100 SAY XGTOT4  PICTURE '99999999.99'

IF llRpDec
  @ ROW,073 SAY XGTOT2  PICTURE '9999999999.99'
  @ ROW,087 SAY XGTOT3  PICTURE '9999999999.99'
  @ ROW,100 SAY XGTOT4  PICTURE '99999999.99'
ELSE
  @ ROW,073 SAY XGTOT2  PICTURE '9999999999999'
  @ ROW,087 SAY XGTOT3  PICTURE '9999999999999' 
  @ ROW,100 SAY XGTOT4  PICTURE '99999999999'
ENDIF
*B605654,1 Allow to print decimals. [End]

*B603713,5 NAD 02/10/2000 (End)


*B603169,1 BWA 03/05/2000 [END]
XGTOT5=XGTOT3-XGTOT4 

*B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]
*@ ROW,110 SAY XGTOT5  PICTURE '99999999.99'
*B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
*@ ROW,109 SAY XGTOT5  PICTURE '999999999.99'

*B605654,1 Allow to print decimals. [Begin]
*@ ROW,112 SAY XGTOT5  PICTURE '999999999.99'
IF llRpDec
  @ ROW,112 SAY XGTOT5  PICTURE '999999999.99'
ELSE
  @ ROW,112 SAY XGTOT5  PICTURE '999999999999'
ENDIF  
*B605654,1 Allow to print decimals. [End]

*B603713,5 NAD 02/10/2000 (End)
XGTOT6=ROUND(IIF(XGTOT3<>0,(XGTOT5/XGTOT3)*100,0),2)
XGTOT6=IIF((XGTOT5<0 .AND. XGTOT3<0),-XGTOT6,XGTOT6)

*@ ROW,123 SAY XGTOT6 PICTURE '99999.99 %'
*B603713,5 NAD 02/10/2000 (START) increase the Price field picture.
*@ ROW,123 SAY XGTOT6 PICTURE '99999.99%'

*B605654,1 Allow to print decimals. [Begin]
*@ ROW,125 SAY XGTOT6 PICTURE '999.99%'
IF llRpDec
  @ ROW,125 SAY XGTOT6 PICTURE '999.99%'
ELSE
  @ ROW,125 SAY XGTOT6 PICTURE '999999%'
ENDIF  
*B605654,1 Allow to print decimals. [End]

*B603713,5 NAD 02/10/2000 (END)
*B603169,1 BWA 03/05/2000 [END]

ROW = ROW+1
@ ROW,00 SAY REPLICATE('=',132)

*B603970,1 BWA 10/19/2000 Add wait window to show the transaction no. while printing.[START]
WAIT CLEAR
*B603970,1 [END]

*!*************************************************************
*! Name      : lpDetail
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : printing report in detail format
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpDetail
*!*************************************************************

PROCEDURE lpDetail

*B603704,1 BWA 06/25/2000 Initial variables to use them in calculation the cost.[START]
IF llRetuV
  PRIVATE lcDiscunta,lcTradeDis,lcTotDisnt
  STORE 0 TO lcDiscunta,lcTradeDis,lcTotDisnt
ENDIF
*B603704,1 [END]


llRetExst=.F.      &&Flag for Existing of the returns.
DO WHILE .T.
  *--Calculate report fields.
  lnQty    = IIF(Type='INV',TotQty,-(TotQty))
  
  *B603115,1 [start]
  *lnGross  = lnQty*Price
  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
  *B603115,1 [end..]
  
  lnNet    = lnGross * (1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))
  
  *C101712,1 A custome process for EF to calculate the gross and the net amount.[START]  
  *B603704,1 BWA 06/25/2000 Comment the calling of the trigger.[START]
  *=gfDoTriger('ARGROSSP',PADR('SALESPRICE',10))
  IF llRetuV
    lnGross    = lnQty*IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea)
    lnMerch    = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
    lcDiscunta = (lnGross - lnMerch) + lnMerch*InvHdr.Discpcnt/100
    lcTradeDis = (lnGross-lcDiscunta)*InvHdr.Trde_Disc/100
    lcTotDisnt = lcDiscunta + lcTradeDis
    lnNet      = lnGross - lcTotDisnt
  ENDIF
  *B603704,1 [END]
  *C101712,1 [END]
  
  *B603115,1 [start]
  *lnCost   = lnQty * Cost 
  *B603387,1 [Start]
  *lnCost   = lnQty * IIF(!llCallGfam,gfAmntDisp(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost) 
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost) 
  *B603115,1 [end..]
  *B603387,1 [End]
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  IF ! llRetExst
    @ ROW,000 SAY Account
    
    *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]    
    *@ ROW,008 SAY SUBSTR(Name,1,IIF(llInclRet,17,21)) &&font "ms sans serif",8
    @ ROW,006 SAY SUBSTR(Name,1,IIF(llInclRet,17,21)) &&font "ms sans serif",8    
    
  ENDIF
  IF llInclRet
  
    *@ ROW,026 SAY Type
    @ ROW,024 SAY Type
    *B603169,1 BWA 03/05/2000 [END]
    
  ENDIF
  
  *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]  
  *@ ROW,030 SAY Code
  *@ ROW,038 SAY Style
  *@ ROW,060 SAY lnQty PICTURE '999999'
  
  @ ROW,028 SAY Code

  *B605654,1 Shift Style and Qtys. [Begin]
  *@ ROW,036 SAY Style
  *@ ROW,056 SAY lnQty PICTURE '999999'
  @ ROW,035 SAY Style
  @ ROW,055 SAY lnQty PICTURE '999999'
  *B605654,1 Shift Style and Qtys. [End]

  *B603169,1 BWA 03/05/2000 [END]
    
  *B603115,1 [start]
  *@ ROW,068 SAY Price PICTURE '99999.99'
  
  *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]  
  *@ ROW,068 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '99999.99'

  *B603656,1 BWA Modifications on the calculations for the custom process to get the price from the style file not from the invline file.[START]
  *@ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '99999.99'    
  IF llRetuV
    *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
    *@ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea) PICTURE '99999.99'

    *B605654,1 Allow to print decimals. [Begin]
    *@ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea) PICTURE '9999999.99'
    IF llRpDec
      @ ROW,062 SAY IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea) PICTURE '9999999.99'
    ELSE
      @ ROW,062 SAY IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea) PICTURE '9999999999'
    ENDIF  
    *B605654,1 Allow to print decimals. [End]

    *B603713,5 NAD 02/10/2000 (End)
  ELSE
    *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
    *@ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '99999.99'

    *B605654,1 Allow to print decimals. [Begin]
    *@ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '9999999.99'
    IF llRpDec
      @ ROW,062 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '9999999.99'
    ELSE
      @ ROW,062 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '9999999999'
    ENDIF  
    *B605654,1 Allow to print decimals. [End]

    *B603713,5 NAD 02/10/2000 (End)
  ENDIF
  *B603656,1 [END]
   
  *B603169,1 BWA 03/05/2000 [END]
  *B603115,1 [end..]
  
  IF lcSubGros = 'Y'
    *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]  
    *@ ROW,079 SAY lnGross PICTURE '999999.99'
    *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
    *@ ROW,072 SAY lnGross PICTURE '999999999.99'
    
    *B605654,1 Allow to print decimals. [Begin]
    *@ ROW,074 SAY lnGross PICTURE '9999999999.99'
    IF llRpDec
      @ ROW,073 SAY lnGross PICTURE '9999999999.99'
    ELSE
      @ ROW,073 SAY lnGross PICTURE '9999999999999'
    ENDIF  
    *B605654,1 Allow to print decimals. [End]
    
    *B603713,5 NAD 02/10/2000 (End)
  ELSE
    IF TYPE='INV'
      *@ ROW,079 SAY lnGross PICTURE '999999.99'
      *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
      *@ ROW,072 SAY lnGross PICTURE '999999999.99'
      
      *B605654,1 Allow to print decimals. [Begin]
      *@ ROW,074 SAY lnGross PICTURE '9999999999.99'
      IF llRpDec
        @ ROW,073 SAY lnGross PICTURE '9999999999.99'
      ELSE
        @ ROW,073 SAY lnGross PICTURE '9999999999999'
      ENDIF
      *B605654,1 Allow to print decimals. [End]

      *B603713,5 NAD 02/10/2000 (End)
    ENDIF   
  ENDIF
  *@ ROW,090 SAY lnNet PICTURE '999999.99'
  *@ ROW,101 SAY lnCost          PICTURE '999999.99'   
  *@ ROW,112 SAY lnNet - lnCost  PICTURE '999999.99'

  *B605654,1 Allow to print decimals. [Begin]
  *@ ROW,088 SAY lnNet PICTURE '999999999.99'    
  *@ ROW,101 SAY lnCost          PICTURE '9999999.99'
  *@ ROW,112 SAY lnNet - lnCost  PICTURE '999999999.99'
  IF llRpDec
    @ ROW,087 SAY lnNet PICTURE '9999999999.99'
    @ ROW,101 SAY lnCost          PICTURE '9999999.99'
    @ ROW,112 SAY lnNet - lnCost  PICTURE '999999999.99'
  ELSE
    @ ROW,087 SAY lnNet PICTURE '9999999999999'
    @ ROW,101 SAY lnCost          PICTURE '9999999999'
    @ ROW,112 SAY lnNet - lnCost  PICTURE '999999999999'
  ENDIF
  *B605654,1 Allow to print decimals. [End]
 
  lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
  lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
  
  *@ ROW,123 SAY lnPercnt PICTURE '99999.99 %'
  *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
  *@ ROW,123 SAY lnPercnt PICTURE '99999.99%'

  *B605654,1 Allow to print decimals. [Begin]
  *@ ROW,125 SAY lnPercnt PICTURE '999.99%'
  IF llRpDec
    @ ROW,125 SAY lnPercnt PICTURE '999.99%'
  ELSE
    @ ROW,125 SAY lnPercnt PICTURE '999999%'  
  ENDIF  
  *B605654,1 Allow to print decimals. [End]

  *B603713,5 NAD 02/10/2000 (End)
  *B603169,1 BWA 03/05/2000 [END]

  *B803410,1 BWA 07/13/2000 Fix the bug of not printing item description.[START]
  llGMA = IIF(TYPE('llGMA') = "U", .F. , llGMA )
  IF llGMA
    IF TYPE='INV' .AND. SEEK(Style,'STYLE') .AND. !EMPTY(Style.Desc1)
      ROW=ROW+1
      @ ROW,038 SAY Style.Desc1
    ENDIF
  ENDIF
  *B803410,1 [END]

  XRTOT1 = XRTOT1 + lnQty
  
  IF lcSubGros = 'Y'
    XRTOT2 = XRTOT2 + lnGross       
  ELSE
    IF TYPE='INV'     
      XRTOT2 = XRTOT2 + lnGross
    ENDIF
  ENDIF

  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost
 
  XAcName=Name
  BREAK  =&lcHBreak
  SKIP


  IF llInclRet
    *B606944,1 - WAB (Start) -if we the Current customer didn 't have any invoice in the selected period and have only return 
    *-- the program acumulate the pervious totals to this customer 
    *B607372,1 ABD - Remove the sort type from the break point. [Begin]
    *IF lcSort='A' AND Break <> lcHBreak
    IF Break <> lcHBreak
      *B607372,1 ABD - [End]
      EXIt
    ENDIF
    *B606944,1 - WAB (end )
    IF Type='RET' .AND. Invoice<>'ZZZZZZ'  &&Return exist for invoice.
      llRetExst = .T.
      ROW = ROW + 1
    ELSE
      EXIT
    ENDIF
  ELSE
    EXIT  
  ENDIF

ENDDO

IF llRetExst
  ROW = ROW + 1
  *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]    
  *@ ROW,062 SAY 'NET AMOUNT AFTER RETURNS  :'
  *@ ROW,090 SAY XRTOT3   PICTURE '999999.99'
  *@ ROW,101 SAY XRTOT4   PICTURE '999999.99'
  *B603713,5 NAD 02/10/2000(Start) increase the Price field picture.
  *@ ROW,060 SAY 'NET AMOUNT AFTER RETURNS  :' 
  @ ROW,060 SAY 'NET AMOUNT AFTER RETURNS :'
  *@ ROW,088 SAY XRTOT3   PICTURE '999999.99'
  
  *B605654,1 Allow to print decimals. [Begin]
  *@ ROW,088 SAY XRTOT3   PICTURE '999999999.99'
  *B603713,5 NAD 02/10/2000(End)
  *@ ROW,101 SAY XRTOT4   PICTURE '9999999.99'
  IF llRpDec
    @ ROW,087 SAY XRTOT3   PICTURE '9999999999.99'
    @ ROW,101 SAY XRTOT4   PICTURE '9999999.99'
  ELSE
    @ ROW,087 SAY XRTOT3   PICTURE '9999999999999'
    @ ROW,101 SAY XRTOT4   PICTURE '9999999999'
  ENDIF
  *B605654,1 Allow to print decimals. [End]

  XRTOT5=XRTOT3-XRTOT4 

  *B605654,1 Allow to print decimals. [Begin]
  *@ ROW,111 SAY XRTOT5  PICTURE '999999999.99'
  IF llRpDec
    @ ROW,112 SAY XRTOT5  PICTURE '999999999.99'
  ELSE
    @ ROW,112 SAY XRTOT5  PICTURE '999999999999'
  ENDIF  
  *B605654,1 Allow to print decimals. [End]

  XRTOT6=ROUND(IIF(XRTOT3<>0,(XRTOT5/XRTOT3)*100,0),2)
  XRTOT6=IIF((XRTOT5<0 .AND. XRTOT3<0),-XRTOT6,XRTOT6)
  
 *@ ROW,123 SAY XRTOT6 PICTURE '99999.99 %'
 *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
 *@ ROW,123 SAY XRTOT6 PICTURE '99999.99%'

  *B605654,1 Allow to print decimals. [Begin]
  *@ ROW,125 SAY XRTOT6 PICTURE '999.99%'
  IF llRpDec
    @ ROW,125 SAY XRTOT6 PICTURE '999.99%'
  ELSE
    @ ROW,125 SAY XRTOT6 PICTURE '999999%'
  ENDIF

 *B603713,5 NAD 02/10/2000 (End)
 *B603169,1 BWA 03/05/2000 [END]
ENDIF
ROW=ROW+1


*!*************************************************************
*! Name      : lpSummary
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : printing report in summary format
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpSummary
*!*************************************************************

PROCEDURE lpSummary

*B603704,1 BWA 06/25/2000 Initial variables to use them in calculation the cost.[START]
IF llRetuV
  PRIVATE lcDiscunta,lcTradeDis,lcTotDisnt
  STORE 0 TO lcDiscunta,lcTradeDis,lcTotDisnt
ENDIF
*B603704,1 [END]


DO WHILE .T.
  *--Calculate report fields.
  lnQty    = IIF(Type='INV',TotQty,-(TotQty))
  
  *B603115,1 [start]
  *lnGross  = lnQty*Price
  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
  *B603115,1 [end..]
  
  lnNet    = lnGross * (1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))
  
  *C101712,1 A custome process for EF to calculate the gross and the net amount.[START]  
  *B603704,1 BWA 06/25/2000 Comment the calling of the trigger.[START]
  *=gfDoTriger('ARGROSSP',PADR('SALESPRICE',10))
  IF llRetuV
    lnGross    = lnQty*IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea)
    lnMerch    = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
    lcDiscunta = (lnGross - lnMerch) + lnMerch*InvHdr.Discpcnt/100
    lcTradeDis = (lnGross-lcDiscunta)*InvHdr.Trde_Disc/100
    lcTotDisnt = lcDiscunta + lcTradeDis
    lnNet      = lnGross - lcTotDisnt
  ENDIF
  *B603704,1 [END]
  *C101712,1 [END]
  
  
  *B603115,1 [start]
  *lnCost   = lnQty * Cost 
  *B603387,1 [Start]
  *lnCost   = lnQty * IIF(!llCallGfam,gfAmntDisp(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost) 
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost) 
  *B603115,1 [end..]
  *B603387,1 []End[
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  XRTOT1 = XRTOT1 + lnQty
  
  IF lcSubGros = 'Y'
    XRTOT2 = XRTOT2 + lnGross       
  ELSE
    IF TYPE='INV'     
      XRTOT2 = XRTOT2 + lnGross       
    ENDIF
  ENDIF

  XRTOT3 = XRTOT3 + lnNet
  XRTOT4 = XRTOT4 + lnCost

  XAcName=Name
  BREAK  =&lcHBreak
  SKIP

  IF llInclRet
    IF ! (Type='RET' .AND. Invoice<>'ZZZZZZ')
      EXIT
    ENDIF

    *B606944,1 - WAB (Start) -if we the Current customer didn 't have any invoice in the selected period and have only return 
    *-- the program acumulate the pervious totals to this customer 
    *B607372,1 ABD - Remove the sort type from the break point. [Begin]
    *IF lcSort='A' AND Break <> lcHBreak
    IF Break <> lcHBreak
      *B607372,1 ABD - [End]
      EXIt
    ENDIF
    *B606944,1 - WAB (End) 
  
  ELSE
    EXIT 
  ENDIF
  
ENDDO


*!*************************************************************
*! Name      : lpSubTotals
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : printing Sub totals
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpSubTotals
*!*************************************************************

PROCEDURE lpSubTotals

*-- Begin Subtotals Loop --
DO WHILE LEN(TRIM(lcHBreak)) <> 0

  IF &lcHBreak = BREAK
     EXIT
  ENDIF

  DO CASE
    CASE lcSort='A'
      lcSubTitl=BREAK+'   '+IIF(lcReport='D',SUBSTR(XAcName,1,26),XAcName)
    CASE lcSort='C'

      *B802059,1 ==> 3- Variable lcSubTitl not found (Sort by Class) [Begin]
      *lcSubTitl=BREAK+'   '+= gfCodDes(&INVLTEMP..CLASS,'CLASS')
      
      *B603315,1 BWA 05/12/1999 Fix the bug of show return transactions[START]      
      *lcSubTitl=BREAK+'   '+gfCodDes(&INVLTEMP..CLASS,'CLASS')
      lcSubTitl=BREAK+'   '+gfCodDes(BREAK,'CLASS')
      *B603315,1 BWA 05/12/1999 [END]
      
      *B802059,1 ==> 3- Variable lcSubTitl not found (Sort by Class) [End  ]
      SELECT &INVLTEMP
      
    *C101795,1 Show the salesrep name in case sorting with the salesrep. [START]
    CASE lcSort='R'
      IF SEEK(BREAK , 'SalesRep')
        lcNamSrep = Salesrep.Name
      ENDIF
      lcSubTitl=BREAK +'  ' + lcNamSrep
    *C101795,1 [END]
    
    OTHE
      lcSubTitl=BREAK
  ENDCASE  

  IF lcReport='D'
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW+1
    @ ROW,000 SAY '* SUB TOTAL *  '+lcSubTitl
  ELSE
    IF XGrdTotO='N'
*      @ ROW,IIF(lcSort='I',38,IIF(lcSort='S',46,0)) SAY ;
            IIF(lcSort='C',' CLASS : ','')+lcSubTitl
     *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]              
     *@ ROW,IIF(lcSort $ 'IS',38,0) SAY ;
            IIF(lcSort='C',' CLASS : ','')+lcSubTitl
            
     *C101795,1 Show the salesrep name in case sorting with the salesrep. [START]
     *@ ROW,IIF(lcSort = 'S',35,IIF(lcSort = 'I', 27 , 0)) SAY ;
            IIF(lcSort='C',' CLASS : ','')+lcSubTitl
     @ ROW,IIF(lcSort = 'S',35,IIF(lcSort = 'I', 27 , 0)) SAY ;
            IIF(lcSort='C',' CLASS : ',IIF(lcSort='R',' Sales Rep : ',''))+lcSubTitl
     *C101795,1 [END]
     
     *B603169,1 BWA 03/05/2000 [END]

    ENDIF
  ENDIF

  IF XGrdTotO='N'
    *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]    
    *@ ROW,058 SAY XTOT1  PICTURE '99999999'
    *@ ROW,077 SAY XTOT2  PICTURE '99999999.99'
    *@ ROW,088 SAY XTOT3  PICTURE '99999999.99'
    *@ ROW,099 SAY XTOT4  PICTURE '99999999.99'
    
    *B605654,1 Shift the pieces. [Begin]
    *@ ROW,054 SAY XTOT1  PICTURE '99999999'
    @ ROW,053 SAY XTOT1  PICTURE '99999999'
    *B605654,1 Shift the pieces. [End]

    *B605654,1 Allow to print decimals. [Begin]
    *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
    *@ ROW,072 SAY XTOT2  PICTURE '999999999.99'
    *@ ROW,074 SAY XTOT2  PICTURE '9999999999.99'
    *B603713,5 NAD 02/10/2000 (End)
    *@ ROW,088 SAY XTOT3  PICTURE '999999999.99'
    *@ ROW,100 SAY XTOT4  PICTURE '99999999.99'

    *XTOT5=XTOT3-XTOT4
    
    *@ ROW,110 SAY XTOT5  PICTURE '99999999.99'
    *@ ROW,112 SAY XTOT5  PICTURE '999999999.99'

    *XTOT6=ROUND(IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0),2)
    *XTOT6=IIF((XTOT5<0 .AND. XTOT3<0),-XTOT6,XTOT6)
    
    *@ ROW,123 SAY XTOT6 PICTURE '99999.99 %'
    *@ ROW,125 SAY XTOT6 PICTURE '999.99%'
   *B603169,1 BWA 03/05/2000 [END]

    IF llRpDec
      @ ROW,073 SAY XTOT2  PICTURE '9999999999.99'
      @ ROW,087 SAY XTOT3  PICTURE '9999999999.99'
      @ ROW,100 SAY XTOT4  PICTURE '99999999.99'
      XTOT5=XTOT3-XTOT4
      @ ROW,112 SAY XTOT5  PICTURE '999999999.99'
      XTOT6=ROUND(IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0),2)
      XTOT6=IIF((XTOT5<0 .AND. XTOT3<0),-XTOT6,XTOT6)
      @ ROW,125 SAY XTOT6 PICTURE '999.99%'
    ELSE
      @ ROW,073 SAY XTOT2  PICTURE '9999999999999'
      @ ROW,087 SAY XTOT3  PICTURE '9999999999999'      
      @ ROW,100 SAY XTOT4  PICTURE '99999999999'
      XTOT5=XTOT3-XTOT4
      @ ROW,112 SAY XTOT5  PICTURE '999999999999'
      XTOT6=ROUND(IIF(XTOT3<>0,(XTOT5/XTOT3)*100,0),2)
      XTOT6=IIF((XTOT5<0 AND XTOT3<0),-XTOT6,XTOT6)
      @ ROW,125 SAY XTOT6 PICTURE '999999%'    
    ENDIF
    *B605654,1 Allow to print decimals. [End]

  ENDIF

  IF lcReport='D'
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('-',132)
  ENDIF 

  IF XGrdTotO='N'
    ROW = ROW+1
  ENDIF

  XGTOT1 = XGTOT1 + XTOT1
  XGTOT2 = XGTOT2 + XTOT2
  XGTOT3 = XGTOT3 + XTOT3
  XGTOT4 = XGTOT4 + XTOT4
  STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5

  BREAK=&lcHBreak

  EXIT
ENDDO
*-- End Subtotals Loop --


*!*************************************************************
*! Name      : lpRetNOInv
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : IF include returns and printing by invoice date
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpRetNOInv
*!*************************************************************

PROCEDURE lpRetNOInv

*B603704,1 BWA 06/25/2000 Initial variables to use them in calculation the cost.[START]
IF llRetuV
  PRIVATE lcDiscunta,lcTradeDis,lcTotDisnt
  STORE 0 TO lcDiscunta,lcTradeDis,lcTotDisnt
ENDIF
*B603704,1 [END]


IF lcReport='D' .AND. IIF(lcSort<>'I',&lcHBreak=BREAK,.T.)
  ROW = ROW + 1
  @ ROW,00 SAY 'RETURNS WITHOUT INVOICES :'       
ENDIF

DO WHILE INVOICE='ZZZZZZ' .AND. IIF(lcSort<>'I',&lcHBreak=BREAK,.T.)

  lnQty    = IIF(TYPE='INV',TOTQTY,-(TOTQTY))
  
  *B603115,1 [start]
  *lnGross  = lnQty*PRICE
  lnGross  = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
  *B603115,1 [end..]
  
  lnNet    = lnGross * (1 - (DISCPCNT/100)) * (1 - (TRDE_DISC/100))

  *C101712,1 A custome process for EF to calculate the gross and the net amount.[START]  
  *B603704,1 BWA 06/25/2000 Comment the calling of the trigger.[START]
  *=gfDoTriger('ARGROSSP',PADR('SALESPRICE',10))
  IF llRetuV
    lnGross    = lnQty*IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea)
    lnMerch    = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
    lcDiscunta = (lnGross - lnMerch) + lnMerch*InvHdr.Discpcnt/100
    lcTradeDis = (lnGross-lcDiscunta)*InvHdr.Trde_Disc/100
    lcTotDisnt = lcDiscunta + lcTradeDis
    lnNet      = lnGross - lcTotDisnt
  ENDIF
  *B603704,1 [END]
  *C101712,1 [END]
  
  *B603115,1 [start]
  *lnCost   = lnQty * COST 
  *B603387,1 [Start] 
  *lnCost   = lnQty * IIF(!llCallGfam,gfAmntDisp(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost) 
  lnCost   = lnQty * IIF(!llCallGfam,lfGetFCurr(Cost, lcRpCurr , ldRpExDate , lcRpTmpNam),Cost) 
  *B603115,1 [end..]
  *B603387,1 [End]
  lnProfit = lnNet - lnCost
  lnProPer = IIF( lnNet<>0 , lnProfit/lnNet , 0)

  IF lcReport='D'
    ROW=ROW+1
    @ ROW,000 SAY Account
    *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]      
    *@ ROW,008 SAY SUBSTR(Name,1,17)
    *@ ROW,026 SAY Type
    *@ ROW,030 SAY Code
    *@ ROW,038 SAY Style
    *@ ROW,060 SAY lnQty PICTURE '999999'
    
    @ ROW,06 SAY SUBSTR(Name,1,17)    
    @ ROW,024 SAY Type
    @ ROW,028 SAY Code
    
    *B605902,1 Shift one char. to the left. [Begin]
    *@ ROW,036 SAY Style
    *@ ROW,056 SAY lnQty PICTURE '999999'
    @ ROW,035 SAY Style
    @ ROW,055 SAY lnQty PICTURE '999999'
    *B605902,1 Shift one char. to the left. [End]
    
    *B603169,1 BWA 03/05/2000 [END]
    
    *B603115,1 [start]
    *@ ROW,068 SAY Price PICTURE '99999.99'

    *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]      
    *@ ROW,068 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '99999.99'
    
    *B603656,1 BWA Modifications on the calculations for the custom process to get the price from the style file not from the invline file.[START]    
    *@ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '99999.99'
    IF llRetuV
      *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
      *@ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea) PICTURE '99999.99'
      
      *B605902,1 Shift one char. to the left. [Begin]
      *@ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea) PICTURE '9999999.99'
      IF llRpDec 
        @ ROW,062 SAY IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea) PICTURE '9999999.99'
      ELSE
        @ ROW,062 SAY IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea) PICTURE '9999999999'
      ENDIF  
      *B605902,1 Shift one char. to the left. [End]
      
      *B603713,5 NAD 02/10/2000 (End)
    ELSE
    *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
      *@ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '99999.99'

      *B605902,1 Shift one char. to the left. [Begin]
      *@ ROW,063 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '9999999.99'
      IF llRpDec 
        @ ROW,062 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '9999999.99'
      ELSE
        @ ROW,062 SAY IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price) PICTURE '9999999999'
      ENDIF  
      *B605902,1 Shift one char. to the left. [End]

    *B603713,5 NAD 02/10/2000 (End)
    ENDIF
    *B603656,1 BWA [END]
    *B603169,1 BWA 03/05/2000 [END]
    
    *B603115,1 [end..]
    
    IF lcSubGros = 'Y'
    
      *B603169,1 Fix the bug of the totals print *****.** in the report.[START]    
      *@ ROW,079 SAY lnGross PICTURE '999999.99'
       *B603713,5 NAD 02/10/2000 (Start)increase the Price field picture.
       *@ ROW,075 SAY lnGross PICTURE '999999.99' 
       
       *B605902,1 Shift one char. to the left. [Begin]
       *@ ROW,074 SAY lnGross PICTURE '9999999999.99' 
       IF llRpDec
         @ ROW,073 SAY lnGross PICTURE '9999999999.99'
       ELSE
         @ ROW,073 SAY lnGross PICTURE '9999999999999'
       ENDIF
       *B605902,1 Shift one char. to the left. [End]

       *B603713,5 NAD 02/10/2000 (End)
      *B603169,1 [END]
      
    ELSE
    
      IF TYPE='INV'
        *B603169,1 Fix the bug of the totals print *****.** in the report.[START]      
        *@ ROW,079 SAY lnGross PICTURE '999999.99'
        *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
        *@ ROW,075 SAY lnGross PICTURE '999999.99'        
               
       *B605902,1 Shift one char. to the left. [Begin]
       *@ ROW,074 SAY lnGross PICTURE '9999999999.99'  
       IF llRpDec
         @ ROW,073 SAY lnGross PICTURE '9999999999.99'
       ELSE
         @ ROW,073 SAY lnGross PICTURE '9999999999999'
       ENDIF
       *B605902,1 Shift one char. to the left. [End]

        *B603713,5 NAD 02/10/2000 (End)
        *B603169,1 [END]
        
      ENDIF
    ENDIF
    *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]      
    *@ ROW,090 SAY lnNet PICTURE '999999.99'
    *@ ROW,101 SAY lnCost          PICTURE '999999.99'   
    *@ ROW,112 SAY lnNet - lnCost  PICTURE '999999.99'
    *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
    *@ ROW,085 SAY lnNet PICTURE '999999.99'
    *@ ROW,99  SAY lnCost          PICTURE '999999.99'   
    *@ ROW,110 SAY lnNet - lnCost  PICTURE '999999.99'
    
    *B605902,1 Apply printing decimals. [Begin]
    *@ ROW,088  SAY lnNet           PICTURE '9999999.99'
    *@ ROW,101 SAY lnCost           PICTURE '9999999.99'   
    *@ ROW,111 SAY lnNet - lnCost   PICTURE '999999999.99'
    IF llRpDec
      @ ROW,087 SAY lnNet           PICTURE '9999999999.99'
      @ ROW,101 SAY lnCost          PICTURE '9999999.99'
      @ ROW,112 SAY lnNet - lnCost  PICTURE '999999999.99'
    ELSE
      @ ROW,087 SAY lnNet           PICTURE '9999999999999'
      @ ROW,101 SAY lnCost          PICTURE '9999999999'
      @ ROW,112 SAY lnNet - lnCost  PICTURE '999999999999'
    ENDIF        
    *B605902,1 Apply printing decimals. [End]
        
    *B603713,5 NAD 02/10/2000 (End)
    lnPercnt = ROUND(IIF(lnNet <> 0,((lnNet - lnCost)/lnNet)*100,0),2)
    lnPercnt = IIF(((lnNet - lnCost)<0 .AND. lnNet<0),-lnPercnt,lnPercnt)
    
    *@ ROW,123 SAY lnPercnt PICTURE '99999.99 %'
    *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
    *@ ROW,123 SAY lnPercnt PICTURE '99999.99%'
    
    *B605902,1 Apply printing decimals. [Begin]
    *@ ROW,125 SAY lnPercnt PICTURE '999.99%'
    IF llRpDec
      @ ROW,125 SAY lnPercnt PICTURE '999.99%'
    ELSE
      @ ROW,125 SAY lnPercnt PICTURE '999999%'  
    ENDIF
    *B605902,1 Apply printing decimals. [End]
    
    *B603713,5 NAD 02/10/2000 (End)
    *B603169,1 BWA 03/05/2000 [END]
    
    *B803410,1 BWA 07/13/2000 Fix the bug of not printing item description.[START]
    llGMA = IIF(TYPE('llGMA') = "U", .F. , llGMA )
    IF llGMA
      IF SEEK(Style ,'STYLE') .AND. !EMPTY(Style.Desc1)
        ROW=ROW+1
        @ ROW,038 SAY Style.Desc1
      ENDIF
    ENDIF
    *B803410,1 [END]
    
  ENDIF
    
  xRTot1 = xRTot1 + lnQty
  IF lcSubGros = 'Y'
    xRTot2 = xRTot2 + lnGross
  ELSE
    IF TYPE='INV'     
      xRTot2 = xRTot2 + lnGross
    ENDIF
  ENDIF
  xRTot3 = xRTot3 + lnNet
  xRTot4 = xRTot4 + lnCost
  
  xRTot5 = xRTOT3 - XRTOT4
  xRTot6 = ROUND(IIF(XRTOT3<>0,(XRTOT5/XRTOT3)*100,0),2)
  xRTot6 = IIF((XRTOT5<0 .AND. XRTOT3<0),-XRTOT6,XRTOT6)
  
  BREAK=&lcHBreak

 SKIP
ENDDO  

ROW = ROW + IIF(lcReport='D',1,0)
IF (xRTot1+xRTot3<>0 .AND. lcSort='I'.AND. lcReport='S')
  ROW = ROW + 1
  @ ROW,00 SAY 'RETURNS WITHOUT INVOICES :'       
  *B603169,1 BWA 03/05/2000 Fix the bug of the totals print *****.** in the report.[START]    
  *@ ROW,060 SAY xRTot1 PICTURE '999999'
  *@ ROW,079 SAY xRTot2 PICTURE '999999.99'
  *@ ROW,090 SAY xRTot3 PICTURE '999999.99'

  *@ ROW,101 SAY xRTot4 PICTURE '999999.99'
  *@ ROW,112 SAY xRTot5 PICTURE '999999.99'
  *@ ROW,123 SAY xRTot6 PICTURE '99999.99 %'

  *B605902,1 Shift one char. to the left. [Begin]
  *@ ROW,056 SAY xRTot1 PICTURE '999999'
  @ ROW,055 SAY xRTot1 PICTURE '999999'
  *B605902,1 Shift one char. to the left. [End]

  *B603713,5 NAD 02/10/2000 (Start) increase the Price field picture.
  *@ ROW,075 SAY xRTot2 PICTURE '999999.99'
  *@ ROW,088 SAY xRTot3 PICTURE '999999.99'
  *@ ROW,99  SAY xRTot4 PICTURE '999999.99'
  *@ ROW,112 SAY xRTot5 PICTURE '999999.99'
  *@ ROW,123 SAY xRTot6 PICTURE '99999.99%'
  
  *B605902,1 Apply printing decimals. [Begin]
  *@ ROW,074 SAY xRTot2 PICTURE '999999999.99'
  *@ ROW,088 SAY xRTot3 PICTURE '999999999.99'
  *@ ROW,101  SAY xRTot4 PICTURE'999999999.99'
  *@ ROW,111 SAY xRTot5 PICTURE  '99999999.99'
  *@ ROW,125 SAY xRTot6 PICTURE      '999.99%'
  IF llRpDec
    @ ROW,073 SAY xRTot2 PICTURE '9999999999.99'
    @ ROW,087 SAY xRTot3 PICTURE '9999999999.99'
    @ ROW,101 SAY xRTot4 PICTURE '9999999.99'
    @ ROW,112 SAY xRTot5 PICTURE '999999999.99'
    @ ROW,125 SAY xRTot6 PICTURE      '999.99%'
  ELSE
    @ ROW,073 SAY xRTot2 PICTURE '9999999999999'
    @ ROW,087 SAY xRTot3 PICTURE '9999999999999'
    @ ROW,101 SAY xRTot4 PICTURE '9999999999'
    @ ROW,112 SAY xRTot5 PICTURE '999999999999'
    @ ROW,125 SAY xRTot6 PICTURE      '999999%'
  ENDIF
  *B605902,1 Apply printing decimals. [End]

  *B603713,5 NAD 02/10/2000 (End)
  *B603169,1 BWA 03/05/2000 [END]
ENDIF


*!*************************************************************
*! Name      : lfwCurCode
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B603115,1 New when function to save the old currency code.

FUNCTION lfwCurCode

lcOldCurr = laOGFxFlt[2,6]

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B603115,1 New valid function to validate the currency code.

FUNCTION lfvCurCode
lnAlias=SELECT(0)


*-- Allow blank curreny in case user have no selected forign currency.
IF EMPTY(laOGFxFlt[2,6]) .AND. lcRpCurr <> 'F' 
  RETURN
ENDIF

IF !SEEK(laOGFxFlt[2,6],'SYCCURR') .OR. ATC("?",laOGFxFlt[2,6]) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    laOGFxFlt[2,6] = lcOldCurr
  ELSE
    laOGFxFlt[2,6] = laTemp[1]
  ENDIF
ENDIF

SHOW GET laOGFxFlt[2,6]

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : This function called from the currency Display
*!             field to display currency options screen .
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B603115,1 
*
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)


*-- If forgin currency you must be sure invoice currency has value.
IF lcRpCurr = 'F' .AND. EMPTY(laOGFxFlt[2,6])
  laOGFxFlt[2,6]= gcBaseCurr
ENDIF
SHOW GET laOGFxFlt[2,6]


*!*************************************************************
*! Name      : lfRepShow
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B603115,1 
*
FUNCTION lfRepShow

laOGFxFlt[2,6]= gcBaseCurr
*laOGObjCnt[11] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")

*B603387,1  (Start) 
*!********************************************************************
*! Name      : lfGetFCurr
*! Developer : Sameh Aldesouki (SAM)
*! Date      : 01/16/2000
*! Purpose   : Return the Foreign amount From Base currency.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : lcRpDispCur  && The way to display the amount.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcTmepFile   && The temp file name that hold the temp. 
*!           :                 exchange rates.
*!           : llAprvCurr   && If you are using the Approved currency.
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
*! Example   : lfGetFCurr(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.).
*!********************************************************************
FUNCTION lfGetFCurr
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile

PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)
IF lcRpDispCur = 'F'
  lnExRate   = 0
  lnUnit     = 0
  
  IF EMPTY(lcGetFile)
    lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
  ELSE
    lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
  ENDIF
  
  IF lcCurrCode = gcBaseCurr
    lnExRate = 1
    lnUnit   = 1
  ELSE
    
    ldExRateDt=DATE
*    lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
    lnExRate = &INVLTEMP..Nexrate   
    lnUnit   = &INVLTEMP..Ncurrunit      
  ENDIF
  
  lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
  lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
  
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
  lcExSin1 = IIF(lcExSin1 = '/','*','/')
  lcExSin2 = IIF(lcExSin2 = '*','/','*')
  
  lnAmount = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
  
ENDIF  
  
SELECT (lnSavAlias)
RETURN lnAmount
*-- end of lfGetFCurr.
*B603387,1(End)

*B802858,1 BWA 03/05/2000 Fix the bug of the OK bottom get an error Massage.[START]
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 03/03/2000
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPbOk()
*!*************************************************************

FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  ldRPPLDate = ldFrom
  ldRPPHDate = ldTo
  CLEAR READ
ENDIF
*B802858,1 BWA 03/05/2000 [END]

*E301432,1 BWA 07/10/2000 Add a new filter to the criteria, which is posting date.[START]
*!*************************************************************
*! Name      : lfvPostDat
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/26/2000
*! Purpose   : Add the posting date filter to the filter if selected.
*!*************************************************************
*! Called from : ARGROSSP.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPostDat()
*!*************************************************************
FUNCTION lfvPostDat
PRIVATE lcDatPost

STORE  "" TO lcDatPost

lnDataPos = ASCAN(laOgVrFlt,'Post Date')
IF lnDataPos > 0
  lnDataPos  = ASUBSCRIPT(laOgVrFlt,lnDataPos,1)
  DO CASE
    CASE AT('Like',laOgVrFlt[lnDataPos,5])             <> 0       && Like
      lnDatPost = AT('(DTOS(Post Date)',LCRPEXP)

    CASE AT('Greater Than',laOgVrFlt[lnDataPos,5])     <> 0       && Greater Than
      lnDatPost = AT('(DTOS(Post Date)',LCRPEXP)

    CASE AT('Less Than',laOgVrFlt[lnDataPos,5])        <> 0       && Less Than
      lnDatPost = AT('(DTOS(Post Date)',LCRPEXP)

    CASE AT('Greater Or Equal',laOgVrFlt[lnDataPos,5]) <> 0       && Greater Or Equal
      lnDatPost = AT('(DTOS(Post Date)',LCRPEXP)

    CASE AT('Less Or Equal',laOgVrFlt[lnDataPos,5])    <> 0       && Less Or Equal
      lnDatPost = AT('(DTOS(Post Date)',LCRPEXP)

    CASE AT('Between',laOgVrFlt[lnDataPos,5])          <> 0       && Between
      lnDatPost = AT('(BETWEEN(DTOS(Post Date)',LCRPEXP)
      
    CASE AT('In List',laOgVrFlt[lnDataPos,5])          <> 0       && In List
      lnDatPost = AT('(INLIST(DTOS(Post Date)',LCRPEXP)
  ENDCASE

  IF lnDatPost > 0
    lnPos1     = AT('AND' , SUBSTR(LCRPEXP,lnDatPost))
    IF lnPos1 > 0
      lcDatPost = SUBSTR(lcRpExp ,lnDatPost , lnPos1-1)
    ELSE
      lcDatPost = SUBSTR(lcRpExp ,lnDatPost)
    ENDIF
  ENDIF
  
  *--Case invhdr file
  lcDatPost = STRTRAN(lcDatPost,"Post Date" , "INVHDR.DPOSTDATE")
  IF !EMPTY(lcDatPost)
    XFILTER  = XFILTER  + IIF(EMPTY(lcDatPost) ,""," .AND. "  ) + lcDatPost
  ENDIF

  lcDatPost = STRTRAN(lcDatPost,"INVHDR." , "RETHDR.")
  IF !EMPTY(lcDatPost)
    XRFILTER = XRFILTER + IIF(EMPTY(lcDatPost) ,""," .AND. "  ) + lcDatPost
  ENDIF
ENDIF

*-- End of lfvPostDat
*E301432,1 [END]


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/26/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

SET ORDER TO CODES    IN CODES
SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO SALESREP IN SALESREP
SET ORDER TO STYLE    IN STYLE
SET ORDER TO INVHDR   IN INVHDR
SET ORDER TO INVLINES IN INVLINE

IF llRetModu
  SET ORDER TO RETHDRA  IN RETHDR
  SET ORDER TO RETLINES IN RETLINE
ENDIF

*B604674,1 BWA 07/16/2001 Fix the of Advanced button get error when the company isn't link to Gl.[START]
IF ALLTRIM(gfGetMemVar('M_LINK_GL')) <>  'Y'
  SHOW GET pbAdvanced DISABLE
ENDIF
*B604674,1 BWA 07/16/2001 [END]

= lfvSumDet()

*B604360,1 BWA 04/22/2001 Fix bug "lfvaccount" not found.[START]
*!*************************************************************
*! Name      : lfvAccount
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/2001
*! Purpose   : Validate function for the Account.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvAccount()
*!*************************************************************
FUNCTION lfvAccount

lcAccount = lcRngVarNa
IF LASTKEY() = 13 AND !MDOWN()
  XACCOUNT = &lcAccount
  IF !SEEK('M'+XACCOUNT,'Customer')) 
    DO CUSBROWM WITH XACCOUNT
    &lcAccount = XACCOUNT
  ENDIF
ENDIF

RETURN &lcAccount

*--End of lfvAccount.
*B604360,1 [END]