*:************************************************************************
*:
*: Procedure file: APAPRIN.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: AP
*:         Author: Renee Ezzat
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:  Documented      /  /
*:************************************************************************
*B600492,1 Reham 06/22/95 
*B600492,1 Change the width of invoice no. & vendor code in the browse.
*E300258,4 MAN 06/29/95 Add the check on supressing 1099 processing
*B600624,1 REN 08/14/95 Fix the problem of miscalculating the number of
*B600624,1              installments such that sometimes there is one
*B600624,1              extra installment.
*B600630,1 REN 08/15/95 Store Bank code, Checking account and G/L
*B600630,1              checking account into APINVHDR.     
*B600650,1 ORABY 08/17/95 The number of installments is not calculated correctly
*B601526,1  HS  01/20/97  Add the new field [nInvFAAp] in the APINVHDR file 
*B601526,1                [the approved amount in the approve currency] 
*E300643,1  HS 04/14/97 1)Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300643,1              2)Make some changes for we have change the function
*E300643,1              [gfCodDes]
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E300692,1 ESSMAT 06/29/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E301077,80 IHB 03/04/1999 Enhance opening and closing files 
*E300789,4 IHB  07/03/1999 Remove company ID from ACCOD, FISHD, FSPRD, FSHLD, CODES
*:************************************************************************

DECLARE laBrHdrs[12],laTemp[1], laPayMeth[1,2]

** lcTAllCode    comes from codes file ('All')
** lcAprInCh1    Child window 1 of gcBaseWind
** lcAprInCh2    Child window 2 of gcBaseWind
** lcAprInCh3    Child window 3 of gcBaseWind
** lcBrInvTtl    browse window title
** lc_Invoices   temporary file that holds generated installments
** lcVendTtl     vendor summary window title
** lcInvoice     invoice summary window title
** lcVenSum      Variable to hold  
** lcInvSum      Variable to hold 
** lcDivision    Objects from the selection screen
** lcDivOpt      Variable to hold   
** lcPayPrior    Variable to hold 
** lcPayMeth     Variable to hold 
** lcPMethOpt    Variable to hold 
** lcVendComp    Variable to hold  
** lcCurrYear    Current fiscal year
** lcMainFlt     Variable to hold  
** lcInvFlt      Variable to hold  
** lcOldVal      character type old values variable
** lcSlPr        Variable to hold 
** lcTBrowse     Variable to hold  
** lcGenStat     state of display of generate button
** lcAprStat     state of display of screen objects
** laCtrStat     To disable the browse pad in the menu
** lcColPair     Variable to hold 
** lcVendCode    Variable to hold  
** lcInvRef      Variable to hold  
** lcPayPrior    Variable to hold  
**
** lnTime        holds tiem value in seconds (for mouse trap)
** lnBrRecNo     current record number
** lnPayMLen     width of payment method popup
** lnOldrbScp    old radio button value
** lnOldVal      old value for numeric type fields
** lnTotPay      vendor's total approved amount
** lnTotDisc     vendor's total approved discount
** lnTotAdj      vendor's total approved adjustments
**
** ldGenDate     generation date
** ldOldVal      old value for date type fields          
** ldDueFrom     due date from object in APAPRINI.SPR
** ldDueTo       due date to object in APAPRINI.SPR
** ldDiscFrom    discount date from object in APAPRINI.SPR
** ldDiscTo      discount date to object in APAPRINI.SPR
**
** llBrowse      Variable to hold  
** llUpdated     .T. if selection criterion is updated (APAPRINI.SPR)
** llMayGenr     .T. if generation button is enabled
** llNoContrl    .T. if there is no control panel included in the READ
**
** rbScope       invoices selection radio button in the main screen
** rbDates       due/discount dates button in APAPRINI screen
** puPayMeth     payment method popup under WINDOWS in APAPRINI screen
** puDivision    divisions popup under WINDOWS in APPRINI screen
** puDivision    Variable to hold  

STORE ''          TO lcTAllCode , lcAprInCh1 , lcAprInCh2 , lcAprInCh3 ,;
                     lcBrInvTtl , lc_Invoices, lcVendTtl  , lcInvoice  ,;
                     lcVenSum   , lcInvSum   , lcDivision , lcDivOpt   ,;
                     lcPayMeth  , lcPMethOpt , lcVendComp , lcOldVal   ,;
                     puDivision 

STORE {}          TO ldGenDate  , ldOldVal   , ldDueFrom  , ldDueTo    ,;
                     ldDiscFrom , ldDiscTo
STORE .F.         TO llBrowse
STORE .T.         TO llUpdated  , llMayGenr  , llNoContrl
STORE 0           TO lnTime     , lnBrRecNo  , lnPayMLen  , lnOldVal   ,;
                     lnTotPay   , lnTotDisc  , lnTotAdj
STORE 1           TO rbScope    , rbDates    , puPayMeth  , lnOldrbScp

STORE SPACE(1)    TO lcPayPrior
STORE SPACE(8)    TO lcVendCode
STORE SPACE(15)   TO lcInvRef
STORE 'ENABLE'    TO lcGenStat 
STORE 'DISABLE'   TO lcAprStat 
STORE 'DISABLE'   TO laCtrStat  
STORE '\<Select'  TO lcSlPr
STORE '\<Browse'  TO lcTBrowse
STORE SCHEME(1,2) TO lcColPair

*E301077,80 IHB 03/04/1999 opend needed files only [start]
*-- llSYCCOMP, llFSPRD : are used to identify whether the files are used or not
STORE .F. TO llSYCCOMP, llFSPRD
*E301077,80 IHB 03/04/1999 opend needed files only [end]

*E301077,80 IHB 03/04/1999 Move opening of this file after gfSetup() [start]
*lcCurrYear  = IIF(SEEK(gcPrnt_Cmp,'SYCCOMP'),SYCCOMP.cCurr_Yer," ")
*E301077,80 IHB [end]

lcMainFlt   = [cAutMType + cVendCode + cInvNo = 'I'] 
lcInvFlt    = lcMainFlt

IF !gfSetup()
  RETURN
ENDIF

*E301077,80 IHB 03/04/1999 opend needed files only [start]
llSycComp   = gfSysOpen(gcSYSHome+'SYCCOMP'  ,'Ccomp_id','SH')
IF SEEK(gcPrnt_Cmp,'SYCCOMP')
  lcCurrYear  = SYCCOMP.cCurr_Yer
  lcDataDir   = gfGetDataDir(ALLT(SYCCOMP.cCom_DDir))
ELSE
  lcCurrYear  = " "
  lcDataDir   = gcDataDir
ENDIF
IF llSycComp .AND. USED('SYCCOMP')
  =gfSysClose('SYCCOMP')
ENDIF
*-- closing SYCCOMP will be after idetifying parent company, down
*E301077,80 IHB 03/04/1999 opend needed files only [end]

*** Check if there are any installments in the automatic header file
*** If there are not, present the following message and exit the program
SELECT APINVAHD
SET ORDER TO TAG TVENDINV
IF !SEEK('I')
  *** Message: " There are no ð.          "
  *** Choices: "          ® Ok ¯          "
  =gfModalGen("TRM04101B00000","DIALOG", "installments to approve")
  glQuitting = .T.
  RETURN
ENDIF

ldCurDate   = DATE()
llVendExst  = .T.

DECLARE laWndObj[4,3], laFields[2,3]

IF !WEXIST(gcBaseWind)
  
  *** Get temporary names for the child windows
  lcAprInCh1  = gfTempName()
  lcAprInCh2  = gfTempName()
  lcAprInCh3  = gfTempName()
  lc_Invoices = gfTempName()      && temp file
  lcVenSum    = 'CWR'+SUBSTR(gfTempName(),2)
  lcInvSum    = 'CWR'+SUBSTR(gfTempName(),2)

  *** Get payment methods array and add an 'All' to the array (.T.)
  lnPayMLen = gfGetVld('CVENPMETH', @laPayMeth, .T.)
  *** Remove 'Credit cards' (C) option from the array
  lcSetExact = SET('EXACT')
  SET EXACT ON
  lnElem = ASCAN(laPayMeth, 'C')
  IF lnElem > 0 
    =ADEL(laPayMeth, ASUBSCRIPT(laPayMeth, lnElem, 1))
    DIMENSION laPayMeth[ALEN(laPayMeth,1)-1,2]  
  ENDIF
  SET EXACT &lcSetExact

  *** Create invoices temporary file as follows
  CREATE TABLE (gcWorkDir+lc_Invoices) ;
    (cInclude  C( 3)   ,;
     cInvNo    C(12)   ,;
     cVendCode C( 8)   ,;
     nNoOfInst N( 4, 0),;
     nInstAmnt N(15, 2),;
     nDisAmnt  N(10, 2),;
     n1099Amnt N(15, 2),;
     nOpenAmnt N(15, 2),;
     nAprAmnt  N(15, 2),;
     cTimStamp C(FSIZE('cAdd_User',lcBaseFile) + 10 + ;
                 FSIZE('cAdd_Time',lcBaseFile)))
  
  *** Default the generation date to the system date   
  ldGenDate   = gdSysDate          && generation date 
  lcPayMeth   = laPayMeth[1,1]

  *E300643,1 Change this line for the changes we have made 
  *          to (gfCodDes) [Begin]
  *lcTAllCode  = gfCodDes('*')
  lcTAllCode  = gfCodDes('*' , ' ')
  *E300643,1 Change this line [End]
  
  lcDivision  = lcTAllCode    
  
ELSE
  rbScope = lnOldrbScp
ENDIF


laWndObj [1,1] = lcAprInCh1
laWndObj [1,2] = "ldGenDate"
laWndObj [1,3] = "pbClose"

laWndObj [2,1] = lcAprInCh3
laWndObj [2,2] = "ldGenDate"
laWndObj [2,3] = "pbClose"

laWndObj [3,1] = lcVenSum
laWndObj [3,2] = "pbVeClose"
laWndObj [3,3] = "pbVeClose"

laWndObj [4,1] = lcInvSum
laWndObj [4,2] = "pbInClose"
laWndObj [4,3] = "pbInClose"

*** Array laFields is formed as follows :
*** A row for every field to be browsed
*** Column 1 : the object name corresponding to the field
*** Column 2 : the physical field name
*** Column 3 : the tag name to be used for seeking
laFields[1,1] = 'lcVendCode'         &&'lcVendCode'
laFields[1,2] = 'cVendCode'
laFields[1,3] = 'VENCODE'
laFields[2,1] = 'lcVendComp'         && 'lcVenComp'
laFields[2,2] = 'cVenComp'
laFields[2,3] = 'VENCOMP'

*** Prepare a fields popup for divisions from the
*** codes file
IF _WINDOWS

  *E300643,1 Change this lines for the changes we have made to 
  *          SYCCODES [Begin]
  *lcDivision = SYCCODES.cdiscrep
  *DEFINE POPUP puDivision PROMPT FIELD SYCCODES.cdiscrep SCROLL;
  *  FROM 5.63,12.88 TO 9.73,46.0;
  *  MESSAGE gfObj_msg()

  lcDivision = CODES.cdiscrep
  DEFINE POPUP puDivision PROMPT FIELD CODES.cdiscrep SCROLL;
    FROM 5.63,12.88 TO 9.73,46.0;
    MESSAGE gfObj_msg()

  *E300643,1 Change this lines [End]
  
  ON SELECTION POPUP puDivision DO lfvDivision
ENDIF  

lcCheck        = ' û '
lcUncheck      = SPACE(3) 

*** Set the tag of the payable invoices file to VENDINV
*** tag expression : cVendCode + cInvNo
SET ORDER TO TAG VENDINV IN APINVHDR

*** Set the tag of the vendors file to VENCODE
*** tag expression : cVendCode 
SET ORDER TO TAG VENCODE IN APVENDOR

*** Set the tag of the vendor history file to VENDYEAR
*** tag expression : cVendCode + cFisFYear
SET ORDER TO TAG VENDYEAR IN APVENHST

*** Set filter to cdivision fields in SYCCODES file
*** for the divisions popup in the invoice selection screen.

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SET ORDER  TO 0

*E300789,4 IHB Remove company ID [start]
*SET FILTER TO CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp+'N'+'CDIVISION';
           .OR. CCOMP_ID+CRLTFIELD+CFLD_NAME = '  '+'N'+'N/A';
           .OR. CCOMP_ID+CRLTFIELD+CFLD_NAME = '  '+'N'+'ALL'
SET FILTER TO CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'CDIVISION';
           .OR. CRLTFIELD+CFLD_NAME = 'N'+'N/A';
           .OR. CRLTFIELD+CFLD_NAME = 'N'+'ALL'
*E300789,4 IHB [start]

SET ORDER TO TAG IDRLTFNAME

*** Set the tag of the automatic invoice header file to TVENDINV
*** tag expression : cAutMType + cVendCode + cInvNo
SELECT APINVAHD
SET ORDER TO 0
SET FILTER TO
SET FILTER TO EVALUATE(lcInvFlt) 
SET ORDER TO TAG TVENDINV
SET RELATION TO cVendCode INTO APVENDOR ADDITIVE
SET RELATION TO cVendCode + cInvNo INTO APINVHDR ADDITIVE
SET RELATION TO cVendCode + lcCurrYear INTO APVENHST ADDITIVE

SELECT (lc_Invoices) 
SET RELATION TO 'I' + cVendCode + cInvNo INTO APINVAHD

*** Prepare key settings for the browse
PUSH KEY
lcOnCtrlEntr  = ON("KEY","CTRL+ENTER")
lcOnEsc       = ON("KEY","ESC")
ON KEY LABEL TAB         DO lpTab      
ON KEY LABEL BACKTAB     DO lpShiftTab 

*E300683,1 Call *.SPR from screens directory
* DO APAPRIN.SPR 
DO (gcScrDir + gcWinAppl + '\APAPRIN.SPR')
*E300683,1 end          

RELEASE PAD _BROWSE OF _MSYSMENU
POP KEY

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SET FILTER TO

SELECT APINVAHD
SET RELATION OFF INTO APVENDOR
SET RELATION OFF INTO APINVHDR
SET RELATION OFF INTO APVENHST 
SET FILTER TO

IF glQuitting
  SELECT APINVAHD

  IF USED(lc_Invoices)
    USE IN (lc_Invoices)
  ENDIF

  IF WVISIBLE(lcVenSum)
    HIDE WINDOW (lcVenSum)
  ENDIF  
  IF WVISIBLE(lcInvSum)
    HIDE WINDOW (lcInvSum)
  ENDIF  
  HIDE WINDOW (gcBaseWind)

  RELEASE WINDOWS (lcVenSum), (lcInvSum)
  RELEASE POPUPS puDivision
  
  ERASE (gcWorkDir+lc_Invoices+'.DBF')  
  ERASE (gcWorkDir+lc_Invoices+'.CDX')  
ENDIF
RELEASE POPUPS puDivision
RELEASE WINDOWS (lcBrInvTtl)
glFromBrow  = .F.

*!**************************************************************************
*!
*!        Function : lfModalShow
*!
*!**************************************************************************
* Show function called from the modal screen of 'Selected invoices',
* called upon selection from radio button scope in the main window
FUNCTION lfModalShow
IF _DOS
  RELEASE PAD _BROWSE OF _MSYSMENU
  DEFINE PAD _BROWSE OF _MSYSMENU PROMPT lcTBrowse
ENDIF


*!**************************************************************************
*!
*!        Function : lfwOldVals
*!
*!**************************************************************************
FUNCTION lfwOldVals
PARAMETERS lcOldObjNm
&lcOldObjNm = EVALUATE(SYS(18))

*!**************************************************************************
*!
*!        Function : lfwOldBrVals
*!
*!**************************************************************************
FUNCTION lfwOldBrVals
PARAMETERS lnFldContent
lnOldVal = lnFldContent

*!**************************************************************************
*!
*!        Function : lfBrowInv
*!
*!**************************************************************************
*  browse function fro the main window's browse
FUNCTION lfBrowInv
PRIVATE lcClrSchm

lcClrSchm  = IIF(_DOS," COLOR SCHEME 13","NOMENU")

SELECT (lc_Invoices)
lnBrRecNo  = RECNO()

*B600492,1 Change the width of invoice no. & vendor code.
*E300258,4 MAN Add the check on supressing 1099 processing
*E300258,4 MAN Marked the browse coompletely and made new browse
*E300258,4 MAN using variable to hold the browse fields to be able
*E300258,4 MAN to control the existance of the 1099 field
*BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecNo,'',' '):H=' ':1:W=.F.,; 
              cInclude:H=laBrHdrs[1]:R,;
              APINVAHD.cInvNo:12:H=laBrHdrs[2]:R,;
              APINVAHD.cVendCode:8:H=laBrHdrs[3]:R,;
              nNoOfInst:H=laBrHdrs[4]:R,;
              nInstAmnt:H=laBrHdrs[5]:W=lfwOldBrVals(nInstAmnt);
              :V=lfvBrAmnts('nInstAmnt', nInstAmnt + nDisAmnt,;
               nOpenAmnt, lcTApr, lcTOpenAmt),;
              nDisAmnt:H=laBrHdrs[6]:W=lfwOldBrVals(nDisAmnt);
              :V=lfvBrAmnts('nDisAmnt', nInstAmnt + nDisAmnt,;
               nOpenAmnt, lcTApr, lcTOpenAmt),;
              n1099Amnt:H=laBrHdrs[7]:W=lfwOldBrVals(n1099Amnt);
              :V=lfvBrAmnts('n1099Amnt', n1099Amnt,;
               nInstAmnt, lcT1099, lcTAprInst),;
              nOpenAmnt:H=laBrHdrs[8]:R,; 
              nAprAmnt :H=laBrHdrs[9]:R,;  
              APINVAHD.cBnkCode:H=laBrHdrs[10]:R,; 
              APINVAHD.cChkAcct:H=laBrHdrs[11]:R,; 
              APINVAHD.cChkGlAcc:H=laBrHdrs[12]:R ; 
         WINDOW (lcAprInCh2) ;
         IN WINDOW (gcBaseWind) ;
         LOCK 0;
         NOAPPEND;
	     NOCLEAR;
         NODELETE;
         NOWAIT;
         SAVE;
 	     TITLE lcBrInvTtl;
         WHEN lfwBroWhen(); 
 	     VALID :F lfvBrowse();
         &lcClrSchm

lcBrsFlds = "cMarker=IIF(RECNO()=lnBrRecNo,'',' '):H=' ':1:W=.F.,"+; 
              "cInclude:H=laBrHdrs[1]:R,"+;
              "APINVAHD.cInvNo:12:H=laBrHdrs[2]:R,"+;
              "APINVAHD.cVendCode:8:H=laBrHdrs[3]:R,"+;
              "nNoOfInst:H=laBrHdrs[4]:R,"+;
              "nInstAmnt:H=laBrHdrs[5]:W=lfwOldBrVals(nInstAmnt)"+;
              ":V=lfvBrAmnts('nInstAmnt', nInstAmnt + nDisAmnt,"+;
              "nOpenAmnt, lcTApr, lcTOpenAmt),"+;
              "nDisAmnt:H=laBrHdrs[6]:W=lfwOldBrVals(nDisAmnt)"+;
              ":V=lfvBrAmnts('nDisAmnt', nInstAmnt + nDisAmnt,"+;
              " nOpenAmnt, lcTApr, lcTOpenAmt),"+;
IIF(llApS1099,"","n1099Amnt:H=laBrHdrs[7]:W=lfwOldBrVals(n1099Amnt):V=lfvBrAmnts('n1099Amnt', n1099Amnt,nInstAmnt, lcT1099, lcTAprInst),")+;
              "nOpenAmnt:H=laBrHdrs[8]:R,"+; 
              "nAprAmnt :H=laBrHdrs[9]:R,"+;  
              "APINVAHD.cBnkCode:H=laBrHdrs[10]:R,"+; 
              "APINVAHD.cChkAcct:H=laBrHdrs[11]:R,"+; 
              "APINVAHD.cChkGlAcc:H=laBrHdrs[12]:R"  

BROWSE FIELDS &lcBrsFlds; 
       WINDOW (lcAprInCh2) ;
       IN WINDOW (gcBaseWind) ;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOWAIT;
       SAVE;
 	   TITLE lcBrInvTtl;
       WHEN lfwBroWhen(); 
       VALID :F lfvBrowse();
       &lcClrSchm





GO TOP  
SHOW WINDOW (lcBrInvTtl) REFRESH

*!**************************************************************************
*!
*!        Function : lfwBroWhen
*!
*!**************************************************************************
* 
FUNCTION lfwBroWhen
glFromBrow  = .T.
lnBrRecNo   = RECNO()
IF !EMPTY(cInclude)
  lcSlPr  = lcTUnSelect
  SHOW GET pbSelect,1 PROMPT lcTUnSelect
ELSE
  lcSlPr  = lcTSelect
  SHOW GET pbSelect,1 PROMPT lcTSelect
ENDIF

IF WVISIBLE(lcVenSum) .OR. WVISIBLE(lcVenSum)
  IF WVISIBLE(lcVenSum)
    =lfvVenTotals()
  ENDIF
  GO RECNO('APINVAHD') IN APINVAHD
  =lfRefresh()
ENDIF

SHOW WINDOW (lcBrInvTtl) REFRESH

*!**************************************************************************
*!
*!        Function : lfvBrowse
*!
*!**************************************************************************
* 
FUNCTION lfvBrowse
IF WONTOP(lcBrInvTtl)
  IF !WEXIST(lcAprInCh2) 
    glFromBrow = .F.
    glQuitting = .T.
    CLEAR READ
    KEYBOARD CHR(13)
    RETURN TO APAPRIN.SPR
  ENDIF
ELSE
  =lfBrowUnTrap() .AND. lfClrMous() 
  IF !WVISIBLE(gcBaseWind)
    glQuitting = .T.
  ENDIF  
  =gfStopBrow()
ENDIF    

*!**************************************************************************
*!
*!        Function : lfvBrAmnts
*!
*!**************************************************************************
FUNCTION lfvBrAmnts
PARAMETERS lcAmntFld, lnAmnt, lnCmpAmnt, lcTAmtStr, lcTCmpStr
*** If the enetered value is negative, present the folllowing
*** message and return the old value,
*** Message: " Negative values are not allowed.  "
*** Choices: "              ® Ok ¯               "
*** Or, 
*** If the entered value is greater than the comparison
*** value, present the following message and return the old value
*** Message: " ð cannot be greater than ð.       "
*** Choices: "              ® Ok ¯               "
IF ( &lc_Invoices..&lcAmntFld < 0 .AND.;
  gfModalGen("TRM04087B00000", "DIALOG") > 0);
 .OR.;
 (lnAmnt > lnCmpAmnt .AND. ;
   gfModalGen("TRM04025B00000", "DIALOG", lcTAmtStr+'|'+lcTCmpStr) > 0)
  REPLACE &lc_Invoices..&lcAmntFld WITH lnOldVal
  SHOW WINDOW (lcBrInvTtl) REFRESH SAME  
ENDIF  


*!**************************************************************************
*!
*!        Function : lfvGenDate
*!
*!**************************************************************************
* Valid function for get field ldGenDate (generation date)
*
FUNCTION lfvGenDate
IF ldGenDate <> ldOldVal .AND. WVISIBLE(gcBaseWind)
  IF !EMPTY(ldGenDate) .AND. !lfVlDate(gcPrnt_Cmp)
    *** If the date is invalid, present the following message
    *** and blank the field
    *** Message: " The ð date should be within the posting window."
    *** Choices: "                       ® Ok ¯                   "
    =gfModalGen('TRM04113B00000', 'DIALOG', lcTIns)
    ldGenDate = IIF(RECCOUNT(lc_Invoices) > 0, ldOldVal, {})
    _CUROBJ   = OBJNUM(ldGenDate)
  ELSE
    *** If there are generated invoices, and the 
    *** date is changed, present a warning message that
    *** generated invoices are going to be disacrded.
    *** Message: " The generated invoices will be discarded! "
    *** Choices: "          <   Ok   >    < Cancel >         "
    IF RECCOUNT(lc_Invoices) > 0 
      IF gfModalGen("TRM04004B04004", "DIALOG") = 1
        =lfClearGen()
      ELSE
        ldGenDate = ldOldVal
        _CUROBJ   = OBJNUM(ldGenDate)
      ENDIF
    ENDIF
  ENDIF
  ldOldVal = ldGenDate
ENDIF

*!**************************************************************************
*!
*!      Function: lfvScope
*!
*!**************************************************************************
*  Valid function for radio button rbScope
*
FUNCTION lfvScope
PRIVATE lcCurAlias, lcDueState, lcDscState, lcOldDiv, lcOldDOpt,;
        lcOldInvRef, lcOldPPr, lcOldPMeth, lcOldVend, lcOldComp,;
        lcOldPOpt, ldOldDuF, ldOldDuT, ldOldDsF, ldOldDsT, lnOldrbDat,;
        lnOldpuPM, lcOldpuDv     

lcCurAlias = ALIAS()

llUpdated  = .F.
llInvOK    = rbScope = 1    && .T. if all invoices, .F. otherwise

DO CASE
  CASE rbScope = 1 .AND. rbScope <> lnOldrbScp
    llUpdated   = .T.
  CASE rbScope = 2   
    *** Save old data so as to restore it if the user cancels
    lcOldDiv    = lcDivision
    lcOldDOpt   = lcDivOpt
    lcOldInvRef = lcInvRef
    lcOldPPr    = lcPayPrior
    lcOldPMeth  = lcPayMeth
    lcOldPOpt   = lcPMethOpt  
    lcOldVend   = lcVendCode 
    lcOldComp   = lcVendComp
    lcOldpuDv   = puDivision
    ldOldDuF    = ldDueFrom
    ldOldDuT    = ldDueTo
    ldOldDsF    = ldDiscFrom
    ldOldDsT    = ldDiscTo
    lnOldrbDat  = rbDates
    lnOldpuPM   = puPayMeth
    
    
    IF rbDates = 1
      lcDueState = "ENABLE"
      lcDscState = "DISABLE"
    ELSE
      lcDscState = "ENABLE"
      lcDueState = "DISABLE"
    ENDIF  
    lcHidObjClr = SUBSTR(SCHEME(5,1),ATC('/',SCHEME(5,1))+1)
    lcHidObjClr = lcHidObjClr + '/' + STRTRAN(lcHidObjClr, '*', '+')

    PUSH KEY
    ON KEY
    *E300683,1 Call *.SPR from screens directory
    * DO APAPRINI.SPR 
    DO (gcScrDir + gcWinAppl + '\APAPRINI.SPR')
    *E300683,1 end          
    RELEASE PAD _BROWSE OF _MSYSMENU
    POP KEY
    
ENDCASE

*** If there are generated invoices, and the 
*** selected invoice are changed, present a warning message that
*** generated invoices are going to be disacrded.
*** Message: " The generated invoices will be discarded! "
*** Choices: "           <   Ok   >    < Cancel >        "
 
IF RECCOUNT(lc_Invoices) > 0 .AND. llUpdated .AND. llInvOk
  IF gfModalGen("TRM04004B04004", "DIALOG") = 1
    =lfClearGen()
  ELSE
    *** If rbScope = 2, reset llOnvOk (as if cancelled)
    llInvOk = rbScope <> 2 
    rbScope = lnOldrbScp
    SHOW GET rbScope
  ENDIF  
ENDIF

*** If Cancelled, restore old data
IF !llInvOk   
  lcDivision = lcOldDiv    
  lcDivOpt   = lcOldDOpt
  lcInvRef   = lcOldInvRef 
  lcPayPrior = lcOldPPr    
  lcPayMeth  = lcOldPMeth  
  lcPMethOpt = lcOldPOpt
  lcVendCode = lcOldVend
  lcVendComp = lcOldComp
  ldDueFrom  = ldOldDuF    
  ldDueTo    = ldOldDuT    
  ldDiscFrom = ldOldDsF    
  ldDiscTo   = ldOldDsT    
  rbDates    = lnOldrbDat
  puPayMeth  = lnOldpuPM
  puDivision = lcOldpuDv
ENDIF

SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)

*** Save the current radio button selection
lnOldrbScp  = rbScope


*!**************************************************************************
*!
*!      Function: lfShowDate
*!
*!**************************************************************************
*  When function for push button pbGenerate, rbScope
*
FUNCTION lfShowDate
SHOW GET ldGenDate

*!**************************************************************************
*!
*!      Function: lfvGenerate
*!
*!**************************************************************************
*  Valid function for push button update
*
FUNCTION lfvGenerate
PRIVATE lcCurAlias, lcScanCmd, lnTotRec, lnCurRec, lnCount, lnNoOfInst ,;
        ln1099InstNo, lnInstAmnt, ln1099Amnt, lnOpenAmnt

*** If the generation date is empty, present the following message
*** and do not generate, else continue with the generaton process.
IF EMPTY(ldGenDate)
  *** Message: " You have to enter the ð.  "
  *** Choices: "            ® Ok ¯         "
  =gfModalGen("TRM04066B00000", "DIALOG", lcTInsDate)
  _CUROBJ = OBJNUM(ldGenDate)
ELSE
  lcCurAlias  = ALIAS()
  SET ORDER TO 0 IN APINVAHD
  IF rbScope = 1
    lcInvFlt  = lcMainFlt
    lcScanCmd = ""
  ELSE
    lcInvFlt  = lcMainFlt + ;
                IIF(!EMPTY(lcVendCode), [.AND.cVendCode=lcVendCode],"")
    lcScanCmd = IIF(!EMPTY(lcInvRef),;
                [.AND.LIKE(STRTRAN(UPPER(lcInvRef),' ','?'),UPPER(APINVHDR.cInvRef))],"") +;
                IIF(!('*' $ lcDivOpt), [.AND.APINVHDR.cDivision=lcDivOpt],"") + ;
                IIF(!EMPTY(lcPayPrior),[.AND.APINVHDR.cVenPrior=lcPayPrior],"") +;
                IIF(!EMPTY(lcPMethOpt),[.AND.APINVHDR.cVenPMeth=lcPMethOpt],"")+;
                IIF(rbDates = 1,;
                    IIF(!EMPTY(ldDueFrom),;
                        IIF(!EMPTY(ldDueTo),;
                            [.AND.BETWEEN(APINVHDR.dInvDuDat,ldDueFrom,ldDueTo)],;  
                            [.AND.APINVHDR.dInvDuDat >= ldDueFrom]),;
                        IIF(!EMPTY(ldDueTo),;
                            [.AND. APINVHDR.dInvDuDat <= ldDueTo], " ")),;
                     IIF(!EMPTY(ldDiscFrom),;
                         IIF(!EMPTY(ldDiscTo),;
                             [.AND.BETWEEN(APINVHDR.dInvDate+APINVHDR.nTerDiscD, ldDiscFrom, ldDiscTo)],;  
                             [.AND.APINVHDR.dInvDate+APINVHDR.nTerDiscD >= ldDiscFrom]),;
                           IIF(!EMPTY(ldDiscTo),;
                             [.AND.APINVHDR.dInvDate+APINVHDR.nTerDiscD <= ldDiscTo], " ")))
  ENDIF

  *** Refresh the filter expression in the APINVAHD
  SET ORDER TO TAG TVENDINV IN APINVAHD
  GO TOP IN APINVAHD

  *** If the filter expression produces an eof in apinvahd,
  *** the total records is 0
  lnTotRec      = IIF(EOF('APINVAHD'), 0, RECCOUNT('APINVAHD'))

  IF lnTotRec > 0

    SELECT (lc_Invoices)
    SET RELATION OFF INTO APINVAHD
    ZAP

    *** Prepare thermometer variables
    lnTotRec     = lnTotRec + 1
    lnCurRec     = 0
    
    *E300789,4 IHB Remove company ID [start]
    *E301077,80 IHB 03/04/1999 opend needed files only [start]
    =gfOpenFile(lcDataDir+'FSPRD' ,'COMFYRPRDI','SH')
    *E300789,4 IHB [end]
  
    *** If the current vendor has a zero payment priority, 
    *** or, if the current invoice has a zero payment priority,
    *** present the following message and do not invluce the 
    *** current invoice in the temporary file
    *** Message: " ð has a zero payment priority.  ð is "
    ***          " on hold.  ð cannot be approved.      "
    *** Choices: "                ® Ok ¯                "
    SELECT APINVAHD
  
    SCAN FOR !EMPTY(dAutNxGen) ;
       .AND. dAutNxGen <= ldGenDate;
       .AND. lfVldNextDate(dAutNxGen) ;
       .AND. (APVENDOR.cVenPrior <> '0' .OR. ;
              gfModalGen("TRM04070B00000", "DIALOG",;
                          lcTVendor + ALLTRIM(cVendCode) + '|' +;
                          lcTThisVend + '|' + lcTVenInst + ALLTRIM(cInvNo)) < 0);
       .AND. (APINVHDR.cVenPrior <> '0' .OR. ;
              gfModalGen("TRM04070B00000", "DIALOG",;
                          lcTVendor + ALLTRIM(cVendCode) + ' ' +;
                          lcTInvoice + ALLTRIM(cInvNo) + '|' +;
                          lcTThisInv + '|' + lcTInvInst) < 0);
       .AND. (APINVHDR.nInvAmnt - APINVHDR.nInvPaid ;
               - APINVHDR.nInvDisTk - APINVHDR.nInvAdj) <> 0 ;
       &lcScanCmd        
               
      *** Increment thermometer step
      lnCurRec = lnCurRec + 1
      =gfThermo(lnTotRec, lnCurRec, lcTGenIns,;
                lcTVendor + APINVAHD.cVendCode + ;
                SPACE(3) + lcTInvoice + APINVAHD.cInvNo)

      *** Get the number of installments due from the next generation
      *** date of the installment until the generation date
      *B600624,1 Get the closest greater integer for the
      *B600624,1 calculation of the number of installments (CEILING)
      *B600624,1 instead of getting the INTeger.
      *B600624,1 Add a 1 to the number of days only.
      *lnNoOfInst   = IIF(APINVAHD.cAutFUnit = 'D',;
                         INT((ldGenDate - APINVAHD.dAutNxGen);
                         / APINVAHD.nAutFreq),;
                         INT(lfGetNoOfInst() / APINVAHD.nAutFreq)) + 1
      lnNoOfInst   = IIF(APINVAHD.cAutFUnit = 'D',;
                         INT((ldGenDate - APINVAHD.dAutNxGen);
                         / APINVAHD.nAutFreq) + 1,;
                         CEILING(lfGetNoOfInst() / APINVAHD.nAutFreq))
      *B600624,1 end.  
                           
      *** Calculate the open invoice amount taking the maximum of the 
      *** offered discount and the discount taken.
      lnOpenAmnt   =  APINVHDR.nInvAmnt - APINVHDR.nInvPaid ;
                      - MAX(APINVHDR.nInvDisTk, APINVHDR.nInvDisOf) ;
                      - APINVHDR.nInvAdj
      
      *** Total installment amount is the minimum of
      *** The total installment amount calculated as follows :
      *** If base is 'A'mounts : installment * no of installments
      *** else                 : installment % * invoice amount *
      ***                        no of installments
      *** and the open invoice amount calculated above. 
      lnInstAmnt   =  MIN( IIF(APINVAHD.cAutMBase = 'A',;
                           APINVAHD.nInvAmtAp,;
                           APINVHDR.nInvAmnt * APINVAHD.nInvAmtAp / 100) ;
                           * lnNoOfInst, lnOpenAmnt)

      *** Total 1099 amount is the minimum of
      *** The total 1099 amount calculated as follows :
      *** If base is 'A'mounts : 1099 amount * no of possible installments
      *** else                 : 1099 amount % * invoice amount *
      ***                        no of possible installments
      *** and the open invoice amount calculated above. 

      *** The number of possible installments is the quotient of
      *** dividing the total installments amount by an installment
      *** amount.
      ln1099InstNo = INT(lnInstAmnt / APINVAHD.nInvAmtAp)
      ln1099Amnt   =  MIN( IIF(APINVAHD.cAutMBase = 'A',;
                           APINVAHD.nInvA1099,;
                           APINVHDR.nInvAmnt * APINVAHD.nInvA1099 / 100) ;
                           * ln1099InstNo, lnOpenAmnt)

      INSERT INTO (lc_Invoices);
          (cInvNo, cVendCode, nNoOfInst, nInstAmnt,;
           nDisAmnt, n1099Amnt, nOpenAmnt, nAprAmnt, cTimStamp);
        VALUES(APINVAHD.cInvNo, APINVAHD.cVendCode, lnNoOfInst, lnInstAmnt,;
               MAX(APINVHDR.nInvDisOf - APINVHDR.nInvDisTk, 0), ln1099Amnt,;
               APINVHDR.nInvAmnt - APINVHDR.nInvPaid ;
               - APINVHDR.nInvDisTk - APINVHDR.nInvAdj,;
               APINVHDR.nInvAmtAp + APINVHDR.nInvDisAp + APINVHDR.nInvAdjAp,;
               APINVHDR.cAdd_User + DTOC(APINVHDR.dAdd_Date) + ;
               APINVHDR.cAdd_Time)

      SELECT APINVAHD
    ENDSCAN

    *** Close the thermometer if not already closed
    IF lnCurRec > 0 .AND. lnTotRec > lnCurRec
      FOR lnCount = lnCurRec TO lnTotRec
        =gfThermo(lnTotRec, lnCount, lcTGenIns, "")
      ENDFOR
    ENDIF  

    SELECT (lc_Invoices) 
    SET RELATION TO 'I' + cVendCode + cInvNo INTO APINVAHD
    =lfBrowInv()
    IF RECCOUNT(lc_Invoices) > 0
      lcAprStat = "ENABLE"
      lcGenStat = "DISABLE"
      lcTSlPr   = lcTSelect
      llMayGenr = .F.
      SHOW GETS ENABLE 
      SHOW GET pbSelect,1 PROMPT lcTSelect
      SHOW GET pbGenerate DISABLE
    ELSE
      ** Message: " No ð matching the selected invoices criteria."
      ** Choices: "                      ® Ok ¯                  "
      =gfModalGen("TRM04089B00000","DIALOG",'open invoices')
    ENDIF
  *** No selected invoices
  ELSE
    *** Message: " No ð matching the selected invoices criteria."
    *** Choices: "                 ® Ok ¯                       "
    =gfModalGen("TRM04089B00000","DIALOG",'open invoices')
  ENDIF
 
  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
ENDIF  


*!**************************************************************************
*!
*!      Function: lfvSelect
*!
*!**************************************************************************
*
FUNCTION lfvSelect
IF EMPTY(cInclude)
  REPLACE cInclude WITH lcCheck
  lcSlPr  = lcTUnSelect
  SHOW GET pbSelect,1 PROMPT lcTUnSelect
ELSE
  REPLACE cInclude WITH lcUnCheck
  lcSlPr  = lcTSelect
  SHOW GET pbSelect,1 PROMPT lcTSelect
ENDIF
SHOW WINDOW (lcBrInvTtl) REFRESH SAME


*!**************************************************************************
*!
*!      Function: lfvSelAll
*!
*!**************************************************************************
*
FUNCTION lfvSelAll
PARAMETERS lcCheckMark
PRIVATE lnRecNo

lnRecNo = RECNO()
REPLACE ALL cInclude WITH lcCheckMark
IF lnRecNo <=RECCOUNT()
  GO lnRecNo
ELSE
  GO TOP  
ENDIF  
IF !EMPTY(cInclude)
  lcSlPr  = lcTUnSelect
  SHOW GET pbSelect,1 PROMPT lcTUnSelect
ELSE
  lcSlPr  = lcTSelect
  SHOW GET pbSelect,1 PROMPT lcTSelect
ENDIF
SHOW WINDOW (lcBrInvTtl) REFRESH SAME

*!**************************************************************************
*!
*!      Function: lfvInvert
*!
*!**************************************************************************
*
FUNCTION lfvInvert
PRIVATE lnRecNo
lnRecNo = RECNO()
REPLACE ALL cInclude WITH ;
        IIF(EMPTY(cInclude), lcCheck, lcUnCheck) 
IF lnRecNo <=RECCOUNT()
  GO lnRecNo
ELSE
  GO TOP  
ENDIF  
IF !EMPTY(cInclude)
  lcSlPr  = lcTUnSelect
  SHOW GET pbSelect,1 PROMPT lcTUnSelect
ELSE
  lcSlPr  = lcTSelect
  SHOW GET pbSelect,1 PROMPT lcTSelect
ENDIF
SHOW WINDOW (lcBrInvTtl) REFRESH SAME


*!**************************************************************************
*!
*!        Function : lfvApprove
*!
*!**************************************************************************
FUNCTION lfvApprove
PRIVATE lcCurAlias, lcPrd, lcSetExact, lnAutNxtNo, lnInvNoSiz, lnTotRec,;
        lnCurRec, lnCount

*** Check if there are any selected records for update
*** If there are not, present the following message and return
lcCurALias = ALIAS()
lcSetExact = SET('EXACT')
SET EXACT OFF

SELECT (lc_Invoices)
LOCATE FOR !EMPTY(cInclude)
IF FOUND()
  lnTotRec  = RECCOUNT(lc_Invoices) + 1
  lnCurRec  = 0
  
  *** Check if the current invoice's audit information is changed
  *** since generation. If it is, present the following message and
  *** skip spprovign this invoice,
  *** Message: " Vendor : ð   Invoice : ð has been modified  " 
  ***          " since the beginning of the current session. "
  ***          " The installment for this invoice cannot be  "
  ***          " approved.  You need to regenerate this      "
  ***          " installment.                                "
  *** Choices: "                      ® Ok ¯                 "

  *** Check if the current invoice already has approved amounts,
  *** If it is, present a message enquiring from the user if it is
  *** to be overwritten, or skip approving this invoice,
  *** Message: " Vendor : ð   Invoice : ð has approved amounts.      "
  ***          " Approving the installment for this invoice will     "
  ***          " overwrite its approved amounts.                     "
  *** Choices: " < Overwrite approved amounts > < Skip this invoice >"

  *** Attempt to record lock the current invoice
  SCAN FOR !EMPTY(cInclude)
    SELECT APINVAHD
    IF (&lc_Invoices..cTimStamp = APINVHDR.cAdd_User + ;
       DTOC(APINVHDR.dAdd_Date) + APINVHDR.cAdd_Time ;
      .OR.;
       gfModalGen("TRM04125B00000", "DIALOG",;
       ALLTRIM(cVendCode) + '|' + ALLTRIM(cInvNo)) < 0);
      .AND.;
      (APINVHDR.nInvAmtAp + APINVHDR.nInvDisAp + APINVHDR.nInvAdjAp +;
      APINVHDR.nInvA1099 = 0 ;
      .OR.;
      gfModalGen("TRM04126B04005", "DIALOG",;
       ALLTRIM(cVendCode) + '|' + ALLTRIM(cInvNo)) = 1);
     .AND. gfRLock("APINVHDR",.T.) 
        
      *** Increment thermometer step
      lnCurRec = lnCurRec + 1
      =gfThermo(lnTotRec, lnCurRec, lcTAprIns,;
                lcTVendor + cVendCode + ;
                SPACE(3) + lcTInvoice + cInvNo)

      REPLACE;
        APINVAHD.dAutNxGen WITH ;
           IIF(APINVAHD.cAutFUnit = 'D',;
               ldGenDate + APINVAHD.nAutFreq,;
               lfGetNextDate(ldGenDate, nAutFreq)),;
        APINVAHD.dAutLGen  WITH ldGenDate,;
        APINVAHD.cAdd_User WITH gcUser_ID,;
        APINVAHD.dAdd_Date WITH ldCurDate,;
        APINVAHD.cAdd_Time WITH gfGetTime()

      SELECT APINVHDR  
      *B600630,1 Store Bank code, Checking account and G/L
      *B600630,1 checking account into APINVHDR.     
      *REPLACE;
         APINVHDR.nInvAmtAp WITH &lc_Invoices..nInstAmnt,;
         APINVHDR.nInvDisAp WITH &lc_Invoices..nDisAmnt,;
         APINVHDR.nInvA1099 WITH &lc_Invoices..n1099Amnt,;             
         APINVHDR.cAdd_User WITH gcUser_ID,;
         APINVHDR.dAdd_Date WITH ldCurDate,;
         APINVHDR.cAdd_Time WITH gfGetTime()

      *B601526,1 Add the new field (nInvFAAp) [Begin]   
      *REPLACE;
      *  APINVHDR.nInvAmtAp WITH &lc_Invoices..nInstAmnt,;
      *  APINVHDR.nInvDisAp WITH &lc_Invoices..nDisAmnt,;
      *  APINVHDR.nInvA1099 WITH &lc_Invoices..n1099Amnt,;             
      *  APINVHDR.cBnkCode  WITH APINVAHD.cBnkCode,;
      *  APINVHDR.cChkAcct  WITH APINVAHD.cChkAcct,;
      *  APINVHDR.cChkGlAcc WITH APINVAHD.cChkGlAcc,;
      *  APINVHDR.cAdd_User WITH gcUser_ID,;
      *  APINVHDR.dAdd_Date WITH ldCurDate,;
      *  APINVHDR.cAdd_Time WITH gfGetTime()

      REPLACE;
        APINVHDR.nInvAmtAp WITH &lc_Invoices..nInstAmnt,;
        APINVHDR.nInvDisAp WITH &lc_Invoices..nDisAmnt,;
        APINVHDR.nInvA1099 WITH &lc_Invoices..n1099Amnt,;             
        APINVHDR.cBnkCode  WITH APINVAHD.cBnkCode,;
        APINVHDR.cChkAcct  WITH APINVAHD.cChkAcct,;
        APINVHDR.cChkGlAcc WITH APINVAHD.cChkGlAcc,;
        APINVHDR.cAdd_User WITH gcUser_ID,;
        APINVHDR.dAdd_Date WITH ldCurDate,;
        APINVHDR.cAdd_Time WITH gfGetTime(),;
        APINVHDR.nInvFAAp  WITH &lc_Invoices..nInstAmnt

      *B601526,1 Add the new field (nInvFAAp) [End]   

      *B600630,1 end.
    ENDIF
    =gfRLock("APINVHDR",.F.) 
      
    SELECT (lc_Invoices)
  ENDSCAN
 
    *** Close the thermometer if not already closed
  IF lnCurRec > 0 .AND. lnTotRec > lnCurRec
    FOR lnCount = lnCurRec TO lnTotRec
      =gfThermo(lnTotRec, lnCount, lcTAprIns, "")
    ENDFOR
  ENDIF  

  *** Reset variables and files, to be ready for a new selection
  =lfClearGen(.T.)

ELSE
  *** Message: " There are no ð.                 "
  *** Choices: "              ® Ok ¯             "
  =gfModalGen("TRM04101B00000","DIALOG", lcTNoSInv)
ENDIF

SET EXACT &lcSetExact
SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)


*!**************************************************************************
*!
*!        Function : lfvVendor
*!
*!**************************************************************************
FUNCTION lfvVendor
PRIVATE laRetVal
DECLARE laRetVal[3]
laRetVal     = " "
IF lfGetExt('APVENDOR', 'VENCODE', lcOldVal, llBrowse,  'cVendCode',;
   @laRetVal,'cVendCode,cVenComp,cPhoneNo')  
  lcVendComp = laRetVal[2]
  SHOW GET lcVendComp
ENDIF  
llUpdated    = lcVendCode <> lcOldVal
llBrowse     = .F.


*!**************************************************************************
*!
*!      Function: lfvInvRef
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvInvRef
IF lcInvRef <> lcOldVal
  lcInvRef = PADR(ALLTRIM(lcInvRef),FSIZE('cInvRef','APINVHDR'))
  SHOW GET lcInvRef
  llUpdated = .T.
ENDIF  


*!**************************************************************************
*!
*!      Function: lfvPayPr
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvPayPr
IF lcPayPrior <> lcOldVal
  IF lcPayPrior = '0' 
    lcPayPrior = ' '
    SHOW GET lcPayPrior
  ENDIF
  llUpdated = .T.
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvDivision
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvDivision
PRIVATE lcCurAlias

lcCurAlias   = ALIAS()

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

IF _DOS

  *E300643,1 Change this line for the changes we have made to 
  *          SYCCODES [Begin]
  *lcDivOpt   = gfActPop(3,28,8,62,'SYCCODES','cCode_No','cDiscrep',@lcDivision)
  lcDivOpt   = gfActPop(3,28,8,62,'CODES','cCode_No','cDiscrep',@lcDivision)
  *E300643,1 Change this line [End]
  
  SHOW GETS
ELSE

  *E300643,1 Change this lines for the changes we have made to 
  *          SYCCODES [Begin]
  *lcDivision = SYCCODES.cdiscrep
  *lcDivOpt   = SYCCODES.cCode_No
  lcDivision = CODES.cdiscrep
  lcDivOpt   = CODES.cCode_No
  *E300643,1 Change this line [End]

  SHOW GET lcDivision
  DEACTIVATE POPUP puDivision
ENDIF

SELECT IIF(EMPTY(lcCurAlias), 0, (lcCurAlias))
llUpdated = .T.

*!**************************************************************************
*!
*!      Function: lfvPayMeth
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvPayMeth
lcPMethOpt = lfvPopups('laPayMeth', @lcPayMeth, 5,48,MIN(lnPayMLen,21)) 
SHOW GETS 
llUpdated = .T.


*!**************************************************************************
*!
*!      Function: lfvDueDate
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvDueDate

PRIVATE lcCurrObj, lcCentSet

IF !EMPTY(ldDueFrom) .AND. !EMPTY(ldDueTo) ;
  .AND. (ldDueFrom > ldDueTo)
  *** If due from date (ldDueFrom) > due to date(ldDueTo)
  *** present the following message and return the old dalue.
  *** Message: " The ð date cannot be less than the ð date."
  *** Choices: "                     ® Ok ¯                "
  lcCentSet = SET('CENTURY')
  SET CENTURY ON
  =gfModalGen("TRM04028B00000","DIALOG",lcDueTo+'|'+lcDueFrom)
  SET CENTURY &lcCentSet             
  lcCurrObj = SYS(18)
  &lcCurrObj = ldOldVal
  SHOW GET &lcCurrObj
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvDscDate
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvDscDate
PRIVATE lcCurrObj, lcCentSet

IF !EMPTY(ldDiscFrom) .AND. !EMPTY(ldDiscTo);
  .AND. ldDiscFrom > ldDiscTo 
  *** If discount from date (ldDiscFrom) > discount to date(ldDiscTo)
  *** present the following message and return the old dalue.
  *** Message: " The ð date cannot be less than the ð date."
  *** Choices: "                     ® Ok ¯                "
  lcCentSet = SET('CENTURY')
  SET CENTURY ON
  =gfModalGen("TRM04028B00000","DIALOG", lcDiscTo+'|'+lcDiscFrom)
  SET CENTURY &lcCentSet                                
  lcCurrObj = SYS(18)
  &lcCurrObj = ldOldVal
  SHOW GET &lcCurrObj
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvDates
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvDates
IF rbDates = 1
  SHOW GET ldDueFrom   ENABLE
  SHOW GET ldDueTo     ENABLE
  SHOW GET ldDiscFrom DISABLE 
  SHOW GET ldDiscTo   DISABLE 
ELSE
  SHOW GET ldDueFrom  DISABLE 
  SHOW GET ldDueTo    DISABLE 
  SHOW GET ldDiscFrom  ENABLE
  SHOW GET ldDiscTo    ENABLE
ENDIF  
SHOW GETS


*!**************************************************************************
*!
*!      Function: lfvInvOk
*!
*!**************************************************************************
*  Valid function for push button OK in APAPLDBI screen ( Select invoices)
*
FUNCTION lfvInvOk
llInvOk = .T. 
CLEAR READ  

*!**************************************************************************
*!
*!      Function: lfBrowTrap
*!
*!*******************************************************************************
*  Sets key traps when a browse is active.
*  function : to enable calling the outer objects on an ALT+hot key presss
*
FUNCTION lfBrowTrap
ON KEY LABEL ESC        DO lpEsc
ON KEY LABEL CTRL+ENTER DO lpEsc
ON KEY LABEL Alt-G DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-L DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-V DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-D DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-C DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-S DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-A DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-N DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-I DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-P DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-C DO lpBrowKeyTrap WITH LASTKEY()

*!**************************************************************************
*!
*!      Function: lfBrowUnTrap
*!
*!*******************************************************************************
*  Sets key traps when a browse is active.
*  function : to enable calling the outer objects on an ALT+hot key presss
*
FUNCTION lfBrowUnTrap
ON KEY LABEL CTRL+ENTER &lcOnCtrlEntr
ON KEY LABEL ESC        &lcOnEsc
ON KEY LABEL Alt-G 
ON KEY LABEL Alt-L 
ON KEY LABEL Alt-V 
ON KEY LABEL Alt-D 
ON KEY LABEL Alt-C 
ON KEY LABEL Alt-S 
ON KEY LABEL Alt-A 
ON KEY LABEL Alt-N 
ON KEY LABEL Alt-I 
ON KEY LABEL Alt-P 
ON KEY LABEL Alt-C 

*!**************************************************************************
*!
*!      Procedure: lpBrowKeyTrap
*!
*!*******************************************************************************
*  Key traps when browsing
*
PROCEDURE lpBrowKeyTrap
PARAMETERS lnLastKey

KEYBOARD "{SHIFT+HOME}"  && dummy key press to release menu access

*** Cases that are valid whether the browse has records or not
DO CASE
  CASE lnLastKey = 38    && Alt-L     () All invoices
      rbScope = 1
      SHOW GET rbScope
     =lfvScope()
  CASE lnLastKey = 47    && Alt-V     () Selected invoices...
    rbScope = 2
    SHOW GET rbScope
    =lfvScope()
ENDCASE

*** If there are no records in the browse, we can only generate,
*** hence
IF RECCOUNT(lc_Invoices) = 0
  IF lnLastKey = 34    && Alt-G     < Generate >
      =lfvGenerate()
  ENDIF
ELSE
  DO CASE  
    CASE lnLastKey = 32    && Alt-D     < Vendor... >
      CLEAR TYPEAHEAD
      ACTIVATE WINDOW (lcAprInCh1)
      =lfvVenSum()
    CASE lnLastKey = 46    && Alt-C     < Invoice... >
      CLEAR TYPEAHEAD
      ACTIVATE WINDOW (lcAprInCh1)
      =lfvInvSum()
    CASE lnLastKey = 31    && Alt-S     < Select >
      =lfvSelect()
    CASE lnLastKey = 30    && Alt-A     < Select all >
      =lfvSelAll(lcCheck)
    CASE lnLastKey = 49    && Alt-N     < Select none >
      =lfvSelAll(lcUnCheck)
    CASE lnLastKey = 23    && Alt-I     < Invert >
      =lfvInvert()
    CASE lnLastKey = 25    && Alt-P     < Approve >
      =lfvApprove()
  ENDCASE
ENDIF


*!**************************************************************************
*!
*!      Function: lfActBrows
*!
*!**************************************************************************
*
FUNCTION lfActBrows
RELEASE PAD _BROWSE OF _MSYSMENU
ON KEY LABEL ALT+B
ACTIVATE WINDOW (lcBrInvTtl) TOP


*!**************************************************************************
*!
*!      Function: lfDefinePad
*!
*!**************************************************************************
* Defines an additional 'Browse' pad when one does not exist (i.e. when
* a browse is not active)
*
FUNCTION lfDefinePad
DEFINE PAD _BROWSE OF _MSYSMENU PROMPT lcTBrowse
ON SELECTION PAD _BROWSE OF _MSYSMENU DO lfActBrows
ON KEY LABEL ALT+B DO lfActBrows

*!**************************************************************************
*!
*!      Function: lfActivate
*!
*!**************************************************************************
* Current Read's activate function
* releases the browse pad if a browse window is on top, and traps keys
* and vice versa.
*
FUNCTION lfActivate
SELECT (lc_Invoices)
IF _DOS
  =lfDefinePad()
ENDIF



*!**************************************************************************
*!
*!      Function: lfDeact
*!
*!**************************************************************************
* Current Read's deactivate function
* releases the browse pad if a browse window is on top, and traps keys
* and vice versa.
*
FUNCTION lfDeact
IF WONTOP(lcBrInvTtl)
  glFromBrow  = .T. 
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY LABEL ALT+B
  =lfBrowTrap() .AND. lfMousClk()
ENDIF


*!**************************************************************************
*!
*!      Function: lfMousClk
*!
*!**************************************************************************
* mouse trap 
FUNCTION lfMousClk
ON KEY LABEL LEFTMOUSE DO lpMClkSel

*!**************************************************************************
*!
*!      Procedure: lpMClkSel
*!
*!**************************************************************************
*If mouse double click select record.
FUNCTION lpMClkSel

IF SECONDS() < lnTime .AND. RECCOUNT(lc_Invoices) > 0
  =lfvSelect()
ENDIF
lnTime = _DBLCLICK+SECONDS()

*!**************************************************************************
*!
*!      Function: lfClrMous
*!
*!**************************************************************************
*Clear mouse trap. 
FUNCTION lfClrMous
ON KEY LABEL LEFTMOUSE
           
*!**************************************************************************
*!
*!      Procedure: lpEsc
*!
*!**************************************************************************
* Used by both Ctrl+Entr and Esc
*
PROCEDURE lpEsc
IF _DOS
  HIDE MENU _MSYSMENU
  SHOW MENU _MSYSMENU
ENDIF  
ACTIVATE WINDOW (lcAprInCh3)
_CUROBJ=OBJNUM(pbClose)
KEYBOARD CHR(13)



*!**************************************************************************
*!
*!      Function: lfClearGen
*!
*!**************************************************************************
* Clears previously generated installments
FUNCTION lfClearGen
PARAMETERS llFromScpBt
PRIVATE lcCurAlias

lcCurAlias = ALIAS()
SELECT (lc_Invoices)
ZAP
=lfBrowInv()
lcGenStat  = 'ENABLE'
lcAprStat  = 'DISABLE'
llMayGenr  = .T.
IF !llFromScpBt
  STORE 1 TO rbScope, lnOldrbScp
  *** Reset values in the selection screen
  lcVendCode = SPACE(8)
  lcInvRef   = SPACE(15)
  STORE " " TO lcDivOpt, lcPayPrior, lcPayMeth,;
               lcPMethOpt, lcVendComp  
  lcPayMeth  = lcTAll
  lcDivision = lcTAllCode                 

  *E300643,1 Change this line for the changes we have made to 
  *          SYCCODES [Begin]
  *GO TOP IN SYCCODES             
  GO TOP IN CODES             
  *E300643,1 Change this line [End]
  
  rbDates     = 1
ENDIF
=lfShowInv(lcAprStat)

*** CLose chile windows if visible
IF WVISIBLE(lcVenSum)      && close vendor window if it's opened.
  =gfChClose(lcVenSum)
ENDIF  
IF WVISIBLE(lcInvSum)      && close invoice window if it's opened.
  =gfChClose(lcInvSum)
ENDIF  

SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)



*!**************************************************************************
*!
*!      Function: lfvVenSum
*!
*!**************************************************************************
* Activate verndor window.
*
FUNCTION lfvVenSum

*** Get all required totals
=lfvVenTotals()
GO RECNO('APINVAHD') IN APINVAHD
=lfRefresh()
*** Call the child screen through the global function activate window. ***
=gfActWind(lcVenSum, lcVendTtl, gcBaseWind)

*!**************************************************************************
*!
*!      Function: lfvInvSum
*!
*!**************************************************************************
* Activate invoice window.
*
FUNCTION lfvInvSum
*** Call the child screen through the global function activate window. ***
GO RECNO('APINVAHD') IN APINVAHD
=lfRefresh()
=gfActWind(lcInvSum, lcInvoice, gcBaseWind)




*!**************************************************************************
*!
*!      Function: lfVldNextDate
*!
*!**************************************************************************
*
FUNCTION lfVldNextDate
PARAMETERS ldNxtDate
PRIVATE llVldDate
llVldDate = .F.

*** Check if the date lies within the posting window. 
*** If it does not, present the following message and return .f.
*** Message: " The next installment date ð for Vendor ð Invoice ð "
***          " is out of the posting window.  You should correct  "
***          " the next installment date for this invoice.        "
*** Choices: "                          ® Ok ¯                    "
llVldDate = ! ((!lfVlDate(gcPrnt_Cmp,' ',' ', ldNxtDate);
             .AND. gfModalGen('TRM04076B00000', 'DIALOG', ;
                   DTOC(ldNxtDate) + '|' + ALLTRIM(cVendCode) +;
                   '|' +ALLTRIM(cInvNo)) > 0))  

RETURN llVldDate
  

*!**************************************************************************
*!
*!      Function: lfGetNoOfInst
*!
*!**************************************************************************
*
FUNCTION lfGetNoOfInst
PRIVATE lcCurAlias, lnCountPrds, ldNextDate

lcCurAlias  = ALIAS()
lnCountPrds = 0
ldNextDate  = dAutNxGen

*E300692,1 CHANGE FILE NAME FROM SYCFSPRO TO FSPRO
*SELECT SYCFSPRD

SELECT FSPRD
*E300692,1 end

*E300789,4 IHB Remove company ID [start]
*IF SEEK(gcPrnt_Cmp)
GO TOP
IF !EOF()
*E300789,4 IHB [end]

  *E300692,1 CHANGE FILE NAME FROM SYCFSPRO TO FSPRO
  *LOCATE REST FOR BETWEEN(ldNextDate, SYCFSPRD.dFspPBgDt, SYCFSPRD.dFspPEnDt)
  LOCATE REST FOR BETWEEN(ldNextDate, FSPRD.dFspPBgDt, FSPRD.dFspPEnDt)
  *E300692,1 end
  IF FOUND()
  
    *B600624,1 Include the generation date in the scan condition
    *SCAN REST WHILE SYCFSPRD.cComp_ID = gcPrnt_Cmp;
            .AND. ldNextDate < ldGenDate
    *B600650,1 Change the SCAN loop to DO WHILE        
    *SCAN REST WHILE SYCFSPRD.cComp_ID = gcPrnt_Cmp;
            .AND. ldNextDate <= ldGenDate
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
    *DO WHILE SYCFSPRD.cComp_ID = gcPrnt_Cmp;
            .AND. ldNextDate <= ldGenDate
    
    *E300789,4 IHB Remove company ID [start]
    *DO WHILE FSPRD.cComp_ID = gcPrnt_Cmp;
            .AND. ldNextDate <= ldGenDate
    DO WHILE ldNextDate <= ldGenDate
    *E300789,4 IHB [end]
            
  *E300692,1 end                    
    *B600624,1 end.
      *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
      *ldNextDate  = ldNextDate  + ;
                   (SYCFSPRD.dFspPEnDt - SYCFSPRD.dFspPBgDt) + 1
      ldNextDate  = ldNextDate  + ;
                    (FSPRD.dFspPEnDt - FSPRD.dFspPBgDt) + 1                   
      *E300692,1 end            
                   
      lnCountPrds = lnCountPrds + 1
      *B600650,1 (START) After moving the next period make sure that the next date falls in it
      SKIP 1
      IF !BETWEEN(ldNextDate, dFspPBgDt, dFspPEnDt)
        ldNextDate =   dFspPEnDt
      ENDIF
    ENDDO
*    ENDSCAN          
    *B600650,1 (END)
  ENDIF  
ENDIF

SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)

RETURN lnCountPrds

*!**************************************************************************
*!
*!      Function: lfGetNextDate
*!
*!**************************************************************************
*
*FUNCTION lfGetNextDate
*PARAMETERS ldNxtDate, lnNumOfPrds
*PRIVATE lcCurAlias, lnCountDays, lnCountPrds

*lcCurAlias  = ALIAS()
*STORE 0 TO lnCountDays, lnCountPrds

*SELECT SYCFSPRD

*IF SEEK(gcPrnt_Cmp)
*  SCAN REST WHILE SYCFSPRD.cComp_ID = gcPrnt_Cmp;
*            .AND. lnCountPrds < lnNumOfPrds
*    lnCountDays = lnCountDays  + ;
*                   (SYCFSPRD.dFspPEnDt - SYCFSPRD.dFspPBgDt) + 1
*    lnCountPrds = lnCountPrds + 1
*  ENDSCAN            
*ENDIF

*SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)

*ldNxtDate = IIF(lnCountPrds = lnNumOfPrds, ;
                ldNxtDate + lnCountDays, {})
*RETURN ldNxtDate   
******************

FUNCTION lfGetNextDate
PARAMETERS ldNxtDate, lnNumOfPrds
PRIVATE lcCurAlias, lnCountDays, lnCountPrds

lcCurAlias  = ALIAS()
STORE 0 TO lnCountDays, lnCountPrds
*E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
*SELECT SYCFSPRD

SELECT FSPRD
*E300692,1 end

*E300789,4 IHB Remove company ID [start]
*IF SEEK(gcPrnt_Cmp)
GO TOP
IF !EOF()
*E300789,4 IHB [end]

  *B600650,1 Locate for the right period
  LOCATE REST FOR BETWEEN(ldNxtDate, dFspPBgDt, dFspPEnDt)

  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
  *lnCur_Pdr = VAL(SYCFSPRD.cFspPrdID)
  *SCAN REST WHILE SYCFSPRD.cComp_ID = gcPrnt_Cmp;
              .AND. lnCountPrds < lnNumOfPrds
    *lnCountDays = lnCountDays  + ;
                   (SYCFSPRD.dFspPEnDt - SYCFSPRD.dFspPBgDt) + 1
  
    lnCur_Pdr = VAL(FSPRD.cFspPrdID)

    *E300789,4 IHB Remove company ID [start]
    *SCAN REST WHILE FSPRD.cComp_ID = gcPrnt_Cmp;
              .AND. lnCountPrds < lnNumOfPrds
    SCAN REST WHILE lnCountPrds < lnNumOfPrds
    *E300789,4 IHB [end]
              
    lnCountDays = lnCountDays  + ;
                (FSPRD.dFspPEnDt - FSPRD.dFspPBgDt) + 1
    *E300692,1 end                
    lnCountPrds = lnCountPrds + 1
  ENDSCAN            
ENDIF

ldNxtDate = IIF(lnCountPrds = lnNumOfPrds, ;
                ldNxtDate + lnCountDays, {})

*B600650,1 (START) Make sure that we did not jump a period while calculating the next date
= SEEK(gcPrnt_Cmp)
LOCATE REST FOR BETWEEN(ldNxtDate, dFspPBgDt, dFspPEnDt)
*E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
*DO WHILE lnCur_Pdr + lnNumOfPrds < VAL(SYCFSPRD.cFspPrdID)
DO WHILE lnCur_Pdr + lnNumOfPrds < VAL(FSPRD.cFspPrdID)
*E300692,1 end
  SKIP -1
  ldNxtDate =   dFspPEnDt
ENDDO
*B600650,1  (END)

SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
              
RETURN ldNxtDate   


*!**************************************************************************
*!
*!      Function: lfvPopups
*!
*!**************************************************************************
*
FUNCTION lfvPopups
PARAMETERS lcArrName, lcDispFld, lnStRow, lnStCol, lnPopWidth

PRIVATE lcRetVal, lnPopLen

DO CASE
  CASE _DOS
    lnPopLen     = ALEN((lcArrName),1)
    lcRetVal     = gfActPop(lnStRow, lnStCol, ;
                            IIF(lnPopLen>5,lnStRow + 7,lnStRow + 2 + lnPopLen),;
                            lnStCol + 3 + lnPopWidth,;
                            lcArrName, 2, 1, @lcDispFld)
  CASE _WINDOWS
    lcRetVal    = IIF(EVALUATE(SYS(18)) > 0,;
                      &lcArrName[EVALUATE(SYS(18)),2]," ")
                      
ENDCASE
RETURN lcRetVal

*!**************************************************************************
*!
*!        Function : lfShowInv
*!
*!**************************************************************************
* shows the following objects, 
FUNCTION lfShowInv
PARAMETERS lcStatOfDisp
PRIVATE lcRevDisp

lcSlPr  = lcTSelect
lcRevDisp = IIF('DISABLE' $ lcStatOfDisp, 'ENABLE', 'DISABLE')

SHOW GETS &lcStatOfDisp 
SHOW GET pbSelect,1  &lcStatOfDisp PROMPT lcTSelect
SHOW GET pbGenerate  &lcRevDisp
SHOW GET ldGenDate  ENABLE
SHOW GET rbScope    ENABLE
SHOW GET pbClose    ENABLE



*!**************************************************************************
*!
*!      Function: lfGetExt
*!
*!**************************************************************************
* Valid function for get field laData[43] representing factor
*
FUNCTION lfGetExt
PARAMETERS lcSrchFile, lcSrchTag, lcOldVal, llBrowse,;
           lcFldName, laTemp, lcSrchFlds, lcDataDir 
*** Old factor value
*** .T. if browsed from browsing invisible button
PRIVATE lcSrchVal, lcSrchObj, lcCurAlias, lcSavBrFld, lcSavTitle,;
        lcTempSrch, lnClosRec, llSrchFound

lcSrchObj  = SYS(18)
lcSrchVal  = ALLTRIM(EVALUATE(lcSrchObj))

llSrchFound  = .T.
IF llBrowse .OR. lcSrchVal <> lcOldVal 
  laTemp     = ' '
  IF llBrowse .OR. !EMPTY(lcSrchVal)
    lcSrchFlds = IIF(EMPTY(lcSrchFlds), " ", STRTRAN(lcSrchFlds,' '))
    lcDataDir  = IIF(EMPTY(lcDataDir), gcDataDir, lcDataDir)
    lcCurAlias = ALIAS()

    lcTempSrch = gfTempName()
    SELECT 0
    USE (lcDataDir+lcSrchFile) ORDER TAG (lcSrchTag) AGAIN ALIAS (lcTempSrch)

    lcSrchVal  = PADR(lcSrchVal, FSIZE(lcFldName, lcTempSrch))
    &lcSrchObj = lcSrchVal
    SHOW GET (lcSrchObj)

    IF llBrowse .OR. !SEEK(lcSrchVal, lcTempSrch)
      *** If a record is to be selected 
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      lnClosRec  = RECNO(0)
    
      *** Get browse fields from dictionary
      =lfGetBrF(@lcFile_Ttl, @lcBrFields, lcSrchFile,lcSrchFlds)
      IF BETWEEN(lnClosRec,1,RECCOUNT())
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF        

      =gfBrows(.F.,lcSrchFlds,'laTemp')
         
      lcBrFields=lcSavBrFld
      lcFile_Ttl=lcSavTitle
    
      *** If a selection occurs
      IF !EMPTY(laTemp[1])
        &lcSrchObj  = laTemp[1]
        llSrchFound = .T.      
      ELSE
        &lcSrchObj  = lcOldVal
        llSrchFound = .F.       
      ENDIF
      SHOW GET (lcSrchObj)
    ELSE
      =gfSubStr(lcSrchFlds,@laTemp,",")
      FOR lnCount = 1 TO ALEN(laTemp)
        laTemp[lnCount] = &laTemp[lnCount]
      ENDFOR  
      llSrchFound = .T.         
    ENDIF
    IF USED(lcTempSrch)
      USE IN (lcTempSrch)
    ENDIF
    SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
  ENDIF
ENDIF
RETURN llSrchFound


*!**************************************************************************
*!
*!      Procedure: lpTab
*!
*!**************************************************************************
*
PROCEDURE lpTab
IF _DOS
  HIDE MENU _MSYSMENU
  SHOW MENU _MSYSMENU
ENDIF  
ON KEY LABEL TAB lnDummmi = 1
DO CASE
  CASE WONTOP(lcAprInCh1) .AND. _CUROBJ = OBJNUM(ldGenDate) 
    =lfvGenDate()
    _CUROBJ = _CUROBJ + 1

  CASE WONTOP(lcAprInCh1) .AND.;
    (llMayGenr .AND. _CUROBJ = OBJNUM(rbScope) + 1;
    .OR. !llMayGenr .AND. _CUROBJ = OBJNUM(pbInvoice))     
    ACTIVATE WINDOW (lcBrInvTtl)

  CASE WONTOP(lcBrInvTtl)
    ACTIVATE WINDOW (lcAprInCh3)
    _CUROBJ = IIF(llMayGenr, OBJNUM(pbClose), OBJNUM(pbSelect))

  CASE WONTOP(lcAprInCh3) .AND. _CUROBJ = OBJNUM(pbClose)
    ACTIVATE WINDOW (lcAprInCh1)
    _CUROBJ = OBJNUM(ldGenDate)
    
  OTHERWISE
    _CUROBJ = _CUROBJ + 1
ENDCASE  
ON KEY LABEL TAB DO lpTab

*!**************************************************************************
*!
*!      Procedure: lpShiftTab
*!
*!**************************************************************************
*
PROCEDURE lpShiftTab
IF _DOS
  HIDE MENU _MSYSMENU
  SHOW MENU _MSYSMENU
ENDIF  
ON KEY LABEL BACKTAB lnDummmi = 1

DO CASE
  CASE WONTOP(lcAprInCh1)
    DO CASE
      CASE _CUROBJ = OBJNUM(ldGenDate) 
        =lfvGenDate()
        SHOW GET ldGenDate
        ACTIVATE WINDOW (lcAprInCh3)
        _CUROBJ = OBJNUM(pbClose)
      CASE !llMayGenr .AND. _CUROBJ = OBJNUM(rbScope)
        _CUROBJ = OBJNUM(ldGenDate)
      OTHERWISE
        _CUROBJ = _CUROBJ - 1 
    ENDCASE    

  CASE WONTOP(lcBrInvTtl)
    ACTIVATE WINDOW (lcAprInCh1)
    _CUROBJ = IIF(llMayGenr, OBJNUM(rbScope) + 1, OBJNUM(pbInvoice))

  CASE WONTOP(lcAprInCh3) .AND. ;
     (llMayGenr .AND. _CUROBJ = OBJNUM(pbClose);
     .OR. !llMayGenr .AND. _CUROBJ = OBJNUM(pbSelect))
    ACTIVATE WINDOW (lcBrInvTtl)
    
  OTHERWISE
    _CUROBJ = _CUROBJ - 1
ENDCASE  
ON KEY LABEL BACKTAB DO lpShiftTab

*!**************************************************************************
*!
*!      Function: lfvVenTotals
*!
*!**************************************************************************
* Get totals of the vendor summary screen.
FUNCTION lfvVenTotals
SELECT APINVHDR
IF SEEK(&lc_Invoices..cVendCode)
  SUM REST APINVHDR.NINVAMTAP,APINVHDR.NINVDISAP,APINVHDR.NINVADJAP;
       TO  lnTotPay          ,lnTotDisc         ,lnTotAdj ;
       WHILE APINVHDR.CVENDCODE = &lc_Invoices..cVendCode
ENDIF
SELECT APINVAHD
