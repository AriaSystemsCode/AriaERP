*!**************************************************************************
*! Name      : DAVMAIN.PRG
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 12/12/2002
*! Purpose   : David Luke Ltd Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*! C200431,1 REA,TMI Quick order entry screen for sales order 
*! C200443,1 SSE 12/24/2002 Custom add early date option to Sales Order Invoice screen.
*! C200452,1 TMI 01/16/2003 Quick order entry screen for PO module
*! B606927,1 TMI 02/03/2003 Solve some problems for C#200431
*! B606931,1 KHM 02/05/2003 Fix the bug of not refreshing the charges field.
*! B606945,1 ASH 02/16/2003 The checking of completion dates is not working properly.
*! C200500,1 ASH 02/17/2003 Don't apply the Earliest Invoice Date on the Invoice Date and Posting Date,
*! C200500,1                and apply it only on the due date.
*! B607077,1 KHM 03/24/2003 To zero the nCustCharg field when removing the charages.
*! C200520,1 ABD 03/30/2003 Add a navigator buttons in this screen (First,
*! C200520,1 ABD            Last, Next and Previous) in order to display more
*! C200520,1 ABD            Than 10 sizes buckets.
*! C200496,1 ABD 04/09/2003 Add new code to handle customer Statement form A 
*! C200496,1 ABD            For David Luke.
*! B607189,1 ABD 04/22/2003 Quick Order Entry Screen goes into a loop!.
*! B607215,1 ABD 01/05/2003 Stop calling the lfCollhist function that call from 
*! B607215,1 ABD             the standard customer statement program , and made 
*! B607215,1 ABD             A new customer statement for this customer.
*! C200540,1 ABD 05/27/2003 add trigger while save the invoice to update the invhdr.pikdate
*! C200540,1 ABD from custom ship date filed from the pack_hdr file.
*! B607374,1 KHM 07/03/2003 Fix the bug of adding a charge record in case of totchg > min order
*! C200551,1 TMI 07/02/2003 Changes in the Quick order entry screen
*! C200551,1 TMI              1) Show customer notes at order entry
*! C200551,1 TMI              2) Add date range to the style pricing screen
*! C200551,1 TMI              3) Increase the number of customer pricing codes to 15
*! C200551,3 TMI 07/16/2003 Fix the bug that if the first style is cancelled the program refuse 
*! C200551,3                all rest colours, also if one colour other than the first it must be refused
*! B607371,1 TMI 07/22/2003 Fix the bug of replace Colordv with only 5 char. It should be 6
*! C200587,1 TMI 08/05/2003 1)Update completion date for a style in ordline with Sold out date based on a new parameter
*! C200587,1 TMI            2)Warn the user if the style.soldout date is earlier than the entered date            
*! C200587,1 TMI            3)Allow quick ordr entry screen to work with standard case and extended case
*! B119490,1 TMI 08/31/2003 Fix the bug that if two price code with the same date range but different 
*! B119490,1 TMI            styles,the standard case always returns the first one
*! B607491,1 ABD 09/04/2003 Problem in enter customer price screen in the style program.
*! B119914,1 ABD 10/08/2003 Fix bug that quick order entry screen is picking up the style 
*! B119914,1 ABD            Standard price not the defined price which is defined in customer price.
*! C037252,1 ABD 12/01/2003 When the first invoice is generated for a sales order it automatically 
*! C037252,1 ABD            Updates ORDHDR.LCARRGCHRG to 'Yes' even if carriage has not been 
*! C037252,1 ABD            Charged on that first invoice.  [Begin]
*! C037727,1 TMI 02/23/2004 Allow adding styles in SO only for those assinged to the customer of the Sales order if the customer field in style is filled with this customer
*! C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*! B122433,1 TMI 05/09/2004 Add season check based on setup option ( Customized for MBI )
*! B124083,1 TMI 11/11/2004 Add style field to the key index of CSTPRICH file
*! C123853,1 MHM 01/15/2005 modification in Custom Bin Location For David Luke
*! C123847,1 TMI 01/03/2005 new way to browse data for DIR03. They use extended with two dimenstions , and sizes are the 
*                           same for all dims, and need to show each dim in one line in the quick order entry screen
*! C126994,1 NNA 05/26/2005 modification in David Luke's Custom Bin Location to Use it for GPS00
*! B128798,1 MMR 07/05/2005 Fix the bug of Not Updating Bin location with Returns in case of 
*! B128798,1 MMR            damaged and 2nd Quality.  
*! B128987,1 MHM 07/14/2005 Fix the bug of not working correct if transfer from warehouse to another
*! B128755,1 TMI 07/31/2005 Fix a bug that wrong calculating total line 
*! B129431,1 TMI 08/14/2005 If the style is non-inv style , do not update binlocation files with its data
*! B129429,1 MHM 08/14/2005 Fix bug of Incorrect bin location update with invoice sales order
*! B129334,1 NNA 08/29/2005 Fix bug that if you deleted a style you find that it didn't deleted from the CSTPRICE file
*! B129334,1 NNA            so this Causes a problem when a style is deleted and then recrated with a different price scale or
*! B129334,1 NNA            a colour is deleted and then added - as the Customer Pricing Screen shows the prices from the 
*! B129334,1 NNA            deleted records and not the new records
*! C128481,1 NNA 09/05/2005 Amendment to CP#200443 (Auto Carriage Charging on Invoice) By adding Charge for Currancy EURO
*! B128959,1 NNA 09/04/2005 Fix bug that Style with Inv.Style Not Checked not invoiced if onhand = 0
*! C128481,1 NNA 09/12/2005 Use the Same Trigger as in CP#126994
*! B129283,1 NNA 09/21/2005 Fix bug that quick order entry screen not updating open qty Correctly
*! B039782,1 MHM 10/15/2005 Add new index to PKBINLOC and IVBINLOC and collect data on both new index
*! B130409,1 NNA 11/22/2005 Fix a bug that CP#126994 does not work correctly if there is a shipment with Two orders with the 
*! B130409,1 NNA            Same Style in both
*! B130690,1 TMI 12/21/2005 Fix a bug at DCC that when release a picktickit and repick in the same session the data in whbinloc goes wrong
*! B131115,1 MHM 02/19/2006 Fix a bug that CP#126994 does not work correctly if there is a Po with Style added twoith
*! B132036,1 NNA 06/22/2006 fix bug that if you create an invoice with the all avilable stock for a style , the whbinloc file
*! B132036,1 NNA            not deleted
*! B131267,1 NNA 06/22/2006 Fix bug of incorrect updating in case of voiding Credit memo
*! B132317,1 TMI 07/10/2006 use price level based on customer price level setup in customr screen , if at qty level use priceA
*! B132317,1 TMI 07/16/2006 let complete date be read only , if sold out date is empty take the order complete date
*! C132754,1 TMI 07/19/2006 Bin replenshment update
*T20061016.0001 TMI 11/28/2006 Consider the case if the bin location is not installed at all
*! B607992,1 TMI 03/04/2007 If Binlocation not installed then the trigger "ARCKSVBN" must return .T. 
*! B608092,1 TMI 05/23/2007 update the nStkVal with nCost*nTotStk for more accuracy   ( ticket T20070305.0001 )
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'
***ash1
*=gfItemMask(@laMajSeg)
*lnMajSeg  = gfItemMask('SM')
*lnNonSeg  = gfItemMask('SN')  
*lcItemTl  = gfItemMask('HI')
*lcMjrTtl  = gfItemMask('HM')
*lcNMjrTl  = gfItemMask('HN')
*lcMjrPct  = gfItemMask('PM')
*lcNMjrPt  = gfItemMask('PN')
*lcIMjrPt  = gfItemMask('PI')
*lnstylewid=LEN(lcMjrPct)
*lncolorwid=LEN(lcNMjrPt)
*lcSepart  =SUBSTR(lcIMjrPt,lnstylewid+1,1)

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfDavMenu
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/12/2002
*! Purpose   : Add a new entry to the Option Menu - "Enter Customer Prices".
*!**************************************************************************

FUNCTION lfDavMenu

*-- This Option Menu will be enable in the Edit mode.
DEFINE BAR 10 OF _lPopOpt PROMPT "\-" SKIP FOR .T.
*C200431,4 TMI [Start] fix that The bar is enabled in select mode
*DEFINE BAR 11 OF _lPopOpt PROMPT "\<Enter Customer Prices" SKIP FOR (laScrMode[2]) OR llallcolors
DEFINE BAR 11 OF _lPopOpt PROMPT "\<Enter Customer Prices" SKIP FOR (!laScrMode[3]) OR llAllColors

*C037816,1 MHM 04/06/2004 Define New Bar To Style Screen[Start]
DEFINE BAR 12 OF _lPopOpt PROMPT "\<Bin Locations" SKIP FOR (laScrMode[1]) OR llAllColors OR ALLTRIM(lcWareCode) = "All"
ON SELECTION BAR 12 OF _lPopOpt DO lfvBinBrow IN DAVMAIN
*C037816,1 MHM 04/06/2004 [End]

*C200431,4 TMI [End  ] 

*!**************************************************************************
*! Name      : lfOptnBar
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/12/2002
*! Purpose   : Call the Customer Prices screen.
*!**************************************************************************

FUNCTION lfOptnBar

DECLARE laBrowArr[1]
STORE '' TO laBrowArr , lcSz1     , lcSz2    , lcSz3     , lcSz4   , lcSz5    ,;
             lcSz6    , lcSz7     , lcSz8    , lcSz9 , lcSz10

*C123847,2  TMI [Start] define fit variables lcFit1,...,lcFit10 for dimension 
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
  STORE ' ' TO lcFit1,lcFit2,lcFit3,lcFit4,lcFit5,lcFit6,lcFit7,lcFit8,lcFit9,lcFit10
ENDIF 
*C123847,2  TMI [End  ] 

lcTempCst = gfTempName()

*C200520,1 ABD - Create a temp File to hold more then 10 sizes. [Begin]
lcTempSizs = gfTempName()
Create Table (gcWorkDir + lcTempSizs);
       ( cSequnc C(6) , cSize1 c(15) , cSize2 c(15) , cSize3 c(15),;
         cSize4 c(15) , cSize5 c(15) , cSize6 c(15) , cSize7 c(15),;
         cSize8 c(15) , cSize9 c(15) , cSize10 c(15),;
         nPrice1 N(6,2), nPrice2 N(6,2), nPrice3 N(6,2), nPrice4 N(6,2),;
         nPrice5 N(6,2), nPrice6 N(6,2), nPrice7 N(6,2), nPrice8 N(6,2),;
         nPrice9 N(6,2), nPrice10 N(6,2),;
         nComm1  N(6,2), nComm2   N(6,2), nComm3   N(6,2), nComm4   N(6,2),;
         nComm5  N(6,2), nComm6   N(6,2), nComm7   N(6,2), nComm8   N(6,2),;
         nComm9  N(6,2), nComm10  N(6,2))
lcSizSeq = '1'
*C200520,1 ABD - [End]

*-- Open the needed files.
=lfOpenFil()
*-- Create the Temp. file.
=lfTempFil()

*B607491,1 ABD - Add new file to browse the currency code and currency price. [Begin]
lnPrvAlias = SELECT(0)
lcTempCode = gfTempName()
CREATE TABLE (gcWorkDir + lcTempCode);
       ( Stydv C(19),  Priccode C(6) , cCurrcod c(03) )
INDEX ON Stydv + Priccode + cCurrcod TAG (lcTempCode)
SELECT CSTPRICE
lcOldOrder = ORDER()
SET ORDER TO Cstycode
LOCATE
IF SEEK(Style.Style,'CSTPRICE')
  SCAN REST WHILE STYDV = Style.Style
    IF SEEK(Stydv + Priccode + cCurrcod ,lcTempCode)
      LOOP
    ELSE
      INSERT INTO (lcTempCode) (Stydv,Priccode,cCurrcod);
      VALUES (CSTPRICE.Stydv,CSTPRICE.Priccode,CSTPRICE.cCurrcod)
    ENDIF
  ENDSCAN
ENDIF
SET ORDER TO &lcOldOrder
SELECT(lnPrvAlias)
*B607491,1 ABD - [End]


PUSH KEY
ON KEY
=lfSizes()
lcBro_Titl = 'Sizes_Lines'

*C200520,1 ABD - enable and disable the next, previues ... ect , in case first time. [Begin]
lcSzMode = IIF (lcSizSeq = '1',"DISABLE","ENABLE")
ON KEY LABEL CTRL+LEFTARROW  DO lfvPrevius
ON KEY LABEL CTRL+RIGHTARROW DO lfvnext
*C200520,1 ABD - [End]
*C200551,1  TMI [Start] Initialize price range variables
STORE {} TO ldRangeFr,ldRangeTo
*C200551,1  TMI [End  ] 
*B607371,1  TMI [Start] Define variables hold color len and position
PRIVATE lnClrPos,lnClrLen
STORE 0  TO lnClrPos,lnClrLen
=lfGetClrD()
*B607371,1  TMI [End  ] 

*B124083,1  TMI [Start] Get the set value of the variable "V.Dates on Pri.Code only"
* Values available    .T.  :  Validation Dates use Price Code only
*                     .F.  :  Validation Dates use Price Code and styleColor
lM_PRCCOD = gfGetMemvar('M_PRCCOD')
*B124083,1  TMI [End  ] 
*C123847,1  TMI [Start] case if other than DavLuck used davmain and the option M_PRCCOD is not defined for him
lM_PRCCOD = IIF(TYPE('lM_PRCCOD')#'L',.T.,lM_PRCCOD)
*C123847,1  TMI [End  ] 

*C123847,1  TMI [Start] if DIR03 , call the screen ICPRDIR.Spr instead   ( trigger for ICSTYLE program )
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
  DO (gcScrDir+gcWinAppl+"\ICPRDIR.SPX")
  lcTmpSzFit = 'Q'+SUBSTR(lcFolder,2)
  IF USED(lcTmpSzFit)
    USE IN &lcTmpSzFit
  ENDIF  
  ERASE (gcWorkDir+lcTmpSzFit+'.DBF')
  ERASE (gcWorkDir+lcTmpSzFit+'.CDX')
ELSE
  *C123847,1  TMI [End  ] 

  DO (gcScrDir+gcWinAppl+"\ICPRCLS.SPX")
  
  *C123847,1  TMI [Start] 
ENDIF
*C123847,1  TMI [End  ]   
POP KEY
*-- Close the files.
=lfCloseFil()
lnBarNo = 0

*!**************************************************************************
*! Name      : lfTraps
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/12/2002
*! Purpose   : 
*!**************************************************************************

FUNCTION lfTraps

PARAMETERS lnTrap

DO CASE 
  *-- Case TAB
  CASE lnTrap = 1
    IF WONTOP()= lcBro_Titl
      glFromBrow = .T.
      ACTI WINDOW ICPRCLS3 TOP
    ELSE
      _CUROBJ = _CUROBJ +1
    ENDIF
    
  *-- Case BACKTAB
  CASE lnTrap = 2
    ACTI WINDOW ICPRCLS1 TOP
    
  *-- Case ESC
  CASE lnTrap = 3
   DO  lpDetEsc
    
  *-- Case CTRL+END
  CASE lnTrap = 4
    GO BOTTOM
  
  *-- Case CTRL+HOME
  CASE lnTrap = 5
    GO TOP
  
  *-- Case CTRL+ENTER
  CASE lnTrap = 6
    RETURN  
ENDCASE

*!**************************************************************************
*! Name      : lpDetEsc
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/12/2002
*! Purpose   : Trap Esc for lines entry.
*!**************************************************************************

PROCEDURE lpDetEsc

ACTIVATE WINDOW ('gwcContrl1')
_CUROBJ = OBJNUM(pbCls)
KEYBOARD '{ENTER}'
RETURN

*!**************************************************************************
*! Name      : lfvAccCurr
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/12/2002
*! Purpose   : Valid function of Currency code.
*!**************************************************************************

FUNCTION lfvAccCurr
PRIVATE lcCurrency

*IF EMPTY(lcCurrCod) .AND. !llBrowse
*  lcMsg2 = 'You have to select the Currency Code'
*  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
*   _CUROBJ = _CUROBJ
*   _CUROBJ = OBJNUM(lcCurrCod)
*   RETURN
*ENDIF

STORE '' TO lcMessage

IF llBrowse .OR. (!EMPTY(lcCurrCod) .AND. !SEEK(lcCurrCod,'SycCurr') .AND. !SEEK(lcPrcCod+lcCurrCod,'CSTPRICH'))
  lcCurrency = lcCurrCod
  IF !gfCurrBrow(@lcCurrency)
    lcCurrency = lcOLdValue
  ENDIF
  lcCurrCod = lcCurrency
  llBrowse = .F.  
ENDIF
=lfvCurr()
IF EMPTY(lcCurrCod) OR LEFT(lcCurrCod,1)="?"
  RETURN
ENDIF
SHOW GET pbSave ENABLE
SHOW GET pbClr ENABLE
SHOW GET lcPrcCod DISABLE
SHOW GET lcCurrCod DISABLE

*C123847,1  TMI [Start] enable buttons responsible for sizes screen
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
  PRIVATE lnI,lcI
  FOR lnI = 1 TO 10
    lcI = ALLTRIM(STR(lnI))
    IF !EMPTY(lcSz&lcI)
      SHOW GET pbSz&lcI ENABLED
    ENDIF
  ENDFOR
ENDIF 
*C123847,1  TMI [End  ] 

*!**************************************************************************
*! Name      : lfvCurr
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/12/2002
*! Purpose   : Valid function for Currency field.
*!**************************************************************************

FUNCTION lfvCurr
IF EMPTY(lcCurrCod) OR LEFT(lcCurrCod,1)="?"
  RETURN
ENDIF

*B124083,1  TMI [Start] Use style in the key
*IF !SEEK(lcPrcCod+lcCurrCod,'CSTPRICH')
*  lcMessage = 'This Price / Currency code'
PRIVATE lcStyClr,lcStCl
*-tmi 02/24/2005
lM_PRCCOD = IIF(TYPE('lM_PRCCOD')#'L',.T.,lM_PRCCOD)
*-tmi
lcStyClr = IIF(lM_PRCCOD , '' , PADR(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),19) )

*B124083,3  TMI [Start] If they need the add the style to the key when seeking for the price ;
*B124083,3              codes,this code deals with old data , i.e price codes added with no styles
IF !lM_PRCCOD .AND. !SEEK(lcPrcCod+lcCurrCod+lcStyClr,'CSTPRICH')
  IF SEEK(lcPrcCod+lcCurrCod+SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),'CSTPRICE')
    =SEEK(lcPrcCod+lcCurrCod,'CSTPRICH')
    SELECT CSTPRICH
    SCATTER MEMVAR
    INSERT INTO CSTPRICH FROM MEMVAR
    REPLACE STYLE WITH SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1)
  ENDIF
ENDIF
*B124083,3  TMI [End  ] 

IF !SEEK(lcPrcCod+lcCurrCod+lcStyClr,'CSTPRICH')
  lcStCl = IIF(EMPTY(lcStyClr),'',' Style Colour /')
  lcMessage = 'This Price /&lcStCl. Currency code'
  *B124083,1  TMI [End  ] 

  lnOption = gfModalGen('QRM00001B00001','Dialog',lcMessage)

  DO CASE
    CASE lnOption = 1
      *-- Browse Case
      llBrowse = .T.
      =lfvAccCurr()
            
    CASE lnOption = 2
      *-- Add Case
      SELECT CSTPRICH
      *B124083,1  TMI [Start] seek price code with new key with style added
      *IF !SEEK(lcPrcCod+CcurrCod)
      lcCurCd = IIF(lM_PRCCOD,CcurrCod,lcCurrCod)
      IF !SEEK(lcPrcCod+lcCurCd+lcStyClr,'CSTPRICH')
        *B124083,1  TMI [End  ] 
      
        APPEND BLANK
        REPLACE Priccode WITH lcPrcCod  ,;
                CcurrCod WITH lcCurrCod
      ELSE
        REPLACE CcurrCod WITH lcCurrCod
      ENDIF
      *B124083,1  TMI [Start] Update style field
      REPLACE STYLE WITH lcStyClr
      *B124083,1  TMI [End  ] 
      
*      SELECT CSTPRICE
*      IF !SEEK(lcPrcCod+lcCurrCod)
*        APPEND BLANK
*      ENDIF
*      REPLACE Priccode  WITH lcPrcCod  ,;
*              CcurrCod  WITH lcCurrCod ,;
*              Stydv     WITH STYLE.Style   ,;
*              DCrt_Date WITH gdSysDate ,;
*              Colordv   WITH SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-4)
      
      FOR lnI = 1 TO 10
        lcI = ALLT(STR(lnI,2))
        STORE 0 TO lnPrc&lcI
        STORE 0 TO lnComm&lcI
        SHOW GET lnComm&lcI ENABLE
        SHOW GET lnPrc&lcI ENABLE          

      ENDFOR

    CASE lnOption = 3
      *-- Reenter Case
      lcCurrCod = ''
      _CUROBJ = _CUROBJ
  ENDCASE

ELSE

  lcCurrCod = CSTPRICH.cCurrCod
  SHOW GET lcCurrCod

  FOR lnI = 1 TO 10
    lcI = ALLT(STR(lnI,2))
    STORE 0 TO lnPrc&lcI
    STORE 0 TO lnComm&lcI
    SHOW GET lnComm&lcI ENABLE
    SHOW GET lnPrc&lcI ENABLE          
  ENDFOR
  lnI = 1
  *C200551,1  TMI [Start] Get price code date range variables  values
  ldRangeFr = CSTPRICH.DVLDPRFR
  ldRangeTo = CSTPRICH.DVLDPRTO
  SHOW GET ldRangeFr
  SHOW GET ldRangeTo
  *C200551,1  TMI [End  ] 
  SELECT CSTPRICE
  *ash1
  *=SEEK(lcPrcCod+lcCurrCod+STYLE.Style)
  *C200520,1 ABD - Define new flage to know if we arraive to 10 sizes. [ Begin]
  llFirst10 = .T.
  *C200520,1 ABD - [End]
      
  =SEEK(lcPrcCod+lcCurrCod+lcMajor+lcSepart+LEFT(lcNonMjr,LEN(ALLTRIM(lcNonMjr))-3))
  SCAN REST WHILE priccode+ccurrcod+stydv = lcPrcCod+lcCurrCod+lcMajor+lcSepart+LEFT(lcNonMjr,LEN(ALLTRIM(lcNonMjr))-3)
        
    *C200520,1 ABD - Remark the next lines and update the temp file with Price & comm. [Begi]
    *lcI = ALLT(STR(lnI,2))
    *lnPrc&lcI = IIF(!EMPTY(lcSz&lcI),CSTPRICE.Pricedv,0)
    *lnComm&lcI= IIF(!EMPTY(lcSz&lcI),CSTPRICE.Commdv,0)
    *SHOW GET lnComm&lcI ENABLE
    *SHOW GET lnPrc&lcI ENABLE          
    *LnI = 1 + lnI 
    *IF lnI > 10
    *  EXIT
    *ENDIF
         
    IF LnI = 11
      llFirst10 = .F.
      LnI = 1
      lnOldFile = SELECT(0)
      SELECT (lcTempSizs)
      SKIP
      IF EOF()
        EXIT
      ENDIF
      SELECT (lnOldFile)
    ENDIF
    lcI = ALLT(STR(lnI))
    *-- Don't update the showing variable on the screen after 10 sizes save
    *-- it only in the temp file.
      
    IF llFirst10
      lnPrc&lcI = IIF(!EMPTY(lcSz&lcI),CSTPRICE.Pricedv,0)
      lnComm&lcI= IIF(!EMPTY(lcSz&lcI),CSTPRICE.Commdv,0)
      SHOW GET lnComm&lcI ENABLE
      SHOW GET lnPrc&lcI ENABLE
    ENDIF  
        
    lnOldAlias = SELECT(0)
    SELECT (lcTempSizs)
    REPLACE nPrice&lcI WITH CSTPRICE.Pricedv,;
            nComm&lcI  WITH CSTPRICE.Commdv
        
    LnI = 1 + lnI
    SELECT (lnOldAlias)
    *C200520,1 ABD - [End]
  ENDSCAN
      
  lnOldAlias = SELECT(0)
  SELECT (lcTempSizs)
  LOCATE
  SELECT (lnOldAlias)
      
ENDIF

*!**************************************************************************
*! Name      : lfSizes
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/12/2002
*! Purpose   : Call the Customer Prices screen.
*!**************************************************************************

FUNCTION lfSizes

lnAlias = SELECT()

*C200520,1 ABD - Comment the next few line and handel by another way, to show more
*C200520,1 ABD - Than 10 sizes by using the next, previes ... etc. [Begin]
*lnCont = 0
*ASH1
*lcScale = SUBSTR(lcNonMjr,AT('-',lcNonMjr)+1,1)
*lcScale = SUBSTR(RIGHT(ALLTRIM(STYLE.STYLE),3),1,2)
*SELECT SCALE
*LOCATE
*=SEEK('S'+lcScale)
*SCAN REST WHILE type+scale+prepak = 'S' + lcScale  
*  lnCnt  = SCALE.cnt
*  *C200431,1 TMI [Start] 
*  *lcCnt  = STR(lnCnt,1)
*  lcCnt  = ALLT(STR(lnCnt))
*  *C200431,1 TMI [End  ] 
*  lnCont = lnCont + 1
*  *C200431,1 TMI [Start] 
*  *lcCont = STR(lnCont,1)
*  lcCont = ALLT(STR(lnCont))
*  *C200431,1 TMI [End  ] 
*  lcsZ&lcCont = ALLT(ALLT(SCALE.SZ1) + '-' + SCALE.SZ&lcCnt)
*  =lfRefresh('icprcls')
*  *C200431,1 TMI [Start] 
*  IF lnCont = 10
*    EXIT
*  ENDIF
*  *C200431,1 TMI [End  ] 
*ENDSCAN

lnCont = 1
lcScale = SUBSTR(RIGHT(ALLTRIM(STYLE.STYLE),3),1,2)
SELECT SCALE
LOCATE
IF SEEK('S'+lcScale)
  SELECT (lcTempSizs)
  APPEND BLANK
  SELECT SCALE
  SCAN REST WHILE type+scale+prepak = 'S' + lcScale  

    IF lnCont = 11
      SELECT (lcTempSizs)
      APPEND BLANK
      lnCont = 1
      lcSizSeq = ALLTRIM(Str(Val(lcSizSeq) + 1))
    ENDIF

    lnCnt  = SCALE.cnt
    lcCnt  = ALLT(STR(lnCnt))
    lcCont = ALLT(STR(lnCont))
    lcsZ&lcCont = ALLT(ALLT(SCALE.SZ1) + '-' + SCALE.SZ&lcCnt)
    Replace &lcTempSizs..cSize&lcCont  With lcsZ&lcCont,;
            &lcTempSizs..cSequnc       WITH lcSizSeq
    *C123847,2  TMI [Start] 
    IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
      lcFit&lcCont = SCALE.CDIM1
    ENDIF
    *C123847,2  TMI [End  ] 
    =lfRefresh('icprcls')
    lnCont = lnCont + 1
  ENDSCAN
ENDIF

SELECT (lcTempSizs)
LOCATE
FOR I = 1 TO 10
  lcSize  = ALLTRIM(STR(I))
  lcsZ&lcSize = ALLTRIM(&lcTempSizs..cSize&lcSize)
ENDFOR

=lfRefresh('icprcls')

SELECT (lnAlias)
*C200520,1 ABD - [End]

*!**************************************************************************
*! Name      : lfvPrcCod
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/12/2002
*! Purpose   : Valid function of price code.
*!**************************************************************************

FUNCTION lfvPrcCod
PARAMETERS lcPCode

*IF llBrowse &&.OR. (!EMPTY(lcPrcCod) .AND. !SEEK(lcPrcCod,'CSTPRICH'))
  IF llbrowse OR ATC('?',lcPrcCod) <> 0
    llbrowse = .F.
    =lfPCodBrow(@lcPCode)
  ELSE
    lcPCode = lcPrcCod
  ENDIF
  =lfvPCode()
*ENDIF

*!**************************************************************************
*! Name      : lfDavMenu
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/16/2002
*! Purpose   : Add a new entry to the Option Menu - "Enter Customer Prices".
*!**************************************************************************

FUNCTION lfPCodBrow
PARAMETERS lcPCode

DO CASE
  CASE !EMPTY(lcPCode) .AND. SEEK(lcPCode,'CSTPRICH')
    RETURN
  OTHERWISE
  
    *B607491,1 ABD - Save the old alias and browse form the temp file. [Begin]
    lnPrvAlias = SELECT(0)
    SELECT CSTPRICH
    IF !(TYPE('lcTempCode') = 'U') .AND. USED(lcTempCode)
      SELECT (lcTempCode)
    ENDIF
    LOCATE
    *B607491,1 ABD - [End]

    lcOld = lcBrFields
    llStyle = .F. 
    lcBrFields = "Priccode:H='Price Code':R , ccurrcod:H='Currency Code':R"
    =ARIABROW('',"Price Codes",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Priccode','laBrowArr')
    lcPCode = laBrowArr[1]
    lcBrFields = lcOld
    lcPrcCod = lcPCode

    *B607491,1 ABD - Return to old alias. [Begin]
    SELECT(lnPrvAlias)
    *B607491,1 ABD - [End]
    
ENDCASE

*!**************************************************************************
*! Name      : lfCloseFil
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/16/2002
*! Purpose   : Close all the open files.
*!**************************************************************************

FUNCTION lfCloseFil

IF USED('CSTPRICE')
  USE IN CSTPRICE
ENDIF
IF USED('CSTPRICH')
  USE IN CSTPRICH
ENDIF
*ash1
*IF USED('SycCurr')
*  =gfCloseFile('SycCurr')
*ENDIF
IF USED(lcTempCst)
  USE IN (lcTempCst)
ENDIF
ERASE (gcDef_Path+lcTempCst+".DBF")
ERASE (gcDef_Path+lcTempCst+".CDX")
ERASE (gcDef_Path+lcTempCst+".FPT")

*B607491,1 ABD - [Begin]
IF USED(lcTempCode)
  USE IN (lcTempCode)
ENDIF
ERASE (gcWorkDir+lcTempCode+".DBF")
ERASE (gcWorkDir+lcTempCode+".CDX")
ERASE (gcWorkDir+lcTempCode+".FPT")

*B607491,1 ABD - [End]

*!**************************************************************************
*! Name      : lfvSave
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/16/2002
*! Purpose   : Valid function of Save Buttom.
*!**************************************************************************

FUNCTION lfvSave

PRIVATE lnSave , lcClrChng
lcClrChng = SPACE(0)

*C200551,1  TMI [Start] Check valid dates are legal
IF ldRangeFr > ldRangeTo
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The end price valid date can not be less than the starting one.')
  _CUROBJ = OBJNUM(ldRangeTo)
  RETURN
ENDIF
*-tmi 02/24/2005
lM_PRCCOD = IIF(TYPE('lM_PRCCOD')#'L',.T.,lM_PRCCOD)
*-tmi
*- Update the price Valid range dates
*B124083,1  TMI [Start] Add styleColor to the key
IF lM_PRCCOD
  SELECT CSTPRICH
  *B124083,1  TMI [End  ] 
  IF SEEK(lcPrcCod+lcCurrCod,'CSTPRICH')
    REPLACE CSTPRICH.DVLDPRFR WITH ldRangeFr,;
            CSTPRICH.DVLDPRTO WITH ldRangeTo 
  ENDIF            
  *B124083,1  TMI [Start] 
ENDIF
*B124083,1  TMI [End  ] 
*C200551,1  TMI [End  ] 

*B607491,1 ABD - Save the currency code and currency into temp file to browse this new
*B607491,1 ABD - currency code from the temp file.  [Begin]
IF !SEEK(Style.Style + Priccode + cCurrcod ,lcTempCode)
    INSERT INTO (lcTempCode) (Stydv,Priccode,cCurrcod);
    VALUES (Style.Style,lcPrcCod,lcCurrCod)
ENDIF
*B607491,1 ABD - [End]

*-- Would you like to Update prices for all colors ?
*-- 	Yes			No
lnSave = gfModalGen("QRM42251B00006","DIALOG")

llAllColor = IIF(lnSave = 1,.T.,.F.)

lcStyRng = IIF(llAllColor,lcMajor,lcMajor+lcSepart+LEFT(lcNonMjr,LEN(ALLTRIM(lcNonMjr))-3))

*C200520,1 ABD - Select temp file & go to first record. [Begin]
*lnCont = 0
lnCont = 1
SELECT (lcTempSizs)
LOCATE
lcOldColor = ''
*C200520,1 ABD - [End]

SELECT STYLE
lnRecNo = RECNO()

*-tmi 02/24/2005
lM_PRCCOD = IIF(TYPE('lM_PRCCOD')#'L',.T.,lM_PRCCOD)
*-tmi

SCAN FOR STYLE = lcStyRng
  *B124083,1  TMI [Start] Add one line for each color in case lM_PRCCOD is .F. , i.e. use price code with style/color
  IF lM_PRCCOD = .F.
    SELECT CSTPRICH
    IF !SEEK(lcPrcCod+lcCurrCod+ PADR(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),19) ,'CSTPRICH')
      APPEND BLANK
    ENDIF
    REPLACE PRICCODE  WITH lcPrcCod  ;
            CCURRCOD  WITH lcCurrCod ;
            DVLDPRFR  WITH ldRangeFr ;
            DVLDPRTO  WITH ldRangeTo ;
            STYLE     WITH PADR(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),19)
    SELECT STYLE
  ENDIF
  *B124083,1  TMI [End  ] 
  
  lcCurColor = STRTRAN(STYLE,lcMajor)
  lcCurColor = LEFT(lcCurColor,LEN(ALLTRIM(lcCurColor))-3)
  
  *C200520,1 ABD - Remark the next few line, and handle the way to save more then 10 sizes. [Begin]
  *lnCont = lnCont + 1 
  *C200431,1 TMI [Start] Ignore lines more than 10th 
  *IF lnCont > 0
  *IF lnCont > 10  &&tmi
  *  EXIT
  *ENDIF
  IF lnCont = 11
    lnCont = 1
    lnOldFile = SELECT(0)
    SELECT (lcTempSizs)
    SKIP
    IF EOF()
      *EXIT
      LOCATE
    ENDIF
    SELECT (lnOldFile)
  ELSE
    IF lcOldColor # lcCurColor
      lnCont = 1
      lnOldFile = SELECT(0)
      SELECT (lcTempSizs)
      LOCATE
      SELECT (lnOldFile)
    ENDIF  
  ENDIF
  lcCont = ALLTRIM(STR(lnCont))
  lcSz&lcCont   = &lcTempSizs..cSize&lcCont
  lnPrc&lcCont  = &lcTempSizs..nPrice&lcCont
  lnComm&lcCont = &lcTempSizs..nComm&lcCont
  *C200520,1 ABD - [End]
  
  *C200431,1 TMI [End  ]  
  
  IF lcClrChng # lcCurColor AND !EMPTY(lcClrChng)
     lnCont = 1
  ENDIF
  lcClrChng = lcCurColor      
  lcCont = ALLTRIM(STR(lnCont,2))

  SELECT CSTPRICE
  IF !SEEK(lcPrcCod+lcCurrCod+STYLE.Style,'CSTPRICE')    
    APPEND BLANK
    *B607371,1  TMI [Start] replace the variable "lncolorwid" with "lnClrLen,lnClrPos"     
    *REPLACE Priccode  WITH lcPrcCod                                 ,;
            CcurrCod  WITH lcCurrCod                                ,;
            Stydv     WITH STYLE.Style                              ,;
            DCrt_Date WITH gdSysDate                                ,;
            Scaledv   WITH SUBSTR(STYLE.Style,17,3)                 ,;
            Colordv   WITH SUBSTR(STYLE.Style,lnstylewid+2,lncolorwid-4),;
            Pricedv   WITH IIF(!EMPTY(lcSz&lcCont),lnPrc&lcCont,0)  ,;
            Commdv    WITH IIF(!EMPTY(lcSz&lcCont),lnComm&lcCont,0)
    REPLACE Priccode  WITH lcPrcCod                                 ,;
            CcurrCod  WITH lcCurrCod                                ,;
            Stydv     WITH STYLE.Style                              ,;
            DCrt_Date WITH gdSysDate                                ,;
            Scaledv   WITH SUBSTR(STYLE.Style,17,3)                 ,;
            Colordv   WITH SUBSTR(STYLE.Style,lnClrPos,lnClrLen),;
            Pricedv   WITH IIF(!EMPTY(lcSz&lcCont),lnPrc&lcCont,0)  ,;
            Commdv    WITH IIF(!EMPTY(lcSz&lcCont),lnComm&lcCont,0)
    *B607371,1  TMI [End  ]         
  ELSE
    *B607371,1  TMI [Start] replace the variable "lncolorwid" with "lnClrLen,lnClrPos" 
    *REPLACE Priccode  WITH lcPrcCod                                 ,;
            CcurrCod  WITH lcCurrCod                                ,;
            Stydv     WITH STYLE.Style                              ,;
            DCrt_Date WITH gdSysDate                                ,;
            Scaledv   WITH SUBSTR(STYLE.Style,17,3)                 ,;
            Colordv   WITH SUBSTR(STYLE.Style,lnstylewid+2,lncolorwid-4),;
            Pricedv   WITH IIF(!EMPTY(lcSz&lcCont),lnPrc&lcCont,0)  ,;
            Commdv    WITH IIF(!EMPTY(lcSz&lcCont),lnComm&lcCont,0)  
    REPLACE Priccode  WITH lcPrcCod                                 ,;
            CcurrCod  WITH lcCurrCod                                ,;
            Stydv     WITH STYLE.Style                              ,;
            DCrt_Date WITH gdSysDate                                ,;
            Scaledv   WITH SUBSTR(STYLE.Style,17,3)                 ,;
            Colordv   WITH SUBSTR(STYLE.Style,lnClrPos,lnClrLen),;
            Pricedv   WITH IIF(!EMPTY(lcSz&lcCont),lnPrc&lcCont,0)  ,;
            Commdv    WITH IIF(!EMPTY(lcSz&lcCont),lnComm&lcCont,0)  
    *B607371,1  TMI [End  ] 
  ENDIF
  *C123847,1  TMI [Start] if Called from DIR03 then update the fields PRICE1,..., PRICE8
  IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
    =lfUpDirPrc()
  ENDIF
  *C123847,1  TMI [End  ] 
  
  *C200520,1 ABD - Add 1 to the counter and save the old color. [End]
  
  lnCont = lnCont + 1 
  lcCurColor = STRTRAN(Style.Style,lcMajor)
  lcOldColor = LEFT(lcCurColor,LEN(ALLTRIM(lcCurColor))-3)
  *C200520,1 ABD - [End]
  
  *C200431,4 TMI [START] Enable to save in style screen
  llCUpdate = .T.
  *C200431,4 TMI [END  ] 
ENDSCAN
GO lnRecNo
=lfvClear()

*!**************************************************************************
*! Name      : lfvClear
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/16/2002
*! Purpose   : Valid function of Clear Buttom.
*!**************************************************************************

FUNCTION lfvClear

STORE '' TO lcCurrCod , lcPrcCod
SHOW GET lcCurrCod ENABLE
SHOW GET lcPrcCod ENABLE
FOR lnI = 1 TO 10
  lcI = ALLT(STR(lnI,2))
  STORE 0 TO lnPrc&lcI
  STORE 0 TO lnComm&lcI
  SHOW GET lnComm&lcI DISABLE
  SHOW GET lnPrc&lcI DISABLE
ENDFOR
SHOW GET lcPrcCod ENABLE
SHOW GET lcCurrCod ENABLE

SHOW GET pbSave DISABLE
SHOW GET pbClr DISABLE


*C200520,1 ABD - Clear the temp file and get the first sizes. [begin]
lnOldAlias = SELECT (0)
SELECT (lcTempSizs)
LOCATE
SCAN
  FOR I = 1 To 10
    lcNumber = ALLTRIM(STR(I))
    REPLACE nPrice&lcNumber  WITH 0,;
            nComm&lcNumber   WITH 0
  ENDFOR
ENDSCAN
IF ALLTRIM(lcSizSeq) # '1'
  SHOW GET pbNext ENABLE
  SHOW GET PbLast ENABLE
ENDIF
SHOW GET PbFirst   DISABLE
SHOW GET pbPrevius DISABLE

LOCATE
I = 1
FOR I = 1 TO 10
  lcSize = ALLTRIM(STR(I))
  lcSz&lcSize = &lcTempSizs..cSize&lcSize
ENDFOR
=lfRefresh('icprcls')
SELECT(lnOldAlias)
*C200520,1 ABD - [End]


*C200551,1  TMI [Start] Empty date range variables
STORE {} TO ldRangeFr,ldRangeTo
SHOW GET ldRangeFr
SHOW GET ldRangeTo
*C200551,1  TMI [End  ] 

_CUROBJ = _CUROBJ
_CUROBJ = OBJNUM(lcPrcCod)

*C123847,1  TMI [Start] close the file &lcTmpSzF if this is DIR03
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
  lcTmpSzF = 'Q'+SUBSTR(lcFolder,2)
  IF USED(lcTmpSzF)
    USE IN &lcTmpSzF
  ENDIF
  ERASE (gcWorkDir+lcTmpSzF+'.DBF') 
ENDIF 
*C123847,1  TMI [End  ] 

*!**************************************************************************
*! Name      : lfOpenFil
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/16/2002
*! Purpose   : Open all the needed file.
*!**************************************************************************

FUNCTION lfOpenFil

IF !USED('CSTPRICE')
  =gfOpenFile(gcDataDir+'CSTPRICE','CSTPRICE','SH')
ENDIF
IF !USED('CSTPRICH')
  =gfOpenFile(gcDataDir+'CSTPRICH','CSTPRICH','SH')
ENDIF
IF !USED('SycCurr')
  =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
ENDIF

*!**************************************************************************
*! Name      : lfTempFil
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/16/2002
*! Purpose   : Create temp. file.
*!**************************************************************************

FUNCTION lfTempFil

SELECT CSTPRICE
lnFileStru = AFIELDS(laFileStru)
  
CREATE TABLE (lcTempCst) FROM ARRAY laFileStru
INDEX ON priccode+ccurrcod+colordv+scaledv TAG (lcTempCst)



*!**************************************************************************
*! Name      : lfvPCode
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 12/12/2002
*! Purpose   : Call the Customer Prices screen.
*!**************************************************************************

FUNCTION lfvPCode

IF EMPTY(lcPrcCod) OR LEFT(lcPrcCod,1)="?"
  RETURN
ENDIF
IF !SEEK(lcPrcCod,'CSTPRICH')
  lcMessag2 = 'This Price Code'
  lnOption = gfModalGen('QRM00001B00001','Dialog',lcMessag2)

  DO CASE
    CASE lnOption = 1
      *-- Browse Case
      llBrowse = .T.
      =lfPCodBrow(@lcPCode)
      llBrowse = .F.

      =SEEK(lcPrcCod,'CSTPRICH')
      lcCurrCod = CSTPRICH.cCurrCod
      SHOW GET lcCurrCod
*      FOR lnI = 1 TO 10
*        lcI = ALLT(STR(lnI,2))
*        lnPrc&lcI  = CSTPRICE.PriceDv
*        lnComm&lcI = CSTPRICE.CommDv
*        SHOW GET lnComm&lcI ENABLE
*        SHOW GET lnPrc&lcI ENABLE
*     ENDFOR
*      _CUROBJ = OBJNUM(lnPrc1)
*      SHOW GET pbSave ENABLE
*      SHOW GET pbClr ENABLE  

    CASE lnOption = 2
      *-- Add Case
      SELECT CSTPRICH
      =SEEK(lcPrcCod)
      *B124083,1  TMI [Start] if lM_PRCCOD is set to use price code/style (i.e = .F. ), do not add a line here
      *C123847,1  TMI [Start] if the variable lM_PRCCOD is not defined , let it be .T.
      lM_PRCCOD = IIF(TYPE('lM_PRCCOD')#'L',.T.,lM_PRCCOD)
      *C123847,1  TMI [End  ] 
      IF lM_PRCCOD
        *B124083,1  TMI [End  ] 
        APPEND BLANK
        REPLACE Priccode WITH lcPrcCod
        *B124083,1  TMI [Start] close the if statement
      ENDIF
      *B124083,1  TMI [End  ] 
      _CUROBJ = _CUROBJ
      _CUROBJ = OBJNUM(lcCurrCod)
            
    CASE lnOption = 3
      *-- Reenter Case
      lcPrcCod = ''
      _CUROBJ  = _CUROBJ
  ENDCASE
ELSE
  lcCurrCod = CSTPRICH.cCurrCod
  SHOW GET lcCurrCod
ENDIF

*!**************************************************************************
*! Name      : lfSaveDate
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : To save default date in ordhdr field .
*!**************************************************************************
*! Example   : =lfSaveDate()
*!**************************************************************************
*!C200443,1
FUNCTION lfSaveDate
ldDavValue = ldDefInvDate
SAVE ALL LIKE l?DavValu* TO (gcWorkDir+lcInvHdr)
*-- End of lfSaveDate.

*!**************************************************************************
*! Name      : lfEarlyDat
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : To check for early date in ordhdr field.
*!**************************************************************************
*! Example   : =lfEarlyDat()
*!**************************************************************************
*!C200443,1
FUNCTION lfEarlyDat
RESTORE ADDITIVE FROM (gcWorkDir+lcInvHdr)
*C200500,1 ASH 02/17/2003 (Begin) Don't apply the Earliest Invoice Date on the Invoice Date and Posting Date,
*C200500,1                and apply it only on the due date.
*ldDefInvDate = ldDavValue 

IF EMPTY(OrdHdr.dEarlyInvDt)
  RETURN
ENDIF

IF OrdHdr.dEarlyInvDt > ldDavValue 
  *STORE OrdHdr.dEarlyInvDt TO ldDefInvDate , ldDefPstDate 
  ldOldShp = ShipDate 
  REPLACE ShipDate WITH OrdHdr.dEarlyInvDt
  m.ShipDate = OrdHdr.dEarlyInvDt
  m.cTermCode = cTermCode
  =lfGetDueDate()
  REPLACE ShipDate WITH ldOldShp 
  m.ShipDate = ldOldShp 
ENDIF
*C200500,1 ASH 02/17/2003 (End)
*-- End of lfEarlyDat.

*!**************************************************************************
*! Name      : lfEraseMem
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : To erase lcInvHdr MEM file.
*!**************************************************************************
*! Example   : =lfEraseMem()
*!**************************************************************************
*!C200443,1
FUNCTION lfEraseMem
ERASE (gcWorkDir+lcInvHdr+".MEM")
*-- End of lfEraseMem.

*!**************************************************************************
*! Name      : lfCollData
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : Collect data for invoice charges.
*!**************************************************************************
*! Example   : =lfCollData()
*!**************************************************************************
*!C200443,1
*B607374,1 KHM 07/03/2003 (Begin) Changing the name of the function and rewrite it.
*FUNCTION lfCollData
FUNCTION lfCollDatX
*B607374,1 KHM 07/03/2003 (Begin) Changing the name of the function and rewrite it.
IF llIsEngland
  PRIVATE lnCustRec , lcAlias , lnStandChg
  lcAlias = ALIAS()
  DECLARE laStdShipV[1,2]
  lnStandChg = 0
  laStdShipV[1,1] = 'NSTANDCHRG'
  laStdShipV[1,2] = 'lnStandChg'
  =gfRltFld(&lcInvHdr..ShipVia,@laStdShipV,'SHIPVIA')
  RESTORE ADDITIVE FROM (gcWorkDir+lcInvHdr)
  lnCustRec = RECNO('Customer')
  =SEEK(IIF(EMPTY(OrdHdr.Store),'M'+OrdHdr.Account,'S'+OrdHdr.Account+OrdHdr.Store),'Customer')
  SELECT (lcInvHdr)
  REPLACE lCustCharg WITH .T.
  IF Customer.lCharge
    IF EVALUATE(lcInvHdr+'.TotalChg') > lnDavValue
      REPLACE lCustCharg WITH .F. , nCustCharg WITH lnStandChg
    ELSE
      IF OrdHdr.lCarrgChrg
        REPLACE lCustCharg WITH .F. , nCustCharg WITH lnStandChg
      ELSE
        REPLACE lCustCharg WITH .T. , nCustCharg WITH lnStandChg  
        *-- We need to add a record in Charges screen.
        PRIVATE lnCodRecNo , llCodExist
        llCodExist = .T.
        lnCodRecNo = RECNO('CODES')
        m.cChrgCode = lcDavValue 
        IF EMPTY(m.cChrgCode)
          =gfModalGen("TRM000000B00000","DIALOG",'','','There is no Non Mechandise Tax code with description called Carriage.')
          llCodExist = .F.
        ENDIF
        IF llCodExist
          m.cFrgtAcnt = ''
          PRIVATE laChRltFld
          IF gfGetMemVar('M_TAX',gcAct_Comp)='Y'
            PRIVATE lcTaxRate
            DECLARE laChRltFld[1,2]
            STORE '' TO lcTaxRate
            laChRltFld[1,1] = 'CTAXCODE'
            laChRltFld[1,2] = 'lcTaxRate'
            =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
            IF !EMPTY(lcTaxRate)
              laChRltFld[1,1] = 'NTAXRATE'
              laChRltFld[1,2] = 'lcTaxRate'
              =gfRltFld(ALLTRIM(lcTaxRate),@laChRltFld,'CTAXCODE')
              m.nTaxRate = lcTaxRate
            ENDIF            
          ENDIF
          IF gfGetMemVar('M_LINK_GL',gcAct_Comp)='Y'
            PRIVATE lcFrgtAcnt
            DECLARE laChRltFld[1,2]
            STORE '' TO lcFrgtAcnt
            laChRltFld[1,1] = 'CFRGTACNT'
            laChRltFld[1,2] = 'lcFrgtAcnt'
            =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
            m.cFrgtAcnt= lcFrgtAcnt
          ENDIF
          
          *B606931,1 KHM 02/05/2003 (Begin) check if the existance of the record before insert.
          IF !SEEK(m.Order+m.Store+m.PikTkt+lcDavValue,(lcEngChrg))
          *B606931,1 KHM 02/05/2003 (End)
          
          INSERT INTO (lcEngChrg) (Order,PikTkt,cStore) ;
          VALUES (m.Order,m.PikTkt,m.Store)

          *B606931,1 KHM 02/05/2003 (Begin)
          ENDIF
          *B606931,1 KHM 02/05/2003 (End)
          
          =RLOCK()
          SELECT (lcEngChrg)
          REPLACE cChrgCode WITH m.cChrgCode ,;
                  nTaxRate  WITH m.nTaxRate  ,;
                  cFrgtAcnt WITH m.cFrgtAcnt ,;
                  nChrgAmnt WITH lnStandChg
          UNLOCK              
          SELECT (lcInvHdr)
          REPLACE nCariagTax WITH nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100 ,;
                  nChrgTax   WITH nChrgTax + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
        ELSE
          REPLACE lCustCharg WITH .F. , nCustCharg WITH lnStandChg          
        ENDIF  
      ENDIF
    ENDIF      
  ELSE
    REPLACE lCustCharg WITH .F. , nCustCharg WITH lnStandChg
  ENDIF    
  
  *B607077,1 KHM 03/24/2003 (Begin) Check if the total value of SO is greater than the
  *B607077,1                minimum sales order value then replace OrdHdr.lCarrgChrg with .T.
  IF OrdHdr.BookAmt >  lnDavValue
    REPLACE OrdHdr.lCarrgChrg WITH .T.
  ENDIF  
  *B607077,1 KHM 03/24/2003 (End)

  IF BETWEEN(lnCustRec,1,RECCOUNT('Customer'))
    GOTO lnCustRec IN Customer
  ENDIF
  IF !EMPTY(lcAlias)
    SELECT (lcAlias)
  ENDIF
ENDIF  
*-- End of lfCollData.

*!**************************************************************************
*! Name      : lfUpdCharg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : Update charges for invoices.
*!**************************************************************************
*! Example   : =lfUpdCharg()
*!**************************************************************************
*!C200443,1
FUNCTION lfUpdCharg
IF llIsEngland
  PRIVATE lcCurAlias , lcOldInvTg , lnOldInvRn , lcCurInKey , lnCustRec , ;
          lnOrdHdRec , llSavCharg , lnStandChg , llOldCharg , lnOldCharg
  RESTORE ADDITIVE FROM (gcWorkDir+lcInvHdr)        
  DECLARE laStdShipV[1,2]
  lnStandChg = 0
  *C128481,1 NNA 09/05/2005 (Start) in case Sales Order Currency By ERUO then get minimum order value by EURO.
  lnEuroChrg = 0
  IF ORDHDR.CCURRCODE = "EUR"
    laStdShipV[1,1] = 'NEUROCHRG '
    laStdShipV[1,2] = 'lnEuroChrg'
  ELSE
  *C128481,1 NNA (END)
  
    laStdShipV[1,1] = 'NSTANDCHRG'
    laStdShipV[1,2] = 'lnStandChg'

  *C128481,1 NNA (START)
  ENDIF
  *C128481,1 NNA (END)

  lcCurAlias = ALIAS()
  lnCustRec  = RECNO('Customer')
  lnOrdHdRec = RECNO('OrdHdr')
  lcOldInvTg = ORDER(lcInvHdr)
  lnOldInvRn = RECNO(lcInvHdr)

  *C128481,1 NNA 09/05/2005 (Start) Hold the Current Order and Store
  lcCurOrder = &lcInvHdr..ORDER
  lcCurStore = &lcInvHdr..STORE
  *C128481,1 NNA (End)

  SET ORDER TO TAG 'CONSOL' IN (lcInvHdr)
  lcCurInKey = &lcInvHdr..Account + &lcInvHdr..cDivision + &lcInvHdr..cCurrCode
  llSavCharg = .T.
  IF SEEK('N'+lcCurInKey,(lcInvHdr))
    SELECT (lcInvHdr)

    *C128481,1 NNA 09/05/2005 (Start) Scan For the Hold Order and Store to get the correct Record
    *SCAN REST WHILE Consol + Account + cDivision + cCurrCode = 'N' + &lcInvHdr..Account + &lcInvHdr..cDivision + &lcInvHdr..cCurrCode
    SCAN REST WHILE Consol + Account + cDivision + cCurrCode = 'N' + &lcInvHdr..Account + &lcInvHdr..cDivision + &lcInvHdr..cCurrCode ;
         FOR ORDER = lcCurOrder AND STORE = lcCurStore
    *C128481,1 NNA (End)
    
      =gfRltFld(&lcInvHdr..ShipVia,@laStdShipV,'SHIPVIA')  
      =SEEK('O'+&lcInvHdr..Order,'OrdHdr')
      =SEEK(IIF(EMPTY(OrdHdr.Store),'M'+OrdHdr.Account,'S'+OrdHdr.Account+OrdHdr.Store),'Customer')
      *C128481,1 NNA 09/05/2005 (Start) Use OrdHdr.lCarrgChrg instead of Customer.lCharge
      *IF Customer.lCharge
      *  IF EVALUATE(lcInvHdr+'.TotalChg') - IIF(EVALUATE(lcInvHdr+'.lCustCharg'),nCustCharg+nCariagTax,0) > lnDavValue
      *    llOldCharg = lCustCharg
      *    lnOldCharg = nCustCharg
      *    REPLACE lCustCharg WITH .F.
      *    REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,lnOldCharg,0) ,;
      *            nCharges WITH nCharges - IIF(llOldCharg,lnOldCharg,0)
      *    IF SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))          
      *      REPLACE nChrgTax WITH nChrgTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
      *              Tax_Amt WITH Tax_Amt - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
      *              nCariagTax WITH nCariagTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)
      *      REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)
      *    ENDIF
      *    REPLACE Cod_Amt WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
      *    IF llOldCharg AND SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
      *      SELECT (lcEngChrg)
      *      DELETE
      *      SELECT (lcInvHdr)
      *    ENDIF
      *    llSavCharg = .F.
      *  ELSE
      *    IF OrdHdr.lCarrgChrg
      *      llOldCharg = lCustCharg
      *      lnOldCharg = nCustCharg
      *      REPLACE lCustCharg WITH .F.
      *      REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,lnOldCharg,0) ,;
      *              nCharges WITH nCharges - IIF(llOldCharg,lnOldCharg,0)
      *      IF SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))        
      *        REPLACE nChrgTax WITH nChrgTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
      *                Tax_Amt WITH Tax_Amt - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)   ,;
      *                nCariagTax WITH nCariagTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)
      *        REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)
      *      ENDIF
      *      REPLACE Cod_Amt WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
      *      IF llOldCharg AND SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
      *        SELECT (lcEngChrg)
      *        DELETE
      *        SELECT (lcInvHdr)
      *      ENDIF
      *      llSavCharg = .F.
      *    ELSE
      *      *-- We need to add a record in Charges screen.
      *      PRIVATE lnCodRecNo , llCodExist
      *      llCodExist = .T.
      *      lnCodRecNo = RECNO('CODES')
      *      m.cChrgCode = lcDavValue 
      *      IF EMPTY(m.cChrgCode)
      *        =gfModalGen("TRM000000B00000","DIALOG",'','','There is no Non Mechandise Tax code with description called Carriage.')
      *        llCodExist = .F.
      *      ENDIF
      *      IF llCodExist
      *        m.cFrgtAcnt = ''
      *        PRIVATE laChRltFld
      *        IF gfGetMemVar('M_TAX',gcAct_Comp)='Y'
      *          PRIVATE lcTaxRate
      *          DECLARE laChRltFld[1,2]
      *          STORE '' TO lcTaxRate
      *          laChRltFld[1,1] = 'CTAXCODE'
      *          laChRltFld[1,2] = 'lcTaxRate'
      *          =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
      *          IF !EMPTY(lcTaxRate)
      *            laChRltFld[1,1] = 'NTAXRATE'
      *            laChRltFld[1,2] = 'lcTaxRate'
      *            =gfRltFld(ALLTRIM(lcTaxRate),@laChRltFld,'CTAXCODE')
      *            m.nTaxRate = lcTaxRate
      *          ENDIF            
      *        ENDIF
      *        IF gfGetMemVar('M_LINK_GL',gcAct_Comp)='Y'
      *          PRIVATE lcFrgtAcnt
      *          DECLARE laChRltFld[1,2]
      *          STORE '' TO lcFrgtAcnt
      *          laChRltFld[1,1] = 'CFRGTACNT'
      *          laChRltFld[1,2] = 'lcFrgtAcnt'
      *          =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
      *          m.cFrgtAcnt= lcFrgtAcnt
      *        ENDIF
      *        IF !SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
      *          INSERT INTO (lcEngChrg) (Order,PikTkt,cStore) ;
      *          VALUES (m.Order,m.PikTkt,m.Store)
      *        ENDIF  
      *        =RLOCK()
      *        SELECT (lcEngChrg)
      *        REPLACE cChrgCode WITH m.cChrgCode ,;
      *                nTaxRate  WITH m.nTaxRate  ,;
      *                cFrgtAcnt WITH m.cFrgtAcnt ,;
      *                nChrgAmnt WITH lnStandChg
      *        UNLOCK            
      *        SELECT (lcInvHdr)
      *        llOldCharg = lCustCharg
      *        lnOldCharg = nCustCharg
      *        REPLACE lCustCharg WITH .T. , nCustCharg WITH lnStandChg
      *        REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,lnOldCharg,0) + nCustCharg ,;
      *                nCharges WITH nCharges - IIF(llOldCharg,lnOldCharg,0) + nCustCharg
      *        IF SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))        
      *          REPLACE nChrgTax WITH nChrgTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ;
      *                  + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100) ,;              
      *                  Tax_Amt  WITH Tax_Amt - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ;
      *                  + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100) ,;
      *                  nCariagTax WITH nCariagTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ;
      *                  + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
      *          REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ;
      *                  + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
      *        ENDIF
      *        REPLACE Cod_Amt WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
      *      ENDIF  
      *    ENDIF
      *  ENDIF        
      IF OrdHdr.lCartbC
        *--NNA 11/06/2005 (Start)
        *IF EVALUATE(lcInvHdr+'.TotalChg') - IIF(EVALUATE(lcInvHdr+'.lCustCharg'),nCustCharg+nCariagTax,0) > lnDavValue
        IF OrdHdr.BookAmt - IIF(EVALUATE(lcInvHdr+'.lCustCharg'),nCustCharg+nCariagTax,0) > lnDavValue
        *--NNA 11/06/2005 (End)

          llOldCharg = lCustCharg
          lnOldCharg = nCustCharg
          REPLACE lCustCharg WITH .F.
          REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,lnOldCharg,0) ,;
                  nCharges WITH nCharges - IIF(llOldCharg,lnOldCharg,0)
          IF SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))          
            REPLACE nChrgTax WITH nChrgTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
                    Tax_Amt WITH Tax_Amt - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
                    nCariagTax WITH nCariagTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)
            REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)
          ENDIF
          REPLACE Cod_Amt WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
          IF llOldCharg AND SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
            SELECT (lcEngChrg)
            DELETE
            SELECT (lcInvHdr)
          ENDIF
          llSavCharg = .F.
        ELSE
          IF OrdHdr.lCarrgChrg
            llOldCharg = lCustCharg
            lnOldCharg = nCustCharg
            REPLACE lCustCharg WITH .F.
            REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,lnOldCharg,0) ,;
                    nCharges WITH nCharges - IIF(llOldCharg,lnOldCharg,0)
            IF SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))        
              REPLACE nChrgTax WITH nChrgTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
                      Tax_Amt WITH Tax_Amt - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)   ,;
                      nCariagTax WITH nCariagTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)
              REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)
            ENDIF
            REPLACE Cod_Amt WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
            IF llOldCharg AND SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
              SELECT (lcEngChrg)
              DELETE
              SELECT (lcInvHdr)
            ENDIF
            llSavCharg = .F.
          ELSE
            *-- We need to add a record in Charges screen.
            PRIVATE lnCodRecNo , llCodExist
            llCodExist = .T.
            lnCodRecNo = RECNO('CODES')
            m.cChrgCode = lcDavValue 
            IF EMPTY(m.cChrgCode)
              =gfModalGen("TRM000000B00000","DIALOG",'','','There is no Non Mechandise Tax code with description called Carriage.')
              llCodExist = .F.
            ENDIF
            IF llCodExist
              m.cFrgtAcnt = ''
              PRIVATE laChRltFld
              IF gfGetMemVar('M_TAX',gcAct_Comp)='Y'
                PRIVATE lcTaxRate
                DECLARE laChRltFld[1,2]
                STORE '' TO lcTaxRate
                laChRltFld[1,1] = 'CTAXCODE'
                laChRltFld[1,2] = 'lcTaxRate'
                =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
                IF !EMPTY(lcTaxRate)
                  laChRltFld[1,1] = 'NTAXRATE'
                  laChRltFld[1,2] = 'lcTaxRate'
                  =gfRltFld(ALLTRIM(lcTaxRate),@laChRltFld,'CTAXCODE')
                  m.nTaxRate = lcTaxRate
                ENDIF            
              ENDIF
              IF gfGetMemVar('M_LINK_GL',gcAct_Comp)='Y'
                PRIVATE lcFrgtAcnt
                DECLARE laChRltFld[1,2]
                STORE '' TO lcFrgtAcnt
                laChRltFld[1,1] = 'CFRGTACNT'
                laChRltFld[1,2] = 'lcFrgtAcnt'
                =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
                m.cFrgtAcnt= lcFrgtAcnt
              ENDIF
              IF !SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
                INSERT INTO (lcEngChrg) (Order,PikTkt,cStore) ;
                VALUES (m.Order,m.PikTkt,m.Store)
              ENDIF  
              =RLOCK()
              SELECT (lcEngChrg)
              REPLACE cChrgCode WITH m.cChrgCode ,;
                      nTaxRate  WITH m.nTaxRate  ,;
                      cFrgtAcnt WITH m.cFrgtAcnt ,;
                      nChrgAmnt WITH IIF(ORDHDR.CCURRCODE = "EUR",lnEuroChrg,lnStandChg)
              UNLOCK            
              SELECT (lcInvHdr)
              llOldCharg = lCustCharg
              lnOldCharg = nCustCharg
              REPLACE lCustCharg WITH .T. , nCustCharg WITH IIF(ORDHDR.CCURRCODE = "EUR",lnEuroChrg,lnStandChg)
              REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,lnOldCharg,0) + nCustCharg ,;
                      nCharges WITH nCharges - IIF(llOldCharg,lnOldCharg,0) + nCustCharg
              IF SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))        
                REPLACE nChrgTax WITH nChrgTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ;
                        + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100) ,;              
                        Tax_Amt  WITH Tax_Amt - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ;
                        + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100) ,;
                        nCariagTax WITH nCariagTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ;
                        + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
                REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ;
                        + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
              ENDIF
              REPLACE Cod_Amt WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
            ENDIF  
          ENDIF
        ENDIF
      *C128481,1 NNA (End)
      ELSE
        llOldCharg = lCustCharg
        lnOldCharg = nCustCharg
        REPLACE lCustCharg WITH .F.
        REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,lnOldCharg,0) ,;
                nCharges WITH nCharges - IIF(llOldCharg,lnOldCharg,0)
        IF SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
          REPLACE nChrgTax WITH nChrgTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
                  Tax_Amt  WITH Tax_Amt - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)  ,;
                  nCariagTax WITH nCariagTax - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)
          REPLACE TotalChg WITH TotalChg - IIF(llOldCharg,(lnOldCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0)
        ENDIF
        REPLACE Cod_Amt WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
        IF llOldCharg AND SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
          SELECT (lcEngChrg)
          DELETE
          SELECT (lcInvHdr)
        ENDIF      
        llSavCharg = .F.
      ENDIF
    ENDSCAN  
    IF llSavCharg
      IF SEEK('Y'+lcCurInKey,(lcInvHdr))
        SELECT (lcInvHdr)
        REPLACE lCustCharg WITH .T. ,;
                nCharges   WITH nCharges - nCustCharg ,;
                TotalChg   WITH TotalChg - nCustCharg - nCariagTax ,;
                nChrgTax WITH nChrgTax - nCariagTax ,;
                Tax_Amt  WITH Tax_Amt  - nCariagTax ,;
                nCariagTax WITH 0 ,;
                Cod_Amt  WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)                  
        PRIVATE lnChrgTax , lnTotCharg
        STORE 0 TO lnChrgTax , lnTotCharg
        =SEEK('N'+m.Account+m.cDivision+m.cCurrCode)
        SCAN REST WHILE Consol+Account+cDivision+cCurrCode = 'N'+m.Account+m.cDivision+m.cCurrCode
          =SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
          lnChrgTax = lnChrgTax + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
          lnTotCharg = lnTotCharg + nCustCharg
        ENDSCAN
        IF SEEK('Y'+lcCurInKey,(lcInvHdr))
          REPLACE nCharges WITH nCharges + lnTotCharg ,;
                  TotalChg WITH TotalChg + lnTotCharg + lnChrgTax ,;
                  nChrgTax WITH nChrgTax + lnChrgTax ,;
                  Tax_Amt  WITH Tax_Amt  + lnChrgTax ,;
                  nCariagTax WITH lnChrgTax ,;
                  nCustCharg WITH lnTotCharg ,;
                  Cod_Amt  WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)                  
          m.nCharges = m.nCharges
          m.TotalChg = TotalChg
          m.Tax_Amt = Tax_Amt      
        ENDIF
      ENDIF
    ELSE    && reject lines
      IF SEEK('N'+lcCurInKey,(lcInvHdr))
        SELECT (lcInvHdr)
        PRIVATE lnChrgTax , lnTotCharg
        STORE 0 TO lnChrgTax , lnTotCharg
        *=SEEK('N'+m.Account+m.cDivision+m.cCurrCode)
        SCAN REST WHILE Consol+Account+cDivision+cCurrCode = 'N'+m.Account+m.cDivision+m.cCurrCode
          =SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
          m.TotalChg = TotalChg
          m.nCharges = m.nCharges
          m.Tax_Amt = Tax_Amt
          lnChrgTax = lnChrgTax + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
          lnTotCharg = lnTotCharg + nCustCharg
          IF lCustCharg
            m.nCharges = nCharges - nCustCharg
            REPLACE TotalChg   WITH TotalChg - nCustCharg - (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100) ,;
                    nCharges   WITH nCharges - nCustCharg ,;
                    nChrgTax   WITH nChrgTax - (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100) ,;
                    Tax_Amt    WITH Tax_Amt  - (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100) ,;
                    nCariagTax WITH nCariagTax - (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100) ,;
                    Cod_Amt    WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0) ,;
                    lCustCharg WITH .F.
            m.TotalChg = TotalChg
            m.nCharges = m.nCharges
            m.Tax_Amt = Tax_Amt
          ENDIF          
          IF SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
            SELECT (lcEngChrg)
            DELETE
          ENDIF          
        ENDSCAN
        IF SEEK('Y'+lcCurInKey,(lcInvHdr))
          REPLACE lCustCharg WITH .F. ,;
                  nCharges   WITH nCharges - nCustCharg ,;
                  TotalChg WITH TotalChg - nCustCharg - nCariagTax ,;
                  nChrgTax WITH nChrgTax - nCariagTax ,;
                  Tax_Amt  WITH Tax_Amt  - nCariagTax ,;
                  nCariagTax WITH 0 ,;
                  Cod_Amt    WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0) ,;
                  nCustCharg WITH lnTotCharg
          m.nCharges = m.nCharges
          m.TotalChg = TotalChg
          m.Tax_Amt = Tax_Amt      
        ENDIF
      ENDIF  
    ENDIF
  ENDIF
  SET ORDER TO TAG (lcOldInvTg) IN (lcInvHdr)
  IF BETWEEN(lnOldInvRn,1,RECCOUNT(lcInvHdr))
    GOTO lnOldInvRn IN (lcInvHdr)
    m.TotalChg = TotalChg
    
    *B606931,1 KHM 02/05/2003 (Begin) Re-Initializing the memory variables.
    m.Tax_Amt  = Tax_Amt
    m.nCharges = nCharges
    *B606931,1 KHM 02/05/2003 (End)

  ENDIF
  IF BETWEEN(lnCustRec,1,RECCOUNT('Customer'))
    GOTO lnCustRec IN Customer
  ENDIF
  IF BETWEEN(lnOrdHdRec,1,RECCOUNT('OrdHdr'))
    GOTO lnOrdHdRec IN OrdHdr
  ENDIF
  IF !EMPTY(lcCurAlias)
    SELECT (lcCurAlias)
  ENDIF
ENDIF  
*-- End of lfUpdCharg.
*!**************************************************************************
*! Name      : lfAddField
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : Update charges for invoices.
*!**************************************************************************
*! Example   : =lfAddField()
*!**************************************************************************
*!C200443,1
FUNCTION lfAddField
IF llIsEngland
  *! C123853,1 MHM 01/15/2005  let array count from array length not from program[Start]
  *DIMENSION laFileStru[lnFileStru+17,4]
  *laFileStru[lnFileStru+15,1] = 'lCustCharg'
  *laFileStru[lnFileStru+15,2] = 'L'
  *laFileStru[lnFileStru+15,3] = 1
  *laFileStru[lnFileStru+15,4] = 0
  *laFileStru[lnFileStru+16,1] = 'nCustCharg'
  *laFileStru[lnFileStru+16,2] = 'N'
  *laFileStru[lnFileStru+16,3] = 20
  *laFileStru[lnFileStru+16,4] = 8
  *laFileStru[lnFileStru+17,1] = 'nCariagTax'
  *laFileStru[lnFileStru+17,2] = 'N'
  *laFileStru[lnFileStru+17,3] = 14
  *laFileStru[lnFileStru+17,4] = 2
  
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+3,4]
  laFileStru[lnFileStru+1,1] = 'lCustCharg'
  laFileStru[lnFileStru+1,2] = 'L'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'nCustCharg'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 20
  laFileStru[lnFileStru+2,4] = 8
  laFileStru[lnFileStru+3,1] = 'nCariagTax'
  laFileStru[lnFileStru+3,2] = 'N'
  laFileStru[lnFileStru+3,3] = 14
  laFileStru[lnFileStru+3,4] = 2
  
  *! C123853,1 MHM 01/15/2005  [End]
ENDIF  
*-- End of lfAddField.

*!**************************************************************************
*! Name      : lfSingleIn
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : Update Single invoice (not consolidated).
*!**************************************************************************
*! Example   : =lfSingleIn()
*!**************************************************************************
*!C200443,1
FUNCTION lfSingleIn
IF llIsEngland AND lnInvoices <= 1
  RESTORE ADDITIVE FROM (gcWorkDir+lcInvHdr)
  PRIVATE lnOldAlias , lnChrgRec , lnOldRecNo 
  lnOldAlias = ALIAS()
  lnChrgRec = RECNO(lcEngChrg)
  lnOldRecNo = RECNO(lcInvHdr)
  SCAN REST WHILE Consol+Account+cDivision+cCurrCode = lcKey  
    =SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))

    *C128481,1 NNA 09/12/2005 (Begin) Check if the Customer not Exempted to calc. a vat
    *REPLACE TotalChg   WITH TotalChg + IIF(lCustCharg,nCustCharg,0) + IIF(lCustCharg,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
            nCharges   WITH nCharges + IIF(lCustCharg,nCustCharg,0) ,;
            nChrgTax   WITH nChrgTax + IIF(lCustCharg,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
            Tax_Amt    WITH Tax_Amt  + IIF(lCustCharg,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
            nCariagTax WITH IIF(lCustCharg,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
            Cod_Amt    WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
    *--Check if the Customer main account or the store is exemted
    lnCuRecord = RECNO('CUSTOMER')
    IF !EMPTY(Store)
      =SEEK('S'+Account+Store,'CUSTOMER')
      llVatExm = Customer.lvatexem 
    ELSE
      =SEEK('M'+Account,'CUSTOMER')
      llVatExm = Customer.lvatexem 
    ENDIF
    GOTO lnCuRecord in Customer
    REPLACE TotalChg   WITH TotalChg + IIF(lCustCharg,nCustCharg,0) + IIF(lCustCharg AND !llVatExm,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
            nCharges   WITH nCharges + IIF(lCustCharg,nCustCharg,0) ,;
            nChrgTax   WITH nChrgTax + IIF(lCustCharg AND !llVatExm,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
            Tax_Amt    WITH Tax_Amt  + IIF(lCustCharg AND !llVatExm,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
            nCariagTax WITH IIF(lCustCharg ,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
            Cod_Amt    WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
    *C128481,1 NNA (End)
  ENDSCAN
  IF BETWEEN(lnOldRecNo,1,RECCOUNT(lcInvHdr))
    GOTO lnOldRecNo IN (lcInvHdr)
  ENDIF    
  IF BETWEEN(lnChrgRec,1,RECCOUNT(lcEngChrg))
    GOTO lnChrgRec IN (lcEngChrg)
  ENDIF  
  IF !EMPTY(lnOldAlias)
    SELECT (lnOldAlias)
  ENDIF
ENDIF
*-- End of lfSingleIn.
*!**************************************************************************
*! Name      : lfAddConsl
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : Add the charges when adding the consol. line in ConsInvH.
*!**************************************************************************
*! Example   : =lfAddConsl()
*!**************************************************************************
*!C200443,1
FUNCTION lfAddConsl

*C128481,1 NNA 09/12/2005 (Begin) Check if the Customer is David Luke or Panache
lnOldAlias = SELECT(0)
SELECT(lcInvHdr)
lnInvRec = RECNO(lcInvHdr)
LOCATE FOR lCustCharg = .T.
IF FOUND() AND ASCAN(laEvntTrig , PADR('CHRGINV',10)) <> 0
  m.lCustCharg = .T.
ENDIF
IF BETWEEN(lnInvRec,1,RECCOUNT(lcInvHdr))
  GOTO lnInvRec IN (lcInvHdr)
ENDIF
SELECT(lnOldAlias)
*C128481,1 NNA (End)

IF llIsEngland
  RESTORE ADDITIVE FROM (gcWorkDir+lcInvHdr)
  =SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
  REPLACE lCustCharg WITH m.lCustCharg , nCustCharg WITH m.nCustCharg
ENdIF  
*-- End of lfAddConsl.

*!**************************************************************************
*! Name      : lfUpdConsl
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : Update the charges when adding the consol. line in ConsInvH.
*!**************************************************************************
*! Example   : =lfUpdConsl()
*!**************************************************************************
*!C200443,1
FUNCTION lfUpdConsl
IF llIsEngland
  REPLACE lCustCharg WITH IIF(lCustCharg,m.lCustCharg,.F.) , ;
          nCustCharg WITH nCustCharg + m.nCustCharg , ;
          nChrgTax   WITH nChrgTax + m.nChrgTax , ;
          nCariagTax WITH nCariagTax + m.nCariagTax
ENDIF
*-- End of lfUpdConsl.
*!**************************************************************************
*! Name      : lfTotalChg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : Update the charges after finishing the consol. line in ConsInvH.
*!**************************************************************************
*! Example   : =lfTotalChg()
*!**************************************************************************
*!C200443,1
FUNCTION lfTotalChg
IF llIsEngland
  *C128481,1 NNA 09/12/2005 (Begin) Check if the Customer not Exempted to calc. a vat
  *REPLACE TotalChg WITH TotalChg + IIF(lCustCharg,nCustCharg,0) + IIF(lCustCharg,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
          nCharges WITH nCharges + IIF(lCustCharg,nCustCharg,0) ,;
          Tax_Amt  WITH Tax_Amt  + IIF(lCustCharg,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
          Cod_Amt  WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
  lnCuRecord = RECNO('CUSTOMER')
  IF !EMPTY(Store)
    =SEEK('S'+Account+Store,'CUSTOMER')
    llVatExm = Customer.lvatexem 
  ELSE
    =SEEK('M'+Account,'CUSTOMER')
    llVatExm = Customer.lvatexem 
  ENDIF
  GOTO lnCuRecord in Customer
  REPLACE TotalChg WITH TotalChg + IIF(lCustCharg,nCustCharg,0) + IIF(lCustCharg,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
          nCharges WITH nCharges + IIF(lCustCharg,nCustCharg,0) ,;
          Tax_Amt  WITH Tax_Amt  + IIF(lCustCharg AND !llVatExm,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100),0) ,;
          Cod_Amt  WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0)
  *C128481,1 NNA (End)
ENDIF          
*-- End of lfTotalChg.
*!**************************************************************************
*! Name      : lfUpdAllIn
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : Update all lines for all invoices.
*!**************************************************************************
*! Example   : =lfUpdAllIn()
*!**************************************************************************
*!C200443,1
FUNCTION lfUpdAllIn
IF llIsEngland
  PRIVATE lnOldRecNo , llSavCharg , lcOldAlias
  lcOldAlias = ALIAS()
  lnOldRecNo = RECNO(lcInvHdr)
  =SEEK('Y'+m.Account+m.cDivision+m.cCurrCode)
  llSavCharg = EVALUATE(lcInvHdr+'.lCustCharg')

  *C128481,1 NNA 09/12/2005 (Begin) Check if the Customer is David Luke or Panache
  llSavCharg = IIF(ASCAN(laEvntTrig , PADR('CHRGINV',10)) = 0,llSavCharg,.T.)
  *C128481,1 NNA (End)

  RESTORE ADDITIVE FROM (gcWorkDir+lcInvHdr)
  =SEEK('N'+m.Account+m.cDivision+m.cCurrCode)
  SCAN REST WHILE Consol+Account+cDivision+cCurrCode = 'N'+m.Account+m.cDivision+m.cCurrCode

    *C128481,1 NNA 09/12/2005 (Begin) Check if the Customer not Exempted to calc. a vat
    *REPLACE TotalChg WITH TotalChg + IIF(llSavCharg,nCustCharg,0) ;
                                   + IIF(llSavCharg,nCustCharg*(1-Trde_Disc/100)*(&lcEngChrg..nTaxRate/100),0) ,; 
            nCharges WITH nCharges + IIF(llSavCharg,nCustCharg,0) ,;
            nChrgTax WITH nChrgTax + IIF(llSavCharg,nCustCharg*(1-Trde_Disc/100)*(&lcEngChrg..nTaxRate/100),0) ,;   
            Tax_Amt  WITH Tax_Amt  + IIF(llSavCharg,nCustCharg*(1-Trde_Disc/100)*(&lcEngChrg..nTaxRate/100),0) ,;
            nCariagTax WITH IIF(llSavCharg,nCustCharg*(1-Trde_Disc/100)*(&lcEngChrg..nTaxRate/100),0) ,;
            Cod_Amt  WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0) ,;
            lCustCharg WITH llSavCharg
    lnCuRecord = RECNO('CUSTOMER')
    IF !EMPTY(Store)
      =SEEK('S'+Account+Store,'CUSTOMER')
      llVatExm = Customer.lvatexem 
    ELSE
      =SEEK('M'+Account,'CUSTOMER')
      llVatExm = Customer.lvatexem 
    ENDIF
    GOTO lnCuRecord in Customer
    REPLACE TotalChg WITH TotalChg + IIF(llSavCharg,nCustCharg,0) ;
                                   + IIF(llSavCharg,nCustCharg*(1-Trde_Disc/100)*(&lcEngChrg..nTaxRate/100),0) ,; 
            nCharges WITH nCharges + IIF(llSavCharg,nCustCharg,0) ,;
            nChrgTax WITH nChrgTax + IIF(llSavCharg,nCustCharg*(1-Trde_Disc/100)*(&lcEngChrg..nTaxRate/100),0) ,;   
            Tax_Amt  WITH Tax_Amt  + IIF(llSavCharg AND !llVatExm,nCustCharg*(1-Trde_Disc/100)*(&lcEngChrg..nTaxRate/100),0) ,;
            nCariagTax WITH IIF(llSavCharg,nCustCharg*(1-Trde_Disc/100)*(&lcEngChrg..nTaxRate/100),0) ,;
            Cod_Amt  WITH IIF(Cod_Flag='Y',ShipAmt+nCharges+Tax_Amt+Discount,0) ,;
            lCustCharg WITH llSavCharg
    *C128481,1 NNA (End)
    
    IF SEEK(&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+lcDavValue,(lcEngChrg))
      SELECT (lcEngChrg)
      IF !llSavCharg
        DELETE
      ENDIF  
    ENDIF  
  ENDSCAN
  IF BETWEEN(lnOldRecNo,1,RECCOUNT(lcInvHdr))
    GOTO lnOldRecNo IN (lcInvHdr)
  ENDIF
  IF !EMPTY(lcOldAlias)
    SELECT (lcOldAlias)
  ENDIF
ENDIF  
*-- End of lfUpdAllIn.

*!**************************************************************************
*! Name      : lfGetVar
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : Get initial variables used in Sales Order Invoice.
*!**************************************************************************
*! Example   : =lfGetVar()
*!**************************************************************************
*!C200443,1
FUNCTION lfGetVar
IF llIsEngland
  PRIVATE lcAlias , lnDavValue , laStdShipV , ldOldDate , ldDavValue
  lcAlias = ALIAS()
  ldDavValue = {}
  RESTORE ADDITIVE FROM (gcWorkDir+lcInvHdr)
  lnDavValue = 0

  *C128481,1 NNA 09/05/2005 (Start) Get the Minimum Order Value in Case Order Currency by ERUO.
  *lnDavValue = gfGetMemVar('M_MINORDER' ,gcAct_Comp)
  lnDavValue = IIF(ORDHDR.CCURRCODE='EUR',gfGetMemVar('M_EURMNORD' ,gcAct_Comp),gfGetMemVar('M_MINORDER' ,gcAct_Comp))
  *C128481,1 NNA (End)

  PRIVATE lnCodRecNo
  lnCodRecNo = RECNO('CODES')
  lcDavValue = ''
  IF SEEK('N'+'CCHRGCODE','Codes')
    SELECT Codes

    *C128481,1 NNA 09/12/2005 (BEGIN) Get the Setup Value for Non Merchandise Charge (Postage for Panache and Carriage for David Luke)
    lcPostage = ''
    lcPostage = gfGetMemVar('M_POSTAGE' ,gcAct_Comp)
    *C128481,1 NNA (End)

    SCAN REST WHILE cDefCode + cFld_Name + cCode_No + cDiscrep + cRltd_Nam = 'NCCHRGCODE'
     
      *C128481,1 NNA 09/12/2005 (BEGIN) IF the order's currancy in "EUR" then get the Postage Charges (for Panache)
      *IF UPPER(cDiscrep) = PADR('CARRIAGE',30,' ')
      IF UPPER(cDiscrep) = IIF(lcPostage='P',PADR('SMALL ORDER CHARGE',30,' '),PADR('CARRIAGE',30,' '))
      *C128481,1 NNA (END)

        lcDavValue = cCode_No
      ENDIF
    ENDSCAN
  ENDIF
  IF BETWEEN(lnCodRecNo,1,RECCOUNT('Customer'))
    GOTO lnCodRecNo IN Codes
  ENDIF
  SAVE ALL LIKE l?DavValu* TO (gcWorkDir+lcInvHdr)
  IF !EMPTY(lcAlias)
    SELECT (lcAlias)
  ENDIF
ENDIF  
*-- End of lfGetVar.
*!**************************************************************************
*! Name      : lfSavLChrg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/31/2002
*! Purpose   : Save the charges flag in Order Header file.
*!**************************************************************************
*! Example   : =lfSavLChrg()
*!**************************************************************************
*!C200443,1
FUNCTION lfSavLChrg
IF llIsEngland

  *C037252,1 ABD - When the first invoice is generated for a sales order it automatically 
  *C037252,1 ABD - Updates ORDHDR.LCARRGCHRG to 'Yes' even if carriage has not been 
  *C037252,1 ABD - charged on that first invoice.  [Begin]
  *REPLACE lCarrgChrg WITH &lcHdrFile..lCustCharg
  REPLACE OrdHdr.lCarrgChrg WITH .T.
  *C037252,1 ABD - [End]

ENDIF  
*-- End of lfSavLChrg.

*:**************************************************************************
*:* Name        : lfQKORDDEF
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/08/2006
*:* Purpose     : Bar definition for the QKORD trigger for Davmain
*:***************************************************************************
*:* Called from : SOORD.PRG
*:***************************************************************************
*132754
FUNCTION lfQKORDDEF
PRIVATE lnBarNo
lnBarNo = CNTBAR('_INQURYPOP') + 1
DEFINE BAR lnBarNo   OF _INQURYPOP PROMPT 'Quic\<k Order Entry'  SKIP FOR (lnActFolder<>2) .OR. (laScrMode[2])

ON SELECTION BAR lnBarNo   OF _INQURYPOP DO lfDoQKORD IN DAVMAIN.PRG
*-- end of lfQKORDDEF.


*:**************************************************************************
*:* Name        : lfQKORDDEF
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/08/2006
*:* Purpose     : Bar definition for the QKORD trigger for Davmain
*:***************************************************************************
*:* Called from : SOORD.PRG
*:***************************************************************************
*132754
FUNCTION lfDoQKORD

IF lfQKORD()
  =lpShow()
  =lfRefresh(lcWinCh2) 
ENDIF

*-- end of lfDoQKORD.


*:***************************************************************************
*:* Name        : lfQkOrd                                         
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/26/2002
*:* Purpose     : Open Quick order entry screen
*:***************************************************************************
*:* Called from : soord
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfQkOrd()
*:***************************************************************************
*C#200431,1
FUNCTION lfQkOrd
*C123851,3  TMI [Start] Add a parameter to allow to show the quick order entry 
*                       screen when the employee has no uniform assigned
PARAMETERS llFrmEntSc
*C123851,3  TMI [End  ] 
PRIVATE laSize,laTot,m.Style,m.TotQty,m.Desc1,m.Gros_Price,m.Disc_Pcnt,m.Price,m.Comm1,;
        lcScrTtl,lcDet_Ttl,lnClrLen,lnClrPos,lcClrSpr,lcTempCur,lcTempNote,lcAlias,lnMrk ,;
        lcQkWin0,lcQkWin1,lcQkWin2,lcQkWin3,lcQkWin4,lcOldValue,lcSepart,lcItemPct,laExtSz,lcOrd,llEdit,;
        llChang,laStyClQty,llDifPrice,llShw1,laOldVal,lcBrowFlds

*-- laSize      : Array to Hold Sizes descriptions for the current style scale
*-- laTot       : Array to hold Column sum and nTotal field sum
*-- m.Style     : Style
*-- m.TotQty    : Total qty - check qty
*-- m.Desc1     : Description
*-- m.Gros_Price: Gross price
*-- m.Disc_Pcnt : Dicount percent
*-- m.Price     : Style price
*-- m.Comm1     : Sales rep commesion 
*-- lcScrTtl    : Screen title
*-- lcDet_Ttl   : Screen title
*-- lnClrLen    : Color lenght in style structure
*-- lnClrPos    : Color position in style structure
*-- lcClrSpr    : Color separator 
*-- lcTempCur   : Temp cursor name
*-- lcTempNote  : Temp name for Notes Alias
*-- lcAlias     : Alias Variable
*-- lnMrk       : Marker used in browse
*-- lcQkWin0..4 : Temp variables for screens
*-- lcOldValue  : Variable to hold old value
*-- lcSepart    : Variable to hold style separator
*-- lcItemPct   : Variable to hold Style Picture
*-- laExtSz     : Array to hold Sizes
*-- lcOrd       : Variable to hold order
*-- llEdit      : Edit mode
*-- llChang     : if .T. then qtys are changed 
*-- laStyClQty  : Array hold Style-Color Qty - usefull in edited styles, 0 in added ones
*-- laOldVal    : Array to Hold old Values of the browse
*-- lcBrowFlds  : Variable to hold browse fields for the main browse screen for colors and sizes
*-- llUsePrFl   : if to use the special customer price file in standard sales order entry 
*-- llCodPrice  : Variable to show If this customer has a price code and this stylemajor is linked to a price code for current customer
*-- lnFldGrp    : No of group of fields that appear currently in browse
*-- lcPhyStk    : String holds fields contains physical stock that displayed down in the screen
*-- lcFreStk    : String holds fields contains Free stock that displayed down in the screen
*-- lnBrFlCn    : Browse fields count (My design now is 10 fields )
*-- lnCntSeen   : Count of fields seen in the totals screen
*-- lcPRICCODE  : Variable holds the customer price code for main account
*-- llShw1      : if all sizes has the same price call "lfQkPrice" only once , this case happen if
*--               at least one style-color-size-scale has an entry in "CSTPRICE"
*-- llDifPrice  : .T. if scales has diffrent prices

*C123851,3  TMI [Start] if an entitlment line is added then prevent any standard lines ( for DIR03 )
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 .AND. !llFrmEntSc
  IF lfEntlOrd()
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Quick Order Entry Screen not available.')
    RETURN .F.
  ENDIF
ENDIF 
*C123851,3  TMI [End  ] 


SELECT (lcOrdLine)

SCATTER MEMVAR BLANK
DIMENSION laSize[1],laTot[1,2],laExtSz[1,2],laStyClQty[1,3],laOldVal[1]
STORE '' TO laSize,lcOldValue,lcBrowFlds,lcClrSpr,lcPhyStk,lcFreStk,lcPRICCODE
STORE 0 TO laTot,lnClrLen,lnClrPos,m.Gros_Price,m.Price,m.Disc_Pcnt,laOldVal
STORE 1 TO lnMrk,lnFldGrp,lnCntSeen,lnSclNum
STORE .F. TO llEdit,llChang,llDifPrice,llCodPrice
STORE .T. TO llShw1
lnBrFlCn = 10
DIMENSION laPhyStk[lnBrFlCn+1],laFreStk[lnBrFlCn+1],laShowPen[11,2]
STORE 0 TO laPhyStk,laFreStk
laShowPen = 'RGB(192,192,192)'
*C123847,1  TMI [Start] 
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
  DIMENSION laPlnStk[lnBrFlCn+1]
  STORE 0 TO laPlnStk
  STORE '' TO lcPlnStk
ENDIF
*C123847,1  TMI [End  ] 

m.COMPLETE = laData[10]
lcItemPct = gfItemMask('PI')
lcSepart  = SUBSTR(lcItemPct,lnMajorLen+1,1)
*-- Get color position and color length 
=lfGetClrD()
  
lcScrTtl  = 'Quick Sales Order Entry Screen.'
lcDet_Ttl = 'Quick Lines Entry Screen'

lcQkWin0    = gftempname()     &&Temp name for Windows.
lcQkWin1    = gftempname()     &&Temp name for Windows.
lcQkWin2    = gftempname()     &&Temp name for Windows.
lcQkWin3    = gftempname()     &&Temp name for Windows.
lcQkWin4    = gftempname()     &&Temp name for Windows.
lcTempCur   = gfTempName()     && Cursor holds qty /color/size
lcTempNote  = gfTempName()     && Cursor holds Notes

PUSH KEY
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDet_Ttl) 
ON KEY LABEL TAB
ON KEY LABEL BACKTAB DO lpBacktab 
*--Define keys to navigate throgh the browse by keys
ON KEY LABEL CTRL+LEFTARROW  DO lfPrNxBrow WITH -1
ON KEY LABEL CTRL+RIGHTARROW DO lfPrNxBrow WITH 1 
ON KEY LABEL CTRL+UPARROW    DO lfGoTop
ON KEY LABEL CTRL+DNARROW    DO lfGoBtm
ON KEY LABEL ESC             DO lfvQkClose
ON KEY LABEL CTRL+R          DO lfvqClear WITH .T.
ON KEY LABEL CTRL+S          DO lfvQkSav

lcAlias = SELECT(0)
lcOrd = ORDER(lcOrdLine)   
SET ORDER TO ORDLINST IN (lcOrdLine)
*--CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)

*--Open files
=lfOpnFls()
*B606927,1 TMI [Start] Save order for Ordline file
PRIVATE lcSvOrdLn
lcSvOrdLn = ORDER('ORDLINE')
SET ORDER TO ORDLINES IN ORDLINE
*B606927,1 TMI [End  ] 
*C200587,1  TMI [Start] Obtain the variables that Check if style.soldout date is earlier than order entered date
PRIVATE llCMP2SOLD,llWRNSLDEN
llCMP2SOLD = gfGetMemVar('M_CMP2SOLD')
llWRNSLDEN = gfGetMemvar('M_WRNSLDEN')
*- if this company is Extended size scale
llExtSS    = gfGetMemVar('M_USEEXSSC')
*C200587,1  TMI [End  ] 

*-- Create temp notepad file
CREATE TABLE (gcWorkDir+lcTempNote) (NOTE_MEM M)
APPEND BLANK
*-- Create temp colors , sizes file
*C200520,1 ABD - Don't send any Paramater to the create temp screen, we will create 
*C200520,1 ABD - One file with fixed structure and diel with it. [Begin]
*=lfCrtTmp(1)
=lfCrtTmp()
*C200520,1 ABD - [End]

*B124083,1  TMI [Start] Get the set value of the variable "V.Dates on Pri.Code only"
* Values available    .T.  :  Validation Dates use Price Code only
*                     .F.  :  Validation Dates use Price Code and styleColor
lM_PRCCOD = gfGetMemvar('M_PRCCOD')
*B124083,1  TMI [End  ] 
*C123847,1  TMI [Start] case if other than DavLuck used davmain and the option M_PRCCOD is not defined for him
lM_PRCCOD = IIF(TYPE('lM_PRCCOD')#'L',.T.,lM_PRCCOD)
*C123847,1  TMI [End  ] 

*C123847,1  TMI [Start] 
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
  lcCSTYNOT = ''
  DO (gcScrDir+'SO\DirQOrd.SPX')
ELSE  
  *C123847,1  TMI [End  ] 
  DO (gcScrDir+'SO\QuickOrd.SPX')
  *C123847,1  TMI [Start] 
ENDIF
*C123847,1  TMI [End  ]     
POP KEY
*B606927,1 TMI [Start] restore order for Ordline file
SET ORDER TO &lcSvOrdLn IN ORDLINE
*B606927,1 TMI [End  ] 

SELECT (lcOrdLine)
SET ORDER TO &lcOrd IN (lcOrdLine)
GO BOTTOM
SELECT (lcAlias)
*-- end of lfQkOrd.

*:**************************************************************************
*:* Name        : lfOpnFls										
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/29/2002
*:* Purpose     : Open files
*:***************************************************************************
*C#200431,1
FUNCTION lfOpnFls
IF !USED("CSTPRICE")
  =gfOpenFile(gcDataDir+"CSTPRICE","CSTPRICE","SH")
ENDIF  
IF !USED("CSTPRICH")
  =gfOpenFile(gcDataDir+"CSTPRICH","CSTPRICH","SH")
ENDIF  
IF !USED("CUSTOMER")
  =gfOpenFile(gcDataDir+"CUSTOMER","CUSTOMER","SH")
ENDIF  
*B606927,1 TMI [START] Open file styprice
IF !USED('STYPRICE')
  =gfOpenFile(gcDataDir+'STYPRICE','STYPRICE','SH')
ENDIF
*B606927,1 TMI [START] Open file styprice

*-- end of lfOpnFls.

*:**************************************************************************
*:* Name        : lfCrtTmp                                     		
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2002
*:* Purpose     : Create temp file to browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrtTmp()
*:***************************************************************************
*C#200431,1
FUNCTION lfCrtTmp

*C200520,1 ABD - Remark the sended paramter no need for it. [Begin]
*PARAMETERS lnFlds
*C200520,1 ABD - [End]

PRIVATE lnFl,lnJ,lcJ,lcCrtTbl,laTblFld
DIMENSION laTblFld[1,4]
lnFl = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "LASTLINE"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 1
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "STYMAJOR"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = lnMajorLen
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "COLOR"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = lnClrLen
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "COLORDSC"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 20
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "NTOTAL"
laTblFld[lnFl,2] = "N"
laTblFld[lnFl,3] = 6
laTblFld[lnFl,4] = 0

*-- Separate each group of fields to ease trace
*C200520,1 ABD - Remark the next lines , we will not create a fileds depend on ,
*C200520,1 ABD - Number of scale we will create a 10 field for needed field. [Begin]
*FOR lnJ = 1 TO lnFlds
*  lcJ = ALLT(STR(lnJ))
*  lnFl = lnFl + 1
*  DIMENSION laTblFld[lnFl,4]
*  laTblFld[lnFl,1] = "S&lcJ"
*  laTblFld[lnFl,2] = "N"
*  laTblFld[lnFl,3] = 5
*  laTblFld[lnFl,4] = 0
*ENDFOR
*FOR lnJ = 1 TO lnFlds
*  lcJ = ALLT(STR(lnJ))
*  lnFl = lnFl + 1
*  DIMENSION laTblFld[lnFl,4]
*  laTblFld[lnFl,1] = "FreStk&lcJ"
*  laTblFld[lnFl,2] = "N"
*  laTblFld[lnFl,3] = 6
*  laTblFld[lnFl,4] = 0
*ENDFOR

FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "S&lcJ"
  laTblFld[lnFl,2] = "N"
  laTblFld[lnFl,3] = 5
  laTblFld[lnFl,4] = 0
ENDFOR

FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "FreStk&lcJ"
  laTblFld[lnFl,2] = "N"
  laTblFld[lnFl,3] = 6
  laTblFld[lnFl,4] = 0
ENDFOR

FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "PhyStk&lcJ"
  laTblFld[lnFl,2] = "N"
  laTblFld[lnFl,3] = 6
  laTblFld[lnFl,4] = 0
ENDFOR

FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "Size&lcJ"
  laTblFld[lnFl,2] = "C"
  laTblFld[lnFl,3] = 6
  laTblFld[lnFl,4] = 0
ENDFOR

FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "GPric&lcJ"
  laTblFld[lnFl,2] = "N"
  laTblFld[lnFl,3] = 12
  laTblFld[lnFl,4] = 2
ENDFOR

FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "Comm&lcJ"
  laTblFld[lnFl,2] = "N"
  laTblFld[lnFl,3] = 12
  laTblFld[lnFl,4] = 2
ENDFOR

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "cSequnc"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 6
laTblFld[lnFl,4] = 0
*C200520,1 ABD - [End]

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "TotFreStk"
laTblFld[lnFl,2] = "N"
laTblFld[lnFl,3] = 7
laTblFld[lnFl,4] = 0

*C200520,1 ABD - don't create a physical fields depend on the scale-sizes. [Begin]
*FOR lnJ = 1 TO lnFlds
*  lcJ = ALLT(STR(lnJ))
*  lnFl = lnFl + 1
*  DIMENSION laTblFld[lnFl,4]
*  laTblFld[lnFl,1] = "PhyStk&lcJ"
*  laTblFld[lnFl,2] = "N"
*  laTblFld[lnFl,3] = 6
*  laTblFld[lnFl,4] = 0
*ENDFOR
*C200520,1 ABD - [End]

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "TotPhyStk"
laTblFld[lnFl,2] = "N"
laTblFld[lnFl,3] = 7
laTblFld[lnFl,4] = 0

*B606927,1 TMI [Start] Add a memo field to hold prices
lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "SCLPRC"
laTblFld[lnFl,2] = "M"
laTblFld[lnFl,3] = 10
laTblFld[lnFl,4] = 0
*B606927,1 TMI [End  ] 

*C123847,1  TMI [Start] Add 10 fields to hold scale of this size
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "FIT"
  laTblFld[lnFl,2] = "C"
  laTblFld[lnFl,3] = 5
  laTblFld[lnFl,4] = 0
  
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "FITNO"
  laTblFld[lnFl,2] = "N"
  laTblFld[lnFl,3] = 1
  laTblFld[lnFl,4] = 0
  
  FOR lnJ = 1 TO 10
    lcJ = ALLT(STR(lnJ))
    lnFl = lnFl + 1
    DIMENSION laTblFld[lnFl,4]
    laTblFld[lnFl,1] = "SCALE" + lcJ
    laTblFld[lnFl,2] = "C"
    laTblFld[lnFl,3] = 4
    laTblFld[lnFl,4] = 0
  ENDFOR
  
  FOR lnJ = 1 TO 10
    lcJ = ALLT(STR(lnJ))
    lnFl = lnFl + 1
    DIMENSION laTblFld[lnFl,4]
    laTblFld[lnFl,1] = "PLNSTK" + lcJ
    laTblFld[lnFl,2] = "N"
    laTblFld[lnFl,3] = 7
    laTblFld[lnFl,4] = 0
  ENDFOR
  
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "TotPlnStk"
  laTblFld[lnFl,2] = "N"
  laTblFld[lnFl,3] = 7
  laTblFld[lnFl,4] = 0
  
ENDIF  
*C123847,1  TMI [End  ] 

*C127807,1  TMI [Start] Add more 10 fields to hold SCALE.SCALE for PAN21
FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "SCL&lcJ"
  laTblFld[lnFl,2] = "C"
  laTblFld[lnFl,3] = 3
  laTblFld[lnFl,4] = 0
ENDFOR
*C127807,1  TMI [End  ] 

*C200520,1 ABD - Remak the next lines and create a new index. [End]
*IF ALEN(laTblFld,1)<=256
*  CREATE TABLE &gcWorkDir.&lcTempCur FROM ARRAY laTblFld
*  INDEX ON LASTLINE+STYMAJOR+COLOR TAG (lcTempCur)
*ELSE
*  RETURN .F.  
*ENDIF

CREATE TABLE &gcWorkDir.&lcTempCur FROM ARRAY laTblFld
INDEX ON ALLTRIM(cSequnc) + COLORDSC TAG 'lcTempCur1'
INDEX ON LASTLINE+STYMAJOR+COLOR TAG (lcTempCur) ADDITIVE
*C200520,1 ABD - [End]


*-- end of lfCrtTmp.

*:**************************************************************************
*:* Name        : lfBrowLine
*:* Developer   : TMI - TAREKM MOHAMED IBRAHIM
*:* Date        : 12/26/2002
*:* Purpose     : Browse Line fn
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfBrowLine()
*:***************************************************************************
*C#200431,1
FUNCTION lfBrowLine
PARAMETERS llGoTop

SELECT (lcTempCur)
IF llGoTop
  GO TOP
ENDIF  

lnMrk  = RECNO()
STORE '' TO lcPhyStk,lcFreStk
lnCntSeen = 1
laShowPen = 'RGB(192,192,192)'
lcBrowFlds = "cMrk=IIF(RECNO()=lnMrk,'>',' '):H=' ':R:1:W=.F.,"+;
             "COLORDSC:R:H=PADC('Colours',12):15:W=.F.,"
*C123847,1  TMI [Start] Add fit field for DIR03
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0
  lcBrowFlds = lcBrowFlds + "FIT :R:H='Fit' :7 :W=.F.,"
  lcPlnStk = ''
ENDIF  
*C123847,1  TMI [End  ]              
PRIVATE lnSizeLen

*C200520,1 ABD - Remark the next line and show the 10 scale or less not as the old way. [End]
*lnSizeLen = IIF(EMPTY(laSize[1]),0,ALEN(laSize,1))
*FOR lnX = 1+(lnFldGrp-1)*lnBrFlCn TO MIN(lnFldGrp*lnBrFlCn,lnSizeLen)
*  lcX = ALLT(STR(lnX))
*  *B606927,1 TMI [Start] Add a when function to the browse , it saves the current field number
*  *lcBrowFlds = lcBrowFlds + "S&lcX:H='"+laSize[lnX]+"':6:V=lfvBrFld(&lcX),"
*  lcBrowFlds = lcBrowFlds + "S&lcX:H='"+laSize[lnX]+"':6:V=lfvBrFld(&lcX):W=lfwBrFld(&lcX),"
*  *B606927,1 TMI [End  ] 
*  lcFreStk = lcFreStk + "FreStk&lcX,"
*  lcPhyStk = lcPhyStk + "PhyStk&lcX,"
*  
*  laShowPen[lnCntSeen,1] = "RGB(128,128,128,128,128,128)" && lcGray  
*  laShowPen[lnCntSeen,2] = "RGB(255,255,255,255,255,255)" && lcWhite   
*  lnCntSeen = lnCntSeen + 1
*ENDFOR
FOR lnX = 1 TO 10
  lcX = ALLT(STR(lnX))
  lcBrowFlds = lcBrowFlds + IIF(EMPTY(Size&lcX),'',"S&lcX:H='"+Size&lcX+"':6:V=lfvBrFld(&lcX):W=lfwBrFld(&lcX),")
  IF !EMPTY(Size&lcX)
    lcFreStk = lcFreStk + "FreStk&lcX,"
    lcPhyStk = lcPhyStk + "PhyStk&lcX,"
    *C123847,1  TMI [Start] 
    IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
      lcPlnStk = lcPlnStk + "PlnStk&lcX,"
    ENDIF
    *C123847,1  TMI [End  ] 
    laShowPen[lnCntSeen,1] = "RGB(128,128,128,128,128,128)" && lcGray  
    laShowPen[lnCntSeen,2] = "RGB(255,255,255,255,255,255)" && lcWhite   
    lnCntSeen = lnCntSeen + 1
  ENDIF
  
ENDFOR
*C200520,1 ABD - [End]

lcBrowFlds = lcBrowFlds + "NTOTAL:H='Total':R:W=.F."

laShowPen[lnCntSeen,1] = "RGB(128,128,128,128,128,128)" && lcGray  
laShowPen[lnCntSeen,2] = "RGB(255,255,255,255,255,255)" && lcWhite   

lcFreStk = lcFreStk + "TotFreStk"
lcPhyStk = lcPhyStk + "TotPhyStk"
*C123847,1  TMI [Start] 
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
  lcPlnStk = lcPlnStk + "TotPlnStk"
ENDIF
*C123847,1  TMI [End  ] 

*Nearly 26 Character per field, Allowed maximum lenght of the browse string is 1024 , apporximatly 36 fields
SCATTER MEMVAR TO laOldVal

BROWSE FIELDS &lcBrowFlds;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       VALID :F lfvqBrow();       
       WHEN lfwBrowUp();
       TITLE lcDet_Ttl ;
       WINDOW (lcQkWin2) IN WINDOW (lcQkWin0)

=lfwBrowUp()
=lfRefresh(lcQkWin0)
=lfRefresh(lcDet_Ttl)

*-- end of lfBrowLine.

*:**************************************************************************
*:* Name        : lfwBrFld
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/04/2003
*:* Purpose     : When function for the browse to save the current field number
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwBrFld()
*:***************************************************************************
*B606927,1 
FUNCTION lfwBrFld
PARAMETERS lnFldNo

*C200520,1 ABD - Get the needed fields from the current record not from
*C200520,1 ABD - The memo field as the old way.[Begin]
*lnSclNum = lfSclPos(lnFldNo) 
*m.Gros_Price = VAL(SUBSTR(SCLPRC,(lnSclNum-1)*23+1        ,12))
*m.Comm1      = VAL(SUBSTR(SCLPRC,(lnSclNum-1)*23+1 +12    ,5 ))
*m.Disc_Pcnt  = VAL(SUBSTR(SCLPRC,(lnSclNum-1)*23+1 +12 +5 ,6 ))
*m.Price      = ROUND( m.Gros_Price*(1-m.Disc_Pcnt/100) ,2 )

PRIVATE lcFldPos
lcFldPos     = ALLTRIM(STR(lnFldNo))
m.Gros_Price = GPric&lcFldPos
m.Comm1      = Comm&lcFldPos
m.Price      = ROUND( m.Gros_Price*(1-m.Disc_Pcnt/100) ,2 )
*C200520,1 ABD - [End]

*B132317,1  TMI [Start] Update the m.complete variable with the correct sold out date
PRIVATE lcStyl
lcStyl = STYMAJOR+lcSepart+COLOR+lcClrSpr+laExtSz[lfSclPos( LNFLDNO+10*(lnFldGrp-1) ),1]
M.COMPLETE = IIF(SEEK(lcStyl,'STYLE'), IIF(LLCMP2SOLD .AND.  .NOT. EMPTY(STYLE.SOLDOUT),STYLE.SOLDOUT,LADATA(10)) , {} )
SHOW GET M.COMPLETE
*B132317,1  TMI [End  ] 

SHOW GET m.Gros_Price
SHOW GET m.Comm1     
SHOW GET m.Price     
SHOW GET m.Disc_Pcnt 

*-- end of lfwBrFld.

*:**************************************************************************
*:* Name        : lfvBrFld											
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/24/2002
*:* Purpose     : Valid fn. for fields in temp browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvBrFld()
*:***************************************************************************
*C#200431,1
FUNCTION lfvBrFld
PARAMETERS lnIndex
PRIVATE lnFPos
lcIndex = ALLT(STR(lnIndex))

PRIVATE lnLineTot,lnCount,lnRecno,m.AddSty,lnIncrm

SELECT (lcTempCur)
IF laOldVal[lnIndex+5] #  S&lcIndex
  *-- Prevent update totals in total field
  IF !EMPTY(LASTLINE)
    REPLACE S&lcIndex WITH laOldVal[lnIndex+5]
    RETURN
  ENDIF

  IF S&lcIndex < 0
    *--Can not accept negative values
    =gfModalGen('TRM42000B40011','DIALOG')
    REPLACE S&lcIndex WITH laOldVal[lnIndex+5]
    RETURN 
  ENDIF
  IF S&lcIndex > 999
    *--Can not exceed 999
    *-- 40171 : ð cannot exceeds ð  
    =gfModalGen('TRM40171B00000','DIALOG','Size Quantity|999.')
    REPLACE S&lcIndex WITH laOldVal[lnIndex+5]
    RETURN
  ENDIF
  *C200587,1  TMI [Start] Check if style.soldout date is earlier than order entered date
  IF llWRNSLDEN 
    PRIVATE lcStyl
    lcStyl = STYMAJOR+lcSepart+COLOR+lcClrSpr+laExtSz[lfSclPos( lnIndex+10*(lnFldGrp-1) ),1]
    IF SEEK(lcStyl,'STYLE') .AND. ;
      !EMPTY(STYLE.SOLDOUT) .AND. STYLE.SOLDOUT < laData[8] .AND. ;
      gfModalGen('QRM00000B00006',.F.,.F.,.F.,'You have selected a style/colour that has passed its '+;
                                              'Sold Out Date - Do you wish to continue ?') = 2
      REPLACE S&lcIndex WITH 0
      RETURN
    ENDIF
  ENDIF
  *C200587,1  TMI [End  ] 

  m.AddSty = STYMAJOR+lcSepart+SUBSTR(COLOR,1,lnClrLen)
  SELECT (lcTempCur)
  IF laSetups[5,2]='Y'

    *-- Get scale Position 
    *C200587,1  TMI [Start] Take in considration current browse 
    *lnSclPos = lfSclPos(lnIndex)
    lnSclPos = lfSclPos(lnIndex+10*(lnFldGrp-1))
    *C200587,1  TMI [End  ] 
      
    *-- Assign a style to a location if not Assigned befor
    *C200431,4 TMI [Start] use the value "laWareHouse[lnWareHouse,2]" instead of laData[31]
    *IF !SEEK(PADR(m.AddSty+lcClrSpr+laExtSz[lnSclPos,1],19)+laData[31]+SPACE(10),'StyDye')
    IF !SEEK(PADR(m.AddSty+lcClrSpr+laExtSz[lnSclPos,1],19)+laWareHouse[lnWareHouse,2]+SPACE(10),'StyDye')
      *C200431,4 TMI [End  ] 
      *E300408,1 Message : 40012
      *E300408,1 Style/color xxx is not assigned to warehouse xxx
      *E300408,1 Button : 40002
      *E300408,1 Add Reenter
      *C200431,4 TMI [Start] use the value "laWareHouse[lnWareHouse,2]" instead of laData[31]
      *IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.AddSty+lcClrSpr+laExtSz[lnSclPos,1])+'|'+TRIM(laData[31]))=1
      *  DO gpAdStyWar WITH PADR(m.AddSty+lcClrSpr+laExtSz[lnSclPos,1],19),SPACE(10),laData[31]
      IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.AddSty+lcClrSpr+laExtSz[lnSclPos,1])+'|'+TRIM(laWareHouse[lnWareHouse,2]))=1
        DO gpAdStyWar WITH PADR(m.AddSty+lcClrSpr+laExtSz[lnSclPos,1],19),SPACE(10),laWareHouse[lnWareHouse,2]
        *C200431,4 TMI [End  ] 
      ELSE      
        REPLACE S&lcIndex WITH 0
        KEYBOARD '{LEFTARROW}'
        SCATT MEMVAR TO laOldVal      
        RETURN
      ENDIF
    ENDIF

  ENDIF
  
  *--Sum total of the current line
  lnLineTot = 0
  *--Line Total
  *C200520,1 ABD - remark the next loop and became 10 times only. [Begin]
  *FOR lnCount = 1 TO ALEN(laSize)
  *B128755,1  TMI [Start] get the line total for all sizes
  PRIVATE lcKey,lcFilt,lnTotal,lcKeyFld
  lcKey = EVAL(STRTRAN(KEY(),'+CSEQUNC',''))
  lcKeyFld = KEY()
  lcFilt = FILTER()
  SET FILT TO 
  =SEEK(lcKey)
  *SCAN REST WHILE LASTLINE+STYMAJOR+COLOR  = lcKey
  SCAN REST WHILE &lcKeyFld. = lcKey  
    *B128755,1  TMI [End  ] 
    FOR lnCount = 1 TO 10
      *C200520,1 ABD - [End]
      lcCount = ALLTRIM(STR(lnCount))
      lnLineTot = lnLineTot + S&lcCount
    ENDFOR
    *B128755,1  TMI [Start] update the line total for all sizes
    *REPLACE NTOTAL WITH lnLineTot
  ENDSCAN
  =SEEK(lcKey)
  *REPLACE NTOTAL WITH lnLineTot ;
    REST WHILE LASTLINE+STYMAJOR+COLOR  = lcKey
  REPLACE NTOTAL WITH lnLineTot ;
    REST WHILE &lcKeyFld.  = lcKey
  SET FILTER TO &lcFilt 
  lnTotal = 0 
  SUM NTOTAL TO lnTotal FOR LASTLINE = ' '
  =SEEK(lcKey)
  *B128755,1  TMI [End  ]   

  *B128755,1  TMI [Start] populate old value array
  SCATT MEMVAR TO laOldVal
  *B128755,1  TMI [End  ]   
  *-- current Column and total columns sum 
  *C200520,1 ABD - Add [Begin]
  *SELECT SUM(S&lcIndex),SUM(NTOTAL) FROM (lcTempCur) WHERE EMPTY(LASTLINE) INTO ARRAY laTot
  SELECT SUM(S&lcIndex),SUM(NTOTAL) FROM (lcTempCur) WHERE EMPTY(LASTLINE) .AND. Val(cSequnc) = lnfldgrp INTO ARRAY laTot 
  *C200520,1 ABD - [End]
  
  lnRecno = RECNO()
  GO BOTTOM
  *-- Update last line for these two columns
  REPLACE S&lcIndex WITH laTot[1,1],;
          NTOTAL    WITH laTot[1,2]
  *B128755,1  TMI [Start] Update the NTOTAL with the correct sum
  lcFilt = FILTER()
  SET FILT TO 
  =SEEK(CHR(255))
  REPLACE NTOTAL WITH lnTotal ;
    REST WHILE LASTLINE+STYMAJOR+COLOR  = CHR(255)
  SET FILTER TO &lcFilt 
  *B128755,1  TMI [End  ]   
  llChang = .T.
  IF BETWEEN(lnRecno,1,RECCOUNT()-1)
    GOTO (lnRecno)
  ELSE
    GO TOP
  ENDIF  

  lcSavStat = IIF(llEdit OR laTot[1,2]>0,'ENABLE','DISABLE')
  SHOW GET pbSav &lcSavStat
  *B128755,1  TMI [Start] comment this line and move it above
  *SCATT MEMVAR TO laOldVal
  *B128755,1  TMI [End  ] 

ENDIF  
*-- end of lfvBrFld.

*:**************************************************************************
*:* Name        : lfPrNxBrow									
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/30/2002
*:* Purpose     : Prev. or Next Browse 
*:***************************************************************************
*:* Called from : Screen - Prev,next button
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPrNxBrow()
*:***************************************************************************
*C#200431,1
FUNCTION lfPrNxBrow
PARAMETERS lnPrNx

PRIVATE lnFldGrpCn  && Field groups count
lnFldGrpCn = CEILING(ALEN(laSize)/lnBrFlCn)

IF !EMPTY(lnPrNx)
  IF lnPrNx = -1
    IF lnFldGrp = 1
      RETURN
    ENDIF
    lnFldGrp = lnFldGrp - 1  
  ENDIF  
  IF lnPrNx = 1
    IF lnFldGrp = lnFldGrpCn
      RETURN
    ENDIF
    lnFldGrp = lnFldGrp + 1  
  ENDIF
ENDIF

lcPrevStat = IIF(lnFldGrp = 1 , 'DISABLE' , 'ENABLE')
lcNextStat = IIF(lnFldGrp = lnFldGrpCn , 'DISABLE' , 'ENABLE' )

SHOW GET pbPrev &lcPrevStat
SHOW GET pbNext &lcNextStat

PRIVATE lnRecno
lnRecno = RECNO(lcTempCur)
*-- Rebrowse and refresh
=lfBrowLine(.T.)
IF BETWEEN(lnRecno,1,RECCOUNT(lcTempCur))
  GOTO lnRecno IN &lcTempCur
ENDIF

*C200520,1 ABD - Go top at the temp file to refresh the filter,
*C200520,1 ABD - that will show the correct data at the wuick entry screen. [Begin]
lcSeekExp = EVAL(Key())
IF !SEEK(lcSeekExp)
  LOCATE
ENDIF
*C200520,1 ABD - [End]

=lfwBrowUp()
ACTIVATE WINDOW (lcDet_Ttl)
*-- end of lfPrNxBrow.

*:**************************************************************************
*:* Name        : lfSclPos
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/26/2002
*:* Purpose     : Loop to get the scale number dependign on the size position in the list of all sizes
*:***************************************************************************
*:* Called from : lfv 
*:***************************************************************************
*:* Parameters : lnIndex
*:***************************************************************************
*:* Return      : Scale Position
*:***************************************************************************
*:* Example     :  = lfSclPos()
*:***************************************************************************
*C#200431,1
FUNCTION lfSclPos
PARAMETERS lnIndex
PRIVATE lnIndex,lnIncrm,lnCount
lnCount = 1  
*-- Loop to get the scale number dependign on the size position in the list of all sizes
lnIncrm = 0
FOR lnCount = 1 TO ALEN(laExtSz,1)    
  IF lnIncrm < lnIndex AND lnIndex <= lnIncrm+laExtSz[lnCount,2]
    RETURN lnCount
  ENDIF
  lnIncrm = lnIncrm + laExtSz[lnCount,2]
ENDFOR
*-- end of lfSclPos.

*:**************************************************************************
*:* Name        : lfvqBrow                                      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/26/2002
*:* Purpose     : Valid function for browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqBrow()
*:***************************************************************************
*C#200431,1
FUNCTION lfvqBrow
IF WONTOP() # lcDet_Ttl
  = gfStopBrow()
ENDIF

*:**************************************************************************
*:* Name        : lfwBrowUp                                      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/26/2002
*:* Purpose     : When function for browse
*:***************************************************************************
*:* Called from : lfBrowLine
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwBrowUp()
*:***************************************************************************
*C#200431,1
FUNCTION lfwBrowUp
PRIVATE lnI
lnMrk = RECNO(lcTempCur)
*--Initiate the array used instead of lcOldVal when variable
SCATTER MEMVAR TO laOldVal
SHOW WINDOW (lcDet_Ttl) REFRESH

*--Get current browse total fields values
STORE 0 TO laFreStk,laPhyStk
SCATTER FIELDS &lcFreStk TO laFreStk
SCATTER FIELDS &lcPhyStk TO laPhyStk
FOR lnI = 1 TO lnBrFlCn+1
  SHOW GET laFreStk[lnI]
  SHOW GET laPhyStk[lnI]
ENDFOR
*C123847,1  TMI [Start] 
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
  STORE 0 TO laPlnStk
  SCATTER FIELDS &lcPlnStk TO laPlnStk
  FOR lnI = 1 TO lnBrFlCn+1
    SHOW GET laPlnStk[lnI]
  ENDFOR
ENDIF
*C123847,1  TMI [End  ] 
=lfRefresh(lcQkWin4)

*-- end of lfwBrowUp.

*:**************************************************************************
*:* Name        : lpTab                                      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/26/2002
*:* Purpose     : Tab fn.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpTab()
*:***************************************************************************
*C#200431,1
PROCEDURE lpTab

IF WONTOP() = lcDet_Ttl
  ACTIVATE WINDOW (lcQkWin3)
  _CUROBJ=OBJNUM(pbSav)  
ELSE
  _CUROBJ=_CUROBJ+1
ENDIF

*:**************************************************************************
*:* Name        : lpBacktab                                       
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/26/2002
*:* Purpose     : Backtab Trap fn.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpBacktab ()
*:***************************************************************************
*C#200431,1
FUNCTION lpBacktab 
IF RECCOUNT(lcTempCur) = 0
  DO CASE
    CASE WONTOP() = lcDet_Ttl
      ACTIVATE WINDOW (lcQkWin1)
    CASE WONTOP() = lcQkWin1
      ACTIVATE WINDOW (lcQkWin3)             
    CASE WONTOP() = lcQkWin3 
      ACTIVATE WINDOW (lcDet_Ttl)
  ENDCASE
ELSE
  IF WONTOP() = lcDet_Ttl
    ACTIVATE WINDOW (lcQkWin1)
    _CUROBJ=OBJNUM(m.Comm1)
  ELSE
    DO CASE
      CASE WONTOP() = lcQkWin1
        IF _CUROBJ = OBJNUM(m.TotQty)
          ACTIVATE WINDOW (lcQkWin3)
          _CUROBJ = OBJNUM(pbExit)
        ELSE
          IF llDifPrice
            IF _CUROBJ = OBJNUM(m.Comm1)
              _CUROBJ = OBJNUM(m.TotQty)
            ENDIF
            IF _CUROBJ = OBJNUM(m.TotQty)
              ACTIVATE WINDOW (lcQkWin3)
              _CUROBJ = OBJNUM(pbExit)             
            ENDIF
          ELSE
            _CUROBJ=_CUROBJ - 1
          ENDIF
        ENDIF
        
      CASE WONTOP() = lcQkWin3
        IF laTot[1,2]=0
          IF _CUROBJ = OBJNUM(pbClear)
            ACTIVATE WINDOW (lcDet_Ttl)
          ELSE
            _CUROBJ = OBJNUM(pbSav)
          ENDIF
        ELSE
          IF _CUROBJ = OBJNUM(pbSav)
            ACTIVATE WINDOW (lcDet_Ttl)
          ELSE
            _CUROBJ=_CUROBJ - 1
          ENDIF
        ENDIF
        
    ENDCASE
  ENDIF
ENDIF  
*-- end of lpBacktab .

*:**************************************************************************
*:* Name        : lfvqClear                                      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/26/2002
*:* Purpose     : Clear valid fucntio for screen quickord
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqClear()
*:***************************************************************************
*C#200431,1
FUNCTION lfvqClear
PARAMETERS llFrmKeys

IF llFrmKeys AND EMPTY(m.Style)
  RETURN
ENDIF

DIME laSize[1]
STORE ' ' TO m.Style,m.Desc1,laSize
STORE 0  TO m.Price,m.TotQty,m.Disc_Pcnt,m.Gros_Price,m.Comm1,laTot
STORE  .F. TO llEdit,llChang,llDifPrice
STORE .T. TO llShw1
STORE 1 TO lnFldGrp

m.COMPLETE =  laData[10] 

SHOW GET m.Style ENABLE 
SHOW GET m.COMPLETE DISABLE
SHOW GET m.Desc1 DISABLE
SHOW GET m.Price DISABLE
SHOW GET m.TotQty DISABLE
SHOW GET m.Disc_Pcnt DISABLE
SHOW GET m.Gros_Price DISABLE
SHOW GET m.Comm1 DISABLE
SHOW GET pbNote DISABLE
SHOW GET pbClear DISABLE
SHOW GET pbSav   DISABLE
_CUROBJ = OBJNUM(m.Style)

*--Clear notes
SELECT (lcTempNote)
REPLACE NOTE_MEM WITH ''

*-- Update the free & Physical stock
STORE 0 TO laFreStk  ,laPhyStk
FOR lnI = 1 TO lnBrFlCn+1
  SHOW GET laFreStk[lnI]
  SHOW GET laPhyStk[lnI]
ENDFOR
*C123847,1  TMI [Start] 
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
  STORE 0 TO laPlnStk
  FOR lnI = 1 TO lnBrFlCn+1
    SHOW GET laPlnStk[lnI]
  ENDFOR
ENDIF  
*C123847,1  TMI [End  ] 
=lfRefresh(lcQkWin4)

SELECT (lcTempCur)
*C200520,1 ABD - remove the filter from the current file. [Begin]
SET FILTER TO
*C200520,1 ABD - [End]

ZAP  

=lfPrNxBrow()

*--If fields more than lnBrFlCn(=10) , enable next button - for next browse
*IF CEILING(ALEN(laSize)/lnBrFlCn) > 1 
*  SHOW GET pbNext  ENABLE 
*ENDIF
SHOW GET pbNext DISABLE
SHOW GET pbPrev DISABLE
IF llFrmKeys
  ACTIVATE WINDOW (lcQkWin1)
  _CUROBJ = OBJNUM(M.STYLE)
ENDIF

*-- end of lfvqClear.


*:**************************************************************************
*:* Name        : lfvQStyle                                      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/18/2002
*:* Purpose     : Valid funciton for m.QStyle field
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvQStyle()
*:***************************************************************************
*C#200431,1
FUNCTION lfvQStyle

PRIVATE lcAlias,lcGetSty,lcStat,lnIncrmnt,lnClrs,lnCount,lnJ,lcSeekSty,laSum,laSavStat,lcStyle
*--- Select
lcAlias = ALIAS()


IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF

STORE 0 TO laPhyStk,laFreStk
*C123847,1  TMI [Start] 
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0 
  STORE 0 TO laPhlnStk
ENDIF  
*C123847,1  TMI [End  ] 
m.Style = PADR(ALLT(m.Style),lnMajorLen)

IF !EMPTY(m.Style) AND ('?' $ m.Style .OR. !SEEK(m.Style,'STYLE'))
  SELECT STYLE  
  lcGetSty = PADR(gfStyBrw("M" , m.Style , "" , .F.),lnMajorLen)
  llBrowse = .F.
  IF EMPTY(lcGetSty)
    m.Style = lcOldvalue
    _CUROBJ = OBJNUM(m.Style) 
    RETURN
  ELSE
    m.Style = lcGetSty
  ENDIF
ENDIF  

IF SEEK(m.Style,'Style')
  *E300408,1 Message : 32017
  *E300408,1 Style scale not found in the scale file. Cannot accept.
  *E300408,1 Button : 00000 
  *E300408,1 Ok

  *E300408,1 Message : 32018
  *E300408,1 This is a canceled style. Cannot accept.
  *E300408,1 Button : 00000 
  *E300408,1 Ok

  *E300408,1 Message : 32019
  *E300408,1 This style/color is on hold. 
  *E300408,1 Button : 32003
  *E300408,1 Accept Reenter

  *E300408,1 Message : 32020
  *E300408,1 Styles restricted to XXX!
  *E300408,1 Button : 00000
  *E300408,1 Ok

  *E300408,1 Message : 32021
  *E300408,1 Style XXX date is 
  *E300408,1 Button : 32003
  *E300408,1 Accept Reenter

  *C200551,3  TMI [Start] Current status if the 1st color is cancelled the code refuse all the style colors
  *C200551,3              We need to Refuse only cancelled colours.
  *IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
  *     gfModalGen('TRM32017B00000','ALERT')=1) ;
  * .OR. (Style.Status='X' .AND. ;
  *     gfModalGen('TRM32018B00000','ALERT')=1) ;
  * .OR. (Style.Status='H' .AND. ;
  *     gfModalGen('QRM32019B32003','ALERT')=2) ;
  * .OR. (Style.cDivision <> laData[15] .AND. ;
  *     gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
  * .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[9] .AND. ;
  *     gfModalGen('QRM32021B32003','ALERT','start|'+DTOC(Style.Start))=2) ;
  * .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
  *     gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)  

  *C200587,4  TMI [Start] No need to check the style.soldout date compated with start date here
  *IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
        gfModalGen('TRM32017B00000','ALERT')=1) ;
   .OR. (Style.Status='H' .AND. ;
        gfModalGen('QRM32019B32003','ALERT')=2) ;
   .OR. (Style.cDivision <> laData[15] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
   .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[9] .AND. ;
        gfModalGen('QRM32021B32003','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)  
  *B122433,1  TMI [Start] Add season check based on setup option ( Customized for MBI )
  *IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
  *      gfModalGen('TRM32017B00000','ALERT')=1) ;
  * .OR. (Style.Status='H' .AND. ;
  *      gfModalGen('QRM32019B32003','ALERT')=2) ;
  * .OR. (Style.cDivision <> laData[15] .AND. ;
  *      gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1)
  IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
        gfModalGen('TRM32017B00000','ALERT')=1) ;
   .OR. (Style.Status='H' .AND. ;
        gfModalGen('QRM32019B32003','ALERT')=2) ;
   .OR. (Style.cDivision <> laData[15] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
   .OR. IIF(ASCAN(laEvntTrig , PADR('STYLEVALID',10)) <> 0 AND TYPE('laSetups[15,2]')='L' AND laSetups[15,2] = .F.,.F.,(ALLTRIM(laData[14])<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season<>laData[14] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1))
  *B122433,1  TMI [End  ]         
  *C200587,4  TMI [End  ] 
  *C200551,3  TMI [End  ]          

    STORE lcOldvalue TO m.Style
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
  *C037727,1  TMI [Start] Check if the style customer saved in &lcTmpSty..CCHKACCNT match selected Account in sales order
  IF lfChkAccSto() = .F.
    m.Style = SPACE(lnMajorLen)
    SHOW GET M.STYLE
    RETURN .F.
  ENDIF  
  *C037727,1  TMI [End  ] 
  
  *C200587,4  TMI [Start] if option "Default comp. date to Soldout date" is Yes 
  *C200587,4              update m.complete date with soldout date of first style
  IF llCMP2SOLD
    M.COMPLETE = STYLE.SOLDOUT
  ENDIF
  *C200587,4  TMI [End  ]
  
  *-* =>| *C200431,4 TMI [Start] Add a line for the current style to prevent the screen "STYPRIC" to appear  
  *-* =>| IF llMulCurr .AND. laData[33]<>gcBaseCurr 
  *-* =>|   IF !USED('STYPRICE')
  *-* =>|     =gfOpenFile(gcDataDir+'STYPRICE','STYPRICE','SH')
  *-* =>|   ENDIF
  *-* =>|   IF !SEEK(STYLE.STYLE+laData[33],'STYPRICE')
  *-* =>|     PRIVATE laPr
  *-* =>|     DIMENSION laPr[1]
  *-* =>|     laPr = 1
  *-* =>|     *--If style has a line in "CSTPRICE" file get this price
  *-* =>|     lcDvKey = STYLE.STYLE+laData[33]
  *-* =>|     SELECT PRICEDV FROM CSTPRICE WHERE STYDV+CCURRCOD=lcDvKey INTO ARRAY laPr
  *-* =>|     SELECT STYPRICE
  *-* =>|     APPEND BLANK
  *-* =>|     REPLACE STYLE     WITH STYLE.STYLE,;
  *-* =>|             CCURRCODE WITH laData[33],;
  *-* =>|             PRICEA    WITH laPr[1],;
  *-* =>|             PRICEB    WITH laPr[1],;
  *-* =>|             PRICEC    WITH laPr[1]
  *-* =>|   ENDIF
  *-* =>| ENDIF  
  *-* =>| *C200431,4 TMI [End  ] 
  
  *B606927,1 TMI [Start] Comment all code of dealing with prices,it will be dealed other way
  */*  *-- if Customer has no price code , or current selected style has no entry in the file cstprice then use the default case
  */*  llCodPrice = .F.  && If this style is linked to a price code for current customer
  */*  IF EMPTY(CUSTOMER.PRICCODE) .OR. !SEEK(CUSTOMER.PRICCODE+laData[33]+SUBSTR(m.Style,1,lnMajorLen),'CSTPRICE')
  */*    *--Get style price according to ordered quantity - (Default case)
  */*    PUSH KEY
  */*    ON KEY
  */*    m.lContract = lcOrdType<>'C' .AND. lfvContPri(m.Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
  */*    IF !m.lContract
  */*      lcStyle = STYLE.STYLE
  */*      m.Gros_Price = MAX( lfGetprice(lcStyle,lcPriceLvl,m.TotQty) , 0 )
  */*      ** To prevent the screen SOSTYPRI to appear user must enter price <> 0  for all sizes
  */*    ENDIF
  */*    POP KEY
  */*  
  */*    m.Price    = m.Gros_Price
  */*    m.Desc1    = Style.Desc1
  */*    m.Comm1    = IIF(Style.Commission,laData[28],0)   &&The first commession 
  */*    *C200431 TMI [Start] 
  */*    lcCommStat = 'ENABLE'
  */*    *C200431 TMI [End  ] 
  */*    
  */*    *-- Get Style discount percent and Calculate net price  
  */*    IF !m.lContract
  */*      *-- get the cDiscCode From stydye in every case
  */*      *C200431,4 TMI [Start] use the value "laWareHouse[lnWareHouse,2]" instead of laData[31]
  */*      *lcDiscCode  = IIF(SEEK(m.Style+laData[31]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
  */*      *m.Disc_Pcnt = 0     && We get this value from screen
  */*      lcDiscCode  = IIF(SEEK(m.Style+laWareHouse[lnWareHouse,2]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
  */*      *C200431,4 TMI [End  ] 
  */*      IF !EMPTY(ALLTRIM(lcDiscCode))
  */*        *-- Get the disecound related filed to now which 
  */*        *-- type whole Sale Or Retail sale Or Both.
  */*        DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
  */*        STORE '' To lcDisType , ldstartDte ,ldEndDate
  */*        *-- Array to get the Discount affect for DecCode.
  */*        laDisType[1,1]  = 'CCOSTAFECT'
  */*        laDisType[1,2]  = 'lcDisType'
  */*        *-- Array to get the start date For DescCode.
  */*        lastartDte[1,1] = 'START'
  */*        lastartDte[1,2] = 'ldstartDte'
  */*        *-- Array to get the end date For DescCode.
  */*        laEndDate[1,1]  = 'DENDATE'
  */*        laEndDate[1,2]  = 'ldEndDate'
  */*        = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
  */*        = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
  */*        = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
  */*        lnDisc_Pcnt = 0
  */*        IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[8],ldstartDte,ldEndDate)
  */*          lnDisc_Pcnt = m.Disc_Pcnt
  */*          =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
  */*          m.Disc_Pcnt = lnDisc_Pcnt
  */*        ENDIF
  */*      ENDIF  
  */*      m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100  
  */*    ENDIF 
  */*  
  */*    *-- Show if there are more than one price for each scale
  */*    PRIVATE laPrices,lcStyClr
  */*    SELECT STYLE  
  */*    *C200431,4 TMI [Start] Compare prices for all colors-scales depending on Stylemajor
  */*    *lcStyClr = SUBSTR(STYLE,1,lnMajorLen+1+lnClrLen)
  */*    lcStyClr = SUBSTR(STYLE,1,lnMajorLen)
  */*    *C200431,4 TMI [End  ] 
  */*  
  */*    IF !llMulCurr .OR. laData[33]=gcBaseCurr
  */*      *--Single currency case
  */*      SCATTER FIELDS PRICEA,PRICEB,PRICEC TO laPrices
  */*      SCAN REST WHILE STYLE = lcStyClr
  */*        llDifPrice = PRICEA <> laPrices[1] .OR. ;
  */*                     PRICEB <> laPrices[2] .OR. ;
  */*                     PRICEC <> laPrices[3]
  */*        IF llDifPrice
  */*          EXIT
  */*        ENDIF             
  */*        SCATTER FIELDS PRICEA,PRICEB,PRICEC TO laPrices             
  */*      ENDSCAN
  */*    ELSE
  */*      *--Multipe currency case
  */*      SELECT STYPRICE
  */*      LOCATE FOR STYLE=lcStyClr .AND. CCURRCODE = laData[33]
  */*      SCATTER FIELDS PRICEA,PRICEB,PRICEC TO laPrices
  */*      SCAN REST WHILE STYLE = lcStyClr FOR CCURRCODE = laData[33]
  */*        llDifPrice = PRICEA <> laPrices[1] .OR. ;
  */*                     PRICEB <> laPrices[2] .OR. ;
  */*                     PRICEC <> laPrices[3]
  */*        IF llDifPrice
  */*          EXIT
  */*        ENDIF             
  */*        SCATTER FIELDS PRICEA,PRICEB,PRICEC TO laPrices             
  */*      ENDSCAN
  */*    ENDIF
  */*    *-- Show if there are more than one price for each scale 
  */*  
  */*  ELSE  
  */*  
  */*    llCodPrice = .T.  
  */*    *C200431,4 TMI [Start] Get the first price in file CSTPRICE , assign it to m.Gros_Price
  */*    STORE CSTPRICE.PRICEDV TO m.Gros_Price,m.Price
  */*    
  */*    *--Commesion State field
  */*    PRIVA laComm
  */*    DIMENSION laComm[1]
  */*    laComm = 0
  */*    lcPrCod = CUSTOMER.PRICCODE    
  */*    SELECT DIST COMMDV ;
  */*    FROM CSTPRICE ;
  */*    WHERE CSTPRICE.PRICCODE+CSTPRICE.CCURRCOD+CSTPRICE.STYDV = lcPrCod+laData[33]+SUBSTR(m.Style,1,lnMajorLen) ;
  */*    AND COMMDV<>0 ;
  */*    INTO ARRAY laComm
  */*    M.COMM1 = IIF(laComm[1]>0,0,laData[28])
  */*    lcCommStat = IIF(laComm[1]>0,'DISABLE','ENABLE')
  */*    *C200431,4 TMI [End  ]     
  */*  ENDIF  
  *B606927,1 TMI [End  ] Comment all code of dealing with prices,it will be dealed other way  
  
  lcStat = IIF(!EMPTY(m.Style),'ENABLE','DISABLE')
  SHOW GET pbClear &lcStat  
  
  *--Get Sizes  
  laSize = ''
  =SEEK(m.Style,'STYLE')
  lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
  m.SCALE = STYLE.SCALE
  *khm
  m.Desc1 = Style.Desc1

  SELECT Scale,CNT FROM SCALE WHERE Type+Scale='S'+SUBSTR(m.SCALE,1,lnScaleLen) ORDER BY 1 INTO ARRAY laExtSz
  SELECT SCALE
  =SEEK('S'+laExtSz[1],'SCALE')

  *-- If style is entred befor , warn the user
  IF SEEK(lcOrdType+laData[1]+laData[3]+m.Style,(lcOrdLine))
    *--This Style has entred on this order
    *--Button 02011 : \!\<OK;\?\<Cancel
    lnResp = gfModalGen('QRM00000B02011',.F.,.F.,.F.,'This style has already been entred on this order '+;
                                                    +'- do you wish to create new order lines ?')
    IF lnResp = 2 
      =lfvqClear() 
      RETURN
    ENDIF
  ENDIF
  
  lnIncrmnt = 0
  FOR lnCount = 1 TO ALEN(laExtSz,1)
  *FOR lnCount = 1 TO MIN( ALEN(laExtSz,1) , 10 )
    =SEEK('S'+laExtSz[lnCount,1],'SCALE')
    FOR lnJ = 1 TO laExtSz[lnCount,2]
      lcZ = STR(lnJ,1)
      DIMENSION laSize[lnIncrmnt+lnJ]
      laSize[lnIncrmnt+lnJ] = ALLTRIM(SCALE.SZ&lcZ)
    ENDFOR
    lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
  ENDFOR
  PRIVATE lnFldCnt,lnSzCnt
  lnFldCnt = ALEN(laSize)
  
  *--Clear Notes (if any)
  SELECT (lcTempNote)
  REPLACE NOTE_MEM WITH ''
  
  *--Create the temp file for colors , all sizes in one line
  *C200520,1 ABD - Remark the next lines and replace it with a new function to handle 
  *C200520,1 ABD - All this code by the new way. [Begin]
  *IF !lfCrtTmp(lnFldCnt)
  *  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not create the temp file!')
  *  =lfvqClear() 
  *  RETURN
  *ENDIF
  *--Create color array
  *SELECT DISTINCT SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) FROM STYLE ;
  *  WHERE STYLE = PADR(m.Style,lnMajorLen) ;
  *  INTO ARRAY laClr  
  *--Add a line for each color
  *PRIVATE lnMaxInd
  *FOR lnClrs = 1 TO ALEN(laClr,1)
  *  *B606927,1 TMI [Start] populate the variable M.SCLPRC
  *  M.SCLPRC = SPACE(23*ALEN(laExtSz,1))
  *  *B606927,1 TMI [End  ] 
  
  *  SELECT (lcTempCur)
  *  SCATTER MEMVAR BLANK
  *  M.STYMAJOR = PADR(m.Style,lnMajorLen)
  *  M.COLOR = laClr[lnClrs]
  *  M.COLORDSC = PADR(gfCodDes(PADR(laClr[lnClrs],lnClrLen) , 'COLOR'),20)
  *  lnIncrmnt = 0
  *  *--Update Quantites from lcOrdLine if style is entred befor for the same store
  *  FOR lnCount = 1 TO ALEN(laExtSz,1)
  *  *FOR lnCount = 1 TO MIN( ALEN(laExtSz,1) , 10 )
  *    lcSeekSty = M.STYMAJOR+lcSepart+PADR(M.COLOR,lnClrLen)+lcClrSpr+laExtSz[lnCount,1]
  *    lcSeekSty = PADR(lcSeekSty,19)

  *    *-- Fill the fields PhyStk and FreStk
  *    *C200431,4 TMI [Start] use the value "laWareHouse[lnWareHouse,2]" instead of laData[31]
  *    *=SEEK(lcSeekSty+laData[31],'STYDYE')
  *    =SEEK(lcSeekSty+laWareHouse[lnWareHouse,2],'STYDYE')
  *    *C200431,4 TMI [End  ] 
      
  *    PRIVATE laSoldQty
  *    DIMENSION laSoldQty[8]
  *    laSoldQty = 0
  *    ldNxM1stDy = lfNxM1stDy()            && Get Next month first day
  *    *C200431,4 TMI [Start] use the value "laWareHouse[lnWareHouse,2]" instead of laData[31]
  *    *SELECT SUM(QTY1),SUM(QTY2),SUM(QTY3),SUM(QTY4),SUM(QTY5),SUM(QTY6),SUM(QTY7),SUM(QTY8) ;
  *    *FROM ORDLINE,ORDHDR ;
  *    *WHERE STYLE = lcSeekSty ;
  *    *  AND ORDHDR.ORDER = ORDLINE.ORDER ;            
  *    *  AND ORDHDR.STATUS $ 'OH' ;
  *    *  AND ORDLINE.START < ldNxM1stDy ;          
  *    *  AND ORDLINE.CWARECODE = laData[31] ;
  *    *INTO ARRAY laSoldQty
  *    *B606927,1 TMI [Start] Use Seek instead of select sql statement
  *    *SELECT SUM(QTY1),SUM(QTY2),SUM(QTY3),SUM(QTY4),SUM(QTY5),SUM(QTY6),SUM(QTY7),SUM(QTY8) ;
  *    FROM ORDLINE,ORDHDR ;
  *    WHERE STYLE = lcSeekSty ;
  *      AND ORDHDR.ORDER = ORDLINE.ORDER ;            
  *      AND ORDHDR.STATUS $ 'OH' ;
  *      AND ORDLINE.START < ldNxM1stDy ;          
  *      AND ORDLINE.CWARECODE = laWareHouse[lnWareHouse,2] ;
  *    INTO ARRAY laSoldQty
  *    
  *    SELECT ORDLINE
  *    SEEK lcSeekSty
  *    SCAN REST WHILE STYLE = lcSeekSty ;
  *                FOR ORDLINE.CWARECODE = laWareHouse[lnWareHouse,2] AND ORDLINE.START<ldNxM1stDy
  *      IF SEEK(ORDLINE.CORDTYPE+ORDLINE.ORDER,'ORDHDR') AND ORDHDR.STATUS $ 'OH'
  *        laSoldQty[1] = laSoldQty[1] + ORDLINE.QTY1
  *        laSoldQty[2] = laSoldQty[2] + ORDLINE.QTY2
  *        laSoldQty[3] = laSoldQty[3] + ORDLINE.QTY3
  *        laSoldQty[4] = laSoldQty[4] + ORDLINE.QTY4
  *        laSoldQty[5] = laSoldQty[5] + ORDLINE.QTY5
  *        laSoldQty[6] = laSoldQty[6] + ORDLINE.QTY6
  *        laSoldQty[7] = laSoldQty[7] + ORDLINE.QTY7
  *        laSoldQty[8] = laSoldQty[8] + ORDLINE.QTY8
  *      ENDIF
  *    ENDSCAN
  *    *B606927,1 TMI [End  ]       
  *    *C200431,4 TMI [End  ] 
  *    *B606927,1 TMI [Start] Get the price from CSTPRICE.dbf or from style.dbf file
  *    =SEEK(lcSeekSty,'STYLE')
  *    IF SEEK(lfPRICCODE()+laData[33]+lcSeekSty,'CSTPRICE')        
  *    
  *      lnStyComm = IIF(CSTPRICE.COMMDV=0,IIF(Style.Commission,laData[28],0),CSTPRICE.COMMDV)
  *      
  *             * GROS PRICE                COMMESION                dicount
  *             *123456789,12              +12,45                   +123,56 
  *      lcPri = STR(CSTPRICE.PRICEDV,12,2)+STR(lnStyComm,5,2)+STR(0,6,2)        
  *    ELSE      
  *      lcPri = STR(0,12,2) + STR(IIF(Style.Commission,laData[28],0),5,2) + STR(0,6,2)
  *      IF llMulCurr .AND. laData[33]<>gcBaseCurr 
  *        IF SEEK(lcSeekSty+laData[33],'STYPRICE')
  *          lcPri = STR(STYPRICE.PRICEA,12,2) + STR(IIF(Style.Commission,laData[28],0),5,2) + STR(0,6,2)
  *        ENDIF
  *      ELSE     
  *        IF SEEK(lcSeekSty,'STYLE')
  *          lcPri = STR(STYLE.PRICEA,12,2) + STR(IIF(Style.Commission,laData[28],0),5,2) + STR(0,6,2)
  *        ENDIF
  *      ENDIF
  *    ENDIF
  *    M.SCLPRC = STUFF(M.SCLPRC,(lnCount-1)*23+1,23,lcPri)
  *    *B606927,1 TMI [End  ] 
  *    
  *    FOR lnJ = 1 TO laExtSz[lnCount,2]      
  *      lcZ = STR(lnJ,1)
  *      lcX = ALLT(STR(lnIncrmnt+lnJ))
  *      
  *      *--Stock (free - physical)
  *      *M.FreStk&lcX = STYDYE.Stk&lcZ - STYDYE.Ord&lcZ
  *      M.FreStk&lcX = STYDYE.Stk&lcZ - laSoldQty[lnJ]
  *      M.PhyStk&lcX = STYDYE.Stk&lcZ        

  *      **--Total qty , Free stk , Phys stk
  *       *m.nTotal = m.nTotal + &lcOrdLine..Qty&lcZ
  *      m.TotFreStk = m.TotFreStk + M.FreStk&lcX
  *      m.TotPhyStk = m.TotPhyStk + M.PhyStk&lcX

  *    ENDFOR

  *    lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
  *  ENDFOR     
  *  INSERT INTO (lcTempCur) FROM MEMVAR
  *ENDFOR
  *C123847,1  TMI [Start] Add a trigger to collect sizes for DIR03 taking in considration the field SCALE.CDIM1 
  IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0
    IF !lfCOLLFIT()
      =lfvqClear()
      RETURN 
    ENDIF
  ELSE
    *C123847,1  TMI [End  ] 
    = lfCollect ()
    *C123847,1  TMI [Start] Add a trigger to collect sizes for DIR03 taking in considration the field SCALE.CDIM1 
  ENDIF
  *C123847,1  TMI [End  ] 
  *C200520,1 ABD - [End]
  =SEEK(PADR(m.Style,lnMajorLen),'STYLE')
  
  SELECT (lcTempCur)
  GO TOP
  lcTempTot = gfTempName()
  TOTAL ON .T. TO (gcWorkDir+lcTempTot)
  SELECT 0
  USE (gcWorkDir+lcTempTot) 
  SCATTER MEMVAR
  USE IN &lcTempTot
  ERASE (gcWorkDir+lcTempTot+'.DBF') 
  
  *C200520,1 ABD - Insert the totals record while I collect the data for the current
  *C200520,1 ABD - style color. [Begin]
  *--Add total line
  *M.LASTLINE = CHR(255)
  *M.STYMAJOR = ''
  *M.COLOR    = ''
  *M.COLORDSC = 'Totals'   
  *INSERT INTO (lcTempCur) FROM MEMVAR
  *C200520,1 ABD - [End]
  
  SELECT (lcTempCur)
  GO TOP
  STORE M.NTOTAL TO laTot[1,2],m.TotQty
  llEdit = M.NTOTAL>0
  
  laSavStat = IIF(llEdit,'ENABLE','DISABLE')
  SHOW GET pbSav &laSavStat

  SHOW GET m.Style DISABLE
  SHOW GET m.TotQty ENABLE
  SHOW GET m.Comm1 ENABLE
  SHOW GET pbNote ENABLE
  SHOW GET m.Desc1 DISABLE

  lcStat = IIF(llCodPrice .OR. llDifPrice ,'DISABLE','ENABLE')
  SHOW GET m.Gros_Price &lcStat
  *C200431,4 TMI [Start] The discount need to be always enabled as per Usama
  *SHOW GET m.Disc_Pcnt &lcStat
  SHOW GET m.Disc_Pcnt ENABLED
  *C200431,4 TMI [End  ] 
  SHOW GET m.Price &lcStat
  *C200431,4 TMI [Start] Disable only if there is a price code for this style  
  *SHOW GET m.Comm1 &lcStat       
  *SHOW GET m.Comm1 &lcCommStat  *B606927,1 TMI
  *C200431,4 TMI [End  ] 
  SHOW GET m.COMPLETE ENABLE

  *B606927,1 TMI [Start] Disable price , discount , gros price , commession 
  SHOW GET m.Gros_Price DISABLE
  SHOW GET m.Price      DISABLE
  SHOW GET m.Comm1      DISABLE
  *B606927,1 TMI [End  ] 

  *--Next/Prev buttons
  =lfPrNxBrow()
  
  *B132317,1  TMI [Start] Disable the m.complete field
  *_CUROBJ = OBJNUM(M.COMPLETE)
  _CUROBJ = OBJNUM(m.TotQty)
  M.COMPLETE = {}   
  SHOW GET M.COMPLETE DISABLE
  *B132317,1  TMI [End  ] 

  
  *C200551,3  TMI [Start] if all colors are cancelled notify by a message
  IF RECCOUNT(lcTempCur) = 0
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'All colours are not Active.')
    _CUROBJ = OBJNUM(pbClear)
  ENDIF
  *C200551,3  TMI [End  ] 
   
ENDIF

*-- end of lfvQStyle.

*:**************************************************************************
*:* Name        : lfNxM1stDy
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/18/2002
*:* Purpose     : Get Next month first day
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfNxM1stDy()
*:***************************************************************************
*C#200431,1
FUNCTION lfNxM1stDy
lnMonth = IIF(MONTH(gdSysDate)=12,1,MONTH(gdSysDate)+1)
lnYear  = IIF(MONTH(gdSysDate)=12,YEAR(gdSysDate)+1,YEAR(gdSysDate))
ldFrstDay = CTOD('1/'+STR(lnMonth)+'/'+str(lnYear))
RETURN ldFrstDay
*-- end of lfNxM1stDy.
*:**************************************************************************
*:* Name        : lfvQkSav                                       
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/18/2002
*:* Purpose     : Save valid fucntio for screen quickord
*:***************************************************************************
*:* Called from : quickord.scx
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvQkSav()
*:***************************************************************************
*C#200431,1
FUNCTION lfvQkSav
PRIVATE lcOrd,lnQkCnt,lnJ,lcJ,lcQ,lnIncrmnt,lnK,llDfChkQty
IF laTot[1,2] = 0
  RETURN
ENDIF

*C123851,3  TMI [Start] comment this code and move it after all checks
*-* *C200520,1 ABD - Remove the filter before start saving. [Begin]
*-* SELECT (lcTempCur)
*-* SET ORDER TO TAG (lcTempCur)
*-* SET FILTER TO 
*-* *C200520,1 ABD - [End]
*C123851,3  TMI [End  ] 

*-- Complete date can not be empty
*B132317,1  TMI [Start] no need for this warning 
*IF EMPTY(m.COMPLETE)
*  =gfModalGen('INM00001B00000',.F.,.F.,.F.,'Complete date can not be empty.')
*  _CUROBJ = OBJNUM(m.COMPLETE)
*  RETURN
*ENDIF
*B132317,1  TMI [End  ] 

*-- If the complete date has been used on a previous ORDLINE for that Style - 
*-- then show a warning message with the option to change the Complete Date or Clear.
*B606945,1 ASH 02/16/2003 (Begin) Check the completion date for the same order not all orders.
*SELECT ORDLINE
*GO TOP
*IF SEEK(SUBSTR(m.STYLE,1,lnMajorLen))
*  LOCATE REST WHILE STYLE = SUBSTR(m.STYLE,1,lnMajorLen) ;
*              FOR COMPLETE = M.COMPLETE
*ENDIF              

SELECT (lcOrdLine)
IF SEEK(lcOrdType+laData[1]+laData[3]+SUBSTR(m.STYLE,1,lnMajorLen))
  LOCATE REST WHILE CORDTYPE+ORDER+STORE+STYLE = lcOrdType+laData[1]+laData[3]+SUBSTR(m.STYLE,1,lnMajorLen);
              FOR COMPLETE = M.COMPLETE
ENDIF
*B606945,1 ASH 02/16/2003 (End)
IF FOUND()
  *--Button 32014 : Chan\<ge the Complete Date;\<Clear
  lnResp = gfModalGen('QRM00000B32014',.F.,.F.,.F.,;
                      'The selected complete date has been used on a previous ORDLINE for that Style')
  IF lnResp = 1
  ELSE
    *B606945,1 ASH 02/16/2003 (Begin) Check the completion date for the same order not all orders.
    IF lnResp = 2
      _CUROBJ = OBJNUM(m.COMPLETE)  
    ELSE
      =lfvqClear() 
    ENDIF
    *B606945,1 ASH 02/16/2003 (End)
    RETURN
  ENDIF                      
ENDIF

*C123851,3  TMI [Start] remove the filter to collect data
SELECT (lcTempCur)
*C127807,1  TMI [Start] Save old filter
PRIVATE lcFilter
lcFilter = FILTER()
*C127807,1  TMI [End  ] 
SET ORDER TO TAG (lcTempCur)
SET FILTER TO 
GO TOP
*C123851,3  TMI [End  ] 

*- Compare check qty with total qty
*C200520,1 ABD - sum the total qty to check the order qty. [Begin]
SELECT (lcTempCur)
*B128755,1  TMI [Start] Get the correct total sum
*SUM nTotal FOR EMPTY(LASTLINE) To laTot[1,2]
LOCATE FOR LASTLINE = CHR(255)
laTot[1,2] = nTotal
*B128755,1  TMI [End  ] 
LOCATE
*C200520,1 ABD - [End]

llDfChkQty = m.TotQty <> laTot[1,2]
IF llDfChkQty AND gfModalGen('QRM32031B32000','ALERT',ALLTRIM(STR(laTot[1,2],8)))=2  
  *C127807,1  TMI [Start] restore old filter
  SELECT (lcTempCur)
  SET FILTER TO &lcFilter
  GO TOP
  *C127807,1  TMI [End  ] 
  RETURN .F.
ENDIF

SELECT (lcTempNote)
SCATTER MEMVAR MEMO

*--Assign variables values
m.CORDTYPE  = lcOrdType
m.ORDER     = laData[1]
m.ACCOUNT   = laData[2]
m.STORE     = laData[3]
m.CustPo    = IIF(!laData[7],&lcOrdHdr..CustPo,'')   &&Save Cust PO # in OrdLine if not Multi Store
*C200431,4 TMI [Start] use the value "laWareHouse[lnWareHouse,2]" instead of laData[31]
*m.cWareCode = laData[31]
m.cWareCode = laWareHouse[lnWareHouse,2]
*C200431,4 TMI [End  ] 
m.Start     = laData[9]

*C200520,1 ABD - Remark the next lines and replace with new procedure to save the the data
*C200520,1 ABD - with a new way from the new temp file. [Begin]
*SELECT (lcTempCur)
*GO TOP
*SCAN FOR EMPTY(LASTLINE) AND NTOTAL>0
*  lnIncrmnt = 0
*  FOR lnQkCnt = 1 TO ALEN(laExtSz,1)    
*  *FOR lnQkCnt = 1 TO MIN( ALEN(laExtSz,1) , 10 )
*    SELECT (lcTempCur)
*    m.Style = STYMAJOR+lcSepart+SUBSTR(COLOR,1,lnClrLen)+lcClrSpr+laExtSz[lnQkCnt,1]
*    IF SEEK(m.Style,'STYLE')
*      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,;
*                 m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8,m.TotBook
*      FOR lnJ = lnIncrmnt + 1 TO lnIncrmnt + laExtSz[lnQkCnt,2]
*        lcJ = ALLT(STR(lnJ))
*        lcQ = STR(lnJ - lnIncrmnt,1)
*        m.TotQty = m.TotQty +  S&lcJ
*        m.Qty&lcQ = S&lcJ
*        m.Book&lcQ = S&lcJ
*      ENDFOR    
*      m.TotBook  = m.TotQty
*      m.Scale    = STYLE.SCALE     
*      m.Desc1    = STYLE.Desc1 
*      m.PrePak   = Style.PrePak
*      m.Season   = Style.Season
*      m.Gl_Sales = ALLTRIM(laData[53])+Style.cSlsGlLink
*      m.Gl_Sales = IIF(laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')
*      m.Flag     = 'N' 
*
*      *-- Get price for each line of scales has different prices , 
*      *-- or check qty differs from entred qty        
*            
*      IF m.TotQty > 0
*        *B606927,1 TMI [Start] get prices
*        *\* IF llCodPrice
*        *\*   =lfCodPrice(.T.)
*        *\* ELSE
*        *\*   IF llDifPrice
*        *\*     =lfQkPrice()
*        *\*   ENDIF
*        *\* ENDIF
*        m.Gros_Price = VAL(SUBSTR(SCLPRC,(lnQkCnt-1)*23+1        ,12))
*        m.Comm1      = VAL(SUBSTR(SCLPRC,(lnQkCnt-1)*23+1 +12    ,5 ))
*        *m.Disc_Pcnt = VAL(SUBSTR(SCLPRC,(lnQkCnt-1)*23+1 +12 +5 ,6 ))
*        m.Price      = ROUND( m.Gros_Price*(1-m.Disc_Pcnt/100) , 2 )
*        *B606927,1 TMI [End  ] 
*        
*        *IF m.TotQty>0
*        lnLines    = lnLines + 1
*        m.LineNo   = lnLines      
*        INSERT INTO (lcOrdLine) FROM MEMVAR
*        *--Update laData array 
*        laData[35] = laData[35] + m.TotBook
*        laData[36] = laData[36] + m.TotBook*m.Price
*        laData[41] = laData[41] + m.TotQty
*        laData[42] = laData[42] + m.TotQty*m.Price          
*        *ENDIF        
*      ENDIF      
*      lnIncrmnt = lnIncrmnt + laExtSz[lnQkCnt,2]
*    ENDIF
*  ENDFOR
*ENDSCAN

*C123847,1  TMI [Start] save function for DIR03
IF ASCAN(laEvntTrig,PADR('COLLFIT',10))<>0
  =lfSavFit()
ELSE  
  *C123847,1  TMI [End  ] 
  *-- Save the data with a new way.
  DO lpQukSave
  *C123847,1  TMI [Start] 
ENDIF  
*C123847,1  TMI [End  ]   
*C200520,1 ABD - [End]

*--Save NOTE_MEM
SELECT (lcTempCur)
LOCATE
m.Style = STYMAJOR
SELECT (lcOrdLine)
IF SEEK(lcOrdType+laData[1]+laData[3]+m.Style,(lcOrdLine))
  M.NOTE_MEM = &lcTempNote..NOTE_MEM
  *--Clear field note_mem for colors other than the first color filled with qty>0
  SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = lcOrdType+laData[1]+laData[3]+m.Style
    REPLACE NOTE_MEM WITH m.Note_mem
    m.Note_mem = ''      
  ENDSCAN
ENDIF 
*-- Clear screen
=lfvqClear(.T.)
*-- end of lfvQkSav.

*:**************************************************************************
*:* Name        : lfvqTotQty                                       
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/2002
*:* Purpose     : Valid fn for total qty
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqTotQty()
*:***************************************************************************
*C#200431,1
FUNCTION lfvqTotQty
IF m.TotQty < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.TotQty = lcOldValue
  _CUROBJ = OBJNUM(m.TotQty)
  RETURN
ENDIF
IF LASTKEY()=15
  ACTIVATE WINDOW (lcQkWin3)
  _CUROBJ=OBJNUM(pbExit)  
ENDIF
=lfDoTab()

*-- end of lfvqTotQty.

*:**************************************************************************
*:* Name        : lfvqStyDesc                                       
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqStyDesc()
*:***************************************************************************
*C#200431,1
FUNCTION lfvqStyDesc
  
*-- end of lfvqStyDesc.

*:**************************************************************************
*:* Name        : lfvNotes                                         
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/24/2002
*:* Purpose     : Entering Line notes , that will be saved againest the first 
*                 color that has quantity
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvNotes()
*:***************************************************************************
*C#200431,1
FUNCTION lfvNotes
PRIVATE lnRecno,lcAlias
SELECT (lcTempNote)
DO (gcScrDir+"ARLNOTES.SPX")
SELECT (lcTempCur)
*-- end of lfvNotes.

*:**************************************************************************
*:* Name        : lfvqGPrice
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/23/2003
*:* Purpose     : Gross price valid fn
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqGPrice()
*:***************************************************************************
*C#200431,1
FUNCTION lfvqGPrice
IF m.Gros_Price < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Gros_Price = lcOldValue
  _CUROBJ = OBJNUM(m.Gros_Price)
  RETURN
ENDIF
m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
SHOW GET m.Price
=lfDoTab()

*-- end of lfvqGPrice.

*:**************************************************************************
*:* Name        : lfvqPrcDisc                                       
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/2002
*:* Purpose     : Discount on price valid fn.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqPrcDisc()
*:***************************************************************************
*C#200431,1
FUNCTION lfvqPrcDisc
IF m.Disc_Pcnt < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Disc_Pcnt = lcOldValue
  _CUROBJ = OBJNUM(m.Disc_Pcnt)
  RETURN
ENDIF
IF m.Disc_Pcnt > 99.99
  *--Can not exceed 999
  *-- 40171 : ð cannot exceeds ð  
  =gfModalGen('TRM40171B00000','DIALOG','Percent Discount|99.99')
  m.Disc_Pcnt = lcOldValue
  _CUROBJ = OBJNUM(m.Disc_Pcnt)
  RETURN
ENDIF
m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
SHOW GET m.Price
=lfDoTab()
*-- end of lfvqPrcDisc.

*:**************************************************************************
*:* Name        : lfvqNPrice
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/23/2003
*:* Purpose     : Net  price valid fn
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqNPrice()
*:***************************************************************************
*C#200431,1
FUNCTION lfvqNPrice

IF m.Price < 0
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Price = lcOldValue
  _CUROBJ = OBJNUM(m.Price)
  RETURN
ENDIF

m.Price = IIF(m.Price>m.Gros_Price,m.Gros_Price,m.Price)
m.Disc_Pcnt = IIF(m.Gros_Price=0,0,100-m.Price*100/m.Gros_Price)

SHOW GET m.Gros_Price
SHOW GET m.Disc_Pcnt
SHOW GET m.Price

=lfDoTab()
*-- end of lfvqNPrice.


*:**************************************************************************
*:* Name        : lfvqComm                                        
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/25/2002
*:* Purpose     : Valid fn for commesion 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqComm()
*:***************************************************************************
*C#200431,1
FUNCTION lfvqComm

*-- end of lfvqComm.

*:**************************************************************************
*:* Name        : lfGetClrD                                       
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2002
*:* Purpose     : Get Color Position and Color length
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetClrD()
*:***************************************************************************
*C#200431,1
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
PRIVATE lnCount &&Tmi 07/15/2002
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])
    EXIT
  ENDIF
ENDFOR

*C200452,1 TMI [Start] Get size position and size segment lenght 
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnSizeLen = LEN(laItemSeg[lnCount,3])
    lnSizePos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*C200452,1 TMI [End  ] 

SELECT(lcOldSelect)
*--end function lfGetClrD

*:**************************************************************************
*:* Name        : lfvQkClose                                     
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/24/2002
*:* Purpose     : Close fn. for quickord screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvQkClose()
*:***************************************************************************
*C#200431,1
FUNCTION lfvQkClose
PRIVATE llClose
llClose = .T.
IF (llEdit AND llChang) OR (!llEdit AND laTot[1,2]>0)
  *M38093   ð. Are you sure you want to proceed with closing ð?  
  *B32005   \<Proceed;\?\<Cancel
  llClose = (gfModalGen('QRM38093B32005','ALERT','Quantities have been '+IIF(llEdit,'changed','entered');
                                                +'|The Quick Order Entry screen')=1)
ENDIF
IF llClose
  ERASE (gcWorkDir+lcTempCur+'.*')
  ERASE (gcWorkDir+lcTempNote+'.*')
  CLEAR READ
ELSE
  ACTIVATE WINDOW (lcDet_Ttl)  
ENDIF

*-- end of lfvQkClose.

*:**************************************************************************
*:* Name        : lfCodPrice
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/29/2002
*:* Purpose     : Get price from new customer code price file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCodPrice()
*:***************************************************************************
*C#200431,1
FUNCTION lfCodPrice
PARAMETERS lnFrmDav   && Variable means if .t. the fn. is called from within davmain.prg
PRIVATE lnSlct,llSPCSTPRI,llCodFound
llCodFound = .F.
lnSlct = SELECT()
IF !USED('CSTPRICE')
  =gfOpenFile(gcDataDir+'CSTPRICE','CSTPRICE','SH')
ENDIF

llSPCSTPRI = gfGetMemVar('M_SPCSTPRI')
IF lnFrmDav .OR. llSPCSTPRI
  IF SEEK(lfPRICCODE()+laData[33]+m.Style,'CSTPRICE')
    llCodFound = .T.
    m.Gros_Price = CSTPRICE.PRICEDV
    *C200431,4 TMI [Start] Get discount value from the screen and apply it to the price
    *m.Price    = m.Gros_Price
    *m.Disc_Pcnt = 0 
    m.Price    = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
    *C200431,4 TMI [End  ] 
    
    IF CSTPRICE.COMMDV <> 0  
      m.Comm1 = IIF(!EMPTY(laData[27]),CSTPRICE.COMMDV,0)
      m.Comm2 = IIF(!EMPTY(laData[29]),CSTPRICE.COMMDV,0)
    *ELSE  
      *m.Comm1 = IIF(!EMPTY(laData[27]),laData[28],0)
      *m.Comm2 = IIF(!EMPTY(laData[29]),laData[30],0)
    ENDIF
  ELSE  
    IF lnFrmDav
      IF llDifPrice .OR. llShw1
        =lfQkPrice()    
        llShw1 = .F.
      ENDIF
    ENDIF    
  ENDIF
ENDIF

SELECT (lnSlct)

RETURN llSPCSTPRI .AND. llCodFound
*-- end of lfCodPrice.

*:**************************************************************************
*:* Name        : lfPRICCODE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/02/2003
*:* Purpose     : get Customer Price code 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPRICCODE()
*:***************************************************************************
*C#200431,1
FUNCTION lfPRICCODE
*C127807,1  TMI [Start] get the field name to Compare valid  prices with ( for Banach based on setup - Entered, Start or Complete Dates)
IF ASCAN(laEvntTrig,'CHKPRIAG') <> 0
  lcChkDate = gfDoTriger('SOORD','CHKPRIAG')
ENDIF
*C127807,1  TMI [End  ] 
IF TYPE('lcPRICCODE')#'C' .OR. EMPTY(lcPRICCODE)
  PRIVATE lcSlct,lnRecno
  lcSlct = SELECT()
  SELECT CUSTOMER
  lnRecno = RECNO()
  =SEEK('M'+laData[2],'CUSTOMER')
  *C200551,1  TMI [Start] Pick the suitable price code with respect to the Entered date
  *lcPRICCODE = UPPER(CUSTOMER.PRICCODE)
  PRIVATE lnI,lcI,lcNoDtPCod
  IF !USED('CSTPRICH')
    =gfOpenFile(gcDataDir+'CSTPRICH','CSTPRICH','SH')
  ENDIF
  *-- Loop through all price codes
  *-   pick the first price code with suitable date,
  *-   if no one, pick the first one with no valid dates , otherwise return chr(255)
  lM_PRCCOD =gfGetMemvar('M_PRCCOD')
  *C123847,1  TMI [Start] case if other than DavLuck used davmain and the option M_PRCCOD is not defined for him
  lM_PRCCOD = IIF(TYPE('lM_PRCCOD')#'L',.T.,lM_PRCCOD)
  *C123847,1  TMI [End  ] 
  lcNoDtPCod = CHR(255)
  *ash123853
  PRIVATE lnClrPos,lnClrLen
  STORE 0  TO lnClrPos,lnClrLen
  =lfGetClrD()
  FOR lnI = 1 TO 15
    lcI = IIF(lnI = 1 , '' , PADL(lnI,2,'0') ) 
    *B119490,1  TMI [Start] locate price code for current style
    *IF !EMPTY(CUSTOMER.PRICCODE&lcI) .AND. SEEK(CUSTOMER.PRICCODE&lcI+laData[33],'CSTPRICH')
    *B124083,1  TMI [Start] Add the style field to the key of CSTPRICH file
    *IF !EMPTY(CUSTOMER.PRICCODE&lcI) .AND. SEEK(CUSTOMER.PRICCODE&lcI+laData[33],'CSTPRICH') ;
                                     .AND. SEEK(CUSTOMER.PRICCODE&lcI+laData[33]+STYLE.STYLE,'CSTPRICE')
    PRIVATE lcStyClr
    lcStyClr = IIF(lM_PRCCOD , '' , PADR(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),19) )
    IF !EMPTY(CUSTOMER.PRICCODE&lcI) .AND. SEEK(CUSTOMER.PRICCODE&lcI+laData[33]+lcStyClr,'CSTPRICH') ;
                                     .AND. SEEK(CUSTOMER.PRICCODE&lcI+laData[33]+STYLE.STYLE,'CSTPRICE')
      *B124083,1  TMI [End  ] 
     
      *B119490,1  TMI [End  ]                                       
      IF EMPTY(CSTPRICH.DVLDPRTO)
        *- Get no valid date price code
        lcNoDtPCod = UPPER(CUSTOMER.PRICCODE&lcI)
      ELSE
        *C127807,1  TMI [Start] Compare valid  prices for Banach based on setup ( Entered, Start or Complete Dates)
        IF ASCAN(laEvntTrig,'CHKPRIAG') <> 0
          IF BETWEEN(&lcChkDate,CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)
            EXIT
          ENDIF
        ELSE
          *C127807,1  TMI [End  ]         
          
          IF BETWEEN(laData[8],CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)
            EXIT
          ENDIF
          
          *C127807,1  TMI [Start] 
        ENDIF
        *C127807,1  TMI [End  ] 
      ENDIF
    ENDIF    
  ENDFOR
  lcPRICCODE = IIF(lnI < 16 , UPPER(CUSTOMER.PRICCODE&lcI) , lcNoDtPCod )
  *C200551,1  TMI [End  ]   

  IF BETWEEN(lnRecno,1,RECCOUNT('CUSTOMER'))
    *C200551,1  TMI [Start] go to record in the correct alias
    *GOTO (lnRecno)
    GOTO (lnRecno) IN CUSTOMER  
    *C200551,1  TMI [End  ] 
  ENDIF
  SELECT (lcSlct)
ENDIF
RETURN lcPRICCODE
*-- end of lfPRICCODE.
*:**************************************************************************
*:* Name        : lfQkPrice                                       
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2002
*:* Purpose     : Get Prices in save case
 *:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfQkPrice()
*:***************************************************************************
*C#200431,1
FUNCTION lfQkPrice

  *--Get style price according to ordered quantity
  PUSH KEY
  ON KEY
  m.lContract = lcOrdType<>'C' .AND. lfvContPri(m.Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
  IF !m.lContract
    m.Gros_Price = MAX( lfGetprice(m.Style,lcPriceLvl,m.TotQty) , 0 )
  ENDIF
  POP KEY

  m.Price    = m.Gros_Price
  
  *-- Get Style discount percent and Calculate net price  
  IF !m.lContract
    *-- get the cDiscCode From stydye in every case
    *C200431,4 TMI [Start] use the value "laWareHouse[lnWareHouse,2]" instead of laData[31]
    *lcDiscCode  = IIF(SEEK(m.Style+laData[31]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
    *m.Disc_Pcnt = 0    && We get this value from the screen
    lcDiscCode  = IIF(SEEK(m.Style+laWareHouse[lnWareHouse,2]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
    *C200431,4 TMI [End  ] 
    IF !EMPTY(ALLTRIM(lcDiscCode))
      *-- Get the disecound related filed to now which 
      *-- type whole Sale Or Retail sale Or Both.
      DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
      STORE '' To lcDisType , ldstartDte ,ldEndDate
      *-- Array to get the Discount affect for DecCode.
      laDisType[1,1]  = 'CCOSTAFECT'
      laDisType[1,2]  = 'lcDisType'
      *-- Array to get the start date For DescCode.
      lastartDte[1,1] = 'START'
      lastartDte[1,2] = 'ldstartDte'
      *-- Array to get the end date For DescCode.
      laEndDate[1,1]  = 'DENDATE'
      laEndDate[1,2]  = 'ldEndDate'
      = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
      = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
      = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
      lnDisc_Pcnt = 0
      IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[8],ldstartDte,ldEndDate)
        lnDisc_Pcnt = m.Disc_Pcnt
        =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
        m.Disc_Pcnt = lnDisc_Pcnt
      ENDIF
    ENDIF  
    m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100  
  ENDIF 

*-- end of lfQkPrice.

*:**************************************************************************
*:* Name        : lfQkWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2002
*:* Purpose     : When Fn that disable tab
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfQkWhen()
*:***************************************************************************
*C#200431,1
FUNCTION lfQkWhen
lcOldValue = EVALUATE('m.' + SYS(18))
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
*-- end of lfQkWhen.

*:**************************************************************************
*:* Name        : lfDoTab
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2002
*:* Purpose     : ReDefine tab keys
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfDoTab()
*:***************************************************************************
*C#200431,1
FUNCTION lfDoTab
ON KEY LABEL BACKTAB DO lpBacktab 

*-- end of lfDoTab.

*:**************************************************************************
*:* Name        : lfwGPrice
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/18/2002
*! Purpose      : Disable When function for Style Gross price (Specialized for MBI)
*               : This is to use only price level A
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwGPrice()
*:***************************************************************************
*C#200431,1
FUNCTION lfwGPrice
RETURN .T.

*:**************************************************************************
*:* Name        : lfvStyInq
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/01/2003
*:* Purpose     : Style inquery 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvStyInq()
*:***************************************************************************
*C#200431,1
FUNCTION lfvStyInq
PRIVATE lcParameter

lcParameter = "'"+m.Style+"',''"
DO gpDoProg WITH 'AWRICSTYLE',.F.,'IC',lcParameter
*-- end of lfvStyInq.

*:**************************************************************************
*:* Name        : lfGrpNo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/05/2003
*:* Purpose     : GROUP size No/Total No of Size group , The return value is 
*:*               displayed in the screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGrpNo()
*:***************************************************************************
*C#200431,1
FUNCTION lfGrpNo
PRIVATE lcRet
lcRet = PADL(lnFldGrp,2)+'/'+PADR(CEILING(ALEN(laSize)/lnBrFlCn),2)
RETURN lcRet
*-- end of lfGrpNo.

*:**************************************************************************
*:* Name        : lfGoTop 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Go top in temp file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGoTop ()
*:***************************************************************************
FUNCTION lfGoTop 
SELECT (lcTempCur)
GO TOP
=lfwBrowUp()
*-- end of lfGoTop .

*:**************************************************************************
*:* Name        : lfGoBtm
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Go bottom in temp file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGoBtm()
*:***************************************************************************
FUNCTION lfGoBtm
SELECT (lcTempCur)
GO BOTTOM
=lfwBrowUp()

*-- end of lfGoBtm.


*:**************************************************************************
*:* Name        : lfPoQOrdPD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Define pad line for po order entry screen
*:***************************************************************************
*:* Called from : postyle.prg
*:***************************************************************************
*:* Example     :  = lfPoQOrdPD()
*:***************************************************************************
*C200452,1
FUNCTION lfPoQOrdPD
lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Quic\<k Order Entry Screen'  ;
             SKIP FOR (lnActFolder<>2) .OR. (laScrMode[2])
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO gfDoTriger WITH "POSTY",PADR("POQKORD",10)
*-- end of lfPoQOrdPD.

*:**************************************************************************
*:* Name        : lfPOQKORD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Quick PO order entry screen for David Luke
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPOQKORD()
*:***************************************************************************
*C200452,1
FUNCTION lfPOQKORD
PRIVATE laSize,laTot,m.Style,m.Desc,m.Gros_Price,;
        lcDet_Ttl,lnClrLen,lnClrPos,lcClrSpr,lcTempCur,lcAlias,lnMrk ,;
        lcQkWin0,lcQkWin1,lcQkWin2,lcQkWin3,lcSepart,lcItemPct,laExtSz,lcOrd,llEdit,;
        laOldVal,lcBrowFlds,lcSvStyOrd
*vars
*-- laSize      : Array to Hold Sizes descriptions for the current style scale
*-- laTot       : Array to hold Column sum and nTotal field sum
*-- m.Style     : Style
*-- m.Desc      : Description
*-- m.Gros_Price: Gross price
*-- lcDet_Ttl   : Screen title
*-- lnClrLen    : Color lenght in style structure
*-- lnClrPos    : Color position in style structure
*-- lcClrSpr    : Color separator 
*-- lcTempCur   : Temp cursor name
*-- lcAlias     : Alias Variable
*-- lnMrk       : Marker used in browse
*-- lcQkWin0..3 : Temp variables for screens
*-- lcSepart    : Variable to hold style separator
*-- lcItemPct   : Variable to hold Style Picture
*-- laExtSz     : Array to hold Sizes
*-- lcOrd       : Variable to hold order
*-- llEdit      : Edit mode
*-- laOldVal    : Array to Hold old Values of the browse
*-- lcBrowFlds  : Variable to hold browse fields for the main browse screen for colors and sizes
*-- lnFldGrp    : No of group of fields that appear currently in browse
*-- lnBrFlCn    : Browse fields count (My design now is 10 fields )
*-- lnSizeLen   : Size lenght
*-- lnSizePos   : Size Position 
*-- lnGPrice,lnDiscPcnt,lnCost1,lnLinTotQ : These variables are used in the screen to update Gross price
*           Discount,Net Price Total Line Qty  when moving for each size , assign the appropriate group price
*-- ln1stSz     : No of field that is the first size in block of sizes
*-- lnPrNxTime  : Variable to save last time that the key is pressed
*-- lcSvStyOrd  : Saves order of the style file

SELECT (lcPOLine)

SCATTER MEMVAR BLANK
DIMENSION laSize[1],laTot[1,2],laExtSz[1,2],laOldVal[1]
STORE '' TO laSize,lcBrowFlds,lcClrSpr,lcGrpNo,lcOldVal
STORE 0 TO laTot,lnClrLen,lnClrPos,m.Gros_Price,laOldVal,lnSizePos,lnSizeLen,ln1stSz
STORE 0 TO lnGPrice,lnDiscPcnt,lnCost1,lnLinTotQ
STORE 1 TO lnMrk,lnFldGrp
STORE .F. TO llEdit
STORE .T. TO llChang
lnBrFlCn = 10
lnPrNxTime = SECONDS()

*C200452,4 TMI [Start] Save order of the style file
lcSvStyOrd = ORDER('STYLE')
*C200452,4 TMI [End  ] 

STORE 0 TO laPhyStk,laFreStk
lcKeyBmp   = gcBmpHome + "ExtKey.BMP"

lcItemPct = gfItemMask('PI')
lcSepart  = SUBSTR(lcItemPct,lnMjrWid+1,1)
*-- Get color position and color length 
=lfGetClrD()

*--Open needed files
=gfOpenFile(gcDataDir+'BOM','BOM','SH')

lcDet_Ttl = 'Quick Lines Entry Screen'

lcQkWin0    = gftempname()     &&Temp name for Windows.
lcQkWin1    = gftempname()     &&Temp name for Windows.
lcQkWin2    = gftempname()     &&Temp name for Windows.
lcQkWin3    = gftempname()     &&Temp name for Windows.
lcTempCur   = gfTempName()     && Cursor holds qty /color/size
lcGrpCur   = gfTempName()     && Cursor holds qty /color/size

PUSH KEY

*--Define keys to navigate throgh the browse by keys
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDet_Ttl) 
ON KEY LABEL TAB
ON KEY LABEL BACKTAB DO lpBktab 
ON KEY LABEL CTRL+LEFTARROW  DO lfPrNxPoBr WITH -1
ON KEY LABEL CTRL+RIGHTARROW DO lfPrNxPoBr WITH 1 
ON KEY LABEL CTRL+UPARROW    DO lfpoGoTop
ON KEY LABEL CTRL+DNARROW    DO lfpoGoBot
ON KEY LABEL ESC             DO lfpoQkClos
ON KEY LABEL CTRL+R          DO lfvqPoCler WITH .T.
ON KEY LABEL CTRL+S          DO lfvQpoSav

lcAlias = SELECT(0)
lcOrd = ORDER(lcPoLine)   

*-- Create temp colors , sizes file
=lfpoCrtTmp(1)   

DO (gcScrDir+'PO\PoQkOrd.SPX')
POP KEY

SELECT (lcAlias)
SET ORDER TO &lcOrd IN &lcPoLine

*--Refresh the detail screen in the postyle.spx
SHOW WINDOW (lcBrTtl1) REFRESH SAME
*C200452,4 TMI [Start] Restore order of the style file
SET ORDER TO &lcSvStyOrd IN STYLE
*C200452,4 TMI [End  ] 

*-- end of lfPOQKORD.

*:**************************************************************************
*:* Name        : lfpoCrtTmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Create the temp file lcTempcur to used by qkordent screen in PO
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfpoCrtTmp()
*:***************************************************************************
*C200452,1
FUNCTION lfpoCrtTmp
PARAMETERS lnFlds

PRIVATE lnFl,lnJ,lcJ,lcCrtTbl,laTblFld
DIMENSION laTblFld[1,4]
lnFl = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "LASTLINE"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 1
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "STYMAJOR"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = lnMjrWid
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "COLOR"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = lnClrLen
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "COLORDSC"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 20
laTblFld[lnFl,4] = 0

*-- Separate each group of fields to ease trace
ln1stSz = lnFl
*--Qty fields
FOR lnJ = 1 TO lnFlds
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "S&lcJ"
  laTblFld[lnFl,2] = "N"
  laTblFld[lnFl,3] = 7
  laTblFld[lnFl,4] = 0
ENDFOR

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "NTOTAL"
laTblFld[lnFl,2] = "N"
laTblFld[lnFl,3] = 9
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "OLDTOT"
laTblFld[lnFl,2] = "M"
laTblFld[lnFl,3] = 10
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "LCHANGED"
laTblFld[lnFl,2] = "L"
laTblFld[lnFl,3] = 1
laTblFld[lnFl,4] = 0

*C123847,1  TMI [Start] create temp files for DIR03 
IF ASCAN(laEvntTrig,PADR('POTMPFIT',10))<>0 
  =lfPOTMPFIT()
ELSE
  *C123847,1  TMI [End  ] 

  IF ALEN(laTblFld,1)<=256
    CREATE TABLE &gcWorkDir.&lcTempCur FROM ARRAY laTblFld
    INDEX ON LASTLINE+STYMAJOR+COLOR TAG (lcTempCur)
  ELSE
    RETURN .F.  
  ENDIF   
  
  *C123847,1  TMI [Start] 
ENDIF  
*C123847,1  TMI [End  ] 

*===========< Create a temp file to store style with size groups >=============*
lnFl = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "STYLE"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 19
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "SIZES"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 8
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "GROS_PRICE"
laTblFld[lnFl,2] = "N"
laTblFld[lnFl,3] = 12
laTblFld[lnFl,4] = 2

CREATE TABLE &gcWorkDir.&lcGrpCur FROM ARRAY laTblFld
*INDEX ON STYLE+SIZES+STR(LINENO,6) TAG &lcGrpCur   &&tmi
INDEX ON STYLE+SIZES TAG &lcGrpCur

*-- end of lfpoCrtTmp.


*:**************************************************************************
*:* Name        : lfPOTMPFIT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/06/2005
*:* Purpose     : create temp files for DIR03 
*:***************************************************************************
FUNCTION lfPOTMPFIT

PRIVATE lnFl,lnJ,lcJ,lcCrtTbl

DIMENSION laTblFld[1,4]
lnFl = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "LASTLINE"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 1
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "STYMAJOR"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = lnMjrWid
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "COLOR"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = lnClrLen
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "COLORDSC"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 20
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "NTOTAL"
laTblFld[lnFl,2] = "N"
laTblFld[lnFl,3] = 6
laTblFld[lnFl,4] = 0

FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "S&lcJ"
  laTblFld[lnFl,2] = "N"
  laTblFld[lnFl,3] = 5
  laTblFld[lnFl,4] = 0
ENDFOR

FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "Size&lcJ"
  laTblFld[lnFl,2] = "C"
  laTblFld[lnFl,3] = 5
  laTblFld[lnFl,4] = 0
ENDFOR

FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "Pric&lcJ"
  laTblFld[lnFl,2] = "N"
  laTblFld[lnFl,3] = 12
  laTblFld[lnFl,4] = 2
ENDFOR

FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "DISCNT&lcJ"
  laTblFld[lnFl,2] = "N"
  laTblFld[lnFl,3] = 12
  laTblFld[lnFl,4] = 2
ENDFOR

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "FIT"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 5
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "FITNO"
laTblFld[lnFl,2] = "N"
laTblFld[lnFl,3] = 1
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "GP"
laTblFld[lnFl,2] = "N"
laTblFld[lnFl,3] = 1
laTblFld[lnFl,4] = 0

FOR lnJ = 1 TO 10
  lcJ = ALLT(STR(lnJ))
  lnFl = lnFl + 1
  DIMENSION laTblFld[lnFl,4]
  laTblFld[lnFl,1] = "SCALE" + lcJ
  laTblFld[lnFl,2] = "C"
  laTblFld[lnFl,3] = 4
  laTblFld[lnFl,4] = 0
ENDFOR

lnFl = lnFl + 1
DIMENSION laTblFld[lnFl,4]
laTblFld[lnFl,1] = "LCHANGED"
laTblFld[lnFl,2] = "L"
laTblFld[lnFl,3] = 1
laTblFld[lnFl,4] = 0

CREATE TABLE &gcWorkDir.&lcTempCur FROM ARRAY laTblFld
INDEX ON LASTLINE+STYMAJOR+COLOR+STR(FITNO,1)+STR(GP,1) TAG (lcTempCur)

*-- end of lfPOTMPFIT.

*:**************************************************************************
*:* Name        : lfPOCOLLFT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/06/2005
*:* Purpose     : Collecting data for DIR03 for PO quick order entry screen
*:***************************************************************************
FUNCTION lfPOCOLLFT
  PRIVATE lnQty
  lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
  llClrIgnrd = .F.  

  *--Add a line for each color

  FOR lnClrs = 1 TO ALEN(laClr,1)
    
    *--If this color has no lines in BOM ignore it
    IF !(laData[3]$'HB')
      IF !SEEK(PADR(m.Style,19)+'1'+SUBSTR(m.Style,1,lnMjrWid)+lcSepart+REPL('*',lnClrLen),'BOM') .AND. ;
        !SEEK(PADR(m.Style,19)+'1'+SUBSTR(m.Style,1,lnMjrWid)+lcSepart+laClr[lnClrs,1],'BOM')
        llClrIgnrd = .T.
        LOOP
      ENDIF
    ENDIF
    
    SELECT (lcTempCur)
    SCATTER MEMVAR BLANK
    M.STYMAJOR = PADR(m.Style,lnMjrWid)
    M.COLOR = laClr[lnClrs,1]
    M.COLORDSC = PADR(gfCodDes(PADR(laClr[lnClrs,1],lnClrLen) , 'COLOR'),20)
    
    *:***************************************************************************
    =SEEK(m.Style,'STYLE')
    lcStyScl = SUBSTR(STYLE.SCALE,1,lnScaleLen)
    =SEEK('S'+lcStyScl,'SCALE')
    *-- Get all sizes for current style.
    SELECT SCALE
    lnFitNo = 0
    DO WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl .AND. !EOF('SCALE')
  
      lcFit = SCALE.CDIM1
      lnFitNo = lnFitNo + 1
      lnSzCnt = 0
      lnGp = 1    
      SCAN REST WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl .AND.;
                      CDIM1 = lcFit

        FOR lnI = 1 TO SCALE.CNT
        
          lcI = STR(lnI,1)    
          lnSzCnt = lnSzCnt + 1
          IF lnSzCnt > 10
            lnGp = lnGp + 1
            lnSzCnt = 1
          ENDIF
 
          SELECT &lcTempCur
          IF !SEEK(' '+PADR(m.Style,lnMjrWid)+laClr[lnClrs]+STR(lnFitNo,1)+STR(lnGp,1),lcTempCur) 
            APPEND BLANK
            REPLACE STYMAJOR WITH m.Style        ;
                    COLOR    WITH laClr[lnClrs]  ;
                    COLORDSC WITH M.COLORDSC     ;
                    FIT      WITH lcFit          ;
                    FITNO    WITH lnFitNo        ;
                    GP       WITH lnGp
          ENDIF

          lcSeekSty = PADR(PADR(m.Style,lnMjrWid)+lcSepart+laClr[lnClrs]+lcClrSpr+Scale.Scale,19)
          WAIT WINDOW NOWAIT lcSeekSty
          
          *-If style is not assigned to the selected WH ...
          IF laSetups[5,2]='Y' .AND. !SEEK(lcSeekSty+laData[19]+SPACE(10),'StyDye')
            *E300408,1 Button : 40002  :: \<Add;\?\<Reenter
            lcMsg = 'Style ' + lcSeekSty + ' is not assigned to location ' + laData[19]
            lnResp = gfModalGen('QRM00000B40002',.F.,.F.,.F.,lcMsg)
            IF lnResp = 1
              DO gpAdStyWar WITH lcSeekSty,SPACE(10),laData[19]
            ELSE
              RETURN .F.
            ENDIF
          ENDIF

          *-- Get the price from CSTPRICE.dbf or from style.dbf file
          STORE 0 To lnStyComm , lnGrssPric
          =SEEK(lcSeekSty,'STYLE')
          
          STORE 0 TO lnQty,lnGrssPric,lnDiscnt 
          
          IF SEEK(lcSeekSty,lcPoLine)
            SELECT &lcPoLine
            LOCATE REST WHILE STYLE = lcSeekSty FOR !EMPTY(QTY&lcI)
            IF FOUND()
              lnGrssPric = &lcPoLine..GROS_PRICE 
              lnDiscnt   = &lcPoLine..DISC_PCNT 
              lnQty      = &lcPoLine..QTY&lcI
            ENDIF
          ELSE
            IF SEEK(lcSeekSty,lcGrpCur)
              SELECT &lcGrpCur
              LOCATE REST WHILE STYLE = lcSeekSty FOR lcI $ SIZES
              lnGrssPric = &lcGrpCur..GROS_PRICE 
            ENDIF
          ENDIF
          
          lcSzCnt = ALLTRIM(STR(lnSzCnt))
          SELECT (lcTempCur)        
          REPLACE SCALE&lcSzCnt  WITH SCALE.SCALE + lcI ;
                  Size&lcSzCnt   WITH Scale.SZ&lcI      ;
                  Pric&lcSzCnt   WITH lnGrssPric        ;
                  DISCNT&lcSzCnt WITH lnDiscnt          ;
                  S&lcSzCnt      WITH lnQty
                
        ENDFOR
      ENDSCAN
    ENDDO

  ENDFOR  
 
  =SEEK(PADR(m.Style,lnMjrWid),'STYLE')
  
  *--Add total lines
  FOR lnJ = 1 TO lnGp
    M.LASTLINE = CHR(255)
    M.STYMAJOR = ''
    M.COLOR    = ''
    M.COLORDSC = 'Totals'   
    M.GP = lnJ
    INSERT INTO (lcTempCur) FROM MEMVAR
  ENDFOR
  
  SELECT (lcTempCur)
  CALC MAX(GP) TO lnGp
  lcGrpNo = ' 1/'+STR(lnGp,1)
  IF lnGp>1
    SHOW GET pbNext ENABLE    
  ENDIF
  SHOW GET lcGrpNo
  GO TOP
  
  SHOW GET pbSav   DISABLE
  SHOW GET m.Style DISABLE
  
  lcRefStat = IIF(EMPTY(m.Reference),'ENABLE','DISABLE')
  SHOW GET m.Reference &lcRefStat
  SHOW GET m.cVenSty   &lcRefStat  

  _CUROBJ = OBJNUM(m.Reference)

  IF llClrIgnrd
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Some colours have no lines in BOM file , they are ignored')
  ENDIF    

  SET FILTER TO GP = 1  
  =lfBrowPoLn(.T.)
  =lfRefresh(lcQkWin3)
  
  SET ORDER TO &lcStyOrd IN STYLE

*-- end of lfPOCOLLFT.


*:**************************************************************************
*:* Name        : lfvqPoCler
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Clear funtion for the PO quick order entry screen 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqPoCler()
*:***************************************************************************
*C200452,1 
FUNCTION lfvqPoCler
PARAMETERS llFrmKeys

IF llFrmKeys AND EMPTY(m.Style)
  RETURN
ENDIF

DIME laSize[1]
STORE ' ' TO m.Style,m.Desc,laSize,m.Reference,m.cVenSty
STORE 0  TO laTot
STORE 0  TO lnGPrice,lnDiscPcnt,lnCost1,lnLinTotQ
STORE .F. TO llEdit
STORE 1 TO lnFldGrp

*C200452,4 TMI [Start] refresh style desctiption field
SHOW GET M.DESC
*C200452,4 TMI [End  ] 

SHOW GET m.Style ENABLE 

SHOW GET m.Reference DISABLE
SHOW GET m.cVenSty DISABLE

SHOW GET pbNote DISABLE
SHOW GET pbClear DISABLE
SHOW GET pbSav   DISABLE

_CUROBJ = OBJNUM(m.Style)

SELECT (lcTempCur)
ZAP  

=lfPrNxPoBr()
lcGrpNo = ' 1/1 '

SHOW GET pbNext DISABLE
SHOW GET pbPrev DISABLE
IF llFrmKeys
  ACTIVATE WINDOW (lcQkWin1)
  _CUROBJ = OBJNUM(M.STYLE)
ENDIF

*-- end of lfvqPoCler.

*:**************************************************************************
*:* Name        : lfPrNxPoBr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Previous/Next buttons for the browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPrNxPoBr()
*:***************************************************************************
*C200452,1
FUNCTION lfPrNxPoBr
PARAMETERS lnPrNx
PRIVATE lnFldGrpCn  && Field groups count
lnFldGrpCn = CEILING(ALEN(laSize)/lnBrFlCn)
*C200452,4 KHM [Start] Comment out , since the calling stack no of "lfPrNxPoBr" need not always be 17
**C200452,4 TMI [Start] The number of the funtion "lfPrNxPoBr" in calling stack sequence when
*C200452,4             called by pressing hot keys "CTRL+LEFTARROW/RIGHTARROW" is 17.
*C200452,4             If user press the combination of hot keys and hold pressing the stack 
*C200452,4             is increased , this cause an error and fox is terminate , so prevent the
*C200452,4             funtion to run in this case.
*PRIVATE lnCnt
*lnCnt = 1
*DO WHILE !EMPTY(PROGRAM(lnCnt))
*  lnCnt = lnCnt + 1
*ENDDO
*lnCnt = lnCnt - 1
*IF lnCnt > 17
*  RETURN
*ENDIF
**C200452,4 TMI [End  ] 
*C200452,4 KHM [End  ] 

IF !EMPTY(lnPrNx)
  IF lnPrNx = -1
    IF lnFldGrp = 1
      RETURN
    ENDIF
    lnFldGrp = lnFldGrp - 1  
  ENDIF  
  IF lnPrNx = 1
    IF lnFldGrp = lnFldGrpCn
      RETURN
    ENDIF
    lnFldGrp = lnFldGrp + 1  
  ENDIF
ENDIF

lcPrevStat = IIF(lnFldGrp = 1 , 'DISABLE' , 'ENABLE')
lcNextStat = IIF(lnFldGrp = lnFldGrpCn , 'DISABLE' , 'ENABLE' )

SHOW GET pbPrev &lcPrevStat
SHOW GET pbNext &lcNextStat

PRIVATE lnRecno
lnRecno = RECNO(lcTempCur)

*B129283,1 NNA 09/21/2005 (Start) Comment the Following lines and move it to before ending of the function
*-- Rebrowse and refresh
*=lfBrowPoLn(.T.)
*IF BETWEEN(lnRecno,1,RECCOUNT(lcTempCur))
*  GOTO lnRecno IN &lcTempCur
*ENDIF
*B129283,1 NNA (End)

*C123847,1  TMI [Start] 
IF ASCAN(laEvntTrig,'POBRWFIT')<>0
  SELECT &lcTempCur
  lcKey = SUBSTR(EVAL(KEY()),1,LEN(EVAL(KEY()))-1)
  SET FILTER TO 
  CALC MAX(GP) TO lnGp
  lcGrpNo = PADL(lnFldGrp,2)+'/'+STR(lnGp,1)
  SET FILTER TO GP = lnFldGrp
  IF !SEEK( lcKey )
    GO TOP
  ENDIF

  lcPrevStat = IIF(lnFldGrp = 1 , 'DISABLE' , 'ENABLE')
  lcNextStat = IIF(lnFldGrp = VAL(RIGHT(lcGrpNo,1)) , 'DISABLE' , 'ENABLE' )
  SHOW GET pbPrev &lcPrevStat
  SHOW GET pbNext &lcNextStat
  SHOW GET lcGrpNo 
ELSE
  *C123847,1  TMI [End  ] 
   
  lcGrpNo = lfGrpNo()
  
  *C123847,1  TMI [Start] 
ENDIF
*C123847,1  TMI [End  ]     

*B129283,1 NNA 09/21/2005 (Start) get the next lines from the previous section to here to get the next po window correctly
*-- Rebrowse and refresh
=lfBrowPoLn(.T.)
IF BETWEEN(lnRecno,1,RECCOUNT(lcTempCur))
  GOTO lnRecno IN &lcTempCur
ENDIF
*B129283,1 NNA (End)

=lfRefresh(lcQkWin3)

=lfwPoBrUp()
ACTIVATE WINDOW (lcDet_Ttl)

*-- end of lfPrNxPoBr.

*:**************************************************************************
*:* Name        : lfBrowPoLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Browse function for Po quick order entry screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfBrowPoLn()
*:***************************************************************************
*C200452,1 
FUNCTION lfBrowPoLn
PARAMETERS llGoTop

SELECT (lcTempCur)
IF llGoTop
  GO TOP
ENDIF  

*C123847,1  TMI [Start] 
IF ASCAN(laEvntTrig,'POBRWFIT')<>0
  =lfPOBRWFIT()
ELSE  
  *C123847,1  TMI [End  ] 

  lnMrk  = RECNO()
  lcBrowFlds = "cMrk=IIF(RECNO()=lnMrk,'>',' '):H=' ':R:1:W=.F.,"+;
               "COLORDSC:R:H=PADC('Colours',12):15:W=.F.,"
  PRIVATE lnSizeLn
  lnSizeLn = IIF(EMPTY(laSize[1]),0,ALEN(laSize,1))
  FOR lnX = 1+(lnFldGrp-1)*lnBrFlCn TO MIN(lnFldGrp*lnBrFlCn,lnSizeLn)
    lcX = ALLT(STR(lnX))
    lcBrowFlds = lcBrowFlds + "S&lcX:H='"+laSize[lnX]+"':6:V=lfvPoBrFld(&lcX):W=lfPoW(&lcX),"
  ENDFOR
  lcBrowFlds = lcBrowFlds + "NTOTAL:H='Total':R:W=.F."
  
  *Nearly 26 Character per field, Allowed maximum lenght of the browse string is 1024 , apporximatly 36 fields
  
  *C123847,1  TMI [Start] 
ENDIF
*C123847,1  TMI [End  ] 

SCATTER MEMVAR TO laOldVal
BROWSE FIELDS &lcBrowFlds;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       VALID :F lfvqBrow();       
       WHEN lfwPoBrUp();
       TITLE lcDet_Ttl ;
       WINDOW (lcQkWin2) IN WINDOW (lcQkWin0)

*=lfwPoBrUp()
*=lfRefresh(lcQkWin0)
=lfRefresh(lcDet_Ttl)

*-- end of lfBrowPoLn.

*:**************************************************************************
*:* Name        : lfPOBRWFIT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/07/2005
*:* Purpose     : Browse fields for DIR03 ( Considering Fits )
*:***************************************************************************
FUNCTION lfPOBRWFIT
PRIVATE lnX,lcX,laSize
  lnMrk  = RECNO()
  lcBrowFlds = "cMrk=IIF(RECNO()=lnMrk,'>',' '):H=' ':R:1:W=.F.,"+;
               "COLORDSC:R:H=PADC('Colours',12):15:W=.F.,"+;
               "FIT     :R:H='Fit' :7 :W=.F.,"

  IF RECCOUNT(lcTempCur) > 0
    
    SELECT &lcTempCur
    GO TOP
    SCATTER FIELDS SIZE1,SIZE2,SIZE3,SIZE4,SIZE5,SIZE6,SIZE7,SIZE8,SIZE9,SIZE10 TO laSize
    
    FOR lnX = 1 TO 10
      lcX = ALLT(STR(lnX))
      lcBrowFlds = lcBrowFlds + "S&lcX:H='"+laSize[lnX]+"':6:V=lfvPoBrFld(&lcX):W=lfPoW(&lcX),"
    ENDFOR

  ENDIF
  lcBrowFlds = lcBrowFlds + "NTOTAL:H='Total':R:W=.F."
*-- end of lfPOBRWFIT.

*:**************************************************************************
*:* Name        : lfwPoBrUp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Browse update line fn for the po quick order entry screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwPoBrUp()
*:***************************************************************************
*C200452,1 
FUNCTION lfwPoBrUp
PRIVATE lnI
lnMrk = RECNO(lcTempCur)
*--Initiate the array used instead of lcOldVal when variable
SCATTER MEMVAR TO laOldVal
SHOW WINDOW (lcDet_Ttl) REFRESH
*-- end of lfwPoBrUp.

*:**************************************************************************
*:* Name        : lfvQpoSty
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Valid function for style field for the po quick order entry screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvQpoSty()
*:***************************************************************************
*C200452,1 
FUNCTION lfvQpoSty

PRIVATE lcAlias,lcGetSty,lcStat,lnIncrmnt,lnClrs,lnCount,lnJ,lcSeekSty,laSum,laSavStat,lcStyle
*--- Select
lcAlias = ALIAS()

IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF

m.Style = PADR(ALLT(m.Style),lnMjrWid)
SET ORDER TO STYLE IN STYLE
IF !EMPTY(m.Style) AND ('?' $ m.Style .OR. !SEEK(m.Style,'STYLE'))
  SELECT STYLE  
  IF '?' $ m.Style
    GO TOP IN STYLE
    M.STYLE = ''
  ENDIF
  lcGetSty = PADR(gfStyBrw("M" , m.Style , "" , .F.),lnMjrWid)
  llBrowse = .F.
  IF EMPTY(lcGetSty)
    m.Style = ' ' 
    _CUROBJ = OBJNUM(m.Style) 
    RETURN
  ELSE
    m.Style = lcGetSty
  ENDIF
ENDIF  

*--If the po order is generated from a sales order , do not accept any style
IF lfSOGen()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This PO has been generated from a sales order. Cannot proceed.')
  m.Style = ' ' 
  _CUROBJ = OBJNUM(m.Style) 
  RETURN
ENDIF

*C200452,4 TMI [Start] Ask about 1) vendor
*C200452,4                       2) location if not found
IF EMPTY(laData[2])
  *-You have to select a xxx code.
  =gfModalGen('TRM34020B34000','DIALOG','vendor'+'|'+' ')
  m.style = ' '
  _CUROBJ = OBJNUM(m.style)
  RETURN
ENDIF
IF !llMultiWare AND EMPTY(laData[19]) AND EMPTY(laData[69])
  *-You have to select a xxx code.
  =gfModalGen('TRM34020B34000','DIALOG','ship to location or customer'+'|'+' ')
  m.style = ' '
  _CUROBJ = OBJNUM(m.style)
  RETURN
ENDIF
*C200452,4 TMI [End  ] 
lcQltyKey=laStyGrd[lnGrade,2]
SELECT STYLE
SET ORDER TO STYLE 
IF SEEK(m.Style,'Style')
  
  llAbort = .F.
  DO WHILE .T.
    *-- Validation steps for the selected style
    *--> 1- IF the style is cancelled
    *--> 2- Style should be !Make
    *--> 3- Style quality should be equal to the quality in the PO header 
    *--> 4- IF the style division is different than the PO division.
    *--> 5- IF the style group is different than the PO group.
    *--> 6- IF PO status is NOT hold then the style should have a cost sheet
    *--> 7- If not multi ship to    
     
    *--> 1- IF the style is cancelled
    IF STYLE.Status='X'
      *-This is a canceled style. Not allowed to enter here, Cannot proceed!
      =gfModalGen('TRM34040B34000','DIALOG')        
      llAbort=.T.
      EXIT
    ENDIF
    
    *--> 2- Style should be !Make
    IF STYLE.Make
      *-Only purchased styles can be selected. Cannot proceed.
      =gfModalGen('TRM34039B34000','DIALOG')        
      llAbort=.T.
      EXIT
    ENDIF
    
    *--> 3- Style quality should be equal to the quality in the PO header 
    SET ORDER TO STYQLTY IN STYLE
    IF !SEEK(lcQltyKey+ALLTRIM(m.Style),'STYLE')
      =gfModalGen('TRM46014B00000','DIALOG',laStyGrd[lnGrade,1])
      llAbort=.T.
      EXIT
    ENDIF
  
    *--> 4- IF the style division is different than the PO division.
    IF Style.cDivision <> ALLTRIM(laData[4])
      *-Conflict ! styles restricted to division XXXX, Cannot proceed!
      =gfModalGen('TRM34041B34000','DIALOG',ALLTRIM(laDiv[lnDiv,1]) )        
      llAbort=.T.
      EXIT
    ENDIF
  
    *--> 5- IF the style group is different than the PO group.
    IF !EMPTY(Style.cPurCode) AND Style.cPurCode <> ALLTRIM(laData[67])
      *-Conflict ! styles restricted to purchase XXXX, Cannot proceed!
      =gfModalGen('TRM34110B34000','DIALOG',ALLTRIM(laPurCode[lnPurCode,1]) )
      llAbort=.T.
      EXIT
    ENDIF
    
    *--> 6- IF PO status is NOT hold then the style should have a cost sheet
    IF !(laData[3]$'HB')
      IF !SEEK(SUBSTR(m.Style,1,lnMjrWid),'BOM')
        *-No cost lines found in the cost sheet, Cannot proceed!
        =gfModalGen('TRM34037B34000','DIALOG')        
        llAbort=.T.        
        EXIT
      ENDIF
    ELSE 
      IF !SEEK(SUBSTR(m.Style,1,lnMjrWid),'BOM')
        =gfModalGen('INM34038B34000','DIALOG')        
      ENDIF
    ENDIF
  
    *--> 7- If not multi ship to
    IF !llMultiWare
      *--If ship to customer the location will be the ship drop location.
      m.cWareCode = IIF(lnShpLoc = 2,laData[69],laData[19])
      lnLShpLoc   = lnShpLoc
      m.Account   = IIF(lnShpLoc = 2,laData[69],'')
      m.Store     = laData[70]
    ELSE
      *-- If multi ware house
      IF llWareHous
        m.cWareCode = IIF(lcAType$'NA' AND PADR(laData[2],6)=STYLE.CDefWare,SPACE(6),STYLE.CDefWare)
      ELSE  
        lnLShpLoc = 2
        m.cWareCode = SPACE(5)
      ENDIF
    ENDIF  
    
    EXIT
    
  ENDDO
  
  IF llAbort
    m.Style   = SPACE(lnStyleWid)
    lcStyDesc = SPACE(0)
    _CUROBJ = OBJNUM(m.Style)
    SHOW GET m.Style ENABLE
    RETURN
  ENDIF

  *-- If style is entred befor , warn the user
  IF SEEK(m.Style,(lcPoLine))
    *--This Style has entred on this order
    *--Button 02011 : \!\<OK;\?\<Cancel
    lnResp = gfModalGen('QRM00000B02011',.F.,.F.,.F.,'This style has already been entred on this order '+;
                                                    +'- do you wish to reload quantities?')
    IF lnResp = 2 
      =lfvqPoCler() 
      RETURN
    ENDIF
  ENDIF  
  
  ***** Style is Valid , now build its colors and sizes sets *****
  PRIVATE laPrices,lcStyClr
  SELECT STYLE  
  lcStyOrd = ORDER('STYLE')
  SET ORDER TO STYLE IN STYLE
  lcStyClr = SUBSTR(STYLE,1,lnMjrWid+1+lnClrLen)

  lcStat = IIF(!EMPTY(m.Style),'ENABLE','DISABLE')
  SHOW GET pbClear &lcStat    
  
  *--Get style desctiption 
  m.Desc = STYLE.DESC
  *C200452,4 TMI [Start] Refresh style description
  SHOW GET M.DESC
  *C200452,4 TMI [End  ] 
  
  *--Get Sizes  
  laSize = ''  
  =SEEK(m.Style,'STYLE')
  lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
  m.SCALE = STYLE.SCALE
  
  *-- Fields meanings
  *  -- 1st field : Scale id
  *  -- 2nd field : Count of sizes in this scale

  SELECT Scale,CNT ;
  FROM SCALE ;
  WHERE Type+Scale='S'+SUBSTR(m.SCALE,1,lnScaleLen) ;
  ORDER BY 1 ;
  INTO ARRAY laExtSz

  SELECT SCALE
  =SEEK('S'+laExtSz[1],'SCALE')
  
  lnIncrmnt = 0
  FOR lnCount = 1 TO ALEN(laExtSz,1)
    =SEEK('S'+laExtSz[lnCount,1],'SCALE')
    FOR lnJ = 1 TO laExtSz[lnCount,2]
      lcZ = STR(lnJ,1)
      DIMENSION laSize[lnIncrmnt+lnJ]
      laSize[lnIncrmnt+lnJ] = ALLTRIM(SCALE.SZ&lcZ)
    ENDFOR
    lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
  ENDFOR
  PRIVATE lnFldCnt,lnSzCnt
  lnFldCnt = ALEN(laSize)
  
  *C123847,1  TMI [Start]  
  IF ASCAN(laEvntTrig,'POTMPFIT')<>0
    =lfPOTMPFIT()
  ELSE
    *C123847,1  TMI [End  ] 

    *--Create the temp file for colors , all sizes in one line   
    IF !lfpoCrtTmp(lnFldCnt)
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not create the temp file!')
      =lfvqPoCler() 
      RETURN
    ENDIF
    
    *C123847,1  TMI [Start] 
  ENDIF
  *C123847,1  TMI [End  ] 
  
  *--Get refrence if entred befor
  IF SEEK(PADR(m.Style,lnMjrWid),lcPoLine)
    m.Reference = &lcPoLine..Reference
    m.cVenSty   = &lcPoLine..cVenSty
  ENDIF

  *--Create color array
  SELECT DISTINCT SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) FROM STYLE ;
    WHERE STYLE = PADR(m.Style,lnMjrWid) ;
    INTO ARRAY laClr  
  
  *-- Fill the file lcGrpCur that holds price groups
  =lfGetPriGp()

  llClrIgnrd = .F.  
  *C123847,1  TMI [Start] 
  IF ASCAN(laEvntTrig,'POTMPFIT')<>0
    =lfPOCOLLFT()
    RETURN
  ENDIF
  *C123847,1  TMI [End  ] 
*--Add a line for each color
  FOR lnClrs = 1 TO ALEN(laClr,1)
    
    *--If this color has no lines in BOM ignore it
    IF !(laData[3]$'HB')
      *C200452,4 TMI [Start] Fix the seek expression 
      *IF !SEEK(SUBSTR(m.Style,1,lnMjrWid)+'1'+laClr[lnClrs,1],'BOM') .OR. ;
      *   !SEEK(SUBSTR(m.Style,1,lnMjrWid)+'1'+REPL('*',lnClrLen),'BOM')
      IF !SEEK(PADR(m.Style,19)+'1'+SUBSTR(m.Style,1,lnMjrWid)+lcSepart+REPL('*',lnClrLen),'BOM')
        IF !SEEK(PADR(m.Style,19)+'1'+SUBSTR(m.Style,1,lnMjrWid)+lcSepart+laClr[lnClrs,1],'BOM')
          *C200452,4 TMI [End  ]       
          llClrIgnrd = .T.
          LOOP
          *C200452,4 TMI [Start] 
        ENDIF
        *C200452,4 TMI [End  ] 
      ENDIF
    ENDIF
    
    SELECT (lcTempCur)
    SCATTER MEMVAR BLANK
    M.STYMAJOR = PADR(m.Style,lnMjrWid)
    M.COLOR = laClr[lnClrs,1]
    M.COLORDSC = PADR(gfCodDes(PADR(laClr[lnClrs,1],lnClrLen) , 'COLOR'),20)
    lnIncrmnt = 0   
    m.OldTot = '0'+SPACE(lnFldCnt*6)
    *--Update Quantites from lcPoLine if style is entred befor for the same store
    FOR lnCount = 1 TO ALEN(laExtSz,1) 
    
      lcSeekSty = M.STYMAJOR+lcSepart+PADR(M.COLOR,lnClrLen)+lcClrSpr+laExtSz[lnCount,1]
      lcSeekSty = PADR(lcSeekSty,19)
      
      IF SEEK(lcSeekSty,(lcPoLine))
        SELECT &lcPoLine
        *-- if style is entred in more than one line then each line is a separate price group
        lcInclSz = ''
        SCAN REST WHILE STYLE+STR(LINENO,6) = lcSeekSty FOR !EMPTY(PRIGRP)
          FOR lnJ = 1 TO laExtSz[lnCount,2]
            lcZ = STR(lnJ,1)
            lcX = ALLT(STR(lnIncrmnt+lnJ))               
            IF &lcPoLine..Qty&lcZ>0              
              lcInclSz = lcInclSz + lcZ                     
              M.S&lcX = &lcPoLine..Qty&lcZ
              m.nTotal = m.nTotal + &lcPoLine..Qty&lcZ
              m.OldTot = STUFF(m.OldTot,(lnIncrmnt+lnJ)*6-5,6,"+"+STR(&lcPoLine..Qty&lcZ,5))
            ENDIF            
          ENDFOR    
        ENDSCAN
        
        SELECT (lcTempCur)
      ENDIF      
      
      *FOR lnJ = 1 TO laExtSz[lnCount,2]
      *  lcZ = STR(lnJ,1)
      *  lcX = ALLT(STR(lnIncrmnt+lnJ))        
      *--Total qty
      *  m.nTotal = m.nTotal + &lcPoLine..Qty&lcZ
      *ENDFOR

      lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
    ENDFOR     
    ******** m.OldTot = m.nTotal
    INSERT INTO (lcTempCur) FROM MEMVAR
  ENDFOR  
 
  =SEEK(PADR(m.Style,lnMjrWid),'STYLE')
  
  SELECT (lcTempCur)
  GO TOP
  lcTempTot = gfTempName()
  TOTAL ON .T. TO (gcWorkDir+lcTempTot)
  SELECT 0
  USE (gcWorkDir+lcTempTot) 
  SCATTER MEMVAR
  USE IN &lcTempTot
  ERASE (gcWorkDir+lcTempTot+'.DBF') 
  
  *--Add total line
  M.LASTLINE = CHR(255)
  M.STYMAJOR = ''
  M.COLOR    = ''
  M.COLORDSC = 'Totals'   
  INSERT INTO (lcTempCur) FROM MEMVAR
  SELECT (lcTempCur)
  GO TOP
  
  STORE M.NTOTAL TO laTot[1,2]
  
  SHOW GET pbSav DISABLE
  SHOW GET m.Style DISABLE
  
  lcRefStat = IIF(EMPTY(m.Reference),'ENABLE','DISABLE')
  SHOW GET m.Reference &lcRefStat
  SHOW GET m.cVenSty &lcRefStat  

  _CUROBJ = OBJNUM(m.Reference)

  IF llClrIgnrd
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Some colours have no lines in BOM file , they are ignored')
  ENDIF    
  *--Next/Prev buttons
  =lfPrNxPoBr()
  
  SET ORDER TO &lcStyOrd IN STYLE

ENDIF
*:***************************************************************************
*-- end of lfvQpoSty.

*:**************************************************************************
*:* Name        : lfGetPriGp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/13/2003
*:* Purpose     : Get price code from BOM file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetPriGp()
*:***************************************************************************
*C200452,1 
FUNCTION lfGetPriGp
*BOM :: CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR              

PRIVATE lcSlct,lcSeek1,lcSeek2,lcSeek3,lcSclSz,lnI,lnK,laPriGrp,lcSlctdSz,lcAlSclSz
lcSlct = SELECT()
SELECT BOM

M.STYMAJOR = PADR(m.Style,lnMjrWid)

FOR lnClrs = 1 TO ALEN(laClr,1)
  FOR lnCount = 1 TO ALEN(laExtSz,1)
    lcSeekSty = M.STYMAJOR+lcSepart+PADR(laClr[lnClrs,1],lnClrLen)+lcClrSpr+laExtSz[lnCount,1]
    lcSeekSty = PADR(lcSeekSty,19)
    
    lcSeek1 = STUFF(lcSeekSty,lnClrPos,lnClrLen,REPLICATE('*',lnClrLen))
    lcSeek2 = STUFF(lcSeek1,lnSizePos,lnSizeLen,REPLICATE('*',lnSizeLen))
    lcSeek3 = STUFF(lcSeekSty,lnSizePos,lnSizeLen,REPLICATE('*',lnSizeLen))
    
    lcAlSclSz = ''                        && All scale sizes
    FOR lnI = 1 TO laExtSz[lnCount,2]
      lcAlSclSz = lcAlSclSz + STR(lnI,1)
    ENDFOR

    DO CASE
      *-- Case Stymajor-Color-Scl
      CASE SEEK(PADR(M.STYMAJOR,19)+'1'+lcSeekSty,'BOM') 
        =lfAddGp(LBASONSIZ,lcSeekSty)
    
      *-- Case Stymajor-*****-Scl
      CASE SEEK(PADR(M.STYMAJOR,19)+'1'+lcSeek1,'BOM')
        =lfAddGp(LBASONSIZ,lcSeek1)
    
      *-- Case Stymajor-*****-***
      CASE SEEK(PADR(M.STYMAJOR,19)+'1'+lcSeek2,'BOM')        
        =lfAddGp(LBASONSIZ,lcSeek2,.T.)
        
      *-- Case Stymajor-Color-***
      CASE SEEK(PADR(M.STYMAJOR,19)+'1'+lcSeek3,'BOM')        
        =lfAddGp(LBASONSIZ,lcSeek3,.T.)
    
    ENDCASE

    IF !SEEK(lcSeekSty,lcGrpCur)
      INSERT INTO &lcGrpCur ;
             VALUES (lcSeekSty,lcAlSclSz,0)
    ELSE
      *--Add a new group for sizes with zero price  
      SELECT STYLE, SIZES , GROS_PRICE ;
      FROM &lcGrpCur ;
      WHERE STYLE = lcSeekSty ;
      INTO ARRAY laPriGrp
      
      lcSlctdSz = ''
      FOR lnK = 1 TO ALEN(laPriGrp,1)
        lcSlctdSz = lcSlctdSz + laPriGrp[lnK,2]
      ENDFOR
      lcAdSz = ''
      FOR lnI = 1 TO laExtSz[lnCount,2]
        IF !STR(lnI,1) $ lcSlctdSz
          lcAdSz = lcAdSz + STR(lnI,1) 
        ENDIF
      ENDFOR
      IF !EMPTY(lcAdSz)
        INSERT INTO &lcGrpCur ;
               VALUES (lcSeekSty,lcAdSz,0)
      ENDIF
    ENDIF
  ENDFOR
ENDFOR

SELECT (lcSlct) 
*-- end of lfGetPriGp.


*:**************************************************************************
*:* Name        : lfAddGp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/14/2003
*:* Purpose     : Add a line to price groups file lcGrpCur
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAddGp()
*:***************************************************************************
*C200452,1 
FUNCTION lfAddGp
PARAMETERS lcBsonsz , lcSeek , llSz
IF lcBsonsz
  SCAN REST WHILE CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR = PADR(M.STYMAJOR,19)+'1'+lcSeek
    lcSz = IIF(llSz,MLINE(MSIZES,ATLINE(laExtSz[lnCount,1],MSIZES)),MSIZES)
    lcSz = ALLT(STRTRAN(SUBSTR(lcSz,5),','))       
    lcSz = STRTRAN(lcSz,CHR(13))
    IF !SEEK(lcSeekSty+lcSz,lcGrpCur)
      INSERT INTO &lcGrpCur ;
             VALUES (lcSeekSty,lcSz,BOM.UNTCOST)
    ENDIF
  ENDSCAN        
ELSE
  IF !SEEK(lcSeekSty+lcAlSclSz,lcGrpCur)
    INSERT INTO &lcGrpCur ;
           VALUES (lcSeekSty,lcAlSclSz,BOM.UNTCOST)
  ENDIF
ENDIF

*-- end of lfAddGp.

*:**************************************************************************
*:* Name        : lfvQpoSav
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/09/2003
*:* Purpose     : Save function for the po quick order entry screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvQpoSav()
*:***************************************************************************
*C200452,1 
FUNCTION lfvQpoSav
*Save
PRIVATE lcOrd,lnQkCnt,lnJ,lcJ,lcQ,lnIncrmnt,lnK,lcStyOrd

lcStyOrd = ORDER('STYLE')
SET ORDER TO STYLE IN STYLE

*--Assign variables values
m.CSTYTYPE  = 'P'
m.PO        = laData[1]
m.TRANCD    = '1'
m.VENDOR    = laData[2]
m.CWARECODE = laData[19]

*C123847,1  TMI [Start] 
IF ASCAN(laEvntTrig,'POFITSAV')<>0
  =lfPOFITSAV()
  RETURN
ENDIF
*C123847,1  TMI [End  ] 

SELECT (lcTempCur)
GO TOP
SCAN FOR EMPTY(LASTLINE)
  lnIncrmnt = 0  
  FOR lnQkCnt = 1 TO ALEN(laExtSz,1)
    SELECT (lcTempCur)
    m.Style = STYMAJOR+lcSepart+SUBSTR(COLOR,1,lnClrLen)+lcClrSpr+laExtSz[lnQkCnt,1]
    m.Style = PADR(m.Style,19)
    *--KHM,TMI
    =SEEK(m.Style,'Style')

    *- Create an array that hold price groups
    DIMENSION laPriGrp[1,2]
    laPriGrp = ''
    SELECT SIZES,GROS_PRICE ;
    FROM &lcGrpCur ;
    WHERE STYLE = M.STYLE ;
    INTO ARRAY laPriGrp
    
    FOR lnSzGrps = 1 TO ALEN(laPriGrp,1)
      SELECT (lcTempCur)
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,;
                 m.nCost1,m.nCost2,m.nCost3,m.nCost4,m.nCost5,;
                 m.nECost1,m.nECost2,m.nECost3,m.nECost4,m.nECost5
      lcFlds = ''
      FOR lnJ = lnIncrmnt + 1 TO lnIncrmnt + laExtSz[lnQkCnt,2]        
        lcJ = ALLT(STR(lnJ))
        lcQ = STR(lnJ - lnIncrmnt,1)
        IF lcQ $ laPriGrp[lnSzGrps,1]
          m.TotQty = m.TotQty +  S&lcJ
          m.Qty&lcQ = S&lcJ
          lcFlds = lcFlds + 'QTY'+lcQ+','
        ENDIF
      ENDFOR    
      lcFlds = lcFlds + 'TOTQTY'
      
      IF m.TotQty>0
        m.PRIGRP      = laPriGrp[lnSzGrps,1]
        *C200452,4 TMI [Start] Let Gross price depends on style.CPRICECUR field
        *m.GROS_PRICE  = laPriGrp[lnSzGrps,2]        
        m.GROS_PRICE  = IIF(!llMulCurr OR laData[26]=STYLE.CPRICECUR,laPriGrp[lnSzGrps,2],0)
        *C200452,4 TMI [End  ] 
        m.nCost1      = m.GROS_PRICE 
  
        m.Scale       = STYLE.SCALE     
        m.PrePak      = Style.CbuyPrePk

        m.nCost2 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost2,0)
        m.nCost3 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost3,0)
        m.nCost4 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost4,0)
        m.nCost5 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost5,0)
      
        m.nECost1= lfvEquCost('1',m.nCost1,laData[28],lnCurrUnt1)
        m.nECost2= lfvEquCost('2',m.nCost2,laData[29],lnCurrUnt2)
        m.nECost3= lfvEquCost('3',m.nCost3,laData[29],lnCurrUnt2)
        m.nECost4= lfvEquCost('4',m.nCost4,laData[29],lnCurrUnt2)
        m.nECost5= lfvEquCost('5',m.nCost5,laData[29],lnCurrUnt2)

        SELECT &lcPoLine
        LOCATE FOR STYLE+PRIGRP = M.STYLE+m.PRIGRP
        IF !FOUND()
          lnLines  = lnLines + 1
          m.LineNo = lnLines        
          INSERT INTO (lcPoLine) FROM MEMVAR
          laData[36] = laData[36] + m.TotQty
          laData[40] = laData[40] + m.TotQty          
        ELSE
          *--Update laData array       
          m.nCost1      = m.GROS_PRICE*( 1 - &lcPoLine..DISC_PCNT/100)
          laData[36] = laData[36] + m.TotQty - &lcPoLine..TOTQTY    
          laData[40] = laData[40] + m.TotQty - &lcPoLine..TOTQTY             
          GATHER FIELDS &lcFlds MEMVAR
        ENDIF
      ENDIF
    ENDFOR      
    lnIncrmnt = lnIncrmnt + laExtSz[lnQkCnt,2]
  ENDFOR
ENDSCAN

*-- Clear screen
=lfvqpoCler(.T.)

SET ORDER TO &lcStyOrd IN STYLE
*-- end of lfvQpoSav.

*:**************************************************************************
*:* Name        : lfPOFITSAV
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/07/2005
*:* Purpose     : Insert lines in lcPoline from the temp file 'lcTempCur ' for DIR03
*:***************************************************************************
FUNCTION lfPOFITSAV
PRIVATE lnSzCnt,lcSzCnt,lnI,lcI

lcPoTag = 'A'+SUBSTR(lcPoLine,2)
SELECT &lcPoLine
INDEX ON STYLE+STR(GROS_PRICE,12,2) TO &lcPoTag

SELECT (lcTempCur)
SET FILTER TO 
GO TOP
SCAN FOR EMPTY(LASTLINE)
  FOR lnSzCnt = 1 TO 10 
    SELECT (lcTempCur)
    lcSzCnt = ALLTRIM(STR(lnSzCnt))
    lcI = SUBSTR(SCALE&lcSzCnt,4,1)
    
    IF !EMPTY(S&lcSzCnt)
    
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,;
                   m.nCost1,m.nCost2,m.nCost3,m.nCost4,m.nCost5,;
                   m.nECost1,m.nECost2,m.nECost3,m.nECost4,m.nECost5

      m.Style = PADR(STYMAJOR+lcSepart+SUBSTR(COLOR,1,lnClrLen)+lcClrSpr+LEFT(SCALE&lcSzCnt,3),19)
      =SEEK(m.Style,'Style')
      m.Qty&lcI = &lcTempCur..S&lcSzCnt
      m.GROS_PRICE  = IIF(!llMulCurr OR laData[26]=STYLE.CPRICECUR,&lcTempCur..PRIC&lcSzCnt,0)
      m.DISC_PCNT   = &lcTempCur..DISCNT&lcI
      m.Pirce       = m.GROS_PRICE*( 1 - m.DISC_PCNT/100)
      m.nCost1      = m.GROS_PRICE*( 1 - m.DISC_PCNT/100)
      
      *- Let Gross price depends on style.CPRICECUR field
      m.Scale       = STYLE.SCALE     
      m.PrePak      = Style.CbuyPrePk

      m.nCost2 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost2,0)
      m.nCost3 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost3,0)
      m.nCost4 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost4,0)
      m.nCost5 = IIF(!llMulCurr OR STYLE.cDutyCur =laData[27],Style.nICost5,0)
      
      m.nECost1= lfvEquCost('1',m.nCost1,laData[28],lnCurrUnt1)
      m.nECost2= lfvEquCost('2',m.nCost2,laData[29],lnCurrUnt2)
      m.nECost3= lfvEquCost('3',m.nCost3,laData[29],lnCurrUnt2)
      m.nECost4= lfvEquCost('4',m.nCost4,laData[29],lnCurrUnt2)
      m.nECost5= lfvEquCost('5',m.nCost5,laData[29],lnCurrUnt2)

      IF !SEEK(m.Style+STR(PRIC&lcSzCnt,12,2),lcPoLine)
        lnLines  = lnLines + 1
        m.LineNo = lnLines        
        m.TotQty = m.Qty&lcI
        
        INSERT INTO (lcPoLine) FROM MEMVAR
        
        laData[36] = laData[36] + m.Qty&lcI
        laData[40] = laData[40] + m.Qty&lcI         
        
      ELSE
        
        SELECT &lcPoLine
        *B129283,1 NNA 09/21/2005 (Start) Move the next line to after the accumulating line to get 
        *B129283,1 NNA            the correct booked and open qtys. 
        *REPLACE Qty&lcI WITH m.Qty&lcI ;
                TOTQTY  WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
        *B129283,1 NNA (End)

        laData[36] = laData[36] + m.Qty&lcI - &lcPoLine..QTY&lcI
        laData[40] = laData[40] + m.Qty&lcI - &lcPoLine..QTY&lcI

        *B129283,1 NNA 09/21/2005 (Start) 
        REPLACE Qty&lcI WITH m.Qty&lcI ;
                TOTQTY  WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
        *B129283,1 NNA (End)

      ENDIF

    ENDIF
    
  ENDFOR
  
ENDSCAN

*-- Clear screen
=lfvqpoCler(.T.)

SET ORDER TO &lcStyOrd IN STYLE
SET ORDER TO &lcPoLine IN &lcPoLine

*-- end of lfPOFITSAV.

*:**************************************************************************
*:* Name        : lfGtPriGrp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/12/2003
*:* Purpose     : Fill the array laPriGrp
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGtPriGrp()
*:***************************************************************************
*C200452,1 
FUNCTION lfGtPriGrp
PARAMETERS lnScl,lcGrps
PRIVATE lnSclPos,lnCnt,lnI,lnScl,lcGrps
lnSclPos = AT(lnScl+';',lcGrps)
lnCnt = VAL(SUBSTR(lcGrps,lnSclPos+4,1))

DIMENSION laPriGrp[lnCnt,2]
FOR lnI = 1 TO lnCnt
  lnSclPos = AT(lnScl,lcGrps,lnI)    
  laPriGrp[lnI,1] = SUBSTR(lcGrps,lnSclPos+4,16)
  laPriGrp[lnI,2] = VAL(SUBSTR(lcGrps,lnSclPos+21,11))
ENDFOR

*-- end of lfGtPriGrp.

*:**************************************************************************
*:* Name        : lfvqPoGPri
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/09/2003
*:* Purpose     : Gross 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqPoGPri()
*:***************************************************************************
*C200452,1 
FUNCTION lfvqPoGPri
RETURN .T.
*-- end of lfvqPoGPri.


*:**************************************************************************
*:* Name        : lfPoW
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/15/2003
*:* Purpose     : When fn. for Po quick order entry screen fields
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPoW()
*:***************************************************************************
*C200452,1 
FUNCTION lfPoW
PARAMETERS lnIndex
PRIVATE laGrpArr,lcSty,lnSz,lcSz,lnCount

SELECT (lcTempCur)
IF !EMPTY(LASTLINE)
  RETURN .F.
ENDIF

*C123847,1  TMI [Start] 
IF ASCAN(laEvntTrig,'POTMPFIT')<>0
  PRIVATE lcI
  lcI = ALLTRIM(STR(lnIndex))
  IF EMPTY(SIZE&lcI)
    RETURN .F.
  ENDIF
  lcOldVal = S&lcI
  
  lnGPrice = PRIC&lcI
  lnDiscPcnt = 0
  lnCost1 = lnGPrice
  
ELSE

  *C123847,1  TMI [End  ] 

  *-- Get scale Position 
  lnSclPos = lfSclPos(lnIndex) 
  
  *-- Get price , discount from lcGrpCur alias
  m.AddSty = STYMAJOR+lcSepart+SUBSTR(COLOR,1,lnClrLen)
  lcSty = PADR(m.AddSty+lcClrSpr+laExtSz[lnSclPos,1],19)
    
  *--Get the position of the selected size in its scale
  lnSz = lnIndex
  lnPrevSz = 0
  FOR lnCount = 1 TO lnSclPos - 1
    lnSz = lnSz - laExtSz[lnCount,2]
    lnPrevSz = lnPrevSz + laExtSz[lnCount,2]
  ENDFOR
  lcSz = STR(lnSz,1)
    
  SELECT SIZES , GROS_PRICE ;
  FROM &lcGrpCur ;
  WHERE STYLE = lcSty AND lcSz $ SIZES;
  INTO ARRAY laGrpArr  
      
  lnGPrice = laGrpArr[2]
  lnDiscPcnt = 0
  lnCost1 = lnGPrice
  
  *--Get the total of the sizes of this group 
  lnLinTotQ = 0
  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    IF lcCount $ laGrpArr[1]
      lcCurSz = ALLT(STR(lnPrevSz+lnCount))
      lnLinTotQ = lnLinTotQ + S&lcCurSz
    ENDIF
  ENDFOR  
  
  *-- If style is entred in lcPoLine , get gross price and discount from it
  SELECT &lcPoLine
  LOCATE FOR STYLE = lcSty AND lcSz $ PRIGRP
  IF FOUND()
    lnGPrice = &lcPoLine..GROS_PRICE
    lnDiscPcnt = &lcPoLine..DISC_PCNT 
    lnCost1 = lnGPrice*(1-lnDiscPcnt/100)
  ENDIF
  
  *C123847,1  TMI [Start] 
ENDIF
*C123847,1  TMI [End  ] 

SHOW GET lnGPrice
SHOW GET lnDiscPcnt
SHOW GET lnCost1
SHOW GET lnLinTotQ
=lfRefresh(lcQkWin1)
  
SELECT (lcTempCur)
  
*-- end of lfPoW.

*:**************************************************************************
*:* Name        : lfvPoBrFld
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/09/2003
*:* Purpose     : Valid function for PO quick ordline entry screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvPoBrFld()
*:***************************************************************************
*C200452,1 
FUNCTION lfvPoBrFld
PARAMETERS lnIndex
PRIVATE lnFPos
lcIndex = ALLT(STR(lnIndex))

*C123847,1  TMI [Start] 
IF ASCAN(laEvntTrig,'POVLDFIT')<>0
  =lfPOVLDFIT()
  RETURN
ENDIF
*C123847,1  TMI [End  ] 

PRIVATE lnLineTot,lnCount,lnRecno,m.AddSty,lnIncrm

SELECT (lcTempCur)
IF laOldVal[lnIndex+ln1stSz] #  S&lcIndex
  *-- Prevent update totals in total field
  IF !EMPTY(LASTLINE)
    REPLACE S&lcIndex WITH laOldVal[lnIndex+5]
    RETURN
  ENDIF

  IF S&lcIndex < 0
    *--Can not accept negative values
    =gfModalGen('TRM42000B40011','DIALOG')
    REPLACE S&lcIndex WITH laOldVal[lnIndex+5]
    RETURN 
  ENDIF
  IF S&lcIndex > 99999
    *--Can not exceed 999
    *-- 40171 : ð cannot exceeds ð  
    =gfModalGen('TRM40171B00000','DIALOG','Size Quantity|99999.')
    REPLACE S&lcIndex WITH laOldVal[lnIndex+5]
    RETURN
  ENDIF

  *-- Get scale Position 
  lnSclPos = lfSclPos(lnIndex) 

  m.AddSty = STYMAJOR+lcSepart+SUBSTR(COLOR,1,lnClrLen)
  SELECT (lcTempCur)
  IF llWareHous
    *-- Assign a style to a location if not Assigned befor
    IF !SEEK(PADR(m.AddSty+lcClrSpr+laExtSz[lnSclPos,1],19)+laData[19]+SPACE(10),'StyDye')
      *E300408,1 Message : 40012
      *E300408,1 Style/color xxx is not assigned to warehouse xxx
      *E300408,1 Button : 40002
      *E300408,1 Add Reenter
      IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.AddSty+lcClrSpr+laExtSz[lnSclPos,1])+'|'+TRIM(laData[19]))=1
        DO gpAdStyWar WITH PADR(m.AddSty+lcClrSpr+laExtSz[lnSclPos,1],19),SPACE(10),laData[19]
      ELSE      
        REPLACE S&lcIndex WITH 0
        KEYBOARD '{LEFTARROW}'
        SCATT MEMVAR TO laOldVal      
        RETURN
      ENDIF
    ENDIF

  ENDIF  
  
  *--Sum total of the current line
  lnLineTot = 0
  *--Line Total
  FOR lnCount = 1 TO ALEN(laSize)
    lcCount = ALLTRIM(STR(lnCount))
    lnLineTot = lnLineTot + S&lcCount
  ENDFOR
  REPLACE NTOTAL WITH lnLineTot
  
  *-- Update Save Status button
  llEdit = ( NTOTAL<>EVAL(OLDTOT) ) .OR. ( S&lcIndex <> VAL(SUBSTR(OldTot,lnIndex*6-5+1,5)) ) 
  
  *-- current Column and total columns sum 
  SELECT SUM(S&lcIndex),SUM(NTOTAL) FROM (lcTempCur) WHERE EMPTY(LASTLINE) INTO ARRAY laTot
  lnRecno = RECNO()
  GO BOTTOM
  *-- Update last line for these two columns
  REPLACE S&lcIndex WITH laTot[1,1],;
          NTOTAL    WITH laTot[1,2]
  IF BETWEEN(lnRecno,1,RECCOUNT()-1)
    GOTO (lnRecno)
  ELSE
    GO TOP
  ENDIF  

  lcSavStat = IIF(llEdit ,'ENABLE','DISABLE')
  SHOW GET pbSav &lcSavStat
  SCATT MEMVAR TO laOldVal
  
ENDIF  
*-- end of lfvPoBrFld


*:**************************************************************************
*:* Name        : lfPOVLDFIT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/07/2005
*:* Purpose     : Valid funtion for browse for DIR03
*:***************************************************************************
FUNCTION lfPOVLDFIT
PRIVATE lnLineTot,lnCount,lnRecno,m.AddSty,lnIncrm

SELECT (lcTempCur)
IF lcOldVal # S&lcIndex 
  *-- Prevent update totals in total field
  IF !EMPTY(LASTLINE)
    REPLACE S&lcIndex WITH lcOldVal
    RETURN
  ENDIF

  IF S&lcIndex < 0
    *--Can not accept negative values
    =gfModalGen('TRM42000B40011','DIALOG')
    REPLACE S&lcIndex WITH lcOldVal
    RETURN 
  ENDIF
  
  IF S&lcIndex > 99999
    *--Can not exceed 999
    *-- 40171 : ð cannot exceeds ð  
    =gfModalGen('TRM40171B00000','DIALOG','Size Quantity|99999.')
    REPLACE S&lcIndex WITH lcOldVal
    RETURN
  ENDIF
  
  *-- Update Save Status button
  SHOW GET pbSav ENABLE
  llEdit = .T.

  *--Sum total of the current line
  *B129283,1 NNA 09/21/2005 (Start) Remove the Duplicated Size in S8
  *REPLACE NTOTAL WITH S1+S2+S3+S4+S5+S6+S7+S8+S8+S9+S10
  REPLACE NTOTAL WITH S1+S2+S3+S4+S5+S6+S7+S8+S9+S10
  *B129283,1 NNA (End)


  *-- current Column and total columns sum 
  PRIVATE lnFLD,lnTOT
  STORE 0 TO lnFLD,lnTOT
  SUM S&lcIndex,NTOTAL TO lnFLD,lnTOT FOR EMPTY(LASTLINE)
  lnRecno = RECNO()
  GO BOTTOM
  *-- Update last line for these two columns
  REPLACE S&lcIndex WITH lnFld,;
          NTOTAL    WITH lnTOT
  IF BETWEEN(lnRecno,1,RECCOUNT()-1)
    GOTO (lnRecno)
  ELSE
    GO TOP
  ENDIF  
ENDIF  

*-- end of lfPOVLDFIT.

*:**************************************************************************
*:* Name        : lfpoGoTop 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/09/2003
*:* Purpose     : Go top
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfpoGoTop ()
*:***************************************************************************
*C200452,1 
FUNCTION lfpoGoTop 
SELECT (lcTempCur)
GO TOP
=lfwPoBrUp()
*-- end of lfpoGoTop .

*:**************************************************************************
*:* Name        : lfpoGoBot
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/09/2003
*:* Purpose     : Go bottom 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfpoGoBot()
*:***************************************************************************
*C200452,1 
FUNCTION lfpoGoBot
SELECT (lcTempCur)
GO BOTTOM
=lfwPoBrUp()
*-- end of lfpoGoBot.



*:**************************************************************************
*:* Name        : lfADDGPFLD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/14/2003
*:* Purpose     : Add the field 'PRIGRP' to lcPoLine for David Luke
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfADDGPFLD()
*:***************************************************************************
*C200452,1 
FUNCTION lfADDGPFLD

DIMENSION laStrufile[ALEN(laStrufile,1)+1,4]
laStrufile[ALEN(laStrufile,1),1] = 'PRIGRP'
laStrufile[ALEN(laStrufile,1),2] = 'C'
laStrufile[ALEN(laStrufile,1),3] = 8
laStrufile[ALEN(laStrufile,1),4] = 0

*-- end of lfADDGPFLD.


*:**************************************************************************
*:* Name        : lpBktab
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/14/2003
*:* Purpose     : Backtab function
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpBktab()
*:***************************************************************************
*C200452,1 
FUNCTION lpBktab
IF RECCOUNT(lcTempCur) = 0
  DO CASE
    CASE WONTOP() = lcDet_Ttl
      ACTIVATE WINDOW (lcQkWin1)
    CASE WONTOP() = lcQkWin1
      ACTIVATE WINDOW (lcQkWin3)             
    CASE WONTOP() = lcQkWin3 
      ACTIVATE WINDOW (lcDet_Ttl)
  ENDCASE
ELSE
  IF WONTOP() = lcDet_Ttl
    ACTIVATE WINDOW (lcQkWin1)
  ELSE
    DO CASE
      CASE WONTOP() = lcQkWin1
        IF _CUROBJ = OBJNUM(m.Reference)
          ACTIVATE WINDOW (lcQkWin3)
          _CUROBJ = OBJNUM(pbExit)
        ELSE
          _CUROBJ=_CUROBJ - 1
        ENDIF
        
      CASE WONTOP() = lcQkWin3
        IF laTot[1,2]=0
          IF _CUROBJ = OBJNUM(pbClear)
            ACTIVATE WINDOW (lcDet_Ttl)
          ELSE
            _CUROBJ = OBJNUM(pbSav)
          ENDIF
        ELSE
          IF _CUROBJ = OBJNUM(pbSav)
            ACTIVATE WINDOW (lcDet_Ttl)
          ELSE
            _CUROBJ=_CUROBJ - 1
          ENDIF
        ENDIF
    ENDCASE
  ENDIF
ENDIF  
*-- end of lpBktab.



*:**************************************************************************
*:* Name        : lfpoQkClos
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/15/2003
*:* Purpose     : Close funtion for the PO quick order entry screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfpoQkClos()
*:***************************************************************************
*C200452,1 
FUNCTION lfpoQkClos
PRIVATE llClose
llClose = .T.
IF llEdit 
  *M38093   ð. Are you sure you want to proceed with closing ð?  
  *B32005   \<Proceed;\?\<Cancel
  llClose = (gfModalGen('QRM38093B32005','ALERT','Quantities have been entered or changed';
                                                +'|The Quick Order Entry screen')=1)
ENDIF
IF llClose
  IF USED(lcTempCur)
    USE IN &lcTempCur
  ENDIF
  IF USED(lcGrpCur)
    USE IN &lcGrpCur
  ENDIF  
  ERASE (gcWorkDir+lcTempCur+'.*')
  ERASE (gcWorkDir+lcGrpCur+'.*')
  
  *-- Enable recalculation in summary folder
  llRecalTot = .T.
  
  CLEAR READ
ELSE
  ACTIVATE WINDOW (lcDet_Ttl)  
ENDIF

*-- end of lfpoQkClos.

*:**************************************************************************
*:* Name        : lfSOGen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/15/2003
*:* Purpose     : If the po order is generated from a sales order , do not accept any style
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSOGen()
*:***************************************************************************
*C200452,1 TMI 
FUNCTION lfSOGen
PRIVATE laCtktno
*--If this PO is generated from SO then prevent the screen from appear
_TALLY = 0
SELECT CTKTNO FROM CUTPICK WHERE TRANCD+CTKTNO = '2'+laData[1] INTO ARRAY laCtktno
RETURN _TALLY>0

*-- end of lfSOGen.
*:**************************************************************************
*:* Name        : lfUpdCtChg
*:* Developer   : Khalid Mohi El-Din Mohamed KHM
*:* Date        : 03/24/2003
*:* Purpose     : To zero the nCustCharg field when removing the charages.
*:***************************************************************************
*:* Example     :  = lfUpdCtChg()
*:***************************************************************************
*: B607077,1 KHM 03/24/2003
*:***************************************************************************
FUNCTION lfUpdCtChg
PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT (lcInvHdr)

REPLACE nCustCharg WITH 0

SELECT(lnAlias )

*- C200520,1 [Begin]
*:*************************************************************
*: Name      : lfvFirst
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 03/25/2003
*: Purpose   : Function to go to top of the file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvFirst ()
*:*************************************************************
*:C200520,1
FUNCTION lfvFirst

PRIVATE lnPrvAls
lnPrvAls = SELECT (0)

SELECT (lcTempSizs)
LOCATE

FOR I = 1 TO 10
  lcSize  = ALLTRIM(STR(I))
  lcsZ&lcSize = &lcTempSizs..cSize&lcSize
  *-- Price , Comm
  lnPrc&lcSize  = &lcTempSizs..nPrice&lcSize
  lnComm&lcSize = &lcTempSizs..nComm&lcSize

  SHOW GET lnPrc&lcSize
  SHOW GET lnComm&lcSize

ENDFOR

SHOW GET PbFirst   DISABLE
SHOW GET pbPrevius DISABLE
SHOW GET pbNext ENABLE
SHOW GET PbLast ENABLE
=lfRefresh('icprcls')
SELECT (lnPrvAls)

*-- End OF lfvFirst
*:*************************************************************
*: Name      : lfvPrevius
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 03/25/2003
*: Purpose   : Function to go to Previus record in the file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvPrevius ()
*:*************************************************************
*: C200520,1
FUNCTION lfvPrevius
PRIVATE lnPrvAls
lnPrvAls = SELECT (0)

SELECT (lcTempSizs)
SKIP - 1
IF BOF()
  SKIP
  SHOW GET PbFirst   DISABLE
  SHOW GET pbPrevius DISABLE
ELSE
  FOR I = 1 TO 10
    lcSize  = ALLTRIM(STR(I))
    lcsZ&lcSize = &lcTempSizs..cSize&lcSize
    *-- Price , Comm
    lnPrc&lcSize  = &lcTempSizs..nPrice&lcSize
    lnComm&lcSize = &lcTempSizs..nComm&lcSize

    SHOW GET lnPrc&lcSize
    SHOW GET lnComm&lcSize
    
  ENDFOR
  =lfRefresh('icprcls')
  IF  ALLTRIM(&lcTempSizs..cSequnc) = '1'
    SHOW GET PbFirst   DISABLE
    SHOW GET pbPrevius DISABLE
  ENDIF
ENDIF

IF ALLTRIM(lcSizSeq) # '1'
  SHOW GET pbNext ENABLE
  SHOW GET PbLast ENABLE
ENDIF

SELECT (lnPrvAls)

*-- End OF lfvPrevius
*:*************************************************************
*: Name      : lfvnext
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 03/25/2003
*: Purpose   : Function to go to next record in the file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvnext()
*:*************************************************************
*: C200520,1
FUNCTION lfvnext

PRIVATE lnPrvAls
lnPrvAls = SELECT (0)

SELECT (lcTempSizs)
SKIP
IF EOF()
  SKIP - 1
  SHOW GET pbNext DISABLE
  SHOW GET PbLast DISABLE
ELSE
  FOR I = 1 TO 10
    lcSize  = ALLTRIM(STR(I))
    lcsZ&lcSize = &lcTempSizs..cSize&lcSize
  
    *-- Price , Comm
    lnPrc&lcSize  = &lcTempSizs..nPrice&lcSize
    lnComm&lcSize = &lcTempSizs..nComm&lcSize
  
    SHOW GET lnPrc&lcSize
    SHOW GET lnComm&lcSize

  ENDFOR
  =lfRefresh('icprcls')
  IF  ALLTRIM(lcSizSeq) = ALLTRIM(&lcTempSizs..cSequnc)
    SHOW GET pbNext DISABLE
    SHOW GET PbLast DISABLE
  ENDIF
  SHOW GET PbFirst   ENABLE
  SHOW GET pbPrevius ENABLE

ENDIF


SELECT (lnPrvAls)

*-- End OF lfvnext
*:*************************************************************
*: Name      : lfvLast
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 03/25/2003
*: Purpose   : Function to go to next record in the file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvLast ()
*:*************************************************************
*: C200520,1
FUNCTION lfvLast
PRIVATE lnPrvAls
lnPrvAls = SELECT (0)

SELECT (lcTempSizs)
GO BOTTOM
FOR I = 1 TO 10
  lcSize  = ALLTRIM(STR(I))
  lcsZ&lcSize = &lcTempSizs..cSize&lcSize
  *-- Price , Comm
  lnPrc&lcSize  = &lcTempSizs..nPrice&lcSize
  lnComm&lcSize = &lcTempSizs..nComm&lcSize
  
  SHOW GET lnPrc&lcSize
  SHOW GET lnComm&lcSize
ENDFOR

SHOW GET pbNext    DISABLE
SHOW GET PbLast    DISABLE
SHOW GET PbFirst   ENABLE
SHOW GET pbPrevius ENABLE

=lfRefresh('icprcls')

SELECT (lnPrvAls)

*-- End Of lfvLast
*:*************************************************************
*: Name      : lfvPrice
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 03/25/2003
*: Purpose   : Function to Save the Price.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvPrice ()
*:*************************************************************
*: C200520,1
FUNCTION  lfvPrice
PARAMETER lcParam
PRIVATE lnOldAls

lnOldAls = SELECT (0)
IF Type('lcParam') # 'C'
  RETURN
ENDIF


SELECT (lcTempSizs)
REPLACE nPrice&lcParam WITH lnPrc&lcParam


SELECT(lnOldAls)

*-- End Of lfvPrice

*:*************************************************************
*: Name      : lfvComm
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 03/25/2003
*: Purpose   : Function to Save the Comm.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvPrice ()
*:*************************************************************
*: C200520,1
FUNCTION  lfvComm
PARAMETER lcParam
PRIVATE lnOldAls

lnOldAls = SELECT (0)
IF Type('lcParam') # 'C'
  RETURN
ENDIF


SELECT (lcTempSizs)
REPLACE nComm&lcParam WITH lnComm&lcParam


SELECT(lnOldAls)

*-- End Of lfvComm

*- C200520,1 [End]
*:*************************************************************
*: Name      : lfCollect
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 03/25/2003
*: Purpose   : Function to collect the data
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfCollect ()
*:*************************************************************
*: C200520,1
FUNCTION lfCollect
PRIVATE lnMaxInd , lnPrvAlias , lcCountNo , lnStyComm , lnGrssPric

lnPrvAlias = SELECT (0)
SELECT (lcTempCur)
lcOldOrder = ORDER()
SET ORDER TO lcTempCur1

SELECT DISTINCT SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) FROM STYLE ;
WHERE STYLE = PADR(m.Style,lnMajorLen) ;
INTO ARRAY laClr  

=SEEK(M.Style,'STYLE')

*B132317,1  TMI [Start] use price level based on customer price level setup in customr screen , if at qty level use priceA
lcPrLvl = IIF(CUSTOMER.PRICELVL='Q','A',CUSTOMER.PRICELVL)
lcPrLvl = IIF( !(lcPrLvl $ "ABCQ") , "A" , lcPrLvl )  && TMI 08/08/2006
*B132317,1  TMI [End  ] 

*-- Get all color for the current style.
FOR lnClrs = 1 TO ALEN(laClr,1)
  *-- Add field to hold the total.

  M.SCLPRC = SPACE(23*ALEN(laExtSz,1))
  SELECT (lcTempCur)
  SCATTER MEMVAR BLANK
  M.STYMAJOR = PADR(m.Style,lnMajorLen)
  M.COLOR = laClr[lnClrs]
  *C200551,3  TMI [Start] if color is not active ( hold or cancelled ) then skip it
  IF SEEK(M.STYMAJOR+lcSepart+PADR(M.COLOR,lnClrLen),'STYLE') .AND. STYLE.STATUS <> 'A'
    LOOP
  ENDIF
  *C200551,3  TMI [End  ] 
  M.COLORDSC = PADR(gfCodDes(PADR(laClr[lnClrs],lnClrLen) , 'COLOR'),20)
  lnIncrmnt = 0
  WAIT WINDOW 'Please Wait ... Collecting data for Color# : ' + M.COLORDSC NOWAIT
  *--Update Quantites from lcOrdLine if style is entred befor for the same store
  STORE 1 TO lnSequNo , lnSizeNo
  m.cSequnc = ALLTRIM(STR(lnSequNo))

  *--Add total line
  IF !SEEK(M.cSequnc + 'Totals',lcTempCur)
    lnPrvAlis = SELECT (0)
    SELECT (lcTempCur)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE LASTLINE WITH CHR(255) ,;
            STYMAJOR WITH  ''      ,;
            COLOR    WITH  ''      ,;
            COLORDSC WITH  'Totals',;
            SCLPRC   WITH ''
    SELECT (lnPrvAlis)
  ENDIF
  
  APPEND BLANK
  GATHER MEMVAR MEMO

  *-- Get all sizes for current style.
  SELECT SCALE
  =SEEK('S'+LEFT(Style.Scale,lnScaleLen))
  SCAN REST WHILE type+scale+prepak = 'S' + LEFT(Style.Scale,lnScaleLen)
    *-- get the sales order once for the current scale.
    lcSeekSty = M.STYMAJOR+lcSepart+PADR(M.COLOR,lnClrLen)+lcClrSpr+Scale.Scale
    lcSeekSty = PADR(lcSeekSty,19)
    
    *-- Fill the fields PhyStk and FreStk
    =SEEK(lcSeekSty+laWareHouse[lnWareHouse,2],'STYDYE')
    PRIVATE laSoldQty
    DIMENSION laSoldQty[8]
    laSoldQty = 0
    ldNxM1stDy = lfNxM1stDy()            && Get Next month first day
   
    *C200520,1 ABD - Remark the next few line, no need to callculate the 
    *C200520,1 ABD - the total sals order , we will get the alocation Qty
    *C200520,1 ABD - from the Stydye file in staed of sales order file. [Begin]
    *SELECT ORDLINE
    *SEEK lcSeekSty
    *SCAN REST WHILE STYLE = lcSeekSty ;
    *          FOR ORDLINE.CWARECODE = laWareHouse[lnWareHouse,2] AND ORDLINE.START<ldNxM1stDy
    *  IF SEEK(ORDLINE.CORDTYPE+ORDLINE.ORDER,'ORDHDR') AND ORDHDR.STATUS $ 'OH'
    *    laSoldQty[1] = laSoldQty[1] + ORDLINE.QTY1
    *    laSoldQty[2] = laSoldQty[2] + ORDLINE.QTY2
    *    laSoldQty[3] = laSoldQty[3] + ORDLINE.QTY3
    *    laSoldQty[4] = laSoldQty[4] + ORDLINE.QTY4
    *    laSoldQty[5] = laSoldQty[5] + ORDLINE.QTY5
    *    laSoldQty[6] = laSoldQty[6] + ORDLINE.QTY6
    *    laSoldQty[7] = laSoldQty[7] + ORDLINE.QTY7
    *    laSoldQty[8] = laSoldQty[8] + ORDLINE.QTY8
    *  ENDIF
    *ENDSCAN
    *C200520,1 ABD - [End]

    STORE 0 To lnStyComm , lnGrssPric
    *-- Get the price from CSTPRICE.dbf or from style.dbf file
    =SEEK(lcSeekSty,'STYLE')
    
    *B119914,1 ABD - The quick order entry screen is picking up the style 
    *B119914,1 ABD - Standard price not the defined price which  is 
    *B119914,1 ABD - Defined in  customer price. [Begin]
    lcPRICCODE = ''
    *B119914,1 ABD - [End]
    
    IF SEEK(lfPRICCODE()+laData[33]+lcSeekSty,'CSTPRICE')        
      lnStyComm  = IIF(CSTPRICE.COMMDV=0,IIF(Style.Commission,laData[28],0),CSTPRICE.COMMDV)
      lnGrssPric = CSTPRICE.PRICEDV
    ELSE      
      lcPri = STR(0,12,2) + STR(IIF(Style.Commission,laData[28],0),5,2) + STR(0,6,2)
      IF llMulCurr .AND. laData[33]<>gcBaseCurr 
        IF SEEK(lcSeekSty+laData[33],'STYPRICE')
          lnStyComm = IIF(Style.Commission,laData[28],0)
          *B132317,1  TMI [Start] use price level based on customer price level setup in customr screen , if at qty level use priceA
          *lnGrssPric= STYPRICE.PRICEA
          lnGrssPric = STYPRICE.PRICE&lcPrLvl 
          *B132317,1  TMI [End  ] 

        ENDIF
      ELSE     
        IF SEEK(lcSeekSty,'STYLE')
          lnStyComm  = IIF(Style.Commission,laData[28],0)
          *B132317,1  TMI [Start] use price level based on customer price level setup in customr screen , if at qty level use priceA
          *lnGrssPric = STYLE.PRICEA
          lnGrssPric  = STYLE.PRICE&lcPrLvl 
          *B132317,1  TMI [End  ] 
        ENDIF
       ENDIF
    ENDIF
    SELECT SCALE  
    
    FOR I = 1 To 8
      lcCountNo = ALLTRIM(STR(I))
      *-- get every size at the scale, if empty get out from the for loop.
      IF Empty(SZ&lcCountNo)
        EXIT
      ELSE
        IF lnSizeNo = 11
          lnSizeNo = 1
          lnSequNo = lnSequNo + 1
          m.cSequnc = ALLTRIM(STR(lnSequNo))
          SELECT (lcTempCur)
          *--Add total line
          IF !SEEK(M.cSequnc + 'Totals')
            APPEND BLANK
            GATHER MEMVAR MEMO
            REPLACE LASTLINE WITH CHR(255) ,;
                    STYMAJOR WITH  ''      ,;
                    COLOR    WITH  ''      ,;
                    COLORDSC WITH  'Totals'
          ENDIF
          
          APPEND BLANK
          GATHER MEMVAR MEMO
          SELECT SCALE
        ENDIF
        lcSizeNo = ALLTRIM(STR(lnSizeNo))
        SELECT (lcTempCur)
        REPLACE Size&lcSizeNo  WITH Scale.SZ&lcCountNo,;
                GPric&lcSizeNo WITH lnGrssPric,;
                Comm&lcSizeNo  WITH lnStyComm
        *C127807,1  TMI [Start] Update the scale to use for PAN21
        REPLACE SCL&lcSizeNo  WITH Scale.SCALE
        *C127807,1  TMI [End  ] 
        
        *-- Update the Physical & Free stock.
        SELECT (lcTempCur)
        *--Stock (free - physical)
        *C200520,1 ABD - Remark the next few line, no need to callculate the 
        *C200520,1 ABD - the total sals order , we will get the alocation Qty
        *C200520,1 ABD - from the Stydye file in staed of sales order file. [Begin]
        *REPLACE FreStk&lcSizeNo WITH STYDYE.Stk&lcCountNo - laSoldQty[I],;
        *        PhyStk&lcSizeNo WITH STYDYE.Stk&lcCountNo               ,;
        *        TotFreStk       WITH TotFreStk + FreStk&lcSizeNo        ,;
        *        TotPhyStk       WITH TotPhyStk + PhyStk&lcSizeNo
        REPLACE FreStk&lcSizeNo WITH STYDYE.Stk&lcCountNo - STYDYE.ALO&lcCountNo,;
                PhyStk&lcSizeNo WITH STYDYE.Stk&lcCountNo               ,;
                TotFreStk       WITH TotFreStk + FreStk&lcSizeNo        ,;
                TotPhyStk       WITH TotPhyStk + PhyStk&lcSizeNo
        *C200520,1 ABD - [End]
                
        SELECT SCALE
        lnSizeNo = lnSizeNo + 1
      ENDIF
    ENDFOR
  ENDSCAN
ENDFOR
SELECT (lcTempCur)
SET FILTER TO (Val(cSequnc) = lnfldgrp)
SET ORDER TO &lcOldOrder

WAIT CLEAR
SELECT (lnPrvAlias)
RETURN


*-- End OF lfCollect.
*:*************************************************************
*: Name      : lpQukSave
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 03/25/2003
*: Purpose   : Procedure to save the data into temp file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lpQukSave ()
*:*************************************************************
*: C200520,1
PROCEDURE lpQukSave
Private lnPrvsAls , lnSizeSeq , lnScaleLen , lnQkCnt , lnGrprice , lnCommPrc,;
        lcOldColor , llSkip

lcOldColor = ''
lnPrvsAls = SELECT (0)
llSkip = .F.

*B607189,1 ABD - Define new variable to hold how many size for thw current scale. []
lnScalCont = 0
*B607189,1 ABD - [End]

lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
SELECT (lcTempCur)
SET FILTER TO
LOCATE
STORE 0 TO lnGrprice , lnCommPrc , lnQkCnt
DO WHILE EMPTY(LASTLINE)

  *B607224,1 ABD - Check on this condation if arraive to total lines.. [Begin]
  *-- check if u arraive to total recored don't add this record.
  IF !EMPTY(LASTLINE)
    EXIT
  ENDIF
  *B607224,1 ABD - [End]

  lnIncrmnt = 0
  lnSizeSeq = 1
  IF lnQkCnt = ALEN(laExtSz,1) + 1
    *B607189,1 ABD - Remark the next line and add one cond. to skip in case
    *B607189,1 ABD - The scale less than 10 sizes, this will not let 
    *B607189,1 ABD - The program goes into loop.[Begin]
    *IF llSkip 
    IF llSkip .OR.  lnScalCont < 10
      lnScalCont = 0
      *B607189,1 ABD - [End] 
      SELECT (lcTempCur)
      SKIP
    ENDIF
    *B607189,1 ABD - count the number of sizes for current scale if more than 10 
    *B607189,1 ABD - Assign Zero to it. [Begin]
    lnScalCont = IIF(lnScalCont <= 10,0,lnScalCont)
    *B607189,1 ABD - [End]
  ENDIF
  
  FOR lnQkCnt = 1 TO ALEN(laExtSz,1)
    SELECT (lcTempCur)
    *C200587,1  TMI [Start] Allow dealing with extended and non extended cases
    *m.Style = STYMAJOR+lcSepart+SUBSTR(COLOR,1,lnClrLen)+lcClrSpr+laExtSz[lnQkCnt,1]  
    m.Style = STYMAJOR+lcSepart+SUBSTR(COLOR,1,lnClrLen)+IIF(llExtSS,lcClrSpr+laExtSz[lnQkCnt,1] , '' )
    *C200587,1  TMI [End  ]     
    WAIT WINDOW 'Collect data for Style # : ' + M.Style +'...'    NOWAIT
    IF SEEK(m.Style,'STYLE')
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,;
                 m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8,m.TotBook,;
                 lnGrprice , lnCommPrc
      FOR lnJ = 1 To laExtSz[lnQkCnt,2]
        lcJ = ALLTRIM(STR(lnSizeSeq))
        lcQ = STR(lnJ,1)
        m.TotQty   = m.TotQty +  S&lcJ
        m.Qty&lcQ  = S&lcJ
        m.Book&lcQ = S&lcJ
        lnGrprice  = GPric&lcJ
        lnCommPrc  = Comm&lcJ
        
        lnSizeSeq  = lnSizeSeq + 1
        IF lnSizeSeq = 11
          lnSizeSeq = 1
          lcOldColor = COLOR
          SKIP
          llSkip = !(lcOldColor # Color)
        ENDIF
      ENDFOR

      *B607189,1 ABD - count the number of sizes for current scale. [Begin]
      lnScalCont = lnScalCont + laExtSz[lnQkCnt,2]
      *B607189,1 ABD - [End]

      *-- Don't add any record with totalQty =Equal zero.
      IF m.TotQty = 0
        LOOP
      ENDIF
      
      *B607224,1 ABD - Remark the next lines.  [Begin]
      *-- check if u arraive to total recored don't add this record.
      *IF !EMPTY(LASTLINE)
      *  EXIT
      *ENDIF
      *B607224,1 ABD - [End]
      
      m.TotBook  = m.TotQty
      m.Scale    = STYLE.SCALE     
      m.Desc1    = STYLE.Desc1 
      m.PrePak   = Style.PrePak
      m.Season   = Style.Season
      m.Gl_Sales = ALLTRIM(laData[53])+Style.cSlsGlLink
      m.Gl_Sales = IIF(laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')
      m.Flag     = 'N'      
      *C200587,1  TMI [Start] Update completion date for a style in ordline with Sold out date based on a new parameter      
      M.COMPLETE  = IIF( llCMP2SOLD .AND. !EMPTY(STYLE.SOLDOUT) , STYLE.SOLDOUT , laData[10] )
      *C200587,1  TMI [End  ] 

      *-- Get price for each line of scales has different prices , 
      *-- or check qty differs from entred qty        
            
      IF m.TotQty > 0
        m.Gros_Price = lnGrprice
        m.Comm1      = lnCommPrc
        m.Price      = ROUND( m.Gros_Price*(1-m.Disc_Pcnt/100) ,2 )
        lnLines    = lnLines + 1
        m.LineNo   = lnLines      
        INSERT INTO (lcOrdLine) FROM MEMVAR

        *--Update laData array 
        laData[35] = laData[35] + m.TotBook
        laData[36] = laData[36] + m.TotBook*m.Price
        laData[41] = laData[41] + m.TotQty
        laData[42] = laData[42] + m.TotQty*m.Price          
      ENDIF      
    ENDIF
  ENDFOR
ENDDO  
WAIT CLEAR
SELECT (lnPrvsAls)
*-- End Of lpQukSave
*B607215,1 ABD Remark the next lines. [Begin]
*!*	*C200496,1 ABD [Begin]
*!*	*:*************************************************************
*!*	*: Name      : lfCollhist
*!*	*: Developer : ABDOU ELGENDI -  (ABD)
*!*	*: Date      : 03/25/2003
*!*	*: Purpose   : Procedure to Collect the data from History File.
*!*	*:*************************************************************
*!*	*: Calls     : 
*!*	*:             Procedures : ....
*!*	*:             Functions  : ....
*!*	*:*************************************************************
*!*	*: Passed Parameters  : ............
*!*	*:*************************************************************
*!*	*: Returns            : ............
*!*	*:*************************************************************
*!*	*: Example   : = lfCollhist ()
*!*	*:*************************************************************
*!*	*:C200496,1 ABD 
*!*	FUNCTION lfCollhist

*!*	PRIVATE lcInstalNo , lnPrvsAlis , lcOldOrder

*!*	lnPrvsAlis = SELECT()

*!*	lcTempHist = gfTempName()
*!*	SELECT ARHIST
*!*	lcOldOrder = ORDER()
*!*	SET ORDER TO Arhistht
*!*	= AFIELDS(laFileStru)
*!*	= gfCrtTmp(lcTempHist,@laFileStru,[Account],lcTempHist)
*!*	lcTmpInve = gfTempName()

*!*	CREATE TABLE (lcTmpInve) (Tran c(6))
*!*	INDEX ON TRAN TAG (lcTmpInve)
*!*	*-- Set relation with customer file.
*!*	SELECT ARHIST
*!*	SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
*!*	STORE '' TO m.cAddress4 , m.cAddress5 , m.Region


*!*	SELECT (lcTmpTrans)
*!*	lcOldOrdr = ORDER()
*!*	INDEX ON Account+Tran TAG 'lcTmpTran1' ADDITIVE
*!*	SET ORDER TO lcTmpTran1
*!*	SELECT ARHIST
*!*	*-- Scan around Credit file for records matcjing criteria
*!*	ldStartDat = ldRpHDate - Day(ldRpHDate) + 1
*!*	SCAN FOR &lcRpExp .AND. BETWEEN(HistDate,ldStartDat,ldRpHDate)
*!*	  SCATTER MEMVAR MEMO
*!*	  INSERT INTO (lcTempHist) FROM MEMVAR 
*!*	ENDSCAN

*!*	*-- Return to old order.

*!*	SET ORDER TO &lcOldOrder
*!*	SET RELATION TO  && Rest relation.

*!*	SELECT (lcTempHist)
*!*	SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
*!*	LOCATE

*!*	SCAN
*!*	  lnAmount = 0
*!*	  m.cAddress4 = Customer.cAddress4 
*!*	  m.cAddress5 = Customer.cAddress5
*!*	  m.Region    = Customer.Region

*!*	  SCATTER MEMVAR
*!*	  *-- Don't collect any data for Void or return record.
*!*	  IF M.Trantype $ 'IR'
*!*	    LOOP
*!*	  ELSE
*!*	    IF lfVoidRcrd ()
*!*	      LOOP
*!*	    ENDIF
*!*	  ENDIF
*!*	  
*!*	  *-- calculate the recored one time for evry invoice.
*!*	  IF M.Trantype = '1'
*!*	    IF SEEK(M.tran,lcTmpInve)
*!*	      LOOP
*!*	    ELSE
*!*	      INSERT INTO (lcTmpInve) FROM MEMVAR
*!*	    ENDIF
*!*	  ENDIF

*!*	  *-- Function to get the amount.
*!*	  DO lpGetAmnt
*!*	 
*!*	  IF llMulCurr AND Amount <> 0 AND (cCurrCode <> gcBaseCurr)
*!*	    
*!*	    lnAmount = ROUND(lfAmntDisp(Amount,"O",ldRpExDate,lcRpTmpCur),2)

*!*	    *-- Checking for the occurence of this currency with "O" type
*!*	    IF !SEEK(cCurrCode,lcTmpCurrc)
*!*	      INSERT INTO (lcTmpCurrc) (cCurrCode) VALUES (&lcTempHist..cCurrCode)
*!*	    ENDIF

*!*	    m.nORate = lfAmntDisp(1,"O",ldRpExDate,lcRpTmpCur)    
*!*	  ELSE
*!*	    lnAmount = Amount
*!*	    m.nORate = 1
*!*	  ENDIF  

*!*	  lcInstalNo = ''
*!*	  DO lpGetHAmnt WITH lcInstalNo    && Calculate the amount from History file
*!*	  DO lpHstDeal   && Calculate Ages and fill memory variables (OpenCr and NetBal)
*!*	  DO lpInshstRc  && Insert New Record in Transaction Temporary File and Totals Temporary file.

*!*	ENDSCAN  && end Scan Credit records achieve both customer and Credit filters.
*!*	*-- EndScan around Credit file for records matcjing criteria

*!*	SET RELATION TO  && Rest relation.
*!*	*-- Close the temp Files.
*!*	IF USED(lcTmpInve)
*!*	  USE IN (lcTmpInve)
*!*	  ERASE (gcWorkDir+lcTmpInve+".DBF")
*!*	  ERASE (gcWorkDir+lcTmpInve+".CDX")
*!*	  ERASE (gcWorkDir+lcTmpInve+".FPT")
*!*	ENDIF
*!*	*-- Close the temp Files.
*!*	IF USED(lcTempHist)
*!*	  USE IN (lcTempHist)
*!*	  ERASE (gcWorkDir+lcTempHist+".DBF")
*!*	  ERASE (gcWorkDir+lcTempHist+".CDX")
*!*	  ERASE (gcWorkDir+lcTempHist+".FPT")
*!*	ENDIF

*!*	SELECT (lcTmpTrans)
*!*	SET ORDER TO &lcOldOrdr

*!*	SELECT (lnPrvsAlis)
*!*	*-- End OF lfCollhist
*!*	*:**************************************************************************
*!*	*: Name      : lpGetHAmnt
*!*	*: Developer : ABDOU ELGENDI -  (ABD)
*!*	*: Date      : 03/25/2003
*!*	*: Purpose   : To Calculate the amount from History file
*!*	*:**************************************************************************
*!*	*: Called from : lfCollhist.
*!*	*:**************************************************************************
*!*	*: Example   : DO lpGetHAmnt
*!*	*:**************************************************************************
*!*	*:C200496,1 ABD 
*!*	PROCEDURE lpGetHAmnt
*!*	PARAMETER lcInstalNo
*!*	PRIVATE lnOldAlias

*!*	lnOldAlias = SELECT(0)
*!*	*-- If there is Payment in AR History file
*!*	m.nHstAmount = 0
*!*	IF SEEK(m.Account + m.Tran + lcInstalNo,'ArHist') 
*!*	  *-- get all record for this account & transaction.
*!*	  SELECT ARHIST
*!*	  SCAN REST WHILE account+tran+cinstalno = m.Account + m.Tran
*!*	    m.nHstAmount = m.nHstAmount + ArHist.Amount
*!*	  ENDSCAN
*!*	ENDIF

*!*	SELECT (lnOldAlias)
*!*	*-- EndIf of Payment in AR History file

*!*	*-- End of lpGetHAmnt.
*!*	*:**************************************************************************
*!*	*: Name      : lpHstDeal
*!*	*: Developer : ABDOU ELGENDI -  (ABD)
*!*	*: Date      : 03/25/2003
*!*	*: Purpose   : To Calculate the Credit Aging
*!*	*:**************************************************************************
*!*	*: Called from : lpInsCredt
*!*	*:**************************************************************************
*!*	*: Example   : DO lpHstDeal
*!*	*:**************************************************************************
*!*	*:C200496,1 ABD 
*!*	PROCEDURE lpHstDeal

*!*	=lfUpdtHAge('C',gdSysDate - TranDate) && Update Date ages

*!*	*-- Save open credit and net balance per transaction
*!*	STORE m.Amount TO m.OpenCr , m.NetBal

*!*	STORE lnAmount TO lnOpenCr , lnNetBal

*!*	*-- End of lpHstDeal.
*!*	*:**************************************************************************
*!*	*: Name      : lfUpdtHAge
*!*	*: Developer : ABDOU ELGENDI -  (ABD)
*!*	*: Date      : 03/25/2003
*!*	*: Purpose   : Calculate the Aging 
*!*	*:**************************************************************************
*!*	*: Called from : lpHstDeal
*!*	*:**************************************************************************
*!*	*: Passed Parameters : lcUpdtTyp --> 'D' for Debit , 'C' for Credit
*!*	*:                     lnAgeDays --> No of Days left  
*!*	*:**************************************************************************
*!*	*: Example   : =lfUpdtHAge('C',gdSysDate - TranDate)
*!*	*:**************************************************************************
*!*	*:C200496,1 ABD 
*!*	FUNCTION lfUpdtHAge
*!*	PARAMETERS lcUpdtTyp , lnAgeDays

*!*	*-- Intialize variables that calculate data from transaction files. [begin]
*!*	STORE 0.00 TO m.Current , m.Age30 , m.Age60 , m.Age90 , m.Age120 , ;
*!*	              m.OpenCr , m.Totage , m.NetBal , m.Age00 ,;
*!*	              m.nGroup , m.nPostDChq

*!*	STORE 0 TO lnCurrent , lnAge30 , lnAge60 , lnAge90 , lnAge120 , ;
*!*	           lnOpenCr , lnTotAge , lnNetBal , lnAge00
*!*	*-- Intialize variables that calculates data from transaction files. [End]

*!*	*-- if no parameter passed (i.e. want to intializing only) then return...
*!*	*-- does occur in this program but it may be.
*!*	IF TYPE('lcUpdtTyp') $ 'UL'
*!*	  RETURN
*!*	ENDIF

*!*	*-- lcAges : Varaible Hold Field description (Age00, 30 , 60 , 90 , and 120)
*!*	*-- if Debit
*!*	IF lcUpdtTyp = 'D'

*!*	  *-- if Age By Date.
*!*	  IF lcAgeType = 'D'
*!*	    lcAges = 'm.Age'                                                    +;
*!*	      IIF(lnAgeDays >= 120,'120',IIF(lnAgeDays >= 90 ,'90'   ,;
*!*	      IIF(lnAgeDays >= 60 ,'60' ,IIF(lnAgeDays >= 30 ,'30','00')))) 
*!*	  ELSE  && else if Age By Terms.
*!*	    lcAges = 'm.Age'                                                      +;
*!*	      IIF(lnAgeDays >= 91,'120',IIF(lnAgeDays >= 61 ,'90' ,;
*!*	      IIF(lnAgeDays >= 31 ,'60' ,IIF(lnAgeDays >= 1 ,'30','00'))))   
*!*	  ENDIF  && end if Age By Date.

*!*	ELSE  && else if Credit
*!*	  lcAges = 'm.Age'                                                      +;
*!*	    IIF(lnAgeDays >= 120,'120',IIF(lnAgeDays >= 90 ,'90' ,;
*!*	    IIF(lnAgeDays >= 60 ,'60' ,IIF(lnAgeDays >= 30 ,'30','00')))) 
*!*	ENDIF  && end if Debit

*!*	&lcAges   = m.Amount       && Fill Age variable
*!*	m.Current = m.Age00

*!*	PRIVATE lcTotAccAg         && Fill the Accumulated Age Variable in lcTmpAcct
*!*	lcTotAccAg  = STRTRAN(lcAges,"m.","ln")
*!*	&lcTotAccAg = lnAmount
*!*	lnCurrent   = lnAge00

*!*	*-- End of lfUpdtHAge.
*!*	*:**************************************************************************
*!*	*: Name      : lpInshstRc
*!*	*: Developer : ABDOU ELGENDI -  (ABD)
*!*	*: Date      : 03/25/2003
*!*	*: Purpose   : To Calculate the Credit Aging 
*!*	*:**************************************************************************
*!*	*: Called from : lpInsDebit OR lpInsCredt
*!*	*:**************************************************************************
*!*	*: Example   : DO lpInshstRc
*!*	*:**************************************************************************
*!*	*:C200496,1 ABD 
*!*	PROCEDURE lpInshstRc

*!*	m.cGroupKey = EVALUATE(lcReplExpr)

*!*	*-- negative number is assigned to nGroup field in Transaction file 
*!*	*-- because index is sorted by nGroup so when we add the incremented 
*!*	*-- positive numbers for Group Position in Group change proc. with 
*!*	*-- SCAN/ENDSCAN the line remains in its position 
*!*	*-- AND
*!*	*-- m.nGroup must take value -1 to get the nGroup field begin with -1 not 0 
*!*	*-- to treat the case of the instalment of the invoices.
*!*	m.nGroup = -1
*!*	m.nGroup = -99999


*!*	*-- Check if this invoice is still open or not if still open update the current record
*!*	*-- that came from the Debit file.
*!*	IF SEEK(M.Account+M.Tran,lcTmpTrans)
*!*	  lnPrvAls = SELECT(0)
*!*	  IF EMPTY(&lcTmpTrans..Desc)
*!*	    M.Desc = IIF(EMPTY(TRANCODE),M.Desc,gfCodDes(M.TRANCODE,'TRANCODE'))
*!*	  ENDIF
*!*	  SELECT (lcTmpTrans)
*!*	  REPLACE nHstAmount WITH m.nHstAmount,;
*!*	          Reference  WITH IIF(M.Trantype = '1','PART PAID',Reference) ,;
*!*	          Desc       WITH M.Desc
*!*	  SELECT(lnPrvAls)
*!*	  RETURN
*!*	ELSE
*!*	  IF EMPTY(M.Desc)
*!*	    M.Desc = IIF(EMPTY(TRANCODE),M.Desc,gfCodDes(M.TRANCODE,'TRANCODE'))
*!*	  ENDIF

*!*	  INSERT INTO (lcTmpTrans) FROM MEMVAR         && Insert Transaction Record
*!*	ENDIF

*!*	*-- Don't update header file in case not Invoice.
*!*	IF M.Trantype # '1'
*!*	  RETURN
*!*	ENDIF

*!*	*-- If cGroupKey with this currency is found in Temp Acc Currency file
*!*	IF llMulCurr
*!*	  IF SEEK(&lcTmpTrans..cGroupKey+&lcTmpTrans..cCurrCode,lcTmpAcCur)
*!*	    SELECT (lcTmpAcCur)
*!*	    REPLACE Current WITH Current + &lcTmpTrans..Current ,;
*!*	            Age30   WITH Age30   + &lcTmpTrans..Age30   ,;
*!*	            Age60   WITH Age60   + &lcTmpTrans..Age60   ,;
*!*	            Age90   WITH Age90   + &lcTmpTrans..Age90   ,;
*!*	            Age120  WITH Age120  + &lcTmpTrans..Age120
*!*	  ELSE

*!*	    INSERT INTO (lcTmpAcCur) (cGroupKey , Account , cAddress4 , cAddress5 , Region ,;
*!*	                              cCurrCode , Current , Age30 , Age60 , Age90 , Age120) ;
*!*	           VALUES (&lcTmpTrans..cGroupKey , &lcTmpTrans..Account , &lcTmpTrans..cAddress4 , ;
*!*	                   &lcTmpTrans..cAddress5 , &lcTmpTrans..Region , &lcTmpTrans..cCurrCode,;
*!*	                   &lcTmpTrans..Current , &lcTmpTrans..Age30 ,&lcTmpTrans..Age60 , ;
*!*	                   &lcTmpTrans..Age90 , &lcTmpTrans..Age120)
*!*	  ENDIF
*!*	ENDIF

*!*	*-- if Find this account in Temporary totals file.
*!*	IF SEEK(m.cGroupKey,lcTmpAcct)
*!*	  SELECT (lcTmpAcct)
*!*	  REPLACE Current WITH Current + lnCurrent ,;
*!*	            Age30 WITH Age30   + lnAge30   ,;
*!*	            Age60 WITH Age60   + lnAge60   ,;
*!*	            Age90 WITH Age90   + lnAge90   ,;
*!*	           Age120 WITH Age120  + lnAge120  ,;
*!*	           TotAge WITH TotAge  + lnTotAge  ,;
*!*	           OpenCr WITH OpenCr  + lnOpenCr  ,;
*!*	           NetBal WITH NetBal  + lnNetBal

*!*	ELSE  && else if this account not found before.

*!*	  *-- Add new record in temporary total files.
*!*	  WAIT WINDOW 'Collect data for Customer ' + Account NOWAIT

*!*	  *-- Adding postDated Cheques to lcTmpAcct file in single currency comp.
*!*	  IF !llMulCurr AND SEEK(m.Account,'PostDChq')
*!*	    SELECT PostDChq
*!*	    SUM Amount REST WHILE Account = m.Account TO m.nPostDChq
*!*	  ENDIF

*!*	  INSERT INTO (lcTmpAcct) (Current, Age30, Age60, Age90, Age120, OpenCr,;
*!*	               TotAge, NetBal, cGroupKey, Account, BtName, cAddress4,;
*!*	               cAddress5, Region, nPostDchq, cFacCode) VALUES ;
*!*	              (lnCurrent, lnAge30, lnAge60, lnAge90, lnAge120, lnOpenCr,;
*!*	               lnTotAge, lnNetBal, m.cGroupKey, m.Account, Customer.BtName,;
*!*	               Customer.cAddress4, Customer.cAddress5, Customer.Region,;
*!*	               m.nPostDchq, Customer.cFacCode)
*!*	ENDIF  && End if Find this account in Temporary totals file.

*!*	*-- End of lpInshstRc.
*!*	*:**************************************************************************
*!*	*: Name      : lpInshstRc
*!*	*: Developer : ABDOU ELGENDI -  (ABD)
*!*	*: Date      : 03/25/2003
*!*	*: Purpose   : To Calculate the Credit Aging 
*!*	*:**************************************************************************
*!*	*: Called from : lpInsDebit OR lpInsCredt
*!*	*:**************************************************************************
*!*	*: Example   : DO lpInshstRc
*!*	*:**************************************************************************
*!*	*:C200496,1 ABD 
*!*	PROCEDURE lpGetAmnt

*!*	*-- seek to know if this invoice is complete paid or no.

*!*	IF !SEEK(M.Account+M.Tran,'DEBIT')
*!*	  m.Amount = 0
*!*	ENDIF

*!*	*- End OF lpGetAmnt
*!*	*:*************************************************************
*!*	*: Name      : lfVoidRcrd
*!*	*: Developer : ABDOU ELGENDI -  (ABD)
*!*	*: Date      : 03/25/2003
*!*	*: Purpose   : Function to check if this invoice is void or not.
*!*	*:*************************************************************
*!*	*: Calls     : 
*!*	*:             Procedures : ....
*!*	*:             Functions  : ....
*!*	*:*************************************************************
*!*	*: Passed Parameters  : ............
*!*	*:*************************************************************
*!*	*: Returns            : ............
*!*	*:*************************************************************
*!*	*: Example   : = lfVoidRcrd ()
*!*	*:*************************************************************
*!*	*:C200496,1 ABD 
*!*	FUNCTION lfVoidRcrd
*!*	PRIVATE llVoidRcrd , lnAOldAls

*!*	lnAOldAls = SELECT (0)
*!*	llVoidRcrd = .F.

*!*	IF SEEK(m.Account + m.Tran,'ArHist') 
*!*	  *-- get all record for this account & transaction.
*!*	  SELECT ARHIST
*!*	  SCAN REST WHILE account+tran+cinstalno = m.Account + m.Tran
*!*	    IF Trantype $ 'IR'
*!*	      llVoidRcrd = .T.
*!*	    ENDIF
*!*	  ENDSCAN
*!*	ENDIF

*!*	SELECT(lnAOldAls)

*!*	RETURN llVoidRcrd

*!*	*-- End OF lfVoidRcrd.
*!*	*C200496,1 ABD [END]
*!*	*:*************************************************************
*B607215,1 ABD [End]
*:*************************************************************
*: Name      : lfUpPikHdr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 05/25/2003
*: Purpose   : Save the piktkt date at the INVHDR.PIKDATE.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfUpPikHdr ()
*:*************************************************************
*: C200540,1
FUNCTION lfUpPikHdr
PRIVATE lnPrvAlias

lnPrvAlias = SELECT (0)
IF !EMPTY(m.Piktkt) .AND. ;
  SEEK(M.ORDER+M.STORE+M.PikTkt,'PACK_HDR') .AND. !EMPTY(PACK_HDR.DSHIPDATE)
  M.PikDate = PACK_HDR.DSHIPDATE
ENDIF

SELECT (lnPrvAlias)
*-- End OF lfUpPikHdr.
*C200540,1
*:*************************************************************

*:**************************************************************************
*:* Name        : lfSHWCSTNT                                      *C200551,1  
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/02/2003
*:* Purpose     : Show customer notes at order entry
*:***************************************************************************
*:* Called from : SOORD.PRG (   lfvAccount()    )
*:***************************************************************************
FUNCTION lfSHWCSTNT
PRIVATE lcSHWCSTNT
lcSHWCSTNT = gfGetMemVar('M_SHWCSTNT')
*- Show customer notepad if option set in SO setup to Always or Customer Dependent
*-  if "CUSTOMER.LSHWORDNT" , use the user field CUSTOMER.LSHWORDNT

IF lcSHWCSTNT = 'A' .OR. (lcSHWCSTNT = 'C' .AND. CUSTOMER.LSHWORDNT)
  =NOTEPAD('A',laData[2])
ENDIF  
*-- end of lfSHWCSTNT.

*:**************************************************************************
*:* Name        : lfvPrCode                                       *C200551,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/03/2003
*:* Purpose     : Valid function for price codes
*:***************************************************************************
*:* Called from : user definable fields for price codes in customer screen 
*:***************************************************************************
*:* Parameters : # of field
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
FUNCTION lfvPrCode
PARAMETERS llReturn,lcFldNo
llReturn = .T.
PRIVATE lnSlct,lcPCode,lnIndx,lnI,lcI

IF TYPE('laOgFxFlt') = 'C' AND !EMPTY(laOgFxFlt)
                             && When OK is pressed on OG , the array laOgFxFlt is released
                             && so ignor running this code at OG-Ok pressed,Use it only in normal case when focus is changed of fields
  lnSlct = SELECT()

  lnIndx = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'PRICCODE'+lcFldNo),1)
  lcPCode = laOgFxFlt[lnIndx,6]
  IF !EMPTY(lcPCode)
  
    *-- Open files if not opened
    =lfOpenFil()
    
    *-- Check that this is a valid price code
    IF !SEEK(lcPCode,'CSTPRICH')
      laBrowArr = ''            && empty the array used in browse
      =lfPCodBrow(@lcPCode)  
    ENDIF
    
    IF !EMPTY(lcPCode)
      *-- Check that this code is not repeated 
      FOR lnI = 1 TO 15
        IF lnI # IIF(EMPTY(lcFldNo),1,VAL(lcFldNo))
          lcI = IIF(lnI=1,'',PADL(lnI,2,'0'))
          lnIndx = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'PRICCODE'+lcI),1)
          IF !EMPTY(laOgFxFlt[lnIndx,6]) .AND. lcPCode = laOgFxFlt[lnIndx,6]
            =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
                 'This value is already entered for price code '+ALLT(STR(lnI))+'.')
            lcPCode = ''
            EXIT
          ENDIF
          
        ENDIF
      ENDFOR  
    ENDIF
  
    *-- Update the user definable field
    lnIndx = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'PRICCODE'+lcFldNo),1)
    laOgFxFlt[lnIndx,6] = lcPCode
    laUsrField[lnIndx,6] = lcPCode
  ENDIF  
  SELECT (lnSlct)
  
ENDIF  

RETURN llReturn
*-- end of lfvPrCode.

*!**************************************************************************
*! Name      : lfCollData
*! Developer : Khalid Mohi El-din
*! Date      : 07/03/2003
*! Purpose   : Collect data for invoice charges.
*!**************************************************************************
*! Example   : =lfCollData()
*!**************************************************************************
*B607374,1 KHM 07/03/2003 
*!**************************************************************************
FUNCTION lfCollData

IF llIsEngland
  PRIVATE lnCustRec , lcAlias , lnStandChg
  lcAlias = ALIAS()
  DECLARE laStdShipV[1,2]
  lnStandChg = 0

  *C128481,1 NNA 09/05/2005 (Start) Comment the Next Code to Rewrite it in case Sales Order Currency By ERUO.
  *laStdShipV[1,1] = 'NSTANDCHRG'
  *laStdShipV[1,2] = 'lnStandChg'
  *=gfRltFld(&lcInvHdr..ShipVia,@laStdShipV,'SHIPVIA')
  *RESTORE ADDITIVE FROM (gcWorkDir+lcInvHdr)
  *lnCustRec = RECNO('Customer')
  *=SEEK(IIF(EMPTY(OrdHdr.Store),'M'+OrdHdr.Account,'S'+OrdHdr.Account+OrdHdr.Store),'Customer')
  *SELECT (lcInvHdr)
  *REPLACE lCustCharg WITH .T.
  *IF Customer.lCharge
  *  IF EVALUATE(lcInvHdr+'.TotalChg') > lnDavValue
  *    REPLACE lCustCharg WITH .F. , nCustCharg WITH lnStandChg
  *    IF SEEK(m.Order+m.Store+m.PikTkt+lcDavValue,(lcEngChrg))
  *      SELECT (lcInvHdr)
  *      REPLACE nCariagTax WITH 0 ,;
  *              nChrgTax   WITH nChrgTax - (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
  *      SELECT (lcEngChrg)
  *      DELETE
  *      SELECT (lcInvHdr)            
  *    ENDIF
  *  ELSE
  *    IF OrdHdr.lCarrgChrg
  *      REPLACE lCustCharg WITH .F. , nCustCharg WITH lnStandChg
  *    ELSE
  *      REPLACE lCustCharg WITH .T. , nCustCharg WITH lnStandChg  
  *      *-- We need to add a record in Charges screen.
  *      PRIVATE lnCodRecNo , llCodExist
  *      llCodExist = .T.
  *      lnCodRecNo = RECNO('CODES')
  *      m.cChrgCode = lcDavValue 
  *      IF EMPTY(m.cChrgCode)
  *        =gfModalGen("TRM000000B00000","DIALOG",'','','There is no Non Mechandise Tax code with description called Carriage.')
  *        llCodExist = .F.
  *      ENDIF
  *      IF llCodExist
  *        m.cFrgtAcnt = ''
  *        PRIVATE laChRltFld
  *        IF gfGetMemVar('M_TAX',gcAct_Comp)='Y'
  *          PRIVATE lcTaxRate
  *          DECLARE laChRltFld[1,2]
  *          STORE '' TO lcTaxRate
  *          laChRltFld[1,1] = 'CTAXCODE'
  *          laChRltFld[1,2] = 'lcTaxRate'
  *          =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
  *          IF !EMPTY(lcTaxRate)
  *            laChRltFld[1,1] = 'NTAXRATE'
  *            laChRltFld[1,2] = 'lcTaxRate'
  *            =gfRltFld(ALLTRIM(lcTaxRate),@laChRltFld,'CTAXCODE')
  *            m.nTaxRate = lcTaxRate
  *          ENDIF            
  *        ENDIF
  *        IF gfGetMemVar('M_LINK_GL',gcAct_Comp)='Y'
  *          PRIVATE lcFrgtAcnt
  *          DECLARE laChRltFld[1,2]
  *          STORE '' TO lcFrgtAcnt
  *          laChRltFld[1,1] = 'CFRGTACNT'
  *          laChRltFld[1,2] = 'lcFrgtAcnt'
  *          =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
  *          m.cFrgtAcnt= lcFrgtAcnt
  *        ENDIF
  *        IF !SEEK(m.Order+m.Store+m.PikTkt+lcDavValue,(lcEngChrg))
  *          IF EVALUATE(lcInvHdr+'.TotalChg') < lnDavValue
  *            INSERT INTO (lcEngChrg) (Order,PikTkt,cStore,cChrgCode,nTaxRate,cFrgtAcnt,nChrgAmnt) ;
  *            VALUES (m.Order,m.PikTkt,m.Store,m.cChrgCode,m.nTaxRate,m.cFrgtAcnt,lnStandChg)
  *            SELECT (lcInvHdr)
  *            REPLACE nCariagTax WITH nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100 ,;
  *                    nChrgTax   WITH nChrgTax + (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
  *          ENDIF
  *        ELSE
  *          IF EVALUATE(lcInvHdr+'.TotalChg') > lnDavValue
  *             SELECT (lcInvHdr)
  *             REPLACE nCariagTax WITH 0 ,;
  *             nChrgTax   WITH nChrgTax - (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
  *             SELECT (lcEngChrg)
  *             DELETE
  *             SELECT (lcInvHdr)
  *          ENDIF             
  *        ENDIF          
  *      ELSE
  *        REPLACE lCustCharg WITH .F. , nCustCharg WITH lnStandChg          
  *      ENDIF  
  *    ENDIF
  *  ENDIF      
  *ELSE
  *  REPLACE lCustCharg WITH .F. , nCustCharg WITH lnStandChg
  *ENDIF    
  lnEuroChrg = 0
  IF ORDHDR.CCURRCODE = "EUR"
    laStdShipV[1,1] = 'NEUROCHRG '
    laStdShipV[1,2] = 'lnEuroChrg'
  ELSE
    laStdShipV[1,1] = 'NSTANDCHRG'
    laStdShipV[1,2] = 'lnStandChg'
  ENDIF
  =gfRltFld(&lcInvHdr..ShipVia,@laStdShipV,'SHIPVIA')
  RESTORE ADDITIVE FROM (gcWorkDir+lcInvHdr)
  lnCustRec = RECNO('Customer')
  =SEEK(IIF(EMPTY(OrdHdr.Store),'M'+OrdHdr.Account,'S'+OrdHdr.Account+OrdHdr.Store),'Customer')
  SELECT (lcInvHdr)
  REPLACE lCustCharg WITH .T.
  IF OrdHdr.lCartBc

    *--NNA 11/06/2005 (Start)
    *IF EVALUATE(lcInvHdr+'.TotalChg') > lnDavValue
    IF OrdHdr.BookAmt > lnDavValue
    *--NNA 11/06/2005 (End)
    
      REPLACE lCustCharg WITH .F. , nCustCharg WITH IIF(ORDHDR.CCURRCODE = "EUR",lnEuroChrg,lnStandChg)
      IF SEEK(m.Order+m.Store+m.PikTkt+lcDavValue,(lcEngChrg))
        SELECT (lcInvHdr)
        REPLACE nCariagTax WITH 0 ,;
                nChrgTax   WITH nChrgTax - (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
        SELECT (lcEngChrg)
        DELETE
        SELECT (lcInvHdr)            
      ENDIF
    ELSE
      *--Dealing with minimum order value if > or < the order booked amount not equal
      IF OrdHdr.BookAmt < lnDavValue
        IF OrdHdr.lCarrgChrg

          *C128481,1 NNA 09/05/2005 (Start) if order Carriage Charged before then replace ncustcharg with 0
          *REPLACE lCustCharg WITH .F. , nCustCharg WITH lnStandChg
          REPLACE lCustCharg WITH .F. , nCustCharg WITH 0
          *C128481,1 NNA (End)

        ELSE
          REPLACE lCustCharg WITH .T. , nCustCharg WITH IIF(ORDHDR.CCURRCODE = "EUR",lnEuroChrg,lnStandChg)
          *-- We need to add a record in Charges screen.
          PRIVATE lnCodRecNo , llCodExist
          llCodExist = .T.
          lnCodRecNo = RECNO('CODES')
          m.cChrgCode = lcDavValue 
          IF EMPTY(m.cChrgCode)
            =gfModalGen("TRM000000B00000","DIALOG",'','','There is no Non Mechandise Tax code with description called Carriage.')
            llCodExist = .F.
          ENDIF
          IF llCodExist
            m.cFrgtAcnt = ''
            PRIVATE laChRltFld
            IF gfGetMemVar('M_TAX',gcAct_Comp)='Y'
              PRIVATE lcTaxRate
              DECLARE laChRltFld[1,2]
              STORE '' TO lcTaxRate
              laChRltFld[1,1] = 'CTAXCODE'
              laChRltFld[1,2] = 'lcTaxRate'
              =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
              IF !EMPTY(lcTaxRate)
                laChRltFld[1,1] = 'NTAXRATE'
                laChRltFld[1,2] = 'lcTaxRate'
                =gfRltFld(ALLTRIM(lcTaxRate),@laChRltFld,'CTAXCODE')
                m.nTaxRate = lcTaxRate
              ENDIF            
            ENDIF
            IF gfGetMemVar('M_LINK_GL',gcAct_Comp)='Y'
              PRIVATE lcFrgtAcnt
              DECLARE laChRltFld[1,2]
              STORE '' TO lcFrgtAcnt
              laChRltFld[1,1] = 'CFRGTACNT'
              laChRltFld[1,2] = 'lcFrgtAcnt'
              =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
              m.cFrgtAcnt= lcFrgtAcnt
            ENDIF
            IF !SEEK(m.Order+m.Store+m.PikTkt+lcDavValue,(lcEngChrg))
              IF OrdHdr.BookAmt < lnDavValue
                *--Check if the Customer not Exempted to calc. a vat
                lnCuRecord = RECNO('CUSTOMER')
                IF !EMPTY(Store)
                  =SEEK('S'+Account+Store,'CUSTOMER')
                  llVatExm = Customer.lvatexem 
                ELSE
                  =SEEK('M'+Account,'CUSTOMER')
                  llVatExm = Customer.lvatexem 
                ENDIF
                GOTO lnCuRecord in Customer
                  INSERT INTO (lcEngChrg) (Order,PikTkt,cStore,cChrgCode,nTaxRate,cFrgtAcnt,nChrgAmnt) ;
                  VALUES (m.Order,m.PikTkt,m.Store,m.cChrgCode,IIF(!llVatExm,m.nTaxRate,0),m.cFrgtAcnt,;
                         IIF(ORDHDR.CCURRCODE = "EUR",lnEuroChrg,lnStandChg))
                  SELECT (lcInvHdr)
                  REPLACE nCariagTax WITH nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100 ,;
                          nChrgTax   WITH nChrgTax + IIF(llVatExm,0,(nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100))
                ENDIF
            ELSE
              IF OrdHdr.BookAmt > lnDavValue
                SELECT (lcInvHdr)
                REPLACE nCariagTax WITH 0 ,;
                nChrgTax   WITH nChrgTax - (nCustCharg*(1-Trde_Disc/100)*&lcEngChrg..nTaxRate/100)
                SELECT (lcEngChrg)
                DELETE
                SELECT (lcInvHdr)
              ENDIF             
            ENDIF          
          ELSE
            REPLACE lCustCharg WITH .F. , nCustCharg WITH IIF(ORDHDR.CCURRCODE = "EUR",lnEuroChrg,lnStandChg)          
          ENDIF  
        ENDIF
      ENDIF
    ENDIF
  ELSE
    REPLACE lCustCharg WITH .F. , nCustCharg WITH 0
  ENDIF    
  *C128481,1 NNA (End)

  *B607077,1 KHM 03/24/2003 (Begin) Check if the total value of SO is greater than the
  *B607077,1                minimum sales order value then replace OrdHdr.lCarrgChrg with .T.
  *C037252,1 ABD - When the first invoice is generated for a sales order it automatically 
  *C037252,1 ABD - Updates ORDHDR.LCARRGCHRG to 'Yes' even if carriage has not been 
  *C037252,1 ABD - charged on that first invoice.  [Begin]
  *IF OrdHdr.BookAmt >  lnDavValue
  *  REPLACE OrdHdr.lCarrgChrg WITH .T.
  *ENDIF  
  *REPLACE OrdHdr.lCarrgChrg WITH .T.
  *C037252,1 ABD - [End]
  *B607077,1 KHM 03/24/2003 (End)

  IF BETWEEN(lnCustRec,1,RECCOUNT('Customer'))
    GOTO lnCustRec IN Customer
  ENDIF
  IF !EMPTY(lcAlias)
    SELECT (lcAlias)
  ENDIF
ENDIF  
*-- End of lfCollData.
*!**************************************************************************
*! Name      : lfDelChrg
*! Developer : Khalid Mohi El-din
*! Date      : 07/03/2003
*! Purpose   : To delete all charages if InvHdr.nCharges = 0
*!**************************************************************************
*! Example   : =lfDelChrg()
*!**************************************************************************
*!B607374,1 KHM 07/03/2003 
*!**************************************************************************
FUNCTION lfDelChrg
IF &lcHdrFile..nCharges = 0
  IF SEEK(m.Order+m.Store+m.PikTkt,lcEngChrg)
    SELECT (lcEngChrg)
    DELETE REST WHILE ORDER+STORE+PikTkt+cchrgcode = m.Order+m.Store+m.PikTkt
  ENDIF
ENDIF

*:**************************************************************************
*:* Name        : lfGetClrD                                     *B607371,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/22/2003
*:* Purpose     : Get color width for David luck
*:***************************************************************************
*B124083,1  TMI [Start] Remove this function , there is another funtion with the same name
*FUNCTION lfGetClrD
FUNCTION x_lfGetClrD
*B124083,1  TMI [End  ] 

DECLARE laItemSeg[1]
lcOldSelect=select()

=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*--end function lfGetClrD

*:**************************************************************************
*:* Name        : lfCHKENTRD  
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/05/2003
*:* Purpose     : Check if style.soldout date is earlier than the order entered date
*               : then ask the user is you want to continue ( based on a setup parameter )
*:***************************************************************************
*:* Called from : SOORD.PRG(lfvStyle fn.)
*:***************************************************************************
*C200587,1  
FUNCTION lfCHKENTRD  
PRIVATE llOk,lcStyAls

*C037727,1  TMI [Start] Check if the style customer saved in &lcTmpSty..CCHKACCNT match selected Account in sales order
IF lfChkAccSto() = .F.
  RETURN .F.
ENDIF
*C037727,1  TMI [End  ] 

llOk = .T.
IF gfGetMemvar('M_WRNSLDEN')
  lcStyAls = IIF( TYPE('lcTmpSty') = 'C' AND !EMPTY(lcTmpSty) AND USED(lcTmpSty) , lcTmpSty , 'STYLE' )
  IF !EMPTY(&lcStyAls..SOLDOUT) .AND. ;
    &lcStyAls..SOLDOUT < laData[8] .AND. ;
    gfModalGen('QRM00000B00006',.F.,.F.,.F.,'You have selected a style/colour that has passed its '+;
                                            'Sold Out Date - Do you wish to continue ?') = 2
    llOk = .F.
  ENDIF
ENDIF
RETURN llOk
*-- end of lfCHKENTRD.

*:**************************************************************************
*:* Name        : lfChkAccSto
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/22/2004
*:* Purpose     : Check if the style customer saved in &lcTmpSty..CCHKACCNT match 
*               : selected Account/Store in sales order
*:***************************************************************************
*C037727,1  
FUNCTION lfChkAccSto
PRIVATE lcStyAls
lcStyAls = IIF( TYPE('lcTmpSty') = 'C' AND !EMPTY(lcTmpSty) AND USED(lcTmpSty) , lcTmpSty , 'STYLE' )

IF !EMPTY(&lcStyAls..CCHKCUSTMR)
  IF laData[2] <> &lcStyAls..CCHKCUSTMR
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This style is not assigned to this customer and cannot be used!')
    _CUROBJ = OBJNUM(m.Style)
    RETURN .F.
  ELSE    
    *C037727,3 TMI [START] Do not ask if the Store is empty or not, just compare laData[3] & CCHKSTORE
    *IF !EMPTY(laData[3]) .AND. !EMPTY(&lcStyAls..CCHKSTORE) .AND. laData[3] <> &lcStyAls..CCHKSTORE
    IF !EMPTY(&lcStyAls..CCHKSTORE) .AND. laData[3] <> &lcStyAls..CCHKSTORE
      *C037727,3 TMI [END  ] Do not ask if the Store is empty or not, just compare laData[3] & CCHKSTORE
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This style is not assigned to this customer/store and cannot be used!')
      _CUROBJ = OBJNUM(m.Style)
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
*-- end of lfChkAccSto.

*:**************************************************************************
*:* Name        : lfVCHKCUST
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/22/2004
*:* Purpose     : Valid funciton for the custom field "STYLE.CCHKCUSTMR" and "STYLE.CCHKSTORE"
*:***************************************************************************
*C037727,1  
FUNCTION lfVCHKCUST
PARAMETERS llReturn,lcType
llReturn = .T.
PRIVATE lnSlct,lnIndx,lcAccSto 

IF TYPE('laOgFxFlt') = 'C' AND !EMPTY(laOgFxFlt)
                             && When OK is pressed on OG , the array laOgFxFlt is released
                             && so ignor running this code at OG-Ok pressed,Use it only in normal case when focus is changed of fields
  lnSlct = SELECT()
  =gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
  
  lnAcc = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'CCHKCUSTMR'),1)
  lcAcc = PADR(laOgFxFlt[lnAcc,6],5)
  lnSto = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'CCHKSTORE'),1)
  lcSto = PADR(laOgFxFlt[lnSto,6],8)
  DO CASE 
    CASE lcType = 'M'

      IF !EMPTY(lcAcc)

        IF !SEEK('M'+lcAcc,'CUSTOMER')
          DO CUSBROWM WITH lcAcc
          lcSto = ''
        ELSE
          IF !SEEK('S'+lcAcc+lcSto,'CUSTOMER')
            lcSto = ''
          ENDIF
        ENDIF

      ENDIF
    
    CASE lcType = 'S'
      
      IF EMPTY(lcAcc)
        lcSto = ''
      ELSE     

        IF !EMPTY(lcSto)
          IF !SEEK('S'+lcAcc+lcSto,'CUSTOMER')
            xStore   = lcSto
            IF !CUSBROWS(lcAcc,.T.)
              STORE SPACE(8) TO xStore
            ENDIF
            lcSto = xStore
          ENDIF      
        ENDIF

      ENDIF
      
  ENDCASE
  STORE lcAcc TO laOgFxFlt[lnAcc,6] , laUsrField[lnAcc,6]
  STORE lcSto TO laOgFxFlt[lnSto,6] , laUsrField[lnSto,6]

  SELECT (lnSlct)
  
ENDIF  

RETURN llReturn
*-- end of lfVCHKCUST.

*:**************************************************************************
*:* Name        : lfSOLDOUT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/08/2003
*:* Purpose     : Update completion date in standard screen with the ;
*                 style.soldout based on a parameter M_CMP2SOLD
*:***************************************************************************
*C200587,1  
FUNCTION lfSOLDOUT
PRIVATE lnRecno,llEof

IF gfGetMemVar('M_CMP2SOLD')

  *- If the calling program is SOESSQTY.PRG
  IF TYPE('lcTmpSty') = 'C' AND !EMPTY(lcTmpSty) AND USED(lcTmpSty)

    M.COMPLETE = IIF( !EMPTY(&lcTmpSty..SOLDOUT) , &lcTmpSty..SOLDOUT , laData[10] )

  ELSE  
    *- If the calling program is SOORD.PRG  
    llEof = EOF()
    lcSty = STYLE.STYLE
    =SEEK(M.STYLE,'STYLE')
  
    M.COMPLETE = IIF( !EMPTY(STYLE.SOLDOUT) , STYLE.SOLDOUT , laData[10] )
    
    *- Relocate pointer
    IF llEof
      GO BOTTOM IN STYLE
      SKIP IN STYLE
    ELSE  
      =SEEK(lcSty,'STYLE')
    ENDIF  

  ENDIF
  
ENDIF

*-- end of lfSOLDOUT.
*:**************************************************************************
*:* Name        : lfDLRCVQTY
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 03/14/2004
*:* Module      : PO
*:* Purpose     : get received data screen for Bins logic
*:***************************************************************************
*C037816,1 MHM 04/06/2004 Custom Screen for DL[Start]
FUNCTION lfDLRCVQTY

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  lnAlias = SELECT()

  IF !USED('WHSLOC') 
    =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
  ENDIF

  IF !USED('WHBINLOC')
    =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
  ENDIF

  *--create array for locations 
  DIME laBinLoc[8]
  STORE "" TO laBinLoc
  FOR lnI = 1 TO 8
    lcI = ALLT(STR(lnI,2))
    IF !EMPTY(&lcTmpLine..loc&lcI)
      laBinLoc[lnI] = &lcTmpLine..loc&lcI
    ENDIF  
  ENDFOR

  DO (gcScrDir+gcWinAppl+"\podlrcv.SPX") 

  SELECT(lnAlias)
ELSE
  DO (gcScrDir+"MFRCVQ.SPX")
ENDIF
*:*************************************************************
*! Name        : lfVBNLOC
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : PO
*! Purpose     : Bin Locations valid
*:*************************************************************
*C037816,1 MHM 04/06/2004 Custom Screen for DL[Start]
FUNCTION lfVBNLOC
PARA lnCnxt
lcCnxt = STR(lnCnxt,1)

IF EMPTY(laBinLoc[lnCnxt]) 
  SELECT (lcTmpLine)
  REPLACE &lcTmpLine..Loc&lcCnxt WITH laBinLoc[lnCnxt]
ENDIF
IF !EMPTY(laBinLoc[lnCnxt]) 
  lnAlias = SELECT()
  SELECT WHSLOC
  SET ORDER TO TAG WhsLoc

  *- from testing
  *IF !SEEK(m.cWareCode+UPPER(laBinLoc[lnCnxt])+m.style,'WHSLOC')
  IF !SEEK(m.cWareCode+laBinLoc[lnCnxt],'WHSLOC')
    SET ORDER TO TAG WhsLocSt IN WHSLOC 
    =gfModalGen('INM00000B00000','','','',ALLTRIM(laBinLoc[lnCnxt]) + ' is not a valid location')
    _CUROBJ = OBJNUM(laBinLoc[lnCnxt])
    laBinLoc[lnCnxt] = ''
    RETURN
  ELSE
    =SEEK(m.cWareCode+laBinLoc[lnCnxt],'WHSLOC')
    IF !(WhsLoc.cFlatHang = style.cflathang)
    *  *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot issue the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      laBinLoc[lnCnxt] = ''
      RETURN
    ENDIF
    llClass = .F.
    lcCount = STR(lnCount,1)
    IF (WhsLoc.cBinClass == style.cPrimClss&lcCnxt)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cSecClss&lcCnxt)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cRemClss&lcCnxt)
      llClass = .T.
    ENDIF

    IF !llClass
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot Issue the style to a bin of a different class.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      laBinLoc[lnCnxt] = ''
      RETURN
    ENDIF

    IF !SEEK(m.cWareCode+laBinLoc[lnCnxt]+m.style,'WHSLOC')
      =SEEK(m.cWareCode+laBinLoc[lnCnxt],'WHSLOC')
      lcLocSty = m.style
      SCATT MEMVAR MEMO
      m.style = lcLocSty
      m.clocation = laBinLoc[lnCnxt]
      INSERT INTO WHSLOC FROM MEMVAR 
    ENDIF
    SELECT (lcTmpLine)
    REPLACE &lcTmpLine..Loc&lcCnxt WITH laBinLoc[lnCnxt]
  ENDIF
  SELECT WHSLOC
  SET ORDER TO TAG WhsLocSt
  SELECT(lnAlias)
ENDIF

*:*************************************************************
*! Name        : lfDEFNMNU
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : IC
*! Purpose     : Define Bin Locations valid
*:*************************************************************
*C037816,1 MHM 04/06/2004 Define New Menu[Start]
FUNCTION lfDEFNMNU

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

*--check if the option pad is already defined on the sysmenu
IF !lfFoundPad('Options')
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' ' SKIP FOR IIF(TYPE('lcProgName')='C' AND lcProgName = "ICLOCN",.F.,.T.)
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF
lnBarNo = 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Bins Detail" 
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfBinData IN DAVMAIN
RETURN

*:*************************************************************
*! Name        : lfBinData
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : IC
*! Purpose     : Define Bin Locations valid 
*:*************************************************************
*C037816,1 MHM 04/06/2004 Validate Bin Location Screen in case of IC[Start]
*C132754,3  TMI [Start] comment this funciton and copy ( then use ) instead that of the binmain.prg
*FUNCTION lfBinData
FUNCTION X_lfBinData
*C132754,3  TMI [End  ] 

*C126994,1 NNA 05/26/2005 initialize a new variable to hold the value of Bulk or Pick
*PRIVATE lcFlatHang, lnCapacity,lcBinClass
PRIVATE lcFlatHang, lnCapacity,lcBinClass,lnBlkPck,lcBinSectn
*C126994,1 NNA (End)

IF laScrMode[3] OR laScrMode[4]
  lcFlatHang = IIF(&lcWLocline..cFlatHang='H',2,1) 
  lcBinClass = &lcWLocline..cBinClass
  lnCapacity = &lcWLocline..nCapacity
  *C126994,1 NNA 05/26/2005 (Begin) Get the value for lnBlkPck and section if screen in the edit or the adding mode
  FOR I = 1 TO FCOUNT(lcWLocline)
    IF UPPER(FIELD(I))='CBLKPCK'
      lnBlkPck   = IIF(&lcWLocline..cBlkPck='B',2,1)
    ENDIF
    IF UPPER(FIELD(I))='CSECTION'
      lcBinSectn = &lcWLocline..cSection
    ENDIF
  ENDFOR
  *C126994,1 NNA (End)
ELSE
  lcFlatHang = IIF(WhsLoc.cFlatHang='H',2,1) 
  lcBinClass = WhsLoc.cBinClass
  lnCapacity = WhsLoc.nCapacity
  *C126994,1 NNA 05/26/2005 (Begin) Get the value for lnBlkPck and Section if screen in the view or the selecting mode
  FOR I = 1 TO FCOUNT('WHSLOC')
    IF UPPER(FIELD(I))='CBLKPCK'
      lnBlkPck   = IIF(WhsLoc.cBlkPck='B',2,1)
    ENDIF
    IF UPPER(FIELD(I))='CSECTION'
      lcBinSectn = WhsLoc.cSection
    ENDIF
  ENDFOR
  *C126994,1 NNA (End)
ENDIF

PUSH KEY
ON KEY

DO (gcScrDir+gcWinAppl+"\ICFLTHN.SPX")

POP KEY

*C132754,3  TMI [Start] a copied version from the binmain 
*!***************************************************************************
*!* Name        : lfBinData
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Show the Bin Detail's window 
*!***************************************************************************
*!* Called from : BinMain.prg -->lfDEFNMNU()
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfBinData()
*!***************************************************************************
FUNCTION lfBinData
PRIVATE lcFlatHang, lnCapacity,lcBinClass,lnBlkPck,lcBinSectn,lcWinTitle,llhasStock,lcBnLocatn
STORE '' TO lcWinTitle,lcFlatHang,lcBinClass,lcBinSectn
STORE .F. TO llhasStock
STORE 0 TO lnCapacity,lnBlkPck,lnStock
IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLOC,WHSLOC",'')
  RETURN
ENDIF
IF laScrMode[3] OR laScrMode[4]
  lcFlatHang = IIF(&lcWLocline..cFlatHang='H',2,1) 
  lcBinClass = &lcWLocline..cBinClass
  lnCapacity = &lcWLocline..nCapacity
  *--Get the value for lnBlkPck and section if screen in the edit or the adding mode
  lnBlkPck   = IIF(&lcWLocline..cBlkPck='B',2,1)
  lcBinSectn = &lcWLocline..cSection
ELSE
  lcFlatHang = IIF(WhsLoc.cFlatHang='H',2,1) 
  lcBinClass = WhsLoc.cBinClass
  lnCapacity = WhsLoc.nCapacity
 *--Get the value for CBLKPCK and Section if screen in the view or the selecting mode
 lnBlkPck   = IIF(WhsLoc.cBlkPck='B',2,1)
 lcBinSectn = WhsLoc.cSection
ENDIF
*C132754,1  TMI [Start] Define replenshement field to be used in the icflthn.scx screen
PRIVATE lnRplnsh,lcTbl
lcTbl = IIF(laScrMode[3] OR laScrMode[4],lcWLocline,'WHSLOC')
lnRplnsh = IIF(&lcTbl..CREPLENISH='Y',1,2)
*C132754,1  TMI [End  ] 
IF laScrMode[3]
  IF SEEK(LADATA[1]+&lcWLocline..cLocation,'WHBINLOC')
    llhasStock= .T.
  ENDIF
ENDIF
PUSH KEY
ON KEY
lcWinTitle = 'Warehouse detail for Bin : ' + ALLTRIM(IIF(laScrMode[3] OR laScrMode[4],&lcWLocline..cLocation,WhsLoc.cLocation))
DO (gcScrDir+gcWinAppl+"\ICFLTHN.SPX")
POP KEY
*-- End of Function lfBinData.
*C132754,3 TMI [End  ] 

*:*************************************************************
*! Name        : lfvsavlin
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : IC
*! Purpose     : Save lines to warehouse file
*:*************************************************************
*C037816,1 MHM 04/06/2004 Save Data to custom Fields[Start]
FUNCTION lfvsavlin

IF laScrMode[3] OR laScrMode[4]
  REPLACE &lcWLocline..cFlatHang WITH IIF(lcFlatHang=2,'H','F') 
  REPLACE &lcWLocline..cBinClass WITH IIF(EMPTY(lcBinClass),'',ALLTRIM(lcBinClass))
  REPLACE &lcWLocline..nCapacity WITH lnCapacity

  *C126994,1 NNA 05/26/2005 (Begin) Replace the temp file Field with the selected Value
  FOR I = 1 TO FCOUNT(lcWLocline)
    IF UPPER(FIELD(I))='CBLKPCK'
      REPLACE &lcWLocline..cBlkPck WITH IIF(lnBlkPck=2,'B','P')
    ENDIF
    IF UPPER(FIELD(I))='CSECTION'
      REPLACE &lcWLocline..cSection WITH lcBinSectn
    ENDIF
  ENDFOR
  *C126994,1 NNA (End)

  *C132754,1  TMI [Start] update the replenshemnt field
  REPLACE &lcWLocline..CREPLENISH WITH IIF(lnRplnsh=1,'Y',' ')
  *C132754,1  TMI [End  ]           

ENDIF

*!*************************************************************
*! Name        : lfWShwlin
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : IC
*! Purpose     : Show lines of table
*!*************************************************************
*C037816,1 MHM 04/06/2004 Control Showing[Start]
FUNCTION lfWShwlin

IF laScrMode[3] OR laScrMode[4]
  SHOW GET lcFlatHang ENABLE
  SHOW GET lcBinClass ENABLE
  SHOW GET lnCapacity ENABLE
  SHOW GET pbSave     ENABLE
  
  *C126994,1 NNA 05/26/2005 (Begin) if the screen is in the edit or the adding mode then get the (Bulk or Pick) popup Enable
  SHOW GET lcBinSectn ENABLE
  SHOW GET lnBlkPck   ENABLE
  *C126994,1 NNA (End)
  
ELSE
  SHOW GET lcFlatHang DISABLE
  SHOW GET lcBinClass DISABLE
  SHOW GET lnCapacity DISABLE
  SHOW GET pbSave     DISABLE

  *C126994,1 NNA 05/26/2005 (Begin) if the screen is in the view or the selecting mode then get the (Bulk or Pick) popup Disable
  SHOW GET lcBinSectn DISABLE
  SHOW GET lnBlkPck   DISABLE
  *C126994,1 NNA (End)

ENDIF

*!*************************************************************
*! Name        : lfDLSAVCS
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : IC
*! Purpose     : Save custom fields
*!*************************************************************
*C037816,1 MHM 04/06/2004 Save Custom Field
FUNCTION lfDLSAVCS
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF
IF laScrMode[4]
  REPLACE cFlatHang WITH &lcWLocline..cFlatHang ,;
          cBinClass WITH &lcWLocline..cBinClass ,;
          nCapacity WITH &lcWLocline..nCapacity 
 *C126994,1 NNA 05/26/2005 (Begin) Save the New Field (cBlkPck) and cSection
  FOR I = 1 TO FCOUNT(lcWLocline)
    IF UPPER(FIELD(I))='CBLKPCK'
      REPLACE cBlkPck WITH &lcWLocline..cBlkPck
    ENDIF
    IF UPPER(FIELD(I))='CSECTION'
      REPLACE cSection WITH &lcWLocline..cSection
    ENDIF
  ENDFOR
  *C126994,1 NNA (End)
ELSE
  
  *C132754,3  TMI [Start] 
  IF laScrMode[3]
  *C132754,3  TMI [End  ] 
  
  *C126994,1 NNA 05/26/2005 (Begin) Be sure that fields (cBlkPck and cSection) found in file WhSloc
  STORE .F. TO llWSLOCBPK,llWSLOCSEC
  SELECT WHSLOC
  FOR I = 1 TO FCOUNT('WHSLOC')
    IF UPPER(FIELD(I))='CBLKPCK'
      llWSLOCBPK = .T.
    ENDIF
    IF UPPER(FIELD(I))='CSECTION'
      llWSLOCSEC = .T.
    ENDIF
  ENDFOR
  *C126994,1 NNA (End)
  
  SELECT (lcWLocline)
  SCAN FOR cWareCode+cLocation+Style+Color = ladata[1] 
    *-- If you add new location.
    IF SEEK(cWareCode+cLocation+Style+Color,'WhsLoc')
      REPLACE WhsLoc.cFlatHang WITH &lcWLocline..cFlatHang ,;
              WhsLoc.cBinClass WITH &lcWLocline..cBinClass ,;
              WhsLoc.nCapacity WITH &lcWLocline..nCapacity 
      *C126994,1 NNA 05/26/2005 (Begin) Save the New Field (cBlkPck)
      IF llWSLOCBPK
        REPLACE WhsLoc.cBlkPck   WITH &lcWLocline..cBlkPck               
      ENDIF
      IF llWSLOCSEC
        REPLACE WhsLoc.cSection WITH &lcWLocline..cSection
      ENDIF
      *C126994,1 NNA (End)
    ENDIF

    *C132754,1  TMI [Start] Update the CREPLENISH field
    SELECT WhsLoc
    REPLACE CREPLENISH WITH &lcWLocline..CREPLENISH    
    *C132754,1  TMI [End  ] 
    
  ENDSCAN  
  
  *C132754,3  TMI [Start] 
  ENDIF
  *C132754,3  TMI [End  ] 
  
ENDIF
*!*************************************************************
*! Name        : lfADDFILDS
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : IC
*! Purpose     : Add Custom Fields for temp file
*!*************************************************************
*C037816,1 MHM 04/06/2004 add Custom Fields
FUNCTION lfADDFILDS

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  lnFStru = ALEN(laFStru,1)

  DIMENSION laFStru[lnFStru+8,4]

  laFStru[lnFStru+1,1] = 'Loc1'
  laFStru[lnFStru+1,2] = 'C'
  laFStru[lnFStru+1,3] = 10
  laFStru[lnFStru+1,4] = 0

  laFStru[lnFStru+2,1] = 'Loc2'
  laFStru[lnFStru+2,2] = 'C'
  laFStru[lnFStru+2,3] = 10
  laFStru[lnFStru+2,4] = 0

  laFStru[lnFStru+3,1] = 'Loc3'  
  laFStru[lnFStru+3,2] = 'C'  
  laFStru[lnFStru+3,3] = 10
  laFStru[lnFStru+3,4] = 0

  laFStru[lnFStru+4,1] = 'Loc4'
  laFStru[lnFStru+4,2] = 'C'
  laFStru[lnFStru+4,3] = 10
  laFStru[lnFStru+4,4] = 0

  laFStru[lnFStru+5,1] = 'Loc5'
  laFStru[lnFStru+5,2] = 'C'
  laFStru[lnFStru+5,3] = 10
  laFStru[lnFStru+5,4] = 0

  laFStru[lnFStru+6,1] = 'Loc6'
  laFStru[lnFStru+6,2] = 'C'
  laFStru[lnFStru+6,3] = 10
  laFStru[lnFStru+6,4] = 0

  laFStru[lnFStru+7,1] = 'Loc7'
  laFStru[lnFStru+7,2] = 'C'
  laFStru[lnFStru+7,3] = 10
  laFStru[lnFStru+7,4] = 0

  laFStru[lnFStru+8,1] = 'Loc8'
  laFStru[lnFStru+8,2] = 'C'
  laFStru[lnFStru+8,3] = 10
  laFStru[lnFStru+8,4] = 0
ENDIF
*:*************************************************************
*! Name        : lfSAVBINS
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : PO
*! Purpose     : Save Locations in WHBINLOC File from PO
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfSAVBINS

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

*C126994,1 NNA 05/26/2005 (Begin) if the Receiving into Multiple Bins Screen is found then Save from function (lfSavMltBn)
IF ASCAN(laEvntTrig,'CRTBINLN') <> 0 AND ASCAN(laEvntTrig,'DISBNLOC') <> 0
  =lfSavMltBn()
  RETURN
ENDIF
*C126994,1 NNA (End)

PRIVATE lnAlias
lnAlias = SELECT(0)
IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF

IF !USED('WHSLOC') 
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
ENDIF

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF


IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF

SELECT (lcTmpLine)
lcOldOrder = ORDER()
SET ORDER TO
LOCATE

SCAN 
  IF Trancd = '1'
    m.loc1 = &lcTmpLine..loc1
    m.loc2 = &lcTmpLine..loc2
    m.loc3 = &lcTmpLine..loc3
    m.loc4 = &lcTmpLine..loc4
    m.loc5 = &lcTmpLine..loc5
    m.loc6 = &lcTmpLine..loc6
    m.loc7 = &lcTmpLine..loc7
    m.loc8 = &lcTmpLine..loc8
  ENDIF
  IF Trancd <> '2'
    LOOP
  ENDIF
  FOR lnI = 1 TO 8
    lcI = ALLT(STR(lnI,2))
    IF !EMPTY(m.loc&lcI)
      IF !SEEK(&lcTmpLine..cWareCode+m.loc&lcI+&lcTmpLine..style,'WHBINLOC')
        
        SET ORDER TO TAG WhsLoc IN  WHSLOC
        *! C123853,1 MHM 01/15/2005  seek to only bin location[Start]
        *IF SEEK(&lcTmpLine..cWareCode+m.loc&lcI+&lcTmpLine..style,'WHSLOC')
        IF SEEK(&lcTmpLine..cWareCode+m.loc&lcI,'WHSLOC')
        *! C123853,1 MHM 01/15/2005  [End]
          m.cbinclass = WHSLOC.cbinclass

          *C126994,1 NNA 05/26/2005 (BEGIN) Get cBlkPck and cSection
          STORE 0 TO lnOldAlias
          lnOldAlias=SELECT(0)
          SELECT WHSLOC
          FOR I = 1 TO FCOUNT('WHSLOC')
            IF UPPER(FIELD(I))='CBLKPCK'
              m.cBlkPck   = WHSLOC.cBlkPck
            ENDIF
            IF UPPER(FIELD(I))='CSECTION'
              m.cSection  = WHSLOC.cSection
            ENDIF
          ENDFOR
          SELECT(lnOldAlias)
          *C126994,1 NNA (End)

        ENDIF
        SET ORDER TO TAG WhsLocSt IN  WHSLOC
        
        SELECT WHBINLOC
        APPEND BLANK
        REPLACE Style        WITH  &lcTmpLine..style  ,;
  		        cWareCode    WITH  &lcTmpLine..cWareCode,;
  		        clocation    WITH  m.loc&lcI,;
  		        Qty&lcI      WITH  &lcTmpLine..Qty&lcI,;
  		        TOTQTY       WITH  &lcTmpLine..Qty&lcI,;
  		        cbinclass    WITH  m.cbinclass
 
        *C126994,1 NNA 05/26/2005 (BEGIN) Update cBlkPck and cSection
        FOR I = 1 TO FCOUNT('WHBINLOC')
          IF UPPER(FIELD(I))='CBLKPCK'
            REPLACE cBlkPck WITH m.cBlkPck
          ENDIF
          IF UPPER(FIELD(I))='CSECTION'
            REPLACE cSection WITH m.cSection
          ENDIF
        ENDFOR
        *C126994,1 NNA (End)

        =gfAdd_Info('WHBINLOC')
      ELSE
        REPLACE  WHBINLOC.Qty&lcI WITH WHBINLOC.Qty&lcI + &lcTmpLine..Qty&lcI,;
                 WHBINLOC.TOTQTY  WITH WHBINLOC.TOTQTY  + &lcTmpLine..Qty&lcI
      ENDIF     
       
    ENDIF  
  ENDFOR
ENDSCAN
SET ORDER TO &lcOldOrder

*SELECT STYINVJL
*SET ORDER TO &lcStyOrd 
SELECT(lnAlias)
*:*************************************************************
*! Name        : lfVLDRFRSH
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : PO
*! Purpose     : Refrish custom screen 
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfVLDRFRSH

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  =lfRefresh("PODLRCV")
ENDIF  

*:*************************************************************
*! Name        : lfDLGETBIN
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : IC
*! Purpose     : Screen of get bins in case of adj.
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLGETBIN

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

*IF EMPTY(lcWareH)
*  RETURN
*ENDIF

lnAlias = SELECT()

IF !USED('WHSLOC') 
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
ENDIF

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF

SET ORDER TO TAG WhsLoc IN  WHSLOC
IF !SEEK(lcWareH ,'WHSLOC')
  SET ORDER TO TAG WhsLocSt IN  WHSLOC
  RETURN
ENDIF
SET ORDER TO TAG WhsLocSt IN  WHSLOC

lcBinLoc = ''
DIME laTempData[3]
STORE '' TO laTempData
DO (gcScrDir+gcWinAppl+"\ICDLBIN.SPX")

SELECT(lnAlias)
*:*************************************************************
*! Name        : lfvbinloc
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : IC
*! Purpose     : Screen of get bins in case of adj.
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfvbinloc

PRIVATE llFromSeek
STORE .F.TO llFromSeek

SELECT WhsLoc
IF EMPTY(lcBinLoc) AND !llbrowse
  RETURN
ENDIF
SET ORDER TO TAG WhsBin IN  WHSLOC
IF lcType = 'T'
  IF lnWareNm = 1
    lcWarehs =lcFromWare
  ELSE
    lcWarehs =lcToWare
  ENDIF
ELSE
    lcWarehs =lcFromWare
ENDIF  

IF llbrowse OR (!EMPTY(lcBinLoc) AND !SEEK(lcBinLoc+lcWarehs))

  llbrowse =.F. 
  lcBrFields = "clocation  :H='Bin',"+;
                   "cBinClass  :H='Bin Class',"+;
                   "cFlatHang  :H='Flat or Hang'"
                 
  lcForExpr = " cWareCode = lcWareH  AND EMPTY(Style)"
  STORE '' TO laTempData
  =SEEK(ALLTRIM(lcBinLoc))
  =gfBrows([FOR &lcForExpr],'cLocation,cBinClass,cFlatHang','laTempData','WhsLoc')
  lcBinLoc=laTempData[1]
ELSE
  laTempData[1] = lcBinLoc
  laTempData[2] = WhsLoc.cbinclass
  laTempData[3] = WhsLoc.cflathang
  STORE .T.TO llFromSeek
ENDIF

SET ORDER TO TAG WhsLocSt IN  WHSLOC
PRIVATE lcFileNama
lcFileNama = lcTmpAdj+"A"
IF lcType = 'T'
  IF lnWareNm = 1
    IF !SEEK(lcWareH +ALLTRIM(lcBinLoc),'WHBINLOC')
      lcMsg2 = 'No stock available to transfer'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      lcBinLoc = ''
      _CUROBJ = OBJNUM(pbCan)
      RETURN      
    ENDIF
  ELSE
    IF &lcFileNamA..LocFrom = lcBinLoc And lcFromWare = lcToWare
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot transfer to the same bin Location'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      lcBinLoc = ''
      _CUROBJ = OBJNUM(pbCan)
      RETURN
    ENDIF
    *IF !(&lcFileNamA..ClassFrom = laTempData[2])
    *  *--You cannot transfer from the same warehouse.
    *  lcMsg2 = 'You cannot transfer the style to a bin of a different class.'
    *  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    *  lcBinLoc = ''
    *  _CUROBJ = OBJNUM(pbCan)
    *  RETURN
    *ENDIF

    IF !(&lcFileNamA..FlatFrom = laTempData[3])
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot transfer the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      lcBinLoc = ''
      _CUROBJ = OBJNUM(pbCan)
      RETURN
    ENDIF
  ENDIF
ENDIF
IF llFromSeek
 =lfvOk()
ENDIF

*!**************************************************************************
*! Name      : lfINITVAR
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : To Initiate variables for the new modification
*!**************************************************************************
*! Example   : = lfINITVAR()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfINITVAR   
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF
lcFileNamA = lcTmpAdj+"A"
IF !USED(lcFileNamA)
  CREATE CURSOR (lcFileNamA) (LocFrom C(10),LocTo C(10),ClassFrom C(2), ClassTo C(2), FlatFrom C(1), FlatTo C(1))
  APPEND BLANK
ENDIF  
*:*************************************************************
*! Name        : lfvOk
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : IC
*! Purpose     : Screen of get bins in case of adj.
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfvOk

PRIVATE lcFileNama
IF EMPTY(lcBinLoc)
  lcMsg2 = 'You have to select a bin location'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  _CUROBJ = OBJNUM(lcBinLoc)
  llLoc  = .F.
  laScrMode = .F.
  laScrMode[1] = .T.
  llCUpdate = .F.
  SHOW GETS
  CLEAR READ  

  RETURN
ENDIF
lcFileNama = lcTmpAdj+"A"
IF lcType = 'T'
  IF lnWareNm = 1
    REPLACE  &lcFileNamA..LocFrom    WITH lcBinLoc,;
             &lcFileNamA..ClassFrom  WITH laTempData[2] ,;
             &lcFileNamA..FlatFrom   WITH laTempData[3]
  ELSE
    REPLACE  &lcFileNamA..LocTo    WITH lcBinLoc,;
             &lcFileNamA..ClassTO  WITH laTempData[2] ,;
             &lcFileNamA..FlatTo   WITH laTempData[3]
  ENDIF
ELSE
  *! C123853,1 MHM 01/15/2005  Comment type[Start]
  *IF lcType = 'A'
  *! C123853,1 MHM 01/15/2005  [End]
  
    REPLACE  &lcFileNamA..LocFrom  WITH lcBinLoc

  *! C123853,1 MHM 01/15/2005  Comment[Start]
  *ENDIF
  *! C123853,1 MHM 01/15/2005  [End]
  
ENDIF
CLEAR READ
*!**************************************************************************
*! Name      : lfDLVLDBIN
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Return if called frfom custom Bin system
*!**************************************************************************
*! Example   : = lfINITVAR()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLVLDBIN
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  RETURN
ENDIF
lcMsg2 = 'Sorry, bins cannot be assigned to styles here. You can assign a bin(s) to styles from the Location screen'
=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)

*!**************************************************************************
*! Name      : lfDLUPDTGL
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Return if called from custom Bin system
*!**************************************************************************
*! Example   : = lfINITVAR()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLUPDTGL
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  IF lcType = 'T'
    PRIVATE lcFileNama
    lcFileNama = lcTmpAdj+"A"
    IF !(&lcFileNamA..LocFrom = &lcFileNamA..LocTo AND  lcFromWare = lcToWare)
      WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
      SELECT (lcTmpGlDt)
      *-- Generate a unique session number.
      lcGlSess = gfsequence('GLSESSION')
      REPLACE ALL GLSESSION WITH lcGlSess
      USE
      SELECT GLDIST  
      APPEND FROM &gcWorkDir.&lcTmpGlDt     
      ERASE (gcWorkDir+lcTmpGlDt+'.DBF')
    ENDIF  
  ENDIF  
ELSE
  WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
  SELECT (lcTmpGlDt)
  *-- Generate a unique session number.
  lcGlSess = gfsequence('GLSESSION')
  REPLACE ALL GLSESSION WITH lcGlSess
  USE
  SELECT GLDIST  
  APPEND FROM &gcWorkDir.&lcTmpGlDt     
  ERASE (gcWorkDir+lcTmpGlDt+'.DBF')
ENDIF
*!**************************************************************************
*! Name      : lfDLSAVDAT
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Save Data
*!**************************************************************************
*! Example   : = lfDLSAVDAT()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLSAVDAT
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF

IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF
*SELECT STYINVJL
*lcStyOrd = ORDER()
*SET ORDER TO 

PRIVATE lcFileNama
lcFileNama = lcTmpAdj+"A"

SELECT (lcTmpAdj)
LOCATE
IF lcType = 'T'
  SCAN
    *--from location
    IF SEEK(&lcTmpAdj..cfromware +&lcFileNamA..LocFrom +&lcTmpAdj..Style,'WHBINLOC') 
      SELECT WHBINLOC
      REPLACE Qty1       WITH MAX(Qty1- &lcTmpAdj..adj1,0),;
              Qty2       WITH MAX(Qty2- &lcTmpAdj..adj2,0),;
              Qty3       WITH MAX(Qty3- &lcTmpAdj..adj3,0),;
              Qty4       WITH MAX(Qty4- &lcTmpAdj..adj4,0),;
              Qty5       WITH MAX(Qty5- &lcTmpAdj..adj5,0),;
              Qty6       WITH MAX(Qty6- &lcTmpAdj..adj6,0),;
              Qty7       WITH MAX(Qty7- &lcTmpAdj..adj7,0),;
              Qty8       WITH MAX(Qty8- &lcTmpAdj..adj8,0),;
              TotQty     WITH MAX(TotQty- &lcTmpAdj..Totadj,0)
      IF TotQty = 0
        DELETE
      ENDIF
    ENDIF

    *--To location
    IF !SEEK(&lcTmpAdj..cToware +&lcFileNamA..LocTo +&lcTmpAdj..Style,'WHBINLOC') 
      SELECT WHBINLOC
      APPEND BLANK
      REPLACE STYLE      WITH &lcTmpAdj..Style,;
              CWARECODE  WITH &lcTmpAdj..cToware,;
              clocation  WITH &lcFileNamA..LocTo ,;
              Qty1       WITH Qty1+ &lcTmpAdj..adj1,;
              Qty2       WITH Qty2+ &lcTmpAdj..adj2,;
              Qty3       WITH Qty3+ &lcTmpAdj..adj3,;
              Qty4       WITH Qty4+ &lcTmpAdj..adj4,;
              Qty5       WITH Qty5+ &lcTmpAdj..adj5,;
              Qty6       WITH Qty6+ &lcTmpAdj..adj6,;
              Qty7       WITH Qty7+ &lcTmpAdj..adj7,;
              Qty8       WITH Qty8+ &lcTmpAdj..adj8,;
              TotQty     WITH TotQty+ &lcTmpAdj..Totadj

        *C126994,1 NNA 05/26/2005 (BEGIN) Update cBlkPck and cSection
        lcOldOrd = ORDER('WHSLOC')
        SET ORDER TO TAG WhsLoc IN  WHSLOC
        IF SEEK(&lcTmpAdj..cfromware +&lcFileNamA..LocFrom +SPACE(19),'WHSLOC') 
          SELECT WHSLOC
          FOR I = 1 TO FCOUNT('WHSLOC')
            IF UPPER(FIELD(I))='CBLKPCK'
              lcBlkPck   = WHSLOC.cBlkPck
            ENDIF
            IF UPPER(FIELD(I))='CSECTION'
              lcSection  = WHSLOC.cSection
            ENDIF
          ENDFOR
          SELECT WHBINLOC
          FOR I = 1 TO FCOUNT('WHBINLOC')
            IF UPPER(FIELD(I))='CBLKPCK'
              REPLACE cBlkPck WITH lcBlkPck
            ENDIF
            IF UPPER(FIELD(I))='CSECTION'
              REPLACE cSection WITH lcSection
            ENDIF
          ENDFOR
        ENDIF
        SET ORDER TO TAG &lcOldOrd IN  WHSLOC
        *C126994,1 NNA (End)


      =gfAdd_Info('WHBINLOC')

    ELSE
      SELECT WHBINLOC
      REPLACE Qty1       WITH Qty1+ &lcTmpAdj..adj1,;
              Qty2       WITH Qty2+ &lcTmpAdj..adj2,;
              Qty3       WITH Qty3+ &lcTmpAdj..adj3,;
              Qty4       WITH Qty4+ &lcTmpAdj..adj4,;
              Qty5       WITH Qty5+ &lcTmpAdj..adj5,;
              Qty6       WITH Qty6+ &lcTmpAdj..adj6,;
              Qty7       WITH Qty7+ &lcTmpAdj..adj7,;
              Qty8       WITH Qty8+ &lcTmpAdj..adj8,;
              TotQty     WITH TotQty+ &lcTmpAdj..Totadj

    ENDIF
    SELECT WHBINLOC
    IF TotQty = 0
      DELETE
    ENDIF         
    
  ENDSCAN 
ELSE
  SCAN
    IF !SEEK(&lcTmpAdj..cfromware +&lcFileNamA..LocFrom +&lcTmpAdj..Style,'WHBINLOC') 
      SELECT WHBINLOC
      APPEND BLANK
      REPLACE STYLE      WITH &lcTmpAdj..Style,;
              CWARECODE  WITH &lcTmpAdj..cfromware,;
              clocation  WITH &lcFileNamA..LocFrom ,;
              Qty1       WITH Qty1+ &lcTmpAdj..adj1,;
              Qty2       WITH Qty2+ &lcTmpAdj..adj2,;
              Qty3       WITH Qty3+ &lcTmpAdj..adj3,;
              Qty4       WITH Qty4+ &lcTmpAdj..adj4,;
              Qty5       WITH Qty5+ &lcTmpAdj..adj5,;
              Qty6       WITH Qty6+ &lcTmpAdj..adj6,;
              Qty7       WITH Qty7+ &lcTmpAdj..adj7,;
              Qty8       WITH Qty8+ &lcTmpAdj..adj8,;
              TotQty     WITH TotQty+ &lcTmpAdj..Totadj

        *C126994,1 NNA 05/26/2005 (BEGIN) Update cBlkPck and cSection
        lcOldOrd = ORDER('WHSLOC')
        SET ORDER TO TAG WhsLoc IN  WHSLOC
        IF SEEK(&lcTmpAdj..cfromware +&lcFileNamA..LocFrom +SPACE(19),'WHSLOC') 
          SELECT WHSLOC
          FOR I = 1 TO FCOUNT('WHSLOC')
            IF UPPER(FIELD(I))='CBLKPCK'
              lcBlkPck   = WHSLOC.cBlkPck
            ENDIF
            IF UPPER(FIELD(I))='CSECTION'
              lcSection  = WHSLOC.cSection
            ENDIF
          ENDFOR
          SELECT WHBINLOC
          FOR I = 1 TO FCOUNT('WHBINLOC')
            IF UPPER(FIELD(I))='CBLKPCK'
              REPLACE cBlkPck WITH lcBlkPck
            ENDIF
            IF UPPER(FIELD(I))='CSECTION'
              REPLACE cSection WITH lcSection
            ENDIF
          ENDFOR
        ENDIF
        SET ORDER TO TAG &lcOldOrd IN  WHSLOC
        *C126994,1 NNA (End)

      =gfAdd_Info('WHBINLOC')
      IF TotQty = 0
        DELETE
      ENDIF         
    ELSE
      *! C123853,1 MHM 01/15/2005  include phsical inventory[Start]
      IF lcType = 'P'
        SELECT WHBINLOC
        REPLACE Qty1       WITH MAX(&lcTmpAdj..adj1,0),;
                Qty2       WITH MAX(&lcTmpAdj..adj2,0),;
                Qty3       WITH MAX(&lcTmpAdj..adj3,0),;
                Qty4       WITH MAX(&lcTmpAdj..adj4,0),;
                Qty5       WITH MAX(&lcTmpAdj..adj5,0),;
                Qty6       WITH MAX(&lcTmpAdj..adj6,0),;
                Qty7       WITH MAX(&lcTmpAdj..adj7,0),;
                Qty8       WITH MAX(&lcTmpAdj..adj8,0),;
                TotQty     WITH MAX(&lcTmpAdj..Totadj,0)
        IF TotQty = 0
          DELETE
        ENDIF         
      ELSE
      *! C123853,1 [End]
      
        SELECT WHBINLOC
        REPLACE Qty1       WITH MAX(Qty1+ &lcTmpAdj..adj1,0),;
                Qty2       WITH MAX(Qty2+ &lcTmpAdj..adj2,0),;
                Qty3       WITH MAX(Qty3+ &lcTmpAdj..adj3,0),;
                Qty4       WITH MAX(Qty4+ &lcTmpAdj..adj4,0),;
                Qty5       WITH MAX(Qty5+ &lcTmpAdj..adj5,0),;
                Qty6       WITH MAX(Qty6+ &lcTmpAdj..adj6,0),;
                Qty7       WITH MAX(Qty7+ &lcTmpAdj..adj7,0),;
                Qty8       WITH MAX(Qty8+ &lcTmpAdj..adj8,0),;
                TotQty     WITH MAX(TotQty+ &lcTmpAdj..Totadj,0)
        IF TotQty = 0
          DELETE
        ENDIF         
      *! C123853,1 MHM 01/15/2005  End if statment[Start]
      ENDIF  
      *! C123853,1 MHM 01/15/2005  [End]
    ENDIF
  ENDSCAN 
ENDIF


*SELECT STYINVJL
*SET ORDER TO &lcStyOrd
*SELECT(lnAlias)
  
*!**************************************************************************
*! Name      : lfDLCHKTRN
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/25/2004
*! Purpose   : Check Transaction
*!**************************************************************************
*! Example   : = lfDLCHKTRN()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLCHKTRN

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin

  IF lcType = 'T'
    *--check if Bin transfer to same class as or not
    PRIVATE lcFileNama
    lcFileNama = lcTmpAdj+"A"
  
    SELECT WHSLOC
    SET ORDER TO TAG WhsLoc

    
    IF SEEK(lcToWare+&lcFileNamA..LocTo,'WHSLOC')
      llClass = .F.
      *lcCount = STR(lnCount,1)
      IF (WhsLoc.cBinClass == style.cPrimClss&lcCont)
        llClass = .T.
      ENDIF
      IF (WhsLoc.cBinClass == style.cSecClss&lcCont)
        llClass = .T.
      ENDIF
      IF (WhsLoc.cBinClass == style.cRemClss&lcCont)
        llClass = .T.
      ENDIF

      IF !llClass
        SELECT WHSLOC
        SET ORDER TO TAG WhsLocSt IN WHSLOC 
        *--You cannot transfer from the same warehouse.
        lcMsg2 = 'You cannot Issue the style Size to a bin of a different class.'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        m.Adj&lcCont = 0
        RETURN
      ENDIF
    ENDIF
    SELECT WHSLOC
    SET ORDER TO TAG WhsLocSt IN WHSLOC 

    IF !(&lcFileNamA..LocFrom = &lcFileNamA..LocTo AND  lcFromWare = lcToWare)
      IF !llModiMod
        *--mhm123856
        laTStk[I] = laOTStk[I] + m.Adj&lcCont
        *laTStk[I] = laTStk[I] + m.Adj&lcCont
        *--mhm123856
      ELSE
        laTStk[I] = ( laOTStk[I] - &lcTmpAdj->Adj&lcCont ) + m.Adj&lcCont  
      ENDIF
      laTStk[9] = laTStk[1]+laTStk[2]+laTStk[3]+laTStk[4]+laTStk[5]+laTStk[6]+laTStk[7]+laTStk[8]
   ENDIF 
  ENDIF
ELSE
  IF !llModiMod
    laTStk[I] = laOTStk[I] + m.Adj&lcCont
  ELSE
    laTStk[I] = ( laOTStk[I] - &lcTmpAdj->Adj&lcCont ) + m.Adj&lcCont  
  ENDIF
  laTStk[9] = laTStk[1]+laTStk[2]+laTStk[3]+laTStk[4]+laTStk[5]+laTStk[6]+laTStk[7]+laTStk[8]
ENDIF  
  
*!**************************************************************************
*! Name      : lfDLCHKADJTR
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Check Data in case of transfir data
*!**************************************************************************
*! Example   : = lfDLCHKADJTR()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLCHKADJTR

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  SET ORDER TO TAG WhsLoc IN  WHSLOC
  IF !SEEK(lcWareH ,'WHSLOC')
    lcMsg2 = 'No bin location found for warehouse ' +lcWareH
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    _CUROBJ = OBJNUM(lcWareH)
    SET ORDER TO TAG WhsLocSt IN  WHSLOC
    RETURN
  ENDIF
  SET ORDER TO TAG WhsLocSt IN  WHSLOC

  IF !laScrMode[4]
    laScrMode = .F.
    laScrMode[4] = .T.
    =lfActBrow()
    llCUpdate = .T.
    SHOW GETS   
  ENDIF
ELSE
  IF lcFromWare = lcToWare
    *--You cannot transfer from the same warehouse.
    =gfModalGen('TRM42055B42001','DIALOG')
    RETURN .F.
  ELSE
    IF !laScrMode[4]
      laScrMode = .F.
      laScrMode[4] = .T.
      =lfActBrow()
      llCUpdate = .T.
      SHOW GETS   
    ENDIF
  ENDIF
ENDIF  

*!**************************************************************************
*! Name      : lfDLCHKADJAD
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Check Data in case of adj. data
*!**************************************************************************
*! Example   : = lfDLCHKADJAD()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLCHKADJAD

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  SET ORDER TO TAG WhsLoc IN  WHSLOC
  IF !SEEK(lcWareH ,'WHSLOC')
    lcMsg2 = 'No bin location found for warehouse ' +lcWareH
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    _CUROBJ = OBJNUM(lcWareH)
    SET ORDER TO TAG WhsLocSt IN  WHSLOC
    RETURN
  ENDIF
  SET ORDER TO TAG WhsLocSt IN  WHSLOC

  IF !laScrMode[4]
    laScrMode = .F.
    laScrMode[4] = .T.
    =lfActBrow()
    llCUpdate = .T.
    SHOW GETS   
  ENDIF
ELSE
  IF !EMPTY(lcFromWare)
    laScrMode[1] = .F.
    laScrMode[4] = .T.
    llCUpdate = .T.
    SHOW GETS   
  ENDIF
ENDIF  

*!**************************************************************************
*! Name      : lfALSAVSTY
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/28/2004
*! Purpose   : Save Data to style alocation screen
*!**************************************************************************
*! Example   : = lfALSAVSTY()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfALSAVSTY

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

IF lcPikTkt== "******"
  RETURN
ENDIF

*C126994,1 NNA 05/26/2005 (BEGIN) if Always pick from one Bin Location =Yes then do My Function
IF gfGetMemVar('M_BULKPICK',gcAct_Comp) OR gfGetMemVar('M_PICKONE')   && setting For Always Pick from one Bin Location Yes/No
  = lFALSTYBIN()
  RETURN
ENDIF
*C126994,1 NNA (END)

*IF !(ALIAS()="STYLE")
*  RETURN
*ENDIF
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF

IF SEEK(Style.Style+ StyDye.cWareCode ,'WHBINLOC') 
  SELECT WHBINLOC
  FOR lnCnt = 1 TO 8
    lcCnt = ALLT(STR(lnCnt,2))
    *! C123853,1 MHM 01/15/2005  add function to check on Class level[Start]
    IF !EMPTY(ORDLINE.Pik&lcCnt)
      =lfchkCLas(ORDLINE.Style,ORDLINE.cWareCode,lcCnt)
    ELSE
      LOOP    
    ENDIF
    *! C123853,1 MHM 01/15/2005  [End]
    
    IF WHBINLOC.QTY&lcCnt >= ORDLINE.Pik&lcCnt
      REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+ORDLINE.Pik&lcCnt,0) ,;
              WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +ORDLINE.Pik&lcCnt,0) 
      IF ORDLINE.Pik&lcCnt <> 0        
        =lfGetSTQty(ORDLINE.Pik&lcCnt,lnCnt,lcPikTkT,OrdHdr.Order)
      ENDIF  
    ELSE
      *! C123853,1 MHM 01/15/2005  check net qty[Start]
      *REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+WHBINLOC.QTY&lcCnt,;
              WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +WHBINLOC.QTY&lcCnt
      *lnRem = ORDLINE.Pik&lcCnt - WHBINLOC.QTY&lcCnt        
      lnRem = ORDLINE.Pik&lcCnt - (WHBINLOC.QTY&lcCnt-  WHBINLOC.Alo&lcCnt)      
      REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.QTY&lcCnt,;
              WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo+(WHBINLOC.QTY&lcCnt-  WHBINLOC.Alo&lcCnt)
      *! C123853,1 MHM 01/15/2005  check net qty[End]

      IF ORDLINE.Pik&lcCnt <> 0        
        =lfGetSTQty(WHBINLOC.QTY&lcCnt,lnCnt,lcPikTkT,OrdHdr.Order)
      ENDIF  
      SKIP
      SCAN REST WHILE STYLE+CWARECODE = Style.Style+ StyDye.cWareCode
    
        *! C123853,1 MHM 01/15/2005  add function to check on Class level[Start]
        =lfchkCLas(ORDLINE.Style,ORDLINE.cWareCode,lcCnt)
        *! C123853,1 MHM 01/15/2005  [End]
        
        IF  WHBINLOC.QTY&lcCnt >= lnRem
          REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+lnRem,;
                  WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +lnRem
          IF lnRem <> 0        
            =lfGetSTQty(lnRem,lnCnt,lcPikTkT,OrdHdr.Order)
          ENDIF  
          EXIT        
        ELSE
          *! C123853,1 MHM 01/15/2005  check net qty[Start]
          *REPLACE WHBINLOC.Alo&lcCnt WITH  WHBINLOC.Alo&lcCnt+WHBINLOC.QTY&lcCnt,;
          *        WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +WHBINLOC.QTY&lcCnt
          *lnRem = lnRem - WHBINLOC.QTY&lcCnt        
 
          lnRem = lnRem - (WHBINLOC.QTY&lcCnt-  WHBINLOC.Alo&lcCnt)      
          REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.QTY&lcCnt,;
                  WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +(WHBINLOC.QTY&lcCnt-  WHBINLOC.Alo&lcCnt)
          *! C123853,1 MHM 01/15/2005  check net qty[End]
          
          IF WHBINLOC.QTY&lcCnt <> 0        
            =lfGetSTQty(WHBINLOC.QTY&lcCnt,lnCnt,lcPikTkT,OrdHdr.Order)
          ENDIF  
        ENDIF          
      ENDSCAN 
      =SEEK(Style.Style+ StyDye.cWareCode ,'WHBINLOC')       
    ENDIF  
  ENDFOR

  SELECT WHBINLOC
  FLUSH
  lclocPktkt = IIF(!EMPTY(lcPikTkT),lcPikTkT,OrdLine.PikTkT)
  =lfChkPktk("ORDLINE",lcPikTkT)
ENDIF
  
*!**************************************************************************
*! Name      : lfGetSTQty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/30/2004 
*! Purpose   : GetPikQty
*!**************************************************************************
*! Example   : = lfGetSTQty()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfGetSTQty
PARAMETER lnQty,lnSize,lcPkTkt,lcOrder

lnAlias    = SELECT()
*-- lnQty -------> Qty per size
*-- lnSize -------> size
*-- lcPkTkt -------> PikTkt
*-- lcOrder -------> Order
lcSize = ALLT(STR(lnSize,2))

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF

IF !USED('WHSLOC')
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH') 
ENDIF

IF !EMPTY(STYLE.cPrimClss&lcSize)
  lcClass =  STYLE.cPrimClss&lcSize
ELSE
  IF !EMPTY(STYLE.cSecClss&lcSize)
    lcClass =  STYLE.cSecClss&lcSize
  ELSE
    lcClass =  STYLE.cRemClss&lcSize
  ENDIF
ENDIF

  
IF !SEEK(lcPkTkt+StyDye.cWareCode+WHBINLOC.cLocation+Style.Style ,'PKBINLOC') 
  =SEEK(StyDye.cWareCode+WHBINLOC.cLocation,'WHSLOC')
  SELECT PKBINLOC
  APPEND BLANK
  REPLACE STYLE      WITH Style.Style,;
          CWARECODE  WITH StyDye.cWareCode,;
          PIKTKT     WITH lcPkTkt ,;
          ORDER      WITH lcOrder,;
          cFlatHang  WITH WHSLOC.cFlatHang,;
          clocation  WITH WHBINLOC.cLocation ,;
          Qty&lcSize WITH lnQty,;
          TotQty     WITH lnQty
  REPLACE cBinClass&lcSize  WITH lcClass
  =gfAdd_Info('PKBINLOC')
ELSE
  SELECT PKBINLOC
  REPLACE Qty&lcSize WITH Qty&lcSize+ lnQty,;
          TotQty     WITH TotQty+lnQty
  REPLACE cBinClass&lcSize  WITH lcClass
  IF TotQty = 0
    DELETE
  ENDIF         
ENDIF
SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfALSAVORD
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/31/2002
*! Purpose   : Save Data to Order alocation screen
*!**************************************************************************
*! Example   : = lfALSAVORD()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfALSAVORD

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF
DIMENSION laPikQty[9]
STORE 0 TO laPikQty
lnAlias    = SELECT()

IF m.PikTkt == "******"
  RETURN
ENDIF


*C126994,1 NNA 05/26/2005 (BEGIN) if Always pick from one Bin Location =Yes then do My Function
*--NNA add check to Bulk Pik Too
*IF gfGetMemVar('M_PICKONE')   && setting For Always Pick from one Bin Location Yes/No
IF gfGetMemVar('M_BULKPICK',gcAct_Comp) OR gfGetMemVar('M_PICKONE')   && setting For Always Pick from one Bin Location Yes/No
*--NNA add check to Bulk Pik Too
  = lfAlOrdBin()
  RETURN
ENDIF
*C126994,1 NNA (END)

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF
IF SEEK(Style.Style+ StyDye.cWareCode ,'WHBINLOC') 
  *--NNA (Start)
  
  *--B039782,1 MHM [Start]
  SELECT PKBINLOC
  lcPkBinOrd = ORDER()
  SET ORDER TO PKBINLIN
  *--B039782,1 MHM [End]
  
  SELECT WHBINLOC
  SCAN REST WHILE STYLE+CWARECODE+CLOCATION = Style.Style+ StyDye.cWareCode
    *IF SEEK(&lc_TmpOrdL..PikTkt+&lc_TmpOrdL..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style ,'PKBINLOC') 

    *--mhm126994 add line no to index[Start]
    
    *IF SEEK(IIF(!EMPTY(ALLTRIM(&lc_TmpOrdL..PikTkt)),&lc_TmpOrdL..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),;
       PIKTKT.PIKTKT,ORDLINE.PIKTKT))+&lc_TmpOrdL..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style ,'PKBINLOC') 
    IF SEEK(IIF(!EMPTY(ALLTRIM(&lc_TmpOrdL..PikTkt)),&lc_TmpOrdL..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),;
       PIKTKT.PIKTKT,ORDLINE.PIKTKT))+&lc_TmpOrdL..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style+STR(&lc_TmpOrdL..LineNo,6) ,'PKBINLOC') 
    *--mhm126994 add line no to index [END]

  *--NNA (END)
      FOR lnPkCount =1 To 8
        lcPkCount = ALLT(STR(lnPkCount,2))
        SELECT PKBINLOC
        *--NNA (START)
        *=SEEK(&lc_TmpOrdL..PikTkt+&lc_TmpOrdL..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style) 
        *SCAN REST WHILE piktkt+cwarecode+clocation+style = &lc_TmpOrdL..PikTkt+&lc_TmpOrdL..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style
          *laPikQty[lnPkCount] =laPikQty[lnPkCount]+ PKBINLOC.QTY&lcPkCount
        =SEEK(IIF(!EMPTY(ALLTRIM(&lc_TmpOrdL..PikTkt)),&lc_TmpOrdL..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),;
             PIKTKT.PIKTKT,ORDLINE.PIKTKT))+&lc_TmpOrdL..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style) 
        SCAN REST WHILE piktkt+cwarecode+clocation+style = ;
             IIF(!EMPTY(ALLTRIM(&lc_TmpOrdL..PikTkt)),&lc_TmpOrdL..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),;
             PIKTKT.PIKTKT,ORDLINE.PIKTKT))+&lc_TmpOrdL..cWareCode FOR STYLE = &lc_TmpOrdL..Style
          laPikQty[lnPkCount] =laPikQty[lnPkCount]+ PKBINLOC.QTY&lcPkCount
        *--NNA (END)
        ENDSCAN  
      ENDFOR
      laPikQty[9]= laPikQty[1]+laPikQty[2]+laPikQty[3]+laPikQty[4]+laPikQty[5]+laPikQty[6]+laPikQty[7]+laPikQty[8]
    ENDIF  
  ENDSCAN
  
  *! C123853,1 MHM 01/15/2005  initialize variable to include net qty[Start]
  lnNetQty = 0
  *! C123853,1 MHM 01/15/2005  [End]
  *IF (&lc_TmpOrdL..TotPik - laPikQty[9]) < 0
  IF &lc_TmpOrdL..TotPik <> laPikQty[9]
  *--mhm
      SELECT PKBINLOC
      lcPkOrd = ORDER()
      SET ORDER TO Pkbinpkt
      SEEK(OrdLine.PikTkT+OrdLine.cwarecode)
      *--NNA
      *SCAN REST WHILE piktkt+cwarecode+clocation+style = OrdLine.PikTkT + OrdLine.cwarecode
      SCAN REST WHILE piktkt+cwarecode+clocation+style = OrdLine.PikTkT + OrdLine.cwarecode FOR STYLE = OrdLine.Style
        IF  LineNo <> OrdLine.LineNo
          LOOP
        ENDIF
        IF SEEK(style+cwarecode+clocation ,'WHBINLOC')
          REPLACE WHBINLOC.alo1 WITH MAX(WHBINLOC.alo1 - PKBINLOC.Qty1,0);
                  WHBINLOC.alo2 WITH MAX(WHBINLOC.alo2 - PKBINLOC.Qty2,0);
                  WHBINLOC.alo3 WITH MAX(WHBINLOC.alo3 - PKBINLOC.Qty3,0);
                  WHBINLOC.alo4 WITH MAX(WHBINLOC.alo4 - PKBINLOC.Qty4,0);
                  WHBINLOC.alo5 WITH MAX(WHBINLOC.alo5 - PKBINLOC.Qty5,0);
                  WHBINLOC.alo6 WITH MAX(WHBINLOC.alo6 - PKBINLOC.Qty6,0);
                  WHBINLOC.alo7 WITH MAX(WHBINLOC.alo7 - PKBINLOC.Qty7,0);
                  WHBINLOC.alo8 WITH MAX(WHBINLOC.alo8 - PKBINLOC.Qty8,0);
                  WHBINLOC.Totalo WITH MAX(WHBINLOC.Totalo - PKBINLOC.TotQty,0)
        ENDIF
        DELETE
      ENDSCAN
      SELECT PKBINLOC
      SET ORDER TO &lcPkOrd
    STORE 0 TO laPikQty
  ENDIF

  *--mhm126994
  SELECT PKBINLOC
  SET ORDER TO &lcPkBinOrd
  *--mhm126994
  
  SELECT WHBINLOC
  FOR lnCnt = 1 TO 8

    lcCnt = ALLT(STR(lnCnt,2))
    *! C123853,1 MHM 01/15/2005  add function to check on Class level[Start]
    IF !EMPTY(&lc_TmpOrdL..Pik&lcCnt)
      =lfchkCLas(&lc_TmpOrdL..Style,&lc_TmpOrdL..cWareCode,lcCnt)
    ELSE
      LOOP    
    ENDIF
    *! C123853,1 MHM 01/15/2005 [Start]

    IF (&lc_TmpOrdL..Pik&lcCnt - laPikQty[lnCnt]) >0

      *! C123853,1 MHM 01/15/2005  check Qty- ALO [Start]
      lnNetQty = (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt)
      *IF WHBINLOC.QTY&lcCnt >= (&lc_TmpOrdL..Pik&lcCnt - laPikQty[lnCnt])
      IF lnNetQty >= (&lc_TmpOrdL..Pik&lcCnt - laPikQty[lnCnt])
      *! C123853,1 MHM 01/15/2005  [End]
      
        REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+ (&lc_TmpOrdL..Pik&lcCnt- laPikQty[lnCnt]),0) ,;
                WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +(&lc_TmpOrdL..Pik&lcCnt- laPikQty[lnCnt]),0)
              
        *--mhm126994 Add Line no to parameters[Start]
        *=lfGetORQty((&lc_TmpOrdL..Pik&lcCnt - laPikQty[lnCnt]),lnCnt,m.PikTkT,OrdHdr.Order)
        =lfGetORQty((&lc_TmpOrdL..Pik&lcCnt - laPikQty[lnCnt]),lnCnt,m.PikTkT,OrdHdr.Order,&lc_TmpOrdL..lineno)
        *--mhm126994 Add Line no to parameters [End]
      ELSE

        *! C123853,1 MHM 01/15/2005  check net qty[Start]
        *REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+WHBINLOC.QTY&lcCnt,;
                WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +WHBINLOC.QTY&lcCnt
        *lnRem =(&lc_TmpOrdL..Pik&lcCnt- laPikQty[lnCnt]) - WHBINLOC.QTY&lcCnt        
        *=lfGetORQty(WHBINLOC.QTY&lcCnt,lnCnt,m.PikTkT,OrdHdr.Order)
        REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+lnNetQty ,;
                WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo + lnNetQty 
        lnRem =(&lc_TmpOrdL..Pik&lcCnt- laPikQty[lnCnt]) - lnNetQty 
        *! C123853,1 MHM 01/15/2005  [End]
        
        *--mhm126994
        *=lfGetORQty(lnNetQty,lnCnt,m.PikTkT,OrdHdr.Order)
        =lfGetORQty(lnNetQty,lnCnt,m.PikTkT,OrdHdr.Order,&lc_TmpOrdL..lineno)
        *--mhm126994
        SKIP
        SCAN REST WHILE STYLE+CWARECODE = Style.Style+ StyDye.cWareCode

          *! C123853,1 MHM 01/15/2005  Function to check class[Start]
          *IF  WHBINLOC.QTY&lcCnt >= lnRem
          =lfchkCLas(&lc_TmpOrdL..Style,&lc_TmpOrdL..cWareCode,lcCnt)
          lnNetQty = (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt)
          IF  lnNetQty >= lnRem
          *! C123853,1 MHM 01/15/2005  check net qty[Start]
          
            REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+lnRem,;
                    WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +lnRem

            *--mhm126994 add line no
            *=lfGetORQty(lnRem,lnCnt,m.PikTkT,OrdHdr.Order)
            =lfGetORQty(lnRem,lnCnt,m.PikTkT,OrdHdr.Order,&lc_TmpOrdL..lineno)
            *--mhm126994 add line no
            
            EXIT        
          ELSE
            
            *! C123853,1 MHM 01/15/2005  Check Net Qty[Start]
            *REPLACE WHBINLOC.Alo&lcCnt WITH  WHBINLOC.Alo&lcCnt+WHBINLOC.QTY&lcCnt,;
                    WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +WHBINLOC.QTY&lcCnt
            *lnRem = lnRem - WHBINLOC.QTY&lcCnt        
            *=lfGetORQty(WHBINLOC.QTY&lcCnt,lnCnt,m.PikTkT,OrdHdr.Order)
            REPLACE WHBINLOC.Alo&lcCnt WITH  WHBINLOC.Alo&lcCnt+lnNetQty,;
                    WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +lnNetQty
            lnRem = lnRem - lnNetQty
            
            *--mhm126994 add line no 
            *=lfGetORQty(lnNetQty,lnCnt,m.PikTkT,OrdHdr.Order)
            =lfGetORQty(lnNetQty,lnCnt,m.PikTkT,OrdHdr.Order,&lc_TmpOrdL..lineno)
            *--mhm126994 add line no 
            *! C123853,1 MHM 01/15/2005  [End]
           
          ENDIF          
        ENDSCAN 
        =SEEK(Style.Style+ StyDye.cWareCode ,'WHBINLOC')       
      ENDIF  
    ELSE
      *--mhm2005
      *IF WHBINLOC.QTY&lcCnt >= ABS(&lc_TmpOrdL..Pik&lcCnt - laPikQty[lnCnt])
      *  IF (&lc_TmpOrdL..Pik&lcCnt - laPikQty[lnCnt])<>0
      *    REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+ (&lc_TmpOrdL..Pik&lcCnt- laPikQty[lnCnt]),0) ,;
      *            WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo + (&lc_TmpOrdL..Pik&lcCnt- laPikQty[lnCnt]),0)
      *    =lfGetORQty((&lc_TmpOrdL..Pik&lcCnt- laPikQty[lnCnt]),lnCnt,OrdLine.PikTkT,OrdLine.Order)
      *  ENDIF  
      *ELSE
      *  lnCurrQty = -WHBINLOC.Alo&lcCnt
      *  REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnCurrQty,0),;
      *          WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnCurrQty,0)
      *        
      *  lnRem =(&lc_TmpOrdL..Pik&lcCnt - ORDLINE.Pik&lcCnt) - lnCurrQty
      *  =lfGetORQty(lnCurrQty,lnCnt,OrdLine.PikTkT,OrdLine.Order)
      *  SKIP
      *  SCAN REST WHILE STYLE+CWARECODE = OrdLine.Style+ OrdLine.cWareCode

      *   *! C123853,1 MHM 01/15/2005  Function to check class[Start]
      *   =lfchkCLas(&lc_TmpOrdL..Style,&lc_TmpOrdL..cWareCode,lcCnt)
      *    *! C123853,1 MHM 01/15/2005  [End]
      *    
      *    IF  WHBINLOC.QTY&lcCnt >= ABS(lnRem)
      *      REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnRem,0),;
      *              WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnRem,0)
      *      =lfGetORQty(lnRem,lnCnt,OrdLine.PikTkT,OrdLine.Order)
      *      EXIT        
      *    ELSE
      *      lnCurrQty = -WHBINLOC.Alo&lcCnt
      *      REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnCurrQty,0),;
      *              WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnCurrQty,0)
      *      lnRem = lnRem - lnCurrQty 
      *      =lfGetORQty(lnCurrQty,lnCnt,OrdLine.PikTkT,OrdLine.Order)
      *    ENDIF          
      *  ENDSCAN 
      *  =SEEK(OrdLine.Style+ OrdLine.cWareCode ,'WHBINLOC')       
      *ENDIF  
      *--mhm2005
    ENDIF
  ENDFOR
SELECT WHBINLOC
FLUSH
lclocPktkt = IIF(!EMPTY(m.piktkt),m.piktkt,OrdLine.PikTkT)
=lfChkPktk(lc_TmpOrdL,m.piktkt)
ENDIF
*!**************************************************************************
*! Name      : lfGetORQty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2002
*! Purpose   : Get Ordered Qty
*!**************************************************************************
*! Example   : = lfGetORQty()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfGetORQty
PARAMETER lnQty,lnSize,lcPkTkt,lcOrder,lnLineno

lnAlias    = SELECT()
*-- lnQty -------> Qty per size
*-- lnSize -------> size
*-- lcPkTkt -------> PikTkt
*-- lcOrder -------> Order
lcSize = ALLT(STR(lnSize,2))
IF lnQty = 0 
  SELECT(lnAlias)
  RETURN
ENDIF

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF

IF !USED('WHSLOC')
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH') 
ENDIF

IF !EMPTY(STYLE.cPrimClss&lcSize)
  lcClass =  STYLE.cPrimClss&lcSize
ELSE
  IF !EMPTY(STYLE.cSecClss&lcSize)
    lcClass =  STYLE.cSecClss&lcSize
  ELSE
    lcClass =  STYLE.cRemClss&lcSize
  ENDIF
ENDIF

*--MAX(lnCHold&Z+(Qty&Z*IIF(TranCd='1',1,-1)),0)  
*--mhm126994 Cheange order of pkbinloc
*IF !SEEK(lcPkTkt+WHBINLOC.cWareCode+WHBINLOC.cLocation+WHBINLOC.Style,'PKBINLOC') 
SELECT PKBINLOC
lcPkBinOrd = ORDER()
SET ORDER TO PKBINLIN
IF !SEEK(lcPkTkt+WHBINLOC.cWareCode+WHBINLOC.cLocation+WHBINLOC.Style+STR(lnLineno,6) ,'PKBINLOC') 
*--mhm126994

  =SEEK(StyDye.cWareCode+WHBINLOC.cLocation,'WHSLOC')
  SELECT PKBINLOC
  APPEND BLANK
  REPLACE STYLE      WITH WHBINLOC.Style,;
          CWARECODE  WITH WHBINLOC.cWareCode,;
          PIKTKT     WITH lcPkTkt ,;
          ORDER      WITH lcOrder,;
          cFlatHang  WITH WHSLOC.cFlatHang,;
          clocation  WITH WHBINLOC.cLocation ,;
          Qty&lcSize WITH MAX(Qty&lcSize +lnQty,0),;
          TotQty     WITH MAX(TotQty +lnQty,0)
  REPLACE cBinClass&lcSize  WITH lcClass
  
  *--mhm126994 add line no [Start]
  REPLACE Lineno  WITH lnLineno
  *--mhm126994 Add line no [End]

  IF TotQty = 0
    DELETE
  ENDIF         
  =gfAdd_Info('PKBINLOC')
ELSE
  SELECT PKBINLOC
  REPLACE Qty&lcSize WITH MAX(Qty&lcSize +lnQty,0),;
          TotQty     WITH MAX(TotQty     +lnQty,0)
  REPLACE cBinClass&lcSize  WITH lcClass
  IF TotQty = 0
    DELETE
  ENDIF         
ENDIF

*--mhm126994 Cheange order of pkbinloc
SELECT PKBINLOC
SET ORDER TO &lcPkBinOrd
*--mhm126994

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfALSAVAUT
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/17/2004
*! Purpose   : Save Data to Automatic alocation screen
*!**************************************************************************
*! Example   : = lfALSAVAUT()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfALSAVAUT

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin 
  RETURN
ENDIF

DIMENSION laPikQty[9]
STORE 0 TO laPikQty
lnAlias    = SELECT()

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
SELECT WHBINLOC
lcOldOrder = ORDER()
SEt ORDER TO WHBINLST

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF

*C126994,1 NNA 05/26/2005 (BEGIN) if Always pick from one Bin Location =Yes then do My Function
IF gfGetMemVar('M_BULKPICK',gcAct_Comp) OR gfGetMemVar('M_PICKONE')   && setting For Always Pick from one Bin Location Yes/No
  = lfAlAutBin()
  RETURN
ENDIF
*C126994,1 NNA (END)

IF SEEK(&lcTmpOrdLn..Style+ &lcTmpOrdLn..cWareCode ,'WHBINLOC') 
  *--NNA
  SCAN REST WHILE STYLE+CWARECODE+CLOCATION = &lcTmpOrdLn..Style+ &lcTmpOrdLn..cWareCode
  *--NNA
    IF SEEK(&lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style ,'PKBINLOC') 
      FOR lnPkCount =1 To 8
        lcPkCount = ALLT(STR(lnPkCount,2))
        SELECT PKBINLOC
        =SEEK(&lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style) 
        SCAN REST WHILE piktkt+cwarecode+clocation+style = &lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode+WHBINLOC.cLocation+WHBINLOC.Style
          laPikQty[lnPkCount] =laPikQty[lnPkCount]+ PKBINLOC.QTY&lcPkCount
        ENDSCAN  
      ENDFOR
      laPikQty[9]= laPikQty[1]+laPikQty[2]+laPikQty[3]+laPikQty[4]+laPikQty[5]+laPikQty[6]+laPikQty[7]+laPikQty[8]
    ENDIF  
  ENDSCAN
  *! C123853,1 MHM 01/15/2005  Initiate variable to get QTY-ALO [Start]
  lnNetQty = 0
  *! C123853,1 MHM 01/15/2005  [End]

  SELECT WHBINLOC
  FOR lnCnt = 1 TO 8
    lcCnt = ALLT(STR(lnCnt,2))
    
    *! C123853,1 MHM 01/15/2005  Function to check class[Start]
    IF !EMPTY(&lcTmpOrdLn..Pik&lcCnt)
      =lfchkCLas(&lcTmpOrdLn..Style,&lcTmpOrdLn..cWareCode,lcCnt)
    ELSE
      LOOP    
    ENDIF  
    *IF WHBINLOC.QTY&lcCnt >= (&lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt])
    lnNetQty = (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt)
    IF lnNetQty >= (&lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt])
    *! C123853,1 MHM 01/15/2005  [End]
    
      REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+ (&lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt]),;
              WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo + (&lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt])
              
      *--mhm126994 Add line no
      *=lfGetORQty((&lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt]),lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order)
      =lfGetORQty((&lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt]),lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order,&lcTmpOrdLn..Lineno)
      *--mhm126994
    ELSE

      *! C123853,1 MHM 01/15/2005  use net QTY variable [Start]
      *REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+WHBINLOC.QTY&lcCnt,;
              WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +WHBINLOC.QTY&lcCnt
      *lnRem = (&lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt]) - WHBINLOC.QTY&lcCnt        
      *=lfGetORQty(WHBINLOC.QTY&lcCnt,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order)

      REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+lnNetQty ,;
              WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +lnNetQty 
      lnRem = (&lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt]) - lnNetQty 

      *--mhm126994 add line no
      *=lfGetORQty(lnNetQty ,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order)
      =lfGetORQty(lnNetQty ,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order,&lcTmpOrdLn..Lineno)
      *--mhm126994

      *! C123853,1 MHM 01/15/2005  [End]
      
      SKIP
      SCAN REST WHILE STYLE+CWARECODE = &lcTmpOrdLn..Style+ &lcTmpOrdLn..cWareCode
        *! C123853,1 MHM 01/15/2005  Function to check class[Start]
        =lfchkCLas(&lcTmpOrdLn..Style,&lcTmpOrdLn..cWareCode,lcCnt)
        *--mhm123853,Al
        *IF  WHBINLOC.QTY&lcCnt >= lnRem
        lnNetQty = (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt)
        IF  lnNetQty >= lnRem
        *! C123853,1 MHM 01/15/2005  Function to check class[Start]

          REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+lnRem,;
                  WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +lnRem

          *--mhm126994 Add line no
          *=lfGetORQty(lnRem,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order)
          =lfGetORQty(lnRem,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order,&lcTmpOrdLn..Lineno)
          *--mhm126994
          
          EXIT        
        ELSE

          *! C123853,1 MHM 01/15/2005  check Net Qty [Start]
          *REPLACE WHBINLOC.Alo&lcCnt WITH  WHBINLOC.Alo&lcCnt+WHBINLOC.QTY&lcCnt,;
          *        WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +WHBINLOC.QTY&lcCnt
          *lnRem = lnRem - WHBINLOC.QTY&lcCnt        
          *=lfGetORQty(WHBINLOC.QTY&lcCnt,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order)
          REPLACE WHBINLOC.Alo&lcCnt WITH  WHBINLOC.Alo&lcCnt+lnNetQty ,;
                  WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +lnNetQty 
          lnRem = lnRem - lnNetQty 

          *--mhm126994 Add line no
          *=lfGetORQty(lnNetQty ,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order)
          =lfGetORQty(lnNetQty ,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order,&lcTmpOrdLn..LineNo)
          *--mhm126994 Add line no
          
          *! C123853,1 MHM 01/15/2005  [End]

        ENDIF          
      ENDSCAN 
      =SEEK(&lcTmpOrdLn..Style+ &lcTmpOrdLn..cWareCode ,'WHBINLOC')       
    ENDIF  
  ENDFOR
ENDIF

SELECT WHBINLOC
SET ORDER TO &lcOldOrder
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfALRELORD
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/1/2004
*! Purpose   : Release Data from Order alocation screen
*!**************************************************************************
*! Example   : = lfALRELORD()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfALRELORD
*B130690,1  TMI [Start] Add a new parameter 
PARAMETERS llFlag
*B130690,1  TMI [End  ] 

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

*B130690,1  TMI [Start] continue releasing when the flag is set .t. only when the in same piktkt
IF llFlag .AND. ORDLINE.PIKTKT <> PADR( M.PIKTKT , 6 )
  RETURN
ENDIF
*B130690,1  TMI [End  ] 

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
IF ORDLINE.PikTkt == "******"
  RETURN
ENDIF

*! C123853,1 MHM 01/15/2005  Change the way of releasing[Start]

*IF SEEK(ORDLINE.Style+ ORDLINE.cWareCode ,'WHBINLOC') 
*  SELECT WHBINLOC
*  FOR lnCnt = 1 TO 8
*    lcCnt = ALLT(STR(lnCnt,2))
*    IF !EMPTY(ORDLINE.Pik&lcCnt)
*     IF !lfchkCLas(ORDLINE.Style,ORDLINE.cWareCode,lcCnt)
*       LOOP
*     ENDIF
*      
*    ENDIF  
*    IF WHBINLOC.Alo&lcCnt >=  ORDLINE.Pik&lcCnt
*      REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+ (-ORDLINE.Pik&lcCnt),0) ,;
*              WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo + (- ORDLINE.Pik&lcCnt),0)
*      IF ORDLINE.Pik&lcCnt <>0
*        =lfGetORQty((-ORDLINE.Pik&lcCnt),lnCnt,ORDLINE.PikTkT,ORDLINE.Order)
*      ENDIF
*    ELSE
*      lnCurrQty = WHBINLOC.Alo&lcCnt
*      REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt- lnCurrQty,0),;
*              WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo- lnCurrQty,0)
*              
*      lnRem = ORDLINE.Pik&lcCnt - lnCurrQty        
*      =lfGetORQty(-lnCurrQty,lnCnt,ORDLINE.PikTkT,ORDLINE.Order)
*      SKIP
*      SCAN REST WHILE STYLE+CWARECODE = ORDLINE.Style+ ORDLINE.cWareCode 
*        *--mhm123853,Al
*        =lfchkCLas(ORDLINE.Style,ORDLINE.cWareCode,lcCnt)
*        *--mhm123853,Al
*        IF  WHBINLOC.Alo&lcCnt >= lnRem
*          REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt-lnRem,0),;
*                  WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo -lnRem,0)
*          =lfGetORQty(-lnRem,lnCnt,ORDLINE.PikTkT,ORDLINE.Order)
*          EXIT        
*        ELSE
*          lnCurrQty = WHBINLOC.Alo&lcCnt
*          REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt-lnCurrQty,0),;
*                  WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo -lnCurrQty,0)
*          lnRem = lnRem - lnCurrQty
*          =lfGetORQty(-lnCurrQty,lnCnt,ORDLINE.PikTkT,ORDLINE.Order)
*        ENDIF          
*      ENDSCAN 
*      =SEEK(ORDLINE.Style+ ORDLINE.cWareCode ,'WHBINLOC')       
*    ENDIF  
*  ENDFOR
*ENDIF
IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF
SELECT WHBINLOC
lcWHBOrd = ORDER()
SET ORDER TO WHBINLST

SELECT PKBINLOC
lcPikOrd = ORDER()
*--NNA && SET ORDER TO TAG PKBINLIN = (piktkt+cwarecode+clocation+style+STR(lineno,6))
*B130690,1  TMI [Start] initialize "llLineIndx" with .F. to not need for the else part 
*FOR I = 1 TO 5
llLineIndx = .F.
FOR I = 1 TO 255
  IF !EMPTY(TAG(I))
    IF TAG(I) = 'PKBINLIN'
      SET ORDER TO PKBINLIN
      llLineIndx = .T.
      *B130690,1  TMI [Start] Add Exit and comment remaining lines
      EXIT
    *ELSE
      *SET ORDER TO PKBINPKT
      *llLineIndx = .F.
      *B130690,1  TMI [End  ] 
    ENDIF 
    *B130690,1  TMI [Start] close the if statement
  ELSE
    EXIT
  ENDIF
  *B130690,1  TMI [End  ] 
ENDFOR
*--NNA

IF SEEK(ORDLINE.PikTkt + ORDLINE.cWareCode)

  *--NNA LOOP for the lineno also
  lcForExp = ''
  IF llLineIndx
    lcForExp = 'FOR LINENO=ORDLINE.LINENO'
  ENDIF
  *SCAN REST WHILE piktkt+cwarecode+clocation+style = ORDLINE.PikTkt + ORDLINE.cWareCode
  SCAN REST WHILE piktkt+cwarecode+clocation+style = ORDLINE.PikTkt + ORDLINE.cWareCode &lcForExp
  *--NNA

    IF STYLE <> ORDLINE.Style
      LOOP
    ENDIF
    IF SEEK(ORDLINE.Style+ ORDLINE.cWareCode+PKBINLOC.clocation ,'WHBINLOC') 
      SELECT WHBINLOC
      FOR lnCnt = 1 TO 8
        lcCnt = ALLT(STR(lnCnt,2))
        REPLACE WHBINLOC.ALO&lcCnt WITH MAX(WHBINLOC.ALO&lcCnt - PKBINLOC.Qty&lcCnt,0),;
                WHBINLOC.TotALO    WITH MAX(WHBINLOC.TotALO - PKBINLOC.Qty&lcCnt,0)
      ENDFOR  
    ENDIF
  ENDSCAN
  SELECT PKBINLOC 
  *--NNA  && Add the Lineno
  *DELETE ALL FOR (piktkt+cwarecode+clocation+style = ORDLINE.PikTkt + ORDLINE.cWareCode) AND (STYLE = ORDLINE.Style)
  lcDelExp = ''
  IF llLineIndx
    lcDelExp = 'AND LINENO = ORDLINE.LINENO'
  ENDIF
  DELETE ALL FOR (piktkt+cwarecode+clocation+style = ORDLINE.PikTkt + ORDLINE.cWareCode) AND (STYLE = ORDLINE.Style) &lcDelExp
  *--NNA  
ENDIF

SET ORDER TO &lcPikOrd
SELECT WHBINLOC
SET ORDER TO &lcWHBOrd 
*--MHM2005
FLUSH
*--MHM2005

*! C123853,1 MHM 01/15/2005  [End]

*!**************************************************************************
*! Name      : lfALRELAUT
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/02/2004
*! Purpose   : Release Data to Automatic alocation screen
*!**************************************************************************
*! Example   : = lfALRELAUT()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfALRELAUT

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
 
*--mhm2005
*SELECT WHBINLOC
*lcOldOrder = ORDER()
*SET ORDER TO WHBINLST
*IF SEEK(&lcTmpOrdLn..Style+ &lcTmpOrdLn..cWareCode ,'WHBINLOC') 
*  SELECT WHBINLOC
*  FOR lnCnt = 1 TO 8
*    lcCnt = ALLT(STR(lnCnt,2))

*    *! C123853,1 MHM 01/15/2005  Function to check class[Start]
*    IF !EMPTY(&lcTmpOrdLn..Pik&lcCnt)
*     IF !lfchkCLas(&lcTmpOrdLn..Style,&lcTmpOrdLn..cWareCode ,lcCnt)
*       LOOP
*     ENDIF
*    ENDIF  
**    *! C123853,1 MHM 01/15/2005  [End]

*    IF WHBINLOC.QTY&lcCnt >= &lcTmpOrdLn..Pik&lcCnt 
*      IF &lcTmpOrdLn..Pik&lcCnt  <>0
*        REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt- &lcTmpOrdLn..Pik&lcCnt,0) ,;
*                WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo - &lcTmpOrdLn..Pik&lcCnt,0)
*        =lfGetORQty(-&lcTmpOrdLn..Pik&lcCnt,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order)
*      ENDIF  
*    ELSE
*      lnCurrQty = -WHBINLOC.Alo&lcCnt
*      REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnCurrQty,0) ,;
*              WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnCurrQty ,0)
*              
*      lnRem =&lcTmpOrdLn..Pik&lcCnt  + lnCurrQty         
*      =lfGetORQty(lnCurrQty,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order)
*      SKIP
*      SCAN REST WHILE STYLE+CWARECODE = &lcTmpOrdLn..Style+ &lcTmpOrdLn..cWareCode
*
*        *! C123853,1 MHM 01/15/2005  Function to check class[Start]
*        IF !lfchkCLas(&lcTmpOrdLn..Style,&lcTmpOrdLn..cWareCode ,lcCnt)
*         LOOP
*        ENDIF
*        *! C123853,1 MHM 01/15/2005  [End]
*          
*        IF  WHBINLOC.QTY&lcCnt >= lnRem
*          REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt-lnRem,0),;
*                  WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo -lnRem,0)
*          =lfGetORQty(-lnRem,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order)
*          EXIT        
*        ELSE
*          lnCurrQty = -WHBINLOC.Alo&lcCnt
*          REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnCurrQty,0),;
*                  WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnCurrQty,0)
*          lnRem = lnRem + lnCurrQty
*          =lfGetORQty(lnCurrQty,lnCnt,&lcTmpOrdLn..PikTkT,&lcTmpOrdLn..Order)
*        ENDIF          
*      ENDSCAN 
*      =SEEK(&lcTmpOrdLn..Style+ &lcTmpOrdLn..cWareCode ,'WHBINLOC')       
*    ENDIF  
*  ENDFOR
*ENDIF
*SELECT WHBINLOC
*SET ORDER TO &lcOldOrder

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF
SELECT WHBINLOC
lcWHBOrd = ORDER()
SET ORDER TO WHBINLST

SELECT PKBINLOC
lcPikOrd = ORDER()
SET ORDER TO PKBINPKT
*B130690,1  TMI [Start] use "lcTmpOrdLn" instead on "Ordline" in the next line
*IF SEEK(ORDLINE.PikTkt + &lcTmpOrdLn..cWareCode)
IF SEEK(&lcTmpOrdLn..PikTkt + &lcTmpOrdLn..cWareCode)
  *B130690,1  TMI [End  ] 
  SCAN REST WHILE piktkt+cwarecode+clocation+style = &lcTmpOrdLn..PikTkt + &lcTmpOrdLn..cWareCode
    IF STYLE <> &lcTmpOrdLn..Style
      LOOP
    ENDIF
    IF SEEK(&lcTmpOrdLn..Style+ &lcTmpOrdLn..cWareCode+PKBINLOC.clocation ,'WHBINLOC') 
      SELECT WHBINLOC
      FOR lnCnt = 1 TO 8
        lcCnt = ALLT(STR(lnCnt,2))
        REPLACE WHBINLOC.ALO&lcCnt WITH MAX(WHBINLOC.ALO&lcCnt - PKBINLOC.Qty&lcCnt,0),;
                WHBINLOC.TotALO    WITH MAX(WHBINLOC.TotALO - PKBINLOC.Qty&lcCnt,0)
      ENDFOR  
    ENDIF
  ENDSCAN
  SELECT PKBINLOC 
  DELETE ALL FOR (piktkt+cwarecode+clocation+style = &lcTmpOrdLn..PikTkt + &lcTmpOrdLn..cWareCode) AND (STYLE = &lcTmpOrdLn..Style)
ENDIF

SET ORDER TO &lcPikOrd
SELECT WHBINLOC
SET ORDER TO &lcWHBOrd 

FLUSH
*--MHM2005

*!**************************************************************************
*! Name      : lfALSAVPKT
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/02/2004
*! Purpose   : Save Data to PIKTKT screen
*!**************************************************************************
*! Example   : = lfALSAVPKT()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfALSAVPKT

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF

IF SEEK(OrdLine.Style+ OrdLine.cWareCode ,'WHBINLOC') 

  *! C123853,1 MHM 01/15/2005  Variable to intiate QTY-ALO [Start]
  lnNetQty = 0
  *! C123853,1 MHM 01/15/2005  [End]
  *--mhm2005
  IF (m.TotPik - (m.OldPik1+m.OldPik2+m.OldPik3+m.OldPik4+m.OldPik5+m.OldPik6+m.OldPik7+m.OldPik8)) < 0
      IF !USED('PKBINLOC')
        =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
      ENDIF

      SELECT PKBINLOC
      lcPkOrd = ORDER()
      SET ORDER TO Pkbinpkt
      SEEK(OrdLine.PikTkT+OrdLine.cwarecode)
      SCAN REST WHILE piktkt+cwarecode+clocation+style = OrdLine.PikTkT + OrdLine.cwarecode
        IF  style <> OrdLine.Style
          LOOP
        ENDIF
        IF SEEK(style+cwarecode+clocation ,'WHBINLOC')
          REPLACE WHBINLOC.alo1 WITH MAX(WHBINLOC.alo1 - PKBINLOC.Qty1,0);
                  WHBINLOC.alo2 WITH MAX(WHBINLOC.alo2 - PKBINLOC.Qty2,0);
                  WHBINLOC.alo3 WITH MAX(WHBINLOC.alo3 - PKBINLOC.Qty3,0);
                  WHBINLOC.alo4 WITH MAX(WHBINLOC.alo4 - PKBINLOC.Qty4,0);
                  WHBINLOC.alo5 WITH MAX(WHBINLOC.alo5 - PKBINLOC.Qty5,0);
                  WHBINLOC.alo6 WITH MAX(WHBINLOC.alo6 - PKBINLOC.Qty6,0);
                  WHBINLOC.alo7 WITH MAX(WHBINLOC.alo7 - PKBINLOC.Qty7,0);
                  WHBINLOC.alo8 WITH MAX(WHBINLOC.alo8 - PKBINLOC.Qty8,0);
                  WHBINLOC.Totalo WITH MAX(WHBINLOC.Totalo - PKBINLOC.TotQty,0)
        ENDIF
        DELETE
      ENDSCAN
       
      SELECT PKBINLOC
      SET ORDER TO &lcPkOrd
  ENDIF
  *--mhm2005  
  
  
  SELECT WHBINLOC
  FOR lnCnt = 1 TO 8
    lcCnt = ALLT(STR(lnCnt,2))

    *! C123853,1 MHM 01/15/2005  Function to check class[Start]
    IF !EMPTY(m.Pik&lcCnt)
      =lfchkCLas(OrdLine.Style,OrdLine.cWareCode ,lcCnt)
    ENDIF  
    lnNetQty = (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt)
    *! C123853,1 MHM 01/15/2005  [End]

    *--in case of increase Qty
    *--MHM2005
    *IF (m.Pik&lcCnt - m.OldPik&lcCnt) >0
    IF m.Pik&lcCnt  >0
    *-MHM2005

      *! C123853,1 MHM 01/15/2005  Use net qty variable[Start]
      *IF WHBINLOC.QTY&lcCnt >= ABS(m.Pik&lcCnt - m.OldPik&lcCnt)
      IF lnNetQty >= ABS(m.Pik&lcCnt - m.OldPik&lcCnt)
      *! C123853,1 MHM 01/15/2005  Function to check class[Start]
      
        REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+ (m.Pik&lcCnt - m.OldPik&lcCnt),0) ,;
                WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo + (m.Pik&lcCnt - m.OldPik&lcCnt),0)
              
        *--mhm126994 add line No
        *=lfGetORQty((m.Pik&lcCnt - m.OldPik&lcCnt),lnCnt,OrdLine.PikTkT,OrdLine.Order)
        =lfGetORQty((m.Pik&lcCnt - m.OldPik&lcCnt),lnCnt,OrdLine.PikTkT,OrdLine.Order,OrdLine.LineNo)
        *--mhm126994
      ELSE

        *! C123853,1 MHM 01/15/2005  check net qty[Start]
        *REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+WHBINLOC.QTY&lcCnt,;
        *        WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +WHBINLOC.QTY&lcCnt
        *lnRem =(m.Pik&lcCnt - m.OldPik&lcCnt) - WHBINLOC.QTY&lcCnt        
        *=lfGetORQty(WHBINLOC.QTY&lcCnt,lnCnt,OrdLine.PikTkT,OrdLine.Order)

        REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt+lnNetQty ,;
                WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +lnNetQty 
        lnRem =(m.Pik&lcCnt - m.OldPik&lcCnt) - lnNetQty         

        *--mhm126994 add line no
        *=lfGetORQty(lnNetQty ,lnCnt,OrdLine.PikTkT,OrdLine.Order)
        =lfGetORQty(lnNetQty ,lnCnt,OrdLine.PikTkT,OrdLine.Order,OrdLine.LineNo)
        *--mhm126994 add line no

        *! C123853,1 MHM 01/15/2005  [End]
        
        SKIP
        SCAN REST WHILE STYLE+CWARECODE = OrdLine.Style+ OrdLine.cWareCode

          *! C123853,1 MHM 01/15/2005  Function to check class[Start]
          =lfchkCLas(OrdLine.Style,OrdLine.cWareCode ,lcCnt)
          *IF  WHBINLOC.QTY&lcCnt >= ABS(lnRem)
          lnNetQty = (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt)
          IF  lnNetQty >= ABS(lnRem)
          *! C123853,1 MHM 01/15/2005  [End]
          
            REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnRem,0),;
                    WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnRem,0)
            
            *--mhm126994 add line no
            *=lfGetORQty(lnRem,lnCnt,OrdLine.PikTkT,OrdLine.Order)
            =lfGetORQty(lnRem,lnCnt,OrdLine.PikTkT,OrdLine.Order,OrdLine.LineNo)
            *--mhm126994 add line no
            
            EXIT        
          ELSE

            *! C123853,1 MHM 01/15/2005  replace net qty variable [Start]
            *REPLACE WHBINLOC.Alo&lcCnt WITH  WHBINLOC.Alo&lcCnt+WHBINLOC.QTY&lcCnt,;
            *        WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +WHBINLOC.QTY&lcCnt
            *lnRem = lnRem - WHBINLOC.QTY&lcCnt        
            *=lfGetORQty(WHBINLOC.QTY&lcCnt,lnCnt,OrdLine.PikTkT,OrdLine.Order)

            REPLACE WHBINLOC.Alo&lcCnt WITH  WHBINLOC.Alo&lcCnt+lnNetQty ,;
                    WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +lnNetQty
            lnRem = lnRem - lnNetQty
            *--mhm126994 add line no
            *=lfGetORQty(lnNetQty,lnCnt,OrdLine.PikTkT,OrdLine.Order)
            =lfGetORQty(lnNetQty,lnCnt,OrdLine.PikTkT,OrdLine.Order,OrdLine.LineNo)
            *--mhm126994 add line no
            
            *! C123853,1 MHM 01/15/2005  [End]
          
          ENDIF          
        ENDSCAN 
        =SEEK(OrdLine.Style+ OrdLine.cWareCode ,'WHBINLOC')       
      ENDIF  
    ELSE
	  *--mhm2005	
        *--in case of decrease Qty
      *IF WHBINLOC.QTY&lcCnt >= ABS(m.Pik&lcCnt - m.OldPik&lcCnt)
      *  IF (m.Pik&lcCnt - m.OldPik&lcCnt)<>0
      *    REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+ (m.Pik&lcCnt - m.OldPik&lcCnt),0) ,;
      *            WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo + (m.Pik&lcCnt - m.OldPik&lcCnt),0)
      *    =lfGetORQty((m.Pik&lcCnt - m.OldPik&lcCnt),lnCnt,OrdLine.PikTkT,OrdLine.Order)
      *  ENDIF  
      *ELSE
      *  lnCurrQty = -WHBINLOC.Alo&lcCnt
      *  REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnCurrQty,0),;
      *          WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnCurrQty,0)
      *        
      *  lnRem =(m.Pik&lcCnt - m.OldPik&lcCnt) - lnCurrQty
      *  =lfGetORQty(lnCurrQty,lnCnt,OrdLine.PikTkT,OrdLine.Order)
      *  SKIP
      *  SCAN REST WHILE STYLE+CWARECODE = OrdLine.Style+ OrdLine.cWareCode

      *    *! C123853,1 MHM 01/15/2005  Function to check class[Start]
      *    =lfchkCLas(OrdLine.Style,OrdLine.cWareCode ,lcCnt)
      *    *! C123853,1 MHM 01/15/2005  [Start]
      *    
      *    IF  WHBINLOC.QTY&lcCnt >= ABS(lnRem)
      *      REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnRem,0),;
      *              WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnRem,0)
      *      =lfGetORQty(lnRem,lnCnt,OrdLine.PikTkT,OrdLine.Order)
      *      EXIT        
      *    ELSE
      *      lnCurrQty = -WHBINLOC.Alo&lcCnt
      *      REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnCurrQty,0),;
      *              WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnCurrQty,0)
      *      lnRem = lnRem - lnCurrQty 
      *      =lfGetORQty(lnCurrQty,lnCnt,OrdLine.PikTkT,OrdLine.Order)
      *    ENDIF          
      *  ENDSCAN 
      *  =SEEK(OrdLine.Style+ OrdLine.cWareCode ,'WHBINLOC')       
      *ENDIF  
    ENDIF  
  ENDFOR
ENDIF

*--mhm2005
FLUSH
*--mhm2005

*!*************************************************************
*! Name      : lfFoundPad
*! Developer : Mohamed Shokry(MHM)
*! Date      : 04/03/2004
*! Purpose   : check if any pad menu is exit in _sysmenu
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*! Due to C037816,1 MHM 04/06/2004 
*:**************************************************************************
*
FUNCTION lfFoundPad
PARAMETER lcPadName

PRIVATE llFound
llFound = .F.
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
	IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = lcPadName
        llfound = .T.
		EXIT
	ENDIF
ENDFOR
RETURN(llFOund)
*-- end of lfFoundPad.

*!**************************************************************************
*! Name      : lfALSAVINV
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/03/2004
*! Purpose   : Save Data to Invoice screen
*!**************************************************************************
*! Example   : = lfALSAVINV()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*Function commented and wrettin in new way
****************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION OldlfALSAVINV

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF

*--check in case of consolidate
IF  &lcHdrFile..CONSOL = 'Y'
  RETURN
ENDIF
*

*--lcDetFile
IF SEEK(&lcDetFile..Style+ &lcDetFile..cWareCode ,'WHBINLOC') 
  SELECT WHBINLOC
  FOR lnCnt = 1 TO 8
    lcCnt = ALLT(STR(lnCnt,2))
    *-129429,1 mhm 08/14/2005 Start again from first record in whbinloc [Start]
    IF SEEK(&lcDetFile..PikTkT+&lcDetFile..cWareCode ,'PKBINLOC') 
      SELECT PKBINLOC
      SCAN REST WHILE piktkt+cwarecode+clocation+style = &lcDetFile..PikTkT + &lcDetFile..cWareCode FOR Style = &lcDetFile..Style
        *--mhm126994 check for lineno and Alrady invoiced [Start]
        IF  LineNo <> &lcDetFile..LineNo OR PKBINLOC.Qty&lcCnt = 0
          LOOP
        ENDIF
        *--mhm126994 check for lineno and Alrady invoiced [End]
        
        IF !SEEK(&lcDetFile..Style+ &lcDetFile..cWareCode + PKBINLOC.clocation ,'WHBINLOC')     
          =SEEK(&lcDetFile..Style+ &lcDetFile..cWareCode ,'WHBINLOC')     
          SELECT WHBINLOC   
          SCAN REST WHILE style+cwarecode+clocation = &lcDetFile..Style+ &lcDetFile..cWareCode 
            IF WHBINLOC.Qty&lcCnt >0
              REPLACE PKBINLOC.linvoiced  WITH .T.
              EXIT
            ENDIF
          ENDSCAN  
        ENDIF
        EXIT
        
      ENDSCAN
      SELECT WHBINLOC
    ENDIF
    *-129429,1 mhm 08/14/2005 [END]
    
    IF &lcDetFile..Qty&lcCnt <> 0
      IF WHBINLOC.QTY&lcCnt >= &lcDetFile..Qty&lcCnt 
        IF WHBINLOC.Qty&lcCnt >0
          REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt - &lcDetFile..Qty&lcCnt,0) ,;
                  WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo  - &lcDetFile..Qty&lcCnt,0) ,;
                  WHBINLOC.Qty&lcCnt WITH MAX(WHBINLOC.Qty&lcCnt  - &lcDetFile..Qty&lcCnt,0) ,;
                  WHBINLOC.TotQty    WITH MAX(WHBINLOC.TotQty  - &lcDetFile..Qty&lcCnt ,0)
               
          *--mhm126994
          *=lfGtInvQty(&lcDetFile..Qty&lcCnt,lnCnt,&lcDetFile..PikTkT,&lcDetFile..Order) 
          *=lfTrkInv(&lcDetFile..Qty&lcCnt,lnCnt,&lcDetFile..Order,lcInvNo)
          =lfGtInvQty(&lcDetFile..Qty&lcCnt,lnCnt,&lcDetFile..PikTkT,&lcDetFile..Order,&lcDetFile..LineNo) 
          =lfTrkInv(&lcDetFile..Qty&lcCnt,lnCnt,&lcDetFile..Order,lcInvNo,&lcDetFile..LineNo)
          
          *--mhm126994
          
          SELECT WHBINLOC
          IF TotQty = 0
            DELETE
          ENDIF         
        ENDIF  
      ELSE
        lnRem =&lcDetFile..Qty&lcCnt - WHBINLOC.QTY&lcCnt        
        
        *--mhm126994
        *=lfGtInvQty(WHBINLOC.QTY&lcCnt,lnCnt,&lcDetFile..PikTkT,&lcDetFile..Order)
        *=lfTrkInv(WHBINLOC.QTY&lcCnt,lnCnt,&lcDetFile..Order,lcInvNo)
        =lfGtInvQty(WHBINLOC.QTY&lcCnt,lnCnt,&lcDetFile..PikTkT,&lcDetFile..Order,&lcDetFile..LineNo)
        =lfTrkInv(WHBINLOC.QTY&lcCnt,lnCnt,&lcDetFile..Order,lcInvNo,&lcDetFile..LineNo)
        *--mhm126994
        
        REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt -WHBINLOC.QTY&lcCnt,0),;
                WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo    -WHBINLOC.QTY&lcCnt,0),;
                WHBINLOC.TotQty    WITH MAX(WHBINLOC.TotQty    -WHBINLOC.QTY&lcCnt,0),;
                WHBINLOC.Qty&lcCnt WITH 0
        SELECT WHBINLOC
        IF TotQty = 0
          DELETE
        ENDIF         
                
        *--mhm126994
        IF !EOF()        
          SKIP
        ENDIF
        *--mhm126994
        SCAN REST WHILE STYLE+CWARECODE = &lcDetFile..Style+ &lcDetFile..cWareCode
          *-129429,1 mhm 08/14/2005 Start again from first record in whbinloc [Start]
          IF SEEK(&lcDetFile..PikTkT+&lcDetFile..cWareCode ,'PKBINLOC') 
            SELECT PKBINLOC
            SCAN REST WHILE piktkt+cwarecode+clocation+style = &lcDetFile..PikTkT + &lcDetFile..cWareCode FOR Style = &lcDetFile..Style

              *--mhm126994 check for lineno and Alrady invoiced [Start]
              *IF !linvoiced
              IF !linvoiced AND LineNo = &lcDetFile..LineNo AND PKBINLOC.Qty&lcCnt <> 0
              *--mhm126994 check for lineno and Alrady invoiced [Start]
                IF !SEEK(&lcDetFile..Style+ &lcDetFile..cWareCode + PKBINLOC.clocation ,'WHBINLOC')     
                  =SEEK(&lcDetFile..Style+ &lcDetFile..cWareCode ,'WHBINLOC')     
                  SELECT WHBINLOC   
                  SCAN REST WHILE style+cwarecode+clocation = &lcDetFile..Style+ &lcDetFile..cWareCode 
                    IF WHBINLOC.Qty&lcCnt >0
                      EXIT
                    ENDIF
                  ENDSCAN  
                ENDIF
                EXIT  
              ENDIF  
            ENDSCAN
            SELECT WHBINLOC
          ENDIF
          *-129429,1 mhm 08/14/2005 [END]
        
          IF  WHBINLOC.QTY&lcCnt >= lnRem
            REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt -lnRem,0),;
                    WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo    -lnRem,0),;
                    WHBINLOC.TotQty    WITH MAX(WHBINLOC.TotQty    -lnRem,0),;
                    WHBINLOC.Qty&lcCnt WITH MAX(WHBINLOC.Qty&lcCnt -lnRem,0)

            *--mhm126994
            *=lfGtInvQty(lnRem,lnCnt,&lcDetFile..PikTkT,&lcDetFile..Order)
            *=lfTrkInv(lnRem,lnCnt,&lcDetFile..Order,lcInvNo)
            =lfGtInvQty(lnRem,lnCnt,&lcDetFile..PikTkT,&lcDetFile..Order,&lcDetFile..LineNo)
            =lfTrkInv(lnRem,lnCnt,&lcDetFile..Order,lcInvNo,&lcDetFile..LineNo)
            
            *--mhm126994
            
            SELECT WHBINLOC
            IF TotQty = 0
              DELETE
            ENDIF         
            EXIT        
          ELSE
            lnRem = lnRem - WHBINLOC.QTY&lcCnt        

            *-mhm126994
            *=lfGtInvQty(WHBINLOC.QTY&lcCnt,lnCnt,&lcDetFile..PikTkT,&lcDetFile..Order)
            *=lfTrkInv(WHBINLOC.QTY&lcCnt,lnCnt,&lcDetFile..Order,lcInvNo)
            =lfGtInvQty(WHBINLOC.QTY&lcCnt,lnCnt,&lcDetFile..PikTkT,&lcDetFile..Order,&lcDetFile..LineNo)
            =lfTrkInv(WHBINLOC.QTY&lcCnt,lnCnt,&lcDetFile..Order,lcInvNo,&lcDetFile..LineNo)
            
            *-mhm126994
                        
            REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt   -WHBINLOC.QTY&lcCnt,0),;
                    WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo      -WHBINLOC.QTY&lcCnt,0),;
                    WHBINLOC.TotQty    WITH MAX(WHBINLOC.TotQty      -WHBINLOC.QTY&lcCnt,0),;
                    WHBINLOC.Qty&lcCnt WITH MAX(WHBINLOC.Qty&lcCnt   -WHBINLOC.QTY&lcCnt,0)
            SELECT WHBINLOC
            IF TotQty = 0
              DELETE
            ENDIF         
          ENDIF          
        ENDSCAN 
        =SEEK(&lcDetFile..Style+ &lcDetFile..cWareCode ,'WHBINLOC')       
      ENDIF  
    ENDIF  
  ENDFOR
  SELECT WHBINLOC
  IF TotQty = 0
    DELETE
  ENDIF         
ENDIF
*!**********************************************************************
*! Name      : lfvBinBrow
*! Developer : Mohamed Shokry
*! Date      : 03/31/2004
*! Purpose   : Function to browse Bin Locations.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfvBinBrow
PRIVATE lcbrfields ,lnAlias,lcFile_Ttl

lnAlias=SELECT()
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH') 
ENDIF

IF !SEEK(PADR(lcmajor+lcnmkey,19)+lcWareCode,'WHBINLOC') 
  lcMsg2 = 'No bin location found for Style '+ lcMajor+lcNMKey+ ' Ware House ' +lcWareCode 
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
   RETURN
ENDIF
lcfile_ttl = 'Bin Location'

lcBrFields = "clocation :20 :H='Location',"
FOR lnI=1 TO lnScaleCnt
  lcI=STR(lnI,1)
  IF !EMPTY(lcSz&lcI)
    lcBrFields = lcBrFields + "Qty"+lcI+" :H=PADL(lcSz"+lcI+",5) :P='9999999',"
  ENDIF
ENDFOR
lcBrFields = lcBrFields + "TotQty :H='Total ' :P='999999999',"

*--add alocated data  **PADL(lcSz"+lcI+",5)
FOR lnI=1 TO lnScaleCnt
  lcI=STR(lnI,1)
  IF !EMPTY(lcSz&lcI)
    lcBrFields = lcBrFields + "Alo"+lcI+" :H='Alo '+PADL(lcSz"+lcI+",5)  :P='9999999',"
  ENDIF
ENDFOR
lcBrFields = lcBrFields + "TotAlo :H='Total Alocated ' :P='999999999'"

SELECT WHBINLOC

=ARIABROW([lcMajor+lcNMKey FOR cWareCode=lcWareCode REST],lcfile_ttl,;
          gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'',;
          'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')

SELECT(lnAlias)
RETURN
*!**************************************************************************
*! Name      : lfGtInvQty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/04/2004
*! Purpose   : Get Ordered Qty
*!**************************************************************************
*! Example   : = lfALSAVINV()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfGtInvQty
PARAMETER lnQty,lnSize,lcPkTkt,lcOrder,LnLineNo

lnAlias    = SELECT()
*-- lnQty -------> Qty per size
*-- lnSize -------> size
*-- lcPkTkt -------> PikTkt
*-- lcOrder -------> Order
lcSize = ALLT(STR(lnSize,2))

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF


*--MAX(lnCHold&Z+(Qty&Z*IIF(TranCd='1',1,-1)),0)  
*-mhm126994
SELECT PKBINLOC
lcPkBinOrd = ORDER()
SET ORDER TO PKBINLIN

*IF SEEK(lcPkTkt+WHBINLOC.cWareCode+WHBINLOC.cLocation+WHBINLOC.Style ,'PKBINLOC') 
IF SEEK(lcPkTkt+WHBINLOC.cWareCode+WHBINLOC.cLocation+WHBINLOC.Style+STR(LnLineNo,6) ,'PKBINLOC') 
*--mhm126994

  SELECT PKBINLOC
  *REPLACE Qty&lcSize WITH MAX(Qty&lcSize +lnQty,0),;
  *        TotQty     WITH MAX(TotQty     +lnQty,0)
  REPLACE Qty&lcSize WITH MAX(lnQty,0),;
          TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
          lInvoiced  WITH .T.
ENDIF

*--mhm126994
SELECT PKBINLOC
SET ORDER TO &lcPkBinOrd
*--mhm126994
SELECT (lnAlias)
*!**************************************************************************
*! Name      : lfTrkInv
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/05/2004
*! Purpose   : Trak invoice Qty
*!**************************************************************************
*! Example   : = lfALSAVINV()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfTrkInv
*--mhm126994
*PARAMETER lnQty,lnSize,lcOrder,lcInvoice
PARAMETER lnQty,lnSize,lcOrder,lcInvoice,lnLineNo
*--mhm126994

lnAlias    = SELECT()
*-- lnQty -------> Qty per size
*-- lnSize -------> size
*-- lcPkTkt -------> PikTkt
*-- lcOrder -------> Order
lcSize = ALLT(STR(lnSize,2))

IF !USED('IVBINLOC')
  =gfOpenFile(gcDataDir+'IVBINLOC','IVBININV','SH') 
ENDIF
*--mhm126994
SELECT IVBINLOC
lcIvBinOrd = ORDER()
SET ORDER TO Ivbinlin
*--mhm126994

*--MAX(lnCHold&Z+(Qty&Z*IIF(TranCd='1',1,-1)),0)  
IF !SEEK(lcInvoice+WHBINLOC.Style+STR(lnLineNo,6)+WHBINLOC.cWareCode+WHBINLOC.cLocation ,'IVBINLOC') 
  SELECT IVBINLOC
  APPEND BLANK
  REPLACE STYLE      WITH WHBINLOC.Style,;
          CWARECODE  WITH WHBINLOC.cWareCode,;
          ORDER      WITH lcOrder,;
          Invoice    WITH lcInvoice,;
          clocation  WITH WHBINLOC.cLocation ,;
          Qty&lcSize WITH lnQty,;
          TotQty     WITH lnQty
  *--mhm126994
  REPLACE LineNo  WITH lnLineNo      
  *--mhm126994
  
  =gfAdd_Info('IVBINLOC')
ELSE
  SELECT IVBINLOC
  REPLACE Qty&lcSize WITH Qty&lcSize + lnQty,;
          TotQty     WITH TotQty+lnQty
ENDIF

*--mhm126994
SELECT IVBINLOC
SET ORDER TO &lcIvBinOrd
*--mhm126994

SELECT (lnAlias)
*!**************************************************************************
*! Name      : lfDLVODINV
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Save Data to Void Invoice screen
*!**************************************************************************
*! Example   : = lfDLVODINV()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLVODINV

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
IF !USED('IVBINLOC')
  =gfOpenFile(gcDataDir+'IVBINLOC','IVBININV','SH') 
ENDIF

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF

*! C123853,1 MHM 01/15/2005  new fuction to handle voided invoice[Start]
=lfDLARVOID()
*! C123853,1 MHM 01/15/2005  [End]

*--mhm126994
*IF SEEK(InvLine.Invoice+InvLine.Style+InvHdr.cWareCode ,'IVBINLOC') 
*  SELECT IVBINLOC
*  SCAN REST WHILE Invoice+Style+cWareCode+cLocation=InvLine.Invoice+InvLine.Style+InvHdr.cWareCode

SELECT PKBINLOC
lcPkBinOrd = ORDER()
SET ORDER TO Pkbinlin

SELECT IVBINLOC
lcIvBinOrd = ORDER()
SET ORDER TO Ivbinlin

IF SEEK(InvLine.Invoice+InvLine.Style+STR(InvLine.LineNo,6)+InvHdr.cWareCode ,'IVBINLOC') 
  SCAN REST WHILE Invoice+Style+STR(LineNo,6)+cWareCode+cLocation=InvLine.Invoice+InvLine.Style+STR(InvLine.LineNo,6)+InvHdr.cWareCode
*--mhm126994

    IF SEEK(IvBinLoc.Style+ IvBinLoc.cWareCode+IvBinLoc.cLocation ,'WHBINLOC') 
      SELECT WHBINLOC
      REPLACE Qty1       WITH Qty1+ IvBinLoc.Qty1,;
              Qty2       WITH Qty2+ IvBinLoc.Qty2,;
              Qty3       WITH Qty3+ IvBinLoc.Qty3,;
              Qty4       WITH Qty4+ IvBinLoc.Qty4,;
              Qty5       WITH Qty5+ IvBinLoc.Qty5,;
              Qty6       WITH Qty6+ IvBinLoc.Qty6,;
              Qty7       WITH Qty7+ IvBinLoc.Qty7,;
              Qty8       WITH Qty8+ IvBinLoc.Qty8,;
              TotQty     WITH TotQty+ IvBinLoc.TotQty
    ELSE
      SELECT WHBINLOC
      APPEND BLANK
      REPLACE STYLE      WITH IvBinLoc.Style,;
              CWARECODE  WITH IvBinLoc.cWareCode,;
              clocation  WITH IvBinLoc.cLocation ,;
              Qty1       WITH Qty1+ IvBinLoc.Qty1,;
              Qty2       WITH Qty2+ IvBinLoc.Qty2,;
              Qty3       WITH Qty3+ IvBinLoc.Qty3,;
              Qty4       WITH Qty4+ IvBinLoc.Qty4,;
              Qty5       WITH Qty5+ IvBinLoc.Qty5,;
              Qty6       WITH Qty6+ IvBinLoc.Qty6,;
              Qty7       WITH Qty7+ IvBinLoc.Qty7,;
              Qty8       WITH Qty8+ IvBinLoc.Qty8,;
              TotQty     WITH TotQty+ IvBinLoc.TotQty
              
      =gfAdd_Info('WHBINLOC')
    ENDIF
    *--mhm126994
    *IF SEEK(InvHdr.PIKTKT+IvBinLoc.cWareCode+IvBinLoc.cLocation+IvBinLoc.Style,'PKBINLOC') 
    IF SEEK(InvHdr.PIKTKT+IvBinLoc.cWareCode+IvBinLoc.cLocation+IvBinLoc.Style+STR(IvBinLoc.LineNo,6),'PKBINLOC') 
    *--mhm126994
      SELECT WHBINLOC
      REPLACE Alo1       WITH Alo1+ IvBinLoc.Qty1,;
              Alo2       WITH Alo2+ IvBinLoc.Qty2,;
              Alo3       WITH Alo3+ IvBinLoc.Qty3,;
              Alo4       WITH Alo4+ IvBinLoc.Qty4,;
              Alo5       WITH Alo5+ IvBinLoc.Qty5,;
              Alo6       WITH Alo6+ IvBinLoc.Qty6,;
              Alo7       WITH Alo7+ IvBinLoc.Qty7,;
              Alo8       WITH Alo8+ IvBinLoc.Qty8,;
              TotAlo     WITH TotAlo+ IvBinLoc.TotQty
      SELECT PKBINLOC
      REPLACE lInvoiced   WITH .F.
    ENDIF
    SELECT IVBINLOC
    DELETE
  ENDSCAN
ENDIF

*--mhm126994
SELECT PKBINLOC
SET ORDER TO &lcPkBinOrd

SELECT IVBINLOC
SET ORDER TO &lcIvBinOrd
*--mhm126994

*:*************************************************************
*! Name        : lfARGETBIN
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : AR
*! Purpose     : Screen of get bins in case of ar
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfARGETBIN

lnAlias = SELECT()

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
IF !USED('WHSLOC')
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH') 
ENDIF

lcBinLoc = ''
SELECT(lcInvLine)
SCATTER MEMVAR MEMO
STORE '' TO m.BinQty1,m.BinQty2,m.BinQty3,m.BinQty4,m.BinQty5,m.BinQty6,m.BinQty7,m.BinQty8
*-
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\ARDLBIN.SPX")
POP KEY

SELECT(lnAlias)
*:*************************************************************
*! Name        : lfvArbinloc
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : AR
*! Purpose     : Screen of get bins in case of Direct invoice
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfvArbinloc

PARA lnCnxt
SELECT (lcInvLine)
lnRecNo = RECNO()

lcCnxt = STR(lnCnxt,1)
lcWareCode = m.cWareCode
IF !EMPTY(m.BinLoc&lcCnxt) 
  lnAlias = SELECT()
  SELECT WHSLOC
  SET ORDER TO TAG WhsLoc

  *- from testing
  IF !SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
    SET ORDER TO TAG WhsLocSt IN WHSLOC 
    =gfModalGen('INM00000B00000','','','',ALLTRIM(m.BinLoc&lcCnxt) + ' is not a valid location')
    _CUROBJ = OBJNUM(m.BinLoc&lcCnxt)
    m.BinLoc&lcCnxt = ''
    RETURN
  ELSE
    =SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
    *SELECT STYLE
    *lcOldOrder = ORDER()
    *SET ORDER TO STYLE
    *=SEEK(&lcInvLine..Style,'STYLE')
    *SET ORDER TO &lcOldOrder
    IF !(WhsLoc.cFlatHang = style.cflathang)
    *  *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot issue the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      _CUROBJ = OBJNUM(m.BinLoc&lcCnxt)
      m.BinLoc&lcCnxt= ''
      RETURN
    ENDIF
    llClass = .F.
    *--got from testing

    lcCount = STR(lnCount,1)
    IF (WhsLoc.cBinClass == style.cPrimClss&lcCnxt)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cSecClss&lcCnxt)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cRemClss&lcCnxt)
      llClass = .T.
    ENDIF

    IF !llClass
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot Issue the style to a bin of a different class.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      _CUROBJ = OBJNUM(m.BinLoc&lcCnxt)
      m.BinLoc&lcCnxt= ''
      RETURN
    ENDIF

    IF !SEEK(lcWareCode+LEFT(m.BinLoc&lcCnxt,10)+&lcInvLine..style,'WHSLOC')
      =SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
      lcLocSty = m.style
      SCATT MEMVAR MEMO
      m.cWareCode = lcWareCode
      m.style = lcLocSty
      m.clocation = m.BinLoc&lcCnxt
      INSERT INTO WHSLOC FROM MEMVAR 
    ENDIF

  ENDIF
  SELECT WHSLOC
  SET ORDER TO TAG WhsLocSt
  SELECT(lnAlias)
  IF !SEEK(&lcInvLine..Style+ &lcInvLine..cWareCode+m.BinLoc&lcCnxt ,'WHBINLOC') 
    lcMsg2 = 'Stock not available'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    m.BinLoc&lcCnxt=''
    RETURN
  ENDIF
  IF &lcInvLine..QTY&lcCnxt>(WhBinLoc.Qty&lcCnxt - WhBinLoc.Alo&lcCnxt)
    lcMsg2 = 'Stock not available'
     =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    m.BinLoc&lcCnxt=''
    RETURN
  ENDIF

  SELECT (lcInvLine)
  *IF QTY&lcCnxt > (WHBINLOC.QTY&lcCnxt-QTY&lcCnxt)
  *  lcMsg2 = 'Stock not available'
  *  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  *  m.BinLoc&lcCnxt=''
  *  RETURN
  *ENDIF
  m.BinQTY&lcCnxt = (WhBinLoc.Qty&lcCnxt - WhBinLoc.Alo&lcCnxt )
  SHOW GETS
  SELECT (lcInvLine)
  GOTO lnRecNo
  
ENDIF
*!**************************************************************************
*! Name      : lfDLINTVAR  
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : To Initiate variables for the new modification
*!**************************************************************************
*! Example   : = lfDLINTVAR()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLINTVAR     

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF


lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+8,4]


laFileStru[lnFileStru+ 1 , 1] = 'BinLoc1'
laFileStru[lnFileStru+ 1 , 2] = 'C'
laFileStru[lnFileStru+ 1 , 3] = 10
laFileStru[lnFileStru+ 1 , 4] = 0

laFileStru[lnFileStru+ 2 , 1] = 'BinLoc2'
laFileStru[lnFileStru+ 2 , 2] = 'C'
laFileStru[lnFileStru+ 2 , 3] = 10
laFileStru[lnFileStru+ 2 , 4] = 0

laFileStru[lnFileStru+ 3 , 1] = 'BinLoc3'
laFileStru[lnFileStru+ 3 , 2] = 'C'
laFileStru[lnFileStru+ 3 , 3] = 10
laFileStru[lnFileStru+ 3 , 4] = 0

laFileStru[lnFileStru+ 4 , 1] = 'BinLoc4'
laFileStru[lnFileStru+ 4 , 2] = 'C'
laFileStru[lnFileStru+ 4 , 3] = 10
laFileStru[lnFileStru+ 4 , 4] = 0

laFileStru[lnFileStru+ 5 , 1] = 'BinLoc5'
laFileStru[lnFileStru+ 5 , 2] = 'C'
laFileStru[lnFileStru+ 5 , 3] = 10
laFileStru[lnFileStru+ 5 , 4] = 0

laFileStru[lnFileStru+ 6 , 1] = 'BinLoc6'
laFileStru[lnFileStru+ 6 , 2] = 'C'
laFileStru[lnFileStru+ 6 , 3] = 10
laFileStru[lnFileStru+ 6 , 4] = 0

laFileStru[lnFileStru+ 7 , 1] = 'BinLoc7'
laFileStru[lnFileStru+ 7 , 2] = 'C'
laFileStru[lnFileStru+ 7 , 3] = 10
laFileStru[lnFileStru+ 7 , 4] = 0

laFileStru[lnFileStru+ 8 , 1] = 'BinLoc8'
laFileStru[lnFileStru+ 8 , 2] = 'C'
laFileStru[lnFileStru+ 8 , 3] = 10
laFileStru[lnFileStru+ 8 , 4] = 0


*:*************************************************************
*! Name        : lfvArOk
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : AR
*! Purpose     : Screen of get bins in case of invoice.
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfvArOk

SELECT (lcInvLine)
REPLACE  BinLoc1  WITH m.BinLoc1,;
         BinLoc2  WITH m.BinLoc2,;
         BinLoc3  WITH m.BinLoc3,;
         BinLoc4  WITH m.BinLoc4,;
         BinLoc5  WITH m.BinLoc5,;
         BinLoc6  WITH m.BinLoc6,;
         BinLoc7  WITH m.BinLoc7,;
         BinLoc8  WITH m.BinLoc8
CLEAR READ

*!**************************************************************************
*! Name      : lfDLREMLIN 
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Remove line from Invoice
*!**************************************************************************
*! Example   : = lfDLREMLIN()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLREMLIN

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

PRIVATE lcFileNamA
lcFileNamA = lcInvLine+"A"
IF SEEK(WHBINLOC.style+WHBINLOC.cwarecode+WHBINLOC.clocation,lcFileNamA)
  SELECT (lcFileNamA)
  DELETE
ENDIF

*!**************************************************************************
*! Name      : lfDLSAVINV
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : SaveInvoice in WHBINLOC File
*!**************************************************************************
*! Example   : = lfDLSAVINV()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLSAVINV

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF
IF !USED('IVBINLOC')
  =gfOpenFile(gcDataDir+'IVBINLOC','IVBININV','SH') 
ENDIF

*--mhm126994
SELECT IVBINLOC
lcIvBinOrd = ORDER()
SET ORDER TO Ivbinlin
*--mhm126994

SELECT (lcInvHdr)
LOCATE 
SELECT (lcInvLine)
*--mhm128879
locline = 0
SCAN
  locline = locline + 1
  REPLACE lineno WITH locline
ENDSCAN
*--mhm128879
LOCATE 
IF EOF()
  RETURN
ENDIF
SCAN
  *B128959,1 NNA 09/04/2005 (Start) Don't Update the Bin Location stock files if Not Style.linvsty
  IF !STYLE.LINVSTY
    LOOP
  ENDIF
  *B128959,1 NNA (End)

  FOR lnCount = 1  TO 8
    lcCount = ALLTRIM(STR(lnCount))
    IF !EMPTY(&lcInvLine..BinLoc&lcCount)
      IF SEEK(style+&lcInvLine..cWareCode+&lcInvLine..BinLoc&lcCount,'WHBINLOC')
        SELECT WHBINLOC
        REPLACE  Qty&lcCount    WITH MAX(Qty&lcCount - &lcInvLine..Qty&lcCount,0),;
                 TotQty         WITH MAX(TotQty - &lcInvLine..Qty&lcCount,0),;
                 ALO&lcCount    WITH MAX(ALO&lcCount - &lcInvLine..Qty&lcCount,0),;
                 TotAlo         WITH MAX(TotALO - &lcInvLine..Qty&lcCount,0) 
    
        *--mhm126994
        *IF !SEEK(&lcInvHdr..Invoice+&lcInvLine..Style+&lcInvLine..cWareCode+&lcInvLine..BinLoc&lcCount ,'IVBINLOC') 
        IF !SEEK(&lcInvHdr..Invoice+&lcInvLine..Style+STR(lineno,6)+&lcInvLine..cWareCode+&lcInvLine..BinLoc&lcCount ,'IVBINLOC') 
        *--mhm126994
        
          SELECT IVBINLOC
          APPEND BLANK
          REPLACE STYLE        WITH WHBINLOC.Style,;
                  CWARECODE    WITH WHBINLOC.cWareCode,;
                  ORDER        WITH &lcInvHdr..Order,;
                  Invoice      WITH &lcInvHdr..Invoice,;
                  clocation    WITH &lcInvLine..BinLoc&lcCount
              
          REPLACE  Qty&lcCount WITH &lcInvLine..Qty&lcCount ,;
                   TotQty      WITH &lcInvLine..Qty&lcCount
        
          *--mhm126994
          REPLACE LineNo  WITH &lcInvLine..LineNo      
          *--mhm126994
              
           =gfAdd_Info('IVBINLOC')
        ELSE
          SELECT IVBINLOC
          REPLACE  Qty&lcCount WITH Qty&lcCount + &lcInvLine..Qty&lcCount ,;
                   TotQty      WITH TotQty + &lcInvLine..Qty&lcCount
        ENDIF

        SELECT WHBINLOC
        IF TotQty = 0
          DELETE
        ENDIF         
      ENDIF
    ENDIF  
  ENDFOR  
  *! C123853,1 MHM 01/15/2005  new function to distrebute to custom BININVJL file [Start]
  m.Invoice = &lcInvHdr..Invoice
  =lfDLARBIN()
  *! C123853,1 MHM 01/15/2005  [End]
  
ENDSCAN


*:*************************************************************
*! Name        : lfDLGTRMBN
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : RM
*! Purpose     : Screen of get bins in case of RM.
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLGTRMBN

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

lnAlias = SELECT()

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
IF !USED('WHSLOC')
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH') 
ENDIF
SELECT(lcCrMemLin)
SCATTER MEMVAR MEMO
*-
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\RMDLBIN.SPX")
POP KEY
SELECT(lnAlias)
*:*************************************************************
*! Name        : lfvRmbinloc
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : RM
*! Purpose     : Screen of get bins in case of Direct invoice
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfvRMbinloc

PARA lnCnxt
lcCnxt = STR(lnCnxt,1)
lcWareCode = &lcCrMemHdr..cWareCode
IF !EMPTY(m.BinLoc&lcCnxt) 
  lnAlias = SELECT()
  SELECT WHSLOC
  SET ORDER TO TAG WhsLoc

  *- from testing
  IF !SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
    SET ORDER TO TAG WhsLocSt IN WHSLOC 
    =gfModalGen('INM00000B00000','','','',ALLTRIM(m.BinLoc&lcCnxt) + ' is not a valid location')
    _CUROBJ = OBJNUM(m.BinLoc&lcCnxt)
    m.BinLoc&lcCnxt = ''
    RETURN
  ELSE
    =SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
    IF !(WhsLoc.cFlatHang = style.cflathang)
    *  *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot issue the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      m.BinLoc&lcCnxt= ''
      RETURN
    ENDIF
    llClass = .F.
    *--got from testing

    lcCount = STR(lnCount,1)
    IF (WhsLoc.cBinClass == style.cPrimClss&lcCnxt)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cSecClss&lcCnxt)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cRemClss&lcCnxt)
      llClass = .T.
    ENDIF

    IF !llClass
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot Issue the style to a bin of a different class.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      *laBinLoc[lnCnxt] = ''
      RETURN
    ENDIF

    IF !SEEK(lcWareCode+LEFT(m.BinLoc&lcCnxt,10)+m.style,'WHSLOC')
      =SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
      lcLocSty = m.style
      SCATT MEMVAR MEMO
      m.cWareCode = lcWareCode
      m.style = lcLocSty
      m.clocation = m.BinLoc&lcCnxt
      INSERT INTO WHSLOC FROM MEMVAR 
    ENDIF
    *SELECT (lcTmpLine)

    *REPLACE &lcTmpLine..Loc&lcCnxt WITH m.BinLoc&lcCnxt
  ENDIF
  SELECT WHSLOC
  SET ORDER TO TAG WhsLocSt
  SELECT(lnAlias)
ENDIF

*:*************************************************************
*! Name        : lfvRMOk
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : RM
*! Purpose     : Screen of get bins in case of Credit memo.
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfvRmOk
SELECT (lcCrMemLin)
REPLACE  BinLoc1  WITH m.BinLoc1,;
         BinLoc2  WITH m.BinLoc2,;
         BinLoc3  WITH m.BinLoc3,;
         BinLoc4  WITH m.BinLoc4,;
         BinLoc5  WITH m.BinLoc5,;
         BinLoc6  WITH m.BinLoc6,;
         BinLoc7  WITH m.BinLoc7,;
         BinLoc8  WITH m.BinLoc8
CLEAR READ


*:*************************************************************
*! Name        : lfADDRMFLD
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : RM
*! Purpose     : Add Custom Fields for temp file
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfADDRMFLD

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

lnCrMemLin = ALEN(laCrMemLin,1)

DIMENSION laCrMemLin[lnCrMemLin+8,4]

laCrMemLin[lnCrMemLin + 1 , 1] = 'BinLoc1'
laCrMemLin[lnCrMemLin + 1 , 2] = 'C'
laCrMemLin[lnCrMemLin + 1 , 3] = 10
laCrMemLin[lnCrMemLin + 1 , 4] = 0

laCrMemLin[lnCrMemLin + 2 , 1] = 'BinLoc2'
laCrMemLin[lnCrMemLin + 2 , 2] = 'C'
laCrMemLin[lnCrMemLin + 2 , 3] = 10
laCrMemLin[lnCrMemLin + 2 , 4] = 0

laCrMemLin[lnCrMemLin + 3 , 1] = 'BinLoc3'
laCrMemLin[lnCrMemLin + 3 , 2] = 'C'
laCrMemLin[lnCrMemLin + 3 , 3] = 10
laCrMemLin[lnCrMemLin + 3 , 4] = 0

laCrMemLin[lnCrMemLin + 4 , 1] = 'BinLoc4'
laCrMemLin[lnCrMemLin + 4 , 2] = 'C'
laCrMemLin[lnCrMemLin + 4 , 3] = 10
laCrMemLin[lnCrMemLin + 4 , 4] = 0

laCrMemLin[lnCrMemLin + 5 , 1] = 'BinLoc5'
laCrMemLin[lnCrMemLin + 5 , 2] = 'C'
laCrMemLin[lnCrMemLin + 5 , 3] = 10
laCrMemLin[lnCrMemLin + 5 , 4] = 0

laCrMemLin[lnCrMemLin + 6 , 1] = 'BinLoc6'
laCrMemLin[lnCrMemLin + 6 , 2] = 'C'
laCrMemLin[lnCrMemLin + 6 , 3] = 10
laCrMemLin[lnCrMemLin + 6 , 4] = 0

laCrMemLin[lnCrMemLin + 7 , 1] = 'BinLoc7'
laCrMemLin[lnCrMemLin + 7 , 2] = 'C'
laCrMemLin[lnCrMemLin + 7 , 3] = 10
laCrMemLin[lnCrMemLin + 7 , 4] = 0

laCrMemLin[lnCrMemLin + 8 , 1] = 'BinLoc8'
laCrMemLin[lnCrMemLin + 8 , 2] = 'C'
laCrMemLin[lnCrMemLin + 8 , 3] = 10
laCrMemLin[lnCrMemLin + 8 , 4] = 0

*!**************************************************************************
*! Name      : lfDLSAVRM
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Save Credit Memo in WHBINLOC File
*!**************************************************************************
*! Example   : = lfDLSAVRM()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLSAVRM

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF
PRIVATE lnCurAlias
lnCurAlias = ALIAS()
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH') 
ENDIF

*! C123853,1 MHM 01/15/2005  open needed file and create tem file to distribute StyInvJl[Start]
SELECT WHBINLOC
lcOldOrder = ORDER()
SET ORDER TO WHBINLST


IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF

IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF

IF !USED('RMBINLOC')
  =gfOpenFile(gcDataDir+'RMBINLOC','RMBINCRM','SH') 
ENDIF

DIMENSION laBinArr[8]
PRIVATE lcCurTmp
lcCurTmp = gfTempName()
CREATE TABLE (gcWorkDir + lcCurTmp);
       ( nstk1 N(7), nstk2 N(7),nstk3 N(7),nstk4 N(7),nstk5 N(7),nstk6 N(7),nstk7 N(7),;
       nstk8 N(7), nTotstk N(7), Binloc C(10) )
INDEX ON Binloc  TAG (lcCurTmp)
*! C123853,1 MHM 01/15/2005  [End]

SELECT (lcCrMemLin)
LOCATE 
IF EOF()
  RETURN
ENDIF
PRIVATE llChkUpdt
SCAN
  =SEEK(Style,'STYLE')
  *--mhmcheck 129431 
  IF !STYLE.LINVSTY
    LOOP
  ENDIF
  *--mhm

  *! C123853,1 MHM 01/15/2005  Assign location per size[Start]
  FOR lncI = 1 TO 8 
    lccI = ALLTRIM(STR(lncI,1))
    laBinArr[lncI] =  &lcCrMemLin..BinLoc&lccI 
  ENDFOR

  SELECT (lcCurTmp)
  FOR lni = 1 To 8
    lcI = ALLTRIM(STR(lni,1))
    IF &lcCrMemLin..Qty&lcI<> 0  
      IF !SEEK(laBinArr[lni],lcCurTmp) 
        APPEND BLANK
        REPLACE nstk&lcI WITH &lcCrMemLin..Qty&lcI,;
                nTotstk  WITH &lcCrMemLin..Qty&lcI,;
                Binloc   WITH laBinArr[lni]
      ELSE
        REPLACE nstk&lcI WITH &lcCrMemLin..Qty&lcI,;
                nTotstk  WITH nTotstk + &lcCrMemLin..Qty&lcI
      ENDIF
    ENDIF  
  ENDFOR
  SELECT (lcCrMemLin)
  *! C123853,1 MHM 01/15/2005  [End]
  
  FOR lnCount = 1  TO 8
    lcCount = ALLTRIM(STR(lnCount))
    IF !EMPTY(&lcCrMemLin..BinLoc&lcCount)
      *B128798,1 MMR 07/05/2005 Fix the bug of Not Updating Bin location with Returns in case of 
      *B128798,1 MMR            damaged and 2nd Quality.[Start]  
      *IF SEEK(style+&lcCrMemHdr..cWareCode+&lcCrMemLin..BinLoc&lcCount,'WHBINLOC')
        *SELECT WHBINLOC
        *REPLACE  Qty&lcCount    WITH Qty&lcCount + &lcCrMemLin..Qty&lcCount,;
                 *TotQty         WITH TotQty + &lcCrMemLin..Qty&lcCount
      *ELSE
        *SELECT WHBINLOC
        *APPEND BLANK
        *REPLACE  Style       WITH &lcCrMemLin..style,;
                 *cWareCode   WITH &lcCrMemHdr..cWareCode,;
                 *clocation   WITH &lcCrMemLin..BinLoc&lcCount,;
                 *Qty&lcCount WITH &lcCrMemLin..Qty&lcCount,;
                 *TotQty      WITH &lcCrMemLin..Qty&lcCount
      IF EMPTY(&lcCrMemLin..Cretsty)           
        *B129431,1  TMI [Start] Update the Whbinloc file only if the style is inv-style 
        =SEEK(&lcCrMemLin..STYLE,'STYLE')
        IF STYLE.LINVSTY           
          *B129431,1  TMI [End  ] 
          IF SEEK(style+&lcCrMemHdr..cWareCode+&lcCrMemLin..BinLoc&lcCount,'WHBINLOC')
            SELECT WHBINLOC
            REPLACE  Qty&lcCount    WITH Qty&lcCount + &lcCrMemLin..Qty&lcCount,;
                     TotQty         WITH TotQty + &lcCrMemLin..Qty&lcCount
          ELSE
            SELECT WHBINLOC
            APPEND BLANK
            REPLACE  Style       WITH &lcCrMemLin..style,;
                     cWareCode   WITH &lcCrMemHdr..cWareCode,;
                     clocation   WITH &lcCrMemLin..BinLoc&lcCount,;
                     Qty&lcCount WITH &lcCrMemLin..Qty&lcCount,;
                     TotQty      WITH &lcCrMemLin..Qty&lcCount
          ENDIF           
          *B129431,1  TMI [Start] 
        ENDIF
        *B129431,1  TMI [End  ] 
      ELSE 
        IF SEEK(&lcCrMemLin..Cretsty+&lcCrMemHdr..cWareCode+&lcCrMemLin..BinLoc&lcCount,'WHBINLOC')
          SELECT WHBINLOC
          REPLACE  Qty&lcCount    WITH Qty&lcCount + &lcCrMemLin..Qty&lcCount,;
                   TotQty         WITH TotQty + &lcCrMemLin..Qty&lcCount
        ELSE
          SELECT WHBINLOC
          APPEND BLANK
          REPLACE  Style       WITH &lcCrMemLin..Cretsty,;
                   cWareCode   WITH &lcCrMemHdr..cWareCode,;
                   clocation   WITH &lcCrMemLin..BinLoc&lcCount,;
                   Qty&lcCount WITH &lcCrMemLin..Qty&lcCount,;
                   TotQty      WITH &lcCrMemLin..Qty&lcCount
        ENDIF             
        *B128798,1 MMR[End]           
        =gfAdd_Info('WHBINLOC')
      ENDIF
    ENDIF  
  ENDFOR  

  *! C123853,1 MHM 01/15/2005 update classes [Start]
  SELECT STYINVJL
  *B128798,1 MMR 07/05/2005 Fix the bug of Not Updating Bin location with Returns in case of 
  *B128798,1 MMR            damaged and 2nd Quality.[Start]
  IF EMPTY(&lcCrMemLin..Cretsty)
  *B128798,1 MMR  
    IF !SEEK(&lcCrMemLin..style+&lcCrMemHdr..cWareCode+lcRmGlSess+DTOS(laData[5])+laData[1])
      lcRmGlSess = STR(VAL(lcRmGlSess)+1)
      lcRmGlsess = ALLTRIM(lcRmGlsess)
      IF LEN(lcRmGlsess) = 5
        lcRmGlsess = '0'+lcRmGlsess 
      ENDIF
      =SEEK(&lcCrMemLin..style+&lcCrMemHdr..cWareCode+lcRmGlSess+DTOS(laData[5])+laData[1])
    ENDIF
  ELSE
    IF !SEEK(&lcCrMemLin..Cretsty+&lcCrMemHdr..cWareCode+lcRmGlSess+DTOS(laData[5])+laData[1])
      lcRmGlSess = STR(VAL(lcRmGlSess)+1)
      lcRmGlsess = ALLTRIM(lcRmGlsess)
      IF LEN(lcRmGlsess) = 5
        lcRmGlsess = '0'+lcRmGlsess 
      ENDIF
      =SEEK(&lcCrMemLin..Cretsty+&lcCrMemHdr..cWareCode+lcRmGlSess+DTOS(laData[5])+laData[1])
    ENDIF
  ENDIF   
  *B128798,1 MMR[End]
  SCATT MEMVAR MEMO
  lnCurCost = m.nCost  
  SELECT (lcCurTmp)
  SCAN
    SCATT MEMVAR MEMO
    m.clocation = m.Binloc 
    m.nCost     = lnCurCost 
    m.nstkval   = m.nTotstk * m.nCost
    SELECT BININVJL
    APPEND BLANK
    GATHER MEMVAR MEMO 
    FOR lni = 1 To 8
      SELECT RMBINLOC
      lcI = ALLTRIM(STR(lni,1))
     *B128798,1 MMR 07/05/2005 Fix the bug of Not Updating Bin location with Returns in case of 
     *B128798,1 MMR            damaged and 2nd Quality.[Start]
      IF EMPTY(&lcCrMemLin..Cretsty)
      *B128798,1 MMR
        IF SEEK(&lcCrMemLin..crmemo+&lcCrMemLin..style+&lcCrMemHdr..cWareCode+&lcCurTmp..Binloc)
          IF &lcCurTmp..nstk&lcI <> 0  
            REPLACE Qty&lcI  WITH &lcCurTmp..nstk&lcI,;
                    TotQty   WITH TotQty  + &lcCurTmp..nstk&lcI
          ENDIF  
        ELSE
          SELECT RMBINLOC
          APPEND BLANK
          REPLACE  Style       WITH &lcCrMemLin..style,;
                   cWareCode   WITH &lcCrMemHdr..cWareCode,;
                   clocation   WITH &lcCurTmp..Binloc ,;
                   CrMemo      WITH &lcCrMemLin..crmemo,;
                   Qty&lcI     WITH &lcCurTmp..nstk&lcI,;
                   TotQty      WITH TotQty  + &lcCurTmp..nstk&lcI
          =gfAdd_Info('RMBINLOC')
        ENDIF
      ELSE
        IF SEEK(&lcCrMemLin..crmemo+&lcCrMemLin..Cretsty+&lcCrMemHdr..cWareCode+&lcCurTmp..Binloc)
          IF &lcCurTmp..nstk&lcI <> 0  
            REPLACE Qty&lcI  WITH &lcCurTmp..nstk&lcI,;
                    TotQty   WITH TotQty  + &lcCurTmp..nstk&lcI
          ENDIF  
        ELSE
          SELECT RMBINLOC
          APPEND BLANK
          REPLACE  Style       WITH &lcCrMemLin..Cretsty,;
                   cWareCode   WITH &lcCrMemHdr..cWareCode,;
                   clocation   WITH &lcCurTmp..Binloc ,;
                   CrMemo      WITH &lcCrMemLin..crmemo,;
                   Qty&lcI     WITH &lcCurTmp..nstk&lcI,;
                   TotQty      WITH TotQty  + &lcCurTmp..nstk&lcI
          =gfAdd_Info('RMBINLOC')
        ENDIF
      ENDIF  
      *B128798,1 MMR[End]      
    ENDFOR
  ENDSCAN
  SELECT (lcCurTmp)
  ZAP
  *! C123853,1 MHM 01/15/2005 [End]
ENDSCAN

SELECT WHBINLOC
SET ORDER TO &lcOldOrder

SELECT (lnCurAlias) 

*!**************************************************************************
*! Name      : lfDLVLDSTY
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Validate Style in case of IC Adj.
*!**************************************************************************
*! Example   : = lfDLVLDSTY()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLVLDSTY

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN .F.
ENDIF

PRIVATE lcFileNama
lcFileNama = lcTmpAdj+"A"

SET ORDER TO TAG WhsLoc IN WHSLOC 

IF !SEEK(lcFromWare+&lcFileNamA..LocFrom+m.style,'WHSLOC')
  =SEEK(lcFromWare+&lcFileNamA..LocFrom,'WHSLOC')
  lcLocSty = m.style
  SCATT MEMVAR MEMO
  m.cWareCode = lcFromWare
  m.style     = lcLocSty
  m.clocation = &lcFileNamA..LocFrom
  INSERT INTO WHSLOC FROM MEMVAR 
ENDIF

=SEEK(lcFromWare+&lcFileNamA..LocFrom,'WHSLOC')
IF !(WhsLoc.cFlatHang = style.cflathang)
  SET ORDER TO TAG WhsLocSt IN WHSLOC 
  *--You cannot transfer from the same warehouse.
  lcMsg2 = 'You cannot Adjust a style with a bin of a different Hang/Flat.'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  RETURN
ENDIF
llClass = .F.
FOR lnCount = 1 TO 8
  IF llClass  
    EXIT
  ENDIF
  lcCount = STR(lnCount,1)
  IF (WhsLoc.cBinClass = style.cPrimClss&lcCount)
    llClass = .T.
  ENDIF
  IF (WhsLoc.cBinClass = style.cSecClss&lcCount)
    llClass = .T.
  ENDIF
  IF (WhsLoc.cBinClass = style.cRemClss&lcCount)
    llClass = .T.
  ENDIF
ENDFOR
IF !llClass
  *--You cannot transfer from the same warehouse.
  SET ORDER TO TAG WhsLocSt IN WHSLOC 
  lcMsg2 = 'You cannot Adjust a style with a bin of a different class.'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  *laBinLoc[lnCnxt] = ''
  RETURN
ENDIF

SET ORDER TO TAG WhsLocSt IN WHSLOC 

IF !llDyelot OR Style.cDye_Flg<>'Y'
  =lfGetInfo()
ELSE
  lcStyDesc = Desc1
  SHOW GET ibDye    ENABLE
  SHOW GET m.Dyelot ENABLE
ENDIF

IF SEEK(lcFromWare+&lcFileNamA..LocFrom+m.Style,'WHBINLOC')
  m.stk1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
  m.stk2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
  m.stk3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
  m.stk4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
  m.stk5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
  m.stk6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
  m.stk7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
  m.stk8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
  m.Totstk =WHBINLOC.TotQty - WHBINLOC.TotAlo
  *! C123853,1 MHM 01/15/2005  Consider Physical case [Start]
  IF lcType = 'P'
    m.Adj1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
    m.Adj2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
    m.Adj3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
    m.Adj4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
    m.Adj5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
    m.Adj6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
    m.Adj7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
    m.Adj8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
    m.TotAdj =WHBINLOC.TotQty - WHBINLOC.TotAlo
  ENDIF  
  *! C123853,1 MHM 01/15/2005  [End]

  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laTStk[9] = m.Totstk
  
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laOTStk[9] = m.Totstk
  

ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    m.Stk&lcCount = 0
  ENDFOR
  m.tOTstk  =0
  
  *! C123853,1 MHM 01/15/2005  Consider Physical case[Start]
  IF lcType = 'P'
    FOR lnCount = 1 TO 8 
      lcCount = ALLTRIM(STR(lnCount))
      m.Adj&lcCount = 0
    ENDFOR
    m.TotAdj = 0
  ENDIF
  *! C123853,1 MHM 01/15/2005  [End]

  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = 0
  ENDFOR
  laTStk[9] = 0

  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = 0
  ENDFOR
  laOTStk[9] = 0
ENDIF

*! C123853,1 MHM 01/15/2005  Consider Physical case[Start]
SHOW GET    m.Adj1   ENABLE
SHOW GET    m.Adj2   ENABLE
SHOW GET    m.Adj3   ENABLE
SHOW GET    m.Adj4   ENABLE
SHOW GET    m.Adj5   ENABLE
SHOW GET    m.Adj6   ENABLE
SHOW GET    m.Adj7   ENABLE
SHOW GET    m.Adj8   ENABLE
*! C123853,1 MHM 01/15/2005  [End]

=lfRefresh('ICINVT_A')
RETURN .T.

*!**************************************************************************
*! Name      : lfTRVLDSTY
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Validate Style in case of IC Transfeer.
*!**************************************************************************
*! Example   : = lfDLVLDSTY()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfTRVLDSTY
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN .F.
ENDIF

PRIVATE lcFileNama
lcFileNama = lcTmpAdj+"A"

*SET ORDER TO TAG WhsLoc IN WHSLOC 
*IF !SEEK(lcFromWare+&lcFileNamA..LocFrom+m.style,'WHSLOC')
*  SET ORDER TO TAG WhsLocSt IN WHSLOC 
*  lcMsg2 = 'No bin location found for Style '+ m.style+ ' Ware House ' +lcFromWare
*  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
*   RETURN
*ENDIF

IF !SEEK(lcToWare+&lcFileNamA..LocTo+m.style,'WHSLOC')
  =SEEK(lcToWare+&lcFileNamA..LocTO,'WHSLOC')
  lcLocSty = m.style
  SCATT MEMVAR MEMO
  m.cWareCode = lcToWare
  m.style     = lcLocSty
  m.clocation = &lcFileNamA..LocTo
  INSERT INTO WHSLOC FROM MEMVAR 
ENDIF
SET ORDER TO TAG WhsLocSt IN WHSLOC 

IF !llDyelot OR Style.cDye_Flg<>'Y'
  =lfGetInfo()
ELSE
  lcStyDesc = Desc1
  SHOW GET ibDye    ENABLE
  SHOW GET m.Dyelot ENABLE
  =lfRefresh('ICINVT_A')
ENDIF
IF SEEK(lcFromWare+&lcFileNamA..LocFrom+M.Style,'WHBINLOC')
*IF SEEK(lcToWare+&lcFileNamA..LocTo+M.Style,'WHBINLOC')
  m.stk1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
  m.stk2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
  m.stk3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
  m.stk4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
  m.stk5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
  m.stk6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
  m.stk7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
  m.stk8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
  m.tOTstk =WHBINLOC.TotQty - WHBINLOC.TotAlo
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laTStk[9] = m.Totstk
ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    m.Stk&lcCount = 0
  ENDFOR
  m.tOTstk  =0
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = 0
  ENDFOR
  laTStk[9] = 0
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = 0
  ENDFOR
  laOTStk[9] = 0
ENDIF

IF SEEK(lcToWare+&lcFileNamA..LocTo+M.Style,'WHBINLOC')
  laTStk[1]  =WHBINLOC.Qty1 - WHBINLOC.Alo1
  laTStk[2]  =WHBINLOC.Qty2 - WHBINLOC.Alo2
  laTStk[3]  =WHBINLOC.Qty3 - WHBINLOC.Alo3
  laTStk[4]  =WHBINLOC.Qty4 - WHBINLOC.Alo4
  laTStk[5]  =WHBINLOC.Qty5 - WHBINLOC.Alo5
  laTStk[6]  =WHBINLOC.Qty6 - WHBINLOC.Alo6
  laTStk[7]  =WHBINLOC.Qty7 - WHBINLOC.Alo7
  laTStk[8]  =WHBINLOC.Qty8 - WHBINLOC.Alo8
  laTStk[9]  =WHBINLOC.TotQty - WHBINLOC.TotAlo
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = laTStk[lnCount]
  ENDFOR
  laOTStk[9] =laTStk[9] 
ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = 0
    laOTStk[lnCount] = 0
  ENDFOR
  laOTStk[9] = 0
  laTStk[9] = 0
ENDIF
=lfRefresh('ICINVT_A')
RETURN .T.

*!**************************************************************************
*! Name      : lfDLMODBRW
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Validate Style in case of IC Adj.
*!**************************************************************************
*! Example   : = lfDLMODBRW()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLMODBRW

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

PRIVATE lcFileNama
lcFileNama = lcTmpAdj+"A"

IF llDyelot
  IF lcType = 'T'

    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R ,"+;
                 "STYLE.DESC1 :24 :H='Description':10 :R,"+;
                 "Dyelot      :19 :H='Dyelot':R,"+;
                 "TotOld      :10 :H=' '+lcFromWare :R,"+;
                 "Total1 = TotAdj      :10 :H='B/Location '+&lcFileNama..LocFrom :R,"+;
                 "TotAdj      :10 :H='Transf.':R,"+;
                 "Total=(IIF(SEEK(Style+lcToWare+Dyelot,'STYDYE'),STYDYE.TotStk,0)+TotAdj) :10 :P='9999999' :H=' '+lcToWare :R,"+;
                 "Total2 = TotAdj  :10 :P='9999999' :H='B/Location '+&lcFileNama..locTo :R "
  ELSE
    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE        :30 :H=lcStyHdr :R ,"+;
                 "STYLE.DESC1  :24 :H='Description' :R,"+;
                 "Dyelot       :19 :H='Dyelot':R,"+;
                 "TotOld       :10  :H='Existing':R,"+;
                 "Old_Cost     :09:P='999999.99'  :H='Old Cost':R,"+;
                 "&lcFileNama..LocFrom    :10 :H='Bin Location' :R,"+;   
                 "TotAdj       :10  :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
                 "Unt_Cost     :09:P='999999.99'  :H='New Cost':R,"+;
                 "Total=IIF(lcType='P',TotAdj,(TotOld+TotAdj)) :10:P='9999999' :H='  Total':R "
  
  ENDIF               
ELSE
  IF lcType = 'T'
    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R,"+;
                 "STYLE.DESC1 :24 :H='Description':R,"+;
                 "TotOld      :12 :H=' '+lcFromWare :R,"+;
                 "Total1 = TotAdj       :12 :H='B/Location '+&lcFileNama..locFrom :R,"+;
                 "TotAdj      :12 :H='Transfer':R,"+;
                 "nTotOldTo   :12 :P='9999999' :H=' '+lcToWare :R,"+;
                 "Total2 = TotAdj   :12 :P='9999999' :H='B/Location '+&lcFileNama..LocTo :R "
  ELSE
    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R,"+;
                 "STYLE.DESC1 :24 :H='Description':R,"+;
                 "TotOld      :12 :H='Existing':R,"+;
                 "Old_Cost    :09:P='999999.99' :H='Old Cost':R,"+;
                 "&lcFileNama..locFrom  :12 :H='B/Location  ' :R,"+;
                 "TotAdj      :12 :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
                 "Unt_Cost    :09:P='999999.99' :H='New Cost':R,"+;
                 "Total=IIF(lcType='P',TotAdj,(TotOld+TotAdj)) :12 :P='9999999' :H='  Total':R "
  ENDIF               
ENDIF
*!**************************************************************************
*! Name      : lfDLCHKSAV
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Validate Po Receiveing 
*!**************************************************************************
*! Example   : = lfDLCHKSAV()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLCHKSAV
PRIVATE llmessag
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN .F.
ENDIF

llmessag  = .F.
llChckLoc = .F.
llChckCls = .F.

*C126994,1 NNA 05/26/2005 (Begin) if the Receiving into Multiple Bins Screen is found then Check Bins from function (lfChkMltBn)
IF ASCAN(laEvntTrig,'CRTBINLN') <> 0 AND ASCAN(laEvntTrig,'DISBNLOC') <> 0
  =lfChkMltBn()
  RETURN llReturn
ENDIF
*C126994,1 NNA (End)

*! C123853,1 MHM 01/15/2005  redim style array [Start]
DIMENSION laStyArr[1]
STORE '' TO laStyArr
*! C123853,1 MHM 01/15/2005  [End]

SELECT (lcTmpLine)
lcOldOrder = ORDER()
SET ORDER TO POSLN
LOCATE

SCAN 
  IF llmessag
    EXIT
  ENDIF
  m.loc1 = &lcTmpLine..loc1
  m.loc2 = &lcTmpLine..loc2
  m.loc3 = &lcTmpLine..loc3
  m.loc4 = &lcTmpLine..loc4
  m.loc5 = &lcTmpLine..loc5
  m.loc6 = &lcTmpLine..loc6
  m.loc7 = &lcTmpLine..loc7
  m.loc8 = &lcTmpLine..loc8
  *--NNA
  *IF Trancd = '1'
  IF Trancd = '1' AND !EOF()
   *--NNA
    SKIP
  ENDIF
  IF Trancd <> '2'
    LOOP
  ENDIF
  
  FOR lnI = 1 TO 8
    lcI = ALLT(STR(lnI,2))
    IF !EMPTY(&lcTmpLine..Qty&lcI)
      IF EMPTY(m.loc&lcI)
        llChckLoc = .T.
        =SEEK(Style,'STYLE')
        IF EMPTY(laStyArr[1])
          laStyArr[1] = Style.cflathang
        ELSE
          IF !(laStyArr[1] = Style.cflathang)
            llChckCls = .T.
            EXIT
          ENDIF
        ENDIF
      ENDIF
    ENDIF  
  ENDFOR
ENDSCAN

SELECT (lcTmpLine)
LOCATE
IF llChckCls
  lcMsg2 = 'You have mixed Flat and Hanging styles on this transaction - please enter the bin locations manually'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llmessag = .T.
  STORE .F. TO llShow,llCSave
  SELECT (lcTmpLine)
  SET ORDER TO &lcOldOrder
  LOCATE
  RETURN llmessag
ENDIF

IF llChckLoc
  *! C123853,1 MHM 01/15/2005  Add message to check assign bin location [Start]
  lcMsg2 = 'You have to assign bin location before saving . Do you want to assign bin location now ?'
  lnChoose =gfModalGen("QRM00000B38006","DIALOG",.F.,.F.,lcMsg2)
  *! C123853,1 MHM 01/15/2005  [End]
  
  IF lnChoose = 1
    IF !USED('WHSLOC') 
      =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
    ENDIF
    DIME laTempData[3]
    
    Store '' TO lcBinLoc , laTempData
    DO (gcScrDir+gcWinAppl+"\POdlbnSC.Spx")
    IF EMPTY(lcBinLoc)
      lcMsg2 = 'You have to assign bin location before saving .'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      llmessag = .T.
      STORE .F. TO llShow,llCSave
      SELECT (lcTmpLine)
      SET ORDER TO &lcOldOrder
      LOCATE
      RETURN llmessag
    ENDIF
    
    =SEEK(&lcTmpLine..Style,'Style')
    IF !(laTempData[3] = style.cflathang)
    *  'You cannot saving the style to a bin of a different Hang/Flat.'
      lcMsg2 = 'You cannot saving the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      llmessag = .T.
      STORE .F. TO llShow,llCSave
      SELECT (lcTmpLine)
      SET ORDER TO &lcOldOrder
      LOCATE
      RETURN llmessag
    ENDIF
  ELSE
    llmessag = .T.
    STORE .F. TO llShow,llCSave
    SELECT (lcTmpLine)
    SET ORDER TO &lcOldOrder
    LOCATE
    RETURN llmessag
  ENDIF  
  SELECT (lcTmpLine)
  SCAN
  *! C123853,1 MHM 01/15/2005  Comment this part no need for it now[Start]
  *IF Trancd = '1'
  *  LOOP
  *ENDIF
  *! C123853,1 MHM 01/15/2005  [End]
  
    REPLACE &lcTmpLine..loc1  WITH laTempData[1],;
            &lcTmpLine..loc2  WITH laTempData[1],;
            &lcTmpLine..loc3  WITH laTempData[1],;
            &lcTmpLine..loc4  WITH laTempData[1],;
            &lcTmpLine..loc5  WITH laTempData[1],;
            &lcTmpLine..loc6  WITH laTempData[1],;
            &lcTmpLine..loc7  WITH laTempData[1],;
            &lcTmpLine..loc8  WITH laTempData[1]
  
  ENDSCAN

ENDIF

SELECT (lcTmpLine)
SET ORDER TO &lcOldOrder
LOCATE

*! C123853,1 MHM 01/15/2005  refresh all object[Start]
SHOW GETS
*! C123853,1 MHM 01/15/2005  [End]

RETURN llmessag
*!**************************************************************************
*! Name      : lfDLCKSVIC
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Validate Ic Adj.
*!**************************************************************************
*! Example   : = lfDLCKSVIC()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLCKSVIC
PRIVATE llmessag
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN .F.
ENDIF

PRIVATE lcFileNama
lcFileNama = lcTmpAdj+"A"

llmessag = .F.
SELECT (lcFileNamA)
LOCATE
IF lcType = 'T'
 IF EMPTY(&lcFileNamA..LocFrom) OR EMPTY(&lcFileNamA..LocTO)
    lcMsg2 = 'You can not add style without assign bin location.'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    llmessag = .T.
  ENDIF  
ELSE
 IF EMPTY(&lcFileNamA..LocFrom )
    lcMsg2 = 'You can not add styles without assign bin location.'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    llmessag = .T.
  ENDIF  
ENDIF

RETURN llmessag


*!**************************************************************************
*! Name      : lfDLCANORD
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Validate Cancelled Qty in Cas or Sales Order.
*!**************************************************************************
*! Example   : = lfDLCKSVIC()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLCANORD
PRIVATE llmessag
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN .F.
ENDIF

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF
SELECT WHBINLOC
lcOldOrder = ORDER()
SET ORDER TO WHBINLOC

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF
    
SELECT (lcOrdLine)
LOCATE
SCAN 
  IF SEEK(&lcOrdLine..piktkt+&lcOrdLine..cwarecode,'PKBINLOC')
    SELECT PKBINLOC
    SCAN REST WHILE PikTkt+cWareCode =&lcOrdLine..piktkt+&lcOrdLine..cwarecode
      IF SEEK(cwarecode+clocation+style,'WHBINLOC')
        REPLACE WHBINLOC.ALO1    WITH WHBINLOC.ALO1 - QTY1,;
                WHBINLOC.ALO2    WITH WHBINLOC.ALO2 - QTY2,;
                WHBINLOC.ALO3    WITH WHBINLOC.ALO3 - QTY3,;
                WHBINLOC.ALO4    WITH WHBINLOC.ALO4 - QTY4,;
                WHBINLOC.ALO5    WITH WHBINLOC.ALO5 - QTY5,;
                WHBINLOC.ALO6    WITH WHBINLOC.ALO6 - QTY6,;
                WHBINLOC.ALO7    WITH WHBINLOC.ALO7 - QTY7,;
                WHBINLOC.ALO8    WITH WHBINLOC.ALO8 - QTY8,;
                WHBINLOC.TotALO  WITH WHBINLOC.TotALO - TotQTY
         DELETE         
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN


SELECT WHBINLOC
SET ORDER TO &lcOldOrder

*:*************************************************************
*! Name        : lfRMDFNMNU
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : RM
*! Purpose     : Define Bin Locations valid
*:*************************************************************
*C037816,1 MHM 04/06/2004 Define New Menu[Start]
FUNCTION lfRMDFNMNU

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

*--check if the option pad is already defined on the sysmenu
IF !lfFoundPad('Options')
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' ' SKIP FOR IIF(TYPE('lcProgName')='C' AND lcProgName = "RMCRMEM",.F.,.T.) ;
  OR (laScrMode[1]) OR (laScrMode[2]) 
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF
lnBarNo = 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Bins Detail" 
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfDLGTRMBN IN DAVMAIN
RETURN


*:*************************************************************
*! Name        : lfRMDFNMNU
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : RM
*! Purpose     : Define Bin Locations valid
*:*************************************************************
*C037816,1 MHM 04/06/2004 Define New Menu[Start]
FUNCTION lfWBnLoc
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN .F.
ENDIF


FOR lnCount = 1 TO 8 
  lcCount = ALLTRIM(STR(lnCount))
  IF !EMPTY(laSize[lnCount])
    SHOW GET m.BinLoc&lcCount ENABLE
  ELSE
    SHOW GET m.BinLoc&lcCount DISABLE
  ENDIF
ENDFOR  

SHOW GETS

*:*************************************************************
*! Name        : lfRMCKSVBN
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : RM
*! Purpose     : Define Bin Locations valid
*:*************************************************************
*C037816,1 MHM 04/06/2004 Define New Menu[Start]
FUNCTION lfRMCKSVBN

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN llCSave
ENDIF

STORE .F. TO  llmessag

*! C123853,1 MHM 01/15/2005  check location if found or not to prevent user to save empty Bin[Start]
llChckLoc = .F.
llChckCls = .F.
DIMENSION laStyArr[1]
STORE '' TO laStyArr
*! C123853,1 MHM 01/15/2005  [End]

SELECT (lcCrMemLin)

LOCATE
SCAN 
  IF llmessag
    EXIT
  ENDIF
  m.loc1 = &lcCrMemLin..Binloc1
  m.loc2 = &lcCrMemLin..Binloc2
  m.loc3 = &lcCrMemLin..Binloc3
  m.loc4 = &lcCrMemLin..Binloc4
  m.loc5 = &lcCrMemLin..Binloc5
  m.loc6 = &lcCrMemLin..Binloc6
  m.loc7 = &lcCrMemLin..Binloc7
  m.loc8 = &lcCrMemLin..Binloc8
  FOR lnI = 1 TO 8
    lcI = ALLT(STR(lnI,2))
    IF !EMPTY(&lcCrMemLin..Qty&lcI)
      IF EMPTY(m.loc&lcI)
        *! C123853,1 MHM 01/15/2005  comment message as per tony [Start]
        *lcMsg2 = 'You cannot save style '+&lcCrMemLin..Style + ' without assign bin location.'
        *=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        *llmessag = .T.
        *STORE .F. TO llShow,llCSave
        *RETURN llCSave

        =SEEK(Style,'STYLE')
        *--mhmcheck 129431 
        *llChckLoc = .T.
        IF STYLE.LINVSTY
          llChckLoc = .T.
        ENDIF
        *--mhm
        
        IF EMPTY(laStyArr[1])
          laStyArr[1] = Style.cflathang
        ELSE
          IF !(laStyArr[1] = Style.cflathang)
            llChckCls = .T.
            EXIT
          ENDIF
        ENDIF
        *! C123853,1 MHM 01/15/2005  [Start]
        
      ENDIF
    ENDIF  
  ENDFOR
ENDSCAN

*! C123853,1 MHM 01/15/2005  modification as per tony to check bins[Start]
SELECT (lcCrMemLin)
LOCATE
IF llChckCls
  lcMsg2 = 'You have mixed Flat and Hanging styles on this transaction - please enter the bin locations manually'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llmessag = .T.
  STORE .F. TO llShow,llCSave
  RETURN llCSave
ENDIF
*! C123853,1 MHM 01/15/2005  [End]

IF llChckLoc
  *! C123853,1 MHM 01/15/2005  message as per tony to check before saving [Start]
  lcMsg2 = 'You have to assign bin location before saving . Do you want to assign bin location now ?'
  lnChoose =gfModalGen("QRM00000B38006","DIALOG",.F.,.F.,lcMsg2)
  *! C123853,1 MHM 01/15/2005   [End]

  IF lnChoose = 1
    IF !USED('WHSLOC') 
      =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
    ENDIF
    DIME laTempData[3]
    lcWarecode = laData[29]
    Store '' TO lcBinLoc , laTempData
    DO (gcScrDir+'PO'+"\POdlbnSC.Spx")
    IF EMPTY(lcBinLoc)
      lcMsg2 = 'You have to assign bin location before saving .'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      llmessag = .T.
      STORE .F. TO llShow,llCSave
      RETURN llCSave
    ENDIF
    
    =SEEK(&lcCrMemLin..Style,'Style')
    IF !(WhsLoc.cFlatHang = style.cflathang)
    *  'You cannot saving the style to a bin of a different Hang/Flat.'
      lcMsg2 = 'You cannot saving the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      llmessag = .T.
      STORE .F. TO llShow,llCSave
      RETURN llCSave
    ENDIF
  ELSE
    llmessag = .T.
    STORE .F. TO llShow,llCSave
    RETURN llCSave
  ENDIF  
  SELECT (lcCrMemLin)
  SCAN
    REPLACE &lcCrMemLin..Binloc1  WITH laTempData[1],;
            &lcCrMemLin..Binloc2  WITH laTempData[1],;
            &lcCrMemLin..Binloc3  WITH laTempData[1],;
            &lcCrMemLin..Binloc4  WITH laTempData[1],;
            &lcCrMemLin..Binloc5  WITH laTempData[1],;
            &lcCrMemLin..Binloc6  WITH laTempData[1],;
            &lcCrMemLin..Binloc7  WITH laTempData[1],;
            &lcCrMemLin..Binloc8  WITH laTempData[1]
  ENDSCAN
  SHOW GETS

ENDIF


RETURN llCSave

*!**************************************************************************
*! Name      : lfARDFNMNU
*! Developer : Mohamed Shokry()
*! Date      : 12/12/2002
*! Purpose   : Add a new entry to the Option Menu - "Enter Customer Prices".
*!**************************************************************************

FUNCTION lfARDFNMNU

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  *B608092,1 TMI [Start] 
  *RETURN .F.
  RETURN
  *B608092,1 TMI [End  ] 
ENDIF

lnBarNo = CNTBAR('_INQURYPOP') + 1

DEFINE BAR lnBarNo OF _INQURYPOP PROMPT "\<Bin Locations" SKIP FOR (laScrMode[1]) OR (laScrMode[2])
ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfARGETBIN IN DAVMAIN
 
*:*************************************************************
*! Name        : lfWArBnLoc
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : RM
*! Purpose     : Define Bin Locations valid
*:*************************************************************
*C037816,1 MHM 04/06/2004 Define New Menu[Start]
FUNCTION lfWArBnLoc

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN .F.
ENDIF


FOR lnCount = 1 TO 8 
  lcCount = ALLTRIM(STR(lnCount))
  IF !EMPTY(Scale.Sz&lcCount)
    SHOW GET m.BinLoc&lcCount ENABLE
  ELSE
    SHOW GET m.BinLoc&lcCount DISABLE
  ENDIF
ENDFOR  
SHOW GETS
   

*:*************************************************************
*! Name        : lfARCKSVBN
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : AR
*! Purpose     : Define Bin Locations valid
*:*************************************************************
*C037816,1 MHM 04/06/2004 Define New Menu[Start]
FUNCTION lfARCKSVBN

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  *B607992,1 TMI [Start] if bin location is not used for a company then return with .T. to complete standard normal saving
  *RETURN .F.
  RETURN .T.
  *B607992,1 TMI [Start] 
ENDIF

STORE .F. TO  llmessag
SELECT (lcInvLine)
*lcOldOrder = ORDER()
*SET ORDER TO
LOCATE
SCAN 
  IF llmessag
    EXIT
  ENDIF
  m.loc1 = &lcInvLine..Binloc1
  m.loc2 = &lcInvLine..Binloc2
  m.loc3 = &lcInvLine..Binloc3
  m.loc4 = &lcInvLine..Binloc4
  m.loc5 = &lcInvLine..Binloc5
  m.loc6 = &lcInvLine..Binloc6
  m.loc7 = &lcInvLine..Binloc7
  m.loc8 = &lcInvLine..Binloc8
  FOR lnI = 1 TO 8
    lcI = ALLT(STR(lnI,2))

    *B128959,1 NNA 09/04/2005 (Start) Don't assign Bin Location if Not Style.linvsty
    *IF !EMPTY(&lcInvLine..Qty&lcI)
    IF !EMPTY(&lcInvLine..Qty&lcI) AND STYLE.LINVSTY
    *B128959,1 NNA (End)

      IF EMPTY(m.loc&lcI)
        lcMsg2 = 'You cannot save style '+&lcInvLine..Style + ' without assign bin location.'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        llmessag = .T.
        STORE .F. TO llShow,llCSave
        RETURN llCSave
      ENDIF
    ENDIF  
  ENDFOR
ENDSCAN
*SET ORDER TO &lcOldOrder

RETURN llCSave
*:*************************************************************
*! Name        : lfALMODAUT
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : AL
*! Purpose     : Modify Piked Qty
*:*************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfALMODAUT
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN .F.
ENDIF


lnAlias    = SELECT()
*-- lnQty -------> Qty per size
*-- lnSize -------> size
*-- lcPkTkt -------> PikTkt
*-- lcOrder -------> Order

lcSize = ALLT(STR(lnParm,2))

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF

IF SEEK(&lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode ,'PKBINLOC') 
  SELECT PKBINLOC
  SCAN REST WHILE PikTkt+cWareCode = &lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode ;
                  FOR STYLE = &lcTmpOrdLn..Style
                  
    IF SEEK(PKBINLOC.cWareCode+PKBINLOC.cLocation+PKBINLOC.Style ,'WHBINLOC')   
      REPLACE WHBINLOC.Alo&lcSize WITH WHBINLOC.Alo&lcSize +laPik[lnParm] - laOldVal,;
              WHBINLOC.TotAlo     WITH WHBINLOC.TotAlo +laPik[lnParm] - laOldVal
    ENDIF
    SELECT PKBINLOC
    REPLACE Qty&lcSize WITH laPik[lnParm],;
            TotQty     WITH TotQty +laPik[lnParm] - laOldVal
    IF TotQty = 0
      DELETE
    ENDIF 
  ENDSCAN  
ENDIF

SELECT(lnAlias)

*:**************************************************************************
*:* Name        : lfLDFNPOIS  
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : PO
*:* Purpose     : Call Issue Screen with modification of Bin Location
*:***************************************************************************
*! C123853,1 MHM 01/15/2005   Custom Screen for DL[Start]
FUNCTION lfLDFNPOIS  

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  lnAlias = SELECT()

  IF !USED('WHSLOC') 
    =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
  ENDIF

  IF !USED('WHBINLOC')
    =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
  ENDIF
  PRIVATE m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
  STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
  STORE '' TO lcBinLoc
  *--create array for locations 

  DO (gcScrDir+"PODLSISS.SPX") WITH .T.
  SELECT(lnAlias)
ELSE
    DO (gcScrDir+"MFSTYISS.SPX") WITH .T.
ENDIF


*:**************************************************************************
*:* Name        : lfvPoBnLc
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : PO
*:* Purpose     : Validate bin location
*:***************************************************************************
*! C123853,1 MHM 01/15/2005  Custom Screen for DL[Start]
FUNCTION lfvPoBnLc

PRIVATE llFromSeek
STORE .F.TO llFromSeek
=gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')

SELECT WhsLoc
IF EMPTY(lcBinLoc) AND !llbrowse
  RETURN
ENDIF
SET ORDER TO TAG WhsBin IN  WHSLOC

IF llbrowse OR (!EMPTY(lcBinLoc) AND !SEEK(lcBinLoc+lcIssWare))
  IF llIssue
    llbrowse =.F. 
    lnScaleCnt = Scale.cnt
    lcBrFields = "clocation  :H='Bin',"+;
                 "cBinClass  :H='Bin Class',"
    FOR lnI=1 TO lnScaleCnt
      lcI=STR(lnI,1)
      IF !EMPTY(Scale.Sz&lcI)
        lcBrFields = lcBrFields + "Qty"+lcI+" :H=PADL(Scale.Sz"+lcI+",5) :P='9999999',"
      ENDIF
    ENDFOR
    lcBrFields = lcBrFields + "TotQty :H='Total ' :P='999999999',"

    *--add alocated data  **PADL(lcSz"+lcI+",5)
    FOR lnI=1 TO lnScaleCnt
      lcI=STR(lnI,1)
      IF !EMPTY(Scale.Sz&lcI)
        lcBrFields = lcBrFields + "Alo"+lcI+" :H='Alo '+PADL(Scale.Sz"+lcI+",5)  :P='9999999',"
      ENDIF
    ENDFOR
    lcBrFields = lcBrFields + "TotAlo :H='Total Alocated ' :P='999999999'"

    DIME laTempData[3]
    STORE '' TO laTempData
    =SEEK(ALLTRIM(lcBinLoc))
  
    SELECT WHBINLOC 
    lcForExpr = " cWareCode = lcIssWare  AND (TotQty-TotAlo>0) AND STYLE = m.Item"
    =gfBrows([FOR &lcForExpr],'cLocation,cBinClass','laTempData','WHBINLOC')
    lcBinLoc=laTempData[1]
  ELSE
    llbrowse =.F. 
    lcBrFields = "clocation  :H='Bin',"+;
                 "cBinClass  :H='Bin Class'"
    DIME laTempData[3]
    STORE '' TO laTempData
    =SEEK(ALLTRIM(lcBinLoc))
    lcForExpr = " cWareCode = lcIssWare  AND EMPTY(Style)"
    =gfBrows([FOR &lcForExpr],'cLocation,cBinClass,cFlatHang','laTempData','WhsLoc')
    lcBinLoc=laTempData[1]
  ENDIF
  SELECT WHBINLOC
  IF SEEK(lcIssWare+lcBinLoc+m.Item)
    m.Bin_Qty1 = Qty1 - Alo1
    m.Bin_Qty2 = Qty2 - Alo2
    m.Bin_Qty3 = Qty3 - Alo3
    m.Bin_Qty4 = Qty4 - Alo4
    m.Bin_Qty5 = Qty5 - Alo5
    m.Bin_Qty6 = Qty6 - Alo6
    m.Bin_Qty7 = Qty7 - Alo7
    m.Bin_Qty8 = Qty8 - Alo8 
  ELSE
    m.Bin_Qty1 = 0
    m.Bin_Qty2 = 0
    m.Bin_Qty3 = 0
    m.Bin_Qty4 = 0
    m.Bin_Qty5 = 0
    m.Bin_Qty6 = 0
    m.Bin_Qty7 = 0
    m.Bin_Qty8 = 0
  ENDIF
  SHOW GETS  
ELSE
  SELECT WHBINLOC
  IF SEEK(lcIssWare+lcBinLoc+m.Item)
    m.Bin_Qty1 = Qty1 - Alo1
    m.Bin_Qty2 = Qty2 - Alo2
    m.Bin_Qty3 = Qty3 - Alo3
    m.Bin_Qty4 = Qty4 - Alo4
    m.Bin_Qty5 = Qty5 - Alo5
    m.Bin_Qty6 = Qty6 - Alo6
    m.Bin_Qty7 = Qty7 - Alo7
    m.Bin_Qty8 = Qty8 - Alo8 
  ELSE
    m.Bin_Qty1 = 0
    m.Bin_Qty2 = 0
    m.Bin_Qty3 = 0
    m.Bin_Qty4 = 0
    m.Bin_Qty5 = 0
    m.Bin_Qty6 = 0
    m.Bin_Qty7 = 0
    m.Bin_Qty8 = 0
  ENDIF
  SHOW GETS  
ENDIF

SET ORDER TO TAG WhsLocSt IN  WHSLOC

*:**************************************************************************
*:* Name        : lfLDFNPORT
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : PO
*:* Purpose     : Call Return Screen with modification of Bin Location
*:***************************************************************************
**! C123853,1 MHM 01/15/2005  Custom Screen for DL[Start]
FUNCTION lfLDFNPORT  

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 


IF llUseBin
  lnAlias = SELECT()

  IF !USED('WHSLOC') 
    =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
  ENDIF

  IF !USED('WHBINLOC')
    =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
  ENDIF
  PRIVATE m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
  STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
  STORE '' TO lcBinLoc
  *--create array for locations 
  
  DO (gcScrDir+"PODLSISS.SPX") WITH .F.

  SELECT(lnAlias)
ELSE
    DO (gcScrDir+"MFSTYISS.SPX") WITH .F.
ENDIF

*:**************************************************************************
*:* Name        : lfDLVLDWRH
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : PO
*:* Purpose     : Valid Ware House with bin location
*:***************************************************************************
*! C123853,1 MHM 01/15/2005  Custom Screen for DL[Start]
FUNCTION lfDLVLDWRH

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin

  STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
  STORE '' TO lcBinLoc

  lcIssWare = laStyWare[lnIssWare,2]
  =SEEK(m.Item+lcIssWare+space(10),'StyDye')
  lnIssCost = IIF(laSetups[10,2]='A',StyDye.Ave_Cost,Style.TotCost)
  =lfRefresh('PODLSISS')
  SHOW GETS 
ELSE
  lcIssWare = laStyWare[lnIssWare,2]
  =SEEK(m.Item+lcIssWare+space(10),'StyDye')
  lnIssCost = IIF(laSetups[10,2]='A',StyDye.Ave_Cost,Style.TotCost)
  =lfRefresh('MFSTYISS')
  SHOW GETS WINDOW 'MFSTYISS' ONLY
ENDIF

*:**************************************************************************
*:* Name        : lfDLUPDQTY
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : PO
*:* Purpose     : Update BinInvJL & WHBINLOC file
*:***************************************************************************
*:lfDLUPDQTY Trigger called from three Places
*:1 - from manual Issue (TYPE('lcBinLoc') <> 'U')
*:2 - from Automatic Issue (TYPE('lcBinLoc') = 'U')
*:1 - from Delete Po Cost sheet llFromDel
*! C123853,1 MHM 01/15/2005  Custom Screen for DL[Start]

FUNCTION lfDLUPDQTY

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

DIMENSION laOldQty[9]
STORE 0 TO laOldQty
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF

IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF
SELECT STYINVJL
SCATT MEMVAR MEMO
lcGlSession = STYINVJL.csession
IF TYPE('lcBinLoc') = 'U' AND !llFromDel
  ***laIssued
  m.Item = &lcIssLtFile..Item
  SELECT WHBINLOC
  lcOrdOrdr = ORDER()
  SET ORDER TO Whbinlst
  IF SEEK(m.Item + laData[32])
      SELECT STYINVJL
      =SEEK(m.Item + laData[32] + lcGlSession)
      SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) =;
                      m.Item + laData[32] + lcGlSession     
        SCATT MEMVAR MEMO
        SELECT WHBINLOC    
        =SEEK(m.Item + laData[32])
        SCAN REST WHILE style+cwarecode+clocation = m.Item + laData[32]
          IF m.nTOtStk <> 0
            STORE 0 TO laOldQty
            FOR lnI  = 1 TO 8
              lcI = ALLT(STR(lnI,2))
              IF (m.nStk&lcI = 0 ) OR Qty&lcI - ALO&lcI + m.nStk&lcI > 0
                laOldQty[lnI] = m.nStk&lcI
                *--mhmwael129109
                *laOldQty[9]   = laOldQty[9] - m.nStk&lcI
                *m.nTOtStk     = m.nTOtStk + m.nStk&lcI
                laOldQty[9]    = laOldQty[9] + m.nStk&lcI
                m.nTOtStk      = m.nTOtStk - m.nStk&lcI
                *--mhmwael129109
                m.nStk&lcI    = 0
              ELSE
                laOldQty[lnI] = -1 * Qty&lcI
                laOldQty[9]   = laOldQty[9] -1 * Qty&lcI
                m.nStk&lcI    = m.nStk&lcI + Qty&lcI
                m.nTOtStk     = m.nTOtStk + Qty&lcI
              ENDIF
            ENDFOR
            IF laOldQty[9] <>0 
              REPLACE QTY1   WITH QTY1   + laOldQty[1],;
                      QTY2   WITH QTY2   + laOldQty[2],;  
                      QTY3   WITH QTY3   + laOldQty[3],;  
                      QTY4   WITH QTY4   + laOldQty[4],;  
                      QTY5   WITH QTY5   + laOldQty[5],;  
                      QTY6   WITH QTY6   + laOldQty[6],;  
                      QTY7   WITH QTY7   + laOldQty[7],;  
                      QTY8   WITH QTY8   + laOldQty[8],;
                      TOTQTY WITH TOTQTY + laOldQty[9]
              m.clocation = clocation
              SELECT BININVJL
              APPEND BLANK
              GATHER MEMVAR MEMO
              REPLACE NSTK1   WITH laOldQty[1],;
                      NSTK2   WITH laOldQty[2],;  
                      NSTK3   WITH laOldQty[3],;  
                      NSTK4   WITH laOldQty[4],;  
                      NSTK5   WITH laOldQty[5],;  
                      NSTK6   WITH laOldQty[6],;  
                      NSTK7   WITH laOldQty[7],;  
                      NSTK8   WITH laOldQty[8],;
                      NTOTSTK WITH laOldQty[9],;
                      nstkval WITH laOldQty[9] * nCost
             ENDIF           
          ELSE
            EXIT
          ENDIF
        ENDSCAN    
      ENDSCAN
    *ENDIF        
  ENDIF  
  SELECT WHBINLOC
  SET ORDER TO &lcOrdOrdr 
ELSE
  IF llFromDel
    IF !USED('WHSLOC') 
      =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
    ENDIF
    DIME laTempData[3]
    
    Store '' TO lcBinLoc , laTempData
    DO (gcScrDir+gcWinAppl+"\POdlbnSC.Spx")
    IF EMPTY(lcBinLoc)
      DO WHILE EMPTY(lcBinLoc)
        lcMsg2 = 'You have to assign bin location before saving .'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        DO (gcScrDir+gcWinAppl+"\POdlbnSC.Spx")
      ENDDO  
    ENDIF
    m.Iss_Qty1 = laIssued[1] 
    m.Iss_Qty2 = laIssued[2] 
    m.Iss_Qty3 = laIssued[3] 
    m.Iss_Qty4 = laIssued[4] 
    m.Iss_Qty5 = laIssued[5] 
    m.Iss_Qty6 = laIssued[6] 
    m.Iss_Qty7 = laIssued[7] 
    m.Iss_Qty8 = laIssued[8] 
    m.Item = STYINVJL.Style
  ENDIF
  
  SELECT STYINVJL
  =SEEK(m.Item + laData[32] + lcGlSession)
  SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) =;
                  m.Item + laData[32] + lcGlSession     
    SCATT MEMVAR MEMO              
    m.clocation = lcBinLoc
    SELECT BININVJL
    APPEND BLANK
    GATHER MEMVAR MEMO
    =gfAdd_Info('BININVJL')
    
  ENDSCAN
  
  SELECT WHBINLOC
  IF SEEK(laData[32]+lcBinLoc+m.Item)
    IF cirtype = 'R'
      REPLACE QTY1 WITH QTY1 + m.Iss_Qty1,;
              QTY2 WITH QTY2 + m.Iss_Qty2,;  
              QTY3 WITH QTY3 + m.Iss_Qty3,;  
              QTY4 WITH QTY4 + m.Iss_Qty4,;  
              QTY5 WITH QTY5 + m.Iss_Qty5,;  
              QTY6 WITH QTY6 + m.Iss_Qty6,;  
              QTY7 WITH QTY7 + m.Iss_Qty7,;  
              QTY8 WITH QTY8 + m.Iss_Qty8
      REPLACE TOTQTY WITH TOTQTY +(m.Iss_Qty1+m.Iss_Qty2+m.Iss_Qty3+m.Iss_Qty4+m.Iss_Qty5+m.Iss_Qty6+m.Iss_Qty7+m.Iss_Qty8)
    ELSE
      REPLACE QTY1 WITH QTY1 - m.Iss_Qty1,;
              QTY2 WITH QTY2 - m.Iss_Qty2,;  
              QTY3 WITH QTY3 - m.Iss_Qty3,;  
              QTY4 WITH QTY4 - m.Iss_Qty4,;  
              QTY5 WITH QTY5 - m.Iss_Qty5,;  
              QTY6 WITH QTY6 - m.Iss_Qty6,;  
              QTY7 WITH QTY7 - m.Iss_Qty7,;  
              QTY8 WITH QTY8 - m.Iss_Qty8
      REPLACE TOTQTY WITH TOTQTY -(m.Iss_Qty1+m.Iss_Qty2+m.Iss_Qty3+m.Iss_Qty4+m.Iss_Qty5+m.Iss_Qty6+m.Iss_Qty7+m.Iss_Qty8)
      IF TOTQTY = 0
        DELETE       
      ENDIF
    ENDIF    
  ELSE
    IF cirtype = 'R'
      m.clocation = lcBinLoc
      m.Style     = m.Item
      =SEEK(laData[32]+m.clocation,'WHSLOC')
      m.cbinclass = WHSLOC.cbinclass
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE QTY1 WITH QTY1 + m.Iss_Qty1,;
              QTY2 WITH QTY2 + m.Iss_Qty2,;  
              QTY3 WITH QTY3 + m.Iss_Qty3,;  
              QTY4 WITH QTY4 + m.Iss_Qty4,;  
              QTY5 WITH QTY5 + m.Iss_Qty5,;  
              QTY6 WITH QTY6 + m.Iss_Qty6,;  
              QTY7 WITH QTY7 + m.Iss_Qty7,;  
              QTY8 WITH QTY8 + m.Iss_Qty8
      REPLACE TOTQTY WITH TOTQTY +(m.Iss_Qty1+m.Iss_Qty2+m.Iss_Qty3+m.Iss_Qty4+m.Iss_Qty5+m.Iss_Qty6+m.Iss_Qty7+m.Iss_Qty8)
      =gfAdd_Info('WHBINLOC')
      
    ENDIF  
  ENDIF
ENDIF

*SELECT STYINVJL
*SET ORDER TO &lcStyOrd 

*:**************************************************************************
*:* Name        : lfDLSVBNI
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : PO
*:* Purpose     : Update BinInvJL & from RECEIVE Styinvjl
*:***************************************************************************
*! C123853,1 MHM 01/15/2005  Custom Screen for DL[Start]
FUNCTION lfDLSVBNR
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

PRIVATE lcStyOrd,lnAlias
lnAlias = SELECT(0)
IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF

IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF

PRIVATE lcFileNama ,lnCrRec 
lcFileNama = lcTmpAdj+"A"

SELECT STYINVJL
lnCrRec = RECNO()
SEEK Style+&lcTmpAdj..cToWare+lcRcvSessNo
 
SCATT MEMVAR MEMO
m.clocation = &lcFileNama..LocTo

SELECT BININVJL
APPEND BLANK
GATHER MEMVAR MEMO

SELECT STYINVJL
GOTO lnCrRec 

*:**************************************************************************
*:* Name        : lfDLSVBNI
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : PO
*:* Purpose     : Update BinInvJL & from issue Styinvjl
*:***************************************************************************
*! C123853,1 MHM 01/15/2005  Custom Screen for DL[Start]
FUNCTION lfDLSVBNI
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

PRIVATE lcStyOrd,lnAlias
lnAlias = SELECT(0)
IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF

IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF
PRIVATE lcFileNama
lcFileNama = lcTmpAdj+"A"
 
SELECT STYINVJL 
lnCurrec = RECNO()

*! B128987,1 MHM 07/14/2005 Fix the bug of not working correct if transfer from warehouse to another [Start]
lcsty1 = Style
lcWarhos = &lcTmpAdj..cFromWare
*! B128987,1 MHM 07/14/2005 [End]

SEEK Style+&lcTmpAdj..cFromWare+lcIsuSessNo

*! B128987,1 MHM 07/14/2005 Fix the bug of not working correct if transfer from warehouse to another [Start]
*SCAN WHILE Style+cWareCode+cSession = Style+&lcTmpAdj..cFromWare+lcIsuSessNo
IF EOF()
  lcWarhos = &lcTmpAdj..cToWare
  SEEK lcsty1 + lcWarhos + lcIsuSessNo
ENDIF
SCAN WHILE Style+cWareCode+cSession = lcsty1 + lcWarhos + lcIsuSessNo
*! B128987,1 MHM 07/14/2005 [END]

  SCATT MEMVAR MEMO

  IF cirtype = 'I'
    m.clocation = &lcFileNama..LocFrom
  ELSE
    m.clocation = &lcFileNama..LocTO
    IF EMPTY(&lcFileNama..LocTO)
      m.clocation = &lcFileNama..LocFrom
    ENDIF  
  ENDIF  

  SELECT BININVJL
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

SELECT STYINVJL 
GOTO lnCurrec
SELECT(lnAlias)

*:**************************************************************************
*:* Name        : lfDLSBNPOR
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : PO
*:* Purpose     : Update BinInvJL & from issue Styinvjl
*:***************************************************************************
*! C123853,1 MHM 01/15/2005 Custom Screen for DL[Start]
FUNCTION lfDLSBNPOR
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

*C126994,1 NNA 05/26/2005 (Begin) if the Receiving into Multiple Bins Screen is found then Save from function (lfSavMltBn)
IF ASCAN(laEvntTrig,'CRTBINLN') <> 0 AND ASCAN(laEvntTrig,'DISBNLOC') <> 0

  *--mhm2006
  SELECT SYCTRIGG
  lncurrec = RECNO()
  lcCurOrder  = ORDER()
  SET ORDER TO Mainprog
  IF SEEK('PANMAIN')
    lcMsg2 = 'You can not run bin location, Please Contact Aria Systems'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    SELECT SYCTRIGG
    GOTO lncurrec 
    SET ORDER TO lcCurOrder  
    RETURN
  ENDIF
  SELECT SYCTRIGG
  GOTO lncurrec 
  SET ORDER TO lcCurOrder  
  *--mhm2006

  =lfUpdBnInv()
  RETURN
ENDIF
*C126994,1 NNA (End)

PRIVATE lcStyOrd,lnAlias,lcCurTmp 
DIMENSION laBinArr[8]

lnAlias = SELECT(0)
IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF

IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF
SELECT (lcTmpLine)
IF Trancd = '2'
  SKIP-1
  FOR lncI = 1 TO 8 
    lccI = ALLTRIM(STR(lncI,1))
    laBinArr[lncI] =  &lcTmpLine..loc&lccI
  ENDFOR
  *--NNA
  IF !EOF()
  *--NNA
    SKIP
  ENDIF
ENDIF
lcCurTmp = gfTempName()
CREATE TABLE (gcWorkDir + lcCurTmp);
       ( nstk1 N(7), nstk2 N(7),nstk3 N(7),nstk4 N(7),nstk5 N(7),nstk6 N(7),nstk7 N(7),;
       nstk8 N(7), nTotstk N(7), Binloc C(10) )
INDEX ON Binloc  TAG (lcCurTmp)
SELECT (lcCurTmp)
FOR lni = 1 To 8
  lcI = ALLTRIM(STR(lni,1))
  IF &lcTmpLine..Qty&lcI<> 0  
    IF !SEEK(laBinArr[lni],lcCurTmp) 
      APPEND BLANK
      REPLACE nstk&lcI WITH &lcTmpLine..Qty&lcI,;
              nTotstk  WITH &lcTmpLine..Qty&lcI,;
              Binloc   WITH laBinArr[lni]
    ELSE
      REPLACE nstk&lcI WITH &lcTmpLine..Qty&lcI,;
              nTotstk  WITH nTotstk + &lcTmpLine..Qty&lcI
    ENDIF
  ENDIF  
ENDFOR
SELECT (lcCurTmp)
LOCATE
IF EOF()
  SELECT(lnAlias)
  RETURN
ENDIF
SELECT STYINVJL
=SEEK(&lcTmpLine..Style+&lcTmpLine..cwarecode+lcGlSession)
SCATT MEMVAR MEMO

SELECT (lcCurTmp)
SCAN
  SCATT MEMVAR MEMO
  m.clocation = m.Binloc 
  m.nstkval   = m.nTotstk * m.nCost
  SELECT BININVJL
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

IF USED(lcCurTmp)
  USE IN &lcCurTmp
ENDIF  
ERASE (gcWorkDir+lcCurTmp+'.DBF')
ERASE (gcWorkDir+lcCurTmp+'.CDX')

SELECT(lnAlias)

*:**************************************************************************
*:* Name        : lfDLARBIN
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : PO
*:* Purpose     : Update BinInvJL & WHBINLOC file
*:***************************************************************************
*! C123853,1 MHM 01/15/2005   Custom Screen for DL[Start]
FUNCTION lfDLARBIN
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

*- tmi 05/31/2005 [start]
*- this code was copied here to check if the calling program is ariinv and no data in the field &lcDetFile..Order
IF  lcProgName = 'ARIINV'
  *--mhm2005 handle consolidated invoice case
   
  *IF  EMPTY(&lcDetFile..Order)
  IF  &lcHdrFile..CONSOL = 'Y'
    RETURN
  ENDIF
  
  lcinvline = lcDetFile
  lcInvHdr = lcHdrFile
ENDIF
*- this code was copied here to check if the calling program is ariinv and no data in the field &lcDetFile..Order
*- tmi 05/31/2005 [end]


PRIVATE lnCurAlias,lcTmpIvbin,lcTmpBnInv
lnCurAlias = SELECT()

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF
IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF

IF !USED('IVBINLOC')
  =gfOpenFile(gcDataDir+'IVBINLOC','IVBININV','SH') 
ENDIF

IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF

lcTmpIvbin = gfTempName()
lcTmpBnInv = gfTempName()

*--create tmpfile for invoice bin location
SELECT IVBINLOC
DIMENSION laFileStru[1,4]
= AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpIvbin) FROM ARRAY laFileStru

*--mhm126994
*INDEX ON Style+cWareCode+clocation TAG (lcTmpIvbin)
INDEX ON Style+STR(lineno,6)+cWareCode+clocation TAG (lcTmpIvbin)
*--mhm126994

*--create tmpfile for bin invintory file
SELECT BININVJL
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpBnInv) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+clocation TAG (lcTmpBnInv)

*- tmi 05/31/2005 [start]
*- comment this code and copy it befor creating the temp files to prevent the error too many files open
*IF  lcProgName = 'ARIINV'
*  *--mhm2005 handle consolidated invoice case
*  IF  EMPTY(&lcDetFile..Order)
*    SELECT (lnCurAlias)
*    RETURN
*  ENDIF
*  
*  lcinvline = lcDetFile
*  lcInvHdr = lcHdrFile
*ENDIF
*- comment this code and copy it befor creating the temp files to prevent the error too many files open
*- tmi 05/31/2005 [end]

SELECT (lcInvHdr)
lnCurRec = RECNO()
SELECT IVBINLOC
SEEK(&lcInvHdr..Invoice)
SCAN REST WHILE Invoice+Style+cWareCode+clocation = &lcInvHdr..Invoice
  IF TotQty = 0
    LOOP
  ENDIF
  SCATT MEMVAR MEMO
  INSERT INTO (lcTmpIvbin) FROM MEMVAR
ENDSCAN

 
SELECT (lcInvLine)
lnCurRec = RECNO()
*SCAN
  =SEEK(style+cwarecode+lcGlSession,'STYINVJL')
  SELECT STYINVJL
  SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6)= ;
                  &lcInvLine..style+&lcInvLine..cwarecode+lcGlSession
    IF  (ctrcode <> &lcInvHdr..Invoice) OR (lineNo <> &lcInvLine..LineNo)
      LOOP
    ENDIF
                  
    SCATT MEMVAR MEMO
    
    *--mhm126994
    *=SEEK(Style+cWareCode,lcTmpIvbin)
    =SEEK(Style+STR(lineno,6)+cWareCode,lcTmpIvbin)
    *--mhm126994
    
    SELECT(lcTmpIvbin)
    *--mhm126994
    *SCAN REST WHILE Style+cWareCode+clocation = STYINVJL.Style+STYINVJL.cWareCode
    SCAN REST WHILE Style+STR(lineno,6)+cWareCode+clocation = STYINVJL.Style+STR(STYINVJL.lineno,6)+STYINVJL.cWareCode
    *--mhm126994
      llChkQty = .F.
      FOR lnI  = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        IF (m.nStk&lcI = 0 ) OR (&lcTmpIvbin..Qty&lcI > ABS(m.nStk&lcI))
          llChkQty = .T.
        ELSE
          llChkQty = .F.
          EXIT
        ENDIF
      ENDFOR

      IF llChkQty 
        SELECT (lcTmpBnInv)
        m.clocation = &lcTmpIvbin..clocation
        APPEND BLANK
        GATHER MEMVAR MEMO
        SELECT(lcTmpIvbin)
        FOR lnI = 1 TO 8
          lcI = ALLT(STR(lnI,2))
          REPLACE Qty&lcI WITH Qty&lcI - ABS(m.nStk&lcI)
        ENDFOR  
        REPLACE TotQty WITH TotQty - ABS(m.nTotStk)
        IF TotQty = 0
          DELETE
        ENDIF
        EXIT
      ENDIF
      IF m.nTotStk <>0 AND &lcTmpIvbin..TotQty <> 0
        SELECT (lcTmpBnInv)
        m.clocation = &lcTmpIvbin..clocation
        APPEND BLANK
        GATHER MEMVAR MEMO
        FOR lnI = 1 TO 8
          lcI = ALLT(STR(lnI,2))
          IF ABS(m.nStk&lcI) > &lcTmpIvbin..Qty&lcI
            REPLACE nStk&lcI WITH -1* &lcTmpIvbin..Qty&lcI
            m.nStk&lcI = m.nStk&lcI + &lcTmpIvbin..Qty&lcI
            m.nTotStk  = m.nTotStk  + &lcTmpIvbin..Qty&lcI
            
            REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty - &lcTmpIvbin..Qty&lcI
            REPLACE &lcTmpIvbin..Qty&lcI WITH 0
          ELSE
            REPLACE nStk&lcI WITH m.nStk&lcI
            REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty + m.nStk&lcI
            REPLACE &lcTmpIvbin..Qty&lcI WITH  &lcTmpIvbin..Qty&lcI + m.nStk&lcI
            m.nTotStk  = m.nTotStk  - m.nStk&lcI
            m.nStk&lcI = 0
          ENDIF 
        ENDFOR  
        REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        REPLACE nstkval WITH nTotStk * nCost
        IF nTotStk = 0
          DELETE
        ENDIF
        
      ENDIF
    ENDSCAN
  ENDSCAN                
*ENDSCAN

SELECT (lcInvLine)
IF BETWEEN(lnCurRec ,1,RECCOUNT())
  GOTO lnCurRec 
  m.LineNo = &lcInvLine..LineNo
ENDIF

SELECT (lcTmpBnInv)
SCAN
  IF nTotStk = 0
    LOOP
  ENDIF
  SCATTER MEMVAR MEMO
  INSERT INTO BININVJL FROM MEMVAR
ENDSCAN

IF USED(lcTmpBnInv)
  USE IN (lcTmpBnInv)
ENDIF
ERASE (gcWorkDir +lcTmpBnInv+".DBF")
ERASE (gcWorkDir +lcTmpBnInv+".CDX")
ERASE (gcWorkDir +lcTmpBnInv+".FPT")

IF USED(lcTmpIvbin)
  USE IN (lcTmpIvbin)
ENDIF
ERASE (gcWorkDir +lcTmpIvbin+".DBF")
ERASE (gcWorkDir +lcTmpIvbin+".CDX")
ERASE (gcWorkDir +lcTmpIvbin+".FPT")


SELECT (lnCurAlias)
*:**************************************************************************
*:* Name        : lfDLARBIN
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : PO
*:* Purpose     : Validate Bin qty to issue from
*:***************************************************************************
*! C123853,1 MHM 01/15/2005  Custom Screen for DL[Start]
FUNCTION lfDVLDPOBN
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin

  llChkQty = .F.
  FOR lnI  = 1 TO 8
    lcI = ALLT(STR(lnI,2))  
    IF (ABS(laIssQty[lnI]) <> 0 ) AND (ABS(laIssQty[lnI]) > m.Bin_Qty&lcI)
      llChkQty = .T.
      EXIT
    ELSE
      llChkQty = .F.
    ENDIF
  ENDFOR
  IF llIssue AND llChkQty 
        lcStyle = m.Item
        *--The receiving quantity are not covered the issued quantity
        *--for Style XXXX , This transaction line will be ignored.
        *=gfModalGen('TRM42115B42000','DIALOG',lcStyle)
        lcMsg2 = 'There is not enough stock of component '+lcStyle+ ' on hand at this location - please adjust the Issue Quantity'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  ELSE
    =lfIssRetSty(m.Typ,m.cCatgTyp,m.Item,lcIssWare,m.Dyelot,@laIssQty,lnIssCost,;
               m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
               IIF(llIssue,SPACE(6),lcSession),IIF(llIssue,lcSession,SPACE(6)))
  ENDIF             
ELSE
  =lfIssRetSty(m.Typ,m.cCatgTyp,m.Item,lcIssWare,m.Dyelot,@laIssQty,lnIssCost,;
               m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
               IIF(llIssue,SPACE(6),lcSession),IIF(llIssue,lcSession,SPACE(6)))
ENDIF

*:**************************************************************************
*:* Name        : lfDLARBIN
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : PO
*:* Purpose     : Validate Bin qty to issue from
*:***************************************************************************
*! C123853,1 MHM 01/15/2005 Custom Screen for DL[Start]
FUNCTION lfDVLDPOAU
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin

  llChkQty = .F.
  DIMENSION laOldQty[9]
  STORE 0 TO laOldQty
  IF !USED('WHBINLOC')
    =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
  ENDIF
  lcStyle = &lcIssLtFile..Item
  SELECT WHBINLOC
  lcOrdOrdr = ORDER()
  SET ORDER TO Whbinlst
  IF SEEK(lcStyle + laData[32])
    FOR lnCntr = 1 TO 9
      laOldQty[lnCntr] = laIssQty[lnCntr] 
    ENDFOR
  
    SCAN REST WHILE style+cwarecode+clocation = lcStyle + laData[32]
      FOR lnI  = 1 TO 8
        lcI = ALLT(STR(lnI,2))  
        IF (ABS(laOldQty[lnI]) <> 0 ) AND (ABS(laOldQty[lnI]) > (WHBINLOC.Qty&lcI-WHBINLOC.Alo&lcI))
           laOldQty[lnI]    = laOldQty[lnI] + (WHBINLOC.Qty&lcI-WHBINLOC.Alo&lcI)
           laOldQty[9]      = laOldQty[9] + (WHBINLOC.Qty&lcI-WHBINLOC.Alo&lcI)
        ELSE
           laOldQty[9]      = laOldQty[9] - laOldQty[lnI]
           laOldQty[lnI]    = 0
        ENDIF
      ENDFOR
    ENDSCAN
  ENDIF
  IF  laOldQty[9] = laIssQty[9] 
        lcStyle = &lcIssLtFile..Item
        *--The receiving quantity are not covered the issued quantity
        *--for Style XXXX , This transaction line will be ignored.
        *=gfModalGen('TRM42115B42000','DIALOG',lcStyle)
        lcMsg2 = 'There is not enough stock of component '+lcStyle+ ' on hand at this location - please adjust the Issue Quantity'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  ELSE
    SELECT (lcIssLtFile)
    =lfIssRetSty(Typ,cCatgTyp,lcStyle,cWareCode,Dyelot,;
                 @laIssQty,lnIssCost,lcOprCode,lcLotNo,ldIssDate,SPACE(6),lcSession)
  ENDIF             
ELSE
  =lfIssRetSty(Typ,cCatgTyp,Item,cWareCode,Dyelot,;
               @laIssQty,lnIssCost,lcOprCode,lcLotNo,ldIssDate,SPACE(6),lcSession)
ENDIF


*!**************************************************************************
*! Name      : lfDLMDYSTY
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Validate in case of modify
*!**************************************************************************
*! Example   : = lfDLMDYSTY()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLMDYSTY

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN .F.
ENDIF

PRIVATE lcFileNama ,lnCurAlias
lcFileNama = lcTmpAdj+"A"
lnCurAlias = SELECT()

IF SEEK(lcFromWare+&lcFileNamA..LocFrom+m.Style,'WHBINLOC')
  m.stk1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
  m.stk2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
  m.stk3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
  m.stk4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
  m.stk5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
  m.stk6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
  m.stk7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
  m.stk8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
  m.Totstk =WHBINLOC.TotQty - WHBINLOC.TotAlo
  IF lcType = 'P'
    m.Adj1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
    m.Adj2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
    m.Adj3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
    m.Adj4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
    m.Adj5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
    m.Adj6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
    m.Adj7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
    m.Adj8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
    m.TotAdj =WHBINLOC.TotQty - WHBINLOC.TotAlo
  ENDIF  

  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laTStk[9] = m.Totstk
  
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laOTStk[9] = m.Totstk

ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    m.Stk&lcCount = 0
  ENDFOR
  m.tOTstk  =0
  *! C123853,1 MHM 01/15/2005  consider physical case [Start]
  IF lcType = 'P'
    FOR lnCount = 1 TO 8 
      lcCount = ALLTRIM(STR(lnCount))
      m.Adj&lcCount = 0
    ENDFOR
    m.TotAdj = 0
  ENDIF
  *! C123853,1 MHM 01/15/2005  [End]
  
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = 0
  ENDFOR
  laTStk[9] = 0

  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = 0
  ENDFOR
  laOTStk[9] = 0
ENDIF

SELECT(lnCurAlias)

*!**************************************************************************
*! Name      : lfTRMDSTY
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Validate Style in case of IC Transfeer (modify ).
*!**************************************************************************
*! Example   : = lfTRMDSTY()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfTRMDSTY
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN .F.
ENDIF

PRIVATE lcFileNama,lnCurAlias
lcFileNama = lcTmpAdj+"A"
lnCurAlias = SELECT()

IF SEEK(lcFromWare+&lcFileNamA..LocFrom+M.Style,'WHBINLOC')
  m.stk1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
  m.stk2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
  m.stk3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
  m.stk4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
  m.stk5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
  m.stk6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
  m.stk7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
  m.stk8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
  m.tOTstk =WHBINLOC.TotQty - WHBINLOC.TotAlo
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laTStk[9] = m.Totstk
ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    m.Stk&lcCount = 0
  ENDFOR
  m.tOTstk  =0
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = 0
  ENDFOR
  laTStk[9] = 0
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = 0
  ENDFOR
  laOTStk[9] = 0
ENDIF

IF SEEK(lcToWare+&lcFileNamA..LocTo+M.Style,'WHBINLOC')
  laTStk[1]  =WHBINLOC.Qty1 - WHBINLOC.Alo1 +m.adj1
  laTStk[2]  =WHBINLOC.Qty2 - WHBINLOC.Alo2 +m.adj2
  laTStk[3]  =WHBINLOC.Qty3 - WHBINLOC.Alo3 +m.adj3
  laTStk[4]  =WHBINLOC.Qty4 - WHBINLOC.Alo4 +m.adj4
  laTStk[5]  =WHBINLOC.Qty5 - WHBINLOC.Alo5 +m.adj5
  laTStk[6]  =WHBINLOC.Qty6 - WHBINLOC.Alo6 +m.adj6
  laTStk[7]  =WHBINLOC.Qty7 - WHBINLOC.Alo7 +m.adj7
  laTStk[8]  =WHBINLOC.Qty8 - WHBINLOC.Alo8 +m.adj8
  laTStk[9]  =WHBINLOC.TotQty - WHBINLOC.TotAlo +m.Totadj
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = laTStk[lnCount]
  ENDFOR
  laOTStk[9] =laTStk[9] 
ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = 0
    laOTStk[lnCount] = 0
  ENDFOR
  laOTStk[9] = 0
  laTStk[9] = 0
ENDIF
SELECT (lnCurAlias)

*:**************************************************************************
*:* Name        : lfDLARVOID
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : Ar
*:* Purpose     : Update BinInvJL & WHBINLOC file in case of void Invoice
*:***************************************************************************
*! C123853,1 MHM 01/15/2005 Custom Screen for DL[Start]
FUNCTION lfDLARVOID

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF
PRIVATE lnCurAlias,lcTmpIvbin,lcTmpBnInv
lnCurAlias = SELECT()

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF
IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF

IF !USED('IVBINLOC')
  =gfOpenFile(gcDataDir+'IVBINLOC','IVBININV','SH') 
ENDIF

IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF

lcTmpIvbin = gfTempName()
lcTmpBnInv = gfTempName()

*--create tmpfile for invoice bin location
SELECT IVBINLOC
DIMENSION laFileStru[1,4]
= AFIELDS(laFileStru)
CREATE TABLE (lcTmpIvbin) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+clocation TAG (lcTmpIvbin)

*--create tmpfile for bin invintory file
SELECT BININVJL
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
CREATE TABLE (lcTmpBnInv) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+clocation TAG (lcTmpBnInv)
IF  lcProgName = 'ARIINV'
  *lcinvline = lcDetFile
  *lcInvHdr = lcHdrFile
ENDIF


SELECT IVBINLOC
SEEK(InvHdr.Invoice)
SCAN REST WHILE Invoice+Style+cWareCode+clocation = InvHdr.Invoice
  SCATT MEMVAR MEMO
  INSERT INTO (lcTmpIvbin) FROM MEMVAR
ENDSCAN


SELECT InvLine
lnCurRec = RECNO()
=SEEK(INVHDR.INVOICE)
SCAN REST WHILE invoice+STR(lineno,6) = INVHDR.INVOICE
  *--mhm2005
  *=SEEK(style+cwarecode+lcGlSession,'STYINVJL')
  =SEEK(style+cwarecode,'STYINVJL')
  *--mhm2005
  SELECT STYINVJL
  SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6)= ;
                  InvLine.style+InvLine.cwarecode
    IF !(ctrtype = '4' And ctrcode = INVHDR.INVOICE)
      LOOP
    ENDIF
    SCATT MEMVAR MEMO
    =SEEK(Style+cWareCode,lcTmpIvbin)
    SELECT(lcTmpIvbin)
    SCAN REST WHILE Style+cWareCode+clocation = STYINVJL.Style+STYINVJL.cWareCode
      llChkQty = .F.
      FOR lnI  = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        IF (m.nStk&lcI = 0 ) OR (&lcTmpIvbin..Qty&lcI > ABS(m.nStk&lcI))
          llChkQty = .T.
        ELSE
          llChkQty = .F.
          EXIT
        ENDIF
      ENDFOR

      IF llChkQty 
        SELECT (lcTmpBnInv)
        m.clocation = &lcTmpIvbin..clocation
        APPEND BLANK
        GATHER MEMVAR MEMO
        SELECT(lcTmpIvbin)
        FOR lnI = 1 TO 8
          lcI = ALLT(STR(lnI,2))
          REPLACE Qty&lcI WITH Qty&lcI + ABS(m.nStk&lcI)
        ENDFOR  
        REPLACE TotQty WITH TotQty + ABS(m.nTotStk)
        EXIT
      ENDIF
      IF m.nTotStk <>0 AND &lcTmpIvbin..TotQty <> 0
        SELECT (lcTmpBnInv)
        m.clocation = &lcTmpIvbin..clocation
        APPEND BLANK
        GATHER MEMVAR MEMO
        FOR lnI = 1 TO 8
          lcI = ALLT(STR(lnI,2))
          IF ABS(m.nStk&lcI) > &lcTmpIvbin..Qty&lcI
            REPLACE nStk&lcI WITH  &lcTmpIvbin..Qty&lcI
            m.nStk&lcI = m.nStk&lcI - &lcTmpIvbin..Qty&lcI
            m.nTotStk  = m.nTotStk  - &lcTmpIvbin..Qty&lcI
            
            REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty + &lcTmpIvbin..Qty&lcI
            REPLACE &lcTmpIvbin..Qty&lcI WITH 0
          ELSE
            REPLACE nStk&lcI WITH m.nStk&lcI
            REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty - m.nStk&lcI
            REPLACE &lcTmpIvbin..Qty&lcI WITH  &lcTmpIvbin..Qty&lcI - m.nStk&lcI
            m.nTotStk  = m.nTotStk  + m.nStk&lcI
            m.nStk&lcI = 0
          ENDIF 
        ENDFOR  
        REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        REPLACE nstkval WITH nTotStk * nCost
      ENDIF
    ENDSCAN
  ENDSCAN                
ENDSCAN

SELECT InvLine
IF BETWEEN(lnCurRec ,1,RECCOUNT())
  GOTO lnCurRec 
ENDIF

SELECT (lcTmpBnInv)
SCAN
 SCATTER MEMVAR MEMO
  INSERT INTO BININVJL FROM MEMVAR
ENDSCAN

IF USED(lcTmpBnInv)
  USE IN (lcTmpBnInv)
ENDIF
ERASE (gcWorkDir +lcTmpBnInv+".DBF")
ERASE (gcWorkDir +lcTmpBnInv+".CDX")
ERASE (gcWorkDir +lcTmpBnInv+".FPT")

IF USED(lcTmpIvbin)
  USE IN (lcTmpIvbin)
ENDIF
ERASE (gcWorkDir +lcTmpIvbin+".DBF")
ERASE (gcWorkDir +lcTmpIvbin+".CDX")
ERASE (gcWorkDir +lcTmpIvbin+".FPT")

SELECT (lnCurAlias)


*!**************************************************************************
*! Name      : lfDLVODINV
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Save Data to Void Credit memo screen
*!**************************************************************************
*! Example   : = lfDLVODINV()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*! C123853,1 MHM 01/15/2005
FUNCTION lfDLVODRM

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
IF !USED('RMBINLOC')
  =gfOpenFile(gcDataDir+'RMBINLOC','RMBINCRM','SH') 
ENDIF

*B131267,1 NNA 06/22/2006 (Begin) asking for Return style if a style returned to second or damage
STORE "" TO lcRetStyle
lcRetStyle = IIF(!EMPTY(RetLine.cRetSty),RetLine.cRetSty,RetLine.Style)
*B131267,1 NNA (End)

*! C123853,1 MHM 01/15/2005 function to handle bininvjl in case of void RM[Start]
=lfDLRMVOID()
*! C123853,1 MHM 01/15/2005[End]

*B131267,1 NNA 06/22/2006 (Begin) asking for Return style if a style returned to second or damage
*IF SEEK(RETHDR.CRMEMO+RETLINE.Style+RETHDR.cWareCode ,'RMBINLOC')
IF SEEK(RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode ,'RMBINLOC')
*B131267,1 NNA (End)

  SELECT RMBINLOC
  
  *B131267,1 NNA 06/22/2006 (Begin) asking for Return style if a style returned to second or damage
  *SCAN REST WHILE crmemo+style+cwarecode+clocation = RETHDR.CRMEMO+RETLINE.Style+RETHDR.cWareCode
  SCAN REST WHILE crmemo+style+cwarecode+clocation = RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode
  *B131267,1 NNA (End)
  
    IF SEEK(RMBINLOC.Style+ RMBINLOC.cWareCode + RMBINLOC.cLocation ,'WHBINLOC') 
      SELECT WHBINLOC
      REPLACE Qty1       WITH Qty1- RmBinLoc.Qty1,;
              Qty2       WITH Qty2- RmBinLoc.Qty2,;
              Qty3       WITH Qty3- RmBinLoc.Qty3,;
              Qty4       WITH Qty4- RmBinLoc.Qty4,;
              Qty5       WITH Qty5- RmBinLoc.Qty5,;
              Qty6       WITH Qty6- RmBinLoc.Qty6,;
              Qty7       WITH Qty7- RmBinLoc.Qty7,;
              Qty8       WITH Qty8- RmBinLoc.Qty8,;
              TotQty     WITH TotQty- RmBinLoc.TotQty
      IF TotQty =0
        DELETE 
      ENDIF        
    ENDIF
    SELECT RMBINLOC
    DELETE
  ENDSCAN
ENDIF
*:**************************************************************************
*:* Name        : lfDLRMVOID
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 12/08/2004
*:* Module      : Ar
*:* Purpose     : Update BinInvJL & WHBINLOC file in case of void Credit memo
*:***************************************************************************
*! C123853,1 MHM 01/15/2005 Custom Screen for DL[Start]
FUNCTION lfDLRMVOID

PRIVATE lnCurAlias,lcTmpIvbin,lcTmpBnInv
lnCurAlias = SELECT()

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF
IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF

IF !USED('RMBINLOC')
  =gfOpenFile(gcDataDir+'RMBINLOC','RMBINCRM','SH') 
ENDIF

IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF

lcTmpIvbin = gfTempName()
lcTmpBnInv = gfTempName()

*--create tmpfile for invoice bin location
SELECT RMBINLOC
DIMENSION laFileStru[1,4]
= AFIELDS(laFileStru)
CREATE TABLE (lcTmpIvbin) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+clocation TAG (lcTmpIvbin)

*--create tmpfile for bin invintory file
SELECT BININVJL
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
CREATE TABLE (lcTmpBnInv) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+clocation TAG (lcTmpBnInv)

SELECT RMBINLOC
SEEK(RETHDR.CRMEMO)
SCAN REST WHILE crmemo+style+cwarecode+clocation = RETHDR.CRMEMO
  SCATT MEMVAR MEMO
  INSERT INTO (lcTmpIvbin) FROM MEMVAR
ENDSCAN
SELECT RETLINE
lnCurRec = RECNO()
=SEEK(RETHDR.CRMEMO)
SCAN REST WHILE crmemo+style+cret_linno+cret_trncd = RETHDR.CRMEMO
  
  *--NNA B131267 seek for the return style if returned to second or damage else the credit memo style , also seek for the receiving session
  *=SEEK(style+RETHDR.cwarecode+lcRecSess,'STYINVJL')
  *SELECT STYINVJL
  *SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6)= ;
                  RETLINE.style+RETHDR.cwarecode
    *IF !(ctrtype = '8' And ctrcode = RETHDR.CRMEMO)
    *  LOOP
    *ENDIF
  =SEEK(lcRetStyle+RETHDR.cwarecode+lcVRGLSess,'STYINVJL')
  SELECT STYINVJL
  SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6)= lcRetStyle+RETHDR.cwarecode + lcVRGLSess ;
    FOR ctrtype = '8' And ctrcode = RETHDR.CRMEMO AND STR(LineNo,6) = PADL(ALLTRIM(RETLINE.CRET_LINNO),6,' ')
  *B131267,1 NNA (End)

    SCATT MEMVAR MEMO
    =SEEK(Style+cWareCode,lcTmpIvbin)
    SELECT(lcTmpIvbin)
    SCAN REST WHILE Style+cWareCode+clocation = STYINVJL.Style+STYINVJL.cWareCode
      llChkQty = .F.
      FOR lnI  = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        IF (m.nStk&lcI = 0 ) OR (&lcTmpIvbin..Qty&lcI > ABS(m.nStk&lcI))
          llChkQty = .T.
        ELSE
          llChkQty = .F.
          EXIT
        ENDIF
      ENDFOR
      IF llChkQty 
        SELECT (lcTmpBnInv)
        m.clocation = &lcTmpIvbin..clocation
        APPEND BLANK
        GATHER MEMVAR MEMO
        SELECT(lcTmpIvbin)
        FOR lnI = 1 TO 8
          lcI = ALLT(STR(lnI,2))
          REPLACE Qty&lcI WITH Qty&lcI + ABS(m.nStk&lcI)
        ENDFOR  
        REPLACE TotQty WITH TotQty + ABS(m.nTotStk)
        EXIT
      ENDIF
      IF m.nTotStk <>0 AND &lcTmpIvbin..TotQty <> 0
        SELECT (lcTmpBnInv)
        m.clocation = &lcTmpIvbin..clocation
        APPEND BLANK
        GATHER MEMVAR MEMO
        FOR lnI = 1 TO 8
          lcI = ALLT(STR(lnI,2))
          IF ABS(m.nStk&lcI) > &lcTmpIvbin..Qty&lcI
            REPLACE nStk&lcI WITH  &lcTmpIvbin..Qty&lcI
            m.nStk&lcI = m.nStk&lcI - &lcTmpIvbin..Qty&lcI
            m.nTotStk  = m.nTotStk  - &lcTmpIvbin..Qty&lcI
            
            REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty + &lcTmpIvbin..Qty&lcI
            REPLACE &lcTmpIvbin..Qty&lcI WITH 0
          ELSE
            REPLACE nStk&lcI WITH m.nStk&lcI
            REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty - m.nStk&lcI
            REPLACE &lcTmpIvbin..Qty&lcI WITH  &lcTmpIvbin..Qty&lcI - m.nStk&lcI
            m.nTotStk  = m.nTotStk  + m.nStk&lcI
            m.nStk&lcI = 0
          ENDIF 
        ENDFOR  
        REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        REPLACE nstkval WITH nTotStk * nCost
      ENDIF
    ENDSCAN
  ENDSCAN                
ENDSCAN

SELECT RETLINE
IF BETWEEN(lnCurRec ,1,RECCOUNT())
  GOTO lnCurRec 
ENDIF

SELECT (lcTmpBnInv)
SCAN
 SCATTER MEMVAR MEMO
  INSERT INTO BININVJL FROM MEMVAR
ENDSCAN

IF USED(lcTmpBnInv)
  USE IN (lcTmpBnInv)
ENDIF
ERASE (gcWorkDir +lcTmpBnInv+".DBF")
ERASE (gcWorkDir +lcTmpBnInv+".CDX")
ERASE (gcWorkDir +lcTmpBnInv+".FPT")

IF USED(lcTmpIvbin)
  USE IN (lcTmpIvbin)
ENDIF
ERASE (gcWorkDir +lcTmpIvbin+".DBF")
ERASE (gcWorkDir +lcTmpIvbin+".CDX")
ERASE (gcWorkDir +lcTmpIvbin+".FPT")

SELECT (lnCurAlias)

*:**************************************************************************
*:* Name        : lfDLCHKBIN
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 01/03/2005
*:* Module      : PO
*:* Purpose     : validate bin location 
*:***************************************************************************
*! C123853,1 MHM 01/15/2005 Custom Screen for DL[Start]
FUNCTION lfDLCHKBIN
PRIVATE llmessag

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF

llmessag = .F.

IF EMPTY(lcBinLoc)
  lcMsg2 = 'You have to select a bin location'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llmessag = .T.
ENDIF
RETURN llmessag

*:**************************************************************************
*:* Name        : lfvPOScbn
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 01/03/2005
*:* Module      : PO
*:* Purpose     : validate bin location in Po receiving screen
*:***************************************************************************
*! C123853,1 MHM 01/15/2005 Custom Screen for DL[Start]
FUNCTION lfvPOScbn

PRIVATE llFromSeek
STORE .F.TO llFromSeek

SELECT WhsLoc
IF EMPTY(lcBinLoc) AND !llbrowse
  RETURN
ENDIF
SET ORDER TO TAG WhsBin IN  WHSLOC

IF llbrowse OR (!EMPTY(lcBinLoc) AND !SEEK(lcBinLoc+lcWareCode))

  llbrowse =.F. 
  *T20061031.0001 TMI [Start] 
  PRIVATE lcBrFields 
  *T20061031.0001 TMI [End  ] 
  lcBrFields = "clocation  :H='Bin',"+;
                   "cBinClass  :H='Bin Class',"+;
                   "cFlatHang  :H='Flat or Hang'"
                 
  lcForExpr = " cWareCode = lcWareCode  AND EMPTY(Style)"
  STORE '' TO laTempData
  =SEEK(ALLTRIM(lcBinLoc))
  =gfBrows([FOR &lcForExpr],'cLocation,cBinClass,cFlatHang','laTempData','WhsLoc')
  lcBinLoc=laTempData[1]
ELSE
  laTempData[1] = lcBinLoc
  laTempData[2] = WhsLoc.cbinclass
  laTempData[3] = WhsLoc.cflathang
  STORE .T.TO llFromSeek
ENDIF

SET ORDER TO TAG WhsLocSt IN  WHSLOC

IF llFromSeek
 =lfvPOScOk()
ENDIF
*:**************************************************************************
*:* Name        : lfvPOScbn
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 01/03/2005
*:* Module      : PO
*:* Purpose     : validate bin location in Po receiving screen
*:***************************************************************************
*! C123853,1 MHM 01/15/2005 Custom Screen for DL[Start]
FUNCTION lfvPOScOk
IF EMPTY(lcBinLoc)
  lcMsg2 = 'You have to assign Bin location'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  RETURN
ENDIF
CLEAR READ

*:**************************************************************************
*:* Name        : lfchkCLas
*:* Developer   : Mohamed Shokry (MHM)
*:* Date        : 01/03/2005
*:* Module      : PO
*:* Purpose     : validate bin location in Po receiving screen
*:***************************************************************************
*! C123853,1 MHM 01/15/2005 check for the same class[Start]
FUNCTION lfchkCLas
PARAMETER lcStyle,lcWareCode,lcSize

PRIVATE llCheckHF,llChckPrm ,llChckScn,llChckRem,lnCurrAlis,lnCrreC
STORE .F. TO llCheckHF,llChckPrm ,llChckScn,llChckRem
lnCurrAlis = SELECT(0)

lnCrreC = RECNO('WHBINLOC')
IF !USED('WHSLOC') 
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
ENDIF

=SEEK(lcStyle,'STYLE')


*--check for primary class 
=SEEK(lcStyle+ lcWareCode ,'WHBINLOC') 
SELECT WHBINLOC
SCAN REST WHILE style+cwarecode+clocation =lcStyle+ lcWareCode
  =SEEK(lcWareCode+WHBINLOC.clocation,'WHSLOC')
  
  IF  (WhsLoc.cBinClass = STYLE.cPrimClss&lcSize) AND (WhsLoc.cFlatHang = style.cflathang) AND (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt) >0
    llChckPrm = .T.
    EXIT
  ENDIF
ENDSCAN

IF llChckPrm 
  SELECT (lnCurrAlis)
  RETURN llChckPrm 
ENDIF


*--check for Secondry class 
=SEEK(lcStyle+ lcWareCode ,'WHBINLOC') 
SELECT WHBINLOC
SCAN REST WHILE style+cwarecode+clocation =lcStyle+ lcWareCode
  =SEEK(lcWareCode+WHBINLOC.clocation,'WHSLOC')
  
  IF  (WhsLoc.cBinClass = STYLE.cSecClss&lcSize) AND (WhsLoc.cFlatHang = style.cflathang) AND (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt) > 0
    llChckScn= .T.
    EXIT
  ENDIF
ENDSCAN

IF llChckScn
  SELECT (lnCurrAlis)
  RETURN llChckScn
ENDIF

*--check for Secondry class 
=SEEK(lcStyle+ lcWareCode ,'WHBINLOC') 
SELECT WHBINLOC
SCAN REST WHILE style+cwarecode+clocation =lcStyle+ lcWareCode
  =SEEK(lcWareCode+WHBINLOC.clocation,'WHSLOC')
  
  IF  (WhsLoc.cBinClass = STYLE.cRemClss&lcSize) AND (WhsLoc.cFlatHang = style.cflathang) AND (WHBINLOC.QTY&lcCnt-WHBINLOC.Alo&lcCnt) > 0
    llChckRem= .T.
    EXIT
  ENDIF
ENDSCAN

IF llChckRem
  SELECT (lnCurrAlis)
  RETURN llChckRem
ENDIF

SELECT (lnCurrAlis)
  IF BETWEEN(lnCrreC ,1,RECCOUNT('WHBINLOC'))
    GOTO lnCrreC IN WHBINLOC
  ENDIF

RETURN .F.

*:**************************************************************************
*:* Name        : lfCOLLFIT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 30/12/2004
*:* Purpose     :  Add a trigger to collect sizes for DIR03 taking in considration the field SCALE.CDIM1 
*:***************************************************************************
*:* Called from : lfvQStyle IN DAVMAIN.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfCOLLFIT
PRIVATE lnPrvAlias, lcSafe , lnStyComm , lnGrssPric , lnMax ,;
        lnI, lcI, lnSzCnt, lcSzCnt , lnClrs , laClr , lnGp ,;
        lcSeekSty , lnStyComm , lnGrssPric,lcPRICCODE, lcStyScl


IF !USED('BOM')
  =gfOpenFile(gcDataDir+'BOM','BOM','SH')
ENDIF

lcSafe = SET('SAFE')
SET SAFETY OFF
lnPrvAlias = SELECT (0)
SELECT (lcTempCur)
SET FILTER TO
*INDEX ON LASTLINE+STYMAJOR+COLOR+FIT+CSEQUNC TAG (lcTempCur) ADDITIVE 
INDEX ON LASTLINE+STYMAJOR+COLOR+STR(FITNO,1)+CSEQUNC TAG (lcTempCur) ADDITIVE
SET SAFETY &lcSafe

SELECT DISTINCT SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) FROM STYLE ;
        WHERE STYLE = PADR(m.Style,lnMajorLen) ;
        INTO ARRAY laClr  

=SEEK(M.Style,'STYLE')
lcCSTYNOT = STYLE.CSTYNOT
SHOW GET lcCSTYNOT
=lfRefresh(lcQkWin3)

lcStyScl = SUBSTR(STYLE.SCALE,1,lnScaleLen)

*B132317,1  TMI [Start] use price level based on customer price level setup in customr screen , if at qty level use priceA
lcPrLvl = IIF(CUSTOMER.PRICELVL='Q','A',CUSTOMER.PRICELVL)
lcPrLvl = IIF( !(lcPrLvl $ "ABCQ") , "A" , lcPrLvl )  && TMI 08/08/2006
*B132317,1  TMI [End  ] 

*-- Get all color for the current style.
FOR lnClrs = 1 TO ALEN(laClr)
  *-- Add field to hold the total.

  SELECT (lcTempCur)
  SCATTER MEMVAR BLANK
  M.STYMAJOR = PADR(m.Style,lnMajorLen)
  M.COLOR = laClr[lnClrs]
  IF SEEK(M.STYMAJOR+lcSepart+PADR(M.COLOR,lnClrLen),'STYLE') .AND. STYLE.STATUS <> 'A'
    LOOP
  ENDIF
  M.COLORDSC = PADR(gfCodDes(PADR(laClr[lnClrs],lnClrLen) , 'COLOR'),20)

  WAIT WINDOW 'Please Wait ... Collecting data for Color# : ' + M.COLORDSC NOWAIT

  *-- Get all sizes for current style.
  SELECT SCALE
  =SEEK('S'+lcStyScl)
  lnFitNo = 0
  DO WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl .AND. !EOF('SCALE')
  
    lcFit = SCALE.CDIM1
    lnFitNo = lnFitNo + 1
    lnSzCnt = 0
    lnGp = 1    
    SCAN REST WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl .AND.;
                    CDIM1 = lcFit

      FOR lnI = 1 TO SCALE.CNT
        
        lnSzCnt = lnSzCnt + 1
        IF lnSzCnt > 10
          lnGp = lnGp + 1
          lnSzCnt = 1
        ENDIF
 
        SELECT &lcTempCur
*        IF !SEEK(' '+PADR(m.Style,lnMajorLen)+laClr[lnClrs]+lcFit+STR(lnGp,6),lcTempCur) 
        IF !SEEK(' '+PADR(m.Style,lnMajorLen)+laClr[lnClrs]+STR(lnFitNo,1)+STR(lnGp,6),lcTempCur) 
          APPEND BLANK
          REPLACE STYMAJOR WITH m.Style        ;
                  COLOR    WITH laClr[lnClrs]  ;
                  COLORDSC WITH M.COLORDSC     ;
                  FIT      WITH lcFit          ;
                  FITNO    WITH lnFitNo        ;
                  cSequnc  WITH STR(lnGp,6)
        ENDIF

        *-- get the sales order once for the current scale.
        lcSeekSty = PADR(PADR(m.Style,lnMajorLen)+lcSepart+laClr[lnClrs]+lcClrSpr+Scale.Scale,19)
    
        *-If style is not assigned to the selected WH ...
        IF laSetups[5,2]='Y' .AND. !SEEK(lcSeekSty+laData[31]+SPACE(10),'StyDye')
          *E300408,1 Button : 40002  :: \<Add;\?\<Reenter
          lcMsg = 'Style ' + lcSeekSty + ' is not assigned to location ' + laWareHouses[lnWareHouse]
          lnResp = gfModalGen('QRM00000B40002',.F.,.F.,.F.,lcMsg)
          IF lnResp = 1
            DO gpAdStyWar WITH lcSeekSty,SPACE(10),laData[31]
          ELSE            
            RETURN .F. 
          ENDIF
        ENDIF

        *-- Get the price from CSTPRICE.dbf or from style.dbf file
        STORE 0 To lnStyComm , lnGrssPric
        =SEEK(lcSeekSty,'STYLE')
    
        lcPRICCODE = ''
        lcI = STR(lnI,1)    
        IF SEEK(lfPRICCODE()+laData[33]+lcSeekSty,'CSTPRICE')
          lnStyComm  = IIF(CSTPRICE.COMMDV=0,IIF(Style.Commission,laData[28],0),CSTPRICE.COMMDV)
          *- use the special size price , if it is 0 use the special scale price ( FOR DIR03 ) 
          lnGrssPric = IIF(CSTPRICE.PRICE&lcI<>0,CSTPRICE.PRICE&lcI, CSTPRICE.PRICEDV)
        ELSE      
          lcPri = STR(0,12,2) + STR(IIF(Style.Commission,laData[28],0),5,2) + STR(0,6,2)
          IF llMulCurr .AND. laData[33]<>gcBaseCurr 
            IF SEEK(lcSeekSty+laData[33],'STYPRICE')
              lnStyComm = IIF(Style.Commission,laData[28],0)
              *B132317,1  TMI [Start] use price level based on customer price level setup in customr screen , if at qty level use priceA
              *lnGrssPric= STYPRICE.PRICEA
              lnGrssPric= STYPRICE.PRICE&lcPrLvl
              *B132317,1  TMI [End  ]
            ENDIF
          ELSE
            IF SEEK(lcSeekSty,'STYLE')
              lnStyComm  = IIF(Style.Commission,laData[28],0)
              *B132317,1  TMI [Start] use price level based on customer price level setup in customr screen , if at qty level use priceA
              *lnGrssPric = STYLE.PRICEA
              lnGrssPric = STYLE.PRICE&lcPrLvl
              *B132317,1  TMI [End  ] 
            ENDIF
           ENDIF
        ENDIF
        SELECT SCALE
    
        *-- Update Gross price & commesion 
        *-         the Physical & Free stock.

        *lcI = STR(lnI,1)
        lcSzCnt = ALLTRIM(STR(lnSzCnt))
        SELECT (lcTempCur)        
        REPLACE SCALE&lcSzCnt  WITH SCALE.SCALE + lcI ;
                Size&lcSzCnt   WITH Scale.SZ&lcI      ;
                GPric&lcSzCnt  WITH lnGrssPric        ;
                Comm&lcSzCnt   WITH lnStyComm         ;
                FreStk&lcSzCnt WITH STYDYE.Stk&lcI - STYDYE.ALO&lcI ;
                PhyStk&lcSzCnt WITH STYDYE.Stk&lcI                  ;
                TotFreStk      WITH TotFreStk + FreStk&lcSzCnt      ;
                TotPhyStk      WITH TotPhyStk + PhyStk&lcSzCnt 
                
        IF SEEK(PADR(SUBSTR(lcSeekSty,1,lnMajorLen),19)+'2','BOM')
          lcCompSty = STUFF(lcSeekSty,1,lnMajorLen,SUBSTR(BOM.ITEM,1,lnMajorLen))
          =SEEK(lcCompSty+laWareHouse[lnWareHouse,2]+SPACE(10),'STYDYE')
          REPLACE PlnStk&lcSzCnt WITH STYDYE.Stk&lcI - STYDYE.ALO&lcI,;
                  TotPlnStk      WITH TotPlnStk + PlnStk&lcSzCnt
        ENDIF
      ENDFOR
    ENDSCAN
  ENDDO
ENDFOR
    
SELECT (lcTempCur)
CALC MAX(VAL(cSequnc)) TO lnMax
FOR lnI = 1 TO lnMax
  INSERT INTO (lcTempCur) (LASTLINE,COLORDSC,CSEQUNC) VALUES (CHR(255),'Totals',STR(lnI,6) )
ENDFOR

*- Resize the headers array
DIMENSION laSize[lnMax*lnBrFlCn]

SET FILTER TO (Val(cSequnc) = lnfldgrp)
WAIT CLEAR
SELECT (lnPrvAlias)

*-- end of lfCOLLFIT.


*:**************************************************************************
*:* Name        : lfSavFit
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Save function for Quick order screen for DIR03
*:***************************************************************************
*C123847,1
FUNCTION lfSavFit
PRIVATE lcSty,lcOrd,lnQkCnt,lnJ,lcJ,lcQ,lnIncrmnt,lnK,llDfChkQty
Private lnPrvsAls , lnSizeSeq , lnScaleLen , lnQkCnt , lnGrprice , lnCommPrc,;
        lcOldColor , llSkip, lcTmpFit,laStru ,lnSlct,lcPrice

lnSlct = SELECT()
lcTmpFit = gfTempName()
SELECT &lcOrdLine
=AFIELDS(laStru)
CREATE TABLE &gcWorkDir.&lcTmpFit FROM ARRAY laStru
INDEX ON STYLE+STR(Price,12,2) TAG &lcTmpFit  && Saving the Quick order screen should create 
                                              && a separate line for each scale/price.
SELECT &lcTempCur
SET FILTER TO
GO TOP
SCAN
  WAIT WINDOW 'Save data for Style # : ' + STYMAJOR + '/'+ COLOR +'...'    NOWAIT
  FOR lnI = 1 TO 10
    lcI = ALLTRIM(STR(lnI))
    IF !EMPTY(&lcTempCur..SCALE&lcI)
    
      lcSty = &lcTempCur..STYMAJOR+lcSepart+&lcTempCur..COLOR+lcClrSpr+SUBSTR(&lcTempCur..SCALE&lcI,1,3)
      lcSty = PADR(lcSty , 19)
      =SEEK(lcSty,'STYLE')
      =SEEK('S'+STYLE.SCALE,'SCALE')
      
      lcPrice = STR( ROUND( &lcTempCur..GPRIC&lcI * (1-m.Disc_Pcnt/100) ,2 ) , 12,2)
      IF !SEEK(lcSty+lcPrice , lcTmpFit ) .AND. &lcTempCur..S&lcI > 0
        *- Update line count
        lnLines    = lnLines + 1
        
        m.Gl_Sales = ALLTRIM(laData[53])+Style.cSlsGlLink
        m.Gl_Sales = IIF(laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')
        M.COMPLETE = IIF( llCMP2SOLD .AND. !EMPTY(STYLE.SOLDOUT) , STYLE.SOLDOUT , laData[10] )
        SELECT &lcTmpFit
        APPEND BLANK        
        REPLACE CORDTYPE   WITH lcOrdType ;
                ORDER      WITH laData[1] ;
                ACCOUNT    WITH laData[2] ;
                STORE      WITH laData[3] ;
                CUSTPO     WITH IIF(!laData[7],&lcOrdHdr..CustPo,'') ;
                cWareCode  WITH laWareHouse[lnWareHouse,2] ;
                SCALE      WITH LEFT(&lcTempCur..SCALE&lcI,3) ;
                STYLE      WITH lcSty ;
                Desc1      WITH STYLE.Desc1  ;
                PrePak     WITH Style.PrePak ;
                Season     WITH Style.Season ;
                Gl_Sales   WITH m.Gl_Sales ;
                Flag       WITH 'N'        ;
                COMPLETE   WITH M.COMPLETE ;
                Gros_Price WITH &lcTempCur..GPRIC&lcI ;
                Price      WITH ROUND( Gros_Price*(1-m.Disc_Pcnt/100) ,2 ) ;
                COMM1      WITH &lcTempCur..COMM&lcI  ;
                Start      WITH laData[9]  ;
                CUSTPO     WITH laData[4]  ;
                LineNo     WITH lnLines      
        =lfAudtFlds()
      ENDIF

      lcSz = SUBSTR(&lcTempCur..SCALE&lcI,4,1)

      *--Update laData array 
      laData[35] = laData[35] + &lcTempCur..S&lcI
      laData[36] = laData[36] + &lcTempCur..S&lcI*Price
      laData[41] = laData[41] + &lcTempCur..S&lcI
      laData[42] = laData[42] + &lcTempCur..S&lcI*Price
      
      SELECT &lcTmpFit
      REPLACE QTY&lcSz  WITH &lcTempCur..S&lcI ;
              BOOK&lcSz WITH &lcTempCur..S&lcI ;
              TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
              TOTBOOK   WITH TOTQTY   
    ENDIF
  ENDFOR
ENDSCAN

SELECT &lcOrdLine
APPEND FROM &gcWorkDir.&lcTmpFit

USE IN &lcTmpFit
ERASE (gcWorkDir+lcTmpFit+'.DBF')
ERASE (gcWorkDir+lcTmpFit+'.CDX')

SELECT (lnSlct)
*-- end of lfSavFit.

*:**************************************************************************
*:* Name        : lfAudtFlds
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Update audit fields
*:***************************************************************************
*C123847,1
FUNCTION lfAudtFlds
IF laScrMode[4]
  REPLACE CADD_USER WITH gcUser_ID ;
          CADD_TIME WITH TIME()    ;
          DADD_DATE WITH gdSysDate
ELSE
  REPLACE CEDIT_USER WITH gcUser_ID ;
          CEDIT_TIME WITH TIME()    ;
          DEDIT_DATE WITH gdSysDate
ENDIF
*-- end of lfAudtFlds.

*:**************************************************************************
*:* Name        : lfvSz
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/03/2005
*:* Purpose     : Open sizes box
*:***************************************************************************
*:* Called from : icprcls.scx for DIR03
*:***************************************************************************
*C123847,1
FUNCTION lfvSz
PARAMETERS lcSzno
PRIVATE lnSlct,lcTmpSzFit,lcScl,laStru,lnLen,lnI,lcStyle,;
        lnSizePos,lnSizeLen
lnSlct = SELECT()

STORE 0 TO lnSizePos,lnSizeLen
=lfGetClrD() 

SELECT SCALE
=AFIELD(laStru)
lnLen = ALEN(laStru,1)

*- Add price fields
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  lnLen = lnLen + 1
  DIMENSION laStru[lnLen,4]
  laStru[lnLen,1] = 'PRICE'+lcI
  laStru[lnLen,2] = 'N'
  laStru[lnLen,3] = 7
  laStru[lnLen,4] = 2
ENDFOR

*- Fields to save old prices
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  lnLen = lnLen + 1
  DIMENSION laStru[lnLen,4]
  laStru[lnLen,1] = 'OldPRICE'+lcI
  laStru[lnLen,2] = 'N'
  laStru[lnLen,3] = 7
  laStru[lnLen,4] = 2
ENDFOR

lnLen = lnLen + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'FITNO'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 1
laStru[lnLen,4] = 0

lcScl = SUBSTR(STYLE.SCALE,1,lnScaleLen)

*- make a temp file with the scale data 
lcTmpSzFit = 'Q'+SUBSTR(lcFolder,2)
IF !USED(lcTmpSzFit)
  CREATE TABLE (gcWorkDir+lcTmpSzFit) FROM ARRAY laStru
  
  *- Fill the created file with the scale data
  SELECT SCALE
  =SEEK('S'+lcScl,'SCALE')
  M.FitNO = 1
  lcDim1 = SCALE.CDIM1
  SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+lcScl
    IF SCALE.CDIM1 <> lcDim1
      m.FITNO = m.FITNO + 1
      lcDim1 = SCALE.CDIM1
    ENDIF
    SCATTER MEMVAR
    lcStyle = STUFF(STYLE.STYLE,lnSizePos,lnSizeLen,SCALE.SCALE) 
    =SEEK(lcPrcCod+lcCurrCod+lcStyle,'CSTPRICE')
    SELECT CSTPRICE
    SCATTER FIELDS PRICE1,PRICE2,PRICE3,PRICE4,PRICE5,PRICE6,PRICE7,PRICE8 MEMVAR
    STORE PRICE1 TO M.PRICE1,M.OLDPRICE1
    STORE PRICE2 TO M.PRICE2,M.OLDPRICE2
    STORE PRICE3 TO M.PRICE3,M.OLDPRICE3
    STORE PRICE4 TO M.PRICE4,M.OLDPRICE4
    STORE PRICE5 TO M.PRICE5,M.OLDPRICE5
    STORE PRICE6 TO M.PRICE6,M.OLDPRICE6
    STORE PRICE7 TO M.PRICE7,M.OLDPRICE7
    STORE PRICE8 TO M.PRICE8,M.OLDPRICE8
    
    INSERT INTO &lcTmpSzFit FROM MEMVAR
  ENDSCAN
ENDIF

SELECT &lcTmpSzFit
SET FILTER TO
GO TOP
IF EMPTY(&lcTmpSzFit..CDIM1)
  SET FILTER TO RECNO() = VAL(lcSzno)
ELSE
  GOTO VAL(lcSzno)
  m.FITNO = &lcTmpSzFit..FITNO
*  SET FILTER TO FIT = m.FIT
  SET FILTER TO 
ENDIF
LOCATE

lnMrk = 1
lcRLTit = 'Sizes Prices'
lcLotRo  = gfTempName()
lcLotRo2 = gfTempName()
lcLotRo3 = gfTempName()
lcSzFlds = 'SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8'
SCATTER FIELDS &lcSzFlds MEMVAR

*- Use a browse for the fit prices
ON KEY LABEL ALT+R ACTIVATE WINDOW (lcRLTit)
DO (gcScrDir+gcWinAppl+"\ICPRFIT.SPX")

SELECT (lnSlct)
*-- end of lfvSz.
*:**************************************************************************
*:* Name        : lfFitBr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/06/2005
*:* Purpose     : Browse function for FIT
*:***************************************************************************
*:* Called from : ICPRFIT
*:***************************************************************************
FUNCTION lfFitBr

PRIVATE lnI, lcBrFlds, lcBrTitle, lcKey, lnSlct, ;
        lcSz1, lcSz2, lcSz3, lcSz4, lcSz5, lcSz, lcSz7, lcSz8,;
        lcTmpSzFit
lnSlct = SELECT(0)
lcTmpSzFit = 'Q'+SUBSTR(lcFolder,2)

SELECT &lcTmpSzFit
lcSz1 = ALLTRIM(&lcTmpSzFit..SZ1)
lcSz2 = ALLTRIM(&lcTmpSzFit..SZ2)
lcSz3 = ALLTRIM(&lcTmpSzFit..SZ3)
lcSz4 = ALLTRIM(&lcTmpSzFit..SZ4)
lcSz5 = ALLTRIM(&lcTmpSzFit..SZ5)
lcSz6 = ALLTRIM(&lcTmpSzFit..SZ6)
lcSz7 = ALLTRIM(&lcTmpSzFit..SZ7)
lcSz8 = ALLTRIM(&lcTmpSzFit..SZ8)

lnMrk = RECNO(lcTmpSzFit)
lcBrFlds = "lcMrk=IIF(lnMrk=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
           "CDIM1 :H='FIT' :W=.F. :R:10,"+;
           "Price1    :H='' :W=!EMPTY(Sz1) :7,"+;
           "Price2    :H='' :W=!EMPTY(Sz2) :7,"+;
           "Price3    :H='' :W=!EMPTY(Sz3) :7,"+;
           "Price4    :H='' :W=!EMPTY(Sz4) :7,"+;
           "Price5    :H='' :W=!EMPTY(Sz5) :7,"+;
           "Price6    :H='' :W=!EMPTY(Sz6) :7,"+;
           "Price7    :H='' :W=!EMPTY(Sz7) :7,"+;
           "Price8    :H='' :W=!EMPTY(Sz8) :7 "
         
BROWSE FIELDS &lcBrFlds;
       LOCK 0  ;
       NOAPPEND;
       NOCLEAR ;
       NODELETE;
       NOMENU  ;
       NOWAIT  ;
       SAVE    ;
       WHEN lfwBrows()    ;
       TITLE (lcRLTit)    ;
       WINDOW (lcLotRo2)  ;
       IN WINDOW (lcLotRo)

=lfRefresh()

SELECT (lnSlct)

*-- end of lfFitBr.

*:**************************************************************************
*:* Name        : lfwBrows
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2005
*:* Purpose     : When function for the FitBrowse screen
*:***************************************************************************
FUNCTION lfwBrows
lnMrk  = RECNO(lcTmpSzFit)
SCATTER FIELDS &lcSzFlds MEMVAR
SHOW WINDOW (lcRLTit)  REFRESH
=lfRefresh()
*-- end of lfwBrows.

*:**************************************************************************
*:* Name        : lfvFitOk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2005
*:* Purpose     : Ok valid funtion for the fit prices
*:***************************************************************************
FUNCTION lfvFitOk
SELECT &lcTmpSzFit
REPLACE ALL OLDPRICE1 WITH PRICE1 ;
            OLDPRICE2 WITH PRICE2 ;
            OLDPRICE3 WITH PRICE3 ;
            OLDPRICE4 WITH PRICE4 ;
            OLDPRICE5 WITH PRICE5 ;
            OLDPRICE6 WITH PRICE6 ;
            OLDPRICE7 WITH PRICE7 ;
            OLDPRICE8 WITH PRICE8

*-- end of lfvFitOk.

*:**************************************************************************
*:* Name        : lfvFitCan
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2005
*:* Purpose     : Cancel Valid funtion for the fit prices
*:***************************************************************************
FUNCTION lfvFitCan
SELECT &lcTmpSzFit
REPLACE ALL PRICE1 WITH OLDPRICE1 ;
            PRICE2 WITH OLDPRICE2 ;
            PRICE3 WITH OLDPRICE3 ;
            PRICE4 WITH OLDPRICE4 ;
            PRICE5 WITH OLDPRICE5 ;
            PRICE6 WITH OLDPRICE6 ;
            PRICE7 WITH OLDPRICE7 ;
            PRICE8 WITH OLDPRICE8

*-- end of lfvFitCan.

*:**************************************************************************
*:* Name        : lfUpDirPrc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2005
*:* Purpose     : if Called from DIR03 then update the fields PRICE1,..., PRICE8
*:***************************************************************************
*:* Called from : lfvSave
*:***************************************************************************
FUNCTION lfUpDirPrc
PRIVATE lnSlct,lcTmpSzFit

lnSlct = SELECT()
lcTmpSzFit = 'Q'+SUBSTR(lcFolder,2)
IF USED(lcTmpSzFit)
  
  SELECT &lcTmpSzFit
  SET FILTER TO 
  GO TOP
  
  LOCATE FOR SCALE = STYLE.SCALE
  SELECT CSTPRICE 
  REPLACE PRICE1 WITH &lcTmpSzFit..PRICE1 ;
          PRICE2 WITH &lcTmpSzFit..PRICE2 ;
          PRICE3 WITH &lcTmpSzFit..PRICE3 ;
          PRICE4 WITH &lcTmpSzFit..PRICE4 ;
          PRICE5 WITH &lcTmpSzFit..PRICE5 ;
          PRICE6 WITH &lcTmpSzFit..PRICE6 ;
          PRICE7 WITH &lcTmpSzFit..PRICE7 ;
          PRICE8 WITH &lcTmpSzFit..PRICE8 

ENDIF
*-- end of lfUpDirPrc.

*:**************************************************************************
*:* Name        : lfvprClo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/27/2005
*:* Purpose     : Close the "ICPRDIR" screen
*:***************************************************************************
*:* Called from : "ICPRDIR" screen 
*:***************************************************************************
FUNCTION lfvprClo
CLEAR READ
*-- end of lfvprClo.
*:**************************************************************************
*:* Name        : lfvprdir
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/27/2005
*:* Purpose     : Valid function for 'ICPRFIT
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvprdir()
*:***************************************************************************
FUNCTION lfvPrFit
RETURN .F.
*-- end of lfvprdir.

*:**************************************************************************
*:* Name        : lfvFitSv
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/27/2005
*:* Purpose     : fit screen save procedure
*:***************************************************************************
FUNCTION lfvFitSv
SELECT &lcTmpSzFit
LOCATE
REPLACE ALL ;
        OldPRICE1 WITH PRICE1 ;
        OldPRICE2 WITH PRICE2 ;
        OldPRICE3 WITH PRICE3 ;
        OldPRICE4 WITH PRICE4 ;
        OldPRICE5 WITH PRICE5 ;
        OldPRICE6 WITH PRICE6 ;
        OldPRICE7 WITH PRICE7 ;
        OldPRICE8 WITH PRICE8 
CLEAR READ

*-- end of lfvFitSv.

*:**************************************************************************
*:* Name        : lfvFitClr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/27/2005
*:* Purpose     : Fit screen clear procedure
*:***************************************************************************
FUNCTION lfvFitClr
SELECT &lcTmpSzFit
LOCATE
REPLACE ALL ;
        PRICE1 WITH OldPRICE1 ;
        PRICE2 WITH OldPRICE2 ;
        PRICE3 WITH OldPRICE3 ;
        PRICE4 WITH OldPRICE4 ;
        PRICE5 WITH OldPRICE5 ;
        PRICE6 WITH OldPRICE6 ;
        PRICE7 WITH OldPRICE7 ;
        PRICE8 WITH OldPRICE8 
CLEAR READ

*-- end of lfvFitClr.

*:**************************************************************************
*:* Name        : lfClrTrap2
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/17/2004
*! Purpose   : Clear Trap Function of the Browse window ( Carton Types Screen )
*:***************************************************************************
*C123616,1
FUNCTION lfClrTrap2

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

IF glFromBrow
*  =lfFitBr()
*  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESCAPE llDummy = .T.

*:**************************************************************************
*:* Name        : lfTrap2
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/27/2005
*:* Purpose     : Deactivate fn.
*:***************************************************************************
FUNCTION lfTrap2

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key
IF WONTOP()  = lcRLTit
  glFromBrow = .T.
*  ON KEY LABEL TAB     DO lfBrTab2
*  ON KEY LABEL BACKTAB DO lfBrBack2
  ON KEY LABEL TAB     
  ON KEY LABEL BACKTAB 
  ON KEY LABEL ESCAPE llDummy = .T.
ENDIF

*:***************************************************************************
*:***************************************************************************
FUNCTION lfBrTab2
ON KEY LABE TAB
IF WONTOP()  = lcRLTit
  ACTIVATE WINDOW (lcLotRo3)
  _CUROBJ = OBJNUM(pbOk)
ENDIF

*:***************************************************************************
*:***************************************************************************
FUNCTION lfBrBack2
ON KEY LABE BACKTAB	
IF WONTOP()  = lcRLTit
  ACTIVATE WINDOW (lcLotRo3)
  _CUROBJ = OBJNUM(pbOk)
ENDIF

***----mhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmhmh
*! C123853,1 MHM 01/15/2005 * my modification related to GFStyctrl Between 
*                           *  
*!**************************************************************************
*! Name      : lfDLSTYCRL
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/09/2005
*! Purpose   : we copy the Standard GFSTYCTRL and modify it to work with bin location
*!**************************************************************************
*! Example   : =lfEraseMem()
*!**************************************************************************

FUNCTION lfDLSTYCRL

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  IF TYPE('lcType') = 'U'
    PRIVATE lcFileNama
    lcFileNamA = lcTmpQuery +"A"
    lcBinLoc = &lcFileNamA..cLocation 

    lnRet = lfStyCrl('9',lcPStyle,ladata[8],lcDye1,ladata[5],'',@laAdjust,laAdjust[10],;
                     '',.T.,lcAdjReason,1,'MDINVNTL','NSTEPS',@laGLDistAr,0,'','',@laOldStk)
    =lfUPDWHBIN(lcBinLoc)
  ELSE
    lnRet=lfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                   lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
  ENDIF
ELSE
   *-- to handle case of not bin location and called from inventory locking
   IF TYPE('lcAdjTyp') = 'U'
      lnRet = gfStyCrl('9',lcPStyle,ladata[8],lcDye1,ladata[5],'',@laAdjust,laAdjust[10],;
                           '',.T.,lcAdjReason,1,'MDINVNTL','NSTEPS',@laGLDistAr,0,'','',@laOldStk)
   ELSE
    lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                   lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
   ENDIF
ENDIF


*!**************************************************************************
*! Name      : lfSTYCRL
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/09/2005
*! Purpose   : we copy the Standard GFSTYCTRL and modify it to work with bin location
*!**************************************************************************
*! Example   : =lfEraseMem()
*!**************************************************************************

FUNCTION lfSTYCRL
PARAMETERS lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,;
           laAdjStk,lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn,;
           lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,lnLineNo,;
           lcLastRSess,lcAdjRef,laLockInfo, lnTranCost
PRIVATE lnTranCost
IF TYPE('lnTranCost') $ 'LU'
  lnTranCost = 0
ENDIF  

lcSysType = gfGetMemVar('M_SYSTYPE')
IF lcSysType = 'P'
  PRIVATE llOpnWarhs 
  llOpnWarhs = .F.
  IF !USED('WAREHOUS')
    llOpnWarhs = gfOpenFile(gcDataDir+"WAREHOUS","WAREHOUS","SH")
  ENDIF
  IF SEEK(lcWareCode,'WAREHOUS') AND WAREHOUS.cSiteId <> gcCurSite
    IF llOpnWarhs
      USE IN STYINVJL
    ENDIF
    RETURN (1)
  ENDIF
ENDIF

*! C123853,1 MHM 01/15/2005 Open needed files [Start]
IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF

IF !(TYPE('lcType') = 'U')
  PRIVATE lcFileNama
  lcFileNama = lcTmpAdj+"A"
  lcBinLoc = &lcFileNama..LocFrom
ENDIF  
*! C123853,1 MHM 01/15/2005[End]


PRIVATE lnStkVal
STORE 0 TO lnStkVal

PRIVATE lnLineNo
IF TYPE('lnLineNo') = 'L'
  lnLineNo = 0
ENDIF  

PRIVATE lcLastRSess
IF TYPE('lcLastRSess') = 'L'
  lcLastRSess = SPACE(6)
ENDIF  

PRIVATE lcAdjRef
IF TYPE('lcAdjRef') = 'L'
  lcAdjRef = SPACE(6)
ENDIF

*--Initialize function variables.
PRIVATE lcOldWAr,laOldstk,llChekUncmp,lnSAveCost,lnWAveCost,;
        lnSOldStk,lnSOldCst,lnWOldStk,lnWOldCst,lcCostMeth,;
        lcAdjAcct,lcTmpJour,lcInvJour,lnRetStep,llUInvtry,;
        lnSStkVal,lnWStkVal

*--Style and Warehouse Average Cost,Old Stock and Old Cost variables.
STORE 0 TO lnSAveCost,lnSOldStk,lnSOldCst
STORE 0 TO lnWAveCost,lnWOldStk,lnWOldCst
DIME laOldstk[9]
laOldstk   = 0
lcOldWAr   = ALIAS()                && Current Work aera.
lcAdjCdRsn = IIF(TYPE('lcAdjCdRsn') $ 'UL','',lcAdjCdRsn)
lcAdjAcct  = ' '                    && Adjustment Code GL Account.  
*--Dyelot if not used must be 10 chr len,needed in exprestion.
lcSDyelot  = IIF(EMPTY(lcSDyelot),SPACE(10),lcSDyelot)

*--Check Uncomplete session flag if steps are passed as value not as zero.
llChekUncmp = ( lnStarStep <> 0 )    
*--Check if needed to update G/L.
llGLUsed = IIF(TYPE('laGLInvAry') $ 'UL',.F.,IIF(EMPTY(laGLInvAry[1,1]),.F.,.T.))
*--Return step to continue for after exit the function.
lnRetStep   = 0
*--Check the costing method ,Average ,Standard ,FIFO or LIFO.
lcCostMeth = gfGetMemVar('M_Cost_Meth')


*--Check the existing of the style and
*--Point the record in style and style dyelot files.
IF ! SEEK(lcStyle,'STYLE') OR !SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
  *--The style ???? record are missing,
  *--Cannot proceed with updating Stock,
  *--This transaction line will be ignored.
  =gfModalGen('TRM42114B42000','DIALOG',lcStyle)
  RETURN (0)
ENDIF


*--Check if StyInvJL file is Open.
llOpnJurnl=gfOpenFile(gcDataDir+"StyInvJl","StyInvJl","SH")

*--Check the Transaction Type if it Issue or Receive 'I' or 'R'.
*--Depends on Total adjusted stock is negative or positive.
lcIRType = IIF(laAdjStk[9]<0 AND lcTrType $ '123689I' , 'I' , 'R' )



*--Check the style Inventory Yes or No.
llUInvtry = STYLE.lInvSty
*--Get the Old Stock and Cost before updateing the new tansaction.

lnSOldStk = STYLE.TotStk
lnSOldCst = ABS(IIF(Style.TotStk=0,STYLE.Ave_Cost,STYLE.nStkVal / Style.TotStk))
lnWOldStk = STYDYE.TotStk

IF lcTrType = '9'
  lnWOldCst = IIF(laLockInfo[9]=0,0,laLockInfo[10]/laLockInfo[9])
ELSE
  lnWOldCst = ABS(IIF(StyDye.TotStk=0,STYDYE.Ave_Cost,STYDYE.nStkVal / StyDye.TotStk))
ENDIF  

*--Stock Value variable for style and StyDye.
lnSStkVal = IIF(lcTrType $ '29',0,STYLE.nStkVal )
lnOldSVal = IIF(lcTrType $ '29',STYDYE.nStkVal,0)

IF lcTrType = '9'
  lnWStkVal = laLockInfo[10]
ELSE
  lnWStkVal = IIF(lcTrType $ '29',0,STYDYE.nStkVal)
ENDIF

PRIVATE lnDyeCost
lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)


PRIVATE lnPrvQty,lnPrvVal
lnPrvQty = StyDye.TotStk
lnPrvVal = StyDye.nStkVal
IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  lnPrvQty = StyDye.TotStk
  lnPrvVal = StyDye.TotStk * lnDyeCost
ENDIF

IF lcIRType = 'I' AND (lcTrType <> 'I' OR lcCostMeth $ 'FL')

  DO CASE
    CASE lcCostMeth = 'A'   && Average.
      lnNewCost = IIF((gfGetMemVar('M_WareHouse')='Y'),lnWOldCst ,lnSOldCst )

    CASE lcCostMeth = 'S'   && Standard.
      lnNewCost = STYLE.TotCost

    CASE lcCostMeth $ 'FL'  && FIFO or LIFO.
      *--In this case may be has more than cost so we hold this costs
      *--in lcTmpJour file that the following function will return.
      lcTmpJour = gfTempName()
      IF ! lfIsueCost(.F.)
        SELECT (lcOldWAr)
        RETURN (0)
      ENDIF
  ENDCASE
ENDIF

IF lcTrType = '9'
  = ACOPY(laLockInfo,laOldstk,1,9)
  lnOldSVal = laLockInfo[10]
ELSE
  SELECT STYDYE
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldstk
ENDIF

*--Temp Old Stock Array used in Physical or Markdown 
*--issue the old stock first and then receive the pysical quantity.
*-- In all other cases this array is Zero.

IF lcTrType $ '2'
  *! C123853,1 MHM 01/15/2005 modification of Bin Location  [Start]
  *SELECT STYDYE
  *IF !EMPTY(lcSDyelot)
  *  = SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  *ENDIF
  SELECT WHBINLOC
  IF !EMPTY(lcBinLoc)
    *--mhmwael
    *= SEEK(lcStyle+lcWareCode+lcBinLoc,'WHBINLOC')
    = SEEK(lcWareCode+lcBinLoc+lcStyle,'WHBINLOC')
    *--mhmwael
  ENDIF
  FOR lnCnt = 1 TO 8 
    lcCnt = STR(lnCnt, 1)
    laOldstk[lnCnt] = WHBINLOC.Qty&lcCnt
  ENDFOR
  laOldstk[9] = WHBINLOC.TotQty
  *! C123853,1 MHM 01/15/2005 [End]
ENDIF

*--Calculate Avarage Cost for Style and StyDye records. 

*-- 1)  Update Style journal file. -------------------------------------
*--Read session no.
*--If receiving transaction and costing methos Lifo or Fifo make sure
*--that the session not duplecated fir same key.
lcRISessn = IIF( TYPE('lcRISessn')='C', lcRISessn ,'' )
IF lcIRType = 'R' AND lcCostMeth $ 'FL' AND !EMPTY(lcRISessn)
  lnJrlRec  = IIF(EOF('STYINVJL'),0,RECNO('STYINVJL'))
  lcRISessn = IIF(SEEK(lcStyle+lcWareCode+lcRISessn,'STYINVJL'),'',lcRISessn)
  IF lnJrlRec<>0
    GOTO lnJrlRec IN STYINVJL
  ENDIF
ENDIF
*--Read session no.
IF EMPTY(lcRISessn)
  lcRISessn = gfSequence('GLSESSION')
ENDIF

*--Read the adjustment code reason to get the GL Account.
IF !EMPTY(lcAdjCdRsn)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  =gfRltFld(lcAdjCdRsn , @laTrmRltFd , "CADJREASON")
ENDIF

*--Initialize next step to continue.
lnTmpStp = lnStarStep

*--Update journal for Issue Transaction ,FIFO or LIFO method.
IF lcIRType = 'I' AND lcCostMeth $ 'FL'

   lnIssTCst = 0
   lnIssTStk = 0

  SELECT (lcTmpJour)  
  SCAN
    REPLACE cSession  WITH lcRISessn,;
            cISession WITH cSession,;
            cTrCode   WITH IIF(cTrType $ "12" AND EMPTY(lcTrCode),cSession,cTrCode)
    SCATTER MEMVAR
    IF lfCheckUnCmp(lnTmpStp)      
      SELECT STYINVJL
      APPEND BLANK
      GATHER MEMVAR  

      lnIssTCst = lnIssTCst + m.nTotStk * m.nCost
      lnIssTStk = lnIssTStk + m.nTotStk

      REPLACE Reference  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
              cAdjReason WITH lcAdjCdRsn,;
              cAdjAcct   WITH lcAdjAcct,;
              nStkVal    WITH nTotStk * nCost,;
              LineNo     WITH lnLineNo,;
              nPrvSQty   WITH lnPrvQty,;
              nPrvSval   WITH lnPrvVal

      REPLACE cAdjRef    WITH lcAdjRef
      
      *-- Call global function to add audit fields info.
      =gfAdd_Info('STYINVJL')
      *! C123853,1 MHM 01/15/2005 in case of Inventory lock [Start]
      IF lcTrType='9'
        SCATT MEMVAR MEMO
        m.clocation   = lcBinLoc 
        INSERT INTO ('BININVJL') FROM MEMVAR
      ENDIF
      *! C123853,1 MHM 01/15/2005 [End]

      *--Update Uncomplete session Step.
      =lfUpdStep(lnTmpStp)  

      *--Call TraceKey global function.
      =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
    ENDIF  
    lnTmpStp = lnTmpStp + 1

    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()

    = lfStyWarDy()

  ENDSCAN

  IF lnIssTStk <> 0
    lnIssAvg = ABS(lnIssTCst / lnIssTStk )
  ELSE
    lnIssAvg = 0
  ENDIF  

  IF USED(lcTmpJour)
    USE IN (lcTmpJour)
  ENDIF
  *--Erase the temp. journal file.
  ERASE (gcWorkDir+lcTmpJour+'.DBF')
  ERASE (gcWorkDir+lcTmpJour+'.CDX')

ELSE  && Not LIFO or FIFO or Receiving.

  *--Create an issue record for Physical inventory or 
  *--Markdown inventory transaction in Style inventory Journal.

  IF lcTrType $ '29'
    IF lfDoPhys('I')
      =lfIsuJlTr()
      STORE 0 TO lnPrvQty,lnPrvVal
    ENDIF
  ENDIF

  IF !(lcTrType $ '29') AND lcIRType = 'R' AND lnWOldStk < 0 AND lnWOldCst <> lnNewCost
    *-- This is to create 2 records in journal file
    *-- one for rec. the qty with it's old cost
    *-- the other for issue the qty with it's new cost
    = lfAdjRec()
  ENDIF

    *--Create a main record in journal file.
    IF !(lcTrType $ '29') OR (lcTrType $ '29' AND lfDoPhys('R'))          
      IF lfCheckUnCmp(lnTmpStp)      
        SELECT STYINVJL
    
        = lfCalcStkVal()
        APPEND BLANK
        *B608092,1 TMI [Start] 
        * remove the command          "nStkVal    WITH lnStkVal   ,;"
        * replace it with the command "nStkVal    WITH nCost * nTotStk ,;"
        * so that the multiplication result always correct
        *REPLACE cSession   WITH lcRISessn,;
                Style      WITH lcStyle,;
                cWareCode  WITH lcWareCode,;
                cDyelot    WITH lcSDyelot,;
                dTrDate    WITH ldTrDate,;
                cTrType    WITH lcTrType,;
                cTrCode    WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),lcRISessn,lcTrCode),;
                nCost      WITH lnNewCost,;
                cIRType    WITH lcIRType,;
                nStk1      WITH laAdjStk[1],;
                nStk2      WITH laAdjStk[2],;
                nStk3      WITH laAdjStk[3],;
                nStk4      WITH laAdjStk[4],;
                nStk5      WITH laAdjStk[5],;
                nStk6      WITH laAdjStk[6],;
                nStk7      WITH laAdjStk[7],;
                nStk8      WITH laAdjStk[8],;
                nTotStk    WITH laAdjStk[9],;
                nStkVal    WITH lnStkVal   ,;
                Reference  WITH IIF(ctrType = '2' AND lcIRType = 'I','Auto. zeroing of stock',lcRefer),;
                lLockFlg   WITH IIF(lcTrType='9',.T.,lLockFlg),;
                cAdjReason WITH lcAdjCdRsn ,;
                cAdjAcct   WITH lcAdjAcct  ,;
                cISession  WITH IIF(cIRType='I',cSession,''),;
                cRSession  WITH IIF(cIRType='R',cSession,''),;
                LineNo     WITH lnLineNo,;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSVal   WITH lnPrvVal
        REPLACE cSession   WITH lcRISessn,;
                Style      WITH lcStyle,;
                cWareCode  WITH lcWareCode,;
                cDyelot    WITH lcSDyelot,;
                dTrDate    WITH ldTrDate,;
                cTrType    WITH lcTrType,;
                cTrCode    WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),lcRISessn,lcTrCode),;
                nCost      WITH lnNewCost,;
                cIRType    WITH lcIRType,;
                nStk1      WITH laAdjStk[1],;
                nStk2      WITH laAdjStk[2],;
                nStk3      WITH laAdjStk[3],;
                nStk4      WITH laAdjStk[4],;
                nStk5      WITH laAdjStk[5],;
                nStk6      WITH laAdjStk[6],;
                nStk7      WITH laAdjStk[7],;
                nStk8      WITH laAdjStk[8],;
                nTotStk    WITH laAdjStk[9],;
                nStkVal    WITH nCost * nTotStk ,;
                Reference  WITH IIF(ctrType = '2' AND lcIRType = 'I','Auto. zeroing of stock',lcRefer),;
                lLockFlg   WITH IIF(lcTrType='9',.T.,lLockFlg),;
                cAdjReason WITH lcAdjCdRsn ,;
                cAdjAcct   WITH lcAdjAcct  ,;
                cISession  WITH IIF(cIRType='I',cSession,''),;
                cRSession  WITH IIF(cIRType='R',cSession,''),;
                LineNo     WITH lnLineNo,;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSVal   WITH lnPrvVal
        *B608092,1 TMI [End  ] 
        REPLACE cAdjRef    WITH lcAdjRef

        REPLACE nTranCost WITH lnTranCost
        
        *-- Call global function to add audit fields info.
        =gfAdd_Info('STYINVJL')
        *! C123853,1 MHM 01/15/2005 in case of inventory lock [Start]
        IF lcTrType='9'
          SCATT MEMVAR MEMO
          m.clocation   = lcBinLoc 
          INSERT INTO ('BININVJL') FROM MEMVAR
        ENDIF
        *! C123853,1 MHM 01/15/2005 [End]

        *--Update Uncomplete session Step.
        =lfUpdStep(lnTmpStp)  

        *--Call TraceKey global function.
        =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
      ENDIF
      lnTmpStp = lnTmpStp + 1


      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfStyWarDy()

    IF lcTrType = '9'
      = lfLkAdjRec()
    ENDIF
  ENDIF
ENDIF

*--Initialize next step to continue when return.
lnRetStep = lnTmpStp

*--Close style journal if this function open it.
IF llOpnJurnl AND USED("StyInvJl")
  USE IN STYINVJL
ENDIF

SELECT (lcOldWAr)
RETURN IIF(llChekUncmp , lnRetStep , 1 )
*--End of gfStyCrl.prg

*!***********************************************************************
*! Name      : lfCheckUnCmp
*! Developer : Timour A. K.
*! Date      : 01/22/98
*! Purpose   : Function To Check uncomplete session Steps.
*!***********************************************************************
*! Return    : .T. for Check , .F. for Already checked no need to check.
*!***********************************************************************
*! Example   : lfCheckUnCmp()
*!***********************************************************************
FUNCTION lfCheckUnCmp
PARA lnStepNo

IF !llChekUncmp
   RETURN .T.
ELSE
  RETURN ( &lcTmpLFile..&lcStepFld < lnStepNo )
ENDIF


*!***********************************************************************
*! Name      : lfUpdStep
*! Developer : Timour A. K.
*! Date      : 01/22/98
*! Purpose   : Update uncomplete session step.
*!***********************************************************************
*! Example   : lfUpdStep()
*!***********************************************************************
FUNCTION lfUpdStep
PARA lnCurntStep
PRIVATE lnAlas
IF llChekUncmp
  lnAlas = SELECT()
  SELECT (lcTmpLFile)
  =RLOCK()
  REPLACE &lcStepFld WITH lnCurntStep
  UNLOCK
  SELECT(lnAlas)
ENDIF
RETURN


*!*************************************************************
*! Name      : lfIsueCost
*! Developer : Timour A. K.
*! Date      : 01/22/98
*! Purpose   : This function will get the journal records for the
*!             receiving transactions that will be applied on
*!             the current Issue transaction and take the cost of
*!             the receiving to be a new cost of issueing,
*!              depends on cost method FIFO or LIFO.    
*!*************************************************************
*! Parameter : If it calls for Physical inventorty procedure.
*!*************************************************************
*! Returns   : A temp file that contain all needed information
*!             this file named as 'lcTmpJour'
*!             If this function returns .F. that means that there
*!             is somthing wrong with creating data. 
*!*************************************************************
*! Example   :  =lfIsueCost()
*!*************************************************************
FUNCTION lfIsueCost
PARA llForPhys

PRIVATE lcAlias,llContnu,laTotRcvd
lcAlias = ALIAS()

*! C123853,1 MHM 01/15/2005 modification of Bin Location  [Start]
*SELECT STYINVJL
**--Create the Temp journal file with open receiving transactions.
*SELECT cSession,Style,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,nCost,;
*       cIRType,cRSession,cISession,;
*       SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',;
*       SUM(nStk4) AS 'nStk4',SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',;
*       SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
*       SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', .F. AS 'lNeeded' ,SPACE(6) as cAdjRef ;
*FROM  STYINVJL ;
*WHERE Style + cWareCode + cDyelot + cRSession + cISession = ;
*      lcStyle + lcWareCode + lcSDyelot ;
*GROUP BY STYINVJL.Style,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
*ORDER BY STYINVJL.Style,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
*INTO DBF (gcWorkDir+lcTmpJour)

SELECT BININVJL
*--Create the Temp journal file with open receiving transactions.
SELECT cSession,Style,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,nCost,;
       cIRType,cRSession,cISession,;
       SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',;
       SUM(nStk4) AS 'nStk4',SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',;
       SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
       SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', .F. AS 'lNeeded' ,SPACE(6) as cAdjRef ;
FROM  BININVJL ;
WHERE Style + cWareCode + cDyelot + cRSession + cISession = ;
      lcStyle + lcWareCode + lcSDyelot AND clocation =  lcBinLoc;
GROUP BY BININVJL.Style,BININVJL.cWareCode,BININVJL.cDyelot,BININVJL.cRSession ;
ORDER BY BININVJL.Style,BININVJL.cWareCode,BININVJL.cDyelot,BININVJL.cRSession ;
INTO DBF (gcWorkDir+lcTmpJour)
*! C123853,1 MHM 01/15/2005 modification of Bin Location  [End]

SELECT (lcTmpJour)

DELETE ALL FOR nStk1=0 AND nStk2=0 AND nStk3=0 AND nStk4=0 AND nStk5=0 AND nStk6=0 AND nStk7=0 AND nStk8=0

GO TOP
IF EOF()
  IF !llForPhys
    *--No open receiving exist for style XXXX ,
    *--This transaction line will be ignored.
    =gfModalGen('TRM42116B42000','DIALOG',lcStyle)
    USE
    RETURN .F.
  ELSE  && Get issue for Physical transactions.
    APPEND BLANK
    REPLACE cSession  WITH lcRISessn,;
            Style     WITH lcStyle,;
            cWareCode WITH lcWareCode,;
            cDyelot   WITH lcSDyelot,;
            nCost     WITH lnWOldCst,;
            dTrDate   WITH ldTrDate,;
            cTrType   WITH lcTrType,;
            cTrCode   WITH lcTrCode,;
            cIRType   WITH "I",;
            nStkVal   WITH lnOldSVal

    REPLACE cAdjRef    WITH lcAdjRef

    RETURN
  ENDIF 
ENDIF

*--For Not Phyical.
IF !llForPhys
  *--Indexing the file on Ascending or Descending expresion 
  *--depends on LIFO or FIFO method.
  IF lcCostMeth = 'F'
    INDEX ON Style+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour
  ELSE
    INDEX ON Style+cWareCode+cDyelot+cRSession+cISession DESCENDING TAG &lcTmpJour
  ENDIF
  GO TOP


  *--Start checking the only needed open receinving transaction for this
  *--issue transaction and put zero for all not needed receivings.

  *--Array to Hold the accomulation of the receiving untill it cover
  *--the issue quantity needed.
  DIME laTotRcvd[9]
  laTotRcvd = 0      
  SCAN
    llContnu  = .F.
    FOR I=1 TO 8 
      Z=STR(I,1)
      IF ABS(laAdjStk[I]) > laTotRcvd[I]
        llContnu = .T.
        laTotRcvd[I] = laTotRcvd[I] + nStk&Z
        laTotRcvd[9] = nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        IF ABS(laAdjStk[I]) <= laTotRcvd[I]
          REPLACE nStk&Z  WITH nStk&Z - (laTotRcvd[I] - ABS(laAdjStk[I]))
          REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
          REPLACE lNeeded WITH .T.

        ELSE
          REPLACE lNeeded WITH .T.

        ENDIF   
      ELSE
        REPLACE nStk&Z  WITH 0
        REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        REPLACE lNeeded WITH .T.
      ENDIF   
    ENDFOR 
    IF !llContnu 
      EXIT
    ENDIF
  ENDSCAN

  *--Check if all Issue quantity are covered by the receivings.
  IF llContnu 
    FOR I=1 TO 8 
      Z=STR(I,1)
      IF laAdjStk[I] < 0 and ABS(laAdjStk[I]) > laTotRcvd[I]
        *--The receiving quantity are not covered the issued quantity
        *--for Style XXXX , This transaction line will be ignored.
        =gfModalGen('TRM42115B42000','DIALOG',lcStyle)
        USE
        RETURN .F. 
      ENDIF
    ENDFOR
  ENDIF

  *--Delete all not needed receiving transactions.
  DELETE ALL FOR nTotStk = 0 OR !lNeeded
ENDIF

*--Change it to Issue transactions,to use it in updating master Journal file.
REPLACE ALL cIRType WITH "I"    ,;
            dTrDate WITH ldTrDate,;
            cTrType WITH lcTrType,;
            cTrCode WITH lcTrCode,;
            nStk1   WITH -nStk1 ,;
            nStk2   WITH -nStk2 ,;  
            nStk3   WITH -nStk3 ,;  
            nStk4   WITH -nStk4 ,;  
            nStk5   WITH -nStk5 ,;  
            nStk6   WITH -nStk6 ,;  
            nStk7   WITH -nStk7 ,;  
            nStk8   WITH -nStk8 ,;  
            nTotStk WITH -nTotStk,;
            nStkVal WITH -nStkVal

SELECT (lcAlias)
RETURN .T.


*!*************************************************************
*! Name      : lfIsuJlTr
*! Developer : Timour A. K.
*! Date      : 01/22/98
*! Purpose   : This function will update the journal file with 
*!             Issue transaction record(s) for Physical or
*!             markdown transactions only.  
*!             If the method is Standard or Average we create
*!             only one Issue record for old stock before 
*!             physical transaction was done Else if the method
*!             is LIFO or FIFO we create issue records depends
*!             on all open receivings exist in journal.
*!*************************************************************
*! Call      :  lfIsueCost(.T.)
*!*************************************************************
*! Example   :  =lfIsuJlTr()
*!*************************************************************
FUNCTION lfIsuJlTr

IF lcCostMeth $ 'FL'  && ISSUE FIFO or LIFO.
  *--Get the open receivings.
  lcTmpJour = gfTempName()
  =lfIsueCost(.T.)

  SELECT (lcTmpJour)
  SCAN
    IF nTotStk <> 0 OR nStkVal <> 0
      REPLACE cSession  WITH lcRISessn,;
              cISession WITH cSession,;
              cTrCode   WITH IIF(cTrType $ "12",cSession,cTrCode)
      SCATTER MEMVAR       
      IF lfCheckUnCmp(lnTmpStp)
        SELECT STYINVJL
        APPEND BLANK
        GATHER MEMVAR
        REPLACE Reference  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
                cAdjReason WITH lcAdjCdRsn,;
                cAdjAcct   WITH lcAdjAcct ,;
                nStkVal    WITH IIF(nTotStk = 0,nStkVal,nTotStk * nCost),;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSval   WITH lnPrvVal
        
        REPLACE cAdjRef    WITH lcAdjRef

        REPLACE lLockFlg  WITH (lcTrType='9')

        REPLACE nTranCost WITH lnTranCost
        
        *-- Call global function to add audit fields info.
        =gfAdd_Info('STYINVJL')
        *! C123853,1 MHM 01/15/2005 in case of inventory lock[Start]
        IF lcTrType='9'
          SCATT MEMVAR MEMO
          m.clocation   = lcBinLoc 
          INSERT INTO ('BININVJL') FROM MEMVAR
        ENDIF
        *! C123853,1 MHM 01/15/2005 [End]

        *--Update Uncomplete session Step.
        =lfUpdStep(lnTmpStp)  

        *--Call TraceKey global function.
        =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
      ENDIF  
      lnTmpStp = lnTmpStp + 1

      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfStyWarDy()
    ENDIF
  ENDSCAN
  USE
  *--Erase the temp. journal file.
  ERASE (gcWorkDir+lcTmpJour+'.DBF')
ELSE
      
  IF lfCheckUnCmp(lnTmpStp)       
    SELECT STYINVJL
    APPEND BLANK
    *B608092,1 TMI [Start] replace the command
    *                                     nStkVal   WITH -(lnOldSVal)   ,;
    * with the equivelant                 nStkVal   WITH nCost*nTotStk  ,;
    * So that we alyas sure that the multiplication result is correct
    *REPLACE cSession  WITH lcRISessn,;
            Style     WITH lcStyle,;
            cWareCode WITH lcWareCode,;
            cDyelot   WITH lcSDyelot,;
            dTrDate   WITH ldTrDate,;
            cTrType   WITH lcTrType,;
            cTrCode   WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),cSession,lcTrCode),;
            nCost     WITH lnWOldCst,;
            cIRType   WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,"R","I"),;
            cISession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,'',cSession),;
            cRSession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,cSession,''),;
            nStk1     WITH -(laOldstk[1]) ,;
            nStk2     WITH -(laOldstk[2]) ,;
            nStk3     WITH -(laOldstk[3]) ,;
            nStk4     WITH -(laOldstk[4]) ,;
            nStk5     WITH -(laOldstk[5]) ,;
            nStk6     WITH -(laOldstk[6]) ,;
            nStk7     WITH -(laOldstk[7]) ,;
            nStk8     WITH -(laOldstk[8]) ,;
            nTotStk   WITH -(laOldstk[9]) ,;
            nStkVal   WITH -(lnOldSVal) ,;
            lLockFlg  WITH (lcTrType='9') ,;
            Reference WITH IIF(cTrType = '2','Auto. zeroing of stock',lcRefer),;
            cAdjReason WITH lcAdjCdRsn,;
            cAdjAcct   WITH lcAdjAcct,;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal
     REPLACE cSession  WITH lcRISessn,;
            Style     WITH lcStyle,;
            cWareCode WITH lcWareCode,;
            cDyelot   WITH lcSDyelot,;
            dTrDate   WITH ldTrDate,;
            cTrType   WITH lcTrType,;
            cTrCode   WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),cSession,lcTrCode),;
            nCost     WITH lnWOldCst,;
            cIRType   WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,"R","I"),;
            cISession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,'',cSession),;
            cRSession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,cSession,''),;
            nStk1     WITH -(laOldstk[1]) ,;
            nStk2     WITH -(laOldstk[2]) ,;
            nStk3     WITH -(laOldstk[3]) ,;
            nStk4     WITH -(laOldstk[4]) ,;
            nStk5     WITH -(laOldstk[5]) ,;
            nStk6     WITH -(laOldstk[6]) ,;
            nStk7     WITH -(laOldstk[7]) ,;
            nStk8     WITH -(laOldstk[8]) ,;
            nTotStk   WITH -(laOldstk[9]) ,;
            nStkVal   WITH nCost*nTotStk ,;
            lLockFlg  WITH (lcTrType='9') ,;
            Reference WITH IIF(cTrType = '2','Auto. zeroing of stock',lcRefer),;
            cAdjReason WITH lcAdjCdRsn,;
            cAdjAcct   WITH lcAdjAcct,;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal
    *B608092,1 TMI [End  ] 
    REPLACE cAdjRef    WITH lcAdjRef
    REPLACE nTranCost WITH lnTranCost

    
    *-- Call global function to add audit fields info.
    =gfAdd_Info('STYINVJL')
    *! C123853,1 MHM 01/15/2005 in case of inventory lock[Start]
    IF lcTrType='9'
      SCATT MEMVAR MEMO
      m.clocation   = lcBinLoc 
      INSERT INTO ('BININVJL') FROM MEMVAR
    ENDIF
    *! C123853,1 MHM 01/15/2005 [End]

    *--Update Uncomplete session Step.
    =lfUpdStep(lnTmpStp)  

    *--Call TraceKey global function.

    =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')

    lnTmpStp = lnTmpStp + 1
  
    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfStyWarDy()
  ENDIF

ENDIF  

RETURN

*!***********************************************************************
*! Name      : lfAdjRec
*! Developer : Ahmed Amer
*! Date      : 11/22/98
*! Purpose   : Add Rec. record and Iss. record in StyInvJl.
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfAdjRec()
*!***********************************************************************

FUNCTION lfAdjRec

*-- Rec. with the old cost
      
IF lfCheckUnCmp(lnTmpStp)
  SELECT STYINVJL
  APPEND BLANK
  REPLACE cSession   WITH lcRISessn,;
          Style      WITH lcStyle,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcSDyelot,;
          dTrDate    WITH ldTrDate,;
          cTrType    WITH '1',;
          cTrCode    WITH lcRISessn,;
          nCost      WITH lnWOldCst,;
          cIRType    WITH "R",;
          nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[1],laAdjStk[1]),;
          nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[2],laAdjStk[2]),;
          nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[3],laAdjStk[3]),;
          nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[4],laAdjStk[4]),;
          nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[5],laAdjStk[5]),;
          nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[6],laAdjStk[6]),;
          nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[7],laAdjStk[7]),;
          nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[8],laAdjStk[8]),;
          nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
          nStkVal    WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-lnWStkVal,nTotStk * lnWOldCst),;
          Reference  WITH "Auto cost adj. " + cTrCode ,;
          cAdjReason WITH lcAdjCdRsn ,;
          cAdjAcct   WITH lcAdjAcct  ,;
          cRSession  WITH cSession   ,;
          nPrvSQty   WITH lnPrvQty   ,;
          nPrvSVal   WITH lnPrvVal 

  REPLACE cAdjRef    WITH lcAdjRef
  
  REPLACE nTranCost WITH lnTranCost

  *-- Call global function to add audit fields info.
  =gfAdd_Info('STYINVJL')

  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  

  *--Call TraceKey global function.
  =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
ENDIF
lnTmpStp = lnTmpStp + 1

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfStyWarDy()

*-- Iss. with the new cost

IF lfCheckUnCmp(lnTmpStp)   
  SELECT STYINVJL
  APPEND BLANK
  *B608092,1 TMI [Start] update nStkVal with nTotStk * nCost for more acuracy
  *REPLACE cSession   WITH lcRISessn,;
          Style      WITH lcStyle,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcSDyelot,;
          dTrDate    WITH ldTrDate,;
          cTrType    WITH '1',;
          cTrCode    WITH lcRISessn,;
          nCost      WITH lnNewCost,;
          cIRType    WITH "I",;
          nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[1],-laAdjStk[1]),;
          nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[2],-laAdjStk[2]),;
          nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[3],-laAdjStk[3]),;
          nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[4],-laAdjStk[4]),;
          nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[5],-laAdjStk[5]),;
          nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[6],-laAdjStk[6]),;
          nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[7],-laAdjStk[7]),;
          nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[8],-laAdjStk[8]),;
          nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
          nStkVal    WITH nTotStk * lnNewCost,;
          Reference  WITH "Auto cost adj. " + cTrCode ,;
          cAdjReason WITH lcAdjCdRsn ,;
          cAdjAcct   WITH lcAdjAcct  ,;
          cISession  WITH cSession   ,;
          nPrvSQty   WITH lnPrvQty   ,;
          nPrvSVal   WITH lnPrvVal
  
  REPLACE cSession   WITH lcRISessn,;
          Style      WITH lcStyle,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcSDyelot,;
          dTrDate    WITH ldTrDate,;
          cTrType    WITH '1',;
          cTrCode    WITH lcRISessn,;
          nCost      WITH lnNewCost,;
          cIRType    WITH "I",;
          nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[1],-laAdjStk[1]),;
          nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[2],-laAdjStk[2]),;
          nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[3],-laAdjStk[3]),;
          nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[4],-laAdjStk[4]),;
          nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[5],-laAdjStk[5]),;
          nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[6],-laAdjStk[6]),;
          nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[7],-laAdjStk[7]),;
          nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[8],-laAdjStk[8]),;
          nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
          nStkVal    WITH nTotStk * nCost,;
          Reference  WITH "Auto cost adj. " + cTrCode ,;
          cAdjReason WITH lcAdjCdRsn ,;
          cAdjAcct   WITH lcAdjAcct  ,;
          cISession  WITH cSession   ,;
          nPrvSQty   WITH lnPrvQty   ,;
          nPrvSVal   WITH lnPrvVal
  *B608092,1 TMI [End  ] 
  
  REPLACE cAdjRef    WITH lcAdjRef

  REPLACE nTranCost WITH lnTranCost

  *-- Call global function to add audit fields info.
  =gfAdd_Info('STYINVJL')

  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  

  *--Call TraceKey global function.
  =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
ENDIF
lnTmpStp = lnTmpStp + 1

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfStyWarDy()

*!*************************************************************
*! Name      : lfUpdGLDist()
*! Developer : Timour A. K.
*! Date      : 01/22/98
*! Purpose   : Update Temp G/L Distribution file.
*!*************************************************************
*:      [1] LinkCode  ,[2] Category Key ,[3] Amount sign
*:      [4] Tran Type ,[5] Tran No.     ,[6] Tran Date
*:      [7] Gl Year   ,[8] Gl Period    ,[9] Temp GlDist file name
*:      [10]Gl Account,[11]Currency Code,[12]CurrUnit,[13]Excg Rate.
*!*************************************************************
*! Call      : GLDIST
*!*************************************************************
*! Example   : =lfUpdGLDist()
*!*************************************************************
FUNCTION lfUpdGLDist

*-- llNegStkAd Showes if it is main record (Start)
*-- or it is adj. record because the stock is less than Zero
*-- AAMER 11/22/98
PARAMETERS llNegStkAd,llLockAdj

*-- llNegStkAd Showes if it is main record (End)
PRIVATE lnCurAlias

*--Donot update if no GL used.
IF ! llGLUsed
  RETURN
ENDIF

*-- This means it is Main Record (Start)

IF !llNegStkAd
*-- This means it is Main Record (End)

  *--Update Gl for Main inventory record for Isue or Receive.
  *- Receiving Trans.(+1,2,4,5,+6,7):    None
  *-  => +/-  lnAmount = Total Recv. Qty * New Recv. Cost     
  *- Issue Trans.(-1,-3,-6,-8,-2)     :  None
  *-  => +/-  lnAmount = Total Issue Qty * Issue Cost     
  FOR lnAln=1 TO ALEN(laGLInvAry,1)

    laGLInvAry[lnAln,5] = STYINVJL.cTrCode

    IF lfCheckUnCmp(lnTmpStp)
      
      lnGLEnAmount = STYINVJL.nStkVal * laGLInvAry[lnAln,3]

      DO GLDIST WITH laGLInvAry[lnAln,1],laGLInvAry[lnAln,2]  ,;
                     lnGLEnAmount,; 
                     laGLInvAry[lnAln,4],laGLInvAry[lnAln,5]  ,;
                     IIF(llLockAdj,laLockInfo[11],laGLInvAry[lnAln,6]),laGLInvAry[lnAln,7]  ,;
                     laGLInvAry[lnAln,8],laGLInvAry[lnAln,9]  ,;
                     laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],;
                     laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]

      DO CASE
        CASE &laGLInvAry[lnAln,9]..catg_Key = '006'
          lnCurAlias = SELECT(0)
          SELECT StyInvJl
          REPLACE cICAcnt WITH &laGLInvAry[lnAln,9]..GLAccount
          SELECT (lnCurAlias)

        *--update cadjact field in all cases 

        *--Updae cAdjAcct if it is empty in all catg_keys (Start) AAMER 04/13/99
        *--not if it is empty and catg_key = '007' 
        CASE &laGLInvAry[lnAln,9]..catg_Key = '013' OR EMPTY(StyInvJl.cAdjAcct)
        *--Updae cAdjAcct if it is empty in all catg_keys (End)
          lnCurAlias = SELECT(0)
          SELECT StyInvJl
          REPLACE cAdjAcct WITH &laGLInvAry[lnAln,9]..GLAccount
          SELECT (lnCurAlias)
      ENDCASE
    
      *--Update Uncomplete session Step.
      =lfUpdStep(lnTmpStp)  
    ENDIF
    lnTmpStp = lnTmpStp + 1
  ENDFOR

*-- This means it is Adj. Record (Start)
ELSE

  lcStyLink = IIF(EMPTY(StyDye.GL_Link),Style.Link_Code,StyDye.GL_Link)
   DO GLDIST WITH lcStyLink,'006',StyInvJl.nStkVal,'IA',;
                 StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],StyInvJl.DtrDate),laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],'','','',''                 
  lnCurAlias = SELECT(0)
  SELECT StyInvJl
  REPLACE cICAcnt WITH &laGLInvAry[1,9]..GLAccount
  SELECT (lnCurAlias)
  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  
  lnTmpStp = lnTmpStp + 1

  DO GLDIST WITH lcStyLink,'007',-StyInvJl.nStkVal,'IA',;
                 StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],StyInvJl.DtrDate),laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],StyInvJl.cAdjAcct,'','',''
                 
  IF EMPTY(StyInvJl.cAdjAcct)
    lnCurAlias = SELECT(0)
    SELECT StyInvJl
    REPLACE cAdjAcct WITH &laGLInvAry[1,9]..GLAccount
    SELECT (lnCurAlias)
  ENDIF
  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  
  lnTmpStp = lnTmpStp + 1

ENDIF
RETURN

************************************************************************


FUNCTION lfStyWarDy

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

PRIVATE llNew_Cost
llNew_Cost = .F. 
IF lcIRType = 'R' .AND. !(lcCostMeth $ 'FL') .AND. !(lcTrType = '4' .AND. lcCostMeth $ 'FL' ) ;
  .AND. ( !(lcTrType $ '29') .OR. (lcTrType $ '29' AND lfDoPhys('R')))
  llNew_Cost = .T.
ENDIF


lnValDiff=0
*** We changed the sequence of updating, we will update the stydye record first then the style record.

*--1 ) Update Stock and Avarege cost in Style Dyelot file Warehouse record.
IF lfCheckUnCmp(lnTmpStp)

  SELECT STYDYE
  =SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
  =RLOCK()
  
  lnPrvStk  = TotStk   && Old Stock
  lnStkVal  = nStkVal  && Old Stock Value
  lnAveCost = Ave_Cost && Old Average Cost
  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8
 
  
  IF StyInvJl.nTotStk > 0                && Receive transaction
    IF lnPrvStk < 0                      && The stock was negative
      
      IF StyInvJl.nTotStk + lnPrvStk < 0   && The stock still negative after receiving
        IF llNew_Cost
          lnStkVal = lnStkVal + StyInvJl.nTotStk * lnNewCost
        ELSE
          lnStkVal = lnStkVal +  StyInvJl.nTotStk * lnAveCost 
        ENDIF
      ELSE                               && Use the transaction cost if the stock will be > 0
        lnStkVal = (StyInvJl.nTotStk+lnPrvStk) * IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
      ENDIF
    ELSE
      lnStkVal = lnStkVal + (StyInvJl.nTotStk * IIF(llNew_Cost,lnNewCost,StyInvJl.nCost))
    ENDIF
  ELSE                                   && Issue transaction
    IF lnPrvStk = 0                      && If it is the 1st transaction for this style or the stock became 0.
      lnAveCost = IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
    ENDIF
    lnStkVal = TotStk * IIF(llNew_Cost,lnNewCost,IIF(lnprvstk=0,lnAveCost,lnstkval/lnprvstk))
  ENDIF
  IF TotStk = 0 AND StyInvJl.nTotStk > 0
     lnAveCost = IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
  ENDIF
  IF TotStk > 0
    lnAveCost = IIF(llNew_Cost,lnNewCost,lnStkVal/TotStk)
  ENDIF  
  
  IF lcIRType='I' AND !EMPTY(lcLastRSess)
    lnAveCost = IIF(TotStk=0,StyInvJl.nCost,lnStkVal/TotStk)
  ENDIF
  lnValDiff = lnStkVal - nStkVal
  *B608092,1 TMI [Start] stop updating the ave_cost when invoicing
  *REPLACE nStkVal  WITH IIF(TotStk=0,0,lnStkVal),;
          Ave_Cost WITH lnAveCost 
  REPLACE nStkVal  WITH IIF(TotStk=0,0,lnStkVal)
  IF lcTrType <> '3'
    REPLACE Ave_Cost WITH lnAveCost 
  ENDIF
  *B608092,1 TMI [End  ] 

  UNLOCK 
  *--Update Uncomplete session Step.
  =lfUpdStep(lnStarStep)  
  *--Call TraceKey global function.
  =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')
  lnPrvQty  = TotStk
  lnPrvVal  = nStkVal
  lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)

ENDIF 

lnTmpStp = lnTmpStp + 1

*--2 ) Update Stock and Avarege cost in Style file. ------------
IF lfCheckUnCmp(lnStarStep+1)
  SELECT STYLE
  =RLOCK()
  
  *B608092,1 TMI [Start] stop updating the ave_cost when invoicing
  *REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8,;
          nStkVal  WITH nStkVal + lnValDiff ,;
          Ave_Cost WITH IIF(TotStk = 0,Ave_Cost,ABS(nStkVal/TotStk))
  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8,;
          nStkVal  WITH nStkVal + lnValDiff
   IF lcTrType <> '3'
     REPLACE Ave_Cost WITH IIF(TotStk = 0,Ave_Cost,ABS(nStkVal/TotStk))
   ENDIF
  *B608092,1 TMI [End  ] 
  UNLOCK 

  *--Update Uncomplete session Step.
  =lfUpdStep(lnStarStep+1)  
  *--Call TraceKey global function.
  =gfTraceKey('STYLE',STYLE.Style,'M')

ENDIF 

lnTmpStp = lnTmpStp + 1

*--3 )  Update Stock in Style Dyelot file Dyelot record. --------
IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  IF lfCheckUnCmp(lnStarStep+2)
    SELECT STYDYE
    =RLOCK()
    REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
            Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
            Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
            Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
            Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
            Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
            Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
            Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
            TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8
    UNLOCK 
    *--Update Uncomplete session Step.
    =lfUpdStep(lnStarStep+2)  
    *--Call TraceKey global function.
    =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')

    lnPrvQty = TotStk
    lnPrvVal = TotStk * lnDyeCost

  ENDIF 
ENDIF

lnTmpStp = lnTmpStp + 1

SELECT(lnCurAlias)

************************************************************************

FUNCTION lfCalcStkVal

PRIVATE lnOrgJRec,lnRJRec,llDiffSess,lcOrgTran,lcJourTag,lnCurRec,lcOrgTrTyp
llDiffSess = .F.

lnStkVal = laAdjStk[9] * lnNewCost
*************************************

FUNCTION lfInvJour

SELECT SPACE(6) AS cSession,Style,cWareCode,cDyelot,dTrDate,'4' AS cTrType,cTrCode,;
       nCost,'R' AS cIRType,SPACE(6) AS cRSession,SPACE(6) AS cISession,LineNo    ,;
       -nStk1 AS nStk1,-nStk2 AS nStk2,-nStk3 AS nStk3,-nStk4 AS nStk4            ,;
       -nStk5 AS nStk5,-nStk6 AS nStk6,-nStk7 AS nStk7,-nStk8 AS nStk8            ,;
       -nTotStk AS nTotStk,-nStkVal AS nStkVal                ;
FROM  STYINVJL                                                ;
WHERE ctrcode +coprcode+clotno  +ctrtype+style  +cwarecode  = ;
      lcTrCode+SPACE(6)+SPACE(2)+'3'    +lcStyle+lcWareCode   ;
AND   LineNo = lnLineNo                   ;
AND   Dyelot = lcSDyelot                  ;
ORDER BY Style,cWareCode,cDyelot,LineNo   ;
INTO  DBF (gcWorkDir+lcInvJour)

***************************************

FUNCTION lfDoPhys
PARAMETERS lcRI
PRIVATE lcRI
lnRet = .F.


DO CASE
  CASE lcRI = 'I'
    *-- Issue if the old stock value or the old stock qty doen't equal zero 
    *-- or the cost value has changed or the balance has changed.
    lnRet = lnOldSVal   # 0 OR ;
            laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
            laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR ;
            lnWOldCst   # lnNewCost   OR ;
            laOldstk[9] # laAdjStk[9]

  CASE lcRI = 'R'
    *-- Receive only if the Issue record hasn't been issued or
    *-- the new balance is not zero or cost value has changed
    lnRet = !(lnOldSVal   # 0           OR ;
              laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
              laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR ;
              lnWOldCst   # lnNewCost   OR ;
              laOldstk[9] # laAdjStk[9]      ) OR ;
              laAdjStk[9] # 0                  OR ;
              lnWOldCst   # lnNewCost                      
ENDCASE

RETURN lnRet

*******************************************************************

FUNCTION lfLkAdjRec

PRIVATE lcJourTag,lnCurAlias,;
        lnTotQty,lnTotVal,lnTranVal,lnDiffere

lnCurAlias = SELECT(0)

SELECT StyInvJl
lcJourTag = ORDER('StyInvJl')
SET ORDER TO StyInvJl

lnTotQty = laAdjStk[9]
lnTotVal = laAdjStk[9] * lnNewCost
lnTranVal = 0
lnDiffere = 0
IF SEEK(lcStyle+lcWareCode,'StyInvJl')
  SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrDate)+cTrCode = ;
                  lcStyle+lcWareCode ;
            FOR   cDyelot = lcSDyelot AND !lLockFlg
    lnTranVal = IIF(cIRType='I',nTotStk*lnNewCost,nStkVal)
    lnDiffere = lnDiffere + (nStkVal - lnTranVal)
    lnTotVal  = lnTotVal  + lnTranVal
    lnTotQty  = lnTotQty  + nTotStk
    lnNewCost = IIF(lnTotQty=0,lnNewCost,lnTotVal/lnTotQty)
  ENDSCAN
  
  lnDiffere = - 1 * lnDiffere
  IF lnDiffere # 0 AND lfCheckUnCmp(lnTmpStp)
    SELECT STYINVJL
    APPEND BLANK
    REPLACE cSession   WITH lcRISessn,;
            Style      WITH lcStyle,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcSDyelot,;
            dTrDate    WITH laLockInfo[11],;
            cTrType    WITH lcTrType,;
            cTrCode    WITH IIF(EMPTY(lcTrCode),lcRISessn,lcTrCode),;
            cIRType    WITH IIF(lnDiffere<0,'I','R'),;
            nStkVal    WITH lnDiffere   ,;
            Reference  WITH 'Mark Down Adjustement Value',;
            cAdjReason WITH lcAdjCdRsn ,;
            cAdjAcct   WITH lcAdjAcct  ,;
            cISession  WITH IIF(lnDiffere<0,cSession,''),;
            cRSession  WITH IIF(lnDiffere>0,cSession,''),;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal,;
            cAdjRef    WITH lcAdjRef
    
    REPLACE nTranCost WITH lnTranCost

    =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
    lnTmpStp = lnTmpStp + 1
    =lfUpdGLDist(.F.,.T.)
    =lfStyWarDy()
  ENDIF
ENDIF

SET ORDER TO (lcJourTag) IN StyInvJl
SELECT (lnCurAlias)
 
*--mhmhmmhmhmhmhmhmhmhmhmhm


*!**************************************************************************
*! Name      : lfDLASSBIN
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Assign bin location to temp file in Inventory locking
*!**************************************************************************
*! Example   : = lfDLASSBIN()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLASSBIN

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  IF SEEK(m.STYLE+m.Color,lcTmpQuery) AND m.clocation = &lcTmpQuery..Bin
    m.BIN     = m.clocation
    SELECT (lcTmpQuery)
    REPLACE Stock WITH Stock + m.Stock;
            oStock WITH oStock + m.oStock
  ELSE
    m.BIN     = m.clocation
    INSERT INTO (lcTmpQuery) FROM MEMVAR
  ENDIF
  *--mhmh
  *--create Temp to store Bin Location Variable in
  PRIVATE lcFileNamA
  lcFileNamA = lcTmpQuery+"A"
  CREATE CURSOR (lcFileNamA) (cLocation C(10))
  APPEND BLANK
  *--mhmh

ELSE
  IF SEEK(m.STYLE+m.Color,lcTmpQuery)
    SELECT (lcTmpQuery)
    REPLACE Stock WITH Stock + m.Stock;
            oStock WITH oStock + m.oStock
  ELSE
    INSERT INTO (lcTmpQuery) FROM MEMVAR
  ENDIF
ENDIF

*!**************************************************************************
*! Name      : lfDLVLDBIN
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Return if called frfom custom Bin system
*!**************************************************************************
*! Example   : = lfINITVAR()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLSELREC

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  IF !llMatModul
    SCAN FOR &lcExpr AND IIF(llWareHus,CwareCode = ladata[8] ,.T.)
      SCAT MEMVAR MEMO
      IF !USED('WHBINLOC')
        =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
      ENDIF

      IF SEEK(STYDYE.style+STYDYE.cwarecode,'WHBINLOC')
        SELECT WHBINLOC
        SCAN REST WHILE style+cwarecode+clocation = STYDYE.style+STYDYE.cwarecode
          FOR lnContr = 1 TO 8
            lcContr = STR(lnContr,1)
            m.stk&lcContr = WHBINLOC.Qty&lcContr
          ENDFOR
          m.Totstk = WHBINLOC.TotQty
          m.StyDesc = Style.Desc1
          DO CASE
            CASE  lcCostMeth = 'S'  && Standard
              m.Cost    = STYLE.TotCost
              m.oCost   = STYLE.TotCost
            OTHERWISE       && 'A' Average
              m.Cost    = Ave_Cost
              m.oCost   = Ave_Cost
          ENDCASE
          m.ITEM    = ''
          m.STOCK   = WHBINLOC.TotQty
          m.oSTOCK  = WHBINLOC.TotQty
          m.nStkVal = (Ave_Cost*WHBINLOC.TotQty)
          m.BIN     = WHBINLOC.clocation
      
          IF lfDLChkBn()
            SELECT MDINVNTL
            SET ORDER TO MDINVNTLS
            WAIT WINDOW "Selecting Record... Please Wait" NOWAIT
            
            IF lfCheckSty(m.Style,m.Color) AND lfDLICChBN(m.Style,m.Color,cLocation)
              INSERT INTO (lcTmpQuery) FROM MEMVAR
              SELECT(lcTmpQuery)
              REPLACE nStkVal WITH m.nStkVal
            ENDIF
          ENDIF
        ENDSCAN  
      ENDIF
    ENDSCAN
  ENDIF
ELSE
  SCAN FOR &lcExpr AND IIF(llWareHus,CwareCode = ladata[8] ,.T.)

    SCAT MEMVAR MEMO
    IF llMatModul
      m.FabDesc = FABRIC.Desc
      m.CLRDESC = gfCodDes(COLOR,'COLOR')
    ELSE
      m.StyDesc = Style.Desc1
    ENDIF
    DO CASE
      CASE  lcCostMeth = 'S'  && Standard
        m.Cost    = IIF(llMatModul,FABRIC.CostUse,STYLE.TotCost)
        m.oCost   = IIF(llMatModul,FABRIC.CostUse,STYLE.TotCost)
      OTHERWISE       && 'A' Average
        m.Cost    = IIF(llMAtModul,nFAve_Cost,Ave_Cost)
        m.oCost   = IIF(llMAtModul,nFAve_Cost,Ave_Cost)
    ENDCASE
    m.ITEM    = IIF(llMatModul,Fabric,'')
    m.STOCK   = IIF(llMatModul,OnHand,TotStk)
    m.oSTOCK  = IIF(llMatModul,OnHand,TotStk)
    m.nStkVal = nStkVal
    IF lfChekBin()
      SELECT MDINVNTL
      SET ORDER TO MDINVNTLS
      WAIT WINDOW "Selecting Record... Please Wait" NOWAIT
      =lfAddLTemp(IIF(llMatModul,PADR(m.ITEM,19),m.Style),m.Color)
    ENDIF
  ENDSCAN

ENDIF

*!*************************************************************
*! Name      : lfDLChkBn
*! Developer : Mohamed Shokry - (MHM)
*! Date      : 01/20/2005
*! Purpose   : Functio to Check bin.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfDLChkBn()
*!*************************************************************
*! C123853,1 MHM 01/15/2005
FUNCTION lfDLChkBn

PRIVATE llToRet

llToRet = .T.
IF llLoc AND !EMPTY(laRpTarget)
  IF ASCAN(laRpTarget,ALLTRIM(WHBINLOC.clocation)) <> 0
    llToRet = .T.
  ELSE
    llToRet = .F.
  ENDIF
ENDIF

RETURN(llToRet)
*!*************************************************************
*! Name      : lfDLICChBN
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Functio to Check bin.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfDLICChBN()
*!*************************************************************
*! C123853,1 MHM 01/15/2005
FUNCTION  lfDLICChBN
PARAMETER lcItem,lcColor,lcBinLoc

IF SEEK(lcItem+lcColor,lcTmpQuery)
  SCAN REST WHILE Style+Color = lcItem+lcColor
    IF BIN = lcBinLoc
      RETURN .F.
    ENDIF
  ENDSCAN
  RETURN .T.
ELSE
  RETURN .T.
ENDIF

*!**************************************************************************
*! Name      : lfDLVLDBIN
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Return if called frfom custom Bin system
*!**************************************************************************
*! Example   : = lfINITVAR()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLBRWBIN

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  lcBrfield1 = [Style      :H = lcStyPic    ,] +; 
               [STYDESC:40 :H = 'Desc.'     ,] +;
               [Bin        :H = 'Bin ' ,] +;
               [OStock     :H = 'Old Stock' ,] +;
               [OCost      :H = 'Old Cost'  ,] +;
               [Stock      :H = 'New Stock' ,] +;
               [Cost       :H = 'New Cost'   ]
ENDIF
*!**************************************************************************
*! Name      : lfDLSELTMP
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/15/2005
*! Purpose   : Return if called frfom custom Bin system
*!**************************************************************************
*! Example   : = lfDLSELTMP()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLSELTMP

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
   lcLocBin = &lcTmpQuery..Bin                         
  SELECT * FROM (lcBatLin) WHERE Style = lcSty  ;
                             AND cLocation = lcLocBin ;
                             AND IIF(llDyelot AND lfStyDye(lcSty),!EMPTY(&lcBatLin..DYELOT),.T.) INTO DBF (lcDetLin)

  FOR I = 1 TO 8
    Z = STR (I,1)
    lnStk&Z = Stk&Z
  ENDFOR
  lnMCost   = COST
  lnOldCost = OldCOST
  lcReason  = cReason
  lcStyDesc = IIF(SEEK(lcSty,'Style'),Style.Desc1,'')
  lnNewTot  = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
ELSE
    SELECT * FROM (lcBatLin) WHERE Style = lcSty  ;
                               AND IIF(llDyelot AND lfStyDye(lcSty),!EMPTY(&lcBatLin..DYELOT),.T.) INTO DBF (lcDetLin)
ENDIF

*!**************************************************************************
*! Name      : lfDLDELALL
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Return if called frfom custom Bin system
*!**************************************************************************
*! Example   : = lfINITVAR()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLDELALL

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  DELETE ALL FOR cbattype+cLkBatch+style+color+DYELOT =;
                 lcKey+laData[2]+&lcDetLin..Style+&lcDetLin..Color;
                 AND &lcBatLin..clocation = &lcDetLin..clocation;
                 AND IIF(llDyelot AND lfStyDye(&lcDetLin..Style,&lcDetLin..Color),!EMPTY(&lcBatLin..DYELOT),.T.)
ELSE
  DELETE ALL FOR cbattype+cLkBatch+style+color+DYELOT =;
                 lcKey+laData[2]+&lcDetLin..Style+&lcDetLin..Color;
                 AND IIF(llDyelot AND lfStyDye(&lcDetLin..Style,&lcDetLin..Color),!EMPTY(&lcBatLin..DYELOT),.T.)

ENDIF

*!**************************************************************************
*! Name      : lfDLVLDBIN
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Return if called frfom custom Bin system
*!**************************************************************************
*! Example   : = lfINITVAR()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLPSTLCK
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin
  lcBinLoc = MDINVNTL.clocation  
  PRIVATE lcFileNama
  lcFileNamA = lcTmpQuery+"A"
  REPLACE &lcFileNamA..cLocation WITH MDINVNTL.clocation  
  
  SCAN REST WHILE cbattype+cLkBatch+style+color+DYELOT =;
                  lcPType +lcPBatch+lcPStyle+lcPColor+lcDye1;
            AND clocation  = lcBinLoc ;
            FOR   ((OldTotStk<>0 OR OLDSTK1<>0 OR OLDSTK2<>0 OR ;
                                    OLDSTK3<>0 OR OLDSTK4<>0 OR ;
                                    OLDSTK5<>0 OR OLDSTK6<>0 OR ;
                                    OLDSTK7<>0 OR OLDSTK8<>0)   ;
                                    OR TotStk <> 0 )

    WAIT WINDOW "Posting Batch# " + MDINVNTH.cLkBatch + ;
                                    IIF(llMatModule,'  Item   - Color \ ' + PADR(STYLE,7)+'-'+COLOR ,;
                                    lcStyPic +[\ ]+Style)  NOWAIT
    lnCountLn = lnCountLn + 1
    =gfThermo(lnTotRec,lnCountLn,'Posting')
    
    *----Start Calculate Stk for posting
    lcAdjReason = MDINVNTL.cAdjReason
    IF !llMatModule
      FOR lnIndex = 1 TO 8
        lcSub = STR(lnIndex,1)
        laAdjust[lnIndex] = laAdjust[lnIndex] + MDINVNTL.Stk&lcSub
        laOldStk[lnIndex] = laOldStk[lnIndex] + MDINVNTL.OldStk&lcSub
      ENDFOR
    ENDIF
    laAdjust[9]  = laAdjust[9] + MDINVNTL.TOTSTK
    laAdjust[10] = MDINVNTL.COST
    laOldStk[9]  = laOldStk[9] + MDINVNTL.OldTOTSTK
  ENDSCAN

ELSE
  SCAN REST WHILE cbattype+cLkBatch+style+color+DYELOT =;
                  lcPType +lcPBatch+lcPStyle+lcPColor+lcDye1;
            FOR   ((OldTotStk<>0 OR OLDSTK1<>0 OR OLDSTK2<>0 OR ;
                                    OLDSTK3<>0 OR OLDSTK4<>0 OR ;
                                    OLDSTK5<>0 OR OLDSTK6<>0 OR ;
                                    OLDSTK7<>0 OR OLDSTK8<>0)   ;
                                    OR TotStk <> 0 )
    WAIT WINDOW "Posting Batch# " + MDINVNTH.cLkBatch + ;
                                    IIF(llMatModule,'  Item   - Color \ ' + PADR(STYLE,7)+'-'+COLOR ,;
                                    lcStyPic +[\ ]+Style)  NOWAIT
    lnCountLn = lnCountLn + 1
    =gfThermo(lnTotRec,lnCountLn,'Posting')
    
    *----Start Calculate Stk for posting
    lcAdjReason = MDINVNTL.cAdjReason
    IF !llMatModule
      FOR lnIndex = 1 TO 8
        lcSub = STR(lnIndex,1)
        laAdjust[lnIndex] = laAdjust[lnIndex] + MDINVNTL.Stk&lcSub
        laOldStk[lnIndex] = laOldStk[lnIndex] + MDINVNTL.OldStk&lcSub
      ENDFOR
    ENDIF
    laAdjust[9]  = laAdjust[9] + MDINVNTL.TOTSTK
    laAdjust[10] = MDINVNTL.COST
    laOldStk[9]  = laOldStk[9] + MDINVNTL.OldTOTSTK
  ENDSCAN

ENDIF

*!**************************************************************************
*! Name      : lfDLTMPBAT
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : Return if called from custom Bin system
*!**************************************************************************
*! Example   : = lfDLTMPBAT()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*C037816,1 MHM 04/06/2004 
FUNCTION lfDLTMPBAT
llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF llUseBin

SELECT(lcTmpQuery)
GOTO TOP
  SCAN
    SCAT MEMVAR MEMO
    SELECT IIF(llMatModul,'FABDYE','STYDYE')
    REPLACE dLlokDate WITH ladata[5]
    m.Style     = IIF(llMatModul,m.ITEM,m.Style)
    m.Creason   = IIF(llMatModul,'MATERIAL LOCK INVENTORY     ' ,;
                                 'STYLE LOCK INVENTORY     ')
    m.cWareCode = ladata[8]

    lnAlias = SELECT(0)
    SELECT STYDYE
    =SEEK(m.Style+IIF(EMPTY(m.Color),'',m.Color)+;
               IIF(llWareHus,ladata[8],''))
    DO CASE
      CASE  lcCostMeth = 'S'  && Standard
        m.Cost    = STYLE.TotCost
        m.OldCost = STYLE.TotCost
     OTHERWISE       && 'A' Average
        m.Cost    = STYDYE.Ave_Cost
        m.OldCost = STYDYE.Ave_Cost
    ENDCASE

    SCAN REST WHILE style+cwarecode+dyelot = m.Style+IIF(llWareHus,ladata[8],'')
      *Nader
      *IF SEEK(STYDYE.style+STYDYE.cwarecode,'WHBINLOC')
      IF SEEK(STYDYE.style+STYDYE.cwarecode+m.Bin,'WHBINLOC')
      *Nader
        SELECT WHBINLOC
        SCAN REST WHILE style+cwarecode+clocation = STYDYE.style+STYDYE.cwarecode+m.Bin
          *Nader
          *IF WHBINLOC.clocation <> m.Bin
          *  IF !ASCAN(laRpTarget,ALLTRIM(WHBINLOC.clocation)) <> 0
          *    LOOP
          *  ENDIF
          *ENDIF
          *Nader
          FOR lnInd = 1 TO 8
            Index = STR(lnInd,1)
            m.OldStk&Index = WHBINLOC.Qty&Index
            m.Stk&Index    = WHBINLOC.Qty&Index
          ENDFOR
          m.OldTotStk = WHBINLOC.TotQty
          m.TotStk    = WHBINLOC.TotQty
          m.Dyelot    = STYDYE.Dyelot
          m.Scale     = Style.Scale
          m.clocation = &lcTmpQuery..Bin
          INSERT INTO (lcBatLin) FROM MEMVAR
        ENDSCAN  
      ENDIF
    ENDSCAN
  ENDSCAN
ELSE
  SELECT(lcTmpQuery)
  GOTO TOP
  SCAN
    SCAT MEMVAR MEMO
    SELECT IIF(llMatModul,'FABDYE','STYDYE')
    REPLACE dLlokDate WITH ladata[5]
    m.Style     = IIF(llMatModul,m.ITEM,m.Style)
    m.Creason   = IIF(llMatModul,'MATERIAL LOCK INVENTORY     ' ,;
                               'STYLE LOCK INVENTORY     ')
    m.cWareCode = ladata[8]
    IF llMatModul
    *--- Parameters : 1-Item ,2-Color ,3-MultiMare
      =lfAddLin(SUBSTR(m.ITEM,1,7),m.Color)
    ELSE
      =lfAddLin(m.style,SPACE(06))
    ENDIF
  ENDSCAN

ENDIF

*!*************************************************************
*! Name      : lfUPDWHBIN
*! Developer : Mohamed Shokry - (MHM)
*! Date      : 01/15/2005
*! Purpose   : LOCK THE INVENTORY For Both Multi and 
*!           : Single Warehouse.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lfUPDWHBIN
*!*************************************************************
*! C123853,1 MHM 01/15/2005
FUNCTION lfUPDWHBIN
PARAMETER lcbinloc

SELECT WHBINLOC
lcOldOrder = ORDER()
SET ORDER TO Whbinloc

SELECT (lcBatLin)
SCAN FOR &lcBatLin..clocation = lcbinloc
  =SEEK(&lcBatLin..cwarecode + &lcBatLin..clocation+&lcBatLin..Style,'WHBINLOC') 
  SELECT WHBINLOC
  REPLACE Qty1       WITH MAX(&lcBatLin..stk1,0),;
          Qty2       WITH MAX(&lcBatLin..stk2,0),;
          Qty3       WITH MAX(&lcBatLin..stk3,0),;
          Qty4       WITH MAX(&lcBatLin..stk4,0),;
          Qty5       WITH MAX(&lcBatLin..stk5,0),;
          Qty6       WITH MAX(&lcBatLin..stk6,0),;
          Qty7       WITH MAX(&lcBatLin..stk7,0),;
          Qty8       WITH MAX(&lcBatLin..stk8,0),;
          TotQty     WITH MAX(&lcBatLin..Totstk,0)

ENDSCAN
SELECT WHBINLOC
SET ORDER TO &lcOldOrder 

SELECT MDINVNTL
*!**************************************************************************
*! Name      : lfAlStyBin
*! Developer : Nader NABIL (NNA)
*! Date      : 03/16/2005
*! Purpose   : Save Data to style allocation screen If Always Pick from one
*!           : Bin Location is Yes.
*!**************************************************************************
*! Example   : lfAlStyBin()
*!**************************************************************************
*! C126994,1 NADER 05/26/2005
*!**************************************************************************
FUNCTION lfAlStyBin
PRIVATE lnPicked , lnRemain, lcAsecend , llPickOne
STORE 0 TO lnPicked , lnRemain
STORE .F. TO llPickOne
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
IF !USED('WHSLOC')
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH') 
ENDIF
llPickOne = gfGetMemVar('M_PICKONE',gcAct_Comp)   && setting For Always Pick from one Bin Location Yes/No
SELECT WHBINLOC
SET RELATION TO
SET RELATION TO Whbinloc.cwarecode+Whbinloc.clocation+SPACE(19) INTO Whsloc ADDITIVE
IF SEEK(laData[1] + StyDye.cWareCode ,'WHBINLOC') 
  SELECT WHBINLOC
  lcAsecend = IIF(ORDHDR.cBlkPck='P','DESCENDING','')
  FOR lnCnt = 1 TO 8
    lcCnt = ALLT(STR(lnCnt,2))
    IF ORDLINE.Pik&lcCnt <> 0        
      IF llPickOne				&& Always Pick From one Bin Location / Yes.
        SELECT DISTINCT Whbinloc.Style, Whbinloc.cWarecode, Whbinloc.cLocation,;
		 			   (Whbinloc.Qty1-Whbinloc.Alo1) AS QTY1,(Whbinloc.Qty2-Whbinloc.Alo2) AS QTY2,;
					   (Whbinloc.Qty3-Whbinloc.Alo3) AS QTY3,(Whbinloc.Qty4-Whbinloc.Alo4) AS QTY4,;
					   (Whbinloc.Qty5-Whbinloc.Alo5) AS QTY5,(Whbinloc.Qty6-Whbinloc.Alo6) AS QTY6,;
					   (Whbinloc.Qty7-Whbinloc.Alo7) AS QTY7,(Whbinloc.Qty8-Whbinloc.Alo8) AS QTY8,;
					   (Whbinloc.TotQty-Whbinloc.TotAlo) AS TOTQTY,Whsloc.cblkpck;
                FROM  Whbinloc, Whsloc;
                WHERE Whsloc.clocation = Whbinloc.clocation AND Whsloc.cWarecode = Whbinloc.cWarecode ;
                      AND Whsloc.style = SPACE(19) AND Whbinloc.style = laData[1] ;
                ORDER BY Whsloc.cblkpck &lcAsecend, Whbinloc.qty&lcCnt DESCENDING INTO TABLE BINLOC.Dbf
        SELECT BINLOC
        lnRemain = ORDLINE.Pik&lcCnt 
        SCAN FOR BINLOC.QTY&lcCnt > 0
          SELECT WHBINLOC
          IF BINLOC.QTY&lcCnt >= lnRemain
            IF SEEK(STYLE.STYLE + BINLOC.CWARECODE+BINLOC.CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnRemain,0)               ,;
                      WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnRemain,0) 
              =lfUpdPkBin(lnRemain,lcCnt,lcPikTkT,OrdHdr.Order,OrdLine.LineNo)
              EXIT
            ENDIF
          ELSE
            IF SEEK(STYLE.STYLE + BINLOC.CWARECODE+BINLOC.CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt + BINLOC.Qty&lcCnt            ,;
                      WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +BINLOC.Qty&lcCnt
              lnRemain = lnRemain - BINLOC.Qty&lcCnt
              *--Updates Record in the PkBinLoc File
              =lfUpdPkBin(BINLOC.Qty&lcCnt,lcCnt,lcPikTkT,OrdHdr.Order,OrdLine.LineNo)
            ENDIF
          ENDIF
        ENDSCAN
        SELECT WHBINLOC
      ELSE						&& Always Pick From one Bin Location / No.
       SELECT DISTINCT Whbinloc.Style, Whbinloc.cWarecode, Whbinloc.cLocation,;
					  (Whbinloc.Qty1-Whbinloc.Alo1) AS QTY1,(Whbinloc.Qty2-Whbinloc.Alo2) AS QTY2,;
					  (Whbinloc.Qty3-Whbinloc.Alo3) AS QTY3,(Whbinloc.Qty4-Whbinloc.Alo4) AS QTY4,;
					  (Whbinloc.Qty5-Whbinloc.Alo5) AS QTY5,(Whbinloc.Qty6-Whbinloc.Alo6) AS QTY6,;
					  (Whbinloc.Qty7-Whbinloc.Alo7) AS QTY7,(Whbinloc.Qty8-Whbinloc.Alo8) AS QTY8,;
					  (Whbinloc.TotQty-Whbinloc.TotAlo) AS TOTQTY,Whsloc.cblkpck;
                FROM  Whbinloc, Whsloc;
                WHERE Whsloc.clocation = Whbinloc.clocation;
                      AND (Whsloc.cwarecode = WHBINLOC.CWARECODE;
                      AND Whsloc.style = SPACE(19) AND Whbinloc.style = laData[1]);
                ORDER BY Whsloc.cblkpck &lcAsecend;
                INTO  TABLE BINLOC.Dbf
        SELECT BINLOC
        lnRemain = ORDLINE.Pik&lcCnt 
        SCAN FOR BINLOC.QTY&lcCnt > 0
          SELECT WHBINLOC
          IF BINLOC.QTY&lcCnt >= lnRemain
            IF SEEK(STYLE.STYLE + BINLOC.CWARECODE+BINLOC.CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnRemain,0) ,;
                      WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnRemain,0) 
              =lfUpdPkBin(lnRemain,lcCnt,lcPikTkT,OrdHdr.Order,OrdLine.LineNo)
              EXIT
            ENDIF
          ELSE
            IF SEEK(STYLE.STYLE + BINLOC.CWARECODE+BINLOC.CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt + BINLOC.Qty&lcCnt            ,;
                      WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +BINLOC.Qty&lcCnt
              lnRemain = lnRemain - BINLOC.Qty&lcCnt
              *--Updates Record in the PkBinLoc File
              =lfUpdPkBin(BINLOC.Qty&lcCnt,lcCnt,lcPikTkT,OrdHdr.Order,OrdLine.LineNo)
            ENDIF
          ENDIF
        ENDSCAN
        SELECT WHBINLOC
      ENDIF 
    ENDIF
    lnPicked = 0
    lnRemain = 0
  ENDFOR
ENDIF
SELECT WHBINLOC
FLUSH
lclocPktkt = IIF(!EMPTY(lcPikTkT),lcPikTkT,OrdLine.PikTkT)
=lfChkPktk("ORDLINE",lclocPktkt)
*--End of Function lfAlStyBin
*!**************************************************************************
*! Name      : lfAlOrdBin
*! Developer : Nader NABIL (NNA)
*! Date      : 03/16/2005
*! Purpose   : Save Data to Order allocation screen If Always Pick from one
*!           : Bin Location is Yes.
*!**************************************************************************
*! Example   : lfAlOrdBin()
*!**************************************************************************
*! C126994,1 NADER 05/26/2005
*!**************************************************************************
FUNCTION lfAlOrdBin
PRIVATE lnPicked , lnRemain, lcAsecend , llPickOne , lcStyle,lcBINLOC,lcSortBin
STORE '' TO lcBINLOC,lcSortBin
STORE 0 TO lnPicked , lnRemain
STORE .F. TO llPickOne
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
IF !USED('WHSLOC')
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH') 
ENDIF
IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINLIN','SH') 
ELSE
  IF ORDER('PKBINLOC')<>'PKBINLIN'
    SET ORDER TO TAG PKBINLIN IN PKBINLOC
  ENDIF
ENDIF

llPickOne = gfGetMemVar('M_PICKONE',gcAct_Comp)   && setting For Always Pick from one Bin Location Yes/No
SELECT WHBINLOC
SET RELATION TO
SET RELATION TO Whbinloc.cwarecode+Whbinloc.clocation+SPACE(19) INTO Whsloc ADDITIVE
lcStyle = Style.Style
IF SEEK(lcStyle + StyDye.cWareCode ,'WHBINLOC') 
  IF SEEK(IIF(!EMPTY(ALLTRIM(&lc_TmpOrdL..PikTkt)),&lc_TmpOrdL..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),;
     PIKTKT.PIKTKT,ORDLINE.PIKTKT))+&lc_TmpOrdL..cWareCode,'PKBINLOC') 
    SELECT PKBINLOC
    SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laPikQty FOR PikTkt = ;
        IIF(!EMPTY(ALLTRIM(&lc_TmpOrdL..PikTkt)),&lc_TmpOrdL..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),;
        PIKTKT.PIKTKT,ORDLINE.PIKTKT)) AND cWarecode = &lc_TmpOrdL..cWareCode AND Style = lcStyle AND LINENO = &lc_TmpOrdL..LineNo

    *---MHM2006
    *IF (&lc_TmpOrdL..TOTPIK - laPikQty[9]) < 0   	&& if user Decreased the picked qty
    *IF (&lc_TmpOrdL..TOTPIK - laPikQty[9]) < 0  OR (&lc_TmpOrdL..TOTPIK - laPikQty[9]) = 0 	&& if user Decreased the picked qty
    IF &lc_TmpOrdL..TOTPIK <> laPikQty[9]
    *---MHM2006
      = SEEK(IIF(!EMPTY(ALLTRIM(&lc_TmpOrdL..PikTkt)),&lc_TmpOrdL..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),;
             PIKTKT.PIKTKT,ORDLINE.PIKTKT))+&lc_TmpOrdL..cWareCode,'PKBINLOC') 
      SCAN REST WHILE piktkt+cwarecode+clocation+style+STR(LINENO,6) = ;
           IIF(!EMPTY(ALLTRIM(&lc_TmpOrdL..PikTkt)),&lc_TmpOrdL..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),;
           PIKTKT.PIKTKT,ORDLINE.PIKTKT))+&lc_TmpOrdL..cWareCode FOR STYLE = WHBINLOC.Style AND LINENO = &lc_TmpOrdL..LineNo
        IF SEEK(Style+cWarecode+cLocation ,'WHBINLOC')
          SELECT WHBINLOC
          FOR lnPkCount =1 To 8
            lcPkCount = ALLT(STR(lnPkCount,2))
            REPLACE WHBINLOC.Alo&lcPkCount  WITH MAX(WHBINLOC.Alo&lcPkCount - PKBINLOC.Qty&lcPkCount,0)
          ENDFOR
            REPLACE WHBINLOC.TOTALO WITH MAX(WHBINLOC.TOTALO - PKBINLOC.TOTQTY,0)
        ENDIF    
      ENDSCAN  
      STORE 0 TO laPikQty
      SELECT PKBINLOC
      LOCATE
      DELETE ALL FOR PIKTKT = IIF(!EMPTY(ALLTRIM(&lc_TmpOrdL..PikTkt)),&lc_TmpOrdL..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),;
            PIKTKT.PIKTKT,ORDLINE.PIKTKT)) AND cWareCode = &lc_TmpOrdL..cWareCode ;
           AND STYLE = WHBINLOC.Style AND LINENO = &lc_TmpOrdL..LineNo
    ENDIF
  ENDIF  
  SELECT WHBINLOC
  SET RELATION TO
  SET RELATION TO WHBINLOC.CWARECODE+WHBINLOC.CLOCATION+SPACE(19) INTO WHSLOC ADDITIVE

  *-- Select statment to get style's records from Whbinloc file and sorted by CBlkPck Field (Depend on OrdHdr.cBlkPck)
  *-- and sorted by Qty(X) Depend on the current Size and if option (Always Pick from one Bin Location) is Set To Yes or No
  *-- lcAsecend -->Hold 'DESCENDING' if ORDHDR.CBLKPCK='P'(PICK) OR hold 'ASCENDING' if ORDHDR.CBLKPCK='B'(BULK)
  *-- I save the Result in the (lcBinLoc) then select records that have Qtys. and match the process size .
  lcAsecend = IIF(ORDHDR.cBlkPck='P','DESCENDING','')
  SELECT DISTINCT Whbinloc.Style, Whbinloc.cWarecode, Whbinloc.cLocation,;
	   (Whbinloc.Qty1-Whbinloc.Alo1) AS QTY1,(Whbinloc.Qty2-Whbinloc.Alo2) AS QTY2,;
	   (Whbinloc.Qty3-Whbinloc.Alo3) AS QTY3,(Whbinloc.Qty4-Whbinloc.Alo4) AS QTY4,;
       (Whbinloc.Qty5-Whbinloc.Alo5) AS QTY5,(Whbinloc.Qty6-Whbinloc.Alo6) AS QTY6,;
       (Whbinloc.Qty7-Whbinloc.Alo7) AS QTY7,(Whbinloc.Qty8-Whbinloc.Alo8) AS QTY8,;
       (Whbinloc.TotQty-Whbinloc.TotAlo) AS TOTQTY,Whsloc.cblkpck;
       FROM  Whbinloc, Whsloc;
       WHERE Whsloc.clocation = Whbinloc.clocation AND Whsloc.cWarecode = Whbinloc.cWarecode ;
             AND Whsloc.style = SPACE(19) AND Whbinloc.style = lcStyle ;
       INTO CURSOR lcBINLOC

  FOR lnCnt = 1 TO 8
    lcCnt = ALLTRIM(STR(lnCnt))
    *-- if there are remain qty. not picked
    IF (&lc_TmpOrdL..Pik&lcCnt - laPikQty[lnCnt]) > 0
      IF llPickOne				&& Always Pick From one Bin Location / Yes.
        *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field and Qty. for the current Size.
        SELECT Style,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
              FROM  lcBinLoc ORDER BY cblkpck &lcAsecend, qty&lcCnt DESCENDING INTO CURSOR lcSortBin
      ELSE                        && Always Pick From one Bin Location / No.
        *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field only for the current Size.
        SELECT Style,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
              FROM  lcBinLoc ORDER BY cblkpck &lcAsecend INTO CURSOR lcSortBin
      ENDIF
      SELECT lcSortBin
      lnRemain = &lc_TmpOrdL..Pik&lcCnt - laPikQty[lnCnt]
      SCAN FOR lcSortBin.QTY&lcCnt > 0
        SELECT WHBINLOC
        IF lcSortBin.QTY&lcCnt >= lnRemain
          IF SEEK(lcStyle + lcSortBin.CWARECODE+lcSortBin.CLOCATION)
            REPLACE WHBINLOC.ALO&lcCnt WITH MAX(WHBINLOC.ALO&lcCnt+lnRemain,0)               ,;
                    WHBINLOC.TOTALO    WITH MAX(WHBINLOC.TOTALO +lnRemain,0) 
            =lfUpdPkBin(lnRemain,lcCnt,m.PikTkT,&lc_TmpOrdL..Order,&lc_TmpOrdL..LineNo)
            EXIT
          ENDIF
        ELSE
          IF SEEK(lcStyle + lcSortBin.CWARECODE+lcSortBin.CLOCATION)
            REPLACE WHBINLOC.ALO&lcCnt WITH WHBINLOC.ALO&lcCnt + lcSortBin.QTY&lcCnt            ,;
                    WHBINLOC.TOTALO    WITH WHBINLOC.TOTALO +lcSortBin.QTY&lcCnt
            lnRemain = lnRemain - lcSortBin.Qty&lcCnt
            *--Updates Record in the PkBinLoc File
            =lfUpdPkBin(lcSortBin.Qty&lcCnt,lcCnt,m.PikTkT,&lc_TmpOrdL..Order,&lc_TmpOrdL..LineNo)
          ENDIF
        ENDIF
      ENDSCAN
      SELECT WHBINLOC
    ENDIF
    lnPicked = 0
    lnRemain = 0
  ENDFOR
ENDIF
SELECT WHBINLOC
FLUSH
lclocPktkt = IIF(!EMPTY(m.piktkt),m.piktkt,OrdLine.PikTkT)
=lfChkPktk(lc_TmpOrdL,lclocPktkt)

*--End of Function lfAlOrdBin
*!**************************************************************************
*! Name      : lfAlAutBin
*! Developer : Nader NABIL (NNA)
*! Date      : 03/16/2005
*! Purpose   : Save Data to Automatic allocation screen If Always Pick from one
*!           : Bin Location is Yes.
*!**************************************************************************
*! Example   : lfAlAutBin()
*!**************************************************************************
*! C126994,1 NADER 05/26/2005
*!**************************************************************************
FUNCTION lfAlAutBin

PRIVATE lnPicked , lnRemain, lcAsecend , llPickOne , lcStyle
STORE 0 TO lnPicked , lnRemain
STORE .F. TO llPickOne

lnAlias    = SELECT()

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINLIN','SH') 
ELSE
  IF ORDER('PKBINLOC')<>'PKBINLIN'
    SET ORDER TO TAG PKBINLIN IN PKBINLOC
  ENDIF
ENDIF
IF !USED('WHSLOC')
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH') 
ENDIF

llPickOne = gfGetMemVar('M_PICKONE',gcAct_Comp)   && setting For Always Pick from one Bin Location Yes/No
SELECT WHBINLOC
SET RELATION TO
SET RELATION TO Whbinloc.cwarecode+Whbinloc.clocation+SPACE(19) INTO Whsloc ADDITIVE
lcStyle = &lcTmpOrdLn..Style
IF SEEK(lcStyle + &lcTmpOrdLn..cWareCode ,'WHBINLOC') 
  IF SEEK(&lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode,'PKBINLOC') 
    SELECT PKBINLOC
    SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laPikQty FOR PikTkt = &lcTmpOrdLn..PikTkt AND;
        cWarecode = &lcTmpOrdLn..cWareCode AND Style = lcStyle AND LINENO = &lcTmpOrdLn..LineNo
    IF (&lcTmpOrdLn..TOTPIK - laPikQty[9]) < 0   	&& if user Decreased the picked qty
      = SEEK(&lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode,'PKBINLOC') 
      SCAN REST WHILE piktkt+cwarecode+clocation+style+STR(LINENO,6) = &lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode FOR ;
                STYLE = WHBINLOC.Style AND LINENO = &lcTmpOrdLn..LineNo
        IF SEEK(Style+cWarecode+cLocation ,'WHBINLOC')
          SELECT WHBINLOC
          FOR lnPkCount =1 To 8
            lcPkCount = ALLT(STR(lnPkCount,2))
            REPLACE WHBINLOC.Alo&lcPkCount  WITH MAX(WHBINLOC.Alo&lcPkCount - PKBINLOC.Qty&lcPkCount,0)
          ENDFOR
            REPLACE WHBINLOC.TOTALO WITH MAX(WHBINLOC.TOTALO - PKBINLOC.TOTQTY,0)
        ENDIF    
      ENDSCAN  
    ENDIF
    STORE 0 TO laPikQty
    SELECT PKBINLOC
    LOCATE
    DELETE ALL FOR PIKTKT = &lcTmpOrdLn..PikTkt AND cWareCode = &lcTmpOrdLn..cWareCode ;
           AND STYLE = WHBINLOC.Style AND LINENO = &lcTmpOrdLn..LineNo
  ENDIF  
  SELECT WHBINLOC
  lcAsecend = IIF(ORDHDR.cBlkPck='P','DESCENDING','')
  FOR lnCnt = 1 TO 8
    lcCnt = ALLT(STR(lnCnt,2))
    *!* if there are remain qty. not picked
    IF (&lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt]) > 0
      IF llPickOne				&& Always Pick From one Bin Location / Yes.
        SELECT DISTINCT Whbinloc.Style, Whbinloc.cWarecode, Whbinloc.cLocation,;
         			   (Whbinloc.Qty1-Whbinloc.Alo1) AS QTY1,(Whbinloc.Qty2-Whbinloc.Alo2) AS QTY2,;
            		   (Whbinloc.Qty3-Whbinloc.Alo3) AS QTY3,(Whbinloc.Qty4-Whbinloc.Alo4) AS QTY4,;
                       (Whbinloc.Qty5-Whbinloc.Alo5) AS QTY5,(Whbinloc.Qty6-Whbinloc.Alo6) AS QTY6,;
                       (Whbinloc.Qty7-Whbinloc.Alo7) AS QTY7,(Whbinloc.Qty8-Whbinloc.Alo8) AS QTY8,;
                       (Whbinloc.TotQty-Whbinloc.TotAlo) AS TOTQTY,Whsloc.cblkpck;
                FROM  Whbinloc, Whsloc;
                WHERE Whsloc.clocation = Whbinloc.clocation AND Whsloc.cWarecode = Whbinloc.cWarecode ;
                      AND Whsloc.style = SPACE(19) AND Whbinloc.style = lcStyle ;
                ORDER BY Whsloc.cblkpck &lcAsecend, Whbinloc.qty&lcCnt DESCENDING INTO TABLE BINLOC.Dbf
        SELECT BINLOC
        lnRemain = &lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt]
        SCAN FOR BINLOC.QTY&lcCnt > 0
          SELECT WHBINLOC
          IF BINLOC.QTY&lcCnt >= lnRemain
            IF SEEK(lcStyle + BINLOC.CWARECODE+BINLOC.CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnRemain,0)               ,;
                      WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnRemain,0) 
              =lfUpdPkBin(lnRemain,lcCnt,m.PikTkT,OrdHdr.Order,Ordline.LineNo)
              EXIT
            ENDIF
          ELSE
            IF SEEK(lcStyle + BINLOC.CWARECODE+BINLOC.CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt + BINLOC.Qty&lcCnt            ,;
                      WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +BINLOC.Qty&lcCnt
              lnRemain = lnRemain - BINLOC.Qty&lcCnt
              *--Updates Record in the PkBinLoc File
              =lfUpdPkBin(BINLOC.Qty&lcCnt,lcCnt,m.PikTkT,OrdHdr.Order,Ordline.LineNo)
            ENDIF
          ENDIF
        ENDSCAN
        SELECT WHBINLOC
      ELSE						&& Always Pick From one Bin Location / No.
        SELECT DISTINCT Whbinloc.Style, Whbinloc.cWarecode, Whbinloc.cLocation,;
                        (Whbinloc.Qty1-Whbinloc.Alo1) AS QTY1,(Whbinloc.Qty2-Whbinloc.Alo2) AS QTY2,;
                        (Whbinloc.Qty3-Whbinloc.Alo3) AS QTY3,(Whbinloc.Qty4-Whbinloc.Alo4) AS QTY4,;
                        (Whbinloc.Qty5-Whbinloc.Alo5) AS QTY5,(Whbinloc.Qty6-Whbinloc.Alo6) AS QTY6,;
                        (Whbinloc.Qty7-Whbinloc.Alo7) AS QTY7,(Whbinloc.Qty8-Whbinloc.Alo8) AS QTY8,;
                        (Whbinloc.TotQty-Whbinloc.TotAlo) AS TOTQTY,Whsloc.cblkpck;
                FROM  Whbinloc, Whsloc;
                WHERE Whsloc.clocation = Whbinloc.clocation;
                      AND (Whsloc.cwarecode = WHBINLOC.CWARECODE;
                      AND Whsloc.style = SPACE(19) AND Whbinloc.style = lcStyle);
                ORDER BY Whsloc.cblkpck &lcAsecend;
                INTO  TABLE BINLOC.Dbf
        SELECT BINLOC
        lnRemain = &lcTmpOrdLn..Pik&lcCnt 
        SCAN FOR BINLOC.QTY&lcCnt > 0
          SELECT WHBINLOC
          IF BINLOC.QTY&lcCnt >= lnRemain
            IF SEEK(lcStyle + BINLOC.CWARECODE+BINLOC.CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnRemain,0) ,;
                      WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnRemain,0) 
              =lfUpdPkBin(lnRemain,lcCnt,m.PikTkT,OrdHdr.Order,Ordline.LineNo)
             EXIT
            ENDIF
          ELSE
            IF SEEK(lcStyle + BINLOC.CWARECODE+BINLOC.CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt + BINLOC.Qty&lcCnt            ,;
                      WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +BINLOC.Qty&lcCnt
              lnRemain = lnRemain - BINLOC.Qty&lcCnt
              *--Updates Record in the PkBinLoc File
              =lfUpdPkBin(BINLOC.Qty&lcCnt,lcCnt,m.PikTkT,OrdHdr.Order,Ordline.LineNo)
            ENDIF
          ENDIF
        ENDSCAN
        SELECT WHBINLOC
      ENDIF 
    ENDIF
    lnPicked = 0
    lnRemain = 0
  ENDFOR
ENDIF
SELECT WHBINLOC
FLUSH
lclocPktkt = IIF(!EMPTY(m.piktkt),m.piktkt,OrdLine.PikTkT)
=lfChkPktk(lcTmpOrdLn,lclocPktkt)
*--End of Function lfAlAutBin.
*!**************************************************************************
*! Name      : lfUpdPkBin
*! Developer : Nader NABIL (NNA)
*! Date      : 03/16/2005
*! Purpose   : Update the PKBinLoc file with the piktkt file
*!**************************************************************************
*! Example   : lfUpdPkBin()
*!**************************************************************************
*! C126994,1 NADER 05/26/2005
*!**************************************************************************
FUNCTION lfUpdPkBin
PARAMETER lnQty,lcSize,lcPkTkt,lcOrder,lnLineNo
STORE '' TO lcClass
lnAlias    = SELECT()
IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINLIN','SH') 
ELSE
  IF ORDER('PKBINLOC')<>'PKBINLIN'
    SET ORDER TO TAG PKBINLIN IN PKBINLOC
  ENDIF
ENDIF
IF !SEEK(lcPkTkt+StyDye.cWareCode+WHBINLOC.cLocation+Style.Style+STR(lnLineNo,6) ,'PKBINLOC') AND lnQty > 0
  IF !EMPTY(STYLE.cPrimClss&lcSize)
    lcClass =  STYLE.cPrimClss&lcSize
  ELSE
    IF !EMPTY(STYLE.cSecClss&lcSize)
      lcClass =  STYLE.cSecClss&lcSize
    ELSE
      lcClass =  STYLE.cRemClss&lcSize
    ENDIF
  ENDIF
  =SEEK(StyDye.cWareCode+WHBINLOC.cLocation,'WHSLOC')
  SELECT PKBINLOC
  APPEND BLANK
  REPLACE STYLE              WITH WHBINLOC.Style            ,;
          CWARECODE          WITH WHBINLOC.cWareCode        ,;
          PIKTKT             WITH lcPkTkt                   ,;
          ORDER              WITH lcOrder                   ,;
          cFlatHang          WITH WHSLOC.cFlatHang          ,;
          clocation          WITH WHBINLOC.cLocation        ,;
          Qty&lcSize         WITH MAX(Qty&lcSize+lnQty,0)   ,;
          TotQty             WITH MAX(TotQty +lnQty,0)      ,;
          cBinClass&lcSize   WITH lcClass                   ,;
          LINENO             WITH lnLineNo
  =gfAdd_Info('PKBINLOC')
ELSE
  SELECT PKBINLOC
  REPLACE Qty&lcSize         WITH Qty&lcSize + lnQty        ,;
          TotQty             WITH TotQty + lnQty            ,;
          cBinClass&lcSize   WITH lcClass
ENDIF
SELECT(lnAlias)
*--End of Function lfUpdPkBin.

*!*************************************************************
*! Name      : lfChkPktk
*! Developer : Mohamed Shokry - (MHM)
*! Date      : 01/15/2005
*! Purpose   : Function to check 
*!           : Single Warehouse.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : ICINVLK.PRG
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lfUPDWHBIN
*!*************************************************************
*! mhm2005
FUNCTION lfChkPktk
PARAMETER lcTempFile,lcPiktkt

IF EMPTY(lcPiktkt)
  RETURN
ENDIF
PRIVATE lcOldOrd
DIMENSION laPkQty[9]
STORE 0 TO laPkQty

SELECT PKBINLOC
lcOldOrd = ORDER()
*--mhm126994 Set order to PKBINLIN
*SET ORDER TO PKBINPKT
SET ORDER TO PKBINLIN
*--mhm126994

IF !SEEK(lcPiktkt+&lcTempFile..cWareCode ,'PKBINLOC') 
  lcMsg2 = "No line updated to Bin Location Files Please Contact Aria Support"
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
ELSE
    FOR lnPkCount =1 To 8
      lcPkCount = ALLT(STR(lnPkCount,2))
      SELECT PKBINLOC
      =SEEK(lcPiktkt+&lcTempFile..cWareCode ,'PKBINLOC') 

      SCAN REST WHILE piktkt+cwarecode+clocation+style = lcPiktkt+&lcTempFile..cWareCode FOR Style  = &lcTempFile..Style

        *--mhm126994
        IF  LineNo <> &lcTempFile..LineNo
          LOOP
        ENDIF
        *--mhm126994
      
        laPkQty[lnPkCount] =laPkQty[lnPkCount]+ PKBINLOC.QTY&lcPkCount
      ENDSCAN  
    ENDFOR
    laPkQty[9]= laPkQty[1]+laPkQty[2]+laPkQty[3]+laPkQty[4]+laPkQty[5]+laPkQty[6]+laPkQty[7]+laPkQty[8]

    IF  &lcTempFile..TotPik <> laPkQty[9]
      lcMsg2 = "Piked Qty " + ALLTRIM(STR(&lcTempFile..TotPik)) +  " Not equal bin Piked Qty " + ALLTRIM(STR(laPkQty[9])) +;
               " for Style " + &lcTempFile..Style + " Piktkt " + lcPiktkt + " Please contact aria support "
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    ENDIF  
ENDIF

SELECT PKBINLOC
SET ORDER TO &lcOldOrd

*:**************************************************************************
*:* Name        : lfStandard
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/18/2004
*:* Purpose     : Check if this order is standard or entered via SOENORD custom screen
*                 if EMPLOYEE field is empty , it is standard
*:***************************************************************************
*C123851,3  TMI ( original entry *C123847,1 )

FUNCTION lfEntlOrd
PRIVATE lnSlct, lnRecno, llEntlOrd
llEntlOrd = .F.
lnSlct = SELECT()

SELECT &lcOrdLine
lnRecno = RECNO()

GO TOP
IF !EOF()
  llEntlOrd = !EMPTY(EMPLOYEE)

  IF BETWEEN(lnRecno,1,RECCOUNT())
    GOTO (lnRecno)
  ENDIF
  
ENDIF
  
SELECT (lnSlct)

RETURN llEntlOrd
*-- end of lfStandard.


*:**************************************************************************
*:* Name        : lfPriComDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/14/2005
*:* Purpose     : Valid function for complete date
*:***************************************************************************
*:* Called from : quickord.spx screen
*:***************************************************************************
FUNCTION lfPriComDt

*C127807,1  TMI [Start] Update prices if complete date is changed for PAN21
IF ASCAN(laEvntTrig,'PNPRICOM')<>0
  =gfDoTriger('SOORD','PNPRICOM')
ENDIF
*C127807,1  TMI [End  ] 
*-- end of lfPriComDt.

*:**************************************************************************
*:* Name        : lfOldValue
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/14/2005
*:* Purpose     : Save old value 
*:***************************************************************************
FUNCTION lfOldValue
*--mhm2005,
*lcOldValue = EVALUATE('m.' + SYS(18))
IF lcProgName = 'SOORD'
  lcOldValue = EVALUATE('m.' + SYS(18))
ELSE
  lcOldValue = EVALUATE(SYS(18))
ENDIF  
*--mhm2005
*-- end of lfOldValue.

*!*************************************************************
*! Name      : lfUSRFELDS
*! Developer : NADER NABIL - (NNA)
*! Date      : 06/26/2005
*! Purpose   : Function to update the Ordhdr.cBlkPck Field with 
*!           : the default value from the customer file
*!*************************************************************
*! Calls     : Procedures 
*!             Functions  
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lfUSRFELDS
*!*************************************************************
*!C126994,1
*!C128481,1
FUNCTION lfUSRFELDS
IF !EMPTY(laUsrFields)
  FOR J = 1 TO ALEN(laUsrFields,1)
    IF ALLTRIM(laUsrFields[J,1]) = "CBLKPCK"
      laUsrFields[J,6] = CUSTOMER.CBLKPCK
    ENDIF
    *C128481,1 NNA 09/12/2005 (Begin) Get the customer Default for OrdHdr.LCARTBC and make the default value for Lcharge=.T.
    IF ALLTRIM(laUsrFields[J,1]) = "LCARTBC"
      laUsrFields[J,6] = CUSTOMER.lCharge
    ENDIF
    IF ALLTRIM(laUsrFields[J,1]) = "LCHARGE"
      laUsrFields[J,6] = .T.
    ENDIF
    *C128481,1 NNA (End)
  ENDFOR
ENDIF
*!**************************************************************************
*!* Name        : LFPORECPAD
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 30/06/2005
*!* Purpose     : Add option menu to the receiving PO Screen.
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = LFPORECPAD()
*!***************************************************************************
*!126994,1
FUNCTION LFPORECPAD
*--check if the option pad is already defined on the sysmenu
PRIVATE llFound
STORE .F. TO llFound
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = 'Options'
    llfound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' ' SKIP FOR ;
         IIF(TYPE('lcProgName')='C' AND lcProgName = "POSTREC",.F.,.T.) OR (laScrMode[1]) OR (laScrMode[2]) 
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
  lnBarNo = 1
  *T20061016.0001 TMI [Start] 
  *DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Receive into Multiple Bins" SKIP FOR ;
             laScrMode[1] OR EMPTY(lcTCode) OR !gfGetMemvar('M_DLUSEBIN') 
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Receive into Multiple Bins" SKIP FOR ;
             laScrMode[1] OR EMPTY(lcTCode) OR EMPTY(gfGetMemvar('M_DLUSEBIN'))
  *T20061016.0001 TMI [End  ]              
  
  ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfDispScr in DAVMAIN
ELSE
  *-- Count Options BARS to add the new BAR at the end of the Popup
  lnBarNo = CNTBAR(LEFT('_OPTIONPOP',10)) + 1

  *-- Skip The New BAR if this is the Select mode or the PO field is Empty.
  ON PAD _OPTION OF _MSYSMENU ACTIVATE POPUP _OPTIONPOP
  *T20061016.0001 TMI [Start] 
  *DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Receive into Multiple Bins" SKIP FOR ;
             laScrMode[1] OR EMPTY(lcTCode) OR !gfGetMemvar('M_DLUSEBIN') 
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Receive into Multiple Bins" SKIP FOR ;
             laScrMode[1] OR EMPTY(lcTCode) OR EMPTY(gfGetMemvar('M_DLUSEBIN')) 
  *T20061016.0001 TMI [End  ]             
  ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfDispScr in DAVMAIN
ENDIF
RETURN
*-- End of Function LFPORECPAD.
*!*************************************************************************
*!* Name        : lfCrtBinLn
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/08/2005
*!* Purpose     : Create new cursor for the receive into Multiple Bins screen
*!***************************************************************************
*!* Called from : Postrec.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCrtBinLn()
*!***************************************************************************
*!126994,1
FUNCTION lfCrtBinLn
lcBinLine = lcTmpLine + 'A'
SELECT(lcTmpLine)
= AFIELDS(laFileStru)
CREATE CURSOR (lcBinLine) FROM ARRAY laFileStru

*B130409,1 NNA 11/22/2005 (Begin) Index on PO + Style to Deal with any Shipment with multi Orders
*INDEX ON STYLE TAG (lcBinLine)

*! B131115,1 MHM 02/19/2006 AddLine no to the index [Start]
*INDEX ON PO + STYLE TAG (lcBinLine)
INDEX ON PO + STYLE + STR(LINENO,6) TAG (lcBinLine)
*! B131115,1 MHM  [End]

*B130409,1 NNA (End)

*-- End of Function lfCrtBinLn.
*!**************************************************************************
*!* Name        : lfDispScr
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 30/06/2005
*!* Purpose     : 
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfDispScr()
*!***************************************************************************
*!126994,1
FUNCTION lfDispScr
DECLARE laShipQty[9],laInputQty[9],laBinLoc[8],laBrowArr[1],laReceived[9]
STORE "" TO lcMjrPct,lcStyMjr,lcColor,lcStyDesc,lcBinLine,lcSlcStyle,laBinLoc,lcSize1,;
           lcSize2,lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8,laBrowArr,lcSelBin,lcWareCode
STORE 0 TO lnClrLen,lnClrStPos,lnStyLen,lnStyStPos,lnScaLen,lnScaStPos,lnBalQty1,lnBalQty2,;
           lnBalQty3,lnBalQty4,lnBalQty5,lnBalQty6,lnBalQty7,lnBalQty8,lnBalQty9,laShipQty,;
           laInputQty,lnRecNo,lnOldValue,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,;
           lnQty8,lnQty9,lnMarker,lnLineno,LnPoNo
STORE .F. TO llNew ,llMove
= lfChkStrct()
lcDetTtl   = "Detail Receiving Lines"
lcOkKey    = (gcBmpHome+ "OK.BMP")
lcRemKey   = (gcBmpHome+ "Remove.BMP")
lcCanKey   = (gcBmpHome+ "Can.BMP")
lcExtKey   = (gcBmpHome+ "EXTKEY.BMP")
lcNewKey   = (gcBmpHome+ "NEW.BMP")
STORE " " TO POSHPBN
lcBinLine  = lcTmpLine + 'A'
SELECT(lcTmpLine)
lnRecNo = RECNO()
lcSlcStyle = &lcTmpLine..Style
lnLineNo   = &lcTmpLine..LineNo
lnPoNo     = &lcTmpLine..PO
lcWareCode = &lcTmpLine..cWareCode
lcWinTitle = "Receive into Multiple Bins For Purchase Order " + lnPoNo 
*--Get the Received Qty. as Cancel,Second and Damaged
*--mhmnna
*SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laReceived FOR STYLE = lcSlcStyle AND !(TRANCD $ '12')
SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laReceived FOR STYLE = lcSlcStyle AND LineNo =lnLineNo AND !(TRANCD $ '12')
*--mhmnna

IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpLine))
  GOTO lnRecNo IN (lcTmpLine)
ENDIF
*--get the Original Qty.
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laShipQty
*--get the Opened Qty after remove the already received qty as Cancel,Second and Damaged
FOR I = 1 TO 9
  laShipQty[I] = laShipQty[I] - laReceived[I]
ENDFOR

IF !USED(lcBinLine)
  SELECT(lcTmpLine)
  LOCATE
  SCATTER MEMVAR MEMO
  SCAN FOR TRANCD='2' AND &lcTmpLine..Style = lcSlcStyle AND &lcTmpLine..LineNo =lnLineNo   
    SCATTER MEMVAR MEMO
    IF !EMPTY(ALLTRIM(LOC1 + LOC2 + LOC3 + LOC4 + LOC5 + LOC6 + LOC7 + LOC8))
      INSERT INTO (lcBinLine) FROM MEMVAR
      FOR INQT = 1 TO 8
        lcInqt = ALLTRIM(STR(INQT))
        IF !EMPTY(LOC&lcInqt)
          laInputQty[INQT] = laInputQty[INQT] + m.Qty&lcInqt
        ENDIF
      ENDFOR
    ENDIF
  ENDSCAN
ELSE
  SCATTER MEMVAR MEMO
  SELECT(lcBinLine)
  
  *B130409,1 NNA 11/22/2005 (Begin) Add the Po Number because in case of a Shipment with Multi orders
  *SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laInputQty FOR STYLE = lcSlcStyle
  SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laInputQty FOR PO = lnPoNo AND STYLE = lcSlcStyle AND LineNo = lnLineNo
  *B130409,1 NNA (End)

ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpLine))
  GOTO lnRecNo IN (lcTmpLine)
ENDIF

SELECT(lcBinLine)
SET RELATION OFF INTO SCALE
SET RELATION TO 'S'+SCALE INTO SCALE

*B130409,1 NNA 11/22/2005 (Begin) Add the Po Number because in case of a Shipment with Multi orders
*LOCATE FOR STYLE = lcSlcStyle AND LineNo = lnLineNo
LOCATE FOR PO = lnPoNo AND STYLE = lcSlcStyle AND LineNo = lnLineNo
*B130409,1 NNA (End)

= SEEK('S'+&lcTmpLine..Scale,'SCALE')
lcSize1 = SCALE.SZ1
lcSize2 = SCALE.SZ2
lcSize3 = SCALE.SZ3
lcSize4 = SCALE.SZ4
lcSize5 = SCALE.SZ5
lcSize6 = SCALE.SZ6
lcSize7 = SCALE.SZ7
lcSize8 = SCALE.SZ8

FOR I = 1 TO 8
  LCI=ALLTRIM(STR(I))
  laBinLoc[I] = &lcBinLine..LOC&LCI
  lnQty&LCI   = &lcBinLine..Qty&LCI
ENDFOR

lnQty9   = &lcBinLine..TotQty 
lcStyMjr  = SUBSTR(lcSlcStyle,lnStyStPos,lnStyLen)
lcColor   = gfCodDes(SUBSTR(lcSlcStyle,lnClrStPos,lnClrLen),'COLOR')
IF SEEK(lcSlcStyle,'STYLE')
  lcStyDesc = STYLE.DESC1
ENDIF

PUSH KEY
ON KEY LABEL ENTER
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDetTtl) 
ON KEY LABEL TAB DO lplTab 
ON KEY LABEL BACKTAB DO lplBackTab 

lcMjrPct  = gfItemMask('PM')
DO (gcScrDir+gcWinAppl+"\POSHPBN.SPX")

IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpLine))
  GOTO lnRecNo IN (lcTmpLine)
ENDIF

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
POP KEY

*!**************************************************************************
*!* Name        : lplTab
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/20/2005
*!* Purpose     : Trap TAB key
*!***************************************************************************
*!* Called from : Davmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lplTab()
*!***************************************************************************
*!126994,1
PROCEDURE lplTab

DO CASE
  CASE WONTOP() = lcDetTtl
    ACTIVATE WINDOW POSHPB3
   _CUROBJ=OBJNUM(laShipQty[1])  
  OTHER
    FOR I = 1 TO 8
      LCI=ALLTRIM(STR(I))
      IF _CUROBJ=OBJNUM(lnQty&LCI)  
        = lfvBnQty(I)
      ENDIF
      IF _CUROBJ=OBJNUM(laBinLoc[I])  
        = lfvRecBin(I)
      ENDIF
    ENDFOR
    IF llMove
      _CUROBJ = _CUROBJ + 1
    ENDIF
ENDCASE
*-- End of Function lplTab.
*!**************************************************************************
*!* Name        : lpLBackTab
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/20/2005
*!* Purpose     : Trap BACKTAB key
*!***************************************************************************
*!* Called from : Davmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lpLBackTab()
*!***************************************************************************
*!126994,1
PROCEDURE lpLBackTab
DO CASE
  CASE WONTOP() = lcDetTtl
    ACTIVATE WINDOW POSHPB3
   _CUROBJ=OBJNUM(PbCancel)  
  OTHER
    FOR I = 1 TO 8
      LCI=ALLTRIM(STR(I))
      IF _CUROBJ=OBJNUM(lnQty&LCI)  
        = lfvBnQty(I)
      ELSE
        llMove = .T.
      ENDIF
    ENDFOR
    IF llMove
      _CUROBJ = _CUROBJ - 1
      llMove = .F.
    ENDIF
ENDCASE
*-- End of Function lpLBackTab.
*!*************************************************************
*! Name      : lfReadAct
*! Developer : NADER NABIL (NNA)
*! Date      : 06/20/2005
*! Purpose   : READ Activate function of invoice screen
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
*!126994,1
FUNCTION lfReadAct
IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDetTtl) 
*--End of Function lfReadAct.
*!*************************************************************
*! Name      : lfReadDAct
*! Developer : NADER NABIL (NNA)
*! Date      : 06/20/2005
*! Purpose   : READ Deactivate function of screen Ord100
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfReadDAct()
*!*************************************************************
*!126994,1
FUNCTION lfReadDAct
IF WONTOP() = (lcWinTitle)
  *-- Set browsing variable
  glFromBrow  = .T.
ENDIF
ON KEY LABEL TAB DO lplTab 
ON KEY LABEL BACKTAB DO lpLBackTab 
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDetTtl) 
=lfClearKey()
*--End of Function lfReadDAct.
*!**************************************************************************
*!* Name        : lfClearKey
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/03/2005 
*!* Purpose     : Clear the keys
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfClearKey()
*!***************************************************************************
*!126994,1
FUNCTION lfClearKey
ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL ENTER
*-- End of Function lfClearKey.
*!*************************************************************
*! Name          : lfBrowse
*! Developer     : NNA - NADER NABIL ABD-ALMONAM
*! Date          : 07/03/2005
*! Purpose       : browse lines for the selected P/O
*!*************************************************************
*! Calls         : None.
*!*************************************************************
*! Parameters    : None.
*!*************************************************************
*! Returns       : None.
*!*************************************************************
*! Example       : =lfBrowse()
*!*************************************************************
*!126994,1
FUNCTION lfBrowse
STORE '' TO lcBrowFlds
STORE 0 TO lnCountRec,lnRecordNo
lnAlias = SELECT()
SELECT (lcBinLine)

lnMarker = RECNO()
lcBrowFlds = [lcMarker=IIF(RECNO() = lnMarker ,'>',' '):1:H=' ':W=.F. :5,]+;
             [cWareCode :R :H='Location' :15,QTY1  :R :H='Qty1' :10 ,QTY2 :R :H='Qty2' :10,]+;
             [QTY3      :R :H='Qty3'       :10,QTY4  :R :H='Qty4' :10 ,QTY5 :R :H='Qty5' :10,]+;
             [QTY6      :R :H='Qty6'       :10,QTY7  :R :H='Qty7' :10 ,QTY8 :R :H='Qty8' :10,]+;
             [TOTQTY    :R :H='TotQty'     :10]

*B130409,1 NNA 11/22/2005 (Begin) Add the Po Number because in case of a Shipment with Multi orders
*BROWSE FIELDS &lcBrowFlds 				 ;
              FOR STYLE = lcSlcStyle 	 ;
              AND  lineNo = lnlineno     ;
			  NOAPPEND	  				 ;
			  NOCLEAR     				 ;
			  NODELETE    				 ;
			  NOMENU      				 ;
			  NOWAIT      				 ;
			  SAVE        				 ;
			  VALID :F lfvBrowse() 		 ;
			  WHEN lfwBrowse()			 ;
              TITLE lcDetTtl 			 ;
              WINDOW Poshpb2 IN WINDOW Poshpbn
BROWSE FIELDS &lcBrowFlds 				 ;
              FOR PO + STYLE = lnPoNo + lcSlcStyle 	 ;
              AND  lineNo = lnlineno     ;
			  NOAPPEND	  				 ;
			  NOCLEAR     				 ;
			  NODELETE    				 ;
			  NOMENU      				 ;
			  NOWAIT      				 ;
			  SAVE        				 ;
			  VALID :F lfvBrowse() 		 ;
			  WHEN lfwBrowse()			 ;
              TITLE lcDetTtl 			 ;
              WINDOW Poshpb2 IN WINDOW Poshpbn
*B130409,1 NNA (End)

_CUROBJ = OBJNUM(PbNew)
IF laScrMode[2]
  SHOW GET PbNew      DISABLE 
  SHOW GET PbRemove   DISABLE 
ENDIF
lnRecordNo = RECNO()

*B130409,1 NNA 11/22/2005 (Begin) Add the Po Number because in case of a Shipment with Multi orders
*COUNT FOR (STYLE = lcSlcStyle)  AND (lineNo = lnlineno) TO lnCountRec
COUNT FOR (PO = lnPoNo) AND (STYLE = lcSlcStyle)  AND (lineNo = lnlineno) TO lnCountRec
*B130409,1 NNA (END)

IF BETWEEN(lnRecNo,1,RECCOUNT(lcBinLine))
  GOTO lnRecNo IN (lcBinLine)
ENDIF
FOR N = 1 TO 8
  LCN = ALLTRIM(STR(N))
  lnBalQty&LCN = laShipQty[N] - laInputQty[N]
  m.QTY&LCN = 0
  lcBin&LCN = &lcBinLine..Loc&LCN
  SHOW GET lcBin&LCN
ENDFOR
m.TOTQTY = 0
m.TranCd = '2'
ACTIVATE WINDOW (lcDetTtl) SAME
SELECT(lnAlias)

*-- End Of lfBrowse.
*!**************************************************************************
*!* Name        : lfvBrowse
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/03/2005
*!* Purpose     : 
*!***************************************************************************
*!* Called from : Davmain.prg
*!***************************************************************************
*!* Parameters : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfvBrowse()
*!***************************************************************************
*!126994,1
FUNCTION lfvBrowse
IF TYPE('lcDetTtl') = 'C'
  IF WONTOP() # (lcDetTtl)
    = gfStopBrow()
  ENDIF
ENDIF
*-- End of Function lfvBrowse.
*!**************************************************************************
*!* Name        : lfWbrowse
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/03/2005
*!* Purpose     : When function for Screen's browse [Receive into Multiple Bins Screen]
*!***************************************************************************
*!* Called from : Davmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : =lfWbrowse()
*!***************************************************************************
*!126994,1
FUNCTION lfWbrowse
lnMarker = RECNO()
FOR I = 1 TO 8
  LCI = ALLTRIM(STR(I))
  laBinLoc[I] = &lcBinLine..Loc&LCI
  lnQty&LCI   = &lcBinLine..QTY&LCI
ENDFOR
lnQty9 = &lcBinLine..TotQty
SHOW GETS
SHOW WINDOW (lcDetTtl) REFRESH SAME
*-- End of Function lfWbrowse.
*!*************************************************************
*!* Name        : lfChkStrct
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/03/2005
*!* Purpose     : Get the Style and Color Length.
*!*************************************************************
*!* Calls       : Procedures : ....
*!*               Functions  : ....
*!*************************************************************
*!* Called from : Davmain.Prg
*!*************************************************************
*!* Parameters  : None
*!*************************************************************
*!* Returns     : None
*!*************************************************************
*!* Example     : =lfChkStrct()
*!*************************************************************
*!126994,1
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen   = LEN(laItemSeg[lnCount,3])
    lnClrStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLen   = LEN(laItemSeg[lnCount,3])
    lnStyStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLen   = LEN(laItemSeg[lnCount,3])
    lnScaStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*!**************************************************************************
*!* Name        : lfValidBut
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/05/2005
*!* Purpose     : Validation for New,Remove and Ok buttons
*!***************************************************************************
*!* Called from : PoShpBn.SCX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfValidBut()
*!***************************************************************************
*!126994,1
FUNCTION lfValidBut
PARAMETERS lcButton
PRIVATE llGo , lnCountRec , lcOldOrder,lnReturn
STORE 0 TO lnReturn,lnCountRec
llGo = .T.
lcBinLine = lcTmpLine + 'A'
lnOldAlias=SELECT(0)
DO CASE
  *--*--*--*--*--*--*--*-- New Button
  CASE lcButton = 'New'
    llNew = .T.
    SELECT(lcBinLine)
    FOR I = 1 TO 8
      LCI = ALLTRIM(STR(I))
      IF lnQty&LCI > 0
        IF EMPTY(laBinLoc[I])
          llGo = .F.
          EXIT
        ENDIF
      ENDIF
      IF lnQty&LCI = 0
        SHOW GET laBinLoc[I] ENABLE
        SHOW GET lnQty&LCI   ENABLE        
      ENDIF
    ENDFOR
    SHOW GET PbRemove ENABLE
    IF llGo 
      APPEND BLANK
      GATHER MEMVAR MEMO
      STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnQty9
      STORE '' TO laBinLoc
      _CUROBJ = OBJNUM(laBinLoc[1])
    ELSE
      =gfModalGen("QRM00000B00000","DIALOG",.F.,.F.,'You have to assign bins before adding new record')
      FOR I = 1 TO 8
        IF EMPTY(laBinLoc[I]) AND laShipQty[I] > 0
          _CUROBJ = OBJNUM(laBinLoc[I])      
          EXIT
        ENDIF
      ENDFOR
      RETURN
    ENDIF
  *--*--*--*--*--*--*--*-- Remove Button
  CASE lcButton = 'Remove'
    SELECT(lcBinLine)
    FOR I = 1 TO 8
      LCI = ALLTRIM(STR(I))
      laInputQty[I] = laInputQty[I] - &lcBinLine..Qty&LCI
      lnBalQty&LCI  = laShipQty[I]  - laInputQty[I]
    ENDFOR
    DELETE
    LOCATE
    COUNT FOR !DELETED() TO lnCountRec 
    GO BOTTOM
    IF lnCountRec = 0
      SHOW GET PbRemove DISABLE
    ENDIF
    = lfWbrowse()
    SHOW GETS
  *--*--*--*--*--*--*--*-- Ok Button
  CASE lcButton = 'Ok'
    IF laShipQty[1]+laShipQty[2]+laShipQty[3]+laShipQty[4]+laShipQty[5]+laShipQty[6]+laShipQty[7]+laShipQty[8] > laInputQty[9]
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'You have to receive all open quantities to bins before save')
      RETURN
    ENDIF
    lcOldOrder = ORDER(lcTmpLine)
    SET ORDER TO TAG POSLN IN (lcTmpLine)
    SELECT (lcBinLine)
    LOCATE
    SELECT(lcTmpline)
    lnTmplRec = RECNO()
    DELETE ALL FOR cstytype+po+style+STR(lineno,6)+trancd = 'P' + lnPoNo + lcSlcStyle+STR(lnLineNo,6)+'2'
    SELECT (lcBinLine)
    llOk = .T.
    SCAN FOR TOTQTY > 0
      FOR I = 1 TO 8
        LCI = ALLTRIM(STR(I))
        IF QTY&LCI > 0 AND EMPTY(LOC&LCI)
          llOk = .F.
          lnReturn = RECNO()
          EXIT  
        ENDIF
      ENDFOR
    ENDSCAN
    IF !llOk
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,"You have to assign all quantities to bins location before saving .")
      IF BETWEEN(lnReturn,1,RECCOUNT(lcBinLine))
        GOTO lnReturn IN (lcBinLine)
        lnMarker = lnReturn
      ENDIF
      FOR I = 1 TO 8
        IF EMPTY(laBinLoc[I]) AND laShipQty[I] > 0
          _CUROBJ = OBJNUM(laBinLoc[I])      
          EXIT
        ENDIF
      ENDFOR
      RETURN
    ENDIF
    
    *B130409,1 NNA 11/22/2005 (Begin) Add the Po Number because in case of a Shipment with Multi orders
    *SCAN FOR TOTQTY > 0 AND STYLE = lcSlcStyle AND  LineNo = m.LineNo
    SCAN FOR TOTQTY > 0 AND PO = lnPoNo AND STYLE = lcSlcStyle AND  LineNo = m.LineNo
    *B130409,1 NNA (END)

      SCATTER MEMVAR MEMO
      IF !SEEK('P'+lnPoNo+m.Style+STR(m.LineNo,6)+'2',lcTmpLine)
        INSERT INTO (lcTmpLine) FROM MEMVAR
      ELSE
        SELECT (lcTmpLine)
        FOR I = 1 TO 8
          LCI = ALLTRIM(STR(I))
          REPLACE QTY&LCI	WITH QTY&LCI + m.Qty&LCI
        ENDFOR
        REPLACE TOTQTY	    WITH TOTQTY + m.TOTQTY
      ENDIF
    ENDSCAN
    IF SEEK('P'+lnPoNo+lcSlcStyle+STR(lnLineNo,6)+'1',lcTmpLine)
      SELECT(lCTmpLine)
      
      *B130409,1 NNA 11/22/2005 (Begin) Acumulate the TotStk Field in case of manual Receiving and not Receiving by Shipment
      *REPLACE TOTSTK WITH  IIF(lcAuto='M',TOTSTK + laInputQty[9],laInputQty[9])  ,;
              TOTBAL WITH  lnBalQty9
      REPLACE TOTSTK WITH IIF(lcAuto='M' AND lcPType<>'S',TOTSTK + laInputQty[9],laInputQty[9])  ,;
              TOTBAL WITH lnBalQty9
      *B130409,1 NNA (End)
      
    ENDIF    
    SELECT(lcBinLine)
    LOCATE
    lnTotalStk = 0
    SUM ALL TOTQTY TO lnTotalStk
    lnTotStk = lnTotalStk
    IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpLine))
      GOTO lnTmplRec IN (lcTmpLine)
    ENDIF
    SET ORDER TO TAG &lcOldOrder IN (lcTmpLine)
    CLEAR READ
  *--*--*--*--*--*--*--*-- Cancel Button
  CASE lcButton = 'Cancel'
    COUNT FOR STYLE = EVAL(lcTmpLine+'.STYLE') TO lnCountRec 
    IF lnCountRec > 0
      IF gfModalGen("QRM00031B00006","DIALOG") = 2
        RETURN
      ELSE
        SELECT(lcBinLine)
        LOCATE
        DELETE ALL FOR STYLE=EVAL(lcTmpLine+'.STYLE')
        CLEAR READ
      ENDIF
    ELSE
      CLEAR READ
    ENDIF
ENDCASE
SHOW GETS
lnMarker = RECNO()
SHOW WINDOW (lcDetTtl) REFRESH SAME
SELECT(lnOldAlias)

*-- End of Function lfValidBut.
*!**************************************************************************
*!* Name        : lfShipQty
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/05/2005
*!* Purpose     : Validate the ship Qty fields
*!***************************************************************************
*!* Called from : PoShpBn.SCX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfShipQty()
*!***************************************************************************
*!126994,1
FUNCTION lfShipQty
PARAMETERS lnFldNo
lcFldNo = ALLTRIM(STR(lnFldNo))
lnBalQty&lcFldNo = laShipQty[lnFldNo] - laInputQty[lnFldNo]
SHOW GETS
*-- End of Function lfShipQty.
*!**************************************************************************
*!* Name        : lfBnOldVal
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/05/2005
*!* Purpose     : Save old value for Receive into Multiple Bins Screen
*!***************************************************************************
*!* Called from : PoShpBn.SCX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfBnOldVal()
*!***************************************************************************
*!126994,1
FUNCTION lfBnOldVal
lcOldValue = EVALUATE(SYS(18))
*-- End of Function lfBnOldVal.
*!**************************************************************************
*!* Name        : lfvBnQty
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/05/2005
*!* Purpose     : Valid on the Received Qty per Bin
*!***************************************************************************
*!* Called from : PoShpBn.SCX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfvBnQty()
*!***************************************************************************
*!126994,1
FUNCTION lfvBnQty
PARAMETERS lnFldNo
lcFldNo = ALLTRIM(STR(lnFldNo))
laInputQty[lnFldNo] =  MAX(laInputQty[lnFldNo] - lcOldValue,0)
IF lnQty&lcFldNo > (laShipQty[lnFldNo]-laInputQty[lnFldNo])
  lcMsg = "Bin quantity is greater than the remaining quantity "
  lcMsg = lcMsg + IIF((laShipQty[lnFldNo] - laInputQty[lnFldNo])>0, ", you can only add ",'')
  lcMsg = lcMsg + ALLTRIM(STR(laShipQty[lnFldNo] - laInputQty[lnFldNo])) 
  lcMsg = lcMsg + " , Do you want to increase ship quantity to over receive ?"
  IF gfModalGen('INM00000B34001',.F.,.F.,.F.,lcMsg) = 2
    lnQty&lcFldNo = IIF(lcOldValue=0,0,lcOldValue)
    laInputQty[lnFldNo] = laInputQty[lnFldNo] + lnQty&lcFldNo
    llMove = .F.
    _CUROBJ = OBJNUM(lnQty&lcFldNo)
    RETURN
  ELSE
    laShipQty[lnFldNo] = laShipQty[lnFldNo] + IIF(lcOldValue = 0 AND lnQty&lcFldNo > laShipQty[lnFldNo],;
                         (lnQty&lcFldNo - laShipQty[lnFldNo]), (lnQty&lcFldNo - lcOldValue))
  ENDIF
ENDIF
llMove = .T.
SELECT(lcBinLine)
lnTmpRec  = RECNO()
lnQty9              = (lnQty9 -lcOldValue) + lnQty&lcFldNo
REPLACE &lcBinLine..Qty&lcFldNo WITH lnQty&lcFldNo	,;
        &lcBinLine..TotQty      WITH lnQty9
GO BOTTOM
GO TOP

*B130409,1 NNA 11/22/2005 (Begin) Add the Po Number because in case of a Shipment with Multi orders
*SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laInputQty FOR STYLE = lcSlcStyle AND LineNo =lnLineNo
SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laInputQty FOR PO=lnPoNo AND STYLE = lcSlcStyle AND LineNo =lnLineNo
*B130409,1 NNA (End)


IF BETWEEN(lnTmpRec,1,RECCOUNT(lcBinLine))
  GOTO lnTmpRec IN (lcBinLine)
ENDIF

lnBalQty&lcFldNo = laShipQty[lnFldNo] - laInputQty[lnFldNo]
SHOW GETS
SHOW WINDOW (lcDetTtl) REFRESH SAME
*-- End of Function lfvBnQty.
*!**************************************************************************
*!* Name        : lfvRecBin
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/05/2005
*!* Purpose     : Bin Locations valid for Receive into Multiple Bins Screen
*!***************************************************************************
*!* Called from : POSHPBN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfvRecBin()
*!***************************************************************************
*!126994,1
FUNCTION lfvRecBin
PARAMETERS lnFldNo
PRIVATE lcOldOrder,lcOldBnOrd,lcTmpLnOrd
lcFldNo = ALLTRIM(STR(lnFldNo))
lnAlias = SELECT()
lcOldOrder = ORDER('WHSLOC')
SET ORDER TO TAG WHSLOC IN WHSLOC
lcTmpLnOrd = ORDER(lcTmpLine)
SET ORDER TO TAG TmpLine1 IN (lcTmpLine)
IF llbrowse OR (!EMPTY(laBinLoc[lnFldNo]) AND !SEEK(lcWareCode+laBinLoc[lnFldNo],'WHSLOC')) OR;
   '?' $ laBinLoc[lnFldNo]
  llbrowse =.F. 
  SELECT WHSLOC
  lcOldBnOrd = ORDER()
  SET ORDER TO TAG WHSBIN
  lcBrFields = "cWareCode :R:h='Warehouse',clocation :R :H='Bin',cBinClass :R :H='Bin Class',cFlatHang :R :H='Flat or Hang'"
  lcForExpr = " cWareCode = lcWareCode AND Style = SPACE(19)"
  lcBrTitle = "Available Bins Location"
  lcSelBin  = IIF(ARIABROW([FOR &lcForExpr],lcBrTitle,5,15,25,55,'','','cLocation','laBrowArr'),laBrowArr[1],'')
  laBinLoc[lnFldNo] = IIF(!EMPTY(lcSelBin),ALLTRIM(lcSelBin),lcOldVAlue)
  SHOW GET laBinLoc[lnFldNo]
  SET ORDER TO TAG &lcOldBnOrd
ENDIF
IF EMPTY(laBinLoc[lnFldNo])
  llMove = .T.
  RETURN
ENDIF
IF EMPTY(laBinLoc[lnFldNo]) 
  SELECT (lcTmpLine)
  REPLACE &lcBinLine..Loc&lcFldNo WITH laBinLoc[lnFldNo]
ENDIF
IF !EMPTY(laBinLoc[lnFldNo]) 
  SELECT WHSLOC
  SET ORDER TO TAG WhsLoc
  IF !SEEK(lcWareCode+laBinLoc[lnFldNo],'WHSLOC')
    SET ORDER TO TAG WhsLocSt IN WHSLOC 
    =gfModalGen('INM00000B00000','','','',ALLTRIM(laBinLoc[lnFldNo]) + ' is not a valid location')
    _CUROBJ = OBJNUM(laBinLoc[lnFldNo])
    laBinLoc[lnFldNo] = ''
    RETURN
  ELSE
    =SEEK(lcWareCode+laBinLoc[lnFldNo],'WHSLOC')
    IF !(WhsLoc.cFlatHang = style.cflathang)
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot issue the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      laBinLoc[lnFldNo] = ''
      RETURN
    ENDIF
    llClass = .F.
    IF (WhsLoc.cBinClass == style.cPrimClss&lcFldNo)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cSecClss&lcFldNo)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cRemClss&lcFldNo)
      llClass = .T.
    ENDIF
    IF !llClass
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot Issue the style to a bin of a different class.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      laBinLoc[lnFldNo] = ''
      RETURN
    ENDIF
    SELECT (lcBinLine)
    REPLACE &lcBinLine..Loc&lcFldNo WITH laBinLoc[lnFldNo]
  ENDIF
  SELECT WHSLOC
  SET ORDER TO TAG WhsLocSt
ENDIF
llMove = .T.
SET ORDER TO TAG &lcOldOrder IN WHSLOC
SELECT(lnAlias)

*-- End of Function lfvRecBin.
*!*************************************************************************
*!* Name        : lfChkMltBn
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/10/2005
*!* Purpose     : Validate Po Receiveing into multiple Bins.
*!***************************************************************************
*!* Called from : DAVMAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChkMltBn()
*!***************************************************************************
*!126994,1
FUNCTION lfChkMltBn
llReturn  = .F.
llChckLoc = .F.
llChckCls = .F.
llNotAssgn= .F.
lcBinLine = lcTmpLine + 'A'
DIMENSION laStyArr[1]
STORE '' TO laStyArr
SELECT (lcTmpLine)
lcOldOrder = ORDER()
SET ORDER TO POSLN
LOCATE
SCAN FOR TRANCD ='1'
  IF llReturn
    EXIT
  ENDIF
  
  *B130409,1 NNA 11/22/2005 (Begin) Add the PO Number to the Seek and Scan Statement
  *IF SEEK(&lcTmpLine..Style,lcBinLine)
  *  SELECT(lcBinLine)
  *  SCAN REST WHILE STYLE = &lcTmpLine..Style

  *! B131115,1 MHM 02/19/2006 Add Line no to the index [Start]
  *IF SEEK(&lcTmpLine..PO + &lcTmpLine..Style,lcBinLine)
  IF SEEK(&lcTmpLine..PO + &lcTmpLine..Style+ STR(&lcTmpLine..LINENO,6) ,lcBinLine)
  *! B131115,1 MHM  [End]

    SELECT(lcBinLine)

    *! B131115,1 MHM 02/19/2006 Add Line no to the index [Start]
    *SCAN REST WHILE PO + STYLE = &lcTmpLine..PO + &lcTmpLine..Style
    SCAN REST WHILE PO + STYLE + STR(LINENO,6) = &lcTmpLine..PO + &lcTmpLine..Style +STR(&lcTmpLine..LINENO,6)
    *! B131115,1 MHM [End]

  *B130409,1 NNA (End)
  
      m.loc1 = &lcBinLine..loc1
      m.loc2 = &lcBinLine..loc2
      m.loc3 = &lcBinLine..loc3
      m.loc4 = &lcBinLine..loc4
      m.loc5 = &lcBinLine..loc5
      m.loc6 = &lcBinLine..loc6
      m.loc7 = &lcBinLine..loc7
      m.loc8 = &lcBinLine..loc8
      FOR lnI = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        IF !EMPTY(&lcBinLine..Qty&lcI)
          *--mhm2006 check
          *IF EMPTY(m.loc&lcI) 
          IF EMPTY(m.loc&lcI) AND TOTSTK <> 0
          *--mhm2006 check
            llChckLoc = .T.
            =SEEK(Style,'STYLE')
            IF EMPTY(laStyArr[1])
              laStyArr[1] = Style.cflathang
            ELSE
              IF !(laStyArr[1] = Style.cflathang)
                llChckCls = .T.
                llReturn  = .T.
                EXIT
              ENDIF
            ENDIF
          ENDIF
        ENDIF  
      ENDFOR
    ENDSCAN
  ELSE
    llReturn   = .F.
    *--mhm2006 check
    *llNotAssgn = .T.
    IF TOTSTK <> 0
      llNotAssgn = .T.
    ENDIF
    *--mhm2006 check
    SCATTER MEMVAR MEMO
    INSERT INTO (lcBinLine) FROM MEMVAR
  ENDIF
ENDSCAN
SELECT (lcBinLine)
LOCATE
IF llChckCls
  lcMsg2 = 'You have mixed Flat and Hanging styles on this transaction - please enter the bin locations manually'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llReturn = .T.
  STORE .F. TO llShow,llCSave
  SELECT (lcTmpLine)
  SET ORDER TO &lcOldOrder
  LOCATE
  RETURN llReturn
ENDIF
IF llChckLoc OR llNotAssgn
  lcMsg2 = "There is a line or more that didn't assign to bins,you have to assign bin location before saving ." + ;
          " Do you want to assign bin location now to not assigned Quantities?"
  lnChoose =gfModalGen("QRM00000B38006","DIALOG",.F.,.F.,lcMsg2)
  IF lnChoose = 1
    IF !USED('WHSLOC') 
      =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
    ENDIF
    DIME laTempData[3]
    Store '' TO lcBinLoc , laTempData
    DO (gcScrDir+gcWinAppl+"\POdlbnSC.Spx")
    IF EMPTY(lcBinLoc)
      lcMsg2 = 'You have to assign bin location before saving .'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      llReturn = .T.
      STORE .F. TO llShow,llCSave
      SELECT (lcTmpLine)
      SET ORDER TO &lcOldOrder
      LOCATE
      RETURN llReturn
    ENDIF
    SELECT (lcBinLine)
    llReturn = .F.
    SCAN
      =SEEK(&lcBinLine..Style,'Style')
      IF !(laTempData[3] = style.cflathang)
        *--'You cannot saving the style to a bin of a different Hang/Flat.'
        lcMsg2 = 'You cannot saving the style to a bin of a different Hang/Flat.'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        llReturn = .T.
        STORE .F. TO llShow,llCSave
        SELECT (lcTmpLine)
        SET ORDER TO &lcOldOrder
        LOCATE
        RETURN llReturn
      ELSE
        FOR I = 1 TO 8
          LCI = ALLTRIM(STR(I))
          IF EMPTY(&lcBinLine..LOC&LCI) AND &lcBinLine..QTY&LCI > 0
            REPLACE &lcBinLine..LOC&LCI  WITH laTempData[1]
          ENDIF           
        ENDFOR
      ENDIF
    ENDSCAN
    STORE .F. TO llShow,llCSave
    SELECT (lcTmpLine)
    SET ORDER TO &lcOldOrder
    LOCATE
    RETURN llReturn
  ELSE
    llReturn = .T.
    STORE .F. TO llShow,llCSave
    SELECT (lcTmpLine)
    SET ORDER TO &lcOldOrder
    LOCATE
    RETURN llReturn
  ENDIF  
ENDIF
SELECT (lcTmpLine)
SET ORDER TO &lcOldOrder
LOCATE
SHOW GETS
RETURN llReturn

*-- End of Function lfChkMltBn.
*!*************************************************************************
*!* Name        : lfSavMltBn
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/08/2005
*!* Purpose     : Save Multiple Bins Location Qty. to whbnloc file
*!***************************************************************************
*!* Called from : Davmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSavMltBn()
*!***************************************************************************
*!126994,1
FUNCTION lfSavMltBn
PRIVATE lnAlias
lnAlias = SELECT(0)
lcBinLine = lcTmpLine + 'A'
IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF
IF !USED('WHSLOC') 
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
ENDIF
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF
IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF
SELECT (lcBinLine)
LOCATE
SCAN 
  m.loc1 = &lcBinLine..loc1
  m.loc2 = &lcBinLine..loc2
  m.loc3 = &lcBinLine..loc3
  m.loc4 = &lcBinLine..loc4
  m.loc5 = &lcBinLine..loc5
  m.loc6 = &lcBinLine..loc6
  m.loc7 = &lcBinLine..loc7
  m.loc8 = &lcBinLine..loc8
  FOR R = 1 TO 8
    LCR = ALLT(STR(R,2))
    IF !EMPTY(m.loc&lcR) AND &lcBinLine..QTY&LCR > 0
      IF !SEEK(&lcBinLine..cWareCode+m.loc&LCR+&lcBinLine..style,'WHBINLOC')
        SET ORDER TO TAG WhsLoc IN  WHSLOC
        IF SEEK(&lcBinLine..cWareCode+m.loc&LCR,'WHSLOC')
          SELECT WHSLOC
          m.cbinclass = WHSLOC.cbinclass
          FOR I = 1 TO FCOUNT('WHSLOC')
            IF UPPER(FIELD(I))='CBLKPCK'
              m.cBlkPck   = WHSLOC.cBlkPck
            ENDIF
            IF UPPER(FIELD(I))='CSECTION'
              m.cSection  = WHSLOC.cSection
            ENDIF
          ENDFOR
          SELECT(lnOldAlias)
        ENDIF
        SET ORDER TO TAG WhsLocSt IN  WHSLOC
        SELECT WHBINLOC
        APPEND BLANK
        REPLACE Style        WITH  &lcBinLine..style     ,;
  		        cWareCode    WITH  &lcBinLine..cWareCode ,;
  		        clocation    WITH  m.loc&LCR             ,;
  		        Qty&LCR      WITH  &lcBinLine..Qty&LCR   ,;
  		        TOTQTY       WITH  &lcBinLine..Qty&LCR   ,;
  		        cbinclass    WITH  m.cbinclass
        FOR I = 1 TO FCOUNT('WHBINLOC')
          IF UPPER(FIELD(I))='CBLKPCK'
            REPLACE cBlkPck WITH m.cBlkPck
          ENDIF
          IF UPPER(FIELD(I))='CSECTION'
            REPLACE cSection WITH m.cSection
          ENDIF
        ENDFOR
        =gfAdd_Info('WHBINLOC')
      ELSE
        REPLACE  WHBINLOC.Qty&LCR WITH WHBINLOC.Qty&LCR + &lcBinLine..Qty&LCR,;
                 WHBINLOC.TOTQTY  WITH WHBINLOC.TOTQTY  + &lcBinLine..Qty&LCR
      ENDIF     
      lcOldOrder = ORDER('WHSLOC')
      SET ORDER TO TAG WhsLoc IN  WHSLOC
      IF !SEEK(&lcBinLine..cWareCode+m.loc&LCR+&lcBinLine..style,'WHSLOC')
        =SEEK(EVAL(lcBinLine+'.cWareCode')+m.loc&LCR,'WHSLOC')
        SCATT MEMVAR MEMO
        m.STYLE     = &lcBinLine..style
        m.clocation = m.loc&LCR
        INSERT INTO WHSLOC FROM MEMVAR 
      ENDIF
      SET ORDER TO TAG &lcOldOrder IN  WHSLOC
    ENDIF  
  ENDFOR
ENDSCAN
SELECT(lnAlias)
*-- End of Function lfSavMltBn.
*!*************************************************************************
*!* Name        : lfUpdBnInv
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/08/2005
*!* Purpose     : Update BinInvJL & from issue Styinvjl in the Receiving into
*!*             : Multiple Bins Screen.
*!***************************************************************************
*!* Called from : DAVMAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfUpdBnInv()
*!***************************************************************************
*!C126994,1
FUNCTION lfUpdBnInv
PRIVATE lcStyOrd,lnAlias,lcCurTmp 
DIMENSION laBinArr[8]

lcBinLine = lcTmpLine + 'A'

lnAlias = SELECT(0)

*--mhmnna
lnGetRec = RECNO() 
lnCurLin = Lineno
lcStybn = IIF(!EMPTY(&lcTmpLine..STYLE),&lcTmpLine..STYLE,m.STYLE)
lcPobn = IIF(!EMPTY(&lcTmpLine..PO),&lcTmpLine..Po,m.Po)
*--mhmnna

IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF
IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','STYINVJL','SH')
ENDIF
lcCurTmp = gfTempName()
CREATE TABLE (gcWorkDir + lcCurTmp);
       ( nstk1 N(7), nstk2 N(7),nstk3 N(7),nstk4 N(7),nstk5 N(7),nstk6 N(7),nstk7 N(7),;
       nstk8 N(7), nTotstk N(7), Binloc C(10) )
INDEX ON Binloc  TAG (lcCurTmp)
SELECT (lcTmpLine)



*! B131115,1 MHM 02/19/2006 Add Line no to the index [Start]
*IF SEEK(lcStybn,lcBinLine)
IF SEEK(lcPobn + lcStybn + STR(lnCurLin,6) ,lcBinLine)
*! B131115,1 MHM 02/19/2006 AddLine no to the index [Start]

  SELECT(lcBinLine)
    
  *--mhmnna
  *SCAN REST WHILE &lcBinLine..STYLE = &lcTmpLine..STYLE
  *--mhm2006
  *SCAN REST WHILE STYLE = lcStybn 
  
  *! B131115,1 MHM 02/19/2006 Add line no to the scan [Start]
  *SCAN REST WHILE PO + STYLE = lcPobn + lcStybn 
  SCAN REST WHILE PO + STYLE + STR(Lineno,6) = lcPobn + lcStybn + STR(lnCurLin,6)
  *! B131115,1 MHM 02/19/2006 [End]
    
    STORE '' TO laBinArr
    FOR I = 1 TO 8 
      LCI = ALLTRIM(STR(I,1))
      laBinArr[I] =  &lcBinLine..loc&LCI
    ENDFOR
    SELECT (lcCurTmp)
    FOR N = 1 To 8
      LCN = ALLTRIM(STR(N,1))
      IF &lcBinLine..Qty&LCN<> 0  
        IF !SEEK(laBinArr[N],lcCurTmp) 
          APPEND BLANK
          REPLACE nstk&LCN WITH &lcBinLine..Qty&LCN,;
                  nTotstk  WITH &lcBinLine..Qty&LCN,;
                  Binloc   WITH laBinArr[N]
        ELSE
          *REPLACE nstk&LCN WITH &lcBinLine..Qty&LCN,;
                  nTotstk  WITH nTotstk + &lcBiNLine..Qty&LCN
          REPLACE nstk&LCN WITH &lcBinLine..Qty&LCN + nstk&LCN ,;
                  nTotstk  WITH nTotstk + &lcBiNLine..Qty&LCN
        ENDIF
      ENDIF  
    ENDFOR
  ENDSCAN
ENDIF
SELECT (lcCurTmp)
LOCATE
IF EOF()
  SELECT(lnAlias)
  IF BETWEEN(lnGetRec,1,RECCOUNT())
    GOTO lnGetRec 
  ENDIF
  *--mhm2006
  IF USED(lcCurTmp)
    USE IN &lcCurTmp
  ENDIF  
  ERASE (gcWorkDir+lcCurTmp+'.DBF')
  ERASE (gcWorkDir+lcCurTmp+'.CDX')
  *--mhm2006

  RETURN
ENDIF
SELECT STYINVJL
*--mhmnna
*=SEEK(&lcTmpLine..Style+&lcTmpLine..cwarecode+lcGlSession)
=SEEK(lcStybn+m.cwarecode)
SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = lcStybn+m.cwarecode
 IF (dtrdate = ldTrDate) AND (ctrcode= lcPobn) AND (lineno = m.lineno)
   EXIT
 ENDIF
ENDSCAN
*--mhmnna
SCATT MEMVAR MEMO
SELECT (lcCurTmp)
SCAN
  SCATT MEMVAR MEMO
  m.clocation = m.Binloc 
  m.nstkval   = m.nTotstk * m.nCost
  SELECT BININVJL
  *--mhmnna
  IF !SEEK(lcStybn+m.cwarecode+m.csession+DTOS(m.dtrdate)+m.Ctrcode+STR(m.lineno,6))
  *--mhmnna
    APPEND BLANK
    GATHER MEMVAR MEMO
  ELSE
    llBinFound = .F.
    SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = ;
         m.Style+m.cwarecode+m.csession+DTOS(m.dtrdate)+m.Ctrcode+STR(m.lineno,6) FOR ALLTRIM(cLocation) = ALLTRIM(m.clocation)
      llBinFound = .T.
    ENDSCAN
    IF !llBinFound
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDIF
  ENDIF
  llBinFound = .F.
ENDSCAN

IF USED(lcCurTmp)
  USE IN &lcCurTmp
ENDIF  
ERASE (gcWorkDir+lcCurTmp+'.DBF')
ERASE (gcWorkDir+lcCurTmp+'.CDX')

SELECT(lnAlias)
IF BETWEEN(lnGetRec,1,RECCOUNT())
  GOTO lnGetRec 
ENDIF

*-- End of Function lfUpdBnInv.
*!*************************************************************************
*!* Name        : lfDELSTY
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 08/29/2005
*!* Purpose     : Delete a Style from the CSTPRICE file (custom file for David Luke)
*!***************************************************************************
*!* Called from : PROCEDURE LPDELSCR IN ICSTYLE.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDELSTY()
*!***************************************************************************
*!129334,1
FUNCTION lfDELSTY
PRIVATE lnSlct,lnColorNo,lcColorCod
lnSlct = SELECT()
IF !USED('CSTPRICE')
  =gfOpenFile(gcDataDir+'CSTPRICE','CSTYCODE','SH')
ENDIF
lnColorNo  = IIF(llallcolors,ALEN(latarget),1)
lcColorCod =''
FOR I = 1 TO lnColorNo
  lcColorCod = IIF(llAllColors , laTarget(I), SUBSTR(lcNMKey,2,LEN(lcNMKey)-1))
  SELECT CSTPRICE
  IF SEEK(lcMajor+lcSepart+lcColorCod)
    BLANK REST FOR (StyDv=lcMajor+lcSepart+lcColorCod) 
    DELETE ALL FOR STYDV = SPACE(19) AND COLORDV = SPACE(6) AND SCALEDV=SPACE(3)
  ENDIF
ENDFOR
SELECT (lnSlct)
*-- End of Function lfDELSTY.

*********mhmhmhmh
*!**************************************************************************
*! Name      : lfALSAVINV
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/03/2004
*! Purpose   : Save Data to Invoice screen
*!**************************************************************************
*! Example   : = lfALSAVINV()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*CB131116,1 MHM 04/06/2004 
FUNCTION lfALSAVINV

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN
ENDIF
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF

IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF

*--check in case of consolidate
IF  &lcHdrFile..CONSOL = 'Y'
  RETURN
ENDIF
*

SELECT PKBINLOC
lcCuOrdr  = ORDER()
SET ORDER TO Pkline
IF SEEK(&lcDetFile..PikTkT+&lcDetFile..cWareCode+STR(&lcDetFile..lineno,6)+&lcDetFile..style ,'PKBINLOC') 
  SCAN REST WHILE piktkt+cwarecode+STR(lineno,6)+style+clocation= &lcDetFile..PikTkT+&lcDetFile..cWareCode+STR(&lcDetFile..lineno,6)+&lcDetFile..style
    IF SEEK(&lcDetFile..Style+ &lcDetFile..cWareCode+PKBINLOC.clocation,'WHBINLOC') 

       *B132036,1 NNA 06/22/2006 (Begin) select Whbinloc file to delete from it because in this case the cursor is on pkbinloc file
       SELECT WHBINLOC
       *B132036,1 NNA (END)

       REPLACE WHBINLOC.Alo1 WITH MAX(WHBINLOC.Alo1 - PkBinLoc.Qty1,0) 
       REPLACE WHBINLOC.Alo2 WITH MAX(WHBINLOC.Alo2 - PkBinLoc.Qty2,0) 
       REPLACE WHBINLOC.Alo3 WITH MAX(WHBINLOC.Alo3 - PkBinLoc.Qty3,0) 
       REPLACE WHBINLOC.Alo4 WITH MAX(WHBINLOC.Alo4 - PkBinLoc.Qty4,0) 
       REPLACE WHBINLOC.Alo5 WITH MAX(WHBINLOC.Alo5 - PkBinLoc.Qty5,0) 
       REPLACE WHBINLOC.Alo6 WITH MAX(WHBINLOC.Alo6 - PkBinLoc.Qty6,0) 
       REPLACE WHBINLOC.Alo7 WITH MAX(WHBINLOC.Alo7 - PkBinLoc.Qty7,0) 
       REPLACE WHBINLOC.Alo8 WITH MAX(WHBINLOC.Alo8 - PkBinLoc.Qty8,0) 
       REPLACE WHBINLOC.TotAlo WITH MAX(WHBINLOC.TotAlo - PkBinLoc.TotQty,0) 
       
       REPLACE WHBINLOC.Qty1 WITH MAX(WHBINLOC.Qty1 - PkBinLoc.Qty1,0) 
       REPLACE WHBINLOC.Qty2 WITH MAX(WHBINLOC.Qty2 - PkBinLoc.Qty2,0) 
       REPLACE WHBINLOC.Qty3 WITH MAX(WHBINLOC.Qty3 - PkBinLoc.Qty3,0) 
       REPLACE WHBINLOC.Qty4 WITH MAX(WHBINLOC.Qty4 - PkBinLoc.Qty4,0) 
       REPLACE WHBINLOC.Qty5 WITH MAX(WHBINLOC.Qty5 - PkBinLoc.Qty5,0) 
       REPLACE WHBINLOC.Qty6 WITH MAX(WHBINLOC.Qty6 - PkBinLoc.Qty6,0) 
       REPLACE WHBINLOC.Qty7 WITH MAX(WHBINLOC.Qty7 - PkBinLoc.Qty7,0) 
       REPLACE WHBINLOC.Qty8 WITH MAX(WHBINLOC.Qty8 - PkBinLoc.Qty8,0) 
       REPLACE WHBINLOC.TotQty WITH MAX(WHBINLOC.TotQty - PkBinLoc.TotQty,0) 
       *132036,1 NNA 06/22/2006 (Begin) select Whbinloc file to delete from it because in this case the cursor is on pkbinloc file
       IF TotQty = 0
         DELETE
       ENDIF
       *B132036,1          
      
    ENDIF
    *B132036,1 NNA 06/22/2006 (BEGIN) RETURN BACK TO THE PKBIONLOC FILE
    SELECT PKBINLOC
    *B132036,1 NNA (END)
    FOR lnCnt = 1 TO 8
      lcCnt  = ALLT(STR(lnCnt))
      IF PkBinLoc.Qty&lcCnt <> 0
        =lfTrkInv(PkBinLoc.Qty&lcCnt,lnCnt,&lcDetFile..Order,lcInvNo,&lcDetFile..LineNo)
      ENDIF  
    ENDFOR  
    REPLACE PKBINLOC.linvoiced WITH .T.
  ENDSCAN
ENDIF

SELECT PKBINLOC
SET ORDER TO &lcCuOrdr 
*!**************************************************************************
*! Name      : lfDISBINLC
*! Developer : Mohamed Shokry (MHM)
*! Date      : 07/02/2006
*! Purpose   : Disable bin check box in case of inventory locking
*!**************************************************************************
*! Example   : = lfDISBINLC()
*!**************************************************************************
*! Due to ,1
*!**************************************************************************
*B130752,1 MHM 08/02/2006 
FUNCTION lfDISBINLC	

SHOW GET lnBin DISABLE

*:*************************************************************
*! Name        : lfRMCKVDBN
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/14/2004
*! Module      : AR
*! Purpose     : Validate void credit memo
*:*************************************************************
*C037816,1 MHM 04/06/2004 Define New Menu[Start]
FUNCTION lfRMCKVDBN

llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
*T20061016.0001 TMI [Start] consider the case if the option is not installed at all 
llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
*T20061016.0001 TMI [End  ] 

IF !llUseBin
  RETURN .F.
ENDIF

STORE .F. TO llMessage
STORE '' TO lcMsgstr
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
IF !USED('RMBINLOC')
  =gfOpenFile(gcDataDir+'RMBINLOC','RMBINCRM','SH') 
ENDIF

*B131267,1 NNA 06/22/2006 (Begin) asking for Return style if a style returned to second or damage
*IF SEEK(RETHDR.CRMEMO+RETLINE.Style+RETHDR.cWareCode ,'RMBINLOC')
*  SELECT RMBINLOC
*  SCAN REST WHILE crmemo+style+cwarecode+clocation = RETHDR.CRMEMO+RETLINE.Style+RETHDR.cWareCode
STORE "" TO lcRetStyle
lcRetStyle = IIF(!EMPTY(RetLine.cRetSty),RetLine.cRetSty,RetLine.Style)
IF SEEK(RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode ,'RMBINLOC')
  SELECT RMBINLOC
  SCAN REST WHILE crmemo+style+cwarecode+clocation = RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode
*B131267,1 NNA (End)

    IF SEEK(RMBINLOC.Style+ RMBINLOC.cWareCode + RMBINLOC.cLocation ,'WHBINLOC') 
      SELECT WHBINLOC
      FOR lnCurCont = 1 TO 8
        lcCurCont = ALLTRIM(STR(lnCurCont))
        IF Qty&lcCurCont < RmBinLoc.Qty&lcCurCont
          llMessage = .T.
          lcMsgstr  = lcMsgstr + "Size"+ lcCurCont + " "
        ENDIF
      ENDFOR
    ELSE
      llMessage = .T.
      FOR lnCurCont = 1 TO 8
        lcCurCont = ALLTRIM(STR(lnCurCont))
        IF !EMPTY(RMBINLOC.Qty&lcCurCont)
          lcMsgstr  = lcMsgstr + "Size"+ lcCurCont + " "
        ENDIF  
      ENDFOR
    ENDIF
    IF llMessage
      lcMsgstr  = "No enoght stock for bin "+ RMBINLOC.clocation + "Sizes " + lcMsgstr  
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsgstr)
    ENDIF
  ENDSCAN
ENDIF
llReturn= IIF(llMessage,.F.,.T.)
RETURN llReturn


*!***************************************************************************
*!* Name        : lfOpnFiles
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Purpose     : Open the Needed files for Bin Location System and Check if
*!*             : Setting (Use Bin Location) is yes or no
*!***************************************************************************
*!* Called from : SMDLRBL.PRG
*!***************************************************************************
*!* Parameters  : lcFilesExp --> Files that need to open.
*!*             : lcTages    --> Tags for the opened files.
*!*             : lcCurPik   --> Current Piktkt.
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfOpnFiles()
*!***************************************************************************
FUNCTION lfOpnFiles
PARAMETERS lcFilesExp,lcTages,lcCurPik
DIMENSION laOpnFiles[1],laOpnTages[1]
IF !gfGetMemVar('M_DLUSEBIN',gcAct_Comp) OR lcCurPik == "******"
  RETURN .F.
ENDIF
=GFSUBSTR(lcFilesExp,@laOpnFiles,',')
=GFSUBSTR(lcTages,@laOpnTages,',')
FOR I=1 TO ALEN(laOpnFiles,1)
  IF !USED(laOpnFiles[I])
    =gfOpenFile(gcDataDir+laOpnFiles[I],laOpnTages[I],'SH')
  ELSE
    SET ORDER TO TAG laOpnTages[I] IN laOpnFiles[I]
  ENDIF
ENDFOR
RETURN .T.
*-- End of Function lfOpnFiles.

*:**************************************************************************
*:* Name        : lfUPRPLNSH
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/03/2006
*:* Purpose     : Update the CREPLENISH field for GPS00
*:***************************************************************************
*:* Called from : lfvNew in ICLOCN.PRG
*:***************************************************************************
*C132754,1  
FUNCTION lfUPRPLNSH

  REPLACE CREPLENISH WITH 'Y'

*-- end of lfUPRPLNSH.

*:**************************************************************************
*:* Name        : lfTRMTE_CANL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/06/2006
*:* Purpose     : do not cancel order if it has picked qty
*:***************************************************************************
*:* Called from : lpDelScr in SOUPDATE.PRG
*:***************************************************************************
FUNCTION lfTRMTE_CANL
PRIVATE lnSlct
lnSlct = SELECT()

=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Some order lines have been picked, cannot proceed!')

*- Release ordhdr lock
SELECT ORDHDR
=gfObj_Lock(.F.)

SELECT (lnSlct)
*-- end of lfTRMTE_CANL.