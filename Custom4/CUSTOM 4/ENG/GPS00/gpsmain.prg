*!**************************************************************************
*! Name      : GPSMAIN.PRG
*! Developer : TMI (TAREK MOHAMMED IBRAHIM)
*! Date      : 10/02/2007
*! Purpose   : GPS Custom Process Program.
*!  C200869  ==> for Aria4  attachments
*!  C200882  ==> for Aria27 attachments
*! Ticket id *T20070703.0001
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*! Modifications:
*! C200906,1 MMT 01/15/2008 Convert Shipment cost sheet to Aria4xp[T20061128.0001]
*! C201030,1 MMT 07/21/2008 Change label of complete date in PO screen[T20061128.0018]
*! C201079,1 MMT 12/02/2008 Add disribution button to Ap invoice screen[T20080829.0003]
*! C201079,2 MMT 01/11/2009 Fix bug of Negative cost in some lines[End]
*! B608982,1 TMI 08/25/2009 LCONTRACT variable not found [T20070214.0006]
*! C201384,1 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057]
*! B609930,1 SAB 05/21/2012 Fix bug of not updating StyInvJL if invoice has default style[T20120518.0035]
*! B609930,2 MMT 08/06/2012 Fix bug of not Error 'StyInvJL is not found' at R&M[T20120130.0001]
*! C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [T20121004.0002]
*! C201539,2 SAB 12/18/2012 Add New option Merge by season [T20121217.0001]
*! C201543,1 MMT 12/19/2012 Add option to Direct invoice screen Scan Style barcodes[T20121218.0008]
*! B610201,1 HIA 01/17/2013 Ar Direct invoicing not checking bin locatuion stock  [T20130108.0012]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:**************************************************************************
*:* Name        : lfCSTPRICE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/02/2007
*:* Purpose     : Calling the CSTPRICE gps custom screen
*:***************************************************************************
*:* Called from : ICSTYLE.SCX
*:***************************************************************************
*T20070821.0033 TMI
FUNCTION lfCSTPRICE
LOCAL lnCntBar
lnCntBar = CNTBAR('_lPopOpt')+1
DEFINE BAR lnCntBar OF _lPopOpt PROMPT '\<Enter Customer Prices' SKIP FOR ;
  _SCREEN.ACTIVEFORM.PARENT.ActiveMode <> 'E' .OR. ;
  _SCREEN.ACTIVEFORM.PARENT.llAllColors .OR. ;
  _SCREEN.ACTIVEFORM.PARENT.llAllScales

ON SELECTION BAR lnCntBar OF _lPopOpt DO lfOpnCstPrice

*-- end of lfCSTPRICE.

*:**************************************************************************
*:* Name        : lfOpnCstPrice
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/02/2007
*:* Purpose     : Calling the cstprice screen
*:***************************************************************************
FUNCTION lfOpnCstPrice

*- Create a temp File to hold more then 10 sizes. [Begin]
lcTempSizs = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir+lcTempSizs);
  ( cSequnc C(6) , ;
  cFit1 C(5),cFit2 C(5),cFit3 C(5),cFit4 C(5),cFit5 C(5),cFit6 C(5),cFit7 C(5),cFit8 C(5),cFit9 C(5),cFit10 C(5),;
  cSize1 c(15), cSize2 c(15), cSize3 c(15), cSize4 c(15), cSize5 c(15) ,;
  cSize6 c(15), cSize7 c(15), cSize8 c(15), cSize9 c(15), cSize10 c(15),;
  nPrice1 N(6,2), nPrice2 N(6,2), nPrice3 N(6,2), nPrice4 N(6,2), nPrice5 N(6,2) ,;
  nPrice6 N(6,2), nPrice7 N(6,2), nPrice8 N(6,2), nPrice9 N(6,2), nPrice10 N(6,2),;
  nComm1 N(6,2), nComm2 N(6,2), nComm3 N(6,2), nComm4 N(6,2), nComm5  N(6,2), ;
  nComm6 N(6,2), nComm7 N(6,2), nComm8 N(6,2), nComm9 N(6,2), nComm10 N(6,2))

lcTmpSzFit = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir+lcTmpSzFit);
  ( SCALE C(3),CDIM1 C(5),;
  Price1 N(7,2), Price2 N(7,2), Price3 N(7,2), Price4 N(7,2), Price5 N(7,2), Price6 N(7,2), Price7 N(7,2), Price8 N(7,2),;
  oldPrice1 N(7,2), oldPrice2 N(7,2), oldPrice3 N(7,2), oldPrice4 N(7,2), oldPrice5 N(7,2), oldPrice6 N(7,2), oldPrice7 N(7,2), oldPrice8 N(7,2))
INDEX ON SCALE TAG &lcTmpSzFit
lcSizSeq = '1'

*- open needed tables
=lfOpenTbls()

*-- Get Sizes
=lfSizes()

*- Define variables hold color len and position
PRIVATE lnClrPos,lnClrLen
STORE 0  TO lnClrPos,lnClrLen
*T20071119.0006 TMI [Start] Get the Size Len and Pos in the STYLE.STYLE field
STORE 0 TO lnSizePos,lnSizeLen
*T20071119.0006 TMI [End  ]
=lfGetClrD()

lnStyleWid  = _SCREEN.ACTIVEFORM.PARENT.lnStyleWid
lnColorWid1 = _SCREEN.ACTIVEFORM.PARENT.lnColorWid1  && ( = lnClrLen )

lcSepart = _SCREEN.ACTIVEFORM.PARENT.lcSepart
lcMajor  = PADR(_SCREEN.ACTIVEFORM.PARENT.Ariaform1.kbStyleMajor.Keytextbox.VALUE,lnStyleWid)
lcNonMjr = PADR(_SCREEN.ACTIVEFORM.PARENT.Ariaform1.kbNonMajor.Keytextbox.VALUE,lnColorWid1)

PUSH KEY

*- open the cstprice screen
DO FORM (oAriaApplication.ScreenHome+'\IC\CSTPRICE.SCX') WITH _SCREEN.ACTIVEFORM.PARENT

POP KEY

*:**************************************************************************
*:* Name        : lfSizes
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/14/2007
*:* Purpose     : Call the Customer Prices screen.
*:***************************************************************************
FUNCTION lfSizes

LOCAL lnAlias
lnAlias = SELECT(0)

lnCont = 1
lnScaleLen = _SCREEN.ACTIVEFORM.PARENT.lnScaleLen
lcScale = ALLTRIM(_SCREEN.ACTIVEFORM.pgfStyleInfo.Page1.kbScale.Keytextbox.VALUE)
lcScale = PADR(lcScale,lnScaleLen )

SELECT SCALE
IF gfSEEK('S'+lcScale)
  SELECT (lcTempSizs)
  APPEND BLANK
  REPLACE &lcTempSizs..cSequnc WITH lcSizSeq

  SELECT SCALE
  =SEEK('S'+lcScale)
  SCAN REST WHILE TYPE+SCALE+prepak = 'S' + lcScale
    IF lnCont = 11
      SELECT (lcTempSizs)
      APPEND BLANK
      lnCont = 1
      lcSizSeq = LTRIM(STR(VAL(lcSizSeq) + 1))
    ENDIF

    lcCnt  = ALLT(STR(SCALE.CNT))
    lcCont = ALLT(STR(lnCont))
    lcsZ&lcCont = ALLT(ALLT(SCALE.SZ1) + '-' + SCALE.SZ&lcCnt)
    REPLACE &lcTempSizs..cSize&lcCont WITH lcsZ&lcCont ;
      &lcTempSizs..cFit&lcCont  WITH SCALE.CDIM1
    lnCont = lnCont + 1

    *-- Add a line for each line in the scale file
    INSERT INTO &lcTmpSzFit ( SCALE, CDIM1) VALUES ( SCALE.SCALE, SCALE.CDIM1 )

  ENDSCAN
ENDIF

SELECT (lnAlias)

*:**************************************************************************
*:* Name        : lfvPrCode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/07/2007
*:* Purpose     : Valid function for price codes
*:***************************************************************************
*:* Called from : user definable fields for price codes in customer screen
*:***************************************************************************
FUNCTION lfvPrCode
PARAMETERS llReturn,lcFldNo
LOCAL lnSlct
lnSlct = SELECT()

llReturn = .T.
PRIVATE lcPCode,lnIndx,lnI,lcI

IF TYPE('laOgFxFlt') = 'C' AND !EMPTY(laOgFxFlt)
  && When OK is pressed on OG , the array laOgFxFlt is released
  && so ignor running this code at OG-Ok pressed,Use it only in normal case when focus is changed of fields

  lnIndx = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'PRICCODE'+lcFldNo),1)
  lcPCode = laOgFxFlt[lnIndx,6]
  IF !EMPTY(lcPCode)

    *-- Open files if not opened
    IF !USED('CSTPRICH')
      =gfOpenTable(oAriaApplication.DataDir+'CSTPRICH','CSTPRICH','SH')
      SELECT CSTPRICH
      =gfSeek('')
    ENDIF

    *-- Check that this is a valid price code
    IF !SEEK(lcPCode,'CSTPRICH')
      laBrowArr = ''            && empty the array used in browse
      =lfPCodBrow(@lcPCode)
    ENDIF

    IF !EMPTY(lcPCode)
      *-- Check that this code is not repeated
      FOR lnI = 1 TO 15
        IF lnI <> IIF(EMPTY(lcFldNo),1,VAL(lcFldNo))
          lcI = IIF(lnI=1,'',PADL(lnI,2,'0'))
          lnIndx = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'PRICCODE'+lcI),1)
          IF !EMPTY(laOgFxFlt[lnIndx,6]) .AND. lcPCode = laOgFxFlt[lnIndx,6]
            =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              'This value is already entered for Price Code '+ALLT(STR(lnI))+'.')
            lcPCode = ''
            EXIT
          ENDIF

        ENDIF
      ENDFOR
    ENDIF

    *-- Update the user definable field
    lnIndx = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'PRICCODE'+lcFldNo),1)
    loOgScroll.laOgFxFlt[lnIndx,6] = lcPCode
  ENDIF

ENDIF

SELECT (lnSlct)
RETURN llReturn
*-- end of lfvPrCode.

*:**************************************************************************
*:* Name        : lfPCodBrow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/07/2007
*:* Purpose     : Valid function for price codes
*:***************************************************************************
*:* Called from : user definable fields for price codes in customer screen
*:***************************************************************************
FUNCTION lfPCodBrow
PARAMETERS lcPCode
LOCAL lnSlct
lnSlct = SELECT(0)

DO CASE
CASE !EMPTY(lcPCode) .AND. SEEK(lcPCode,'CSTPRICH')
  RETURN
OTHERWISE

  SELECT CSTPRICH
  LOCATE

  DIMENSION laTemp[1]
  laTemp = ''

  llStyle = .F.
  lcFile_Ttl = 'Price Codes'
  *T20071119.0006 TMI [Start]
  *lcBrFields = "Priccode:H='Price Code':R , ccurrcod:H='Currency Code':R"
  lcBrFields = "Priccode:H='Price Code':R , ccurrcod:H='Currency Code':R, Style:R:18"
  *T20071119.0006 TMI [End  ]
  =gfBrows([''],'PRICCODE,CCURRCOD','laTemp',lcFile_Ttl)
  lcPCode = laTemp[1]

ENDCASE
SELECT(lnSlct)

*:**************************************************************************
*:* Name        : lfUPPRCLNS
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/14/2007
*:* Purpose     : Update the prices if called from packs or template screens
*:***************************************************************************
FUNCTION lfUPPRCLNS
LOCAL lnSlct
lnSlct = SELECT (0)


llSoldOut = gfGetMemVar('M_CMP2SOLD')
lcDateTyp = gfGetMemVar('M_CHKPRIAG')

*- Open needed table if not opened
=lfOpenTbls()

lcPricCursr = gfTempName()
lcRecno = gfTempName()

lcDetailFile = loFormSet.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.DetailFile
SELECT &lcDetailFile
SELECT RECNO() AS RECNO FROM (DBF(lcDetailFile)) WHERE EMPTY(CSTSZPRICE)INTO CURSOR &lcRecno

STORE 0  TO lnClrPos,lnClrLen
=lfGetClrD()

LOCAL llRejected
SELECT &lcRecno
LOCATE
SCAN
  SELECT &lcDetailFile
  GOTO (&lcRecno..RECNO)
  llRejected = .F.
  IF !lfChkAccSt(.T.,ACCOUNT,STORE) &&check that the style.style is located correctly
    llRejected = .T.
    DELETE
    LOOP
  ENDIF
  =IIF(lcDateTyp<>'N',lfCodPrice(.T.),'')
ENDSCAN

*- Update hdr file
IF RECCOUNT(lcRecno)>0  && if no records , nothing have been done

  =lfUpdHdr()

  IF llRejected
    =gfModalGen('INM00000B00000','','','','Styles are reserved to other customers/stores will be removed.')
  ENDIF

ENDIF

USE IN &lcRecno

SELECT (lnSlct)
*-- end of lfUPPRCLNS.

*:***************************************************************************
*:* Example     :  = lfCodPrice()
*:***************************************************************************
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/07/2007
*:* Purpose     : Adding price lines to the detail file based on the price code assigned to the customer
*
*                          price/size spearation process

* First we get the price code for the current added style
* Take the non-zero qty of the current added line to &lcDetailFile.
* based on the price1,...,price8 make a group for each price
* add a line to lcDetailFile for each price
* Update the OrdHdr temp file with the new added lines and changed prices

NOTE filling the temp lcOrdline may come from several sources, you should check each of them
NOTE from the soord screen : This founction
NOTE from the Packs/template/extended size screens: the "lfUPPRCLNS" trigger
*:***************************************************************************
FUNCTION lfCodPrice
PARAMETERS llFrmMain
*llFrmMain : if .t. this means that the function is called from within another function in this main file , not as a separate triggerLOCAL lnSlct
lnSlct = SELECT()

*-if called from tamplete screen then exit, the lines came from this place will be updated using the "UPPRCLNS" trigger
IF TYPE('loFormSet.AriaForm1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value')='U'
  RETURN
ENDIF

IF !llFrmMain
  lcDateTyp = gfGetMemVar('M_CHKPRIAG')
  IF lcDateTyp = 'N'
    RETURN
  ENDIF

  llSoldOut = gfGetMemVar('M_CMP2SOLD')

  *- Open needed table if not opened
  =lfOpenTbls()

  lcPricCursr = gfTempName()

  STORE 0  TO lnClrPos,lnClrLen
  =lfGetClrD()
ENDIF


lcDetailFile = loFormSet.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.DetailFile
SELECT &lcDetailFile
IF llSoldOut AND !EMPTY(STYLE.SOLDOUT)
  REPLACE &lcDetailFile..COMPLETE WITH STYLE.SOLDOUT
  loFormSet.AriaForm1.Ariapageframe1.Page1.txtComplete.Text1.VALUE = STYLE.SOLDOUT
ENDIF

*B608982,1 TMI [start] Remove the LCONTRACT field as I found no trace of it in sydfield or sydflfld
*IF &lcDetailFile..LCONTRACT  .OR. ;
*   &lcDetailFile..TOTQTY = 0 .OR. ;
*   !EMPTY(&lcDetailFile..CSTSZPRICE)
IF &lcDetailFile..TOTQTY = 0 .OR. ;
    !EMPTY(&lcDetailFile..CSTSZPRICE)
  *B608982,1 TMI [End] Remove the LCONTRACT field as I found no trace of it in sydfield or sydflfld
  RETURN
ENDIF

*- Get the date to check the valid code price date when compared with
* This is based on a setup option
DO CASE
CASE lcDateTyp = 'E'   && Entered
  ldChkDate = loFormSet.AriaForm1.Ariapageframe1.Page1.txtEntered.Text1.VALUE

CASE lcDateTyp = 'S'   && Start
  ldChkDate = loFormSet.AriaForm1.Ariapageframe1.Page1.txtStart.Text1.VALUE

CASE lcDateTyp = 'C'   && Complete
  ldChkDate = loFormSet.AriaForm1.Ariapageframe1.Page1.txtComplete.Text1.VALUE

ENDCASE

LOCAL lcPricCode
*-- loop here on sizes to gather them into price-groups

*- Get the price code
lcCurrCode = loFormSet.AriaForm1.Ariapageframe1.Page1.keyCurrency.Keytextbox.VALUE
lcPricCode = lfPRICCODE()

*- Get the currency code
IF SEEK(lcPricCode+lcCurrCode+&lcDetailFile..STYLE,'CSTPRICE')
  CREATE CURSOR &lcPricCursr (SIZES C(8),Gros_Price N(12,2),COMMDV N(12,2),;
    QTY1 N(6),QTY2 N(6),QTY3 N(6),QTY4 N(6),QTY5 N(6),QTY6 N(6),QTY7 N(6),QTY8 N(6),TOTQTY N(6))
  INDEX ON Gros_Price TAG Gros_Price
  LOCAL lnI,lcI
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    IF !EMPTY(&lcDetailFile..QTY&lcI)
      *- use the special size price , if it is 0 use the special scale price ( FOR DIR03 )
      lnGrssPric = IIF(CSTPRICE.PRICE&lcI<>0,CSTPRICE.PRICE&lcI,;
        IIF(CSTPRICE.PRICEDV<>0  ,CSTPRICE.PRICEDV,  ;
        &lcDetailFile..GROS_PRICE))
      lnStyComm  = IIF(STYLE.COMMISSION,CSTPRICE.COMMDV,0)

      *- Add a line per price to the temp ordline file
      IF !SEEK(lnGrssPric,lcPricCursr)
        INSERT INTO &lcPricCursr (SIZES,Gros_Price,COMMDV,QTY&lcI,TOTQTY) VALUES (lcI,lnGrssPric,lnStyComm,&lcDetailFile..QTY&lcI,&lcDetailFile..QTY&lcI)
      ELSE
        SELECT &lcPricCursr
        REPLACE QTY&lcI WITH &lcDetailFile..QTY&lcI ;
          TOTQTY  WITH TOTQTY + QTY&lcI       ;
          SIZES   WITH ALLTRIM(SIZES)+lcI
      ENDIF
    ENDIF
  ENDFOR

  lcRep1 = loFormSet.AriaForm1.Ariapageframe1.Page1.keySalesRep1.Keytextbox.VALUE
  lcRep2 = loFormSet.AriaForm1.Ariapageframe1.Page1.keySalesRep2.Keytextbox.VALUE

  LOCAL lcOrdHdr
  lcOrdHdr = loFormSet.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.Headerfile

  *- save the current line data to memo variables and Delete it.
  lnLnCnt = 1
  SELECT &lcDetailFile
  SCATTER MEMVAR MEMO FIELDS EXCEPT QTY*

  SELECT &lcPricCursr
  SCAN
    SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,GROS_PRICE MEMVAR
    SELECT &lcDetailFile

    && if only one price group then just update the line , otherwise add extra lines for groups other than the first one
    IF lnLnCnt>1
      APPEND BLANK
      SELECT &lcOrdHdr
      REPLACE LASTLINE WITH LASTLINE + 1
      m.LINENO = &lcOrdHdr..LASTLINE
      SELECT &lcDetailFile
    ENDIF

    GATHER MEMVAR MEMO
    REPLACE PRICE  WITH GROS_PRICE*(100-DISC_PCNT)/100 ;
      COMM1  WITH IIF(!EMPTY(lcRep1),&lcPricCursr..COMMDV,COMM1) ;
      COMM2  WITH IIF(!EMPTY(lcRep2),&lcPricCursr..COMMDV,COMM2) ;
      COWNER  WITH &lcPricCursr..SIZES ;
      BOOK1   WITH QTY1 ;
      BOOK2   WITH QTY2 ;
      BOOK3   WITH QTY3 ;
      BOOK4   WITH QTY4 ;
      BOOK5   WITH QTY5 ;
      BOOK6   WITH QTY6 ;
      BOOK7   WITH QTY7 ;
      BOOK8   WITH QTY8 ;
      TOTBOOK WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8 ;
      CSTSZPRICE WITH &lcPricCursr..SIZES
    lnLnCnt = lnLnCnt + 1

  ENDSCAN

  *- Release the temp cursor
  USE IN &lcPricCursr

  *- update the header only if this function is called from the trigger
  =IIF(llFrmMain,'',lfUpdHdr())

ENDIF

SELECT (lnSlct)
RETURN
*-- end of lfCodPrice.

*:**************************************************************************
*:* Name        : lfUpdHdr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/14/2007
*:* Purpose     : Update the lcOrdHdr file
*:***************************************************************************
FUNCTION lfUpdHdr

LOCAL lnLastLine,lnOpenQty,lnOpenPrice,lnBookQty,lnBookAmt
SELECT &lcDetailFile
LOCATE
CALCULATE SUM(TOTQTY),SUM(TOTQTY*PRICE),SUM(TOTBOOK),SUM(TOTBOOK*PRICE) TO lnOpenQty,lnOpenPrice,lnBookQty,lnBookAmt
GO BOTTOM
SELECT (loFormSet.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.Headerfile)
REPLACE OPEN     WITH lnOpenQty  ;
  OPENAMT  WITH lnOpenPrice ;
  BOOK     WITH lnBookQty ;
  BOOKAMT  WITH lnBookAmt

*-- end of lfUpdHdr.

*:**************************************************************************
*:* Name        : lfPRICCODE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/02/2003
*:* Purpose     : get Customer Price code
*:***************************************************************************
FUNCTION lfPRICCODE
LOCAL lcSlct,lcPricCode,lcSvKey
lcSlct = SELECT()
lcPricCode = ''

SELECT CUSTOMER
lcAccount = loFormSet.AriaForm1.keyAccount.Keytextbox.VALUE

lcSvKey = EVALUATE(KEY())
=gfSEEK('M'+lcAccount,'CUSTOMER')

LOCAL lnI,lcI,lcNoDtPCod

*-- Loop through all price codes
*-   pick the first price code with suitable date,
*-   if no one, pick the first one with no valid dates , otherwise return chr(255)
lcNoDtPCod = CHR(255)

*- Download needed lines to seek in from cstprice and cstprich sql files
SELECT CSTPRICE
=gfSetOrder('CSTYCODE')
=gfSEEK(&lcDetailFile..STYLE,'CSTPRICE')
=gfSetOrder('CSTPRICE')

PRIVATE lcStyClr
lcStyClr = PADR(SUBSTR(&lcDetailFile..STYLE,1,lnClrPos+lnClrLen-1),19)
SELECT CSTPRICH
=gfSetOrder('STYLE')
=gfSEEK(lcStyClr,'CSTPRICH')
=gfSetOrder('CSTPRICH')

FOR lnI = 1 TO 15
  lcI = IIF(lnI = 1 , '' , PADL(lnI,2,'0') )

  IF !EMPTY(CUSTOMER.PRICCODE&lcI).AND.SEEK(CUSTOMER.PRICCODE&lcI+lcCurrCode+&lcDetailFile..STYLE,'CSTPRICE') ;
      .AND.SEEK(CUSTOMER.PRICCODE&lcI+lcCurrCode+lcStyClr,'CSTPRICH')
    IF EMPTY(CSTPRICH.DVLDPRTO)
      *- Get no valid date price code
      lcNoDtPCod = UPPER(CUSTOMER.PRICCODE&lcI)
    ELSE
      *- Compare valid  prices for Banach based on setup ( Entered, Start or Complete Dates)
      IF BETWEEN(ldChkDate,CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)
        EXIT
      ENDIF

    ENDIF
  ENDIF
ENDFOR

lcPRICCODE = IIF(lnI < 16 , UPPER(CUSTOMER.PRICCODE&lcI) , lcNoDtPCod )

*- restore customer record
=gfSeek(lcSvKey,'CUSTOMER')

SELECT (lcSlct)
RETURN lcPRICCODE
*-- end of lfPRICCODE.

*:**************************************************************************
*:* Name        : lfWRNSLDEN
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/11/2007
*:* Purpose     : Warn of Sold Out Dates
*:***************************************************************************
*:* Called from : soord.scx
*:***************************************************************************
FUNCTION lfWRNSLDEN
LOCAL llValidate

*T20071119.0006 TMI [Start] only check if sold out date is not empty
IF EMPTY(STYLE.SOLDOUT)
  RETURN
ENDIF
*T20071119.0006 TMI [End  ]

llValidate = .T.
llWRNSLDEN = gfGetMemVar('M_WRNSLDEN')
IF llWRNSLDEN
  ldEntered = loFormSet.AriaForm1.Ariapageframe1.Page1.txtEntered.Text1.VALUE
  IF  ldEntered > STYLE.SOLDOUT
    llValidate = gfModalGen('INM00000B34012','','','',"Order Entered Date is greater than the Style Sold Out Date") = 1
  ENDIF
ENDIF
RETURN llValidate
*-- end of lfWRNSLDEN.

*:**************************************************************************
*:* Name        : lfSHWCSTNT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/11/2007
*:* Purpose     : Show Customer Order Notes at Sales Order Entry ?
*:***************************************************************************
FUNCTION lfSHOWNTS
LOCAL lnSlct,loNotePad
lnSlct = SELECT(0)
IF CUSTOMER.LSHWORDNT
  loNotePad = CREATEOBJECT('NOTEPAD',loFormSet.AriaForm1)
  =loNotePad.DO('A',loFormSet.Ariaform1.keyAccount.Keytextbox.VALUE)
ENDIF
SELECT (lnSlct)
*-- end of lfSHWCSTNT.

*:**************************************************************************
*:* Name        : lfVCHKCUST
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/22/2004
*:* Purpose     : Valid funciton for the custom field "STYLE.CCHKCUSTMR" and "STYLE.CCHKSTORE"
*:***************************************************************************
FUNCTION lfVCHKCUST
PARAMETERS llReturn,lcType
LOCAL lnSlct,lnAcc,lnSto
lnSlct = SELECT(0)
llReturn = .T.

IF TYPE('laOgFxFlt') = 'C' AND !EMPTY(laOgFxFlt)
  && When OK is pressed on OG , the array laOgFxFlt is released
  && so ignor running this code at OG-Ok pressed,Use it only in normal case when focus is changed of fields
  IF !USED('CUSTOMER')
    =gfOpenTable(oAriaApplication.DataDir+'CUSTOMER','CUSTOMER','SH')
  ENDIF

  lnAcc = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'CCHKCUSTMR'),1)
  lcAcc = PADR(laOgFxFlt[lnAcc,6],5)
  lnSto = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'CCHKSTORE'),1)
  lcSto = PADR(laOgFxFlt[lnSto,6],8)
  DO CASE
  CASE lcType = 'M'

    IF !EMPTY(lcAcc)

      IF !gfSEEK('M'+lcAcc,'CUSTOMER')
        DO CUSBROWM WITH lcAcc
        lcSto = ''
      ELSE
        IF !gfSEEK('S'+lcAcc+lcSto,'CUSTOMER')
          lcSto = ''
        ENDIF
      ENDIF

    ELSE
      lcSto = ''
    ENDIF

  CASE lcType = 'S'

    IF EMPTY(lcAcc)
      lcSto = ''
    ELSE

      IF !EMPTY(lcSto)
        IF !gfSEEK('S'+lcAcc+lcSto,'CUSTOMER')
          xStore   = lcSto
          IF !CUSBROWS(lcAcc,.T.)
            STORE SPACE(8) TO xStore
          ENDIF
          lcSto = xStore
        ENDIF
      ENDIF

    ENDIF

  ENDCASE
  loOgScroll.laOgFxFlt[lnAcc,6] = lcAcc
  loOgScroll.laOgFxFlt[lnSto,6] = lcSto

ENDIF

SELECT (lnSlct)
RETURN llReturn
*-- end of lfVCHKCUST.

*:**************************************************************************
*:* Name        : lfChkAccSt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/13/2007
*:* Purpose     : Check if the style customer saved in &lcTmpSty..CCHKACCNT match
*               : selected Account/Store in sales order
*:***************************************************************************
FUNCTION lfChkAccSt
PARAMETERS llFrmMain,lcAccount,lcStore
LOCAL lcAcc,lcSto
lcAcc = IIF(llFrmMain,lcAccount,loFormSet.Ariaform1.keyAccount.Keytextbox.VALUE)
lcSto = IIF(llFrmMain,lcStore  ,loFormset.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.keyStore.Keytextbox.VALUE)

IF !EMPTY(STYLE.CCHKCUSTMR)
  IF lcAcc <> STYLE.CCHKCUSTMR
    IF !llFrmMain
      =gfModalGen('INM00000B00000','','','','This style is reserved for customer '+STYLE.CCHKCUSTMR+' and you cannot proceed.')
    ENDIF
    RETURN .F.
  ELSE
    IF !EMPTY(STYLE.CCHKSTORE) .AND. ;
        lcSto <> STYLE.CCHKSTORE
      IF !llFrmMain
        =gfModalGen('INM00000B00000','','','','This style is reserved for customer/store '+STYLE.CCHKCUSTMR+'/'+ALLTRIM(STYLE.CCHKSTORE)+' and you cannot proceed.')
      ENDIF
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
*-- end of lfChkAccSto.

*:**************************************************************************
*:* Name        : lfCHKACCS2
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/14/2007
*:* Purpose     : Valid fn called if customer changes store after enetring the style
*:***************************************************************************
FUNCTION lfCHKACCS2
LOCAL llValid , lOb
IF loFormSet.Ariaform1.Ariapageframe1.Page2.grdEditLines.STYLE.keyStyle.VALUE <> ' '
  llValid = lfChkAccSt()
  IF !llValid
    lOb = loFormSet.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.keyStore.Keytextbox
    lOb.VALUE = lOb.OLDValue
  ENDIF
ENDIF
*-- end of lfCHKACCS2.

*:**************************************************************************
*:* Name        : lfCSTSZPRI
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/12/2007
*:* Purpose     : return .F. in the When method for txtQty1,...,8 fields in Soord
*               : screen if this line has entered with custom price
*:***************************************************************************
FUNCTION lfCSTSZPRI
LOCAL lcI,lcDetFl
lcI = RIGHT(loCSTSZPRI.NAME,1)
lcDetFl = loCSTSZPRI.PARENT.PARENT.detailfile
IF !EMPTY(&lcDetFl..CSTSZPRICE) .AND. !lcI $ &lcDetFl..CSTSZPRICE
  RETURN .F.
ENDIF
*-- end of lfCSTSZPRI.

*:**************************************************************************
*:* Name        : lfOpenTbls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/01/2007
*:* Purpose     : open needed tables
*:***************************************************************************
FUNCTION lfOpenTbls

IF !USED('SycCurr')
  =gfOpenTable(oAriaApplication.SysPath+'SycCurr',oAriaApplication.SysPath+'cCurrCode','SH')
ENDIF
IF !USED('CSTPRICH')
  =gfOpenTable(oAriaApplication.DataDir+'CSTPRICH','CSTPRICH','SH')
ENDIF
IF !USED('CSTPRICE')
  =gfOpenTable(oAriaApplication.DataDir+'CSTPRICE','CSTPRICE','SH')
ENDIF
IF !USED('STYLE')
  =gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
ENDIF
IF !USED('SCALE')
  =gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
ENDIF
*-- end of lfOpenTbls.

*:**************************************************************************
*:* Name        : lfGetClrD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2002
*:* Purpose     : Get Color Position and Color length
*:***************************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
PRIVATE lnCount &&Tmi 07/15/2002
lcOldSelect=SELECT()

=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])

  CASE laItemSeg[lnCount,1]='S'
    lnSizeLen = LEN(laItemSeg[lnCount,3])
    lnSizePos = laItemSeg[lnCount,4]
  ENDCASE
ENDFOR

SELECT(lcOldSelect)
*--end function lfGetClrD


*:**************************************************************************
*:* Name        : lfCLSFLS
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/21/2007
*:* Purpose     : Trigger to close CSTPRICE, CSTPRICH tables if opened
*:***************************************************************************
*:* Called from : icstyle.desctroy method
*:***************************************************************************
FUNCTION lfCLSFLS
IF USED('CSTPRICE')
  USE IN CSTPRICE
ENDIF
IF USED('CSTPRICH')
  USE IN CSTPRICH
ENDIF
*-- end of lfCLSFLS.
*! C200906,1 MMT 01/15/2008 Convert Shipment cost sheet to Aria4xp[Start]
*:**************************************************************************
*:* Name        : GPSDUTY
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/03/2008
*:* Purpose     : use the same exchnge rate for Paurchase price to the duty for GPS
*:***************************************************************************
*:* Called from : POACFRV.PRG
*:***************************************************************************
FUNCTION lfGPSDUTY
m.NDUTYRAT   = IIF(SHPRLFLD.NDUTYRAT = 0 , POSHD.NPRICERAT , SHPRLFLD.NDUTYRAT )

*-- end of GPSDUTY.

*:**************************************************************************
*:* Name        : lfDUTYSGNS
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/12/2005
*:* Purpose     :
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfDUTYSGNS()
*:***************************************************************************
FUNCTION lfDUTYSGNS

lcTmpSpLn = loFormSet.lcTmpSpLn
lcDPMethod = &lcTmpSpLn..CCURMETH
lcDPUnMeth = &lcTmpSpLn..CUNTMETH

*-- end of lfDUTYSGNS.
*:**************************************************************************
*:* Name        : lfGPSROYLT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 29/10/2005
*:* Purpose     :  Update royalty cost item for GPS00 from the Standard Royalty
*:***************************************************************************
FUNCTION lfGPSROYLT
PRIVATE lnPos,lcItm,lnSlct
lnSlct = SELECT()
lcTmpSpLn = loFormSet.lcTmpSpLn
SELECT &lcTmpSpLn
lnPos = ASCAN(loFormSet.laCostItem,'*ROYALT*')
IF lnPos > 0
  lnPos = ASUBSCRIPT(loFormSet.laCostItem,lnPos,1)
  lcItm = ALLTRIM(STR(lnPos-1))
  REPLACE COST&lcItm WITH NROYALTY
ENDIF
SELECT (lnSlct)
*:**************************************************************************
*:* Name        : lfSETDTYRT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/12/2005
*:* Purpose     : Set the duty rate division operator as the Paurchase price
*               : since the duty currency is ALWAYS the as the base currency for GPS
*:***************************************************************************
*:* Called from : POACFRV.PRG
*:***************************************************************************
FUNCTION lfSETDTYRT
REPLACE NLANDURAT WITH 1
*! C200906,1 MMT 01/15/2008 Convert Shipment cost sheet to Aria4xp[End]

*! C201030,1 MMT 07/21/2008 Change label of complete date in PO screen[Start]
*:**************************************************************************
*:* Name        : lfCHNGLBLDT
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 07/21/2008
*:* Purpose     : Change complete date label
*:***************************************************************************
FUNCTION lfCHNGLBLDT
IF ALLTRIM(gfGetMemVar('M_POCOMAVL'))='A'
  loFormSet.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.lblCompleteDate.CAPTION = 'Avail. Date'
ENDIF
*! C201030,1 MMT 07/21/2008 Change label of complete date in PO screen[End]

*! C201079,1 MMT 12/02/2008 Add disribution button to Ap invoice screen[Start]
*:**************************************************************************
*:* Name        : lfAUTODIST
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/02/2008
*:* Purpose     : Add button screen
*:***************************************************************************
FUNCTION lfAUTODIST
loAutoForm.ADDPROPERTY('lnDistAmt',0)
loAutoForm.AriaForm1.ADDOBJECT("cmdAutoDist",'gpsdistbutton')
loAutoForm.AriaForm1.cmdAutoDist.CAPTION = "\<Distribute"
loAutoForm.AriaForm1.cmdAutoDist.TOP = loAutoForm.AriaForm1.cmdProceed.TOP
loAutoForm.AriaForm1.cmdAutoDist.WIDTH = loAutoForm.AriaForm1.cmdProceed.WIDTH
loAutoForm.AriaForm1.cmdAutoDist.LEFT = 143
loAutoForm.AriaForm1.cmdProceed.LEFT = 298
loAutoForm.AriaForm1.cmdClose.LEFT = 453
loAutoForm.AriaForm1.cmdAutoDist.VISIBLE = .T.
loAutoForm.AriaForm1.cmdAutoDist.ENABLED = loAutoForm.AriaForm1.cmdProceed.ENABLED

*:**************************************************************************
*:* Name        : lfREFDISTBUT
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/02/2008
*:* Purpose     : refresh button State (Enabled,disabled)
*:***************************************************************************
FUNCTION lfREFDISTBUT

IF TYPE("loFormAuto.cmdAutoDist") = 'O'
  loFormAuto.cmdAutoDist.ENABLED = loFormAuto.cmdProceed.ENABLED
ENDIF
*:**************************************************************************
*:* Name        : lfVDistAmt
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 12/02/2008
*:* Purpose     : Disribute amount
*:***************************************************************************
FUNCTION lfVDistAmt
PARAMETERS loApFormSet,lnAmountToDist
lnSelect = SELECT(0)
SELECT (loApFormSet.lcAutoInv)
lnRecNum = RECNO()
lcOrderOld = ORDER()
SET ORDER TO TAG 'SELECT'
=SEEK("�")
lnTotInvQty = 0
SUM nInvQty REST WHILE cSelect+ITEM+cWareCode+cDyelot = "�" FOR !DELETED() TO lnTotInvQty

IF lnTotInvQty <> 0 AND lnAmountToDist <> 0
  *C201079,1 MMT 01/11/2009 Fix bug of Negative cost in some lines[Start]
  *lnUntCost = ROUND(lnAmountToDist/lnTotInvQty ,3)
  lnUntCost = lnAmountToDist/lnTotInvQty
  *C201079,1 MMT 01/11/2009 Fix bug of Negative cost in some lines[End]

  lnTOTDist  = lnTotInvQty
  lnTotAmtDist = 0
  SELECT (loApFormSet.lcAutoInv)
  LOCATE
  =SEEK("�")
  SCAN REST WHILE cSelect+ITEM+cWareCode+cDyelot = "�" FOR !DELETED()
    IF lnTOTDist = nInvQty
      lnUntCost = ROUND((lnAmountToDist - lnTotAmtDist)/nInvQty,3)
      REPLACE nTktCst WITH lnUntCost ,;
        nTktAmnt WITH ROUND((lnAmountToDist - lnTotAmtDist),2) ,;
        nAplAmnt WITH ROUND((lnAmountToDist - lnTotAmtDist),2) ,;
        nInvAmnt WITH ROUND((lnAmountToDist - lnTotAmtDist),2)

    ELSE
      REPLACE nTktCst WITH lnUntCost ,;
        nTktAmnt WITH ROUND(lnUntCost * nTktQty,2) ,;
        nAplAmnt WITH ROUND(lnUntCost * nAplQty,2) ,;
        nInvAmnt WITH ROUND(lnUntCost * nInvQty,2)
    ENDIF
    lnTOTDist = lnTOTDist - nInvQty
    lnTotAmtDist = lnTotAmtDist + nInvAmnt
  ENDSCAN
ENDIF
SELECT (loApFormSet.lcAutoInv)
SET ORDER TO (lcOrderOld )
IF BETWEEN(lnRecNum ,1,RECCOUNT())
  GO RECORD lnRecNum
ENDIF
SELECT(lnSelect)
*! C201079,1 MMT 12/02/2008 Add disribution button to Ap invoice screen[End]

*! C201384,1 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][Start]
*:**************************************************************************
*:* Name        : lfUPDCSTCN
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/29/2012
*:* Purpose     : Call gfStyCrl to Receive Qty
*:***************************************************************************
FUNCTION lfUPDCSTCN
IF TYPE('lcStyleDet') = 'C' AND TYPE('lcXmlData') = 'C'
  IF ALLTRIM(&lcDetFile..CADD_USER) == 'CONCESDEF'
    lcRISessn = ''
    lnOrgLine = m.LineNo
    *! B609930,1 SAB 05/21/2012 Fix bug of not updating StyInvJL if invoice has default style[T20120518.0035][Start]
    *! B609930,2 MMT 08/06/2012 Fix bug of not Error 'StyInvJL is not found' at R&M[T20120130.0001][Start]
    IF USED('StyInvjl')
      *! B609930,2 MMT 08/06/2012 Fix bug of not Error 'StyInvJL is not found' at R&M[T20120130.0001][End]
      LOCAL lnAlias
      lnAlias = SELECT()
      SELECT STYINVJL
      =gfTableUpdate()
      SELECT (lnAlias)
      *! B609930,1 SAB 05/21/2012 Fix bug of not updating StyInvJL if invoice has default style[T20120518.0035][End]
      *! B609930,2 MMT 08/06/2012 Fix bug of not Error 'StyInvJL is not found' at R&M[T20120130.0001][Start]
      *IF USED('StyInvjl')
      *! B609930,2 MMT 08/06/2012 Fix bug of not Error 'StyInvJL is not found' at R&M[T20120130.0001][End]
      =gfCloseTable('StyInvjl')
    ENDIF
    *! C201539,2 SAB 12/18/2012 Add New option Merge by season [Start]
    lcAdjReason = IIF(gfSeek("DCADJREASON",'Codes','CCODE_NO'),codes.ccode_no,'')
    lcAdjAcct = ' '
    IF llUse_GL AND !EMPTY(lcAdjReason)
      DECLARE laTrmRltFd[1,2]
      laTrmRltFd[1,1] = 'GLACCOUNT'
      laTrmRltFd[1,2] = 'lcAdjAcct'
      =gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
    ELSE
      lcAdjReason = ' '
    ENDIF

    llUse_GL = ALLTRIM(gfGetMemVar('M_Link_GL',oAriaApplication.ActiveCompanyID)) ='Y'
    lcGlFYear  = ''
    lcGlPeriod = ''
    =CHECKPRD(oAriaApplication.SystemDate,'lcGLFYear','lcGLPeriod','IA',.T.)

    IF llUse_GL
      =gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
      SELECT GLDist
      lcTmpGlDt = IIF(TYPE('lcDistFile') <> 'C',gfTempName(),lcDistFile)
      IF !USED(lcTmpGlDt)
        lcWorkDir = oAriaApplication.workdir
        COPY STRU TO (lcWorkDir+lcTmpGlDt+'.DBF')
        USE (lcWorkDir+lcTmpGlDt+'.DBF') IN 0 EXCLUSIVE
        SELECT (lcTmpGlDt)
      ENDIF
    ENDIF

    lcLinkCode = IIF(llUse_GL ,IIF(!EMPTY(STYLE.Link_Code),STYLE.Link_Code,'DEFDEF'),"")
    IF llUse_GL
      DECLARE laGLDistAr[2,13]
      laGLDistAr[1,1] = lcLinkCode
      laGLDistAr[2,1] = lcLinkCode
      laGLDistAr[1,2] = '006'
      laGLDistAr[2,2] = '007'
      laGLDistAr[1,3] = 1
      laGLDistAr[2,3] = -1
      STORE 'IA' TO laGLDistAr[1,4],laGLDistAr[2,4]
      STORE ''   TO laGLDistAr[1,5],laGLDistAr[2,5]
      STORE oAriaApplication.SystemDate TO laGLDistAr[1,6],laGLDistAr[2,6]
      STORE lcGlFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      STORE lcTmpGlDt  TO laGLDistAr[1,9],laGLDistAr[2,9]
      laGLDistAr[2,10] = lcAdjAcct
    ELSE
      DIME laGLDistAr[1,1]
      laGLDistAr = ''
    ENDIF
    *! C201539,2 SAB 12/18/2012 Add New option Merge by season [End]

    *! C201539,2 SAB 12/18/2012 Add New option Merge by season [Start]
    *=gfStyCrl('1',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,'',@laAdjStk,;
    &lcDetFile..Cost,'',@lcRISessn ,lcAdjReason,0,'','',@laGlArray,0,'','')
    =gfStyCrl('1',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,'',@laAdjStk,;
      &lcDetFile..Cost,'',@lcRISessn ,lcAdjReason,0,'','',@laGLDistAr,0,'','')
    *! C201539,2 SAB 12/18/2012 Add New option Merge by season [End]
    lfUpdateBinLocTables(m.Style,m.cWareCode,lcDefBin,lcRISessn)
    m.Cost = &lcDetFile..Cost
    lcRISessn = ''
    m.LineNo = lnOrgLine
  ENDIF
ENDIF
*:**************************************************************************
*:* Name        : lfUpdateBinLocTables
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/29/2012
*:* Purpose     : Update Bin location tables
*:***************************************************************************
FUNCTION lfUpdateBinLocTables
LPARAMETERS lcStyle,lcLocation,lcBin,lcGLSess
lcCurAlisSel = SELECT()
IF !USED('StyInvjl_bin')
  =gfOpenTable('StyInvjl','STYDATE','SH','StyInvjl_bin')
ENDIF
IF !USED('BININVJL_A')
  =gfOpenTable('BININVJL','WHBINSTY','SH','BININVJL_A')
ENDIF
LOCAL lnMaxLnNo
lnMaxLnNo = 0
SELECT 'StyInvjl_bin'
lcSessionID = lcGLSess
SET ORDER TO 'STYDATE' ASCENDING
=gfSeek(lcStyle+lcLocation)
SCAN WHILE STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE = lcStyle+lcLocation FOR CSESSION = lcSessionID
  SCATT MEMVAR MEMO
  m.clocation = lcBin
  SELECT BININVJL_A
  =lfIncLineNUM(@lnMaxLnNo)
  SELECT BININVJL_A
  =gfAppend('BININVJL_A',.T.)
ENDSCAN
SELECT  BININVJL_A
=gfTableUpdate()
=gfCloseTable('BININVJL_A')
=gfCloseTable('StyInvjl_bin')
SELECT(lcCurAlisSel)
*:**************************************************************************
*:* Name        : lfIncLineNUM
*:* Developer   : Mariam Mazhar[MMT]
*:* Date        : 01/29/2012
*:* Purpose     : used to increment the m.LINENO variable to keep the key of BININVJL
*:***************************************************************************
FUNCTION lfIncLineNUM
LPARAMETERS lnMaxLnNo
SELECT BININVJL_A
=gfSeek(m.CWARECODE+m.CLOCATION+m.STYLE)
LOCATE
SCAN
  IF CWARECODE+CLOCATION+STYLE+CTRCODE+cirtype+CSESSION+crsession+DTOS(DTRDATE) = ;
      m.CWARECODE+m.CLOCATION+m.STYLE+m.CTRCODE+m.cirtype+m.CSESSION+m.crsession+DTOS(m.DTRDATE)
    lnMaxLnNo = MAX(LINENO,lnMaxLnNo)
    m.LineNo = lnMaxLnNo+1
  ENDIF
ENDSCAN
*:**************************************************************************
*:* Name        : LFUPDCSTVA
*:* Developer   : Mariam Mazhar[MMT]
*:* Date        : 01/29/2012
*:* Purpose     : Update Cost memory variable
*:***************************************************************************
FUNCTION LFUPDCSTVA
IF TYPE('lcStyleDet') = 'C' AND TYPE('lcXmlData') = 'C'
  IF ALLTRIM(&lcDetFile..CADD_USER) == 'CONCESDEF'
    m.Cost = &lcDetFile..Cost
  ENDIF
ENDIF
*! C201384,1 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][End]

*! C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [Start]
*:**************************************************************************
*:* Name        : lfvStyle
*:* Developer   : Saber A Razek [SAB]
*:* Date        : 12/11/2012
*:* Purpose     : Valid function for Style browse
*:***************************************************************************
FUNCTION lfvStyle
PARAMETERS llRet

lnAlias    = SELECT(0)
lcControl  = loOGScroll.FocusControl
lcCurVar   = loOGScroll.&lcControl.
lcCurVal   = PADR(lcCurVar.VALUE, 19)

IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
ENDIF
SELECT STYLE
SET ORDER TO STYLE
IF !EMPTY(lcCurVal) AND !SEEK(lcCurVal,'STYLE','STYLE')
  SELECT STYLE
  DIMENSION laTempData[1]
  laTempData[1] = ''
  PRIVATE lcBrFields
  lcBrFields = [STYLE :30 :H='STYLE - COLOR', DESC :20 :H='Description', DESC1 :35 :H='Long Description', SEASON :20 :H='Season', CDIVISION :20 :H='Division', PRICEA :10 :H='Price A', PRICEB :10 :H='Price B', PRICEC :10 :H='Price C', TOTWIP :12 :H='WIP']
  lcBrFields = lcBrFields + [, TOTSTK :12 :H='Stock', TOTORD :12 :H='Orders', OTS=(TOTWIP+TOTSTK-TOTORD):12 :H='O.T.S.', CPRIFABRIC :15 :H='Fabric']
  llReturn = AriaBrow('', "Styles", .F., .F., .F., .F., '', '', 'Style', 'laTempData')
  lcCurVar.VALUE = laTempData[1]
  IF llReturn .AND. TYPE('loOgScroll') = 'O'
    loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = laTempData[1]
    =lfOGShowGet(lcCurVar.PARENT.nRowIndex)
  ENDIF
ENDIF

SELECT(lnAlias)
llRet= .T.
RETURN  llRet

ENDFUNC
*- End of lfvStyle


*:**************************************************************************
*:* Name        : lfvDStyle
*:* Developer   : Saber A Razek [SAB]
*:* Date        : 12/11/2012
*:* Purpose     : Valid function for Style browse
*:***************************************************************************
FUNCTION lfvDStyle
PARAMETERS llRet

lnAlias    = SELECT(0)
lcControl  = loOGScroll.FocusControl
lcCurVar   = loOGScroll.&lcControl.
lcCurVal   = PADR(lcCurVar.VALUE, 19)

SET STEP ON

IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
ENDIF
SELECT STYLE
SET ORDER TO STYLE
IF !EMPTY(lcCurVal) AND !SEEK(lcCurVal,'STYLE','STYLE')
  SELECT STYLE
  DIMENSION laTempData[1]
  laTempData[1] = ''
  PRIVATE lcBrFields
  lcBrFields = [STYLE :30 :H='STYLE - COLOR', DESC :20 :H='Description', DESC1 :35 :H='Long Description', SEASON :20 :H='Season', CDIVISION :20 :H='Division', PRICEA :10 :H='Price A', PRICEB :10 :H='Price B', PRICEC :10 :H='Price C', TOTWIP :12 :H='WIP']
  lcBrFields = lcBrFields + [, TOTSTK :12 :H='Stock', TOTORD :12 :H='Orders', OTS=(TOTWIP+TOTSTK-TOTORD):12 :H='O.T.S.', CPRIFABRIC :15 :H='Fabric']
  llReturn = AriaBrow(' FOR STYLE.CSTYGRADE = "3"', "Styles", .F., .F., .F., .F., '', '', 'Style', 'laTempData')
  lcCurVar.VALUE = laTempData[1]
  IF llReturn .AND. TYPE('loOgScroll') = 'O'
    loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = laTempData[1]
    =lfOGShowGet(lcCurVar.PARENT.nRowIndex)
  ENDIF
ENDIF

SELECT(lnAlias)
llRet= .T.
RETURN  llRet

ENDFUNC
*- End of lfvDStyle
*! C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [End]
*! C201543,1 MMT 12/19/2012 Add option to Direct invoice screen Scan Style barcodes[T20121218.0008][Start]
*!*************************************************************
*! Name      : lfValidateBrCd
*! Developer : Mariam Mazhar
*! Date      : 12/19/2012
*! Purpose   : Validate scanned barcode
*!*************************************************************
FUNCTION lfValidateBrCd
LPARAMETERS loparentformset ,lcUpcValue
SET DATASESSION TO (loparentformset.DATASESSIONID)
IF !USED("STYLEUPC")
  =gfOpenTable("STYLEUPC","STYUPCN")
ELSE
  SELECT STYLEUPC
  =gfSetOrder("STYUPCN")
ENDIF
lcHdrFile = loparentformset.lcInvhdr
SELECT STYLEUPC
IF gfSeek(lcUpcValue)
  lcSelStyle = STYLEUPC.STYLE
  lnSize     = VAL(STYLEUPC.SIZE)
  =SEEK(lcSelStyle,'STYLE','STYLE')
  IF STYLE.cDivision <> &lcHdrFile..cDivision
    *-- Message : 40009
    *-- Styles restricted to XXX!
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM40009B00000','ALERT',"division "+' '+ALLTRIM(gfCodDes(&lcHdrFile..cDivision,'CDIVISION')))
    RETURN
  ENDIF
  IF ALLTRIM(&lcHdrFile..Season)<>'*' AND TRIM(STYLE.Season)<>'Y' AND STYLE.Season <> &lcHdrFile..Season
    =gfModalGen('TRM40009B00000','ALERT',"season "+' '+ALLTRIM(gfCodDes(&lcHdrFile..Season,'SEASON')))
    RETURN
  ENDIF
    *! B610201,1 HIA 01/17/2013 Ar Direct invoicing not checking bin locatuion stock  [T20130108.0012][Start]
  IF lfIsUseBin() AND TYPE('loparentformset.lcDefBin')='C' AND !EMPTY(loparentformset.lcDefBin)
  lcX = STR(lnSize,1)
    IF !USED('WHBINLOC')
      = gfOpenTable( 'WHBINLOC', 'WHBINLOC', 'SH')
    ENDIF
    SELECT WHBINLOC

    lcFromWare = loparentformset.AriaForm1.AriaPageFrame1.Page1.cboWarehouse.VALUE
    lcKey =   lcFromWare+ loparentformset.lcDefBin + lcSelStyle

    IF !gfSeek(lcKey,'WHBINLOC') OR WHBINLOC.QTY&lcx <=0
      lcMsg = 'The style '+lcSelStyle+' has no stock in the Wh/Bin : ' + ALLTRIM(lcFromWare)+ ' / ' + ALLTRIM(loparentformset.lcDefBin) + '.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
      
      SELECT STYLEUPC
      RETURN
    ENDIF
    SELECT STYLEUPC
    
  ENDIF
  *! B610201,1 HIA 01/17/2013 Ar Direct invoicing not checking bin locatuion stock  [T20130108.0012][End]
  
  lcOldProcd = SET("Procedure")
  lcOldProcd  = (oAriaApplication.ApplicationHome + 'ar\ARCNINM.FXP') +IIF(!EMPTY(lcOldProcd),",",'')+lcOldProcd
  SET PROCEDURE TO &lcOldProcd.
  lnChoice = 1
  WITH  loparentformset.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1
    .mResetControlSource ()
    STORE .T. TO .llNewline,.llAddLine
    .keyStyle.ENABLED = .T.
    .keyStyle.TxtItem.VALUE = lcSelStyle
    .keyStyle.VALID(.T.,0,lcSelStyle ,SPACE(19),lcSelStyle )
    .txtQuantity.VALUE =1
    .txtQuantity.VALID()
    .mResetControlSource ()
    .cntQuantity.txtQty1.CONTROLSOURCE = ''
    .cntQuantity.txtQty2.CONTROLSOURCE = ''
    .cntQuantity.txtQty3.CONTROLSOURCE = ''
    .cntQuantity.txtQty4.CONTROLSOURCE = ''
    .cntQuantity.txtQty5.CONTROLSOURCE = ''
    .cntQuantity.txtQty6.CONTROLSOURCE = ''
    .cntQuantity.txtQty7.CONTROLSOURCE = ''
    .cntQuantity.txtQty8.CONTROLSOURCE = ''
    .cntQuantity.txtTotQty.CONTROLSOURCE = ''
    lcX = STR(lnSize,1)
    .cntQuantity.txtQty&lcX..VALUE  = 1
    .cntQuantity.txtQty&lcX..VALID
    .cntQuantity.LOSTFOCUS()
    IF lfIsUseBin() AND TYPE('loparentformset.lcDefBin')='C' AND !EMPTY(loparentformset.lcDefBin)
      REPLACE Binloc&lcX WITH loparentformset.lcDefBin IN (loparentformset.lcInvLine)
    ENDIF
  ENDWITH

  IF UPPER((oAriaApplication.ApplicationHome + 'ar\ARCNINM.FXP')) $ UPPER(SET("Procedure"))
    RELEASE PROCEDURE (oAriaApplication.ApplicationHome + 'ar\ARCNINM.FXP')
  ENDIF
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid UPC Number")
  RETURN
ENDIF
*!*************************************************************
*! Name      : lfARDSCNBR
*! Developer : Mariam Mazhar
*! Date      : 12/19/2012
*! Purpose   : add option to invoice screen
*!*************************************************************
FUNCTION lfARDSCNBR
lnCntBar = CNTBAR('_INQURYPOP')+1
DEFINE BAR lnCntBar OF _INQURYPOP PROMPT '\<Scan Style Barcode' SKIP FOR ;
  _SCREEN.ACTIVEFORM.PARENT.ActiveMode $ 'VSE' OR _SCREEN.ACTIVEFORM.PARENT.AriaForm1.AriaPageFrame1.ACTIVEPAGE <> 2
ON SELECTION BAR lnCntBar OF _INQURYPOP DO lfOpnScnBrSc
*!*************************************************************
*! Name      : lfOpnScnBrSc
*! Developer : Mariam Mazhar
*! Date      : 12/19/2012
*! Purpose   : Open Scan barcode screen
*!*************************************************************
FUNCTION lfOpnScnBrSc
IF TYPE('_Screen.ActiveForm.Parent.llUseScanOption')<> 'L'
  _SCREEN.ACTIVEFORM.PARENT.ADDPROPERTY("llUseScanOption",.T.)
ELSE
  _SCREEN.ACTIVEFORM.PARENT.llUseScanOption = .T.
ENDIF
IF lfIsUseBin() AND TYPE('_Screen.ActiveForm.Parent.lcDefBin')<>'C' &&AND !EMPTY(_Screen.ActiveForm.Parent.lcDefBin)
  IF !USED('WHSLOC')
    =gfOpenTable(oAriaApplication.DataDir+'WHSLOC','WHSLOC','SH')
  ENDIF
  lcSvOrd = ORDER('WHSLOC')
  SELECT WHSLOC
  =gfSetOrder('WHSLOC')
  lcWareCode = _SCREEN.ACTIVEFORM.PARENT.AriaForm1.AriaPageFrame1.Page1.cboWarehouse.VALUE
  lcWH = ALLTRIM(lcWareCode)
  lcKey = "PADR('"+lcWH+"',6) FOR EMPTY(Style)"
  lcFile_Ttl = lcWH+" Bins"
  lcBrFields = "clocation  :H='Bin',bn= cBinClass+'-'+CFLATHANG  :H='Bin Class/FH'"
  DIMENSION laTempData[3]
  STORE '' TO laTempData
  =AriaBrow(lcKey,'Bin Locations',.F.,.F.,.F.,.F.,'',.T.,'CLOCATION,CBINCLASS,CFLATHANG','laTempData')
  lcBin = laTempData[1]
  IF !EMPTY(lcBin)
    IF TYPE('_Screen.ActiveForm.Parent.lcDefBin')<> 'C'
      _SCREEN.ACTIVEFORM.PARENT.ADDPROPERTY("lcDefBin",lcBin)
    ELSE
      _SCREEN.ACTIVEFORM.PARENT.lcDefBin = lcBin
    ENDIF
  ENDIF
ENDIF
DO FORM (oAriaApplication.ScreenHome+'\AR\ARSNCBR.SCX') WITH _SCREEN.ACTIVEFORM.PARENT
FUNCTION lfARDORDNUM
IF TYPE('loFormSet.llUseScanOption')= 'L' AND loFormSet.llUseScanOption
  lcHdrFile = loFormSet.lcInvHdr
  lcOrderNo = gfSequence('ORDER','','',&lcHdrFile..cDivision)
ELSE
  DO FORM (oAriaApplication.ScreenHome+"ARORDER") WITH 'O','O' TO lcOrderNo
ENDIF
*:**************************************************************************
*:* Name        : LFARDRSTFLAG
*:* Developer   : MARIAM MAZHAR-MMT
*:* Date        : 12/20/2012
*:* Purpose     : reset flags
*:***************************************************************************
FUNCTION LFARDRSTFLAG
IF TYPE('loFormSet.llUseScanOption')= 'L'
  loFormSet.llUseScanOption = .F.
  loFormSet.HASNOTES = .T.
ENDIF
*:**************************************************************************
*:* Name        : LFARDNONOTEP
*:* Developer   : MARIAM MAZHAR-MMT
*:* Date        : 12/20/2012
*:* Purpose     : don't show notes message
*:***************************************************************************
FUNCTION LFARDNONOTEP
IF TYPE('loFormSet.llUseScanOption')= 'L' AND loFormSet.llUseScanOption
  loFormSet.HASNOTES = .F.
ENDIF
*:**************************************************************************
*:* Name        : lfIsUseBin
*:* Developer   : MARIAM MAZHAR-MMT
*:* Date        : 12/20/2012
*:* Purpose     : check if binlocation is used
*:***************************************************************************
FUNCTION lfIsUseBin

LOCAL llUseBin,llTrackBins
STORE .F. TO llUseBin,llTrackBins

llTrackBins = gfGetMemVar('M_WARELOC') = 'Y'  && Keep track of bins
llTrackBins = IIF(TYPE('llTrackBins')='L',llTrackBins,.F.)
IF llTrackBins
  llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
ENDIF
RETURN llTrackBins .AND. llUseBin
*-- end of lfIsUseBin.
*! C201543,1 MMT 12/19/2012 Add option to Direct invoice screen Scan Style barcodes[T20121218.0008][End]
