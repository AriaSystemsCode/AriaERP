*!**************************************************************************
*! Name      : MBIMAIN.PRG
*! Developer : MMT (Mariam Mazhar)
*! Date      : 01/24/2008
*! Purpose   : MBI Custom Process Program.
*!  C200915  ==> for Aria4  attachments
*!  C200916  ==> for Aria27 attachments
*! Ticket id T20071119.0001
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
* Modifications
*B608450,1 TMI 02/24/2008 ticket (T20071030.0001) fix a problem that the trigger CodPrice is not called if the scale has only one size
*! C200957,1 MHM 03/02/2008 T20070920.0001  Add register No. for All UK customers.
*! C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[T20070919.0020]
*! C200969,1 MMT 03/27/2008 Add Quick Order entry screen to Sales order screen[T20070131.0001]
*! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[T20070131.0001]
*! C200969,3 MMT 05/13/2008 Fix bug of wrong Free stock Value                 [T20070131.0001]
*! B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4	  [T20061128.0001]
*! B608617,1 MMT 07/14/2008 Add trigger to update bomline with new ex rate    [T20080624.0001]
*! C201115,1 MMT 03/11/2009 Convert PO Quick Order Entry Screen to Aria4      [T20070323.0013]
*! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4      [T20070323.0013]
*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[T20070214.0006]
*! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect    [T20090511.0002]
*! B608962,1 AHS 08/06/2009 Quick order entry screen accepts cancelled styles [T20090723.0001]
*! B608983,1 TMI 08/25/2009 Fix error while loading a style into the quick order entry screen [T20080811.0001 ]
*! B608998,1 TMI 09/06/2009 Fix a bug that if the first style is cancelled then it is not shown in the browse to be selected from quick entry screen [T20080811.0001]
*! C201219,1 MMT 03/15/2010 Enable Cost sheet Button in case of Status Actualized [T20091217.0005]
*! B609194,1 MMT 03/31/2010 Fix bug of vendor Style is not displayed in PO Quick entry[T20100226.0005]
*! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[T20100430.0001]
*! C201245,1 MMT 06/01/2010 Custom Program to allow user to change account of EDI Temp. Order[T20100219.0001]
*:***************************************************************************

PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:**************************************************************************
*:* Name        : lfCPYNOTES
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/24/2008
*:* Purpose     : Query user if wants to copy Style Notes or not
*:***************************************************************************
FUNCTION lfCPYNOTES

IF gfModalGen('QRM00443B42002','DIALOG')=1
  gfOpenTable('objlink','COBJLINK','SH','OBLNK')
  gfOpenTable('objects','OBJECTID','SH','OBJCT')
  gfOpenTable('Notepad','NOTEPAD','SH','NOTES')
  
 
  
  IF gfSeek('S'+loFormSet.lcFromStyle,'OBLNK','OBJLNKTY') AND gfSeek(OBLNK.cobject_id,'OBJCT','OBJECTID')
    loFormSet.ariaform1.cntThumbnail.ariaimage1.visible = .F.
    loFormSet.ariaform1.cntThumbnail.oleboundcontrol1.ControlSource = 'OBJCT.GOBJECT'
    loFormSet.ariaform1.cntThumbnail.oleBoundControl1.Visible = !EMPTY(OBJCT.GOBJECT)
  ENDIF 
  
ENDIF 


*:**************************************************************************
*:* Name        : lfLastObjID
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/24/2008
*:* Purpose     : Get New object ID
*:***************************************************************************
FUNCTION lfLastObjID

lcReturn = ''
IF USED('OBLNK')
  SELECT OBLNK
  lcFilter = FILTER()
  SET FILTER TO
  GO BOTT
  lcReturn = PADL(INT(VAL(COBJECT_ID))+1,10,'0')
  SET FILTER TO &lcFilter
ENDIF 
RETURN lcReturn

*:**************************************************************************
*:* Name        : lfSAVNOTES
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/24/2008
*:* Purpose     : Save Copied data
*:***************************************************************************
FUNCTION lfSAVNOTES
*!*	ACTIVATE WINDOW trace
*!*	SUSPEND 
IF !USED('OBLNK')  OR !USED('OBJCT') OR !USED('NOTES')
  RETURN 
ENDIF 
lcAlis = ALIAS()
lcNewObjId = lfLastObjID()
SELECT OBLNK
lcOrder = ORDER()
gfsetOrder('OBJLNKTY')
IF gfSeek('S'+loFormSet.lcFromStyle,'OBLNK','OBJLNKTY')
    SELECT OBLNK
    SCAN REST WHILE  cobjlnktyp+cobjlink='S'+loFormSet.lcFromStyle 
      gfSeek(OBLNK.cobject_id,'OBJCT','OBJECTID')
      lcOldObj = OBLNK.cobject_id
	  SELECT OBLNK
	  lnRecNum = RECNO()
	  SCATTER MEMO MEMVAR 
	  m.cobject_id = lcNewObjId 
	  m.cobjlink  = Style.CstyMajor
	  gfAppend('IN OBLNK',.T.)
	  =gfAdd_Info('OBLNK') 
	  gfReplace()
	  gfTableUpdate()
	  
	  SELECT OBJCT
	  lcTempName = gfTempName()
	  SELECT * FROM OBJCT WHERE cobject_id = lcOldObj INTO DBF (Oariaapplication.workdir+lcTempName +'.dbf')
	  
	  IF !USED(lcTempName) AND FILE(Oariaapplication.workdir+lcTempName +'.dbf')
	    USE (Oariaapplication.workdir+lcTempName +'.dbf') IN 0 
	  ENDIF 
	    
	  REPLACE cobject_id WITH  lcNewObjId
	  USE IN (lcTempName)
	  SELECT OBJCT
	  APPEND FROM (Oariaapplication.workdir+lcTempName +'.dbf') 
	  =gfAdd_Info('OBJCT') 
	  gfReplace()
	  gfTableUpdate()

	  ERASE (Oariaapplication.workdir+lcTempName +'.*')
	  lcNewObjId= PADL(INT(VAL(lcNewObjId))+1,10,'0')
	  SELECT OBLNK
	  IF BETWEEN(lnRecNum,1,RECCOUNT())
	    GO RECORD lnRecNum
	  ENDIF 
	ENDSCAN  
ENDIF   
SELECT OBLNK 
gfSetOrder(lcOrder )
  gfCloseTable('OBJCT')
  gfCloseTable('OBLNK')
 
IF gfSeek("F"+loFormSet.lcFromStyle,'NOTES')
  SELECT 'NOTES'
  SCATTER MEMO MEMVAR 
  m.Key =Style.CstyMajor
  m.cdesc = "Notes For Style Number : " + Style.CstyMajor
  gfAppend('IN NOTES',.T.)
  =gfAdd_Info('NOTES') 
  gfReplace()
  gfTableUpdate()
  gfCloseTable('NOTES')
ENDIF 
SELECT(lcAlis)

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
    _Screen.ActiveForm.Parent.ActiveMode <> 'E' .OR. ;
    _Screen.ActiveForm.Parent.llAllColors .OR. ;
    _Screen.ActiveForm.Parent.llAllScales

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
Create Table (oAriaApplication.WorkDir+lcTempSizs);
   ( cSequnc C(6) , ;
     cFit1 C(5),cFit2 C(5),cFit3 C(5),cFit4 C(5),cFit5 C(5),cFit6 C(5),cFit7 C(5),cFit8 C(5),cFit9 C(5),cFit10 C(5),;
     cSize1 c(15), cSize2 c(15), cSize3 c(15), cSize4 c(15), cSize5 c(15) ,;
     cSize6 c(15), cSize7 c(15), cSize8 c(15), cSize9 c(15), cSize10 c(15),;
     nPrice1 N(6,2), nPrice2 N(6,2), nPrice3 N(6,2), nPrice4 N(6,2), nPrice5 N(6,2) ,;
     nPrice6 N(6,2), nPrice7 N(6,2), nPrice8 N(6,2), nPrice9 N(6,2), nPrice10 N(6,2),;
     nComm1 N(6,2), nComm2 N(6,2), nComm3 N(6,2), nComm4 N(6,2), nComm5  N(6,2), ;
     nComm6 N(6,2), nComm7 N(6,2), nComm8 N(6,2), nComm9 N(6,2), nComm10 N(6,2))

lcTmpSzFit = gfTempName()
Create Table (oAriaApplication.WorkDir+lcTmpSzFit);
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

lnStyleWid  = _Screen.ActiveForm.Parent.lnStyleWid 
lnColorWid1 = _Screen.ActiveForm.Parent.lnColorWid1  && ( = lnClrLen )

lcSepart = _Screen.ActiveForm.Parent.lcSepart
lcMajor  = PADR(_Screen.ActiveForm.Parent.Ariaform1.kbStyleMajor.Keytextbox.Value,lnStyleWid)
lcNonMjr = PADR(_Screen.ActiveForm.Parent.Ariaform1.kbNonMajor.Keytextbox.Value,lnColorWid1)

PUSH KEY

*- open the cstprice screen
DO FORM (oAriaApplication.ScreenHome+'\IC\CSTPRICE.SCX') WITH _Screen.ActiveForm.Parent

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
lnScaleLen = _Screen.ActiveForm.Parent.lnScaleLen
lcScale = ALLTRIM(_Screen.ActiveForm.pgfStyleInfo.Page1.kbScale.Keytextbox.Value)
lcScale = PADR(lcScale,lnScaleLen )

SELECT SCALE
IF gfSEEK('S'+lcScale)
  SELECT (lcTempSizs)
  APPEND BLANK
  REPLACE &lcTempSizs..cSequnc WITH lcSizSeq
  
  SELECT SCALE
  =SEEK('S'+lcScale)
  SCAN REST WHILE type+scale+prepak = 'S' + lcScale  
    IF lnCont = 11
      SELECT (lcTempSizs)
      APPEND BLANK
      lnCont = 1
      lcSizSeq = LTRIM(Str(Val(lcSizSeq) + 1))
    ENDIF

    lcCnt  = ALLT(STR(SCALE.cnt))
    lcCont = ALLT(STR(lnCont))
    lcsZ&lcCont = ALLT(ALLT(SCALE.SZ1) + '-' + SCALE.SZ&lcCnt)
    Replace &lcTempSizs..cSize&lcCont With lcsZ&lcCont ;
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
  loFormSet.AriaForm1.Ariapageframe1.Page1.txtComplete.Text1.Value = STYLE.SOLDOUT
ENDIF

*B608450,1 TMI [Start] make the check on "lcDetailFile..TOTQTY" be separate, based on the variable "llTxtQtyCall" 
*                           to be defined
*IF &lcDetailFile..LCONTRACT  .OR. ;
   &lcDetailFile..TOTQTY = 0 .OR. ;
   !EMPTY(&lcDetailFile..CSTSZPRICE)
IF &lcDetailFile..LCONTRACT  .OR. ;
   !EMPTY(&lcDetailFile..CSTSZPRICE)      
  RETURN
ENDIF
IF TYPE('llTxtQtyCall')<>'L'
  IF &lcDetailFile..TOTQTY = 0
    RETURN
  ENDIF
ENDIF
*B608450,1 TMI [End  ] 

*- Get the date to check the valid code price date when compared with 
* This is based on a setup option
DO CASE
  CASE lcDateTyp = 'E'   && Entered
    ldChkDate = loFormSet.AriaForm1.Ariapageframe1.Page1.txtEntered.Text1.Value

  CASE lcDateTyp = 'S'   && Start
    ldChkDate = loFormSet.AriaForm1.Ariapageframe1.Page1.txtStart.Text1.Value
  
  CASE lcDateTyp = 'C'   && Complete
    ldChkDate = loFormSet.AriaForm1.Ariapageframe1.Page1.txtComplete.Text1.Value

ENDCASE

LOCAL lcPricCode
*-- loop here on sizes to gather them into price-groups

*- Get the price code
lcCurrCode = loFormSet.AriaForm1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value
lcPricCode = lfPRICCODE()

*- Get the currency code
IF SEEK(lcPricCode+lcCurrCode+&lcDetailFile..Style,'CSTPRICE')
  CREATE CURSOR &lcPricCursr (SIZES C(8),Gros_Price N(12,2),COMMDV N(12,2),;
         QTY1 N(6),QTY2 N(6),QTY3 N(6),QTY4 N(6),QTY5 N(6),QTY6 N(6),QTY7 N(6),QTY8 N(6),TOTQTY N(6))
  INDEX ON Gros_Price TAG Gros_Price
  LOCAL lnI,lcI
  *B608450,1 TMI [Start] loop only to the scale.cnt number  
  *FOR lnI = 1 TO 8
  FOR lnI = 1 TO SCALE.CNT
    *B608450,1 TMI [End  ] 
    lcI = STR(lnI,1)
    *B608450,1 TMI [Start] go in this loop also when llTxtQtyCall is defined,( called from the txtQuantity field still with no actual qty added to the size fields )
    *IF !EMPTY(&lcDetailFile..QTY&lcI)
      IF !EMPTY(&lcDetailFile..QTY&lcI) .OR. TYPE('llTxtQtyCall')='L'
      *B608450,1 TMI [End  ] 
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
  
  
  *B608450,1 TMI [Start] if more than one price then make rest updates later
  IF RECCOUNT(lcPricCursr)>1 .AND. TYPE('llTxtQtyCall')='L'
    RETURN
  ENDIF
  *B608450,1 TMI [End  ] 
  
  lcRep1 = loFormSet.AriaForm1.Ariapageframe1.Page1.keySalesRep1.Keytextbox.Value
  lcRep2 = loFormSet.AriaForm1.Ariapageframe1.Page1.keySalesRep2.Keytextbox.Value    
  
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
lcAccount = loFormSet.AriaForm1.keyAccount.Keytextbox.Value  

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
  ldEntered = loFormSet.AriaForm1.Ariapageframe1.Page1.txtEntered.Text1.Value
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
  =loNotePad.Do('A',loFormSet.Ariaform1.keyAccount.Keytextbox.Value)
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
lcAcc = IIF(llFrmMain,lcAccount,loFormSet.Ariaform1.keyAccount.Keytextbox.value)
lcSto = IIF(llFrmMain,lcStore  ,loFormset.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.keyStore.Keytextbox.value)

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
IF loFormSet.Ariaform1.Ariapageframe1.Page2.grdEditLines.Style.keyStyle.Value <> ' '
  llValid = lfChkAccSt()
  IF !llValid
    lOb = loFormSet.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.keyStore.Keytextbox
    lOb.Value = lOb.OLDValue    
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
lcI = RIGHT(loCSTSZPRI.Name,1)
lcDetFl = loCSTSZPRI.parent.parent.detailfile
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
lcOldSelect=select()

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
*:**************************************************************************
*:* Name        : lfADDAPREG
*:* Developer   : MHM-Mohamed Shokry
*:* Date        : 03/02/2007
*:* Purpose     : Trigger to add reg. seq no.
*:***************************************************************************
*:* Called from : APpyinv.desctroy method
*! C200957,1 MHM 03/02/2008 T20070920.0001  Add register No. for All UK customers.
*:***************************************************************************
FUNCTION lfADDAPREG
IF loFormSet.activemode= "A" 
	lnAlias = SELECT()

	lcregno =gfSequence('NAPREGIST')
	=gfModalGen('INM00000B00000',.F.,.F.,.F.,"This Invoice has been allocated - Register No: "+lcregno)
	SELECT APINVHDR 
	REPLACE NAPREGIST WITH lcregno
	SELECT (lnAlias)
ENDIF
*C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[Start]
*:**************************************************************************
*:* Name        : lfGTPOEXRAT
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/27/2008
*:* Purpose     : Get PO Price Ex. Rate
*:***************************************************************************
FUNCTION lfGTPOEXRAT
llGetPORate = gfGetMemVar('M_POPRDEF')
lcPoln = loFormSet.lcPosln


*B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[Start]
IF loFormSet.lcPType $ 'S' AND USED('SHPRLFLD') .AND. gfGetMemVar('M_APRVSHIP')
 IF gfSEEK(EVALUATE(lcPoln +'.SHIPNO')+EVALUATE(lcPoln +'.PO')+STR(EVALUATE(lcPoln +'.LINENO'),6),'SHPRLFLD') 
   RETURN 
 ENDIF 
ENDIF         
*B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]



IF llGetPORate  AND (&lcPoln..Cstytype = 'P' AND &lcPoln..CBUSDOCU = 'P')
  IF !USED('POSHDR_R')
    gfOpenTable('POSHDR','POSHDR','SH','POSHDR_R')
  ENDIF 
  IF gfSeek(&lcPoln..CBUSDOCU+&lcPoln..Cstytype+&lcPoln..PO,'POSHDR_R')
    lnCrRt1 = POSHDR_R.npricerat
    
    STORE '/' TO lcExSign, lcUntSin
    lcExSign = gfGetExSin(@lcUntSin, POSHDR_R.cPriceCur)
    loFormSet.laECost[1] = EVALUATE(loFormSet.lcTmpLine+'.nFLanCost1') &lcExSign lnCrRt1 &lcUntSin POSHDR_R.nCurrUnit
  ENDIF 
  
ENDIF 
*C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[End]
*! C200969,1 MMT 03/27/2007 Add Quick Order entry screen to Sales order screen[Start]
*:**************************************************************************
*:* Name        : lfQKORDENT
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : Add Menu of Quick Order Entry Screen(C200969,1)
*:***************************************************************************
FUNCTION lfQKORDENT
DEFINE BAR loFormSet.NextOptionBar OF _INQURYPOP PROMPT 'Quic\<k Order Entry'  SKIP FOR  gfFormIsActive(&lcHostFormName) AND ((TYPE('_screen.ActiveForm.ariapageframe1') = "O" AND  _screen.ActiveForm.ariapageframe1.ActivePage<>2) .OR. (_Screen.ActiveForm.Parent.ActiveMode = 'V'))
ON SELECTION BAR loFormSet.NextOptionBar OF _INQURYPOP  DO lfOpnQkScr

loFormSet.NextOptionBar = loFormSet.NextOptionBar + 1

*:**************************************************************************
*:* Name        : lfOpnQkScr
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : open Quick Order Entry Screen(C200969,1)
*:***************************************************************************
FUNCTION lfOpnQkScr
lcAlias = SELECT()

PRIVATE laSize,laTot,;
        lcScrTtl,lcDet_Ttl,lnClrLen,lnClrPos,lcClrSpr,lcTempCur,lcTempNote,lcAlias,lnMrk ,;
        lcQkWin0,lcQkWin1,lcQkWin2,lcQkWin3,lcQkWin4,lcOldValue,lcSepart,lcItemPct,laExtSz,lcOrd,llEdit,;
        llChang,laStyClQty,llDifPrice,llShw1,laOldVal,lcBrowFlds

DO FORM  (oAriaApplication.ScreenHome+'\SO\SoQKORD.scx') WITH _Screen.ActiveForm.Parent
SELECT (lcAlias)


*:**************************************************************************
*:* Name        : lfInitQkOrd
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : Init Function of Quick Order Entry Screen(C200969,1)
*:***************************************************************************
FUNCTION lfInitQkOrd
PARAMETERS loBranchFormSet
WITH loBranchFormSet.AriaForm1
  .kbstymaj.Enabled = .T. 
  .spnComm.Enabled = .F. 
  .spnDisc.Enabled = .F. 
  .txtChkQty.Enabled= .F.
  .txtGrsPrc.Enabled= .F. 
  .txtNetPri.Enabled = .F. 
  .dtpCompDate.Enabled= .F. 
  .cmdSave.Enabled= .F. 
  .cmdNote.Enabled = .F. 
  .grdLines.RecordSource = ''
  .grdStk.RecordSource = ''
ENDWITH 
loBranchFormSet.lctempcur = gfTempName()
loBranchFormSet.lcTmpStk = gfTempName()
loBranchFormSet.llincwip = gfGetMemVar('M_SOWIPQK')
loBranchFormSet.llExtended = gfGetMemVar('M_USEEXSSC')
gfOpenTable('STYPRICE','STYPRICE')

*:**************************************************************************
*:* Name        : lfvQStyle
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : Validate Style Field(C200969,1)
*:***************************************************************************
FUNCTION lfvQStyle
PARAMETERS loBranchFormSet,loParentForm


IF loParentForm.DataSessionId <> SET("Datasession") 
  SET DATASESSION TO loParentForm.DataSessionId
ENDIF   


lcStyleVal = loBranchFormSet.AriaForm1.kbstymaj.KeyTextBox.Value
PRIVATE lcAlias,lcGetSty,lcStat,lnIncrmnt,lnClrs,lnCount,lnJ,lcSeekSty,laSum,laSavStat,lcStyle
*--- Select
lcAlias = ALIAS()
loBranchFormSet.lnMajorLen = LEN(loParentForm.ariaform1.ariapageframe1.page2.ariaeditregion1.stylemajorpicture)

lcStyleVal = PADR(ALLTRIM(lcStyleVal),loBranchFormSet.lnMajorLen)
IF (!EMPTY(lcStyleVal) AND ('?' $ lcStyleVal  .OR. !SEEK(lcStyleVal ,'STYLE'))) OR ;
   loBranchFormSet.AriaForm1.kbstymaj.selectedfrombrowse 

  *-* HES && Link Style to price table if the setup option related to this feature set to YES
  IF ASCAN(loParentForm.laEvntTrig,PADR('FLTPRCSTL',10),1,ALEN(loParentForm.laEvntTrig,1),1) > 0 
    loParentForm.mDoTrigger(PADR('FLTPRCSTL',10))
  ENDIF 
  *-* HES

  SELECT STYLE  
  *B608998,1 TMI [start] use the ARIABROW instead to not use the CSTYLE index
  *lcGetSty = PADR(gfStyBrw("M" , lcStyleVal  , "" , .F.),loBranchFormSet.lnMajorLen)
  LOCATE
  lcFile_Ttl = "Styles"
  lcBrFields = "style :30 ,cdivision :H='Division' :20"
  DIMENSION laTempData[1]
  laTempData[1] = ''
  =AriaBrow('','Styles',.F.,.F.,.F.,.F.,'',.T.,'STYLE','laTempData')  
  lcGetSty = PADR(laTempData[1],loBranchFormSet.lnMajorLen)  
  *B608998,1 TMI [end  ] use the ARIABROW instead to not use the CSTYLE index
  loBranchFormSet.AriaForm1.kbstymaj.selectedfrombrowse  = .F.
  IF EMPTY(lcGetSty)
    loBranchFormSet.AriaForm1.kbstymaj.KeyTextBox.Value = ''
    RETURN .F.
  ELSE
    loBranchFormSet.AriaForm1.kbstymaj.KeyTextBox.Value = lcGetSty
  ENDIF
ENDIF  


lcOrdHDr = loParentForm.OFORMENVIRONMENT.lcOrdHdr
lcOrdLine = loParentForm.ariaform1.ariapageframe1.page2.ariaeditregion1.Detailfile
=SEEK('M'+&lcOrdHdr..Account,'Customer')
lcpricelevel = IIF(INLIST(Customer.PriceLvl,"A","B","C","Q"),Customer.PriceLvl,'A')
m.Style   = ALLTRIM(loBranchFormSet.AriaForm1.kbstymaj.KeyTextBox.Value)
* B608998,1 TMI [start] use the STYLE index instead of CSTYLE
*IF SEEK(PADR(m.Style,19),'Style','Cstyle')
IF SEEK(PADR(m.Style,loBranchFormSet.lnMajorLen),'Style','style')
  * B608998,1 TMI [end  ] use the STYLE index instead of CSTYLE
  llValidStyle = .T.
  IF !SEEK('S'+Style.Scale,'Scale')
    =gfModalGen('TRM32017B00000','ALERT')
    llValidStyle = .F.
  ENDIF 
  
  *IF llValidStyle  AND (Style.Status='X') 
  *   = gfModalGen('TRM32018B00000','ALERT')
  *   llValidStyle = .F.
  *ENDIF   
  
  IF llValidStyle  AND (Style.Status='H')
    = gfModalGen('QRM32019B32003','ALERT')
     llValidStyle = .F.
  ENDIF    
  
  IF llValidStyle  AND (Style.cDivision <> &lcOrdHDr..cDivision)
    = gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(gfCodDes(&lcOrdHDr..cDivision,'CDIVISION')))
    llValidStyle = .F.
  ENDIF   
  
     
  IF llValidStyle  AND (!EMPTY(Style.Start) .AND. Style.Start > &lcOrdHDr..Complete) 
    =gfModalGen('QRM32021B32003','ALERT','start|'+DTOC(Style.Start))
    llValidStyle = .F.
  ENDIF
     
  IF llValidStyle  AND !EMPTY(Style.SoldOut) .AND. Style.SoldOut < &lcOrdHDr..Start 
    IF  gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut)) <> 1
      llValidStyle = .F.
    ENDIF   
  ENDIF 
        
  IF llValidStyle AND  ASCAN(loParentForm.laEvntTrig , PADR('STYLEVALID',10)) <> 0 
    IF ALLTRIM(gfGetMemVar("M_SEASNCHK")) = "Y"  AND  (ALLTRIM(&lcOrdHDr..Season )<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season <> &lcOrdHDr..Season)
      gfModalGen('TRM32020B00000','ALERT','season '+ ALLTRIM(gfCodDes(&lcOrdHDr..Season,'SEASON')))      
      llValidStyle = .F.
    ENDIF   
  ENDIF
  
  IF !llValidStyle 
    loBranchFormSet.AriaForm1.kbstymaj.KeyTextBox.Value = ''
    RETURN .F.
  ELSE
    loBranchFormSet.AriaForm1.SpnComm.Enabled = (!EMPTY(loParentForm.ariaform1.ariapageframe1.page1.keySalesRep1.KeyTextBox.Value) and Style.Commission)
    loBranchFormSet.AriaForm1.spnDisc.Enabled = .T.
    loBranchFormSet.AriaForm1.cmdNote.Enabled = .T.
    loBranchFormSet.AriaForm1.txtChkQty.Enabled = .T.
    loBranchFormSet.AriaForm1.DtpCompDate.Enabled = loParentForm.ariaform1.ariapageframe1.page2.ariaeditregion1.Editlinecompletedate
    loBranchFormSet.AriaForm1.DtpCompDate.Value = loParentForm.ariaform1.ariapageframe1.page1.txtComplete.Value
  ENDIF 
  
  
  STORE 0   TO m.Gros_Price, m.Price, m.Disc_Pcnt
  STORE .F. TO m.lContract
  STORE '' TO m.Store 
*!*    m.lContract = loParentForm.lcOrdType <> 'C' 
*!*    
*!*    lfStyPrice(ALLTRIM(m.Style),m.Store,'m.Gros_Price','m.Price','m.Disc_Pcnt','m.lContract',0)

*!*    IF m.Gros_Price < 0 
*!*      RETURN .F.
*!*    ENDIF 

  =SEEK(allt(m.Style),'STYLE')
  
  WITH loBranchFormSet.AriaForm1
    .txtGrsPrc.Value = m.Gros_Price
    .txtNetPri.Value = m.Gros_Price
    .txtStydesc.Value = Style.Desc1
    .SpnComm.Value = IIF(Style.Commission,loParentForm.ariaform1.ariapageframe1.page1.spnComm1.Value,0)
  ENDWITH  
  
  
  *-- Get Style discount percent and Calculate net price  
*!*    IF !m.lContract
*!*      *-- get the cDiscCode From stydye in every case
*!*      lcDiscCode  = IIF(SEEK(m.Style+&lcOrdHDr..CWARECODE+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
*!*      loBranchFormSet.AriaForm1.spnDisc.Value = 0 
*!*      IF !EMPTY(ALLTRIM(lcDiscCode))
*!*        *-- Get the disecound related filed to now which 
*!*        *-- type whole Sale Or Retail sale Or Both.
*!*        DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
*!*        STORE '' To lcDisType , ldstartDte ,ldEndDate
*!*        *-- Array to get the Discount affect for DecCode.
*!*        laDisType[1,1]  = 'CCOSTAFECT'
*!*        laDisType[1,2]  = 'lcDisType'
*!*        *-- Array to get the start date For DescCode.
*!*        lastartDte[1,1] = 'START'
*!*        lastartDte[1,2] = 'ldstartDte'
*!*        *-- Array to get the end date For DescCode.
*!*        laEndDate[1,1]  = 'DENDATE'
*!*        laEndDate[1,2]  = 'ldEndDate'
*!*        = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
*!*        = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
*!*        = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
*!*        lnDisc_Pcnt = 0
*!*        IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(&lcOrdHDr..Entered,ldstartDte,ldEndDate)
*!*          lnDisc_Pcnt = m.Disc_Pcnt
*!*          =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
*!*          loBranchFormSet.AriaForm1.spnDisc.Value  = lnDisc_Pcnt
*!*        ENDIF
*!*      ENDIF  
*!*      loBranchFormSet.AriaForm1.txtNetPri.Value   = loBranchFormSet.AriaForm1.txtGrsPrc.Value*(100-loBranchFormSet.AriaForm1.spnDisc.Value )/100  
*!*    ENDIF 




  PRIVATE laPrices,lcStyClr
  SELECT STYLE  
  =SEEK(allt(m.Style),'STYLE')
  loBranchFormSet.AriaForm1.cmdClear.Enabled = !EMPTY(m.Style)
  laSize = ''
  loBranchFormSet.laSize = ''
  loBranchFormSet.laExtSz = ''
  
  =SEEK(allt(m.Style),'STYLE')
  loBranchFormSet.lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
  m.SCALE = STYLE.SCALE
  SELECT Scale,CNT FROM SCALE WHERE Type+Scale='S'+SUBSTR(m.SCALE,1,loBranchFormSet.lnScaleLen) ORDER BY 1 INTO ARRAY laExtSz
  SELECT SCALE
  lcStyScl = SUBSTR(STYLE.SCALE,1,loBranchFormSet.lnScaleLen)
  =SEEK('S'+lcStyScl ,'SCALE')
  IF !EMPTY(SCALE.CDIM1)
    loBranchFormSet.llMultiDime = .T.
  ENDIF
  lnMaxSz = 0
  IF loBranchFormSet.llMultiDime 
    lnMaxSz = 0
    lnLastCnt = 0
    SELECT Scale 
    =SEEK('S'+lcStyScl ,'SCALE')
    DO WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl .AND. !EOF('SCALE')
      lnRecord = RECNO('Scale')
      lcFit = SCALE.CDIM1
      lnMaxSz = 0
      SCAN REST WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl FOR ;
                    CDIM1 = lcFit
        FOR lnI = 1 TO SCALE.CNT
          lnMaxSz = lnMaxSz  + 1
        ENDFOR
      ENDSCAN 
      IF lnMaxSz > lnLastCnt
        lnLastCnt = lnMaxSz 
      ENDIF
      lnMaxSz = IIF(lnMaxSz >= lnLastCnt,lnMaxSz,  lnLastCnt)                
      IF BETWEEN(lnRecord+1,1,RECCOUNT('Scale'))
        GO lnRecord+1 IN Scale
      ENDIF 
    ENDDO 
  ENDIF 
  loBranchFormSet.lnMaxSz = lnMaxSz



  lnIncrmnt = 0
  FOR lnCount = 1 TO ALEN(laExtSz,1)
    lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
  ENDFOR  
  IF lnIncrmnt > 16
    *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'This Style has scale with More than 16 Sizes.')
  ENDIF
  
  
  IF SEEK(loParentForm.lcOrdType+&lcOrdHdr..Order+&lcOrdHdr..Store+ALLTRIM(m.Style),(lcOrdLine),'ORDLINST')
    *--This Style has entred on this order
    lnResp = gfModalGen('QRM00000B02011',.F.,.F.,.F.,'This style has already been entred on this order '+;
                                                    +'- do you wish to create new order lines ?')
    IF lnResp = 2 
      =lfvClear(loBranchFormSet) 
      RETURN 
    ENDIF
  ENDIF
  
  
  lnIncrmnt = 0
  
  
  FOR lnCount = 1 TO ALEN(laExtSz,1)
    =SEEK('S'+laExtSz[lnCount,1],'SCALE')
    FOR lnJ = 1 TO laExtSz[lnCount,2]
      lcZ = STR(lnJ,1)
      DIMENSION laSize[lnIncrmnt+lnJ]
      laSize[lnIncrmnt+lnJ] = ALLTRIM(SCALE.Sz&lcZ)
    ENDFOR
    lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
  ENDFOR
  
  *--Get Colors
  STORE 0  TO lnClrPos,lnClrLen
  PRIVATE lnClrPos,lnClrLen
  STORE 0 TO lnSizePos,lnSizeLen
  lfGetClrD()
  loBranchFormSet.lnClrLen = lnClrLen
  loBranchFormSet.lnClrPos = lnClrPos
  
  lfCrtTempFiles(loBranchFormSet)  
  
  *--Create color array
  SELECT DISTINCT SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) FROM STYLE ;
    WHERE STYLE = PADR(m.Style,loBranchFormSet.lnMajorLen) ;
    INTO ARRAY laClr  
  
  
  STORE '' TO lcStySp,lcSclSp
  DECLARE laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnJ = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnJ,1] = 'F'
       lcStySp = laItemSeg[lnJ,6]
       loBranchFormSet.lcStySp = lcStySp
       LOOP 
    ENDIF 
    IF laItemSeg[lnJ,1] = 'C'
       lcSclSp = laItemSeg[lnJ,6]
       loBranchFormSet.lcSclSp = lcSclSp 
       LOOP 
    ENDIF 
  ENDFOR  
  

  STORE 0   TO m.Gros_Price, m.Price, m.Disc_Pcnt
  STORE .F. TO m.lContract
  STORE '' TO m.Store 
  lcPriceLevel = IIF(INLIST(lcPriceLevel ,'A','B','C'),lcPriceLevel ,'A')
  *--Add a line for each color
  PRIVATE lnMaxInd
  lnMaxInd = IIF(ALEN(laClr,1)>=10,10,ALEN(laClr,1))
  FOR lnClrs = 1 TO lnMaxInd
    SELECT (loBranchFormSet.lcTempCur)
    SCATTER MEMVAR BLANK
    M.STYMAJOR = PADR(m.Style,loBranchFormSet.lnMajorLen)
    M.COLOR = laClr[lnClrs]
    M.COLORDSC = PADR(gfCodDes(PADR(laClr[lnClrs],lnClrLen) , 'COLOR'),20)
    lnIncrmnt = 0
    DIMENSION laSzQty[ALEN(laSize,1)]
    STORE 0 TO laSzQty
    lnCounSz = 1
    =SEEK(allt(m.Style),'STYLE')
    lcStyScl = SUBSTR(STYLE.SCALE,1,loBranchFormSet.lnScaleLen)
    DIMENSION laFits[1]
    laFits = ''
    SELECT SCALE
    =SEEK('S'+lcStyScl)
    lcScaleCurr = ''
    SCAN REST  WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl FOR !EOF('SCALE') 
      lnRecord = RECNO('Scale')
      lcFit = SCALE.CDIM1
      
      IF ASCAN(laFits ,lcFit,1) > 0
        LOOP 
      ENDIF 
      
       =gfseek(M.STYMAJOR+'-'+M.COLOR+SCALE.SCALE,'STYLE')   
         
      *!B608962,1 08/06/2009 AHS quick order entry screen accepts cancelled styles [start]
      *-- If style status is cancelled.
      IF STYLE.Status='X'
        *-This is a canceled style. Not allowed to enter here, Cannot proceed!
        WAIT WINDOW NOWAIT 'The style '+STYLE.STYLE+' is cancelled'
        LOOP
      ENDIF    
      *!B608962,1 08/06/2009 AHS quick order entry screen accepts cancelled styles [end]
      
      
      IF !EMPTY(SCALE.CDIM1)
        m.FitDesc = SCALE.CDIM1
      ELSE
        m.FitDesc = SCALE.CSCL_DESC
      ENDIF  
      
      *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
      m.Scale = Scale.Scale
      *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[End]
      
      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
      *lnCounter = 1 
      lnSzCounter = 1       
      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]      
      
      FOR lnCont = 1 TO IIF(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz,ALEN(laSize,1))
        lcCont  = ALLTRIM(STR(lnCont))
        STORE '' TO m.SZ&lcCont
      ENDFOR 
      IF EMPTY(laFits[1])
        laFits[1] = lcFit 
      ELSE
        DIMENSION laFits[ALEN(laFits,1)+1] 
        laFits[ALEN(laFits,1)] =lcFit 
      ENDIF    
      lnCounting = 1
      SCAN REST WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl FOR  CDIM1 = lcFit 
        lcCounting = ALLTRIM(STR(lnCounting))
        lnCounting = lnCounting  + 1
        IF ALLTRIM(lcScaleCurr) <> ALLTRIM(Scale.Scale)
          *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]        
          *lcSeekSty = M.STYMAJOR+lcStySp +PADR(M.COLOR,lnClrLen)+IIF(loBranchFormSet.llExtended,lcSclSp +Scale.Scale,'')
          lcSeekSty = PADR(M.STYMAJOR+lcStySp +PADR(M.COLOR,lnClrLen)+IIF(loBranchFormSet.llExtended,ALLTRIM(lcSclSp) +Scale.Scale,''),19)
          *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]
          
          IF loParentForm.oFormEnvironment.laSetups[5,2]='Y' .AND. !SEEK(lcSeekSty+loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value+SPACE(10),'StyDye')
            lcMsg = 'Style ' + lcSeekSty + ' is not assigned to location ' + loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value
            lnResp = gfModalGen('QRM00000B40002',.F.,.F.,.F.,lcMsg)
            IF lnResp = 1
              DO gpAdStyWar WITH lcSeekSty,SPACE(10),loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value
            ELSE            
              RETURN .F. 
            ENDIF
          ENDIF
          STORE 0   TO m.Gros_Price, m.Price, m.Disc_Pcnt
          STORE .F. TO m.lContract
          STORE '' TO m.Store 
          
          
          =SEEK(ALLTRIM(lcSeekSty),'Style')
          
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
          lcPRICCODE = ''
          llPricCode = .F.
          ldChkDate = loParentForm.AriaForm1.Ariapageframe1.Page1.txtEntered.Text1.Value
          IF gfSEEK(lfQKPRICCODE()+loParentForm.ariaform1.ariapageframe1.page1.keyCurrency.Keytextbox.value+lcSeekSty,'CSTPRICE')        
	 	    m.Comm = IIF(CSTPRICE.COMMDV=0,IIF(Style.Commission,loBranchFormSet.AriaForm1.SpnComm.Value,0),CSTPRICE.COMMDV)
	        m.Gros_Price = CSTPRICE.PRICEDV
	        llPricCode = .T.
		  ELSE    	  
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
          
            IF loParentForm.oFormEnvironment.laSetups[17,1]='Y' .AND. loParentForm.ariaform1.ariapageframe1.page1.keyCurrency.Keytextbox.value <> oAriaApplication.BaseCurrency 
               IF gfSEEK(lcSeekSty+loParentForm.ariaform1.ariapageframe1.page1.keyCurrency.Keytextbox.value ,'STYPRICE')
                  m.Gros_Price = STYPRICE.PRICE&lcPriceLevel
               ENDIF 
             ELSE
             m.Gros_Price = STYle.PRICE&lcPriceLevel
            ENDIF 
          
         *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
          ENDIF          
         *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]          
         
          IF ASCAN(laExtSz,Scale.Scale,1) > 0
            lnCounting  = ASUBSCRIPT(laExtSz,ASCAN(laExtSz,Scale.Scale,1),1)
            lcCounting  = ALLTRIM(STR(lnCounting))
          ENDIF 
          
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
*!*	          m.Pric&lcCounting  =  m.Gros_Price
*!*	          m.NtPri&lcCounting  =  m.Gros_Price
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
          
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
          IF !llPricCode         
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
          
            m.Comm =  IIF(Style.Commission,loBranchFormSet.AriaForm1.SpnComm.Value,0)
            
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]            
          ENDIF 
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]  
        ENDIF 
        lcScaleCurr = Scale.Scale
        m.FitDesc = lcFit   
        

             
        FOR lnI = 1 TO SCALE.CNT

		  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]            
          *lcCounter = ALLTRIM(STR(lnCounter))
          lcCounter = ALLTRIM(STR(lnSzCounter))          
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]            
          
          lcI = STR(lnI,1)
          
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]            
          *lnCounter = lnCounter + 1
          lnSzCounter= lnSzCounter+ 1
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]            
          
          m.SZ&lcCounter = Scale.SZ&lcI 
          
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]            
          IF llPricCode AND m.Gros_Price = 0
            m.Pric&lcCounter = CSTPRICE.Price&lcI 
            m.NtPri&lcCounter = CSTPRICE.Price&lcI 
          ELSE
            m.Pric&lcCounter = m.Gros_Price
            m.NtPri&lcCounter = m.Gros_Price
          ENDIF 
		  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]            

          
          SELECT(loBranchFormSet.lcTempCur)        
          *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
          *IF !SEEK(PADR(M.STYMAJOR,19)+M.COLOR+padR(m.FitDesc,10),loBranchFormSet.lcTempCur) 
          IF !SEEK(PADR(M.STYMAJOR,19)+M.COLOR+padR(m.Scale,3),loBranchFormSet.lcTempCur) 
          *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[End]
            APPEND BLANK
            SELECT(loBranchFormSet.lcTmpStk)
            APPEND BLANK
          ENDIF 
          SELECT(loBranchFormSet.lcTempCur)  
          GATHER MEMO MEMVAR
          SELECT(loBranchFormSet.lcTmpStk)                  
          GATHER MEMO MEMVAR
        ENDFOR   
      ENDSCAN   
      IF BETWEEN(lnRecord,1,RECCOUNT('Scale'))
        GO lnRecord IN Scale
      ENDIF             
    ENDSCAN 


  ENDFOR  
 
  =SEEK(PADR(m.Style,loBranchFormSet.lnMajorLen),'STYLE')
 
  SELECT (loBranchFormSet.lcTempCur)
  GO TOP 
  
  *!B608962,1 AHS 08/06/2009 Quick order entry screen accepts cancelled styles [start]
  IF EOF()
    RETURN .F.
  ENDIF
  *!B608962,1 AHS 08/06/2009 Quick order entry screen accepts cancelled styles [start]
  
  SCATTER MEMO MEMVAR 
  APPEND BLANK 
  GATHER MEMO MEMVAR 
  REPLACE StyMajor WITH CHR(255),;
          Color    WITH '',;
          COLORDSC WITH 'Totals' ,;
          FitDesc  WITH ''
  
  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
  Replace Scale WITH ''
  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
  
  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]           
  *FOR lnCount = 1 TO ALEN(laExtSz,1)

  *B608983,1 TMI [START] Change the loop length based on llMultiDime variable
  *FOR lnCount = 1 TO ALEN(laSize,1)  
  FOR lnCount = 1 TO IIF(loBranchFormSet.llMultiDime,lnMaxSz,ALEN(laSize,1) )    
  *B608983,1 TMI [END  ] Change the loop length based on llMultiDime variable
  
  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]   
    lcI = ALLTRIM(STR(lnCount))
    REPLACE Pric&lcI  WITH 0,;
            NtPri&lcI WITH 0
  ENDFOR 
  
         
  FOR lnC = 1 TO IIF(loBranchFormSet.llMultiDime,lnMaxSz,ALEN(laSize,1) )    
    lcC = ALLTRIM(STR(lnC))
    SELECT (loBranchFormSet.lcTempCur)
    SUM (Qty&lcC) TO lnTotal FOR StyMajor <> CHR(255)
    GO BOTTOM 
    REPLACE Qty&lcC WITH lnTotal
  ENDFOR 
  SELECT (loBranchFormSet.lcTempCur)
  LOCATE 

  WITH loBranchFormSet.AriaForm1
    .kbstymaj.Enabled = .F. 
    .spnDisc.Enabled = .T. 
  ENDWITH 


  ACOPY(laSize,loBranchFormSet.laSize)
  ACOPY(laExtSz,loBranchFormSet.laExtSz)
  
  lfGetStock(loBranchFormSet)
  lfAddCntrSrc(loBranchFormSet)
  lfCalcTot(loBranchFormSet)
  
  SELECT (loBranchFormSet.lcTempCur)
  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
  *  SET RELATION TO StyMajor+color+FitDesc INTO (loBranchFormSet.lcTmpStk) ADDITIVE 
  SET RELATION TO StyMajor+color+Scale INTO (loBranchFormSet.lcTmpStk) ADDITIVE 
  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[End]
  
  loBranchFormSet.AriaForm1.grdStk.refresh    
  loBranchFormSet.AriaForm1.grdLines.refresh    
  loBranchFormSet.AriaForm1.grdLines.AFterRowColChange
ENDIF 
*-- end of lfvQStyle.
*:**************************************************************************
*:* Name        : lfvClear
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : Clear Screen(C200969,1)
*:***************************************************************************
FUNCTION lfvClear
PARAMETERS loBranchFormSet,llFromScreen
IF llFromScreen
  loBranchFormSet.AriaForm1.grdLines.RecordSource = ''
  loBranchFormSet.AriaForm1.grdStk.RecordSource = ''
ENDIF 
loBranchFormSet.llMultiDime = .F.
IF USED(loBranchFormSet.lctempcur)
  SELECT(loBranchFormSet.lctempcur)
  ZAP
ENDIF 
IF USED(loBranchFormSet.lcTmpStk)
  SELECT (loBranchFormSet.lcTmpStk)
  ZAP
ENDIF
WITH loBranchFormSet.AriaForm1
  .kbstymaj.Enabled = .T. 
  .kbstymaj.KeyTextBox.Value = ''
  .spnComm.Enabled = .F. 
  .spnComm.value = 0
  .spnDisc.Enabled = .F. 
  .spnDisc.VAlue = 0
  .txtChkQty.Enabled= .F.
  .txtChkQty.value = 0
  .txtGrsPrc.Enabled= .F. 
  .txtGrsPrc.value = 0
  .txtNetPri.Enabled = .F. 
  .txtNetPri.value = 0
  .dtpCompDate.Enabled= .F. 
  .cmdSave.Enabled= .F. 
  .cmdNote.Enabled = .F. 
  .grdLines.RecordSource = ''
  .grdStk.RecordSource = ''
  .txtStydesc.Value = ''
ENDWITH 
DIMENSION loBranchFormSet.laSize[1],loBranchFormSet.laExtSz[1]


*:**************************************************************************
*:* Name        : lfStyPrice
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : Get Style Price (C200969,1)
*:***************************************************************************
FUNCTION lfStyPrice
LPARAMETERS lcStyle,lcStore,lcGros_Price,lcPrice,lcDisc_Pcnt,lcContract, lnTotQty
*-- Get Price from contract
&lcContract = &lcOrdHDr..cOrdType<>'C' .AND. lfGetContractPrice(lcStyle,lcStore,lcGros_Price,lcPrice,lcDisc_Pcnt)
IF !&lcContract
  *-- Get Style Gross Price
  &lcGros_Price = gfGetprice(lcStyle,lcPriceLevel ,lnTotQty,&lcOrdHDr..cCurrCode)
  IF &lcGros_Price = 0
    IF lcPriceLevel  = 'Q'
      DO CASE
        CASE Style.nAtQtyC > 0 AND lnTotQty > Style.nAtQtyC
          lcLevel = 'C'
        CASE Style.nAtQtyB > 0 AND lnTotQty > Style.nAtQtyB
          lcLevel = 'B'
        OTHERWISE
          lcLevel = 'A'
      ENDCASE
    ELSE
      lcLevel=IIF(INLIST(lcPriceLevel ,'A','B','C'),lcPriceLevel ,'A')
    ENDIF
    &lcGros_Price = lfCheckPri(lcStyle,lcLevel,&lcOrdHDr..cCurrCode)
  ENDIF  

  *-- get style discount
  lcDiscCode  = IIF(SEEK(lcStyle+&lcOrdHDr..cWareCode+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
  m.Disc_Pcnt = 0 
  IF !EMPTY(lcDiscCode)
    *-- Get discount type, start date, end date, and discount percent
    DECLARE laDisRltFld[4,2]
    STORE '' TO lcDisType
    STORE {} TO ldstartDte, ldEndDate
    STORE 0  TO lnDisc_Pcnt
    laDisRltFld[1,1] = 'CCOSTAFECT'
    laDisRltFld[1,2] = 'lcDisType'
    laDisRltFld[2,1] = 'START'
    laDisRltFld[2,2] = 'ldstartDte'
    laDisRltFld[3,1] = 'DENDATE'
    laDisRltFld[3,2] = 'ldEndDate'
    laDisRltFld[4,1] = 'DISCPCNT'
    laDisRltFld[4,2] = 'lnDisc_Pcnt'
    =gfRltFld(lcDiscCode, @laDisRltFld, 'CDISCCODE')
    IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(&lcOrdHDr..Entered,ldstartDte,ldEndDate)
      &lcDisc_Pcnt = lnDisc_Pcnt
    ENDIF
  ENDIF  
  &lcPrice = &lcGros_Price*(100-&lcDisc_Pcnt)/100  
ENDIF

*:**************************************************************************
*:* Name        : lfGetContractPrice
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : Get Contract Style Price (C200969,1)
*:***************************************************************************
FUNCTION lfGetContractPrice
PARAMETERS lcStyle,lcStore,lcGPrice,lcNPrice,lcDiscount
PRIVATE lnAlias,lnGPrice,lnNPrice,lnDiscount,llContract
lnAlias = SELECT()

lnGPrice   = &lcGPrice
lnNPrice   = &lcNPrice
lnDiscount = &lcDiscount
llContract = .F.
SET ORDER TO TAG ORDLINST IN OrdLine
SELECT (loParentForm.ariaform1.ariapageframe1.page2.ariaeditregion1.ContractFile)
IF SEEK(&lcOrdHDr..Account+'C')
  *-- Check open contracts for order account having same date period and currency code
  SCAN REST WHILE Account+cOrdType+Order = &lcOrdHDr..Account+'C' ;
       FOR  !INLIST(Status,'X','B') .AND. BETWEEN(&lcOrdHDr..Entered,START,COMPLETE) .AND. CCURRCODE=&lcOrdHDr..cCurrCode
    lcContact = Order
    SELECT OrdLine
    =SEEK('C'+lcContact+lcStore+lcStyle)
    LOCATE REST WHILE cordtype+order+store+style+STR(lineno,6)='C'+lcContact+lcStore+lcStyle ;
                FOR   BETWEEN(&lcOrdHDr..Entered,START,COMPLETE) AND Price <> 0
    IF FOUND()
      lnGPrice   = OrdLine.Gros_Price
      lnNPrice   = OrdLine.Price
      lnDiscount = OrdLine.Disc_Pcnt
      llContract = .T.
      EXIT
    ENDIF
  ENDSCAN
ENDIF
IF !llContract AND SEEK(&lcOrdHDr..Account+'C')
  *-- Check open contracts for order account having same date period and currency code
  SCAN REST WHILE Account+cOrdType+Order = &lcOrdHDr..Account+'C' ;
       FOR  !INLIST(Status,'X','B') .AND. BETWEEN(&lcOrdHDr..Entered,START,COMPLETE) .AND. CCURRCODE=&lcOrdHDr..cCurrCode
    lcContact = Order
    SELECT OrdLine
    =SEEK('C'+lcContact+SPACE(8)+lcStyle)
    LOCATE REST WHILE cordtype+order+store+style+STR(lineno,6)='C'+lcContact+SPACE(8)+lcStyle ;
                FOR   BETWEEN(&lcOrdHDr..Entered,START,COMPLETE) AND Price <> 0
    IF FOUND()
      lnGPrice   = OrdLine.Gros_Price
      lnNPrice   = OrdLine.Price
      lnDiscount = OrdLine.Disc_Pcnt
      llContract = .T.
      EXIT
    ENDIF
  ENDSCAN
ENDIF
SET ORDER TO TAG OrdLine IN OrdLine
SELECT (lnAlias)
&lcGPrice   = lnGPrice
&lcNPrice   = lnNPrice
&lcDiscount = lnDiscount
RETURN(llContract)
*:**************************************************************************
*:* Name        : lfvqGPrice
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Gross price valid fn(C200969,1)
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqGPrice()
*:***************************************************************************
FUNCTION lfvqGPrice
PARAMETERS loBranchFormSet

*-* HES && Restric Editing prices debend on User privilege
IF loBranchFormSet.AriaForm1.txtGrsPrc.Value <> loBranchFormSet.AriaForm1.txtGrsPrc.OldValue
  IF ASCAN(loBranchFormSet.laEvntTrig,PADR('EDITPRICE',10),1,ALEN(loBranchFormSet.laEvntTrig,1),1) > 0
    IF !loBranchFormSet.mDoTrigger(PADR('EDITPRICE',10)) 
      loBranchFormSet.AriaForm1.txtGrsPrc.Value = loBranchFormSet.AriaForm1.txtGrsPrc.OldValue
    ENDIF
  ENDIF
ENDIF 
*-* HES

IF loBranchFormSet.AriaForm1.txtGrsPrc.Value < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loBranchFormSet.AriaForm1.txtGrsPrc.Value = loBranchFormSet.AriaForm1.txtGrsPrc.OldValue
  RETURN .F.
ENDIF

loBranchFormSet.AriaForm1.txtNetPri.Value = ROUND(loBranchFormSet.AriaForm1.txtGrsPrc.Value *(100-loBranchFormSet.AriaForm1.spnDisc.Value)/100,2)

*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
lcFPos = ALLTRIM(STR(loBranchFormSet.lccurrsz))
lnPosSc  = ASCAN(loBranchFormSet.laSize,ALLTRIM(loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption))
IF lnPosSc  > 0
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[Start]
  IF loBranchFormSet.llMultiDime
	=SEEK('S'+SUBSTR(Style.SCALE,1,loBranchFormSet.lnScaleLen),'Scale')
	SELECT scale 
	lnPosScnt = 0
	llScaleFound = .F.
	SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+ SUBSTR(Style.SCALE,1,loBranchFormSet.lnScaleLen) FOR;
	     IIF(loBranchFormSet.llMultiDime,ALLTRIM(cDim1)==ALLTRIM(&lcTempCur..fitdesc),.T.)
	  IF llScaleFound  
	    EXIT 
	  ENDIF    
	  FOR lnI = 1 TO Scale.Cnt
  	    lnPosScnt = lnPosScnt + 1 
	    lcI = ALLTRIM(STR(lnI,1))
	    IF ALLTRIM(UPPER(SZ&lcI)) = ALLTRIM(UPPER(PADR(loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption,5)))
	      llScaleFound  = .T.
	      EXIT 
	    ENDIF 
	  ENDFOR  
	ENDSCAN 
    IF lnPosScnt = 0
      RETURN 
    ENDIF 
	lcPos = ALLTRIM(STR(lnPosScnt))
  ELSE 
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[End]
    lnPosSc = ASUBSCRIPT(loBranchFormSet.laSize,lnPosSc,1)
    lcPos = ALLTRIM(STR(lnPosSc))
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[Start]
  ENDIF 
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[End]
  REPLACE Pric&lcPos  WITH loBranchFormSet.AriaForm1.txtGrsPrc.Value,;
          NtPri&lcPos WITH loBranchFormSet.AriaForm1.txtNetPri.Value
ENDIF 
*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

*:**************************************************************************
*:* Name        : lfvqPrcDisc                                       
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Discount on price valid fn.(C200969,1)
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqPrcDisc()
*:***************************************************************************
FUNCTION lfvqPrcDisc
PARAMETERS loBranchFormSet

*-* HES && Restric Editing prices debend on User privilege
IF loBranchFormSet.AriaForm1.spnDisc.Value <> loBranchFormSet.AriaForm1.spnDisc.OldValue
  IF ASCAN(loBranchFormSet.laEvntTrig,PADR('EDITPRICE',10),1,ALEN(loBranchFormSet.laEvntTrig,1),1) > 0
    IF !loBranchFormSet.mDoTrigger(PADR('EDITPRICE',10)) 
      loBranchFormSet.AriaForm1.spnDisc.Value = loBranchFormSet.AriaForm1.spnDisc.OldValue
    ENDIF
  ENDIF
ENDIF 
*-* HES

IF loBranchFormSet.AriaForm1.spnDisc.Value < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loBranchFormSet.AriaForm1.spnDisc.Value = loBranchFormSet.AriaForm1.spnDisc.oldValue
  RETURN .F.
ENDIF

IF loBranchFormSet.AriaForm1.spnDisc.Value  > 99.99
  *--Can not exceed 999
  *-- 40171 : ð cannot exceeds ð  
  =gfModalGen('TRM40171B00000','DIALOG','Percent Discount|99.99')
  loBranchFormSet.AriaForm1.spnDisc.Value = loBranchFormSet.AriaForm1.spnDisc.oldValue
  RETURN .F.
ENDIF
loBranchFormSet.AriaForm1.txtNetPri.Value = ROUND(loBranchFormSet.AriaForm1.txtGrsPrc.Value *(100-loBranchFormSet.AriaForm1.spnDisc.Value)/100,2)

lctempcur =  loBranchFormSet.lctempcur
*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
*FOR lnCount = 1 TO ALEN(loBranchFormSet.laExtSz,1)
*! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[Start]
*FOR lnCount = 1 TO ALEN(loBranchFormSet.laSize,1)
FOR lnCount = 1 TO  IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(loBranchFormSet.laSize,1))    
*! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[End]
*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
 
  lcCnt = ALLTRIM(STR(lnCount))
  *C200969,3 MMT 05/13/2008 Let Comm and Discount applicable on all lines{Start}
  * REPLACE Discount WITH loBranchFormSet.AriaForm1.spnDisc.Value ,;
          NtPri&lcCnt WITH ROUND(Pric&lcCnt * (100 - Discount)/100 ,2),;
          comm WITH loBranchFormSet.AriaForm1.SpnComm.Value  IN  (lctempcur)
  REPLACE ALL Discount WITH loBranchFormSet.AriaForm1.spnDisc.Value ,;
          NtPri&lcCnt WITH ROUND(Pric&lcCnt * (100 - Discount)/100 ,2),;
          comm WITH loBranchFormSet.AriaForm1.SpnComm.Value  IN  (lctempcur)
  *C200969,3 MMT 05/13/2008 Let Comm applicable on all lines{End}        
ENDFOR 



*-- end of lfvqPrcDisc.
*:**************************************************************************
*:* Name        : lfvNotes                                         
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Entering Line notes , that will be saved againest the first 
*                 color that has quantity(C200969,1)
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvNotes()
*:***************************************************************************
FUNCTION lfvNotes
PARAMETERS loBranchFormSet,loParForm

LNRECNUM = RECNO(loBranchFormSet.lctempcur)

DO FORM (oAriaApplication.ScreenHome + 'Arlnotes') WITH loBranchFormSet.lctempcur ,loParForm.oFormEnvironment.Activemode <> 'V'
*!*  SELECT (loBranchFormSet.lctempcur )
*!*  m.Mnotes = Note_mem
*!*  REPLACE  ALL Note_mem WITH m.Mnotes 
*!*  IF BETWEEN(LNRECNUM,1,RECCOUNT(loBranchFormSet.lctempcur))
*!*    GO LNRECNUM IN (loBranchFormSet.lctempcur)
*!*  ENDIF 
*REPLACE Flag WITH IIF(Flag='N' .OR. loParForm.ariaform1.ariapageframe1.page2.ariaeditregion1.Multistores,Flag,'M') IN (loParForm.ariaform1.ariapageframe1.page2.ariaeditregion1.Detailfile)
*:**************************************************************************
*:* Name        : lfvqTotQty                                       
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Valid fn for total qty(C200969,1)
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvqTotQty()
*:***************************************************************************
FUNCTION lfvqTotQty
PARAMETERS loBranchFormSet

IF IIF(TYPE('loBranchFormSet.AriaForm1.txtChkQty.VAlue') = "C",VAL(loBranchFormSet.AriaForm1.txtChkQty.VAlue),loBranchFormSet.AriaForm1.txtChkQty.VAlue) < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loBranchFormSet.AriaForm1.txtChkQty.Value = loBranchFormSet.AriaForm1.txtChkQty.OldValue 
  RETURN .F.
ENDIF
*:**************************************************************************
*:* Name        : lfvqNPrice
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Net  price valid fn(C200969,1)
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
PARAMETERS loBranchFormSet

*-* HES && Restric Editing prices debend on User privilege
IF loBranchFormSet.AriaForm1.txtNetPri.Value <> loBranchFormSet.AriaForm1.txtNetPri.OldValue
  IF ASCAN(loBranchFormSet.laEvntTrig,PADR('EDITPRICE',10),1,ALEN(loBranchFormSet.laEvntTrig,1),1) > 0
    IF !loBranchFormSet.mDoTrigger(PADR('EDITPRICE',10)) 
      loBranchFormSet.AriaForm1.txtNetPri.Value = loBranchFormSet.AriaForm1.txtNetPri.OldValue
    ENDIF
  ENDIF
ENDIF 
*-* HES

IF loBranchFormSet.AriaForm1.txtNetPri.Value < 0
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loBranchFormSet.AriaForm1.txtNetPri.Value  = loBranchFormSet.AriaForm1.txtNetPri.OldValue 
  RETURN .F.
ENDIF

loBranchFormSet.AriaForm1.txtNetPri.Value  = IIF(loBranchFormSet.AriaForm1.txtNetPri.Value>loBranchFormSet.AriaForm1.txtGrsPrc.Value,loBranchFormSet.AriaForm1.txtGrsPrc.Value,loBranchFormSet.AriaForm1.txtNetPri.Value)

loBranchFormSet.AriaForm1.spnDisc.Value  = IIF(loBranchFormSet.AriaForm1.txtGrsPrc.Value=0,0,100-loBranchFormSet.AriaForm1.txtNetPri.Value*100/loBranchFormSet.AriaForm1.txtGrsPrc.Value)

*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
lcFPos = ALLTRIM(STR(loBranchFormSet.lccurrsz))
lnPosSc  = ASCAN(loBranchFormSet.laSize,ALLTRIM(loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption))
IF lnPosSc  > 0
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[Start]
  IF loBranchFormSet.llMultiDime
	=SEEK('S'+SUBSTR(Style.SCALE,1,loBranchFormSet.lnScaleLen),'Scale')
	SELECT scale 
	lnPosScnt = 0
	llScaleFound = .F.
	SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+ SUBSTR(Style.SCALE,1,loBranchFormSet.lnScaleLen) FOR;
	     IIF(loBranchFormSet.llMultiDime,ALLTRIM(cDim1)==ALLTRIM(&lcTempCur..fitdesc),.T.)
	  IF llScaleFound  
	    EXIT 
	  ENDIF    
	  FOR lnI = 1 TO Scale.Cnt
  	    lnPosScnt = lnPosScnt + 1 
	    lcI = ALLTRIM(STR(lnI,1))
	    IF ALLTRIM(UPPER(SZ&lcI)) = ALLTRIM(UPPER(PADR(loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption,5)))
	      llScaleFound  = .T.
	      EXIT 
	    ENDIF 
	  ENDFOR  
	ENDSCAN 
    IF lnPosScnt = 0
      RETURN 
    ENDIF 
	lcPos = ALLTRIM(STR(lnPosScnt))
  ELSE 
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[End]
    lnPosSc = ASUBSCRIPT(loBranchFormSet.laSize,lnPosSc,1)
    lcPos = ALLTRIM(STR(lnPosSc))
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[Start]
  ENDIF
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[End]
  
  REPLACE Pric&lcPos  WITH loBranchFormSet.AriaForm1.txtGrsPrc.Value,;
          NtPri&lcPos WITH loBranchFormSet.AriaForm1.txtNetPri.Value
ENDIF 
*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

*-- end of lfvqNPrice.
*:**************************************************************************
*:* Name        : lfClose
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Close Button Function(C200969,1)
*:***************************************************************************
FUNCTION lfClose
PARAMETERS loBranchFormSet
llClose  = .F.
lctempcur = loBranchFormSet.lctempcur
lnRecno = RECNO(lctempcur)
SELECT (lctempcur)
LOCATE FOR StyMajor = CHR(255)
IF FOUND() AND &lctempcur..nTotal > 0
  llClose = (gfModalGen('QRM38093B32005','ALERT','Quantities have been entered';
                                                +'|The Quick Order Entry screen')=1)
ELSE
 IF FOUND() AND &lctempcur..nTotal = 0                                                  
   loBranchFormSet.Release
 ENDIF 
ENDIF 
IF llClose 
  loBranchFormSet.Release
ELSE
  IF BETWEEN(lnRecno ,1,RECCOUNT(lctempcur))
    GO lnRecno  IN (lctempcur)
  ENDIF
  RETURN   
ENDIF 
*:**************************************************************************
*:* Name        : lfCrtTempFiles
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Create Temp Files(C200969,1)
*:***************************************************************************
FUNCTION lfCrtTempFiles
PARAMETERS loBranchFormSet

*! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
*  DIMENSION laTblFld[8,4]
  DIMENSION laTblFld[9,4]
*! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[End]
  laTblFld[1,1] = "STYMAJOR"
  laTblFld[1,2] = "C"
  laTblFld[1,3] = 19
  laTblFld[1,4] = 0
  
  laTblFld[2,1] = "COLOR"
  laTblFld[2,2] = "C"
  laTblFld[2,3] = lnClrLen
  laTblFld[2,4] = 0
 
  
  laTblFld[3,1] = "COLORDSC"
  laTblFld[3,2] = "C"
  laTblFld[3,3] = 20
  laTblFld[3,4] = 0

  laTblFld[4,1] = "NTOTAL"
  laTblFld[4,2] = "N"
  laTblFld[4,3] = 6
  laTblFld[4,4] = 0
  
  laTblFld[5,1] = "NOTE_MEM"
  laTblFld[5,2] = "M"
  laTblFld[5,3] = 10
  laTblFld[5,4] = 0
  
  
  laTblFld[6,1] = "Discount"
  laTblFld[6,2] = "N"
  laTblFld[6,3] = 5
  laTblFld[6,4] = 2

  laTblFld[7,1] = "Comm"
  laTblFld[7,2] = "N"
  laTblFld[7,3] = 5
  laTblFld[7,4] = 2
  
  laTblFld[8,1] = "FitDesc"
  laTblFld[8,2] = "C"
  laTblFld[8,3] = 10
  laTblFld[8,4] = 0
  
  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
  laTblFld[9,1] = "SCALE"
  laTblFld[9,2] = "C"
  laTblFld[9,3] = 3
  laTblFld[9,4] = 0
  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[End]
  
   
  IF loBranchFormSet.llMultiDime

    FOR lnI = 1 TO lnMaxSz
      lcI = ALLTRIM(STR(lnI))
      DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
      laTblFld[ALEN(laTblFld,1),1] = 'SZ'+lcI
      laTblFld[ALEN(laTblFld,1),2] = "C"
      laTblFld[ALEN(laTblFld,1),3] = 5
      laTblFld[ALEN(laTblFld,1),4] = 0
    ENDFOR
     
    FOR lnI = 1 TO lnMaxSz
      lcI = ALLTRIM(STR(lnI))
      DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
      laTblFld[ALEN(laTblFld,1),1] = 'QTY'+lcI
      laTblFld[ALEN(laTblFld,1),2] = "N"
      laTblFld[ALEN(laTblFld,1),3] = 5
      laTblFld[ALEN(laTblFld,1),4] = 0
    ENDFOR 
    
  ELSE
    FOR lnI = 1 TO ALEN(laSize,1)
      lcI = ALLTRIM(STR(lnI))
      DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
      laTblFld[ALEN(laTblFld,1),1] = 'SZ'+lcI
      laTblFld[ALEN(laTblFld,1),2] = "C"
      laTblFld[ALEN(laTblFld,1),3] = 5
      laTblFld[ALEN(laTblFld,1),4] = 0
    ENDFOR
     
    FOR lnI = 1 TO ALEN(laSize,1)
      lcI = ALLTRIM(STR(lnI))
      DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
      laTblFld[ALEN(laTblFld,1),1] = 'QTY'+lcI
      laTblFld[ALEN(laTblFld,1),2] = "N"
      laTblFld[ALEN(laTblFld,1),3] = 5
      laTblFld[ALEN(laTblFld,1),4] = 0
    ENDFOR 
  ENDIF   
  
 *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]  
 *FOR lnCount = 1 TO ALEN(laExtSz,1)
 *B608983,1 TMI [START] Change the loop length based on llMultiDime variable
 *FOR lnCount = 1 TO ALEN(laSize,1)
 FOR lnCount = 1 TO IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(laSize,1))
 *B608983,1 TMI [END  ] Change the loop length based on llMultiDime variable
 *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]  
 
    lcI = ALLTRIM(STR(lnCount))
    DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'Pric'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "N"
    laTblFld[ALEN(laTblFld,1),3] = 7
    laTblFld[ALEN(laTblFld,1),4] = 2
  ENDFOR 

  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]  
  *FOR lnCount = 1 TO ALEN(laExtSz,1)
  *B608983,1 TMI [START] Change the loop length based on llMultiDime variable
  *FOR lnCount = 1 TO ALEN(laSize,1)
  FOR lnCount = 1 TO IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(laSize,1))
  *B608983,1 TMI [END  ] Change the loop length based on llMultiDime variable

  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]    
  
    lcI = ALLTRIM(STR(lnCount))
    DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'NtPri'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "N"
    laTblFld[ALEN(laTblFld,1),3] = 7
    laTblFld[ALEN(laTblFld,1),4] = 2
  ENDFOR 

 *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
  *=gfCrtTmp(loBranchFormSet.lctempcur ,@laTblFld,'STYMAJOR+COLOR+FitDesc',loBranchFormSet.lctempcur )
    =gfCrtTmp(loBranchFormSet.lctempcur ,@laTblFld,'STYMAJOR+COLOR+SCALE',loBranchFormSet.lctempcur )  
 *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
  
  *B608983,1 TMI [START] Change the loop length based on llMultiDime variable
  *FOR lnI = 1 TO ALEN(laSize,1)
  FOR lnI = 1 TO IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(laSize,1))
  *B608983,1 TMI [END  ] Change the loop length based on llMultiDime variable  
    lcI = ALLTRIM(STR(lnI))
    DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'FStk'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "N"
    laTblFld[ALEN(laTblFld,1),3] = 5
    laTblFld[ALEN(laTblFld,1),4] = 0
  ENDFOR 

  FOR lnI = 1 TO IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(laSize,1))
    lcI = ALLTRIM(STR(lnI))
    DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'PhStk'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "N"
    laTblFld[ALEN(laTblFld,1),3] = 5
    laTblFld[ALEN(laTblFld,1),4] = 0
  ENDFOR 
  
  FOR lnI = 1 TO IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(laSize,1))
    lcI = ALLTRIM(STR(lnI))
    DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'PStk'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "N"
    laTblFld[ALEN(laTblFld,1),3] = 5
    laTblFld[ALEN(laTblFld,1),4] = 0
  ENDFOR 
  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
  *  =gfCrtTmp(loBranchFormSet.lcTmpStk ,@laTblFld,'STYMAJOR+COLOR+FitDesc',loBranchFormSet.lcTmpStk)  
  =gfCrtTmp(loBranchFormSet.lcTmpStk ,@laTblFld,'STYMAJOR+COLOR+SCALE',loBranchFormSet.lcTmpStk)  
  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[End]

*:**************************************************************************
*:* Name        : lfAddCntrSrc
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Add Control Source to Grids(C200969,1)
*:***************************************************************************
FUNCTION lfAddCntrSrc
PARAMETERS loBranchFormSet
WITH loBranchFormSet.AriaForm1.grdLines
  .ColumnCount =IIF(loBranchFormSet.llMultiDime,lnMaxSz,ALEN(laSize,1))+3
  .RecordSource = loBranchFormSet.lctempcur 
  .Column1.Readonly = .T.
  .Column1.Header1.Caption ='Colour'
  .Column1.ControlSource  = loBranchFormSet.lctempcur +'.COLORDSC'
  .Column2.Readonly = .T.
  .Column2.Header1.Caption ='Fit'
  .Column2.ControlSource  = loBranchFormSet.lctempcur +'.FitDesc'
  
  lnCount = 2
  FOR lnT = 1 TO IIF(loBranchFormSet.llMultiDime,lnMaxSz,ALEN(laSize,1))
    lnCount = lnCount + 1
    lcT = ALLTRIM(STR(lnT))
    lcCount= ALLTRIM(STR(lnCount))
    .Column&lcCount..ControlSource = loBranchFormSet.lctempcur +'.QTY'+lcT
    .Column&lcCount..Header1.Caption = EVALUATE(loBranchFormSet.lctempcur +'.SZ'+lcT)
    .Column&lcCount..Enabled = .T.
    .Column&lcCount..Format = '99999'
    .Column&lcCount..InputMAsk = '99999'

    *! C201129,1 MMT 04/06/2009 Reduce width of columns[Start]
    .Column&lcCount..Width = 50
    *! C201129,1 MMT 04/06/2009 Reduce width of columns[End]

    .Column&lcCount..Readonly = .F.
    BINDEVENT(.Column&lcCount..Text1,"GotFocus",loBranchFormSet,'lfvgtstyszqty')
    BINDEVENT(.Column&lcCount..Text1,"LostFocus",loBranchFormSet,'lfvStySzqty')
  ENDFOR 
  lnCount = lnCount + 1
  lcCount= ALLTRIM(STR(lnCount))
  .Column&lcCount..ControlSource = loBranchFormSet.lctempcur +'.Ntotal'
  .Column&lcCount..Header1.Caption = 'Total'
  .Column&lcCount..Readonly = .T.
ENDWITH 

WITH loBranchFormSet.AriaForm1.grdStk
  .RecordSource = loBranchFormSet.lcTmpStk 
  .ColumnCount =IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(laSize,1))+2
  .Column1.ControlSource = loBranchFormSet.lcTmpStk +'.COLORDSC'
  .Column1.Header1.Caption = ''
  .Column1.Readonly = .T.
  lnCount = 1
  FOR lnT = 1 TO IIF(loBranchFormSet.llMultiDime,lnMaxSz,ALEN(laSize,1))
    lnCount = lnCount + 1
    lcT = ALLTRIM(STR(lnT))
    lcCount= ALLTRIM(STR(lnCount))
    .Column&lcCount..ControlSource = loBranchFormSet.lcTmpStk +'.QTY'+lcT
    .Column&lcCount..Header1.Caption = EVALUATE(loBranchFormSet.lcTmpStk +'.SZ'+lcT)
    .Column&lcCount..Readonly = .T.
  ENDFOR
  lnCount = lnCount + 1
  lcCount= ALLTRIM(STR(lnCount))
  .Column&lcCount..ControlSource = loBranchFormSet.lcTmpStk +'.Ntotal'
  .Column&lcCount..Header1.Caption = 'Total'
  .Column&lcCount..Readonly = .T.
ENDWITH 

loBranchFormSet.AriaForm1.grdStk.refresh    
loBranchFormSet.AriaForm1.grdLines.refresh    

*:**************************************************************************
*:* Name        : lfvStySzqty
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Valid fn. for fields in grid(C200969,1)
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvBrFld()
*:***************************************************************************
FUNCTION lfvStySzqty
PARAMETERS loBranchFormSet,loParentForm
PRIVATE lcFPos

lcTempCur = loBranchFormSet.lctempcur 
lcFPos = ALLTRIM(STR(loBranchFormSet.lccurrsz))
lnCurrSz  = loBranchFormSet.ariaform1.grdLines.column&lcFPos..ControlSource
PRIVATE lnLineTot,lnCount,lnRecno,m.AddSty,lnIncrm

SELECT (lcTempCur)
IF STYMAJOR = CHR(255) 
  REPLACE &lnCurrSz WITH loBranchFormSet.lnOldVAlue   IN (lcTempCur)
  RETURN 
ENDIF 
IF EMPTY(loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption)
  REPLACE &lnCurrSz WITH 0 IN (lcTempCur)
  RETURN
ENDIF 


IF loBranchFormSet.lnOldVAlue  #  EVALUATE(lnCurrSz)  
  IF EVALUATE(lnCurrSz) < 0
    *--Can not accept negative values
    =gfModalGen('TRM42000B40011','DIALOG')
    
    REPLACE &lnCurrSz WITH loBranchFormSet.lnOldVAlue  
    RETURN 
  ENDIF
  IF EVALUATE(lnCurrSz) > 99999
    *--Can not exceed 999
    *-- 40171 : ð cannot exceeds ð  
    =gfModalGen('TRM40171B00000','DIALOG','Size Quantity|99999.')
    REPLACE &lnCurrSz WITH loBranchFormSet.lnOldVAlue  
    RETURN
  ENDIF
*!*    *C200587,1  TMI [Start] Check if style.soldout date is earlier than order entered date
*!*    IF llWRNSLDEN 
*!*      PRIVATE lcStyl
*!*      lcStyl = STYMAJOR+lcSepart+COLOR+lcClrSpr+laExtSz[lfSclPos( lnIndex+10*(lnFldGrp-1) ),1]
*!*      IF SEEK(lcStyl,'STYLE') .AND. ;
*!*        !EMPTY(STYLE.SOLDOUT) .AND. STYLE.SOLDOUT < laData[8] .AND. ;
*!*        gfModalGen('QRM00000B00006',.F.,.F.,.F.,'You have selected a style/colour that has passed its '+;
*!*                                                'Sold Out Date - Do you wish to continue ?') = 2
*!*        REPLACE S&lcIndex WITH 0
*!*        RETURN
*!*      ENDIF
*!*    ENDIF
*!*    *C200587,1  TMI [End  ] 

  m.AddSty = PADR(STYMAJOR,loBranchFormSet.lnmajorlen) +loBranchFormSet.lcstysp +SUBSTR(COLOR,1,loBranchFormSet.lnClrLen)
  SELECT (lcTempCur)
  IF loParentForm.oFormEnvironment.laSetups[5,2]='Y'
    lcSclPos = lfGetScale(loBranchFormSet,loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption,&lcTempCur..FitDesc)
    *-- Assign a style to a location if not Assigned befor
    
    *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]        
    *IF !SEEK(PADR(m.AddSty+IIF(loBranchFormSet.llExtended,loBranchFormSet.lcsclsp+lcSclPos,'') ,19)+loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value+SPACE(10),'StyDye')
    IF !SEEK(PADR(m.AddSty+IIF(loBranchFormSet.llExtended,ALLTRIM(loBranchFormSet.lcsclsp)+lcSclPos,'') ,19)+loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value+SPACE(10),'StyDye')
    *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]        
    
      *E300408,1 Message : 40012
      *E300408,1 Style/color xxx is not assigned to warehouse xxx
      *E300408,1 Button : 40002
      *E300408,1 Add Reenter
      
      *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]        
      *IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.AddSty+IIF(loBranchFormSet.llExtended,loBranchFormSet.lcsclsp+lcSclPos,'') )+'|'+TRIM(loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value))=1
      IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.AddSty+IIF(loBranchFormSet.llExtended,ALLTRIM(loBranchFormSet.lcsclsp)+lcSclPos,'') )+'|'+TRIM(loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value))=1
      *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]        
      
        *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]        
        *DO gpAdStyWar WITH PADR(m.AddSty+IIF(loBranchFormSet.llExtended,loBranchFormSet.lcsclsp+lcSclPos,'') ,19),SPACE(10),loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value
        DO gpAdStyWar WITH PADR(m.AddSty+IIF(loBranchFormSet.llExtended,ALLTRIM(loBranchFormSet.lcsclsp)+lcSclPos,'') ,19),SPACE(10),loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value
        *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]        
      ELSE      
        REPLACE &lnCurrSz WITH 0
        RETURN
      ENDIF
    ENDIF

  ENDIF
  SELECT (lcTempCur)
  lnLineTot = 0
  FOR lnT = 1 TO IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(loBranchFormSet.laSize,1) ) 
    lcT = ALLTRIM(STR(lnT))
    lnLineTot = lnLineTot + EVALUATE(lcTempCur+'.QTY'+lcT)  
  ENDFOR 
  REPLACE NTOTAL WITH lnLineTot
ENDIF  
lfCalcTot(loBranchFormSet)
*-- end of lfvBrFld.
*:**************************************************************************
*:* Name        : lfSclPos
*:* Developer   : Mariam Mazhar
*:* Date        : 03/27/2008(C200969,1)
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
FUNCTION lfSclPos
PARAMETERS lnIndex,loBarnFormSet
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
*:**************************************************************************
*:* Name        : lfGetScale
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : get Scale of sent size(C200969,1)
*:***************************************************************************
FUNCTION lfGetScale
PARAMETERS loBarnFormSet,lcScale,lcFit
lcRetScale = ''
=SEEK('S'+SUBSTR(Style.SCALE,1,loBarnFormSet.lnScaleLen),'Scale')
SELECT scale 
SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+ SUBSTR(Style.SCALE,1,loBarnFormSet.lnScaleLen) FOR;
     IIF(loBarnFormSet.llMultiDime,ALLTRIM(cDim1)==ALLTRIM(lcFit),.T.)
  FOR lnI = 1 TO 8
    lcI = ALLTRIM(STR(lnI,1))
    IF ALLTRIM(UPPER(SZ&lcI)) = ALLTRIM(UPPER(PADR(lcScale,5)))
      lcRetScale = Scale.Scale
      EXIT 
    ENDIF 
  ENDFOR  
ENDSCAN 
RETURN lcRetScale 

*:**************************************************************************
*:* Name        : lfCalcTot
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Calculate Totals(C200969,1)
*:***************************************************************************
FUNCTION lfCalcTot
PARAMETERS loBarnFormSet
SELECT (loBarnFormSet.lcTempCur)
lnRecn = RECNO()
GO BOTTOM 

FOR lnC = 1 TO IIF(loBarnFormSet.llMultiDime ,loBarnFormSet.lnMaxSz,ALEN(loBarnFormSet.laSize,1))
  lcC = ALLTRIM(STR(lnC))
  SELECT (loBarnFormSet.lcTempCur)
  SUM (Qty&lcC) TO lnTotal FOR StyMajor <> CHR(255)
  GO BOTTOM 
  IF StyMajor = CHR(255)
    REPLACE Qty&lcC WITH lnTotal
  ENDIF   
ENDFOR 

SELECT (loBarnFormSet.lcTempCur)
SUM nTotal TO lnTotal FOR StyMajor <> CHR(255)
GO BOTTOM 
IF StyMajor = CHR(255)
  REPLACE ntotal WITH lnTotal 
ENDIF   
loBarnFormSet.AriaForm1.CmdSave.Enabled = lnTotal > 0  


SELECT (loBarnFormSet.lcTempCur)
IF BETWEEN(lnRecn ,1,RECCOUNT())
  GO lnRecn 
ENDIF 

*:**************************************************************************
*:* Name        : lfGetStock
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : get Style Stock Values(C200969,1)
*:***************************************************************************
FUNCTION lfGetStock
PARAMETERS loBarnFormSet

IF !USED('BOM')
  =gfOpenTable('BOM','MULTIBOM','SH')
endif 


SELECT(loBarnFormSet.lcTmpStk)
lcTmpStk = loBarnFormSet.lcTmpStk
SCAN FOR !INLIST(UPPER(COLORDSC) ,UPPER('Free Stock'),UPPER('Phys. Stock'),UPPER('Plain Stock'))
  DIMENSION laFrStk[IIF(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,ALEN(loBarnFormSet.laSize,1))],;
            laPhyStk[IIF(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,ALEN(loBarnFormSet.laSize,1))],;
            laPlainStk[IIF(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,ALEN(loBarnFormSet.laSize,1))]
            
  STORE 0 to laFrStk,laPhyStk,laPlainStk
  lnCounting = 1
  SELECT SCALE
  =SEEK('S'+LEFT(Style.Scale,loBarnFormSet.lnScaleLen))
  SCAN REST WHILE type+scale+prepak = 'S' + LEFT(Style.Scale,loBarnFormSet.lnScaleLen) FOR ;
       IIF(loBarnFormSet.llMultiDime,ALLTRIM(Scale.CDIM1)==ALLTRIM(&lcTmpStk..FITDESC),.T.)
    
    *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]               
    *lcSeekSty = PADR(&lcTmpStk..STYMAJOR,lobarnformset.lnmajorlen ) +loBarnFormSet.lcstysp +PADR(&lcTmpStk..COLOR,lnClrLen)+IIF(loBranchFormSet.llExtended,loBarnFormSet.lcsclsp+Scale.Scale,'')
    lcSeekSty = PADR(&lcTmpStk..STYMAJOR,lobarnformset.lnmajorlen ) +loBarnFormSet.lcstysp +PADR(&lcTmpStk..COLOR,lnClrLen)+IIF(loBranchFormSet.llExtended,ALLTRIM(loBarnFormSet.lcsclsp)+Scale.Scale,'')
    *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]        
    
    lcSeekSty = PADR(lcSeekSty,19)
    =SEEK(lcSeekSty+loBarnFormSet.loparentform.ariaform1.ariapageframe1.page1.cboWarehouse.Value,'STYDYE')
    =SEEK(lcSeekSty,'STYLE')
    FOR I = 1 To Scale.Cnt
      lcCountNo = ALLTRIM(STR(I))
      *C200969,3 MMT 05/13/2008 Fix bug of wrong Free stock Value{Start}
      *laFrStk[lnCounting] = STYDYE.Stk&lcCountNo - STYDYE.ALO&lcCountNo+IIF(loBarnFormSet.llincwip,STYDYE.WIP&lcCountNo,0)
      laFrStk[lnCounting] = STYDYE.Stk&lcCountNo - STYDYE.Ord&lcCountNo+IIF(loBarnFormSet.llincwip,STYDYE.WIP&lcCountNo,0)
      *C200969,3 MMT 05/13/2008 Fix bug of wrong Free stock Value{End}
      
      laPhyStk[lnCounting] = STYDYE.Stk&lcCountNo
      
      
      IF gfSEEK('0001'+PADR(SUBSTR(lcSeekSty,1,loBarnFormSet.lnMajorLen),19),'BOM')
        SELECT BOM
        SCAN REST WHILE cinvtype+ citmmajor+ ccstshttyp+ ccstsht_id+ typ+ citmmask+ mfgcode+cinvtypc+ item+ STR(nlineno,6) = '0001'+PADR(SUBSTR(lcSeekSty,1,loBarnFormSet.lnMajorLen),19) ;
             FOR cinvtypc = '0001'
          lcCompSty = STUFF(lcSeekSty,1,loBarnFormSet.lnMajorLen,SUBSTR(BOM.ITEM,1,loBarnFormSet.lnMajorLen))
          lnRecNoStyDye = RECNO('STYDYE')
          =SEEK(lcCompSty+loBarnFormSet.loparentform.ariaform1.ariapageframe1.page1.cboWarehouse.Value+SPACE(10),'STYDYE')
          
          *C200969,3 MMT 05/13/2008 Fix bug of wrong Free stock Value{Start}
          *laPlainStk[lnCounting] = STYDYE.Stk&lcCountNo - STYDYE.ALO&lcCountNo
          laPlainStk[lnCounting] = STYDYE.Stk&lcCountNo 
          *C200969,3 MMT 05/13/2008 Fix bug of wrong Free stock Value{End}
          
          IF BETWEEN(lnRecNoStyDye ,1,RECCOUNT('STYDYE'))
            GO lnRecNoStyDye  IN STYDYE
          ENDIF 
        ENDSCAN 
      ENDIF 
      lnCounting  = lnCounting + 1
    ENDFOR   
    SELECT Scale 
  ENDSCAN 

  SELECT(loBarnFormSet.lcTmpStk)
  REPLACE COLORDSC WITH 'Free Stock'
  REPLACE Ntotal WITH 0
  FOR lnC = 1 TO IIF(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,ALEN(loBarnFormSet.laSize,1))
    lcC = ALLTRIM(STR(lnC))
    REPLACE QTY&lcC WITH laFrStk[lnC]
    REPLACE Ntotal WITH Ntotal + QTY&lcC
  ENDFOR 
  SCATTER MEMO MEMVAR 
  m.COLORDSC = 'Phys. Stock'
  APPEND BLANK 
  m.Ntotal = 0
  FOR lnC = 1 TO IIF(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,ALEN(loBarnFormSet.laSize,1))
    lcC = ALLTRIM(STR(lnC))
    m.QTY&lcC = laPhyStk[lnC]
    m.Ntotal = m.Ntotal + m.QTY&lcC
  ENDFOR 
  gathER MEMO MEMVAR 
  m.COLORDSC = 'Plain Stock'
  APPEND BLANK
  m.Ntotal = 0 
  FOR lnC = 1 TO IIF(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,ALEN(loBarnFormSet.laSize,1))
    lcC = ALLTRIM(STR(lnC))
    m.QTY&lcC = laPlainStk[lnC]
    m.Ntotal = m.Ntotal + m.QTY&lcC
  ENDFOR 
  gathER MEMO MEMVAR 
ENDSCAN 

*:**************************************************************************
*:* Name        : lfvSave
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Save button function(C200969,1)
*:***************************************************************************
FUNCTION lfvSave
PARAMETERS loBranchFormSet


lnTotal =    0
lcTempCur = loBranchFormSet.lctempcur 
SELECT (lctempcur)
LOCATE FOR stymajor = CHR(255)
IF FOUND() 
  IF &lctempcur..Ntotal = 0
    RETURN .F.
  ELSE
   lnTotal =  &lctempcur..Ntotal   
  ENDIF  
ELSE
  RETURN .F.    
ENDIF 

lcOrdHDr  = loBranchFormSet.loparentform.OFORMENVIRONMENT.lcOrdHdr
lcOrdLine = loBranchFormSet.loparentform.ariaform1.ariapageframe1.page2.ariaeditregion1.Detailfile
m.Style   = loBranchFormSet.AriaForm1.kbstymaj.KeyTextBox.Value
SELECT (lcOrdLine)
IF loBranchFormSet.loparentform.ariaform1.ariapageframe1.page2.ariaeditregion1.Editlinecompletedate AND SEEK(loBranchFormSet.loparentform.lcOrdType+&lcOrdHDr..order+&lcOrdHDr..Store+SUBSTR(m.STYLE,1,loBranchFormSet.lnMajorLen),lcOrdLine,'ORDLINST')
  LOCATE REST WHILE CORDTYPE+ORDER+STORE+STYLE = loBranchFormSet.loparentform.lcOrdType+&lcOrdHDr..order+&lcOrdHDr..Store+SUBSTR(m.STYLE,1,loBranchFormSet.lnMajorLen);
              FOR COMPLETE = loBranchFormSet.AriaForm1.dtpCompDate.value
              
  IF FOUND()              
    lnResp = gfModalGen('QRM00000B32014',.F.,.F.,.F.,;
                      'The selected complete date has been used on a previous ORDLINE for that Style')
    IF lnResp = 1
    ELSE
      IF lnResp = 2
        loBranchFormSet.AriaForm1.dtpCompDate.SetFocus()
      ELSE
       =lfvClear(loBranchFormSet) 
      ENDIF
      RETURN
    ENDIF                      
  ENDIF 
ENDIF

SELECT (lcTempCur)
PRIVATE lcFilter
lcFilter = FILTER()
SET ORDER TO TAG (lcTempCur)
SET FILTER TO 
GO TOP
SELECT (lcTempCur)

llDfChkQty  = .F.

IF IIF(TYPE('loBranchFormSet.AriaForm1.txtChkQty.VAlue') = "C",VAL(loBranchFormSet.AriaForm1.txtChkQty.VAlue),loBranchFormSet.AriaForm1.txtChkQty.VAlue) > 0
  llDfChkQty = IIF(TYPE('loBranchFormSet.AriaForm1.txtChkQty.VAlue') = "C",VAL(loBranchFormSet.AriaForm1.txtChkQty.VAlue),loBranchFormSet.AriaForm1.txtChkQty.VAlue) <> lnTotal 
ENDIF
   

IF llDfChkQty AND gfModalGen('QRM32031B32000','ALERT',ALLTRIM(STR(lnTotal,8)))=2  
  SELECT (lcTempCur)
  SET FILTER TO &lcFilter
  GO TOP
  RETURN .F.
ENDIF

m.CORDTYPE  = loBranchFormSet.loparentform.lcOrdType
m.ORDER     = &lcOrdHDr..order
m.ACCOUNT   = &lcOrdHDr..ACCOUNT   
m.STORE     = &lcOrdHDr..STORE     
m.CustPo    = IIF(!&lcOrdHdr..multipo ,&lcOrdHdr..CustPo,'')   &&Save Cust PO # in OrdLine if not Multi Store
m.cWareCode = loBranchFormSet.loparentform.ariaform1.ariapageframe1.page1.cboWarehouse.Value
m.Start     = loBranchFormSet.loparentform.ariaform1.ariapageframe1.page1.txtStart.Value

DO lpQukSave

lfvClear(loBranchFormSet)

*:*************************************************************
*: Name         : lpQukSave
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*: Purpose      : Procedure to save the data into temp file.(C200969,1)
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
PROCEDURE lpQukSave
Private lnPrvsAls , lnSizeSeq , lnScaleLen , lnQkCnt , lnGrprice , lnCommPrc,;
        lcOldColor , llSkip

lcOldColor = ''
lnPrvsAls = SELECT (0)
llSkip = .F.



lnScaleLen = loBranchFormSet.lnScaleLen      && Extended size Scale ID Length.
SELECT (lcTempCur)
SET FILTER TO
LOCATE
STORE 0 TO lnGrprice , lnCommPrc , lnQkCnt
SCAN FOR  StyMajor <> CHR(255) AND Ntotal > 0
  m.Note_Mem = Note_mem
  lnInc = 1 
  
  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
  DIMENSION laAddSz[1]
  STORE '' TO laAddSz
  STORE 0 TO lnGrosprice ,lnSzGrp
  STORE .F. TO llRealPrice
  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
            
  FOR lnQkCnt = 1 TO ALEN(loBranchFormSet.laExtSz,1)
    SELECT (lcTempCur)
   
   
    *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
    IF lnSzGrp > 0
      lnQkCnt = lnSzGrp 
      lnInc = lnInc - loBranchFormSet.laExtSz[lnQkCnt,2] 
    ENDIF 
    *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
    
      
    *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]               
    *m.Style = PADR(STYMAJOR,loBranchFormSet.lnMajorLen)+ loBranchFormSet.lcStySp +SUBSTR(COLOR,1,loBranchFormSet.lnClrLen )+IIF(loBranchFormSet.llExtended,loBranchFormSet.lcSclSp+ loBranchFormSet.laExtSz[lnQkCnt,1] , '' )
    m.Style = PADR(PADR(STYMAJOR,loBranchFormSet.lnMajorLen)+ loBranchFormSet.lcStySp +SUBSTR(COLOR,1,loBranchFormSet.lnClrLen )+IIF(loBranchFormSet.llExtended,ALLTRIM(loBranchFormSet.lcSclSp)+ loBranchFormSet.laExtSz[lnQkCnt,1] , '' ),19)
    *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]               
    
    =SEEK('S'+loBranchFormSet.laExtSz[lnQkCnt,1],'SCALE')
    IF ALLTRIM(Scale.CDim1) <> ALLTRIM(&lcTempCur..FitDesc)
      LOOP 
    ENDIF 
    
    WAIT WINDOW 'Collect data for Style # : ' + M.Style +'...'    NOWAIT
    IF SEEK(m.Style,'STYLE')
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,;
                 m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8,m.TotBook,;
                 lnGrprice , lnCommPrc
      
      
      
      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
      STORE 0 TO lnNetPrice
      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

      
      FOR lnJ = 1 To loBranchFormSet.laExtSz[lnQkCnt,2]
        IF IIF(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz < lnInc ,.F.)
          lnInc = 1
        ENDIF 
         
        lcInc = ALLTRIM(STR(lnInc))
        lcQ = STR(lnJ,1)
        m.TotQty   = m.TotQty +  QTY&lcInc
        m.Qty&lcQ  = QTY&lcInc
        m.Book&lcQ = QTY&lcInc
        
        lnGrprice  = loBranchFormSet.AriaForm1.txtGrsPrc.Value
        lnCommPrc  = loBranchFormSet.AriaForm1.SpnComm.Value
        
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
        lnSzGrp = lnQkCnt 
        IF ASCAN(laAddSz,lcInc,1)= 0
          IF lnGrosprice = 0 AND !llRealPrice 
            lnGrosprice = &lcTempCur..Pric&lcInc
            lnNetPrice = &lcTempCur..NtPri&lcInc
            llRealPrice = .T.
            IF EMPTY(laAddSz[1])
              laAddSz[1] = lcInc
            ELSE
              DIMENSION laAddSz[ALEN(laAddSz,1)+1]
              laAddSz[ALEN(laAddSz,1)]= lcInc
            ENDIF  
            
          ELSE
            IF lnGrosprice =&lcTempCur..Pric&lcInc
              IF EMPTY(laAddSz[1])
                laAddSz[1] = lcInc
              ELSE
                DIMENSION laAddSz[ALEN(laAddSz,1)+1]
                laAddSz[ALEN(laAddSz,1)]= lcInc
              ENDIF 
            ELSE
              m.TotQty   = m.TotQty -  QTY&lcInc
              m.Qty&lcQ  = 0
              m.Book&lcQ = 0
            ENDIF 
          ENDIF   
        ELSE
          m.TotQty   = m.TotQty -  QTY&lcInc
          m.Qty&lcQ  = 0
          m.Book&lcQ = 0
        ENDIF   
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
        
        lnInc  = lnInc + 1
      ENDFOR

      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
  	  lnGrprice = lnGrosprice 
  	  STORE 0 TO lnGrosprice 
	  STORE .F. TO llRealPrice
      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

      *-- Don't add any record with totalQty =Equal zero.
      IF m.TotQty = 0
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
        DIMENSION laAddSz[1]
        STORE '' TO laAddSz
        lnSzGrp = 0
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
        LOOP
      ENDIF
      
      
      m.TotBook  = m.TotQty
      m.Scale    = STYLE.SCALE     
      m.Desc1    = STYLE.Desc1 
      m.PrePak   = Style.PrePak
      m.Season   = Style.Season
      m.Gl_Sales = ALLTRIM(&lcOrdHDr..Gl_Sales)+Style.cSlsGlLink
      m.Gl_Sales = IIF(loBranchFormSet.loparentform.OFORMENVIRONMENT.laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')
      m.Flag     = 'N'      
      M.COMPLETE  = loBranchFormSet.AriaForm1.dtpCompDate.value
      *-- Get price for each line of scales has different prices , 
      *-- or check qty differs from entred qty   
      lcQkCnt = ALLTRIM(STR(lnQkCnt))
      m.Disc_Pcnt =  loBranchFormSet.AriaForm1.spnDisc.Value     
      IF m.TotQty > 0
        m.Gros_Price = lnGrprice
        m.Comm1      = lnCommPrc
        m.Price      = ROUND( m.Gros_Price*(1-m.Disc_Pcnt/100) ,2 )
        
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
        *m.Gros_Price = &lcTempCur..Pric&lcQkCnt
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
        
        m.Comm1      = &lcTempCur..Comm
        
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
        *m.Price      = ROUND( &lcTempCur..NtPri&lcQkCnt ,2 )
        m.Price      = ROUND(lnNetPrice  ,2 )        
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
        
        m.Disc_Pcnt  =  &lcTempCur..Discount
        
        
        loBranchFormSet.loparentform.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo = loBranchFormSet.loparentform.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo  + 1
        m.LineNo   = loBranchFormSet.loparentform.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo 
        INSERT INTO (lcOrdLine) FROM MEMVAR

        REPLACE Book  WITH Book + m.TotBook,;
                Open  WITH open + m.TotQty ,;
                bookamt WITH bookamt + m.TotBook*m.Price,;
                openamt WITH openamt + m.TotQty*m.Price  IN (lcOrdHdr)        

        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
		IF m.TOTQTY <> 0
          lnQkCnt = lnSzGrp-1 
          LOOP 
        ENDIF         
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]                
                
      ENDIF      
    ENDIF
  ENDFOR
ENDSCAN 
WAIT CLEAR
SELECT (lnPrvsAls)
*:**************************************************************************
*:* Name        : lfAftRoColChng
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : after row Col. of the Grid(C200969,1)
*:***************************************************************************
FUNCTION lfAftRoColChng
PARAMETERS loBranchFormSet
lcTempCur = loBranchFormSet.lctempcur 

lnCount = 2
FOR lnS = 1 TO IIF(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz,ALEN(loBranchFormSet.laSize,1))
  lnCount = lnCount + 1
  lcT = ALLTRIM(STR(lnS))
  lcCount= ALLTRIM(STR(lnCount))
  loBranchFormSet.AriaForm1.grdLines.Column&lcCount..Header1.Caption = EVALUATE(loBranchFormSet.lctempcur +'.SZ'+lcT)
ENDFOR 
lfStkGridRefresh(loBranchFormSet)
IF loBranchFormSet.lccurrsz < 3 OR loBranchFormSet.lccurrsz > loBranchFormSet.ariaform1.grdLines.columnCount - 1 
  RETURN 
ENDIF 



lcFPos = ALLTRIM(STR(loBranchFormSet.lccurrsz))
lnCurrSz  = loBranchFormSet.ariaform1.grdLines.column&lcFPos..ControlSource
lcSclPos = lfGetScale(loBranchFormSet,loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption,&lcTempCur..fitdesc)
lnPosSc = 0

*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
*lnPosSc  = ASCAN(loBranchFormSet.laExtSz,lcSclPos) 
lnPosSc  = ASCAN(loBranchFormSet.laSize,ALLTRIM(loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption))
*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

IF lnPosSc  > 0 
  
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[Start]
  IF loBranchFormSet.llMultiDime
	=SEEK('S'+SUBSTR(Style.SCALE,1,loBranchFormSet.lnScaleLen),'Scale')
	SELECT scale 
	lnPosScnt = 0
	llScaleFound = .F.
	SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+ SUBSTR(Style.SCALE,1,loBranchFormSet.lnScaleLen) FOR;
	     IIF(loBranchFormSet.llMultiDime,ALLTRIM(cDim1)==ALLTRIM(&lcTempCur..fitdesc),.T.)
	  IF llScaleFound  
	    EXIT 
	  ENDIF    
	  FOR lnI = 1 TO Scale.Cnt
  	    lnPosScnt = lnPosScnt + 1 
	    lcI = ALLTRIM(STR(lnI,1))
	    IF ALLTRIM(UPPER(SZ&lcI)) = ALLTRIM(UPPER(PADR(loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption,5)))
	      llScaleFound  = .T.
	      EXIT 
	    ENDIF 
	  ENDFOR  
	ENDSCAN 
    
    IF lnPosScnt = 0
      RETURN 
    ENDIF 
	lcPos = ALLTRIM(STR(lnPosScnt))
	
  ELSE 
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[End]


    *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
	*lnPosSc = ASUBSCRIPT(loBranchFormSet.laExtSz,lnPosSc,1)
	lnPosSc = ASUBSCRIPT(loBranchFormSet.laSize,lnPosSc,1)
	*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
	  
	lcPos = ALLTRIM(STR(lnPosSc))
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[Start]
  ENDIF
  *! B609243,1 MMT 05/12/2010 Fix bug of Error while Entering Qty in Multi-Dimension Scales[End]
  WITH loBranchFormSet.AriaForm1
    .txtGrsPrc.Value = &lctempcur..Pric&lcPos 
    .txtNetPri.Value = &lctempcur..NtPri&lcPos 
    .SpnDisc.VAlue  = &lctempcur..Discount
    .SpnComm.Value = &lctempcur..Comm
  ENDWITH  
ENDIF 


*:**************************************************************************
*:* Name        : lfVComm
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Validate comm. field (C200969,1)
*:***************************************************************************
FUNCTION lfVComm
PARAMETERS loBranchFormSet
lcTempCur = loBranchFormSet.lctempcur 
*C200969,3 MMT 05/13/2008 Let Comm applicable on all lines{Start}
*REPLACE comm WITH loBranchFormSet.AriaForm1.SpnComm.Value  IN  (lctempcur)
REPLACE  ALL comm WITH loBranchFormSet.AriaForm1.SpnComm.Value  IN  (lctempcur)
*C200969,3 MMT 05/13/2008 Let Comm applicable on all lines{End}

*:**************************************************************************
*:* Name        : lfStkGridRefresh
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Refresh stock grid headers (C200969,1)
*:***************************************************************************
FUNCTION lfStkGridRefresh
PARAMETERS loBranchFormSet

lnCount = 1
FOR lnT = 1 TO IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(loBranchFormSet.laSize,1))
  lnCount = lnCount + 1
  lcT = ALLTRIM(STR(lnT))
  lcCount= ALLTRIM(STR(lnCount))
  loBranchFormSet.AriaForm1.grdStk.Column&lcCount..Header1.Caption = EVALUATE(loBranchFormSet.lcTmpStk +'.SZ'+lcT)
ENDFOR
*! C200969,1 MMT 03/27/2007 Add Quick Order entry screen to Sales order screen[End]

*! B608617,1 MMT 07/14/2008 Add trigger to update bomline with new ex rate    [Start]
*:**************************************************************************
*:* Name        : lfUPDEXRATE 
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 07/14/2008
*:* Purpose     : Add trigger to update bomline with new ex rate(B608617,1)
*:***************************************************************************
FUNCTION lfUPDEXRATE 
llGetPORate = gfGetMemVar('M_POPRDEF')

IF llGetPORate AND (&lcTmpLine..Cstytype = 'P' AND &lcTmpLine..CBUSDOCU = 'P')
  REPLACE nexrate WITH &lcTmpLine..nLanPrRat 
ENDIF   
*! B608617,1 MMT 07/14/2008 Add trigger to update bomline with new ex rate    [End]
*! C201115,1 MMT 03/11/2009 Convert PO Quick Order Entry Screen to Aria4      [Start]
*:**************************************************************************
*:* Name        : lfPOQKORD
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Add Menu to Call Quick Order entry from PO Screen
*:***************************************************************************
FUNCTION lfPOQKORD

lcHostFormName = '[' + loFormSet.cHostFormName + ']'
IF loFormSet.Ariaform1.mainWorkOrder.cWorkOrdTyp  = 'PP'
  LOCAL lnCntBar
  lnCntBar = CNTBAR('_OPTIONPOP')+1
  DEFINE BAR lnCntBar OF _OPTIONPOP PROMPT 'Quic\<k Order Entry'  SKIP FOR  ;
  			 gfFormIsActive(&lcHostFormName) AND ((TYPE('_screen.ActiveForm.PgfPOStyle') = "O" AND  _screen.ActiveForm.PgfPOStyle.ActivePage<>2) .OR. (_Screen.ActiveForm.Parent.ActiveMode = 'V'))
  ON SELECTION BAR lnCntBar  OF _OPTIONPOP DO lfOpnPQkScr
ENDIF
*:**************************************************************************
*:* Name        : lfOpnPQkScr
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Call Quick Order entry from PO Screen
*:***************************************************************************
FUNCTION lfOpnPQkScr
loFormSet = _Screen.ActiveForm.Parent
*-- If user did not select the vendor
WITH loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder
  IF EMPTY(.oKeyHeaderClass.kbVendor.KeyTextBox.Value)
    =gfModalGen('TRM34020B34000','DIALOG',IIF(ALLTRIM(.cTransType)$'NN|PA','source location','vendor')+'|'+' ')
    .oKeyHeaderClass.kbVendor.KeyTextBox.SetFocus
    RETURN .F.
  ENDIF
  *-- If not multi ware house and there is no ware house selected or account code.
  llMulChk = .oKeyHeaderClass.chkMulShpTo.Value
ENDWITH 
WITH loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder.editregion1
  IF .Parent.oMainClass.lMultiWarehous AND !llMulChk AND;
    EMPTY(.Parent.oKeyHeaderClass.kbWarehouse.KeyTextBox.Value) AND;
    EMPTY(.Parent.oKeyHeaderClass.kbAccount.KeyTextBox.Value)
    *-You have to enter the select a ware house or account code.
    =gfModalGen('TRM34020B34000','DIALOG','ship to location or customer'+'|'+' ')
    IF .Parent.lLocationCustomer
      .Parent.oKeyHeaderClass.kbWarehouse.KeyTextBox.SetFocus
    ELSE
      .Parent.oKeyHeaderClass.kbAccount.KeyTextBox.SetFocus  
    ENDIF
    RETURN .F.
  ENDIF
ENDWITH   
lcAlias = SELECT()
PRIVATE laSize,laTot,;
        lcScrTtl,lcDet_Ttl,lnClrLen,lnClrPos,lcClrSpr,lcTempCur,lcTempNote,lcAlias,lnMrk ,;
        lcQkWin0,lcQkWin1,lcQkWin2,lcQkWin3,lcQkWin4,lcOldValue,lcSepart,lcItemPct,laExtSz,lcOrd,llEdit,;
        llChang,laStyClQty,llDifPrice,llShw1,laOldVal,lcBrowFlds



DO FORM  (oAriaApplication.ScreenHome+'\PO\PoQKORD.scx') WITH _Screen.ActiveForm.Parent
SELECT (lcAlias)
*:**************************************************************************
*:* Name        : lfvPQStyle
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Validate Style in Quick PO Entry Screen
*:***************************************************************************
FUNCTION lfvPQStyle
PARAMETERS loBranchFormSet,loParentForm

loBranchFormSet.lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
IF loParentForm.DataSessionId <> SET("Datasession") 
  SET DATASESSION TO loParentForm.DataSessionId
ENDIF   


lcStyleVal = loBranchFormSet.AriaForm1.kbstymaj.KeyTextBox.Value
PRIVATE lcAlias,lcGetSty,lcStat,lnIncrmnt,lnClrs,lnCount,lnJ,lcSeekSty,laSum,laSavStat,lcStyle
*--- Select
lcAlias = ALIAS()
loBranchFormSet.lnMajorLen = LEN(gfItemMask("PM", '', '0001'))

lcStyleVal = PADR(ALLTRIM(lcStyleVal),loBranchFormSet.lnMajorLen)
IF (!EMPTY(lcStyleVal) AND ('?' $ lcStyleVal  .OR. !SEEK(lcStyleVal ,'STYLE'))) OR ;
   loBranchFormSet.AriaForm1.kbstymaj.selectedfrombrowse 
  SELECT STYLE  
  lcGetSty = PADR(gfStyBrw("M" , lcStyleVal  , "" , .F.),loBranchFormSet.lnMajorLen)
  loBranchFormSet.AriaForm1.kbstymaj.selectedfrombrowse  = .F.
  IF EMPTY(lcGetSty)
    loBranchFormSet.AriaForm1.kbstymaj.KeyTextBox.Value = ''
    RETURN .F.
  ELSE
    loBranchFormSet.AriaForm1.kbstymaj.KeyTextBox.Value = lcGetSty
  ENDIF
ENDIF  

STORE ''  TO lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
STORE 1 TO lnCurrUnt1, lnCurrUnt2
llConCst     = .F.
m.cCstSht_Id = " "
lcTmpFile   = loParentForm.ariaForm1.mainworkorder.cpoline
lcMastPosHd = loParentForm.ariaForm1.mainworkorder.cposhdr
m.Style   = ALLTRIM(loBranchFormSet.AriaForm1.kbstymaj.KeyTextBox.Value)
lcStyleVal = m.Style   


WITH loParentForm.ariaform1.pgfPOStyle.page2.cntDetailFolder
  STORE 0 TO .nFcost1, .nFcost2, .nFcost3, .nFcost4, .nFcost5, .nFcost6, .nFcost7 
  *-- Get the selected price currency and rate in the header folder
  lcPriceCurr   = loParentForm.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cntMultiCurrency.KbPriceCurrency.KeyTextBox.Value
  lnPricRate    = loParentForm.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cntMultiCurrency.txtPriceRate.Value
  *-- Get the selected duty currency in the header folder
  lcDutyCurr    = loParentForm.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cntMultiCurrency.KbDutyCurrency.KeyTextBox.Value
  lnDutyRate    = loParentForm.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cntMultiCurrency.txtDutyRate.Value
	
  ldEnteredDate = loParentForm.AriaForm1.PgfPOStyle.page1.cntheaderFolder.DtPickerEntered.Value
  *-- Get the complete date in the header folder  
  ldCompDate    = loParentForm.AriaForm1.PgfPOStyle.page1.cntheaderFolder.DtPickerComplete.Value
  lcCstShtId = ''
  m.complete = ldCompDate  
  *-- Get the style cost sheet type. "I" for Style PO (PP), "M" for Dye Order (PD)
  lcCstShtType = IIF(.cTransType = "PD","M","I")
  
  *-- The ware house in the header key section
  m.cWareCode = loParentForm.AriaForm1.cntPoHeader.kbWareHouse.KeyTextBox.Value
  *-- To check if there is any reason of rejecting the style
  llAbort =.F.

  *-- If system setup to point of sale don't allow to add location.
  llPosSetN = .ctranstype = "NN" AND loParentForm.ariaForm1.mainworkorder.cSystemType = "P"
 
  *-- Style major length
  lnStyleLen = loBranchFormSet.lnMajorLen
  *-- Get the style major
  lcStyMaj = SUBSTR(lcStyleVal,1,lnStyleLen)
  *loParentForm.ariaForm1.mainworkorder.cStyMajor = lcStyMaj
  IF !EMPTY(lcStyleVal)
    SELECT STYLE
   	SET ORDER TO Style  
   	SEEK lcStyleVal
    loBranchFormSet.Ariaform1.txtStydesc.Value = 	Style.desc1
    DO WHILE .T.
    *B608962 AHS quick order entry screen accepts cancelled styles [start]
*!*	      *-- If style status is cancelled.
*!*	      IF STYLE.Status='X'
*!*	        *-This is a canceled style. Not allowed to enter here, Cannot proceed!
*!*	        =gfModalGen('TRM34040B34000','DIALOG')        
*!*	        llAbort=.T.
*!*	        EXIT
*!*	      ENDIF
    *B608962 AHS quick order entry screen accepts cancelled styles [end]
      *-- If there is a conflict in the style division
      IF Style.cDivision <> loParentForm.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cboDivision.Value
        *-Conflict ! styles restricted to division XXXX, Cannot proceed!
        =gfModalGen('TRM34041B34000','DIALOG',ALLTRIM(loParentForm.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cboDivision.Value))
        llAbort=.T.
        EXIT
      ENDIF

      *-- If there is a conflict in the style puchasing group
      IF !EMPTY(Style.cPurCode) AND Style.cPurCode <> ALLTRIM(loParentForm.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cboPurGroup.Value)
        *-Conflict ! styles restricted to purchase XXXX, Cannot proceed!
        =gfModalGen('TRM34110B34000','DIALOG',ALLTRIM(loParentForm.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cboPurGroup.DisplayValue))
        llAbort=.T.
        EXIT
      ENDIF

      .omainclass.mstycstsht (lcCstShtType, PADR(lcStyMaj,19), .T.)
      IF .cTransType $ 'PP|PD|NN|RP' 
        IF !(loParentForm.AriaForm1.cntPoHeader.cboStatus.Value $ 'HB')
          IF SEEK(lcStyMaj,.omainclass.cBomHdr)
            SELECT (.omainclass.cBomHdr)
   	        LOCATE REST WHILE cItmMajor+cCstShtTyp+cCstSht_Id = PADR(lcStyMaj,19)+lcCstShtType;
	                    FOR lDefCstSht AND !EMPTY(cCstSht_Id)
            IF !FOUND()
             *-No cost lines found in the cost sheet, Cannot proceed!
              *=gfModalGen('TRM34037B34000','DIALOG')        
              llAbort=.T.
              SELECT (.omainclass.cPosHdr)
              EXIT
            ELSE
              m.cCstSht_Id = cCstSht_Id
            ENDIF
          ELSE
            *-No cost lines found in the cost sheet, Cannot proceed!
            *=gfModalGen('TRM34037B34000','DIALOG')        
            llAbort=.T.
            EXIT
          ENDIF
        ELSE 
          IF !SEEK(PADR(lcStyMaj,19)+lcCstShtType,.omainclass.cBomHdr) OR EMPTY(cCstSht_Id)
            *-- If the style has no cost sheet, don't continue in case of dye order
            *-- Warrning ! This style has no cost sheet.
            IF .cTransType = 'PD' 
              *-No cost lines found in the cost sheet, Cannot proceed!
              *=gfModalGen('TRM34037B00000','DIALOG')        
              llAbort=.T.
              EXIT
            ELSE
              *-- Give a notification message the the style does not have cost sheet
             * =gfModalGen('INM34038B34000','DIALOG')
            ENDIF
          ELSE
            *-- If there is a cost sheet then get the default code.
            SELECT (.omainclass.cBomHdr)
            LOCATE REST WHILE cItmMajor+cCstShtTyp+cCstSht_Id = PADR(lcStyMaj,19)+lcCstShtType;
                         FOR lDefCstSht
            IF FOUND()
              m.cCstSht_Id = cCstSht_Id
            ENDIF
          ENDIF
          SELECT (.oMainClass.cPoLine)
        ENDIF       
      ENDIF
      lcCstShtId = m.cCstSht_Id
      lcTmpBom = .omainclass.cBom
      EXIT
    ENDDO
  ENDIF
  IF !EMPTY(lcStyleVal) AND !llAbort
    m.llAddNew = .F.
    IF SEEK(lcStyleVal,.oMainClass.cPoLine)
    *--This Style has entred on this order
    *--Button 02011 : \!\<OK;\?\<Cancel
    lnResp = gfModalGen('QRM00000B34015',.F.,.F.,.F.,'This style has already been entred on this order '+;
                                                    +'- do you wish to Create a New Line?')
    IF lnResp = 1 
      m.llAddNew = .T.
    ELSE
      IF lnResp = 2
        m.llAddNew = .F.
      ELSE   
        =lfvPClear(loBranchFormSet,.F.) 
        RETURN
      ENDIF 
    ENDIF
  ENDIF  
  loBranchFormSet.AriaForm1.spnDisc.Enabled = .T.
  loBranchFormSet.AriaForm1.DtpCompDate.Enabled = .T.
  loBranchFormSet.AriaForm1.DtpCompDate.Value = loParentForm.AriaForm1.PgfPOStyle.page1.cntheaderFolder.dtpickercomplete.Value
  STORE 0   TO m.Gros_Price, m.Price, m.Disc_Pcnt
  =SEEK(allt(m.Style),'STYLE')
  loBranchFormSet.AriaForm1.cmdClear.Enabled = !EMPTY(m.Style)
  STORE 0   TO m.Gros_Price, m.Price, m.Disc_Pcnt
  STORE .F. TO m.lContract
  STORE '' TO m.Store 

  =SEEK(allt(m.Style),'STYLE')
  laSize = ''
  loBranchFormSet.laSize = ''
  loBranchFormSet.laExtSz = ''
  
  =SEEK(allt(m.Style),'STYLE')
  loBranchFormSet.lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
  m.SCALE = STYLE.SCALE
  SELECT Scale,CNT FROM SCALE WHERE Type+Scale='S'+SUBSTR(m.SCALE,1,loBranchFormSet.lnScaleLen) ORDER BY 1 INTO ARRAY laExtSz
  SELECT SCALE
  lcStyScl = SUBSTR(STYLE.SCALE,1,loBranchFormSet.lnScaleLen)
  =SEEK('S'+lcStyScl ,'SCALE')
  IF !EMPTY(SCALE.CDIM1)
    loBranchFormSet.llMultiDime = .T.
  ENDIF
  lnMaxSz = 0
  IF loBranchFormSet.llMultiDime 
    lnMaxSz = 0
    lnLastCnt = 0
    SELECT Scale 
    =SEEK('S'+lcStyScl ,'SCALE')
    DO WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl .AND. !EOF('SCALE')
     lnRecord = RECNO('Scale')
     lcFit = SCALE.CDIM1
     lnMaxSz = 0
     SCAN REST WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl FOR ;
                   CDIM1 = lcFit
       FOR lnI = 1 TO SCALE.CNT
         lnMaxSz = lnMaxSz  + 1
       ENDFOR
     ENDSCAN 
     IF lnMaxSz > lnLastCnt
       lnLastCnt = lnMaxSz 
     ENDIF
     lnMaxSz = IIF(lnMaxSz >= lnLastCnt,lnMaxSz,  lnLastCnt)                
     IF BETWEEN(lnRecord+1,1,RECCOUNT('Scale'))
       GO lnRecord+1 IN Scale
     ENDIF 
   ENDDO 
ENDIF 
loBranchFormSet.lnMaxSz = lnMaxSz
lnIncrmnt = 0
FOR lnCount = 1 TO ALEN(laExtSz,1)
  lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
ENDFOR  
IF lnIncrmnt > 16
  *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'This Style has scale with More than 16 Sizes.')
ENDIF
  
lnIncrmnt = 0
FOR lnCount = 1 TO ALEN(laExtSz,1)
  =SEEK('S'+laExtSz[lnCount,1],'SCALE')
  FOR lnJ = 1 TO laExtSz[lnCount,2]
    lcZ = STR(lnJ,1)
    DIMENSION laSize[lnIncrmnt+lnJ]
    laSize[lnIncrmnt+lnJ] = ALLTRIM(SCALE.Sz&lcZ)
  ENDFOR
  lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
ENDFOR
  
*--Get Colors
STORE 0  TO lnClrPos,lnClrLen
PRIVATE lnClrPos,lnClrLen
STORE 0 TO lnSizePos,lnSizeLen
lfGetClrD()
loBranchFormSet.lnClrLen = lnClrLen
loBranchFormSet.lnClrPos = lnClrPos

lfPOCrtTempFiles(loBranchFormSet)  
  
*--Create color array
SELECT DISTINCT SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) FROM STYLE ;
   WHERE STYLE = PADR(m.Style,loBranchFormSet.lnMajorLen) ;
   INTO ARRAY laClr  
  
  
STORE '' TO lcStySp,lcSclSp
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnJ = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnJ,1] = 'F'
    lcStySp = laItemSeg[lnJ,6]
    loBranchFormSet.lcStySp = lcStySp
    LOOP 
  ENDIF 
  IF laItemSeg[lnJ,1] = 'C'
    lcSclSp = laItemSeg[lnJ,6]
    loBranchFormSet.lcSclSp = lcSclSp 
    LOOP 
  ENDIF 
ENDFOR  
STORE 0   TO m.Gros_Price, m.Price, m.Disc_Pcnt
STORE .F. TO m.lContract
STORE '' TO m.Store 

PRIVATE lnMaxInd
lnMaxInd = IIF(ALEN(laClr,1)>=10,10,ALEN(laClr,1))
FOR lnClrs = 1 TO lnMaxInd
  llAddNewVal = m.llAddNew
  SELECT (loBranchFormSet.lcTempCur)
  SCATTER MEMVAR BLANK
  m.llAddNew =llAddNewVal
  m.cCstSht_Id =lcCstShtId
  M.STYMAJOR = SUBSTR(lcStyleVal,1,lnStyleLen)
  loParentForm.AriaForm1.PgfPOStyle.page2.cntDetailFolder.cSTYmAJOR = M.STYMAJOR
  M.COLOR = laClr[lnClrs]
  M.COLORDSC = PADR(gfCodDes(PADR(laClr[lnClrs],lnClrLen) , 'COLOR'),20)
  lnIncrmnt = 0
  DIMENSION laSzQty[ALEN(laSize,1)]
  STORE 0 TO laSzQty
  lnCounSz = 1
  lcFullStyClr = PADR(M.STYMAJOR ,loBranchFormSet.lnMajorLen) +;
               loBranchFormSet.lcstysp +PADR( M.COLOR,loBranchFormSet.lnClrLen )
  =SEEK(allt(lcFullStyClr),'STYLE')
  lcStyScl = SUBSTR(STYLE.SCALE,1,loBranchFormSet.lnScaleLen)
  DIMENSION laFits[1]
  laFits = ''
  SELECT SCALE
  =SEEK('S'+lcStyScl)
  lcScaleCurr = ''
  SCAN REST  WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl FOR !EOF('SCALE') 
    lnRecord = RECNO('Scale')
    lcFit = SCALE.CDIM1
    IF ASCAN(laFits ,lcFit,1) > 0
      LOOP 
    ENDIF 
    IF !EMPTY(SCALE.CDIM1)
      m.FitDesc = SCALE.CDIM1
    ELSE
      m.FitDesc = SCALE.CSCL_DESC
    ENDIF  
    lnCounter = 1 
    FOR lnCont = 1 TO IIF(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz,ALEN(laSize,1))
      lcCont  = ALLTRIM(STR(lnCont))
      STORE '' TO m.SZ&lcCont
    ENDFOR 
    IF EMPTY(laFits[1])
      laFits[1] = lcFit 
    ELSE
      DIMENSION laFits[ALEN(laFits,1)+1] 
      laFits[ALEN(laFits,1)] =lcFit 
    ENDIF    
    lnCounting = 1
    SCAN REST WHILE TYPE+SCALE+PREPAK = 'S' + lcStyScl FOR  CDIM1 = lcFit 
      lcCounting = ALLTRIM(STR(lnCounting))
      lnCounting = lnCounting  + 1
      IF ALLTRIM(lcScaleCurr) <> ALLTRIM(Scale.Scale)
        lcSeekSty = PADR(PADR(lcStyleVal,lnStyleLen)+lcStySp +PADR(M.COLOR,lnClrLen)+IIF(loBranchFormSet.llExtended,ALLTRIM(lcSclSp) +Scale.Scale,''),19)
        STORE 0   TO m.Gros_Price, m.Price, m.Disc_Pcnt
        STORE .F. TO m.lContract
        STORE '' TO m.Store 
        =SEEK(ALLTRIM(lcSeekSty),'Style')
          
        IF !loParentForm.AriaForm1.cntPoHeader.chkMulShpTo.Value
	      *--If ship to customer the location will be the drop down location.
          m.Account   = loParentForm.AriaForm1.cntPoHeader.kbAccount.keytextbox.Value
  	      m.Store     = loParentForm.AriaForm1.cntPoHeader.kbStore.keytextbox.Value
    	  m.cWareCode = IIF(loParentForm.AriaForm1.cntPoHeader.cboShpLoc.Value = "C",;
                        .oMainClass.lcDropLoc, m.cWareCode)
	      IF !SEEK(Style.Style+PADR(m.cWareCode,6)+SPACE(10),'STYDYE')
        	  *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
    	    IF gfModalGen('QRM34048B34004','DIALOG',Style.Style+'|'+m.cWareCode) = 1
	          DO gpAdStyWar WITH Style.Style,SPACE(10),m.cWareCode
    	    ELSE
        	  LOOP 
	        ENDIF
    	  ENDIF
        ELSE
          m.cwarecode = STYLE.CDefWare
          m.Account   = loParentForm.AriaForm1.cntPoHeader.kbAccount.KeyTextBox.Value
          m.Store     = loParentForm.AriaForm1.cntPoHeader.kbStore.KeyTextBox.Value        
        ENDIF
        lnRecn = RECNO('Scale')
	    loParentForm.AriaForm1.PgfPOStyle.page2.cntDetailFolder.editregion1.kbCostSheet.keytextbox.Value = lcCstShtId
        lcOldWared = m.cWareCode 
        llAddRec = .F.
        IF SEEK(Style.stYle,.oMainClass.cPoLine)
          SELECT (.oMainClass.cPoLine)
          SCATTER MEMO MEMVAR 
          m.LineNo = 0
          APPEND BLANK 
          GATHER MEMO MEMVAR 
          llAddRec =.T.
        ENDIF 
        loParentForm.AriaForm1.PgfPOStyle.page2.cntDetailFolder.editregion1.kbCostSheet.sharedvalidation ()
        IF llAddRec 
          SELECT (.oMainClass.cPoLine)
          DELETE 
        ENDIF 
      
      
        m.cCstSht_Id =lcCstShtId
        m.cWareCode = lcOldWared
        m.complete = ldCompDate 
  	    lcBomFile = loParentForm.ariaform1.Mainworkorder.cbom
   	    lcBomHdrFile = loParentForm.ariaform1.Mainworkorder.cbomhdr
  	    =SEEK(ALLTRIM(lcSeekSty),'Style')
  	    =SEEK('S'+Style.Scale,'Scale')
	    lcScale = Style.scale
	    lcStyle = Style.stYle
   	    m.lbasonsiz = .F.

      
	    IF &lcBomHdrFile..lbasonsiz
	      m.lbasonsiz = .T.
	      SELECT (lcBomFile)
	      SCAN FOR LIKE(STRTRAN(EVALUATE(lcBomFile+'.cItmMask'),'*','?') , lcStyle) .AND. ;
    		     (lcScale $ EVALUATE(lcBomFile+'.mSizes')) AND CCATGTYP = 'P'
    		  
      	    lnLineNo   = ATCLINE(lcScale , ALLTRIM(EVALUATE(lcBomFile+'.mSizes')))
			    *-- Get the string that hold the sizes.
  		    lcGetSize  = IIF(lnLineNo > 0 , SUBSTR(ALLTRIM(MLINE(EVALUATE(lcBomFile+'.mSizes'),lnLineNo)),5) , "")
	  	    lcSzes = STRTRAN(lcGetSize  ,",")    
            lnUntCst = 0 
          
            IF !EMPTY(EVALUATE(lcBomFile+'.CCURRCODE')) AND EVALUATE(lcBomFile+'.CCURRCODE')  <> lcPriceCurr 
              IF lcPriceCurr <> oAriaApplication.BaseCurrency
                * IF PO Currecny is not same as company base currecny, we will convert to base currency and then convert from base 
                * currecny to PO currecny
                IF EVALUATE(lcBomFile+'.CCURRCODE') <> oAriaApplication.BaseCurrency
                  LOCAL lcUnitSign,lcunitsign ,lcRateSign
                  lcUnitSign = "/"
                  lnCurrUnit = EVALUATE(lcBomFile+'.nCurrUnit')
                  lcRateSign = gfGetExSin(@lcUnitSign , EVALUATE(lcBomFile+'.CCURRCODE'))
                  lcunitsign = lcUnitSign
                   lcUntCstBase = EVALUATE(lcBomFile+'.TotCost' + lcRateSign+;
                                             STR(EVALUATE(lcBomFile+'.nEXRate'),9,4)+lcUnitSign+ STR(lnCurrUnit))

                ELSE
                  lcUntCstBase = EVALUATE(lcBomFile+'.TotCost')
                ENDIF                                
             
                *Convert to Po currecny
                lcUnitSign = "/"
                lnCurrUnit = 1
                lcRateSign = gfGetExSin(@lcUnitSign , EVALUATE(lcBomFile+'.CCURRCODE'))
                lcRateSign = IIF(lcRateSign = '/','*','/')
                lcUnitSign = IIF(lcUnitSign = '/','*','/')
                lnUntCst = EVALUATE(STR(lcUntCstBase,13,3)  + lcRateSign+;
                                              STR(lnPricRate,9,4)+lcUnitSign+ STR(lnCurrUnit))                             
             ELSE 
               LOCAL lcUnitSign,lcunitsign ,lcRateSign
               lcUnitSign = "/"
               lnCurrUnit = EVALUATE(lcBomFile+'.nCurrUnit')
               IF EMPTY(EVALUATE(lcBomFile+'.CCURRCODE'))
                 lnCurrUnit = 1
               ENDIF 
               lcRateSign = gfGetExSin(@lcUnitSign , EVALUATE(lcBomFile+'.CCURRCODE'))
               lcunitsign = lcUnitSign
               lnUntCst = EVALUATE(lcBomFile+'.TotCost' + lcRateSign+;
                                            STR(EVALUATE(lcBomFile+'.nEXRate'),9,4)+lcUnitSign+ STR(lnCurrUnit))
            ENDIF   
          ELSE
            lnUntCst =EVALUATE(lcBomFile+'.TotCost')  
          ENDIF 
          *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[Start]
  		  *IF !SEEK(lcStyle+lcSzes,loBranchFormSet.lcGrpCur)    
          lcSvSz = ''
          lcNewSz = lcSzes 
  		  IF !SEEK(lcStyle+PADR(lcSzes,8),loBranchFormSet.lcGrpCur)    
  		    DIMENSION laSzArr[LEN(lcNewSz)]
  		    STORE '' TO laSzArr
  		    FOR L=1 TO LEN(lcNewSz)
    		  laSzArr[L] =  VAL(SUBSTR(lcNewSz,L,1))
    		ENDFOR    
    		lnVal = laSzArr[1]
    		FOR S=1 TO ALEN(laSzArr,1)
    		  *Find The Samllest Value
    		  FOR Z = S+1 TO ALEN(laSzArr,1)
    		    IF laSzArr[S] > laSzArr[Z] 
    		      lnVal = laSzArr[S]
    		      laSzArr[S] = laSzArr[Z]
    		      laSzArr[Z] = lnVal 
    		    ENDIF 
    		  ENDFOR 
    		ENDFOR  
  		    FOR L=1 TO ALEN(laSzArr,1)
    		  lcSvSz = lcSvSz +  STR(laSzArr[L],1)
    		ENDFOR    
  		    lcSzes = lcSvSz 
  		    *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[End]
   		    INSERT INTO (loBranchFormSet.lcGrpCur) VALUES (lcStyle,lcSzes,lnUntCst )
   		  ELSE
   		    REPLACE GROS_PRICE WITH GROS_PRICE +lnUntCst  IN (loBranchFormSet.lcGrpCur) 
   		  ENDIF 
        ENDSCAN 
      ELSE
        SELECT (lcBomFile)
        lnUntCst = 0
        SCAN FOR LIKE(STRTRAN(EVALUATE(lcBomFile+'.cItmMask'),'*','?') , lcStyle) AND CCATGTYP = 'P'
          IF !EMPTY(EVALUATE(lcBomFile+'.CCURRCODE')) AND EVALUATE(lcBomFile+'.CCURRCODE') <> lcPriceCurr
            IF lcPriceCurr = oAriaApplication.BaseCurrency
              LOCAL lcUnitSign,lcunitsign ,lcRateSign
              lcUnitSign = "/"
              lnCurrUnit = EVALUATE(lcBomFile+'.nCurrUnit')
              lcRateSign = gfGetExSin(@lcUnitSign , EVALUATE(lcBomFile+'.CCURRCODE'))
              lcunitsign = lcUnitSign
              lnUntCst = lnUntCst +  EVALUATE(lcBomFile+'.TotCost' + lcRateSign+;
                        STR(EVALUATE(lcBomFile+'.nEXRate'),9,4)+lcUnitSign+ STR(lnCurrUnit))
            ELSE
              * IF PO Currecny is not same as company base currecny, we will convert to base currency and then convert from base 
              * currecny to PO currecny
              LOCAL lcUnitSign,lcunitsign ,lcRateSign
              lcUnitSign = "/"
              lnCurrUnit = EVALUATE(lcBomFile+'.nCurrUnit')
              lcRateSign = gfGetExSin(@lcUnitSign , EVALUATE(lcBomFile+'.CCURRCODE'))
              lcunitsign = lcUnitSign
              lcUntCstBase = EVALUATE(lcBomFile+'.TotCost' + lcRateSign+;
                                            STR(EVALUATE(lcBomFile+'.nEXRate'),9,4)+lcUnitSign+ STR(lnCurrUnit))
             
              *Convert to Po currecny
              lcUnitSign = "/"
              lnCurrUnit = 1
              lcRateSign = gfGetExSin(@lcUnitSign , EVALUATE(lcBomFile+'.CCURRCODE'))
              lcRateSign = IIF(lcRateSign = '/','*','/')
              lcUnitSign = IIF(lcUnitSign = '/','*','/')
              lnUntCst = lnUntCst +  EVALUATE(STR(lcUntCstBase,13,3)+ lcRateSign+;
                                            STR(lnPricRate,9,4)+lcUnitSign+ STR(lnCurrUnit)) 
            ENDIF             
          ELSE
            lnUntCst = lnUntCst +  EVALUATE(lcBomFile+'.TotCost')  
          ENDIF 
        ENDSCAN 
          m.Gros_Price = lnUntCst
		  ENDIF 
          IF BETWEEN(lnRecn,1,RECCOUNT('Scale'))
            GO RECORD lnRecn IN 'Scale'
          ENDIF  
          
          IF ASCAN(laExtSz,Scale.Scale,1) > 0
            lnCounting  = ASUBSCRIPT(laExtSz,ASCAN(laExtSz,Scale.Scale,1),1)
            lcCounting  = ALLTRIM(STR(lnCounting))
          ENDIF 
          m.Pric&lcCounting  =  m.Gros_Price
          m.NtPri&lcCounting  =  m.Gros_Price

        ENDIF 
        lcScaleCurr = Scale.Scale
        m.FitDesc = lcFit        
        
        STORE 0 TO m.Qty1, m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8
        =gfSeek('S'+Style.Scale,'Scale')
        FOR lnI = 1 TO SCALE.CNT


          lcCounter = ALLTRIM(STR(lnCounter))
          lcI = STR(lnI,1)
          
          m.SZ&lcCounter = Scale.SZ&lcI 
          *Check if Style entered before   
           m.cVenSty = ''

		 *! B609194,1 MMT 03/31/2010 Fix bug of vendor Style is not displayed in PO Quick entry[Start]
         m.cVenSty = Style.cVenSty
         *! B609194,1 MMT 03/31/2010 Fix bug of vendor Style is not displayed in PO Quick entry[End]
           
          IF !m.llAddNew AND SEEK(Style.stYle,.oMainClass.cPoLine)
            SELECT (.oMainClass.cPoLine)
            m.cRefer= EVALUATE(.oMainClass.cPoLine+'.Reference')
            m.cVenSty = EVALUATE(.oMainClass.cPoLine+'.cVenSty')
            m.Complete = EVALUATE(.oMainClass.cPoLine+'.Complete')
            *-- if style is entred in more than one line then each line is a separate price group
            lcPoLine = .oMainClass.cPoLine
            SCAN REST WHILE STYLE+STR(LINENO,6) = Style.stYle
               M.Qty&lcCounter = M.Qty&lcCounter +  Qty&lcI.
               m.nTotal = m.nTotal + &lcPoLine..Qty&lcI.
            ENDSCAN
          ENDIF       

          
          
          SELECT(loBranchFormSet.lcTempCur)                   
          IF !SEEK(PADR(M.STYMAJOR,19)+M.COLOR+padR(m.FitDesc,10),loBranchFormSet.lcTempCur) 
            APPEND BLANK
            SELECT(loBranchFormSet.lcTempCur)  
          ELSE
            sELECT(loBranchFormSet.lcTempCur) 
            IF lnCounter > 8
              FOR lnx = 1 TO 8
                lcx= ALLTRIM(STR(lnx))
                m.Qty&lcx = Qty&lcx
              ENDFOR   
            ENDIF   
          ENDIF 
          IF !emptY(cCstSht_Id) AND EMPTY(m.cCstSht_Id)
            m.cCstSht_Id= cCstSht_Id
          ENDIF
          IF !emptY(cVenSty) AND EMPTY(m.cVenSty )
            m.cVenSty = cVenSty 
          ENDIF
           
          GATHER MEMO MEMVAR
          lnCounter = lnCounter + 1
        ENDFOR   
      ENDSCAN   
      

      
      IF BETWEEN(lnRecord,1,RECCOUNT('Scale'))
        GO lnRecord IN Scale
      ENDIF             
    ENDSCAN 
  ENDFOR  

 ELSE
   loBranchFormSet.AriaForm1.kbstymaj.KeyTextBox.Value = ''
   RETURN .F.
 ENDIF    
 SELECT (.oMainClass.cPoLine)
ENDWITH 

SELECT (loBranchFormSet.lcTempCur)
GO TOP 
  SCATTER MEMO MEMVAR 
  APPEND BLANK 
  GATHER MEMO MEMVAR 
  REPLACE StyMajor WITH CHR(255),;
       Color    WITH '',;
       COLORDSC WITH 'Totals' ,;
       FitDesc  WITH ''
       
  FOR lnCount = 1 TO ALEN(laExtSz,1)
 lcI = ALLTRIM(STR(lnCount))
 REPLACE Pric&lcI  WITH 0,;
         NtPri&lcI WITH 0
  ENDFOR 
      
  FOR lnC = 1 TO IIF(loBranchFormSet.llMultiDime,lnMaxSz,ALEN(laSize,1) )    
 lcC = ALLTRIM(STR(lnC))
 SELECT (loBranchFormSet.lcTempCur)
 SUM (Qty&lcC) TO lnTotal FOR StyMajor <> CHR(255)
 GO BOTTOM 
 REPLACE Qty&lcC WITH lnTotal
  ENDFOR 
  SELECT (loBranchFormSet.lcTempCur)
  LOCATE 

  WITH loBranchFormSet.AriaForm1
 .kbstymaj.Enabled = .F. 
 .spnDisc.Enabled = .T. 
  ENDWITH 


  ACOPY(laSize,loBranchFormSet.laSize)
  ACOPY(laExtSz,loBranchFormSet.laExtSz)


SELECT(loBranchFormSet.lcTempCur)
LOCATE 
lfPOAddCntrSrc(loBranchFormSet)
lfPOCalcTot(loBranchFormSet)
loBranchFormSet.AriaForm1.grdLines.refresh    
loBranchFormSet.AriaForm1.grdLines.AFterRowColChange
RETURN 
*:**************************************************************************
*:* Name        : lfInitPQkOrd
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : init of Quick PO Entry Screen
*:***************************************************************************
FUNCTION lfInitPQkOrd
PARAMETERS loBranchFormSet

WITH loBranchFormSet.AriaForm1
  .kbstymaj.Enabled = .T. 
  .spnDisc.Enabled = .F. 
  .txtGrsPrc.Enabled= .F. 
  .txtNetPri.Enabled = .F. 
  .dtpCompDate.Enabled= .F. 
  .cmdSave.Enabled= .F. 
  .grdLines.RecordSource = ''
ENDWITH 
loBranchFormSet.lctempcur = gfTempName()
loBranchFormSet.lcGrpCur= gfTempName()
loBranchFormSet.llincwip = gfGetMemVar('M_SOWIPQK')
loBranchFormSet.llExtended = gfGetMemVar('M_USEEXSSC')

STORE '' TO lcStySp,lcSclSp
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnJ = 1 TO ALEN(laItemSeg,1)
 IF laItemSeg[lnJ,1] = 'F'
    lcStySp = laItemSeg[lnJ,6]
    loBranchFormSet.lcStySp = lcStySp
    LOOP 
 ENDIF 
 IF laItemSeg[lnJ,1] = 'C'
    lcSclSp = laItemSeg[lnJ,6]
    loBranchFormSet.lcSclSp = lcSclSp 
    LOOP 
 ENDIF 
ENDFOR  

STORE 0  TO lnClrPos,lnClrLen
STORE 0 TO lnSizePos,lnSizeLen
=lfGetClrD()
STORE lnClrLen TO loBranchFormSet.lnClrLen 
STORE lnClrPos TO loBranchFormSet.lnClrPos
*:**************************************************************************
*:* Name        : lfvPClear
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Clear Button of Quick PO Entry Screen
*:***************************************************************************
FUNCTION lfvPClear
PARAMETERS loBranchFormSet,llFromScreen
IF llFromScreen
  loBranchFormSet.AriaForm1.grdLines.RecordSource = ''
ENDIF 
loBranchFormSet.llMultiDime = .F.
IF USED(loBranchFormSet.lctempcur)
  SELECT(loBranchFormSet.lctempcur)
  ZAP
ENDIF 
WITH loBranchFormSet.AriaForm1
  .kbstymaj.Enabled = .T. 
  .kbstymaj.KeyTextBox.Value = ''
  .spnDisc.Enabled = .F. 
  .spnDisc.VAlue = 0
  .txtGrsPrc.Enabled= .F. 
  .txtGrsPrc.value = 0
  .txtNetPri.Enabled = .F. 
  .txtNetPri.value = 0
  .dtpCompDate.Enabled= .F. 
  .cmdSave.Enabled= .F. 
  .grdLines.RecordSource = ''
  .txtStydesc.Value = ''
  .txtVenSty.Value =''
  .txtRefr.Value = ''
ENDWITH 
DIMENSION loBranchFormSet.laSize[1],loBranchFormSet.laExtSz[1]
STORE '' TO loBranchFormSet.laSize,loBranchFormSet.laExtSz
*:**************************************************************************
*:* Name        : lfPOAddCntrSrc
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Add Control Source of Quick PO Entry Screen grid
*:***************************************************************************
FUNCTION lfPOAddCntrSrc
PARAMETERS loBranchFormSet
WITH loBranchFormSet.AriaForm1.grdLines
  .ColumnCount =IIF(loBranchFormSet.llMultiDime,lnMaxSz,ALEN(laSize,1))+3
  .RecordSource = loBranchFormSet.lctempcur 
  .Column1.Readonly = .T.
  .Column1.Header1.Caption ='Colour'
  .Column1.ControlSource  = loBranchFormSet.lctempcur +'.COLORDSC'
  .Column2.Readonly = .T.
  .Column2.Header1.Caption ='Fit'
  .Column2.ControlSource  = loBranchFormSet.lctempcur +'.FitDesc'
  
  lnCount = 2
  FOR lnT = 1 TO IIF(loBranchFormSet.llMultiDime,lnMaxSz,ALEN(laSize,1))
    lnCount = lnCount + 1
    lcT = ALLTRIM(STR(lnT))
    lcCount= ALLTRIM(STR(lnCount))
    .Column&lcCount..ControlSource = loBranchFormSet.lctempcur +'.QTY'+lcT
    .Column&lcCount..Header1.Caption = EVALUATE(loBranchFormSet.lctempcur +'.SZ'+lcT)
    .Column&lcCount..Enabled = .T.
    .Column&lcCount..Format = '99999'

    *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[Start]
    .Column&lcCount..Width = 50
    *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[End]

    .Column&lcCount..InputMAsk = '99999'
    .Column&lcCount..Readonly = .F.
    BINDEVENT(.Column&lcCount..Text1,"GotFocus",loBranchFormSet,'lfvgtstyszqty')
    BINDEVENT(.Column&lcCount..Text1,"LostFocus",loBranchFormSet,'lfvstyszqty')
  ENDFOR 
  lnCount = lnCount + 1
  lcCount= ALLTRIM(STR(lnCount))
  .Column&lcCount..ControlSource = loBranchFormSet.lctempcur +'.Ntotal'
  .Column&lcCount..Header1.Caption = 'Total'
  .Column&lcCount..Readonly = .T.
ENDWITH 
loBranchFormSet.AriaForm1.grdLines.REFRESH    
*:**************************************************************************
*:* Name        : lfPOAftRoColChng
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : After Row Column Change of Quick PO Entry Screen grid
*:***************************************************************************
FUNCTION lfPOAftRoColChng
PARAMETERS loBranchFormSet
lcTempCur = loBranchFormSet.lctempcur 

loBranchFormSet.ariaForm1.txtGrsPrc.Enabled = .F.
loBranchFormSet.AriaForm1.spnDisc.Enabled = .F.
loBranchFormSet.ariaForm1.txtRefr.Enabled = .T. 
loBranchFormSet.ariaForm1.txtVenSty.Enabled = .T. 
=SEEK(ALLTRIM(loBranchFormSet.ariaForm1.kbstymaj.KeyTextBox.Value),'Style')
lnCount = 2
FOR lnS = 1 TO IIF(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz,ALEN(loBranchFormSet.laSize,1))
  lnCount = lnCount + 1
  lcT = ALLTRIM(STR(lnS))
  lcCount= ALLTRIM(STR(lnCount))
  loBranchFormSet.AriaForm1.grdLines.Column&lcCount..Header1.Caption = EVALUATE(loBranchFormSet.lctempcur +'.SZ'+lcT)
ENDFOR 
IF &lcTempCur..StyMajor = CHR(255)
  WITH loBranchFormSet.AriaForm1
    .DtpCompDate.value = {}
    .txtVenSty.Value = ''
    .txtRefr.Value = ''
    .txtVenSty.Enabled = .F.
    .txtRefr.Enabled= .F.
    .DtpCompDate.Enabled= .F.
  ENDWITH  
ELSE
  WITH loBranchFormSet.AriaForm1
    .DtpCompDate.value = &lctempcur..complete 
    .txtVenSty.Value = &lcTempCur..cVenSty
    .txtRefr.Value = &lcTempCur..cRefer
    .txtVenSty.Enabled = .T.
    .txtRefr.Enabled= .T.
    .DtpCompDate.Enabled= .T.
  ENDWITH  
ENDIF   


IF loBranchFormSet.lccurrsz < 3 OR loBranchFormSet.lccurrsz > loBranchFormSet.ariaform1.grdLines.columnCount - 1 
  RETURN 
ENDIF 

lcSizorder = ''

lcFPos = ALLTRIM(STR(loBranchFormSet.lccurrsz))
lnCurrSz  = loBranchFormSet.ariaform1.grdLines.column&lcFPos..ControlSource
lcSclPos = lfGetPOScale(loBranchFormSet,loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption,&lcTempCur..fitdesc)
lnPosSc = 0
lnPosSc  = ASCAN(loBranchFormSet.laExtSz,lcSclPos) 
IF lnPosSc  > 0 
  lnPosSc = ASUBSCRIPT(loBranchFormSet.laExtSz,lnPosSc,1)
  lcPos = ALLTRIM(STR(lnPosSc))
  WITH loBranchFormSet.AriaForm1
    .txtGrsPrc.Value = &lctempcur..Pric&lcPos 
    .txtNetPri.Value = &lctempcur..NtPri&lcPos 
    .SpnDisc.VAlue  = &lctempcur..Discount
    .DtpCompDate.value = &lctempcur..complete 
    .txtVenSty.Value = &lcTempCur..cVenSty
	  .txtRefr.Value = &lcTempCur..cRefer
  ENDWITH  
ENDIF 

IF &lcTempCur..lbasonsiz
  lnAlias = SELECT()
  lcFullStyClrScl = PADR(&lcTempCur..STYMAJOR,loBranchFormSet.lnMajorLen) +;
                 loBranchFormSet.lcstysp +PADR(&lcTempCur..COLOR,loBranchFormSet.lnClrLen )+;
                 IIF(loBranchFormSet.llExtended,ALLTRIM(loBranchFormSet.lcsclsp)+lcSclPos,'')
                 
  lcGrpCur = loBranchFormSet.lcGrpCur 
  SELECT(lcGrpCur)
  =SEEK(lcFullStyClrScl ,lcGrpCur)
  lnPrice = 0
  SCAN REST WHILE STYLE+SIZES = lcFullStyClrScl FOR lcSizorder $ SIZES
    lnPrice = lnPrice + GROS_PRICE
  ENDSCAN 
  WITH loBranchFormSet.AriaForm1
    .txtGrsPrc.Value = lnPrice 
    .SpnDisc.VAlue = 0
    .SpnDisc.Enabled = .F.
    .txtNetPri.Value  =lnPrice 
  ENDWITH   
  SELECT (lnAlias)
ENDIF   
*:**************************************************************************
*:* Name        : lfvPOStySzqty
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Valid fn. for fields in grid(PO Quick Order Entyr)
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvBrFld()
*:***************************************************************************
FUNCTION lfvPOStySzqty
PARAMETERS loBranchFormSet,loParentForm
PRIVATE lcFPos
lcTempCur = loBranchFormSet.lctempcur 
lcFPos = ALLTRIM(STR(loBranchFormSet.lccurrsz))
lnCurrSz  = loBranchFormSet.ariaform1.grdLines.column&lcFPos..ControlSource
PRIVATE lnLineTot,lnCount,lnRecno,m.AddSty,lnIncrm

SELECT (lcTempCur)
IF STYMAJOR = CHR(255) 
  REPLACE &lnCurrSz WITH loBranchFormSet.lnOldVAlue   IN (lcTempCur)
  RETURN 
ENDIF 
IF EMPTY(loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption)
  REPLACE &lnCurrSz WITH 0 IN (lcTempCur)
  RETURN
ENDIF 


IF loBranchFormSet.lnOldVAlue  #  EVALUATE(lnCurrSz)  
  IF EVALUATE(lnCurrSz) < 0
    *--Can not accept negative values
    =gfModalGen('TRM42000B40011','DIALOG')
    
    REPLACE &lnCurrSz WITH loBranchFormSet.lnOldVAlue  
    RETURN 
  ENDIF
  IF EVALUATE(lnCurrSz) > 99999
    *--Can not exceed 999
    *-- 40171 : ð cannot exceeds ð  
    =gfModalGen('TRM40171B00000','DIALOG','Size Quantity|99999.')
    REPLACE &lnCurrSz WITH loBranchFormSet.lnOldVAlue  
    RETURN
  ENDIF

  m.AddSty = PADR(STYMAJOR,loBranchFormSet.lnmajorlen) +loBranchFormSet.lcstysp +SUBSTR(COLOR,1,loBranchFormSet.lnClrLen)
  SELECT (lcTempCur)

    lcSclPos = lfGetPOScale(loBranchFormSet,loBranchFormSet.ariaform1.grdLines.column&lcFPos..Header1.caption,&lcTempCur..FitDesc)
    *-- Assign a style to a location if not Assigned befor
    
    IF !SEEK(PADR(m.AddSty+IIF(loBranchFormSet.llExtended,ALLTRIM(loBranchFormSet.lcsclsp)+lcSclPos,'') ,19)+loParentForm.AriaForm1.cntPoHeader.kbWareHouse.KeyTextBox.Value+SPACE(10),'StyDye')

    
      *E300408,1 Message : 40012
      *E300408,1 Style/color xxx is not assigned to warehouse xxx
      *E300408,1 Button : 40002
      *E300408,1 Add Reenter
      
      IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.AddSty+IIF(loBranchFormSet.llExtended,ALLTRIM(loBranchFormSet.lcsclsp)+lcSclPos,'') )+'|'+TRIM(loParentForm.AriaForm1.cntPoHeader.kbWareHouse.KeyTextBox.Value))=1
      
        DO gpAdStyWar WITH PADR(m.AddSty+IIF(loBranchFormSet.llExtended,ALLTRIM(loBranchFormSet.lcsclsp)+lcSclPos,'') ,19),SPACE(10),loParentForm.AriaForm1.cntPoHeader.kbWareHouse.KeyTextBox.Value

      ELSE      
        REPLACE &lnCurrSz WITH 0
        RETURN
      ENDIF
    ENDIF


  SELECT (lcTempCur)
  lnLineTot = 0
  FOR lnT = 1 TO IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(loBranchFormSet.laSize,1) ) 
    lcT = ALLTRIM(STR(lnT))
    lnLineTot = lnLineTot + EVALUATE(lcTempCur+'.QTY'+lcT)  
  ENDFOR 
  REPLACE NTOTAL WITH lnLineTot
ENDIF  
lfPOCalcTot(loBranchFormSet)
*:**************************************************************************
*:* Name        : lfPOCalcTot
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Calculate Totals(PO Quick Order Entyr)
*:***************************************************************************
FUNCTION lfPOCalcTot
PARAMETERS loBarnFormSet

SELECT (loBarnFormSet.lcTempCur)
lnRecn = RECNO()
GO BOTTOM 

FOR lnC = 1 TO IIF(loBarnFormSet.llMultiDime ,loBarnFormSet.lnMaxSz,ALEN(loBarnFormSet.laSize,1))
  lcC = ALLTRIM(STR(lnC))
  SELECT (loBarnFormSet.lcTempCur)
  SUM (Qty&lcC) TO lnTotal FOR StyMajor <> CHR(255)
  GO BOTTOM 
  IF StyMajor = CHR(255)
    REPLACE Qty&lcC WITH lnTotal
  ENDIF   
ENDFOR 

SELECT (loBarnFormSet.lcTempCur)
SUM nTotal TO lnTotal FOR StyMajor <> CHR(255)
GO BOTTOM 
IF StyMajor = CHR(255)
  REPLACE ntotal WITH lnTotal 
ENDIF   
loBarnFormSet.AriaForm1.CmdSave.Enabled = lnTotal > 0  
*

SELECT (loBarnFormSet.lcTempCur)
IF BETWEEN(lnRecn ,1,RECCOUNT())
  GO lnRecn 
ENDIF 
loFormset =  loBarnFormSet.loparentform
lcTempCur = loBarnFormSet.lcTempCur
lccPoLine = loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder.editregion1.Parent.oMainClass.cPoLine
IF nTotal = 0 AND !&lcTempCur..llAddNew AND  SEEK(ALLTRIM(&lcTempCur..StyMajor),lccPoLine)
  loBarnFormSet.AriaForm1.CmdSave.Enabled = .T.
ENDIF 
*:**************************************************************************
*:* Name        : lfPOCrtTempFiles
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Create Temp Files(PO Quick Order Entry)
*:***************************************************************************
FUNCTION lfPOCrtTempFiles
PARAMETERS loBranchFormSet
  DIMENSION laTblFld[13,4]
  laTblFld[1,1] = "STYMAJOR"
  laTblFld[1,2] = "C"
  laTblFld[1,3] = 19
  laTblFld[1,4] = 0
  
  laTblFld[2,1] = "COLOR"
  laTblFld[2,2] = "C"
  laTblFld[2,3] = lnClrLen
  laTblFld[2,4] = 0
 
  
  laTblFld[3,1] = "COLORDSC"
  laTblFld[3,2] = "C"
  laTblFld[3,3] = 20
  laTblFld[3,4] = 0

  laTblFld[4,1] = "NTOTAL"
  laTblFld[4,2] = "N"
  laTblFld[4,3] = 6
  laTblFld[4,4] = 0
  
  laTblFld[5,1] = "NOTE_MEM"
  laTblFld[5,2] = "M"
  laTblFld[5,3] = 10
  laTblFld[5,4] = 0
  
  
  laTblFld[6,1] = "Discount"
  laTblFld[6,2] = "N"
  laTblFld[6,3] = 5
  laTblFld[6,4] = 2

  laTblFld[7,1] = "Complete"
  laTblFld[7,2] = "D"
  laTblFld[7,3] = 8
  laTblFld[7,4] = 0
  
  laTblFld[8,1] = "FitDesc"
  laTblFld[8,2] = "C"
  laTblFld[8,3] = 10
  laTblFld[8,4] = 0
   
  laTblFld[9,1] = "cVenSty"
  laTblFld[9,2] = "C"
  laTblFld[9,3] = 19
  laTblFld[9,4] = 0

  laTblFld[10,1] = "cRefer"
  laTblFld[10,2] = "C"
  laTblFld[10,3] = 15
  laTblFld[10,4] = 0
   
  laTblFld[11,1] = "lbasonsiz"
  laTblFld[11,2] = "L"
  laTblFld[11,3] = 1
  laTblFld[11,4] = 0
   
  laTblFld[12,1] = "cCstSht_Id"
  laTblFld[12,2] = "C"
  laTblFld[12,3] = 6
  laTblFld[12,4] = 0
   
  laTblFld[13,1] = "llAddNew"
  laTblFld[13,2] = "L"
  laTblFld[13,3] = 1
  laTblFld[13,4] = 0
   
  IF loBranchFormSet.llMultiDime

    FOR lnI = 1 TO lnMaxSz
      lcI = ALLTRIM(STR(lnI))
      DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
      laTblFld[ALEN(laTblFld,1),1] = 'SZ'+lcI
      laTblFld[ALEN(laTblFld,1),2] = "C"
      laTblFld[ALEN(laTblFld,1),3] = 5
      laTblFld[ALEN(laTblFld,1),4] = 0
    ENDFOR
     
    FOR lnI = 1 TO lnMaxSz
      lcI = ALLTRIM(STR(lnI))
      DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
      laTblFld[ALEN(laTblFld,1),1] = 'QTY'+lcI
      laTblFld[ALEN(laTblFld,1),2] = "N"
      laTblFld[ALEN(laTblFld,1),3] = 5
      laTblFld[ALEN(laTblFld,1),4] = 0
    ENDFOR 
    
  ELSE
    FOR lnI = 1 TO ALEN(laSize,1)
      lcI = ALLTRIM(STR(lnI))
      DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
      laTblFld[ALEN(laTblFld,1),1] = 'SZ'+lcI
      laTblFld[ALEN(laTblFld,1),2] = "C"
      laTblFld[ALEN(laTblFld,1),3] = 5
      laTblFld[ALEN(laTblFld,1),4] = 0
    ENDFOR
     
    FOR lnI = 1 TO ALEN(laSize,1)
      lcI = ALLTRIM(STR(lnI))
      DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
      laTblFld[ALEN(laTblFld,1),1] = 'QTY'+lcI
      laTblFld[ALEN(laTblFld,1),2] = "N"
      laTblFld[ALEN(laTblFld,1),3] = 5
      laTblFld[ALEN(laTblFld,1),4] = 0
    ENDFOR 
  ENDIF   
  
  FOR lnCount = 1 TO ALEN(laExtSz,1)
    lcI = ALLTRIM(STR(lnCount))
    DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'Pric'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "N"
    laTblFld[ALEN(laTblFld,1),3] = 7
    laTblFld[ALEN(laTblFld,1),4] = 2
  ENDFOR 

  FOR lnCount = 1 TO ALEN(laExtSz,1)
    lcI = ALLTRIM(STR(lnCount))
    DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'NtPri'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "N"
    laTblFld[ALEN(laTblFld,1),3] = 7
    laTblFld[ALEN(laTblFld,1),4] = 2
  ENDFOR 


  
=gfCrtTmp(loBranchFormSet.lctempcur ,@laTblFld,'STYMAJOR+COLOR+FitDesc',loBranchFormSet.lctempcur )  


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

=gfCrtTmp(loBranchFormSet.lcGrpCur,@laTblFld,'STYLE+SIZES',loBranchFormSet.lcGrpCur)  
*:**************************************************************************
*:* Name        : lfPOClose
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Close  Button(PO Quick Order Entry)
*:***************************************************************************
FUNCTION lfPOClose
PARAMETERS loBranchFormSet
llClose  = .F.
lctempcur = loBranchFormSet.lctempcur
lnRecno = RECNO(lctempcur)
SELECT (lctempcur)
LOCATE FOR StyMajor = CHR(255)
IF FOUND() AND &lctempcur..nTotal > 0
  
  llClose = (gfModalGen('QRM38093B32005','ALERT','Quantities have been entered';
                                                +'|The Quick Order Entry screen')=1)
ELSE
 IF FOUND() AND &lctempcur..nTotal = 0                                                  
   loBranchFormSet.Release
 ENDIF 
ENDIF 
IF llClose 
  loBranchFormSet.Release
ELSE
  IF BETWEEN(lnRecno ,1,RECCOUNT(lctempcur))
    GO lnRecno  IN (lctempcur)
  ENDIF
  RETURN   
ENDIF 
FUNCTION lfUpdateCmpDate
PARAMETERS loBranchFormSet
REPLACE Complete WITH loBranchFormSet.AriaForm1.DtpCompDate.value IN (loBranchFormSet.lctempcur)
*:**************************************************************************
*:* Name        : lfUpdateVenSty
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : VenDor Style Validation(PO Quick Order Entry)
*:***************************************************************************
FUNCTION lfUpdateVenSty
PARAMETERS loBranchFormSet
REPLACE cVenSty WITH loBranchFormSet.AriaForm1.txtVenSty.Value IN (loBranchFormSet.lctempcur)
*:**************************************************************************
*:* Name        : lfUpdateRef
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Refrence Validation(PO Quick Order Entry)
*:***************************************************************************
FUNCTION lfUpdateRef
PARAMETERS loBranchFormSet
REPLACE cRefer WITH loBranchFormSet.AriaForm1.txtRefr.value IN (loBranchFormSet.lctempcur)
*:**************************************************************************
*:* Name        : lfGetPOScale
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : get Scale of sent size
*:***************************************************************************
FUNCTION lfGetPOScale
PARAMETERS loBarnFormSet,lcScale,lcFit
lcRetScale = ''
=SEEK('S'+SUBSTR(Style.SCALE,1,loBarnFormSet.lnScaleLen),'Scale')
SELECT scale 
llFoundScl = .F.
SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+ SUBSTR(Style.SCALE,1,loBarnFormSet.lnScaleLen) FOR;
     IIF(loBarnFormSet.llMultiDime,ALLTRIM(cDim1)==ALLTRIM(lcFit),.T.)
  FOR lnI = 1 TO 8
    lcI = ALLTRIM(STR(lnI,1))
    IF ALLTRIM(UPPER(SZ&lcI)) = ALLTRIM(UPPER(PADR(lcScale,5)))
      lcSizorder =lcI
      lcRetScale = Scale.Scale
      llFoundScl = .T.
      EXIT 
    ENDIF 
  ENDFOR
  IF llFoundScl
    EXIT 
  ENDIF   
ENDSCAN 
RETURN lcRetScale 
*:**************************************************************************
*:* Name        : lfvPOSave
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Save button function of PO Quick Order entry
*:***************************************************************************
FUNCTION lfvPOSave
PARAMETERS loBranchFormSet
lcStyOrd = ORDER('STYLE')
SET ORDER TO STYLE IN STYLE
SELECT (loBranchFormSet.lcTempCur)
GO BOTTOM 
*!*	IF StyMajor = CHR(255)
*!*	  IF ntotal = 0 
*!*	    return 
*!*	  ENDIF 
*!*	ENDIF   
loFormset =  loBranchFormSet.loparentform
LOCAL lnI, lcTmpPosHdr, lcTmpPoLn, lcLineKey,laOldECst, laNewECst, laOldFCst, laNewFCst
PRIVATE lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth,lnCurrUnt1, lnCurrUnt2,llConCst
DIMENSION laOldECst[7], laNewECst[7], laOldFCst[7], laNewFCst[7]
STORE 0  TO laOldECst, laNewECst, laOldFCst, laNewFCst
llMultiCur = loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder.editregion1.Parent.omainclass.lMultiCurrency
lcGrpCur = loBranchFormSet.lcGrpCur 
lcStySp=  loBranchFormSet.lcStySp
lcSclSp = loBranchFormSet.lcSclSp 
lnClrLen =loBranchFormSet.lnClrLen 
lnMajorLen = loBranchFormSet.lnMajorLen
llExtended = loBranchFormSet.llExtended
lcPoLine = loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder.editregion1.Parent.oMainClass.cPoLine
m.CSTYTYPE  = 'P'
m.CBUSDOCU  = 'P'
m.PO        = loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder.editregion1.Parent.oKeyHeaderClass.kbPONo.KeyTextBox.value
m.TRANCD    = '1'
m.VENDOR    = loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder.oKeyHeaderClass.kbVendor.KeyTextBox.Value
m.CWARECODE = loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder.editregion1.Parent.oKeyHeaderClass.kbWarehouse.KeyTextBox.value
m.Scale     = Style.Scale
m.PrePak    = Style.CbuyPrePk
m.cVenSty   = Style.cVenSty
m.cStyGrade = Style.cStyGrade
m.CINVTYPE = '0001'
m.cUomCode = "EAC"
m.Shipvia = ALLTRIM(loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder.oHeaderClass.cboShipVia.Value)
lcPriceCurr   = loFormSet.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cntMultiCurrency.KbPriceCurrency.KeyTextBox.Value
lnPricRate    = loFormSet.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cntMultiCurrency.txtPriceRate.Value
*-- Get the selected duty currency in the header folder
lcDutyCurr    = loFormSet.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cntMultiCurrency.KbDutyCurrency.KeyTextBox.Value
lnDutyRate    = loFormSet.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cntMultiCurrency.txtDutyRate.Value
lcTempCur = loBranchFormSet.lcTempCur
SELECT (lcTempCur)
GO TOP
loDetailSection = loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder
lcPosHdr = loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder.editregion1.Parent.omainclass.cPosHdr
lcDutCurr = loFormSet.AriaForm1.PgfPOStyle.page1.cntheaderFolder.cntMultiCurrency.KbDutyCurrency.KeyTextBox.Value

lccPoLine = loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder.editregion1.Parent.oMainClass.cPoLine
SCAN FOR StyMajor <> CHR(255)&&and nTotal>0
  m.cCstSht_Id = &lcTempCur..cCstSht_Id
  M.Reference = &lcTempCur..cRefer 
  M.cVenSty = &lcTempCur..cVenSty 
  lnIncrmnt = 0  
  FOR lnQkCnt = 1 TO ALEN(loBranchFormSet.laExtSz,1)
  
     IF IIF(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz < lnIncrmnt ,.F.)
       lnIncrmnt = 0
     ENDIF 
  
     SELECT (lcTempCur)
     m.Style = PADR(PADR(StyMajor ,loBranchFormSet.lnmajorlen )+lcStySp +PADR(COLOR,lnClrLen)+IIF(loBranchFormSet.llExtended,ALLTRIM(lcSclSp) +loBranchFormSet.laExtSz[lnQkCnt,1],''),19)
     =SEEK(m.Style,'Style')
     =SEEK('S'+loBranchFormSet.laExtSz[lnQkCnt,1],'SCALE')
     IF ALLTRIM(Scale.CDim1) <> ALLTRIM(&lcTempCur..FitDesc)
       LOOP 
     ENDIF 
     
     SELECT (lcTempCur)
     STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,;
                m.nICost1,m.nICost2,m.nICost3,m.nICost4,m.nICost5,m.nICost6,m.nICost7,;
                m.nECost1,m.nECost2,m.nECost3,m.nECost4,m.nECost5,m.nECost7,m.nECost6
      STORE 0  TO loDetailSection.nFcost1, loDetailSection.nFcost2, loDetailSection.nFcost3, loDetailSection.nFcost4,loDetailSection.nFcost5, loDetailSection.nFcost6, loDetailSection.nFcost7 
      STORE '' TO lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
      STORE 1  TO lnCurrUnt1, lnCurrUnt2


      DIMENSION laNewECst[7], laNewFCst[7], laOldECst [7], laOldFCst[7]
      STORE 0 TO laNewECst, laNewFCst, laOldECst, laOldFCst
      IF nTotal>0 OR (nTotal = 0 AND !&lcTempCur..llAddNew AND  SEEK(m.stYle,lccPoLine))
        SELECT(lcGrpCur)
        IF SEEK(m.Style,lcGrpCur)
          DIMENSION laSizesArr[1]
          STORE '' TO  laSizesArr
          SCAN REST WHILE STYLE+SIZES = m.Style &&FOR lcSizorder $ SIZES
            lnRecNumber = RECNO()
            SELECT (lcTempCur)
            STORE 0  TO loDetailSection.nFcost1, loDetailSection.nFcost2, loDetailSection.nFcost3, loDetailSection.nFcost4,loDetailSection.nFcost5, loDetailSection.nFcost6, loDetailSection.nFcost7 
            STORE '' TO lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth,lcSizes
            STORE 1  TO lnCurrUnt1, lnCurrUnt2
            STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
           
            *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[Start]
            lcCurSizes = ALLTRIM(&lcGrpCur..SIZES)
            llfoundLess = .F.
            IF !EMPTY(lcCurSizes ) 
              llfoundLess = .F.
              FOR s=1 TO LEN(lcCurSizes)
                llSzAdd = .F.
                FOR lnA = 1 TO ALEN(laSizesArr,1)
                  IF SUBSTR(lcCurSizes,s,1) $ laSizesArr[lnA]
                    llSzAdd = .T.
                    EXIT 
                  ENDIF 
                ENDFOR  
                IF llSzAdd 
                  LOOP 
                ENDIF 
                IF SEEK(m.Style,lcGrpCur)
                  SELECT(lcGrpCur)
                  SCAN REST WHILE STYLE+SIZES = m.Style FOR SUBSTR(lcCurSizes,s,1) $ SIZES AND LEN(ALLTRIM(&lcGrpCur..SIZES)) < LEN(lcCurSizes)
                    llfoundLess = .T.
                    EXIT 
                  ENDSCAN
                  IF llfoundLess
                    EXIT 
                  ENDIF  
                ENDIF 
              ENDFOR
            ENDIF
            IF !llfoundLess
              SELECT(lcGrpCur)
              IF BETWEEN(lnRecNumber,1,RECCOUNT())
                GO RECORD lnRecNumber
              ENDIF 
            ENDIF 
            SELECT (lcTempCur)
            *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[End]
            
            FOR lnJ = lnIncrmnt + 1 TO lnIncrmnt + loBranchFormSet.laExtSz[lnQkCnt,2]        
              lcJ = ALLT(STR(lnJ))
              lcQ = STR(lnJ - lnIncrmnt,1)
              IF !(lcQ $ &lcGrpCur..SIZES)
                LOOP  
              ENDIF 
              lloop =.F.
              FOR lnW = 1 TO ALEN(laSizesArr,1)
                IF lcQ $ laSizesArr[lnW]
                  lloop =.T.
                  EXIT 
                ENDIF 
              ENDFOR  
              IF lloop 
                LOOP  
              ENDIF 
              lcSizes = lcSizes + lcQ 
              m.TotQty = m.TotQty +  Qty&lcJ
              m.Qty&lcQ = Qty&lcJ
            ENDFOR  
            IF m.TotQty = 0 OR EMPTY(lcSizes) AND !((m.TotQty = 0 AND !&lcTempCur..llAddNew AND  SEEK(m.stYle,lccPoLine)))
              LOOP 
            ENDIF   
            loDetailSection.mGetbomCurr(&lcTempCur..cCstSht_Id) 
            loDetailSection.nFCost1 = loDetailSection.laBomInfo[1,5]
            loDetailSection.nFCost2 = loDetailSection.laBomInfo[2,5]
            loDetailSection.nFCost3 = loDetailSection.laBomInfo[3,5]
            loDetailSection.nFCost4 = loDetailSection.laBomInfo[4,5]
            loDetailSection.nFCost5 = loDetailSection.laBomInfo[5,5]
            loDetailSection.nFCost6 = loDetailSection.laBomInfo[6,5]
            loDetailSection.nFCost7 = loDetailSection.laBomInfo[7,5]
            m.nFCost1 = loDetailSection.nFCost1 
            m.nFCost2 = loDetailSection.nFCost2 
            m.nFCost3 = loDetailSection.nFCost3 
            m.nFCost4 = loDetailSection.nFCost4
            m.nFCost5 = loDetailSection.nFCost5 
            m.nFCost6 = loDetailSection.nFCost6 
            m.nFCost7 = loDetailSection.nFCost7 
            m.cPackUOM   = Style.cPackUOM
            m.cPackType  = Style.cPackType
            m.nInPackQty = Style.nInPackQty
            m.nInPackHgt = Style.nInPackHgt
            m.nInPackWdt = Style.nInPackWdt
            m.nInPackLen = Style.nInPackLen
            m.nInPackWgh = Style.nInPackWgt    
            m.nMsPackQty = Style.nMsPackQty
            m.nMsPackHgt = Style.nMsPackHgt
            m.nMsPackWdt = Style.nMsPackWdt
            m.nMsPackLen = Style.nMsPackLen
            m.nMsPackWgh = Style.nMsPackWgt
            m.Complete  = Complete
            m.cStyGrade = Style.cStyGrade
            SELECT (lcGrpCur)
            =SEEK(m.Style,lcGrpCur)
            
            *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[Start]
            *SUM GROS_PRICE REST WHILE STYLE+SIZES = m.Style FOR ALLTRIM(lcSizes) $ SIZES  TO m.GROS_PRICE
            STORE '' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8
            FOR lnW =1 TO LEN(ALLTRIM(lcSizes))
              lcW = STR(lnW,1)
              STORE SUBSTR(ALLTRIM(lcSizes),lnW,1) TO lcSz&lcW
            ENDFOR 
            SUM GROS_PRICE REST WHILE STYLE+SIZES = m.Style FOR IIF(!EMPTY(lcSz1),lcSz1 $ SIZES,.T.) AND ;
													            IIF(!EMPTY(lcSz2),lcSz2 $ SIZES,.T.) AND ;
													            IIF(!EMPTY(lcSz3),lcSz3 $ SIZES,.T.) AND ;
													            IIF(!EMPTY(lcSz4),lcSz4 $ SIZES,.T.) AND ;
													            IIF(!EMPTY(lcSz5),lcSz5 $ SIZES,.T.) AND ;
													            IIF(!EMPTY(lcSz6),lcSz6 $ SIZES,.T.) AND ;
													            IIF(!EMPTY(lcSz7),lcSz7 $ SIZES,.T.) AND ;
													            IIF(!EMPTY(lcSz8),lcSz8 $ SIZES,.T.)  TO m.GROS_PRICE
            *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[End]
            
            IF EMPTY(laSizesArr[1])
              laSizesArr[1] = lcSizes 
            ELSE
              DIMENSION  laSizesArr[ALEN(laSizesArr,1)+1] 
              laSizesArr[ALEN(laSizesArr,1)] = lcSizes 
            ENDIF 
            m.nICost1      = m.GROS_PRICE 
            m.Scale       = STYLE.SCALE     
            m.PrePak      = Style.CbuyPrePk
            STORE m.Gros_Price TO loDetailSection.nFCost1, m.nFCost1
            m.nICost1 = loDetailSection.laBomInfo[1,4]
            m.nICost2 = loDetailSection.laBomInfo[2,4]
            m.nICost3 = loDetailSection.laBomInfo[3,4]
            m.nICost4 = loDetailSection.laBomInfo[4,4]
            m.nICost5 = loDetailSection.laBomInfo[5,4]
            m.nICost6 = loDetailSection.laBomInfo[6,4]
            m.nICost7 = loDetailSection.laBomInfo[7,4]
            
            *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[Start]
            STORE m.Gros_Price TO  m.nICost1
            *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[End]
            
            SELECT &lcPoLine
            LOCATE FOR STYLE  = M.STYLE AND Complete = m.Complete AND GROS_PRICE = m.GROS_PRICE  
            IF !FOUND() OR &lcTempCur..llAddNew
              loDetailSection.nPoLineNo  =  loDetailSection.npolineno + 1
              m.LineNo = loDetailSection.nPoLineNo  
              INSERT INTO (lcPoLine) FROM MEMVAR
              SELECT(loDetailSection.omainclass.cposhdr)
              REPLACE nStyOrder WITH nStyOrder + m.TotQty,;
                      Open      WITH Open      + m.TotQty IN (lcPosHdr )
              
              FOR lnI = 1 TO 7
                lcI = STR(lnI,1)
                STORE m.nICost&lcI TO laOldECst[lnI], laNewECst[lnI]
                STORE m.nFCost&lcI TO laOldFCst[lnI], laNewFCst[lnI]
              ENDFOR 
              loDetailSection.omainclass.mCalEstCst(m.TotQty, 0,@laOldECst,@laNewECst,;
                     @laOldFCst,@laNewFCst,llMultiCur,.T.,lcPriceCurr,lnPricRate,;
                               lcDutyCurr)

              
            ELSE
              *--Update laData array    
              *(m.TotQty = 0 AND !&lcTempCur..llAddNew AND  SEEK(m.stYle,lccPoLine))
             FOR lnI = 1 TO 7
               lcI = STR(lnI,1)
               STORE nICost&lcI TO laOldECst[lnI]
               STORE m.nICost&lcI TO  laNewECst[lnI]
               STORE nFCost&lcI TO laOldFCst[lnI]
               STORE m.nICost&lcI TO  laNewFCst[lnI]
             ENDFOR 

             lnOldQty = TotQty
             IF (m.TotQty = 0 AND !&lcTempCur..llAddNew )   
                DELETE 
                REPLACE nStyOrder WITH nStyOrder -lnOldQty  ,;
	                  Open      WITH Open      -lnOldQty IN (lcPosHdr )
             ELSE
               IF &lcTempCur..llAddNew 
                 APPEND BLANK 
                 lnOldQty  = 0
                 FOR lnI = 1 TO 7
                   lcI = STR(lnI,1)
                   STORE m.nICost&lcI TO laOldECst[lnI]
                   STORE m.nICost&lcI TO  laNewECst[lnI]
                   STORE m.nFCost&lcI TO laOldFCst[lnI]
                   STORE m.nICost&lcI TO  laNewFCst[lnI]
                 ENDFOR 

               ENDIF 
               GATHER  MEMO  MEMVAR
               REPLACE nStyOrder WITH nStyOrder -lnOldQty +  m.TotQty  ,;
		                Open      WITH Open      -lnOldQty +  m.TotQty IN (lcPosHdr )
             ENDIF   
             loDetailSection.omainclass.mCalEstCst(m.TotQty, lnOldQty ,@laOldECst,@laNewECst,;
                     @laOldFCst,@laNewFCst,llMultiCur,.T.,lcPriceCurr,lnPricRate,;
                               lcDutyCurr)

            ENDIF
            SELECT(lcGrpCur)
            IF BETWEEN(lnRecNumber,1,RECCOUNT())
              GO RECORD lnRecNumber
            ENDIF 
          ENDSCAN   
        ELSE
          SELECT (lcTempCur)
          STORE 0  TO loDetailSection.nFcost1, loDetailSection.nFcost2, loDetailSection.nFcost3, loDetailSection.nFcost4,loDetailSection.nFcost5, loDetailSection.nFcost6, loDetailSection.nFcost7 
          STORE '' TO lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
          STORE 1  TO lnCurrUnt1, lnCurrUnt2
          FOR lnJ = lnIncrmnt + 1 TO lnIncrmnt + loBranchFormSet.laExtSz[lnQkCnt,2]        
            lcJ = ALLT(STR(lnJ))
            lcQ = STR(lnJ - lnIncrmnt,1)
            m.TotQty = m.TotQty +  Qty&lcJ
            m.Qty&lcQ = Qty&lcJ
          ENDFOR  
          IF m.TotQty = 0 AND !((m.TotQty = 0 AND !&lcTempCur..llAddNew AND  SEEK(m.stYle,lccPoLine)))
            LOOP 
          ENDIF   
          loDetailSection.mGetbomCurr(&lcTempCur..cCstSht_Id) 
          loDetailSection.nFCost1 = loDetailSection.laBomInfo[1,5]
          loDetailSection.nFCost2 = loDetailSection.laBomInfo[2,5]
          loDetailSection.nFCost3 = loDetailSection.laBomInfo[3,5]
          loDetailSection.nFCost4 = loDetailSection.laBomInfo[4,5]
          loDetailSection.nFCost5 = loDetailSection.laBomInfo[5,5]
          loDetailSection.nFCost6 = loDetailSection.laBomInfo[6,5]
          loDetailSection.nFCost7 = loDetailSection.laBomInfo[7,5]
          m.nFCost1 = loDetailSection.nFCost1 
          m.nFCost2 = loDetailSection.nFCost2 
          m.nFCost3 = loDetailSection.nFCost3 
          m.nFCost4 = loDetailSection.nFCost4
          m.nFCost5 = loDetailSection.nFCost5 
          m.nFCost6 = loDetailSection.nFCost6 
          m.nFCost7 = loDetailSection.nFCost7 
          m.cPackUOM   = Style.cPackUOM
          m.cPackType  = Style.cPackType
          m.nInPackQty = Style.nInPackQty
          m.nInPackHgt = Style.nInPackHgt
          m.nInPackWdt = Style.nInPackWdt
          m.nInPackLen = Style.nInPackLen
          m.nInPackWgh = Style.nInPackWgt    
          m.nMsPackQty = Style.nMsPackQty
          m.nMsPackHgt = Style.nMsPackHgt
          m.nMsPackWdt = Style.nMsPackWdt
          m.nMsPackLen = Style.nMsPackLen
          m.nMsPackWgh = Style.nMsPackWgt
          m.Complete  = Complete
          m.cStyGrade = Style.cStyGrade
          m.Scale       = STYLE.SCALE     
          m.PrePak      = Style.CbuyPrePk
          *STORE m.Gros_Price TO loDetailSection.nFCost1, m.nFCost1
          m.nICost1 = loDetailSection.laBomInfo[1,4]
          m.nICost2 = loDetailSection.laBomInfo[2,4]
          m.nICost3 = loDetailSection.laBomInfo[3,4]
          m.nICost4 = loDetailSection.laBomInfo[4,4]
          m.nICost5 = loDetailSection.laBomInfo[5,4]
          m.nICost6 = loDetailSection.laBomInfo[6,4]
          m.nICost7 = loDetailSection.laBomInfo[7,4]
          m.GROS_PRICE =  m.nICost1
          SELECT &lcPoLine
          LOCATE FOR STYLE  = M.STYLE AND Complete = m.Complete AND GROS_PRICE = m.GROS_PRICE  
          IF !FOUND() OR &lcTempCur..llAddNew
            loDetailSection.nPoLineNo  =  loDetailSection.npolineno + 1
            m.LineNo = loDetailSection.nPoLineNo  
            INSERT INTO (lcPoLine) FROM MEMVAR
            SELECT(loDetailSection.omainclass.cposhdr)
            REPLACE nStyOrder WITH nStyOrder + m.TotQty,;
                    Open      WITH Open      + m.TotQty IN (lcPosHdr )
            
            FOR lnI = 1 TO 7
              lcI = STR(lnI,1)
              STORE m.nICost&lcI TO laOldECst[lnI], laNewECst[lnI]
              STORE m.nFCost&lcI TO laOldFCst[lnI], laNewFCst[lnI]
            ENDFOR 
            loDetailSection.omainclass.mCalEstCst(m.TotQty, 0,@laOldECst,@laNewECst,;
                   @laOldFCst,@laNewFCst,llMultiCur,.T.,lcPriceCurr,lnPricRate,;
                             lcDutyCurr)

            
          ELSE
            *--Update laData array    
            FOR lnI = 1 TO 7
               lcI = STR(lnI,1)
               STORE nICost&lcI TO laOldECst[lnI]
               STORE m.nICost&lcI TO  laNewECst[lnI]
               STORE nFCost&lcI TO laOldFCst[lnI]
               STORE m.nICost&lcI TO  laNewFCst[lnI]
             ENDFOR 
             

            lnOldQty = TotQty  
            *(m.TotQty = 0 AND !&lcTempCur..llAddNew AND  SEEK(m.stYle,lccPoLine))
            IF  m.TotQty = 0 AND !&lcTempCur..llAddNew
              DELETE 
              REPLACE nStyOrder WITH nStyOrder -lnOldQty  ,;
	                  Open      WITH Open      -lnOldQty IN (lcPosHdr )
            ELSE
              IF &lcTempCur..llAddNew  AND m.TotQty <> 0 
                APPEND BLANK 
                lnOldQty  = 0
                FOR lnI = 1 TO 7
                  lcI = STR(lnI,1)
                  STORE m.nICost&lcI TO laOldECst[lnI]
                  STORE m.nICost&lcI TO  laNewECst[lnI]
                  STORE m.nFCost&lcI TO laOldFCst[lnI]
                  STORE m.nICost&lcI TO  laNewFCst[lnI]
                ENDFOR 
              ENDIF 
              GATHER  MEMO MEMVAR
              REPLACE nStyOrder WITH nStyOrder -lnOldQty +  m.TotQty  ,;
	                  Open      WITH Open      -lnOldQty +  m.TotQty IN (lcPosHdr )
            ENDIF 
            loDetailSection.omainclass.mCalEstCst(m.TotQty, lnOldQty ,@laOldECst,@laNewECst,;
                   @laOldFCst,@laNewFCst,llMultiCur,.T.,lcPriceCurr,lnPricRate,;
                             lcDutyCurr)
                             

        ENDIF
      ENDIF   
     ENDIF
     lnIncrmnt = lnIncrmnt + loBranchFormSet.laExtSz[lnQkCnt,2]
  ENDFOR      
  
ENDSCAN
*-- Clear screen
lfvPClear(loBranchFormSet,.T.) 
loFormSet.ariaform1.pgfPOStyle.page2.cntDetailFolder.grdPODetails.refresh()  
SET ORDER TO &lcStyOrd IN STYLE
*! C201115,1 MMT 03/11/2009 Convert PO Quick Order Entry Screen to Aria4      [End]


*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
*:**************************************************************************
*:* Name        : lfPRICCODE
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 04/05/2009
*:* Purpose     : get Customer Price code for Quick Order entry screen
*:***************************************************************************
FUNCTION lfQKPRICCODE
LOCAL lcSlct,lcPricCode,lcSvKey
lcSlct = SELECT()
lcPricCode = ''
lcCurrCode=loParentForm.ariaform1.ariapageframe1.page1.keyCurrency.Keytextbox.value
IF !USED('Customer_A')
  gfOpenTable('Customer','Customer','SH','Customer_A')
ENDIF 
IF !USED('CSTPRICE')
  gfOpenTable('CSTPRICE','CSTPRICE','SH','CSTPRICE')
ENDIF 
IF !USED('CSTPRICH')
   gfOpenTable('CSTPRICH','CSTPRICH','SH','CSTPRICH')
ENDIF 


SELECT Customer_A
lcAccount = &lcOrdHdr..Account

lcSvKey = EVALUATE(KEY())
=gfSEEK('M'+lcAccount,'Customer_A')

LOCAL lnI,lcI,lcNoDtPCod

*-- Loop through all price codes
*-   pick the first price code with suitable date,
*-   if no one, pick the first one with no valid dates , otherwise return chr(255)
lcNoDtPCod = CHR(255)
  
*- Download needed lines to seek in from cstprice and cstprich sql files
SELECT CSTPRICE
=gfSetOrder('CSTYCODE')
=gfSEEK(lcSeekSty,'CSTPRICE')
=gfSetOrder('CSTPRICE')

PRIVATE lcStyClr
lcStyClr = PADR(SUBSTR(lcSeekSty,1,lnClrPos+lnClrLen-1),19) 
SELECT CSTPRICH
=gfSetOrder('STYLE')
=gfSEEK(lcStyClr,'CSTPRICH')
=gfSetOrder('CSTPRICH')
  
FOR lnI = 1 TO 15
  lcI = IIF(lnI = 1 , '' , PADL(lnI,2,'0') ) 
    
  IF !EMPTY(Customer_A.PRICCODE&lcI).AND. gfSEEK(Customer_A.PRICCODE&lcI+lcCurrCode+lcSeekSty,'CSTPRICE') ;
                                  .AND. gfSEEK(Customer_A.PRICCODE&lcI+lcCurrCode+lcStyClr,'CSTPRICH')
    IF EMPTY(CSTPRICH.DVLDPRTO)
      *- Get no valid date price code
      lcNoDtPCod = UPPER(Customer_A.PRICCODE&lcI)
    ELSE
      *- Compare valid  prices for Banach based on setup ( Entered, Start or Complete Dates)
      IF BETWEEN(ldChkDate,CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)
        EXIT
      ENDIF

    ENDIF
  ENDIF    
ENDFOR

lcPRICCODE = IIF(lnI < 16 , UPPER(Customer_A.PRICCODE&lcI) , lcNoDtPCod )

*- restore customer record
=gfSeek(lcSvKey,'Customer_A')

SELECT (lcSlct)
RETURN lcPRICCODE
*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

*! C201219,1 MMT 03/15/2010 Enable Cost sheet Button in case of Status Actualized [Start]
*:**************************************************************************
*:* Name        : lfENBCSTSHT 
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/15/2010
*:* Purpose     : Enable Cost sheet Button In PO Screen in case of Status Actualized
*:***************************************************************************
FUNCTION lfENBCSTSHT 
lcTmpHdr = loFormSet.ariaForm1.MainWorkOrder.cPosHdr
IF  loFormSet.ActiveMode = 'V' AND  loFormSet.ariaForm1.MainWorkOrder.cworkordtyp $ "PP|NN" AND ;
  &lcTmpHdr..Status = 'A'
  oariaapplication.otoolbar.ChangeButtonStatus('cmdCostSh','ENABLED')
ENDIF  
*! C201219,1 MMT 03/15/2010 Enable Cost sheet Button in case of Status Actualized [End]

*! C201245,1 MMT 06/01/2010 Custom Program to allow user to change account of EDI Temp. Order[Start]
*:**************************************************************************
*:* Name        : lfADDMNCHGAC
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 06/01/2010
*:* Purpose     : Add OPtion Menu to Change order account
*:***************************************************************************
FUNCTION lfADDMNCHGAC
LOCAL lnCntBar
lcHostFormName = '[' + loFormSet.cHostFormName + ']'
lnCntBar = CNTBAR('_INQURYPOP')+1
DEFINE BAR lnCntBar OF _INQURYPOP PROMPT '\<Change Account Code' SKIP FOR ;
    gfFormIsActive(&lcHostFormName) .AND. ((_screen.ActiveForm.Parent.ActiveMode<>'E') OR  ( _Screen.ActiveForm.cboStatus.Value <> 'B'))

ON SELECTION BAR lnCntBar OF _INQURYPOP DO lfEnbaleAccount

*:**************************************************************************
*:* Name        : lfEnbaleAccount
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 06/01/2010
*:* Purpose     : enable account field 
*:***************************************************************************
FUNCTION lfEnbaleAccount
loFormSet = _Screen.ActiveForm.Parent
IF loFormSet.ActiveMode = 'E' AND loFormSet.AriaForm1.cboStatus.Value = 'B'
  loFormSet.ariaform1.keyAccount.Enabled = .T. 
  loFormSet.ariaform1.keyAccount.keyTextBox.SetFocus()

ENDIF   
*:**************************************************************************
*:* Name        : lfEDITMPACCV
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 06/01/2010
*:* Purpose     : account field validation
*:***************************************************************************
FUNCTION lfEDITMPACCV
xAccount = loFormSet.ariaform1.keyAccount.keytextbox.Value
IF !INLIST(Customer.Status,'A','P')
  IF Customer.Status = 'H' AND loFormSet.oFormEnvironment.laSetups[23,2]
    IF (gfModalGen('QRM32145B32005','ALERT',xAccount) <> 1)
      loFormSet.ariaform1.keyAccount.keytextbox.Value = loFormSet.ariaform1.keyAccount.keytextbox.OldValue
      loFormSet.ariaform1.keyAccount.Tag ='1'
      RETURN
    ENDIF 
  ELSE  
        *E302520,1 MMT 04/09/2008 Add Setup to Allow user create SO for Hold Accounts{End}
    *-- Message : 32023
    *-- This a non-active xxxxx. Order entry is not allowed!
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32023B00000','ALERT','account'+'|'+"Order")
    loFormSet.ariaform1.keyAccount.keytextbox.Value = loFormSet.ariaform1.keyAccount.keytextbox.OldValue
    loFormSet.ariaform1.keyAccount.Tag ='1'
    RETURN
  ENDIF
ENDIF   
*check if Order has a Store Code and the Store Code does not exist on the new account code 
lcTmpOrdHdr = loFormSet.OFORMENVIRONMENT.lcOrdHdr
lcStoreNum = &lcTmpOrdHdr..Store 
IF !EMPTY(lcStoreNum)
  lnCustRec = RECNO('Customer')
  IF !SEEK('S'+xAccount+lcStoreNum ,'Customer','Customer')  
    IF gfModalGen('QRM00000B00031',.F.,.F.,.F.,;
       'The new account code selected does include store code '+ALLTRIM(lcStoreNum)+' - please add this store to new account '+xAccount+' or select a different account code') = 1
      oAriaApplication.DoProgram("AWRARCUST",'"'+xAccount+'"',.F.,'')       
      _screen.ActiveForm.KeyStore.KeyTextBox.Value = lcStoreNum 
      _screen.ActiveForm.KeyStore.KeyTextBox.Valid()
      _screen.ActiveForm.Parent.SHOW(1)       
      IF !SEEK('S'+xAccount+lcStoreNum ,'Customer','Customer')  
        loFormSet.ariaform1.keyAccount.keytextbox.Value = loFormSet.ariaform1.keyAccount.keytextbox.OldValue
        loFormSet.ariaform1.keyAccount.Tag ='1'
        IF BETWEEN(lnCustRec,1,RECCOUNT('Customer'))
          GO RECORD lnCustRec IN 'Customer'
        ENDIF 
        RETURN
      ENDIF 
    ELSE
      loFormSet.ariaform1.keyAccount.keytextbox.Value = loFormSet.ariaform1.keyAccount.keytextbox.OldValue
      loFormSet.ariaform1.keyAccount.Tag ='1'
      IF BETWEEN(lnCustRec,1,RECCOUNT('Customer'))
        GO RECORD lnCustRec IN 'Customer'
      ENDIF 
      RETURN
    ENDIF 
  ENDIF 
  IF BETWEEN(lnCustRec,1,RECCOUNT('Customer'))
    GO RECORD lnCustRec IN 'Customer'
  ENDIF 
ENDIF 
lcTmpOrdLine = loFormSet.oFormEnvironment.lcOrdLine
lnRecnDet = RECNO(lcTmpOrdLine)
REPLACE Account WITH loFormSet.ariaform1.keyAccount.keyTextBox.Value ,;
        FLAG WITH IIF(FLAG <> 'N','M',FLAG)  IN (lcTmpOrdLine) ALL 
IF BETWEEN(lnRecnDet ,1,RECCOUNT(lcTmpOrdLine))
  GO RECORD lnRecnDet IN (lcTmpOrdLine)
ENDIF 
loFormSet.mGetOrderInformation ()
*! C201245,1 MMT 06/01/2010 Custom Program to allow user to change account of EDI Temp. Order[End]