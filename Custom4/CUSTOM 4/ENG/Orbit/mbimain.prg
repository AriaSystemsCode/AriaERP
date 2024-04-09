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
*! C201245,1 MMT 06/01/2010 Custom Program to allow user to change account of EDI Temp. Order[T20100219.0001]
*! B609449,1 MMT 11/04/2010 SO quick order entry screen style browser displays style color[T20100906.0003]
*! B609449,1 MMT 11/04/2010 SO quick order entry screen Give error when style has about 35 sizes[T20100906.0003]
*! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{T20101013.0001}
*! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{T20101013.0001}
*! B609499,1 MMT 01/13/2011 Quick Order entry screen uses wrong style purchase price[T20101102.0011]
*! B609500,1 TMI 01/15/2011 QOE screen does not save last qty [T20101130.0002 ]
*! C201302,3 MMT 01/24/2011 Display Price message in Quick So entry based on trigger{T20101013.0001}
*! B609513,1 MMT 01/27/2011 Fix error in SO Quick order entry screen in case of Mutli dimension scale[T20110114.0005]
*! B609536,1 MMT 02/24/2011 PO Quick Order entry not pick up Purchase Prices for 2nd Style [T20110221.0001]
*! B609542,1 SAB 03/02/2011 Sales order Quick order entry error when style filter in place [T20110222.0005]
*! B609542,2 MMT 03/16/2011 Sales order Quick order entry error when style filter in place [T20110222.0005]
*! B609576,1 MMT 05/02/2011 PO- Quick Order entry screen uses incorrect Purchase Price {T20110404.0002}
*! B609683,1 MMT 10/04/2011 Fix bug of wrong gross price while adding line from PO Quick Order entry [T20110819.0002]
*! B609736,1 MMT 11/20/2011 SO - Quick Order entry not pkcking up Styprice values[T20110725.0007]
*! B609736,1 MMT 11/28/2011 So - quick order entry screen not showing all colours of a style[T20110822.0008]
*! B609905,1 MMT 05/06/2012 Quick order entry does not validate style season[T20120330.0007]
*! B609905,2 MMT 05/10/2012 Validate Season on color level [T20120330.0007]
*! B609905,3 MMT 05/15/2012 Change Season Validation message [T20120330.0007]
*! B610000,1 HIA 07/12/2012 Color description, not displayed compelteded [T20120703.0019]
*! B610103,1 MMT 09/26/2012 Ignore OnHold Style/Color in SO Quich Order entry screen[T20120911.0002]
*! B610153,2 SAB 11/13/2012 Fix problem of not showing Order Charges on Sales Order Option [T20121102.0002]
*! B610208,1 HIA 01/22/2013 Aria4xp - PO - Quick Order Entry displays browse of Imported Style cost [T20130109.0027]
*! C201550,1 RAS 01/27/2013 Aria4xp - SO - Modi Sizes Aligmnent at SoQKORD Screen to be Right [T20130109.0026]
*! C201550,2 RAS 01/27/2013 Aria4xp - SO - Modi Sizes Aligmnent at SoQKORD Screen to be Right [T20130109.0026]
*! C201563,E303372,1 TMI 03/22/2013 open the appyinv screen from the GLQENTR.Scx SCREEN, update the custom fields in the GLPTRNDT with the invoice # & vendor code [T20130301.0001]
*! B610473,1 HIA 15/08/2013 T20130805.0001 - Aria4xp - Style Purchase Order - Quick Order Entry screen not updating PO detail screen
*! B610473,2 HIA 10/10/2013 Error in Quick Order Entry screen when style has cost sheet per size[T20130805.0001]
*! B610639,1 MMT 12/26/2013 Quick order entry screen gives error with certain style[T20131218.0001]
*! C201930 SARA.O 01/31/2017 DCC Price uplift by Store [P20170117.0001]
*:***************************************************************************

Parameter loFormSet,lcEvntFun,lcFunPars

lcFunPars  = Iif(Type('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+Allt(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = Eval(lcFunToRun)

Return llRetValue

*:**************************************************************************
*:* Name        : lfCPYNOTES
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/24/2008
*:* Purpose     : Query user if wants to copy Style Notes or not
*:***************************************************************************
Function lfCPYNOTES

If gfModalGen('QRM00443B42002','DIALOG')=1
  gfOpenTable('objlink','COBJLINK','SH','OBLNK')
  gfOpenTable('objects','OBJECTID','SH','OBJCT')
  gfOpenTable('Notepad','NOTEPAD','SH','NOTES')



  If gfSeek('S'+loFormSet.lcFromStyle,'OBLNK','OBJLNKTY') And gfSeek(OBLNK.cobject_id,'OBJCT','OBJECTID')
    loFormSet.ariaform1.cntThumbnail.ariaimage1.Visible = .F.
    loFormSet.ariaform1.cntThumbnail.oleboundcontrol1.ControlSource = 'OBJCT.GOBJECT'
    loFormSet.ariaform1.cntThumbnail.oleboundcontrol1.Visible = !Empty(OBJCT.GOBJECT)
  Endif

Endif


*:**************************************************************************
*:* Name        : lfLastObjID
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/24/2008
*:* Purpose     : Get New object ID
*:***************************************************************************
Function lfLastObjID

lcReturn = ''
If Used('OBLNK')
  Select OBLNK
  lcFilter = Filter()
  Set Filter To
  Go Bott
  lcReturn = Padl(Int(Val(cobject_id))+1,10,'0')
  Set Filter To &lcFilter
Endif
Return lcReturn

*:**************************************************************************
*:* Name        : lfSAVNOTES
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/24/2008
*:* Purpose     : Save Copied data
*:***************************************************************************
Function lfSAVNOTES
*!*	ACTIVATE WINDOW trace
*!*	SUSPEND
If !Used('OBLNK')  Or !Used('OBJCT') Or !Used('NOTES')
  Return
Endif
lcAlis = Alias()
lcNewObjId = lfLastObjID()
Select OBLNK
lcOrder = Order()
gfsetOrder('OBJLNKTY')
If gfSeek('S'+loFormSet.lcFromStyle,'OBLNK','OBJLNKTY')
  Select OBLNK
  Scan Rest While  cobjlnktyp+cobjlink='S'+loFormSet.lcFromStyle
    gfSeek(OBLNK.cobject_id,'OBJCT','OBJECTID')
    lcOldObj = OBLNK.cobject_id
    Select OBLNK
    lnRecNum = Recno()
    Scatter Memo Memvar
    m.cobject_id = lcNewObjId
    m.cobjlink  = Style.CstyMajor
    gfAppend('IN OBLNK',.T.)
    =gfAdd_Info('OBLNK')
    gfReplace()
    gfTableUpdate()

    Select OBJCT
    lcTempName = gfTempName()
    Select * From OBJCT Where cobject_id = lcOldObj Into Dbf (Oariaapplication.workdir+lcTempName +'.dbf')

    If !Used(lcTempName) And File(Oariaapplication.workdir+lcTempName +'.dbf')
      Use (Oariaapplication.workdir+lcTempName +'.dbf') In 0
    Endif

    Replace cobject_id With  lcNewObjId
    Use In (lcTempName)
    Select OBJCT
    Append From (Oariaapplication.workdir+lcTempName +'.dbf')
    =gfAdd_Info('OBJCT')
    gfReplace()
    gfTableUpdate()

    Erase (Oariaapplication.workdir+lcTempName +'.*')
    lcNewObjId= Padl(Int(Val(lcNewObjId))+1,10,'0')
    Select OBLNK
    If Between(lnRecNum,1,Reccount())
      Go Record lnRecNum
    Endif
  Endscan
Endif
Select OBLNK
gfsetOrder(lcOrder )
gfCloseTable('OBJCT')
gfCloseTable('OBLNK')

If gfSeek("F"+loFormSet.lcFromStyle,'NOTES')
  Select 'NOTES'
  Scatter Memo Memvar
  m.Key =Style.CstyMajor
  m.cdesc = "Notes For Style Number : " + Style.CstyMajor
  gfAppend('IN NOTES',.T.)
  =gfAdd_Info('NOTES')
  gfReplace()
  gfTableUpdate()
  gfCloseTable('NOTES')
Endif
Select(lcAlis)

*:**************************************************************************
*:* Name        : lfCSTPRICE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/02/2007
*:* Purpose     : Calling the CSTPRICE gps custom screen
*:***************************************************************************
*:* Called from : ICSTYLE.SCX
*:***************************************************************************
*T20070821.0033 TMI
Function lfCSTPRICE
Local lnCntBar
lnCntBar = Cntbar('_lPopOpt')+1
Define Bar lnCntBar Of _lPopOpt Prompt '\<Enter Customer Prices' Skip For ;
  _SCREEN.ActiveForm.Parent.ActiveMode <> 'E' .Or. ;
  _SCREEN.ActiveForm.Parent.llAllColors .Or. ;
  _SCREEN.ActiveForm.Parent.llAllScales

On Selection Bar lnCntBar Of _lPopOpt Do lfOpnCstPrice

*-- end of lfCSTPRICE.

*:**************************************************************************
*:* Name        : lfOpnCstPrice
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/02/2007
*:* Purpose     : Calling the cstprice screen
*:***************************************************************************
Function lfOpnCstPrice

*- Create a temp File to hold more then 10 sizes. [Begin]
lcTempSizs = gfTempName()
Create Table (Oariaapplication.workdir+lcTempSizs);
  ( cSequnc C(6) , ;
  cFit1 C(5),cFit2 C(5),cFit3 C(5),cFit4 C(5),cFit5 C(5),cFit6 C(5),cFit7 C(5),cFit8 C(5),cFit9 C(5),cFit10 C(5),;
  cSize1 C(15), cSize2 C(15), cSize3 C(15), cSize4 C(15), cSize5 C(15) ,;
  cSize6 C(15), cSize7 C(15), cSize8 C(15), cSize9 C(15), cSize10 C(15),;
  nPrice1 N(6,2), nPrice2 N(6,2), nPrice3 N(6,2), nPrice4 N(6,2), nPrice5 N(6,2) ,;
  nPrice6 N(6,2), nPrice7 N(6,2), nPrice8 N(6,2), nPrice9 N(6,2), nPrice10 N(6,2),;
  nComm1 N(6,2), nComm2 N(6,2), nComm3 N(6,2), nComm4 N(6,2), nComm5  N(6,2), ;
  nComm6 N(6,2), nComm7 N(6,2), nComm8 N(6,2), nComm9 N(6,2), nComm10 N(6,2))

lcTmpSzFit = gfTempName()
Create Table (Oariaapplication.workdir+lcTmpSzFit);
  ( Scale C(3),CDIM1 C(5),;
  Price1 N(7,2), Price2 N(7,2), Price3 N(7,2), Price4 N(7,2), Price5 N(7,2), Price6 N(7,2), Price7 N(7,2), Price8 N(7,2),;
  oldPrice1 N(7,2), oldPrice2 N(7,2), oldPrice3 N(7,2), oldPrice4 N(7,2), oldPrice5 N(7,2), oldPrice6 N(7,2), oldPrice7 N(7,2), oldPrice8 N(7,2))
Index On Scale Tag &lcTmpSzFit
lcSizSeq = '1'

*- open needed tables
=lfOpenTbls()

*-- Get Sizes
=lfSizes()

*- Define variables hold color len and position
Private lnClrPos,lnClrLen
Store 0  To lnClrPos,lnClrLen
*T20071119.0006 TMI [Start] Get the Size Len and Pos in the STYLE.STYLE field
Store 0 To lnSizePos,lnSizeLen
*T20071119.0006 TMI [End  ]
=lfGetClrD()

lnStyleWid  = _Screen.ActiveForm.Parent.lnStyleWid
lnColorWid1 = _Screen.ActiveForm.Parent.lnColorWid1  && ( = lnClrLen )

lcSepart = _Screen.ActiveForm.Parent.lcSepart
lcMajor  = Padr(_Screen.ActiveForm.Parent.ariaform1.kbStyleMajor.Keytextbox.Value,lnStyleWid)
lcNonMjr = Padr(_Screen.ActiveForm.Parent.ariaform1.kbNonMajor.Keytextbox.Value,lnColorWid1)

Push Key

*- open the cstprice screen
Do Form (Oariaapplication.ScreenHome+'\IC\CSTPRICE.SCX') With _Screen.ActiveForm.Parent

Pop Key

*:**************************************************************************
*:* Name        : lfSizes
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/14/2007
*:* Purpose     : Call the Customer Prices screen.
*:***************************************************************************
Function lfSizes

Local lnAlias
lnAlias = Select(0)

lnCont = 1
lnScaleLen = _Screen.ActiveForm.Parent.lnScaleLen
lcScale = Alltrim(_Screen.ActiveForm.pgfStyleInfo.Page1.kbScale.Keytextbox.Value)
lcScale = Padr(lcScale,lnScaleLen )

Select Scale
If gfSeek('S'+lcScale)
  Select (lcTempSizs)
  Append Blank
  Replace &lcTempSizs..cSequnc With lcSizSeq

  Select Scale
  =Seek('S'+lcScale)
  Scan Rest While Type+Scale+prepak = 'S' + lcScale
    If lnCont = 11
      Select (lcTempSizs)
      Append Blank
      lnCont = 1
      lcSizSeq = Ltrim(Str(Val(lcSizSeq) + 1))
    Endif

    lcCnt  = Allt(Str(Scale.Cnt))
    lcCont = Allt(Str(lnCont))
    lcsZ&lcCont = Allt(Allt(Scale.SZ1) + '-' + Scale.SZ&lcCnt)
    Replace &lcTempSizs..cSize&lcCont With lcsZ&lcCont ;
      &lcTempSizs..cFit&lcCont  With Scale.CDIM1
    lnCont = lnCont + 1

    *-- Add a line for each line in the scale file
    Insert Into &lcTmpSzFit ( Scale, CDIM1) Values ( Scale.Scale, Scale.CDIM1 )

  Endscan
Endif

Select (lnAlias)

*:**************************************************************************
*:* Name        : lfvPrCode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/07/2007
*:* Purpose     : Valid function for price codes
*:***************************************************************************
*:* Called from : user definable fields for price codes in customer screen
*:***************************************************************************
Function lfvPrCode
Parameters llReturn,lcFldNo
Local lnSlct
lnSlct = Select()

llReturn = .T.
Private lcPCode,lnIndx,lnI,lcI

If Type('laOgFxFlt') = 'C' And !Empty(laOgFxFlt)
  && When OK is pressed on OG , the array laOgFxFlt is released
  && so ignor running this code at OG-Ok pressed,Use it only in normal case when focus is changed of fields

  lnIndx = Asubscript(laOgFxFlt,Ascan(laOgFxFlt,'PRICCODE'+lcFldNo),1)
  lcPCode = laOgFxFlt[lnIndx,6]
  If !Empty(lcPCode)

    *-- Open files if not opened
    If !Used('CSTPRICH')
      =gfOpenTable(Oariaapplication.DataDir+'CSTPRICH','CSTPRICH','SH')
      Select CSTPRICH
      =gfSeek('')
    Endif

    *-- Check that this is a valid price code
    If !Seek(lcPCode,'CSTPRICH')
      laBrowArr = ''            && empty the array used in browse
      =lfPCodBrow(@lcPCode)
    Endif

    If !Empty(lcPCode)
      *-- Check that this code is not repeated
      For lnI = 1 To 15
        If lnI <> Iif(Empty(lcFldNo),1,Val(lcFldNo))
          lcI = Iif(lnI=1,'',Padl(lnI,2,'0'))
          lnIndx = Asubscript(laOgFxFlt,Ascan(laOgFxFlt,'PRICCODE'+lcI),1)
          If !Empty(laOgFxFlt[lnIndx,6]) .And. lcPCode = laOgFxFlt[lnIndx,6]
            =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              'This value is already entered for Price Code '+Allt(Str(lnI))+'.')
            lcPCode = ''
            Exit
          Endif

        Endif
      Endfor
    Endif

    *-- Update the user definable field
    lnIndx = Asubscript(laOgFxFlt,Ascan(laOgFxFlt,'PRICCODE'+lcFldNo),1)
    loOgScroll.laOgFxFlt[lnIndx,6] = lcPCode
  Endif

Endif

Select (lnSlct)
Return llReturn
*-- end of lfvPrCode.

*:**************************************************************************
*:* Name        : lfPCodBrow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/07/2007
*:* Purpose     : Valid function for price codes
*:***************************************************************************
*:* Called from : user definable fields for price codes in customer screen
*:***************************************************************************
Function lfPCodBrow
Parameters lcPCode
Local lnSlct
lnSlct = Select(0)

Do Case
Case !Empty(lcPCode) .And. Seek(lcPCode,'CSTPRICH')
  Return
Otherwise

  Select CSTPRICH
  Locate

  Dimension laTemp[1]
  laTemp = ''

  llStyle = .F.
  lcFile_Ttl = 'Price Codes'
  *T20071119.0006 TMI [Start]
  *lcBrFields = "Priccode:H='Price Code':R , ccurrcod:H='Currency Code':R"
  lcBrFields = "Priccode:H='Price Code':R , ccurrcod:H='Currency Code':R, Style:R:18"
  *T20071119.0006 TMI [End  ]
  =gfBrows([''],'PRICCODE,CCURRCOD','laTemp',lcFile_Ttl)
  lcPCode = laTemp[1]

Endcase
Select(lnSlct)

*:**************************************************************************
*:* Name        : lfUPPRCLNS
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/14/2007
*:* Purpose     : Update the prices if called from packs or template screens
*:***************************************************************************
Function lfUPPRCLNS
Local lnSlct
lnSlct = Select (0)


llSoldOut = gfGetMemVar('M_CMP2SOLD')
lcDateTyp = gfGetMemVar('M_CHKPRIAG')

*- Open needed table if not opened
=lfOpenTbls()

lcPricCursr = gfTempName()
lcRecno = gfTempName()

lcDetailFile = loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.DetailFile
Select &lcDetailFile
Select Recno() As Recno From (Dbf(lcDetailFile)) Where Empty(CSTSZPRICE)Into Cursor &lcRecno

Store 0  To lnClrPos,lnClrLen
=lfGetClrD()

Local llRejected
Select &lcRecno
Locate
Scan
  Select &lcDetailFile
  Goto (&lcRecno..Recno)
  llRejected = .F.
  If !lfChkAccSt(.T.,ACCOUNT,Store) &&check that the style.style is located correctly
    llRejected = .T.
    Delete
    Loop
  Endif
  =Iif(lcDateTyp<>'N',lfCodPrice(.T.),'')
Endscan

*- Update hdr file
If Reccount(lcRecno)>0  && if no records , nothing have been done

  =lfUpdHdr()

  If llRejected
    =gfModalGen('INM00000B00000','','','','Styles are reserved to other customers/stores will be removed.')
  Endif

Endif

Use In &lcRecno

Select (lnSlct)
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
Function lfCodPrice
Parameters llFrmMain
*llFrmMain : if .t. this means that the function is called from within another function in this main file , not as a separate triggerLOCAL lnSlct
lnSlct = Select()

*-if called from tamplete screen then exit, the lines came from this place will be updated using the "UPPRCLNS" trigger
If Type('loFormSet.AriaForm1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value')='U'
  Return
Endif

If !llFrmMain
  lcDateTyp = gfGetMemVar('M_CHKPRIAG')
  If lcDateTyp = 'N'
    Return
  Endif

  llSoldOut = gfGetMemVar('M_CMP2SOLD')

  *- Open needed table if not opened
  =lfOpenTbls()

  lcPricCursr = gfTempName()

  Store 0  To lnClrPos,lnClrLen
  =lfGetClrD()
Endif


lcDetailFile = loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.DetailFile
Select &lcDetailFile
If llSoldOut And !Empty(Style.SOLDOUT)
  Replace &lcDetailFile..Complete With Style.SOLDOUT
  loFormSet.ariaform1.Ariapageframe1.Page1.txtComplete.Text1.Value = Style.SOLDOUT
Endif

*B608450,1 TMI [Start] make the check on "lcDetailFile..TOTQTY" be separate, based on the variable "llTxtQtyCall"
*                           to be defined
*IF &lcDetailFile..LCONTRACT  .OR. ;
&lcDetailFile..TOTQTY = 0 .OR. ;
!EMPTY(&lcDetailFile..CSTSZPRICE)
If &lcDetailFile..LCONTRACT  .Or. ;
    !Empty(&lcDetailFile..CSTSZPRICE)
  Return
Endif
If Type('llTxtQtyCall')<>'L'
  If &lcDetailFile..TOTQTY = 0
    Return
  Endif
Endif
*B608450,1 TMI [End  ]

*- Get the date to check the valid code price date when compared with
* This is based on a setup option
Do Case
Case lcDateTyp = 'E'   && Entered
  ldChkDate = loFormSet.ariaform1.Ariapageframe1.Page1.txtEntered.Text1.Value

Case lcDateTyp = 'S'   && Start
  ldChkDate = loFormSet.ariaform1.Ariapageframe1.Page1.txtStart.Text1.Value

Case lcDateTyp = 'C'   && Complete
  ldChkDate = loFormSet.ariaform1.Ariapageframe1.Page1.txtComplete.Text1.Value

Endcase

Local lcPricCode
*-- loop here on sizes to gather them into price-groups

*- Get the price code
lcCurrCode = loFormSet.ariaform1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value
lcPricCode = lfPRICCODE()

*- Get the currency code
If Seek(lcPricCode+lcCurrCode+&lcDetailFile..Style,'CSTPRICE')
  Create Cursor &lcPricCursr (SIZES C(8),Gros_Price N(12,2),COMMDV N(12,2),;
    QTY1 N(6),QTY2 N(6),QTY3 N(6),QTY4 N(6),QTY5 N(6),QTY6 N(6),QTY7 N(6),QTY8 N(6),TOTQTY N(6))
  Index On Gros_Price Tag Gros_Price
  Local lnI,lcI
  *B608450,1 TMI [Start] loop only to the scale.cnt number
  *FOR lnI = 1 TO 8
  For lnI = 1 To Scale.Cnt
    *B608450,1 TMI [End  ]
    lcI = Str(lnI,1)
    *B608450,1 TMI [Start] go in this loop also when llTxtQtyCall is defined,( called from the txtQuantity field still with no actual qty added to the size fields )
    *IF !EMPTY(&lcDetailFile..QTY&lcI)
    If !Empty(&lcDetailFile..QTY&lcI) .Or. Type('llTxtQtyCall')='L'
      *B608450,1 TMI [End  ]
      *- use the special size price , if it is 0 use the special scale price ( FOR DIR03 )
      lnGrssPric = Iif(CSTPRICE.PRICE&lcI<>0,CSTPRICE.PRICE&lcI,;
        IIF(CSTPRICE.PRICEDV<>0  ,CSTPRICE.PRICEDV,  ;
        &lcDetailFile..Gros_Price))
      lnStyComm  = Iif(Style.COMMISSION,CSTPRICE.COMMDV,0)

      *- Add a line per price to the temp ordline file
      If !Seek(lnGrssPric,lcPricCursr)
        Insert Into &lcPricCursr (SIZES,Gros_Price,COMMDV,QTY&lcI,TOTQTY) Values (lcI,lnGrssPric,lnStyComm,&lcDetailFile..QTY&lcI,&lcDetailFile..QTY&lcI)
      Else
        Select &lcPricCursr
        Replace QTY&lcI With &lcDetailFile..QTY&lcI ;
          TOTQTY  With TOTQTY + QTY&lcI       ;
          SIZES   With Alltrim(SIZES)+lcI
      Endif
    Endif
  Endfor


  *B608450,1 TMI [Start] if more than one price then make rest updates later
  If Reccount(lcPricCursr)>1 .And. Type('llTxtQtyCall')='L'
    Return
  Endif
  *B608450,1 TMI [End  ]

  lcRep1 = loFormSet.ariaform1.Ariapageframe1.Page1.keySalesRep1.Keytextbox.Value
  lcRep2 = loFormSet.ariaform1.Ariapageframe1.Page1.keySalesRep2.Keytextbox.Value

  Local lcOrdHdr
  lcOrdHdr = loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.Headerfile

  *- save the current line data to memo variables and Delete it.
  lnLnCnt = 1
  Select &lcDetailFile
  Scatter Memvar Memo Fields Except QTY*

  Select &lcPricCursr
  Scan
    Scatter Fields QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,Gros_Price Memvar
    Select &lcDetailFile

    && if only one price group then just update the line , otherwise add extra lines for groups other than the first one
    If lnLnCnt>1
      Append Blank
      Select &lcOrdHdr
      Replace LASTLINE With LASTLINE + 1
      m.LINENO = &lcOrdHdr..LASTLINE
      Select &lcDetailFile
    Endif

    Gather Memvar Memo
    Replace PRICE  With Gros_Price*(100-DISC_PCNT)/100 ;
      COMM1  With Iif(!Empty(lcRep1),&lcPricCursr..COMMDV,COMM1) ;
      COMM2  With Iif(!Empty(lcRep2),&lcPricCursr..COMMDV,COMM2) ;
      COWNER  With &lcPricCursr..SIZES ;
      BOOK1   With QTY1 ;
      BOOK2   With QTY2 ;
      BOOK3   With QTY3 ;
      BOOK4   With QTY4 ;
      BOOK5   With QTY5 ;
      BOOK6   With QTY6 ;
      BOOK7   With QTY7 ;
      BOOK8   With QTY8 ;
      TOTBOOK With BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8 ;
      CSTSZPRICE With &lcPricCursr..SIZES
    lnLnCnt = lnLnCnt + 1

  Endscan

  *- Release the temp cursor
  Use In &lcPricCursr

  *- update the header only if this function is called from the trigger
  =Iif(llFrmMain,'',lfUpdHdr())

Endif

Select (lnSlct)
Return
*-- end of lfCodPrice.

*:**************************************************************************
*:* Name        : lfUpdHdr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/14/2007
*:* Purpose     : Update the lcOrdHdr file
*:***************************************************************************
Function lfUpdHdr

Local lnLastLine,lnOpenQty,lnOpenPrice,lnBookQty,lnBookAmt
Select &lcDetailFile
Locate
Calculate Sum(TOTQTY),Sum(TOTQTY*PRICE),Sum(TOTBOOK),Sum(TOTBOOK*PRICE) To lnOpenQty,lnOpenPrice,lnBookQty,lnBookAmt
Go Bottom
Select (loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.Headerfile)
Replace Open     With lnOpenQty  ;
  OPENAMT  With lnOpenPrice ;
  BOOK     With lnBookQty ;
  BOOKAMT  With lnBookAmt

*-- end of lfUpdHdr.

*:**************************************************************************
*:* Name        : lfPRICCODE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/02/2003
*:* Purpose     : get Customer Price code
*:***************************************************************************
Function lfPRICCODE
Local lcSlct,lcPricCode,lcSvKey
lcSlct = Select()
lcPricCode = ''

Select CUSTOMER
lcAccount = loFormSet.ariaform1.keyAccount.Keytextbox.Value

lcSvKey = Evaluate(Key())
=gfSeek('M'+lcAccount,'CUSTOMER')

Local lnI,lcI,lcNoDtPCod

*-- Loop through all price codes
*-   pick the first price code with suitable date,
*-   if no one, pick the first one with no valid dates , otherwise return chr(255)
lcNoDtPCod = Chr(255)

*- Download needed lines to seek in from cstprice and cstprich sql files
Select CSTPRICE
=gfsetOrder('CSTYCODE')
=gfSeek(&lcDetailFile..Style,'CSTPRICE')
=gfsetOrder('CSTPRICE')

Private lcStyClr
lcStyClr = Padr(Substr(&lcDetailFile..Style,1,lnClrPos+lnClrLen-1),19)
Select CSTPRICH
=gfsetOrder('STYLE')
=gfSeek(lcStyClr,'CSTPRICH')
=gfsetOrder('CSTPRICH')

For lnI = 1 To 15
  lcI = Iif(lnI = 1 , '' , Padl(lnI,2,'0') )

  If !Empty(CUSTOMER.PRICCODE&lcI).And.Seek(CUSTOMER.PRICCODE&lcI+lcCurrCode+&lcDetailFile..Style,'CSTPRICE') ;
      .And.Seek(CUSTOMER.PRICCODE&lcI+lcCurrCode+lcStyClr,'CSTPRICH')
    If Empty(CSTPRICH.DVLDPRTO)
      *- Get no valid date price code
      lcNoDtPCod = Upper(CUSTOMER.PRICCODE&lcI)
    Else
      *- Compare valid  prices for Banach based on setup ( Entered, Start or Complete Dates)
      If Between(ldChkDate,CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)
        Exit
      Endif

    Endif
  Endif
Endfor

lcPricCode = Iif(lnI < 16 , Upper(CUSTOMER.PRICCODE&lcI) , lcNoDtPCod )

*- restore customer record
=gfSeek(lcSvKey,'CUSTOMER')

Select (lcSlct)
Return lcPricCode
*-- end of lfPRICCODE.

*:**************************************************************************
*:* Name        : lfWRNSLDEN
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/11/2007
*:* Purpose     : Warn of Sold Out Dates
*:***************************************************************************
*:* Called from : soord.scx
*:***************************************************************************
Function lfWRNSLDEN
Local llValidate

*T20071119.0006 TMI [Start] only check if sold out date is not empty
If Empty(Style.SOLDOUT)
  Return
Endif
*T20071119.0006 TMI [End  ]

llValidate = .T.
llWRNSLDEN = gfGetMemVar('M_WRNSLDEN')
If llWRNSLDEN
  ldEntered = loFormSet.ariaform1.Ariapageframe1.Page1.txtEntered.Text1.Value
  If  ldEntered > Style.SOLDOUT
    llValidate = gfModalGen('INM00000B34012','','','',"Order Entered Date is greater than the Style Sold Out Date") = 1
  Endif
Endif
Return llValidate
*-- end of lfWRNSLDEN.

*:**************************************************************************
*:* Name        : lfSHWCSTNT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/11/2007
*:* Purpose     : Show Customer Order Notes at Sales Order Entry ?
*:***************************************************************************
Function lfSHOWNTS
Local lnSlct,loNotePad
lnSlct = Select(0)
If CUSTOMER.LSHWORDNT
  loNotePad = Createobject('NOTEPAD',loFormSet.ariaform1)
  =loNotePad.Do('A',loFormSet.ariaform1.keyAccount.Keytextbox.Value)
Endif
Select (lnSlct)
*-- end of lfSHWCSTNT.

*:**************************************************************************
*:* Name        : lfVCHKCUST
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/22/2004
*:* Purpose     : Valid funciton for the custom field "STYLE.CCHKCUSTMR" and "STYLE.CCHKSTORE"
*:***************************************************************************
Function lfVCHKCUST
Parameters llReturn,lcType
Local lnSlct,lnAcc,lnSto
lnSlct = Select(0)
llReturn = .T.

If Type('laOgFxFlt') = 'C' And !Empty(laOgFxFlt)
  && When OK is pressed on OG , the array laOgFxFlt is released
  && so ignor running this code at OG-Ok pressed,Use it only in normal case when focus is changed of fields
  If !Used('CUSTOMER')
    =gfOpenTable(Oariaapplication.DataDir+'CUSTOMER','CUSTOMER','SH')
  Endif

  lnAcc = Asubscript(laOgFxFlt,Ascan(laOgFxFlt,'CCHKCUSTMR'),1)
  lcAcc = Padr(laOgFxFlt[lnAcc,6],5)
  lnSto = Asubscript(laOgFxFlt,Ascan(laOgFxFlt,'CCHKSTORE'),1)
  lcSto = Padr(laOgFxFlt[lnSto,6],8)
  Do Case
  Case lcType = 'M'

    If !Empty(lcAcc)

      If !gfSeek('M'+lcAcc,'CUSTOMER')
        Do CUSBROWM With lcAcc
        lcSto = ''
      Else
        If !gfSeek('S'+lcAcc+lcSto,'CUSTOMER')
          lcSto = ''
        Endif
      Endif

    Else
      lcSto = ''
    Endif

  Case lcType = 'S'

    If Empty(lcAcc)
      lcSto = ''
    Else

      If !Empty(lcSto)
        If !gfSeek('S'+lcAcc+lcSto,'CUSTOMER')
          xStore   = lcSto
          If !CUSBROWS(lcAcc,.T.)
            Store Space(8) To xStore
          Endif
          lcSto = xStore
        Endif
      Endif

    Endif

  Endcase
  loOgScroll.laOgFxFlt[lnAcc,6] = lcAcc
  loOgScroll.laOgFxFlt[lnSto,6] = lcSto

Endif

Select (lnSlct)
Return llReturn
*-- end of lfVCHKCUST.

*:**************************************************************************
*:* Name        : lfChkAccSt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/13/2007
*:* Purpose     : Check if the style customer saved in &lcTmpSty..CCHKACCNT match
*               : selected Account/Store in sales order
*:***************************************************************************
Function lfChkAccSt
Parameters llFrmMain,lcAccount,lcStore
Local lcAcc,lcSto
lcAcc = Iif(llFrmMain,lcAccount,loFormSet.ariaform1.keyAccount.Keytextbox.Value)
lcSto = Iif(llFrmMain,lcStore  ,loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.keyStore.Keytextbox.Value)

If !Empty(Style.CCHKCUSTMR)
  If lcAcc <> Style.CCHKCUSTMR
    If !llFrmMain
      =gfModalGen('INM00000B00000','','','','This style is reserved for customer '+Style.CCHKCUSTMR+' and you cannot proceed.')
    Endif
    Return .F.
  Else
    If !Empty(Style.CCHKSTORE) .And. ;
        lcSto <> Style.CCHKSTORE
      If !llFrmMain
        =gfModalGen('INM00000B00000','','','','This style is reserved for customer/store '+Style.CCHKCUSTMR+'/'+Alltrim(Style.CCHKSTORE)+' and you cannot proceed.')
      Endif
      Return .F.
    Endif
  Endif
Endif
*-- end of lfChkAccSto.

*:**************************************************************************
*:* Name        : lfCHKACCS2
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/14/2007
*:* Purpose     : Valid fn called if customer changes store after enetring the style
*:***************************************************************************
Function lfCHKACCS2
Local llValid , lOb
If loFormSet.ariaform1.Ariapageframe1.Page2.grdEditLines.Style.keyStyle.Value <> ' '
  llValid = lfChkAccSt()
  If !llValid
    lOb = loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.keyStore.Keytextbox
    lOb.Value = lOb.OLDValue
  Endif
Endif
*-- end of lfCHKACCS2.

*:**************************************************************************
*:* Name        : lfCSTSZPRI
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/12/2007
*:* Purpose     : return .F. in the When method for txtQty1,...,8 fields in Soord
*               : screen if this line has entered with custom price
*:***************************************************************************
Function lfCSTSZPRI
Local lcI,lcDetFl
lcI = Right(loCSTSZPRI.Name,1)
lcDetFl = loCSTSZPRI.Parent.Parent.DetailFile
If !Empty(&lcDetFl..CSTSZPRICE) .And. !lcI $ &lcDetFl..CSTSZPRICE
  Return .F.
Endif
*-- end of lfCSTSZPRI.

*:**************************************************************************
*:* Name        : lfOpenTbls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/01/2007
*:* Purpose     : open needed tables
*:***************************************************************************
Function lfOpenTbls

If !Used('SycCurr')
  =gfOpenTable(Oariaapplication.SysPath+'SycCurr',Oariaapplication.SysPath+'cCurrCode','SH')
Endif
If !Used('CSTPRICH')
  =gfOpenTable(Oariaapplication.DataDir+'CSTPRICH','CSTPRICH','SH')
Endif
If !Used('CSTPRICE')
  =gfOpenTable(Oariaapplication.DataDir+'CSTPRICE','CSTPRICE','SH')
Endif
If !Used('STYLE')
  =gfOpenTable(Oariaapplication.DataDir+'STYLE','STYLE','SH')
Endif
If !Used('SCALE')
  =gfOpenTable(Oariaapplication.DataDir+'SCALE','SCALE','SH')
Endif
*-- end of lfOpenTbls.

*:**************************************************************************
*:* Name        : lfGetClrD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2002
*:* Purpose     : Get Color Position and Color length
*:***************************************************************************
Function lfGetClrD
Declare laItemSeg[1]
Private lnCount &&Tmi 07/15/2002
lcOldSelect=Select()

=gfItemMask(@laItemSeg)
For lnCount = 1 To Alen(laItemSeg,1)
  Do Case
  Case laItemSeg[lnCount,1]='C'
    lnClrLen = Len(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = Allt(laItemSeg[lnCount,6])

  Case laItemSeg[lnCount,1]='S'
    lnSizeLen = Len(laItemSeg[lnCount,3])
    lnSizePos = laItemSeg[lnCount,4]
  Endcase
Endfor

Select(lcOldSelect)
*--end function lfGetClrD


*:**************************************************************************
*:* Name        : lfCLSFLS
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/21/2007
*:* Purpose     : Trigger to close CSTPRICE, CSTPRICH tables if opened
*:***************************************************************************
*:* Called from : icstyle.desctroy method
*:***************************************************************************
Function lfCLSFLS
If Used('CSTPRICE')
  Use In CSTPRICE
Endif
If Used('CSTPRICH')
  Use In CSTPRICH
Endif
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
Function lfADDAPREG
If loFormSet.ActiveMode= "A"
  lnAlias = Select()

  lcregno =gfSequence('NAPREGIST')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"This Invoice has been allocated - Register No: "+lcregno)
  Select APINVHDR
  Replace NAPREGIST With lcregno
  Select (lnAlias)
Endif
*C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[Start]
*:**************************************************************************
*:* Name        : lfGTPOEXRAT
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/27/2008
*:* Purpose     : Get PO Price Ex. Rate
*:***************************************************************************
Function lfGTPOEXRAT
llGetPORate = gfGetMemVar('M_POPRDEF')
lcPoln = loFormSet.lcPosln


*B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[Start]
If loFormSet.lcPType $ 'S' And Used('SHPRLFLD') .And. gfGetMemVar('M_APRVSHIP')
  If gfSeek(Evaluate(lcPoln +'.SHIPNO')+Evaluate(lcPoln +'.PO')+Str(Evaluate(lcPoln +'.LINENO'),6),'SHPRLFLD')
    Return
  Endif
Endif
*B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]



If llGetPORate  And (&lcPoln..Cstytype = 'P' And &lcPoln..CBUSDOCU = 'P')
  If !Used('POSHDR_R')
    gfOpenTable('POSHDR','POSHDR','SH','POSHDR_R')
  Endif
  If gfSeek(&lcPoln..CBUSDOCU+&lcPoln..Cstytype+&lcPoln..PO,'POSHDR_R')
    lnCrRt1 = POSHDR_R.npricerat

    Store '/' To lcExSign, lcUntSin
    lcExSign = gfGetExSin(@lcUntSin, POSHDR_R.cPriceCur)
    loFormSet.laECost[1] = Evaluate(loFormSet.lcTmpLine+'.nFLanCost1') &lcExSign lnCrRt1 &lcUntSin POSHDR_R.nCurrUnit
  Endif

Endif
*C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[End]
*! C200969,1 MMT 03/27/2007 Add Quick Order entry screen to Sales order screen[Start]
*:**************************************************************************
*:* Name        : lfQKORDENT
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : Add Menu of Quick Order Entry Screen(C200969,1)
*:***************************************************************************
Function lfQKORDENT
Define Bar loFormSet.NextOptionBar Of _INQURYPOP Prompt 'Quic\<k Order Entry'  Skip For  gfFormIsActive(&lcHostFormName) And ((Type('_screen.ActiveForm.ariapageframe1') = "O" And  _Screen.ActiveForm.Ariapageframe1.ActivePage<>2) .Or. (_Screen.ActiveForm.Parent.ActiveMode = 'V'))
On Selection Bar loFormSet.NextOptionBar Of _INQURYPOP  Do lfOpnQkScr

loFormSet.NextOptionBar = loFormSet.NextOptionBar + 1

*:**************************************************************************
*:* Name        : lfOpnQkScr
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : open Quick Order Entry Screen(C200969,1)
*:***************************************************************************
Function lfOpnQkScr
lcAlias = Select()

Private laSize,laTot,;
  lcScrTtl,lcDet_Ttl,lnClrLen,lnClrPos,lcClrSpr,lcTempCur,lcTempNote,lcAlias,lnMrk ,;
  lcQkWin0,lcQkWin1,lcQkWin2,lcQkWin3,lcQkWin4,lcOldValue,lcSepart,lcItemPct,laExtSz,lcOrd,llEdit,;
  llChang,laStyClQty,llDifPrice,llShw1,laOldVal,lcBrowFlds

Do Form  (Oariaapplication.ScreenHome+'\SO\SoQKORD.scx') With _Screen.ActiveForm.Parent
Select (lcAlias)


*:**************************************************************************
*:* Name        : lfInitQkOrd
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : Init Function of Quick Order Entry Screen(C200969,1)
*:***************************************************************************
Function lfInitQkOrd
Parameters loBranchFormSet
With loBranchFormSet.ariaform1
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
Endwith
loBranchFormSet.lcTempCur = gfTempName()
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
Function lfvQStyle
Parameters loBranchFormSet,loParentForm


If loParentForm.DataSessionId <> Set("Datasession")
  Set DataSession To loParentForm.DataSessionId
Endif


lcStyleVal = loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value
Private lcAlias,lcGetSty,lcStat,lnIncrmnt,lnClrs,lnCount,lnJ,lcSeekSty,laSum,laSavStat,lcStyle
*--- Select
lcAlias = Alias()
loBranchFormSet.lnMajorLen = Len(loParentForm.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.stylemajorpicture)

lcStyleVal = Padr(Alltrim(lcStyleVal),loBranchFormSet.lnMajorLen)
If (!Empty(lcStyleVal) And ('?' $ lcStyleVal  .Or. !Seek(lcStyleVal ,'STYLE'))) Or ;
    loBranchFormSet.ariaform1.kbstymaj.selectedfrombrowse

  *-* HES && Link Style to price table if the setup option related to this feature set to YES
  If Ascan(loParentForm.laEvntTrig,Padr('FLTPRCSTL',10),1,Alen(loParentForm.laEvntTrig,1),1) > 0
    loParentForm.mDoTrigger(Padr('FLTPRCSTL',10))
  Endif
  *-* HES

  Select Style
  *B608998,1 TMI [start] use the ARIABROW instead to not use the CSTYLE index
  *! B609542,1 SAB 03/02/2011 Sales order Quick order entry error when style filter in place [T20110222.0005][Start]
  lcGetSty = Padr(gfStyBrw("M" , lcStyleVal  , "" , .F.),loBranchFormSet.lnMajorLen)
  *! B609542,1 SAB 03/02/2011 Sales order Quick order entry error when style filter in place [T20110222.0005][End]
  *! B609542,2 MMT 03/16/2011 Sales order Quick order entry error when style filter in place [Start]
  *!*	  LOCATE
  *!*	  lcFile_Ttl = "Styles"
  *!*	  *! B609449,1 MMT 11/04/2010 SO quick order entry screen style browser displays style color[Start]
  *!*	  *lcBrFields = "style :30 ,cdivision :H='Division' :20"
  *!*	  lcBrFields = "CSTYMAJOR :30 :H ='Style',cdivision :H='Division' :20"
  *!*	  =SEEK(lcStyleVal ,'STYLE')
  *!*	  *! B609449,1 MMT 11/04/2010 SO quick order entry screen style browser displays style color[End]
  *!*	  DIMENSION laTempData[1]
  *!*	  laTempData[1] = ''
  *!*	  *! B609449,1 MMT 11/04/2010 SO quick order entry screen style browser displays style color[Start]
  *!*	  *=AriaBrow('','Styles',.F.,.F.,.F.,.F.,'',.T.,'STYLE','laTempData')
  *!*	  lcOrderStyle = ORDER()
  *!*	  SET ORDER TO cStyle
  *!*	  =AriaBrow('','Styles',.F.,.F.,.F.,.F.,'',.T.,'CSTYMAJOR','laTempData')
  *!*	  SELECT STYLE
  *!*	  IF !EMPTY(lcOrderStyle)
  *!*	    SET ORDER TO (lcOrderStyle)
  *!*	  ENDIF
  *!*	  *! B609449,1 MMT 11/04/2010 SO quick order entry screen style browser displays style color[End]
  *!*	  lcGetSty = PADR(laTempData[1],loBranchFormSet.lnMajorLen)
  *! B609542,2 MMT 03/16/2011 Sales order Quick order entry error when style filter in place [End]
  *B608998,1 TMI [end  ] use the ARIABROW instead to not use the CSTYLE index
  loBranchFormSet.ariaform1.kbstymaj.selectedfrombrowse  = .F.
  If Empty(lcGetSty)
    loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value = ''
    Return .F.
  Else
    loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value = lcGetSty
  Endif
Endif


lcOrdHdr = loParentForm.OFORMENVIRONMENT.lcOrdHdr
lcOrdLine = loParentForm.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.DetailFile
=Seek('M'+&lcOrdHdr..ACCOUNT,'Customer')
lcpricelevel = Iif(Inlist(CUSTOMER.PriceLvl,"A","B","C","Q"),CUSTOMER.PriceLvl,'A')
m.Style   = Alltrim(loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value)
* B608998,1 TMI [start] use the STYLE index instead of CSTYLE
*IF SEEK(PADR(m.Style,19),'Style','Cstyle')
If Seek(Padr(m.Style,loBranchFormSet.lnMajorLen),'Style','style')
  * B608998,1 TMI [end  ] use the STYLE index instead of CSTYLE
  llValidStyle = .T.
  If !Seek('S'+Style.Scale,'Scale')
    =gfModalGen('TRM32017B00000','ALERT')
    llValidStyle = .F.
  Endif

  *IF llValidStyle  AND (Style.Status='X')
  *   = gfModalGen('TRM32018B00000','ALERT')
  *   llValidStyle = .F.
  *ENDIF
  *! B610103,1 MMT 09/26/2012 Ignore OnHold Style/Color in SO Quich Order entry screen[Start]
  *!*	  IF llValidStyle  AND (Style.Status='H')
  *!*	    = gfModalGen('QRM32019B32003','ALERT')
  *!*	     llValidStyle = .F.
  *!*	  ENDIF
  *! B610103,1 MMT 09/26/2012 Ignore OnHold Style/Color in SO Quich Order entry screen[END]

  If llValidStyle  And (Style.cDivision <> &lcOrdHdr..cDivision)
    = gfModalGen('TRM32020B00000','ALERT','division '+Alltrim(gfCodDes(&lcOrdHdr..cDivision,'CDIVISION')))
    llValidStyle = .F.
  Endif


  If llValidStyle  And (!Empty(Style.Start) .And. Style.Start > &lcOrdHdr..Complete)
    =gfModalGen('QRM32021B32003','ALERT','start|'+Dtoc(Style.Start))
    llValidStyle = .F.
  Endif

  If llValidStyle  And !Empty(Style.SOLDOUT) .And. Style.SOLDOUT < &lcOrdHdr..Start
    If  gfModalGen('QRM40010B40001','ALERT','sold out|'+Dtoc(Style.SOLDOUT)) <> 1
      llValidStyle = .F.
    Endif
  Endif
  *! B609905,1 MMT 05/06/2012 Quick order entry does not validate style season[T20120330.0007][Start]
  *!*	  IF llValidStyle AND  ASCAN(loParentForm.laEvntTrig , PADR('STYLEVALID',10)) <> 0
  *!*	    IF ALLTRIM(gfGetMemVar("M_SEASNCHK")) = "Y"  AND  (ALLTRIM(&lcOrdHDr..Season )<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season <> &lcOrdHDr..Season)
  *! B609905,2 MMT 05/10/2012 Validate Season on color level [T20120330.0007][Start]
  *!*	  IF llValidStyle
  *!*	    IF (ALLTRIM(&lcOrdHDr..Season )<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season <> &lcOrdHDr..Season)
  *!*	  *! B609905,1 MMT 05/06/2012 Quick order entry does not validate style season[T20120330.0007][END]
  *!*	      gfModalGen('TRM32020B00000','ALERT','season '+ ALLTRIM(gfCodDes(&lcOrdHDr..Season,'SEASON')))
  *!*	      llValidStyle = .F.
  *!*	    ENDIF
  *!*	  ENDIF
  *! B609905,3 MMT 05/15/2012 Change Season Validation message [Start]
  llValidSeason = .F.
  Select Style
  =Seek(Padr(m.Style,loBranchFormSet.lnMajorLen),'Style','style')
  Scan Rest While Style = Padr(m.Style,loBranchFormSet.lnMajorLen)
    If (Alltrim(&lcOrdHdr..Season )='*' Or  Trim(Style.Season)='Y' Or  Style.Season = &lcOrdHdr..Season)
      llValidSeason = .T.
    Endif
  Endscan
  =Seek(Padr(m.Style,loBranchFormSet.lnMajorLen),'Style','style')
  If !llValidSeason
    =gfModalGen('TRM32020B00000','ALERT','season '+ Alltrim(gfCodDes(&lcOrdHdr..Season,'SEASON')))
    loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value = ''
    Return .F.
  Endif

  *! B610103,1 MMT 09/26/2012 Ignore OnHold Style/Color in SO Quich Order entry screen[Start]
  llValidHold= .T.
  Select Style
  =Seek(Padr(m.Style,loBranchFormSet.lnMajorLen),'Style','style')
  Scan Rest While Style = Padr(m.Style,loBranchFormSet.lnMajorLen)
    If (Style.Status <> 'H')
      llValidHold = .F.
    Endif
  Endscan
  =Seek(Padr(m.Style,loBranchFormSet.lnMajorLen),'Style','style')
  If llValidHold
    = gfModalGen('TRM32019B00000','ALERT')
    llValidStyle = .F.
  Endif
  *! B610103,1 MMT 09/26/2012 Ignore OnHold Style/Color in SO Quich Order entry screen[End]
  *! B609905,3 MMT 05/15/2012 Change Season Validation message [END]
  llShowMessage = .T.
  *! B609905,2 MMT 05/10/2012 Validate Season on color level [T20120330.0007][end]
  If !llValidStyle
    loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value = ''
    Return .F.
  Else
    loBranchFormSet.ariaform1.spnComm.Enabled = (!Empty(loParentForm.ariaform1.Ariapageframe1.Page1.keySalesRep1.Keytextbox.Value) And Style.COMMISSION)
    loBranchFormSet.ariaform1.spnDisc.Enabled = .T.
    loBranchFormSet.ariaform1.cmdNote.Enabled = .T.
    loBranchFormSet.ariaform1.txtChkQty.Enabled = .T.
    loBranchFormSet.ariaform1.dtpCompDate.Enabled = loParentForm.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.Editlinecompletedate
    loBranchFormSet.ariaform1.dtpCompDate.Value = loParentForm.ariaform1.Ariapageframe1.Page1.txtComplete.Value
  Endif

  *! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{Start}
  llUsePriceCtn = .F.
  *! C201302,3 MMT 01/24/2011 Display Price message in Quick So entry based on trigger{Start}
  If Ascan(loParentForm.laEvntTrig,Padr('STYCTNPRIC',10),1,Alen(loParentForm.laEvntTrig,1),1) > 0
    *! C201302,3 MMT 01/24/2011 Display Price message in Quick So entry based on trigger{End}
    If gfModalGen('QRM00000B00042','','','',"Use Carton Price") = 1
      llUsePriceCtn = .T.
    Endif
    *! C201302,3 MMT 01/24/2011 Display Price message in Quick So entry based on trigger{Start}
  Endif
  *! C201302,3 MMT 01/24/2011 Display Price message in Quick So entry based on trigger{End}
  *! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{End}

  Store 0   To m.Gros_Price, m.PRICE, m.DISC_PCNT
  Store .F. To m.LCONTRACT
  Store '' To m.Store
  m.lContract = loParentForm.lcOrdType <> 'C'
  
  lfStyPrice(ALLTRIM(m.Style),m.Store,'m.Gros_Price','m.Price','m.Disc_Pcnt','m.lContract',0)

  IF m.Gros_Price < 0
    RETURN .F.
  ENDIF

  =Seek(Allt(m.Style),'STYLE')

  With loBranchFormSet.ariaform1
    .txtGrsPrc.Value = m.Gros_Price
    .txtNetPri.Value = m.Gros_Price
    .txtStydesc.Value = Style.Desc1
    .spnComm.Value = Iif(Style.COMMISSION,loParentForm.ariaform1.Ariapageframe1.Page1.spnComm1.Value,0)
  Endwith


  *-- Get Style discount percent and Calculate net price
  IF !m.lContract
        *-- get the cDiscCode From stydye in every case
        lcDiscCode  = IIF(SEEK(m.Style+&lcOrdHDr..CWARECODE+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
        loBranchFormSet.AriaForm1.spnDisc.Value = 0
        IF !EMPTY(ALLTRIM(lcDiscCode))
  *!*        *-- Get the disecound related filed to now which
  *!*        *-- type whole Sale Or Retail sale Or Both.
         DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
          STORE '' To lcDisType , ldstartDte ,ldEndDate
          *-- Array to get the Discount affect for DecCode.
          laDisType[1,1]  = 'CCOSTAFECT'
          laDisType[1,2]  = 'lcDisType'
  *!*        *-- Array to get the start date For DescCode.
          lastartDte[1,1] = 'START'
         lastartDte[1,2] = 'ldstartDte'
          *-- Array to get the end date For DescCode.
          laEndDate[1,1]  = 'DENDATE'
          laEndDate[1,2]  = 'ldEndDate'
          = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
          = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
          = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
          lnDisc_Pcnt = 0
         IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(&lcOrdHDr..Entered,ldstartDte,ldEndDate)
           lnDisc_Pcnt = m.Disc_Pcnt
          =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
            loBranchFormSet.AriaForm1.spnDisc.Value  = lnDisc_Pcnt
          ENDIF
      ENDIF
        loBranchFormSet.AriaForm1.txtNetPri.Value   = loBranchFormSet.AriaForm1.txtGrsPrc.Value*(100-loBranchFormSet.AriaForm1.spnDisc.Value )/100
    ENDIF




  Private laPrices,lcStyClr
  Select Style
  =Seek(Allt(m.Style),'STYLE')
  loBranchFormSet.ariaform1.cmdClear.Enabled = !Empty(m.Style)
  laSize = ''
  loBranchFormSet.laSize = ''
  loBranchFormSet.laExtSz = ''

  =Seek(Allt(m.Style),'STYLE')
  loBranchFormSet.lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
  m.SCALE = Style.Scale
  Select Scale,Cnt From Scale Where Type+Scale='S'+Substr(m.SCALE,1,loBranchFormSet.lnScaleLen) Order By 1 Into Array laExtSz
  Select Scale
  lcStyScl = Substr(Style.Scale,1,loBranchFormSet.lnScaleLen)
  =Seek('S'+lcStyScl ,'SCALE')
  If !Empty(Scale.CDIM1)
    loBranchFormSet.llMultiDime = .T.
  Endif
  lnMaxSz = 0
  If loBranchFormSet.llMultiDime
    lnMaxSz = 0
    lnLastCnt = 0
    Select Scale
    =Seek('S'+lcStyScl ,'SCALE')
    Do While Type+Scale+prepak = 'S' + lcStyScl .And. !Eof('SCALE')
      *! B609736,1 MMT 11/20/2011 SO - Quick Order entry not pkcking up Styprice values[Start]
      *lnRecord = RECNO('Scale')
      lcFullKeyExp = Evaluate(Key())
      *! B609736,1 MMT 11/20/2011 SO - Quick Order entry not pkcking up Styprice values[END]
      lcFit = Scale.CDIM1
      lnMaxSz = 0
      Scan Rest While Type+Scale+prepak = 'S' + lcStyScl For ;
          CDIM1 = lcFit
        For lnI = 1 To Scale.Cnt
          lnMaxSz = lnMaxSz  + 1
        Endfor
      Endscan
      If lnMaxSz > lnLastCnt
        lnLastCnt = lnMaxSz
      Endif
      lnMaxSz = Iif(lnMaxSz >= lnLastCnt,lnMaxSz,  lnLastCnt)
      *! B609736,1 MMT 11/20/2011 SO - Quick Order entry not pkcking up Styprice values[Start]
      *IF BETWEEN(lnRecord+1,1,RECCOUNT('Scale'))
      *  GO lnRecord+1 IN Scale
      If Seek(lcFullKeyExp,'Scale')
        Try
          Skip In Scale
        Catch
        Endtry
        *! B609736,1 MMT 11/20/2011 SO - Quick Order entry not pkcking up Styprice values[END]
      Endif
    Enddo
  Endif
  loBranchFormSet.lnMaxSz = lnMaxSz



  lnIncrmnt = 0
  For lnCount = 1 To Alen(laExtSz,1)
    lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
  Endfor
  If lnIncrmnt > 16
    *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'This Style has scale with More than 16 Sizes.')
  Endif


  If Seek(loParentForm.lcOrdType+&lcOrdHdr..Order+&lcOrdHdr..Store+Alltrim(m.Style),(lcOrdLine),'ORDLINST')
    *--This Style has entred on this order
    lnResp = gfModalGen('QRM00000B02011',.F.,.F.,.F.,'This style has already been entred on this order '+;
      +'- do you wish to create new order lines ?')
    If lnResp = 2
      =lfvClear(loBranchFormSet)
      Return
    Endif
  Endif


  lnIncrmnt = 0


  For lnCount = 1 To Alen(laExtSz,1)
    =Seek('S'+laExtSz[lnCount,1],'SCALE')
    For lnJ = 1 To laExtSz[lnCount,2]
      lcZ = Str(lnJ,1)
      Dimension laSize[lnIncrmnt+lnJ]
      laSize[lnIncrmnt+lnJ] = Alltrim(Scale.SZ&lcZ)
    Endfor
    lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
  Endfor

  *--Get Colors
  Store 0  To lnClrPos,lnClrLen
  Private lnClrPos,lnClrLen
  Store 0 To lnSizePos,lnSizeLen
  lfGetClrD()
  loBranchFormSet.lnClrLen = lnClrLen
  loBranchFormSet.lnClrPos = lnClrPos

  lfCrtTempFiles(loBranchFormSet)

  *--Create color array
  Select Distinct Substr(Style.Style,lnClrPos,lnClrLen) From Style ;
    WHERE Style = Padr(m.Style,loBranchFormSet.lnMajorLen) ;
    INTO Array laClr


  Store '' To lcStySp,lcSclSp
  Declare laItemSeg[1]
  =gfItemMask(@laItemSeg)
  For lnJ = 1 To Alen(laItemSeg,1)
    If laItemSeg[lnJ,1] = 'F'
      lcStySp = laItemSeg[lnJ,6]
      loBranchFormSet.lcStySp = lcStySp
      Loop
    Endif
    If laItemSeg[lnJ,1] = 'C'
      lcSclSp = laItemSeg[lnJ,6]
      loBranchFormSet.lcSclSp = lcSclSp
      Loop
    Endif
  Endfor


  Store 0   To m.Gros_Price, m.PRICE, m.DISC_PCNT
  Store .F. To m.LCONTRACT
  Store '' To m.Store
  lcpricelevel = Iif(Inlist(lcpricelevel ,'A','B','C'),lcpricelevel ,'A')
  *--Add a line for each color
  Private lnMaxInd
  *! B609736,1 MMT 11/28/2011 So - quick order entry screen not showing all colours of a style[T20110822.0008][Start]
  *lnMaxInd = IIF(ALEN(laClr,1)>=10,10,ALEN(laClr,1))
  lnMaxInd = Alen(laClr,1)
  *! B609736,1 MMT 11/28/2011 So - quick order entry screen not showing all colours of a style[T20110822.0008][END]
  For lnClrs = 1 To lnMaxInd
    Select (loBranchFormSet.lcTempCur)
    Scatter Memvar Blank
    m.STYMAJOR = Padr(m.Style,loBranchFormSet.lnMajorLen)
    m.COLOR = laClr[lnClrs]
    *! B610000,1 HIA 07/12/2012 Color description, not displayed compelteded [T20120703.0019][Begin]
    *M.COLORDSC = PADR(gfCodDes(PADR(laClr[lnClrs],lnClrLen) , 'COLOR'),20)
    m.COLORDSC = Padr(gfCodDes(Padr(laClr[lnClrs],lnClrLen) , 'COLOR'),30)
    *! B610000,1 HIA 07/12/2012 Color description, not displayed compelteded [T20120703.0019][End]
    lnIncrmnt = 0
    Dimension laSzQty[ALEN(laSize,1)]
    Store 0 To laSzQty
    lnCounSz = 1
    =Seek(Allt(m.Style),'STYLE')
    lcStyScl = Substr(Style.Scale,1,loBranchFormSet.lnScaleLen)
    Dimension laFits[1]
    laFits = ''
    Select Scale
    =Seek('S'+lcStyScl)
    lcScaleCurr = ''
    Scan Rest  While Type+Scale+prepak = 'S' + lcStyScl For !Eof('SCALE')
      lnRecord = Recno('Scale')
      lcFit = Scale.CDIM1

      If Ascan(laFits ,lcFit,1) > 0
        Loop
      Endif


      *! B609905,2 MMT 05/10/2012 Validate Season on color level [T20120330.0007][Start]
      *=gfseek(M.STYMAJOR+'-'+M.COLOR+SCALE.SCALE,'STYLE')
      lcScaleFld = Scale.Scale
      =gfSeek(Padr(M.STYMAJOR+lcStySp +Padr(M.COLOR,lnClrLen)+Iif(loBranchFormSet.llExtended,Alltrim(lcSclSp) +lcScaleFld ,''),19),'STYLE')
      If (Alltrim(&lcOrdHdr..Season )<>'*' And Trim(Style.Season)<>'Y' And Style.Season <> &lcOrdHdr..Season)
        If llShowMessage
          *! B609905,2 MMT 05/10/2012 Validate Season on color level [T20120330.0007][Start]
          *gfModalGen('TRM32020B00000','ALERT','season '+ ALLTRIM(gfCodDes(&lcOrdHDr..Season,'SEASON')))
          =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
            'Some Colours on this style do not match the season code on the order - Only valid colours will be available for selection.')
          *! B609905,2 MMT 05/10/2012 Validate Season on color level [T20120330.0007][END]
          llShowMessage = .F.
        Endif
        Loop
      Endif
      *! B609905,2 MMT 05/10/2012 Validate Season on color level [T20120330.0007][end]

      *!B608962,1 08/06/2009 AHS quick order entry screen accepts cancelled styles [start]
      *-- If style status is cancelled.
      If Style.Status='X'
        *-This is a canceled style. Not allowed to enter here, Cannot proceed!
        Wait Window Nowait 'The style '+Style.Style+' is cancelled'
        Loop
      Endif
      *!B608962,1 08/06/2009 AHS quick order entry screen accepts cancelled styles [end]
      *! B610103,1 MMT 09/26/2012 Ignore OnHold Style/Color in SO Quich Order entry screen[Start]
      If (Style.Status = 'H')
        Loop
      Endif
      *! B610103,1 MMT 09/26/2012 Ignore OnHold Style/Color in SO Quich Order entry screen[End]

      If !Empty(Scale.CDIM1)
        m.FitDesc = Scale.CDIM1
      Else
        m.FitDesc = Scale.CSCL_DESC
      Endif

      *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
      m.SCALE = Scale.Scale
      *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[End]

      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
      *lnCounter = 1
      lnSzCounter = 1
      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

      For lnCont = 1 To Iif(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz,Alen(laSize,1))
        lcCont  = Alltrim(Str(lnCont))
        Store '' To m.SZ&lcCont
      Endfor
      If Empty(laFits[1])
        laFits[1] = lcFit
      Else
        Dimension laFits[ALEN(laFits,1)+1]
        laFits[ALEN(laFits,1)] =lcFit
      Endif
      lnCounting = 1
      *! B609736,1 MMT 11/20/2011 SO - Quick Order entry not pkcking up Styprice values[Start]
      =Seek( 'S' + lcStyScl)
      *! B609736,1 MMT 11/20/2011 SO - Quick Order entry not pkcking up Styprice values[End]
      Scan Rest While Type+Scale+prepak = 'S' + lcStyScl For  CDIM1 = lcFit
        lcCounting = Alltrim(Str(lnCounting))
        lnCounting = lnCounting  + 1
        If Alltrim(lcScaleCurr) <> Alltrim(Scale.Scale)
          *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]
          *lcSeekSty = M.STYMAJOR+lcStySp +PADR(M.COLOR,lnClrLen)+IIF(loBranchFormSet.llExtended,lcSclSp +Scale.Scale,'')
          lcSeekSty = Padr(M.STYMAJOR+lcStySp +Padr(M.COLOR,lnClrLen)+Iif(loBranchFormSet.llExtended,Alltrim(lcSclSp) +Scale.Scale,''),19)
          *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]

          If loParentForm.OFORMENVIRONMENT.laSetups[5,2]='Y' .And. !Seek(lcSeekSty+loParentForm.ariaform1.Ariapageframe1.Page1.cboWarehouse.Value+Space(10),'StyDye')
            lcMsg = 'Style ' + lcSeekSty + ' is not assigned to location ' + loParentForm.ariaform1.Ariapageframe1.Page1.cboWarehouse.Value
            lnResp = gfModalGen('QRM00000B40002',.F.,.F.,.F.,lcMsg)
            If lnResp = 1
              Do gpAdStyWar With lcSeekSty,Space(10),loParentForm.ariaform1.Ariapageframe1.Page1.cboWarehouse.Value
            Else
              Return .F.
            Endif
          Endif
          Store 0   To m.Gros_Price, m.PRICE, m.DISC_PCNT
          Store .F. To m.LCONTRACT
          Store '' To m.Store


          =Seek(Alltrim(lcSeekSty),'Style')
		  lfStyPrice(ALLTRIM(lcSeekSty),m.Store,'m.Gros_Price','m.Price','m.Disc_Pcnt','m.lContract',0)
*!*	          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
*!*	          lcPricCode = ''
 	          llPricCode = .F.
*!*	          ldChkDate = loParentForm.ariaform1.Ariapageframe1.Page1.txtEntered.Text1.Value
*!*	          If gfSeek(lfQKPRICCODE()+loParentForm.ariaform1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value+lcSeekSty,'CSTPRICE')
*!*	            m.Comm = Iif(CSTPRICE.COMMDV=0,Iif(Style.COMMISSION,loBranchFormSet.ariaform1.spnComm.Value,0),CSTPRICE.COMMDV)
*!*	            m.Gros_Price = CSTPRICE.PRICEDV
*!*	            llPricCode = .T.
*!*	          Else
*!*	            *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
*!*	            *! B609736,1 MMT 11/20/2011 SO - Quick Order entry not pkcking up Styprice values[Start]
*!*	            *IF loParentForm.oFormEnvironment.laSetups[17,1]='Y' .AND. loParentForm.ariaform1.ariapageframe1.page1.keyCurrency.Keytextbox.value <> oAriaApplication.BaseCurrency
*!*	            If loParentForm.OFORMENVIRONMENT.laSetups[17,2] .And. loParentForm.ariaform1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value <> Oariaapplication.BaseCurrency
*!*	              If !Used('STYPRICE')
*!*	                =gfOpenTable('STYPRICE','STYPRICE')
*!*	              Endif
*!*	              *! B609736,1 MMT 11/20/2011 SO - Quick Order entry not pkcking up Styprice values[END]
*!*	              If gfSeek(lcSeekSty+loParentForm.ariaform1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value ,'STYPRICE')
*!*	                m.Gros_Price = STYPRICE.PRICE&lcpricelevel
*!*	                *! B609736,1 MMT 11/20/2011 SO - Quick Order entry not pkcking up Styprice values[Start]
*!*	              Else
*!*	                m.Gros_Price = Style.PRICE&lcpricelevel
*!*	                *! B609736,1 MMT 11/20/2011 SO - Quick Order entry not pkcking up Styprice values[End]
*!*	              Endif
*!*	            Else
*!*	              m.Gros_Price = Style.PRICE&lcpricelevel
*!*	            Endif

*!*	            *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
*!*	          Endif
*!*	          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

          If Ascan(laExtSz,Scale.Scale,1) > 0
            lnCounting  = Asubscript(laExtSz,Ascan(laExtSz,Scale.Scale,1),1)
            lcCounting  = Alltrim(Str(lnCounting))
          Endif
          
          *! C201930 SARA.O 01/31/2017 DCC Price uplift by Store [Start]
          If Ascan(loParentForm.laEvntTrig,Padr('UPDPRUPLFQ',10),1,Alen(loParentForm.laEvntTrig,1),1) > 0
    		 loParentForm.mDoTrigger(Padr('UPDPRUPLFQ',10))
 		  ENDIF
 		  *! C201930 SARA.O 01/31/2017 DCC Price uplift by Store [End]

          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
          *!*	          m.Pric&lcCounting  =  m.Gros_Price
          *!*	          m.NtPri&lcCounting  =  m.Gros_Price
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]

          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
          If !llPricCode
            *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

            m.Comm =  Iif(Style.COMMISSION,loBranchFormSet.ariaform1.spnComm.Value,0)

            *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
          Endif
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
        Endif
        lcScaleCurr = Scale.Scale
        m.FitDesc = lcFit

*!*	        *! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{Start}
*!*	        If llUsePriceCtn
*!*	          llPricCtnCode = .F.
*!*	          If !Empty(CUSTOMER.cpricctn1) And gfSeek(CUSTOMER.cpricctn1+loParentForm.ariaform1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value+lcSeekSty,'CSTPRICE') And ;
*!*	              IIF(gfSeek(CUSTOMER.cpricctn1+loParentForm.ariaform1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value+Padr(Substr(lcSeekSty,1,lnClrPos+lnClrLen-1),19) ,'CSTPRICH'),;
*!*	              EMPTY(CSTPRICH.DVLDPRTO) Or (!Empty(CSTPRICH.DVLDPRTO) And Between(ldChkDate ,CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)),.T.)
*!*	            * m.Comm = IIF(CSTPRICE.COMMDV=0,IIF(Style.Commission,loBranchFormSet.AriaForm1.SpnComm.Value,0),CSTPRICE.COMMDV)
*!*	            llPricCtnCode= .T.
*!*	          Else
*!*	            If !Empty(CUSTOMER.cpricctn2) And gfSeek(CUSTOMER.cpricctn2+loParentForm.ariaform1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value+lcSeekSty,'CSTPRICE')  And ;
*!*	                IIF(gfSeek(CUSTOMER.cpricctn2+loParentForm.ariaform1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value+Padr(Substr(lcSeekSty,1,lnClrPos+lnClrLen-1),19) ,'CSTPRICH'),;
*!*	                EMPTY(CSTPRICH.DVLDPRTO) Or (!Empty(CSTPRICH.DVLDPRTO) And Between(ldChkDate ,CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)),.T.)
*!*	              llPricCtnCode = .T.
*!*	              *  m.Comm = IIF(CSTPRICE.COMMDV=0,IIF(Style.Commission,loBranchFormSet.AriaForm1.SpnComm.Value,0),CSTPRICE.COMMDV)
*!*	            Endif
*!*	          Endif
*!*	        Endif
*!*	        *! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{END}

        For lnI = 1 To Scale.Cnt

          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
          *lcCounter = ALLTRIM(STR(lnCounter))
          lcCounter = Alltrim(Str(lnSzCounter))
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

          lcI = Str(lnI,1)

          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
          *lnCounter = lnCounter + 1
          lnSzCounter= lnSzCounter+ 1
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

          m.SZ&lcCounter = Scale.SZ&lcI

          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
          *! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{Start}
          *!*	          IF llPricCode AND m.Gros_Price = 0
          *!*	            m.Pric&lcCounter = CSTPRICE.Price&lcI
          *!*	            m.NtPri&lcCounter = CSTPRICE.Price&lcI
*!*	          If (llPricCode And m.Gros_Price = 0) Or (llUsePriceCtn  And llPricCtnCode)
*!*	            m.Pric&lcCounter = Iif(CSTPRICE.PRICE&lcI = 0,CSTPRICE.PRICEDV,CSTPRICE.PRICE&lcI)
*!*	            m.NtPri&lcCounter = Iif(CSTPRICE.PRICE&lcI = 0,CSTPRICE.PRICEDV,CSTPRICE.PRICE&lcI)
*!*	            *! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{End}
*!*	          Else
*!*	            *! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{Start}
*!*	            If llUsePriceCtn   And !llPricCtnCode
*!*	              m.Pric&lcCounter = gfGetprice(Padr(lcSeekSty,19),'B',0,loParentForm.ariaform1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value)
*!*	              m.NtPri&lcCounter = m.Pric&lcCounter
*!*	            Else
              *! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{End}
              m.Pric&lcCounter = m.Gros_Price
              m.NtPri&lcCounter = m.Gros_Price
              *! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{Start}
*!*	            Endif
*!*	            *! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{End}
*!*	          Endif
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]


          Select(loBranchFormSet.lcTempCur)
          *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
          *IF !SEEK(PADR(M.STYMAJOR,19)+M.COLOR+padR(m.FitDesc,10),loBranchFormSet.lcTempCur)
          If !Seek(Padr(M.STYMAJOR,19)+M.COLOR+Padr(m.SCALE,3),loBranchFormSet.lcTempCur)
            *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[End]
            Append Blank
            Select(loBranchFormSet.lcTmpStk)
            Append Blank
          Endif
          Select(loBranchFormSet.lcTempCur)
          Gather Memo Memvar
          Select(loBranchFormSet.lcTmpStk)
          Gather Memo Memvar
        Endfor
      Endscan
      If Between(lnRecord,1,Reccount('Scale'))
        Go lnRecord In Scale
      Endif
    Endscan


  Endfor

  =Seek(Padr(m.Style,loBranchFormSet.lnMajorLen),'STYLE')

  Select (loBranchFormSet.lcTempCur)
  Go Top

  *!B608962,1 AHS 08/06/2009 Quick order entry screen accepts cancelled styles [start]
  If Eof()
    Return .F.
  Endif
  *!B608962,1 AHS 08/06/2009 Quick order entry screen accepts cancelled styles [start]

  Scatter Memo Memvar
  Append Blank
  Gather Memo Memvar
  Replace STYMAJOR With Chr(255),;
    COLOR    With '',;
    COLORDSC With 'Totals' ,;
    FitDesc  With ''

  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
  Replace Scale With ''
  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]

  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
  *FOR lnCount = 1 TO ALEN(laExtSz,1)

  *B608983,1 TMI [START] Change the loop length based on llMultiDime variable
  *FOR lnCount = 1 TO ALEN(laSize,1)
  For lnCount = 1 To Iif(loBranchFormSet.llMultiDime,lnMaxSz,Alen(laSize,1) )
    *B608983,1 TMI [END  ] Change the loop length based on llMultiDime variable

    *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
    lcI = Alltrim(Str(lnCount))
    Replace Pric&lcI  With 0,;
      NtPri&lcI With 0
  Endfor


  For lnC = 1 To Iif(loBranchFormSet.llMultiDime,lnMaxSz,Alen(laSize,1) )
    lcC = Alltrim(Str(lnC))
    Select (loBranchFormSet.lcTempCur)
    Sum (QTY&lcC) To lnTotal For STYMAJOR <> Chr(255)
    Go Bottom
    Replace QTY&lcC With lnTotal
  Endfor
  Select (loBranchFormSet.lcTempCur)
  Locate

  With loBranchFormSet.ariaform1
    .kbstymaj.Enabled = .F.
    .spnDisc.Enabled = .T.
  Endwith


  Acopy(laSize,loBranchFormSet.laSize)
  Acopy(laExtSz,loBranchFormSet.laExtSz)

  lfGetStock(loBranchFormSet)
  lfAddCntrSrc(loBranchFormSet)
  lfCalcTot(loBranchFormSet)

  Select (loBranchFormSet.lcTempCur)
  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
  *  SET RELATION TO StyMajor+color+FitDesc INTO (loBranchFormSet.lcTmpStk) ADDITIVE
  Set Relation To STYMAJOR+Color+Scale Into (loBranchFormSet.lcTmpStk) Additive
  *! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[End]

  loBranchFormSet.ariaform1.grdStk.Refresh
  loBranchFormSet.ariaform1.grdLines.Refresh
  loBranchFormSet.ariaform1.grdLines.AfterRowColChange
Endif
*-- end of lfvQStyle.
*:**************************************************************************
*:* Name        : lfvClear
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : Clear Screen(C200969,1)
*:***************************************************************************
Function lfvClear
Parameters loBranchFormSet,llFromScreen
If llFromScreen
  loBranchFormSet.ariaform1.grdLines.RecordSource = ''
  loBranchFormSet.ariaform1.grdStk.RecordSource = ''
Endif
loBranchFormSet.llMultiDime = .F.
If Used(loBranchFormSet.lcTempCur)
  Select(loBranchFormSet.lcTempCur)
  Zap
Endif
If Used(loBranchFormSet.lcTmpStk)
  Select (loBranchFormSet.lcTmpStk)
  Zap
Endif
With loBranchFormSet.ariaform1
  .kbstymaj.Enabled = .T.
  .kbstymaj.Keytextbox.Value = ''
  .spnComm.Enabled = .F.
  .spnComm.Value = 0
  .spnDisc.Enabled = .F.
  .spnDisc.Value = 0
  .txtChkQty.Enabled= .F.
  .txtChkQty.Value = 0
  .txtGrsPrc.Enabled= .F.
  .txtGrsPrc.Value = 0
  .txtNetPri.Enabled = .F.
  .txtNetPri.Value = 0
  .dtpCompDate.Enabled= .F.
  .cmdSave.Enabled= .F.
  .cmdNote.Enabled = .F.
  .grdLines.RecordSource = ''
  .grdStk.RecordSource = ''
  .txtStydesc.Value = ''
Endwith
Dimension loBranchFormSet.laSize[1],loBranchFormSet.laExtSz[1]


*:**************************************************************************
*:* Name        : lfStyPrice
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : Get Style Price (C200969,1)
*:***************************************************************************
Function lfStyPrice
Lparameters lcStyle,lcStore,lcGros_Price,lcPrice,lcDisc_Pcnt,lcContract, lnTotQty
*-- Get Price from contract
&lcContract = &lcOrdHdr..cOrdType<>'C' .And. lfGetContractPrice(lcStyle,lcStore,lcGros_Price,lcPrice,lcDisc_Pcnt)
If !&lcContract
  *-- Get Style Gross Price
  &lcGros_Price = gfGetprice(lcStyle,lcpricelevel ,lnTotQty,&lcOrdHdr..cCurrCode)
  If &lcGros_Price = 0
    If lcpricelevel  = 'Q'
      Do Case
      Case Style.nAtQtyC > 0 And lnTotQty > Style.nAtQtyC
        lcLevel = 'C'
      Case Style.nAtQtyB > 0 And lnTotQty > Style.nAtQtyB
        lcLevel = 'B'
      Otherwise
        lcLevel = 'A'
      Endcase
    Else
      lcLevel=Iif(Inlist(lcpricelevel ,'A','B','C'),lcpricelevel ,'A')
    Endif
    &lcGros_Price = lfCheckPri(lcStyle,lcLevel,&lcOrdHdr..cCurrCode)
  Endif

  *-- get style discount
  lcDiscCode  = Iif(Seek(lcStyle+&lcOrdHdr..cWareCode+Space(10),'StyDye'),StyDye.cDiscCode,'')
  m.DISC_PCNT = 0
  If !Empty(lcDiscCode)
    *-- Get discount type, start date, end date, and discount percent
    Declare laDisRltFld[4,2]
    Store '' To lcDisType
    Store {} To ldstartDte, ldEndDate
    Store 0  To lnDisc_Pcnt
    laDisRltFld[1,1] = 'CCOSTAFECT'
    laDisRltFld[1,2] = 'lcDisType'
    laDisRltFld[2,1] = 'START'
    laDisRltFld[2,2] = 'ldstartDte'
    laDisRltFld[3,1] = 'DENDATE'
    laDisRltFld[3,2] = 'ldEndDate'
    laDisRltFld[4,1] = 'DISCPCNT'
    laDisRltFld[4,2] = 'lnDisc_Pcnt'
    =gfRltFld(lcDiscCode, @laDisRltFld, 'CDISCCODE')
    If Alltrim(lcDisType) <> 'R' .And. Between(&lcOrdHdr..Entered,ldstartDte,ldEndDate)
      &lcDisc_Pcnt = lnDisc_Pcnt
    Endif
  Endif
  &lcPrice = &lcGros_Price*(100-&lcDisc_Pcnt)/100
Endif

*:**************************************************************************
*:* Name        : lfGetContractPrice
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/27/2008
*:* Purpose     : Get Contract Style Price (C200969,1)
*:***************************************************************************
Function lfGetContractPrice
Parameters lcStyle,lcStore,lcGPrice,lcNPrice,lcDiscount
Private lnAlias,lnGPrice,lnNPrice,lnDiscount,llContract
lnAlias = Select()

lnGPrice   = &lcGPrice
lnNPrice   = &lcNPrice
lnDiscount = &lcDiscount
llContract = .F.
Set Order To Tag ORDLINST In OrdLine
Select (loParentForm.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.ContractFile)
If Seek(&lcOrdHdr..ACCOUNT+'C')
  *-- Check open contracts for order account having same date period and currency code
  Scan Rest While ACCOUNT+cOrdType+Order = &lcOrdHdr..ACCOUNT+'C' ;
      FOR  !Inlist(Status,'X','B') .And. Between(&lcOrdHdr..Entered,Start,Complete) .And. cCurrCode=&lcOrdHdr..cCurrCode
    lcContact = Order
    Select OrdLine
    =Seek('C'+lcContact+lcStore+lcStyle)
    Locate Rest While cOrdType+Order+Store+Style+Str(Lineno,6)='C'+lcContact+lcStore+lcStyle ;
      FOR   Between(&lcOrdHdr..Entered,Start,Complete) And PRICE <> 0
    If Found()
      lnGPrice   = OrdLine.Gros_Price
      lnNPrice   = OrdLine.PRICE
      lnDiscount = OrdLine.DISC_PCNT
      llContract = .T.
      Exit
    Endif
  Endscan
Endif
If !llContract And Seek(&lcOrdHdr..ACCOUNT+'C')
  *-- Check open contracts for order account having same date period and currency code
  Scan Rest While ACCOUNT+cOrdType+Order = &lcOrdHdr..ACCOUNT+'C' ;
      FOR  !Inlist(Status,'X','B') .And. Between(&lcOrdHdr..Entered,Start,Complete) .And. cCurrCode=&lcOrdHdr..cCurrCode
    lcContact = Order
    Select OrdLine
    =Seek('C'+lcContact+Space(8)+lcStyle)
    Locate Rest While cOrdType+Order+Store+Style+Str(Lineno,6)='C'+lcContact+Space(8)+lcStyle ;
      FOR   Between(&lcOrdHdr..Entered,Start,Complete) And PRICE <> 0
    If Found()
      lnGPrice   = OrdLine.Gros_Price
      lnNPrice   = OrdLine.PRICE
      lnDiscount = OrdLine.DISC_PCNT
      llContract = .T.
      Exit
    Endif
  Endscan
Endif
Set Order To Tag OrdLine In OrdLine
Select (lnAlias)
&lcGPrice   = lnGPrice
&lcNPrice   = lnNPrice
&lcDiscount = lnDiscount
Return(llContract)
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
Function lfvqGPrice
Parameters loBranchFormSet

*-* HES && Restric Editing prices debend on User privilege
If loBranchFormSet.ariaform1.txtGrsPrc.Value <> loBranchFormSet.ariaform1.txtGrsPrc.OLDValue
  If Ascan(loBranchFormSet.laEvntTrig,Padr('EDITPRICE',10),1,Alen(loBranchFormSet.laEvntTrig,1),1) > 0
    If !loBranchFormSet.mDoTrigger(Padr('EDITPRICE',10))
      loBranchFormSet.ariaform1.txtGrsPrc.Value = loBranchFormSet.ariaform1.txtGrsPrc.OLDValue
    Endif
  Endif
Endif
*-* HES

If loBranchFormSet.ariaform1.txtGrsPrc.Value < 0
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loBranchFormSet.ariaform1.txtGrsPrc.Value = loBranchFormSet.ariaform1.txtGrsPrc.OLDValue
  Return .F.
Endif

loBranchFormSet.ariaform1.txtNetPri.Value = Round(loBranchFormSet.ariaform1.txtGrsPrc.Value *(100-loBranchFormSet.ariaform1.spnDisc.Value)/100,2)

*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
lcFPos = Alltrim(Str(loBranchFormSet.lccurrsz))
lnPosSc  = Ascan(loBranchFormSet.laSize,Alltrim(loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption))
If lnPosSc  > 0
  lnPosSc = Asubscript(loBranchFormSet.laSize,lnPosSc,1)
  *! B609513,1 MMT 01/27/2011 Fix error in SO Quick order entry screen in case of Mutli dimension scale[Start]
  If loBranchFormSet.llMultiDime
    lcTempCur =  loBranchFormSet.lcTempCur
    lnPosSc = lfGetScPos(Alltrim(loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption),&lcTempCur..Scale)
    If lnPosSc = 0
      Return
    Endif
  Endif
  *! B609513,1 MMT 01/27/2011 Fix error in SO Quick order entry screen in case of Mutli dimension scale[End]
  lcPos = Alltrim(Str(lnPosSc))
  Replace Pric&lcPos  With loBranchFormSet.ariaform1.txtGrsPrc.Value,;
    NtPri&lcPos With loBranchFormSet.ariaform1.txtNetPri.Value
Endif
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
Function lfvqPrcDisc
Parameters loBranchFormSet

*-* HES && Restric Editing prices debend on User privilege
If loBranchFormSet.ariaform1.spnDisc.Value <> loBranchFormSet.ariaform1.spnDisc.OLDValue
  If Ascan(loBranchFormSet.laEvntTrig,Padr('EDITPRICE',10),1,Alen(loBranchFormSet.laEvntTrig,1),1) > 0
    If !loBranchFormSet.mDoTrigger(Padr('EDITPRICE',10))
      loBranchFormSet.ariaform1.spnDisc.Value = loBranchFormSet.ariaform1.spnDisc.OLDValue
    Endif
  Endif
Endif
*-* HES

If loBranchFormSet.ariaform1.spnDisc.Value < 0
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loBranchFormSet.ariaform1.spnDisc.Value = loBranchFormSet.ariaform1.spnDisc.OLDValue
  Return .F.
Endif

If loBranchFormSet.ariaform1.spnDisc.Value  > 99.99
  *--Can not exceed 999
  *-- 40171 : ð cannot exceeds ð
  =gfModalGen('TRM40171B00000','DIALOG','Percent Discount|99.99')
  loBranchFormSet.ariaform1.spnDisc.Value = loBranchFormSet.ariaform1.spnDisc.OLDValue
  Return .F.
Endif
loBranchFormSet.ariaform1.txtNetPri.Value = Round(loBranchFormSet.ariaform1.txtGrsPrc.Value *(100-loBranchFormSet.ariaform1.spnDisc.Value)/100,2)

lcTempCur =  loBranchFormSet.lcTempCur
*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
*FOR lnCount = 1 TO ALEN(loBranchFormSet.laExtSz,1)
*! B609513,1 MMT 01/27/2011 Fix error in SO Quick order entry screen in case of Mutli dimension scale[Start]
*FOR lnCount = 1 TO ALEN(loBranchFormSet.laSize,1)
For lnCount = 1 To Iif(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz,Alen(loBranchFormSet.laSize,1))
  *! B609513,1 MMT 01/27/2011 Fix error in SO Quick order entry screen in case of Mutli dimension scale[End]
  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

  lcCnt = Alltrim(Str(lnCount))
  *C200969,3 MMT 05/13/2008 Let Comm and Discount applicable on all lines{Start}
  * REPLACE Discount WITH loBranchFormSet.AriaForm1.spnDisc.Value ,;
  NtPri&lcCnt WITH ROUND(Pric&lcCnt * (100 - Discount)/100 ,2),;
  comm WITH loBranchFormSet.AriaForm1.SpnComm.Value  IN  (lctempcur)
  Replace All Discount With loBranchFormSet.ariaform1.spnDisc.Value ,;
    NtPri&lcCnt With Round(Pric&lcCnt * (100 - Discount)/100 ,2),;
    COMM With loBranchFormSet.ariaform1.spnComm.Value  In  (lcTempCur)
  *C200969,3 MMT 05/13/2008 Let Comm applicable on all lines{End}
Endfor



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
Function lfvNotes
Parameters loBranchFormSet,loParForm

lnRecNum = Recno(loBranchFormSet.lcTempCur)

Do Form (Oariaapplication.ScreenHome + 'Arlnotes') With loBranchFormSet.lcTempCur ,loParForm.OFORMENVIRONMENT.ActiveMode <> 'V'
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
Function lfvqTotQty
Parameters loBranchFormSet

If Iif(Type('loBranchFormSet.AriaForm1.txtChkQty.VAlue') = "C",Val(loBranchFormSet.ariaform1.txtChkQty.Value),loBranchFormSet.ariaform1.txtChkQty.Value) < 0
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loBranchFormSet.ariaform1.txtChkQty.Value = loBranchFormSet.ariaform1.txtChkQty.OLDValue
  Return .F.
Endif
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
Function lfvqNPrice
Parameters loBranchFormSet

*-* HES && Restric Editing prices debend on User privilege
If loBranchFormSet.ariaform1.txtNetPri.Value <> loBranchFormSet.ariaform1.txtNetPri.OLDValue
  If Ascan(loBranchFormSet.laEvntTrig,Padr('EDITPRICE',10),1,Alen(loBranchFormSet.laEvntTrig,1),1) > 0
    If !loBranchFormSet.mDoTrigger(Padr('EDITPRICE',10))
      loBranchFormSet.ariaform1.txtNetPri.Value = loBranchFormSet.ariaform1.txtNetPri.OLDValue
    Endif
  Endif
Endif
*-* HES

If loBranchFormSet.ariaform1.txtNetPri.Value < 0
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loBranchFormSet.ariaform1.txtNetPri.Value  = loBranchFormSet.ariaform1.txtNetPri.OLDValue
  Return .F.
Endif

loBranchFormSet.ariaform1.txtNetPri.Value  = Iif(loBranchFormSet.ariaform1.txtNetPri.Value>loBranchFormSet.ariaform1.txtGrsPrc.Value,loBranchFormSet.ariaform1.txtGrsPrc.Value,loBranchFormSet.ariaform1.txtNetPri.Value)

loBranchFormSet.ariaform1.spnDisc.Value  = Iif(loBranchFormSet.ariaform1.txtGrsPrc.Value=0,0,100-loBranchFormSet.ariaform1.txtNetPri.Value*100/loBranchFormSet.ariaform1.txtGrsPrc.Value)

*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
lcFPos = Alltrim(Str(loBranchFormSet.lccurrsz))
lnPosSc  = Ascan(loBranchFormSet.laSize,Alltrim(loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption))
If lnPosSc  > 0
  lnPosSc = Asubscript(loBranchFormSet.laSize,lnPosSc,1)
  *! B609513,1 MMT 01/27/2011 Fix error in SO Quick order entry screen in case of Mutli dimension scale[Start]
  If loBranchFormSet.llMultiDime
    lcTempCur =  loBranchFormSet.lcTempCur
    lnPosSc = lfGetScPos(Alltrim(loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption),&lcTempCur..Scale)
    If lnPosSc = 0
      Return
    Endif
  Endif
  *! B609513,1 MMT 01/27/2011 Fix error in SO Quick order entry screen in case of Mutli dimension scale[End]

  lcPos = Alltrim(Str(lnPosSc))
  Replace Pric&lcPos  With loBranchFormSet.ariaform1.txtGrsPrc.Value,;
    NtPri&lcPos With loBranchFormSet.ariaform1.txtNetPri.Value
Endif
*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

*-- end of lfvqNPrice.
*:**************************************************************************
*:* Name        : lfClose
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Close Button Function(C200969,1)
*:***************************************************************************
Function lfClose
Parameters loBranchFormSet
llClose  = .F.
lcTempCur = loBranchFormSet.lcTempCur
lnRecno = Recno(lcTempCur)
Select (lcTempCur)
Locate For STYMAJOR = Chr(255)
If Found() And &lcTempCur..nTotal > 0
  llClose = (gfModalGen('QRM38093B32005','ALERT','Quantities have been entered';
    +'|The Quick Order Entry screen')=1)
Else
  If Found() And &lcTempCur..nTotal = 0
    loBranchFormSet.Release
  Endif
Endif
If llClose
  loBranchFormSet.Release
Else
  If Between(lnRecno ,1,Reccount(lcTempCur))
    Go lnRecno  In (lcTempCur)
  Endif
  Return
Endif
*:**************************************************************************
*:* Name        : lfCrtTempFiles
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Create Temp Files(C200969,1)
*:***************************************************************************
Function lfCrtTempFiles
Parameters loBranchFormSet

*! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
*  DIMENSION laTblFld[8,4]
Dimension laTblFld[9,4]
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
*! B610000,1 HIA 07/12/2012 Color description, not displayed compelteded [T20120703.0019][Begin]
*laTblFld[3,3] = 20
laTblFld[3,3] = 30
*! B610000,1 HIA 07/12/2012 Color description, not displayed compelteded [T20120703.0019][End]
laTblFld[3,4] = 0

laTblFld[4,1] = "NTOTAL"
laTblFld[4,2] = "N"
*! B609449,1 MMT 11/04/2010 SO quick order entry screen Give error when style has about 35 sizes[Start]
*laTblFld[4,3] = 6
laTblFld[4,3] = 9
*! B609449,1 MMT 11/04/2010 SO quick order entry screen Give error when style has about 35 sizes[End]
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


If loBranchFormSet.llMultiDime

  For lnI = 1 To lnMaxSz
    lcI = Alltrim(Str(lnI))
    Dimension laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'SZ'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "C"
    laTblFld[ALEN(laTblFld,1),3] = 5
    laTblFld[ALEN(laTblFld,1),4] = 0
  Endfor

  For lnI = 1 To lnMaxSz
    lcI = Alltrim(Str(lnI))
    Dimension laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'QTY'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "N"
    laTblFld[ALEN(laTblFld,1),3] = 5
    laTblFld[ALEN(laTblFld,1),4] = 0
  Endfor

Else
  For lnI = 1 To Alen(laSize,1)
    lcI = Alltrim(Str(lnI))
    Dimension laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'SZ'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "C"
    laTblFld[ALEN(laTblFld,1),3] = 5
    laTblFld[ALEN(laTblFld,1),4] = 0
  Endfor

  For lnI = 1 To Alen(laSize,1)
    lcI = Alltrim(Str(lnI))
    Dimension laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'QTY'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "N"
    *! B609449,1 MMT 11/04/2010 SO quick order entry screen Give error when style has about 35 sizes[Start]
    *laTblFld[ALEN(laTblFld,1),3] = 5
    laTblFld[ALEN(laTblFld,1),3] = 7
    *! B609449,1 MMT 11/04/2010 SO quick order entry screen Give error when style has about 35 sizes[End]
    laTblFld[ALEN(laTblFld,1),4] = 0
  Endfor
Endif

*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
*FOR lnCount = 1 TO ALEN(laExtSz,1)
*B608983,1 TMI [START] Change the loop length based on llMultiDime variable
*FOR lnCount = 1 TO ALEN(laSize,1)
For lnCount = 1 To Iif(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,Alen(laSize,1))
  *B608983,1 TMI [END  ] Change the loop length based on llMultiDime variable
  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]

  lcI = Alltrim(Str(lnCount))
  Dimension laTblFld[ALEN(laTblFld,1)+1,4]
  laTblFld[ALEN(laTblFld,1),1] = 'Pric'+lcI
  laTblFld[ALEN(laTblFld,1),2] = "N"
  laTblFld[ALEN(laTblFld,1),3] = 7
  laTblFld[ALEN(laTblFld,1),4] = 2
Endfor

*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
*FOR lnCount = 1 TO ALEN(laExtSz,1)
*B608983,1 TMI [START] Change the loop length based on llMultiDime variable
*FOR lnCount = 1 TO ALEN(laSize,1)
For lnCount = 1 To Iif(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,Alen(laSize,1))
  *B608983,1 TMI [END  ] Change the loop length based on llMultiDime variable

  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

  lcI = Alltrim(Str(lnCount))
  Dimension laTblFld[ALEN(laTblFld,1)+1,4]
  laTblFld[ALEN(laTblFld,1),1] = 'NtPri'+lcI
  laTblFld[ALEN(laTblFld,1),2] = "N"
  laTblFld[ALEN(laTblFld,1),3] = 7
  laTblFld[ALEN(laTblFld,1),4] = 2
Endfor

*! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
*=gfCrtTmp(loBranchFormSet.lctempcur ,@laTblFld,'STYMAJOR+COLOR+FitDesc',loBranchFormSet.lctempcur )
=gfCrtTmp(loBranchFormSet.lcTempCur ,@laTblFld,'STYMAJOR+COLOR+SCALE',loBranchFormSet.lcTempCur )
*! B608933,1 AHS 07/13/2009 Quick Order entry-sort order of Fits incorrect[Start]
*! B609449,1 MMT 11/04/2010 SO quick order entry screen Give error when style has about 35 sizes[Start]
*B608983,1 TMI [START] Change the loop length based on llMultiDime variable
*FOR lnI = 1 TO ALEN(laSize,1)
*!*	  FOR lnI = 1 TO IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(laSize,1))
*!*	  *B608983,1 TMI [END  ] Change the loop length based on llMultiDime variable
*!*	    lcI = ALLTRIM(STR(lnI))
*!*	    DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
*!*	    laTblFld[ALEN(laTblFld,1),1] = 'FStk'+lcI
*!*	    laTblFld[ALEN(laTblFld,1),2] = "N"
*!*	    laTblFld[ALEN(laTblFld,1),3] = 5
*!*	    laTblFld[ALEN(laTblFld,1),4] = 0
*!*	  ENDFOR

*!*	  FOR lnI = 1 TO IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(laSize,1))
*!*	    lcI = ALLTRIM(STR(lnI))
*!*	    DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
*!*	    laTblFld[ALEN(laTblFld,1),1] = 'PhStk'+lcI
*!*	    laTblFld[ALEN(laTblFld,1),2] = "N"
*!*	    laTblFld[ALEN(laTblFld,1),3] = 5
*!*	    laTblFld[ALEN(laTblFld,1),4] = 0
*!*	  ENDFOR
*!*
*!*	  FOR lnI = 1 TO IIF(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,ALEN(laSize,1))
*!*	    lcI = ALLTRIM(STR(lnI))
*!*	    DIMENSION laTblFld[ALEN(laTblFld,1)+1,4]
*!*	    laTblFld[ALEN(laTblFld,1),1] = 'PStk'+lcI
*!*	    laTblFld[ALEN(laTblFld,1),2] = "N"
*!*	    laTblFld[ALEN(laTblFld,1),3] = 5
*!*	    laTblFld[ALEN(laTblFld,1),4] = 0
*!*	  ENDFOR
*! B609449,1 MMT 11/04/2010 SO quick order entry screen Give error when style has about 35 sizes[End]
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
Function lfAddCntrSrc
Parameters loBranchFormSet
With loBranchFormSet.ariaform1.grdLines
  .ColumnCount =Iif(loBranchFormSet.llMultiDime,lnMaxSz,Alen(laSize,1))+3
  .RecordSource = loBranchFormSet.lcTempCur
  .Column1.ReadOnly = .T.
  .Column1.Header1.Caption ='Colour'
  .Column1.ControlSource  = loBranchFormSet.lcTempCur +'.COLORDSC'
  .Column2.ReadOnly = .T.
  .Column2.Header1.Caption ='Fit'
  .Column2.ControlSource  = loBranchFormSet.lcTempCur +'.FitDesc'

  lnCount = 2
  For lnT = 1 To Iif(loBranchFormSet.llMultiDime,lnMaxSz,Alen(laSize,1))
    lnCount = lnCount + 1
    lcT = Alltrim(Str(lnT))
    lcCount= Alltrim(Str(lnCount))
    .Column&lcCount..ControlSource = loBranchFormSet.lcTempCur +'.QTY'+lcT
    .Column&lcCount..Header1.Caption = Evaluate(loBranchFormSet.lcTempCur +'.SZ'+lcT)
    .Column&lcCount..Enabled = .T.
    .Column&lcCount..Format = '99999'
    .Column&lcCount..InputMask = '99999'
    *! C201550,1 RAS 01/27/2013 Aria4xp - SO - Modi Sizes Aligmnent at SoQKORD Screen to be Right [begin]
    .Column&lcCount..Header1.Alignment = 1
    *! C201550,1 RAS 01/27/2013 Aria4xp - SO - Modi Sizes Aligmnent at SoQKORD Screen to be Right [end]

    *! C201129,1 MMT 04/06/2009 Reduce width of columns[Start]
    .Column&lcCount..Width = 50
    *! C201129,1 MMT 04/06/2009 Reduce width of columns[End]

    .Column&lcCount..ReadOnly = .F.
    Bindevent(.Column&lcCount..Text1,"GotFocus",loBranchFormSet,'lfvgtstyszqty')
    Bindevent(.Column&lcCount..Text1,"LostFocus",loBranchFormSet,'lfvStySzqty')
  Endfor
  lnCount = lnCount + 1
  lcCount= Alltrim(Str(lnCount))
  .Column&lcCount..ControlSource = loBranchFormSet.lcTempCur +'.Ntotal'
  .Column&lcCount..Header1.Caption = 'Total'
  .Column&lcCount..ReadOnly = .T.
Endwith

With loBranchFormSet.ariaform1.grdStk
  .RecordSource = loBranchFormSet.lcTmpStk
  .ColumnCount =Iif(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,Alen(laSize,1))+2
  .Column1.ControlSource = loBranchFormSet.lcTmpStk +'.COLORDSC'
  .Column1.Header1.Caption = ''
  .Column1.ReadOnly = .T.
  lnCount = 1
  For lnT = 1 To Iif(loBranchFormSet.llMultiDime,lnMaxSz,Alen(laSize,1))
    lnCount = lnCount + 1
    lcT = Alltrim(Str(lnT))
    lcCount= Alltrim(Str(lnCount))
    .Column&lcCount..ControlSource = loBranchFormSet.lcTmpStk +'.QTY'+lcT
    .Column&lcCount..Header1.Caption = Evaluate(loBranchFormSet.lcTmpStk +'.SZ'+lcT)
    .Column&lcCount..ReadOnly = .T.
    *! C201550,2 RAS 01/27/2013 Aria4xp - SO - Modi Sizes Aligmnent at SoQKORD Screen to be Right [begin]
    .Column&lcCount..Header1.Alignment = 1
    *! C201550,2 RAS 01/27/2013 Aria4xp - SO - Modi Sizes Aligmnent at SoQKORD Screen to be Right [end]
  Endfor
  lnCount = lnCount + 1
  lcCount= Alltrim(Str(lnCount))
  .Column&lcCount..ControlSource = loBranchFormSet.lcTmpStk +'.Ntotal'
  .Column&lcCount..Header1.Caption = 'Total'
  .Column&lcCount..ReadOnly = .T.
Endwith

loBranchFormSet.ariaform1.grdStk.Refresh
loBranchFormSet.ariaform1.grdLines.Refresh

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
Function lfvStySzqty
Parameters loBranchFormSet,loParentForm
Private lcFPos

lcTempCur = loBranchFormSet.lcTempCur
lcFPos = Alltrim(Str(loBranchFormSet.lccurrsz))
lnCurrSz  = loBranchFormSet.ariaform1.grdLines.Column&lcFPos..ControlSource
Private lnLineTot,lnCount,lnRecno,m.AddSty,lnIncrm

Select (lcTempCur)
If STYMAJOR = Chr(255)
  Replace &lnCurrSz With loBranchFormSet.lnOldVAlue   In (lcTempCur)
  Return
Endif
If Empty(loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption)
  Replace &lnCurrSz With 0 In (lcTempCur)
  Return
Endif


If loBranchFormSet.lnOldVAlue  #  Evaluate(lnCurrSz)
  If Evaluate(lnCurrSz) < 0
    *--Can not accept negative values
    =gfModalGen('TRM42000B40011','DIALOG')

    Replace &lnCurrSz With loBranchFormSet.lnOldVAlue
    Return
  Endif
  If Evaluate(lnCurrSz) > 99999
    *--Can not exceed 999
    *-- 40171 : ð cannot exceeds ð
    =gfModalGen('TRM40171B00000','DIALOG','Size Quantity|99999.')
    Replace &lnCurrSz With loBranchFormSet.lnOldVAlue
    Return
  Endif
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

  m.AddSty = Padr(STYMAJOR,loBranchFormSet.lnMajorLen) +loBranchFormSet.lcStySp +Substr(Color,1,loBranchFormSet.lnClrLen)
  Select (lcTempCur)
  If loParentForm.OFORMENVIRONMENT.laSetups[5,2]='Y'
    lcSclPos = lfGetScale(loBranchFormSet,loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption,&lcTempCur..FitDesc)
    *-- Assign a style to a location if not Assigned befor

    *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]
    *IF !SEEK(PADR(m.AddSty+IIF(loBranchFormSet.llExtended,loBranchFormSet.lcsclsp+lcSclPos,'') ,19)+loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value+SPACE(10),'StyDye')
    If !Seek(Padr(m.AddSty+Iif(loBranchFormSet.llExtended,Alltrim(loBranchFormSet.lcSclSp)+lcSclPos,'') ,19)+loParentForm.ariaform1.Ariapageframe1.Page1.cboWarehouse.Value+Space(10),'StyDye')
      *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]

      *E300408,1 Message : 40012
      *E300408,1 Style/color xxx is not assigned to warehouse xxx
      *E300408,1 Button : 40002
      *E300408,1 Add Reenter

      *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]
      *IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.AddSty+IIF(loBranchFormSet.llExtended,loBranchFormSet.lcsclsp+lcSclPos,'') )+'|'+TRIM(loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value))=1
      If gfModalGen('QRM40012B40002','ALERT',Trim(m.AddSty+Iif(loBranchFormSet.llExtended,Alltrim(loBranchFormSet.lcSclSp)+lcSclPos,'') )+'|'+Trim(loParentForm.ariaform1.Ariapageframe1.Page1.cboWarehouse.Value))=1
        *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]

        *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]
        *DO gpAdStyWar WITH PADR(m.AddSty+IIF(loBranchFormSet.llExtended,loBranchFormSet.lcsclsp+lcSclPos,'') ,19),SPACE(10),loParentForm.ariaform1.ariapageframe1.page1.cboWarehouse.Value
        Do gpAdStyWar With Padr(m.AddSty+Iif(loBranchFormSet.llExtended,Alltrim(loBranchFormSet.lcSclSp)+lcSclPos,'') ,19),Space(10),loParentForm.ariaform1.Ariapageframe1.Page1.cboWarehouse.Value
        *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]
      Else
        Replace &lnCurrSz With 0
        Return
      Endif
    Endif

  Endif
  Select (lcTempCur)
  lnLineTot = 0
  For lnT = 1 To Iif(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,Alen(loBranchFormSet.laSize,1) )
    lcT = Alltrim(Str(lnT))
    lnLineTot = lnLineTot + Evaluate(lcTempCur+'.QTY'+lcT)
  Endfor
  Replace nTotal With lnLineTot
Endif
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
Function lfSclPos
Parameters lnIndex,loBarnFormSet
Private lnIndex,lnIncrm,lnCount
lnCount = 1
*-- Loop to get the scale number dependign on the size position in the list of all sizes
lnIncrm = 0
For lnCount = 1 To Alen(laExtSz,1)
  If lnIncrm < lnIndex And lnIndex <= lnIncrm+laExtSz[lnCount,2]
    Return lnCount
  Endif
  lnIncrm = lnIncrm + laExtSz[lnCount,2]
Endfor
*:**************************************************************************
*:* Name        : lfGetScale
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : get Scale of sent size(C200969,1)
*:***************************************************************************
Function lfGetScale
Parameters loBarnFormSet,lcScale,lcFit
lcRetScale = ''
=Seek('S'+Substr(Style.Scale,1,loBarnFormSet.lnScaleLen),'Scale')
Select Scale
Scan Rest While Type+Scale+prepak = 'S'+ Substr(Style.Scale,1,loBarnFormSet.lnScaleLen) For;
    IIF(loBarnFormSet.llMultiDime,Alltrim(CDIM1)==Alltrim(lcFit),.T.)
  For lnI = 1 To 8
    lcI = Alltrim(Str(lnI,1))
    If Alltrim(Upper(SZ&lcI)) = Alltrim(Upper(Padr(lcScale,5)))
      lcRetScale = Scale.Scale
      Exit
    Endif
  Endfor
Endscan
Return lcRetScale

*:**************************************************************************
*:* Name        : lfCalcTot
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Calculate Totals(C200969,1)
*:***************************************************************************
Function lfCalcTot
Parameters loBarnFormSet
Select (loBarnFormSet.lcTempCur)
lnRecn = Recno()
Go Bottom

For lnC = 1 To Iif(loBarnFormSet.llMultiDime ,loBarnFormSet.lnMaxSz,Alen(loBarnFormSet.laSize,1))
  lcC = Alltrim(Str(lnC))
  Select (loBarnFormSet.lcTempCur)
  Sum (QTY&lcC) To lnTotal For STYMAJOR <> Chr(255)
  Go Bottom
  If STYMAJOR = Chr(255)
    Replace QTY&lcC With lnTotal
  Endif
Endfor

Select (loBarnFormSet.lcTempCur)
Sum nTotal To lnTotal For STYMAJOR <> Chr(255)
Go Bottom
If STYMAJOR = Chr(255)
  Replace nTotal With lnTotal
Endif
loBarnFormSet.ariaform1.cmdSave.Enabled = lnTotal > 0


Select (loBarnFormSet.lcTempCur)
If Between(lnRecn ,1,Reccount())
  Go lnRecn
Endif

*:**************************************************************************
*:* Name        : lfGetStock
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : get Style Stock Values(C200969,1)
*:***************************************************************************
Function lfGetStock
Parameters loBarnFormSet

If !Used('BOM')
  =gfOpenTable('BOM','MULTIBOM','SH')
Endif

*! B610639,1 MMT 12/26/2013 Quick order entry screen gives error with certain style[T20131218.0001][Start]
If !Used('Scale_Stk')
  =gfOpenTable('Scale','Scale','SH','Scale_Stk')
Endif
*! B610639,1 MMT 12/26/2013 Quick order entry screen gives error with certain style[T20131218.0001][END]

Select(loBarnFormSet.lcTmpStk)
lcTmpStk = loBarnFormSet.lcTmpStk
Scan For !Inlist(Upper(COLORDSC) ,Upper('Free Stock'),Upper('Phys. Stock'),Upper('Plain Stock'))
  Dimension laFrStk[IIF(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,ALEN(loBarnFormSet.laSize,1))],;
    laPhyStk[IIF(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,ALEN(loBarnFormSet.laSize,1))],;
    laPlainStk[IIF(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,ALEN(loBarnFormSet.laSize,1))]

  Store 0 To laFrStk,laPhyStk,laPlainStk
  lnCounting = 1
  *! B610639,1 MMT 12/26/2013 Quick order entry screen gives error with certain style[T20131218.0001][Start]
*!*	  Select Scale
*!*	  =Seek('S'+Left(Style.Scale,loBarnFormSet.lnScaleLen))
*!*	  Scan Rest While Type+Scale+prepak = 'S' + Left(Style.Scale,loBarnFormSet.lnScaleLen) For ;
*!*	      IIF(loBarnFormSet.llMultiDime,Alltrim(Scale.CDIM1)==Alltrim(&lcTmpStk..FitDesc),.T.)
  Select Scale_Stk
  lcScaleValue = &lcTmpStk..Scale
  =Seek('S'+Left(lcScaleValue,loBarnFormSet.lnScaleLen))
  Scan Rest While Type+Scale+prepak = 'S' + Left(lcScaleValue,loBarnFormSet.lnScaleLen) For ;
      IIF(loBarnFormSet.llMultiDime,Alltrim(Scale_Stk.CDIM1)==Alltrim(&lcTmpStk..FitDesc),.T.)
  *! B610639,1 MMT 12/26/2013 Quick order entry screen gives error with certain style[T20131218.0001][End]
    *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]
    *lcSeekSty = PADR(&lcTmpStk..STYMAJOR,lobarnformset.lnmajorlen ) +loBarnFormSet.lcstysp +PADR(&lcTmpStk..COLOR,lnClrLen)+IIF(loBranchFormSet.llExtended,loBarnFormSet.lcsclsp+Scale.Scale,'')
    *! B610639,1 MMT 12/26/2013 Quick order entry screen gives error with certain style[T20131218.0001][Start]
    *lcSeekSty = Padr(&lcTmpStk..STYMAJOR,loBarnFormSet.lnMajorLen ) +loBarnFormSet.lcStySp +Padr(&lcTmpStk..Color,lnClrLen)+Iif(loBranchFormSet.llExtended,Alltrim(loBarnFormSet.lcSclSp)+Scale.Scale,'')
    lcSeekSty = Padr(&lcTmpStk..STYMAJOR,loBarnFormSet.lnMajorLen ) +loBarnFormSet.lcStySp +Padr(&lcTmpStk..Color,lnClrLen)+Iif(loBranchFormSet.llExtended,Alltrim(loBarnFormSet.lcSclSp)+Scale_Stk.Scale,'')
    *! B610639,1 MMT 12/26/2013 Quick order entry screen gives error with certain style[T20131218.0001][END]
    *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]

    lcSeekSty = Padr(lcSeekSty,19)
    =Seek(lcSeekSty+loBarnFormSet.loParentForm.ariaform1.Ariapageframe1.Page1.cboWarehouse.Value,'STYDYE')
    =Seek(lcSeekSty,'STYLE')
    *! B610639,1 MMT 12/26/2013 Quick order entry screen gives error with certain style[T20131218.0001][Start]
    *For I = 1 To Scale.Cnt
    For I = 1 To Scale_Stk.Cnt
    *! B610639,1 MMT 12/26/2013 Quick order entry screen gives error with certain style[T20131218.0001][End]
      lcCountNo = Alltrim(Str(I))
      *C200969,3 MMT 05/13/2008 Fix bug of wrong Free stock Value{Start}
      *laFrStk[lnCounting] = STYDYE.Stk&lcCountNo - STYDYE.ALO&lcCountNo+IIF(loBarnFormSet.llincwip,STYDYE.WIP&lcCountNo,0)
      laFrStk[lnCounting] = StyDye.Stk&lcCountNo - StyDye.Ord&lcCountNo+Iif(loBarnFormSet.llincwip,StyDye.WIP&lcCountNo,0)
      *C200969,3 MMT 05/13/2008 Fix bug of wrong Free stock Value{End}

      laPhyStk[lnCounting] = StyDye.Stk&lcCountNo


      If gfSeek('0001'+Padr(Substr(lcSeekSty,1,loBarnFormSet.lnMajorLen),19),'BOM')
        Select BOM
        Scan Rest While cinvtype+ citmmajor+ ccstshttyp+ ccstsht_id+ typ+ citmmask+ mfgcode+cinvtypc+ Item+ Str(nlineno,6) = '0001'+Padr(Substr(lcSeekSty,1,loBarnFormSet.lnMajorLen),19) ;
            FOR cinvtypc = '0001'
          lcCompSty = Stuff(lcSeekSty,1,loBarnFormSet.lnMajorLen,Substr(BOM.Item,1,loBarnFormSet.lnMajorLen))
          lnRecNoStyDye = Recno('STYDYE')
          =Seek(lcCompSty+loBarnFormSet.loParentForm.ariaform1.Ariapageframe1.Page1.cboWarehouse.Value+Space(10),'STYDYE')

          *C200969,3 MMT 05/13/2008 Fix bug of wrong Free stock Value{Start}
          *laPlainStk[lnCounting] = STYDYE.Stk&lcCountNo - STYDYE.ALO&lcCountNo
          laPlainStk[lnCounting] = StyDye.Stk&lcCountNo
          *C200969,3 MMT 05/13/2008 Fix bug of wrong Free stock Value{End}

          If Between(lnRecNoStyDye ,1,Reccount('STYDYE'))
            Go lnRecNoStyDye  In StyDye
          Endif
        Endscan
      Endif
      lnCounting  = lnCounting + 1
    ENDFOR
    *! B610639,1 MMT 12/26/2013 Quick order entry screen gives error with certain style[T20131218.0001][Start]
    *Select Scale
    SELECT Scale_Stk
    *! B610639,1 MMT 12/26/2013 Quick order entry screen gives error with certain style[T20131218.0001][End]
  Endscan

  Select(loBarnFormSet.lcTmpStk)
  Replace COLORDSC With 'Free Stock'
  Replace nTotal With 0
  For lnC = 1 To Iif(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,Alen(loBarnFormSet.laSize,1))
    lcC = Alltrim(Str(lnC))
    Replace QTY&lcC With laFrStk[lnC]
    Replace nTotal With nTotal + QTY&lcC
  Endfor
  Scatter Memo Memvar
  m.COLORDSC = 'Phys. Stock'
  Append Blank
  m.nTotal = 0
  For lnC = 1 To Iif(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,Alen(loBarnFormSet.laSize,1))
    lcC = Alltrim(Str(lnC))
    m.QTY&lcC = laPhyStk[lnC]
    m.nTotal = m.nTotal + m.QTY&lcC
  Endfor
  Gather Memo Memvar
  m.COLORDSC = 'Plain Stock'
  Append Blank
  m.nTotal = 0
  For lnC = 1 To Iif(loBarnFormSet.llMultiDime,loBarnFormSet.lnMaxSz,Alen(loBarnFormSet.laSize,1))
    lcC = Alltrim(Str(lnC))
    m.QTY&lcC = laPlainStk[lnC]
    m.nTotal = m.nTotal + m.QTY&lcC
  Endfor
  Gather Memo Memvar
Endscan

*:**************************************************************************
*:* Name        : lfvSave
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Save button function(C200969,1)
*:***************************************************************************
Function lfvSave
Parameters loBranchFormSet


lnTotal =    0
lcTempCur = loBranchFormSet.lcTempCur
Select (lcTempCur)
Locate For STYMAJOR = Chr(255)
If Found()
  If &lcTempCur..nTotal = 0
    Return .F.
  Else
    lnTotal =  &lcTempCur..nTotal
  Endif
Else
  Return .F.
Endif

lcOrdHdr  = loBranchFormSet.loParentForm.OFORMENVIRONMENT.lcOrdHdr
lcOrdLine = loBranchFormSet.loParentForm.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.DetailFile
m.Style   = loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value
Select (lcOrdLine)
If loBranchFormSet.loParentForm.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.Editlinecompletedate And Seek(loBranchFormSet.loParentForm.lcOrdType+&lcOrdHdr..Order+&lcOrdHdr..Store+Substr(m.Style,1,loBranchFormSet.lnMajorLen),lcOrdLine,'ORDLINST')
  Locate Rest While cOrdType+Order+Store+Style = loBranchFormSet.loParentForm.lcOrdType+&lcOrdHdr..Order+&lcOrdHdr..Store+Substr(m.Style,1,loBranchFormSet.lnMajorLen);
    FOR Complete = loBranchFormSet.ariaform1.dtpCompDate.Value

  If Found()
    lnResp = gfModalGen('QRM00000B32014',.F.,.F.,.F.,;
      'The selected complete date has been used on a previous ORDLINE for that Style')
    If lnResp = 1
    Else
      If lnResp = 2
        loBranchFormSet.ariaform1.dtpCompDate.SetFocus()
      Else
        =lfvClear(loBranchFormSet)
      Endif
      Return
    Endif
  Endif
Endif

Select (lcTempCur)
Private lcFilter
lcFilter = Filter()
Set Order To Tag (lcTempCur)
Set Filter To
Go Top
Select (lcTempCur)

llDfChkQty  = .F.

If Iif(Type('loBranchFormSet.AriaForm1.txtChkQty.VAlue') = "C",Val(loBranchFormSet.ariaform1.txtChkQty.Value),loBranchFormSet.ariaform1.txtChkQty.Value) > 0
  llDfChkQty = Iif(Type('loBranchFormSet.AriaForm1.txtChkQty.VAlue') = "C",Val(loBranchFormSet.ariaform1.txtChkQty.Value),loBranchFormSet.ariaform1.txtChkQty.Value) <> lnTotal
Endif


If llDfChkQty And gfModalGen('QRM32031B32000','ALERT',Alltrim(Str(lnTotal,8)))=2
  Select (lcTempCur)
  Set Filter To &lcFilter
  Go Top
  Return .F.
Endif

m.cOrdType  = loBranchFormSet.loParentForm.lcOrdType
m.ORDER     = &lcOrdHdr..Order
m.ACCOUNT   = &lcOrdHdr..ACCOUNT
m.Store     = &lcOrdHdr..Store
m.CustPo    = Iif(!&lcOrdHdr..multipo ,&lcOrdHdr..CustPo,'')   &&Save Cust PO # in OrdLine if not Multi Store
m.cWareCode = loBranchFormSet.loParentForm.ariaform1.Ariapageframe1.Page1.cboWarehouse.Value
m.Start     = loBranchFormSet.loParentForm.ariaform1.Ariapageframe1.Page1.txtStart.Value

Do lpQukSave

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
Procedure lpQukSave
Private lnPrvsAls , lnSizeSeq , lnScaleLen , lnQkCnt , lnGrprice , lnCommPrc,;
  lcOldColor , llSkip

lcOldColor = ''
lnPrvsAls = Select (0)
llSkip = .F.



lnScaleLen = loBranchFormSet.lnScaleLen      && Extended size Scale ID Length.
Select (lcTempCur)
Set Filter To
Locate
Store 0 To lnGrprice , lnCommPrc , lnQkCnt
Scan For  STYMAJOR <> Chr(255) And nTotal > 0
  m.Note_Mem = Note_Mem
  lnInc = 1

  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
  Dimension laAddSz[1]
  Store '' To laAddSz
  Store 0 To lnGrosprice ,lnSzGrp
  Store .F. To llRealPrice
  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

  For lnQkCnt = 1 To Alen(loBranchFormSet.laExtSz,1)
    Select (lcTempCur)


    *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
    If lnSzGrp > 0
      lnQkCnt = lnSzGrp
      lnInc = lnInc - loBranchFormSet.laExtSz[lnQkCnt,2]
    Endif
    *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]


    *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[Start]
    *m.Style = PADR(STYMAJOR,loBranchFormSet.lnMajorLen)+ loBranchFormSet.lcStySp +SUBSTR(COLOR,1,loBranchFormSet.lnClrLen )+IIF(loBranchFormSet.llExtended,loBranchFormSet.lcSclSp+ loBranchFormSet.laExtSz[lnQkCnt,1] , '' )
    m.Style = Padr(Padr(STYMAJOR,loBranchFormSet.lnMajorLen)+ loBranchFormSet.lcStySp +Substr(Color,1,loBranchFormSet.lnClrLen )+Iif(loBranchFormSet.llExtended,Alltrim(loBranchFormSet.lcSclSp)+ loBranchFormSet.laExtSz[lnQkCnt,1] , '' ),19)
    *! C200969,2 MMT 04/15/2008 Add Quick Order entry screen to Sales order screen[End]

    =Seek('S'+loBranchFormSet.laExtSz[lnQkCnt,1],'SCALE')
    If Alltrim(Scale.CDIM1) <> Alltrim(&lcTempCur..FitDesc)
      Loop
    Endif

    Wait Window 'Collect data for Style # : ' + M.Style +'...'    Nowait
    If Seek(m.Style,'STYLE')
      Store 0 To m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TOTQTY,;
        m.BOOK1,m.BOOK2,m.BOOK3,m.BOOK4,m.BOOK5,m.BOOK6,m.BOOK7,m.BOOK8,m.TOTBOOK,;
        lnGrprice , lnCommPrc



      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
      Store 0 To lnNetPrice
      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]


      For lnJ = 1 To loBranchFormSet.laExtSz[lnQkCnt,2]
        If Iif(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz < lnInc ,.F.)
          lnInc = 1
        Endif

        lcInc = Alltrim(Str(lnInc))
        lcQ = Str(lnJ,1)
        m.TOTQTY   = m.TOTQTY +  QTY&lcInc
        m.QTY&lcQ  = QTY&lcInc
        m.BOOK&lcQ = QTY&lcInc

        *B609500,1 TMI 01/15/2011 [Start] go only in this loop if there is a supplied qty
        If m.QTY&lcQ > 0
          *B609500,1 TMI 01/15/2011 [End  ]

          lnGrprice  = loBranchFormSet.ariaform1.txtGrsPrc.Value
          lnCommPrc  = loBranchFormSet.ariaform1.spnComm.Value

          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
          lnSzGrp = lnQkCnt
          If Ascan(laAddSz,lcInc,1)= 0
            If lnGrosprice = 0 And !llRealPrice
              lnGrosprice = &lcTempCur..Pric&lcInc
              lnNetPrice = &lcTempCur..NtPri&lcInc
              llRealPrice = .T.
              If Empty(laAddSz[1])
                laAddSz[1] = lcInc
              Else
                Dimension laAddSz[ALEN(laAddSz,1)+1]
                laAddSz[ALEN(laAddSz,1)]= lcInc
              Endif

            Else
              If lnGrosprice =&lcTempCur..Pric&lcInc
                If Empty(laAddSz[1])
                  laAddSz[1] = lcInc
                Else
                  Dimension laAddSz[ALEN(laAddSz,1)+1]
                  laAddSz[ALEN(laAddSz,1)]= lcInc
                Endif
              Else
                m.TOTQTY   = m.TOTQTY -  QTY&lcInc
                m.QTY&lcQ  = 0
                m.BOOK&lcQ = 0
              Endif
            Endif
          Else
            m.TOTQTY   = m.TOTQTY -  QTY&lcInc
            m.QTY&lcQ  = 0
            m.BOOK&lcQ = 0
          Endif
          *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

          *B609500,1 TMI 01/15/2011 [Start]
        Endif
        *B609500,1 TMI 01/15/2011 [End  ]

        lnInc  = lnInc + 1
      Endfor

      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
      lnGrprice = lnGrosprice
      Store 0 To lnGrosprice
      Store .F. To llRealPrice
      *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

      *-- Don't add any record with totalQty =Equal zero.
      If m.TOTQTY = 0
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
        Dimension laAddSz[1]
        Store '' To laAddSz
        lnSzGrp = 0
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]
        Loop
      Endif


      m.TOTBOOK  = m.TOTQTY
      m.SCALE    = Style.Scale
      m.Desc1    = Style.Desc1
      m.prepak   = Style.prepak
      m.Season   = Style.Season
      m.Gl_Sales = Alltrim(&lcOrdHdr..Gl_Sales)+Style.cSlsGlLink
      m.Gl_Sales = Iif(loBranchFormSet.loParentForm.OFORMENVIRONMENT.laSetups[4,2]='Y' And Seek('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')
      m.Flag     = 'N'
      m.COMPLETE  = loBranchFormSet.ariaform1.dtpCompDate.Value
      *-- Get price for each line of scales has different prices ,
      *-- or check qty differs from entred qty
      lcQkCnt = Alltrim(Str(lnQkCnt))
      m.DISC_PCNT =  loBranchFormSet.ariaform1.spnDisc.Value
      If m.TOTQTY > 0
        m.Gros_Price = lnGrprice
        m.COMM1      = lnCommPrc
        m.PRICE      = Round( m.Gros_Price*(1-m.DISC_PCNT/100) ,2 )

        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
        *m.Gros_Price = &lcTempCur..Pric&lcQkCnt
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

        m.COMM1      = &lcTempCur..Comm

        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
        *m.Price      = ROUND( &lcTempCur..NtPri&lcQkCnt ,2 )
        m.PRICE      = Round(lnNetPrice  ,2 )
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

        m.DISC_PCNT  =  &lcTempCur..Discount


        loBranchFormSet.loParentForm.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo = loBranchFormSet.loParentForm.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo  + 1
        m.LINENO   = loBranchFormSet.loParentForm.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo
        Insert Into (lcOrdLine) From Memvar

        Replace BOOK  With BOOK + m.TOTBOOK,;
          OPEN  With Open + m.TOTQTY ,;
          BOOKAMT With BOOKAMT + m.TOTBOOK*m.PRICE,;
          OPENAMT With OPENAMT + m.TOTQTY*m.PRICE  In (lcOrdHdr)

        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
        If m.TOTQTY <> 0
          lnQkCnt = lnSzGrp-1
          Loop
        Endif
        *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

      Endif
    Endif
  Endfor
Endscan
Wait Clear
Select (lnPrvsAls)
*:**************************************************************************
*:* Name        : lfAftRoColChng
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : after row Col. of the Grid(C200969,1)
*:***************************************************************************
Function lfAftRoColChng
Parameters loBranchFormSet
lcTempCur = loBranchFormSet.lcTempCur

lnCount = 2
For lnS = 1 To Iif(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz,Alen(loBranchFormSet.laSize,1))
  lnCount = lnCount + 1
  lcT = Alltrim(Str(lnS))
  lcCount= Alltrim(Str(lnCount))
  loBranchFormSet.ariaform1.grdLines.Column&lcCount..Header1.Caption = Evaluate(loBranchFormSet.lcTempCur +'.SZ'+lcT)
Endfor
lfStkGridRefresh(loBranchFormSet)
If loBranchFormSet.lccurrsz < 3 Or loBranchFormSet.lccurrsz > loBranchFormSet.ariaform1.grdLines.ColumnCount - 1
  Return
Endif



lcFPos = Alltrim(Str(loBranchFormSet.lccurrsz))
lnCurrSz  = loBranchFormSet.ariaform1.grdLines.Column&lcFPos..ControlSource
lcSclPos = lfGetScale(loBranchFormSet,loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption,&lcTempCur..FitDesc)
lnPosSc = 0

*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
*lnPosSc  = ASCAN(loBranchFormSet.laExtSz,lcSclPos)
lnPosSc  = Ascan(loBranchFormSet.laSize,Alltrim(loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption))
*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

If lnPosSc  > 0
  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
  *lnPosSc = ASUBSCRIPT(loBranchFormSet.laExtSz,lnPosSc,1)
  lnPosSc = Asubscript(loBranchFormSet.laSize,lnPosSc,1)
  *! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

  *! B609513,1 MMT 01/27/2011 Fix error in SO Quick order entry screen in case of Mutli dimension scale[Start]
  If loBranchFormSet.llMultiDime
    lnPosSc = lfGetScPos(Alltrim(loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption),&lcTempCur..Scale)
    If lnPosSc = 0
      Return
    Endif
  Endif
  *! B609513,1 MMT 01/27/2011 Fix error in SO Quick order entry screen in case of Mutli dimension scale[End]

  lcPos = Alltrim(Str(lnPosSc))
  With loBranchFormSet.ariaform1
    .txtGrsPrc.Value = &lcTempCur..Pric&lcPos
    .txtNetPri.Value = &lcTempCur..NtPri&lcPos
    .spnDisc.Value  = &lcTempCur..Discount
    .spnComm.Value = &lcTempCur..Comm
  Endwith
Endif


*:**************************************************************************
*:* Name        : lfVComm
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Validate comm. field (C200969,1)
*:***************************************************************************
Function lfVComm
Parameters loBranchFormSet
lcTempCur = loBranchFormSet.lcTempCur
*C200969,3 MMT 05/13/2008 Let Comm applicable on all lines{Start}
*REPLACE comm WITH loBranchFormSet.AriaForm1.SpnComm.Value  IN  (lctempcur)
Replace  All Comm With loBranchFormSet.ariaform1.spnComm.Value  In  (lcTempCur)
*C200969,3 MMT 05/13/2008 Let Comm applicable on all lines{End}

*:**************************************************************************
*:* Name        : lfStkGridRefresh
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/27/2008
*:* Purpose     : Refresh stock grid headers (C200969,1)
*:***************************************************************************
Function lfStkGridRefresh
Parameters loBranchFormSet

lnCount = 1
For lnT = 1 To Iif(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,Alen(loBranchFormSet.laSize,1))
  lnCount = lnCount + 1
  lcT = Alltrim(Str(lnT))
  lcCount= Alltrim(Str(lnCount))
  loBranchFormSet.ariaform1.grdStk.Column&lcCount..Header1.Caption = Evaluate(loBranchFormSet.lcTmpStk +'.SZ'+lcT)
Endfor
*! C200969,1 MMT 03/27/2007 Add Quick Order entry screen to Sales order screen[End]

*! B608617,1 MMT 07/14/2008 Add trigger to update bomline with new ex rate    [Start]
*:**************************************************************************
*:* Name        : lfUPDEXRATE
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 07/14/2008
*:* Purpose     : Add trigger to update bomline with new ex rate(B608617,1)
*:***************************************************************************
Function lfUPDEXRATE
llGetPORate = gfGetMemVar('M_POPRDEF')

If llGetPORate And (&lcTmpLine..Cstytype = 'P' And &lcTmpLine..CBUSDOCU = 'P')
  Replace nexrate With &lcTmpLine..nLanPrRat
Endif
*! B608617,1 MMT 07/14/2008 Add trigger to update bomline with new ex rate    [End]
*! C201115,1 MMT 03/11/2009 Convert PO Quick Order Entry Screen to Aria4      [Start]
*:**************************************************************************
*:* Name        : lfPOQKORD
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Add Menu to Call Quick Order entry from PO Screen
*:***************************************************************************
Function lfPOQKORD

lcHostFormName = '[' + loFormSet.cHostFormName + ']'
If loFormSet.ariaform1.mainWorkOrder.cWorkOrdTyp  = 'PP'
  Local lnCntBar
  lnCntBar = Cntbar('_OPTIONPOP')+1
  Define Bar lnCntBar Of _OPTIONPOP Prompt 'Quic\<k Order Entry'  Skip For  ;
    gfFormIsActive(&lcHostFormName) And ((Type('_screen.ActiveForm.PgfPOStyle') = "O" And  _Screen.ActiveForm.PgfPOStyle.ActivePage<>2) .Or. (_Screen.ActiveForm.Parent.ActiveMode = 'V'))
  On Selection Bar lnCntBar  Of _OPTIONPOP Do lfOpnPQkScr
Endif
*:**************************************************************************
*:* Name        : lfOpnPQkScr
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Call Quick Order entry from PO Screen
*:***************************************************************************
Function lfOpnPQkScr
loFormSet = _Screen.ActiveForm.Parent
*-- If user did not select the vendor
With loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder
  If Empty(.oKeyHeaderClass.kbVendor.Keytextbox.Value)
    =gfModalGen('TRM34020B34000','DIALOG',Iif(Alltrim(.cTransType)$'NN|PA','source location','vendor')+'|'+' ')
    .oKeyHeaderClass.kbVendor.Keytextbox.SetFocus
    Return .F.
  Endif
  *-- If not multi ware house and there is no ware house selected or account code.
  llMulChk = .oKeyHeaderClass.chkMulShpTo.Value
Endwith
With loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1
  If .Parent.oMainClass.lMultiWarehous And !llMulChk And;
      EMPTY(.Parent.oKeyHeaderClass.kbWarehouse.Keytextbox.Value) And;
      EMPTY(.Parent.oKeyHeaderClass.kbAccount.Keytextbox.Value)
    *-You have to enter the select a ware house or account code.
    =gfModalGen('TRM34020B34000','DIALOG','ship to location or customer'+'|'+' ')
    If .Parent.lLocationCustomer
      .Parent.oKeyHeaderClass.kbWarehouse.Keytextbox.SetFocus
    Else
      .Parent.oKeyHeaderClass.kbAccount.Keytextbox.SetFocus
    Endif
    Return .F.
  Endif
Endwith
lcAlias = Select()
Private laSize,laTot,;
  lcScrTtl,lcDet_Ttl,lnClrLen,lnClrPos,lcClrSpr,lcTempCur,lcTempNote,lcAlias,lnMrk ,;
  lcQkWin0,lcQkWin1,lcQkWin2,lcQkWin3,lcQkWin4,lcOldValue,lcSepart,lcItemPct,laExtSz,lcOrd,llEdit,;
  llChang,laStyClQty,llDifPrice,llShw1,laOldVal,lcBrowFlds



Do Form  (Oariaapplication.ScreenHome+'\PO\PoQKORD.scx') With _Screen.ActiveForm.Parent


Select (lcAlias)
*:**************************************************************************
*:* Name        : lfvPQStyle
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Validate Style in Quick PO Entry Screen
*:***************************************************************************
Function lfvPQStyle
Parameters loBranchFormSet,loParentForm

loBranchFormSet.lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
If loParentForm.DataSessionId <> Set("Datasession")
  Set DataSession To loParentForm.DataSessionId
Endif


lcStyleVal = loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value
Private lcAlias,lcGetSty,lcStat,lnIncrmnt,lnClrs,lnCount,lnJ,lcSeekSty,laSum,laSavStat,lcStyle
*--- Select
lcAlias = Alias()
loBranchFormSet.lnMajorLen = Len(gfItemMask("PM", '', '0001'))

lcStyleVal = Padr(Alltrim(lcStyleVal),loBranchFormSet.lnMajorLen)
If (!Empty(lcStyleVal) And ('?' $ lcStyleVal  .Or. !Seek(lcStyleVal ,'STYLE'))) Or ;
    loBranchFormSet.ariaform1.kbstymaj.selectedfrombrowse
  Select Style
  lcGetSty = Padr(gfStyBrw("M" , lcStyleVal  , "" , .F.),loBranchFormSet.lnMajorLen)
  loBranchFormSet.ariaform1.kbstymaj.selectedfrombrowse  = .F.
  If Empty(lcGetSty)
    loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value = ''
    Return .F.
  Else
    loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value = lcGetSty
  Endif
Endif

Store ''  To lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
Store 1 To lnCurrUnt1, lnCurrUnt2
llConCst     = .F.
m.ccstsht_id = " "
lcTmpFile   = loParentForm.ariaform1.mainWorkOrder.cpoline
lcMastPosHd = loParentForm.ariaform1.mainWorkOrder.cposhdr
m.Style   = Alltrim(loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value)
lcStyleVal = m.Style


With loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder
  Store 0 To .nFcost1, .nFcost2, .nFcost3, .nFcost4, .nFcost5, .nFcost6, .nFcost7
  *-- Get the selected price currency and rate in the header folder
  lcPriceCurr   = loParentForm.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cntMultiCurrency.KbPriceCurrency.Keytextbox.Value
  lnPricRate    = loParentForm.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cntMultiCurrency.txtPriceRate.Value
  *-- Get the selected duty currency in the header folder
  lcDutyCurr    = loParentForm.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cntMultiCurrency.KbDutyCurrency.Keytextbox.Value
  lnDutyRate    = loParentForm.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cntMultiCurrency.txtDutyRate.Value

  ldEnteredDate = loParentForm.ariaform1.PgfPOStyle.Page1.cntheaderFolder.DtPickerEntered.Value
  *-- Get the complete date in the header folder
  ldCompDate    = loParentForm.ariaform1.PgfPOStyle.Page1.cntheaderFolder.DtPickerComplete.Value
  lcCstShtId = ''
  m.COMPLETE = ldCompDate
  *-- Get the style cost sheet type. "I" for Style PO (PP), "M" for Dye Order (PD)
  lcCstShtType = Iif(.cTransType = "PD","M","I")

  *-- The ware house in the header key section
  m.cWareCode = loParentForm.ariaform1.cntPoHeader.kbWarehouse.Keytextbox.Value
  *-- To check if there is any reason of rejecting the style
  llAbort =.F.

  *-- If system setup to point of sale don't allow to add location.
  llPosSetN = .cTransType = "NN" And loParentForm.ariaform1.mainWorkOrder.cSystemType = "P"

  *-- Style major length
  lnStyleLen = loBranchFormSet.lnMajorLen
  *-- Get the style major
  lcStyMaj = Substr(lcStyleVal,1,lnStyleLen)
  *loParentForm.ariaForm1.mainworkorder.cStyMajor = lcStyMaj
  If !Empty(lcStyleVal)
    Select Style
    Set Order To Style
    Seek lcStyleVal
    loBranchFormSet.ariaform1.txtStydesc.Value = 	Style.Desc1
    Do While .T.
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
      If Style.cDivision <> loParentForm.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cboDivision.Value
        *-Conflict ! styles restricted to division XXXX, Cannot proceed!
        =gfModalGen('TRM34041B34000','DIALOG',Alltrim(loParentForm.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cboDivision.Value))
        llAbort=.T.
        Exit
      Endif

      *-- If there is a conflict in the style puchasing group
      If !Empty(Style.cPurCode) And Style.cPurCode <> Alltrim(loParentForm.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cboPurGroup.Value)
        *-Conflict ! styles restricted to purchase XXXX, Cannot proceed!
        =gfModalGen('TRM34110B34000','DIALOG',Alltrim(loParentForm.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cboPurGroup.DisplayValue))
        llAbort=.T.
        Exit
      Endif

      .oMainClass.mstycstsht (lcCstShtType, Padr(lcStyMaj,19), .T.)
      If .cTransType $ 'PP|PD|NN|RP'
        If !(loParentForm.ariaform1.cntPoHeader.cboStatus.Value $ 'HB')
          If Seek(lcStyMaj,.oMainClass.cBomHdr)
            Select (.oMainClass.cBomHdr)
            Locate Rest While citmmajor+ccstshttyp+ccstsht_id = Padr(lcStyMaj,19)+lcCstShtType;
              FOR lDefCstSht And !Empty(ccstsht_id)
            If !Found()
              *-No cost lines found in the cost sheet, Cannot proceed!
              *=gfModalGen('TRM34037B34000','DIALOG')
              llAbort=.T.
              Select (.oMainClass.cposhdr)
              Exit
            Else
              m.ccstsht_id = ccstsht_id
            Endif
          Else
            *-No cost lines found in the cost sheet, Cannot proceed!
            *=gfModalGen('TRM34037B34000','DIALOG')
            llAbort=.T.
            Exit
          Endif
        Else
          If !Seek(Padr(lcStyMaj,19)+lcCstShtType,.oMainClass.cBomHdr) Or Empty(ccstsht_id)
            *-- If the style has no cost sheet, don't continue in case of dye order
            *-- Warrning ! This style has no cost sheet.
            If .cTransType = 'PD'
              *-No cost lines found in the cost sheet, Cannot proceed!
              *=gfModalGen('TRM34037B00000','DIALOG')
              llAbort=.T.
              Exit
            Else
              *-- Give a notification message the the style does not have cost sheet
              * =gfModalGen('INM34038B34000','DIALOG')
            Endif
          Else
            *-- If there is a cost sheet then get the default code.
            Select (.oMainClass.cBomHdr)
            Locate Rest While citmmajor+ccstshttyp+ccstsht_id = Padr(lcStyMaj,19)+lcCstShtType;
              FOR lDefCstSht
            If Found()
              m.ccstsht_id = ccstsht_id
            Endif
          Endif
          Select (.oMainClass.cpoline)
        Endif
      Endif
      lcCstShtId = m.ccstsht_id
      lcTmpBom = .oMainClass.cBom
      Exit
    Enddo
  Endif
  If !Empty(lcStyleVal) And !llAbort
    m.llAddNew = .F.
    If Seek(lcStyleVal,.oMainClass.cpoline)
      *--This Style has entred on this order
      *--Button 02011 : \!\<OK;\?\<Cancel
      lnResp = gfModalGen('QRM00000B34015',.F.,.F.,.F.,'This style has already been entred on this order '+;
        +'- do you wish to Create a New Line?')
      If lnResp = 1
        m.llAddNew = .T.
      Else
        If lnResp = 2
          m.llAddNew = .F.
        Else
          =lfvPClear(loBranchFormSet,.F.)
          Return
        Endif
      Endif
    Endif
    loBranchFormSet.ariaform1.spnDisc.Enabled = .T.
    loBranchFormSet.ariaform1.dtpCompDate.Enabled = .T.
    loBranchFormSet.ariaform1.dtpCompDate.Value = loParentForm.ariaform1.PgfPOStyle.Page1.cntheaderFolder.DtPickerComplete.Value
    Store 0   To m.Gros_Price, m.PRICE, m.DISC_PCNT
    =Seek(Allt(m.Style),'STYLE')
    loBranchFormSet.ariaform1.cmdClear.Enabled = !Empty(m.Style)
    Store 0   To m.Gros_Price, m.PRICE, m.DISC_PCNT
    Store .F. To m.LCONTRACT
    Store '' To m.Store

    =Seek(Allt(m.Style),'STYLE')
    laSize = ''
    loBranchFormSet.laSize = ''
    loBranchFormSet.laExtSz = ''

    =Seek(Allt(m.Style),'STYLE')
    loBranchFormSet.lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
    m.SCALE = Style.Scale
    Select Scale,Cnt From Scale Where Type+Scale='S'+Substr(m.SCALE,1,loBranchFormSet.lnScaleLen) Order By 1 Into Array laExtSz
    Select Scale
    lcStyScl = Substr(Style.Scale,1,loBranchFormSet.lnScaleLen)
    =Seek('S'+lcStyScl ,'SCALE')
    If !Empty(Scale.CDIM1)
      loBranchFormSet.llMultiDime = .T.
    Endif
    lnMaxSz = 0
    If loBranchFormSet.llMultiDime
      lnMaxSz = 0
      lnLastCnt = 0
      Select Scale
      =Seek('S'+lcStyScl ,'SCALE')
      Do While Type+Scale+prepak = 'S' + lcStyScl .And. !Eof('SCALE')
        lnRecord = Recno('Scale')
        lcFit = Scale.CDIM1
        lnMaxSz = 0
        Scan Rest While Type+Scale+prepak = 'S' + lcStyScl For ;
            CDIM1 = lcFit
          For lnI = 1 To Scale.Cnt
            lnMaxSz = lnMaxSz  + 1
          Endfor
        Endscan
        If lnMaxSz > lnLastCnt
          lnLastCnt = lnMaxSz
        Endif
        lnMaxSz = Iif(lnMaxSz >= lnLastCnt,lnMaxSz,  lnLastCnt)
        If Between(lnRecord+1,1,Reccount('Scale'))
          Go lnRecord+1 In Scale
        Endif
      Enddo
    Endif
    loBranchFormSet.lnMaxSz = lnMaxSz
    lnIncrmnt = 0
    For lnCount = 1 To Alen(laExtSz,1)
      lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
    Endfor
    If lnIncrmnt > 16
      *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'This Style has scale with More than 16 Sizes.')
    Endif

    lnIncrmnt = 0
    For lnCount = 1 To Alen(laExtSz,1)
      =Seek('S'+laExtSz[lnCount,1],'SCALE')
      For lnJ = 1 To laExtSz[lnCount,2]
        lcZ = Str(lnJ,1)
        Dimension laSize[lnIncrmnt+lnJ]
        laSize[lnIncrmnt+lnJ] = Alltrim(Scale.SZ&lcZ)
      Endfor
      lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
    Endfor

    *--Get Colors
    Store 0  To lnClrPos,lnClrLen
    Private lnClrPos,lnClrLen
    Store 0 To lnSizePos,lnSizeLen
    lfGetClrD()
    loBranchFormSet.lnClrLen = lnClrLen
    loBranchFormSet.lnClrPos = lnClrPos

    lfPOCrtTempFiles(loBranchFormSet)

    *--Create color array
    Select Distinct Substr(Style.Style,lnClrPos,lnClrLen) From Style ;
      WHERE Style = Padr(m.Style,loBranchFormSet.lnMajorLen) ;
      INTO Array laClr


    Store '' To lcStySp,lcSclSp
    Declare laItemSeg[1]
    =gfItemMask(@laItemSeg)
    For lnJ = 1 To Alen(laItemSeg,1)
      If laItemSeg[lnJ,1] = 'F'
        lcStySp = laItemSeg[lnJ,6]
        loBranchFormSet.lcStySp = lcStySp
        Loop
      Endif
      If laItemSeg[lnJ,1] = 'C'
        lcSclSp = laItemSeg[lnJ,6]
        loBranchFormSet.lcSclSp = lcSclSp
        Loop
      Endif
    Endfor
    Store 0   To m.Gros_Price, m.PRICE, m.DISC_PCNT
    Store .F. To m.LCONTRACT
    Store '' To m.Store

    Private lnMaxInd
    *! B609736,1 MMT 11/28/2011 So - quick order entry screen not showing all colours of a style[T20110822.0008][Start]
    *lnMaxInd = IIF(ALEN(laClr,1)>=10,10,ALEN(laClr,1))
    lnMaxInd = Alen(laClr,1)
    *! B609736,1 MMT 11/28/2011 So - quick order entry screen not showing all colours of a style[T20110822.0008][END]
    For lnClrs = 1 To lnMaxInd
      llAddNewVal = m.llAddNew
      Select (loBranchFormSet.lcTempCur)
      Scatter Memvar Blank
      m.llAddNew =llAddNewVal
      m.ccstsht_id =lcCstShtId
      m.STYMAJOR = Substr(lcStyleVal,1,lnStyleLen)
      loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.CstyMajor = M.STYMAJOR
      m.COLOR = laClr[lnClrs]
      *! B610000,1 HIA 07/12/2012 Color description, not displayed compelteded [T20120703.0019][End]
      *M.COLORDSC = PADR(gfCodDes(PADR(laClr[lnClrs],lnClrLen) , 'COLOR'),20)
      m.COLORDSC = Padr(gfCodDes(Padr(laClr[lnClrs],lnClrLen) , 'COLOR'),30)
      *! B610000,1 HIA 07/12/2012 Color description, not displayed compelteded [T20120703.0019][End]
      lnIncrmnt = 0
      Dimension laSzQty[ALEN(laSize,1)]
      Store 0 To laSzQty
      lnCounSz = 1
      lcFullStyClr = Padr(M.STYMAJOR ,loBranchFormSet.lnMajorLen) +;
        loBranchFormSet.lcStySp +Padr( M.COLOR,loBranchFormSet.lnClrLen )
      =Seek(Allt(lcFullStyClr),'STYLE')
      lcStyScl = Substr(Style.Scale,1,loBranchFormSet.lnScaleLen)
      Dimension laFits[1]
      laFits = ''
      Select Scale
      =Seek('S'+lcStyScl)
      lcScaleCurr = ''
      Scan Rest  While Type+Scale+prepak = 'S' + lcStyScl For !Eof('SCALE')
        lnRecord = Recno('Scale')
        lcFit = Scale.CDIM1
        If Ascan(laFits ,lcFit,1) > 0
          Loop
        Endif
        If !Empty(Scale.CDIM1)
          m.FitDesc = Scale.CDIM1
        Else
          m.FitDesc = Scale.CSCL_DESC
        Endif
        lnCounter = 1
        For lnCont = 1 To Iif(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz,Alen(laSize,1))
          lcCont  = Alltrim(Str(lnCont))
          Store '' To m.SZ&lcCont
        Endfor
        If Empty(laFits[1])
          laFits[1] = lcFit
        Else
          Dimension laFits[ALEN(laFits,1)+1]
          laFits[ALEN(laFits,1)] =lcFit
        Endif
        lnCounting = 1
        Scan Rest While Type+Scale+prepak = 'S' + lcStyScl For  CDIM1 = lcFit
          lcCounting = Alltrim(Str(lnCounting))
          lnCounting = lnCounting  + 1
          If Alltrim(lcScaleCurr) <> Alltrim(Scale.Scale)
            lcSeekSty = Padr(Padr(lcStyleVal,lnStyleLen)+lcStySp +Padr(M.COLOR,lnClrLen)+Iif(loBranchFormSet.llExtended,Alltrim(lcSclSp) +Scale.Scale,''),19)
            Store 0   To m.Gros_Price, m.PRICE, m.DISC_PCNT
            Store .F. To m.LCONTRACT
            Store '' To m.Store
            =Seek(Alltrim(lcSeekSty),'Style')

            If !loParentForm.ariaform1.cntPoHeader.chkMulShpTo.Value
              *--If ship to customer the location will be the drop down location.
              m.ACCOUNT   = loParentForm.ariaform1.cntPoHeader.kbAccount.Keytextbox.Value
              m.Store     = loParentForm.ariaform1.cntPoHeader.kbStore.Keytextbox.Value
              m.cWareCode = Iif(loParentForm.ariaform1.cntPoHeader.cboShpLoc.Value = "C",;
                .oMainClass.lcDropLoc, m.cWareCode)
              If !Seek(Style.Style+Padr(m.cWareCode,6)+Space(10),'STYDYE')
                *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
                If gfModalGen('QRM34048B34004','DIALOG',Style.Style+'|'+m.cWareCode) = 1
                  Do gpAdStyWar With Style.Style,Space(10),m.cWareCode
                Else
                  Loop
                Endif
              Endif
            Else
              m.cWareCode = Style.CDefWare
              m.ACCOUNT   = loParentForm.ariaform1.cntPoHeader.kbAccount.Keytextbox.Value
              m.Store     = loParentForm.ariaform1.cntPoHeader.kbStore.Keytextbox.Value
            Endif
            lnRecn = Recno('Scale')
            loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1.kbCostSheet.Keytextbox.Value = lcCstShtId
            lcOldWared = m.cWareCode
            llAddRec = .F.
            If Seek(Style.Style,.oMainClass.cpoline)
              Select (.oMainClass.cpoline)
              Scatter Memo Memvar
              m.LINENO = 0
              Append Blank
              Gather Memo Memvar
              llAddRec =.T.
            Endif
            loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1.kbCostSheet.sharedvalidation ()

            *! B610208,1 HIA 01/22/2013 Aria4xp - PO - Quick Order Entry displays browse of Imported Style cost [T20130109.0027][Start]
            loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.CstyMajor = M.STYMAJOR
            *! B610208,1 HIA 01/22/2013 Aria4xp - PO - Quick Order Entry displays browse of Imported Style cost [T20130109.0027][End]

            *! B609536,1 MMT 02/24/2011 PO Quick Order entry not pick up Purchase Prices for 2nd Style [Start]
            loParentForm.ariaform1.mainWorkOrder.mstycstsht(Iif(.cTransType = "PD","M","I"), Padr(M.STYMAJOR,19), Iif(.cTransType <> "PA",.T.,.F.))
            *! B609536,1 MMT 02/24/2011 PO Quick Order entry not pick up Purchase Prices for 2nd Style [End]
            If llAddRec
              Select (.oMainClass.cpoline)
              Delete
            Endif


            m.ccstsht_id =lcCstShtId
            m.cWareCode = lcOldWared
            m.COMPLETE = ldCompDate
            lcBomFile = loParentForm.ariaform1.mainWorkOrder.cBom
            lcBomHdrFile = loParentForm.ariaform1.mainWorkOrder.cBomHdr
            =Seek(Alltrim(lcSeekSty),'Style')
            =Seek('S'+Style.Scale,'Scale')
            lcScale = Style.Scale
            lcStyle = Style.Style
            m.lbasonsiz = .F.
            *B609576,1 MMT 05/02/2011 PO- Quick Order entry screen uses incorrect Purchase Price {Start}
            =Seek(Padr(M.STYMAJOR,19)+'I'+lcCstShtId,lcBomHdrFile)
            *B609576,1 MMT 05/02/2011 PO- Quick Order entry screen uses incorrect Purchase Price {END}
            If &lcBomHdrFile..lbasonsiz
              m.lbasonsiz = .T.
              Select (lcBomFile)
              *! B609499,1 MMT 01/13/2011 Quick Order entry screen uses wrong style purchase price[Start]
              *!*  	      SCAN FOR LIKE(STRTRAN(EVALUATE(lcBomFile+'.cItmMask'),'*','?') , lcStyle) .AND. ;
              *!*      		     (lcScale $ EVALUATE(lcBomFile+'.mSizes')) AND CCATGTYP = 'P'
              Scan For Like(Strtran(Evaluate(lcBomFile+'.cItmMask'),'*','?') , lcStyle) .And. ;
                  (lcScale $ Evaluate(lcBomFile+'.mSizes')) And CCATGTYP = 'P' And ccstsht_id =lcCstShtId
                *! B609499,1 MMT 01/13/2011 Quick Order entry screen uses wrong style purchase price[End]

                lnLineNo   = Atcline(lcScale , Alltrim(Evaluate(lcBomFile+'.mSizes')))
                *-- Get the string that hold the sizes.
                lcGetSize  = Iif(lnLineNo > 0 , Substr(Alltrim(Mline(Evaluate(lcBomFile+'.mSizes'),lnLineNo)),5) , "")
                lcSzes = Strtran(lcGetSize  ,",")
                lnUntCst = 0

                If !Empty(Evaluate(lcBomFile+'.CCURRCODE')) And Evaluate(lcBomFile+'.CCURRCODE')  <> lcPriceCurr
                  If lcPriceCurr <> Oariaapplication.BaseCurrency
                    * IF PO Currecny is not same as company base currecny, we will convert to base currency and then convert from base
                    * currecny to PO currecny
                    If Evaluate(lcBomFile+'.CCURRCODE') <> Oariaapplication.BaseCurrency
                      Local lcUnitSign,lcUnitSign ,lcRateSign
                      lcUnitSign = "/"
                      lnCurrUnit = Evaluate(lcBomFile+'.nCurrUnit')
                      lcRateSign = gfGetExSin(@lcUnitSign , Evaluate(lcBomFile+'.CCURRCODE'))
                      lcUnitSign = lcUnitSign
                      lcUntCstBase = Evaluate(lcBomFile+'.TotCost' + lcRateSign+;
                        STR(Evaluate(lcBomFile+'.nEXRate'),9,4)+lcUnitSign+ Str(lnCurrUnit))

                    Else
                      lcUntCstBase = Evaluate(lcBomFile+'.TotCost')
                    Endif

                    *Convert to Po currecny
                    lcUnitSign = "/"
                    lnCurrUnit = 1
                    lcRateSign = gfGetExSin(@lcUnitSign , Evaluate(lcBomFile+'.CCURRCODE'))
                    lcRateSign = Iif(lcRateSign = '/','*','/')
                    lcUnitSign = Iif(lcUnitSign = '/','*','/')
                    lnUntCst = Evaluate(Str(lcUntCstBase,13,3)  + lcRateSign+;
                      STR(lnPricRate,9,4)+lcUnitSign+ Str(lnCurrUnit))
                  Else
                    Local lcUnitSign,lcUnitSign ,lcRateSign
                    lcUnitSign = "/"
                    lnCurrUnit = Evaluate(lcBomFile+'.nCurrUnit')
                    If Empty(Evaluate(lcBomFile+'.CCURRCODE'))
                      lnCurrUnit = 1
                    Endif
                    lcRateSign = gfGetExSin(@lcUnitSign , Evaluate(lcBomFile+'.CCURRCODE'))
                    lcUnitSign = lcUnitSign
                    lnUntCst = Evaluate(lcBomFile+'.TotCost' + lcRateSign+;
                      STR(Evaluate(lcBomFile+'.nEXRate'),9,4)+lcUnitSign+ Str(lnCurrUnit))
                  Endif
                Else
                  lnUntCst =Evaluate(lcBomFile+'.TotCost')
                Endif
                *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[Start]
                *IF !SEEK(lcStyle+lcSzes,loBranchFormSet.lcGrpCur)
                lcSvSz = ''
                lcNewSz = lcSzes
                If !Seek(lcStyle+Padr(lcSzes,8),loBranchFormSet.lcGrpCur)
                  Dimension laSzArr[LEN(lcNewSz)]
                  Store '' To laSzArr
                  For L=1 To Len(lcNewSz)
                    laSzArr[L] =  Val(Substr(lcNewSz,L,1))
                  Endfor
                  lnVal = laSzArr[1]
                  For S=1 To Alen(laSzArr,1)
                    *Find The Samllest Value
                    For Z = S+1 To Alen(laSzArr,1)
                      If laSzArr[S] > laSzArr[Z]
                        lnVal = laSzArr[S]
                        laSzArr[S] = laSzArr[Z]
                        laSzArr[Z] = lnVal
                      Endif
                    Endfor
                  Endfor
                  For L=1 To Alen(laSzArr,1)
                    lcSvSz = lcSvSz +  Str(laSzArr[L],1)
                  Endfor
                  lcSzes = lcSvSz
                  *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[End]
                  Insert Into (loBranchFormSet.lcGrpCur) Values (lcStyle,lcSzes,lnUntCst )
                Else
                  Replace Gros_Price With Gros_Price +lnUntCst  In (loBranchFormSet.lcGrpCur)
                Endif
              Endscan
            Else
              Select (lcBomFile)
              lnUntCst = 0
              *! B609499,1 MMT 01/13/2011 Quick Order entry screen uses wrong style purchase price[Start]
              *!*          SCAN FOR LIKE(STRTRAN(EVALUATE(lcBomFile+'.cItmMask'),'*','?') , lcStyle) AND CCATGTYP = 'P'
              Scan For Like(Strtran(Evaluate(lcBomFile+'.cItmMask'),'*','?') , lcStyle) And CCATGTYP = 'P' And ccstsht_id =lcCstShtId
                *! B609499,1 MMT 01/13/2011 Quick Order entry screen uses wrong style purchase price[End]
                If !Empty(Evaluate(lcBomFile+'.CCURRCODE')) And Evaluate(lcBomFile+'.CCURRCODE') <> lcPriceCurr
                  If lcPriceCurr = Oariaapplication.BaseCurrency
                    Local lcUnitSign,lcUnitSign ,lcRateSign
                    lcUnitSign = "/"
                    lnCurrUnit = Evaluate(lcBomFile+'.nCurrUnit')
                    lcRateSign = gfGetExSin(@lcUnitSign , Evaluate(lcBomFile+'.CCURRCODE'))
                    lcUnitSign = lcUnitSign
                    lnUntCst = lnUntCst +  Evaluate(lcBomFile+'.TotCost' + lcRateSign+;
                      STR(Evaluate(lcBomFile+'.nEXRate'),9,4)+lcUnitSign+ Str(lnCurrUnit))
                  Else
                    * IF PO Currecny is not same as company base currecny, we will convert to base currency and then convert from base
                    * currecny to PO currecny
                    Local lcUnitSign,lcUnitSign ,lcRateSign
                    lcUnitSign = "/"
                    lnCurrUnit = Evaluate(lcBomFile+'.nCurrUnit')
                    lcRateSign = gfGetExSin(@lcUnitSign , Evaluate(lcBomFile+'.CCURRCODE'))
                    lcUnitSign = lcUnitSign
                    lcUntCstBase = Evaluate(lcBomFile+'.TotCost' + lcRateSign+;
                      STR(Evaluate(lcBomFile+'.nEXRate'),9,4)+lcUnitSign+ Str(lnCurrUnit))

                    *Convert to Po currecny
                    lcUnitSign = "/"
                    lnCurrUnit = 1
                    lcRateSign = gfGetExSin(@lcUnitSign , Evaluate(lcBomFile+'.CCURRCODE'))
                    lcRateSign = Iif(lcRateSign = '/','*','/')
                    lcUnitSign = Iif(lcUnitSign = '/','*','/')
                    lnUntCst = lnUntCst +  Evaluate(Str(lcUntCstBase,13,3)+ lcRateSign+;
                      STR(lnPricRate,9,4)+lcUnitSign+ Str(lnCurrUnit))
                  Endif
                Else
                  lnUntCst = lnUntCst +  Evaluate(lcBomFile+'.TotCost')
                Endif
              Endscan
              m.Gros_Price = lnUntCst
            Endif
            If Between(lnRecn,1,Reccount('Scale'))
              Go Record lnRecn In 'Scale'
            Endif

            If Ascan(laExtSz,Scale.Scale,1) > 0
              lnCounting  = Asubscript(laExtSz,Ascan(laExtSz,Scale.Scale,1),1)
              lcCounting  = Alltrim(Str(lnCounting))
            Endif
            m.Pric&lcCounting  =  m.Gros_Price
            m.NtPri&lcCounting  =  m.Gros_Price

          Endif
          lcScaleCurr = Scale.Scale
          m.FitDesc = lcFit

          Store 0 To m.QTY1, m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,m.QTY7,m.QTY8
          =gfSeek('S'+Style.Scale,'Scale')
          For lnI = 1 To Scale.Cnt


            lcCounter = Alltrim(Str(lnCounter))
            lcI = Str(lnI,1)

            m.SZ&lcCounter = Scale.SZ&lcI
            *Check if Style entered before
            m.cVenSty = ''

            *! B609194,1 MMT 03/31/2010 Fix bug of vendor Style is not displayed in PO Quick entry[Start]
            m.cVenSty = Style.cVenSty
            *! B609194,1 MMT 03/31/2010 Fix bug of vendor Style is not displayed in PO Quick entry[End]

            If !m.llAddNew And Seek(Style.Style,.oMainClass.cpoline)
              Select (.oMainClass.cpoline)
              m.cRefer= Evaluate(.oMainClass.cpoline+'.Reference')
              m.cVenSty = Evaluate(.oMainClass.cpoline+'.cVenSty')
              m.COMPLETE = Evaluate(.oMainClass.cpoline+'.Complete')
              *-- if style is entred in more than one line then each line is a separate price group
              lcPoLine = .oMainClass.cpoline
              Scan Rest While Style+Str(Lineno,6) = Style.Style
                m.QTY&lcCounter = M.QTY&lcCounter +  QTY&lcI.
                m.nTotal = m.nTotal + &lcPoLine..QTY&lcI.
              Endscan
            Endif



            Select(loBranchFormSet.lcTempCur)
            If !Seek(Padr(M.STYMAJOR,19)+M.COLOR+Padr(m.FitDesc,10),loBranchFormSet.lcTempCur)
              Append Blank
              Select(loBranchFormSet.lcTempCur)
            Else
              Select(loBranchFormSet.lcTempCur)
              If lnCounter > 8
                For lnx = 1 To 8
                  lcx= Alltrim(Str(lnx))
                  m.QTY&lcx = QTY&lcx
                Endfor
              Endif
            Endif
            If !Empty(ccstsht_id) And Empty(m.ccstsht_id)
              m.ccstsht_id= ccstsht_id
            Endif
            If !Empty(cVenSty) And Empty(m.cVenSty )
              m.cVenSty = cVenSty
            Endif

            Gather Memo Memvar
            lnCounter = lnCounter + 1
          Endfor
        Endscan



        If Between(lnRecord,1,Reccount('Scale'))
          Go lnRecord In Scale
        Endif
      Endscan
    Endfor

  Else
    loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value = ''
    Return .F.
  Endif
  Select (.oMainClass.cpoline)
Endwith

Select (loBranchFormSet.lcTempCur)
Go Top
Scatter Memo Memvar
Append Blank
Gather Memo Memvar
Replace STYMAJOR With Chr(255),;
  COLOR    With '',;
  COLORDSC With 'Totals' ,;
  FitDesc  With ''

For lnCount = 1 To Alen(laExtSz,1)
  lcI = Alltrim(Str(lnCount))
  Replace Pric&lcI  With 0,;
    NtPri&lcI With 0
Endfor

For lnC = 1 To Iif(loBranchFormSet.llMultiDime,lnMaxSz,Alen(laSize,1) )
  lcC = Alltrim(Str(lnC))
  Select (loBranchFormSet.lcTempCur)
  Sum (QTY&lcC) To lnTotal For STYMAJOR <> Chr(255)
  Go Bottom
  Replace QTY&lcC With lnTotal
Endfor
Select (loBranchFormSet.lcTempCur)
Locate

With loBranchFormSet.ariaform1
  .kbstymaj.Enabled = .F.
  .spnDisc.Enabled = .T.
Endwith


Acopy(laSize,loBranchFormSet.laSize)
Acopy(laExtSz,loBranchFormSet.laExtSz)


Select(loBranchFormSet.lcTempCur)
Locate
lfPOAddCntrSrc(loBranchFormSet)
lfPOCalcTot(loBranchFormSet)
loBranchFormSet.ariaform1.grdLines.Refresh
loBranchFormSet.ariaform1.grdLines.AfterRowColChange
Return
*:**************************************************************************
*:* Name        : lfInitPQkOrd
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : init of Quick PO Entry Screen
*:***************************************************************************
Function lfInitPQkOrd
Parameters loBranchFormSet

With loBranchFormSet.ariaform1
  .kbstymaj.Enabled = .T.
  .spnDisc.Enabled = .F.
  .txtGrsPrc.Enabled= .F.
  .txtNetPri.Enabled = .F.
  .dtpCompDate.Enabled= .F.
  .cmdSave.Enabled= .F.
  .grdLines.RecordSource = ''
Endwith
loBranchFormSet.lcTempCur = gfTempName()
loBranchFormSet.lcGrpCur= gfTempName()
loBranchFormSet.llincwip = gfGetMemVar('M_SOWIPQK')
loBranchFormSet.llExtended = gfGetMemVar('M_USEEXSSC')

Store '' To lcStySp,lcSclSp
Declare laItemSeg[1]
=gfItemMask(@laItemSeg)
For lnJ = 1 To Alen(laItemSeg,1)
  If laItemSeg[lnJ,1] = 'F'
    lcStySp = laItemSeg[lnJ,6]
    loBranchFormSet.lcStySp = lcStySp
    Loop
  Endif
  If laItemSeg[lnJ,1] = 'C'
    lcSclSp = laItemSeg[lnJ,6]
    loBranchFormSet.lcSclSp = lcSclSp
    Loop
  Endif
Endfor

Store 0  To lnClrPos,lnClrLen
Store 0 To lnSizePos,lnSizeLen
=lfGetClrD()
Store lnClrLen To loBranchFormSet.lnClrLen
Store lnClrPos To loBranchFormSet.lnClrPos
*:**************************************************************************
*:* Name        : lfvPClear
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Clear Button of Quick PO Entry Screen
*:***************************************************************************
Function lfvPClear
Parameters loBranchFormSet,llFromScreen
If llFromScreen
  loBranchFormSet.ariaform1.grdLines.RecordSource = ''
Endif
loBranchFormSet.llMultiDime = .F.
If Used(loBranchFormSet.lcTempCur)
  Select(loBranchFormSet.lcTempCur)
  Zap
Endif

*! B609499,1 MMT 01/13/2011 Quick Order entry screen uses wrong style purchase price[Start]
If Used(loBranchFormSet.loParentForm.ariaform1.mainWorkOrder.cBom)
  Select(loBranchFormSet.loParentForm.ariaform1.mainWorkOrder.cBom)
  Delete All
Endif
If Used(loBranchFormSet.loParentForm.ariaform1.mainWorkOrder.cBomHdr)
  Select(loBranchFormSet.loParentForm.ariaform1.mainWorkOrder.cBomHdr)
  Delete All
Endif
*! B609499,1 MMT 01/13/2011 Quick Order entry screen uses wrong style purchase price[End]

With loBranchFormSet.ariaform1
  .kbstymaj.Enabled = .T.
  .kbstymaj.Keytextbox.Value = ''
  .spnDisc.Enabled = .F.
  .spnDisc.Value = 0
  .txtGrsPrc.Enabled= .F.
  .txtGrsPrc.Value = 0
  .txtNetPri.Enabled = .F.
  .txtNetPri.Value = 0
  .dtpCompDate.Enabled= .F.
  .cmdSave.Enabled= .F.
  .grdLines.RecordSource = ''
  .txtStydesc.Value = ''
  .txtVenSty.Value =''
  .txtRefr.Value = ''
Endwith
Dimension loBranchFormSet.laSize[1],loBranchFormSet.laExtSz[1]
Store '' To loBranchFormSet.laSize,loBranchFormSet.laExtSz
*:**************************************************************************
*:* Name        : lfPOAddCntrSrc
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Add Control Source of Quick PO Entry Screen grid
*:***************************************************************************
Function lfPOAddCntrSrc
Parameters loBranchFormSet
With loBranchFormSet.ariaform1.grdLines
  .ColumnCount =Iif(loBranchFormSet.llMultiDime,lnMaxSz,Alen(laSize,1))+3
  .RecordSource = loBranchFormSet.lcTempCur
  .Column1.ReadOnly = .T.
  .Column1.Header1.Caption ='Colour'
  .Column1.ControlSource  = loBranchFormSet.lcTempCur +'.COLORDSC'
  .Column2.ReadOnly = .T.
  .Column2.Header1.Caption ='Fit'
  .Column2.ControlSource  = loBranchFormSet.lcTempCur +'.FitDesc'

  lnCount = 2
  For lnT = 1 To Iif(loBranchFormSet.llMultiDime,lnMaxSz,Alen(laSize,1))
    lnCount = lnCount + 1
    lcT = Alltrim(Str(lnT))
    lcCount= Alltrim(Str(lnCount))
    .Column&lcCount..ControlSource = loBranchFormSet.lcTempCur +'.QTY'+lcT
    .Column&lcCount..Header1.Caption = Evaluate(loBranchFormSet.lcTempCur +'.SZ'+lcT)
    .Column&lcCount..Enabled = .T.
    .Column&lcCount..Format = '99999'
    *! C201550,1 RAS 01/27/2013 Aria4xp - SO - Modi Sizes Aligmnent at SoQKORD Screen to be Right [begin]
    .Column&lcCount..Header1.Alignment=1
    *! C201550,1 RAS 01/27/2013 Aria4xp - SO - Modi Sizes Aligmnent at SoQKORD Screen to be Right [end]

    *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[Start]
    .Column&lcCount..Width = 50
    *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[End]

    .Column&lcCount..InputMask = '99999'
    .Column&lcCount..ReadOnly = .F.
    Bindevent(.Column&lcCount..Text1,"GotFocus",loBranchFormSet,'lfvgtstyszqty')
    Bindevent(.Column&lcCount..Text1,"LostFocus",loBranchFormSet,'lfvstyszqty')
  Endfor
  lnCount = lnCount + 1
  lcCount= Alltrim(Str(lnCount))
  .Column&lcCount..ControlSource = loBranchFormSet.lcTempCur +'.Ntotal'
  .Column&lcCount..Header1.Caption = 'Total'
  .Column&lcCount..ReadOnly = .T.
Endwith
loBranchFormSet.ariaform1.grdLines.Refresh
*:**************************************************************************
*:* Name        : lfPOAftRoColChng
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : After Row Column Change of Quick PO Entry Screen grid
*:***************************************************************************
Function lfPOAftRoColChng
Parameters loBranchFormSet
lcTempCur = loBranchFormSet.lcTempCur

loBranchFormSet.ariaform1.txtGrsPrc.Enabled = .F.
loBranchFormSet.ariaform1.spnDisc.Enabled = .F.
loBranchFormSet.ariaform1.txtRefr.Enabled = .T.
loBranchFormSet.ariaform1.txtVenSty.Enabled = .T.
=Seek(Alltrim(loBranchFormSet.ariaform1.kbstymaj.Keytextbox.Value),'Style')
lnCount = 2
For lnS = 1 To Iif(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz,Alen(loBranchFormSet.laSize,1))
  lnCount = lnCount + 1
  lcT = Alltrim(Str(lnS))
  lcCount= Alltrim(Str(lnCount))
  loBranchFormSet.ariaform1.grdLines.Column&lcCount..Header1.Caption = Evaluate(loBranchFormSet.lcTempCur +'.SZ'+lcT)
Endfor
If &lcTempCur..STYMAJOR = Chr(255)
  With loBranchFormSet.ariaform1
    .dtpCompDate.Value = {}
    .txtVenSty.Value = ''
    .txtRefr.Value = ''
    .txtVenSty.Enabled = .F.
    .txtRefr.Enabled= .F.
    .dtpCompDate.Enabled= .F.
  Endwith
Else
  With loBranchFormSet.ariaform1
    .dtpCompDate.Value = &lcTempCur..Complete
    .txtVenSty.Value = &lcTempCur..cVenSty
    .txtRefr.Value = &lcTempCur..cRefer
    .txtVenSty.Enabled = .T.
    .txtRefr.Enabled= .T.
    .dtpCompDate.Enabled= .T.
  Endwith
Endif


If loBranchFormSet.lccurrsz < 3 Or loBranchFormSet.lccurrsz > loBranchFormSet.ariaform1.grdLines.ColumnCount - 1
  Return
Endif

lcSizorder = ''

lcFPos = Alltrim(Str(loBranchFormSet.lccurrsz))
lnCurrSz  = loBranchFormSet.ariaform1.grdLines.Column&lcFPos..ControlSource
lcSclPos = lfGetPOScale(loBranchFormSet,loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption,&lcTempCur..FitDesc)
lnPosSc = 0
lnPosSc  = Ascan(loBranchFormSet.laExtSz,lcSclPos)
If lnPosSc  > 0
  lnPosSc = Asubscript(loBranchFormSet.laExtSz,lnPosSc,1)
  lcPos = Alltrim(Str(lnPosSc))
  With loBranchFormSet.ariaform1
    .txtGrsPrc.Value = &lcTempCur..Pric&lcPos
    .txtNetPri.Value = &lcTempCur..NtPri&lcPos
    .spnDisc.Value  = &lcTempCur..Discount
    .dtpCompDate.Value = &lcTempCur..Complete
    .txtVenSty.Value = &lcTempCur..cVenSty
    .txtRefr.Value = &lcTempCur..cRefer
  Endwith
Endif

If &lcTempCur..lbasonsiz
  lnAlias = Select()
  lcFullStyClrScl = Padr(&lcTempCur..STYMAJOR,loBranchFormSet.lnMajorLen) +;
    loBranchFormSet.lcStySp +Padr(&lcTempCur..Color,loBranchFormSet.lnClrLen )+;
    IIF(loBranchFormSet.llExtended,Alltrim(loBranchFormSet.lcSclSp)+lcSclPos,'')

  lcGrpCur = loBranchFormSet.lcGrpCur
  Select(lcGrpCur)
  =Seek(lcFullStyClrScl ,lcGrpCur)
  lnPrice = 0
  Scan Rest While Style+SIZES = lcFullStyClrScl For lcSizorder $ SIZES
    lnPrice = lnPrice + Gros_Price
  Endscan
  With loBranchFormSet.ariaform1
    .txtGrsPrc.Value = lnPrice
    .spnDisc.Value = 0
    .spnDisc.Enabled = .F.
    .txtNetPri.Value  =lnPrice
  Endwith
  Select (lnAlias)
Endif
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
Function lfvPOStySzqty
Parameters loBranchFormSet,loParentForm
Private lcFPos
lcTempCur = loBranchFormSet.lcTempCur
lcFPos = Alltrim(Str(loBranchFormSet.lccurrsz))
lnCurrSz  = loBranchFormSet.ariaform1.grdLines.Column&lcFPos..ControlSource
Private lnLineTot,lnCount,lnRecno,m.AddSty,lnIncrm

Select (lcTempCur)
If STYMAJOR = Chr(255)
  Replace &lnCurrSz With loBranchFormSet.lnOldVAlue   In (lcTempCur)
  Return
Endif
If Empty(loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption)
  Replace &lnCurrSz With 0 In (lcTempCur)
  Return
Endif


If loBranchFormSet.lnOldVAlue  #  Evaluate(lnCurrSz)
  If Evaluate(lnCurrSz) < 0
    *--Can not accept negative values
    =gfModalGen('TRM42000B40011','DIALOG')

    Replace &lnCurrSz With loBranchFormSet.lnOldVAlue
    Return
  Endif
  If Evaluate(lnCurrSz) > 99999
    *--Can not exceed 999
    *-- 40171 : ð cannot exceeds ð
    =gfModalGen('TRM40171B00000','DIALOG','Size Quantity|99999.')
    Replace &lnCurrSz With loBranchFormSet.lnOldVAlue
    Return
  Endif

  m.AddSty = Padr(STYMAJOR,loBranchFormSet.lnMajorLen) +loBranchFormSet.lcStySp +Substr(Color,1,loBranchFormSet.lnClrLen)
  Select (lcTempCur)

  lcSclPos = lfGetPOScale(loBranchFormSet,loBranchFormSet.ariaform1.grdLines.Column&lcFPos..Header1.Caption,&lcTempCur..FitDesc)
  *-- Assign a style to a location if not Assigned befor

  If !Seek(Padr(m.AddSty+Iif(loBranchFormSet.llExtended,Alltrim(loBranchFormSet.lcSclSp)+lcSclPos,'') ,19)+loParentForm.ariaform1.cntPoHeader.kbWarehouse.Keytextbox.Value+Space(10),'StyDye')


    *E300408,1 Message : 40012
    *E300408,1 Style/color xxx is not assigned to warehouse xxx
    *E300408,1 Button : 40002
    *E300408,1 Add Reenter

    If gfModalGen('QRM40012B40002','ALERT',Trim(m.AddSty+Iif(loBranchFormSet.llExtended,Alltrim(loBranchFormSet.lcSclSp)+lcSclPos,'') )+'|'+Trim(loParentForm.ariaform1.cntPoHeader.kbWarehouse.Keytextbox.Value))=1

      Do gpAdStyWar With Padr(m.AddSty+Iif(loBranchFormSet.llExtended,Alltrim(loBranchFormSet.lcSclSp)+lcSclPos,'') ,19),Space(10),loParentForm.ariaform1.cntPoHeader.kbWarehouse.Keytextbox.Value

    Else
      Replace &lnCurrSz With 0
      Return
    Endif
  Endif


  Select (lcTempCur)
  lnLineTot = 0
  For lnT = 1 To Iif(loBranchFormSet.llMultiDime,loBranchFormSet.lnMaxSz,Alen(loBranchFormSet.laSize,1) )
    lcT = Alltrim(Str(lnT))
    lnLineTot = lnLineTot + Evaluate(lcTempCur+'.QTY'+lcT)
  Endfor
  Replace nTotal With lnLineTot
Endif
lfPOCalcTot(loBranchFormSet)
*:**************************************************************************
*:* Name        : lfPOCalcTot
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Calculate Totals(PO Quick Order Entyr)
*:***************************************************************************
Function lfPOCalcTot
Parameters loBarnFormSet

Select (loBarnFormSet.lcTempCur)
lnRecn = Recno()
Go Bottom

For lnC = 1 To Iif(loBarnFormSet.llMultiDime ,loBarnFormSet.lnMaxSz,Alen(loBarnFormSet.laSize,1))
  lcC = Alltrim(Str(lnC))
  Select (loBarnFormSet.lcTempCur)
  Sum (QTY&lcC) To lnTotal For STYMAJOR <> Chr(255)
  Go Bottom
  If STYMAJOR = Chr(255)
    Replace QTY&lcC With lnTotal
  Endif
Endfor

Select (loBarnFormSet.lcTempCur)
Sum nTotal To lnTotal For STYMAJOR <> Chr(255)
Go Bottom
If STYMAJOR = Chr(255)
  Replace nTotal With lnTotal
Endif
loBarnFormSet.ariaform1.cmdSave.Enabled = lnTotal > 0
*

Select (loBarnFormSet.lcTempCur)
If Between(lnRecn ,1,Reccount())
  Go lnRecn
Endif
loFormSet =  loBarnFormSet.loParentForm
lcTempCur = loBarnFormSet.lcTempCur
lccPoLine = loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1.Parent.oMainClass.cpoline
If nTotal = 0 And !&lcTempCur..llAddNew And  Seek(Alltrim(&lcTempCur..STYMAJOR),lccPoLine)
  loBarnFormSet.ariaform1.cmdSave.Enabled = .T.
Endif
*:**************************************************************************
*:* Name        : lfPOCrtTempFiles
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Create Temp Files(PO Quick Order Entry)
*:***************************************************************************
Function lfPOCrtTempFiles
Parameters loBranchFormSet
Dimension laTblFld[13,4]
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
*! B610000,1 HIA 07/12/2012 Color description, not displayed compelteded [T20120703.0019][Begin]
*laTblFld[3,3] = 20
laTblFld[3,3] = 30
*! B610000,1 HIA 07/12/2012 Color description, not displayed compelteded [T20120703.0019][End]
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

If loBranchFormSet.llMultiDime

  For lnI = 1 To lnMaxSz
    lcI = Alltrim(Str(lnI))
    Dimension laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'SZ'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "C"
    laTblFld[ALEN(laTblFld,1),3] = 5
    laTblFld[ALEN(laTblFld,1),4] = 0
  Endfor

  For lnI = 1 To lnMaxSz
    lcI = Alltrim(Str(lnI))
    Dimension laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'QTY'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "N"
    laTblFld[ALEN(laTblFld,1),3] = 5
    laTblFld[ALEN(laTblFld,1),4] = 0
  Endfor

Else
  For lnI = 1 To Alen(laSize,1)
    lcI = Alltrim(Str(lnI))
    Dimension laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'SZ'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "C"
    laTblFld[ALEN(laTblFld,1),3] = 5
    laTblFld[ALEN(laTblFld,1),4] = 0
  Endfor

  For lnI = 1 To Alen(laSize,1)
    lcI = Alltrim(Str(lnI))
    Dimension laTblFld[ALEN(laTblFld,1)+1,4]
    laTblFld[ALEN(laTblFld,1),1] = 'QTY'+lcI
    laTblFld[ALEN(laTblFld,1),2] = "N"
    laTblFld[ALEN(laTblFld,1),3] = 5
    laTblFld[ALEN(laTblFld,1),4] = 0
  Endfor
Endif

For lnCount = 1 To Alen(laExtSz,1)
  lcI = Alltrim(Str(lnCount))
  Dimension laTblFld[ALEN(laTblFld,1)+1,4]
  laTblFld[ALEN(laTblFld,1),1] = 'Pric'+lcI
  laTblFld[ALEN(laTblFld,1),2] = "N"
  laTblFld[ALEN(laTblFld,1),3] = 7
  laTblFld[ALEN(laTblFld,1),4] = 2
Endfor

For lnCount = 1 To Alen(laExtSz,1)
  lcI = Alltrim(Str(lnCount))
  Dimension laTblFld[ALEN(laTblFld,1)+1,4]
  laTblFld[ALEN(laTblFld,1),1] = 'NtPri'+lcI
  laTblFld[ALEN(laTblFld,1),2] = "N"
  laTblFld[ALEN(laTblFld,1),3] = 7
  laTblFld[ALEN(laTblFld,1),4] = 2
Endfor



=gfCrtTmp(loBranchFormSet.lcTempCur ,@laTblFld,'STYMAJOR+COLOR+FitDesc',loBranchFormSet.lcTempCur )


lnFl = 0

lnFl = lnFl + 1
Dimension laTblFld[lnFl,4]
laTblFld[lnFl,1] = "STYLE"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 19
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
Dimension laTblFld[lnFl,4]
laTblFld[lnFl,1] = "SIZES"
laTblFld[lnFl,2] = "C"
laTblFld[lnFl,3] = 8
laTblFld[lnFl,4] = 0

lnFl = lnFl + 1
Dimension laTblFld[lnFl,4]
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
Function lfPOClose
Parameters loBranchFormSet

llClose  = .F.
lcTempCur = loBranchFormSet.lcTempCur
lnRecno = Recno(lcTempCur)
Select (lcTempCur)
Locate For STYMAJOR = Chr(255)
If Found() And &lcTempCur..nTotal > 0

  llClose = (gfModalGen('QRM38093B32005','ALERT','Quantities have been entered';
    +'|The Quick Order Entry screen')=1)
Else
  If Found() And &lcTempCur..nTotal = 0

    *! B609499,1 MMT 01/13/2011 Quick Order entry screen uses wrong style purchase price[Start]
    Select(loBranchFormSet.loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1.Parent.oMainClass.cpoline)
    loBranchFormSet.Hide ()
    loBranchFormSet.loParentForm.Activate()
    loBranchFormSet.loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.grdPODetails.Refresh()
    loBranchFormSet.loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.grdPODetails.AfterRowColChange()
    loBranchFormSet.loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.mSetobjstatus(!Eof(),Scale.Cnt)
    *! B609499,1 MMT 01/13/2011 Quick Order entry screen uses wrong style purchase price[End]


    loBranchFormSet.Release
  Endif
Endif
If llClose
  *! B609499,1 MMT 01/13/2011 Quick Order entry screen uses wrong style purchase price[Start]
  Select(loBranchFormSet.loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1.Parent.oMainClass.cpoline)
  loBranchFormSet.Hide ()
  loBranchFormSet.loParentForm.Activate()
  loBranchFormSet.loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.grdPODetails.Refresh()
  loBranchFormSet.loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.grdPODetails.AfterRowColChange()
  loBranchFormSet.loParentForm.ariaform1.PgfPOStyle.Page2.cntDetailFolder.mSetobjstatus(!Eof(),Scale.Cnt)
  *! B609499,1 MMT 01/13/2011 Quick Order entry screen uses wrong style purchase price[End]

  loBranchFormSet.Release
Else
  If Between(lnRecno ,1,Reccount(lcTempCur))
    Go lnRecno  In (lcTempCur)
  Endif
  Return
Endif


Function lfUpdateCmpDate
Parameters loBranchFormSet
Replace Complete With loBranchFormSet.ariaform1.dtpCompDate.Value In (loBranchFormSet.lcTempCur)
*:**************************************************************************
*:* Name        : lfUpdateVenSty
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : VenDor Style Validation(PO Quick Order Entry)
*:***************************************************************************
Function lfUpdateVenSty
Parameters loBranchFormSet
Replace cVenSty With loBranchFormSet.ariaform1.txtVenSty.Value In (loBranchFormSet.lcTempCur)
*:**************************************************************************
*:* Name        : lfUpdateRef
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Refrence Validation(PO Quick Order Entry)
*:***************************************************************************
Function lfUpdateRef
Parameters loBranchFormSet
Replace cRefer With loBranchFormSet.ariaform1.txtRefr.Value In (loBranchFormSet.lcTempCur)
*:**************************************************************************
*:* Name        : lfGetPOScale
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : get Scale of sent size
*:***************************************************************************
Function lfGetPOScale
Parameters loBarnFormSet,lcScale,lcFit
lcRetScale = ''
=Seek('S'+Substr(Style.Scale,1,loBarnFormSet.lnScaleLen),'Scale')
Select Scale
llFoundScl = .F.
Scan Rest While Type+Scale+prepak = 'S'+ Substr(Style.Scale,1,loBarnFormSet.lnScaleLen) For;
    IIF(loBarnFormSet.llMultiDime,Alltrim(CDIM1)==Alltrim(lcFit),.T.)
  For lnI = 1 To 8
    lcI = Alltrim(Str(lnI,1))
    If Alltrim(Upper(SZ&lcI)) = Alltrim(Upper(Padr(lcScale,5)))
      lcSizorder =lcI
      lcRetScale = Scale.Scale
      llFoundScl = .T.
      Exit
    Endif
  Endfor
  If llFoundScl
    Exit
  Endif
Endscan
Return lcRetScale
*:**************************************************************************
*:* Name        : lfvPOSave
*:* Developer   : Mariam Mazhar{MMT}
*:* Date        : 03/11/2009
*:* Purpose     : Save button function of PO Quick Order entry
*:***************************************************************************
Function lfvPOSave
Parameters loBranchFormSet
lcStyOrd = Order('STYLE')
Set Order To Style In Style
Select (loBranchFormSet.lcTempCur)
Go Bottom
*!*	IF StyMajor = CHR(255)
*!*	  IF ntotal = 0
*!*	    return
*!*	  ENDIF
*!*	ENDIF
loFormSet =  loBranchFormSet.loParentForm
Local lnI, lcTmpPosHdr, lcTmpPoLn, lcLineKey,laOldECst, laNewECst, laOldFCst, laNewFCst
Private lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth,lnCurrUnt1, lnCurrUnt2,llConCst
Dimension laOldECst[7], laNewECst[7], laOldFCst[7], laNewFCst[7]
Store 0  To laOldECst, laNewECst, laOldFCst, laNewFCst
llMultiCur = loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1.Parent.oMainClass.lMultiCurrency
lcGrpCur = loBranchFormSet.lcGrpCur
lcStySp=  loBranchFormSet.lcStySp
lcSclSp = loBranchFormSet.lcSclSp
lnClrLen =loBranchFormSet.lnClrLen
lnMajorLen = loBranchFormSet.lnMajorLen
llExtended = loBranchFormSet.llExtended
lcPoLine = loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1.Parent.oMainClass.cpoline
m.Cstytype  = 'P'
m.CBUSDOCU  = 'P'
m.PO        = loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1.Parent.oKeyHeaderClass.kbPONo.Keytextbox.Value
m.TRANCD    = '1'
m.VENDOR    = loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder.oKeyHeaderClass.kbVendor.Keytextbox.Value
m.cWareCode = loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1.Parent.oKeyHeaderClass.kbWarehouse.Keytextbox.Value
m.SCALE     = Style.Scale
m.prepak    = Style.CbuyPrePk
m.cVenSty   = Style.cVenSty
m.cStyGrade = Style.cStyGrade
m.cinvtype = '0001'
m.cUomCode = "EAC"
m.Shipvia = Alltrim(loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder.oHeaderClass.cboShipVia.Value)
lcPriceCurr   = loFormSet.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cntMultiCurrency.KbPriceCurrency.Keytextbox.Value
lnPricRate    = loFormSet.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cntMultiCurrency.txtPriceRate.Value
*-- Get the selected duty currency in the header folder
lcDutyCurr    = loFormSet.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cntMultiCurrency.KbDutyCurrency.Keytextbox.Value
lnDutyRate    = loFormSet.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cntMultiCurrency.txtDutyRate.Value
lcTempCur = loBranchFormSet.lcTempCur
Select (lcTempCur)
Go Top
loDetailSection = loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder
lcPosHdr = loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1.Parent.oMainClass.cposhdr
lcDutCurr = loFormSet.ariaform1.PgfPOStyle.Page1.cntheaderFolder.cntMultiCurrency.KbDutyCurrency.Keytextbox.Value

lccPoLine = loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder.editregion1.Parent.oMainClass.cpoline
Scan For STYMAJOR <> Chr(255)&&and nTotal>0
  m.ccstsht_id = &lcTempCur..ccstsht_id
  m.Reference = &lcTempCur..cRefer
  m.cVenSty = &lcTempCur..cVenSty
  lnIncrmnt = 0
  For lnQkCnt = 1 To Alen(loBranchFormSet.laExtSz,1)

    If Iif(loBranchFormSet.llMultiDime ,loBranchFormSet.lnMaxSz < lnIncrmnt ,.F.)
      lnIncrmnt = 0
    Endif

    Select (lcTempCur)
    m.Style = Padr(Padr(STYMAJOR ,loBranchFormSet.lnMajorLen )+lcStySp +Padr(Color,lnClrLen)+Iif(loBranchFormSet.llExtended,Alltrim(lcSclSp) +loBranchFormSet.laExtSz[lnQkCnt,1],''),19)
    =Seek(m.Style,'Style')
    =Seek('S'+loBranchFormSet.laExtSz[lnQkCnt,1],'SCALE')
    If Alltrim(Scale.CDIM1) <> Alltrim(&lcTempCur..FitDesc)
      Loop
    Endif

    Select (lcTempCur)
    Store 0 To m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TOTQTY,;
      m.nICost1,m.nICost2,m.nICost3,m.nICost4,m.nICost5,m.nICost6,m.nICost7,;
      m.nECost1,m.nECost2,m.nECost3,m.nECost4,m.nECost5,m.nECost7,m.nECost6
    Store 0  To loDetailSection.nFcost1, loDetailSection.nFcost2, loDetailSection.nFcost3, loDetailSection.nFcost4,loDetailSection.nFcost5, loDetailSection.nFcost6, loDetailSection.nFcost7
    Store '' To lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
    Store 1  To lnCurrUnt1, lnCurrUnt2


    Dimension laNewECst[7], laNewFCst[7], laOldECst [7], laOldFCst[7]
    Store 0 To laNewECst, laNewFCst, laOldECst, laOldFCst
    If nTotal>0 Or (nTotal = 0 And !&lcTempCur..llAddNew And  Seek(m.Style,lccPoLine))
      Select(lcGrpCur)
      If Seek(m.Style,lcGrpCur)
        Dimension laSizesArr[1]
        Store '' To  laSizesArr
        Scan Rest While Style+SIZES = m.Style &&FOR lcSizorder $ SIZES
          lnRecNumber = Recno()
          Select (lcTempCur)
          Store 0  To loDetailSection.nFcost1, loDetailSection.nFcost2, loDetailSection.nFcost3, loDetailSection.nFcost4,loDetailSection.nFcost5, loDetailSection.nFcost6, loDetailSection.nFcost7
          Store '' To lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth,lcSizes
          Store 1  To lnCurrUnt1, lnCurrUnt2
          Store 0 To m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TOTQTY

          *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[Start]
          lcCurSizes = Alltrim(&lcGrpCur..SIZES)
          llfoundLess = .F.
          If !Empty(lcCurSizes )
            llfoundLess = .F.
            For S=1 To Len(lcCurSizes)
              llSzAdd = .F.
              For lnA = 1 To Alen(laSizesArr,1)
                If Substr(lcCurSizes,S,1) $ laSizesArr[lnA]
                  llSzAdd = .T.
                  Exit
                Endif
              Endfor
              If llSzAdd
                Loop
              Endif
              If Seek(m.Style,lcGrpCur)
                Select(lcGrpCur)
                Scan Rest While Style+SIZES = m.Style For Substr(lcCurSizes,S,1) $ SIZES And Len(Alltrim(&lcGrpCur..SIZES)) < Len(lcCurSizes)
                  llfoundLess = .T.
                  Exit
                Endscan
                If llfoundLess
                  Exit
                Endif
              Endif
            Endfor
          Endif
          If !llfoundLess
            Select(lcGrpCur)
            If Between(lnRecNumber,1,Reccount())
              Go Record lnRecNumber
            Endif
          Endif
          Select (lcTempCur)
          *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[End]

          For lnJ = lnIncrmnt + 1 To lnIncrmnt + loBranchFormSet.laExtSz[lnQkCnt,2]
            lcJ = Allt(Str(lnJ))
            lcQ = Str(lnJ - lnIncrmnt,1)
            If !(lcQ $ &lcGrpCur..SIZES)
              Loop
            Endif
            lloop =.F.
            For lnW = 1 To Alen(laSizesArr,1)
              If lcQ $ laSizesArr[lnW]
                lloop =.T.
                Exit
              Endif
            Endfor
            If lloop
              Loop
            Endif
            lcSizes = lcSizes + lcQ
            m.TOTQTY = m.TOTQTY +  QTY&lcJ
            m.QTY&lcQ = QTY&lcJ
          Endfor
          If m.TOTQTY = 0 Or Empty(lcSizes) And !((m.TOTQTY = 0 And !&lcTempCur..llAddNew And  Seek(m.Style,lccPoLine)))
            *B610473,1 HIA 15/08/2013 T20130805.0001 - Aria4xp - Style Purchase Order - Quick Order Entry screen not updating PO detail screen [Begin]
            *! B610473,2 HIA 10/10/2013 Error in Quick Order Entry screen when style has cost sheet per size[T20130805.0001][Start]
            *lnIncrmnt = lnIncrmnt  + loBranchFormSet.laExtSz[lnQkCnt,2]
            *! B610473,2 HIA 10/10/2013 Error in Quick Order Entry screen when style has cost sheet per size[T20130805.0001][End]
            *B610473,1 HIA 15/08/2013 T20130805.0001 - Aria4xp - Style Purchase Order - Quick Order Entry screen not updating PO detail screen [End]
            Loop
          Endif
          loDetailSection.mGetbomCurr(&lcTempCur..ccstsht_id)
          loDetailSection.nFcost1 = loDetailSection.laBomInfo[1,5]
          loDetailSection.nFcost2 = loDetailSection.laBomInfo[2,5]
          loDetailSection.nFcost3 = loDetailSection.laBomInfo[3,5]
          loDetailSection.nFcost4 = loDetailSection.laBomInfo[4,5]
          loDetailSection.nFcost5 = loDetailSection.laBomInfo[5,5]
          loDetailSection.nFcost6 = loDetailSection.laBomInfo[6,5]
          loDetailSection.nFcost7 = loDetailSection.laBomInfo[7,5]
          m.nFcost1 = loDetailSection.nFcost1
          m.nFcost2 = loDetailSection.nFcost2
          m.nFcost3 = loDetailSection.nFcost3
          m.nFcost4 = loDetailSection.nFcost4
          m.nFcost5 = loDetailSection.nFcost5
          m.nFcost6 = loDetailSection.nFcost6
          m.nFcost7 = loDetailSection.nFcost7
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
          m.COMPLETE  = Complete
          m.cStyGrade = Style.cStyGrade
          Select (lcGrpCur)
          =Seek(m.Style,lcGrpCur)

          *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[Start]
          *SUM GROS_PRICE REST WHILE STYLE+SIZES = m.Style FOR ALLTRIM(lcSizes) $ SIZES  TO m.GROS_PRICE
          Store '' To lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8
          For lnW =1 To Len(Alltrim(lcSizes))
            lcW = Str(lnW,1)
            Store Substr(Alltrim(lcSizes),lnW,1) To lcsZ&lcW
          Endfor
          Sum Gros_Price Rest While Style+SIZES = m.Style For Iif(!Empty(lcSz1),lcSz1 $ SIZES,.T.) And ;
            IIF(!Empty(lcSz2),lcSz2 $ SIZES,.T.) And ;
            IIF(!Empty(lcSz3),lcSz3 $ SIZES,.T.) And ;
            IIF(!Empty(lcSz4),lcSz4 $ SIZES,.T.) And ;
            IIF(!Empty(lcSz5),lcSz5 $ SIZES,.T.) And ;
            IIF(!Empty(lcSz6),lcSz6 $ SIZES,.T.) And ;
            IIF(!Empty(lcSz7),lcSz7 $ SIZES,.T.) And ;
            IIF(!Empty(lcSz8),lcSz8 $ SIZES,.T.)  To m.Gros_Price
          *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[End]

          If Empty(laSizesArr[1])
            laSizesArr[1] = lcSizes
          Else
            Dimension  laSizesArr[ALEN(laSizesArr,1)+1]
            laSizesArr[ALEN(laSizesArr,1)] = lcSizes
          Endif
          m.nICost1      = m.Gros_Price
          m.SCALE       = Style.Scale
          m.prepak      = Style.CbuyPrePk
          Store m.Gros_Price To loDetailSection.nFcost1, m.nFcost1
          m.nICost1 = loDetailSection.laBomInfo[1,4]
          m.nICost2 = loDetailSection.laBomInfo[2,4]
          m.nICost3 = loDetailSection.laBomInfo[3,4]
          m.nICost4 = loDetailSection.laBomInfo[4,4]
          m.nICost5 = loDetailSection.laBomInfo[5,4]
          m.nICost6 = loDetailSection.laBomInfo[6,4]
          m.nICost7 = loDetailSection.laBomInfo[7,4]

          *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[Start]
          Store m.Gros_Price To  m.nICost1
          *! C201115,2 MMT 04/02/2009 Convert PO Quick Order Entry Screen to Aria4[End]

          Select &lcPoLine
          Locate For Style  = M.Style And Complete = m.COMPLETE And Gros_Price = m.Gros_Price
          If !Found() Or &lcTempCur..llAddNew
            loDetailSection.nPoLineNo  =  loDetailSection.nPoLineNo + 1
            m.LINENO = loDetailSection.nPoLineNo
            Insert Into (lcPoLine) From Memvar
            Select(loDetailSection.oMainClass.cposhdr)
            Replace nStyOrder With nStyOrder + m.TOTQTY,;
              OPEN      With Open      + m.TOTQTY In (lcPosHdr )

            For lnI = 1 To 7
              lcI = Str(lnI,1)
              Store m.nICost&lcI To laOldECst[lnI], laNewECst[lnI]
              Store m.nFCost&lcI To laOldFCst[lnI], laNewFCst[lnI]
            Endfor
            loDetailSection.oMainClass.mCalEstCst(m.TOTQTY, 0,@laOldECst,@laNewECst,;
              @laOldFCst,@laNewFCst,llMultiCur,.T.,lcPriceCurr,lnPricRate,;
              lcDutyCurr)


          Else
            *--Update laData array
            *(m.TotQty = 0 AND !&lcTempCur..llAddNew AND  SEEK(m.stYle,lccPoLine))
            For lnI = 1 To 7
              lcI = Str(lnI,1)
              Store nICost&lcI To laOldECst[lnI]
              Store m.nICost&lcI To  laNewECst[lnI]
              Store nFCost&lcI To laOldFCst[lnI]
              Store m.nICost&lcI To  laNewFCst[lnI]
            Endfor

            lnOldQty = TOTQTY
            If (m.TOTQTY = 0 And !&lcTempCur..llAddNew )
              Delete
              Replace nStyOrder With nStyOrder -lnOldQty  ,;
                OPEN      With Open      -lnOldQty In (lcPosHdr )
            Else
              If &lcTempCur..llAddNew
                Append Blank
                lnOldQty  = 0
                For lnI = 1 To 7
                  lcI = Str(lnI,1)
                  Store m.nICost&lcI To laOldECst[lnI]
                  Store m.nICost&lcI To  laNewECst[lnI]
                  Store m.nFCost&lcI To laOldFCst[lnI]
                  Store m.nICost&lcI To  laNewFCst[lnI]
                Endfor

              Endif
              Gather  Memo  Memvar
              Replace nStyOrder With nStyOrder -lnOldQty +  m.TOTQTY  ,;
                OPEN      With Open      -lnOldQty +  m.TOTQTY In (lcPosHdr )
            Endif
            loDetailSection.oMainClass.mCalEstCst(m.TOTQTY, lnOldQty ,@laOldECst,@laNewECst,;
              @laOldFCst,@laNewFCst,llMultiCur,.T.,lcPriceCurr,lnPricRate,;
              lcDutyCurr)

          Endif
          Select(lcGrpCur)
          If Between(lnRecNumber,1,Reccount())
            Go Record lnRecNumber
          Endif
        Endscan
      Else
        Select (lcTempCur)
        Store 0  To loDetailSection.nFcost1, loDetailSection.nFcost2, loDetailSection.nFcost3, loDetailSection.nFcost4,loDetailSection.nFcost5, loDetailSection.nFcost6, loDetailSection.nFcost7
        Store '' To lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
        Store 1  To lnCurrUnt1, lnCurrUnt2
        For lnJ = lnIncrmnt + 1 To lnIncrmnt + loBranchFormSet.laExtSz[lnQkCnt,2]
          lcJ = Allt(Str(lnJ))
          lcQ = Str(lnJ - lnIncrmnt,1)
          m.TOTQTY = m.TOTQTY +  QTY&lcJ
          m.QTY&lcQ = QTY&lcJ
        Endfor
        If m.TOTQTY = 0 And !((m.TOTQTY = 0 And !&lcTempCur..llAddNew And  Seek(m.Style,lccPoLine)))
          *B610473,1 HIA 15/08/2013 T20130805.0001 - Aria4xp - Style Purchase Order - Quick Order Entry screen not updating PO detail screen [Begin]
          lnIncrmnt = lnIncrmnt  + loBranchFormSet.laExtSz[lnQkCnt,2]
          *B610473,1 HIA 15/08/2013 T20130805.0001 - Aria4xp - Style Purchase Order - Quick Order Entry screen not updating PO detail screen [End]
          Loop
        Endif
        loDetailSection.mGetbomCurr(&lcTempCur..ccstsht_id)
        loDetailSection.nFcost1 = loDetailSection.laBomInfo[1,5]
        loDetailSection.nFcost2 = loDetailSection.laBomInfo[2,5]
        loDetailSection.nFcost3 = loDetailSection.laBomInfo[3,5]
        loDetailSection.nFcost4 = loDetailSection.laBomInfo[4,5]
        loDetailSection.nFcost5 = loDetailSection.laBomInfo[5,5]
        loDetailSection.nFcost6 = loDetailSection.laBomInfo[6,5]
        loDetailSection.nFcost7 = loDetailSection.laBomInfo[7,5]
        m.nFcost1 = loDetailSection.nFcost1
        m.nFcost2 = loDetailSection.nFcost2
        m.nFcost3 = loDetailSection.nFcost3
        m.nFcost4 = loDetailSection.nFcost4
        m.nFcost5 = loDetailSection.nFcost5
        m.nFcost6 = loDetailSection.nFcost6
        m.nFcost7 = loDetailSection.nFcost7
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
        m.COMPLETE  = Complete
        m.cStyGrade = Style.cStyGrade
        m.SCALE       = Style.Scale
        m.prepak      = Style.CbuyPrePk
        *STORE m.Gros_Price TO loDetailSection.nFCost1, m.nFCost1
        m.nICost1 = loDetailSection.laBomInfo[1,4]
        m.nICost2 = loDetailSection.laBomInfo[2,4]
        m.nICost3 = loDetailSection.laBomInfo[3,4]
        m.nICost4 = loDetailSection.laBomInfo[4,4]
        m.nICost5 = loDetailSection.laBomInfo[5,4]
        m.nICost6 = loDetailSection.laBomInfo[6,4]
        m.nICost7 = loDetailSection.laBomInfo[7,4]
        *! B609683,1 MMT 10/04/2011 Fix bug of wrong gross price while adding line from PO Quick Order entry [START]
        *m.GROS_PRICE =  m.nICost1
        m.Gros_Price =  m.nFcost1
        *! B609683,1 MMT 10/04/2011 Fix bug of wrong gross price while adding line from PO Quick Order entry [END]
        Select &lcPoLine
        Locate For Style  = M.Style And Complete = m.COMPLETE And Gros_Price = m.Gros_Price
        If !Found() Or &lcTempCur..llAddNew
          loDetailSection.nPoLineNo  =  loDetailSection.nPoLineNo + 1
          m.LINENO = loDetailSection.nPoLineNo
          Insert Into (lcPoLine) From Memvar
          Select(loDetailSection.oMainClass.cposhdr)
          Replace nStyOrder With nStyOrder + m.TOTQTY,;
            OPEN      With Open      + m.TOTQTY In (lcPosHdr )

          For lnI = 1 To 7
            lcI = Str(lnI,1)
            Store m.nICost&lcI To laOldECst[lnI], laNewECst[lnI]
            Store m.nFCost&lcI To laOldFCst[lnI], laNewFCst[lnI]
          Endfor
          loDetailSection.oMainClass.mCalEstCst(m.TOTQTY, 0,@laOldECst,@laNewECst,;
            @laOldFCst,@laNewFCst,llMultiCur,.T.,lcPriceCurr,lnPricRate,;
            lcDutyCurr)


        Else
          *--Update laData array
          For lnI = 1 To 7
            lcI = Str(lnI,1)
            Store nICost&lcI To laOldECst[lnI]
            Store m.nICost&lcI To  laNewECst[lnI]
            Store nFCost&lcI To laOldFCst[lnI]
            Store m.nICost&lcI To  laNewFCst[lnI]
          Endfor


          lnOldQty = TOTQTY
          *(m.TotQty = 0 AND !&lcTempCur..llAddNew AND  SEEK(m.stYle,lccPoLine))
          If  m.TOTQTY = 0 And !&lcTempCur..llAddNew
            Delete
            Replace nStyOrder With nStyOrder -lnOldQty  ,;
              OPEN      With Open      -lnOldQty In (lcPosHdr )
          Else
            If &lcTempCur..llAddNew  And m.TOTQTY <> 0
              Append Blank
              lnOldQty  = 0
              For lnI = 1 To 7
                lcI = Str(lnI,1)
                Store m.nICost&lcI To laOldECst[lnI]
                Store m.nICost&lcI To  laNewECst[lnI]
                Store m.nFCost&lcI To laOldFCst[lnI]
                Store m.nICost&lcI To  laNewFCst[lnI]
              Endfor
            Endif
            Gather  Memo Memvar
            Replace nStyOrder With nStyOrder -lnOldQty +  m.TOTQTY  ,;
              OPEN      With Open      -lnOldQty +  m.TOTQTY In (lcPosHdr )
          Endif
          loDetailSection.oMainClass.mCalEstCst(m.TOTQTY, lnOldQty ,@laOldECst,@laNewECst,;
            @laOldFCst,@laNewFCst,llMultiCur,.T.,lcPriceCurr,lnPricRate,;
            lcDutyCurr)


        Endif
      Endif
    Endif
    lnIncrmnt = lnIncrmnt + loBranchFormSet.laExtSz[lnQkCnt,2]
  Endfor

Endscan
*-- Clear screen
lfvPClear(loBranchFormSet,.T.)
loFormSet.ariaform1.PgfPOStyle.Page2.cntDetailFolder.grdPODetails.Refresh()
Set Order To &lcStyOrd In Style
*! C201115,1 MMT 03/11/2009 Convert PO Quick Order Entry Screen to Aria4      [End]


*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[Start]
*:**************************************************************************
*:* Name        : lfPRICCODE
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 04/05/2009
*:* Purpose     : get Customer Price code for Quick Order entry screen
*:***************************************************************************
Function lfQKPRICCODE
Local lcSlct,lcPricCode,lcSvKey
lcSlct = Select()
lcPricCode = ''
lcCurrCode=loParentForm.ariaform1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value
If !Used('Customer_A')
  gfOpenTable('Customer','Customer','SH','Customer_A')
Endif
If !Used('CSTPRICE')
  gfOpenTable('CSTPRICE','CSTPRICE','SH','CSTPRICE')
Endif
If !Used('CSTPRICH')
  gfOpenTable('CSTPRICH','CSTPRICH','SH','CSTPRICH')
Endif


Select Customer_A
lcAccount = &lcOrdHdr..ACCOUNT

lcSvKey = Evaluate(Key())
=gfSeek('M'+lcAccount,'Customer_A')

Local lnI,lcI,lcNoDtPCod

*-- Loop through all price codes
*-   pick the first price code with suitable date,
*-   if no one, pick the first one with no valid dates , otherwise return chr(255)
lcNoDtPCod = Chr(255)

*- Download needed lines to seek in from cstprice and cstprich sql files
Select CSTPRICE
=gfsetOrder('CSTYCODE')
=gfSeek(lcSeekSty,'CSTPRICE')
=gfsetOrder('CSTPRICE')

Private lcStyClr
lcStyClr = Padr(Substr(lcSeekSty,1,lnClrPos+lnClrLen-1),19)
Select CSTPRICH
=gfsetOrder('STYLE')
=gfSeek(lcStyClr,'CSTPRICH')
=gfsetOrder('CSTPRICH')

For lnI = 1 To 15
  lcI = Iif(lnI = 1 , '' , Padl(lnI,2,'0') )

  If !Empty(Customer_A.PRICCODE&lcI).And. gfSeek(Customer_A.PRICCODE&lcI+lcCurrCode+lcSeekSty,'CSTPRICE') ;
      .And. gfSeek(Customer_A.PRICCODE&lcI+lcCurrCode+lcStyClr,'CSTPRICH')
    If Empty(CSTPRICH.DVLDPRTO)
      *- Get no valid date price code
      lcNoDtPCod = Upper(Customer_A.PRICCODE&lcI)
    Else
      *- Compare valid  prices for Banach based on setup ( Entered, Start or Complete Dates)
      If Between(ldChkDate,CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)
        Exit
      Endif

    Endif
  Endif
Endfor

lcPricCode = Iif(lnI < 16 , Upper(Customer_A.PRICCODE&lcI) , lcNoDtPCod )

*- restore customer record
=gfSeek(lcSvKey,'Customer_A')

Select (lcSlct)
Return lcPricCode
*! C201129,1 MMT 04/05/2009 Read Style Price from Customer price table in SO Quick order entry[End]

*! C201219,1 MMT 03/15/2010 Enable Cost sheet Button in case of Status Actualized [Start]
*:**************************************************************************
*:* Name        : lfENBCSTSHT
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 03/15/2010
*:* Purpose     : Enable Cost sheet Button In PO Screen in case of Status Actualized
*:***************************************************************************
Function lfENBCSTSHT
lcTmpHdr = loFormSet.ariaform1.mainWorkOrder.cposhdr
If  loFormSet.ActiveMode = 'V' And  loFormSet.ariaform1.mainWorkOrder.cWorkOrdTyp $ "PP|NN" And ;
    &lcTmpHdr..Status = 'A'
  Oariaapplication.otoolbar.ChangeButtonStatus('cmdCostSh','ENABLED')
Endif
*! C201219,1 MMT 03/15/2010 Enable Cost sheet Button in case of Status Actualized [End]
*! C201245,1 MMT 06/01/2010 Custom Program to allow user to change account of EDI Temp. Order[Start]
*! C201245,1 MMT 06/01/2010 Custom Program to allow user to change account of EDI Temp. Order[Start]
*:**************************************************************************
*:* Name        : lfADDMNCHGAC
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 06/01/2010
*:* Purpose     : Add OPtion Menu to Change order account
*:***************************************************************************
Function lfADDMNCHGAC
Local lnCntBar
lcHostFormName = '[' + loFormSet.cHostFormName + ']'
lnCntBar = Cntbar('_INQURYPOP')+1
Define Bar lnCntBar Of _INQURYPOP Prompt '\<Change Account Code' Skip For ;
  gfFormIsActive(&lcHostFormName) .And. ((_Screen.ActiveForm.Parent.ActiveMode<>'E') Or  ( _Screen.ActiveForm.cboStatus.Value <> 'B'))

On Selection Bar lnCntBar Of _INQURYPOP Do lfEnbaleAccount

*!B610153,2 SAB 11/13/2012 Fix problem of not showing Order Charges on Sales Order Option [Start]
loFormSet.NextOptionBar = loFormSet.NextOptionBar + 1
*!B610153,2 SAB 11/13/2012 Fix problem of not showing Order Charges on Sales Order Option [End]

*:**************************************************************************
*:* Name        : lfEnbaleAccount
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 06/01/2010
*:* Purpose     : enable account field
*:***************************************************************************
Function lfEnbaleAccount
loFormSet = _Screen.ActiveForm.Parent
If loFormSet.ActiveMode = 'E' And loFormSet.ariaform1.cboStatus.Value = 'B'
  loFormSet.ariaform1.keyAccount.Enabled = .T.
  loFormSet.ariaform1.keyAccount.Keytextbox.SetFocus()

Endif
*:**************************************************************************
*:* Name        : lfEDITMPACCV
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 06/01/2010
*:* Purpose     : account field validation
*:***************************************************************************
Function lfEDITMPACCV
xAccount = loFormSet.ariaform1.keyAccount.Keytextbox.Value
If !Inlist(CUSTOMER.Status,'A','P')
  If CUSTOMER.Status = 'H' And loFormSet.OFORMENVIRONMENT.laSetups[23,2]
    If (gfModalGen('QRM32145B32005','ALERT',xAccount) <> 1)
      loFormSet.ariaform1.keyAccount.Keytextbox.Value = loFormSet.ariaform1.keyAccount.Keytextbox.OLDValue
      loFormSet.ariaform1.keyAccount.Tag ='1'
      Return
    Endif
  Else
    *E302520,1 MMT 04/09/2008 Add Setup to Allow user create SO for Hold Accounts{End}
    *-- Message : 32023
    *-- This a non-active xxxxx. Order entry is not allowed!
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32023B00000','ALERT','account'+'|'+"Order")
    loFormSet.ariaform1.keyAccount.Keytextbox.Value = loFormSet.ariaform1.keyAccount.Keytextbox.OLDValue
    loFormSet.ariaform1.keyAccount.Tag ='1'
    Return
  Endif
Endif
*check if Order has a Store Code and the Store Code does not exist on the new account code
lcTmpOrdHdr = loFormSet.OFORMENVIRONMENT.lcOrdHdr
lcStoreNum = &lcTmpOrdHdr..Store
If !Empty(lcStoreNum)
  lnCustRec = Recno('Customer')
  If !Seek('S'+xAccount+lcStoreNum ,'Customer','Customer')
    If gfModalGen('QRM00000B00031',.F.,.F.,.F.,;
        'The new account code selected does include store code '+Alltrim(lcStoreNum)+' - please add this store to new account '+xAccount+' or select a different account code') = 1
      Oariaapplication.DoProgram("AWRARCUST",'"'+xAccount+'"',.F.,'')
      _Screen.ActiveForm.keyStore.Keytextbox.Value = lcStoreNum
      _Screen.ActiveForm.keyStore.Keytextbox.Valid()
      _Screen.ActiveForm.Parent.Show(1)
      If !Seek('S'+xAccount+lcStoreNum ,'Customer','Customer')
        loFormSet.ariaform1.keyAccount.Keytextbox.Value = loFormSet.ariaform1.keyAccount.Keytextbox.OLDValue
        loFormSet.ariaform1.keyAccount.Tag ='1'
        If Between(lnCustRec,1,Reccount('Customer'))
          Go Record lnCustRec In 'Customer'
        Endif
        Return
      Endif
    Else
      loFormSet.ariaform1.keyAccount.Keytextbox.Value = loFormSet.ariaform1.keyAccount.Keytextbox.OLDValue
      loFormSet.ariaform1.keyAccount.Tag ='1'
      If Between(lnCustRec,1,Reccount('Customer'))
        Go Record lnCustRec In 'Customer'
      Endif
      Return
    Endif
  Endif
  If Between(lnCustRec,1,Reccount('Customer'))
    Go Record lnCustRec In 'Customer'
  Endif
Endif
lcTmpOrdLine = loFormSet.OFORMENVIRONMENT.lcOrdLine
lnRecnDet = Recno(lcTmpOrdLine)
Replace ACCOUNT With loFormSet.ariaform1.keyAccount.Keytextbox.Value ,;
  FLAG With Iif(Flag <> 'N','M',Flag)  In (lcTmpOrdLine) All
If Between(lnRecnDet ,1,Reccount(lcTmpOrdLine))
  Go Record lnRecnDet In (lcTmpOrdLine)
Endif
loFormSet.mGetOrderInformation ()
*! C201245,1 MMT 06/01/2010 Custom Program to allow user to change account of EDI Temp. Order[End]
*! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{Start}
*:**************************************************************************
*:* Name        : lfSTYCTNPRIC
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/04/2011
*:* Purpose     : Check if user wants the carton price
*:***************************************************************************
Function lfSTYCTNPRIC

*!*	IF (loFormSet.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.keyStyle.llExtendedScale AND loFormSet.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.llExSSScr)
*!*	  RETURN
*!*	ENDIF
If !llStyBrowse And !Inlist(Lastkey(),13,9)
  Return
Endif

If Type('loformSet.OldStyle') ='C' And Padr(loFormSet.OldStyle,19)  = Padr(loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.keyStyle.Value ,19)
  Return
Endif
*32000
If gfModalGen('QRM00000B00042','','','',"Use Carton Price") = 1
  Local lcSlct,lcPricCode
  lcSlct = Select()
  Private lnClrPos,lnClrLen
  Store 0  To lnClrPos,lnClrLen
  lfGetClrD()
  ldcChkDate = loFormSet.ariaform1.Ariapageframe1.Page1.txtEntered.Text1.Value
  lcOrdHdr = loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.Headerfile
  lcOrdln = loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.DetailFile
  lcCurrCode=loFormSet.ariaform1.Ariapageframe1.Page1.keyCurrency.Keytextbox.Value
  lcSeekSty =Padr(loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.keyStyle.Value ,19)
  If !Used('Customer_B')
    gfOpenTable('Customer','Customer','SH','Customer_B')
  Endif
  If !Used('CSTPRICE_B')
    gfOpenTable('CSTPRICE','CSTPRICE','SH','CSTPRICE_B')
  Endif
  If !Used('CSTPRICH_B')
    gfOpenTable('CSTPRICH','CSTPRICH','SH','CSTPRICH_B')
  Endif

  Select Customer_B
  lcAccount = &lcOrdHdr..ACCOUNT

  =gfSeek('M'+lcAccount,'Customer_B')

  lcPriceCode = ''
  llPricCode = .F.
  If !Empty(Customer_B.cpricctn1) And gfSeek(Customer_B.cpricctn1+lcCurrCode+lcSeekSty,'CSTPRICE_B') And ;
      IIF(gfSeek(Customer_B.cpricctn1+lcCurrCode+Padr(Substr(lcSeekSty,1,lnClrPos+lnClrLen-1),19) ,'CSTPRICH_B'),;
      EMPTY(CSTPRICH_B.DVLDPRTO) Or (!Empty(CSTPRICH_B.DVLDPRTO) And Between(ldcChkDate ,CSTPRICH_B.DVLDPRFR,CSTPRICH_B.DVLDPRTO)),.T.)
    lcPriceCode = Customer_B.cpricctn1
    llPricCode = .T.
  Else
    If !Empty(Customer_B.cpricctn2) And gfSeek(Customer_B.cpricctn2+lcCurrCode+lcSeekSty,'CSTPRICE_B')And ;
        IIF(gfSeek(Customer_B.cpricctn2+lcCurrCode+Padr(Substr(lcSeekSty,1,lnClrPos+lnClrLen-1),19) ,'CSTPRICH_B'),;
        EMPTY(CSTPRICH_B.DVLDPRTO) Or (!Empty(CSTPRICH_B.DVLDPRTO) And Between(ldcChkDate ,CSTPRICH_B.DVLDPRFR,CSTPRICH_B.DVLDPRTO)),.T.)
      lcPriceCode = Customer_B.cpricctn2
      llPricCode = .T.
    Endif
  Endif

  *! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{Start}
  If Type('loFormSet.llUseCustPrice') ='U'
    loFormSet.AddProperty("llUseCustPrice",llPricCode)
  Endif
  If Type('loFormSet.llNewStyleLine') ='U'
    loFormSet.AddProperty("llNewStyleLine",.F.)
  Endif
  loFormSet.llNewStyleLine = .T.
  loFormSet.llUseCustPrice = llPricCode
  If Type('loFormSet.laCustPrice[1]') ='U'
    loFormSet.AddProperty("laCustPrice[1]",'')
    loFormSet.laCustPrice = ''
  Endif
  *! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{End}

  If llPricCode
    Dimension laPriceArr[1]
    laPriceArr = 0
    *! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{Start}
    Dimension loFormSet.laCustPrice[scale.cnt]
    *! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{End}
    For lnS =1 To Scale.Cnt
      lcS = Str(lnS,1)
      If (CSTPRICE_B.PRICE&lcS. <> 0 Or CSTPRICE_B.PRICEDV <> 0)And Ascan(laPriceArr,Iif(CSTPRICE_B.PRICE&lcS. = 0,CSTPRICE_B.PRICEDV,CSTPRICE_B.PRICE&lcS.),1,0,1,2) = 0
        If laPriceArr[1] = 0
          laPriceArr[1] = Iif(CSTPRICE_B.PRICE&lcS. = 0,CSTPRICE_B.PRICEDV,CSTPRICE_B.PRICE&lcS.)
        Else
          Dimension laPriceArr[ALEN(laPriceArr,1)+1]
          laPriceArr[ALEN(laPriceArr,1)]= Iif(CSTPRICE_B.PRICE&lcS. = 0,CSTPRICE_B.PRICEDV,CSTPRICE_B.PRICE&lcS.)
        Endif
      Endif
      *! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{Start}
      loFormSet.laCustPrice[lnS] = Iif(CSTPRICE_B.PRICE&lcS. = 0,CSTPRICE_B.PRICEDV,CSTPRICE_B.PRICE&lcS.)
      *! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{End}
    Endfor
    lnCurLnRec = Recno(lcOrdln)
    Replace Gros_Price With laPriceArr[1],;
      PRICE      With Round(((Gros_Price)*(100-DISC_PCNT))/100,2)        In (lcOrdln)

    * Comm1      WITH IIF(CSTPRICE_B.COMMDV=0, Comm1,CSTPRICE_B.COMMDV)
    *! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{Start}
    *!*      IF ALEN(laPriceArr,1)>=1 AND laPriceArr[1] <> 0
    *!*        *Need to repeat records in ordline table
    *!*        FOR lnX = 2 TO ALEN(laPriceArr,1)
    *!*          SELECT (lcOrdln)
    *!*          SCATTER MEMO MEMVAR
    *!*          m.Gros_price = laPriceArr[lnX]
    *!*          m.Price    =  ROUND(((m.Gros_price )*(100-m.Disc_Pcnt))/100,2)
    *!*          *m.Comm1= IIF(CSTPRICE_B.COMMDV=0, m.Comm1,CSTPRICE_B.COMMDV)
    *!*          loFormSet.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo = loFormSet.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo + 1
    *!*          m.LineNo = loFormSet.Ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo
    *!*          INSERT INTO (lcOrdln) FROM MEMVAR
    *!*        ENDFOR
    *!*        IF BETWEEN(lnCurLnRec ,1,RECCOUNT(lcOrdln))
    *!*          GO RECORD lnCurLnRec IN (lcOrdln)
    *!*        ENDIF
    *!*      ENDIF
    *! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{End}
  Else
    Replace Gros_Price With gfGetprice(lcSeekSty,'B',0,lcCurrCode),;
      PRICE      With Round(((Gros_Price)*(100-DISC_PCNT))/100,2) In (lcOrdln)
  Endif
  Select(lcSlct)
Endif
loFormSet.OldStyle =  loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.keyStyle.Value
loFormSet.llNewStyleLine = .T.
*:**************************************************************************
*:* Name        : lfSTYOLDVALU
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/04/2011
*:* Purpose     : Function to keep the style old value in new property
*:***************************************************************************
Function lfSTYOLDVALU

If Type('loFormSet.OldStyle') ='U'
  loFormSet.AddProperty("OldStyle",'')
Endif
*! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{Start}
If Type('loFormSet.llUseCustPrice') ='U'
  loFormSet.AddProperty("llUseCustPrice",.F.)
Endif
If Type('loFormSet.llNewStyleLine') ='U'
  loFormSet.AddProperty("llNewStyleLine",.F.)
Endif
*! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{End}
loFormSet.OldStyle = Iif(loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.llNewLine,Space(19),loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.keyStyle.Value)
*! C201302,1 MMT 01/04/2011 Get the Price Code from Customer UDF for carton price{End}

*! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{Start}
*:**************************************************************************
*:* Name        : lfCUSTPRICE
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/11/2011
*:* Purpose     : Function to update ordline with new lines based on price
*:***************************************************************************
Function lfCUSTPRICE

If Type('loFormSet.llUseCustPrice') ='U' Or   Type('loFormSet.llNewStyleLine') ='U'
  Return
Endif
If (Type('loFormSet.llNewStyleLine') ='L' And loFormSet.llNewStyleLine  And;
    TYPE('loFormSet.llUseCustPrice') ='L' And !loFormSet.llUseCustPrice)
  If Ascan(loFormSet.laEvntTrig,Padr('CODPRICE',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
    loFormSet.mDoTrigger(Padr('CODPRICE',10))
  Endif
  Return
Endif

If Type('loFormSet.llUseCustPrice') ='L' And loFormSet.llUseCustPrice And ;
    TYPE('loFormSet.llNewStyleLine') ='L' And !loFormSet.llNewStyleLine
  Return
Endif



lcOrdln = loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.DetailFile
lcOrdHdr = loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.Headerfile
If Type('loFormSet.laCustPrice[1]') ='U' Or &lcOrdln..TOTQTY = 0 Or Scale.Cnt =1 Or ;
    (Type('llTxtQtyCall') = 'L' And llTxtQtyCall)
  Return
Endif

lnSlctFile = Select()
lnOrgLine = Recno(lcOrdln)
Dimension laSzAdded[ALEN(loFormSet.laCustPrice,1)],laSzQty[ALEN(loFormSet.laCustPrice,1)]
laSzAdded = .F.
llOrgEdited = .F.
laSzQty = 0
For lnA = 1 To Alen(loFormSet.laCustPrice,1)
  If laSzAdded[lnA]
    Loop
  Endif
  lnPrice =  loFormSet.laCustPrice[lnA]
  lcNotQtyScat = ""
  lcQtyAdd = ''
  For lnB = 1 To Alen(loFormSet.laCustPrice,1)
    lcB = Str(lnB,1)
    If loFormSet.laCustPrice[lnB] = lnPrice And !laSzAdded[lnB]
      laSzAdded[lnB] = .T.
      lcQtyAdd = lcQtyAdd + Iif(Empty(lcQtyAdd ),'',',')+'Qty'+Str(lnB,1)
    Else
      lcNotQtyScat = lcNotQtyScat + Iif(Empty(lcNotQtyScat),'',',')+'Qty'+Str(lnB,1)
    Endif
  Endfor
  laSzAdded[lnA] = .T.
  If llOrgEdited

    Select (lcOrdln)
    Scatter Memo Memvar
    For lnC =1 To Alen(loFormSet.laCustPrice,1)
      lcC = Str(lnC,1)
      m.QTY&lcC. =  0
    Endfor
    Dimension laLineQty[1]
    laLineQty = ''
    =gfSubstr(lcQtyAdd ,@laLineQty ,',')
    For lnE=1 To Alen(laLineQty,1)
      lcNum = Right(laLineQty[lnE],1)
      lcQtyFld = laLineQty[lnE]
      m.&lcQtyFld.  = laSzQty[VAL(lcNum)]
    Endfor
    m.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
    loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo = loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo + 1
    m.LINENO = loFormSet.ariaform1.Ariapageframe1.Page2.Ariaeditregion1.LastLineNo
    m.Gros_Price = loFormSet.laCustPrice[lnA]
    m.PRICE    =  Round(((m.Gros_Price )*(100-m.DISC_PCNT))/100,2)
    If m.TOTQTY > 0
      Insert Into (lcOrdln) From Memvar
    Endif
    If Between(lnOrgLine ,1,Reccount(lcOrdln))
      Go Record lnOrgLine  In (lcOrdln)
    Endif
  Else
    llOrgEdited = .T.
    For lnC =1 To Alen(loFormSet.laCustPrice,1)
      lcC = Str(lnC,1)
      laSzQty [lnC] = &lcOrdln..QTY&lcC.
    Endfor


    Replace Gros_Price With  loFormSet.laCustPrice[lnA],;
      PRICE      With  Round(((Gros_Price )*(100-DISC_PCNT))/100,2)
    If !Empty(lcNotQtyScat)
      Dimension laZeroSize[1]
      laZeroSize = ''
      =gfSubstr(lcNotQtyScat ,@laZeroSize,',')
      For lnD =1 To Alen(laZeroSize,1)
        lcsZ = laZeroSize[lnD]
        Replace &lcsZ. With 0
      Endfor
    Endif
    Replace TOTQTY With QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
  Endif
Endfor
loFormSet.laCustPrice = 0
loFormSet.llNewStyleLine = .F.
Select(lnSlctFile)
*! C201301,2 MMT 01/11/2011 Custom to get the Price Code from Customer UDF for carton price{End}

*! B609513,1 MMT 01/27/2011 Fix error in SO Quick order entry screen in case of Mutli dimension scale[Start]
*:**************************************************************************
*:* Name        : lfGetScPos
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 01/27/2011
*:* Purpose     : Function to Get Size Number in Scale (Multi-dimension)
*:***************************************************************************
Function lfGetScPos
Lparameters lcSize,lcScale
lcOldSel = Select()
lnScalePos = 0
=Seek('S'+lcScale,'Scale','Scale')
lcScaleFit = Scale.CDIM1
Select Scale
=Seek('S'+Substr(lcScale,1,loBranchFormSet.lnScaleLen))
lnCntScl = 0
llScaleFound = .F.
Scan Rest While Type+Scale+prepak  = 'S'+Substr(lcScale,1,loBranchFormSet.lnScaleLen) For  Scale.CDIM1 = lcScaleFit
  For lnY =1 To Scale.Cnt
    lcY = Str(lnY,1)
    lnCntScl = lnCntScl + 1
    If Alltrim(lcSize) == Alltrim(Scale.SZ&lcY.)
      lnScalePos= lnCntScl
      llScaleFound  = .T.
      Exit
    Endif
  Endfor
  If llScaleFound
    Exit
  Endif
Endscan
Select(lcOldSel)
Return lnScalePos
*! B609513,1 MMT 01/27/2011 Fix error in SO Quick order entry screen in case of Mutli dimension scale[End]
* HES
*:**************************************************************************
*:* Name        : lfBTCHVR
*:* Developer   : HES - Hesham Elmasry
*:* Date        : 01/28/2013
*:* Purpose     : Show default variables screen for the selected batch
*:***************************************************************************
Function lfBTCHVR

Set Step On
Store .F. To llVrAdded
Do Form (Oariaapplication.ScreenHome+"\IC\ICVRBTCH.SCX")
If !llVrAdded
  Return .F.
Endif
* End of lfBTCHVR

*:**************************************************************************
*:* Name        : lfUPDTVR
*:* Developer   : HES - Hesham Elmasry
*:* Date        : 01/28/2013
*:* Purpose     : Update fields cReason,cRefer and cAdjReason with the defaults if empty
*:***************************************************************************
Function lfUPDTVR

Set Step On
Select TempAdj
Scan
  Replace cReason    With Iif(Empty(Alltrim(cReason)),lcReason,cReason)
  Replace cRefer     With Iif(Empty(Alltrim(cRefer)),lcRef,cRefer)
  Replace cAdjReason With Iif(Empty(Alltrim(cAdjReason)),lcAdjReasonCode,cAdjReason)
Endscan
* End of lfUPDTVR
* HES
************************************************************
*! Name      : lfOPNAPINV
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/22/2013
*! Purpose   : open the appyinv screen from the GLQENTR.Scx SCREEN
************************************************************
Function lfOPNAPINV

oGrd = _Screen.ActiveForm.Ariagrid1
LCCURSOR1 = oGrd.RecordSource
If Seek(Evaluate(LCCURSOR1+'.CBATCHNO')+Evaluate(LCCURSOR1+'.CTRANNO'),'GLPTRNDT','BATCHTRN') ;
    .And.  .Not. Empty(Alltrim(GLPTRNDT.CINVNO+GLPTRNDT.CVENDCODE))

  lcRunPrg = lfGetFile('AP\APPYINV.FXP')
  Do (lcRunPrg) With GLPTRNDT.CVENDCODE,GLPTRNDT.CINVNO

Else
  = gfModalGen('INM00000B00000',.F.,.F.,.F.,'No payable invoice exists for this transaction')
  Return .F.
Endif


************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
*C201563,E303372,1 TMI 03/22/2013 [Start]
Function lfGetFile
Parameters lcFile
Local lcRun
If Oariaapplication.Multiinst And File(Oariaapplication.CLIENTAPPLICATIONHOME+lcFile)
  lcRun = Oariaapplication.CLIENTAPPLICATIONHOME+lcFile
Else
  lcRun = Oariaapplication.APPLICATIONHOME+lcFile
Endif
Return lcRun
*- End of lfGetScx.

************************************************************
*! Name      : lfGLPTRNDT
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/22/2013
*! Purpose   : update the custom fields cinvno,cvendcode in GLPTRNDT
************************************************************
*C201563,E303372,1 TMI 03/22/2013 [Start]
Function lfGLPTRNDT

Dimension AR_INV_VEN( 2)
Local lnPos
Public M.CINVNO, M.CVENDCODE
m.CINVNO = ''
m.CVENDCODE = ''
If GLTRNSHD.CSRCMODUL='AP'
  If  .Not. Used('APDIST')
    =GFOPENFILE(Oariaapplication.DataDir+'APDIST','INVVEND','SH')
  Endif
  If Type('loFormSet.AR_INV_VEN')='U'
    loFormSet.AddProperty('AR_INV_VEN[1,4]')
    loFormSet.AR_INV_VEN = ' '
  Endif
  If loFormSet.AR_INV_VEN[1,1] <> M.CBATCHNO
    Dimension loFormSet.AR_INV_VEN[1,4]
    Select CBATCHNO,CTRNSLEDN,CINVNO, CVENDCODE From APDIST Where APDIST.CBATCHNO=M.CBATCHNO Into Array loFormSet.AR_INV_VEN
  Endif
  For lnPos = 1 To Alen(loFormSet.AR_INV_VEN,1)
    If loFormSet.AR_INV_VEN[lnPos,2] = GLTRNSDT.CTRANNO
      m.CINVNO = loFormSet.AR_INV_VEN[lnPos,3]
      m.CVENDCODE = loFormSet.AR_INV_VEN[lnPos,4]
      Exit
    Endif
  Endfor
Endif
*- End of lfGLPTRNDT.
