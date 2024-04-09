*!*************************************************************
*! Name      : lfSavScr
*! Developer : Wael Ali Mohamed
*! Date      : 08/01/2002
*! Purpose   : Save new or modified order
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : llFromEDI   : Save EDI Order
*!                      lcActiveMode: Screen Active Mode
*!                      lcOrdLine   : Order lines temporary file
*!                      lcAlocated  : Allocated lines temporary file
*!                      lcOrdCanLn  : Cancelled lines temporary file
*!                      lcT_BomVar  : Variant Cost Sheet temporary file
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSavScr()
*!*************************************************************
*! Modifications :
*! N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char
*! C124447,1 HBG 09/27/2004 Allow editing CustPO in line level for GMA
*! HBG 12/22/2004 Fix bug of changing the order type in case of EDI sales order
*! B607585,1 AMH 05/25/2005 Add cwarecode field to POSLN index.
*! B607917,1 WAM 01/02/2007 Enhance performance of cancelation SO when allocated to POs (T20061013.0005)
*! B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode
*! B608398,1 WAM 12/31/2007 Get sales order type from the order header temporary file
*! B608431,1 WAM 02/12/2008 Unless the pick ticket is sent to edi partner, allow cancel the sales order and releaes the pick ticket
*! B608474,1 WAM 03/06/2008 Don't change location for picked lines
*! B608526,1 WAM 04/21/2008 Fix bug of saving order lines with zero quantity
*! C200981,1 MMT 04/10/2008 Fix problem of wrong line No. in SOCODES File(T20080214.0006)
*! N000612,1 WLD 06/03/2008 Adding New Transmission 865 (Supplier PO Change)in EDITrans (T20080430.0009)
*! B608600,1 MMT 07/01/2008 Fix bug of repeated lines in OORDCANLN File [T20080305.0010]
*! C201070,1 TMI 11/24/2008 Add a trigger to update the order charge file
*! C201131,1 MOS 04/26/2009 add custom saving msg when importing orders from spraedsheet[T20080812.0001]
*! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[T20090715.0001]
*! B608980,1 TMI 08/23/2009 show custom message for the cancel FOR BIN10 [T20070214.0006 ]
*! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [t20080812.0001]
*! B609006,2 MMT 11/18/2009 Fix bug of not updating StyHIST.Nused AND dend Date[T20080812.0001]
*! E302650,1 MMT 12/02/2009 Update Project Status When order or one of its lines is cancelled[T20091118.0003]
*! B609141,1 WAM 02/11/2010 Update style ordered quantity when update WEB order [T20100201.0025]
*! B609208,1 MMT 04/14/2010 Fix bug of cancelled records of same Order are Overwritten [T20100202.0013]
*! B609265,1 MMT 05/24/2010 Sales order Saving update lineNo incorrectly in ORDDSGN table[T20100503.0014]
*! B609371,1 SMA 08/08/2010 Fix bug of SO-Mass SO Cancellation does not update styhist [T20100720.0003]
*! E302741,1 WAM 08/25/2010 Convert Audit Trail file into SQL [T20100819.0020]
*! B609408,1 MMT 09/14/2010 Update Dend in Styhist while Saving the Sales order[T20100806.0002]
*! E302763,1 MMT 09/21/2010 modify SO Screen to apply the trade discount per line[T20100120.0060]
*! B609402,1 SMA 09/05/2010 [this bug fixes this date but added to the VSS later]fix bug of SO- Orddgsn table standard fields not updating [T20100813.0001]
*! E302811,1 TMI 12/12/2010 create a new sequence for Web orders [T20100927.0002]
*! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[T20110307.0005]
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003]
*: B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011
*! B609717,1 MMT 10/27/2011 CRM Saving SO Hangs because of deleted rec. in Contacts table[T20111025.0019]
*! B609797,1 MMT 01/26/2012 cannot allocate order created from Allocated Bulk order[T20111206.0020]
*! C201483,1 SAB 05/13/2012 New Customization to update site budget info on customer file[T20111206.0020]
*! B609945,1 HIA 28/05/2012 Issues with deposits - Set deleted missing [T20120427.0001]
*! B609984,1 MMT 07/03/2012 Coverting EDI Temp. Order to open order does not update orddsgn table[T20120425.0001]
*: B610001,1 HIA 07/12/2012 Approved ampunt not related to the open amount [T20120618.0004]
*! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035]
*! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[T20120711.0017]
*! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035]
*! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[T20121106.0009]
*! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017]
*! B610297,1 HIA 04/15/2013 T20121226.0001 - ARIA EDI : Received some edi release po’s for some edi bulk
*! B610337,1 HIA 05/22/13 T20130510.0004 PO allocation not showing in sales order #031660 Active Apparel Inc.
*! B610578,1 MMT 11/07/2013 Fix issue of uncancelling SO , changes order status to Bid not Open[T20131017.0007]
*! B610651,1 TMI 01/05/2014 Don't cancel an order that has piktkt with packing list [T20131220.0007 ] 
*! B610693,1 TMI 03/09/2014 Fix a problem that cancelling an order and then a pick ticket not updating the PIKTKT edit fields [T20140304.0032]
*! B610877,1 MMT 09/07/2014 Fix of error while editing sales order and lines got missed[T20150730.0007]
*! B611789 ,1 ES 06/19/2019 Auto Allocate crashing with duplicate SO order header entry [T20190509.0001]
*! B612506,1 MMT 12/13/2021 Prevent Ordline LineNo duplication[T20211203.0001]
*!*****************************************************************************
#INCLUDE R:\ARIA4XP\SCREENS\SO\SOORD.H

FUNCTION lfSavScr
*C124447,1 HBG 09/27/2004 Add parameter to have refrence to the formset [Begin]
*PARAMETERS llFromEDI, lcActiveMode, lcOrdLine, lcAlocated, lcOrdCanLn, lcT_BomVar

*B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode
*PARAMETERS llFromEDI, lcActiveMode, lcOrdLine, lcAlocated, lcOrdCanLn, lcT_BomVar,loFormSet
PARAMETERS llFromEDI, lcActiveMode, lcOrdHDr, lcOrdLine, lcAlocated, lcOrdCanLn, lcT_BomVar,loFormSet
*B608262,1 WAM 09/09/2007 (End)
*C124447,1 [End]

*B128066,1 HBG Fix bug of file is used by another user [Start]
LOCAL lnRePro
lnRePro = SET("Reprocess")
SET REPROCESS TO -1
*B128066,1 HBG [End]
*! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
*!*	LOCAL lnCanBulk,lnCancel,lnCancelAmt,lnLineCount,lcDetAlo,lcBulkYear,lcBulkPrd,;
*!*	      llPODsPrc, llMFDsPrc, llStyMark
LOCAL lnCanBulk,lnCancel,lnCancelAmt,lnLineCount,lcDetAlo,lcBulkYear,lcBulkPrd,;
  llPODsPrc, llMFDsPrc
*! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]

*B128682,1 HBG 06/28/2005 Fix bug of not updating location in detail [Begin]
IF TYPE('loFormSet') = 'O' AND TYPE('loFormSet.lcWareCode') = 'C'
  lcWareCode = loFormSet.lcWareCode
ELSE
  lcWareCode = &lcOrdHDr..cWareCode
ENDIF
*B128682,1 [End]

*-- llFromEDI  :- Variable hold true or false in case call save from EDI module.
*-- lnCanBulk  :- Variable hold cancel qty from Depleted bulk orders Quantity.
*-- lnCancel   :- Variable hold cancel qty from Depleted bulk orders Quantity.
*-- lnCancelAmt:- Variable hold cancel amount from Depleted bulk orders Quantity.
*-- lnLineCount:- Variable hold number of lines in order line.
*-- lcDetAlo   :- Variable hold CutTktL Or PosLn File if we have alocated qty.
llFromEDI  = IIF(TYPE('llFromEDI') = 'L' , llFromEDI , .F.)
llBomVarnt = gfGetMemVar('M_BOMVAR' ,oAriaApplication.ActiveCompanyID)
*-- Update Purchase Order Selling Price and Gross Margin
llPODsPrc = .F.
IF 'PO' $ oAriaApplication.CompanyInstalledModules
  llPODsPrc = gfGetMemVar('M_PoDspPrc')
ENDIF
*-- Update Cutting Tickets Selling Price and Gross Margin
llMFDsPrc = .F.
IF 'MF' $ oAriaApplication.CompanyInstalledModules
  llMFDsPrc = gfGetMemVar('M_MFDspPrc')
ENDIF
*-- Style MarkUp
llStyMark = gfGetMemVar('M_stymark') ='T'

*! E302763,1 MMT 09/21/2010 modify SO Screen to apply the trade discount per line[Start]
llTrdDscLn = gfGetMemVar("M_TRDDISCL",oAriaApplication.ActiveCompanyID)
llTrdDscLn = IIF(EMPTY(llTrdDscLn),.F.,llTrdDscLn)  && This line added by TMI to adjust the llTrdDscLn  value to be .F. if the option has not be setupsed
*! E302763,1 MMT 09/21/2010 modify SO Screen to apply the trade discount per line[End]

*B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode
*-- Last order line number
*lnLineCount = IIF(lcActiveMode="A",0,OLDVAL("LastLine","OrdHdr"))
*-- Order Status
*lcOrgStatus = IIF(lcActiveMode="A"," ",OLDVAL("Status","OrdHdr"))
*-- Last order line number
lnLineCount = IIF(lcActiveMode="A",0,OrdHdr.LastLine)
*-- Order Status
lcOrgStatus = IIF(lcActiveMode="A"," ",OrdHdr.STATUS)
*B608262,1 WAM 09/09/2007 (End)

SET KEY TO '' IN ORDLINE

*-- Web order and Customer Relation Module is Installed
IF &lcOrdHDr..lFromWeb AND 'CR' $ oAriaApplication.CompanyInstalledModules

  lcConfMail = gfGetMemVar('M_CONFMAIL',oAriaApplication.ActiveCompanyID)  && Confirmation email field
  lcSOAPR    = gfGetMemVar('M_SOAPR',oAriaApplication.ActiveCompanyID)     && Order Aproval Id
  lcSOCAN    = gfGetMemVar('M_SOCAN',oAriaApplication.ActiveCompanyID)     && Order Cancelation Id

  *-- If Web EDI Bid order status has been changed
  IF &lcOrdHDr..cOrdType = 'T' AND lcOrgStatus = 'B' AND  &lcOrdHDr..STATUS <> 'B'
    IF EMPTY(lcConfMail)
      *-- Message : 32090
      *-- Text    : The Confirmation E-mail Address on the setup screen is blank.
      *--           We can not save your order.
      *-- Button  : 00000
      *--           <Ok>
      =gfModalGen('TRM32090B00000','DIALOG')
      *B128066,1 HBG Fix bug of file is used by another user [Start]
      SET REPROCESS TO lnRePro
      *B128066,1 HBG [End]
      *B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode
      IF TYPE('loFormSet') = 'O'
        loFormSet.llUpdate = .F.
      ENDIF
      *B608262,1 WAM 09/09/2007 (End)
      RETURN .F.
    ENDIF

    *-- If the User approved(Open/Hold) the Web Order
    IF EMPTY(lcSOAPR) AND INLIST(&lcOrdHDr..STATUS,"O","H")
      *-- Message : 32091
      *-- Text    : The Approved ID on the setup screen is blank. Are you sure you want to continue?
      *-- Button  : 32000
      *--           <Yes> <No>
      IF gfModalGen('QRM32091B32000','DIALOG') =2
        *B128066,1 HBG Fix bug of file is used by another user [Start]
        SET REPROCESS TO lnRePro
        *B128066,1 HBG [End]
        *B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode
        IF TYPE('loFormSet') = 'O'
          loFormSet.llUpdate = .F.
        ENDIF
        *B608262,1 WAM 09/09/2007 (End)
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
ENDIF
*-- Check if no lines has been entered
IF !SEEK(&lcOrdHDr..cOrdType,lcOrdLine) .OR. &lcOrdHDr..Book = 0
  *-- Message : 32044
  *-- No lines entered! No updates performed.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM32044B00000','DIALOG')
  *B128066,1 HBG Fix bug of file is used by another user [Start]
  SET REPROCESS TO lnRePro
  *B128066,1 HBG [End]
  *B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode
  IF TYPE('loFormSet') = 'O'
    loFormSet.llUpdate = .F.
  ENDIF
  *B608262,1 WAM 09/09/2007 (End)
  RETURN .F.
ENDIF

*-- don't update in case the exchange rate less than or equal  zero.
IF &lcOrdHDr..cCurrCode = oAriaApplication.BaseCurrency
ELSE
  IF &lcOrdHDr..nExRate <=0
    *-- Message : 32028
    *-- The exchange rate must be greater than zero
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32028B00000','ALERT')
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]
    *B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode
    IF TYPE('loFormSet') = 'O'
      loFormSet.llUpdate = .F.
    ENDIF
    *B608262,1 WAM 09/09/2007 (End)
    RETURN .F.
  ENDIF
ENDIF
*- get the oreder number in case of manual or sequence order.
lcOrderNo = IIF(lcActiveMode = "E",&lcOrdHDr..ORDER,;
  IIF(gfGetMemVar('M_GenOrNum',oAriaApplication.ActiveCompanyID)='Y',lfGtOrder(),gfSequence('ORDER','','',&lcOrdHDr..cDivision)))
IF EMPTY(lcOrderNo)
  *B128066,1 HBG Fix bug of file is used by another user [Start]
  SET REPROCESS TO lnRePro
  *B128066,1 HBG [End]
  *B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode
  IF TYPE('loFormSet') = 'O'
    loFormSet.llUpdate = .F.
  ENDIF
  *B608262,1 WAM 09/09/2007 (End)
  RETURN .F.
ENDIF

*C201070 TMI 11/24/2008 [Start] Add a trigger to update the order charge file
IF ASCAN(loFormSet.laEvntTrig , PADR('CHRGORD',10)) <> 0
  =loFormSet.mDoTrigger(PADR('CHRGORD',10))  && remove this
ENDIF
*C201070 TMI 11/24/2008 [End  ]

*-- Do not allow Open or hold orders for potintial account or stores
IF &lcOrdHDr..STATUS <> "B" AND SEEK('M'+&lcOrdHDr..Account,'Customer') .AND. Customer.STATUS = 'P'
  *-- Message : 32085
  *-- Text    : You are about to save the sales order as Open while the customer is Potintial.
  *--           If you keep the order as is, the customer will be changed to Active.
  *--           Otherwise, Keep your customer as Potential and change the Order to Bid.
  *-- Button  : 32008
  *--           <Keep Order>  <Keep Customer>
  IF gfModalGen("QRM32085B32008","DIALOG",IIF(&lcOrdHDr..STATUS="O","Open","Hold")) = 1
    *-- Keep order status and change customer status to active
    SELECT Customer
    IF gfObj_Lock(.T.)
      REPLACE Customer.STATUS WITH "A"
      =gfObj_Lock(.F.)
    ELSE
      *-- Customer is in use by  another user, Keep customer as potintial and
      *-- change order status to BId
      =gfModalGen("INM32087B00000","DIALOG",'account')
      REPLACE STATUS WITH 'B' IN (lcOrdHDr)
    ENDIF
  ELSE
    *-- Keep customer as potintial and change order status to Bid
    REPLACE STATUS WITH 'B' IN (lcOrdHDr)
  ENDIF
ENDIF
*-- Check if bid order is ship to potintial stores
=lfChkStores()

*-- Get Fiscal Year and period of order entered date
STORE ' ' TO lcGlYear,lcGlPeriod
IF &lcOrdHDr..cOrdType='O' .AND. &lcOrdHDr..STATUS <> 'B' .AND. !CHECKPRD(&lcOrdHDr..Entered,'lcGlYear','lcGlPeriod','',.T.)
  STORE SPACE(4) TO lcGlYear
  STORE SPACE(2) TO lcGlPeriod
ENDIF
*-- Get Fiscal Year and period of bulk order entered date
STORE ' ' TO lcBulkYear,lcBulkPrd
*B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [Begin]
*IF &lcOrdHDr..cOrdType='O' .AND. &lcOrdHDr..STATUS <> 'B' .AND. lcActiveMode="A" .AND. !EMPTY(&lcOrdHDr..cFromOrder)
IF &lcOrdHDr..cOrdType='O' .AND. &lcOrdHDr..STATUS <> 'B' .AND. !EMPTY(&lcOrdHDr..cFromOrder)
  *B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [End]
  =gfOpenFile(oAriaApplication.DataDir+'OrdHdr',oAriaApplication.DataDir+'ORDHDR','SH','BlkOrdHd',.T.)
  =SEEK(&lcOrdHDr..cOrdType+&lcOrdHDr..cFromOrder,'BlkOrdHd')
  =CHECKPRD(BlkOrdHd.Entered,'lcBulkYear','lcBulkPrd','',.T.)
ENDIF

*-- if no change in lcOrdline.item_no get style.cvensty value.
IF ASCAN(oAriaApplication.laEvntTrig , PADR('UPDITMNO',10)) <> 0
  =gfDoTriger('SOORD',PADR('UPDITMNO',10))
ENDIF

*-- If there are user fields for this order, Call process Id to
*-- execute function before saving the entire order.
IF ASCAN(oAriaApplication.laEvntTrig , PADR('ORD_SAV',10)) <> 0
  IF !gfDoTriger('SOORD',PADR('ORD_SAV',10))
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]
    *B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode
    IF TYPE('loFormSet') = 'O'
      loFormSet.llUpdate = .F.
    ENDIF
    *B608262,1 WAM 09/09/2007 (End)
    RETURN .F.
  ENDIF
ENDIF

*-- If there are user fields for this order, Call process Id to
*-- execute function before saving the entire order.
IF ASCAN(oAriaApplication.laEvntTrig , PADR('ALO_RES',10)) <> 0
  llReturn = gfDoTriger('SOORD',PADR('ALO_RES',10))
ENDIF

*-- Validation for Completion date in Packs file (Custom process for Cathy Daniels)
IF ASCAN(oAriaApplication.laEvntTrig , PADR('COMPLDAT',10)) <> 0
  IF !gfDoTriger('SOORD',PADR('COMPLDAT',10))
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]
    *B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode
    IF TYPE('loFormSet') = 'O'
      loFormSet.llUpdate = .F.
    ENDIF
    *B608262,1 WAM 09/09/2007 (End)
    RETURN .F.
  ENDIF
ENDIF

*-- Save for MBI.

*!*	IF ASCAN(oAriaApplication.laEvntTrig , PADR('SAVSCR',10)) <> 0
*!*	  IF gfDoTriger('SOORD',PADR('SAVSCR',10))
IF TYPE('loFormSet') = 'O' AND ASCAN(loFormSet.laEvntTrig , PADR('SAVSCR',10)) <> 0
  IF loFormSet.mDoTrigger(PADR('SAVSCR',10))

    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]
    *B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode
    IF TYPE('loFormSet') = 'O'
      loFormSet.llUpdate = .T.
    ENDIF
    *B608262,1 WAM 09/09/2007 (End)
    RETURN .T.
  ENDIF
ENDIF
*- If we are in the edit mode.
IF lcActiveMode = "E"
  DO CASE
    *-- If the order status changed to "Open"
  CASE &lcOrdHDr..STATUS <> OrdHdr.STATUS .AND. &lcOrdHDr..STATUS = "O" .AND. ;
      ASCAN(oAriaApplication.laEvntTrig , PADR('CHG_OPN',10)) <> 0
    llReturn = gfDoTriger('SOORD',PADR('CHG_OPN',10))
    *-- If the order status changed to "On Hold"
  CASE &lcOrdHDr..STATUS <> OrdHdr.STATUS .AND. &lcOrdHDr..STATUS = "H" .AND. ;
      ASCAN(oAriaApplication.laEvntTrig , PADR('CHG_HLD',10)) <> 0
    llReturn = gfDoTriger('SOORD',PADR('CHG_HLD',10))
    *-- If there is any modification happened on the current order header.
  OTHERWISE
    *-- If any field has been modified
    IF ASCAN(oAriaApplication.laEvntTrig , PADR('MOD_ORD',10)) <> 0
      llReturn = gfDoTriger('SOORD',PADR('MOD_ORD',10))
    ENDIF
  ENDCASE
ENDIF

*! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[Start]
STORE 0 TO lnMajorLen , lnClrPos, lnClrLen
lfGetClrD()
lnMajorLen = LEN(gfItemMask("PM"))

IF !USED('STYHIST')
  =gfOpenTable('STYHIST','STYHIST','SH')
ENDIF
IF !USED('UNIFORM')
  =gfOpenTable('UNIFORM','UNIFORM','SH')
ENDIF

IF !USED('Contact_A')
  =gfOpenTable('Contact','Contact','SH','Contact_A')
ENDIF
*! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[End]

*! B609408,1 MMT 09/14/2010 Update Dend in Styhist while Saving the Sales order[Start]
IF !USED('STYHIST_OS')
  =gfOpenTable('STYHIST','STYHIST','SH','STYHIST_OS')
ENDIF
*! B609408,1 MMT 09/14/2010 Update Dend in Styhist while Saving the Sales order[End]

*-- Update order modified allocated quantity
IF &lcOrdHDr..cOrdType='O' .AND. &lcOrdHDr..STATUS <> 'B' .AND. lcActiveMode="E" .AND. USED(lcAlocated)
  lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
  IF TYPE('lcTranCode') = 'N'
    =oAriaApplication.RemoteCompanyData.CheckRetResult("BEGINTRAN",lcTranCode,.T.)
  ELSE
    WAIT 'Updating alocated quantity...' WINDOW NOWAIT
    SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
    SELECT (lcAlocated)
    GO TOP
    DO WHILE !EOF()
      lcTranCd = TRANCD
      lcTktNo  = CTKTNO
      lnOrdQty = 0
      *HBG 3/28/2005 Get lines of POSLN to calculate the Amount of the PO/CT [Begin]
      lcSqlStatment = "SELECT * FROM POSLN [INDEX=POSLN] WHERE cBusDocu='P' AND cStyType='"+IIF(lcTranCd='1','U','P')+"' AND PO='"+lcTktNo+"'"
      lnConnectionHandlar = oAriaApplication.RemoteCompanyData.SqlRun(lcSqlStatment,'POSLN','POSLN',lcTranCode,4,'SAVE',SET("DATASESSION"))
      *HBG [End]
      SELECT (lcAlocated)
      SCAN REST WHILE TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE = lcTranCd+lcTktNo
        SCATTER MEMVAR
        *-- Get original allocated quantity
        =SEEK(m.TRANCD+m.CTKTNO+m.CTKTLINENO+m.ORDER+m.STYLE+m.CORDLINE,'CUTPICK','Cutpkord')
        IF !EMPTY(m.cUpdSizes)
          lnOrdQty = lnOrdQty + m.TotQty-CUTPICK.TotQty
          IF SEEK(&lcOrdHDr..cOrdType+m.ORDER+PADL(ALLTRIM(m.CORDLINE),6),lcOrdLine,'OrdLine')
            *-- Update style WIP & WO
            IF SEEK(m.STYLE,'Style')
              =RLOCK('Style')
              FOR lnSize = 1 TO LEN(ALLTRIM(m.cUpdSizes))
                lcSize = SUBSTR(m.cUpdSizes,lnSize,1)
                REPLACE WIP&lcSize WITH WIP&lcSize - CUTPICK.Qty&lcSize + m.Qty&lcSize , ;
                  TOTWIP     WITH TOTWIP     - CUTPICK.Qty&lcSize + m.Qty&lcSize , ;
                  NWO&lcSize WITH NWO&lcSize - CUTPICK.Qty&lcSize + m.Qty&lcSize , ;
                  NTOTWO     WITH NTOTWO     - CUTPICK.Qty&lcSize + m.Qty&lcSize IN STYLE
              ENDFOR
              UNLOCK IN STYLE
            ENDIF
            *-- Update Warehouse WIP & WO
            IF SEEK(m.STYLE + &lcOrdLine..cWareCode+SPACE(10) ,'StyDye')
              =RLOCK('StyDye')
              FOR lnSize = 1 TO LEN(ALLTRIM(m.cUpdSizes))
                lcSize = SUBSTR(m.cUpdSizes,lnSize,1)
                REPLACE WIP&lcSize WITH WIP&lcSize - CUTPICK.Qty&lcSize + m.Qty&lcSize , ;
                  TOTWIP     WITH TOTWIP     - CUTPICK.Qty&lcSize + m.Qty&lcSize , ;
                  NWO&lcSize WITH NWO&lcSize - CUTPICK.Qty&lcSize + m.Qty&lcSize , ;
                  NTOTWO     WITH NTOTWO     - CUTPICK.Qty&lcSize + m.Qty&lcSize IN StyDye
              ENDFOR
              UNLOCK IN StyDye
            ENDIF
            *-- Update configuration WIP & WO
            IF !EMPTY(&lcOrdLine..Dyelot) AND SEEK(m.STYLE + &lcOrdLine..cWareCode+&lcOrdLine..Dyelot,'StyDye')
              =RLOCK('StyDye')
              FOR lnSize = 1 TO LEN(ALLTRIM(m.cUpdSizes))
                lcSize = SUBSTR(m.cUpdSizes,lnSize,1)
                REPLACE WIP&lcSize WITH WIP&lcSize - CUTPICK.Qty&lcSize + m.Qty&lcSize , ;
                  TOTWIP     WITH TOTWIP     - CUTPICK.Qty&lcSize + m.Qty&lcSize , ;
                  NWO&lcSize WITH NWO&lcSize - CUTPICK.Qty&lcSize + m.Qty&lcSize , ;
                  NTOTWO     WITH NTOTWO     - CUTPICK.Qty&lcSize + m.Qty&lcSize IN StyDye
              ENDFOR
              UNLOCK IN StyDye
            ENDIF
          ENDIF
        ENDIF
        *-- Update ticket lines Ordered quantity
        *HBG 3/28/2005 Update qty and order qty in POSLN [Begin]
        SELECT POSLN
        LOCATE FOR cBusDocu+cstytype+po+cInvType+STYLE+STR(LINENO,6)+TRANCD=IIF(lcTranCd = '1','PU','PP')+m.CTKTNO+'0001'+m.STYLE+m.CTKTLINENO+'1'
        REPLACE Ord1 WITH Ord1 + m.Qty1 - CUTPICK.Qty1 ,;
          Ord2 WITH Ord2 + m.Qty2 - CUTPICK.Qty2 ,;
          Ord3 WITH Ord3 + m.Qty3 - CUTPICK.Qty3 ,;
          Ord4 WITH Ord4 + m.Qty4 - CUTPICK.Qty4 ,;
          Ord5 WITH Ord5 + m.Qty5 - CUTPICK.Qty5 ,;
          Ord6 WITH Ord6 + m.Qty6 - CUTPICK.Qty6 ,;
          Ord7 WITH Ord7 + m.Qty7 - CUTPICK.Qty7 ,;
          Ord8 WITH Ord8 + m.Qty8 - CUTPICK.Qty8 ,;
          TotOrd WITH TotOrd + m.TotQty - CUTPICK.TotQty
        IF !EMPTY(m.cUpdSizes)
          REPLACE Qty1 WITH Qty1 + IIF('1' $ m.cUpdSizes,m.Qty1-CUTPICK.Qty1,0) ,;
            Qty2 WITH Qty2 + IIF('2' $ m.cUpdSizes,m.Qty2-CUTPICK.Qty2,0) ,;
            Qty3 WITH Qty3 + IIF('3' $ m.cUpdSizes,m.Qty3-CUTPICK.Qty3,0) ,;
            Qty4 WITH Qty4 + IIF('4' $ m.cUpdSizes,m.Qty4-CUTPICK.Qty4,0) ,;
            Qty5 WITH Qty5 + IIF('5' $ m.cUpdSizes,m.Qty5-CUTPICK.Qty5,0) ,;
            Qty6 WITH Qty6 + IIF('6' $ m.cUpdSizes,m.Qty6-CUTPICK.Qty6,0) ,;
            Qty7 WITH Qty7 + IIF('7' $ m.cUpdSizes,m.Qty7-CUTPICK.Qty7,0) ,;
            Qty8 WITH Qty8 + IIF('8' $ m.cUpdSizes,m.Qty8-CUTPICK.Qty8,0) ,;
            TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        ENDIF
        *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
        IF SEEK('O'+CUTPICK.ORDER + CUTPICK.CORDLINE ,lcOrdLine,'ORDLINE')
          REPLACE Cut1   WITH MAX(Cut1 + m.Qty1 - CUTPICK.Qty1,0) ,;
            Cut2   WITH MAX(Cut2 + m.Qty2 - CUTPICK.Qty2,0) ,;
            Cut3   WITH MAX(Cut3 + m.Qty3 - CUTPICK.Qty3,0) ,;
            Cut4   WITH MAX(Cut4 + m.Qty4 - CUTPICK.Qty4,0) ,;
            Cut5   WITH MAX(Cut5 + m.Qty5 - CUTPICK.Qty5,0) ,;
            Cut6   WITH MAX(Cut6 + m.Qty6 - CUTPICK.Qty6,0) ,;
            Cut7   WITH MAX(Cut7 + m.Qty7 - CUTPICK.Qty7,0) ,;
            Cut8   WITH MAX(Cut8 + m.Qty8 - CUTPICK.Qty8,0) ,;
            TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8 IN (lcOrdLine)
        ENDIF
        *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]

        *!*	        lcSelString="UPDATE PosLn SET Ord1=Ord1+"+ALLTRIM(STR(m.Qty1-CutPick.Qty1))+",Ord2=Ord2+"+ALLTRIM(STR(m.Qty2-CutPick.Qty2))+;
        *!*	        ",Ord3=Ord3+"+ALLTRIM(STR(m.Qty3-CutPick.Qty3))+",Ord4=Ord4+"+ALLTRIM(STR(m.Qty4-CutPick.Qty4))+;
        *!*	        ",Ord5=Ord5+"+ALLTRIM(STR(m.Qty5-CutPick.Qty5))+",Ord6=Ord6+"+ALLTRIM(STR(m.Qty6-CutPick.Qty6))+;
        *!*	        ",Ord7=Ord7+"+ALLTRIM(STR(m.Qty7-CutPick.Qty7))+",Ord8=Ord8+"+ALLTRIM(STR(m.Qty8-CutPick.Qty8))+;
        *!*	        ",TotOrd=TotOrd+"+ALLTRIM(STR(m.TotQty-CutPick.TotQty))
        *!*	        *-- Update ticket lines budget quantities
        *!*	        IF !EMPTY(m.cUpdSizes)
        *!*	          lcSelString=lcSelString+IIF('1' $ m.cUpdSizes,",Qty1=Qty1+"+ALLTRIM(STR(m.Qty1-CUTPICK.Qty1)),'')+;
        *!*	          IIF('2' $ m.cUpdSizes,",Qty2=Qty2+"+ALLTRIM(STR(m.Qty2-CUTPICK.Qty2)),'')+;
        *!*	          IIF('3' $ m.cUpdSizes,",Qty3=Qty3+"+ALLTRIM(STR(m.Qty3-CUTPICK.Qty3)),'')+;
        *!*	          IIF('4' $ m.cUpdSizes,",Qty4=Qty4+"+ALLTRIM(STR(m.Qty4-CUTPICK.Qty4)),'')+;
        *!*	          IIF('5' $ m.cUpdSizes,",Qty5=Qty5+"+ALLTRIM(STR(m.Qty5-CUTPICK.Qty5)),'')+;
        *!*	          IIF('6' $ m.cUpdSizes,",Qty6=Qty6+"+ALLTRIM(STR(m.Qty6-CUTPICK.Qty6)),'')+;
        *!*	          IIF('7' $ m.cUpdSizes,",Qty7=Qty7+"+ALLTRIM(STR(m.Qty7-CUTPICK.Qty7)),'')+;
        *!*	          IIF('8' $ m.cUpdSizes,",Qty8=Qty8+"+ALLTRIM(STR(m.Qty8-CUTPICK.Qty8)),'')+;
        *!*	          ",TotQty=TotQty+"+ALLTRIM(STR(m.TotQty-CUTPICK.TotQty))
        *!*	        ENDIF
        *!*	        IF m.TranCd = '1'
        *!*	          lcSelString=lcSelString+" WHERE cBusDocu+cstytype+po+cInvType+style+STR([lineno],6)+trancd='PU"+m.CTKTNO+'0001'+m.Style+m.cTktLineNo+"1'"
        *!*	        ELSE
        *!*	          lcSelString=lcSelString+" WHERE cBusDocu+cstytype+po+cInvType+style+STR([lineno],6)+trancd='PP"+m.CTKTNO+'0001'+m.Style+m.cTktLineNo+"1'"
        *!*	        ENDIF
        *!*	        lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode,4,'',SET("DATASESSION"))
        *HBG [End]
        *-- Update modified allocated quantity
        SELECT CUTPICK
        IF m.TotQty = 0
          DELETE
        ELSE
          GATHER MEMVAR
        ENDIF
      ENDSCAN
      *HBG 3/28/2005 Calculate the Amount of the PO/CT from POSLN [Begin]
      SELECT POSLN
      STORE 0 TO lnCost1,lnCost2,lnCost3,lnCost4,lnCost5,lnCost6,lnCost7 , lnPOTotal
      STORE 0 TO lnFCost1,lnFCost2,lnFCost3,lnFCost4,lnFCost5,lnFCost6,lnFCost7
      SCAN
        lnCost1 = lnCost1 + (TotQty * nicost1)
        lnCost2 = lnCost2 + (TotQty * nicost2)
        lnCost3 = lnCost3 + (TotQty * nicost3)
        lnCost4 = lnCost4 + (TotQty * nicost4)
        lnCost5 = lnCost5 + (TotQty * nicost5)
        lnCost6 = lnCost6 + (TotQty * nicost6)
        lnCost7 = lnCost7 + (TotQty * nicost7)

        lnFCost1 = lnFCost1 + (TotQty * nFcost1)
        lnFCost2 = lnFCost2 + (TotQty * nFcost2)
        lnFCost3 = lnFCost3 + (TotQty * nFcost3)
        lnFCost4 = lnFCost4 + (TotQty * nFcost4)
        lnFCost5 = lnFCost5 + (TotQty * nFcost5)
        lnFCost6 = lnFCost6 + (TotQty * nFcost6)
        lnFCost7 = lnFCost7 + (TotQty * nFcost7)
      ENDSCAN
      lnPOTotal = lnCost1+lnCost2+lnCost3+lnCost4+lnCost5+lnCost6+lnCost7

      *B607585,1 AMH Add cwarecode field to POSLN index [Start]
      *lnResult = oAriaApplication.RemoteCompanyData.SqlUpdate('POSLN',lcTranCode,SET("DATASESSION"),'cBusDocu,cStyType,PO,cInvType,Style,LineNo,TranCd','POSLN','POSLN')
      lnResult = oAriaApplication.RemoteCompanyData.SqlUpdate('POSLN',lcTranCode,SET("DATASESSION"),'CBUSDOCU,CSTYTYPE,PO,CRSESSION,SHIPNO,CINVTYPE,STYLE,LINENO,TRANCD,CSTYGRADE,CWARECODE','POSLN','POSREC')
      *B607585,1 AMH [End]

      IF lnResult <=0
        =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
        =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
      ENDIF
      *HBG [End]

      *-- Update ticket total Ordered & budget quantity
      IF lcTranCd = '1'
        *-- Change Cutting quantity if order allocated quantity has been changed
        *HBG 3/28/2005 Update the Amount of the CT in POSHDR [Begin]
        *lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd+"+ALLTRIM(STR(lnOrdQty))+",nStyOrder = nStyOrder + "+ALLTRIM(STR(lnOrdQty))+" ,[Open] = [Open] + "+ALLTRIM(STR(lnOrdQty))+" WHERE cBusDocu+cstytype+po='PU"+lcTktNo+"'"
        lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd+"+ALLTRIM(STR(lnOrdQty))+",nStyOrder = nStyOrder + "+ALLTRIM(STR(lnOrdQty))+" ,[Open] = [Open] + "+ALLTRIM(STR(lnOrdQty))+;
          " ,niCost1 = "+ALLTRIM(STR(lnCost1))+" ,niCost2 = "+ALLTRIM(STR(lnCost2))+" ,niCost3 = "+ALLTRIM(STR(lnCost3))+" ,niCost4 = "+ALLTRIM(STR(lnCost4))+;
          " ,niCost5 = "+ALLTRIM(STR(lnCost5))+" ,niCost6 = "+ALLTRIM(STR(lnCost6))+" ,niCost7 = "+ALLTRIM(STR(lnCost7))+;
          ",nFCost1= "+ALLTRIM(STR(lnFCost1))+",nFCost2= "+ALLTRIM(STR(lnFCost2))+",nFCost3= "+ALLTRIM(STR(lnFCost3))+",nFCost4= "+ALLTRIM(STR(lnFCost4))+;
          ",nFCost5= "+ALLTRIM(STR(lnFCost5))+",nFCost6= "+ALLTRIM(STR(lnFCost6))+",nFCost7= "+ALLTRIM(STR(lnFCost7))+;
          " ,POTotal = "+ALLTRIM(STR(lnPOTotal))+" WHERE cBusDocu+cstytype+po='PU"+lcTktNo+"'"
        *HBG [End]
      ELSE
        *-- Change Purchased quantity if order allocated quantity has been changed
        *HBG 3/28/2005 Update the Amount of the PO in POSHDR [Begin]
        *lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd+"+ALLTRIM(STR(lnOrdQty))+",nStyOrder = nStyOrder + "+ALLTRIM(STR(lnOrdQty))+" ,[Open] = [Open] + "+ALLTRIM(STR(lnOrdQty))+" WHERE cBusDocu+cstytype+po='PP"+lcTktNo+"'"
        lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd+"+ALLTRIM(STR(lnOrdQty))+",nStyOrder = nStyOrder + "+ALLTRIM(STR(lnOrdQty))+" ,[Open] = [Open] + "+ALLTRIM(STR(lnOrdQty))+;
          " ,niCost1 = "+ALLTRIM(STR(lnCost1))+" ,niCost2 = "+ALLTRIM(STR(lnCost2))+" ,niCost3 = "+ALLTRIM(STR(lnCost3))+" ,niCost4 = "+ALLTRIM(STR(lnCost4))+;
          " ,niCost5 = "+ALLTRIM(STR(lnCost5))+" ,niCost6 = "+ALLTRIM(STR(lnCost6))+" ,niCost7 = "+ALLTRIM(STR(lnCost7))+;
          ",nFCost1= "+ALLTRIM(STR(lnFCost1))+",nFCost2= "+ALLTRIM(STR(lnFCost2))+",nFCost3= "+ALLTRIM(STR(lnFCost3))+",nFCost4= "+ALLTRIM(STR(lnFCost4))+;
          ",nFCost5= "+ALLTRIM(STR(lnFCost5))+",nFCost6= "+ALLTRIM(STR(lnFCost6))+",nFCost7= "+ALLTRIM(STR(lnFCost7))+;
          " ,POTotal = "+ALLTRIM(STR(lnPOTotal))+" WHERE cBusDocu+cstytype+po='PP"+lcTktNo+"'"
        *HBG [End]
      ENDIF
      lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode,4,'',SET("DATASESSION"))
      SELECT (lcAlocated)
    ENDDO

    lnResult = oAriaApplication.RemoteCompanyData.SqlUpdate('CUTPICK',lcTranCode, SET("DATASESSION"),'trancd,ctktno,ctktlineno,order,style,cordline')
    IF lnResult <=0
      =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
      =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    ELSE
      IF oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.) = 1
      ELSE
        =oAriaApplication.RemoteCompanyData.CheckRetResult("COMMITTRAN",lnResult,.T.)
      ENDIF
    ENDIF
    WAIT CLEAR
  ENDIF
ENDIF
*! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
*=gfOpenFile(oAriaApplication.DataDir+'icStyHst',oAriaApplication.DataDir+'Styhst','SH')
=gfOpenTable('ICSTYHST','STYHST','SH')
*! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
*! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
lnTableICStyHst = gfGetRemoteTable(SET("Datasession"),'ICSTYHST')
lcICStyHstTable = ''
IF lnTableICStyHst<>0 && Remote Table Object was Found
  lcICStyHstTable = oAriaApplication.laRemoteTable[lnTableICStyHst].lcCursorUpdate
ENDIF
*! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
IF 'AL' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'PikTkt',oAriaApplication.DataDir+'PikTkt','SH')
ENDIF
=gfOpenFile(oAriaApplication.DataDir+'ORDCANLN',oAriaApplication.DataDir+'ORDCANLN','SH')

*-- Move bulk order allocation to new distribution order
IF &lcOrdHDr..cOrdType='O' AND lcActiveMode="A" AND !EMPTY(&lcOrdHDr..cFromOrder) AND ;
    SEEK('O'+&lcOrdHDr..cFromOrder,'BLKORDHD') AND BlkOrdHd.TotCut > 0

  lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
  IF TYPE('lcTranCode') = 'N'
    =oAriaApplication.RemoteCompanyData.CheckRetResult("BEGINTRAN",lcTranCode,.T.)
  ELSE
    lcSelString = "SELECT * FROM cutpick WHERE TranCd+[Order]+cOrdLine LIKE '1"+&lcOrdHDr..cFromOrder+"%' UNION "
    lcSelString = lcSelString + "SELECT * FROM cutpick WHERE TranCd+[Order]+cOrdLine LIKE '2"+&lcOrdHDr..cFromOrder+"%'"
    lnConnectionHandlar = oAriaApplication.RemoteCompanyData.Execute(lcSelString ,'',"CutPick","CutPick",lcTranCode,4,'',SET("DATASESSION"))
    IF oAriaApplication.RemoteCompanyData.Execute(lcSelString ,'',"CutPick","CutPick",lcTranCode,4,'',SET("DATASESSION")) = 1
    
      *! B610337,1 HIA 05/22/13 T20130510.0004 PO allocation not showing in sales order #031660 Active Apparel Inc. [Start]
      oldalias = ALIAS()
      SELECT (lcOrdLine)
      
      lcNewLineNo = 0
      SCAN
        IF FLAG = 'N' .AND. !DELETED()  AND TotQty > 0
          lcNewLineNo = lcNewLineNo + 1 
          Replace cedit_user WITH ALLTRIM(STR(lcNewLineNo))
        ENDIF
      ENDSCAN
      lcCurrOrder = ORDER()
      SET ORDER TO 
      
      LOCATE 
      Replace lineno WITH VAL(cedit_user) FOR FLAG = 'N' .AND. !DELETED()  AND TotQty > 0 ALL 
      SET ORDER TO (lcCurrOrder)
      LOCATE 
      
      TRY
        SELECT (oldalias)
      CATCH

      ENDTRY
      *! B610337,1 HIA 05/22/13 T20130510.0004 PO allocation not showing in sales order #031660 Active Apparel Inc. [End]


      SELECT CUTPICK
      =CURSORSETPROP("Buffering" ,3,"CutPick")
      INDEX ON ORDER+CORDLINE+CTKTNO+CTKTLINENO TAG CUTORD
      =CURSORSETPROP("Buffering" ,5,"CutPick")

      SELECT (lcOrdLine)
      SCAN
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laDistQty
        SELECT CUTPICK
        IF SEEK(&lcOrdHDr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6))
          lcTranCd = TRANCD
          SCAN REST ;
              WHILE ORDER+CORDLINE+CTKTNO+CTKTLINENO = ;
              &lcOrdHDr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6) AND ;
              laDistQty[1]+laDistQty[2]+laDistQty[3]+laDistQty[4]+;
              laDistQty[5]+laDistQty[6]+laDistQty[7]+laDistQty[8] >0
            *-- Get Original allocated quantity
            m.CTKTLINENO = CTKTLINENO
            m.CTKTNO = CTKTNO
            m.Cut1 = Qty1
            m.Cut2 = Qty2
            m.Cut3 = Qty3
            m.Cut4 = Qty4
            m.Cut5 = Qty5
            m.Cut6 = Qty6
            m.Cut7 = Qty7
            m.Cut8 = Qty8
            *-- Release bulk order allocation
            REPLACE Qty1   WITH MAX(Qty1-laDistQty[1],0) ,;
              Qty2   WITH MAX(Qty2-laDistQty[2],0) ,;
              Qty3   WITH MAX(Qty3-laDistQty[3],0) ,;
              Qty4   WITH MAX(Qty4-laDistQty[4],0) ,;
              Qty5   WITH MAX(Qty5-laDistQty[5],0) ,;
              Qty6   WITH MAX(Qty6-laDistQty[6],0) ,;
              Qty7   WITH MAX(Qty7-laDistQty[7],0) ,;
              Qty8   WITH MAX(Qty8-laDistQty[8],0) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
            IF TotQty = 0
              DELETE
            ENDIF
            *-- Do not exceed order quantity
            m.Cut1 = MIN(m.Cut1,laDistQty[1])
            m.Cut2 = MIN(m.Cut2,laDistQty[2])
            m.Cut3 = MIN(m.Cut3,laDistQty[3])
            m.Cut4 = MIN(m.Cut4,laDistQty[4])
            m.Cut5 = MIN(m.Cut5,laDistQty[5])
            m.Cut6 = MIN(m.Cut6,laDistQty[6])
            m.Cut7 = MIN(m.Cut7,laDistQty[7])
            m.Cut8 = MIN(m.Cut8,laDistQty[8])
            m.TotCut = m.Cut1+m.Cut2+m.Cut3+m.Cut4+m.Cut5+m.Cut6+m.Cut7+m.Cut8
            *-- Allocate C/T to newly depleted order
            IF !SEEK(&lcOrdHDr..ORDER+STR(&lcOrdLine..LINENO,6)+m.CTKTNO+m.CTKTLINENO,'CUTPICK')
              INSERT INTO 'CUTPICK' ;
                (CTKTNO,CTKTLINENO,TRANCD,ORDER,CORDLINE,STYLE) VALUES ;
                (m.CTKTNO,m.CTKTLINENO,lcTranCd,lcOrderNo,STR(&lcOrdLine..LINENO,6),&lcOrdLine..STYLE)
            ENDIF
            REPLACE Qty1   WITH Qty1   + m.Cut1 ,;
              Qty2   WITH Qty2   + m.Cut2 ,;
              Qty3   WITH Qty3   + m.Cut3 ,;
              Qty4   WITH Qty4   + m.Cut4 ,;
              Qty5   WITH Qty5   + m.Cut5 ,;
              Qty6   WITH Qty6   + m.Cut6 ,;
              Qty7   WITH Qty7   + m.Cut7 ,;
              Qty8   WITH Qty8   + m.Cut8 ,;
              TotQty WITH TotQty + m.TotCut IN CUTPICK
            =SEEK(&lcOrdHDr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6)+m.CTKTNO+m.CTKTLINENO)

            *-- Update allocated quantity in order line
            SELECT (lcOrdLine)
            REPLACE Cut1   WITH Cut1 + m.Cut1 ,;
              Cut2   WITH Cut2 + m.Cut2 ,;
              Cut3   WITH Cut3 + m.Cut3 ,;
              Cut4   WITH Cut4 + m.Cut4 ,;
              Cut5   WITH Cut5 + m.Cut5 ,;
              Cut6   WITH Cut6 + m.Cut6 ,;
              Cut7   WITH Cut7 + m.Cut7 ,;
              Cut8   WITH Cut8 + m.Cut8 ,;
              TotCut WITH Cut1 + Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
            *-- Update allocated quantity in order header
            SELECT (lcOrdHDr)
            REPLACE TotCut WITH TotCut + m.TotCut

            *-- Release bulk order allcation
            IF SEEK('O'+&lcOrdHDr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6),'ORDLINE')
              SELECT ORDLINE
              =RLOCK()
              REPLACE Cut1   WITH MAX(Cut1-m.Cut1,0) ,;
                Cut2   WITH MAX(Cut2-m.Cut2,0) ,;
                Cut3   WITH MAX(Cut3-m.Cut3,0) ,;
                Cut4   WITH MAX(Cut4-m.Cut4,0) ,;
                Cut5   WITH MAX(Cut5-m.Cut5,0) ,;
                Cut6   WITH MAX(Cut6-m.Cut6,0) ,;
                Cut7   WITH MAX(Cut7-m.Cut7,0) ,;
                Cut8   WITH MAX(Cut8-m.Cut8,0) ,;
                TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
              UNLOCK
              IF SEEK('O'+&lcOrdHDr..cFromOrder,'BLKORDHD')
                SELECT BlkOrdHd
                =RLOCK()
                REPLACE TotCut WITH MAX(TotCut - m.TotCut,0)
                UNLOCK
              ENDIF
            ENDIF
            laDistQty[1] = laDistQty[1] - m.Cut1
            laDistQty[2] = laDistQty[2] - m.Cut2
            laDistQty[3] = laDistQty[3] - m.Cut3
            laDistQty[4] = laDistQty[4] - m.Cut4
            laDistQty[5] = laDistQty[5] - m.Cut5
            laDistQty[6] = laDistQty[6] - m.Cut6
            laDistQty[7] = laDistQty[7] - m.Cut7
            laDistQty[8] = laDistQty[8] - m.Cut8
          ENDSCAN
        ENDIF
      ENDSCAN
      lnResult = oAriaApplication.RemoteCompanyData.SqlUpdate('CUTPICK',lcTranCode, SET("DATASESSION"),'trancd,ctktno,ctktlineno,order,style,cordline')
      IF lnResult <=0
        =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
        =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
      ELSE
        IF oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.) = 1
        ELSE
          =oAriaApplication.RemoteCompanyData.CheckRetResult("COMMITTRAN",lnResult,.T.)
        ENDIF
      ENDIF
      USE IN CUTPICK
    ENDIF
  ENDIF
ENDIF

*-- Save pament schedul.
IF ASCAN(oAriaApplication.laEvntTrig , PADR('SAV_PAY',10)) <> 0
  =gfDoTriger('SOORD',PADR('SAV_PAY',10))
ENDIF

SELECT ORDLINE
SET RELATION TO

SELECT (lcOrdLine)
SET DELETE OFF
SET ORDER TO TAG 0
GO TOP
WAIT 'Updating order lines...' WINDOW NOWAIT

llLineComission = gfGetMemVar('M_STY_COM',oAriaApplication.ActiveCompanyID)='Y'
llCompleteDate  = gfGetMemVar('M_CMPDOLN',oAriaApplication.ActiveCompanyID)
*-- Check If order warehouse, Store, Custpo, Complete date, or commission changed to modify all lines
*B128682,1 HBG 06/28/2005 Fix bug of not updating location in detail [Begin]
*!*	llModifyAllLines = cWareCode <> OrdHdr.cWareCode OR ;
*!*	                  (OrdHDr.Multi <> 'Y' AND Store <> OrdHDr.Store) OR ;
*!*	                  (!OrdHdr.MultiPo AND CustPo <> OrdHdr.CustPo) OR ;
*!*	                  (!llLineComission AND (Comm1 <> ORdHDr.Comm1 OR Comm2 <> OrdHDr.Comm2)) OR ;
*!*	                  (!llCompleteDate  AND Complete <> OrdHdr.Complete)
llModifyAllLines = cWareCode <> lcWareCode OR ;
  (&lcOrdHDr..MULTI <> 'Y' AND STORE <> &lcOrdHDr..STORE) OR ;
  (!&lcOrdHDr..MultiPo AND CustPo <> &lcOrdHDr..CustPo) OR ;
  (!llLineComission AND (Comm1 <> &lcOrdHDr..Comm1 OR Comm2 <> &lcOrdHDr..Comm2)) OR ;
  (!llCompleteDate  AND COMPLETE <> &lcOrdHDr..COMPLETE)
*B128682,1 [End]
*! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
llCancelBulk = .F.
llShowMessage = .T.
*! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[End]

*C124447,1 HBG 09/27/2004 Allow editing CustPO in line level for GMA [Begin]
IF ASCAN(loFormSet.laEvntTrig , PADR('SETFLAG',10)) <> 0
  =loFormSet.mDoTrigger(PADR('SETFLAG',10))
ENDIF
*C124447,1 [End]
*-- Get Exchange rate sign and unit sign
lcUntSin = '/'
lcExRSin = gfGetExSin(@lcUntSin, &lcOrdHDr..cCurrCode)
STORE 0 TO lnCancel,lnCancelAmt
SELECT (lcOrdLine)
*B608526,1 WAM 04/21/2008 Fix bug of saving order lines with zero quantity
*SCAN FOR IIF(llModifyAllLines OR (lcOrgStatus="B" AND &lcOrdHdr..Status<>'B'),.T.,;
Flag='M' .OR. (Flag='N' .AND. !DELETED() .AND. TotQty > 0))
SCAN FOR IIF(llModifyAllLines OR (lcOrgStatus="B" AND &lcOrdHDr..STATUS<>'B'),.T.,;
    FLAG='M' .OR. (FLAG='N' .AND. !DELETED()) ) AND IIF(FLAG='N',TotQty > 0,.T.)
  *B608526,1 WAM 04/21/2008 (End)

  *-- Check if style is assigned to new warehouse
  IF !SEEK(STYLE+&lcOrdHDr..cWareCode+SPACE(10),'StyDye')
    DO gpAdStyWar WITH STYLE,SPACE(10),&lcOrdHDr..cWareCode
    IF !EMPTY(Dyelot) AND !SEEK(STYLE+&lcOrdHDr..cWareCode+Dyelot,'StyDye')
      DO gpAdStyWar WITH STYLE,Dyelot,&lcOrdHDr..cWareCode
    ENDIF
  ENDIF

  *-- Assign order store, custpo, Complete Date, Commission, and warehouse code to all lines
  *C124447,1 HBG 09/27/2004 Allow editing CustPO in line level for GMA [Begin]
  IF ASCAN(loFormSet.laEvntTrig , PADR('SAVCSTPO',10)) <> 0
    =loFormSet.mDoTrigger(PADR('SAVCSTPO',10))
  ELSE
    *C124447,1 [End]
    *B608474,1 WAM 03/06/2008 Don't change location for picked lines
    *REPLACE cWareCode WITH &lcOrdHdr..cWareCode ,;
    Store     WITH IIF(&lcOrdHdr..Multi="Y",Store   ,&lcOrdHdr..Store) ,;
    CustPo    WITH IIF(&lcOrdHdr..MultiPo  ,CustPo  ,&lcOrdHdr..CustPo) ,;
    Complete  WITH IIF(llCompleteDate  ,Complete,&lcOrdHdr..Complete) ,;
    Comm1     WITH IIF(llLineComission ,Comm1   ,&lcOrdHdr..Comm1) ,;
    Comm2     WITH IIF(llLineComission ,Comm2   ,&lcOrdHdr..Comm2)
    REPLACE cWareCode WITH IIF(Picked,cWareCode,&lcOrdHDr..cWareCode) ,;
      STORE     WITH IIF(&lcOrdHDr..MULTI="Y",STORE   ,&lcOrdHDr..STORE) ,;
      CustPo    WITH IIF(&lcOrdHDr..MultiPo  ,CustPo  ,&lcOrdHDr..CustPo) ,;
      COMPLETE  WITH IIF(llCompleteDate  ,COMPLETE,&lcOrdHDr..COMPLETE) ,;
      Comm1     WITH IIF(llLineComission ,Comm1   ,&lcOrdHDr..Comm1) ,;
      Comm2     WITH IIF(llLineComission ,Comm2   ,&lcOrdHDr..Comm2)
    *B608474,1 WAM 03/06/2008 (End)

    *C124447,1 HBG 09/27/2004 End if GNA trigger [begin]
  ENDIF
  *C124447,1 [End]
  *-- Depleted order line. New or approved


  IF &lcOrdHDr..cOrdType='O' .AND. &lcOrdHDr..STATUS <> 'B' .AND. !DELETED()  .AND. ;
      (lcActiveMode = "A" .OR. lcOrgStatus='B') .AND. !EMPTY(cFromOrder) .AND. ;
      SEEK(cOrdType+cFromOrder+STR(BulkLineNo,6),'OrdLine')

    =SEEK('O'+cFromOrder,'BlkOrdHd','OrdHdr')

    *-- Decrease ordered quantity of the warehouse originaly assigned to the bulk
    *-- order being depleted
    =SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')
    =RLOCK('StyDye')
    REPLACE Ord1 WITH Ord1 - MIN(ORDLINE.Qty1,&lcOrdLine..Qty1),;
      Ord2 WITH Ord2 - MIN(ORDLINE.Qty2,&lcOrdLine..Qty2),;
      Ord3 WITH Ord3 - MIN(ORDLINE.Qty3,&lcOrdLine..Qty3),;
      Ord4 WITH Ord4 - MIN(ORDLINE.Qty4,&lcOrdLine..Qty4),;
      Ord5 WITH Ord5 - MIN(ORDLINE.Qty5,&lcOrdLine..Qty5),;
      Ord6 WITH Ord6 - MIN(ORDLINE.Qty6,&lcOrdLine..Qty6),;
      Ord7 WITH Ord7 - MIN(ORDLINE.Qty7,&lcOrdLine..Qty7),;
      Ord8 WITH Ord8 - MIN(ORDLINE.Qty8,&lcOrdLine..Qty8),;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
    UNLOCK IN 'StyDye'
    *-- Update the Qty on the Configuration level
    IF !EMPTY(ORDLINE.Dyelot) AND SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+ORDLINE.Dyelot,'StyDye')
      =RLOCK('StyDye')
      REPLACE Ord1 WITH Ord1 - MIN(ORDLINE.Qty1,&lcOrdLine..Qty1),;
        Ord2 WITH Ord2 - MIN(ORDLINE.Qty2,&lcOrdLine..Qty2),;
        Ord3 WITH Ord3 - MIN(ORDLINE.Qty3,&lcOrdLine..Qty3),;
        Ord4 WITH Ord4 - MIN(ORDLINE.Qty4,&lcOrdLine..Qty4),;
        Ord5 WITH Ord5 - MIN(ORDLINE.Qty5,&lcOrdLine..Qty5),;
        Ord6 WITH Ord6 - MIN(ORDLINE.Qty6,&lcOrdLine..Qty6),;
        Ord7 WITH Ord7 - MIN(ORDLINE.Qty7,&lcOrdLine..Qty7),;
        Ord8 WITH Ord8 - MIN(ORDLINE.Qty8,&lcOrdLine..Qty8),;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
      UNLOCK IN 'StyDye'
    ENDIF
    *-- Decrease style ordered quantity by the quantity depleted from the bulk order
    =SEEK(ORDLINE.STYLE,'Style')
    =RLOCK('Style')
    REPLACE Ord1   WITH Ord1 - MIN(ORDLINE.Qty1,&lcOrdLine..Qty1),;
      Ord2   WITH Ord2 - MIN(ORDLINE.Qty2,&lcOrdLine..Qty2),;
      Ord3   WITH Ord3 - MIN(ORDLINE.Qty3,&lcOrdLine..Qty3),;
      Ord4   WITH Ord4 - MIN(ORDLINE.Qty4,&lcOrdLine..Qty4),;
      Ord5   WITH Ord5 - MIN(ORDLINE.Qty5,&lcOrdLine..Qty5),;
      Ord6   WITH Ord6 - MIN(ORDLINE.Qty6,&lcOrdLine..Qty6),;
      Ord7   WITH Ord7 - MIN(ORDLINE.Qty7,&lcOrdLine..Qty7),;
      Ord8   WITH Ord8 - MIN(ORDLINE.Qty8,&lcOrdLine..Qty8),;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN STYLE
    UNLOCK IN 'Style'

    *-- Quantity to be deducted from bulk order line
    lnCanBulk = MIN(ORDLINE.Qty1,Qty1)+MIN(ORDLINE.Qty2,Qty2)+MIN(ORDLINE.Qty3,Qty3)+;
      MIN(ORDLINE.Qty4,Qty4)+MIN(ORDLINE.Qty5,Qty5)+MIN(ORDLINE.Qty6,Qty6)+;
      MIN(ORDLINE.Qty7,Qty7)+MIN(ORDLINE.Qty8,Qty8)

    *-- Total quantity & amount to be deducted from bulk order
    lnCancel    = lnCancel    + lnCanBulk
    lnCancelAmt = lnCancelAmt + lnCanBulk*ORDLINE.Price
    *-- Deducted amount in base currency
    lnOpmAmnt   = lnCanBulk*ORDLINE.Price &lcExRSin BlkOrdHd.nExRate &lcUntSin BlkOrdHd.nCurrUnit

    *! B609797,1 MMT 01/26/2012 cannot allocate order created from Allocated Bulk order[T20111206.0020][Start]
    IF !EMPTY(Ordline.Piktkt) AND Ordline.PIKTKT <> REPLICATE('*',FSIZE('PIKTKT','Ordline'))
      lnCurSel = SELECT(0)
      IF !USED('PIKLINE')
        =gfOpenFile(oAriaApplication.DataDir+'PIKLINE',oAriaApplication.DataDir+'PIKLINE','SH')
      ENDIF
      SELECT Ordline
      IF !SEEK(Ordline.PikTkt+Ordline.ORDER+STR(Ordline.LINENO,6),'PIKLINE','PIKLINE')
        SCATTER MEMO MEMVAR
        INSERT INTO PIKLINE FROM MEMVAR
      ENDIF
      FOR lnCntSz = 1 TO 8
        lcCntSz = STR(lnCntSz,1)
        =SEEK(ORDLINE.STYLE,'Style')
        REPLACE alo&lcCntSz. WITH alo&lcCntSz. - Ordline.PIK&lcCntSz. ,;
          TotAlo WITH TotAlo  - Ordline.PIK&lcCntSz.  IN STYLE

        =SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')
        REPLACE alo&lcCntSz. WITH alo&lcCntSz. - Ordline.PIK&lcCntSz.,;
          TotAlo WITH TotAlo  - Ordline.PIK&lcCntSz. IN STYDYE
        IF !EMPTY(ORDLINE.Dyelot) AND SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+ORDLINE.Dyelot,'StyDye')
          REPLACE alo&lcCntSz. WITH alo&lcCntSz. - Ordline.PIK&lcCntSz.,;
            TotAlo WITH TotAlo  - Ordline.PIK&lcCntSz. IN STYDYE
        ENDIF
        REPLACE PIK&lcCntSz. WITH 0 IN Ordline
      ENDFOR
      lcPiktktNo = Ordline.Piktkt

      REPLACE TOTPIK WITH 0,;
        picked WITH .F.,;
        Piktkt WITH '',;
        pikdate WITH {} IN Ordline

      SELECT Ordline
      lcOrderLIndex = ORDER()
      lcBulkOrderNum = ORDLINE.ORDER
      lcBulkOrderType = ORDLINE.cOrdType
      lcCurOrdLKey = EVALUATE(KEY())
      SET ORDER TO ORDLINE   && CORDTYPE+ORDER+STR(LINENO,6)
      =SEEK(lcBulkOrderType +lcBulkOrderNum)
      LOCATE REST WHILE CORDTYPE+ORDER+STR(LINENO,6)  = lcBulkOrderType +lcBulkOrderNum FOR piktkt = lcPiktktNo
      IF !FOUND()
        IF SEEK(lcPiktktNo ,'PIKTKT','PIKTKT')
          REPLACE STATUS WITH 'X' IN PIKTKT
        ENDIF
      ENDIF
      SELECT Ordline
      IF !EMPTY(lcOrderLIndex)
        SET ORDER TO (lcOrderLIndex)
      ENDIF
      IF !EMPTY(lcCurOrdLKey)
        =SEEK(lcCurOrdLKey,'Ordline')
      ENDIF
      SELECT(lnCurSel)
    ENDIF
    *! B609797,1 MMT 01/26/2012 cannot allocate order created from Allocated Bulk order[T20111206.0020][END]

    *-- Decrease Depleted bulk orders Quantity & Amount in the Style History file
    *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
    *IF SEEK(STYLE+lcBulkYear,'icStyHst')
    old_Alias   = ALIAS()
    lcStyle_Var = STYLE
    SELECT icStyHst
    *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][Begin]
    *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
    *IF !gfSEEK(lcStyle_Var+ lcBulkYear)
    IF !SEEK(lcStyle_Var+ lcBulkYear,lcICStyHstTable) AND !gfSEEK(lcStyle_Var+ lcBulkYear,'icStyHst')
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
      =gfOpenTable('ICSTYHST','STYHST','SH','ICSTYHSX')
      SELECT ICSTYHSX

      APPEND BLANK
      REPLACE STYLE WITH lcStyle_Var
      REPLACE CFISFYEAR  WITH lcBulkYear
      = GFREPLACE('')
      =gfTABLEUPDATE()

      *USE IN ICSTYHSX
      =gfcloseTable('ICSTYHSX')
      SELECT ICSTYHST
    ENDIF
    *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][End]
    *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
    *IF gfSEEK(lcStyle_Var+lcBulkYear)
    IF SEEK(lcStyle_Var+lcBulkYear,lcICStyHstTable)  OR gfSEEK(lcStyle_Var+lcBulkYear,'ICSTYHST')
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
      SELECT (old_alias)
      *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
      IF !SEEK(lcStyle_Var+lcBulkYear,lcICStyHstTable)
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
        =RLOCK("icStyHst")
        REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCanBulk ,;
          nOrdQty           WITH nOrdQty           - lnCanBulk ,;
          nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOpmAmnt ,;
          nOrdAmt           WITH nOrdAmt           - lnOpmAmnt IN icStyHst
        UNLOCK IN icStyHst
        *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
        SELECT icStyHst
        =gfreplace('')
        *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
      ELSE
        REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCanBulk ,;
          nOrdQty           WITH nOrdQty           - lnCanBulk ,;
          nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOpmAmnt ,;
          nOrdAmt           WITH nOrdAmt           - lnOpmAmnt IN (lcICStyHstTable)
      ENDIF
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
    ENDIF
    *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
    SELECT (old_Alias)
    *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]

    *-- Decrease Bulk order line open and book quantity
    =RLOCK("ORDLINE")
    REPLACE Qty1   WITH MAX(Qty1-&lcOrdLine..Qty1,0),;
      Qty2   WITH MAX(Qty2-&lcOrdLine..Qty2,0),;
      Qty3   WITH MAX(Qty3-&lcOrdLine..Qty3,0),;
      Qty4   WITH MAX(Qty4-&lcOrdLine..Qty4,0),;
      Qty5   WITH MAX(Qty5-&lcOrdLine..Qty5,0),;
      Qty6   WITH MAX(Qty6-&lcOrdLine..Qty6,0),;
      Qty7   WITH MAX(Qty7-&lcOrdLine..Qty7,0),;
      Qty8   WITH MAX(Qty8-&lcOrdLine..Qty8,0),;
      TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 IN ORDLINE
    *-- Decrease Bulk order line open and book quantity
    REPLACE Book1   WITH MAX(Book1-&lcOrdLine..Book1,0),;
      Book2   WITH MAX(Book2-&lcOrdLine..Book2,0),;
      Book3   WITH MAX(Book3-&lcOrdLine..Book3,0),;
      Book4   WITH MAX(Book4-&lcOrdLine..Book4,0),;
      Book5   WITH MAX(Book5-&lcOrdLine..Book5,0),;
      Book6   WITH MAX(Book6-&lcOrdLine..Book6,0),;
      Book7   WITH MAX(Book7-&lcOrdLine..Book7,0),;
      Book8   WITH MAX(Book8-&lcOrdLine..Book8,0),;
      TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 IN ORDLINE
    UNLOCK IN ORDLINE
  ENDIF

  *-- For Non-Bid orders, Update style ordered quantity in the assigned warehouse

  *B609141,1 WAM 02/11/2010 Update style ordered quantity when update WEB order
  *IF &lcOrdHdr..cOrdType='O' .AND. &lcOrdHdr..Status <> 'B'
  IF (&lcOrdHDr..cOrdType='O' OR (&lcOrdHDr..cOrdType='T' AND &lcOrdHDr..lFromWeb) ) .AND. &lcOrdHDr..STATUS <> 'B'
    *B609141,1 WAM 02/11/2010 (End)

    *-- Modifed order line
    IF lcOrgStatus <> 'B' AND SEEK(cOrdType+ORDER+STR(LINENO,6),'OrdLine')
      *-- Deduct original order line quantity
      =SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')
      =RLOCK('StyDye')
      REPLACE Ord1   WITH Ord1   - ORDLINE.Qty1 ,;
        Ord2   WITH Ord2   - ORDLINE.Qty2 ,;
        Ord3   WITH Ord3   - ORDLINE.Qty3 ,;
        Ord4   WITH Ord4   - ORDLINE.Qty4 ,;
        Ord5   WITH Ord5   - ORDLINE.Qty5 ,;
        Ord6   WITH Ord6   - ORDLINE.Qty6 ,;
        Ord7   WITH Ord7   - ORDLINE.Qty7 ,;
        Ord8   WITH Ord8   - ORDLINE.Qty8 ,;
        TotOrd WITH TotOrd - ORDLINE.TotQty IN StyDye
      UNLOCK IN 'StyDye'
      *-- Update the Qty on the Configuration level [Begin]
      IF !EMPTY(ORDLINE.Dyelot) AND SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+ORDLINE.Dyelot,'StyDye')
        =RLOCK('StyDye')
        REPLACE Ord1   WITH Ord1   - ORDLINE.Qty1 ,;
          Ord2   WITH Ord2   - ORDLINE.Qty2 ,;
          Ord3   WITH Ord3   - ORDLINE.Qty3 ,;
          Ord4   WITH Ord4   - ORDLINE.Qty4 ,;
          Ord5   WITH Ord5   - ORDLINE.Qty5 ,;
          Ord6   WITH Ord6   - ORDLINE.Qty6 ,;
          Ord7   WITH Ord7   - ORDLINE.Qty7 ,;
          Ord8   WITH Ord8   - ORDLINE.Qty8 ,;
          TotOrd WITH TotOrd - ORDLINE.TotQty IN StyDye
        UNLOCK IN 'StyDye'
      ENDIF
      *-- Deduct original order line quantity
      =SEEK(ORDLINE.STYLE,'Style')
      =RLOCK('Style')
      REPLACE  Ord1   WITH Ord1   - ORDLINE.Qty1 ,;
        Ord2   WITH Ord2   - ORDLINE.Qty2 ,;
        Ord3   WITH Ord3   - ORDLINE.Qty3 ,;
        Ord4   WITH Ord4   - ORDLINE.Qty4 ,;
        Ord5   WITH Ord5   - ORDLINE.Qty5 ,;
        Ord6   WITH Ord6   - ORDLINE.Qty6 ,;
        Ord7   WITH Ord7   - ORDLINE.Qty7 ,;
        Ord8   WITH Ord8   - ORDLINE.Qty8 ,;
        TotOrd WITH TotOrd - ORDLINE.TotQty IN STYLE
      UNLOCK IN 'Style'
      *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
      *IF SEEK(ORDLINE.STYLE+lcGlYear,'icStyHst')
      old_Alias = ALIAS()
      SELECT icStyHst
      *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][Begin]
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
      *IF !gfSEEK(ORDLINE.STYLE+ lcGlYear)
      IF !SEEK(ORDLINE.STYLE+ lcGlYear,lcICStyHstTable) AND !gfSEEK(ORDLINE.STYLE+ lcGlYear,'icStyHst')
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
        =gfOpenTable('ICSTYHST','STYHST','SH','ICSTYHSX')
        SELECT ICSTYHSX

        APPEND BLANK
        REPLACE STYLE WITH ORDLINE.STYLE
        REPLACE CFISFYEAR  WITH lcGlYear
        = GFREPLACE('')
        =gfTABLEUPDATE()

        *USE IN ICSTYHSX
        =gfcloseTable('ICSTYHSX')
        SELECT ICSTYHST

      ENDIF
      *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][End]
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
      *IF gfSEEK(ORDLINE.STYLE+lcGlYear)
      IF SEEK(ORDLINE.STYLE+lcGlYear,lcICStyHstTable) OR gfSEEK(ORDLINE.STYLE+lcGlYear,'ICSTYHST')
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
        SELECT (old_Alias )
        *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
        lnOrdAmt = ORDLINE.TotQty*ORDLINE.Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
        IF !SEEK(ORDLINE.STYLE+lcGlYear,lcICStyHstTable)
          *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
          =RLOCK("icStyHst")
          REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - ORDLINE.TotQty ,;
            nOrdQty              WITH nOrdQty- ORDLINE.TotQty ,;
            nOrdAmt&lcGlPeriod   WITH nOrdAmt&lcGlPeriod - lnOrdAmt ,;
            nOrdAmt              WITH nOrdAmt - lnOrdAmt IN icStyHst
          UNLOCK IN icStyHst
          *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
          SELECT icstyhst
          =gfreplace('')
          *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
          *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
        ELSE
          REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - ORDLINE.TotQty ,;
            nOrdQty              WITH nOrdQty- ORDLINE.TotQty ,;
            nOrdAmt&lcGlPeriod   WITH nOrdAmt&lcGlPeriod - lnOrdAmt ,;
            nOrdAmt              WITH nOrdAmt - lnOrdAmt IN (lcICStyHstTable)
        ENDIF
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
      ENDIF
      *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
      SELECT (Old_Alias)
      *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
    ENDIF

    *-- Increase style ordered quantity
    IF !DELETED(lcOrdLine)
      =SEEK(&lcOrdLine..STYLE+&lcOrdLine..cWareCode+SPACE(10),'StyDye')
      =RLOCK('StyDye')
      REPLACE Ord1   WITH Ord1   + &lcOrdLine..Qty1 ,;
        Ord2   WITH Ord2   + &lcOrdLine..Qty2 ,;
        Ord3   WITH Ord3   + &lcOrdLine..Qty3 ,;
        Ord4   WITH Ord4   + &lcOrdLine..Qty4 ,;
        Ord5   WITH Ord5   + &lcOrdLine..Qty5 ,;
        Ord6   WITH Ord6   + &lcOrdLine..Qty6 ,;
        Ord7   WITH Ord7   + &lcOrdLine..Qty7 ,;
        Ord8   WITH Ord8   + &lcOrdLine..Qty8 ,;
        TotOrd WITH TotOrd + &lcOrdLine..TotQty IN StyDye
      UNLOCK IN 'StyDye'
      *-- Update the Qty on the Configuration level
      IF !EMPTY(&lcOrdLine..Dyelot) AND SEEK(&lcOrdLine..STYLE+&lcOrdLine..cWareCode+&lcOrdLine..Dyelot,'StyDye')
        REPLACE Ord1   WITH Ord1   + &lcOrdLine..Qty1 ,;
          Ord2   WITH Ord2   + &lcOrdLine..Qty2 ,;
          Ord3   WITH Ord3   + &lcOrdLine..Qty3 ,;
          Ord4   WITH Ord4   + &lcOrdLine..Qty4 ,;
          Ord5   WITH Ord5   + &lcOrdLine..Qty5 ,;
          Ord6   WITH Ord6   + &lcOrdLine..Qty6 ,;
          Ord7   WITH Ord7   + &lcOrdLine..Qty7 ,;
          Ord8   WITH Ord8   + &lcOrdLine..Qty8 ,;
          TotOrd WITH TotOrd + &lcOrdLine..TotQty IN StyDye
        UNLOCK IN 'StyDye'
      ENDIF
      =SEEK(&lcOrdLine..STYLE,'Style')
      =RLOCK('Style')
      REPLACE  Ord1   WITH Ord1   + &lcOrdLine..Qty1 ,;
        Ord2   WITH Ord2   + &lcOrdLine..Qty2 ,;
        Ord3   WITH Ord3   + &lcOrdLine..Qty3 ,;
        Ord4   WITH Ord4   + &lcOrdLine..Qty4 ,;
        Ord5   WITH Ord5   + &lcOrdLine..Qty5 ,;
        Ord6   WITH Ord6   + &lcOrdLine..Qty6 ,;
        Ord7   WITH Ord7   + &lcOrdLine..Qty7 ,;
        Ord8   WITH Ord8   + &lcOrdLine..Qty8 ,;
        TotOrd WITH TotOrd + &lcOrdLine..TotQty IN STYLE
      UNLOCK IN 'Style'

      *-- Update orders Quantity & Amount in the Style History file
      *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
      *IF SEEK(STYLE+lcGlYear,'icStyHst')
      old_Alias = ALIAS()
      lcStyle_Var = STYLE
      SELECT icStyHst
      *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][Begin]
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
      *!*	      IF !IIF(lcActiveMode="E",SEEK(lcStyle_Var+ lcGlYear)        , gfSEEK(lcStyle_Var+ lcGlYear)        )
      *!*
      *!*	        IF lcActiveMode=="E"
      *!*	          SELECT ICSTYHST
      *!*	        ELSE
      IF !SEEK(lcStyle_Var+ lcGlYear,lcICStyHstTable) AND !gfSEEK(lcStyle_Var+ lcGlYear,'icStyHst')
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
        =gfOpenTable('ICSTYHST','STYHST','SH','ICSTYHSX')
        SELECT ICSTYHSX
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
        *ENDIF
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
        APPEND BLANK
        REPLACE STYLE WITH lcStyle_Var
        REPLACE CFISFYEAR  WITH lcGlYear
        = GFREPLACE('')
        =gfTABLEUPDATE()
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
        *IF !(lcActiveMode=="E")
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
        *USE IN ICSTYHSX
        =gfcloseTable('ICSTYHSX')
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
        *ENDIF
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
        SELECT ICSTYHST
      ENDIF
      *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][End]
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
      *!*	      IF IIF(lcActiveMode="E",SEEK(lcStyle_Var+lcGlYear,'icStyHst')        , gfSEEK(lcStyle_Var+lcGlYear,'icStyHst')        )
      *!*	        SELECT (old_Alias )
      IF SEEK(lcStyle_Var+ lcGlYear,lcICStyHstTable) OR gfSEEK(lcStyle_Var+lcGlYear,'icStyHst')
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
        *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
        lnOrdAmt = &lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin &lcOrdHDr..nExRate &lcUntSin &lcOrdHDr..nCurrUnit
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
        IF !SEEK(lcStyle_Var+ lcGlYear,lcICStyHstTable)
          *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
          =RLOCK("icStyHst")
          REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + &lcOrdLine..TotQty ,;
            nOrdQty            WITH nOrdQty + &lcOrdLine..TotQty ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
            nOrdAmt            WITH nOrdAmt + lnOrdAmt IN icStyHst
          UNLOCK IN icStyHst
          *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
          SELECT icstyhst
          = gfReplace('')
          *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
          *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
        ELSE
          REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + &lcOrdLine..TotQty ,;
            nOrdQty            WITH nOrdQty + &lcOrdLine..TotQty ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
            nOrdAmt            WITH nOrdAmt + lnOrdAmt IN (lcICStyHstTable)
        ENDIF
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
      ENDIF
      *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
      SELECT (Old_Alias)
      *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
    ENDIF
  ENDIF

  *-- Update pick ticket store and cust PO# if changed for picked order lines
  IF Picked .AND. SEEK(PikTkt,'PikTkt')
    =RLOCK("PikTkt")
    REPLACE STORE  WITH &lcOrdLine..STORE ,;
      CustPo WITH &lcOrdLine..CustPo IN PikTkt
    UNLOCK IN PikTkt
  ENDIF
  =SEEK(cOrdType+ORDER+STR(LINENO,6),'OrdLine')

  *-- Update Order lines canceled quantity
  IF lcActiveMode="E"  AND SEEK(cOrdType+ORDER+STR(LINENO,6),lcOrdCanLn)
    SELECT (lcOrdCanLn)
    SCATTER TO laCanRec

    *! B608600,1 MMT 07/01/2008 Fix bug of repeated lines in OORDCANLN File [Start]
    *! B609208,1 MMT 04/14/2010 Fix bug of cancelled records of same Order are Overwritten [Start]
    * IF !SEEK(cOrdType+Order+STR(LineNo,6),'ORDCANLN','ORDCANLN')
    *! B609208,1 MMT 04/14/2010 Fix bug of cancelled records of same Order are Overwritten [End]
    *! B608600,1 MMT 07/01/2008 Fix bug of repeated lines in OORDCANLN File [End]

    INSERT INTO ORDCANLN FROM ARRAY laCanRec

    *! B608600,1 MMT 07/01/2008 Fix bug of repeated lines in OORDCANLN File [Start]
    *! B609208,1 MMT 04/14/2010 Fix bug of cancelled records of same Order are Overwritten [Start]
    *!*	    ELSE
    *!*	      SELECT ORDCANLN
    *!*	      GATHER FROM laCanRec
    *!*	    ENDIF
    *! B609208,1 MMT 04/14/2010 Fix bug of cancelled records of same Order are Overwritten [END]
    *! B608600,1 MMT 07/01/2008 Fix bug of repeated lines in OORDCANLN File [End]

  ENDIF
  *! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[Start]
  IF !EMPTY(&lcOrdLine..Employee)
    lfUpStyHist()
  ENDIF
  *! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[End]

  SELECT (lcOrdLine)
  *-- Update order line
  SCATTER MEMVAR MEMO
  m.Start = &lcOrdHDr..START
  m.Flag  = SPACE(1)
  m.ORDER = lcOrderNo
  *! E302763,1 MMT 09/21/2010 modify SO Screen to apply the trade discount per line[Start]
  IF llTrdDscLn
    m.Disc_Pcnt = 0
    m.Price = m.gros_price
  ENDIF
  *! E302763,1 MMT 09/21/2010 modify SO Screen to apply the trade discount per line[End]
  DO CASE
  CASE !DELETED() .AND. !SEEK(cOrdType+ORDER+STR(LINENO,6),'OrdLine')
    lnLineCount = lnLineCount + 1

    *B612506,1 MMT 12/13/2021 Prevent Ordline LineNo duplication[T20211203.0001][Start]
    DO WHILE SEEK(cOrdType+ORDER+STR(lnLineCount ,6),'OrdLine')
      lnLineCount = lnLineCount + 1
    ENDDO 
    *B612506,1 MMT 12/13/2021 Prevent Ordline LineNo duplication[T20211203.0001][End]
    
    m.LineNo    = lnLineCount
    INSERT INTO ORDLINE FROM MEMVAR

    *! C200981,1 MMT 04/10/2008 Fix problem of wrong line No. in SOCODES File(Start)
    IF ASCAN(loFormSet.laEvntTrig , PADR('UPDCODLIN',10)) <> 0
      =loFormSet.mDoTrigger(PADR('UPDCODLIN',10))
    ENDIF
    *! C200981,1 MMT 04/10/2008 Fix problem of wrong line No. in SOCODES File(End)

    *! B609265,1 MMT 05/24/2010 Sales order Saving update lineNo incorrectly in ORDDSGN table[Start]
    IF llBomVarnt
      lcOldAlias = SELECT(0)
      SELECT (lcT_BomVar)
      IF SEEK(&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6))
        SCAN REST WHILE ORDER+CORDLINE+STR(LINENO,6)  = &lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6) FOR !DELETED()
          SCATTER MEMO MEMVAR
          m.CORDLINE = STR(lnLineCount,6)
          m.ORDER = lcOrderNo
          *B609402,1 SMA 09/05/2010 fix bug of SO- Orddgsn table standard fields not updating.....[BEGIN]
          m.cAdd_USer = oAriaApplication.User_ID
          m.cAdd_Time = gfGettime()
          m.dAdd_Date = oAriaApplication.SystemDate
          *B609402,1 SMA 09/05/2010 fix bug of SO- Orddgsn table standard fields not updating.....[END]
          INSERT INTO ORDDSGN FROM MEMVAR
        ENDSCAN
      ENDIF
      SELECT(lcOldAlias)
    ENDIF
    *! B609265,1 MMT 05/24/2010 Sales order Saving update lineNo incorrectly in ORDDSGN table[End]


  CASE !DELETED() .AND. SEEK(cOrdType+ORDER+STR(LINENO,6),'OrdLine')
    *HBG 12/22/2004 Fix bug of changing the order type in case of EDI sales order [begin]
    *m.cOrdType  = IIF(OrdHdr.lEdiOrder OR OrdHdr.lFromWeb AND OrdHdr.Status <> 'B','O',m.cOrdType)
    *B126848,1 HBG 03/20/2005 Fix bug of Updating the web orders always even if the status not changed [begin]
    *m.cOrdType  = IIF(llFromEDI OR (OrdHdr.lFromWeb AND OrdHdr.Status <> 'B'),'O',m.cOrdType)
    m.cOrdType  = IIF(llFromEDI OR (&lcOrdHDr..lFromWeb AND &lcOrdHDr..STATUS $ 'HO' AND loFormSet.lcOldStatus = 'B'),'O',m.cOrdType)
    *B126848,1 [End]
    *HBG [End]
    lnLineCount = IIF(llFromEDI,m.LineNo,lnLineCount)
    IF llMFDsPrc .OR. llPODsPrc
      =lfUpdtPoCT(m.ORDER,m.STYLE,STR(m.LineNo,6),m.TotQty,m.Price,ORDLINE.Price,ORDLINE.TotQty,.F.)
    ENDIF
    *B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [Begin]
    && Modified lines
    SELECT ORDLINE
    CurRecOrdLine = ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
    && Adjust Style And StyDye Record Pointers.
    = SEEK(ORDLINE.STYLE,'Style')
    = SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')

    FOR innerstr = 1 TO 8
      cstr = ALLTRIM(STR(innerstr))
      OldQTY&cstr.  = ORDLINE.Qty&cstr.
      OldBook&cstr. = ORDLINE.Book&cstr.
    ENDFOR

    lcFromOrder = ORDLINE.cFromOrder
    lBulkLineNo = ORDLINE.BulkLineNo
    lcDyelot    = ORDLINE.Dyelot
    lcSTYLE     = ORDLINE.STYLE
    lcWareCode  = ORDLINE.cWareCode
    IF !EMPTY(lcFromOrder) .AND. SEEK(cOrdType+lcFromOrder+STR(lBulkLineNo,6),'OrdLine') .AND. SEEK(cOrdType+lcFromOrder,'BlkOrdHd','OrdHdr')
      *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
      STORE 0 TO lnQtyB1,lnQtyB2,lnQtyB3,lnQtyB4,lnQtyB5,lnQtyB6,lnQtyB7,lnQtyB8
      *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]

      lnOpen      = 0
      lnOpenAmt   = 0
      lnCancel    = 0
      lnCancelAmt = 0

      FOR innerLoopCounter = 1 TO 8
        cstr  = ALLTRIM(STR(innerLoopCounter))
        xDiff = m.Qty&cstr. - OldQTY&cstr.
        lnOldBLKQTY = ORDLINE.Qty&cstr.
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        IF llShowMessage AND xDiff < 0
          IF  gfModalGen('QRM52026B32000','DIALOG',ordhdr.cfromorder) =1
            llCancelBulk = .T.
          ENDIF
          llShowMessage = .F.
        ENDIF
        SELECT (lcOrdLine)
        IF !llCancelBulk  OR xDiff > 0
          *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]
          REPLACE Qty&cstr.   WITH MAX((Qty&cstr. - xDiff),0) IN ORDLINE
          REPLACE TotQty      WITH MAX((TotQty - xDiff),0)    IN ORDLINE
          lnOpen    = lnOpen + xDiff
          lnOpenAmt = lnOpenAmt + (xDiff * ORDLINE.Price)
          *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
          lnQtyB&cstr = 0
        ELSE
          *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][Start]
          *lnQtyB&cstr = ABS(xDiff)
          *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][End]
        ENDIF
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]

        *! B610297,1 HIA 04/15/2013 T20121226.0001 - ARIA EDI : Received some edi release po’s for some edi bulk [Start]
        *xDiff = ( m.Book&cstr. - OldBook&cstr. )
        *! B610297,1 HIA 04/15/2013 T20121226.0001 - ARIA EDI : Received some edi release po’s for some edi bulk [End]

        REPLACE Book&cstr.  WITH MAX((Book&cstr. - xDiff),0) IN ORDLINE
        REPLACE TotBook     WITH MAX((TotBook - xDiff),0)    IN ORDLINE
        lnCancel = lnCancel + xDiff
        lnCancelAmt = lnCancelAmt + (xDiff * ORDLINE.Price)
        IF NOT((lnOldBLKQTY = 0 ) AND (SIGN(xDiff) = 1)) THEN
          =RLOCK('Style')
          REPLACE  Ord&cstr.   WITH Ord&cstr. + (OrdLine.QTY&cstr. - lnOldBLKQTY ) ,;
            TotOrd      WITH TotOrd    + (OrdLine.QTY&cstr. - lnOldBLKQTY) IN STYLE
          UNLOCK IN 'Style'

          =RLOCK('StyDye')
          REPLACE Ord&cstr.  WITH Ord&cstr. + (OrdLine.QTY&cstr. - lnOldBLKQTY)  ,;
            TotOrd     WITH TotOrd    + (OrdLine.QTY&cstr. - lnOldBLKQTY) IN StyDye
          UNLOCK IN 'StyDye'
          *-- Update the Qty on the Configuration level
          IF !EMPTY(lcDyelot) AND SEEK(lcSTYLE+lcWareCode+lcDyelot,'StyDye')
            REPLACE Ord&cstr.   WITH Ord&cstr. + (OrdLine.QTY&cstr. - lnOldBLKQTY),;
              TotOrd      WITH TotOrd    + (OrdLine.QTY&cstr. - lnOldBLKQTY) IN StyDye
            UNLOCK IN 'StyDye'
          ENDIF
        ENDIF

      ENDFOR

      && Modify the Bulk Order Header Totals And Adjust The Status Depend On The Totals values
      =RLOCK("BlkOrdHd")
      SELECT BlkOrdHd
      *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
      IF !llCancelBulk
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[end]
        REPLACE Book      WITH MAX(Book    - lnCancel,0)   ,;
          BookAmt   WITH MAX(BookAmt - lnCancelAmt,0),;
          OPEN      WITH MAX(OPEN    - lnOpen,0)     ,;
          OpenAmt   WITH MAX(OpenAmt - lnOpenAmt,0)  ,;
          STATUS    WITH IIF(OPEN = 0 ,'X',STATUS) ,;
          APPRAMT   WITH MAX(0,APPRAMT - lnOpenAmt) IN BlkOrdHd
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
      ELSE
        *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][Start]
        *!*	        IF lnQtyB1 > 0 OR lnQtyB2 > 0 OR lnQtyB3 > 0 OR lnQtyB4 > 0 OR lnQtyB5 > 0 OR lnQtyB6 > 0 OR lnQtyB7 > 0 OR lnQtyB8 > 0
        *!*	          SELECT BlkOrdHd
        *!*	          REPLACE CANCEL    WITH CANCEL + lnQtyB1+lnQtyB2+lnQtyB3+lnQtyB4+lnQtyB5+lnQtyB6+lnQtyB7+lnQtyB8  ,;
        *!*	            CANCELAmt   WITH CANCELAmt + (lnQtyB1+lnQtyB2+lnQtyB3+lnQtyB4+lnQtyB5+lnQtyB6+lnQtyB7+lnQtyB8)*Ordline.price,;
        *!*	            Book      WITH Book + lnQtyB1+lnQtyB2+lnQtyB3+lnQtyB4+lnQtyB5+lnQtyB6+lnQtyB7+lnQtyB8  ,;
        *!*	            BookAmt   WITH BookAmt + (lnQtyB1+lnQtyB2+lnQtyB3+lnQtyB4+lnQtyB5+lnQtyB6+lnQtyB7+lnQtyB8)*Ordline.price,;
        *!*	            STATUS    WITH IIF(OPEN = 0 ,'X',STATUS) ,;
        *!*	            APPRAMT   WITH MAX(0,APPRAMT - lnOpenAmt) IN BlkOrdHd
        *!*	          SELECT ORDCANLN
        *!*	          APPEND BLANK
        *!*	          REPLACE STYLE WITH Ordline.STYLE,;
        *!*	            Qty1  WITH lnQtyB1,;
        *!*	            Qty2  WITH lnQtyB2,;
        *!*	            Qty3  WITH lnQtyB3,;
        *!*	            Qty4  WITH lnQtyB4,;
        *!*	            Qty5  WITH lnQtyB5,;
        *!*	            Qty6  WITH lnQtyB6,;
        *!*	            Qty7  WITH lnQtyB7,;
        *!*	            Qty8  WITH lnQtyB8,;
        *!*	            TotQty  WITH lnQtyB1+lnQtyB2+lnQtyB3+lnQtyB4+lnQtyB5+lnQtyB6+lnQtyB7+lnQtyB8,;
        *!*	            cordtype WITH BlkOrdHd.cordtype,;
        *!*	            ORDER WITH BlkOrdHd.ORDER,;
        *!*	            Account WITH BlkOrdHd.Account,;
        *!*	            STORE  WITH Ordline.STORE,;
        *!*	            Price  WITH Ordline.Price,;
        *!*	            Dyelot WITH Ordline.Dyelot,;
        *!*	            LINENO WITH Ordline.LINENO
        *!*	          =gfAdd_Info('ORDCANLN')
        *!*	        ENDIF
        *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][End]
      ENDIF
      *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]

      UNLOCK IN BlkOrdHd

      && Re-Open the bulk if the order line qty decreaded and its status was 'X'
      IF BlkOrdHd.STATUS='X' .AND. (BlkOrdHd.OPEN > 0 ).AND. SEEK('M'+BlkOrdHd.Account,'Customer')
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        STORE 0 TO lnOpenBlk ,lnOpenBlkAmt ,lnBookBlk ,lnBookBlkAmt
        lfRebalBulkOrder(BlkOrdHd.ORDER,Ordline.LINENO)
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[End]

        =RLOCK("BlkOrdHd")
        REPLACE STATUS WITH "O" IN BlkOrdHd
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        REPLACE OPEN WITH lnOpenBlk ,;
          OpenAmt WITH  lnOpenBlkAmt ,;
          Book WITH lnBookBlk ,;
          BookAmt WITH lnBookBlkAmt  IN BlkOrdHd
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[End]

        UNLOCK IN BlkOrdHd

        =RLOCK('Customer')
        REPLACE nBulk WITH nBulk + 1 IN Customer
        UNLOCK IN 'Customer'
      ENDIF

      && change the bulk order status to "X" if its open = 0
      *! B610877,1 MMT 09/07/2014 Fix of error while editing sales order and lines got missed[T20150730.0007][Start]
      *IF BlkOrdHd.STATUS='X' .AND. SEEK('M'+BlkOrdHd.Account,'Customer')
      IF BlkOrdHd.STATUS='X' .AND. BlkOrdHd.STATUS <> OLDVAL('STATUS','BlkOrdHd') .AND. SEEK('M'+BlkOrdHd.Account,'Customer')
      *! B610877,1 MMT 09/07/2014 Fix of error while editing sales order and lines got missed[T20150730.0007][End]
        =RLOCK('Customer')
        REPLACE nBulk WITH nBulk - 1 IN Customer
        UNLOCK IN 'Customer'
      ENDIF

    ENDIF
    SELECT ORDLINE
    =SEEK(CurRecOrdLine,"ORDLINE")

    *B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [End]
    SELECT ORDLINE
    IF DELETED()
      RECALL
    ENDIF
    GATHER MEMVAR MEMO

    *! B609265,1 MMT 05/24/2010 Sales order Saving update lineNo incorrectly in ORDDSGN table[Start]
    IF llBomVarnt
      lcOldAlias = SELECT(0)
      IF SEEK(&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6),'ORDDSGN')
        SELECT ORDDSGN
        DELETE REST WHILE ORDER+CORDLINE+STR(LINENO,6) = &lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6)
      ENDIF
      SELECT (lcT_BomVar)
      =SEEK(&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6))
      SCAN REST WHILE ORDER+CORDLINE+STR(LINENO,6)  = &lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6) FOR !DELETED()
        SCATTER MEMO MEMVAR
        m.ORDER = lcOrderNo
        *B609402,1 SMA 09/05/2010 fix bug of SO- Orddgsn table standard fields not updating.....[BEGIN]
        m.cAdd_USer  = oAriaApplication.User_ID
        m.cAdd_Time = gfGettime()
        m.dAdd_Date = oAriaApplication.SystemDate
        *B609402,1 SMA 09/05/2010 fix bug of SO- Orddgsn table standard fields not updating.....[END]
        INSERT INTO ORDDSGN FROM MEMVAR
      ENDSCAN
      SELECT(lcOldAlias)
    ENDIF
    *! B609265,1 MMT 05/24/2010 Sales order Saving update lineNo incorrectly in ORDDSGN table[End]

  CASE DELETED() .AND. SEEK(cOrdType+ORDER+STR(LINENO,6),'OrdLine')
    IF llMFDsPrc .OR. llPODsPrc
      =lfUpdtPoCT(ORDLINE.ORDER,ORDLINE.STYLE,STR(ORDLINE.LINENO,6),0,0,ORDLINE.Price,ORDLINE.TotQty,.T.)
    ENDIF

    *! E302650,1 MMT 12/02/2009 Update Project Status When order or one of its lines is cancelled[Start]
    lfUpdPrjSt(ORDLINE.cOrdType,ORDLINE.ORDER,ORDLINE.LINENO)
    *! E302650,1 MMT 12/02/2009 Update Project Status When order or one of its lines is cancelled[End]


    *B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [Begin]
    SELECT ORDLINE
    CurRecOrdLine = ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)

    && Adjust Style And StyDye Record Pointers.
    = SEEK(ORDLINE.STYLE,'Style')
    = SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')

    FOR innerstr = 1 TO 8
      cstr = ALLTRIM(STR(innerstr))
      OldQTY&cstr.  = ORDLINE.Qty&cstr.
      OldBook&cstr. = ORDLINE.Book&cstr.
    ENDFOR

    lcFromOrder = ORDLINE.cFromOrder
    lBulkLineNo = ORDLINE.BulkLineNo
    lcDyelot    = ORDLINE.Dyelot
    lcSTYLE     = ORDLINE.STYLE
    lcWareCode  = ORDLINE.cWareCode
    IF !EMPTY(lcFromOrder) .AND. SEEK(cOrdType+lcFromOrder+STR(lBulkLineNo,6),'OrdLine') .AND. SEEK(cOrdType+lcFromOrder,'BlkOrdHd','OrdHdr')

      lnOpen      = 0
      lnOpenAmt   = 0
      lnCancel    = 0
      lnCancelAmt = 0
      *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
      IF llShowMessage
        IF  gfModalGen('QRM52026B32000','DIALOG',ordhdr.cfromorder) =1
          llCancelBulk = .T.
        ENDIF
        llShowMessage =.F.
      ENDIF
      SELECT (lcOrdLine)
      STORE 0 TO lnQtyB1,lnQtyB2,lnQtyB3,lnQtyB4,lnQtyB5,lnQtyB6,lnQtyB7,lnQtyB8
      *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[End]

      FOR innerLoopCounter = 1 TO 8
        cstr  = ALLTRIM(STR(innerLoopCounter))
        xDiff =  OldQTY&cstr.
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        IF !llCancelBulk
          *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]
          REPLACE Qty&cstr.  WITH (Qty&cstr. + xDiff) IN ORDLINE
          REPLACE TotQty     WITH (TotQty + xDiff) IN ORDLINE
          lnOpen    = lnOpen + xDiff
          lnOpenAmt = lnOpenAmt + (xDiff * ORDLINE.Price)
          *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        ELSE
          *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][Start]
          *lnQtyB&cstr. =  OldQTY&cstr.
          *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][End]
        ENDIF
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[End]
        xDiff = OldBook&cstr.
        REPLACE Book&cstr  WITH (Book&cstr. + xDiff) IN ORDLINE
        REPLACE TotBook    WITH (TotBook + xDiff)    IN ORDLINE
        lnCancel = lnCancel + xDiff
        lnCancelAmt = lnCancelAmt + (xDiff * ORDLINE.Price)
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        IF !llCancelBulk
          *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]
          =RLOCK('Style')
          REPLACE  Ord&cstr.   WITH Ord&cstr. + (ORDLINE.Qty&cstr. ) ,;
            TotOrd WITH TotOrd + (ORDLINE.Qty&cstr. ) IN STYLE
          UNLOCK IN 'Style'

          =RLOCK('StyDye')
          REPLACE Ord&cstr.  WITH Ord&cstr.   + (ORDLINE.Qty&cstr. ) ,;
            TotOrd WITH TotOrd + (ORDLINE.Qty&cstr. ) IN StyDye
          UNLOCK IN 'StyDye'
          *-- Update the Qty on the Configuration level
          IF !EMPTY(lcDyelot) AND SEEK(lcSTYLE+lcWareCode+lcDyelot,'StyDye')
            REPLACE Ord&cstr.   WITH Ord&cstr.   + (ORDLINE.Qty&cstr. ) ,;
              TotOrd WITH TotOrd + (ORDLINE.Qty&cstr. ) IN StyDye
            UNLOCK IN 'StyDye'
          ENDIF
          *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        ENDIF
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]
      ENDFOR

      && Modify the Bulk Order Header Totals And Adjust The Status Depend On The Totals values
      =RLOCK("BlkOrdHd")
      SELECT BlkOrdHd
      REPLACE Book      WITH Book    + lnCancel   ,;
        BookAmt   WITH BookAmt + lnCancelAmt,;
        OPEN      WITH OPEN    + lnOpen     ,;
        OpenAmt   WITH OpenAmt + lnOpenAmt  ,;
        APPRAMT   WITH (APPRAMT + lnOpenAmt) IN BlkOrdHd
      *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][Start]
      *!*	      *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
      *!*	      IF llCancelBulk
      *!*	        REPLACE CANCEL      WITH CANCEL   + lnCancel   ,;
      *!*	          CancelAmt   WITH CancelAmt+ lnCancelAmt
      *!*	        SELECT ORDCANLN
      *!*	        APPEND BLANK
      *!*	        REPLACE STYLE WITH Ordline.STYLE,;
      *!*	          Qty1  WITH lnQtyB1,;
      *!*	          Qty2  WITH lnQtyB2,;
      *!*	          Qty3  WITH lnQtyB3,;
      *!*	          Qty4  WITH lnQtyB4,;
      *!*	          Qty5  WITH lnQtyB5,;
      *!*	          Qty6  WITH lnQtyB6,;
      *!*	          Qty7  WITH lnQtyB7,;
      *!*	          Qty8  WITH lnQtyB8,;
      *!*	          TotQty  WITH lnQtyB1+lnQtyB2+lnQtyB3+lnQtyB4+lnQtyB5+lnQtyB6+lnQtyB7+lnQtyB8,;
      *!*	          cordtype WITH BlkOrdHd.cordtype,;
      *!*	          ORDER WITH BlkOrdHd.ORDER,;
      *!*	          Account WITH BlkOrdHd.Account,;
      *!*	          STORE  WITH Ordline.STORE,;
      *!*	          Price  WITH Ordline.Price,;
      *!*	          Dyelot WITH Ordline.Dyelot,;
      *!*	          LINENO WITH Ordline.LINENO
      *!*	        =gfAdd_Info('ORDCANLN')
      *!*	      ENDIF
      *!**! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]
      *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][End]
      UNLOCK IN BlkOrdHd

      IF BlkOrdHd.STATUS = 'X' .AND. (BlkOrdHd.OPEN > 0 ).AND. SEEK('M'+BlkOrdHd.Account,'Customer')
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        STORE 0 TO lnOpenBlk ,lnOpenBlkAmt ,lnBookBlk ,lnBookBlkAmt
        *
        lfRebalBulkOrder(BlkOrdHd.ORDER,Ordline.LINENO)
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[End]
        =RLOCK("BlkOrdHd")
        REPLACE STATUS    WITH "O" IN BlkOrdHd
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]

        REPLACE OPEN WITH lnOpenBlk ,;
          OpenAmt WITH  lnOpenBlkAmt ,;
          Book WITH lnBookBlk ,;
          BookAmt WITH lnBookBlkAmt  IN BlkOrdHd
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[End]
        UNLOCK IN BlkOrdHd

        =RLOCK('Customer')
        REPLACE nBulk WITH nBulk + 1 IN Customer
        UNLOCK IN 'Customer'
      ENDIF


    ENDIF
    SELECT ORDLINE
    =SEEK(CurRecOrdLine,"ORDLINE")


    *B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [End]

    SELECT ORDLINE
    DELETE

    *! B609265,1 MMT 05/24/2010 Sales order Saving update lineNo incorrectly in ORDDSGN table[Start]
    IF llBomVarnt AND SEEK(&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6),'ORDDSGN')
      lcOldAlias = SELECT(0)
      SELECT ORDDSGN
      DELETE REST WHILE ORDER+CORDLINE+STR(LINENO,6) = &lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6)
      SELECT(lcOldAlias)
    ENDIF
    *! B609265,1 MMT 05/24/2010 Sales order Saving update lineNo incorrectly in ORDDSGN table[End]
  ENDCASE
ENDSCAN
*-- if no change in lcOrdline.item_no get style.cvensty value.
IF ASCAN(oAriaApplication.laEvntTrig , PADR('UPDITMNO',10)) <> 0
  llOrdLn = .T.
  =gfDoTriger('SOORD',PADR('UPDITMNO',10))
ENDIF
*-- Replace Ordline Pack no with Appropriate Pack No. in Edit mode
IF ASCAN(oAriaApplication.laEvntTrig , PADR('COMPLDAT',10)) <> 0
  = SEEK(lcOrdType+laData[1])
  REPLACE REST cPack WITH lcPack WHILE cOrdType+ORDER=lcOrdType+laData[1]
ENDIF
*-- If Customer J & L replace Temp Ordline with cLevel , cLabel.
IF ASCAN(oAriaApplication.laEvntTrig , PADR('SOJLMER',10)) <> 0
  = gfDoTriger('SOORD',PADR('SOJLMER',10))
ENDIF
*-- Save Cutomer profile..
IF ASCAN(oAriaApplication.laEvntTrig , PADR('SAV_CUST',10)) <> 0
  =gfDoTriger('SOORD',PADR('SAV_CUST',10))
ENDIF
*-- Replace Temp OrdHdr with Appropriate Pack No. [Begin]
*-- Validation for Completion date in Packs file (Custom process for Cathy Daniels)
IF ASCAN(oAriaApplication.laEvntTrig , PADR('COMPLDAT',10)) <> 0
  PRIVATE lnUsrField
  lnUsrField = ASCAN(laUsrFields,"CPACK")
  IF lnUsrField > 0
    lnUsrField = ASUBSCRIPT(laUsrFields,lnUsrField,1)
    laUsrFields[lnUsrField,6] = lcPack
  ENDIF
ENDIF
*-- If customer is Cathy Daniel export Sales Order to Old System
IF ASCAN(oAriaApplication.laEvntTrig , PADR('EXPCTOLD',10)) <> 0
  =gfDoTriger('SOORD',PADR('EXPCTOLD',10))
ENDIF
*-- trigger to save
IF ASCAN(oAriaApplication.laEvntTrig , PADR('SAVLABEL',10)) <> 0
  =gfDoTriger('SOORD',PADR('SAVLABEL',10))
ENDIF
SET DELETE ON
IF llBomVarnt
  *-- If Customer J&L save data to BomVar & Custom thread color file.
  IF ASCAN(oAriaApplication.laEvntTrig, PADR('DOSCRN',10)) <> 0
    =gfDoTriger('SOORD',PADR('DOSCRN',10))
  ELSE
    *-- Update BOM Variant cost sheet file
    *E302303,1 WAM 09/01/2006 changes in design screen
    *!*	    SELECT BOMVAR
    *!*	    =SEEK('SO'+lcOrderNo)
    *!*	    DELETE REST WHILE cIdType+cCost_Id+STR(LineNo,6) = 'SO'+lcOrderNo
    *!*	    SELECT (lcT_BomVar)
    *!*	    *SCAN FOR !INLIST(cStatus,'S','D')
    *!*	    SCAN FOR !DELETED()
    *!*	      SCATTER MEMVAR
    *!*	      m.cCost_Id = lcOrderNo
    *!*	      INSERT INTO BOMVAR FROM MEMVAR
    *!*	    ENDSCAN

    *! B609265,1 MMT 05/24/2010 Sales order Saving update lineNo incorrectly in ORDDSGN table[Start]
    *!*	    SELECT ORDDSGN
    *!*	    =SEEK(lcOrderNo)
    *!*	    DELETE REST WHILE order+cordline+STR(lineno,6)= lcOrderNo
    *!*	    SELECT (lcT_BomVar)
    *!*	    lnLineNO = 0
    *!*	    SCAN FOR !DELETED()
    *!*	      lnLineNO = lnLineNO + 1
    *!*	      SCATTER MEMVAR
    *!*	      m.order = lcOrderNo
    *!*	      m.LineNo = lnLineNO
    *!*	      INSERT INTO ORDDSGN FROM MEMVAR
    *!*	    ENDSCAN
    *! B609265,1 MMT 05/24/2010 Sales order Saving update lineNo incorrectly in ORDDSGN table[End]

    *E302303,1 WAM 09/01/2006 (End)

  ENDIF
ENDIF

*-- Update orders Quantity & Amount in the Customer History file
IF &lcOrdHDr..cOrdType='O' .AND. &lcOrdHDr..STATUS <>'B'
  =gfOpenFile(oAriaApplication.DataDir+'arCusHst',oAriaApplication.DataDir+'Acthst','SH')
  IF SEEK(&lcOrdHDr..Account+lcGlYear,'arCusHst')
    lnOrdAmt = &lcOrdHDr..OpenAmt &lcExRSin &lcOrdHDr..nExRate &lcUntSin &lcOrdHDr..nCurrUnit - ;
      IIF(lcActiveMode="A" OR lcOrgStatus='B',0,;
      OrdHdr.OpenAmt &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit)
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - IIF(lcActiveMode="A" OR lcOrgStatus='B',0,OrdHdr.OPEN) + &lcOrdHDr..OPEN,;
      nOrdQty            WITH nOrdQty - IIF(lcActiveMode="A" OR lcOrgStatus='B',0,OrdHdr.OPEN) + &lcOrdHDr..OPEN,;
      nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
      nOrdAmt            WITH nOrdAmt + lnOrdAmt
    UNLOCK
  ENDIF
ENDIF

*! C201483,1 SAB 05/13/2012 New Customization to update site budget info on customer file[T20111206.0020][Start]
*-- Add Trigger to Update Customer Site Budget info
IF ASCAN(loFormSet.laEvntTrig , PADR('SBUPDTORD',10)) <> 0
  =loFormSet.mDoTrigger(PADR('SBUPDTORD',10))
ENDIF
*! C201483,1 SAB 05/13/2012 New Customization to update site budget info on customer file[T20111206.0020][End]

*-- Update bulk order total amount and quantity
IF &lcOrdHDr..cOrdType='O' .AND. &lcOrdHDr..STATUS <>'B' .AND. (lcActiveMode = "A"  .OR. lcOrgStatus='B') .AND. ;
    !EMPTY(&lcOrdHDr..cFromOrder) AND SEEK(&lcOrdHDr..cOrdType+&lcOrdHDr..cFromOrder,'BlkOrdHd')

  *-- Decrease bulk orders depleted Quantity & Amount in the Customer History file
  IF SEEK(&lcOrdHDr..Account+lcBulkYear,'arCusHst')

    lnOrdAmt = lnCancelAmt &lcExRSin BlkOrdHd.nExRate &lcUntSin BlkOrdHd.nCurrUnit
    =RLOCK("arCusHst")
    REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCancel ,;
      nOrdQty           WITH nOrdQty           - lnCancel ,;
      nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOrdAmt ,;
      nOrdAmt           WITH nOrdAmt           - lnOrdAmt IN arCusHst
    UNLOCK IN arCusHst
  ENDIF
  =RLOCK("BlkOrdHd")
  REPLACE Book      WITH Book    - lnCancel   ,;
    BookAmt   WITH BookAmt - lnCancelAmt,;
    OPEN      WITH Book    - CANCEL     ,;
    OpenAmt   WITH BookAmt - Cancelamt  ,;
    APPRAMT   WITH MAX(0,APPRAMT - &lcOrdHDr..OpenAmt) ,;
    STATUS    WITH IIF(OPEN = 0 ,'X',STATUS) IN BlkOrdHd
  UNLOCK IN BlkOrdHd
  IF BlkOrdHd.STATUS='X' .AND. SEEK('M'+BlkOrdHd.Account,'Customer')
    =RLOCK('Customer')
    REPLACE nBulk WITH nBulk - 1 IN Customer
    UNLOCK IN 'Customer'
  ENDIF
ENDIF
*-- Increase number of bulk orders for this customer
IF &lcOrdHDr..cOrdType='O' AND &lcOrdHDr..STATUS <>'B' AND (lcActiveMode="A" OR lcOrgStatus='B') AND ;
    &lcOrdHDr..Bulk='Y' AND SEEK('M'+&lcOrdHDr..Account,'Customer')
  =RLOCK('Customer')
  REPLACE nBulk WITH nBulk + 1 IN Customer
  UNLOCK IN Customer
ENDIF

WAIT CLEAR
IF !llFromEDI AND &lcOrdHDr..cOrdType='O' AND 'EB' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT'  ,'SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD'   ,oAriaApplication.DataDir+'PARTTRANS','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY'  ,'SH')

  *-- Add new 855 record in the EDI transaction file to be sent
  IF SEEK('A'+&lcOrdHDr..Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'855','EDIPD')
    *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
    *IF !SEEK('855'+PADR(lcOrderNo,20)+'A'+OrdHdr.Account,'EDITRANS')
    IF !SEEK('855'+PADR(lcOrderNo,40)+'A'+&lcOrdHDr..Account,'EDITRANS')
      *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [End]
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,KEY,TYPE,cPartner) VALUES ;
        ('855',lcOrderNo,'A',&lcOrdHDr..Account)
    ENDIF
    REPLACE cStatus WITH 'N' IN EDITRANS
    =gfAdd_Info('EDITRANS')
  ENDIF
  *-- Add new 850 record in the EDI transaction file to be sent to the customer
  IF SEEK('A'+&lcOrdHDr..Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'850','EDIPD') AND EDIPD.cTranType = "S"
    *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
    *IF !SEEK('850'+PADR(lcOrderNo,20)+'A'+OrdHdr.Account,'EDITRANS')
    IF !SEEK('850'+PADR(lcOrderNo,40)+'A'+&lcOrdHDr..Account,'EDITRANS')
      *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [End]
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,KEY,TYPE,cPartner) VALUES ;
        ('850',lcOrderNo,'A',&lcOrdHDr..Account)
    ENDIF
    REPLACE cStatus WITH 'N' IN EDITRANS
    =gfAdd_Info('EDITRANS')
  ENDIF
  *-- Add new 850 record in the EDI transaction file to be sent to the factor
  IF SEEK('F'+&lcOrdHDr..cFacCode,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'850','EDIPD') AND EDIPD.cTranType = "S"
    *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
    *IF !SEEK('850'+PADR(lcOrderNo,20)+'F'+OrdHdr.cFacCode,'EDITRANS')
    IF !SEEK('850'+PADR(lcOrderNo,40)+'F'+&lcOrdHDr..cFacCode,'EDITRANS')
      *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,KEY,TYPE,cPartner) VALUES ;
        ('850',lcOrderNo,'F',&lcOrdHDr..cFacCode)
    ENDIF
    REPLACE cStatus WITH 'N' IN EDITRANS
    =gfAdd_Info('EDITRANS')
  ENDIF
  *N000612,1 WLD 06/03/2008 Adding New Transmission 865 (Supplier PO Change) in EDITrans [Begin]
  *-- Add new 865 record in the EDI transaction file to be sent
  IF SEEK('A'+&lcOrdHDr..Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'865','EDIPD')
    IF !SEEK('865'+PADR(&lcOrdHDr..ORDER,40)+'A'+&lcOrdHDr..Account,'EDITRANS')
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,KEY,TYPE,cPartner) VALUES ;
        ('865',&lcOrdHDr..ORDER,'A',&lcOrdHDr..Account)
    ENDIF
    REPLACE cStatus WITH 'N' IN EDITRANS
    =gfAdd_Info('EDITRANS')
  ENDIF
  *N000612,1 WLD 06/03/2008 Adding New Transmission 865 (Supplier PO Change) in EDITrans [End]
ENDIF

*-- Update the CRMesag file
IF &lcOrdHDr..lFromWeb AND 'CR' $ oAriaApplication.CompanyInstalledModules AND ;
    &lcOrdHDr..STATUS $ 'HO' AND lcOrgStatus = 'B'

  *-- Get the Memory Variables without checking for the CRM module.
  =gfOpenFile(oAriaApplication.DataDir+'CRMesag',oAriaApplication.DataDir+'TransType','SH')
  SELECT CRMesag
  APPEND BLANK
  REPLACE cTransType WITH 'O' ,;
    cTransNo   WITH lcOrderNo ,;
    lApprove   WITH .T. , ;
    cMailFrom  WITH gfGetMemVar('M_NOTEMAIL') ,;
    dTransDate WITH oAriaApplication.SystemDate ,;
    cMailTo    WITH IIF(SEEK('M'+&lcOrdHDr..Account,'Customer'),EVALUATE('Customer.' + gfGetMemVar('M_CONFMAIL')),'') , ;
    cMsgSubjct WITH "Your Sales Order# " + lcOrderNo + ' has been approved.' , ;
    cLetterId  WITH gfGetMemVar('M_SOAPR') ,;
    lSent      WITH .F.

  *!*	  *E302811,1 TMI 12/12/2010 [Start] create a new sequence for Web orders
  *!*	  llCRMNewSoID = gfGetMemVar('M_CRMNEWID')
  *!*	  IF llCRMNewSoID
  *!*	    lcOrderNo = gfSequence('ORDER','','',&lcOrdHdr..cDivision)
  *!*	    SELECT &LCORDLINE
  *!*	    SCAN
  *!*	      =SEEK('O'+&LCORDLINE..Order+STR(&LCORDLINE..LINENO,6),'ORDLINE')
  *!*	      SELECT ORDLINE
  *!*	      REPLACE ORDER WITH lcOrderNo
  *!*	    ENDSCAN
  *!*	  ENDIF
  *!*	  *E302811,1 TMI 12/12/2010 [End  ] create a new sequence for Web orders

ENDIF

*E302811,3 TMI 12/12/2010 [Start] move the code here for Tony with no check on the CRM modle is isntalled
IF &lcOrdHDr..lFromWeb AND ;
    &lcOrdHDr..STATUS $ 'HO' AND lcOrgStatus = 'B'

  llCRMNewSoID = gfGetMemVar('M_CRMNEWID')
  llCRMNewSoID = IIF(EMPTY(llCRMNewSoID),.F.,llCRMNewSoID)
  IF llCRMNewSoID
    lcOrderNo = gfSequence('ORDER','','',&lcOrdHDr..cDivision)
    SELECT &lcOrdLine
    SCAN
      =SEEK('O'+&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6),'ORDLINE')
      SELECT ORDLINE
      REPLACE ORDER WITH lcOrderNo
      *B609984,1 MMT 07/03/2012 Coverting EDI Temp. Order to open order does not update orddsgn table[T20120425.0001][Start]
      IF llBomVarnt
        SELECT (lcT_BomVar)
        =SEEK(&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6))
        SCAN REST WHILE ORDER+CORDLINE+STR(LINENO,6) = &lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6)
          SELECT OrdDsgn
          IF SEEK(&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6)+STR(&lcT_BomVar..LINENO,6))
            REPLACE ORDER WITH lcOrderNo
          ENDIF
        ENDSCAN
      ENDIF
      *B609984,1 MMT 07/03/2012 Coverting EDI Temp. Order to open order does not update orddsgn table[T20120425.0001][END]
    ENDSCAN
  ENDIF
ENDIF
*E302811,3 TMI 12/12/2010 [End  ]


SELECT (lcOrdHDr)
*HBG 12/22/2004 Fix bug of changing the order type in case of EDI sales order [begin]
*!*	REPLACE Order    WITH lcOrderNo ,;
*!*	        cOrdType WITH IIF((OrdHdr.lEdiOrder AND llFromEDI) OR (OrdHdr.lFromWeb AND OrdHdr.Status <> 'B'),'O',cOrdType) ,;
*!*	        Status   WITH IIF(Open=0,IIF(Ship>0,'C','X'),Status) ,;
*!*	        LastLine WITH lnLineCount
*B126848,1 HBG 03/20/2005 Fix bug of Updating the web orders always even if the status not changed [begin]
*!*	REPLACE Order    WITH lcOrderNo ,;
*!*	        cOrdType WITH IIF(llFromEDI OR (OrdHdr.lFromWeb AND OrdHdr.Status <> 'B'),'O',cOrdType) ,;
*!*	        Status   WITH IIF(Open=0,IIF(Ship>0,'C','X'),Status) ,;
*!*	        LastLine WITH lnLineCount
REPLACE ORDER    WITH lcOrderNo ,;
  cOrdType WITH IIF(llFromEDI OR (&lcOrdHDr..lFromWeb AND &lcOrdHDr..STATUS $ 'HO' AND loFormSet.lcOldStatus = 'B'),'O',cOrdType) ,;
  STATUS   WITH IIF(OPEN=0,IIF(Ship>0,'C','X'),STATUS) ,;
  LastLine WITH lnLineCount
*B608262,1 WAM 09/09/2007 Store header information in a temporary file instead of buffer mode

*B610001,1 HIA 07/12/2012 Approved ampunt not related to the open amount [T20120618.0004][Begin]
REPLACE ApprAmt WITH MIN(ApprAmt,OpenAmt)
*B610001,1 HIA 07/12/2012 Approved ampunt not related to the open amount [T20120618.0004][End]


SCATTER MEMVAR MEMO
IF lcActiveMode = 'A'
  INSERT INTO OrdHdr FROM MEMVAR
ELSE
  SELECT OrdHdr
  *B611789 ,1 ES 06/19/2019 Auto Allocate crashing with duplicate SO order header entry [Start]
  =SEEK(m.cOrdType+m.Order,'OrdHdr','OrdHdr')
  *B611789 ,1 ES 06/19/2019 Auto Allocate crashing with duplicate SO order header entry [End]
  GATHER MEMVAR
ENDIF
*B608262,1 WAM 09/09/2007 (End)

*B126848,1 [End]
*HBG [End]

*! B609408,1 MMT 09/14/2010 Update Dend in Styhist while Saving the Sales order[Start]
SELECT 'STYHIST_OS'
=gfTableUpdate()
*! B609408,1 MMT 09/14/2010 Update Dend in Styhist while Saving the Sales order[End]

*! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
SELECT icstyhst
=gfTableUpdate()
*! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]

*! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[Start]
SELECT STYHIST
=gfTableUpdate()
*! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[End]

*E302350,1 MMT 01/11/2007 Enhancement to add Deposit Screen[Start]
=lfUpdateCredit()
*E302350,1 MMT 01/11/2007 Enhancement to add Deposit Screen[End]

*-- Display generated order number
IF lcActiveMode = "A" AND !llFromEDI
  *-- Message : 32045
  *-- Order has been saved as xxxxxx
  *-- Button : 00000
  *-- Ok

  *!--C201131 - MOS - SAVING SPREADSHEET 04/23/2009 [START]
  IF ASCAN(loFormSet.laEvntTrig , PADR('SOIMPRO',10)) <> 0
    =lfCustMsg()
  ELSE
    *!--C201131 - MOS - SAVING SPREADSHEET 04/23/2009 [END  ]

    *- C201136 - MOS - single entry sales order T20080815.0001  [START]
    IF ASCAN(loFormSet.laEvntTrig , PADR('SPSHSAV',10)) <> 0 AND TYPE('llselMode')= 'L' AND llselMode = .T.
      =loFormSet.mDoTrigger(PADR('SPSHSAV',10))  && remove this
    ELSE
      *- C201136 - MOS - single entry sales order T20080815.0001  [END  ]

      =gfModalGen('INM32045B00000','DIALOG',IIF(OrdHdr.cOrdType='C','Contract','Order')+'|'+lcOrderNo)

      *!--C201131 - MOS - SAVING SPREADSHEET 04/23/2009 [START]
    ENDIF
  ENDIF
  *!--C201131 - MOS - SAVING SPREADSHEET 04/23/2009 [END  ]
ENDIF
*B128066,1 HBG Fix bug of file is used by another user [Start]
SET REPROCESS TO lnRePro
*B128066,1 HBG [End]

RETURN

*!*************************************************************
*! Name      : lfDelScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/08/2002
*! Purpose   : Cancel/Uncancel order
*!*************************************************************
*! Calls     : gfModalGen,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDelScr()
*!*************************************************************
*!Modifications
*!B607917,1 WAM 01/02/2007 Enhance performance of cancelation SO when allocated to POs
*!N000612,1 WLD 06/03/2008 Adding New Transmission 865 (Supplier PO Change)in EDITrans in case of Cancel SO
*!B609894,1 HIA 22/04/2011 When cancelling an order has pick ticket, error 'Status' variable not found [T20120327.0036]
*!*************************************************************
FUNCTION lfDelScr

*B608980,1 TMI [START] Add a new parameter loFormSet
*PARAMETERS llFromEDI
PARAMETERS llFromEDI,loFormSet
*B608980,1 TMI [END  ] Add a new parameter loFormSet

*B128066,1 HBG Fix bug of file is used by another user [Start]
LOCAL lnRePro
lnRePro = SET("Reprocess")
SET REPROCESS TO -1
*B128066,1 HBG [End]

*-- Check if customer is SADIMARA.
IF ASCAN(oAriaApplication.laEvntTrig , PADR('ORDCHECK',10)) <> 0
  IF !gfDoTriger('SOORD',PADR('ORDCHECK',10))
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]

    RETURN
  ENDIF
ENDIF
llFromEDI = IIF(TYPE('llFromEDI') = 'L' , llFromEDI , .F.)

*B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [Begin]
=gfOpenFile(oAriaApplication.DataDir+'OrdHdr',oAriaApplication.DataDir+'ORDHDR','SH','BlkOrdHd',.T.)
=SEEK(OrdHdr.cOrdType+OrdHdr.cFromOrder,'BlkOrdHd')
=gfOpenFile(oAriaApplication.DataDir+'OrdLine',oAriaApplication.DataDir+'OrdLine','SH','BlkOrdLine',.T.)
*B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [End]

*N000612,1 WLD 06/03/2008 Adding New Transmission 865 (Supplier PO Change) in EDITrans [Begin]
IF !llFromEDI AND OrdHdr.cOrdType='O' AND 'EB' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT'  ,'SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD'   ,oAriaApplication.DataDir+'PARTTRANS','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY'  ,'SH')

  *-- Add new 865 record in the EDI transaction file to be sent
  IF SEEK('A'+OrdHdr.Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'865','EDIPD')
    IF !SEEK('865'+PADR(OrdHdr.ORDER,40)+'A'+OrdHdr.Account,'EDITRANS')
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,KEY,TYPE,cPartner) VALUES ;
        ('865',OrdHdr.ORDER,'A',OrdHdr.Account)
    ENDIF
    REPLACE cStatus WITH 'N' IN EDITRANS
    =gfAdd_Info('EDITRANS')
  ENDIF
ENDIF
*N000612,1 WLD 06/03/2008 Adding New Transmission 865 (Supplier PO Change) in EDITrans [End]

*! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[Start]
IF OrdHdr.cOrdType='O'
  *Styhist File update
  STORE 0 TO lnMajorLen , lnClrPos, lnClrLen
  lfGetClrD()
  lnMajorLen = LEN(gfItemMask("PM"))

  IF !USED('STYHIST')
    =gfOpenTable('STYHIST','STYHIST','SH')
  ENDIF
  IF !USED('UNIFORM')
    =gfOpenTable('UNIFORM','UNIFORM','SH')
  ENDIF
  IF !USED('Contact_A')
    =gfOpenTable('Contact','Contact','SH','Contact_A')
  ENDIF
ENDIF
*! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[End]


*-- Get Fiscal Year and period of order entered date
STORE '' TO lcGlYear,lcGlPeriod
*B124641,1 HBG 10/18/2004 Comment this block of code because there is no need to check for the period with Sales orders [Begin]
*!*	IF OrdHdr.cOrdType='O' .AND. !CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod','',.T.)
*!*	  RETURN
*!*	ENDIF
*! E303218,1 HIA 08/22/2012 As per MMT, re-open the above code to get the period, Convert ICSTYHST to SQL [T20120813.0035][BEGIN]
IF OrdHdr.cOrdType='O' .AND. !CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod','',.T.)
  RETURN
ENDIF
*! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][END]
*B124641,1 [End]
*-- Cancel/Uncancel web order, Check the CRM setup
IF OrdHdr.lFromWeb  AND 'CR' $ oAriaApplication.CompanyInstalledModules
  lcConfMail = gfGetMemVar('M_CONFMAIL',oAriaApplication.ActiveCompanyID)
  lcSOAPR    = gfGetMemVar('M_SOAPR',oAriaApplication.ActiveCompanyID)
  lcSOCAN    = gfGetMemVar('M_SOCAN',oAriaApplication.ActiveCompanyID)

  IF EMPTY(lcConfMail)
    *-- Message : 32090
    *-- Text    : The Confirmation E-mail Address on the setup screen is blank.
    *--           We can not save your order.
    *-- Button  : 00000
    *--           <Ok>
    =gfModalGen('TRM32090B00000','DIALOG')
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]

    RETURN
  ENDIF
  * Uncancel Web Order
  IF OrdHdr.STATUS = 'X'
    * If the User approved(Open/Hold) the Web Order
    IF EMPTY(lcSOAPR)
      *-- Message : 32091
      *-- Text    : The Approved ID on the setup screen is blank. Are you sure you want to continue?
      *-- Button  : 32000
      *--           <Yes> <No>
      IF gfModalGen('QRM32091B32000','DIALOG') =2
        *B128066,1 HBG Fix bug of file is used by another user [Start]
        SET REPROCESS TO lnRePro
        *B128066,1 HBG [End]
        RETURN
      ENDIF
    ENDIF
  ELSE
    *-- Check Order Cancelation Id
    IF EMPTY(lcSOCAN)
      *-- Message : 32092
      *-- Text    : The Cancelled ID on the setup screen is blank. Are you sure you want to continue?
      *-- Button  : 32000
      *--          <Yes> <No>
      IF gfModalGen('QRM32092B32000','DIALOG') = 2
        *B128066,1 HBG Fix bug of file is used by another user [Start]
        SET REPROCESS TO lnRePro
        *B128066,1 HBG [End]

        RETURN
      ENDIF
    ENDIF
  ENDIF
ENDIF
lcUntSin = '/'
lcExRSin = gfGetExSin(@lcUntSin, OrdHdr.cCurrCode)
*-- Open the order cancellation table if we have shipped qty.
=gfOpenFile(oAriaApplication.DataDir+'ORDCANLN',oAriaApplication.DataDir+'ORDCANLN','SH')
*-- Uncancel order
IF OrdHdr.STATUS='X'
  IF OrdHdr.cOrdType='O' .AND. !SEEK(OrdHdr.cOrdType+OrdHdr.ORDER,'ORDLINE')
    *-- Message : 32000
    *-- The lines for this order are missing! Cannot update cut & sold.
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32000B00000','ALERT')
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]

    RETURN
  ENDIF

  *-- a custome trigger to check Uncancele or Delete the canceled order
  IF ASCAN(oAriaApplication.laEvntTrig,PADR('DELORD',10)) <> 0
    IF !gfDoTriger('SOORD',PADR('DELORD',10))
      *B128066,1 HBG Fix bug of file is used by another user [Start]
      SET REPROCESS TO lnRePro
      *B128066,1 HBG [End]

      RETURN
    ENDIF
  ELSE
    *-- Message : 32002
    *-- Order is canceled. Uncancel?
    *-- Button : 32000
    *-- Yes/No
    IF gfModalGen('QRM32002B32000','ALERT',IIF(OrdHdr.cOrdType='C','Contract','Order')) =2
      *B128066,1 HBG Fix bug of file is used by another user [Start]
      SET REPROCESS TO lnRePro
      *B128066,1 HBG [End]

      RETURN
    ENDIF
  ENDIF
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET ORDER TO TAG OrdHdr  IN OrdHdr

  IF OrdHdr.CANCEL = 0 AND OrdHdr.Bulk <> 'Y'
    SELECT OrdHdr
    REPLACE STATUS     WITH 'B' ,;
      cCancReson WITH SPACE(6) ,;
      Cancelled  WITH {} ,;
      FLAG       WITH SPACE(1)
    *-- Update the CRMesag file.
    DO lpUpdMesag
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]

    RETURN
  ENDIF
  *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
  lcICStyHstTable = ''
  *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
  STORE 0 TO lnNewOpen,lnNewOAmt
  IF OrdHdr.cOrdType='O'
    WAIT 'Uncancelling and updating Cut & Sold...' WINDOW NOWAIT
    *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
    *=gfOpenFile(oAriaApplication.DataDir+'icStyHst',oAriaApplication.DataDir+'Styhst','SH')
    =gfOpenTable('ICSTYHST','STYHST','SH')
    *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
    *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
    lnTableICStyHst = gfGetRemoteTable(SET("Datasession"),'ICSTYHST')
    IF lnTableICStyHst<>0 && Remote Table Object was Found
      lcICStyHstTable = oAriaApplication.laRemoteTable[lnTableICStyHst].lcCursorUpdate
    ENDIF
    *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
  ENDIF
  SELECT ORDLINE
  =SEEK(OrdHdr.cOrdType+OrdHdr.ORDER)
  SCAN REST WHILE cOrdType+ORDER+STR(LINENO,6) = OrdHdr.cOrdType+OrdHdr.ORDER
    IF OrdHdr.cOrdType='O'
      *! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[Start]
      IF !EMPTY(ORDLINE.Employee)
        lfUpStyHist(.T.)
      ENDIF
      *! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[End]
      *-- Increase warehouse ordered quantity
      IF SEEK(STYLE+cWareCode+SPACE(10),'StyDye')
        =RLOCK("StyDye")
        REPLACE Ord1   WITH Ord1 + ORDLINE.Qty1 ,;
          Ord2   WITH Ord2 + ORDLINE.Qty2 ,;
          Ord3   WITH Ord3 + ORDLINE.Qty3 ,;
          Ord4   WITH Ord4 + ORDLINE.Qty4 ,;
          Ord5   WITH Ord5 + ORDLINE.Qty5 ,;
          Ord6   WITH Ord6 + ORDLINE.Qty6 ,;
          Ord7   WITH Ord7 + ORDLINE.Qty7 ,;
          Ord8   WITH Ord8 + ORDLINE.Qty8 ,;
          TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
        *N000300,1 HBG 5/10/2004 Update the Qty on the Configuration level [Begin]
        IF !EMPTY(Dyelot) AND SEEK(STYLE+cWareCode+Dyelot,'StyDye')
          REPLACE Ord1   WITH Ord1 + ORDLINE.Qty1 ,;
            Ord2   WITH Ord2 + ORDLINE.Qty2 ,;
            Ord3   WITH Ord3 + ORDLINE.Qty3 ,;
            Ord4   WITH Ord4 + ORDLINE.Qty4 ,;
            Ord5   WITH Ord5 + ORDLINE.Qty5 ,;
            Ord6   WITH Ord6 + ORDLINE.Qty6 ,;
            Ord7   WITH Ord7 + ORDLINE.Qty7 ,;
            Ord8   WITH Ord8 + ORDLINE.Qty8 ,;
            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
        ENDIF
        *N000300,1 [End]
        UNLOCK IN StyDye
      ENDIF

      *-- Increase style ordered quantity
      IF SEEK(STYLE,'Style')
        =RLOCK("STYLE")
        REPLACE Ord1   WITH Ord1 + ORDLINE.Qty1 ,;
          Ord2   WITH Ord2 + ORDLINE.Qty2 ,;
          Ord3   WITH Ord3 + ORDLINE.Qty3 ,;
          Ord4   WITH Ord4 + ORDLINE.Qty4 ,;
          Ord5   WITH Ord5 + ORDLINE.Qty5 ,;
          Ord6   WITH Ord6 + ORDLINE.Qty6 ,;
          Ord7   WITH Ord7 + ORDLINE.Qty7 ,;
          Ord8   WITH Ord8 + ORDLINE.Qty8 ,;
          TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN STYLE
        UNLOCK IN STYLE
      ENDIF
      *-- Increase ordered quantity and amount in the style history file
      *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
      *IF SEEK(STYLE+lcGlYear,'icStyHst')
      old_alias = ALIAS()
      lcstyle_var = STYLE
      SELECT icStyhst
      *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][Begin]
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
      *IF !gfSEEK(lcStyle_Var+ lcGlYear)
      IF  !SEEK(lcStyle_Var+ lcGlYear,lcICStyHstTable) AND !gfSEEK(lcStyle_Var+ lcGlYear,'icStyhst')
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
        =gfOpenTable('ICSTYHST','STYHST','SH','ICSTYHSX')
        SELECT ICSTYHSX

        APPEND BLANK
        REPLACE STYLE WITH lcstyle_var
        REPLACE CFISFYEAR  WITH lcGlYear
        = GFREPLACE('')
        =gfTABLEUPDATE()

        *USE IN ICSTYHSX
        =gfcloseTable('ICSTYHSX')
        SELECT ICSTYHST
      ENDIF
      *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][End]
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
      *IF gfSEEK(lcstyle_var+lcGlYear)
      IF SEEK(lcstyle_var+lcGlYear,lcICStyHstTable) OR gfSEEK(lcstyle_var+lcGlYear,'ICSTYHST')
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
        SELECT (old_alias)
        *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
        lnOrdAmt = TotQty*Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
        IF !SEEK(lcstyle_var+lcGlYear,lcICStyHstTable)
          *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
          =RLOCK("icStyHst")
          REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + ORDLINE.TotQty ,;
            nOrdQty            WITH nOrdQty            + ORDLINE.TotQty ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
            nOrdAmt            WITH nOrdAmt            + lnOrdAmt IN icStyHst
          UNLOCK IN icStyHst
          *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
          SELECT icstyhst
          =GfReplace('')
          *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
          *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
        ELSE
          REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + ORDLINE.TotQty ,;
            nOrdQty            WITH nOrdQty            + ORDLINE.TotQty ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
            nOrdAmt            WITH nOrdAmt            + lnOrdAmt IN (lcICStyHstTable)
        ENDIF
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
      ENDIF
      *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
      SELECT (Old_Alias)
      *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
    ENDIF
    lnNewOpen = lnNewOpen+ TotQty
    lnNewOAmt = lnNewOAmt+ TotQty*Price

    *B609698,1 HIA 10/04/2011 When Uncancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [Begin]
    SELECT ORDLINE
    *CurRecOrdLine = ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
    && Adjust Style And StyDye Record Pointers.
    = SEEK(ORDLINE.STYLE,'Style')
    = SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')

    FOR innerstr = 1 TO 8
      cstr = ALLTRIM(STR(innerstr))
      OldQTY&cstr.  = ORDLINE.Qty&cstr.
      OldBook&cstr. = ORDLINE.Book&cstr.
    ENDFOR

    lcFromOrder = ORDLINE.cFromOrder
    lBulkLineNo = ORDLINE.BulkLineNo
    lcOrdType   = ORDLINE.cOrdType
    lcDyelot    = ORDLINE.Dyelot
    lcSTYLE     = ORDLINE.STYLE
    lcWareCode  = ORDLINE.cWareCode

    IF !EMPTY(lcFromOrder) .AND. SEEK(lcOrdType+lcFromOrder+STR(lBulkLineNo,6),'BlkOrdLine') .AND. SEEK(lcOrdType+lcFromOrder,'BlkOrdHd','OrdHdr')
      lnOpen      = 0
      lnOpenAmt   = 0
      lnCancel    = 0
      lnCancelAmt = 0

      FOR innerLoopCounter = 1 TO 8
        cstr = ALLTRIM(STR(innerLoopCounter))
        xDiff = OldQTY&cstr.
        REPLACE Qty&cstr.  WITH MAX((Qty&cstr. - xDiff),0) IN BlkOrdLine
        REPLACE TotQty     WITH MAX((TotQty - xDiff),0)    IN BlkOrdLine
        lnOpen    = lnOpen + xDiff
        lnOpenAmt = lnOpenAmt + (xDiff * ORDLINE.Price)

        xDiff = OldBook&cstr.
        REPLACE Book&cstr.  WITH MAX((Book&cstr. - xDiff),0) IN BlkOrdLine
        REPLACE TotBook     WITH MAX((TotBook - xDiff),0)    IN BlkOrdLine
        lnCancel = lnCancel + xDiff
        lnCancelAmt = lnCancelAmt + (xDiff * ORDLINE.Price)

        =RLOCK('Style')
        REPLACE  Ord&cstr.   WITH Ord&cstr. - xDiff ,;
          TotOrd      WITH TotOrd    - xDiff IN STYLE
        UNLOCK IN 'Style'

        =RLOCK('StyDye')
        REPLACE Ord&cstr.  WITH Ord&cstr.  - xDiff  ,;
          TotOrd     WITH TotOrd     - xDiff  IN StyDye
        UNLOCK IN 'StyDye'
        *-- Update the Qty on the Configuration level
        IF !EMPTY(lcDyelot) AND SEEK(lcSTYLE+lcWareCode+lcDyelot,'StyDye')
          REPLACE Ord&cstr.   WITH Ord&cstr. - xDiff ,;
            TotOrd      WITH TotOrd    - xDiff IN StyDye
          UNLOCK IN 'StyDye'
        ENDIF



      ENDFOR

      && Modify the Bulk Order Header Totals And Adjust The Status Depend On The Totals values
      =RLOCK("BlkOrdHd")
      SELECT BlkOrdHd
      REPLACE Book      WITH MAX(Book    - lnCancel,0)   ,;
        BookAmt   WITH MAX(BookAmt - lnCancelAmt,0),;
        OPEN      WITH MAX(OPEN    - lnOpen,0)     ,;
        OpenAmt   WITH MAX(OpenAmt - lnOpenAmt,0)  ,;
        STATUS    WITH IIF(OPEN = 0 ,'X',STATUS) ,;
        APPRAMT   WITH MAX(0,APPRAMT - lnOpenAmt) IN BlkOrdHd

      UNLOCK IN BlkOrdHd
      IF BlkOrdHd.STATUS='X' .AND. SEEK('M'+BlkOrdHd.Account,'Customer')
        =RLOCK('Customer')
        REPLACE nBulk WITH nBulk - 1 IN Customer
        UNLOCK IN 'Customer'
      ENDIF

    ENDIF
    SELECT ORDLINE
    *=SEEK(CurRecOrdLine,"ORDLINE")
    *B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [End]


  ENDSCAN
  IF OrdHdr.cOrdType='O'
    *-- Update ordered quantity and amount in the customer history file
    =gfOpenFile(oAriaApplication.DataDir+'arCusHst',oAriaApplication.DataDir+'Acthst','SH')
    =SEEK(OrdHdr.Account+lcGlYear,'arCusHst')
    lnOrdAmt = lnNewOAmt &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
    =RLOCK("arCusHst")
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + lnNewOpen ,;
      nOrdQty            WITH nOrdQty            + lnNewOpen ,;
      nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt  ,;
      nOrdAmt            WITH nOrdAmt            + lnOrdAmt IN arCusHst
    UNLOCK IN arCusHst
  ENDIF
  *-- Increase number of bulk orders for this account
  IF OrdHdr.Bulk ='Y' .AND. SEEK('M'+OrdHdr.Account,'Customer')
    =RLOCK("Customer")
    REPLACE nBulk WITH nBulk + 1 IN Customer
    UNLOCK IN Customer
  ENDIF
  *-- Update order header cancel and open quantity and amount
  SELECT OrdHdr
  REPLACE STATUS     WITH 'O'      ,;
    cCancReson WITH SPACE(6) ,;
    Cancelled  WITH {} ,;
    CANCEL     WITH IIF(Bulk='Y',CANCEL,CANCEL-lnNewOpen) ,;
    Cancelamt  WITH IIF(Bulk='Y',Cancelamt,Cancelamt-lnNewOAmt) ,;
    OPEN       WITH lnNewOpen ,;
    OpenAmt    WITH lnNewOAmt ,;
    FLAG       WITH SPACE(1)

  *-- Cheak if this order is bulk decrease the book Qty & amount.
  IF OrdHdr.Bulk ='Y'
    REPLACE Book    WITH Ship    + CANCEL   + lnNewOpen ,;
      BookAmt WITH ShipAmt + Cancelamt+ lnNewOAmt
  ENDIF
ELSE
  *-- Cancel order
  IF ASCAN(oAriaApplication.laEvntTrig , PADR('DLEALOSO',10)) <> 0
    IF !gfDoTriger('SOORD',PADR('DLEALOSO',10))
      *B128066,1 HBG Fix bug of file is used by another user [Start]
      SET REPROCESS TO lnRePro
      *B128066,1 HBG [End]

      RETURN
    ENDIF
  ENDIF
  *-- Do not cancel shipped complete order
  IF OrdHdr.STATUS='C'
    *-- Message : 32003
    *-- Order has been shipped complete! Cannot cancel.
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32003B00000','ALERT')
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]

    RETURN
  ENDIF
  *-- Do not cancel order with no lines
  IF !(OrdHdr.cOrdType='T' AND OrdHdr.Mon_Flg='L') AND !SEEK(OrdHdr.cOrdType+OrdHdr.ORDER,'ORDLINE')
    *-- Message : 32000
    *-- The lines for this order are missing! Cannot update cut & sold.
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32000B00000','ALERT')
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]

    RETURN
  ENDIF
  *-- Check if allocation module is installed.
  IF 'AL' $ oAriaApplication.CompanyInstalledModules
    =gfOpenFile(oAriaApplication.DataDir+'PIKTKT',oAriaApplication.DataDir+'ORDPIK','SH')
    *-- Do not cancel picked orders
    IF SEEK(OrdHdr.ORDER,'PikTkt','ORDPIK')
      *-- check Piktkt status for picked lines mesg.
      SELECT PikTkt
      LOCATE REST WHILE ORDER = OrdHdr.ORDER FOR STATUS $ 'OHP'
      IF FOUND()
        *-- Start, not allow to proceed,
        IF ASCAN(oAriaApplication.laEvntTrig , PADR('TRMTE_CANL',10)) <> 0
          =gfDoTriger('SOORD',PADR('TRMTE_CANL',10))
          *B128066,1 HBG Fix bug of file is used by another user [Start]
          SET REPROCESS TO lnRePro
          *B128066,1 HBG [End]

          RETURN
        ELSE

          *B608980,1 TMI show custom message for the cancel for BIN10 [start]
          IF ASCAN(loFormSet.laEvntTrig , PADR('SHCANMSG',10)) <> 0  && Show cancel message for BIN10 that user should cancel the allocation first
            =loFormSet.mDoTrigger(PADR('SHCANMSG',10))  && remove this
            RETURN .F.
          ELSE
            *B608980,1 TMI show custom message for the cancel FOR BIN10 [end  ]

            *-- Message : 32004
            *-- Some order lines hve been picked.
            *-- Button : 00000
            *-- Ok
            =gfModalGen('INM32004B00000','ALERT')

            *B610651,1 TMI 01/05/2014 13:13 [Start] If there is a piktkt in the ORDLINE with packing list in PACK_HDR then cancel the operatoin 
            IF lfCheck_if_Piktkt_is_Packed()
              RETURN .F.
            ENDIF 
            *B610651,1 TMI 01/05/2014 13:13 [End  ] 

            *B608980,1 TMI show custom message for the cancel FOR BIN10 [START]
          ENDIF
          *B608980,1 TMI show custom message for the cancel FOR BIN10 [end  ]

          *B608431,1 WAM 02/12/2008 Unless the pick ticket is sent to edi partner, allow cancel the sales order
          *B608431,1 WAM 02/12/2008 and releaes the pick ticket
          *!*			  *B128066,1 HBG Fix bug of file is used by another user [Start]
          *!*			  SET REPROCESS TO lnRePro
          *!*			  *B128066,1 HBG [End]
          *!*	          RETURN
          *B609894,1 HIA 22/04/2011 When cancelling an order has pick ticket, error 'Status' variable not found T20120327.0036 [Begin]
          SELECT piktkt
          *B609894,1 HIA 22/04/2011 When cancelling an order has pick ticket, error 'Status' variable not found T20120327.0036 [End]
          IF 'AS' $ oAriaApplication.CompanyInstalledModules
            LOCATE REST WHILE ORDER = OrdHdr.ORDER FOR STATUS $ 'OHP' AND  cSndPkt ='Y'
            IF FOUND()
              *-- Message : 32144
              *-- Warehouse shipping order has been sent for piktkt# 999999. Can't cancel sales order.
              *-- Button : 00000
              *-- Ok
              =gfModalGen('INM32144B00000','ALERT',PikTkt.PikTkt)
              SET REPROCESS TO lnRePro
              RETURN
            ENDIF
          ENDIF
          *B608431,1 WAM 02/12/2008 (End)
        ENDIF
      ENDIF
    ENDIF
    =gfOpenFile(oAriaApplication.DataDir+'PIKLINE',oAriaApplication.DataDir+'PIKLINE','SH')
  ENDIF
  IF OrdHdr.TotCut > 0
    *-- Message : 32054
    *-- This order has quantity allocated.
    *-- Canceling this order will release this allocation.
    *-- Would you like to continue ?
    *-- Button : 32000
    *-- Yes  No
    IF gfModalGen('QRM32054B32000','ALERT')=2
      *B128066,1 HBG Fix bug of file is used by another user [Start]
      SET REPROCESS TO lnRePro
      *B128066,1 HBG [End]

      RETURN
    ENDIF
  ENDIF
  *-- Message : 32005
  *-- Cancel all open items on this order?
  *-- Button : 320000
  *-- Yes/No
  IF gfModalGen('QRM32005B32000','ALERT',IIF(OrdHdr.cOrdType='C','contract','order')) = 2
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]

    RETURN
  ENDIF
  *-- Delete order's records in CSTTRAIN,CSTSOFTW files.
  IF ASCAN(oAriaApplication.laEvntTrig , PADR('DELSOFT',10)) <> 0
    =gfDoTriger('SOORD',PADR('DELSOFT',10))
  ENDIF
  *-- Get Cancellation reason
  lcCanReason = lfCanReason()
  IF EMPTY(lcCanReason)
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]

    RETURN
  ENDIF
  IF OrdHdr.STATUS='B'
    SELECT OrdHdr
    REPLACE STATUS     WITH 'X' ,;
      cCancReson WITH lcCanReason ,;
      Cancelled  WITH oAriaApplication.SystemDate ,;
      FLAG       WITH SPACE(1)
    *-- Update the CRMesag file
    DO lpUpdMesag
    *B128066,1 HBG Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 HBG [End]

    RETURN
  ENDIF
  *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
  llCancelBulk = .F.
  IF !EMPTY(ordhdr.cfromorder) AND  gfModalGen('QRM52026B32000','DIALOG',ordhdr.cfromorder) =1
    llCancelBulk = .T.
  ENDIF
  *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]

  WAIT 'Cancelling and updating Cut & Sold...' WINDOW NOWAIT
  *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
  *=gfOpenFile(oAriaApplication.DataDir+'icStyHst',oAriaApplication.DataDir+'Styhst','SH')
  =gfOpenTable('ICSTYHST','Styhst','SH')
  *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
  *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
  lcICStyHstTable = ''
  lnTableICStyHst = gfGetRemoteTable(SET("Datasession"),'ICSTYHST')
  IF lnTableICStyHst<>0 && Remote Table Object was Found
    lcICStyHstTable = oAriaApplication.laRemoteTable[lnTableICStyHst].lcCursorUpdate
  ENDIF
  *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
  STORE 0 TO lnOpen,lnOpenAmt,lnBook,lnBookAmt
  SELECT ORDLINE
  =SEEK(OrdHdr.cOrdType+OrdHdr.ORDER)
  SCAN REST WHILE cOrdType+ORDER+STR(LINENO,6)= OrdHdr.cOrdType+OrdHdr.ORDER

    *! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[Start]
    IF !EMPTY(ORDLINE.Employee)
      lfUpStyHist(.T.)
    ENDIF
    *! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[End]

    *-- Decrease warehouse ordered quantity with order open quantity
    IF cOrdType='O' .AND. SEEK(STYLE+cWareCode+SPACE(10),'StyDye')
      =RLOCK("StyDye")
      REPLACE Ord1   WITH MAX(Ord1-ORDLINE.Qty1,0) ,;
        Ord2   WITH MAX(Ord2-ORDLINE.Qty2,0) ,;
        Ord3   WITH MAX(Ord3-ORDLINE.Qty3,0) ,;
        Ord4   WITH MAX(Ord4-ORDLINE.Qty4,0) ,;
        Ord5   WITH MAX(Ord5-ORDLINE.Qty5,0) ,;
        Ord6   WITH MAX(Ord6-ORDLINE.Qty6,0) ,;
        Ord7   WITH MAX(Ord7-ORDLINE.Qty7,0) ,;
        Ord8   WITH MAX(Ord8-ORDLINE.Qty8,0) ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
      *-- Decrease warehouse allocated quantity with order picked quantity
      IF Picked
        REPLACE Alo1   WITH MAX(Alo1-ORDLINE.Pik1,0) ,;
          Alo2   WITH MAX(Alo2-ORDLINE.Pik2,0) ,;
          Alo3   WITH MAX(Alo3-ORDLINE.Pik3,0) ,;
          Alo4   WITH MAX(Alo4-ORDLINE.Pik4,0) ,;
          Alo5   WITH MAX(Alo5-ORDLINE.Pik5,0) ,;
          Alo6   WITH MAX(Alo6-ORDLINE.Pik6,0) ,;
          Alo7   WITH MAX(Alo7-ORDLINE.Pik7,0) ,;
          Alo8   WITH MAX(Alo8-ORDLINE.Pik8,0) ,;
          TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8 IN StyDye
      ENDIF
      UNLOCK IN StyDye
    ENDIF

    *-- Decrease style ordered quantity with order open quantity
    IF cOrdType='O' .AND. SEEK(STYLE,'Style')
      =RLOCK("Style")
      REPLACE Ord1   WITH MAX(Ord1-ORDLINE.Qty1,0) ,;
        Ord2   WITH MAX(Ord2-ORDLINE.Qty2,0) ,;
        Ord3   WITH MAX(Ord3-ORDLINE.Qty3,0) ,;
        Ord4   WITH MAX(Ord4-ORDLINE.Qty4,0) ,;
        Ord5   WITH MAX(Ord5-ORDLINE.Qty5,0) ,;
        Ord6   WITH MAX(Ord6-ORDLINE.Qty6,0) ,;
        Ord7   WITH MAX(Ord7-ORDLINE.Qty7,0) ,;
        Ord8   WITH MAX(Ord8-ORDLINE.Qty8,0) ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN STYLE
      *-- Decrease style allocated quantity with order picked quantity
      IF Picked
        REPLACE Alo1   WITH MAX(Alo1-ORDLINE.Pik1,0) ,;
          Alo2   WITH MAX(Alo2-ORDLINE.Pik2,0) ,;
          Alo3   WITH MAX(Alo3-ORDLINE.Pik3,0) ,;
          Alo4   WITH MAX(Alo4-ORDLINE.Pik4,0) ,;
          Alo5   WITH MAX(Alo5-ORDLINE.Pik5,0) ,;
          Alo6   WITH MAX(Alo6-ORDLINE.Pik6,0) ,;
          Alo7   WITH MAX(Alo7-ORDLINE.Pik7,0) ,;
          Alo8   WITH MAX(Alo8-ORDLINE.Pik8,0) ,;
          TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8 IN STYLE
      ENDIF
      UNLOCK IN STYLE
    ENDIF
    *-- Update dyelot allocated quantity
    IF cOrdType='O' AND !EMPTY(Dyelot) AND SEEK(STYLE+cWareCode+Dyelot,'StyDye')
      =RLOCK("StyDye")
      REPLACE Ord1   WITH MAX(Ord1-ORDLINE.Qty1,0) ,;
        Ord2   WITH MAX(Ord2-ORDLINE.Qty2,0) ,;
        Ord3   WITH MAX(Ord3-ORDLINE.Qty3,0) ,;
        Ord4   WITH MAX(Ord4-ORDLINE.Qty4,0) ,;
        Ord5   WITH MAX(Ord5-ORDLINE.Qty5,0) ,;
        Ord6   WITH MAX(Ord6-ORDLINE.Qty6,0) ,;
        Ord7   WITH MAX(Ord7-ORDLINE.Qty7,0) ,;
        Ord8   WITH MAX(Ord8-ORDLINE.Qty8,0) ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
      IF Picked
        REPLACE Alo1   WITH MAX(Alo1-ORDLINE.Pik1,0) ,;
          Alo2   WITH MAX(Alo2-ORDLINE.Pik2,0) ,;
          Alo3   WITH MAX(Alo3-ORDLINE.Pik3,0) ,;
          Alo4   WITH MAX(Alo4-ORDLINE.Pik4,0) ,;
          Alo5   WITH MAX(Alo5-ORDLINE.Pik5,0) ,;
          Alo6   WITH MAX(Alo6-ORDLINE.Pik6,0) ,;
          Alo7   WITH MAX(Alo7-ORDLINE.Pik7,0) ,;
          Alo8   WITH MAX(Alo8-ORDLINE.Pik8,0) ,;
          TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8 IN StyDye
      ENDIF
      UNLOCK IN StyDye
    ENDIF

    *-- Decrease ordered quantity & amount in the style history with order open quantity
    *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
    *IF cOrdType='O' .AND. SEEK(STYLE+lcGlYear,'icStyHst')

    old_Alias = ALIAS()
    lcOrdType = cOrdType
    lcstyle_var = STYLE
    SELECT icStyHst
    *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][Begin]
    *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
    *IF !gfSEEK(lcStyle_Var+ lcGlYear)
    IF !SEEK(lcStyle_Var+ lcGlYear,lcICStyHstTable) AND !gfSEEK(lcStyle_Var+ lcGlYear)
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
      =gfOpenTable('ICSTYHST','STYHST','SH','ICSTYHSX')
      SELECT ICSTYHSX

      APPEND BLANK
      REPLACE STYLE WITH lcstyle_var
      REPLACE CFISFYEAR  WITH lcGlYear
      = GFREPLACE('')
      =gfTABLEUPDATE()

      *USE IN ICSTYHSX
      =gfcloseTable('ICSTYHSX')
      SELECT ICSTYHST
    ENDIF
    *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][End]
    *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
    *IF lcOrdType='O' .AND. gfSEEK(lcstyle_var +lcGlYear)
    IF lcOrdType='O' .AND.(SEEK(lcstyle_var +lcGlYear,lcICStyHstTable) OR  gfSEEK(lcstyle_var +lcGlYear,'ICSTYHST'))
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
      SELECT (old_Alias)
      *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
      lnOrdAmt = TotQty*Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
      IF !SEEK(lcstyle_var +lcGlYear,lcICStyHstTable)
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[END]
        =RLOCK("icStyHst")
        REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - ORDLINE.TotQty ,;
          nOrdQty            WITH nOrdQty            - ORDLINE.TotQty ,;
          nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt ,;
          nOrdAmt            WITH nOrdAmt            - lnOrdAmt IN icStyHst
        UNLOCK IN icStyHst
        *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
        SELECT icStyHst
        =gfReplace('')
        *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
        *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[Start]
      ELSE
        REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - ORDLINE.TotQty ,;
          nOrdQty            WITH nOrdQty            - ORDLINE.TotQty ,;
          nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt ,;
          nOrdAmt            WITH nOrdAmt            - lnOrdAmt IN (lcICStyHstTable)
      ENDIF
      *! B610149,1 MMT 11/08/2012 Error while saving order after editing order to add new line[End]
    ENDIF
    *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
    SELECT (Old_Alias)
    *! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
    *-- Release pick ticket and zero out order picked quantity
    IF Picked
      IF SEEK(PikTkt,'PIKTKT','PIKTKT')
        =RLOCK("PIKTKT")
        REPLACE STATUS WITH 'X' IN PikTkt
        *B610693,1 TMI 03/09/2014 12:53 [Start] update the edit fields in PIKTKT table 
        REPLACE CEDIT_USER WITH oAriaApplication.User_ID ;               
                DEDIT_DATE WITH DATE() ;
                CEDIT_TIME WITH TIME() ;
                IN PikTkt
        *B610693,1 TMI 03/09/2014 12:53 [End  ] 
        UNLOCK IN PikTkt
        *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
        IF TYPE('loFormSet') ='O' AND ASCAN(loFormSet.laEvntTrig,PADR('CANPCKPACK',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
          =loFormSet.mDoTrigger(PADR('CANPCKPACK',10))
        ENDIF
        *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
      ENDIF
      IF !SEEK(PikTkt+ORDER+STR(LINENO,6),'PIKLINE')
        SCATTER MEMVAR MEMO
        INSERT INTO PIKLINE FROM MEMVAR
      ENDIF
      SELECT ORDLINE
      =RLOCK()
      REPLACE Pik1   WITH 0 ,;
        Pik2   WITH 0 ,;
        Pik3   WITH 0 ,;
        Pik4   WITH 0 ,;
        Pik5   WITH 0 ,;
        Pik6   WITH 0 ,;
        Pik7   WITH 0 ,;
        Pik8   WITH 0 ,;
        TOTPIK WITH 0 ,;
        Picked WITH .F. ,;
        PikTkt WITH SPACE(6) ,;
        PIKDATE WITH {}
      IF SEEK(AltStyle,'STYLE') .AND. SEEK('S'+STYLE.SCALE,'SCALE')
        REPLACE STYLE     WITH AltStyle,;
          AltStyle  WITH SPACE(19),;
          SCALE     WITH SCALE.SCALE,;
          cWareCode WITH OrdHdr.cWareCode
        REPLACE Qty1  WITH IIF(SCALE.CNT>=1,Qty1,0)  ,;
          Book1 WITH IIF(SCALE.CNT>=1,Book1,0) ,;
          Qty2  WITH IIF(SCALE.CNT>=2,Qty2,0)  ,;
          Book2 WITH IIF(SCALE.CNT>=2,Book2,0) ,;
          Qty3  WITH IIF(SCALE.CNT>=3,Qty3,0)  ,;
          Book3 WITH IIF(SCALE.CNT>=3,Book3,0) ,;
          Qty4  WITH IIF(SCALE.CNT>=4,Qty4,0)  ,;
          Book4 WITH IIF(SCALE.CNT>=4,Book4,0) ,;
          Qty5  WITH IIF(SCALE.CNT>=5,Qty5,0)  ,;
          Book5 WITH IIF(SCALE.CNT>=5,Book5,0) ,;
          Qty6  WITH IIF(SCALE.CNT>=6,Qty6,0)  ,;
          Book6 WITH IIF(SCALE.CNT>=6,Book6,0) ,;
          Qty7  WITH IIF(SCALE.CNT>=7,Qty7,0)  ,;
          Book7 WITH IIF(SCALE.CNT>=7,Book7,0) ,;
          Qty8  WITH IIF(SCALE.CNT>=8,Qty8,0)  ,;
          Book8 WITH IIF(SCALE.CNT>=8,Book8,0) ,;
          TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
          TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8
      ENDIF
      UNLOCK
    ENDIF
    SELECT ORDLINE
    =RLOCK()
    REPLACE Cut1   WITH 0 ,;
      Cut2   WITH 0 ,;
      Cut3   WITH 0 ,;
      Cut4   WITH 0 ,;
      Cut5   WITH 0 ,;
      Cut6   WITH 0 ,;
      Cut7   WITH 0 ,;
      Cut8   WITH 0 ,;
      TotCut WITH 0
    UNLOCK
    lnOpen    = lnOpen    + ORDLINE.TotQty
    lnOpenAmt = lnOpenAmt + ORDLINE.TotQty*ORDLINE.Price
    lnBook    = lnBook    + ORDLINE.TotBook
    lnBookAmt = lnBookAmt + ORDLINE.TotBook*ORDLINE.Price
    IF OrdHdr.Ship > 0 .AND. ORDLINE.TotQty > 0
      IF !SEEK(ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'ORDCANLN')
        *-- Update OrdCanLn with Style,Account,Store and Dyelot.
        INSERT INTO ('ORDCANLN') ;
          (cOrdType,ORDER,LINENO,Cancelled,cCancReson,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty, ;
          Account,STYLE,STORE,Dyelot,Price) ;
          VALUES ;
          (ORDLINE.cOrdType,ORDLINE.ORDER,ORDLINE.LINENO,oAriaApplication.SystemDate,lcCanReason,ORDLINE.Qty1,ORDLINE.Qty2,;
          ORDLINE.Qty3,ORDLINE.Qty4,ORDLINE.Qty5,ORDLINE.Qty6,ORDLINE.Qty7,ORDLINE.Qty8,;
          ORDLINE.TotQty,ORDLINE.Account,ORDLINE.STYLE,ORDLINE.STORE,ORDLINE.Dyelot,ORDLINE.Price)
      ENDIF
      REPLACE Qty1   WITH 0 ,;
        Qty2   WITH 0 ,;
        Qty3   WITH 0 ,;
        Qty4   WITH 0 ,;
        Qty5   WITH 0 ,;
        Qty6   WITH 0 ,;
        Qty7   WITH 0 ,;
        Qty8   WITH 0 ,;
        TotQty WITH 0
    ENDIF
    *B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [Begin]
    SELECT ORDLINE
    *CurRecOrdLine = ORDLINE.cOrdType+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
    && Adjust Style And StyDye Record Pointers.
    = SEEK(ORDLINE.STYLE,'Style')
    = SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')

    FOR innerstr = 1 TO 8
      cstr = ALLTRIM(STR(innerstr))
      OldQTY&cstr.  = ORDLINE.Qty&cstr.
      OldBook&cstr. = ORDLINE.Book&cstr.
    ENDFOR

    lcFromOrder = ORDLINE.cFromOrder
    lBulkLineNo = ORDLINE.BulkLineNo
    lcOrdType   = ORDLINE.cOrdType
    lcDyelot    = ORDLINE.Dyelot
    lcSTYLE     = ORDLINE.STYLE
    lcWareCode  = ORDLINE.cWareCode

    IF !EMPTY(lcFromOrder) .AND. SEEK(lcOrdType+lcFromOrder+STR(lBulkLineNo,6),'BlkOrdLine') .AND. SEEK(lcOrdType+lcFromOrder,'BlkOrdHd','OrdHdr')
      lnOpen      = 0
      lnOpenAmt   = 0
      lnCancel    = 0
      lnCancelAmt = 0
      *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
      *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][Start]
      *!*	      IF llCancelBulk
      *!*	        lnAliasSel = SELECT()
      *!*	        SELECT BlkOrdLine
      *!*	        SCATTER MEMO MEMVAR
      *!*	        m.TotQty = 0
      *!*	        FOR innerLoopCounter = 1 TO 8
      *!*	          cstr = ALLTRIM(STR(innerLoopCounter))
      *!*	          m.Qty&cstr = 0
      *!*	        ENDFOR
      *!*	        m.cCancReson = lcCanReason
      *!*	        m.Cancelled  =oAriaApplication.SystemDate
      *!*	        SELECT (lnAliasSel)
      *!*	      ENDIF
      *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][End]
      *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[End]

      FOR innerLoopCounter = 1 TO 8
        cstr = ALLTRIM(STR(innerLoopCounter))
        xDiff = OldQTY&cstr.
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        IF !llCancelBulk
          *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]
          REPLACE Qty&cstr  WITH (Qty&cstr. + xDiff) IN BlkOrdLine
          REPLACE TotQty    WITH (TotQty + xDiff)    IN BlkOrdLine
          lnOpen    = lnOpen + xDiff
          lnOpenAmt = lnOpenAmt + (xDiff * ORDLINE.Price)
          *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        ELSE
          *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][Start]
          *m.Qty&cstr = xDiff
          *m.TotQty = m.TotQty +  m.Qty&cstr
          *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][End]
          *! B610578,1 MMT 11/07/2013 Fix issue of uncancelling SO , changes order status to Bid not Open[T20131017.0007][Start]
          lnOpen    = lnOpen + xDiff
          lnOpenAmt = lnOpenAmt + (xDiff * ORDLINE.Price)
          *! B610578,1 MMT 11/07/2013 Fix issue of uncancelling SO , changes order status to Bid not Open[T20131017.0007][End]
        ENDIF
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]
        xDiff = OldBook&cstr.
        REPLACE Book&cstr  WITH (Book&cstr. + xDiff) IN BlkOrdLine
        REPLACE TotBook    WITH (TotBook + xDiff)    IN BlkOrdLine
        lnCancel = lnCancel + xDiff
        lnCancelAmt = lnCancelAmt + (xDiff * ORDLINE.Price)
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        IF !llCancelBulk
          *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]
          =RLOCK('Style')
          REPLACE  Ord&cstr.   WITH Ord&cstr. + xDiff ,;
            TotOrd      WITH TotOrd    + xDiff IN STYLE
          UNLOCK IN 'Style'

          =RLOCK('StyDye')
          REPLACE Ord&cstr.  WITH Ord&cstr.  + xDiff  ,;
            TotOrd     WITH TotOrd     + xDiff  IN StyDye
          UNLOCK IN 'StyDye'
          *-- Update the Qty on the Configuration level
          IF !EMPTY(lcDyelot) AND SEEK(lcSTYLE+lcWareCode+lcDyelot,'StyDye')
            REPLACE Ord&cstr.   WITH Ord&cstr. + xDiff ,;
              TotOrd      WITH TotOrd    + xDiff IN StyDye
            UNLOCK IN 'StyDye'
          ENDIF
          *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
        ENDIF
        *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]
      ENDFOR

      && Modify the Bulk Order Header Totals And Adjust The Status Depend On The Totals values
      *! B610578,1 MMT 11/07/2013 Fix issue of uncancelling SO , changes order status to Bid not Open[T20131017.0007][Start]
      IF !llCancelBulk
      *! B610578,1 MMT 11/07/2013 Fix issue of uncancelling SO , changes order status to Bid not Open[T20131017.0007][End]
      =RLOCK("BlkOrdHd")
      SELECT BlkOrdHd
      REPLACE Book      WITH (Book  + lnCancel)   ,;
        BookAmt   WITH (BookAmt + lnCancelAmt),;
        OPEN      WITH (OPEN    + lnOpen)     ,;
        OpenAmt   WITH (OpenAmt + lnOpenAmt)  ,;
        STATUS    WITH IIF(OPEN = 0 ,'X',STATUS) ,;
        APPRAMT   WITH MAX(0,APPRAMT + lnOpenAmt) IN BlkOrdHd
      *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]
      *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][Start]
      *!*	      IF llCancelBulk
      *!*	        IF !USED('ORDCANLN_A')
      *!*	          =gfOpenTable('ORDCANLN','ORDCANLN','SH','ORDCANLN_A')
      *!*	        ENDIF
      *!*	        SELECT 'ORDCANLN_A'
      *!*	        APPEND BLANK
      *!*	        GATHER MEMO MEMVAR
      *!*	        =gfAdd_Info('ORDCANLN_A')
      *!*	        SELECT BlkOrdHd
      *!*	        REPLACE CANCEL      WITH (CANCEL    + lnCancel)     ,;
      *!*	          CANCELAmt   WITH (CANCELAmt + lnCancelAmt) IN BlkOrdHd
      *!*	      ENDIF
      *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][End]
      *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]
      UNLOCK IN BlkOrdHd
      IF BlkOrdHd.STATUS='X' .AND. BlkOrdHd.OPEN > 0 .AND. SEEK('M'+BlkOrdHd.Account,'Customer')
        =RLOCK("BlkOrdHd")
        REPLACE STATUS WITH "O" IN BlkOrdHd
        UNLOCK IN BlkOrdHd

        =RLOCK('Customer')
        REPLACE nBulk WITH nBulk + 1 IN Customer
        UNLOCK IN 'Customer'
      ENDIF
    *! B610578,1 MMT 11/07/2013 Fix issue of uncancelling SO , changes order status to Bid not Open[T20131017.0007][Start]
    ENDIF
    *! B610578,1 MMT 11/07/2013 Fix issue of uncancelling SO , changes order status to Bid not Open[T20131017.0007][End]
    ENDIF
    SELECT ORDLINE
    *=SEEK(CurRecOrdLine,"ORDLINE")
    *B609698,1 HIA 10/04/2011 When cancelling an order attached to a bulk , the QTY should go back to the bulk T20061219.0011 [End]

  ENDSCAN
  *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]

  IF llCancelBulk AND USED('ORDCANLN_A')
    SELECT 'ORDCANLN_A'
    =gfTableUpdate()
  ENDIF
  *! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[END]

  *-- Release order allocated quantity to any cutting ticket order style purchae order
  llUpdate = .F.
  IF OrdHdr.TotCut > 0
    lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
    IF TYPE('lcTranCode') = 'N'
      =oAriaApplication.RemoteCompanyData.CheckRetResult("BEGINTRAN",lcTranCode,.T.)
    ELSE
      lcSelString =               "SELECT CutPick.*,PosHdr.Status FROM CutPick INNER JOIN PosHdr On PosHdr.cBusDocu+PosHdr.cStyType+PosHdr.Po = 'PU'+CutPick.cTktNo WHERE CutPick.TranCd+CutPick.[Order]+CutPick.cOrdLine LIKE '1" + OrdHdr.ORDER + "%' UNION "
      lcSelString = lcSelString + "SELECT CutPick.*,PosHdr.Status FROM CutPick INNER JOIN PosHdr ON PosHdr.cBusDocu+PosHdr.cStyType+PosHdr.Po = 'PP'+CutPick.cTktNo WHERE CutPick.TranCd+CutPick.[Order]+CutPick.cOrdLine LIKE '2" + OrdHdr.ORDER + "%' ORDER BY trancd,ctktno,cTKtLineNo"
      IF oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"CutPick","CutPick ",lcTranCode,4,'',SET("DATASESSION")) = 1

        *B607585,1 AMH Open the POSLN file [Start]
        LOCAL lcPosLn,loPosLn
        lcPosLn = gfTempName()
        loPosLn = CREATEOBJECT('RemoteTable','POSLN','POSLN',lcPosLn,SET("DATASESSION"))
        *B607585,1 AMH [End]

        SELECT CUTPICK
        GO TOP
        DO WHILE !EOF()
          lcTranCd = TRANCD
          lcTktNo  = CTKTNO
          lnTotOrd = 0
          *B607917,1 WAM 01/02/2007 Get all lines for this PO
          IF lcTranCd = '1'
            =loPosLn.SEEK("PU"+CUTPICK.CTKTNO+'0001')
          ELSE
            =loPosLn.SEEK("PP"+CUTPICK.CTKTNO+'0001')
          ENDIF
          *B607917,1 WAM (End)

          *-- Message : 32006
          *-- Cuttkt# xxxxxx status is HOLD. Do you wish to update the generated
          *-- cuttkt quantities accordingly?
          *-- Button : 32000
          *-- Yes/No
          *-- Message : 32007
          *-- Purchase order# xxxxxx status is HOLD. Do you wish to update the generated
          *-- Purchase order quantities accordingly?
          *-- Button : 32000
          *-- Yes/No
          llUpdate= STATUS='H' .AND. ((TRANCD='1' .AND. gfModalGen('QRM32006B32000','ALERT',CTKTNO)=1) .OR. ;
            (TRANCD='2' .AND. gfModalGen('QRM32007B32000','ALERT',CTKTNO)=1) )

          SCAN REST WHILE TRANCD+CTKTNO+CTKTLINENO = lcTranCd+lcTktNo

            =SEEK('O'+ORDER+CORDLINE,'OrdLine','OrdLine')

            *-- Update ticket lines Ordered quantity
            *B607585,1 AMH Update POSLN file [Start]
            *lcSelString="UPDATE PosLn SET Ord1=Ord1-"+ALLTRIM(STR(CutPick.Qty1))+",Ord2=Ord2-"+ALLTRIM(STR(CutPick.Qty2))+;
            *",Ord3=Ord3-"+ALLTRIM(STR(CutPick.Qty3))+",Ord4=Ord4-"+ALLTRIM(STR(CutPick.Qty4))+;
            *",Ord5=Ord5-"+ALLTRIM(STR(CutPick.Qty5))+",Ord6=Ord6-"+ALLTRIM(STR(CutPick.Qty6))+;
            *",Ord7=Ord7-"+ALLTRIM(STR(CutPick.Qty7))+",Ord8=Ord8-"+ALLTRIM(STR(CutPick.Qty8))+;
            *",TotOrd=TotOrd-"+ALLTRIM(STR(CutPick.TotQty))
            *-- Update ticket lines budget quantities
            *IF llUpdate
            *  lcSelString=lcSelString+;
            *  ",Qty1=Qty1-"+ALLTRIM(STR(CUTPICK.Qty1))+",Qty2=Qty2-"+ALLTRIM(STR(CUTPICK.Qty2))+;
            *  ",Qty3=Qty3-"+ALLTRIM(STR(CUTPICK.Qty3))+",Qty4=Qty4-"+ALLTRIM(STR(CUTPICK.Qty4))+;
            *  ",Qty5=Qty5-"+ALLTRIM(STR(CUTPICK.Qty5))+",Qty6=Qty6-"+ALLTRIM(STR(CUTPICK.Qty6))+;
            *  ",Qty7=Qty7-"+ALLTRIM(STR(CUTPICK.Qty7))+",Qty8=Qty8-"+ALLTRIM(STR(CUTPICK.Qty8))+;
            *  ",TotQty=TotQty-"+ALLTRIM(STR(CUTPICK.TotQty))
            *ENDIF
            *IF lcTranCd = '1'
            *  lcSelString=lcSelString+" WHERE cBusDocu+cstytype+po+cInvType+style+STR([lineno],6)+trancd='PU"+CUTPICK.CTKTNO+'0001'+CUTPICK.Style+CUTPICK.cTktLineNo+"1'"
            *ELSE
            *  lcSelString=lcSelString+" WHERE cBusDocu+cstytype+po+cInvType+style+STR([lineno],6)+trancd='PP"+CUTPICK.CTKTNO+'0001'+CUTPICK.Style+CUTPICK.cTktLineNo+"1'"
            *ENDIF
            *lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode,4,'',SET("DATASESSION"))
            IF lcTranCd = '1'
              *B607917,1 WAM 01/02/2007 Get all lines for this PO
              *=loPosLn.seek("PU"+CUTPICK.CTKTNO+'0001'+CUTPICK.Style+CUTPICK.cTktLineNo+"1")
              =SEEK("PU"+CUTPICK.CTKTNO+'0001'+CUTPICK.STYLE+CUTPICK.CTKTLINENO+"1",lcPosLn)
              *B607917,1 WAM (End)
            ELSE
              *B607917,1 WAM 01/02/2007 Get all allocated lines for this PO
              *=loPosLn.seek("PP"+CUTPICK.CTKTNO+'0001'+CUTPICK.Style+CUTPICK.cTktLineNo+"1")
              =SEEK("PP"+CUTPICK.CTKTNO+'0001'+CUTPICK.STYLE+CUTPICK.CTKTLINENO+"1",lcPosLn)
              *B607917,1 WAM (End)
            ENDIF
            SELECT (lcPosLn)
            *-- Update ticket lines Ordered quantity
            loPosLn.REPLACE("Ord1   WITH Ord1   - CUTPICK.Qty1,"+;
              "Ord2   WITH Ord2   - CUTPICK.Qty2,"+;
              "Ord3   WITH Ord3   - CUTPICK.Qty3,"+;
              "Ord4   WITH Ord4   - CUTPICK.Qty4,"+;
              "Ord5   WITH Ord5   - CUTPICK.Qty5,"+;
              "Ord6   WITH Ord6   - CUTPICK.Qty6,"+;
              "Ord7   WITH Ord7   - CUTPICK.Qty7,"+;
              "Ord8   WITH Ord8   - CUTPICK.Qty8,"+;
              "TotOrd WITH TotOrd - CUTPICK.TotQty")
            *-- Update ticket lines budget quantities
            IF llUpdate
              loPosLn.REPLACE("Qty1   WITH Qty1   - CUTPICK.Qty1,"+;
                "Qty2   WITH Qty2   - CUTPICK.Qty2,"+;
                "Qty3   WITH Qty3   - CUTPICK.Qty3,"+;
                "Qty4   WITH Qty4   - CUTPICK.Qty4,"+;
                "Qty5   WITH Qty5   - CUTPICK.Qty5,"+;
                "Qty6   WITH Qty6   - CUTPICK.Qty6,"+;
                "Qty7   WITH Qty7   - CUTPICK.Qty7,"+;
                "Qty8   WITH Qty8   - CUTPICK.Qty8,"+;
                "TotQty WITH TotQty - CUTPICK.TotQty")
              *B607917,1 WAM 01/02/2007 Update all PO lines once after the loop
              REPLACE Qty1   WITH Qty1   - CUTPICK.Qty1,;
                Qty2   WITH Qty2   - CUTPICK.Qty2,;
                Qty3   WITH Qty3   - CUTPICK.Qty3,;
                Qty4   WITH Qty4   - CUTPICK.Qty4,;
                Qty5   WITH Qty5   - CUTPICK.Qty5,;
                Qty6   WITH Qty6   - CUTPICK.Qty6,;
                Qty7   WITH Qty7   - CUTPICK.Qty7,;
                Qty8   WITH Qty8   - CUTPICK.Qty8,;
                TotQty WITH TotQty - CUTPICK.TotQty IN (lcPosLn)
              *B607917,1 WAM (End)
            ENDIF
            *B607917,1 WAM 01/02/2007 Update all PO lines once after the loop
            *loPosLn.TableUpdate(lcTranCode)
            *B607917,1 WAM (End)

            *B607585,1 AMH [End]

            IF SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')
              =RLOCK("StyDye")
              REPLACE WIP1   WITH MAX(WIP1-ORDLINE.Qty1,0) ,;
                WIP2   WITH MAX(WIP2-ORDLINE.Qty2,0) ,;
                WIP3   WITH MAX(WIP3-ORDLINE.Qty3,0) ,;
                WIP4   WITH MAX(WIP4-ORDLINE.Qty4,0) ,;
                WIP5   WITH MAX(WIP5-ORDLINE.Qty5,0) ,;
                WIP6   WITH MAX(WIP6-ORDLINE.Qty6,0) ,;
                WIP7   WITH MAX(WIP7-ORDLINE.Qty7,0) ,;
                WIP8   WITH MAX(WIP8-ORDLINE.Qty8,0) ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8 IN StyDye
              REPLACE NWO1   WITH MAX(NWO1-ORDLINE.Qty1,0) ,;
                NWO2   WITH MAX(NWO2-ORDLINE.Qty2,0) ,;
                NWO3   WITH MAX(NWO3-ORDLINE.Qty3,0) ,;
                NWO4   WITH MAX(NWO4-ORDLINE.Qty4,0) ,;
                NWO5   WITH MAX(NWO5-ORDLINE.Qty5,0) ,;
                NWO6   WITH MAX(NWO6-ORDLINE.Qty6,0) ,;
                NWO7   WITH MAX(NWO7-ORDLINE.Qty7,0) ,;
                NWO8   WITH MAX(NWO8-ORDLINE.Qty8,0) ,;
                NTOTWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8 IN StyDye
              UNLOCK IN StyDye
            ENDIF
            IF !EMPTY(ORDLINE.Dyelot) AND SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+ORDLINE.Dyelot,'StyDye')
              =RLOCK("StyDye")
              REPLACE WIP1   WITH MAX(WIP1-ORDLINE.Qty1,0) ,;
                WIP2   WITH MAX(WIP2-ORDLINE.Qty2,0) ,;
                WIP3   WITH MAX(WIP3-ORDLINE.Qty3,0) ,;
                WIP4   WITH MAX(WIP4-ORDLINE.Qty4,0) ,;
                WIP5   WITH MAX(WIP5-ORDLINE.Qty5,0) ,;
                WIP6   WITH MAX(WIP6-ORDLINE.Qty6,0) ,;
                WIP7   WITH MAX(WIP7-ORDLINE.Qty7,0) ,;
                WIP8   WITH MAX(WIP8-ORDLINE.Qty8,0) ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8 IN StyDye
              REPLACE NWO1   WITH MAX(NWO1-ORDLINE.Qty1,0) ,;
                NWO2   WITH MAX(NWO2-ORDLINE.Qty2,0) ,;
                NWO3   WITH MAX(NWO3-ORDLINE.Qty3,0) ,;
                NWO4   WITH MAX(NWO4-ORDLINE.Qty4,0) ,;
                NWO5   WITH MAX(NWO5-ORDLINE.Qty5,0) ,;
                NWO6   WITH MAX(NWO6-ORDLINE.Qty6,0) ,;
                NWO7   WITH MAX(NWO7-ORDLINE.Qty7,0) ,;
                NWO8   WITH MAX(NWO8-ORDLINE.Qty8,0) ,;
                NTOTWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8 IN StyDye
              UNLOCK IN StyDye
            ENDIF
            *-- Decrease style ordered quantity with order open quantity
            IF SEEK(ORDLINE.STYLE,'Style')
              =RLOCK("Style")
              REPLACE WIP1   WITH MAX(WIP1-ORDLINE.Qty1,0) ,;
                WIP2   WITH MAX(WIP2-ORDLINE.Qty2,0) ,;
                WIP3   WITH MAX(WIP3-ORDLINE.Qty3,0) ,;
                WIP4   WITH MAX(WIP4-ORDLINE.Qty4,0) ,;
                WIP5   WITH MAX(WIP5-ORDLINE.Qty5,0) ,;
                WIP6   WITH MAX(WIP6-ORDLINE.Qty6,0) ,;
                WIP7   WITH MAX(WIP7-ORDLINE.Qty7,0) ,;
                WIP8   WITH MAX(WIP8-ORDLINE.Qty8,0) ,;
                TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8 IN STYLE
              REPLACE NWO1   WITH MAX(NWO1-ORDLINE.Qty1,0) ,;
                NWO2   WITH MAX(NWO2-ORDLINE.Qty2,0) ,;
                NWO3   WITH MAX(NWO3-ORDLINE.Qty3,0) ,;
                NWO4   WITH MAX(NWO4-ORDLINE.Qty4,0) ,;
                NWO5   WITH MAX(NWO5-ORDLINE.Qty5,0) ,;
                NWO6   WITH MAX(NWO6-ORDLINE.Qty6,0) ,;
                NWO7   WITH MAX(NWO7-ORDLINE.Qty7,0) ,;
                NWO8   WITH MAX(NWO8-ORDLINE.Qty8,0) ,;
                NTOTWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8 IN STYLE
              UNLOCK IN STYLE
            ENDIF
            lnTotOrd = lnTotOrd + CUTPICK.TotQty
          ENDSCAN
          *HBG 3/28/2005 Get lines of POSLN to calculate the Amount of the PO/CT [Begin]

          *B607917,1 WAM 01/02/2007 PO lines already retreived. Commented out
          *lcSqlStatment = "SELECT * FROM POSLN [INDEX=POSLN] WHERE cBusDocu='P' AND cStyType='"+IIF(lcTranCd='1','U','P')+"' AND PO='"+lcTktNo+"'"
          *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.SqlRun(lcSqlStatment,'POSLN','POSLN',lcTranCode,4,'SAVE',SET("DATASESSION"))
          *SELECT POSLN
          SELECT (lcPosLn)
          *B607917,1 WAM (End)

          STORE 0 TO lnCost1,lnCost2,lnCost3,lnCost4,lnCost5,lnCost6,lnCost7 , lnPOTotal
          STORE 0 TO lnFCost1,lnFCost2,lnFCost3,lnFCost4,lnFCost5,lnFCost6,lnFCost7
          SCAN FOR TRANCD = '1'
            lnCost1 = lnCost1 + (TotQty * nicost1)
            lnCost2 = lnCost2 + (TotQty * nicost2)
            lnCost3 = lnCost3 + (TotQty * nicost3)
            lnCost4 = lnCost4 + (TotQty * nicost4)
            lnCost5 = lnCost5 + (TotQty * nicost5)
            lnCost6 = lnCost6 + (TotQty * nicost6)
            lnCost7 = lnCost7 + (TotQty * nicost7)

            lnFCost1 = lnFCost1 + (TotQty * nFcost1)
            lnFCost2 = lnFCost2 + (TotQty * nFcost2)
            lnFCost3 = lnFCost3 + (TotQty * nFcost3)
            lnFCost4 = lnFCost4 + (TotQty * nFcost4)
            lnFCost5 = lnFCost5 + (TotQty * nFcost5)
            lnFCost6 = lnFCost6 + (TotQty * nFcost6)
            lnFCost7 = lnFCost7 + (TotQty * nFcost7)
          ENDSCAN
          lnPOTotal = lnCost1+lnCost2+lnCost3+lnCost4+lnCost5+lnCost6+lnCost7
          *HBG [End]

          *B607917,1 WAM 01/02/2007 Update all PO lines once
          loPosLn.TABLEUPDATE(lcTranCode)
          *B607917,1 WAM (End)

          *-- Update ticket total Ordered & budget quantity
          IF lcTranCd = '1'
            *-- Change Cutting quantity if order allocated quantity has been changed
            *HBG 4/11/2005 Update the Amount of the CT in POSHDR [Begin]
            *lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd-"+ALLTRIM(STR(lnTotOrd))+;
            IIF(llUpdate,",nStyOrder = nStyOrder-"+ALLTRIM(STR(lnTotOrd))+",[Open]=[Open]-"+ALLTRIM(STR(lnTotOrd)),'')+;
            " WHERE cBusDocu+cstytype+po='PU"+lcTktNo+"'"
            lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd-"+ALLTRIM(STR(lnTotOrd))+;
              IIF(llUpdate,",nStyOrder = nStyOrder-"+ALLTRIM(STR(lnTotOrd))+",[Open]=[Open]-"+ALLTRIM(STR(lnTotOrd)),'')+;
              " ,niCost1 = "+ALLTRIM(STR(lnCost1))+" ,niCost2 = "+ALLTRIM(STR(lnCost2))+" ,niCost3 = "+ALLTRIM(STR(lnCost3))+" ,niCost4 = "+ALLTRIM(STR(lnCost4))+;
              " ,niCost5 = "+ALLTRIM(STR(lnCost5))+" ,niCost6 = "+ALLTRIM(STR(lnCost6))+" ,niCost7 = "+ALLTRIM(STR(lnCost7))+;
              ",nFCost1= "+ALLTRIM(STR(lnFCost1))+",nFCost2= "+ALLTRIM(STR(lnFCost2))+",nFCost3= "+ALLTRIM(STR(lnFCost3))+",nFCost4= "+ALLTRIM(STR(lnFCost4))+;
              ",nFCost5= "+ALLTRIM(STR(lnFCost5))+",nFCost6= "+ALLTRIM(STR(lnFCost6))+",nFCost7= "+ALLTRIM(STR(lnFCost7))+;
              " ,POTotal = "+ALLTRIM(STR(lnPOTotal))+" WHERE cBusDocu+cstytype+po='PU"+lcTktNo+"'"
            *HBG [End]
          ELSE
            *-- Change Purchased quantity if order allocated quantity has been changed
            *HBG 3/28/2005 Update the Amount of the CT in POSHDR [Begin]
            *lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd-"+ALLTRIM(STR(lnTotOrd))+;
            IIF(llUpdate,",nStyOrder = nStyOrder-"+ALLTRIM(STR(lnTotOrd))+",[Open]=[Open]-"+ALLTRIM(STR(lnTotOrd)),'')+;
            " WHERE cBusDocu+cstytype+po='PP"+lcTktNo+"'"
            lcSelString = "UPDATE PosHdr SET TotOrd=TotOrd-"+ALLTRIM(STR(lnTotOrd))+;
              IIF(llUpdate,",nStyOrder = nStyOrder-"+ALLTRIM(STR(lnTotOrd))+",[Open]=[Open]-"+ALLTRIM(STR(lnTotOrd)),'')+;
              " ,niCost1 = "+ALLTRIM(STR(lnCost1))+" ,niCost2 = "+ALLTRIM(STR(lnCost2))+" ,niCost3 = "+ALLTRIM(STR(lnCost3))+" ,niCost4 = "+ALLTRIM(STR(lnCost4))+;
              " ,niCost5 = "+ALLTRIM(STR(lnCost5))+" ,niCost6 = "+ALLTRIM(STR(lnCost6))+" ,niCost7 = "+ALLTRIM(STR(lnCost7))+;
              ",nFCost1= "+ALLTRIM(STR(lnFCost1))+",nFCost2= "+ALLTRIM(STR(lnFCost2))+",nFCost3= "+ALLTRIM(STR(lnFCost3))+",nFCost4= "+ALLTRIM(STR(lnFCost4))+;
              ",nFCost5= "+ALLTRIM(STR(lnFCost5))+",nFCost6= "+ALLTRIM(STR(lnFCost6))+",nFCost7= "+ALLTRIM(STR(lnFCost7))+;
              " ,POTotal = "+ALLTRIM(STR(lnPOTotal))+" WHERE cBusDocu+cstytype+po='PP"+lcTktNo+"'"
            *HBG [End]
          ENDIF
          lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode,4,'',SET("DATASESSION"))
          SELECT CUTPICK
        ENDDO

        *B607585,1 AMH Clsoe the POSLN file [Start]
        loPosLn = NULL
        *B607585,1 AMH [End]

        SELECT CUTPICK
        DELETE ALL
        lnResult = oAriaApplication.RemoteCompanyData.SqlUpdate('CUTPICK',lcTranCode, SET("DATASESSION"),'trancd,ctktno,ctktlineno,order,style,cordline')
        IF lnResult <=0
          =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
          =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
        ELSE
          IF oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.) = 1
          ELSE
            =oAriaApplication.RemoteCompanyData.CheckRetResult("COMMITTRAN",lnResult,.T.)
          ENDIF
        ENDIF
        USE IN CUTPICK
      ENDIF
    ENDIF
  ENDIF

  *-- Add any removed lines quantity to computed booked quantity
  IF SEEK(OrdHdr.cOrdType+OrdHdr.ORDER,'ORDCANLN')
    SELECT ORDCANLN
    SCAN REST WHILE cOrdType+ORDER+STR(LINENO,6) = OrdHdr.cOrdType+OrdHdr.ORDER
      IF !SEEK(cOrdType+ORDER+STR(LINENO,6),'OrdLine')
        lnBook    = lnBook    + ORDCANLN.TotQty
        lnBookAmt = lnBookAmt + ORDCANLN.TotQty*ORDCANLN.Price
      ENDIF
    ENDSCAN
  ENDIF
  *-- Decrease number of bulk orders for this account
  IF OrdHdr.Bulk='Y' .AND. SEEK('M'+OrdHdr.Account,'Customer')
    =RLOCK("CUSTOMER")
    REPLACE nBulk WITH nBulk - 1 IN Customer
    UNLOCK IN Customer
  ENDIF

  *! C201483,1 SAB 05/13/2012 New Customization to update site budget info on customer file[T20111206.0020][Start]
  *-- Add Trigger to Update Customer Site Budget info
  IF ASCAN(loFormSet.laEvntTrig , PADR('SBDELORD',10)) <> 0
    =loFormSet.mDoTrigger(PADR('SBDELORD',10))
  ENDIF
  *! C201483,1 SAB 05/13/2012 New Customization to update site budget info on customer file[T20111206.0020][End]

  *-- Decrease orders quantity and amount in the customerr history file
  IF OrdHdr.cOrdType='O'
    =gfOpenFile(oAriaApplication.DataDir+'arCusHst',oAriaApplication.DataDir+'Acthst','SH')
    =SEEK(OrdHdr.Account+lcGlYear,'arCusHst')
    lnOrdAmt = OrdHdr.OpenAmt &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
    =RLOCK("arCusHst")
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - OrdHdr.OPEN ,;
      nOrdQty            WITH nOrdQty            - OrdHdr.OPEN ,;
      nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt    ,;
      nOrdAmt            WITH nOrdAmt            - lnOrdAmt IN arCusHst
    UNLOCK IN arCusHst
  ENDIF

  *-- Update order open, Cancel and book quantity and amount and order status
  SELECT OrdHdr
  *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][Start]
  *!*	  REPLACE STATUS     WITH IIF(Ship > 0,'C','X') ,;
  *!*	    cCancReson WITH lcCanReason ,;
  *!*	    Cancelled  WITH oAriaApplication.SystemDate  ,;
  *!*	    CANCEL     WITH IIF(Bulk='Y',CANCEL ,CANCEL+lnOpen),;
  *!*	    Cancelamt  WITH IIF(Bulk='Y',Cancelamt ,Cancelamt+lnOpenAmt),;
  *!*	    OPEN       WITH 0,;
  *!*	    OpenAmt    WITH 0,;
  *!*	    TotCut     WITH 0,;
  *!*	    Book       WITH IIF(Bulk='Y',Ship + CANCEL,lnBook)    ,;
  *!*	    BookAmt    WITH IIF(Bulk='Y',ShipAmt + CanlnBookAmtcelamt,lnBookAmt) ,;
  *!*	    FLAG       WITH SPACE(1)
  IF !(Bulk='Y' AND lnBook <= 0)
    REPLACE STATUS     WITH IIF(Ship > 0,'C','X') ,;
      cCancReson WITH lcCanReason ,;
      Cancelled  WITH oAriaApplication.SystemDate  ,;
      CANCEL     WITH IIF(Bulk='Y',CANCEL ,CANCEL+lnOpen),;
      Cancelamt  WITH IIF(Bulk='Y',Cancelamt ,Cancelamt+lnOpenAmt),;
      OPEN       WITH 0,;
      OpenAmt    WITH 0,;
      TotCut     WITH 0,;
      Book       WITH IIF(Bulk='Y',Ship + CANCEL,lnBook)    ,;
      BookAmt    WITH IIF(Bulk='Y',ShipAmt + Cancelamt,lnBookAmt) ,;
      FLAG       WITH SPACE(1)
  ELSE
    = gfModalGen('QRM54040B00000','DIALOG',ordhdr.ORDER)
  ENDIF
  *! B610206,1 HIA 01/21/2013 WHEN CANCELLING AN EDI CONFIRMATION ORDER WE FIND THAT THE BULK ORDER FOR THAT EDI CONFIRMATION WAS REVERSED [T20120711.0017][End]
ENDIF


*! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
lcOldAlias = ALIAS()
SELECT ICSTYHST
=gfTableUpdate()
SELECT(lcOldAlias)
*! E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]

*! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[Start]
IF OrdHdr.cOrdType='O'
  lcOldAlias = SELECT()
  SELECT STYHIST
  =gfTableUpdate()
  SELECT(lcOldAlias)
ENDIF
*! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[End]

*! E302650,1 MMT 12/02/2009 Update Project Status When order or one of its lines is cancelled[Start]
IF OrdHdr.STATUS = 'X'
  lfUpdPrjSt(OrdHdr.cOrdType,OrdHdr.ORDER)
ENDIF
*! E302650,1 MMT 12/02/2009 Update Project Status When order or one of its lines is cancelled[End]

*-- Update the CRMesag file.
DO lpUpdMesag
*-- Update version one with cancelled orders.
IF ASCAN(oAriaApplication.laEvntTrig , PADR('EXPCTOLD',10)) <> 0
  =gfDoTriger('SOORD',PADR('EXPCTOLD',10))
ENDIF

*B128066,1 HBG Fix bug of file is used by another user [Start]
SET REPROCESS TO lnRePro
*B128066,1 HBG [End]

RETURN

*!*************************************************************
*! Name      : lfUpdtPoCT
*! Developer : WAB - WALID A. WAHAB
*! Date      : 09/26/1999
*! Purpose   : update selling price in po/ct lines
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcOrder	---> sales order no
*!             LcStyle  ---> Style COde
*!             lcLineNo ---> sales order line no
*!             lnTotQty ---> new qty
*!             lnPrice  ---> new price
*!             lnOldPrc ---> old price
*!             lnOldQty ---> old qty
*!			   llRecDel ---> record is deleted (remove line)
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfUpdtPoCT()
*!*************************************************************
FUNCTION lfUpdtPoCT
PARAMETER  lcOrder,lcSTYLE,lcLineNo,lnQty,lnPrice,lnOldPrc,lnOldQty,llRecDel
*! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
*!*	=gfOpenFile(oAriaApplication.DataDir+'CUTPICK',oAriaApplication.DataDir+'CUTORD','SH')
*!*	=gfOpenFile(oAriaApplication.DataDir+'POSLN',oAriaApplication.DataDir+'POSLN','SH')
*!*	=gfOpenFile(oAriaApplication.DataDir+'CUTTKTL',oAriaApplication.DataDir+'CUTTKTLS','SH')
*SELECT CUTPICK
*IF SEEK('2'+lcOrder+lcLineNo)
=gfOpenTable('CUTPICK','CUTORD','SH','CUTPICKUP')
=gfOpenTable('POSLN','POSLN','SH','POSLNUP')
SELECT CUTPICKUP
IF gfSEEK('2'+lcOrder+lcLineNo)
  *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]
  *-- there are a po generated from curr. sales order
  SCAN REST WHILE TRANCD+ORDER+CORDLINE = '2'+lcOrder+lcLineNo
    lnGenQty   = TotQty					&& generated qty
    lnGenPerc  = TotQty/lnOldQty  		&& genereted percent
    lnCtktNo   = CTKTNO					&& PO number
    lnTotQty   = INT(lnQty*lnGenPerc)   && new generated qty
    *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
    *IF SEEK('P'+lnCtktNo+lcStyle,'POSLN')
    IF gfSEEK('PP'+lnCtktNo+'0001' +lcSTYLE+PADR(CUTPICKUP.CTKTLINENO,6)+'1','POSLNUP')
      *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]
      *-- new selling price = --> curr selling price * total qty
      *--                     --> - oldprice * old qty generated
      *--					  --> + newprice * new generated qty
      *-- 					  --> / total qty - old qty generated + new generated qty
      IF llRecDel
        *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
        *!*	        lnNewPrc = ((POSLN.nSelPrice*POSLN.TotQty)-(lnOldPrc*lnGenQty));
        *!*	                   /(POSLN.TotQty-lngenqty)
        lnNewPrc = ((POSLNUP.nSelPrice*POSLNUP.TotQty)-(lnOldPrc*lnGenQty));
          /(POSLNUP.TotQty-lnGenQty)
        *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]
      ELSE
        *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
        *!*	        lnNewPrc = ((POSLN.nSelPrice*POSLN.TotQty)-(lnOldPrc*lnGenQty)+;
        *!*	                    (lnPrice*lnTotQty)) / (POSLN.TotQty-lngenqty+lnTotQty)
        lnNewPrc = ((POSLNUP.nSelPrice*POSLNUP.TotQty)-(lnOldPrc*lnGenQty)+;
          (lnPrice*lnTotQty)) / (POSLNUP.TotQty-lnGenQty+lnTotQty)
        *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]
      ENDIF
      lnNewPrc = IIF(BETWEEN(lnNewPrc,-999999999.99,999999999.99),lnNewPrc,0)
      =SEEK(STYLE,'STYLE')
      *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
      *lnTotCost= nEcost1+nEcost2+nEcost3+nEcost4+nEcost5
      lnTotCost= POSLNUP.nicost1+POSLNUP.nicost2+POSLNUP.nicost3+POSLNUP.nicost4+POSLNUP.nicost5+POSLNUP.nicost6+POSLNUP.nicost7
      *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]
      lnRotSub   = IIF(llStyMark,lnTotCost,lnNewPrc)
      lnGrosMrgn = IIF(lnRotSub=0,0,((lnNewPrc - lnTotCost)/lnRotSub)*100)
      *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
      *!*	            REPLACE POSLN.nSelPrice  WITH lnNewPrc ,;
      *!*	              POSLN.nGrosMrgn  WITH lnGrosMrgn
      REPLACE POSLNUP.nSelPrice  WITH lnNewPrc ,;
        POSLNUP.nGrosMrgn  WITH lnGrosMrgn

      SELECT POSLNUP
      =gfReplace('')
      *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]
    ENDIF
  ENDSCAN
ENDIF
*! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
*SELECT CUTPICK
*IF SEEK('1'+lcOrder+lcLineNo)
SELECT CUTPICKUP
IF gfSEEK('1'+lcOrder+lcLineNo)
  *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]

  *-- there are a CT generated from curr. sales order
  SCAN REST WHILE TRANCD+ORDER+CORDLINE = '1'+lcOrder+lcLineNo
    lnGenQty   = TotQty					&& generated qty
    lnGenPerc  = TotQty/lnOldQty  		&& genereted percentg
    lnCtktNo   = CTKTNO					&& PO number
    lnTotQty   = INT(lnQty*lnGenPerc)   && new generated qty
    *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
    *IF SEEK(lcStyle+lnCtktNo+'1','CUTTKTL')
    IF gfSEEK('PU'+lnCtktNo+'0001' +lcSTYLE+PADR(CUTPICKUP.CTKTLINENO,6)+'1','POSLNUP')
      *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]
      *-- new selling price = --> curr selling price * total qty
      *--                     --> - oldprice * old qty generated
      *--					  --> + newprice * new generated qty
      *-- 					  --> / total qty - old qty generated + new generated qty
      IF llRecDel
        *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
        *!*	        lnNewPrc = ((CUTTKTL.nSelPrice*CUTTKTL.TotQty)-(lnOldPrc*lnGenQty));
        *!*	                   /(CUTTKTL.TotQty-lngenqty)
        lnNewPrc = ((POSLNUP.nSelPrice*POSLNUP.TotQty)-(lnOldPrc*lnGenQty));
          /(POSLNUP.TotQty-lnGenQty)
        *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]
      ELSE
        *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
        *!*	        lnNewPrc = ((CUTTKTL.nSelPrice*CUTTKTL.TotQty)-(lnOldPrc*lnGenQty)+;
        *!*	                   (lnPrice*lnTotQty)) / (CUTTKTL.TotQty-lngenqty+lnTotQty)
        lnNewPrc = ((POSLNUP.nSelPrice*POSLNUP.TotQty)-(lnOldPrc*lnGenQty)+;
          (lnPrice*lnTotQty)) / (POSLNUP.TotQty-lnGenQty+lnTotQty)
        *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]
      ENDIF
      lnNewPrc = IIF(BETWEEN(lnNewPrc,-999999999.99,999999999.99),lnNewPrc,0)
      =SEEK(STYLE,'STYLE')
      lnRotSub   = IIF(llStyMark,STYLE.TotCost,lnNewPrc)
      lnGrosMrgn = IIF(lnRotSub=0,0,((lnNewPrc - STYLE.TotCost)/lnRotSub)*100)
      *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
      *!*	      REPLACE CUTTKTL.nSelPrice  WITH lnNewPrc ,;
      *!*	              CUTTKTL.nGrosMrgn  WITH lnGrosMrgn
      REPLACE POSLNUP.nSelPrice  WITH lnNewPrc ,;
        POSLNUP.nGrosMrgn  WITH lnGrosMrgn
      SELECT POSLNUP
      =gfReplace('')
      *! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]
    ENDIF
  ENDSCAN
ENDIF
*! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[Start]
SELECT POSLNUP
=gfTableUpdate()
=gfCloseTable('POSLNUP')
=gfCloseTable('CUTPICKUP')
*! B609551,1 MMT 03/16/2011 Sales order Cut1-Cut8 quantities not zeroising when allocation is released[End]
*!**************************************************************************
*! Name      : lpUpdMesag
*! Developer : Wael Ali Mohamed
*! Date      : 08/01/2002
*! Purpose   : Update the Message file in CRM Module.
*!**************************************************************************
*! Reference :
*!**************************************************************************
*! Example   : DO lpUpdMesag
*!**************************************************************************
*
PROCEDURE lpUpdMesag
IF OrdHdr.lFromWeb AND 'CR' $ oAriaApplication.CompanyInstalledModules
  LOCAL lcAlias
  lcAlias = ALIAS()
  =gfOpenFile(oAriaApplication.DataDir+'CRMesag',oAriaApplication.DataDir+'TransType','SH')
  SET ORDER TO TAG TransType DESCENDING
  SELECT CRMesag
  IF !SEEK('O' + OrdHdr.ORDER,'CRMesag') OR lSent
    APPEND BLANK
  ENDIF
  REPLACE cTransType WITH 'O' ,;
    cTransNo   WITH OrdHdr.ORDER,;
    cMailFrom  WITH gfGetMemVar('M_NOTEMAIL',oAriaApplication.ActiveCompanyID) ,;
    dTransDate WITH oAriaApplication.SystemDate ,;
    cMailTo    WITH IIF(SEEK('M'+OrdHdr.Account,'Customer'),EVALUATE('Customer.' + gfGetMemVar('M_CONFMAIL',oAriaApplication.ActiveCompanyID)),'')
  IF OrdHdr.STATUS = "X"
    REPLACE lApprove   WITH .F. ,;
      cMsgSubjct WITH "Your Sales Order# " + OrdHdr.ORDER + ' has been cancelled.' ,;
      cLetterId  WITH gfGetMemVar('M_SOCAN',oAriaApplication.ActiveCompanyID)
  ELSE
    REPLACE lApprove   WITH .T. ,;
      cMsgSubjct WITH "Your Sales Order# " + OrdHdr.ORDER + ' has been approved.' ,;
      cLetterId  WITH gfGetMemVar('M_SOAPR',oAriaApplication.ActiveCompanyID)
  ENDIF
  =gfAdd_Info('CRMesag')
  SELECT (lcAlias)
ENDIF
*-- End of lpUpdMesag.

*!*************************************************************
*! Name      : lfChkStores
*! Developer : Wael Ali Mohamed
*! Date      : 08/01/2002
*! Purpose   : Check if Open or Hold order is ship to potintial customer or stores.
*!             If so, it either change order status to bid or change custer/stores to active
*!*************************************************************
*! Calls     : gfModalGen, gfObj_Lock
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfChkStores()
*!*************************************************************
FUNCTION lfChkStores
LOCAL laStores, lnChoice
*HBG
*!*	IF OrdHdr.Status <> "B" AND OrdHdr.Multi = "Y"
*!*	  DECLARE laStores[1]
*!*	  *-- Check if order is ship to any potintial stores
*!*	  SELECT DISTINCT &lcOrdLine..Store FROM (lcOrdLine),Customer ;
*!*	  WHERE Customer.Type+Customer.Account+Customer.Store="S"+&lcOrdLine..Account+&lcOrdLine..Store AND Customer.Status="P" ;
*!*	  INTO ARRAY laStores
*!*	  IF _TALLY = 0
*!*	    RETURN
*!*	  ENDIF
*!*	  *-- Message : 32086
*!*	  *-- Text    : You are about to save the sales order as Open while some stores are Potintial.
*!*	  *--           If you keep the order as is, the potintial stores will be changed to Active.
*!*	  *--           Otherwise, Keep the stores as Potential and change the Order to Bid.
*!*	  *-- Button  : 32009
*!*	  *--           <Keep Order>  <Keep Stores>

*!*	  IF gfModalGen("QRM32086B32009","DIALOG",IIF(OrdHdr.Status="O","Open","Hold"))  =1
*!*	    *-- Keep order status and change stores status to active
*!*	    FOR lnCount = 1 TO ALEN(laStores)
*!*	      =SEEK('S'+OrdHdr.Account+laStores[lnCount],'Customer')
*!*	      SELECT Customer
*!*	      IF gfObj_Lock(.T.)
*!*	        REPLACE Status WITH "A"
*!*	        =gfObj_Lock(.F.)
*!*	      ELSE
*!*	        *-- Store is in use by another user, Keep stores as potintial and
*!*	        *-- change order status to BId
*!*	        =gfModalGen("INM32087B00000","DIALOG",'store')
*!*	        REPLACE Status WITH "B" IN OrdHdr
*!*	        RETURN
*!*	      ENDIF
*!*	    ENDFOR
*!*	  ELSE
*!*	    *-- Keep stores as potintial and change order status to Bid
*!*	    REPLACE Status WITH "B" IN OrdHDr
*!*	  ENDIF
*!*	ENDIF
SELECT(lcOrdLine)
GOTO TOP
lnMessage = 0
SCAN
  IF !EMPTY(STORE) .AND. SEEK('S'+Account+STORE,'Customer') ;
      .AND. Customer.STATUS = 'P'
    IF OrdHdr.STATUS <> 'B'
      lcMessage = IIF(OrdHdr.STATUS="O","Open","Hold")+LANG_Message1+IIF(OrdHdr.STATUS="O","Open","Hold")+LANG_Message2
      IF lnMessage = 0
        lnMessage = gfModalGen("QRM32086B32009","DIALOG",lcMessage)
      ENDIF
      IF lnMessage = 1
        *-- Keep order status and change customer status to active
        SELECT Customer
        IF gfObj_Lock(.T.)
          REPLACE Customer.STATUS WITH "A"
          =gfObj_Lock(.F.)
        ELSE
          *-- Customer is in use by  another user, Keep customer as potintial and
          *-- change order status to BId
          =gfModalGen("INM32087B00000","DIALOG",'account')
          REPLACE STATUS WITH 'B' IN OrdHdr
        ENDIF
      ELSE
        *-- Keep customer as potintial and change order status to Bid
        REPLACE STATUS WITH 'B' IN OrdHdr
      ENDIF
    ENDIF
  ENDIF
ENDSCAN
*HBG

*!*************************************************************
*! Name      : lfCanReason
*! Developer : Wael Ali Mohamed
*! Date      : 08/01/2002
*! Purpose   : Get cancel reason
*!*************************************************************
*! Calls     : Form ARORDER
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCanReason()
*!*************************************************************
FUNCTION lfCanReason
lcCanReason = ''
DO FORM (oAriaApplication.ScreenHome+"ARORDER") WITH 'X' TO lcCanReason
RETURN lcCanReason

*!*************************************************************
*! Name      : lfGtOrder
*! Developer : Wael Ali Mohamed
*! Date      : 08/01/2002
*! Purpose   : Get Order number manullay
*!*************************************************************
*! Calls     : Form ARORDER
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfGtOrder()
*!*************************************************************
FUNCTION lfGtOrder
LOCAL lcOrderNo
lcOrderNo = ''

*B608398,1 WAM 12/31/2007 Get sales order type from the order header temporary file
*DO FORM (oAriaApplication.ScreenHome+"ARORDER") WITH 'O',OrdHdr.cOrdType TO lcOrderNo
DO FORM (oAriaApplication.ScreenHome+"ARORDER") WITH 'O',&lcOrdHDr..cOrdType TO lcOrderNo
*B608398,1 WAM 12/31/2007 (End)
RETURN lcOrderNo

*E302350,1 MMT 01/11/2007 Enhancement to add Deposit Screen[Start]
*!*************************************************************
*! Name          : lfUpdateCredit
*: Developer     : Mariam Mazhar- [MMT]
*: Date          : 09/26/2006.
*! Purpose       : Function to update Credit file
*!*************************************************************
FUNCTION lfUpdateCredit

lnOldAlias =SELECT(0)
lcDeleSt = SET("Deleted")
*B609945,1 HIA 28/05/2012 Issues with deposits - Set deleted missing [T20120427.0001][Begin]
SET DELETED OFF
*B609945,1 HIA 28/05/2012 Issues with deposits - Set deleted missing [T20120427.0001][End]
IF USED(loFormSet.lcdeposittemp)
  SELECT (loFormSet.lcdeposittemp)
  LOCATE
  IF !EOF()
    SCAN FOR !EMPTY(TRAN) AND Amount <> 0
      IF gfSEEK(TRAN+OrdHdr.ORDER,'DEPOSITS','DEPOSITS') AND !DELETED()
        IF DEPOSITS.Amount <> EVALUATE(loFormSet.lcdeposittemp+'.Amount')
          SELECT DEPOSITS
          gfReplace ([Amount with EVALUATE(loFormSet.lcdeposittemp+'.Amount')])
          =gfAdd_Info('DEPOSITS')
        ENDIF
      ELSE
        IF DELETED() AND gfSEEK(TRAN+OrdHdr.ORDER,'DEPOSITS','DEPOSITS')
          SELECT DEPOSITS
          gfDelete()
        ELSE
          SELECT (loFormSet.lcdeposittemp)
          SCATTER MEMO MEMVAR
          SELECT DEPOSITS
          APPEND BLANK
          REPLACE Account   WITH m.Account,;
            ORDER     WITH OrdHdr.ORDER,;
            Amount    WITH m.Amount,;
            BATCH     WITH m.batch ,;
            cchkno    WITH m.Store,;
            TRAN      WITH m.tran,;
            lpaidflag WITH .F.
          =gfAdd_Info('DEPOSITS')
          gfReplace()
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  =gfTableUpdate(.T.,'DEPOSITS')
ENDIF
IF loFormSet.Activemode = 'A' AND USED(loFormSet.lcdeposittemp)
  SELECT(loFormSet.lcdeposittemp)
  ZAP
ENDIF
SELECT(lnOldAlias)
SET DELETED &lcDeleSt
*E302350,1 MMT 01/11/2007 Enhancement to add Deposit Screen[End]
*! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[Start]
*!*************************************************************
*! Name      : lfGetClrD
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/28/2009
*! Purpose   : To get color position and  color length
*!*************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
PRIVATE lnCount
lcOldSelect=SELECT()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])
  ENDCASE
ENDFOR
SELECT(lcOldSelect)
*--end function lfGetClrD
*!*************************************************************
*! Name      : lfUpStyHist
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/28/2009
*! Purpose   : Update StyHist Table
*!*************************************************************
FUNCTION lfUpStyHist
*B609371,1 SMA 08/08/2010 Update Styhist Table when Sales Order is cancelled from mass so cancellation screen....[BEGIN]
*PARAMETERS llFromCanc
PARAMETERS llFromCanc ,llFromMass && Check if this function called from so-mass so cancellation screen
*B609371,1 SMA 08/08/2010 Update Styhist Table when Sales Order is cancelled from mass so cancellation screen....[END]
lcOldAlias = SELECT()
lcOrdHdrFile  = IIF(llFromCanc ,'ORDHDR',lcOrdHDr)
lcOrdLineFile = IIF(llFromCanc ,'ORDLINE',lcOrdLine)
IF &lcOrdHdrFile..cOrdType = 'O' OR (llFromEDI OR (&lcOrdHdrFile..lFromWeb AND &lcOrdHdrFile..STATUS $ 'HO' AND loFormSet.lcOldStatus = 'B'))
  *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [Start]
  *=gfSEEK('C'+PADR(&lcOrdHdrFile..Account,8)+&lcOrdHdrFile..Store,'Contact_A')
  =gfSEEK('C'+PADR(&lcOrdHdrFile..Account,8)+IIF(&lcOrdHdrFile..MULTI="Y",&lcOrdLineFile..STORE,&lcOrdHdrFile..STORE),'Contact_A')
  *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [End]
  SELECT Contact_A
  *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [Start]
  *!*    LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = 'C'+PADR(&lcOrdHdrFile..Account,8)+&lcOrdHdrFile..Store FOR;
  *!*              CCNTCTCODE = ALLTRIM(&lcOrdLineFile..Employee)
  *! B609717,1 MMT 10/27/2011 CRM Saving SO Hangs because of deleted rec. in Contacts table[Start]
  *!*      LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = 'C'+PADR(&lcOrdHdrFile..Account,8)+IIF(&lcOrdHdrFile..MULTI="Y",&lcOrdLineFile..STORE,&lcOrdHdrFile..STORE) FOR;
  *!*        CCNTCTCODE = ALLTRIM(&lcOrdLineFile..Employee)
  LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = 'C'+PADR(&lcOrdHdrFile..Account,8)+IIF(&lcOrdHdrFile..MULTI="Y",&lcOrdLineFile..STORE,&lcOrdHdrFile..STORE) FOR;
    CCNTCTCODE = ALLTRIM(&lcOrdLineFile..Employee) AND !DELETED()
  *! B609717,1 MMT 10/27/2011 CRM Saving SO Hangs because of deleted rec. in Contacts table[END]
  *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [End]
  IF FOUND()
    lcSty = PADR(SUBSTR(&lcOrdLineFile..STYLE,1,lnMajorLen),19)
    lcClr = PADR(SUBSTR(&lcOrdLineFile..STYLE,lnClrPos,lnClrLen),6)
    lnUpdFctr = 1
    lnEntlmnt = 0
    IF !EMPTY(Contact_A.UCODE)
      =gfSEEK(Contact_A.UCODE+lcSty+lcClr,'UNIFORM')
      lnEntlmnt = UNIFORM.ENTITLEMNT

      DO CASE
      CASE UNIFORM.TYPE = 'V'
        lnUpdFctr = &lcOrdLineFile..Price
      CASE UNIFORM.TYPE = 'P'
        lnUpdFctr = UNIFORM.PNTSVAL
      ENDCASE
    ENDIF

    SELECT STYHIST

    llUpdCurs = .F.
    lnTableStyHst = gfGetRemoteTable(SET("Datasession"),'STYHIST')
    lcStyHstTable = ''
    IF lnTableStyHst<>0 && Remote Table Object was Found
      lcStyHstTable = oAriaApplication.laRemoteTable[lnTableStyHst].lcCursorUpdate
    ENDIF
    *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [Start]
    *!*      IF (lnTableStyHst <> 0 AND !SEEK(&lcOrdHdrFile..Account+&lcOrdHdrFile..Store+&lcOrdLineFile..EMPLOYEE+lcSty+lcClr,lcStyHstTable)) AND ;
    *!*         !gfSEEK(&lcOrdHdrFile..Account+&lcOrdHdrFile..Store+&lcOrdLineFile..EMPLOYEE+lcSty+lcClr ,'STYHIST')
    IF (lnTableStyHst <> 0 AND !SEEK(&lcOrdHdrFile..Account+IIF(&lcOrdHdrFile..MULTI="Y",&lcOrdLineFile..STORE,&lcOrdHdrFile..STORE)+&lcOrdLineFile..Employee+lcSty+lcClr,lcStyHstTable)) AND ;
        !gfSEEK(&lcOrdHdrFile..Account+IIF(&lcOrdHdrFile..MULTI="Y",&lcOrdLineFile..STORE,&lcOrdHdrFile..STORE)+&lcOrdLineFile..Employee+lcSty+lcClr ,'STYHIST')
      *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [End]
      SELECT STYHIST
      APPEND BLANK
      *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [Start]
      *!*        REPLACE ACCOUNT    WITH &lcOrdHdrFile..Account ;
      *!*                STORE      WITH &lcOrdHdrFile..Store ;
      *!*                EMPLOYEE   WITH &lcOrdLineFile..EMPLOYEE ;
      *!*                CSTYMAJOR  WITH lcSty ;
      *!*                COLOR      WITH lcClr ;
      *!*                ENTITLEMNT WITH lnEntlmnt
      REPLACE Account    WITH &lcOrdHdrFile..Account ;
        STORE      WITH IIF(&lcOrdHdrFile..MULTI="Y",&lcOrdLineFile..STORE,&lcOrdHdrFile..STORE) ;
        Employee   WITH &lcOrdLineFile..Employee ;
        CSTYMAJOR  WITH lcSty ;
        COLOR      WITH lcClr ;
        ENTITLEMNT WITH lnEntlmnt
      *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [End]

      *! B609006,2 MMT 11/18/2009 Fix bug of not updating StyHIST.Nused AND dend Date[Start]
      IF !EMPTY(Contact_A.UCODE)
        REPLACE DEND WITH  GOMONTH(Contact_A.DSTART ,UNIFORM.NPERIOD)-1
      ENDIF
      *! B609006,2 MMT 11/18/2009 Fix bug of not updating StyHIST.Nused AND dend Date[End]

    ELSE
      *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [Start]
      *IF lnTableStyHst <> 0 AND SEEK(&lcOrdHdrFile..Account+&lcOrdHdrFile..Store+&lcOrdLineFile..EMPLOYEE+lcSty+lcClr,lcStyHstTable)
      IF lnTableStyHst <> 0 AND SEEK(&lcOrdHdrFile..Account+IIF(&lcOrdHdrFile..MULTI="Y",&lcOrdLineFile..STORE,&lcOrdHdrFile..STORE)+&lcOrdLineFile..Employee+lcSty+lcClr,lcStyHstTable)
        *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [End]
        SELECT (lcStyHstTable)
        llUpdCurs = .T.
      ELSE
        SELECT STYHIST
      ENDIF
    ENDIF

    *! B609408,1 MMT 09/14/2010 Update Dend in Styhist while Saving the Sales order[Start]
    IF !EMPTY(Contact_A.UCODE) ;
        AND ((&lcOrdHdrFile..lFromWeb AND &lcOrdHdrFile..STATUS = 'O') OR (llFromEDI  AND &lcOrdHdrFile..STATUS $ 'HO' AND loFormSet.lcOldStatus = 'B'))
      lnAliasBef = SELECT()
      IF !EMPTY(DEND) AND (DEND < oAriaApplication.SystemDate)
        *! B609717,1 MMT 10/27/2011 CRM Saving SO Hangs because of deleted rec. in Contacts table[Start]
        IF UNIFORM.NPERIOD > 0
          *! B609717,1 MMT 10/27/2011 CRM Saving SO Hangs because of deleted rec. in Contacts table[END]
          DO WHILE (DEND < oAriaApplication.SystemDate)
            REPLACE DEND  WITH GOMONTH(DEND,UNIFORM.NPERIOD)-1
          ENDDO
          *! B609717,1 MMT 10/27/2011 CRM Saving SO Hangs because of deleted rec. in Contacts table[Start]
        ENDIF
        *! B609717,1 MMT 10/27/2011 CRM Saving SO Hangs because of deleted rec. in Contacts table[END]
        REPLACE nUSED WITH  0
        ldEndDate = DEND
        lnUniLn = UNIFORM.nLine
        SELECT UNIFORM
        lcCurrKey = EVALUATE(KEY())
        =gfSEEK(Contact_A.UCODE,'UNIFORM')
        SCAN REST WHILE UCODE+CSTYMAJOR+COLOR = Contact_A.UCODE FOR nlink1 <> 0 AND  IIF(nlink2 = 0,nlink1 =lnUniLn,BETWEEN(lnUniLn,nlink1,nlink2))
          IF gfSEEK(&lcOrdHdrFile..Account+IIF(&lcOrdHdrFile..MULTI="Y",&lcOrdLineFile..STORE,&lcOrdHdrFile..STORE)+&lcOrdLineFile..Employee+UNIFORM.CSTYMAJOR+UNIFORM.COLOR,'STYHIST_OS')
            SELECT 'STYHIST_OS'
            REPLACE DEND WITH ldEndDate ,;
              nUSED WITH 0
            =gfAdd_Info('STYHIST_OS')
            =gfReplace('')
          ENDIF
        ENDSCAN
        SELECT UNIFORM
        =gfSEEK(lcCurrKey)
      ENDIF
      SELECT(lnAliasBef)
    ENDIF
    *! B609408,1 MMT 09/14/2010 Update Dend in Styhist while Saving the Sales order[End]


    lnOrgTotQty = 0
    IF !llFromCanc AND lcActiveMode="E" AND &lcOrdHdrFile..cOrdType = 'O'
      IF SEEK(&lcOrdLineFile..cOrdType+&lcOrdLineFile..ORDER+STR(&lcOrdLineFile..LINENO,6),'ORDLINE')
        lnOrgTotQty = ORDLINE.TotQty
      ENDIF
    ENDIF
    DO CASE
    CASE llFromCanc AND &lcOrdHdrFile..STATUS = 'X'
      REPLACE nUSED    WITH nUSED + (&lcOrdLineFile..TotQty)*lnUpdFctr

    CASE (DELETED(lcOrdLineFile) AND;
        SEEK(&lcOrdLineFile..cOrdType+&lcOrdLineFile..ORDER+STR(&lcOrdLineFile..LINENO,6),'ORDLINE')) OR llFromCanc
      *B609371,1 SMA 08/08/2010 Update Styhist Table when Sales Order is cancelled from mass so cancellation screen....[BEGIN]
      IF llFromMass
        REPLACE nUSED    WITH nUSED + (ORDLINE.TOTPIK - ORDLINE.TotQty)*lnUpdFctr
      ELSE
        *B609371,1 SMA 08/08/2010 Update Styhist Table when Sales Order is cancelled from mass so cancellation screen....[END]
        REPLACE nUSED    WITH nUSED + (0 - ORDLINE.TotQty)*lnUpdFctr
        *B609371,1 SMA 08/08/2010 Update Styhist Table when Sales Order is cancelled from mass so cancellation screen....[BEGIN]
      ENDIF
      *B609371,1 SMA 08/08/2010 Update Styhist Table when Sales Order is cancelled from mass so cancellation screen....[END]
    OTHERWISE
      REPLACE nUSED    WITH nUSED + (&lcOrdLineFile..TotQty - lnOrgTotQty)*lnUpdFctr
    ENDCASE

    IF !llFromCanc AND (llFromEDI OR (&lcOrdHdrFile..lFromWeb AND &lcOrdHdrFile..STATUS $ 'HO' AND loFormSet.lcOldStatus = 'B'))
      REPLACE DLASTORD WITH &lcOrdHdrFile..Entered ;
        ORDER    WITH lcOrderNo
    ENDIF
    IF llUpdCurs
      gfAdd_Info(lcStyHstTable)
    ELSE
      =gfAdd_Info('STYHIST')
      =gfReplace('')
    ENDIF

    *:***************************************************************************
    IF !EMPTY(Contact_A.UCODE)
      =gfSEEK(Contact_A.UCODE,'UNIFORM')
      SELECT UNIFORM
      SCAN REST WHILE UCODE+CSTYMAJOR+COLOR = Contact_A.UCODE
        *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [Start]
        *IF !gfSEEK(&lcOrdHdrFile..Account+&lcOrdHdrFile..Store+&lcOrdLineFile..EMPLOYEE+CSTYMAJOR+COLOR ,'STYHIST')
        *! B609006,2 MMT 11/18/2009 Fix bug of not updating StyHIST.Nused AND dend Date[Start]
        *IF !gfSEEK(&lcOrdHdrFile..Account+IIF(&lcOrdHdrFile..Multi="Y",&lcOrdLineFile..Store,&lcOrdHdrFile..Store)+&lcOrdLineFile..EMPLOYEE+CSTYMAJOR+COLOR ,'STYHIST')
        IF (lnTableStyHst <> 0 AND !SEEK(&lcOrdHdrFile..Account+IIF(&lcOrdHdrFile..MULTI="Y",&lcOrdLineFile..STORE,&lcOrdHdrFile..STORE)+&lcOrdLineFile..Employee+CSTYMAJOR+COLOR,lcStyHstTable)) AND ;
            !gfSEEK(&lcOrdHdrFile..Account+IIF(&lcOrdHdrFile..MULTI="Y",&lcOrdLineFile..STORE,&lcOrdHdrFile..STORE)+&lcOrdLineFile..Employee+CSTYMAJOR+COLOR ,'STYHIST')
          *! B609006,2 MMT 11/18/2009 Fix bug of not updating StyHIST.Nused AND dend Date[End]
          *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [End]
          SELECT STYHIST
          APPEND BLANK
          *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [Start]
          *!*            REPLACE ACCOUNT    WITH &lcOrdHdrFile..Account         ;
          *!*                    STORE      WITH &lcOrdHdrFile..Store      ;
          *!*                    EMPLOYEE   WITH &lcOrdLineFile..EMPLOYEE ;
          *!*                    CSTYMAJOR  WITH UNIFORM.CSTYMAJOR    ;
          *!*                    COLOR      WITH UNIFORM.COLOR       ;
          *!*                    ENTITLEMNT WITH UNIFORM.ENTITLEMNT
          REPLACE Account    WITH &lcOrdHdrFile..Account         ;
            STORE      WITH IIF(&lcOrdHdrFile..MULTI="Y",&lcOrdLineFile..STORE,&lcOrdHdrFile..STORE)     ;
            Employee   WITH &lcOrdLineFile..Employee ;
            CSTYMAJOR  WITH UNIFORM.CSTYMAJOR    ;
            COLOR      WITH UNIFORM.COLOR       ;
            ENTITLEMNT WITH UNIFORM.ENTITLEMNT
          *! B609006,2 MMT 11/18/2009 Fix bug of not updating StyHIST.Nused AND dend Date[Start]
          *REPLACE DEND WITH  Contact_A.DSTART +UNIFORM.NPERIOD-1
          REPLACE DEND WITH  GOMONTH(Contact_A.DSTART ,UNIFORM.NPERIOD)-1

          *! B609408,1 MMT 09/14/2010 Update Dend in Styhist while Saving the Sales order[Start]
          DO WHILE (DEND < oAriaApplication.SystemDate)
            REPLACE DEND  WITH GOMONTH(DEND,UNIFORM.NPERIOD)-1
          ENDDO
          *! B609408,1 MMT 09/14/2010 Update Dend in Styhist while Saving the Sales order[End]

          *! B609006,2 MMT 11/18/2009 Fix bug of not updating StyHIST.Nused AND dend Date[End]

          *! B609006,1 MMT 09/13/2009 Fix bug of not updating StyHIST Table        [End]
          =gfAdd_Info('STYHIST')
          =gfReplace('')
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
ENDIF
SELECT (lcOldAlias)
*! B608948,1 MMT 07/28/2009 Update Styhist Table when Sales Order is Modified[End]

*! E302650,1 MMT 12/02/2009 Update Project Status When order or one of its lines is cancelled[Start]
*!*************************************************************
*! Name      : lfUpdPrjSt
*! Developer : Mariam Mazhar[MMT]
*! Date      : 12/02/2009
*! Purpose   : Update Project Status
*!*************************************************************
FUNCTION lfUpdPrjSt
LPARAMETERS lcOrdType,lcOrder,lnLineNo
PRIVATE lnPrvAlias
lnPrvAlias = SELECT(0)
IF !USED('PMPRJHD')
  =gfOpenTable('PMPRJHD'  , 'PMPRJHD' , 'SH')
ENDIF

IF !USED('PMPRJDT')
  =gfOpenTable('PMPRJDT'  , 'PMPRJDT' , 'SH')
ENDIF

IF !USED('SYSCHDUL')
  =gfOpenTable('SYSCHDUL' , 'Coprusr' , 'SH')
ENDIF

IF !USED('AUDTRAIL')
  =gfOpenTable('AUDTRAIL' , 'AUDTRAIL', 'SH')
ENDIF

*E302741,1 WAM 08/25/2010 Convert Audit Trail file to SQL
IF !USED("PMCTGHD")
  =gfOpenTable('PMCTGHD','PMCTGHD','SH')
ENDIF
*E302741,1 WAM 08/25/2010 (End)

IF gfSEEK(lcOrdType+lcOrder,'PMPRJHD')
  SELECT PMPRJHD
  SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6) = lcOrdType+lcOrder FOR !(PMPRJHD.cprj_stts $ 'CH') AND ;
      IIF(TYPE('lnLineNo') = 'N',LINENO = lnLineNo,.T.)
    gfReplace("cprj_stts WITH 'X'")
    lcProg  = 'SOORD'
    lcKey   =lcOrdType+ lcOrder+ SUBSTR(PMPRJHD.CSTYLE,1,LEN(CSTYLE))+STR(PMPRJHD.LINENO,6)
    SELECT PMPRJDT
    IF gfSEEK(SUBSTR(PMPRJHD.CPRJ_TYP,1,LEN(CPRJ_TYP)) + SUBSTR(PMPRJHD.CPRJ_ID,1,LEN(CPRJ_ID)) +;
        SUBSTR(PMPRJHD.CSTYLE,1,LEN(CSTYLE))+STR(PMPRJHD.LINENO,6))
      SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6) +coprt_ctg+coprt_id =;
          SUBSTR(PMPRJHD.CPRJ_TYP,1,LEN(CPRJ_TYP)) + SUBSTR(PMPRJHD.CPRJ_ID,1,LEN(CPRJ_ID)) +;
          SUBSTR(PMPRJHD.CSTYLE,1,LEN(CSTYLE))+STR(PMPRJHD.LINENO,6)
        IF gfSEEK(PMPRJDT.CPRJ_TYP + PMPRJDT.CPRJ_ID + PMPRJDT.CSTYLE +STR(PMPRJDT.LINENO,6)+ PMPRJDT.coprt_ctg+PMPRJDT.coprt_id,'SYSCHDUL')
          SELECT SYSCHDUL
          gfReplace("COPERSTAT WITH 'X'")

          *E302741,1 WAM 08/25/2010 Add a record in the audit trail file for each canceld task
          SELECT PMPRJDT
          =gfSEEK(coprt_ctg,"PMCTGHD")
          lcInform = 'Operation Category: '           + PMCTGHD.cCtg_Dsc                     + CHR(13)+;
            'Activity: '                     + cOprt_Dsc                            + CHR(13)+;
            'User: '                         + cOprt_res                            + CHR(13)+;
            'Remaining Time on Completion: ' + ALLTRIM(STR(nRem_Dur,3)) + ' day(s)' + CHR(13)+;
            'Transaction Date: '             + DTOC(dEst_strt)                      + CHR(13)+;
            'Completion Date: '              + DTOC(dEst_Fnsh)                      + CHR(13)+;
            'Status: '+IIF(SYSCHDUL.COPERSTAT=[O],[Open],IIF(SYSCHDUL.COPERSTAT=[C],[Complete],IIF(SYSCHDUL.COPERSTAT=[P],[In Work],;
            IIF(SYSCHDUL.COPERSTAT=[H],[Hold],IIF(SYSCHDUL.COPERSTAT=[X],[Void],[])))))

          =GFAUDTRL(loFormSet , lcProg  , lcKey , 'MFPROJ' , 'CANCEL', lcInform )
          *E302741,1 WAM 08/25/2010 (End)

        ENDIF
      ENDSCAN
    ENDIF
    *E302741,1 WAM 08/25/2010 Don't delete audit trail records for cenceled project
    *!*	     SELECT AUDTRAIL
    *!*	     =gfSeek(PADR(lcProg,10)+lcKey)
    *!*	     SCAN REST WHILE capobjnam+key+caudtralid = PADR(lcProg,10)+lcKey
    *!*	       gfDELETE()
    *!*		 ENDSCAN
    *E302741,1 WAM 08/25/2010 Don't delete audit trail records for cenceled project
  ENDSCAN
ENDIF
*E302741,1 WAM 08/25/2010 Convert Audit Trail file to SQL
*!*	SELECT AUDTRAIL
*!*	=gfTableUpdate()
*E302741,1 WAM 08/25/2010 (End)

SELECT PMPRJHD
=gfTableUpdate()
SELECT PMPRJDT
=gfTableUpdate()
SELECT (lnPrvAlias)
*! E302650,1 MMT 12/02/2009 Update Project Status When order or one of its lines is cancelled[End]

*! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[Start]

FUNCTION lfRebalBulkOrder
LPARAMETERS lcBlkOrderNo,lnBlkLineNo
SELECT Ordline
lcTableOrder = ORDER()
lcKeyFlds = EVALUATE(KEY())
=gfSetOrder('Ordline')
=gfSEEK('O'+lcBlkOrderNo)
SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6)  = 'O'+lcBlkOrderNo
  lnOpenBlk = lnOpenBlk + TOTQTY
  lnOpenBlkAmt = lnOpenBlkAmt + TOTQTY*PRICE
  lnBookBlk = lnBookBlk + TotBook
  lnBookBlkAmt = lnBookBlkAmt+ TotBook*PRICE
  IF Ordline.LINENO <> lnBlkLineNo
    = SEEK(ORDLINE.STYLE,'Style')
    = SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')
    =RLOCK('Style')
    FOR lnCntSz = 1 TO 8
      lcCntSz = STR(lnCntSz,1)
      REPLACE  Ord&lcCntSz.   WITH Ord&lcCntSz. + OrdLine.QTY&lcCntSz. ,;
        TotOrd      WITH TotOrd    + OrdLine.QTY&lcCntSz.  IN STYLE
    ENDFOR
    UNLOCK IN 'Style'

    =RLOCK('StyDye')
    FOR lnCntSz = 1 TO 8
      lcCntSz = STR(lnCntSz,1)
      REPLACE Ord&lcCntSz.  WITH Ord&lcCntSz. + OrdLine.QTY&lcCntSz.  ,;
        TotOrd     WITH TotOrd    + OrdLine.QTY&lcCntSz.  IN StyDye
    ENDFOR
    UNLOCK IN 'StyDye'
    *-- Update the Qty on the Configuration level
    IF !EMPTY(ORDLINE.Dyelot) AND SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+ORDLINE.Dyelot,'StyDye')
      REPLACE Ord&lcCntSz.   WITH Ord&lcCntSz. + OrdLine.QTY&lcCntSz. ,;
        TotOrd      WITH TotOrd    + OrdLine.QTY&lcCntSz.  IN StyDye
    ENDIF
  ENDIF
ENDSCAN
=gfSetOrder(lcTableOrder)
=gfSeek(lcKeyFlds)
*! B610053,1 MMT 08/22/2012 When cancel release order from bulk order, reopen bulk order if user confirmed to reopen it[End]

************************************************************
*! Name      : lfCheck_if_Piktkt_is_Packed
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/05/2014
*! Purpose   : Check if there is any Piktkt in the ORDLINE file is Packed, if so the don't continue the CANCEL process
*! B610651
************************************************************
FUNCTION lfCheck_if_Piktkt_is_Packed
            
LOCAL lcORDLINE_SvOrd,lcPACK_HDR_SvOrd,lcPACKED_PIKTKT,llIsPacked
lcORDLINE_SvOrd = ORDER('ORDLINE')
SET ORDER TO ORDLINST IN ORDLINE  && CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
IF !USED('PACK_HDR')
  =gfOpenTable('PACK_HDR','ORDERPCK','SH')   && ORDER+STORE+PACK_NO 
ENDIF 
SELECT PACK_HDR
lcPACK_HDR_SvOrd = ORDER()
SET ORDER TO ORDERPCK   && ORDER+STORE+PACK_NO

llIsPacked = .F.
lcPACKED_PIKTKT = ''
IF SEEK(ordhdr.order,'PACK_HDR')              
  =SEEK(ORDHDR.CORDTYPE+ORDHDR.ORDER,'ORDLINE')
  SELECT ORDLINE
  DO WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
    SELECT ORDLINE
    lcOrderStore = CORDTYPE+ORDER+STORE
    IF SEEK(ORDLINE.ORDER+ORDLINE.STORE,'PACK_HDR')
      SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = lcOrderStore ;
                  FOR !EMPTY(PIKTKT)
        SELECT PACK_HDR
        LOCATE REST WHILE ORDER+STORE+PACK_NO = ORDLINE.ORDER+ORDLINE.STORE FOR PIKTKT = ORDLINE.PIKTKT
        IF FOUND()
          lcPACKED_PIKTKT = ORDLINE.PIKTKT
          EXIT
        ENDIF 
      ENDSCAN
    ELSE
      *- this block is just to get out of the list of records or the current store as it is no longer in PACK_HDR
      SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = lcOrderStore
      ENDSCAN
    ENDIF               
    IF !EMPTY(lcPACKED_PIKTKT)
      EXIT
    ENDIF 
  ENDDO
ENDIF 

SELECT PACK_HDR
SET ORDER TO &lcPACK_HDR_SvOrd
SELECT ORDLINE
SET ORDER TO &lcORDLINE_SvOrd
 IF !EMPTY(lcPACKED_PIKTKT)
  =gfModalGen('INM44001B00000','DIALOG',lcPACKED_PIKTKT)
  llIsPacked = .T.
ENDIF 
RETURN llIsPacked
*- End of lfCheck_if_Piktkt_is_Packed.
