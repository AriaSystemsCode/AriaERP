*:*********************************************************************************
*: Program file  : algpcns.PRG
*: Program desc. : DCC scan, pack and auto-invoice  
*:        System : Aria4 XP.
*:        Module : AL(Allocation).
*:     Developer : Mariam Mazhar (MMT)
*:     Entry     : C201082==>A27,C201094==>A40[T20080717.0001]
*:*********************************************************************************
*: Modifications:
*: C201094,1 MMT 02/02/2009 Fix bug of Error while invoicing[T20080717.0001]
*: B608854,1 MMT 04/23/2009 Validate the Consignment No in custom scan and pack for DCC[T20080717.0001]
*: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table					   [T20080717.0001]
*! B608953,1 MMT 08/02/2009 Fix bug of erorr after Saving packing list[T20080717.0001]
*! B608953,2 MMT 08/11/2009 Fix bug of Printing same packing list if scanned 2 PL in same session[T20080717.0001]
*: B608854,3 MMT 10/13/2009 ORDHDR Table is not udpated after creating invoice [T20080717.0001]
*: B608854,4 MMT 10/22/2009 Error While Printing larg number of PLs   [T20080717.0001]
*: B608854,5 MMT 11/12/2009 Try to solve error of not enough memory at DCC [T20080717.0001]
*: B608854,6 MMT 01/10/2010 Try to solve error of not enough memory at DCC [T20080717.0001]
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [T20080717.0001]
*: B608854,8 MMT 02/15/2010 Solve error of not enough memory at DCC [T20080717.0001]
*: B609224,1 MMT 04/28/2010 Close Cusror opened after printing PL form[T20080717.0001]
*: B609224,2 MMT 07/27/2010 Change Calling the form Init [T20080717.0001]
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003]
*: B609682,1 MMT 10/04/2011 INVCHRG records sometimes not being created[T20110527.0001]
*: B610977,1 MMT 04/07/2015 Fix the Scan pick and pack program for VAT calculcations[T20150319.0010]
*: B610982,1 MMT 04/19/2015 Custom Scan Pick ticket program include trade disc in vat[T20150319.0010]
*:*********************************************************************************
DO FORM  oAriaApplication.ScreenHome+"\AL\alscnPk.scx"

*:*********************************************************************************
*:* Name        : lfInitForm
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     : Init Form
*:*********************************************************************************
FUNCTION  lfInitForm
PARAMETERS loFormSet

*: B608854,8 MMT 02/15/2010 Solve error of not enough memory at DCC [Start]
IF TYPE('loFormSet.lFrstTime') = 'U'
  loFormSet.AddProperty('lFrstTime',.F.)
ENDIF
IF !loFormSet.lFrstTime
*: B608854,8 MMT 02/15/2010 Solve error of not enough memory at DCC [End]


loFormSet.lcScanned = gfTempName()
loFormSet.LCTMPPKTK = GFTEMPNAME()

*: B608854,5 MMT 11/12/2009 Try to solve error of not enough memory at DCC [Start]
loFormSet.LCTMPCUR = GFTEMPNAME()
loFormSet.LCPCKLIN = GFTEMPNAME()
loFormSet.LCCTNHDR = GFTEMPNAME()
loFormSet.LCCTNDTL = GFTEMPNAME()
loFormSet.LCTMPDETFL = GFTEMPNAME()
loFormSet.LCTMASNSHP = GFTEMPNAME()
loFormSet.LCSCAFILE = GFTEMPNAME()
loFormSet.LCCARTONSZ = GFTEMPNAME()
loFormSet.lcPrnAsnShp = gfTempName()
loFormSet.lcAsnLabel = gfTempName()
loFormSet.LCINVHDR = GFTEMPNAME()
loFormSet.LCINVLINE = GFTEMPNAME()
loFormSet.LCCONSINVH = GFTEMPNAME()
loFormSet.LCCONSINVD = GFTEMPNAME()
loFormSet.LCUPSBOX = GFTEMPNAME()
loFormSet.LCPACKHDR = GFTEMPNAME()
loFormSet.LCINSTHDR = GFTEMPNAME()
loFormSet.LCINSTLIN = GFTEMPNAME()
loFormSet.LCAPPCRDT = GFTEMPNAME()
loFormSet.LCTEMPSTK = GFTEMPNAME()
loFormSet.LCPACKLINE = GFTEMPNAME()
loFormSet.LCORDCANLN = GFTEMPNAME()
loFormSet.LCCRTTMP = GFTEMPNAME()
loFormSet.LCUPSTMP = GFTEMPNAME()
*: B608854,5 MMT 11/12/2009 Try to solve error of not enough memory at DCC [End]
*: B608854,8 MMT 02/15/2010 Solve error of not enough memory at DCC [Start]
ENDIF
*: B608854,8 MMT 02/15/2010 Solve error of not enough memory at DCC [End]
*: C201094,1 MMT 02/02/2009 Fix bug of Error while invoicing[STart]
loFormSet.AriaForm1.caption = 'Scan Pick Tickets'
*: C201094,1 MMT 02/02/2009 Fix bug of Error while invoicing[End]

=gfOpenTAble('PIKTKT','PIKTKT')
=gfOpenTAble('PACK_HDR','PACK_HDR')
=gfOpenTAble('SeTups','MODVAR')
=gfOpenTAble('Ordline','Ordline')
=gfOpenTAble('Customer','Customer')
=gfOpenTAble('Ordhdr','Ordhdr')
=gfOpenTAble('ICISTRU','SEGNO')
=gfOpenTAble('SyUUser','CUSER_ID','SH','USers')
=gfOpenTAble('CUSTDEPT','CUSTDEPT','SH')
loFormSet.lcMask     = gfItemMask("PM", '', '0001')
loFormSet.llExtSizSc = gfGetMemVar('M_USEEXSSC')
loFormSet.llUPCInst  = ('UP' $ oAriaApplication.CompanyInstalledModules)
STORE 0 TO loFormSet.lnSizePos,loFormSet.lnSizeLen
IF loFormSet.llExtSizSc 
  DECLARE laStySeg[1,1]
  STORE "" TO laStySeg
  =gfItemMask(@laStySeg)
  FOR lnCnt = 1 TO ALEN(laStySeg,1)
    IF laStySeg[lnCnt , 1] = "S"
      loFormSet.lcSizeSep  = ALLTRIM(laStySeg[lnCnt-1 , 6])
      loFormSet.lnSizePos  = laStySeg[lnCnt , 4] - IIF(!EMPTY(loFormSet.lcSizeSep) , 1 , 0)
      loFormSet.lnSizeLen  = LEN(laStySeg[lnCnt , 3]) + IIF(!EMPTY(loFormSet.lcSizeSep) , 1 , 0)
    ENDIF
  ENDFOR
ENDIF


loFormSet.ChangeMode("A")
*: B608854,8 MMT 02/15/2010 Solve error of not enough memory at DCC [Start]
IF !loFormSet.lFrstTime
  loFormSet.lFrstTime = .T.
*: B608854,8 MMT 02/15/2010 Solve error of not enough memory at DCC [ENd]

DIMENSION laFileStruct[1,4]
laFileStruct[1,1] = 'ScPiktkt'
laFileStruct[1,2] = 'C'
laFileStruct[1,3] = 6
laFileStruct[1,4] = 0
gfCrtTmp(loFormSet.lcScanned,@laFileStruct,"ScPiktkt",loFormSet.lcScanned,.F.)

DIMENSION laPkStruct[6,4]
laPkStruct[1,1] = 'ACCOUNT'
laPkStruct[1,2] = 'C'
laPkStruct[1,3] = 5
laPkStruct[1,4] = 0

laPkStruct[2,1] = 'STORE'
laPkStruct[2,2] = 'C'
laPkStruct[2,3] = 8
laPkStruct[2,4] = 0

laPkStruct[3,1] = 'ORDER'
laPkStruct[3,2] = 'C'
laPkStruct[3,3] = 6
laPkStruct[3,4] = 0

laPkStruct[4,1] = 'PIKTKT'
laPkStruct[4,2] = 'C'
laPkStruct[4,3] = 6
laPkStruct[4,4] = 0

laPkStruct[5,1] = 'NCARTON'
laPkStruct[5,2] = 'N'
laPkStruct[5,3] = 6
laPkStruct[5,4] = 0

laPkStruct[6,1] = 'PIKQTY'
laPkStruct[6,2] = "N"
laPkStruct[6,3] = 6
laPkStruct[6,4] = 0

gfCrtTmp(loFormSet.LCTMPPKTK,@laPkStruct,"ACCOUNT+STORE+ORDER+PIKTKT",loFormSet.LCTMPPKTK,.F.)
*: B608854,8 MMT 02/15/2010 Solve error of not enough memory at DCC [Start]
ELSE
  SELECT(loFormSet.LCTMPPKTK)
  ZAP 
  SELECT(loFormSet.lcScanned)
  ZAP 
ENDIF
*: B608854,8 MMT 02/15/2010 Solve error of not enough memory at DCC [End]
lfAddControlSrc(loFormSet)

*:**************************************************************************
*:* Name        : lfChngeMode
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     : Change mode
*:***************************************************************************
FUNCTION lfChngeMode
PARAMETERS loFormSet


*:**************************************************************************
*:* Name        : lfvPiktkt
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     : Validate PIKTKT No.
*:***************************************************************************
FUNCTION lfvPiktkt
PARAMETERS lcPiktkt,loFormSet

LNSLCT = SELECT()
llContinue = .F.
IF !EMPTY(lcPiktkt)
  IF gfSEEK(lcPiktkt,'PIKTKT')
    IF PIKTKT.STATUS $ 'C'
       = GFMODALGEN('INM00000B00000',.F.,.F.,.F.,'This Pick Ticket is completed, Can not be scanned')
       loFormSet.AriaForm1.txtPiktkt.Value = ''
       SELECT (LNSLCT)
       RETURN 0
    ELSE
      IF SEEK(lcPiktkt,loFormSet.lcScanned) OR PIKTKT.SCANNED
         lcMsg = 'Pick Ticket &lcPiktkt has already been scanned - please scan another Pick Ticket or Save '
         = GFMODALGEN('INM00000B00000',.F.,.F.,.F.,LCMSG)
         loFormSet.AriaForm1.txtPiktkt.Value = ''
         SELECT (LNSLCT)         
         RETURN 0
      ENDIF 
      IF gfSEEK(PIKTKT.ORDER+PIKTKT.STORE,'PACK_HDR')
        SELECT PACK_HDR
        LOCATE REST FOR PIKTKT=PIKTKT.PIKTKT WHILE ORDER+STORE+PACK_NO=PIKTKT.ORDER+PIKTKT.STORE
        IF FOUND()
          SELECT (loFormSet.lcScanned)
          APPEND BLANK 
          REPLACE ScPiktkt WITH PIKTKT.PIKTKT
          lcMsg = 'Pick Ticket &lcPiktkt  has a related pack list created - please scan another Pick Ticket or Save '
          = GFMODALGEN('INM00000B00000',.F.,.F.,.F.,LCMSG)
          loFormSet.AriaForm1.txtPiktkt.Value = ''
          SELECT (LNSLCT)
          RETURN 0
        ENDIF 
      ENDIF 
    ENDIF 
  ENDIF 
ELSE
  RETURN 
ENDIF 
IF !llContinue
  llContinue = gfSEEK(lcPiktkt,'PIKTKT') .AND. PIKTKT.STATUS $ 'PO'
ENDIF
IF llContinue
  GO TOP IN (loFormSet.LCTMPPKTK)
  IF .NOT. EOF(loFormSet.LCTMPPKTK)
    LCTMPPKTK = loFormSet.LCTMPPKTK
    IF PIKTKT.ACCOUNT <> &lcTmpPkTk..ACCOUNT .OR. PIKTKT.STORE <> &lcTmpPkTk..STORE
      WAIT WINDOW NOWAIT 'The piktkt &lcPiktkt has different Account/Store, can not accept' TIMEOUT 2
      SELECT (LNSLCT)
      RETURN 0 
    ENDIF
  ENDIF
ENDIF

*!*	IF llContinue &&.AND. gfSEEK(lcPiktkt,'SLSPKHST')
*!*	  = GFMODALGEN('INM00000B00000',.F.,.F.,.F.,'You cannot scan the same record in twice.')
*!*	  loFormSet.AriaForm1.txtPiktkt.Value = ''
*!*	  SELECT (LNSLCT)
*!*	  RETURN 0 
*!*	ENDIF

IF !llContinue AND !EMPTY(lcPiktkt)
  llContinue = lfPiktktBr()
ENDIF

LCTMPPKTK = loFormSet.LCTMPPKTK
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
*!*  IF !USED('PICKPACK')
*!*   =gfOpenTable('PICKPACK','PICKPACK')
*!*  ENDIF
*!*  IF !gfSeek(PIKTKT.PIKTKT,'PICKPACK')
*!*    = GFMODALGEN('TRM00000B00000',.F.,.F.,.F., 'Pick Ticket has yet to be issued - Please check with your supervisor')
*!*    loFormSet.AriaForm1.txtPiktkt.Value = ''
*!*    SELECT (LNSLCT)
*!*    RETURN 0
*!*  ENDIF
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
IF llContinue
  IF SEEK(PIKTKT.ACCOUNT+PIKTKT.STORE+PIKTKT.ORDER+PIKTKT.PIKTKT,LCTMPPKTK)
     WAIT WINDOW NOWAIT 'The piktkt &lcPiktkt is already scanned in the current batch.' TIMEOUT 2
  ELSE
    SELECT &lcTmpPkTk 
    LOCATE
    IF EOF()
      *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
*!*    	  IF EMPTY(ALLTRIM(PICKPACK.CPICKBY)) && OR EMPTY(ALLTRIM(PICKPACK.CHKEDBY))
*!*    	    loFormSet.ariaform1.cboPick.Value = ""
*!*  	    *loFormSet.ariaform1.cboPack.Value = ""
*!*          = GFMODALGEN('INM00000B00000',.F.,.F.,.F.,'You cannot scan Pick Ticket that has an empty Picked by or Checked by value')
*!*  	    loFormSet.AriaForm1.txtPiktkt.Value = ''
*!*  	    RETURN 0
*!*   	  ENDIF	 
	  *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
       LLSCANOK = .T.
       *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
       *loFormSet.ariaform1.cboPick.Value = ALLTRIM(PICKPACK.CPICKBY)
	   *loFormSet.ariaform1.cboPack.Value = ALLTRIM(PICKPACK.CHKEDBY)
*	   loFormSet.ariaform1.txtCartons.Value = IIF(ISNULL(PICKPACK.NOOFCARTON) OR PICKPACK.NOOFCARTON = 0,1,PICKPACK.NOOFCARTON)
	*   loFormSet.ariaform1.txtCongNo.Value = PICKPACK.CONSGMENT
       *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
    ELSE
       llScanOk = ( PIKTKT.ACCOUNT+PIKTKT.STORE = &lcTmpPkTk..ACCOUNT+&lcTmpPkTk..STORE )
*!*         *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
*!*         IF loFormSet.ariaform1.cboPick.Value <> ALLTRIM(PICKPACK.CPICKBY) &&OR loFormSet.ariaform1.cboPack.Value <> ALLTRIM(PICKPACK.CHKEDBY)
*!*           llScanOk = .F.
*!*           WAIT WINDOW NOWAIT 'The picked by or checked by does not match the first pick ticket scanned' TIMEOUT 2
*!*           RETURN 0
*!*         ENDIF
*!*         loFormSet.ariaform1.txtCartons.Value = loFormSet.ariaform1.txtCartons.Value +  IIF(ISNULL(PICKPACK.NOOFCARTON) or PICKPACK.NOOFCARTON = 0,1,PICKPACK.NOOFCARTON)
*!*         *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
    ENDIF
    IF LLSCANOK
      APPEND BLANK
      REPLACE ACCOUNT WITH PIKTKT.ACCOUNT,;
              STORE   WITH PIKTKT.STORE,;
              ORDER   WITH PIKTKT.ORDER,;
              PIKTKT  WITH PIKTKT.PIKTKT,;
              NCARTON WITH 1
      *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
      loFormSet.ariaform1.txtCartons.VAlue = 1       
      *REPLACE NCARTON WITH IIF(ISNULL(PICKPACK.NOOFCARTON) OR PICKPACK.NOOFCARTON=0,1,PICKPACK.NOOFCARTON) 
      *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
      = gfSEEK('O'+ORDER,'ORDLINE')
      SELECT ORDLINE
      SUM TOTQTY TO lnTotPik  REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+ &lcTmpPkTk..ORDER  FOR PIKTKT = &lcTmpPkTk..PIKTKT
      SELECT &lcTmpPkTk 
      REPLACE PIKQTY WITH LNTOTPIK
      loFormSet.ariaForm1.grdPiktkt.Refresh()
      loFormSet.ariaForm1.cmdFinish.Enabled = .T. 
      loFormSet.AriaForm1.txtPiktkt.Value = ''
      =gfSeek("M"+PIKTKT.ACCOUNT,'Customer','Customer')
      loFormSet.ariaform1.cboShipVia.value = Customer.ShipVia
      RETURN 0
    ENDIF
  ENDIF
ENDIF 

*:**************************************************************************
*:* Name        : lfPiktktBr
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     : Browse PIKTKT 
*:***************************************************************************
FUNCTION  lfPiktktBr
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [Start]
*PRIVATE LCBRFIELDS, LCFILE_TTL, LABROW, LCFIELDS, LNCURALIAS, LCCURTAG, LLRETURN, LCBRFIELDS, LNSLCT, LCFORCond 
PRIVATE LCBRFIELDS, LCFILE_TTL, LABROW, LCFIELDS, LLRETURN, LCBRFIELDS, LNSLCT, LCFORCond 
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [End]
LNSLCT = SELECT()
DIMENSION LABROW[ 7]
STORE SPACE(0) TO LABROW
LCFIELDS = 'PikTkt,Order,Store,Date,Account'
LCBRFIELDS = "PikTkt:H='PikTkt'  ,"+"Account:H='Account',"+"Store :H='Store'   ,"+"Order :H='Order#'  ,"+"Date:H='PikDate'    "
LCFILE_TTL = 'Pick Tickets'
lcPiktktFile = loFormSet.LCTMPPKTK
GO TOP IN (loFormSet.LCTMPPKTK)
IF EOF()
   LCFORCond = ''
ELSE
   LCFORCond = &lcPiktktFile..ACCOUNT + &lcPiktktFile..STORE
ENDIF
SELECT PIKTKT
=gfSeek("")
LOCATE
IF !EMPTY(LCFORCond)
  lcBrCond = "'' FOR PikTkt.Account + PikTkt.STORE = LCFORCond AND PikTkt.Status $ 'PO' AND PIKTKT.SCANNED=.F."
ELSE
  lcBrCond = "'' FOR PikTkt.Status $ 'PO' AND PIKTKT.SCANNED=.F."
ENDIF   

LLRETURN = ARIABROW(lcBrCond,LCFILE_TTL,GNBRFSROW1,GNBRFSCOL1,GNBRFSROW2,GNBRFSCOL2,.F.,.F.,LCFIELDS,'laBrow',.F.,'PikTkt',.F.)
loFormSet.AriaForm1.txtPiktkt.Value = LABROW(1)
SELECT (LNSLCT)
RETURN LLRETURN

*:**************************************************************************
*:* Name        : lfAddControlSrc
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     : Add Grid control source
*:***************************************************************************
FUNCTION lfAddControlSrc
PARAMETERS loFormSet

loFormSet.ariaForm1.grdPiktkt.RecordSource  = ''
loFormSet.ariaForm1.grdPiktkt.RecordSource  = loFormSet.LCTMPPKTK
loFormSet.ariaForm1.grdPiktkt.Column1.ControlSource = loFormSet.LCTMPPKTK + '.Account'
loFormSet.ariaForm1.grdPiktkt.Column2.ControlSource = loFormSet.LCTMPPKTK + '.Store'
loFormSet.ariaForm1.grdPiktkt.Column3.ControlSource = loFormSet.LCTMPPKTK + '.Order'
loFormSet.ariaForm1.grdPiktkt.Column4.ControlSource = loFormSet.LCTMPPKTK + '.PIKTKT'
loFormSet.ariaForm1.grdPiktkt.Column5.ControlSource = loFormSet.LCTMPPKTK + '.PIKQTY'
loFormSet.ariaForm1.grdPiktkt.Column6.ControlSource = loFormSet.LCTMPPKTK + '.NCARTON'

*:**************************************************************************
*:* Name        : lfUndo
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     : UNDO Function
*:***************************************************************************
FUNCTION lfUndo
PARAMETERS loFormset
loFormset.PreferenceName = ""
*: B609224,2 MMT 07/27/2010 Change Calling the form Init [Start]
*loFormset.init()
lfInitForm(loFormset)
IF TYPE("loFormset.ariaForm1.ariaPAGEFRAME1.page4.cntInvoicesummary.ChargesFile") = 'U'
  loFormset.ariaForm1.ariaPAGEFRAME1.page4.cntInvoicesummary.AddProperty ('ChargesFile',"")
ENDIF 
*: B609224,2 MMT 07/27/2010 Change Calling the form Init [End]
*:**************************************************************************
*:* Name        : lfSavePktkt
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     : Save PKTKT Function
*:***************************************************************************
FUNCTION lfSavePktkt
PARAMETERS loFormset
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
*!*  lcTimNow = gfGetTime()
*!*  lcDespatch = loFormset.ariaForm1.txtDespatch.Value
*!*  lcCongs = loFormset.ariaForm1.txtCongNo.Value
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
LCTMPPKTK = loFormSet.LCTMPPKTK
SELECT &lcTmpPkTk
LOCATE
SCAN
   =gfSEEK(&lcTmpPkTk..PIKTKT,'PIKTKT')
   SELECT PIKTKT
   gfREPLACE("SCANNED WITH .T.")
*!*     *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
*!*     SELECT PICKPACK
*!*     =gfSeek(&lcTmpPkTk..PIKTKT,'PICKPACK')
*!*     =gfReplace("CPSTATUS   WITH 'Despatched'")
*!*     =gfReplace("CDESPBY WITH lcDespatch")
*!*     =gfReplace("DDESPBY WITH oAriaApplication.SystemDate")
*!*     =gfReplace("CDESPTIME WITH lcTimNow")   
*!*     =gfReplace("CONSGMENT WITH lcCongs")   
   *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
ENDSCAN
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
*!*  SELECT PICKPACK
*!*  =gfTableUpdate()
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
SELECT PIKTKT
=gfTableUpdate()


= LFCRTPACK(loFormset)
= LFUPDENT()
GO TOP IN &lcTmpPkTk
=gfSEEK('M'+&lcTmpPkTk..ACCOUNT,'CUSTOMER')


IF CUSTOMER.CONSOL='N'
  = LFCRTINVS()
ENDIF
SELECT Ordline
=gfTableUpdate()

SELECT PIKTKT
=gfTableUpdate()

= LFPRNPACK()


*:**************************************************************************
*:* Name        : LFBEFORSAV
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     : Before Save PKTKT Function
*:***************************************************************************
FUNCTION LFBEFORSAV
PARAMETERS loformSet
lcTmpPkTk =loFormSet.LCTMPPKTK
SELECT &lcTmpPkTk
LOCATE
SCAN
   =gfSEEK('O'+&lcTmpPkTk..ORDER,'ORDHDR')
   PRIVATE LNSYUSERRE, LCLOK_USER
   * HES
   LCLOK_USER = ""
   * HES
   IF ORDHDR.LLOK_STAT
      LNSYUSERRE = IIF(RECNO('Users')>RECCOUNT('Users'),0,RECNO('Users'))
      IF gfSeek(ORDHDR.CLOK_USER,"USers")
        LCLOK_USER = USers.CUSER_ID
        LCLOK_USER = IIF(EMPTY(LCLOK_USER),oAriaApplication.User_ID,LCLOK_USER)
      ENDIF   
      IF GFMODALGEN('INM40182B00000','ALERT','Order #'+ORDHDR.ORDER+'|'+LCLOK_USER)=1
         RETURN .F.
      ENDIF
   ENDIF
   
   LCORDERNO = ORDHDR.ORDER
   IF ORDHDR.STATUS='C'
      = GFMODALGEN('TRM40109B00000','ALERT',LCORDERNO)
      RETURN .F.
   ENDIF
   IF ORDHDR.STATUS = 'B'
      = GFMODALGEN('TRM40155B00000','ALERT',LCORDERNO)
      RETURN .F.
   ENDIF
   IF  !gfSEEK('M'+ORDHDR.ACCOUNT,'Customer')
      = GFMODALGEN('TRM40112B00000','ALERT','Account '+ORDHDR.ACCOUNT)
      RETURN .F.
   ENDIF
   IF CUSTOMER.STATUS <> 'A'
      = GFMODALGEN('TRM40113B00000','ALERT','Account '+ORDHDR.ACCOUNT)
      RETURN .F.
   ENDIF
   
   lcStoreNo = &lcTmpPkTk..STORE
   IF  !EMPTY(LCSTORENO) .AND.  !gfSEEK('S'+ORDHDR.ACCOUNT+LCSTORENO,'Customer')
      = GFMODALGEN('TRM40112B00000','ALERT','Store '+ALLTRIM(LCSTORENO))
      RETURN .F.
   ENDIF
   IF CUSTOMER.STATUS <> 'A'
      = GFMODALGEN('TRM40113B00000','ALERT','Store '+ALLTRIM(LCSTORENO))
      RETURN .F.
   ENDIF
   lcOrderNo = &lcTmpPkTk..ORDER 
   IF !gfSEEK('O'+LCORDERNO,'ordline')
      = GFMODALGEN('TRM40114B00000','ALERT',LCORDERNO)
      RETURN .F.
   ENDIF
   IF ORDHDR.BULK = 'Y' .AND. gfGetMemVar('M_INVBULK')='N'
      = GFMODALGEN('TRM40151B00000','ALERT',LCORDERNO)
      RETURN (.F.)
   ENDIF
   IF ORDHDR.STATUS='H'
      IF gfGetMemVar('M_INVHOLD')='Y'
         IF GFMODALGEN('QRM40111B40003','ALERT',LCORDERNO+'| ')=2
            RETURN (.F.)
         ENDIF
      ELSE
         = GFMODALGEN('TRM40111B00000','ALERT',LCORDERNO+'| Cannot Ship.')
         RETURN (.F.)
      ENDIF
   ENDIF
ENDSCAN
*:**************************************************************************
*:* Name        : LFCRTPACK
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     : Create Packing list for  PKTKT Function
*:***************************************************************************
FUNCTION LFCRTPACK
PARAMETERS loFormset
PRIVATE LASTRU, LCTMPCUR, LCPCKLIN, LCSVPKLNOR, LNTOTPIK, LNI
= gfOpenTable('INVHDR','INVHDR','SH')
= gfOpenTable('INVLINE','INVLINE','SH')
= gfOpenTable('Style','Style','SH')
= gfOpenTable('Scale','Scale','SH')
= gfOpenTable('STYDYE','STYDYE','SH')


IF  !USED('PACK_HDR')
   =gfOpenTable('PACK_HDR','PACK_HDR','SH')
ENDIF
SELECT PACK_HDR
=gfSetOrder('PACK_HDR')

IF  !USED('PACK_LIN')
   =gfOpenTable('PACK_LIN','PACK_LIN','SH')
ENDIF

*: B608854,5 MMT 11/12/2009 Try to solve error of not enough memory at DCC [Start]
*!*	LCTMPCUR = GFTEMPNAME()
*!*	LCPCKLIN = GFTEMPNAME()
LCTMPCUR = loFormSet.LCTMPCUR
LCPCKLIN = loFormSet.LCPCKLIN 
*: B608854,5 MMT 11/12/2009 Try to solve error of not enough memory at DCC [End]

SELECT &lcTmpPkTk
LOCATE

=gfSEEK('M'+&lcTmpPkTk..ACCOUNT,'CUSTOMER')

SCAN
   WAIT WINDOW NOWAIT 'produce and print a Packing List '+PIKTKT
   =gfSEEK('O'+&lcTmpPkTk..Order,'ORDHDR')
   =gfSEEK('O'+&lcTmpPkTk..Order,'ORDLINE') 
   LCSVPKLNOR = ORDER('PACK_LIN')
   SELECT PACK_LIN
   =gfSetOrder('PACKSTYLE')
   
   SELECT PACK_HDR
   
   IF !gfSEEK(&lcTmpPkTk..PIKTKT,'PACK_HDR')
      gfAPPEND("") 
   ENDIF
   gfREPLACE([PACK_NO    WITH &lcTmpPkTk..PIKTKT,]+;
   			 [PIKTKT     WITH &lcTmpPkTk..PIKTKT,]+;
   			 [ORDER      WITH &lcTmpPkTk..ORDER,]+;
   			 [ACCOUNT    WITH &lcTmpPkTk..ACCOUNT,]+;
   			 [STORE      WITH &lcTmpPkTk..STORE,]+;
   			 [TOT_CART   WITH 1,]+;
   			 [SHIPVIA    WITH ORDHDR.SHIPVIA,]+;
   			 [CTOSTORCN  WITH 'S',]+;
   			 [CWARECODE  WITH ORDHDR.CWARECODE,]+;
   			 [CONSGMENT  WITH '',]+;
   			 [WEIGHTDL   WITH 0 ,]+;
   			 [CCRTNVLTYP WITH '',]+;
   			 [PICKEDBY   WITH '',]+;
   			 [CHECKEDBY  WITH '',]+;
   			 [LCARRCTNID WITH .F.,]+;
   			 [DSHIPDATE  WITH  oAriaApplication.SystemDate])
   			 
   gfReplace([NOOFCARTON WITH loFormSet.ariaform1.txtCartons.VAlue])
   gfReplace([CONSGMENT  WITH loFormSet.ariaform1.txtCongNo.value])
   			 
   gfReplace([PICKEDBY WITH loFormSet.ariaform1.cboPick.Displayvalue,]+;
   			 [CHECKEDBY WITH loFormSet.ariaform1.cboPack.Displayvalue,]+;
   			 [CARRIER WITH loFormSet.ariaform1.cboShipVia.Displayvalue])
   
   = gfAdd_Info('PACK_HDR')			 
   = gfReplace("")

   IF gfSEEK(&lcTmpPkTk..PIKTKT,'PACK_LIN')
     SELECT PACK_LIN
     SCAN REST WHILE PACK_NO = &lcTmpPkTk..PIKTKT
       gfDELETE() 
     ENDSCAN 
   ENDIF
   
   SELECT ORDLINE
   =AFIELDS(LASTRU)
   =gfCrtTmp(LCTMPCUR,@LASTRU)
   
   SELECT PACK_LIN
   = AFIELDS(LASTRU)
   =gfCrtTmp(LCPCKLIN,@LASTRU,"STR(NO_CART,4)+STYLE+STR(NORDLINENO,6)",lcPckLin)
   
   SELECT ORDLINE
   LNTOTPIK = 0
   SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+&lcTmpPkTk..ORDER  FOR PIKTKT  = &lcTmpPkTk..PIKTKT .AND.  ACCOUNT = &lcTmpPkTk..ACCOUNT .AND.  STORE   = &lcTmpPkTk..STORE
      =gfSeek(Style,'Style','Style')
      gfREPLACE([NPCK1 WITH PIK1,]+;
      			[NPCK2 WITH PIK2,]+;
      			[NPCK3 WITH PIK3,]+;
      			[NPCK4 WITH PIK4,]+;
      			[NPCK5 WITH PIK5,]+;
      			[NPCK6 WITH PIK6,]+;
      			[NPCK7 WITH PIK7,]+;
      			[NPCK8 WITH PIK8,]+;
      			[NPWGHT WITH STYLE.NSTYWEIGHT*TOTPIK])
      			
      SCATTER MEMVAR
      INSERT INTO &lcTmpCur FROM MEMVAR
      LNTOTPIK = LNTOTPIK+M.TOTPIK
   ENDSCAN
   lnCrtQty = INT(lnTotPik/&lcTmpPkTk..NCARTON)
   SELECT &lcTmpCur
   GOTO TOP
   DO WHILE LNTOTPIK>0
      WAIT WINDOW NOWAIT 'Creating cartons distribution for packing list : ' + &lcTmpPkTk..PIKTKT 
      LNSZ = 1
      FOR lnCarton = 1 TO &lcTmpPkTk..NCARTON
         FOR LNI = 1 TO LNCRTQTY
            SELECT &lcTmpCur
            LCSZ = STR(LNSZ,1)
            DO WHILE EVALUATE(LCTMPCUR+'.PIK'+LCSZ)=0
               LNSZ = LNSZ+1
               IF LNSZ>8
                  LNSZ = 1
                  SKIP IN &lcTmpCur
                  IF EOF(LCTMPCUR)
                     GO TOP IN &lcTmpCur
                  ENDIF
               ENDIF
               LCSZ = STR(LNSZ,1)
            ENDDO
            IF !SEEK(STR(lnCarton,4)+&lcTmpCur..STYLE+STR(&lcTmpCur..LINENO,6),lcPckLin)
              =gfSEEK(&lcTmpCur..STYLE , 'STYLE' )
              =gfSEEK(&lcTmpCur..STYLE+&lcTmpCur..CWARECODE , 'STYDYE' )
              SELECT PACK_HDR
              gfREPLACE ([NLASTLNO WITH NLASTLNO+1])
              SELECT &lcPckLin
              APPEND BLANK
              REPLACE PACK_NO    WITH &lcTmpPkTk..PIKTKT,;
            	      LINE_NO    WITH PACK_HDR.NLASTLNO,;
              		  STYLE      WITH &lcTmpCur..STYLE,;
               		  NO_CART    WITH lnCarton,;
               		  NORDLINENO WITH &lcTmpCur..LINENO 
              = gfAdd_Info(lcPckLin)			 
            ENDIF
            
            REPLACE &lcTmpCur..PIK&lcSz WITH &lcTmpCur..PIK&lcSz - 1 
            REPLACE &lcPckLin..QTY&lcSz WITH &lcPckLin..QTY&lcSz + 1
            
            LNTOTPIK = LNTOTPIK-1
            IF LNTOTPIK=0
               EXIT
            ENDIF
         ENDFOR
         IF LNTOTPIK=0
            EXIT
         ENDIF
      ENDFOR
   ENDDO
   SELECT &lcPckLin
   GOTO TOP
   SCAN 
     =gfSeek(Style,'Style','Style')
     REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8,;
   	  	     WEIGHT WITH STYLE.NSTYWEIGHT*TOTQTY 
   	  	     
   	 SCATTER MEMO MEMVAR 
   	 SELECT PACK_LIN
   	 APPEND BLANK 
   	 GATHER MEMO MEMVAR 
   	 gfReplace('')
   ENDSCAN 	  	     
   DIMENSION LASUMS[3]
   LASUMS = 0
   SELECT SUM(TOTQTY), SUM(WEIGHT), MAX(NO_CART) FROM (LCPCKLIN) INTO ARRAY LASUMS
   SELECT PACK_HDR
   gfREPLACE([TOT_PCS WITH LASUMS(1),]+;
   			 [TOT_WGHT WITH LASUMS(2),]+;
   			 [NLASTCART WITH LASUMS(3)])
   SELECT PACK_LIN  
   = LFSAVCARTN()
ENDSCAN
USE IN (LCTMPCUR)
USE IN (LCPCKLIN)
SELECT PACK_LIN  
=gfTableUpdate()

SELECT PACK_Hdr
=gfTableUpdate()


*! B608953,2 MMT 08/11/2009 Fix bug of Printing same packing list if scanned 2 PL in same session[Start]   
SELECT Style 
=gfTableUpdate()

SELECT Stydye
=gfTableUpdate()
*! B608953,2 MMT 08/11/2009 Fix bug of Printing same packing list if scanned 2 PL in same session[Start]   


SELECT ASN_ship
=gfTableUpdate()


*:**************************************************************************
*:* Name        : LFSAVCARTN
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  Packing list Cartons save Function
*:***************************************************************************
FUNCTION LFSAVCARTN
PRIVATE LCSCFIELDS, LADATA, LNLN
LCSCFIELDS = 'Pack_No, Order, Piktkt, Account, Store, Note, ShipVia,'+;
	         'Tot_Wght, Tot_Cart, Tot_Pcs, Sp_Inst1, Sp_Inst2, LStandCtn,'+;
	         'CToStorCn, CPkChCode, CPkDsCode'
	         
LNLN = OCCURS(',',LCSCFIELDS)+1
DIMENSION LADATA[ LNLN]
laData[1] = &lcTmpPkTk..PIKTKT
laData[2] = &lcTmpPkTk..ORDER
laData[3] = &lcTmpPkTk..PIKTKT
laData[4] = &lcTmpPkTk..ACCOUNT
laData[5] = &lcTmpPkTk..STORE

*: B608854,5 MMT 11/12/2009 Try to solve error of not enough memory at DCC [Start]
*!*	LCCTNHDR = GFTEMPNAME()
*!*	LCCTNDTL = GFTEMPNAME()
*!*	LCTMPDETFL = GFTEMPNAME()
*!*	LCTMASNSHP = GFTEMPNAME()
*!*	LCSCAFILE = GFTEMPNAME()
*!*	LCCARTONSZ = GFTEMPNAME()
*!*	lcPrnAsnShp = gfTempName()
*!*	lcAsnLabel = gfTempName()
LCCTNHDR    = loFormSet.LCCTNHDR
LCCTNDTL    = loFormSet.LCCTNDTL 
LCTMPDETFL  = loFormSet.LCTMPDETFL 
LCTMASNSHP  = loFormSet.LCTMASNSHP 
LCSCAFILE   = loFormSet.LCSCAFILE 
LCCARTONSZ  = loFormSet.LCCARTONSZ 
lcPrnAsnShp = loFormSet.lcPrnAsnShp 
lcAsnLabel  = loFormSet.lcAsnLabel 
*: B608854,5 MMT 11/12/2009 Try to solve error of not enough memory at DCC [End]

lcSndPort = "COM2"
= LFCRTNDTL()

IF  !USED('WAREHOUS')
   = gfOpenTable('WAREHOUS','WAREHOUS','SH')
ENDIF

IF  !USED('SPCK_LIN')
   = gfOpenTable('SPCK_LIN','SPCKLINS','SH')
ENDIF

LLEDIACC = .F.
LLEDISYS = ('AS' $ oAriaApplication.CompanyInstalledModules)
IF LLEDISYS
   IF  !USED('EDIACPRT')
      = gfOPenTable('EDIACPRT','ACCFACT','SH')
   ENDIF
   IF  !USED('EDIPH')
      = gfOPenTable('EDIPH','PARTNER','SH')
   ENDIF
   LLEDIACC = LLEDISYS .AND. gfSEEK('A'+LADATA(4),'EDIACPRT') .AND. gfSEEK(EDIACPRT.CPARTCODE,'EDIPH')
ENDIF
DIMENSION laflStru[1,4]
laflStru[1,1] =  'SZCODE'
laflStru[1,2] =  'C'
laflStru[1,3] = 1
laflStru[1,4] = 0
=gfcrtTmp (LCCARTONSZ,@laflStru,'SZCODE',LCCARTONSZ)

FOR LNI = 1 TO 8
   INSERT INTO (LCCARTONSZ) ( SZCODE ) VALUE ( STR(LNI,1) )
ENDFOR

STORE 0 TO LNNONMAJST, LNCOLORLEN, LNMAJSEG, LNMAJLEN
STORE '' TO LCFREE_CLR

= LFEVALSEGS()
= GFOPENTable('Asn_Ship','Asn_Ship','SH')
SELECT 'Asn_Ship'
IF  .NOT. USED(LCTMASNSHP)
   COPY TO (oAriaApplication.WORKDIR+LCTMASNSHP) STRUCTURE
   = GFOPENFILE(oAriaApplication.WORKDIR+LCTMASNSHP,'','EX')
   INDEX ON STR(CART_NO,6) TAG (LCTMASNSHP)
ENDIF
= GFOPENTabLE(oAriaApplication.SysPath+'SYCASNLB','ASNlbl','SH')
= GFOPENTabLE(oAriaApplication.SysPath+'SYCASNHD','VerPrt','SH')
= GFOPENTabLE(oAriaApplication.SysPath+'SYCASNDT','CVer','SH')
SELECT SYCASNHD
=gfSeek("")
LOCATE FOR SYCASNHD.LDETLABEL=.T.
IF FOUND()
   LLDETLABEL = .T.
ENDIF
LLUPCINST = ('UP'$ oAriaApplication.CompanyInstalledModules)
LCDETLBALL = ''
IF LLUPCINST
   = GFOPENTabLE('STYLEUPC','STYLEUPC','SH')
ENDIF
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*PRIVATE LLCHOICE, LNCARTONS, LNCARREF, LNACTALIAS, LCDETORDER, LLPRINSEL
PRIVATE LNCARTONS, LNCARREF, LNACTALIAS, LCDETORDER
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]
LNACTALIAS = SELECT(0)
STORE SPACE(0) TO LASOURCE, LATARGET
LNCARTONS = 0
GOTO IN (LCCTNDTL) BOTTOM
lnCartons = &lcCtnDtl..Cart_No
IF LNCARTONS<>0
   DIMENSION LASOURCE[ LNCARTONS], LATARGET[ 1]
ENDIF
DIMENSION LATARGET[ 1]
LNCARREF = 0
LCDETORDER = ORDER(LCCTNDTL)
SET ORDER TO (LCCTNDTL) IN (LCCTNDTL)
FOR LNCARREF = 1 TO LNCARTONS
   = SEEK(STR(LNCARREF,4),LCCTNDTL) .AND. LFVLBLINFO(LNCARREF)
   laSource[lnCarRef] =  "Carton # " + PADL(ALLTRIM(STR(&lcCtnDtl..CART_NO)),4) 
   IF SEEK(STR(LNCARREF,6),LCTMASNSHP)
      SELECT (LCTMASNSHP)
      SCATTER MEMO MEMVAR
      IF gfSEEK(&lcTmAsnShp..BOL_NO+&lcTmAsnShp..PACK_NO+STR(&lcTmAsnShp..CART_NO,6),"Asn_Ship")
         SELECT ASN_SHIP
         GATHER MEMVAR MEMO
         =gfReplace('')
      ELSE
         INSERT INTO Asn_Ship FROM MEMVAR
         =gfReplace("")
      ENDIF
   ENDIF
ENDFOR
IF USED(LCCTNHDR)
   USE IN &lcCtnHdr
ENDIF
ERASE (oAriaApplication.WORKDIR+LCCTNHDR+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCCTNHDR+'.CDX')
IF USED(LCCTNDTL)
   USE IN &lcCtnDtl
ENDIF
ERASE (oAriaApplication.WORKDIR+LCCTNDTL+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCCTNDTL+'.CDX')
IF USED(LCTMPDETFL)
   USE IN &lcTmpDetFl
ENDIF
ERASE (oAriaApplication.WORKDIR+LCTMPDETFL+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCTMPDETFL+'.CDX')
IF USED(LCTMASNSHP)
   USE IN &lcTmAsnShp
ENDIF
ERASE (oAriaApplication.WORKDIR+LCTMASNSHP+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCTMASNSHP+'.CDX')
IF USED(LCCARTONSZ)
   USE IN &lcCartonSz
ENDIF
ERASE (oAriaApplication.WORKDIR+LCCARTONSZ+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCCARTONSZ+'.CDX')
*!*	IF USED(LCSCAFILE)
*!*	   USE IN &lcScaFile
*!*	ENDIF
*!*	ERASE (oAriaApplication.WORKDIR+LCSCAFILE+'.DBF')
*!*	ERASE (oAriaApplication.WORKDIR+LCSCAFILE+'.CDX')
SELECT (LNACTALIAS)


*:**************************************************************************
*:* Name        : LFCRTNDTL
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  Create Packing list Cartons dtls save Function
*:***************************************************************************
FUNCTION LFCRTNDTL
PRIVATE LNCURALIAS, LNTOTQTY
LNCURALIAS = SELECT(0)
DIMENSION LAFILESTRU[ 5, 4]
LAFILESTRU[ 1, 1] = 'Cart_No'
LAFILESTRU[ 1, 2] = 'N'
LAFILESTRU[ 1, 3] = 4
LAFILESTRU[ 1, 4] = 0
LAFILESTRU[ 2, 1] = 'Pal_No'
LAFILESTRU[ 2, 2] = 'N'
LAFILESTRU[ 2, 3] = 4
LAFILESTRU[ 2, 4] = 0
LAFILESTRU[ 3, 1] = 'TotPcs'
LAFILESTRU[ 3, 2] = 'N'
LAFILESTRU[ 3, 3] = 7
LAFILESTRU[ 3, 4] = 0
LAFILESTRU[ 4, 1] = 'TotWgh'
LAFILESTRU[ 4, 2] = 'N'
LAFILESTRU[ 4, 3] = 9
LAFILESTRU[ 4, 4] = 2
LAFILESTRU[ 5, 1] = 'Empty'
LAFILESTRU[ 5, 2] = 'C'
LAFILESTRU[ 5, 3] = 1
LAFILESTRU[ 5, 4] = 0

LNFILESTRU = ALEN(LAFILESTRU,1)
DIMENSION LAFILESTRU[ LNFILESTRU+1, 4]
LAFILESTRU[ LNFILESTRU+1, 1] = 'cCrtnVlTyp'
LAFILESTRU[ LNFILESTRU+1, 2] = 'C'
LAFILESTRU[ LNFILESTRU+1, 3] = 6
LAFILESTRU[ LNFILESTRU+1, 4] = 0
LNFILESTRU = ALEN(LAFILESTRU,1)
DIMENSION LAFILESTRU[ LNFILESTRU+1, 4]
LAFILESTRU[ LNFILESTRU+1, 1] = 'CCARRCTNID'
LAFILESTRU[ LNFILESTRU+1, 2] = 'C'
LAFILESTRU[ LNFILESTRU+1, 3] = 25
LAFILESTRU[ LNFILESTRU+1, 4] = 0
DIMENSION LAINDX[ 2, 2]
LAINDX[ 1, 1] = 'STR(Cart_No,4)+STR(Pal_No,4)'
LAINDX[ 1, 2] = LCCTNHDR
LAINDX[ 2, 1] = 'Empty+STR(Cart_No,4)+STR(Pal_No,4)'
LAINDX[ 2, 2] = 'EMPTY'
= GFCRTTMP(LCCTNHDR,@LAFILESTRU,@LAINDX)
SET ORDER TO lcCtnHdr IN (LCCTNHDR)
LNI = 1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Cart_No'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 4
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Style'
LAFILESTRU[ LNI, 2] = 'C'
LAFILESTRU[ LNI, 3] = 19
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'nOrdLineNo'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 6
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Size1'
LAFILESTRU[ LNI, 2] = 'C'
LAFILESTRU[ LNI, 3] = 5
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Size2'
LAFILESTRU[ LNI, 2] = 'C'
LAFILESTRU[ LNI, 3] = 5
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Size3'
LAFILESTRU[ LNI, 2] = 'C'
LAFILESTRU[ LNI, 3] = 5
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Size4'
LAFILESTRU[ LNI, 2] = 'C'
LAFILESTRU[ LNI, 3] = 5
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Size5'
LAFILESTRU[ LNI, 2] = 'C'
LAFILESTRU[ LNI, 3] = 5
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Size6'
LAFILESTRU[ LNI, 2] = 'C'
LAFILESTRU[ LNI, 3] = 5
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Size7'
LAFILESTRU[ LNI, 2] = 'C'
LAFILESTRU[ LNI, 3] = 5
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Size8'
LAFILESTRU[ LNI, 2] = 'C'
LAFILESTRU[ LNI, 3] = 5
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Qty1'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 6
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Qty2'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 6
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Qty3'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 6
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Qty4'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 6
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Qty5'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 6
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Qty6'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 6
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Qty7'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 6
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Qty8'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 6
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Weight1'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 9
LAFILESTRU[ LNI, 4] = 2
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Weight2'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 9
LAFILESTRU[ LNI, 4] = 2
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Weight3'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 9
LAFILESTRU[ LNI, 4] = 2
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Weight4'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 9
LAFILESTRU[ LNI, 4] = 2
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Weight5'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 9
LAFILESTRU[ LNI, 4] = 2
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Weight6'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 9
LAFILESTRU[ LNI, 4] = 2
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Weight7'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 9
LAFILESTRU[ LNI, 4] = 2
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Weight8'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 9
LAFILESTRU[ LNI, 4] = 2
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'nStep'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 1
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'PackLineNo'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 6
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'OrgWgh'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 5
LAFILESTRU[ LNI, 4] = 2
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'SzCnt'
LAFILESTRU[ LNI, 2] = 'N'
LAFILESTRU[ LNI, 3] = 1
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'cStatus'
LAFILESTRU[ LNI, 2] = 'C'
LAFILESTRU[ LNI, 3] = 1
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Br1'
LAFILESTRU[ LNI, 2] = 'L'
LAFILESTRU[ LNI, 3] = 1
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Br2'
LAFILESTRU[ LNI, 2] = 'L'
LAFILESTRU[ LNI, 3] = 1
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Br3'
LAFILESTRU[ LNI, 2] = 'L'
LAFILESTRU[ LNI, 3] = 1
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Br4'
LAFILESTRU[ LNI, 2] = 'L'
LAFILESTRU[ LNI, 3] = 1
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Br5'
LAFILESTRU[ LNI, 2] = 'L'
LAFILESTRU[ LNI, 3] = 1
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Br6'
LAFILESTRU[ LNI, 2] = 'L'
LAFILESTRU[ LNI, 3] = 1
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Br7'
LAFILESTRU[ LNI, 2] = 'L'
LAFILESTRU[ LNI, 3] = 1
LAFILESTRU[ LNI, 4] = 0
LNI = ALEN(LAFILESTRU,1)+1
DIMENSION LAFILESTRU[ LNI, 4]
LAFILESTRU[ LNI, 1] = 'Br8'
LAFILESTRU[ LNI, 2] = 'L'
LAFILESTRU[ LNI, 3] = 1
LAFILESTRU[ LNI, 4] = 0
DIMENSION LAINDX[ 2, 2]
LAINDX[ 1, 1] = 'STR(Cart_No,4)+Style+STR(nOrdLineNo,6)'
LAINDX[ 1, 2] = LCCTNDTL
LAINDX[ 2, 1] = 'cStatus'
LAINDX[ 2, 2] = 'Status'
= GFCRTTMP(LCCTNDTL,@LAFILESTRU,@LAINDX)
SET ORDER TO lcCtnDtl IN (LCCTNDTL)

SELECT PACK_LIN
=gfSEEK(&lcTmpPkTk..PIKTKT,'PACK_LIN')
SCAN REST WHILE PACK_NO+STR(LINE_NO,6)+STYLE+CPACKCOLOR =  &lcTmpPkTk..PIKTKT
   SCATTER MEMVAR
   INSERT INTO &lcCtnDtl FROM MEMVAR
   = gfSEEK(PACK_LIN.STYLE,'STYLE')
   = gfSEEK('S'+STYLE.SCALE,'SCALE')
   SELECT &lcCtnDtl
   REPLACE CART_NO WITH NO_CART,;
   		   SIZE1 WITH SCALE.SZ1,;
   		   SIZE2 WITH SCALE.SZ2,;
   		   SIZE3 WITH SCALE.SZ3,;
   		   SIZE4 WITH SCALE.SZ4,;
   		   SIZE5 WITH SCALE.SZ5,;
   		   SIZE6 WITH SCALE.SZ6,;
   		   SIZE7 WITH SCALE.SZ7,;
   		   SIZE8 WITH SCALE.SZ8,;
   		   WEIGHT1 WITH STYLE.NSTYWEIGHT,;
   		   WEIGHT2 WITH STYLE.NSTYWEIGHT,;
   		   WEIGHT3 WITH STYLE.NSTYWEIGHT,;
  		   WEIGHT4 WITH STYLE.NSTYWEIGHT,;
  		   WEIGHT5 WITH STYLE.NSTYWEIGHT,;
  		   WEIGHT6 WITH STYLE.NSTYWEIGHT,;
  		   WEIGHT7 WITH STYLE.NSTYWEIGHT,;
  		   WEIGHT8 WITH STYLE.NSTYWEIGHT,;
  		   ORGWGH WITH STYLE.NSTYWEIGHT,;
  		   SZCNT WITH SCALE.CNT,;
  		   CSTATUS WITH 'M',;
  		   BR1 WITH QTY1>0,;
  		   BR2 WITH QTY2>0,;
  		   BR3 WITH QTY3>0,;
  		   BR4 WITH QTY4>0,;
  		   BR5 WITH QTY5>0,;
  		   BR6 WITH QTY6>0,;
  		   BR7 WITH QTY7>0,;
  		   BR8 WITH QTY8>0
  		   
   LNTOTQTY = QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
   
   IF !SEEK(STR(&lcCtnDtl..CART_NO,4),lcCtnHdr)
      INSERT INTO &lcCtnHdr (Cart_No) VALUES (&lcCtnDtl..Cart_No)
   ENDIF
   SELECT &lcCtnHdr
   REPLACE TOTPCS WITH TOTPCS+LNTOTQTY,;
   		   TOTWGH WITH TOTWGH+LNTOTQTY*STYLE.NSTYWEIGHT
ENDSCAN

*:**************************************************************************
*:* Name        : LFEVALSEGS
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  segment values Function
*:***************************************************************************
FUNCTION  LFEVALSEGS
LCNONMAJPI = ''
LNMAJSEG = GFITEMMASK('SM')
LNMAJLEN = LEN(GFITEMMASK('PM'))
DIMENSION LAMAJSEGS[ 1, 1]
= GFITEMMASK(@LAMAJSEGS)
FOR LNI = LNMAJSEG+1 TO ALEN(LAMAJSEGS,1)
   IF LAMAJSEGS(LNI,1)$'CF'
      LCFREE_CLR = LAMAJSEGS(LNI,1)
      LNNONMAJST = IIF(LNNONMAJST=0 .OR. LAMAJSEGS(LNI,1)='C',LAMAJSEGS(LNI,4),LNNONMAJST)
      LCNONMAJPI = IIF(EMPTY(LCNONMAJPI) .OR. LAMAJSEGS(LNI,1)='C',LAMAJSEGS(LNI,3),LCNONMAJPI+LAMAJSEGS(LNI-1,6)+LAMAJSEGS(LNI,3))
   ENDIF
   IF LAMAJSEGS(LNI,1)='C' .OR. ( .NOT. EMPTY(LCFREE_CLR) .AND. LAMAJSEGS(LNI,1)<>'F')
      EXIT
   ENDIF
ENDFOR
LNCOLORLEN = LEN(LCNONMAJPI)
RETURN ''

*:**************************************************************************
*:* Name        : LFVLBLINFO
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  Label info Function
*:***************************************************************************
FUNCTION  LFVLBLINFO
PARAMETER LNCARTON, LLPRINT
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [Start]
*!*	PRIVATE LNCARTON, LCUCCVER, LAACSHIPTO, LADCSHIPTO, LAWAREADDR, LCSTYLE, LCCOLOR, LCSIZE, LLPRINT
*!*	DIMENSION LAACSHIPTO[ 5], LADCSHIPTO[ 5], LAWAREADDR[ 5], LALBLINFO[ 1, 6]
*!*	STORE '' TO LAACSHIPTO, LADCSHIPTO, LAWAREADDR, LALBLINFO, LCSTYLE, LCCOLOR, LCSIZE, LCCRTNSKU, LCCRTNUPC
PRIVATE LNCARTON, LCUCCVER,  LCSTYLE, LCCOLOR, LCSIZE, LLPRINT
DIMENSION LALBLINFO[ 1, 6]
STORE '' TO LALBLINFO, LCSTYLE, LCCOLOR, LCSIZE, LCCRTNSKU, LCCRTNUPC
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [End]
= LFGETSTYSZ()
M.BOL_NO = ''
M.CPRO_NO = ''
M.CART_NO = LNCARTON
LCWARECODE = ORDHDR.CWARECODE
= gfSEEK(LCWARECODE,'Warehous')
M.VND_NAME = WAREHOUS.CDESC
M.VND_ADDR1 = WAREHOUS.CADDRESS1
M.VND_ADDR2 = WAREHOUS.CADDRESS2
M.VND_CITY = WAREHOUS.CADDRESS3
M.VND_STATE = WAREHOUS.CADDRESS4
M.VND_ZIP = WAREHOUS.CADDRESS5
SELECT CUSTOMER
= gfSEEK(IIF(EMPTY(LADATA(5)),'M'+LADATA(4),'S'+LADATA(4)+LADATA(5)),'Customer')
LCDISTCTR = CUSTOMER.DIST_CTR
STORE LADATA(5) TO M.CSTSTORE, M.STORE
STORE CUSTOMER.STNAME TO M.CSTNAME, M.SHP_NAME
STORE CUSTOMER.CADDRESS1 TO M.CSTADDR1, M.SHP_ADDR1
STORE CUSTOMER.CADDRESS2 TO M.CSTADDR2, M.SHP_ADDR2
STORE CUSTOMER.CADDRESS3 TO M.CSTCITY, M.SHP_CITY
STORE CUSTOMER.CADDRESS4 TO M.CSTSTATE, M.SHP_STATE
STORE CUSTOMER.CADDRESS5 TO M.CSTZIP, M.SHP_ZIP
= gfSEEK('M'+LADATA(4),'Customer')
M.INT_VEND = CUSTOMER.CCUSVEND
IF gfSEEK('S'+LADATA(4)+LCDISTCTR,'Customer')
   M.STORE = LCDISTCTR
   M.SHP_NAME = CUSTOMER.STNAME
   M.SHP_ADDR1 = CUSTOMER.CADDRESS1
   M.SHP_ADDR2 = CUSTOMER.CADDRESS2
   M.SHP_CITY = CUSTOMER.CADDRESS3
   M.SHP_STATE = CUSTOMER.CADDRESS4
   M.SHP_ZIP = CUSTOMER.CADDRESS5
ENDIF
M.SHIPVIA = ORDHDR.SHIPVIA
LCWRNAME = LCWARECODE
SELECT ORDHDR
gfSetOrder('ORDHDR')
= gfSEEK('O'+LADATA(2))
LCORSTORE = ORDHDR.STORE
LCUCCVER = ''
IF LLEDISYS .AND. LLEDIACC
   LCUCCVER = IIF(LNDRCTTO=1,EDIPH.CASNLBL1,EDIPH.CASNLBL2)
   M.LPLT = EDIPH.LPLTSHP
ENDIF
LCUCCVER = IIF(EMPTY(LCUCCVER),'XXX',LCUCCVER)
M.CUSTPO = ORDHDR.CUSTPO
M.DEPT = ORDHDR.DEPT
M.NOTE1 = ORDHDR.NOTE1
M.NOTE2 = ORDHDR.NOTE2
M.INT_VEND = IIF(EMPTY(ORDHDR.INT_VEND),M.INT_VEND,ORDHDR.INT_VEND)
M.CANCELLED = ORDHDR.COMPLETE
M.EVENT_COD = ORDHDR.EVENT_COD
LCMANUFID = gfGetMemVar('XMANUFID')
M.MANUF_ID = PADL(ALLTRIM(LCMANUFID),7,'0')
M.UCC9 = RIGHT(PADL(ALLTRIM(LADATA(1)),6,'0'),5)+PADL(LNCARTON,4,'0')
M.UCC_CHECK = LFCHECKNO('000'+M.MANUF_ID+M.UCC9)
M.ASN_VER = LCUCCVER
IF (LCFREE_CLR<>'C') .OR. EMPTY(LCCOLOR) .OR. ('MIXED'$LCCOLOR)
   DIMENSION LACODDESC[ 1, 3]
   LACODDESC = ''
   LACODDESC[ 1, 1] = M.SHIPVIA
   LACODDESC[ 1, 2] = 'SHIPVIA'
   M.CCLRDESC = LCCOLOR
ELSE
   DIMENSION LACODDESC[ 2, 3]
   LACODDESC = ''
   LACODDESC[ 1, 1] = M.SHIPVIA
   laCodDesc[2,1] = SUBSTR(&lcCtnDtl..Style,lnNonMajSt,lnColorLen)
   LACODDESC[ 1, 2] = 'SHIPVIA'
   LACODDESC[ 2, 2] = 'COLOR'
   = GFCODDES(@LACODDESC)
   M.CCLRDESC = LACODDESC(2,1)
ENDIF
= GFCODDES(@LACODDESC)
M.PACK_NO = IIF(EMPTY(LADATA(1)),LADATA(3),LADATA(1))
M.CARRIER = LACODDESC(1,3)
M.BOL_NO = ''
M.STYLE = LCSTYLE
= SEEK(STR(M.CART_NO,4),LCCTNHDR)
m.TotQty    = &lcCtnHdr..TotPcs
M.CSIZEDESC = LCSIZE
M.CUPC = LCCRTNUPC
M.PACK_ID = LCCRTNSKU
LCDETORDER = ORDER(LCCTNHDR)
USE IN 0 (oAriaApplication.WORKDIR+LCCTNHDR) AGAIN ALIAS (LCTMPDETFL) ORDER (LCDETORDER)
GOTO IN (LCTMPDETFL) BOTTOM
m.Cartons = &lcTmpDetFl..Cart_No
USE IN (LCTMPDETFL)
SELECT (LCTMASNSHP)
IF SEEK(STR(M.CART_NO,6),LCTMASNSHP)
   GATHER MEMVAR MEMO
ELSE
   INSERT INTO (LCTMASNSHP) FROM MEMVAR
ENDIF
= GFADD_INFO(LCTMASNSHP)
IF TYPE('laLblInfo[1,4]')='C' .AND.  .NOT. EMPTY(LALBLINFO(1,4))
   SAVE TO MEMO MLBLINFO ALL LIKE laLblInfo
ENDIF
IF LLPRINT
   SELECT SYCASNHD
   =gfSeek("")
   GOTO TOP
   LOCATE FOR CVER=LCUCCVER .AND. CTYPE='Y'
   IF FOUND()
      SELECT (LCTMASNSHP)
      = LFVSULLBL(loFormSet,LCUCCVER,ALLTRIM(STR(M.CART_NO)))
   ELSE
      LOCATE FOR CVER=LCUCCVER .AND. CTYPE='N'
      IF FOUND()
         SELECT (LCTMASNSHP)
         = LFPRINTLBL(loFormSet,LCUCCVER)
      ENDIF
   ENDIF
   IF loFormSet.llntfound
      RETURN
   ENDIF
ENDIF
SELECT (LCPCKLIN)


*:**************************************************************************
*:* Name        : LFGETSTYSZ
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  get style sizes Function
*:***************************************************************************
FUNCTION  LFGETSTYSZ
PRIVATE LCCURRALIS, LNRECPOINT, LNDETPOINT
LCCURRALIS = SELECT(0)
SELECT (LCCTNDTL)
LNRECPOINT = RECNO()
LNDETPOINT = RECNO(LCCARTONSZ)
LCSETSKIP = SET('SKIP')
SET SKIP TO
SEEK STR(LNCARTON,4)
LCSTYLE = SUBSTR(STYLE,1,LNMAJLEN)
LCUPDSTY = STYLE
IF LCFREE_CLR='C'
   LCCOLOR = SUBSTR(STYLE,LNNONMAJST,LNCOLORLEN)
ELSE
   LCCOLOR = SPACE(6)
ENDIF
LNLBLINFO = 0
DIMENSION LALBLINFO[ 1, 6]
LCCRTNSKU = ''
LCCRTNUPC = ''
LCSIZE = ''
SCAN REST WHILE STR(CART_NO,4)=STR(LNCARTON,4)
   LCSTYLE = IIF(LCSTYLE<>SUBSTR(STYLE,1,LNMAJLEN),SPACE(LNMAJLEN),LCSTYLE)
   LCUPDSTY = IIF(LCUPDSTY<>STYLE,SPACE(19),LCUPDSTY)
   IF LCFREE_CLR='C'
      LCCOLOR = IIF(EMPTY(LCSTYLE),SPACE(LNCOLORLEN),IIF(LCCOLOR<>SUBSTR(STYLE,LNNONMAJST,LNCOLORLEN),'MIXED',LCCOLOR))
   ENDIF
   FOR LNSZNO1 = 1 TO 8
      LCSZNO = STR(LNSZNO1,1)
      IF Br&lcSzNO
         LCSKU = SPACE(16)
         IF gfSEEK('S'+laData[4] + &lcCtnDtl..Style, 'Spck_Lin')
            SELECT SPCK_LIN
            LOCATE REST  WHILE Type + Account + Style = 'S' + laData[4] + &lcCtnDtl..Style FOR Qty&lcSzNo = 1
            IF FOUND()
               LCSKU = PADR(PACK_ID,16)
            ELSE
               =gfSEEK('S' + laData[4] + &lcCtnDtl..Style, 'Spck_Lin')
               LOCATE REST WHILE Type + Account + Style = 'S' + laData[4] + &lcCtnDtl..Style FOR TotQty = 0
               IF FOUND()
                  LCSKU = PADR(PACK_ID,16)
               ENDIF
            ENDIF
         ENDIF
         LCUPC = SPACE(13)
         IF llUPCInst AND gfSEEK(&lcCtnDtl..Style+lcSzNo,'StyleUpc')
            LCUPC = STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3
         ENDIF
         SELECT (LCCTNDTL)
         LNLBLINFO = LNLBLINFO+1
         DIMENSION LALBLINFO[ LNLBLINFO, 6]
         LALBLINFO[ LNLBLINFO, 1] = LCSKU
         laLblInfo[lnLblInfo,2] = Qty&lcSzNo
         laLblInfo[lnLblInfo,3] = Size&lcSzNo
         LALBLINFO[ LNLBLINFO, 4] = STYLE
         LALBLINFO[ LNLBLINFO, 5] = LCUPC
         LALBLINFO[ LNLBLINFO, 6] = LNSZNO1
         lcSize    = IIF(Size&lcSzNo <> lcSize   ,'MIXED',Size&lcSzNo)
         LCCRTNSKU = IIF(LCSKU<>LCCRTNSKU,SPACE(16),LCSKU)
         LCCRTNUPC = IIF(LCUPC<>LCCRTNUPC,SPACE(13),LCUPC)
      ENDIF
   ENDFOR
ENDSCAN
LCSIZE = IIF(EMPTY(LCSTYLE),SPACE(5),LCSIZE)
IF  .NOT. EMPTY(LCSETSKIP)
   SET SKIP TO (LCSETSKIP)
ENDIF
IF BETWEEN(LNRECPOINT,1,RECCOUNT())
   GOTO LNRECPOINT
ENDIF
IF BETWEEN(LNDETPOINT,1,RECCOUNT(LCCARTONSZ))
   GOTO IN (LCCARTONSZ) LNDETPOINT
ENDIF
SELECT (LCCURRALIS)

*:**************************************************************************
*:* Name        : LFCHECKNO
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  Check No. Function
*:***************************************************************************
FUNCTION  LFCHECKNO
PARAMETER LCUCCNO
PRIVATE LNCHKDIGIT, LNSUMODD, LNSUMEVEN, LNCOUNT
STORE 0 TO LNSUMODD, LNSUMEVEN, LNCHKDIGIT
FOR LNCOUNT = 1 TO 9
   LNSUMODD = LNSUMODD+VAL(SUBSTR(LCUCCNO,LNCOUNT*2-1,1))
   LNSUMEVEN = LNSUMEVEN+VAL(SUBSTR(LCUCCNO,LNCOUNT*2,1))
ENDFOR
LNSUMODD = LNSUMODD+VAL(SUBSTR(LCUCCNO,19,1))
LNCHKDIGIT = MOD(LNSUMODD*3+LNSUMEVEN,10)
RETURN (IIF(LNCHKDIGIT=0,'0',STR(INT(10-LNCHKDIGIT),1)))


  *:**************************************************************************
  *:* Name        : lfVsulLbl
  *! Developer : Mariam Mazhar
  *! Date      : 01/15/2009
  *:* Purpose     : Calling  Visual UCC128 Label Report E302315
  *:***************************************************************************
  *:* Called from : lfSavCartn  and lfvLblInfo
  *:***************************************************************************
  *:* Parameters : loFormSet,lcVersion && Label Version
  *:***************************************************************************
  *:* Return      : None
  *:***************************************************************************
  *:* Example     :  = lfVsulLbl()
  *:***************************************************************************
FUNCTION lfVsulLbl
  PARAMETERS loFormSet,lcVersion, lcCartons

  STORE 0 TO lnChoiceDtl
  lcAccount  = PADR(LADATA(4),5)
  IF llEdiAcc
    *-- Check for the detail Version for current account.
    IF gfSEEK('A' + lcAccount,'EDIACPRT') .AND.;
        gfSEEK(EDIACPRT.cPartCode,'EDIPH') .AND. ;
        Ediph.lDtlbl
      loFormSet.llDetLabel = .T.
      loFormSet.lcDetailVr = Ediph.cDtlbl
    ELSE
      loFormSet.llDetLabel = .F.
    ENDIF
  ELSE
    *- Function to get the number of XX? into array.
    STORE '' TO loFormSet.lcDetailVr , loFormSet.lcDetLbAll
    DIMENSION laVersn[1]
    lnVerCount = lfGetVerXX(loFormSet)
    loFormSet.llDetLabel = (lnVerCount >= 1)
  ENDIF
  IF !llEdiAcc .AND. lnVerCount >= 1
    *-- the customer should print one from the current version.
    = lfSeleVer(loFormSet,laVersn)
  ENDIF
  *Print the detailed label, if needed [Begin]

  STORE .F. TO llPrintLbl
  IF loFormSet.llDetLabel
    IF EMPTY(loFormSet.lcDetLbAll)
      lnChoiceDtl = gfModalGen('TRM44105B40016' , 'DIALOG' , lcCartons)

      DO CASE
        CASE lnChoiceDtl = 1
          llPrintLbl = .T.
        CASE lnChoiceDtl = 2
          llPrintLbl = .T.
          loFormSet.lcDetLbAll = "Y"
        CASE lnChoiceDtl = 3
          llPrintLbl = .F.
        CASE lnChoiceDtl = 4
          llPrintLbl = .F.
          loFormSet.lcDetLbAll = "N"
      ENDCASE
    ELSE
      llPrintLbl = (loFormSet.lcDetLbAll = "Y")
    ENDIF
  ENDIF

  SELECT (lcTmAsnShp)
  lnEdiRecNo = RECNO()
  
  COPY TO (oAriaApplication.WorkDir+lcPrnAsnShp)
  SELECT 0
  USE (oAriaApplication.WorkDir+lcPrnAsnShp) EXCLUSIVE
  INDEX ON bol_no+pack_no+STR(cart_no,6)+asn_ver TAG (lcPrnAsnShp)
  USE IN (lcPrnAsnShp)

  tcEDIAct   = lcAccount
  tcEDIShp   = lcPrnAsnShp
  tcEDICmp   = oAriaApplication.ActiveCompanyID
  tcEDIPrtNm = lcSndPort
  tcEDIBolNo = Bol_No
  tcEDIPckNo = Pack_No
  tnEDICrtNo = lcCartons
  tcEDIVer   = lcVersion
  tlEDIDetLb = llPrintLbl
  tcEDIDetVr = IIF(llPrintLbl,loFormSet.lcDetailVr,'')
  lcEdiPath = ALLTRIM(UPPER(SUBSTR(oariaapplication.syspath,1,AT('\',oariaapplication.syspath,2))))
  lcEdiPath  = SUBSTR(lcEdiPath,1,LEN(lcEdiPath)-1)
  lcAria4XPPath = ALLTRIM(UPPER(SUBSTR(oAriaApplication.ApplicationHome,1,AT('\',oAriaApplication.ApplicationHome,2))))
  lcAria4XPPath = SUBSTR(lcAria4XPPath,1,LEN(lcAria4XPPath)-1)

  IF (oAriaApplication.ClassDir+'EDI.VCX' $ SET('classlib'))
    RELEASE CLASSLIB (oAriaApplication.ClassDir+'EDI.VCX')
  ENDIF
  lcoldAppHome = oAriaApplication.ApplicationHome
  lcoldAppRep  = oAriaApplication.ReportHome
  lcoldAppBitMapHome = oAriaApplication.BitMapHome
  lcoldAppCls = oAriaApplication.ClassDir
  lcoldAppScx = oAriaApplication.ScreenHome
  lcEdiPath = UPPER(SUBSTR(oariaapplication.syspath,1,AT('\',oariaapplication.syspath,2)))
  oAriaApplication.ApplicationHome = lcEdiPath +  'PRGS\'
  oAriaApplication.ReportHome = lcEdiPath +  'REPORTS\'
  oAriaApplication.BitMapHome = lcEdiPath +  'BMPs\'
  oAriaApplication.ClassDir   = lcEdiPath +  'CLASSES\'
  oAriaApplication.ScreenHome = lcEdiPath +  'SCREENS\'

  SET CLASSLIB TO (oAriaApplication.ClassDir +'EDI.VCX') ADDIT
  SET PROCEDURE TO (oAriaApplication.ApplicationHome +'EDIGLOBL.FXP') ADDITIV


  oPrnLabel=CREATEOBJECT('PrnLabel',.T.,ALLTRIM(lcAria4XPPath),ALLTRIM(tcEDIAct),ALLTRIM(tcEDIShp) ,;
    ALLTRIM(tcEDIPrtNm), ALLTRIM(tcEDIBolNo), ALLTRIM(tcEDIPckNo),tnEDICrtNo)

  oPrnLabel.DO(.T.,,ALLTRIM(lcAria4XPPath),ALLTRIM(tcEDIAct),ALLTRIM(tcEDIShp) ,;
    ALLTRIM(tcEDIPrtNm), ALLTRIM(tcEDIBolNo), ALLTRIM(tcEDIPckNo),tnEDICrtNo,ALLTRIM(tcEDIVer), tlEDIDetLb, ;
    IIF(tlEDIDetLb,ALLTRIM(tcEDIDetVr),''))
  RELEASE oPrnLabel

  oAriaApplication.ApplicationHome = lcoldAppHome
  oAriaApplication.ReportHome      = lcoldAppRep
  oAriaApplication.BitMapHome      = lcoldAppBitMapHome
  oAriaApplication.ClassDir        = lcoldAppCls
  oAriaApplication.ScreenHome      = lcoldAppScx

  IF USED(lcPrnAsnShp)
    USE IN (lcPrnAsnShp)
    ERASE (oAriaApplication.WorkDir+lcPrnAsnShp+'.*')
  ENDIF

  SELECT (lcTmAsnShp)
  GOTO lnEdiRecNo

  WAIT CLEAR
  *!*************************************************************
  *! Name      : lfSeleVer
  *! Developer : Mariam Mazhar
  *! Date      : 01/15/2009
  *! Purpose   : Function to get the Version types
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  *! Example   :  =lfSeleVer()
  *!*************************************************************
  FUNCTION lfSeleVer
  LPARAMETERS loFormSet,laVersn

  PRIVATE lnPRvAlias

  lnPRvAlias = SELECT(0)

  DO FORM (oAriaApplication.ScreenHome+'ALDtVer.SCX') WITH loFormSet,laVersn

  SELECT(lnPRvAlias)
  *-- End OF lfSeleVer
  *!*************************************************************
  *! Name      : lfGetVerXX
  *! Developer : Mariam Mazhar
  *! Date      : 01/15/2009
  *! Purpose   : Function to get the number of XX? into array.
  *!*************************************************************
  *! Parameters: None.
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  *! Example   :  =lfGetVerXX()
  *!*************************************************************
  FUNCTION lfGetVerXX
  LPARAMETERS loFormSet

  PRIVATE lnPRvAlias , lnReturnVr
  lnReturnVr = 0
  lnPRvAlias = SELECT(0)

  SELECT SYCASNLB
  =gfSeek("")
  SELECT DIST cVer FROM sycasnlb ;
    WHERE LEFT(Cver,2) = 'XX' .AND. Cver # 'XXX';
    INTO ARRAY laVersn

  lnReturnVr = ALEN(laVersn,1)
  *-- if the customer have one version it should print it direct.
  IF lnReturnVr >= 1
    loFormSet.lcDetailVr = laVersn[1]
  ENDIF

  SELECT(lnPRvAlias)

  RETURN lnReturnVr

  *-- End Of lfGetVerXX

  *!*************************************************************
  *! Name      : lfPrintLbl
  *! Developer : Mariam Mazhar
  *! Date      : 01/15/2009
  *! Purpose   : Print carton labels
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   :
  *!*************************************************************
  FUNCTION lfPrintLbl
  LPARAMETERS loFormSet , lcVersion
  PRIVATE lnCurAlias, lnHandle, lcOutFile, lcString, lcData
  lnCurAlias = SELECT(0)
  lcString  = SPACE(40)
  SELECT SYCASNLB
  IF !gfSEEK(lcVersion+'H','SYCASNLB') .AND. gfModalGen('INM44068B00000','DIALOG' )=1
    loFormSet.llntfound = .T.
    RETURN
  ENDIF
  lcOutFile = oAriaApplication.WorkDir+lcAsnLabel+".TXT"
  lnHandle  = FCREATE(lcOutFile,0)
  =FSEEK(lnHandle,0,2)

  SCAN WHILE 	cVer+cEdiType = lcVersion+'H'
    STORE DATA TO lcData
    lcString = &lcData
    =FPUTS(lnHandle,lcString)
  ENDSCAN

  lcString = SPACE(3)
  =FPUTS(lnHandle,lcString)


  SELECT (loFormSet.lcTmAsnShp)
  SCATTER MEMVAR

  MUCB = PADL(ALLTRIM(lfManufID(loFormSet,m.Bol_No)) , 7 , '0') + PADL(ALLTRIM(m.Bol_No) , 9 , '0')
  MUCB = MUCB + lfCheckDgt(MUCB,'E')

  DECLARE laDRltFld[1,2]
  laDRltFld[1,1] = 'DIVLNAME  '
  laDRltFld[1,2] = 'lcDivLName'
  lcDivLName = ''
  =gfRltFld(ORDHDR.cDivision,@laDRltFld,'CDIVISION ')

  m.DivLName = lcDivLName
  =gfSEEK(OrdHdr.Account+OrdHdr.Dept,'CUSTDEPT')
  m.DeptDesc = CustDept.cDeptDesc
  =gfSEEK(ALLTRIM(STYLE),'STYLE')
  m.StyDesc = STYLE.DESC

  =SEEK(STR(EVALUATE(lcTmAsnShp+'.cart_no'),4),lcCtnHdr)
  m.Weight = EVALUATE(lcCtnHdr+'.TotWgh')


  SELECT (lcTmAsnShp)
  m.Date     = oAriaApplication.SystemDate

  m.Order    = PADR(laData[2],6)
  m.Account  = PADR(LADATA[4],5)
  m.Pattern  = STYLE.PATTERN


  STORE '' TO m.SizeDesc1,m.SizeDesc2,m.SizeDesc3,m.SizeDesc4,m.SizeDesc5,m.SizeDesc6,m.SizeDesc7,m.SizeDesc8
  STORE '' TO m.SizeSku1,m.Sizesku2,m.Sizesku3,m.Sizesku4,m.Sizesku5,m.Sizesku6,m.Sizesku7,m.Sizesku8
  STORE 0  TO m.SizeQty1,m.SizeQty2,m.SizeQty3,m.SizeQty4,m.SizeQty5,m.SizeQty6,m.SizeQty7,m.SizeQty8


  SELECT(lcCtnDtl)
  lnStyRec = RECNO(lcCtnDtl)
  SET RELATION TO
  IF loFormSet.llExtSizSc
    SELECT (lcCtnDtl)
    IF SEEK(STR(EVALUATE(lcTmAsnShp+'.cart_no'),4)+ALLTRIM(EVALUATE(lcTmAsnShp+'.Style')))
      lnSizeCount = 0
      lcCurrSty = SUBSTR(STYLE,1,loFormSet.lnSizePos-IIF(EMPTY(loFormSet.lcSizeSep),1,2))+Dyelot
      SCAN REST WHILE STR(Cart_No,4)+STYLE+Dyelot+STR(nOrdLineNo,6) = STR(EVALUATE(lcTmAsnShp+'.cart_no'),4)+lcCurrSty ;
          AND lnSizeCount < 8
        gfSEEK('S'+SUBSTR(STYLE,loFormSet.lnSizePos,loFormSet.lnSizeLen),'Scale')
        lcSizeNo = EVALUATE(lcCtnDtl+'.cSizeNo')
        IF EVALUATE(lcCtnDtl+'.Qty') <> 0
          lnSizeCount = lnSizeCount + 1
          lcSizeCount = STR(lnSizeCount,1)
          m.SizeDesc&lcSizeCount = ALLTRIM(SCALE.Sz&lcCount)
          m.SizeQty&lcSizeCount  = EVALUATE(lcCtnDtl+'.Qty')
          SELECT SPCK_LIN
          gfSetOrder('Spklnstcn')
          gfSEEK('S'+m.Account+EVALUATE(lcCtnDtl+'.style')+EVALUATE(lcCtnDtl+'.Dyelot'),'SPCK_LIN')
          LOCATE REST WHILE TYPE+account+STYLE+Dyelot+pack_id = 'S'+m.Account+EVALUATE(lcCtnDtl+'.style')+EVALUATE(lcCtnDtl+'.Dyelot') FOR QTY&lcSizeNo= 1
          IF FOUND()
            m.SizeSku&lcSizeCount = ALLTRIM(Spck_Lin.Pack_Id)
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF
  ELSE
    SELECT (lcTmAsnShp)
    gfSEEK('S'+STYLE.SCALE,'SCALE')
    IF SEEK(STR(Cart_no,4)+ALLTRIM(STYLE),lcCtnDtl)
      lcCurrSty = ALLTRIM(EVALUATE(lcCtnDtl+'.Style'))+EVALUATE(lcCtnDtl+'.Dyelot')
      SELECT (lcCtnDtl)
      lcSizeNo = EVALUATE(lcCtnDtl+'.cSizeNo')
      lnSizeCount = VAL(lcSizeNo)
      SCAN REST WHILE STR(Cart_No,4)+STYLE+Dyelot+STR(nOrdLineNo,6) = STR(EVALUATE(loFormSet.lcTmAsnShp+'.cart_no'),4)+lcCurrSty ;
          AND lnSizeCount < 8
        lcSizeNo = EVALUATE(lcCtnDtl+'.cSizeNo')
        lnSizeCount = VAL(lcSizeNo)
        IF EVALUATE(lcCtnDtl+'.Qty') <> 0
          m.SizeDesc&lcSizeNo= ALLTRIM(SCALE.Sz&lcSizeNo)
          m.SizeQty&lcSizeNo= EVALUATE(lcCtnDtl+'.Qty')
          SELECT SPCK_LIN
          gfSetOrder('Spklnstcn')
          gfSEEK('S'+m.Account+EVALUATE(lcCtnDtl+'.style')+EVALUATE(lcCtnDtl+'.Dyelot'),'SPCK_LIN')
          LOCATE REST WHILE TYPE+account+STYLE+Dyelot+pack_id = 'S'+m.Account+EVALUATE(lcCtnDtl+'.style')+EVALUATE(lcCtnDtl+'.Dyelot') FOR QTY&lcSizeNo= 1
          IF FOUND()
            m.SizeSku&lcSizeNo = ALLTRIM(Spck_Lin.Pack_Id)
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
  SELECT (lcTmAsnShp)

  TMP_ADDR = ALLTRIM(VND_CITY) + ",  " +ALLTRIM(VND_STATE) + "   " + ALLTRIM(VND_ZIP)
  SELECT SYCASNLB
  gfSEEK(lcVersion+'L','SYCASNLB')
  SCAN WHILE cVer+cEdiType= lcVersion+'L'
    STORE DATA TO lcData
    lcString = &lcData
    =FPUTS(lnHandle,lcString)
  ENDSCAN
  lcString = SPACE(3)
  =FPUTS(lnHandle,lcString)


  IF llEdiAcc
    *-- Check for the detail Version for current account.
    IF gfSEEK('A' + m.Account,'EDIACPRT') .AND.;
        gfSEEK(EDIACPRT.cPartCode,'EDIPH') .AND. ;
        Ediph.lDtlbl
      loFormSet.llDetLabel = .T.
      loFormSet.lcDetailVr = Ediph.cDtlbl
    ELSE
      loFormSet.llDetLabel = .F.
    ENDIF
  ELSE
    *- Function to get the number of XX? into array.
    STORE '' TO loFormSet.lcDetailVr , loFormSet.lcDetLbAll
    DIMENSION laVersn[1]
    lnVerCount = lfGetVerXX(loFormSet)
    loFormSet.llDetLabel = (lnVerCount >= 1)
  ENDIF
  IF loFormSet.llDetLabel
    PRIVATE lnChoice , llPrintLbl , lnMajorLen , lnClrLen , lnClrPos , llUseColor ,;
      lcUPCStyle , lcGenColor , lcGenSty

    IF EMPTY(loFormSet.lcDetLbAll)

      lnChoice = gfModalGen('TRM44105B40016' , 'DIALOG' , ALLTRIM(STR(EVALUATE(lcTmAsnShp+'.Cart_No') , 4)))
      DO CASE
        CASE lnChoice = 1
          llPrintLbl = .T.
        CASE lnChoice = 2
          llPrintLbl = .T.
          loFormSet.lcDetLbAll = "Y"
        CASE lnChoice = 3
          llPrintLbl = .F.
        CASE lnChoice = 4
          llPrintLbl = .F.
          loFormSet.lcDetLbAll = "N"
      ENDCASE
    ELSE
      llPrintLbl = (loFormSet.lcDetLbAll = "Y")
    ENDIF

    IF llPrintLbl

      IF !llEdiAcc .AND. lnVerCount >= 1
        *-- the customer should print one from the current version.
        = lfSeleVer(loFormSet,laVersn)
      ENDIF

      STORE '' TO MDSTYLE1 , MDSTYLE2 , MDSTYLE3 , MDSTYLE4 , MDSTYLE5 ,;
        MDSTYLE6 , MDSTYLE7 , MDSTYLE8 , MDSTYLE9 , MDSTYLE10,;
        MDSTYLE11, MDSTYLE12, MDSTYLE13, MDSTYLE14, MDSTYLE15,;
        MDSTYLE16, MDSTYLE17, MDSTYLE18, MDSTYLE19, MDSTYLE20

      STORE '' TO MDSTYMAJ1 , MDSTYMAJ2 , MDSTYMAJ3 , MDSTYMAJ4 , MDSTYMAJ5 ,;
        MDSTYMAJ6 , MDSTYMAJ7 , MDSTYMAJ8 , MDSTYMAJ9 , MDSTYMAJ10,;
        MDSTYMAJ11, MDSTYMAJ12, MDSTYMAJ13, MDSTYMAJ14, MDSTYMAJ15,;
        MDSTYMAJ16, MDSTYMAJ17, MDSTYMAJ18, MDSTYMAJ19, MDSTYMAJ20

      STORE '' TO MDCOLOR1 , MDCOLOR2 , MDCOLOR3 , MDCOLOR4 , MDCOLOR5 ,;
        MDCOLOR6 , MDCOLOR7 , MDCOLOR8 , MDCOLOR9 , MDCOLOR10,;
        MDCOLOR11, MDCOLOR12, MDCOLOR13, MDCOLOR14, MDCOLOR15,;
        MDCOLOR16, MDCOLOR17, MDCOLOR18, MDCOLOR19, MDCOLOR20

      STORE '' TO MDSKU1 , MDSKU2 , MDSKU3 , MDSKU4 , MDSKU5 ,;
        MDSKU6 , MDSKU7 , MDSKU8 , MDSKU9 , MDSKU10,;
        MDSKU11, MDSKU12, MDSKU13, MDSKU14, MDSKU15,;
        MDSKU16, MDSKU17, MDSKU18, MDSKU19, MDSKU20

      STORE '' TO MDSTYUPC1 , MDSTYUPC2 , MDSTYUPC3 , MDSTYUPC4 , MDSTYUPC5 ,;
        MDSTYUPC6 , MDSTYUPC7 , MDSTYUPC8 , MDSTYUPC9 , MDSTYUPC10,;
        MDSTYUPC11, MDSTYUPC12, MDSTYUPC13, MDSTYUPC14, MDSTYUPC15,;
        MDSTYUPC16, MDSTYUPC17, MDSTYUPC18, MDSTYUPC19, MDSTYUPC20

      STORE '' TO MDSIZDES1 , MDSIZDES2 , MDSIZDES3 , MDSIZDES4 , MDSIZDES5 ,;
        MDSIZDES6 , MDSIZDES7 , MDSIZDES8 , MDSIZDES9 , MDSIZDES10,;
        MDSIZDES11, MDSIZDES12, MDSIZDES13, MDSIZDES14, MDSIZDES15,;
        MDSIZDES16, MDSIZDES17, MDSIZDES18, MDSIZDES19, MDSIZDES20

      STORE 0  TO MDQTY1 , MDQTY2 , MDQTY3 , MDQTY4 , MDQTY5 ,;
        MDQTY6 , MDQTY7 , MDQTY8 , MDQTY9 , MDQTY10,;
        MDQTY11, MDQTY12, MDQTY13, MDQTY14, MDQTY15,;
        MDQTY16, MDQTY17, MDQTY18, MDQTY19, MDQTY20

      *-- Get major length
      lnMajorLen = LEN(loFormSet.lcMask)

      *-- Get color start position and length
      STORE 0   TO lnClrLen , lnClrPos
      STORE .F. TO llUseColor
      DECLARE laItemSeg[1]
      =gfItemMask(@laItemSeg)
      FOR lnCount = 1 TO ALEN(laItemSeg , 1)
        IF laItemSeg[lnCount,1] = 'C'
          llUseColor = .T.
          lnClrLen   = LEN(laItemSeg[lnCount,3])
          lnClrPos   = laItemSeg[lnCount,4]
          EXIT
        ENDIF
      ENDFOR

      lcGenColor = PADR(gfGetMemVar("MCLRASSCOD",oAriaApplication.ActiveCompanyID) , 6)

      SELECT (lcCtnDtl)
      IF SEEK(STR(EVALUATE(lcTmAsnShp+'.Cart_No') , 4))
        lnSizeCount = 0
        SCAN REST WHILE STR(Cart_No , 4) = STR(EVALUATE(lcTmAsnShp+'.Cart_No') , 4) .AND. lnSizeCount < 20
          gfSEEK(EVALUATE(lcCtnDtl+'.Style'),'STYLE')
          gfSEEK('S' + STYLE.SCALE,'SCALE')

          IF loFormSet.llUPCInst
            lcUPCStyle = EVALUATE(lcCtnDtl+'.Style')
            IF llUseColor
              lcGenSty = SUBSTR(lcUPCStyle , 1 , lnClrPos - 1) + lcGenColor +;
                SUBSTR(lcUPCStyle , lnClrPos + lnCLrLen)
            ENDIF
          ENDIF

          lcCount = EVALUATE(lcCtnDtl+'.cSizeNo')
          IF lnSizeCount < 20 .AND. EVALUATE(lcCtnDtl+'.Qty') <> 0
            lnSizeCount = lnSizeCount + 1
            lcSizeCount = ALLTRIM(STR(lnSizeCount,2))

            MDStyle&lcSizeCount  = EVALUATE(lcCtnDtl+'.Style')
            MDStyMaj&lcSizeCount = LEFT(EVALUATE(lcCtnDtl+'.Style'), lnMajorLen)
            MDSizDes&lcSizeCount = ALLTRIM(SCALE.Sz&lcCount)
            MDQty&lcSizeCount    = EVALUATE(lcCtnDtl+'.Qty')

            IF llUseColor
              MDColor&lcSizeCount = SUBSTR( EVALUATE(lcCtnDtl+'.Style') , lnClrPos , lnClrLen)
            ENDIF

            IF loFormSet.llUPCInst
              SELECT 'STYLEUPC'
              gfSetOrder('STYLEUPC')
              IF gfSEEK(lcUPCStyle + lcCount ,'StyleUPC')
                MDStyUPC&lcSizeCount = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
              ELSE
                IF llUseColor .AND. gfSEEK(lcGenSty + lcCount,'StyleUPC')
                  MDStyUPC&lcSizeCount = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 +;
                    StyleUPC.cUPCNum3
                ENDIF
              ENDIF
            ENDIF

            SELECT SPCK_LIN
            gfSetOrder('Spklnstcn')
            gfSEEK('S' + m.Account + EVALUATE(lcCtnDtl+'.Style')+EVALUATE(lcCtnDtl+'.Dyelot'),'SPCK_LIN')
            LOCATE REST;
              WHILE TYPE + Account + STYLE + Pack_ID = 'S' + m.Account + EVALUATE(lcCtnDtl+'.Style')+EVALUATE(lcCtnDtl+'.Dyelot');
              FOR QTY&lcCount = 1

            IF FOUND()
              MDSKU&lcSizeCount = ALLTRIM(Spck_Lin.Pack_Id)
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF

      SELECT SYCASNLB
      *--- collect data for the selected Version.
      gfSEEK(loFormSet.lcDetailVr + 'H','SYCASNLB')
      SCAN WHILE cVer + cEdiType = loFormSet.lcDetailVr + 'H'
        STORE DATA TO lcData
        lcString = &lcData
        =FPUTS(lnHandle,lcString)
      ENDSCAN
      lcString = SPACE(3)
      =FPUTS(lnHandle,lcString)

      gfSEEK(loFormSet.lcDetailVr  + 'L','SYCASNLB')
      SCAN WHILE cVer + cEdiType = loFormSet.lcDetailVr + 'L'
        STORE DATA TO lcData
        lcString = &lcData
        =FPUTS(lnHandle,lcString)
      ENDSCAN
    ENDIF
  ENDIF

  =FCLOSE(lnHandle)
  lcCommand = "TYPE " + lcOutFile + " > " + lcSndPort
  ! &lcCommand

  WAIT CLEAR

  SELECT (lcCtnDtl)
  IF lnStyRec <= RECCOUNT(lcCtnDtl)
    GOTO lnStyRec
  ENDIF
  = RLOCK(lcCtnDtl)
  UNLOCK IN (lcCtnDtl)

  SELECT(lnCurAlias)
  RETURN
  *-- End Of Function lfPrintLbl


  *!*************************************************************
  *! Name      : lfManufID
  *! Developer : Mariam Mazhar
  *! Date      : 01/15/2009
  *! Purpose   : validate pbDtlNew button
  *!*************************************************************
  *! Example   : = lfManufID(Bill of lading no)
  *!*************************************************************
FUNCTION lfManufID
  LPARAMETERS loFormSet,lcBolNo
  PRIVATE lcManuf_Id,laRltdFld, MUCCLEVEL

  lcManuf_Id = loFormSet.lcManufId
  MUCCLEVEL  = gfGetMemVar('M_UCCDIV',oAriaApplication.ActiveCompanyID)

  *-- Maintain UCC manufaturer ID at division level
  IF MUCCLEVEL = 'N'
    DECLARE laRltdFld[1,2]
    STORE '' TO laRltdFld,LCUPCMAN
    laRltdFld[1,1] = "CUPCMAN"
    laRltdFld[1,2] = 'LCUPCMAN'
    =gfRltFld(ORDHDR.cDivision,@laRltdFld,'CDIVISION')
    lcManuf_Id = IIF(EMPTY(LCUPCMAN),lcManuf_Id,LCUPCMAN)
  ENDIF
  RETURN ALLTRIM(lcManuf_Id)
  *-- End of lfManufID.

  *:**************************************************************************
  *:* Name        : lfCheckDgt
  *! Developer : Mariam Mazhar
  *! Date      : 01/15/2009
  *:* Purpose     :
  *:***************************************************************************
  *:* Called from :
  *:***************************************************************************
  *:* Parameters : None
  *:***************************************************************************
  *:* Return      : None
  *:***************************************************************************
  *:* Example     :  = lfCheckDgt()
  *:***************************************************************************
FUNCTION lfCheckDgt
  PARAMETER lcUccNo, lcType
  PRIVATE lnChkDigit ,lnSumOdd  ,lnSumEven ,lnCount
  STORE 0 TO lnChkDigit ,lnSumOdd  ,lnSumEven ,lnTop

  lnTop = LEN(lcUccNo)
  FOR lnCount = 1 TO lnTop STEP 2
    lnSumOdd  = lnSumOdd  + VAL(SUBSTR(lcUccNo,lnCount     , 1))
    lnSumEven = lnSumEven + VAL(SUBSTR(lcUccNo,lnCount + 1 , 1))
  ENDFOR
  IF lcType = 'O'
    lnChkDigit = MOD(lnSumOdd*3 + lnSumEven , 10)
  ELSE
    lnChkDigit = MOD(lnSumOdd + lnSumEven*3 , 10)
  ENDIF
  RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))

*:**************************************************************************
*:* Name        : LFUPDENT
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  update entitlement info. Function
*:***************************************************************************
FUNCTION  LFUPDENT
IF !USED('STYHIST')
   = GFOPENTabLE('STYHIST','STYHIST','SH')
ENDIF
IF !USED('Contact')
   =  GFOPENTabLE('Contact','Contact','SH')
ENDIF
IF  !USED('UNIFORM')
   =  GFOPENTabLE('UNIFORM','UNIFORM','SH')
ENDIF
STORE 0 TO LNCLRPOS, LNCLRLEN, LNMAJORLEN
= LFGETCLRD()
SELECT &lcTmpPkTk
LOCATE
=gfSEEK('M'+&lcTmpPkTk..ACCOUNT,'CUSTOMER')
SCAN
   =gfSEEK('O'+&lcTmpPkTk..Order,'ORDHDR')
   =gfSEEK('O'+&lcTmpPkTk..Order,'ORDLINE') 
   SELECT ORDLINE
   SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+&lcTmpPkTk..ORDER  FOR PIKTKT = &lcTmpPkTk..PIKTKT
      SELECT ORDLINE
      IF  .NOT. EMPTY(EMPLOYEE)
         WAIT WINDOW NOWAIT 'Update entitlments used field for order ' + &lcTmpPkTk..ORDER 
         =gfSEEK("C"+PADR(&lcTmpPkTk..ACCOUNT,8)+PADR(&lcTmpPkTk..STORE,8),'Contact')
         SELECT Contact
        
         *: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[Start]
         *LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT= "C"+PADR(&lcTmpPkTk..ACCOUNT,8)+PADR(&lcTmpPkTk..STORE,8) ;
         		FOR CCONTCODE = ORDLINE.EMPLOYEE
         LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT= "C"+PADR(&lcTmpPkTk..ACCOUNT,8)+PADR(&lcTmpPkTk..STORE,8) ;
         		FOR CCNTCTCODE = ORDLINE.EMPLOYEE
         *: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[End]         		
         
         SELECT ORDLINE
         LCSTY = PADR(SUBSTR(ORDLINE.STYLE,1,LNMAJORLEN),19)
         LCCLR = PADR(SUBSTR(ORDLINE.STYLE,LNCLRPOS,LNCLRLEN),6)
         LNUPDFCTR = 1
         LNENTLMNT = 0
         IF  .NOT. EMPTY(Contact.UCODE)
            = gfSEEK(Contact.UCODE+LCSTY+LCCLR,'UNIFORM')
            LNENTLMNT = UNIFORM.ENTITLEMNT
            DO CASE
               CASE UNIFORM.TYPE='V'
                  LNUPDFCTR = ORDLINE.PRICE
               CASE UNIFORM.TYPE='P'
                  LNUPDFCTR = UNIFORM.PNTSVAL
            ENDCASE
         ENDIF
         SELECT STYHIST
         IF !gfSEEK(&lcTmpPkTk..ACCOUNT+&lcTmpPkTk..STORE+ORDLINE.EMPLOYEE+lcSty+lcClr , 'STYHIST' )
            APPEND BLANK
            *: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[Start]
            *REPLACE ACCOUNT    WITH &lcTmpPkTk..ACCOUNT ,;
            		STORE      WITH &lcTmpPkTk..STORE,;
            		EMPLOYEE   WITH ORDLINE.EMPLOYEE,;
            		CSTYMAJOR  WITH lcSty ,;
            		COLOUR     WITH lcClr ,;
	           		ENTITLEMNT WITH lnEntlmnt
            REPLACE ACCOUNT    WITH &lcTmpPkTk..ACCOUNT ,;
            		STORE      WITH &lcTmpPkTk..STORE,;
            		EMPLOYEE   WITH ORDLINE.EMPLOYEE,;
            		CSTYMAJOR  WITH lcSty ,;
            		COLOR     WITH lcClr ,;
	           		ENTITLEMNT WITH lnEntlmnt
	         *: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[End]	           		
            =gfReplace("")
         ENDIF
         gfREPLACE("NUSED WITH NUSED+ORDLINE.TOTPIK*LNUPDFCTR")
         gfREPLACE([DLASTORD WITH ORDHDR.ENTERED,]+;
         			[ORDER WITH ORDHDR.ORDER])
         = gfAdd_Info('STYHIST')			 
         
         IF  !EMPTY(Contact.UCODE)
            = gfSEEK(Contact.UCODE,'UNIFORM')
            SELECT UNIFORM
            *: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[Start]
            *SCAN REST WHILE UCODE+CSTYMAJOR+COLOUR=Contact.UCODE
            *IF !gfSEEK(&lcTmpPkTk..ACCOUNT+&lcTmpPkTk..STORE+ORDLINE.EMPLOYEE+CSTYMAJOR+COLOUR , 'STYHIST' )
            SCAN REST WHILE UCODE+CSTYMAJOR+COLOR=Contact.UCODE
               IF !gfSEEK(&lcTmpPkTk..ACCOUNT+&lcTmpPkTk..STORE+ORDLINE.EMPLOYEE+CSTYMAJOR+COLOR , 'STYHIST' )
            *: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[End]   
                  SELECT STYHIST
                  APPEND BLANK
                  *: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[Start]
                  *                  REPLACE ACCOUNT    WITH &lcTmpPkTk..ACCOUNT   STORE      WITH &lcTmpPkTk..STORE     EMPLOYEE   WITH ORDLINE.EMPLOYEE      CSTYMAJOR  WITH UNIFORM.CSTYMAJOR     COLOUR     WITH UNIFORM.COLOUR        ENTITLEMNT WITH UNIFORM.ENTITLEMNT
                  REPLACE ACCOUNT    WITH &lcTmpPkTk..ACCOUNT   STORE      WITH &lcTmpPkTk..STORE     EMPLOYEE   WITH ORDLINE.EMPLOYEE      CSTYMAJOR  WITH UNIFORM.CSTYMAJOR     COLOR     WITH UNIFORM.COLOR        ENTITLEMNT WITH UNIFORM.ENTITLEMNT
                  *: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[End]
                  =gfReplace("")
				  = gfAdd_Info('STYHIST')
               ENDIF
            ENDSCAN
         ENDIF
      ENDIF
   ENDSCAN
ENDSCAN
SELECT STYHIST
gfTableUpdate()
*:**************************************************************************
*:* Name        : LFGETCLRD
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  get Color length Function
*:***************************************************************************
PROCEDURE LFGETCLRD
DIMENSION LAITEMSEG[ 1]
PRIVATE LNCOUNT
LCOLDSELEC = SELECT()
= GFITEMMASK(@LAITEMSEG)
FOR LNCOUNT = 1 TO ALEN(LAITEMSEG,1)
   DO CASE
      CASE LAITEMSEG(LNCOUNT,1)='C'
         LNCLRLEN = LEN(LAITEMSEG(LNCOUNT,3))
         LNCLRPOS = LAITEMSEG(LNCOUNT,4)
         LCCLRSPR = ALLTRIM(LAITEMSEG(LNCOUNT,6))
   ENDCASE
ENDFOR
LNMAJORLEN = LEN(GFITEMMASK('PM'))
SELECT (LCOLDSELEC)

*:**************************************************************************
*:* Name        : LFCRTINVS
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  Create invoice Function
*:***************************************************************************
FUNCTION LFCRTINVS

*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*DIMENSION LAFOLDERS[ 4, 2], LAFOLDWIND[ 4, 2], LAADDRESS[ 6, 3], LAQTYPASTE[ 1, 8], LASETUPS[ 26, 2], LABROWARR[ 1], LASHIPNAME[ 1, 2]
DIMENSION  LASETUPS[ 26, 2] 
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]
LLCONSINV = .F.
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*DIMENSION LACODES[ 6, 10], LATERMS[ 1, 2], LASEASONS[ 1, 2], LADIVISION[ 1, 2], LAVRLTFLD[ 1, 2], LASHIPVIA[ 1, 2], LASPCINST[ 1, 2], LATRLTFLD[ 6, 2], LAVARS[ 11], LAWAREHOUS[ 1, 2]
*DIMENSION LAGLOCODES[ 3, 10], LAGLOTERMS[ 1, 2], LAGSHIPVIA[ 1, 2], LAGSPCINST[ 1, 2]
*STORE '' TO LAGLOCODES, LAGLOTERMS, LAGSHIPVIA, LAGSPCINST, LCREP1, LCREP2, LCGLONOTE1, LCGLONOTE2, LCGLOLDVAL
*STORE '' TO LCREP1, LCREP2, LCGLONOTE1, LCGLONOTE2, LCGLOLDVAL
*STORE oAriaApplication.SYStemDATE TO LDSHIPDATE, LDGDUEDATE
*STORE 0.00 TO LNDISCPCNT, LNGLOCOMM1, LNGLOCOMM2, LNGTAXRATE, LNGPSTRATE, LNGTRDDISC
*STORE 1 TO LNGPSTRULE, LNUPSINS, LNGCODCHRG
*STORE .F. TO CBGMERDISC, CBGSHPDATE, CBGREP1, CBGREP2, CBGCOMM1, CBGCOMM2, CBGNOTE1, CBGNOTE2, CBGTAXRATE, CBGPSTRATE, CBGTRDDISC, CBGDUEDATE
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [Start]
*DIMENSION  LATRLTFLD[ 6, 2],LAWAREHOUS[ 1, 2]
DIMENSION  LATRLTFLD[ 6, 2]
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [End]
STORE 0.00 TO LNDISCPCNT
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]


LCCRTTMP = ''

*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*LLCALLD = .F.
*LCCHRGTYP = ''
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]

LCUPSTMP = ''
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*!*	DIMENSION LACANREASO[ 1, 2]
*!*	STORE 0 TO LNREP1COMM, LNREP2COMM
*STORE '' TO LACANREASO, LCBACKORD
*STORE 1 TO LNCANREASO
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]

DIMENSION LADISRLTFL[ 1, 2]
STORE 0 TO LNDISC_PCN
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*!*	STORE '' TO LACODES, LATERMS, LASHIPVIA, LASPCINST, LASEASONS, LADIVISION, LAVARS, LCGLSESSIO, LABROWARR, LASHIPNAME, LCTAXNAME
STORE '' TO  LCGLSESSIO,  LCTAXNAME
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]
*!*	LADEFPROC[ 9] = .F.
*!*	LADEFPROC[ 10] = .F.
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*STORE 0 TO LNORDMARK, LNINVMARK, LNTOTTAX, LAQTYPASTE
*STORE ' ' TO LCORDBROW, LCINVBROW, LCBROWSETL, LCGLYEAR, LCGLPERIOD, LCSYSYEAR, LCSYSPERIO, LCIDEFSES, LCIDEFDIV, LCPRICELVL, LCIDEFWARE
STORE 0 TO LNORDMARK, LNINVMARK, LNTOTTAX
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]

STORE 0 TO LNOLDCHRG
STORE {} TO LDDEFINVDA, LDDEFPSTDA
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*STORE 0 TO LNCARTONS, LNSOUCOMM1, LNSOUCOMM2, LNACTFOLDE, LNSTYLNGTH
*STORE 1 TO LNSHIPADDR, LNPSTRULE, LNWAREHOUS, LNSHIPNAME
*STORE '' TO LCSHIPNAME, LCSHIPADD1, LCSHIPADD2, LCSHIPADD3, LCSHIPADD4, LCSHIPADD5, LCBILLNAME, LCBILLADD1, LCBILLADD2, LCBILLADD3, LCBILLADD4, LCBILLADD5, LCFOLDER, LCTAXTITLE, LCTAXBREAK, LCSESSION, LCUPSTYPE, LCORDSTAT
*STORE .F. TO LLBROWSE, LLZOOM, LLISCANADA, LLISENGLAN, LLMULCURR, LLEDITEXRT, LLCOD, LLCONTINUE, LLSYSDATE, LLNOSHOW, LASETUPS, LLINSTTERM, LLADDLINE, LLUPCNSLIN, LLOPNCRDT
STORE 0 TO LNCARTONS,  LNSTYLNGTH
STORE '' TO LCTAXTITLE, LCTAXBREAK, LCSESSION
STORE .F. TO LLISCANADA, LLISENGLAN, LLMULCURR, LLEDITEXRT,LASETUPS
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]

*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*STORE '' TO LCINVLINE, LCINVHDR, LCPACKHDR, LCUPSBOX, LCENGCHRG, LCFLFIELDS, LCSCFIELDS, LCWINCHRG, LAWAREHOUS, LCSTYHDR, LCEMPTYSTY, LCINSTLIN, LCINSTHDR, LCAPPCRDT, LCMJRMSK, LCSTYMJR, LCCONSINVH, LCCONSINVD
*!*	LCSADINVH = ''
*!*	LCSADINVL = ''
*!*	LCSADUPSB = ''
*!*	LCSADINSH = ''
*!*	LCSADINSL = ''
*!*	LCSADORDC = ''
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [Start]
*!*	STORE '' TO LCINVLINE, LCINVHDR, LCPACKHDR, LCUPSBOX, LCENGCHRG, LCFLFIELDS, LCSCFIELDS, LAWAREHOUS,;
*!*				LCSTYHDR, LCEMPTYSTY, LCINSTLIN, LCINSTHDR, LCAPPCRDT, LCMJRMSK, LCSTYMJR, LCCONSINVH, LCCONSINVD
STORE '' TO LCINVLINE, LCINVHDR, LCPACKHDR, LCUPSBOX, LCENGCHRG, LCFLFIELDS, LCSCFIELDS,;
			LCSTYHDR, LCEMPTYSTY, LCINSTLIN, LCINSTHDR, LCAPPCRDT, LCMJRMSK, LCSTYMJR, LCCONSINVH, LCCONSINVD
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [End]
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]

LCTEMPSTK = ''
STORE '' TO LCPACKLINE
STORE '' TO LCORDCANLN
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*STORE '' TO LCTEOM, LCTCOD, LCOLDVALUE, LCUPSTRACK
*STORE 0 TO LNTDAYSDUE, LNTERDISCR, LNLASTMODE
STORE '' TO LCTEOM, LCTCOD
STORE 0 TO LNTDAYSDUE, LNTERDISCR
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]

STORE 20 TO LNEOMDAY

*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*STORE .F. TO M.LUPSINS, M.LNEWLINE, M.LPACKED, M.LBACKORD, LLCNREMAIN
STORE .F. TO M.LUPSINS, M.LNEWLINE, M.LPACKED, M.LBACKORD
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]

STORE 0 TO M.NTAXRATE, M.NCHRGTAX, M.NMERCHTAX

*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*LNSESSNO = GNPROGCOPY
*STORE 1 TO LNTERMS, LNSHIPVIA, LNSPCINST, LNSEASON, LNDIVISION
*STORE 1 TO LNGLOTERMS, LNGSHIPVIA, LNGSPCINST
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]

LCORDER = SPACE(6)
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*STORE .T. TO LLGOANDCHK, LLCHKUNCOM
*STORE '' TO LCADTRNSEQ, LCACTRNSEQ, LCHISSEQ, LCGLSESS, LCREPBAT, LNUNCMSERC
*STORE .T. TO LLUPDGLDIF, LLUPDMSTGL
*STORE .T. TO LLFIRSTTIM
*STORE .F. TO LLCONSOLD
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]




DIMENSION LAENGSTYTA[ 1, 2]
STORE 0 TO LNTAXRATE
STORE '' TO LAENGSTYTA
*LCSVKEY = LCSYDKEY
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*!*	LCPROGID = 'IINVOICE'
*!*	LCORDBROW = 'List'
*!*	LCINVBROW = 'Invoice Lines'
*!*	LCBRTTLZ = 'Zoom Invoice Lines'
*!*	LCSIZE1 = 'Size1'
*!*	LCSIZE2 = 'Size2'
*!*	LCSIZE3 = 'Size3'
*!*	LCSIZE4 = 'Size4'
*!*	LCSIZE5 = 'Size5'
*!*	LCSIZE6 = 'Size6'
*!*	LCSIZE7 = 'Size7'
*!*	LCSIZE8 = 'Size8'
*!*	LAFOLDERS[ 1, 1] = 'List'
*!*	LAFOLDERS[ 1, 2] = 1
*!*	LAFOLDERS[ 2, 1] = 'Header'
*!*	LAFOLDERS[ 2, 2] = 2
*!*	LAFOLDERS[ 3, 1] = 'Details'
*!*	LAFOLDERS[ 3, 2] = 3
*!*	LAFOLDERS[ 4, 1] = 'Charges'
*!*	LAFOLDERS[ 4, 2] = 4
*!*	LNNOFLD = 4
*!*	LCWFOLDCHN = '=lfActFolder()'
*!*	LNFOLDERCE = 102.000
*!*	LNFOLDERRE = 2.000
*!*	STORE 'ENABLE' TO LCSCOPESTA
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]
PRIVATE LCUPDPIK
STORE SPACE(0) TO LCUPDPIK
LCUPDPIK = gfGetMemVar('M_MPKTKIN',oAriaApplication.ActiveCompanyID)
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*LNLASTMODE = 0
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]
SELECT ORDLINE
SCATTER BLANK MEMO MEMVAR
SELECT INVHDR
SCATTER BLANK MEMVAR
LCMJRMSK = GFITEMMASK('PM')
LCEMPTYSTY = PADR(STRTRAN(GFITEMMASK('PI'),'X',' '),19)
LCSTYHDR = GFITEMMASK('HI')
LCSTYMJR = GFITEMMASK('HM')
LNSTYLNGTH = LEN(LCSTYHDR)
LCSCFIELDS = 'Order,CustPo,PikTkt,Account,Store,InvDate,cWareCode,Season,                Comm1,Comm2,Invoice,Flag,cDivision,CCURRCODE'
LCFLFIELDS = 'SHIPDATE,DUEDATE,APPROVAL,cTermCode,SHIPVIA,SPCINST,NCHRGTAX,'+'DEPT,NOTE1,NOTE2,REP1,REP2,lUpsIns,Cod_Flag,NEXRATE,cFacCode,'+'CARTONS,WEIGHT,CODTAG,COD_AMT,TRDE_DISC,SHIPAMT,DISCPCNT,DISCOUNT,'+'FREIGHT,INSUR,COD,TAX_AMT,TOTALCHG,NCHARGES,NCURRUNIT,SHIP,'+'nPstAmt,cTaxRule,TAX_RATE,nPstRate,nHstRate,nHstAmt,BOL_NO,APPRAMT,DPOSTDATE,CCODTRCKNO,STATUS'
LCSESSION = GFSEQUENCE('CSESSION')
LCGLSESSIO = GFSEQUENCE('GLSESSION')
LASETUPS[ 1, 1] = 'M_PACK'
LASETUPS[ 2, 1] = 'M_STY_COM'
LASETUPS[ 3, 1] = 'M_LN_NOTE'
LASETUPS[ 4, 1] = 'M_LINK_GL'
LASETUPS[ 5, 1] = 'M_WareHouse'
LASETUPS[ 6, 1] = 'M_GenOrNum'
LASETUPS[ 7, 1] = 'M_COST_METH'
LASETUPS[ 8, 1] = 'M_DYELOT'
LASETUPS[ 9, 1] = 'M_TAX'
LASETUPS[ 10, 1] = 'M_TAX_RATE'
LASETUPS[ 11, 1] = 'M_TAX_METH'
LASETUPS[ 12, 1] = 'M_UPC_USE'
LASETUPS[ 13, 1] = 'M_DIV_LINK'
LASETUPS[ 14, 1] = 'M_REP_COMM'
LASETUPS[ 15, 1] = 'M_UPSBOX'
LASETUPS[ 16, 1] = 'XAGINGTYPE'
LASETUPS[ 17, 1] = 'XPOSTFINV'
LASETUPS[ 18, 1] = 'XUPSFROM'
LASETUPS[ 19, 1] = 'M_CRDT_LMT'
LASETUPS[ 20, 1] = 'M_EDTPRICE'
LASETUPS[ 21, 1] = 'M_INVBULK'
LASETUPS[ 22, 1] = 'M_INVHOLD'
LASETUPS[ 23, 1] = 'M_BACKORD'
LASETUPS[ 24, 1] = 'M_TAX_DESC'
LASETUPS[ 26, 1] = 'M_HST_RATE'
= gfGetMemVar(@LASETUPS,oAriaApplication.ActiveCompanyID)
LCTAXNAME = IIF(EMPTY(LASETUPS(24,2)),'G.S.T. Tax',LASETUPS(24,2))

*: B608854,5 MMT 11/12/2009 Try to solve error of not enough memory at DCC [Start]
*!*	LCINVHDR = GFTEMPNAME()
*!*	LCINVLINE = GFTEMPNAME()
*!*	LCCONSINVH = GFTEMPNAME()
*!*	LCCONSINVD = GFTEMPNAME()
*!*	LCUPSBOX = GFTEMPNAME()
*!*	LCPACKHDR = GFTEMPNAME()
*!*	LCINSTHDR = GFTEMPNAME()
*!*	LCINSTLIN = GFTEMPNAME()
*!*	LCAPPCRDT = GFTEMPNAME()
*!*	LCTEMPSTK = GFTEMPNAME()
*!*	LCPACKLINE = GFTEMPNAME()
*!*	LCORDCANLN = GFTEMPNAME()
*!*	LCCRTTMP = GFTEMPNAME()
*!*	LCUPSTMP = GFTEMPNAME()
LCINVHDR   = loFormSet.LCINVHDR   
LCINVLINE  = loFormSet.LCINVLINE  
LCCONSINVH = loFormSet.LCCONSINVH 
LCCONSINVD = loFormSet.LCCONSINVD 
LCUPSBOX   = loFormSet.LCUPSBOX   
LCPACKHDR  = loFormSet.LCPACKHDR  
LCINSTHDR  = loFormSet.LCINSTHDR  
LCINSTLIN  = loFormSet.LCINSTLIN  
LCAPPCRDT  = loFormSet.LCAPPCRDT  
LCTEMPSTK  = loFormSet.LCTEMPSTK  
LCPACKLINE = loFormSet.LCPACKLINE 
LCORDCANLN = loFormSet.LCORDCANLN 
LCCRTTMP   = loFormSet.LCCRTTMP   
LCUPSTMP   = loFormSet.LCUPSTMP   
*: B608854,5 MMT 11/12/2009 Try to solve error of not enough memory at DCC [End]

LLISCANADA = IIF(UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='CANADA',.T.,.F.)
IF UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='ENG'
   DIMENSION LAENGSTYTA[ 1, 2]
   LAENGSTYTA[ 1, 1] = 'NTAXRATE'
   LAENGSTYTA[ 1, 2] = 'lnTaxRate'
   STORE '' TO LCTAXTITLE, LCTAXBREAK
   LLISENGLAN = .T.
   LCENGCHRG = GFTEMPNAME()
   LASETUPS[ 15, 2] = 'N'
ELSE
   LLISENGLAN = .F.
ENDIF
LLMULCURR = gfGetMemVar('llMulCurr',oAriaApplication.ActiveCompanyID)
LLEDITEXRT = gfGetMemVar('LLEDITEXRA',oAriaApplication.ActiveCompanyID)

*: B608854,5 MMT 11/12/2009 Try to solve error of not enough memory at DCC [Start]
*!*	LCWINCH1 = GFTEMPNAME()
*!*	LCWINCH2 = GFTEMPNAME()
*!*	LCWINCH21 = GFTEMPNAME()
*!*	LCWINCH22 = GFTEMPNAME()
*!*	LCWINCH3 = GFTEMPNAME()
*!*	LCWINCH4 = GFTEMPNAME()
*!*	LCWINCH40 = GFTEMPNAME()
*!*	LCWINCH41 = GFTEMPNAME()
*!*	LCWINCH42 = GFTEMPNAME()
*!*	LCWINCH43 = GFTEMPNAME()
*!*	LCWINCH44 = GFTEMPNAME()
*!*	LCWINCH45 = GFTEMPNAME()
*!*	LCWINCH46 = GFTEMPNAME()
*!*	LCWINCH51 = GFTEMPNAME()
*!*	LCWINCH52 = GFTEMPNAME()
*!*	LCWINCH53 = GFTEMPNAME()
*!*	LCFOLDER = GFTEMPNAME()
*!*	LCFOLDPRNT = GCBASEWIND
*!*	LNACTFOLDE = 1
*!*	LCWINCH6 = GFTEMPNAME()
*!*	LCWINCHRG = IIF(LLISENGLAN,LCWINCH51,IIF(LLISCANADA,LCWINCH52,LCWINCH53))
*!*	LAFOLDWIND[ 1, 1] = LAFOLDERS(1,1)
*!*	LAFOLDWIND[ 1, 2] = LCWINCH2
*!*	LAFOLDWIND[ 2, 1] = LAFOLDERS(2,1)
*!*	LAFOLDWIND[ 2, 2] = LCWINCH3
*!*	LAFOLDWIND[ 3, 1] = LAFOLDERS(3,1)
*!*	LAFOLDWIND[ 3, 2] = LCWINCH4
*!*	LAFOLDWIND[ 4, 1] = LAFOLDERS(4,1)
*!*	LAFOLDWIND[ 4, 2] = LCWINCHRG
*: B608854,5 MMT 11/12/2009 Try to solve error of not enough memory at DCC [End]


*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*LLNOSHOW = .F.
*!*	LAVARS[ 1] = 'ldDefInvDate'
*!*	LAVARS[ 2] = 'lcGlSession'
*!*	LAVARS[ 3] = 'ldDefPstDate'
*!*	LAVARS[ 4] = 'lnUnCmSeRc'
*!*	LAVARS[ 5] = 'lcAdTrnSeq'
*!*	LAVARS[ 6] = 'lcAcTrnSeq'
*!*	LAVARS[ 7] = 'lcHisSeq'
*!*	LAVARS[ 8] = 'lcGlSess'
*!*	LAVARS[ 9] = 'lcRepBat'
*!*	LAVARS[ 10] = 'llUpdGlDif'
*!*	LAVARS[ 11] = 'llUpdMstGL'
*!*	LACODES[ 1, 1] = 'CTERMCODE'
*!*	LACODES[ 1, 2] = 'laTerms'
*!*	LACODES[ 1, 3] = 'lnTerms'
*!*	LACODES[ 1, 4] = ''
*!*	LACODES[ 1, 5] = .F.
*!*	LACODES[ 1, 6] = .F.
*!*	LACODES[ 1, 7] = LCINVHDR
*!*	LACODES[ 1, 8] = LCINVHDR
*!*	LACODES[ 1, 9] = 'laData[4]+laData[1]+laData[5]+laData[3]'
*!*	LACODES[ 1, 10] = 'cTermCode'
*!*	LAVRLTFLD[ 1, 1] = 'CUPS'
*!*	LAVRLTFLD[ 1, 2] = 'lcUpsType'
*!*	LADISRLTFL[ 1, 1] = 'DISCPCNT'
*!*	LADISRLTFL[ 1, 2] = 'lnDisc_Pcnt'
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]
LATRLTFLD[ 1, 1] = 'NTERDISCR'
LATRLTFLD[ 1, 2] = 'lnTerDiscR'
LATRLTFLD[ 2, 1] = 'EOM'
LATRLTFLD[ 2, 2] = 'lcTEOM'
LATRLTFLD[ 3, 1] = 'NTERDUED'
LATRLTFLD[ 3, 2] = 'lnTDaysDue'
LATRLTFLD[ 4, 1] = 'CODYN'
LATRLTFLD[ 4, 2] = 'lcTCod'
LATRLTFLD[ 5, 1] = 'LINSTALLM'
LATRLTFLD[ 5, 2] = 'llInstTerm'
LATRLTFLD[ 6, 1] = 'EOMDAY'
LATRLTFLD[ 6, 2] = 'lnEomDay'
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [Start]
*!*	LACODES[ 2, 1] = 'SHIPVIA'
*!*	LACODES[ 2, 2] = 'laShipVia'
*!*	LACODES[ 2, 3] = 'lnShipVia'
*!*	LACODES[ 2, 4] = ''
*!*	LACODES[ 2, 5] = .T.
*!*	LACODES[ 2, 6] = .F.
*!*	LACODES[ 2, 7] = LCINVHDR
*!*	LACODES[ 2, 8] = LCINVHDR
*!*	LACODES[ 2, 9] = 'laData[4]+laData[1]+laData[5]+laData[3]+cdivision'
*!*	LACODES[ 2, 10] = 'SHIPVIA'
*!*	LACODES[ 3, 1] = 'SPCINST'
*!*	LACODES[ 3, 2] = 'laSpcInst'
*!*	LACODES[ 3, 3] = 'lnSpcInst'
*!*	LACODES[ 3, 4] = ''
*!*	LACODES[ 3, 5] = .F.
*!*	LACODES[ 3, 6] = .F.
*!*	LACODES[ 3, 7] = LCINVHDR
*!*	LACODES[ 3, 8] = LCINVHDR
*!*	LACODES[ 3, 9] = 'laData[4]+laData[1]+laData[5]+laData[3]+cdivision'
*!*	LACODES[ 3, 10] = 'SPCINST'
*!*	LACODES[ 4, 1] = 'SEASON'
*!*	LACODES[ 4, 2] = 'laSeasons'
*!*	LACODES[ 4, 3] = 'lnSeason'
*!*	LACODES[ 4, 4] = ''
*!*	LACODES[ 4, 5] = .T.
*!*	LACODES[ 4, 6] = .F.
*!*	LACODES[ 4, 7] = LCINVHDR
*!*	LACODES[ 4, 8] = LCINVHDR
*!*	LACODES[ 4, 9] = 'laData[4]+laData[1]+laData[5]+laData[3]+cdivision'
*!*	LACODES[ 4, 10] = 'SEASON'
*!*	LACODES[ 5, 1] = 'CDIVISION'
*!*	LACODES[ 5, 2] = 'laDivision'
*!*	LACODES[ 5, 3] = 'lnDivision'
*!*	LACODES[ 5, 4] = ''
*!*	LACODES[ 5, 5] = .F.
*!*	LACODES[ 5, 6] = .F.
*!*	LACODES[ 5, 7] = LCINVHDR
*!*	LACODES[ 5, 8] = LCINVHDR
*!*	LACODES[ 5, 9] = 'laData[4]+laData[1]+laData[5]+laData[3]+cdivision'
*!*	LACODES[ 5, 10] = 'cDivision'
*!*	LACODES[ 6, 1] = 'CCANCRESON'
*!*	LACODES[ 6, 2] = 'laCanReason'
*!*	LACODES[ 6, 3] = 'lnCanReason'
*!*	LACODES[ 6, 4] = ''
*!*	LACODES[ 6, 5] = .F.
*!*	LACODES[ 6, 6] = .F.
*!*	LACODES[ 6, 7] = LCORDCANLN
*!*	LACODES[ 6, 8] = LCORDCANLN
*!*	LACODES[ 6, 9] = "'O'+m.Order+STR(m.LineNo,6)"
*!*	LACODES[ 6, 10] = 'CCANCRESON'
*!*	LAGLOCODES[ 1, 1] = 'CTERMCODE'
*!*	LAGLOCODES[ 1, 2] = 'laGloTerms'
*!*	LAGLOCODES[ 1, 3] = 'lnGloTerms'
*!*	LAGLOCODES[ 1, 4] = ''
*!*	LAGLOCODES[ 1, 5] = .T.
*!*	LAGLOCODES[ 1, 6] = .F.
*!*	LAGLOCODES[ 1, 7] = LCINVHDR
*!*	LAGLOCODES[ 1, 8] = LCINVHDR
*!*	LAGLOCODES[ 1, 9] = 'laData[4]+laData[1]+laData[5]+laData[3]'
*!*	LAGLOCODES[ 1, 10] = 'cTermCode'
*!*	LAGLOCODES[ 2, 1] = 'SPCINST'
*!*	LAGLOCODES[ 2, 2] = 'laGSpcInst'
*!*	LAGLOCODES[ 2, 3] = 'lnGSpcInst'
*!*	LAGLOCODES[ 2, 4] = ''
*!*	LAGLOCODES[ 2, 5] = .T.
*!*	LAGLOCODES[ 2, 6] = .F.
*!*	LAGLOCODES[ 2, 7] = LCINVHDR
*!*	LAGLOCODES[ 2, 8] = LCINVHDR
*!*	LAGLOCODES[ 2, 9] = 'laData[4]+laData[1]+laData[5]+laData[3]+cdivision'
*!*	LAGLOCODES[ 2, 10] = 'SPCINST'
*!*	LAGLOCODES[ 3, 1] = 'SHIPVIA'
*!*	LAGLOCODES[ 3, 2] = 'laGShipVia'
*!*	LAGLOCODES[ 3, 3] = 'lnGShipVia'
*!*	LAGLOCODES[ 3, 4] = ''
*!*	LAGLOCODES[ 3, 5] = .T.
*!*	LAGLOCODES[ 3, 6] = .F.
*!*	LAGLOCODES[ 3, 7] = LCINVHDR
*!*	LAGLOCODES[ 3, 8] = LCINVHDR
*!*	LAGLOCODES[ 3, 9] = 'laData[4]+laData[1]+laData[5]+laData[3]+cdivision'
*!*	LAGLOCODES[ 3, 10] = 'SHIPVIA'
*!*	= LFWFILLPOP(@LAGLOCODES,'CTERMCODE','A')
*!*	= LFWFILLPOP(@LAGLOCODES,'SHIPVIA','A')
*!*	= LFWFILLPOP(@LAGLOCODES,'SPCINST','A')
*: B608854,6 MMT 01/10/2009 Try to solve error of not enough memory at DCC [End]

*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [Start]
*= GFOPENTabLE('WAREHOUS','WAREHOUS','SH')
*!*	SELECT LEFT(CDESC,20), CWARECODE FROM WAREHOUS INTO ARRAY LAWAREHOUS
IF !USED('WAREHOUS')
  = GFOPENTabLE('WAREHOUS','WAREHOUS','SH')
ENDIF 
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [End]
IF 'CR'$ oAriaApplication.CompanyInstalledModules          
   STORE .T. TO LLCRFIRST
   =  GFOPENTabLE('ORDCHARG','ORDER','SH')
ENDIF
LLINVCHRG = LLISENGLAN .AND.  GFOPENTabLE('InvChrg','InvChrg','SH')
IF 'AL'$ oAriaApplication.CompanyInstalledModules     
   LLPIKTKT = GFOPENTabLE('PIKTKT','Ordpik','SH')
   SELECT PIKTKT
   =gfSeek("")
   SET FILTER TO STATUS<>'X' .AND. STATUS<>'C'
ENDIF
   =  GFOPENTabLE('Customer','Customer','SH',"DistCntr")
LLOPNFILES = .T.
= LFCRATTEMP()
SELECT &lcTmpPkTk
LOCATE
=gfSEEK('M'+&lcTmpPkTk..ACCOUNT,'CUSTOMER')
SCAN
   WAIT WINDOW NOWAIT 'Preparing files to create an invoice for piktkt : '+ &lcTmpPkTk..PIKTKT
   =gfSEEK('O'+&lcTmpPkTk..Order,'ORDHDR')    
   =lfGetOrder(&lcTmpPkTk..Order,&lcTmpPkTk..Store,.T.,&lcTmpPkTk..PIKTKT,&lcTmpPkTk..PIKTKT)
ENDSCAN

loFORMSET.lformhastriggers = .T.

IF ASCAN(loFORMSET.laEvntTrig,'INVCHARG')=0
   DIMENSION loFORMSET.laEvntTrig[ ALEN(loFORMSET.laEvntTrig,1)+1,4]
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),1] = PADR('INVCHARG',10)
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),2] = "DIRMAIN"
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),3] = "'INVCHARG'"
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),4] = "C"
ENDIF
IF ASCAN(loFORMSET.laEvntTrig,'ALSAVINV')=0
   DIMENSION loFORMSET.laEvntTrig[ ALEN(loFORMSET.laEvntTrig,1)+1,4]
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),1] = PADR('ALSAVINV',10)
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),2] = "bn4main"
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),3] = "'ALSAVINV'"
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),4] = "C"

ENDIF
IF ASCAN(loFORMSET.laEvntTrig,'DLARBIN')=0
   DIMENSION loFORMSET.laEvntTrig[ ALEN(loFORMSET.laEvntTrig,1)+1,4]
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),1] = PADR('DLARBIN',10)
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),2] = "bn4main"
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),3] = "'DLARBIN'"
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),4] = "C"

ENDIF

IF ASCAN(loFORMSET.laEvntTrig,'TRNHIST')=0
   DIMENSION loFORMSET.laEvntTrig[ ALEN(loFORMSET.laEvntTrig,1)+1,4]
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),1] = PADR('TRNHIST',10)
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),2] = "DIRMAIN"
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),3] = "'TRNHIST'"
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),4] = "C"

ENDIF

IF ASCAN(loFORMSET.laEvntTrig,'GFSTYCRL')=0
   DIMENSION loFORMSET.laEvntTrig[ ALEN(loFORMSET.laEvntTrig,1)+1,4]
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),1] = PADR('GFSTYCRL',10)
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),2] = "bn4main"
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),3] = "'GFSTYCRL'"
   loFORMSET.LAEVNTTRIG[ ALEN(loFORMSET.laEvntTrig,1),4] = "C"

ENDIF





SELECT &lcInvHdr
SET ORDER TO
SET DELETED OFF
GOTO TOP
SCAN
   IF CONSOL<>'Y' .AND. SEEK(ACCOUNT+ORDER+STORE+PIKTKT+'Y'+SPACE(10),LCINVLINE)
      SELECT (LCINVLINE)
      LOCATE REST FOR Account+Order+Store+PikTkt+'Y'+SPACE(10) = &lcInvHdr..Account+ &lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt+'Y'+SPACE(10)  AND TotQty <> 0
      IF FOUND()
         = GFMODALGEN('TRM40153B00000','ALERT')
         LLCSAVE = .F.
         EXIT
      ENDIF
      SELECT (LCINVHDR)
   ENDIF
   IF CONSOL='Y' .OR. EMPTY(FLAG)
      LLSAVEINV = .F.
      DO GFCHKSAVINV IN (oAriaApplication.ApplicationHome+'AR\ARINV.PRG') WITH ;
      	ACCOUNT, ORDER, STORE, PIKTKT, LCINVHDR, IIF(USED(LCINSTHDR),LCINSTHDR,''),;
      	IIF(USED(LCAPPCRDT),LCAPPCRDT,''), IIF(USED(LCUPSBOX),LCUPSBOX,''), 'llSaveInv',.F.,.F.,'',loFormSet
      IF  .NOT. LLSAVEINV
         DELETE
      ENDIF
   ENDIF
   SELECT &lcInvHdr
   REPLACE DPOSTDATE WITH oAriaApplication.SystemDate
   DIMENSION  LADATA[5]
   LADATA[ 1] = ORDER
   LADATA[ 3] = PIKTKT
   LADATA[ 5] = STORE
   M.NCHARGES = 0
   = LFUPDIRCHG()
ENDSCAN
SELECT &lcInvHdr
SET DELETED ON
GOTO TOP
loFormset.ariaForm1.ariaPAGEFRAME1.page4.cntInvoicesummary.ChargesFile =LCENGCHRG		 

loFormSet.lcInvHdr = lcInvHdr
loFormSet.lcinvline = LCINVLINE


lcOldName = loFormSet.Name 
loFormSet.Name = 'AWRARIINV'
DO GPSAVEINV IN (oAriaApplication.ApplicationHome+'AR\ARINV.PRG') WITH LCINVHDR, LCINVLINE,;
			 IIF(USED(LCUPSBOX),LCUPSBOX,''), IIF(USED(LCENGCHRG),LCENGCHRG,''),;
			 IIF(USED(LCINSTHDR),LCINSTHDR,''), IIF(USED(LCINSTLIN),LCINSTLIN,''),;
			 IIF(USED(LCORDCANLN),LCORDCANLN,''),IIF(USED(LCAPPCRDT),LCAPPCRDT,''), LCGLSESSIO, 'laInv',.F.,.F.,loFormset
loFormSet.Name = lcOldName   


*!*	IF ASCAN(loformset.laEvntTrig , PADR('TRNHIST',10)) <> 0			 
*!*	   =loFormSet.mDoTrigger(PADR('TRNHIST',10))
*!*	ENDIF 

*!*	IF ASCAN(loformset.laEvntTrig , PADR('INVCHARG',10)) <> 0
*!*	  loformset.mDoTrigger(PADR('INVCHARG',10)) 
*!*	ENDIF  
*! B608953,1 MMT 08/02/2009 Fix bug of erorr after Saving packing list[Start]
*USE IN 'DistCntr'
=gfCloseTable('DistCntr')
*! B608953,1 MMT 08/02/2009 Fix bug of erorr after Saving packing list[End]
DO LPRELEASLK
IF USED(LCINVHDR)
   USE IN (LCINVHDR)
ENDIF
ERASE (oAriaApplication.WORKDIR+LCINVHDR+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCINVHDR+'.CDX')

IF USED(LCINVLINE)
   USE IN (LCINVLINE)
ENDIF
ERASE (oAriaApplication.WORKDIR+LCINVLINE+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCINVLINE+'.CDX')
ERASE (oAriaApplication.WORKDIR+LCINVLINE+'.FPT')
IF USED(LCUPSBOX)
   USE IN (LCUPSBOX)
ENDIF
ERASE (oAriaApplication.WORKDIR+LCUPSBOX+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCUPSBOX+'.CDX')
IF USED(LCINSTHDR)
   USE IN (LCINSTHDR)
ENDIF
ERASE (oAriaApplication.WORKDIR+LCINSTHDR+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCINSTHDR+'.CDX')
IF USED(LCINSTLIN)
   USE IN (LCINSTLIN)
ENDIF
ERASE (oAriaApplication.WORKDIR+LCINSTLIN+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCINSTLIN+'.CDX')
IF USED(LCAPPCRDT)
   USE IN (LCAPPCRDT)
ENDIF
ERASE (oAriaApplication.WORKDIR+LCAPPCRDT+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCAPPCRDT+'.CDX')
IF LLISENGLAN .AND. USED(LCENGCHRG)
   USE IN (LCENGCHRG)
ENDIF
ERASE (oAriaApplication.WORKDIR+LCENGCHRG+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCENGCHRG+'.CDX')
IF USED(LCORDCANLN)
   USE IN (LCORDCANLN)
ENDIF
ERASE (oAriaApplication.WORKDIR+LCORDCANLN+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCORDCANLN+'.CDX')
IF USED(LCCONSINVH)
   USE IN (LCCONSINVH)
ENDIF
ERASE (oAriaApplication.WORKDIR+LCCONSINVH+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCCONSINVH+'.CDX')
IF USED(LCCONSINVD)
   USE IN (LCCONSINVD)
ENDIF
ERASE (oAriaApplication.WORKDIR+LCCONSINVD+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCCONSINVD+'.CDX')
ERASE (oAriaApplication.WORKDIR+LCCONSINVD+'.FPT')
*!*	IF ASCAN(LAEVNTTRIG,PADR('SADSAV',10))<>0 .AND.  .NOT. TYPE('lcSadInvH')$'UL'
*!*	   IF USED(LCSADINVH)
*!*	      USE IN (LCSADINVH)
*!*	   ENDIF
*!*	   ERASE (GCWORKDIR+LCSADINVH+'.DBF')
*!*	   ERASE (GCWORKDIR+LCSADINVH+'.CDX')
*!*	   IF USED(LCSADINVL)
*!*	      USE IN (LCSADINVL)
*!*	   ENDIF
*!*	   ERASE (GCWORKDIR+LCSADINVL+'.DBF')
*!*	   ERASE (GCWORKDIR+LCSADINVL+'.CDX')
*!*	   IF USED(LCSADUPSB)
*!*	      USE IN (LCSADUPSB)
*!*	   ENDIF
*!*	   ERASE (GCWORKDIR+LCSADUPSB+'.DBF')
*!*	   ERASE (GCWORKDIR+LCSADUPSB+'.CDX')
*!*	   IF USED(LCSADINSH)
*!*	      USE IN (LCSADINSH)
*!*	   ENDIF
*!*	   ERASE (GCWORKDIR+LCSADINSH+'.DBF')
*!*	   ERASE (GCWORKDIR+LCSADINSH+'.CDX')
*!*	   IF USED(LCSADINSL)
*!*	      USE IN (LCSADINSL)
*!*	   ENDIF
*!*	   ERASE (GCWORKDIR+LCSADINSL+'.DBF')
*!*	   ERASE (GCWORKDIR+LCSADINSL+'.CDX')
*!*	   IF USED(LCORDCANLN)
*!*	      USE IN (LCORDCANLN)
*!*	   ENDIF
*!*	   ERASE (GCWORKDIR+LCORDCANLN+'.DBF')
*!*	   ERASE (GCWORKDIR+LCORDCANLN+'.CDX')
*!*	ENDIF
IF USED(LCTEMPSTK)
   USE IN (LCTEMPSTK)
ENDIF
ERASE (oAriaApplication.WORKDIR+LCTEMPSTK+'.DBF')
ERASE (oAriaApplication.WORKDIR+LCTEMPSTK+'.CDX')
IF 'AL'$ oAriaApplication.CompanyInstalledModules          
*!*	   IF LLPIKTKT
*!*	      gfCloseTable("PIKTKT")
*!*	   ELSE
      SELECT PIKTKT
      SET FILTER TO
*!*	   ENDIF
ENDIF

SELECT Invhdr
=gfTableUpdate()
SELECT Invline 
=gfTableUpdate()
SELECT Customer
=gfTableUpdate()

*! B608953,2 MMT 08/11/2009 Fix bug of Printing same packing list if scanned 2 PL in same session[Start]   
SELECT Style 
=gfTableUpdate()

SELECT Stydye
=gfTableUpdate()


*: B608854,3 MMT 10/13/2009 ORDHDR Table is not udpated after creating invoice [Start]
SELECT Ordhdr
=gfTableUpdate()

SELECT OrdLine
=gfTableUpdate()
*: B608854,3 MMT 10/13/2009 ORDHDR Table is not udpated after creating invoice [End]


TRY
  IF USED('STYINVJL')
    SELECT STYINVJL
    TABLEUPDATE(.T.,.T.)
  ENDIF 
CATCH 
ENDTRY 
*! B608953,2 MMT 08/11/2009 Fix bug of Printing same packing list if scanned 2 PL in same session[Start]   


*: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[Start]	
IF USED('InvChrg')
  SELECT InvChrg
  =gfTableUpdate()
ENDIF 
*: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[End]	

*:**************************************************************************
*:* Name        : LFCRATTEMP
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  Create Temp. Files Function
*:***************************************************************************
FUNCTION  LFCRATTEMP
PRIVATE LAFILESTRU, LNFILESTRU, LAINDEX
SELECT INVHDR
= AFIELDS(LAFILESTRU)
LNFILESTRU = ALEN(LAFILESTRU,1)
DIMENSION LAFILESTRU[ LNFILESTRU+17, 18]
LAFILESTRU[ LNFILESTRU+1, 1] = 'cSelect'
LAFILESTRU[ LNFILESTRU+1, 2] = 'C'
LAFILESTRU[ LNFILESTRU+1, 3] = 1
LAFILESTRU[ LNFILESTRU+1, 4] = 0
LAFILESTRU[ LNFILESTRU+2, 1] = 'Picked'
LAFILESTRU[ LNFILESTRU+2, 2] = 'N'
LAFILESTRU[ LNFILESTRU+2, 3] = 7
LAFILESTRU[ LNFILESTRU+2, 4] = 0
LAFILESTRU[ LNFILESTRU+3, 1] = 'lUpsIns'
LAFILESTRU[ LNFILESTRU+3, 2] = 'L'
LAFILESTRU[ LNFILESTRU+3, 3] = 1
LAFILESTRU[ LNFILESTRU+3, 4] = 0
LAFILESTRU[ LNFILESTRU+4, 1] = 'nSteps'
LAFILESTRU[ LNFILESTRU+4, 2] = 'N'
LAFILESTRU[ LNFILESTRU+4, 3] = 6
LAFILESTRU[ LNFILESTRU+4, 4] = 0
LAFILESTRU[ LNFILESTRU+5, 1] = 'nChrgTax'
LAFILESTRU[ LNFILESTRU+5, 2] = 'N'
LAFILESTRU[ LNFILESTRU+5, 3] = 13
LAFILESTRU[ LNFILESTRU+5, 4] = 2
LAFILESTRU[ LNFILESTRU+6, 1] = 'nMerchTax'
LAFILESTRU[ LNFILESTRU+6, 2] = 'N'
LAFILESTRU[ LNFILESTRU+6, 3] = 13
LAFILESTRU[ LNFILESTRU+6, 4] = 5
LAFILESTRU[ LNFILESTRU+7, 1] = 'lCompUps'
LAFILESTRU[ LNFILESTRU+7, 2] = 'L'
LAFILESTRU[ LNFILESTRU+7, 3] = 1
LAFILESTRU[ LNFILESTRU+7, 4] = 0
LAFILESTRU[ LNFILESTRU+8, 1] = 'LastLine'
LAFILESTRU[ LNFILESTRU+8, 2] = 'N'
LAFILESTRU[ LNFILESTRU+8, 3] = 6
LAFILESTRU[ LNFILESTRU+8, 4] = 0
LAFILESTRU[ LNFILESTRU+9, 1] = 'LKEYOFF'
LAFILESTRU[ LNFILESTRU+9, 2] = 'L'
LAFILESTRU[ LNFILESTRU+9, 3] = 1
LAFILESTRU[ LNFILESTRU+9, 4] = 0
LAFILESTRU[ LNFILESTRU+10, 1] = 'NTAXDUE'
LAFILESTRU[ LNFILESTRU+10, 2] = 'N'
LAFILESTRU[ LNFILESTRU+10, 3] = 17
LAFILESTRU[ LNFILESTRU+10, 4] = 6
LAFILESTRU[ LNFILESTRU+11, 1] = 'NCARTONS'
LAFILESTRU[ LNFILESTRU+11, 2] = 'N'
LAFILESTRU[ LNFILESTRU+11, 3] = 11
LAFILESTRU[ LNFILESTRU+11, 4] = 5
LAFILESTRU[ LNFILESTRU+12, 1] = 'Ordered'
LAFILESTRU[ LNFILESTRU+12, 2] = 'N'
LAFILESTRU[ LNFILESTRU+12, 3] = 7
LAFILESTRU[ LNFILESTRU+12, 4] = 0
LAFILESTRU[ LNFILESTRU+13, 1] = 'cConStore'
LAFILESTRU[ LNFILESTRU+13, 2] = 'C'
LAFILESTRU[ LNFILESTRU+13, 3] = 8
LAFILESTRU[ LNFILESTRU+13, 4] = 0
LAFILESTRU[ LNFILESTRU+14, 1] = 'NTrueShip'
LAFILESTRU[ LNFILESTRU+14, 2] = 'N'
LAFILESTRU[ LNFILESTRU+14, 3] = 17
LAFILESTRU[ LNFILESTRU+14, 4] = 5
LAFILESTRU[ LNFILESTRU+15, 1] = 'NTrueDscnt'
LAFILESTRU[ LNFILESTRU+15, 2] = 'N'
LAFILESTRU[ LNFILESTRU+15, 3] = 17
LAFILESTRU[ LNFILESTRU+15, 4] = 5
LAFILESTRU[ LNFILESTRU+16, 1] = 'NTrueChrg'
LAFILESTRU[ LNFILESTRU+16, 2] = 'N'
LAFILESTRU[ LNFILESTRU+16, 3] = 17
LAFILESTRU[ LNFILESTRU+16, 4] = 5
LAFILESTRU[ LNFILESTRU+17, 1] = 'NTruTaxRat'
LAFILESTRU[ LNFILESTRU+17, 2] = 'N'
LAFILESTRU[ LNFILESTRU+17, 3] = 17
LAFILESTRU[ LNFILESTRU+17, 4] = 5

FOR lnC = LNFILESTRU TO LNFILESTRU+17
  STORE ' ' TO  laFileStru[lnC,7],laFileStru[lnC ,8],;
                laFileStru[lnC,9],laFileStru[lnC ,10],;
                laFileStru[lnC ,11],laFileStru[lnC ,12],;
                laFileStru[lnC ,13],laFileStru[lnC ,14],;
                laFileStru[lnC ,15],laFileStru[lnC ,16]
      STORE 0 TO    laFileStru[lnC ,17] ,laFileStru[lnC ,18]
ENDFOR 
DIMENSION LAINDEX[ 3, 2]
LAINDEX[ 1, 1] = 'Account+Order+Store+PikTkt+CDIVISION'
LAINDEX[ 1, 2] = LCINVHDR
LAINDEX[ 2, 1] = 'cSelect+Account+Order+Store+PikTkt+CDIVISION'
LAINDEX[ 2, 2] = 'Select'
LAINDEX[ 3, 1] = 'Consol+Account+cDivision+cCurrCode+CDIVISION'
LAINDEX[ 3, 2] = 'Consol'
= GFCRTTMP(LCINVHDR,@LAFILESTRU,@LAINDEX)
= GFCRTTMP(LCCONSINVH,@LAFILESTRU,'Consol+Account+cDivision+cCurrCode',LCCONSINVH)
SELECT (LCINVHDR)
SET ORDER TO (LCINVHDR)
SCATTER MEMVAR FIELDS &lcFlFields BLANK
SCATTER FIELDS &lcScFields TO laData BLANK
SELECT ORDLINE
= AFIELDS(LAFILESTRU)
LNFILESTRU = ALEN(LAFILESTRU,1)
DIMENSION LAFILESTRU[ LNFILESTRU+13, 18]
LAFILESTRU[ LNFILESTRU+1, 1] = 'LNEWLINE'
LAFILESTRU[ LNFILESTRU+1, 2] = 'L'
LAFILESTRU[ LNFILESTRU+1, 3] = 0
LAFILESTRU[ LNFILESTRU+1, 4] = 0
LAFILESTRU[ LNFILESTRU+2, 1] = 'LPACKED'
LAFILESTRU[ LNFILESTRU+2, 2] = 'L'
LAFILESTRU[ LNFILESTRU+2, 3] = 0
LAFILESTRU[ LNFILESTRU+2, 4] = 0
LAFILESTRU[ LNFILESTRU+3, 1] = 'LBACKORD'
LAFILESTRU[ LNFILESTRU+3, 2] = 'L'
LAFILESTRU[ LNFILESTRU+3, 3] = 0
LAFILESTRU[ LNFILESTRU+3, 4] = 0
LAFILESTRU[ LNFILESTRU+4, 1] = 'nSteps'
LAFILESTRU[ LNFILESTRU+4, 2] = 'N'
LAFILESTRU[ LNFILESTRU+4, 3] = 6
LAFILESTRU[ LNFILESTRU+4, 4] = 0
LAFILESTRU[ LNFILESTRU+5, 1] = 'nTaxRate'
LAFILESTRU[ LNFILESTRU+5, 2] = 'N'
LAFILESTRU[ LNFILESTRU+5, 3] = 10
LAFILESTRU[ LNFILESTRU+5, 4] = 2
LAFILESTRU[ LNFILESTRU+6, 1] = 'cCurrCode'
LAFILESTRU[ LNFILESTRU+6, 2] = 'C'
LAFILESTRU[ LNFILESTRU+6, 3] = 3
LAFILESTRU[ LNFILESTRU+6, 4] = 0
LAFILESTRU[ LNFILESTRU+7, 1] = 'cDivision'
LAFILESTRU[ LNFILESTRU+7, 2] = 'C'
LAFILESTRU[ LNFILESTRU+7, 3] = 6
LAFILESTRU[ LNFILESTRU+7, 4] = 0
LAFILESTRU[ LNFILESTRU+8, 1] = 'Consol'
LAFILESTRU[ LNFILESTRU+8, 2] = 'C'
LAFILESTRU[ LNFILESTRU+8, 3] = 1
LAFILESTRU[ LNFILESTRU+8, 4] = 0
LAFILESTRU[ LNFILESTRU+9, 1] = 'nNetAmnt'
LAFILESTRU[ LNFILESTRU+9, 2] = 'N'
LAFILESTRU[ LNFILESTRU+9, 3] = 18
LAFILESTRU[ LNFILESTRU+9, 4] = 10
LAFILESTRU[ LNFILESTRU+10, 1] = 'nGrosAmnt'
LAFILESTRU[ LNFILESTRU+10, 2] = 'N'
LAFILESTRU[ LNFILESTRU+10, 3] = 18
LAFILESTRU[ LNFILESTRU+10, 4] = 10
LAFILESTRU[ LNFILESTRU+11, 1] = 'LTAXABLE'
LAFILESTRU[ LNFILESTRU+11, 2] = 'L'
LAFILESTRU[ LNFILESTRU+11, 3] = 0
LAFILESTRU[ LNFILESTRU+11, 4] = 0
LAFILESTRU[ LNFILESTRU+12, 1] = 'cDyeFlag'
LAFILESTRU[ LNFILESTRU+12, 2] = 'C'
LAFILESTRU[ LNFILESTRU+12, 3] = 1
LAFILESTRU[ LNFILESTRU+12, 4] = 0
LAFILESTRU[ LNFILESTRU+13, 1] = 'cConStore'
LAFILESTRU[ LNFILESTRU+13, 2] = 'C'
LAFILESTRU[ LNFILESTRU+13, 3] = 8
LAFILESTRU[ LNFILESTRU+13, 4] = 0
FOR lnC = LNFILESTRU TO LNFILESTRU+13
  STORE ' ' TO  laFileStru[lnC,7],laFileStru[lnC ,8],;
                laFileStru[lnC,9],laFileStru[lnC ,10],;
                laFileStru[lnC ,11],laFileStru[lnC ,12],;
                laFileStru[lnC ,13],laFileStru[lnC ,14],;
                laFileStru[lnC ,15],laFileStru[lnC ,16]
      STORE 0 TO    laFileStru[lnC ,17] ,laFileStru[lnC ,18]
ENDFOR 

DIMENSION LAINDEX[ 4, 2]
LAINDEX[ 1, 1] = 'Account+Order+Store+PikTkt+STR(LineNo,6)'
LAINDEX[ 1, 2] = LCINVLINE
LAINDEX[ 2, 1] = 'Account+Order+Store+PikTkt+Style'
LAINDEX[ 2, 2] = 'Styles'
LAINDEX[ 3, 1] = 'Consol+Account+cDivision+cCurrCode+Style'
LAINDEX[ 3, 2] = 'Consol'
LAINDEX[ 4, 1] = 'Account+Order+Store+PikTkt+cDyeFlag+Dyelot'
LAINDEX[ 4, 2] = 'Dyelot'
= GFCRTTMP(LCINVLINE,@LAFILESTRU,@LAINDEX)
= GFCRTTMP(LCCONSINVD,@LAFILESTRU,'Consol+Account+cDivision+cCurrCode+cWareCode+Style',LCCONSINVD)
SELECT (LCINVLINE)
SET ORDER TO (LCINVLINE)
SCATTER BLANK MEMO MEMVAR
IF LASETUPS(15,2)='Y'
   = GFOPENTABLE('UPSBOX','UPSBOX','SH')
   
   *: C201094,1 MMT 02/02/2009 Fix bug of Error while invoicing[STart]
   SELECT UPSBOX
   *: C201094,1 MMT 02/02/2009 Fix bug of Error while invoicing[End]
   
   = AFIELDS(LAFILESTRU)
   
   *: C201094,1 MMT 02/02/2009 Fix bug of Error while invoicing[STart]
   *USE IN 'UPSBOX'
   =gfCloseTable('UPSBOX')
   *: C201094,1 MMT 02/02/2009 Fix bug of Error while invoicing[End]
   
   LNFILESTRU = ALEN(LAFILESTRU,1)
   DIMENSION LAFILESTRU[ LNFILESTRU+2, 18]
   LAFILESTRU[ LNFILESTRU+1, 1] = 'Order'
   LAFILESTRU[ LNFILESTRU+1, 2] = 'C'
   LAFILESTRU[ LNFILESTRU+1, 3] = 6
   LAFILESTRU[ LNFILESTRU+1, 4] = 0
   LAFILESTRU[ LNFILESTRU+2, 1] = 'PikTkt'
   LAFILESTRU[ LNFILESTRU+2, 2] = 'C'
   LAFILESTRU[ LNFILESTRU+2, 3] = 6
   LAFILESTRU[ LNFILESTRU+2, 4] = 0
   
   FOR lnC = LNFILESTRU TO LNFILESTRU+2
    STORE ' ' TO  laFileStru[lnC,7],laFileStru[lnC ,8],;
                laFileStru[lnC,9],laFileStru[lnC ,10],;
                laFileStru[lnC ,11],laFileStru[lnC ,12],;
                laFileStru[lnC ,13],laFileStru[lnC ,14],;
                laFileStru[lnC ,15],laFileStru[lnC ,16]
   STORE 0 TO    laFileStru[lnC ,17] ,laFileStru[lnC ,18]
ENDFOR 

   
   = GFCRTTMP(LCUPSBOX,@LAFILESTRU,'Order+Store+PikTkt+STR(CARTONS,5)',LCUPSBOX)
ENDIF
IF LLISENGLAN
   IF  .NOT. USED('InvChrg')
      = GFOPENATaBLE('InvChrg','InvChrg','SH')
   ENDIF
   SELECT INVCHRG
   = AFIELDS(LAFILESTRU)
   * HES
*!*	   LNFILESTRU = ALEN(LAFILESTRU,1)
*!*	   DIMENSION LAFILESTRU[ LNFILESTRU+2, 18]
*!*	   LAFILESTRU[ LNFILESTRU+1, 1] = 'Order'
*!*	   LAFILESTRU[ LNFILESTRU+1, 2] = 'C'
*!*	   LAFILESTRU[ LNFILESTRU+1, 3] = 6
*!*	   LAFILESTRU[ LNFILESTRU+1, 4] = 0
*!*	   LAFILESTRU[ LNFILESTRU+2, 1] = 'PikTkt'
*!*	   LAFILESTRU[ LNFILESTRU+2, 2] = 'C'
*!*	   LAFILESTRU[ LNFILESTRU+2, 3] = 6
*!*	   LAFILESTRU[ LNFILESTRU+2, 4] = 0
*!*	   FOR lnC = LNFILESTRU TO LNFILESTRU+2
*!*	    STORE ' ' TO  laFileStru[lnC,7],laFileStru[lnC ,8],;
*!*	                laFileStru[lnC,9],laFileStru[lnC ,10],;
*!*	                laFileStru[lnC ,11],laFileStru[lnC ,12],;
*!*	                laFileStru[lnC ,13],laFileStru[lnC ,14],;
*!*	                laFileStru[lnC ,15],laFileStru[lnC ,16]
*!*	   STORE 0 TO    laFileStru[lnC ,17] ,laFileStru[lnC ,18]
*!*	ENDFOR 
  * HES


   = GFCRTTMP(LCENGCHRG,@LAFILESTRU,'Order+cStore+PikTkt+cchrgcode',LCENGCHRG)
ENDIF
= GFCRTTMP(LCINSTHDR,'(Order C(6),Store C(8),PikTkt C(6), cInstmType C(1), nInstmFreq N(3),nInstIAmnt N(11,2),dInstmStDt D, nNoInstm N(3), cInstmRef C(30),nInstIPcnt N(6,2),nInstmAmnt N(10,2))','Order+Store+PikTkt',LCINSTHDR)
= GFCRTTMP(LCINSTLIN,'(Order C(6),Store C(8),PikTkt C(6), cInstalNo C(3),nInstmAmnt N(10,2),DueDate D,nInstmPcnt N(6,2),cInstmNote C(30))','Order+Store+PikTkt+cInstalNo',LCINSTLIN)
= GFCRTTMP(LCORDCANLN,'(cOrdType C(1),Order C(6),LineNo N(6),Qty1 N(6), Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6), Qty8 N(6),TotQty N(7),Cancelled D,cCancReson C(6),Price N(12,2))','CORDTYPE+ORDER+STR(LINENO,6)',LCORDCANLN)
= GFCRTTMP(LCPACKLINE,'(Pack_No C(6),Order C(6),nOrdLineNo N(6),Qty1 N(6), Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6), Qty8 N(6),TotQty N(7))','PACK_NO+ORDER+STR(nOrdLineNo,6)',LCPACKLINE)
= GFCRTTMP(LCTEMPSTK,'(Style C(19)    , Qty1 N(10)     , Qty2 N(10)     , Qty3 N(10) ,  Qty4 N(10)     , Qty5 N(10)     , Qty6 N(10)     , Qty7 N(10) ,  Qty8 N(10)     , TotQty N(10)   , cWareCode C(6) , Dyelot c(10))','Style + cWareCode + Dyelot',LCTEMPSTK)

*:**************************************************************************
*:* Name        : LFGETORDER
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  Check Sales order Function
*:***************************************************************************
FUNCTION  LFGETORDER
PARAMETER LCORDERNO, LCSTORENO, LLPICKED, LCPIKFROM, LCPIKTO, LLSCOPE
PRIVATE LDDUEDATE, LCSTORE, LCCUSTLINK, LCCUSTSALE, LCSALEREP1, LCSALEREP2, LCCURRCODE, LNEXRATE, LNCURRUNIT, LLPACKED, LCLINESCON, LNTAXQTY, LNTAXRATE, LLBACKORD
PRIVATE LNALLCARTN
LNALLCARTN = 0
IF 'AL'$ oAriaApplication.CompanyInstalledModules          
   IF LLOPNFILES
      = GFOPENTabLE('PACK_HDR','Orderpck','SH')
      = GFOPENTabLE('PACK_LIN','PACK_LIN','SH')
   ENDIF
ENDIF
IF  .NOT. LLSCOPE
   IF LLOPNFILES
      = GFOPENtaBLE('STYLE','STYLE','SH')
      = GFOPENtaBLE('STYDYE','STYDYE','SH')
      = GFOPENtaBLE('SCALE','SCALE','SH')
   ENDIF
   SELECT STYLE
   *SET RELATION TO 'S'+SCALE INTO SCALE
   SELECT (LCINVLINE)
*!*	   SET RELATION TO STYLE INTO STYLE
*!*	   SET RELATION TO STYLE+CWARECODE+DYELOT INTO STYDYE ADDITIVE
ENDIF
IF LLPICKED
   LCLINESCON = 'Picked .AND. TotPik > 0'
   DO CASE
      CASE  .NOT. EMPTY(LCPIKFROM) .AND. EMPTY(LCPIKTO)
         LCLINESCON = LCLINESCON+'AND PikTkt >= lcPikFrom'
      CASE EMPTY(LCPIKFROM) .AND.  .NOT. EMPTY(LCPIKTO)
         LCLINESCON = LCLINESCON+'AND PikTkt <= lcPikTo'
      CASE  .NOT. EMPTY(LCPIKFROM) .AND.  .NOT. EMPTY(LCPIKTO)
         LCLINESCON = LCLINESCON+'AND BETWEEN(PikTkt,lcPikFrom,lcPikTo)'
   ENDCASE
   DO CASE
      CASE LLSCOPE .AND.  .NOT. EMPTY(LDFROMPICK) .AND. EMPTY(LDTOPICK)
         LCLINESCON = LCLINESCON+'AND pikdate >= ldFromPick'
      CASE LLSCOPE .AND. EMPTY(LDFROMPICK) .AND.  .NOT. EMPTY(LDTOPICK)
         LCLINESCON = LCLINESCON+'AND pikdate <= ldToPick'
      CASE LLSCOPE .AND.  .NOT. EMPTY(LDFROMPICK) .AND.  .NOT. EMPTY(LDTOPICK)
         LCLINESCON = LCLINESCON+'AND BETWEEN(pikdate,ldFromPick,ldToPick)'
   ENDCASE
ELSE
   LCLINESCON = 'TotQty >0'
ENDIF
IF LLSCOPE .AND.  .NOT. EMPTY(LASTYTARGE)
   LCLINESCON = LCLINESCON+' AND ASCAN(laStyTarge,ALLTRIM(SUBSTR(Style,1,LEN(lcMjrMsk))))>0'
ENDIF
= GFRLTFLD(ORDHDR.CTERMCODE,@LATRLTFLD,'CTERMCODE')
LNEOMDAY = IIF(TYPE('lnEOMDay')<>'N' .OR. LNEOMDAY=0,20,LNEOMDAY-1)
LDDEFINVDA = oAriaApplication.SystemDate
IF LLISENGLAN
   LDDUEDATE = IIF(LCTEOM<>'Y',LDDEFINVDA+LNTDAYSDUE,CTOD('01'+SUBSTR(DTOC(GOMONTH(LDDEFINVDA,1)),3))-1+LNTDAYSDUE)
ELSE
   LDDUEDATE = IIF(LCTEOM<>'Y',LDDEFINVDA+LNTDAYSDUE,GOMONTH(CTOD(SUBSTR(DTOC(LDDEFINVDA),1,3)+'10'+SUBSTR(DTOC(LDDEFINVDA),6,5)),IIF(DAY(LDDEFINVDA)>LNEOMDAY,2,1))+LNTDAYSDUE)
ENDIF
LCCURRCODE = IIF(EMPTY(ORDHDR.CCURRCODE),oAriaApplication.BaseCurrency,ORDHDR.CCURRCODE)
STORE 1 TO LNEXRATE, LNCURRUNIT
IF LCCURRCODE<>oAriaApplication.BaseCurrency                     
   LNEXRATE = GFCHKRATE('lnCurrUnit',LCCURRCODE,LDDEFINVDA,.T.,.F.,.F.,LLEDITEXRT)
   IF LNEXRATE=0
      IF LLEDITEXRT
         = GFMODALGEN('INM00262B00000','ALERT',ALLTRIM(LCCURRCODE)+'|'+ALLTRIM(oAriaApplication.BaseCurrency)+'|'+DTOC(LDDEFINVDA))
      ELSE
         LCCURRCODE = oAriaApplication.BaseCurrency                     
         STORE 1 TO LNEXRATE, LNCURRUNIT
      ENDIF
   ENDIF
ENDIF
= gfSEEK('M'+ORDHDR.ACCOUNT,'Customer')
IF EMPTY(ORDHDR.STORE)
   LCPHONE = CUSTOMER.PHONE1
ENDIF
LLBACKORD = INLIST(IIF(EMPTY(CUSTOMER.CBACKORD),LASETUPS(23,2),CUSTOMER.CBACKORD),'A','I')
SELECT ORDLINE
gfSetOrder('Ordlinst')
= gfSEEK('O'+LCORDERNO+ALLTRIM(LCSTORENO))
DO WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)='O'+LCORDERNO+ALLTRIM(LCSTORENO)
   LCSTORE = STORE
   = IIF(EMPTY(STORE),gfSEEK('M'+ACCOUNT,'Customer'),gfSEEK('S'+ACCOUNT+STORE,'Customer'))
   LCSALEREP1 = ORDHDR.REP1
   LNCOMM1 = ORDHDR.COMM1
   LCSALEREP2 = ORDHDR.REP2
   LNCOMM2 = ORDHDR.COMM2
   IF ORDHDR.MULTI='Y' .AND. EMPTY(ORDHDR.REP1) .AND. EMPTY(ORDHDR.REP2)
      LCSALEREP1 = CUSTOMER.SALESREP
      LNCOMM1 = CUSTOMER.COMM
      LCSALEREP2 = CUSTOMER.REP2
      LNCOMM2 = CUSTOMER.COMM2
   ENDIF
   SCAN REST FOR EVALUATE(LCLINESCON) WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)='O'+LCORDERNO+LCSTORE
      LLPACKED = .F.
      LCSEEKKEY = IIF(EMPTY(ORDLINE.PIKTKT),'OrdLine.Order+OrdLine.Store','OrdLine.Order+OrdLine.Store+OrdLine.PikTkt')
      IF 'AL' $ oAriaApplication.CompanyInstalledModules AND  gfSEEK(&lcSeekKey,'Pack_Hdr') AND  gfSEEK(PACK_HDR.Pack_No,'Pack_Lin')
         IF  .NOT. SEEK(IIF( .NOT. EMPTY(ORDLINE.PIKTKT),ORDLINE.PIKTKT,PACK_HDR.PACK_NO)+ORDLINE.ORDER,LCPACKLINE)
            = LFPACKLIN(ORDLINE.ORDER,IIF( .NOT. EMPTY(ORDLINE.PIKTKT),ORDLINE.PIKTKT,PACK_HDR.PACK_NO))
         ENDIF
         LLPACKED = SEEK(IIF( .NOT. EMPTY(ORDLINE.PIKTKT),ORDLINE.PIKTKT,PACK_HDR.PACK_NO)+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),LCPACKLINE)
         SELECT ORDLINE
      ENDIF
      SCATTER MEMO MEMVAR
      M.GROS_PRICE = IIF(M.GROS_PRICE=0,M.PRICE,M.GROS_PRICE)
      = gfSEEK(M.STYLE,'Style')
      STORE 0 TO LNTAXQTY, LNTAXRATE
      IF LLISENGLAN .AND. LASETUPS(9,2)='Y' .AND.  .NOT. CUSTOMER.LVATEXEM .AND. STYLE.NTAXBREAK<>0
         = GFRLTFLD(STYLE.CTAXCODE,@LAENGSTYTA,'CTAXCODE')
      ENDIF
      M.LTAXABLE = STYLE.LTAXABLE
      IF LLPACKED
         m.Pik1 = &lcPackLine..Qty1
         m.Pik2 = &lcPackLine..Qty2
         m.Pik3 = &lcPackLine..Qty3
         m.Pik4 = &lcPackLine..Qty4
         m.Pik5 = &lcPackLine..Qty5
         m.Pik6 = &lcPackLine..Qty6
         m.Pik7 = &lcPackLine..Qty7
         m.Pik8 = &lcPackLine..Qty8
         m.TotPik = &lcPackLine..TotQty
         IF  .NOT. EMPTY(M.PREPAK)
            = gfSEEK('P'+M.SCALE+M.PREPAK,'Scale')
            IF SCALE.PP1/SCALE.PPTOT*M.TOTPIK<>M.PIK1 .OR. SCALE.PP2/SCALE.PPTOT*M.TOTPIK<>M.PIK2 .OR. SCALE.PP3/SCALE.PPTOT*M.TOTPIK<>M.PIK3 .OR. SCALE.PP4/SCALE.PPTOT*M.TOTPIK<>M.PIK4 .OR. SCALE.PP5/SCALE.PPTOT*M.TOTPIK<>M.PIK5 .OR. SCALE.PP6/SCALE.PPTOT*M.TOTPIK<>M.PIK6 .OR. SCALE.PP7/SCALE.PPTOT*M.TOTPIK<>M.PIK7 .OR. SCALE.PP8/SCALE.PPTOT*M.TOTPIK<>M.PIK8
               M.PREPAK = ''
               M.PPQTY = 0
            ELSE
               M.PPQTY = IIF(SCALE.PPTOT=0,0,M.TOTPIK/SCALE.PPTOT)
            ENDIF
            = gfSEEK('S'+M.SCALE,'Scale')
         ENDIF
      ENDIF
      STORE 0 TO LNTOTPCK
      STORE .F. TO LLPIKQTY
      LNTOTPCK = ORDLINE.NPCK1+ORDLINE.NPCK2+ORDLINE.NPCK3+ORDLINE.NPCK4+ORDLINE.NPCK5+ORDLINE.NPCK6+ORDLINE.NPCK7+ORDLINE.NPCK8
      LLPIKQTY = LLPACKED .AND. LNTOTPCK>0
      lcInvNo = ''
      IF  .NOT. SEEK(M.ACCOUNT+M.ORDER+M.STORE+M.PIKTKT,LCINVHDR)
*!*	         lcInvNo=IIF(EMPTY(&lcInvHdr..cFacCode), gfSequence('INVOICE','','',&lcInvHdr..cDivision), gfSequence('CFINVOICE' ,'','',&lcInvHdr..cDivision))
*!*	         DO WHILE gfSEEK(LCINVNO,'InvHdr')
*!*	            lcInvNo=IIF(EMPTY(&lcInvHdr..cFacCode), gfSequence('INVOICE','','',&lcInvHdr..cDivision), gfSequence('CFINVOICE' ,'','',&lcInvHdr..cDivision))
*!*	         ENDDO
*!*	         SELECT INVHDR
*!*	         APPEND BLANK
*!*	         = RLOCK()
*!*	         REPLACE INVOICE WITH LCINVNO
*!*	         =gfReplace("")
*!*	         UNLOCK
      ENDIF
      INSERT INTO (LCINVLINE) ( INVOICE, ORDER, ACCOUNT, LINENO, STORE, PIKTKT, STYLE, DYELOT, NOTE_MEM, COMM1, COMM2, BOOK1, BOOK2, BOOK3, BOOK4, BOOK5, BOOK6, BOOK7, BOOK8, TOTBOOK, FLAG, PIK1, PIK2, PIK3, PIK4, PIK5, PIK6, PIK7, PIK8, TOTPIK, QTY1, QTY2, QTY3, QTY4, QTY5, QTY6, QTY7, QTY8, TOTQTY, PRICE, PACK_ID, GROS_PRICE, DISC_PCNT, LPACKED, LBACKORD, NTAXRATE, GROUP, PREPAK, DESC1, SEASON, PPQTY, SCALE, CWARECODE, CONSOL, CDIVISION, CCURRCODE, LTAXABLE, CDYEFLAG, CWARECODE, GL_SALES ) VALUE ( LCINVNO, M.ORDER, M.ACCOUNT, M.LINENO, M.STORE, M.PIKTKT, M.STYLE, M.DYELOT, M.NOTE_MEM, M.COMM1, M.COMM2, M.QTY1, M.QTY2, M.QTY3, M.QTY4, M.QTY5, M.QTY6, M.QTY7, M.QTY8, M.TOTQTY, IIF(LLPICKED,'B',' '), M.PIK1, M.PIK2, M.PIK3, M.PIK4, M.PIK5, M.PIK6, M.PIK7, M.PIK8, M.TOTPIK, IIF(LLPICKED .OR. LLPIKQTY,M.PIK1,M.QTY1), IIF(LLPICKED .OR. LLPIKQTY,M.PIK2,M.QTY2), IIF(LLPICKED .OR. LLPIKQTY,M.PIK3,M.QTY3), IIF(LLPICKED .OR. LLPIKQTY,M.PIK4,M.QTY4), IIF(LLPICKED .OR. LLPIKQTY,M.PIK5,M.QTY5), IIF(LLPICKED .OR. LLPIKQTY,M.PIK6,M.QTY6), IIF(LLPICKED .OR. LLPIKQTY,M.PIK7,M.QTY7), IIF(LLPICKED .OR. LLPIKQTY,M.PIK8,M.QTY8), IIF(LLPICKED .OR. LLPIKQTY,M.TOTPIK,M.TOTQTY), M.PRICE, M.PACK_ID, M.GROS_PRICE, M.DISC_PCNT, LLPACKED, LLBACKORD, LNTAXRATE, M.GROUP, M.PREPAK, M.DESC1, M.SEASON, M.PPQTY, M.SCALE, M.CWARECODE, 'N', ORDHDR.CDIVISION, ORDHDR.CCURRCODE, M.LTAXABLE, STYLE.CDYE_FLG, M.CWARECODE, ORDLINE.GL_SALES )
      SELECT (LCINVLINE)
      IF INLIST(LASETUPS(7,2),'F','L')
         * HES
*!*	         = LFUPSTKQTY()
         * HES
      ENDIF
      = gfSEEK(M.STYLE,'Style')
      IF LLISENGLAN .AND. LASETUPS(9,2)='Y' .AND.  .NOT. CUSTOMER.LVATEXEM .AND. STYLE.NTAXBREAK<>0
         FOR LNCOUNT = STYLE.NTAXBREAK TO 8
            LNTAXQTY = LNTAXQTY+EVALUATE(LCINVLINE+'.Qty'+STR(LNCOUNT,1))
         ENDFOR
      ENDIF
      SCATTER MEMVAR FIELDS QTY1, QTY2, QTY3, QTY4, QTY5, QTY6, QTY7, QTY8, TOTQTY
      SELECT (LCINVHDR)
      LLINVPCK =  .NOT. EMPTY(M.PIKTKT) .AND. SEEK(M.ORDER+M.STORE+M.PIKTKT,'Pack_Hdr')
      IF  .NOT. SEEK(M.ACCOUNT+M.ORDER+M.STORE+M.PIKTKT)
         APPEND BLANK
         REPLACE CSELECT WITH '»',;
         		 INVOICE WITH LCINVNO,;
		         ORDER WITH M.ORDER,;
		         STORE WITH M.STORE,;
		         PIKTKT WITH M.PIKTKT,;
		         CUSTPO WITH IIF(EMPTY(M.CUSTPO),ORDHDR.CUSTPO,M.CUSTPO),;
		         DIST_CTR WITH CUSTOMER.DIST_CTR,;
		         ACCOUNT WITH ORDHDR.ACCOUNT,;
		         CTERMCODE WITH ORDHDR.CTERMCODE,;
		         SPCINST WITH ORDHDR.SPCINST,;
		         LUPSINS WITH (ORDHDR.CINSUR='Y'),;
		         REP1 WITH LCSALEREP1,;
		         COMM1 WITH LNCOMM1,;
		         REP2 WITH LCSALEREP2,;
		         COMM2 WITH LNCOMM2,;
		         NOTE1 WITH ORDHDR.NOTE1,;
		         NOTE2 WITH ORDHDR.NOTE2,;
		         LCOMPUPS WITH .T.,;
		         CONSOL WITH 'N'
		         
         REPLACE LASTLINE WITH ORDHDR.LASTLINE
         IF LLINVPCK .AND.  .NOT. EMPTY(PACK_HDR.SHIP_DATE)
            IF LLISENGLAN
               LDDUEDATE = IIF(LCTEOM<>'Y',PACK_HDR.SHIP_DATE+LNTDAYSDUE,CTOD('01'+SUBSTR(DTOC(GOMONTH(PACK_HDR.SHIP_DATE,1)),3))-1+LNTDAYSDUE)
            ELSE
               LDDUEDATE = IIF(LCTEOM<>'Y',PACK_HDR.SHIP_DATE+LNTDAYSDUE,GOMONTH(CTOD(SUBSTR(DTOC(PACK_HDR.SHIP_DATE),1,3)+'10'+SUBSTR(DTOC(PACK_HDR.SHIP_DATE),6,5)),IIF(DAY(PACK_HDR.SHIP_DATE)>LNEOMDAY,2,1))+LNTDAYSDUE)
            ENDIF
         ENDIF
         REPLACE DISCPCNT WITH ORDHDR.DISC,;
         		 INVDATE WITH LDDEFINVDA,;
         		 SHIPDATE WITH IIF(LLINVPCK .AND.  .NOT. EMPTY(PACK_HDR.SHIP_DATE),PACK_HDR.SHIP_DATE,LDDEFINVDA),;
         		 DPOSTDATE WITH LDDEFPSTDA,;
         		 DUEDATE WITH LDDUEDATE,;
         		 DEPT WITH ORDHDR.DEPT,;
         		 CFACCODE WITH ORDHDR.CFACCODE,;
         		 APPROVAL WITH ORDHDR.APPROVAL,;
         		 APPRAMT WITH ORDHDR.APPRAMT,;
         		 SEASON WITH ORDHDR.SEASON,;
         		 CDIVISION WITH ORDHDR.CDIVISION,;
         		 UPSZONE WITH CUSTOMER.UPSZONE,;
         		 PHONE WITH IIF(EMPTY(ORDHDR.STORE),LCPHONE,CUSTOMER.PHONE1),;
         		 CWARECODE WITH IIF(LLPICKED .AND. gfSEEK(M.ORDER+M.PIKTKT,'PikTkt'),PIKTKT.CWARECODE,ORDHDR.CWARECODE),;
         		 TRDE_DISC WITH LNTERDISCR,;
         		 TAX_RATE WITH IIF(LASETUPS(9,2)<>'Y',0,IIF(LLISCANADA,LASETUPS(10,2),CUSTOMER.NTAXRATE)),;
         		 NPSTRATE WITH IIF(LASETUPS(9,2)='Y' .AND. LLISCANADA,CUSTOMER.NTAXRATE,0),;
         		 CTAXRULE WITH IIF(LASETUPS(9,2)='Y' .AND. LLISCANADA,CUSTOMER.CTAXRULE,''),;
         		 COD_FLAG WITH IIF(LCTCOD='Y','Y','N'),;
         		 STATUS WITH ORDHDR.STATUS,;
         		 CCURRCODE WITH LCCURRCODE,;
         		 NEXRATE WITH LNEXRATE,;
         		 NCURRUNIT WITH LNCURRUNIT,;
         		 DADD_DATE WITH oAriaApplication.SystemDate,;
         		 CADD_TIME WITH TIME(),;
         		 CADD_USER WITH oAriaApplication.User_ID
         		                           
         REPLACE NHSTRATE WITH IIF(LASETUPS(9,2)<>'Y' .OR.  .NOT. LLISCANADA,0,LASETUPS(26,2))
         IF LLINVPCK
            REPLACE WEIGHT WITH PACK_HDR.TOT_WGHT,;
            		NCARTONS WITH PACK_HDR.TOT_CART,;
            		CARTONS WITH PACK_HDR.TOT_CART
            REPLACE BOL_NO WITH PACK_HDR.BILL_LADG
         ENDIF
      ENDIF
      LNCARTONS = NCARTONS+IIF(STYLE.QTY_CTN>0,M.TOTQTY/STYLE.QTY_CTN,0)
      LNALLCARTN = IIF(CEILING(LNCARTONS)=0,1,CEILING(LNCARTONS))
      *: B610982,1 MMT  04/19/2015 Custom Scan Pick ticket program include trade disc in vat[T20150319.0010][Start]
*!*	      REPLACE Ordered   WITH Ordered  + &lcInvLine..TotBook ,;
*!*	      		  Ship      WITH Ship     + m.TotQty ,;
*!*	      		  ShipAmt   WITH ShipAmt  + m.TotQty*m.Price ,;
*!*	      		  Discount  WITH -ShipAmt * DiscPcnt/100,;
*!*	      		  Weight    WITH Weight   + IIF(llInvPck,0,m.TotQty*Style.nStyWeight) ,;
*!*	      		  nCartons  WITH IIF(llInvPck,nCartons,lnCartons) ,;
*!*	      		  Cartons   WITH IIF(llInvPck,Cartons,lnAllCartn) ,;
*!*	      		  Picked    WITH Picked  + m.TotPik ,;
*!*	      		  ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia, IIF(llInvPck,IIF(ALLTRIM(Pack_Hdr.ShipVia)='*',Customer.ShipVia,Pack_Hdr.ShipVia),IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia))),;
*!*	      		  nMerchTax WITH nMerchTax + lnTaxQty * m.Price * lnTaxRate/100 , ;
*!*	      		  Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
*!*	      		  Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,;
*!*	      		  TotalChg  WITH ShipAmt+Tax_Amt+Discount,;
*!*	      		  nTaxDue   WITH nTaxDue + IIF(m.lTaxable,m.TotQty*m.Price,0)
      REPLACE Ordered   WITH Ordered  + &lcInvLine..TotBook ,;
      		  Ship      WITH Ship     + m.TotQty ,;
      		  ShipAmt   WITH ShipAmt  + m.TotQty*m.Price ,;
      		  Discount  WITH -ShipAmt * DiscPcnt/100,;
      		  Weight    WITH Weight   + IIF(llInvPck,0,m.TotQty*Style.nStyWeight) ,;
      		  nCartons  WITH IIF(llInvPck,nCartons,lnCartons) ,;
      		  Cartons   WITH IIF(llInvPck,Cartons,lnAllCartn) ,;
      		  Picked    WITH Picked  + m.TotPik ,;
      		  ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia, IIF(llInvPck,IIF(ALLTRIM(Pack_Hdr.ShipVia)='*',Customer.ShipVia,Pack_Hdr.ShipVia),IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia))),;
      		  nMerchTax WITH nMerchTax + lnTaxQty * m.Price * lnTaxRate/100 , ;
      		  Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100 ,;
      		  Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,;
      		  TotalChg  WITH ShipAmt+Tax_Amt+Discount,;
      		  nTaxDue   WITH nTaxDue + IIF(m.lTaxable,m.TotQty*m.Price,0)
      *: B610982,1 MMT  04/19/2015 Custom Scan Pick ticket program include trade disc in vat[T20150319.0010][End]      		  
      IF LLISENGLAN
         REPLACE NTRUESHIP WITH NTRUESHIP+(M.TOTQTY*M.PRICE), NTRUEDSCNT WITH -NTRUESHIP*DISCPCNT/100, NTRUECHRG WITH NTRUESHIP+NMERCHTAX+NTRUEDSCNT
      ENDIF
   ENDSCAN
ENDDO
IF  .NOT. LLSCOPE .AND. EOF(LCINVHDR)
   = GFMODALGEN('TRM40128B00000','ALERT')
   RETURN (.F.)
ENDIF
RETURN

*:**************************************************************************
*:* Name        : LFUPDIRCHG
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  update inv. charges  Function
*:***************************************************************************
FUNCTION LFUPDIRCHG
PRIVATE LNSLCT, LCTAXCODE, LCGLACCNT, LARLTFLD, LNTAXRATE
LNSLCT = SELECT()
DIMENSION LARLTFLD[ 2, 2], LARLTFLD2[ 1, 2]
LARLTFLD[ 1, 1] = 'CTAXCODE'
LARLTFLD[ 1, 2] = 'lcTaxCode'
LARLTFLD[ 2, 1] = 'CFRGTACNT'
LARLTFLD[ 2, 2] = 'lcGlAccnt'
LARLTFLD2[ 1, 1] = 'NTAXRATE'
LARLTFLD2[ 1, 2] = 'lnTaxRate'
IF  .NOT. USED('ORDCHG')
   = GFOPENTabLE('ORDCHG','ORDCHG','SH')
ENDIF

*: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[Start]
*IF SEEK(LADATA(1),'ORDCHG') .AND.  .NOT. ORDCHG.INVOICED
IF gfSEEK(LADATA(1),'ORDCHG') .AND.  .NOT. ORDCHG.INVOICED
*: B608854,2 MMT 05/19/2009 ORDCHG Table is SQL table Not Fox Table[End]

   SELECT ORDCHG
   SCAN REST WHILE ORDER+CORDCHG=LADATA(1)
      IF  .NOT. SEEK(LADATA(1)+LADATA(5)+LADATA(3)+ORDCHG.CORDCHG,LCENGCHRG)
         LCGLACCNT = ' '
         LCTAXCODE = ' '
         = GFRLTFLD(ORDCHG.CORDCHG,@LARLTFLD,'CCHRGCODE')
         LNTAXRATE = 0
         = GFRLTFLD(LCTAXCODE,@LARLTFLD2,'CTAXCODE')
         SELECT &lcEngChrg
         APPEND BLANK
         REPLACE ORDER WITH LADATA(1),;
         		 PIKTKT WITH LADATA(3),;
         		 CCHRGCODE WITH ORDCHG.CORDCHG,;
         		 NCHRGAMNT WITH ORDCHG.NORDCHG,;
         		 CFRGTACNT WITH LCGLACCNT,;
         		 NTAXRATE WITH LNTAXRATE
         *: B609682,1 MMT 10/04/2011 INVCHRG records sometimes not being created[Start]
         REPLACE cStore WITH LADATA(5)         
         *: B609682,1 MMT 10/04/2011 INVCHRG records sometimes not being created[END]
         M.NCHARGES = M.NCHARGES+ORDCHG.NORDCHG
         M.TAX_AMT = M.TAX_AMT+NCHRGAMNT*NTAXRATE
      ENDIF
   ENDSCAN
   = LFVCHARGES()
ENDIF
SELECT (LNSLCT)


*:**************************************************************************
*:* Name        : LFVCHARGES
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  Validate inv. charges  Function
*:***************************************************************************
FUNCTION  LFVCHARGES
PRIVATE LNOCHARGES, LNOCHRGTAX
LASCRMODE = .F.
DIMENSION LASCRMODE[4]
LASCRMODE[ 4] = .T.
LNOCHARGES = M.NCHARGES
LNOCHRGTAX = M.NCHRGTAX
DO GPCHARGES WITH LCENGCHRG, SPACE(6), LADATA(4), LADATA(1), LADATA(5), LADATA(3), M.TRDE_DISC, 'm.nCharges', 'm.nChrgTax'
SELECT (LCINVHDR)
*: B609682,1 MMT 10/04/2011 INVCHRG records sometimes not being created[Start]
lcOldHdOrder = ORDER()
*: B609682,1 MMT 10/04/2011 INVCHRG records sometimes not being created[END]

LNRECNO = RECNO()
SET ORDER TO 'CONSOL'
IF SEEK('Y'+ACCOUNT+CDIVISION+CCURRCODE)
   LCCURRKEY = ACCOUNT+CDIVISION+CCURRCODE
   LNCHRGTAX = 0
   LNTAX_AMT = 0
   = SEEK('N'+LCCURRKEY)
   SCAN REST WHILE CONSOL+ACCOUNT+CDIVISION+CCURRCODE='N'+LCCURRKEY
      LNDISCPCNT = DISCPCNT
      LNTRDE_DIS = TRDE_DISC
      LCCHRGKEY = ORDER+STORE+PIKTKT
      SELECT (LCENGCHRG)
      = SEEK(LCCHRGKEY)
      *: B610982,1 MMT  04/19/2015 Custom Scan Pick ticket program include trade disc in vat[T20150319.0010][Start]
      *SUM REST WHILE ORDER+CSTORE+PIKTKT+CCHRGCODE=LCCHRGKEY NCHRGAMNT*(1-LNTRDE_DIS/100)*NTAXRATE/100 TO LNINVCHRGT
      SUM REST WHILE ORDER+CSTORE+PIKTKT+CCHRGCODE=LCCHRGKEY NCHRGAMNT*NTAXRATE/100 TO LNINVCHRGT
      *: B610982,1 MMT  04/19/2015 Custom Scan Pick ticket program include trade disc in vat[T20150319.0010][End]
      LNCHRGTAX = LNCHRGTAX+LNINVCHRGT
      *: B610977,1 MMT 04/07/2015 Fix the Scan pick and pack program for VAT calculcations[T20150319.0010][Start]
      *lnTax_Amt  = lnTax_Amt + lninvChrgTax + &lcInvHdr..nMerchTax*(100-lnDiscPcnt)/100*(100-lnTrde_Disc)/100
      lnTax_Amt  = lnTax_Amt + lninvChrgTax + &lcInvHdr..nMerchTax*(100-lnDiscPcnt)/100
      *: B610977,1 MMT 04/07/2015 Fix the Scan pick and pack program for VAT calculcations[T20150319.0010][End]
   ENDSCAN
   = SEEK('Y'+LCCURRKEY)
   = RLOCK()
   REPLACE NCHARGES WITH NCHARGES-LNOCHARGES+M.NCHARGES, NCHRGTAX WITH LNCHRGTAX, TAX_AMT WITH LNTAX_AMT, COD_AMT WITH IIF(COD_FLAG='Y',SHIPAMT+NCHARGES+TAX_AMT+DISCOUNT,0), TOTALCHG WITH SHIPAMT+NCHARGES+TAX_AMT+DISCOUNT
   UNLOCK
ENDIF
*: B609682,1 MMT 10/04/2011 INVCHRG records sometimes not being created[Start]
*SET ORDER TO (LCINVHDR)
SELECT (LCINVHDR)
SET ORDER TO (lcOldHdOrder)
*: B609682,1 MMT 10/04/2011 INVCHRG records sometimes not being created[END]

GOTO LNRECNO
= RLOCK()
*: B610982,1 MMT  04/19/2015 Custom Scan Pick ticket program include trade disc in vat[T20150319.0010][Start]
*M.TAX_AMT = M.NCHRGTAX+NMERCHTAX*(100-M.DISCPCNT)/100*(100-M.TRDE_DISC)/100
M.TAX_AMT = M.NCHRGTAX+NMERCHTAX*(100-M.DISCPCNT)/100
*: B610982,1 MMT  04/19/2015 Custom Scan Pick ticket program include trade disc in vat[T20150319.0010][End]
REPLACE NCHARGES WITH M.NCHARGES, NCHRGTAX WITH M.NCHRGTAX, TAX_AMT WITH M.TAX_AMT, COD_AMT WITH IIF(COD_FLAG='Y',SHIPAMT+NCHARGES+TAX_AMT+DISCOUNT,0), TOTALCHG WITH SHIPAMT+NCHARGES+TAX_AMT+DISCOUNT
*!*	IF ASCAN(LAEVNTTRIG,PADR('TAXBYWHS',10))<>0
*!*	   = GFDOTRIGER('ARIINV',PADR('TAXBYWHS',10))
*!*	ENDIF
UNLOCK
M.COD_AMT = COD_AMT
M.TOTALCHG = TOTALCHG



PROCEDURE GPCHARGES
PARAMETER LCCHRGFILE, LCINVOICE, LCACCOUNT, LCORDERNO, LCSTORE, LCPIKTKT, LNTRDDSC, LCTOTCHRG, LCTOTTAX
PRIVATE LNALIAS, LACHARGES, LNCHRGMARK, LNTOTCHRG, LNTOTTAX, LLCOMPTAX, LACHRLTFLD
PRIVATE LNOLDCHRG
STORE 0 TO LNOLDCHRG
LNALIAS = SELECT()
LLVATEXMP = .F.
IF  .NOT. EMPTY(STORE)
   LNCUSREC = RECNO('CUSTOMER')
   = gfSEEK('S'+ACCOUNT+STORE,'CUSTOMER')
   LLVATEXMP = CUSTOMER.LVATEXEM
   GOTO IN CUSTOMER LNCUSREC
ELSE
   LLVATEXMP = CUSTOMER.LVATEXEM
ENDIF
LLCOMPTAX = (gfGetMemVar('M_TAX',GCACT_COMP)='Y') .AND.  .NOT. LLVATEXMP
LLGLLINK = (gfGetMemVar('M_LINK_GL',GCACT_COMP)='Y')
LNCHRGMARK = 0
DIMENSION LALCODES[ 1, 10], LACHARGES[ 1, 2]
STORE '' TO LACHARGES
lnTotChrg = &lcTotChrg
= GFOPENTabLE('InvChrg','InvChrg','SH')
SELECT (LCCHRGFILE)
IF LASCRMODE(4)
   = SEEK(LCORDERNO+LCSTORE+LCPIKTKT)
   SUM REST WHILE ORDER+CSTORE+PIKTKT+CCHRGCODE=LCORDERNO+LCSTORE+LCPIKTKT NCHRGAMNT*(1-LNTRDDSC/100)*NTAXRATE/100 TO LNTOTTAX
   = SEEK(LCORDERNO+LCSTORE+LCPIKTKT)
ELSE
   = SEEK(LCINVOICE+LCSTORE)
   SUM REST WHILE INVOICE+CSTORE+CCHRGCODE=LCINVOICE+LCSTORE NCHRGAMNT*(1-LNTRDDSC/100)*NTAXRATE/100 TO LNTOTTAX
   = SEEK(LCINVOICE+LCSTORE)
ENDIF
LLNEWCHRG = .F.
SCATTER MEMVAR
*!*	LALCODES[ 1, 1] = 'CCHRGCODE'
*!*	LALCODES[ 1, 2] = 'laCharges'
*!*	LALCODES[ 1, 3] = 'puCharges'
*!*	LALCODES[ 1, 4] = ''
*!*	LALCODES[ 1, 5] = .F.
*!*	LALCODES[ 1, 6] = .F.
*!*	LALCODES[ 1, 7] = LCCHRGFILE
*!*	LALCODES[ 1, 8] = LCCHRGFILE
*!*	LALCODES[ 1, 9] = IIF(LASCRMODE(2),'lcInvoice+lcStore+m.cchrgcode','lcOrderNo+lcStore+lcPikTkt+m.cchrgcode')
*!*	LALCODES[ 1, 10] = 'CCHRGCODE'
*!*	= GFWCODEPOP(@LALCODES,'CCHRGCODE','N')
IF LASCRMODE(4)
   SELECT (LCCHRGFILE)
   = SEEK(IIF(LASCRMODE(2),LCINVOICE,LCORDERNO+LCSTORE+LCPIKTKT))
   SUM REST WHILE ORDER+CSTORE+PIKTKT+CCHRGCODE=LCORDERNO+LCSTORE+LCPIKTKT NCHRGAMNT*(1-LNTRDDSC/100)*NTAXRATE/100 TO LNTOTTAX
   &lcTotChrg = lnTotChrg
   &lcTotTax  = lnTotTax
ENDIF
SELECT (LNALIAS)

PROCEDURE LPRELEASLK
IF RECCOUNT(LCINVHDR)=0
   RETURN
ENDIF
PRIVATE LCALIAS, LNINVHDRRC, LNORDHDRRC, LCORDHDRTG, LCSETDELET
LCALIAS = ALIAS()
LCSETDELET = SET('DELETE')
SET DELETED OFF
LNINVHDRRC = RECNO(LCINVHDR)
LNORDHDRRC = RECNO('OrdHdr')
LCORDHDRTG = ORDER('OrdHdr')
SET ORDER TO OrdHdr IN ORDHDR
SELECT (LCINVHDR)
SCAN FOR  .NOT. EMPTY(ORDER)
   IF gfSEEK('O'+ORDER,'OrdHdr') .AND. ORDHDR.LLOK_STAT
      SELECT ORDHDR
      = RLOCK()
      REPLACE LLOK_STAT WITH .F., CLOK_USER WITH '', DLOK_DATE WITH {}, CLOK_TIME WITH ''
      UNLOCK
   ENDIF
ENDSCAN
SET ORDER TO (LCORDHDRTG) IN ORDHDR
IF BETWEEN(LNORDHDRRC,1,RECCOUNT('OrdHdr'))
   GOTO IN ORDHDR LNORDHDRRC
ENDIF
IF BETWEEN(LNINVHDRRC,1,RECCOUNT('InvHdr'))
   GOTO IN INVHDR LNINVHDRRC
ENDIF
SET DELETE &lcSetDelet.
IF  .NOT. EMPTY(LCALIAS)
   SELECT (LCALIAS)
ENDIF

*:**************************************************************************
*:* Name        : LFPRNPACK
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  print packing list  Function
*:***************************************************************************
FUNCTION  LFPRNPACK
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [Start]
*PRIVATE LNSLCT, LNRESP, LCRPEXP, LCPACKLST, LCS, LCSETPROCD
PRIVATE LNSLCT,LCS
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [End]
LNSLCT = SELECT()
LCS = IIF(RECCOUNT(LCTMPPKTK)>1,'s','')
IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Do you want to print the created Packing list&lcS') = 1
*!*	   lcPacks =gfTemPname()
*!*	   DIMENSION laArrStr[2,4]
*!*	   laArrStr[1,1] = 'KEYEXP'
*!*	   laArrStr[1,2] = 'C'
*!*	   laArrStr[1,3] = 6
*!*	   laArrStr[1,4] = 0
*!*	   
*!*	   laArrStr[2,1] = 'Pack_no'
*!*	   laArrStr[2,2] = 'C'
*!*	   laArrStr[2,3] = 6
*!*	   laArrStr[2,4] = 0
*!*	   gfCrtTmp(lcPacks ,@laArrStr,"KEYEXP",lcPacks ,.F.)

*! B608953,2 MMT 08/11/2009 Fix bug of Printing same packing list if scanned 2 PL in same session[Start]
   SELECT REP_OBJECT
   RESTORE FROM MEMO mRepFxFlt ADDITIVE
   lctmppktk = loFormSet.lctmppktk
*! B608953,2 MMT 08/11/2009 Fix bug of Printing same packing list if scanned 2 PL in same session[End]

   llMoreThan140 = .F.
   SELECT (LCTMPPKTK)
   LOCATE 
   STORE '' TO lcPik
   IF RECCOUNT(lcTmpPkTk) > 2
     IF RECCOUNT(lcTmpPkTk) <= 27 
       SELECT &lcTmpPkTk
       LOCATE 
       lnCnt = RECCOUNT()
       lcPik = &lcTmpPkTk..PIKTKT +"' OR INLIST(PACK_NO"
       SCAN FOR RECNO() > 1 AND  RECNO() < lnCnt 
         lcPik = lcPik + ",'"+&lcTmpPkTk..PIKTKT+"'"
       ENDSCAN
       GO BOTTOM 
       lcPik = lcPik + ") OR PAcK_NO ='"+&lcTmpPkTk..PIKTKT+""
     ELSE && > 27 we need to convert it more than one inlist
       SELECT &lcTmpPkTk
       LOCATE 
       lcInList = ''
       lnCntLst = 1
       lnCnt = RECCOUNT()
       
       IF lnCnt > 140
         llMoreThan140 = .T.
       ENDIF
       
       lcPik = &lcTmpPkTk..PIKTKT +"'" 
       *: B608854,4 MMT 10/22/2009 Error While Printing larg number of PLs   [Start]
*!*	       SCAN FOR RECNO() > 1 AND  RECNO() < lnCnt 
*!*	         IF lnCntLst = 1
*!*	           lcInList= lcInList + IIF(EMPTY(lcInList),'',' OR ') + "inliST(PACK_NO"
*!*	         ENDIF   
*!*	         lcInList = lcInList +",'" +&lcTmpPkTk..PIKTKT+"'"  
*!*	         lnCntLst = lnCntLst + 1
*!*	         IF lnCntLst >=25
*!*	           lnCntLst = 1 
*!*	           lcInList = lcInList +")" 
*!*	         ENDIF 
*!*	       ENDSCAN
*!*	       IF lnCntLst < 25
*!*	         lcInList = lcInList + ")"
*!*	       ENDIF 
  	    SCAN FOR RECNO() > 1 AND  RECNO() < lnCnt 
      	  IF !EMPTY(lcInList)
      	    lcEvalExp = STRTRAN(lcInList,'PACK_NO','PIKTKT') 
      	    IF EVALUATE(lcEvalExp) 	
        	      LOOP 
        	    ENDIF   
      	  ENDIF 
          lcInList = lcInList +  IIF(!EMPTY(lcInList)," OR ","")+"BETWEEN(PACK_NO,'"+&lcTmpPkTk..PIKTKT+"','"
  	      lcPack = 	&lcTmpPkTk..PIKTKT 
      	  lnPack = VAL(&lcTmpPkTk..PIKTKT)
      	  lnNextPack = lnPack + 1
      	  lcNextPack =PADL(ALLTRIM(STR(lnNextPack)),6,'0')
      	  lnCurrRec = RECNO()
      	  LOCATE FOR PIKTKT = lcNextPack 
      	  IF FOUND()
        	    FOR A = lnNextPack+1 TO 999999
                lcNeWNextPack =PADL(ALLTRIM(STR(A)),6,'0')
                LOCATE FOR PIKTKT = lcNeWNextPack
                IF FOUND()
                  lcNextPack = lcNeWNextPack
                ELSE
                  lcNextPack =  PADL(ALLTRIM(STR(A-1)),6,'0')
                  EXIT   
                ENDIF 
      	    ENDFOR 
      	  ELSE
      		lcNextPack =  lcPack 
      	  ENDIF   
      	  IF BETWEEN(lnCurrRec ,1,RECCOUNT()) 
      	    GO RECORD lnCurrRec 
      	  ENDIF 
      	  lcInList = lcInList +  lcNextPack +"')"
    	 ENDSCAN 	  	
       *: B608854,4 MMT 10/22/2009 Error While Printing larg number of PLs   [End]
       lcPik = lcPik + " OR " + lcInList 
       SELECT &lcTmpPkTk
       GO BOTTOM 
	   lcPik = lcPik + " OR PAcK_NO ='"+&lcTmpPkTk..PIKTKT+""              
     ENDIF   
  ELSE
    IF RECCOUNT(lcTmpPkTk) = 2 && 2
      SELECT &lcTmpPkTk
      LOCATE 
      lcPik = &lcTmpPkTk..PIKTKT +"' OR PACK_NO = '"
      SELECT &lcTmpPkTk
      GO BOTTOM 
      lcPik = lcPik + &lcTmpPkTk..PIKTKT 
    ELSE && 1
      GO TOP 
      lcPik = &lcTmpPkTk..PIKTKT 
    ENDIF 
  ENDIF    
   

   SELECT REP_OBJECT
   *! B608953,2 MMT 08/11/2009 Fix bug of Printing same packing list if scanned 2 PL in same session[Start]   
   *RESTORE FROM MEMO mRepFxFlt ADDITIVE
   *! B608953,2 MMT 08/11/2009 Fix bug of Printing same packing list if scanned 2 PL in same session[End]
   lnSeaPos = ASCAN(laOgFXFlt,"PACK_HDR.PACK_NO")
   IF lnSeaPos > 0 
     lnSeaPos = ASUBSCRIPT(laOgFXFlt,lnSeaPos,1)
     laOgFXFlt[lnSeaPos,6] = "lcPik"
   ENDIF   
   SAVE TO MEMO mRepFxFlt 
   IF llMoreThan140 
     =SYS(3055,2040)
   ENDIF 
   
   loFormSet.Print("P")
   
   IF llMoreThan140 
     =SYS(3055,320)
   ENDIF 
   *: B609224,1 MMT 04/28/2010 Close Cusror opened after printing PL form[Start]
   IF USED(laogFxflt(1,6))
     USE IN (laogFxflt(1,6))
   ENDIF 
   *: B609224,1 MMT 04/28/2010 Close Cusror opened after printing PL form[End]
	
*!*	   LCRPPRG = 'ALPKLS'
*!*	   STORE '' TO LCFORMNAME, LCOPTPROG
*!*	   DIMENSION LASETTINGS[ 1, 2], LASPECFRM[ 1, 2], LAALLSETS[ 1, 3]
*!*	   = GFCHKFORM(@LCRPPRG,@LCFORMNAME,@LASETTINGS,@LASPECFRM,@LAALLSETS)
*!*	   LAALLSETS = ''
*!*	   LCSETPROCD = SET('PROCEDURE')
*!*	   SET PROCEDURE TO (GCREPHOME+'ALPKLS')
*!*	   = GFOPENTabLE(GCSYSHOME+'SYDREPRT','CREP_ID','SH')
*!*	   SELECT SYDREPRT
*!*	   = SEEK('ALPKLS  ')
*!*	   SCATTER MEMO MEMVAR
*!*	   LCSYDRPRT = GFTEMPNAME()
*!*	   COPY TO (GCWORKDIR+LCSYDRPRT) CDX NEXT 1
*!*	   USE IN SYDREPRT
*!*	   SELECT 0
*!*	   USE (GCWORKDIR+LCSYDRPRT) ALIAS SYDREPRT ORDER CREP_ID
*!*	   REPLACE CREADWHEN WITH 'lfDirWhen()'
*!*	   DO (GCREPHOME+'ALREPORT.APP') WITH 'ALPKLS'
*!*	   USE IN SYDREPRT
*!*	   ERASE (GCWORKDIR+LCSYDRPRT+'.DBF')
*!*	   ERASE (GCWORKDIR+LCSYDRPRT+'.CDX')
*!*	   SET PROCEDURE TO &lcSetProcd
ENDIF
SELECT (LNSLCT)
loFormset.PreferenceName = ""
*: B609224,2 MMT 07/27/2010 Change Calling the form Init [Start]
*loFormset.init()
lfInitForm(loFormset)
IF TYPE("loFormset.ariaForm1.ariaPAGEFRAME1.page4.cntInvoicesummary.ChargesFile") = 'U'
  loFormset.ariaForm1.ariaPAGEFRAME1.page4.cntInvoicesummary.AddProperty ('ChargesFile',"")
ENDIF 
*: B609224,2 MMT 07/27/2010 Change Calling the form Init [End]


*:**************************************************************************
*:* Name        : LFWFILLPOP
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  Get codes  Function
*:***************************************************************************
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [Start]
*!*	FUNCTION  LFWFILLPOP
*!*	PARAMETER LAINFARRAY, LCFIELD, LCFILLWITH


*!*	=gfOpenTable('Codes','Codes','SH')
*!*	PRIVATE LNALIAS, LCTAG, LCTOUPDAT, LCCURPOPUP, LCSTATUS, LLADDALL
*!*	PRIVATE LLADDNOA, LCALTFILE, LCALTINDX, LCALTEXP, LCALTFIELD, LLEDITABLE
*!*	PRIVATE LCALL, LCALLCODE, LCSEPRAT, LCCURCODE, LCFIELDS, LNNEWVAL
*!*	LNPOS = ASCAN(LAINFARRAY,LCFIELD)
*!*	LCFIELD = UPPER(PADR(LCFIELD,10))
*!*	LCTOUPDAT = LAINFARRAY(LNPOS+1)
*!*	LCCURPOPUP = LAINFARRAY(LNPOS+2)
*!*	LCSTATUS = LAINFARRAY(LNPOS+3)
*!*	LLADDALL = LAINFARRAY(LNPOS+4)
*!*	LLADDNOA = LAINFARRAY(LNPOS+5)
*!*	LCALTFILE = LAINFARRAY(LNPOS+6)
*!*	LCALTINDX = LAINFARRAY(LNPOS+7)
*!*	LCALTEXP = LAINFARRAY(LNPOS+8)
*!*	LCALTFIELD = LAINFARRAY(LNPOS+9)
*!*	LNFW = 0
*!*	LLEDITABLE = GFISEDTBLE(LCFIELD,@LNFW,oAriaApplication.ActiveCompanyID)
*!*	LCALL = PADR('Same',IIF(LLEDITABLE,39,30))
*!*	LCALLCODE = CHR(42)
*!*	LCSEPRAT = SPACE(1)+'-'+SPACE(1)
*!*	LNALIAS = SELECT()
*!*	DO CASE
*!*	   CASE LCFILLWITH='L' .AND. LCSTATUS<>LCFILLWITH
*!*	      lcCurCode = &lcToUpdat[&lcCurPopUp,2]
*!*	      SELECT CODES
*!*	      LCTAG = ORDER()
*!*	      SET ORDER TO
*!*	      LCFIELDS = IIF(LLEDITABLE,'CCODE_NO+lcSeprat+CDISCREP AS Code','CDISCREP')
*!*	      IF LLEDITABLE
*!*	         SELECT &lcFields,CCODE_NO FROM Codes WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = "NN"+lcField ORDER BY CCODE_NO  INTO ARRAY &lcToUpdat
*!*	      ELSE
*!*	         SELECT &lcFields,CCODE_NO FROM Codes WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = "NN"+lcField ORDER BY CDISCREP  INTO ARRAY &lcToUpdat
*!*	      ENDIF
*!*	      SELECT CODES
*!*	      gfSetOrder(lcTag)
*!*	      LAINFARRAY[ LNPOS+3] = 'L'
*!*	      IF LLADDALL
*!*	         DIMENSION &lcToUpdat[ALEN(&lcToUpdat,1)+1,2]
*!*	         = AINS(&lcToUpdat,1)
*!*	         &lcToUpdat[1] = lcAll
*!*	         &lcToUpdat[2] = lcAllCode
*!*	      ENDIF
*!*	      lnNewVal    = ASCAN(&lcToUpdat, lcCurCode)
*!*	      &lcCurPopUp = IIF(lnNewVal <> 0, IIF(lnNewVal=1,1,ROUND(lnNewVal/2,0)), 1)
*!*	   CASE LCFILLWITH='A'
*!*	      DIMENSION &lcToUpdat[1,2]
*!*	      &lcToUpdat[1,1] = lcAll
*!*	      &lcToUpdat[1,2] = lcAllCode
*!*	      &lcCurPopUp     = 1
*!*	      LAINFARRAY[ LNPOS+3] = LCFILLWITH
*!*	ENDCASE
*!*	SHOW GET &lcCurPopUp
*!*	SELECT (LNALIAS)
*: B608854,7 MMT 01/26/2010 Try to solve error of not enough memory at DCC [End]
PROCEDURE LFPACKLIN
PARAMETER LCORDER, LCPACKNO
WAIT WINDOW NOWAIT 'Computing packed quantity...'
SELECT PACK_LIN
= gfSEEK(LCPACKNO)
SCAN REST WHILE PACK_NO+STR(LINE_NO,3)+STYLE+CPACKCOLOR=LCPACKNO
   IF  .NOT. SEEK(LCPACKNO+LCORDER+STR(NORDLINENO,6),LCPACKLINE)
      INSERT INTO (LCPACKLINE) ( ORDER, PACK_NO, NORDLINENO ) VALUE ( LCORDER, LCPACKNO, PACK_LIN.NORDLINENO )
   ENDIF
   SELECT (LCPACKLINE)
   REPLACE QTY1 WITH QTY1+PACK_LIN.QTY1, QTY2 WITH QTY2+PACK_LIN.QTY2, QTY3 WITH QTY3+PACK_LIN.QTY3, QTY4 WITH QTY4+PACK_LIN.QTY4, QTY5 WITH QTY5+PACK_LIN.QTY5, QTY6 WITH QTY6+PACK_LIN.QTY6, QTY7 WITH QTY7+PACK_LIN.QTY7, QTY8 WITH QTY8+PACK_LIN.QTY8, TOTQTY WITH TOTQTY+PACK_LIN.TOTQTY
ENDSCAN
WAIT CLEAR

*:**************************************************************************
*:* Name        : LfVCNFRM
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  Save confirmation  Function
*:***************************************************************************
FUNCTION  LfVCNFRM
PARAMETERS loFormSet
IF LFBEFORSAV(loFormSet)
   IF GFMODALGEN('INM00000B00006',.F.,.F.,.F.,'Are you sure you want to Save')=1
     
     *B608854,1 MMT 04/23/2009 Validate the Consignment No in custom scan and pack for DCC[Start]
	 IF EMPTY(loFormSet.ariaform1.txtCongNo.value)
       = GFMODALGEN('INM00000B00000',.F.,.F.,.F.,'No consignment no has been entered - please update the no and then save the batch')
       loFormSet.ariaform1.txtCongNo.SetFocus()
       RETURN .F.
     ENDIF 
     *B608854,1 MMT 04/23/2009 Validate the Consignment No in custom scan and pack for DCC[End]
	 *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
*!*  	 IF EMPTY(loFormSet.ariaform1.cboPick.Value) &&OR EMPTY(loFormSet.ariaform1.cboPack.Value)
*!*  	   = GFMODALGEN('INM00000B00000',.F.,.F.,.F.,'Cannot save with empty Picked by or Empty Checked by')
*!*  	   RETURN .F.
*!*  	 ENDIF	 
	 *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
     RETURN .T.
   ELSE
 	 RETURN  .F.
   ENDIF 	 
else
  RETURN  .F.
ENDIF


*:**************************************************************************
*:* Name        : lfCrtFl
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  crt tmp file  Function
*:***************************************************************************
FUNCTION lfCrtFl

lcPacks =gfTemPname()
   DIMENSION laArrStr[2,4]
   laArrStr[1,1] = 'KEYEXP'
   laArrStr[1,2] = 'C'
   laArrStr[1,3] = 6
   laArrStr[1,4] = 0
   
   laArrStr[2,1] = 'Pack_no'
   laArrStr[2,2] = 'C'
   laArrStr[2,3] = 6
   laArrStr[2,4] = 0
   gfCrtTmp(lcPacks ,@laArrStr,"KEYEXP",lcPacks ,.F.)
lcValuesToConvert = lcPiktkts 
IF !EMPTY(lcPiktkts)
  lnStart=1 
  lnEnd=AT(',',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcPacks ) 
    APPEND BLANK 
     REPLACE PACK_NO WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1),;
     		 KeyExp WITH  SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT(',',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcPacks ) 
    APPEND BLANK 
     REPLACE PACK_NO WITH lcValuesToConvert ,;
     		 KeyExp WITH  lcValuesToConvert 
  ENDIF 
ENDIF 
   SCAN
     SELECT (lcPacks )
     APPEND BLANK
     REPLACE PACK_NO WITH &lcTmpPkTk..PIKTKT,;
     		 KeyExp WITH  &lcTmpPkTk..PIKTKT
   ENDSCAN   
   
lnSeaPos = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.PACK_NO")
IF lnSeaPos > 0 
  lnSeaPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnSeaPos,1)
  loOgScroll.laOgFXFlt[lnSeaPos,6] = lcPacks 
ENDIF   
   
*:**************************************************************************
*:* Name        : LFVCRT
*! Developer : Mariam Mazhar
*! Date      : 01/15/2009
*:* Purpose     :  update carton no.
*:***************************************************************************
 
FUNCTION LFVCRT
PARAMETERS loFormSet
lcTmpPkTk = loFormSet.LCTMPPKTK
lnCrt = loFormSet.ariaform1.txtCartons.VAlue  
SELECT &lcTmpPkTk
lnRecN = RECNO()
LOCATE 
IF  .NOT. EOF()
  SCAN 
    IF NCARTON > MIN(lnCrt ,PIKQTY)
       *WAIT WINDOW NOWAIT 'Cartons no. can not be greater than the total no of cartons or the total no. of pices in the Pick ticket'
       REPLACE NCARTON WITH MIN(lnCrt ,PIKQTY)
    ENDIF
  ENDSCAN   
ENDIF
IF BETWEEN(lnRecN ,1,RECCOUNT())   
  GO RECORD lnRecN 
ENDIF 