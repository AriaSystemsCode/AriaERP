*:************************************************************************
*: Program file  : RMRTATH.PRG
*: Program desc. : RETURN AUTHORIZATION.
*: For screen    : RMRTATH.SCX
*: System        : ARIA4
*: Module        : Returns Merchandise Module
*: Developer     : Ahmad Shoukry Mohammed (ASM)
*:*************************************************************
*: Parameters    : lcRetAth --> R/A #
*:*************************************************************
*! Modifications :
*! B999999,1 ASM 06/13/2005 Read Lines For Consolidated Invoice even when No Store was Specified
*! B608234,1 WAM 08/23/2007 Browse shifts lines up when add new line
*! B608275,1 MMT 09/23/2007 put default location the first location in locations combobox[T20070830.0011]
*! B608520,1 WAM 04/17/2008 Remove decimal points from RA quantity [T20080326.0014]
*! B608545,1 WAM 05/08/2008 Don't generate new line# for modified lines [T20080408.0002]
*! B608974,1 MMT 08/18/2009 Fix bug of can add style if Style Segment entry setups is set to yes [T20090805.0008]
*! B609120,1 MMT 12/30/2009 Fix bug of padding RA# with 0'S [T20091217.0017]
*! B609217,1 WAM 04/22/2010 Restrict sales orders to selected account/store [T20100420.0005]
*! E302734,1 MMT 08/23/2010 Modify Screen to be called from the Customer screen Task Pane[T20100307.0001]
*! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[T20100804.0005]
*! B609464,1 MMT 11/22/2010 Error while saving RA has no lines after editing it[T20101028.0017]
*! C201291,1 MMT 11/25/2010 Customization for OSP00 to add PO# per RA line[T20101020.0023]
*! B609577,1 WAM 05/03/2011 Enhance the performace of invoice browse [T20110407.0043]
*! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[T20110328.0001]
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*! B610460,1 HIA 07/08/2013 T20130731.0014 - Olsen Return Authorization Issue : not updating and not saving.
*! B610626,1 TMI 12/15/2013 fix a problem that loading the program is very slow [T20131209.0002] 
*! E303662,1 MMT 04/21/2016 Add trigger to read style price from CSTPRICE table[T20160324.0006]
*! E303662,2 MMT 05/26/2016 Add trigger to read style price from CSTPRICE table while adding lines[T20160324.0006]
*************************************************************************
*! E302734,1 MMT 08/23/2010 Modify Screen to be called from the Customer screen Task Pane[Start]
*!*	PARAMETERS lcRetAth
*!*	  DO FORM (oAriaApplication.ScreenHome+"\RM\RMRTATH.SCX") WITH lcRetAth
PARAMETERS LCRETATH,LCACCOUNT,LCSTORE
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[START]
*DO FORM (oAriaApplication.ScreenHome+"\RM\RMRTATH.SCX") WITH lcRetAth,lcAccount,lcStore
=GFCALLFORM('RMRTATH','RM',"lcRetAth,lcAccount,lcStore")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*! E302734,1 MMT 08/23/2010 Modify Screen to be called from the Customer screen Task Pane[End]
RETURN


DEFINE CLASS PRG AS CUSTOM

  *lcRetAth = m.lcRetAth
  LOFORMSET=.F.
  LOFORM=.F.

  LLEDIT = .T.
  LLDELETE = .T.
  LLNOTES = .T.
  LLOBJECTLINK = .F.

  LLCRMINS = IIF('CR' $ OARIAAPPLICATION.COMPANYINSTALLEDMODULES,.T.,.F.)

  *-- Array hold the status text
  DECLARE LASTATUS[4]
  LASTATUS[1] = "Open"
  LASTATUS[2] = "Completed"
  LASTATUS[3] = "Canceled"
  LASTATUS[4] = "Electronic"

  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
  *-- Array hold Consolidated Invoice Orders
  DECLARE LACONSINVORDRS[1]
  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]

  *-- Var. hold the browse titles.
  LCBROWTTL  = "R/A Lines"

  *-- Var. hold the temp. R/A lines file.
  LCTMPRETLN = ""
  LCRCURALIS = ""
  LCKEYRET   = ""

  LNPSTRATE  = 0         && To hold the PST rate.
  LNCUSPSTRT = 0         && To hold the default PST rate.

  LCNAME = ''
  LCFNAME = ''
  LCSTATUS = ''
  LCFADDRES1 = ''
  LCFADDRES2 = ''
  LCFADDRES3 = ''
  LCFADDRES4 = ''
  LCFADDRES5 = ''
  LCTADDRES1 = ''
  LCTADDRES2 = ''
  LCTADDRES3 = ''
  LCTADDRES4 = ''
  LCTADDRES5 = ''
  LCSTYLE = ''
  LCDESC = ''
  LCTAXNAME = ''
  LCREASON = ''
  LCCUSTPLVL = ''
  LCDYELOT = ''
  LCCONSOL = ''
  LCDEFWARE = ''
  LCRA_LINNO = ''

  LCWARSTAT = "DISABLE"
  LCINQSTAT = "DISABLE"
  LCNEWSTAT = "DISABLE"
  LCLINSTAT = "DISABLE"
  LCDIVSTAT = "DISABLE"
  LCCURSTAT = "DISABLE"
  LCEXRSTAT = "DISABLE"
  LCENTRSTAT = "DISABLE"
  LCRELSTAT = "DISABLE"
  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
  LCORDSTAT = "DISABLE"
  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]
  LCDYESTAT = "DISABLE"
  LCOBJSTAT = "DISABLE"
  LCINVSTAT = "DISABLE"
  *Define variable to control the enabling & disabling of the account field.
  LCACCSTAT = "ENABLE"

  LLTAXYES = .F.
  LLMULTIWH = .F.
  LLDYELOT = .F.
  LLGLLINK = .F.
  LLBASED = .F.
  LLISCANADA = .F.
  LLMULCURR = .F.
  LLEDITEXRT = .F.

  *-- Array hold all the available warehouses.
  DECLARE LAWAREHOUS[1,2]
  PUWAREHOUS = 1

  DECLARE LASIZE[8] , LAQTYSTK[8]
  LASIZE     = ""
  LAQTYSTK   = 0
  LCSTYHDR   = ""
  LNSTYLNGTH = 0

  LNTOTQTY = 0
  LNPRICE = 0
  LNTOTAL = 0
  LNMARKER = 0
  LNLINE = 0
  LNNOOFLINES = 0
  LNSCALE = 0
  LCSCALE = ""
  LNDEFTAXRT = 0
  LNTAXRATE = 0
  LNDISCPCNT = 0
  LNRA_LINNO = 0
  LNTAXZAMT = 0

  LLBROWSE = .F.
  CBCOMPLETE = .F.
  LLUPDATE = .F.
  *Define a flag to know if entered the detail folder or not yet.
  LLENTERDET = .F.

  LCCURRCODE = OARIAAPPLICATION.BASECURRENCY &&  Variable to hold currency code.
  LNEXRATE   = 1          &&  Variable to hold exchange rate.
  LNCURRUNIT = 1          &&  Variable to hold currency unit.
  LCUNTSIN   = '/'        &&  Variable to hold unit sign.
  LCEXRSIN   = '/'        &&  Variable to hold exchange rate sign.

  LLREBROWSE = .T.    && Flag to know if execute the browse or not.
  LLADDMODE  = .F.    && Flag to know if entered the add mode or not.
  LLCUPDATE  = .F.    && Flag to determine if there is something changed in the screen.

  LNRESON1 = 1
  LNRESON2 = 1
  LNDIV = 1

  LLALOWNEW = .T.     && Flag to allow adding new credit memos.
  LLNOSHOW  = .F.     && Flag to force the execution of the show procedure.

  *N038891,1 KHM 01/26/2006 [Start]
  CEXTENDEDSIZESCALETEMPFILE = ''
  *N038891,1 KHM 01/26/2006 [End]

  *!*************************************************************
  *! Name      : lpInit
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To be called from the Init Event of the FormSet
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : True / Flase
  *!*************************************************************
  FUNCTION LFINIT
  LPARAMETERS LOFRM
  SET MULTILOCKS ON
  SET DELETED ON

  THIS.LOFORM = LOFRM
  THIS.LOFORMSET = LOFRM.PARENT
  IF THIS.LLCRMINS
    THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'CRMesag',OARIAAPPLICATION.DATADIR+'TransType')
  ENDIF

  *-- Prepare the R/A line temp. file.
  THIS.LCTMPRETLN = GFTEMPNAME()

  *N038891,1 KHM 01/26/2006 [Start]
  THIS.CEXTENDEDSIZESCALETEMPFILE = GFTEMPNAME()
  *N038891,1 KHM 01/26/2006 [End]

  THIS.LCRCURALIS = IIF(THIS.LOFORMSET.ACTIVEMODE="V" ,'RALINE',THIS.LCTMPRETLN)
  THIS.LCKEYRET   = IIF(THIS.LOFORMSET.ACTIVEMODE="V" ,"KEY RetAuth.rano" ,"")
  IF !(THIS.LFOPENFILE('RETAUTH','RETAUTH') AND THIS.LFOPENFILE('RALINE','RALINE') AND ;
      THIS.LFOPENFILE('CUSTOMER','CUSTOMER') AND THIS.LFOPENFILE('CODES','CCODE_NO') AND ;
      THIS.LFOPENFILE('SCALE','SCALE') AND THIS.LFOPENFILE('STYLE','STYLE') AND ;
      THIS.LFOPENFILE('WAREHOUS','WAREHOUS'))
    RETURN .F.
  ENDIF
  WITH THIS.LOFORMSET
    .DATAENVIRONMENT.INITIALSELECTEDALIAS = 'RETAUTH'
    .CBROWSETABLEDBENGINE   = 'SQL'
    .CBROWSEFILENAME        = "RETAUTH"
    .CBROWSEINDEXEXPRESSION = "RANO"
    .CBROWSEINDEXFIELDS     = "RANO"
    .CBROWSEINDEXNAME       = "RETAUTH"
    .CBROWSEALIASNAME       = "RETAUTH"
    .CBROWSETABLENAME       = "RETAUTH"

    *WSH [Start]
    .BROWSETITLE            = "Return Authorization"
    *WSH [End]

  ENDWITH

  THIS.LFOPENFILE(OARIAAPPLICATION.SYSPATH+'SycComp',OARIAAPPLICATION.SYSPATH+'cComp_Id')

  THIS.LCTAXNAME  = GFGETMEMVAR('M_TAX_DESC' , OARIAAPPLICATION.ACTIVECOMPANYID)
  THIS.LCTAXNAME  = IIF(EMPTY(THIS.LCTAXNAME) , 'Gst Tax' , THIS.LCTAXNAME)
  THIS.LOFORM.PAGEFRAME.PAGE1.LBLTAXNAME.CAPTION = THIS.LCTAXNAME
  *-- Var. hold the style header.
  THIS.LCSTYHDR   = GFITEMMASK("HI")
  THIS.LNSTYLNGTH = LEN(THIS.LCSTYHDR)
  *-- If calculate tax.
  THIS.LLTAXYES   = ALLTRIM(GFGETMEMVAR('M_TAX',OARIAAPPLICATION.ACTIVECOMPANYID))       = 'Y'
  *-- Get the default tax rate from the setup.
  THIS.LNDEFTAXRT = GFGETMEMVAR('M_TAX_RATE',OARIAAPPLICATION.ACTIVECOMPANYID)
  *-- Multi warehouse.
  THIS.LLMULTIWH  = ALLTRIM(GFGETMEMVAR('M_WareHouse',OARIAAPPLICATION.ACTIVECOMPANYID)) = 'Y'
  *-- Use Dyelot or not.
  THIS.LLDYELOT   = ALLTRIM(GFGETMEMVAR('M_DYELOT',OARIAAPPLICATION.ACTIVECOMPANYID))    = 'Y'
  *ASM, Hide or Display the DyeLot Control, and change to Configuration if needed [Start]
  IF !THIS.LLDYELOT
    STORE .F. TO THIS.LOFORM.PAGEFRAME.PAGE2.LBLDYELOT.VISIBLE, ;
      THIS.LOFORM.PAGEFRAME.PAGE2.KBDYELOT.VISIBLE
  ELSE
    IF ALLTRIM(GFGETMEMVAR('M_STYCNFG',OARIAAPPLICATION.ACTIVECOMPANYID)) = 'Y'
      THIS.LOFORM.PAGEFRAME.PAGE2.LBLDYELOT.CAPTION = 'Configuration'
    ENDIF
  ENDIF
  *ASM, Hide or Display the DyeLot Control, and change to Configuration if needed [End]
  *-- See if there is GL link or not.
  THIS.LLGLLINK   = ALLTRIM(GFGETMEMVAR('M_LINK_GL',OARIAAPPLICATION.ACTIVECOMPANYID))   = 'Y'
  *-- Sequence on Division or not.
  THIS.LLBASED    = ALLTRIM(GFGETMEMVAR('M_Div_Seq',OARIAAPPLICATION.ACTIVECOMPANYID))   = 'Y'
  *-- If country is Canada
  THIS.LLISCANADA = IIF(UPPER(ALLTRIM(OARIAAPPLICATION.DEFAULTCOUNTRY)) = 'CANADA', .T., .F.)
  *-- Multi Currency.
  THIS.LLMULCURR  = GFGETMEMVAR('llMulCurr',OARIAAPPLICATION.ACTIVECOMPANYID)
  *-- Edit Exchange rate.
  THIS.LLEDITEXRT = GFGETMEMVAR('LLEDITEXRA',OARIAAPPLICATION.ACTIVECOMPANYID)

  *-- If multi currency.
  IF THIS.LLMULCURR
    THIS.LCCURRCODE = SPACE(3)   &&  Variable to hold currency code.
    THIS.LNEXRATE   = 0          &&  Variable to hold exchange rate.
    THIS.LNCURRUNIT = 0          &&  Variable to hold currency unit.
    THIS.LCUNTSIN   = '/'        &&  Variable to hold unit sign.
    THIS.LCEXRSIN   = '/'        &&  Variable to hold exchange rate sign.
    IF !USED("SycCurr")
      *-- Open the currency file if was not open.
      THIS.LFOPENFILE(OARIAAPPLICATION.SYSPATH+'SycCurr',OARIAAPPLICATION.SYSPATH+'cCurrCode')
    ENDIF
    IF !USED("SycExch")
      *-- Open the exchange file if was not open.
      THIS.LFOPENFILE(OARIAAPPLICATION.SYSPATH+'SycExch','')
    ENDIF
  ELSE
    THIS.LCCURRCODE = OARIAAPPLICATION.BASECURRENCY &&  Variable to hold currency code.
    THIS.LNEXRATE   = 1          &&  Variable to hold exchange rate.
    THIS.LNCURRUNIT = 1          &&  Variable to hold currency unit.
    THIS.LCUNTSIN   = '/'        &&  Variable to hold unit sign.
    THIS.LCEXRSIN   = '/'        &&  Variable to hold exchange rate sign.
  ENDIF
  *-- Fill the warehouse array.
  IF !THIS.LLMULTIWH
    SELECT WAREHOUS
    GO TOP
    THIS.LCDEFWARE       = WAREHOUS.CWARECODE
    THIS.LAWAREHOUS[1,1] = WAREHOUS.CDESC
    THIS.LAWAREHOUS[1,2] = WAREHOUS.CWARECODE
  ELSE
    THIS.LCDEFWARE       = ""
    *!* B608275,1 MMT 09/23/2007 put default location the first location in locations combobox[Start]
    *SELECT cDesc , cWareCode ;
    FROM WAREHOUS ;
    INTO ARRAY This.laWareHous
    SELECT CDESC , CWARECODE ;
      FROM WAREHOUS ;
      INTO ARRAY THIS.LAWAREHOUS ORDER BY WAREHOUS.LDEFWARE DESC
    *!* B608275,1 MMT 09/23/2007 put default location the first location in locations combobox[End]
  ENDIF

  SELECT RALINE
  DIMENSION LAFILESTRU[1]
  =AFIELDS(LAFILESTRU)
  LNFILESTRU = ALEN(LAFILESTRU,1)
  DIMENSION LAFILESTRU[lnFileStru+3,18]
  LAFILESTRU[lnFileStru+1,1] = 'CSTATUS'
  LAFILESTRU[lnFileStru+1,2] = 'C'
  LAFILESTRU[lnFileStru+1,3] = 1
  LAFILESTRU[lnFileStru+1,4] = 0
  LAFILESTRU[lnFileStru+2,1] = 'NRECNO'
  LAFILESTRU[lnFileStru+2,2] = 'N'
  LAFILESTRU[lnFileStru+2,3] = 5
  LAFILESTRU[lnFileStru+2,4] = 0
  *Add logical field to know if this style was shipped to this customer before or not.
  LAFILESTRU[lnFileStru+3,1] = 'LSHIPPED'
  LAFILESTRU[lnFileStru+3,2] = 'L'
  LAFILESTRU[lnFileStru+3,3] = 1
  LAFILESTRU[lnFileStru+3,4] = 0
  FOR LNLOOP = 1  TO 3
    STORE '' TO LAFILESTRU[lnFileStru+lnLoop ,7],LAFILESTRU[lnFileStru+lnLoop  ,8],;
      LAFILESTRU[lnFileStru+lnLoop ,9],LAFILESTRU[lnFileStru+lnLoop  ,10],;
      LAFILESTRU[lnFileStru+lnLoop ,11],LAFILESTRU[lnFileStru+lnLoop ,12],;
      LAFILESTRU[lnFileStru+lnLoop ,13],LAFILESTRU[lnFileStru+lnLoop ,14],;
      LAFILESTRU[lnFileStru+lnLoop ,15],LAFILESTRU[lnFileStru+lnLoop ,16]

    STORE 0 TO  LAFILESTRU[lnFileStru+lnLoop ,17],LAFILESTRU[lnFileStru+lnLoop ,18]
  NEXT

  CREATE TABLE (OARIAAPPLICATION.WORKDIR + THIS.LCTMPRETLN) FROM ARRAY LAFILESTRU
  SELECT RETAUTH
  *-- If the program was called from another program, force to view mode.
  IF TYPE('This.lcRetAth') = 'C' .AND. THIS.RETAUTH.SEEK(THIS.LCRETATH)
    THIS.LOFORMSET.ACTIVEMODE="V"
  ELSE
    THIS.LOFORMSET.ACTIVEMODE="S"
  ENDIF


  *Define variable to control the enabling & disabling of the account field.
  *ASM, Disable Account Code if Invoice or Order # was entered [Start]
  *This.lcAccStat = IIF(This.loFormSet.ActiveMode="V" , "DISABLE" , "ENABLE")
  THIS.LCACCSTAT = IIF(THIS.LOFORMSET.ACTIVEMODE="V" OR ;
    !EMPTY(THIS.LOFORM.PAGEFRAME.PAGE1.KBINVOICE.KEYTEXTBOX.VALUE) OR ;
    !EMPTY(THIS.LOFORM.PAGEFRAME.PAGE1.KBORDER.KEYTEXTBOX.VALUE), "DISABLE" , "ENABLE")
  *ASM, Disable Account Code if Invoice or Order # was entered [End]

  *-- Default the status varibales of the screen objects to its right values.
  THIS.LCOBJSTAT = IIF(THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A" , "ENABLE" , "DISABLE")
  THIS.LCWARSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. THIS.LLMULTIWH , "ENABLE" , "DISABLE")
  THIS.LCINQSTAT = IIF(!EMPTY(RETAUTH.INVOICE) .AND. !THIS.LOFORMSET.ACTIVEMODE="S" , "ENABLE" , "DISABLE")
  THIS.LCINVSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") , "ENABLE" , "DISABLE")
  THIS.LCDIVSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(RETAUTH.INVOICE) .AND. THIS.LNNOOFLINES = 0 , "ENABLE" , "DISABLE")

  THIS.LCCURSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(RETAUTH.INVOICE) , "ENABLE" , "DISABLE")
  THIS.LCEXRSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(RETAUTH.INVOICE) .AND. !EMPTY(RETAUTH.CCURRCODE) .AND. THIS.LLEDITEXRT .AND. (RETAUTH.CCURRCODE <> OARIAAPPLICATION.BASECURRENCY) , "ENABLE" , "DISABLE")

  *Don't include lnNoOfLines when determining the invoice object status
  *                  as it should be always enabled in Add/Edit modes
  THIS.LCENTRSTAT= IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EOF(THIS.LCTMPRETLN) .AND.;
    !EMPTY(RETAUTH.INVOICE) , "ENABLE" , "DISABLE")
  THIS.LCRELSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(RETAUTH.INVOICE) , "ENABLE" ,"DISABLE")
  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
  THIS.LCORDSTAT = IIF((THIS.LCRELSTAT == "ENABLE" .OR. (TYPE('This.laConsInvOrdrs') <> 'U' .AND. ALEN(THIS.LACONSINVORDRS,1) >1)) , "ENABLE" ,"DISABLE")
  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]


  SELECT RETAUTH
  *-- set realtion between R/A header & customer file.
  *Old: SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER

  THIS.LFCONTROLSOURCE()

  ENDFUNC
  *--end of lfInit


  *!*************************************************************
  *! Name      : lfControlSource
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Assign Control Source Property for the Form's Controls
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : True / Flase
  *!*************************************************************
  FUNCTION LFCONTROLSOURCE
  LOCAL LNCOUNT, LCCOUNT

  WITH THIS.LOFORM
    .KBRANO.KEYTEXTBOX.CONTROLSOURCE = "RetAuth.rano"
    .KBACCOUNT.KEYTEXTBOX.CONTROLSOURCE = "RetAuth.account"
    .TXTACCOUNTNAME.CONTROLSOURCE = "ThisFormSet.Prg.lcName"
    .KBSTORE.KEYTEXTBOX.CONTROLSOURCE = "RetAuth.store"
    .TXTSTATUS.CONTROLSOURCE = "ThisFormSet.Prg.lcStatus"

    WITH .PAGEFRAME.PAGE1
      .DTRADATE.CONTROLSOURCE = "RetAuth.radate"
      .DTVOID.CONTROLSOURCE = "RetAuth.void"
      .TXTAUTH.CONTROLSOURCE = "RetAuth.auth"
      .TXTAUTHAMT.CONTROLSOURCE = "RetAuth.authamt"
      .TXTFNAME.CONTROLSOURCE = "ThisFormSet.Prg.lcFname"
      .TXTFADDRESS1.CONTROLSOURCE = "ThisFormSet.Prg.lcFaddres1"
      .TXTFADDRESS2.CONTROLSOURCE = "ThisFormSet.Prg.lcFaddres2"
      .TXTFADDRESS3.CONTROLSOURCE = "ThisFormSet.Prg.lcFaddres3"
      .TXTFADDRESS4.CONTROLSOURCE = "ThisFormSet.Prg.lcFaddres4"
      .TXTFADDRESS5.CONTROLSOURCE = "ThisFormSet.Prg.lcFaddres5"
      .CBOCWARECODE.ROWSOURCE = "ThisFormSet.Prg.laWareHous"
      .CBOCWARECODE.CONTROLSOURCE = "RetAuth.cWareCode"
      .TXTTADDRESS1.CONTROLSOURCE = "ThisFormSet.Prg.lcTaddres1"
      .TXTTADDRESS2.CONTROLSOURCE = "ThisFormSet.Prg.lcTaddres2"
      .TXTTADDRESS3.CONTROLSOURCE = "ThisFormSet.Prg.lcTaddres3"
      .TXTTADDRESS4.CONTROLSOURCE = "ThisFormSet.Prg.lcTaddres4"
      .TXTTADDRESS5.CONTROLSOURCE = "ThisFormSet.Prg.lcTaddres5"
      .KBINVOICE.KEYTEXTBOX.CONTROLSOURCE = "RetAuth.invoice"
      .CHKENTIREINVOICE.CONTROLSOURCE = "ThisFormSet.Prg.cbComplete"
      .TXTTAXRATE.CONTROLSOURCE = "ThisFormSet.Prg.lntaxrate"
      .KBORDER.KEYTEXTBOX.CONTROLSOURCE = "RetAuth.order"
      .TXTCARTONS.CONTROLSOURCE = "RetAuth.cartons"
      .TXTCUSTPO.CONTROLSOURCE = "RetAuth.custpo"
      .CBOREASON.CONTROLSOURCE = "RetAuth.reason"
      .CBODIVISION.CONTROLSOURCE = "RetAuth.cDivision"
      .TXTCRETNOTE1.CONTROLSOURCE = "RetAuth.cretnote1"
      .TXTCRETNOTE2.CONTROLSOURCE = "RetAuth.cretnote2"
      .KBCCURRCODE.KEYTEXTBOX.CONTROLSOURCE = "RetAuth.ccurrcode"
      .TXTNEXRATE.CONTROLSOURCE = "RetAuth.nexrate"
      .TXTNRETA_BUD.CONTROLSOURCE = "RetAuth.nReta_Bud"
      .TXTNRETA_REC.CONTROLSOURCE = "RetAuth.nReta_Rec"
      .TXTNRETA_CAN.CONTROLSOURCE = "RetAuth.nReta_Can"
      .TXTNRETA_OPN.CONTROLSOURCE = "RetAuth.nReta_Opn"
      .TXTNRTOPNAMT.CONTROLSOURCE = "RetAuth.nRtOpnAmt"
    ENDWITH

    WITH .PAGEFRAME.PAGE2
      .CNTSTYLE.CONTROLSOURCE = THIS.LCTMPRETLN+'.Style'
      .TXTSTYDESC.CONTROLSOURCE = "ThisFormSet.Prg.lcDesc"
      .KBDYELOT.KEYTEXTBOX.CONTROLSOURCE = "ThisFormSet.Prg.lcDyeLot"
      .CBOREASON.CONTROLSOURCE = THIS.LCTMPRETLN+'.Reason'
      .TXTPRICE.CONTROLSOURCE = "ThisFormSet.Prg.lnPrice"
      .TXTTOTQTY.CONTROLSOURCE = "ThisFormSet.Prg.lnTotQty"
      .TXTTOTAL.CONTROLSOURCE = "ThisFormSet.Prg.lnTotal"
      *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[Start]
      .CBOEMPL.CONTROLSOURCE = THIS.LCTMPRETLN+'.Employee'
      *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[End]
      FOR LNCOUNT =1 TO 8
        LCCOUNT=ALLTRIM(STR(LNCOUNT))
        .CNTSIZES.TXTQTY&LCCOUNT..CONTROLSOURCE = THIS.LCTMPRETLN+'.Qty'+LCCOUNT
      NEXT
    ENDWITH

  ENDWITH
  ENDFUNC
  *--end of lfControlSource



  *!*************************************************************
  *! Name      : lpShow
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To be called from the Refresh Event of the FormSet
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LPSHOW
  *-- Change the prompt of the delete bar in the edit pad to be canceled
  *-- instead of delete.
  *Old: lcCanBar = "IIF(RetAuth.Status='X' , '\<Uncancel' , '\<Cancel')" && Revise

  *Define a flag to know if entered the detail folder or not yet.
  THIS.LLENTERDET = .F.

  STORE .T. TO THIS.LLEDIT, THIS.LLDELETE, THIS.LLNOTES

  SELECT RETAUTH
  DO CASE
  CASE THIS.LOFORMSET.ACTIVEMODE="S"
    *-- Blank the R/A lines temp. file.
    SELECT (THIS.LCTMPRETLN)
    ZAP
    *-- Initialize the needed variables.
    THIS.LLADDMODE = .F.
    THIS.LLCUPDATE = .F.
    STORE ' ' TO THIS.LCNAME , THIS.LCFNAME , THIS.LCSTATUS , ;
      THIS.LCFADDRES1 , THIS.LCFADDRES2 , THIS.LCFADDRES3 ,;
      THIS.LCFADDRES4 , THIS.LCFADDRES5 , THIS.LCTADDRES1 ,;
      THIS.LCTADDRES2 , THIS.LCTADDRES3 , THIS.LCTADDRES4 ,;
      THIS.LCTADDRES5 , THIS.LCSTYLE , THIS.LCDESC ,;
      THIS.LCREASON , THIS.LCCUSTPLVL , THIS.LCDYELOT ,;
      THIS.LCCONSOL , THIS.LCRA_LINNO

    STORE 0 TO THIS.LNTOTQTY , THIS.LNPRICE , THIS.LNTOTAL ,;
      THIS.LNNOOFLINES , THIS.LNTAXRATE , THIS.LNPSTRATE , THIS.LNDISCPCNT , ;
      THIS.LNRA_LINNO , THIS.LNTAXZAMT

    THIS.PUWAREHOUS = 1 && Revise
    THIS.LASIZE     = ""
    THIS.LAQTYSTK   = 0
    THIS.CBCOMPLETE = .F.
    THIS.LNRESON2   = 1 && Revise


    *-- Disable the notes & the object linking buttons in the control pannel.
    *Old: SHOW GET pbNotes  DISABLE
    THIS.LLNOTES = .F.
    *Old: SHOW GET pbLinkTo DISABLE && Revise

    *-- Disable the delete button with the cancel icon.
    *Revise: SHOW GET pbDlt,1 DISABLE PROMPT lcCancel && Old
    THIS.LLDELETE = .F.

    *Point to the header folder after save or cancel.
    THIS.LOFORM.PAGEFRAME.PAGE1.ACTIVATE
    *Disable the detail folder in the select mode
    THIS.LOFORM.PAGEFRAME.PAGE2.ENABLED = .F.

  CASE THIS.LOFORMSET.ACTIVEMODE="V"
    *-- Define the browse titles.
    *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[Start]
    IF !EMPTY(THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE) AND THIS.CUSTOMER.SEEK("M" + PADR(ALLTRIM(THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE),5))
      IF CUSTOMER.LCHKENTLM
        THIS.LOFORM.PARENT.LLENTCUST = .T.
        THIS.LFFILLEMPARR(THIS.LOFORM.PARENT,PADR(ALLTRIM(THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE),5),THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE)
        THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.VISIBLE = .T.
        THIS.LOFORM.PAGEFRAME.PAGE2.LBLEMPL.VISIBLE = .T.
        THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.REQUERY()
        *This.loForm.pageFrame.page2.cboEmpl.Value = SPACE(12)
      ELSE
        THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.VISIBLE = .F.
        THIS.LOFORM.PAGEFRAME.PAGE2.LBLEMPL.VISIBLE = .F.
      ENDIF
    ELSE
      IF !EMPTY(THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE) AND THIS.LOFORM.PARENT.LLENTCUST
        THIS.LFFILLEMPARR(THIS.LOFORM.PARENT,PADR(ALLTRIM(THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE),5))
        THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.REQUERY()
      ENDIF
    ENDIF
    *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[End]

    THIS.LCSTATUS  = IIF(AT(RETAUTH.STATUS,"OCXE") > 0 , THIS.LASTATUS[AT(RetAuth.Status,"OCXE")] , "")

    *-- Get the tax rate from the lines file.
    THIS.LNTAXRATE = IIF(THIS.RALINE.SEEK(RETAUTH.RANO),RALINE.TAX_RATE , 0)

    *Pick the store address in the from address if the store field is not empty.
    IF (!EMPTY(RETAUTH.STORE) .AND. THIS.CUSTOMER.SEEK('S'+RETAUTH.ACCOUNT+RETAUTH.STORE)) .OR. ;
        (EMPTY(RETAUTH.STORE) .AND.  THIS.CUSTOMER.SEEK('M'+RETAUTH.ACCOUNT))
      *-- Get the current customer info (name & address).
      THIS.LCNAME    = CUSTOMER.BTNAME
      THIS.LCFNAME    = CUSTOMER.STNAME
      THIS.LCFADDRES1 = CUSTOMER.CADDRESS1
      THIS.LCFADDRES2 = CUSTOMER.CADDRESS2
      THIS.LCFADDRES3 = CUSTOMER.CADDRESS3
      THIS.LCFADDRES4 = ALLTRIM(CUSTOMER.CADDRESS4)+', '+ALLTRIM(CUSTOMER.CADDRESS5)
      THIS.LCFADDRES5 = CUSTOMER.CADDRESS6

    ELSE
      STORE SPACE(30) TO THIS.LCNAME , THIS.LCFNAME , THIS.LCFADDRES1 ,;
        THIS.LCFADDRES2 , THIS.LCFADDRES3 , THIS.LCFADDRES4 ,;
        THIS.LCFADDRES5
    ENDIF
    *-- Fill the warehouse popup with the right value.
    THIS.PUWAREHOUS = IIF(ASCAN(THIS.LAWAREHOUS,RETAUTH.CWARECODE) > 0 , ASUBSCRIPT(THIS.LAWAREHOUS , ASCAN(THIS.LAWAREHOUS,RETAUTH.CWARECODE) , 1) , 0) && Revise
    *-- Get the warehouse address.
    IF THIS.WAREHOUS.SEEK(RETAUTH.CWARECODE)
      THIS.LCTADDRES1 = WAREHOUS.CADDRESS1
      THIS.LCTADDRES2 = WAREHOUS.CADDRESS2
      THIS.LCTADDRES3 = WAREHOUS.CADDRESS3
      THIS.LCTADDRES4 = ALLTRIM(WAREHOUS.CADDRESS4)+', '+ALLTRIM(WAREHOUS.CADDRESS5)
      THIS.LCTADDRES5 = WAREHOUS.CADDRESS6
    ELSE
      STORE SPACE(30) TO THIS.LCTADDRES1 , THIS.LCTADDRES2 , THIS.LCTADDRES3 ,;
        THIS.LCTADDRES4 , THIS.LCTADDRES5
    ENDIF
    *-- Define check box "[] return entire invoice".
    THIS.CBCOMPLETE = IIF(RETAUTH.CINTR_INV='Y',.T.,.F.)


    *-- Search for the first R/A line in the master file.
    =THIS.RALINE.SEEK(RETAUTH.RANO)
    * Read the RALINE records in a Temprory File even in the View Mode
    LCTEMP = DBF('Raline')
    SELECT * , 'S' AS 'CSTATUS' , RECNO() AS 'NRECNO' , ;
      IIF(!EMPTY(RETAUTH.CWARECODE) , .T. , .F.) AS 'LSHIPPED', ;
      STYLE AS OLDSTY ;
      FROM RALINE ;
      WHERE RALINE.RANO = RETAUTH.RANO ;
      INTO DBF (OARIAAPPLICATION.WORKDIR + THIS.LCTMPRETLN)

    *-- Change the delete prompt in the toolbar & the delete message.
    *Old: lcPromp    = IIF(RetAuth.Status = 'X' , lcUnCan , lcCancel )
    *Old: lcDelMesag = IIF(RetAuth.Status = 'X' , "uncancel" , "cancel" )
    *Old: SHOW GET pbDlt,1 PROMPT lcPromp && Revise

    *-- Control the enabling & disabling of the edit button in the control pannel.
    IF RETAUTH.STATUS $ 'XC'
      *Old: SHOW GET pbEdt DISABLE
      THIS.LLEDIT = .F.
    ELSE
      *Old: SHOW GET pbEdt ENABLE
      THIS.LLEDIT = .T.
    ENDIF

    *-- Enable the notes & the object linking buttons in the control pannel.
    *Old: SHOW GET pbNotes  ENABLE
    THIS.LLNOTES = .T.
    *Old: SHOW GET pbLinkTo ENABLE && Revise
    THIS.LLOBJECTLINK = .T.

    *B99999,1 18/04/2006 MMT,fix bug of enabled account field in view mode[Start]
    THIS.LOFORM.KBACCOUNT.ENABLED = .F.
    *B99999,1 18/04/2006 MMT,fix bug of enabled account field in view mode[End]
    *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[Start]
    IF THIS.CUSTOMER.SEEK("M" + RETAUTH.ACCOUNT) AND CUSTOMER.LCHKENTLM
      THIS.LOFORM.PARENT.LLENTCUST = .T.
      THIS.LFFILLEMPARR(THIS.LOFORM.PARENT,RETAUTH.ACCOUNT,RETAUTH.STORE)
      THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.REQUERY()
      THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.VISIBLE = .T.
      THIS.LOFORM.PAGEFRAME.PAGE2.LBLEMPL.VISIBLE = .T.
    ELSE
      THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.VISIBLE = .F.
      THIS.LOFORM.PAGEFRAME.PAGE2.LBLEMPL.VISIBLE = .F.
    ENDIF
    *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[End]
  CASE THIS.LOFORMSET.ACTIVEMODE="E"
    *-- Define the browse titles.
    THIS.LLCUPDATE = .T.
    _TALLY    = 0

    *Add lShipped field in the temp lines file.
    *-- Select the current R/A lines in the temp. file.
    THIS.RALINE.SEEK(RETAUTH.RANO) && In Case RaLine is SQL, this will retrieve all the Lines of the RA
    SELECT * , 'S' AS 'CSTATUS' , RECNO() AS 'NRECNO' , ;
      IIF(!EMPTY(RETAUTH.CWARECODE) , .T. , .F.) AS 'LSHIPPED', ;
      STYLE AS OLDSTY ;
      FROM RALINE ;
      WHERE RALINE.RANO = RETAUTH.RANO ;
      INTO DBF (OARIAAPPLICATION.WORKDIR + THIS.LCTMPRETLN)

    THIS.LNNOOFLINES = _TALLY

    *B608545,1 WAM 05/08/2008 Get the last RA line number
    DECLARE LARA_LINNO[1]
    SELECT MAX(CRA_LINNO) FROM RALINE ;
      WHERE RANO = RETAUTH.RANO INTO ARRAY LARA_LINNO

    *B609464,1 MMT 11/22/2010 Error while saving RA has no lines after editing it[Start]
    LARA_LINNO[1] = IIF(THIS.LNNOOFLINES > 0,LARA_LINNO[1],'0')
    *B609464,1 MMT 11/22/2010 Error while saving RA has no lines after editing it[End]

    THIS.LNRA_LINNO = VAL(LARA_LINNO[1])
    *B608545,1 WAM 05/08/2008 (End)

    GO TOP

    *-- Enable the notes & the object linking buttons in the control pannel.
    *Old: SHOW GET pbNotes  ENABLE
    THIS.LLNOTES = .T.
    *Old: SHOW GET pbLinkTo ENABLE && Revise
    THIS.LLOBJECTLINK = .T.

  CASE THIS.LOFORMSET.ACTIVEMODE="A"
    *-- Define the needed variables incase of add mode.
    IF !THIS.LLADDMODE
      *-- Flag to know if enter the add mode before or not.
      THIS.LLADDMODE  = .T.
      THIS.LLCUPDATE  = .T.
      *-- Blank the temp. R/A lines file.
      SELECT (THIS.LCTMPRETLN)
      ZAP
      *-- Initialize the needed variables.
      STORE ' ' TO THIS.LCNAME , THIS.LCFNAME , ;
        THIS.LCFADDRES1 , THIS.LCFADDRES2 , THIS.LCFADDRES3 ,;
        THIS.LCFADDRES4 , THIS.LCFADDRES5 , THIS.LCSTYLE , THIS.LCDESC ,;
        THIS.LCREASON , THIS.LCCUSTPLVL , THIS.LCDYELOT ,;
        THIS.LCCONSOL , THIS.LCRA_LINNO

      STORE 0 TO THIS.LNTOTQTY,THIS.LNPRICE,THIS.LNTOTAL,;
        THIS.LNNOOFLINES,THIS.LNTAXRATE,THIS.LNPSTRATE,THIS.LNDISCPCNT ,;
        THIS.LNRA_LINNO , THIS.LNTAXZAMT

      *-- Default the tax rate from the company setup.
      THIS.LNTAXRATE = THIS.LNDEFTAXRT

      *-- Set R/A status to be open.
      REPLACE RETAUTH.STATUS WITH 'O' IN RETAUTH
      THIS.LCSTATUS   = IIF(AT(RETAUTH.STATUS,"OCXE") > 0 , THIS.LASTATUS[AT(RetAuth.Status,"OCXE")] , "")

      REPLACE RETAUTH.RADATE  WITH OARIAAPPLICATION.SYSTEMDATE IN RETAUTH
      REPLACE RETAUTH.VOID  WITH IIF(DOW(OARIAAPPLICATION.SYSTEMDATE+10) <> 7 , OARIAAPPLICATION.SYSTEMDATE+10 , OARIAAPPLICATION.SYSTEMDATE+12) IN RETAUTH
      REPLACE RETAUTH.CARTONS WITH 1 IN RETAUTH
      REPLACE RETAUTH.CINTR_INV WITH 'N' IN RETAUTH

      *-- Get the warehouse value & its address.
      REPLACE RETAUTH.CWARECODE  WITH THIS.LAWAREHOUS[1,2]  IN RETAUTH
      THIS.PUWAREHOUS = 1 && Revise
      IF THIS.WAREHOUS.SEEK(RETAUTH.CWARECODE)
        THIS.LCTADDRES1 = WAREHOUS.CADDRESS1
        THIS.LCTADDRES2 = WAREHOUS.CADDRESS2
        THIS.LCTADDRES3 = WAREHOUS.CADDRESS3
        THIS.LCTADDRES4 = ALLTRIM(WAREHOUS.CADDRESS4)+', '+ALLTRIM(WAREHOUS.CADDRESS5)
        THIS.LCTADDRES5 = WAREHOUS.CADDRESS6
      ELSE
        STORE SPACE(30) TO THIS.LCTADDRES1 , THIS.LCTADDRES2 , THIS.LCTADDRES3 ,;
          THIS.LCTADDRES4 , THIS.LCTADDRES5
      ENDIF
      THIS.LASIZE     = ""
      THIS.LAQTYSTK   = 0
      THIS.CBCOMPLETE = .F.

      *-- Fill the codes popups with the default values.
      *-- (Header reason , Division , Detail Reason).
      THIS.LNRESON2   = 1
      THIS.LOFORM.PAGEFRAME.PAGE1.CBOREASON.VALUE = THIS.LOFORM.PAGEFRAME.PAGE1.CBOREASON.CODEDEFAULTVALUE
      THIS.LOFORM.PAGEFRAME.PAGE1.CBODIVISION.VALUE = THIS.LOFORM.PAGEFRAME.PAGE1.CBODIVISION.CODEDEFAULTVALUE

      *Old: _CUROBJ = OBJNUM(RetAuth.Account) && Revise
      THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.SETFOCUS()
    ENDIF
    THIS.CBCOMPLETE = IIF(RETAUTH.CINTR_INV = 'Y' , .T. , .F.)

    *-- Disable the notes button in the control pannel.
    *Old: SHOW GET pbNotes DISABLE
    THIS.LLNOTES = .F.
  ENDCASE

  IF !THIS.LOFORMSET.ACTIVEMODE="S"
    *Enable the detail folder.
    THIS.LOFORM.PAGEFRAME.PAGE2.ENABLED = .T.
  ELSE
    THIS.LOFORMSET.LCSYDKEY = "Z"+"'"+THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE+"'"
    THIS.LOFORM.KBRANO.ENABLED = .T.
    THIS.LOFORM.KBRANO.KEYTEXTBOX.SETFOCUS()
  ENDIF
  THIS.LOFORM.KBSTORE.ENABLED = INLIST(THIS.LOFORMSET.ACTIVEMODE,'A','E')



  *-- Assign the status of each object on the screen does not use var. "lcObjStat"
  THIS.LCOBJSTAT = IIF(THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A" , "ENABLE" , "DISABLE")
  *Enable the warehouse popup even there is invoice.
  THIS.LCWARSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. THIS.LLMULTIWH , "ENABLE" , "DISABLE")
  THIS.LCINQSTAT = IIF(!EMPTY(RETAUTH.INVOICE) .AND. !THIS.LOFORMSET.ACTIVEMODE="S" , "ENABLE" , "DISABLE")

  *Don't include lnNoOfLines when determining the invoice object status
  *                  as it should be always enabled in Add/Edit modes
  THIS.LCINVSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A"), "ENABLE" , "DISABLE")

  *Disable the division if entered any styles.
  THIS.LCDIVSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(RETAUTH.INVOICE) .AND. THIS.LNNOOFLINES = 0 , "ENABLE" , "DISABLE")

  THIS.LCCURSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(RETAUTH.INVOICE) , "ENABLE" , "DISABLE")
  THIS.LCEXRSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(RETAUTH.INVOICE) .AND. !EMPTY(RETAUTH.CCURRCODE) .AND. THIS.LLEDITEXRT .AND. (RETAUTH.CCURRCODE <> OARIAAPPLICATION.BASECURRENCY) , "ENABLE" , "DISABLE")

  *Don't include lnNoOfLines when determining the invoice object status
  *                  as it should be always enabled in Add/Edit modes
  THIS.LCENTRSTAT= IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EOF(THIS.LCTMPRETLN) .AND. !EMPTY(RETAUTH.INVOICE) , "ENABLE" , "DISABLE")

  THIS.LCRELSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(RETAUTH.INVOICE) , "ENABLE" ,"DISABLE")
  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
  THIS.LCORDSTAT = IIF((THIS.LCRELSTAT == "ENABLE" .OR. (TYPE('This.laConsInvOrdrs') <> 'U' .AND. ALEN(THIS.LACONSINVORDRS,1) >1)) , "ENABLE" ,"DISABLE")
  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]

  *Define variable to control the enabling & disabling of the account field.
  *This.lcAccStat = IIF(This.loFormSet.ActiveMode="V" , "DISABLE" , "ENABLE")
  THIS.LCACCSTAT = IIF(THIS.LOFORMSET.ACTIVEMODE="V" OR ;
    !EMPTY(THIS.LOFORM.PAGEFRAME.PAGE1.KBINVOICE.KEYTEXTBOX.VALUE) OR ;
    !EMPTY(THIS.LOFORM.PAGEFRAME.PAGE1.KBORDER.KEYTEXTBOX.VALUE), "DISABLE" , "ENABLE")
  *ASM, Disable Account Code if Invoice or Order # was entered [End]

  *-- Set flag to rebrowse in the detail folder.
  THIS.LLREBROWSE = .T.

  *Old: = lfActFolder()


  IF THIS.LOFORM.PAGEFRAME.ACTIVEPAGE=1
    THIS.LFREFHDR()
  ELSE
    *B608234,1 WAM 08/23/2007 Browse shifts lines up when add new line
    *This.lfRefDet()
    *B608234,1 WAM 08/23/2007 (End)
  ENDIF
  *B608234,1 WAM 08/23/2007 Browse shifts lines up when add new line
  THIS.LFREFDET()
  *B608234,1 WAM 08/23/2007 (End)


  *-- Select the main file.
  SELECT RETAUTH

  ENDFUNC
  *--end of lpShow


  *!*************************************************************
  *! Name      : lfRefHdr
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To be called from the Activate Event of the PageFrame.Page1
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFREFHDR
  PRIVATE LNCOUNT , LCCOUNT

  WITH THIS.LOFORM
    *Refresh the account objects by the variable defined.
    .KBACCOUNT.ENABLED =  (THIS.LCACCSTAT='ENABLE')
    *-- Refresh the objects in screen : (lcRtAth1)

    WITH .PAGEFRAME.PAGE1
      STORE (THIS.LCOBJSTAT='ENABLE') TO .DTVOID.ENABLED, .TXTAUTH.ENABLED, ;
        .TXTAUTHAMT.ENABLED, .CBOREASON.ENABLED, .TXTTAXRATE.ENABLED, ;
        .TXTCRETNOTE1.ENABLED, .TXTCRETNOTE2.ENABLED

      *Disable the division if entered any styles.
      THIS.LCDIVSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(RETAUTH.INVOICE) .AND. THIS.LNNOOFLINES = 0 , "ENABLE" , "DISABLE")

      *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
      *.cmdInvoice.Enabled =(This.lcInqStat='ENABLE')
      *.kbinvoice.Enabled = (This.lcInvStat='ENABLE')
      *.chkEntireInvoice.Enabled = (This.lcEntrStat='ENABLE')
      .CMDINVOICE.ENABLED = (THIS.LCINQSTAT='ENABLE') .AND. !THIS.LOFORM.PAGEFRAME.PAGE1.CHKENTIREINVOICE.VALUE
      .KBINVOICE.ENABLED  = (THIS.LCINVSTAT='ENABLE') .AND. !THIS.LOFORM.PAGEFRAME.PAGE1.CHKENTIREINVOICE.VALUE
      .CHKENTIREINVOICE.ENABLED = (THIS.LCENTRSTAT='ENABLE')
      .KBORDER.ENABLED    = (THIS.LCORDSTAT='ENABLE') .AND. !THIS.LOFORM.PAGEFRAME.PAGE1.CHKENTIREINVOICE.VALUE
      *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]

      .TXTCUSTPO.ENABLED = (THIS.LCRELSTAT='ENABLE')
      *Enable the cartons field to let the user change it's numer to be used while printing the stickers.
      .TXTCARTONS.ENABLED = (THIS.LCINVSTAT='ENABLE')
      .CBOCWARECODE.ENABLED = (THIS.LCWARSTAT='ENABLE')
      .CBODIVISION.ENABLED = (THIS.LCDIVSTAT='ENABLE')
      .KBCCURRCODE.ENABLED = (THIS.LCCURSTAT='ENABLE')
      .TXTNEXRATE.ENABLED = (THIS.LCEXRSTAT='ENABLE')
    ENDWITH

  ENDWITH

  ENDFUNC
  *--end of lfRefHdr

  *!*************************************************************
  *! Name      : lfRefDet
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To be called from the Activate Event of the PageFrame.Page2
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFREFDET

  THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'INVHDR',OARIAAPPLICATION.DATADIR+'INVHDR')
  THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'INVLINE',OARIAAPPLICATION.DATADIR+'INVLINE')
  THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'STYDYE',OARIAAPPLICATION.DATADIR+'STYDYE')
  THIS.LLENTERDET = .T.
  SELECT RETAUTH
  THIS.LFBRLINE()
  *This.loForm.PageFrame.Page2.grdRALines.SetFocus()

  ENDFUNC
  *--end of lfRefDet

  *!*************************************************************
  *! Name      : lfvRANO
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the kbRANO Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVRANO

  *-- Fill the R/A with zeros on the left if not browsing.
  THIS.LLBROWSE = THIS.LOFORM.KBRANO.SELECTEDFROMBROWSE
  THIS.LLBROWSE  = IIF((!THIS.LLBROWSE .AND. "?" $ THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE) .OR. THIS.LLBROWSE , .T. , .F.)
  *! B609120,1 MMT 12/30/2009 Fix bug of padding RA# with 0'S [Start]
  *This.loForm.kbrano.KeyTextBox.Value = IIF(This.llBrowse .OR. EMPTY(This.loForm.kbrano.KeyTextBox.Value) , This.loForm.kbrano.KeyTextBox.Value , PADL(ALLTRIM(This.loForm.kbrano.KeyTextBox.Value) , 6 , "0"))
  THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE = IIF(THIS.LLBROWSE .OR. EMPTY(THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE) , THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE , ALLTRIM(THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE))
  *! B609120,1 MMT 12/30/2009 Fix bug of padding RA# with 0'S [End]


  IF (THIS.LLBROWSE .OR. !EMPTY(THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE))
    *-- Fill the browse fields.
    LCBRFIELDS = "Retauth.rano:7 :H='R/A#', Retauth.status:2 :H='S', "+;
      "Retauth.account:6 :H='Acct#', "+;
      "lcDummy=LOOKUP(CUSTOMER.BtName,'M'+Retauth.account,CUSTOMER.ACCOUNT,'CUSTOMER') :H='Name':15, "+;
      "Retauth.store:9 :H='Store', Retauth.radate:8 :H='Issued', "+;
      "Retauth.void:8 :H='Void', Retauth.auth:7 :H='Auth.', "+;
      "Retauth.tran:7 :H='Credit #'"

    *Give the ability to browse RAs by customer.
    IF !EMPTY(THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE) AND !EMPTY(THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE) AND !THIS.RETAUTH.SEEK(THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE)
      SELECT RETAUTH
      THIS.RETAUTH.SETORDER('Retautha')

      DECLARE LARA[1]
      LARA[1]   = THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE
      =GFBROWS("RetAuth.Account" , "RANO" , "laRA")
      THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE = IIF(THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE = LARA[1] , SPACE(6) , LARA[1])

      THIS.RETAUTH.SETORDER('Retauth')

      THIS.LLBROWSE = .F.
    ENDIF

    THIS.LOFORM.KBRANO.KEYTEXTBOX.REFRESH()

    *If the RA object not empty, call global function to handle the modes.
    IF THIS.LLBROWSE OR !EMPTY(THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE)
      *-- Call global function to handle the action & the screen mode
      *-- according to this action.
      *Old: =gfSeekRec()
      IF EMPTY(THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE) OR ALLTRIM(THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE)='?'
        IF !OARIAAPPLICATION.OTOOLBAR.CMDFIND.CLICK()
          THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE = ''
          RETURN 0
        ENDIF
      ELSE
        THIS.LOFORMSET.SEEKRECORD(THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE)
        IF THIS.LOFORMSET.ACTIVEMODE<>'V'
          THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE = ''
          RETURN 0
        ENDIF
      ENDIF
      *-- Blank the credit memo field to get a seq. no. upon saving.
      THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE = IIF(THIS.LOFORMSET.ACTIVEMODE="A" , SPACE(6) , THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE)
    ENDIF

  ENDIF
  THIS.LLBROWSE = .F.

  SELECT RETAUTH

  RETURN 1
  ENDFUNC
  *--end of lfvRANO


  *!*************************************************************
  *! Name      : lfvAccount
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the kbAccount Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVACCOUNT

  *! B609603,1 SAB 09/26/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
  DIMENSION THIS.LACONSINVORDRS [1,1]
  *! B609603,1 SAB 09/26/2011 Consolidated Invoice Case not checking order and not loading employee info[End]

  THIS.LLBROWSE = THIS.LOFORM.KBACCOUNT.SELECTEDFROMBROWSE
  IF (THIS.LLBROWSE .OR. !(THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE == THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.OLDVALUE))
    IF !THIS.CUSTOMER.SEEK ("M" + THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE) .OR. THIS.LLBROWSE
      *-- Call the main customer browse.
      XACCOUNT  = THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE
      DO CUSBROWM WITH XACCOUNT
      THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE = XACCOUNT
    ENDIF
    *we still validition for cancelled and potentail customer
    IF !EMPTY(THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE) .AND. ! CUSTOMER.STATUS $ 'AH'
      *Message : 46038
      *"Unable to create a new R/A, customer status might be canceled or potential."
      *Button  : 00000
      *                         <  Ok  >
      =GFMODALGEN('TRM46038B00000','ALERT','R/A')
      THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE = ''
      THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE = ''
      THIS.LOFORM.KBSTORE.KEYTEXTBOX.REFRESH()
      THIS.LOFORM.TXTACCOUNTNAME.VALUE = ''
      THIS.LOFORM.TXTACCOUNTNAME.REFRESH()
      RETURN 0
    ENDIF

    THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.REFRESH()

    *Pick the store address in the from address if the store field is not empty.
    IF (!EMPTY(THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE) .AND. ;
        THIS.CUSTOMER.SEEK('S'+THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE+THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE) .OR. ;
        (EMPTY(THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE) .AND.  THIS.CUSTOMER.SEEK('M'+THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE)))
      *-- Get the current customer address.


      *Get the main account addresses or main store addresses not
      *the alternatives addresses for the account or the store.
      THIS.LCFNAME    = CUSTOMER.STNAME
      THIS.LCFADDRES1 = CUSTOMER.CADDRESS1
      THIS.LCFADDRES2 = CUSTOMER.CADDRESS2
      THIS.LCFADDRES3 = CUSTOMER.CADDRESS3
      THIS.LCFADDRES4 = ALLTRIM(CUSTOMER.CADDRESS4)+','+ALLTRIM(CUSTOMER.CADDRESS5)
      THIS.LCFADDRES5 = CUSTOMER.CADDRESS6

    ELSE
      STORE SPACE(1) TO THIS.LCFNAME,THIS.LCFADDRES1,THIS.LCFADDRES2,THIS.LCFADDRES3,THIS.LCFADDRES4,THIS.LCFADDRES5
    ENDIF

    IF THIS.CUSTOMER.SEEK("M" + THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE)
      THIS.LCNAME     = CUSTOMER.BTNAME
      THIS.LCCUSTPLVL = CUSTOMER.PRICELVL
      THIS.LNCUSPSTRT = CUSTOMER.NTAXRATE
      THIS.LNPSTRATE  = IIF(THIS.LLISCANADA , CUSTOMER.NTAXRATE , 0)
      THIS.LNTAXRATE  = IIF(!THIS.LLISCANADA , CUSTOMER.NTAXRATE , 0)
      *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[Start]
      IF CUSTOMER.LCHKENTLM
        THIS.LOFORM.PARENT.LLENTCUST = .T.
        THIS.LFFILLEMPARR(THIS.LOFORM.PARENT,THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE)
        THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.REQUERY()
        THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.VISIBLE = .T.
        THIS.LOFORM.PAGEFRAME.PAGE2.LBLEMPL.VISIBLE = .T.
      ELSE
        THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.VISIBLE = .F.
        THIS.LOFORM.PAGEFRAME.PAGE2.LBLEMPL.VISIBLE = .F.
      ENDIF
      *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[End]

      THIS.LOFORM.PAGEFRAME.PAGE1.KBCCURRCODE.KEYTEXTBOX.VALUE = CUSTOMER.CCURRCODE
      IF THIS.LLMULCURR
        IF THIS.LOFORM.PAGEFRAME.PAGE1.KBCCURRCODE.KEYTEXTBOX.VALUE = OARIAAPPLICATION.BASECURRENCY
          REPLACE RETAUTH.NCURRUNIT WITH 1
          THIS.LOFORM.PAGEFRAME.PAGE1.TXTNEXRATE.VALUE = 1
        ELSE
          *-- If the currency is different from the base currency, get the rate & the unit.
          LCCURRENCY = THIS.LOFORM.PAGEFRAME.PAGE1.KBCCURRCODE.KEYTEXTBOX.VALUE
          LNCURRUNIT = THIS.LNCURRUNIT
          REPLACE RETAUTH.NEXRATE WITH GFCHKRATE('lnCurrUnit' , LCCURRENCY , OARIAAPPLICATION.SYSTEMDATE,;
            .T. , OARIAAPPLICATION.ACTIVECOMPANYID , .F. , THIS.LLEDITEXRT) IN RETAUTH
          THIS.LNCURRUNIT = LNCURRUNIT
          REPLACE RETAUTH.NCURRUNIT WITH THIS.LNCURRUNIT IN RETAUTH
          THIS.LOFORM.PAGEFRAME.PAGE1.KBCCURRCODE.KEYTEXTBOX.VALUE = LCCURRENCY
        ENDIF
      ELSE
        THIS.LOFORM.PAGEFRAME.PAGE1.KBCCURRCODE.KEYTEXTBOX.VALUE = OARIAAPPLICATION.BASECURRENCY
        REPLACE RETAUTH.NCURRUNIT WITH 1 IN RETAUTH
        THIS.LOFORM.PAGEFRAME.PAGE1.TXTNEXRATE.VALUE = 1
      ENDIF
    ELSE
      *-- If the customer not found, blank the related variable.
      STORE SPACE(5) TO THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE
      STORE SPACE(1) TO THIS.LCNAME , THIS.LCCUSTPLVL
      THIS.LNCUSPSTRT = 0
      THIS.LNPSTRATE  = 0

      THIS.LNTAXRATE  = IIF(THIS.LOFORMSET.ACTIVEMODE="S" , 0 , THIS.LNDEFTAXRT)
      THIS.LOFORM.PAGEFRAME.PAGE1.KBCCURRCODE.KEYTEXTBOX.VALUE = IIF(THIS.LOFORMSET.ACTIVEMODE="S" , "" , OARIAAPPLICATION.BASECURRENCY)
      REPLACE RETAUTH.NCURRUNIT WITH IIF(THIS.LOFORMSET.ACTIVEMODE="S" , 0 , 1) IN RETAUTH
      THIS.LOFORM.PAGEFRAME.PAGE1.TXTNEXRATE.VALUE = IIF(THIS.LOFORMSET.ACTIVEMODE="S" , 0 , 1)
    ENDIF
    THIS.LCEXRSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(RETAUTH.INVOICE) .AND. !EMPTY(RETAUTH.CCURRCODE) .AND. THIS.LLEDITEXRT .AND. (RETAUTH.CCURRCODE <> OARIAAPPLICATION.BASECURRENCY) , "ENABLE" , "DISABLE")

    THIS.LOFORM.TXTACCOUNTNAME.REFRESH()
    WITH THIS.LOFORM.PAGEFRAME.PAGE1
      .REFRESH()
      .TXTNEXRATE.ENABLED = (THIS.LCEXRSTAT='ENABLE')
    ENDWITH
    *If the RA object is empty, go to fill it.
    IF THIS.LOFORMSET.ACTIVEMODE="S" AND EMPTY(RETAUTH.RANO) && Global
      *This.loForm.kbRANO.KeyTextBox.SetFocus()
      RETURN -1
    ENDIF
  ENDIF
  THIS.LLBROWSE   = .F.
  SELECT RETAUTH

  RETURN 1
  ENDFUNC
  *--end of lfvAccount


  *!*************************************************************
  *! Name      : lfvStore
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the kbStore Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVSTORE

  THIS.LLBROWSE = THIS.LOFORM.KBSTORE.SELECTEDFROMBROWSE
  * IF ACCOUNT CODE IS EMPTY AND ENTRING STORE CODE
  IF EMPTY(RETAUTH.ACCOUNT) .AND. !EMPTY(THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE) && Global
    *Message : 02040
    *"You have to enter an account code."
    *Button  : 00000
    *                       <  Ok  >
    =GFMODALGEN('TRM02040B00000','ALERT')
    THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE = ''
    RETURN -1
  ENDIF

  IF (THIS.LLBROWSE .OR. !(THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE == THIS.LOFORM.KBSTORE.KEYTEXTBOX.OLDVALUE))
    IF !THIS.CUSTOMER.SEEK ('S'+RETAUTH.ACCOUNT+THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE) .OR. THIS.LLBROWSE
      XSTORE = THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE
      *-- Call function to browse the available stores for the current customer.
      IF !CUSBROWS(RETAUTH.ACCOUNT,.T.)
        THIS.LLBROWSE = .F.
        THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE = SPACE(8)
      ELSE
        THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE = XSTORE
      ENDIF
    ENDIF
    THIS.LOFORM.KBSTORE.KEYTEXTBOX.REFRESH()
    IF !(THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE == THIS.LOFORM.KBSTORE.KEYTEXTBOX.OLDVALUE)
      *-- If there is valid store entered, get its address.
      *Get the main account addresses or main store addresses not
      *the alternatives addresses for the account or the store. [Begin]
      IF !EMPTY(THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE)
        THIS.LCFNAME    = CUSTOMER.STNAME
        THIS.LCFADDRES1 = CUSTOMER.CADDRESS1
        THIS.LCFADDRES2 = CUSTOMER.CADDRESS2
        THIS.LCFADDRES3 = CUSTOMER.CADDRESS3
        THIS.LCFADDRES4 = ALLTRIM(CUSTOMER.CADDRESS4)+','+ALLTRIM(CUSTOMER.CADDRESS5)
        THIS.LCFADDRES5 = CUSTOMER.CADDRESS6
      ELSE
        *-- If there is no store entered, get the customer address.
        IF THIS.CUSTOMER.SEEK("M" + RETAUTH.ACCOUNT)
          THIS.LCFNAME    = CUSTOMER.STNAME
          THIS.LCFADDRES1 = CUSTOMER.CADDRESS1
          THIS.LCFADDRES2 = CUSTOMER.CADDRESS2
          THIS.LCFADDRES3 = CUSTOMER.CADDRESS3
          THIS.LCFADDRES4 = ALLTRIM(CUSTOMER.CADDRESS4)+','+ALLTRIM(CUSTOMER.CADDRESS5)
          THIS.LCFADDRES5 = CUSTOMER.CADDRESS6
        ELSE
          *-- If there is no customer , blank the address fields.
          REPLACE RETAUTH.ACCOUNT WITH SPACE(5) IN RETAUTH
          STORE SPACE(1) TO THIS.LCFNAME,THIS.LCFADDRES1,THIS.LCFADDRES2,;
            THIS.LCFADDRES3,THIS.LCFADDRES4,THIS.LCFADDRES5
        ENDIF
      ENDIF
      *-- Refresh the customer name & address objects.
      THIS.LOFORM.PAGEFRAME.PAGE1.REFRESH()
    ENDIF
  ENDIF

  *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[Start]
  IF !EMPTY(THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE)
    LNOLDSELECT = SELECT()
    SELECT CUSTOMER
    LCCUSTKEY = EVALUATE(KEY())
    IF THIS.CUSTOMER.SEEK('M'+RETAUTH.ACCOUNT) AND CUSTOMER.LCHKENTLM
      THIS.LFFILLEMPARR(THIS.LOFORM.PARENT,RETAUTH.ACCOUNT,THIS.LOFORM.KBSTORE.KEYTEXTBOX.VALUE)
      THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.REQUERY()
      THIS.LOFORM.PARENT.LLENTCUST = .T.
    ENDIF
    THIS.CUSTOMER.SEEK(LCCUSTKEY)
    SELECT(LNOLDSELECT)
  ENDIF
  *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[End]

  THIS.LLBROWSE = .F.
  SELECT RETAUTH

  RETURN 1
  ENDFUNC
  *--end of lfvStore


  *!*************************************************************
  *! Name      : lfvWareHouse
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the cboWareCode Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVWAREHOUSE

  *display message if user select location not assign to style[start]
  PRIVATE LNRECNO
  LCPRVALIS = ALIAS()      && Save current alias.
  LNRECNO=RECNO()
  LLFIRST   = .T.          && flag to display the message only once.
  LLASSIGN  = .F.          && Flag to assign all styles to changed warehouse.
  SELECT (THIS.LCTMPRETLN)      && Select temp. file hold return auth lines.
  IF RECCOUNT()>0
    *--if there is one or more style assign to location
    *Open the stydye file to avoid error 'alias not found' when editing an existing RA, and try to change the warehouse.
    THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'STYDYE',OARIAAPPLICATION.DATADIR+'STYDYE')
    SELECT (THIS.LCTMPRETLN)
    SCAN
      *-- Check if there is any styles not assigned to the changed warehouse.
      LCTEMP = PADR(EVALUATE(THIS.LCTMPRETLN+'.Style'),19)
      IF !THIS.STYDYE.SEEK(LCTEMP + THIS.LOFORM.PAGEFRAME.PAGE1.CBOCWARECODE.VALUE + SPACE(10))
        IF LLFIRST
          LLFIRST = .F.
          *** There is one or more styles not assigned to location: laWareHous[puWareHous,2]. ***
          *** <  Assign  > - < Cancel > ***
          IF GFMODALGEN("QRM46034B46003" , "DIALOG" , THIS.LOFORM.PAGEFRAME.PAGE1.CBOCWARECODE.VALUE) = 1
            *--Set flag to assign the unsaaigned styles to the current warehouse.
            LLASSIGN = .T.
          ENDIF
        ENDIF
        *--Assign the styles to the selected location.
        IF LLASSIGN
          REPLACE RETAUTH.CWARECODE WITH THIS.LOFORM.PAGEFRAME.PAGE1.CBOCWARECODE.VALUE IN RETAUTH
          =THIS.WAREHOUS.SEEK(RETAUTH.CWARECODE)
          DO GPADSTYWAR WITH PADR(EVALUATE(THIS.LCTMPRETLN+'.Style'),19) , SPACE(10) , THIS.LOFORM.PAGEFRAME.PAGE1.CBOCWARECODE.VALUE
        ELSE
          *-- Do not assign & restore the old location.
          THIS.LOFORM.PAGEFRAME.PAGE1.CBOCWARECODE.VALUE = THIS.LOFORM.PAGEFRAME.PAGE1.CBOCWARECODE.OLDVALUE
          =THIS.WAREHOUS.SEEK(THIS.LOFORM.PAGEFRAME.PAGE1.CBOCWARECODE.VALUE)
          *--refresh oldvalue
          THIS.LOFORM.PAGEFRAME.PAGE1.CBOCWARECODE.REFRESH()
          SELECT(LCPRVALIS)
          RETURN
        ENDIF
      ENDIF
      SELECT (THIS.LCTMPRETLN)
    ENDSCAN
  ENDIF
  SELECT(LCPRVALIS)
  *-- If enetered valid warehouse, get its address.
  IF THIS.WAREHOUS.SEEK(RETAUTH.CWARECODE)
    THIS.LCTADDRES1 = WAREHOUS.CADDRESS1
    THIS.LCTADDRES2 = WAREHOUS.CADDRESS2
    THIS.LCTADDRES3 = WAREHOUS.CADDRESS3
    THIS.LCTADDRES4 = ALLTRIM(WAREHOUS.CADDRESS4)+', '+ALLTRIM(WAREHOUS.CADDRESS5)
    THIS.LCTADDRES5 = WAREHOUS.CADDRESS6
  ENDIF
  THIS.LOFORM.PAGEFRAME.PAGE1.REFRESH()

  ENDFUNC
  *--end of lfvWareHouse


  *!*************************************************************
  *! Name      : lfvVoid
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the dtVoid Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVVOID

  IF THIS.LOFORM.PAGEFRAME.PAGE1.DTVOID.VALUE < RETAUTH.RADATE
    *** Void after cannot be less than the entered date. ***
    *** <  Ok  > ***
    =GFMODALGEN("INM46016B00000" , "DIALOG")
    THIS.LOFORM.PAGEFRAME.PAGE1.DTVOID.VALUE = THIS.LOFORM.PAGEFRAME.PAGE1.DTVOID.OLDVALUE
    RETURN 0
  ENDIF

  ENDFUNC
  *--end of lfvVoid


  *!*************************************************************
  *! Name      : lfvInqInv
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To be called in the Click event of the cmdInvoice Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVINQINV

  *Open the invoice header file.
  THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'InvHdr',OARIAAPPLICATION.DATADIR+'InvHdr')

  THIS.INVHDR.SETORDER('INVHDR')
  *-- If there is valid invoice, inquire with the invoice no.
  IF THIS.INVHDR.SEEK(RETAUTH.INVOICE)
    *-- Call the invoice inquire screen.
    LCINV = '"'+RETAUTH.INVOICE+'"'
    OARIAAPPLICATION.DOPROGRAM("AWRARDINV",LCINV,.F.,'AR')
  ELSE
    *** Invoice # RetAuth.invoice is missing, Unable to inquire. ***
    *** <  Ok  > ***
    =GFMODALGEN("TRM46011B00000" , "DIALOG" , RETAUTH.INVOICE)
  ENDIF

  ENDFUNC
  *--end of lfvInqInv


  *!*************************************************************
  *! Name      : lfvInvoice
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the kbInvoice Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVINVOICE
  LOCAL KBINVOICE

  *Open the invoice header file & the consolidated invoice header file.
  THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'InvHdr',OARIAAPPLICATION.DATADIR+'InvHdr')
  THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'ConsInvH',OARIAAPPLICATION.DATADIR+'ConsInvH')

  KBINVOICE = THIS.LOFORM.PAGEFRAME.PAGE1.KBINVOICE.KEYTEXTBOX
  THIS.LLBROWSE = KBINVOICE.PARENT.SELECTEDFROMBROWSE
  IF (THIS.LLBROWSE .OR. !(KBINVOICE.VALUE == KBINVOICE.OLDVALUE))
    LCINVEXP = IIF(EMPTY(RETAUTH.STORE) , ".T." , "((INVHDR.CONSOL='Y' AND This.CONSINVH.SEEK(kbInvoice.Value+RetAuth.Store)) OR INVHDR.STORE=RetAuth.Store)")
    IF !EMPTY(KBINVOICE.VALUE) .OR. THIS.LLBROWSE
      *-- Add zeros to the right of the invoice no.
      KBINVOICE.VALUE = IIF(THIS.LLBROWSE .OR. EMPTY(KBINVOICE.VALUE)  .OR. ("?" $ KBINVOICE.VALUE) , KBINVOICE.VALUE , PADL(ALLTRIM(KBINVOICE.VALUE) , 6 , "0"))
      SELECT INVHDR
      THIS.INVHDR.SETORDER('INVHDRA')
      IF THIS.INVHDR.SEEK(RETAUTH.ACCOUNT + KBINVOICE.VALUE) .AND. &LCINVEXP .AND. !THIS.LLBROWSE
        *Do not accept any voided invoices.
        IF INVHDR.STATUS = 'V'
          *** Cannot accept voided invoices. ***
          *** < Ok > ***
          =GFMODALGEN("INM46032B00000" , "Dialog")
          KBINVOICE.VALUE  = KBINVOICE.OLDVALUE
        ENDIF
      ELSE
        *B609577,1 WAM 05/03/2011 Enhance the performace of invoice browse
        *!*	        IF This.INVHDR.SEEK(RetAuth.Account)
        *!*	          LOCATE REST WHILE Account = RetAuth.Account FOR Invhdr.status<>'V' AND ;
        *!*	                            IIF(EMPTY(RetAuth.Store),.T.,STORE=RetAuth.Store OR ;
        *!*	                            (CONSOL='Y' AND This.CONSINVH.SEEK(INVHDR.INVOICE+RetAuth.Store) ))
        *!*	        ENDIF
        *!*	        IF !FOUND()

        LCACCOUNT = RETAUTH.ACCOUNT
        LCSTORE   = RETAUTH.STORE
        LLFOUND   = .F.
        IF !EMPTY(LCSTORE)
          SELECT * FROM INVHDR WHERE ACCOUNT+STORE+INVOICE = LCACCOUNT+LCSTORE AND STATUS <> 'V' UNION ;
            (SELECT INVHDR.* FROM INVHDR INNER JOIN CONSINVH ON INVHDR.INVOICE = CONSINVH.INVOICE WHERE INVHDR.ACCOUNT = LCACCOUNT AND INVHDR.STATUS <> 'V' AND CONSINVH.STORE=LCSTORE) ;
            INTO CURSOR TMPINVOICES
          LLFOUND = _TALLY > 0
        ELSE
          IF THIS.INVHDR.SEEK(LCACCOUNT)
            LOCATE REST WHILE ACCOUNT = LCACCOUNT FOR INVHDR.STATUS <> 'V'
            LLFOUND = FOUND()
          ENDIF
        ENDIF
        IF !LLFOUND
          *B609577,1 WAM 05/03/2011 (ENd)

          *** There is no records to display. ***
          *** <  Ok  > ***
          =GFMODALGEN("INM00052B00000" , "Dialog")
          KBINVOICE.VALUE  = KBINVOICE.OLDVALUE
        ELSE
          *-- Do the soft seek to get the first available record
          *-- similar to the entered invoice.
          IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
            GO RECNO(0)
          ELSE
            GO TOP
          ENDIF
          *-- Set the browse fields.
          LCBRFIELDS = "INVOICE :H='Invoice' , STATUS :H='S' ,"+;
            "INVDATE :H='Date' , STORE :H='Store' ,"+;
            "ORDER :H='Order' ,"+;
            "lcDummy = LOOKUP(CUSTOMER.StName,'M'+RetAuth.Account,CUSTOMER.Account,'CUSTOMER'):H='Name':22 ,"+;
            "lcTrmDesc = gfCodDes(cTermCode,'CTERMCODE'):23:R:H='Terms' ,"+;
            "Rep1  :H='Rp1' , Rep2 :H='Rp2', TOTALCHG :H='Amount',"+;
            "Note1 :H='Note1' , Note2 :H='Note2'"

          LCFILE_TTL = "Invoices"
          DECLARE LAINVOICE[1]
          LAINVOICE[1] = KBINVOICE.VALUE

          *B609577,1 WAM 05/03/2011 Enhance the performace of invoice browse
          *B609577,1 WAM 05/03/2011 use the ARIABROW function and Pass the INVHDR as the master table to be able to use the task pane and the Add/Remove browse features

          *!*	          lcKey      = "RetAuth.Account FOR IIF(EMPTY(RetAuth.Store),Invhdr.status<>'V',;
          *!*	                       ((Invhdr.status<>'V' AND CONSOL='Y' AND SEEK(INVHDR.INVOICE+RetAuth.Store,'CONSINVH')) ;
          *!*	                       OR (Invhdr.status<>'V' AND STORE=RetAuth.Store)))"

          IF !EMPTY(LCSTORE)
            =ARIABROW("", 'INVOICES', .F., .F., .F., .F., .F., .F., "invoice", "laInvoice", .T., .F.,.F., 'INVHDR')
            USE IN TMPINVOICES
            SELECT INVHDR
          ELSE
            LCKEY = "RetAuth.Account FOR Invhdr.status<>'V'"
            *B609577,1 WAM 05/03/2011 (End)

            *-- Call the global browse.
            =GFBROWS(LCKEY,"invoice","laInvoice")
            *B609577,1 WAM 05/03/2011 Enhance the performace of invoice browse
          ENDIF
          *B609577,1 WAM 05/03/2011 (End)

          KBINVOICE.VALUE  = IIF(KBINVOICE.VALUE = LAINVOICE[1] , KBINVOICE.OLDVALUE , LAINVOICE[1])
        ENDIF
      ENDIF
    ENDIF

    *-- Get the default values for the current customer+invoice.
    IF THIS.INVHDR.SEEK(RETAUTH.ACCOUNT + KBINVOICE.VALUE) .AND. INVHDR.STATUS <> 'V' .AND. !EMPTY(KBINVOICE.VALUE)

      *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
      IF INVHDR.CONSOL = 'Y'
        SELECT DISTINCT ORDER FROM CONSINVH WHERE CONSINVH.INVOICE = INVHDR.INVOICE INTO ARRAY THIS.LACONSINVORDRS
      ENDIF
      *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]

      *Don't re collect invoice data if there is lines in the detail folder
      *otherwise enavle Return Entire INV. Check Box
      IF EOF(THIS.LCTMPRETLN)
        THIS.CBCOMPLETE = .F. && Private
        *-- If multi warehouse & the invoice consoloidated & exist in the
        *-- consolidated invoice header.
        IF THIS.LLMULTIWH .AND. INVHDR.CONSOL = 'Y' .AND. THIS.CONSINVH.SEEK(KBINVOICE.VALUE + RETAUTH.STORE)
          REPLACE RETAUTH.CWARECODE WITH IIF(THIS.LLMULTIWH , CONSINVH.CWARECODE , THIS.LCDEFWARE) IN RETAUTH
          *Adjust the popup returned row.
          THIS.PUWAREHOUS  = IIF(ASCAN(THIS.LAWAREHOUS,RETAUTH.CWARECODE) > 0 , ASUBSCRIPT(THIS.LAWAREHOUS , ASCAN(THIS.LAWAREHOUS,RETAUTH.CWARECODE) , 1) , 1) && Revise

          *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
          *REPLACE RetAuth.order WITH CONSINVH.ORDER,RetAuth.custpo WITH CONSINVH.CustPo, ;
          RetAuth.cartons WITH CONSINVH.Cartons, RetAuth.ccurrcode WITH INVHDR.cCurrCode, ;
          RetAuth.ncurrunit WITH INVHDR.nCurrUnit, RetAuth.nexrate WITH INVHDR.nExRate, ;
          RetAuth.cDivision WITH CONSINVH.cDivision IN RetAuth
          REPLACE RETAUTH.CUSTPO WITH CONSINVH.CUSTPO, ;
            RETAUTH.CARTONS WITH CONSINVH.CARTONS, RETAUTH.CCURRCODE WITH INVHDR.CCURRCODE, ;
            RETAUTH.NCURRUNIT WITH INVHDR.NCURRUNIT, RETAUTH.NEXRATE WITH INVHDR.NEXRATE, ;
            RETAUTH.CDIVISION WITH CONSINVH.CDIVISION IN RETAUTH
          IF !(TYPE('This.laConsInvOrdrs') <> 'U' .AND. ALEN(THIS.LACONSINVORDRS,1) >1)
            REPLACE RETAUTH.ORDER WITH CONSINVH.ORDER
          ENDIF
          *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]

          THIS.LNTAXRATE   = CONSINVH.TAX_RATE
          THIS.LNPSTRATE   = CONSINVH.NPSTRATE
          THIS.LNDISCPCNT  = CONSINVH.DISCPCNT
          THIS.LCCUSTPLVL  = IIF(LEN(ALLTRIM(THIS.LCCUSTPLVL))=0,'A',THIS.LCCUSTPLVL)
          THIS.LCCONSOL    = INVHDR.CONSOL
        ELSE
          *-- If single or multi warehouse or the invoice not consoloidated or
          *-- the invoice does not exist in the consolidated invoice header.
          REPLACE RETAUTH.CWARECODE WITH IIF(THIS.LLMULTIWH , INVHDR.CWARECODE , THIS.LCDEFWARE) IN RETAUTH
          *Adjust the popup returned row.
          THIS.PUWAREHOUS  = IIF(ASCAN(THIS.LAWAREHOUS,RETAUTH.CWARECODE) > 0 , ASUBSCRIPT(THIS.LAWAREHOUS , ASCAN(THIS.LAWAREHOUS,RETAUTH.CWARECODE) , 1) , 1) && Revise
          REPLACE RETAUTH.ORDER WITH INVHDR.ORDER, RETAUTH.CUSTPO WITH INVHDR.CUSTPO, ;
            RETAUTH.CARTONS WITH INVHDR.CARTONS, RETAUTH.CCURRCODE WITH INVHDR.CCURRCODE, ;
            RETAUTH.NCURRUNIT WITH INVHDR.NCURRUNIT, RETAUTH.NEXRATE WITH INVHDR.NEXRATE, ;
            RETAUTH.CDIVISION WITH INVHDR.CDIVISION IN RETAUTH
          THIS.LNTAXRATE   = INVHDR.TAX_RATE
          THIS.LNPSTRATE   = INVHDR.NPSTRATE
          THIS.LNDISCPCNT  = INVHDR.DISCPCNT
          THIS.LCCUSTPLVL  = IIF(LEN(ALLTRIM(THIS.LCCUSTPLVL))=0,'A',THIS.LCCUSTPLVL)
          THIS.LCCONSOL    = INVHDR.CONSOL
        ENDIF
      ELSE && Else IF EOF() Get invoice order # and CUSTPO#
        REPLACE RETAUTH.ORDER WITH INVHDR.ORDER, RETAUTH.CUSTPO WITH INVHDR.CUSTPO IN RETAUTH
      ENDIF
    ELSE
      KBINVOICE.VALUE = SPACE(6)
      REPLACE  RETAUTH.ORDER WITH SPACE(6), RETAUTH.CUSTPO WITH SPACE(15), ;
        RETAUTH.CARTONS WITH 1, RETAUTH.CCURRCODE WITH CUSTOMER.CCURRCODE, ;
        RETAUTH.NCURRUNIT WITH 0, RETAUTH.NEXRATE WITH 0 IN RETAUTH
      THIS.LNPSTRATE  = IIF(THIS.LLISCANADA , CUSTOMER.NTAXRATE , 0)
      THIS.LNTAXRATE  = IIF(!THIS.LLISCANADA , CUSTOMER.NTAXRATE , 0)
      THIS.LNDISCPCNT = 0
      THIS.LCCUSTPLVL = IIF(LEN(ALLTRIM(THIS.LCCUSTPLVL))=0,'A',THIS.LCCUSTPLVL)
      THIS.LCCONSOL   = "N"
    ENDIF
    SELECT INVHDR
    THIS.INVHDR.SETORDER('INVHDR')
  ENDIF

  *-- Update the status variables for all the objects in the main screen.
  THIS.LCOBJSTAT = IIF(THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A" , "ENABLE" , "DISABLE")
  *Enable the warehouse popup even there is invoice.
  THIS.LCWARSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. THIS.LLMULTIWH , "ENABLE" , "DISABLE")
  THIS.LCINQSTAT = IIF(!EMPTY(KBINVOICE.VALUE) .AND. !THIS.LOFORMSET.ACTIVEMODE="S" , "ENABLE" , "DISABLE")

  *Don't include lnNoOfLines when determining the invoice object status
  *as it should be always enabled in Add/Edit modes
  THIS.LCINVSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") , "ENABLE" , "DISABLE")

  *Disable the division if entered any styles.
  THIS.LCDIVSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(KBINVOICE.VALUE) .AND. THIS.LNNOOFLINES = 0 , "ENABLE" , "DISABLE")

  THIS.LCCURSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(KBINVOICE.VALUE) , "ENABLE" , "DISABLE")
  THIS.LCEXRSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(KBINVOICE.VALUE) .AND. !EMPTY(RETAUTH.CCURRCODE) .AND. THIS.LLEDITEXRT .AND. (RETAUTH.CCURRCODE <> OARIAAPPLICATION.BASECURRENCY) , "ENABLE" , "DISABLE")

  *Don't include lnNoOfLines when determining the invoice object status
  * as it should be always enabled in Add/Edit modes

  THIS.LCENTRSTAT= IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EOF(THIS.LCTMPRETLN) .AND. !EMPTY(KBINVOICE.VALUE) , "ENABLE" , "DISABLE")

  THIS.LCRELSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(KBINVOICE.VALUE) , "ENABLE" ,"DISABLE")
  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
  THIS.LCORDSTAT = IIF((THIS.LCRELSTAT == "ENABLE" .OR. (TYPE('This.laConsInvOrdrs') <> 'U' .AND. ALEN(THIS.LACONSINVORDRS,1) >1)) , "ENABLE" ,"DISABLE")
  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]


  *-- Refresh the objects in the main screen.

  WITH THIS.LOFORM.PAGEFRAME.PAGE1
    .REFRESH()
    .TXTTAXRATE.ENABLED = (THIS.LCOBJSTAT='ENABLE')
    .CMDINVOICE.ENABLED =(THIS.LCINQSTAT='ENABLE')
    .KBINVOICE.ENABLED = (THIS.LCINVSTAT='ENABLE')
    .CHKENTIREINVOICE.ENABLED = (THIS.LCENTRSTAT='ENABLE')
    *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
    *.kbOrder.Enabled = (This.lcRelStat='ENABLE')
    .KBORDER.ENABLED = (THIS.LCORDSTAT='ENABLE')
    *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]

    .TXTCUSTPO.ENABLED = (THIS.LCRELSTAT='ENABLE')
    *Enable the cartons field to let the user change it's numer to be used while printing the stickers.
    .TXTCARTONS.ENABLED = (THIS.LCINVSTAT='ENABLE')
    .CBOCWARECODE.ENABLED = (THIS.LCWARSTAT='ENABLE')
    .CBODIVISION.ENABLED = (THIS.LCDIVSTAT='ENABLE')
    .KBCCURRCODE.ENABLED = (THIS.LCCURSTAT='ENABLE')
    .TXTNEXRATE.ENABLED = (THIS.LCEXRSTAT='ENABLE')
  ENDWITH

  THIS.LLBROWSE = .F.
  SELECT RETAUTH
  THIS.LOFORM.PAGEFRAME.PAGE1.CBODIVISION.REFRESH()
  *ASM, Disable Account Code if Invoice or Order # was entered [Start]
  THIS.LCACCSTAT = IIF(THIS.LOFORMSET.ACTIVEMODE="V" OR ;
    !EMPTY(THIS.LOFORM.PAGEFRAME.PAGE1.KBINVOICE.KEYTEXTBOX.VALUE) OR ;
    !EMPTY(THIS.LOFORM.PAGEFRAME.PAGE1.KBORDER.KEYTEXTBOX.VALUE), "DISABLE" , "ENABLE")
  THIS.LOFORM.KBACCOUNT.ENABLED =  (THIS.LCACCSTAT='ENABLE')
  *ASM, Disable Account Code if Invoice or Order # was entered [End]

  RETURN 1
  ENDFUNC
  *--end of lfvInvoice


  *!*************************************************************
  *! Name      : lfvOrder
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the kbOrder Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVORDER
  LOCAL KBORDER

  *Open the Order header file.
  THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'OrdHdr',OARIAAPPLICATION.DATADIR+'OrdAcct')

  KBORDER = THIS.LOFORM.PAGEFRAME.PAGE1.KBORDER.KEYTEXTBOX
  THIS.LLBROWSE = KBORDER.PARENT.SELECTEDFROMBROWSE
  IF (THIS.LLBROWSE .OR. !(KBORDER.VALUE == KBORDER.OLDVALUE))
    KBORDER.VALUE = IIF(!THIS.LLBROWSE .AND. !EMPTY(KBORDER.VALUE) .AND. !("?" $ KBORDER.VALUE) , PADL(ALLTRIM(KBORDER.VALUE) , 6 , "0") , KBORDER.VALUE)
    SELECT ORDHDR
    THIS.ORDHDR.SETORDER('ORDACCT')
    *-- If the entered invalid order # , browse the available orders.

    *B609217,1 WAM 04/22/2010 Restrict sales orders to selected account/store
    *IF (!EMPTY(kbOrder.Value) .AND. !This.ORDHDR.SEEK(RetAuth.Account+"O"+kbOrder.Value)) .OR. This.llBrowse

    THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'ORDLINE',OARIAAPPLICATION.DATADIR+'ORDLINST')
    IF (!EMPTY(KBORDER.VALUE) .AND. (!THIS.ORDHDR.SEEK(RETAUTH.ACCOUNT+"O"+KBORDER.VALUE) OR ;
        !THIS.ORDLINE.SEEK("O"+KBORDER.VALUE+ALLTRIM(RETAUTH.STORE))) ) .OR. THIS.LLBROWSE
      *B609217,1 WAM 04/22/2010 (End)

      IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
        GO RECNO(0)
      ELSE
        GO TOP
      ENDIF
      *-- Set the browse fields.
      *B609217,1 WAM 04/22/2010 Following lines are commented out
      *!*	      lcBrFields = [Order:H="Order#",status:1:H="S",Season:H="SE",cDivision:H="DI",]+;
      *!*	                   [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
      *!*	                   [ACCOUNT:H="Acct",store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
      *!*	                   [Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",Ship:H="Ship.Qty.",Shipamt:H="Ship.Amt.",]+;
      *!*	                   [start:H="Start",Complete:H="Complete"]
      *!*	      lcFile_ttl  = "Orders"
      *!*	      DECLARE laOrders[1]
      *!*	      laOrders[1] = kbOrder.Value
      *!*	      *-- call the global browse to browse the available orders.
      *!*	      =gfBrows("RetAuth.Account" , "Order" , "laOrders")
      *!*	      kbOrder.Value   = IIF(kbOrder.Value = laOrders[1] , kbOrder.OldValue , laOrders[1])

      *B609217,1 WAM 04/22/2010 Call the global order browse
      XORDER = ''
      XSTORE  = RETAUTH.STORE

      *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
      *kbOrder.Value = IIF(ORDBROWA(RetAuth.Account ,.T.,'O'),XORDER,'')
      *_SCREEN.Visible = .T.
      *ACTIVATE WINDOW Debug
      *SUSPEND
      IF !(THIS.LCORDSTAT == "ENABLE" .AND. (TYPE('This.laConsInvOrdrs') <> 'U' .AND. ALEN(THIS.LACONSINVORDRS) >1))
        KBORDER.VALUE = IIF(ORDBROWA(RETAUTH.ACCOUNT ,.T.,'O'),XORDER,'')
      ELSE
        KBORDER.VALUE = THIS.LFORDERBROWSE(RETAUTH.ACCOUNT, XORDER)
      ENDIF
      *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]

      *B609217,1 WAM 04/22/2010 (End)

    ENDIF
    KBORDER.PARENT.REFRESH()
  ENDIF

  THIS.LLBROWSE = .F.

  SELECT RETAUTH

  *ASM, Disable Account Code if Invoice or Order # was entered [Start]
  THIS.LCACCSTAT = IIF(THIS.LOFORMSET.ACTIVEMODE="V" OR ;
    !EMPTY(THIS.LOFORM.PAGEFRAME.PAGE1.KBINVOICE.KEYTEXTBOX.VALUE) OR ;
    !EMPTY(THIS.LOFORM.PAGEFRAME.PAGE1.KBORDER.KEYTEXTBOX.VALUE), "DISABLE" , "ENABLE")
  THIS.LOFORM.KBACCOUNT.ENABLED =  (THIS.LCACCSTAT='ENABLE')
  *ASM, Disable Account Code if Invoice or Order # was entered [End]

  RETURN 1
  ENDFUNC
  *--end of lfvOrder


  *!*************************************************************
  *! Name      : lfvCurrency
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the kbCurrCode Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVCURRENCY
  LOCAL KBCURRCODE

  *-- Added condition to prevent user from editing currency
  *-- if the this RA has lines or return by invioce.
  KBCURRCODE = THIS.LOFORM.PAGEFRAME.PAGE1.KBCCURRCODE.KEYTEXTBOX
  THIS.LLBROWSE = KBCURRCODE.PARENT.SELECTEDFROMBROWSE
  IF THIS.LLBROWSE .OR. !(KBCURRCODE.VALUE == KBCURRCODE.OLDVALUE)
    LCCURCODE = KBCURRCODE.VALUE
    *-- Call global function to browse the available currencies.
    IF !GFCURRBROW(@LCCURCODE)
      KBCURRCODE.VALUE = KBCURRCODE.OLDVALUE
    ELSE
      KBCURRCODE.VALUE = LCCURCODE
    ENDIF

    IF !EMPTY(KBCURRCODE.VALUE)
      LNCURRUNIT = THIS.LNCURRUNIT
      REPLACE RETAUTH.NEXRATE WITH GFCHKRATE('lnCurrUnit' , KBCURRCODE.VALUE , OARIAAPPLICATION.SYSTEMDATE,;
        .T. , OARIAAPPLICATION.ACTIVECOMPANYID , .F. , THIS.LLEDITEXRT) IN RETAUTH
      THIS.LNCURRUNIT = LNCURRUNIT
      REPLACE RETAUTH.NCURRUNIT WITH THIS.LNCURRUNIT IN RETAUTH

      IF !(KBCURRCODE.VALUE == OARIAAPPLICATION.BASECURRENCY)
        IF RETAUTH.NEXRATE = 0 .AND. THIS.LLEDITEXRT
          *** A valid ALLTRIM(kbCurrCode.Value) to ALLTRIM(gcBaseCurr) ***
          *** exchange rate could not be found on DTOC(gdSysDate) ***
          *** <  Ok  > ***
          LCTMPTXT = ALLTRIM(KBCURRCODE.VALUE) + "|" + ALLTRIM(OARIAAPPLICATION.BASECURRENCY) + "|" + DTOC(OARIAAPPLICATION.SYSTEMDATE)
          =GFMODALGEN("INM46000B00000" , "DIALOG" , LCTMPTXT)
        ENDIF
      ELSE
        REPLACE RETAUTH.NEXRATE WITH 1, RETAUTH.NCURRUNIT WITH 1 IN RETAUTH
      ENDIF
      *-- Got exchange rate sign and unit sign.
      THIS.LCUNTSIN = ' '
      LCUNTSIN = THIS.LCUNTSIN
      THIS.LCEXRSIN = GFGETEXSIN(@LCUNTSIN, KBCURRCODE.VALUE)
      THIS.LCUNTSIN = LCUNTSIN
    ENDIF
  ELSE
    KBCURRCODE.VALUE = KBCURRCODE.OLDVALUE
  ENDIF

  THIS.LCEXRSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EMPTY(RETAUTH.INVOICE) .AND. !EMPTY(KBCURRCODE.VALUE) .AND. THIS.LLEDITEXRT .AND. (KBCURRCODE.VALUE <> OARIAAPPLICATION.BASECURRENCY) , "ENABLE" , "DISABLE")

  *-- Refresh the currency & its exchange rate objects.
  KBCURRCODE.PARENT.REFRESH()
  THIS.LOFORM.PAGEFRAME.PAGE1.TXTNEXRATE.REFRESH()

  THIS.LLBROWSE = .F.

  RETURN 1
  ENDFUNC
  *--end of lfvCurrency


  *!*************************************************************
  *! Name      : lfvExRate
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the txtnExRate Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVEXRATE

  WITH THIS.LOFORM.PAGEFRAME.PAGE1
    IF .TXTNEXRATE.OLDVALUE <>  .TXTNEXRATE.VALUE .OR. .TXTNEXRATE.VALUE <=0
      *-- The exchange rate should not be equal or less than zero.
      IF .TXTNEXRATE.VALUE <=0
        *** The exchange rate must be greater than zero. ***
        *** <  Ok  > ***
        =GFMODALGEN("INM46001B00000" , "DIALOG")
        .TXTNEXRATE.VALUE = IIF(.TXTNEXRATE.OLDVALUE > 0 , .TXTNEXRATE.OLDVALUE , .TXTNEXRATE.VALUE)
        RETURN (IIF(.TXTNEXRATE.OLDVALUE > 0 , 0 , 1))
      ELSE
        RETURN 1
      ENDIF
    ENDIF
  ENDWITH

  ENDFUNC
  *--end of lfvExRate


  *!*************************************************************
  *! Name      : lfvEntireInvoice
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To be called when checking the chkEntireInvoice Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVENTIREINVOICE
  LOCAL CHKENTIRE

  *Open the invoice line & the consolidated invoice line file.
  THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'ConsInvL',OARIAAPPLICATION.DATADIR+'ConsInvL')
  THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'InvLine',OARIAAPPLICATION.DATADIR+'InvLine')

  CHKENTIRE = THIS.LOFORM.PAGEFRAME.PAGE1.CHKENTIREINVOICE
  *IF !(chkEntire.OldValue == chkEntire.Value) && No need for this condition as it is called in
  *the InterActiveChange event
  THIS.CBCOMPLETE = CHKENTIRE.VALUE
  IF .T.
    REPLACE RETAUTH.CINTR_INV WITH IIF(THIS.CBCOMPLETE , 'Y' , 'N') IN RETAUTH
    DO CASE
      *-- If the Returne entire invoice check box was checked.
    CASE CHKENTIRE.VALUE
      STORE 0 TO THIS.LNNOOFLINES , _TALLY
      REPLACE RETAUTH.NRETA_BUD WITH 0, RETAUTH.NRETA_OPN WITH 0, ;
        RETAUTH.NRTOPNAMT WITH 0 IN RETAUTH
      IF THIS.LLMULTIWH .AND. THIS.LCCONSOL = 'Y'
        THIS.LNRA_LINNO = 1

        SELECT CONSINVL
        *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
        **B999999,1 ASM 06/13/2005 Read Lines For Consolidated Invoice even when No Store was Specified [Start]
        **This.Consinvl.SEEK(RetAuth.invoice+RetAuth.Store)
        **SCAN REST WHILE invoice + store = RetAuth.invoice + RetAuth.Store
        *This.Consinvl.SEEK(RetAuth.invoice+IIF(EMPTY(RetAuth.Store),'',RetAuth.Store))
        *SCAN REST WHILE invoice + store = RetAuth.invoice + IIF(EMPTY(RetAuth.Store),'',RetAuth.Store)
        **B999999,1 ASM 06/13/2005 Read Lines For Consolidated Invoice even when No Store was Specified [End]
        *          This.Consinvl.SEEK(RetAuth.invoice+IIF(EMPTY(RetAuth.Store),SPACE(8),RetAuth.Store)+IIF(EMPTY(RetAuth.Order),'',RetAuth.Order))
        *          SCAN REST WHILE invoice + store + order = RetAuth.invoice + IIF(EMPTY(RetAuth.Store),SPACE(8),RetAuth.Store) + IIF(EMPTY(RetAuth.Order),'',RetAuth.Order)
        LOCAL LCORDERFILTER
        LCORDERFILTER = IIF(EMPTY(RETAUTH.ORDER),"","FOR Order='" +  RETAUTH.ORDER + "'")

        THIS.CONSINVL.SEEK(RETAUTH.INVOICE+IIF(EMPTY(RETAUTH.STORE),'',RETAUTH.STORE))
        SCAN REST WHILE INVOICE + STORE = RETAUTH.INVOICE + IIF(EMPTY(RETAUTH.STORE),'',RETAUTH.STORE) &LCORDERFILTER.
          *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]
          SELECT (THIS.LCTMPRETLN)
          APPEND BLANK
          *include discount percentage when calculating the price and total amount
          REPLACE ACCOUNT    WITH CONSINVL.ACCOUNT ;
            STYLE      WITH CONSINVL.STYLE ;
            CRA_LINNO  WITH ALLTRIM(STR(THIS.LNRA_LINNO)) ;
            DYELOT     WITH CONSINVL.DYELOT ;
            REASON     WITH RETAUTH.REASON ;
            PRICE      WITH CONSINVL.PRICE * (1 - THIS.LNDISCPCNT / 100) ;
            QTY1       WITH CONSINVL.QTY1 ;
            QTY2       WITH CONSINVL.QTY2 ;
            QTY3       WITH CONSINVL.QTY3 ;
            QTY4       WITH CONSINVL.QTY4 ;
            QTY5       WITH CONSINVL.QTY5 ;
            QTY6       WITH CONSINVL.QTY6 ;
            QTY7       WITH CONSINVL.QTY7 ;
            QTY8       WITH CONSINVL.QTY8 ;
            TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
            NOPNQTY1   WITH CONSINVL.QTY1 ;
            NOPNQTY2   WITH CONSINVL.QTY2 ;
            NOPNQTY3   WITH CONSINVL.QTY3 ;
            NOPNQTY4   WITH CONSINVL.QTY4 ;
            NOPNQTY5   WITH CONSINVL.QTY5 ;
            NOPNQTY6   WITH CONSINVL.QTY6 ;
            NOPNQTY7   WITH CONSINVL.QTY7 ;
            NOPNQTY8   WITH CONSINVL.QTY8 ;
            NTOTOPNQTY WITH NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8 ;
            TAX_RATE   WITH IIF(THIS.LLTAXYES , THIS.LNTAXRATE , 0) ;
            NPSTRATE   WITH IIF(THIS.LLTAXYES .AND. THIS.LLISCANADA , THIS.LNPSTRATE , 0) ;
            AMOUNT     WITH CONSINVL.PRICE * (1 - THIS.LNDISCPCNT / 100) * CONSINVL.TOTQTY ;
            CSTATUS    WITH "A"
            
          *B610460,1 HIA 07/08/2013 T20130731.0014 - Olsen Return Authorization Issue : not updating and not saving [Begin].
          IF THIS.LOFORMSET.ACTIVEMODE = "E"
            REPLACE RANO WITH RETAUTH.RANO
          ENDIF   
          *B610460,1 HIA 07/08/2013 T20130731.0014 - Olsen Return Authorization Issue : not updating and not saving [End].

          *Add true in the logical field to know that this line was
          *shipped to the customer befor.
          REPLACE LSHIPPED  WITH .T.

          *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
          IF !USED('OrdLine')
            =GFOPENTABLE('OrdLine','ORDLINST','SH', 'Ordline')    && CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
          ENDIF
          IF GFSEEK('O'+CONSINVL.ORDER+CONSINVL.STORE+CONSINVL.STYLE,'Ordline') AND !EMPTY(ORDLINE.EMPLOYEE)
            REPLACE EMPLOYEE WITH ORDLINE.EMPLOYEE
          ENDIF
          *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]

          *-- Call global function to add audit fields info.
          =GFADD_INFO(THIS.LCTMPRETLN)

          *-- Total Budget.
          REPLACE RETAUTH.NRETA_BUD WITH RETAUTH.NRETA_BUD + EVALUATE(THIS.LCTMPRETLN+'.TotQty'), ;
            RETAUTH.NRETA_OPN WITH RETAUTH.NRETA_OPN + EVALUATE(THIS.LCTMPRETLN+'.nTotOpnQty'), ;
            RETAUTH.NRTOPNAMT WITH RETAUTH.NRTOPNAMT + (EVALUATE(THIS.LCTMPRETLN+'.nTotOpnQty') * CONSINVL.PRICE) IN RETAUTH

          THIS.LNRA_LINNO = THIS.LNRA_LINNO + 1
          SELECT CONSINVL
        ENDSCAN

        *include discount percentage & taxes & charges when calculating the total amount
        LLISENGLND = IIF(UPPER(ALLTRIM(OARIAAPPLICATION.DEFAULTCOUNTRY)) = 'ENG', .T., .F.)
        IF LLISENGLND
          LNOTHER = INVHDR.NCHARGES
        ELSE
          LNOTHER = INVHDR.FREIGHT + INVHDR.COD + INVHDR.INSUR
        ENDIF

        LNDISCAMT = RETAUTH.NRTOPNAMT * (THIS.LNDISCPCNT / 100)

        THIS.LNTAXZAMT = (RETAUTH.NRTOPNAMT - LNDISCAMT + LNOTHER ) * (THIS.LNTAXRATE / 100)
        REPLACE RETAUTH.NRTOPNAMT WITH ;
          RETAUTH.NRTOPNAMT + LNOTHER + THIS.LNTAXZAMT - LNDISCAMT IN RETAUTH

        THIS.LNNOOFLINES = THIS.LNRA_LINNO
        SELECT (THIS.LCTMPRETLN)
        GO TOP
      ELSE
        THIS.LNRA_LINNO = 1

        SELECT INVLINE
        THIS.INVLINE.SEEK(RETAUTH.INVOICE)
        SCAN REST WHILE INVOICE = RETAUTH.INVOICE
          SELECT (THIS.LCTMPRETLN)
          APPEND BLANK

          *include discount percentage when calculating the price and total amount
          REPLACE ACCOUNT    WITH INVLINE.ACCOUNT ;
            STYLE      WITH INVLINE.STYLE ;
            CRA_LINNO  WITH ALLTRIM(STR(THIS.LNRA_LINNO)) ;
            DYELOT     WITH INVLINE.DYELOT ;
            REASON     WITH RETAUTH.REASON ;
            PRICE      WITH INVLINE.PRICE  * (1 - THIS.LNDISCPCNT / 100) ;
            QTY1       WITH INVLINE.QTY1 ;
            QTY2       WITH INVLINE.QTY2 ;
            QTY3       WITH INVLINE.QTY3 ;
            QTY4       WITH INVLINE.QTY4 ;
            QTY5       WITH INVLINE.QTY5 ;
            QTY6       WITH INVLINE.QTY6 ;
            QTY7       WITH INVLINE.QTY7 ;
            QTY8       WITH INVLINE.QTY8 ;
            TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
            NOPNQTY1   WITH INVLINE.QTY1 ;
            NOPNQTY2   WITH INVLINE.QTY2 ;
            NOPNQTY3   WITH INVLINE.QTY3 ;
            NOPNQTY4   WITH INVLINE.QTY4 ;
            NOPNQTY5   WITH INVLINE.QTY5 ;
            NOPNQTY6   WITH INVLINE.QTY6 ;
            NOPNQTY7   WITH INVLINE.QTY7 ;
            NOPNQTY8   WITH INVLINE.QTY8 ;
            NTOTOPNQTY WITH NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8 ;
            TAX_RATE   WITH IIF(THIS.LLTAXYES , THIS.LNTAXRATE , 0) ;
            NPSTRATE   WITH IIF(THIS.LLTAXYES .AND. THIS.LLISCANADA , THIS.LNPSTRATE , 0) ;
            AMOUNT     WITH (INVLINE.PRICE * (1 - THIS.LNDISCPCNT / 100)) * INVLINE.TOTQTY ;
            CSTATUS    WITH "A"

          *B610460,1 HIA 07/08/2013 T20130731.0014 - Olsen Return Authorization Issue : not updating and not saving [Begin].
          IF THIS.LOFORMSET.ACTIVEMODE = "E"
            REPLACE RANO WITH RETAUTH.RANO
          ENDIF   
          *B610460,1 HIA 07/08/2013 T20130731.0014 - Olsen Return Authorization Issue : not updating and not saving [End].

          *Add true in the logical field to know that this line was
          *shipped to the customer befor.
          REPLACE LSHIPPED  WITH .T.

          *! B609603,1 SAB 09/26/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
          IF !USED('OrdLine')
            =GFOPENTABLE('OrdLine','ORDLINST','SH', 'Ordline')    && CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
          ENDIF
          IF GFSEEK('O'+INVLINE.ORDER+INVLINE.STORE+INVLINE.STYLE,'Ordline') AND !EMPTY(ORDLINE.EMPLOYEE)
            REPLACE EMPLOYEE WITH ORDLINE.EMPLOYEE
          ENDIF
          *! B609603,1 SAB 09/26/2011 Consolidated Invoice Case not checking order and not loading employee info[End]

          *-- Call global function to add audit fields info.
          =GFADD_INFO(THIS.LCTMPRETLN)
          *-- Total Budget.

          REPLACE RETAUTH.NRETA_BUD WITH RETAUTH.NRETA_BUD + EVALUATE(THIS.LCTMPRETLN+'.TotQty'), ;
            RETAUTH.NRETA_OPN WITH RETAUTH.NRETA_OPN + EVALUATE(THIS.LCTMPRETLN+'.nTotOpnQty'), ;
            RETAUTH.NRTOPNAMT WITH RETAUTH.NRTOPNAMT + (EVALUATE(THIS.LCTMPRETLN+'.nTotOpnQty') * INVLINE.PRICE) IN RETAUTH



          THIS.LNRA_LINNO = THIS.LNRA_LINNO + 1
          SELECT INVLINE
        ENDSCAN
        *include discount percentage & taxes & charges when calculating the total amount

        LLISENGLND = IIF(UPPER(ALLTRIM(OARIAAPPLICATION.DEFAULTCOUNTRY)) = 'ENG', .T., .F.)
        IF LLISENGLND
          LNOTHER = INVHDR.NCHARGES
        ELSE
          LNOTHER = INVHDR.FREIGHT + INVHDR.COD + INVHDR.INSUR
        ENDIF

        LNDISCAMT = RETAUTH.NRTOPNAMT * (THIS.LNDISCPCNT / 100)

        THIS.LNTAXZAMT = (RETAUTH.NRTOPNAMT - LNDISCAMT + LNOTHER ) * (THIS.LNTAXRATE / 100)
        REPLACE RETAUTH.NRTOPNAMT WITH ;
          RETAUTH.NRTOPNAMT + LNOTHER + THIS.LNTAXZAMT - LNDISCAMT IN RETAUTH

        THIS.LNNOOFLINES = THIS.LNRA_LINNO
        SELECT (THIS.LCTMPRETLN)
        GO TOP
      ENDIF
      *-- If the Returne entire invoice check box was unchecked.
    CASE !CHKENTIRE.VALUE
      *-- Blank the temp. R/A lines file.
      SELECT (THIS.LCTMPRETLN)
      ZAP
      *-- Set the lines counter to zero.
      THIS.LNNOOFLINES = 0
    ENDCASE

    *-- Update the status variable to refresh the objects.
    *Don't include lnNoOfLines when determining the invoice object status
    *as it should be always enabled in Add/Edit modes
    THIS.LCENTRSTAT= IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EOF(THIS.LCTMPRETLN) .AND. !EMPTY(RETAUTH.INVOICE) , "ENABLE" , "DISABLE")

    CHKENTIRE.ENABLED = (THIS.LCENTRSTAT='ENABLE')
    *Don't include lnNoOfLines when determining the invoice object status
    *as it should be always enabled in Add/Edit modes
    THIS.LCINVSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A"), "ENABLE" , "DISABLE")

    WITH THIS.LOFORM.PAGEFRAME.PAGE1
      *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
      *.kbinvoice.Enabled = (This.lcInvStat='ENABLE')
      .KBINVOICE.ENABLED  = (THIS.LCINVSTAT='ENABLE') .AND. !THIS.LOFORM.PAGEFRAME.PAGE1.CHKENTIREINVOICE.VALUE
      .KBORDER.ENABLED    = (THIS.LCORDSTAT='ENABLE') .AND. !THIS.LOFORM.PAGEFRAME.PAGE1.CHKENTIREINVOICE.VALUE
      .CMDINVOICE.ENABLED = (THIS.LCINQSTAT='ENABLE') .AND. !THIS.LOFORM.PAGEFRAME.PAGE1.CHKENTIREINVOICE.VALUE
      *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]
      .REFRESH()
    ENDWITH

  ENDIF

  SELECT RETAUTH

  ENDFUNC
  *--end of lfvEntireInvoice


  *!*************************************************************
  *! Name      : lfvTaxRate
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the txtTaxRate Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVTAXRATE
  PRIVATE LNCURRECNO

  SELECT (THIS.LCTMPRETLN) && Private
  LNCURRECNO = RECNO()

  *-- Update the tax rate in the R/A lines temp. files.
  REPLACE ALL TAX_RATE WITH THIS.LNTAXRATE, CSTATUS  WITH IIF(CSTATUS = 'S','M',CSTATUS)
  *-- Restore the lines pointer.
  IF LNCURRECNO > 0 .AND. LNCURRECNO <= RECCOUNT()
    GOTO LNCURRECNO
  ENDIF

  THIS.LOFORM.PAGEFRAME.PAGE1.TXTTAXRATE.REFRESH()

  SELECT RETAUTH

  ENDFUNC
  *--end of lfvTaxRate


  *!*************************************************************
  *! Name      : lfvReson1
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the cboReason Control in the Header Page
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVRESON1


  *If not entered the detail folder, appy header reason to all lines reason
  IF !THIS.LLENTERDET AND THIS.LNNOOFLINES > 0
    SELECT (THIS.LCTMPRETLN)
    REPLACE ALL REASON  WITH RETAUTH.REASON, CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS)
    *-- Select the main file.
    SELECT RETAUTH
  ENDIF

  RETURN 1
  ENDFUNC
  *--end of lfvReson1


  *!*************************************************************
  *! Name      : lfBrLine
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Format the Grid Lines
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFBRLINE
  LOCAL LNCOL

  *-- Prepare the alias, key, browse title for the current browse.
  THIS.LCRCURALIS = IIF(THIS.LOFORMSET.ACTIVEMODE='V' , 'RALINE'  , THIS.LCTMPRETLN)
  THIS.LCKEYRET   = IIF(THIS.LOFORMSET.ACTIVEMODE='V' , "KEY RetAuth.RANO" , "")
  * Read the RALINE records in a Temprory File even in the View Mode
  THIS.LCRCURALIS = THIS.LCTMPRETLN

  SELECT (THIS.LCRCURALIS)

  WITH THIS.LOFORM.PAGEFRAME.PAGE2.GRDRALINES

    .RECORDSOURCE=''
    .COLUMNCOUNT=0
    .RECORDSOURCE = THIS.LCRCURALIS
    .COLUMNCOUNT=6
    LNCOL = 0
    THIS.LFADDGRIDCOL(@LNCOL,"Style","Style",,150)
    THIS.LFADDGRIDCOL(@LNCOL,"IIF(ThisFormSet.Prg.Style.Seek(Style),Style.Desc,'')","Description",,150)
    THIS.LFADDGRIDCOL(@LNCOL,"gfCodDes(Reason,'REASON')",'Reason',,100)
    *B608520,1 WAM 04/17/2008 Remove decimal points from RA quantity
    *This.lfAddGridCol(@lnCol,"TotQty","Qty",,50)
    THIS.LFADDGRIDCOL(@LNCOL,"TotQty","Qty",,50,'99999999')
    *B608520,1 WAM 04/17/2008 (End)

    THIS.LFADDGRIDCOL(@LNCOL,"Price","Price",,75)
    THIS.LFADDGRIDCOL(@LNCOL,"Amount","Total",,75)

    *! C201291,1 MMT 11/25/2010 Customization for OSP00 to add PO# per RA line[Start]
    IF ASCAN(THIS.LOFORM.PARENT.LAEVNTTRIG, PADR('ADDPOCOL',10)) <> 0
      =THIS.LOFORM.PARENT.MDOTRIGGER(PADR('ADDPOCOL',10))
    ENDIF
    *! C201291,1 MMT 11/25/2010 Customization for OSP00 to add PO# per RA line[End]

    THIS.LFWHENBROW()

  ENDWITH

  ENDFUNC
  *--end of lfBrLine


  *!*************************************************************
  *! Name      : lfAddGridCol
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Add a Column to the Grid
  *!*************************************************************
  *! Parameters: Column No, Control Source, Caption, Header Alignment, Width, InputMask
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFADDGRIDCOL
  LPARAMETERS LNCOL, LCCONTROLSOURCE, LCCAPTION, LNHDRALIGMENT, LNWIDTH, LCINPUTMASK

  LNCOL = LNCOL +1
  WITH THIS.LOFORM.PAGEFRAME.PAGE2.GRDRALINES
    .COLUMNS(LNCOL).CONTROLSOURCE  = LCCONTROLSOURCE
    .COLUMNS(LNCOL).HEADER1.CAPTION = LCCAPTION
    .COLUMNS(LNCOL).HEADER1.ALIGNMENT = IIF(TYPE('lnHdrAligment')='N',LNHDRALIGMENT,0)
    .COLUMNS(LNCOL).WIDTH = IIF(TYPE('lnWidth')='N',LNWIDTH,75)
    IF TYPE('lcInputMask')='C'
      .COLUMNS(LNCOL).INPUTMASK = LCINPUTMASK
    ENDIF
  ENDWITH

  ENDFUNC
  *--end of lfAddGridCol

  *!*************************************************************
  *! Name      : lfWhenBrow
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To be called from the AfterRowColChange Event of the Grid
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFWHENBROW

  *-- Refresh the browse pointer.
  SELECT (THIS.LCRCURALIS)
  THIS.LNMARKER   = RECNO(THIS.LCRCURALIS)

  *-- If there is no lines in the browse.
  IF BOF() .OR. EOF()
    *-- Store zero to qty. array, & blank its headers.
    THIS.LAQTYSTK = 0
    THIS.LASIZE   = ""

    *-- Set the status variables to disable.
    THIS.LCLINSTAT = "DISABLE"
    THIS.LCDYESTAT = "DISABLE"

    *-- Set the needed variables.
    THIS.LCSTYLE    = ""
    THIS.LCRA_LINNO = ""
    THIS.LCDESC     = ""
    THIS.LCREASON   = ""
    THIS.LCDYELOT   = ""
    THIS.LNTOTQTY   = 0
    THIS.LNPRICE    = 0
    THIS.LNTOTAL    = 0
    THIS.LCSCALE    = ""
    THIS.LNSCALE    = 0
    THIS.LOFORM.PAGEFRAME.PAGE2.CNTSIZES.SCALE = THIS.LCSCALE
    *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[Start]
    THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.VALUE = SPACE(12)
    *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[End]


    *-- Refresh the scale sizes & qty.
    FOR LNCOUNT = 1 TO 8
      *SHOW GET This.laSize  [lnCount] DISABLE && Revise
      *SHOW GET This.laQtyStk[lnCount] DISABLE && Revise
    ENDFOR
  ELSE
    THIS.LCSTYLE = EVALUATE(THIS.LCTMPRETLN+'.Style')
    *-- Update the status variables (Enable -or- Disable)
    THIS.LCLINSTAT = IIF(THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A" , "ENABLE" , "DISABLE")
    THIS.LCDYESTAT = IIF(THIS.LLDYELOT .AND. THIS.STYLE.SEEK(PADR(THIS.LCSTYLE,19)) .AND. ;
      STYLE.CDYE_FLG = 'Y' , "ENABLE" , "DISABLE")

    *-- Set the needed variables.
    THIS.LCSTYLE    = STYLE
    THIS.LCRA_LINNO = CRA_LINNO
    THIS.LCDESC     = IIF(THIS.STYLE.SEEK(STYLE),STYLE.DESC,"")
    THIS.LCREASON  = REASON
    THIS.LCDYELOT  = DYELOT
    THIS.LNTOTQTY  = TOTQTY
    THIS.LNPRICE  = PRICE
    THIS.LNTOTAL   = AMOUNT
    THIS.LCSCALE   = IIF(!EMPTY(THIS.LCSTYLE) .AND. THIS.STYLE.SEEK(PADR(THIS.LCSTYLE,19)) , STYLE.SCALE , "")
    THIS.LNSCALE   = IIF(!EMPTY(THIS.LCSCALE) .AND. THIS.SCALE.SEEK("S" + THIS.LCSCALE) , SCALE.CNT , 0)
    THIS.LOFORM.PAGEFRAME.PAGE2.CNTSIZES.SCALE = THIS.LCSCALE
    *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[Start]
    THIS.LOFORM.PAGEFRAME.PAGE2.CBOEMPL.VALUE = EMPLOYEE
    *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[End]

    *-- Refresh the scale sizes & qty.
    FOR LNCOUNT = 1 TO 8
      LCCOUNT   = STR(LNCOUNT,1)
      IF LNCOUNT <= THIS.LNSCALE
        THIS.LAQTYSTK[lnCount] = QTY&LCCOUNT
        THIS.LASIZE  [lnCount] = IIF(THIS.LNSCALE = 0 , "" , SCALE.SZ&LCCOUNT)
        *SHOW GET This.laSize  [lnCount] &This.lcLinStat && Revise
        *SHOW GET This.laQtyStk[lnCount] &This.lcLinStat && Revise
      ELSE
        THIS.LAQTYSTK[lnCount] = 0
        THIS.LASIZE  [lnCount] = ""
        *SHOW GET This.laSize  [lnCount] DISABLE && Revise
        *SHOW GET This.laQtyStk[lnCount] DISABLE && Revise
      ENDIF

      *In case modifying the style, and the scale of the new style has sizese
      *less than the old one, recalculate qtys and amount.
      IF !THIS.LOFORMSET.ACTIVEMODE="V"
        *ASM, No need for replacing fields with memory variables
        *REPLACE QTY&lcCount     WITH This.laQtyStk[lnCount] ;
        Totqty          WITH This.laQtyStk[1]+This.laQtyStk[2]+This.laQtyStk[3]+This.laQtyStk[4]+This.laQtyStk[5]+This.laQtyStk[6]+This.laQtyStk[7]+This.laQtyStk[8] ;
        Amount          WITH TOTQTY * PRICE ;
        nOpnQty&lcCount WITH IIF(lnCount <= This.lnScale,nOpnQty&lcCount,0);
        nTotOpnQty      WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8 ;
        CSTATUS         WITH IIF(CSTATUS = 'S','M',CSTATUS)
        REPLACE TOTQTY          WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
          AMOUNT          WITH TOTQTY * PRICE ;
          NOPNQTY&LCCOUNT WITH IIF(LNCOUNT <= THIS.LNSCALE,NOPNQTY&LCCOUNT,0);
          NTOTOPNQTY      WITH NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8 ;
          CSTATUS         WITH IIF(CSTATUS = 'S','M',CSTATUS)
      ENDIF
    ENDFOR
  ENDIF
  *-- Set the new button status.
  THIS.LCNEWSTAT = IIF(THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A" , "ENABLE", "DISABLE")

  *-- Refresh the objects in screen
  WITH THIS.LOFORM.PAGEFRAME.PAGE2
    STORE (THIS.LCLINSTAT='ENABLE') TO .CNTSTYLE.ENABLED, .CBOREASON.ENABLED, ;
      .TXTPRICE.ENABLED, .CNTSIZES.ENABLED, .CMDREMOVE.ENABLED
    .KBDYELOT.ENABLED = (THIS.LCLINSTAT='ENABLE' AND THIS.LCDYESTAT='ENABLE')
    .CMDNEW.ENABLED = (THIS.LCNEWSTAT='ENABLE')
    *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[Start]
    STORE (THIS.LCLINSTAT='ENABLE') TO .CBOEMPL.ENABLED
    *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[ENd]

    .CNTSTYLE.REFRESH()
    .TXTSTYDESC.REFRESH()
    .CBOREASON.REFRESH()
    .TXTPRICE.REFRESH()
    .TXTTOTQTY.REFRESH()
    .TXTTOTAL.REFRESH()
    .CMDREMOVE.REFRESH()
    .KBDYELOT.REFRESH()
    .CMDNEW.REFRESH()
    .CNTSIZES.REFRESH()

  ENDWITH
  
  *! E303662,2 MMT 05/26/2016 Add trigger to read style price from CSTPRICE table while adding lines[T20160324.0006][Start]
  IF ASCAN(THIS.LOFORM.PARENT.LAEVNTTRIG, PADR('REFFSIZ',10)) <> 0
    =THIS.LOFORM.PARENT.MDOTRIGGER(PADR('REFFSIZ',10))
  ENDIF
  *! E303662,2 MMT 05/26/2016 Add trigger to read style price from CSTPRICE table while adding lines[T20160324.0006][End]

  ENDFUNC
  *--end of lfWhenBrow


  *!*************************************************************
  *! Name      : lfvNewLine
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To be called from the Click Event of the cmdNew Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVNEWLINE

  LLADDLINE = .T.

  *-- Fill the detail reason popup with the right value.
  THIS.LCREASON = RETAUTH.REASON && Private  Global



  *-- Add new line in the temp. R/A lines file.
  SELECT (THIS.LCTMPRETLN)
  LOCATE FOR EMPTY(STYLE)
  IF !FOUND()
    APPEND BLANK
  ENDIF
  BLANK
  REPLACE RANO    WITH IIF(THIS.LOFORMSET.ACTIVEMODE="E" , RETAUTH.RANO , SPACE(6)) ;
    ACCOUNT WITH RETAUTH.ACCOUNT ;
    REASON  WITH THIS.LCREASON ;
    CSTATUS WITH 'A'

  *-- Call global function to add audit fields info.
  =GFADD_INFO(THIS.LCTMPRETLN)

  *-- Add 1 to the lines records counter.
  THIS.LNNOOFLINES = THIS.LNNOOFLINES + 1

  *-- Refresh the detail folder.
  THIS.LOFORM.PAGEFRAME.PAGE2.GRDRALINES.REFRESH()
  THIS.LFWHENBROW()
  THIS.LOFORM.PAGEFRAME.PAGE2.CNTSTYLE.SETFOCUS()


  ENDFUNC
  *--end of lfvNewLine


  *!*************************************************************
  *! Name      : lfvRemLine
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To be called from the Click Event of the cmdRemove Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVREMLINE

  *-- Confirm Removing current line.
  *** Are you sure you want to "Remove" this record? ***
  *** < Yes > - < No > ***

  *B608234,1 WAM 08/23/2007 Fix message
  *IF gfModalGen("QRM00002B00006","ALERT") = 1
  IF GFMODALGEN("QRM00002B00006","ALERT","remove") = 1
    *B608234,1 WAM 08/23/2007 (End)


    *-- Decrease the lines with 1.
    THIS.LNNOOFLINES = THIS.LNNOOFLINES - 1

    *-- Get the original Qty. & open qty. from the RALINE file if not new record.
    IF EVALUATE(THIS.LCTMPRETLN+'.cStatus') = "A"
      LNDEDUCT   = 0
    ELSE
      IF THIS.RALINE.SEEK(RETAUTH.RANO + PADR(EVALUATE(THIS.LCTMPRETLN+'.Style'),19))
        LNOLDRAOPN = RALINE.NTOTOPNQTY
        LNCURRAOPN = MAX(0 , EVALUATE(THIS.LCTMPRETLN+'.nTotOpnQty') + EVALUATE(THIS.LCTMPRETLN+'.TotQty') - RALINE.TOTQTY)
        LNDEDUCT   = IIF(EVALUATE(THIS.LCTMPRETLN+'.cStatus') = "S" , LNOLDRAOPN , ;
          IIF(LNCURRAOPN < LNOLDRAOPN , ;
          LNOLDRAOPN - LNCURRAOPN , 0))
      ELSE
        LNDEDUCT   = 0
      ENDIF
    ENDIF

    *-- Total Canceled Qty.
    REPLACE RETAUTH.NRETA_CAN WITH RETAUTH.NRETA_CAN + LNDEDUCT, ;
      RETAUTH.NRETA_OPN WITH RETAUTH.NRETA_OPN - EVALUATE(THIS.LCTMPRETLN+'.TotQty'), ;
      RETAUTH.NRETA_BUD WITH RETAUTH.NRETA_CAN + RETAUTH.NRETA_OPN, ;
      RETAUTH.NRTOPNAMT WITH RETAUTH.NRTOPNAMT - (EVALUATE(THIS.LCTMPRETLN+'.TotQty') * EVALUATE(THIS.LCTMPRETLN+'.Price')) IN RETAUTH

    *-- Delete the current line.
    SELECT (THIS.LCTMPRETLN)
    REPLACE CSTATUS WITH SUBSTR('DDS',AT(CSTATUS,'SMA'),1)
    DELETE
    *B608234,1 WAM 08/23/2007 Do not go top after delete line
    *GO TOP
    *B608234,1 WAM 08/23/2007 (End)

    *-- If there is no lines in the R/A temp. lines file.
    IF THIS.LNNOOFLINES = 0 && Private

      *Don't include lnNoOfLines when determining the invoice object status
      *as it should be always enabled in Add/Edit modes
      THIS.LCENTRSTAT= IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EOF(THIS.LCTMPRETLN) .AND. !EMPTY(RETAUTH.INVOICE) , "ENABLE" , "DISABLE")
      THIS.LFREFHDR()
    ENDIF
    *B608234,1 WAM 08/23/2007 Do not go top after delete line
    *This.loForm.PageFrame.Page2.grdRALines.Refresh()
    THIS.LOFORM.PAGEFRAME.PAGE2.GRDRALINES.SETFOCUS
    *B608234,1 WAM 08/23/2007 (End)

    THIS.LFWHENBROW()

  ENDIF

  ENDFUNC
  *--end of lfvRemLine

  *!*************************************************************
  *! Name      : lfvSizesEntry
  *! Developer : Khalid Mohi El-Din Mohamed
  *! Date      : 02/06/2006
  *! Purpose   : Before valid style check if extended size scale.
  *!*************************************************************
  *! Parameters:
  *!*************************************************************
  *! Return    :
  *!*************************************************************
  *  N038891,1 KHM 01/26/2006
  *!*************************************************************
  FUNCTION LFVSIZESENTRY
  LPARAMETERS LCSTYLEV
  PRIVATE LLRETURNVALUE
  LLRETURNVALUE = .F.
  *-- If the style division different from the header division, reenter the style.
  IF STYLE.CDIVISION <> RETAUTH.CDIVISION
    *** Style / Division confilct! ***
    *** <  Ok  > ***
    =GFMODALGEN("INM46003B00000" , "DIALOG")
    THIS.LFSTYCAN()
    RETURN .F.
  ENDIF
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[START]
  *DO FORM (oAriaApplication.ScreenHome+"RM\RMSSCL") WITH This,lcStyleV TO llReturnValue
  LOCALLINGFORM = THIS
  LCSTYLEVALUE = LCSTYLEV
  =GFCALLFORM('RMSSCL','RM',"loCallingForm ,lcStyleValue","llReturnValue")
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
  IF LLRETURNVALUE
    *-- Fill the detail reason popup with the right value.
    THIS.LCREASON = RETAUTH.REASON && Private  Global

    LCTEMPFILE = THIS.CEXTENDEDSIZESCALETEMPFILE


    SELECT (LCTEMPFILE)
    SCAN FOR TOTQTY > 0
      SELECT (THIS.LCTMPRETLN)
      LOCATE FOR EMPTY(STYLE)
      IF !FOUND()
        APPEND BLANK
      ENDIF
      REPLACE RANO    WITH IIF(THIS.LOFORMSET.ACTIVEMODE="E" , RETAUTH.RANO , SPACE(6)) ;
        ACCOUNT WITH RETAUTH.ACCOUNT ;
        REASON  WITH THIS.LCREASON ;
        CSTATUS WITH 'A';
        STYLE   WITH EVALUATE(LCTEMPFILE+'.Style')

      THIS.LFVSTYLE(.F.,0,STYLE, ' ', STYLE)

      SELECT (THIS.LCTMPRETLN)
      REPLACE PRICE    WITH EVALUATE(LCTEMPFILE+'.Price') ;
        QTY1     WITH EVALUATE(LCTEMPFILE+'.Qty1') ;
        QTY2     WITH EVALUATE(LCTEMPFILE+'.Qty2') ;
        QTY3     WITH EVALUATE(LCTEMPFILE+'.Qty3') ;
        QTY4     WITH EVALUATE(LCTEMPFILE+'.Qty4') ;
        QTY5     WITH EVALUATE(LCTEMPFILE+'.Qty5') ;
        QTY6     WITH EVALUATE(LCTEMPFILE+'.Qty6') ;
        QTY7     WITH EVALUATE(LCTEMPFILE+'.Qty7') ;
        QTY8     WITH EVALUATE(LCTEMPFILE+'.Qty8') ;
        TOTQTY   WITH EVALUATE(LCTEMPFILE+'.TotQty') ;
        REASON   WITH RETAUTH.REASON ;
        TAX_RATE WITH IIF(THIS.LLTAXYES , THIS.LNTAXRATE , 0) ;
        NPSTRATE WITH IIF(THIS.LLTAXYES .AND. THIS.LLISCANADA , THIS.LNPSTRATE , 0) ;
        DYELOT   WITH THIS.LCDYELOT ;
        CSTATUS  WITH IIF(CSTATUS = 'S','M',CSTATUS)

      *-- Call global function to add audit fields info.
      =GFADD_INFO(THIS.LCTMPRETLN)

      *-- Add 1 to the lines records counter.
      THIS.LNNOOFLINES = THIS.LNNOOFLINES + 1

      REPLACE RETAUTH.NRETA_OPN WITH RETAUTH.NRETA_OPN + EVALUATE(THIS.LCTMPRETLN+'.TotQty'), ;
        RETAUTH.NRTOPNAMT WITH RETAUTH.NRTOPNAMT + (EVALUATE(THIS.LCTMPRETLN+'.TotQty') * EVALUATE(THIS.LCTMPRETLN+'.Price')),;
        RETAUTH.NRETA_BUD WITH RETAUTH.NRETA_CAN + RETAUTH.NRETA_OPN IN RETAUTH
      THIS.LNTOTQTY  = TOTQTY
      THIS.LNTOTAL   = AMOUNT

    ENDSCAN
    SELECT (THIS.LCTMPRETLN)
    LOCATE
    THIS.LFWHENBROW()
  ENDIF


  *!*************************************************************
  *! Name      : lfvStyle
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the cntStyle control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVSTYLE
  LPARAMETERS LLBROWSE,LNITEMPART,LCVALUE,LCOLDVALUE,LCSTYLEVAL

  *This.lcStyle = EVALUATE(This.lcTmpRetLn+'.Style')

  *! B608974,1 MMT 08/18/2009 Fix bug of can add style if Style Segment entry setups is set to yes [START]
  *This.lcStyle = lcValue
  THIS.LCSTYLE = LCSTYLEVAL
  *! B608974,1 MMT 08/18/2009 Fix bug of can add style if Style Segment entry setups is set to yes [END]

  *-- If do not change the style & do not press the browse icon.
  IF (LCOLDVALUE = THIS.LCSTYLE .OR. EMPTY(THIS.LCSTYLE)) .AND. !LLBROWSE
    RETURN
  ENDIF
  SELECT STYLE

  LLBROWSE = .F.
  IF LLBROWSE .OR. !THIS.STYLE.SEEK(PADR(THIS.LCSTYLE,19))
    *Enable the soft seek in the style browse.
    *-- Call the global style browse.
    THIS.LCSTYLE = PADR(GFSTYBRW("I" , THIS.LCSTYLE , "" , .F.),19)
    SELECT (THIS.LCTMPRETLN)
    THIS.LFBRLINE()
  ENDIF

  LLBROWSE = .F.

  *-- If enter valid style.
  IF !EMPTY(THIS.LCSTYLE) .AND. THIS.STYLE.SEEK(PADR(THIS.LCSTYLE,19))
    *-- Get the selected style discount code..
    LCDISCCODE = STYLE.CDISCCODE

    *-- If the style division different from the header division, reenter the style.
    IF STYLE.CDIVISION <> RETAUTH.CDIVISION
      *** Style / Division confilct! ***
      *** <  Ok  > ***
      =GFMODALGEN("INM46003B00000" , "DIALOG")
      THIS.LFSTYCAN()
      RETURN .F.
      *--program enter this path if style.cdivision equal ladata[19]
      *-- we put endif instead of else to allow the program alwayse enter
      *ELSE
    ENDIF &&STYLE.cDivision <> RetAuth.cDivision
    *-- Check if the entered style assigned to the header warehouse.
    IF !THIS.STYDYE.SEEK(PADR(THIS.LCSTYLE,19) + RETAUTH.CWARECODE + SPACE(10))
      *** Style: ALLTRIM(lcStyle) is not assigned ***
      *** to warehouse: ALLTRIM(laData[7])   ***
      *** <  Add  > - < Reenter > ***
      LCTMPTXT = ALLTRIM(THIS.LCSTYLE) + "|" + ALLTRIM(RETAUTH.CWARECODE)
      IF GFMODALGEN("QRM46004B00031" , "DIALOG" , LCTMPTXT) = 1
        *-- Call global function to assign the warehouse.
        DO GPADSTYWAR WITH PADR(THIS.LCSTYLE,19) , SPACE(10) , RETAUTH.CWARECODE
      ELSE
        *-- Reenter the style.
        THIS.LFSTYCAN()
        RETURN .F.
      ENDIF
    ENDIF &&!SEEK(PADR(This.lcStyle,19) + RetAuth.cwarecode + SPACE(10) , 'StyDye')
    *-- Get the style scale.
    THIS.STYLE.SEEK(PADR(THIS.LCSTYLE,19))
    IF THIS.SCALE.SEEK('S' + STYLE.SCALE)
      THIS.LNSCALE     = SCALE.CNT
      FOR LNCOUNT = 1 TO 8
        LCCOUNT = ALLTRIM(STR(LNCOUNT))
        THIS.LASIZE[lnCount] = SCALE.SZ&LCCOUNT
        IF LNCOUNT <= THIS.LNSCALE
          *SHOW GET This.laQtyStk[lnCount] ENABLE && Revise
        ELSE
          *SHOW GET This.laQtyStk[lnCount] DISABLE &&Revise
        ENDIF
      ENDFOR
    ENDIF && SEEK('S' + STYLE.SCALE , 'SCALE')
    *-- Get the customer price level.
    THIS.LOFORM.PAGEFRAME.PAGE2.CNTSIZES.SCALE = STYLE.SCALE
    THIS.LCCUSTPLVL = IIF(LEN(ALLTRIM(THIS.LCCUSTPLVL))=0,'A',THIS.LCCUSTPLVL)

    THIS.LCDYELOT  = SPACE(10)
    LLRETRY   = .F.
    LLSHIPPED = .F.

    *-- Get the tax rate from the invoice header file.
    IF !EMPTY(RETAUTH.INVOICE)
      THIS.INVHDR.SEEK(RETAUTH.INVOICE)
      SELECT INVLINE
      THIS.INVLINE.SETORDER('INVLINES')
      IF THIS.INVLINE.SEEK(PADR(THIS.LCSTYLE,19) + RETAUTH.CWARECODE)
        THIS.LNPRICE = INVLINE.PRICE * (1-THIS.LNDISCPCNT/100)
        IF THIS.LLDYELOT
          THIS.LCDYELOT = INVLINE.DYELOT
        ENDIF
        LLSHIPPED  = .T.
      ELSE
        LLRETRY    = .T.
      ENDIF
    ELSE && !EMPTY(RetAuth.invoice) && Global
      *-- If no invoice was entered then search the lines for price.
      LLRETRY = .T.
    ENDIF && !EMPTY(RetAuth.invoice) && Global

    *-- Search for the style in the invoice header file if there is no invoice #.
    IF LLRETRY
      SELECT INVLINE
      THIS.INVLINE.SETORDER('INVLINES')
      SELECT INVHDR
      *-- Make invhdra the active index tag
      SET ORDER TO TAG INVHDRA DESCENDING && Revise
      =THIS.INVHDR.SEEK(RETAUTH.ACCOUNT)
      *-- Searching for the last invoice this style was shipped on using
      *-- the same current currency.
      WAIT WINDOW 'Searching for the last invoice this style was shipped on ....' NOWAIT

      SCAN REST WHILE ACCOUNT = RETAUTH.ACCOUNT FOR STATUS <> 'V'

        IF (THIS.LLMULCURR .AND. (INVHDR.CCURRCODE = RETAUTH.CCURRCODE)) .OR. !THIS.LLMULCURR
          LCSRCHINV = INVHDR.INVOICE
          SELECT INVLINE
          IF THIS.INVLINE.SEEK(PADR(THIS.LCSTYLE,19) + LCSRCHINV)
            THIS.LNPRICE   = PRICE * (1-INVHDR.DISCPCNT/100)
            LLSHIPPED = .T.
            IF THIS.LLDYELOT
              THIS.LCDYELOT = IIF(INVHDR.CWARECODE=RETAUTH.CWARECODE,INVLINE.DYELOT,SPACE(10))
            ENDIF
            *Exit for the scan to take the latest invoice price.
            EXIT
          ENDIF &&SEEK(PADR(This.lcStyle,19) + lcSrchInv , "INVLINE")
        ENDIF &&(This.llMulCurr .AND. (INVHDR.cCurrCode = RetAuth.ccurrcode)) .OR. !This.llMulCurr
        SELECT INVHDR
      ENDSCAN
      WAIT CLEAR
      SELECT INVLINE
      THIS.INVLINE.SETORDER('INVLINE')
      SELECT INVHDR
      THIS.INVHDR.SETORDER('INVHDR')

    ENDIF && llRetry

    IF !LLSHIPPED
      *** This style was not shipped to this customer! continue? ***
      *** <  Yes  > - <  No  > ***
      IF GFMODALGEN("QRM46005B00006" , "DIALOG") = 2
        THIS.LFSTYCAN()
        RETURN .F.
      ENDIF
      THIS.LNPSTRATE = THIS.LNCUSPSTRT
    ENDIF && !llShipped
    IF !LLSHIPPED

      *Move this line after checking if this style was shipped to the customer or not.
      THIS.LNPRICE = THIS.LFGETPRICE(THIS.LCSTYLE,THIS.LCCUSTPLVL,THIS.LNTOTQTY)

      *-- If the discount code for the selected style not empty.
      IF !EMPTY(LCDISCCODE)
        *-- Get the discount percentage for this discount code from the codes file.
        DECLARE LASTYDISC[1,2]
        LNDISCPCNT = THIS.LNDISCPCNT
        LASTYDISC[1,1] = 'DISCPCNT'
        LASTYDISC[1,2] = 'lnDiscPCnt'
        =GFRLTFLD(LCDISCCODE , @LASTYDISC , "CDISCCODE")
        THIS.LNDISCPCNT = LNDISCPCNT
        *-- If the discount percentage greater than zero...
        IF THIS.LNDISCPCNT > 0 && Private
          LCTMPSTR = ALLTRIM(STR(THIS.LNDISCPCNT)) +"|" + ALLTRIM(STR(THIS.LNPRICE,10,3)) + ;
            "|" + ALLTRIM(STR(THIS.LNPRICE * THIS.LNDISCPCNT /100,10,3)) + "|" + ;
            ALLTRIM(STR(THIS.LNPRICE - (THIS.LNPRICE * THIS.LNDISCPCNT / 100),10,3))
          *** There is discount percentage on this style : {lnDiscPcnt} ***
          *** Gross Price is : {lnPrice}
          *** Discount amount is : {lnPrice * lnDiscPcnt /100}
          *** Net Price is : {lnPrice - (lnPrice * lnDiscPcnt / 100)}
          =GFMODALGEN("INM46030B00000" , "DIALOG" , LCTMPSTR)

          *-- Get a discount on the total gross price, & inform the user.
          THIS.LNPRICE = THIS.LNPRICE - (THIS.LNPRICE * THIS.LNDISCPCNT / 100)
        ENDIF && This.lnDiscPCnt > 0
      ENDIF && !EMPTY(lcDiscCode)
    ENDIF && !llShipped
    *Adjust Open and Open Amount for case of changing the style.
    IF !THIS.LOFORMSET.ACTIVEMODE="V"
      *-- Total Open Qty.
      REPLACE RETAUTH.NRETA_OPN WITH RETAUTH.NRETA_OPN - EVALUATE(THIS.LCTMPRETLN+'.TotQty'), ;
        RETAUTH.NRTOPNAMT WITH RETAUTH.NRTOPNAMT - (EVALUATE(THIS.LCTMPRETLN+'.TotQty') * EVALUATE(THIS.LCTMPRETLN+'.Price')) IN RETAUTH
    ENDIF


    *-- Add line into the R/A temp. file.
    SELECT (THIS.LCTMPRETLN)
    REPLACE STYLE    WITH PADR(THIS.LCSTYLE,19) ;
      PRICE    WITH THIS.LNPRICE ;
      REASON   WITH RETAUTH.REASON ;
      TAX_RATE WITH IIF(THIS.LLTAXYES , THIS.LNTAXRATE , 0) ;
      NPSTRATE WITH IIF(THIS.LLTAXYES .AND. THIS.LLISCANADA , THIS.LNPSTRATE , 0) ;
      DYELOT   WITH THIS.LCDYELOT ;
      CSTATUS  WITH IIF(CSTATUS = 'S','M',CSTATUS)

    *Add true in the logical field if this style was shipped to this customer befor.
    REPLACE LSHIPPED  WITH LLSHIPPED
    
    *! E303662,2 MMT 05/26/2016 Add trigger to read style price from CSTPRICE table while adding lines[T20160324.0006][Start]
    IF ASCAN(THIS.LOFORM.PARENT.LAEVNTTRIG, PADR('CALCPRCE',10)) <> 0
      =THIS.LOFORM.PARENT.MDOTRIGGER(PADR('CALCPRCE',10))
    ENDIF
    *! E303662,2 MMT 05/26/2016 Add trigger to read style price from CSTPRICE table while adding lines[T20160324.0006][End]
    
  ELSE && !EMPTY(This.lcStyle) .AND. SEEK(PADR(This.lcStyle,19) , "STYLE")
    *-- Reenter the style.
    THIS.LFSTYCAN()
    RETURN .F.
  ENDIF && !EMPTY(This.lcStyle) .AND. SEEK(PADR(This.lcStyle,19) , "STYLE")

  SELECT (THIS.LCTMPRETLN) && Private
  THIS.LOFORM.PAGEFRAME.PAGE2.REFRESH()

  *-- Call the when browse function.
  THIS.LFWHENBROW()
  *Adjust Open and Open Amount for case of changing the style.
  IF !THIS.LOFORMSET.ACTIVEMODE="V"
    *-- Total Open Qty.
    REPLACE RETAUTH.NRETA_OPN WITH RETAUTH.NRETA_OPN + EVALUATE(THIS.LCTMPRETLN+'.TotQty'), ;
      RETAUTH.NRTOPNAMT WITH RETAUTH.NRTOPNAMT + (EVALUATE(THIS.LCTMPRETLN+'.TotQty') * EVALUATE(THIS.LCTMPRETLN+'.Price')),;
      RETAUTH.NRETA_BUD WITH RETAUTH.NRETA_CAN + RETAUTH.NRETA_OPN IN RETAUTH
    THIS.LNTOTQTY  = TOTQTY
    THIS.LNTOTAL   = AMOUNT
  ENDIF

  THIS.LOFORM.PAGEFRAME.PAGE2.KBDYELOT.LCSTYLECODE = LCSTYLEVAL
  THIS.LOFORM.PAGEFRAME.PAGE2.KBDYELOT.LCWARECODE  = THIS.LOFORM.PAGEFRAME.PAGE1.CBOCWARECODE.VALUE


  ENDFUNC
  *--end of lfvStyle


  *!*************************************************************
  *! Name      : lfStyCan
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Cancel adding a style
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFSTYCAN

  SELECT (THIS.LCTMPRETLN)
  IF EVALUATE(THIS.LCTMPRETLN+'.CSTATUS') = 'A'
    *-- If the record was new, change the status to be "S->Same"
    REPLACE CSTATUS WITH 'S'
    DELETE
    GO TOP
    THIS.LCSTYLE     = EVALUATE(THIS.LCTMPRETLN+'.STYLE')
    *-- Subtract 1 from the records counter.
    THIS.LNNOOFLINES = THIS.LNNOOFLINES - 1
    IF THIS.LNNOOFLINES = 0
      *Don't include lnNoOfLines when determining the invoice object status
      *as it should be always enabled in Add/Edit modes
      THIS.LCENTRSTAT= IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") .AND. EOF(THIS.LCTMPRETLN) .AND. !EMPTY(RETAUTH.INVOICE) , "ENABLE" , "DISABLE")
      THIS.LCINVSTAT = IIF((THIS.LOFORMSET.ACTIVEMODE="E" .OR. THIS.LOFORMSET.ACTIVEMODE="A") , "ENABLE" , "DISABLE")
    ENDIF
  ELSE
    THIS.LCSTYLE = EVALUATE(THIS.LCTMPRETLN+'.STYLE')
  ENDIF
  THIS.LOFORM.PAGEFRAME.PAGE2.REFRESH()
  *-- Call the when browse function.
  THIS.LFWHENBROW()

  ENDFUNC
  *--end of lfStyCan


  *!*************************************************************
  *! Name      : lfGetprice
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Get Style Price
  *!*************************************************************
  *! Parameters:  lcStyle : Style
  *!              lcLevel : Price level
  *!              lnQuantity : Total Quantity
  *!*************************************************************
  *! Return    : Alternative price
  *!*************************************************************
  FUNCTION LFGETPRICE
  PARAMETERS LCSTYLE,LCLEVEL,LNQUANTITY
  PRIVATE LNSTYPRIC,LCLEVEL,LNQUANTITY

  *Get the style price level.
  THIS.STYLE.SEEK(THIS.LCSTYLE)
  IF LCLEVEL = 'Q'
    DO CASE
    CASE STYLE.NATQTYC > 0 AND LNQUANTITY > STYLE.NATQTYC
      LCLEVEL = 'C'
    CASE STYLE.NATQTYB > 0 AND LNQUANTITY > STYLE.NATQTYB
      LCLEVEL = 'B'
    OTHERWISE
      LCLEVEL = 'A'
    ENDCASE
  ELSE
    LCLEVEL=IIF(INLIST(LCLEVEL,'A','B','C'),LCLEVEL,'A')
  ENDIF

  LNSTYPRIC = IIF(!THIS.LLMULCURR OR RETAUTH.CCURRCODE = OARIAAPPLICATION.BASECURRENCY,STYLE.PRICE&LCLEVEL,;
    GFSTYPRICE(THIS.LCSTYLE,THIS.LCCUSTPLVL,RETAUTH.CCURRCODE))

  *Call global function to get the price level.
  *Call local function to select between the different existing
  *price levels if the selected one is zero.
  *ASM, Call the global function lfCheckPri instead of lfCheckPric [Start]
  *RETURN(IIF(lnStyPric = 0 , This.lfCheckPric(This.lcStyle,lcLevel) , lnStyPric))
  RETURN(IIF(LNSTYPRIC = 0 , LFCHECKPRI(THIS.LCSTYLE,LCLEVEL) , LNSTYPRIC))
  *ASM, Call the global function lfCheckPri instead of lfCheckPric [End]
  ENDFUNC
  *--end of lfGetprice


  *!*************************************************************
  *! Name      : lfCheckPric
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Select alternative price level (No Need for it. Replaced by the global function lfCheckPri
  *!*************************************************************
  *! Parameters:  lcStyle : Style
  *!              lcLevel : Price level
  *!*************************************************************
  *! Return    : Alternative price
  *!*************************************************************
  FUNCTION LFCHECKPRIC
  PARAMETERS LCSTYLE,LCLEVEL
  PRIVATE LCTITLE,LCPROMPT,LCPROMPT1,LCPROMPT2,LCPROMPT3,LNSTYPRIC,LNSTYPRIC1,;
    LNSTYPRIC2,LNSTYPRIC3, LNPRICE

  *Seek in the style file to get the right price level.
  THIS.STYLE.SEEK(THIS.LCSTYLE)
  LCTITLE  = PROPER(THIS.LCSTYHDR)+': '+THIS.LCSTYLE
  LNSTYPRIC1 = IIF(!THIS.LLMULCURR OR RETAUTH.CCURRCODE=OARIAAPPLICATION.BASECURRENCY,STYLE.PRICEA,;
    GFSTYPRICE(THIS.LCSTYLE,'A',RETAUTH.CCURRCODE))
  LNSTYPRIC2 = IIF(!THIS.LLMULCURR OR RETAUTH.CCURRCODE=OARIAAPPLICATION.BASECURRENCY,STYLE.PRICEB,;
    GFSTYPRICE(THIS.LCSTYLE,'B',RETAUTH.CCURRCODE))
  LNSTYPRIC3 = IIF(!THIS.LLMULCURR OR RETAUTH.CCURRCODE=OARIAAPPLICATION.BASECURRENCY,STYLE.PRICEC,;
    GFSTYPRICE(THIS.LCSTYLE,'C',RETAUTH.CCURRCODE))
  LNSTYPRIC  = IIF(LNSTYPRIC1 >0,1,IIF(LNSTYPRIC2 >0,2,AT(LCLEVEL,'ABC')))
  LCPROMPT   = "Price level '"+LCLEVEL+"' is zero. Proceed with"
  LCPROMPT1  = 'Price level A, '+ALLTRIM(STR(LNSTYPRIC1,12,2))
  LCPROMPT2  = 'Price level B, '+ALLTRIM(STR(LNSTYPRIC2,12,2))
  LCPROMPT3  = 'Price level C, '+ALLTRIM(STR(LNSTYPRIC3,12,2))

  THIS.LNPRICE    = 1

  *Call screen to select price levels.
  *DO (oAriaApplication.ScreenHome+"SOSTYPRI.SPX")

  RETURN(EVAL('lnStyPric'+STR(THIS.LNPRICE,1))) && Private

  ENDFUNC
  *--end of lfCheckPric



  *!*************************************************************
  *! Name      : lfvReson2
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the cboReason Control in the Details Page
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVRESON2

  *-- Update the R/A lines temp. file.
  REPLACE CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS) IN (THIS.LCTMPRETLN)

  RETURN 1
  ENDFUNC
  *--end of lfvReson2


  *!*************************************************************
  *! Name      : lfvPrice
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the txtPrice Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVPRICE

  *Update the open amounts in the header folder if change the price.
  *-- Total Open Amount.
  REPLACE RETAUTH.NRTOPNAMT WITH RETAUTH.NRTOPNAMT - ;
    (EVALUATE(THIS.LCTMPRETLN+'.TotQty') * EVALUATE(THIS.LCTMPRETLN+'.Price')) IN RETAUTH

  *-- Update the price & the amount in the R/A lines temp. files.
  SELECT (THIS.LCTMPRETLN)
  REPLACE PRICE   WITH THIS.LNPRICE ;
    AMOUNT  WITH TOTQTY * PRICE ;
    CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS)

  *Update the open amounts in the header folder if change the price.
  *-- Total Open Amount.
  REPLACE RETAUTH.NRTOPNAMT WITH RETAUTH.NRTOPNAMT + ;
    (EVALUATE(THIS.LCTMPRETLN+'.TotQty') * EVALUATE(THIS.LCTMPRETLN+'.Price')) IN RETAUTH

  *-- Refresh the objects in the detail screen.
  THIS.LNPRICE  = PRICE
  THIS.LNTOTAL  = AMOUNT
  THIS.LOFORM.PAGEFRAME.PAGE2.REFRESH()

  ENDFUNC
  *--end of lfvPrice


  *!*************************************************************
  *! Name      : lfvTotQty
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the txtTotQty Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVTOTQTY

  *-- Update the amount in the R/A lines temp. files.
  SELECT (THIS.LCTMPRETLN)
  REPLACE AMOUNT  WITH THIS.LNTOTQTY * PRICE, ;
    CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS)

  THIS.LNTOTAL  = AMOUNT
  THIS.LOFORM.PAGEFRAME.PAGE2.REFRESH()


  ENDFUNC
  *--end of lfvTotQty


  *!*************************************************************
  *! Name      : lfvQty
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the Qty of the Sizes
  *!*************************************************************
  *! Parameters: lnFrom -> No. to know the qty. no.
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVQTY
  PARAMETERS LNFROM
  LOCAL LCFROM, LOFROM

  LCFROM = ALLTRIM(STR(LNFROM))
  LOFROM = THIS.LOFORM.PAGEFRAME.PAGE2.CNTSIZES.TXTQTY&LCFROM

  IF LOFROM.OLDVALUE <> LOFROM.VALUE

    *-- Total Open Qty.
    REPLACE RETAUTH.NRETA_OPN WITH RETAUTH.NRETA_OPN - EVALUATE(THIS.LCTMPRETLN+'.TotQty'), ;
      RETAUTH.NRTOPNAMT WITH RETAUTH.NRTOPNAMT - (EVALUATE(THIS.LCTMPRETLN+'.TotQty') * EVALUATE(THIS.LCTMPRETLN+'.Price')) IN RETAUTH

    IF THIS.RALINE.SEEK(RETAUTH.RANO + PADR(EVALUATE(THIS.LCTMPRETLN+'.Style'),19)) AND ;
        EVALUATE(THIS.LCTMPRETLN+'.cStatus') <> "A"
      LNOLDRAOPN = RALINE.NTOTOPNQTY - EVALUATE(THIS.LCTMPRETLN+'.TotQty')
    ENDIF

    *-- Update the qty., total qty. and amount in the R/A lines temp. files.
    SELECT (THIS.LCTMPRETLN)
    REPLACE TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
    REPLACE AMOUNT     WITH TOTQTY * PRICE, CSTATUS    WITH IIF(CSTATUS = 'S','M',CSTATUS)

    *-- If this record is added new in this session, Default the open qty. with
    *-- the entered qty.
    IF EVALUATE(THIS.LCTMPRETLN+'.cStatus') = "A"
      REPLACE NOPNQTY&LCFROM WITH QTY&LCFROM, ;
        NTOTOPNQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
    ENDIF

    *-- Get the original Qty. & open qty. from the RALINE file if not new record.
    IF EVALUATE(THIS.LCTMPRETLN+'.cStatus') = "A"
      LNOLDRAOPN = 0
      LNCURRAOPN = 0
    ELSE
      IF THIS.RALINE.SEEK(RETAUTH.RANO + PADR(EVALUATE(THIS.LCTMPRETLN+'.Style'),19))
        LNCURRAOPN = MAX(0 , RALINE.NTOTOPNQTY - EVALUATE(THIS.LCTMPRETLN+'.TotQty'))
      ELSE
        LNOLDRAOPN = 0
        LNCURRAOPN = 0
      ENDIF
    ENDIF

    *Check if this style was shipped to this customer before.
    IF !EVALUATE(THIS.LCTMPRETLN+'.lShipped')
      *Get the price level per qty.
      THIS.LCCUSTPLVL = IIF(EMPTY(CUSTOMER.PRICELVL) , "A" , CUSTOMER.PRICELVL)
      IF THIS.LCCUSTPLVL = "Q"
        LNCURPRICE = THIS.LFGETPRICE(THIS.LCSTYLE,THIS.LCCUSTPLVL,EVALUATE(THIS.LCTMPRETLN+'.TOTQTY'))

        *Calculate the discount for the current style price.
        LNCURPRICE = THIS.LFCALCDISC(THIS.LCSTYLE , LNCURPRICE)

        *Update the price.
        SELECT (THIS.LCTMPRETLN) && Private
        REPLACE PRICE   WITH LNCURPRICE ;
          AMOUNT  WITH TOTQTY * PRICE
      ENDIF
      THIS.LNPRICE = PRICE
      THIS.LOFORM.PAGEFRAME.PAGE2.TXTPRICE.REFRESH()
    ENDIF
    THIS.INVLINE.SETORDER('InvLine')

    *-- Total Canceled Qty.
    REPLACE RETAUTH.NRETA_CAN WITH RETAUTH.NRETA_CAN - LNOLDRAOPN + LNCURRAOPN, ;
      RETAUTH.NRETA_OPN WITH RETAUTH.NRETA_OPN + EVALUATE(THIS.LCTMPRETLN+'.TotQty') IN RETAUTH
    REPLACE RETAUTH.NRETA_BUD WITH RETAUTH.NRETA_CAN + RETAUTH.NRETA_OPN, ;
      RETAUTH.NRTOPNAMT WITH RETAUTH.NRTOPNAMT + ;
      (EVALUATE(THIS.LCTMPRETLN+'.TotQty') * EVALUATE(THIS.LCTMPRETLN+'.Price')) IN RETAUTH

    THIS.LNTOTQTY = TOTQTY
    THIS.LNTOTAL  = AMOUNT

  ENDIF
  *Force to push button new if last size.

  IF LNFROM = THIS.LNSCALE
    *This.loForm.PageFrame.Page2.cmdNew.SetFocus()
  ENDIF
  THIS.LOFORM.PAGEFRAME.PAGE2.REFRESH()

  RETURN 1
  ENDFUNC
  *--end of lfvQty


  *!*************************************************************
  *! Name      : lfCalcDisc
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Calculate style dicount for the selected price
  *!*************************************************************
  *! Parameters:  lcStyle   : Style
  *!              lnCurPric : Price level
  *!*************************************************************
  *! Return    : Price after discount
  *!*************************************************************
  FUNCTION LFCALCDISC
  PARAMETERS LCSTYLE , LNCURPRIC
  PRIVATE LCSTYLE , LNCURPRIC

  *Seek to find there is a discount for the current style price.
  IF THIS.STYLE.SEEK(THIS.LCSTYLE)
    LCDISCCODE = STYLE.CDISCCODE
    *-- If the discount code for the selected style not empty.
    IF !EMPTY(LCDISCCODE)
      *-- Get the discount percentage for this discount code from the codes file.
      DECLARE LASTYDISC[1,2]
      LASTYDISC[1,1] = 'DISCPCNT'
      LNDISCPCNT = THIS.LNDISCPCNT
      LASTYDISC[1,2] = 'lnDiscPCnt' && Private
      =GFRLTFLD(LCDISCCODE , @LASTYDISC , "CDISCCODE")
      THIS.LNDISCPCNT = LNDISCPCNT
      *-- If the discount percentage greater than zero...
      IF THIS.LNDISCPCNT > 0
        *Add condition to display the message only if the price changed.
        IF !(EVALUATE(THIS.LCTMPRETLN+'.PRICE') == (LNCURPRIC - (LNCURPRIC * THIS.LNDISCPCNT / 100)))
          LCTMPSTR = ALLTRIM(STR(THIS.LNDISCPCNT)) +"|" + ALLTRIM(STR(LNCURPRIC,10,3)) + ;
            "|" + ALLTRIM(STR(LNCURPRIC * THIS.LNDISCPCNT /100,10,3)) + "|" + ;
            ALLTRIM(STR(LNCURPRIC - (LNCURPRIC * THIS.LNDISCPCNT / 100),10,3))
          *** There is discount percentage on this style : {lnDiscPcnt} ***
          *** Gross Price is : {lnCurPric}
          *** Discount amount is : {lnCurPric * lnDiscPcnt /100}
          *** Net Price is : {lnCurPric - (lnCurPric * lnDiscPcnt / 100)}
          =GFMODALGEN("INM46030B00000" , "DIALOG" , LCTMPSTR)

        ENDIF
        *-- Get a discount on the total gross price, & inform the user.
        LNCURPRIC = LNCURPRIC - (LNCURPRIC * THIS.LNDISCPCNT / 100)
      ENDIF
    ENDIF
  ENDIF

  *Return with the current price.
  RETURN LNCURPRIC
  ENDFUNC
  *--end of lfCalcDisc


  *!*************************************************************
  *! Name      : lfvDyelot
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate the kbDyeLot Control
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFVDYELOT
  LOCAL KBDYELOT

  *-- If entered new dyelot.
  KBDYELOT = THIS.LOFORM.PAGEFRAME.PAGE2.KBDYELOT
  IF !EMPTY(KBDYELOT.KEYTEXTBOX.VALUE) .AND. KBDYELOT.KEYTEXTBOX.OLDVALUE <> KBDYELOT.KEYTEXTBOX.VALUE
    *-- If entered invalid dyelot, browse the available dyelots in the system.
    IF !THIS.STYDYE.SEEK(PADR(THIS.LCSTYLE,19) + RETAUTH.CWARECODE + THIS.LCDYELOT) .OR. "?" $ KBDYELOT.KEYTEXTBOX.VALUE
      KBDYELOT.KEYTEXTBOX.VALUE = ''
      LCDYELOT = KBDYELOT.KEYTEXTBOX.VALUE
      IF !SDYEBROW(PADR(THIS.LCSTYLE,19) , @LCDYELOT , .T. , RETAUTH.CRETNOTE1)
        KBDYELOT.KEYTEXTBOX.VALUE = LCDYELOT
        IF EMPTY(KBDYELOT.KEYTEXTBOX.VALUE)
          *** The dyelot cannot be empty.  This line will be removed. ***
          *** <  OK  > ***
          =GFMODALGEN("INM46007B00000" , "DIALOG")
          THIS.LFSTYCAN()
          RETURN
        ENDIF
      ENDIF
    ENDIF
    *-- Update the dyelot in the R/A lines temp. files.
    SELECT (THIS.LCTMPRETLN)
    REPLACE DYELOT  WITH KBDYELOT.KEYTEXTBOX.VALUE, CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS)
    KBDYELOT.REFRESH()
  ENDIF

  ENDFUNC
  *--end of lfvDyelot


  *!*************************************************************
  *! Name      : lfBeforeDelete
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : Called From The BeforeDelete Event of the FormSet
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : True / False
  *!*************************************************************
  FUNCTION LFBEFOREDELETE
  LOCAL LCASK

  LCASK = IIF(RETAUTH.STATUS<>'X',"Do you want to Cancel this record?",;
    "Do you want to UnCancel this record?")
  IF MESSAGEBOX(LCASK,36,THIS.LOFORM.CAPTION) <> 6
    RETURN .F.
  ENDIF

  *Open the style dyelot file.
  THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'StyDye',OARIAAPPLICATION.DATADIR+'StyDye')

  SELECT RETAUTH
  *Check for status 'Electronic' And cancelled IF come from CRM module ,
  *check for not EMPTY allneeded filed in updated file CRMESG
  IF (THIS.LLCRMINS AND RETAUTH.LFROMWEB) .AND.  RETAUTH.STATUS $ "EX"
    LCMTO       = IIF(THIS.CUSTOMER.SEEK('M'+RETAUTH.ACCOUNT),EVALUATE('Customer.' + GFGETMEMVAR('M_CONFMAIL')),'')
    LCLETTERID  = GFGETMEMVAR('M_RAAPR' , OARIAAPPLICATION.ACTIVECOMPANYID)

    IF EMPTY(LCMTO)
      *The Confirmation E-mail Address on the setup screen is blank.
      *<OK>
      =GFMODALGEN('TRM46039B00000','DIALOG')
      RETURN .F.
    ENDIF

    DO CASE
    CASE RETAUTH.STATUS = "E"
      IF EMPTY(LCLETTERID)
        *The Cancelled ID on the setup screen is blank. Are you sure you want to continue?
        *<OK>
        IF GFMODALGEN('QRM46041B32000','DIALOG') = 2
          RETURN .F.
        ENDIF
      ENDIF
    ENDCASE
  ENDIF

  DO CASE
    *-- You caccnot cancel a completed R/A.
  CASE RETAUTH.STATUS = "C"
    *** R/A has been received.  Cannot cancel it! ***
    *** < Ok > ***
    =GFMODALGEN("INM46019B00000" , "DIALOG")
    RETURN .F.
    *-- If the R/A status is opened.
  ENDCASE

  ENDFUNC
  *--end of lfBeforeDelelet


  *!*************************************************************
  *! Name      : lpDelScr
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To be called from the Delete mthod of the FormSet
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LPDELSCR
  LOCAL LCKEY

  LCKEY = THIS.LOFORM.KBRANO.KEYTEXTBOX.VALUE

  DO CASE

  CASE RETAUTH.STATUS  = "O"
    *** There is a partially receive had happened for the ***
    *** current R/A.  Do you wish to cancel the rest of ***
    *** the current R/A? ***
    *** < Yes > - < No > ***
    IF (RETAUTH.NRETA_REC = 0) .OR. (RETAUTH.NRETA_REC > 0 .AND. GFMODALGEN("INM46021B00006" , "DIALOG") = 1)
      LNTOTALCAN = 0
      SELECT RALINE
      THIS.RALINE.SEEK(RETAUTH.RANO)
      SCAN FOR RANO = RETAUTH.RANO
        *-- Consider the open qty. as canceled qty.
        LNTOTALCAN = LNTOTALCAN + NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8
        *-- Update the Ra values in the style file with the canceled qty.
        THIS.STYLE.SEEK(RALINE.STYLE)

        IF STYLE.LINVSTY
          REPLACE STYLE.RA1   WITH STYLE.RA1 - RALINE.NOPNQTY1 ;
            STYLE.RA2   WITH STYLE.RA2 - RALINE.NOPNQTY2 ;
            STYLE.RA3   WITH STYLE.RA3 - RALINE.NOPNQTY3 ;
            STYLE.RA4   WITH STYLE.RA4 - RALINE.NOPNQTY4 ;
            STYLE.RA5   WITH STYLE.RA5 - RALINE.NOPNQTY5 ;
            STYLE.RA6   WITH STYLE.RA6 - RALINE.NOPNQTY6 ;
            STYLE.RA7   WITH STYLE.RA7 - RALINE.NOPNQTY7 ;
            STYLE.RA8   WITH STYLE.RA8 - RALINE.NOPNQTY8 ;
            STYLE.TOTRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8 ;
            IN STYLE
          *-- Add the modified record to the CursorUpdate
          THIS.STYLE.REPLACE()
          *-- Update the Ra values for each style+warehouse with the canceled qty.
          IF THIS.STYDYE.SEEK(RALINE.STYLE + RETAUTH.CWARECODE + SPACE(10))
            REPLACE STYDYE.RA1   WITH STYDYE.RA1 - RALINE.NOPNQTY1 ;
              STYDYE.RA2   WITH STYDYE.RA2 - RALINE.NOPNQTY2 ;
              STYDYE.RA3   WITH STYDYE.RA3 - RALINE.NOPNQTY3 ;
              STYDYE.RA4   WITH STYDYE.RA4 - RALINE.NOPNQTY4 ;
              STYDYE.RA5   WITH STYDYE.RA5 - RALINE.NOPNQTY5 ;
              STYDYE.RA6   WITH STYDYE.RA6 - RALINE.NOPNQTY6 ;
              STYDYE.RA7   WITH STYDYE.RA7 - RALINE.NOPNQTY7 ;
              STYDYE.RA8   WITH STYDYE.RA8 - RALINE.NOPNQTY8 ;
              STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8 ;
              IN STYDYE
            *-- Add the modified record to the CursorUpdate
            THIS.STYDYE.REPLACE()
          ENDIF
          *-- Update the Ra values for each style+warehouse+dyelot with the canceled qty.
          *Make sure the style uses Dylots
          IF THIS.LLDYELOT AND !EMPTY(RALINE.DYELOT) .AND. THIS.STYDYE.SEEK(RALINE.STYLE + RETAUTH.CWARECODE + RALINE.DYELOT)
            REPLACE STYDYE.RA1   WITH STYDYE.RA1 - RALINE.NOPNQTY1 ;
              STYDYE.RA2   WITH STYDYE.RA2 - RALINE.NOPNQTY2 ;
              STYDYE.RA3   WITH STYDYE.RA3 - RALINE.NOPNQTY3 ;
              STYDYE.RA4   WITH STYDYE.RA4 - RALINE.NOPNQTY4 ;
              STYDYE.RA5   WITH STYDYE.RA5 - RALINE.NOPNQTY5 ;
              STYDYE.RA6   WITH STYDYE.RA6 - RALINE.NOPNQTY6 ;
              STYDYE.RA7   WITH STYDYE.RA7 - RALINE.NOPNQTY7 ;
              STYDYE.RA8   WITH STYDYE.RA8 - RALINE.NOPNQTY8 ;
              STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8 ;
              IN STYDYE
            *-- Add the modified record to the CursorUpdate
            THIS.STYDYE.REPLACE()
          ENDIF
        ENDIF
      ENDSCAN
      SELECT RETAUTH
      *-- Instead of deleting the RA, just cancel it.
      REPLACE STATUS    WITH IIF(RETAUTH.NRETA_REC = 0 , "X" , "C") ;
        NRETA_CAN WITH LNTOTALCAN ;
        NRETA_OPN WITH 0
    ENDIF
    *-- If the R/A status was canceled, change it to be open.
  CASE RETAUTH.STATUS = 'X'
    LNTOTALOPN = 0
    SELECT RALINE
    THIS.RALINE.SEEK(RETAUTH.RANO)
    SCAN FOR RANO = RETAUTH.RANO
      *-- Calculate the opened qty.
      LNTOTALOPN = LNTOTALOPN + NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8
      *-- Update the opened qty. in the R/A lines file.
      SELECT RALINE
      REPLACE RALINE.NOPNQTY1   WITH RALINE.QTY1 ;
        RALINE.NOPNQTY2   WITH RALINE.QTY2 ;
        RALINE.NOPNQTY3   WITH RALINE.QTY3 ;
        RALINE.NOPNQTY4   WITH RALINE.QTY4 ;
        RALINE.NOPNQTY5   WITH RALINE.QTY5 ;
        RALINE.NOPNQTY6   WITH RALINE.QTY6 ;
        RALINE.NOPNQTY7   WITH RALINE.QTY7 ;
        RALINE.NOPNQTY8   WITH RALINE.QTY8 ;
        RALINE.NTOTOPNQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
      *-- Add the modified record to the CursorUpdate
      THIS.RALINE.REPLACE()
      *-- Update the Ra values in the style file with the opened qty.
      THIS.STYLE.SEEK(RALINE.STYLE)
      IF STYLE.LINVSTY
        REPLACE STYLE.RA1   WITH STYLE.RA1 + EVALUATE(THIS.LCTMPRETLN+'.QTY1'), ;
          STYLE.RA2   WITH STYLE.RA2 + EVALUATE(THIS.LCTMPRETLN+'.QTY2'), ;
          STYLE.RA3   WITH STYLE.RA3 + EVALUATE(THIS.LCTMPRETLN+'.QTY3'), ;
          STYLE.RA4   WITH STYLE.RA4 + EVALUATE(THIS.LCTMPRETLN+'.QTY4'), ;
          STYLE.RA5   WITH STYLE.RA5 + EVALUATE(THIS.LCTMPRETLN+'.QTY5'), ;
          STYLE.RA6   WITH STYLE.RA6 + EVALUATE(THIS.LCTMPRETLN+'.QTY6'), ;
          STYLE.RA7   WITH STYLE.RA7 + EVALUATE(THIS.LCTMPRETLN+'.QTY7'), ;
          STYLE.RA8   WITH STYLE.RA8 + EVALUATE(THIS.LCTMPRETLN+'.QTY8'), ;
          STYLE.TOTRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8 ;
          IN STYLE
        *-- Add the modified record to the CursorUpdate
        THIS.STYLE.REPLACE()
        *-- Update the Ra values for each style+warehouse with the opened qty.
        IF THIS.STYDYE.SEEK(RALINE.STYLE + RETAUTH.CWARECODE + SPACE(10))
          REPLACE STYDYE.RA1   WITH STYDYE.RA1 + EVALUATE(THIS.LCTMPRETLN+'.QTY1'), ;
            STYDYE.RA2   WITH STYDYE.RA2 + EVALUATE(THIS.LCTMPRETLN+'.QTY2'), ;
            STYDYE.RA3   WITH STYDYE.RA3 + EVALUATE(THIS.LCTMPRETLN+'.QTY3'), ;
            STYDYE.RA4   WITH STYDYE.RA4 + EVALUATE(THIS.LCTMPRETLN+'.QTY4'), ;
            STYDYE.RA5   WITH STYDYE.RA5 + EVALUATE(THIS.LCTMPRETLN+'.QTY5'), ;
            STYDYE.RA6   WITH STYDYE.RA6 + EVALUATE(THIS.LCTMPRETLN+'.QTY6'), ;
            STYDYE.RA7   WITH STYDYE.RA7 + EVALUATE(THIS.LCTMPRETLN+'.QTY7'), ;
            STYDYE.RA8   WITH STYDYE.RA8 + EVALUATE(THIS.LCTMPRETLN+'.QTY8'), ;
            STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8 ;
            IN STYDYE
          *-- Add the modified record to the CursorUpdate
          THIS.STYDYE.REPLACE()
        ENDIF
        *-- Update the Ra values for each style+warehouse+dyelot with the opened qty.
        *Make sure the style uses Dylots
        IF THIS.LLDYELOT AND !EMPTY(RALINE.DYELOT) .AND. THIS.STYDYE.SEEK(RALINE.STYLE + RETAUTH.CWARECODE + RALINE.DYELOT)
          REPLACE STYDYE.RA1   WITH STYDYE.RA1 + EVALUATE(THIS.LCTMPRETLN+'.QTY1'), ;
            STYDYE.RA2   WITH STYDYE.RA2 + EVALUATE(THIS.LCTMPRETLN+'.QTY2'), ;
            STYDYE.RA3   WITH STYDYE.RA3 + EVALUATE(THIS.LCTMPRETLN+'.QTY3'), ;
            STYDYE.RA4   WITH STYDYE.RA4 + EVALUATE(THIS.LCTMPRETLN+'.QTY4'), ;
            STYDYE.RA5   WITH STYDYE.RA5 + EVALUATE(THIS.LCTMPRETLN+'.QTY5'), ;
            STYDYE.RA6   WITH STYDYE.RA6 + EVALUATE(THIS.LCTMPRETLN+'.QTY6'), ;
            STYDYE.RA7   WITH STYDYE.RA7 + EVALUATE(THIS.LCTMPRETLN+'.QTY7'), ;
            STYDYE.RA8   WITH STYDYE.RA8 + EVALUATE(THIS.LCTMPRETLN+'.QTY8'), ;
            STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8 ;
            IN STYDYE
          *-- Add the modified record to the CursorUpdate
          THIS.STYDYE.REPLACE()
        ENDIF
      ENDIF
    ENDSCAN
    SELECT RETAUTH
    *-- Instead of deleting the RA, just cancel it.
    REPLACE STATUS    WITH "O" ;
      NRETA_CAN WITH 0 ;
      NRETA_OPN WITH LNTOTALOPN
    *we will Search on the custom table if the mail sent we append new one
    *else we replace all with approved and the remanning text according to it[Start]
    IF (THIS.LLCRMINS AND RETAUTH.LFROMWEB)
      SELECT CRMESAG
      THIS.CRMESAG.SEEK('R' + RETAUTH.RANO)
      LCMFROM     = GFGETMEMVAR('M_NOTEMAIL' , OARIAAPPLICATION.ACTIVECOMPANYID)
      LCMTO       = IIF(THIS.CUSTOMER.SEEK('M'+RETAUTH.ACCOUNT),EVALUATE('Customer.' + GFGETMEMVAR('M_CONFMAIL')),'')

      *--if sent or not found add new one
      IF LSENT OR EOF()
        APPEND BLANK
        =GFADD_INFO('CRMesag')
        REPLACE CTRANSTYPE   WITH 'R',;
          CTRANSNO     WITH RETAUTH.RANO,;
          CMAILFROM    WITH LCMFROM,;
          DTRANSDATE   WITH OARIAAPPLICATION.SYSTEMDATE,;
          CMAILTO      WITH LCMTO,;
          LSENT        WITH .F.

        LCMSGSBJCT  = 'Your R/A Request #:'+ RETAUTH.RANO + 'has been approved.'
        LCLETTERID  = GFGETMEMVAR('M_RAAPR' , OARIAAPPLICATION.ACTIVECOMPANYID)

        REPLACE LAPPROVE WITH .T. , CMSGSUBJCT WITH LCMSGSBJCT , ;
          CLETTERID WITH LCLETTERID

      ELSE
        REPLACE CTRANSTYPE   WITH 'R' ,;
          CTRANSNO     WITH RETAUTH.RANO , ;
          CMAILFROM    WITH LCMFROM ,;
          DTRANSDATE   WITH OARIAAPPLICATION.SYSTEMDATE,;
          CMAILTO      WITH    LCMTO, ;
          LSENT        WITH .F.

        LCMSGSBJCT  = 'Your R/A Request #:'+ RETAUTH.RANO + 'has been approved.'
        LCLETTERID  = GFGETMEMVAR('M_RAAPR' , OARIAAPPLICATION.ACTIVECOMPANYID)
        REPLACE LAPPROVE   WITH .T. ,;
          CMSGSUBJCT WITH LCMSGSBJCT , ;
          CLETTERID  WITH LCLETTERID

      ENDIF
      *-- Add the modified record to the CursorUpdate
      THIS.CRMESAG.REPLACE()
    ENDIF
    *-in case of cancel Electronic one
  CASE RETAUTH.STATUS = 'E'
    IF THIS.LLCRMINS
      LNTOTALCAN = 0
      SELECT RALINE
      THIS.RALINE.SEEK(RETAUTH.RANO)
      SCAN FOR RANO = RETAUTH.RANO
        *-- Consider the open qty. as canceled qty.
        LNTOTALCAN = LNTOTALCAN + NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8

      ENDSCAN
      SELECT RETAUTH
      *-- Instead of deleting the RA, just cancel it.
      REPLACE STATUS    WITH IIF(RETAUTH.NRETA_REC = 0 , "X" , "C") ;
        NRETA_CAN WITH LNTOTALCAN ;
        NRETA_OPN WITH 0
    ENDIF
    *--get variables
    LCMFROM     = GFGETMEMVAR('M_NOTEMAIL' , OARIAAPPLICATION.ACTIVECOMPANYID)
    LCMSGSBJCT  = 'Your R/A Request #:'+ RETAUTH.RANO + 'has not been approved.'

    SELECT CRMESAG
    APPEND BLANK
    REPLACE CTRANSTYPE WITH 'R',;
      CTRANSNO   WITH RETAUTH.RANO,;
      LAPPROVE  WITH .F.,;
      CMAILFROM  WITH LCMFROM,;
      CMAILTO    WITH LCMTO ,;
      CMSGSUBJCT WITH LCMSGSBJCT,;
      CLETTERID  WITH LCLETTERID,;
      LSENT      WITH .F.
    *-- Add the modified record to the CursorUpdate
    THIS.CRMESAG.REPLACE()

  ENDCASE
  *-- Refresh laData array that hold the header info with the updated values.
  SELECT RETAUTH
  *Old: SCATTER FIELDS &lcScFields TO laData
  *-- Add the modified record to the CursorUpdate
  THIS.RETAUTH.REPLACE()

  THIS.LFSAVEFILES()
  IF THIS.LOFORMSET.AUDITTRAIL
    LCEVENT = "DELETE"
    THIS.LOFORMSET.ADDAUDIT(LCEVENT)
  ENDIF

  SELECT RETAUTH
  THIS.LOFORMSET.SEEKRECORD(LCKEY)
  THIS.LPSHOW()

  ENDFUNC
  *--end of lpDelScr


  *!*************************************************************
  *! Name      : lfBeforeSave
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To be Called From the BeforeSave Event of the FormSet
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : True / False
  *!*************************************************************
  FUNCTION LFBEFORESAVE

  *ASM, Do not save if the account was left blank [Start]
  IF EMPTY(THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.VALUE)
    *** You have to enter an account. Cannot save! ***
    *** <  Ok  > ***
    =GFMODALGEN('INM0240B00000','ALERT')
    THIS.LOFORM.KBACCOUNT.KEYTEXTBOX.SETFOCUS()
    LLCSAVE = .F.
    RETURN .F.
  ENDIF
  *ASM, Do not save if the account was left blank [End]

  *-- Do not save if the warehouse code was not entered.
  IF THIS.LLMULTIWH
    IF EMPTY(RETAUTH.CWARECODE)
      *** You have to enter a warehouse. Cannot save! ***
      *** <  Ok  > ***
      =GFMODALGEN('INM46013B00000','ALERT')
      IF THIS.LOFORM.PAGEFRAME.ACTIVEPAGE = 1
        THIS.LOFORM.PAGEFRAME.PAGE1.COBCWARECODE.SETFOCUS()
      ENDIF
      LLCSAVE = .F.
      RETURN .F.
    ENDIF
  ELSE
    REPLACE RETAUTH.CWARECODE WITH THIS.LCDEFWARE IN RETAUTH
  ENDIF

  *-- Do not save if the system was multi currency & the
  *-- currency info was not entered.
  IF THIS.LLMULCURR
    DO CASE
    CASE EMPTY(RETAUTH.CCURRCODE)
      *** You cannot leave the currency code empty! ***
      *** <  Ok  > ***
      =GFMODALGEN("INM46015B00000" , "DIALOG")
      IF THIS.LOFORM.PAGEFRAME.ACTIVEPAGE = 1
        THIS.LOFORM.PAGEFRAME.PAGE1.KBCCURRCODE.SETFOCUS()
      ENDIF
      LLCSAVE = .F.
      RETURN .F.
    CASE EMPTY(RETAUTH.NEXRATE)
      *** The exchange rate must be greater than zero. ***
      *** <  Ok  > ***
      =GFMODALGEN("INM46001B00000" , "DIALOG")
      IF THIS.LOFORM.PAGEFRAME.ACTIVEPAGE = 1
        THIS.LOFORM.PAGEFRAME.PAGE1.TXTNEXRATE.SETFOCUS()
      ENDIF
      LLCSAVE = .F.
      RETURN .F.
    ENDCASE
    *-- Got exchange rate sign and unit sign.
    THIS.LCUNTSIN = ' '
    LCUNTSIN = THIS.LCUNTSIN
    THIS.LCEXRSIN = GFGETEXSIN(@LCUNTSIN, RETAUTH.CCURRCODE)
    THIS.LCUNTSIN = LCUNTSIN
  ELSE
    REPLACE RETAUTH.CCURRCODE WITH OARIAAPPLICATION.BASECURRENCY, ;
      RETAUTH.NCURRUNIT WITH 1, RETAUTH.NEXRATE WITH 1 IN RETAUTH
    THIS.LCUNTSIN   = '/'        &&  Variable to hold unit sign. && Private
    THIS.LCEXRSIN   = '/'        &&  Variable to hold exchange rate sign. && Private
  ENDIF

  *-- Flag to know if enter the add mode before or not.
  THIS.LLADDMODE  = .F.

  *-- Check the authorized amount.
  IF RETAUTH.AUTHAMT = 0
    *** Total authorization amount is zero.  Continue? ***
    *** < Yes > - < No > ***
    IF GFMODALGEN("INM46017B00006" , "DIALOG") = 2
      IF THIS.LOFORM.PAGEFRAME.ACTIVEPAGE = 1
        THIS.LOFORM.PAGEFRAME.PAGE1.TXTAUTH.SETFOCUS()
      ENDIF
      LLCSAVE = .F.
      RETURN .F.
    ENDIF
  ENDIF

  *! E303662,1 MMT 04/21/2016 Add trigger to read style price from CSTPRICE table[T20160324.0006][Start]
  *! E303662,2 MMT 05/26/2016 Add trigger to read style price from CSTPRICE table while adding lines[T20160324.0006][Start]
*!*	  IF ASCAN(THIS.LOFORM.PARENT.LAEVNTTRIG, PADR('CALCPRCE',10)) <> 0
*!*	    =THIS.LOFORM.PARENT.MDOTRIGGER(PADR('CALCPRCE',10))
*!*	  ENDIF
  *! E303662,2 MMT 05/26/2016 Add trigger to read style price from CSTPRICE table while adding lines[T20160324.0006][End]
  *! E303662,1 MMT 04/21/2016 Add trigger to read style price from CSTPRICE table[T20160324.0006][End]

  *Do not adjust the authorized qty. & amount if there is no lines
  *in the current RA, save them as they entered due to a 2.6 feature.
  IF THIS.LNNOOFLINES > 0
    *-- Count total qty. & total amount for al valid lines.
    SELECT (THIS.LCTMPRETLN)
    SUM TOTQTY , AMOUNT FOR (CSTATUS <> "D" .OR. !DELETED()) ;
      .AND. !EMPTY(STYLE) TO LNALLQTY , LNALLAMONT

    *-- Compare the total Qty. & total amount for all the lines with
    *-- the total Qty. & total amount in the header info.
    IF (LNALLAMONT <> RETAUTH.AUTHAMT) .OR. (LNALLQTY <> RETAUTH.AUTH)
      *** Authorized pieces and amount do not match the line items!  Continue? ***
      *** < Yes > - < No > ***
      IF GFMODALGEN("INM46018B00006" , "DIALOG") = 2
        IF THIS.LOFORM.PAGEFRAME.ACTIVEPAGE = 1
          IF LNALLQTY <> RETAUTH.AUTH
            THIS.LOFORM.PAGEFRAME.PAGE1.TXTAUTH.SETFOCUS()
          ELSE
            THIS.LOFORM.PAGEFRAME.PAGE1.TXTAUTHAMT.SETFOCUS()
          ENDIF
        ENDIF
        LLCSAVE = .F.
        RETURN .F.
      ENDIF
    ENDIF

    *-- Assign the total Qty. & total amount for all the lines to
    *-- the total Qty. & total amount in the header info.
    REPLACE RETAUTH.AUTHAMT WITH LNALLAMONT, RETAUTH.AUTH WITH LNALLQTY, ;
      RETAUTH.NRETA_OPN WITH LNALLQTY IN RETAUTH
  ENDIF

  ENDFUNC
  *--end of lfBeforeSave


  *!*************************************************************
  *! Name      : lpSavScr
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Called from the SaveFiles method of the FormSet
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LPSAVSCR
  LPARAMETERS LLCALLEDFROMDELETE
  LOCAL LNSEQ

  *-- If add mode, assign a R/A no. for the current R/A & add it for all lines.
  IF THIS.LOFORMSET.ACTIVEMODE="A"
    THIS.RETAUTH.APPEND()
    *wael
    *!*	    IF This.llBased
    *!*	      *B132218,1 KHM 05/25/2006 [Start]
    *!*	      *lnSeq = This.gfSequence('RANO','','',RetAuth.cDivision)
    *!*	      lnSeq = gfSequence('RANO','','',RetAuth.cDivision)
    *!*	      *B132218,1 KHM 05/25/2006 [End]
    *!*
    *!*	      REPLACE RetAuth.rano WITH lnSeq IN RetAuth
    *!*	    ELSE
    *!*	      *B132218,1 KHM 05/25/2006 [Start]
    *!*	      *lnSeq = This.gfSequence('RANO')
    *!*	      lnSeq = This.gfSequence('RANO')
    *!*	      *B132218,1 KHM 05/25/2006 [End]
    *!*
    *!*	      REPLACE RetAuth.rano WITH lnSeq IN RetAuth
    *!*	    ENDIF
    LNSEQ = GFSEQUENCE('RANO','','',RETAUTH.CDIVISION)
    REPLACE RANO WITH LNSEQ IN RETAUTH
    *wael

    SELECT (THIS.LCTMPRETLN)
    REPLACE ALL RANO WITH RETAUTH.RANO
  ELSE
    THIS.RETAUTH.REPLACE()
  ENDIF

  *-- Update the header file.
  SELECT RETAUTH
  *!*    TABLEUPDATE(.F.)
  *!*    lcSavOrder = TAG()
  *!*    This.RETAUTH.SetOrder('RETAUTH')
  *Old: IF This.loFormSet.ActiveMode="A"
  *Old: APPEND BLANK
  *Old: ELSE
  *Old: This.RETAUTH.SEEK(RetAuth.rano)
  *Old: ENDIF
  *Old: GATHER FIELDS &lcScFields FROM laData
  *-- Call global function to add audit fields info.
  REPLACE LLOK_STAT WITH .F.
  =GFADD_INFO('RETAUTH')
  *-- Add the modified record to the CursorUpdate
  SCATTER MEMVAR MEMO
  SELECT (THIS.RETAUTH.LCCURSORUPDATE)
  GATHER MEMVAR MEMO
  SELECT RETAUTH
  *!*    SELECT RetAuth
  *!*    TABLEUPDATE(.F.)
  *!*    This.RETAUTH.SetOrder(lcSavOrder)


  *-- R/A line no.
  *B608545,1 WAM 05/08/2008 Increament to the last RA line number
  *This.lnRa_LinNo = 1
  *B608545,1 WAM 05/08/2008 (End)

  *-- Calculate the total
  STORE 0 TO LNTOTALOPN , LNTOTALCAN
  *Open the style dyelot file.
  THIS.LFOPENFILE(OARIAAPPLICATION.DATADIR+'StyDye',OARIAAPPLICATION.DATADIR+'StyDye')

  *-- Change the status for empty styles records to be "SAME" to
  *-- prevent it from processing.
  SELECT (THIS.LCTMPRETLN)
  REPLACE ALL CSTATUS WITH "S" FOR EMPTY(STYLE)
  DELETE  ALL FOR EMPTY(STYLE) .AND. CSTATUS = "S"

  *-- Process the R/A lines.
  LCSAVDELETE = SET('DELETE')
  SET DELETE OFF

  LNCURREC  = 1                     && Var. hold current record no.
  LNTOTREC  = RECCOUNT(THIS.LCTMPRETLN)  && Var. hold total lines record count. && Private

  *-- process only the lines that its status not equal "S"
  SCAN FOR !EMPTY(STYLE)
    *-- Call the global function that execute the thermometer.
    *Old: =gfTherm(lnTotRec,lnCurRec,"Saving return authorization # : "+RetAuth.rano)
    LNCURREC = LNCURREC + 1

    SELECT (THIS.LCTMPRETLN)
    IF CSTATUS <> 'S'
      THIS.STYLE.SEEK(EVALUATE(THIS.LCTMPRETLN+'.Style'))
      DO CASE
        *-- If the record was added.
      CASE CSTATUS = "A"

        *Check for status 'Electronic'
        IF RETAUTH.STATUS <> "E"
          IF STYLE.LINVSTY
            *-- Update the Ra values in the style file.
            REPLACE STYLE.RA1   WITH STYLE.RA1 + EVALUATE(THIS.LCTMPRETLN+'.QTY1'), ;
              STYLE.RA2   WITH STYLE.RA2 + EVALUATE(THIS.LCTMPRETLN+'.QTY2'), ;
              STYLE.RA3   WITH STYLE.RA3 + EVALUATE(THIS.LCTMPRETLN+'.QTY3'), ;
              STYLE.RA4   WITH STYLE.RA4 + EVALUATE(THIS.LCTMPRETLN+'.QTY4'), ;
              STYLE.RA5   WITH STYLE.RA5 + EVALUATE(THIS.LCTMPRETLN+'.QTY5'), ;
              STYLE.RA6   WITH STYLE.RA6 + EVALUATE(THIS.LCTMPRETLN+'.QTY6'), ;
              STYLE.RA7   WITH STYLE.RA7 + EVALUATE(THIS.LCTMPRETLN+'.QTY7'), ;
              STYLE.RA8   WITH STYLE.RA8 + EVALUATE(THIS.LCTMPRETLN+'.QTY8'), ;
              STYLE.TOTRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8 ;
              IN STYLE
            *-- Add the modified record to the CursorUpdate
            THIS.STYLE.REPLACE()
            *-- Update the Ra values for each style+warehouse with the canceled qty.
            IF THIS.STYDYE.SEEK(EVALUATE(THIS.LCTMPRETLN+'.Style') + RETAUTH.CWARECODE + SPACE(10))
              REPLACE STYDYE.RA1   WITH STYDYE.RA1 + EVALUATE(THIS.LCTMPRETLN+'.QTY1'),  ;
                STYDYE.RA2   WITH STYDYE.RA2 + EVALUATE(THIS.LCTMPRETLN+'.QTY2'),  ;
                STYDYE.RA3   WITH STYDYE.RA3 + EVALUATE(THIS.LCTMPRETLN+'.QTY3'),  ;
                STYDYE.RA4   WITH STYDYE.RA4 + EVALUATE(THIS.LCTMPRETLN+'.QTY4'),  ;
                STYDYE.RA5   WITH STYDYE.RA5 + EVALUATE(THIS.LCTMPRETLN+'.QTY5'),  ;
                STYDYE.RA6   WITH STYDYE.RA6 + EVALUATE(THIS.LCTMPRETLN+'.QTY6'),  ;
                STYDYE.RA7   WITH STYDYE.RA7 + EVALUATE(THIS.LCTMPRETLN+'.QTY7'),  ;
                STYDYE.RA8   WITH STYDYE.RA8 + EVALUATE(THIS.LCTMPRETLN+'.QTY8'),  ;
                STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8 ;
                IN STYDYE
              *-- Add the modified record to the CursorUpdate
              THIS.STYDYE.REPLACE()
            ENDIF
            *Make sure the style uses Dylots
            IF THIS.LLDYELOT AND !EMPTY(EVALUATE(THIS.LCTMPRETLN+'.Dyelot')) .AND. THIS.STYDYE.SEEK(EVALUATE(THIS.LCTMPRETLN+'.Style') + RETAUTH.CWARECODE + EVALUATE(THIS.LCTMPRETLN+'.Dyelot'))

              REPLACE STYDYE.RA1   WITH STYDYE.RA1 + EVALUATE(THIS.LCTMPRETLN+'.QTY1'), ;
                STYDYE.RA2   WITH STYDYE.RA2 + EVALUATE(THIS.LCTMPRETLN+'.QTY2'), ;
                STYDYE.RA3   WITH STYDYE.RA3 + EVALUATE(THIS.LCTMPRETLN+'.QTY3'), ;
                STYDYE.RA4   WITH STYDYE.RA4 + EVALUATE(THIS.LCTMPRETLN+'.QTY4'), ;
                STYDYE.RA5   WITH STYDYE.RA5 + EVALUATE(THIS.LCTMPRETLN+'.QTY5'), ;
                STYDYE.RA6   WITH STYDYE.RA6 + EVALUATE(THIS.LCTMPRETLN+'.QTY6'), ;
                STYDYE.RA7   WITH STYDYE.RA7 + EVALUATE(THIS.LCTMPRETLN+'.QTY7'), ;
                STYDYE.RA8   WITH STYDYE.RA8 + EVALUATE(THIS.LCTMPRETLN+'.QTY8'), ;
                STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8 ;
                IN STYDYE
              *-- Add the modified record to the CursorUpdate
              THIS.STYDYE.REPLACE()
            ENDIF
          ENDIF
        ENDIF
        *-- Calculate the open qty.
        LNTOTALOPN = LNTOTALOPN + (NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8)
        *-- If the record was modified.
      CASE CSTATUS = "M"
        *-- Update the R/A line open qty.
        *Seek with the line's original style as it might be modified.
        THIS.RALINE.SEEK(EVALUATE(THIS.LCTMPRETLN+'.RaNo') + EVALUATE(THIS.LCTMPRETLN+'.oldSty') + EVALUATE(THIS.LCTMPRETLN+'.cra_linno'))
        REPLACE NOPNQTY1   WITH MAX(0 , (NOPNQTY1 + QTY1 - RALINE.QTY1)) ;
          NOPNQTY2   WITH MAX(0 , (NOPNQTY2 + QTY2 - RALINE.QTY2)) ;
          NOPNQTY3   WITH MAX(0 , (NOPNQTY3 + QTY3 - RALINE.QTY3)) ;
          NOPNQTY4   WITH MAX(0 , (NOPNQTY4 + QTY4 - RALINE.QTY4)) ;
          NOPNQTY5   WITH MAX(0 , (NOPNQTY5 + QTY5 - RALINE.QTY5)) ;
          NOPNQTY6   WITH MAX(0 , (NOPNQTY6 + QTY6 - RALINE.QTY6)) ;
          NOPNQTY7   WITH MAX(0 , (NOPNQTY7 + QTY7 - RALINE.QTY7)) ;
          NOPNQTY8   WITH MAX(0 , (NOPNQTY8 + QTY8 - RALINE.QTY8)) ;
          NTOTOPNQTY WITH NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8
        *Check for status 'Electronic' [Start]
        IF RETAUTH.STATUS <> "E"
          IF STYLE.LINVSTY
            *-- Update the Ra values in the style file for each R/A line.
            REPLACE STYLE.RA1   WITH STYLE.RA1 + EVALUATE(THIS.LCTMPRETLN+'.QTY1') - RALINE.QTY1 ;
              STYLE.RA2   WITH STYLE.RA2 + EVALUATE(THIS.LCTMPRETLN+'.QTY2') - RALINE.QTY2 ;
              STYLE.RA3   WITH STYLE.RA3 + EVALUATE(THIS.LCTMPRETLN+'.QTY3') - RALINE.QTY3 ;
              STYLE.RA4   WITH STYLE.RA4 + EVALUATE(THIS.LCTMPRETLN+'.QTY4') - RALINE.QTY4 ;
              STYLE.RA5   WITH STYLE.RA5 + EVALUATE(THIS.LCTMPRETLN+'.QTY5') - RALINE.QTY5 ;
              STYLE.RA6   WITH STYLE.RA6 + EVALUATE(THIS.LCTMPRETLN+'.QTY6') - RALINE.QTY6 ;
              STYLE.RA7   WITH STYLE.RA7 + EVALUATE(THIS.LCTMPRETLN+'.QTY7') - RALINE.QTY7 ;
              STYLE.RA8   WITH STYLE.RA8 + EVALUATE(THIS.LCTMPRETLN+'.QTY8') - RALINE.QTY8 ;
              STYLE.TOTRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8 ;
              IN STYLE
            *-- Add the modified record to the CursorUpdate
            THIS.STYLE.REPLACE()
            *-- Seek the RA values for each style+warehouse in the style dyelot file.
            IF THIS.STYDYE.SEEK(EVALUATE(THIS.LCTMPRETLN+'.Style') + RETAUTH.CWARECODE + SPACE(10))
              REPLACE STYDYE.RA1   WITH STYDYE.RA1 + EVALUATE(THIS.LCTMPRETLN+'.QTY1') - RALINE.QTY1 ;
                STYDYE.RA2   WITH STYDYE.RA2 + EVALUATE(THIS.LCTMPRETLN+'.QTY2') - RALINE.QTY2 ;
                STYDYE.RA3   WITH STYDYE.RA3 + EVALUATE(THIS.LCTMPRETLN+'.QTY3') - RALINE.QTY3 ;
                STYDYE.RA4   WITH STYDYE.RA4 + EVALUATE(THIS.LCTMPRETLN+'.QTY4') - RALINE.QTY4 ;
                STYDYE.RA5   WITH STYDYE.RA5 + EVALUATE(THIS.LCTMPRETLN+'.QTY5') - RALINE.QTY5 ;
                STYDYE.RA6   WITH STYDYE.RA6 + EVALUATE(THIS.LCTMPRETLN+'.QTY6') - RALINE.QTY6 ;
                STYDYE.RA7   WITH STYDYE.RA7 + EVALUATE(THIS.LCTMPRETLN+'.QTY7') - RALINE.QTY7 ;
                STYDYE.RA8   WITH STYDYE.RA8 + EVALUATE(THIS.LCTMPRETLN+'.QTY8') - RALINE.QTY8 ;
                STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8 ;
                IN STYDYE
              *-- Add the modified record to the CursorUpdate
              THIS.STYDYE.REPLACE()
            ENDIF
            *-- If the system & style dyelot yes, Update the Ra values for each
            *-- style+warehouse+dyelot in the style dyelot file.
            *Make sure the style uses Dylots
            IF THIS.LLDYELOT AND !EMPTY(EVALUATE(THIS.LCTMPRETLN+'.Dyelot')) .AND. THIS.STYDYE.SEEK(EVALUATE(THIS.LCTMPRETLN+'.Style') + RETAUTH.CWARECODE + EVALUATE(THIS.LCTMPRETLN+'.Dyelot'))

              REPLACE STYDYE.RA1   WITH STYDYE.RA1 + EVALUATE(THIS.LCTMPRETLN+'.QTY1') - RALINE.QTY1 ;
                STYDYE.RA2   WITH STYDYE.RA2 + EVALUATE(THIS.LCTMPRETLN+'.QTY2') - RALINE.QTY2 ;
                STYDYE.RA3   WITH STYDYE.RA3 + EVALUATE(THIS.LCTMPRETLN+'.QTY3') - RALINE.QTY3 ;
                STYDYE.RA4   WITH STYDYE.RA4 + EVALUATE(THIS.LCTMPRETLN+'.QTY4') - RALINE.QTY4 ;
                STYDYE.RA5   WITH STYDYE.RA5 + EVALUATE(THIS.LCTMPRETLN+'.QTY5') - RALINE.QTY5 ;
                STYDYE.RA6   WITH STYDYE.RA6 + EVALUATE(THIS.LCTMPRETLN+'.QTY6') - RALINE.QTY6 ;
                STYDYE.RA7   WITH STYDYE.RA7 + EVALUATE(THIS.LCTMPRETLN+'.QTY7') - RALINE.QTY7 ;
                STYDYE.RA8   WITH STYDYE.RA8 + EVALUATE(THIS.LCTMPRETLN+'.QTY8') - RALINE.QTY8 ;
                STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8 ;
                IN STYDYE
              *-- Add the modified record to the CursorUpdate
              THIS.STYDYE.REPLACE()
            ENDIF
          ENDIF
          *Check for status 'Electronic'
        ENDIF
        LNTOTALOPN = LNTOTALOPN + (NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8)
        *-- If the record was deleted.
      CASE CSTATUS = "D"
        *-- Update the R/A values for each line in the style file.
        *Check for status 'Electronic'
        IF RETAUTH.STATUS <> "E"
          THIS.RALINE.SEEK(EVALUATE(THIS.LCTMPRETLN+'.RaNo') + EVALUATE(THIS.LCTMPRETLN+'.Style') + EVALUATE(THIS.LCTMPRETLN+'.cra_linno'))
          IF STYLE.LINVSTY
            REPLACE STYLE.RA1   WITH STYLE.RA1 - RALINE.QTY1 ;
              STYLE.RA2   WITH STYLE.RA2 - RALINE.QTY2 ;
              STYLE.RA3   WITH STYLE.RA3 - RALINE.QTY3 ;
              STYLE.RA4   WITH STYLE.RA4 - RALINE.QTY4 ;
              STYLE.RA5   WITH STYLE.RA5 - RALINE.QTY5 ;
              STYLE.RA6   WITH STYLE.RA6 - RALINE.QTY6 ;
              STYLE.RA7   WITH STYLE.RA7 - RALINE.QTY7 ;
              STYLE.RA8   WITH STYLE.RA8 - RALINE.QTY8 ;
              STYLE.TOTRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8 ;
              IN STYLE
            *-- Add the modified record to the CursorUpdate
            THIS.STYLE.REPLACE()
            *-- Seek the RA values for each style+warehouse in the style dyelot file.
            IF THIS.STYDYE.SEEK(EVALUATE(THIS.LCTMPRETLN+'.Style') + RETAUTH.CWARECODE + SPACE(10))
              REPLACE STYDYE.RA1   WITH STYDYE.RA1 - RALINE.QTY1 ;
                STYDYE.RA2   WITH STYDYE.RA2 - RALINE.QTY2 ;
                STYDYE.RA3   WITH STYDYE.RA3 - RALINE.QTY3 ;
                STYDYE.RA4   WITH STYDYE.RA4 - RALINE.QTY4 ;
                STYDYE.RA5   WITH STYDYE.RA5 - RALINE.QTY5 ;
                STYDYE.RA6   WITH STYDYE.RA6 - RALINE.QTY6 ;
                STYDYE.RA7   WITH STYDYE.RA7 - RALINE.QTY7 ;
                STYDYE.RA8   WITH STYDYE.RA8 - RALINE.QTY8 ;
                STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8 ;
                IN STYDYE
              *-- Add the modified record to the CursorUpdate
              THIS.STYDYE.REPLACE()
            ENDIF

            *-- If the system & style dyelot yes, Update the Ra values for each
            *-- style+warehouse+dyelot in the style dyelot file.
            *Make sure the style uses Dylots
            IF THIS.LLDYELOT AND !EMPTY(EVALUATE(THIS.LCTMPRETLN+'.Dyelot')) .AND. THIS.STYDYE.SEEK(EVALUATE(THIS.LCTMPRETLN+'.Style') + RETAUTH.CWARECODE + EVALUATE(THIS.LCTMPRETLN+'.Dyelot'))
              REPLACE STYDYE.RA1   WITH STYDYE.RA1 - RALINE.QTY1 ;
                STYDYE.RA2   WITH STYDYE.RA2 - RALINE.QTY2 ;
                STYDYE.RA3   WITH STYDYE.RA3 - RALINE.QTY3 ;
                STYDYE.RA4   WITH STYDYE.RA4 - RALINE.QTY4 ;
                STYDYE.RA5   WITH STYDYE.RA5 - RALINE.QTY5 ;
                STYDYE.RA6   WITH STYDYE.RA6 - RALINE.QTY6 ;
                STYDYE.RA7   WITH STYDYE.RA7 - RALINE.QTY7 ;
                STYDYE.RA8   WITH STYDYE.RA8 - RALINE.QTY8 ;
                STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8 ;
                IN STYDYE
              *-- Add the modified record to the CursorUpdate
              THIS.STYDYE.REPLACE()
            ENDIF
          ENDIF
        ENDIF
        *-- Calculate the total canceled qty.
        *lnTotalCan = lnTotalCan + (nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8)
      ENDCASE
    ELSE
      *Detect addng open qty. for the deleted lines in the add mode.
      IF !DELETED() .OR. !THIS.LOFORMSET.ACTIVEMODE="A"
        *-- Calculate the total open qty.
        LNTOTALOPN = LNTOTALOPN + (NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8)
      ENDIF
    ENDIF
    *B608545,1 WAM 05/08/2008 Don't generate new line# for modified lines [T20080408.0002]
    *!*	    IF cStatus <> "D"
    *!*	      *-- Save the R/A line #.
    *!*	      REPLACE cRa_LinNo WITH ALLTRIM(STR(This.lnRa_LinNo))
    *!*	      *-- Adjust the line no. for each line.
    *!*	      This.lnRa_LinNo = This.lnRa_LinNo + 1
    *!*	    ENDIF
    IF CSTATUS = "A"
      *-- Adjust the line no. for each line.
      THIS.LNRA_LINNO = THIS.LNRA_LINNO + 1
      *-- Save the R/A line #.
      REPLACE CRA_LINNO WITH ALLTRIM(STR(THIS.LNRA_LINNO))
    ENDIF
    *B608545,1 WAM 05/08/2008 (End)

  ENDSCAN

  *-- If the thermometer was not closed, call global function to close it.
  *Don't call gfTherm if the total record is 0
  IF LNCURREC # LNTOTREC AND LNTOTREC > 0
    *Old: =gfTherm(lnTotRec , lnTotRec , "Saving return authorization # : "+RetAuth.rano)
  ENDIF

  SET DELETE &LCSAVDELETE

  *-- Update the lines file.
  THIS.GFTMP2MAST(THIS.RALINE , THIS.LCTMPRETLN , 'Saving the lines for R/A # : ' + RETAUTH.RANO + ' ...')

  *-- Clear the R/A temp. lines file.
  SELECT (THIS.LCTMPRETLN)
  ZAP

  *-- Update the header file with the opened, canceled & budget qty.
  SELECT RETAUTH
  *REPLACE nRetA_Can WITH nRetA_Can + lnTotalCan ;
  nRetA_Opn WITH lnTotalOpn ;
  nRetA_Bud WITH nRetA_Can + nRetA_Opn
  REPLACE NRETA_OPN WITH LNTOTALOPN ;
    NRETA_BUD WITH NRETA_CAN + NRETA_OPN

  IF !THIS.LFSAVEFILES()
    MESSAGEBOX('Error While Saving')
  ENDIF

  *-- Inform the user with the return authorization no.
  IF THIS.LOFORMSET.ACTIVEMODE="A"
    *** Return Authorization has been saved as : {RetAuth.rano}. ***
    *** <   OK   > ***
    =GFMODALGEN('INM46029B00000','DIALOG' , RETAUTH.RANO)
    THIS.LOFORMSET.LCSYDKEY = "Z'"+RETAUTH.RANO+"'"
  ENDIF
  *-- if using audit trail then call the method to add the audit trail record
  *-- with the spedified triger that occured
  IF THIS.LOFORMSET.AUDITTRAIL
    LCEVENT = IIF(LLCALLEDFROMDELETE,"DELETE",IIF(THIS.LOFORMSET.ACTIVEMODE="A",'INSERT','UPDATE'))
    THIS.LOFORMSET.ADDAUDIT(LCEVENT)
  ENDIF


  THIS.RETAUTH.SEEK(RETAUTH.RANO)
  IF THIS.LOFORMSET.HASNOTES AND THIS.LOFORMSET.ACTIVEMODE="A" AND  GFMODALGEN('QRM00349B00006','DIALOG')=1
    THIS.LOFORMSET.CALLNOTEPAD()
    *IF !EMPTY(This.loFormSet.FormHasToolBar)
    *oAriaApplication.oToolBar.Init(This.loFormSet)
    *ENDIF
  ENDIF
  THIS.LOFORMSET.ACTIVEMODE = 'V'
  THIS.LOFORMSET.REFRESH()


  ENDFUNC
  *--end of lpSavScr


  *!*************************************************************
  *! Name      : lfSaveFiles
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Save all the edited files
  *!*************************************************************
  *! Parameters:  None
  *!*************************************************************
  *! Return    : True / False
  *!*************************************************************
  FUNCTION LFSAVEFILES
  LOCAL LCTRANCODE, LLRETVAL
  LCTRANCODE = OARIAAPPLICATION.REMOTECOMPANYDATA.BEGINTRAN(OARIAAPPLICATION.ACTIVECOMPANYCONSTR,3,'')
  IF TYPE('lcTranCode') = 'N'
    =OARIAAPPLICATION.REMOTECOMPANYDATA.CHECKRETRESULT("BeginTran",LCTRANCODE,.T.)
    RETURN .F.
  ENDIF
  LLRETVAL = THIS.STYLE.TABLEUPDATE(LCTRANCODE) AND THIS.STYDYE.TABLEUPDATE(LCTRANCODE) ;
    AND (IIF(THIS.LLCRMINS,THIS.CRMESAG.TABLEUPDATE(LCTRANCODE),.T.)) AND ;
    THIS.RETAUTH.TABLEUPDATE(LCTRANCODE) AND THIS.RALINE.TABLEUPDATE(LCTRANCODE)
  IF LLRETVAL
    OARIAAPPLICATION.REMOTECOMPANYDATA.COMMITTRAN(LCTRANCODE)
  ELSE
    OARIAAPPLICATION.REMOTECOMPANYDATA.ROLLBACKTRAN(LCTRANCODE)
  ENDIF

  RETURN LLRETVAL
  ENDFUNC

  *!*************************************************************
  *! Name      : lfActPad
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Activate a new pad with the Option
  *!*************************************************************
  *! Parameters:  None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFACTPAD

  DEFINE PAD _OPTION OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _OPTION OF _MSYSMENU ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
  DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Approve R/A'   SKIP FOR (LADATA[4] <> 'E')OR LASCRMODE[1] OR LASCRMODE[3] OR LASCRMODE[4]

  ON SELECTION POPUP _OPTIONPOP DO LPOPTIONS WITH BAR()
  ON KEY LABEL ALT+P ACTIVATE POPUP _OPTIONPOP

  ENDFUNC
  *--end of lfActPad


  *!*************************************************************
  *! Name      : lpOptions
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To define the actions done when selecting one of the option bars
  *!*************************************************************
  *! Parameters:  lnBar --> Bar #
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  PROCEDURE LPOPTIONS
  PARAMETERS LNBAR
  DO CASE
  CASE LNBAR = 1
    IF GFMODALGEN("QRM00002B00006","ALERT",'','','Are you sure you want to approve this R/A ?') = 1
      =LFAPPROV()
    ENDIF
  ENDCASE

  ENDFUNC
  *--end of lpOptions


  *!*************************************************************
  *! Name      : lfApprov
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Validate approve for RA
  *!*************************************************************
  *! Parameters:  None
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION LFAPPROV
  PRIVATE LNCURRALS

  LNCURRALS = SELECT()
  *--Change to Open
  SELECT RETAUTH
  LCMTO = IIF(SEEK('M' + RETAUTH.ACCOUNT,'Customer'), EVALUATE('Customer.' + GFGETMEMVAR('M_CONFMAIL')) ,'')
  LCLETTERID  = GFGETMEMVAR('M_RAAPR' , GCACT_COMP)

  IF EMPTY(LCMTO)
    *The Confirmation E-mail Address on the setup screen is blank. We can not Approve your R/M.
    *<OK>
    =GFMODALGEN('TRM46039B00000','DIALOG')
    SELECT (LNCURRALS)
    RETURN
  ENDIF

  IF EMPTY(LCLETTERID)
    *The Approved ID on the setup screen is blank. Are you sure you want to continue?.
    *<YES> , <NO>
    IF GFMODALGEN('QRM46040B32000','DIALOG') = 2
      SELECT (LNCURRALS)
      RETURN
    ENDIF
  ENDIF

  REPLACE RETAUTH.STATUS   WITH "O",;
    RETAUTH.LFROMWEB WITH .F.
  LADATA[4]= "O"
  LCSTATUS = LASTATUS[1]
  SHOW GET LCSTATUS

  THIS.LFOPENFILE(GCDATADIR+'STYDYE',GCDATADIR+'STYDYE')
  SELECT RALINE
  =SEEK(LADATA[1])
  SCAN FOR RANO = LADATA[1]
    =SEEK(RALINE.STYLE , 'Style')
    IF STYLE.LINVSTY
      *-- Update the Ra values in the style file.
      REPLACE STYLE.RA1   WITH STYLE.RA1 + QTY1 ;
        STYLE.RA2   WITH STYLE.RA2 + QTY2 ;
        STYLE.RA3   WITH STYLE.RA3 + QTY3 ;
        STYLE.RA4   WITH STYLE.RA4 + QTY4 ;
        STYLE.RA5   WITH STYLE.RA5 + QTY5 ;
        STYLE.RA6   WITH STYLE.RA6 + QTY6 ;
        STYLE.RA7   WITH STYLE.RA7 + QTY7 ;
        STYLE.RA8   WITH STYLE.RA8 + QTY8 ;
        STYLE.TOTRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8
      *-- Update the Ra values for each style+warehouse with the canceled qty.
      IF SEEK(&LCTMPRETLN..STYLE + LADATA[7] + SPACE(10) , 'STYDYE')
        REPLACE STYDYE.RA1   WITH STYDYE.RA1 + QTY1 ;
          STYDYE.RA2   WITH STYDYE.RA2 + QTY2 ;
          STYDYE.RA3   WITH STYDYE.RA3 + QTY3 ;
          STYDYE.RA4   WITH STYDYE.RA4 + QTY4 ;
          STYDYE.RA5   WITH STYDYE.RA5 + QTY5 ;
          STYDYE.RA6   WITH STYDYE.RA6 + QTY6 ;
          STYDYE.RA7   WITH STYDYE.RA7 + QTY7 ;
          STYDYE.RA8   WITH STYDYE.RA8 + QTY8 ;
          STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
      ENDIF
      *B606480,1 (Begin) Make sure the style uses Dylots
      *IF llDyelot .AND. SEEK(&lcTmpRetLn..Style + laData[7] + &lcTmpRetLn..Dyelot , 'STYDYE')
      IF LLDYELOT AND !EMPTY(&LCTMPRETLN..DYELOT) .AND. SEEK(&LCTMPRETLN..STYLE + LADATA[7] + &LCTMPRETLN..DYELOT , 'STYDYE')
        *B606480,1 (End)

        REPLACE STYDYE.RA1   WITH STYDYE.RA1 + QTY1 ;
          STYDYE.RA2   WITH STYDYE.RA2 + QTY2 ;
          STYDYE.RA3   WITH STYDYE.RA3 + QTY3 ;
          STYDYE.RA4   WITH STYDYE.RA4 + QTY4 ;
          STYDYE.RA5   WITH STYDYE.RA5 + QTY5 ;
          STYDYE.RA6   WITH STYDYE.RA6 + QTY6 ;
          STYDYE.RA7   WITH STYDYE.RA7 + QTY7 ;
          STYDYE.RA8   WITH STYDYE.RA8 + QTY8 ;
          STYDYE.TOTRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
      ENDIF
    ENDIF
  ENDSCAN
  *--add record to CRMESAG
  *--get variable from CRM module setups
  LCMFROM     = GFGETMEMVAR('M_NOTEMAIL' , GCACT_COMP)
  LCMSGSBJCT  = 'Your R/A Request #:'+ LADATA[1] + 'has been approved.'
  *lcMto       = IIF(SEEK('M'+RetAuth.Account,'Customer'),EVALUATE('Customer.' + gfGetMemVar('M_CONFMAIL')),'')
  *lCLetterid  = gfGetMemVar('M_RAAPR' , gcAct_Comp)
  SELECT CRMESAG
  APPEND BLANK
  =GFADD_INFO('CRMesag')
  REPLACE CTRANSTYPE WITH 'R',;
    CTRANSNO   WITH LADATA[1],;
    LAPPROVE   WITH .T.,;
    CMAILFROM  WITH LCMFROM,;
    CMAILTO    WITH LCMTO ,;
    CMSGSUBJCT WITH LCMSGSBJCT,;
    CLETTERID  WITH LCLETTERID,;
    LSENT      WITH .F.
  SELECT (LNCURRALS)

  ENDFUNC
  *--end of lfApprov



  *!*************************************************************
  *! Name      : lfOpenFile
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To Open a Table Remotely
  *!*************************************************************
  *! Parameters:  lcFile, lcTag
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  PROCEDURE LFOPENFILE
  PARAMETERS LCFILE, LCTAG
  LOCAL LCPROP

  LCFILE = JUSTSTEM(LCFILE)
  LCTAG = JUSTSTEM(LCTAG)
  LCPROP = LCFILE
  IF !PEMSTATUS(THIS,LCPROP,5)
    THIS.ADDPROPERTY(LCPROP)
  ENDIF
  LCPROP = 'This.'+LCPROP

  IF TYPE(LCPROP)<>'O'
    &LCPROP = CREATEOBJECT("RemoteTable",LCFILE,LCTAG,LCFILE,THIS.LOFORMSET.DATASESSIONID)
  ELSE
    &LCPROP..SETORDER(LCTAG)
  ENDIF

  ENDFUNC
  *--end of lfOpenFile




  *!*************************************************************
  *! Name      : gfTmp2Mast
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 04/26/2005
  *! Purpose   : To update master file from a temp one
  *!*************************************************************
  *! Parameters: Master file Remote Table Object
  *!             Temp file name
  *!             Fixed message in thermo
  *!             variable message in thermo
  *!*************************************************************
  *! Return    : None
  *!*************************************************************
  FUNCTION GFTMP2MAST
  PARAMETERS LOMASTTABLE, LCTEMPFILE, LCFXDMSG, LCVARMSG
  PRIVATE LCMASTFILE, LCTEMPFILE, LCSAVALIAS, LCFXDMSG, LCVARMSG

  LOTOOLBARWINDOW = OARIAAPPLICATION.OTOOLBAR.OWINDPARENT
  *-- Include the .H file
  #INCLUDE R:\ARIA4XP\PRGS\SY\GFTMP2MAST.H

  LCSAVALIAS = SELECT(0)
  LCFXDMSG   = IIF(TYPE('lcFxdMsg')<>'C',LANG_SAVE,LCFXDMSG )
  LCVARMSG   = IIF(TYPE('lcVarMsg')<>'C',' ',LCVARMSG)

  LCSAVEDEL = SET ('DELETE')
  SET DELETE OFF

  *-- Initialize the progress bar needed variables.
  OPROGRESS = NEWOBJECT('ariaprogressbar',OARIAAPPLICATION.CLASSDIR+'utility.vcx')
  OPROGRESS.TOTALPROGRESS = RECCOUNT(LCTEMPFILE)
  OPROGRESS.LBLFIRSTLABEL.CAPTION = LCFXDMSG
  OPROGRESS.SHOW()
  LNCURRECORD  = 1

  SELECT (LCTEMPFILE)
  GO TOP
  *-- Scan through all the Added,Modified or Deleted records
  SCAN FOR CSTATUS <> 'S'
    *-- Call the progress bar.
    OPROGRESS.LBLSECONDLABEL.CAPTION = LCVARMSG
    OPROGRESS.CURRENTPROGRESS(LNCURRECORD)
    LNCURRECORD = LNCURRECORD + 1

    DO CASE
      *-- New added record
    CASE CSTATUS = 'A'
      SCATTER MEMVAR MEMO
      SELECT (LOMASTTABLE.LCCURSORUPDATE)
      APPEND BLANK
      GATHER MEMVAR MEMO

      *-- Record was modified
    CASE CSTATUS = 'M'
      SCATTER MEMVAR MEMO                 && Collect data from temp
      LOMASTTABLE.GOREC(REC_NO)
      LOMASTTABLE.REPLACE()
      SELECT (LOMASTTABLE.LCCURSORUPDATE)
      GATHER  MEMVAR MEMO                 && Replace master data

      *-- Record was deleted
    CASE CSTATUS = 'D' .AND.  DELETED()
      LOMASTTABLE.GOREC(REC_NO)
      LOMASTTABLE.DELETE()                 && Delete recored not in temp
    ENDCASE

    SELECT  (LCTEMPFILE)
    REPLACE CSTATUS WITH "S"
  ENDSCAN
  *-- Terminate the progress bar
  OPROGRESS=NULL
  OARIAAPPLICATION.OTOOLBAR.OWINDPARENT = LOTOOLBARWINDOW

  SET DELETE &LCSAVEDEL
  SELECT (LCSAVALIAS)

  ENDFUNC
  *--end of gfTmp2Mast


  FUNCTION GFSEQUENCE
  PARAMETERS LCSEQTYPE,LCCOMPANYID,LCGROUPID,LCDIVISION,LCFIELD
  LOCAL LCSAVALIAS, LNRETVAL

  LNRETVAL = 1
  LCSAVALIAS = SELECT(0)
  IF !USED('SEQUENCE')
    LUSEQUENCE = .T.
    USE (OARIAAPPLICATION.DATADIR+"SEQUENCE") IN 0 ORDER TAG 'cSeq_Type'
  ELSE
    SELECT SEQUENCE
    SET ORDER TO TAG CSEQ_TYPE IN SEQUENCE
  ENDIF

  SELECT SEQUENCE
  IF SEEK(PADR(LCSEQTYPE,10),'SEQUENCE')
    LNRETVAL = NSEQ_NO + 1
    REPLACE NSEQ_NO WITH IIF(LNRETVAL > 999999,0,LNRETVAL)
    LNRETVAL = NSEQ_NO
  ENDIF

  SELECT (LCSAVALIAS)

  RETURN PADL(LNRETVAL,6,'0')
  ENDFUNC

  *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[Start]
  *!*************************************************************
  *! Name      : lfFillEmpArr
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 09/23/2010
  *! Purpose   : To Fill Employee Array Based on Selected customer/Store
  *!*************************************************************
  FUNCTION LFFILLEMPARR
  PARAMETERS LOPARFORMSET,LCEMPACCOUT,LCEMPSTORE
  LNOLDSEL = SELECT()
  DIMENSION LOPARFORMSET.LAEMPLOYEE[1,2]
  LOPARFORMSET.LAEMPLOYEE= SPACE(12)
  IF !USED('Contact_EMP')
    =GFOPENTABLE('Contact','Contact','SH','Contact_EMP')
  ENDIF
  SELECT CONTACT_EMP
  =GFSEEK('C'+PADR(LCEMPACCOUT,8)+IIF(TYPE('lcEmpStore') ='C' AND !EMPTY(LCEMPSTORE),PADR(LCEMPSTORE,8),''))
  *B610626,1 TMI 12/15/2013 14:22 [Start] use a select into array instead of a scan loop
  *SCAN REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = ;
  *    'C'+PADR(LCEMPACCOUT,8)+IIF(TYPE('lcEmpStore') ='C' AND !EMPTY(LCEMPSTORE),PADR(LCEMPSTORE,8),'') FOR !EMPTY(CCNTCTCODE)
  *  IF ASCAN(LOPARFORMSET.LAEMPLOYEE,CCNTCTCODE,1,0,2) = 0
  *    DIMENSION LOPARFORMSET.LAEMPLOYEE[ALEN(loParFormSet.LAEmployee,1)+1,2]
  *    LOPARFORMSET.LAEMPLOYEE[ALEN(loParFormSet.LAEmployee,1),1] = ALLTRIM(CCNTCTCODE)+" - "+CONTACT
  *    LOPARFORMSET.LAEMPLOYEE[ALEN(loParFormSet.LAEmployee,1),2] = CCNTCTCODE
  *  ENDIF
  *ENDSCAN
  LOCAL lcWh
  lcWh = 'C'+PADR(LCEMPACCOUT,8) + ;
         IIF(TYPE('lcEmpStore') ='C' AND !EMPTY(LCEMPSTORE),PADR(LCEMPSTORE,8),'') 
  SELECT DISTINCT ALLTRIM(CCNTCTCODE)+" - "+CONTACT,CCNTCTCODE FROM CONTACT_EMP INTO ARRAY LOPARFORMSET.LAEMPLOYEE ;
         WHERE !EMPTY(CCNTCTCODE) AND ;
         CCONTTYPE+CCONT_ID+STORE+CONTACT = lcWh
  *B610626,1 TMI 12/15/2013 14:29 [End  ] 
  =ASORT(LOPARFORMSET.LAEMPLOYEE)
  LOPARFORMSET.ARIAFORM1.PAGEFRAME.PAGE2.CBOEMPL.REQUERY()
  SELECT (LNOLDSEL)
  *!*************************************************************
  *! Name      : lfvEmployee
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 09/23/2010
  *! Purpose   : To Validate Employee Field
  *!*************************************************************
  FUNCTION LFVEMPLOYEE

  *-- Update the R/A lines temp. file.
  REPLACE CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS) IN (THIS.LCTMPRETLN)

  RETURN 1
  ENDFUNC
  *! E302766,1 MMT 09/23/2010 Add Employee Field to the Return Authorization screen[ENd]

  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[Start]
  *!*************************************************************
  *! Name      : lfOrderBrowse
  *! Developer : Saber Saber(SAB)
  *! Date      : 06/08/2011
  *! Purpose   : To Browse Order Header Using Filter Criteria
  *!*************************************************************
  FUNCTION LFORDERBROWSE
  LPARAMETERS LCACCOUNT, LCXORDER
  LOCAL LCSELECT, LCFILTERSETTING, LCSELECTEDINDEX

  *-- Include the .H file
  #INCLUDE R:\ARIA4XP\PRGS\SY\ORDBROW.H

  LCSELECT = ALIAS(SELECT(0))
  SELECT ORDHDR
  LCSELECTEDINDEX = SET("Order")
  LCFILTERSETTING = SET("Filter")

  SET ORDER TO ORDHDR   && CORDTYPE+ORDER
  LOCAL LCFILTEREXP
  LCFILTEREXP = "CORDTYPE = 'O' AND INLIST(ORDER,"
  FOR I = 1 TO ALEN(THIS.LACONSINVORDRS,1)
    LCFILTEREXP = LCFILTEREXP + "'" + THIS.LACONSINVORDRS[i,1] + "',"
  ENDFOR
  LCFILTEREXP = SUBSTR(LCFILTEREXP,1,LEN(LCFILTEREXP)-1) + ")"
  SET FILTER TO &LCFILTEREXP.


  *Calling Function
  LCBROWTITL = 'Orders'
  LCBRFIELDS = [Order:H="]+LANG_LABELORDER+[",status:H="]+LANG_LABELSTATUS+[",lcSesDesc=gfCodDes(Season,'SEASON'):H="]+LANG_LABELSEASON+[",lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="]+LANG_LABELDIVISION+[",]+;
    [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="]+LANG_LABELCUSTPO+[",]+;
    [ACCOUNT:H="]+LANG_LABELACCOUNT+[",store=IIF(MULTI='Y','*Multi*',STORE):H="]+LANG_LABELSTORE+[",Customer.stname]+;
    [:H="]+LANG_LABELNAME+[",Open:H="]+LANG_LABELOPEN+[",OpenAmt:H="]+LANG_LABELOPENAMT+[",Ship:H="]+LANG_LABELSHIP+[",Shipamt:H="]+LANG_LABELSHIPAMT+[",]+;
    [start:H="]+LANG_LABELSTART+[",Complete:H="]+LANG_LABELCOMPLETE+[",Note1:H="]+LANG_LABELNOTE+["]
  DIMENSION LABROWARR[1]
  LABROWARR = ''
  IF ARIABROW("", 'Orders', .F., .F., .F., .F., .F., .F., "ORDER", "laBrowArr", .T., .F.,.F., 'ORDHDR')
    LCORDER = LABROWARR[1]
  ELSE
    STORE SPACE(6)  TO LCORDER
  ENDIF





  SELECT ORDHDR
  SET ORDER TO &LCSELECTEDINDEX.
  SET FILTER TO &LCFILTERSETTING.
  SELECT &LCSELECT.

  RETURN LCORDER
  ENDFUNC
  *! B609603,1 SAB 06/05/2011 Consolidated Invoice Case not checking order and not loading employee info[End]

ENDDEFINE


