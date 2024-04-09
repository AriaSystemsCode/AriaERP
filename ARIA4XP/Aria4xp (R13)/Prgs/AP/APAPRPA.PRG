***********************************************************************
*:  Program File: APAPRPA.prg
*:  Desc.       : Approve for Payment
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 12/13/2011
*:  Reference   : *E303011,1
*:************************************************************************
*E303011,1 TMI 12/22/2011 [Start] refer to the local until the entry closes
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004]
*!B609890,1 SAB 04/26/2012 Diabled buttons after saving problemFix Forign [T20120304.0004]
*!B609971,1 MMT 06/21/2012 Fix bug of Approve for payment screen open scope OG after saving[T20120511.0006]
*!B610215,1 HIA 01/23/2013 Aria4xp - AP - Approve for payment bank account issue [T20130107.0001]
*!B610789,1 MMT 08/03/2014 Remove Division field from Grid as it slows down the screen[T20140729.0008]
*!B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012]
*!E303922,1 AHH 01/29/2018 Aria 4XP - A report shows all the 1099 in details forms [T20171208.0136]
*!B611697,1 HMS 11/19/2018 -Aria 5 - Issue with approving a payment[T20181116.0004 ]
*!B611740,1 ES 3/12/2019 Partially Approve screen has no decimals in 1099 and adjustment amounts in Approve for payment screen [T20190308.0004]
*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001]
*!B612542,1 MMT 03/31/2022 Approve for payment screen UNDO method unlocks locked records locked by another program[T20220128.0001]
*:************************************************************************
#INCLUDE R:\aria4xp\PRGS\ap\APAPRPA.H
Do Form (oAriaApplication.ScreenHome+"\AP\APAPRPA.SCX")

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : E303011,1 TMI 12/13/2011
*! Purpose   : called from the Screen init Method
*!*************************************************************
Function lfFormInit
Parameters loFormSet

Set Multilocks On
*- Open tables
=gfOpenTable('APSETUP','APSETUP','SH')
=gfOpenTable('ACCOD','ACCSEGNO','SH')
=gfOpenTable('APDIV','DIVISION','SH')
=gfOpenTable('apvendor','vencode','SH')
=gfOpenTable('apinvhdr','VENDINV','SH')
=gfOpenTable('APVENHST','VENDYEAR','SH')
=gfOpenTable('APDIST','INVVEND','SH')
=gfOpenTable('APCHECKS','BANKCHECK','SH')   && CBNKCODE+CCHKACCT
*E303011,1 TMI 01/29/2012 [Start]
=gfOpenTable(oAriaApplication.SysPath + 'SYCCURR' , 'CCURRCODE' , 'SH')
*E303011,1 TMI 01/29/2012 [End  ]

*- initializations
With loFormSet
	.nWorkArea = 'APINVHDR'
	.otoolbar.nWorkArea = 'APINVHDR'
	.DataEnvironment.InitialSelectedAlias = 'APINVHDR'
Endwith

*- Define variables
=lfDefineVars(loFormSet)

*-- Define custom tool bar buttons
Declare loFormSet.lapanelobj[1,6]
Store '' To loFormSet.lapanelobj
*-- Scope Button
loFormSet.lapanelobj[1,1] = 'pbScop'
loFormSet.lapanelobj[1,2] = oAriaApplication.BitMapHome+"SCOPE.BMP"
loFormSet.lapanelobj[1,3] = 'lfGetData'
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
loFormSet.lapanelobj[1,4] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_OPTIONGRID,loFormSet.GetHeaderText("LANG_APAPRPA_OPTIONGRID",loFormSet.HeaderAlias))
loFormSet.lapanelobj[1,5] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_OPTIONGRID,loFormSet.GetHeaderText("LANG_APAPRPA_OPTIONGRID",loFormSet.HeaderAlias))
*N000682,1 MMT 12/09/2012 Globalization changes[END]
loFormSet.lapanelobj[1,6] = 'V'

*- Variable holding Filter expresion
lcExprsion = "((APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ) <> 0)"

*!*    *** Prepare an array to hold bank objects to be used for
*!*    *** global bank and checking accounts validations as follows :
*!*    *** One row for every object, such that
*!*    *** row no. 1 holds bank object names,
*!*    *** row no. 2 holds checking account object names
*!*    *** row no. 3 holds the corresponding G/L account object names,
*!*    *** Columns are ordered as follows :
*!*    *** Column no. 1 : invisible button name for corresponding object
*!*    *** Column no. 2 : object name (e.g. bank object name)
*!*    *** Column no. 3 : object description name(if required)
*!*    laBankObjs  = ' '
*!*    laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
*!*    laBankObjs[1,2] = 'lcBankCode'     && Bank code
*!*    laBankObjs[2,1] = 'ibChecks'       && Checking account invisible button
*!*    laBankObjs[2,2] = 'lcCheckCode'     && Checking account
*!*    laBankObjs[3,1] = 'ibGlAcc'        && G/L account invisible button
*!*    laBankObjs[3,2] = 'lcGlAcct'     && G/L account
*!*
*!*  ELSE && If window exist
*!*    rbScopeOn=lnScopeOn  && restoring radio button value.
*!*    lcMethod   = lcOMethod
*!*    rbScope    = lnOScope
*!*    rbSort     = lnOSort
*!*    IF rbScope = 1
*!*      lcExprsion = "((APINVHDR.NINVAMNT -APINVHDR.NINVPAID - "+;
*!*                    "APINVHDR.NINVDISTK -APINVHDR.NINVADJ) <> 0)"

*!*    ELSE
*!*      =lfvOk() && returning the existing browse filter.
*!*    ENDIF
*!*  ENDIF

*!*  *** store scape trap.
*!*  lcEscTrp=ON("KEY","ESC")
*!*  *** Trap some keys before browse. ***
*!*  PUSH KEY
*!*  =lfPushKey()
*E303011,1 TMI 12/21/2011 [End  ]

Select CODES
=lfAddProp(loFormSet,'lcCodeFilt','CDIVISION ')
lcCodeFilt = loFormSet.lcCodeFilt
Set Filter To (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'&lcCodeFilt') Or;
	(CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'N/A') Or;
	(CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'ALL')

*- Set relation between invoice header, vendor, and division file
Select APVENHST
Set Order To Tag VENDYEAR

Select APVENDOR
Set Order To Tag VENCODE

Select APDIV
Set Order To Tag DIVISION

Select APINVHDR
Set Order To Tag VENDINV
lcCurYear=loFormSet.lcCurYear
Set Relation To APINVHDR.CVENDCODE+'&lcCurYear' Into APVENHST Additive

Set Relation To APINVHDR.CVENDCODE           Into APVENDOR Additive
Set Relation To APINVHDR.CDIVISION           Into APDIV Additive

lcSavOrd = Order()
Set Order To 0
Set Filter To APINVHDR.CVENDCODE+APINVHDR.CINVNO = '' .And.;
	CINVSTAT<>'V' .And.  Evaluate(lcExprsion)
Set Order To &lcSavOrd
Go Top    &&Go to the top of the file.

*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to approved currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
* lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,APINVHDR.cAprCurCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
If APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
	lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cAprCurCod,APINVHDR.cCurrCode)
	lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
	lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
Else   && Else
	lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,APINVHDR.cAprCurCod)
Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')

glQuitting = .F.
If Eof()
** MESSAGE : "There are no open invoices."
**           "        ® Ok ¯            "
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*=gfModalGen("TRM04101B00000","DIALOG",'open invoices')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM04101B00000","DIALOG",LANG_APAPRPA_OPENINVOICES)
	=gfModalGen("TRM04101B00000","DIALOG",Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_OPENINVOICES,loFormSet.GetHeaderText("LANG_APAPRPA_OPENINVOICES",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[eND]
	glQuitting=.T.
Else
	If Empty(APSETUP.CBNKCODE) .Or. Empty(APSETUP.CCHKACCT)
** MESSAGE : "You have to enter default bank code and checking account"
**           "in the accounts payable setup. "
**           "            ® Ok ¯             "
		=gfModalGen("TRM04103B00000","DIALOG")
		glQuitting=.T.
	Else
*,1 Call *.SPR from screens directory
* DO APAPRPA.SPR
*E303011,1 TMI 12/21/2011 [Start]
*DO (gcScrDir + gcWinAppl + '\APAPRPA.SPR')
*E303011,1 TMI 12/21/2011 [End  ]
*,1 end

	Endif
Endif

Select CODES
Set Filter To

Select APINVHDR

*** Clear relation ***
Set Filter To
Set Relation To

*
***Release the browse window.
*RELEASE WINDOW (lcBrTtl)

If glQuitting
*!*    RELEASE WINDOW (lcVenSum)
*!*    RELEASE WINDOW (lcInvSum)
*!*    RELEASE POPUPS puDivision
	Return .F.
Endif
glFromBrow  = .F.

*- Create the temp table that holds the invoices that are ready to be approved
=lfAddProp(loFormSet,'lcAPINVHDR',gfTempName())
Local laStru
Dimension laStru[1,18]
Select APINVHDR
=Afields(laStru)

Dimension laIndex[2,2]
laIndex[1,1] = 'CINVNO+CVENDCODE'
laIndex[1,2] = 'INVVEND'
laIndex[2,1] = 'CVENDCODE+CINVNO'
laIndex[2,2] = 'VENDINV'
=gfCrtTmp(loFormSet.lcAPINVHDR,@laStru,@laIndex,loFormSet.lcAPINVHDR)

*- add property to create in it the class to call the vendor/ap invoice  screen
=lfAddProp(loFormSet,'clsVendCall' ,'')
loFormSet.clsVendCall = Createobject('clsVendCall',loFormSet.lcAPINVHDR)

* Browse fields list is :
* CINVNO,CVENDCODE ,CVENPRIOR ,NINVAMTAP  ,NINVDISAP   ,NINVADJAP  ,NINVA1099,NINVAMNT,CVENPMETH,DINVDATE,DINVDUDAT  ,NTERDISCD  ,CDIVISION,CBNKCODE   ,CCHKACCT   ,CCHKGLACC
With loFormSet.ariaform1.grdAPROVE
	.RecordSource = ''
	.RecordSource = loFormSet.lcAPINVHDR
	Local lcAPINVHDR
	lcAPINVHDR = loFormSet.lcAPINVHDR

	.Column1.ControlSource = '&lcAPINVHDR..CINVNO'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column1.Header1.Caption = LANG_APAPRPA_Inv_No           &&"Inv. No."
	.Column1.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Inv_No,loFormSet.GetHeaderText("LANG_APAPRPA_Inv_No",loFormSet.HeaderAlias))           &&"Inv. No."
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column2.ControlSource = ''
*!*    .Column2.RemoveObject('Text1')
*!*    .Column2.AddObject('Command1','commandbutton')
*!*    .Column2.Sparse = .F.
*!*    .Column2.CurrentControl = 'Command1'
	Bindevent(.Column2.Command1,"Click",loFormSet.clsVendCall,"lfInvoice")

	.Column3.ControlSource = '&lcAPINVHDR..CVENDCODE'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column3.Header1.Caption = LANG_APAPRPA_Vendor           &&"Vendor"
	.Column3.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Vendor,loFormSet.GetHeaderText("LANG_APAPRPA_Vendor",loFormSet.HeaderAlias))           &&"Vendor"
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column4.ControlSource = ''
*!*    .Column4.RemoveObject('Text1')
*!*    .Column4.AddObject('Command1','commandbutton')
*!*    .Column4.Sparse = .F.
*!*    .Column4.CurrentControl = 'Command1'
	Bindevent(.Column4.Command1,"Click",loFormSet.clsVendCall,"lfVendor")

	.Column5.ControlSource = '&lcAPINVHDR..CVENPRIOR'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column5.Header1.Caption = LANG_APAPRPA_Priority         &&"P"
	.Column5.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Priority,loFormSet.GetHeaderText("LANG_APAPRPA_Priority",loFormSet.HeaderAlias))         &&"P"
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column6.ControlSource = '&lcAPINVHDR..NINVAMTAP'  &&
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column6.Header1.Caption = LANG_APAPRPA_Appr_to_pay      &&"Appr. to pay"
	.Column6.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Appr_to_pay,loFormSet.GetHeaderText("LANG_APAPRPA_Appr_to_pay",loFormSet.HeaderAlias))      &&"Appr. to pay"
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column7.ControlSource = '&lcAPINVHDR..NINVDISAP'  &&"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column7.Header1.Caption = LANG_APAPRPA_Appr_Disc        &&"Appr. disc."
	.Column7.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Appr_Disc,loFormSet.GetHeaderText("LANG_APAPRPA_Appr_Disc",loFormSet.HeaderAlias))        &&"Appr. disc."
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column8.ControlSource = '&lcAPINVHDR..NINVADJAP'  &&
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column8.Header1.Caption = LANG_APAPRPA_Appr_adj         &&"Appr. adj."
	.Column8.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Appr_adj,loFormSet.GetHeaderText("LANG_APAPRPA_Appr_adj",loFormSet.HeaderAlias))         &&"Appr. adj."
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column9.ControlSource = Iif(loFormSet.ap1.llApS1099,'','&lcAPINVHDR..NINVA1099')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column9.Header1.Caption = LANG_APAPRPA_Approved_1099    &&"Approved 1099"
	.Column9.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Approved_1099,loFormSet.GetHeaderText("LANG_APAPRPA_Approved_1099",loFormSet.HeaderAlias))    &&"Approved 1099"
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column10.ControlSource = '&lcAPINVHDR..NINVAMNT - &lcAPINVHDR..NINVPAID - &lcAPINVHDR..NINVDISTK - &lcAPINVHDR..NINVADJ'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column10.Header1.Caption = LANG_APAPRPA_Open_Amount      &&"Open amount"
	.Column10.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Open_Amount,loFormSet.GetHeaderText("LANG_APAPRPA_Open_Amount",loFormSet.HeaderAlias))      &&"Open amount"
*N000682,1 11/20/2012 MMT Globlization changes[End]

*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[Start]
*.Column11.ControlSource = 'lfGetPayMeth(Thisformset)'
	.Column11.ControlSource = 'Thisformset.lfGetAPRPayMeth()'
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[End]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column11.Header1.Caption = LANG_APAPRPA_Pay_Meth         &&"  Pay.  Meth  "
	.Column11.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Pay_Meth,loFormSet.GetHeaderText("LANG_APAPRPA_Pay_Meth",loFormSet.HeaderAlias))         &&"  Pay.  Meth  "
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column12.ControlSource = '&lcAPINVHDR..DINVDATE'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column12.Header1.Caption = LANG_APAPRPA_Inv_Date         &&"Inv. Date"
	.Column12.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Inv_Date,loFormSet.GetHeaderText("LANG_APAPRPA_Inv_Date",loFormSet.HeaderAlias))         &&"Inv. Date"
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column13.ControlSource = '&lcAPINVHDR..DINVDUDAT'  &&
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column13.Header1.Caption = LANG_APAPRPA_Due_Date              &&"Due Date"
	.Column13.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Due_Date,loFormSet.GetHeaderText("LANG_APAPRPA_Due_Date",loFormSet.HeaderAlias))              &&"Due Date"
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column14.ControlSource = '&lcAPINVHDR..NTERDISCD'  &&
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column14.Header1.Caption = LANG_APAPRPA_Discount_Days         &&"Discount Days"
	.Column14.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Discount_Days,loFormSet.GetHeaderText("LANG_APAPRPA_Discount_Days",loFormSet.HeaderAlias))         &&"Discount Days"
*N000682,1 11/20/2012 MMT Globlization changes[End]

*!B610789,1 MMT 08/03/2014 Remove Division field from Grid as it slows down the screen[T20140729.0008][Start]
*!*	  .Column15.ControlSource = 'gfCodDes(&lcAPINVHDR..cDivision,"CDIVISION")'
*!*	  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*!*	*.Column15.Header1.Caption = LANG_APAPRPA_Division         &&"Division"
*!*	.Column15.Header1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Division,loFormSet.GetHeaderText("LANG_APAPRPA_Division",loFormSet.HeaderAlias))         &&"Division"
	.Column15.Visible =.F.
*!B610789,1 MMT 08/03/2014 Remove Division field from Grid as it slows down the screen[T20140729.0008][End]
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column16.ControlSource = '&lcAPINVHDR..CBNKCODE'  &&
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column16.Header1.Caption = LANG_APAPRPA_Bank             &&"Bank"
	.Column16.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Bank,loFormSet.GetHeaderText("LANG_APAPRPA_Bank",loFormSet.HeaderAlias))             &&"Bank"
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column17.ControlSource = '&lcAPINVHDR..CCHKACCT'  &&
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column17.Header1.Caption = LANG_APAPRPA_Checking_Account      &&"Checking Account"
	.Column17.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Checking_Account,loFormSet.GetHeaderText("LANG_APAPRPA_Checking_Account",loFormSet.HeaderAlias))      &&"Checking Account"
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.Column18.ControlSource = '&lcAPINVHDR..CCHKGLACC'  &&
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column18.Header1.Caption = LANG_APAPRPA_GL_Account       &&"GL Account"
	.Column18.Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_GL_Account,loFormSet.GetHeaderText("LANG_APAPRPA_GL_Account",loFormSet.HeaderAlias))       &&"GL Account"
*N000682,1 11/20/2012 MMT Globlization changes[End]


	.ReadOnly = .T.
	.Refresh()
	.DoScroll(2)
Endwith

*- Initialize the session fields
With loFormSet.ariaform1

	Store .T. To ;
		.lnSesAmnt.ReadOnly,;
		.lnSesDisc.ReadOnly,;
		.lnSesAdj.ReadOnly,;
		.lnSes1099.ReadOnly,;
		.txtInvoiceCur.ReadOnly,;
		.txtApprovCur.ReadOnly,;
		.txtApprovedAmount.ReadOnly

	Store loFormSet.llMultiCr To ;
		.lblInvoiceCur.Visible,;
		.lblApprovCur.Visible,;
		.lblApprovedAmount.Visible,;
		.txtInvoiceCur.Visible,;
		.txtApprovCur.Visible,;
		.txtApprovedAmount.Visible

	If !loFormSet.llMultiCr
		.grdAPROVE.Top = .grdAPROVE.Top - 22
		.grdAPROVE.Height = .grdAPROVE.Height + 22
	Endif

	lcBaseSmbl = loFormSet.lcBaseSmbl
*N000682,1 MMT 11/20/2012 Globalization project[Start]
*!*	    .lblApprovedAmt.Caption      = ALLTRIM('Appr. amount '+lcBaseSmbl)
*!*	    .lblApprovedDiscount.Caption = ALLTRIM('Appr. disc. ' +lcBaseSmbl)
*!*	    .lblApprovedAdj.Caption      = ALLTRIM('Appr. adj. '  +lcBaseSmbl)
*!*	    .lblApproved1099.Caption     = ALLTRIM('Appr. 1099 '  +lcBaseSmbl)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblApprovedAmt.Caption      = ALLTRIM(LANG_APAPRPA_APPRAMOUNT +lcBaseSmbl)
	.lblApprovedAmt.Caption      = Alltrim(Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_APPRAMOUNT,loFormSet.GetHeaderText("LANG_APAPRPA_APPRAMOUNT",loFormSet.HeaderAlias)) +lcBaseSmbl)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblApprovedDiscount.Caption = ALLTRIM(LANG_APAPRPA_Appr_Disc+' ' +lcBaseSmbl)
	.lblApprovedDiscount.Caption = Alltrim(Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Appr_Disc,loFormSet.GetHeaderText("LANG_APAPRPA_Appr_Disc",loFormSet.HeaderAlias))+' ' +lcBaseSmbl)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblApprovedAdj.Caption      = ALLTRIM(LANG_APAPRPA_Appr_adj+' '  +lcBaseSmbl)
	.lblApprovedAdj.Caption      = Alltrim(Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Appr_adj,loFormSet.GetHeaderText("LANG_APAPRPA_Appr_adj",loFormSet.HeaderAlias))+' '  +lcBaseSmbl)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblApproved1099.Caption     = ALLTRIM(LANG_APAPRPA_Approved_1099+' '+lcBaseSmbl)
	.lblApproved1099.Caption     = Alltrim(Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_Approved_1099,loFormSet.GetHeaderText("LANG_APAPRPA_Approved_1099",loFormSet.HeaderAlias))+' '+lcBaseSmbl)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/20/2012 Globalization project[END]
Endwith

*- add two properties for lcRpSortBy & lcRpListFor
=lfAddProp(loFormSet,'lcRpSortBy,lcRpListFor','')

*- fill up the temp file and collect data
=lfGetData(loFormSet)

*- End of lfFormInit


***********************************************************************************
*Name    : lfGetData
*Developer : TMI - Tarek Mohamed Inbrahim
*Date    : 12/24/2011
*Purpose : called from the AfterRowColChange event in the grdApprove on the screen
***********************************************************************************
Function lfGetData
Parameters loFormSet
Local lcExpr

*- Change screen to the view mode
With loFormSet.ariaform1
	Store 0 To ;
		.lnSesAmnt.Value,;
		.lnSesDisc.Value,;
		.lnSesAdj.Value,;
		.lnSes1099.Value
Endwith

lcRpSortBy  = loFormSet.lcRpSortBy
lcRpListFor = loFormSet.lcRpListFor

*- Call the "Approve to Pay" OG
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
*lcExpr = gfOpGrid('APAPRPA',.T. ,.F. ,.F. ,.T. ,.T.)
Local lcSetProc
lcSetProc = Set("Procedure")
lcExpr = gfOpGrid('APAPRPA',.T. ,.F. ,.F. ,.T. ,.T.)
Set Procedure To &lcSetProc.
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]

*-empty the temp file
Select (loFormSet.lcAPINVHDR)
Zap

*- if cancel clicked , go to Select Mode
If lcExpr == '.F.'
	loFormSet.ChangeMode('S')
	Return
Endif

loFormSet.lcRpSortBy  = lcRpSortBy
loFormSet.lcRpListFor = lcRpListFor
oAriaApplication.otoolbar.ChangeButtonStatus('pbRel','DISABLED')

*- Set the comparison operator based on if this is a Debit memo, Invoice or both
Local lcCmpOpr,lcOrder
lcCmpOpr = Iif(lcRpListFor='I',">",Iif(lcRpListFor='D',"<","<>"))
lcOrder = Iif(lcRpSortBy='I','INVVEND','VENDINV')

Select APINVHDR
Scan For APINVHDR.CVENDCODE+APINVHDR.CINVNO='' .And.CINVSTAT<>"V" .And. ;
		APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ &lcCmpOpr 0 And ;
		&lcExpr

	Scatter Memvar
	m.COWNER = ''  && use it later in the save process
	Select (loFormSet.lcAPINVHDR)
	Append Blank
	Gather Memvar Memo
Endscan
Select (loFormSet.lcAPINVHDR)
Set Order To &lcOrder
Locate

*- go to Edit mode when data is selected
loFormSet.ChangeMode('E')


*- End of lfGetData.

***********************************************************************************
*Name    : lfAfterRowColChange
*Developer : TMI - Tarek Mohamed Inbrahim
*Date    : 12/22/2011
*Purpose : called from the AfterRowColChange event in the grdApprove on the screen
***********************************************************************************
Function lfAfterRowColChange
Parameters loFormSet

Local lcAPINVHDR
lcAPINVHDR = loFormSet.lcAPINVHDR

lcExSin4 = ' '
If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
	lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cAprCurCod,&lcAPINVHDR..cCurrCode)
	lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
	lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
Else   && Else
	lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,&lcAPINVHDR..cAprCurCod)
Endif  && End of IF

With loFormSet.ariaform1
	.txtInvoiceCur.Value = &lcAPINVHDR..cCurrCode
	.txtApprovCur.Value = &lcAPINVHDR..cAprCurCod
	.txtApprovedAmount.Value = ;
		IIF(&lcAPINVHDR..nAprCurUnt > 0 ,;
		ROUND(&lcAPINVHDR..nInvAmtAp &lcExSin3 &lcAPINVHDR..nAprExRat &lcExSin4 &lcAPINVHDR..nAprCurUnt, 2), 0)
Endwith
*- End of lfAfterRowColChange.

***************************************************************************************
* Name      : clsVendCall
* Developer : Tmi Trek mohammed Ibfrahim
* Date      : 12/21/2011
* Purpose   : Define a class to bind its method to the button added to the grdApprove to call the vendor and appyinv screens
***************************************************************************************
Define Class clsVendCall As Custom
	lcAPINVHDR = ''
	Procedure Init
	Parameters lcAPINVHDR
	This.lcAPINVHDR = lcAPINVHDR
	Endproc

	Procedure lfVendor
*call the vendor screen here for vendor# :CVENDCODE
	Local lcCVENDCODE
	lcCVENDCODE = Evaluate(This.lcAPINVHDR+".CVENDCODE")
	oAriaApplication.DoProgram("AWRAPVENDR",'"&lcCVENDCODE"',.F.,"")
	Endproc

	Procedure lfInvoice
*call the Invoice screen here for Inv# :CINVNO
	Local lcCVENDCODE,lcCINVNO
	lcCVENDCODE = Evaluate(This.lcAPINVHDR+".CVENDCODE")
	lcCINVNO    = Evaluate(This.lcAPINVHDR+".CINVNO")
*!B609857,1 SAB 03/11/2012 Solve Media reported Problems[Start]
*oAriaApplication.DoProgram("AWRAPPYINV",'"&lcCVENDCODE.","&lcCINVNO."',.F.,"")
*B611697 ,1 HMS , 11/19/2018 -Aria 5 - Issue with approving a payment[T20181116.0004 ][Begin]
*oAriaApplication.DoProgram("AWRAPPYINV",'"&lcCVENDCODE.","&lcCINVNO."',.F.,"AP")
	oAriaApplication.DoProgram("AWRAPPYINV","[&lcCVENDCODE.],[&lcCINVNO.]",.F.,"AP")
*B611697 ,1 HMS , 11/19/2018 -Aria 5 - Issue with approving a payment[T20181116.0004 ][END]
*!B609857,1 SAB 03/11/2012 Solve Media reported Problems[End]
	Endproc

Enddefine

***************************************************************************************
* Name      : lfDefineVars
* Developer : Tmi Trek mohammed Ibfrahim
* Date      : 12/21/2011
* Purpose   : Define all variables to be used in the screen
***************************************************************************************
Function lfDefineVars
Parameters loFormSet
*---  Variables used
** laBankObjs has bank objects names for validations

** ibmethod     Variable to hold ib Method of payment.
** laCtrStat    To disable the browse pad in the menu
** cMarker      Vairable to mark current record.
**
** rbSort       Variable to hold rb Sort By.
** rbDuDsDt     Variable to hold rb due date and discount date.
** rbODuDsDt    Variable to hold rb due date and discount date.
** rbScope      Variable to hold rb Scope.
** rbAprAll     Variable to hold rb Apr with default or not in approve all.
** rbScopeOn    Variable to hold rb Scope On in rb Scope.
**
** puDivDes     Variable to hold pop up division.
** puDiv        Variable to hold pop up division.
** puDivision   Variable to hold pop up division.
**
** lnOSort      Variable to hold old rb Sort By.
** lnOScope     Variable to hold old rb Scope.
** lnMethod     Variable to hold ib Method of payment.
** lnDivDes     Variable to hold pu Division description.
** lnPyChMN     Variable to Display payment method in scope screen.
** lnTotPay     Variable to hold Total Payment.
** lnTotDisc    Variable to hold Total Discount.
** lnTotAdj     Variable to hold Total Adjustment.
** lnBrRecNo    Browse record number
** lnAprToPay   Variable to hold approve to pay amount.
** lnAprDisc    Variable to hold approve discount.
** lnAprAdj     Variable to hold approve adjustment.
** ln1099amnt   Variable to hold 1099 ammount.
** lnOldAprTPy  Variable to hold old approve to pay amount.
** lnOldAprDisc Variable to hold old approve discount.
** lnOldAprAdj  Variable to hold old approve adjustment.
** lnO1099amnt  Variable to hold old 1099 amount.
** lnSesAmnt    Variable to hold sesion approve amount.
** lnSesDisc    Variable to hold sesion approve discount.
** lnSesAdj     Variable to hold sesion approve adjustment.
** lnSes1099    Variable to hold sesion approve 1099.
** lnScopeOn    Variable to hole rb scope old value.
**
** lc1099St     Variable to hold approve 1099 state.
** lcFuncName   Variable to hold Refresh window.
** lcVenSum     Vendor  summary window name
** lcInvSum     Invoice summary window name
** lcBrowName   Variable to hold Brows Window name.
** lcReadNam1   Variable to hold Read Window name.
** lcReadNam2   Variable to hold Read Window name.
** lcExprsion   Variable to hold filter expresion.
** lcOldBank    Variable to hold old bank code.
** lcOldCheck   Variable to hold old check code.
** lcOldGlAcc   Variable to hold old GL account.
** lcHOldGlAc   Variable to hold old cash account.
** lcOldAcc     Variable to hold
** lcOldGLDes   Variable to hold old Account description.
** lcTAprAmnt   The Approve amount in approve message.
** lcDivision   Variable to hold division description.
** lcCInvRef    Variable to hold current Inv. Ref.
** lcPriority   Variable to hold Paroirity.
** lcPaymeth    Variable to hold pay method.
** lcOldDvCod   Variable to hold old division code.
** lcOMethod    Variable to hold old method.
** lcVend       Variable to hold Vendor Code to get tot payment for each vend.
** lcVendor     Variable to hold vendor code.
** lcTInvoice   Variable to hold word Invoice.
** lcTDebitM    Variable to hold word debit memo.
** lcTThrou     Variable to hold word through.
** lcTFrom      Variable to hold word From.
** lcOldPrty    Variable to hold old Paroirity.
** lcOldpymth   Variable to hold old pay method.
** lcTBlnkCode  Variable to hold text not aplicable.
** lcDivDisc    Variable to hold division discription.
** lcTInvNo     Variable to hold text
** lcTVendor    Variable to hold text
** lcTPrior     Variable to hold text
** lcTAprPay    Variable to hold text
** lcTAprDisc   Variable to hold text
** lcTAprAdj    Variable to hold text
** lcTApr1099   Variable to hold text
** lcTOpnAmnt   Variable to hold text
** lcTPayMeth   Variable to hold text
** lcTInvDate   Variable to hold text
** lcTDivDes    Variable to hold text
** lcTBank      Variable to hold text
** lcTGlAcct    Variable to hold text
** lcCurYear    Variable to hold current year.
** lcDivCode    Variable to hold division code.
** lcBrowFile   Vairable to hold the Browse File.
** lcVendTtl    Variable to hold vendor window title.
** lcInvoice    Variable to hold invoice window title.
** lcBrTtl      Variable to hold Browse Window Title.
** lcBankCode   Variable to hold bank code.
** lcCheckcode  Variable to hold check code.
** lcGlAcct     Variable to hold GL account.
** lcHGlAct     Variable to hold cash account.
** lcInvRef     Variable to hold Inv. Ref.
** lcOldInvRf   Variable to hold old Inv. Ref.
** lcCodeFilt   Variable to hold field filter.
** lcVendCode   Variable to hold vendor code initial val. must be null.
** lcOldVndcd   Variable to hold old vendor code initial val. must be null.
** lcVendComp   Variable to hold vendor company.
** lcOVendCod   Variable to hold old vendor code.
** lcOldVnCmp   Variable to hold old vendor company.
** lcOldDivDs   Variable to hold old division description.
** lcEscTrp     Variable to hold escape trap.
** lcOldVal     Variable to hold old field value from when functions

*  Added currency equation sign variables. (Begin)
** in case of exchange currency from invoice currency to company base currency.
** lcExSin1             Variable to hold the first sign in the equation.
** lcExSin2             Variable to hold the second sign in the equation.
** in case of exchange currency from invoice currency to approved currency.
** lcExSin3             Variable to hold the first sign in the equation.
** lcExSin4             Variable to hold the second sign in the equation.

** ldFrDueDat   Variable to hold from due date.
** ldToDueDat   Variable to hold to due date.
** ldFrDisDat   Variable to hold from discount date.
** ldToDisDat   Variable to hold to discount date.
** ldOFrDueDt   Variable to hold old from due date.
** ldOToDueDt   Variable to hold old to due date.
** ldOFrDisDt   Variable to hold old from discount date.
** ldOToDisDt   Variable to hold old to discount date.
**
** llScope      Variable to hold if you want scope to be done.
** llAprPart    Variable to hold parameter to branch between appr. fully and appr. part.
** llBrowse     Variable to hold flag in case of activate browse by mouse.
** llCanclAp    True if 'Cancel' is selected
** llVendSumm   True if vendor screen is open
** llExit       Flag indecate if the user want to exit thermo.
** llVendExst   If Vendor object is exist in scope screen.
** llNoContrl   No control panel (AP.PRG)
* lcTAprCurCod Text of field header of approved currency field
* lcTPayAcct   Text of cash payment account check
*********************

*- Create a property to hold the AP class as a reference
=lfAddProp(loFormSet,'AP1' ,'')
loFormSet.ap1 = Createobject('AP')

*- Put all variables in a two diminsional array, one column for the variable name , the other for its initial value.
*- In the start of each function that changes the APINVHDR file initialize all the variables.
=lfAddProp(loFormSet,'laPropArr[1]' ,'Temp')

*- Define variables, initialize them with SPACE(1)
lcVars = "lc1099St   , lcFuncName , lcVenSum   , lcInvSum   ,"+;
	"cMarker    , lcBrowName , lcReadNam1 , lcReadNam2 ,"+;
	"lcExprsion , lcOldBank  , lcOldCheck , lcOldGlAcc ,"+;
	"lcHOldGlAc , lcOldAcc   , lcOldGLDes , lcTAprAmnt ,"+;
	"lcDivision , lcCInvRef  , lcPriority , lcPaymeth  ,"+;
	"lcOldDvCod , lcOMethod  , lcVend     , lcVendor   ,"+;
	"lcTInvoice , lcTDebitM  , lcTThrou   , lcTFrom    ,"+;
	"lcOldPrty  , lcOldpymth , lcOldVal   , lcTBlnkCode,"+;
	"lcDivDisc  , lcTInvNo   , lcTVendor  , lcTPrior   ,"+;
	"lcTAprPay  , lcTAprDisc , lcTAprAdj  , lcTApr1099 ,"+;
	"lcTOpnAmnt , lcTPayMeth , lcTInvDate , lcTDivDes  ,"+;
	"lcTBank    , lcTGlAcct  , lcCurYear  , puDiv      ,"+;
	"puDivision , lcTAprCurCod, lcTPayAcct, lcExSin1   ,"+;
	"lcExSin2   , lcExSin3    , lcExSin4"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , ' ')

*-
lcVars = "lcVendCode , lcOldVndcd , lcVendComp , lcOVendCod ,"+;
	"lcOldVnCmp , lcOldDivDs , lcEscTrp"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , '')

*-
lcVars = "rbSort     , lnOSort    , rbScope    , lnOScope   ,"+;
	"rbAprAll   , ibmethod   , puDivDes   , lnMethod   ,"+;
	"rbDuDsDt   , rbODuDsDt  , lnDivDes"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , 1)

*-
lcVars = "lnPyChMN   , lnTotPay   , lnTotDisc  , lnTotAdj   ,"+;
	"lnBrRecNo"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , 0)

*-
lcVars = "lnAprToPay , lnAprDisc  , lnAprAdj   , ln1099amnt ,"+;
	"lnOldAprTPy, lnOldAprAdj, lnO1099amnt, lnSesAmnt  ,"+;
	"lnSesDisc  , lnSesAdj   , lnSes1099  , lnOldAprDisc"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , 0.00)

*-
lcVars = "rbScopeOn  , lnScopeOn"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , 3)

*-
lcVars = "ldFrDueDat , ldToDueDat , ldFrDisDat , ldToDisDat ,"+;
	"ldOFrDueDt , ldOToDueDt , ldOFrDisDt , ldOToDisDt"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , {})

*-
*lcVars = "llScope    , llAprPart  , llBrowse   , llCanclAp  ,"+;
"llVendSumm , llExit"
lcVars = "llScope    , llBrowse   , llCanclAp  ,"+;
	"llVendSumm , llExit"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , .F.)

*-
lcVars = "llVendExst ,llNoContrl"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , .T.)

*- single assigned vars
=lfPopPropArray(loFormSet,'laPropArr','lcBankCode' , Space(8))

=lfPopPropArray(loFormSet,'laPropArr','lcCheckcode',Space(12)    )

=lfPopPropArray(loFormSet,'laPropArr','lcInvRef,lcOldInvRf',Space(15)    )

=lfPopPropArray(loFormSet,'laPropArr','lcGlAcct,lcHGlAct', loFormSet.ap1.lcEmptyAcc )

=lfPopPropArray(loFormSet,'laPropArr','lcInvoice,lcBrTtl',"Invoices"   )

=lfPopPropArray(loFormSet,'laPropArr','lcDivCode',"*")

=lfPopPropArray(loFormSet,'laPropArr','lcBrowFile',"APINVHDR")

=lfPopPropArray(loFormSet,'laPropArr','lcVendTtl',"Vendors")

=lfPopPropArray(loFormSet,'laPropArr','laCtrStat',"DISABLE")

*- Prepare currency fields
*- lcAprCurCod  : Approval currency code
*- lcAprCshCod  : Approval currency code
*- lnAprExRat   : Approval currency exchange rate between the
*-                approval currency and the invoice currency
*- lnAprCshRat  : Cash payment exchange rate field in APAPRALL.SPR
*- lnAprCurUnt  : Approval currency unit
*- lnExchAmnt   : Approved amount in approval currency
*- llMultiCr    : .T. if multi currency
*- llEdirEx     : .T. if editing an exchange rate per transaction
*-                is allowed
*- llExchRt     : .T. if adding an exchange rate on the fly is allowed
*- lcRateDisp   : Display status of the exchange rate field,
*-                in Approve partially screen, whether enabled
*-                or disabled
*- lnOldVal     : old exchange rate value
*- lcOldCurr    : old lcAprCurCod
*- lnOldUnt     : old lnAprCurUnt
*- lnOldExRat   : old lnAprExRat
*- lcTExRateMsg : message text

*-
lcVars = "lcAprCurCod, lcOldCurr, lcAprCshCod"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , Space(5) )

*-
lcVars = "lnAprExRat, lnExchAmnt, lnAprCurUnt, lnOldVal,"+;
	"lnOldUnt, lnOldExRat, lnAprCshRat"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , 0)
=lfPopPropArray(loFormSet,'laPropArr','lcTExRateMsg','The exchange rate|zero')
=lfAddProp(loFormSet,'lcTExRateMsg','')  && *x NOTE I added lcTExRateMsg as an element in the array laPropArr and as a property, this should be reviewed to check if it would be used

*-
lcVars = "llMultiCr, llEditEx, llExchRt"
=lfAddProp(loFormSet,lcVars,.F.)
=lfAddProp(loFormSet,'lcBaseSmbl','')

With loFormSet
	.lcTExRateMsg = 'The exchange rate|zero'
	If gfGetMemVar('LLMULCURR')
		.llMultiCr  = .T.
		.llEditEx    = gfGetMemVar('LLEDITEXRA')
		.llExchRt    = gfGetMemVar('LLEXCHRATE')
		.lcBaseSmbl  = Iif(Seek(oAriaApplication.BaseCurrency, 'SYCCURR'),;
			ALLTRIM(SYCCURR.cCurrSmbl), '')
	Endif
Endwith

*** Get current year form parent company.
lcCurYear = ''
Select SYCCOMP
Locate For CCOMP_ID = oAriaApplication.PRntcompanyid
If Found()
	lcCurYear = CCURR_YER
Endif
=lfAddProp(loFormSet,'lcCurYear',lcCurYear)
=lfAddProp(loFormSet,'laPayMeth[1]' ,'')   && Array to hold Popups.
=lfAddProp(loFormSet,'laBankObjs[1]' ,'')
Dimension loFormSet.laBankObjs[3,3]

*** Get payment methods array
Dimension laPayMeth[1,2]
lnPayMLen = gfGetVld('CVENPMETH',@laPayMeth,.T.)
lnAryPos = Ascan(laPayMeth,'C',1)
If lnAryPos > 0
	=Adel(laPayMeth,Asubscript(laPayMeth, lnAryPos, 1))
	Dimension laPayMeth[ALEN(laPayMeth,1)-1,2]
Endif
Dimension loFormSet.laPayMeth[ALEN(laPayMeth,1),ALEN(laPayMeth,2)]
=Acopy(laPayMeth,loFormSet.laPayMeth)

=lfPopPropArray(loFormSet,'laPropArr','lcPayMeth', laPayMeth[1,1] ) && Variable to hold method.
=lfPopPropArray(loFormSet,'laPropArr','lcMethod' , laPayMeth[1,2] ) && Variable to hold method code.
=lfAddProp(loFormSet,'lcTBlnkCode' ,gfCodDes(' ',' '))

*- End of lfDefineVars.

*!*************************************************************
*! Name      : lfAddProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303011,1 TMI 12/17/2011
*! Purpose   : A function to add properties to the object that passed as a parameter
*!*************************************************************
Function lfAddProp
Parameters loObj,lcPropName,PropValue
*- check if lcPropname is not just a variable, but a list of variables, then create a loop to walk around
Local lnI,lnLen,lcPropToCreate
lcPropName = lcPropName + ','
lnLen = Occurs(',',lcPropName)
For lnI = 1 To lnLen
	lcPropToCreate = Alltrim(Substr(lcPropName,1,At(',',lcPropName)-1))
	If Type('loObj.&lcPropToCreate')='U'
		loObj.AddProperty(lcPropToCreate,PropValue)
	Endif
	lcPropName = Substr(lcPropName,At(',',lcPropName)+1)
Endfor

************************************************************************************************
* Name         : lfPopPropArray
* Developer    : tmi - Tarek Mohamed Ibrahim
* Date         : 12/21/2011
* Purpose      : Put all variables in a two diminsional array, one column for the variable name , the other for its initial value.
*-             : In the start of each function that changes the APINVHDR file initialize all the variables.
************************************************************************************************
Function lfPopPropArray
Lparameters loObj,lcArrPropName,lcVarsString,InitVal
Local lnLen,lnOldLen,lnI,lnCommaPos

lcVarsString = lcVarsString+','
lnLen = Occurs(',',lcVarsString)
lnOldLen = Alen(loObj.&lcArrPropName,1)
Dimension loObj.&lcArrPropName.[lnOldLen+lnLen,2]
For lnI = 1 To lnLen
	lnCommaPos = At(',',lcVarsString)
	loObj.&lcArrPropName.[lnOldLen+lnI,1] = Alltrim(Substr(lcVarsString,1,lnCommaPos-1))
	loObj.&lcArrPropName.[lnOldLen+lnI,2] = InitVal
	lcVarsString = Substr(lcVarsString,lnCommaPos+1)
Endfor

*- End of lfPopPropArray.

*!*************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303011,1 TMI 12/13/2011
*! Purpose   : called from the Screen Change mode Method
*!*************************************************************
Function lfChangeMode
Parameters loFormSet
Do Case
Case loFormSet.ActiveMode = 'S'

	loFormSet.otoolbar.cmdEdit.Enabled = .F.
	oAriaApplication.otoolbar.ChangeButtonStatus('pbScop','ENABLED')

Case loFormSet.ActiveMode = 'V'
	loFormSet.otoolbar.cmdEdit.Enabled = .T.
*!B609890,1 SAB 04/26/2012 Diabled buttons after saving problemFix Forign [T20120304.0004][Start]
	If loFormSet.llAfterSave
		loFormSet.ChangeMode('E')
		loFormSet.llAfterSave = .F.
	Endif
*!B609890,1 SAB 04/26/2012 Diabled buttons after saving problemFix Forign [T20120304.0004][End]

Case loFormSet.ActiveMode = 'E'
	oAriaApplication.otoolbar.ChangeButtonStatus('pbScop','DISABLED')

Endcase

Local llEdt
llEdt = (loFormSet.ActiveMode = 'E' And !Eof(loFormSet.lcAPINVHDR))
With loFormSet.ariaform1
	Store llEdt To ;
		.cmdAprov.Enabled ,;
		.cmdAprovePartially.Enabled  ,;
		.cmdDisapproveAll.Enabled  ,;
		.cmdApproveAll.Enabled  ,;
		.cmdDisApprove.Enabled
Endwith

With loFormSet.otoolbar
	.cmdSelect.Enabled = .F.
	.cmdFind.Enabled = .F.
	.cmdTop.Enabled = .F.
	.cmdPrev.Enabled = .F.
	.cmdNext.Enabled = .F.
	.cmdEnd.Enabled = .F.
Endwith
*- End of lfChangeMode.

*!*************************************************************
*! Name      : lfFormSavefiles
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/13/2011
*! Purpose   : Save data
*!*************************************************************
Function lfFormSavefiles
Parameters loFormSet

Local lnSlct,lnRec
lnSlct = Select(0)
Select (loFormSet.lcAPINVHDR)
lnRec = Recno()

Locate
Scan For COWNER = 'ARIA'
*REPLACE LLOK_STAT WITH .F.
	Scatter Memvar
	m.COWNER = ''
	lcKey = CVENDCODE+CINVNO

	Select APINVHDR
	=Seek(lcKey,'APINVHDR','VENDINV')

	Rlock()
	Replace CBNKCODE   With m.CBNKCODE   ;
		CCHKACCT   With m.CCHKACCT   ;
		CCHKGLACC  With m.CCHKGLACC  ;
		nInvAmtAp  With m.nInvAmtAp  ;
		NINVDISAP  With m.NINVDISAP  ;
		NINVADJAP  With m.NINVADJAP  ;
		nInvA1099  With m.nInvA1099  ;
		cAprCurCod With m.cAprCurCod ;
		nAprExRat  With m.nAprExRat  ;
		nAprCurUnt With m.nAprCurUnt ;
		nInvFAAp   With m.nInvFAAp   ;
		LLOK_STAT  With m.LLOK_STAT  ;
		CLOK_USER  With m.CLOK_USER  ;
		DLOK_DATE  With m.DLOK_DATE  ;
		CLOK_TIME  With m.CLOK_TIME  ;
		cEdit_User With oAriaApplication.User_ID ;
		dEdit_Date  With Date()    ;
		cEdit_Time  With gfGetTime()

*=gfAdd_Info('APINVHDR')
  

	Unlock
Endscan

Goto lnRec In (loFormSet.lcAPINVHDR)
loFormSet.ariaform1.grdAPROVE.Refresh

*- Update
Select APINVHDR
=gfTableUpdate()

*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][Start]
Select (loFormSet.lcAPINVHDR)
lnRec = Recno()
LOCATE 
SCAN FOR COWNER = 'ARIA'
  lcKey = CVENDCODE+CINVNO
  Select APINVHDR
  =Seek(lcKey,'APINVHDR','VENDINV')
  =gfObj_Lock(.F.)
ENDSCAN  
Goto lnRec In (loFormSet.lcAPINVHDR)
loFormSet.ariaform1.grdAPROVE.Refresh	
*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][End]

*!B609971,1 MMT 06/21/2012 Fix bug of Approve for payment screen open scope OG after saving[T20120511.0006][Start]
*!*	SELECT (loFormSet.lcAPINVHDR)
*!*	ZAP
*!*	=lfGetData(loFormset)
*!B609971,1 MMT 06/21/2012 Fix bug of Approve for payment screen open scope OG after saving[T20120511.0006][End]
*!B609890,1 SAB 04/26/2012 Diabled buttons after saving problemFix Forign [T20120304.0004][Start]
loFormSet.llAfterSave = .T.
*!B609890,1 SAB 04/26/2012 Diabled buttons after saving problemFix Forign [T20120304.0004][End]
*!B609971,1 MMT 06/21/2012 Fix bug of Approve for payment screen open scope OG after saving[T20120511.0006][Start]
loFormSet.ChangeMode('V')
*!B609971,1 MMT 06/21/2012 Fix bug of Approve for payment screen open scope OG after saving[T20120511.0006][END]
Select (lnSlct)
*- End of lfFormSavefiles

************************************************************************************************
*Name : lfFormUndo
*Developer :TMI - Tarek Mohamed Ibrahim
*Date:12/24/2011
*Purpose:undo procedure
************************************************************************************************
Function lfFormUndo
Parameters loFormSet
lcAPINVHDR = loFormSet.lcAPINVHDR

*- Check if there are any updates, then notify the user with
Select (loFormSet.lcAPINVHDR)
Locate
Locate For COWNER = 'ARIA'  && this means that a line has been edited
If Found()
* Are you sure you want to ð?
* \!\<Yes;\?\<No
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*IF gfModalGen('QRM40169B00006','DIALOG','lose all your changes') <> 1  &&
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen('QRM40169B00006','DIALOG',LANG_APAPRPA_LOSEALLYOURCHANGES) <> 1  &&
	If gfModalGen('QRM40169B00006','DIALOG',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_LOSEALLYOURCHANGES,loFormSet.GetHeaderText("LANG_APAPRPA_LOSEALLYOURCHANGES",loFormSet.HeaderAlias))) <> 1  &&
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[END]
		Return
	Endif
Endif

*- Clear the locks
Select (loFormSet.lcAPINVHDR)
*!B612542,1 MMT 03/31/2022 Approve for payment screen UNDO method unlocks locked records locked by another program[T20220128.0001][Start]
*SCAN
SCAN FOR COWNER = 'ARIA'
*!B612542,1 MMT 03/31/2022 Approve for payment screen UNDO method unlocks locked records locked by another program[T20220128.0001][End]
	=Seek(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
	Select APINVHDR
	=gfObj_Lock(.F.)
Endscan

*- Delete data
Select (loFormSet.lcAPINVHDR)
Zap

*- Call the lfGetData again
*!*  loFormSet.ChangeMode('S')
*!*  loFormSet.ARIAFORM1.refresh()
=lfGetData(loFormSet)
*- End of lfFormUndo.


******************************************************************************************************************<
************************** from here the start of the original program's functions *******************************<
******************************************************************************************************************<




*!**************************************************************************
*!
*!      Function: lfGetPayMeth
*!
*!**************************************************************************
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[Start]
*FUNCTION lfGetPayMeth
Function lfGetAPRPayMeth
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[End]
Parameter loFormSet
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][Start]
Set DataSession To loFormSet.DataSessionId
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][End]

lcAPINVHDR = loFormSet.lcAPINVHDR
lcPayCode = &lcAPINVHDR..CVENPMETH

lnposition= Ascan(loFormSet.laPayMeth,lcPayCode,1)

If lnposition <> 0
	Return loFormSet.laPayMeth[ASUBSCRIPT(loFormSet.laPayMeth,lnposition,1),1]
Else
	Return " "
Endif


***********************************************************************************
*Name    : lfvDisAppr
*Developer : TMI - Tarek Mohamed Inbrahim
*Date    : 12/22/2011
*Purpose : * Disaprove the pointed record.
***********************************************************************************
Function lfvDisAppr
Parameters loFormSet

*E303011,1 TMI 12/21/2011 [Start] define variables
Local lnI,lcVar
For lnI = 1 To Alen(loFormSet.laPropArr,1)
	lcVar = loFormSet.laPropArr[lnI,1]
	&lcVar = loFormSet.laPropArr[lnI,2]
Endfor
*- locate the same line in the APINVHDR file
lcAPINVHDR = loFormSet.lcAPINVHDR
=Seek(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
=Seek(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
*E303011,1 TMI 12/22/2011 [End  ]

lcExSin2 = ' '
lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')


*IF gfObj_lock(.T.) && lock the pointed record.
*E303011,1 TMI 12/22/2011 [Start] use the global function gfObj_lock
*IF lfObj_lock(.T.) && lock the pointed record.
Select APINVHDR
*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][Start]
*If &lcAPINVHDR..COWNER='ARIA' Or gfObj_Lock(.T.) && lock the pointed record.
IF &lcAPINVHDR..COWNER='ARIA' Or lfObj_lock(.T.) 
*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][End]
	Select &lcAPINVHDR
	Replace COWNER With 'ARIA'  && this is used as a tag, this means that this record has been changed by the approve to pay program
*E303011,1 TMI 12/22/2011 [End  ]

	lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit
	With loFormSet.ariaform1
		.lnSesAmnt.Value = .lnSesAmnt.Value  - Round(&lcAPINVHDR..nInvAmtAp &lcExSin1 lnRateVal, 2)
		.lnSesDisc.Value = .lnSesDisc.Value  - Round(&lcAPINVHDR..NINVDISAP &lcExSin1 lnRateVal, 2)
		.lnSesAdj.Value  = .lnSesAdj.Value   - Round(&lcAPINVHDR..NINVADJAP &lcExSin1 lnRateVal, 2)
		.lnSes1099.Value = .lnSes1099.Value  - Round(&lcAPINVHDR..nInvA1099 &lcExSin1 lnRateVal, 2)
		.Refresh()
	Endwith

	Replace &lcAPINVHDR..nInvAmtAp  With 0 ,;
		&lcAPINVHDR..NINVDISAP  With 0 ,;
		&lcAPINVHDR..NINVADJAP  With 0 ,;
		&lcAPINVHDR..nInvA1099  With 0 ,;
		&lcAPINVHDR..CBNKCODE   With '',;
		&lcAPINVHDR..CCHKACCT   With '',;
		&lcAPINVHDR..CCHKGLACC  With '',;
		&lcAPINVHDR..cAprCurCod With '',;
		&lcAPINVHDR..nAprCurUnt With 0,;
		&lcAPINVHDR..nAprExRat  With 0,;
		&lcAPINVHDR..nInvFAAp   With 0

*=gfAdd_Info()  && Add the audit information to the record.
*E303011,1 TMI 12/22/2011 [Start]
*  =gfObj_lock(.F.)  && unlocking the pointed record
*E303011,1 TMI 12/22/2011 [End  ]
	If !Bof()
		Skip -1
***Refresh the record pointer. ***
		lnBrRecNo  = Recno('&lcAPINVHDR.')
***Refresh the invoice push button. ***
***Refresh say field open apen amount. ***
		loFormSet.ariaform1.Refresh()
	Endif
*Old : SHOW WINDOW (lcBrTtl) REFRESH
Endif

loFormSet.ariaform1.grdAPROVE.AfterRowColChange()
loFormSet.ariaform1.Refresh()
*- End of lfvDisAppr

***********************************************************************************
*Name      : lfvDApprAll
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Disapprove all records in browse.
***********************************************************************************
Function lfvDApprAll
Parameters loFormSet
Private lnTotInv1 , lnTotInv2

*E303011,1 TMI 12/24/2011 [Start] define variables
Local lnI,lcVar
For lnI = 1 To Alen(loFormSet.laPropArr,1)
	lcVar = loFormSet.laPropArr[lnI,1]
	&lcVar = loFormSet.laPropArr[lnI,2]
Endfor
*- locate the same line in the APINVHDR file
lcAPINVHDR = loFormSet.lcAPINVHDR
*E303011,1 TMI 12/24/2011 [End  ]

lnTotInv1   = 0
lnTotInv2   = 0
Select &lcAPINVHDR
lcSavOrd = Order()
Set Order To 0
Count To lnTotInv1
Set Order To &lcSavOrd
***Exit from disapproving loop.
lcEscap=Set("ESCAPE")
Set Escape On
On Escape Store .T. To llExit

oProgress = Newobject('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
oProgress.TotalProgress = Reccount(loFormSet.lcAPINVHDR)
*N000682,1 MMT 11/20/2012 Globalization Changes[Start]
*oProgress.lblFirstLabel.CAPTION = 'Approve All'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*oProgress.lblFirstLabel.CAPTION =LANG_APAPRPA_APPROVE_ALL
oProgress.lblFirstLabel.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_APPROVE_ALL,loFormSet.GetHeaderText("LANG_APAPRPA_APPROVE_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/20/2012 Globalization Changes[End]

lnThermNo = 0
oProgress.Show()

Scan
	=Seek(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
	=Seek(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')

	lnTotInv2 = lnTotInv2 + 1
*=gfThermo(lnTotInv1,lnTotInv2,"Disapproving invoices...",;
"Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)



*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to company base currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
	lcExSin2 = ' '
	lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')


*IF gfObj_lock(.T.) && lock the pointed record.
*E303011,1 TMI 12/22/2011 [Start]
*IF lfObj_lock(.T.) && lock the pointed record.
	Select APINVHDR
	*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][Start]
	*If &lcAPINVHDR..COWNER='ARIA' Or gfObj_Lock(.T.) && lock the pointed record.
	IF &lcAPINVHDR..COWNER='ARIA' Or lfObj_lock(.T.) 
	*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][End]
		Select &lcAPINVHDR
		Replace COWNER With 'ARIA'  && this is used as a tag, this means that this record has been changed by the approve to pay program
*E303011,1 TMI 12/22/2011 [End  ]

		lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit
		With loFormSet.ariaform1
			.lnSesAmnt.Value = .lnSesAmnt.Value  - Round(&lcAPINVHDR..nInvAmtAp &lcExSin1 lnRateVal, 2)
			.lnSesDisc.Value = .lnSesDisc.Value  - Round(&lcAPINVHDR..NINVDISAP &lcExSin1 lnRateVal, 2)
			.lnSesAdj.Value  = .lnSesAdj.Value   - Round(&lcAPINVHDR..NINVADJAP &lcExSin1 lnRateVal, 2)
			.lnSes1099.Value = .lnSes1099.Value  - Round(&lcAPINVHDR..nInvA1099 &lcExSin1 lnRateVal, 2)
			.Refresh()
		Endwith

		Replace &lcAPINVHDR..nInvAmtAp  With 0 ,;
			&lcAPINVHDR..NINVDISAP  With 0 ,;
			&lcAPINVHDR..NINVADJAP  With 0 ,;
			&lcAPINVHDR..nInvA1099  With 0 ,;
			&lcAPINVHDR..CBNKCODE   With '',;
			&lcAPINVHDR..CCHKACCT   With '',;
			&lcAPINVHDR..CCHKGLACC  With '',;
			&lcAPINVHDR..cAprCurCod With '',;
			&lcAPINVHDR..nAprCurUnt With 0,;
			&lcAPINVHDR..nAprExRat  With 0,;
			&lcAPINVHDR..nInvFAAp   With 0

*=gfAdd_Info()  && Add the audit information to the record.
*E303011,1 TMI 12/22/2011 [Start]
*    =gfObj_lock(.F.) && lock the pointed record.
*E303011,1 TMI 12/22/2011 [End  ]
	Endif
	lnThermNo = lnThermNo + 1
	oProgress.CurrentProgress(lnThermNo)
	oProgress.lblFirstLabel.Caption = APINVHDR.CVENDCODE
	If llExit  &&If Escape pressed.
		lnTotInv1 = lnTotInv2
*IF wvisible("GWDTHERMO")
*  RELEASE WINDOW ("GWDTHERMO")
*ENDIF
		llExit    = .F.
		Exit
	Endif
Endscan

oProgress = Null

*- Check if the Thermometer window exist then force it to close
*!*  IF WEXIST('gwdThermo')
*!*    =gfThermo(lnTotInv1,lnTotInv1,"Disapproving invoices ...",;
*!*            "Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
*!*  ENDIF
Go Bottom
Set Escape &lcEscap

*B500787,1 Removed browse activation and replaced it with a call to the
*B500787,1 BROWSE COMMAND WHEN function so as to refresh all data, as
*B500787,1 well as the browse and the file pointer with the values of the
*B500787,1 current record. (the last record in the browse in this case)
*ACTIVATE WINDOW (lcBrTtl)
*=lfwInvBrow()

loFormSet.ariaform1.grdAPROVE.AfterRowColChange()
loFormSet.ariaform1.Refresh()
*- End of lfvDApprAll

***********************************************************************************
*Name      : lfvApprAll
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Saving the old values and calling approve all window
***********************************************************************************
Function lfvApprAll
Parameters loFormSet
*E303011,1 TMI 12/21/2011 [Start] define variables
Local lnI,lcVar
For lnI = 1 To Alen(loFormSet.laPropArr,1)
	lcVar = loFormSet.laPropArr[lnI,1]
	&lcVar = loFormSet.laPropArr[lnI,2]
Endfor
*- locate the same line in the APINVHDR file
lcAPINVHDR = loFormSet.lcAPINVHDR
=Seek(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
=Seek(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
*E303011,1 TMI 12/22/2011 [End  ]

*** get default bank, checks, and gl account.
lcBankCode = APSETUP.CBNKCODE
lcCheckCode= APSETUP.CCHKACCT
*B600808,1 Default the G/L account whether or not it is empty.
*** get account if there is a bank code and check code available. ***
*IF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
*  .AND. !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
If Seek(lcBankCode+lcCheckCode,'APCHECKS')
	lcGlAcct = Iif(!Empty(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC, loFormSet.ap1.lcEmptyAcc)
*B600808,1 Cash payment account is edfaulted by the
*B600808,1 cash payment account in APSETUP and not bey lcGLAcct
*lcHGlAct = lcGlAcct
*- Get the currency of the checking account.
	lcAprCurCod = APCHECKS.cCurrCode
Endif

*- Get default Cash payment account.
lcHGlAct    = APSETUP.cCashAcct

*- Default the cash payment currency with the base currency.
lcAprCshCod = oAriaApplication.BaseCurrency

*B600808,1 There is no need to store the following values if we default
*B600808,1 every time we get into the screen.
*lcOldBank   = lcBankCode
*lcOldCheck  = lcCheckCode
*lcOldGlAcc  = lcGLAcct
*lcHOldGlAc  = lcHGlAct

*Old : PUSH KEY
*Old : ON KEY      && refresh the key traped

*,1 Call *.SPR from screens directory
* DO APAPRALL.SPR  && calling the approved all window.
Do Form (oAriaApplication.ScreenHome+"\AP\APAPRALL.SCX") With loFormSet
*,1 end

*** Push the same keys again after coming ***
*** from global function browse. ***
* old : POP KEY

*B500787,1 Removed browse activation and replaced it with a call to the
*B500787,1 BROWSE COMMAND WHEN function so as to refresh all data, as
*B500787,1 well as the browse and the file pointer with the values of the
*B500787,1 current record. (the last record in the browse in this case)
*ACTIVATE WINDOW (lcBrTtl)
*Old : =lfwInvBrow()

loFormSet.ariaform1.grdAPROVE.AfterRowColChange()
loFormSet.ariaform1.Refresh()
*- End of lfvApprAll

***********************************************************************************
*Name      : lfShAprAll
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : control showing objects in case of approve default or approve accounts entered.
***********************************************************************************
Function lfShAprAll
Parameters loAprAllFormSet
loFormSet = loAprAllFormSet.Callingform
*** in case of approve default disable all objects.
With loAprAllFormSet.ariaform1
	If rbAprAll = 1
*Old :   SHOW GET lcBankCode  DISABLE
*Old :   SHOW GET ibBank      DISABLE
*Old :   SHOW GET lcCheckCode DISABLE
*Old :   SHOW GET ibChecks    DISABLE
		.BankChk.Enabled = .F.
*Old :   SHOW GET lcGlAcct    DISABLE
*Old :   SHOW GET lcHGlAct    DISABLE
*Old :   SHOW GET ibGLAcc     DISABLE
*Old :   SHOW GET ibHGLAcc    DISABLE
		.glChkActCode.Enabled = .F.
		.glCashPayAcc.Enabled = .F.
*- Disable currency fields as well
*Old :   SHOW GET ibAprCurCod DISABLE
*Old :   SHOW GET lcAprCshCod DISABLE
		.kbAprCurrCode.Enabled = .F.
		.kbCashPayCurr.Enabled = .F.
*** in case of approve accounts the user entered. ***
	Else
		lcOldBank   = lcBankCode   && get the bank old value.
		lcOldCheck  = lcCheckCode  && get the check old value.
		lcOldGlAcc  = lcGlAcct     && get the account old value.
		lcHOldGlAc  = lcHGlAct
		If Empty(lcBankCode) && if empty of bank.
*Old :     SHOW GET lcBankCode ENABLE
*Old :     SHOW GET ibBank     ENABLE
			.BankChk.kbBanks.Enabled = .T.
		Else
			If Empty(lcCheckCode) && if empty of check.
*Old :       SHOW GET lcBankCode  ENABLE
*Old :       SHOW GET ibBank      ENABLE
*Old :       SHOW GET lcCheckCode ENABLE
*Old :       SHOW GET ibChecks    ENABLE
				.BankChk.Enabled = .T.
			Else                  && if empty of account.
*Old : SHOW GET lcBankCode  ENABLE
*Old : SHOW GET ibBank      ENABLE
*Old : SHOW GET lcCheckCode ENABLE
*Old : SHOW GET ibChecks    ENABLE
*Old : SHOW GET lcGlAcct    ENABLE
*Old : SHOW GET ibGLAcc     ENABLE
				.BankChk.Enabled = .T.
				.glChkActCode.Enabled = .T.
				.glCashPayAcc.Enabled = .T.

			Endif
		Endif

*- Enable cash payment account any way
*Old : SHOW GET lcHGlAct    ENABLE
*Old : SHOW GET ibHGLAcc    ENABLE
		.glCashPayAcc.Enabled = .T.

		If lcHGlAct <> loFormSet.ap1.lcEmptyAcc

*Old : SHOW GET ibAprCurCod ENABLE
*Old : SHOW GET lcAprCshCod ENABLE
			.kbCashPayCurr.Enabled = .T.
		Endif

	Endif
*Old : SHOW GET pbCancel ENABLE
	.cmdCancel.Enabled = .T.
Endwith
*- End of lfShAprAll.

***********************************************************************************
*Name      : lfvpbAprAl
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : approve the all browse record in the file.
***********************************************************************************
Function lfvpbAprAl
Parameters loAprAllFormSet
Private lcCurAlias, lnTotInv1 , lnTotInv2
loFormSet = loAprAllFormSet.Callingform

lnTotInv1   = 0
lnTotInv2   = 0

*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
lcVen0Prior = ''

* Old : SELECT APINVHDR
*E303011,1 TMI 12/24/2011 [Start]
Select &lcAPINVHDR
*E303011,1 TMI 12/24/2011 [End  ]

lcSavOrd = Order()

Set Order To 0
Count To lnTotInv1
Set Order To &lcSavOrd
**Exit from approving loop.**
lcEscap=Set("ESCAPE")
Set Escape On
On Escape Store .T. To llExit

*** in case of appove with default values. ***
With loFormSet.ariaform1
	.lnSesAmnt.Value = 0
	.lnSesDisc.Value = 0
	.lnSesAdj.Value  = 0
	.lnSes1099.Value = 0
Endwith

* Old : lnSes1099 = 0
* Old : SHOW GET lnSes1099 LEVEL RDLEVEL()-1

If rbAprAll = 1

	oProgress = Newobject('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
	oProgress.TotalProgress = Reccount(loFormSet.lcAPINVHDR)
*N000682,1 MMT 11/20/2012 Globalization Changes[Start]
*oProgress.lblFirstLabel.CAPTION = 'Approve All'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*oProgress.lblFirstLabel.CAPTION =LANG_APAPRPA_APPROVE_ALL
	oProgress.lblFirstLabel.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_APPROVE_ALL,loFormSet.GetHeaderText("LANG_APAPRPA_APPROVE_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/20/2012 Globalization Changes[End]
	lnThermNo = 0
	oProgress.Show()

	Scan
*E303011,1 TMI 12/24/2011 [Start]
		=Seek(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
		=Seek(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
*E303011,1 TMI 12/24/2011 [End  ]

*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
		If !((CVENPRIOR = '0') .Or. (APVENDOR.CVENPRIOR = '0'))

			lnTotInv2 = lnTotInv2 + 1
*=gfThermo(lnTotInv1,lnTotInv2,"Approving invoices...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)

*IF gfObj_lock(.T.) && lock the pointed record.
*E303011,1 TMI 12/22/2011 [Start]
*IF lfObj_lock(.T.) && lock the pointed record.
			Select APINVHDR
			*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][Start]
			*If &lcAPINVHDR..COWNER='ARIA' Or gfObj_Lock(.T.) && lock the pointed record.
			IF &lcAPINVHDR..COWNER='ARIA' OR lfObj_lock(.T.) 
			*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][End]
				Select &lcAPINVHDR
				Replace COWNER With 'ARIA'  && this is used as a tag, this means that this record has been changed by the approve to pay program
*E303011,1 TMI 12/22/2011 [End  ]

***If the vendor or invoice is on hold. ***
*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
*IF (CVENPRIOR <> '0') .AND. (APVENDOR.CVENPRIOR <> '0')
				If &lcAPINVHDR..CVENPMETH <> 'H'
					lcBankCode   = CBNKCODE && get bank initial value.
					lcCheckCode  = CCHKACCT
					lcGlAcct     = Iif(!Empty(CCHKGLACC),CCHKGLACC,loFormSet.ap1.lcEmptyAcc)
*B600808,1 Cash payment account does not default from
*B600808,1 the bank/checking account.
*lcHGlAct   = lcGlAcct

					If Empty(lcBankCode) && if there is no initial value.
						Do Case
						Case !Empty(APVENDOR.CBNKCODE) && try to get approve value form vendor file.
							lcBankCode = APVENDOR.CBNKCODE
							lcCheckCode= APVENDOR.CCHKACCT
*B600808,1 Clear G/L account
							lcGlAcct   = loFormSet.ap1.lcEmptyAcc

						Case !Empty(APDIV.CBNKCODE)    && try to get approve value form division file.
							lcBankCode = APDIV.CBNKCODE
							lcCheckCode= APDIV.CCHKACCT
*B600808,1 Clear G/L account
							lcGlAcct   = loFormSet.ap1.lcEmptyAcc

						Otherwise                      && try to get approve value form setup file.
							lcBankCode = APSETUP.CBNKCODE
							lcCheckCode= APSETUP.CCHKACCT
*B600808,1 Clear G/L account
							lcGlAcct   = loFormSet.ap1.lcEmptyAcc
						Endcase

*B600808,1 Add an ENDIFto close the empty bank condition
*B600808,1 here. Get the defaults anyway.
					Endif

*** get account value in case of empty and the is bank and check.
*B600808,1 Move condition below.
* IF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
.AND. !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
					If Seek(lcBankCode+lcCheckCode,'APCHECKS')
*lcGlAcct = IIF(!EMPTY(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC,lcEmptyAcc)
*B600808,1 No cash payment account
*lcHGlAct = lcGlAcct
*** replace the default values got to the file. ***
*B600808,1 Check here for empty account
						If Empty(Strtran(Strtran(lcGlAcct,'-'),'0'))
							lcGlAcct = Iif(!Empty(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC,loFormSet.ap1.lcEmptyAcc)
						Endif

*- Get default currency of the checking account.
						lcAprCurCod   = APCHECKS.cCurrCode

*- Get a corresponding exchange rate
*- between the approval currency and the invoice currency.
*- If the currency is different, implies that the system
*- is multi currency, call the global function gfChkRate
*- that validates an exchange rate.

						If &lcAPINVHDR..cCurrCode <> lcAprCurCod
							If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
								lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,&lcAPINVHDR..dInvDate, ;
									.T., .F., &lcAPINVHDR..cCurrCode, .T.)
							Else      && Else
								lnAprExRat = gfChkRate('lnAprCurUnt', &lcAPINVHDR..cCurrCode ,&lcAPINVHDR..dInvDate, ;
									.T., .F., lcAprCurCod, .T.)
							Endif     && End of IF
*- Change the calling of gfChkRate [END]

*B600849,1 (End)
						Else
							Store 1 To lnAprExRat, lnAprCurUnt
						Endif      && ENDIF APINVHDR.cCurrCode <> lcAPrCurCod
					Endif

*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to company base currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
					lcExSin2 = ' '
					lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*FUNCTION  HISH 12/05/95. to exchange currency from invoice currency to approved currency.
					lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
					If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
						lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,&lcAPINVHDR..cCurrCode)
						lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
						lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
					Else   && Else
						lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCurCod)
					Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')


*- If the exchange rate = 0, do not approve, and present
*- the following message
					If lnAprExRat = 0
						If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
							=gfModalGen("INM04157B00000", "DIALOG",;
								ALLTRIM(lcAprCurCod)+'|' ;
								+Alltrim(&lcAPINVHDR..cCurrCode)+;
								'|'+Dtoc(&lcAPINVHDR..dInvDate))
						Else    && Else
							=gfModalGen("INM04157B00000", "DIALOG",;
								ALLTRIM(&lcAPINVHDR..cCurrCode)+'|' ;
								+Alltrim(lcAprCurCod)+;
								'|'+Dtoc(&lcAPINVHDR..dInvDate))
						Endif     && End of IF

					Else

						Replace CBNKCODE   With lcBankCode ,;
							CCHKACCT   With lcCheckCode,;
							CCHKGLACC  With Iif(Empty(Strtran(Strtran(lcGlAcct,'-'),'0')),;
							SPACE(lnApsAcLen),lcGlAcct),;
							nInvAmtAp  With (&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF),;
							NINVDISAP  With &lcAPINVHDR..NINVDISOF,;
							NINVADJAP  With 0,;
							nInvA1099  With 0,;
							cAprCurCod With lcAprCurCod,;
							nAprCurUnt With lnAprCurUnt,;
							nAprExRat  With lnAprExRat,;
							nInvFAAp   With Round((&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)

*B601526,1 Change this line [End]

*=gfAdd_Info('&lcAPINVHDR.')  && Add the audit information to the record.
					Endif
				Else     && If Cash Payment
					Do Case
*B600808,1 Add a case to get the cash payment account of
*B600808,1 the invoice,
					Case !Empty(&lcAPINVHDR..CCHKGLACC)
						lcHGlAcct   = CCHKGLACC
					Case !Empty(APVENDOR.cCashAcct)
						lcHGlAct= APVENDOR.cCashAcct
					Case !Empty(&lcAPINVHDR..CDIVISION) .And. !Empty(APDIV.cCashAcct)
						lcHGlAct= APDIV.cCashAcct
					Otherwise
						lcHGlAct = Iif(!Empty(APSETUP.cCashAcct),;
							APSETUP.cCashAcct,loFormSet.ap1.lcEmptyAcc)
					Endcase

					Replace CCHKGLACC  With Iif(Empty(Strtran(Strtran(lcHGlAct,'-'),'0')),;
						SPACE(lnApsAcLen), lcHGlAct),;
						nInvAmtAp  With (&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF),;
						NINVDISAP  With &lcAPINVHDR..NINVDISOF,;
						NINVADJAP  With 0,;
						nInvA1099  With 0,;
						cAprCurCod With &lcAPINVHDR..cCurrCode,;
						nAprCurUnt With 1,;
						nAprExRat  With 1,;
						nInvFAAp   With Round((&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)

*=gfAdd_Info('&lcAPINVHDR.')  && Add the audit information to the record.
				Endif

*B601013- HISH  04/18/96. Got the equation signs. (Begin)
*B601013-  to exchange currency from invoice currency to company base currency.
				lcExSin2 = ' '
				lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)

				lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit
				With loFormSet.ariaform1
					.lnSesAmnt.Value = .lnSesAmnt.Value + Round(&lcAPINVHDR..nInvAmtAp &lcExSin1 lnRateVal, 2)
					.lnSesDisc.Value = .lnSesDisc.Value + Round(&lcAPINVHDR..NINVDISAP &lcExSin1 lnRateVal, 2)
				Endwith

*E303011,1 TMI 12/22/2011 [Start]
*=gfObj_lock(.F.) && lock the pointed record.
*E303011,1 TMI 12/22/2011 [End  ]
			Endif
			If llExit  &&If Escape pressed.
				lnTotInv1 = lnTotInv2
				If Wvisible("GWDTHERMO")
					Release Window ("GWDTHERMO")
				Endif
				llExit    = .F.
				Exit
			Endif
		Else
*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
			Do Case
			Case CVENPRIOR = '0'
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)+' Invoice No. '+ALLTRIM(CINVNO)
				lcvendorr=Alltrim(APVENDOR.CVENDCODE)+' '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_INVNO,loFormSet.GetHeaderText("LANG_APAPRPA_INVNO",loFormSet.HeaderAlias))+' '+Alltrim(CINVNO)
*N000682,1 MMT 12/09/2012 Globalization changes[END]
** MESSAGE : " Vendor XXXXXX has payment priority 0."
**           " This vendor in on hold.              "
**           "                ® OK ¯                "
				=gfModalGen("TRM04060B00000","DIALOG",lcvendorr)

			Case APVENDOR.CVENPRIOR = '0' .And. lcVen0Prior <> CVENDCODE
				lcvendorr=Alltrim(APVENDOR.CVENDCODE)
** MESSAGE : " Vendor XXXXXX has payment priority 0."
**           " This vendor in on hold.              "
**           "                ® OK ¯                "
				=gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
			Endcase
			lcVen0Prior = CVENDCODE
			lnTotInv2 = lnTotInv2 + 1
		Endif

		lnThermNo = lnThermNo + 1
		oProgress.CurrentProgress(lnThermNo)
		oProgress.lblFirstLabel.Caption = APINVHDR.CVENDCODE

	Endscan
	oProgress = Null

*- Check if the Thermometer window exist then force it to close
*!*    IF WEXIST('gwdThermo')
*!*      =gfThermo(lnTotInv1,lnTotInv1,"Approving invoices ...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
*!*    ENDIF
	Go Bottom
*Old : CLEAR READ

Else && in case of appove with entered values.


** if any approved values are empty. **
	If(Empty(lcBankCode) .Or. Empty(lcCheckCode))
*!*                         .OR. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'));
*!*                         .OR. (ATC("?",lcGlAcct)>0));
*!*                         .OR. EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0'));
*!*                         .OR. (ATC("?",lcHGlAct)>0))
** MESSAGE : " You have to enter the bank code,the     "
**           " checking account,the GL account,and Cash payment."
**           "                    ® Ok ¯               "
		=gfModalGen("TRM04090B00000","DIALOG")

		If Empty(lcBankCode)
* Old : SHOW GET lcBankCode
* Old : _CUROBJ = OBJNUM(lcBankCode)
			loAprAllFormSet.ariaform1.BankChk.kbBanks.keyTextBox.SetFocus()

		Else
			If Empty(lcCheckCode)
*Old : SHOW GET lcCheckCode
* Old : _CUROBJ = OBJNUM(lcCheckCode)
				loAprAllFormSet.ariaform1.BankChk.kbBanks.keyTextBox.SetFocus()
			Else
* Old : SHOW GET lcGlAcct
* Old : SHOW GET lcHGlAcct
* Old : _CUROBJ = OBJNUM(lcGlAcct)
				loAprAllFormSet.ariaform1.glChkActCode.keyTextBox.SetFocus()
			Endif
		Endif
*- Add checks for currency fields

		Return .F.
	Endif

*!B610215,1 HIA 01/23/2013 Aria4xp - AP - Approve for payment bank account issue [T20130107.0001][Start]
	lcBankCode  = loAprAllFormSet.ariaform1.BankChk.kbBanks.keyTextBox.Value
	lcCheckCode = loAprAllFormSet.ariaform1.BankChk.kbChkAccount.keyTextBox.Value
*!B610215,1 HIA 01/23/2013 Aria4xp - AP - Approve for payment bank account issue [T20130107.0001][End]

*- Add checks for currency fields
	If loFormSet.llMultiCr .And. Empty(lcAprCshCod)
*** Message : " You have to enter the ð."
***                      <   OK   >
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*=gfModalGen("TRM04066B00000","DIALOG",'cash payment approved currency')
		=gfModalGen("TRM04066B00000","DIALOG",Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_CASHPAMENTAPPROVEDCURRENCY,loFormSet.GetHeaderText("LANG_APAPRPA_CASHPAMENTAPPROVEDCURRENCY",loFormSet.HeaderAlias)))
*N000682,1 MMT 11/22/2012 Globalization changes[END]
* Old : _CUROBJ = OBJNUM(lcAprCshCod)
		loAprAllFormSet.ariaform1.kbCashPayCurr.keyTextBox.SetFocus()
		Return .F.
	Else && if all approved values is full.
*CLEAR READ
	Endif

	lcCurAlias  = Alias()

*E303011,1 TMI 12/24/2011 [Start]
*SELECT APINVHDR
	Select &lcAPINVHDR
*E303011,1 TMI 12/24/2011 [End  ]

	oProgress = Newobject('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
	oProgress.TotalProgress = Reccount(loFormSet.lcAPINVHDR)
*N000682,1 MMT 11/20/2012 Globalization Changes[Start]
*  oProgress.lblFirstLabel.CAPTION = 'Approve All'
	oProgress.lblFirstLabel.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_APPROVE_ALL,loFormSet.GetHeaderText("LANG_APAPRPA_APPROVE_ALL",loFormSet.HeaderAlias))
*N000682,1 MMT 11/20/2012 Globalization Changes[End]
	lnThermNo = 0
	oProgress.Show()

	Scan

*E303011,1 TMI 12/24/2011 [Start]
		=Seek(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
		=Seek(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
*E303011,1 TMI 12/24/2011 [End  ]

*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
		If !((CVENPRIOR = '0') .Or. (APVENDOR.CVENPRIOR = '0'))

			lnTotInv2 = lnTotInv2 + 1
*=gfThermo(lnTotInv1,lnTotInv2,"Approving invoices...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)

*IF gfObj_lock(.T.) && lock the pointed record.
*E303011,1 TMI 12/22/2011 [Start]
*IF lfObj_lock(.T.) && lock the pointed record.
			Select APINVHDR
			*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][Start]
			*If &lcAPINVHDR..COWNER='ARIA' Or gfObj_Lock(.T.) && lock the pointed record.
			If &lcAPINVHDR..COWNER='ARIA' OR lfObj_lock(.T.)
			*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][End]
				Select &lcAPINVHDR
				Replace COWNER With 'ARIA'  && this is used as a tag, this means that this record has been changed by the approve to pay program
*E303011,1 TMI 12/22/2011 [End  ]

***If the vendor or invoice is on hold. ***
*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
* IF (CVENPRIOR <> '0') .AND. (APVENDOR.CVENPRIOR <> '0')
				If &lcAPINVHDR..CVENPMETH <> 'H'
*- Get exchange rate between the checking account
*- currency and the invoice currency.
*- If the currency is different, implies that the system
*- is multi currency, call the global function gfChkRate
*- that validates an exchange rate.
					If &lcAPINVHDR..cCurrCode <> lcAprCurCod
						If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
							lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,&lcAPINVHDR..dInvDate, ;
								.T., .F., &lcAPINVHDR..cCurrCode, .T.)
						Else      && Else
							lnAprExRat = gfChkRate('lnAprCurUnt', &lcAPINVHDR..cCurrCode ,&lcAPINVHDR..dInvDate, ;
								.T., .F., lcAprCurCod, .T.)
						Endif     && End of IF
*- Change the calling of gfChkRate [END]

*B600849,1 (End)
*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to company base currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
						lcExSin2 = ' '
						lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*FUNCTION  HISH 12/05/95. to exchange currency from invoice currency to approved currency.
						lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
						If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
							lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,&lcAPINVHDR..cCurrCode)
							lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
							lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
						Else   && Else
							lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCurCod)
						Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')

					Else
						Store 1 To lnAprExRat, lnAprCurUnt
*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to company base currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
						lcExSin2 = ' '
						lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*FUNCTION  HISH 12/05/95. to exchange currency from invoice currency to approved currency.
						lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
						If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
							lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,&lcAPINVHDR..cCurrCode)
							lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
							lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
						Else   && Else
							lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCurCod)
						Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')

					Endif      && ENDIF APINVHDR.cCurrCode <> lcAPrCurCod

*- If the exchange rate = 0, do not approve, and present
*- the following message
					If lnAprExRat = 0
*- IF editing the exchange rate on the fly is not
*- allowed, present the following message:
*- Message : " A valid ð to ð exchange rate could not "
*-           " be found for ð.                        "
*-           "                  ® Ok ¯                "

*B601667,1 Change this line to fix the parameters [Begin]
*=gfModalGen("INM04157B00000", "DIALOG",;
*             ALLTRIM(APINVHDR.cCurrCode)+'|' ;
*             +ALLTRIM(lcAprCurCod)+;
*             '|'+DTOC(APINVHDR.dInvDate))

*B601667,1 IF Statment to check if the Invoice currency is the
*          Base Currency
						If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
							=gfModalGen("INM04157B00000", "DIALOG",;
								ALLTRIM(lcAprCurCod)+'|' ;
								+Alltrim(&lcAPINVHDR..cCurrCode)+;
								'|'+Dtoc(&lcAPINVHDR..dInvDate))
						Else    && Else
							=gfModalGen("INM04157B00000", "DIALOG",;
								ALLTRIM(&lcAPINVHDR..cCurrCode)+'|' ;
								+Alltrim(lcAprCurCod)+;
								'|'+Dtoc(&lcAPINVHDR..dInvDate))
						Endif    && End of IF
*B601667,1 Change this line to fix the parameters [End]

					Else
*- Include currency fields update.
*B600808,1 Clear Approved 1099 amount field as well.
*REPLACE CBNKCODE  WITH lcBankCode ,;
CCHKACCT  WITH lcCheckCode,;
CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
SPACE(lnApsAcLen),lcGlAcct),;
NINVAMTAP WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
NINVDISAP WITH NINVDISOF,;
NINVADJAP WITH 0

*B601526,1 Change this line [Begin]
*REPLACE CBNKCODE   WITH lcBankCode ,;
*        CCHKACCT   WITH lcCheckCode,;
*        CCHKGLACC  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
*                                  SPACE(lnApsAcLen),lcGlAcct),;
*        NINVAMTAP  WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
*        NINVDISAP  WITH NINVDISOF,;
*        NINVADJAP  WITH 0,;
*        nInvA1099  WITH 0,;
*        cAprCurCod WITH lcAprCurCod,;
*        nAprCurUnt WITH lnAprCurUnt,;
*        nAprExRat  WITH lnAprExRat

						Replace CBNKCODE   With lcBankCode ,;
							CCHKACCT   With lcCheckCode,;
							CCHKGLACC  With Iif(Empty(Strtran(Strtran(lcGlAcct,'-'),'0')),;
							SPACE(lnApsAcLen),lcGlAcct),;
							nInvAmtAp  With (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
							NINVDISAP  With NINVDISOF,;
							NINVADJAP  With 0,;
							nInvA1099  With 0,;
							cAprCurCod With lcAprCurCod,;
							nAprCurUnt With lnAprCurUnt,;
							nAprExRat  With lnAprExRat,;
							nInvFAAp   With Round((&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)

*B601526,1 Change this line [End]

*=gfAdd_Info()  && Add the audit information to the record.
					Endif
				Else    && Else if payment method is cash payment.
*- Get exchange rate between the checking account
*- currency and the invoice currency.
*- If the currency is different, implies that the system
*- is multi currency, call the global function gfChkRate
*- that validates an exchange rate.
					If &lcAPINVHDR..cCurrCode <> lcAprCshCod
*B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
*B600849,1                currency to approved currency.
*lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCshCod,APINVHDR.dInvDate, ;
.T., .F., APINVHDR.cCurrCode, .T.)

*- Change the calling of gfChkRate so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
*                        .T., .F., lcAprCshCod, .T.)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
						If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
							lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCshCod ,&lcAPINVHDR..dInvDate, ;
								.T., .F., &lcAPINVHDR..cCurrCode, .T.)
						Else      && Else
							lnAprExRat = gfChkRate('lnAprCurUnt', &lcAPINVHDR..cCurrCode ,&lcAPINVHDR..dInvDate, ;
								.T., .F., lcAprCshCod, .T.)
						Endif     && End of IF
*- Change the calling of gfChkRate [END]

*B600849,1 (End)
*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to company base currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
						lcExSin2 = ' '
						lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*FUNCTION  HISH 12/05/95. to exchange currency from invoice currency to approved currency.
						lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCshCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
						If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
							lcExSin3 = gfGetExSin(@lcExSin4,lcAprCshCod,&lcAPINVHDR..cCurrCode)
							lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
							lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
						Else   && Else
							lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCshCod)
						Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')

					Else
						Store 1 To lnAprExRat, lnAprCurUnt
*B601013- HISH  04/18/96. Got the equation signs. (Begin)
*B601013- exchange currency from invoice currency to company base currency.
						lcExSin2 = ' '
						lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)
*B601013- exchange currency from invoice currency to approved currency.
						lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCshCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
						If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
							lcExSin3 = gfGetExSin(@lcExSin4,lcAprCshCod,&lcAPINVHDR..cCurrCode)
							lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
							lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
						Else   && Else
							lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCshCod)
						Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*B601013- (End)
					Endif      && ENDIF APINVHDR.cCurrCode <> lcAprCshCod

*- If the exchange rate = 0, do not approve, and present
*- the following message
					If lnAprExRat = 0
*- IF editing the exchange rate on the fly is not
*- allowed, present the following message:
*- Message : " A valid ð to ð exchange rate could not "
*-           " be found for ð.                        "
*-           "                  ® Ok ¯                "

*B601667,1 Change this line to fix the parameters [Begin]
*=gfModalGen("INM04157B00000", "DIALOG",;
*             ALLTRIM(APINVHDR.cCurrCode)+'|' ;
*             +ALLTRIM(lcAprCshCod)+;
*             '|'+DTOC(APINVHDR.dInvDate))

*B601667,1 IF Statment to check if the Invoice currency is the
*          Base Currency
						If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
							=gfModalGen("INM04157B00000", "DIALOG",;
								ALLTRIM(lcAprCshCod)+'|' ;
								+Alltrim(&lcAPINVHDR..cCurrCode)+;
								'|'+Dtoc(&lcAPINVHDR..dInvDate))
						Else     && Else
							=gfModalGen("INM04157B00000", "DIALOG",;
								ALLTRIM(&lcAPINVHDR..cCurrCode)+'|' ;
								+Alltrim(lcAprCshCod)+;
								'|'+Dtoc(&lcAPINVHDR..dInvDate))
						Endif    && End of IF
*B601667,1 Change this line to fix the parameters [End]

					Else
*- Include currency fields update.
*B600808,1 Clear Approved 1099 amount field as well.
*REPLACE CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
SPACE(lnApsAcLen),lcHGlAct),;
NINVAMTAP WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
NINVDISAP WITH NINVDISOF,;
NINVADJAP WITH 0

*B601526,1 Change this line [Begin]
*REPLACE CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
*                                 SPACE(lnApsAcLen),lcHGlAct),;
*        NINVAMTAP WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
*        NINVDISAP WITH NINVDISOF,;
*        NINVADJAP WITH 0,;
*        nInvA1099  WITH 0,;
*        cAprCurCod WITH lcAprCshCod,;
*        nAprCurUnt WITH lnAprCurUnt,;
*        nAprExRat  WITH lnAprExRat

						Replace CCHKGLACC With Iif(Empty(Strtran(Strtran(lcHGlAct,'-'),'0')),;
							SPACE(lnApsAcLen),lcHGlAct),;
							nInvAmtAp With (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
							NINVDISAP With NINVDISOF,;
							NINVADJAP With 0,;
							nInvA1099  With 0,;
							cAprCurCod With lcAprCshCod,;
							nAprCurUnt With lnAprCurUnt,;
							nAprExRat  With lnAprExRat,;
							nInvFAAp   With Round((&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)

*B601526,1 Change this line [End]

*=gfAdd_Info()  && Add the audit information to the record.
					Endif
				Endif
* Get the totals in base currency.
				lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit
				With loFormSet.ariaform1
					.lnSesAmnt.Value = .lnSesAmnt.Value + Round(&lcAPINVHDR..nInvAmtAp &lcExSin1 lnRateVal, 2)
					.lnSesDisc.Value = .lnSesDisc.Value + Round(&lcAPINVHDR..NINVDISAP &lcExSin1 lnRateVal, 2)
				Endwith
*E303011,1 TMI 12/22/2011 [Start]
*        =gfObj_lock(.F.) && lock the pointed record.
*E303011,1 TMI 12/22/2011 [End  ]
			Endif
			If llExit  &&If Escape pressed.
				lnTotInv1 = lnTotInv2
				If Wvisible("GWDTHERMO")
					Release Window ("GWDTHERMO")
				Endif
				llExit    = .F.
				Exit
			Endif
		Else
*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
			Do Case
			Case CVENPRIOR = '0'
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)+' Invoice No. '+ALLTRIM(CINVNO)
				lcvendorr=Alltrim(APVENDOR.CVENDCODE)+' '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_INVNO,loFormSet.GetHeaderText("LANG_APAPRPA_INVNO",loFormSet.HeaderAlias))+' '+Alltrim(CINVNO)
*N000682,1 MMT 12/09/2012 Globalization changes[END]
** MESSAGE : " Vendor XXXXXX has payment priority 0."
**           " This vendor in on hold.              "
**           "                ® OK ¯                "
				=gfModalGen("TRM04060B00000","DIALOG",lcvendorr)

			Case APVENDOR.CVENPRIOR = '0' .And. lcVen0Prior <> CVENDCODE
				lcvendorr=Alltrim(APVENDOR.CVENDCODE)
** MESSAGE : " Vendor XXXXXX has payment priority 0."
**           " This vendor in on hold.              "
**           "                ® OK ¯                "
				=gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
			Endcase
			lcVen0Prior = CVENDCODE
			lnTotInv2 = lnTotInv2 + 1

			lnThermNo = lnThermNo + 1
			oProgress.CurrentProgress(lnThermNo)
			oProgress.lblFirstLabel.Caption = APINVHDR.CVENDCODE
		Endif
	Endscan
	oProgress = Null
*- Check if the Thermometer window exist then force it to close
*!*    IF WEXIST('gwdThermo')
*!*      =gfThermo(lnTotInv1,lnTotInv1,"Approving invoices ...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
*!*    ENDIF
	Select (lcCurAlias)
	Go Bottom
Endif

Set Escape &lcEscap

***********************************************************************************
*Name      : lfGetDefu
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : get dufault bank, check, and gl account.
***********************************************************************************
Function lfGetDefu
Parameters loFormSet
*B600808,1 Remove the following check. Get the default anyway.
*IF EMPTY(lcBankCode)
*B600808,1 Get the defaul bank code, checking account and G/L checking
*B600808,1 account if the payment method is not cash payment,
*B600808,1 otherwise get the default cash payment account.
If APINVHDR.CVENPMETH <> 'H'
	Do Case
	Case !Empty(APINVHDR.CBNKCODE) && if found values in invoice header file.
		lcBankCode = APINVHDR.CBNKCODE
		lcCheckCode= APINVHDR.CCHKACCT
		lcGlAcct   = Iif(!Empty(APINVHDR.CCHKGLACC),APINVHDR.CCHKGLACC, loFormSet.ap1.lcEmptyAcc)
*B600808,1 Get cash payment account below. Removed.
*lcHGlAct   = lcGlAcct
	Case !Empty(APVENDOR.CBNKCODE) && if found values in vendor file.
		lcBankCode = APVENDOR.CBNKCODE
		lcCheckCode= APVENDOR.CCHKACCT
*B600808,1 Clear G/L account
		lcGlAcct   = loFormSet.ap1.lcEmptyAcc
	Case !Empty(APDIV.CBNKCODE)    && if found values in division file.
		lcBankCode = APDIV.CBNKCODE
		lcCheckCode= APDIV.CCHKACCT
*B600808,1 Clear G/L account
		lcGlAcct   = loFormSet.ap1.lcEmptyAcc
	Otherwise                      && get value from setup file.
		lcBankCode = APSETUP.CBNKCODE
		lcCheckCode= APSETUP.CCHKACCT
*B600808,1 Clear G/L account
		lcGlAcct   = loFormSet.ap1.lcEmptyAcc
	Endcase
*** get account if there is a bank code and check code available. ***
*B600808,1 Seek the checking account whether or not the G/L account
*B600808,1 is empty.
*IF  EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
.AND. !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
	If !Empty(lcBankCode) .And. !Empty(lcCheckCode)
		If Seek(lcBankCode+lcCheckCode,'APCHECKS')
			If Empty(Strtran(Strtran(lcGlAcct,'-'),'0'))
				lcGlAcct    = Iif(!Empty(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC, loFormSet.ap1.lcEmptyAcc)
*B600808,1 Get cash payment account below. Removed.
*lcHGlAct   = lcGlAcct
			Endif    && ENDIF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))

*- Get default currency of the checking account.
			lcAprCurCod   = APCHECKS.cCurrCode
*- Get a corresponding exchange rate
*- between the approval currency and the invoice currency.
*- If the currency is different, implies that the system
*- is multi currency, call the global function gfChkRate
*- that validates an exchange rate.
			If APINVHDR.cCurrCode <> lcAprCurCod
*B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
*B600849,1                currency to approved currency.
*lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod,APINVHDR.dInvDate, ;
!llAprPart, .F., APINVHDR.cCurrCode, .T.)

*- Change the calling of gfChkRate so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
*                        !llAprPart, .F., lcAprCurCod, .T.)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
				If APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
					lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,APINVHDR.dInvDate, ;
						!llAprPart, .F., APINVHDR.cCurrCode, .T.)
				Else      && Else
					lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
						!llAprPart, .F., lcAprCurCod, .T.)
				Endif     && End of IF
*- Change the calling of gfChkRate [END]

*B600849,1 (End)
*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to company base currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
				lcExSin2 = ' '
				lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*FUNCTION  HISH 12/05/95. to exchange currency from invoice currency to approved currency.
				lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
				If APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
					lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
					lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
					lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
				Else   && Else
					lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
				Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')

			Else
				Store 1 To lnAprExRat, lnAprCurUnt
*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to company base currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
				lcExSin2 = ' '
				lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*FUNCTION  to exchange currency from invoice currency to approved currency.
				lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
				If APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
					lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
					lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
					lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
				Else   && Else
					lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
				Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')

			Endif      && ENDIF APINVHDR.cCurrCode <> lcAPrCurCod
		Endif      && ENDIF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
	Endif      && ENDIF !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)

*B600808,1 Else, if payment method is cash payment
Else
*B600808,1 Get the default cash payment account
	lcGlAcct = Iif(!Empty(APINVHDR.CCHKGLACC), APINVHDR.CCHKGLACC,;
		IIF(!Empty(APVENDOR.cCashAcct), APVENDOR.cCashAcct, ;
		IIF(!Empty(APDIV.cCashAcct), APDIV.cCashAcct, ;
		IIF(!Empty(APSETUP.cCashAcct), APSETUP.cCashAcct, ;
		loFormSet.ap1.lcEmptyAcc))))
*- If not multi currency,
*- or it the payment method is cash payment,
*- Default currency with that of the invoice.
	lcAprCurCod = APINVHDR.cCurrCode
	Store 1 To lnAprExRat, lnAprCurUnt
*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to company base currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
	lcExSin2 = ' '
	lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*FUNCTION   to exchange currency from invoice currency to approved currency.
	lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
	If APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
		lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
		lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
		lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
	Else   && Else
		lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
	Endif  && End of IF
*- Change the calling of gfGetExSin [END]

Endif

***********************************************************************************
*Name      : lfvApprPart
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : this function calling from push button approve fully and partialy
***********************************************************************************
Function lfvApprPart
Parameters loFormSet,llAprPart

*E303011,1 TMI 12/22/2011 [Start] do locking when clicking the approve button itself
*IF !lfObj_lock(.T.)
*  RETURN
*ENDIF
*E303011,1 TMI 12/22/2011 [End  ]
Set Step On
*E303011,1 TMI 12/21/2011 [Start] define variables
Local lnI,lcVar
For lnI = 1 To Alen(loFormSet.laPropArr,1)
	lcVar = loFormSet.laPropArr[lnI,1]
	&lcVar = loFormSet.laPropArr[lnI,2]
Endfor
*- locate the same line in the APINVHDR file

lcAPINVHDR = loFormSet.lcAPINVHDR
=Seek(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
=Seek(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
*E303011,1 TMI 12/22/2011 [End  ]

*** get the default values from approved values. ***
=lfGetDefu(loFormSet)
*** Get approve to pay and approve discount defalt. ***
*B600846,1 HISH 11/29/95. Moved this part below with the same bug number (Begin)
*lnAprToPay=APINVHDR.NINVAMNT-NINVPAID-NINVADJ-;
IIF(APINVHDR.NINVDISTK<APINVHDR.NINVDISOF,;
APINVHDR.NINVDISOF-APINVHDR.NINVDISTK,0.00)
*lnAprDisc=IIF(APINVHDR.NINVDISTK<APINVHDR.NINVDISOF,;
APINVHDR.NINVDISOF-APINVHDR.NINVDISTK,0.00)
*B600846,1. (End)

lcOldBank   = lcBankCode
lcOldCheck  = lcCheckCode
lcOldGlAcc  = lcGlAcct
lcHOldGlAc  = lcHGlAct

*- Store old currency fields values
lcOldCurr   = lcAprCurCod
lnOldUnt    = lnAprCurUnt
lnOldExRat  = lnAprExRat

*E303011,1 TMI 12/22/2011 [Start]
*!*  PUSH KEY
*!*  ON KEY
*E303011,1 TMI 12/22/2011 [End  ]

*** In case of approve partially get old values of payment, discount, and adjustment.
*** calling approved partially window.
If APVENDOR.CVENPRIOR = '0' .Or. APINVHDR.CVENPRIOR = '0'
	lcvendorr=Alltrim(APVENDOR.CVENDCODE)
** MESSAGE : " Vendor XXXXXX has payment priority 0."
**           " This vendor in on hold.              "
**           "                ® OK ¯                "
	=gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
	Return
Else
	If APINVHDR.CINVSTAT = 'A'
*N000682,1 MMT 11/20/2012 Globalization project[START]
*lcCanApr= "approve"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCanApr= LANG_APAPRPA_APPROVE
		lcCanApr= Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_APPROVE,loFormSet.GetHeaderText("LANG_APAPRPA_APPROVE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/20/2012 Globalization project[End]
** MESSAGE : " You Can not CCCC a debit memo created by an advanced"
**           " payment.                                            "
**           "                    ® Ok ¯                           "
		=gfModalGen("TRM04058B00000","DIALOG",lcCanApr)
		Return
	Else
*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
*    IF (APINVHDR.NINVAMNT < 0) .AND. (APINVHDR.CVENPRIOR = '0')
		If (APINVHDR.NINVAMNT < 0) .And. (APINVHDR.CVENPRIOR = '0' .Or. APVENDOR.CVENPRIOR = '0')
*N000682,1 MMT 11/20/2012 Globalization project[START]
*lcDebit="Debit memo "+APINVHDR.CINVNO
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcDebit=LANG_APAPRPA_DEBITMEMO+APINVHDR.CINVNO
			lcDebit=Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_DEBITMEMO,loFormSet.GetHeaderText("LANG_APAPRPA_DEBITMEMO",loFormSet.HeaderAlias))+APINVHDR.CINVNO
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/20/2012 Globalization project[End]
** MESSAGE : " YYYY XXXXX has payment priority 0 "
**           " YYYY is on hold.                  "
**           "                    ® Ok ¯         "
			=gfModalGen("TRM04059B00000","DIALOG",lcDebit+"|"+lcTDebitM)
			Return
		Else
*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
*      IF (APINVHDR.NINVAMNT > 0) .AND. (APINVHDR.CVENPRIOR = '0')
			If (APINVHDR.NINVAMNT > 0) .And. (APINVHDR.CVENPRIOR = '0' .Or. APVENDOR.CVENPRIOR = '0')
*N000682,1 MMT 11/20/2012 Globalization project[START]
*lcDebit="Invoice"+ALLTRIM(APINVHDR.CINVNO)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcDebit=LANG_APAPRPA_C_INVOICE+ALLTRIM(APINVHDR.CINVNO)
				lcDebit=Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_C_INVOICE,loFormSet.GetHeaderText("LANG_APAPRPA_C_INVOICE",loFormSet.HeaderAlias))+Alltrim(APINVHDR.CINVNO)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/20/2012 Globalization project[END]
** MESSAGE : " YYYY XXXXX has payment priority 0 "
**           " YYYY is on hold.                  "
**           "                    ® Ok ¯         "
				=gfModalGen("TRM04059B00000","DIALOG",lcDebit+"|"+lcTInvoice)
				Return
			Else
				If llAprPart  && Case approve part.
					If Empty(APVENDOR.CVEN1099t)  && If the vendor 1099 amount is empty.
						lc1099St = 'DISABLE'
					Else
						lc1099St = 'ENABLE'
					Endif

					lnOldAprTPy = lnAprToPay
					lnOldAprDisc= lnAprDisc
					lnOldAprAdj = lnAprAdj
					lnO1099amnt = ln1099amnt

*B600846,1 HISH 11/29/95. Put variable defualt values in a condition, (Begin)
*B600846,1                that if this invoice aproved priviously put
*B600846,1                the last approve amount other with put the
*B600846,1                open amount - paid amount - adjusted amount -discount.
					If &lcAPINVHDR..nInvAmtAp = 0
						lnAprToPay=&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVADJ-;
							IIF(&lcAPINVHDR..NINVDISTK<&lcAPINVHDR..NINVDISOF,;
							&lcAPINVHDR..NINVDISOF-&lcAPINVHDR..NINVDISTK,0.00)
						lnAprDisc=Iif(&lcAPINVHDR..NINVDISTK<&lcAPINVHDR..NINVDISOF,;
							&lcAPINVHDR..NINVDISOF-&lcAPINVHDR..NINVDISTK,0.00)
			*B611740,1 ES 3/12/2019 Partially Approve screen has no decimals in 1099 and adjustment amounts in Approve for payment screen [Start]
						*lnAprAdj   = 0
						*ln1099amnt = 0
                         lnAprAdj   = 0.00
                         ln1099amnt = 0.00
            *B611740,1 ES 3/12/2019 Partially Approve screen has no decimals in 1099 and adjustment amounts in Approve for payment screen [End]

					Else
*B600808,1 Initialize variables prior to the screen entry
*B600808,1 instead of the when of the browse.
						lnAprToPay  = &lcAPINVHDR..nInvAmtAp
						lnAprDisc   = &lcAPINVHDR..NINVDISAP
						lnAprAdj    = &lcAPINVHDR..NINVADJAP
						ln1099amnt  = &lcAPINVHDR..nInvA1099
					Endif
*B600846,1 (END)
*!E303922,1 AHH 01/29/2018 Aria 4XP - A report shows all the 1099 in details forms [T20171208.0136][Start]
					If (APSETUP.LDEF1099AP=.T.).And.!Empty(APVENDOR.CVEN1099t)
						ln1099amnt  =lnAprToPay
					Endif
*!E303922,1 AHH 01/29/2018 Aria 4XP - A report shows all the 1099 in details forms [T20171208.0136][End]
					llCanclAp   = .F.

*- Get the approved to pay amount in approval currency.
*FUNCTION  HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
*lnExchAmnt  = IIF(lnAprCurUnt > 0,;
ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2),;
0)
					lnExchAmnt  = Iif(lnAprCurUnt > 0 And lnAprExRat > 0,;
						ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2),;
						0)

*- Disable rate field if editing is not allowed,
*- or if the currency code is that of the invoice.
*- or if the checking account is empty,
					lcRateDisp  = Iif(!loFormSet.llEditEx .Or. ;
						&lcAPINVHDR..cCurrCode = &lcAPINVHDR..cAprCurCod .Or.;
						(&lcAPINVHDR..CVENPMETH <> 'H' .And. ;
						EMPTY(lcCheckCode)),;
						'DISABLE', 'ENABLE')

					If APINVHDR.CVENPMETH <> 'H'
*,1 Call *.SPR from screens directory
* DO APAPRPRT.SPR  && calling the approved all window.
*Old : DO (gcScrDir + gcWinAppl + '\APAPRPRT.SPR')
*,1 end

*B608903,1 TMI [Start] Release the lock after the screen has been closed
*SELECT APINVHDR
*E303011,1 TMI 12/22/2011 [Start]
*=lfObj_lock(.F.)
*E303011,1 TMI 12/22/2011 [Start]
*=gfObj_lock(.F.)
*E303011,1 TMI 12/22/2011 [End  ]
*E303011,1 TMI 12/22/2011 [End  ]
*B608903,1 TMI [End  ] Release the lock after the screen has been closed

					Else


*B601569,1 Add this line to set the default currency and
*          exchange rate [Begin]
						=lfSetDef()
*B601569,1 Add this line [End]

*,1 Call *.SPR from screens directory
* DO APAPPRT2.SPR
*Old : DO (gcScrDir + gcWinAppl + '\APAPPRT2.SPR')
*,1 end

					Endif

					Do Form (oAriaApplication.ScreenHome+"\AP\APAPRPRT.SCX") With loFormSet

***Refresh the invoice push button. ***
***Refresh say field open apen amount. ***
*Old : =lfRefresh()
					lnBrRecNo  = Recno(lcAPINVHDR)
*Old : SHOW WINDOW (lcBrTtl) REFRESH
				Endif
			Endif
		Endif
	Endif
Endif

loFormSet.ariaform1.grdAPROVE.AfterRowColChange()
loFormSet.ariaform1.Refresh()
*- End of lfvApprPart

***********************************************************************************
*Name      : lfvAprov
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Validation function of the <ok> push button of the Approved for payment.
***********************************************************************************
Function lfvAprov
Parameters loAprFormSet,llAprPart

*- this function is called either from ARAPRPRT.SCX or APAPRPA.SCX
If Type('loAprFormSet.CallingForm')='O'
* called from ARAPRPRT.SCX
	loFormSet = loAprFormSet.Callingform
Else
* called from APAPRPA.SCX
	loFormSet = loAprFormSet
Endif

*E303011,1 TMI 12/18/2011 [Start]
*!*  As there is a temp file to hold the lines of APINVHDR to approve , any line that has been updated will be marked in the LLOC_STAT field
*!*  This field will be cleared when collecting data into the temp file at the first time
*!*  When saving, we loop over this field and update the APINVHDR accordingly
*!*  When IGNORE is clicked, data would be recollected from APINVHDR
*E303011,1 TMI 12/18/2011 [End  ]

*** in case of approve partially. ***
If llAprPart            && approve part validation.
***if there is bankcode, check, and account but there isn't any payment, discount, or adjustment.
*B600808,1 Change validation sequence
*IF !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) .AND.;
lnAprToPay+lnAprDisc+lnAprAdj  = 0
	Do Case
*B600808,1 Copied from below.
*B600808,1 Check if approved to pay amount is not zero.
*** in case of not enter approved to pay amount. ***
*CASE !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
.AND. lnAprToPay = 0
	Case lnAprToPay = 0
** MESSAGE : " You have to enter the approved to pay   "
**           " amount.                                 "
**           "                    ® Ok ¯               "
		=gfModalGen("TRM04022B00000","DIALOG")
*SHOW GET lnAprToPay
*old :_CUROBJ = OBJNUM(lnAprToPay)
		loAprFormSet.ariaform1.txtinvamtap.SetFocus()
		Return .F.

*B600808,1 Remove. There is no check for non check payment,
*B600808,1 probably meant cash payment. Substitute by the
*B600808,1 following case. In case of cash payments.
*** in case of non check payment. ***
*CASE CVENPMETH = 'N' .AND. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))
** MESSAGE : You have to enter the ð account.
**           "                    ® Ok ¯               "
*=gfModalGen("TRM04020B00000","DIALOG",'GL')
	Case &lcAPINVHDR..CVENPMETH = 'H' .And. Empty(Strtran(Strtran(lcGlAcct,'-'),'0'))
*B600808,1  Message : " You have to enter the ð account.     "
*B600808,1                       ®  Ok  ¯
		=gfModalGen("TRM04020B00000","DIALOG",lcTPayAcct)
*old : _CUROBJ = OBJNUM(lcGlAcct)
		loAprFormSet.ariaform1.glChkActCode.keyTextBox.SetFocus()
		Return .F.

*B600808,1 Remove. There is no special case for non check payment,
*B600808,1 probably meant cash payment. Substitute by the
*B600808,1 following case (in case of payment methods other than
*B600808,1 cash payments).
*** in case of there isn't bank, or check, or account. ***
*CASE CVENPMETH <> 'N' .AND.;
*     lnAprToPay+lnAprDisc+lnAprAdj > 0 .AND.;
*     (EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
*                        .OR. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'));
*                        .OR. (ATC("?",lcGlAcct)>0) )
	Case &lcAPINVHDR..CVENPMETH <> 'H'  .And.;
			(Empty(lcBankCode) .Or.;
			EMPTY(lcCheckCode) .Or.;
			EMPTY(Strtran(Strtran(lcGlAcct,'-'),'0')))
** MESSAGE : " You have to enter the bank code,the     "
**           " checking account,and the GL account.    "
**           "                    ® Ok ¯               "
		=gfModalGen("TRM04021B00000","DIALOG")
		If Empty(lcBankCode)
*SHOW GET lcBankCode
*_CUROBJ = OBJNUM(lcBankCode)
			loAprFormSet.ariaform1.BankChk.kbBanks.keyTextBox.SetFocus()
		Else
			If Empty(lcCheckCode)
*SHOW GET lcCheckCode
*_CUROBJ = OBJNUM(lcCheckCode)
				loAprFormSet.ariaform1.BankChk.kbChkAccount.keyTextBox.SetFocus()
			Else
*SHOW GET lcGlAcct
*_CUROBJ = OBJNUM(lcGlAcct)
				loAprFormSet.ariaform1.glChkActCode.keyTextBox.SetFocus()
			Endif
		Endif
		Return .F.

*B600808,1 Copied above to match the payable invoices validation.
*** in case of not enter approved to pay amount. ***
*CASE !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
*     .AND. lnAprToPay = 0
*  ** MESSAGE : " You have to enter the approved to pay   "
*  **           " amount.                                 "
*  **           "                    ® Ok ¯               "
*  =gfModalGen("TRM04022B00000","DIALOG")
*  SHOW GET lnAprToPay
*  _CUROBJ = OBJNUM(lnAprToPay)
*  RETURN

*B600808,1 Check for 1099 amount. Change check condition.
*CASE !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
.AND. (!BETWEEN(ln1099amnt,lnAprToPay,0);
.AND. !BETWEEN(ln1099amnt,0,lnAprToPay))
	Case &lcAPINVHDR..CVENPMETH <> 'H'  .And. !loFormSet.ap1.llApS1099 .And.;
			(!Between(ln1099amnt,lnAprToPay,0) .And. ;
			!Between(ln1099amnt,0,lnAprToPay))
** MESSAGE : " The 1099 amount cannot be greater than  "
**           " the ð.             "
**           "                 ® Ok ¯
*N000682,1 MMT 11/20/2012 Globalization project[START]                "
*=gfModalGen("TRM04061B00000","DIALOG","approved amount to pay")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM04061B00000","DIALOG",LANG_APAPRPA_APPROVEAMOUNT)
		=gfModalGen("TRM04061B00000","DIALOG",Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_APPROVEAMOUNT,loFormSet.GetHeaderText("LANG_APAPRPA_APPROVEAMOUNT",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/20/2012 Globalization project[End]
		ln1099amnt=lnO1099amnt
		Show Get ln1099amnt
* Old : _CUROBJ = OBJNUM(ln1099amnt)
		loAprFormSet.ariaform1.txtinva1099.SetFocus()
		Return .F.

*- Add a case for empty exchange rate
	Case (&lcAPINVHDR..CVENPMETH = 'H' .Or. !Empty(lcBankCode)) .And. lnAprExRat = 0
		If lcAprCurCod <> &lcAPINVHDR..cCurrCode
*B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
*B600849,1                currency to approved currency.
*lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod,APINVHDR.dInvDate, ;
.T., .F., APINVHDR.cCurrCode, .T.)

*- Change the calling of gfChkRate so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
*                        .T., .F., lcAprCurCod, .T.)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
			If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
				lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,&lcAPINVHDR..dInvDate, ;
					.T., .F., &lcAPINVHDR..cCurrCode, .T.)
			Else      && Else
				lnAprExRat = gfChkRate('lnAprCurUnt', &lcAPINVHDR..cCurrCode ,&lcAPINVHDR..dInvDate, ;
					.T., .F., lcAprCurCod, .T.)
			Endif     && End of IF
*- Change the calling of gfChkRate [END]

*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to company base currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
			lcExSin2 = ' '
			lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*FUNCTION  to exchange currency from invoice currency to approved currency.
			lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
			If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
				lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,&lcAPINVHDR..cCurrCode)
				lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
				lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
			Else   && Else
				lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCurCod)
			Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')

*B600849,1 (End)
*- If the exchange rate = 0, do not approve, and present
*- the following message
			If lnAprExRat = 0
*- IF editing the exchange rate on the fly is not
*- allowed, present the following message:
*- Message : " A valid ð to ð exchange rate could not "
*-           " be found for ð.                        "
*-           "                  ® Ok ¯                "

*B601667,1 Change this line to fix the parameters [Begin]
*=gfModalGen("INM04157B00000", "DIALOG",;
*             ALLTRIM(APINVHDR.cCurrCode)+'|' ;
*             +ALLTRIM(lcAprCurCod)+;
*             '|'+DTOC(APINVHDR.dInvDate))

*B601667,1 IF Statment to check if the Invoice currency is the
*          Base Currency
				If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
					=gfModalGen("INM04157B00000", "DIALOG",;
						ALLTRIM(lcAprCurCod)+'|' ;
						+Alltrim(&lcAPINVHDR..cCurrCode)+;
						'|'+Dtoc(&lcAPINVHDR..dInvDate))
				Else     && Else
					=gfModalGen("INM04157B00000", "DIALOG",;
						ALLTRIM(&lcAPINVHDR..cCurrCode)+'|' ;
						+Alltrim(lcAprCurCod)+;
						'|'+Dtoc(&lcAPINVHDR..dInvDate))
				Endif     && End of IF
*B601667,1 Change this line to fix the parameters [End]

				Return .F.
			Endif
		Else
			Store 1 To lnAprExRat, lnAprCurUnt
		Endif

*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
*old     CASE &lcAPVENDOR..CVENPRIOR = '0' .OR. CVENPRIOR = '0'
	Case APVENDOR.CVENPRIOR = '0' .Or. &lcAPINVHDR..CVENPRIOR = '0'
		lcvendorr=Alltrim(APVENDOR.CVENDCODE)
** MESSAGE : " Vendor XXXXXX has payment priority 0."
**           " This vendor in on hold.              "
**           "                ® OK ¯                "
		=gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
		Return .F.
	Otherwise
* Old:     CLEAR READ
	Endcase

*B600808,1 Removed IF..ELSE..ENDIF condition
*ELSE
*  IF (EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
*                          .OR.  EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'));
*                          .OR. (ATC("?",lcGlAcct)>0) )
*    ** MESSAGE : " You have to enter the bank code,the     "
*    **           " checking account,and the GL account.    "
*    **           "                    ® Ok ¯               "
*    =gfModalGen("TRM04021B00000","DIALOG")
*    RETURN
*  ENDIF
*CLEAR READ
*ENDIF

Else && in case of approved fully

*E303011,1 TMI 12/21/2011 [Start] define variables
	Local lnI,lcVar
	For lnI = 1 To Alen(loFormSet.laPropArr,1)
		lcVar = loFormSet.laPropArr[lnI,1]
		&lcVar = loFormSet.laPropArr[lnI,2]
	Endfor
*E303011,1 TMI 12/21/2011 [End  ]

*E303011,1 TMI 12/22/2011 [Start] locate the same line in the APINVHDR file
	lcAPINVHDR = loFormSet.lcAPINVHDR
	=Seek(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
	=Seek(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
	=lfGetDefu(loFormSet)
*E303011,1 TMI 12/22/2011 [End  ]

*B600808,1 If payment method is not cash payment,
	If &lcAPINVHDR..CVENPMETH <> 'H'
		If (Empty(lcBankCode) .Or. Empty(lcCheckCode);
				.Or.  Empty(Strtran(Strtran(lcGlAcct,'-'),'0'));
				.Or. (Atc("?",lcGlAcct)>0) )
** MESSAGE : " You have to enter the bank code,the     "
**           " checking account,and the GL account.    "
**           "                    ® Ok ¯               "
			=gfModalGen("TRM04021B00000","DIALOG")
			Return
		Endif

*- If multi currency and the exchange rate is zero,
*- do not approve
		If loFormSet.llMultiCr .And. lnAprExRat = 0
*- IF editing the exchange rate on the fly is not
*- allowed, present the following message:
*- Message : " A valid ð to ð exchange rate could not "
*-           " be found for ð.                        "
*-           "                  ® Ok ¯                "

*B601667,1 IF Statment to check if the Invoice currency is the
*          Base Currency
			If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
				=gfModalGen("INM04157B00000", "DIALOG",;
					ALLTRIM(lcAprCurCod)+'|' ;
					+Alltrim(&lcAPINVHDR..cCurrCode)+;
					'|'+Dtoc(&lcAPINVHDR..dInvDate))
			Else    && Else
				=gfModalGen("INM04157B00000", "DIALOG",;
					ALLTRIM(&lcAPINVHDR..cCurrCode)+'|' ;
					+Alltrim(lcAprCurCod)+;
					'|'+Dtoc(&lcAPINVHDR..dInvDate))
			Endif     && End of IF
			Return
		Endif    && ENDIF llMultiCr .AND. lnAprExRat = 0
	Endif    && ENDIF APINVHDR.cVenPMeth <> 'H'
*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
* old  IF APVENDOR.CVENPRIOR = '0' .OR. CVENPRIOR = '0'
	If APVENDOR.CVENPRIOR = '0'  .Or. &lcAPINVHDR..CVENPRIOR = '0'
		lcvendorr=Alltrim(APVENDOR.CVENDCODE)
** MESSAGE : " Vendor XXXXXX has payment priority 0."
**           " This vendor in on hold.              "
**           "                ® OK ¯                "
		=gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
		Return
	Endif
Endif

Select APINVHDR

*** lock the pointed record and replaced the approved values. ***
*B128400,1 WAM 10/12/2006 Commented out. Invoice already locked when click Partial approve
*IF lfObj_lock(.T.) && Added by TMI 08/24/2006
*E303011,1 TMI 12/22/2011 [Start] use the global function gfObj_lock
*IF IIF(llAprPart, .T., lfObj_lock(.T.) )

*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][Start]
*If &lcAPINVHDR..COWNER='ARIA' Or gfObj_Lock(.T.)
IF &lcAPINVHDR..COWNER='ARIA' Or lfObj_lock(.T.)
*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][End]
*E303011,1 TMI 12/22/2011 [End  ]
*E303011,1 TMI 12/22/2011 [Start]
	Select (loFormSet.lcAPINVHDR)
*replace LLOK_STAT WITH .T.
	Replace COWNER With 'ARIA'  && this is used as a tag, this means that this record has been changed by the approve to pay program
*E303011,1 TMI 12/22/2011 [End  ]
*B128400,1 WAM 10/12/2006 (End)
	If &lcAPINVHDR..CVENPMETH <> "H"

*B601519,1 IF Statment to check if the Approved amount in the Approved
*             currency is not 0 or this vlidation function is not for
*             the Push Button pbApprov
		If lnExchAmnt <> 0 .Or. Sys(18) <> 'PBAPPROV'
			Replace CBNKCODE  With lcBankCode ,;
				CCHKACCT  With lcCheckCode,;
				CCHKGLACC With Iif(Empty(Strtran(Strtran(lcGlAcct,'-'),'0')),;
				SPACE(lnApsAcLen), lcGlAcct)
		Endif       && End of IF
*B601519,1 Change this line [End]

	Else
		If !Empty(Strtran(Strtran(lcGlAcct,'-'),'0'))

*B601519,1 Change this line [Begin]
*REPLACE CCHKGLACC WITH lcGlAcct

*B601519,1 IF Statment to check if the Approved amount in the Approved
*             currency is not 0 or this vlidation function is not for
*             the Push Button pbApprov
			If lnExchAmnt <> 0 .Or. Sys(18) <> 'PBAPPROV'
				Replace CCHKGLACC With lcGlAcct
			Endif
*B601519,1 Change this line [End]

		Else
** MESSAGE : " You have to enter the bank code,the     "
**           " checking account,and the GL account.    "
**           "                    ® Ok ¯               "
			=gfModalGen("TRM04021B00000","DIALOG")
		Endif
	Endif
***in case of approved partially replaced the rest of approved values. ***
	If llAprPart   && approve partially.

		lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit

		If lnExchAmnt <> 0 .Or. Sys(18) <> 'PBAPPROV'
			loFormSet.ariaform1.lnSesAmnt.Value = loFormSet.ariaform1.lnSesAmnt.Value + Round((lnAprToPay - nInvAmtAp) &lcExSin1 lnRateVal, 2)
		Endif    && End of IF



		With loFormSet.ariaform1
			.lnSesDisc.Value = .lnSesDisc.Value + Round((lnAprDisc  - NINVDISAP) &lcExSin1 lnRateVal, 2)
			.lnSesAdj.Value  = .lnSesAdj.Value  + Round((lnAprAdj   - NINVADJAP) &lcExSin1 lnRateVal, 2)
			.lnSes1099.Value = .lnSes1099.Value + Round((ln1099amnt - nInvA1099) &lcExSin1 lnRateVal, 2)
		Endwith

*B601519,1 IF Statment to check if the Approved amount in the Approved
*             currency is not 0
		If lnExchAmnt <> 0

			Replace nInvAmtAp  With lnAprToPay ,;
				NINVDISAP  With lnAprDisc  ,;
				NINVADJAP  With lnAprAdj   ,;
				nInvA1099  With ln1099amnt ,;
				cAprCurCod With lcAprCurCod,;
				nAprExRat  With lnAprExRat ,;
				nAprCurUnt With lnAprCurUnt,;
				nInvFAAp   With lnExchAmnt

		Endif   && End of IF

	Else           && approve fully.

		lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit
		With loFormSet.ariaform1
			.lnSesAmnt.Value = .lnSesAmnt.Value + Round((&lcAPINVHDR..NINVAMNT ;
				- &lcAPINVHDR..NINVPAID ;
				- &lcAPINVHDR..NINVDISTK;
				- &lcAPINVHDR..NINVADJ  ;
				- &lcAPINVHDR..NINVDISOF;
				- &lcAPINVHDR..nInvAmtAp) &lcExSin1 lnRateVal, 2)
			.lnSesDisc.Value = .lnSesDisc.Value + Round((&lcAPINVHDR..NINVDISOF;
				- &lcAPINVHDR..NINVDISAP) &lcExSin1 lnRateVal, 2)
			.lnSesAdj.Value  = .lnSesAdj.Value  - Round(&lcAPINVHDR..NINVADJAP   &lcExSin1 lnRateVal, 2)
*E303922,1 AHH 01/29/2018 Aria 4XP - A report shows all the 1099 in details forms [T20171208.0136][Start]
			If (APSETUP.LDEF1099AP=.T.).And.!Empty(APVENDOR.CVEN1099t)
				.lnSes1099.Value = .lnSes1099.Value - Round((&lcAPINVHDR..NINVAMNT ;
					- &lcAPINVHDR..NINVPAID ;
					- &lcAPINVHDR..NINVDISTK;
					- &lcAPINVHDR..NINVADJ  ;
					- &lcAPINVHDR..NINVDISOF;
					- &lcAPINVHDR..nInvAmtAp) &lcExSin1 lnRateVal, 2)
			Else
*E303922,1 AHH 01/29/2018 Aria 4XP - A report shows all the 1099 in details forms [T20171208.0136][End]
				.lnSes1099.Value = .lnSes1099.Value - Round(&lcAPINVHDR..nInvA1099   &lcExSin1 lnRateVal, 2)
*E303922,1 AHH 01/29/2018 Aria 4XP - A report shows all the 1099 in details forms [T20171208.0136][Start]
			Endif
*E303922,1 AHH 01/29/2018 Aria 4XP - A report shows all the 1099 in details forms [T20171208.0136][End]
			.Refresh()
		Endwith

		Replace nInvAmtAp  With (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
			NINVDISAP  With NINVDISOF,;
			NINVADJAP  With 0,;
			nInvA1099  With 0,;
			cAprCurCod With lcAprCurCod,;
			nAprExRat  With lnAprExRat ,;
			nAprCurUnt With lnAprCurUnt,;
			nInvFAAp   With Round((NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)

*E303922,1 AHH 01/29/2018 Aria 4XP - A report shows all the 1099 in details forms [T20171208.0136][Start]
		If (APSETUP.LDEF1099AP=.T.) .And. !Empty(APVENDOR.CVEN1099t)
			Replace nInvA1099 With nInvAmtAp
		Endif
*E303922,1 AHH 01/29/2018 Aria 4XP - A report shows all the 1099 in details forms [T20171208.0136][End]

	Endif

*=gfAdd_Info()  && Add the audit information to the record.
*E303011,1 TMI 12/22/2011 [Start] unlock as a whole when saving
*=gfObj_lock(.F.)  && unlock the pointed record.
*E303011,1 TMI 12/22/2011 [End  ]

*B601519,1 IF Statment to check if the Approved amount in the Approved
*             currency is not 0 or this vlidation function is not for
*             the Push Button pbApprov
	If lnExchAmnt <> 0 .Or. Sys(18) <> 'PBAPPROV'
		Skip Iif(Eof(),0,1)   && if !eof go to the next record.
		Skip Iif(Eof(),-1,0)  && if went to eof return to the last record.
	Endif   && End of IF
*B601519,1 Change this lines [Begin]

	If !llAprPart
***Refresh say field open apen amount. ***
		loFormSet.ariaform1.Refresh()
		lnBrRecNo  = Recno('&lcAPINVHDR')
	Endif

Endif

lnExchAmnt  = Iif(lnAprCurUnt > 0 And lnAprExRat > 0,;
	ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)
Show Get lnAprExRat
Show Get lnExchAmnt    &&

loFormSet.ariaform1.grdAPROVE.AfterRowColChange()
loFormSet.ariaform1.Refresh()
*- End of lfvAprov

***********************************************************************************
*Name      : lfvCanPay
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Validation function of the cancel push button of the Approved for payment.
***********************************************************************************
Function lfvCanPay

*** restore the old approved values. ***
lcBankCode  = lcOldBank
lcCheckCode = lcOldCheck
lcGlAcct    = lcOldGlAcc
lcHGlAct    = lcHOldGlAc

lfvCanPay  = .T.

*** restore the old approved values in case of approved partially. ***
If llAprPart

*- Restore old currency fields values
	lcAprCurCod = lcOldCurr
	lnAprCurUnt = lnOldUnt
	lnAprExRat  = lnOldExRat

	lnAprToPay = lnOldAprTPy
	lnAprDisc  = lnOldAprDisc
	lnAprAdj   = lnOldAprAdj
	ln1099amnt = lnO1099amnt
Endif

*SELECT APINVHDR
llCanclAp   = .T.      &&set cancelled flag
*old : CLEAR READ


***********************************************************************************
*Name      : lfvAprDisc
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Valid function of the discount.
***********************************************************************************
Function lfvAprDisc
*E303011,1 TMI 12/23/2011 [Start]
Parameters loAprFormSet,loObj

*** ifinvoice amount is positive. ***
If &lcAPINVHDR..NINVAMNT >= 0
*** if the total approved ammount not between zero and invoice amount. ***
*B600808,1 Compare the new approved amount with the open invoice amount
*B600808,1 excluding the old approved amounts,
*B600808,1 i.e. remove the approved amounts field from the open invoice
*B600808,1 amount calculation.
	If !Between(lnAprToPay + lnAprDisc + lnAprAdj, 0, ;
			&lcAPINVHDR..NINVAMNT  - &lcAPINVHDR..NINVPAID ;
			- &lcAPINVHDR..NINVDISTK - &lcAPINVHDR..NINVADJ)

** MESSAGE : " Total approved amount can not be greater than "
**           " the open amount.                              "
**           "                       ® Ok ¯
		=gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTInvoice)
*lnAprDisc = lnOldAprDisc       && get the old discount amount.
*SHOW GET lnAprToPay
		loObj.Value = loObj.OldValue

	Endif
Else && if the invoice amount is negative. ***
*** if the total approved ammount not between invoice amount and zero. ***
*B600808,1 Compare the new approved amount with the open invoice amount
*B600808,1 excluding the old approved amounts,
*B600808,1 i.e. remove the approved amounts field from the open invoice
*B600808,1 amount calculation.
	If !Between(lnAprToPay + lnAprDisc + lnAprAdj, ;
			&lcAPINVHDR..NINVAMNT  - &lcAPINVHDR..NINVPAID ;
			- &lcAPINVHDR..NINVDISTK - &lcAPINVHDR..NINVADJ, 0)

** MESSAGE : " Total approved amount can not be greater than "
**           " the open amount.                              "
**           "                       ® Ok ¯
		=gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTDebitM)
*lnAprDisc = lnOldAprDisc        && get the old discount amount.
*SHOW GET lnAprToPay
		loObj.Value = loObj.OldValue
	Endif
Endif
loAprFormSet.ariaform1.Refresh()
*- End of lfvAprDisc.

***********************************************************************************
*Name      : lfvAprToPay
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Validation function of to pay.
***********************************************************************************
Function lfvAprToPay
Parameters loAprFormSet,loObj
*** if the invoice amount is positive. ***
If &lcAPINVHDR..NINVAMNT >= 0
*** if the total approved ammount not between zero and invoice amount. ***
*B600808,1 Compare the new approved amount with the open invoice amount
*B600808,1 excluding the old approved amounts,
*B600808,1 i.e. remove the approved amounts field from the open invoice
*B600808,1 amount calculation.
	If !Between(lnAprToPay + lnAprDisc + lnAprAdj, 0, ;
			&lcAPINVHDR..NINVAMNT  - &lcAPINVHDR..NINVPAID ;
			- &lcAPINVHDR..NINVDISTK - &lcAPINVHDR..NINVADJ)
** MESSAGE : " Total approved amount can not be greater than "
**           " the open amount.                              "
**           "                       ® Ok ¯
		=gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTInvoice)
*lnAprToPay = lnOldAprTPy   && save the old payment amount.
*SHOW GET lnAprToPay
		loObj.Value = loObj.OldValue
	Endif
*** if the invoice amount is negative
Else
*** if the total approved ammount not between invoice amount and zero. ***
*B600808,1 Compare the new approved amount with the open invoice amount
*B600808,1 excluding the old approved amounts,
*B600808,1 i.e. remove the approved amounts field from the open invoice
*B600808,1 amount calculation.
	If !Between(lnAprToPay + lnAprDisc + lnAprAdj,;
			&lcAPINVHDR..NINVAMNT - &lcAPINVHDR..NINVPAID  - ;
			&lcAPINVHDR..NINVDISTK - &lcAPINVHDR..NINVADJ, 0)
** MESSAGE : " Total approved amount can not be greater than "
**           " the open amount.                              "
**           "                       ® Ok ¯
		=gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTDebitM)
*lnAprToPay = lnOldAprTPy    && get the old payment amount.
*old : SHOW GET lnAprToPay
		loObj.Value = loObj.OldValue
	Endif
Endif

*- Get the approved to pay amount in approval currency.
*FUNCTION  HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
*lnExchAmnt  = IIF(lnAprCurUnt > 0,;
ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2), 0)
lnExchAmnt  = Iif(lnAprCurUnt > 0 And lnAprExRat > 0,;
	ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)
*old : SHOW GET lnExchAmnt
loAprFormSet.ariaform1.Refresh()
*- End of lfvAprToPay

***********************************************************************************
*Name      : lfv1099amnt
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Validation function of 1099 AMOUNT.
***********************************************************************************
Function lfv1099amnt
*E303011,1 TMI 12/23/2011 [Start]
Parameters loAprFormSet,loObj

If !Between(ln1099amnt,lnAprToPay,0) .And. !Between(ln1099amnt,0,lnAprToPay)
** MESSAGE : " The 1099 amount cannot be greater than  "
**           " the ð.     "
**           "                 ® Ok ¯                  "
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*=gfModalGen("TRM04061B00000","DIALOG","approved amount to pay")
	=gfModalGen("TRM04061B00000","DIALOG",Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_APPROVEAMOUNT,loAprFormSet.GetHeaderText("LANG_APAPRPA_APPROVEAMOUNT",loAprFormSet.HeaderAlias)))
*N000682,1 MMT 12/09/2012 Globalization changes[END]
*ln1099amnt=lnO1099amnt
*SHOW GET ln1099amnt
	loObj.Value = loObj.OldValue
	loAprFormSet.ariaform1.Refresh()
Endif

***********************************************************************************
*Name      : lfvAprAdj
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Valid function of the Appr. Adjust.
***********************************************************************************
Function lfvAprAdj
*E303011,1 TMI 12/23/2011 [Start]
Parameters loAprFormSet,loObj

*** if the invoice ammount is positive. ***
If &lcAPINVHDR..NINVAMNT >= 0
*** if the total approved ammount not between zero and invoice amount. ***
*B600808,1 Compare the new approved amount with the open invoice amount
*B600808,1 excluding the old approved amounts,
*B600808,1 i.e. remove the approved amounts field from the open invoice
*B600808,1 amount calculation.
	If !Between(lnAprToPay + lnAprDisc + lnAprAdj, 0, ;
			&lcAPINVHDR..NINVAMNT  - &lcAPINVHDR..NINVPAID ;
			- &lcAPINVHDR..NINVDISTK - &lcAPINVHDR..NINVADJ)
** MESSAGE : " Total approved amount can not be greater than "
**           " the open amount.                              "
**           "                       ® Ok ¯
		=gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTInvoice)
*lnAprAdj = lnOldAprAdj            && get the old adjustment amount.
*SHOW GET lnAprToPay
		loObj.Value = loObj.OldValue
	Endif
*** if the invoice ammount is negative. ***
Else
*** if the total approved ammount not between invoice amount and zero. ***
*B600808,1 Compare the new approved amount with the open invoice amount
*B600808,1 excluding the old approved amounts,
*B600808,1 i.e. remove the approved amounts field from the open invoice
*B600808,1 amount calculation.
	If !Between(lnAprToPay + lnAprDisc + lnAprAdj, ;
			&lcAPINVHDR..NINVAMNT  - &lcAPINVHDR..NINVPAID ;
			- &lcAPINVHDR..NINVDISTK - &lcAPINVHDR..NINVADJ, 0)

** MESSAGE : " Total approved amount can not be greater than "
**           " the open amount.                              "
**           "                       ® Ok ¯
		=gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTDebitM)
*lnAprAdj = lnOldAprAdj              && get the old adjustment value.
*SHOW GET lnAprToPay
		loObj.Value = loObj.OldValue
	Endif
Endif
loAprFormSet.ariaform1.Refresh()
*- End of lfvAprAdj.

***********************************************************************************
*Name      : lfvCancel
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Restore old values if push cancel from scope.
***********************************************************************************
Function lfvCancel

llScope    = .F.

*** RESTORE OLD VAR. IN CURRENT VAR.
ldToDisDat = ldOToDisDt
ldFrDisDat = ldOFrDisDt
ldToDueDat = ldOToDueDt
ldFrDueDat = ldOFrDueDt
lcPriority = lcOldPrty
lcVendCode = lcOVendCod
lcVendComp = lcOldVnCmp
lcMethod   = lcOMethod
lcDivCode  = lcOldDvCod
* Remark the next line because it gives an error
* if we go in the Select Invoice scope screen again
* and at the same time the variable lcMethod was reinitialized
* Previously
*lcMethod   = lcOldpymth

lcInvRef   = lcOldInvRf
rbODuDsDt  = rbDuDsDt

***********************************************************************************
*Name      : lfvBnkChk
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Valid function for get fields lcBankCode (bank code), and lcCheckCode (checking account code)
***********************************************************************************
Function lfvBnkChk
Parameters loAprFormSet,loObj
loFormSet = loAprFormSet.Callingform
*- Store the old currency field values before validation
*PRIVATE llVldObj
*Old : PRIVATE lcOldCurr, lnOldRate, lnOldUnit, llVldObj
* old : IF llBrowse .OR. EVALUATE(SYS(18)) <> lcOldVal
If loObj.Value <> loObj.OldValue
*- lcOldCurr : old currency code
*- lnOldRate : old exchange rate
*- lnOldUnit : old currency unit
	lcOldCurr = lcAprCurCod
*lnOldRate = lnAprExRat
	lnOldExRat = lnAprExRat
*lnOldUnit = lnAprCurUnt
	lnOldUnt = lnAprCurUnt

*old : llVldObj = lfBnkChk(@laBankObjs, lcOldVal, @llBrowse)

*- Get the currency of the checking account.
*old : IF llVldObj .AND. EVALUATE(SYS(18)) <> lcOldVal
	lcAprCurCod = APCHECKS.cCurrCode
	If lcAprCurCod <> APINVHDR.cCurrCode
*B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
*B600849,1                currency to approved currency.
*lnAprExRat  = gfChkRate('lnAprCurUnt'    , lcAprCurCod,;
APINVHDR.dInvDate, .F., .F., ;
APINVHDR.cCurrCode, .T.)

*- Change the calling of gfChkRate so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
*                        .F., .F., lcAprCurCod, .T.)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
*IF APINVHDR.cCurrCode = gcBaseCurr
		If APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]
			lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,APINVHDR.dInvDate, ;
				.F., .F., APINVHDR.cCurrCode, .T.)
		Else      && Else
			lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
				.F., .F., lcAprCurCod, .T.)
		Endif     && End of IF
*- Change the calling of gfChkRate [END]

*B600849,1 (End)
*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to company base currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
		lcExSin2 = ' '
		lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*FUNCTION  to exchange currency from invoice currency to approved currency.
		lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
*IF APINVHDR.cCurrCode = gcBaseCurr
		If APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]
			lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
			lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
			lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
		Else   && Else
			lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
		Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')

*FUNCTION  HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
*lnExchAmnt  = IIF(lnAprCurUnt > 0,;
ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2), 0)
		lnExchAmnt  = Iif(lnAprCurUnt > 0 And lnAprExRat > 0,;
			ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)

	Else
		Store 1 To lnAprExRat, lnAprCurUnt
		lnExchAmnt  = lnAprToPay
	Endif
	Show Get lcAprCurCod
	Show Get lnExchAmnt
*Old : IF loFormSet.llEditEx .AND. lcAprCurCod <> APINVHDR.cCurrCode
*Old :   SHOW GET lnAprExRat ENABLE
*Old : ELSE
*Old :   SHOW GET lnAprExRat DISABLE
*Old : ENDIF
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
*loAprFormset.AriaForm1.txtaprexrat.Enabled  =  loFormSet.llEditEx .AND. lcAprCurCod <> APINVHDR.cCurrCode
	If Type('loAprFormset.AriaForm1.txtaprexrat') == 'O'
		loAprFormSet.ariaform1.txtaprexrat.Enabled  =  loFormSet.llEditEx .And. lcAprCurCod <> APINVHDR.cCurrCode
	Endif
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]
Else
	If !Empty(lcBankCode)
*- Restore old currency fields
		lcAprCurCod  = lcOldCurr
*lnAprExRat   = lnOldRate
		lnAprExRat   = lnOldExRat
*lnAprCurUnt  = lnOldUnit
		lnAprCurUnt  = lnOldUnt

	Else
		lcAprCurCod = Space(5)
		Store 0 To lnAprExRat, lnAprCurUnt, lnExchAmnt
*Old : SHOW GET lcAprCurCod
*Old : SHOW GET lnAprExRat DISABLE
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
*loAprFormset.AriaForm1.txtaprexrat.Enabled  =  .F.
		If Type('loAprFormset.AriaForm1.txtaprexrat')
			loAprFormSet.ariaform1.txtaprexrat.Enabled  =  .F.
		Endif
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]
*Old : SHOW GET lnExchAmnt
	Endif
* Old : ENDIF

*old : =lfRefresh()
*SELECT APINVHDR
*RETURN IIF(llVldObj, .T., 1)
	loAprFormSet.ariaform1.Refresh()
Endif

***********************************************************************************
*Name      : lfvExRate
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Valid function for lnAprExRat field
***********************************************************************************
Function lfvExRate
Parameters loArpFormset,loObj &&
*- if the entered value is not greater than zero,
*- Message :  "   ð should be greater than ð.   "
*-                 ®   OK   ¯
lnOldVal = loObj.OldValue
If lnAprExRat <> lnOldVal
	If lnAprExRat < 0 .And. ;
			gfModalGen("TRM04072B00000","DIALOG", lcTExRateMsg) > 0
		lnAprExRat = lnOldVal
	Endif
*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to approved currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
	lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
	If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
		lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,&lcAPINVHDR..cCurrCode)
		lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
		lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
	Else   && Else
		lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCurCod)
	Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*- Get the approved to pay amount in approval currency.
*FUNCTION  HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
*lnExchAmnt  = IIF(lnAprCurUnt > 0,;
ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2), 0)
	lnExchAmnt  = Iif(lnAprCurUnt > 0 And lnAprExRat > 0,;
		ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)
	Show Get lnExchAmnt
	loArpFormset.ariaform1.Refresh()
Endif
*- end

***********************************************************************************
*Name      : lfvAprCurCod
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Valid function for lcAprCurCod field
***********************************************************************************
Function lfvAprCurCod
Parameters loArpFormset,llGetExRate
Local loFormSet,loAprov,loAprExRat
loFormSet = loArpFormset.Callingform
loAprov = loArpFormset.ariaform1
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
*loAprExRat = loArpFormset.Ariaform1.txtaprexrat
If Type('loArpFormset.Ariaform1.txtaprexrat') == 'O'
	loAprExRat = loArpFormset.ariaform1.txtaprexrat
Endif
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]

Private laRetVal, lcCurObj
Declare laRetVal[1]
laRetVal     = " "
*old : lcCurObj = SYS(18)
*E303011,1 TMI 01/29/2012 [Start] move above with the lfforminit function
*=gfOpenTable(oAriaApplication.SysPath + 'SYCCURR' , 'CCURRCODE' , 'SH')
*E303011,1 TMI 01/29/2012 [End  ]

llBrowse = loAprov.kbAprCurrCode.Selectedfrombrowse
If llBrowse .Or. !Seek(loAprov.kbAprCurrCode.keyTextBox.Value,'SYCCURR') .Or. ;
		ATC("?",loAprov.kbAprCurrCode.keyTextBox.Value) > 0
	Select SYCCURR
*N000682,1 MMT 11/20/2012 Globalization project[Start]
*!*	  lcFile_Ttl = "Currency"
*!*	  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
*!*	      "CCURRDESC :R :H= 'Description',  " +;
*!*	      "CCURRSMBL :R :H= 'Symbol'"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl = LANG_APAPRPA
	lcFile_Ttl = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA,loFormSet.GetHeaderText("LANG_APAPRPA",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

	lcBrFields = "CCURRCODE :R :H= '"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_CURRENCYCODE,loFormSet.GetHeaderText("LANG_APAPRPA_CURRENCYCODE",loFormSet.HeaderAlias)) +"'," +;
		"CCURRDESC :R :H= '"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_DESC,loFormSet.GetHeaderText("LANG_APAPRPA_DESC",loFormSet.HeaderAlias)) +"',  " +;
		"CCURRSMBL :R :H= '"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_SYMBOL,loFormSet.GetHeaderText("LANG_APAPRPA_SYMBOL",loFormSet.HeaderAlias)) +"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/20/2012 Globalization project[END]
	=ARIABROW('', lcFile_Ttl , .F., .F., .F., .F., .F., .T., ;
		'CCURRCODE', 'laRetVal', .F., .F., .F., ;
		.F., .F., .F., .F., .F., ;
		.F., .F.)
	If Empty(laRetVal[1]) &&
		loAprov.kbAprCurrCode.keyTextBox.Value = loAprov.kbAprCurrCode.keyTextBox.OldValue
	Else
		loAprov.kbAprCurrCode.keyTextBox.Value = laRetVal[1]
	Endif
Endif

*Old :IF lfGetExt('SYCCURR', 'CCURRCODE', loCur.OldValue, llBrowse,  'cCurrCode',;
@laRetVal)
If loAprov.kbAprCurrCode.keyTextBox.Value <> loAprov.kbAprCurrCode.keyTextBox.OldValue
*old : &lcCurObj   = laRetVal[1]
	If llGetExRate
		If laRetVal[1] <> &lcAPINVHDR..cCurrCode
*B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
*B600849,1                currency to approved currency.
*STORE gfChkRate('lnAprCurUnt'    , laRetVal[1],     ;
APINVHDR.dInvDate, .T., .F.,;
APINVHDR.cCurrCode, .T.)    ;
TO lnAprExRat

*- Change the calling of gfChkRate so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*STORE gfChkRate('lnAprCurUnt'    ,APINVHDR.cCurrCode,     ;
*                        APINVHDR.dInvDate, .T., .F.,;
*                        laRetVal[1], .T.)    ;
*      TO lnAprExRat
*- IF Statment to check if the Invoice currency is the
* same as the base currency
			If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
				Store gfChkRate('lnAprCurUnt'    ,laRetVal[1],     ;
					&lcAPINVHDR..dInvDate, .T., .F.,;
					&lcAPINVHDR..cCurrCode, .T.)    ;
					TO lnAprExRat
			Else      && Else
				Store gfChkRate('lnAprCurUnt'    ,&lcAPINVHDR..cCurrCode,     ;
					&lcAPINVHDR..dInvDate, .T., .F.,;
					laRetVal[1], .T.)    ;
					TO lnAprExRat
			Endif     && End of IF
*- Change the calling of gfChkRate [END]


*B600849,1 (End)
*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to company base currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
			lcExSin2 = ' '
			lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*FUNCTION  to exchange currency from invoice currency to approved currency.
			lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,laRetVal[1])
*- IF Statment to check if the Invoice currency is the
* same as the base currency
			If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
				lcExSin3 = gfGetExSin(@lcExSin4,laRetVal[1],&lcAPINVHDR..cCurrCode)
				lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
				lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
			Else   && Else
				lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,laRetVal[1])
			Endif  && End of IF
*- Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')


*FUNCTION  HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
*lnExchAmnt  = IIF(lnAprCurUnt > 0,;
ROUND(lnAprToPay * lnAprExRat ;
/ lnAprCurUnt, 2), 0)
			lnExchAmnt  = Iif(lnAprCurUnt > 0 And lnAprExRat > 0,;
				ROUND(lnAprToPay &lcExSin3 lnAprExRat ;
				&lcExSin4 lnAprCurUnt, 2), 0)
*IF loFormset.llEditEx
*old : SHOW GET lnAprExRat ENABLE
*ELSE
*old : SHOW GET lnAprExRat DISABLE
*ENDIF
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
			If Type("loAprExRat") ="O"
*N000682,1 MMT 12/09/2012 Globalization changes[End]
				loAprExRat.Enable = loFormSet.llEditEx
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
			Endif
*N000682,1 MMT 12/09/2012 Globalization changes[End]

		Else
			Store 1 To lnAprExRat, lnAprCurUnt
			lnExchAmnt = lnAprToPay
*old : SHOW GET lnAprExRat DISABLE
		Endif
*old : SHOW GET lnExchAmnt
	Endif
Endif
llBrowse     = .F.
loArpFormset.ariaform1.Refresh()
*- End of lfvAprCurCod

***********************************************************************************
*Name      : lfSetDef
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Function to set the default currency and exchange rate
***********************************************************************************
Function lfSetDef

If !Empty(&lcAPINVHDR..nInvAmtAp)
	lcAprCurCod = &lcAPINVHDR..cAprCurCod
	lnAprExRat = &lcAPINVHDR..nAprExRat
	lnExchAmnt = &lcAPINVHDR..nInvFAAp
	lnAprToPay = &lcAPINVHDR..nInvAmtAp
	lnAprDisc = &lcAPINVHDR..NINVDISAP
	lnAprAdj = &lcAPINVHDR..NINVADJAP
	ln1099amnt = &lcAPINVHDR..nInvA1099
	lnAprCurUnt = &lcAPINVHDR..nCurrUnit

	If &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
		lcExSin3 = gfGetExSin(@lcExSin4 , lcAprCurCod , &lcAPINVHDR..cCurrCode)
		lcExSin3 = Iif(lcExSin3 = '*' , '/' , '*')
		lcExSin4 = Iif(lcExSin4 = '*' , '/' , '*')
	Else   && Else
		lcExSin3 = gfGetExSin(@lcExSin4 , &lcAPINVHDR..cCurrCode , lcAprCurCod)
	Endif  && End of IF
Endif

*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][Start]
*!*************************************************************
*! Name      : lfObj_Lock
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/10/2021
*! Purpose   : To object lock any record in any file
*!*************************************************************
FUNCTION lfObj_Lock
PARAMETERS lLok_Set
PRIVATE lnRecNo,lRet_Flag

PRIVATE lnOldrpSt

lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

IF !USED('SYUUSER')
  =gfOpenFile(ADDBS(ADDBS(oAriaApplication.ClientA27Path)+'SysFiles')+'SYUUSER','CUSER_ID','SH','SYUUSER')   && CUSER_ID
ENDIF
IF !USED('SYUSTATC')
  =gfOpenFile(ADDBS(oAriaApplication.cAria4SysPath)+'SYUSTATC','CUSER_ID','SH','SYUSTATC')   && COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID+CSTATION
ENDIF
SELECT  APINVHDR
DO WHILE .T.
  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
    llLocked = RLOCK()
    IF DELETED()
      UNLOCK
      =gfModalGen('INM00095B00000','ALERT')
      laScrMode     = .F.
      laScrMode [1] = .T.
      SHOW GETS
      RETURN .F.
    ENDIF
  ENDIF

  *** Chek if the record is in use by another user
  IF lLok_Set
    *** Chek if the field cLok_User in the structur
    IF !lLok_Stat .AND. llLocked
      *** Record is not locked you may lock it
      lLok_It   = .T.
    ELSE
      IF !EMPTY(cLok_User)
        lnOldrpSt = SET('REPROCESS')
        SET REPROCESS TO 1
        IF SEEK ('INI'+'OLDVARS'+cLok_User,'SyuStatc')
          UNLOCK
          lnSavRec   = IIF(RECNO('SYUUSER')>RECCOUNT('SYUUSER'),0, RECNO('SYUUSER'))
          lcLok_User = ALLTRIM(PROPER(LOOKUP(syuUser.cUsr_name,cLok_User, syuUser.cUser_id,'cUser_id')))
          IF lnSavRec > 0
            GO lnSavRec IN SYUUSER
          ENDIF


          *** Record is in use by user ????
          *N000682,1 MMT 11/22/2012 Globalization changes[Start]
          *lcRtyCncMs = "Invoice "+ALLTRIM(APINVHDR.CINVNO)+" for vendor "+ ALLTRIM(APINVHDR.CVendCode)+" is being edited by user " + lcLok_User+"."
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *lcRtyCncMs = LANG_APMNCHP_INVOICE_S+ALLTRIM(APINVHDR.CINVNO)+LANG_APMNCHP_FORVENDOR+ ALLTRIM(APINVHDR.CVendCode)+LANG_APMNCHP_BEINGEDITED + lcLok_User+"."
          lcRtyCncMs = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_INVOICE_S,;
          loFormSet.GetHeaderText("LANG_APAPRPA_INVOICE_S",loFormSet.HeaderAlias))+ALLTRIM(APINVHDR.CINVNO)+;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APAPRPA_FORVENDOR,loFormSet.GetHeaderText("LANG_APAPRPA_FORVENDOR",loFormSet.HeaderAlias))+;
           ALLTRIM(APINVHDR.CVendCode)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",;
           LANG_APAPRPA_BEINGEDITED ,loFormSet.GetHeaderText("LANG_APAPRPA_BEINGEDITED",loFormSet.HeaderAlias)) + lcLok_User+"."
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/22/2012 Globalization changes[END]
          IF  gfModalGen("INM00274B00015","ALERT",lcRtyCncMs) = 1
            LOOP
          ENDIF
          lLok_It    = .F.
          lRet_Flag  = .F.
        ELSE
          lLok_It    = .T.
        ENDIF
        SET REPROCESS TO  lnOldrpSt
      ELSE
        *** Display the message "Record is in use by another"
        IF gfModalGen("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF
        lLok_It    = .F.
        lRet_Flag  = .F.
      ENDIF
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
        TYPE ('dLok_Date') <> "U" .AND. ;
        TYPE ('cLok_Time') <> "U"

      *** Unlock the record
      REPLACE lLok_Stat WITH .F. , ;
        cLok_User WITH ""  , ;
        dLok_Date WITH {}  , ;
        cLok_Time WITH ""
      lRet_Flag  = .T.
    ENDIF
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
IF lLok_It
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
      TYPE ('dLok_Date') <> "U" .AND. ;
      TYPE ('cLok_Time') <> "U"
    *** Lock the record for this user with date and time
    WAIT WINDOW NOWAIT ' ... Locking ... '
    REPLACE lLok_Stat WITH .T. ,;
      cLok_User WITH oAriaApplication.User_ID ,;
      dLok_Date WITH DATE() ,;
      cLok_Time WITH gfGetTime()
    =gfTableUpdate()
    WAIT CLEAR
    lRet_Flag  = .T.
  ENDIF
ENDIF

UNLOCK

RETURN lRet_Flag
*!B612444,1 MMT 08/10/2021 Approve for payment allow user to lock invoice locked by the same user in another session[T20210728.0001][End]