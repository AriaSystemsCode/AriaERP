*!*****************************************************************************************
*! Name      : ALUSPS.PRG
*! Developer : HIA
*! Date      : 04/08/2010
*! Purpose   : Picking Ticket Form A IN ARIA4
*! Entry no. : AlUSPS
*!*****************************************************************************************
*! Modification
*E302730,2 MMT 08/08/2010 The integration with Endicia Label Server for Jasco.[T20100804.0003]
*B609650,1 MMT 07/21/2011 Printing USPS labels consumes large memory[T20100205.0005]
*E302954,1 MMT 08/16/2011 Interface to send shipping information, receive tracking# and label[T20101207.0006]
*E302961,1 MMT 08/25/2011 Add Sort by option to the OG of Print PL\Carrier label program[T20110531.0028]
*B612290,1 Es  12/29/2020 In Print Integ. Packing List/Carrier Label screen, Sales order browser in the option grid takes long time [T20201207.0001]
*B612435,1 MMT 07/21/2021 User got an error while printing many Fedex labels from Print Integ. Packing list/Carrier Label screen[T20210720.0002]
*B612586,1 MMT 06/23/2022 The Print carrier integration screen option grid takes long time to load Picking tickets browser[T20220616.0001]
*!*****************************************************************************************
*!*	If looGscroll.llOGFltCh
*!*		lfCrtTemp()
*!*		lfCollectData()
*!*	Else
*!*		Use oAriaApplication.WorkDir +  lcOrdlTmp + ".DBF" In 0
*!*	Endif
*PUBLIC  laScFxflt,laScHDflt,laScvrflt
*STORE '' TO laScFxflt,laScHDflt,laScvrflt
*oAriaApplication.DoProgram("AWRSOORDSL",,.F.,'SO')
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
#INCLUDE r:\aria4xp\reports\al\alplusps.H
*N000682,1 MMT 02/10/2013 Globalization changes[End]
*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Hassan.I
*! Date      : 04/08/2010
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************



Function lfvOptMsg
LPARAMETERS llshpopup

Private laOptMsg
Declare laOptMsg[2,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[1,2] = 60                && Line length
laOptMsg[2,1] = 'lcRpMsg2'        && 1st. line Variable
lcTypInv ='uspsoptmsg'
If Empty(lcRpMsg1).And. Empty(lcRpMsg2)
	If File(gcDataDir+lcTypInv+'.MEM')
		Restore From gcDataDir+lcTypInv+'.MEM' Additive
	Endif
ENDIF
IF llshpopup
	=gfOptMsg('laOptMsg')
ENDIF
Set Memowidth To 120             && the length of the memo field.
Save To gcDataDir+lcTypInv+'.MEM' All Like lcRpMsg*
Endfunc

Function lfvFileOutput
LPARAMETERS llshpopup
Private laOptMsg
Declare laOptMsg[2,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpOutPutFile'        && 1st. line Variable
laOptMsg[1,2] = 80                && Line length
lcTypInv ='uspsFileMem'
If Empty(lcRpOutPutFile)
	If File(gcDataDir+lcTypInv+'.MEM')
		Restore From gcDataDir+lcTypInv+'.MEM' Additive
	Endif
ENDIF
IF llshpopup
	=gfOptMsg('laOptMsg')
ENDIF
Set Memowidth To 80             && the length of the memo field.
Save To gcDataDir+lcTypInv+'.MEM' All Like lcRpOutPutFile
Endfunc

*-- end of lfvOptMsg.

Function lfRepWhen
*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
lcTranlcTitle    =IIF(llRPPrintPackingISFOR='O','Orders without Tracking#','PickTkts without Tracking#')
*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]
  Select PikTkt
  Dimension laPiktkt[1,18]
  Store '' To laPiktkt
  lnFldWidth = Afields(laPiktkt)
  =gfCrtTmp(lcPikTkt,@laPiktkt,'PIKTKT',lcPikTkt)

Select Ordhdr
Dimension laOrdhdr[1,18]
Store '' To laOrdhdr
lnFldWidth = Afields(laOrdhdr)
=gfCrtTmp(lcOrdhdr,@laOrdhdr,'CORDTYPE+ORDER',lcOrdhdr)
=lfvOptMsg(.F.)
=lfvFileOutput(.F.)
*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
IF EMPTY(laRpSource)
  IF !USED('CARRIERS_T')
	lnCarriers  = oAriaApplication.RemoteCompanyData.execute("Select * From CARRIERS_T" ,'',"CARRIERS_T","",oAriaApplication.sqlsysfilesconnectionstring ,3,"",SET("Datasession"))
  ENDIF
  IF USED('CARRIERS_T')
    DECLARE laRpSource[RECCOUNT('CARRIERS_T')],laRpTarget[RECCOUNT('CARRIERS_T')]  && Redeclare the source and target arrays.
    laRpTarget = ''
    SELECT CARRIERS_T
    lnCntCr = 1
    SCAN
      laRpSource[lnCntCr] = ALLTRIM(CARRIERS_T.ID)
      lnCntCr = lnCntCr + 1
    ENDSCAN
  ELSE
    DECLARE laRpSource[1],laRpTarget[1]  && Redeclare the source and target arrays.
    STORE '' TO laRpSource,laRpTarget
  ENDIF
ENDIF
*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]

Endfunc

Function LFSORDER
Lparameters lSetPara

llAccSelect = .F.
lcAccSelect  = lfCheckFilter(1,"Customer.account")
If !Empty(lcAccSelect)
	Select(lcAccSelect)
	Locate
	If !Eof()
		llAccSelect = .T.
	Endif
ENDIF

llPikTkt = .F.
lcPikTktSelect  = lfCheckFilter(1,"PikTkt.PikTkt")
If !Empty(lcPikTktSelect)
	Select(lcPikTktSelect)
	Locate
	If !Eof()
		llPikTkt = .T.
	Endif
Endif
ldOrdEnteredDat  = lfCheckFilter(1,"ORDHDR.ENTERED")
ldLOrdEntered    = Ctod(Substr(ldOrdEnteredDat  ,1,    Atc('|',ldOrdEnteredDat)-1))
ldHOrdEntered    = Ctod(Substr(ldOrdEnteredDat  ,      Atc('|',ldOrdEnteredDat)+1))

ldOrdStartDat  = lfCheckFilter(1,"ORDHDR.START")
ldLOrdStart    = Ctod(Substr(ldOrdStartDat,1,    Atc('|',ldOrdStartDat)-1))
ldHOrdStart    = Ctod(Substr(ldOrdStartDat,      Atc('|',ldOrdStartDat)+1))

ldOrdCompleteDat  = lfCheckFilter(1,"ORDHDR.COMPLETE")
ldLOrdComplete    = Ctod(Substr(ldOrdCompleteDat  ,1,    Atc('|',ldOrdCompleteDat)-1))
ldHOrdComplete    = Ctod(Substr(ldOrdCompleteDat  ,      Atc('|',ldOrdCompleteDat)+1))

Select (lcOrdhdr)
*B612290,1 Es  12/29/2020 In Print Integ. Packing List/Carrier Label screen, Sales order browser in the option grid takes long time [Start]
*Delete All
  ZAP
*B612290,1 Es  12/29/2020 In Print Integ. Packing List/Carrier Label screen, Sales order browser in the option grid takes long time [End]
If lSetPara = 'S'
	Select Ordhdr
  =SEEK('O')
  *E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
*!*		SCAN REST WHILE CORDTYPE+ORDER  = 'O' FOR  IIF(llAccSelect,Seek(Ordhdr.Account,lcAccSelect),.T.) and ;
*!*	        IIF(llPikTkt,SEEK(Ordhdr.Order,lcPikTktSelect),.T.) AND ;
*!*	        IIF(Empty(ldOrdEnteredDat),.T.,Between(Ordhdr.entered,ldLOrdEntered,ldHOrdEntered)) AND;
*!*	        IIF(Empty(ldOrdStartDat),.T.,Between(Ordhdr.Start,ldLOrdStart,ldHOrdStart)) AND;
*!*	        IIF(Empty(ldOrdCompleteDat),.T.,Between(Ordhdr.Complete,ldLOrdComplete,ldHOrdComplete))AND;
*!*	        IIF(EMPTY(llRPOrdStatus),.T.,ordhdr.status = llRPOrdStatus)
	SCAN REST WHILE CORDTYPE+ORDER  = 'O' FOR Status $ 'COH' AND  IIF(llAccSelect,Seek(Ordhdr.Account,lcAccSelect),.T.) and ;
        IIF(llPikTkt,SEEK(Ordhdr.Order,lcPikTktSelect),.T.) AND ;
        IIF(Empty(ldOrdEnteredDat),.T.,Between(Ordhdr.entered,ldLOrdEntered,ldHOrdEntered)) AND;
        IIF(Empty(ldOrdStartDat),.T.,Between(Ordhdr.Start,ldLOrdStart,ldHOrdStart)) AND;
        IIF(Empty(ldOrdCompleteDat),.T.,Between(Ordhdr.Complete,ldLOrdComplete,ldHOrdComplete))AND;
        IIF(EMPTY(llRPOrdStatus),.T.,ordhdr.status = llRPOrdStatus)

        *E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]
	**Start Mostafa 20-4
    lcMAILCLASS = ""
    *E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
    lcCarrierID = ''
	*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]
	
*!*      DIMENSION laCUPS (1,2)
*!*      laCUPS (1,1) = 'CUPS'
*!*      laCUPS (1,2) = 'lcMAILCLASS'
*!*                   = gfRltFld(OrdHdr.Shipvia , @laCUPS , 'SHIPVIA') && gfRltFld
    lfGetCodes()
*!*      if LEFT(UPPER(lcMAILCLASS),4) = "USPS"
*!*       	**Start Mostafa 20-4
*!*      ENDIF

        *E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
*!*			IF  !(LEFT(UPPER(lcMAILCLASS),4) == "USPS")
*!*				Loop
*!*			Endif
        IF !EMPTY(lcRpCarr) AND !(ALLTRIM(UPPER(lcCarrierID)) $ UPPER(lcRpCarr))
          LOOP
        ENDIF
        IF 'USPS' $ ALLTRIM(UPPER(lcCarrierID)) AND  !(LEFT(UPPER(lcMAILCLASS),4) == "USPS")
          Loop
        Endif
        *E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]

		If !Seek(Ordhdr.CORDTYPE+Ordhdr.Order,lcOrdhdr)
			Scatter Memo Memvar
			Insert Into (lcOrdhdr) From Memvar
		Endif
	ENDSCAN


ENDIF

Endfunc

Function LFSSHIPFOR
If Ascan(looGscroll.laOGFxFlt,'PIKTKT.PIKTKT') # 0
	lnVendorPo = Asubscript(looGscroll.laOGFxFlt, Ascan(laOGFxFlt,'PIKTKT.PIKTKT'),1)
	If llRPPrintPackingISFOR <> 'P'
*-- Enable the vendor if not Inter Location PO
		LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(LAOGFXFLT,1) + LNVENDORPO] = .F.

	Else
*-- Disable the vendor if Inter Location PO
		LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(LAOGFXFLT,1) + LNVENDORPO] = .T.

	Endif
*-- Refresh the Option Grid
	= LFOGSHOWGET('LAOGFXFLT[' + Alltrim(Str(lnVendorPo)) + ',6]')

Endif
Endfunc

*lcVenPmthd = lfCheckFilter(3,"APINVHDR.CVENPMETH")
*!*************************************************************
Function lfCheckFilter
Lparameters lnArrayType, lcFilter

Local lcReturn, lnPOS
Do Case
Case lnArrayType = 1
  *lnPOS = Ascan(looGscroll.laOGFxFlt,lcFilter)
  lnPOS = Ascan(looGscroll.laOGFxFlt,lcFilter,1,0,1,1)
	If lnPOS > 0
		lnPOS    = Asubscript(looGscroll.laOGFxFlt,lnPOS,1)
		lcReturn = looGscroll.laOGFxFlt[lnPOS,6]
	Else
		lcReturn = ""
	Endif
Case lnArrayType = 2
	lnPOS = Ascan(looGscroll.laOGHDFlt,lcFilter)
	If lnPOS > 0
		lnPOS    = Asubscript(looGscroll.laOGHDFlt,lnPOS,1)
		lcReturn = looGscroll.laOGHDFlt[lnPOS,6]
	Else
		lcReturn = ""
	Endif
Case lnArrayType = 3
	lnPOS = Ascan(looGscroll.laOGvrFlt,lcFilter)
	If lnPOS > 0
		lnPOS    = Asubscript(looGscroll.laOGvrFlt,lnPOS,1)
		lcReturn = looGscroll.laOGvrFlt[lnPOS,6]
	Else
		lcReturn = ""
	Endif
Otherwise
	lcReturn = ""
Endcase

Return lcReturn
* End of lfCheckFilter


*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
FUNCTION lfCREATEArrs

DIMENSION laScFxflt[ALEN(loogscroll.laogfxflt,1),ALEN(loogscroll.laogfxflt,2)]
ACOPY(loogscroll.laogfxflt,laScFxflt)

DIMENSION laScHDflt[ALEN(loogscroll.laogHDflt,1),ALEN(loogscroll.laogHDflt,2)]
ACOPY(loogscroll.laogHDflt,laScHDflt)

DIMENSION laScVRflt[ALEN(loogscroll.laogVRflt,1),ALEN(loogscroll.laogVRflt,2)]
ACOPY(loogscroll.laogVRflt,laScVRflt)
looGscroll_llOGFltCh = looGscroll.llOGFltCh
*_llRPOWOT            = llRPOWOT

*_lctmpOrdHdr           = lctmpOrdHdr
_lcRpMsg1              = lcRpMsg1
_lcRpMsg2              = lcRpMsg2
_llRPOWOT              = llRPOWOT
_llRPPRCUSTNOTEPAD     = llRPPRCUSTNOTEPAD
_llRPPRIAMINFO         = llRPPRIAMINFO
_lcRpoutputFile        = lcRpoutputFile
_llRPPRVERADD          = llRPPRVERADD
_llRPUSESAMPPOS        = llRPUSESAMPPOS
_llRPDONTPRUNVERADD    = llRPDONTPRUNVERADD
_llRPPRREQTODEF        = llRPPRREQTODEF
_llRPPrintPackingISFOR = llRPPrintPackingISFOR
_looGscroll_llOGFltCh  = looGscroll_llOGFltCh
_LLRPORDSTATUS         = LLRPORDSTATUS
_LCRPLABELTYPE         = LCRPLABELTYPE

*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]
*E302730,2 MMT 08/08/2010 The integration with Endicia Label Server for Jasco.[Start]
_llRpTstL = llRpTstL
_llRpHdPS = llRpHdPS
*E302730,2 MMT 08/08/2010 The integration with Endicia Label Server for Jasco.[End]

*E302961,1 MMT 08/25/2011 Add Sort by option to the OG of Print PL\Carrier label program[Start]
_LCRPSORT = LCRPSORT
*E302961,1 MMT 08/25/2011 Add Sort by option to the OG of Print PL\Carrier label program[END]


*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
_lcCarriers = lcRpCarr
_lcRpPrnt =lcRpPrnt
_lcRpLbFrm =lcRpLbFrm
_llRpGenB  = llRpGenB
*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]

FUNCTION lfSeeklogo
LPARAMETERS lcAccountid
*B609650,1 MMT 07/21/2011 Printing USPS labels consumes large memory[Start]
lcScLanel = &lcTmpoutput..labelpath2
lcFrstLab = &lcTmpoutput..labelpath
*B609650,1 MMT 07/21/2011 Printing USPS labels consumes large memory[END]
lcpath = .f.
lccimgpath = ""
  IF SEEK('DA'+lcAccountid,'OBJLINK','OBJDEFA')
    IF SEEK(OBJLINK.COBJECT_ID,'OBJECTS')
      * lccimgpath = ALLTRIM(OBJECTS.cimgpath)
      lccimgpath = ALLTRIM(OBJECTS.mimgpath)
      lcpath = .t.
    ENDIF
  ENDIF
SELECT (lcTmpoutput)
RETURN lcpath
ENDFUNC

FUNCTION pcustaddr
PARAMETERS lacc
=SEEK('M'+lacc,'customer')
RETURN customer.caddress1
ENDFUNC


* Filter PickTkt Start
Function LFSPikTkt
Lparameters lSetPara

llAccSelect = .F.
lcAccSelect  = lfCheckFilter(1,"Customer.account")
If !Empty(lcAccSelect)
  Select(lcAccSelect)
  Locate
  If !Eof()
    llAccSelect = .T.
  Endif
ENDIF

Select (lcPikTkt)
*B612586,1 MMT 06/23/2022 The Print carrier integration screen option grid takes long time to load Picking tickets browser[T20220616.0001][Start]
*Delete All
ZAP
*B612586,1 MMT 06/23/2022 The Print carrier integration screen option grid takes long time to load Picking tickets browser[T20220616.0001][End]
If lSetPara = 'S'
  SELECT PIKTKT
  *E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
  *SCAN ALL
  SCAN FOR STATUS $ 'OC' AND !EMPTY(PIKTKT) AND PIKTKT <> '******'
  *E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]
  **Start Mostafa 20-4
    IF  IIF(llAccSelect,!Seek(PikTkt.Account,lcAccSelect),.F.)
      Loop
    ENDIF
    *E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
    IF !EMPTY(lcRpCarr)
      =SEEK('O'+PIKTKT.ORDER,'ORDHDR','ORDHDR')
  	  lcMAILCLASS = ""
      lcCarrierID = ''
      lfGetCodes()
      IF !EMPTY(lcRpCarr) AND !(ALLTRIM(UPPER(lcCarrierID)) $ UPPER(lcRpCarr))
        LOOP
      ENDIF
    ENDIF
    *E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]
    If !Seek(PikTkt.piktkt,lcPikTkt)
      Scatter Memo Memvar
      Insert Into (lcPikTkt) From Memvar
    Endif
  ENDSCAN
ENDIF
Endfunc
* Filter PickTkt End

FUNCTION lfGetCodes
lcAlias = ALIAS()
if !used('Codes')
  =gfOpenTable('Codes','Codes')
endif
SELECT Codes
SET ORDER TO TAG Codes
*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
*IF SEEK('N'+OrdHdr.Shipvia +'Y'+'SHIPVIA')
*  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+OrdHdr.Shipvia +'Y'+'SHIPVIA'
lcShipV = OrdHdr.Shipvia
=SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER','CUSTOMER')
lcShipV = IIF(LEFT(lcShipV ,1)<>'*',lcShipV , CUSTOMER.Shipvia)
IF SEEK('N'+lcShipV +'Y'+'SHIPVIA')
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+lcShipV  +'Y'+'SHIPVIA'
*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]
    IF crltd_nam = 'CUPS      '
      lcMAILCLASS = crltd_vlu
    ENDIF
    *E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
	IF crltd_nam = 'CCARRIERID'
      lcCarrierID = crltd_vlu
    ENDIF
	*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]
  ENDSCAN
ENDIF
SELECT (lcAlias)
*B609650,1 MMT 07/21/2011 Printing USPS labels consumes large memory[Start]
*!*************************************************************
*! Name      : lfClrRes
*! Developer : Mariam Mazhar{MMT}
*! Date      : 07/21/2011
*! Purpose   : Function to Clear resouces called from FRX ALPLUSPS.FRX
*!*************************************************************
FUNCTION lfClrRes
CLEAR RESOURCES (lcScLanel)
CLEAR RESOURCES (lcFrstLab)
*B609650,1 MMT 07/21/2011 Printing USPS labels consumes large memory[END]

*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[Start]
FUNCTION RefreshCarrier
LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + ALLTRIM(laRpTarget[lnTarget])
    ENDFOR
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF
  RETURN lcStatusStr
ENDFUNC


FUNCTION lfvOCarrier
PRIVATE lcOldStat,lcCurrChr
lcOldStat = lcRpCarr
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*= lfOGMover(@laRpSource,@laRpTarget,"Select Carrier",.T.,'')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= lfOGMover(@laRpSource,@laRpTarget,LANG_SELECT_CARR,.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,;
           IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECT_CARR,oAriaApplication.GetHeaderText("LANG_SELECT_CARR",AHEADERFILE)),.T.,'')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/10/2013 Globalization changes[End]
lcRpCarr = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    IF !EMPTY(laRpTarget[lnI])
      lcRpCarr = lcRpCarr + ALLTRIM(laRpTarget[lnI])+"|"
    ENDIF
  ENDFOR
ENDIF
ClearRead()
*E302954,1 MMT 08/09/2011  Interface to send shipping information, receive tracking# and label[END]
*B612435,1 MMT 07/21/2021 User got an error while printing many Fedex labels from Print Integ. Packing list/Carrier Label screen[T20210720.0002][Start]
FUNCTION lfClrResFed
lcImagePath = &lcShipInfo..cLabelPath
CLEAR RESOURCES (lcImagePath)
*B612435,1 MMT 07/21/2021 User got an error while printing many Fedex labels from Print Integ. Packing list/Carrier Label screen[T20210720.0002][End]