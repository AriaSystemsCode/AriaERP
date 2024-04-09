*:****************************************************************
*: Program file  : ALPKSCN.PRG
*: Program desc. : Packing and Scan.
*: System        : Aria Apparel System     [A27].
*: Module        : Sales Order Allocation. [AL]
*: Developer     : ABDOU ELGENDI - (ABD) Due to E500443,1
*: Date          : 11/18/2001
*:****************************************************************
*: Calls
*:               : FUNCTIONS  : lfReadAct , lfNonMaj  , lfvAccount
*:               :            : lfvPackNo , lfvPikTkt , lfActFolder
*:               :            : lpShow    , lfOpenFils, lfClosFils
*:               :            : lfvStore  , lfvOrder  , lfvCustPo
*:               :            : lfReIntiVr, lfBrows   , lfShowData
*:               :            : lfvAddPack, lfvAdOrdPk, lfvPackInfo
*:               :            : lfvbuild  , lfvCancel , lfvShipVia
*:               :            : lfvaccept , lfClearFld, lfAddRec
*:               :            : lfWQty    , lfvQty    , lfPrntLab
*:               :            : lfPrnUcc128,lfCheckNo , lfvRemUpc
*:               :            : lfNewCart , lfvUpc    , lfvNewUpc
*:               :            : lfvReScn  , lfvAutPck , lpSavScr
*:               :            : lfGetUpc  , lfRdDctOP , lfTrapKey
*:               :            : lfEsc     , lfReUsefls, lfCheckLns
*:               :            : lfUpDtTmp , lfvTotCrt ,lfVsulLbl.
*:               -----------------------------------------------
*:               : PROCEDURE  : lpClose   .
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: E#500443,1         :
*:****************************************************************
*: Modifications :
*: E301784,1 ABD 12-2-2001  Apply the pack&scan program for EIL10
*: E301784,1 ABD            On the new standard  program that we
*: E301784,1 ABD            did based on ENH#500443.
*: B605884,1 ABD 07-24-2002 If the there are many BOL's with the same Account,store,... of the packing list,
*: B605884,1 ABD            The user should be able to browse them and select one of them.
*: C102789,4 ASH 02/16/2003 Add an option menu to hold the carton type.
*: B606886,1 ABD 03/09/2003 Printing UCC 128 Labels as soon as packing slip is created
*: E302144,1 AMH 04/15/2003 Change the custom C102789 to be standard.
*: B607420,1 TMI 07/20/2003 Fix the bug that Multiple users able to pack same picking Ticket
*: B037019,1 TMI 09/01/2003 Lock records of ordline file of the piktkt selected only
*: E037191,1 SSE 10/12/2003 Add MUCB variable to printing Label function.
*: B038498,1 WLD 09/14/2004 Total catons not correct.
*: B123624,1 TMI 09/15/2004 Do not save if no lines entered in cartons temp file
*: E302210,1 WLD 09/09/2004 Using visual label report
*: E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch)
*: B126515,1 ASH 02/13/2005 Fix bug of not saving the audit fields in the pack_hdr.
*: C125077,1 AEH 06/02/2005 Add Carton Number to File
*: B128758,1 MMR 07/12/2005 Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen)
*: B039957,1 HBG 11/20/2005 Update the new file EDICRTSQ with carton sequence
*: E040123,1 HBG 03/14/2006 Get the UCC # structure from the setup
*: E302372,1 TMI 03/21/2007 Call the prnlabl3.exe that prints labels from EDI3
*:******************************************************************
*:
*-- Begin Declaration variables.
*-- llEDIInstl  :- This flag should be true only when UPC module is Install.
*-- llUniPrtcd  :- This flag should be true only when UniVersal Product Code module is Install..
*-- lcWinCh [1,2,3,4,5] :- Screen Temp Name.
*-- llEdiAcPrt :- Flage Hold True When Open EDI File EDISCPRT.
*-- llEdiPh    :- Flage Hold True When Open Edi File EdiPh.
*-- llEdiPd    :- Flage Hold True When Open Edi File EdiPd.
*-- llBOL_Hdr  :- Flage Hold True When Open Edi File BOL_Hdr.
*-- llSYCASNLB :- Flage Hold True When Open Edi File SYCASNLB.
*-- llBOL_LIN  :- Flage Hold True When Open Edi File BOL_LIN.
*-- llASN_SHIP :- Flage Hold True When Open Edi File ASN_SHIP.
*-- llBOL_LIN  :- Flage Hold True When Open Edi File BOL_LIN.
*-- lnTotOrdr  :- Total order in the Order line File.
*-- llAccOne   :- Hold true in case we will accepte to one carton.
*-- llAutoPack :- Hold true in case we will auto pack.

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE laKeyField [1,4]

*-- Arrays to hold the folder,cartons,shipvia codes
DECLARE lafoldwinds[2,2],laCartons[1,2],laShpVia[1],laCodInfo[1,10]

STORE ''  TO lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcWinCh5
*B128758,1 MMR 07/12/2005 Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen)[Start]
lcSetConf  = SET('CONFIRM')	 && Save the confirmation setting.
llConfirm  = .F.             && If set confirmation is on or off.
lcCnfrm    = ""              && Variable hold the confirmation setting.
llConfirm  =(ALLTRIM(gfGetMemVar('M_CONFIRM',gcAct_Comp))="Y")
*B128758,1 MMR [End]

*-- Initializing the necessary variables for the program.
STORE '' TO lcEvenCode,lcVendNo,lcUccVer,lcWrName,lcWrAddr1,;
	lcWrAddr2,lcWrCity,lcWrState,lcWrZip,lcDistCtr,lcStName,;
	lcStAddr1,lcStAddr2,lcStCity,lcStState,lcStZip,lcDCName,;
	lcDCAddr1,lcDCAddr2,lcDCCity,lcDCState,lcDCZip,lcOldValue,;
	laShpVia,lcShipVia,lcPkChTxt,lcPkDsTxt , lcDivLName
STORE SPACE(6) TO lcPackNo,lcOrderNo,lcBol,lcStyColor
STORE SPACE(5) TO lcAccount,lcDept

*E302210,1 WLD 09/09/2004 Using visual label report
STORE {} TO ldOrdCanc
STORE '' TO lcOrdNote1, lcOrdNote2

STORE .F. TO llDetLabel, llPrintLbl
STORE ""  TO lcDetailVr, lcDetLbAll
*E302210,1 WLD (End)

*E040123,1 HBG 03/14/2006 Check for UCC # structure setup [Begin]
llIncPLNum = .F.
lnNumOfDig = 5
*E040123,1 HBG 03/14/2006 [End]

STORE 0 TO lnTotCart,lnTotPiece,lnTotWg,lnShpVia,laCodInfo,lnMarker,;
	lnToStorCn,laCartons,lnTotOrdr,lnTotPktk,lnScnToQty
STORE '' TO  lcTmpBrow , lcTmpData ,lcTmpData1 , TmpAsnShip
STORE .F. TO llAccOne , llAutoPack
lcTmpPkLin = gfTempName()
lcTmpBrow  = gfTempName()
lcTmpData  = gfTempName()
lcTmpData1 = gfTempName()
TmpAsnShip = gfTempName()
lcAsnLabel = gfTempName()

lcStore    = SPACE(08)
lcCustPo   = SPACE(10)
lcNotes    = SPACE(30)
lcSp_Inst1 = SPACE(78)
lcSp_Inst2 = SPACE(78)
lcPackChar = SPACE(05)
lcPackDesc = SPACE(07)
DECLARE lafolders[2,2]

llUniPrtcd = ('UP' $ gcComp_mdl)
*-- Checking if the system is using the UPC or not. IF its not then displays
*-- a message to inform the user that the system has not been setup to use
*-- UPC numbers. Cannot proceed
IF !llUniPrtcd
	*- Message Text   :- The system has not been setup to use UPC numbers. Cannot proceed.
	*- Message No.    :- 44061.
	*- Buttom Message :- Ok
	*- Buttom Number  :-00000
	=gfModalGen('INM44061B00000','DIALOG' )
	RETURN
ENDIF


lcObjColor = ",RGB(,,,192,192,192),,,,,,,,RGB(100,100,100,192,192,192)"
lcBackClr  = ',,,,,,,,RGB(192,192,192,192,192,192),;
	RGB(192,192,192,192,192,192),'
lcPinBack  = 'RGB(192,192,192,192,192,192)'
lcTopLeft  = 'RGB(128,128,128,128,128,128)'
lcBotRgt   = 'RGB(255,255,255,255,255,255)'

*-- Scan variables
STORE SPACE(12) TO lcUpc,lcStyle
lcSize     = SPACE(05)
STORE 0 TO lnQty1,lnQtyWg,lnStyWg,lnCartonNo,lnLastCart

*-- Variables to hold the non major segments.
STORE 0 TO lnClrPo,lnColorLen

*-- To get the lenght of the style major
lnMajLen = LEN(gfItemMask('PM'))

*-- To get the non major segments.
= lfNonMaj()



llReBrow   = .T.
*E302210,1 WLD 09/09/2004 Using visual label report -new variable llSYCASNHD [Begin]
*!*	STORE .F. TO llBrowse,llEdiFound,llPkChrDes,llToStOrCn,llEdiExist,llUpdate,llEDIInstl,;
*!*	             llEdiAcPrt,llEdiPh,llEdiPd,llBOL_Hdr,llBOL_LIN,llASN_SHIP,llBOL_LIN ,llHangFlat,;
*!*	             llSYCASNLB
*B039957,1 HBG 11/20/2005 Add flag to Open the new file EDICRTSQ [Begin]
*STORE .F. TO llBrowse,llEdiFound,llPkChrDes,llToStOrCn,llEdiExist,llUpdate,llEDIInstl,;
	llEdiAcPrt,llEdiPh,llEdiPd,llBOL_Hdr,llBOL_LIN,llASN_SHIP,llBOL_LIN ,llHangFlat,;
	llSYCASNLB,llSYCASNHD
STORE .F. TO llBrowse,llEdiFound,llPkChrDes,llToStOrCn,llEdiExist,llUpdate,llEDIInstl,;
	llEdiAcPrt,llEdiPh,llEdiPd,llBOL_Hdr,llBOL_LIN,llASN_SHIP,llBOL_LIN ,llHangFlat,;
	llSYCASNLB,llSYCASNHD,llEDICRTSQ 
*B039957,1 [End]
	
lcPrnAsnShp = gfTempName()
*E302210,1 WLD 09/09/2004 Using visual label report [End]

lcWarCode  = SPACE(10)
lcBrTit    = 'Cartons'


laDefProc[9]  = .F.     && Save procedure(lpSavScr)
lafolders[1,1] = 'Header'
lafolders[1,2] = 1
lafolders[2,1] = 'Scan'
lafolders[2,2] = 2


*-- End Declaration variables.
*-- Folder coordinations (starting row,col and ending row,col)
lnFolderCEnd = 97.60
lnFolderREnd = 2.00

*-- Number of folders
lnNoFld      =   2
lcfolder     = gfTempName()      && Folder Window Name
lcwfoldchng  = '=lfActFolder()'
lnActFolder  = 1
lafoldwinds[1,1] = 'Header'
lafoldwinds[1,2] = lcWinCh2
lafoldwinds[2,1] = 'Scan'
lafoldwinds[2,2] = lcWinCh3
lcfoldprnt  = gcBaseWind        && window parent name for the folder
lnactfolder = 1                 && active folder

lafoldwinds[1,1] = lafolders[1,1]
lafoldwinds[1,2] = lcWinCh2
lafoldwinds[2,1] = lafolders[2,1]
lafoldwinds[2,2] = lcWinCh3


*-- shipvia
laCodInfo[1,01] = "SHIPVIA"                 && Field Name
laCodInfo[1,02] = "laShpVia"                && Array Name
laCodInfo[1,03] = "lnShpVia"                && Popup Name
laCodInfo[1,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
laCodInfo[1,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[1,06] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[1,07] = "Pack_Hdr"                && Alternative File (For default val.)
laCodInfo[1,08] = "Pack_Hdr"                && Use this index for the Alternative file.
laCodInfo[1,09] = "lcShipVia"               && Seek this expretion.
laCodInfo[1,10] = "ShipVia"                 && Alternative Field Name
= gfwCodePop(@laCodInfo, "SHIPVIA", "N")

*E302144,1 AMH Change the custom C102789 to be standard [Start]
lcRPCart = SPACE(6)
*E302144,1 AMH [End]


IF !gfSetup()
	RETURN
ENDIF

*E302144,1 AMH Change the custom C102789 to be standard [Start]
*C102789,4 ASH 02/15/2003 (Begin) Define a cursor to hold the carton type value.
*IF ASCAN(laEvntTrig , PADR('INITVAR',10)) <> 0
*  =gfDoTriger('ALPKSCN',PADR('INITVAR',10))
*ENDIF
*C102789,4 ASH 02/15/2003 (End)
*E302144,1 AMH [End]


*E301784,1 ABD - Apply the pack&scan program for EIL10
*E301784,1 ABD - On the new standard  program that we
*E301784,1 ABD - Did based on ENH#500443. [Begin]
*-- This trigger only for our customer Eileen Fisher to scan the hang and
*-- Flat style in the pick scan file, other wise I will get all
*-- The pick style in the automatic pack to let the user to use Auto Pack.
IF ASCAN(laEvntTrig , PADR('HANGFLAT',10)) <> 0
	llHangFlat = .T.
ENDIF
*E301784,1 Abd - [End]

*-- Check if Edi Module is Install.
llEDIInstl = ('AS' $ gcComp_mdl)
STORE .F. TO llDyelot , llMulWare , llUseUpc , lcManufId

*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
*DIMENSION laSetup[4,2]
DIMENSION laSetup[5,2]
*E040123,1 HBG 03/13/2006 [End]

laSetUp[1,1] = 'M_DYELOT'
laSetUp[2,1] = 'M_WareHouse'
laSetUp[3,1] = 'M_UPC_USE'
laSetUp[4,1] = 'XMANUFID'
*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
laSetup[5,1] = 'M_UCCBSDON'
*E040123,1 HBG 03/13/2006 [End]

=gfGetMemVar(@laSetUp,gcAct_Comp)

llDyelot  = ALLTRIM(laSetUp[1,2]) = 'Y'
llMulWare = ALLTRIM(laSetUp[2,2]) = 'Y'
llUseUpc  = ALLTRIM(laSetUp[3,2]) = 'Y'
lcManufId = PADL(ALLTRIM(laSetUp[4,2]),7,'0')

*E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]

llDispMsg  = .F.
=gfOpenFile(gcDataDir+"SETUPS",gcDataDir+'MODVAR','SH')
=gfOpenFile(gcSysHome+"SYCCONFG",gcSysHome+'MODVAR','SH')
IF SEEK('ALM_UCCBSDON','SYCCONFG') AND EVAL(ALLTRIM(SYCCONFG.mData_def)) = 0
  PUSH KEY
  lcTitle  = 'Select UCC # Structure'
  DO (gcScrDir + '\ALUCCSTR.SPR')
  POP KEY
  SELECT SYCCONFG
  REPLACE mVentries WITH '5 Digits of PL # + 4 Digits for Carton #|6 Digits of PL # + 3 Digits for Carton #|9 Digits for Carton #~5|6|9',;
          mData_def WITH '5'
  IF SEEK('ALM_UCCBSDON','SETUPS') 
    SELECT SETUPS
    REPLACE mVentries WITH SYCCONFG.mVentries ,;
            mData_def WITH ALLTRIM(STR(lnNumOfDig))
  ELSE
    SELECT SYCCONFG           
    SCATTER MEMVAR MEMO
    SELECT SETUPS
    APPEND BLANK
    m.mData_def = ALLTRIM(STR(lnNumOfDig))
    GATHER MEMVAR MEMO
  ENDIF
ELSE
  lnNumOfDig = EVAL(laSetup[5,2])
ENDIF
llIncPLNum = (lnNumOfDig = 5 OR lnNumOfDig = 6)
*E040123,1 HBG 03/14/2006 [End]

*-- Initialize some variables
*-- Create temporary files
*-- Windows Temp Name.
lcWinCh1    = gfTempName()
lcWinCh2    = gfTempName()
lcWinCh3    = gfTempName()
lcWinCh4    = gfTempName()
lcWinCh5    = gfTempName()
lcScFields  = 'Pack_No'

*E302144,1 AMH Change the custom C102789 to be standard [Start]
*C102789,4 ASH [Start] Add new field "CCRTNVLTYP" for KrazyCat to the field list in the variable "lcScFields"
*IF ASCAN(laEvntTrig , PADR('ADDTPFLD',10)) <> 0
*  =gfDoTriger('ALPKSCN',PADR('ADDTPFLD',10))
*ENDIF
*C102789,4 ASH [End  ]
lcScFields = lcScFields + ',CCRTNVLTYP'
*E302144,1 AMH [End]

SELECT (lcBaseFile)
SCATTER FIELDS &lcScFields MEMO TO laData BLANK


*-- If Edi Instl open needed file from this module.
IF llEDIInstl
	*-- call function to open need files.
	= lfOpenFils ()
ENDIF

*-- Create temp files we will use in this program.
= lfvbuild ()

*B606886,1 ABD - Define Option menu pad for Printing port. [Begin]
*-- llCanPrnLb : - Hold The State Of The User Rights If He can Print Or Not.
*-- llScrPrnLb : - Hold The State of the option menue print label after each carton
*-- lcSndPort  : - Hold the name of the printing port.
llCanPrnLb = gfUserPriv('AL','ALPLIST','PRNPACKING')
llSkip = .F.
IF llCanPrnLb
	llScrPrnLb = .F.
	lcSndPort  = "COM2"

	*Detail Label B
	lcDetLbAll = ''
	*Detail Label E

ENDIF
= lfActPad()
*B606886,1 ABD - [End]


IF !llMulWare
	GOTO TOP  IN WareHous
	lcWrName  = WareHous.cDesc
	lcWrAddr1 = WareHous.cAddress1
	lcWrAddr2 = WareHous.cAddress2
	lcWrCity  = ALLTRIM(WareHous.cAddress3)
	lcWrState = ALLTRIM(WareHous.cAddress4)
	lcWrZip   = ALLTRIM(WareHous.cAddress5)
ENDIF


SELECT STYLE
SET RELATION TO
SET RELATION TO 'S'+Scale INTO SCALE ADDITIVE
SELECT PikTkt
SET FILTER TO STATUS = 'O'
SET RELATION TO
SET RELATION TO 'M'+ Account INTO Customer, 'O'+ORDER INTO OrdHdr ADDITIVE
lcConfStat = SET('CONFIRM')


*-- Run the Pack and scan Screen.
PUSH KEY
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTit)

DO (gcScrDir+gcAct_Appl+"\ALPkScn.SPX")
POP KEY

*B606886,1 ABD - Release the optional menu. [Begin]
= lfRELPad ()
*B606886,1 ABD - [End]
IF glQuitting

	IF USED(lcTmpBrow)
		SELECT (lcTmpBrow)
		USE
		ERASE (gcWorkDir+lcTmpBrow+'.DBF')
		ERASE (gcWorkDir+lcTmpBrow+'.CDX')
	ENDIF

	IF USED(lcTmpData)
		SELECT (lcTmpData)
		USE
		ERASE (gcWorkDir+lcTmpData+'.DBF')
		ERASE (gcWorkDir+lcTmpData+'.CDX')
		ERASE (gcWorkDir+lcTmpData1+'.CDX')
	ENDIF

	SELECT (lcTmpPkLin)
	USE
	ERASE (gcWorkDir+lcTmpPkLin+'.DBF')
	ERASE (gcWorkDir+lcTmpPkLin+'.CDX')

	IF USED(TmpAsnShip)
		SELECT (TmpAsnShip)
		USE
		ERASE (gcWorkDir+TmpAsnShip+'.DBF')
		ERASE (gcWorkDir+TmpAsnShip+'.CDX')
	ENDIF

	*-- If Edi Instl close opend files from this module.
	IF llEDIInstl
		*-- call function to Close opened files.
		= lfClosFils ()
	ENDIF

ENDIF


*-- End of program code.
*:*************************************************************
*: Name      : lfNonMaj
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/18/2001
*: Purpose   : To get the style nonmajor segement structure
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfNonMaj ()
*:*************************************************************
*:
FUNCTION lfNonMaj

lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnNonMajPo = 0
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
	lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
	IF laMajSeg[lnI,1] = 'F' AND !llStopConc
		lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
		lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
			lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
		lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
			lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
	ENDIF
	IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
		IF laMajSeg[lnI,1] = 'C'
			lnClrPo    = laMajSeg[lnI,4]
			lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
			lcNonMajPi = laMajSeg[lnI,3]
			lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
			EXIT
		ELSE
			llStopConc = .T.
		ENDIF
	ENDIF
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
RETURN ''
*-- End OF lfNonMaj.
*:*************************************************************
*: Name      : lfvAccount
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/18/2001
*: Purpose   : Function to Validate account
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvAccount()
*:*************************************************************
*:
FUNCTION lfvAccount


IF llBrowse .OR. (!EMPTY(lcAccount) .AND. !SEEK('M'+lcAccount,'CUSTOMER'))
	DO CUSBrowM WITH lcAccount
ENDIF
llBrowse = .F.

IF EMPTY(lcAccount)
	STORE '' TO lcStore
	SHOW GET lcStore DISABLE
	SHOW GET pbStore DISABLE
ELSE
	SHOW GET lcStore ENABLE
	SHOW GET pbStore ENABLE
ENDIF

=lfRefresh(lcWinCh1)

*-- End Of lfvAccount.
*:*************************************************************
*: Name      : lfvPackNo
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/18/2001
*: Purpose   : Function to Validate Pack#
*:*************************************************************
*: Calls     : = lfvPikTkt ()
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvPackNo()
*:*************************************************************
*:
FUNCTION lfvPackNo
PRIVATE lnAlias
lcPackNo = PADR(lcPackNo,6)
IF llBrowse .OR. !EMPTY(lcPackNo)
	= lfvPikTkt ()
ENDIF
llBrowse = .F.

*-- End Of lfvPackNo.
*:*************************************************************
*: Name      : lfvPikTkt
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/18/2001
*: Purpose   : Function to Validate Pack#
*:*************************************************************
*: Calls     :gfModalGen , AriaBrow , lpShow
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvPikTkt()
*:*************************************************************
*:
FUNCTION lfvPikTkt

PRIVATE laBrowVal
PRIVATE laAddress
DIMENSION laBrowVal[1],laAddress[6,3]
STORE '' TO laBrowVal,laAddress
SELECT PikTkt
IF llBrowse .OR. (!EMPTY(lcPackNo) .AND. !SEEK(lcPackNo))
	lcBrFields  = [PikTkt          :H='Piktkt#'      ,]+;
		[STATUS          =IIF(STATUS = 'O' , 'Open' , IIF(STATUS = 'H' ,'On Hold',IIF(STATUS = 'C','Complete', IIF(STATUS = 'K','Packed','Pulled')))) :R :H= 'Status  '              ,]+;
		[Account         :H='Account'      ,]+;
		[Customer.StName :H='Account Name' ,]+;
		[Store           :H='Store'        ,]+;
		[Order           :H='Order Number' ,]+;
		[OrdHdr.Complete :H='Complete Date',]+;
		[LABELS          :H='Labels'       ,]+;
		[CWARECODE       :H='Warehouse'    ,]+;
		[CUSTPO          :H='Customer PO#'  ]

	DO CASE
	CASE !EMPTY(lcOrderNo)
		SET ORDER TO TAG ORDPIK IN PIKTKT
		IF !SEEK(lcOrderNo)
			*- Text Message   :- No picking ticket found for order XXX.
			*- Text Number    :- 44028
			*- button message :- OK
			*- button Number  :- 00000
			=gfModalGen("INM44028B00000","Dialog",lcOrderNo)
			RETURN
		ELSE
			IF AriaBrow('lcOrderNo','Pick Tickets',gnBrFSRow1, gnBrFSCol1,;
					gnBrFSRow2, gnBrFSCol2,.F.,.F.,"PikTkt.PikTkt","laBrowVal")
				lcPackNo = laBrowVal[1]
			ELSE
				STORE SPACE(6) TO lcPackNo
			ENDIF
		ENDIF
		SET ORDER TO TAG PIKTKT IN PIKTKT
	OTHERWISE
		LOCATE FOR ACCOUNT = ALLTRIM(lcAccount) AND STORE = ALLTRIM(lcStore)
		IF !FOUND()
			DO CASE
			CASE EMPTY(lcAccount)
				*- Text Message   :- No picking tickets found.
				*- Text Number    :- 44062
				*- button message :- OK
				*- button Number  :- 00000
				=gfModalGen("INM44062B00000","Dialog")
			CASE EMPTY(lcStore)
				*- Text Message   :- No picking ticket found for account xxx.
				*- Text Number    :- 44043
				*- button message :- OK
				*- button Number  :- 00000
				=gfModalGen("INM44043B00000","Dialog",lcAccount)
			CASE !EMPTY(lcStore)
				*- Text Message   :- No picking ticket found for account xxx.
				*- Text Number    :- 44043
				*- button message :- OK
				*- button Number  :- 00000
				=gfModalGen("INM44043B00000","Dialog",lcStore)
			ENDCASE
			STORE SPACE(6) TO lcPackNo
			RETURN
		ELSE
			IF AriaBrow('FOR ACCOUNT = ALLTRIM(lcAccount) AND;
					STORE = ALLTRIM(lcStore)','Pick Tickets',gnBrFSRow1, gnBrFSCol1,;
					gnBrFSRow2, gnBrFSCol2,.F.,.F.,"PikTkt.PikTkt","laBrowVal")
				lcPackNo =  laBrowVal[1]
			ELSE
				STORE SPACE(6) TO lcPackNo
			ENDIF
		ENDIF
	ENDCASE
	llBrowse = .F.
ENDIF

IF !EMPTY(lcPackNo)
	IF !SEEK(lcPackNo,'PACK_HDR')
		*- Text Message   :- Packing Slip# XXX not found.
		*- Text Number    :- 44029
		*- button message :- \!\<Add;\?\<Reenter
		*- button Number  :- 44004
		*B607420,1  TMI [Start] check that if ordline file is locked , if no Lock and continue , if Yes , exit
		*IF gfModalGen("QRM44029B44004","Dialog",lcPackNo) = 2
		IF !lfOrdLck(.T.) .OR. gfModalGen("QRM44029B44004","Dialog",lcPackNo) = 2
			*B607420,1  TMI [End  ]
			STORE .F. TO laScrMode
			laScrMode [1] = .T.
			SHOW GETS
		ELSE
			laScrMode [4] = .T.
			lcOrderNo = PikTkt.Order
			lcAccount = PikTkt.Account
			lcStore   = PikTkt.Store
			=SEEK('O'+lcOrderNo,'OrdHdr')
			=SEEK(IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore),'Customer')
			lcDistCtr  = Customer.Dist_Ctr
			lcCustPo   = PikTkt.CustPo
			lcDept     = OrdHdr.Dept

			*-- Pick up Order Division long
			SET ORDER TO TAG CCODE_NO IN CODES
			=SEEK('N'+PADR('CDIVISION',10)+ORDHDR.CDIVISION+SPACE(30)+'DIVLNAME','CODES')
			lcDivLName = CODES.cRltd_Vlu
			SET ORDER TO TAG CODES IN CODES

			lcWarCode  = PikTkt.cWareCode
			lcShipVia  = Customer.ShipVia
			=gfwCodePop(@laCodInfo, "SHIPVIA", "V,"+lcShipVia)

			IF llEDIInstl
				llEdiFound = SEEK ('A'+lcAccount,'EdiAcPrt')
				llEdiExist = SEEK (EdiAcPrt.cPartCode,'EdiPh')
				llPkChrDes = llEdiFound AND llEdiExist AND EdiAcPrt.lPkChrDes
				llToStOrCn = llEdiFound AND llEdiExist AND EdiAcPrt.cMulUccLbl='Y'
				lcPkChTxt  = IIF(llPkChrDes,'Pack Characteristic  :','')
				lcPkDsTxt  = IIF(llPkChrDes,'Pack Description     :','')
				lcPackChar = IIF(llPkChrDes,EdiAcPrt.cPckChCode,SPACE(5))
				lcPackDesc = IIF(llPkChrDes,EdiAcPrt.cPckDsCode,SPACE(7))
				*E302210,1 WLD 09/09/2004 Using visual label report
				llDetLabel = .F.
				lcDetailVr =  ''
				IF Ediph.lDtlbl
					llDetLabel = .T.
					lcDetailVr = eDiph.cDtlbl
				ENDIF
				*E302210,1 WLD (End)

			ENDIF
			lnToStorCn = 0

			*-- Labels Informations
			lcEvenCode = OrdHdr.Event_Cod
			lcVendNo   = Customer.cCusVend

			*E302210,1 WLD 09/09/2004 Using visual label report
			ldOrdCanc  = OrdHdr.Complete
			lcOrdNote1 = OrdHdr.NOTE1
			lcOrdNote2 = OrdHdr.NOTE2
			*E302210,1 WLD (End)

			IF llMulWare .AND. SEEK(lcWarCode,'WareHous')
				lcWrName  = WareHous.cDesc
				lcWrAddr1 = WareHous.cAddress1
				lcWrAddr2 = WareHous.cAddress2
				lcWrCity  = WareHous.cAddress3
				lcWrState = WareHous.cAddress4
				lcWrZip   = WareHous.cAddress5
			ENDIF

			=SEEK(IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore),'Customer')
			lcDistCtr = Customer.Dist_Ctr
			lcStName  = Customer.StName

			*-- ShipTo address
			=gfGetAdr('CUSTOMER','','','',1,'')
			lcStAddr1 = laAddress[1,2]
			lcStAddr2 = laAddress[2,2]
			lcStCity  = laAddress[3,2]
			lcStState = laAddress[4,2]
			lcStZip   = laAddress[5,2]

			*--  Check if partner doing 856
			IF llEDIInstl
				lcAlias = ALIAS()
				SELECT EDIPD
				IF SEEK(EdiAcPrt.cPartCode+'856')
					*B605884,1 ABD - If the there are many BOL's with the same
					*B605884,1 ABD - Account,store,....   of the packing list,
					*B605884,1 ABD - The user should be able to browse them
					*B605884,1 ABD - And select one of them. [Begin]
					*SELECT BOL_HDR
					*lcOldBlOrd = ORDER()
					*SET ORDER TO Bolacc
					*= SEEK(lcAccount)
					*
					* LOCATE REST WHILE Account = lcAccount FOR Store+W_CODE+cDivision+ShipVia = ;
					*       lcDistCtr+lcWarCode+OrdHdr.cDivision+lcShipVia .AND. Status <> 'C'
					*SET ORDER TO &lcOldBlOrd
					*IF !FOUND()
					*  lcBol = gfSequence('BOL_NO')
					*  INSERT INTO BOL_HDR ;
					*   (BOL_NO,ACCOUNT,STORE,STANCARTON,BOLDATE,W_CODE,cDivision,ShipVia) VALUES ;
					*   (lcBol,lcAccount,lcDistCtr,'N',gdSysDate,lcWarCode,OrdHdr.cDivision,lcShipVia)
					*ELSE
					*  lcBol = BOL_HDR.BOL_NO
					*ENDIF
					*IF !SEEK(lcBol+lcOrderNo+lcPackNo,'BOL_LIN')
					*  INSERT INTO BOL_LIN (BOL_NO,ORDER,PACK_NO) VALUES (lcBol,lcOrderNo,lcPackNo)
					*ENDIF

					*-- Create BOL#
					PRIVATE laFields      && array to hold defulted fields for BOL

					DIMENSION laFields[3,2]
					laFields[1,1] = "cgronhang"
					laFields[1,2] = "N"
					laFields[2,1] = "ctranmthd"
					laFields[2,2] = "M"
					laFields[3,1] = "packtype"
					laFields[3,2] = "CTN25"
					lcBol = lfGetBOL("",lcAccount,lcStore,lcWarCode,OrdHdr.ShipVia,'N',"laFields")
					*B605884,1 ABD - [End]
				ENDIF
				SELECT (lcAlias)
			ENDIF

			=SEEK('S'+lcAccount+lcDistCtr,'Customer')
			*-- ShipTo address
			=gfGetAdr('CUSTOMER','','','',1,'')

			lcDCName  = Customer.StName
			lcDCAddr1 = laAddress[1,2]
			lcDCAddr2 = laAddress[2,2]
			lcDCCity  = laAddress[3,2]
			lcDCState = laAddress[4,2]
			lcDCZip   = laAddress[5,2]

			*E301784,1 ABD - This next Check Variable only for our customer Eileen Fisher to scan the hang and
			*E301784,1 ABD - Flat style in the pick scan file, other wise I will get all
			*E301784,1 ABD - The pick style in the automatic pack to let the user to use Auto Pack. [Begin]
			IF llHangFlat
				= lfvAddPack ("")
			ELSE
				*-- This Function Will show the Order Qty and the Piked Qty.
				= lfvAdOrdPk ("")
			ENDIF
			*E301784,1 ABD - [End]
		ENDIF
	ELSE
		laScrMode     = .F.
		laScrMode [2] = .T.
		*E301784,1 ABD - This next Check Variable only for our customer Eileen Fisher to scan the hang and
		*E301784,1 ABD - Flat style in the pick scan file, other wise I will get all
		*E301784,1 ABD - The pick style in the automatic pack to let the user to use Auto Pack. [Begin]
		IF llHangFlat
			= lfvAddPack ()
		ELSE
			*-- This Function Will show the Order Qty and the Piked Qty.
			= lfvAdOrdPk ()
		ENDIF
		*E301784,1 ABD - [End]
	ENDIF
ENDIF

*-- End Of lfvPikTkt.
*:*************************************************************
*: Name      : lfActFolder
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/18/2001
*: Purpose   : Function to be used when moving through folders
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfActFolder ()
*:*************************************************************
*:
FUNCTION lfActFolder


DO CASE
CASE lnActFolder = 1
	lcStatus = IIF(laScrMode[3] .OR. laScrMode[4] ,'ENABLE','DISABLE')
	SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
	SHOW GETS WINDOW (lcWinCh4) DISABLE ONLY
	ACTIVATE WINDOW  (lcWinCh2)
	SHOW GETS WINDOW (lcWinCh2) &lcStatus ONLY
	=lfRefresh(lcWinCh2)


	*-- Accepte to 1 carton , disable carton field.
	IF llAccOne
		SHOW GET lnTotCart DISABLE
	ENDIF

	IF !llPkChrDes
		SHOW GET lcPackChar DISABLE
		SHOW GET lcPackDesc DISABLE
	ENDIF
	IF !llToStOrCn
		SHOW GET lnToStorCn DISABLE
	ENDIF

	IF laScrMode[1]
		SHOW GET ibFolder[2] DISABLE
	ELSE
		SHOW GET ibFolder[2] ENABLE
		SHOW GET pbStore DISABLE
		SHOW GET lcStore DISABLE
	ENDIF

CASE lnActFolder = 2
	SELECT (lcTmpPkLin)
	= lfBrows()

	SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
	SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
	SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY

	IF laScrMode [1] .AND. lnCartonNo > 0
		SHOW GET pbPrntLab ENABLE
	ENDIF
	IF laScrMode [4] .OR. laScrMode [3]

		IF lnCartonNo > 0
			SHOW GET lnQty1    ENABLE
			SHOW GET pbNewUpc  ENABLE
			IF !EOF(lcTmpPkLin)
				SHOW GET pbPrntLab ENABLE
				SHOW GET pbRemUpc  ENABLE
			ENDIF
			SHOW GET pbAutoPak ENABLE
			IF llAutoPack
				SHOW GET pbNewCart ENABLE
			ENDIF
		ELSE
			IF EOF(lcTmpPklin)
				SHOW GET pbNewCart ENABLE
			ENDIF
		ENDIF
	ENDIF

	IF laScrMode[4]
		SHOW GET pbStore DISABLE
		SHOW GET lcStore DISABLE
	ENDIF


ENDCASE

*-- End OF lfActFolder.
*!************************************************************************
*! Name      : lpShow
*! Developer : ABDOU ELGENDI
*! Date      : 05/24/99
*! Purpose   : Show objecrts function.
*!************************************************************************
*! Parameters: None
*!************************************************************************
*! Returns   :  None.
*!************************************************************************
*! Example   :  =lpShow()
*!************************************************************************
*
FUNCTION lpShow


SHOW GET pbDlt      DISABLE
SHOW GET pbCpCalndr DISABLE
SHOW GET pbcptask   DISABLE
SHOW GET pbcpCalc   DISABLE


*B606886,1 ABD - Move This Code To lfActPad - [Begin]
*C102789,4 ASH 02/16/2003 (Begin) Add a new option menu to hold carton type.
*IF ASCAN(laEvntTrig , PADR('ACTPAD',10)) <> 0
*  =gfDoTriger('ALPKSCN',PADR('ACTPAD',10))
*ENDIF
*C102789,4 ASH 02/16/2003 (End)
*B606886,1 ABD - [End]



DO CASE
	***--- S E L E C T   M O D E ---***
CASE laScrMode[1]

	*B607420,1  TMI [Start] Release ordhdr lock
	=lfOrdLck()
	*B607420,1  TMI [End  ]

	*-- In case you are in Folder 2
	SHOW GET ibFolder[2] DISABLE
	IF lnactfolder <> 1
		lnlastfold  = lnactfolder
		lnactfolder = 1
		= lfchngfolder(lnactfolder)
	ENDIF

	= lfReIntiVr ()

	llReBrow = .T.
	SHOW GET pbPackNo   ENABLE
	SHOW GET lcPackNo   ENABLE
	SHOW GET pbAccount  ENABLE
	SHOW GET lcAccount  ENABLE
	SHOW GET pbStore    DISABLE
	SHOW GET lcStore    DISABLE
	SHOW GET pbOrder    ENABLE
	SHOW GET lcOrderNo  ENABLE
	SHOW GET lcCustPo   ENABLE
	SHOW GET lcDept     DISABLE
	=lfRefresh(lcWinCh1)
	_CUROBJ = OBJNUM(lcPackNo)


	***--- V I E W   M O D E ---***
CASE laScrMode[2]
	lcPackNo = Piktkt.Piktkt
	= lfvPackInfo()
	SHOW GET lcPackChar DISABLE COLOR &lcObjColor
	SHOW GET lcPackDesc DISABLE COLOR &lcObjColor
	SHOW GET lnToStorCn DISABLE

	IF !(laScrMode[4])
		*-- Function to Update the Temp Data file With the Old Qty.
		= lfUpDtTmp ()
		*-- Function to add the upc records in the lcTmpPkLin file.
		= lfGetUpc ()
	ENDIF
	= lfActFolder()
	IF lnActFolder = 2
		IF lnCartonNo > 0
			SHOW GET pbPrntLab ENABLE
		ENDIF
		=lfBrows()
		SHOW GET pbNewUpc   DISABLE
		SHOW GET pbRemUpc   DISABLE
		SHOW GET pbNewCart  DISABLE
		SHOW GET pbAutoPak  DISABLE
		SHOW GET pbPrntLab  DISABLE
	ENDIF

	IF !llToStOrCn
		SHOW GET lnToStorCn DISABLE
	ENDIF


	***--- E D I T   M O D E ---***
CASE laScrMode[3]
	IF lnActFolder = 1
		SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
		*B038498,1 WLD Total catons not correct [Begin]
		*SHOW GET lnTotCart   DISABLE
		SHOW GET lnTotCart   ENABLE
		*B038498,1 WLD Total catons not correct [End]
		SHOW GET lnShpVia    ENABLE
		SHOW GET lcNotes     ENABLE
		SHOW GET lcSp_Inst1  ENABLE
		SHOW GET lcSp_Inst2  ENABLE

		IF llPkChrDes
			SHOW GET lcPackChar ENABLE
			SHOW GET lcPackDesc ENABLE
		ELSE
			SHOW GET lcPackChar DISABLE
			SHOW GET lcPackDesc DISABLE
		ENDIF
	ENDIF

	IF lnActFolder = 2 .AND. lnCartonNo > 0
		SHOW GET lnQty1    ENABLE
		SHOW GET pbPrntLab ENABLE
		SHOW GET pbNewUpc  ENABLE
		SHOW GET pbRemUpc  ENABLE

		=lfBrows()
		IF llAccOne
			SHOW GET pbNewCart DISABLE
		ELSE
			SHOW GET pbNewCart ENABLE
			SHOW GET pbAutoPak ENABLE
		ENDIF
	ENDIF
	STORE .T. TO llAutoPack

	SHOW GET pbPackNo  DISABLE
	SHOW GET lcPackNo  DISABLE
	SHOW GET pbAccount DISABLE
	SHOW GET lcAccount DISABLE
	SHOW GET pbOrder   DISABLE
	SHOW GET lcOrderNo DISABLE
	SHOW GET lcCustPo  DISABLE
	SHOW GET pbStore   DISABLE
	SHOW GET lcStore   DISABLE

	IF !llToStOrCn
		SHOW GET lnToStorCn DISABLE
	ENDIF

	*E302144,1 AMH Change the custom C102789 to be standard [Start]
	*C102789,4 ASH 02/16/2003 (Begin) Get carton type from pack_hdr.
	*IF ASCAN(laEvntTrig , PADR('GETDATA',10)) <> 0
	*  =gfDoTriger('ALPKSCN',PADR('GETDATA',10))
	*ENDIF
	*C102789,4 ASH 02/16/2003 (End)
	lcRPCart = PACK_HDR.CCRTNVLTYP
	*E302144,1 AMH [End]

	***--- A D D   M O D E ---***
CASE laScrMode[4]
	SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
	= lfRefresh(lcWinCh2)

	SHOW GET pbPackNo  DISABLE
	SHOW GET lcPackNo  DISABLE
	SHOW GET pbAccount DISABLE
	SHOW GET lcAccount DISABLE
	SHOW GET pbOrder   DISABLE
	SHOW GET lcOrderNo DISABLE
	SHOW GET lcCustPo  DISABLE
	SHOW GET pbStore   DISABLE
	SHOW GET lcStore   DISABLE


	SHOW GET ibFolder[2] ENABLE
	IF llPkChrDes
		SHOW GET lcPackChar ENABLE
		SHOW GET lcPackDesc ENABLE
	ELSE
		SHOW GET lcPackChar DISABLE
		SHOW GET lcPackDesc DISABLE
	ENDIF

	SHOW GET lnShpVia   ENABLE
	SHOW GET lcNotes    ENABLE
	SHOW GET lcSp_Inst1 ENABLE
	SHOW GET lcSp_Inst2 ENABLE

	IF !llToStOrCn
		SHOW GET lnToStorCn DISABLE
	ENDIF

	IF lnActFolder = 1 .AND. llAccOne
		SHOW GET lnTotCart DISABLE
	ENDIF

	IF lnActFolder = 2 .AND. lnCartonNo > 0
		IF llAccOne
			SHOW GET pbAutoPak ENABLE
			SHOW GET pbPrntLab ENABLE
		ENDIF

		*-- Disable most variable in Folder 2.
		IF llAutoPack
			SHOW GET pbNewUpc  DISABLE
			SHOW GET pbRemUpc  DISABLE
			SHOW GET pbAutoPak DISABLE
			SHOW GET pbPrntLab DISABLE
			_CUROBJ = OBJNUM(pbNewCart)
		ENDIF
	ENDIF

ENDCASE

*B606886,1 ABD - Activate the optional menu Pad. [Begin]
=lfActPad()
*B606886,1 ABD - [End]

SELECT (lcBaseFile)
*-- END OF lpShow

*:*************************************************************
*: Name      : lfOpenFils
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Open need file when EDi Install.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfOpenFils()
*:*************************************************************
*
FUNCTION lfOpenFils

IF !USED('EDISCPRT')
	llEdiAcPrt = gfOpenFile(gcDataDir+'EdiAcPrt','AccFact','SH')
ENDIF

IF !USED('EDIPH')
	llEdiPh = gfOpenFile(gcDataDir+'EdiPh','Partner','SH')
ENDIF

IF  !USED('EDIPD')
	llEdiPd = gfOpenFile(gcDataDir+'EdiPd','PARTTRANS','SH')
ENDIF

IF !USED('BOL_HDR')
	llBOL_Hdr = gfOpenFile(gcDataDir+'BOL_HDR','BOL_HDR','SH')
ENDIF

IF !USED('BOL_LIN')
	llBOL_LIN =gfOpenFile(gcDataDir+'BOL_LIN','BOL_LIN','SH')
ENDIF

IF !USED('ASN_SHIP')
	llASN_SHIP = gfOpenFile(gcDataDir+'Asn_Ship','Asn_Ship','SH')
	COPY STRUCTURE TO (gcWorkDir+TmpAsnShip)
	=gfOpenFile(gcWorkDir+TmpAsnShip,'','EX')
	INDEX ON PACK_NO+STR(CART_NO,6) TAG (TmpAsnShip)
ENDIF

*B039957,1 HBG 11/20/2005 Open the new file EDICRTSQ [Begin]
IF !USED('EDICRTSQ')
  llEDICRTSQ =gfOpenFile(gcDataDir + 'EDICRTSQ' , gcDataDir + 'EDICRTSQ' , 'SH')
ENDIF
*B039957,1 [End]

IF !USED('SYCASNLB')
	llSYCASNLB =gfOpenFile(gcSysHome+'SYCASNLB','ASNlbl','SH')
ENDIF
*E302210,1 WLD 09/09/2004 Using visual label report [Begin]
IF !USED('SYCASNHD')
	llSYCASNHD =gfOpenFile(gcSysHome+'SYCASNHD','VerPrt','SH')
ENDIF
=gfOpenFile(gcDataDir+'SPCK_LIN','Spcklins','SH')
*E302210,1 WLD 09/09/2004 Using visual label report [End  ]

*-- End of lfOpenFils
*:*************************************************************
*: Name      : lfClosFils
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Close files when close the screen.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfClosFils()
*:*************************************************************
*
FUNCTION lfClosFils

IF llEdiAcPrt
	=gfCloseFile('EdiAcPrt')
ENDIF

IF llEdiPh
	=gfCloseFile('EdiPh')
ENDIF

IF llEdiPd
	=gfCloseFile('EdiPd')
ENDIF

IF llBOL_Hdr
	=gfCloseFile('BOL_Hdr')
ENDIF

IF llBOL_LIN
	=gfCloseFile('BOL_LIN')
ENDIF

IF llASN_SHIP
	=gfCloseFile('ASN_SHIP')
ENDIF

*B039957,1 HBG 11/20/2005 Close the new file EDICRTSQ [Begin]
IF llEDICRTSQ
  =gfCloseFile('EDICRTSQ')
ENDIF	
*B039957,1 [End]

IF llSYCASNLB
	=gfCloseFile('SYCASNLB')
ENDIF

*E302210,1 WLD 09/09/2004 Using visual label report [Begin]
IF llSYCASNHD
	=gfCloseFile('SYCASNHD')
ENDIF
*E302210,1 WLD 09/09/2004 Using visual label report [End]

*-- End Of lfClosFils.
*:*************************************************************
*: Name      : lfvStore
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Validate store.
*:*************************************************************
*: Calls     : CUSBROWS.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvStore()
*:*************************************************************
*
FUNCTION lfvStore

PRIVATE xStore
IF llBrowse OR (!EMPTY(lcStore) AND !SEEK('S'+lcAccount+lcStore,'CUSTOMER'))
	xStore   = lcStore
	IF !CUSBROWS(lcAccount,.T.)
		STORE SPACE(8) TO xStore
	ENDIF
	lcStore = xStore
ENDIF
llBrowse = .F.
=lfRefresh(lcWinCh1)

*-- End of lfvStore.
*:*************************************************************
*: Name      : lfvOrder
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Validate Order #.
*:*************************************************************
*: Calls     : ORDBROWO,lfvPikTkt.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvOrder()
*:*************************************************************
*
FUNCTION lfvOrder
PRIVATE xStore,xOrder,xAccount
xAccount = lcAccount
xStore   = lcStore
xOrder   = lcOrderNo

IF EMPTY(lcAccount)
	IF llBrowse OR (!EMPTY(lcOrderNo) AND !SEEK('O'+lcOrderNo,'ORDHDR'))
		=ORDBROWO(@lcOrderNo)
	ENDIF
ELSE
	SET ORDER TO TAG ORDACCT IN ORDHDR
	IF llBrowse OR (!EMPTY(lcOrderNo) AND !SEEK(lcAccount+'O'+lcOrderNo,'ORDHDR'))
		llBrowse = .T.
		=ORDBROWA(lcAccount,.T.)
		lcOrderNo = xOrder
	ENDIF
	SET ORDER TO TAG ORDHDR IN ORDHDR
ENDIF
llBrowse = .F.

IF !EMPTY(lcOrderNo)
	lcAccount = OrdHdr.Account
	lcStore   = OrdHdr.Store
	lcCustPo  = OrdHdr.CustPo
	=lfvPikTkt()
ENDIF

*-- End of lfvOrder.
*:*************************************************************
*: Name      : lfvCustPo
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Validate CustPo#
*:*************************************************************
*: Calls     : lfvPikTkt.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvCustPo()
*:*************************************************************
*
FUNCTION lfvCustPo

IF !EMPTY(lcAccount) .OR. !EMPTY(lcCustPo)
	= lfvPikTkt()
ENDIF

*-- End Of lfvCustPo.
*:*************************************************************
*: Name      : lfReIntiVr
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to reintializing the global variables
*:*************************************************************
*: Calls     : lfvPikTkt.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfReIntiVr()
*:*************************************************************
*
FUNCTION lfReIntiVr
PRIVATE lnAlias

lnAlias = ''
lnAlias = SELECT (0)
STORE 0 TO lnTotCart , lnTotPiece , lnTotWg , lnShipVia , lnToStorCn,;
	lnQty1 ,lnQtyWg , lnCartonNo , lnLastCart , laCartons ,lnScnToQty


lcPackNo   = SPACE(06)
lcOrderNo  = SPACE(06)
lcAccount  = SPACE(05)
lcStore    = SPACE(08)
lcCustPo   = SPACE(10)
lcDept     = SPACE(05)
lcDivLName =''
lcBol      = SPACE(06)
lcShipVia  = ''
lcNotes    = SPACE(30)
lcPackChar = SPACE(05)
lcPackDesc = SPACE(07)
lnToStorCn = 0
lcUpc      = SPACE(12)
lcStyle    = SPACE(12)
lcStyColor = SPACE(06)
lcSize     = SPACE(05)
lcSp_Inst1 = SPACE(78)
lcSp_Inst2 = SPACE(78)


= gfwCodePop(@laCodInfo, "SHIPVIA", "N")

STORE .F. TO llPkChrDes,llUpdate,llReBrow,llToStOrCn
STORE '' TO lcPkChTxt,lcPkDsTxt
SELECT (lcTmpPkLin)
ZAP

IF llEDIInstl .AND. USED(TmpAsnShip)
	SELECT (TmpAsnShip)
	ZAP
ENDIF
SELECT (lnAlias)

*-- End of lfReIntiVr.
*:*************************************************************
*: Name      : lfBrows
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to browse the packed style.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfBrows ()
*:*************************************************************
*
FUNCTION lfBrows

SELECT (lcTmpPkLin)
SET ORDER TO StyleSize

lnMarker = RECNO()
BROWSE FIELDS ;
	cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,;
	CartonNo :H= 'Crtn#' :R                                 ,;
	STYLE      = SUBSTR(STYLE,1,lnMajLen):12:R              ,;
	COLOR      = SUBSTR(STYLE,lnClrPo,lnColorLen):6:R         ,;
	cSize    :H= 'Size'  :R,;
	TOtQty   :H= 'Qty. ':6:R ;
	WINDOW    (lcWinCh4)    ;
	IN WINDOW (lcWinCh3)    ;
	NOMENU            ;
	NOAPPEND          ;
	NODELETE          ;
	NOWAIT            ;
	SAVE              ;
	NOCLEAR           ;
	WHEN lfShowData() ;
	TITLE lcBrTit

= lfShowData()
llReBrow = .F.

*-- End Fo lfBrows
*:*************************************************************
*: Name      : lfShowData
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   :  Function to display upc data when moving through
*:           :  upc records in the brows
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfShowData ()
*:*************************************************************
*
FUNCTION lfShowData

IF !EOF (lcTmpPkLin)
	lcUpc      = &lcTmpPkLin..cUpc
	lnQty1     = &lcTmpPkLin..TotQty
	lcStyle    = SUBSTR(&lcTmpPkLin..Style,1,lnMajLen)
	lcStyColor = SUBSTR(&lcTmpPkLin..Style,lnClrPo,lnColorLen)
	lcSize     = &lcTmpPkLin..cSize
	=SEEK(&lcTmpPkLin..Style,'Style')
	lnStyWg    = Style.nStyWeight
	lnCartonNo = &lcTmpPkLin..CartonNo
	SET ORDER TO StyleSize
ENDIF

IF LASTKEY() # 27
	SHOW GETS WINDOW (lcWinCh5)  ONLY
	=lfRefresh(lcWinCh5)
ENDIF
IF laScrMode[3] .OR. laScrMode[4]
	IF EOF(lcTmpPkLin)
		SHOW GET pbRemUpc DISABLE
	ELSE
		SHOW GET pbRemUpc ENABLE
		SHOW GET lnQty1   ENABLE
	ENDIF
ENDIF

IF !(laScrMode [2]) .AND. (llAccOne .OR. llAutoPack)
	SHOW GET pbRemUpc  ENABLE
	SHOW GET pbNewUpc  ENABLE
	SHOW GET pbAutoPak ENABLE
	SHOW GET pbPrntLab ENABLE
	IF llAutoPack
		SHOW GET pbNewCart ENABLE
	ELSE
		SHOW GET pbNewCart DISABLE
	ENDIF
ENDIF

SHOW GET lcUpc DISABLE
lnMarker = RECNO(lcTmpPkLin)
SHOW WINDOW (lcBrTit) REFRESH SAME

*-- End of lfShowData.
*:*************************************************************
*: Name      : lfvAddPack
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   :  Function that open a new screen
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvAddPack ()
*:*************************************************************
*E301784,1
*-- Refer to C#102285
FUNCTION lfvAddPack
PARAMETER lcFrom
PRIVATE lnHngOrd , lnFltOrd , lnTotOrd , lnHngScn , lnFltScn , lnTotScn , lcAlias
STORE 0 TO lnHngOrd , lnFltOrd , lnTotOrd , lnHngScn , lnFltScn , lnTotScn

lcAlias = SELECT(0)
=gfOpenFile(gcDataDir+'PICKSCAN','cPikTkt','SH')

*-- Function to delete data from used file.
= lfReUsefls ()


SELECT PickScan
=SEEK (lcPackNo)
IF FOUND()
	SCAN REST WHILE LEFT(ALLTRIM(cPikTkt),6) = lcPackNo
		IF RIGHT(ALLTRIM(cPikTkt),1) = "H"
			lnHngScn = lnHngScn + nScn_Qty
		ELSE
			lnFltScn = lnFltScn + nScn_Qty
		ENDIF
		SELECT OrdLine
		lcOrder = ORDER()
		SET ORDER TO OrdLines
		=SEEK (PickScan.Style)
		IF FOUND()
			SCAN REST WHILE STYLE+DTOS(COMPLETE)+cOrdType+ORDER+STORE+STR(LINENO,6) = PickScan.Style FOR PikTkt = lcPackNo
				=SEEK (PickScan.Style,'Style')
				SELECT (lcTmpData)
				APPEND BLANK
				lcQtyNo = lfGetScl(OrdLine.Style,PickScan.cPik_Size)
				IF Style.chanFld = "H"
					lnHngOrd = lnHngOrd + OrdLine.Qty&lcQtyNo
				ELSE
					lnFltOrd = lnFltOrd + OrdLine.Qty&lcQtyNo
				ENDIF
				REPLACE UPC        WITH PickScan.UPC                             ,;
					STYLE      WITH OrdLine.Style                            ,;
					SIZE       WITH lcQtyNo                                  ,;
					cSize      WITH Scale.Sz&lcQtyNo                         ,;
					nStyWeight WITH Style.nStyWeight                         ,;
					TotQty     WITH IIF(EMPTY(lcQtyNo),0,OrdLine.Qty&lcQtyNo),;
					TotPik     WITH IIF(EMPTY(lcQtyNo),0,OrdLine.Pik&lcQtyNo),;
					nScn_Qty   WITH PickScan.nScn_Qty                        ,;
					nRemQty    WITH nRemQty + PickScan.nScn_Qty              ,;
					LINENO     WITH Ordline.lineNo

			ENDSCAN
		ENDIF
	ENDSCAN
	lnTotScn = lnHngScn + lnFltScn
	lnTotOrd = lnHngOrd + lnFltOrd
	SELECT OrdLine
	SET ORDER TO TAG (lcOrder)

	*-- Order Quantity should ALWAYS be displayed.  Currently, it is only
	*-- displaying if the items have been scanned.  Because the Packers may not
	*-- receive the Hanging and Flat portions of the Pick Ticket at the same time,
	*-- it is essential that they know there are more items on the order.

ELSE
	SELECT OrdLine
	=SEEK('O'+OrdHdr.Order)
	SCAN REST WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+OrdHdr.Order FOR PikTkt = lcPackNo
		=SEEK (STYLE,'Style')
		IF Style.chanFld = "H"
			lnHngOrd = lnHngOrd + OrdLine.TotQty
		ELSE
			lnFltOrd = lnFltOrd + OrdLine.TotQty
		ENDIF
	ENDSCAN
	lnTotOrd = lnHngOrd + lnFltOrd
ENDIF
SELECT (lcAlias)
IF TYPE("lcFrom") = "C"
	PUSH KEY
	ON KEY LABEL ESC
	ON KEY LABEL ESC DO lpClose

	DO (gcScrDir+gcAct_Appl+"\ALEIL80.SPX")
	POP KEY

ENDIF
SHOW GETS

*-- End of lfvAddPack.
*:*************************************************************
*: Name      : lfvAdOrdPk
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to display the order and piked Qty.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvAdOrdPk ()
*:*************************************************************
*
FUNCTION lfvAdOrdPk
PARAMETER lcFrom
PRIVATE lnAlias , lnStart , lcSizeScl
STORE 0 TO lnTotOrdr , lnTotPktk ,lnStart
STORE '' TO lcSizeScl

lnAlias = SELECT (0)

*-- Function to delete data from used file.
= lfReUsefls ()


SELECT (lcTmpData)
SET ORDER TO &lcTmpData

SELECT OrdLine
=SEEK('O'+OrdHdr.Order)
SCAN REST WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+OrdHdr.Order FOR PikTkt = lcPackNo
	STORE 1 TO lnStart
	lnTotOrdr = lnTotOrdr + OrdLine.TotQty
	lnTotPktk = lnTotPktk + OrdLine.Totpik
	= SEEK(Ordline.Style,'STYLE')
	FOR lnStart = 1 TO SCALE.CNT
		lcSizeScl = STR(lnStart,1)
		IF OrdLine.Pik&lcSizeScl = 0
			LOOP
		ENDIF
		IF SEEK(Ordline.Style +lcSizeScl,'StyleUpc')
			SELECT (lcTmpData)
			IF !SEEK(Ordline.Style +lcSizeScl)
				APPEND BLANK
			ENDIF
			REPLACE Upc        WITH StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3 ,;
				STYLE      WITH OrdLine.Style                                         ,;
				cSize      WITH Scale.Sz&lcSizeScl                                    ,;
				nStyWeight WITH Style.nStyWeight                                      ,;
				SIZE       WITH lcSizeScl                                             ,;
				TotQty     WITH TotQty  + OrdLine.Qty&lcSizeScl                       ,;
				TotPik     WITH TotPik  + OrdLine.Pik&lcSizeScl                       ,;
				nRemQty    WITH nRemQty + OrdLine.Pik&lcSizeScl                       ,;
				LINENO     WITH Ordline.lineNo
		ENDIF
	ENDFOR
ENDSCAN

IF TYPE("lcFrom") = "C"
	PUSH KEY
	ON KEY LABEL ESC
	ON KEY LABEL ESC DO lpClose

	DO (gcScrDir+gcAct_Appl+"\ALAdOrdP.SPX")
	POP KEY
ENDIF

SHOW GETS



SELECT (lnAlias)
*-- End Of lfvAdOrdPk.
*:*************************************************************
*: Name      : lpClose
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpClose
*:*************************************************************
*
PROCEDURE lpClose

_CUROBJ = OBJNUM(pbCancel)
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpClose
*:*************************************************************
*: Name      : lfvbuild
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Build a temp file for storing the browsing result.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvbuild ()
*:*************************************************************
*
FUNCTION lfvbuild


*-- File Will hold the lines in the browse screen.
IF USED(lcTmpPkLin)
	= lfReUsefls()
	RETURN
ENDIF

*E302144,1 AMH Change the custom C102789 to be standard [Start]
* CREATE TABLE (gcWorkDir+lcTmpPkLin) ;
(CartonNo N(4),cUpc C(12),STYLE C(19),cSize C(5), SIZE C(2),;
TotQty N(5),Weight N(10,2) ,OldCrtNo N(4), OrdLineNo N(6))
*C102789,4 ASH 02/16/2003 (Begin) Add a carton type fieled to the temp. file.
*IF ASCAN(laEvntTrig , PADR('CRTTMP',10)) <> 0
*  =gfDoTriger('ALPKSCN',PADR('CRTTMP',10))
*ENDIF
*C102789,4 ASH 02/16/2003 (End)
CREATE TABLE (gcWorkDir+lcTmpPkLin) ;
	(CartonNo N(4),cUpc C(12),STYLE C(19),cSize C(5), SIZE C(2),;
	TotQty N(5),Weight N(10,2) ,OldCrtNo N(4), OrdLineNo N(6), CCRTNVLTYP C(6))
*E302144,1 AMH [End]

INDEX ON STR(OrdLineNo,6)+STR(CartonNo,4)+cUpc TAG OrdLineNo
INDEX ON STR(CartonNo,4)+STYLE+SIZE+STR(OrdLineNo,6) TAG StyleSize ADDITIVE
INDEX ON STR(CartonNo,4)+cUpc+STR(OrdLineNo,6) TAG (lcTmpPkLin)    ADDITIVE


*-- File will hold the UPC that will return from the in range screen.
CREATE TABLE (gcWorkDir + lcTmpBrow) (UPC C(12))
ZAP
INDEX ON UPC TAG (lcTmpBrow)
=gfOpenFile(gcWorkDir+lcTmpBrow,'','SH')

*-- File Will hold all the data from order line file.
CREATE TABLE (gcWorkDir + lcTmpData) (UPC C(12), STYLE C(19),;
	cSize C(5),SIZE C(2), TotQty N(7), TotPik N(6),;
	nScn_Qty N(10),LINENO N(6), nStyWeight N(5.2) ,;
	nRemQty  N(10))

INDEX ON STYLE+SIZE TAG (lcTmpData) OF (gcWorkDir + lcTmpData)
INDEX ON UPC TAG (lcTmpData1) OF (gcWorkDir + lcTmpData)
ZAP
=gfOpenFile(gcWorkDir+lcTmpData,'','SH')

*-- End OF lfvbuild.
*:*************************************************************
*: Name      : lfvCancel
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   :Function to Cancel a new add screen.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvCancel()
*:*************************************************************
*
FUNCTION lfvCancel
CLEAR READ

laScrMode = .F.
laScrMode[1] =.T.
= lpShow()


*-- End of lfvCancel.
*:*************************************************************
*: Name      : lfvShipVia
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to validate the shipvia.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvShipVia ()
*:*************************************************************
*
FUNCTION lfvShipVia

lcShipVia = laShpVia[lnShpVia,2]

*-- End OF lfvShipVia.
*:*************************************************************
*: Name      : lfvaccept
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function of Accept to one carton Button
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvaccept ()
*:*************************************************************
*
FUNCTION lfvaccept
CLEAR READ

STORE .T. TO llCUpdate , llAccOne

lnLastCart = 1
lnCartonNo = lnLastCart
*-- Add all recored in one carton.
lnCartonNo = IIF(lnCartonNo=0,1,lnCartonNo)

SELECT (lcTmpData)
LOCATE
SCAN WHILE !EOF()
	lcUpc = UPC
	lcStySize = ALLTRIM(StyleUpc.Size)
	SELECT (lcTmpPkLin)
	IF !SEEK (STR(lnCartonNo,4)+lcUpc+STR(OrdLine.LineNo,6))
		INSERT INTO (lcTmpPkLin) (CartonNo,STYLE,cSize,SIZE,cUpc,OrdLineNo) VALUES ;
			(lnCartonNo,&lcTmpData..Style,&lcTmpData..cSize,&lcTmpData..Size,lcUpc,&lcTmpData..LineNo)
	ENDIF

	REPLACE TotQty   WITH TotQty + &lcTmpData..TotPik     ,;
		Weight  WITH Weight + &lcTmpData..nStyWeight
	lnTotWg    = lnTotWg    + (&lcTmpData..nStyWeight * &lcTmpData..TotPik)
	lnTotPiece = lnTotPiece + &lcTmpData..TotPik
	laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] + (&lcTmpData..nStyWeight * &lcTmpData..TotPik)
	*-- Update the Remmining Qty After Accepte to One Carton.
	SELECT (lcTmpData)
	REPLACE nRemQty WITH 0
ENDSCAN

SELECT (lcTmpPkLin)
LOCATE
lnTotCart  = 1
laCartons[lnCartonNo,1] = lnTotPiece

= lfClearFld ()

SHOW GET ibFolder[2] ENABLE
laScrMode = .F.
laScrMode[4] =.T.


*-- End of lfvaccept.
*:*************************************************************
*: Name      : lfClearFld
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to clear the scan variables.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfClearFld ()
*:*************************************************************
*
FUNCTION lfClearFld

STORE SPACE(12) TO lcUpc, lcStyle
STORE 0 TO lnStyWg, lnQty1
lcStyColor = SPACE(6)
lcSize     = SPACE(5)

*-- End of lfClearFld.
*:*************************************************************
*: Name      : lfAddRec
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to Add record in the lcTmpPkLin file
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfAddRec ()
*:*************************************************************
*
FUNCTION lfAddRec
PRIVATE lcOldOrder , lnAlias , lcOrderNo

lnAlias = SELECT (0)
IF SEEK(StyleUpc.Style,'Style')
	lnCartonNo = IIF(lnCartonNo=0,1,lnCartonNo)
	lcStySize = ALLTRIM(StyleUpc.Size)
	SELECT (lcTmpPkLin)
	lcOrderNo = ORDER ()
	SET ORDER TO (lcTmpPkLin)
	IF !SEEK (STR(lnCartonNo,4)+lcUpc+STR(OrdLine.LineNo,6))
		INSERT INTO (lcTmpPkLin) (CartonNo,STYLE,cSize,SIZE,cUpc,OrdLineNo) VALUES ;
			(lnCartonNo,Style.Style,Scale.Sz&lcStySize,ALLTRIM(StyleUpc.Size),lcUpc,OrdLine.LineNo)
	ENDIF
	SET ORDER TO &lcOrderNo
	*-- add the scan Qty in case the user pick more than 1 piece. [Begin]
	IF lnScnToQty = 0
		REPLACE TotQty  WITH TotQty+1 ,;
			Weight  WITH Weight+Style.nStyWeight
		lnTotWg    = lnTotWg    + Style.nStyWeight
		lnTotPiece = lnTotPiece + 1
		laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] + 1
		laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] + Style.nStyWeight
	ELSE
		REPLACE TotQty  WITH TotQty + lnScnToQty ,;
			Weight  WITH Weight+(Style.nStyWeight*lnScnToQty)

		lnTotWg    = lnTotWg    + (Style.nStyWeight*lnScnToQty)
		lnTotPiece = lnTotPiece + lnScnToQty
		laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] + lnScnToQty
		laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] + (Style.nStyWeight * lnScnToQty)
	ENDIF
	SELECT (lcTmpData)
	lcOldOrder = ORDER()
	SET ORDER TO &lcTmpData1
	IF SEEK (lcUpc,lcTmpData)
		REPLACE nRemQty WITH nRemQty  - IIF(lnScnToQty= 0 , 1, lnScnToQty)
	ENDIF
	SET ORDER TO &lcOldOrder

	*-- Increase the Qty in the remining file. [Begin]
	= lfBrows ()
	SELECT (lcTmpPkLin)
	=lfRefresh(lcWinCh5)
	*-- Assign Zero to total scaned qty and activate the browse. [Begin]
	lnScnToQty = 0
ENDIF

SELECT (lnAlias)
*-- End OF lfAddRec.
*:*************************************************************
*: Name      : lfWQty
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : When Function for upc quantity field.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfWQty ()
*:*************************************************************
*
FUNCTION lfWQty
lcOldValue = lnQty1

*-- End of lfWQty.
*:*************************************************************
*: Name      : lfvQty
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Valid Function for upc quantity field.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvQty ()
*:*************************************************************
*
FUNCTION lfvQty
PRIVATE lnAlias , lcOldOrder
lnAlias = ''


*-- 3 cases to return again to the field or to return to th screen.
DO CASE
CASE (TYPE("lnQty1") # "N") OR (lnQty1 > 99999)
	lnQty1 = lcOldValue
	SHOW GET lnQty1
	_CUROBJ = _CUROBJ
	RETURN
CASE lnQty1 <= 0
	lnQty1 = lcOldValue
	SHOW GET lnQty1
	RETURN
CASE lnCartonNo = 0
	RETURN
ENDCASE


lnAlias = SELECT (0)
SELECT (lcTmpPkLin)

lnRecNo = RECNO()

SUM TotQty TO lnPacked FOR cUPc = lcUpc
IF BETWEEN(lnRecNo,1,RECCOUNT())
	GO lnRecNo
ENDIF

= SEEK(STYLE+SIZE,'STYLEUPC')


SELECT OrdLine
lcOldOrder = ORDER()
SET ORDER TO Ordlinst

STORE 0   TO lnPicked,lnOrdered
=SEEK('O'+lcOrderNo+lcStore+StyleUpc.Style)
SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
		'O'+lcOrderNo+lcStore+StyleUpc.Style FOR PikTkt = lcPackNo
	lnPicked  = lnPicked  + EVAL('PIK'+ALLTRIM(StyleUpc.Size))
	lnOrdered = lnOrdered + EVAL('QTY'+ALLTRIM(StyleUpc.Size))
	IF lnPacked+1 <= lnPicked
		EXIT
	ENDIF
ENDSCAN

=SEEK(StyleUpc.Style,'Style')
*-- Check on the order Qty.
IF (lnPacked - lcOldValue + lnQty1) > lnOrdered
	*- Message Text   :- Packed Quantity for Style/Size XXXXX exceeds
	*- Message Text   :- Ordered Quantity. Can not modify the Ordered Quantity?
	*- Message No.    :- 44112.
	*- Buttom Message :- Ok
	*- Buttom Number  :-00000
	= gfModalGen('INM44112B00000','DIALOG',&lcTmpPkLin..Style+'/'+&lcTmpPkLin..cSize)
	lnQty1 = lcOldValue
	_CUROBJ = OBJNUM(lnQty1)
	SELECT (lcTmpPkLin)
	RETURN
ENDIF

*-- Check on the Piked qty.
IF (lnPacked - lcOldValue + lnQty1) > lnPicked
	*- Message Text   :- Packed quantity for XX exceeds picked quantity.
	*- Message Text   :- Do you want to add in the packing list?
	*- Message No.    :- 44073.
	*- Buttom Message :- Ok
	*- Buttom Number  :- 44009
	IF gfModalGen('QRM44073B44009','DIALOG',StyleUpc.Style+'\'+EVAL('SCALE.SZ'+ALLTRIM(StyleUpc.Size)) ) = 2
		lnQty1 = lcOldValue
		_CUROBJ = OBJNUM(lnQty1)
		SELECT (lcTmpPkLin)
		RETURN
	ENDIF
ENDIF

*-- Update the temp file with the new Qty.
SELECT (lcTmpPkLin)
REPLACE TotQty WITH TotQty - lcOldValue + lnQty1 ,;
	Weight WITH TotQty * lnStyWg
laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] - lcOldValue + lnQty1
laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] - (lcOldValue-lnQty1)*lnStyWg
lnTotWg    = lnTotWg    - (lcOldValue-lnQty1) * lnStyWg
lnTotPiece = lnTotPiece - lcOldValue + lnQty1



SELECT OrdLine
SET ORDER TO &lcOldOrder
*--  Descrese or increase the scand qty in remining file also. [Begin]

SELECT (lcTmpData)
lcOldOrder = ORDER()
SET ORDER TO &lcTmpData1
IF SEEK (&lcTmpPkLin..cUpc,lcTmpData)
	REPLACE nRemQty WITH nRemQty + lcOldValue - lnQty1
ENDIF
SET ORDER TO &lcOldOrder


=lfRefresh(lcWinCh5)
SHOW WINDOW (lcBrTit) REFRESH SAME
SELECT (lnAlias)
*-- End Of lfvQty.

*:*************************************************************
*: Name      : lfPrntLab
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to Print the packing slip lable.
*:*************************************************************
*: Calls     : lfPrnUcc128,lfCheckNo.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfPrntLab()
*:*************************************************************
*
FUNCTION lfPrntLab
PARAMETERS lnCarton
PRIVATE lcStyle,lcColor,lcSize,lnAlias

lnAlias = SELECT(0)
SELECT (lcTmpPkLin)
lnRecNo = RECNO()
=SEEK(STR(lnCarton,4))
lcStyle  = SUBSTR(STYLE,1,lnMajLen)
lcColor  = SUBSTR(STYLE,lnClrPo,lnColorLen)
lcUpdSty = STYLE
lcSize   = cSize

*E302210,1 WLD 09/09/2004 Using visual label report
lcUpc     = cUpc
lnLblInfo = 0
DIMENSION laLblInfo[1, 6]
lcCrtnSku = SPACE(16)
IF SEEK('S' + lcAccount + &lcTmpPkLin..Style, 'Spck_Lin')
	SELECT SPCK_LIN
	LOCATE REST ;
		WHILE TYPE + Account + STYLE = 'S' + lcAccount + &lcTmpPkLin..Style ;
		FOR EVAL('Qty'+&lcTmpPkLin..Size) = 1
	IF FOUND()
		lcCrtnSku = PADR(Pack_Id, 16)
	ELSE
		=SEEK('S' + lcAccount + &lcTmpPkLin..Style, 'Spck_Lin')
		LOCATE REST;
			WHILE TYPE + Account + STYLE = 'S' + lcAccount + &lcTmpPkLin..Style ;
			FOR TotQty = 0
		IF FOUND()
			lcCrtnSku = PADR(Pack_Id, 16)
		ENDIF
	ENDIF
ENDIF
SELECT (lcTmpPkLin)
*E302210,1 WLD (End)

SCAN REST WHILE STR(CartonNo,4) = STR(lnCarton,4)
	lcStyle = IIF(lcStyle<>SUBSTR(STYLE,1,lnMajLen),SPACE(12),lcStyle)
	lcColor = IIF(EMPTY(lcStyle),SPACE(6),IIF(lcColor<>SUBSTR(STYLE,lnClrPo,lnColorLen),'MIXED',lcColor))
	lcSize  = IIF(EMPTY(lcStyle),SPACE(5),IIF(lcSize <>cSize,'MIXED',lcSize))

	*E302210,1 WLD 09/09/2004 Using visual label report
	lcSKU = ""
	IF SEEK('S' + lcAccount + &lcTmpPkLin..Style, 'Spck_Lin')
		SELECT SPCK_LIN
		LOCATE REST ;
			WHILE TYPE + Account + STYLE = 'S' + lcAccount + &lcTmpPkLin..Style ;
			FOR EVAL('Qty'+&lcTmpPkLin..Size) = 1
		IF FOUND()
			lcSKU = PADR(Pack_Id, 16)
		ELSE
			=SEEK('S' + lcAccount + &lcTmpPkLin..Style, 'Spck_Lin')
			LOCATE REST;
				WHILE TYPE + Account + STYLE = 'S' + lcAccount + &lcTmpPkLin..Style ;
				FOR TotQty = 0
			IF FOUND()
				lcSKU = PADR(Pack_Id, 16)
			ENDIF
		ENDIF
	ENDIF
	SELECT (lcTmpPkLin)
	lcCrtnSku = IIF(lcCrtnSku <> lcSKU,SPACE(16),lcSKU)
	lcUpc     = IIF(lcUpc <> cUpc,SPACE(12),cUpc)
	lnLblInfo = lnLblInfo + 1
	DIMENSION laLblInfo[lnLblInfo, 6]
	laLblInfo[lnLblInfo,1] = lcSKU
	laLblInfo[lnLblInfo,2] = TotQty
	laLblInfo[lnLblInfo,3] = ALLTRIM(cSize)
	laLblInfo[lnLblInfo,4] = STYLE
	laLblInfo[lnLblInfo,5] = cUpc
	laLblInfo[lnLblInfo,6] = SIZE
	*E302210,1 WLD (End)
ENDSCAN

IF BETWEEN(lnRecNo,1,RECCOUNT())
	GOTO lnRecNo IN (lcTmpPkLin)
ENDIF

*B605884,1 ABD - Don't add Shipment in case we havn't BOL #.  [Begin]
*IF llEDIInstl
IF llEDIInstl .AND. !EMPTY(lcBol)
	*B605884,1 ABD - [End]
	=SEEK ('A'+lcAccount,'EdiAcPrt') .AND. SEEK(EdiAcPrt.cPartCode,'EdiPH')
	lcUccVer = IIF(lnToStorCn=0,EdiPH.cASNLbl1,EdiPH.cASNLbl2)
	lcUccVer = IIF(EMPTY(lcUccVer),'XXX',lcUccVer)

	SELECT (tmpAsnShip)
	*B039957,1 HBG 11/15/2005 Get the Carton # form the new file EDICRTSQ instead of calculating it [Begin]
    *lcUCC9   = RIGHT(PADL(ALLTRIM(lcPackNo),6,'0'),5)+PADL(lnCarton,4,'0')
    *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
    IF llIncPLNum
      lcUCC9  = RIGHT(PADL(ALLTRIM(lcPackNo),6,'0'),lnNumOfDig)+PADL(lnCarton,IIF(lnNumOfDig = 5,4,3),'0')
    ELSE 
    *E040123,1 HBG 03/13/2006 [End]
      SET ORDER TO PCKCRTSQ IN EDICRTSQ
      IF SEEK(lcPackNo+STR(lnCarton,6),'EDICRTSQ')
        lcUCC9  = PADL(EDICRTSQ.Ucc9,9,'0')
      *--mhm2006
      ELSE
        lcUCC9  =''
      *--mhm2006  
      ENDIF
    *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
    ENDIF
    *E040123,1 HBG 03/13/2006 [End]
    *B039957,1 [End]

	*E037191,1 SSE 10/12/2003 Add MUCB no variable. [Begin]
	lcManufId  = PADL(lfManufID(lcBol),7,'0')
	*E037191,1 SSE 10/12/2003 Add MUCB no variable. [End]

	lcCheckNo = lfCheckNo('000'+lcManufId+lcUcc9)

	IF !SEEK(lcPackNo+STR(lnCarton,6))
		APPEND BLANK
	ENDIF
	REPLACE Cart_No   WITH lnCarton  ,;
		VND_NAME  WITH lcWrName  ,;
		VND_ADDR1 WITH lcWrAddr1 ,;
		VND_ADDR2 WITH lcWrAddr2 ,;
		VND_CITY  WITH lcWrCity  ,;
		VND_STATE WITH lcWrState ,;
		VND_ZIP   WITH lcWrZip   ,;
		STORE     WITH IIF(EMPTY(lcDistCtr), lcStore  , lcDistCtr) ,;
		SHP_NAME  WITH IIF(EMPTY(lcDistCtr), lcStName , lcDCName)  ,;
		SHP_ADDR1 WITH IIF(EMPTY(lcDistCtr), lcStAddr1, lcDCAddr1) ,;
		SHP_ADDR2 WITH IIF(EMPTY(lcDistCtr), lcStAddr2, lcDCAddr2) ,;
		SHP_CITY  WITH IIF(EMPTY(lcDistCtr), lcStCity , lcDCCity)  ,;
		SHP_STATE WITH IIF(EMPTY(lcDistCtr), lcStState, lcDCState) ,;
		SHP_ZIP   WITH IIF(EMPTY(lcDistCtr), lcStZip  , lcDCZip)   ,;
		CUSTPO    WITH lcCustPo  ,;
		DEPT      WITH lcDept    ,;
		MANUF_ID  WITH lcManufId ,;
		UCC9      WITH lcUCC9    ,;
		UCC_CHECK WITH lcCheckNo ,;
		ASN_VER   WITH lcUccVer  ,;
		EVENT_COD WITH lcEvenCode
	*-- Store Pack_No in the ASN_SHIP file.
	REPLACE PACK_NO   WITH lcPackNo  ,;
		ShipVia   WITH lcShipVia ,;
		Carrier   WITH ALLTRIM(gfCodDes(lcShipVia,'SHIPVIA   ')),;
		Int_Vend  WITH lcVendNo  ,;
		Bol_No    WITH lcBol     ,;
		cStStore  WITH lcStore   ,;
		cStName   WITH lcStName  ,;
		cStAddr1  WITH lcStAddr1 ,;
		cStAddr2  WITH lcStAddr2 ,;
		cStCity   WITH lcStCity  ,;
		cStState  WITH lcStState ,;
		cStZip    WITH lcStZip   ,;
		TotQty    WITH laCartons[lnCarton,1] ,;
		Cartons   WITH lnTotCart ,;
		STYLE     WITH lcUpdSty ,;
		cClrDesc  WITH IIF(lcColor='MIXED',lcColor,gfCodDes(lcColor,'COLOR     ')) ,;
		cSizeDesc WITH lcSize

	*E302210,1 WLD 09/09/2004 Using visual label report
	REPLACE Note1     WITH lcOrdNote1 ,;
		Note2     WITH lcOrdNote2 ,;
		Cancelled WITH ldOrdCanc  ,;
		cUPC      WITH lcUpc      ,;
		Pack_Id   WITH lcCrtnSku  ,;
		cPRO_NO   WITH Bol_hdr.cPRO_NO
	=gfAdd_Info(tmpAsnShip)
	IF TYPE("laLblInfo[1,4]") = "C" .AND. !EMPTY(laLblInfo[1,4])
		SAVE TO MEMO MLBLINFO ALL LIKE laLblInfo
	ENDIF
	IF llCanPrnLb .AND. llScrPrnLb .AND. !EMPTY(lcSndPort)

		*E302210,1 WLD 09/09/2004 Using visual label report
		*-- Check for the detail Version for current account.
		IF llDetLabel
			IF EMPTY(lcDetLbAll)
				*-- Do you want to print detailed label for carton number ð.
				*-- <Yes> <Yes to All> <No> <No to All>
				lnChoice = gfModalGen('TRM44105B40016' , 'DIALOG' , ALLTRIM(STR(lnCarton , 4)))
				DO CASE
				CASE lnChoice = 1
					llPrintLbl = .T.
				CASE lnChoice = 2
					llPrintLbl = .T.
					lcDetLbAll = "Y"
				CASE lnChoice = 3
					llPrintLbl = .F.
				CASE lnChoice = 4
					llPrintLbl = .F.
					lcDetLbAll = "N"
				ENDCASE
			ELSE
				llPrintLbl = (lcDetLbAll = "Y")
			ENDIF
		ENDIF
		*E302210,1 WLD (End)
		*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [Begin]
		SELECT SycAsnHd
		GO TOP
		LOCATE FOR cver = lcUccVer AND cType = 'Y'
		IF FOUND()
			SELECT (tmpAsnShip)
			*IF SEEK(lcUccVer,'SycAsnHd') AND SycAsnHd.cType = 'Y' && lcVSULBL
			*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [End  ]
			=lfVsulLbl(lcUccVer)
		ELSE
			*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [Begin]
			LOCATE FOR cver = lcUccVer AND cType = 'N'
			IF FOUND()
				SELECT (tmpAsnShip)
				*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [End  ]
				*E302210,1 wld Using visual label report [End]
				=lfPrnUcc128(lcUccVer)
				*E302210,1 wld Using visual label report [Begin ]
				WAIT CLEAR
				*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [Begin]
			ENDIF
			*E038766,1 WLD 12/15/2004 Change selecting label type (visual/monarch) [End  ]
		ENDIF
	ENDIF
	*E302210,1 wld Using visual label report [End ]

ENDIF
SELECT (lcTmpPkLin)

SELECT (lnAlias)
*-- End OF lfPrntLab.
*:*************************************************************
*: Name      : lfPrnUcc128
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to Print the packing slip lable.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfPrnUcc128()
*:*************************************************************
*
FUNCTION lfPrnUcc128
PARAMETERS lcVersion
PRIVATE lnHandle, lcOutFile, lcString, lcData, lnAlaias
lnAlaias = SELECT (0)
WAIT 'Generating Shipping Label...Please stand by....' WINDOW NOWAIT
lcString  = SPACE(40)
SELECT SYCASNLB
IF !SEEK(lcVersion+'H') .AND. gfModalGen('INM44068B00000','DIALOG' )=1
	RETURN
ENDIF

lcOutFile = gcWorkDir+lcAsnLabel+".TXT"
lnHandle  = FCREATE(lcOutFile,0)
=FSEEK(lnHandle,0,2)

SCAN WHILE 	cVer+cEdiType = lcVersion+'H'
	STORE DATA TO lcData
	lcString = &lcData
	=FPUTS(lnHandle,lcString)
ENDSCAN

lcString = SPACE(3)
=FPUTS(lnHandle,lcString)
SELECT (tmpAsnShip)
SCATTER MEMVAR
*-- Pickup Order Division long name, Department name and style description.
m.DivLName = lcDivLName
=SEEK(lcAccount+lcDept,'CUSTDEPT')
m.DeptDesc = CustDept.cDeptDesc
=SEEK(ALLTRIM(lcUpdSty),'Style')
m.StyDesc = Style.Desc

*E037191,1 SSE 10/12/2003 Add MUCB no variable. [Begin]
MUCB = m.MANUF_ID + PADL(ALLTRIM(m.Bol_No) , 9 , '0')
MUCB = MUCB + lfCheckDgt(MUCB,'E')
*E037191,1 SSE 10/12/2003 Add MUCB no variable. [End]

TMP_ADDR = ALLTRIM(VND_CITY) + ",  " +ALLTRIM(VND_STATE) + "   " + ALLTRIM(VND_ZIP)
SELECT SYCASNLB
=SEEK(lcVersion+'L')
SCAN WHILE cVer+cEdiType= lcVersion+'L'
	STORE DATA TO lcData
	lcString = &lcData
	=FPUTS(lnHandle,lcString)
ENDSCAN
lcString = SPACE(3)
=FPUTS(lnHandle,lcString)
=FCLOSE(lnHandle)
WAIT 'Sending Shipping Labels to Printer...Please stand by....' WINDOW NOWAIT


*B606886,1 ABD - Print the 128 UCC Label. [Begin]
IF llCanPrnLb .AND. llScrPrnLb .AND. !EMPTY(lcSndPort)
	lcCommand = "TYPE " + lcOutFile + " > " + lcSndPort
	!/N0 &lcCommand
	WAIT CLEAR
ENDIF
*B606886,1 ABD - [End]

WAIT CLEAR

SELECT(lnAlaias)

*-- End Of lfPrnUcc128.
*:*************************************************************
*: Name      : lfCheckNo
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Algorithm to Compute the modulus-10 Check digit
*: Purpose   : For the UCC 128 code
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : lcUccNo : UCC number without check digit
*:*************************************************************
*: Returns   : UCC Check digit
*:*************************************************************
*: Example   : =lfCheckNo('1234567890123456789')
*:*************************************************************
*
FUNCTION lfCheckNo
PARAMETER lcUccNo
PRIVATE lnChkDigit,lnSumOdd,lnSumEven,lnCount

STORE 0 TO lnSumOdd,lnSumEven,lnChkDigit
FOR lnCount = 1 TO 9
	lnSumOdd  = lnSumOdd + VAL(SUBSTR(lcUccNo,lnCount*2-1,1))
	lnSumEven = lnSumEven+ VAL(SUBSTR(lcUccNo,lnCount*2,1))
ENDFOR
lnSumOdd   = lnSumOdd + VAL(SUBSTR(lcUccNo,19,1))
lnChkDigit = MOD(lnSumOdd*3+lnSumEven,10)
RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))

*-- End OF lfCheckNo.
*:*************************************************************
*: Name      : lfvRemUpc
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Remove UPC from a carton.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : =lfvRemUpc ()
*:*************************************************************
*
FUNCTION lfvRemUpc
PRIVATE lnAlias , lcOldOrder

lnAlias = SELECT (0)
SELECT (lcTmpPkLin)
IF lnCartonNo <> 0 .AND. gfModalGen("QRM44067B44008","Dialog",cUpc+'|'+ALLTRIM(STR(lnCartonNo,4))) = 1
	laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] - TotQty
	laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] - Weight
	lnTotWg    = lnTotWg    - Weight
	lnTotPiece = lnTotPiece - TotQty

	*-- Descries the remaining qty in the data file.
	SELECT (lcTmpData)
	lcOldOrder = ORDER()
	SET ORDER TO &lcTmpData1
	IF SEEK (&lcTmpPkLin..cUpc,lcTmpData)
		REPLACE nRemQty WITH nRemQty + &lcTmpPkLin..TotQty
	ENDIF
	SET ORDER TO &lcOldOrder
	SELECT (lcTmpPkLin)
	DELETE
	llCUpdate  = .T.
	IF !SEEK(STR(lnCartonNo,4))
		WAIT 'Rearranging cartons...' WINDOW NOWAIT
		IF llEDIInstl AND SEEK(lcPackNo+STR(lnCartonNo,4),tmpAsnShip)
			SELECT (TmpAsnShip)
			DELETE
		ENDIF
		SELECT (lcTmpPkLin)
		LOCATE
		STORE 0 TO lnCartonNo,lnLastCart
		DO WHILE !EOF()
			lnLastCart = lnLastCart + 1
			lnCartonNo = lnLastCart

			*-- Update Cartons # each time Remove carton.
			lnTotCart = lnLastCart
			DIMENSION laCartons[lnCartonNo,2]
			STORE 0 TO laCartons[lnCartonNo,1],laCartons[lnCartonNo,2]
			lnOldCarton = CartonNo
			SCAN REST WHILE STR(CartonNo,4)+cUpc = STR(lnOldCarton,4)
				REPLACE CartonNo WITH lnCartonNo
				laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] + TotQty
				laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] + Weight
			ENDSCAN
		ENDDO
		WAIT CLEAR
		GO TOP
		IF lnCartonNo = 0
			=lfClearFld ()
			SHOW GETS WINDOW (lcWinCh5) DISABLE
			SELECT (lcTmpPkLin)
			LOCATE

			IF llAccOne
				SHOW GET pbNewCart DISABLE
			ELSE
				SHOW GET pbNewCart ENABLE
				_CUROBJ = OBJNUM(pbNewCart)
			ENDIF
		ENDIF
	ENDIF


	*-- assign Def to the total cartons number. [Begin]
	IF EOF()
		STORE 0 TO lnTotCart, lnTotPiece , lnTotWg
		SHOW GET pbNewCart ENABLE
	ENDIF
	SHOW GET lnTotCart DISABLE
	SHOW WINDOW (lcBrTit) REFRESH SAME
	=lfRefresh(lcWinch5)
ENDIF

SELECT (lnAlias)
*-- End Of lfvRemUpc
*:*************************************************************
*: Name      : lfNewCart
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to make a new carton
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : =lfNewCart ()
*:*************************************************************
*
FUNCTION lfNewCart
PARAMETER lcFrom
PRIVATE lnAlias , lnRecNo , llNewCrt , lnLastCrt
STORE .T. TO llNewCrt
STORE 0 TO lnAlias , lnRecNo , lnLastCrt
lnAlias = SELECT (0)

SELECT (lcTmpPkLin)
IF !EOF() .AND. !BOF()
	lnRecNo = RECNO()
	GOTO BOTTOM
	lnLastCrt = CartonNo
	IF  (lnLastCart+1) - lnLastCrt = 2
		GOTO lnRecNo
		llNewCrt = .F.
	ENDIF
	SELECT (lcTmpPkLin)
	GOTO lnRecNo
ELSE
	IF (lnLastCart+1) - lnLastCrt = 2
		RETURN
	ENDIF
ENDIF
SELECT (lnAlias)

IF llNewCrt  && Don't add new carton if you have a new caron allready. [Begin]
	*B038498,1 WLD Total catons not correct [Begin]
	IF lnLastCart <> 0
      = lfPrntLab(lnLastCart)
	ENDIF

	IF lnTotCart < (lnLastCart + 1)
		*- Message Text   :- Carton sequence excceeds total cartons.
		*- Message No.    :- 44123.
		*- Buttom Message :- \!\<OK;\?\<Cancel
		*- Buttom Number  :- 02011
		IF gfModalGen('INM44123B02011','DIALOG')=1
			lnLastCart = lnLastCart + 1
			lnTotCart  = lnTotCart  + 1
		ELSE
			RETURN
		ENDIF
	ELSE
		*B038498,1 WLD Total catons not correct [End  ]
		lnLastCart = lnLastCart + 1
		*B038498,1 WLD Total catons not correct [Begin]
		*lnTotCart  = lnTotCart  + 1
	ENDIF
	*B038498,1 WLD Total catons not correct [End  ]
ENDIF
*B038498,1 WLD Total catons not correct [Begin]
STORE lnLastCart TO lnCartonNo
*STORE lnLastCart TO lnCartonNo , lnTotCart
*B038498,1 WLD Total catons not correct [End  ]

DIMENSION laCartons[lnCartonNo,2]
STORE 0 TO laCartons[lnCartonNo,1],laCartons[lnCartonNo,2]

IF llAccOne
	SHOW GET pbNewCart DISABLE
ELSE
	SHOW GET pbNewCart ENABLE
	SHOW GET pbAutoPak ENABLE
ENDIF
= lfClearFld ()
*B038498,1 WLD Total catons not correct [Begin]
*IF lnCartonNo <> 1
*  = lfPrntLab(lnCartonNo-1)
*ENDIF
*B038498,1 WLD Total catons not correct [End  ]
SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
=lfRefresh(lcWinCh5)
SHOW GET lcUpc     ENABLE

SHOW GET lnTotCart DISABLE
SET CONFIRM ON

IF llAutoPack
	SHOW GET pbAutoPak ENABLE
	SHOW GET pbNewCart ENABLE
	SHOW GET pbNewUpc  ENABLE
ELSE
	SHOW GET pbAutoPak ENABLE
	SHOW GET pbNewUpc  ENABLE
ENDIF

SELECT (lnAlias)
*-- End Of lfNewCart.
*:*************************************************************
*: Name      : lfvUpc
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to validate the upc number.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvUpc ()
*:*************************************************************
*
FUNCTION lfvUpc
PARAMETER lcFrom
PRIVATE lnRecNo,lnPicked,lnOrdered ,lnAlias ,lcOldOrder
lcOldOrder = ''
lnAlias = SELECT (0)
lcUpc = PADR(ALLTRIM(lcUpc),12)
*-- Clear keyboard buffer as the scanner somtimes issue CHR(13) causing any
CLEAR TYPEAHEAD
SET ORDER TO TAG STYUPCN IN STYLEUPC
SET ORDER TO TAG Ordlinst IN ORDLINE

IF !EMPTY(lcUpc)
	IF !SEEK(lcUpc,'StyleUpc')
		*- Message Text   :- UPC XXXX is not found.
		*- Message No.    :- 44065.
		*- Buttom Message :- Ok
		*- Buttom Number  :- 00000
		=gfModalGen('INM44065B00000','DIALOG',lcUpc)
		lcUpc = SPACE(12)
		_CUROBJ = OBJNUM(lcUpc)
		RETURN
	ENDIF
	SELECT OrdLine
	=SEEK('O'+lcOrderNo+lcStore+StyleUpc.Style)
	LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
		'O'+lcOrderNo+lcStore+StyleUpc.Style;
		FOR PikTkt = lcPackNo

	IF !FOUND()
		*- Message Text   :- UPC XXX is not found in piktkt# XXX.
		*- Message No.    :- 44066.
		*- Buttom Message :- Ok
		*- Buttom Number  :- 00000
		=gfModalGen('INM44066B00000','DIALOG',lcUpc+'|'+lcPackNo)
		lcUpc = SPACE(12)
		_CUROBJ = OBJNUM(lcUpc)
		SELECT (lcTmpPkLin)
		RETURN
	ENDIF
	llCUpdate  = .T.
	SELECT (lcTmpPkLin)
	lnRecNo = RECNO()
	SUM TotQty TO lnPacked FOR cUPc = lcUpc
	IF BETWEEN(lnRecNo,1,RECCOUNT())
		GO lnRecNo
	ENDIF

	SELECT OrdLine
	STORE 0   TO lnPicked,lnOrdered
	=SEEK('O'+lcOrderNo+lcStore+StyleUpc.Style)
	SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
			'O'+lcOrderNo+lcStore+StyleUpc.Style FOR PikTkt = lcPackNo
		lnPicked  = lnPicked  + EVAL('PIK'+ALLTRIM(StyleUpc.Size))
		lnOrdered = lnOrdered + EVAL('QTY'+ALLTRIM(StyleUpc.Size))
		IF lnPacked+1 <= lnPicked
			EXIT
		ENDIF
	ENDSCAN

	=SEEK(StyleUpc.Style,'Style')
	*- Message Text   :- Packed Quantity for Style/Size XXXXX exceeds
	*- Message Text   :- Ordered Quantity. Can not modify the Ordered Quantity?
	*- Message No.    :- 44112.
	*- Buttom Message :- Ok
	*- Buttom Number  :-00000
	IF lnPacked+1 > lnOrdered
		= gfModalGen('INM44112B00000','DIALOG',&lcTmpPkLin..Style+'/'+&lcTmpPkLin..cSize)
		lcUpc = SPACE(12)
		_CUROBJ = OBJNUM(lcUpc)
		SELECT (lcTmpPkLin)
		RETURN
	ENDIF

	IF lnPacked > lnPicked
		*- Message Text   :- Packed quantity for ð exceeds picked quantity.
		*- Message Text   :- Do you want to add in the packing list?
		*- Message No.    :- 44073.
		*- Buttom Message :- Ok
		*- Buttom Number  :-00000
		IF gfModalGen('QRM44073B44009','DIALOG',StyleUpc.Style+'\'+EVAL('SCALE.SZ'+ALLTRIM(StyleUpc.Size)) ) = 2
			lcUpc = SPACE(12)
			_CUROBJ = OBJNUM(lcUpc)
			SELECT (lcTmpPkLin)
			RETURN
		ENDIF
		SELECT OrdLine
		=SEEK('O'+lcOrderNo+lcStore+StyleUpc.Style)
		LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
			'O'+lcOrderNo+lcStore+StyleUpc.Style;
			FOR PikTkt = lcPackNo
	ENDIF
	SELECT (lcTmpPkLin)
	= lfAddRec ()
	= lfClearFld()
	SHOW GET lcUpc ENABLE
	lcUpc = SPACE(12)
	_CUROBJ = OBJNUM(lcUpc)
ELSE
	= lfClearFld()
	IF LASTKEY() # 27
		SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
		=lfRefresh(lcWinCh5)
		= lfBrows()
	ENDIF
	IF llAutoPack
		SHOW GET pbNewCart ENABLE
	ENDIF

	SHOW GET pbAutoPak ENABLE
	SHOW GET pbNewUpc  ENABLE
	SHOW GET pbPrntLab ENABLE
ENDIF
SET ORDER TO TAG STYLEUPC IN STYLEUPC
SELECT (lnAlias)

*B128758,1 MMR 07/12/2005 Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen)[Start]
*--Restore confirmation setting.
SET CONFIRM &lcSetConf
*B128758,1 MMR[End]

*-- End Of lfvUpc.
*:*************************************************************
*: Name      : lfvNewUpc
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to add new upc to a carton.
*:*************************************************************
*: Calls     : lfClearFld.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvNewUpc ()
*:*************************************************************
*
FUNCTION lfvNewUpc

= lfClearFld()
IF LASTKEY() # 27
	SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
	=lfRefresh(lcWinCh5)
ENDIF
SHOW GET lcUpc     ENABLE

IF llAccOne
	SHOW GET pbNewCart DISABLE
ELSE
	SHOW GET pbNewCart ENABLE
ENDIF
SHOW GET pbAutoPak ENABLE
SHOW GET pbNewUpc  ENABLE
SHOW GET pbPrntLab ENABLE
SET CONFIRM ON
_CUROBJ = OBJNUM(lcUpc)

*-- End Of lfvNewUpc.
*:*************************************************************
*: Name      : lfvReScn
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function of Rescan/Autopack Button.
*:*************************************************************
*: Calls     : lfClearFld.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvReScn ()
*:*************************************************************
*
FUNCTION lfvReScn
CLEAR READ

STORE 0 TO lnTotCart , lnTotPiece , lnTotWg , lnShipVia , lnToStorCn,;
	lnQty1 ,lnQtyWg , lnCartonNo , lnLastCart , laCartons ,lnScnToQty


STORE .T. TO llAutoPack , llCUpdate
laScrMode = .F.
laScrMode [4] = .T.

*-- End OF lfvReScn.
*:*************************************************************
*: Name      : lfvAutPck
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to Show the orders in the browse.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvAutPck()
*:*************************************************************
*
FUNCTION lfvAutPck
PRIVATE lnAlias , lcOldOrder , lcOldOrdr
STORE '' TO lcOldOrder ,lcOldOrdr

lnAlias = SELECT (0)
*-- This file will hold the selected users.
SELECT (lcTmpBrow)
ZAP
*E301784,1 ABD - Add new Field "Scaned Qty" to the Browse. [Begin]
IF llHangFlat
	lcBrowFlds = [Style :H= 'Style':19,cSize :H='Size':5,TotQty :H= 'Order Qty':7,TotPik :H= 'Pick Qty':6,;
		nScn_Qty :H= 'Scanned Qty':15,nRemQty:H= 'Remaining Scanned Qty':20]
ELSE
	lcBrowFlds = [Style :H= 'Style':19,cSize :H='Size':5,TotQty :H= 'Order Qty':7,TotPik :H= 'Pick Qty':6,;
		nRemQty:H= 'Remaining Scanned Qty':20]
ENDIF
*E301784,1 ABD - [End]

SELECT (lcTmpData)
GO TOP

lnRecNo = RECNO()
SUM nRemQty TO lnRemQty
IF BETWEEN(lnRecNo,1,RECCOUNT())
	GO lnRecNo
ENDIF


IF EOF() .OR. lnRemQty = 0
	=gfModalGen('INM00000B00000',.F.,.F.,.F.,'No records to browse.')
ELSE
	SELECT (lcTmpPklin)
	lcOldOrdr = ORDER()
	SET ORDER TO (lcTmpPklin)
	SELECT (lcTmpData)
	SET ORDER TO (lcTmpData)
	PUSH KEY
	IF gfrange(lcBrowFlds,lcTmpBrow,"UPC"," FOR nRemQty > 0 ","","","@! XXXXXXXXXXXXXXXXXXX")
		SELECT (lcTmpPklin)
		SET ORDER TO &lcOldOrdr
		SELECT (lcTmpData)
		lcOldOrder = ORDER()
		SET ORDER TO &lcTmpData1
		SELECT (lcTmpBrow)
		LOCATE
		SCAN WHILE !EOF()
			IF SEEK(&lcTmpBrow..Upc,lcTmpData) .AND. (&lcTmpData..nRemQty = 0)
				SELECT (lcTmpBrow)
				LOOP
			ENDIF
			SELECT (lcTmpBrow)
			= SEEK(&lcTmpBrow..Upc,lcTmpData)
			lnScnToQty = &lcTmpData..nRemQty
			lcUpc      = UPC
			= lfvUPC ()
		ENDSCAN
	ENDIF
	POP KEY
	= lfBrows()
	SELECT (lcTmpPklin)
	SET ORDER TO &lcOldOrdr

	SELECT (lcTmpData)
	SET ORDER TO &lcOldOrder
	SET RELATION TO

	SELECT (lcTmpPklin)
	= lfvNewupc ()
ENDIF
SELECT (lnAlias)
*-- End Of lfvAutPck.
*:*************************************************************
*: Name      : lpSavScr
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Save new or modified order
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lpSavScr()
*:*************************************************************
*:
FUNCTION lpSavScr

*- Message Text   :- Actual cartons number is not equal to the tape cartons number.
*- Message No.    :- 44070.
*- Buttom Message :- \<Proceed;\<Resume
*- Buttom Number  :- 44010

IF lnTotCart <> lnLastCart .AND. gfModalGen("QRM44070B44010","Dialog")= 2
	RETURN
ENDIF

*B123624,1  TMI [Start] Do not save if no lines entered in cartons temp file
PRIVATE lnRecCnt
SELECT &lcTmpPkLin
GO TOP
lnRecCnt = 0
COUNT TO lnRecCnt
IF lnRecCnt = 0
	=gfModalGen('INM44010B00000','DIALOG','Cartons')
	llCSave = .F.
	RETURN
ENDIF
*B123624,1  TMI [End  ]

llDelPack = .F.
SET ORDER TO TAG ORDLINE   IN ORDLINE
SET ORDER TO TAG ORDLINENO IN (lcTmpPkLin)
IF SEEK('O'+lcOrderNo,'ORDLINE')
	SELECT ORDLINE
	LOCATE REST WHILE cOrdType+ORDER+STR(LINENO,6) = 'O'+lcOrderNo ;
		FOR PICKED .AND. PIKTKT=lcPackNo .AND. !lfCheckLns ()
	IF FOUND()
		*- Message Text   :- Some of the lines in the pick ticket are
		*- Message Text   :- not packed. Do you want to release them?
		*- Message No.    :- 44071.
		*- Buttom Message :- \!\<Release;\<Resume Packing;\<Save as Is
		*- Buttom Number  :- 44006

		lnChoise= gfModalGen("QRM44071B44006","Dialog")
		DO CASE
		CASE lnChoise = 2
			llCSave = .F.
			RETURN
		CASE lnChoise = 1
			SCAN REST WHILE cOrdType+ORDER+STR(LINENO,6) = 'O'+lcOrderNo ;
					FOR PICKED .AND. PIKTKT=lcPackNo .AND. !SEEK(STR(LINENO,6),lcTmpPkLin)
				*-- 1) Updating the style file.
				IF SEEK(STYLE,'Style')
					SELECT STYLE
					=RLOCK()
					REPLACE Alo1   WITH Alo1   - OrdLine.Pik1 ,;
						Alo2   WITH Alo2   - OrdLine.Pik2 ,;
						Alo3   WITH Alo3   - OrdLine.Pik3 ,;
						Alo4   WITH Alo4   - OrdLine.Pik4 ,;
						Alo5   WITH Alo5   - OrdLine.Pik5 ,;
						Alo6   WITH Alo6   - OrdLine.Pik6 ,;
						Alo7   WITH Alo7   - OrdLine.Pik7 ,;
						Alo8   WITH Alo8   - OrdLine.Pik8 ,;
						TotAlo WITH TotAlo - OrdLine.TotPik
					UNLOCK
				ENDIF
				*-- 2) Updating the stydye file.
				IF llMulWare .AND. ;
						SEEK (OrdLine.Style+lcWarCode+SPACE(10),'StyDye')
					SELECT StyDye
					=RLOCK()
					REPLACE Alo1   WITH Alo1   - OrdLine.Pik1 ,;
						Alo2   WITH Alo2   - OrdLine.Pik2 ,;
						Alo3   WITH Alo3   - OrdLine.Pik3 ,;
						Alo4   WITH Alo4   - OrdLine.Pik4 ,;
						Alo5   WITH Alo5   - OrdLine.Pik5 ,;
						Alo6   WITH Alo6   - OrdLine.Pik6 ,;
						Alo7   WITH Alo7   - OrdLine.Pik7 ,;
						Alo8   WITH Alo8   - OrdLine.Pik8 ,;
						TotAlo WITH TotAlo - OrdLine.TotPik
					UNLOCK
				ENDIF
				*-- 3) Updating the Ordline file.
				SELECT OrdLine
				=RLOCK()
				REPLACE PIK1   WITH 0 ,;
					PIK2   WITH 0 ,;
					PIK3   WITH 0 ,;
					PIK4   WITH 0 ,;
					PIK5   WITH 0 ,;
					PIK6   WITH 0 ,;
					PIK7   WITH 0 ,;
					PIK8   WITH 0 ,;
					TOTPIK WITH 0 ,;
					Picked WITH .F.,;
					PikTkt WITH SPACE(6)
				UNLOCK
			ENDSCAN
			SELECT ORDLINE
			=SEEK('O'+lcOrderNo,'ORDLINE')
			LOCATE REST WHILE cOrdType+ORDER+STR(LINENO,6) = 'O'+lcOrderNo;
				FOR PICKED .AND. PIKTKT=lcPackNo
			IF !FOUND() .AND. SEEK(lcPackNo,'PIKTKT')
				*-- Release the Pick ticket
				SELECT PIKTKT
				DELETE
				llDelPack = .T.
			ENDIF
		ENDCASE
	ENDIF
ENDIF


lnTotCart = 0
lnLineNo = IIF(SEEK(lcPackNo,'PACK_HDR'),PACK_HDR.nLastLNo,0)
SELECT (lcTmpPkLin)
SET DELETE OFF
GO TOP
DO WHILE !EOF()
	lcOrdLNo = STR(OrdLineNo,6)
	=SEEK(STYLE,'Style')
	IF llMulWare
		=SEEK(STYLE+lcWarCode,'StyDye')
	ENDIF
	STORE 0 TO lnPack1,lnPack2,lnPack3,lnPack4,lnPack5,lnPack6,lnPack7,lnPack8
	SCAN REST WHILE STR(OrdLineNo,6) = lcOrdLNo
		lcStySize = SIZE
		lnPack&lcStySize = lnPack&lcStySize + IIF(DELETED(),0,TotQty)
		DO CASE
		CASE !DELETED(lcTmpPkLin) .AND. OldCrtNo = 0
			SELECT PACK_LIN

			=SEEK(lcPackNo+STR(&lcTmpPkLin..CartonNo,4)+&lcTmpPkLin..Style)

			LOCATE REST WHILE Pack_no+STR(no_cart,4)+STYLE = ;
				lcPackNo+STR(&lcTmpPkLin..CartonNo,4)+&lcTmpPkLin..Style ;
				FOR nORDLINENO=&lcTmpPkLin..ORDLINENO

			IF !FOUND()
				lnLineNo = lnLineNo + 1
				INSERT INTO 'PACK_LIN' ;
					(PACK_NO,Line_No,STYLE,No_cart,From_Crt,To_Crt,nOrdLineNo) VALUES ;
					(lcPackNo,lnLineNo,&lcTmpPkLin..Style,&lcTmpPkLin..CartonNo,&lcTmpPkLin..CartonNo,;
					&lcTmpPkLin..CartonNo,&lcTmpPkLin..OrdLineNo)
				=gfAdd_Info('Pack_Lin')
			ENDIF
			REPLACE Qty&lcStySize WITH Qty&lcStySize + &lcTmpPkLin..TotQty ,;
				TotQty        WITH TotQty        + &lcTmpPkLin..TotQty ,;
				Weight        WITH TotQty * Style.nStyWeight

			*E302144,1 AMH Change the custom C102789 to be standard [Start]
			*C102789,4 ASH 02/16/2003 (Begin) Save carton type in pack_lin file.
			*IF ASCAN(laEvntTrig , PADR('SAVLN',10)) <> 0
			*  =gfDoTriger('ALPKSCN',PADR('SAVLN',10))
			*ENDIF
			*C102789,4 ASH 02/16/2003 (End)
			REPLACE CCRTNVLTYP WITH IIF(EMPTY(&lcTmpPkLin..CCRTNVLTYP),lcRPCart,&lcTmpPkLin..CCRTNVLTYP)
			*E302144,1 AMH [End]


		CASE !DELETED(lcTmpPkLin) .AND. OldCrtNo <> 0
			SELECT PACK_LIN

			=SEEK(lcPackNo+STR(&lcTmpPkLin..OldCrtNo,4)+&lcTmpPkLin..Style)

			LOCATE REST WHILE Pack_no+STR(no_cart,4)+STYLE =        ;
				lcPackNo+STR(&lcTmpPkLin..OldCrtNo,4)+&lcTmpPkLin..Style;
				FOR nORDLINENO=&lcTmpPkLin..ORDLINENO

			IF FOUND()
				REPLACE From_Crt      WITH &lcTmpPkLin..CartonNo ,;
					To_Crt        WITH &lcTmpPkLin..CartonNo ,;
					Qty&lcStySize WITH &lcTmpPkLin..TotQty   ,;
					TotQty        WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
					Weight        WITH TotQty*Style.nStyWeight,;
					No_Cart       WITH &lcTmpPkLin..CartonNo

			ENDIF


			*E302144,1 AMH Change the custom C102789 to be standard [Start]
			*C102789,4 ASH 02/16/2003 (Begin) Save carton type in pack_lin file.
			*IF ASCAN(laEvntTrig , PADR('SAVLN',10)) <> 0
			*  =gfDoTriger('ALPKSCN',PADR('SAVLN',10))
			*ENDIF
			*C102789,4 ASH 02/16/2003 (End)
			REPLACE CCRTNVLTYP WITH IIF(EMPTY(&lcTmpPkLin..CCRTNVLTYP),lcRPCart,&lcTmpPkLin..CCRTNVLTYP)
			*E302144,1 AMH [End]

		CASE DELETED(lcTmpPkLin) .AND. OldCrtNo <> 0
			SELECT PACK_LIN
			=SEEK(lcPackNo+STR(&lcTmpPkLin..OldCrtNo,4)+&lcTmpPkLin..Style)

			LOCATE REST WHILE Pack_no+STR(no_cart,4)+STYLE =        ;
				lcPackNo+STR(&lcTmpPkLin..OldCrtNo,4)+&lcTmpPkLin..Style;
				FOR nORDLINENO=&lcTmpPkLin..ORDLINENO

			IF FOUND()
				REPLACE Qty&lcStySize WITH 0 ,;
					TotQty        WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
					Weight        WITH TotQty*Style.nStyWeight
			ENDIF
			IF TotQty = 0
				BLANK
				DELETE
			ENDIF
		ENDCASE

		lnTotCart = MAX(lnTotCart,&lcTmpPkLin..CartonNo)
	ENDSCAN
	=SEEK('O'+lcOrderNo+lcOrdLNo,'OrdLine')
	*-- Update the Alo Qty in the style File.
	SELECT STYLE
	=RLOCK()
	REPLACE ALO1   WITH ALO1 - OrdLine.Pik1 + lnPack1 ,;
		ALO2   WITH ALO2 - OrdLine.Pik2 + lnPack2 ,;
		ALO3   WITH ALO3 - OrdLine.Pik3 + lnPack3 ,;
		ALO4   WITH ALO4 - OrdLine.Pik4 + lnPack4 ,;
		ALO5   WITH ALO5 - OrdLine.Pik5 + lnPack5 ,;
		ALO6   WITH ALO6 - OrdLine.Pik6 + lnPack6 ,;
		ALO7   WITH ALO7 - OrdLine.Pik7 + lnPack7 ,;
		ALO8   WITH ALO8 - OrdLine.Pik8 + lnPack8 ,;
		TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
	UNLOCK
	IF llMulWare
		*-- Update the Alo Qty in the STYDYE File.
		SELECT StyDye
		=RLOCK()
		REPLACE ALO1   WITH ALO1 - OrdLine.Pik1 + lnPack1 ,;
			ALO2   WITH ALO2 - OrdLine.Pik2 + lnPack2 ,;
			ALO3   WITH ALO3 - OrdLine.Pik3 + lnPack3 ,;
			ALO4   WITH ALO4 - OrdLine.Pik4 + lnPack4 ,;
			ALO5   WITH ALO5 - OrdLine.Pik5 + lnPack5 ,;
			ALO6   WITH ALO6 - OrdLine.Pik6 + lnPack6 ,;
			ALO7   WITH ALO7 - OrdLine.Pik7 + lnPack7 ,;
			ALO8   WITH ALO8 - OrdLine.Pik8 + lnPack8 ,;
			TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
		UNLOCK
	ENDIF
	SELECT OrdLine
	=RLOCK()
	REPLACE Pik1   WITH IIF(lnPack1 = 0,Pik1,lnPack1),;
		Pik2   WITH IIF(lnPack2 = 0,Pik2,lnPack2),;
		Pik3   WITH IIF(lnPack3 = 0,Pik3,lnPack3),;
		Pik4   WITH IIF(lnPack4 = 0,Pik4,lnPack4),;
		Pik5   WITH IIF(lnPack5 = 0,Pik5,lnPack5),;
		Pik6   WITH IIF(lnPack6 = 0,Pik6,lnPack6),;
		Pik7   WITH IIF(lnPack7 = 0,Pik7,lnPack7),;
		Pik8   WITH IIF(lnPack8 = 0,Pik8,lnPack8),;
		TotPik WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8 ,;
		Picked WITH TotPik>0 ,;
		PikTkt WITH IIF(TotPik>0 ,PikTKt,'')
	*-- Update the Npck Fields.
	REPLACE npck1  WITH lnPack1 ,;
		npck2  WITH lnPack2 ,;
		npck3  WITH lnPack3 ,;
		npck4  WITH lnPack4 ,;
		npck5  WITH lnPack5 ,;
		npck6  WITH lnPack6 ,;
		npck7  WITH lnPack7 ,;
		npck8  WITH lnPack8 ,;
		npwght WITH ((npck1+npck2+npck3+npck4+npck5+npck6+npck7+npck8) * Style.nStyWeight)

	UNLOCK
	SELECT (lcTmpPkLin)
ENDDO
*C125077,1 AEH 06/02/2005 Add Carton Number to File [Start]
IF ASCAN(laEvntTrig , PADR('ADDCRTNO',10)) <> 0
	=gfDoTriger('ALPKSCN',PADR('ADDCRTNO',10))
ENDIF
*C125077,1 AEH 06/02/2005 Add Carton Number to File [END]
SET DELETE ON

*-- Check for Edi Module.
IF llEDIInstl

  *B039957,1 HBG 11/15/2005 Update the new file EDICRTSQ , and the new field Ucc9 in EDIACPRT [Begin]
  *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
  IF !llIncPLNum
  *E040123,1 HBG 03/13/2006 [End]
    lcSetDele = SET('DELETE')
    SET DELETE ON
    SET ORDER TO PCKCRTSQ IN EDICRTSQ
    IF SEEK(lcPackNo,'EDICRTSQ')
      SELECT EDICRTSQ      
      DELETE FOR pack_no+STR(cart_no,6) = lcPackNo
    ENDIF  
    SET DELETE &lcSetDele 

    SELECT Pack_Lin
    =SEEK(lcPackNo)
    DIMENSION laMaxAcc[1]
    SCAN REST WHILE Pack_No+STR(No_Cart,4)+Style = lcPackNo
      SET ORDER TO PCKCRTSQ IN EDICRTSQ
      IF !SEEK(lcPackNo+STR(Pack_Lin.No_Cart,6),'EDICRTSQ')
        *-- If this customer is a partner , get the last Ucc # from EDIACPRT file,
        IF SEEK('A'+lcAccount,'EDIACPRT')
  	      lnUcc9  = IIF(EMPTY(EDIACPRT.Ucc9),0,EVAL(EDIACPRT.Ucc9)) + 1
  	      SET ORDER TO EDICRTSQ IN EDICRTSQ
  	      llFound = SEEK(lcAccount+PADL(lnUcc9,9,'0'),'EDICRTSQ')
          DO WHILE llFound 
	  	    lnUcc9  = lnUcc9  + 1
            llFound = SEEK(lcAccount+PADL(lnUcc9,9,'0'),'EDICRTSQ')
		  ENDDO
          INSERT INTO EDICRTSQ (Pack_No,Account,cart_no,Ucc9);
          	         VALUE (lcPackNo,lcAccount,Pack_Lin.No_Cart,PADL(lnUcc9,9,'0'))
          REPLACE EDIACPRT.Ucc9 WITH PADL(lnUcc9,9,'0')
          	         
   	    ELSE    && Get the last Ucc # from EDICRTSQ file.
          laMaxAcc[1] = .F.
          SELECT MAX(EDICRTSQ.Ucc9) FROM EDICRTSQ WHERE EDICRTSQ.Account = lcAccount INTO ARRAY laMaxAcc
          IF TYPE('laMaxAcc[1]') <> 'C'
		    lcUcc9Val = '000000001'
          ELSE
	  	    lcUcc9Val = PADL(EVAL(laMaxAcc[1])+1,9,'0')
          ENDIF  
	  	  INSERT INTO EDICRTSQ (Pack_No,Account,cart_no,Ucc9);
                       VALUE (lcPackNo,lcAccount,Pack_Lin.No_Cart,lcUcc9Val)
	    ENDIF  

        *--mhm2006
        SELECT (tmpAsnShip)
        IF SEEK(lcPackNo+STR(Pack_Lin.No_Cart,6)) AND EMPTY(ucc9)
          REPLACE ucc9 WITH IIF(TYPE("lcUcc9Val")='C',lcUcc9Val,PADL(lnUcc9,9,'0'))
        ENDIF
        *--mhm2006
      ENDIF  
    ENDSCAN
    *B039957,1 [End]
  *E040123,1 HBG 03/13/2006 Add new setup of including P\L number in UCC [Begin]
  ENDIF
  *E040123,1 HBG 03/13/2006 [End]

	*B605884,1 ABD - Don't add Shipment in case we havn't BOL #.  [Begin]
	*IF llEDIInstl .AND. SEEK ('A'+lcAccount,'EdiAcPrt') .AND. SEEK(EdiAcPrt.cPartCode+'856','EDIPD')
	IF llEDIInstl .AND. SEEK ('A'+lcAccount,'EdiAcPrt') .AND. SEEK(EdiAcPrt.cPartCode+'856','EDIPD') AND !EMPTY(lcBol)
		*B605884,1 ABD - [End]
		SELECT BOL_HDR
		*B605884,1 ABD - Go to Correct Record. [Begin]
		*-- I will Update the BOL_Lin Here Not When the Program Create the BOL#.
		IF !EMPTY(lcBol) .AND. !SEEK(lcBol+lcOrderNo+lcPackNo,'BOL_LIN')
			INSERT INTO BOL_LIN (BOL_NO,ORDER,PACK_NO,COWNER) VALUES (lcBol,lcOrderNo,lcPackNo,'N')
		ENDIF
		= SEEK(lcBol)
		*B605884,1 ABD - [End]
		IF !SEEK(lcPackNo,'Pack_Hdr')
			REPLACE Tot_Wght WITH Tot_Wght + lnTotWg   ,;
				Tot_Cart WITH Tot_Cart + lnTotCart ,;
				Tot_Pcs  WITH Tot_Pcs  + lnTotPiece
		ELSE
			REPLACE Tot_Wght WITH Tot_Wght - Pack_Hdr.Tot_Wght + lnTotWg   ,;
				Tot_Cart WITH Tot_Cart - Pack_Hdr.Tot_Cart + lnTotCart ,;
				Tot_Pcs  WITH Tot_Pcs  - Pack_Hdr.Tot_Pcs  + lnTotPiece
		ENDIF
	ENDIF

	SELECT Pack_Hdr
	IF llDelPack
		IF SEEK(lcPackNo)
			DELETE
		ENDIF
		IF SEEK(lcBol+lcOrderNo+lcPackNo,'BOL_LIN')
			SELECT BOL_LIN
			DELETE
		ENDIF
	ELSE
		IF !SEEK(lcPackNo)
			APPEND BLANK
		ENDIF
		REPLACE Pack_No   WITH lcPackNo   ,;
			ORDER     WITH lcOrderNo  ,;
			Account   WITH lcAccount  ,;
			cWareCode WITH lcWarCode  ,;
			STORE     WITH lcStore    ,;
			Bill_ladg WITH lcBol      ,;
			Tot_Wght  WITH lnTotWg    ,;
			Tot_Pcs   WITH lnTotPiece ,;
			Tot_Cart  WITH lnTotCart  ,;
			Note      WITH lcNotes    ,;
			ShipVia   WITH lcShipVia  ,;
			cPkChCode WITH lcPackChar ,;
			cPkDsCode WITH lcPackDesc ,;
			cToStorCn WITH IIF(lnToStorCn=0,'S','C') ,;
			Piktkt    WITH lcPackNo   ,;
			Sp_Inst1  WITH lcSp_Inst1 ,;
			Sp_Inst2  WITH lcSp_Inst2 ,;
			nLastLNo  WITH lnLineNo   ,;
			nLastCart WITH lnLastCart

	ENDIF
	*B126515,1 ASH 02/13/2005 (Begin) Fix bug of not saving the audit fields in the pack_hdr.
	=gfAdd_Info()
	*B126515,1 ASH 02/13/2005 (End)
	*E302144,1 AMH Change the custom C102789 to be standard [Start]
	*C102789,4 ASH 02/16/2003 (Begin) Save carton type in pack_hdr file.
	*IF ASCAN(laEvntTrig , PADR('SAVHDR',10)) <> 0
	*  =gfDoTriger('ALPKSCN',PADR('SAVHDR',10))
	*ENDIF
	*C102789,4 ASH 02/16/2003 (End)
	REPLACE CCRTNVLTYP WITH lcRPCart
	*E302144,1 AMH [End]

	*aeh
	IF ASCAN(laEvntTrig , PADR('ADDCRTNO',10)) <> 0
		=gfDoTriger('ALPKSCN',PADR('ADDCRTNO',10))
	ENDIF
	*aeh
	IF llEDIInstl
		SELECT (TmpAsnShip)
		SCAN
			SCATTER MEMVAR MEMO
			SELECT ASN_SHIP
			*-- Store Pack_No in the ASN_SHIP file.
			IF !SEEK(&tmpAsnShip..Bol_No+&tmpAsnShip..PACK_NO+STR(&tmpAsnShip..Cart_No,6))
				APPEND BLANK
			ENDIF
			GATHER MEMVAR MEMO
		ENDSCAN
		ZAP
	ENDIF
	SET ORDER TO TAG ORDLINST   IN ORDLINE
	SET ORDER TO TAG (lcTmpPkLin) IN (lcTmpPkLin)
ENDIF
*B605884,1 ABD - [End]
*E301784,1 ABD - Update the status for Piktkt field with 'K' in case we run this
*E301784,1 ABD - Program at our customer Eilein Fisher . [Begin]
*--  If a user select the button of accept one carton Change the status to 'K' in PikTkt file[Begin].
IF llAccOne .AND. llHangFlat
	lcCurAls = SELECT(0)
	SELECT PikTkt
	REPLACE STATUS WITH 'K'
	SELECT (lcCurAls)
ENDIF
*E301784,1 ABD - [End]


SELECT PIKTKT

RETURN

*-- End OF lpSavScr.
*:*************************************************************
*: Name      : lfGetUpc
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Function to add upc records in the lcTmpPkLin file
*:             in order to use it as a brows in the second folder
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfGetUpc()
*:*************************************************************
*:
FUNCTION lfGetUpc
PRIVATE lnAlias , lcOldOrder

lnAlias = SELECT(0)

SELECT (lcTmpData)
lcOldOrder = ORDER ()
SET ORDER TO (lcTmpData1)

SELECT (lcTmpPkLin)
ZAP
STORE 0 TO lnLastCart,lnCartonNo
IF SEEK (lcPackNo,'PACK_LIN')
	SELECT Pack_Lin
	SCAN REST WHILE Pack_No = lcPackNo
		FOR lnCount = 1 TO 8
			lcCount = STR(lnCount,1)
			IF Pack_Lin.Qty&lcCount <> 0 .AND. ;
					SEEK(Pack_Lin.Style+lcCount,'StyleUpc') AND SEEK(Pack_Lin.Style,'Style')
				SELECT (lcTmpPkLin)
				FOR lnCount2 = Pack_Lin.No_Cart TO Pack_Lin.No_Cart
					IF !SEEK(STR(lnCount2,4))
						lnLastCart = lnLastCart + 1
						DIMENSION laCartons[lnLastCart,2]
						STORE 0 TO laCartons[lnLastCart,1],laCartons[lnLastCart,2]
						lnCartonNo = lnLastCart
					ELSE
						lnCartonNo = lnCount2
					ENDIF
					APPEND BLANK
					REPLACE cUpc      WITH StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,;
						STYLE     WITH Pack_Lin.Style                           ,;
						cSize     WITH Scale.Sz&lcCount                         ,;
						SIZE      WITH lcCount                                  ,;
						Weight    WITH Pack_Lin.Qty&lcCount*Style.nStyWeight    ,;
						CartonNo  WITH lnCount2   							  ,;
						OldCrtNo  WITH lnCount2   							  ,;
						TotQty    WITH Pack_Lin.Qty&lcCount 					  ,;
						OrdLineNo WITH Pack_Lin.nOrdLineNo
					laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] + TotQty
					laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] + Weight

					*E302144,1 AMH Change the custom C102789 to be standard [Start]
					*C102789,4 ASH 02/16/2003 (Begin) Get carton type from pack_lin file.
					*IF ASCAN(laEvntTrig , PADR('GETDTLN',10)) <> 0
					*  =gfDoTriger('ALPKSCN',PADR('GETDTLN',10))
					*ENDIF
					*C102789,4 ASH 02/16/2003 (End)
					IF !EOF(lcTmpPkLin)
						REPLACE &lcTmpPkLin..CCRTNVLTYP WITH Pack_Lin.CCRTNVLTYP
					ENDIF
					*E302144,1 AMH [End]

					SELECT (lcTmpData)

					IF SEEK(&lcTmpPkLin..Cupc)
						REPLACE nRemQty WITH nRemQty - &lcTmpPkLin..TotQty
					ENDIF
					SELECT (lcTmpPkLin)

				ENDFOR
			ENDIF
		ENDFOR
	ENDSCAN
	GO TOP IN (lcTmpPkLin)
	lnCartonNo = IIF(lnLastCart>0,1,0)
ENDIF


SELECT (lcTmpData)
SET ORDER TO &lcOldOrder

SELECT (lnAlias)

*-- End Of lfGetUpc
*:*************************************************************
*: Name      : lfvPackInfo
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Get Pack informations
*:*************************************************************
*: Calls     : AriaBrow,lpShow
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfvPackInfo()
*:*************************************************************
*:
FUNCTION lfvPackInfo
PRIVATE laAddress
DIMENSION laAddress[6,3]

llReBrow = .T.
SELECT PACK_HDR
IF !SEEK(lcPackNo)
	= lfvPikTkt ()
	RETURN .F.
ENDIF
=SEEK('O'+ORDER,'OrdHdr')
=SEEK(lcPackNo,'PikTKt')
lcOrderNo = ORDER
lcAccount = Account
lcStore   = STORE
lcCustPo  = PikTkt.CustPo
lcDept    = OrdHdr.Dept

*-- Pick up Order Division long
SET ORDER TO TAG CCODE_NO IN CODES
=SEEK('N'+PADR('CDIVISION',10)+ORDHDR.CDIVISION+SPACE(30)+'DIVLNAME','CODES')
lcDivLName = CODES.cRltd_Vlu
SET ORDER TO TAG CODES IN CODES

lcNotes   = NOTE
lcSp_Inst1= Sp_Inst1
lcSp_Inst2= Sp_Inst2
lcWarCode = cWareCode
lcShipVia = ShipVia
=gfwCodePop(@laCodInfo, "SHIPVIA", "V,"+lcShipVia)

IF llEDIInstl
	llEdiFound = SEEK ('A'+lcAccount,'EdiAcPrt')
	llEdiExist = SEEK (EdiAcPrt.cPartCode,'EdiPh')
	llPkChrDes = llEdiFound AND llEdiExist AND EdiAcPrt.lPkChrDes
	llToStOrCn = llEdiFound AND llEdiExist AND EdiAcPrt.cMulUccLbl='Y'
	lcPkChTxt  = IIF(llPkChrDes,'Pack Characteristic:','')
	lcPkDsTxt  = IIF(llPkChrDes,'Pack Description   :','')
	lcPackChar = cpkChCode
	lcPackDesc = cpkDsCode
	lnToStorCn = IIF(cToStorCn='C',1,0)

	*E302210,1 WLD 09/09/2004 Using visual label report
	llDetLabel = .F.
	lcDetailVr =  ''
	IF Ediph.lDtlbl
		llDetLabel = .T.
		lcDetailVr = eDiph.cDtlbl
	ENDIF
	*E302210,1 WLD (End)
ENDIF
lcBol      = Bill_Ladg
lnTotPiece = Tot_Pcs
lnTotWg    = Tot_Wght
lnTotCart  = Tot_Cart


*-- Labels Informations.
=SEEK('O'+lcOrderNo,'ORDHDR')
lcEvenCode = OrdHdr.Event_Cod
lcVendNo  = Customer.cCusVend

*E302210,1 WLD 09/09/2004 Using visual label report
ldOrdCanc  = OrdHdr.Complete
lcOrdNote1 = OrdHdr.NOTE1
lcOrdNote2 = OrdHdr.NOTE2
*E302210,1 WLD (End)

IF llMulWare .AND. SEEK(lcWarCode,'WareHous')
	lcWrName  = WareHous.cDesc
	lcWrAddr1 = WareHous.cAddress1
	lcWrAddr2 = WareHous.cAddress2
	lcWrCity  = ALLTRIM(WareHous.cAddress3)
	lcWrState = ALLTRIM(WareHous.cAddress4)
	lcWrZip   = ALLTRIM(WareHous.cAddress5)
ENDIF

=SEEK(IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore),'Customer')
lcDistCtr = Customer.Dist_Ctr
lcStName  = Customer.StName

*-- ShipTo address.

=gfGetAdr('CUSTOMER','','','',1,'')
lcStAddr1 = laAddress[1,2]
lcStAddr2 = laAddress[2,2]
lcStCity  = laAddress[3,2]
lcStState = laAddress[4,2]
lcStZip   = laAddress[5,2]

=SEEK('S'+lcAccount+lcDistCtr,'Customer')
lcDCName  = Customer.StName
*-- ShipTo address
=gfGetAdr('CUSTOMER','','','',1,'')
lcDCAddr1 = laAddress[1,2]
lcDCAddr2 = laAddress[2,2]
lcDCCity  = laAddress[3,2]
lcDCState = laAddress[4,2]
lcDCZip   = laAddress[5,2]

*-- End OF lfvPackInfo.
*:*************************************************************
*: Name      : lfReadAct
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : READ Activate function of SoOrd
*:*************************************************************
*: Calls     : lfClearKey.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfReadAct()
*:*************************************************************
FUNCTION lfReadAct

IF glFromBrow
	=gfStopBrow()
	glFromBrow = .F.
ENDIF

*-- End Of lfReadAct.
*:*************************************************************
*: Name      : lfRdDctOP
*: Developer : Abdou ElGendi
*: Date      : 11/19/2001
*: Purpose   : READ Deactivate function of main screen.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfRdDctOP()
*:*************************************************************
*:
FUNCTION lfRdDctOP
*-- Key traps for the browse
IF WONTOP() = lcBrTit
	ON KEY LABEL TAB        llDummy = lfTrapKey(lcWinCh5, OBJNUM(lcUpc))
	ON KEY LABEL ESC        DO lfEsc
	ON KEY LABEL CTRL+Q     lnDummy = 1
	ON KEY LABEL CTRL+W     lnDummy = 1
	ON KEY LABEL CTRL+HOME  GO TOP
	ON KEY LABEL CTRL+END   GO BOTTOM
	glFromBrow = .T.
ELSE
	glFromBrow = .F.
ENDIF

RETURN .F.
*-- END Of lfRdDctOP.
*:*************************************************************
*: Name      : lfTrapKey
*: Developer : Abdou ElGendi
*: Date      : 11/19/2001
*: Purpose   : function to set the focus on a certain object.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfTrapKey()
*:*************************************************************
*:
FUNCTION lfTrapKey
PARAMETERS lcWindName, lnObjNum

ON KEY LABEL TAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum

*-- End Of lfTrapKey.
*:*************************************************************
*: Name      : lfEsc
*: Developer : Abdou ElGendi
*: Date      : 11/19/2001
*: Purpose   : Function to exit the screen.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfEsc()
*:*************************************************************
*:
FUNCTION lfEsc

ACTIVATE WINDOW (lcWinCh5)
_CUROBJ = OBJNUM(pbCls)
KEYBOARD CHR(13) CLEAR

*-- End OF lfEsc.
*:*************************************************************
*: Name      : lfReUsefls
*: Developer : Abdou ElGendi
*: Date      : 11/19/2001
*: Purpose   : Function to delete data from used file.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfReUsefls()
*:*************************************************************
*:
FUNCTION lfReUsefls
PRIVATE lnAlias

lnAlias = SELECT (0)

SELECT (lcTmpBrow)
DELE ALL

SELECT (lcTmpData)
DELE ALL


SELECT (lcTmpPklin)
DELE ALL

SELECT (lnAlias)
*-- End OF lfReUsefls.
*:*************************************************************
*: Name      : lfGetScl
*: Developer : Abdou ElGendi
*: Date      : 12/02/2001
*: Purpose   : Function to get the size from scale file.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfGetScl()
*:*************************************************************
*:E301784,1
FUNCTION lfGetScl
PARAMETER lcCurrSty , lcScl

PRIVATE lnOldAls
lcInd = ""
lcToRet = ""
lnOldAls = SELECT(0)
IF SEEK(lcCurrSty,"Style")
	FOR lnInd = 1 TO 8
		lcInd = STR(lnInd,1)
		IF ALLTRIM(Scale.Sz&lcInd) == ALLTRIM(lcScl)
			lcToRet = lcInd
			EXIT
		ENDIF
	ENDFOR
ENDIF
SELECT(lnOldAls)
RETURN(lcToRet)
*:*************************************************************
*: Name      : lfCheckLns.
*: Developer : Abdou ElGendi
*: Date      : 12/02/2001
*: Purpose   : Function to get the size from scale file.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfCheckLns()
*:*************************************************************
*:
FUNCTION lfCheckLns
PRIVATE lnAlias    , lnRecNo,lnPacked
STORE 0 TO lnAlias , lnRecNo,lnPacked
llFlage = .F.

lnAlias = SELECT (0)
*-- Check if this line packed or not.
IF  SEEK(STR(LINENO,6),lcTmpPkLin)
	SELECT (lcTmpPkLin)
	lnRecNo = RECNO()

	SUM TotQty TO lnPacked FOR STYLE = ORDLINE.STYLE
	IF BETWEEN(lnRecNo,1,RECCOUNT())
		GO lnRecNo
	ENDIF

	IF lnPacked = ORDLINe.TotPik
		llFlage = .T.
	ELSE
		llFlage =  .F.
	ENDIF
ELSE
	*-- Return True if you didn't found this line packed.
	llFlage =  .F.
ENDIF
SELECT(lnAlias)

RETURN llFlage

*-- End OF lfCheckLns.
*:*************************************************************
*: Name      : lfUpDtTmp.
*: Developer : Abdou ElGendi
*: Date      : 12/02/2001
*: Purpose   : Function to get the Data To Tmpdata file.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfUpDtTmp()
*:*************************************************************
*:
FUNCTION  lfUpDtTmp

PRIVATE lnAlias , lcOrder
lnAlias = SELECT (0)
SELECT PikTkt

IF !EMPTY(lcPackNo) .AND. SEEK(lcPackNo,'PACK_HDR')
	*-- Function to delete data from used file.
	= lfReUsefls ()


	IF llHangFlat
		STORE 0 TO lnHngScn,lnFltScn
		SELECT PickScan
		=SEEK (lcPackNo)
		IF FOUND()
			SCAN REST WHILE LEFT(ALLTRIM(cPikTkt),6) = lcPackNo
				SELECT OrdLine
				lcOrder = ORDER()
				SET ORDER TO OrdLines
				=SEEK (PickScan.Style)
				IF FOUND()
					SCAN REST WHILE STYLE+DTOS(COMPLETE)+cOrdType+ORDER+STORE+STR(LINENO,6) = PickScan.Style FOR PikTkt = lcPackNo
						=SEEK (PickScan.Style,'Style')
						SELECT (lcTmpData)
						APPEND BLANK
						lcQtyNo = lfGetScl(OrdLine.Style,PickScan.cPik_Size)
						REPLACE UPC        WITH PickScan.UPC                             ,;
							STYLE      WITH OrdLine.Style                            ,;
							SIZE       WITH lcQtyNo                                  ,;
							cSize      WITH Scale.Sz&lcQtyNo                         ,;
							nStyWeight WITH Style.nStyWeight                         ,;
							TotQty     WITH IIF(EMPTY(lcQtyNo),0,OrdLine.Qty&lcQtyNo),;
							TotPik     WITH IIF(EMPTY(lcQtyNo),0,OrdLine.Pik&lcQtyNo),;
							nScn_Qty   WITH PickScan.nScn_Qty                        ,;
							nRemQty    WITH nRemQty + PickScan.nScn_Qty              ,;
							LINENO     WITH Ordline.lineNo

					ENDSCAN
				ENDIF
			ENDSCAN
			SELECT OrdLine
			SET ORDER TO TAG (lcOrder)
		ENDIF
	ELSE
		STORE 0 TO lnTotOrdr , lnTotPktk
		SELECT (lcTmpData)
		SET ORDER TO &lcTmpData
		SELECT OrdLine
		=SEEK('O'+OrdHdr.Order)
		SCAN REST WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+OrdHdr.Order FOR PikTkt = lcPackNo
			STORE 1 TO lnStart
			lnTotOrdr = lnTotOrdr + OrdLine.TotQty
			lnTotPktk = lnTotPktk + OrdLine.Totpik
			= SEEK(Ordline.Style,'STYLE')
			FOR lnStart = 1 TO SCALE.CNT
				lcSizeScl = STR(lnStart,1)
				IF OrdLine.Pik&lcSizeScl = 0
					LOOP
				ENDIF
				IF SEEK(Ordline.Style +lcSizeScl,'StyleUpc')
					SELECT (lcTmpData)
					IF !SEEK(Ordline.Style +lcSizeScl)
						APPEND BLANK
					ENDIF
					REPLACE Upc        WITH StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3 ,;
						STYLE      WITH OrdLine.Style                                         ,;
						cSize      WITH Scale.Sz&lcSizeScl                                    ,;
						nStyWeight WITH Style.nStyWeight                                      ,;
						SIZE       WITH lcSizeScl                                             ,;
						TotQty     WITH TotQty  + OrdLine.Qty&lcSizeScl                       ,;
						TotPik     WITH TotPik  + OrdLine.Pik&lcSizeScl                       ,;
						nRemQty    WITH nRemQty + OrdLine.Pik&lcSizeScl                       ,;
						LINENO     WITH Ordline.lineNo
				ENDIF
			ENDFOR
		ENDSCAN
	ENDIF
ENDIF

SELECT (lnAlias)

*-- End OF lfUpDtTmp
*B606886,1 ABD - [Begin]
*:*************************************************************
*: Name      : lfActPad
*: Developer : Abdou ElGendi
*: Date      : 03/09/2003
*: Purpose   : Bulid a new menu pad [Options]
*:*************************************************************
*: Calls     : lpvInquiry
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfActPad()
*:*************************************************************
*:
FUNCTION lfActPad

DEFINE PAD   _OPTIONS OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , SPACE(1)
DEFINE POPUP _OPTPOP MARGIN RELATIVE
ON PAD _OPTIONS OF _MSYSMENU ACTIVATE POPUP _OPTPOP

llSkip = !(llCanPrnLb .AND. (laScrMode[3] .OR. laScrMode[4]))
DEFINE BAR 201 OF _OPTPOP PROMPT '\<Print Labels Setup'  SKIP FOR llSkip

IF llCanPrnLb
	ON SELECTION BAR 201 OF _OPTPOP  DO lpPrintCom
ENDIF

*E302144,1 AMH Change the custom C102789 to be standard [Start]
*C102789,4 ASH 02/16/2003 (Begin) Add a new option menu to hold carton type.
*IF ASCAN(laEvntTrig , PADR('ACTPAD',10)) <> 0
*  =gfDoTriger('ALPKSCN',PADR('ACTPAD',10))
*ENDIF
*C102789,4 ASH 02/16/2003 (End)
DEFINE BAR 100 OF _OPTPOP PROMPT '\<Update Carton Dimensions type field'  SKIP FOR (laScrMode[1] .OR. laScrMode[2])
ON SELECTION BAR 100 OF _OPTPOP DO lfCRTNTYPE
*E302144,1 AMH [End]

*-- End OF lfActPad
*:*************************************************************
*: Name      : lpPrintCom.
*: Developer : Abdou ElGendi
*: Date      : 03/09/2003
*: Purpose   : Procedure to select prot for print.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  Do lpPrintCom
*:*************************************************************
*:
PROCEDURE lpPrintCom

PRIVATE lcOldPort , llOldPrn , llCancel
lcOldPort = lcSndPort
llOldPrn  = llScrPrnLb
llCancel  = .F.

IF RIGHT(gcScrDir,1)='\'
	DO (gcScrDir + 'ALOUTPRT.SPX')
ELSE
	DO (gcScrDir + '\ALOUTPRT.SPX')
ENDIF
IF llCancel
	lcSndPort  = lcOldPort
	llScrPrnLb = llOldPrn
ENDIF

*-- End OF lfPrintCom
*:*************************************************************
*: Name      : lfRELPad
*: Developer : Abdou ElGendi
*: Date      : 03/09/2003
*: Purpose   : To release the "Option" menu pad.
*:*************************************************************
*: Calls     :
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None
*:*************************************************************
*: Example   : = lfRELPad()
*:*************************************************************
*:
FUNCTION lfRELPad

RELE BAR 201 OF _OPTIONS
*-- Define the "Options" menu pad.
RELEASE PAD _OPTIONS OF _MSYSMENU

*-- End OF lfRELPad
*B606886,1 ABD - [End]
*:*************************************************************

*:**************************************************************************
*:* Name        : lfCRTNTYPE
*:* Developer   : AMH - AHMED MAHER
*:* Date        : 04/15/2003
*:* Purpose     : Open the screen that updates the carton dimension field in Pack_lin file
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCRTNTYPE()
*:***************************************************************************
*E302144,1
FUNCTION lfCRTNTYPE

PRIVATE laCodes,lnCrtntype,laDfCod,lcSlctdCod
lnCrtntype = 1
DIMENSION laCodes[1,10],laCrtntype[1,2],laDfCod[1]
laCodes[1,1] = 'CCRTNVLTYP'
laCodes[1,2] = 'laCrtntype'
laCodes[1,3] = 'lnCrtntype'
laCodes[1,4] = ''
laCodes[1,5] = .F.
laCodes[1,6] = .F.
laCodes[1,10] = 'CCRTNVLTYP'

laCrtntype = ''
=gfwCodePop(@laCodes,'CCRTNVLTYP','L')

*--Get the "Default code value" position in the array "laCrtntype"
SELECT CCODE_NO FROM CODES WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'DNCCRTNVLTYP' INTO ARRAY laDfCod
IF !EMPTY(laDfCod[1])
	DO CASE
	CASE lnActFolder = 1
		lcSlctdCod = IIF(!EMPTY(lcRPCart),lcRPCart,laDfCod[1])
	CASE lnActFolder = 2
		lcSlctdCod = IIF(!EMPTY(&lcTmpPkLin..CCRTNVLTYP),&lcTmpPkLin..CCRTNVLTYP,laDfCod[1])
	CASE lnActFolder = 3
		lcSlctdCod = IIF(!EMPTY(&lcCtnHdr..CCRTNVLTYP),&lcCtnHdr..CCRTNVLTYP,laDfCod[1])
	ENDCASE
	lnCrtntype = ASUBSCRIPT( laCrtntype , ASCAN(laCrtntype,lcSlctdCod) , 1 )
	DO (gcScrDir+gcWinAppl+'\ALCRTNTY.SPX')
ELSE
	=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Carton Dimensions type'+;
		' codes has not been set up. Please, set up and try again.')
	RETURN
ENDIF
*-- end of lfCRTNTYPE.

*:**************************************************************************
*:* Name        : lfvCrtnOk
*:* Developer   : AMH - AHMED MAHER
*:* Date        : 04/15/2003
*:* Purpose     : Ok button for ALCRTNTY.SCX SCREEN
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvCrtnOk()
*:***************************************************************************
*E302144,1
FUNCTION lfvCrtnOk

PRIVATE lnPos, lcCrtNum
DO CASE
CASE lnActFolder = 1
	lcRPCart = laCrtntype[lnCrtntype,2]
CASE lnActFolder = 2
	IF !EOF(lcTmpPkLin)
		PRIVATE lcAlias
		lcAlias = ALIAS()
		SELECT (lcTmpPkLin)
		lnRcNo = RECNO()
		lcCrtNum = STR(CartonNo,4)
		REPLACE ALL cCrtnVlTyp WITH laCrtntype[lnCrtntype,2] FOR STR(CartonNo,4)=lcCrtNum
		GOTO lnRcNo
		SELECT (lcAlias)
	ENDIF
CASE lnActFolder = 3
	PRIVATE lcAlias
	lcAlias = ALIAS()
	SELECT (lcCtnHdr)
	IF !EOF(lcCtnHdr)
		REPLACE cCrtnVlTyp WITH laCrtntype[lnCrtntype,2]
	ENDIF
	SELECT (lcAlias)
ENDCASE
CLEAR READ
*-- end of lfvCrtnOk.

*:**************************************************************************
*:* Name        : lfOrdLnLkd                                   *-B607420,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/20/2003
*:* Purpose     : check ordline is locked , if no , lock it for currnet operation
*                 if no , give the user a message and stop current operation
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfOrdLnLkd()
*:***************************************************************************
FUNCTION lfOrdLck
PARAMETERS llLock
PRIVATE llGoOn,lnCurAlias
llGoOn = .T.
lnCurAlias = SELECT(0)
*B037019,1  TMI [Start] Lock only the lines of ORDLINE related to the current selected piktkt
*IF !EMPTY(lcPackNo) .AND. SEEK(lcPackNo,'PIKTKT') .AND. SEEK('O'+PIKTKT.ORDER,'ORDHDR')
*SELECT ORDHDR
PRIVATE lcSvOrd
lcSvOrd = ORDER('ORDLINE')
SET ORDER TO ORDLINST IN ORDLINE
IF !EMPTY(lcPackNo) .AND. SEEK(lcPackNo,'PIKTKT') .AND. SEEK('O'+PIKTKT.ORDER+PIKTKT.STORE,'ORDLINE')
	SELECT ORDLINE
	SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+PIKTKT.ORDER+PIKTKT.STORE ;
			FOR ORDLINE.PIKTKT = lcPackNo
		*B037019,1  TMI [End  ]
		IF llLock
			llGoOn = gfObj_Lock(.T.)
			*B037019,1  TMI [Start] ADD EXIT statement
			IF !llGoOn
				EXIT
			ENDIF
			*B037019,1  TMI [End  ]
		ELSE
			IF gcUser_ID = ORDLINE.CLOK_USER
				=gfObj_Lock(.F.)
			ENDIF
		ENDIF
		*B037019,1  TMI [Start]
	ENDSCAN
	*B037019,1  TMI [End  ]
ENDIF
SELECT (lnCurAlias)
*B037019,1  TMI [Start] restore order in ORDLINE file
SET ORDER TO &lcSvOrd IN ORDLINE
*B037019,1  TMI [End  ]
RETURN llGoOn
*-- end of lfOrdLnLkd.

*!*************************************************************
*! Name      : lfManufID
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/12/2003
*! Purpose   : validate pbDtlNew button
*!*************************************************************
*! Example   : = lfManufID(Bill of lading no)
*!*************************************************************
*E037191,1 This function is copied and modified from EDI
FUNCTION lfManufID
PARAMETERS lcBolNo
PRIVATE lcManuf_Id, laRltdFld, MUCCLEVEL

lcManuf_Id = ALLTRIM(laSetUp[4,2])
MUCCLEVEL  = gfGetMemVar('M_UCCDIV',gcAct_Comp)

*-- Maintain UCC manufaturer ID at division level
IF MUCCLEVEL = 'N'
	DECLARE laRltdFld[1,2]
	STORE '' TO laRltdFld,LCUPCMAN
	laRltdFld[1,1] = "CUPCMAN"
	laRltdFld[1,2] = 'LCUPCMAN'
	=gfRltFld(ORDHDR.cDivision,@laRltdFld,'CDIVISION ')
	lcManuf_Id = IIF(EMPTY(LCUPCMAN),lcManuf_Id,LCUPCMAN)
ENDIF
RETURN ALLTRIM(lcManuf_Id)
*-- End of lfManufID.

*:**************************************************************************
*:* Name        : lfCheckDgt
*:* Developer   : SSE - Sameh
*:* Date        : 10/14/2003
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
*:* Name        : lfvTotCrt
*:* Developer   : WLD - Waleed Hamed
*:* Date        : 09/16/2004
*:* Purpose     : Check Total Cartons nubmer
*:***************************************************************************
*:* Called from : screen ALPKSCN2
*:***************************************************************************
*:* Parameters :
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvTotCrt
*:***************************************************************************
FUNCTION lfvTotCrt

IF lnTotCart =< 0
	*- Message Text   :- The Total cartons number is not acceptable.
	*- Message No.    :- 44124.
	*- Buttom Message :- OK
	*- Buttom Number  :- 00000
	= gfModalGen("TRM44124B44124","Dialog")
	lnTotCart = lnLastCart
	RETURN
ENDIF
IF lnTotCart < lnLastCart
	*- Message Text   :- Actual cartons number is not equal to the tape cartons number.
	*- Message No.    :- 44070.
	*- Buttom Message :- OK
	*- Buttom Number  :- 00000
	= gfModalGen("QRM44070B00000","Dialog")
	lnTotCart = lnLastCart
	RETURN
ENDIF
*-- end of  lfChkTotCrt.

*:**************************************************************************
*:* Name        : lfVsulLbl
*:* Developer   : WLD - Waleed Hamed
*:* Date        : 09/12/2004
*:* Purpose     : Calling Crystal Visual Report Label
*:***************************************************************************
*:* Called from : lfSavCartn  and lfvLblInfo
*:***************************************************************************
*:* Parameters : lcVersion && Label Version
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfVsulLbl()
*:***************************************************************************
FUNCTION lfVsulLbl
PARAMETERS lcVersion

SELECT (tmpAsnShip)
lnEdiRecNo = RECNO()
COPY TO (gcWorkDir+lcPrnAsnShp)

SELECT 0
USE (gcWorkDir+lcPrnAsnShp) EXCLUSIVE
INDEX ON bol_no+pack_no+STR(cart_no,6)+asn_ver TAG (lcPrnAsnShp)
USE IN (lcPrnAsnShp)
SELECT (tmpAsnShip)
GOTO lnEdiRecNo

tcEDIAct    = lcAccount
tcEDIBolNo  = lcBol
tcEDIPckNo  = lcPackNo
tnEDICrtNo  = ALLTRIM(STR(lnCarton))
tcEDIShp    = lcPrnAsnShp  && ASN_SHIP Temp file name
tcEDICmp    = gcAct_Comp   && Active compant
tcEDIPrtNm  = lcSndPort    && Port Name
tcEDIVer    = lcVersion    && Label Version
tlEDIDetLb  = llPrintLbl   && Print Detailed Label
tcEDIDetVr  = lcDetailVr   && Detailed Label Version

*E302372,1 TMI [Start] Create temp files to be used by the EDI3 class PrnLable
STORE '' TO lcTmpBolH,lcTmpBolL,lcTmPckH,lcTmPckL
IF !lfTmpSvScr()
  RETURN
ENDIF  
*E302372,1 TMI [End  ] 

*E302372,1 TMI [Start] send more variables
*SAVE TO (gcWorkDir+lcPrnAsnShp+'.MEM') ALL LIKE T?EDI*
SAVE TO (gcWorkDir+lcPrnAsnShp+'.MEM') ALL LIKE * EXCEPT g*
*E302372,1 TMI [End  ] 

lcCommLine = (gcWorkDir+lcPrnAsnShp+'.MEM')
lcLib = SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
	SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
	SW_HIDE = 0
	lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
	*E302372,1 TMI [Start] run the edi3 print label exe 
	IF FILE(gcAppHome+'PRNLABL3.EXE')
      =EVALUATE("CALLFN("+STR(lnFnWinExec)+",gcAppHome+[PRNLABL3.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
    ELSE
      *E302372,1 TMI [End  ]
	
	  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
		  ",gcAppHome+[PRNLABEL.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
      
      *E302372,1 TMI [Start] End if 
    ENDIF
    *E302372,1 TMI [End  ]
	RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
	WAIT "LIBRARY NOT FOUND" WINDOW
	RETURN .F.
ENDIF
*-- end of  lfVsulLbl.
*:*************************************************************
*: Name      : lfWUpc
*: Developer : Mostafa Rawash - (MMR)
*: Date      : 07/12/2005
*: Purpose   : Function to make the Upc Validated without Enter
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfWUpc ()
*:*************************************************************
*: B128758,1 MMR 07/12/2005 Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen)
FUNCTION lfWUpc
lcCnfrm = IIF(llConfirm,'ON','OFF')
SET CONFIRM &lcCnfrm
*-- End Of lfWUpc.
*: B128758,1 MMR [End]

*:**************************************************************************
*:* Name        : lfvUccStrc
*:* Developer   : Hend Ghanem (HBG)
*:* Date        : 03/14/2006
*:* Purpose     : Valid function to Ok bnuton in UCC # structure screen
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvUccStrc()
*:***************************************************************************
*E040123
FUNCTION lfvUccStrc

DO CASE
  CASE lnChoice = 1
    lnNumOfDig = 5
  CASE lnChoice = 2
    lnNumOfDig = 6
  CASE lnChoice = 3
    lnNumOfDig = 9
ENDCASE

CLEAR READ

*:**************************************************************************
*:* Name        : lfTmpSvScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/04/2007
*:* Purpose     : to simulate the saving process to the actual files to be done to a temp files
*-                then open these temp files when calling the "SendUccLabels" since that class it is based on already saved data
*-                but aria27 is designed to print cartons in edit mode only , i.e based on a temp data
*:***************************************************************************
*E302372,1 TMI 
PROCEDURE lfTmpSvScr
PRIVATE lnCurAlias,lnRecno,laTotCrt
lnCurAlias = SELECT(0) 

*- check if edi3 is not installed , then do not continue
IF !FILE(gcAppHome+'PRNLABL3.EXE')
  RETURN
ENDIF
 
SELECT &lcTmpPkLin
lnRecno = RECNO()
SELECT COUNT(DISTINCT CartonNo) FROM &lcTmpPkLin INTO ARRAY laTotCrt

SET FILTER TO CARTONNO = lnCartonNo
GO TOP
IF EOF()
  SET FILTER TO 
  GO TOP
  IF BETWEEN(lnRecno,1,RECCOUNT())
    GOTO (lnRecno)
  ENDIF
  
  SELECT (lnCurAlias)
  RETURN
ENDIF

*- Create temp names for files that will be used in the edi3 class with the related file aliases
lcTmpBolH = gfTempName()   && related to BOL_HDR
lcTmpBolL = gfTempName()   && related to BOL_LIN
lcTmPckH  = gfTempName()   && related to PACK_HDR
lcTmPckL  = gfTempName()   && related to PACK_LIN

*- Create and Open temp. files
SELECT BOL_HDR
=SEEK(lcBol,'BOL_HDR')
COPY TO (gcWorkDir+lcTmpBolH) NEXT 1 WITH CDX
USE (gcWorkDir+lcTmpBolH) IN 0 ORDER TAG (ORDER('BOL_HDR'))

SELECT BOL_LIN
COPY STRUCTURE TO (gcWorkDir+lcTmpBolL) WITH CDX
USE (gcWorkDir+lcTmpBolL) IN 0 ORDER TAG (ORDER('BOL_LIN'))

SELECT PACK_HDR
COPY STRUCTURE TO (gcWorkDir+lcTmPckH) WITH CDX
USE (gcWorkDir+lcTmPckH) IN 0 ORDER TAG (ORDER('PACK_HDR'))

SELECT PACK_LIN
COPY STRUCTURE TO (gcWorkDir+lcTmPckL) WITH CDX
USE (gcWorkDir+lcTmPckL) IN 0 ORDER TAG (ORDER('PACK_LIN'))

*- Setting order tags
SET ORDER TO TAG ORDLINE   IN ORDLINE
SET ORDER TO TAG ORDLINENO IN (lcTmpPkLin)

lnTotCart = 0
lnLineNo  = IIF(SEEK(lcPackNo,'PACK_HDR'),PACK_HDR.nLastLNo,0)
SELECT (lcTmpPkLin)
GO TOP
DO WHILE !EOF()
  lcOrdLNo = STR(OrdLineNo,6)
  =SEEK(STYLE,'Style')
  STORE 0 TO lnPack1,lnPack2,lnPack3,lnPack4,lnPack5,lnPack6,lnPack7,lnPack8
  SCAN REST WHILE STR(OrdLineNo,6) = lcOrdLNo
    lcStySize = SIZE
    lnPack&lcStySize = lnPack&lcStySize + TotQty
    
    SELECT &lcTmPckL
    =SEEK(lcPackNo+STR(&lcTmpPkLin..CartonNo,4)+&lcTmpPkLin..Style)

    LOCATE REST WHILE Pack_no+STR(no_cart,4)+STYLE = lcPackNo+STR(&lcTmpPkLin..CartonNo,4)+&lcTmpPkLin..Style ;
                  FOR nOrdLineno=&lcTmpPkLin..ORDLINENO
    IF !FOUND()
      lnLineNo = lnLineNo + 1
      INSERT INTO &lcTmPckL (PACK_NO,Line_No,STYLE,No_cart,From_Crt,To_Crt,nOrdLineNo) VALUES ;
        (lcPackNo,lnLineNo,&lcTmpPkLin..Style,&lcTmpPkLin..CartonNo,&lcTmpPkLin..CartonNo,;
        &lcTmpPkLin..CartonNo,&lcTmpPkLin..OrdLineNo)
    ENDIF
    
    REPLACE Qty&lcStySize WITH Qty&lcStySize + &lcTmpPkLin..TotQty  ;
            TotQty        WITH TotQty        + &lcTmpPkLin..TotQty  ;
            Weight        WITH TotQty * Style.nStyWeight            ;
            CCRTNVLTYP    WITH IIF(EMPTY(&lcTmpPkLin..CCRTNVLTYP),lcRPCart,&lcTmpPkLin..CCRTNVLTYP)
    lnTotCart = MAX(lnTotCart,&lcTmpPkLin..CartonNo)
  ENDSCAN
  
  SELECT (lcTmpPkLin)
ENDDO

*-- Check for Edi Module.
IF llEDIInstl
  
  IF llEDIInstl .AND. SEEK ('A'+lcAccount,'EdiAcPrt') .AND. SEEK(EdiAcPrt.cPartCode+'856','EDIPD') AND !EMPTY(lcBol)
    IF !EMPTY(lcBol) .AND. !SEEK(lcBol+lcOrderNo+lcPackNo,lcTmpBolL)
      INSERT INTO &lcTmpBolL (BOL_NO,ORDER,PACK_NO,COWNER) VALUES (lcBol,lcOrderNo,lcPackNo,'N')
    ENDIF
    =SEEK(lcBol,lcTmpBolH)
    SELECT &lcTmpBolH
    REPLACE Tot_Wght WITH lnTotWg      ,;
            Tot_Cart WITH laTotCrt[1]  ,;
            Tot_Pcs  WITH lnTotPiece
  ENDIF

  SELECT &lcTmPckH
  APPEND BLANK
  REPLACE Pack_No    WITH lcPackNo   ,;
          ORDER      WITH lcOrderNo  ,;
          Account    WITH lcAccount  ,;
          cWareCode  WITH lcWarCode  ,;
          STORE      WITH lcStore    ,;
          Bill_ladg  WITH lcBol      ,;
          Tot_Wght   WITH lnTotWg    ,;
          Tot_Pcs    WITH lnTotPiece ,;
          Tot_Cart   WITH laTotCrt[1],;
          Note       WITH lcNotes    ,;
          ShipVia    WITH lcShipVia  ,;
          cPkChCode  WITH lcPackChar ,;
          cPkDsCode  WITH lcPackDesc ,;
          cToStorCn  WITH IIF(lnToStorCn=0,'S','C') ,;
          Piktkt     WITH lcPackNo   ,;
          Sp_Inst1   WITH lcSp_Inst1 ,;
          Sp_Inst2   WITH lcSp_Inst2 ,;
          nLastLNo   WITH lnLineNo   ,;
          nLastCart  WITH lnLastCart ,;
          CCRTNVLTYP WITH lcRPCart
ENDIF

*- Close temp files
USE IN &lcTmpBolH 
USE IN &lcTmpBolL 
USE IN &lcTmPckH  
USE IN &lcTmPckL  

SET ORDER TO TAG ORDLINST   IN ORDLINE
SET ORDER TO TAG (lcTmpPkLin) IN (lcTmpPkLin)

SELECT &lcTmpPkLin
SET FILTER TO
GO TOP
IF BETWEEN(lnRecno,1,RECCOUNT())
  GOTO (lnRecno)
ENDIF

SELECT (lnCurAlias)
