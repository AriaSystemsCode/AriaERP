*!*****************************************************************************************
*! Name      : ALPKTK.PRG
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 06/28/2005
*! Purpose   : Picking Ticket Screen
*! Entry no. : N037425 - Picking Ticket Screen
*:************************************************************************
*: Modifications:
*! B607811,1 WAM 10/29/2006 Fix bug Index tag not found
*! N000592,1 HBG 02/28/2007 T20061201.0014 Print store ship to address or DC address according to flag in SO
*! C200876,1 TMI 05/02/2008 Adding the BIN location triggers
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003]
*! E302946,1 WAM 07/26/2011 Define new bar to call carrier Integration screen [T20101207.0006]
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*! B609818,1 MMT 02/05/2012 Modify screen to display carrier option only if client has carriers[T20120118.0002]
*! B609839,1 MMT 02/22/2012 Picking ticket screen displays incorrect Shipvia  in case of multi store order and Shipvia is on store level[T20120220.0030]
*! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035]
*! B610570,1 MMT 10/31/2013 Fix bug of error while browsing Orders[R13 issues]
*! B611624,1 Es 08/30/2018 Modify the issue of warehouse mismatching  between Ordhdr and ORDLINE"  [T20180717.0024]
*! B612393,1 MMT 05/18/2021 Error while opening Picking ticket screen from Sales and Shipping Process helper
*!*****************************************************************************************
#INCLUDE R:\Aria4xp\Screens\AL\alpktkt.H
PARAMETERS lcpPikTkt

*-IF the program is running stand alone or to view passed style.
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"\AL\ALPKTKT.SCX") WITH lcpPikTkt
=gfCallForm('ALPKTKT','AL',"lcpPikTkt")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
RETURN

*Defining the Business class
DEFINE CLASS Picking_Ticket AS Custom
  loFormSet = .F.
  loForm = .F.
  llBrowse = .F.
  llExCalled = .F.
  lcSentPik = ""
  llSekPak = .F.    &&picking ticket has packing list or not
  llKeyBrowse = .F. &&use to browse by pressing key

*!*****************************************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 06/29/2005
*! Purpose   : Validate icking Ticket #
*!*****************************************************************************************
FUNCTION lfInit
LPARAMETERS loFrm

SET MULTILOCKS ON
This.loForm    = loFrm
This.loFormSet = loFrm.Parent


This.lfopenfile('PikTkt','PikTkt')
SELECT piktkt
SET FILTER TO PIKTKT <>'******'
This.lfopenfile('OrdHdr','OrdHdr')
This.lfopenfile('OrdLine','OrdLine')
This.lfopenfile('PikLine','PikLine')
This.lfopenfile('Customer','Customer')
This.lfopenfile('PACK_HDR','PACK_HDR')
This.lfopenfile('WAREHOUS','WAREHOUS')
This.lfopenfile('Scale','Scale')
This.lfopenfile('STYDYE','STYDYE')
This.lfopenfile('STYLE','STYLE')
This.lfopenfile('ICSTYHST','STYHST')
This.lfopenfile('ARCUSHST','ACTHST')
This.loFormSet.lc_TmpOrdl = gfTempName()
This.loFormSet.llMultWare = (ALLTRIM(gfGetMemVar('M_WareHouse')) = 'Y')

*-- llUseDyes identify whether co. uses Dyelots or not (restore company settings)
*-- furthermore , not only per company but also per style
This.loFormSet.llUseDyes  = (ALLTRIM(gfGetMemVar('M_DYELOT'))    = 'Y')
This.loFormSet.llUse_Configuration =(ALLTRIM(gfGetMemVar("M_STYCNFG")) = "Y")
This.lfCrtTemp()

This.loFormSet.maddcontsource ()

This.loFormSet.llPakStats = .F.
IF ASCAN(This.loFormSet.laEvntTrig  , PADR('LLPAKST',10)) <> 0
  This.loFormSet.llPakStats = .T.
ENDIF

IF This.loFormSet.llPakStats
  DIMENSION This.loFormSet.laPkTktStatus[4,2]
  DIMENSION This.loFormSet.laStatus[7,2]
ELSE
  DIMENSION This.loFormSet.laPkTktStatus[3,2]
  DIMENSION This.loFormSet.laStatus[6,2]
ENDIF

This.loFormSet.laStatus[1,1] = ' '
This.loFormSet.laStatus[1,2] = ' '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.loFormSet.laStatus[2,1] = LANG_Open
This.loFormSet.laStatus[2,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Open,this.loFormSet.GetHeaderText("LANG_Open",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

This.loFormSet.laStatus[2,2] = "O"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.loFormSet.laStatus[3,1] = LANG_Hold
This.loFormSet.laStatus[3,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Hold,this.loFormSet.GetHeaderText("LANG_Hold",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

This.loFormSet.laStatus[3,2] = "H"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.loFormSet.laStatus[4,1] = LANG_Complete
This.loFormSet.laStatus[4,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Complete,this.loFormSet.GetHeaderText("LANG_Complete",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

This.loFormSet.laStatus[4,2] = "C"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.loFormSet.laStatus[5,1] = LANG_Cancel
This.loFormSet.laStatus[5,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Cancel,this.loFormSet.GetHeaderText("LANG_Cancel",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

This.loFormSet.laStatus[5,2] = "X"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.loFormSet.laStatus[6,1] = LANG_Pulled
This.loFormSet.laStatus[6,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Pulled,this.loFormSet.GetHeaderText("LANG_Pulled",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

This.loFormSet.laStatus[6,2] = "P"

IF This.loFormSet.llPakStats
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.loFormSet.laStatus[7,1] = LANG_Packed
This.loFormSet.laStatus[7,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Packed,this.loFormSet.GetHeaderText("LANG_Packed",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  This.loFormSet.laStatus[6,2] = "K"
ENDIF

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.loFormSet.laPkTktStatus[1,1]  = LANG_Open
This.loFormSet.laPkTktStatus[1,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Open,this.loFormSet.GetHeaderText("LANG_Open",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

This.loFormSet.laPkTktStatus[1,2]  = 'O'

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.loFormSet.laPkTktStatus[2,1]  = LANG_Hold
This.loFormSet.laPkTktStatus[2,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Hold,this.loFormSet.GetHeaderText("LANG_Hold",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

This.loFormSet.laPkTktStatus[2,2]  = 'H'

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.loFormSet.laPkTktStatus[3,1]  = LANG_Pulled
This.loFormSet.laPkTktStatus[3,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Pulled,this.loFormSet.GetHeaderText("LANG_Pulled",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

This.loFormSet.laPkTktStatus[3,2]  = 'P'

IF This.loFormSet.llPakStats
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.loFormSet.laPkTktStatus[4,1]  = LANG_Packed
This.loFormSet.laPkTktStatus[4,1]  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Packed,this.loFormSet.GetHeaderText("LANG_Packed",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  This.loFormSet.laPkTktStatus[4,2]  = 'K'
ENDIF

WITH This.loFormSet
  IF !This.PikTkt.llNative
    .cbrowsetabledbengine   = "SQL"
  ELSE
    .cbrowsetabledbengine   = 'NATIVE'
  ENDIF
  .nWorkArea        = 'PikTkt'
  .DataEnvironment.InitialSelectedAlias = 'PIKTKT'
  .cBrowseFileName        = "PIKTKT"
  .cBrowseIndexExpression = "PIKTKT"
  .cBrowseIndexFields     = "PIKTKT"
  .cBrowseIndexName       = "PIKTKT"
  .cBrowseAliasName       = "PIKTKT"
  .cBrowseTableName       = "PIKTKT"
  .cBrowseFilter          = "PIKTKT <> '******'"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*   .BrowseTitle 		  	  ="Picking Tickets"
  .BrowseTitle 		  	  =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BROWTITLE,this.loFormSet.GetHeaderText("LANG_BROWTITLE",this.loFormSet.HeaderAlias))
 
  *N000682,1 11/20/2012 MMT Globlization changes[End]
ENDWITH

This.loFormSet.ariaform1.cboStatus.RowSource = "Thisformset.laStatus"
*!*  IF This.llExCalled AND !EMPTY(This.lcSentPik)
*!*    This.lfGetData(This.lcSentPik)
*!*  ENDIF
*!*****************************************************************************************
*! Name      : lfvPickTicket
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 06/29/2005
*! Purpose   : Validate icking Ticket #
*!*****************************************************************************************
FUNCTION lfvPickTicket

*! B612393,1 MMT 05/18/2021 Error while opening Picking ticket screen from Sales and Shipping Process helper[Start]
IF TYPE('This.loFormSet') <> 'O' OR ISNULL(This.loFormSet)
  RETURN
ENDIF
*! B612393,1 MMT 05/18/2021 Error while opening Picking ticket screen from Sales and Shipping Process helper[End]


LOCAL lcPickTicketNo
lcPickTicketNo = PADR(This.loFormSet.ariaform1.kbPickNo.keytextbox.Value,6)
*!*	IF !EMPTY(lcPickTicketNo) AND  LEN(ALLTRIM(lcPickTicketNo)) < 6 AND !(AT('?',ALLTRIM(lcPickTicketNo),1) > 0)
*!*	  WAIT WINDOW LANG_Invalid_Pik
*!*	  STORE "" TO This.loFormSet.ariaform1.kbPickNo.keytextbox.Value
*!*	  RETURN .F.
*!*	ENDIF
SELECT PIKTKT
LOCATE
IF (EMPTY(lcPickTicketNo) AND This.loFormSet.ariaform1.kbpickNo.selectedfrombrowse) OR ;
  (AT('?',ALLTRIM(lcPickTicketNo),1) > 0) OR;
  (!EMPTY(lcPickTicketNo) AND !This.PikTkt.SEEK(lcPickTicketNo)) OR ;
  This.loFormSet.ariaform1.kbpickNo.selectedfrombrowse

  lcBrFields = [PIKTKT :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Pick_Ticket_No,this.loFormSet.GetHeaderText("LANG_Pick_Ticket_No",this.loFormSet.HeaderAlias))+[' , ACCOUNT :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account,this.loFormSet.GetHeaderText("LANG_Account",this.loFormSet.HeaderAlias))+[', STORE :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Store,this.loFormSet.GetHeaderText("LANG_Store",this.loFormSet.HeaderAlias))+[' ,] +[ORDER :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Order,this.loFormSet.GetHeaderText("LANG_Order",this.loFormSet.HeaderAlias))+[' , DATE :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Date,this.loFormSet.GetHeaderText("LANG_Date",this.loFormSet.HeaderAlias))+[', ] +[CWARECODE :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Location,this.loFormSet.GetHeaderText("LANG_Location",this.loFormSet.HeaderAlias))+[' , CUSTPO :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CustPo,this.loFormSet.GetHeaderText("LANG_CustPo",this.loFormSet.HeaderAlias))+[' ,STATUS = IIF(STATUS = 'O' , ']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Open,this.loFormSet.GetHeaderText("LANG_Open",this.loFormSet.HeaderAlias))+[' , IIF(STATUS = 'H' ,']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Hold,this.loFormSet.GetHeaderText("LANG_Hold",this.loFormSet.HeaderAlias))+[',IIF(STATUS = 'C',']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Complete,this.loFormSet.GetHeaderText("LANG_Complete",this.loFormSet.HeaderAlias))+[',IIF(STATUS = 'K','] +;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Packed	,this.loFormSet.GetHeaderText("LANG_Packed",this.loFormSet.HeaderAlias))+[',IIF(STATUS = 'X',']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Cancel,this.loFormSet.GetHeaderText("LANG_Cancel",this.loFormSet.HeaderAlias))+[',']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Pulled,this.loFormSet.GetHeaderText("LANG_Pulled",this.loFormSet.HeaderAlias))+['))))) :R :H=']+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Status,this.loFormSet.GetHeaderText("LANG_Status",this.loFormSet.HeaderAlias))+[']
*N000682,1 11/20/2012 MMT Globlization changes[End]


  IF !This.loFormSet.ariaform1.kbpickNo.selectedfrombrowse
    This.loFormSet.ariaform1.kbpickNo.selectedfrombrowse = .T.
  ENDIF

  IF This.loFormSet.ariaform1.kbpickNo.selectedfrombrowse
    lcIndexSel = "PIKTKT" 	
    lcFilter = "  For PikTkt <> '******'"
    IF !EMPTY(This.loFormSet.AriaForm1.kbOrder.keytextbox.Value)
      lcSeleOrder = This.loFormSet.AriaForm1.kbOrder.keytextbox.Value
      lcFilter = "ALLTRIM(lcSeleOrder)" + lcFilter
      lcIndexSel = "ORDPIK"
      *+ " AND " + "ORDER+PIKTKT = '"+This.loFormSet.AriaForm1.kbOrder.keytextbox.Value+"'"
    ENDIF
    IF EMPTY(This.loFormSet.AriaForm1.kbOrder.keytextbox.Value) AND;
      !EMPTY(This.loFormSet.AriaForm1.kbAccount.keytextbox.Value)
      lcFilter = lcFilter + " AND " + " ACCOUNT  = '"+This.loFormSet.AriaForm1.kbAccount.keytextbox.Value+"'"
    ENDIF
    *B607811,1 WAM 10/29/2006 Fix bug Index tag not found
    This.PIKTKT.SetOrder(lcIndexSel)
    *B607811,1 WAM 10/29/2006 (End)

    *lcPickTicketNo = IIF(ARIABROW(lcFilter,"Piktkt",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Piktkt','laBrowArr'),Piktkt.Piktkt,SPACE(6))
    *lcPickTicketNo = IIF(ARIABROW(lcFilter,"Piktkt",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
    *                     gnBrFSCol2,'','','Piktkt','laBrowArr', .F.,'',.F.,'PIKTKT','','',lcIndexSel),Piktkt.Piktkt,SPACE(6))
    lcPickTicketNo = IIF(ARIABROW(lcFilter,This.loFormSet.BrowseTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
                         gnBrFSCol2,'','','Piktkt','laBrowArr', .F.,'',.F.,'PIKTKT','','',lcIndexSel),Piktkt.Piktkt,SPACE(6))                         
                         
                         
    *B607811,1 WAM 10/29/2006 Fix bug Index tag not found
    This.PIKTKT.SetOrder('PIKTKT')
    *B607811,1 WAM 10/29/2006 (End)

    This.loFormSet.AriaForm1.kbPickNo.keytextbox.Value = lcPickTicketNo
    This.loFormSet.ariaform1.kbpickNo.selectedfrombrowse = .F.
    RETURN .T.
  ENDIF
ELSE
  IF This.PikTkt.SEEK(lcPickTicketNo)
    This.loFormSet.ariaform1.kbpickNo.selectedfrombrowse = .F.
    RETURN .T. 	
  ENDIF
ENDIF
ENDFUNC
*!*****************************************************************************************
*! Name      : lfvOrder
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 06/29/2005
*! Purpose   : open Files remotely
*!*****************************************************************************************
FUNCTION lfvOrder
LOCAL lcSelOrder
PRIVATE lnCurAlias,lcTag,lcOrder


lnCurAlias = SELECT(0)
lcSelOrder = This.loFormSet.ariaform1.kborder.keytextbox.Value
IF !((EMPTY(lcSelOrder) OR LASTKEY() <> 13) AND !This.loFormSet.AriaForm1.kborder.selectedfrombrowse)
  IF (!EMPTY(lcSelOrder) .AND. !This.OrdHdr.SEEK('O'+lcSelOrder,'OrdHdr')) OR This.loFormSet.AriaForm1.kborder.selectedfrombrowse
    lcOrder = lcSelOrder
    This.lfOrdBrow(@lcOrder,This.loFormSet.ariaform1.kbAccount.keytextbox.Value)
    This.loFormSet.ariaform1.kborder.keytextbox.Value = lcOrder
    This.loFormSet.AriaForm1.kborder.selectedfrombrowse = .F.
  ENDIF
ENDIF
lcSelOrder = This.loFormSet.ariaform1.kborder.keytextbox.Value
IF !EMPTY(This.loFormSet.ariaform1.kborder.keytextbox.Value)
   *--
   This.lfopenfile('PIKTKT','ORDPIK')	
   IF !This.PikTkt.Seek(lcSelOrder)
      *-- 'No piktkts found for order xxxx
      *-- <OK>
      =gfModalGen("INM44028B00000","Dialog",lcSelOrder)
      *-- return to select mode/ini. vars.
      This.lfopenfile('PIKTKT','PIKTKT')	
      This.loFormSet.ariaform1.kborder.keytextbox.Value  = SPACE(6)
      RETURN
   ENDIF
   This.lfopenfile('PIKTKT','PIKTKT')	
   This.loFormSet.ariaform1.kbAccount.keytextbox.Value = ORDHDR.Account
   = THIS.Customer.SEEK('M'+ORDHDR.Account)
   This.loFormSet.AriaForm1.txtAccountName.Value = Customer.StName
ENDIF
SELECT(lnCurAlias)
ENDFUNC
*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : browse all pack list or validated by the account
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : AriaBrow,gfModalGen
*!*************************************************************
*! Passed Parameters  : lcOrder,lcAccount
*!*************************************************************
*! Returns            : llReturn
*!*************************************************************
*! Example   : = lfOrdBrow()
*!*************************************************************
FUNCTION lfOrdBrow

PARAMETERS lcOrder,lcAccount
*-- lcFields   variable that hold the name of browsed fields
*-- laBrow     array that hold the returned values from AriaBrow function
*-- lnCurAlias variable that hold the current alias
*-- lcCurTag   variable that hold the currend tag name
*-- llReturn   variable which is returned by this function
*-- lcTag      variable that hold the name of the tag which is desired to switch
*              file order to it
PRIVATE lcFields,laBrowArr,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields
DIMENSION laBrowArr[1]
STORE SPACE(0) TO lcFields,laBrowArr,lcBrFields

lnCurAlias = SELECT(0)
SELECT ORDHDR
LOCATE
*! B610570,1 MMT 10/31/2013 Fix bug of error while browsing Orders[R13 issues][Start]
*!*	lcBrFields = [Order:H=']+;
*!*	             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Order_No,this.loFormSet.GetHeaderText("LANG_Order_No",this.loFormSet.HeaderAlias))+;
*!*	             [',status:1:H=']+;
*!*	             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Order_Status,this.loFormSet.GetHeaderText("LANG_Order_Status",this.loFormSet.HeaderAlias))+;
*!*	             [',lcSesDesc=gfCodDes(Season,'SEASON'):H=']+;
*!*	             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Season,this.loFormSet.GetHeaderText("LANG_Season",this.loFormSet.HeaderAlias))+;
*!*	             [',lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H=']+;
*!*	             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Division,this.loFormSet.GetHeaderText("LANG_Division",this.loFormSet.HeaderAlias))+[',]+;
*!*	             [CustPo=IIF(multipo,']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MultiPo,this.loFormSet.GetHeaderText("LANG_MultiPo",this.loFormSet.HeaderAlias))+;
*!*	             [',custpo):H=']+;
*!*	             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CustPo,this.loFormSet.GetHeaderText("LANG_CustPo",this.loFormSet.HeaderAlias))+[',]+;
*!*	             [ACCOUNT:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account,this.loFormSet.GetHeaderText("LANG_Account",this.loFormSet.HeaderAlias))+;
*!*	             [',store=IIF(MULTI='Y',']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MultiStore,this.loFormSet.GetHeaderText("LANG_MultiStore",this.loFormSet.HeaderAlias))+;
*!*	             [',STORE):H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Store,this.loFormSet.GetHeaderText("LANG_Store",this.loFormSet.HeaderAlias))+[',Customer.stname]+;
*!*	             [:15:H=']+;
*!*	             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CustName,this.loFormSet.GetHeaderText("LANG_CustName",this.loFormSet.HeaderAlias))+;
*!*	             [',Open:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OpenQty,this.loFormSet.GetHeaderText("LANG_OpenQty",this.loFormSet.HeaderAlias))+;
*!*	             [',OpenAmt:H=']+;
*!*	             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OpenAmt,this.loFormSet.GetHeaderText("LANG_OpenAmt",this.loFormSet.HeaderAlias))+;
*!*	             [',Ship:H=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ShipQty,this.loFormSet.GetHeaderText("LANG_ShipQty",this.loFormSet.HeaderAlias))+;
*!*	             [',Shipamt:H=']+;
*!*	             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ShipAmt,this.loFormSet.GetHeaderText("LANG_ShipAmt",this.loFormSet.HeaderAlias))+[',]+;
*!*	             [start:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Start,this.loFormSet.GetHeaderText("LANG_Start",this.loFormSet.HeaderAlias))+;
*!*	             [',Complete:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Complete,this.loFormSet.GetHeaderText("LANG_Complete",this.loFormSet.HeaderAlias))+[',]+;
*!*	             [Note1:6:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Notes,this.loFormSet.GetHeaderText("LANG_Notes",this.loFormSet.HeaderAlias))+[']
lcBrFields = [Order:H=']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Order_No,this.loFormSet.GetHeaderText("LANG_Order_No",this.loFormSet.HeaderAlias))+;
             [',status:1:H=']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Order_Status,this.loFormSet.GetHeaderText("LANG_Order_Status",this.loFormSet.HeaderAlias))+;
             [',lcSesDesc=gfCodDes(Season,'SEASON'):H=']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Season,this.loFormSet.GetHeaderText("LANG_Season",this.loFormSet.HeaderAlias))+;
             [',lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H=']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Division,this.loFormSet.GetHeaderText("LANG_Division",this.loFormSet.HeaderAlias))+[',]+;
             [CustPo=IIF(multipo,']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MultiPo,this.loFormSet.GetHeaderText("LANG_MultiPo",this.loFormSet.HeaderAlias))+;
             [',custpo):H=']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CustPo,this.loFormSet.GetHeaderText("LANG_CustPo",this.loFormSet.HeaderAlias))+[',]+;
             [ACCOUNT:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account,this.loFormSet.GetHeaderText("LANG_Account",this.loFormSet.HeaderAlias))+;
             [',store=IIF(MULTI='Y',']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MultiStore,this.loFormSet.GetHeaderText("LANG_MultiStore",this.loFormSet.HeaderAlias))+;
             [',STORE):H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Store,this.loFormSet.GetHeaderText("LANG_Store",this.loFormSet.HeaderAlias))+[',Customer.stname]+;
             [:15:H=']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CustName,this.loFormSet.GetHeaderText("LANG_CustName",this.loFormSet.HeaderAlias))+;
             [',Open:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OpenQty,this.loFormSet.GetHeaderText("LANG_OpenQty",this.loFormSet.HeaderAlias))+;
             [',OpenAmt:H=']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OpenAmt,this.loFormSet.GetHeaderText("LANG_OpenAmt",this.loFormSet.HeaderAlias))+;
             [',Ship:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ShipQty,this.loFormSet.GetHeaderText("LANG_ShipQty",this.loFormSet.HeaderAlias))+;
             [',Shipamt:H=']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ShipAmt,this.loFormSet.GetHeaderText("LANG_ShipAmt",this.loFormSet.HeaderAlias))+[',]+;
             [start:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Start,this.loFormSet.GetHeaderText("LANG_Start",this.loFormSet.HeaderAlias))+;
             [',Complete:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Complete,this.loFormSet.GetHeaderText("LANG_Complete",this.loFormSet.HeaderAlias))+[',]+;
             [Note1:6:H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Notes,this.loFormSet.GetHeaderText("LANG_Notes",this.loFormSet.HeaderAlias))+[']
*! B610570,1 MMT 10/31/2013 Fix bug of error while browsing Orders[R13 issues][End]             
*N000682,1 11/20/2012 MMT Globlization changes[End]

DO CASE
  CASE !EMPTY(lcAccount)
    This.lfopenfile('OrdHdr','OrdAcct')	
    This.ordhdr.Seek(lcAccount+'O'+lcOrder)
*    lcOrder = IIF(ARIABROW("lcAccount+'O'",;
                  "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))
    lcOrder = IIF(ARIABROW("lcAccount+'O'",;
                  "Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','',;
                  'Order','laBrowArr',.F.,'',.F.,'ORDHDR','','','OrdAcct'),;
                  OrdHdr.Order,SPACE(6))

  CASE EMPTY(lcAccount)
    This.lfopenfile('OrdHdr','OrdHdr')
    This.ordhdr.Seek('O'+lcOrder)
*    lcOrder = IIF(ARIABROW("'O'","Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))
    lcOrder = IIF(ARIABROW("'O'","Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,;
    						'','','Order','laBrowArr', .F.,'',.F.,'ORDHDR','','','ORDHDR'),;
			                OrdHdr.Order,SPACE(6))


ENDCASE
This.lfopenfile('OrdHdr','OrdHdr')	
SELECT (lnCurAlias)
*!*****************************************************************************************
*! Name      : lfOpenFile
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 06/29/2005
*! Purpose   : open Files remotely
*!*****************************************************************************************
FUNCTION lfOpenFile
PARAMETERS lcFile, lcTag
LOCAL lcProp
  lcFile = JUSTSTEM(lcFile)
  lcTag = JUSTSTEM(lcTag)
  lcProp = lcFile
  IF !PEMSTATUS(This,lcProp,5)
    This.addproperty(lcProp)
  ENDIF
  lcProp = 'This.'+lcProp

  IF TYPE(lcProp)<>'O'
    &lcProp = CREATEOBJECT("RemoteTable",lcFile,lcTag,lcFile,This.loFormSet.DataSessionID)
  ELSE
    &lcProp..SetOrder(lcTag)
  ENDIF
ENDFUNC
*!*****************************************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 06/29/2005
*! Purpose   : create temp file
*!*****************************************************************************************
FUNCTION lfCrtTemp
SELECT OrdLine
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
lnStartSize = lnFileStru
DIMENSION laFileStru[lnFileStru + 19, 18]
*-- Add a field for modification/deletion status
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cStatus'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 1
laFileStru[lnFileStru ,4] = 0
*-- Add a field for record number
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nRecNo'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'OldPik1'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'OldPik2'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'OldPik3'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'OldPik4'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'OldPik5'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'OldPik6'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'OldPik7'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'OldPik8'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Ordln1'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Ordln2'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Ordln3'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Ordln4'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Ordln5'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Ordln6'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Ordln7'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Ordln8'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0
*E303874 MHM Add vendor style column to pick ticket screen
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cvensty'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 19
laFileStru[lnFileStru ,4] = 0
 *E303874 MHM Add vendor style column to pick ticket screen
FOR lnLoop = 1 TO  19
  STORE ' ' TO  laFileStru[lnStartSize +lnLoop,7],laFileStru[lnStartSize+lnLoop,8],;
                laFileStru[lnStartSize+lnLoop,9],laFileStru[lnStartSize+lnLoop,10],;
                laFileStru[lnStartSize+lnLoop,11],laFileStru[lnStartSize+lnLoop,12],;
                laFileStru[lnStartSize+lnLoop,13],laFileStru[lnStartSize+lnLoop,14],;
                laFileStru[lnStartSize+lnLoop,15],laFileStru[lnStartSize+lnLoop,16]
  STORE 0 TO    laFileStru[lnStartSize+lnLoop,17] ,laFileStru[lnStartSize+lnLoop,18]
ENDFOR

=gfCrtTmp(This.loFormSet.lc_tmpordl,@laFileStru,"cOrdType + Order + STR(LineNo,6)",'OrdLine',.T.)
SELECT(This.loFormSet.lc_TmpOrdl)
INDEX ON cOrdType + Order + Pack_Id + cPkColor + cPckSize +;
         cPkVersion + STR(LineNo,6) TAG OrdPack

ENDFUNC
*!*****************************************************************************************
*! Name      : lfGetData
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 06/29/2005
*! Purpose   : open Files remotely
*!*****************************************************************************************
FUNCTION lfGetData
PARAMETERS lcPikticNo
IF !EMPTY(lcPikticNo)
  THIS.PIKTKT.SEEK(lcPikticNo)
ENDIF
  DIMENSION laAddress[6,3]
  WITH This.loFormSet.AriaForm1
  	.kbPickNo.keytextbox.Value 		  = PIKTKT.PIKTKT
    .cboStatus.Value 		      		  = Piktkt.Status
    .kbAccount.keytextbox.Value 	  = Piktkt.Account
    .kbOrder.keytextbox.Value	  	  = Piktkt.Order
    .dtTickDate.Value 				      = piktkt.date
    .txtStore.Value				          = piktkt.Store
    .pgfPikTkt.pgHeader.txtPO.Value = piktkt.CustPo
  ENDWITH

  This.llSekPak = This.lfSekPak()

  IF This.llSekPak OR This.loFormSet.AriaForm1.cboStatus.Value = "X" OR ;
    This.loFormSet.AriaForm1.cboStatus.Value = "C"
    This.loFormSet.llenablebuttons = .F.
  ELSE
    This.loFormSet.llenablebuttons  = .T.
  ENDIF

  lnCUSRecB = 0
  = THIS.Customer.SEEK('M'+Piktkt.Account)
  This.loFormSet.AriaForm1.txtAccountName.Value = Customer.StName
  IF !EMPTY(ALLTRIM(piktkt.Store))
    lnCUSRecB = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    THIS.Customer.SEEK('S'+Piktkt.Account+piktkt.Store)
  ENDIF

  WITH This.loFormSet.AriaForm1.pgfPikTkt.pgHeader
    *N000592,1 HBG 02/27/2007 Print Store Address or DC Address depnding on the Flag of Dircet To Store in ORDHDR [Begin]
    WITH .cntBillAddress
      STORE '' TO .txtBillAdd1.Value,.txtBillAdd2.Value,.txtBillAdd3.Value,;
                  .txtBillAdd4.Value,.txtBillAdd5.Value,.txtBillName.Value
    ENDWITH

    WITH .cntshipAddress
      STORE '' TO .txtShipAdd1.Value,.txtShipAdd2.Value,.txtShipAdd3.Value,;
                  .txtShipAdd4.Value,.txtShipAdd5.Value,.txtShipName.Value
    ENDWITH
    *N000592,1 HBG [End]
    WITH .cntBillAddress
      .txtBillName.Value = Customer.BtName
      =gfGetAdr('Customer','','','',1,'2')
      FOR lnCount = 1 TO ALEN(laAddress,1)
        lcCount   = STR(laAddress[lnCount,1],1)
        .txtBillAdd&lcCount..Value = .txtBillAdd&lcCount..Value + IIF(EMPTY(.txtBillAdd&lcCount..Value),'',',')+;
        SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
      ENDFOR
    ENDWITH

    =This.ordhdr.seek('O'+Piktkt.Order)
    .DTpComplete.VAlue = ordhdr.Complete
    .DTpEntered.Value  = ordhdr.Entered
    .DTpStart.Value    = ordhdr.Start
    .TXTDepatment.Value= ordhdr.Dept
    .txtNotes1.Value   = ordhdr.Note1
    .txtNotes2.Value   = ordhdr.Note2
    .cboTermCode.Value    = ordhdr.cTermCode
    .cboDivision.Value = ordhdr.cDivision
    .cboSeason.Value   = ordhdr.Season
    *! B609839,1 MMT 02/22/2012 Picking ticket screen displays incorrect Shipvia  in case of multi store order and Shipvia is on store level[Start]
    IF ordhdr.ShipVia = '*' AND !EMPTY(ALLTRIM(piktkt.Store))
      .cboShipVia.Value 	   = Customer.ShipVia
    ELSE
    *! B609839,1 MMT 02/22/2012 Picking ticket screen displays incorrect Shipvia  in case of multi store order and Shipvia is on store level[END]
      .cboShipVia.Value 	   = ordhdr.ShipVia
    *! B609839,1 MMT 02/22/2012 Picking ticket screen displays incorrect Shipvia  in case of multi store order and Shipvia is on store level[Start]
    ENDIF
    *! B609839,1 MMT 02/22/2012 Picking ticket screen displays incorrect Shipvia  in case of multi store order and Shipvia is on store level[END]
    .cboSpcInst.Value 	   = ordhdr.SpcInst
    IF ORDHDR.Alt_ShpTo
      WITH .cntShipAddress
        .txtShipName.Value = ORDHDR.STName
        .txtShipAdd1.Value = ORDHDR.cAddress1
        .txtShipAdd2.Value = ORDHDR.cAddress2
        .txtShipAdd3.Value = ORDHDR.cAddress3
        .txtShipAdd4.Value = ORDHDR.cAddress4
        .txtShipAdd5.Value = ORDHDR.cAddress5
      ENDWITH
    ELSE
      WITH .cntShipAddress
        *N000592,1 HBG 02/27/2007 Print Store Address or DC Address depnding on the Flag of Dircet To Store in ORDHDR [Begin]
        IF !EMPTY(Customer.Dist_Ctr) AND !ORDHDR.lStrDirct
          =THIS.Customer.SEEK("S"+Customer.Account+Customer.Dist_ctr)
        ENDIF
        *N000592,1 HBG [End]
        .txtShipName.Value = IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
        =gfGetAdr('CUSTOMER','','','',1,'')
        FOR lnCount = 1 TO ALEN(laAddress,1)
          lcCount   = STR(laAddress[lnCount,1],1)
          .txtShipAdd&lcCount..Value = .txtShipAdd&lcCount..Value + IIF(EMPTY(.txtShipAdd&lcCount..Value),'',',')+;
          SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
        ENDFOR
      ENDWITH
    ENDIF
    IF lnCUSRecB <> 0
      GOTO lnCUSRecB IN CUSTOMER
    ENDIF
  ENDWITH
  lcDefWare = WAREHOUS.cWareCode
  lcWareCode= IIF(This.loFormSet.llMultWare , PIKTKT.cWareCode, lcDefWare)
  IF  This.WAREHOUS.SEEK(lcWareCode) AND WAREHOUS.lStyInv
    This.loFormSet.AriaForm1.txtLocation.Value =IIF(!EMPTY(WAREHOUS.cDesc),;
    WAREHOUS.cDesc, PADR(WAREHOUS.cWareCode,35))
  ELSE
    This.loFormSet.AriaForm1.txtLocation.Value  = ""
  ENDIF
  This.lfBrowse()
  SELECT(This.loFormSet.lc_tmpordl)
  LOCATE
  IF EOF()
    This.loFormSet.llenbleRemove = .F.
  ELSE
    This.loFormSet.llenbleRemove = .T.
  ENDIF
ENDFUNC
*!*****************************************************************************************
*! Name      : lfSekPak
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 06/30/2005
*! Purpose   : Check if Picking Ticket has packing list or not
*!*****************************************************************************************
FUNCTION lfSekPak

PRIVATE lnCurAlias

*-- if The Pick ticket has a packing list
IF This.PACK_HDR.SEEK(PIKTKT.PIKTKT)
  RETURN .T.
ELSE
  RETURN .F.
ENDIF
ENDFUNC
*!*****************************************************************************************
*! Name      : lfBrowse
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 06/30/2005
*! Purpose   : Get lines of picking ticket
*!*****************************************************************************************
FUNCTION lfBrowse
PRIVATE lnCurAlias, lnCurTag
lnCurAlias = SELECT(0)
SELECT(This.loFormSet.lc_tmpordl)
ZAP
IF PikTkt.Status $ 'CX'
  IF This.PikLine.Seek(PikTkt.PikTkt+PikTkt.ORDER)
	  SELECT PikLine
	  SCAN REST WHILE  PIKTKT+ORDER+STR(LINENO,6)= PIKTKT.PIKTKT+PIKTKT.ORDER
        SCATTER MEMO MEMVAR
		INSERT INTO (This.loFormSet.lc_tmpordl) FROM MEMVAR
		*E303874 MHM Add vendor style column to pick ticket screen
	 IF SEEK(m.style,'style')
      replace cvensty WITH style.cvensty IN (This.loFormSet.lc_tmpordl)
      ENDIF 
		*E303874 MHM Add vendor style column to pick ticket screen
	  ENDSCAN
  ENDIF
ELSE
  IF THIS.ordline.seek("O"+PikTkt.ORDER)
    SELECT OrdLine
    SCAN REST WHILE  CORDTYPE+ORDER+STR(LINENO,6)= "O"+PIKTKT.ORDER FOR PIKTKT = PIKTKT.PIKTKT
      SCATTER MEMVAR MEMO        && if memo fields exist use memo
      m.nRecNo  = RECNO()
      m.cStatus = 'S'
      INSERT INTO (This.loFormSet.lc_tmpordl) FROM MEMVAR
		*E303874 MHM Add vendor style column to pick ticket screen
	 IF SEEK(m.style,'style')
      replace cvensty WITH style.cvensty IN (This.loFormSet.lc_tmpordl)
      ENDIF 
		*E303874 MHM Add vendor style column to pick ticket screen
      lcLineFile = This.loFormSet.lc_tmpordl
      REPLACE &lcLineFile..OldPik1 WITH m.Pik1,;
              &lcLineFile..OldPik2 WITH m.Pik2,;
              &lcLineFile..OldPik3 WITH m.Pik3,;
              &lcLineFile..OldPik4 WITH m.Pik4,;
              &lcLineFile..OldPik5 WITH m.Pik5,;
              &lcLineFile..OldPik6 WITH m.Pik6,;
              &lcLineFile..OldPik7 WITH m.Pik7,;
              &lcLineFile..OldPik8 WITH m.Pik8

     REPLACE  &lcLineFile..Ordln1 WITH m.Qty1,;
              &lcLineFile..Ordln2 WITH m.Qty2,;
              &lcLineFile..Ordln3 WITH m.Qty3,;
              &lcLineFile..Ordln4 WITH m.Qty4,;
              &lcLineFile..Ordln5 WITH m.Qty5,;
              &lcLineFile..Ordln6 WITH m.Qty6,;
              &lcLineFile..Ordln7 WITH m.Qty7,;
              &lcLineFile..Ordln8 WITH m.Qty8
    ENDSCAN
  ENDIF
ENDIF
ENDFUNC
*!*****************************************************************************************
*! Name      : lfGetTotStk
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/04/2005
*! Purpose   : Get lines of picking ticket
*!*****************************************************************************************
FUNCTION lfGetTotStk
lcLineFile = This.loFormSet.lc_tmpordl
=This.stydye.SEEK(&lcLineFile..Style+;
    IIF(PikTkt.Status $ 'CX',PIKTKT.CWARECODE,&lcLineFile..cWareCode)+&lcLineFile..Dyelot)

RETURN     MAX(STYDYE.Stk1-STYDYE.Alo1,0)+;
           MAX(STYDYE.Stk2-STYDYE.Alo2,0)+;
           MAX(STYDYE.Stk3-STYDYE.Alo3,0)+;
           MAX(STYDYE.Stk4-STYDYE.Alo4,0)+;
           MAX(STYDYE.Stk5-STYDYE.Alo5,0)+;
           MAX(STYDYE.Stk6-STYDYE.Alo6,0)+;
           MAX(STYDYE.Stk7-STYDYE.Alo7,0)+;
           MAX(STYDYE.Stk8-STYDYE.Alo8,0)
ENDFUNC
*!*****************************************************************************************
*! Name      : lfGetOpen
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/04/2005
*! Purpose   : Get open amount
*!*****************************************************************************************
FUNCTION lfGetOpen
  RETURN EVALUATE(This.loFormSet.lc_TmpOrdL+'.TotQty') * EVALUATE(This.loFormSet.lc_TmpOrdL+'.Price')
ENDFUNC
*!*****************************************************************************************
*! Name      : lfGetOpen
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/04/2005
*! Purpose   : Get pick amount
*!*****************************************************************************************
FUNCTION lfGetPik
  RETURN EVALUATE(This.loFormSet.lc_TmpOrdL+'.TotPik') * EVALUATE(This.loFormSet.lc_TmpOrdL+'.Price')
ENDFUNC
*!*****************************************************************************************
*! Name      : lfGetOpen
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/04/2005
*! Purpose   : Get pick amount
*!*****************************************************************************************
FUNCTION lfvAccount

LOCAL lcSelAccount
*-- only validate in case of empty(piktkt) and empty(order)
*IF (EMPTY(This.loFormSet.ariaform1.kbPickNo.keytextbox.Value) AND;
   EMPTY(This.loFormSet.AriaForm1.kbOrder.keytextbox.Value )) OR This.loFormSet.AriaForm1.kbAccount.selectedfrombrowse
  lcSelAccount = PADR(ALLTRIM(This.loFormSet.AriaForm1.kbAccount.keytextbox.Value),;
               FSIZE('Account', 'PIKTKT'))
  IF This.loFormSet.AriaForm1.kbAccount.selectedfrombrowse  OR (!EMPTY(lcSelAccount) AND !This.Customer.SEEK('M'+lcSelAccount))

    DO CUSBROWM WITH lcSelAccount
    This.loFormSet.AriaForm1.kbAccount.selectedfrombrowse = .F.
    This.loFormSet.ariaform1.kbaccount.keytextbox.Value = lcSelAccount
  ENDIF
  IF EMPTY(This.loFormSet.ariaform1.kbaccount.keytextbox.Value)
    STORE SPACE(6) TO This.loFormSet.AriaForm1.kbOrder.keytextbox.Value
    RETURN
  ELSE
    *B607811,1 WAM 10/29/2006 Fix bug Index tag not found
*!*	    SELECT PIKTKT
*!*	    lcSeleAccount = PADR(ALLTRIM(This.loFormSet.AriaForm1.kbAccount.keytextbox.Value),;
*!*	              				FSIZE('Account', 'PIKTKT'))
*!*	    This.Piktkt.SqlRun("SELECT * FROM piktkt where PIKTKT.ACCOUNT = '"+lcSeleAccount +"'")
*!*	*    SET FILTER TO ACCOUNT = PADR(ALLTRIM(This.loFormSet.AriaForm1.kbAccount.keytextbox.Value),;
*!*	              				FSIZE('Account', 'PIKTKT'))
*!*		SELECT PIKTKT              				
*!*	    LOCATE
*!*	    IF EOF()
*!*	      *-- 'No piktkts found for account xxxxxx
*!*	      *-- <OK>
*!*	      =gfModalGen("INM44043B00000","Dialog",This.loFormSet.ariaform1.kbaccount.keytextbox.Value)
*!*	      *-- return to select mode/ini. vars.
*!*	      STORE SPACE(05) TO This.loFormSet.ariaform1.kbaccount.keytextbox.Value
*!*	      SET FILTER TO PikTkt <> "******"
*!*	      RETURN
*!*	    ELSE
*!*	      = THIS.Customer.SEEK('M'+This.loFormSet.ariaform1.kbaccount.keytextbox.Value)
*!*	      This.loFormSet.AriaForm1.txtAccountName.Value = Customer.StName
*!*	      SELECT PIKTKT
*!*	      SET FILTER TO PikTkt <> "******"
*!*	    ENDIF
     =THIS.Customer.SEEK('M'+This.loFormSet.ariaform1.kbaccount.keytextbox.Value)
     This.loFormSet.AriaForm1.txtAccountName.Value = Customer.StName
     *B607811,1 WAM 10/29/2006 (End)
   ENDIF

*ENDIF
*!*****************************************************************************************
*! Name      : lfRowColChange
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/04/2005
*! Purpose   : row col change in grid
*!*****************************************************************************************
FUNCTION lfRowColChange
LOCAL  lcAlias
lnReturnScale = ""
lcAlias = ALIAS()
WITH This.loFormSet.ariaform1.pgfPikTkt.pgDetail
  .kbStyle.Value = EVALUATE(This.loFormSet.lc_TmpOrdL+'.Style')
  .kbConfiguration.keytextbox.Value = EVALUATE(This.loFormSet.lc_TmpOrdL+'.Dyelot')
  .txtStyDesc.Value = EVALUATE(This.loFormSet.lc_TmpOrdL+'.Desc1')
  .kbAltStyle.Value = EVALUATE(This.loFormSet.lc_TmpOrdL+'.altstyle')
  .txtAltStyDesc.Value = IIF(!EMPTY(EVALUATE(This.loFormSet.lc_TmpOrdL+'.altstyle'));
  						 AND This.Style.Seek(EVALUATE(This.loFormSet.lc_TmpOrdL+'.altstyle')),;
  						 STYLE.Desc1,"")
  .cntAvail.scale = EVALUATE(This.loFormSet.lc_TmpOrdL+'.Scale')
  lnReturnScale   = IIF(!EMPTY(EVALUATE(This.loFormSet.lc_TmpOrdL+'.Scale')) AND this.scale.seek("S"+EVALUATE(This.loFormSet.lc_TmpOrdL+'.Scale')),scale.cnt,"")
  IF This.stydye.SEEK(EVALUATE(This.loFormSet.lc_TmpOrdL+'.Style')+;
    IIF(PikTkt.Status $ 'CX',PIKTKT.CWARECODE,EVALUATE(This.loFormSet.lc_TmpOrdL+'.CWARECODE'))+EVALUATE(This.loFormSet.lc_TmpOrdL+'.Dyelot'))
    WITH  .cntAvail
       .txtQty1.Value  = MAX(STYDYE.Stk1 - STYDYE.Alo1, 0)
       .txtQty2.Value  = MAX(STYDYE.Stk2 - STYDYE.Alo2, 0)
       .txtQty3.Value  = MAX(STYDYE.Stk3 - STYDYE.Alo3, 0)
       .txtQty4.Value  = MAX(STYDYE.Stk4 - STYDYE.Alo4, 0)
       .txtQty5.Value  = MAX(STYDYE.Stk5 - STYDYE.Alo5, 0)
       .txtQty6.Value  = MAX(STYDYE.Stk6 - STYDYE.Alo6, 0)
       .txtQty7.Value  = MAX(STYDYE.Stk7 - STYDYE.Alo7, 0)
       .txtQty8.Value  = MAX(STYDYE.Stk8 - STYDYE.Alo8, 0)
       .txtTotQty.Value= this.lfGetTotStk()
   ENDWITH
  ENDIF
ENDWITH

 SELECT(lcAlias)
 RETURN  lnReturnScale
ENDFUNC
*!*****************************************************************************************
*! Name      : lfShwGets
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/04/2005
*! Purpose   : Disable allocated qty fields in case of range pack
*!*****************************************************************************************
FUNCTION lfShwGets

IF  This.loFormSet.ActiveMode = "E"
  llRange = .T.
  IF !EMPTY(EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pack_id'))
    IF !(EVALUATE(This.loFormSet.lc_TmpOrdL+'.lRange'))
      This.loFormSet.llenablealloc = .T.
    ELSE
	  This.loFormSet.llenablealloc = .F.
    ENDIF
  ELSE
    This.loFormSet.llenablealloc = .T.
  ENDIF

ENDIF
ENDFUNC
*!*****************************************************************************************
*! Name      : lfvAloQty
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/04/2005
*! Purpose   : validate allocated quantity
*!*****************************************************************************************
FUNCTION lfvAloQty
PARAMETERS lnParm
PRIVATE lcParm , lnCurAlias
lnCurAlias = SELECT(0)
lcParm = ALLTRIM(STR(lnParm))

*If the picked quantities of this pick ticket line are zeros this line should be removed from the pick ticket.
WITH This.loFormSet.AriaForm1.pgfPikTkt.pgDetail
*!*	  IF .cntalloc.txtTotQty.Value = 0
*!*	    This.lfvRemove()
*!*	    RETURN
*!*	  ENDIF
  SELECT (This.loFormSet.lc_TmpOrdL)
  IF .cntalloc.txtQty&lcparm..Value < 0
    .cntalloc.txtQty&lcparm..Value =.cntalloc.txtQty&lcparm..oldValue
  ENDIF
  *T20060818.0001(C200876) TMI [Start]
  LOCAL llContinue
  llContinue = .T.
  IF ASCAN(This.loFormSet.laEvntTrig,PADR("PKALOQTY",10)) <> 0
    PRIVATE loPkTktScr
    loPkTktScr = This.loFormSet
    llContinue = This.loFormSet.mDoTrigger(PADR('PKALOQTY',10))
  ENDIF
  *T20060818.0001(C200876) TMI [End  ]

  *-- If alo. > Ord. .cntOrder
  *T20060818.0001(C200876) TMI [Start]
  *IF .cntalloc.txtQty&lcparm..Value > .cntOrder.txtQty&lcparm..Value
  IF llContinue .AND. .cntalloc.txtQty&lcparm..Value > .cntOrder.txtQty&lcparm..Value
    *T20060818.0001(C200876) TMI [End  ]
    *-- Message : "Quantity allocated is greater than ordered. Increase "
    *--           "ordered quantity?                                    "
    *--           "                < Yes >           < No >             "
    IF gfModalGen('TRM44002B00006','DIALOG') = 1
      .cntOrder.txtTotQty.Value = .cntOrder.txtTotQty.Value -.cntOrder.txtQty&lcparm..Value + .cntalloc.txtQty&lcparm..Value
  	  .cntOrder.txtQty&lcparm..Value = .cntalloc.txtQty&lcparm..Value
    ELSE
      .cntalloc.txtQty&lcparm..Value = .cntalloc.txtQty&lcparm..oldValue
    ENDIF
  ENDIF
  IF .cntalloc.txtQty&lcparm..Value <> .cntalloc.txtQty&lcparm..oldValue
    REPLACE cStatus WITH 'M'
  ENDIF
  lnToTal = 0
  FOR counter = 1 TO 8
    lcCount = ALLTRIM(STR(Counter))
    lnToTal = lnToTal  + .cntalloc.txtQty&lcCount..Value
  ENDFOR
  IF lnToTal = 0
    This.lfvRemove()
    RETURN
  ENDIF
*!*	  IF .cntalloc.txtTotQty.Value = 0
*!*	    This.lfvRemove()
*!*	    RETURN
*!*	  ENDIF

ENDWITH

SELECT (lnCurAlias)
ENDFUNC
*!*****************************************************************************************
*! Name      : lfvRemove
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/05/2005
*! Purpose   : remove lines from details
*!*****************************************************************************************
FUNCTION lfvRemove

LOCAL lnCurAlias
lnCurAlias = SELECT(0)
IF !EMPTY(EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pack_ID'))
  IF !EMPTY(EVALUATE(This.loFormSet.lc_TmpOrdL+'.cPckSize')) AND this.Scale.SEEK('S'+LEFT(EVALUATE(This.loFormSet.lc_TmpOrdL+'.cPckSize'),1))
    lcSiz = RIGHT(EVALUATE(This.loFormSet.lc_TmpOrdL+'.cPckSize'),1)
    lcSize = SCALE.Sz&lcSiz
  ELSE
    lcSize = '*****'
  ENDIF
  *-- 'This line belongs to a pack, do you want to remove the whole pack
  *-- <YES>  <NO>
  IF gfModalGen('QRM00000B40000','ALERT',.F.,.F.,;
                IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Pack_Style,this.loFormSet.GetHeaderText("LANG_Pack_Style",this.loFormSet.HeaderAlias)) + ALLTRIM(EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pack_ID'))+'-'+ALLTRIM(EVALUATE(This.loFormSet.lc_TmpOrdL+'.cPkColor'))+;
                '-'+ALLTRIM(lcSize)+'-'+ALLTRIM(EVALUATE(This.loFormSet.lc_TmpOrdL+'.cPkVersion'))+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Remove_Pack,this.loFormSet.GetHeaderText("LANG_Remove_Pack",this.loFormSet.HeaderAlias))) = 1
    lcPack_ID   = EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pack_ID')
    lcPkColor   = EVALUATE(This.loFormSet.lc_TmpOrdL+'.cPkColor')
    lcPckSize   = EVALUATE(This.loFormSet.lc_TmpOrdL+'.cPckSize')
    lcPkVersion = EVALUATE(This.loFormSet.lc_TmpOrdL+'.cPkVersion')
*    lnCurAlias = SELECT(0)
    SELECT (This.loFormSet.lc_TmpOrdL)
    SCAN FOR Pack_ID+cPkColor+cPckSize+cPkVersion = lcPack_ID+lcPkColor+lcPckSize+lcPkVersion
      REPLACE cStatus WITH 'D'
    ENDSCAN
    DELETE FOR Pack_ID+cPkColor+cPckSize+cPkVersion = lcPack_ID+lcPkColor+lcPckSize+lcPkVersion
    This.loFormSet.llRecdeleted = .T.
    IF !EOF()
       SKIP-1
    ENDIF
    SELECT (lnCurAlias)
  ENDIF
ELSE
  *-- 'are sure do you want to delete this line
  *-- <YES>  <NO>
  *-- Confirm deletion from the user. If he confirms, proceed
 * lnCurAlias = SELECT(0)
  IF gfModalGen('QRM44021B00006','DIALOG')=1
    lnCurAlias = SELECT(0)
    SELECT (This.loFormSet.lc_TmpOrdL)
    REPLACE cStatus WITH 'D'
    DELETE
    This.loFormSet.llRecDeleted = .T.
    IF !EOF()
      SKIP-1
    ENDIF
    SELECT (lnCurAlias)
  ENDIF
ENDIF
SELECT (This.loFormSet.lc_TmpOrdL)
lnRecNo = RECNO()
LOCATE
IF EOF()
  This.loFormSet.llenbleRemove = .F.
ELSE
  This.loFormSet.llenbleRemove = .T.
ENDIF
IF BETWEEN(lnRecNo ,1,RECCOUNT())
 GOTO lnRecNo
ENDIF

SELECT (lnCurAlias)
ENDFUNC
*!*****************************************************************************************
*! Name      : lfDeletePik
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/05/2005
*! Purpose   : to release Picking Ticket
*!*****************************************************************************************
FUNCTION lfDeletePik
PRIVATE lnCurAlias

*!*	IF ASCAN(This.loFormSet.laEvntTrig , PADR('NOCHAGMSG',10)) <> 0 AND PIKTKT.cPickType = 'A'
*!*	  llRelPik = .T.
*!*	  *--Only Blank pick ticket can be released, the Adornment pickets should be released throw the Release Pick Tickets screen.
*!*	  =gfDoTriger('ALPKTKT',PADR('NOCHAGMSG',10))
*!*	  RETURN
*!*	ENDIF
*-- 'ARE SURE THAT YOU WANT TO RELEASE THIS PIKTKT
*-- <yes>    <NO>
IF gfModalGen("QRM44023B00006" , "DIALOG") = 1
  lnCurAlias = SELECT(0)
  SELECT PIKTKT
*  IF  gfObj_Lock(.T.)
    *-- two reasons make us delete from ordline not (lc_tmpordl)
    *-- one : we may modify the alo. qty. in (lc_tmpordl) for a line
    *-- then release this line . so we must release from the original in ordline
    *-- two : we may release the picktkt in view mode (whereas the (lc_tmpordl)
    *-- is not collected. so we must release from the original in ordline   .
    *-- set relation is accomplished already (only one time)
    *-- cWareCode of (lc_TmpOrdL) (which is equivalent to ordline)
    *-- this is if changes allocated qtys. ' warehouse (in allocation)
    *-- affects stydye's warehouse in terms of lines' warehouse =.t.
    This.Ordline.seek('O'+ This.loFormSet.AriaForm1.kbOrder.keytextbox.Value)
    SELECT ORDLINE
    *-- Scan records of the picking ticket in ORDLINE (no need for go top)
    *-- for now the sequence of the piktkt is not correct.
    *-- so for now we'll add the order to the equity cond.
    SCAN REST WHILE  cOrdType + ORDER  + STR(LineNo,6) ='O'+ This.loFormSet.AriaForm1.kbOrder.keytextbox.Value ;
       FOR PikTkt = This.loFormSet.ariaform1.kbPickNo.keytextbox.Value
      *-- saving in pikline
      SCATTER MEMVAR MEMO

      this.PIKLINE.INSERT("FROM MEMVAR")
      =gfAdd_Info('PIKLINE')
*      This.lfAdd_Info(This.Pikline,'PIKLINE')&& For edit info.
      *-- when affecting/saving in ordline, style, stydye we must chk. the
      *-- existence of altstyle ..
      *-- if we have altstyle in the ordline . in this case if we release
      *-- this line , then we must do the following :
      *-- assume the altsyle(original) is x and the (new/replacement) style is y
      *-- 1: in the ordline , move altstyle (orignal style) to be the style
      *--    and  clear the altsyle fld.
      *-- 2: in the style (and stydye) move ordered qty from y to x
      *--    and clear pik. qty. for y .
      *-- before changing anything in style and stydye we must chk.
      *-- whether the scale of the style (y) fit/less than/greater than
      *-- the altstyle(x) . if equal or less then move directly .
      *-- if greater then move all the correspondent scales only and ignore
      *-- the rest and adjust ordhdr new ordered qtys.
*!*         IF ASCAN(This.loFormSet.laEvntTrig,PADR("ALRELORD",10)) <> 0
*!*           = gfDoTriger("ALPKTKT",PADR("ALRELORD",10))
*!*         ENDIF
     *T20060818.0001(C200876) TMI [Start]
     IF ASCAN(This.loFormSet.laEvntTrig,PADR("ALRELORD",10)) <> 0
       This.loFormSet.mDoTrigger(PADR('ALRELORD',10))
     ENDIF
     *T20060818.0001(C200876) TMI [End  ]

      =This.ordhdr.Seek('O'+Piktkt.Order)
      SELECT ORDLINE
*!*	      IF !EMPTY(m.AltStyle)       && adjust scale and move ordered qtys.
*!*	        THIS.lpSwchSty(m.Style,m.dyelot, m.AltStyle, m.cWareCode, ORDHDR.cWareCode)
*!*	      ENDIF
      SELECT ORDLINE
      THIS.ORDLINE.REPLACE('Pik1    WITH 0   ,;
    	                    Pik2    WITH 0   ,;
   				            Pik3    WITH 0   ,;
                            Pik4    WITH 0   ,;
                            Pik5    WITH 0   ,;
                            Pik6    WITH 0   ')

      THIS.ORDLINE.REPLACE('Pik7    WITH 0   ,;
                            Pik8    WITH 0   ,;
                            TotPik  WITH 0   ,;
                            PikTkt  WITH " " ,;
                            Picked  WITH .F. ,;
                            PikDate WITH {}')
      =gfAdd_Info('ORDLINE')
*      This.lfAdd_Info(This.ORDLINE,'ORDLINE')

      *-- saving in style (only one rec. carrying the alo. qtys.)
      SELECT STYLE

      *=This.Style.Seek(m.style)
      =This.Style.Seek(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle))
      =RLOCK()
      THIS.STYLE.REPLACE('Alo1 WITH MAX(Alo1 - m.Pik1, 0),;
                          Alo2 WITH MAX(Alo2 - m.Pik2, 0),;
                          Alo3 WITH MAX(Alo3 - m.Pik3, 0),;
                          Alo4 WITH MAX(Alo4 - m.Pik4, 0)')

      THIS.STYLE.REPLACE('Alo5 WITH MAX(Alo5 - m.Pik5, 0),;
                          Alo6 WITH MAX(Alo6 - m.Pik6, 0),;
                          Alo7 WITH MAX(Alo7 - m.Pik7, 0),;
                          Alo8 WITH MAX(Alo8 - m.Pik8, 0)')

      THIS.STYLE.REPLACE('TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')
      =gfAdd_Info('STYLE')
*      This.lfAdd_Info(This.STYLE,'STYLE')
      UNLOCK
      *-- saving in stydye (for every rec. in ordline we have - generally -
      *-- two recs. representing it : one for the entire style+cwarecode
      *-- and the other for style+cwarecode+dyelot (if no dyelot then only one))
      *--&& Repl. for all corresponding recs.
*      IF This.STYDYE.SEEK(m.Style + m.CWARECODE + m.dyelot)
      IF This.STYDYE.SEEK(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle) + m.CWARECODE + m.dyelot)
        SELECT STYDYE
        =RLOCK()
        THIS.STYDYE.REPLACE('Alo1 WITH MAX( Alo1 - m.Pik1, 0 ),;
                             Alo2 WITH MAX( Alo2 - m.Pik2, 0 ),;
                             Alo3 WITH MAX( Alo3 - m.Pik3, 0 ),;
                             Alo4 WITH MAX( Alo4 - m.Pik4, 0 )')

        THIS.STYDYE.REPLACE('Alo5 WITH MAX( Alo5 - m.Pik5, 0 ),;
                             Alo6 WITH MAX( Alo6 - m.Pik6, 0 ),;
                             Alo7 WITH MAX( Alo7 - m.Pik7, 0 ),;
                             Alo8 WITH MAX( Alo8 - m.Pik8, 0 )')

        THIS.STYDYE.REPLACE('TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')
        =gfAdd_Info('STYDYE')
*        This.lfAdd_Info(This.STYDYE,'STYDYE')
        UNLOCK
      ENDIF
      IF This.loFormSet.llUseDyes .AND. STYLE.cDye_Flg = 'Y'

  *      IF THIS.STYDYE.SEEK(m.Style + m.cWareCode + m.Dyelot)
        IF THIS.STYDYE.SEEK(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle)+ m.cWareCode + m.Dyelot)
          SELECT STYDYE
          =RLOCK()
          THIS.STYDYE.REPLACE('Alo1 WITH MAX( Alo1 - m.Pik1, 0 ),;
                               Alo2 WITH MAX( Alo2 - m.Pik2, 0 ),;
                               Alo3 WITH MAX( Alo3 - m.Pik3, 0 ),;
                               Alo4 WITH MAX( Alo4 - m.Pik4, 0 )')

          THIS.STYDYE.REPLACE('Alo5 WITH MAX( Alo5 - m.Pik5, 0 ),;
                               Alo6 WITH MAX( Alo6 - m.Pik6, 0 ),;
                               Alo7 WITH MAX( Alo7 - m.Pik7, 0 ),;
                               Alo8 WITH MAX( Alo8 - m.Pik8, 0 )')

          THIS.STYDYE.REPLACE(' TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')
          =gfAdd_Info('STYDYE')
*          This.lfAdd_Info(This.STYDYE,'STYDYE')
          UNLOCK
        ENDIF
      ENDIF
      *-- restoring original style (altstyle) to be the style
      *-- in ordline and clearing the altstyle fld.
      SELECT ORDLINE
      IF !EMPTY(m.AltStyle)
*        lcNewScale = IIF(THIS.STYLE.SEEK(m.Style) AND THIS.SCALE.SEEK('S'+STYLE.SCALE),SCALE.Scale, ' ')
*        THIS.ORDLINE.REPLACE("Style        WITH AltStyle  ,;
                             AltStyle     WITH SPACE(19) ,;
                             Scale        WITH lcNewScale")

        THIS.ORDLINE.REPLACE("AltStyle     WITH SPACE(19) ")
      ENDIF


     *B611624,1 Es 08/30/2018 Modify the issue of warehouse mismatching  between Ordhdr and ORDLINE"  [Start]
	 IF (ORDHDR.cWARECODE <> ORDLINE.cWareCode)
       THIS.ORDLINE.REPLACE("ORDLINE.cWareCode WITH '"+ORDHDR.cWARECODE+"'")
	 ENDIF 
	*B611624,1 Es 08/30/2018 Modify the issue of warehouse mismatching  between Ordhdr and ORDLINE"  [End]
 
      
    ENDSCAN
    *-- saving in piktkt
    SELECT PIKTKT
    THIS.PIKTKT.REPLACE('Status WITH "X"')
    =gfAdd_Info('PIKTKT')
*    This.lfAdd_Info(This.PIKTKT,'PIKTKT')

    *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
    IF ASCAN(This.loFormSet.laEvntTrig,PADR('CANPCKPACK',10),1,ALEN(This.loFormSet.laEvntTrig,1),1) > 0
       =This.loFormSet.mDoTrigger(PADR('CANPCKPACK',10))
    ENDIF
    *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
    *-- either release while being in edit (i.e. remove all lines),
    *-- or release while in view mode .
    *-- if in case 1 return to view mode .
    *-- if in case 2 return to select mode . (!!!)
    SELECT PIKTKT
*    =gfObj_Lock(.F.)

 * ENDIF
  DIMENSION laTableUpdate[8]
  laTableUpdate[1] = This.Style
  laTableUpdate[2] = This.Ordline
  laTableUpdate[3] = This.piktkt
  laTableUpdate[4] = This.pikline
  laTableUpdate[5] = This.stydye
  laTableUpdate[6] = This.ICSTYHST
  laTableUpdate[7] = This.ARCUSHST
  laTableUpdate[8] = This.ordhdr
  =This.lfTableUpdate()
  SELECT(lnCurAlias)
  RETURN .T.
ELSE
  RETURN .F.
ENDIF
ENDFUNC
*!*************************************************************
*! Name      : lpSwchSty
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/10/2005
*! Purpose   : Switch ordered quantities between a style + warehouse
*!             and another style + warehouse
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: lcFrmSty  : From style  (new/replacement style)
*!             lcToSty   : To Style    (original style)
*!             lcFrmWare : From warehouse (warehouse of replacement)
*!             lcToWare  : To warehouse   (warehouse of original)
*!*************************************************************
*! Returns   :  .T. if switching is successful
*!*************************************************************
*! Example   :  =lpSwchSty(m.Style, m.cWareCode, ;
*!                         m.AltStyle, m.cWareCode)
*!*************************************************************
PROCEDURE lpSwchSty
PARAMETERS lcFrmStyle,lcDyelot, lcToStyle, lcFrmWare, lcToWare
PRIVATE lcSize, lnSize, lnFrmCnt, lnToCnt, lnCurAlias, lnCanc, lnCancAmt
lnCurAlias = SELECT(0)
*-- Get the scale of the target style
IF This.Style.SEEK(lcToStyle) .AND. This.Scale.SEEK('S'+STYLE.Scale)
  lnToCnt   = SCALE.Cnt
  m.Scale   = SCALE.Scale
  This.Style.SEEK(lcFrmStyle)
  lnFrmCnt   = SCALE.Cnt     && count of current scale  && to be altered to the original

  *-- If the scale is greater, cancel any extra ordered amounts
  STORE 0 TO lnCanc, lnCancAmt
  IF lnFrmCnt > lnToCnt
    *-- Cancel any extra ordered amounts in ORDLINE
    *-- N.B. we must drop all ordered qtys. (not only the extra)
    *-- from style and stydye for the style which is to be changed
    SELECT ORDLINE
    FOR lnSize = lnToCnt + 1 TO lnFrmCnt
      lcSize    = STR(lnSize, 1)
      lnQty     = ORDLINE.Qty&lcSize
      lnCanc    = lnCanc    + lnQty
      lnCancAmt = lnCancAmt + lnQty * ORDLINE.Price
      m.Qty&lcSize  = 0
      THIS.ORDLINE.REPLACE ('Qty&lcSize  WITH 0,;
                   		       TotQty      WITH TotQty      - lnQty,;
				                     Book&lcSize WITH Book&lcSize - lnQty,;
			                       TotBook     WITH TotBook     - lnQty')

      *-- Cancel any extra ordered amounts in STYDYE
      = This.StyDye.Seek(Style +cWareCode+dyelot)
      SELECT STYDYE
      THIS.STYDYE.REPLACE("Ord&lcSize WITH MAX(Ord&lcSize - lnQty, 0),;
                           TotOrd     WITH TotOrd - Ord&lcSize ")
	
      *-- we have the stydye consists of each style qtys. per
      *-- warehouse and if the co. uses dyelots and the style uses
      *-- dyelots then we'll have multi records per style representing
      *-- (per one warehouse) each dyelot qtys. plus a record representing
      *-- the total qtys. of this style of these dyelots of this warehouse.
      *-- there's a rule states that : if a style in the order uses dyelots
      *-- then we CAN NOT change/alternate this style .
      *-- so the updating in stydye for alternative style
      *-- always applied on a style that does not use dyelots .
      *-- meaning only one record in stydye.

      *-- Cancel any extra ordered amounts in STYLE
      SELECT STYLE
      THIS.STYLE.REPLACE("Ord&lcSize WITH MAX(Ord&lcSize - lnQty, 0),;
                          TotOrd     WITH TotOrd - Ord&lcSize")
    ENDFOR
  ENDIF

  *-- if the scales are not matched (meaning scale count
  *-- of style (from style) is less than or greater than
  *-- the scale count of altstyle (to style) ,
  *-- don't we need to rescatter ordline before moving qtys. ????

  THIS.lpMoveOrd(lcFrmStyle, lcToStyle, lcFrmWare, lcToWare,lcDyelot)

  *-- Update the ordered quantities in ARCUSHST (customer history)
  *-- style and its ordered quantities
  IF lnCanc <> 0
    *-- Cancel any extra ordered amounts in ORDHDR
    SELECT ORDHDR               && chk. the correct rec.
    THIS.ORDHDR.REPLACE('Open      WITH Open    - lnCanc ,;
                         OpenAmt   WITH OpenAmt - lnCancAmt ,;
                         Book      WITH Book    - lnCanc,;
                         BookAmt   WITH BookAmt - lnCancAmt')


    IF THIS.ARCUSHST.SEEK(m.Account + lcGlYear)
      SELECT ARCUSHST
      THIS.ARCUSHST.REPLACE("nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod ;
                                              			 -ORDLINE.TotQty + m.TotQty,;
                             nOrdQty            WITH nOrdQty            ;
        						 			  -ORDLINE.TotQty + m.TotQty,;
                             nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod -;
				                                             (ORDLINE.TotQty - m.TotQty) * ORDLINE.Price,;
                             nOrdAmt            WITH nOrdAmt            -;
	                              							      (ORDLINE.TotQty - m.TotQty) * ORDLINE.Price")
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpMoveOrd
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/10/2005
*! Purpose   : Move ordered qtys. from a style + warehouse
*!             and another style + warehouse
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: lcFrmSty  : From style
*!             lcToSty   : To Style
*!             lcFrmWare : From warehouse
*!             lcToWare  : To warehouse
*!*************************************************************
*! Returns   :  .T. if switching is successful
*!*************************************************************
*! Example   :  =lpMoveOrd(m.Style, m.cWareCode, ;
*!                         m.AltStyle, m.cWareCode)
*!*************************************************************
PROCEDURE lpMoveOrd
PARAMETERS lcFrmSty, lcToSty, lcFrmWare, lcToWare, lcFrmDye, lcToDye
PRIVATE lnCurAlias, llRetVal, llSavCont

lnCurAlias = SELECT(0)
llRetVal   =.F.

*-- If using dyelots, attempt to lock
*-- altstyle is not allowed for co. which use dyelots
IF This.loFormSet.llUseDyes .AND. STYLE.cDye_flg = 'Y'
  llSavCont = This.StyDye.SEEK(lcToSty + lcToWare  + lcToDye) ;
             .AND. This.StyDye.SEEK(lcFrmSty + lcFrmWare + lcFrmDye) &&AND gfObj_Lock(.T.)
ELSE
  llSavCont = .T.   && in case of moving ordered qtys. for altstyle.
                    && meaning only one rec. in stydye to be updated per style.
ENDIF
IF llSavCont
  IF This.StyDye.SEEK(lcToSty + lcToWare  +  SPACE(10))  ;
    .AND. This.StyDye.SEEK(lcFrmSty + lcFrmWare + lcFrmDye) &&AND gfObj_Lock(.T.)
    *-- Get the year/period of ORDHDR entered date so as to update
    *-- style history.
    IF CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod ','',.T.)
    *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][B
    IF !This.ICSTYHST.SEEK(lcFrmSty + lcGlYear)

      =gfOpenTable('ICSTYHST','STYHST','SH','ICSTYHSX')
      SELECT ICSTYHSX

      APPEND BLANK
      Replace Style WITH lcFrmSty
      Replace CFISFYEAR  WITH lcGlYear
      = GFREPLACE('')
      =gfTABLEUPDATE()

      *USE IN ICSTYHSX
      =gfcloseTable('ICSTYHSX')
      SELECT ICSTYHST
    ENDIF

    IF !This.ICSTYHST.SEEK(lcToSty + lcGlYear)
      =gfOpenTable('ICSTYHST','STYHST','SH','ICSTYHSX')
      SELECT ICSTYHSX

      APPEND BLANK
      Replace Style WITH lcToSty
      Replace CFISFYEAR  WITH lcGlYear
      = GFREPLACE('')
      =gfTABLEUPDATE()

      *USE IN ICSTYHSX
      =gfcloseTable('ICSTYHSX')
      SELECT ICSTYHST
    ENDIF
    *! B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][E
      IF This.ICSTYHST.SEEK(lcToSty + lcGlYear) ;
         .AND.  This.ICSTYHST.SEEK(lcFrmSty + lcGlYear) &&AND gfObj_Lock(.T.)
        IF This.Style.SEEK(lcToSty) ;
          .AND. This.Style.SEEK(lcFrmSty)&& AND gfObj_Lock(.T.)
          SELECT STYLE
          *-- it is already deducted for exess scales.
          THIS.STYLE.REPLACE('Ord1   WITH MAX(Ord1 - m.Qty1, 0),;
                  Ord2   WITH MAX(Ord2 - m.Qty2, 0),;
                  Ord3   WITH MAX(Ord3 - m.Qty3, 0),;
                  Ord4   WITH MAX(Ord4 - m.Qty4, 0)')

          THIS.STYLE.REPLACE('Ord5   WITH MAX(Ord5 - m.Qty5, 0),;
                  Ord6   WITH MAX(Ord6 - m.Qty6, 0),;
                  Ord7   WITH MAX(Ord7 - m.Qty7, 0),;
                  Ord8   WITH MAX(Ord8 - m.Qty8, 0)')

          THIS.STYLE.REPLACE('TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8')

          SELECT STYDYE
          THIS.STYDYE.REPLACE('Ord1   WITH MAX(Ord1 - m.Qty1, 0),;
                               Ord2   WITH MAX(Ord2 - m.Qty2, 0),;
                               Ord3   WITH MAX(Ord3 - m.Qty3, 0),;
                               Ord4   WITH MAX(Ord4 - m.Qty4, 0)')

          THIS.STYDYE.REPLACE('Ord5   WITH MAX(Ord5 - m.Qty5, 0),;
                               Ord6   WITH MAX(Ord6 - m.Qty6, 0),;
                               Ord7   WITH MAX(Ord7 - m.Qty7, 0),;
                               Ord8   WITH MAX(Ord8 - m.Qty8, 0)')

           THIS.STYDYE.REPLACE('TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8')

          SELECT ICSTYHST
          THIS.ICSTYHST.REPLACE('nOrdQty&lcGlPeriod WITH MAX(nOrdQty&lcGlPeriod - m.TotQty, 0),;
                                 nOrdQty            WITH MAX(nOrdQty            - m.TotQty, 0),;
                                 nOrdAmt&lcGlPeriod WITH MAX(nOrdAmt&lcGlPeriod - m.TotQty * ORDLINE.Price, 0) ,;
                                 nOrdAmt            WITH nOrdAmt                - MAX(nOrdAmt&lcGlPeriod - m.TotQty * ORDLINE.Price, 0)')

          *-- incrementing ord. qtys. must be with correct vals.
          *-- rescatter ordline might be here

          IF This.Style.SEEK(lcToSty)
            SELECT STYLE
            THIS.STYLE.REPLACE('Ord1   WITH Ord1 + m.Qty1,;
                                Ord2   WITH Ord2 + m.Qty2,;
                                Ord3   WITH Ord3 + m.Qty3,;
                                Ord4   WITH Ord4 + m.Qty4')
            THIS.STYLE.REPLACE('Ord5   WITH Ord5 + m.Qty5,;
                                Ord6   WITH Ord6 + m.Qty6,;
                                Ord7   WITH Ord7 + m.Qty7,;
                                Ord8   WITH Ord8 + m.Qty8')
            THIS.STYLE.REPLACE('TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8')

          ENDIF
          SELECT STYDYE
          IF SEEK(lcToSty + lcToWare  + SPACE(10))
            THIS.STYDYE.REPLACE('Ord1   WITH Ord1 + m.Qty1,;
                                 Ord2   WITH Ord2 + m.Qty2,;
                                 Ord3   WITH Ord3 + m.Qty3,;
                                 Ord4   WITH Ord4 + m.Qty4')
            THIS.STYDYE.REPLACE('Ord5   WITH Ord5 + m.Qty5,;
                                 Ord6   WITH Ord6 + m.Qty6,;
                                 Ord7   WITH Ord7 + m.Qty7,;
                                 Ord8   WITH Ord8 + m.Qty8')
            THIS.STYDYE.REPLACE('TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8')

          ENDIF

          IF This.ICSTYHST.SEEK(lcToSty + lcGlYear)
            SELECT ICSTYHST
            THIS.ICSTYHST.REPLACE('nOrdQty&lcGlPeriod WITH MAX(nOrdQty&lcGlPeriod - m.TotQty, 0),;
                                   nOrdQty            WITH MAX(nOrdQty            - m.TotQty, 0),;
                                   nOrdAmt&lcGlPeriod WITH MAX(nOrdAmt&lcGlPeriod - m.TotQty * ORDLINE.Price, 0) ,;
                                   nOrdAmt            WITH nOrdAmt                - MAX(nOrdAmt&lcGlPeriod - m.TotQty * ORDLINE.Price, 0)')
          ENDIF
          llRetVal   =.T.
        ENDIF &&ENDIF SEEK(..,'STYLE')
        =THIS.STYLE.SEEK(lcFrmSty) &&.AND. gfObj_Lock(.F.)
        =THIS.STYLE.SEEK(lcToSty) &&.AND. gfObj_Lock(.F.)
      ENDIF  &&ENDIF SEEK(..,'ICSTYHST')
      =THIS.ICSTYHST.SEEK(lcFrmSty + lcGlYear) &&.AND. gfObj_Lock(.F.)
      =THIS.ICSTYHST.SEEK(lcToSty) &&.AND. gfObj_Lock(.F.)
    ENDIF  &&ENDIF CHECKPRD
    *-- Unlock all records
    =THIS.STYDYE.SEEK(lcFrmSty + lcFrmWare + lcFrmDye) &&.AND. gfObj_Lock(.F.)
    =THIS.STYDYE.SEEK(lcToSty + lcToWare  + SPACE(10)) &&.AND. gfObj_Lock(.F.)
  ENDIF
ELSE
  =THIS.STYDYE.SEEK(lcToSty + lcToWare  + lcToDye) &&.AND. gfObj_Lock(.F.)
  =THIS.STYDYE.SEEK(lcFrmSty + lcFrmWare + lcFrmDye) &&.AND. gfObj_Lock(.F.)
ENDIF
SELECT (lnCurAlias)
RETURN llRetVal
*!*************************************************************
*! Name      : lfSavScr
*! Developer : Mariam [MMT]
*! Date      : 07/05/2005
*! Purpose   : Save changes done in screen
*!*************************************************************
FUNCTION lfSavScr
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT PIKTKT
REPLACE llok_stat WITH .F.

lcSetDel = SET('DELETED')
SET DELETED ON
SELECT(This.loFormSet.lc_TmpOrdL)
LOCATE
IF EOF() &&Means all lines are deleted
  This.lfDeletePik()
ELSE
  *-- relations are already set above
  This.OrdLine.Seek("O"+PikTkt.ORDER)
  SELECT ORDLINE
  SELECT(This.loFormSet.lc_TmpOrdL)
  SET DELETED OFF

  PRIVATE lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif , llHaveDiff , lnOrgBook , lnOrgQty
  STORE 0 TO lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif, lnOrgBook , lnOrgQty
  llHaveDiff = .F.  && .T. if differences encounterd
  llPartial = .F.
  SCAN
    IF This.OrdLine.Seek(EVALUATE(This.loFormSet.lc_TmpOrdL+'.cOrdType') + EVALUATE(This.loFormSet.lc_TmpOrdL+'.ORDER') + STR(EVALUATE(This.loFormSet.lc_TmpOrdL+'.LineNo'),6))
      lnOrgBook = OrdLine.ToTBook
      lnOrgQty  = OrdLine.TotQty
    ENDIF
    IF !llPartial .AND. (EVALUATE(This.loFormSet.lc_TmpOrdL+'.cStatus') <> 'D' .AND. ;
	    OrdLine.TotPik > EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pik1')+ ;
	    EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pik2')+ ;
	    EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pik3')+;
	    EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pik4')+;
	    EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pik5')+;
	    EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pik6')+;
	    EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pik7')+;
	    EVALUATE(This.loFormSet.lc_TmpOrdL+'.Pik8'))
      llPartial = .T.
    ENDIF
    *-- chk. if the rec. status is 'D' , 'M' or 'S'
    DO CASE
      CASE cStatus = 'D'
        SCATTER MEMVAR
        *-- saving in ORDLINE
*!*	        IF ASCAN(This.loFormSet.laEvntTrig,PADR("ALRELORD",10)) <> 0
*!*	          = gfDoTriger("ALPKTKT",PADR("ALRELORD",10))
*!*	        ENDIF

        *T20060818.0001(C200876) TMI [Start]
        IF ASCAN(This.loFormSet.laEvntTrig,PADR("ALRELORD",10)) <> 0
          This.loFormSet.mDoTrigger(PADR('ALRELORD',10))
        ENDIF
        *T20060818.0001(C200876) TMI [End  ]

        =This.ordhdr.Seek('O'+Piktkt.Order)
        SELECT ORDLINE

*!*	        IF !EMPTY(m.AltStyle)       && adjust scale and move ordered qtys.
*!*	           THIS.lpSwchSty(m.Style,m.dyelot, m.AltStyle, m.cWareCode, ORDHDR.cWareCode)
*!*	        ENDIF

        IF this.OrdLine.SEEK(m.cOrdType + m.Order  + STR(m.LineNo,6)) AND DELETED() = .F.
          =RLOCK()
          THIS.ORDLINE.REPLACE('Pik1    WITH 0   ,;
                                Pik2    WITH 0   ,;
                                Pik3    WITH 0   ,;
                                Pik4    WITH 0   ')

          THIS.ORDLINE.REPLACE('Pik5    WITH 0   ,;
                                Pik6    WITH 0   ,;
                                Pik7    WITH 0   ,;
                                Pik8    WITH 0   ')

          this.OrdLine.REPLACE('PikTkt  WITH " " ,;
                                TotPik  WITH 0   ,;
                                Picked  WITH .F. ,;
                                PikDate WITH {}')
		  =gfAdd_Info('ORDLINE')
*          This.lfAdd_Info(This.ORDLINE,'ORDLINE')
          UNLOCK
        ENDIF
        *-- saving in style
       * =THIS.STYLE.SEEK(m.style)
        =THIS.STYLE.SEEK(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle))

        SELECT STYLE
        =RLOCK()
        THIS.STYLE.REPLACE('Alo1 WITH MAX(Alo1 - m.Pik1, 0),;
                            Alo2 WITH MAX(Alo2 - m.Pik2, 0),;
                            Alo3 WITH MAX(Alo3 - m.Pik3, 0),;
                            Alo4 WITH MAX(Alo4 - m.Pik4, 0)')
        THIS.STYLE.REPLACE('Alo5 WITH MAX(Alo5 - m.Pik5, 0),;
                            Alo6 WITH MAX(Alo6 - m.Pik6, 0),;
                            Alo7 WITH MAX(Alo7 - m.Pik7, 0),;
                            Alo8 WITH MAX(Alo8 - m.Pik8, 0)')
        THIS.STYLE.REPLACE('TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')
		=gfAdd_Info('STYLE')
*        This.lfAdd_Info(This.STYLE,'STYLE')
        UNLOCK

        *-- saving in stydye (for every rec. in ordline we have - generally -
        *-- two recs. representing it : one for the entire style+cwarecode
        *-- and the other for style+cwarecode+dyelot (if no dyelot then only one))
*        IF THIS.STYDYE.SEEK(m.Style + m.cWareCode+m.dyelot)
        IF THIS.STYDYE.SEEK(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle) + m.cWareCode+m.dyelot)
          SELECT STYDYE      && Repl. for all corresponding recs.
          =RLOCK()
          THIS.STYDYE.REPLACE('Alo1 WITH MAX( Alo1 - m.Pik1, 0 ),;
                  Alo2 WITH MAX( Alo2 - m.Pik2, 0 ),;
                  Alo3 WITH MAX( Alo3 - m.Pik3, 0 ),;
                  Alo4 WITH MAX( Alo4 - m.Pik4, 0 )')

          THIS.STYDYE.REPLACE('Alo5 WITH MAX( Alo5 - m.Pik5, 0 ),;
                  Alo6 WITH MAX( Alo6 - m.Pik6, 0 ),;
                  Alo7 WITH MAX( Alo7 - m.Pik7, 0 ),;
                  Alo8 WITH MAX( Alo8 - m.Pik8, 0 )')

          THIS.STYDYE.REPLACE('TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')
    		=gfAdd_Info('STYDYE')
*          This.lfAdd_Info(This.STYDYE,'STYDYE')
          UNLOCK
        ENDIF
        IF STYLE.cDye_Flg = 'Y'
          *IF THIS.STYDYE.SEEK(m.Style + m.cWareCode + m.Dyelot)
          IF THIS.STYDYE.SEEK(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle) + m.cWareCode + m.Dyelot)
            SELECT STYDYE      && Repl. for all corresponding recs.
            =RLOCK()
            THIS.STYDYE.REPLACE('Alo1 WITH MAX( Alo1 - m.Pik1, 0 ),;
                                 Alo2 WITH MAX( Alo2 - m.Pik2, 0 ),;
                                 Alo3 WITH MAX( Alo3 - m.Pik3, 0 ),;
                                 Alo4 WITH MAX( Alo4 - m.Pik4, 0 )')

            THIS.STYDYE.REPLACE('Alo5 WITH MAX( Alo5 - m.Pik5, 0 ),;
                                 Alo6 WITH MAX( Alo6 - m.Pik6, 0 ),;
                                 Alo7 WITH MAX( Alo7 - m.Pik7, 0 ),;
                                 Alo8 WITH MAX( Alo8 - m.Pik8, 0 )')

            THIS.STYDYE.REPLACE('TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')
       		=gfAdd_Info('STYDYE')
*            This.lfAdd_Info(This.STYDYE,'STYDYE')
            UNLOCK
          ENDIF
        ENDIF
        *-- restoring original style (altstyle) to be the style
        *-- in ordline and clearing the altstyle fld.
        SELECT ORDLINE
        IF !EMPTY(m.AltStyle)
           lcNScale =IIF(THIS.STYLE.SEEK(m.Style) AND THIS.SCALE.SEEK('S'+STYLE.SCALE), SCALE.Scale, ' ')
*           THIS.ORDLINE.REPLACE("Style        WITH AltStyle  ,;
                                 AltStyle     WITH SPACE(19) ,;
                                 Scale        WITH lcNScale")

		   THIS.ORDLINE.REPLACE("AltStyle     WITH SPACE(19) ")

        ENDIF

      CASE cStatus = 'M'
        SCATTER MEMVAR
        *-- saving in ORDLINE
        *-- we have to encounter also if pik > ord , so modify ord. qty. too

        IF THIS.ORDLINE.SEEK(m.cOrdType + m.Order  + STR(m.LineNo,6)) AND DELETED() = .F.
          SELECT ORDLINE
          =RLOCK()
          THIS.ORDLINE.REPLACE('Pik1    WITH m.Pik1 ,;
                                Pik2    WITH m.Pik2 ,;
                                Pik3    WITH m.Pik3 ,;
                                Pik4    WITH m.Pik4 ')

          THIS.ORDLINE.REPLACE('Pik5    WITH m.Pik5 ,;
                                Pik6    WITH m.Pik6 ,;
                                Pik7    WITH m.Pik7 ,;
                                Pik8    WITH m.Pik8 ')
          THIS.ORDLINE.REPLACE('TotPik  WITH m.Pik1+m.Pik2+m.Pik3+m.Pik4+m.Pik5+m.Pik6+m.Pik7+m.Pik8')

          THIS.ORDLINE.REPLACE('Book1    WITH IIF(Qty1 < m.Pik1,Book1-Qty1+m.Pik1,Book1),;
                                        Book2    WITH IIF(Qty2 < m.Pik2,Book2-Qty2+m.Pik2,Book2)')

          THIS.ORDLINE.REPLACE('Book3    WITH IIF(Qty3 < m.Pik3,Book3-Qty3+m.Pik3,Book3),;
                                        Book4    WITH IIF(Qty4 < m.Pik4,Book4-Qty4+m.Pik4,Book4)')

          THIS.ORDLINE.REPLACE('Book5    WITH IIF(Qty5 < m.Pik5,Book5-Qty5+m.Pik5,Book5),;
                                Book6    WITH IIF(Qty6 < m.Pik6,Book6-Qty6+m.Pik6,Book6)')

          THIS.ORDLINE.REPLACE('Book7    WITH IIF(Qty7 < m.Pik7,Book7-Qty7+m.Pik7,Book7),;
                                Book8    WITH IIF(Qty8 < m.Pik8,Book8-Qty8+m.Pik8,Book8)')

          THIS.ORDLINE.REPLACE('TotBook  WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8')

          THIS.ORDLINE.REPLACE('Qty1     WITH IIF(Qty1 < m.Pik1,m.Pik1,Qty1),;
                                Qty2     WITH IIF(Qty2 < m.Pik2,m.Pik2,Qty2)')
          THIS.ORDLINE.REPLACE('Qty3     WITH IIF(Qty3 < m.Pik3,m.Pik3,Qty3),;
                                Qty4     WITH IIF(Qty4 < m.Pik4,m.Pik4,Qty4)')

          THIS.ORDLINE.REPLACE('Qty5     WITH IIF(Qty5 < m.Pik5,m.Pik5,Qty5),;
                                Qty6     WITH IIF(Qty6 < m.Pik6,m.Pik6,Qty6)')

          THIS.ORDLINE.REPLACE('Qty7     WITH IIF(Qty7 < m.Pik7,m.Pik7,Qty7),;
                                Qty8     WITH IIF(Qty8 < m.Pik8,m.Pik8,Qty8)')
          THIS.ORDLINE.REPLACE('TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8')
			=gfAdd_Info('ORDLINE')
*     		  This.lfAdd_Info(This.ORDLINE,'ORDLINE')

          UNLOCK
        ENDIF
        *-- saving in style
        *-- we have to encounter the old value because it may be less
        *-- or greater than the new.
        =THIS.STYLE.SEEK(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle))
        SELECT STYLE
        =RLOCK()
        THIS.STYLE.REPLACE('Alo1 WITH MAX(Alo1 - m.OldPik1 + m.Pik1, 0),;
                            Alo2 WITH MAX(Alo2 - m.OldPik2 + m.Pik2, 0)')

        THIS.STYLE.REPLACE('Alo3 WITH MAX(Alo3 - m.OldPik3 + m.Pik3, 0),;
                            Alo4 WITH MAX(Alo4 - m.OldPik4 + m.Pik4, 0)')

        THIS.STYLE.REPLACE('Alo5 WITH MAX(Alo5 - m.OldPik5 + m.Pik5, 0),;
                            Alo6 WITH MAX(Alo6 - m.OldPik6 + m.Pik6, 0)')

        THIS.STYLE.REPLACE('Alo7 WITH MAX(Alo7 - m.OldPik7 + m.Pik7, 0),;
                            Alo8 WITH MAX(Alo8 - m.OldPik8 + m.Pik8, 0)')

        THIS.STYLE.REPLACE('TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')

        THIS.STYLE.REPLACE('Ord1 WITH IIF(m.Pik1 > m.Ordln1,MAX(Ord1 - m.Ordln1 + m.Pik1, 0),Ord1),;
                Ord2 WITH IIF(m.Pik2 > m.Ordln2,MAX(Ord2 - m.Ordln2 + m.Pik2, 0),Ord2)')

        THIS.STYLE.REPLACE('Ord3 WITH IIF(m.Pik3 > m.Ordln3,MAX(Ord3 - m.Ordln3 + m.Pik3, 0),Ord3),;
                Ord4 WITH IIF(m.Pik4 > m.Ordln4,MAX(Ord4 - m.Ordln4 + m.Pik4, 0),Ord4)')

        THIS.STYLE.REPLACE('Ord5 WITH IIF(m.Pik5 > m.Ordln5,MAX(Ord5 - m.Ordln5 + m.Pik5, 0),Ord5),;
                Ord6 WITH IIF(m.Pik6 > m.Ordln6,MAX(Ord6 - m.Ordln6 + m.Pik6, 0),Ord6)')

        THIS.STYLE.REPLACE('Ord7 WITH IIF(m.Pik7 > m.Ordln7,MAX(Ord7 - m.Ordln7 + m.Pik7, 0),Ord7),;
                Ord8 WITH IIF(m.Pik8 > m.Ordln8,MAX(Ord8 - m.Ordln8 + m.Pik8, 0),Ord8)')

        THIS.STYLE.REPLACE('TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8')

		=gfAdd_Info('STYLE')
       * This.lfAdd_Info(This.STYLE,'STYLE')
        UNLOCK

*!*          IF ASCAN(This.loFormSet.laEvntTrig,PADR("ALSAVPKT",10)) <> 0
*!*            SET DELETED ON
*!*            = gfDoTriger("ALPKTKT",PADR("ALSAVPKT",10))
*!*            SET DELETED OFF
*!*          ENDIF


        *-- saving in stydye (for every rec. in ordline we have - generally -
        *-- two recs. representing it : one for the entire style+cwarecode
        *-- and the other for style+cwarecode+dyelot (if no dyelot then only one))

        *IF THIS.STYDYE.SEEK(m.Style + m.cWareCode+m.dyelot)
        IF THIS.STYDYE.SEEK(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle)+ m.cWareCode+m.dyelot)
          SELECT STYDYE      && Repl. for all corresponding recs.
          =RLOCK()
          THIS.STYDYE.REPLACE('Alo1 WITH MAX(Alo1 - m.OldPik1 + m.Pik1, 0),;
                  Alo2 WITH MAX(Alo2 - m.OldPik2 + m.Pik2, 0),;
                  Alo3 WITH MAX(Alo3 - m.OldPik3 + m.Pik3, 0),;
                  Alo4 WITH MAX(Alo4 - m.OldPik4 + m.Pik4, 0)')
          THIS.STYDYE.REPLACE('Alo5 WITH MAX(Alo5 - m.OldPik5 + m.Pik5, 0),;
                  Alo6 WITH MAX(Alo6 - m.OldPik6 + m.Pik6, 0),;
                  Alo7 WITH MAX(Alo7 - m.OldPik7 + m.Pik7, 0),;
                  Alo8 WITH MAX(Alo8 - m.OldPik8 + m.Pik8, 0)')
          THIS.STYDYE.REPLACE('TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')

          THIS.STYDYE.REPLACE('Ord1 WITH IIF(m.Pik1 > Ord1,MAX(Ord1 - m.OldPik1 + m.Pik1, 0),Ord1),;
                  Ord2 WITH IIF(m.Pik2 > Ord2,MAX(Ord2 - m.OldPik2 + m.Pik2, 0),Ord2)')

          THIS.STYDYE.REPLACE('Ord3 WITH IIF(m.Pik3 > Ord3,MAX(Ord3 - m.OldPik3 + m.Pik3, 0),Ord3),;
                  Ord4 WITH IIF(m.Pik4 > Ord4,MAX(Ord4 - m.OldPik4 + m.Pik4, 0),Ord4)')

          THIS.STYDYE.REPLACE('Ord5 WITH IIF(m.Pik5 > Ord5,MAX(Ord5 - m.OldPik5 + m.Pik5, 0),Ord5),;
                  Ord6 WITH IIF(m.Pik6 > Ord6,MAX(Ord6 - m.OldPik6 + m.Pik6, 0),Ord6)')

          THIS.STYDYE.REPLACE('Ord7 WITH IIF(m.Pik7 > Ord7,MAX(Ord7 - m.OldPik7 + m.Pik7, 0),Ord7),;
                  Ord8 WITH IIF(m.Pik8 > Ord8,MAX(Ord8 - m.OldPik8 + m.Pik8, 0),Ord8)')

          THIS.STYDYE.REPLACE('TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8')
          		=gfAdd_Info('STYDYE')
*          This.lfAdd_Info(This.STYDYE,'STYDYE')
          UNLOCK
        ENDIF

        *T20060818.0001(C200876) TMI [Start]
        IF ASCAN(This.loFormSet.laEvntTrig,PADR("SVBNALOC",10)) <> 0
          SET DELETED ON
          This.loFormSet.mDoTrigger(PADR('SVBNALOC',10))
          SET DELETED OFF
        ENDIF
        *T20060818.0001(C200876) TMI [End  ]

        IF STYLE.cDye_Flg = 'Y'
          *IF THIS.STYDYE.SEEK(m.Style + m.cWareCode + m.Dyelot)
          IF THIS.STYDYE.SEEK(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle)+ m.cWareCode + m.Dyelot)
            SELECT STYDYE      && Repl. for all corresponding recs.
            =RLOCK()
             THIS.STYDYE.REPLACE('Alo1 WITH MAX(Alo1 - m.OldPik1 + m.Pik1, 0),;
                    Alo2 WITH MAX(Alo2 - m.OldPik2 + m.Pik2, 0),;
                    Alo3 WITH MAX(Alo3 - m.OldPik3 + m.Pik3, 0),;
                    Alo4 WITH MAX(Alo4 - m.OldPik4 + m.Pik4, 0)')

             THIS.STYDYE.REPLACE('Alo5 WITH MAX(Alo5 - m.OldPik5 + m.Pik5, 0),;
                    Alo6 WITH MAX(Alo6 - m.OldPik6 + m.Pik6, 0),;
                    Alo7 WITH MAX(Alo7 - m.OldPik7 + m.Pik7, 0),;
                    Alo8 WITH MAX(Alo8 - m.OldPik8 + m.Pik8, 0)')

             THIS.STYDYE.REPLACE('TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')
        		=gfAdd_Info('STYDYE')
 *            This.lfAdd_Info(This.STYDYE,'STYDYE')
            UNLOCK
          ENDIF
        ENDIF
      *--
    ENDCASE


    *Calculate differences per line and accomulate it
    IF THIS.ORDLINE.SEEK(EVALUATE(This.loFormSet.lc_TmpOrdL+'.cOrdType') +  EVALUATE(This.loFormSet.lc_TmpOrdL+'.ORDER')  + STR( EVALUATE(This.loFormSet.lc_TmpOrdL+'.LineNo'),6))
      lnBookDiff = lnBookDiff + (OrdLine.TotBook - lnOrgBook)
      lnBkAmtDif = lnBkAmtDif + ((OrdLine.TotBook - lnOrgBook) * OrdLine.Price)

      lnQtyDiff  = lnQtyDiff  + (OrdLine.TotQty - lnOrgQty)
      lnQyAmtDif = lnQyAmtDif + ((OrdLine.TotQty - lnOrgQty) * OrdLine.Price)
    ENDIF
  ENDSCAN


  llHaveDiff = (lnBookDiff != 0) OR (lnQtyDiff  != 0) OR ;
               (lnBkAmtDif != 0) OR (lnQyAmtDif != 0)

  IF llHaveDiff
    SELECT OrdHdr
    =RLOCK()
    THIS.OrdHdr.REPLACE("BOOK    WITH BOOK    + lnBookDiff ,;
                         BOOKAMT WITH BOOKAMT + lnBkAmtDif ,;
                         OPEN    WITH OPEN    + lnQtyDiff  ,;
                         OPENAMT WITH OPENAMT + lnQyAmtDif")
    UNLOCK
	=gfAdd_Info('ORDHDR')
*    This.lfAdd_Info(This.ORDHDR,'ORDHDR')
  ENDIF
  SET DELETED &lcSetDel

  *-- saving in piktkt
  SELECT PIKTKT

*!*	  IF PikTkt.Status <> This.loFormSet.AriaForm1.cboStatus.Value  .AND. This.loFormSet.AriaForm1.cboStatus.Value  = "P"
*!*	    IF llPartial
*!*	      IF ASCAN(This.loFormSet.laEvntTrig, PADR('PUL_PAR',10)) <> 0
*!*	        llReturn = gfDoTriger('ALPKTKT',PADR('PUL_PAR',10))
*!*	      ENDIF
*!*	    ELSE
*!*	      IF ASCAN(This.loFormSet.laEvntTrig , PADR('PUL_COM',10)) <> 0
*!*	        llReturn = gfDoTriger('ALPKTKT',PADR('PUL_COM',10))
*!*	      ENDIF
*!*	    ENDIF
*!*	  ENDIF
*!*    IF PikTkt.Status = "P" .AND. This.loFormSet.AriaForm1.cboStatus.Value  = "O" .AND. ASCAN( This.loFormSet.laEvntTrig , PADR('PUL_OPN',10)) <> 0
*!*      llReturn = gfDoTriger('ALPKTKT',PADR('PUL_OPN',10))
*!*    ENDIF

lcStatusValue = This.loFormSet.AriaForm1.cboStatus.Value
THIS.PIKTKT.REPLACE("Status WITH  lcStatusValue")
=gfAdd_Info('PIKTKT')
*This.lfAdd_Info(This.PIKTKT,'PIKTKT')

ENDIF
 DIMENSION laTableUpdate[6]
 laTableUpdate[1] = This.Style
 laTableUpdate[2] = This.Ordline
 laTableUpdate[3] = This.piktkt
 laTableUpdate[4] = This.pikline
 laTableUpdate[5] = This.stydye
 laTableUpdate[6] = This.ordhdr
 =This.lfTableUpdate()


SELECT(lnAlias)
ENDFUNC
*!*************************************************************
*! Name      : lfAdd_Info
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/10/2005
*! Purpose   : function to Update user info.
*!*************************************************************
FUNCTION lfAdd_Info
LPARAMETERS loFileName,lcFileName
LOCAL llEdtFld,llAddFld,lcOldAlias
lcOldAlias = SELECT(0)

lcShortVersion = 'A40'
lcUser_ID = oAriaApplication.User_ID

SELECT(lcFileName)
llEdtFld = (TYPE('cEdit_USER') <> 'U') .AND. (TYPE('dEdit_Date') <> 'U') .AND. (TYPE('cEdit_Time') <> 'U');
           AND (TYPE('cEdt_Ver') <> 'U')
llAddFld = (TYPE('cAdd_user') <> 'U') AND (TYPE('dAdd_Date') <> 'U') AND (TYPE('cAdd_Time') <> 'U') ;
           AND (TYPE('cAdd_Ver') <> 'U') AND EMPTY(cAdd_user)

*-- New Record
IF llAddFld
  *** stamp the record for this user with date and time
  loFileName.REPLACE('cAdd_User  WITH lcUser_ID ,;
          dAdd_Date  WITH DATE()    ,;
          cAdd_Time  WITH gfGetTime(),;
          cAdd_Ver   WITH lcShortVersion')
ELSE && Modified Record
  IF llEdtFld
	loFileName.REPLACE('cEdit_User  WITH lcUser_ID ,;
             		dEdit_Date  WITH DATE()    ,;
            		cEdit_Time  WITH gfGetTime(),;
		            cEdt_Ver    WITH lcShortVersion')
  ENDIF        		
ENDIF
SELECT(lcOldAlias)
ENDFUNC
*!*************************************************************
*! Name      : lfTableUpdate
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/10/2005
*! Purpose   : function to Update Sql Tables.
*!*************************************************************
FUNCTION lfTableUpdate

*--Open Dictionary files.
LOCAL lnAlias,lnConnectionHandlar,lcTranCode,lnI,llUpdate
lnAlias = SELECT(0)

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  SELECT (lnAlias)
  RETURN .F.
ENDIF

FOR lnI = 1 TO ALEN(laTableUpdate,1)
  llUpdate = laTableUpdate[lnI].TableUpdate(lcTranCode)
  IF !llUpdate
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDFOR

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  SELECT(lnAlias)
  RETURN .F.
ENDIF

SELECT(lnAlias)
*--end of lfTableUpdate.
*!*****************************************************************************************
*! Name      : lfGetSize
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/06/2005
*! Purpose   : get corresponding size
*!*****************************************************************************************
FUNCTION lfGetSize
PARAMETER lcPackSize

IF !EMPTY(EVALUATE(lcPackSize)) AND This.Scale.SEEK('S'+LEFT(EVALUATE(lcPackSize),1)) AND RIGHT(EVALUATE(lcPackSize),1) <> '0'
  lcSiz = RIGHT(EVALUATE(lcPackSize),1)
  lcSize = SCALE.Sz&lcSiz
ELSE
  lcSize = '*****'
ENDIF
RETURN lcSize
*!*****************************************************************************************
*! Name      : lfAddControlSource
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/06/2005
*! Purpose   : add control source
*!*****************************************************************************************
FUNCTION lfAddControlSource
lcStyleHeader   = gfItemMask("HI")      && label for style

WITH This.loFormSet.AriaForm1.pgfPikTkt.pgDetail.grdDetails
  .RecordSource = ""
  .RecordSource = This.loFormSet.lc_TmpOrdL
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.column1.header1.Caption= LANG_Pack_key
.column1.header1.Caption= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Pack_key,this.loFormSet.GetHeaderText("LANG_Pack_key",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  .column2.header1.Caption= lcStyleHeader
  .column1.ControlSource = This.loFormSet.lc_TmpOrdL+".Pack_ID+;
                '-'+"+This.loFormSet.lc_TmpOrdL+".cpkColor+'-'+"+;
              "ThisFormSet.loPickTkt.lfGetSize(ThisFormSet.lc_TmpOrdL+'.cpcksize')+'-'+"+ALLTRIM(This.loFormSet.lc_TmpOrdL+'.cpkversion')
  .column2.ControlSource  = This.loFormSet.lc_TmpOrdL+'.Style'
  IF !This.loFormSet.llusedyes
    This.loFormSet.llshowconf = .F.
  ELSE
     This.loFormSet.llshowconf = .T.
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.column3.header1.Caption = IIF(This.loFormSet.llUse_Configuration,LANG_Config,IIF(This.loFormSet.llUseDyes,LANG_dyelot,""))
.column3.header1.Caption = IIF(This.loFormSet.llUse_Configuration,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Config,this.loFormSet.GetHeaderText("LANG_Config",this.loFormSet.HeaderAlias)),IIF(This.loFormSet.llUseDyes,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_dyelot,this.loFormSet.GetHeaderText("LANG_dyelot",this.loFormSet.HeaderAlias)),""))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    .column3.ControlSource = This.loFormSet.lc_TmpOrdL+'.Dyelot'
     *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.loFormSet.ariaform1.pgfPikTkt.pgDetail.lblConfig.Caption = IIF(This.loFormSet.llUse_Configuration,LANG_Config,IIF(This.loFormSet.llUseDyes,LANG_dyelot,""))
This.loFormSet.ariaform1.pgfPikTkt.pgDetail.lblConfig.Caption = IIF(This.loFormSet.llUse_Configuration,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Config,this.loFormSet.GetHeaderText("LANG_Config",this.loFormSet.HeaderAlias)),IIF(This.loFormSet.llUseDyes,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_dyelot,this.loFormSet.GetHeaderText("LANG_dyelot",this.loFormSet.HeaderAlias)),""))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDIF
  .column4.ControlSource  = This.loFormSet.lc_TmpOrdL+'.Group'
  .column5.ControlSource  ='ThisFormSet.loPickTkt.lfGetTotStk()'
  .column6.ControlSource  = This.loFormSet.lc_TmpOrdL+'.TotQty'
  .column7.ControlSource  = 'ThisFormSet.loPickTkt.lfGetOpen()'
  .column8.ControlSource  = This.loFormSet.lc_TmpOrdL+'.TotPik'
  .column9.ControlSource  = 'ThisFormSet.loPickTkt.lfGetPik()'
  *E303874 MHM Add vendor style column to pick ticket screen
  .column10.header1.Caption= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_cvensty,this.loFormSet.GetHeaderText("LANG_cvensty",this.loFormSet.HeaderAlias))
  .column10.ControlSource  = This.loFormSet.lc_TmpOrdL+'.cvensty'
  *E303874 MHM Add vendor style column to pick ticket screen
  .SaveFormat()
ENDWITH
WITH This.loFormSet.AriaForm1.pgfPikTkt.pgDetail
  WITH .cntOrder
    .txtQty1.ControlSource   = This.loFormSet.lc_TmpOrdL+'.QTY1'
    .txtQty2.ControlSource   = This.loFormSet.lc_TmpOrdL+'.QTY2'
    .txtQty3.ControlSource   = This.loFormSet.lc_TmpOrdL+'.QTY3'
    .txtQty4.ControlSource   = This.loFormSet.lc_TmpOrdL+'.QTY4'
    .txtQty5.ControlSource   = This.loFormSet.lc_TmpOrdL+'.QTY5'
    .txtQty6.ControlSource   = This.loFormSet.lc_TmpOrdL+'.QTY6'
    .txtQty7.ControlSource   = This.loFormSet.lc_TmpOrdL+'.QTY7'
    .txtQty8.ControlSource   = This.loFormSet.lc_TmpOrdL+'.QTY8'
    .txttotQty.ControlSource = This.loFormSet.lc_TmpOrdL+'.TOTQTY'
  ENDWITH
  WITH .cntAlloc
    .txtQty1.ControlSource   = This.loFormSet.lc_TmpOrdL+'.PIK1'
    .txtQty2.ControlSource   = This.loFormSet.lc_TmpOrdL+'.PIK2'
    .txtQty3.ControlSource   = This.loFormSet.lc_TmpOrdL+'.PIK3'
    .txtQty4.ControlSource   = This.loFormSet.lc_TmpOrdL+'.PIK4'
    .txtQty5.ControlSource   = This.loFormSet.lc_TmpOrdL+'.PIK5'
    .txtQty6.ControlSource   = This.loFormSet.lc_TmpOrdL+'.PIK6'
    .txtQty7.ControlSource   = This.loFormSet.lc_TmpOrdL+'.PIK7'
    .txtQty8.ControlSource   = This.loFormSet.lc_TmpOrdL+'.PIK8'
    .txttotQty.ControlSource = This.loFormSet.lc_TmpOrdL+'.TOTPIK'
  ENDWITH
ENDWITH
*!*****************************************************************************************
*! Name      : lfCustCheck
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/06/2005
*! Purpose   : check customer status
*!*****************************************************************************************
FUNCTION lfCustCheck
IF CUSTOMER.Status <> 'A'
  *-- Message : 46036
  *--             "This is a non-active account."
  *-- Button  : 00000
  *--                        <  Ok  >
  =gfModalGen('TRM46036B00000','ALERT')
  RETURN .F.
ELSE
  RETURN .T.
ENDIF
ENDFUNC
*!*****************************************************************************************
*! Name      : lfMenuUpdate
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/06/2005
*! Purpose   : update menu item when clicked
*!*****************************************************************************************
FUNCTION lfMenuUpdate

*E302946,1 WAM 07/26/2011 Define new bar to call carrier Integration screen
LPARAMETERS lnSelectedBar
DO CASE
  CASE lnSelectedBar = 1
*E302946,1 WAM 07/26/2011 (End)

  IF This.loFormSet.llByLine
    SET ORDER TO OrdPack IN (This.loFormSet.lc_TmpOrdL)
    This.loFormSet.llByLine = .F.
  ELSE
    SET ORDER TO OrdLine IN (This.loFormSet.lc_TmpOrdL)
    This.loFormSet.llByLine = .T.
  ENDIF
  SELECT (This.loFormSet.lc_TmpOrdL)
  LOCATE
  *E302946,1 WAM 07/26/2011 Define new bar to call carrier Integration screen [T20101207.0006]
  *! B609818,1 MMT 02/05/2012 Modify screen to display carrier option only if client has carriers[T20120118.0002][Start]
  *CASE lnSelectedBar = 2
  CASE lnSelectedBar = This.loFormSet.Carrierbarposition
  *! B609818,1 MMT 02/05/2012 Modify screen to display carrier option only if client has carriers[T20120118.0002][END]
    lcExeKeyValue = "'P','"+PIKTKT.PikTkt+"'"
    oAriaApplication.DoProgram("AWRALCARIN",lcExeKeyValue,.F.,'AL')
ENDCASE
*E302946,1 WAM 07/26/2011 (End)
ENDFUNC
*!*****************************************************************************************
*! Name      : lfMenuUpdate
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/06/2005
*! Purpose   : update menu item when clicked
*!*****************************************************************************************
FUNCTION lfRefreshModes
  DO CASE

    *-- SELECT MODE
    CASE This.loFormSet.ActiveMode = "S"
      WITH This.loFormSet.ariaform1
        .cboStatus.RowSource = "Thisformset.laStatus"
        .cboStatus.DisplayValue = ""
        WITH  .pgfPikTkt.pgHeader
          STORE {} TO .dtpComplete.value,.dtpEntered.value,.dtpStart.value
          STORE ' '  TO .txtDepatment.value,.txtNotes1.Value,.txtNotes2.Value

          WITH .cntBillAddress
            STORE '' TO .txtBillAdd1.Value,.txtBillAdd2.Value,.txtBillAdd3.Value,;
                        .txtBillAdd4.Value,.txtBillAdd5.Value,.txtBillName.Value
          ENDWITH

          WITH .cntshipAddress
          STORE '' TO .txtShipAdd1.Value,.txtShipAdd2.Value,.txtShipAdd3.Value,;
                      .txtShipAdd4.Value,.txtShipAdd5.Value,.txtShipName.Value
          ENDWITH
        ENDWITH
      ENDWITH
      lcAlias = ALIAS()
      SELECT(This.loFormSet.lc_TmpOrdL)
      ZAP
      SELECT(lcAlias)

    *-- EDIT MODE
    CASE This.loFormSet.ActiveMode = "E"
      lcOldStats =  This.loFormSet.ariaform1.cbostatus.value
      This.loFormSet.ariaform1.cboStatus.RowSource = 'thisformset.laPkTktStatus'
      This.loFormSet.ariaform1.cbostatus.value  = lcOldStats

    *--VIEW MODE
    CASE This.loFormSet.ActiveMode = "V"
      This.loFormSet.ariaform1.cboStatus.RowSource = "Thisformset.laStatus"
      This.lfGetData(This.loFormSet.ariaform1.kbPickNo.keytextbox.Value)
ENDCASE
ENDFUNC
ENDDEFINE
