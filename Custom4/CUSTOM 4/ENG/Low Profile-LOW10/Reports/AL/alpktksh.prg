*!*****************************************************************************************
*! Name      : ALPKTKSH.PRG
*! Developer : Ahmed Salah shalaby - [SSH]
*! Date      : 10/31/2006
*! Purpose   : Printing Picking Sheet
*! Entry no. : T20060820.0005 - Picking Sheet For Low10
*!*****************************************************************************************
*! Modification
*! C200787,1 change BarCode ID, pick it from warehous file
*! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[T20080807.0001]
*! C201047,2 MMT 09/17/2008 update Add_user info in SDNHIST File   			 [T20080807.0001]
*! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program			 [T20081208.0005]
*! B609054,1 HES 10/21/2009 Wrong pack calculation at Low Profile            [T20090914.0003]
*!*****************************************************************************************
STORE "" TO Tmp2Print,TmpPktkt,TmpCalOf,lcOrderPti,lcStyGrp,TmpWareCod,lcDepot,lcSupplier,lcStytitle,;
             lcScl1,lcScl2,lcScl3,lcScl4,lcScl5,lcScl6,lcScl7,lcScl8,lcStyDesc,lcFirstFrx,lcOldScal,lcCurrTrcukRef,lcMsStore,;
             TmpURNSeq,TmpMnfNo,lcETOFTmp

lnNumber = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"ALTRKMNF.CTRKREF",1),1)
TmpMnfNo = laOgFxFlt[lnNumber,6]

lnNumber = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"ALCALOFF.CCALLOFF",1),1)
TmpCalOf = laOgFxFlt[lnNumber,6]
lcETOFTmp = loOgScroll.gfTempName()

lnNumber = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"ALURNHST.CURNSEQ",1),1)
TmpURNSeq = laOgFxFlt[lnNumber,6]

IF (lcRpETS = "Y" OR lcRpREMNF="Y") AND !(!EMPTY(TmpMnfNo) AND USED(TmpMnfNo))
  IF !EMPTY(TmpMnfNo)
    SELECT (TmpMnfNo)
    GO TOP
    IF EOF()
      WAIT WINDOW "You have to select Truck Manifest!"
      RETURN
    ENDIF
  ELSE
    WAIT WINDOW "You have to select Truck Manifest!"
    RETURN
  ENDIF
ENDIF



IF (lcRpReURN="Y") AND !(!EMPTY(TmpURNSeq) AND USED(TmpURNSeq))
  IF !EMPTY(TmpURNSeq)
    SELECT (TmpURNSeq)
    GO TOP
    IF EOF()
      WAIT WINDOW "You have to select URN Label!"
      RETURN
    ENDIF
  ELSE
    WAIT WINDOW "You have to select URN Label!"
    RETURN
  ENDIF
ENDIF


IF lcRpURN="Y" AND lcRpReURN="Y"
  lcRpURN="N"
ENDIF

DECLARE INTEGER FindWindow IN user32 STRING lpClassName, STRING lpWindowName
DECLARE INTEGER GetParent IN user32 INTEGER hwnd
DECLARE INTEGER GetFocus IN user32
*-- Mahmoud Said Start
IF CallFromExportorEmail()
  SetGlobals()  
ENDIF
*-- Mahmoud Said End

Tmp2Print  = loOGScroll.gfTempName()
*lcOGTmpForm = loOGScroll.gfTempName()
lcStytitle = gfItemMask("HI")
lcOldScal  = "??"
llPrinter = .T.
lcSupplier = lfGetSupplier()


=lfCreateTemp()
llDummy = lfGetSelCri()
=(lcRpETS = "N") AND (lcRpREMNF="N") AND (lcRpReURN="N") AND lfCollect()

IF lcRpPKSHT="Y"
  SELECT (Tmp2Print)
  SELECT (Tmp2Print)
  SET ORDER TO TAG(Tmp2Print)
  GO TOP
  lcFormName = "alpktksh"
  loOGScroll.cCROrientation='L'
  loOgScroll.lcOGLastForm = lcFormName
  loOGScroll.CheckPlatForm()
  =gfCrtFrm(lcFormName ,"",llOGRefForm)

  *-- Mahmoud Said Start
  loOgScroll.lnActivePDFViewerNo  = 1
  *-- Mahmoud Said End

  DO gfDispRe  WITH EVAL('lcFormName')
  *-- Mahmoud Said Start
  *-- 
  IF CallFromExport()
    OpenMainFile()
  ENDIF
  *-- Mahmoud Said End

  SELECT (Tmp2Print)
  SET ORDER TO Tag Tmp2Print2
  GO TOP
  lcFormName = "ALPkShS"
  loOgScroll.lcOGLastForm = lcFormName
  loOGScroll.cCROrientation='L'
  loOGScroll.CheckPlatForm()
  =gfCrtFrm(lcFormName ,"",llOGRefForm)

  *-- Mahmoud Said Start
  IF CallFromExportorEmail()
  	SetExtraFileName("Second")
  ENDIF

  loOgScroll.lnActivePDFViewerNo  = 2
  *-- Mahmoud Said End

  DO gfDispRe WITH EVAL('lcFormName')

  *-- Mahmoud Said Start
  *-- 
  IF CallFromExport()
    OpenExtraFile("Second")
  ENDIF
  
  IF CallFromEmail()
    AddExtraAttachment("Second")
  ENDIF
  *-- Mahmoud Said End
  
  

ENDIF
IF !USED("ALRepURNHst")
  = gfOpenTable(oAriaApplication.DataDir + 'ALURNHst', 'Alurnpk', 'SH','ALRepURNHst')
ENDIF
*llDummy = (lcRpURN = "Y" OR lcRpTRKMNF = "Y" OR lcRpReURN = "Y" OR lcRpREMNF="Y") AND  lfPrintURN()  &&

******** URN Begind *******************
lcURNfile=loOgScroll.gfTempName()
=lfBuildTmp()
llDummy  = (lcRpURN   = "Y")  AND  lfPrintURN()
llDummy  = (lcRpReURN = "Y")  AND  lfRePrnURN()
llDummy  = (lcRpURN = "Y" OR lcRpReURN = "Y" )AND  lfDisplyURN()
******** URN END *******************


******** Manufest Begind *******************

llDummy = (lcRpTRKMNF = "Y") AND lfGetMfst()
llDummy = (lcRpREMNF = "Y")  AND lfRePrntMfst()
llDummy = ((lcRpREMNF  = "Y") OR (lcRpTRKMNF = "Y")) AND lfPrintMfst()
******** Manufest END *******************

******** Manufest Begind *******************
***( lcRpREMNF= "Y" OR lcRpTRKMNF = "Y")  AND
IF USED(lcURNfile)
  SELECT * FROM (lcURNfile) INTO CURSOR (lcETOFTmp) READWRITE
ENDIF
llDummy = (lcRpETS = "Y") AND  lfPrintETOS()
******** Manufest Begind *******************


******** Main Begind *******************
FUNCTION lfCollect
PRIVATE lcSelBy
************************************
lcSelBy = ""
llDummy = lfGetData()


******************************
FUNCTION lfGetData
******************************
SELECT ALCalOff
=gfSeek("")
SCAN
  SCATTER MEMVAR MEMO
  llAddRecord = .T.
  IF !EMPTY(TmpPktkt) AND USED(TmpPktkt)
    llAddRecord = llAddRecord AND lfValdPkTk(m.PikTkt)
  ENDIF
  IF TYPE("lcStyGrp")="C" AND !EMPTY(lcStyGrp) 
    llAddRecord = llAddRecord AND lfValdStGrp(m.Style)
  ENDIF
  IF !EMPTY(TmpWareCod) AND USED(TmpWareCod)
    llAddRecord = llAddRecord AND lfValdWhse(m.PikTkt)
  ENDIF

  IF !EMPTY(TmpCalOf) AND USED(TmpCalOf)
    llAddRecord = llAddRecord AND lfValdCall(m.cCallOff)
  ENDIF
  IF TYPE("lcDepot")="C" AND !EMPTY(lcDepot)
    llAddRecord = llAddRecord AND (m.cDepot $lcDepot)
  ENDIF
  IF TYPE("lcOrderPti")="C" AND !EMPTY(lcOrderPti)
    llAddRecord = llAddRecord AND (lfGetPri(m.Order) = lcOrderPti)
  ENDIF
  
  IF TYPE("lcMsStore")="C" AND !EMPTY(lcMsStore) 
    llAddRecord = llAddRecord AND lfValdMSStr(m.cMSSTORE)
  ENDIF
  
  IF llAddRecord

    IF UPPER(oAriaApplication.gcDevice) <> "SCREEN"
      SELECT ALCalOff
      REPLACE Printed WITH .T.
    ENDIF
    INSERT INTO (Tmp2Print) FROM MEMVAR
    SELECT Style
    =gfSetOrder("STYLE")
    =gfSeek(m.Style)
    *=SEEK(m.Style,'Style','Style')
    SELECT (Tmp2Print)
    REPLACE Scale WITH Style.Scale
  ENDIF
ENDSCAN

SELECT ALCalOff
=gfTableUpdate('ALCalOff')
******************************
FUNCTION lfGetSelCri
******************************

lnNumber = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"ORDHDR.PRIORITY",1),1)
lcOrderPti = laOgFxFlt[lnNumber,6]
lnNumber = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"PIKTKT.PIKTKT    ",1),1)
TmpPktkt = laOgFxFlt[lnNumber,6]
lnNumber = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"STYLE.CPURCODE",1),1)
lcStyGrp = laOgFxFlt[lnNumber,6]

lnNumber  = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"ALCALOFF.CMSSTORE",1),1)
lcMsStore = laOgFxFlt[lnNumber,6]

lnNumber = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"PIKTKT.CWARECODE",1),1)
TmpWareCod = laOgFxFlt[lnNumber,6]
lnNumber = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"ALCALOFF.CCALLOFF",1),1)
TmpCalOf = laOgFxFlt[lnNumber,6]
lnNumber = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"ALCALOFF.CDEPOT",1),1)
lcDepot = laOgFxFlt[lnNumber,6]

lnNumber = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"ALURNHST.CURNSEQ",1),1)
TmpURNSeq = laOgFxFlt[lnNumber,6]

lnNumber = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,"ALTRKMNF.CTRKREF",1),1)
TmpMnfNo = laOgFxFlt[lnNumber,6]



******************************
FUNCTION lfCreateTemp
******************************

PRIVATE lnOldAls

SELECT AlCalOff
SELECT *,SPACE(3) AS Scale FROM AlCalOff WHERE cCallOff="XXZZZ" INTO CURSOR (Tmp2Print) READWRITE
SELECT (Tmp2Print)
INDEX ON cCallOff+cmsstore+cdepot+Scale TAG (Tmp2Print)
INDEX ON cCallOff+Style+Scale TAG Tmp2Print2
INDEX ON cCallOff+PikTkt TAG Tmp2PrintP


******************************
FUNCTION lfSRVLBL
PARAMETERS lcParm
******************************
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
      SET ORDER TO TAG CUSTOMER IN CUSTOMER
      SELECT PIKTKT
      SET RELATION TO 'O' + Order INTO ORDHDR
      SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,'S' + Account + Store) INTO CUSTOMER ADDITIVE
  CASE lcParm = 'R'  && Reset code
      SELECT PIKTKT
      SET RELATION TO 
  OTHERWISE      && Valid code
ENDCASE

******************************
FUNCTION lfSRVCALL 
PARAMETERS lcParm
******************************
PRIVATE lcAlias,llHaveSty
lcAlias = SELECT(0)
DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT ALCalOff
    SET ORDER TO TAG Calldist
    SET RELATION TO PikTkt INTO  PIKTKT ADDITIVE
    GO TOP
  CASE lcParm = 'R'  && Reset code
    SELECT ALCalOff
    SET ORDER TO TAG ALCalOff
    SET RELATION TO 
    GO TOP
  OTHERWISE      && Valid code

ENDCASE
SELECT(lcAlias)

******************************
FUNCTION lfwRepWhen
******************************

= gfOpenTable(oAriaApplication.DataDir + 'Warehous', 'Warehous', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'STYLE', 'STYLE', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'PIKTKT', 'PIKTKT', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'OrdHDr', 'OrdHDr', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'OrdLine', 'OrdLine', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'Customer', 'Customer', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'Scale', 'Scale', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'AlCalOff', 'AlCalOff', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'almnfhst', 'almnfhst', 'SH')
******************************
FUNCTION lfGetSupplier
******************************
GO TOP IN Warehous
RETURN (Warehous.Cana)

******************************
FUNCTION lfGetWareH
LPARAMETERS cPikTkt
******************************

PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT PIKTKT
=gfSeek(cPikTkt)

SELECT Warehous
=gfSeek(PIKTKT.cWareCode)

SELECT(lnOldAls)
RETURN(SUBSTR(Warehous.cWareCode,1,2)+SPACE(2)+ALLTRIM(Warehous.cDesc))

******************************
FUNCTION lfGetPri
LPARAMETERS cOrder
******************************

PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT OrdHdr
=gfSeek("O"+cOrder)

SELECT(lnOldAls)
RETURN(OrdHdr.Priority)

******************************
FUNCTION lfGetStore
LPARAMETERS cStore
******************************

RETURN(cStore+" - "+gfCodDes(cStore, 'CMSSTORE'))

******************************
FUNCTION lfGetDepot
LPARAMETERS cDepot
******************************
RETURN(cDepot+" - "+gfCodDes(cDepot, 'CDEPOT    '))

******************************
FUNCTION lfGetPkSht
LPARAMETERS cStyle
******************************
PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT Style
=gfSeek(cStyle)

SELECT(lnOldAls)

RETURN(SUBSTR(gfCodDes(Style.CPURCODE, 'CPURCODE'),1,3)+"/"+cCallOff)


**************************************
FUNCTION lfGetScale
LPARAMETERS cStyle
**************************************
PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT Style
=gfSeek(cStyle)

SELECT Scale
=gfSeek("S"+Style.Scale)

lcScl1 = Scale.Sz1
lcScl2 = Scale.Sz2
lcScl3 = Scale.Sz3
lcScl4 = Scale.Sz4
lcScl5 = Scale.Sz5
lcScl6 = Scale.Sz6
lcScl7 = Scale.Sz7
lcScl8 = Scale.Sz8
lcStyDesc = Style.Desc
SELECT(lnOldAls)
******************************
FUNCTION lfGetPkNo
LPARAMETERS cOrder
******************************
PRIVATE lnOldAls
lnOldAls = SELECT(0)

*! B609054,1 HES 10/21/2009 Wrong pack calculation at Low Profile [Start]
SELECT ORDHDR
gfSetOrder('ORDHDR')
gfSeek('O'+cOrder)
*! B609054,1 HES 10/21/2009 Wrong pack calculation at Low Profile [End]

SELECT Customer
=gfSeek("M"+OrdHDr.Account)

SELECT(lnOldAls)
RETURN(MAX(VAL(Customer.cPackSize),1))


******************************
FUNCTION lfValdPkTk
LPARAMETERS cPikTkt
******************************
PRIVATE lnOldAls,ll2Return

lnOldAls = SELECT(0)
SELECT (TmpPktkt)
GO TOP
IF !EOF()
  LOCATE FOR PikTkt = cPikTkt
  ll2Return = FOUND()
ELSE
  ll2Return = .T.
ENDIF
SELECT(lnOldAls)
RETURN(ll2Return)

******************************
FUNCTION lfValdStGrp
LPARAMETERS cStyle
******************************

PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT Style
=gfSeek(cStyle)
*=SEEK(cStyle,'Style','Style')
SELECT(lnOldAls)
RETURN(Style.CPURCODE$lcStyGrp)


******************************
FUNCTION lfValdMSStr
LPARAMETERS cpMsStore
******************************
RETURN(cpMsStore$lcMsStore)

******************************
FUNCTION lfValdWhse
LPARAMETERS cPikTkt
******************************

PRIVATE lnOldAls,ll2Return
lnOldAls = SELECT(0)
SELECT PikTkt
=gfSeek(cPikTkt)

SELECT (TmpWareCod)
GO TOP
IF !EOF()
  LOCATE FOR cWareCode = PikTkt.cWareCode
  ll2Return = FOUND()
ELSE
  ll2Return = .T.
ENDIF
SELECT(lnOldAls)
RETURN(ll2Return)

******************************
FUNCTION lfValdCall
LPARAMETERS pcCallOff,ll2Return
******************************
PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT (TmpCalOf)
GO TOP
IF !EOF()
  LOCATE FOR cCallOff = pcCallOff
  ll2Return = FOUND()
ELSE
  ll2Return = .T.
ENDIF
SELECT(lnOldAls)
RETURN(ll2Return)

FUNCTION lfScale


lcOldScal = SCALE

FUNCTION lfResetScl

lcOldScal = "???"

******************************
FUNCTION lfPrintURN
******************************

SELECT (Tmp2Print)
SET ORDER TO TAG (Tmp2Print)
GO TOP
=lfGetURN()
SELECT (lcURNfile)
GO TOP
IF !RECCOUNT()>0
  *-- Message : There are no records to display...!
  *--                < Ok > 
   =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ELSE
*!*	  IF (lcRpTRKMNF = "Y" OR lcRpREMNF="Y")
*!*	    SELECT *,"99" AS TmpKey FROM TmpMnfst INTO CURSOR cPrntManfs ORDER BY Style READWRITE
*!*	    USE IN TmpMnfst
*!*	  ENDIF
ENDIF
*!*	ELSE
*!*	  SELECT *,"99" AS TmpKey FROM (lcURNfile) INTO CURSOR cPrntManfs ORDER BY Style READWRITE
*!*	  SELECT * FROM (lcURNfile) INTO CURSOR (lcETOFTmp) READWRITE
*!*	ENDIF


***************************************
FUNCTION lfGetURN
***************************************



PRIVATE lnTotQty,lcTKey
lnTotQty = 0
SELECT (Tmp2Print)
INDEX ON cDepot+cMsStore+cCallOff+Style TAG URNLBL ADDITIVE
SET ORDER TO TAG URNLBL
SELECT (Tmp2Print)
GO TOP
DO WHILE !EOF()
  SCATTER MEMVAR MEMO
  lcTKey = m.cDepot+m.cMsStore+m.cCallOff
  
  *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[Start]
  IF !EMPTY(m.Pack_ID)
   lnPackSize = lfGetPkSz(m.Pack_ID,m.Style)
   IF lnPackSize = 0
     = gfModalGen('INM00000B00000','F','ALERT',' ','Pack ID '+m.Pack_ID+' for call off '+m.cCallOff+' style '+m.Style+' does not exist in the Pack file - please investigate and correct before creating URN labels')
     RETURN 
   ENDIF 
  ELSE
  *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[End]
  lnPackSize = lfGetPkNo(m.Order)
    *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[Start]
  EndiF
  *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[End]
  SUM REST TotQty TO lnTotQty While cDepot+cMsStore+cCallOff+Style = lcTKey
  lnLabelNo = CEILING(lnTotQty/lnPackSize)
  FOR lnLblNo = 1 TO lnLabelNo
    WAIT WINDOW "Collecting recoreds for CALL OFF : "+m.cCallOff+" STORE: "+m.cMsStore +" LABEL: "+ ALLTRIM(STR(lnLblNo))NOWAIT
    lnQty = MIN(lnPackSize,lnTotQty)
    lnTotQty = ABS(MAX(lnTotQty - lnQty,0))
    SELECT (lcURNfile)
    APPEND BLANK
    REPLACE cDepot    WITH m.cDepot,;
            cMsStore  WITH m.cMsStore,;
            cDept     WITH lfGetStyGroup(m.Style),;
            cCallOff  WITH m.cCallOff,;
            Series    WITH "0"+lfGetLWareH(m.Order),;
            NPACK     WITH lnLblNo,;
            TotPck    WITH lnLabelNo,;
            Qty       WITH PADL(ALLTRIM(STR(lnQty)),3,'0'),;
            Desptch   WITH lfGetBarCode(m.cCallOff) ,;
            cUnique   WITH lfGetUniqSeq(m.cCallOff,m.cDepot,m.cMsStore,m.PikTkt,lnLblNo,lnLabelNo,.F.,lnQty,ALLTRIM(cDept)),;
            cDeptBar  WITH "0"+lfGetStyGroup(m.Style,.T.),;
            cDepotDes WITH gfCodDes(m.cMsStore, 'CMSSTORE  '),;
            Style     WITH m.Style
  ENDFOR
  SELECT (Tmp2Print)
ENDDO
SELECT (lcURNfile)

************************************************************************


FUNCTION lfGetUniqSeq
LPARAMETERS pcCallOf,pcDepot,pcMsStore,pcPkTkt,lnLblNo,lntotPk,llDont,cpnqty,cpDept
******************************

ll2Return = ""
PRIVATE ll2Return,llAdd,llAddRecord,lnOldAls
lnOldAls = SELECT(0)
SELECT ALRepURNHst
=gfSetOrder("Alurnstr")
llAdd = .T.
IF gfSeek(pcCallOf+pcDepot+pcMsStore+pcPkTkt)
  LOCATE FOR ccalloff+cdepot+cmsstore+piktkt=pcCallOf+pcDepot+pcMsStore+pcPkTkt AND NPACKNO = lnLblNo AND nPckQty = lntotPk AND ALLTRIM(CDEPT) =cpDept
  llAdd = !FOUND()
ENDIF
IF llAdd 
  SELECT ALRepURNHst
  APPEND BLANK
  REPLACe cURNSeq   WITH gfSequence('CURNSEQ'),;
          PikTkt    WITH pcPkTkt,;
          cCallOff  WITH pcCallOf,;
          cDepot    WITH pcDepot,;
          cMSSTORE  WITH pcMsStore,;
          NPACKNO   WITH lnLblNo,;
          nPckQty   WITH lntotPk,;
          CQty      WITH ALLTRIM(STR(cpnqty)),;
          CDEPT     WITH cpDept,;
          cAdd_USer WITH oAriaApplication.User_ID,;
          cAdd_Time WITH gfGettime(),;
          dAdd_Date WITH oAriaApplication.SystemDate
  =gfReplace('')
*! B609054,1 HES 10/21/2009 Wrong pack calculation at Low Profile [Start]
*!*	  IF UPPER(oAriaApplication.gcDevice) <> "SCREEN"
*!*	    WAIT WINDOW "Updating URN History..." NOWAIT
*!*	    =gfTableUpdate('ALRepURNHst')
*!*	    WAIT CLEAR
*!*	  ENDIF
*! B609054,1 HES 10/21/2009 Wrong pack calculation at Low Profile [End]
ENDIF

*! B609054,1 HES 10/21/2009 Wrong pack calculation at Low Profile [Start]
IF UPPER(oAriaApplication.gcDevice) <> "SCREEN"
  WAIT WINDOW "Updating URN History..." NOWAIT
  =gfTableUpdate('ALRepURNHst')
  WAIT CLEAR
ENDIF
*! B609054,1 HES 10/21/2009 Wrong pack calculation at Low Profile [End]

ll2Return = ALRepURNHst.cURNSeq
SELECT(lnOldAls)
RETURN(ll2Return)

******************************
FUNCTION lfBuildTmp
******************************
DIMENSION laTempStru[13,18]
STORE '' TO laTempStru
laTempStru[1,1] = 'cDepot'
laTempStru[1,2] = 'C'
laTempStru[1,3] = 2
laTempStru[1,4] = 0
laTempStru[2,1] = 'cMsStore'
laTempStru[2,2] = 'C'
laTempStru[2,3] = 4
laTempStru[2,4] = 0
laTempStru[3,1] = 'cDept'
laTempStru[3,2] = 'C'
laTempStru[3,3] = 6
laTempStru[3,4] = 0
laTempStru[4,1] = 'cCallOff'
laTempStru[4,2] = 'C'
laTempStru[4,3] = 6
laTempStru[4,4] = 0
laTempStru[5,1] = 'Series'
laTempStru[5,2] = 'C'
laTempStru[5,3] = 5
laTempStru[5,4] = 0
laTempStru[6,1] = 'NPACK'
laTempStru[6,2] = 'N'
laTempStru[6,3] = 4
laTempStru[6,4] = 0
laTempStru[7,1] = 'TotPck'
laTempStru[7,2] = 'N'
laTempStru[7,3] = 4
laTempStru[7,4] = 0
laTempStru[8,1] = 'Qty'
laTempStru[8,2] = 'C'
laTempStru[8,3] = 15
laTempStru[8,4] = 0
laTempStru[9,1] = 'Desptch'
laTempStru[9,2] = 'C'
laTempStru[9,3] = 3
laTempStru[9,4] = 0
laTempStru[10,1] = 'cUnique'
laTempStru[10,2] = 'C'
laTempStru[10,3] = 6
laTempStru[10,4] = 0
laTempStru[11,1] = 'cDeptBar'
laTempStru[11,2] = 'C'
laTempStru[11,3] = 3
laTempStru[11,4] = 0
laTempStru[12,1] = 'cDepotDes'
laTempStru[12,2] = 'C'
laTempStru[12,3] = 60
laTempStru[12,4] = 0
laTempStru[13,1] = 'Style'
laTempStru[13,2] = 'C'
laTempStru[13,3] = 19
laTempStru[13,4] = 0

=gfCrtTmp(lcURNfile,@laTempstru,,"",.f.)
 

*****************************
FUNCTION lfGetLWareH
LPARAMETERS cOrder ,llFirst
******************************

PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT OrdHdr
=gfSeek("O"+cOrder)

SELECT (lnOldAls)
IF llFirst
  RETURN(SUBSTR(OrdHdr.cWareCode,1,2))
ELSE
  RETURN(SUBSTR(OrdHdr.cWareCode,3,4))
ENDIF

*******************************
FUNCTION lfGetDevice
*******************************
IF SYS(2040)='2'
  llPrinter = .T.
ENDIF
RETURN .T.

******************************************
FUNCTION lfAdjustCRSettings
******************************************
DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[1,2]  
loOgScroll.lcOGLastForm ='AlUrn'   && LAYOUT NAME
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +lcURNfile + ".DBF"  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'URN Label'
lcFormName ='AlUrn'
loOGScroll.CheckPlatForm()
loOGScroll.cCROrientation='P'
=gfCrtFrm(lcFormName ,"",llOGRefForm)

******************************
FUNCTION lfGetStyGroup
LPARAMETERS cStyle,llParCode
******************************
PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT Style
=gfSeek(cStyle)
SELECT(lnOldAls)
IF llParCode
  RETURN(SUBSTR(gfCodDes(Style.CPURCODE, 'CPURCODE'),2,2))
ELSE
  RETURN(SUBSTR(gfCodDes(Style.CPURCODE, 'CPURCODE'),1,3))
ENDIF

********************************
FUNCTION gfSequence
PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision,lcField
********************************
PRIVATE lnRetVal,lcSavAlias,lcDataDir,lnOldGenNm,lcExtraStr,lcToFind,lcKeyExp,;
        gcDataDir,gcComp_Mdl,gcSysHome,gcCurSite,gcAct_Comp,gcOrgPath

gcDataDir  = oAriaApplication.DataDir
gcComp_Mdl = oAriaApplication.CompanyInstalledModules
gcSysHome  = oAriaApplication.SysPath
gcCurSite  = oAriaApplication.CurrentSite
gcAct_Comp = oAriaApplication.ActiveCompanyId
gcOrgPath  = oAriaApplication.DefaultPath
lcField    = IIF(TYPE("lcField")="C", ALLTRIM(UPPER(lcField)), SPACE(0))
lcSavAlias = SELECT(0)
lcSeqType  = UPPER(lcSeqType)
lcDataDir = gcDataDir
PRIVATE lcCmpCode,lcChrToUpd
lcChrToUpd = CHR(0)
lcCmpCode = IIF(TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId), lcCompanyId , gcAct_Comp)
lcUnqPreFx = gfGetMemVar('M_UNQSTPRX' , lcCmpCode)
IF TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId) AND lcCompanyId <> gcAct_Comp
	lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from syccomp where cComp_ID='"+lcCompanyId+"'",'',"syccomptmp","",oAriaApplication.SystemConnectionString,3)
	IF lnRemResult>=1
  	LOCATE
    lcDataDir = gfGetDataDir(ALLTRIM(syccomptmp.cCom_dDir))
  ENDIF
  IF USED("syccomptmp")
    USE IN syccomptmp
  ENDIF
ENDIF
lcGroupId  = IIF(TYPE('lcGroupId') ='C',PADR(lcGroupId,3),SPACE(3))
lcDivision = IIF(TYPE('lcDivision')='C',ALLTRIM(lcDivision),SPACE(10))
lnRetVal   = 0
llDivOnSeq = gfGetMemVar('M_DIV_SEQ' , lcCompanyId) = 'Y'
IF llDivOnSeq AND EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
  DECLARE laDivDlt[1,2]
  laDivDlt[1,1] = 'DIVGROUP'
  laDivDlt[1,2] = 'lcGroupId'
  =gfRltFld(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
  lcGroupId = SUBSTR(lcGroupId,1,3)
ENDIF
lcGroupId = IIF(llDivOnSeq , SUBSTR(lcGroupId,1,3) , SPACE(3))
IF !USED('SEQUENCE')
  luSequence = .T.
  USE (lcDataDir+"SEQUENCE") IN 0 ORDER TAG 'cSeq_Type'
ELSE
  SELECT SEQUENCE
  luSequence = .F.
  ltSequence = VAL(SYS(21))
  leSequence = RECNO()
  SET ORDER TO TAG Cseq_type IN SEQUENCE
ENDIF

IF !SEEK(PADR(lcSeqType,10)+lcGroupId,'SEQUENCE')
  LOCAL lnDataSess
  lnDataSess = SET("Datasession")  
  lcPropFld = IIF(EMPTY(lcField), lcSeqType, lcField)
  lnRemFldResult = oAriaApplication.remotesystemdata.execute("Select * from sydfield where Cfld_name='"+PADR(lcPropFld,10)+"'",'',"sydfieldtmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
  IF lnRemFldResult=1
    LOCATE
  ENDIF
  lnRemFlFldResult = oAriaApplication.remotesystemdata.execute("Select * from sydflfld where Cfld_name='"+PADR(lcPropFld,10)+"' AND lEnumerate=1",'',"sydflfldtmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
  IF lnRemFlFldResult=1
    LOCATE
  ENDIF
  LOCATE REST WHILE cFld_Name=PADR(lcPropFld,10) FOR lEnumerate
  lnDefSeq = sydflfldtmp.nDef_Seq
  IF !EMPTY(lcGroupId) AND SEEK(PADR(lcSeqType,10),'SEQUENCE')
    SELECT SEQUENCE
    lnDefSeq = 0
    SCAN REST WHILE cseq_type+cseq_group = PADR(lcSeqType,10)
      lnDefSeq = MAX(lnDefSeq,nSeq_No)
    ENDSCAN
    lnDefSeq = (INT(lnDefSeq/50000)+1)*50000
  ENDIF
  INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth,cSeq_Chr) ;
       VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfieldtmp.cData_Typ,;
       sydfieldtmp.nFld_Wdth,CHR(0))
  IF sydflfldtmp.lEnumerate
    lnRemFlResult = oAriaApplication.remotesystemdata.execute("Select * from sydfiles where Cfile_nam='"+sydflfldtmp.cFile_Nam+"'",'',"sydfilestmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
    IF lnRemFlResult=1
      LOCATE
    ENDIF
    SELECT SEQUENCE
    REPLACE cFile_Nam WITH sydfilestmp.cFile_Nam ,;
            cFile_Tag WITH sydfilestmp.cFile_Tag
    IF USED("SYDFILESTMP")
      USE IN Sydfilestmp
    ENDIF
  ENDIF
  IF USED("SYDFLFLDTMP")
    USE IN Sydflfldtmp
  ENDIF
  IF USED("sydfieldtmp")
    USE IN Sydfieldtmp
  ENDIF
ENDIF
DO WHILE !RLOCK("SEQUENCE")
ENDDO
lnRetVal = SEQUENCE.nSeq_No
lcChrToUpd = Sequence.cSeq_Chr
lnRetLen = SEQUENCE.nFld_Wdth - LEN(lcUnqPreFx)
lnOldGenNm = SEQUENCE.nSeq_No
lcExtraStr = ''
IF !EMPTY(SEQUENCE.cSeq_Chr)
  PRIVATE lcChar , lnCharPos , lnI
  IF !(SEQUENCE.cSeq_Chr = CHR(0))
    IF MOD(ASC(SEQUENCE.cSeq_Chr),26) = 0
      lcChar = "Z"
      lnCharPos = ASC(SEQUENCE.cSeq_Chr)/26
    ELSE
      lcChar =  CHR(MOD(ASC(SEQUENCE.cSeq_Chr),26)+64)
      lnCharPos = INT(ASC(SEQUENCE.cSeq_Chr)/26)+1
    ENDIF  
    FOR lnI = 1 TO lnCharPos - 1
      lcExtraStr = lcExtraStr + "Z"
    ENDFOR
    lcExtraStr = lcExtraStr + lcChar
  ELSE
  lcChar=""
  ENDIF
ENDIF
IF !EMPTY(SEQUENCE.cFile_Nam) .AND. UPPER(LEFT(SEQUENCE.cFile_Nam,2))<> 'SY' AND !EMPTY(SEQUENCE.cFile_Tag)
  lcSeqFile = ALLTRIM(SEQUENCE.cFile_Nam)
  lcSeqTag  = ALLTRIM(SEQUENCE.cFile_Tag)
  PRIVATE lcNewSeqFile
  lcNewSeqFile = gfTempName()
  =gfOpenTable(gcDataDir+lcSeqFile,lcSeqTag,'SH',lcNewSeqFile)
  SELECT (lcNewSeqFile)
  lcKeyField = SUBSTR(KEY(),1,AT('+'+lcSeqType,KEY())-1)
  DECLARE laVldEnt[1]
  IF !EMPTY(lcKeyField) .AND. gfGetVld(lcKeyField,@laVldEnt) > 0
    FOR lnCount = 1 TO ALEN(laVldEnt,1)
      lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
      DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcNewSeqFile)
        =gfGetSeq(lnRetVal,lcChrToUpd)
        lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
      ENDDO
    ENDFOR
  ELSE  
    lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
    DO WHILE SEEK(lcKeyExp,lcNewSeqFile)
      =gfGetSeq(lnRetVal,lcChrToUpd)
      lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
    ENDDO
  ENDIF  
  =gfCloseTable(lcNewSeqFile)
ENDIF
SELECT SEQUENCE
IF llPrinter OR UPPER(oAriaApplication.gcDevice) <> "SCREEN"
  REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
ENDIF
lnOldGenNm = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
=gfGetSeq(lnRetVal,lcChrToUpd)
IF llPrinter OR UPPER(oAriaApplication.gcDevice) <> "SCREEN"
REPLACE nSeq_No WITH lnRetVal , cSeq_Chr WITH lcChrToUpd
ENDIF
IF nSeq_No = 0 .AND. lnOldGenNm <> 0 AND (llPrinter OR UPPER(oAriaApplication.gcDevice) <> "SCREEN")
  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
ENDIF
IF !EMPTY(lcExtraStr)
  lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
ENDIF
UNLOCK
lnRetVal = lcUnqPreFx + PADL(lnOldGenNm, lnRetLen, "0")
IF luSequence
  USE IN Sequence
ELSE
  SET ORDER TO TAG ltSequence IN Sequence
  IF BETWEEN(leSequence,1,RECCOUNT('Sequence'))
    GOTO leSequence IN 'Sequence'
  ENDIF
ENDIF
SELECT (lcSavAlias)
RETURN(lnRetVal)



****************** Maniefest****************** 

****************************
FUNCTION lfPrintMfst
****************************

SELECT ALRepURNHst
IF !USED("ALRepTrkMnf")
  = gfOpenTable(oAriaApplication.DataDir + 'ALTrkMnf', 'ALTrkMnf', 'SH','ALRepTrkMnf')
ENDIF
SELECT DISTINCT cCallOff,Style,cDEPT,cmsStore FROM (Tmp2Print) INTO CURSOR TempURN

IF !EMPTY(TmpMnfNo) AND USED(TmpMnfNo)
  SELECT (TmpMnfNo)
  GO TOP
  SELECT ALRepTrkMnf
  =gfSeek(&TmpMnfNo..cTrkRef)
ELSE
  SELECT TempURN
  GO TOP
  SELECT ALRepURNHst
  =gfSeek(TempURN.cCallOff)
  LOCATE FOR cCallOff=TempURN.cCallOff AND cDept=TempURN.cDEPT AND cmsStore=TempURN.cDEPT
  SELECT ALRepTrkMnf
  =gfSeek(ALRepURNHst.cTrkRef)
ENDIF

lcTrcukRef = cTrkRef
lcTrkCo    = cTrkCo
lcRegNo    = cTrkReg
lcPhone    = cContPhon
lcSeal     = cSealNo
lcDate     = cProcDate
lcProcName = cProcBy
lcDrivNam  = cDrive
lcNotes    = mNotes
lcPassLoad = IIF(EMPTY(cPasLoad),"N",cPasLoad)
lcColDept  = IIF(EMPTY(cColDept),"L",cColDept)
*IF EMPTY(cTrkRef)&& OR lcRpREMNF="Y"
IF lcRpTRKMNF = "Y"
  DO FORM (oAriaApplication.ScreenHome+'\AL\ALMNF.SCX') WITH ;
    lcTrcukRef,lcTrkCo,lcRegNo,lcPhone,lcSeal,lcProcName,lcDate,lcDrivNam,lcNotes,lcPassLoad,lcColDept
ENDIF
SELECT ALRepTrkMnf
=gfSeek(lcCurrTrcukRef)
USE IN TempURN

lcFormName = 'ALManfst'
loOgScroll.lcOGLastForm =  lcFormName && LAYOUT NAME
loOGScroll.cCROrientation='P'
loOGScroll.CheckPlatForm()
=gfCrtFrm(lcFormName ,"",llOGRefForm)

SELECT cPrntManfs
IF lcRpTRKMNF ="Y"
  =lfUpdateHst()
ENDIF
IF lcRpREMNF="Y"
  =lfRestMnfHst()
ENDIF
SELECT cPrntManfs
  *-- Mahmoud Said Start
  IF CallFromExportorEmail()
  	SetExtraFileName("Furth")
  ENDIF

  loOgScroll.lnActivePDFViewerNo  = 4
  *-- Mahmoud Said End


  DO gfDispRe  WITH EVAL('lcFormName')
  *-- Mahmoud Said Start
  *-- 
  IF CallFromExport()
    OpenExtraFile("Furth")
  ENDIF
  
  IF CallFromEmail()
    AddExtraAttachment("Furth")
  ENDIF
  *-- Mahmoud Said End
  *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[Start]
  IF lcRpREMNF="Y" AND lcRpETS = "N"
    lcMsg = "Do you want to reprint SDNs for this Truck Manifest"
    IF lcRpREMNF="Y" AND gfModalGen('QRM00000B44009',.F.,.F.,.F.,lcMsg ) = 1
      lfRePrnDes()
    ENDIF   
  ENDIF  
  *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[End]

********************************
FUNCTION lfvSave
LPARAMETERS pCallingForm
********************************
*PRIVATE TxtTrcukRef,TxtTrkCo,TxtRegNo,TxtPhone,TxtSeal,TxtProcName,TxtDate,TxtDrivNam,mNotes
*PRIVATE lcTrcukRef,lcTrkCo,lcRegNo,lcPhone,lcSeal,lcProcName,lcDate,lcDrivNam,lcNotes
lcOldTrkRef = lcTrcukRef
lcTrcukRef  = pCallingForm.AriaForm1.TxtTrkRef.Value
lcTrkCo     = pCallingForm.AriaForm1.TxtTrkCo.Value
lcRegNo     = pCallingForm.AriaForm1.TxtRegNo.Value
lcPhone     = pCallingForm.AriaForm1.TxtPhone.Value
lcSeal      = pCallingForm.AriaForm1.TxtSeal.Value
lcProcName  = pCallingForm.AriaForm1.TxtProcName.Value
lcDate      = pCallingForm.AriaForm1.TxtDate.Value
lcDrivNam   = pCallingForm.AriaForm1.TxtDrivNam.Value
lcNotes     = pCallingForm.AriaForm1.mNotes.Value
lcPassLoad  = pCallingForm.AriaForm1.cboPassLoad.Value
lcColDept   = pCallingForm.AriaForm1.txtColDept.Value
SELECT TempURN   
PRIVATE llAddRecordURN,TmpGrp

SCAN 
  WAIT WINDOW "Updating Truck References…Please Wait" nowait
  TmpGrp=lfGetStyGroup(TempURN.Style)
  SELECT ALRepURNHst
*  REPLACE ALL cTrkRef WITH lcTrcukRef FOR cCallOff = TempURN.cCallOff AND cTrkRef=lcOldTrkRef AND cDept=TmpGrp

  SCAN FOR cCallOff = TempURN.cCallOff AND cTrkRef=lcOldTrkRef AND cDept=TmpGrp
    WAIT WINDOW "Validating Call OFF "+TempURN.cCallOff nowait
    SCATTER MEMVAR MEMO
    llAddRecordURN = .T.
*!*	    IF !EMPTY(TmpPktkt) AND USED(TmpPktkt)
*!*	      llAddRecordURN = llAddRecordURN AND lfValdPkTk(m.PikTkt)
*!*	    ENDIF
*!*	    IF !EMPTY(TmpWareCod) AND USED(TmpWareCod)
*!*	      llAddRecordURN = llAddRecordURN AND lfValdWhse(m.PikTkt)
*!*	    ENDIF
    IF TYPE("lcDepot")="C" AND !EMPTY(lcDepot)
      llAddRecordURN = llAddRecordURN AND (m.cDepot $lcDepot)
    ENDIF
*!*	    IF TYPE("lcOrderPti")="C" AND !EMPTY(lcOrderPti)
*!*	      llAddRecordURN = llAddRecordURN AND (lfGetPri(m.Order) = lcOrderPti)
*!*	    ENDIF
    IF TYPE("lcMsStore")="C" AND !EMPTY(lcMsStore) 
      llAddRecordURN = llAddRecordURN AND lfValdMSStr(m.cMSSTORE)
    ENDIF
    IF llAddRecordURN
      REPLACE cTrkRef WITH lcTrcukRef 
    ENDIF
  ENDSCAN
ENDSCAN
SELECT ALRepURNHst
=gfReplace('')


IF UPPER(oAriaApplication.gcDevice) <> "SCREEN"
  WAIT WINDOW "Updating URN History..." NOWAIT
  =gfTableUpdate('ALRepURNHst')
  WAIT CLEAR
ENDIF

SELECT ALRepTrkMnf
IF gfSeek(lcTrcukRef)
ELSE
  IF !gfSeek(lcOldTrkRef)
    APPEND BLANK
    REPLACE cAdd_USer WITH oAriaApplication.User_ID,;
            cAdd_Time WITH gfGettime(),;
            dAdd_Date WITH oAriaApplication.SystemDate &&,;           CTRLRSEQ  WITH gfSequence('CTRLRSEQ')

  ENDIF
ENDIF
REPLACE cTrkRef   WITH lcTrcukRef,;
        cTrkCo    WITH lcTrkCo,;
        cTrkReg   WITH lcRegNo,;
        cContPhon WITH lcPhone,;
        cSealNo   WITH lcSeal,;
        cProcDate WITH lcDate,;
        cProcBy   WITH lcProcName,;
        cDrive    WITH lcDrivNam,;
        mNotes    WITH lcNotes,;
        cPasLoad  WITH lcPassLoad,;
        cColDept  WITH lcColDept
*!*	IF EMPTY(CTRLRSEQ)
*!*	  REPLACE CTRLRSEQ  WITH gfSequence('CTRLRSEQ')
*!*	ENDIF
=gfReplace('')
IF UPPER(oAriaApplication.gcDevice) <> "SCREEN"
  WAIT WINDOW "Updating URN History..." NOWAIT
  =gfTableUpdate('ALRepTrkMnf')
  WAIT CLEAR
ENDIF
lcCurrTrcukRef = lcTrcukRef

******************************
FUNCTION lfValdURNSEQ
LPARAMETERS pcUrnSeq
******************************
PRIVATE lnOldAls,ll2Return
lnOldAls = SELECT(0)
ll2Return = .T.
SELECT (TmpURNSeq)  && This is because in some cases this file is created but hold no records
GO TOP
IF !EOF()
  LOCATE FOR cURNSeq = pcUrnSeq
  ll2Return = FOUND()
ENDIF
SELECT(lnOldAls)
RETURN(ll2Return)


FUNCTION lfValdMnfNo
LPARAMETERS pcTrkRef
******************************
PRIVATE lnOldAls,ll2Return
lnOldAls = SELECT(0)
ll2Return = .T.
SELECT (TmpMnfNo)
GO TOP
IF !EOF()
  LOCATE FOR cTrkRef= pcTrkRef
  ll2Return = FOUND()
ENDIF
SELECT(lnOldAls)
RETURN(ll2Return)



*-- Mahmoud Said Start
*-- 
*****************************************************************************************************************************************
FUNCTION CallFromExport
RETURN TYPE("Export") = 'O' .AND. TYPE('Export.Name') = 'C'


*****************************************************************************************************************************************
FUNCTION CallFromEmail
RETURN TYPE('_SCREEN.ActiveForm') = 'O'  .AND. UPPER(_SCREEN.ActiveForm.Name) = UPPER("Sendmail")


*****************************************************************************************************************************************
FUNCTION CallFromExportorEmail
RETURN (TYPE("Export") = 'O' .AND. TYPE('Export.Name') = 'C') .OR. ;
       (TYPE('_SCREEN.ActiveForm') = 'O'  .AND. UPPER(_SCREEN.ActiveForm.Name) = UPPER("Sendmail"))

*****************************************************************************************************************************************
FUNCTION SetGlobals
PUBLIC gcExportType, gcExportFileName
IF TYPE('Export') = 'O' .AND. TYPE('Export.Name') = 'C'
  gcExportType = IIF(ALLTRIM(Export.txtPath.Text) == oAriaApplication.gcOutFile, 'E', 'P')
  Export.StopPreview = .T.
ENDIF
gcExportFileName = oAriaApplication.gcOutFile

*****************************************************************************************************************************************
FUNCTION GetMainFileName
IF TYPE('Export') = 'O' .AND. TYPE('Export.Name') = 'C' .AND. !EMPTY(ALLTRIM(Export.txtPath.Text))
  RETURN ALLTRIM(Export.txtPath.Text)
ELSE
  RETURN gcExportFileName
ENDIF

*****************************************************************************************************************************************
FUNCTION GetExtraFileName
LPARAMETERS Extension

LOCAL lcMainFileName 
lcMainFileName = GetMainFileName()

RETURN SUBSTR(lcMainFileName, 1, LEN(lcMainFileName) - 4) + Extension + SUBSTR(lcMainFileName, LEN(lcMainFileName) - 3)


*****************************************************************************************************************************************
FUNCTION SetExtraFileName
LPARAMETERS Extension

oAriaApplication.gcOutFile = GetExtraFileName(Extension)


*****************************************************************************************************************************************
FUNCTION OpenMainFile

*-- Check if the user select preview
IF gcExportType = 'P'
  LOCAL loRun
  IF FILE(GetMainFileName())
    loRun = CreateObject("WScript.Shell")
    loRun.Run(GetMainFileName(), 3)
    loRun = NULL
  ENDIF
ENDIF


*****************************************************************************************************************************************
FUNCTION OpenExtraFile
LPARAMETERS Extension

IF gcExportType = 'P'
  LOCAL loRun
  IF FILE(GetExtraFileName(Extension))
    loRun = CreateObject("WScript.Shell")
    loRun.Run(GetExtraFileName(Extension), 3)
    loRun = NULL
  ENDIF
ENDIF


*****************************************************************************************************************************************
FUNCTION AddExtraAttachment
LPARAMETERS Extension

IF TYPE('_SCREEN.ActiveForm.laExtraAttach') = 'L'
  DIMENSION _SCREEN.ActiveForm.laExtraAttach[1]
ELSE
  DIMENSION _SCREEN.ActiveForm.laExtraAttach[ALEN(_SCREEN.ActiveForm.laExtraAttach, 1) + 1]
ENDIF
  
_SCREEN.ActiveForm.laExtraAttach[ALEN(_SCREEN.ActiveForm.laExtraAttach, 1)] = GetExtraFileName(Extension)
*-- Mahmoud Said End



*WLD ***** P R I N T     E T O S   F I L E   *********************************************
*:*************************************************************
*: Name      : lfPrintETOS
*: Developer : Waleed Hamed (WLD)
*: Date      : 11/29/2006
*: Purpose   : Funtion to create ETOS file for each CallOff # .
*:*************************************************************
*: Calls     :
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfPrintETOS ()
*:*************************************************************
*:
FUNCTION lfPrintETOS

  *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[Start]
  lcSdnFile = loogscroll.gfTempName()
  lfCrtTmpSdn()
  IF !USED('SDNHIST')
    gfOpenTable('SDNHIST','SDNHIST')
  ENDIF 
  *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[End]


  PRIVATE lnOldAls,lcRpPassWord
  lnOldAls = SELECT(0)
  USE (oAriaApplication.defaultpath+"Ftp_Info") SHARED AGAIN IN 0
  SELECT Ftp_Info
  GO TOP
  lcRpPassWord = PADL(cPassWord,8)
  USE IN Ftp_Info
  SELECT (lnOldAls)


  PRIVATE lcTempManfst
  STORE 0 TO lnFilHandl
  STORE '' TO lcRepPath,lcmsgFile
  lcHoursFormat =  SET('HOURS')
  SET HOURS TO 24
  IF lcRpREMNF="Y"
    SELECT (TmpMnfNo)
    GO TOP
    lcTempManfst = cTrkRef
    lcTrkRef   = cTrkRef
    SELECT ALRepURNHst
    SCAN FOR cTrkRef=lcTempManfst
      SCATTER MEMVAR MEMO
      WAIT WINDOW "Restore ETOS Data...Please wait" nowait
      SELECT (lcETOFTmp)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE npack   WITH m.npackno,;
              totpck  WITH m.npckqty,;
              cunique WITH m.curnseq,;
              qty     WITH ALLTRIM(cQty)
    ENDSCAN
    WAIT clear
  ENDIF

  SELECT cPrntmanfs
  INDEX ON  cCalloff TAG  'cCalloff' ADDITIVE
  SET ORDER TO  cCalloff

  lcSegSuffix = '='
  lcLineSep   = "'"
  lcFldSep    = '+'
  lcDataElm   = ':'
  lcRepPath = lfvFpath()

  IF EMPTY(lcRepPath)
    *- Message Text   :- ETOS File path not selected. can not proceed.
    *- Message No.    :- 000000.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM00000B00000','F','ALERT',' ','ETOS File path not selected. can not proceed.')
    RETURN
  ENDIF
  lcSuppMailcode = gfGetMemVar('M_ETOSANA' , oAriaApplication.ActiveCompanyId)
  lcSuppMailcode1 = gfGetMemVar('M_ETOSANA' , oAriaApplication.ActiveCompanyId)
  lcSuppBarcode  = gfGetMemVar('M_SUPBRCD' , oAriaApplication.ActiveCompanyId)
  lcCmpEANLoc    = gfGetMemVar('M_CMPEANLC' , oAriaApplication.ActiveCompanyId)
  lcSysTime = gfGetTime()
  lcSysTime = LEFT(STRTRAN(lcSysTime,':',''),4)
  lcSysDate = STRTRAN(DTOC(gdSysdate),'/','')
  lcSysDate  = RIGHT(lcSysDate,2)+SUBSTR(lcSysDate,3,2)+LEFT(lcSysDate,2)

  SELECT ALURNHst
  =gfSetOrder("ALURNSTR")
  SELECT DISTINCT cCalloff ,cDept FROM cPrntmanfs INTO CURSOR curCallOff
  SELECT DISTINCT cDept FROM cPrntmanfs INTO CURSOR curDept
  SELECT curDept
  SCAN
    SELECT curCallOff
    SCAN FOR cDept=curDept.cDept
      lcCalloff = cCalloff
      STORE 0 TO lnTotPkCalOf,lnTotURN,lnLnDtl,lnDLDSeq
      lcFileSeq = gfsequence('CETOSSEQU')
      IF lcFileSeq = '9999'
        IF !USED('SEQUENCE')
          = gfOpenTable(oAriaApplication.DataDir + 'SEQUENCE', 'cSeq_Type', 'SH')
        ENDIF
        SELECT SEQUENCE
        =gfSetOrder("Cseq_type")
        IF gfSEEK(PADR('CETOSSEQU',10),'SEQUENCE')
          REPLACE nSeq_No WITH 0 IN SEQUENCE
          lcFileSeq = gfsequence('CETOSSEQU')
        ENDIF
      ENDIF
      lcSDNchk = lfSDNno(lcSuppBarcode+RIGHT(lcFileSeq,4))
      lcRpfName = lcSuppBarcode+RIGHT(lcFileSeq,4) + lcSDNchk
      lcFile = lcRepPath+ALLTRIM(lcRpfName)+'.SDN'
      
      *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[Start]
      m.SDN_no = ALLTRIM(lcRpfName) 
      *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[End]

      
      lnFilHandl = FCREAT(lcFile)
      IF lnFilHandl < 0
        = gfModalGen('INM00000B00000','F','ALERT',' ','ETOS File can not be create. can not proceed.')
        RETURN
      ENDIF

      SELECT ccalloff,cmsstore,cdepot,cDept,MAX(npack),MIN(cunique) minurn,MAX(cunique) maxurn ,SUM(VAL(qty)) TotPcs FROM (lcETOFTmp) ;
        WHERE  ccalloff =lccalloff AND cDept=curDept.cDept GROUP BY ccalloff,cmsstore,cdepot,cDept ORDER BY ccalloff,minurn;
        INTO CURSOR CurURNDt
      LOCATE
      SUM TotPcs TO lnTotPkCalOf
      LOCATE
      SUM max_npack TO lnTotURN
       =gfseek(lcCalloff,'ALURNHst')
      IF EMPTY(lcTrkRef)
        lcTrkRef   = ALURNHst.cTrkRef
      ENDIF
      SELECT AlTrkMnf
      =gfseek(lcTrkRef,'AlTrkMnf')
      REPLACE CETOSSEQU WITH RIGHT(lcFileSeq,4)
      *=SEEK(lcCalloff,'cPrntmanfs')
      
      *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[start]
      m.Ctrkref = lcTrkRef
      m.csealno = Altrkmnf.csealno
      
      SELECT 'SDNHIST'
      APPEND BLANK 
      REPLACE CTRKREF WITH m.Ctrkref,;
              CSDNNO  WITH m.SDN_no,;
              cdept   WITH curDept.cDept,;
              cCalloff WITH lcCalloff 

      *! C201047,2 MMT 09/17/2008 update Add_user info in SDNHIST File[Start]
      REPLACE cAdd_User  WITH oAriaApplication.User_ID ,;
    	      dAdd_Date  WITH DATE()    ,;
	          cAdd_Time  WITH gfGetTime()
      *! C201047,2 MMT 09/17/2008 update Add_user info in SDNHIST File[End]
                
      gfReplace('')        
      gfTableUpdate()

      
      LOCAL lnRemoteResult, lcSelectCommand
	  lcSelectCommand = [SELECT * FROM SYCCOMP WHERE CCOMP_ID = '] + oAriaApplication.ActiveCompanyID+ [']
      lnRemoteResult = loOGScroll.SQLExecute("SYCCOMP", lcSelectCommand,"","SYCCOMP","",;
	  oAriaApplication.SystemConnectionString,3,"")
	  IF lnRemoteResult >= 1 
        m.caddress1 =  gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
        m.caddress2 =  gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
        m.caddress3 =  gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
        m.caddress4 =  gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
        m.caddress5 =  gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
      ENDIF 
  	  m.cBarcode = lfGetBarCode(cPrntManfs.cCallOff)
      m.cSeries  = cPrntManfs.Series
      m.cdept =  curDept.cDept
      m.cCallOff  =lcCallOff 
      *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[End]

      
      
      SELECT cPrntmanfs
      LOCATE FOR ccalloff =lccalloff AND cDept=curDept.cDept
      *lcTrlrseq  = PADL(ALLTRIM(AlTrkMnf.cTrlrseq),6,'0')
      lcTrlrseq  = ALLTRIM(gfSequence('CTRLRSEQ'))
      lcDept= ALLTRIM(curDept.cDept)
      lcTrkDate = STRTRAN(DTOC(AlTrkMnf.cProcDate),'/','')
      lcTrkDate  = RIGHT(lcTrkDate,2)+SUBSTR(lcTrkDate,3,2)+LEFT(lcTrkDate,2)
      lcWarecd = cPrntManfs.Series
  *!*	    LCRPLCTEXT  = 'STX' + lcSegSuffix + 'ANA' + lcDataElm + '1' +;
  *!*	      lcFldSep + PADR(lcCmpEANLoc,13,' ')+ lcDataElm + ALLTRIM(OAriaApplication.ActiveCompanyName)+;
  *!*	      lcFldSep + PADR(lcSuppMailcode1,13,' ')+;
  *!*	      lcFldSep + lcSysDate + lcDataElm + PADR(lcSysTime,6,'0') +;
  *!*	      lcFldSep + lcWarecd + lcSuppBarcode + lcTrlrseq +;
  *!*	      lcFldSep + 'XXXXXXXX' + lcFldSep + 'MSSDN' + lcLineSep
    LCRPLCTEXT  = 'STX' + lcSegSuffix + 'ANA' + lcDataElm + '1' +;
      lcFldSep + PADR(lcCmpEANLoc,13,' ')+ lcDataElm + ALLTRIM(OAriaApplication.ActiveCompanyName)+;
      lcFldSep + PADR(lcSuppMailcode1,13,' ')+;
      lcFldSep + lcSysDate + lcDataElm + PADR(lcSysTime,6,'0') +;
      lcFldSep + lcWarecd + lcSuppBarcode + lcTrlrseq +;
      lcFldSep + lcRpPassWord + lcFldSep + 'MSSDN' + lcLineSep
      =FPUTS(lnFilHandl,lcRplctext)
      *-- Function to Get the 1 part - Header.
      = lfPrt1Part ()
      *-- Function to Get the 2 part - Detail.
      = lfPrt2Part ()
      *-- Function to Get the 3 part - Summary .
      = lfPrt3Part ()
      IF !(lnFilHandl < 0)
        DO WHILE !FCLOSE(lnFilHandl)
        ENDDO
      ENDIF
      lcmsgFile = lcmsgFile +'- '+ lcFile
    ENDSCAN
  ENDSCAN
  lcMessage = 'ETOS File(s) ' + lcmsgFile +' - created successfully.'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)

  SET HOURS TO lcHoursFormat
  
  *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[Start]
  SELECT (lcSdnFile )
  LOCATE 
  IF !EOF()
    loOgScroll.lcOGLastForm  = 'ALSDNRP'
    loogScroll.cCROrientation = 'P'
    LoOGScroll.llCrystal = .T.
    USE IN (lcSdnFile )
    DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
    loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcSdnFile +'.DBF' 
    =gfDispRe()
   
    
  ENDIF 
  *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[End]


  *-- End of lfPrintETOS
  *:*************************************************************
  *: Name      : lfPrt1Part
  *: Developer : Waleed Hamed (WLD)
  *: Date      : 11/29/2006
  *: Purpose   : Funtion to get the hedaer part.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Passed Parameters  : ............
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : = lfPrt1Part ()
  *:*************************************************************
  *:
FUNCTION lfPrt1Part
  *-- R e c o r d   1  MHD --*
  lcRplctext = 'MHD' + lcSegSuffix + '1' + lcFldSep + 'DELHDR' + lcDataElm + '9' + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  *-- R e c o r d   2 TYP --*
  lcRplctext = 'TYP' + lcSegSuffix + '0610' + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  *-- R e c o r d   3 SDT --*
  lcRplctext = 'SDT' + lcSegSuffix + PADR(lcCmpEANLoc,13) + lcDataElm &&+ PADR(lcWarecd+lcSuppBarcode,8) + lcFldSep + ALLTRIM(OAriaApplication.ActiveCompanyName) + lcLineSep
*! C200787,1 change BarCode ID, pick it from warehous file
  lcRplctext = lcRplctext+lcWarecd+lfGetBarCode(cPrntManfs.cCallOff) + lcFldSep + ALLTRIM(OAriaApplication.ActiveCompanyName) + lcLineSep
*! C200787,1 change BarCode ID, pick it from warehous file
  =FPUTS(lnFilHandl,lcRplctext)
  *-- R e c o r d   4   CDT --*
  lcRplctext = 'CDT' + lcSegSuffix +  PADR(lcSuppMailcode,13)+ lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  *-- R e c o r d   5 DNA --*
  lcRplctext = 'DNA' + lcSegSuffix + '1'+ lcFldSep +lcFldSep+'153'+ lcDataElm + ALLTRIM(lcTrkRef)+;
    lcDataElm + '165' + lcDataElm + ALLTRIM(AlTrkMnf.cColDept) + lcDataElm + '152' + lcDataElm +;
    PADL(AlTrkMnf.cSealNo,4,'0') + lcDataElm + '148' + lcDataElm + lcTrkDate + lcSysTime + lcFldSep + ALLTRIM(AlTrkMnf.cPasLoad) + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  *-- R e c o r d   7 FIL --*
  lcRplctext = 'FIL'+ lcSegSuffix + '1' + lcFldSep + '1' + lcFldSep + lcSysDate + lcFldSep + lcTrlrseq + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  *-- R e c o r d   8 MTR --*
  lcRplctext = 'MTR'+ lcSegSuffix + '7' + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  *-- R e c o r d   1 MHD --*  Delivery Detail
  lcRplctext = 'MHD'+ lcSegSuffix + '2' + lcFldSep + 'DELIVR' + lcDataElm + '9' + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  lnLnDtl = lnLnDtl + 1
  *-- R e c o r d   2 CLO --*
  lcRplctext = 'CLO'+ lcSegSuffix + lcDataElm + ALLTRIM(AlTrkMnf.cColDept)+ lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  lnLnDtl = lnLnDtl + 1

  *-- R e c o r d   3 DEL --*
  lcRplctext = 'DEL' + lcSegSuffix + lfGetBarCode(cPrntManfs.cCallOff)+lcRpfName + lcFldSep + ALLTRIM(STR(lnTotURN))  +;
    lcFldSep + lcFldSep + lcFldSep + lcFldSep + lcFldSep + lcFldSep + lcFldSep +;
    ALLTRIM(STR(lnTotPkCalOf)) + lcDataElm + ALLTRIM(STR(lnTotURN)) + lcDataElm + 'N' + lcLineSep


  =FPUTS(lnFilHandl,lcRplctext)
  lnLnDtl = lnLnDtl + 1
  *-- R e c o r d   4 ORF --*
  lcRplctext = 'ORF' + lcSegSuffix + '1' + lcFldSep + ALLTRIM(lcDept)+ SPACE(1)  + lcCallOff + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  lnLnDtl = lnLnDtl + 1
  *-- End OF lfPrt1Part
  *:*************************************************************
  *: Name      : lfPrt2Part
  *: Developer : Waleed Hamed (WLD)
  *: Date      : 11/29/2006
  *: Purpose   : Funtion to get the Detail part.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Passed Parameters  : ............
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : = lfPrt2Part ()
  *:*************************************************************
  *:
FUNCTION lfPrt2Part

  *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[Start]
  lnSeq = 0
  *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[End]


  SELECT curURNDt
  SCAN
  
    *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[Start]
    m.nLINENo = lnSeq+1
    m.cMSStore  = curURNDt.cMSStore 
    m.storename = gfCodDes(curURNDt.cMSStore , 'CMSSTORE')
    m.Start =VAL(curURNDt.minURN )
    m.End =VAL(curURNDt.MaxURN)
    m.nSet = curURNDt.max_npack
    m.nSingls =curURNDt.TotPcs
    lnSeq = lnSeq+1
    INSERT INTO (lcSdnFile) FROM MEMVAR 
    *! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[End]

    lnDLDSeq = lnDLDSeq + 1
    *-- R e c o r d   8 DLD --*
    lcRplctext = 'DLD' + lcSegSuffix + '1' + lcFldSep + ALLTRIM(STR(lnDLDSeq)) + lcFldSep + ;
      lcDataElm + curURNDt.minURN + curURNDt.MaxURN + lcFldSep + lcFldSep + lcFldSep + ;
      '1'+lcFldSep+ ALLTRIM(STR(curURNDt.max_npack))+lcLineSep
    =FPUTS(lnFilHandl,lcRplctext)
    lnLnDtl = lnLnDtl + 1
    *-- R e c o r d   9 DNC --*
    lcRplctext = 'DNC' + lcSegSuffix + '1' + lcFldSep + ALLTRIM(STR(lnDLDSeq)) + lcFldSep + '1' + lcFldSep + lcFldSep + ;
      '165' + lcDataElm + curURNDt.cDepot + curURNDt.cMSStore + lcDataElm + '185'+ lcDataElm +;
      ALLTRIM(STR(curURNDt.max_npack))+ lcDataElm + '199' + lcDataElm + ALLTRIM(STR(curURNDt.TotPcs))+ lcLineSep
    =FPUTS(lnFilHandl,lcRplctext)
    lnLnDtl = lnLnDtl + 1
  ENDSCAN
  *-- End OF lfPrt2Part
  *:*************************************************************
  *: Name      : lfPrt3Part
  *: Developer : Waleed Hamed (WLD)
  *: Date      : 11/29/2006
  *: Purpose   : Funtion to get the Summary part.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Passed Parameters  : ............
  *:*************************************************************
  *: Returns            : ............
  *:*************************************************************
  *: Example   : = lfPrt3Part ()
  *:*************************************************************
  *:
FUNCTION lfPrt3Part
  *-- R e c o r d   1 DTR --*
  lcRplctext = 'DTR' + lcSegSuffix + ALLTRIM(STR(lnDLDSeq)) + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  lnLnDtl = lnLnDtl + 1
  *-- R e c o r d   2 MTR --*
  lcRplctext = 'MTR' + lcSegSuffix + ALLTRIM(STR(lnLnDtl + 1)) +lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  *-- R e c o r d   3 MHD --*
  lcRplctext = 'MHD' + lcSegSuffix + '3' + lcFldSep + 'DELTLR' + lcDataElm + '9' + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  *-- R e c o r d   4 DFT --*
  lcRplctext = 'DFT' + lcSegSuffix + '1' + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  *-- R e c o r d   5 MTR --*
  lcRplctext = 'MTR' + lcSegSuffix + '3' + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)
  *-- R e c o r d   6 END --*
  lcRplctext = 'END' + lcSegSuffix + '3' + lcLineSep
  =FPUTS(lnFilHandl,lcRplctext)

  *-- End OF lfPrt3Part
  *:*************************************************************
  *: Name      : lfvFpath
  *: Developer : Waleed Hamed (WLD)
  *: Date      : 11/29/2006
  *: Purpose   : Funtion to get the file path .
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Passed Parameters  : ............
  *:*************************************************************
  *: Returns            : Selected path
  *:*************************************************************
  *: Example   : =lfvFpath()
  *:*************************************************************
  *:
FUNCTION lfvFpath
PRIVATE lcoldPath,lnOldAls,lnRepPath

lnOldAls = SELECT(0)
USE (oAriaApplication.defaultpath+"Ftp_Info") SHARED AGAIN IN 0
SELECT Ftp_Info
GO TOP
lnRepPath = ALLTRIM(cOutgoing)
USE IN Ftp_Info
SELECT (lnOldAls)

lcoldPath = FULLPATH('')
lcRepPath =  GETDIR(lnRepPath,"","")
SET DEFA TO &lcoldPath
RETURN lcRepPath

  *-- End of lfvFpath
  *:*************************************************************
  *: Name      : lfSDNno
  *: Developer : Waleed Hamed (WLD)
  *: Date      : 11/29/2006
  *: Purpose   : Funtion to calculate SDN # CHECK DIGIT.
  *:*************************************************************
  *: Calls     :
  *:             Procedures : ....
  *:             Functions  : ....
  *:*************************************************************
  *: Passed Parameters  : lcSDNno
  *:*************************************************************
  *: Returns            : SDN # CHECK DIGIT
  *:*************************************************************
  *: Example   : =lfSDNno()
  *:*************************************************************
  *:
FUNCTION lfSDNno
  PARAMETERS lcSDNno
  STORE 0 TO lnCalc

  FOR i= 1 TO 7
    DO CASE
      CASE i=1
        lnCalc = lnCalc + VAL(SUBSTR(lcSDNno,i,1))*8
      CASE i=2
        lnCalc = lnCalc + VAL(SUBSTR(lcSDNno,i,1))*4
      CASE i=3
        lnCalc = lnCalc + VAL(SUBSTR(lcSDNno,i,1))*6
      CASE i=4
        lnCalc = lnCalc + VAL(SUBSTR(lcSDNno,i,1))*3
      CASE i=5
        lnCalc = lnCalc + VAL(SUBSTR(lcSDNno,i,1))*5
      CASE i=6
        lnCalc = lnCalc + VAL(SUBSTR(lcSDNno,i,1))*2
      CASE i=7
        lnCalc = lnCalc + VAL(SUBSTR(lcSDNno,i,1))*1
    ENDCASE
  ENDFOR
  lnchktmp = 10- (lnCalc - (INT((lnCalc/11)) * 11))
  DO CASE
    CASE lnchktmp = 0
      lcSDNchk = 'A'
    CASE lnchktmp = 1
      lcSDNchk = 'B'
    CASE lnchktmp = 2
      lcSDNchk = 'D'
    CASE lnchktmp = 3
      lcSDNchk = 'H'
    CASE lnchktmp = 4
      lcSDNchk = 'J'
    CASE lnchktmp = 5
      lcSDNchk = 'L'
    CASE lnchktmp = 6
      lcSDNchk = 'N'
    CASE lnchktmp = 7
      lcSDNchk = 'P'
    CASE lnchktmp = 8
      lcSDNchk = 'R'
    CASE lnchktmp = 9
      lcSDNchk = 'T'
    CASE lnchktmp = 10
      lcSDNchk = 'W'
  ENDCASE
  RETURN  lcSDNchk
  *-- End of lfSDNno

FUNCTION lfGetPkSzByCallOff
LPARAMETERS pcCallOff

PRIVATE lnOldAls,ln2Return
lnOldAls = SELECT(0)
ln2Return = 0
SELECT ALCalOff
LOCATE FOR cCallOff=pcCallOff
IF FOUND()
  ln2Return=lfGetPkNo(ALCalOff.Order)
ENDIF
SELECT(lnOldAls)
RETURN(ln2Return)
*! C200787,1 change BarCode ID, pick it from warehous file
FUNCTION lfGetBarCode
LPARAMETERS lcCallOff
*! C200787,1 change BarCode ID, pick it from warehous file
PRIVATE lnOldAls,lc2Return
lnOldAls = SELECT(0)
lc2Return=""
SELECT AlCalOff
LOCATE FOR cCallOff = lcCallOff
IF FOUND()
  SELECT ORdHdr
  LOCATE FOR cOrdType+Order="O"+AlCalOff.Order
  SELECT Warehous
  LOCATE FOR cWareCode = ORdHdr.cWareCode
  lc2Return = SUBSTR(ALLTRIM(Warehous.cSupBrCd),1,3)
ENDIF
SELECT(lnOldAls)
RETURN (lc2Return)


FUNCTION lfUpdateHst

PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT cPrntManfs

SCAN
  SCATTER MEMVAR MEMO
  SELECT almnfhst
  LOCATE FOR CtrkRef = lcTrcukRef AND cDepot = m.cDepot AND cMssTore = m.cMssTore AND cCallOff = m.cCallOff AND Style=m.Style;
             AND nPack = m.nPack
  IF !FOUND()
    APPEND BLANK
    m.CtrkRef = lcTrcukRef
    m.cQty=m.Qty
    GATHER MEMVAR MEMO
  ENDIF
ENDSCAN
SELECT almnfhst
IF UPPER(oAriaApplication.gcDevice) <> "SCREEN"
  =gfTableUpdate('almnfhst')
ENDIF
SELECT(lnOldAls)

FUNCTION lfRestMnfHst
SELECT cPrntManfs
DELETE  ALL
SELECT almnfhst 
SCAN FOR CtrkRef = lcTrcukRef
  SCATTER MEMVAR MEMO
  m.Qty=m.cQty
  INSERT INTO cPrntManfs FROM MEMVAR
ENDSCAN
SELECT almnfhst 
SELECT DISTINCT totpck FROM almnfhst INTO CURSOR TempSum WHERE CtrkRef = lcTrcukRef
SELECT TempSum
SUM totpck TO lntmpsum
SELECT cPrntManfs
REPLACE ALL totpck WITH lntmpsum


FUNCTION lfvTrkRef
LPARAMETERS pCallingForm
PRIVATE lnOldAls
SELECT ALRepTrkMnf
IF gfSeek(pCallingForm.AriaForm1.TxtTrkRef.Value)
  pCallingForm.AriaForm1.TxtTrkCo.Value =cTrkCo
  pCallingForm.AriaForm1.TxtRegNo.Value = cTrkReg
  pCallingForm.AriaForm1.TxtPhone.Value = cContPhon
  pCallingForm.AriaForm1.TxtSeal.Value=cSealNo
  pCallingForm.AriaForm1.TxtProcName.Value=cProcBy
  pCallingForm.AriaForm1.TxtDate.Value=cProcDate
  pCallingForm.AriaForm1.TxtDrivNam.Value=cDrive
  pCallingForm.AriaForm1.mNotes.Value=mNotes
  pCallingForm.AriaForm1.cboPassLoad.Value=IIF(EMPTY(cPasLoad),"N",cPasLoad)
  pCallingForm.AriaForm1.txtColDept.Value=IIF(EMPTY(cColDept),"L",cColDept)
ENDIF


FUNCTION lfDisplyURN

SELECT * FROM (lcURNfile) INTO CURSOR (lcETOFTmp) READWRITE
USE IN (lcURNfile)
=lfAdjustCRSettings()
*-- Mahmoud Said Start
IF CallFromExportorEmail()
  SetExtraFileName("Third")
ENDIF
loOgScroll.lnActivePDFViewerNo  = 3
*-- Mahmoud Said End
=gfDispRe()
*-- Mahmoud Said Start
IF CallFromExport()
  OpenExtraFile("Third")
ENDIF
IF CallFromEmail()
  AddExtraAttachment("Third")
ENDIF



FUNCTION lfRePrnURN

PRIVATE llAddRecordURN,lcTmpUrnSeq
SELECT (TmpURNSeq)
SCAN
  lcTmpUrnSeq=CURNSEQ
  SELECT ALRepURNHst
  SCAN FOR CURNSEQ=lcTmpUrnSeq
    SCATTER MEMVAR MEMO
    SELECT ALCalOff
    LOCATE FOR cCallOff=m.cCallOff AND PikTkt=m.PikTkt AND cMsStore = m.cMsStore AND cDepot = m.cDepot

    llAddRecordURN = .T.
    IF !EMPTY(TmpPktkt) AND USED(TmpPktkt)
      llAddRecordURN = llAddRecordURN AND lfValdPkTk(m.PikTkt)
    ENDIF
    IF !EMPTY(TmpWareCod) AND USED(TmpWareCod)
      llAddRecordURN = llAddRecordURN AND lfValdWhse(m.PikTkt)
    ENDIF
    IF TYPE("lcDepot")="C" AND !EMPTY(lcDepot)
      llAddRecordURN = llAddRecordURN AND (m.cDepot $lcDepot)
    ENDIF
    IF TYPE("lcMsStore")="C" AND !EMPTY(lcMsStore) 
      llAddRecordURN = llAddRecordURN AND lfValdMSStr(m.cMSSTORE)
    ENDIF
    IF TYPE("lcStyGrp")="C" AND !EMPTY(lcStyGrp) 
      llAddRecordURN = llAddRecordURN AND lfValdStGrp(ALCalOff.Style)
    ENDIF
&&gfCodDes(m.cDepot, 'CDEPOT    ')
    IF llAddRecordURN
      SELECT (lcURNfile)
      APPEND BLANK
      REPLACE cDepot    WITH m.cDepot,;
              cMsStore  WITH m.cMsStore,;
              cDept     WITH m.cDept,;
              cCallOff  WITH m.cCallOff,;
              Series    WITH "0"+lfGetLWareH(ALCalOff.Order),;
              NPACK     WITH nPackNo,;
              TotPck    WITH nPckQty,;
              Qty       WITH PADL(ALLTRIM(cQty),3,'0'),;
              Desptch   WITH lfGetBarCode(m.cCallOff) ,;
              cUnique   WITH m.CURNSEQ,;
              cDeptBar  WITH "0"+lfGetStyGroup(ALCalOff.Style,.T.),;
              cDepotDes WITH gfCodDes(m.cMsStore, 'CMSSTORE  '),;
              Style     WITH ALCalOff.Style
    ENDIF
  ENDSCAN
ENDSCAN

FUNCTION lfGetMfst

PRIVATE lnTotLabSets,lcTKey
lnTotLabSets = lfCountTotLables()

SELECT * FROM (lcURNfile) WHERE .F. INTO CURSOR TmpMnfst READWRITE
SELECT (Tmp2Print)
GO TOP
DO WHILE !EOF()
  SCATTER MEMVAR MEMO
  WAIT WINDOW "Collecting Manufest recoreds for CALL OFF : "+m.cCallOff+" STORE: "+m.cMsStore +" STYLE : "+m.Style NOWAIT
  lcTKey = m.cDepot+m.cMsStore+m.cCallOff+m.Style
    *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[Start]
  IF !EMPTY(m.Pack_ID)
   lnPackSize = lfGetPkSz(m.Pack_ID,m.Style)
   IF lnPackSize = 0
     = gfModalGen('INM00000B00000','F','ALERT',' ','Pack ID '+m.Pack_ID+' for call off '+m.cCallOff+' style '+m.Style+' does not exist in the Pack file - please investigate and correct before creating URN labels')
     RETURN 
   ENDIF 
  ELSE
  *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[End]

  lnPackSize = lfGetPkNo(m.Order)
  *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[Start]
  ENDIF 
  *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[End]
  
  SUM REST TotQty TO lnTotQty While cDepot+cMsStore+cCallOff+Style = lcTKey
  lnLabelNo = CEILING(lnTotQty/lnPackSize)
  FOR lnLblNo = 1 TO lnLabelNo
    lnQty = MIN(lnPackSize,lnTotQty)
    lnTotQty = ABS(MAX(lnTotQty - lnQty,0))
    SELECT TmpMnfst
    APPEND BLANK
    REPLACE cDepot    WITH m.cDepot,;
            cMsStore  WITH m.cMsStore,;
            cDept     WITH lfGetStyGroup(m.Style),;
            cCallOff  WITH m.cCallOff,;
            Series    WITH "0"+lfGetLWareH(m.Order),;
            NPACK     WITH lnLblNo,;
            TotPck    WITH lnTotLabSets,;
            Qty       WITH PADL(ALLTRIM(STR(lnQty)),3,'0'),;
            Desptch   WITH "0"+lfGetLWareH(m.Order,.T.),;
            cDeptBar  WITH "0"+lfGetStyGroup(m.Style,.T.),;
            cDepotDes WITH gfCodDes(m.cDepot, 'CDEPOT    '),;
            Style     WITH m.Style
  ENDFOR
  SELECT (Tmp2Print)
ENDDO

SELECT *,"99" AS TmpKey FROM TmpMnfst INTO CURSOR cPrntManfs ORDER BY Style READWRITE
USE IN TmpMnfst


FUNCTION lfCountTotLables

PRIVATE lnTotQty,lcTKey,lnTotLabSets,lnOldAls
lnTotQty = 0
lnTotLabSets=0
lnOldAls=SELECT(0)
SELECT (Tmp2Print)
INDEX ON cDepot+cMsStore+cCallOff+Style TAG URNLBL ADDITIVE
SET ORDER TO TAG URNLBL
SELECT (Tmp2Print)
GO TOP
DO WHILE !EOF()
  SCATTER MEMVAR MEMO
  lcTKey = m.cDepot+m.cMsStore+m.cCallOff
  
  *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[Start]
  IF !EMPTY(m.Pack_ID)
   lnPackSize = lfGetPkSz(m.Pack_ID,m.Style)
   IF lnPackSize = 0
     = gfModalGen('INM00000B00000','F','ALERT',' ','Pack ID '+m.Pack_ID+' for call off '+m.cCallOff+' style '+m.Style+' does not exist in the Pack file - please investigate and correct before creating URN labels')
     RETURN 
   ENDIF 
  ELSE
  *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[End]

  lnPackSize = lfGetPkNo(m.Order)
  
  *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[Start]
  ENDIF 
  *! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[End]
  
  SUM REST TotQty TO lnTotQty While cDepot+cMsStore+cCallOff+Style = lcTKey
  lnLabelNo = CEILING(lnTotQty/lnPackSize)
  FOR lnLblNo = 1 TO lnLabelNo
    WAIT WINDOW "Calculate Lables for CALL OFF : "+m.cCallOff+" STORE: "+m.cMsStore NOWAIT
    lnQty = MIN(lnPackSize,lnTotQty)
    lnTotQty = ABS(MAX(lnTotQty - lnQty,0))
    SELECT (lcURNfile)
    lnTotLabSets=lnTotLabSets+1
  ENDFOR
  SELECT (Tmp2Print)
ENDDO
RETURN (lnTotLabSets)


FUNCTION lfRePrntMfst
SELECT *,"99" AS TmpKey FROM (lcURNfile) WHERE .F. INTO CURSOR cPrntManfs ORDER BY Style READWRITE


*! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[T20080807.0001]
*!*************************************************************
*! Name      : lfCrtTmpSdn
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/27/2008
*! Purpose   : Create Temp File For despatch info.
*!*************************************************************
FUNCTION lfCrtTmpSdn
DIMENSION laSdnFlStr[19,4]

laSdnFlStr[1,1] = 'SDN_no'
laSdnFlStr[1,2] = 'C'
laSdnFlStr[1,3] = 10
laSdnFlStr[1,4] = 0

laSdnFlStr[2,1] = 'Ctrkref'
laSdnFlStr[2,2] = 'C'
laSdnFlStr[2,3] = 20
laSdnFlStr[2,4] = 0

laSdnFlStr[3,1] = 'csealno'
laSdnFlStr[3,2] = 'C'
laSdnFlStr[3,3] = 15
laSdnFlStr[3,4] = 0

laSdnFlStr[4,1] = 'caddress1'
laSdnFlStr[4,2] = 'C'
laSdnFlStr[4,3] = 30
laSdnFlStr[4,4] = 0


laSdnFlStr[5,1] = 'caddress2'
laSdnFlStr[5,2] = 'C'
laSdnFlStr[5,3] = 30
laSdnFlStr[5,4] = 0

laSdnFlStr[6,1] = 'caddress3'
laSdnFlStr[6,2] = 'C'
laSdnFlStr[6,3] = 30
laSdnFlStr[6,4] = 0

laSdnFlStr[7,1] = 'caddress4'
laSdnFlStr[7,2] = 'C'
laSdnFlStr[7,3] = 30
laSdnFlStr[7,4] = 0

laSdnFlStr[8,1] = 'caddress5'
laSdnFlStr[8,2] = 'C'
laSdnFlStr[8,3] = 30
laSdnFlStr[8,4] = 0

laSdnFlStr[9,1] = 'cBarcode'
laSdnFlStr[9,2] = 'C'
laSdnFlStr[9,3] = 3
laSdnFlStr[9,4] = 0

laSdnFlStr[10,1] = 'cSeries'
laSdnFlStr[10,2] = 'C'
laSdnFlStr[10,3] = 5
laSdnFlStr[10,4] = 0

laSdnFlStr[11,1] = 'cdept'
laSdnFlStr[11,2] = 'C'
laSdnFlStr[11,3] = 6
laSdnFlStr[11,4] = 0


laSdnFlStr[12,1] = 'cCallOff'
laSdnFlStr[12,2] = 'C'
laSdnFlStr[12,3] = 6
laSdnFlStr[12,4] = 0

laSdnFlStr[13,1] = 'nLINENo'
laSdnFlStr[13,2] = 'N'
laSdnFlStr[13,3] = 5
laSdnFlStr[13,4] = 0


laSdnFlStr[14,1] = 'cMsStore'
laSdnFlStr[14,2] = 'C'
laSdnFlStr[14,3] = 6
laSdnFlStr[14,4] = 0

laSdnFlStr[15,1] = 'StoreName'
laSdnFlStr[15,2] = 'C'
laSdnFlStr[15,3] = 30
laSdnFlStr[15,4] = 0

laSdnFlStr[16,1] = 'Start'
laSdnFlStr[16,2] = 'N'
laSdnFlStr[16,3] = 7
laSdnFlStr[16,4] = 0

laSdnFlStr[17,1] = 'End'
laSdnFlStr[17,2] = 'N'
laSdnFlStr[17,3] = 7
laSdnFlStr[17,4] = 0

laSdnFlStr[18,1] = 'nSet'
laSdnFlStr[18,2] = 'N'
laSdnFlStr[18,3] = 6
laSdnFlStr[18,4] = 0

laSdnFlStr[19,1] = 'nSingls'
laSdnFlStr[19,2] = 'N'
laSdnFlStr[19,3] = 6
laSdnFlStr[19,4] = 0


=gfCrtTmp(lcSdnFile,@laSdnFlStr,'SDN_no+STR(nLINENo,5)',lcSdnFile,.F.)



*!*************************************************************
*! Name      : lfRePrnDes
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/27/2008
*! Purpose   : Print Despatach note
*!*************************************************************
FUNCTION lfRePrnDes

IF !USED('SDNHIST')
  gfOpenTable('SDNHIST','SDNHIST')
ENDIF 


lcSdnFile = loogscroll.gfTempName()
lfCrtTmpSdn()

IF !USED(lcETOFTmp)
  IF USED(lcURNfile)
    SELECT * FROM (lcURNfile) INTO CURSOR (lcETOFTmp) READWRITE
  ELSE
    RETURN   
  ENDIF
ENDIF 


SELECT (TmpMnfNo)
GO TOP
lcTempManfst = cTrkRef
lcTrkRef   = cTrkRef
SELECT ALRepURNHst
SCAN FOR cTrkRef=lcTempManfst
  SCATTER MEMVAR MEMO
  SELECT (lcETOFTmp)
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE npack   WITH m.npackno,;
          totpck  WITH m.npckqty,;
          cunique WITH m.curnseq,;
          qty     WITH ALLTRIM(cQty)
ENDSCAN


  SELECT ALURNHst
  =gfSetOrder("ALURNSTR")
  SELECT DISTINCT cCalloff ,cDept FROM cPrntmanfs INTO CURSOR curCallOff
  SELECT DISTINCT cDept FROM cPrntmanfs INTO CURSOR curDept
  SELECT curDept
  SCAN
    SELECT curCallOff
    SCAN FOR cDept=curDept.cDept
      lcCalloff = cCalloff
      STORE 0 TO lnTotPkCalOf,lnTotURN,lnLnDtl,lnDLDSeq
      IF !gfSeek(lcTempManfst+PADR(curDept.cDept,4)+lcCalloff,'SDNHIST')
        LOOP 
      ENDIF 
      m.SDN_no = SDNHIST.CSDNNO
      SELECT ccalloff,cmsstore,cdepot,cDept,MAX(npack),MIN(cunique) minurn,MAX(cunique) maxurn ,SUM(VAL(qty)) TotPcs FROM (lcETOFTmp) ;
        WHERE  ccalloff =lccalloff AND cDept=curDept.cDept GROUP BY ccalloff,cmsstore,cdepot,cDept ORDER BY ccalloff,minurn;
        INTO CURSOR CurURNDt
      LOCATE
      SUM TotPcs TO lnTotPkCalOf
      LOCATE
      SUM max_npack TO lnTotURN
       =gfseek(lcCalloff,'ALURNHst')
      IF EMPTY(lcTrkRef)
        lcTrkRef   = ALURNHst.cTrkRef
      ENDIF
      SELECT AlTrkMnf
      =gfseek(lcTrkRef,'AlTrkMnf')
      
      m.Ctrkref = lcTrkRef
      m.csealno = Altrkmnf.csealno
      LOCAL lnRemoteResult, lcSelectCommand
	  lcSelectCommand = [SELECT * FROM SYCCOMP WHERE CCOMP_ID = '] + oAriaApplication.ActiveCompanyID+ [']
      lnRemoteResult = loOGScroll.SQLExecute("SYCCOMP", lcSelectCommand,"","SYCCOMP","",;
	  oAriaApplication.SystemConnectionString,3,"")
	  IF lnRemoteResult >= 1 

        m.caddress1 =  gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
        m.caddress2 =  gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
        m.caddress3 =  gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
        m.caddress4 =  gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
        m.caddress5 =  gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
      ENDIF 
 		m.cBarcode = lfGetBarCode(cPrntManfs.cCallOff)
        m.cSeries  = cPrntManfs.Series
        m.cdept =  curDept.cDept
        m.cCallOff  =lcCallOff 
        

      
      SELECT cPrntmanfs
      LOCATE FOR ccalloff =lccalloff AND cDept=curDept.cDept


    lnSeq = 0
  
    SELECT curURNDt
    SCAN
      m.nLINENo = lnSeq+1
      m.cMSStore  = curURNDt.cMSStore 
      m.storename = gfCodDes(curURNDt.cMSStore , 'CMSSTORE')
      m.Start =VAL(curURNDt.minURN )
      m.End =VAL(curURNDt.MaxURN)
      m.nSet = curURNDt.max_npack
      m.nSingls =curURNDt.TotPcs
      lnSeq = lnSeq+1
      INSERT INTO (lcSdnFile) FROM MEMVAR 
    ENDSCAN   
  ENDSCAN
ENDSCAN

SELECT (lcSdnFile )
LOCATE 
IF !EOF()
  loOgScroll.lcOGLastForm  = 'ALSDNRP'
  loogScroll.cCROrientation = 'P'
  LoOGScroll.llCrystal = .T.
  USE IN (lcSdnFile )
  DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
  loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcSdnFile +'.DBF' 
  =gfDispRe()
ENDIF 
*! C201047,1 MMT 08/27/2008 print supplier despatch note and save SDN numbers[End]

*! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[Start]
*!*************************************************************
*! Name      : lfGetPkSz
*! Developer : Mariam Mazhar[MMT]
*! Date      : 02/16/2009
*! Purpose   : get pack Size from spck_lin
*!*************************************************************
FUNCTION lfGetPkSz
PARAMETERS lcPack_ID,lcStyle
lcAlias = SELECT()
IF !USED('Spck_lin')
  =gfOpenTable('Spck_lin','SPCKLNST')
ENDIF 
SELECT 'Spck_lin'
=gfSeek('P'+lcStyle)
LOCATE REST WHILE TYPE+STYLE+ACCOUNT+PACK_ID='P'+lcStyle FOR pack_id =lcPack_ID
IF FOUND()
  SELECT(lcAlias)
  RETURN Spck_lin.TotQty
ELSE
  SELECT(lcAlias)
  RETURN 0
ENDIF 
*! C201106,1 MMT 02/16/2009 Add Packs to M&S Pick ticket program[End]