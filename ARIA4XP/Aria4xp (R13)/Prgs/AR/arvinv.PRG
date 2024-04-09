*!**********************************************************************************************
*! Program file  : ARVINV.PRG
*! Program desc. : Voiding Multiple Invoices
*! Screen        : ARVINV.SCX
*! System        : Aria4
*! Module        : Accounts Receivable (AR)
*! Developer     : Ahmad Shoukry Mohammed (ASM)
*! Date          : 12/14/2004
*! Reference     : N038721
*!**********************************************************************************************
*! Parameters    : None
*!**********************************************************************************************
*N000682,1 11/20/2012 MMT Globlization changes[Start]
#include R:\ARIA4XP\PRGS\Ar\ARVINV.h
*N000682,1 11/20/2012 MMT Globlization changes[ENd]
PRIVATE lcAllVar
lcAllVar = 'laCodes,laPanelObj,laSetups,laVars,lcAcTrnSeq,lcAdTrnSeq,lcArGMA11,'+;
           'lcArGMA12,lcBrowStr,lcBrowTtl,lcCurrInv,lcFiles,lcGlDist,lcGlSess,'+;
           'GlSession,lcHisSeq,lcIDefDiv,lcIDefSes,lcIDefWare,lcInvExpr,lcInvFrom,'+;
           'lcInvHdr,lcInvLine,lcInvTo,lcMarker,lcRepBat,lcScrMode,lcScrSessn,'+;
           'lcSelInv,lcSysPeriod,lcSysYear,lcTempFile,lcTmpAR,lcWindTitle,ldDefInvDate,'+;
           'ldDefPstDate,ldOldDate,ldVDate,llActCrdt,llCallScop,llContinue,llEndVoid,'+;
           'llFromEDI,llFrstTime,llGMAPack,llIsEngland,llNoShow,llNoToAll,llReBuild,'+;
           'llRetHdr,llRetLine,llUpdGlDif,llUpdMstGL,llUpsInsur,llVoid,llYesToAll,'+;
           'lnBrRecNo,lnSelRec,lnSessNo,lnShipAddr,lnUnCmSeRc,lnUnSelRec,lnRngAlias'

lcAllVar = lcAllVar + ',Style,StyDye,InvHdr,InvLine,OrdHdr,OrdLine,Customer,'+;
           'Codes,ConSInvH,IcStyHst,ArCusHst,Debit,RepComm,ConsInvL,ArHist,Credit,'+;
           'Scale,SalesRep,GLDist,RetLine,RetHdr,SHDAEXPR,InvChrg,EDITrans'

DIMENSION laAllVar[1,1]
STORE '' TO laAllVar
=gfSubStr(lcAllVar,@laAllVar,',')

LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
  IF LOWER(LEFT(laAllVar[lnI,1],2)) <> 'la'
    PRIVATE &laAllVar[lnI,1].
  ENDIF
ENDFOR


*-- Preparing Screen Variables
PRIVATE lcInvExpr , lcScrSessn , lcTempFile , lcArGMA11 , lcArGMA12 , lcBrowTtl , lcFiles ,;
        lnSelRec , lnUnSelRec , llCallScop , llContinue , lcCurrInv , llVoid , llIsEngland ,;
        lcScrMode , llEndVoid , llYesToAll , llNoToAll , ldVDate , ldOldDate , lcInvFrom ,;
        lcInvTo , lcMarker , lcBrowStr , lcSelInv , lnBrRecNo

*-- All this variable is related to the Voiding part [Begin]
PRIVATE lcInvLine , lcInvHdr , ldDefInvDate , ldDefPstDate , lcInvLine , ;
        lcInvHdr , lcTmpAR , lcSysYear , lcSysPeriod , lcIDefSes , ;
        lcIDefDiv , lcIDefWare , GlSession , llUpsInsur , ;
        llReBuild , lnUnCmSeRc , lcAdTrnSeq , lcAcTrnSeq , ;
        lcHisSeq , lcRepBat , llUpdGlDif , llUpdMstGL , lnShipAddr , ;
        llActCrdt , lcGlDist , llRetLine , llRetHdr , llFromEDI
DECLARE laSetups[17,2] , laVars[20] , laCodes[1,10]

STORE ''  TO lcIDefSes, lcIDefDiv , lcIDefWare , lcAdTrnSeq , lcAcTrnSeq ,;
             lcHisSeq , lcGlSess  , lcRepBat   , laSetups   , laVars ,;
             lcInvHdr , lcInvLine , lcTmpAR    , lcGlDist , laCodes ,;
             lcInvFrom , lcInvTo , lcMarker , lcBrowStr , lcSelInv

STORE {} TO ldDefInvDate , ldDefPstDate , ldVDate , ldOldDate
STORE .F. TO llUpsInsur , llActCrdt , llFromEDI , llVoid , llIsEngland , llEndVoid , ;
             llRetLine , llRetHdr , llYesToAll , llNoToAll
STORE .T. TO llUpdGlDif , llUpdMstGL

*-- GMA is USA client but we have to keep this var. in case we convert this Prg to standard one.
IF UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='ENG'   && in case of England
  llIsEngland = .T.
ELSE
  llIsEngland = .F.
ENDIF
lnUnCmSeRc = 0

*C200352,1 HBG 19/06/2002 Flag to be used when Call ARDINV.prg to determine that This is for GMA[Begin]
llGMAPack = .T.
*C200352,1 [End]

*-- order cancelation reason codes
laCodes[1,1] = 'CCANCRESON'
laCodes[1,2] = 'laCanReason'
laCodes[1,3] = 'lnCanReason'
laCodes[1,4] = ''
laCodes[1,5] = .F.
laCodes[1,6] = .F.
laCodes[1,10] = 'CCANCRESON'

*-- variable used to update the uncomplete session file
laVars[1]  = 'ldDefInvDate'
laVars[2]  = 'lcIDefSes'
laVars[3]  = 'lcIDefDiv'
laVars[4]  = 'lcIDefWare'
laVars[5]  = 'llRebuild'
laVars[6]  = 'GlSession'
laVars[7]  = 'ldDefPstDate'
laVars[8]  = 'llUpsInsur'
laVars[9]  = 'lcScrMode'
laVars[10] = 'lcCurrInv'
laVars[11] = 'lnUnCmSeRc'    && Used by Key off program
laVars[12] = 'lcAdTrnSeq'    && Used by Key off program
laVars[13] = 'lcAcTrnSeq'    && Used by Key off program
laVars[14] = 'lcHisSeq'      && Used by Key off program
laVars[15] = 'lcGlSess'      && Used by Key off program
laVars[16] = 'lcRepBat'      && Used by Key off program
laVars[17] = 'llUpdGlDif'    && Used by Key off program
laVars[18] = 'llUpdMstGL'    && Used by Key off program
laVars[19] = 'lnShipAddr'
laVars[20] = 'llActCrdt'
*-- All this variable is related to the Voiding part [End]

STORE '' TO lcInvExpr , lcScrSessn , lcTempFile , lcArGMA11 , lcArGMA12 , lcBrowTtl , ;
            lcFiles , lcCurrInv
STORE 0 TO lnSelRec , lnUnSelRec
STORE .F. TO llCallScop , llContinue
lcScrMode = 'V'

lnBrRecNo = 1                   && Varible to hold the browse record number
lcBrowTtl = 'Invoices'          && Varible to hold the browse title



*-- Run the Screen
DO FORM (oAriaApplication.ScreenHome+"\AR\ARVINV.SCX")

RETURN
*-- End of Program


*!*************************************************************
*! Name      : lfFormInit
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/14/2004
*! Purpose   : Called from the Init Event of the FormSet
*!*************************************************************
*! Parameters: loFormSet
*!*************************************************************
*! Returns   : True/False
*!*************************************************************
FUNCTION lfFormInit
LPARAMETERS loFormSet
LOCAL lnCol, lcCap

loFormSet.lcCallProg='AR\ARVINV.FXP'
=lfAddPro(loFormSet)

*-- Define custom tool bar buttons
DECLARE loFormSet.lapanelobj[2,6]
STORE '' TO loFormSet.lapanelobj
*-- Scope Button
loFormSet.laPanelObj[1,1] = 'pbScop'
loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"SCOPE.BMP"
loFormSet.laPanelObj[1,3] = 'lfScope()'
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	loFormSet.laPanelObj[1,4] = "Option Grid"
*!*	loFormSet.laPanelObj[1,5] = "Option Grid"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[1,4] = LANG_ARVINV_OPTIONGRID
loFormSet.laPanelObj[1,4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_OPTIONGRID,loFormSet.GetHeaderText("LANG_ARVINV_OPTIONGRID",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[1,5] = LANG_ARVINV_OPTIONGRID
loFormSet.laPanelObj[1,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_OPTIONGRID,loFormSet.GetHeaderText("LANG_ARVINV_OPTIONGRID",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[End]
loFormSet.laPanelObj[1,6] = 'V'
*-- Voiding Button
loFormSet.laPanelObj[2,1] = 'pbRel'
loFormSet.laPanelObj[2,2] = oAriaApplication.BitMapHome+"RELEASE2.BMP"
loFormSet.laPanelObj[2,3] = 'lfvRelScr()'
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	loFormSet.laPanelObj[2,4] = "Void Invoices"
*!*	loFormSet.laPanelObj[2,5] = "Void Invoices"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[2,4] = LANG_ARVINV_VOIDINVOICES
loFormSet.laPanelObj[2,4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_VOIDINVOICES,loFormSet.GetHeaderText("LANG_ARVINV_VOIDINVOICES",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[2,5] = LANG_ARVINV_VOIDINVOICES
loFormSet.laPanelObj[2,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_VOIDINVOICES,loFormSet.GetHeaderText("LANG_ARVINV_VOIDINVOICES",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[End]
loFormSet.laPanelObj[2,6] = 'V'

*Old: lnSessNo = gnProgCopy


*Old: =gfOpenFile(gcDataDir+'Style',gcDataDir+'Style','SH')
*Old: =gfOpenFile(gcDataDir+'StyDye',gcDataDir+'StyDye','SH')
*Old: IF !gfSetup()
  *Old: RETURN
*Old: ENDIF

SET MULTILOCKS ON
IF !(lfopenfile(loFormSet,'Style','Style') AND lfopenfile(loFormSet,'StyDye','StyDye') AND ;
   lfopenfile(loFormSet,'InvHdr','InvHdr') AND lfopenfile(loFormSet,'InvLine','InvLine') AND ;
   lfopenfile(loFormSet,'OrdHdr','OrdHdr') AND lfopenfile(loFormSet,'OrdLine','OrdLine') AND ;
   lfopenfile(loFormSet,'Customer','Customer') AND lfopenfile(loFormSet,'Codes','cCode_No') AND ;
   lfopenfile(loFormSet,'ConsInvH','ConsInvH') AND lfopenfile(loFormSet,'ConsInvL','ConsInvL') AND ;
   lfopenfile(loFormSet,'SalesRep','SalesRep') )
  RETURN
ENDIF

loFormSet.llNoShow   = .F.       && Flag to make the screen call the PROCEDURE lfShow every time it runs
loFormSet.llFrstTime = .T.       && Flag to know if we are going to call lfShow for the first time

loFormSet.lcArGMA11  = gfTempName()
loFormSet.lcArGMA12  = gfTempName()
loFormSet.lcTempFile = gfTempName()
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*loFormSet.lcWindTitle = 'Voiding Multiple Invoices'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcWindTitle = LANG_ARVINV_VOIDMULTIPLEINVOICE
loFormSet.lcWindTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_VOIDMULTIPLEINVOICE,loFormSet.GetHeaderText("LANG_ARVINV_VOIDMULTIPLEINVOICE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[END]
lcCap = loFormSet.AriaForm1.Caption
loFormSet.AriaForm1.Caption = loFormSet.lcWindTitle + SUBSTR(lcCap,AT('/',lcCap)-1)

*-- Uncomplete session
*Old: loFormSet.llContinue = lfChkUnComS()
*Old: loFormSet.lcScrSessn = IIF(loFormSet.llContinue , loFormSet.lcScrSessn , gfSequence('CSESSION'))

*loFormSet.llCallScop = .T.
loFormSet.llCallScop = .F.

*--- First Show an empty Browser
=lpCreatTmp(loFormSet)           && Create the Temp File Based on InvHdr File


*ASM, Format the grid [Start]
SELECT (loFormSet.lcTempFile)
WITH loFormSet.AriaForm1.grdInvoices
  .ColumnCount = 0
  .RecordSourceType = 1
  .ColumnCount = 9
  .RecordSource = loFormSet.lcTempFile

  *lfAddGridCol Parameters: Grid, ColNo, ControlSource, Caption, Header Aligment, Width, InputMask
  lnCol=0
  lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.lSelInv'," ",0,16)
  .column1.addobject('chkStatus','CheckBox')
  STORE .T. to .column1.chkStatus.Visible, .column1.chkStatus.Enabled
  .column1.CurrentControl = 'chkStatus'
  .column1.Sparse = .F.
  .column1.chkStatus.Caption = ''
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	  lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Invoice',"Invoice",0,75)
*!*	  lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.InvDate',"Date",0,75)
*!*	  lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Account',"Account",0,75)
*!*	  lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Store',"Store",0,75)
*!*	  lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,[LOOKUP(Customer.BTName, IIF(EMPTY(Store) , 'M'+ Account ,'S'+ Account+ Store),Customer.Type,'CUSTOMER')],;
*!*	     "Bill To",0,200)
*!*	  lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.CustPO',"Reference",0,150)
*!*	  lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Ship',"Pieces",1,100)
*!*	  lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.ShipAmt',"Merchandise",1,100)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Invoice',LANG_ARVINV_INVOICE,0,75)
lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Invoice',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_INVOICE,loFormSet.GetHeaderText("LANG_ARVINV_INVOICE",loFormSet.HeaderAlias)),0,75)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.InvDate',LANG_ARVINV_DATE,0,75)
lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.InvDate',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_DATE,loFormSet.GetHeaderText("LANG_ARVINV_DATE",loFormSet.HeaderAlias)),0,75)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Account',LANG_ARVINV_ACCOUNT,0,75)
lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Account',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_ACCOUNT,loFormSet.GetHeaderText("LANG_ARVINV_ACCOUNT",loFormSet.HeaderAlias)),0,75)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Store',LANG_ARVINV_STORE,0,75)
lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Store',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_STORE,loFormSet.GetHeaderText("LANG_ARVINV_STORE",loFormSet.HeaderAlias)),0,75)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,[LOOKUP(Customer.BTName, IIF(EMPTY(Store) , 'M'+ Account ,'S'+ Account+ Store),Customer.Type,'CUSTOMER')],;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_BILLTO,loFormSet.GetHeaderText("LANG_ARVINV_BILLTO",loFormSet.HeaderAlias)),0,200)

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.CustPO',LANG_ARVINV_REFERENCE,0,150)
lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.CustPO',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_REFERENCE,loFormSet.GetHeaderText("LANG_ARVINV_REFERENCE",loFormSet.HeaderAlias)),0,150)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Ship',LANG_ARVINV_PIECES,1,100)
lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.Ship',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_PIECES,loFormSet.GetHeaderText("LANG_ARVINV_PIECES",loFormSet.HeaderAlias)),1,100)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.ShipAmt',LANG_ARVINV_MERCHANDISE,1,100)
lfAddGridCol(loFormSet.AriaForm1.grdInvoices,@lnCol,ALIAS()+'.ShipAmt',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_MERCHANDISE,loFormSet.GetHeaderText("LANG_ARVINV_MERCHANDISE",loFormSet.HeaderAlias)),1,100)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
  .SetAll('ReadOnly',.T.,'Column')
  .column1.ReadOnly = .F.
  BINDEVENT(.column1.chkStatus,'Click',loFormSet,'chkboxclick')
ENDWITH
*ASM, Format the grid [End]

loFormSet.ChangeMode("V")
loFormSet.AriaForm1.grdInvoices.column1.CurrentControl = 'chkStatus'
RETURN
*--end of lfFormInit


*!*************************************************************
*! Name      : lfAddPro
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/13/2004
*! Purpose   : function to Add properties to the FormSet.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfAddPro
LPARAMETERS loFormSet


LOCAL lnI, lnRow, lnCol, lcACopy
FOR lnI = 1 TO ALEN(laAllVar,1)
  *WAIT WINDOW LOWER(LEFT(laAllVar[lnI,1],2)) TIMEOUT 0.1
  IF LOWER(LEFT(laAllVar[lnI,1],2)) = 'la'
    IF TYPE(laAllVar[lnI,1])='U'
      loFormSet.AddProperty(laAllVar[lnI,1]+'[1]')
      LOOP
    ENDIF
    lnRow = ALEN(laAllVar[lnI,1],1)
    lnCol = ALEN(laAllVar[lnI,1],2)
    IF lnCol>0
      loFormSet.AddProperty(laAllVar[lnI,1]+'['+ALLTRIM(STR(lnRow))+;
                                            ','+ALLTRIM(STR(lnCol))+']')
    ELSE
      loFormSet.AddProperty(laAllVar[lnI,1]+'['+ALLTRIM(STR(lnRow))+']')
    ENDIF
    lcACopy = '=ACOPY(' + laAllVar[lnI,1] + ',loFormSet.' + laAllVar[lnI,1] + ')'
    &lcACopy.
  ELSE
    loFormSet.AddProperty(laAllVar[lnI,1],IIF(TYPE(laAllVar[lnI,1])<>'U',EVALUATE(laAllVar[lnI,1]),.F.))
  ENDIF
ENDFOR

RETURN
*--end of lfAddPro


*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/14/2004
*! Purpose   : To Open files and store name of files opened
*!*************************************************************
*! Parameters: loFormSet, lcFile, lcTag
*!*************************************************************
*! Returns   : True / False
*!*************************************************************
FUNCTION lfOpenFile
PARAMETERS loFormSet, lcFile, lcTag
LOCAL lcProp, llRetVal

lcProp = lcFile
IF TYPE('loFormSet.'+lcProp)<>'O'
  loFormSet.&lcProp = CREATEOBJECT("RemoteTable",lcFile,lcTag,lcFile,loFormSet.DataSessionID)
ENDIF
llRetVal = TYPE('loFormSet.'+lcProp)='O'

RETURN llRetVal
*-- End of lfOpenFile


*!***************************************************************************
*! Name      : lfAddGridCol
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/19/2001
*! Purpose   : Function to Add a Column to the Grid.
*!***************************************************************************
*! Parameters: Grd, lnCol, lcControlSource, lcCaption, lnHdrAligment, lnWidth, lcInputMask
*!***************************************************************************
FUNCTION lfAddGridCol
LPARAMETERS grd, lnCol, lcControlSource, lcCaption, lnHdrAligment, lnWidth, lcInputMask
LOCAL lcCol

lnCol = lnCol +1
lcCol = ALLTRIM(STR(lnCol))
WITH grd
  .column&lcCol..ControlSource  = lcControlSource
  .column&lcCol..Header1.Caption = lcCaption
  .column&lcCol..Header1.Alignment = IIF(TYPE('lnHdrAligment')='N',lnHdrAligment,0)
  .column&lcCol..Width = IIF(TYPE('lnWidth')='N',lnWidth,75)
  IF TYPE('lcInputMask')='C'
    .column&lcCol..InputMask = lcInputMask
  ENDIF
ENDWITH

RETURN
*-- End of lfAddGridCol


*!*************************************************************
*! Name      : lfFormDestroy
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/14/2004
*! Purpose   : Called from the Destroy Event of the FormSet
*!*************************************************************
*! Parameters: loFormSet
*!*************************************************************
*! Returns   : True/False
*!*************************************************************
FUNCTION lfFormDestroy
LPARAMETERS loFormSet

*Old: IF glQuitting
  IF USED(loFormSet.lcTempFile)
    USE IN (loFormSet.lcTempFile)
  ENDIF
  ERASE (oAriaApplication.WorkDir+loFormSet.lcTempFile+'.DBF')
  ERASE (oAriaApplication.WorkDir+loFormSet.lcTempFile+'.CDX')
  ERASE (oAriaApplication.WorkDir+loFormSet.lcTempFile+'.FPT')
*Old: ENDIF

*Old: RELEASE WINDOW (loFormSet.lcBrowTtl)

*Old: POP KEY

RETURN
*--end of lfFormDestroy


*!***************************************************************************
*! Name      : lfShow
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/19/2004
*! Purpose   : Called from the Refresh event of the FormSet
*!***************************************************************************
*! Parameters: loFormSet
*!*************************************************************
*! Returns   : None
*!*************************************************************
PROCEDURE lfShow
LPARAMETERS loFormSet

*Old: SHOW GET pbEdt     DISABLE
*Old: SHOW GET pbBrws    DISABLE
*Old: SHOW GET pbcpPrint DISABLE
*Old: SHOW GET pbSlct    DISABLE
*Old: SHOW GET pbDlt     DISABLE

WITH oariaapplication.otoolbar
  STORE .f. to .cmdEdit.Enabled, .cmdFind.Enabled, .cmdPrint.Enabled, .cmdSelect.Enabled, ;
               .cmdDelete.Enabled
ENDWITH

*Old: laCtrStat[7]  = "DISABLE"                && Edit button
*Old: laCtrStat[8]  = "DISABLE"                && Delete button
*Old: laCtrStat[9]  = "DISABLE"                && Select button
*Old: laCtrStat[10] = "DISABLE"                && Browse button

*-- IF Statment to check if we are going to call the option grid for the user
*-- IF these is the first time for these session of the program
IF loFormSet.llCallScop
  *Old: SHOW GET pbRel DISABLE &&Revise
  IF loFormSet.llFrstTime
    *loFormSet.AriaForm1.Activate()
  ELSE
    oAriaApplication.oToolBar.ChangeButtonStatus('pbRel','DISABLED')
    =lfvScope(loFormSet)
    loFormSet.llCallScop = .F.
  ENDIF
ENDIF

loFormSet.llFrstTime = .F.

IF USED(loFormSet.lcTempFile)
  IF EOF(loFormSet.lcTempFile)
    STORE .F. to loFormSet.AriaForm1.cmdSelect.Enabled, ;
                 loFormSet.AriaForm1.cmdSelAll.Enabled, ;
                 loFormSet.AriaForm1.cmdSelNon.Enabled, ;
                 loFormSet.AriaForm1.cmdInvert.Enabled
    oAriaApplication.oToolBar.ChangeButtonStatus('pbRel','DISABLED')
  ELSE
    STORE .T. to loFormSet.AriaForm1.cmdSelect.Enabled, ;
                 loFormSet.AriaForm1.cmdSelAll.Enabled, ;
                 loFormSet.AriaForm1.cmdInvert.Enabled
    loFormSet.AriaForm1.cmdSelNon.Enabled = .F.
    *oAriaApplication.oToolBar.ChangeButtonStatus('pbRel','ENABLED')
  ENDIF
ENDIF

loFormSet.lnBrRecNo  = RECNO(loFormSet.lcTempFile)
*Old: IF TYPE('loFormSet.lcInvExpr') <> 'L'
  *Old: SHOW WINDOW (loFormSet.lcBrowTtl) REFRESH
*Old: ENDIF
loFormSet.AriaForm1.grdInvoices.Refresh()
*-- End of lfShow


*!***************************************************************************
*! Name      : lfvScope
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/19/2004
*! Purpose   : Valid function of push button Scope.
*!***************************************************************************
*! Parameters: loFormSet
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
FUNCTION lfvScope
LPARAMETERS loFormSet

*-- Get the current cancellation Type
*Old: PUSH KEY CLEAR
loFormSet.lcInvExpr  = gfOpGrid('ARVINV', .T.)

loFormSet.lcInvExpr = loFormSet.lcInvExpr + IIF(EMPTY(loFormSet.lcAccNo),'',' AND InvHdr.Account = "'+loFormSet.lcAccNo+'"')
STORE '' TO loFormSet.lcAccNo

*Old: POP KEY

=lpCreatTmp(loFormSet)        && Re-Create the Temp File
=lpFillField(loFormSet)       && AnyWay Start filling the Tmp File

IF EOF(loFormSet.lcTempFile)
  STORE .F. to loFormSet.AriaForm1.cmdSelect.Enabled, ;
               loFormSet.AriaForm1.cmdSelAll.Enabled, ;
               loFormSet.AriaForm1.cmdSelNon.Enabled, ;
               loFormSet.AriaForm1.cmdInvert.Enabled
ELSE
  STORE .T. to loFormSet.AriaForm1.cmdSelect.Enabled, ;
               loFormSet.AriaForm1.cmdSelAll.Enabled, ;
               loFormSet.AriaForm1.cmdSelNon.Enabled, ;
               loFormSet.AriaForm1.cmdInvert.Enabled
ENDIF

loFormSet.lnBrRecNo = RECNO()
*Old: = lfDispBrow()    && Browse What U Got
loFormSet.Refresh()

loFormSet.llCallScop = .F.             && Screen Already Initialized

*-- IF The temp file [lcTempFile] is not empty
IF !EOF()
  *loFormSet.ChangeMode("V")
  *loFormSet.AriaForm1.grdInvoices.column1.CurrentControl = 'chkStatus'
  loFormSet.AriaForm1.cmdSelect.Enabled = .T.
  loFormSet.AriaForm1.cmdSelAll.Enabled = .T.
  loFormSet.AriaForm1.cmdInvert.Enabled = .T.
  loFormSet.AriaForm1.cmdSelNon.Enabled = .F.
  GO TOP IN (loFormSet.lcTempFile)
ELSE
  *loFormSet.ChangeMode("S")
  *loFormSet.AriaForm1.grdInvoices.column1.CurrentControl = 'chkStatus'
  STORE .F. TO loFormSet.AriaForm1.cmdSelect.Enabled, ;
               loFormSet.AriaForm1.cmdSelAll.Enabled, ;
               loFormSet.AriaForm1.cmdInvert.Enabled, ;
               loFormSet.AriaForm1.cmdSelNon.Enabled
ENDIF

oAriaApplication.oToolBar.ChangeButtonStatus('pbRel','DISABLED')

RETURN
*-- End of lfvScope


*!***************************************************************************
*! Name      : lpCreatTmp
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/19/2004
*! Purpose   : Function To Create the temp file used in the Grid.
*!***************************************************************************
*! Parameters: loFormSet
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
PROCEDURE lpCreatTmp
LPARAMETERS loFormSet

IF EMPTY(loFormSet.AriaForm1.grdInvoices.RecordSource)
  CREATE CURSOR TempCursor ;
               (Invoice C(6), InvDate D(8) , Account C(5), Store C(8) , ;
                Order   C(6), CustPO  C(15), Rep1    C(3), Ship N(7,0), ;
                ShipAmt N(14,2), lSelInv L(1))
  SELECT TempCursor
  =AFIELDS(laStru)
  use
  =gfCrtTmp(loFormSet.lcTempFile,@laStru,'Invoice',loFormSet.lcTempFile)
ENDIF
SELECT (loFormSet.lcTempFile)
ZAP
loFormSet.nworkarea = ALIAS()
loFormSet.DataEnvironment.InitialSelectedAlias = ALIAS()


RETURN
*-- End of lpCreatTmp


*!***************************************************************************
*! Name      : lpFillField
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/19/2004
*! Purpose   : Function To Fill The Temp Table according to user selection
*!***************************************************************************
*! Parameters: loFormSet
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
PROCEDURE lpFillField
LPARAMETERS loFormSet
LOCAL lcM

SELECT InvHdr
LOCATE

IF TYPE('loFormSet.lcInvExpr') = "C"
  loFormSet.lcInvExpr = loFormSet.lcInvExpr + IIF(EMPTY(loFormSet.lcInvExpr),'',[ AND ]) + [Status <> "V"]
  lcM = loFormSet.lcInvExpr
  SCAN FOR &lcM
    SCATTER MEMVAR MEMO
    m.lSelInv = .F.
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *WAIT WINDOW 'Collecting Invoice# '+ m.Invoice NOWAIT
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_ARVINV_COLLECTINVOICE+ m.Invoice NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_COLLECTINVOICE,loFormSet.GetHeaderText("LANG_ARVINV_COLLECTINVOICE",loFormSet.HeaderAlias))+ m.Invoice NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[End]
    INSERT INTO (loFormSet.lcTempFile) FROM MEMVAR
  ENDSCAN

  SELECT ConsInvH
  LOCATE
  loFormSet.lcInvExpr = STRTRAN(loFormSet.lcInvExpr,'INVHDR.','CONSINVH.')
  lcM = loFormSet.lcInvExpr
  SCAN FOR &lcM
    IF !SEEK(ConsInvH.Invoice,(loFormSet.lcTempFile))
      SCATTER MEMVAR MEMO
      m.lSelInv = .F.
      *N000682,1 MMT 11/22/2012 Globalization changes[Start]
      *WAIT WINDOW 'Collecting Invoice# '+ m.Invoice NOWAIT
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_ARVINV_COLLECTINVOICE + m.Invoice NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_COLLECTINVOICE,loFormSet.GetHeaderText("LANG_ARVINV_COLLECTINVOICE",loFormSet.HeaderAlias)) + m.Invoice NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 MMT 11/22/2012 Globalization changes[End]
      INSERT INTO (loFormSet.lcTempFile) FROM MEMVAR
    ENDIF
  ENDSCAN

  SELECT (loFormSet.lcTempFile)
  IF EOF()
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *WAIT WINDOW 'No Records Selected.' NOWAIT
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_ARVINV_NORECORDSELECTED NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_NORECORDSELECTED,loFormSet.GetHeaderText("LANG_ARVINV_NORECORDSELECTED",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[End]
  ENDIF
ELSE
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *WAIT WINDOW 'No Records Selected.' NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_ARVINV_NORECORDSELECTED NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_NORECORDSELECTED,loFormSet.GetHeaderText("LANG_ARVINV_NORECORDSELECTED",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
ENDIF
WAIT CLEAR

RETURN
*-- End of lpFillField


*!***************************************************************************
*! Name      : lfvSelect
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/19/2004
*! Purpose   : Called from the Click event of the Select command button
*!***************************************************************************
*! Parameters: loFormSet
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
FUNCTION lfvSelect
LPARAMETERS loFormSet

REPLACE lSelInv WITH !lSelInv
=lfvPbSel(loFormSet)
loFormSet.lnSelRec   = IIF(lSelInv , loFormSet.lnSelRec + 1 , loFormSet.lnSelRec - 1)
loFormSet.lnUnSelRec = IIF(lSelInv , loFormSet.lnUnSelRec - 1 , loFormSet.lnUnSelRec + 1)
*SHOW WINDOW (loFormSet.lcBrowTtl) REFRESH
loFormSet.AriaForm1.grdInvoices.Refresh()

*-- IF No records was selected
IF loFormSet.lnSelRec = 0
  loFormSet.AriaForm1.cmdSelNon.Enabled = .F.
  *Old: SHOW GET pbRel    DISABLE &&Revise
  oAriaApplication.oToolBar.ChangeButtonStatus('pbRel','DISABLED')
  loFormSet.AriaForm1.cmdSelAll.Enabled = .T.
ELSE
  loFormSet.AriaForm1.cmdSelNon.Enabled = .T.
  *Old: SHOW GET pbRel    ENABLE &&Revise
  oAriaApplication.oToolBar.ChangeButtonStatus('pbRel','ENABLED')

  *-- IF All the records was selected
  IF loFormSet.lnUnSelRec = 0
    loFormSet.AriaForm1.cmdSelAll.Enabled = .F.
  ELSE
    loFormSet.AriaForm1.cmdSelAll.Enabled = .T.
  ENDIF
ENDIF

RETURN
*-- End of lfvSelect


*!***************************************************************************
*! Name      : lfvSelAll
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/19/2004
*! Purpose   : Called from the Click event of the Select All command button
*!***************************************************************************
*! Parameters: loFormSet
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
FUNCTION lfvSelAll
LPARAMETERS loFormSet

IF !EOF()
  REPLACE ALL lSelInv WITH .T.
  loFormSet.lnSelRec   = RECCOUNT()
  loFormSet.lnUnSelRec = 0
  GO loFormSet.lnBrRecNo
  *Old: SHOW GET loFormSet.AriaForm1.cmdSelect,1 PROMPT 'UnSe\<lect'  &&lower &&Revise
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *  loFormSet.AriaForm1.cmdSelect.Caption = 'UnSe\<lect'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdSelect.Caption = LANG_ARVINV_UNSELECT
loFormSet.AriaForm1.cmdSelect.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_UNSELECT,loFormSet.GetHeaderText("LANG_ARVINV_UNSELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/20/2012 Globalization Changes[End]

  *Old: SHOW WINDOW (loFormSet.lcBrowTtl) REFRESH
  loFormSet.AriaForm1.grdInvoices.Refresh()
  *Old: SHOW GET pbRel    ENABLE &&Revise
  oAriaApplication.oToolBar.ChangeButtonStatus('pbRel','ENABLED')
  *Old: SHOW GET loFormSet.AriaForm1.cmdSelNon ENABLE  &&lower &&Revise
  *Old: SHOW GET loFormSet.AriaForm1.cmdSelAll DISABLE  &&lower &&Revise
  loFormSet.AriaForm1.cmdSelNon.Enabled = .T.
  loFormSet.AriaForm1.cmdSelAll.Enabled = .F.
ENDIF

RETURN
*-- End of lfvSelAll


*!***************************************************************************
*! Name      : lfvSelNon
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 19/12/2004
*! Purpose   : Called from the Click event of the Select None command button
*!***************************************************************************
*! Parameters: loFormSet
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
FUNCTION lfvSelNon
LPARAMETERS loFormSet

IF !EOF()
  REPLACE ALL lSelInv WITH .F.
  loFormSet.lnSelRec   = 0
  loFormSet.lnUnSelRec = RECCOUNT()
  GO loFormSet.lnBrRecNo
  *Old: SHOW GET loFormSet.AriaForm1.cmdSelect,1 PROMPT 'Se\<lect'  &&lower &&Revise
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *  loFormSet.AriaForm1.cmdSelect.Caption = 'Se\<lect'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdSelect.Caption =LANG_ARVINV_SELECT
loFormSet.AriaForm1.cmdSelect.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_SELECT,loFormSet.GetHeaderText("LANG_ARVINV_SELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/20/2012 Globalization Changes[End]

  *SHOW WINDOW (loFormSet.lcBrowTtl) REFRESH
  loFormSet.AriaForm1.grdInvoices.Refresh()
  *Old: SHOW GET pbRel DISABLE &&Revise
  oAriaApplication.oToolBar.ChangeButtonStatus('pbRel','DISABLED')
  *Old: SHOW GET loFormSet.AriaForm1.cmdSelNon DISABLE  &&lower &&Revise
  *Old: SHOW GET loFormSet.AriaForm1.cmdSelAll ENABLE  &&lower &&Revise
  loFormSet.AriaForm1.cmdSelNon.Enabled = .F.
  loFormSet.AriaForm1.cmdSelAll.Enabled = .T.
ENDIF

RETURN
*-- End of lfvSelNon.


*!***************************************************************************
*! Name      : lfvInvert
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 19/12/2004
*! Purpose   : Called from the Click event of the Invert command button
*!***************************************************************************
*! Parameters: loFormSet
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
FUNCTION lfvInvert
LPARAMETERS loFormSet

REPLACE ALL lSelInv WITH !lSelInv
GO loFormSet.lnBrRecNo
=lfvpbSel(loFormSet)
loFormSet.lnUnSelRec = loFormSet.lnSelRec
loFormSet.lnSelRec   = RECCOUNT() - loFormSet.lnSelRec
*Old: SHOW WINDOW (loFormSet.lcBrowTtl) REFRESH
loFormSet.AriaForm1.grdInvoices.Refresh()

*-- IF there is no selected records
IF loFormSet.lnSelRec = 0
  *Old: SHOW GET pbRel    DISABLE &&Revise
  oAriaApplication.oToolBar.ChangeButtonStatus('pbRel','DISABLED')
  *Old: SHOW GET loFormSet.AriaForm1.cmdSelNon DISABLE  &&lower &&Revise
  *Old: SHOW GET loFormSet.AriaForm1.cmdSelAll ENABLE  &&lower &&Revise
  loFormSet.AriaForm1.cmdSelNon.Enabled = .F.
  loFormSet.AriaForm1.cmdSelAll.Enabled = .T.
ELSE
  *Old: SHOW GET pbRel    ENABLE &&Revise
  oAriaApplication.oToolBar.ChangeButtonStatus('pbRel','ENABLED')
  *Old: SHOW GET loFormSet.AriaForm1.cmdSelNon ENABLE  &&lower &&Revise
  loFormSet.AriaForm1.cmdSelNon.Enabled = .T.

  *-- IF All the records was selected
  IF loFormSet.lnUnSelRec = 0
    *Old: SHOW GET loFormSet.AriaForm1.cmdSelAll DISABLE  &&lower &&Revise
    loFormSet.AriaForm1.cmdSelAll.Enabled = .F.
  ENDIF
ENDIF

RETURN
*--End of lfvInvert.


*!***************************************************************************
*! Name      : lfvpbSel
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 19/12/2004
*! Purpose   : Function for Switching 'Select And Unselect'
*!***************************************************************************
*! Parameters: loFormSet
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
FUNCTION lfvpbSel
LPARAMETERS loFormSet

*-- IF The record is selected
IF lSelInv
  *Old: SHOW GET loFormSet.AriaForm1.cmdSelect,1 PROMPT 'UnSe\<lect'  &&lower &&Revise
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *  loFormSet.AriaForm1.cmdSelect.Caption = 'UnSe\<lect'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdSelect.Caption =LANG_ARVINV_UNSELECT
loFormSet.AriaForm1.cmdSelect.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_UNSELECT,loFormSet.GetHeaderText("LANG_ARVINV_UNSELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/20/2012 Globalization Changes[End]

ELSE
  *Old: SHOW GET loFormSet.AriaForm1.cmdSelect,1 PROMPT 'Se\<lect'  &&lower &&Revise
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *  loFormSet.AriaForm1.cmdSelect.Caption = 'Se\<lect'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdSelect.Caption =LANG_ARVINV_SELECT
loFormSet.AriaForm1.cmdSelect.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_SELECT,loFormSet.GetHeaderText("LANG_ARVINV_SELECT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF

RETURN .T.
*-- End of lfvpbSel.


*!**************************************************************************
*! Name      : lfvAccount
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 19/12/2004
*! Purpose   : Function for Switching 'Select And Unselect'
*!***************************************************************************
*! Parameters: loFormSet
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
FUNCTION lfvAccount
PRIVATE lcObjVal

IF !(lcRpAcc == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER
  *-- IF The user want to Browse or if the Account he/she entered is not in the file
  IF '?' $ lcRpAcc .OR. (!EMPTY(lcRpAcc) .AND. !SEEK('M' + lcRpAcc , 'CUSTOMER'))
    llObjRet = CusBrowM(@lcRpAcc , '' , 'M')
    lcRpAcc = IIF(llObjRet , lcRpAcc, lcOldVal)
    oariaapplication.otoolbar.owindparent.lcAccNo = lcRpAcc
    lfOGShowGet('lcRpAcc')
  ENDIF
  oariaapplication.otoolbar.owindparent.lcAccNo = lcRpAcc
  IF !(lcRpAcc == lcOldVal)
    llClearInv = .T.  && Clear previous Invoice Range
    lcOldVal = lcRpAcc
  ENDIF
  SELECT CUSTOMER
  SET ORDER TO &lcCustOrd
  SELECT(lnAlsNo)
ENDIF

RETURN
*-- End of lfvAccount

*!***************************************************************************
*! Name      : lfSRInv
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/23/2004
*! Purpose   : control browse Invoices for InvHdr File. Called from the Option Grid
*!***************************************************************************
*! Parameters: lcParm
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
FUNCTION lfSRInv
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    oariaapplication.otoolbar.owindparent.lnRngAlias = SELECT(0)
    SELECT InvHdr
    *ASM, We will not set the default order to the Customer+Invoice [Start]
    *SET ORDER TO TAG InvHdrA
    SET ORDER TO TAG InvHdr
    LOCATE
    *ASM, We will not set the default order to the Customer+Invoice [End]
  CASE lcParm = 'R'  && Reset code
    llClearInv = .F.
    SELECT InvHdr
    SET ORDER TO TAG InvHdr
    SELECT (oariaapplication.otoolbar.owindparent.lnRngAlias)
ENDCASE

RETURN
*-- End of lfSRInv.

*!***************************************************************************
*! Name      : lfSROrder
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/23/2004
*! Purpose   : control browse Orders for OrdHdr File .Called from the Option Grid
*!***************************************************************************
*! Parameters: lcParm
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm
LOCAL lcCustRel

DO CASE
  CASE lcParm = 'S'
    oariaapplication.otoolbar.owindparent.lnRngAlias = SELECT(0)
    SELECT OrdHdr
    lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET RELATION TO &lcCustRel INTO CUSTOMER       && To customer file.
    LOCATE
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER                 && To customer file.
    SELECT (oariaapplication.otoolbar.owindparent.lnRngAlias)
ENDCASE

RETURN
*-- End of lfSROrder


*!***************************************************************************
*! Name      : lfRelScr
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 12/23/2004
*! Purpose   : Voiding the Selected Lines
*!***************************************************************************
*! Parameters: loFormSet
*!***************************************************************************
*! Returns   : None
*!***************************************************************************
FUNCTION lfRelScr
PARAMETERS loFormSet
PRIVATE ldVoidDate, llYesToAll, llNoToAll, lcCanReason, lcInvTag, lcInvFrom, lcInvTo, ;
   lcInvoice, llRetValue
LOCAL ARRAY laSetups[1,1]

ldVDate = {}
*-- get the void date from the user
DO FORM (oAriaApplication.ScreenHome+"AR\ARVDATE") TO ldVDate
IF EMPTY(ldVDate)
  RETURN .F.
ENDIF
*-- check if the void data is valid or not
STORE '' TO lcSysYear,lcSysPeriod
IF !CHECKPRD(ldVDate,'lcSysYear','lcSysPeriod','VI1')
  RETURN .F.
ENDIF
ldVoidDate = ldVDate
STORE .F. TO llYesToAll, llNoToAll
lcCanReason = ''

loFormSet.laSetups[4,1]  = 'M_LINK_GL'      &&  Check for Gl link
loFormSet.laSetups[8,1]  = 'M_DYELOT'       &&  Use Dylot Y Or N
loFormSet.laSetups[9,1]  = 'M_TAX'          &&  use Taxes Y or N
loFormSet.laSetups[17,1] = 'XPOSTFINV'      &&  Post Factored invoice to customer
=ACOPY(loFormSet.laSetups,laSetups)
=gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)
=ACOPY(laSetups,loFormSet.laSetups)

SELECT InvHdr
lcInvTag = TAG()
SET ORDER TO TAG InvHdr
STORE '' TO lcInvFrom, lcInvTo
loFormSet.GlSession = gfsequence('GLSESSION')
*C200352,1 HBG 19/06/2002 Open 'SHDAEXPR' File to be updated when void the invoices [Begin]
IF FILE(oAriaApplication.DataDir+'SHDAEXPR.DBF')
  =gfOpenFile(oAriaApplication.DataDir+'SHDAEXPR',oAriaApplication.DataDir+'SHDAEXPR','SH')
ENDIF
*C200352,1 [End]

loFormSet.lcTmpAR   = gfTempName()
SELECT (loFormSet.lcTempFile)
GO TOP
SCAN FOR lSelInv
  =SEEK(Invoice,'InvHdr')
  SELECT InvHdr
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *WAIT WINDOW 'Voiding Invoice # ' + InvHdr.Invoice NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_ARVINV_VOIDINVOICE + InvHdr.Invoice NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_VOIDINVOICE,loFormSet.GetHeaderText("LANG_ARVINV_VOIDINVOICE",loFormSet.HeaderAlias)) + InvHdr.Invoice NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
  lcInvoice = ''
  llRetValue = .T.
  DO gpVoidInv IN (oAriaApplication.ApplicationHome+'AR\ARINV.PRG') WITH ;
    lasetups, loFormSet.glsession, loFormSet.llIsEngland, loFormSet.lctmpar, loFormSet, ;
    .T., ldVoidDate, llYesToAll, llNoToAll, lcCanReason
  SELECT (loFormSet.lcTempFile)
  lcInvFrom = IIF(EMPTY(lcInvFrom) and llRetValue,Invoice,lcInvFrom)
  lcInvTo = IIF(llRetValue,Invoice,lcInvTo)
  IF llRetValue
    IF USED('SHDAEXPR') AND SEEK(Invoice,'SHDAEXPR')
      SELECT SHDAEXPR
      REPLACE cSiisVoid WITH 'Y'
      SELECT (loFormSet.lcTempFile)
    ENDIF
  ENDIF

  loFormSet.Style.Tableupdate()
  loFormSet.StyDye.Tableupdate()
  loFormSet.InvHdr.Tableupdate()
  loFormSet.InvLine.Tableupdate()
  loFormSet.OrdHdr.Tableupdate()
  loFormSet.OrdLine.Tableupdate()
  loFormSet.Customer.Tableupdate()
  loFormSet.Codes.Tableupdate()
  loFormSet.ConsInvH.Tableupdate()
  loFormSet.ConsInvL.Tableupdate()

  loFormSet.SaveFiles(.T.)

  SELECT (loFormSet.lcTempFile)
ENDSCAN
WAIT CLEAR

ZAP
=lfShow(loFormSet)

IF EMPTY(lcInvFrom) AND EMPTY(lcInvTo)
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *=gfModalGen('TRM00000B00000','ALERT','','','No Invoice Voided.')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B00000','ALERT','','',LANG_ARVINV_NOINVOICEVOIDED)
=gfModalGen('TRM00000B00000','ALERT','','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_NOINVOICEVOIDED,loFormSet.GetHeaderText("LANG_ARVINV_NOINVOICEVOIDED",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
  *-- <No Invoice Voided.>
  *-- Button : 00000
  *-- <        Ok        >
ENDIF
IF !EMPTY(lcInvFrom) AND !EMPTY(lcInvTo)
  IF lcInvFrom == lcInvTo
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *=gfModalGen('TRM00000B00000','ALERT','','','Invoice# ' + lcInvFrom + ' has been Voided.')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B00000','ALERT','','',LANG_ARVINV_INVOICENUMBER + lcInvFrom +LANG_ARVINV_HASBEENVOIDED)
=gfModalGen('TRM00000B00000','ALERT','','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_INVOICENUMBER,loFormSet.GetHeaderText("LANG_ARVINV_INVOICENUMBER",loFormSet.HeaderAlias)) + lcInvFrom +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_HASBEENVOIDED,loFormSet.GetHeaderText("LANG_ARVINV_HASBEENVOIDED",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[End]
    *-- <Invoice# lcInvFrom has been Voided.>
    *-- Button : 00000
    *-- <                         Ok                         >
  ELSE
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *=gfModalGen('TRM00000B00000','ALERT','','','Invoices from ' + lcInvFrom + ' to ' + lcInvTo + ' have been Voided.')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B00000','ALERT','','',LANG_ARVINV_INVOICEFROM + lcInvFrom + LANG_ARVINV_TO + lcInvTo +LANG_ARVINV_HAVEBEENVOIDED )
=gfModalGen('TRM00000B00000','ALERT','','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_INVOICEFROM,loFormSet.GetHeaderText("LANG_ARVINV_INVOICEFROM",loFormSet.HeaderAlias)) + lcInvFrom + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_TO,loFormSet.GetHeaderText("LANG_ARVINV_TO",loFormSet.HeaderAlias)) + lcInvTo +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARVINV_HAVEBEENVOIDED,loFormSet.GetHeaderText("LANG_ARVINV_HAVEBEENVOIDED",loFormSet.HeaderAlias)) )
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[End]
    *-- <Invoices from lcInvFrom to lcInvTo have been Voided.>
    *-- Button : 00000
    *-- <                         Ok                         >
  ENDIF
ENDIF

SELECT InvHdr
SET ORDER TO TAG (lcInvTag)

RETURN
*-- End of lfRelScr

