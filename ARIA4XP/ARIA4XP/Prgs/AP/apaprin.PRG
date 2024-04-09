*:************************************************************************
*:  Program File: APAPRIN.PRG
*:  Desc.       : Approve Installment
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/14/2012
*:  Reference   : E303065,1
*:************************************************************************
*!B609890,1 SAB 04/17/2012 Fix Error that occure while Opening the Screen [T20120304.0004]

*- Define include variables
#INCLUDE R:\aria4xp\prgs\ap\apaprin.h

*- Call the screen
lcRunScx = lfGetScx("AP\APAPRIN.scx")
DO FORM (lcRunScx)

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/14/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
FUNCTION lfGetScx
PARAMETERS lcScx
LOCAL lcRunScx
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientscreenhome+lcScx)
  lcRunScx = oAriaApplication.clientscreenhome+lcScx
ELSE
  lcRunScx = oAriaApplication.screenhome+lcScx
ENDIF   
RETURN lcRunScx
 *- End of lfGetScx.

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/01/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome+oAriaapplication.ActiveModuleID
SET PROCEDURE TO (lcPath+'\APMAIN.FXP') ADDITIVE 

*- Open tables 
=lfOpenPRGFILES('APAPRIN')

*** Load program base file 
=lfAddProp(loFormSet,'lcBaseFile',sydObjct.cBaseFile)

*- Define variables 
=lfDefineVars(loFormSet)

*- add property to create in it the class to call the vendor/ap invoice  screen 
=lfAddProp(loFormSet,'clsDataGrdValidations' ,'')
loFormSet.clsDataGrdValidations = CREATEOBJECT('clsDataGrdValidations')

*-- Define custom tool bar buttons
DECLARE loFormSet.lapanelobj[1,6] 
STORE '' TO loFormSet.lapanelobj
*-- Scope Button
loFormSet.laPanelObj[1,1] = 'pbScop'
loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"SCOPE.BMP"
loFormSet.laPanelObj[1,3] = 'lfGetData'    && the loFormSet is sent by default as a parameter 
loFormSet.laPanelObj[1,4] = "Option Grid"
loFormSet.laPanelObj[1,5] = "Option Grid"
loFormSet.laPanelObj[1,6] = 'S'

*** Check if there are any installments in the automatic header file
*** If there are not, present the following message and exit the program
SELECT APINVAHD
SET ORDER TO TAG TVENDINV
IF !SEEK('I')
  *** Message: " There are no ð.          "
  *** Choices: "          ® Ok ¯          "
  *=gfModalGen("TRM04101B00000","DIALOG", "installments to approve")
  =gfModalGen("TRM04101B00000","DIALOG", LANG_APAPRIN_lcTNoAprInv)  
  glQuitting = .T.
  RETURN .F.
ENDIF

DECLARE laWndObj[4,3], laFields[2,3]
  
*IF !WEXIST(gcBaseWind)
    
*** Get temporary names for the child windows
*lcAprInCh1  = gfTempName()
*lcAprInCh2  = gfTempName()
*lcAprInCh3  = gfTempName()
lc_Invoices = gfTempName()      && temp file
=lfAddProp(loFormSet,'lc_Invoices',lc_Invoices)
lcVenSum    = 'CWR'+SUBSTR(gfTempName(),2)
=lfAddProp(loFormSet,'lcVenSum',lcVenSum)
lcInvSum    = 'CWR'+SUBSTR(gfTempName(),2)
=lfAddProp(loFormSet,'lcInvSum',lcInvSum)

*** Get payment methods array and add an 'All' to the array (.T.)
DIMENSION laPayMeth[1,2]
lnPayMLen = gfGetVld('CVENPMETH', @laPayMeth, .T.)
*** Remove 'Credit cards' (C) option from the array
lcSetExact = SET('EXACT')
SET EXACT ON
lnElem = ASCAN(laPayMeth, 'C')
IF lnElem > 0 
  =ADEL(laPayMeth, ASUBSCRIPT(laPayMeth, lnElem, 1))
  DIMENSION laPayMeth[ALEN(laPayMeth,1)-1,2]  
ENDIF

=lfAddProp(loFormSet,'laPayMeth[1]','')
DIMENSION loFormSet.laPayMeth[ALEN(laPayMeth,1),ALEN(laPayMeth,2)]
ACOPY(laPayMeth,loFormSet.laPayMeth)

SET EXACT &lcSetExact

*** Create invoices temporary file as follows
*!B609890,1 SAB 04/17/2012 Fix Error that occure while Opening the Screen [T20120304.0004][Start]
*CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lc_Invoices) ;
  (linclude  L       ,;
   cInvNo    C(12)   ,;
   cVendCode C( 8)   ,;
   nNoOfInst N( 4, 0),;
   nInstAmnt N(15, 2),;
   nDisAmnt  N(10, 2),;
   n1099Amnt N(15, 2),;
   nOpenAmnt N(15, 2),;
   nAprAmnt  N(15, 2),;
   cBnkCode  C( 8)   ,;
   cChkAcct  C(12)   ,;
   cChkGlAcc C(24)   ,;   
   cTimStamp C(FSIZE('cAdd_User',loFormset.lcBaseFile) + 10 + ;
               FSIZE('cAdd_Time',loFormset.lcBaseFile)))
CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lc_Invoices) ;
  (linclude  L       ,;
   cInvNo    C(12)   ,;
   cVendCode C( 8)   ,;
   nNoOfInst N( 4, 0),;
   nInstAmnt N(15, 2),;
   nDisAmnt  N(10, 2),;
   n1099Amnt N(15, 2),;
   nOpenAmnt N(15, 2),;
   nAprAmnt  N(15, 2),;
   cBnkCode  C( 8)   ,;
   cChkAcct  C(12)   ,;
   cChkGlAcc C(24)   ,;   
   cTimStamp C(FSIZE('cAdd_User',ALLTRIM(loFormset.lcBaseFile)) + 10 + ;
               FSIZE('cAdd_Time',ALLTRIM(loFormset.lcBaseFile))))
*!B609890,1 SAB 04/17/2012 Fix Error that occure while Opening the Screen [T20120304.0004][End]

*** Default the generation date to the system date   
ldGenDate   = oAriaApplication.SystemDate          && generation date 
loFormset.Ariaform1.ldGenDate.Text1.Value = ldGenDate   
lcPayMeth   = laPayMeth[1,1]

LANG_APAPRIN_lcTAllCode  = gfCodDes('*' , ' ')
  
lcDivision  = LANG_APAPRIN_lcTAllCode    
  
*ELSE
*  rbScope = lnOldrbScp
*ENDIF

*!*	laWndObj [1,1] = lcAprInCh1
*!*	laWndObj [1,2] = "ldGenDate"
*!*	laWndObj [1,3] = "pbClose"

*!*	laWndObj [2,1] = lcAprInCh3
*!*	laWndObj [2,2] = "ldGenDate"
*!*	laWndObj [2,3] = "pbClose"

*!*	laWndObj [3,1] = lcVenSum
*!*	laWndObj [3,2] = "pbVeClose"
*!*	laWndObj [3,3] = "pbVeClose"

*!*	laWndObj [4,1] = lcInvSum
*!*	laWndObj [4,2] = "pbInClose"
*!*	laWndObj [4,3] = "pbInClose"

*** Array laFields is formed as follows :
*** A row for every field to be browsed
*** Column 1 : the object name corresponding to the field
*** Column 2 : the physical field name
*** Column 3 : the tag name to be used for seeking
laFields[1,1] = 'lcVendCode'         &&'lcVendCode'
laFields[1,2] = 'cVendCode'
laFields[1,3] = 'VENCODE'
laFields[2,1] = 'lcVendComp'         && 'lcVenComp'
laFields[2,2] = 'cVenComp'
laFields[2,3] = 'VENCOMP'

*** Prepare a fields popup for divisions from the
*** codes file
*Old : IF _WINDOWS
*Old : 
*Old :   lcDivision = CODES.cdiscrep
*Old :   DEFINE POPUP puDivision PROMPT FIELD CODES.cdiscrep SCROLL;
*Old :     FROM 5.63,12.88 TO 9.73,46.0;
*Old :     MESSAGE gfObj_msg()
*Old : 
*Old :   *E300643,1 Change this lines [End]
*Old :   
*Old :   ON SELECTION POPUP puDivision DO lfvDivision
*Old : ENDIF  

*Old : lcCheck        = ' û '
*Old : lcUncheck      = SPACE(3) 

*** Set the tag of the payable invoices file to VENDINV
*** tag expression : cVendCode + cInvNo
SET ORDER TO TAG VENDINV IN APINVHDR

*** Set the tag of the vendors file to VENCODE
*** tag expression : cVendCode 
SET ORDER TO TAG VENCODE IN APVENDOR

*** Set the tag of the vendor history file to VENDYEAR
*** tag expression : cVendCode + cFisFYear
SET ORDER TO TAG VENDYEAR IN APVENHST

SELECT CODES

SET ORDER  TO 0
SET FILTER TO CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'CDIVISION';
           .OR. CRLTFIELD+CFLD_NAME = 'N'+'N/A';
           .OR. CRLTFIELD+CFLD_NAME = 'N'+'ALL'

SET ORDER TO TAG IDRLTFNAME

*** Set the tag of the automatic invoice header file to TVENDINV
*** tag expression : cAutMType + cVendCode + cInvNo
SELECT APINVAHD
SET ORDER TO 0
SET FILTER TO
*lcInvFlt = loFormSet.lcInvFlt
SET FILTER TO EVALUATE(loFormSet.lcInvFlt) 
SET ORDER TO TAG TVENDINV
SET RELATION TO cVendCode INTO APVENDOR ADDITIVE
SET RELATION TO cVendCode + cInvNo INTO APINVHDR ADDITIVE
lcCurrYear = loFormSet.lcCurrYear
SET RELATION TO cVendCode + '&lcCurrYear' INTO APVENHST ADDITIVE

SELECT (loFormSet.lc_Invoices) 
SET RELATION TO 'I' + cVendCode + cInvNo INTO APINVAHD

*!*	*** Prepare key settings for the browse
*!*	PUSH KEY
*!*	lcOnCtrlEntr  = ON("KEY","CTRL+ENTER")
*!*	lcOnEsc       = ON("KEY","ESC")
**ON KEY LABEL TAB         DO lpTab      
**ON KEY LABEL BACKTAB     DO lpShiftTab 


*- Define Grid titles 
DECLARE loFormSet.laBrHdrs[12]
WITH loFormSet
  .laBrHdrs[1]  = "Inc"
  .laBrHdrs[2]  = "Inv. No."
  .laBrHdrs[3]  = "Vendor"
  .laBrHdrs[4]  = "No."
  .laBrHdrs[5]  = "Installments"
  .laBrHdrs[6]  = "Discount"
  .laBrHdrs[7]  = "1099 amount"
  .laBrHdrs[8]  = "Open amount"
  .laBrHdrs[9]  = "Approved amount"
  .laBrHdrs[10] = "Bank"
  .laBrHdrs[11] = "Checking Ac."
  .laBrHdrs[12] = "G\L account"
ENDWITH 

*- update the grid column's sources
loFormSet.ariaform1.grdData.RecordSource = ''
=lfGetGridColmns(loFormSet)

*!*	*- Add flag lf1stRun 
*!*	=lfAddProp(loFormSet,'lf1stRun',.T.)

*!*	*- fill up the temp file and collect data
*!*	IF ! loFormSet.lf1stRun
*!*	  =lfGetData(loFormset)
*!*	ENDIF   
*- End of lfFormInit.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/01/2012
*! Purpose   : Define variables as properties
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

DECLARE laBrHdrs[12],laTemp[1], laPayMeth[1,2]


=lfAddProp(loFormSet,'ap1',CREATEOBJECT('ap'))

lcVars =            'lcTAllCode , lcAprInCh1 , lcAprInCh2 , lcAprInCh3 ,'+;
                    'lcBrInvTtl , lcVendTtl  , lcInvoice  ,'+;
                    'lcDivision , lcDivOpt   ,'+;
                    'lcPayMeth  , lcPMethOpt , lcVendComp , lcOldVal   ,'+;
                    'puDivision,laBrHdrs[1] '
=lfAddProp(loFormSet,lcVars,'')

lcVars =            'ldGenDate  , ldOldVal   , ldDueFrom  , ldDueTo    ,'+;
                    'ldDiscFrom , ldDiscTo'
=lfAddProp(loFormSet,lcVars,{})

=lfAddProp(loFormSet,'llBrowse',.F.)

lcVars =            'llUpdated  , llMayGenr  , llNoContrl' 
=lfAddProp(loFormSet,lcVars,.T.)

lcVars =            'lnTime     , lnBrRecNo  , lnPayMLen  , lnOldVal   ,'+;
                    'lnTotPay   , lnTotDisc  , lnTotAdj'
=lfAddProp(loFormSet,lcVars,0)

lcVars =            'rbScope    , rbDates    , puPayMeth  , lnOldrbScp' 
=lfAddProp(loFormSet,lcVars,1)

=lfAddProp(loFormSet,'lcPayPrior' ,' ')
=lfAddProp(loFormSet,'lcVendCode' ,SPACE(8))
=lfAddProp(loFormSet,'lcInvRef' ,SPACE(15))
=lfAddProp(loFormSet,'lcGenStat ' ,'ENABLE')
=lfAddProp(loFormSet,'lcAprStat' ,'DISABLE')
=lfAddProp(loFormSet,'laCtrStat ' ,'DISABLE')
=lfAddProp(loFormSet,'lcSlPr' ,'\<Select'  )
=lfAddProp(loFormSet,'lcTBrowse' ,'\<Browse'  )
=lfAddProp(loFormSet,'llSYCCOMP, llFSPRD' ,.F.)

lcMainFlt   = [cAutMType + cVendCode + cInvNo = 'I'] 
=lfAddProp(loFormSet,'lcInvFlt,lcMainFlt',lcMainFlt)


llSycComp   = gfOpenTable(oAriaApplication.SysPath+'SYCCOMP'  ,'Ccomp_id','SH')
*IF SEEK(oAriaApplication.PrntCompanyID,'SYCCOMP')
SELECT SYCCOMP
GO TOP
LOCATE FOR Ccomp_id = oAriaApplication.PrntCompanyID
IF FOUND()
  lcCurrYear  = SYCCOMP.cCurr_Yer
  *lcDataDir   = gfGetDataDir(ALLT(SYCCOMP.cCom_DDir))
ELSE
  lcCurrYear  = " "
  *lcDataDir   = gcDataDir
ENDIF
=lfAddProp(loFormSet,'lcCurrYear',lcCurrYear)
IF llSycComp .AND. USED('SYCCOMP')
  =gfCloseTable('SYCCOMP')
ENDIF

=lfAddProp(loFormSet,'ldCurDate',DATE())
=lfAddProp(loFormSet,'llVendExst',.T.)

=lfAddProp(loFormSet,'laOgFxFlt[1]','')
=lfAddProp(loFormSet,'laOgVrFlt[1]','')

*- Date type
=lfAddProp(loFormSet,'lcRpDtType','1')


=lfAddProp(loFormSet,'lcTApr',"The total approved amount")
=lfAddProp(loFormSet,'lcTOpenAmt',"the open invoice amount")
=lfAddProp(loFormSet,'lcTAprInst',"the approved installment amount")
=lfAddProp(loFormSet,'lcT1099',"The approved 1099 amount")

*- End of lfDefineVars.

********************************************************************************
*Name       : lfGetGridColmns
*Developer  : TMI
*Date       : 01/26/2012
*Purpose    : update the grid column's sources
********************************************************************************
FUNCTION lfGetGridColmns
PARAMETERS loFormSet

LOCAL lc_Invoices

WITH loFormSet.ariaform1.grdData
  lc_Invoices = loFormSet.lc_Invoices
  
  LOCAL lnI,lcI
  lnI = 0  && the first column is the SELECTORE one
  oGrid = loFormSet.ariaform1.grdData 
  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..linclude',loFormSet.laBrHdrs[1],30)
  BINDEVENT(oGrid.Column1.chkSelect,'Click',loFormset.Ariaform1.cmdSelect,'Click')
  
  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..cInvNo',loFormSet.laBrHdrs[2],75)
  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..cVendCode',loFormSet.laBrHdrs[3],75)
  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..nNoOfInst',loFormSet.laBrHdrs[4],75)

  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..nInstAmnt',loFormSet.laBrHdrs[5],75,.T.) 
  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..nDisAmnt',loFormSet.laBrHdrs[6],75,.T.)   
  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..n1099Amnt',loFormSet.laBrHdrs[7],IIF(!loFormSet.ap1.llApS1099,75,0),.T.)
  
  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..nOpenAmnt',loFormSet.laBrHdrs[8],75)
  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..nAprAmnt' ,loFormSet.laBrHdrs[9],75)  
  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..cBnkCode',loFormSet.laBrHdrs[10],75)
  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..cChkAcct',loFormSet.laBrHdrs[11],75)
  =lfAddColumn(@lnI,oGrid,'&lc_Invoices..cChkGlAcc',loFormSet.laBrHdrs[12],75)
  .Refresh()
ENDWITH 

*- Remove extra columns 
FOR lnX = lnI+1 TO oGrid.ColumnCount
  lcX = ALLTRIM(STR(lnX))
  oGrid.RemoveObject("Column&lcX")
ENDFOR 
*- End of lfGetGridColmns.

*!*************************************************************
*! Name      : lfAddColumn
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303014,1 TMI 01/03/2011 
*! Purpose   : A function to add columns to the passed grid object
*!*************************************************************
FUNCTION lfAddColumn
LPARAMETERS lnI,oGrid,lcFld,lcTitle,lnWidth,llEditable
lnWidth = IIF(EMPTY(lnWidth),75,lnWidth)
LOCAL lcI
lnI = lnI + 1
lcI = ALLTRIM(STR(lnI))
WITH oGrid
  .Column&lcI..ControlSource    = ALLTRIM(lcFld)
  .Column&lcI..Header1.Caption  = lcTitle
  .Column&lcI..Width            = lnWidth
  .Column&lcI..Readonly         = IIF(llEditable,.F.,.T.)
ENDWITH 
*- End of lfAddColumn.

***************************************************************************************
* Name      : clsDataGrdValidations
* Developer : Tmi Trek mohammed Ibfrahim
* Date      : 03/01/2012
* Purpose   : Define a class to bind its method to the grdData object
***************************************************************************************
DEFINE CLASS clsDataGrdValidations AS Custom

  PROCEDURE lfwOldBrVals
    
  ENDPROC
  
ENDDEFINE
*- End of clsDataGrdValidations.
************************************************************************************
*Name       : lfFormActivate
*Developer  : TMI
*Date       : 01/08/2012 
*Purpose    : Run lfGetData in the activate method for the first time the screen runs
************************************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- Add flag lf1stRun in case that sometimes the formset does not recognized that this property was added
*!*	=lfAddProp(loFormSet,'lf1stRun',.T.)
*!*	IF loFormSet.lf1stRun
*!*	  loFormSet.lf1stRun = .F.
*!*	  =lfGetData(loFormset)
*!*	ELSE     
  *- refresh the grid
  loFormset.Ariaform1.grdData.SetFocus()
  loFormset.Ariaform1.grdData.Refresh()
*!*	ENDIF   
 *- End of lfFormActivate.

***********************************************************************************
*Name    : lfGetData
*Developer : TMI - Tarek Mohamed Inbrahim
*Date    : 12/24/2011
*Purpose : called from the AfterRowColChange event in the grdApprove on the screen
***********************************************************************************
FUNCTION lfGetData
PARAMETERS loFormSet

loFormSet.ariaform1.grdData.RecordSource = '' 

*- Call the "APINSTM" OG
LOCAL lcExpr

IF FILE(oAriaApplication.WorkDir+loFormSet.lc_Invoices+'.MEM')
  RESTORE FROM (oAriaApplication.WorkDir+loFormSet.lc_Invoices+'.MEM' ) ADDITIVE 
ENDIF 

DECLARE laOgFxFlt[1,2],laOgVrFlt[1,2]    && define filter arrays to use after the OG is closed
lcRpDtType = 1

*E303065,4 TMI 03/13/2012 [Start] change the id of the sydreprt entry
*lcExpr = gfOpGrid('APINSTM',.T. ,.F. ,.F. ,.T. ,.T.)
lcExpr = gfOpGrid('APAPRIN',.T. ,.F. ,.F. ,.T. ,.T.)
*E303065,1 TMI 03/13/2012 [End  ] 


loFormSet.lcRpDtType = INT(lcRpDtType)

*- if cancel clicked , go to Select Mode
IF lcExpr == '.F.'
  loFormSet.ChangeMode('S')   
  RETURN 
ENDIF
loFormSet.rbScope = IIF(lcExpr == '.T.',1,2)
*-Clear data from temp files
SELECT (loFormSet.lc_Invoices)
GO TOP 
IF !EOF()  
  IF gfModalGen("TRM04004B04004", "DIALOG") = 1
    =lfClearGen(loFormSet)
*!*	  ELSE
*!*	    *** If rbScope = 2, reset llOnvOk (as if cancelled)
*!*	    llInvOk = rbScope <> 2 
*!*	    rbScope = lnOldrbScp
*!*	    SHOW GET rbScope
  ELSE 
    loFormSet.ariaform1.grdData.RecordSource = loFormSet.lc_Invoices
    RETURN .F.
  ENDIF    
ENDIF 
SELECT (loFormSet.lc_Invoices)
ZAP

*- save the selected criteria to a mem file
SAVE TO (oAriaApplication.WorkDir+loFormSet.lc_Invoices+'.MEM' ) ALL LIKE laOg*
DIMENSION loFormSet.laOgFxFlt[ALEN(laOgFxFlt,1),ALEN(laOgFxFlt,2)]
DIMENSION loFormSet.laOgVrFlt[ALEN(laOgVrFlt,1),ALEN(laOgVrFlt,2)]
=ACOPY(laOgFxFlt,loFormSet.laOgFxFlt)
=ACOPY(laOgVrFlt,loFormSet.laOgVrFlt)

loFormSet.ariaform1.grdData.RecordSource = loFormSet.lc_Invoices 

*- go to S mode to be ready for Generate
loFormSet.ChangeMode('S')

*- End of lfGetData.

*!*************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303011,1 TMI 12/13/2011 
*! Purpose   : called from the Screen Change mode Method
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet
LOCAL llEnable
llEnable = .F.

DO CASE 
  CASE loFormSet.ActiveMode = 'S'
    
    llEnable = .F.
    
    oAriaApplication.oToolBar.ChangeButtonStatus('pbScop','ENABLED')    
    
    WITH loFormset.Ariaform1
      .cmdGenerate.Enabled = .T.
      .ldGenDate.Enabled = .T.

      .cmdApprove.Enabled = .F.

      .cmdVendor.Enabled = .F.
      .cmdInvoices.Enabled = .F.

      .cmdSelNon.Enabled = .F.
      .cmdSelAll.Enabled = .F.
      .cmdInvert.Enabled = .F.
      .cmdSelect.Enabled = .F.
    ENDWITH 

  CASE loFormSet.ActiveMode = 'E'

    llEnable = .T.

    oAriaApplication.oToolBar.ChangeButtonStatus('pbScop','DISABLED')    
    WITH loFormSet.Ariaform1
*!*	      .cmdApprove.Enabled = .F.

*!*	      .cmdVendor.Enabled = .F.
*!*	      .cmdInvoices.Enabled = .F.

*!*	      .cmdSelNon.Enabled = .F.
*!*	      .cmdSelAll.Enabled = .F.
*!*	      .cmdInvert.Enabled = .F.
*!*	      .cmdSelect.Enabled = .F.
    ENDWITH 

ENDCASE   

WITH loFormSet.oToolBar
  *.cmdSelect.Enabled = .F.
  .cmdFind.Enabled = .F.
  .cmdTop.Enabled = .F.
  .cmdPrev.Enabled = .F.
  .cmdNext.Enabled = .F.
  .cmdEnd.Enabled = .F.
  .cmdEdit.Enabled = .F.  
ENDWITH 
*- End of lfChangeMode.
************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/06/2012
*! Purpose   : Form BeforeSave
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

SELECT (loFormSet.lc_Invoices)
LOCATE FOR LAPPROVED
llFound = FOUND()
IF !llFound
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No invoices have been approved')
ENDIF
SELECT(lnSlct)

RETURN llFound
*- End of lfFormBeforeSave.
*!*************************************************************
*! Name      : lfFormSavefiles
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/13/2011 
*! Purpose   : Save data
*!*************************************************************
FUNCTION lfFormSavefiles
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

SELECT APINVHDR
gfTableUpdate()
SELECT APINVAHD
gfTableUpdate()
  
SELECT(lnSlct)
*- End of lfFormSavefiles.

************************************************************
*! Name      : lfAfterRowColChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/01/2012
*! Purpose   : AfterRowColChange Grid method
************************************************************
FUNCTION lfAfterRowColChange
PARAMETERS loGrid
LOCAL lc_Invoices
lc_Invoices = loGrid.RecordSource
WITH loGrid.Parent
  .cmdSelect.Caption = IIF(&lc_Invoices..LINCLUDE,'Un Se\<lect','Se\<lect')
  .cmdSelect.Refresh
ENDWITH 
*- End of lfAfterRowColChange.

************************************************************
*! Name      : lfCallDetFrm
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/01/2012
*! Purpose   : lfCallDetFrm
************************************************************
FUNCTION lfCallDetFrm
PARAMETERS lcType,loFormSet
DO FORM (oAriaApplication.ScreenHome+"\AP\APAPLDI.SCX") WITH lcType,loFormSet

*- End of lfvInvoice.

*!*************************************************************
*! Name      : lfInitDet
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Debit/Invoice Summary Screen init
*!*************************************************************
*
FUNCTION lfInitDet
LPARAMETERS loBrnchFormSet
LOCAL loPrentForm ,lcType
PRIVATE lc_InvHdr
#INCLUDE R:\aria4xp\PRGS\ap\APAPLDB.h

loPrentForm = loBrnchFormSet.loPrentForm 
lcType = loBrnchFormSet.lcType 
*lc_InvHdr = loPrentForm.lc_InvHdr 
SELECT (loPrentForm.lc_invoices)
SET RELATION TO cVendCode + cInvNo INTO APINVHDR ADDITIVE
GO RECNO()
lc_InvHdr = 'APINVHDR'

*!*	lc_debit = loPrentForm.lc_debit 
DIMENSION loBrnchFormSet.laRemitTo[3,2]
loBrnchFormSet.laRemitTo = ''
DIMENSION laArray[ALEN(loBrnchFormSet.laRemitTo,1),2]
=ACOPY(loBrnchFormSet.laRemitTo,laArray)
lnRemitLen   = gfGetVld('cInvRemit',@laArray)
DIMENSION loBrnchFormSet.laRemitTo[ALEN(laArray,1),2]
=ACOPY(laArray,loBrnchFormSet.laRemitTo)
WITH loBrnchFormSet.ARIAForm1
  .caption = IIF(lcType = 'I' ,LANG_APAPLDB_INVINFO, LANG_APAPLDB_DBINFO)
  .LBl1099.Caption = LANG_APAPLDB_1099AMT
  .LBLAdj.Caption = LANG_APAPLDB_ADJAPP
  .lblAmt.Caption = IIF(lcType = 'I',LANG_APAPLDB_AMOUNT,LANG_APAPLDB_DBAMOUNT)
  .lblDate.Caption = IIF(lcType = 'I',LANG_APAPLDB_DATE,LANG_APAPLDB_DBDATE)
  .lblDisc.Caption = LANG_APAPLDB_DISCOUNT
  .lblDueDate.Caption = IIF(lcType = 'I',LANG_APAPLDB_DUEDATE ,LANG_APAPLDB_DBDUEDATE )
  .lblInvdbNo.Caption = IIF(lcType = 'I',LANG_APAPLDB_INVOICE,LANG_APAPLDB_DBTMEMO )
  .lblPaid.Caption =  LANG_APAPLDB_PAID
  .lblRemit.Caption = LANG_APAPLDB_REMIT
  .kbInvNo.Enabled = .F.
  .kbInvNo.keytextbox.Enabled = .F.
  .kbInvNo.keycmd.Enabled = .F.
  .cboInvRemit.ROWSource =  "ThisFormSet.laRemitTo"

  SELECT (loPrentForm.lc_invoices)
  GO RECNO()

  IF lcType = 'I'
    .cboInvRemit.Value = &lc_InvHdr..cinvremit
    .kbInvNo.keytextbox.VAlue = &lc_InvHdr..cinvNo
    .DtpDate.VAlue = &lc_InvHdr..dinvdate
    .DtpDue.VAlue = &lc_InvHdr..dinvdudat
    .txt1099Amt.VAlue = &lc_InvHdr..ninv1099a
    .txtamt.VAlue = &lc_InvHdr..ninvamnt
    .txtPaid.VAlue = lfPaidAmount('I')
    .txtDiscount.VAlue = &lc_InvHdr..ninvdistk
    .txtAdju.VAlue = &lc_InvHdr..nInvAdj 
  ELSE
    .cboInvRemit.Value = &lc_debit..cinvremit
    .kbInvNo.keytextbox.VAlue = &lc_debit..cinvNo
    .DtpDate.VAlue = &lc_debit..dinvdate
    .DtpDue.VAlue = &lc_debit..dinvdudat
    .txt1099Amt.VAlue = &lc_debit..ninv1099a
    .txtamt.VAlue = &lc_debit..ninvamnt
    .txtPaid.VAlue = lfPaidAmount('D')
    .txtDiscount.VAlue = &lc_debit..ninvdistk
    .txtAdju.VAlue = &lc_debit..nInvAdj 
  ENDIF   
ENDWITH  

SET RELATION OFF INTO APINVHDR
*!*************************************************************
*! Name      : lfPaidAmount
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Get Paid Amount
*!*************************************************************
*
FUNCTION lfPaidAmount
  LPARAMETERS lcType

  LOCAL lnPaidAmount
  IF lcType = 'I'
    lnPaidAmount = &lc_InvHdr..ninvpaid + &lc_InvHdr..ninvadj
  ELSE
    lnPaidAmount = &lc_debit..ninvpaid + &lc_debit..ninvadj
  ENDIF     
  RETURN lnPaidAmount


*!**************************************************************************
*!
*!        Function : lfwOldVals
*!
*!**************************************************************************
FUNCTION lfwOldVals
PARAMETERS lcOldObjNm
&lcOldObjNm = EVALUATE(SYS(18))

*!**************************************************************************
*!
*!        Function : lfwOldBrVals
*!
*!**************************************************************************
FUNCTION lfwOldBrVals
PARAMETERS loFormSet,lnFldContent
loFormSet.lnOldVal = lnFldContent

*!**************************************************************************
*!
*!        Function : lfvBrAmnts
*!
*!**************************************************************************
FUNCTION lfvBrAmnts
PARAMETERS loFormSet,lcAmntFld, lnAmnt, lnCmpAmnt, lcTAmtStr, lcTCmpStr
*** If the enetered value is negative, present the folllowing
*** message and return the old value,
*** Message: " Negative values are not allowed.  "
*** Choices: "              ® Ok ¯               "
*** Or, 
*** If the entered value is greater than the comparison
*** value, present the following message and return the old value
*** Message: " ð cannot be greater than ð.       "
*** Choices: "              ® Ok ¯               "
lc_Invoices = loFormSet.lc_Invoices
IF ( &lc_Invoices..&lcAmntFld < 0 .AND.;
  gfModalGen("TRM04087B00000", "DIALOG") > 0);
 .OR.;
 (lnAmnt > lnCmpAmnt .AND. ;
   gfModalGen("TRM04025B00000", "DIALOG",lcTAmtStr+'|'+lcTCmpStr) > 0)
  REPLACE &lc_Invoices..&lcAmntFld WITH loFormSet.lnOldVal
  * Old : SHOW WINDOW (lcBrInvTtl) REFRESH SAME  
ENDIF  


*!**************************************************************************
*!
*!        Function : lfvGenDate
*!
*!**************************************************************************
* Valid function for get field ldGenDate (generation date)
*
FUNCTION lfvGenDate
PARAMETERS loFormSet,loFld
lc_Invoices = loFormSet.lc_Invoices
*IF ldGenDate <> ldOldVal .AND. WVISIBLE(gcBaseWind)
IF loFld.Value <> loFld.OldValue &&.AND. WVISIBLE(gcBaseWind)
  IF !EMPTY(loFld.Value) .AND. !lfVlDate(oAriaApplication.PrntCompanyID)
    *** If the date is invalid, present the following message
    *** and blank the field
    *** Message: " The ð date should be within the posting window."
    *** Choices: "                       ® Ok ¯                   "
    =gfModalGen('TRM04113B00000', 'DIALOG', LANG_APAPRIN_lcTIns)
    *old :ldGenDate = IIF(RECCOUNT(lc_Invoices) > 0, ldOldVal, {})
    loFld.Value = IIF(RECCOUNT(lc_Invoices) > 0, loFld.OldValue , {})
    *_CUROBJ   = OBJNUM(ldGenDate)
    RETURN .F.
  ELSE
    *** If there are generated invoices, and the 
    *** date is changed, present a warning message that
    *** generated invoices are going to be disacrded.
    *** Message: " The generated invoices will be discarded! "
    *** Choices: "          <   Ok   >    < Cancel >         "
    IF RECCOUNT(lc_Invoices) > 0 
      IF gfModalGen("TRM04004B04004", "DIALOG") = 1
        =lfClearGen(loFormSet)
      ELSE
        *old : ldGenDate = ldOldVal
        loFld.Value = loFld.OldValue
        *_CUROBJ   = OBJNUM(ldGenDate)
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
  *ldOldVal = ldGenDate
ENDIF

*!**************************************************************************
*!
*!      Function: lfvGenerate
*!
*!**************************************************************************
*  Valid function for push button update
**x
FUNCTION lfvGenerate
PARAMETERS loFormSet

PRIVATE lcCurAlias, lcScanCmd, lnTotRec, lnCurRec, lnCount, lnNoOfInst ,;
        ln1099InstNo, lnInstAmnt, ln1099Amnt, lnOpenAmnt
        
*- Get the generation variables
ldGenDate = loFormset.Ariaform1.ldGenDate.Text1.Value        
loformset.ariaform1.grdData.RecordSource = ''

*** If the generation date is empty, present the following message
*** and do not generate, else continue with the generaton process.
IF EMPTY(ldGenDate)
  *** Message: " You have to enter the ð.  "
  *** Choices: "            ® Ok ¯         "
  =gfModalGen("TRM04066B00000", "DIALOG", LANG_APAPRIN_lcTInsDate)
  _CUROBJ = OBJNUM(ldGenDate)
ELSE
  
  
  oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
  oProgress.TotalProgress = RECCOUNT('APINVAHD')
  oProgress.lblFirstLabel.CAPTION = LANG_APAPRIN_lcTGenIns
  lnThermNo = 0
  oProgress.SHOW()

  WITH loFormSet
  
  lcCurAlias  = ALIAS()
  SET ORDER TO 0 IN APINVAHD
  IF .rbScope = 1
    loFormSet.lcInvFlt  = .lcMainFlt
    lcScanCmd = ""
  ELSE
    *- Get here the variables values from the laOg* arrays
    RESTORE FROM (oAriaApplication.WorkDir+loFormSet.lc_Invoices+'.MEM' ) ADDITIVE 
    lcVendCode = lfGetCrVal("APINVAHD.CVENDCODE")
    lcDivOpt = lfGetCrVal("APINVAHD.CDIVISION")
    lcPMethOpt = lfGetCrVal("APINVAHD.CVENPMETH")
    lcPayPrior = lfGetCrVal("APINVAHD.CVENPRIOR")
    lcDue = lfGetCrVal("APINVAHD.DINVDUDAT")
    lcDis = lfGetCrVal("APINVHDR.DINVDATE")
    lcInvRef = lfGetCrVal("APINVAHD.CINVREF")
    rbDates  = lfGetCrVal("rbDates")
    
    ldDueFrom = CTOD(SUBSTR(lcDue,1,AT('|',lcDue)-1))
    ldDueTo   = CTOD(SUBSTR(lcDue,AT('|',lcDue)+1))
    ldDiscFrom = CTOD(SUBSTR(lcDis,1,AT('|',lcDis)-1))
    ldDiscTo   = CTOD(SUBSTR(lcDis,AT('|',lcDis)+1))
    
    rbDates = loFormSet.lcRpDtType
    
    loFormSet.lcInvFlt  = .lcMainFlt + ;
                IIF(!EMPTY(lcVendCode), [.AND. SEEK(cVendCode,'&lcVendCode')],"")
    lcScanCmd = IIF(!EMPTY(lcInvRef),;
                [.AND.LIKE(STRTRAN(UPPER(lcInvRef),' ','?'),UPPER(APINVHDR.cInvRef))],"") +;
                IIF(!EMPTY(lcDivOpt), [.AND.APINVHDR.cDivision$lcDivOpt],"") + ;
                IIF(!EMPTY(lcPayPrior),[.AND.APINVHDR.cVenPrior=lcPayPrior],"") +;
                IIF(!EMPTY(lcPMethOpt),[.AND.APINVHDR.cVenPMeth$lcPMethOpt],"")+;
                IIF(rbDates = 1,;
                    IIF(!EMPTY(ldDueFrom),;
                        IIF(!EMPTY(ldDueTo),;
                            [.AND.BETWEEN(APINVHDR.dInvDuDat,ldDueFrom,ldDueTo)],;  
                            [.AND.APINVHDR.dInvDuDat >= ldDueFrom]),;
                        IIF(!EMPTY(ldDueTo),;
                            [.AND. APINVHDR.dInvDuDat <= ldDueTo], " ")),;
                     IIF(!EMPTY(ldDiscFrom),;
                         IIF(!EMPTY(ldDiscTo),;
                             [.AND.BETWEEN(APINVHDR.dInvDate+APINVHDR.nTerDiscD, ldDiscFrom, ldDiscTo)],;  
                             [.AND.APINVHDR.dInvDate+APINVHDR.nTerDiscD >= ldDiscFrom]),;
                           IIF(!EMPTY(ldDiscTo),;
                             [.AND.APINVHDR.dInvDate+APINVHDR.nTerDiscD <= ldDiscTo], " ")))
  ENDIF

  *** Refresh the filter expression in the APINVAHD
  SET ORDER TO TAG TVENDINV IN APINVAHD
  GO TOP IN APINVAHD

  *** If the filter expression produces an eof in apinvahd,
  *** the total records is 0
  lnTotRec      = IIF(EOF('APINVAHD'), 0, RECCOUNT('APINVAHD'))

  
  IF lnTotRec > 0
  
    SELECT (loFormSet.lc_Invoices)
    SET RELATION OFF INTO APINVAHD
    ZAP

    *** Prepare thermometer variables
    lnTotRec     = lnTotRec + 1
    lnCurRec     = 0
    
    *E300789,4 IHB Remove company ID [start]
    *E301077,80 IHB 03/04/1999 opend needed files only [start]
    *=gfOpenFile(oAriaApplication.DataDir+'FSPRD' ,'COMFYRPRDI','SH')
    *E300789,4 IHB [end]
  
    *** If the current vendor has a zero payment priority, 
    *** or, if the current invoice has a zero payment priority,
    *** present the following message and do not invluce the 
    *** current invoice in the temporary file
    *** Message: " ð has a zero payment priority.  ð is "
    ***          " on hold.  ð cannot be approved.      "
    *** Choices: "                ® Ok ¯                "
    SELECT APINVAHD
  
    SCAN FOR !EMPTY(dAutNxGen) ;
       .AND. dAutNxGen <= ldGenDate;
       .AND. lfVldNextDate(dAutNxGen) ;  
       .AND. (APVENDOR.cVenPrior <> '0' .OR. ;
              gfModalGen("TRM04070B00000", "DIALOG",;
                          LANG_APAPRIN_lcTVendor + ALLTRIM(cVendCode) + '|' +;
                          LANG_APAPRIN_lcTThisVend + '|' + LANG_APAPRIN_lcTVenInst + ALLTRIM(cInvNo)) < 0);
       .AND. (APINVHDR.cVenPrior <> '0' .OR. ;
              gfModalGen("TRM04070B00000", "DIALOG",;
                          LANG_APAPRIN_lcTVendor + ALLTRIM(cVendCode) + ' ' +;
                          LANG_APAPRIN_lcTInvoice + ALLTRIM(cInvNo) + '|' +;
                          LANG_APAPRIN_lcTThisInv + '|' + LANG_APAPRIN_lcTInvInst) < 0);
       .AND. (APINVHDR.nInvAmnt - APINVHDR.nInvPaid ;
               - APINVHDR.nInvDisTk - APINVHDR.nInvAdj) <> 0 ;
       &lcScanCmd        
               
      *** Increment thermometer step
      *lnCurRec = lnCurRec + 1
      
      *=gfThermo(lnTotRec, lnCurRec, LANG_APAPRIN_lcTGenIns,;
                LANG_APAPRIN_lcTVendor + APINVAHD.cVendCode + ;
                SPACE(3) + LANG_APAPRIN_lcTInvoice + APINVAHD.cInvNo)

      lnThermNo = lnThermNo + 1
      oProgress.CurrentProgress(lnThermNo)
      oProgress.lblFirstLabel.CAPTION = LANG_APAPRIN_lcTVendor + APINVAHD.cVendCode + ;
                SPACE(3) + LANG_APAPRIN_lcTInvoice + APINVAHD.cInvNo

      *** Get the number of installments due from the next generation
      *** date of the installment until the generation date
      *B600624,1 Get the closest greater integer for the
      *B600624,1 calculation of the number of installments (CEILING)
      *B600624,1 instead of getting the INTeger.
      *B600624,1 Add a 1 to the number of days only.
      *lnNoOfInst   = IIF(APINVAHD.cAutFUnit = 'D',;
                         INT((ldGenDate - APINVAHD.dAutNxGen);
                         / APINVAHD.nAutFreq),;
                         INT(lfGetNoOfInst() / APINVAHD.nAutFreq)) + 1
      lnNoOfInst   = IIF(APINVAHD.cAutFUnit = 'D',;
                         INT((ldGenDate - APINVAHD.dAutNxGen);
                         / APINVAHD.nAutFreq) + 1,;
                         CEILING(lfGetNoOfInst() / APINVAHD.nAutFreq))
      *B600624,1 end.  
                           
      *** Calculate the open invoice amount taking the maximum of the 
      *** offered discount and the discount taken.
      lnOpenAmnt   =  APINVHDR.nInvAmnt - APINVHDR.nInvPaid ;
                      - MAX(APINVHDR.nInvDisTk, APINVHDR.nInvDisOf) ;
                      - APINVHDR.nInvAdj
      
      *** Total installment amount is the minimum of
      *** The total installment amount calculated as follows :
      *** If base is 'A'mounts : installment * no of installments
      *** else                 : installment % * invoice amount *
      ***                        no of installments
      *** and the open invoice amount calculated above. 
      lnInstAmnt   =  MIN( IIF(APINVAHD.cAutMBase = 'A',;
                           APINVAHD.nInvAmtAp,;
                           APINVHDR.nInvAmnt * APINVAHD.nInvAmtAp / 100) ;
                           * lnNoOfInst, lnOpenAmnt)

      *** Total 1099 amount is the minimum of
      *** The total 1099 amount calculated as follows :
      *** If base is 'A'mounts : 1099 amount * no of possible installments
      *** else                 : 1099 amount % * invoice amount *
      ***                        no of possible installments
      *** and the open invoice amount calculated above. 

      *** The number of possible installments is the quotient of
      *** dividing the total installments amount by an installment
      *** amount.
      ln1099InstNo = INT(lnInstAmnt / APINVAHD.nInvAmtAp)
      ln1099Amnt   =  MIN( IIF(APINVAHD.cAutMBase = 'A',;
                           APINVAHD.nInvA1099,;
                           APINVHDR.nInvAmnt * APINVAHD.nInvA1099 / 100) ;
                           * ln1099InstNo, lnOpenAmnt)

      INSERT INTO (loFormSet.lc_Invoices);
          (cInvNo, cVendCode, nNoOfInst, nInstAmnt,;
           nDisAmnt, n1099Amnt, nOpenAmnt, nAprAmnt, cBnkCode, cChkAcct, cChkGlAcc ,cTimStamp);
        VALUES(APINVAHD.cInvNo, APINVAHD.cVendCode, lnNoOfInst, lnInstAmnt,;
               MAX(APINVHDR.nInvDisOf - APINVHDR.nInvDisTk, 0), ln1099Amnt,;
               APINVHDR.nInvAmnt - APINVHDR.nInvPaid ;
               - APINVHDR.nInvDisTk - APINVHDR.nInvAdj,;
               APINVHDR.nInvAmtAp + APINVHDR.nInvDisAp + APINVHDR.nInvAdjAp,;
               APINVAHD.cBnkCode, APINVAHD.cChkAcct, APINVAHD.cChkGlAcc,;
               APINVHDR.cAdd_User + DTOC(APINVHDR.dAdd_Date) + ;
               APINVHDR.cAdd_Time)
               

      SELECT APINVAHD
    ENDSCAN
    
    oProgress = NULL

    *** Close the thermometer if not already closed
    *IF lnCurRec > 0 .AND. lnTotRec > lnCurRec
    *  FOR lnCount = lnCurRec TO lnTotRec
    *    =gfThermo(lnTotRec, lnCount, LANG_APAPRIN_lcTGenIns, "")
    *  ENDFOR
    *ENDIF  


    SELECT (loFormSet.lc_Invoices)
    *SET RELATION TO 'I' + cVendCode + cInvNo INTO APINVAHD
    LOCATE 

    *=lfBrowInv() 
    IF RECCOUNT(loFormSet.lc_Invoices) > 0
      lcAprStat = "ENABLE"
      lcGenStat = "DISABLE"
      *LANG_APAPRIN_lcTSlPr   = LANG_APAPRIN_lcTSelect
      llMayGenr = .F.
      *SHOW GETS ENABLE 
      *SHOW GET pbSelect,1 PROMPT LANG_APAPRIN_lcTSelect
      *SHOW GET pbGenerate DISABLE

      loFormSet.Ariaform1.cmdGenerate.Enabled = lcGenStat = "ENABLE"
      loFormSet.Ariaform1.cmdApprove.Enabled = lcAprStat = "ENABLE"
      
      STORE lcAprStat = "ENABLE" TO ;
        loFormSet.Ariaform1.cmdVendor.Enabled ,;
        loFormSet.Ariaform1.cmdInvoices.Enabled ,;
        loFormSet.Ariaform1.cmdSelNon.Enabled ,;
        loFormSet.Ariaform1.cmdSelAll.Enabled ,;
        loFormSet.Ariaform1.cmdInvert.Enabled ,;
        loFormSet.Ariaform1.cmdSelect.Enabled 

      loformset.ariaform1.grdData.RecordSource = loFormSet.lc_Invoices
      
      
    ELSE
      ** Message: " No ð matching the selected invoices criteria."
      ** Choices: "                      ® Ok ¯                  "
      =gfModalGen("TRM04089B00000","DIALOG",'open invoices')
    ENDIF
    *** No selected invoices
  ELSE
    *** Message: " No ð matching the selected invoices criteria."
    *** Choices: "                 ® Ok ¯                       "
    =gfModalGen("TRM04089B00000","DIALOG",'open invoices')
  ENDIF
  ENDWITH 
  
 
  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
ENDIF  

*- End of lfvGenerate.


*!**************************************************************************
*!
*!      Function: lfvSelect
*!
*!**************************************************************************
*
FUNCTION lfvSelect
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT(loFormSet.lc_Invoices)

IF EMPTY(linclude)
  REPLACE linclude WITH .T.
  lcSlPr  = LANG_APAPRIN_lcTUnSelect
  *SHOW GET pbSelect,1 PROMPT LANG_APAPRIN_lcTUnSelect
ELSE
  REPLACE linclude WITH .F.
  lcSlPr  = LANG_APAPRIN_lcTSelect
  *SHOW GET pbSelect,1 PROMPT LANG_APAPRIN_lcTSelect
ENDIF
* Old : SHOW WINDOW (lcBrInvTtl) REFRESH SAME
loFormSet.Ariaform1.grdData.Column1.chkSelect.Refresh()
=lfAfterRowColChange(loFormSet.Ariaform1.grdData)

SELECT (lnSlct)
************************************************************
*! Name      : lfvSelNon
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/01/2012
*! Purpose   : 
************************************************************
FUNCTION lfvSelNon
PARAMETERS loFormSet
lfvSelAll(loFormSet,'')
*- End of lfvSelNon.
*!**************************************************************************
*!
*!      Function: lfvSelAll
*!
*!**************************************************************************
*
FUNCTION lfvSelAll
PARAMETERS loFormSet,lcCheckMark
PRIVATE lnRecNo

LOCAL lnSlct
lnSlct = SELECT(0)
SELECT(loFormSet.lc_Invoices)

lnRecNo = RECNO()
REPLACE ALL linclude WITH !EMPTY(lcCheckMark)
IF lnRecNo <=RECCOUNT()
  GO lnRecNo
ELSE
  GO TOP  
ENDIF  
IF !EMPTY(linclude)
  lcSlPr  = LANG_APAPRIN_lcTUnSelect
  *SHOW GET pbSelect,1 PROMPT LANG_APAPRIN_lcTUnSelect
ELSE
  lcSlPr  = LANG_APAPRIN_lcTSelect
  *SHOW GET pbSelect,1 PROMPT LANG_APAPRIN_lcTSelect
ENDIF
* Old : SHOW WINDOW (lcBrInvTtl) REFRESH SAME
=lfAfterRowColChange(loFormSet.Ariaform1.grdData)

SELECT (lnSlct)

*!**************************************************************************
*!
*!      Function: lfvInvert
*!
*!**************************************************************************
*
FUNCTION lfvInvert
PARAMETERS loFormSet
PRIVATE lnRecNo

LOCAL lnSlct
lnSlct = SELECT(0)
SELECT(loFormSet.lc_Invoices)

lnRecNo = RECNO()
REPLACE ALL linclude WITH !linclude

IF lnRecNo <=RECCOUNT()
  GO lnRecNo
ELSE
  GO TOP  
ENDIF  
IF !EMPTY(linclude)
  lcSlPr  = LANG_APAPRIN_lcTUnSelect
  *SHOW GET pbSelect,1 PROMPT LANG_APAPRIN_lcTUnSelect
ELSE
  lcSlPr  = LANG_APAPRIN_lcTSelect
  *SHOW GET pbSelect,1 PROMPT LANG_APAPRIN_lcTSelect
ENDIF
* Old : SHOW WINDOW (lcBrInvTtl) REFRESH SAME
=lfAfterRowColChange(loFormSet.Ariaform1.grdData)

SELECT (lnSlct)

*!**************************************************************************
*!
*!        Function : lfvApprove
*!
*!**************************************************************************
FUNCTION lfvApprove
PARAMETERS loFormSet

PRIVATE lcCurAlias, lcPrd, lcSetExact, lnAutNxtNo, lnInvNoSiz, lnTotRec,;
        lnCurRec, lnCount

*** Check if there are any selected records for update
*** If there are not, present the following message and return
lcCurALias = ALIAS()
lcSetExact = SET('EXACT')
SET EXACT OFF

lc_Invoices = loFormSet.lc_Invoices

SELECT (loFormSet.lc_Invoices)
SET RELATION TO 'I' + cVendCode + cInvNo INTO APINVAHD
LOCATE 

LOCAL lnRec 
lnRec = RECNO()
LOCATE FOR !EMPTY(linclude)
IF FOUND()
  lnTotRec  = RECCOUNT(loFormSet.lc_Invoices) + 1
  lnCurRec  = 0
  
  *** Check if the current invoice's audit information is changed
  *** since generation. If it is, present the following message and
  *** skip spprovign this invoice,
  *** Message: " Vendor : ð   Invoice : ð has been modified  " 
  ***          " since the beginning of the current session. "
  ***          " The installment for this invoice cannot be  "
  ***          " approved.  You need to regenerate this      "
  ***          " installment.                                "
  *** Choices: "                      ® Ok ¯                 "

  *** Check if the current invoice already has approved amounts,
  *** If it is, present a message enquiring from the user if it is
  *** to be overwritten, or skip approving this invoice,
  *** Message: " Vendor : ð   Invoice : ð has approved amounts.      "
  ***          " Approving the installment for this invoice will     "
  ***          " overwrite its approved amounts.                     "
  *** Choices: " < Overwrite approved amounts > < Skip this invoice >"
**gfObj_Lock
  *** Attempt to record lock the current invoice
  
  ldGenDate = loFormset.Ariaform1.ldGenDate.Text1.Value

  oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
  oProgress.TotalProgress = RECCOUNT('APINVAHD')
  oProgress.lblFirstLabel.CAPTION = LANG_APAPRIN_lcTAprIns
  lnThermNo = 0
  oProgress.SHOW()

  SCAN FOR !EMPTY(linclude)
    lnThermNo = lnThermNo + 1
    oProgress.CurrentProgress(lnThermNo)
    oProgress.lblFirstLabel.CAPTION = LANG_APAPRIN_lcTVendor + cVendCode + ;
                SPACE(3) + LANG_APAPRIN_lcTInvoice + cInvNo
    oProgress.Visible = .F.
    SELECT APINVAHD
    IF (&lc_Invoices..cTimStamp = APINVHDR.cAdd_User + ;
       DTOC(APINVHDR.dAdd_Date) + APINVHDR.cAdd_Time ;
      .OR.;
       gfModalGen("TRM04125B00000", "DIALOG",;
       ALLTRIM(cVendCode) + '|' + ALLTRIM(cInvNo)) < 0);
      .AND.;
      (APINVHDR.nInvAmtAp + APINVHDR.nInvDisAp + APINVHDR.nInvAdjAp +;
      APINVHDR.nInvA1099 = 0 ;
      .OR.;
      gfModalGen("TRM04126B04005", "DIALOG",;
       ALLTRIM(cVendCode) + '|' + ALLTRIM(cInvNo)) = 1);
     .AND. gfRLock("APINVHDR",.T.) 
      oProgress.Visible = .T.
        
      *** Increment thermometer step
      lnCurRec = lnCurRec + 1
      *=gfThermo(lnTotRec, lnCurRec, LANG_APAPRIN_lcTAprIns,;
                LANG_APAPRIN_lcTVendor + cVendCode + ;
                SPACE(3) + LANG_APAPRIN_lcTInvoice + cInvNo)

      REPLACE;
        APINVAHD.dAutNxGen WITH ;
           IIF(APINVAHD.cAutFUnit = 'D',;
               ldGenDate + APINVAHD.nAutFreq,;
               lfGetNextDate(ldGenDate, nAutFreq)),;
        APINVAHD.dAutLGen  WITH ldGenDate,;
        APINVAHD.cAdd_User WITH oAriaApplication.User_ID,;
        APINVAHD.dAdd_Date WITH loFormSet.ldCurDate,;
        APINVAHD.cAdd_Time WITH gfGetTime()

      SELECT APINVHDR  
      REPLACE;
        APINVHDR.nInvAmtAp WITH &lc_Invoices..nInstAmnt,;
        APINVHDR.nInvDisAp WITH &lc_Invoices..nDisAmnt,;
        APINVHDR.nInvA1099 WITH &lc_Invoices..n1099Amnt,;             
        APINVHDR.cBnkCode  WITH APINVAHD.cBnkCode,;
        APINVHDR.cChkAcct  WITH APINVAHD.cChkAcct,;
        APINVHDR.cChkGlAcc WITH APINVAHD.cChkGlAcc,;
        APINVHDR.cAdd_User WITH oAriaApplication.User_ID,;
        APINVHDR.dAdd_Date WITH loFormSet.ldCurDate,;
        APINVHDR.cAdd_Time WITH gfGetTime(),;
        APINVHDR.nInvFAAp  WITH &lc_Invoices..nInstAmnt

    ENDIF
    =gfRLock("APINVHDR",.F.) 
      
  ENDSCAN
 
  *** Close the thermometer if not already closed
  *IF lnCurRec > 0 .AND. lnTotRec > lnCurRec
  *  FOR lnCount = lnCurRec TO lnTotRec
  *    =gfThermo(lnTotRec, lnCount, LANG_APAPRIN_lcTAprIns, "")
  *  ENDFOR
  *ENDIF  
  
  *- Update tables
  IF lnCurRec>0
    =lfFormSavefiles(loFormSet)
  ENDIF 
  
  oProgress = NULL
  SELECT (lc_Invoices)
  SET RELATION TO   

  *** Reset variables and files, to be ready for a new selection
  =lfClearGen(loFormSet,.T.)

ELSE

  *** Message: " There are no ð.                 "
  *** Choices: "              ® Ok ¯             "
  =gfModalGen("TRM04101B00000","DIALOG", LANG_APAPRIN_lcTNoSInv)  
  SELECT (loFormSet.lc_Invoices)
  LOCATE 
  GOTO lnRec
ENDIF

SELECT (loFormSet.lc_Invoices)
SET RELATION TO 
LOCATE 


SET EXACT &lcSetExact
SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)


*!**************************************************************************
*!
*!        Function : lfvVendor
*!
*!**************************************************************************
FUNCTION lfvVendor
*!*	IF .f.
*!*		PRIVATE laRetVal
*!*		DECLARE laRetVal[3]
*!*		laRetVal     = " "
*!*		IF lfGetExt('APVENDOR', 'VENCODE', lcOldVal, llBrowse,  'cVendCode',;
*!*		   @laRetVal,'cVendCode,cVenComp,cPhoneNo')  
*!*		  lcVendComp = laRetVal[2]
*!*		  SHOW GET lcVendComp
*!*		ENDIF  
*!*		llUpdated    = lcVendCode <> lcOldVal
*!*		llBrowse     = .F.
*!*	ENDIF 
PARAMETERS loFormSet
LOCAL lc_Invoices
lc_Invoices = loformset.lc_Invoices
lcCVENDCODE = &lc_Invoices..cVendCode 
oAriaApplication.DoProgram("AWRAPVENDR",'"&lcCVENDCODE"',.F.,"")

*!**************************************************************************
*!
*!      Function: lfvInvOk
*!
*!**************************************************************************
*  Valid function for push button OK in APAPLDBI screen ( Select invoices)
*
FUNCTION lfvInvOk
llInvOk = .T. 
*CLEAR READ  

*!**************************************************************************
*!
*!      Procedure: lpBrowKeyTrap
*!
*!*******************************************************************************
*  Key traps when browsing
*
PROCEDURE x_lpBrowKeyTrap
PARAMETERS lnLastKey

KEYBOARD "{SHIFT+HOME}"  && dummy key press to release menu access

*** Cases that are valid whether the browse has records or not
DO CASE
  CASE lnLastKey = 38    && Alt-L     () All invoices
      rbScope = 1
      SHOW GET rbScope
     =lfvScope()
  CASE lnLastKey = 47    && Alt-V     () Selected invoices...
    rbScope = 2
    SHOW GET rbScope
    =lfvScope()
ENDCASE

*** If there are no records in the browse, we can only generate,
*** hence
IF RECCOUNT(lc_Invoices) = 0
  IF lnLastKey = 34    && Alt-G     < Generate >
      =lfvGenerate()
  ENDIF
ELSE
  DO CASE  
    CASE lnLastKey = 32    && Alt-D     < Vendor... >
      CLEAR TYPEAHEAD
      *ACTIVATE WINDOW (lcAprInCh1)
      =lfvVenSum()
    CASE lnLastKey = 46    && Alt-C     < Invoice... >
      CLEAR TYPEAHEAD
      *ACTIVATE WINDOW (lcAprInCh1)
      =lfvInvSum()
    CASE lnLastKey = 31    && Alt-S     < Select >
      =lfvSelect()
    CASE lnLastKey = 30    && Alt-A     < Select all >
      =lfvSelAll(lcCheck)
    CASE lnLastKey = 49    && Alt-N     < Select none >
      =lfvSelAll(lcUnCheck)
    CASE lnLastKey = 23    && Alt-I     < Invert >
      =lfvInvert()
    CASE lnLastKey = 25    && Alt-P     < Approve >
      =lfvApprove()
  ENDCASE
ENDIF

*!**************************************************************************
*!
*!      Function: lfClearGen
*!
*!**************************************************************************
* Clears previously generated installments
FUNCTION lfClearGen
PARAMETERS loFormSet,llFromScpBt
PRIVATE lcCurAlias

lc_Invoices = loFormSet.lc_Invoices

lcCurAlias = ALIAS()
SELECT (lc_Invoices)
ZAP
*=lfBrowInv()
lcGenStat  = 'ENABLE'
lcAprStat  = 'DISABLE'
llMayGenr  = .T.
IF !llFromScpBt
  STORE 1 TO rbScope, lnOldrbScp
  *** Reset values in the selection screen
  lcVendCode = SPACE(8)
  lcInvRef   = SPACE(15)
  STORE " " TO lcDivOpt, lcPayPrior, lcPayMeth,;
               lcPMethOpt, lcVendComp  
  lcPayMeth  = LANG_APAPRIN_lcTAll
  lcDivision = LANG_APAPRIN_lcTAll                 

  *E300643,1 Change this line for the changes we have made to 
  *          SYCCODES [Begin]
  *GO TOP IN SYCCODES             
  GO TOP IN CODES             
  *E300643,1 Change this line [End]
  
  rbDates     = 1
ENDIF
*=lfShowInv(lcAprStat)

*!*	*** CLose chile windows if visible
*!*	IF WVISIBLE(lcVenSum)      && close vendor window if it's opened.
*!*	  =gfChClose(lcVenSum)
*!*	ENDIF  
*!*	IF WVISIBLE(lcInvSum)      && close invoice window if it's opened.
*!*	  =gfChClose(lcInvSum)
*!*	ENDIF  

loFormSet.Ariaform1.cmdGenerate.Enabled = lcGenStat = "ENABLE"
loFormSet.Ariaform1.cmdApprove.Enabled = lcAprStat = "ENABLE"

SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)


*!**************************************************************************
*!
*!      Function: lfVldNextDate
*!
*!**************************************************************************
*
FUNCTION lfVldNextDate
PARAMETERS ldNxtDate
PRIVATE llVldDate
llVldDate = .F.

*** Check if the date lies within the posting window. 
*** If it does not, present the following message and return .f.
*** Message: " The next installment date ð for Vendor ð Invoice ð "
***          " is out of the posting window.  You should correct  "
***          " the next installment date for this invoice.        "
*** Choices: "                          ® Ok ¯                    "
llVldDate = ! ((!lfVlDate(oAriaApplication.PrntCompanyID,' ',' ', ldNxtDate);
             .AND. gfModalGen('TRM04076B00000', 'DIALOG', ;
                   DTOC(ldNxtDate) + '|' + ALLTRIM(cVendCode) +;
                   '|' +ALLTRIM(cInvNo)) > 0))  

RETURN llVldDate
  

*!**************************************************************************
*!
*!      Function: lfGetNoOfInst
*!
*!**************************************************************************
*
FUNCTION lfGetNoOfInst
PRIVATE lcCurAlias, lnCountPrds, ldNextDate

lcCurAlias  = ALIAS()
lnCountPrds = 0
ldNextDate  = dAutNxGen

*E300692,1 CHANGE FILE NAME FROM SYCFSPRO TO FSPRO
*SELECT SYCFSPRD

SELECT FSPRD
GO TOP
IF !EOF()
*E300789,4 IHB [end]

  *E300692,1 CHANGE FILE NAME FROM SYCFSPRO TO FSPRO
  *LOCATE REST FOR BETWEEN(ldNextDate, SYCFSPRD.dFspPBgDt, SYCFSPRD.dFspPEnDt)
  LOCATE REST FOR BETWEEN(ldNextDate, FSPRD.dFspPBgDt, FSPRD.dFspPEnDt)
  *E300692,1 end
  IF FOUND()
    DO WHILE ldNextDate <= ldGenDate
    *E300789,4 IHB [end]
            
  *E300692,1 end                    
    *B600624,1 end.
      *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
      *ldNextDate  = ldNextDate  + ;
                   (SYCFSPRD.dFspPEnDt - SYCFSPRD.dFspPBgDt) + 1
      ldNextDate  = ldNextDate  + ;
                    (FSPRD.dFspPEnDt - FSPRD.dFspPBgDt) + 1                   
      *E300692,1 end            
                   
      lnCountPrds = lnCountPrds + 1
      *B600650,1 (START) After moving the next period make sure that the next date falls in it
      SKIP 1
      IF !BETWEEN(ldNextDate, dFspPBgDt, dFspPEnDt)
        ldNextDate =   dFspPEnDt
      ENDIF
    ENDDO
*    ENDSCAN          
    *B600650,1 (END)
  ENDIF  
ENDIF

SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)

RETURN lnCountPrds

*!**************************************************************************
*!
*!      Function: lfGetNextDate
*!
*!**************************************************************************
FUNCTION lfGetNextDate
PARAMETERS ldNxtDate, lnNumOfPrds
PRIVATE lcCurAlias, lnCountDays, lnCountPrds

lcCurAlias  = ALIAS()
STORE 0 TO lnCountDays, lnCountPrds
*E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
*SELECT SYCFSPRD

SELECT FSPRD
GO TOP
IF !EOF()
*E300789,4 IHB [end]

  *B600650,1 Locate for the right period
  LOCATE REST FOR BETWEEN(ldNxtDate, dFspPBgDt, dFspPEnDt)
    lnCur_Pdr = VAL(FSPRD.cFspPrdID)

    SCAN REST WHILE lnCountPrds < lnNumOfPrds
    *E300789,4 IHB [end]
              
    lnCountDays = lnCountDays  + ;
                (FSPRD.dFspPEnDt - FSPRD.dFspPBgDt) + 1
    *E300692,1 end                
    lnCountPrds = lnCountPrds + 1
  ENDSCAN            
ENDIF

ldNxtDate = IIF(lnCountPrds = lnNumOfPrds, ;
                ldNxtDate + lnCountDays, {})

*B600650,1 (START) Make sure that we did not jump a period while calculating the next date
= SEEK(oAriaApplication.PrntCompanyID)
LOCATE REST FOR BETWEEN(ldNxtDate, dFspPBgDt, dFspPEnDt)
*E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
*DO WHILE lnCur_Pdr + lnNumOfPrds < VAL(SYCFSPRD.cFspPrdID)
DO WHILE lnCur_Pdr + lnNumOfPrds < VAL(FSPRD.cFspPrdID)
*E300692,1 end
  SKIP -1
  ldNxtDate =   dFspPEnDt
ENDDO
*B600650,1  (END)

SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
              
RETURN ldNxtDate   


*!**************************************************************************
*!
*!      Function: lfGetExt
*!
*!**************************************************************************
* Valid function for get field laData[43] representing factor
*
FUNCTION lfGetExt
PARAMETERS lcSrchFile, lcSrchTag, lcOldVal, llBrowse,;
           lcFldName, laTemp, lcSrchFlds, lcDataDir 
*** Old factor value
*** .T. if browsed from browsing invisible button
PRIVATE lcSrchVal, lcSrchObj, lcCurAlias, lcSavBrFld, lcSavTitle,;
        lcTempSrch, lnClosRec, llSrchFound

lcSrchObj  = SYS(18)
lcSrchVal  = ALLTRIM(EVALUATE(lcSrchObj))

llSrchFound  = .T.
IF llBrowse .OR. lcSrchVal <> lcOldVal 
  laTemp     = ' '
  IF llBrowse .OR. !EMPTY(lcSrchVal)
    lcSrchFlds = IIF(EMPTY(lcSrchFlds), " ", STRTRAN(lcSrchFlds,' '))
    *lcDataDir  = IIF(EMPTY(lcDataDir), gcDataDir, lcDataDir)
    lcCurAlias = ALIAS()

    lcTempSrch = gfTempName()
    SELECT 0
    USE (oAriaApplication.DataDir+lcSrchFile) ORDER TAG (lcSrchTag) AGAIN ALIAS (lcTempSrch)

    lcSrchVal  = PADR(lcSrchVal, FSIZE(lcFldName, lcTempSrch))
    &lcSrchObj = lcSrchVal
    *SHOW GET (lcSrchObj)

    IF llBrowse .OR. !SEEK(lcSrchVal, lcTempSrch)
      *** If a record is to be selected 
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      lnClosRec  = RECNO(0)
    
      *** Get browse fields from dictionary
      =lfGetBrF(@lcFile_Ttl, @lcBrFields, lcSrchFile,lcSrchFlds)
      IF BETWEEN(lnClosRec,1,RECCOUNT())
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF        

      =gfBrows(.F.,lcSrchFlds,'laTemp')
         
      lcBrFields=lcSavBrFld
      lcFile_Ttl=lcSavTitle
    
      *** If a selection occurs
      IF !EMPTY(laTemp[1])
        &lcSrchObj  = laTemp[1]
        llSrchFound = .T.      
      ELSE
        &lcSrchObj  = lcOldVal
        llSrchFound = .F.       
      ENDIF
      *SHOW GET (lcSrchObj)
    ELSE
      =gfSubStr(lcSrchFlds,@laTemp,",")
      FOR lnCount = 1 TO ALEN(laTemp)
        laTemp[lnCount] = &laTemp[lnCount]
      ENDFOR  
      llSrchFound = .T.         
    ENDIF
    IF USED(lcTempSrch)
      USE IN (lcTempSrch)
    ENDIF
    SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
  ENDIF
ENDIF
RETURN llSrchFound


************************************************************************************
*Name      : lfGetCrVal
*Developer :*E303014,1 TMI 
*Date      :01/05/2012 [Start] 
*Purpose   :Get the criteria value from the filter arrays
************************************************************************************
FUNCTION lfGetCrVal
PARAMETERS lcFltFld
LOCAL lnPos
lnPos = ASCAN(laOgFxFLt,lcFltFld)
IF lnPos > 0
  lnPOS = ASUBSCRIPT(laOgFxFLt,lnPos,1)
  RETURN laOgFxFLt[lnPos,6]
ENDIF
lnPos = ASCAN(laOgVrFLt,lcFltFld)
IF lnPos > 0
  lnPOS = ASUBSCRIPT(laOgVrFLt,lnPos,1)
  RETURN laOgVrFLt[lnPos,6]
ENDIF
*- End of lfGetCrVal.

*E303016,1 TMI 01/24/2012 [Start] add this function here just until a separate task is created for it
*!*************************************************************
*! Name      : gfObj_Lock
*! Developer : Hesham El_Sheltawi
*! Date      : 08/14/2002
*! Purpose   : Function to logicaly lock a record
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  Lock or unlock
*!                       
*!*************************************************************
*! Returns            :  .T. --> succeded
*!                       .F. --> unsuccess
*!*************************************************************
*! Example            :  gfObj_lock(.T.,.T.)
*!*************************************************************
FUNCTION gfObj_Lock
*E303016,1 TMI 01/24/2012 [Start] do not allow the same user to overwrite the same session
**                                that is, if llDenyOvwrt is .T. then deny overwrite
**                                         if llDenyOvwrt is .F. then allow overwrite    ( as defaul )
*PARAMETERS lLok_Set
PARAMETERS lLok_Set,llDenyOvwrt
*E303016,1 TMI 01/24/2012 [End  ] 
PRIVATE lnRecNo,lRet_Flag
lnWorkArea = ALIAS()
IF EMPTY(lnWorkArea)
  RETURN
ENDIF
PRIVATE lnOldrpSt
SELECT (lnWorkArea)
lnDataSession = SET("DATASESSION")
lnAlias = SELECT()
lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

DO WHILE .T.
  SELECT (lnWorkArea)
  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
   llLocked = RLOCK() 
   UNLOCK RECORD lnRecNo
   IF llLocked
     TABLEREVERT(.F.)
   ENDIF 
   IF DELETED()
     =gfModalGen('INM00095B00000','ALERT')
     SELECT (lnAlias)
     *THIS.CHangemode("S")
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
      lcLok_User = cLok_User
      IF !EMPTY(lcLok_User)
        IF ALLTRIM(lcLok_User) = ALLTRIM(oAriaApplication.User_ID)
          * Messaging the user that he cannot edit the same record
          * from more than one session and permit him from editing
          * the same record
          *E303016,1 TMI 01/24/2012 [Start] 
          IF llDenyOvwrt 
            =gfModalGen("INM00028B00000","ALERT",lcLok_User)
            STORE .F. TO lLok_It, lRet_Flag
          ELSE 
            *E303016,1 TMI 01/24/2012 [End  ] 
            IF gfModalGen("INM00240B00006","ALERT")=2  
              lLok_It    = .F.
              lRet_Flag  = .F.
            ELSE      
              lLok_It    = .T.
            ENDIF
            *E303016,1 TMI 01/24/2012 [Start] 
          ENDIF 
          *E303016,1 TMI 01/24/2012 [End  ] 
        ELSE

          *We save old value of reprocess first.[START]
          lnOldrpSt = SET('REPROCESS')
          SET REPROCESS TO 1
          
					SET DATASESSION TO 1
					llLoop = .F.
					SELECT syuStatc
          IF SEEK ('INI'+'OLDVARS'+lcLok_User,'syuStatc') 
            LOCAL lnStatcRec
            SCAN REST WHILE cobj_typ+ALLTRIM(cobj_name)+cuser_id = 'INI'+'OLDVARS'+lcLok_User
                lnStatcRec = RECNO()
	            IF RLOCK('syuStatc')
  	            UNLOCK RECORD lnStatcRec IN  syuStatc 
        	      lLok_It    = .T.  	            
*!*	  	            lnStatcRec = RECNO()
*!*	    	          GO (oAriaApplication.UserStaticRecord) IN syuStatc 
*!*	      	        =RLOCK('syuStatc')
*!*	        	      GO lnStatcRec
          	  ELSE
            	  UNLOCK
              	 GO (oAriaApplication.UserStaticRecord) IN syuStatc 
              	=RLOCK('syuStatc') 
              	*** Display the message "Record is in use by user AAAA"
              	lcLok_User = oAriaApplication.getUserName(lcLok_User)
              	*** Record is in use by user ????    
              	SET DATASESSION TO (lnDataSession)
              	IF  gfModalGen("INM00028B00015","ALERT",lcLok_User) = 1
                	llLoop = .T.
              	ENDIF  
              	lLok_It    = .F.
              	lRet_Flag  = .F.
              	EXIT 
            	ENDIF
           ENDSCAN 	
          ELSE
            lLok_It    = .T. 
          ENDIF
          * Return the old value of reprocess.
          SET REPROCESS TO  lnOldrpSt
					SET DATASESSION TO (lnDataSession)
          IF llLoop
            LOOP 
          ENDIF 

        ENDIF
      ELSE
        *** Display the message "Record is in use by another"
        SET DATASESSION TO (lnDataSession)
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
			=TABLEUPDATE(0,.T.)              
      lRet_Flag  = .T.
    ENDIF  
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
SET DATASESSION TO (lnDataSession)
IF lLok_It  
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
     TYPE ('dLok_Date') <> "U" .AND. ;
     TYPE ('cLok_Time') <> "U" 
    *** Lock the record for this user with date and time
    REPLACE lLok_Stat WITH .T.       , ;   
             cLok_User WITH oAriaApplication.User_ID , ;
             dLok_Date WITH DATE()    , ;
             cLok_Time WITH gfGetTime()
    =TABLEUPDATE(0,.T.)
    lRet_Flag  = .T.    
  ENDIF
ENDIF
SELECT (lnWorkArea)
UNLOCK
SELECT (lnAlias)

RETURN lRet_Flag
