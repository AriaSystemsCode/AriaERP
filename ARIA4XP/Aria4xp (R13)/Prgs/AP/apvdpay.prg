***********************************************************************
*:  Program File: APVDPAY.PRG
*:  Desc.       : Void Payment screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 01/11/2012
*:  Reference   : E303016,1
*:************************************************************************
* 20100922.0013 check this ticket for the global resolution of calling screens
* after finish , change the way that gfObj_lock works by adding a new parameter to the function such that if .T. then
*if the same user is editing the same record in another session then do not do anything, not like the current case that shows a
*a question "You are editing the same record in another session , overwrite ?<YES><NO>

* but for the current problem, one can save the session id in the owner field, and always check the session #, as one
* invoice may be paid more than once
*:************************************************************************
*Modifications:
*B610031,1 MMT 07/30/2012 Error in void payment screen after opening it after appl debit screen[T20120724.0036]
*B610273,1 HIA 03/18/2013 Aria XP - voiding payment [T20130220.0007]
*B610660,1 TMI 01/22/2014 Modify lfFormInit to change variable lcTNpPay value to be LANG variable with Globalization expression [ T20140121.0039 task] 
*E303442,1 TMI 02/19/2014 Add a new setup to APSETUP table to allow/prevent voiding payments in different periods [T20140214.0007] * I created a differenct fix for R13 E303445
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012]
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022]
*B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [T20180509.0007]
*B611705,1 SAH 1/2/2019   modify apvdpay.prg as Cash Payments cannot be Voided from Void payment screen [T20181203.0001]
*B611768,1 DM 4/21/2019 modify to solve issue: Unable to void payment for Rogers dated March 6, 2019 [T20190418.0008]
*:************************************************************************
#INCLUDE R:\aria4xp\PRGS\ap\apvdpay.H
&& apply the calling from client folder
LOCAL lcRunScx,lcScx
lcScx = "AP\APVDPAY.SCX"
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientscreenhome+lcScx)
  lcRunScx = oAriaApplication.clientscreenhome+lcScx
ELSE
  lcRunScx = oAriaApplication.screenhome+lcScx
ENDIF
DO FORM (lcRunScx)

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : E303011,1 TMI 12/13/2011
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

SET MULTILOCKS ON
*- Open tables
=gfOpenTable('APPAYMNT','TYPMETHDOC','SH')
=gfOpenTable('APINVHDR','INVVEND','SH')
=gfOpenTable('APVENDOR','VENCODE','SH')
=gfOpenTable('APVENHST','VENDYEAR','SH')
=gfOpenTable('APDIST','PAYMNTS','SH')   && CINVNO+CVENDCODE+CAPDTRTYP
=gfOpenTable('APCHECKS','BANKCHECK','SH')
=gfOpenTable('APDIV','DIVISION','SH')
=gfOpenTable('CODES','IDRLTFNAME','SH')
=gfOpenTable('APBANKS','BANKCODE','SH')
*=gfOpenTable('SYCCURR','CCURRCODE','SH')

=gfOpenTable('APSETUP','APSETUP','SH')

*- check if there is any payments

*B611705,1 SAH 1/2/2019  modify apvdpay.prg as Cash Payments cannot be Voided from Void payment screen [start]
*lcMainFilt = "APPAYMNT.CPAYTYPE = 'P' .AND. APPAYMNT.CPAYMETH $ 'PMN' .AND. APPAYMNT.CPAYSTAT <> 'V' .AND. APPAYMNT.CPAYRECST <> 'C'"
lcMainFilt = "APPAYMNT.CPAYTYPE = 'P' .AND. APPAYMNT.CPAYMETH $ 'PMNH' .AND. APPAYMNT.CPAYSTAT <> 'V' .AND. APPAYMNT.CPAYRECST <> 'C'"
*B611705,1 SAH 1/2/2019  modify apvdpay.prg as Cash Payments cannot be Voided from Void payment screen [end]

=lfAddProp(loFormSet,'lcMainFilt',lcMainFilt)
SELECT APPAYMNT
LOCATE
LOCATE FOR &lcMainFilt
IF EOF()
  *B610660,1 TMI 01/22/2014 20:38 [Start] replace lcTNpPay with LANG_lcTNpPay
  *lcTNpPay = 'payments to void'
  lcTNpPay = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_lcTNpPay,loFormSet.GetHeaderText("LANG_lcTNpPay",loFormSet.HeaderAlias))  
  *B610660,1 TMI 01/22/2014 20:39 [End  ] 
  =gfModalGen("TRM04101B00000","DIALOG",lcTNpPay)
  RETURN .F.
ENDIF

** Collect the PAYMENT file fields in an array to create the **
** the Temp. invoice file.                                   **
SELECT APINVHDR
=AFIELDS(laFileStru)
** Add 5 more fiilds to the invoice file structure. **

lnFileStru = ALEN(laFileStru,1)

DIMENSION laFileStru[lnFileStru+5,18]

laFileStru[lnFileStru+1,1] = 'N1099'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 15
laFileStru[lnFileStru+1,4] = 2

laFileStru[lnFileStru+2,1] = 'NPAID'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 15
laFileStru[lnFileStru+2,4] = 2

laFileStru[lnFileStru+3,1] = 'NADJUST'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 15
laFileStru[lnFileStru+3,4] = 2

laFileStru[lnFileStru+4,1] = 'NDISCOUNT'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 10
laFileStru[lnFileStru+4,4] = 2

laFileStru[lnFileStru+5,1] = 'NTOTAL'
laFileStru[lnFileStru+5,2] = 'N'
laFileStru[lnFileStru+5,3] = 15
laFileStru[lnFileStru+5,4] = 2

*- update other array fields
=lfUpdStruArr(@laFileStru,lnFileStru)

*** Creat temp  file from the array.
=lfAddProp(loFormSet,'lcTempInv',gfTempName())
lcTempInv = loFormSet.lcTempInv
CREATE TABLE (oAriaApplication.WorkDir+lcTempInv) FROM ARRAY laFileStru

SELECT APDIST
SELECT 0
=lfAddProp(loFormSet,'lcApDist',gfTempName())
lcApDist = loFormSet.lcApDist
USE (oAriaApplication.DataDir+'APDIST') AGAIN ALIAS (lcApDist)

SELECT APVENDOR
SET ORDER TO TAG VENCODE

SELECT APVENHST
SET ORDER TO TAG VENDYEAR

SELECT APDIST
SET ORDER TO TAG PAYMNTS

SELECT APINVHDR
SET ORDER TO TAG INVVEND

SELECT (lcApDist)
SET ORDER TO TAG PAYMNTS

SELECT APPAYMNT
SET RELATION TO APPAYMNT.CPAYCLNO INTO APVENDOR ADDITIVE
SET RELATION TO APPAYMNT.CPAYCLNO+APPAYMNT.CFISFYEAR INTO APVENHST ADDITIVE
SET RELATION TO APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO INTO APDIST ADDITIVE

SELECT APDIST
SET RELATION TO APDIST.CINVNO+APDIST.CVENDCODE INTO APINVHDR ADDITIVE

*- initializations
WITH loFormSet
  .nWorkArea = 'APPAYMNT'
  .otoolbar.nWorkArea = 'APPAYMNT'
  .DataEnvironment.InitialSelectedAlias = 'APPAYMNT'
ENDWITH
=lfAddProp(loFormSet,'lcSequence',gfSequence('CAPSESSNO'))
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*loFormSet.Ariaform1.Caption = ALLTRIM(loFormSet.Ariaform1.Caption)+'      Session: '+loFormSet.lcSequence
loFormSet.Ariaform1.Caption = ALLTRIM(loFormSet.Ariaform1.Caption)+'      '+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_SESSION,loFormSet.GetHeaderText("LANG_APVDPAY_SESSION",loFormSet.HeaderAlias))+' '+loFormSet.lcSequence
*N000682,1 MMT 12/09/2012 Globalization changes[END]

*- Define variables
=lfDefineVars(loFormset)

*-- Define custom tool bar buttons
DECLARE loFormSet.lapanelobj[1,6]
STORE '' TO loFormSet.lapanelobj
*-- Scope Button
loFormSet.laPanelObj[1,1] = 'pbScop'
loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"SCOPE.BMP"
loFormSet.laPanelObj[1,3] = 'lfGetData'    && the loFormSet is sent by default as a parameter
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*	loFormSet.laPanelObj[1,4] = "Option Grid"
*!*	loFormSet.laPanelObj[1,5] = "Option Grid"
loFormSet.laPanelObj[1,4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_OPTIONGRID,loFormSet.GetHeaderText("LANG_APVDPAY_OPTIONGRID",loFormSet.HeaderAlias))
loFormSet.laPanelObj[1,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_OPTIONGRID,loFormSet.GetHeaderText("LANG_APVDPAY_OPTIONGRID",loFormSet.HeaderAlias))
*N000682,1 MMT 12/09/2012 Globalization changes[END]
*loFormSet.laPanelObj[1,6] = 'V'
loFormSet.laPanelObj[1,6] = 'S'

*- Create temp files
=lfCrTmpFls()

*- update the grid column's sources
loFormSet.ariaform1.grdAPPAYMNT.RecordSource = ''
=lfGetGridColmns(loFormSet)

*- Add flag lf1stRun
=lfAddProp(loFormSet,'lf1stRun',.T.)

*- fill up the temp file and collect data
IF ! loFormSet.lf1stRun
  =lfGetData(loFormset)
ENDIF
*- End of lfFormInit

********************************************************************************
*Name       : lfGetGridColmns
*Developer  : TMI
*Date       : 01/26/2012
*Purpose    : update the grid column's sources
********************************************************************************
FUNCTION lfGetGridColmns
PARAMETERS loFormSet

LOCAL lcAPPAYMNT
WITH loFormSet.ariaform1.grdAPPAYMNT
  lcAPPAYMNT = loFormSet.lcAPPAYMNT
  LOCAL lnI,lcI
  lnI = 0
  oGrid = loFormSet.ariaform1.grdAPPAYMNT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=lfAddColumn(@lnI,oGrid,'&lcAPPAYMNT..CPAYDOCNO'    ,LANG_APVDPAY_CPAYDOCNO    ,75 )
  =lfAddColumn(@lnI,oGrid,'&lcAPPAYMNT..CPAYDOCNO'    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_CPAYDOCNO,loFormSet.GetHeaderText("LANG_APVDPAY_CPAYDOCNO",loFormSet.HeaderAlias))    ,75 )
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=lfAddColumn(@lnI,oGrid,'IIF(&lcAPPAYMNT..LPAYADVAN,"Yes","No")',LANG_APVDPAY_ADVANCED , 30)
  =lfAddColumn(@lnI,oGrid,"IIF(&lcAPPAYMNT..LPAYADVAN,'"+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_YES,loFormSet.GetHeaderText("LANG_APVDPAY_YES",loFormSet.HeaderAlias))+"','"+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_NO,loFormSet.GetHeaderText("LANG_APVDPAY_NO",loFormSet.HeaderAlias))+"')",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_ADVANCED,loFormSet.GetHeaderText("LANG_APVDPAY_ADVANCED",loFormSet.HeaderAlias)) , 30)
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI,oGrid,'lfGetPayMeth(Thisformset)',LANG_APVDPAY_Pay_Meth      ,100 )  &&" Pay.  Meth  "
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[Start]
*=lfAddColumn(@lnI,oGrid,'lfGetPayMeth(Thisformset)',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_Pay_Meth,loFormSet.GetHeaderText("LANG_APVDPAY_Pay_Meth",loFormSet.HeaderAlias))      ,100 )  &&" Pay.  Meth  "
=lfAddColumn(@lnI,oGrid,'Thisformset.lfGetVdPayMeth()',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_Pay_Meth,loFormSet.GetHeaderText("LANG_APVDPAY_Pay_Meth",loFormSet.HeaderAlias))      ,100 )  &&" Pay.  Meth  "
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[End]
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI,oGrid,'&lcAPPAYMNT..DPAYDATE'    ,LANG_APVDPAY_PAYMENT_DATE  ,80)   &&" Payment Date"
=lfAddColumn(@lnI,oGrid,'&lcAPPAYMNT..DPAYDATE'    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_PAYMENT_DATE,loFormSet.GetHeaderText("LANG_APVDPAY_PAYMENT_DATE",loFormSet.HeaderAlias))  ,80)   &&" Payment Date"
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI,oGrid,'&lcAPPAYMNT..NPAYAMNT'    ,LANG_APVDPAY_PAYMENT_AMOUNT,100 )  &&" Payment AMOUNT"
=lfAddColumn(@lnI,oGrid,'&lcAPPAYMNT..NPAYAMNT'    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_PAYMENT_AMOUNT,loFormSet.GetHeaderText("LANG_APVDPAY_PAYMENT_AMOUNT",loFormSet.HeaderAlias)),100 )  &&" Payment AMOUNT"
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI,oGrid,'&lcAPPAYMNT..CPAYCLNO'    ,LANG_APVDPAY_Vendor        ,80 )  &&" Vendor
=lfAddColumn(@lnI,oGrid,'&lcAPPAYMNT..CPAYCLNO'    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_Vendor,loFormSet.GetHeaderText("LANG_APVDPAY_Vendor",loFormSet.HeaderAlias))        ,80 )  &&" Vendor
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI,oGrid,'&lcAPPAYMNT..CPAYCOMP'    ,LANG_APVDPAY_Company       ,200)  &&" Company
=lfAddColumn(@lnI,oGrid,'&lcAPPAYMNT..CPAYCOMP'    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_Company,loFormSet.GetHeaderText("LANG_APVDPAY_Company",loFormSet.HeaderAlias))       ,200)  &&" Company
*N000682,1 11/20/2012 MMT Globlization changes[End]


  .READONLY = .T.
  .Refresh()
ENDWITH
*- End of lfGetGridColmns.

******************************************************************************
*Name      : lfAfterRowColChange
*Developer : TMI - Tarek Mohammed Ibrahim
*Date      : 1/3/2012
*Purpose   : After Row Col Change of the Grid
******************************************************************************
FUNCTION lfAfterRowColChange
PARAMETERS loGrid

*- End of lfAfterRowColChange.

*!*************************************************************
*! Name      : lfAddColumn
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303014,1 TMI 01/03/2011
*! Purpose   : A function to add columns to the passed grid object
*!*************************************************************
FUNCTION lfAddColumn
LPARAMETERS lnI,oGrid,lcFld,lcTitle,lnWidth
lnWidth = IIF(EMPTY(lnWidth),75,lnWidth)
LOCAL lcI
lnI = lnI + 1
lcI = ALLTRIM(STR(lnI))
WITH oGrid
  .Column&lcI..ControlSource    = ALLTRIM(lcFld)
  .Column&lcI..Header1.Caption  = lcTitle
  .Column&lcI..Width            = lnWidth
  .Column&lcI..Readonly         = .T.
ENDWITH
*- End of lfAddColumn.

************************************************************************************************
*Name       : lfUpdStruArr
*Developer  : TMI
*Date       : 01/24/2012
*Purpose    : Update the rest columnas  of a newely added fields to a structure array
************************************************
FUNCTION lfUpdStruArr
PARAMETERS laFileStru,lnFileStru
LOCAL lnI,lnJ
FOR lnI = lnFileStru+1 TO ALEN(laFileStru,1)
  STORE .F. TO laFileStru[lnI,5],laFileStru[lnI,6]
  FOR lnJ = 7 TO 16
    laFileStru[lnI,lnJ] = ""
  ENDFOR
  STORE 0 TO laFileStru[lnI,17],laFileStru[lnI,18]
ENDFOR
*- End of lfUpdStruArr.

************************************************************************************
*Name       : lfFormActivate
*Developer  : TMI
*Date       : 01/08/2012
*Purpose    : Run lfGetData in the activate method for the first time the screen runs
************************************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- Add flag lf1stRun in case that sometimes the formset does not recognized that this property was added
=lfAddProp(loFormSet,'lf1stRun',.T.)
IF loFormSet.lf1stRun
  loFormSet.lf1stRun = .F.
  =lfGetData(loFormset)
ELSE
  *- refresh the grid
  loFormset.Ariaform1.grdAPPAYMNT.SetFocus()
  loFormset.Ariaform1.grdAPPAYMNT.Refresh()
ENDIF
 *- End of lfFormActivate.

********************************************************************************
*Name      : lfCrTmpFls
*Developer : TMI
*Date      : 01/06/2012
*Purpose   : Create the temp table that holds the invoices that are ready to be approved
********************************************************************************
FUNCTION lfCrTmpFls
LOCAL laStru
=lfAddProp(loFormSet,'lcAPPAYMNT',gfTempName())
SELECT APPAYMNT
DIMENSION laStru[1,18]
=AFIELDS(laStru)

lnStruLen = ALEN(laStru,1)
DIMENSION laStru[lnStruLen+2,18]
laStru[lnStruLen+1,1] = 'LVOIDED'
laStru[lnStruLen+1,2] = 'L'
laStru[lnStruLen+1,3] = 1
laStru[lnStruLen+1,4] = 0

laStru[lnStruLen+2,1] = 'MINVLK'  && APINVHDR locked records
laStru[lnStruLen+2,2] = 'M'
laStru[lnStruLen+2,3] = 10
laStru[lnStruLen+2,4] = 0

*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
lnPayDocPos = ASCAN(laStru,'CPAYDOCNO',1)
IF lnPayDocPos > 0
  lnPayDocPos = ASUBSCRIPT(laStru, lnPayDocPos , 1)
  laStru[lnPayDocPos,3]=12
ENDIF
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]

*- update other array fields
=lfUpdStruArr(@laStru,lnStruLen)

DIMENSION laIndex[1,2]
laIndex[1,1] = 'CPAYTYPE+CPAYMETH+CPAYDOCNO+CBNKCODE+CCHKACCT'
laIndex[1,2] = 'TYPMETHDOC'
=gfCrtTmp(loFormSet.lcAPPAYMNT,@laStru,@laIndex,loFormSet.lcAPPAYMNT)
RETURN

*<> try to put this function in a global program to not rewrite it many times
*!*************************************************************
*! Name      : lfAddProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303011,1 TMI 12/17/2011
*! Purpose   : A function to add properties to the object that passed as a parameter
*!*************************************************************
FUNCTION lfAddProp
PARAMETERS loObj,lcPropName,PropValue
*- check if lcPropname is not just a variable, but a list of variables, then create a loop to walk around
LOCAL lnI,lnLen,lcPropToCreate
lcPropName = lcPropName + ','
lnLen = OCCURS(',',lcPropName)
FOR lnI = 1 TO lnLen
  lcPropToCreate = ALLTRIM(SUBSTR(lcPropName,1,AT(',',lcPropName)-1))
  IF TYPE('loObj.&lcPropToCreate')='U'
    loObj.AddProperty(lcPropToCreate,PropValue)
  ENDIF
  lcPropName = SUBSTR(lcPropName,AT(',',lcPropName)+1)
ENDFOR

*<> try to put this function in a global program to not rewrite it many times
************************************************************************************************
* Name         : lfPopPropArray
* Developer    : tmi - Tarek Mohamed Ibrahim
* Date         : 12/21/2011
* Purpose      : Put all variables in a two diminsional array, one column for the variable name , the other for its initial value.
*-             : In the start of each function that changes the APINVHDR file initialize all the variables.
************************************************************************************************
FUNCTION lfPopPropArray
LPARAMETERS loObj,lcArrPropName,lcVarsString,InitVal
LOCAL lnLen,lnOldLen,lnI,lnCommaPos

lcVarsString = lcVarsString+','
lnLen = OCCURS(',',lcVarsString)
lnOldLen = ALEN(loObj.&lcArrPropName,1)
DIMENSION loObj.&lcArrPropName.[lnOldLen+lnLen,2]
FOR lnI = 1 TO lnLen
  lnCommaPos = AT(',',lcVarsString)
  loObj.&lcArrPropName.[lnOldLen+lnI,1] = ALLTRIM(SUBSTR(lcVarsString,1,lnCommaPos-1))
  loObj.&lcArrPropName.[lnOldLen+lnI,2] = InitVal
  lcVarsString = SUBSTR(lcVarsString,lnCommaPos+1)
ENDFOR
*- End of lfPopPropArray.

***********************************************************************************
*Name    : lfGetData
*Developer : TMI - Tarek Mohamed Inbrahim
*Date    : 12/24/2011
*Purpose : called from the AfterRowColChange event in the grdApprove on the screen
***********************************************************************************
FUNCTION lfGetData
PARAMETERS loFormSet

loFormSet.ariaform1.grdAPPAYMNT.RecordSource = ''

*-Clear data from temp files
SELECT (loFormSet.lcAPPAYMNT)
ZAP
*B610031,1 MMT 07/30/2012 Error in void payment screen after opening it after appl debit screen[Start]
LOCAL lcSetProcD
lcSetProcD = SET("Procedure")
*B610031,1 MMT 07/30/2012 Error in void payment screen after opening it after appl debit screen[End]
*- Call the "APVDPAY" OG
LOCAL lcExpr
lcExpr = gfOpGrid('APVDPAY',.T. ,.F. ,.F. ,.T. ,.T.)
*B610031,1 MMT 07/30/2012 Error in void payment screen after opening it after appl debit screen[Start]
SET PROCEDURE TO &lcSetProcD.
*B610031,1 MMT 07/30/2012 Error in void payment screen after opening it after appl debit screen[End]
*- if cancel clicked , go to Select Mode
IF lcExpr == '.F.'
  loFormSet.ChangeMode('S')
  RETURN
ENDIF
SELECT APPAYMNT
* if only one vendor was selected use the index  "TYPCLNO"   && CPAYTYPE+CPAYCLNO
* Else use the index "TYPMETHDOC"   && CPAYTYPE+CPAYMETH+CPAYDOCNO+CBNKCODE+CCHKACCT

*- fill the temp file with the data
=SEEK("P",'APPAYMNT')
lcKey = KEY()
SCAN REST WHILE &lcKey = "P" ;
  FOR EVALUATE(loFormset.lcMainFilt) AND EVALUATE(lcExpr)

  SCATTER MEMVAR
  SELECT (loFormSet.lcAPPAYMNT)
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
llGetApply = .F.
lnPaymthd = ATC('APPAYMNT.CPAYMETH',lcExpr)
IF lnPaymthd > 0
  IF "'D'" $ SUBSTR(lcExpr,lnPaymthd)
    llGetApply = .T.
  ENDIF
ELSE
  llGetApply = .T.    
ENDIF
IF llGetApply
  lcNewExp = lcExpr
  lnPaymthdPos =  ATC("INLIST(APPAYMNT.CPAYMETH",lcExpr)
  IF lnPaymthdPos > 0
    lcMethExp =  SUBSTR(lcExpr,lnPaymthdPos)
    lnEnd = ATC(')',lcMethExp)
    lcMethExp = SUBSTR(lcMethExp ,1,lnEnd)
    lcNewExp = STRTRAN(lcExpr,lcMethExp ," .T. ")
  ENDIF
  lcNewExp  = STRTRAN(lcNewExp ,"APPAYMNT","APDIST")
  lcNewExp  = STRTRAN(lcNewExp ,"CPAYCLNO","CVENDCODE")
  lcNewExp  = STRTRAN(lcNewExp ,"DPAYDATE","DAPDTRDAT")

  SELECT cVendCode,CINVNO,DAPDTRDAT,CBNKCODE,CCHKACCT,SUM(napdamnt) as 'napdamnt'  FROM APDIST WHERE &lcNewExp. AND capdactid ='A' AND capdtrtyp ='A' ;
                                                                                                     AND napdamnt > 0 AND !EMPTY(capdref) AND ;
																		                             gfSEEK(capdref+CVENDCODE,'APINVHDR','INVVEND') AND;
																		                             APINVHDR.ninvamnt > 0 AND cApdStat <> 'V' ;
																		                             GROUP BY cVendCode,CINVNO,DAPDTRDAT,CBNKCODE,CCHKACCT ;
																		                             INTO CURSOR 'APPDEBITS'
   
   IF RECCOUNT('APPDEBITS')>0
     SELECT 'APPDEBITS'
     LOCATE 
     SCAN 
       SCATTER MEMO MEMVAR 
       m.CPAYCLNO  = m.CVENDCODE
       m.DPAYDATE  = m.DAPDTRDAT
       m.CPAYMETH  = 'D'
       m.CPAYDOCNO = m.CINVNO
       m.npayamnt  = m.napdamnt 
       =gfSeek(m.CVENDCODE,'APVENDOR','VENCODE')
       m.CPAYCOMP = APVENDOR.cvencomp 
       INSERT INTO (loFormSet.lcAPPAYMNT) FROM MEMVAR
     ENDSCAN 
   ENDIF
ENDIF
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]


*- Void Date
loFormSet.Ariaform1.ldVoidDate.Text1.Enabled = .T.
loFormSet.Ariaform1.ldVoidDate.Text1.Value = oAriaApplication.SystemDate

*!B610273,1 HIA 03/18/2013 Aria XP - voiding payment [T20130220.0007][Start]
loFormSet.Ariaform1.ldVoidDate.Text1.OldValue = loFormSet.Ariaform1.ldVoidDate.Text1.Value 
*!B610273,1 HIA 03/18/2013 Aria XP - voiding payment [T20130220.0007][End]

loFormSet.ariaform1.grdAPPAYMNT.RecordSource = loFormSet.lcAPPAYMNT
=lfGetGridColmns(loFormSet)

*- check if there is any collected lines or not
SELECT (loFormSet.lcAPPAYMNT)
*- set the filter
SET FILTER TO !LVOIDED
LOCATE
IF EOF()
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *STORE 'payments'              TO lcTMsgTxt  
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_PAYMENTS,loFormSet.GetHeaderText("LANG_APVDPAY_PAYMENTS",loFormSet.HeaderAlias))   TO lcTMsgTxt
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  =gfModalGen("TRM04089B00000","DIALOG",lcTMsgTxt)
  loFormSet.ChangeMode('S')
ELSE
  *- go to Edit mode when data is selected
  loFormSet.ChangeMode('E')
ENDIF
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


  CASE loFormSet.ActiveMode = 'E'

    llEnable = .T.

    oAriaApplication.oToolBar.ChangeButtonStatus('pbScop','DISABLED')

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

*****************************************************************
*Name       : lfFormBeforeSave
*Developer  : TMI
*Date       : 01/24/2012
*Purpose    : check if there is any data to save
*****************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet
LOCAL lnRec,lcFlt,lnSlct
lnSlct = SELECT(0)

SELECT (loFormSet.lcAPPAYMNT)
lnRec = RECNO()
lcFlt = FILTER()

SET FILTER TO
LOCATE
LOCATE FOR LVOIDED
IF !FOUND()
  SELECT (loFormSet.lcAPPAYMNT)
  SET FILTER TO &lcFlt
  LOCATE
  IF BETWEEN(lnRec,1,RECCOUNT(loFormSet.lcAPPAYMNT))
    GO lnRec
  ENDIF
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'No payments have been voided')  
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_NOVOID,loFormSet.GetHeaderText("LANG_APVDPAY_NOVOID",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  SELECT (lnSlct)
  RETURN .F.
ENDIF
SELECT (lnSlct)
 *- End of lfFormBeforeSave.

*!*************************************************************
*! Name      : lfFormSavefiles
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/13/2011
*! Purpose   : Save data
*!*************************************************************
FUNCTION lfFormSavefiles
PARAMETERS loFormSet

loFormSet.ariaform1.grdAPPAYMNT.RecordSource = ''
ldVoidDate = loFormSet.Ariaform1.ldVoidDate.Text1.Value
lcAPPAYMNT = loFormSet.lcAPPAYMNT
SELECT &lcAPPAYMNT
SET FILTER TO
LOCATE
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
*SCAN FOR LVOIDED
SCAN FOR LVOIDED AND CPAYMETH <> 'D'
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
  SELECT &lcAPPAYMNT
  *E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
  *=SEEK(CPAYTYPE+CPAYMETH+CPAYDOCNO+CBNKCODE+CCHKACCT,'APPAYMNT','TYPMETHDOC')  
  =SEEK(CPAYTYPE+CPAYMETH+SUBSTR(CPAYDOCNO,1,8)+CBNKCODE+CCHKACCT,'APPAYMNT','TYPMETHDOC')
  *E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
  SELECT APPAYMNT

  IF APPAYMNT.lPayAdvan
    llAdvPay = .T.
    REPLACE APINVHDR.CINVSTAT  WITH 'V',;
            APINVHDR.LLOK_STAT WITH .F.,;
            APINVHDR.CLOK_USER WITH ' ',;
            APINVHDR.DLOK_DATE WITH {} ,;
            APINVHDR.CLOK_TIME WITH ' '
  ELSE
    llAdvPay = .F.
  ENDIF

  SELECT APDIST

  SCAN REST WHILE APDIST.CAPDTRTYP+APDIST.CBNKCODE+;
                  APDIST.CCHKACCT +APDIST.CAPDREF = ;
                  APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+;
                  APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO;
              FOR APDIST.CAPDSTAT <> 'V'
    DO CASE

      CASE APDIST.CAPDACTID = 'A'

        SCATTER MEMVAR MEMO
        REPLACE cApdStat WITH 'V'

        m.lApdPost  = .F.
        m.cApdStat  = 'V'
        m.nApdAmnt  = 0 - m.nApdAmnt
        m.cBatchNo  = ' '
        m.cTrnSleDn = ' '
        m.cFisFYear = loFormSet.lcFiscalY
        m.cFsPPrdId = loFormSet.lcFiscalP
        m.cApSessNo = loFormSet.lcSequence

        m.nEqvAmnt  = 0 - m.nEqvAmnt

        *- Use the void date
        m.dAPDTrDat = ldVoidDate

        SELECT(loFormSet.lcApDist)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =lfAdd_Info(.T.)

        SELECT APDIST

        *-  Scan through APDIST file for all invoices that may have
        *-  been applied by the current debit memo.
        *-  An application record has cApDTrTyp = 'A'
        *-  Save environment
        lnApDistRec = RECNO()
        lnApDistTag = VAL(SYS(21))
        *-  INVVEND Tag expression is : CINVNO+CVENDCODE+CAPDTRTYP
        *-  Scan only negative records.
        SET ORDER TO TAG INVVEND

        *- If the payment to be void is an advance payment and
        *there is some application record for the current debit memo
        *(The Advance payment Debit memo)
        IF llAdvPay .AND. SEEK(m.cInvNo + m.cVendCode + 'A')
          SCAN REST ;
              WHILE cInvNo +   cVendCode + cAPDTrTyp = m.cInvNo + m.cVendCode + 'A' ;
                FOR cApdStat <> 'V'
            IF nAPDAmnt < 0
              IF cAPDActID = 'A'
                *-   Add the applied amount to the vendor's open debit.
                REPLACE APVENDOR.nVenOpnDr WITH APVENDOR.nVenOpnDr ;
                              + ABS(APDIST.nEqvAmnt)
                *-  Subtract applied amount from APVENHST.nVnHDMApp
                REPLACE APVENHST.nVnHDMApp WITH APVENHST.nVnHDMApp + ;
                               APDIST.nEqvAmnt
              ENDIF    && End of IF cAPDActID = 'A'

              *- If this is a discount record
              IF cAPDActID = 'S'
                SELECT APVENDOR
                REPLACE nVenBal   WITH nVenBal - APDIST.nApdAmnt ,;
                        nVenOpnDr WITH nVenOpnDr + IIF(APINVHDR.nInvAmnt < 0 , APDIST.nApdAmnt , 0)
                SELECT APVENHST
                REPLACE nVnhDisTkn WITH nVnhDisTkn + APDIST.nApdAmnt
              ENDIF    && End of IF cAPDActID = 'S'

              =lfAdd_Info(.F.,'APVENDOR')
              =lfAdd_Info(.F.,'APVENHST')

              IF SEEK(APDIST.cAPDRef + m.cVendCode, 'APINVHDR')
                SELECT APINVHDR
                IF APDIST.cAPDActID = 'A'
                  *-  Deduct the applied amount from the invoice
                  *-  (APDIST.nAPDAmnt is negative)
                  REPLACE nInvAdj WITH nInvAdj + APDIST.nAPDAmnt
                ENDIF    && End of IF cAPDActID = 'A'

                *- If this is a discount record
                IF APDIST.cAPDActID = 'S'
                  REPLACE nInvDisTk WITH nInvDisTk + APDIST.nAPDAmnt
                ENDIF    && End of IF cAPDActID = 'S'
                =lfAdd_Info(.F.,'APINVHDR')
                =lfClearLock()
                SELECT APDIST
              ENDIF
            ENDIF    && End of IF nAPDAmnt < 0

            REPLACE cApdStat WITH 'V'
            SCATTER MEMVAR MEMO

            m.lApdPost  = .F.
            m.nApdAmnt  = 0 - m.nApdAmnt
            m.cBatchNo  = ' '
            m.cTrnSleDn = ' '
            m.cFisFYear = loFormSet.lcFiscalY
            m.cFsPPrdId = loFormSet.lcFiscalP
            m.cApSessNo = loFormSet.lcSequence
            m.dAPDTrDat = ldVoidDate

            SELECT(loFormSet.lcApDist)
            APPEND BLANK
            GATHER MEMVAR MEMO
            =lfAdd_Info(.T.)

            SELECT APDIST
          ENDSCAN
        ENDIF
        *-  Restore environment
        SET ORDER TO (lnApDistTag)
        GO lnApDistRec

        IF !EOF('APINVHDR')
          *- IF Statment to check if the payment is an advanced payment
          IF llAdvPay
            REPLACE APINVHDR.NINVPAID WITH 0
          ELSE    && Else
            REPLACE APINVHDR.NINVPAID WITH APINVHDR.NINVPAID - APDIST.NAPDAMNT
          ENDIF   && End of IF
          =lfAdd_Info(.F.,'APINVHDR')
        ENDIF   && End of IF

      CASE APDIST.CAPDACTID = 'B'

        *- Update Invoice 1099A amount with APDIST.nAPDAmnt,
        *- (same currency as that of the invoice), but,
        *- update Vendor fields with APDIST.nEqvAmnt
        *- (in base currency), not the amount field.
        REPLACE APINVHDR.NINV1099A WITH APINVHDR.NINV1099A + APDIST.NAPDAMNT,;
                APVENDOR.NINV1099B WITH APVENDOR.NINV1099B - APDIST.nEqvAmnt

      CASE APDIST.CAPDACTID = 'C'

        SCATTER MEMVAR MEMO
        REPLACE cApdStat WITH 'V'

        m.lApdPost  = .F.
        m.cApdStat  = 'V'
        m.nApdAmnt  = 0 - m.nApdAmnt
        m.cBatchNo  = ' '
        m.cTrnSleDn = ' '
        m.cFisFYear = loFormSet.lcFiscalY
        m.cFsPPrdId = loFormSet.lcFiscalP
        m.cApSessNo = loFormSet.lcSequence

        *- Add a variable for nEqvAmnt field
        m.nEqvAmnt  = 0 - m.nEqvAmnt

        *- Use the void date
        m.dAPDTrDat = ldVoidDate

        SELECT(loFormSet.lcApDist)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =lfAdd_Info(.T.)

        SELECT APDIST

        lcNewPayFld = 'APVENHST.NVNHPAY'+ALLTRIM(STR(INT(VAL(loFormSet.lcFiscalP))))
        IF llAdvPay

          *- Update Vendor fields with the equivalent
          *- amount (in base currency), not the amount field.
          REPLACE APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,0 - APDIST.nEqvAmnt,0),;
                  APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - APDIST.nEqvAmnt                             ,;
                  APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + APDIST.nEqvAmnt
        ELSE
          *- Update Vendor fields with the equivalent
          *- amount (in base currency), not the amount field.
          REPLACE APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,APDIST.nEqvAmnt,0),;
                  APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - APDIST.nEqvAmnt                             ,;
                  APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + APDIST.nEqvAmnt
        ENDIF
        =lfAdd_Info(.F.,'APVENDOR')

        *- Update Vendor History fields with the equivalent
        *- amount (in base currency), not the amount field.
        REPLACE APVENHST.NVNHTOTPA WITH APVENHST.NVNHTOTPA    + APDIST.nEqvAmnt,;
                &lcNewPayFld       WITH EVALUATE(lcNewPayFld) + APDIST.nEqvAmnt

        =lfAdd_Info(.F.,'APVENHST')

        DO CASE
          CASE APPAYMNT.CPAYMETH = 'M'
            *- Update Vendor History fields with the equivalent
            *- amount (in base currency), not the amount field.
            REPLACE APVENHST.NVNHMCHKP WITH APVENHST.NVNHMCHKP + APDIST.nEqvAmnt

          CASE APPAYMNT.CPAYMETH = 'N'

            *- Update Vendor History fields with the equivalent
            *- amount (in base currency), not the amount field.
            REPLACE APVENHST.NVNHNCHKP WITH APVENHST.NVNHNCHKP + APDIST.nEqvAmnt

          CASE APPAYMNT.CPAYMETH = 'H'

            *- Update Vendor History fields with the equivalent
            *- amount (in base currency), not the amount field.
            REPLACE APVENHST.NVNHCASHP WITH APVENHST.NVNHCASHP + APDIST.nEqvAmnt

          CASE APPAYMNT.CPAYMETH = 'P'
            REPLACE APVENHST.NVNHPCHKP WITH APVENHST.NVNHPCHKP + APDIST.nEqvAmnt
        ENDCASE

        SELECT APDIST

      CASE APDIST.CAPDACTID = 'S'

        SCATTER MEMVAR MEMO
        REPLACE cApdStat WITH 'V'

        m.lApdPost  = .F.
        m.cApdStat  = 'V'
        m.nApdAmnt  = 0 - m.nApdAmnt
        m.cBatchNo  = ' '
        m.cTrnSleDn = ' '
        m.cFisFYear = loFormSet.lcFiscalY
        m.cFsPPrdId = loFormSet.lcFiscalP
        m.cApSessNo = loFormSet.lcSequence

        *- Add a variable for nEqvAmnt field
        m.nEqvAmnt  = 0 - m.nEqvAmnt

        *- Use the void date instead of the payment date.
        m.dAPDTrDat = ldVoidDate
        *- end.

        SELECT(loFormSet.lcApDist)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =lfAdd_Info(.T.)

        SELECT APDIST
        IF llAdvPay
          *- Update Invoice fields with APDIST.nAPDAmnt,
          *- (same currency as that of the invoice), but,
          *- update Vendor fields with APDIST.nEqvAmnt
          *- (in base currency), not the amount field.
          REPLACE APINVHDR.NINVDISTK  WITH APINVHDR.NINVDISTK  + APDIST.NAPDAMNT,;
                  APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR  - IIF(APINVHDR.NINVAMNT < 0,0 - APDIST.nEqvAmnt,0),;
                  APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL    - APDIST.nEqvAmnt
        ELSE
          *- Update Invoice fields with APDIST.nAPDAmnt,
          *- (same currency as that of the invoice), but,
          *- update Vendor fields with APDIST.nEqvAmnt
          *- (in base currency), not the amount field.
          REPLACE APINVHDR.NINVDISTK  WITH APINVHDR.NINVDISTK  + APDIST.NAPDAMNT,;
                  APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR  - IIF(APINVHDR.NINVAMNT < 0,APDIST.nEqvAmnt,0),;
                  APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL    - APDIST.nEqvAmnt
        ENDIF
        =LfAdd_Info(.F.,'APVENDOR')

        *- Update Vendor History fields with APDIST.nEqvAmnt
        *- (in base currency), not the amount field.
        REPLACE APVENHST.NVNHDISTKN WITH APVENHST.NVNHDISTKN + APDIST.nEqvAmnt
        =lfAdd_Info(.F.,'APVENHST')

        IF !EOF('APINVHDR')
          REPLACE APINVHDR.NINVPAID WITH APINVHDR.NINVPAID - APDIST.NAPDAMNT
          =lfAdd_Info(.F.,'APINVHDR')
        ENDIF   && End of IF

        SELECT APDIST

      CASE APDIST.CAPDACTID = 'J'

        SCATTER MEMVAR MEMO
        REPLACE cApdStat WITH 'V'

        m.lApdPost  = .F.
        m.cApdStat  = 'V'
        m.nApdAmnt  = 0 - m.nApdAmnt
        m.cBatchNo  = ' '
        m.cTrnSleDn = ' '
        m.cFisFYear = loFormSet.lcFiscalY
        m.cFsPPrdId = loFormSet.lcFiscalP
        m.cApSessNo = loFormSet.lcSequence

        *- Add a variable for nEqvAmnt field
        m.nEqvAmnt  = 0 - m.nEqvAmnt

        *- Use the void date instead of the payment date.
        m.dAPDTrDat = ldVoidDate

        SELECT(loFormSet.lcApDist)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =lfAdd_Info(.T.)

        SELECT APDIST
        IF llAdvPay
          *- Update Invoice fields with APDIST.nAPDAmnt,
          *- (same currency as that of the invoice), but,
          *- update Vendor fields with APDIST.nEqvAmnt
          *- (in base currency), not the amount field.
          REPLACE APINVHDR.NINVADJ    WITH APINVHDR.NINVADJ   + APDIST.NAPDAMNT,;
                  APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,0 - APDIST.nEqvAmnt,0),;
                  APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL   - APDIST.nEqvAmnt
        ELSE
          *- Update Invoice fields with APDIST.nAPDAmnt,
          *- (same currency as that of the invoice), but,
          *- update Vendor fields with APDIST.nEqvAmnt
          *- (in base currency), not the amount field.
          *- IF Statment to check if the record is  Approved
          *          adjustment record and not a calculated one
          IF APDIST.NAPDLINNO <> 1
            REPLACE APINVHDR.NINVPAID WITH APINVHDR.NINVPAID - APDIST.NAPDAMNT,;
                    APINVHDR.NINVADJ    WITH APINVHDR.NINVADJ   + APDIST.NAPDAMNT,;
                    APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,APDIST.nEqvAmnt,0),;
                    APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL   - APDIST.nEqvAmnt
            =LfAdd_Info(.F.,'APINVHDR')
          ENDIF  && End of IF
        ENDIF
        =LfAdd_Info(.F.,'APVENDOR')
    ENDCASE

    SELECT APINVHDR
    =lfClearLock()
    SELECT APDIST
  ENDSCAN

  SELECT APPAYMNT
  IF !EOF()
    GO RECNO()
  ENDIF

  SELECT APDIST

  SCAN REST WHILE APDIST.CAPDTRTYP+APDIST.CBNKCODE+;
                  APDIST.CCHKACCT +APDIST.CAPDREF = ;
                  APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+;
                  APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO;
              FOR APDIST.CAPDACTID = 'A'
    SELECT APINVHDR
    =lfClearLock()
   SELECT APDIST
  ENDSCAN

  SELECT APPAYMNT
  REPLACE CPAYSTAT  WITH 'V',;
          DPAYVDATE WITH ldVoidDate
  =lfAdd_Info()
  =lfClearLock()
ENDSCAN
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
SCAN FOR LVOIDED AND CPAYMETH ='D'
   SELECT APDIST
   lnApDistTag = ORDER()
   =gfSETORDER('INVVEND')
   =gfSeek(PADR(EVALUATE(loFormSet.lcAPPAYMNT+".CPAYDOCNO"),12)+EVALUATE(loFormSet.lcAPPAYMNT+".CPAYCLNO")+'A') 
   SCAN REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = PADR(EVALUATE(loFormSet.lcAPPAYMNT+".CPAYDOCNO"),12)+EVALUATE(loFormSet.lcAPPAYMNT+".CPAYCLNO")+'A' FOR ;
				  DAPDTRDAT = EVALUATE(loFormSet.lcAPPAYMNT+".DPAYDATE") AND CBNKCODE = EVALUATE(loFormSet.lcAPPAYMNT+".CBNKCODE") AND cApDStat <> 'V' ;
				  AND CCHKACCT  = EVALUATE(loFormSet.lcAPPAYMNT+".CCHKACCT") AND !EMPTY(CAPDREF) AND;
				  gfSeek(CAPDREF+CVENDCODE,'APINVHDR','INVVEND') AND APINVHDR.nInvAmnt > 0 
				  
	 =gfSeek(APDIST.CVENDCODE,'APVENDOR','VENCODE')			  
     DO CASE
       CASE APDIST.CAPDACTID = 'A'
       
         SCATTER MEMVAR MEMO
         REPLACE cApdStat WITH 'V'
         m.lApdPost  = .F.
         m.cApdStat  = 'V'
         m.nApdAmnt  = 0 - m.nApdAmnt
         m.cBatchNo  = ' '
         m.cTrnSleDn = ' '
         m.cFisFYear = loFormSet.lcFiscalY
         m.cFsPPrdId = loFormSet.lcFiscalP
         m.cApSessNo = loFormSet.lcSequence

         m.nEqvAmnt  = 0 - m.nEqvAmnt

         *- Use the void date
         m.dAPDTrDat = ldVoidDate
         SELECT(loFormSet.lcApDist)
         APPEND BLANK
         GATHER MEMVAR MEMO
         =lfAdd_Info(.T.)
         IF APDIST.nApdAmnt < 0
           =gfSeek(APDIST.CINVNO+APDIST.CVENDCODE,'APINVHDR','INVVEND')
           REPLACE APINVHDR.NINVPAID WITH APINVHDR.NINVPAID - APDIST.NAPDAMNT
           =lfAdd_Info(.F.,'APINVHDR')
           REPLACE APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR  - IIF(APINVHDR.NINVAMNT < 0,APDIST.nEqvAmnt,0)
                   *
                   *APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL    - APDIST.nEqvAmnt
                   *
           =LfAdd_Info(.F.,'APVENDOR')
           SELECT APINVHDR
            =lfClearLock()
         ELSE
           = gfSeek(APDIST.CAPDREF+ APDIST.CVENDCODE,'APINVHDR','INVVEND') 
           REPLACE APINVHDR.NINVADJ WITH APINVHDR.NINVADJ - APDIST.NAPDAMNT
           =lfAdd_Info(.F.,'APINVHDR')
           SELECT APINVHDR
            =lfClearLock()
         ENDIF  
         
      CASE APDIST.CAPDACTID = 'S'
        SCATTER MEMVAR MEMO
        REPLACE cApdStat WITH 'V'

        m.lApdPost  = .F.
        m.cApdStat  = 'V'
        m.nApdAmnt  = 0 - m.nApdAmnt
        m.cBatchNo  = ' '
        m.cTrnSleDn = ' '
        m.cFisFYear = loFormSet.lcFiscalY
        m.cFsPPrdId = loFormSet.lcFiscalP
        m.cApSessNo = loFormSet.lcSequence

        *- Add a variable for nEqvAmnt field
        m.nEqvAmnt  = 0 - m.nEqvAmnt

        *- Use the void date instead of the payment date.
        m.dAPDTrDat = ldVoidDate
        *- end.

        SELECT(loFormSet.lcApDist)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =lfAdd_Info(.T.)

        SELECT APDIST
        
          *- Update Invoice fields with APDIST.nAPDAmnt,
          *- (same currency as that of the invoice), but,
          *- update Vendor fields with APDIST.nEqvAmnt
          *- (in base currency), not the amount field.
        =gfSeek(APDIST.CAPDREF+APDIST.CVENDCODE,'APINVHDR','INVVEND')
        REPLACE APINVHDR.NINVDISTK  WITH APINVHDR.NINVDISTK  + APDIST.NAPDAMNT,;
                APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR  - IIF(APINVHDR.NINVAMNT < 0,APDIST.nEqvAmnt,0),;
                APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL    - APDIST.nEqvAmnt
        =LfAdd_Info(.F.,'APVENDOR')

        *- Update Vendor History fields with APDIST.nEqvAmnt
        *- (in base currency), not the amount field.
        REPLACE APVENHST.NVNHDISTKN WITH APVENHST.NVNHDISTKN + APDIST.nEqvAmnt
        =lfAdd_Info(.F.,'APVENHST')

        IF !EOF('APINVHDR')
          REPLACE APINVHDR.NINVPAID WITH APINVHDR.NINVPAID - APDIST.NAPDAMNT
          =lfAdd_Info(.F.,'APINVHDR')
          SELECT APINVHDR
          =lfClearLock()
        ENDIF   && End of IF

        SELECT APDIST

     CASE APDIST.CAPDACTID = 'J'

        SCATTER MEMVAR MEMO
        REPLACE cApdStat WITH 'V'

        m.lApdPost  = .F.
        m.cApdStat  = 'V'
        m.nApdAmnt  = 0 - m.nApdAmnt
        m.cBatchNo  = ' '
        m.cTrnSleDn = ' '
        m.cFisFYear = loFormSet.lcFiscalY
        m.cFsPPrdId = loFormSet.lcFiscalP
        m.cApSessNo = loFormSet.lcSequence

        *- Add a variable for nEqvAmnt field
        m.nEqvAmnt  = 0 - m.nEqvAmnt

        *- Use the void date instead of the payment date.
        m.dAPDTrDat = ldVoidDate

        SELECT(loFormSet.lcApDist)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =lfAdd_Info(.T.)

        SELECT APDIST
        *- Update Invoice fields with APDIST.nAPDAmnt,
        *- (same currency as that of the invoice), but,
        *- update Vendor fields with APDIST.nEqvAmnt
        *- (in base currency), not the amount field.
        *- IF Statment to check if the record is  Approved
        *          adjustment record and not a calculated one
        IF APDIST.NAPDLINNO <> 1
          =gfSeek(APDIST.CAPDREF+APDIST.CVENDCODE,'APINVHDR','INVVEND')
          REPLACE APINVHDR.NINVPAID   WITH APINVHDR.NINVPAID - APDIST.NAPDAMNT,;
                  APINVHDR.NINVADJ    WITH APINVHDR.NINVADJ  + APDIST.NAPDAMNT,;
                  APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL  - APDIST.nEqvAmnt
          *APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,APDIST.nEqvAmnt,0)         
          =LfAdd_Info(.F.,'APINVHDR')
          SELECT APINVHDR
           =lfClearLock()
        ENDIF  && End of IF
        =LfAdd_Info(.F.,'APVENDOR')
     ENDCASE
   ENDSCAN    
   SELECT APDIST
   SET ORDER TO (lnApDistTag)
ENDSCAN
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]

*- Update tables
SELECT APINVHDR
=gfTableUpdate()
SELECT APPAYMNT
=gfTableUpdate()
SELECT APVENDOR
=gfTableUpdate()
SELECT APVENHST
=gfTableUpdate()
SELECT APDIST
=gfTableUpdate()

*- Call a new session of data collection
SELECT (loFormSet.lcAPPAYMNT)
ZAP
loFormSet.ChangeMode('S')

RETURN
*- End of lfFormSavefiles

********************************************************************************
*Name       : lfAdd_Info
*Developer  : TMI
*Date       : 01/24/2012
*Purpose    : user information
********************************************************************************
FUNCTION lfAdd_Info
PARAMETERS llAdd,lcAlias
LOCAL lnSlct
lnSlct = SELECT(0)
lcAlias = IIF(EMPTY(lcAlias),ALIAS(),lcAlias)
SELECT (lcAlias)

IF llAdd
  IF TYPE('&lcAlias..cAdd_User') = 'C'
    REPLACE cAdd_User  WITH oAriaApplication.User_ID ,;
            dAdd_Date  WITH DATE()    ,;
            cAdd_Time  WITH gfGetTime(),;
            cAdd_Ver   WITH oAriaApplication.cShortVersion
  ENDIF
ELSE && Modified Record
  IF TYPE('&lcAlias..cEdit_User') = 'C'
    REPLACE cEdit_User  WITH oAriaApplication.User_ID ,;
            dEdit_Date  WITH DATE()    ,;
            cEdit_Time  WITH gfGetTime(),;
            cEdt_Ver    WITH oAriaApplication.cShortVersion
  ENDIF
ENDIF

IF TYPE('&lcAlias..cowner') = 'C'
  replace &lcAlias..cowner WITH ''
ENDIF

SELECT (lnSlct)
*- End of lfAdd_Info

********************************************************************************
*Name       : lfClearLock
*Developer  : TMI
*Date       : 1/14/2012
*Purpose    : Clear Locks
********************************************************************************
FUNCTION lfClearLock
PARAMETERS lcAlias
LOCAL lnSlct
lnSlct = SELECT(0)
lcAlias = IIF(EMPTY(lcAlias),ALIAS(),lcAlias)
SELECT &lcAlias
*- Clear lock fields
replace LLOK_STAT WITH .f. ;
        CLOK_USER WITH '' ;
        DLOK_DATE WITH {} ;
        CLOK_TIME WITH ''
SELECT (lnSlct)
*- End of lfClearLock.

************************************************************************************************
*Name       : lfFormBeforealtermode
*Developer  : TMI
*Date       : 01/24/2012
*Purpose    : check before undo
************************************************************************************************
FUNCTION lfFormBeforealtermode
PARAMETERS loFormSet
LOCAL lnRec,lcFil
IF TYPE('loFormSet.lcAPINVHDR')='U'
  RETURN
ENDIF

SELECT (loFormSet.lcAPINVHDR)
lnRec = RECNO()
lcFil = FILTER()
SET FILTER TO
LOCATE
LOCATE FOR LUPDATED
IF FOUND()
  * Are you sure you want to ð?
  * \!\<Yes;\?\<No
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *IF gfModalGen('QRM40169B00006','DIALOG','lose all your changes?') <> 1  
  IF gfModalGen('QRM40169B00006','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_LOSECHANGES,loFormSet.GetHeaderText("LANG_APVDPAY_LOSECHANGES",loFormSet.HeaderAlias))) <> 1
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
    SELECT (loFormSet.lcAPINVHDR)
    SET FILTER TO &lcFil
    LOCATE
    GO lnRec
    RETURN .F.
  ENDIF
ENDIF

 *- End of lfFormBeforealtermode             .

************************************************************************************************
*Name      : lfFormUndo
*Developer :TMI - Tarek Mohamed Ibrahim
*Date      :12/24/2011
*Purpose   :undo procedure
************************************************************************************************
FUNCTION lfFormUndo
PARAMETERS loFormSet
LOCAL lnRec,lcSvFilt
SELECT (loFormSet.lcAPPAYMNT)
lnRec = RECNO()
lcSvFilt = FILTER()
SET FILTER TO
LOCATE
LOCATE FOR LVOIDED
IF FOUND()
  * Are you sure you want to ð?
  * \!\<Yes;\?\<No
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *IF gfModalGen('QRM40169B00006','DIALOG','lose all your changes') <> 1  &&
  IF gfModalGen('QRM40169B00006','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_LOSECHANGES,loFormSet.GetHeaderText("LANG_APVDPAY_LOSECHANGES",loFormSet.HeaderAlias))) <> 1  &&    
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    SELECT (loFormSet.lcAPPAYMNT)
    SET FILTER TO &lcSvFilt
    IF BETWEEN(lnRec,1,RECCOUNT(loFormSet.lcAPPAYMNT))
      GO lnRec
    ENDIF
    RETURN .F.
  ENDIF
ENDIF
*- always remove locks
=lfUndoLocks(loFormset)

IF !loFormset.lUnload
  =lfGetData(loFormset)
ENDIF
*- End of lfFormUndo

****************************************************************************************
*Name      : lfDefineVars
*Developer : TMI - TAREK MOHAMED IBRAHIM
*Date      : 1/1/2012
*Purpose   : Define all variables to be used in the screen
***************************************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormset


*** Get payment methods array
=lfAddProp(loFormSet,'laPayMeth[1]' ,'')   && Array to hold payment methods
WITH loFormSet
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
*DIMENSION .laPayMeth[4,2]
DIMENSION .laPayMeth[5,2]
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*	.laPayMeth[1,1] = 'Printed checks'
*!*	.laPayMeth[1,2] = 'P'
*!*	.laPayMeth[2,1] = 'Manual checks'
*!*	.laPayMeth[2,2] = 'M'
*!*	.laPayMeth[3,1] = 'Non check payments'
*!*	.laPayMeth[3,2] = 'N'
*!*	.laPayMeth[4,1] = 'Cash payment'
.laPayMeth[1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_PTINTEDCHECK,loFormSet.GetHeaderText("LANG_APVDPAY_PTINTEDCHECK",loFormSet.HeaderAlias))
.laPayMeth[1,2] = 'P'
.laPayMeth[2,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_MANULCHECK ,loFormSet.GetHeaderText("LANG_APVDPAY_MANULCHECK",loFormSet.HeaderAlias))
.laPayMeth[2,2] = 'M'
.laPayMeth[3,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_NONCHECK ,loFormSet.GetHeaderText("LANG_APVDPAY_NONCHECK",loFormSet.HeaderAlias))
.laPayMeth[3,2] = 'N'
.laPayMeth[4,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_CASHPAY,loFormSet.GetHeaderText("LANG_APVDPAY_CASHPAY",loFormSet.HeaderAlias))
*N000682,1 MMT 12/09/2012 Globalization changes[END]
.laPayMeth[4,2] = 'H'
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
.laPayMeth[5,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_APPLYDEBIT,loFormSet.GetHeaderText("LANG_APVDPAY_APPLYDEBIT",loFormSet.HeaderAlias))
.laPayMeth[5,2] = 'D'
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
ENDWITH

=lfAddProp(loFormSet,'lcFiscalP,lcFiscalY' ,' ')   && Year,period variables
=lfAddProp(loFormSet,'lUnload',.F.)   && Year,period variables

=lfAddProp(loFormSet,'ap1',CREATEOBJECT('ap'))   && Year,period variables



RETURN

*!**************************************************************************
*! Function: lfGetPayMeth
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/24/2012
*!**************************************************************************
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[Start]
*FUNCTION lfGetPayMeth
FUNCTION lfGetVdPayMeth
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[End]
PARAMETER loFormSet
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][Start]
SET DATASESSION TO loFormSet.DataSessionID
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][End]

lcAPPAYMNT = loFormSet.lcAPPAYMNT
lcPayCode = &lcAPPAYMNT..CPAYMETH

lnposition= ASCAN(loFormSet.laPayMeth,lcPayCode,1)
IF lnposition <>  0	
  RETURN loFormSet.laPayMeth[ASUBSCRIPT(loFormSet.laPayMeth, lnposition, 1),1]
ELSE
  RETURN " "
ENDIF


*!*************************************************************
*! Name      : lfVlDate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/24/2012
*! Purpose   : To Validate dates from the periods file.
*!*************************************************************
*! Parameters: The company that you want to validate from its periods.
*!           : Logical parameter to accept the date in a locked period.
*!*************************************************************
*! Returns   : True or False
*!*************************************************************
FUNCTION lfVlDate
PARAMETERS lcCompany, lcCrPrd, lcCrYer, ldDateTVal, llLockPer
LOCAL ap1

PRIVATE llOpenPrd, lcSavOrdPr, llValidDate, lcExactStat

IF TYPE('lcCurYear') <> 'C' .AND. TYPE('lcCurPrd') <> 'C'
  PRIVATE lcCurYear, lcCurPrd, lLockStat

  lcCrYer    = IIF(TYPE('lcCrYer') <> 'C',' ',lcCrYer)

  lcCrPrd    = IIF(TYPE('lcCrPrd') <> 'C',' ',lcCrPrd)
ENDIF

llLockPer  = IIF(llLockPer,llLockPer,.F.)

llValidDate= .F.      && Variable to return with from the function.

lcCurYear  = ' '      && Variable to hold the current year.
lcCurPrd   = ' '      && Varibale to hold the current period.
lcExactStat= ' '      && Variable to hold the SET EXACT status.
lLockStat  = .F.      && Variable to hold the lock stat of the record.

lcSavSelct = ALIAS()  && Variable to save the currently selected file.

llOpenPrd  = .F.      && Variable to indicate that the there is a file;
                        && OPEN BY the FUNCTION.

ap1 = CREATEOBJECT("ap")

*PRIVATE llOpAPS, llOpcomp

*llOpcomp = gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','Ccomp_id','SH')  &&lower
*llOpAPS  = gfOpenFile(oAriaApplication.DataDir+'APSETUP','','SH')  &&lower

lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)

lcCompYer  = oAriaApplication.CurrentYear
lcCompPrd  = oAriaApplication.CurrentPeriod



lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)


lcCompYer  = oAriaApplication.CurrentYear
lcCompPrd  = oAriaApplication.CurrentPeriod

IF !USED('FSPRD')   && Check if the period file is open or not.
  llOpenPrd = .T.      && Indicates that the file is open by the function.
  SELECT 0             && Select an empty work area.
  lcdatadir = ap1.lcDataDir
  =gfOpenTable('fsprd','COMFYRPRDI','SH')

ELSE
  SELECT FSPRD      && The file is open so we are going to select

  lcSavOrdPr = ORDER() && Save the file order
  =gfSetOrder('COMFYRPRDI')
ENDIF

IF TYPE('ldDateTVal') <> 'D'
  ldDate = IIF(TYPE('_screen.ActiveForm.ActiveControl.Value')='D',_SCREEN.ACTIVEFORM.ACTIVECONTROL.VALUE,{})
ELSE
  ldDate = ldDateTVal
ENDIF

lcExactStat = SET('EXACT')
SET EXACT OFF
GO TOP

SET EXACT &lcExactStat


LOCATE REST FOR BETWEEN(ldDate,dfsppbgdt,dfsppendt)
IF FOUND() .AND. BETWEEN(ldDate,ap1.ldPyBgDate,ap1.ldNyEnDate)

  llLockStat = lFspLocks  && Assign the variable with the period lock stat.
  lcCurYear  = cFisFYear  && Assign the variable with fiscal year of the period.
  lcCurPrd   = cFspprdid  && Assign the variable with the period no.
  lcCrYer    = cFisFYear  && Assign the variable with fiscal year of the period.
  lcCrPrd    = cFspprdid  && Assign the variable with the period no.
  llLockPer  = lFspLocks  && Assign the variable with the period lock stat.
  llValidDate= .T.        && Assign the variable with .T.

  IF USED('FSPRD') .AND. llOpenPrd
    llOpenPrd = .F.
    =gfCloseTable('FSPRD')
  ELSE
    SELECT FSPRD
    =gfSetOrder(lcSavOrdPr)
  ENDIF

  IF !EMPTY(lcSavSelct)
    SELECT(lcSavSelct)
  ENDIF
ELSE
  IF USED('FSPRD') .AND. llOpenPrd
    llOpenPrd = .F.
    =gfCloseTable('FSPRD')
  ELSE
    SELECT FSPRD
    =gfSetOrder(lcSavOrdPr)
  ENDIF

  IF !EMPTY(lcSavSelct)
    SELECT(lcSavSelct)
  ENDIF
ENDIF

RETURN llValidDate
*--end of lfVlDate

*!**************************************************************************
*! FUNCTION  : lfvVoid
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/24/2012
*! Purpose   : Void process
*!**************************************************************************
FUNCTION lfvVoid
PARAMETERS loFormSet,llUndo
ldVoidDate = loFormSet.Ariaform1.ldVoidDate.Text1.Value
lcAPPAYMNT = loFormSet.lcAPPAYMNT
SELECT &lcAPPAYMNT

*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
*=SEEK(CPAYTYPE+CPAYMETH+CPAYDOCNO+CBNKCODE+CCHKACCT,'APPAYMNT','TYPMETHDOC')
IF &lcAPPAYMNT..CPAYMETH <> 'D'
  =SEEK(CPAYTYPE+CPAYMETH+SUBSTR(CPAYDOCNO,1,8)+CBNKCODE+CCHKACCT,'APPAYMNT','TYPMETHDOC')
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
  
  

  IF APPAYMNT.CPAYSTAT = 'V'
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen("QRM00000B00000",.F.,.F.,.F.,'Payment has already been voided.')  
    =gfModalGen("QRM00000B00000",.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_PAYVIDED,loFormSet.GetHeaderText("LANG_APVDPAY_PAYVIDED",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]  
    SELECT &lcAPPAYMNT
    DELETE
    =lfPaySkip(lcAPPAYMNT)
    RETURN .F.
  ENDIF
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
ENDIF
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
llObjLock = .F.
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*	STORE 'void'         TO lcTVoid
*!*	STORE 'void date'    TO lcTVoidDate
*!*	STORE "or equal the payment date" TO lcTPayDate
*!*	STORE "Void date"    TO lcTVoidDat
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_VOID,loFormSet.GetHeaderText("LANG_APVDPAY_VOID",loFormSet.HeaderAlias))         TO lcTVoid
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_VOIDDATE,loFormSet.GetHeaderText("LANG_APVDPAY_VOIDDATE",loFormSet.HeaderAlias))   TO lcTVoidDate
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_OREQUAL,loFormSet.GetHeaderText("LANG_APVDPAY_OREQUAL",loFormSet.HeaderAlias)) TO lcTPayDate
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_SVOIDDATE,loFormSet.GetHeaderText("LANG_APVDPAY_SVOIDDATE",loFormSet.HeaderAlias))    TO lcTVoidDat
*N000682,1 MMT 12/09/2012 Globalization changes[END]
*- Add a warning message before void any payment.
IF gfModalGen("QRM04153B00006","ALERT",ALLTRIM(APPAYMNT.CPAYDOCNO)) = 2
  ** Message : "Are you sure you want to void payment # ð?"
  **           "          ® Yes ¯    ® No ¯                "
  RETURN .F.
ENDIF

*- Check that the void date is not empty.
IF EMPTY(ldVoidDate)
  *- If the void date is empty, present a message and return.
  *- Message: " You have to enter the ð.  "
  *- Choices: "            ® Ok ¯         "
  =gfModalGen("TRM04066B00000", "DIALOG", lcTVoidDate)
  loFormSet.Ariaform1.ldVoidDate.Text1.Value = loFormSet.Ariaform1.ldVoidDate.Text1.OldValue
  loFormSet.Ariaform1.ldVoidDate.Text1.SetFocus()
  RETURN .F.
ENDIF

*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
*IF ldVoidDate < APPAYMNT.DPAYDATE
IF IIF(&lcAPPAYMNT..CPAYMETH <> 'D',ldVoidDate < APPAYMNT.DPAYDATE,ldVoidDate < &lcAPPAYMNT..DPAYDATE)
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
  *- If the entered value is not greater than zero,
  *- Message :  "   ð should be greater than ð.   "
  *-                 ®   OK   ¯
  =gfModalGen("TRM04072B00000","DIALOG",lcTVoidDat+"|"+ lcTPayDate)
  loFormSet.Ariaform1.ldVoidDate.Text1.Value = loFormSet.Ariaform1.ldVoidDate.Text1.OldValue
  loFormSet.Ariaform1.ldVoidDate.Text1.SetFocus()
  RETURN .F.
ENDIF

llLockPrd = .F.
STORE ' ' TO lcFiscalP,lcFiscalY
IF !lfVlDate(oAriaApplication.prntcompanyid,@lcFiscalP,@lcFiscalY,ldVoidDate,@llLockPrd)
  =gfModalGen('TRM04113B00000', 'DIALOG', lcTVoid)
  loFormSet.Ariaform1.ldVoidDate.Text1.Value = loFormSet.Ariaform1.ldVoidDate.Text1.OldValue
  loFormSet.Ariaform1.ldVoidDate.Text1.SetFocus()
  RETURN .F.
ENDIF
loFormSet.lcFiscalP = lcFiscalP
loFormSet.lcFiscalY = lcFiscalY

IF llLockPrd
  =gfModalGen("QRM01258B00000" , "DIALOG" )
  RETURN .F.
ENDIF

*E303442,1 TMI 02/19/2014 13:51 [Start] check the period of the payment and the voiding date
IF APSETUP.CVDPAYMNT $ 'SW'
  *E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
  *IF lfGetPeriod(ldVoidDate) <> lfGetPeriod(APPAYMNT.DPAYDATE)
  
 *B611768,1 DM 4/21/2019 modify to solve issue: Unable to void payment for Rogers dated March 6, 2019 [T20190418.0008] {Start}
 ** IF lfGetPeriod(ldVoidDate) <> IF IIF(&lcAPPAYMNT..CPAYMETH <> 'D',lfGetPeriod(APPAYMNT.DPAYDATE),lfGetPeriod(&lcAPPAYMNT..DPAYDATE))
 IF lfGetPeriod(ldVoidDate) <> IIF(&lcAPPAYMNT..CPAYMETH <> 'D',lfGetPeriod(APPAYMNT.DPAYDATE),lfGetPeriod(&lcAPPAYMNT..DPAYDATE))
 *B611768,1 DM 4/21/2019 modify to solve issue: Unable to void payment for Rogers dated March 6, 2019 [T20190418.0008] {End}
  
  *E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
    DO case
    CASE APSETUP.CVDPAYMNT = 'S'
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DIFFERENT_PERIODS_DONT_CONTINUE,loFormSet.GetHeaderText("LANG_DIFFERENT_PERIODS_DONT_CONTINUE",loFormSet.HeaderAlias)))

      RETURN .F.
      
    CASE APSETUP.CVDPAYMNT = 'W'
      IF gfModalGen('INM00000B00006',.F.,.F.,.F.,;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DIFFERENT_PERIODS_DONT_WARNING,loFormSet.GetHeaderText("LANG_DIFFERENT_PERIODS_DONT_WARNING",loFormSet.HeaderAlias))) = 2
      
        RETURN .F.
        
      ENDIF     
    ENDCASE 
  ENDIF 
ENDIF 
*E303442,1 TMI 02/19/2014 13:51 [End  ] 

*- Check if the Payment to be void is an advance payment and it
*has already been paid and if so give the user a message and cancel
*the Voiding process [Begin]
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
IF &lcAPPAYMNT..CPAYMETH <> 'D'
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
SELECT APPAYMNT

*- If the Payment to be void is an advance payment and it has already been paid
IF APPAYMNT.lPayAdvan .AND. ;
   (APINVHDR.nInvPaid + APINVHDR.nInvDisTk + APINVHDR.nInvAdj) <> 0 ;
   .AND. lfPayment()

  ** Message : "Advance payment ð is already paid on one or more payments."
  **           " Cannot void that advance payment.                        "
  **           "                          ® Ok ¯                          "
  =gfModalGen("TRM04164B00000" , "DIALOG" , ALLTRIM(APPAYMNT.cPayDocNo))
  RETURN .F.
ENDIF    && End of IF APPAYMNT.lPayAdvan .AND. ...... .AND. lfPayment()
*- Add these lines to check if the Payment to be void [End]

*-  Lock the payment record
SELECT APPAYMNT
*-  use local function instead, with 'Retry', 'Cancel' buttons
lcLokedRecs = '' && save locked records in APINVHDR file

*B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
*IF gfObj_lock(.T.,.T.)
IF lfObj_lock(.T.,.T.)
*B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]
 
  SELECT APDIST
  SCAN REST WHILE APDIST.CAPDTRTYP+APDIST.CBNKCODE+;
                  APDIST.CCHKACCT +APDIST.CAPDREF = ;
                  APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+;
                  APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO;
              FOR APDIST.CAPDACTID = 'A' AND APDIST.CAPDSTAT <> 'V'
    SELECT APINVHDR
    *-  use local function instead, with 'Retry', 'Cancel' buttons
    IF !EMPTY(APINVHDR.COWNER) AND APINVHDR.COWNER = loFormSet.lcSequence

      *- Do nothing more, as the recored is already locked before

    ELSE
    *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
      *IF !gfObj_lock(.T.,.T.)
       IF !lfObj_lock(.T.,.T.)
      *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

        llObjLock = .T.
        EXIT
      ELSE
        REPLACE APINVHDR.COWNER WITH loFormSet.lcSequence
        *-  save locked records in APINVHDR file
        lcLokedRecs = lcLokedRecs + STR(RECNO('APINVHDR')) + '|'
      ENDIF
    ENDIF

    SELECT APDIST
  ENDSCAN

  IF llObjLock
    SELECT APPAYMNT
    IF !EOF()
      GO RECNO()
    ENDIF
    *-  UnLock the payment record
    *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
    *=gfObj_Lock(.F.)
     =lfObj_Lock(.F.)
    *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

    *-  end.

    SELECT APDIST
    SCAN REST WHILE APDIST.CAPDTRTYP+APDIST.CBNKCODE+;
                    APDIST.CCHKACCT +APDIST.CAPDREF = ;
                    APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+;
                    APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO;
                FOR APDIST.CAPDACTID = 'A'
      SELECT APINVHDR
      *-  Clear only APINVHDR records locked by this program
      IF STR(RECNO('APINVHDR')) $ lcLokedRecs
      *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
       * =gfObj_Lock(.F.)
       =lfObj_Lock(.F.)
        *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

        REPLACE APINVHDR.COWNER WITH ''
      ENDIF

      SELECT APDIST
    ENDSCAN
    llObjLock = .F.
    RETURN .F.
  ENDIF

  SELECT APPAYMNT
  IF !EOF()
    GO RECNO()
  ENDIF

  *-  Scan through APDIST file for all invoices that may have
  *-  been applied by the current debit memo.
  *-  An application record has cApDTrTyp = 'A'
  *-  Save environment
  SELECT APDIST
  lnApDistTag = VAL(SYS(21))
  *-  Store the vendor code and invoice number.
  lcVdVend   = APDIST.cVendCode
  lcVdDM     = APDIST.cInvNo

  *-  INVVEND Tag expression is : CINVNO+CVENDCODE+CAPDTRTYP
  *-  Scan only negative records.
  SET ORDER TO TAG INVVEND
  IF SEEK(lcVdDM + lcVdVend + 'A')
    *-  save locked records of APINVHDR file
    SCAN REST WHILE   cInvNo +   cVendCode + cAPDTrTyp = lcVdDM + lcVdVend + 'A';
              FOR nAPDAmnt < 0 .AND. cAPDActID = 'A'
      IF SEEK(APDIST.cAPDRef + lcVdVend, 'APINVHDR')
        SELECT APINVHDR
        *-  use local function instead, with 'Retry', 'Cancel' buttons
        IF !EMPTY(APINVHDR.COWNER) AND APINVHDR.COWNER = loFormSet.lcSequence

          *- Do nothing more, as the recored is already locked before

        ELSE
 *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
        * IF !gfObj_lock(.T.,.T.)
          IF !lfObj_lock(.T.,.T.)
           *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

            llObjLock = .T.
            EXIT
          ELSE
            REPLACE APINVHDR.COWNER WITH loFormSet.lcSequence
            *-  save locked records of the APINVHDR file
            lcLokedRecs = lcLokedRecs + STR(RECNO('APINVHDR')) + '|'
          ENDIF
        ENDIF
        SELECT APDIST
      ENDIF
    ENDSCAN

    *-  If at least one record is not locked, unlock all records.
    IF llObjLock .AND. SEEK(lcVdDM + lcVdVend + 'A')
      SCAN REST WHILE   cInvNo +   cVendCode + cAPDTrTyp = lcVdDM + lcVdVend + 'A';
       FOR nAPDAmnt < 0 .AND. cAPDActID = 'A'
        IF SEEK(APDIST.cAPDRef + lcVdVend, 'APINVHDR')
          SELECT APINVHDR
          *-  Clear only records locked by this screen , not locked by other screens
          IF STR(RECNO('APINVHDR')) $ lcLokedRecs
          *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
            *=gfObj_Lock(.F.)
              =lfObj_Lock(.F.)
            *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]
        
            REPLACE APINVHDR.COWNER WITH ''
          ENDIF
          SELECT APDIST
        ENDIF
      ENDSCAN

      *-  UnLock the payment record
      SELECT APPAYMNT
                *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
     * =gfObj_Lock(.F.)
     =lfObj_Lock(.F.)
                *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

      RETURN .F.
    ENDIF
  ENDIF
  *-  Restore environment
  SELECT APDIST
  SET ORDER TO (lnApDistTag)

  *-  Refresh relation
  SELECT APPAYMNT
  IF !EOF()
    GO RECNO()
  ENDIF

  SELECT &lcAPPAYMNT
  REPLACE LVOIDED WITH .T. ;
          MINVLK  WITH lcLokedRecs
  =lfPaySkip(lcAPPAYMNT)

  loFormSet.Ariaform1.ldVoidDate.Text1.Enabled = .F.
ENDIF
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
ELSE
  =gfSeek(PADR(&lcAPPAYMNT..cPayDocNo,12)+&lcAPPAYMNT..cpayclno,'APINVHDR','INVVEND') 
  lcLokedRecs = ''
  SELECT APINVHDR
  *-  use local function instead, with 'Retry', 'Cancel' buttons
  IF !EMPTY(APINVHDR.COWNER) AND APINVHDR.COWNER = loFormSet.lcSequence
  ELSE
                  *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
    *IF !gfObj_lock(.T.,.T.)
     IF !lfObj_lock(.T.,.T.)
                    *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

      llObjLock = .T.
      RETURN .F.
    ELSE
      REPLACE APINVHDR.COWNER WITH loFormSet.lcSequence
      *-  save locked records in APINVHDR file
      lcLokedRecs = lcLokedRecs + STR(RECNO('APINVHDR')) + '|'
    ENDIF
  ENDIF
  IF !llObjLock
    SELECT APDIST
    lnApDistTag = ORDER()
    gfSetOrder('INVVEND')
    =gfSeek(PADR(EVALUATE(loFormSet.lcAPPAYMNT+".CPAYDOCNO"),12)+EVALUATE(loFormSet.lcAPPAYMNT+".CPAYCLNO")+'A')
    SCAN REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = PADR(EVALUATE(loFormSet.lcAPPAYMNT+".CPAYDOCNO"),12)+EVALUATE(loFormSet.lcAPPAYMNT+".CPAYCLNO")+'A' FOR ;
 				    DAPDTRDAT = EVALUATE(loFormSet.lcAPPAYMNT+".DPAYDATE") AND CBNKCODE = EVALUATE(loFormSet.lcAPPAYMNT+".CBNKCODE") AND cApDStat <> 'V' ;
					AND nAPDAmnt < 0 and CCHKACCT  = EVALUATE(loFormSet.lcAPPAYMNT+".CCHKACCT") AND !EMPTY(CAPDREF) AND;
					gfSeek(CAPDREF+CVENDCODE,'APINVHDR','INVVEND') AND APINVHDR.nInvAmnt > 0 

	    IF !EMPTY(APINVHDR.COWNER) AND APINVHDR.COWNER = loFormSet.lcSequence   
      ELSE
        SELECT APINVHDR
                          *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
       * IF !gfObj_lock(.T.,.T.)
       IF !lfObj_lock(.T.,.T.)
                          *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

          llObjLock = .T.
          EXIT
        ELSE
          REPLACE APINVHDR.COWNER WITH loFormSet.lcSequence
          *-  save locked records in APINVHDR file
          lcLokedRecs = lcLokedRecs + STR(RECNO('APINVHDR')) + '|'
        ENDIF
      ENDIF
    ENDSCAN  
    IF llObjLock 
      SELECT APINVHDR
      =gfSeek(PADR(&lcAPPAYMNT..cPayDocNo,12)+&lcAPPAYMNT..cpayclno,'APINVHDR','INVVEND') 
                                *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
    *  =gfObj_Lock(.F.)
    =lfObj_Lock(.F.)
                                *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

      SELECT APDIST
      =gfSeek(PADR(EVALUATE(loFormSet.lcAPPAYMNT+".CPAYDOCNO"),12)+EVALUATE(loFormSet.lcAPPAYMNT+".CPAYCLNO")+'A')
      SCAN REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = PADR(EVALUATE(loFormSet.lcAPPAYMNT+".CPAYDOCNO"),12)+EVALUATE(loFormSet.lcAPPAYMNT+".CPAYCLNO")+'A' FOR ;
			  DAPDTRDAT = EVALUATE(loFormSet.lcAPPAYMNT+".DPAYDATE") AND CBNKCODE = EVALUATE(loFormSet.lcAPPAYMNT+".CBNKCODE") AND cApDStat <> 'V' ;
			  AND nAPDAmnt < 0 and CCHKACCT  = EVALUATE(loFormSet.lcAPPAYMNT+".CCHKACCT") AND !EMPTY(CAPDREF) AND ;
			  gfSeek(CAPDREF+CVENDCODE,'APINVHDR','INVVEND') AND APINVHDR.nInvAmnt > 0 
	      IF STR(RECNO('APINVHDR')) $ lcLokedRecs
          SELECT APINVHDR
                                          *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]

         * =gfObj_Lock(.F.)
         =lfObj_Lock(.F.)
                                          *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

          REPLACE APINVHDR.COWNER WITH ''
        ENDIF
      ENDSCAN  
    ENDIF
  ENDIF
  SELECT APDIST
  SET ORDER TO (lnApDistTag)

  IF llObjLock
   RETURN .F.
  ENDIF
  SELECT &lcAPPAYMNT
  REPLACE LVOIDED WITH .T. ;
          MINVLK  WITH lcLokedRecs
  =lfPaySkip(lcAPPAYMNT)
  loFormSet.Ariaform1.ldVoidDate.Text1.Enabled = .F.
ENDIF
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
RETURN
*- End of lfvVoid .

************************************************************************
*Name       : lfPaySkip
*Developer  : TMI
*Date       : 01/26/2012
*Purpose    : skip in lcAPPAYMNT filt
************************************************************************
FUNCTION lfPaySkip
PARAMETERS lcAPPAYMNT
lnSlct = SELECT(0)

SELECT &lcAPPAYMNT
  SKIP
  IF EOF()
    GO BOTTOM
    IF EOF()
      loFormSet.Ariaform1.cmdVoid.Enabled = .F.
    ENDIF
  ENDIF

SELECT (lnSlct )
 *- End of lfPaySkip.

*!*************************************************************
*! Name      : lfPayment
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/24/2012
*! Purpose   : Function to check if the advance payment has already
*!             been paid
*!*************************************************************
*! Called from : lfvVoid()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return     : .T. If the advance payment has already been paid.
*!              .F. Otherwise.
*!*************************************************************
FUNCTION lfPayment

PRIVATE lcInvKey , lcSavOrd , llReturn

llReturn = .F.                                                && Variable to hold the value to be returned
lcInvKey = APINVHDR.cInvNo + APINVHDR.cVendCode               && Variable to hold the search key

SELECT APDIST
lcSavOrd = IIF(EMPTY(ORDER()) , '' , 'TAG ') + ORDER()        && Save the current order tag
SET ORDER TO TAG INVVEND

*--IF Statement to place the record pointer in the first record for the
*--advance payment debit memo in the APDIST file
IF SEEK(lcInvKey)
  LOCATE REST;
        WHILE cInvNo + cVendCode = lcInvKey ;
          FOR cApdActId = 'C' .AND. nApdAmnt > 0

  llReturn = FOUND()
ENDIF    && End of IF SEEK(lcInvKey)

SET ORDER TO &lcSavOrd

*--Refresh the relation
SELECT APPAYMNT

*--If the curent record is valid [not end of file]
IF !EOF()
  GO RECNO()
ENDIF    && End of IF !EOF()

RETURN llReturn
*- End of lfPayment.

*!*************************************************************
*! Name      : lfvVoidDate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/24/2012
*! Purpose   : Valid function for ldVoidDate field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvVoidDate()
*!*************************************************************
FUNCTION lfvVoidDate
PARAMETERS loFormSet

ldVoidDate = loFormSet.Ariaform1.ldVoidDate.Text1.Value
IF EMPTY(ldVoidDate)
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *=gfModalGen('TRM04074B00000', 'DIALOG', 'Voiding Date')  
  =gfModalGen('TRM04074B00000', 'DIALOG', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_VOIDINGDATE,loFormSet.GetHeaderText("LANG_APVDPAY_VOIDINGDATE",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  loFormSet.Ariaform1.ldVoidDate.Text1.Value = loFormSet.Ariaform1.ldVoidDate.Text1.OldValue
  RETURN .F.
ENDIF
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*STORE 'void' TO lcTVoid
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_VOID,loFormSet.GetHeaderText("LANG_APVDPAY_VOID",loFormSet.HeaderAlias)) TO lcTVoid
*N000682,1 MMT 12/09/2012 Globalization changes[END]
llLockPrd = .F.
STORE ' ' TO lcFiscalP, lcFiscalY
IF !lfVlDate(oAriaApplication.prntcompanyid, @lcFiscalP, @lcFiscalY , ldVoidDate , @llLockPrd)
  =gfModalGen('TRM04113B00000', 'DIALOG', lcTVoid)
  loFormSet.Ariaform1.ldVoidDate.Text1.Value = loFormSet.Ariaform1.ldVoidDate.Text1.OldValue
  RETURN .F.
ENDIF
IF llLockPrd
  =gfModalGen("QRM01258B00000" , "DIALOG" )
  loFormSet.Ariaform1.ldVoidDate.Text1.Value = loFormSet.Ariaform1.ldVoidDate.Text1.OldValue
  RETURN .F.
ENDIF
ldOldDate = ldVoidDate
RETURN .T.

****************************************************************************************************
*Name       : lfUndoLocks
*Developer  : TMI
*Date       : 01/24/2012
*Purpose    : Remove locks when the user issues the undo command
****************************************************************************************************
FUNCTION lfUndoLocks
PARAMETERS loFormSet

LOCAL lnSlct,lcLokedRecs,lnI,lnPipe,lnCnt
lnSlct = SELECT(0)
lcAPPAYMNT = loFormSet.lcAPPAYMNT
SELECT &lcAPPAYMNT
SET FILTER TO
LOCATE



*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
*SCAN FOR LVOIDED
*=SEEK(CPAYTYPE+CPAYMETH+CPAYDOCNO+CBNKCODE+CCHKACCT,'APPAYMNT','TYPMETHDOC')
SCAN FOR LVOIDED AND CPAYMETH <> 'D'
  =SEEK(CPAYTYPE+CPAYMETH+SUBSTR(CPAYDOCNO,1,8)+CBNKCODE+CCHKACCT,'APPAYMNT','TYPMETHDOC')
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
  
  SELECT APPAYMNT
                                            *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
 * =gfObj_Lock(.F.)
 =lfObj_Lock(.F.)
                                          *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

  lcLokedRecs = &lcAPPAYMNT..MINVLK
  lnCnt = OCCURS('|',lcLokedRecs)
  FOR lnI = 1 TO lnCnt
    *- unlock apinvhdr records
    lnPipe = AT('|',lcLokedRecs)
    lcRecno = INT(VAL(SUBSTR(lcLokedRecs,1,lnPipe-1)))
    SELECT APINVHDR
    GO (lcRecno)
                                                *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]

   * =gfObj_Lock(.F.)
   =lfObj_Lock(.F.)
                                                *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

    REPLACE COWNER WITH ''
    lcLokedRecs = SUBSTR(lcLokedRecs,lnPipe+1)
  ENDFOR
ENDSCAN
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
SCAN FOR LVOIDED AND CPAYMETH = 'D'
  =gfSeek(PADR(&lcAPPAYMNT..cPayDocNo,12)+&lcAPPAYMNT..cpayclno,'APINVHDR','INVVEND') 
  SELECT APINVHDR
   *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
  *=gfObj_Lock(.F.)
  =lfObj_Lock(.F.)
 *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

  REPLACE COWNER WITH ''
  
  SELECT APDIST
  lnApDistTag = ORDER()
  gfSetOrder('INVVEND')
  =gfSeek(PADR(EVALUATE(loFormSet.lcAPPAYMNT+".CPAYDOCNO"),12)+EVALUATE(loFormSet.lcAPPAYMNT+".CPAYCLNO")+'A')
  SCAN REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = PADR(EVALUATE(loFormSet.lcAPPAYMNT+".CPAYDOCNO"),12)+EVALUATE(loFormSet.lcAPPAYMNT+".CPAYCLNO")+'A' FOR ;
            DAPDTRDAT = EVALUATE(loFormSet.lcAPPAYMNT+".DPAYDATE") AND CBNKCODE = EVALUATE(loFormSet.lcAPPAYMNT+".CBNKCODE") AND cApDStat <> 'V' ;
            AND nAPDAmnt < 0 and CCHKACCT  = EVALUATE(loFormSet.lcAPPAYMNT+".CCHKACCT") AND !EMPTY(CAPDREF) AND;
            gfSeek(CAPDREF+CVENDCODE,'APINVHDR','INVVEND') AND APINVHDR.nInvAmnt > 0 
    SELECT APINVHDR
       *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
    *=gfObj_Lock(.F.)
    =lfObj_Lock(.F.)
       *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

    REPLACE COWNER WITH ''
  ENDSCAN
  SELECT APDIST
  SET ORDER TO (lnApDistTag)
ENDSCAN
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]


SELECT (lnSlct )
*- End of lfUndoLocks.

*!**************************************************************************
*Name       : lfvInvoice
*Developer  : TMI
*Date       : 01/27/2012
*Purpose    : Call the invoices screen
*!**************************************************************************
FUNCTION lfvInvoice
PARAMETERS loFormSet
LOCAL lnSlct,lcTempInv
lnSlct = SELECT(0)

lcInvoice = ' '
lcTempInv = loFormSet.lcTempInv
SELECT(lcTempInv)
ZAP

SELECT APPAYMNT
LOCATE    && just refresh

SELECT (loFormSet.lcAPPAYMNT)
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
IF EVALUATE(loFormSet.lcAPPAYMNT+".CPAYMETH") ='D'
  IF !USED('APDIST_DET')
    =gfOpenTable('APDIST','INVVEND','SH','APDIST_DET')
  ENDIF
  
  IF !USED('APINVHDR_DET')
    =gfOpenTable('APINVHDR','INVVEND','SH','APINVHDR_DET')
  ENDIF

  SELECT APDIST_DET
  =gfSeek(PADR(EVALUATE(loFormSet.lcAPPAYMNT+".CPAYDOCNO"),12)+EVALUATE(loFormSet.lcAPPAYMNT+".CPAYCLNO")+'A')
  SCAN REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = PADR(EVALUATE(loFormSet.lcAPPAYMNT+".CPAYDOCNO"),12)+EVALUATE(loFormSet.lcAPPAYMNT+".CPAYCLNO")+'A' FOR ;
				  DAPDTRDAT = EVALUATE(loFormSet.lcAPPAYMNT+".DPAYDATE") AND CBNKCODE = EVALUATE(loFormSet.lcAPPAYMNT+".CBNKCODE") AND cApDStat <> 'V' AND;
				  CCHKACCT  = EVALUATE(loFormSet.lcAPPAYMNT+".CCHKACCT") AND !EMPTY(CAPDREF) AND ;
				  gfSeek(CAPDREF+CVENDCODE,'APINVHDR_DET','INVVEND') AND APINVHDR_DET.nInvAmnt > 0 
				  
				  
    IF lcInvoice <> APDIST_DET.CAPDREF
      SELECT APINVHDR_DET
      SCATTER MEMVAR MEMO
      m.NADJUST  = ABS(APDIST_DET.NAPDAMNT)
      SELECT(lcTempInv)
      APPEND BLANK
      GATHER MEMVAR MEMO
      lcInvoice = APDIST_DET.CAPDREF
    ENDIF
    SELECT(lcTempInv)
    DO CASE
      CASE APDIST_DET.CAPDACTID = 'A'
        REPLACE NTOTAL  WITH ABS(APDIST_DET.NAPDAMNT)
	    
      CASE APDIST_DET.CAPDACTID = 'B'
        REPLACE N1099   WITH APDIST_DET.NAPDAMNT

      CASE APDIST_DET.CAPDACTID = 'C'
        REPLACE NPAID   WITH APDIST_DET.NAPDAMNT

      CASE APDIST_DET.CAPDACTID = 'J'
        REPLACE NADJUST WITH  APDIST_DET.NAPDAMNT

      CASE APDIST.CAPDACTID = 'S'
        REPLACE NDISCOUNT WITH APDIST_DET.NAPDAMNT
    ENDCASE
    SELECT APDIST_DET
  ENDSCAN
  lcPayment = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_APPLYDEBIT ,loFormSet.GetHeaderText("LANG_APVDPAY_APPLYDEBIT",loFormSet.HeaderAlias))
ELSE
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
*=SEEK(CPAYTYPE+CPAYMETH+CPAYDOCNO+CBNKCODE+CCHKACCT,'APPAYMNT','TYPMETHDOC')
=SEEK(CPAYTYPE+CPAYMETH+SUBSTR(CPAYDOCNO,1,8)+CBNKCODE+CCHKACCT,'APPAYMNT','TYPMETHDOC')
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
*** Refresh relation between payments file and AP distribution file
SELECT APPAYMNT
IF !EOF()
  GO RECNO()
ENDIF

SELECT APDIST

SCAN REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF = ;
                APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+  ;
                APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO
  IF lcInvoice <> APDIST.CINVNO
    SELECT APINVHDR
    SCATTER MEMVAR MEMO

    SELECT(lcTempInv)
    APPEND BLANK
    GATHER MEMVAR MEMO
    lcInvoice = APDIST.CINVNO
  ENDIF

  SELECT(lcTempInv)

  DO CASE
    CASE APDIST.CAPDACTID = 'A'
      REPLACE NTOTAL    WITH APDIST.NAPDAMNT
     
    CASE APDIST.CAPDACTID = 'B'
      REPLACE N1099     WITH APDIST.NAPDAMNT

    CASE APDIST.CAPDACTID = 'C'
      REPLACE NPAID     WITH APDIST.NAPDAMNT

    CASE APDIST.CAPDACTID = 'J'
      REPLACE NADJUST   WITH APDIST.NAPDAMNT

    CASE APDIST.CAPDACTID = 'S'
      REPLACE NDISCOUNT WITH APDIST.NAPDAMNT
  ENDCASE
  SELECT APDIST
ENDSCAN

DO CASE
  CASE APPAYMNT.CPAYMETH = 'P'
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *lcPayment = 'Printed checks'
    lcPayment = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_PTINTEDCHECK,loFormSet.GetHeaderText("LANG_APVDPAY_PTINTEDCHECK",loFormSet.HeaderAlias))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
  CASE APPAYMNT.CPAYMETH = 'M'
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]  
    *lcPayment = 'Manual checks'
    lcPayment = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_MANULCHECK,loFormSet.GetHeaderText("LANG_APVDPAY_MANULCHECK",loFormSet.HeaderAlias))
    *N000682,1 MMT 12/09/2012 Globalization changes[ENd]
  CASE APPAYMNT.CPAYMETH = 'N'
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]  
    *lcPayment = 'Non check payments'
    lcPayment = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_NONCHECK,loFormSet.GetHeaderText("LANG_APVDPAY_NONCHECK",loFormSet.HeaderAlias))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
  OTHERWISE
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]  
    *lcPayment = 'Cash payment'
    lcPayment = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_CASHPAY,loFormSet.GetHeaderText("LANG_APVDPAY_CASHPAY",loFormSet.HeaderAlias))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]    
ENDCASE
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][Start]
ENDIF
*E303661,1 MMT 04/17/2016 Add 'Apply Debit' Payment method to Void payment program[T20160217.0022][End]
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*lcInvTtl = 'Invoice paid by '+lcPayment+' number '+APPAYMNT.CPAYDOCNO
lcInvTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_INVPAYBY,loFormSet.GetHeaderText("LANG_APVDPAY_INVPAYBY",loFormSet.HeaderAlias))+' '+;
           lcPayment+' '+;
           IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_NUMBER,loFormSet.GetHeaderText("LANG_APVDPAY_NUMBER",loFormSet.HeaderAlias))+' '+;
           APPAYMNT.CPAYDOCNO
*N000682,1 MMT 12/09/2012 Globalization changes[END]
lcInvTtl = ALLTRIM(lcInvTtl)

SELECT(lcTempInv)
GO TOP

DO FORM (oAriaApplication.ScreenHome+'\AP\APINVLST.scx') WITH loFormSet

SELECT (lnSlct)
*- End of lfvInvoice.

****************************************************************************************************
*Name       : lfAPINVLSTInit
*Developer  : TMI
*Date       : 01/27/2012
*Purpose    : initiate the APINVLST form
****************************************************************************************************
FUNCTION lfAPINVLSTInit
PARAMETERS loAPINVLST
loFormSet = loAPINVLST.loCallingForm

WITH loAPINVLST.ariaform1.Ariagrid1
  lcTempInv = loFormSet.lcTempInv
  LOCAL lnI,lcI
  lnI = 0
  oGrid = loAPINVLST.ariaform1.Ariagrid1
  oGrid.RecordSource = lcTempInv
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CINVNO','Inv. no.',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..DINVDATE','Inv. date',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CINVREF','Reference',75)
*!*	  =lfAddColumn(@lnI,oGrid,"IIF(CINVREMIT = 'V','Vendor',IIF(CINVREMIT = 'F','Factor','Other'))",'Remit',75)
*!*	  =lfAddColumn(@lnI,oGrid,"IIF(CVENPMETH = 'P','Printed checks',IIF(CVENPMETH = 'M','Manual checks',IIF(CVENPMETH = 'N','Non check payments','Cash payment')))",;
*!*	                        'Payment method',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVDISOF','Disc. Offerd',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVAMTAP','Appr. to pay',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVDISAP','Disc. appr.',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVADJAP','Adj. appr.',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CVENPRIOR','Payment priority',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NTERDUED','Net due days',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NTERDISCD','Disc. days',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NTERDISCR','Disc. percent',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..DINVDUDAT','Due date',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CBNKCODE','Bank code',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CCHKACCT','Bank checking account',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CCHKGLACC','GL checking account',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CCHKNO   ','Check number',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..DCHKDATE ','Check date',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CVENCCVEN','Credit card vendor',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CVENCCINV','Credit card invoice',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CAPACCT  ','AP account',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NTOTAL   ','Total',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..N1099',IIF(loFormSet.ap1.llApS1099,'','1099 amount'),IIF(loFormSet.ap1.llApS1099,75,0))
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NPAID    ','Amount paid',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NADJUST  ','Adj. applied',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NDISCOUNT','Disc. taken',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVPAID ','Total paid',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVDISTK','Total Discount',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVADJ  ','Total Adjustment',75)
*!*	  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINV1099A',IIF(loFormSet.ap1.llApS1099,'','Total 1099 amount'),IIF(loFormSet.ap1.llApS1099,75,0))
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CINVNO',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_INVNO,loFormSet.GetHeaderText("LANG_APVDPAY_INVNO",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..DINVDATE',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_INVDATE,loFormSet.GetHeaderText("LANG_APVDPAY_INVDATE",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CINVREF',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_REFRENCE,loFormSet.GetHeaderText("LANG_APVDPAY_REFRENCE",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,"IIF(CINVREMIT = 'V','"+;
                             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_Vendor,loFormSet.GetHeaderText("LANG_APVDPAY_Vendor",loFormSet.HeaderAlias))+"',"+;
                             "IIF(CINVREMIT = 'F','"+;
                             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_FACTOR,loFormSet.GetHeaderText("LANG_APVDPAY_FACTOR",loFormSet.HeaderAlias))+"','"+;
                             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_OTHER,loFormSet.GetHeaderText("LANG_APVDPAY_OTHER",loFormSet.HeaderAlias))+"'))",;
                             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_REMIT,loFormSet.GetHeaderText("LANG_APVDPAY_REMIT",loFormSet.HeaderAlias)),75)
                             
  =lfAddColumn(@lnI,oGrid,"IIF(CVENPMETH = 'P','"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_PTINTEDCHECK,loFormSet.GetHeaderText("LANG_APVDPAY_PTINTEDCHECK",loFormSet.HeaderAlias))+"',"+;
                       "IIF(CVENPMETH = 'M','"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_MANULCHECK,loFormSet.GetHeaderText("LANG_APVDPAY_MANULCHECK",loFormSet.HeaderAlias))+"',"+;
                       "IIF(CVENPMETH = 'N','"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_NONCHECK,loFormSet.GetHeaderText("LANG_APVDPAY_NONCHECK",loFormSet.HeaderAlias))+"','"+;
                       IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_CASHPAY,loFormSet.GetHeaderText("LANG_APVDPAY_CASHPAY",loFormSet.HeaderAlias))+"')))",;
                        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_PAYMTH,loFormSet.GetHeaderText("LANG_APVDPAY_PAYMTH",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVDISOF',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_DISCOFFER,loFormSet.GetHeaderText("LANG_APVDPAY_DISCOFFER",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVAMTAP',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_APPRPAY,loFormSet.GetHeaderText("LANG_APVDPAY_APPRPAY",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVDISAP',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_DISCAPPR,loFormSet.GetHeaderText("LANG_APVDPAY_DISCAPPR",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVADJAP',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_ADJAPP,loFormSet.GetHeaderText("LANG_APVDPAY_ADJAPP",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CVENPRIOR',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_PAYPRI,loFormSet.GetHeaderText("LANG_APVDPAY_PAYPRI",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NTERDUED',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_NETDUE,loFormSet.GetHeaderText("LANG_APVDPAY_NETDUE",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NTERDISCD',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_DISCDAY,loFormSet.GetHeaderText("LANG_APVDPAY_DISCDAY",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NTERDISCR',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_DISCPER,loFormSet.GetHeaderText("LANG_APVDPAY_DISCPER",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..DINVDUDAT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_DUEDATE,loFormSet.GetHeaderText("LANG_APVDPAY_DUEDATE",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CBNKCODE',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_BANKCODE,loFormSet.GetHeaderText("LANG_APVDPAY_BANKCODE",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CCHKACCT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_CHECKACC,loFormSet.GetHeaderText("LANG_APVDPAY_CHECKACC",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CCHKGLACC',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_GLCHECK,loFormSet.GetHeaderText("LANG_APVDPAY_GLCHECK",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CCHKNO   ',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_CHECKNUM,loFormSet.GetHeaderText("LANG_APVDPAY_CHECKNUM",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..DCHKDATE ',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_CHECKDATE,loFormSet.GetHeaderText("LANG_APVDPAY_CHECKDATE",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CVENCCVEN',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_CREDITCARDVEND,loFormSet.GetHeaderText("LANG_APVDPAY_CREDITCARDVEND",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CVENCCINV',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_CREDITCARDINV,loFormSet.GetHeaderText("LANG_APVDPAY_CREDITCARDINV",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..CAPACCT  ',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_APACC,loFormSet.GetHeaderText("LANG_APVDPAY_APACC",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NTOTAL   ',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_TOTAL,loFormSet.GetHeaderText("LANG_APVDPAY_TOTAL",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..N1099',IIF(loFormSet.ap1.llApS1099,'',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_1099AMNT,loFormSet.GetHeaderText("LANG_APVDPAY_1099AMNT",loFormSet.HeaderAlias))),IIF(loFormSet.ap1.llApS1099,75,0))
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NPAID    ',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_AMNTPAD,loFormSet.GetHeaderText("LANG_APVDPAY_AMNTPAD",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NADJUST  ',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_ADJAPPL,loFormSet.GetHeaderText("LANG_APVDPAY_ADJAPPL",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NDISCOUNT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_DISCTAKE,loFormSet.GetHeaderText("LANG_APVDPAY_DISCTAKE",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVPAID ',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_TOTPAID,loFormSet.GetHeaderText("LANG_APVDPAY_TOTPAID",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVDISTK',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_TOTDISC,loFormSet.GetHeaderText("LANG_APVDPAY_TOTDISC",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINVADJ  ',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_TOTADJ,loFormSet.GetHeaderText("LANG_APVDPAY_TOTADJ",loFormSet.HeaderAlias)),75)
  =lfAddColumn(@lnI,oGrid,'&lcTempInv..NINV1099A',IIF(loFormSet.ap1.llApS1099,'',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APVDPAY_TOT1099,loFormSet.GetHeaderText("LANG_APVDPAY_TOT1099",loFormSet.HeaderAlias))),IIF(loFormSet.ap1.llApS1099,75,0))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  .READONLY = .T.
  .Refresh()
ENDWITH
 *- End of lfAPINVLSTInit.

********************************************************************************
*Name       : lfAfterRowColChINVLST
*Developer  : TMI
*Date       : 01/27/2012
*Purpose    : lfAfterRowColChINVLST
********************************************************************************
FUNCTION lfAfterRowColChINVLST
PARAMETERS loAPINVLST
LOCAL loFormSet ,lcTempInv
loFormSet = loAPINVLST.loCallingForm
lcTempInv = loFormSet.lcTempInv


WITH loAPINVLST.ariaform1
  .cinvno.Value = &lcTempInv..cinvno
  .ninvamnt.Value = &lcTempInv..ninvamnt

  .Advance.Value = IIF(APPAYMNT.LPAYADVAN,'Advance payment',' ')

  .npaid.Value = &lcTempInv..npaid
  .ninvpaid.Value = &lcTempInv..ninvpaid

  .ndiscount.Value = &lcTempInv..ndiscount
  .ninvdistk.Value = &lcTempInv..ninvdistk

  .nadjust.Value = &lcTempInv..nadjust
  .ninvadj.Value = &lcTempInv..ninvadj

  .ntotal.Value = &lcTempInv..ntotal
  .ntotal2.Value = &lcTempInv..ninvpaid+&lcTempInv..ninvdistk+&lcTempInv..ninvadj

  .n1099.Value = &lcTempInv..n1099
  .ninv1099a.Value = &lcTempInv..ninv1099a
ENDWITH
*- End of lfAfterRowColChINVLST.

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
       *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [Begin]
*FUNCTION gfObj_Lock
FUNCTION lfObj_Lock
       *B611582,1 ES 05/29/2018  modify the programs to rename the function gfObj_Lock and its usages  to be different than the global function name [End]

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
************************************************************
*! Name      : lfGetPeriod
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/19/2014
*! Purpose   : Get the period of the passed date
*E303442,1 
************************************************************
FUNCTION lfGetPeriod
PARAMETERS ldPDate
LOCAL lnSlct,llOpenPrd,lcPrd
lnSlct = SELECT(0)
lcPrd = '  '
SELECT 0 

llOpenPrd = .F.
IF !USED('FSPRD')   && Check if the period file is open or not.
  llOpenPrd = .T.      && Indicates that the file is open by the function.
  =gfOpenTable('fsprd','COMFYRPRDI','SH')
ENDIF

SELECT FSPRD
LOCATE REST FOR BETWEEN(ldPDate,dfsppbgdt,dfsppendt)
IF FOUND()
  lcPrd = FSPRD.CFSPPRDID
ENDIF 

IF llOpenPrd
  =gfCloseTable('FSPRD')
ENDIF 

SELECT (lnSlct)
RETURN lcPrd 
*- End of lfGetPeriod.