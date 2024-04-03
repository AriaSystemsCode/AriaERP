*:************************************************************************
*:  Program File: ARIA4XP\PRGS\GL\GLTPOST.PRG
*:  Module      : General Ledger
*:  Desc.       : posting -> Single Transaction
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 05/15/2012
*:  Reference   : *E303151,1
*:************************************************************************
* Modifications
*B610328,1 TMI 05/09/2013 Fix a problem that the end date does not chagne [T20130415.0049] 
*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003]
*B612070,1 MMT 02/26/2020 Fix bug of GLTRNSHD status field is not updated when batch is voided[T20200219.0009]
*:************************************************************************
*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][Start]
LPARAMETERS lcBatchNo
*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][End]
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
#INCLUDE R:\ARIA4XP\PRGS\gl\GLBATCH.H
*N000682,1 MMT 11/22/2012 Globalization changes[End]
*- Get the screen , call it
lcRunScx = lfGetScx("GL\GLBATCH.scx")
*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][Start]
*DO FORM (lcRunScx)
DO FORM (lcRunScx) WITH .F.,lcBatchNo
*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][End]
************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
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
*! Date      : 04/05/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

loFormSet.AddProperty('lcProgName','GLBATCH')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE

*- Open tables
=lfOpenPRGFILES(loFormSet.lcProgName)

IF !lfGL(loFormset)
  RETURN .F.
ENDIF

*** Load program base file
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "CBATCHNO"
  .cBrowseIndexFields     = "CBATCHNO"
  .cBrowseIndexName       = "BATCHNO"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  *N000682,1 12/20/12 TMI Globlization changes[Start]   
*  .BrowseTitle 		  	    = loFormSet.lcFile_Ttl
  .BrowseTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBATCH_CAPTION,loFormSet.GetHeaderText("LANG_GLBATCH_CAPTION",loFormSet.HeaderAlias))
  *N000682,1 12/20/12 TMI Globlization changes[End  ] 
  
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	  .ariaBrFields.edtBrowseFields.Value = "CBATCHNO  :H='GL Batch number',"+;
*!*	                                        "CBATSTAT  :H='Batch status',"+;
*!*	                                        "CBATTYPE  :H='Batch Type',"+;
*!*	                                        "CBATPYR   :H='Posting year',"+;
*!*	                                        "DBATPBEG  :H='Batch posting begin date',"+;
*!*	                                        "DBATPEND  :H='Batch posting end date',"+;
*!*	                                        "CBATREFER :H='Batch reference',"+;
*!*	                                        "CBATDESC  :H='Batch description',"+;
*!*	                                        "NBATCNTOT :H='Batch control total',"+;
*!*	                                        "NBATOTDR  :H='Total debits',"+;
*!*	                                        "NBATOTCR  :H='Total credits',"+;
*!*	                                        "CSRCMODUL :H='Source module',"+;
*!*	                                        "CCOMP_ID  :H='Company ID'"
  .ariaBrFields.edtBrowseFields.Value = "CBATCHNO  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBATCHNO,loFormSet.GetHeaderText("LANG_GLBATCHNO",loFormSet.HeaderAlias))+"',"+;
                                        "CBATSTAT  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCHSTATUS,loFormSet.GetHeaderText("LANG_BATCHSTATUS",loFormSet.HeaderAlias))+"',"+;
                                        "CBATTYPE  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCHTYPE,loFormSet.GetHeaderText("LANG_BATCHTYPE",loFormSet.HeaderAlias))+"',"+;
                                        "CBATPYR   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTINGYEAR,loFormSet.GetHeaderText("LANG_POSTINGYEAR",loFormSet.HeaderAlias))+"',"+;
                                        "DBATPBEG  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCHPOSTBEGINDATE,loFormSet.GetHeaderText("LANG_BATCHPOSTBEGINDATE",loFormSet.HeaderAlias))+"',"+;
                                        "DBATPEND  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCHPOSTENDDATE,loFormSet.GetHeaderText("LANG_BATCHPOSTENDDATE",loFormSet.HeaderAlias))+"',"+;
                                        "CBATREFER :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCHREFERENCE,loFormSet.GetHeaderText("LANG_BATCHREFERENCE",loFormSet.HeaderAlias))+"',"+;
                                        "CBATDESC  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCHDESC,loFormSet.GetHeaderText("LANG_BATCHDESC",loFormSet.HeaderAlias))+"',"+;
                                        "NBATCNTOT :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCHCONTROL,loFormSet.GetHeaderText("LANG_BATCHCONTROL",loFormSet.HeaderAlias)) +"',"+;
                                        "NBATOTDR  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TOTALSDEBIT,loFormSet.GetHeaderText("LANG_TOTALSDEBIT",loFormSet.HeaderAlias))+"',"+;
                                        "NBATOTCR  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TOTALSCREDIT,loFormSet.GetHeaderText("LANG_TOTALSCREDIT",loFormSet.HeaderAlias)) +"',"+;
                                        "CSRCMODUL :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOURCEMODULE,loFormSet.GetHeaderText("LANG_SOURCEMODULE",loFormSet.HeaderAlias))+"',"+;
"CCOMP_ID  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPANYID,loFormSet.GetHeaderText("LANG_COMPANYID",loFormSet.HeaderAlias)) +"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]
  *N000682,1 MMT 11/22/2012 Globalization changes[end]  
ENDWITH

*N000682,1 12/17/12 TMI Globlization changes[Start] LANG_GLBATCH_CAPTION 
loFormSet.Ariaform1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBATCH_CAPTION,loFormSet.GetHeaderText("LANG_GLBATCH_CAPTION",loFormSet.HeaderAlias))
*N000682,1 12/17/12 TMI Globlization changes[End  ] 

*- Define needed variables.
=lfDefineVars(loFormSet)

SELECT GLTYPES
GO TOP
IF EOF()
  *** The types and ranges have not ***
  *** been setup yet.  You have to  ***
  *** define the accounts type and ranges first. ***
  *** < Ok > ***
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.
  RETURN .F.
ENDIF

*** check if the chart of accounts is created.
SELECT GLACCHAR
LOCATE
IF EOF()
  *** The chart of accounts is empty. You have to create. ***
  *** the chart of accounts first...
  *** <  Ok  > ***
  =gfModalGen("TRM02215B00000","DIALOG")
  glQuitting = .T.
  RETURN .F.
ENDIF


*- Set the control sources
=lfSetControlSource(loFormSet)

*- Start with Select Mode

loFormSet.ChangeMode('S')

lfStrEndPos(loFormSet)

*- Define input mask
WITH loFormSet.Ariaform1
  .laData1.KeyTextBox.InputMask = 'X'+REPLICATE('9',FSIZE('CBATCHNO','GLBATCH')-1)
  .laData7.InputMask = REPLICATE('9',FSIZE('NBATCNTOT','GLBATCH')-3)+'.99'
  .laData8.MaxLength = FSIZE('CBATREFER','GLBATCH')
  .laData9.MaxLength = FSIZE('CBATDESC' ,'GLBATCH')
  .laData10.InputMask = REPLICATE('9',FSIZE('NBATCNTOT','GLBATCH')-3)+'.99'
  .laData11.InputMask = REPLICATE('9',FSIZE('NBATCNTOT','GLBATCH')-3)+'.99'
  .lnBalance.InputMask = REPLICATE('9',FSIZE('NBATCNTOT','GLBATCH')-3)+'.99'
ENDWITH

*- End of lfFormInit.

************************************************************
*! Name      : lfStrEndPos
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/07/2012
*! Purpose   : lfStrEndPos
************************************************************
FUNCTION lfStrEndPos
PARAMETERS loFormSet
*- the start , end date positions
WITH loFormSet.Ariaform1
IF loFormSet.llDate
  && Posting by Date
  .lcStartPrd.Visible = .F.
  .lcEndPrd.Visible   = .F.
  .laData5.Left = .lcStartPrd.Left
  .laData6.Left = .lcStartPrd.Left
ELSE
  && Posting by Period
  .laData5.Enabled = .F.
  .laData6.Enabled = .F.
ENDIF
ENDWITH
*- End of lfStrEndPos.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/07/2012
*! Purpose   : fDefine the needed variaVars
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

loFormSet.AddProperty('laKeyField[1,4]','')
loFormSet.laKeyField[1,1] = 'laData[1]'
loFormSet.laKeyField[1,2] =.T.
loFormSet.laKeyField[1,3] = 'BATCHNO'
loFormSet.laKeyField[1,4] = 1

loFormSet.AddProperty('laTstatus[7,1]','')
loFormSet.laTStatus[1,1] = "Empty"
loFormSet.laTStatus[2,1] = "Out of Balance"
loFormSet.laTStatus[3,1] = "Unposted"
loFormSet.laTStatus[4,1] = "Posted"
loFormSet.laTStatus[5,1] = "Summarized"
loFormSet.laTStatus[6,1] = "Hold"
loFormSet.laTStatus[7,1] = "Approved"

loFormSet.AddProperty('laSelect[1,1]','')
loFormSet.AddProperty('laSelectFrm[1,1]','')
loFormSet.AddProperty('laSelectTO[1,1]','')
loFormSet.AddProperty('laAceptRec[1]','')
loFormSet.AddProperty('la_TPostYr[5]','')


*- Add Subledger type to the array
loFormSet.AddProperty('laBatType[3,2]','')
loFormSet.laBatType[1,1] = 'Normal'
loFormSet.laBatType[1,2] = 'N'
loFormSet.laBatType[2,1] = 'Statistical'
loFormSet.laBatType[2,2] = 'S'
loFormSet.laBatType[3,1] = 'Subledger' &&-- added due to B601514,1
loFormSet.laBatType[3,2] = 'L'         &&-- added due to B601514,1

** lcYear        :  Holds the selected batch year.
** lcDelMesag    :  Holds the prompt name insted of delete "void"
** lcPrompt      :
** lcOldStPrd    :  Holds the old start period.
** lcOldEnPrd    :  Holds the old end   period.
** lcStartPrd    :  Holds the     start period.
** lcEndPrd      :  Holds the     end   period.
** lcOldYear     :  Holds the old selected batch year.
** lcDumYear     :
** lcDumStart    :
** lcDummyEnd    :
** lcStatus      :  Holds the batch status.
** lcBalncid     :  Holds the batch balacne id 'CR' or 'DR'

** lcBatType     :  Holds the current batch type Normal,Statistical,Subledger

** lcScope       :  Holds the filter expression.
** lcBStamp      :  Holds the batch stamp (user+date+time).
** lcPeriodLock  :  Holds the locked period number.
** lcObjStatus   :
** laSelect      :  Initialize the array laSelect
** laFisYear     :  Initialize the array laFisYear
** laTstatus     :  Initialize the array laTstatus

** llOK          :  OK Push Button pressed at Preview screen
** loFormSet.llPeriodLock  :  .T. if the period if locked.
** llglobShow    :
** llBrowse      :  To say coming from browse or not.

** lnCentury     :  In case of CENTURY='ON' lnCentury=2 Else =0
** lncbHold      :  Holds the batch hold status.
** lnBalance     :  Holds the diff. between total CR and Total DR.
** rbCriteria    :  Holds the selection of criteria (review screen)

** rbType        :  Holds the selection of review mode Transations or transations detailas.

** rbSort        :  Holds the selection of sort mode (date,trns. no.)
** puActtype     :

** ldOldStDat    :  Holds old batch start date.
** ldOldEnDat    :  Holds old batch end   date.

loFormSet.AddProperty('lcYear',SPACE(4))
loFormSet.AddProperty('lcDelMesag',"void")
loFormSet.AddProperty('lcPrompt','')
loFormSet.AddProperty('lnBalance',0.00)

lcVars = 'lcOldStPrd , lcOldEnPrd   , lcStartPrd  , lcEndPrd   ,'+;
         'lcOldYear  , lcDumStart  , lcDummyEnd ,'+;
         'lcStatus   , lcBalncid    , lcBatType   , lcScope    ,'+;
         'lcBStamp   , lcPeriodLock , lcObjStatus '
=lfAddProp(loFormSet,lcVars,' ')

lcVars = 'llOK       , llPeriodLock , llglobShow  , llBrowse'
=lfAddProp(loFormSet,lcVars,.F.)

lcVars = 'lnCentury  , lncbHold'
=lfAddProp(loFormSet,lcVars,0)

loFormSet.AddProperty('rbCriteria',3)
lcVars = 'rbType     , rbSort       , puActtype   , puBatType'
=lfAddProp(loFormSet,lcVars,1)

lcVars = 'ldOldStDat , ldOldEnDat'
=lfAddProp(loFormSet,lcVars,{})

loFormSet.AddProperty('la_TPostYr[5]','')
loFormSet.la_TPostYr [1]  = 'Previous'
loFormSet.la_TPostYr [2]  = 'Current'
loFormSet.la_TPostYr [3]  = 'Next '
loFormSet.la_TPostYr [4]  = 'History'
loFormSet.la_TPostYr [5]  = ''    && to initialize the year field in the select mode.

loFormSet.AddProperty('llDoLocal',.T.)
loFormSet.AddProperty('lcLoclShow',"lfCheckRec")

loFormSet.AddProperty('laFisYear[3,2]', ' ')
loFormSet.laFisYear[1,1]  = STR(loFormSet.lnCurr_yer-1,4) + ' - Previous'
loFormSet.laFisYear[1,2]  = STR(loFormSet.lnCurr_yer-1,4)
loFormSet.laFisYear[2,1]  = STR(loFormSet.lnCurr_yer  ,4) + ' - Current'
loFormSet.laFisYear[2,2]  = STR(loFormSet.lnCurr_yer  ,4)
loFormSet.laFisYear[3,1]  = STR(loFormSet.lnCurr_yer+1,4) + ' - Next'
loFormSet.laFisYear[3,2]  = STR(loFormSet.lnCurr_yer+1,4)

*- check if the program working with periods format or dates format.
loFormSet.AddProperty('llDate', IIF(LEFT(GLSETUP.cSetPcnt,1)=='D',.T.,.F.) )

lcScFields = 'CBATCHNO,CBATSTAT,CBATTYPE,CBATPYR,DBATPBEG,DBATPEND,NBATCNTOT,'+;
             'CBATREFER,CBATDESC,NBATOTCR,NBATOTDR,CSRCMODUL,CCOMP_ID'
loFormSet.AddProperty('lcScFields',lcScFields)
lcLn = ALLTRIM(STR(OCCURS(',',lcScFields)+1))
loFormSet.AddProperty('laData[&lcLn]',' ')

SELECT GLBATCH
lcScFields = loFormSet.lcScFields
SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK

SELECT GLBATCH
&& initialize the filter to glbatch
lcScope    = "cBatType $ 'NSL' .AND. cBatStat $ 'EOUHAP'"
loFormSet.AddProperty('lcScope',lcScope)

SET FILTER TO
SET FILTER TO &lcScope

****
*- loFormSet.ActiveMode='S'  && Select mode.
*- loFormSet.ActiveMode='V'  && View mode.
*- loFormSet.ActiveMode='E'  && Edit mode.
*- loFormSet.ActiveMode='A'  && Add mode.

loFormSet.AddProperty('lcNewStat', IIF(loFormSet.ActiveMode='S' .OR. loFormSet.ActiveMode='V',"ENABLE","DISABLE"))
loFormSet.AddProperty('lcTrnStat', IIF((loFormSet.ActiveMode='V' .AND. loFormSet.laData[2] <> 'E') .OR. loFormSet.ActiveMode='E',"ENABLE","DISABLE"))
loFormSet.AddProperty('lcPrnStat', IIF(loFormSet.ActiveMode='V' .AND. loFormSet.laData[2]<> 'E',"ENABLE","DISABLE"))
loFormSet.AddProperty('lcRevStat', IIF((loFormSet.ActiveMode='V' .OR. loFormSet.ActiveMode='E') .AND. loFormSet.laData[2]<> 'E',"ENABLE","DISABLE"))
loFormSet.AddProperty('lcPosStat', IIF(loFormSet.ActiveMode='V' .AND. ;
                 (loFormSet.laData[2] ='U' .OR. (loFormSet.laData[2]='O' .AND. loFormSet.laData[3]='S')),"ENABLE","DISABLE"))
loFormSet.AddProperty('lcHldStat', IIF(loFormSet.ActiveMode='E' .AND. loFormSet.laData[2] $ 'UH',"ENABLE","DISABLE" ))
loFormSet.AddProperty('lcTypStat', IIF((loFormSet.ActiveMode='E' .OR. loFormSet.ActiveMode='A') .AND. loFormSet.laData[2] = 'E' ,"ENABLE","DISABLE"))

loFormSet.AddProperty('lcObjStatus', IIF(loFormSet.ActiveMode='V' .AND. loFormSet.laData[2] $ 'EOUZH',"ENABLE","DISABLE"))

*- End of lfDefineVars.

************************************************************
*! Name      : lfSetControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/07/2012
*! Purpose   : Set ControlSource for the screen controls
************************************************************
FUNCTION lfSetControlSource
PARAMETERS loFormset
*- fields are ordered according to their appearence in the screen
WITH loFormSet.Ariaform1
  .laData1.Keytextbox.ControlSource = 'Thisformset.laData[1]'
  .puBatType.ControlSource          = 'Thisformset.laData[3]'
  .laData4.ControlSource            = 'Thisformset.laData[4]'

  .lcStartPrd.ControlSource         = 'Thisformset.lcStartPrd'
  .laData5.ControlSource            = 'Thisformset.laData[5]'
  .lcEndPrd.ControlSource           = 'Thisformset.lcEndPrd'
  .laData6.ControlSource            = 'Thisformset.laData[6]'
  .laData7.ControlSource            = 'Thisformset.laData[7]'
  .laData8.ControlSource            = 'Thisformset.laData[8]'
  .laData9.ControlSource            = 'Thisformset.laData[9]'
  .lcStatus.ControlSource           = 'Thisformset.lcStatus'

  .laData10.ControlSource           = 'Thisformset.laData[10]'
  .laData11.ControlSource           = 'Thisformset.laData[11]'
  .lnBalance.ControlSource          = 'Thisformset.lnBalance'
  .lcBalncId.ControlSource          = 'Thisformset.lcBalncId'
ENDWITH

*- Define row source for popups
WITH loFormSet.Ariaform1
  .puBatType.RowSource  = 'Thisformset.laBatType'
  .laData4.RowSource    = 'ThisFormSet.laFisYear'

  .lcStartPrd.RowSource = 'ThisFormSet.laSelectFrm'

  .lcEndPrd.RowSource   = 'ThisFormSet.laSelectTO'

ENDWITH

*- End of lfSetControlSource.


************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/07/2012
*! Purpose   : lfFormActivate method
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

IF loFormSet.ActiveMode='V' .OR. loFormSet.ActiveMode='E'
  && in case of return to the program after activate the any other
  && program, Reread the batch status, batch Credit, Batch Debit.
  loFormSet.laData[2]  = glbatch.cbatstat
  loFormSet.laData[10] = glbatch.nbatotcr
  loFormSet.laData[11] = glbatch.nbatotdr
  loFormSet.lnBalance  = ABS(loFormSet.laData[11] - loFormSet.laData[10])
  loFormSet.lcBalncID  = IIF(loFormSet.laData[11] > loFormSet.laData[10],"Cr",;
              IIF(loFormSet.laData[11] < loFormSet.laData[10],"Dr",""))

  IF loFormSet.laData[2] = 'U' .AND. loFormSet.lncbHold = 1
    loFormSet.Ariaform1.cbHold.Value = 1
    loFormSet.laData[2] = 'H'
  ELSE
    loFormSet.Ariaform1.cbHold.Value = 0
  ENDIF

  loFormset.lcStatus  = loFormset.laTStatus[AT(loFormSet.laData[2],'EOUPZHA')]
  loFormset.lncbHold = loFormSet.Ariaform1.cbHold.Value


ENDIF

*- End of lfFormActivate.

************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/07/2012
*! Purpose   : Change mode function
************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet

IF TYPE('loFormSet.lcProgName')='U'
  RETURN
ENDIF

SELECT GLBATCH
IF loFormSet.ActiveMode='V' AND loFormSet.lcBStamp <> DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time
  && hold the audit information in lcBStamp from the current record.
  loFormSet.lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time
ENDIF

=lfStrEndPos(loFormset)

IF loFormSet.ActiveMode='V'
  lcScFields = loFormSet.lcScFields
  SCATTER FIELDS &lcScFields TO loFormSet.laData
ENDIF

=lfCheckRec(loFormSet)  && check the current batch status.

DO CASE
  CASE loFormSet.ActiveMode='S'        && ----->  Select Mode
    loFormSet.llPeriodLock = .F.

    loFormSet.lcDumStart = ''
    loFormSet.lcDummyEnd = ''
    loFormSet.lcBStamp   = ''
    loFormSet.lcStatus   = ''
    *loFormSet.lcBatType  = loFormSet.laBatType[1,1] &&-- Default value .
    loFormSet.puBatType  = 1
    loFormSet.Ariaform1.puBatType.Value = 'N'
    loFormSet.lcYear     = ''
    loFormSet.lcStartPrd = ''
    loFormSet.lcEndPrd   = ''
    loFormSet.lnBalance  = 0.00
    loFormSet.lcBalncID  = ''
    loFormSet.Ariaform1.cbHold.Value     = 0
    loFormSet.lncbHold   = loFormSet.Ariaform1.cbHold.Value

    loFormSet.Ariaform1.laData1.Enabled = .T.


    loFormSet.Ariaform1.puBatType.Enabled = 'ENABLE'=   'DISABLE'
    loFormSet.Ariaform1.laData4.ENABLED = 'ENABLE'  =   'DISABLE'

    loFormSet.Ariaform1.laData5.ENABLED = 'ENABLE'  =   'DISABLE'

    loFormSet.Ariaform1.laData6.ENABLED = 'ENABLE'  =   'DISABLE'

    loFormSet.Ariaform1.laData11.ENABLED = 'ENABLE'  =  'DISABLE'
    loFormSet.Ariaform1.laData10.ENABLED = 'ENABLE'  =  'DISABLE'

    loFormSet.Ariaform1.laData7.ENABLED = 'ENABLE'  =   'DISABLE'

    loFormSet.Ariaform1.laData8.ENABLED = 'ENABLE'  =   'DISABLE'
    loFormSet.Ariaform1.laData9.ENABLED = 'ENABLE'  =   'DISABLE'

    loFormSet.lcObjStatus = 'DISABLE'

    loFormSet.Ariaform1.Refresh()
    loFormSet.Ariaform1.laData1.Setfocus()


  && ----> View or Edit Mode
  CASE loFormSet.ActiveMode='V' .OR. loFormSet.ActiveMode='E'
    *** IF RECORD CHANGED AND NOT VIEW MODE

    loFormSet.lcStatus  = loFormset.laTStatus[AT(loFormSet.laData[2],'EOUPZHA')]
    loFormSet.Ariaform1.cbHold.Value    = IIF(loFormSet.laData[2]='H',1,0)
    loFormSet.lncbHold  = loFormSet.Ariaform1.cbHold.Value
    loFormSet.lnBalance = ABS(loFormSet.laData[11] - loFormSet.laData[10])
    loFormSet.lcBalncID = IIF(loFormSet.laData[11] > loFormSet.laData[10],"Cr",;
                IIF(loFormSet.laData[11] < loFormSet.laData[10],"Dr",""))

    loFormSet.lcYear    = loFormSet.la_TPostYr[IIF(loFormSet.laData[4]=loFormSet.lcCurr_yer   ,2,;
                        IIF(VAL(loFormSet.laData[4])=VAL(loFormSet.lcCurr_yer)-1,1,;
                        IIF(VAL(loFormSet.laData[4])=VAL(loFormSet.lcCurr_yer)+1,3,4)))]

    lnStartPrd = gfPeriod(loFormSet.laData[5],oAriaApplication.prntcompanyid)
    loFormSet.lcStartPrd = RIGHT("0"+ALLTRIM(STR(lnStartPrd)),2)
    lnEndPrd   = gfPeriod(loFormSet.laData[6],oAriaApplication.prntcompanyid)
    loFormSet.lcEndPrd   = RIGHT("0"+ALLTRIM(STR(lnEndPrd)),2)
    loFormSet.lcDumStart = loFormSet.lcStartPrd
    loFormSet.lcDummyEnd = loFormSet.lcEndPrd

    *** View mode
    IF loFormSet.ActiveMode='V'
      loFormSet.lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time

      loFormSet.Ariaform1.laData1.Enabled = 'ENABLE' = 'DISABLE'

      loFormSet.Ariaform1.puBatType.Enabled = 'ENABLE'=   'DISABLE'
      loFormSet.Ariaform1.laData4.ENABLED = 'ENABLE'  =   'DISABLE'

      loFormSet.Ariaform1.laData5.ENABLED = 'ENABLE'  =   'DISABLE'

      loFormSet.Ariaform1.laData6.ENABLED = 'ENABLE'  =   'DISABLE'

      loFormSet.Ariaform1.laData11.ENABLED = 'ENABLE'  =  'DISABLE  '
      loFormSet.Ariaform1.laData10.ENABLED = 'ENABLE'  =  'DISABLE'

      loFormSet.Ariaform1.laData7.ENABLED = 'ENABLE'  =   'DISABLE'

      loFormSet.Ariaform1.laData8.ENABLED = 'ENABLE'  =   'DISABLE'
      loFormSet.Ariaform1.laData9.ENABLED = 'ENABLE'  =   'DISABLE'

      *** If empty batch dont go to transaction program
      loFormSet.Ariaform1.pbTrans.Enabled = loFormSet.laData[2]<>"E"

      IF loFormSet.laData[2] $ 'PA'
        loFormSet.lcObjStatus = 'DISABLE'

        loFormSet.oToolBar.cmdDelete.Enabled = .F.

        loFormSet.oToolBar.cmdEdit.Enabled = .F.

        loFormSet.Ariaform1.pbPost.Enabled = 'ENABLE' =  'DISABLE'
      ELSE
        loFormSet.lcObjStatus = 'ENABLE'

        loFormSet.oToolBar.cmdDelete.Enabled = .T.

        loFormSet.oToolBar.cmdEdit.Enabled = .T.

        IF (loFormSet.laData[2]= "U") .OR. (loFormSet.laData[2]= "O" .AND. loFormSet.laData[3]= "S")
          loFormSet.Ariaform1.pbPost.Enabled = 'ENABLE' =   'ENABLE'
        ELSE
          loFormSet.Ariaform1.pbPost.Enabled = 'ENABLE' =   'DISABLE'
        ENDIF
      ENDIF

      IF loFormSet.laData[3]='L'

        loFormSet.oToolBar.cmdEdit.Enabled = .F.

      ENDIF

    ELSE       && ----->  Edit mode
      loFormSet.Ariaform1.laData1.Enabled = 'ENABLE' = 'DISABLE'

      IF loFormSet.laData[2] = "E"
        loFormSet.Ariaform1.puBatType.Enabled = 'ENABLE'=   'ENABLE'
        loFormSet.Ariaform1.laData4.ENABLED = 'ENABLE'  =   'ENABLE'

        *** check if the program working with periods format or dates format.
        IF loFormSet.llDate
          loFormSet.Ariaform1.laData5.ENABLED = 'ENABLE'  =  'ENABLE'
          loFormSet.Ariaform1.laData6.ENABLED = 'ENABLE'  =  'ENABLE'
        ELSE
          loFormSet.Ariaform1.laData5.ENABLED = 'ENABLE'  =  'DISABLE'
          loFormSet.Ariaform1.laData6.ENABLED = 'ENABLE'  =  'DISABLE'
          loFormSet.Ariaform1.lcStartPrd.ENABLED = 'ENABLE' = 'ENABLE'
          loFormSet.Ariaform1.lcEndPrd.ENABLED = 'ENABLE' = 'ENABLE'
        ENDIF
      ELSE

        loFormSet.Ariaform1.puBatType.Enabled = 'ENABLE'=   'DISABLE    '
        loFormSet.Ariaform1.laData4.ENABLED = 'ENABLE'  =   'DISABLE'

        loFormSet.Ariaform1.laData5.ENABLED = 'ENABLE'  =   'DISABLE'
        loFormSet.Ariaform1.laData6.ENABLED = 'ENABLE'  =   'DISABLE'
        loFormSet.Ariaform1.lcStartPrd.ENABLED = 'ENABLE' =  'DISABLE'
        loFormSet.Ariaform1.lcEndPrd.ENABLED = 'ENABLE' = 'DISABLE'
      ENDIF

      loFormSet.Ariaform1.laData11.ENABLED = 'ENABLE'  =  'ENABLE'
      loFormSet.Ariaform1.laData10.ENABLED = 'ENABLE'  =  'ENABLE'
      loFormSet.Ariaform1.lnBalance.ENABLED = 'ENABLE'  =  'ENABLE'
      loFormSet.Ariaform1.lcBalncId.ENABLED = 'ENABLE'  =  'ENABLE'
      loFormSet.Ariaform1.laData7.ENABLED = 'ENABLE'  =   'ENABLE'

      *** If batch is unposted or holded enable the hold check box
      IF loFormSet.laData[2] $ 'UH'
        loFormSet.Ariaform1.cbHold.ENABLED = 'ENABLE'  =   'ENABLE'
      ELSE
        loFormSet.Ariaform1.cbHold.ENABLED = 'ENABLE'  =   'DISABLE'
      ENDIF
      loFormSet.lncbHold   = loFormSet.Ariaform1.cbHold.Value

      loFormSet.Ariaform1.laData8.ENABLED = 'ENABLE'  =   'ENABLE'
      loFormSet.Ariaform1.laData9.ENABLED = 'ENABLE'  =   'ENABLE'
      loFormSet.Ariaform1.pbTrans.ENABLED = 'ENABLE'  =   'ENABLE'

      loFormSet.Ariaform1.pbPost.Enabled = 'ENABLE' =       'DISABLE'

      loFormSet.oToolBar.cmdDelete.Enabled = .F.
      loFormSet.lcObjStatus = 'DISABLE'

    ENDIF

    *** If empty batch dont go to review or print
    IF loFormSet.laData[2]= "E"
      loFormSet.Ariaform1.pbReview.ENABLED = 'ENABLE'  =   'DISABLE'
    ELSE
      loFormSet.Ariaform1.pbReview.ENABLED = 'ENABLE'  =   'ENABLE'
    ENDIF


  && ----->  Add Mode
  CASE loFormSet.ActiveMode='A'

    loFormSet.laData[4]  = loFormSet.lcCurr_yer

    loFormSet.lcYear     = loFormSet.la_TPostYr[2]

    SELECT  FSPRD
    SET ORDER TO COMFYRPRDI
    SEEK (loFormSet.lcCurr_yer+loFormSet.lcCurr_Prd)

    loFormSet.laData[5] = FSPRD.dFsPpBgDt
    loFormSet.laData[6] = FSPRD.dFsPpEnDt

    *- Update the source arrays of the dates popups
    =lfFillAry1(loFormSet)
    =lfFillAry2(loFormSet)

    *loformset.ariaform1.lcstartPrd.RowSource = 'Thisformset.laSelectFrm'
    *loformset.ariaform1.lcEndPrd.RowSource = 'Thisformset.laSelectTO'
    loFormSet.ariaform1.refresh()

    SELECT GLBATCH

    loFormSet.lcStartPrd = loFormSet.lcCurr_Prd
    loFormSet.lcEndPrd   = loFormSet.lcCurr_Prd
    loFormSet.lcDumStart = loFormSet.lcStartPrd
    loFormSet.lcDummyEnd = loFormSet.lcEndPrd
    loFormSet.laData[1]  = ''
    loFormSet.laData[2]  = 'E'

    loFormSet.laData[3]  = loFormSet.laBatType[1,2] &&-- Default value in add mode

    loFormSet.lcStatus   = loFormSet.laTStatus[1]
    loFormSet.laData[11] = 0.00
    loFormSet.laData[10] = 0.00
    loFormSet.lnBalance  = 0.00
    loFormSet.lcBalncID  = ''
    loFormSet.laData[7]  = 0.00
    loFormSet.Ariaform1.cbHold.Value     = 0
    loFormSet.laData[8]  = "On " + IIF(SET('CENTURY')='ON',DTOC(oAriaApplication.SystemDate),;
                         LEFT(DTOC(oAriaApplication.SystemDate),6)+STR(YEAR(oAriaApplication.SystemDate),4))

    loFormSet.laData[9]  = SUBSTR("Created by " + LOOKUP(SYUUSER.cUsr_Name,oAriaApplication.User_Id,SYUUSER.cUser_Id),1,40)

    loFormSet.laData[12] = oAriaApplication.ActiveModuleID
    loFormSet.laData[13] = oAriaApplication.ActiveCompanyID


    loFormSet.Ariaform1.laData1.Enabled = 'ENABLE'=='DISABLE'

    loFormSet.Ariaform1.puBatType.Enabled = 'ENABLE'=   'ENABLE'
    loFormSet.Ariaform1.laData4.ENABLED = 'ENABLE'  =   'ENABLE'

    loFormSet.Ariaform1.lcStartPrd.ENABLED = 'ENABLE' =  'ENABLE'

    loFormSet.Ariaform1.lcEndPrd.ENABLED = 'ENABLE' = 'ENABLE'

    IF loFormSet.llDate
      loFormSet.Ariaform1.laData5.ENABLED = 'ENABLE'  =  'ENABLE'
      loFormSet.Ariaform1.laData6.ENABLED = 'ENABLE'  =  'ENABLE'
    ELSE
      loFormSet.Ariaform1.laData5.ENABLED = 'ENABLE'  =  'DISABLE'
      loFormSet.Ariaform1.laData6.ENABLED = 'ENABLE'  =  'DISABLE'
    ENDIF

    loFormSet.Ariaform1.laData11.ENABLED = 'ENABLE'  =  'ENABLE'
    loFormSet.Ariaform1.laData10.ENABLED = 'ENABLE'  =  'ENABLE'
    loFormSet.Ariaform1.lnBalance.ENABLED = 'ENABLE'  =   'ENABLE'
    loFormSet.Ariaform1.lcBalncId.ENABLED = 'ENABLE'  =   'ENABLE'
    loFormSet.Ariaform1.laData7.ENABLED = 'ENABLE'  =   'ENABLE'
    loFormSet.Ariaform1.cbHold.ENABLED = 'ENABLE'  =   'DISABLE'
    loFormSet.Ariaform1.laData8.ENABLED = 'ENABLE'  =   'ENABLE'
    loFormSet.Ariaform1.laData9.ENABLED = 'ENABLE'  =   'ENABLE'
    loFormSet.Ariaform1.pbTrans.ENABLED = 'ENABLE'  =   'DISABLE'
    loFormSet.Ariaform1.pbReview.ENABLED = 'ENABLE'  =   'DISABLE'

    loFormSet.Ariaform1.pbPost.Enabled = 'ENABLE' =       'DISABLE'
    loFormSet.lncbHold   = loFormSet.Ariaform1.cbHold.Value

    loFormSet.oToolBar.cmdDelete.Enabled = .F.
    loFormSet.lcObjStatus = 'DISABLE'

    *B610269,1 media fixing problems TMI 03/17/2013 [Start] comment this, this could not be called from Valid method
    *loformset.ariaform1.puBatType.Setfocus()
    *B610269,1 media fixing problems TMI 03/17/2013 [End  ] 

    loFormset.Ariaform1.Refresh()
    
    *B610328,1 TMI 05/08/2013 [Start] refresh the TO PERIOD popup
    loFormset.AriaForm1.lcStartPrd.Valid()
    *B610328,1 TMI 05/08/2013 [End  ] 
ENDCASE

SELECT GLBATCH
*- End of lfChangeMode.


*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1
PARAMETERS loFormSet,loFld

loFormSet.laData[1] = loFld.KeyTextbox.Value
llBrowse = loFld.Selectedfrombrowse
IF llBrowse .OR. !EMPTY(loFormSet.laData[1])

  IF LEFT(ALLTRIM(loFormSet.laData[1]),1) <> '?' .AND. !llBrowse
    loFormSet.laData[1] = RIGHT("000000"+ALLTRIM(loFormSet.laData[1]),6)
    loFld.KeyTextbox.Value = loFormSet.laData[1]
  ENDIF

  IF RECNO() <= RECCOUNT() .AND. ! EOF()
    GOTO RECNO()
  ENDIF

  lcFile_Ttl = loFormSet.BrowseTitle
  lcBrFields = loFormSet.ariaBrFields.edtBrowseFields.Value
  llView = .F.
  lcBaseFile = loFormSet.lcBaseFile
  llBrowse = loFld.Selectedfrombrowse

  SELECT (lcBaseFile)
  IF llBrowse .OR. !gfSEEK(loFld.KeyTextBox.VALUE) .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0
    IF llBrowse .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0
      IF loFormSet.oToolBar.cmdFind.Click()
        llView = .T.
      ELSE
        loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
      ENDIF
    ELSE
      *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	      lnOption  = gfModalGen('QRM00001B00001','Dialog',;
*!*	                     'GL Batch Number: '+ALLTRIM(loFld.KeyTextBox.VALUE))
      lnOption  = gfModalGen('QRM00001B00001','Dialog',;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBATCHNUMCOL,loFormSet.GetHeaderText("LANG_GLBATCHNUMCOL",loFormSet.HeaderAlias))+ALLTRIM(loFld.KeyTextBox.VALUE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 MMT 11/22/2012 Globalization changes[END]
      DO CASE
        CASE lnOption = 1
          IF loFormSet.oToolBar.cmdFind.Click()
            llView = .T.
          ELSE
            loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
          ENDIF
        CASE lnOption = 2
          loFormSet.CHangeMode('A')
          RETURN

        CASE lnOption = 3
          loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
          RETURN .F.
      ENDCASE
    ENDIF
  ELSE
    loFld.KeyTextBox.VALUE = &lcBaseFile..CBATCHNO

    llView = .T.
  ENDIF

  IF llView = .T.
    loFormSet.CHangeMode('V')
  ENDIF

   IF loFormSet.ActiveMode='A'    && add mode
    loFormSet.laData[1] = "      "
    loFormSet.laData[2] = 'E'
    loFormSet.Ariaform1.laData1.Refresh()
  ELSE
    lnStartPrd = gfPeriod(loFormSet.laData[5],oAriaApplication.prntcompanyid)
    loFormSet.lcStartPrd = RIGHT("0"+ALLTRIM(STR(lnStartPrd)),2)
    lnEndPrd   = gfPeriod(loFormSet.laData[6],oAriaApplication.prntcompanyid)
    loFormSet.lcEndPrd   = RIGHT("0"+ALLTRIM(STR(lnEndPrd)),2)
    loFormSet.Ariaform1.lcStartPrd.ENABLED = 'ENABLE' = 'DISABLE'
    loFormSet.Ariaform1.lcEndPrd.ENABLED = 'ENABLE' = 'DISABLE'
  ENDIF
ENDIF

loFormSet.CheckNavigation()
loFormSet.OToolbar.NAVrefresh()

*!**************************************************************************
*!
*!      Function: lfvData_7
*!
*!**************************************************************************
FUNCTION lfvData_7
PARAMETERS loFormSet,loFld
*** check the value of audit total

IF SIGN(loFormSet.laData[7]) = -1
  * Negative values are not allowed.
  * < OK >
  =gfModalGen("TRM02036B00000","DIALOG")
  loFormSet.laData[7] = loFld.OldValue
  RETURN 0
ENDIF
RETURN 1


*!**************************************************************************
*!
*!      Function: lfvPostyr
*!
*!**************************************************************************
*
FUNCTION lfvPostyr
PARAMETERS loFormSet,loFld

*** check if the program working with periods format or dates format.
IF loFld.Value <> loFld.OldValue
  loFormSet.lcYear = loFormSet.la_TPostYr[IIF(loFormSet.laData[4]     =    loFormSet.lcCurr_yer   ,2,;
                          IIF(VAL(loFormSet.laData[4])=VAL(loFormSet.lcCurr_yer)-1,1,;
                          IIF(VAL(loFormSet.laData[4])=VAL(loFormSet.lcCurr_yer)+1,3,4)))]

  IF loFormSet.llDate

    loFormSet.lcStartPrd  =  "01"

  ELSE

    IF EMPTY(loFormSet.lcStartPrd)
      loFormSet.lcStartPrd = loFormSet.lcCurr_Prd
    ENDIF

    SELECT  FSPRD
    SET ORDER TO TAG COMFYRPRDI
    SEEK (loFormSet.laData[4]+loFormSet.lcStartPrd)
    IF FOUND()
      loFormSet.laData[5] = FSPRD.dFsPpBgDt

    ELSE
      * Period ð does not exist in fiscal year ð .
      * < OK >
      =gfModalGen("TRM02156B00000","DIALOG",loFormSet.lcStartPrd+'|'+loFormSet.laData[4])
      loFormSet.lcStartPrd = ' '
      loFormSet.laData[5] = {}

      SELECT GLBATCH

    ENDIF

    IF EMPTY(loFormSet.lcEndPrd)
      loFormSet.lcEndPrd = loFormSet.lcCurr_Prd
    ENDIF

    SEEK (loFormSet.laData[4]+loFormSet.lcEndPrd)
    IF FOUND()
      loFormSet.laData[6] = FSPRD.dFsPpEnDt

    ELSE
      * Period ð does not exist in fiscal year ð .
      * < OK >
      =gfModalGen("TRM02156B00000","DIALOG",loFormSet.lcEndPrd+'|'+loFormSet.laData[4])
      loFormSet.lcEndPrd = ' '
      loFormSet.laData[6] = {}

      SELECT GLBATCH

    ENDIF
  ENDIF
ENDIF

=lfFillAry1(loFormSet)
=lfFillAry2(loFormSet)

loFormSet.ariaform1.refresh()
SELECT GLBATCH


*!**************************************************************************
*!
*!      Function: lfvSDatPrd
*!
*!**************************************************************************
*
FUNCTION lfvSDatPrd
PARAMETERS loFormSet

*** check if the program working with periods format or dates format.
IF loFormSet.llDate
  =lfvStaDate()
ELSE
  =lfvStaPrd()
ENDIF


*!**************************************************************************
*!
*!      Function: lfvEDatPrd
*!
*!**************************************************************************
*
FUNCTION lfvEDatPrd
PARAMETERS loFormSet

*** check if the program working with periods format or dates format.
IF loFormSet.llDate
  =lfvEndDate()
ELSE
  =lfvEndPrd()
ENDIF


*!**************************************************************************
*!
*!      Function: lfFillAry1
*!
*!**************************************************************************
*
FUNCTION lfFillAry1
PARAMETERS loFormSet


gcDataDir = oAriaApplication.DataDir
SELECT Cfspprdid,Cfisfyear,Dfsppbgdt,;
    dfsppendt, IIF(lFspLocks,'Locked','Unlocked');
    FROM  &gcDataDir.FSPRD.DBF;
    WHERE  Cfisfyear =  loFormSet.laData[4] ;
    ORDER BY FSPRD.Cfspprdid;
    INTO  ARRAY loFormSet.laSelectFrm

loFormSet.Ariaform1.lcStartPrd.RowSource = 'Thisformset.laSelectFRM'

DIMENSION loFormSet.laSelect[ALEN(loFormSet.laSelectFrm,1),5]
ACOPY(loFormSet.laSelectFrm,loFormSet.laSelect)

*!**************************************************************************
*!
*!      Function: lfFillAry2
*!
*!**************************************************************************
*
FUNCTION lfFillAry2
PARAMETERS loFormSet

*** select all periods id, begin date, end date, lock status
*** where cFspPrdId >= lcStartPrd into array laselect
gcDataDir = oAriaApplication.DataDir
DIMENSION loFormSet.laSelectTO[1,5]
SELECT Cfspprdid,Cfisfyear,Dfsppbgdt,;
    dfsppendt, IIF(lFspLocks,'Locked','Unlocked');
    FROM  &gcDataDir.FSPRD.DBF;
    WHERE  Cfisfyear =  loFormSet.laData[4] ;
    .AND. cFspPrdId >= loFormSet.lcStartPrd ;
    ORDER BY FSPRD.Cfspprdid;
    INTO  ARRAY loFormSet.laSelectTO

loFormSet.Ariaform1.lcEndPrd.RowSource = 'Thisformset.laSelectTO'

DIMENSION loFormSet.laSelect[ALEN(loFormSet.laSelectTO,1),5]
ACOPY(loFormSet.laSelectTO,loFormSet.laSelect)


*!**************************************************************************
*!
*!      Function: lfvStaDate
*!
*!**************************************************************************
*
FUNCTION lfvStaDate
PARAMETERS loFormSet,loFld

LOCAL lnPrds
lnPrds = ALEN(loFormSet.laSelectFrm,1)
IF !BETWEEN(loFormSet.laData[5],;
   loFormSet.laSelectFrm[1,3],loFormSet.laSelectFrm[lnPrds,4])

   lcRunScx = lfGetScx("GL\GLDATELST.scx")
   DO FORM (lcRunScx) WITH loFormSet,loFld

ENDIF

lnStartPrd  =gfPeriod(loFormSet.laData[5],oAriaApplication.prntcompanyid)
loFormSet.lcStartPrd  = RIGHT("0"+ALLTRIM(STR(lnStartPrd)),2)
=lfFillAry2(loFormSet)
RETURN 1

*!**************************************************************************
*!
*!      Function: lfvEndDate
*!
*!**************************************************************************
*
FUNCTION lfvEndDate
PARAMETERS loFormSet,loFld
PRIVATE lcSExact

LOCAL lnPrds
lnPrds = ALEN(loFormSet.laSelectTO,1)
IF !BETWEEN(loFormSet.laData[6],loFormSet.laSelectTO[1,3],loFormSet.laSelectTO[lnPrds,4])

  lcRunScx = lfGetScx("GL\GLDATELST.scx")
  DO FORM (lcRunScx) WITH loFormSet,loFld

ENDIF

lnEndPrd =gfPeriod(loFormSet.laData[6],oAriaApplication.prntcompanyid)
loFormSet.lcEndPrd = RIGHT("0"+ALLTRIM(STR(lnEndPrd)),2)
RETURN 1


*!**************************************************************************
*!
*!      Function: lfvStaPrd
*!
*!**************************************************************************
*
FUNCTION lfvStaPrd
PARAMETERS loFormSet,loFld

IF EMPTY(loFld.Value)

  * You must choose a ð value, it can't be empty
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *=gfModalGen('INM00386B00000','DIALOG','Start Period')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00386B00000','DIALOG',LANG_STARTPERIOD)
=gfModalGen('INM00386B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STARTPERIOD,loFormSet.GetHeaderText("LANG_STARTPERIOD",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
  RETURN 0
ENDIF
*E303190,4 TMI 08/06/2012 [Start] the date is not put correctly
*loFormSet.laData[5] = DATE(VAL(loFormSet.laData[4]),VAL(loFld.Value),1)
=SEEK(loFormSet.laData[4]+loFld.Value,'FSPRD','COMFYRPRDI')   && CFISFYEAR+CFSPPRDID
loFormSet.laData[5] = FSPRD.DFSPPBGDT
*E303190,4 TMI 08/06/2012 [End  ]

=lfFillAry2(loFormSet)
IF loFld.Value > loFormset.ariaform1.lcEndPrd.Value
  loFormset.ariaform1.lcEndPrd.Value = loFld.Value
  *E303190,4 TMI 08/06/2012 [Start] the date is not set correctly
  *loFormSet.laData[6] = GOMONTH(loFormSet.laData[5],1)-1
  loFormSet.laData[6] = FSPRD.DFSPPENDT
  *E303190,4 TMI 08/06/2012 [End  ]
ENDIF
loFormSet.lcStartPrd = loFld.Value
loFormSet.lcEndPrd = loFormset.ariaform1.lcEndPrd.Value
loFormset.ariaform1.refresh()


*!**************************************************************************
*!
*!      Function: lfvEndPrd
*!
*!**************************************************************************
*
FUNCTION lfvEndPrd
PARAMETERS loFormSet,loFld

IF EMPTY(loFld.Value)
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *=gfModalGen('INM00386B00000','DIALOG','End Period')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00386B00000','DIALOG',LANG_ENDPERIOD)
=gfModalGen('INM00386B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ENDPERIOD,loFormSet.GetHeaderText("LANG_ENDPERIOD",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
  RETURN 0
ENDIF

*B610328,1 TMI 05/08/2013 [Start] update the field display based on the change in the TO PERIOD 
=SEEK(loFormSet.laData[4]+loFld.Value,'FSPRD','COMFYRPRDI')   && CFISFYEAR+CFSPPRDID
loFormSet.laData[6] = FSPRD.DFSPPENDT
loFormSet.Ariaform1.laData6.Refresh()
*B610328,1 TMI 05/08/2013 [End  ] 


IF EMPTY(loFormSet.Ariaform1.lcStartPrd.Value)
  loFormSet.Ariaform1.lcStartPrd.Value = loFld.Value
  loFormSet.laData[5] = DATE(VAL(loFormSet.laData[4]),VAL(loFld.Value),1)
  loFormSet.laData[6] = GOMONTH(loFormSet.laData[5],1)-1
ENDIF


*!**************************************************************************
*!
*!      Function: lfvReview
*!
*!**************************************************************************
*
FUNCTION lfvReview
PARAMETERS loFormSet
DECLARE laTdata [1]

IF ! lfCheckRec(loFormSet)   && check the current batch status.
  RETURN
ENDIF

laTdata  = " "
llOk     = .F.

lcRunScx = lfGetScx("GL\GLREVIEW.scx")
DO FORM (lcRunScx) WITH loFormSet TO llOK

IF llOK                       && Push Button OK has been Pressed

  llOk      = .F.


  SELECT GLTRNSHD
  SET FILTER TO

  *** set the filter according to selected criteria.
  IF loFormSet.rbSort = 1
    SET ORDER TO TAG BATCHTRN
  ELSE
    SET ORDER TO TAG POSTDATE
  ENDIF

  IF loFormSet.laData[2] $ 'PA'
    SET FILTER TO  cTrnStat <> 'V' .AND. cBatchNo = loFormSet.laData[1]
  ELSE
    DO CASE
      CASE loFormSet.rbCriteria = 1
        SET FILTER TO cTrnStat = 'U'  .AND. cBatchNo = loFormSet.laData[1]

      CASE loFormSet.rbCriteria = 2
        SET FILTER TO cTrnStat = 'O'  .AND. cBatchNo = loFormSet.laData[1]

      CASE loFormSet.rbCriteria = 3
        SET FILTER TO cTrnStat $ 'UO' .AND. cBatchNo = loFormSet.laData[1]
    ENDCASE
  ENDIF

  DIMENSION laAceptRec[1]
  laAceptRec[1] = ""


  IF loFormSet.rbType  = 1
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	    lcFile_Ttl = 'Transactions'
*!*	    lcBrfields = "cTranNo:H='Trans. No.',"                   +;
*!*	                 "dTrnPDate:H='Trans. Date',"                +;
*!*	                 "cTrnStat:H='Status':15,"                   +;
*!*	                 "nTrnTotDr:H='Total Debit',"                +;
*!*	                 "nTrnTotCr:H='Total Credit',"               +;
*!*	                 "nBal=ABS(nTrnTotCr-nTrnTotDr):H='Balance'"
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl = LANG_S_TRANSACTIONS
lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_S_TRANSACTIONS,loFormSet.GetHeaderText("LANG_S_TRANSACTIONS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lcBrfields = "cTranNo:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANNO,loFormSet.GetHeaderText("LANG_TRANNO",loFormSet.HeaderAlias))+"',"                   +;
                 "dTrnPDate:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANSDATE,loFormSet.GetHeaderText("LANG_TRANSDATE",loFormSet.HeaderAlias))+"',"                +;
                 "cTrnStat:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STATUS,loFormSet.GetHeaderText("LANG_STATUS",loFormSet.HeaderAlias))+"':15,"                   +;
                 "nTrnTotDr:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TOTALDEBIT,loFormSet.GetHeaderText("LANG_TOTALDEBIT",loFormSet.HeaderAlias))+"',"                +;
                 "nTrnTotCr:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TOTALCREDIT,loFormSet.GetHeaderText("LANG_TOTALCREDIT",loFormSet.HeaderAlias))+"',"               +;
"nBal=ABS(nTrnTotCr-nTrnTotDr):H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BALANCE,loFormSet.GetHeaderText("LANG_BALANCE",loFormSet.HeaderAlias))+"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[eND]
    =gfBrows(.F.,"cTranNo","laAceptRec")
  ELSE

    SET RELATION TO gltrnshd.cbatchno+gltrnshd.cTranNo INTO GLTRNSDT ADDITIVE
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	    lcFile_Ttl = 'Transactions details'
*!*	    lcBrfields = "GLTRNSHD.cTranno:H='Transaction #',GLTRNSDT.cAcctcode:H=loFormSet.lcAcsegDes,"   +;
*!*	             "nDebit =IIF(GLTRNSDT.cDrorcr='D',GLTRNSDT.nAmount,0):H='Debit ':15," +;
*!*	             "nCredit=IIF(GLTRNSDT.cDrorcr='C',GLTRNSDT.nAmount,0):H='Credit':15," +;
*!*	             "cDesc=LOOKUP(GLACCHAR.cAccnldes,GLTRNSDT.cAcctcode," +;
*!*	             "GLACCHAR.cAcctcode,'ACCTCODE'):H='Description'"
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl = LANG_TRANSACTIONDETAIL
lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANSACTIONDETAIL,loFormSet.GetHeaderText("LANG_TRANSACTIONDETAIL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lcBrfields = "GLTRNSHD.cTranno:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANSACTION_NO,loFormSet.GetHeaderText("LANG_TRANSACTION_NO",loFormSet.HeaderAlias))+"',GLTRNSDT.cAcctcode:H=loFormSet.lcAcsegDes,"   +;
             "nDebit =IIF(GLTRNSDT.cDrorcr='D',GLTRNSDT.nAmount,0):H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DEBIT,loFormSet.GetHeaderText("LANG_DEBIT",loFormSet.HeaderAlias)) +"':15," +;
             "nCredit=IIF(GLTRNSDT.cDrorcr='C',GLTRNSDT.nAmount,0):H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CREDIT,loFormSet.GetHeaderText("LANG_CREDIT",loFormSet.HeaderAlias))+"':15," +;
             "cDesc=LOOKUP(GLACCHAR.cAccnldes,GLTRNSDT.cAcctcode," +;
"GLACCHAR.cAcctcode,'ACCTCODE'):H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESCRIPTION,loFormSet.GetHeaderText("LANG_DESCRIPTION",loFormSet.HeaderAlias))+"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
    SET SKIP TO GLTRNSDT
    =gfBrows(.F.,"cTranNo","laAceptRec")
    SET RELATION TO
  ENDIF

  SET FILTER TO
  SELECT GLBATCH

  *** if there is a record selected from browse window.
  *** calling transaction program.

  IF !EMPTY(laAceptRec)
    lcMode   = IIF(loFormSet.ActiveMode='V','T','F')
    lcTrOrEn = IIF(loFormSet.rbType = 1,'T','E')
    lnTrnHdRec = STR(RECNO('GLTRNSHD'))
    lnTrnDtRec = STR(RECNO('GLTRNSDT'))


    lcRunPrg = lfGetPrg('GL\GLTRANS.FXP')
    DO (lcRunPrg) WITH loFormSet.laData[1],lnTrnHdRec,lnTrnDtRec,lcMode,lcTrOrEn

    =lfBatchRefresh(loFormSet)

  ENDIF
ENDIF

************************************************************
*! Name      : lfBatchRefresh
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/07/2012
*! Purpose   : Refresh the current batch
************************************************************
FUNCTION lfBatchRefresh
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

IF loFormSet.ActiveMode = 'E'
  SELECT GLBATCH
  lcScFields = loFormSet.lcScFields
  GO RECNO()
  *E303190,4 TMI 07/22/2012 [Start] just update laData10,laData11
  *SCATTER FIELDS &lcScFields TO loFormSet.laData
  loFormSet.laData[10] = GLBATCH.NBATOTCR
  loFormSet.laData[11] = GLBATCH.NBATOTDR
  *E303190,4 TMI 07/22/2012 [End  ]
  loFormset.Ariaform1.Refresh()
ENDIF

SELECT (lnSlct)
*- End of lfBatchRefresh.


*!**************************************************************************
*!
*!      Function: lfvPrint
*!
*!**************************************************************************
*
FUNCTION lfvPrint
PARAMETERS loFormSet

IF ! lfCheckRec(loFormSet)    && check the current batch status.
  RETURN
ENDIF


*!**************************************************************************
*!
*!      Function: lfvPost
*!
*!**************************************************************************
*
FUNCTION lfvPost
PARAMETERS loFormSet

IF ! lfCheckRec(loFormSet)    && check the current batch status.
  RETURN
ENDIF

*** check if the batch year falls in posting window.

lcExactSet = SET("EXACT")
SET EXACT OFF

IF ASCAN(loFormSet.laFisYear,glBatch.cbatpyr) <> 0

  *** check if the user print the batch after last updating.
  IF !EMPTY(glBatch.cBatElUsr)
    llEditList= IIF ( glBatch.dbaTelDat > glBatch.daDd_Date,.T.,;
                IIF ( glBatch.dbaTelDat = glBatch.daDd_Date .AND.;
                      glBatch.cbAtelTim > glBatch.caDd_Time,.T.,.F.))
  ELSE
    llEditList = .F.
  ENDIF

  *** check the force printing flag in glsetup
  llPrint = IIF (glSetup.lSetForel,;
            IIF (llEditList,.T.,.F.),.T.)

  IF llPrint
    * Are you sure you want to post this ð ?
    * <Post> <No>
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *IF gfModalGen("TRM02149B02009","DIALOG",'batch') = 1
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen("TRM02149B02009","DIALOG",LANG_BATCH) = 1
IF gfModalGen("TRM02149B02009","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCH,loFormSet.GetHeaderText("LANG_BATCH",loFormSet.HeaderAlias))) = 1
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
      IF ! lfCheckRec(loFormSet)    && check the current batch status.
        SET EXACT &lcExactSet
        RETURN
      ENDIF

      SELECT GLBATCH
      IF gfObj_Lock(.T.)     && check the lock status.
        IF ! lfCheckRec(loFormSet)    && check the current batch status.
          SET EXACT &lcExactSet
          RETURN
        ENDIF
        *** calling posting program.
        IF lfTBPost(loFormset,'BATCH','','NonBeginning') <> 0
          lcScFields = loFormSet.lcScFields
          SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
          *** update the audit information in the variable lcBStamp.
          loFormSet.lcBStamp      = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time

          =SEEK(loformset.ariaform1.laData1.keytextbox.Value,'GLBATCH','BATCHNO')   && CBATCHNO

          loFormSet.ChangeMode('V')
          loFormSet.Ariaform1.Refresh()

        ENDIF
        =gfObj_Lock(.F.)
      ENDIF

    ENDIF
  ELSE
    * Printing the edit list is required to post this batch.
    * < OK >
    =gfModalGen("TRM02148B00000","Dialog")  &&  Printing the edit list
                                        &&  is a must to post.
  ENDIF
ELSE
  * The ð posting year is out of the posting window.  You cannot post this ð.
  * < OK >
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *=gfModalGen("TRM02147B00000","Dialog",'batch|batch')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM02147B00000","Dialog",LANG_2_BATCH)
=gfModalGen("TRM02147B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_2_BATCH,loFormSet.GetHeaderText("LANG_2_BATCH",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
                                        &&  The Batch posting year is
                                        &&  out of the posting window.
ENDIF

SET EXACT &lcExactSet


*!**************************************************************************
*!
*!      Function: lfvHold
*!
*!**************************************************************************
*
FUNCTION lfvHold
PARAMETERS loFormSet

loFormSet.lncbHold = loFormSet.Ariaform1.cbHold.Value

IF loFormSet.Ariaform1.cbHold.Value = 1
  loFormSet.laData[2]  = 'H'
  loFormSet.lcStatus  = loFormset.laTStatus[AT(loFormSet.laData[2],'EOUPZHA')]
 ELSE
  loFormSet.laData[2]  = 'U'
  loFormSet.lcStatus  = loFormset.laTStatus[AT(loFormSet.laData[2],'EOUPZHA')]
ENDIF

loFormSet.Ariaform1.lcStatus.Refresh()


*!**************************************************************************
*!
*!      Function: lfvBatType
*!
*!**************************************************************************
*
FUNCTION lfvBatType
PARAMETERS loFormSet


*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
*
FUNCTION lfvNew
PARAMETERS loFormSet

*** to add new batch number

IF loFormSet.ActiveMode='V'
 lcScFields = loFormSet.lcScFields
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
ENDIF

loFormSet.laData[1]    = ""

loFormSet.ChangeMode('A')

loFormSet.Ariaform1.Refresh()

*!**************************************************************************
*!
*!      Function: lfvTrans
*!
*!**************************************************************************
*
FUNCTION lfvTrans
PARAMETERS loFormSet

*** branch to the transation screen
*** in case of the batch status was not changed or batch start/end dates
*** was not changed.

*** check year , start (date or period) and end (date or period)
IF loFormSet.laData[4] <> glBatch.cBatpYr  .OR. ;
   loFormSet.laData[5] <> glBatch.dBatpBeg .OR. ;
   loFormSet.laData[6] <> glBatch.dBatpEnd

  * Batch Start/End date is changed, you have to save first..!
  * < OK >
  =gfModalGen("TRM02180B00000","DIALOG")

  RETURN .F.
ENDIF

*** check batch type

IF loFormSet.laData[3] <> glBatch.cBatType
  * Batch type was changed, you have to save first..!
  * < OK >
  =gfModalGen("TRM02185B00000","DIALOG")

  RETURN .F.
ENDIF

*** check the batch posting boundaries if any period was locked.
IF loFormSet.laData[2] = "E" .AND. loFormSet.llPeriodLock
  *** Batch posting boundaries has period ð locked.
  *** You cannot add any transactions until you change posting
  *** boundaries or unlock this period.
  * < OK >
  =gfModalGen("TRM02205B00000","DIALOG",lcPeriodLock)
  RETURN .F.
ENDIF

IF !lfCheckRec(loFormSet)    && check the current batch status.
  RETURN .F.
ENDIF

lcMode  = IIF(loFormSet.ActiveMode='V','T','F')
lnTrnHdRec = STR(RECNO('GLTRNSHD'))
lnTrnDtRec = STR(RECNO('GLTRNSDT'))


lcRunPrg = lfGetPrg('GL\GLTRANS.FXP')
DO (lcRunPrg) WITH loFormSet.laData[1],lnTrnHdRec,lnTrnDtRec,lcMode

=lfBatchRefresh(loFormSet)

************************************************************
*! Name      : lpDelScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/14/2012
*! Purpose   : delete function
************************************************************
FUNCTION lpDelScr
PARAMETERS loFormSet

*** local delete FUNCTION  to void the current batch.
*** void current batch. (logical deletion)

IF lfCheckRec(loFormSet)    && check the current batch status.

  loFormSet.ChangeMode('S')
  REPLACE GLBATCH.cBatStat WITH 'V'
  =gfAdd_Info('GLBATCH')
  =gfTableUpdate()
  =gfObj_Lock(.F.)

   SELECT GLTRNSHD
   SET ORDER TO TAG BATCHTRN
   SELECT GLBATCH
   SET RELATION TO glbatch.cbatchno INTO GLTRNSHD ADDITIVE
   SELECT GLTRNSHD

   SCAN REST WHILE glbatch.cbatchno = GLTRNSHD.cBatchNo
     REPLACE GLTRNSHD.cTrnStat WITH 'V'
     =gfAdd_Info('GLTRNSHD')
   ENDSCAN
   *B612070,1 MMT 02/26/2020 Fix bug of GLTRNSHD status field is not updated when batch is voided[T20200219.0009][Start]
   SELECT GLTRNSHD
   =gfTableUpdate()
   *B612070,1 MMT 02/26/2020 Fix bug of GLTRNSHD status field is not updated when batch is voided[T20200219.0009][End]
   SELECT GLBATCH
   SET RELATION TO
ENDIF

*- End of lpDelScr.

************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/07/2012
*! Purpose   : BeforeSave Form method
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet

*** local FUNCTION  to save the current batch
SELECT FISHD
SEEK (loFormSet.laData[4])
SELECT GLBATCH

*** check the start date (ladata[5]) falls in the selected year (ladata[4])
IF !BETWEEN(loFormSet.laData[5],FISHD.dFisBgDat,FISHD.dFisEnDat)
  * Start ð must fall in selected year.
  * < OK >
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *=gfModalGen("TRM02153B00000","DIALOG",IIF(loFormSet.llDate,'date','period'))
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM02153B00000","DIALOG",IIF(loFormSet.llDate,LANG_DATE ,LANG_PERIOD))
=gfModalGen("TRM02153B00000","DIALOG",IIF(loFormSet.llDate,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DATE,loFormSet.GetHeaderText("LANG_DATE",loFormSet.HeaderAlias)) ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERIOD,loFormSet.GetHeaderText("LANG_PERIOD",loFormSet.HeaderAlias))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]

  *** check if the program working with periods format or dates format.
  IF loFormSet.llDate
    loFormSet.lAriaform1.aData5.Setfocus()
  ELSE
    loFormSet.Ariaform1.lcStartPrd.Setfocus()
  ENDIF

  llcSave = .F.
  SELECT GLBATCH
  RETURN llcSave
ENDIF

*** check the end date ladata[6] falls in the selected year (ladata[4])
IF !BETWEEN(loFormSet.laData[6],FISHD.dFisBgDat,FISHD.dFisEnDat)
  * End ð must fall in selected year.
  * < OK >
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *=gfModalGen("TRM02154B00000","DIALOG",IIF(loFormSet.llDate,'date','period'))
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM02154B00000","DIALOG",IIF(loFormSet.llDate,LANG_DATE ,LANG_PERIOD))
=gfModalGen("TRM02154B00000","DIALOG",IIF(loFormSet.llDate,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DATE,loFormSet.GetHeaderText("LANG_DATE",loFormSet.HeaderAlias)) ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERIOD,loFormSet.GetHeaderText("LANG_PERIOD",loFormSet.HeaderAlias))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
  *** check if the program working with periods format or dates format.
  IF loFormSet.llDate
    loFormSet.Ariaform1.laData6.Setfocus()
  ELSE
    loFormSet.Ariaform1.lcEndPrd.Setfocus()
  ENDIF

  llcSave = .F.
  SELECT GLBATCH
  RETURN llcSave
ENDIF

*** check if the start date (ladata[4]) greaater
*** than the end date (loFormSet.ladata[6])
IF loFormSet.laData[5] > loFormSet.laData[6]

  *** check if the program working with periods format or dates format.
  IF loFormSet.llDate
    loFormSet.Ariaform1.laData5.Setfocus()
    * Beginning date should be less than the end date.
    * < OK >
    =gfModalGen("TRM02009B00000","DIALOG")
  ELSE
    loFormSet.Ariaform1.lcStartPrd.Setfocus()
    * Beginning period should be less than the end period.
    * < OK >
    =gfModalGen("TRM02155B00000","DIALOG")
  ENDIF

  llcSave = .F.
  SELECT GLBATCH
  RETURN llcSave
ENDIF

*** check if the audit total not equal the transations amounts.
IF loFormSet.laData[7] <> loFormSet.laData[10] AND loFormSet.laData[7] <> loFormSet.laData[11] AND loFormSet.laData[2] <>'E'
  * The transaction amounts do not equal the Audit total.
  * < Proceed >  < Cancel >
  IF gfModalGen("TRM02168B00012","DIALOG") = 2
    llcSave = .F.
    loFormSet.Ariaform1.laData7.Setfocus()
    SELECT GLBATCH
    RETURN llcSave
  ENDIF
ENDIF

*** check the batch posting boundaries if any period was locked.
SELECT  FSPRD
SET ORDER TO COMFYRPRDI

SEEK (loFormSet.laData[4]+loFormSet.lcStartPrd)

llPerLock = .F.

SCAN REST FOR cFisfYear  = loFormSet.laData[4]  .AND. ;
              cFspPrdID <= loFormSet.lcEndPrd

  IF lFspLocks
    llPerLock = .T.
    EXIT
  ENDIF
ENDSCAN

IF llPerLock
  * Period ð is manually locked.
  * < OK >
  =gfModalGen("TRM02157B00000","DIALOG",cFspPrdID+'-'+loFormSet.laData[4])
  llcSave = .F.
  IF cFspPrdID = loFormSet.lcStartPrd
    *** check if the program working with periods format or dates format.
    IF loFormSet.llDate
      loFormSet.Ariaform1.laData5.Setfocus()
    ELSE
      loFormSet.Ariaform1.lcStartPrd.Setfocus()
    ENDIF
  ELSE
    *** check if the program working with periods format or dates format.
    IF loFormSet.llDate
      loFormSet.Ariaform1.laData6.Setfocus()
    ELSE
      loFormSet.Ariaform1.lcEndPrd.Setfocus()
    ENDIF
  ENDIF
  SELECT GLBATCH
  RETURN llcSave
ENDIF
SELECT GLBATCH


*** check if the start date ladata[5]
*** falls before the beginning balance date from glSetup.dSetBBDat.
*** and check the status flag of glSetup.dSetBBDat
IF loFormSet.ladata[5] < glSetup.dSetBBDat
  IF glSetup.lSetPBBBd
    * ð date falls before the beginning balance date ð.
    * < OK >
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *=gfModalGen("TRM02159B00000","Dialog","Start|"+DTOC(glSetup.dSetBBDat))
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM02159B00000","Dialog",LANG_1_START+DTOC(glSetup.dSetBBDat))
=gfModalGen("TRM02159B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_1_START,loFormSet.GetHeaderText("LANG_1_START",loFormSet.HeaderAlias))+DTOC(glSetup.dSetBBDat))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
  ELSE
    * ð date falls before the beginning balance date ð.
    * Posting before the beginning balance date is not allowed.
    * < OK >
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *=gfModalGen("TRM02202B00000","Dialog","Start|"+DTOC(glSetup.dSetBBDat))
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM02202B00000","Dialog",LANG_1_START+DTOC(glSetup.dSetBBDat))
=gfModalGen("TRM02202B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_1_START,loFormSet.GetHeaderText("LANG_1_START",loFormSet.HeaderAlias))+DTOC(glSetup.dSetBBDat))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
    *** check if the program working with periods format or dates format.
    IF loFormSet.llDate
      loFormSet.Ariaform1.laData5.Setfocus()
    ELSE
      loFormSet.Ariaform1.lcStartPrd.Setfocus()
    ENDIF
    llcSave = .F.
    RETURN llcSave
  ENDIF
ENDIF

*** check if the difference between the strat date and end date
*** falls in the boundaries posting window (GLSETUP.nSetBatPw).
IF (VAL(loFormSet.lcEndPrd)-VAL(loFormSet.lcStartPrd))+1 > GLSETUP.nSetBatPw
  * The batch posting window cannot exceed ð periods.
  * < OK >
  =gfModalGen("TRM02152B00000","DIALOG",STR(GLSETUP.nSetBatPw))

  *** check if the program working with periods format or dates format.
  IF loFormSet.llDate
    loFormSet.Ariaform1.laData6.Setfocus()
  ELSE
    loFormSet.Ariaform1.lcEndPrd.Setfocus()
  ENDIF

  llcSave = .F.
  SELECT GLBATCH
  RETURN llcSave
ENDIF

*** check if the user is forced to enter the audit total filed
*** in case of loFormSet.ladata[7] is empty.
IF glSetUp.lSetCnTot .AND. EMPTY(loFormSet.laData[7])
  * You have to enter the audit total.
  * < OK >
  =gfModalGen("TRM02146B00000","DIALOG")
   llcSave = .F.
  loFormSet.Ariaform1.laData7.Setfocus()
  SELECT GLBATCH
  RETURN llcSave
ENDIF
*- End of lfFormBeforeSave.
************************************************************
*! Name      : lpSavScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/14/2012
*! Purpose   : lpSavScr
************************************************************
FUNCTION lpSavScr
PARAMETERS loFormset

SELECT GLBATCH
IF loFormSet.ActiveMode='A'     && Add Mode  - New Batch sequence.
  APPEND BLANK

  loFormSet.laData[1]  = gfSequence('CBATCHNO')
  loFormSet.Ariaform1.laData1.Refresh()

  lcScFields = loFormSet.lcScFields
  GATHER FROM loFormSet.laData FIELDS &lcScFields
  =gfAdd_Info('GLBATCH')
  =gfTableUpdate()
  loFormSet.lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time
  =gfObj_Lock(.T.)
  * Batch saved with number ð.
  * Do you want to add transactions to this batch now ?
  * < Yes > < No >
  IF gfModalGen("TRM02162B00006","DIALOG",loFormSet.laData[1]) = 1
    *laScrMode     = .F.
    *loFormSet.ActiveMode='E'
    loFormSet.ChangeMode('E')

    loFormSet.Ariaform1.Refresh()
    llCSave = .F.
    *** update the audit information in the variable lcStamp
    lcStamp = glBatch.cAdd_User+DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time
    lcMode  = IIF(loFormSet.ActiveMode='V','T','F')
    *** branching to the transation screen.

    lcRunPrg = lfGetPrg('GL\GLTRANS.FXP')
    DO (lcRunPrg) WITH loFormSet.laData[1],lcMode

    =lfBatchRefresh(loFormSet)

  ENDIF
ELSE      &&  Edit mode  - Old batch number.
  lcScFields = loFormSet.lcScFields
  GATHER FROM loFormSet.laData FIELDS &lcScFields
  =gfAdd_Info('GLBATCH')
  =gfTableUpdate()
  =gfObj_Lock(.F.)
ENDIF

SELECT GLBATCH

*- End of lpSavScr.

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
FUNCTION lfGetPrg
PARAMETERS lcPrg
LOCAL lcRunPrg
  IF oAriaApplication.Multiinst AND FILE(oAriaApplication.ClientApplicationHome+lcPrg)
    lcRunPrg = oAriaApplication.ClientApplicationHome+lcPrg
  ELSE
    lcRunPrg = oAriaApplication.ApplicationHome+lcPrg
  ENDIF
RETURN lcRunPrg
 *- End of lfGetScx.


*!**************************************************************************
*!
*!      FUNCTION : lfCheckRec
*!
*!**************************************************************************
*

FUNCTION lfCheckRec
PARAMETERS loFormSet

lcLoclShow = "lpShow"

loFormSet.oToolBar.cmdDelete.Enabled = 'ENABLE' = loFormSet.lcObjStatus

*** function to check the batch status.
IF ! loFormSet.ActiveMode='V'
  RETURN
ENDIF

IF EMPTY(loFormSet.laData[1])
  RETURN
ENDIF

IF RECNO() <= RECCOUNT()
  GOTO RECNO()
ENDIF

*** check if the displayed batch is removed (voided) by another workstation.
IF loFormSet.laData[1] <> glbatch.cBatchNo
  * This ð was ð by another user .
  * < Yes > < No >
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	  =gfModalGen("TRM02187B00000","DIALOG",;
*!*	     "batch"+'|'+"voided")
  =gfModalGen("TRM02187B00000","DIALOG",;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCH,loFormSet.GetHeaderText("LANG_BATCH",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOIDED,loFormSet.GetHeaderText("LANG_VOIDED",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
  lcScFields = loFormSet.lcScFields
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
  *laScrMode = .F.
  *loFormSet.ActiveMode='S'
  loFormSet.ChangeMode('S')

  loFormSet.Ariaform1.Refresh()
  RETURN .F.
ENDIF

IF ( loFormSet.lcBStamp = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time ) ;
   .AND. ( loFormSet.laData[2] = glbatch.cbatstat )
  RETURN .T.
ENDIF

lcAdd_User = ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glBatch.cAdd_User,SYUUSER.cUser_Id))
lcAdd_Time = glBatch.cAdd_Time
lcAdd_Date = dtoc(glBatch.dAdd_Date)
lcPos_User = ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glBatch.cPostUser,SYUUSER.cUser_Id))
lcPos_Time = glBatch.cPostTime
lcPos_Date = dtoc(glBatch.dPostDate)

=gfObj_Lock(.F.)

IF glbatch.cbatstat = 'V'
  * This ð was ð by user ð at ð.
  * < Ok >
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	  =gfModalGen("TRM02167B00000","DIALOG",;
*!*	     "batch"+'|'+"voided"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  =gfModalGen("TRM02167B00000","DIALOG",;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCH,loFormSet.GetHeaderText("LANG_BATCH",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOIDED,loFormSet.GetHeaderText("LANG_VOIDED",loFormSet.HeaderAlias))+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  lcScFields = loFormSet.lcScFields
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
  *laScrMode = .F.
  *loFormSet.ActiveMode='S'
  loFormSet.ChangeMode('S')

  loFormSet.Ariaform1.Refresh()
  RETURN .F.
ENDIF


*** check if the displayed batch was approved by another user
*** from another or same workstaion.
IF glbatch.cbatstat = 'A'
  * This ð was ð by user ð at ð.
  * < Ok >
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	  =gfModalGen("TRM02167B00000","DIALOG",;
*!*	     "batch"+'|'+"approved"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  =gfModalGen("TRM02167B00000","DIALOG",;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCH,loFormSet.GetHeaderText("LANG_BATCH",loFormSet.HeaderAlias)) +'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APPROVED,loFormSet.GetHeaderText("LANG_APPROVED",loFormSet.HeaderAlias))+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
  lcScFields = loFormSet.lcScFields
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
  loFormSet.lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time
  *laScrMode = .F.
  *loFormSet.ActiveMode='E'
  loFormSet.ChangeMode('E')

  loFormSet.Ariaform1.Refresh()
  RETURN .F.
ENDIF


*** check if the displayed batch was posted by another user
*** from another or same workstaion.
IF glbatch.cbatstat = 'P'
  * This ð was ð by user ð at ð.
  * < Ok >
   *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	  =gfModalGen("TRM02167B00000","DIALOG",;
*!*	     "batch"+'|'+"posted"+'|'+lcPos_User+'|'+lcPos_Date+' '+lcPos_Time)
  =gfModalGen("TRM02167B00000","DIALOG",;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCH,loFormSet.GetHeaderText("LANG_BATCH",loFormSet.HeaderAlias)) +'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTED,loFormSet.GetHeaderText("LANG_POSTED",loFormSet.HeaderAlias))+'|'+lcPos_User+'|'+lcPos_Date+' '+lcPos_Time)
*N000682,1 11/20/2012 MMT Globlization changes[End]

   *N000682,1 MMT 11/22/2012 Globalization changes[ENd]
  lcScFields = loFormSet.lcScFields
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
  loFormSet.lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time
  *laScrMode = .F.
  *loFormSet.ActiveMode='E'
  loFormSet.ChangeMode('E')

  loFormSet.Ariaform1.Refresh()
  RETURN .F.
ENDIF

*** in case the batch status was changed.
IF glbatch.cbatstat $ 'HOEU'
  * ð status has been changed by user ð at ð.
  * < Ok >
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	  =gfModalGen("TRM02181B00000","DIALOG",;
*!*	  "batch"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  =gfModalGen("TRM02181B00000","DIALOG",;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCH,loFormSet.GetHeaderText("LANG_BATCH",loFormSet.HeaderAlias)) +'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
  lcScFields = loFormSet.lcScFields
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
  loFormSet.lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time

  loFormSet.Ariaform1.Refresh()
  RETURN .F.

ENDIF


*!**************************************************************************
*!
*!      FUNCTION: lfCpEdit
*!
*!**************************************************************************
*

FUNCTION lfCpEdit
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

SELECT GLBATCH
IF RECNO() <= RECCOUNT()
  GOTO RECNO()
ENDIF

loFormSet.llPeriodLock = .F.
lcPeriodLock = " "

SELECT  FSPRD
SET ORDER TO COMFYRPRDI
SEEK (loFormSet.laData[4]+loFormSet.lcStartPrd)
SCAN REST FOR cFisfYear  = loFormSet.laData[4]  .AND. ;
              cFspPrdID <= loFormSet.lcEndPrd

  IF lFspLocks
    loFormSet.llPeriodLock = .T.
    lcPeriodLock = cFspPrdID+'-'+loFormSet.laData[4]
    EXIT
  ENDIF
ENDSCAN

IF loFormSet.laData[2] <> "E" .AND. loFormSet.llPeriodLock
  * Batch posting boundaries has period ð locked.
  * You cannot edit this batch until you unlock this period.
  * < Ok >
  =gfModalGen("TRM02198B00000","DIALOG",lcPeriodLock)
  SELECT (lnSlct)
  RETURN .F.
ENDIF
SELECT (lnSlct)
