*:************************************************************************
*:  Program File: ARIA4XP\PRGS\GL\GLBPOST.PRG
*:  Module      : General Ledger
*:  Desc.       : posting -> Journal Batches
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 05/15/2012
*:  Reference   : *E303151,1
*:************************************************************************
* Modifications
*B610461,1 TMI 08/15/2013 close the loFormSet.lcReportFi temp file[T20130627.0025 ]
*B610497,1 Correct the fix B610461 TMI 09/04/2013 [T20130822.0015] 
*:************************************************************************
*- Call the screen
PARAMETERS lcReportFi
*- Call the screen
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
#INCLUDE R:\ARIA4XP\PRGS\gl\GLBPOST.H
*N000682,1 MMT 11/22/2012 Globalization changes[END]

lcRunScx = lfGetScx("GL\GLBPOST.scx")

IF !EMPTY(lcReportFi)
  PRIVATE oScr
  *N000682,1 12/13/12 TMI Globlization changes[Start] add the correct parameter
  *DO FORM (lcRunScx) WITH llDumyPost NAME oScr NOSHOW
  DO FORM (lcRunScx) WITH lcReportFi NAME oScr NOSHOW  
  *N000682,1 12/13/12 TMI Globlization changes[End  ] 
  IF TYPE('oScr')='O' AND !ISNULL(oScr)
    oScr.Show(1)
  ENDIF
ELSE

  DO FORM (lcRunScx) WITH lcReportFi

ENDIF

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

llDumyPost = IIF(TYPE('loFormSet.lcReportFi')='C',.T.,.F.)
loFormSet.AddProperty('llDumyPost',llDumyPost)

loFormSet.AddProperty('lcProgName','GLBPOST')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE

*- Open tables
=lfOpenPRGFILES(loFormSet.lcProgName)

IF !lfGL(loFormset)
  RETURN .F.
ENDIF


SELECT GLTYPES
GO TOP
IF EOF()
  *** The types and ranges have not ***
  *** been setup yet.  You have to  ***
  *** define the accounts type and ranges first. ***
  *** < Ok > ***
  =gfModalGen("TRM02038B00000","DIALOG")

  RETURN .F.
ENDIF


*** Load program base file
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))


*** To know if the prog. calling from the menu or account validation. ***
*** to know which screen to call the modal screen or the nonmodal one ***

*- Define needed variables.
=lfDefineVars(loFormSet)

WITH loFormSet.Ariaform1
IF loFormSet.llDumyPost
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *  .pbApprov.Caption = 'Inclu\<de'
  *  .pbAll.Caption    = 'Include a\<ll'
  *  .pbNone.Caption   = 'Include \<none'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.pbApprov.Caption =LANG_GLBPOST_INCLUDE
.pbApprov.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_INCLUDE,loFormSet.GetHeaderText("LANG_GLBPOST_INCLUDE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.pbAll.Caption    =LANG_GLBPOST_INCLUDE_ALL
.pbAll.Caption    =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_INCLUDE_ALL,loFormSet.GetHeaderText("LANG_GLBPOST_INCLUDE_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.pbNone.Caption   =LANG_GLBPOST_INCLUDE_NONE
.pbNone.Caption   =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_INCLUDE_NONE,loFormSet.GetHeaderText("LANG_GLBPOST_INCLUDE_NONE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *  .pbApprov.Caption = '\<Approve'
  *  .pbAll.Caption    = 'Approve a\<ll'
  *  .pbNone.Caption   = 'Approve \<none'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.pbApprov.Caption =LANG_GLBPOST_APPROVE
.pbApprov.Caption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_APPROVE,loFormSet.GetHeaderText("LANG_GLBPOST_APPROVE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.pbAll.Caption    =LANG_GLBPOST_APPROVE_ALL
.pbAll.Caption    =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_APPROVE_ALL,loFormSet.GetHeaderText("LANG_GLBPOST_APPROVE_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.pbNone.Caption   =LANG_GLBPOST_APPROVE_NONE
.pbNone.Caption   =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_APPROVE_NONE,loFormSet.GetHeaderText("LANG_GLBPOST_APPROVE_NONE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
ENDWITH


SELECT GLBATCH

*** searches the DBF (glBatch) for the first record
*** that matches a given expression.
*** --> types  (glBatch.cBatType ) , status (glBatch.cBatStat )


LOCATE FOR EVALUATE(loFormSet.lcScope)
llRet = .T.
IF FOUND()

  *- Fill the temp file lcGLBATCH with the needed ines to be posted
  =lfAppFilt(loFormSet)

  =lfSetGridDataSource(loFormSet)
ELSE
  *** if there is no batches to post.
  =gfModalGen("TRM02063B00000","Dialog")
  llRet = .F.
ENDIF

glQuitting  = .T.

SELECT GLBATCH
RETURN llRet

*- End of lfFormInit.
************************************************************
*! Name      : lfFormDestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : lfFormDestroy
************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormSet

*B610497,1 Correct the fix B610461,T20130822.0015 TMI 09/04/2013 [Start] 
IF TYPE('loformset.lcReportFi')='C' AND !EMPTY(loformset.lcReportFi)
  *B610497,1 Correct the fix B610461,T20130822.0015 TMI 09/04/2013 [End  ] 
  *B610461,1 TMI 08/15/2013 [Start] Close the loFormSet.lcReportFi temp file
  IF USED(loFormSet.lcReportFi)
    USE IN (loFormSet.lcReportFi)
  ENDIF   
  *B610461,1 TMI 08/15/2013 [End  ] 
  *B610497,1 Correct the fix B610461,T20130822.0015 TMI 09/04/2013 [Start] 
ENDIF 
*B610497,1 Correct the fix B610461,T20130822.0015 TMI 09/04/2013 [End  ]   

lc_ToPost = loFormSet.lc_ToPost
gcWorkdir = oAriaApplication.WorkDir
IF USED(lc_ToPost)
  USE IN ALIAS (lc_ToPost)
ENDIF
ERASE &gcWorkdir.&lc_ToPost..DBF
ERASE &gcWorkdir.&lc_ToPost..CDX
*- End of lfFormDestroy.

************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 22/05/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loFormSet
  loFormSet.Ariaform1.grdGLBATCH.RecordSource = "GLBATCH"
  *N000682,1 12/13/2012 TMI Globlization changes[Start] 
  *lfSetColumnsProp('1','cBatchNo'   ,'Batch#',6*10)
  *lfSetColumnsProp('2','lfCheckEL()','E/L'   ,3*10)
  *IF loFormSet.llDumyPost
  *  lfSetColumnsProp('3','lBatInd'       ,'Included')
  *ELSE
  *  lfSetColumnsProp('3','cBatStat = "A"','Approved',60)
  *ENDIF
  *lfSetColumnsProp('4','cSrcmodul','S/M'        ,3*10 )
  *lfSetColumnsProp('5','cComp_ID' ,'Co.'        ,3*10 )
  *lfSetColumnsProp('6','DBatpBeg' ,'  From'     ,80)
  *lfSetColumnsProp('7','DBatpEnd' ,'  To'       ,80)
  *lfSetColumnsProp('8','CBatRefer','  Reference',10*25)  
  lcCaption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_COL1,loFormSet.GetHeaderText("LANG_GLBPOST_COL1",loFormSet.HeaderAlias))
  lfSetColumnsProp('1','cBatchNo'   ,lcCaption,6*10)
  lcCaption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_COL2,loFormSet.GetHeaderText("LANG_GLBPOST_COL2",loFormSet.HeaderAlias))
  lfSetColumnsProp('2','lfCheckEL()',lcCaption,3*10)
  IF loFormSet.llDumyPost
    lcCaption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_COL3_1,loFormSet.GetHeaderText("LANG_GLBPOST_COL3_1",loFormSet.HeaderAlias))
    lfSetColumnsProp('3','lBatInd'       ,lcCaption)
  ELSE
    lcCaption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_COL3_2,loFormSet.GetHeaderText("LANG_GLBPOST_COL3_2",loFormSet.HeaderAlias))
    lfSetColumnsProp('3','cBatStat = "A"',lcCaption,60)
  ENDIF
  lcCaption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_COL4,loFormSet.GetHeaderText("LANG_GLBPOST_COL4",loFormSet.HeaderAlias))
  lfSetColumnsProp('4','cSrcmodul',lcCaption     ,3*10 )
  lcCaption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_COL5,loFormSet.GetHeaderText("LANG_GLBPOST_COL5",loFormSet.HeaderAlias))
  lfSetColumnsProp('5','cComp_ID' ,lcCaption    ,3*10 )
  lcCaption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_COL6,loFormSet.GetHeaderText("LANG_GLBPOST_COL6",loFormSet.HeaderAlias))
  lfSetColumnsProp('6','DBatpBeg' ,lcCaption    ,80)
  lcCaption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_COL7,loFormSet.GetHeaderText("LANG_GLBPOST_COL7",loFormSet.HeaderAlias))
  lfSetColumnsProp('7','DBatpEnd' ,lcCaption    ,80)
  lcCaption =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_COL8,loFormSet.GetHeaderText("LANG_GLBPOST_COL8",loFormSet.HeaderAlias))
  lfSetColumnsProp('8','CBatRefer',lcCaption   ,10*25)
  *N000682,1 12/13/2012 TMI Globlization changes[End  ] 
  loFormSet.Ariaform1.grdGLBATCH.ReadOnly = .T.
*- End of lfSetGridDataSource.

************************************************************
*! Name      : lfSetColumnsProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : Set Columns Properties
************************************************************
FUNCTION lfSetColumnsProp
PARAMETERS lcCol,lcSrc,lcHeader,lnWidth
lnWidth = IIF(EMPTY(lnWidth),50,lnWidth)
WITH loFormSet.Ariaform1.grdGLBATCH
  .Column&lcCol..Header1.Caption = lcHeader
  .Column&lcCol..ControlSource   = lcSrc
  .Column&lcCol..Width           = lnWidth
ENDWITH
*- End of lfSetColumnsProp.

************************************************************
*! Name      : lfAfterRowColChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : After Row Col Change
************************************************************
FUNCTION lfAfterRowColChange
PARAMETERS loFormSet
*refresh the grid
loFormset.Ariaform1.grdGLBATCH.Refresh()

WITH loFormSet.Ariaform1

  IF !EOF('GLBATCH')

    .Ariatextbox1.Value  = glBatch.cbatchno
    .Ariatextbox2.Value  = IIF (glBatch.cbatstat $ 'UOA',loFormSet.la_TBtStat[AT(glBatch.cbatstat,'UOA')],'')
    .Ariatextbox3.Value  = glBatch.cbatpyr
    .Ariatextbox4.Value  = glBatch.dbatpbeg
    .Ariatextbox5.Value  = glBatch.dbatpend
    .Ariatextbox6.Value  = glBatch.nbatcntot
    .Ariatextbox7.Value  = glBatch.cbatrefer
    .Ariatextbox8.Value  = glBatch.cbatdesc
    .Ariatextbox9.Value  = loFormSet.la_TPostYr[IIF(glBatch.cbatpyr=loFormSet.lcCurr_yer,2,IIF(VAL(glBatch.cbatpyr)=VAL(loFormSet.lcCurr_yer)-1,1,IIF(VAL(glBatch.cbatpyr)=VAL(loFormSet.lcCurr_yer)+1,3,4)))]
    .Ariatextbox10.Value = IIF (glBatch.cBatType  $ 'NSL',loFormSet.la_TBtType[AT(cbattype,'NSL ')],'')
    .Ariatextbox11.Value = glBatch.nbatotdr
    .Ariatextbox12.Value = glBatch.nbatotcr
    .Ariatextbox13.Value = ABS(glBatch.nbatotdr-glBatch.nbatotcr)
    .Ariatextbox14.Value = IIF(glBatch.nbatotdr>glBatch.nbatotcr,'Dr',IIF(glBatch.nbatotdr<glBatch.nbatotcr,'Cr',' '))

  ELSE

    =lfNoBatches(loFormSet,.F.)
    STORE '' TO .Ariatextbox1.Value  ,;
                .Ariatextbox2.Value  ,;
                .Ariatextbox3.Value  ,;
                .Ariatextbox4.Value  ,;
                .Ariatextbox5.Value  ,;
                .Ariatextbox6.Value  ,;
                .Ariatextbox7.Value  ,;
                .Ariatextbox8.Value  ,;
                .Ariatextbox9.Value  ,;
                .Ariatextbox10.Value ,;
                .Ariatextbox11.Value ,;
                .Ariatextbox12.Value ,;
                .Ariatextbox13.Value ,;
                .Ariatextbox14.Value

  ENDIF

  *check if we in dummy post mode
  IF loFormSet.llDumyPost
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *.pbApprov.Caption = IIF(glBatch.lBatInd ,'\<Exclude','Inclu\<de')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.pbApprov.Caption = IIF(glBatch.lBatInd ,LANG_GLBPOST_S_EXCLUDE,LANG_GLBPOST_INCLUDE)
.pbApprov.Caption = IIF(glBatch.lBatInd ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_S_EXCLUDE,loFormSet.GetHeaderText("LANG_GLBPOST_S_EXCLUDE",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_INCLUDE,loFormSet.GetHeaderText("LANG_GLBPOST_INCLUDE",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
  ELSE
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *.pbApprov.Caption = IIF(glBatch.cBatStat = 'A','\<Disapprove','\<Approve')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.pbApprov.Caption = IIF(glBatch.cBatStat = 'A',LANG_GLBPOST_S_DISAPPROVE,LANG_GLBPOST_APPROVE)
.pbApprov.Caption = IIF(glBatch.cBatStat = 'A',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_S_DISAPPROVE,loFormSet.GetHeaderText("LANG_GLBPOST_S_DISAPPROVE",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_APPROVE,loFormSet.GetHeaderText("LANG_GLBPOST_APPROVE",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[enD]
  ENDIF


ENDWITH

*- End of lfAfterRowColChange.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/15/2012
*! Purpose   : Define needed Variables in the screen
*************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

loFormSet.AddProperty('la_TBtStat[3]','')  && to display status       for each transation
loFormSet.AddProperty('la_TBtType[3]','')  && to display types        for each transation
loFormSet.AddProperty('la_TPostYr[4]','') && to display posting year for each transation

*** Arrays used by mover :
loFormSet.AddProperty('laSApp_ID[1]','') && source array of applications names
loFormSet.AddProperty('laTApp_ID[1]','') && source array of applications names

loFormSet.la_TBtStat [1] = 'Unposted'          && Case status --> 'U'
loFormSet.la_TBtStat [2] = 'Out of balance'    && Case status --> 'O'
loFormSet.la_TBtStat [3] = 'Approved'          && Case status --> 'A'

loFormSet.la_TBtType [1] = 'Normal'            && Case Type  ---> 'N'
loFormSet.la_TBtType [2] = 'Statistical'       && Case Type  ---> 'S'
loFormSet.la_TBtType [3] = 'Subledger'         && Case Type  ---> 'L'

loFormSet.la_TPostYr [1] = 'Previous'      && Case Posting year = lcCurr_yer -1
loFormSet.la_TPostYr [2] = 'Current'       && Case Posting year = lcCurr_yer
loFormSet.la_TPostYr [3] = 'Next '         && Case Posting year = lcCurr_yer +1
loFormSet.la_TPostYr [4] = '  '            && Case else

loFormSet.AddProperty('lcEditLine'    , .F.)  && init. edit list var.
loFormSet.AddProperty('lcCurrBat'     , '')   && set the current record in browse to 0
loFormSet.AddProperty('lcSrcModul'    , '')  && string to store the active suorce modual(s) names
loFormSet.AddProperty('lc_ToPost'     , gfTempName())
*loFormSet.AddProperty('lcGLBATCH'     , gfTempName())


loFormSet.AddProperty('lcModName'     , "General ledger" )  && String to store general ladger module name

loFormSet.AddProperty('laCtrStat'     , 'DISABLE') && To disable the browse pad in the menu

loFormSet.AddProperty('lnRecNo'       , 0)

loFormSet.AddProperty('llprint'       , .T. )

loFormSet.AddProperty('laTmpSMod[1,2]','')
loFormSet.AddProperty('laTmpSMod'  , '')
*loFormSet.AddProperty('lc_TBrowt'  , '')
loFormSet.AddProperty('lcFsWindow' ,'')
loFormSet.AddProperty('lcScope'    ,'')

loFormSet.AddProperty('llGlTrOpen' , .F.)


=gfOpenTable(oAriaApplication.SysPath+'SyGLTran','SYGLTRAN','SH')   && CFILE_NAM
=gfOpenTable(oAriaApplication.SysPath+'SYDAPPL' ,'CAPP_ID' ,'SH')

SELECT DISTINCT SyGLTran.cSrcModul,SydAppl.cApp_Name ;
  FROM SyGLTran,SydAppl ;
  INTO ARRAY loFormSet.laTmpSMod  ;
  WHERE SyGLTran.cSrcModul = SydAppl.cApp_ID

IF !EMPTY(loFormSet.laTmpSMod[1,1])

  =gfOpenTable(oAriaApplication.SysPath+'SycComp','CCOMP_ID','SH')
  SELECT SycComp
  LOCATE FOR CCOMP_ID = oAriaApplication.ActiveCompanyID
  IF FOUND()
    PRIVATE lnI,lnDelCnt
    STORE 0 TO lnI,lnDelCnt

    DIMENSION laDumm(ALEN(loFormSet.laTmpSMod,1),1)
    FOR lnI = 1 TO ALEN(loFormSet.laTmpSMod,1)
      IF !(loFormSet.laTmpSMod[lnI,1] $ SycComp.mModlSet)
      laDumm[LNI,1] = .T.
      lnDelCnt = lnDelCnt + 1
      ENDIF
    ENDFOR
    IF lnDelCnt > 0
      lnDelCnt =0
      FOR lnI = 1 TO ALEN(laDumm,1)
        IF laDumm[LNI,1]
          =ADEL(loFormSet.laTmpSMod,lnI)
          lnDelCnt = lnDelCnt+1
        ENDIF
      ENDFOR
      DIMENSION loFormSet.laTmpSMod[ALEN(loFormSet.laTmpSMod,1)-lnDelCnt,2]
    ENDIF

  ELSE
    STORE " " TO loFormSet.laTmpSMod
  ENDIF


ENDIF


DECLARE loFormSet.laSApp_ID[ALEN(loFormSet.laTmpSMod,1)]

FOR lnSMRow = 1 TO ALEN(loFormSet.laTmpSMod,1)
  loFormSet.laSApp_ID[lnSMRow] = ALLTRIM(loFormSet.laTmpSMod[lnSMRow,1])+"-"+ALLTRIM(loFormSet.laTmpSMod[lnSMRow,2])
ENDFOR

*** Create array with the same number of elements in source array.
*** then copy content from source array to target array.

DECLARE loFormSet.laTApp_ID [ALEN(loFormSet.laSApp_ID,1)]
=ACOPY(loFormSet.laSApp_ID,loFormSet.laTApp_ID)

*** collect all source journals names in the string lcSorJourn
FOR lnCounter = 1 TO ALEN(loFormSet.laSApp_ID)
  loFormSet.lcSrcmodul = loFormSet.lcSrcmodul + LEFT(loFormSet.laSApp_ID[lnCounter],3)
ENDFOR

IF loFormSet.llDumyPost
  *N000682,1 12/13/2012 TMI Globlization changes[Start] 
  *lc_TBrowt = "Dummy Batches Posting"  &&  initial the window header.
  lc_TBrowt = ;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_DUMMY ,loFormSet.GetHeaderText("LANG_GLBPOST_DUMMY",loFormSet.HeaderAlias))
  *N000682,1 12/13/2012 TMI Globlization changes[End  ] 
ELSE
  *N000682,1 12/13/2012 TMI Globlization changes[Start] 
  *lc_TBrowt = "Batches Posting"        &&  initial the window header.
  lc_TBrowt = ;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_STANDARD,loFormSet.GetHeaderText("LANG_GLBPOST_STANDARD",loFormSet.HeaderAlias))
  *N000682,1 12/13/2012 TMI Globlization changes[End  ] 
ENDIF
loFormSet.Ariaform1.Caption = lc_TBrowt

*** the string that contaning the scope of the valid posting years
loFormSet.lcFsWindow    = STR(VAL(loFormSet.lcCurr_yer)-1,4)+" "+loFormSet.lcCurr_yer+" "+;
                          STR(VAL(loFormSet.lcCurr_yer)+1,4)

lfSetScope(loFormSet)
*- End of lfDefineVars.

************************************************************
*! Name      : lfSetScope
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : Set the scope
*************************************************************-Set scope
FUNCTION lfSetScope
PARAMETERS loFormSet

loFormSet.lcScope  =      "((cBatType = 'N' .AND. cBatStat $ 'UA' )"  + ;
                          ".OR.  (cBatType = 'L' .AND. cBatStat $ 'UA' )"  + ;
                          ".OR.  (cBatType = 'S' .AND. cBatStat $ 'UAO'))" + ;
                          ".AND. cSrcmodul $ '"+loFormSet.lcSrcmodul+"'"
*- End of lfSetScope.

************************************************************
*! Name      : lfAppFilt
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : Apply filter
************************************************************
FUNCTION lfAppFilt
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

  SELECT GLBATCH
  lcFilt = loFormSet.lcScope
  SET FILTER TO &lcFilt
  LOCATE
  loFormset.Ariaform1.grdGLBATCH.Refresh()

SELECT (lnSlct )
*- End of lfAppFilt.





************************************************************
*! Name      : lfCheckEL
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   :
*** check if GLBATCH.cBatElUsr not empty then
*** lcEditL = IIF ( glBatch.dbaTelDat > glBatch.daDd_Date,' û',;
***           IIF ( glBatch.dbaTelDat = glBatch.daDd_Date .AND.;
***                 glBatch.cbAtelTim > glBatch.caDd_Time,' û','  '))
*** else lcEditl = ' '
************************************************************
FUNCTION  lfCheckEL

IF !EMPTY(glBatch.cBatElUsr)

  lcEditL = IIF ( glBatch.dbaTelDat > glBatch.daDd_Date,.T.,;
            IIF ( glBatch.dbaTelDat = glBatch.daDd_Date .AND.;
                  glBatch.cbAtelTim > glBatch.caDd_Time,.T.,.F.))
ELSE

  lcEditL = .F.

ENDIF
RETURN lcEditL

***********************************************************
*! Name      : lfvAprOne
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : approve current record
************************************************************
FUNCTION  lfvAprOne
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT GLBatch

*** check if the status for (cBatStat) = 'A' --> approve
*** that is mean the user want to disapprove this record.
*** then we will check the locking status, if there is no
*** user use this record, then change the status according to
*** the diffrent between glBatch.nBatotdr,glBatch.nBatotcr
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance

IF loFormSet.llDumyPost
  IF lBatInd
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glBatch.lBatInd WITH .F.
      SELECT GLBatch
      gfTableUpdate()
      =gfObj_Lock(.F.)

    ENDIF
  ELSE
    IF glBatch.cbatpyr $ loFormSet.lcFsWindow
      IF lfObj_Lock() .AND. gfObj_Lock(.T.)
        REPLACE glBatch.lBatInd WITH .T.
        SELECT GLBatch
        gfTableUpdate()
        =gfObj_Lock(.F.)

      ENDIF
    ELSE
      *N000682,1 MMT 11/22/2012 Globalization changes[Start]
      *=gfModalGen("TRM02064B00000","Dialog",IIF(loFormSet.llDumyPost,'include','approve'))
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM02064B00000","Dialog",IIF(loFormSet.llDumyPost,LANG_GLBPOST_S_INCLUDE,LANG_GLBPOST_S_APPROVE))
=gfModalGen("TRM02064B00000","Dialog",IIF(loFormSet.llDumyPost,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_S_INCLUDE,loFormSet.GetHeaderText("LANG_GLBPOST_S_INCLUDE",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_S_APPROVE,loFormSet.GetHeaderText("LANG_GLBPOST_S_APPROVE",loFormSet.HeaderAlias))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 MMT 11/22/2012 Globalization changes[END]
      *** The Batch posting year is out of the posting window.
    ENDIF
  ENDIF

ELSE

  IF cBatStat = 'A'
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glBatch.cBatStat WITH ;
              IIF (glBatch.nbatotdr=glBatch.nbatotcr,'U','O')
      =gfAdd_Info('glbatch')
      SELECT GLBatch
      gfTableUpdate()
      =gfObj_Lock(.F.)

    ENDIF
  ELSE
    *** that is mean the user want to approve this record.
    *** in this case must be check if the glBatch.cbatpyr
    *** in the scope the posting window (previous,current,next)
    *** if !valid display "The Batch posting year is out of the posting window"
    *** else if check list is approved then if valid check the locking status,
    *** if there is no user use this record, then change the status to 'A'

    IF glBatch.cbatpyr $ loFormSet.lcFsWindow
      loFormSet.lcEditLine=lfCheckEL()

      llPrint = IIF (glSetup.lSetForel,loFormSet.lcEditLine,.T.)

      IF llPrint
        IF lfObj_Lock() .AND. gfObj_Lock(.T.)
          REPLACE glBatch.cBatStat WITH  'A'
          =gfAdd_Info('glbatch')
          SELECT GLBatch
          gfTableUpdate()
          =gfObj_Lock(.F.)

        ENDIF
      ELSE
        *N000682,1 MMT 11/22/2012 Globalization changes[Start]
        *=gfModalGen("TRM02065B00000","Dialog",IIF(loFormSet.llDumyPost,'include','approve'))
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM02065B00000","Dialog",IIF(loFormSet.llDumyPost,LANG_GLBPOST_S_INCLUDE ,LANG_GLBPOST_S_APPROVE))
=gfModalGen("TRM02065B00000","Dialog",IIF(loFormSet.llDumyPost,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_S_INCLUDE,loFormSet.GetHeaderText("LANG_GLBPOST_S_INCLUDE",loFormSet.HeaderAlias)) ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_S_APPROVE,loFormSet.GetHeaderText("LANG_GLBPOST_S_APPROVE",loFormSet.HeaderAlias))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        *N000682,1 MMT 11/22/2012 Globalization changes[END]
        *** Printing the edit list is a must to approve.
      ENDIF
    ELSE
      *N000682,1 MMT 11/22/2012 Globalization changes[Start]
      *=gfModalGen("TRM02064B00000","Dialog",IIF(loFormSet.llDumyPost,'include','approve'))
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM02064B00000","Dialog",IIF(loFormSet.llDumyPost,LANG_GLBPOST_S_INCLUDE ,LANG_GLBPOST_S_APPROVE))
=gfModalGen("TRM02064B00000","Dialog",IIF(loFormSet.llDumyPost,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_S_INCLUDE,loFormSet.GetHeaderText("LANG_GLBPOST_S_INCLUDE",loFormSet.HeaderAlias)) ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_S_APPROVE,loFormSet.GetHeaderText("LANG_GLBPOST_S_APPROVE",loFormSet.HeaderAlias))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 MMT 11/22/2012 Globalization changes[eND]
      &&  The Batch posting year is out of the posting window.
    ENDIF
  ENDIF
ENDIF

=lfAfterRowColChange(loFormSet)

SELECT(lnSlct)
RETURN

***********************************************************
*! Name      : lfvAprAll
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : approve all
************************************************************
FUNCTION  lfvAprAll
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT GLBatch

*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and check if the glBatch.cBatPYr in the scope the posting
*** window (previous,current,next), and check list approved then
*** if valid check the locking status, if there is no user use this
*** record, then change the status to 'A' --> approve

lnRecNo = RECNO()

IF loFormSet.llDumyPost
  SCAN  FOR EVALUATE(loFormSet.lcScope)
    IF glBatch.cbatpyr $ loFormSet.lcFsWindow
      IF !glBatch.lLok_Stat
        REPLACE glBatch.lBatInd WITH  .T.
      ENDIF
    ENDIF
  ENDSCAN

ELSE

  SCAN  FOR EVALUATE(loFormSet.lcScope)
    IF glBatch.cbatpyr $ loFormSet.lcFsWindow
      loFormSet.lcEditLine = lfCheckEL()
      llPrint    = IIF (glSetup.lSetForel,loFormSet.lcEditLine,.T.)
      IF llPrint
        IF !glBatch.lLok_Stat
          REPLACE glBatch.cBatStat      WITH  'A'
          =gfAdd_Info('glbatch')
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

SELECT GLBatch
gfTableUpdate()


*** refresh the browse window
GOTO lnRecNo

=lfAfterRowColChange(loFormSet)
SELECT(lnSlct)
RETURN

***********************************************************
*! Name      : lfvAprNon
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : approve no record
************************************************************
FUNCTION  lfvAprNon
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT GLBatch

*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and change the status for every record have a status 'A' to
*** 'U' or 'O' status according to the diffrent between
*** glbatch.nBatotdr,glbatch.nbatotcr
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
lnRecNo = RECNO()

IF loFormSet.llDumyPost
  SCAN  FOR EVALUATE(loFormSet.lcScope)
    IF glBatch.cbatpyr $ loFormSet.lcFsWindow
      IF !glBatch.lLok_Stat
        REPLACE glBatch.lBatInd WITH  .F.
      ENDIF
    ENDIF
  ENDSCAN

ELSE

  SCAN FOR EVALUATE(loFormSet.lcScope)
    IF !glBatch.lLok_Stat
      REPLACE glBatch.cBatStat WITH ;
              IIF (glBatch.nbatotdr=glBatch.nbatotcr,'U','O')
      =gfAdd_Info('glbatch')
    ENDIF
  ENDSCAN
ENDIF

SELECT GLBatch
gfTableUpdate()

*** refresh the browse window

GOTO lnRecNo

=lfAfterRowColChange(loFormSet)
SELECT(lnSlct)
RETURN

*!**************************************************************************
*! Name      : lfvInvert
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : invert approvals
************************************************************
FUNCTION  lfvInvert
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT GLBatch

*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and change the status for every record have a status 'A' to
*** 'U' or 'O' status according to the diffrent between
*** glBatch.nBatotdr,glBatch.nBatotcr
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
lnRecNo = RECNO()

IF loFormSet.llDumyPost

  SCAN FOR EVALUATE(loFormSet.lcScope)
    IF !glBatch.lLok_Stat
      IF glBatch.lBatInd
        REPLACE glBatch.lBatInd WITH .F.
      ELSE
        IF glBatch.cbatpyr $ loFormSet.lcFsWindow
          IF !glBatch.lLok_Stat
            REPLACE glBatch.lBatInd WITH  .T.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN

ELSE

  SCAN FOR EVALUATE(loFormSet.lcScope)
    IF !glBatch.lLok_Stat
      IF glBatch.cBatStat = 'A'
        REPLACE glBatch.cBatStat WITH ;
                IIF (glBatch.nbatotdr=glBatch.nbatotcr,'U','O')
        =gfAdd_Info('glbatch')
      ELSE
        *** that is mean the user want to approve this record.
        *** in this case must be check if the glBatch.cbatpyr
        *** in the scope the posting window (previous,current,next)
        *** if valid then if check list is approved then if valid
        *** check the locking status,
        *** if there is no user use this record, then change the status to 'A'

        IF glBatch.cbatpyr $ loFormSet.lcFsWindow
          loFormSet.lcEditLine=lfCheckEL()
          llPrint = IIF (glSetup.lSetForel,loFormSet.lcEditLine,.T.)

          IF llPrint
            IF !glBatch.lLok_Stat
              REPLACE glBatch.cBatStat WITH  'A'
              =gfAdd_Info('glbatch')
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

*** refresh the browse window

GOTO lnRecNo

=lfAfterRowColChange(loFormSet)
SELECT(lnSlct)
RETURN

*!**************************************************************************
*! Name      : lfvPost
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : post approved records
************************************************************
FUNCTION  lfvPost
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT GLBatch

*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and if found any record with status 'A' then exit from scan loop
lnRecNo = RECNO()

* sometimes the loFormSet.lcScope compes empty
loFormSet.lcScope = IIF(EMPTY(loFormSet.lcScope),FILTER(),loFormSet.lcScope)


IF loFormSet.llDumyPost
  LOCATE FOR  EVALUATE(loFormSet.lcScope) .AND. glBatch.lBatInd
ELSE
  LOCATE FOR  EVALUATE(loFormSet.lcScope) .AND. glBatch.cBatStat = 'A'
ENDIF

IF lnRecNo <= RECCOUNT()
  GO lnRecNo
ENDIF

lc_ToPost = loFormSet.lc_ToPost
gcWorkdir = oAriaApplication.WorkDir

IF FOUND()
  ***  Post approved batches.
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *IF gfModalGen("TRM02070B02009","DIALOG",IIF(loFormSet.llDumyPost,'included','approved')) = 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen("TRM02070B02009","DIALOG",IIF(loFormSet.llDumyPost,LANG_GLBPOST_D_INCLUDED,LANG_GLBPOST_D_ARROVED)) = 1
IF gfModalGen("TRM02070B02009","DIALOG",IIF(loFormSet.llDumyPost,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_D_INCLUDED,loFormSet.GetHeaderText("LANG_GLBPOST_D_INCLUDED",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_D_ARROVED,loFormSet.GetHeaderText("LANG_GLBPOST_D_ARROVED",loFormSet.HeaderAlias)))) = 1
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[eND]
    *** calling posting procedure

    IF loFormSet.llDumyPost
      SELECT cBatchNo,cBatStat FROM GLBATCH ;
      INTO DBF &gcWorkDir.&lc_ToPost        ;
      WHERE  EVALUATE(loFormSet.lcScope) .AND. glBatch.lBatInd ;
      ORDER BY cBatchNo
    ELSE
      SELECT cBatchNo FROM GLBATCH    ;
      INTO DBF &gcWorkDir.&lc_ToPost  ;
      WHERE  EVALUATE(loFormSet.lcScope) .AND. glBatch.cBatStat = 'A';
      ORDER BY cBatchNo
    ENDIF

    SELECT (lc_ToPost)
    INDEX ON CBATCHNO TAG CBATCHNO
    SET ORDER TO CBATCHNO
      IF lfTBPost(loFormSet,"Batch",lc_ToPost,"Nonbeginning",lcReportFi) > 0
        * WAIT " Posting OK"  WINDOW NOWAIT
        IF loFormSet.llDumyPost
          SELECT (lc_ToPost)
          SET RELATION TO CBATCHNO INTO GLBATCH

          SCAN FOR &lc_ToPost..cBatchNo = 'P'
            REPLACE glBatch.lBatInd WITH .F.
          ENDSCAN

          glQuitting  = .T.
          CLEAR READ
        ENDIF

      ENDIF
      SELECT GLBATCH

      LOCATE
      loformset.ariaform1.grdGLBATCH.Refresh()

  ELSE
    RETURN
  ENDIF
ELSE
  *** if there is no record found with 'A' status display
  *** 'No approved batches to post.'
  *** and set the current object to approve
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *=gfModalGen("TRM02069B00000","DIALOG",IIF(loFormSet.llDumyPost,'included','approved'))
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM02069B00000","DIALOG",IIF(loFormSet.llDumyPost,LANG_GLBPOST_D_INCLUDED,LANG_GLBPOST_D_ARROVED))
=gfModalGen("TRM02069B00000","DIALOG",IIF(loFormSet.llDumyPost,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_D_INCLUDED,loFormSet.GetHeaderText("LANG_GLBPOST_D_INCLUDED",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_D_ARROVED,loFormSet.GetHeaderText("LANG_GLBPOST_D_ARROVED",loFormSet.HeaderAlias))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
ENDIF
=lfAfterRowColChange(loFormSet)
SELECT(lnSlct)
RETURN

***********************************************************
*! Name      : lfvPrint
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   :
************************************************************
FUNCTION  lfvPrint
PARAMETERS loformset
*** printing function


***********************************************************
*! Name      : lfvSM
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Source Module selector
************************************************************
FUNCTION  lfvSM
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT GLBatch

*** calling the global function mover to select the source modules
PRIVATE laSApp_Id,laTApp_Id
DIMENSION laSApp_Id[ALEN(loFormSet.laSApp_Id)]
ACOPY(loFormSet.laSApp_Id,laSApp_Id)
DIMENSION laTApp_Id[ALEN(loFormSet.laTApp_Id)]
ACOPY(loFormSet.laTApp_Id,laTApp_Id)

*N000682,1 12/13/2012 TMI Globlization changes[Start] 
*=gfMover(@laSApp_Id,@laTApp_Id,"Source modules",.T.)
lcSourceModules = ;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_SOURCE_MODULE,loFormSet.GetHeaderText("LANG_GLBPOST_SOURCE_MODULE",loFormSet.HeaderAlias))
=gfMover(@laSApp_Id,@laTApp_Id,lcSourceModules,.T.)
*N000682,1 12/13/2012 TMI Globlization changes[End  ] 

DIMENSION loFormSet.laSApp_Id[ALEN(laSApp_Id)]
ACOPY(laSApp_Id,loFormSet.laSApp_Id)
DIMENSION loFormSet.laTApp_Id[ALEN(laTApp_Id)]
ACOPY(laTApp_Id,loFormSet.laTApp_Id)
*** init. the string variable lcSrcmodul

loFormSet.lcSrcmodul =''

*** collect all source modules names in the string lcSrcmodul
FOR lnCounter = 1 TO ALEN(loFormSet.laTApp_ID,1)
   loFormSet.lcSrcmodul = loFormSet.lcSrcmodul + LEFT(loFormSet.laTApp_ID[lnCounter],3)
ENDFOR

*-Set scope
=lfSetScope(loFormSet)
=lfAppFilt(loFormSet)
=lfAfterRowColChange(loFormSet)
*** check if there is no any source modules are selected
*** in this case disable all objects except (Close),(S.M.) objects.
IF ! EMPTY(loFormSet.lcSrcmodul)
  *** check if there is any transaction(s) found for this module(s)
  *** if there is no any transaction
  *** display "No transactions for this source module(s).
  *** and disable all objects except (Close),(S.M.) objects.


  LOCATE FOR EVALUATE(loFormSet.lcScope)
  IF  FOUND()

    =lfNoBatches(loFormSet,.T.)

  ELSE

    =gfModalGen("TRM02066B00000","Dialog")
    =lfNoBatches(loFormSet,.F.)

  ENDIF

ELSE

  =lfNoBatches(loFormSet,.F.)

ENDIF

*** moves the current pointer (CP) to the first record that
*** matches the specified conditions in (lcScope)
LOCATE FOR EVALUATE(loFormSet.lcScope)

SELECT(lnSlct)
RETURN

************************************************************
*! Name      : lfNoBatches
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : No Batches selected
************************************************************
FUNCTION lfNoBatches
PARAMETERS loFormSet,llEnStat
WITH loFormSet.Ariaform1
  .pbApprov.Enabled = llEnStat
  .pbAll.Enabled    = llEnStat
  .pbNone.Enabled   = llEnStat
  .pbInvert.Enabled = llEnStat
  .cmdPost.Enabled  = llEnStat
ENDWITH
*- End of lfNoBatches.
************************************************************
*! Name      : lfObj_Lock
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : locking records
************************************************************
FUNCTION  lfObj_Lock

IF (!llok_Stat) AND (cLok_User <> oAriaApplication.User_Id)
  RETURN .T.
ELSE
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *=gfModalGen("TRM02213B00000","ALART",IIF(loFormSet.llDumyPost,'include','approve')+'|'+'batch')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM02213B00000","ALART",IIF(loFormSet.llDumyPost,LANG_GLBPOST_S_INCLUDE ,LANG_GLBPOST_S_APPROVE )+'|'+LANG_GLBPOST_BATCH)
=gfModalGen("TRM02213B00000","ALART",IIF(loFormSet.llDumyPost,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_S_INCLUDE,loFormSet.GetHeaderText("LANG_GLBPOST_S_INCLUDE",loFormSet.HeaderAlias)) ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_S_APPROVE,loFormSet.GetHeaderText("LANG_GLBPOST_S_APPROVE",loFormSet.HeaderAlias)) )+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLBPOST_BATCH,loFormSet.GetHeaderText("LANG_GLBPOST_BATCH",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
  RETURN .F.
ENDIF

