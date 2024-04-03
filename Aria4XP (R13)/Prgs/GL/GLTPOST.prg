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
*E303184,4 TMI 09/06/2012 allow include fields to work correctly in the case called from the INCOME STATEMENT report
*B610461,1 TMI 08/15/2013 close the loformset.lcReportFi temp file [T20130627.0025 ] 
*B610497,1 Correct the fix B610461 TMI 09/04/2013 [T20130822.0015] 
*B610714,1 POst Single transaction screen give error when closed       [ T20140407.0001 ]
*:************************************************************************
PARAMETERS lcReportFi

*B610269,1 media fixing problems TMI 03/17/2013 [Start] add the include file
#INCLUDE r:\aria4xp\prgs\gl\gltpost.h
*B610269,1 media fixing problems TMI 03/17/2013 [End  ] 

*- Call the screen
lcRunScx = lfGetScx("GL\GLTPOST.scx")
IF !EMPTY(lcReportFi)
  PRIVATE oScr
  DO FORM (lcRunScx) WITH lcReportFi NAME oScr NOSHOW
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

loFormSet.AddProperty('lcProgName','GLTPOST')

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

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.pbApprov.Caption = 'Inclu\<de'                                                                                                                                                                       
  *.pbAll.Caption    = 'Include a\<ll'                                                                                                                                                                   
  *.pbNone.Caption   = 'Include \<none'                                                                                                                                                                    
  .pbApprov.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_INCLUDE,loFormSet.GetHeaderText("LANG_GLTPOST_INCLUDE",loFormSet.HeaderAlias))
  .pbAll.Caption    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_INCLUDE_ALL,loFormSet.GetHeaderText("LANG_GLTPOST_INCLUDE_ALL",loFormSet.HeaderAlias))
  .pbNone.Caption   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_INCLUDE_NONE,loFormSet.GetHeaderText("LANG_GLTPOST_INCLUDE_NONE",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


ELSE

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*  .pbApprov.Caption = '\<Approve'                                                                                                                                                                       
*  .pbAll.Caption    = 'Approve a\<ll'                                                                                                                                                                   
*  .pbNone.Caption   = 'Approve \<none'                                                                                                                                                                  
  .pbApprov.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_APPROVE,loFormSet.GetHeaderText("LANG_GLTPOST_APPROVE",loFormSet.HeaderAlias))
  .pbAll.Caption    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_APPROVE_ALL,loFormSet.GetHeaderText("LANG_GLTPOST_APPROVE_ALL",loFormSet.HeaderAlias))
  .pbNone.Caption   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_APPROVE_NONE,loFormSet.GetHeaderText("LANG_GLTPOST_APPROVE_NONE",loFormSet.HeaderAlias))
*N000682,1 MMT 11/20/2012 Globalization Changes[End]




ENDIF
ENDWITH

SELECT GLTRNSHD

*** searches the DBF (glTrnsHd) for the first record
*** that matches a given expression.
*** --> status  (glTrnsHd.cTrnStat ) , standard (glTrnsHd.cStandard )

LOCATE FOR EVALUATE(loFormSet.lcScope)
IF FOUND()

  =lfAppFilt(loFormSet)


  SELECT GLTRNSHD

  =lfSetGridDataSource(loFormSet)
  =lfAfterRowColChange(loFormSet)
  loFormSet.Ariaform1.grdGLTRNSHD.Setfocus()
ELSE
  *** if there is no batches to post.
  =gfModalGen("TRM02081B00000","Dialog")
  glQuitting  = .T.

  *RELEASE WINDOW (lc_TBrowt)
  *E303456,1 TMI 04/17/2014 19:37 [Start] comment this line
  *=lfFormDestroy(loFormSet)
  *E303456,1 TMI 04/17/2014 19:37 [End  ] 

  SELECT GLTRNSHD
  RETURN .F.
ENDIF


************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/07/2012
*! Purpose   : Define variables used in the screen
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

*** Arrays used by mover :
loFormSet.AddProperty('laSSorJou[1,1]')    && source array of subjournals names
loFormSet.AddProperty('laTSorJou[1,1]')    && target array of subjournals names

*** Arrays used to display status for each transation :
loFormSet.AddProperty('laTranStat[3]')
loFormSet.AddProperty('laTranType[2]')

loFormSet.laTranStat [1] = 'Unposted'        && case of status = 'U'
loFormSet.laTranStat [2] = 'Out of balance'  && case of status = 'O'
loFormSet.laTranStat [3] = 'Approved'        && case of status = 'A'
	
loFormSet.laTranType [1] = 'Normal'          && case of type = 'N'
loFormSet.laTranType [2] = 'Automatic'       && case of type = 'A'

loFormSet.AddProperty('lcTrNumber', '' )  && var. to store the active transaction in browse
loFormSet.AddProperty('lcSrcjrnl' , ' ' )  && string to store the active subjournal(s) names
loFormSet.AddProperty('lcScope' , ' ' )  && string to store the active subjournal(s) names
lfSetScope(loFormSet)

loFormSet.AddProperty('lc_ToPost' , gfTempName() )
loFormSet.AddProperty('lnRecNo'   , 0  )

*** the string that contaning the scope of the valid posting years
lcFsWindow = STR(VAL(loFormSet.lcCurr_yer)-1,4)+" "+loFormSet.lcCurr_yer+" "+ STR(VAL(loFormSet.lcCurr_yer)+1,4)
loFormSet.AddProperty('lcFsWindow' , lcFsWindow)

*** Create array of 1 column ,each element is the concatenation
*** of two fields (GLSUBJOR.cSrcjrnl,GLSUBJOR.cJorShDes)

gcDataDir = oAriaApplication.DataDir
SELECT cSrcjrnl+" "+cJorShDes From &gcDataDir.glSubJor ;
       INTO ARRAY loFormSet.laSSorJou

*** Check if there is no any subjournals in the GLSUBJOR
*** in this case display error massege "No available source journals."
*** then exit form this program

IF ALEN(loFormSet.laSSorJou,1) = 1 .AND. TYPE('loFormSet.laSSorJou[1]') = 'L'
  loFormSet.laSSorJou = " "
  loFormSet.laSSorJou[1,1] = loFormSet.lcSJ_Def
ELSE
  IF ASCAN(loFormSet.laSSorJou,loFormSet.lcSJ_Def) = 0
    DIMENSION loFormSet.laSSorJou[ALEN(loFormSet.laSSorJou,1)+1,1]
    =AINS(loFormSet.laSSorJou,1)
    loFormSet.laSSorJou[1,1] = loFormSet.lcSJ_Def
  ENDIF
ENDIF

*** Create array with the same number of elements in source array.
*** then copy content from source array to target array.

DIMENSION loFormSet.laTSorJou[ALEN(loFormSet.laSSorJou)]
=ACOPY(loFormSet.laSSorJou,loFormSet.laTSorJou)
*** collect all source journals names in the string lcSrcjrnl
FOR lnCounter = 1 TO ALEN(loFormSet.laSSorJou)
  loFormSet.lcSrcjrnl = loFormSet.lcSrcjrnl + LEFT(loFormSet.laSSorJou[lnCounter],3)
ENDFOR
lfSetScope(loFormSet)

*N000682,1 12/17/12 TMI Globlization changes[Start] 
*loFormSet.AddProperty('lc_TBrowt',IIF(loFormSet.llDumyPost,"Dummy Transaction Posting","Transaction Posting"))
loFormSet.AddProperty('lc_TBrowt',IIF(loFormSet.llDumyPost,;
                      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_CAPTION_DUMMY,loFormSet.GetHeaderText("LANG_GLTPOST_CAPTION_DUMMY",loFormSet.HeaderAlias)),;
                      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_CAPTION,loFormSet.GetHeaderText("LANG_GLTPOST_CAPTION",loFormSet.HeaderAlias))))
*N000682,1 12/17/12 TMI Globlization changes[End  ] 
loFormset.Ariaform1.Caption = loFormSet.lc_TBrowt
*- End of lfDefineVars.

************************************************************
*! Name      : lfSetScope
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/07/2012
*! Purpose   : Set scope function
************************************************************
FUNCTION lfSetScope
PARAMETERS loFormSet

loFormSet.lcScope  =  " VAL(cBatchNo) = 0 " + ;
                      ".AND.   cSrcjrnl $ '"+loFormSet.lcSrcjrnl +"' " + ;
                      ".AND. ((cTrnStat $ 'UA' ) "  + ;
                      ".OR.   (cStandard = 'N' .AND. cTrnStat $ 'UAO' ))"
=lfAppFilt(loFormSet)
*- End of lfSetScope.

************************************************************
*! Name      : lfAppFilt
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/07/2012
*! Purpose   : Apply filter
************************************************************
FUNCTION lfAppFilt
PARAMETERS loFormSet
LOCAL lnSlct,lcFilt
lnSlct = SELECT(0)

SELECT GLTRNSHD
lcFilt = loFormSet.lcScope
SET FILTER TO &lcFilt
LOCATE
loFormset.Ariaform1.grdGLTRNSHD.Refresh()

SELECT (lnSlct )
*- End of lfAppFilt.

************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 22/05/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loFormSet

loFormSet.Ariaform1.grdGLTRNSHD.RecordSource = "GLTRNSHD"
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	lfSetColumnsProp('1','cTranno'   ,'Trans. #',80)
*!*	lfSetColumnsProp('2','cSrcjrnl','S/J'   ,40)
*!*	lfSetColumnsProp('3','cComp_Id','CO'   ,40)
lcTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_TRANS_NO,loFormSet.GetHeaderText("LANG_GLTPOST_TRANS_NO",loFormSet.HeaderAlias))
lfSetColumnsProp('1','cTranno'   ,lcTtl ,80) 
lcTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_SJ,loFormSet.GetHeaderText("LANG_GLTPOST_SJ",loFormSet.HeaderAlias))
lfSetColumnsProp('2','cSrcjrnl',lcTtl ,40)
lcTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_CO,loFormSet.GetHeaderText("LANG_GLTPOST_CO",loFormSet.HeaderAlias))
lfSetColumnsProp('3','cComp_Id',lcTtl  ,40)
*N000682,1 MMT 11/22/2012 Globalization changes[END]
IF loFormSet.llDumyPost
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *lfSetColumnsProp('4','nTrnIndic = 1'  ,'Included',70)  
  lcTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_INCLUDEDED,loFormSet.GetHeaderText("LANG_GLTPOST_INCLUDEDED",loFormSet.HeaderAlias))
  lfSetColumnsProp('4','nTrnIndic = 1'  ,lcTtl ,70)
  *N000682,1 MMT 11/22/2012 Globalization changes[END]
ELSE
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *lfSetColumnsProp('4','nTrnIndic = 1'  ,'Included',70)  
  lcTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_APPROVED,loFormSet.GetHeaderText("LANG_GLTPOST_APPROVED",loFormSet.HeaderAlias))
  lfSetColumnsProp('4','cTrnStat= "A"',lcTtl,70)
  *N000682,1 MMT 11/22/2012 Globalization changes[END]
ENDIF
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	lfSetColumnsProp('5','dTrnPDate','Date'        ,80 )
*!*	lfSetColumnsProp('6','cTrnDesc' ,'Description' ,300 )
lcTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_DATE,loFormSet.GetHeaderText("LANG_GLTPOST_DATE",loFormSet.HeaderAlias))
lfSetColumnsProp('5','dTrnPDate',lcTtl ,80 )
lcTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_Description,loFormSet.GetHeaderText("LANG_GLTPOST_Description",loFormSet.HeaderAlias))
lfSetColumnsProp('6','cTrnDesc' ,lcTtl ,300 )
*N000682,1 MMT 11/22/2012 Globalization changes[END]
loFormSet.Ariaform1.grdGLTRNSHD.ReadOnly = .T.
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
WITH loFormSet.Ariaform1.grdGLTRNSHD
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
SELECT GLTRNSHD
WITH loFormSet.Ariaform1

  IF !EOF('GLTRNSHD')

    .Ariatextbox1.Value  = gltrnshd.ctranno
    .Ariatextbox2.Value  = gltrnshd.DTRNPDATE
    .Ariatextbox3.Value  = gltrnshd.ctrnpprd
    .Ariatextbox9.Value  = gltrnshd.CTRNPYR
    .Ariatextbox4.Value  = gltrnshd.cSrcjrnl + ' ' + lookup(glsubjor.cjorshdes,gltrnshd.cSrcjrnl,glsubjor.cSrcjrnl)
    .Ariatextbox5.Value  = IIF(gltrnshd.cTrnStat $ 'UOA',loFormSet.laTranStat[AT(gltrnshd.cTrnStat,'UOA')],'')
    .Ariatextbox6.Value  = gltrnshd.DTRNREVDT
    .Ariatextbox7.Value  = gltrnshd.CTRNREFER
    .Ariatextbox8.Value  = gltrnshd.CTRNDESC
    .Ariatextbox10.Value = IIF(gltrnshd.CTRNTYPE $ 'NA',loFormSet.laTranType[AT(gltrnshd.CTRNTYPE,'NA')],'')
    .Ariatextbox11.Value = gltrnshd.NTRNTOTDR
    .Ariatextbox12.Value = gltrnshd.NTRNTOTCR
    .Ariatextbox13.Value = ABS(gltrnshd.NTRNTOTDR-gltrnshd.NTRNTOTCR)
    .Ariatextbox14.Value = IIF(NTRNTOTDR>NTRNTOTCR,'Dr',IIF(NTRNTOTDR<NTRNTOTCR,'Cr',' '))

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

  IF loFormSet.llDumyPost
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    lcTtl1 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_SINGLE_INCLUDE,loFormSet.GetHeaderText("LANG_GLTPOST_SINGLE_INCLUDE",loFormSet.HeaderAlias))
    lcTtl2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_SINGLE_EXCLUDE,loFormSet.GetHeaderText("LANG_GLTPOST_SINGLE_EXCLUDE",loFormSet.HeaderAlias))
    .pbApprov.Caption = IIF(glTrnsHd.nTrnIndic = 0,lcTtl1,lcTtl2)
    *N000682,1 MMT 11/22/2012 Globalization changes[END]
  ELSE
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    lcTtl1 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_SINGLE_DISAPPROVE,loFormSet.GetHeaderText("LANG_GLTPOST_SINGLE_DISAPPROVE",loFormSet.HeaderAlias))
    lcTtl2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_APPROVE,loFormSet.GetHeaderText("LANG_GLTPOST_APPROVE",loFormSet.HeaderAlias))
    .pbApprov.Caption = IIF(glTrnsHd.cTrnStat = 'A',lcTtl1,lcTtl2)
    *N000682,1 MMT 11/22/2012 Globalization changes[END]
  ENDIF

ENDWITH
loFormset.Ariaform1.grdGLTRNSHD.Refresh()
*- End of lfAfterRowColChange.


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
  *B610461,1 TMI 08/15/2013 [Start] close the loformset.lcReportFi temp file
  IF USED(loformset.lcReportFi)
    USE IN (loformset.lcReportFi)
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

*!**************************************************************************
*!					
*!      Function:  lfKeyPressed
*!
*!**************************************************************************
*
FUNCTION lfKeyPressed
PARAMETERS loFormSet,nKeyCode

IF nKeyCode = 13

  =lfvAprOne(loFormSet)

ENDIF


*!**************************************************************************
*!					
*!      Function:  lfvAprOne
*!
*!**************************************************************************
FUNCTION lfvAprOne
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

SELECT glTrnsHd
*** check if the status for (cTrnStat) = 'A' --> approve
*** that is mean the user want to disapprove this record.
*** then we will check the locking status, if there is no
*** user use this record, then change the status according to
*** the diffrent between glTrnsHd.nTrnTotdr,glTrnsHd.nTrnTotcr
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance

IF loFormSet.llDumyPost
  IF nTrnIndic = 1
    IF lfObj_Lock(loFormSet) .AND. gfObj_Lock(.T.)
      REPLACE glTrnsHd.nTrnIndic WITH 0
      *E303184,4 TMI 09/06/2012 [Start] update the table with the change in nTrnIndic field
      gfTableUpdate()
      *E303184,4 TMI 09/06/2012 [End  ]
      =gfObj_Lock(.F.)

    ENDIF
  ELSE
    IF glTrnsHd.cTrnPYr $ loFormSet.lcFsWindow
      IF lfObj_Lock(loFormSet) .AND. gfObj_Lock(.T.)
        REPLACE glTrnsHd.nTrnIndic WITH 1
        *E303184,4 TMI 09/06/2012 [Start] update the table with the change in nTrnIndic field
        gfTableUpdate()
        *E303184,4 TMI 09/06/2012 [End  ]
        =gfObj_Lock(.F.)

      ENDIF
    ELSE
      =gfModalGen("TRM02083B00000","Dialog",IIF(loFormSet.llDumyPost,'include','approve'))
      *** The Batch posting year is out of the posting window.
    ENDIF
  ENDIF

ELSE

  IF cTrnStat = 'A'
    IF lfObj_Lock(loFormSet) .AND. gfObj_Lock(.T.)
      REPLACE glTrnsHd.cTrnStat WITH ;
              IIF (glTrnsHd.nTrnTotdr=glTrnsHd.nTrnTotcr,'U','O')
      =gfAdd_Info('glTrnsHd')
      =gfTableUpdate()
      =gfObj_Lock(.F.)

    ENDIF
  ELSE
    *** that is mean the user want to approve this record.
    *** in this case must be check if the glTrnsHd.cTrnPYr
    *** in the scope the posting window (previous,current,next)
    *** if !valid display "The Batch posting year is out of the posting window"
    *** if valid .. check the locking status, if there is no
    *** user use this record, then change the status to 'A' --> approve

    IF glTrnsHd.cTrnPYr $ loFormSet.lcFsWindow
      IF lfObj_Lock(loFormSet) .AND. gfObj_Lock(.T.)
        REPLACE glTrnsHd.cTrnStat WITH  'A'
        =gfAdd_Info('glTrnsHd')
        =gfTableUpdate()
        =gfObj_Lock(.F.)

      ENDIF
    ELSE
      =gfModalGen("TRM02083B00000","Dialog",IIF(loFormSet.llDumyPost,'include','approve'))
      *** The Batch posting year is out of the posting window.
    ENDIF
  ENDIF
ENDIF


*- Refresh screen
=lfAfterRowColChange(loFormset)

SELECT (lnSlct)

*!**************************************************************************
*!
*!      Function:  lfvAprAll
*!
*!**************************************************************************
*
FUNCTION lfvAprAll
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
*** moves the record pointer through the current DBF (GLTRNSHD)
*** for each record that meets the specified conditions in (lcScope)
*** and check if the glTrnsHd.cTrnPYr in the scope the posting
*** window (previous,current,next), if valid check the locking
*** status, if there is no user use this record,
*** then change the status to 'A' --> approve
loFormSet.lnRecNo = RecNo()

SCAN  FOR EVALUATE(loFormSet.lcScope)
  IF glTrnsHd.cTrnPYr $ loFormSet.lcFsWindow
    IF !glTrnsHd.lLok_Stat
      IF loFormSet.llDumyPost
        REPLACE glTrnsHd.nTrnIndic WITH 1
      ELSE
        REPLACE glTrnsHd.cTrnStat      WITH  'A'
        =gfAdd_Info('glTrnsHd')
      ENDIF
    ENDIF
  ENDIF
ENDSCAN
IF !loFormSet.llDumyPost
  =gfTableUpdate()
ENDIF

*** refresh the browse window
GOTO (loFormSet.lnRecNo)

*- Refresh screen
=lfAfterRowColChange(loFormset)
SELECT (lnSlct)

*!**************************************************************************
*!
*!      Function:  lfvAprNon
*!
*!**************************************************************************
*
FUNCTION lfvAprNon
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

*** moves the record pointer through the current DBF (GLTRNSHD)
*** for each record that meets the specified conditions in (lcScope)
*** and change the status for every record have a status 'A' to
*** 'U' or 'O' status according to the diffrent between
*** glTrnsHd.nTrnTotdr,glTrnsHd.nTrnTotcr
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
loFormSet.lnRecNo = RecNo()

SCAN FOR EVALUATE(loFormSet.lcScope)
  IF !glTrnsHd.lLok_Stat
    IF loFormSet.llDumyPost
      REPLACE glTrnsHd.nTrnIndic WITH 0
    ELSE
      REPLACE glTrnsHd.cTrnStat WITH ;
         IIF (glTrnsHd.nTrnTotdr=glTrnsHd.nTrnTotcr,'U','O')
      =gfAdd_Info('glTrnsHd')
    ENDIF
  ENDIF
ENDSCAN
IF !loFormSet.llDumyPost
  =gfTableUpdate()
ENDIF

*** refresh the browse window
GOTO (loFormSet.lnRecNo)


*- Refresh screen
=lfAfterRowColChange(loFormset)
SELECT (lnSlct)

*!**************************************************************************
*!
*!      Function:  lfvInvert
*!
*!**************************************************************************
*
FUNCTION lfvInvert
PARAMETERS loFormSet

LOCAL lnSlct
lnSlct = SELECT(0)

*** moves the record pointer through the current DBF (GLTRNSHD)
*** for each record that meets the specified conditions in (lcScope)
*** and change the status for every record have a status 'A' to
*** 'U' or 'O' status according to the diffrent between
*** glTrnsHd.nTrnTotdr,glTrnsHd.nTrnTotcr
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
loFormSet.lnRecNo = RecNo()

SCAN FOR EVALUATE(loFormSet.lcScope)
  IF !glTrnsHd.lLok_Stat
    IF loFormSet.llDumyPost

      IF glTrnsHd.nTrnIndic = 1
        REPLACE glTrnsHd.nTrnIndic WITH 0
      ELSE
        IF glTrnsHd.cTrnPYr $ loFormSet.lcFsWindow
          REPLACE glTrnsHd.nTrnIndic WITH 1
        ENDIF
      ENDIF

    ELSE
      IF glTrnsHd.cTrnStat = 'A'
        REPLACE glTrnsHd.cTrnStat WITH ;
                IIF (glTrnsHd.nTrnTotdr=glTrnsHd.nTrnTotcr,'U','O')
        =gfAdd_Info('glTrnsHd')
      ELSE
        *** that is mean the user want to approve this record.
        *** in this case must be check if the glTrnsHd.cTrnPYr
        *** in the scope the posting window (previous,current,next)
        *** if valid check the locking status, if there is no user
        *** use this record, then change the status to 'A' --> approve
        IF glTrnsHd.cTrnPYr $ loFormSet.lcFsWindow
          IF !glTrnsHd.lLok_Stat
            REPLACE glTrnsHd.cTrnStat WITH  'A'
            =gfAdd_Info('glTrnsHd')
          ENDIF
        ENDIF
      ENDIF

    ENDIF
  ENDIF
ENDSCAN
IF !loFormSet.llDumyPost
  =gfTableUpdate()
ENDIF
*** refresh the browse window

GOTO (loFormSet.lnRecNo)


*- Refresh screen
=lfAfterRowColChange(loFormset)
SELECT (lnSlct)

*!**************************************************************************
*!
*!      Function:  lfvPost
*!
*!**************************************************************************
*
FUNCTION lfvPost
PARAMETERS loFormSet

*** moves the record pointer through the current DBF (GLTRNSHD)
*** for each record that meets the specified conditions in (lcScope)
*** and if found any record with status 'A' then exit from scan loop
loFormSet.lnRecNo = RecNo()

IF loFormSet.llDumyPost
  LOCATE FOR  EVALUATE(loFormSet.lcScope) .AND. glTrnsHd.nTrnIndic = 1
ELSE
  LOCATE FOR  EVALUATE(loFormSet.lcScope) .AND. glTrnsHd.cTrnStat = 'A'
ENDIF

IF loFormSet.lnRecNo <= RECCOUNT()
  GO (loFormSet.lnRecNo)
ENDIF

gcWorkdir = oAriaApplication.WorkDir
lc_ToPost = loFormSet.lc_ToPost
IF FOUND()
  ***  Post approved transations   <Post      Cancel>.
  IF gfModalGen("TRM02085B02009","DIALOG",IIF(loFormSet.llDumyPost,'included','approved')) = 1

    *** calling posting procedure
    IF loFormSet.llDumyPost
      SELECT cTranNo,cTrnStat FROM glTrnsHd    ;
         INTO DBF &gcWorkDir.&lc_ToPost ;
         WHERE EVALUATE(loFormSet.lcScope) .AND. glTrnsHd.nTrnIndic = 1 ;
         ORDER BY cBatchNo,cTranNo
    ELSE
      SELECT cTranNo FROM glTrnsHd    ;
         INTO DBF &gcWorkDir.&lc_ToPost ;
         WHERE EVALUATE(loFormSet.lcScope) .AND. glTrnsHd.cTrnStat = 'A';
         ORDER BY cBatchNo,cTranNo
    ENDIF

    SELECT (lc_ToPost)
    INDEX ON CTRANNO TAG CTRANNO
    SET ORDER TO CTRANNO

    IF lfTBPost(loFormSet,"Transactions",lc_ToPost,'',lcReportFi) > 0
      *WAIT " Everything is OK     " WINDOW NOWAIT

      IF loFormSet.llDumyPost
        SELECT (lc_ToPost)
        SET RELATION TO '000000' + cTranNo INTO GLTRNSHD

        SCAN FOR &lc_ToPost..cTrnStat = 'P'
          REPLACE glTrnsHd.nTrnIndic WITH 0
        ENDSCAN

        loFormSet.Release
      ENDIF
    ELSE
      *WAIT " Unsuccessful posting " WINDOW NOWAIT
    ENDIF

    SELECT GLTRNSHD
    LOCATE
    =lfAfterRowColChange(loFormset)

    RETURN
  ELSE
    RETURN
  ENDIF
ELSE
  *** if there is no record found with 'A' status display
  *** 'No approved transations to post.'
  *** and set the current object to approve
  =gfModalGen("TRM02082B00000","DIALOG",IIF(loFormSet.llDumyPost,'included','approved'))

  loFormSet.Ariaform1.pbApprov.SetFocus()
ENDIF

*** refresh the browse window

GOTO (loFormSet.lnRecNo)


*- Refresh screen
=lfAfterRowColChange(loFormset)

*!**************************************************************************
*!
*!      Function:  lfvPrint
*!
*!**************************************************************************
*
FUNCTION  lfvPrint
*** print function


***********************************************************
*! Name      : lfvSJ
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Source Journal selector
************************************************************
FUNCTION  lfvSJ
PARAMETERS loFormSet

*** Create array of 1 column ,each element is the concatenation
*** of two fields (GLSUBJOR.cSrcjrnl,GLSUBJOR.cJorShDes)
gcDataDir = oAriaApplication.DataDir
SELECT cSrcjrnl+" "+cJorShDes From &gcDataDir.glSubJor ;
       INTO ARRAY loFormSet.laSSorJou

*** Check if there is no any subjournals in the GLSUBJOR
*** in this case display error massege "No available source journals."
*** then exit form this program

IF ALEN(loFormSet.laSSorJou,1) = 1 .AND. TYPE('loFormSet.laSSorJou[1]') = 'L'
  loFormSet.laSSorJou = " "
  loFormSet.laSSorJou[1,1] = loFormSet.lcSJ_Def
ELSE
  IF ASCAN(loFormSet.laSSorJou,loFormSet.lcSJ_Def) = 0
    DIMENSION loFormSet.laSSorJou[ALEN(loFormSet.laSSorJou,1)+1,1]
    =AINS(loFormSet.laSSorJou,1)
    loFormSet.laSSorJou[1,1] = loFormSet.lcSJ_Def
  ENDIF
ENDIF

*** calling the global function mover to select the sobjournals
DIMENSION laSSorJou[ALEN(loFormSet.laSSorJou)]
DIMENSION laTSorJou[ALEN(loFormSet.laTSorJou)]
ACOPY(loFormSet.laSSorJou,laSSorJou)
ACOPY(loFormSet.laTSorJou,laTSorJou)
*N000682,1 12/14/12 TMI Globlization changes[Start] 
*=gfMover(@laSSorJou,@laTSorJou,"Source journal",.T.)
lcSourceJournal = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLTPOST_SOURCEJOURNAL,loFormSet.GetHeaderText("LANG_GLTPOST_SOURCEJOURNAL",loFormSet.HeaderAlias))
=gfMover(@laSSorJou,@laTSorJou,lcSourceJournal,.T.)
*N000682,1 12/14/12 TMI Globlization changes[End  ] 
DIMENSION loFormSet.laTSorJou[ALEN(laTSorJou)]
ACOPY(laSSorJou,loFormSet.laSSorJou)
ACOPY(laTSorJou,loFormSet.laTSorJou)

*** init. the string variable lcSrcjrnl
loFormSet.lcSrcjrnl = ' '
*** collect all source journals names in the string lcSrcjrnl
FOR lnCounter = 1 TO ALEN(loFormSet.laTSorJou,1)
   loFormSet.lcSrcjrnl = loFormSet.lcSrcjrnl + LEFT(loFormSet.laTSorJou[lnCounter],3)
ENDFOR
lfSetScope(loFormSet)

*** check if there is no any subjournlas are selected
*** in this case disable all objects except (Close),(S.J.) objects.

IF ! EMPTY(loFormSet.lcSrcjrnl)
  *** check if there is any transaction(s) found for this subjournal(s)
  *** if there is no any transaction
  *** display "No transactions for this source journal(s).
  *** and disable all objects except (Close),(S.J.) objects.
  LOCATE FOR EVALUATE(loFormSet.lcScope)
  IF  FOUND()

    =lfNoBatches(loFormSet,.T.)
  ELSE

    =gfModalGen("TRM02084B00000","Dialog")
    =lfNoBatches(loFormSet,.F.)

  ENDIF

ELSE

  =lfNoBatches(loFormSet,.F.)

ENDIF

LOCATE FOR EVALUATE(loFormSet.lcScope)


*- Refresh screen
=lfAfterRowColChange(loFormset)


*!**************************************************************************
*!					
*!      Function:  lfObj_Lock
*!
*!**************************************************************************
*
FUNCTION lfObj_Lock
PARAMETERS loFormSet

IF (!llok_Stat) AND (cLok_User <> oAriaApplication.User_ID)
  RETURN .T.
ELSE
  =gfModalGen("TRM02213B00000","ALART",IIF(loFormSet.llDumyPost,'include','approve')+'|'+'transaction')
  RETURN .F.
ENDIF

