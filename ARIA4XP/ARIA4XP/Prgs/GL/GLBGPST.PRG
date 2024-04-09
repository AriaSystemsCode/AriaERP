*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\GL\GLBGPST.PRG
*:  Module      : General Ledger
*:  Desc.       : posting -> Beginning Balance Posting
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 10/21/2012
*:  Reference   : E303282 
*:************************************************************************
*B610461,1 TMI 08/15/2013 close the loformset.lcReportFi temp file [T20130627.0025 ] 
*B610497,1 Correct the fix B610461 TMI 09/04/2013 [T20130822.0015] 
*:************************************************************************
*- Call the screen
PARAMETERS lcReportFi
*- Call the screen
lcRunScx = lfGetScx("GL\GLBGPST.scx")

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

loFormSet.AddProperty('lcProgName','GLBGPST')

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

*** Check if GLSETUP.dSetBBDat between the posting window
IF Between(GLSETUP.dSetBBDat,loFormSet.ldPyBgDate,loFormSet.ldNyEnDate) 

*** Check if GLSETUP.lSetAlbBe is true,(allowed to post begining balances)
  IF !GLSETUP.lSetAlbBe 
    *** Message "Posting begining balances is not allowed.  
    *** then exit from this program
    =gfModalGen("TRM02098B00000","Dialog")      
    glQuitting  = .T.
    RETURN .F.
  ENDIF
ELSE

  *** Message "Begining balances date is out of the posting window"
  *** then exit from this program
  =gfModalGen("TRM02097B00000","Dialog")      
  glQuitting  = .T.
  RETURN .F.
ENDIF

WITH loFormSet.Ariaform1
IF loFormSet.llDumyPost
  .pbApprov.Caption = 'Inclu\<de'
  .pbAll.Caption    = 'Include a\<ll'
  .pbNone.Caption   = 'Include \<none'  
ELSE
  .pbApprov.Caption = '\<Approve'
  .pbAll.Caption    = 'Approve a\<ll'
  .pbNone.Caption   = 'Approve \<none'
ENDIF
ENDWITH 

SELECT GLBATCH
*** searches the DBF (glBatch) for the first record 
*** that matches a given expression (lcScope).
lcScope = loFormSet.lcScope
SET FILTER TO &lcScope
LOCATE 

IF FOUND()
  *lc_ToPost   = gfTempName()
  
  *** places all current on key labels on a stack in memory.
*!*	  PUSH KEY

*!*	  *** deactivate control keys for browse window 
*!*	  *** ESCAPE, TAB, Ctrl+W, Ctrl+Q, Ctrl+Enter
*!*	  ON KEY LABEL ESC        DO  lpEscape  
*!*	  ON KEY LABEL TAB        DO  lpTab 
*!*	  ON KEY LABEL RIGHTARROW DO  lpTab
*!*	  ON KEY LABEL LEFTARROW  DO  lpShiftTab
*!*	  ON KEY LABEL BACKTAB    DO  lpShiftTab 
*!*	  ON KEY LABEL Ctrl+W      lnDummi = 1
*!*	  ON KEY LABEL Ctrl+Q      lnDummi = 1
*!*	  ON KEY LABEL Ctrl+ENTER  lnDummi = 1

  =lfCalc(loFormSet)    && Totals all glBatch.nBatotCr, glBatch.nBatotDr 
               && only for batches with glBatch.cBatStat = 'A' 
  =lfSetGridDataSource(loFormSet)
  =lfAfterRowColChange(loFormSet)

*!*	  KEYBOARD CHR(49)      && g keyboard buffer.

*!*	  SET CONFIRM OFF

  SELECT GLBATCH  
  *E300683,5 Call *.SPR from screens directory
  * DO Glbgpst.SPR 
  *DO (gcScrDir + gcWinAppl + '\Glbgpst.SPR')
  *E300683,5 end   
*!*	  SET CONFIRM ON
  llRet = .T.
  
  *** restores on key labels that were placed on the stack with push key. 
ELSE  
  =gfModalGen("TRM02099B00000","Dialog")  && there is no batches to post.
  llRet = .F.
ENDIF

SELECT GLBATCH 
RETURN llRet 
*- End of lfFormInit.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/21/2012
*! Purpose   : Define variables to be used in the formset
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormset

loFormSet.AddProperty('la_TBtStat[3]','')  && to display status       for each transation 
loFormSet.la_TBtStat [1] = 'Unposted'          && Case status --> 'U'
loFormSet.la_TBtStat [2] = 'Out of balance'    && Case status --> 'O'
loFormSet.la_TBtStat [3] = 'Approved'          && Case status --> 'A'

loFormSet.AddProperty('laCtrStat', 'DISABLE' ) && To disable the browse pad in the menu

loFormSet.AddProperty('lc_ToPost', gfTempName())
loFormSet.AddProperty('lcEditLine'    , ' ' )   && init. edit list var.  
                                     && Say refresh function        
loFormSet.AddProperty('llprint'       , .T. )
loFormSet.AddProperty('lcCurrBat'     , ''  )   && set the current record in browse to 0

loFormSet.AddProperty('lnTotalCr'     , 0.00)   && total approved credit
loFormSet.AddProperty('lnTotalDr'     , 0.00)   && total approved debit

loFormSet.AddProperty('lnRecNo'       , 0)

loFormSet.AddProperty('lcScope'   , "cBatType = 'B' .AND. cBatStat $ 'UOA' ")

IF loFormSet.llDumyPost
  loFormSet.AddProperty('lc_TBrowt' , "Dummy Posting beginning balances"  )
ELSE  
  loFormSet.AddProperty('lc_TBrowt' , "Posting beginning balances"  )
ENDIF  

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
** put in destroy event
IF USED(lc_ToPost)
  USE IN ALIAS (lc_ToPost)
ENDIF
ERASE &gcWorkdir.&lc_ToPost..DBF
ERASE &gcWorkdir.&lc_ToPost..CDX
SELECT GLBATCH

************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 22/05/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loFormSet  

loFormSet.Ariaform1.grdGLBATCH.RecordSource = "GLBATCH"
lfSetColumnsProp('1','cBatchNo'   ,'Batch#',6*10)
lfSetColumnsProp('2','lfCheckEL()','E/L'   ,3*10)
IF loFormSet.llDumyPost
  lfSetColumnsProp('3','lBatInd'       ,'Included',60)
ELSE  
  lfSetColumnsProp('3','cBatStat = "A"','Approved',60)
ENDIF 
lfSetColumnsProp('4','cSrcmodul','S/M'        ,3*10 )
lfSetColumnsProp('5','DBatpBeg' ,'  Date'     ,80)
lfSetColumnsProp('6','CBatDesc','  Description',10*25)
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

*!*	*xxx
*!*	LOCATE FOR &lcScope
*!*	lcObjState = IIF(!FOUND(),"DISABLE","ENABLE")
*!*	*x

*!*	SHOW GET pbApprov  &lcObjState
*!*	SHOW GET pbAll     &lcObjState
*!*	SHOW GET pbNone    &lcObjState
*!*	SHOW GET pbInvert  &lcObjState
*!*	SHOW GET pbPrint   &lcObjState
*!*	SHOW GET pbPost    &lcObjState

*!*	*xxx

WITH loFormSet.Ariaform1

  IF !EOF('GLBATCH')
    .Ariatextbox1.Value  = glBatch.cbatchno
    .Ariatextbox2.Value  = IIF (glBatch.cbatstat $ 'UOA',loFormSet.la_TBtStat[AT(glBatch.cbatstat,'UOA')],'')                           
    .Ariatextbox3.Value  = glBatch.dbatpbeg
    .Ariatextbox4.Value  = glBatch.nbatotdr
    .Ariatextbox5.Value  = glBatch.nbatotcr
    .Ariatextbox6.Value  = ABS(glBatch.nbatotdr-glBatch.nbatotcr)
    .Ariatextbox7.Value  = IIF(glBatch.nbatotdr>glBatch.nbatotcr,'Dr',IIF(glBatch.nbatotdr<glBatch.nbatotcr,'Cr',' '))
    .Ariatextbox8.Value  = glBatch.nbatcntot
    .Ariatextbox9.Value  = glBatch.cbatrefer
    .Ariatextbox10.Value = glBatch.cbatdesc
    .Ariatextbox11.Value = loFormSet.lnTotalDr
    .Ariatextbox12.Value = loFormSet.lnTotalCr
    .Ariatextbox13.Value = ABS(loFormSet.lnTotalDr-loFormSet.lnTotalCr)
    .Ariatextbox14.Value = IIF(loFormSet.lnTotalDr>loFormSet.lnTotalCr,'Dr',IIF(loFormSet.lnTotalDr<loFormSet.lnTotalCr,'Cr',' '))

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
    .pbApprov.Caption = IIF(glBatch.lBatInd ,'\<Exclude','Inclu\<de')
  ELSE 
    
    .pbApprov.Caption = IIF(glBatch.cBatStat = 'A','\<Disapprove','\<Approve')
    
  ENDIF 
  
ENDWITH   

*- End of lfAfterRowColChange.
******************************>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
****************************** ORIGINAL CODE ************************************

*!*	*!**************************************************************************
*!*	*!
*!*	*!      Function:  lfActBrow   
*!*	*!
*!*	*!**************************************************************************
*!*	*
*!*	FUNCTION  lfActBrow

*!*	LOCATE FOR &lcScope
*!*	lcObjState = IIF(!FOUND(),"DISABLE","ENABLE")
*!*	*x
*!*	IF llDumyPost 

*!*	  IF lBatInd
*!*	    SHOW GET pbApprov,1 PROMPT 'Exclu\<de'
*!*	  ELSE
*!*	    SHOW GET pbApprov,1 PROMPT 'Inclu\<de'
*!*	  ENDIF

*!*	ELSE

*!*	  IF cBatStat = 'A'
*!*	    SHOW GET pbApprov,1 PROMPT '\<Disapprove'
*!*	  ELSE
*!*	    SHOW GET pbApprov,1 PROMPT '\<Approve'
*!*	  ENDIF

*!*	ENDIF

*!*	*x
*!*	SHOW GET pbApprov  &lcObjState
*!*	SHOW GET pbAll     &lcObjState
*!*	SHOW GET pbNone    &lcObjState
*!*	SHOW GET pbInvert  &lcObjState
*!*	SHOW GET pbPrint   &lcObjState
*!*	SHOW GET pbPost    &lcObjState

*!*	lcFieldDis = IIF(llDumyPost,;
*!*	                "Included=IIF(lBatInd       ,'   û','    ')", ;
*!*	                "Approved=IIF(cBatStat = 'A','   û','    ')") 

*!*	*x*
*!*	lcBrString="cDummi1=IIF(lcCurrBat=cBatchNo,'',' '):H=' ':F:V=lfKeyPressed(),"+;
*!*	           "cBatchNo:H='Batch#':6,cDummi2=lfCheckEL():H='E/L':3,"+;
*!*	           "&lcFieldDis"+;
*!*	           ",cSrcModul:H='S/M':3,"+;
*!*	           "DBatpBeg:H='  Date':10,"+;
*!*	           "CBatDesc:H='   Description':40"
 
*!*	    BROWSE FIELDS &lcBrString;
*!*	           LOCK 0   ;
*!*	           NOAPPEND ;
*!*	           NOCLEAR  ;
*!*	           NODELETE ;
*!*	           NOMENU   ;
*!*	           NOWAIT   ;
*!*	           SAVE     ;
*!*	           FONT "FOXFONT", 9 STYLE 'N';
*!*	           FREEZ cDummi1;
*!*	           FOR   &lcScope;
*!*	           TITLE lc_TBrowt ;
*!*	           WHEN lfwBrowhen();
*!*	           WINDOW glBGPst0 IN WINDOW (gcBaseWind)

*!*	*!**************************************************************************
*!*	*!
*!*	*!      Function:  lfwBrowhen
*!*	*!
*!*	*!**************************************************************************
*!*	*
*!*	FUNCTION  lfwBrowhen
*!*	*** check if the status for the ( cBatStat ) 
*!*	*** and change the prompt display
*!*	*** if A -->  display 'Approved ' Else display 'Disapprove'

*!*	IF llDumyPost 

*!*	  IF lBatInd
*!*	    SHOW GET pbApprov,1 PROMPT 'Exclu\<de'
*!*	  ELSE
*!*	    SHOW GET pbApprov,1 PROMPT 'Inclu\<de'
*!*	  ENDIF

*!*	ELSE

*!*	  IF cBatStat = 'A'
*!*	    SHOW GET pbApprov,1 PROMPT '\<Disapprove'
*!*	  ELSE
*!*	    SHOW GET pbApprov,1 PROMPT '\<Approve'
*!*	  ENDIF

*!*	ENDIF

*!*	SHOW GETS

*!**************************************************************************
*!
*!      Function:  lfCheckEL 
*!To put check mark in edit list field ***
*!**************************************************************************
*
FUNCTION  lfCheckEL
LOCAL llEditL
IF !EMPTY(glBatch.cBatElUsr) 
  llEditL = IIF ( glBatch.dbaTelDat > glBatch.daDd_Date,.T.,;
            IIF ( glBatch.dbaTelDat = glBatch.daDd_Date .AND.;
                  glBatch.cbAtelTim > glBatch.caDd_Time,.T.,.F.))
ELSE
  llEditL = .F. 
ENDIF    

RETURN llEditL


*!**************************************************************************
*!					
*!*	*!      Function:  lfKeyPressed
*!*	*!
*!*	*!**************************************************************************
*!*	*
*!*	FUNCTION  lfKeyPressed 

*!*	IF LASTKEY()=13 
*!*	  KEYBOARD CHR(31)
*!*	  =lfvAprOne()
*!*	ENDIF  

*!**************************************************************************
*!					
*!      Function:  lfvAprOne 
*!To approve or disapprove specific transaction ***
*!**************************************************************************
*
FUNCTION  lfvAprOne 
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT glbatch
*** check if the status for (cBatStat) = 'A' --> approve 
*** that is mean the user want to disapprove this record.
*** then we will check the locking status, if there is no
*** user use this record, then change the status according to 
*** the diffrent between glBatch.nBatotdr,glBatch.nBatotcr 
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
*** and subtract glBatch.nBatotCr From lnTotalCr,
***     subtract glBatch.nBatotDr From lnTotalDr
SELECT glbatch
IF loFormSet.llDumyPost
  IF lBatInd
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glBatch.lBatInd WITH .F.
      gfTableUpdate()
      loFormSet.lnTotalCr = loFormSet.lnTotalCr - glBatch.nBatotCr
      loFormSet.lnTotalDr = loFormSet.lnTotalDr - glBatch.nBatotDr
      =gfObj_Lock(.F.)
      SHOW GET pbApprov,1 PROMPT 'Inclu\<de'
    ENDIF
  ELSE 
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glBatch.lBatInd WITH .T.
      gfTableUpdate()
      loFormSet.lnTotalCr = loFormSet.lnTotalCr + glBatch.nBatotCr
      loFormSet.lnTotalDr = loFormSet.lnTotalDr + glBatch.nBatotDr
      =gfObj_Lock(.F.) 
      SHOW GET pbApprov,1 PROMPT 'Exclu\<de'
    ENDIF  
  ENDIF  

ELSE
  IF cBatStat = 'A' 
    IF lfObj_Lock() .AND. gfObj_Lock(.T.)
      REPLACE glBatch.cBatStat WITH ;
              IIF (glBatch.nbatotdr=glBatch.nbatotcr,'U','O')
      gfTableUpdate()
      loFormSet.lnTotalCr = loFormSet.lnTotalCr - glBatch.nBatotCr
      loFormSet.lnTotalDr = loFormSet.lnTotalDr - glBatch.nBatotDr
      =gfAdd_Info('glbatch')
      =gfObj_Lock(.F.)
      SHOW GET pbApprov,1 PROMPT '\<Approve' 
    ENDIF
  ELSE 
    *** that is mean the user want to approve this record.  
    *** if check list is approved then if valid check the locking status,
    *** if there is no user use this record, then change the status to 'A' 
    *** and adding glBatch.nBatotCr to loFormSet.lnTotalCr,
    ***     adding glBatch.nBatotDr to loFormSet.lnTotalDr

    *lcEditLine=lfCheckEL()
    llPrint = IIF (glSetup.lSetForel,;
              lfCheckEL(),.T.)
               
    IF llPrint 
      IF lfObj_Lock() .AND. gfObj_Lock(.T.)
        REPLACE glBatch.cBatStat WITH  'A'
        gfTableUpdate()
        loFormSet.lnTotalCr = loFormSet.lnTotalCr + glBatch.nBatotCr
        loFormSet.lnTotalDr = loFormSet.lnTotalDr + glBatch.nBatotDr
        =gfAdd_Info('glbatch')
        =gfObj_Lock(.F.) 
        SHOW GET pbApprov,1 PROMPT '\<Disapprove'
      ENDIF
    ELSE
      =gfModalGen("TRM02100B00000","Dialog")  && Printing the edit list
                                          && is a must to approve
                                          && the batch for posting.
    ENDIF  
  ENDIF  
ENDIF
  
*SHOW GETS
lfAfterRowColChange(loFormSet)
SELECT (lnSlct)  

*!**************************************************************************
*!
*!      Function:  lfvAprAll 
*!To approve all transactions ***
*!**************************************************************************
*
FUNCTION  lfvAprAll 
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT glbatch

*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and check the locking status, if there is no user use this 
*** record, then change the status to 'A' --> approve
lnRecNo = RECNO()
lcScope = loFormSet.lcScope

IF loFormSet.llDumyPost 
  SCAN  FOR &lcScope
    IF !glBatch.lLok_Stat 
      REPLACE glBatch.lBatInd WITH  .T.
    ENDIF
  ENDSCAN

ELSE

  SCAN  FOR &lcScope
    *lcEditLine = lfCheckEL()
    llPrint    = IIF (glSetup.lSetForel,;
                 lfCheckEL(),.T.)
    IF llPrint 
      IF !glBatch.lLok_Stat 
        REPLACE glBatch.cBatStat      WITH  'A'
        =gfAdd_Info('glbatch')
      ENDIF
    ENDIF  
  ENDSCAN

ENDIF  

=gfTableUpdate()

=lfCalc(loFormSet)    && call this function to recalculate the total Cr,tral Dr
             && for all approved batches.
             
*** refresh the browse window 
*SHOW WINDOW (lc_TBrowt) REFRESH
GOTO lnRecNo
lfAfterRowColChange(loFormSet)

*=lfwBrowhen()

SELECT (lnSlct)  

*!**************************************************************************
*!
*!      Function:  lfvAprNon 
*!To disapprove all transactions ***
*!**************************************************************************
*
FUNCTION  lfvAprNon 
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT glbatch
*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and change the status for every record have a status 'A' to
*** 'U' or 'O' status according to the diffrent between 
*** glbatch.nBatotdr,glbatch.nbatotcr 
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
lnRecNo = RECNO()

lcScope = loFormSet.lcScope

IF loFormSet.llDumyPost 
  SCAN  FOR &lcScope
    IF !glBatch.lLok_Stat 
      REPLACE glBatch.lBatInd WITH  .F.
    ENDIF
  ENDSCAN

ELSE

  SCAN FOR &lcScope
    IF !glBatch.lLok_Stat 
      REPLACE glBatch.cBatStat WITH ;
      IIF (glBatch.nbatotdr=glBatch.nbatotcr,'U','O')
      =gfAdd_Info('glbatch')    
    ENDIF     
  ENDSCAN
ENDIF  

=gfTableUpdate()

=lfCalc(loFormSet)    && call this function to recalculate the total Cr,tral Dr
             && for all approved batches.

*** refresh the browse window 
*SHOW WINDOW (lc_TBrowt) REFRESH
GOTO lnRecNo
*=lfwBrowhen()
lfAfterRowColChange(loFormSet)


SELECT (lnSlct)  

*!**************************************************************************
*!
*!      Function:  lfvInvert 
*!Change status for each transaction ***
*!**************************************************************************
*
FUNCTION  lfvInvert 
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT glbatch
*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and change the status for every record have a status 'A' to
*** 'U' or 'O' status according to the diffrent between 
*** glBatch.nBatotdr,glBatch.nBatotcr 
*** if the diff. = 0 then status = 'U' ---> unposted
***                  elde status = 'O' ---> out of balance
*** and subtract glBatch.nBatotCr From loFormSet.lnTotalCr,
***     subtract glBatch.nBatotDr From loFormSet.lnTotalDr
lnRecNo = RECNO()

lcScope = loFormSet.lcScope
IF loFormSet.llDumyPost 

  SCAN FOR &lcScope
    IF !glBatch.lLok_Stat 
      IF glBatch.lBatInd
        REPLACE glBatch.lBatInd WITH .F.
      ELSE  
        IF !glBatch.lLok_Stat 
          REPLACE glBatch.lBatInd WITH  .T. 
        ENDIF
      ENDIF  
    ENDIF  
  ENDSCAN

ELSE

  SCAN FOR &lcScope
    IF !glBatch.lLok_Stat 
      IF glBatch.cBatStat = 'A'
        REPLACE glBatch.cBatStat WITH ;
                IIF (glBatch.nbatotdr=glBatch.nbatotcr,'U','O')
        =gfAdd_Info('glbatch')              
      ELSE  
        *** that is mean the user want to approve this record.  
        *** if check list is approved then if valid check the locking status,
        *** if there is no user use this record, then change the status to 'A' 

        *lcEditLine=lfCheckEL()
        llPrint = IIF (glSetup.lSetForel,;
                  lfCheckEL(),.T.)
               
        IF llPrint 
          IF !glBatch.lLok_Stat 
            REPLACE glBatch.cBatStat WITH  'A' 
            =gfAdd_Info('glbatch')          
          ENDIF
        ENDIF  
      ENDIF  
    ENDIF  
  ENDSCAN
ENDIF  

=gfTableUpdate()

=lfCalc(loFormSet)    && call this function to recalculate the total Cr,tral Dr
             && for all approved batches.
           
*** refresh the browse window 
*SHOW WINDOW (lc_TBrowt) REFRESH
GOTO lnRecNo
*=lfwBrowhen()
lfAfterRowColChange(loFormSet)

SELECT (lnSlct)  

*!**************************************************************************
*!
*!      Function:  lfvPost 
*!To post approved transactions ***
*!**************************************************************************
*
FUNCTION  lfvPost
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT glbatch

*** moves the record pointer through the current DBF (GLBATCH)
*** for each record that meets the specified conditions in (lcScope)
*** and if found any record with status 'A' then exit from scan loop

lnRecNo = RECNO()
lcScope = loFormSet.lcScope
DO msg

IF loFormSet.llDumyPost
  LOCATE FOR &lcScope .AND. glBatch.lBatInd
ELSE
  LOCATE FOR &lcScope .AND. glBatch.cBatStat = 'A'
ENDIF
  
IF lnRecNo <= RECCOUNT()
  GOTO lnRecNo
ENDIF  

gcWorkDir = oAriaApplication.WorkDir
lc_ToPost = loFormSet.lc_ToPost

IF FOUND()
  *** check if total credit approved batches = total debit approved batches
  *** if eles messaeg "Total approves batches is out of balance by XXX"+
  *** You cannot post the approved batches until it balance.
  
  IF loFormSet.lnTotalCr <> loFormSet.lnTotalDr 
    =gfModalGen("TRM02101B00000","DIALOG",;
       IIF(loFormSet.llDumyPost,'included','approved')+'|'+;
       LTRIM(STR(ABS(loFormSet.lnTotalDr-loFormSet.lnTotalCr),15,2))+;
       ' ' + IIF(loFormSet.lnTotalDr>loFormSet.lnTotalCr,'Dr','Cr')+'|'+;
       IIF(loFormSet.llDumyPost,'included','approved'))

  ELSE  
    ***  Post approved batches.  <Post>     <Cancel>
    IF gfModalGen("TRM02070B02009","DIALOG",IIF(loFormSet.llDumyPost,'included','approved')) = 1 
      *** calling posting procedure  

      IF loFormSet.llDumyPost
        SELECT cBatchNo,cBatStat FROM GLBATCH   ;
        INTO DBF &gcWorkDir.&lc_ToPost ;
        WHERE  &lcScope .AND. glBatch.lBatInd ;
        ORDER BY cBatchNo
      ELSE
        SELECT cBatchNo FROM GLBATCH   ;
        INTO DBF &gcWorkDir.&lc_ToPost ;
        WHERE  &lcScope .AND. glBatch.cBatStat = 'A';
        ORDER BY cBatchNo
      ENDIF

      SELECT (lc_ToPost)
      INDEX ON CBATCHNO TAG CBATCHNO 
      SET ORDER TO CBATCHNO     
    
      IF lfTBPost(loFormSet,"Batch",lc_ToPost,"Beginning",lcReportFi) > 0
        *WAIT " Posting OK"  WINDOW NOWAIT
        IF loFormSet.llDumyPost
          SELECT (lc_ToPost)
          SET RELATION TO CBATCHNO INTO GLBATCH

          SCAN FOR &lc_ToPost..cBatchNo = 'P'
            REPLACE glBatch.lBatInd WITH .F.
          ENDSCAN
      
          glQuitting  = .T.
          *CLEAR READ
          loFormSet.ariaform1.release()
          
        ENDIF
      ELSE
 *        WAIT " Unsuccessful posting "  WINDOW NOWAIT
      ENDIF    
      SELECT GLBATCH
      LOCATE 
      =lfCalc(loFormSet)   
      *=lfActBrow()
      *=lfRefresh()
      lfAfterRowColChange(loFormSet)

      RETURN 
      
    ELSE
      RETURN
    ENDIF
  ENDIF  
ELSE
  *** if there is no record found with 'A' status 
  *** display "No approved batches to post."
  *** and set the current object to approve
  =gfModalGen("TRM02069B00000","DIALOG",IIF(loFormSet.llDumyPost,'included','approved'))
  *_CUROBJ=OBJNUM(pbApprov)
  loFormset.Ariaform1.pbApprov.SetFocus()
ENDIF  

SELECT (lnSlct)  

*!**************************************************************************
*!
*!      Function:  lfvPrint
*!
*!**************************************************************************
*
FUNCTION  lfvPrint
*** printing function



*!*	*!**************************************************************************
*!*	*!
*!*	*!      Procedure:  lpEscape
*!*	*!
*!*	*!**************************************************************************
*!*	*
*!*	PROCEDURE lpEscape
*!*	*** restores on key labels. 
*!*	*** activate the glTpost1 window and close the screen
*!*	*ON KEY
*!*	ACTIVATE WINDOW glBGPst1
*!*	_CUROBJ=OBJNUM(pbClose)
*!*	KEYBOARD CHR(17)+CHR(13)


*!*	*!**************************************************************************
*!*	*!
*!*	*!      Procedure:  lpTab
*!*	*!
*!*	*!**************************************************************************
*!*	*
*!*	PROCEDURE lpTab 
*!*	*** check if the window on top is (lc_TBrowt) ---> browse window
*!*	*** if yes then change the current object to ( Approve )
*!*	IF WONTOP(lc_TBrowt)
*!*	  _CUROBJ=OBJNUM(pbApprov)
*!*	  ACTIVATE WINDOW glBGPst1
*!*	ELSE
*!*	  *** else change the current object to ( Print )
*!*	*B601099,1 M.H 06/27/96 Begin.
*!*	*  IF     (!llDumyPost .AND. _CUROBJ = OBJNUM(pbPrint)) ;
*!*	    .OR. ( llDumyPost .AND. _CUROBJ = OBJNUM(pbClose))
*!*	  IF _CUROBJ = OBJNUM(pbClose)
*!*	*B601099,1 M.H 06/27/96 End.

*!*	    *** case the current object = (Print) then activate browse 
*!*	    *** window (lc_TBrowT)   
*!*	    ACTIVATE WINDOW (lc_TBrowT)
*!*	  ELSE      
*!*	    *** in this case moves the object to next object     
*!*	    _CUROBJ= _CUROBJ + 1 
*!*	  ENDIF
*!*	ENDIF 

*!*	*!**************************************************************************
*!*	*!
*!*	*!      Procedure:  lpShiftTab
*!*	*!
*!*	*!**************************************************************************
*!*	*
*!*	PROCEDURE lpShiftTab 
*!*	*** check if the window on top is (lc_TBrowt) ---> browse window
*!*	*** if yes then change the current object to ( Close )

*!*	IF WONTOP(lc_TBrowt)
*!*	  IF llDumyPost
*!*	    _CUROBJ=OBJNUM(pbClose)
*!*	  ELSE
*!*	*B601099,1 M.H 06/27/96 Begin.
*!*	*    _CUROBJ=OBJNUM(pbPrint)
*!*	    _CUROBJ=OBJNUM(pbClose)
*!*	*B601099,1 M.H 06/27/96 End.
*!*	  ENDIF
*!*	 
*!*	  ACTIVATE WINDOW glBGPst1
*!*	ELSE
*!*	  *** check if the current object is (Approve) then activate
*!*	  *** the browse window (lc_TBrowT)
*!*	  IF _CUROBJ = OBJNUM(pbApprov)
*!*	    ACTIVATE WINDOW (lc_TBrowT)
*!*	  ELSE      
*!*	    *** in this case moves the object to previous object          
*!*	    _CUROBJ= _CUROBJ - 1 
*!*	  ENDIF
*!*	ENDIF 


*!**************************************************************************
*!
*!      Function:  lfCalc
*!
*!**************************************************************************
*
FUNCTION  lfCalc
PARAMETERS loFormSet
lnSlct = SELECT(0)
SELECT glbatch
*** calculate the totals glBatch.nBatotCr, glBatch.nBatotDr 
*** only for approved batches in scope (lcScope).

loFormSet.lnTotalCr = 0.00
loFormSet.lnTotalDr = 0.00
lcScope = loFormSet.lcScope
LOCATE 

IF loFormSet.llDumyPost
  SUM  glBatch.nBatotCr, glBatch.nBatotDr ;
  FOR &lcScope .AND.  glBatch.lBatInd     ;
  TO loFormSet.lnTotalCr,loFormSet.lnTotalDr
ELSE
  SUM  glBatch.nBatotCr, glBatch.nBatotDr ;
  FOR &lcScope .AND.  glBatch.cBatStat ='A';
  TO loFormSet.lnTotalCr,loFormSet.lnTotalDr
ENDIF

LOCATE 
SELECT (lnSlct)  

*!**************************************************************************
*!					
*!      Function:  lfObj_Lock 
*!
*!**************************************************************************
*
FUNCTION  lfObj_Lock

IF (!llok_Stat) AND (cLok_User <> oAriaApplication.User_Id)
  RETURN .T.
ELSE
  =gfModalGen("TRM02213B00000","ALART",IIF(loFormSet.llDumyPost,'Include/Exclude','Approve/Disapprove')+'|'+'beginning balances batch')
  RETURN .F.
ENDIF
