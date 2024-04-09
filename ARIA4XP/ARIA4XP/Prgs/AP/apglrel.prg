*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\AP\APGLREL.PRG
*:  Module      : System Manager 
*:  Desc.       : Release to AP
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 11/14/2012
*:  Reference   : *E303295 
*:  this program is called from the SMGLREL.PRG 
*: I kept everything in this program as it is , I removed the old fix references
*:
*:************************************************************************
*:*B610170,1 TMI 12/06/2012 [T20121206.0004] replace the correct extension
*:*B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012]
*B610312,1 TMI 04/17/2013 [Media] initialize lcDynMsg variable
*B610362,1 TMI 06/09/2013 open the file exclusively to create the index [T20130604.0013]
*B610430,1 HES fix bug of not found variable while releasing GL [T20130701.0016]
*B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [T20140623.0004]
*B610791,1 TMI 08/04/2014 pass the loFld lfApAcs fn. [T20140724.0006]
*:************************************************************************
*
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc,laField_H,laCtrStat,laCostType
DECLARE laSegSize[1,3]

*B610170,1 TMI 12/06/2012 [Start] define variables
gcWinAppl  = oariaapplication.activemoduleid
gcScrDir   = oariaapplication.ScreenHome
*B610170,1 TMI 12/06/2012 [End  ] 

STORE ' '    TO lcSumAccnt , lcPrnPath  , lcOldSusp
STORE ''     TO lcRelOrExp , lcInfMsg                

STORE 0      TO lnBtchPost , lnBalance  , lnXSize    , lnFrsSegSz

STORE {}     TO ldPrdEDat  , ldGlDate

STORE .F.    TO llGLSETUP  , llGLBATCH  , llGLTRNSHD , llGLTRNSDT ,;
                llBrowse

** Open neccessary files and Validate compnay setup
IF !(lfOpenDBF() .AND. lfValidat())
  =lfCloseDBF()
  RETURN
ENDIF

** Get Release order method and the Accounts to be summarized 
** Create a temporary A.P. distribution file
DO lpRelMethod

WAIT CLEAR

=lfvProceed()

=lfCloseDBF()

*!**************************************************************************
*!
*!      Function: lfOpenDBF
*!
*!**************************************************************************
*
FUNCTION lfOpenDBF

WAIT WINDOW 'Open files... Please standby' NOWAIT

** Check if link with GL module.
IF APSETUP.CAPSGLLINK <> 'Y'
  WAIT CLEAR
  ** Message : " A/P Release is available only    "
  **           " if A/P is linked to G/L module. "
  ** Choices : "                 ® Ok ¯            "
  =gfModalGen('TRM04130B00000','Dialog')
  RETURN .F.
ENDIF

** Open System Company file
SELECT SYCCOMP
** Check existence of the parent company

*- replace gcPrnt_Cmp with (lcGLCo) the variable that hold 
*- the GL Linked company
*IF !SEEK(gcPrnt_Cmp,"SYCCOMP")
IF !SEEK(lcGLCo,"SYCCOMP")
  WAIT CLEAR
  ** Message : " The linked GL company not found. "
  **           " Release to GL is canceled.       "
  ** Choices : "                  ® Ok ¯          "
  =gfModalGen('TRM04131B00000','Dialog')
  RETURN .F.
ENDIF

** Get the path of Gl module
*E301098,1 Hesham (Start)
*lcPrnPath = ALLTRIM(SYCCOMP.CCOM_DDIR)
lcPrnPath = gfGetDataDir(ALLTRIM(SYCCOMP.CCOM_DDIR))
*E301098,1 Hesham (End)
IF glMapPath
  lcPrnPath = gfFixPath(lcPrnPath)
ENDIF

*- Commented out this part of code and rewritting it (End)
IF !llUnComFnd AND lcCurProc < '5'
  ** Create Batch temporary file
  lcTmpBatch  = gfTempName()
  DIMENSION laFileStru[1,4]
  STORE SPACE(0) TO laFileStru
  SELECT(lcBatchAl)
  = AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  *E303295 TMI 11/12/2012 [Start] 
  lnOrgLen = lnFileStru 
  *E303295 TMI 11/12/2012 [End  ] 
  DIMENSION laFileStru[lnFileStru + 1, 18]
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'nStep'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0
    *E303281 TMI 11/12/2012 [Start]     
FOR i= lnOrgLen+1 TO ALEN(laFileStru,1)
  STORE .F. TO laFileStru[i,5],laFileStru[i,6]
  STORE '' TO laFileStru[i,7],laFileStru[i,8],laFileStru[i,9],laFileStru[i,10],laFileStru[i,11],laFileStru[i,12],laFileStru[i,13],laFileStru[i,14],laFileStru[i,15],laFileStru[i,16]
  STORE 0 TO laFileStru[i,17],laFileStru[i,18]    
ENDFOR 
    *E303281 TMI 11/12/2012 [End  ] 
  CREATE TABLE (gcWorkDir + lcTmpBatch) FROM ARRAY laFileStru
  lnNdxCnt = 1
  SELECT(lcBatchAl)
  DO WHILE !EMPTY(KEY(lnNdxCnt))
    lcKeyExp = KEY(lnNdxCnt)
    lcTag = TAG(lnNdxCnt)
    SELECT (lcTmpBatch)
    INDEX ON &lcKeyExp TAG &lcTag  OF (gcWorkDir+lcTmpBatch+'.CDX') 
    SELECT(lcBatchAl)
    lnNdxCnt = lnNdxCnt + 1
  ENDDO
  SET ORDER TO 0 IN (lcBatchAl)
  SELECT(lcTmpBatch)
  USE
  USE (gcWorkDir+lcTmpBatch) EXCLUSIVE

  ** Create Transaction header temporary file
  lcTmpTrnHd  = gfTempName()
  SELECT (lcHTrnsAl)
  DIMENSION laFileStru[1,4]
  STORE SPACE(0) TO laFileStru
  = AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  *E303295 TMI 11/12/2012 [Start] 
  lnOrgLen = lnFileStru 
  *E303295 TMI 11/12/2012 [End  ] 
  DIMENSION laFileStru[lnFileStru + 1, 18]
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'nStep'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0
    *E303281 TMI 11/12/2012 [Start]     
FOR i= lnOrgLen+1 TO ALEN(laFileStru,1)
  STORE .F. TO laFileStru[i,5],laFileStru[i,6]
  STORE '' TO laFileStru[i,7],laFileStru[i,8],laFileStru[i,9],laFileStru[i,10],laFileStru[i,11],laFileStru[i,12],laFileStru[i,13],laFileStru[i,14],laFileStru[i,15],laFileStru[i,16]
  STORE 0 TO laFileStru[i,17],laFileStru[i,18]    
ENDFOR 
    *E303281 TMI 11/12/2012 [End  ] 
  CREATE TABLE (gcWorkDir + lcTmpTrnHd) FROM ARRAY laFileStru
  lnNdxCnt = 1
  SELECT(lcHTrnsAl)
  DO WHILE !EMPTY(KEY(lnNdxCnt))
    lcKeyExp = KEY(lnNdxCnt)
    lcTag = TAG(lnNdxCnt)
    SELECT (lcTmpTrnHd)
    INDEX ON &lcKeyExp TAG &lcTag OF (gcWorkDir+lcTmpTrnHd+'.CDX') 
    SELECT(lcHTrnsAl)
    lnNdxCnt = lnNdxCnt + 1
  ENDDO
  SET ORDER TO 0 IN (lcHTrnsAl)
  SELECT(lcTmpTrnHd)
  USE
  USE (gcWorkDir+lcTmpTrnHd) EXCLUSIVE

  ** Create Transaction Detail temporary file
  lcTmpTrnDt  = gfTempName()
  SELECT (lcDTrnsAl)
  DIMENSION laFileStru[1,4]
  STORE SPACE(0) TO laFileStru
  = AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  *E303295 TMI 11/12/2012 [Start] 
  lnOrgLen = lnFileStru 
  *E303295 TMI 11/12/2012 [End  ] 
  DIMENSION laFileStru[lnFileStru + 1, 18]
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'nStep'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0
    *E303281 TMI 11/12/2012 [Start]     
FOR i= lnOrgLen+1 TO ALEN(laFileStru,1)
  STORE .F. TO laFileStru[i,5],laFileStru[i,6]
  STORE '' TO laFileStru[i,7],laFileStru[i,8],laFileStru[i,9],laFileStru[i,10],laFileStru[i,11],laFileStru[i,12],laFileStru[i,13],laFileStru[i,14],laFileStru[i,15],laFileStru[i,16]
  STORE 0 TO laFileStru[i,17],laFileStru[i,18]    
ENDFOR 
    *E303281 TMI 11/12/2012 [End  ] 
  CREATE TABLE (gcWorkDir + lcTmpTrnDt) FROM ARRAY laFileStru
  SELECT (lcTmpTrnDt)
  USE
  USE (gcWorkDir + lcTmpTrnDt) EXCLUSIVE
ENDIF

WAIT CLEAR

*!**************************************************************************
*!
*!      Function: lfvProceed
*!
*!**************************************************************************
*
FUNCTION lfvProceed

*-- Release AP entries
IF lfRelease()
  ** Lock master files
  
  *- Useing GLBATCH,GLTRNSHD,GLTRNSDT files with there aliases name
  *- that are created in the calling program SMGLREL
  *-  Start
  *IF gfFLOCK("GLBATCH,GLTRNSHD,GLTRNSDT",.T.)
  IF gfFLOCK(lcBatchAl+','+lcHTrnsAl+','+lcDTrnsAl,.T.)
  
    ** Update master files
    =lfUpdFiles()
    ** Unock master files
    *- Useing GLBATCH,GLTRNSHD,GLTRNSDT files with there aliases name
    *- that are created in the calling program SMGLREL
    *- Start
    *=gfFLOCK("APDIST,GLBATCH,GLTRNSHD,GLTRNSDT",.F.)
    =gfFLOCK("APDIST,"+lcBatchAl+','+lcHTrnsAl+','+lcDTrnsAl,.F.)
    
    =gfModalGen('TRM00129B00000','Dialog',"Release"+"|"+"successfully done")
  ELSE
    =gfModalGen('TRM00105B00000','Dialog')
  ENDIF  
  ** Unock master files
  *- Useing GLBATCH,GLTRNSHD,GLTRNSDT files with there aliases name
  *- that are created in the calling program SMGLREL
  *- Start
  *=gfFLOCK("APDIST,GLBATCH,GLTRNSHD,GLTRNSDT",.F.)
  =gfFLOCK("APDIST,"+lcBatchAl+','+lcHTrnsAl+','+lcDTrnsAl,.F.)
  
ENDIF

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfValidat
*!
*!**************************************************************************
*
FUNCTION lfValidat

*- Comment out this part of code because 
*- This check is already done in the calling program "SMGLREL" program
*- Release to G/L program

*- Begin
*WAIT WINDOW 'Checking system setup... Please standby' NOWAIT

*** Check if the GL module is installed for the parent company
*IF !GLSETUP.LSETDON 
*  WAIT CLEAR
*  ** Message : " The GL module has not been setup for  "  ERROE
*  **           " company ð.  Release to GL is canceled."  
*  ** Choices : "                  ® Ok ¯               "
*  ** ð is the parent company
*  =gfModalGen('TRM04133B00000','Dialog',gcPrnt_Cmp)
*  RETURN .F.
*ENDIF  


IF APSETUP.NAPSRBPW <= 0
  WAIT CLEAR
  ** MESSAGE : " ð should be greater than ð."
  **           "           ® Ok ¯           "
  =gfModalGen("TRM04072B00000","DIALOG",'The number of released batches posting periods|zero')
  RETURN .F.
ENDIF

** Check if the number of released batches posting periods is greater
** than the number of the batches posting periods

*- Useing GLSETUP file with its aliase name
*- which is created in the calling program SMGLREL
*- Start
*IF APSETUP.NAPSRBPW > GLSETUP.NSETBATPW
IF APSETUP.NAPSRBPW > &lcGLSetAl..NSETBATPW

  WAIT CLEAR
  ** Message : " The AP released batches posting window,"
  **           " currently ð periods,has to be within   "
  **           " the batches posting window of GL,      "
  **           " currently ð periods.  Release to GL is "
  **           " canceled.                              "
  ** Choices : "                 ® Ok ¯                 "
  ** ð is the parent company
  *- Useing GLSETUP file with its aliase name
  *- which is created in the calling program SMGLREL
  *- Start
  *=gfModalGen('TRM04134B00000','Dialog',ALLTRIM(STR(APSETUP.NAPSRBPW,2))+"|"+ALLTRIM(STR(GLSETUP.NSETBATPW,2)))
  =gfModalGen('TRM04134B00000','Dialog',ALLTRIM(STR(APSETUP.NAPSRBPW,2))+"|"+ALLTRIM(STR(&lcGLSetAl..NSETBATPW,2)))
    
  RETURN .F.
ENDIF

** Store the number of released batches posting periods 
lnBtchPost = APSETUP.NAPSRBPW
IF lcCurProc < '6'
  SELECT APDIST
  IF !SEEK(.F.)
    WAIT CLEAR
    ** Message : " There are no entries to post."
    ** Choices : "            ® Ok ¯            "
    IF llNoGLRec
      =gfModalGen('TRM04135B00000','Dialog')
    ENDIF
    RETURN .F.
  ENDIF  
ENDIF  

*E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
*SELECT SYCACCOD

*- the GL Linked company
*- Start

*- Using ACCOD File with its alias name which is used by alias name 
*-  in the calling program SMGLREL
*- Start
*SELECT ACCOD
SELECT (lcAcCodAl)


*E301173,1 (Start) AHM
*--As a result of removing comp id field from account structure code
*IF !SEEK(gcPrnt_Cmp)
GO TOP
IF EOF()
*E301173,1 (End) AHM


  WAIT CLEAR
  ** Message : " The account code structure has not"
  **           " been setup for company ð. Release "
  **           " to GL is canceled.                "
  ** Choices : "                  ® Ok ¯           "
  ** ð is the parent company
  *- replace gcPrnt_Cmp with (lcGLCo) the variable that hold 
  *- the GL Linked company
  *- Start
  *=gfModalGen('TRM04136B00000','Dialog',gcPrnt_Cmp)
  =gfModalGen('TRM04136B00000','Dialog',lcGLCo)
  
  RETURN .F.
ENDIF  


*- Following lines are commented out
*lnAcsNoSeg = nAcsNoSeg
*lnXSize    = LEN(SUBSTR(SYCACCOD.Cacsmask,1,AT('-',SYCACCOD.Cacsmask)-1))
*lnFrsSegSz = lnXSize
*- Eidt Suspense Account Full Length
lnXSize = LEN(lcSuspense)
*- (End)

** The Chart of account file

*-- using lclinkchar as a variable (AAMER) Start
*--the file is used with not an alias name direct
*SELECT lcLinkChar
SELECT (lcLinkChar)
*-- using lclinkchar as a variable (AAMER) Start

IF !SEEK(lcSuspense)
  WAIT CLEAR
  ** Message : " The suspense account does not exist "
  **           " in the chart of accounts. Release to"
  **           " GL is canceled.                     "
  ** Choices : "                ® Ok ¯               "
  =gfModalGen('TRM04137B00000','Dialog')
  RETURN .F.
ENDIF  

*-- using lclinkchar as a variable (AAMER) Start
*IF lcLinkChar.CSegActiv = "I"
IF &lcLinkChar..CSegActiv = "I"
*-- using lclinkchar as a variable (AAMER) End
  WAIT CLEAR
  ** Message : " Suspense account ð is marked is inactive "
  **           " in the chart of accounts. Release to     "
  **           " GL is canceled.                          "
  ** Choices : "                ® Ok ¯                    "
  =gfModalGen('TRM04142B00000','Dialog',ALLTRIM(lcSuspense))
  RETURN .F.
ENDIF                   

*!**************************************************************************
*!
*!      Function : lfCloseDBF
*!
*!**************************************************************************
*
FUNCTION lfCloseDBF



** Erase temporary files and indeses opened by this program
IF USED(lcTmpAPDist)
  USE IN (lcTmpAPDist)
ENDIF
ERASE (gcWorkDir+lcTmpAPDist+'.DBF')
ERASE (gcWorkDir+lcTmpAPDist+'.IDX')

IF USED(lcTmpBatch)
  USE IN (lcTmpBatch)
ENDIF
ERASE (gcWorkDir+lcTmpBatch+'.DBF')
ERASE (gcWorkDir+lcTmpBatch+'.CDX')

IF USED(lcTmpTrnHd)
  USE IN (lcTmpTrnHd)
ENDIF  
ERASE (gcWorkDir+lcTmpTrnHd+'.DBF')
ERASE (gcWorkDir+lcTmpTrnHd+'.CDX')

IF USED(lcTmpTrnDt)
  USE IN (lcTmpTrnDt)
ENDIF  
ERASE (gcWorkDir+lcTmpTrnDt+'.DBF')
ERASE (gcWorkDir+lcTmpTrnDt+'.CDX')

*!**************************************************************************
*!
*!      Function : lfRelease
*!
*!**************************************************************************
*
FUNCTION lfRelease

PRIVATE lnBatchNo ,lnTranNo  ,lcFisYear ,lnPeriod  ,lcTransDoc,lcGlAcnt,;
        lnTotTrnDr,lnTotTrnCr,lnTotBatDr,lnTotBatCr,lnAmount  ,ldGlDate,;
        llContinue, lnEntryNo, llTransAll, lnResponse

** Lock A.P. distribution file
IF !gfFLOCK("APDIST",.T.)
  =gfModalGen('TRM00105B00000','Dialog')
  =gfFLOCK("APDIST",.F.)  
  RETURN .F.
ENDIF  

lnBatchNo  = 0      && Batch Sequence number
lnTranNo   = 0      && Transaction Sequence number
lcFisYear  = ""     && Fiscal Year
lnPeriod   = 0      && Period ID
lcTransDoc = ""     && 
lcGlAcnt   = ""     && Account ID
lnTotTrnDr = 0      && Transaction total debit
lnTotTrnCr = 0      && TRansaction total credit
lnTotBatDr = 0      && Batch total debit
lnTotBatCr = 0      && Batch total credit
lnAmount   = 0      && Entry amount
*- Replacing llContinue Variable with llGoOn variable which is used
*- as a continuation flag in the calling program SMGLREL to avoid
*- over writting this variable because it is used in the global 
*- function which restore laVars array elements that is used in
*- saving program variable in uncomplete session
*- Start
*llContinue = .T.    && Continuation flag
llGoOn = .T.    && Continuation flag

lnEntryNo  = 0      && Transaction Entry Number
llTransAll = .F.
lnResponse = 1

lnBtchPost = lnBtchPost - 1

SELECT (lcTmpAPDist)
lnReccount = RECCOUNT()
lnCount = 0
*- Add this IF Condition for uncomplete session
*- Start
IF lcCurProc < '6'

  DO WHILE !EOF()
    *- (Begin) Call lfGetPerd() to get the period and year if both or date are empty.
    *------ If the function change anything in the file, loop. Refer to documentation in the function.
    IF lfGetPerd()
      LOOP
    ENDIF
    

    ** Initialize the batch total debit and total credit
    lnTotBatDr = 0
    lnTotBatCr = 0
  
    ** Save Fiscal year and period ID for the batch      
    lcFisYear = &lcTmpAPDist..CFISFYEAR
    lnPeriod  = INT(VAL(&lcTmpAPDist..CFSPPRDID))
    ** Get the Batch Begin Date
    *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
    *ldPrdBDat = SYCFSPRD.dFsPpBgDt
    *- Replace FsPrd File with alias name which is opened with 
    *- from the calling program SMGLREL to be opened from the rigth path
    *- Start
    *ldPrdBDat = FSPRD.dFsPpBgDt
    ldPrdBDat = &lcFsPrdAl..dFsPpBgDt
    
    *E300692,1 end
  
    ** Increament the batch sequence number
    lnBatchNo = lnBatchNo + 1
  
    ** Add batch record to GLBTCH temporary file
    SELECT (lcTmpBatch)
    APPEND BLANK

    REPLACE CBATCHNO  WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0')   ,;
            CBATDESC  WITH "Posting from AP module, " + lcComp ,;
            CBATPYR   WITH lcFisYear                               ,;
            DBATPBEG  WITH ldPrdBDat                               ,;
            CBATREFER WITH "On "+ DTOC(gdSysDate)             ,;
            CBATTYPE  WITH "L"                                     ,;
            CBATSTAT  WITH "U"                                     ,;
            CSRCMODUL WITH "AP"                                    ,;
            CCOMP_ID  WITH lcComp
    =RLOCK(lcTmpBatch)
    UNLOCK IN (lcTmpBatch)
     
    ** Process records for batch
    SELECT (lcTmpAPDist)

    ** Create one Batch for every Fiscal Year and Period within the Released 
    ** Batches Posting Window
    
    *- Replacing llContinue Variable with llGoOn variable which is used
    *- as a continuation flag in the calling program SMGLREL to avoid
    *- over writting this variable because it is used in the global 
    *- function which restore laVars array elements that is used in
    *- saving program variable in uncomplete session
    *- Start
    *DO WHILE !EOF() .AND. llContinue .AND. CFISFYEAR = lcFisYear .AND. ;
             BETWEEN(VAL(CFSPPRDID), lnPeriod,lnPeriod+lnBtchPost)
    DO WHILE !EOF() .AND. llGoOn .AND. CFISFYEAR = lcFisYear .AND. ;
             BETWEEN(VAL(CFSPPRDID), lnPeriod,lnPeriod+lnBtchPost)
    
      ** Check the Year - Period
      IF !lfPrdVal(&lcTmpAPDist..CFISFYEAR,&lcTmpAPDist..CFSPPRDID)
        *-- Terminate the program if invalid Year/Period

        *- Replacing llContinue Variable with llGoOn variable which is used
        *- as a continuation flag in the calling program SMGLREL to avoid
        *- over writting this variable because it is used in the global 
        *- function which restore laVars array elements that is used in
        *- saving program variable in uncomplete session
        *- Start
        *llContinue = .F.
        llGoOn = .F.
        
        EXIT
      ENDIF  
           
      lnTranNo   = lnTranNo + 1
      lnEntryNo  = 0
      lnTotTrnDr = 0
      lnTotTrnCr = 0
      ldGlDate   = &lcTmpAPDist..DAPDTRDAT
      lcTransDoc = &lcRelOrExp
      ** Add transaction header record to GLHUNP
      SELECT (lcTmpTrnHd)
      APPEND BLANK

      *C037437,1 MHM 02/05/2004 Replace lcTmpTrnHd..CTRNDESC with new value for Suzan[End]
      IF ASCAN(laEvntTrig,PADR('UPDTGLTR',10))<>0
        =gfDoTriger('SMGLREL',PADR('UPDTGLTR',10))
      ENDIF
      *C037437,1 MHM 02/05/2004 [End]
      REPLACE CBATCHNO   WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0')   ,;
              CTRANNO    WITH PADL(lnTranNo,FSIZE('CTRANNO'),'0')     ,;
              CTRNDESC   WITH &lcInfMsg                               ,;
              CTRNREFER  WITH "On "+ DTOC(gdSysDate)                  ,;
              DTRNPDATE  WITH ldGlDate                                ,;
              CTRNPYR    WITH lcFisYear                               ,;
              CTRNPPRD   WITH PADL(lnPeriod,2,"00")                   ,;
              CTRNSTAT   WITH "U"                                     ,;
              CTRNTYPE   WITH "N"                                     ,;
              CSRCMODUL  WITH "AP"                                    ,;
              CCOMP_ID   WITH lcComp                                  ,;
              CTRNREVER  WITH "N"                                     ,;
              CSTANDARD  WITH "Y"                                     ,;
              CSRCJRNL   WITH "GJ"
      =RLOCK(lcTmpTrnHd)
      UNLOCK IN (lcTmpTrnHd)
      
    
      *C100713,1 M.H End.

      ** Process records for transaction
      SELECT (lcTmpAPDist)

      ** Create one Transaction for every Year/Period plus the Transaction 
      ** Grouping Releasing method as get from the AP setup.
      DO WHILE !EOF() .AND. &lcRelOrExp = lcTransDoc
        lnAmount = 0
        lcGlAcnt = &lcTmpAPDist..CAPDGLACT
        lcTmpAccnt = lcGlAcnt

        ** Check this account
        IF !lfAccValid('lcGlAcnt')
          *- Replacing llContinue Variable with llGoOn variable which is used
          *- as a continuation flag in the calling program SMGLREL to avoid
          *- over writting this variable because it is used in the global 
          *- function which restore laVars array elements that is used in
          *- saving program variable in uncomplete session
          *- Start
          *llContinue = .F.
          llGoOn = .F.
          
          EXIT
        ENDIF  
      
        lnEntryNo = lnEntryNo + 1
        IF &lcTmpAPDist..CAPDACTID $ lcSumAccnt
          ** Account to be summarized
        
          lcAccntId = &lcTmpAPDist..CAPDACTID
          SCAN WHILE &lcRelOrExp = lcTransDoc ;
               .AND. &lcTmpAPDist..CAPDACTID = lcAccntId ;
               .AND. &lcTmpAPDist..CAPDGLACT = lcTmpAccnt
            *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
            *ldPrdEDat  = SYCFSPRD.DFSPPENDT              
       
            *- Replace FsPrd File with alias name which is opened with 
            *- from the calling program SMGLREL to be opened from the rigth path
            *- Start
            *ldPrdEDat  = FSPRD.DFSPPENDT
            ldPrdEDat  = &lcFsPrdAl..DFSPPENDT
            
            *E300692,1 end
            ** Update Batch number, Transaction number and Entry Number in 
            ** the temporary Distribution file       
            REPLACE CBATCHNO  WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'), ;
                    CTRNSLEDN WITH PADL(lnTranNo,FSIZE('CTRNSLEDN'),'0'), ;
                    NENTRYNO  WITH lnEntryNo
            =RLOCK(lcTmpAPDist)
            UNLOCK IN (lcTmpAPDist)

            ** Increment the amount
            lnAmount = lnAmount + NAPDAMNT
            lnCount = lnCount + 1
            =gfThermo(lnReccount,lnCount,'Generating...')
          ENDSCAN            
        ELSE  
          ** Update Batch number, Transaction number and Entry Number in 
          ** the temporary Distribution file       
          REPLACE CBATCHNO  WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'), ;
                  CTRNSLEDN WITH PADL(lnTranNo,FSIZE('CTRNSLEDN'),'0'), ;
                  NENTRYNO  WITH lnEntryNo
          =RLOCK(lcTmpAPDist)
          UNLOCK IN (lcTmpAPDist)
          *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
          *ldPrdEDat  = SYCFSPRD.DFSPPENDT            

          *- Replace FsPrd File with alias name which is opened with 
          *- from the calling program SMGLREL to be opened from the rigth path
          *- Start
          *ldPrdEDat  = FSPRD.DFSPPENDT            
          ldPrdEDat  = &lcFsPrdAl..DFSPPENDT            
          
          *E300692,1 end
          lnAmount = NAPDAMNT 
          lnCount = lnCount + 1        
          =gfThermo(lnReccount,lnCount,'Generating...')
        
          SKIP IN (lcTmpAPDist)
        ENDIF
      
        ** Add transaction detail record to GLDUNP
        SELECT (lcTmpTrnDt)
        APPEND BLANK

        REPLACE CACCTCODE WITH lcGlAcnt                             ,;
                CBATCHNO  WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
                CTRANNO   WITH PADL(lnTranNo,FSIZE('CTRANNO'),'0')  ,;
                DTRNPDATE WITH ldGlDate                             ,;
                NAMOUNT   WITH ABS(lnAmount)                        ,;
                CDRORCR   WITH IIF(lnAmount < 0,'C','D')            ,;
                CTRNPYR   WITH lcFisYear                            ,;
                CTRNPPRD  WITH PADL(lnPeriod,2,"00")                ,; 
                NENTRYNO  WITH lnEntryNo                            ,;
                CTRDTEXP  WITH &lcTmpTrnHd..CTRNDESC
        =RLOCK(lcTmpTrnDt)
        UNLOCK IN (lcTmpTrnDt)
        *C100713,1 M.H Begin.
              
        ** Increment debit and credit totals for transaction and batch
        lnTotTrnDr = lnTotTrnDr + IIF(CdRORCR ='D', NAMOUNT, 0)
        lnTotTrnCr = lnTotTrnCr + IIF(CdRORCR ='C', NAMOUNT, 0)
        
        ** Already on next detail record
         SELECT (lcTmpAPDist)
      ENDDO
    
      *IF WEXIST('gwdThermo')
        =gfThermo(lnReccount,lnReccount,'Generating...')
      *ENDIF

      ** If transaction is out of balance
 
      *- Replacing llContinue Variable with llGoOn variable which is used
      *- as a continuation flag in the calling program SMGLREL to avoid
      *- over writting this variable because it is used in the global 
      *- function which restore laVars array elements that is used in
      *- saving program variable in uncomplete session
      *- Start
      *IF lnTotTrnDr <> lnTotTrnCr .AND. llContinue
      IF lnTotTrnDr <> lnTotTrnCr .AND. llGoOn
      
        ** Setup out of balance amount
        lnAmount = lnTotTrnCr - lnTotTrnDr

        ** Display alert
        ** Message : " Transaction is out of balance by $ ."
        **           " Transfer An offsetting amount to       "
        **           " the suspence account .                "
        ** Choices : " < Transfer > < Transfer All > < Edit > < Cancel > "
        IF !llTransAll
          DO WHILE .T.
            lnResponse = gfModalGen('TRM04138B04006','Dialog',;
                         ALLTRIM(STR(lnTranNo))+"|"+LTRIM(STR(lnAmount, 10, 2));
                         +"|"+ALLTRIM(lcSuspense))
            IF lnResponse <> 3
              EXIT
            ENDIF
            ** Edit the Suspense Account
            * Edit Suspense Account Full Length, Default with 
            * Suspense Account
            lcSelSusp = lcSuspense
            *B610170,1 TMI 12/06/2012 [Start] use the scx version
            *DO (gcScrDir + 'AP' + '\APSUSPAC.SPR')
            DO FORM (gcScrDir + 'AP' + '\APSUSPAC.scx')
            *B610170,1 TMI 12/06/2012 [End  ] 
          
          ENDDO
          IF lnResponse = 4
      
            *- Replacing llContinue Variable with llGoOn variable which is used
            *- as a continuation flag in the calling program SMGLREL to avoid
            *- over writting this variable because it is used in the global 
            *- function which restore laVars array elements that is used in
            *- saving program variable in uncomplete session
            *- Start
            *llContinue = .F.          
            llGoOn = .F.          
            
            EXIT
          ENDIF  
          llTransAll = (lnResponse=2)
        ENDIF  

        ** Add suspense transaction detail record to GLDUNP
        SELECT (lcTmpTrnDt)
        APPEND BLANK
        REPLACE CACCTCODE   WITH lcSuspense                           ,;
                CBATCHNO  WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
                CTRANNO   WITH PADL(lnTranNo,FSIZE('CTRANNO'),'0')  ,;
                CTRDTEXP  WITH "Suspense - Out of Balance"          ,;
                DTRNPDATE WITH ldGlDate                             ,;
                NAMOUNT   WITH ABS(lnAmount)                        ,;
                CDRORCR   WITH IIF(lnAmount < 0, "C", "D")          ,;
                CTRNPYR   WITH lcFisYear                            ,;
                CTRNPPRD  WITH PADL(lnPeriod,2,"00")                ,;
                NENTRYNO  WITH lnEntryNo
        =RLOCK(lcTmpTrnDt)
        UNLOCK IN (lcTmpTrnDt)
      ENDIF

      ** Already on next detail record
      SELECT (lcTmpAPDist)
    ENDDO
    
    *- Replacing llContinue Variable with llGoOn variable which is used
    *- as a continuation flag in the calling program SMGLREL to avoid
    *- over writting this variable because it is used in the global 
    *- function which restore laVars array elements that is used in
    *- saving program variable in uncomplete session
    *- Start
    *IF !llContinue
    IF !llGoOn
    
      EXIT
    ENDIF

    ** Update the Batch End Date by the end Date of the last period in the batch
    SELECT (lcTmpBatch)
    REPLACE DBATPEND WITH ldPrdEDat
    =RLOCK(lcTmpBatch)
    UNLOCK IN (lcTmpBatch)

    SELECT (lcTmpAPDist)
  ENDDO

  IF WVISIBLE('gwdThermo')
    RELEASE WINDOW gwdThermo
  ENDIF
 
  lcFiles = "lcTmpBatch,"  + lcTmpBatch  + "," + ORDER(lcTmpBatch)  + ";" + ;
            "lcTmpTrnHd,"  + lcTmpTrnHd  + "," + ORDER(lcTmpTrnHd)  + ";" + ;
            "lcTmpTrnDt,"  + lcTmpTrnDt  + "," + ORDER(lcTmpTrnDt)  + ";" + ;
            "lcTmpAPDist," + lcTmpAPDist + "," + ORDER(lcTmpAPDist) + ";"
  lcCurProc = '6'
  llNoThing = lfUpdUnCmS("Open", lcCurProc)

*- Add this IF Condition for uncomplete session
*- Start
ENDIF       &&IF lcCurProc < '6'


*- Replacing llContinue Variable with llGoOn variable which is used
*- as a continuation flag in the calling program SMGLREL to avoid
*- over writting this variable because it is used in the global 
*- function which restore laVars array elements that is used in
*- saving program variable in uncomplete session
*- Start
*RETURN(llContinue)
RETURN(llGoOn)


*!**************************************************************************
*!
*!      Function : lfPrdVal
*!
*!**************************************************************************
*
FUNCTION lfPrdVal
PARAMETERS lcFisYear,lcPeriod
PRIVATE llValidPrd

llValidPrd = .T.
** Check for invalid, deleted, or locked posting period
DO CASE
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
  *CASE EOF('SYCFSPRD')

  *- Replace FsPrd File with alias name which is opened with 
  *- from the calling program SMGLREL to be opened from the rigth path
  *- Start
  * CASE EOF('FSPRD')
  CASE EOF(lcFsPrdAl)
  
  *E300692,1 end
    ** If invalid, set flag to cancel and display alert
    llValidPrd = .F.

    ** Message : "Posting period " + lcPeriod + "-" + ;
    **            lcFisYear +  " not found in fiscal calendar." 
    **           " Release to GL is canceled."
    **    Choices : ® Ok ¯
    lcPerYer = lcPeriod+"-"+lcFisYear+" not found"
    =gfModalGen('TRM04139B00000','Dialog',lcPerYer+"|"+" ")

  *E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
  *CASE SYCFISHD.CFISYSTAT = 'H'
  CASE FISHD.CFISYSTAT = 'H'
  *E300692,1 end
    ** If history period
    llValidPrd = .F.

    ** Message : "Posting period " + lcPeriod + "-" + ;
    **           lcFisYear + " is a history period in fiscal calendar." 
    **           " Release to Gl is canceled."
    ** Choices : ® Ok ¯
    
    lcPerYer = lcPeriod+"-"+lcFisYear+" is a history period"
    =gfModalGen('TRM04139B00000','Dialog',lcPerYer+"|"+" ")

  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
  *CASE SYCFSPRD.lFsPLocks

  *- Replace FsPrd File with alias name which is opened with 
  *- from the calling program SMGLREL to be opened from the rigth path
  *- Start
  *CASE FSPRD.lFsPLocks
  CASE &lcFsPrdAl..lFsPLocks
  
  *E300692,1 end
    ** If period marked as locked,
    ** set flag to cancel and display alert
    llValidPrd = .F.

    ** Message : "Posting period " + lcPeriod + "-" + ;
    **            lcFisYear + " is locked in fiscal calendar."
    **           " You must unlock the period before postings can be released." 
    **           " Release to Gl is canceled."
    **   Choices : ® Ok ¯               

    lcPerYer = lcPeriod+"-"+lcFisYear+" is locked"
    lcMsgTxt = " You must unlock the period before postings can be released. " 

    =gfModalGen('TRM04139B00000','Dialog',lcPerYer+"|"+lcMsgTxt)
ENDCASE
      
RETURN(llValidPrd)

*!**************************************************************************
*!
*!      Function : lfAccValid
*!
*!**************************************************************************
*
FUNCTION lfAccValid
PARAMETERS lcAccount
PRIVATE llValidAcc, lnResponse, lcOldAlias

lcOLdAlias = ALIAS()

** Check for invalid, deleted, or inactive account number
llValidAcc = .T.
DO CASE
  *-- using lclinkchar as a variable (AAMER) Start
  *CASE EOF('lcLinkChar')
  CASE EOF(lcLinkChar)
  *-- using lclinkchar as a variable (AAMER) Start
    ** If invalid, display dialog
    llValidAcc = .F.
    lcMsgTxt = ALLTRIM(&lcAccount) + " not found"

    ** Message : "Account " + ALLTRIM(&lcAccount) + ;
    **           " not found in Chart of Accounts." 
    **           " Transfer to Suspense Account (" + ALLTRIM(lcSuspense) + ")"
    **           " or cancel release to GL?"
    ** Choices : "          < Transfer > < Cancel >     "
  *-- using lclinkchar as a variable (AAMER) Start
  *CASE lcLinkChar.CSegActiv = "I"
  CASE &lcLinkChar..CSegActiv = "I"
  *-- using lclinkchar as a variable (AAMER) End
    ** If inactive, display dialog
    llValidAcc = .F.
    lcMsgTxt = ALLTRIM(&lcAccount) + " is marked as Inactive"
    ** Message : "Account " + ALLTRIM(&lcAccount) + ;
    **           " is marked as Inactive in Chart of Accounts." 
    **           " Transfer to Suspense Account (" + ALLTRIM(lcSuspense) + ")"
    **           " or cancel release to GL?"
    ** Choices : "  < Suspense > < Cancel >     "
ENDCASE
*-- Display error dialog if any
IF ! llValidAcc 
  IF !llTransAll
    DO WHILE .T.
      lnResponse = gfModalGen('TRM04140B04006','Dialog',;
                   lcMsgTxt+"|"+ALLTRIM(lcSuspense))
      IF lnResponse = 3
        ** Edit the Suspense Account
        *- Edit Suspense Account Full Length, Default with Suspense Account.
        lcSelSusp = lcSuspense
        *B610170,1 TMI 12/06/2012 [Start] use the scx version 
        *DO (gcScrDir + 'AP' + '\APSUSPAC.SPR')
        DO FORM (gcScrDir + 'AP' + '\APSUSPAC.scx')
        *B610170,1 TMI 12/06/2012 [End  ] 
        
        LOOP
      ENDIF
      EXIT
    ENDDO  
    llValidAcc = !(lnResponse=4)           && Select Cancel
    llTransAll = (lnResponse=2)            && Select Transfer All  
    &lcAccount = IIF(llValidAcc, lcSuspense, &lcAccount)
  ELSE
    llValidAcc = .T.
    &lcAccount = lcSuspense
  ENDIF  
ENDIF  
SELECT (lcOLdAlias)
RETURN(llValidAcc)

*!**************************************************************************
*!
*!      Function : lfUpdFiles
*!
*!**************************************************************************
*
FUNCTION lfUpdFiles

** Set indeses and relations
SELECT (lcTmpAPDist)
SET RELATION TO
INDEX ON CBATCHNO+CTRNSLEDN+STR(NENTRYNO,4)+STR(RECNO(),7) TO (gcWorkDir+lcTmpAPDist)

SELECT (lcTmpTrnHd)
SET ORDER TO TAG BATCHTRN

SELECT (lcTmpBatch)
SET ORDER TO TAG BATCHNO

SELECT (lcTmpTrnDt)
SET RELATION TO &lcTmpTrnDt..cBatchNo + &lcTmpTrnDt..cTranNo ;
             INTO &lcTmpTrnHd
SET RELATION TO &lcTmpTrnDt..cBatchNo + &lcTmpTrnDt..cTranNo + ;
                STR(&lcTmpTrnDt..NENTRYNO,4) INTO (lcTmpAPDist) ADDITIVE
                
SELECT (lcTmpTrnHd)
SET RELATION TO &lcTmpTrnHd..cBatchNo INTO (lcTmpBatch) ADDITIVE
  
** Initialize Batch and Transaction numbers
lcBatKey = ""
lcTrnkey = ""

** Go through header unposted file
SELECT (lcTmpTrnDt)
lnRecCount = RECCOUNT()
lnCount = 0
*B610312,1 TMI 04/17/2013 [Start] initialize lcDynMsg variable
lcDynMsg = ''
*B610312,1 TMI 04/17/2013 [End  ] 
SCAN
  ** New batch
  IF lcBatKey <> &lcTmpTrnDt..cBatchno
    lcBatKey = &lcTmpTrnDt..cBatchno
      
    ** Get new bacth number

    *E300663,1 Change this line for the changes we have 
    *          made to (gfSequence) [Begin]
    *lcBatchNo = PADL(gfSequence("BATCH",1,gcPrnt_Cmp),6,'0')

    *- replace gcPrnt_Cmp with (lcGLCo) the variable that hold 
    *- the GL Linked company and 
    *- Start
    *lcBatchNo = gfSequence('CBATCHNO' , gcPrnt_Cmp)

    *- if it is uncomplete session we neednot to generate new batch
    *- the generated batch will be restored from the uncomplete sess.
    IF &lcTmpTrnDt..nStep < 1

      *B605290,1 MHM  12/30/2001 Change gfsequance to local one [Start]
      *lcBatchNo = gfSequence('CBATCHNO' , lcGLCo)
      lcBatchNo = lfSequence('CBATCHNO' , lcGLCo)
      *B605290,1 MHM   [End]

      llNoThing = lfUpdUnCmS("Open", lcCurProc)
    ENDIF
    
    
    *E300663,1 Change this line [End]      
    
    ** Update batch master file      
 
    IF &lcTmpBatch..nStep < 1
      SELECT (lcTmpBatch)
      SCATTER MEMVAR MEMO
      m.cBatchno  = lcBatchNo
      m.nBatCnTot = 0
      m.nBatotDr  = 0
      m.nBatotCr  = 0
      
      *- Useing GLBATCH file with its aliase name
      *- which is created in the calling program SMGLREL
      *- Start
      *SELECT GLBATCH
      SELECT (lcBatchAl)
      
      APPEND BLANK
      GATHER MEMVAR MEMO
      *E303295 TMI 11/18/2012 [Start] 
      *=gfAdd_Info()
      =gfAdd_Info(lcBatchAl)      
      *E303295 TMI 11/18/2012 [End  ] 
      =RLOCK()
      UNLOCK
      
      SELECT (lcTmpBatch)
      REPLACE nStep WITH 1
      =RLOCK()
      UNLOCK
    ENDIF      &&IF &lcTmpBatch..nStep < 1
    
    ** Initialize batch Debit & Credit totals      
    lnBDrTot = 0
    lnBCrTot = 0
  ENDIF  
    
  ** New transaction
  IF lcTrnKey <> &lcTmpTrnDt..cTranno
    lcTrnKey = &lcTmpTrnDt..cTranno
      
    ** Get new transaction number      

    *E300663,1 Change this line for the changes we have 
    *          made to (gfSequence) [Begin]
    *lcTranNo = PADL(gfSequence('TRANSACT',1,gcPrnt_Cmp),8,'0')

    *- replace gcPrnt_Cmp with (lcGLCo) the variable that hold 
    *- the GL Linked company
    *- Start
    *lcTranNo = gfSequence('CTRANNO' , gcPrnt_Cmp)
    *- if it is uncomplete session we neednot to generate new tran.
    *- the generated tran. will be restored from the uncomplete sess.
    IF &lcTmpTrnHd..nStep < 1

      *B605290,1 MHM  12/30/2001 Change gfsequance to local one [Start]
      *lcTranNo = gfSequence('CTRANNO' , lcGLCo)
      lcTranNo = lfSequence('CTRANNO' , lcGLCo)
      *B605290,1 MHM   [End]

      llNoThing = lfUpdUnCmS("Open", lcCurProc)
    ENDIF   IF &lcTmpTrnHd..nStep < 1
    
    *E300663,1 Change this line [End]
    
    IF &lcTmpTrnHd..nStep < 1
      ** Update header unposted master file      
      SELECT (lcTmpTrnHd)
      SCATTER MEMVAR MEMO
      m.cBatchno  = lcBatchNo
      m.cTranno   = lcTranNo
      m.nTrnTotDr = 0
      m.nTrnTotCr = 0

      *- Useing GLTRNSHD file with its aliase name
      *- which is created in the calling program SMGLREL
      *- Start
      *SELECT GLTRNSHD
      SELECT(lcHTrnsAl)
      
      APPEND BLANK
      GATHER MEMVAR MEMO
      *E303295 TMI 11/18/2012 [Start] 
      *=gfAdd_Info()
      =gfAdd_Info(lcHTrnsAl)
      *E303295 TMI 11/18/2012 [End  ] 
      =RLOCK()
      UNLOCK
   
      SELECT (lcTmpTrnHd)
      REPLACE nStep WITH 1
      =RLOCK()
      UNLOCK

    ENDIF  &&IF &lcTmpTrnHd..nStep < 1
    
    ** Initialize transaction Debit & Credit totals      
    lnTDrTot = 0
    lnTCrTot = 0
  ENDIF  
  lcDynMsg = 'Batch No. : '+lcBatchNo +;
             SPACE(5)+'Transaction No. : '+lcTranNo
  lnCount = lnCount + 1
  =gfThermo(lnReccount,lnCount,'Releasing...',lcDynMsg)

  ** Accumulate batch and transaction totals    
  DO CASE
    CASE &lcTmpTrnDt..cDrorCr = 'D'
      ** Accumulate Transaction Total Debit
      lnTDrTot =  lnTDrTot + &lcTmpTrnDt..nAmount
      ** Accumulate Batch Total Debit     
      lnBDrTot =  lnBDrTot + &lcTmpTrnDt..nAmount
    CASE &lcTmpTrnDt..cDrorCr = 'C'
      ** Accumulate Transaction Total Credit   
      lnTCrTot =  lnTCrTot + &lcTmpTrnDt..nAmount
      ** Accumulate Batch Total Credit          
      lnBCrTot =  lnBCrTot + &lcTmpTrnDt..nAmount
  ENDCASE    
    
  ** Update detailed unposted master file    
  SELECT (lcTmpTrnDt)
  IF &lcTmpTrnDt..nStep < 1
    SCATTER MEMVAR MEMO
    m.cBatchno = lcBatchNo
    m.cTranno  = lcTranNo
    
    *- Useing GLTRNSDT file with its aliase name
    *- which is created in the calling program SMGLREL
    *- Start
    *SELECT GLTRNSDT
    SELECT(lcDTrnsAl)
    
    APPEND BLANK
    GATHER MEMVAR MEMO
    *E303295 TMI 11/18/2012 [Start] 
    *=gfAdd_Info()
    =gfAdd_Info(lcDTrnsAl)
    *E303295 TMI 11/18/2012 [End  ] 
    =RLOCK()
    UNLOCK
    
    SELECT(lcTmpTrnDt)
    REPLACE nStep WITH 1
    =RLOCK()
    UNLOCK
  ENDIF    &&IF &lcTmpTrnDt..nStep < 1
 
  ** Update header unposted file with transaction totals    

  *- Useing GLTRNSHD file with its aliase name
  *- which is created in the calling program SMGLREL
  *- Start
  *SELECT GLTRNSHD
  SELECT(lcHTrnsAl)
  
  REPLACE nTrnTotDr WITH lnTDrTot, ;
          nTrnTotCr WITH lnTCrTot
  =RLOCK()
  UNLOCK
    
  ** Update batch file with batch totals    

  *- Useing GLBATCH file with its aliase name
  *- which is created in the calling program SMGLREL
  *- Start
  *SELECT GLBATCH
  SELECT (lcBatchAl)
  
  REPLACE nBatCnTot WITH lnBDrTot, ;
          nBatotDr  WITH lnBDrTot, ;
          nBatotCr  WITH lnBCrTot
  =RLOCK()
  UNLOCK
          
  ** Update Distribution file with Batch Number, Transaction Number and 
  ** Entry Numebr mark Posted field.
  SELECT (lcTmpAPDist)
  SCAN WHILE cBatchNo+cTrnsLedn+STR(nEntryNo,4) = &lcTmpTrnDt..cBatchno+;
             &lcTmpTrnDt..cTranno+STR(&lcTmpTrnDt..nEntryNo,4)
    GOTO &lcTmpAPDist..NRECNO IN APDIST
    *- Selecting the file befor update it 
    *- Start
    SELECT APDIST
    
    IF &lcTmpAPDist..nStep < 1
      REPLACE APDIST.CBATCHNO  WITH lcBatchNo ,;
              APDIST.CTRNSLEDN WITH lcTranNo  ,;
              APDIST.LAPDPOST  WITH .T.       ,;
              APDIST.NENTRYNO  WITH &lcTmpAPDist..NENTRYNO
      *- (Begin) Update date, period, and year.
      *- 
      REPLACE dApdTrDat  WITH &lcTmpAPDist..dApdTrDat,;
              cFisFYear  WITH &lcTmpAPDist..cFisFYear,;
              cFsPprdId  WITH &lcTmpAPDist..cFsPprdId
                    
      =RLOCK()
      UNLOCK
  
      SELECT(lcTmpAPDist)
      REPLACE nStep WITH 1            
      =RLOCK()
      UNLOCK
    ENDIF    &&&lcTmpAPDist..nStep < 1
  ENDSCAN
ENDSCAN

*IF WEXIST('gwdThermo')
  =gfThermo(lnReccount,lnReccount,'Releasing...',lcDynMsg)
*ENDIF  


*!**************************************************************************
*!
*!      Function : lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel
CLEAR READ

*!**************************************************************************
*!
*!      Procedure : lpRelMethod
*!
*!**************************************************************************
*
PROCEDURE lpRelMethod
*B607211,1 KHM 05/29/2003 (Begin) Adding a new variable to be used when indexing the file
*B607211,1                because it contains date fields and it should be converted to DTOS.
*PRIVATE lcOrderBy
PRIVATE lcOrderBy, lcIndxBy
*B607211,1 KHM 05/29/2003 (End)

*- Dont create apdist file if uncolplete session is dedicted
*- Start
*lcTmpAPDist = gfTempName()  && Create the Temp. File Name to hold the distribution lines.
IF !llUnComFnd AND lcCurProc < '5'
  lcTmpAPDist = gfTempName()  && Create the Temp. File Name to hold the distribution lines.
ENDIF

lcRelOrExp  = "CFISFYEAR+CFSPPRDID"
lcOrderBy   = "CFISFYEAR,CFSPPRDID"

*B607211,1 KHM 05/29/2003 (Begin) Added to be use in the index on command instead of lcOrderBy
*B607211,1                in order to use DTOS in the date fields
lcIndxBy    = "CFISFYEAR,CFSPPRDID"
*B607211,1 KHM 05/29/2003 (End)

DO CASE
  CASE APSETUP.NAPSRELM = 1
    ** Order by Transaction Number
    *B601497,1 M.H Begin.
    *lcRelOrExp = lcRelOrExp + "+CAPDREF"
    *lcOrderBy  = lcOrderBy +  ",CAPDREF"
    lcRelOrExp = lcRelOrExp + "+CVENDCODE+CAPDREF+CAPDTRTYP"
    lcOrderBy  = lcOrderBy +  ",CVENDCODE,CAPDREF,CAPDTRTYP"

    *B607211,1 KHM 05/29/2003 (Begin) Added to be use in the index on command instead of lcOrderBy
    *B607211,1                in order to use DTOS in the date fields
    lcIndxBy  = lcIndxBy +  ",CVENDCODE,CAPDREF,CAPDTRTYP"
    *B607211,1 KHM 05/29/2003 (End)
    
    *B601497,1 M.H End.
    *C100713,1 M.H Begin.
    *--Ramy
    lcInfMsg = "IIF(&lcTmpApDist..CAPDTRTYP='A','Vendor: '+&lcTmpApDist..CVENDCODE+', Apply DM: '+&lcTmpApDist..CINVNO,"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='B','Bank adjustment: '+&lcTmpApDist..CAPDREF,"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='H','Vendor: '+&lcTmpApDist..CVENDCODE+', Cash payment: '+&lcTmpApDist..CAPDREF,"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='I','Vendor: '+&lcTmpApDist..CVENDCODE+', Invoice: '+&lcTmpApDist..CAPDREF,"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='N','Vendor: '+&lcTmpApDist..CVENDCODE+', Non check: '+&lcTmpApDist..CAPDREF,"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='P','Vendor: '+&lcTmpApDist..CVENDCODE+', Print check: '+&lcTmpApDist..CAPDREF,'Vendor: '+&lcTmpApDist..CVENDCODE+', Manual check: '+&lcTmpApDist..CAPDREF))))))"



    *C100713,1 M.H End.

  CASE APSETUP.NAPSRELM = 2
    ** Order by Vendor
    lcRelOrExp = lcRelOrExp + "+CVENDCODE"
    lcOrderBy  = lcOrderBy  + ",CVENDCODE"

    *B607211,1 KHM 05/29/2003 (Begin) Added to be use in the index on command instead of lcOrderBy
    *B607211,1                in order to use DTOS in the date fields
    lcIndxBy  = lcIndxBy  + ",CVENDCODE"
    *B607211,1 KHM 05/29/2003 (End)
    
    *C100713,1 M.H Begin.
    lcInfMsg = "IIF(&lcTmpApDist..CAPDTRTYP='B','Bank adjustment','Vendor: '+&lcTmpApDist..CVENDCODE)"
    *C100713,1 M.H End.

  CASE APSETUP.NAPSRELM = 3
    ** Order by Transaction Date
    lcRelOrExp = lcRelOrExp + "+DTOC(DAPDTRDAT)"
    lcOrderBy  = lcOrderBy  + ",DAPDTRDAT"

    *B607211,1 KHM 05/29/2003 (Begin) Added to be use in the index on command instead of lcOrderBy
    *B607211,1                in order to use DTOS in the date fields
    lcIndxBy  = lcIndxBy  + ",DTOS(DAPDTRDAT)"
    *B607211,1 KHM 05/29/2003 (End)

    *C100713,1 M.H Begin.
    lcInfMsg = "'Date: '+DTOC(&lcTmpApDist..DAPDTRDAT)"
    *C100713,1 M.H End.

  CASE APSETUP.NAPSRELM = 4
    ** Order by Session number
    lcRelOrExp = lcRelOrExp + "+CAPSESSNO"
    lcOrderBy  = lcOrderBy  + ",CAPSESSNO"

    *B607211,1 KHM 05/29/2003 (Begin) Added to be use in the index on command instead of lcOrderBy
    *B607211,1                in order to use DTOS in the date fields
    lcIndxBy  = lcIndxBy  + ",CAPSESSNO"
    *B607211,1 KHM 05/29/2003 (End)

    *C100713,1 M.H Begin.
    lcInfMsg = "'Session: '+&lcTmpApDist..CAPSESSNO"
    *C100713,1 M.H End.

  CASE APSETUP.NAPSRELM = 5
    ** Order by Transaction Type
    lcRelOrExp = lcRelOrExp + "+CAPDTRTYP"
    lcOrderBy  = lcOrderBy  + ",CAPDTRTYP"

    *B607211,1 KHM 05/29/2003 (Begin) Added to be use in the index on command instead of lcOrderBy
    *B607211,1                in order to use DTOS in the date fields
    lcIndxBy  = lcIndxBy  + ",CAPDTRTYP"
    *B607211,1 KHM 05/29/2003 (End)

    *C100713,1 M.H Begin.
    lcInfMsg = "IIF(&lcTmpApDist..CAPDTRTYP='A','DM application',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='B','Bank adjustment',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='H','Cash payment',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='I','Invoice',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='N','Non check payment',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='P','Printed check','Manual check'))))))"
    *C100713,1 M.H End.

  CASE APSETUP.NAPSRELM = 6
    ** Order by  Session number + Transaction Date
    lcRelOrExp = lcRelOrExp + "+CAPSESSNO+DTOC(DAPDTRDAT)"
    lcOrderBy  = lcOrderBy  + ",CAPSESSNO,DAPDTRDAT"

    *B607211,1 KHM 05/29/2003 (Begin) Added to be use in the index on command instead of lcOrderBy
    *B607211,1                in order to use DTOS in the date fields
    lcIndxBy  = lcIndxBy  + ",CAPSESSNO,DTOS(DAPDTRDAT)"
    *B607211,1 KHM 05/29/2003 (End)

    *C100713,1 M.H Begin.
    lcInfMsg = "'Session: '+&lcTmpApDist..CAPSESSNO+', Date: '+DTOC(&lcTmpApDist..DAPDTRDAT)"
    *C100713,1 M.H End.

  CASE APSETUP.NAPSRELM = 7
    ** Order by  Transaction Type + Transaction Date
    lcRelOrExp = lcRelOrExp + "+CAPDTRTYP+DTOC(DAPDTRDAT)"
    lcOrderBy  = lcOrderBy  + ",CAPDTRTYP,DAPDTRDAT"   

    *B607211,1 KHM 05/29/2003 (Begin) Added to be use in the index on command instead of lcOrderBy
    *B607211,1                in order to use DTOS in the date fields
    lcIndxBy  = lcIndxBy  + ",CAPDTRTYP,DTOS(DAPDTRDAT)"


    lcInfMsg = "IIF(&lcTmpApDist..CAPDTRTYP='A','DM application',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='B','Bank adjustment',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='H','Cash payment',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='I','Invoice',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='N','Non check payment',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='P','Printed check','Manual check'))))))+', Date: '+DTOC(&lcTmpApDist..DAPDTRDAT)"


  CASE APSETUP.NAPSRELM = 8
    ** Order by Transaction Date + Vendor
    lcRelOrExp = lcRelOrExp + "+DTOC(DAPDTRDAT)+CVENDCODE"
    lcOrderBy  = lcOrderBy  + ",DAPDTRDAT,CVENDCODE"   

    *B607211,1 KHM 05/29/2003 (Begin) Added to be use in the index on command instead of lcOrderBy
    *B607211,1                in order to use DTOS in the date fields
    lcIndxBy  = lcIndxBy  + ",DTOS(DAPDTRDAT),CVENDCODE"   


    lcInfMsg = "'Date: '+DTOC(&lcTmpApDist..DAPDTRDAT)+"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='B',', Bank adjustment',', Vendor: '+&lcTmpApDist..CVENDCODE)"    

  CASE APSETUP.NAPSRELM = 9
    ** Order by Transaction Type + Vendor
    lcRelOrExp = lcRelOrExp + "+CAPDTRTYP+CVENDCODE"
    lcOrderBy  = lcOrderBy  + ",CAPDTRTYP,CVENDCODE"   

    *B607211,1 KHM 05/29/2003 (Begin) Added to be use in the index on command instead of lcOrderBy
    *B607211,1                in order to use DTOS in the date fields
    lcIndxBy  = lcIndxBy  + ",CAPDTRTYP,CVENDCODE"

    lcInfMsg = "IIF(&lcTmpApDist..CAPDTRTYP='A','Apply DM for vendor ',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='B','Bank adjustment',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='H','Cash payment for vendor ',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='I','Invoice for vendor ',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='N','Non check payment for vendor ',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='P','Printed check for vendor ','Manual check for vendor ')))))) +&lcTmpApDist..CVENDCODE"

  CASE APSETUP.NAPSRELM = 10
    ** Order by Session number + Vendor
    lcRelOrExp = lcRelOrExp + "+CAPSESSNO+CVENDCODE"
    lcOrderBy  = lcOrderBy  + ",CAPSESSNO,CVENDCODE"   

    *B607211,1 KHM 05/29/2003 (Begin) Added to be use in the index on command instead of lcOrderBy
    *B607211,1                in order to use DTOS in the date fields
    lcIndxBy  = lcIndxBy  + ",CAPSESSNO,CVENDCODE"

    lcInfMsg = "'Session: '+&lcTmpApDist..CAPSESSNO+"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='B',', Bank adjustment', ', Vendor: '+&lcTmpApDist..CVENDCODE)"

  CASE APSETUP.NAPSRELM = 11
    ** Order by Transaction Session + Date + Vendor
    lcRelOrExp = lcRelOrExp + "+CAPSESSNO+DTOC(DAPDTRDAT)+CVENDCODE"
    lcOrderBy  = lcOrderBy  + ",CAPSESSNO,DAPDTRDAT,CVENDCODE"   

    lcIndxBy  = lcIndxBy  + ",CAPSESSNO,DTOS(DAPDTRDAT),CVENDCODE"   

    lcInfMsg = "'Sess:'+&lcTmpApDist..CAPSESSNO+',On:'+DTOC(&lcTmpApDist..DAPDTRDAT)+"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='B',',Bank adjust.',',Vnd:'+&lcTmpApDist..CVENDCODE)"

  CASE APSETUP.NAPSRELM = 12
    ** Order by Transaction  Transaction Type + Date + Vendor
    lcRelOrExp = lcRelOrExp + "+CAPDTRTYP+DTOC(DAPDTRDAT)+CVENDCODE"
    lcOrderBy  = lcOrderBy  + ",CAPDTRTYP,DAPDTRDAT,CVENDCODE"  

    lcIndxBy  = lcIndxBy  + ",CAPDTRTYP,DTOS(DAPDTRDAT),CVENDCODE"  

    lcInfMsg = "IIF(&lcTmpApDist..CAPDTRTYP='A','Apply DM.',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='B','Bank adjustment',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='H','Cash payment',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='I','Invoice',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='N','Non check pay',"+;
               "IIF(&lcTmpApDist..CAPDTRTYP='P','Printed check','Manual check'))))))"+;
               "+',On:'+DTOC(&lcTmpApDist..DAPDTRDAT)+"+;
               "IIF(&lcTmpApDist..CAPDTRTYP<>'B',',Vnd:'+&lcTmpApDist..CVENDCODE,'')"
ENDCASE  

** Order by Account ID + Account Code
lcOrderBy = lcOrderBy + ",CAPDACTID,CAPDGLACT"

lcIndxBy = lcIndxBy + ",CAPDACTID,CAPDGLACT"

** Get the Accounts to be summarized 
IF APSETUP.LAPSSAP
  ** AP Accounts
  lcSumAccnt = "A"
ENDIF 

IF APSETUP.LAPSSDIST           
  ** Distribution Accounts
  lcSumAccnt = lcSumAccnt + "D"
ENDIF

IF APSETUP.LAPSSDISC
  ** Discount Accounts
  lcSumAccnt = lcSumAccnt + "S"
ENDIF  

IF APSETUP.LAPSSADJ            
  ** Adjustment Accounts
  lcSumAccnt = lcSumAccnt + "J"
ENDIF

IF APSETUP.LAPSSCHK
  ** Checking Accounts
  lcSumAccnt = lcSumAccnt + "C"
ENDIF  

** Close any active index on APDIST
SELECT APDIST
SET ORDER TO

*- Dont create apdist file if uncolplete session is dedicted
IF !llUnComFnd AND lcCurProc < '5'
  SELECT CVENDCODE,CINVNO,DAPDTRDAT,LAPDPOST,CAPDSTAT ,;
         NCURRUNIT,CAPDREF,CSTUBCHK,CAPDGLACT,CAPDACTID,;
         CBATCHNO,CTRNSLEDN,CFISFYEAR,CFSPPRDID,CAPSESSNO,;
         CTAXCODE,CAPDTRTYP,NAPDLINNO,CBNKCODE,CCHKACCT,;
         NENTRYNO,CCURRCODE,NEXRATE,CADD_USER,DADD_DATE,;
         CADD_TIME,LLOK_STAT,CLOK_USER,DLOK_DATE,CLOK_TIME,;
         NEQVAMNT AS 'NAPDAMNT',RECNO() AS nRecNo,0 AS nStep ;
    FROM APDIST                 ;
    WHERE !LAPDPOST .AND. !EMPTY(CAPDGLACT) ;
   ORDER BY &lcOrderBY                     ;
    INTO DBF &gcWorkDir.&lcTmpAPDist
  SELECT (lcTmpAPDist)
  USE 
  *B610362,1 TMI 06/09/2013 [Start] open the file exclusively to create the index 
  *USE (gcWorkDir+lcTmpAPDist) 
  USE (gcWorkDir+lcTmpAPDist) EXCLUSIVE
  *B610362,1 TMI 06/09/2013 [End  ] 
  
  *- (Begin) Create two temp indexes. One is new and the other will replace ORDER BY to
  *-         switch between both.
  *-         The new index based on cApdTrTyp+cInvNo+cApdRef to get the tran date for the current 
  *-         transction if it has any line having Tran date otherwise browse a screen for
  *-         the user to enable him to enter a date for the current transaction and upfdate
  *-         all the lines ot it by this entedred date.
  INDEX ON cApdTrTyp+cInvNo+cVendCode TAG tran_no OF &gcWorkDir.&lcTmpAPDist COMPACT ADDITIVE

  lcIndxBy = STRTRAN(lcIndxBy,',','+')
  INDEX ON &lcIndxBy TAG lcIndxBy OF &gcWorkDir.&lcTmpAPDist COMPACT ADDITIVE
  
  
  lcFiles = "lcTmpBatch,"  + lcTmpBatch  + "," + ORDER(lcTmpBatch)  + ";" + ;
            "lcTmpTrnHd,"  + lcTmpTrnHd  + "," + ORDER(lcTmpTrnHd)  + ";" + ;
            "lcTmpTrnDt,"  + lcTmpTrnDt  + "," + ORDER(lcTmpTrnDt)  + ";" + ;
            "lcTmpAPDist," + lcTmpAPDist + "," + ORDER(lcTmpAPDist) + ";"
  lcCurProc = '5'
  llNoThing = lfUpdUnCmS("Open", lcCurProc)
ENDIF

** Setup A.P. temporary file relations into Fiscal header, Fiscal Detail
** and Chart of Account files

SELECT (lcTmpAPDist)
SET RELATION TO CFISFYEAR INTO FISHD

SET RELATION TO CFISFYEAR+CFSPPRDID INTO (lcFsPrdAl) ADDITIVE 


SET RELATION TO CAPDGLACT INTO (lcLinkChar) ADDITIVE 
*-- using lclinkchar as a variable (AAMER) End
RETURN(.T.)

*!**************************************************************************
*!
*!      Function: lfwSelSusp
*!
*!**************************************************************************
*
FUNCTION lfwSelSusp

lcOldSusp = lcSelSusp

*!**************************************************************************
*!
*!      Function: lfvSelSusp
*!
*!**************************************************************************
FUNCTION lfvSelSusp
PARAMETERS loFormSet,loFld

*B610170,1 TMI 12/06/2012 [Start] 
llBrowse = loFld.Selectedfrombrowse
*B610170,1 TMI 12/06/2012 [End  ] 

*- Call a global function to browse Accounts
*B610791,1 TMI 08/04/2014 12:31 [Start] pass the loFld lfApAcs fn.
*lcSelSusp = IIF(lfApAcs('',llBrowse),lcSelSusp,lcOldSusp)
lcSelSusp = IIF(lfApAcs(loFld,llBrowse),lcSelSusp,lcOldSusp)
*B610791,1 TMI 08/04/2014 12:31 [End  ] 
llBrowse  = .F.
RETURN
*- (End)

lcSuspen = ALLTRIM(lcSelSusp)+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")

IF llBrowse .OR. (!EMPTY(lcSelSusp) .AND. lcOldSusp <> lcSelSusp)
  *-- using lclinkchar as a variable (AAMER) Start
  *SELECT lcLinkChar
  SELECT (lcLinkChar)
  *-- using lclinkchar as a variable (AAMER) End
  lcSavOrder = SET('ORDER')
  SET ORDER TO ACCTCODE
  LOCATE FOR CACCTCODE = lcSuspen .AND. cStandard = 'Y'
  IF !FOUND() .OR. ATC('?',lcSelSusp) > 0 .OR. llBrowse
    LOCATE FOR cStandard = 'Y'
    IF FOUND()
      DIMENSION laTemp[1]
      laTemp[1] = ''
      lcSavBrFld=lcBrfields
      lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description',;
                  CTYPECODE :H= 'Type Code',;
                  CSTANDARD :H= 'Standard'"

      lcFile_Ttl = 'Chart of Account'

      SET ORDER TO TAG STDACTPOS

      =gfbrows(["Y"FOR VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')

      lcFile_Ttl=lcSavTitle
      lcBrfields=lcSavBrFld

      SET ORDER TO

      IF !EMPTY(laTemp[1])
        lcSelSusp = SUBSTR(laTemp[1],1,lnFrsSegSz)
        lcSuspen  = ALLTRIM(lcSelSusp)+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
      ELSE
        lcSelSusp = lcOldSusp
        _CUROBJ=OBJNUM(lcSelSusp)
      ENDIF   
    ELSE
      =gfModalGen("TRM00052B00000","Dialog")        
      lcSelSusp = SPACE(lnFrsSegSz)
      _CUROBJ=OBJNUM(lcSelSusp)
    ENDIF  
  ENDIF
  SET ORDER TO &lcSavOrder
ENDIF

lcSelSusp = SUBSTR(lcSelSusp,1,lnFrsSegSz)

llBrowse   = .F.

SHOW GET lcSelSusp ENABLE

*!**************************************************************************
*!
*!      Function: lfBuildPic
*!
*!**************************************************************************
*
FUNCTION lfBuildPic

*- Using ACCOD File with its alias name which is used by alias name 
*-  in the calling program SMGLREL
*- Start lcAcCodAl
RETURN "X"+SUBSTR(STRTRAN(&lcAcCodAl..Cacsmask,'#','9'),2)

*!**************************************************************************
*!
*!      Function: lfvOkSusp
*!
*!**************************************************************************
*
FUNCTION lfvOkSusp

*- Edit Suspense Account Full Length
lcSuspense = lcSelSusp

*!******************************************************************************************
*! Name      : lfGetPerd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/11/2000
*! Purpose   : Get the period and year and fill them for the remaining records of the transaction.
*!******************************************************************************************
*! Refer to  : -
*!******************************************************************************************
FUNCTION lfGetPerd

*-- This function will return .F. if nothing for it to do.
IF !EMPTY(dApdTrDat) AND !EMPTY(cFisFYear) AND !EMPTY(cFsPprdId)
  RETURN .F.
ENDIF
PRIVATE lcAlias,lnRecNo
PRIVATE lcAlias,lnRecNo,llUserDate,lcTranTyp
lcAlias = ALIAS()

*--For the screen
lcTranTyp  = IIF(cApdTrtyp= 'I','Invoice',IIF(cApdTrtyp= 'P','Printed check',IIF(cApdTrtyp= 'M','Manual check',IIF(cApdTrtyp= 'N','Non check payment',IIF(cApdTrtyp= 'H','Cash payment',IIF(cApdTrtyp= 'A','Apply Debit','Bank adjustment'))))))
lcTransNo  = ALLTRIM(cInvNo)
llUserDate = .F.
lcVendor = cVendCode
*--This varialbe will show the show Vendor field.
llApShow = .T.

IF EMPTY(dApdTrDat) OR EMPTY(cFisFYear) OR EMPTY(cFsPprdId)
  *-- Save the current record.
  *----------------------------------------------------------------------------------------------
  *-- Note : The program will never enter here unless date or year or period is empty. This means
  *--      : that it will enter here for the most top records in the file as it's indexed on Year+Period.
  *--      : So, after getting the valid date,year, and period and update the file with them the place of the
  *--      : record being updated will change. Therefor if this function change anything in the temp file
  *--      : it will return .T. to LOOP to get another record with date or year or period is empty. As for
  *--      : the record previously updated it will be manipulated when its place in the DO WHILE comes to it according
  *:       : to the index.
  *-----------------------------------------------------------------------------------------------
  *--Get recno of the next transaction record as it will be the current record if the file chnaged.
  *--Key for current record on ORDERBY index.
  lcKey = EVAL(KEY())
  *--Key for current transaction.
  lcTranKey = cApdTrTyp+cInvNo+cVendCode
  *--We look here for any record having date or year or period empty for any other transaction to get it's rec no to get it
  *--back if we update all the lines of the current transaction with the new date,year and period.
  LOCATE FOR (EMPTY(dApdTrDat) OR EMPTY(cFisFYear) OR EMPTY(cFsPprdId)) AND (cApdTrTyp+cInvNo+cVendCode <> lcTranKey)
  IF !FOUND()
    *--No records having empty date or year or period.
    =SEEK(lcKey)
    *--Get the next transaction rec no.
    LOCATE REST FOR cApdTrTyp+cInvNo+cVendCode <> lcTranKey
    IF EOF()
       =SEEK(lcKey)
    ENDIF   
  ENDIF
  lnRecNo = RECNO()
  =SEEK(lcKey)
  IF EMPTY(dApdTrDat)
    *--Is there any date for this tranaction?
    *-- Initialize date.
    ldTranDate = dApdTrDat
    *--Change order to tran_no
    SET ORDER TO TAG tran_no
    =SEEK(lcTranKey)
    SCAN WHILE  cApdTrTyp+cInvNo+cVendCode = lcTranKey FOR !EMPTY(dApdTrDat)
      ldTranDate = dApdTrDat
      IF !EMPTY(ldTranDate)
        EXIT
      ENDIF
    ENDSCAN
    =SEEK(lcTranKey)
    IF EMPTY(ldTranDate)
       *B610170,1 TMI 12/06/2012 [Start] replace the correct extension
       *DO (gcScrDir + gcWinAppl + '\SMGLDATE.SPR')
       
       *B610430,1 HES fix bug of not found variable while releasing GL [Start]
       lcAlias = ALIAS()
       *B610430,1 HES fix bug of not found variable while releasing GL [End  ]
       
       DO FORM (gcScrDir + gcWinAppl + '\SMGLDATE.SCX')
       
       *B610430,1 HES fix bug of not found variable while releasing GL [start]
        SELECT (lcAlias)
       *B610430,1 HES fix bug of not found variable while releasing GL [end  ]       
       
       *B610170,1 TMI 12/06/2012 [End  ] 
       llUserDate = !EMPTY(ldTranDate)
       llApShow = .F.
    ENDIF
    *-- If the user left the date empty.
    IF EMPTY(ldTranDate)
      *-- The user didn't enter a date. Return and let the program does it it has always done.
      SET ORDER TO TAG lcIndxBy

      GO IIF(BETWEEN(lnRecNo,1,RECCOUNT(lcTmpAPDist)),lnRecNo,RECNO())
      IF !EMPTY(lcAlias)
        SELECT (lcAlias)
      ENDIF
      RETURN .F.
    ENDIF
  ENDIF
  *-- IF there is a transaction date, valid it.
  STORE '' TO lcYear,lcPrd
  ldTranDate = IIF(!EMPTY(dApdTrDat),dApdTrDat,ldTranDate)
  *-- If it's not valid.
  llValid = CheckPrd(ldTranDate,'lcYear','lcPrd',capdtrtyp , .T.)
  *--If original date or user date entered is invalid.
  IF !llValid AND !llUserDate
    *B610170,1 TMI 12/06/2012 [Start] replace the correct extension
    *DO (gcScrDir + gcWinAppl + '\SMGLDATE.SPR')
    
    *B610430,1 HES fix bug of not found variable while releasing GL [start]	
    lcAlias = ALIAS()
    *B610430,1 HES fix bug of not found variable while releasing GL [end  ]    
    
    DO FORM (gcScrDir + gcWinAppl + '\SMGLDATE.scx')
    
    *B610430,1 HES fix bug of not found variable while releasing GL [start]	
    SELECT (lcAlias)
    *B610430,1 HES fix bug of not found variable while releasing GL [end  ]    
    
    *B610170,1 TMI 12/06/2012 [End  ] 
    llApShow = .F.
  ENDIF
  *-- If the original date is valid or the user entered a valid date.
  IF llValid OR (!EMPTY(ldTranDate) AND CheckPrd(ldTranDate,'lcYear','lcPrd',capdtrtyp ))
    *-- It's a valid date and we got its year and period.
    *--Verify order tran_no
    SET ORDER TO TAG tran_no
    =SEEK(lcTranKey)
    REPLACE REST dApdTrDat  WITH ldTranDate,;
                 cFisFYear  WITH lcYear,;
                 cFsPprdId  WITH lcPrd;
            WHILE cApdTrTyp+cInvNo+cVendCode = lcTranKey
  ELSE
    RETURN .F.          
  ENDIF
  SET ORDER TO TAG lcIndxBy

  GO IIF(BETWEEN(lnRecNo,1,RECCOUNT(lcTmpAPDist)),lnRecNo,RECNO())
ENDIF
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF

*!*************************************************************
*! Name      : lfSequence                    
*! Developer : Mohamed Shokry(MHM)
*! Date      : 12/30/2001
*! Purpose   : To get new sequance number for any item modified 
*!           : from standerd function 
*!*************************************************************
*! Calls     :  GFADD_INFO()
*!              gfRltFld()
*!*************************************************************
*! Passed Parameters  : Sequance type
*!                      Company ID
*!                      Group ID
*!                      Division Code
*!                      Associated field
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :  lcData[1] = gfSequence('CINVOICE')
*!*************************************************************
*! Modifications
*!*************************************************************
*- This function exactly copy from gfSequence() with minar changes highlighted by MHM
FUNCTION lfSequence
PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision,lcField

PRIVATE lnRetVal,lcSavAlias,lcDataDir, lnOldGenNm,lcExtraStr,lcToFind,lcKeyExp
lcField    = IIF(TYPE("lcField")="C", ALLTRIM(UPPER(lcField)), SPACE(0))

lcSavAlias = SELECT(0)
lcSeqType  = UPPER(lcSeqType)
lcDataDir  = gcDataDir

*:*B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][Start]
gcAct_comp = oAriaApplication.ActiveCompanyID
*:*B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][End]

PRIVATE lcCmpCode
lcCmpCode = IIF(TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId), lcCompanyId , gcAct_Comp)
lcUnqPreFx = gfGetMemVar("M_UNQSTPRX",lcCmpCode)

IF TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId) AND lcCompanyId <> gcAct_Comp
  IF USED("sycComp")
    SELECT sycComp
    luSycComp = .F.
    ltSycComp = VAL(SYS(21))
    leSycComp = RECNO()
    SET ORDER TO TAG cComp_Id IN syccomp
  ELSE
    luSycComp = .T.
    *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [START]
*!*	    USE (gcSysHome+"syccomp") ORDER TAG cComp_Id IN 0
    USE (oAriaApplication.SysPath+"syccomp") ORDER TAG cComp_Id IN 0
    *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [END  ]
  ENDIF

  IF SEEK(lcCompanyId,'syccomp')
    lcDataDir = gfGetDataDir(ALLTRIM(syccomp.cCom_dDir))
  ENDIF

  IF luSycComp
    USE IN syccomp
  ELSE
    SET ORDER TO TAG ltSycComp IN syccomp
    IF BETWEEN(leSycComp,1,RECCOUNT('syccomp'))
      GOTO leSycComp IN 'syccomp'
    ENDIF
  ENDIF
ENDIF

lcGroupId  = IIF(TYPE('lcGroupId') ='C' , PADR(lcGroupId,3)  , SPACE(2))

lcDivision = IIF(TYPE('lcDivision')='C',ALLTRIM(lcDivision),SPACE(10))
lnRetVal   = 0

llDivOnSeq = gfgetMemvar('M_DIV_SEQ' , lcCompanyId) = 'Y'
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
  *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [START]
*!*	  USE &lcDataDir.SEQUENCE IN 0 ORDER TAG 'cSeq_Type'
  USE (lcDataDir+"SEQUENCE") IN 0 ORDER TAG 'cSeq_Type'
  *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [END  ]
ELSE
  SELECT SEQUENCE
  luSequence = .F.
  ltSequence = VAL(SYS(21))
  leSequence = RECNO()
  SET ORDER TO TAG Cseq_type IN SEQUENCE
ENDIF

IF !SEEK(PADR(lcSeqType,10)+lcGroupId,'SEQUENCE')
  IF !USED('sydflfld')
    luSydflfld = .T.
    *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [START]
*!*	    USE &gcSysHome.sydflfld ORDER TAG 'Cfld_name' IN 0 SHARED
    USE (oAriaApplication.SysPath+"sydflfld") ORDER TAG 'Cfld_name' IN 0 SHARED
    *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [END  ]
  ELSE
    SELECT Sydflfld
    luSydflfld = .F.
    ltSydflfld = VAL(SYS(21))
    leSydflfld = RECNO()
    SET ORDER TO TAG Cfld_name IN 'sydflfld'
  ENDIF
  *:*B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][Start]
*!*	  IF !USED('sydfield')
*!*	    luSydfield = .T.
*!*	    USE &gcSysHome.sydfield ORDER TAG 'Cfld_name' IN 0 SHARED
*!*	  ELSE
*!*	    SELECT Sydfield
*!*	    luSydfield = .F.
*!*	    ltSydfield = VAL(SYS(21))
*!*	    leSydfield  = RECNO()
*!*	    SET ORDER TO TAG Cfld_name IN 'sydfield'
*!*	  ENDIF
  IF !USED('Aydfield')
    luSydfield = .T.
    *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [START]
*!*	    USE &gcSysHome.sydfield ORDER TAG 'Cfld_name' IN 0 SHARED ALIAS Aydfield
    USE (oAriaApplication.SysPath+"sydfield") ORDER TAG 'Cfld_name' IN 0 SHARED ALIAS Aydfield AGAIN
    *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [END  ]
  ELSE
    SELECT Aydfield
    luSydfield = .F.
    ltSydfield = VAL(SYS(21))
    leSydfield  = RECNO()
    SET ORDER TO TAG Cfld_name IN 'Aydfield'
  ENDIF
  *:*B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][End]

  lcPropFld = IIF(EMPTY(lcField), lcSeqType, lcField)
  = SEEK(PADR(lcPropFld,10),'sydfield')
  SELECT sydflfld
  = SEEK(PADR(lcPropFld,10))
  LOCATE REST WHILE cFld_Name=PADR(lcPropFld,10) FOR lEnumerate
  

  lnDefSeq = sydflfld.nDef_Seq
  IF !EMPTY(lcGroupId) AND SEEK(PADR(lcSeqType,10),'SEQUENCE')
    SELECT SEQUENCE
    lnDefSeq = 0
    SCAN REST WHILE cseq_type+cseq_group = PADR(lcSeqType,10)
      lnDefSeq = MAX(lnDefSeq,nSeq_No)
    ENDSCAN
    lnDefSeq = (INT(lnDefSeq/50000)+1)*50000
  ENDIF
  
  *:*B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][Start]  
*!*	  INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth) ;
*!*	       VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfield.cData_Typ,;
*!*	       sydfield.nFld_Wdth)
       
  INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth) ;
       VALUES (lcSeqType,lnDefSeq,lcGroupId,Aydfield.cData_Typ,;
       Aydfield.nFld_Wdth)       
  *:*B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][End]       
  IF sydflfld.lEnumerate
    IF !USED('sydfiles')
      luSydfiles = .T.
      *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [START]
*!*	      USE &gcSysHome.sydfiles ORDER TAG 'Cfile_nam' IN 0 SHARED
      USE (oAriaApplication.SysPath+"sydfiles") ORDER TAG 'Cfile_nam' IN 0 SHARED
      *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [END  ]
    ELSE
      SELECT Sydfiles
      luSydfiles = .F.
      ltSydfiles = VAL(SYS(21))
      leSydfiles = RECNO()
      SET ORDER TO TAG Cfile_nam IN 'sydfiles'
    ENDIF
    =SEEK(sydflfld.cFile_Nam,'sydfiles')
    SELECT SEQUENCE
    REPLACE cFile_Nam WITH sydfiles.cFile_Nam ,;
            cFile_Tag WITH sydfiles.cFile_Tag
    IF luSydfiles
      USE IN Sydfiles
    ELSE
      SET ORDER TO TAG ltSydfiles IN Sydfiles
      IF BETWEEN(leSydfiles,1,RECCOUNT('Sydfiles'))
        GOTO leSydfiles IN 'Sydfiles'
      ENDIF
    ENDIF
  ENDIF
  IF luSydflfld
    USE IN Sydflfld
  ELSE
    SET ORDER TO TAG ltSydflfld IN Sydflfld
    IF BETWEEN(leSydflfld,1,RECCOUNT('Sydflfld'))
      GOTO leSydflfld IN 'Sydflfld'
    ENDIF
  ENDIF
  
  *:*B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][Start]    
*!*	  IF luSydfield
*!*	    USE IN Sydfield
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSydfield IN Sydfield
*!*	    IF BETWEEN(leSydfield,1,RECCOUNT('Sydfield'))
*!*	      GOTO leSydfield IN 'Sydfield'
*!*	    ENDIF
*!*	  ENDIF
  IF luSydfield
    USE IN Aydfield
  ELSE
    SET ORDER TO TAG ltSydfield IN Aydfield
    IF BETWEEN(leSydfield,1,RECCOUNT('Aydfield'))
      GOTO leSydfield IN 'Aydfield'
    ENDIF
  ENDIF
  *:*B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][End]
ENDIF
*- Added RLOCK Condition[Start]
   DO WHILE !RLOCK("SEQUENCE")
   ENDDO
  lnRetVal   = SEQUENCE.nSeq_No
*- Added RLOCK Condition[End]

lnRetLen = SEQUENCE.nFld_Wdth - LEN(lcUnqPreFx)
lnOldGenNm = SEQUENCE.nSeq_No
lcExtraStr = ''
IF !EMPTY(SEQUENCE.cSeq_Chr)
  *- get new Extra Char to added to sequance number [Begin]
  *lcExtraStr = SEQUENCE.cSeq_Chr
  lcExtraStr = lfExtraStr()
  *- get new Extra Char to added to sequance number [End]
ENDIF

IF !EMPTY(SEQUENCE.cFile_Nam) .AND. !EMPTY(SEQUENCE.cFile_Tag)
  lcSeqFile = ALLTRIM(SEQUENCE.cFile_Nam)
  lcSeqTag  = ALLTRIM(SEQUENCE.cFile_Tag)
  IF !USED(lcSeqFile)
    luSeqFile = .T.
    *-- we change this fuction from standerd for this part only to get child 
    *-- 
    IF TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId) AND lcCompanyId <> gcAct_Comp
      *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [START]
*!*	      USE &lcDataDir.&lcSeqFile AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED 
      USE (lcDataDir+lcSeqFile) AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED 
      *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [END  ]
    ELSE
      *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [START]
*!*	      USE &gcDataDir.&lcSeqFile AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED 
      USE (gcDataDir+lcSeqFile) AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED 
      *B610758,1 HES 06/24/2014 Fix the USE commands to avoid using macros [END  ]
    ENDIF  
  ELSE
    SELECT (lcSeqFile)
    luSeqFile = .F.
    ltSeqFile = VAL(SYS(21))
    leSeqFile = RECNO()
    SET ORDER TO TAG (lcSeqTag) IN (lcSeqFile)
  ENDIF
  SELECT (lcSeqFile)
  lcKeyField = SUBSTR(KEY(),1,AT('+'+lcSeqType,KEY())-1)
  DECLARE laVldEnt[1]

  IF !EMPTY(lcKeyField) .AND. gfGetVld(lcKeyField,@laVldEnt) > 0
    FOR lnCount = 1 TO ALEN(laVldEnt,1)

      lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
                                       ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
      DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcSeqFile)
        lnRetVal = lnRetVal + 1

        IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
          lcExtraStr = CHR(ASC(lcExtraStr)+1)
        ENDIF
        lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
                                         ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
      ENDDO
    ENDFOR
  ELSE  
    lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
                                     ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
    DO WHILE SEEK(lcKeyExp,lcSeqFile)
      lnRetVal = lnRetVal + 1
      IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
        lcExtraStr = CHR(ASC(lcExtraStr)+1)
      ENDIF
      lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
                                       ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
    ENDDO
  ENDIF  
  
  IF luSeqFile
    USE IN (lcSeqFile)
  ELSE
    SET ORDER TO TAG ltSeqFile IN (lcSeqFile)
    IF BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
      GOTO leSeqFile IN (lcSeqFile)
    ENDIF
  ENDIF
ENDIF
SELECT SEQUENCE
REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
=gfAdd_info('SEQUENCE')
*- Added RLOCK Condition[Start]

IF  nSeq_No = 0 .AND. lnOldGenNm <> 0
  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
ENDIF
IF !EMPTY(lcExtraStr)
  lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
ENDIF

UNLOCK
*- Added RLOCK Condition[End]
 lnRetVal = lcUnqPreFx + PADL(lnRetVal, lnRetLen, "0")

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



*!***************************************************************************************
*! Name      : lfSequence                    
*! Developer : Albert Raif (ALB)
*! Date      : 05/18/2003
*! Purpose   : To get new Extra Char to added to sequance number for any item modified 
*!           : from standerd function 
*!***************************************************************************************
*! Calls     :  
*!***************************************************************************************
*! Passed Parameters  : 
*!***************************************************************************************
*! Returns            : ............
*!***************************************************************************************
*! Example   :  
*!***************************************************************************************
*! Modifications
*!***************************************************************************************
*!- get new Extra Char to added to sequance number
FUNCTION lfExtraStr

PRIVATE lcChar , lnCharPos , lnI,lcExtra
lcExtra = ''
IF !(SEQUENCE.cSeq_Chr = CHR(0))
  IF MOD(ASC(SEQUENCE.cSeq_Chr),26) = 0
    lcChar = "Z"
    lnCharPos = ASC(SEQUENCE.cSeq_Chr)/26
  ELSE
    lcChar =  CHR(MOD(ASC(SEQUENCE.cSeq_Chr),26)+64)
    lnCharPos = INT(ASC(SEQUENCE.cSeq_Chr)/26)+1
  ENDIF  
  FOR lnI = 1 TO lnCharPos - 1
    lcExtra = lcExtra + "Z"
  ENDFOR
  lcExtra = lcExtra + lcChar
ELSE
lcChar=""
ENDIF
RETURN lcExtra