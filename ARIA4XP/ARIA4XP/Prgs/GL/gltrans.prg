*:************************************************************************
*:  Program File: ARIA4XP\PRGS\sy\lftbpos.prg
*:  Module      : General Ledger
*:  Desc.       : Single Transaction screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 05/27/2012
*:  Reference   : E303161,1 
*:************************************************************************
* Modifications
*B610167,1 TMI 12/05/2012 [T20121203.0010] assure that the reference and description fields in GLTRNSHD are saved correctly
*B610212,1 HIA 01/23/2013 when trying to post journal batch in Aria27 and Aria4, an error message appears [T20130117.0032]
*B610266,1 HIA 03/06/2013 Aria4xp - I am not able to add a comment to the GL Entries [T20130207.0022]
*B610276,1 TMI 03/21/2013 calculate the period and year based on the fiscal year [T20130311.0001] 
*B610358,1 HIA 06/06/2013 T20130604.0021 - When trying to post cash receipts after entering the first credit adjustment the information disappears.
*B610364,1 TMI 06/09/2013 remove the fix B610358 [T20130604.0021] 
*B610426,1 TMI 07/09/2013 fix a problem that the ctrnpyr is empty[T20130628.0006] 
*B610453,1 TMI 07/30/2013 Fix problem - Cannot save comments on GL [T20130715.0031]
**************************************************************************

*** Parameter lcBatchno used to pass batch number if the transaction screen
*** used from journal batches screen
*** lcVewMode used to keep currnent view mode if comming from same screen
*** lcEntOrTr used to check if transaction or entry is required
PARAMETERS pcBatchno,pcTrnHdRec,pcTrnDtRec,pcVewMode,pcEntOrTr

lcProg = JUSTSTEM(SYS(16))

*- Call the screen

lcRunScx = lfGetScx("GL\&lcProg..scx")
IF !EMPTY(pcBatchno)
  PRIVATE oScr
  DO FORM (lcRunScx) WITH pcBatchno,pcTrnHdRec,pcTrnDtRec,pcVewMode,pcEntOrTr NAME oScr NOSHOW 
  IF TYPE('oScr')='O' AND !ISNULL(oScr)
    oScr.Show(1)
  ENDIF
ELSE 
  DO FORM (lcRunScx) WITH pcBatchno,pcTrnHdRec,pcTrnDtRec,pcVewMode,pcEntOrTr
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

*!B610358,1 HIA 06/06/2013 T20130604.0021 - When trying to post cash receipts after entering the first credit adjustment the information disappears [Begin]
*B610364,1 TMI 06/09/2013 [Start] comment out
*lcPath = oAriaapplication.ApplicationHome
*SET PROCEDURE TO (lcPath+'GL\LFTBPOST.FXP')  ADDITIVE 
*B610364,1 TMI 06/09/2013 [End  ] 
*!B610358,1 HIA 06/06/2013 T20130604.0021 - When trying to post cash receipts after entering the first credit adjustment the information disappears [End]

IF !EMPTY(loFormSet.pcBatchno)
  loFormset.Ariaform1.pbPost.Visible = .F.
  loFormset.Ariaform1.pbEntries.Left = loFormset.Ariaform1.Width/2 - loFormset.Ariaform1.pbEntries.Width/2
  *E303161,4 TMI 10/07/2012 [Start] add a caption in case of modal open
    loFormset.Ariaform1.Caption = 'Transaction'    
  *E303161,4 TMI 10/07/2012 [End  ] 
ENDIF   

loFormSet.AddProperty('lcProgName','GLTRANS')
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
*E303161,4 TMI 07/11/2012 [Start] define browse title
loFormSet.lcFile_Ttl = LOOKUP(SYDFILES.CFILE_TTL,loFormset.lcBaseFile,SYDFILES.CFILE_NAM)
*E303161,4 TMI 07/11/2012 [End  ] 

*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile 
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "CBATCHNO+CTRANNO"
  .cBrowseIndexFields     = "CBATCHNO,CTRANNO"
  .cBrowseIndexName       = "BATCHTRN"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle 		  	    = loFormSet.lcFile_Ttl 
  .ariaBrFields.edtBrowseFields.Value = gfDbfField(.nWorkArea)
ENDWITH 

*- Define needed variables.
*=lfDefineVars(loFormSet)
*!*	************************************************************
*!*	*! Name      : lfDefineVars
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      : 05/27/2012
*!*	*! Purpose   : Define Variables 
*!*	************************************************************
*!*	FUNCTION fDefineVars
*!*	PARAMETERS loFormSet

*EXTERNAL ARRAY laData,laKeyField,laDefProc,laScrMode,laCtrStat
*** Check if comming from batch program or not
*** Array decleration 
lcScFields = 'CBATCHNO,CTRANNO,DTRNPDATE,CTRNDESC,CTRNREFER,CTRNPYR,CTRNPPRD,CTRNSTAT,NTRNTOTDR,NTRNTOTCR,CSRCJRNL,'+;
             'DTRNREVDT,CTRNREVYR,CTRNREVPR,CTRNREVER,CAUTCODE,CAUTTYPE,CSRCMODUL,CSTANDARD,CTRNTYPE,CCOMP_ID'
loFormSet.AddProperty('lcScFields',lcScFields)
lcLen = ALLTRIM(STR(OCCURS(',' , lcScFields )+1))
loFormSet.AddProperty('laData[&lcLen]','')

loFormSet.AddProperty('laKeyField[2,4]')       && Array to hold key exprtion  

loFormSet.AddProperty('laTStatus[6,1]', ' ')       && Array to hold transaction status
WITH loFormSet
.laTStatus[1,1] = "Empty"
.laTStatus[2,1] = "Out of Balance"
.laTStatus[3,1] = "Unposted"
.laTStatus[4,1] = "Hold"
.laTStatus[5,1] = "Posted"
.laTStatus[6,1] = "Approved"
ENDWITH 

loFormSet.AddProperty('laSourcJor[1,2]')       && Array to hold source journal 
loFormSet.AddProperty('laWndObj[3,3]')       && Have screen name & first & last obj. for each screen.
loFormSet.AddProperty('laFisYear[3,1]')       && Array to Hold previous,current,next year
*loFormSet.AddProperty('laFileStru[1]')

*** This array will hold names of the windows involved in this session 
*** on the name of first and last object in each
WITH loFormSet
.laKeyField[1,1] = 'loFormSet.laData[1]'
.laKeyField[1,2] =.F.
.laKeyField[1,3] = 'BATCHTRN'
.laKeyField[1,4] = 1
.laKeyField[2,1] = 'loFormSet.laData[2]'
.laKeyField[2,2] =.T.
.laKeyField[2,3] = 'BATCHTRN'
.laKeyField[2,4] = 2
ENDWITH 


*** Variables Decleration
loFormSet.AddProperty('lcDrOrCr',     ' '  )             && To Determine Namount debit or Credit 

loFormSet.AddProperty('lcAutDes', SPACE(40))        && Description for automatic template      
loFormSet.AddProperty('lcHoldStat','DISABLE')         && Status for Hold push button 
loFormSet.AddProperty('lcAcctCode', REPLICATE("0",loFormSet.lnAcsSegSz))   && Default for Account code  
loFormSet.AddProperty('lc_Title', "Transaction details" ) && CHild window title
loFormSet.AddProperty('lcADJStat', ' ' )
loFormSet.AddProperty('lcSourcJor', ' ' )              && To display source jornal discription
loFormSet.AddProperty('lcDelMesag', 'void')            && To be displaied in the delete message
loFormSet.AddProperty('lcAutBase', " "    )           && Variable to hold templatte base code

loFormSet.AddProperty('lc_TmpTrDt', gfTempName()   )           && Variable to hold temp. file name
loFormSet.AddProperty('lcAcctDesc', " "    )           && Variable to display account description
loFormSet.AddProperty('lcCodeType', "A"    )           && Variable to hold 1st account type  
loFormSet.AddProperty('lcPrdYear', " "    )           && Variable to hold the current period year
loFormSet.AddProperty('lcPeriod', ' ' )

loFormSet.AddProperty('lcOldCode', " "    )           && Variable to hold old templet code
loFormSet.AddProperty('lcHold', 'DISABLE'  )       && Variable to hold cbHold if disabled/enabled 
loFormSet.AddProperty('lc_TrnDet', " "    )           && Variable to hole the list string

loFormSet.AddProperty('lctempStat','DISABLE' )         && Status for New template code     
loFormSet.AddProperty('lcBStamp', ' '    )           && Stamp for last add user
loFormSet.AddProperty('lcObjStatus', 'DISABLE'    )           
loFormSet.AddProperty('lcObjStat', 'DISABLE')        && used in lfvRem and subseqent functions

loFormSet.AddProperty('lnOldDr', 0      )           && Variable to hold old debit  value
loFormSet.AddProperty('lnOldCr', 0      )           && Variable to hold old credit value
loFormSet.AddProperty('lnDebit', 0      )           && Variable to accept debit  value
loFormSet.AddProperty('lnCredit', 0      )           && Variable to accept cridit value
loFormSet.AddProperty('lnOldTotDr', 0      )           && Variable to hold old total debit
loFormSet.AddProperty('lnOldTotCr', 0      )           && Variable to hold old total credit
loFormSet.AddProperty('lnTranBalD', 0      )           && Variable to hold balance for debit   
loFormSet.AddProperty('lnTranBalC', 0      )           && Variable to hole balance for credit 
loFormSet.AddProperty('lnOldRec', 0      )           && Variable to hold recno to edit
loFormSet.AddProperty('lnAmount', 0      )           && Variable to hold amount to be distributed
loFormSet.AddProperty('lnTrLines', 0      )           && Variable to mantain no of enteris 
loFormSet.AddProperty('lntransNo', 0      )           && Variable to tell if the tran. changed
loFormSet.AddProperty('lnTotalDr', 0      )           && Variable to display total debit
loFormSet.AddProperty('lnTotalCr', 0      )           && Variable to display total credit
loFormSet.AddProperty('lnFcount', 0      )           && Variable to hold no. fields of entry file
loFormSet.AddProperty('lnBalance', 0      )           && Variable to hold balance 
loFormSet.AddProperty('lnSourcNo', 1      )      
loFormSet.AddProperty('puSourcJor',  1     )                   
loFormSet.AddProperty('pbActBrow',  1     )                    
loFormSet.AddProperty('llNew', .F.    )           && flag for new transaction 
loFormSet.AddProperty('llFirstlog', .T.    )           && flag for first time log
loFormSet.AddProperty('lltemplete', .F.    )                  
loFormSet.AddProperty('llFromBton', .F.    )                  
loFormSet.AddProperty('llValidDat', .T.    )                  
loFormSet.AddProperty('llBrowse', .F.    )                   
loFormSet.AddProperty('ldOldTDate', {}     )           && Variable to hold old transaction date

loFormSet.AddProperty('cbHold', 0      )                             
*******
loFormSet.AddProperty('lcObjState', ''     )                     
loFormSet.AddProperty('lcPostSt', ''     )           
loFormSet.AddProperty('lcNewSt', ''     )          
loFormSet.AddProperty('lcRemSt', ''     )           
loFormSet.AddProperty('lcAdjSt', ''     )           
loFormSet.AddProperty('lcDrCrSt', ''     )             
loFormSet.AddProperty('lcAccSt', ''     )              
loFormSet.AddProperty('lcOthSt', ''     )                
loFormSet.AddProperty('lcNewStat', ''     )                
loFormSet.AddProperty('laCtrStat[15]', ''     )                

loFormSet.AddProperty('lcTrdtexp', '' )
loFormSet.AddProperty('lcStatus','')

loFormSet.AddProperty('lstrndet',0)

loFormSet.AddProperty('llDoLocal', .T. )
loFormSet.AddProperty('lcLoclShow', "lpShow"              )

loFormSet.AddProperty('lcBatchno', loFormSet.pcBatchno)

loFormSet.AddProperty('lcVewMode', loFormSet.pcVewMode)

loFormSet.AddProperty('lcEntOrTr', loFormSet.pcEntOrTr)

loFormSet.AddProperty('llFromBat')
IF TYPE('loFormSet.lcBatchno') ='C'
  loFormSet.llFromBat = .T.
  =SEEK(loFormSet.lcBatchno, 'GLBATCH')
ELSE
  loFormSet.lcBatchno = '000000'
  loFormSet.llFromBat = .F.
  IF !glSetup.lSetSinTn
    =gfModalGen("TRM02246B00000","DIALOG")
    glQuitting  = .T.  
    RETURN   .F.
  ENDIF
ENDIF  

*** Check if edit is permiter or not
loFormSet.AddProperty('llViewMode')
IF TYPE('loFormSet.lcVewMode')='C'
  loFormSet.llViewMode = IIF(loFormSet.lcVewMode='T',.T.,.F.)
ELSE
  loFormSet.llViewMode = .F.
ENDIF

*** Check if transaction no or entery is required
loFormSet.AddProperty('lnTrsNo',0)
loFormSet.AddProperty('lnEntNo',0) 

IF TYPE('loFormSet.lcEntOrTr')='C'

  loFormSet.pcTrnHdRec = INT(VAL(loFormSet.pcTrnHdRec))
  SELECT GLTRNSHD
  IF loFormSet.pcTrnHdRec <= RECCOUNT()
    GOTO (loFormSet.pcTrnHdRec  )
  ENDIF  

  loFormSet.pcTrnDtRec = INT(VAL(loFormSet.pcTrnDtRec))
  SELECT GLTRNSDT
  IF loFormSet.pcTrnDtRec <= RECCOUNT()
    GOTO (loFormSet.pcTrnDtRec)
  ENDIF  

  loFormSet.lnTrsNo = RECNO('GLTRNSHD')  
  loFormSet.lnEntNo = RECNO('GLTRNSDT')
ELSE
  loFormSet.lcEntOrTr =''
ENDIF  

SELECT GLTRNSHD
loFormSet.AddProperty('lcModal') 
loFormSet.lcModal         = IIF(loFormSet.llFromBat AND loFormSet.llViewMode,'MODAL','')   && 

loFormSet.laFisYear[1,1]  = STR(loFormSet.lnCurr_yer-1,4) + ' - Previous'
loFormSet.laFisYear[2,1]  = STR(loFormSet.lnCurr_yer  ,4) + ' - Current'
loFormSet.laFisYear[3,1]  = STR(loFormSet.lnCurr_yer+1,4) + ' - Next'

*** Check if their is any account types in the type file
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

SELECT GLTRNSHD
*** Check if runinnig this program for first time
gcDataDir = oAriaApplication.DataDir
SELECT  cSrcJrnl+" "+cJorlnDes,cSrcJrnl;
          FROM &gcDataDir.GLSUBJOR;
          INTO ARRAY loFormSet.laSourcJor
*B610212,1 HIA 01/23/2013 when trying to post journal batch in Aria27 and Aria4, an error message appears [T20130117.0032][Start]
*IF ALEN(loFormSet.laSourcJor,1) = 1 .AND. loFormSet.laSourcJor[1,1] = ' '
IF (_TALLY = 0) OR (ALEN(loFormSet.laSourcJor,1) = 1 .AND. loFormSet.laSourcJor[1,1] = ' ')
*B610212,1 HIA 01/23/2013 when trying to post journal batch in Aria27 and Aria4, an error message appears [T20130117.0032][StarEnd]
  loFormSet.laSourcJor = " "
  loFormSet.laSourcJor[1,1] = loFormSet.lcSJ_Def
  loFormSet.laSourcJor[1,2] = loFormSet.lcSJ_Def
ELSE
  IF ASCAN(loFormSet.laSourcJor,loFormSet.lcSJ_Def) = 0
    DIMENSION loFormSet.laSourcJor[ALEN(loFormSet.laSourcJor,1)+1,2]
    =AINS(loFormSet.laSourcJor,1)
    loFormSet.laSourcJor[1,1] = loFormSet.lcSJ_Def
    loFormSet.laSourcJor[1,2] = loFormSet.lcSJ_Def
  ENDIF  
ENDIF

WITH loFormSet.Ariaform1
  .laSourcJor.RowSource = 'Thisformset.laSourcJor'
  lfColumnWidthes( .laSourcJor )
ENDWITH   

** Creat temp file with new 2 fields to hold record no. and status
SELECT GLTRNSDT
DIMENSION laFileStru[1,18]
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'NRECNO'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 4
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'cStatus'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
  
FOR lnI = lnFileStru+1 TO ALEN(laFileStru,1)
  STORE .F. TO laFileStru[lnI,5],laFileStru[lnI,6]
  FOR lnJ = 7 TO 16
    laFileStru[lnI,lnJ] = ""
  ENDFOR
  STORE 0 TO laFileStru[lnI,17],laFileStru[lnI,18]
ENDFOR


lc_TmpTrDt = loFormSet.lc_TmpTrDt
gcWorkDir = oAriaApplication.WorkDir
CREATE TABLE &gcWorkDir.&lc_TmpTrDt FROM ARRAY laFileStru

SELECT (lc_TmpTrDt)      
loFormSet.lc_TrnDet = "SUBSTR(&lc_TmpTrDt..cAcctcode,1,loFormSet.lnAcsSegSz)"+;
                "+' '+SUBSTR(LOOKUP(GLACCHAR.cAccnldes,&lc_TmpTrDt..cAcctcode,"+;
                "GLACCHAR.cAcctcode,'ACCTCODE'),1,24-loFormSet.lnAcsSegSz)+' '+"+;
                "IIF(&lc_TmpTrDt..cDrorcr = 'D',STR(&lc_TmpTrDt..nAmount,15,2),+;
                 SPACE(15)+STR(&lc_TmpTrDt..nAmount,15,2))"

lcScFields = loFormSet.lcScFields  
SELECT GLTRNSHD
*** If comming from the batch program to view/edit spacific transaction or entry
IF !EMPTY(loFormSet.lcEntOrTr)
  *** Stop on the requisted record and go to vew or edit mode
  GO (loFormSet.lnTrsNo) IN GLTRNSHD
  SCATTER FIELDS &lcScFields TO loFormSet.laData
  IF loFormSet.llViewMode
    loFormSet.ChangeMode('V')
  ELSE
    loFormSet.ChangeMode('E')
  ENDIF    
ELSE
  *** If not initaliz the laData array with blanks
  SCATTER FIELDS &lcScFields TO loformSet.laData BLANK
  * otherwise go to select mode
  loFormSet.ChangeMode('S')
  *E303161,4 TMI 10/07/2012 [End  ] 
ENDIF  
*** Calling the gpStatic FUNCTION  will creat temp record in the static file 

loFormSet.lcNewStat = IIF(loFormSet.ActiveMode = 'S' .OR. loFormSet.ActiveMode = 'V',"ENABLE","DISABLE")

*** Setting brows filter 
*** If comming from the menu filter on transaction with batch No. 0
*** If comming form the batch program filter on the batch No. Sent as 
*** a parameter
SELECT GLTRNSHD
lcFilt = "cBatchNo='"+loFormSet.lcBatchno+"' AND CTRNSTAT $ 'EOUHAP'"

SET FILTER TO &lcFilt
SET RELATION TO "T"+ gltrnshd.cautcode INTO GLAUTHD ADDITIVE

*- Set the browse filter
loFormSet.cBrowseFilter          = lcFilt

llDoLocal  = .T.

IF VAL(loFormSet.lcBatchno) > 0 .AND. !loFormSet.llViewMode AND GLBATCH.CBATSTAT = 'E'
  lnCurObj = 3
  KEYBOARD "{ENTER}"
  KEYBOARD "{ENTER}"
ENDIF  

**********
loFormSet.lcObjStatus = IIF(loFormSet.ActiveMode = 'V' .AND. loFormSet.laData[8] $ 'EOUZH',"ENABLE","DISABLE")
loFormSet.laCtrStat[7]= loFormSet.lcObjStatus
loFormSet.laCtrStat[8]= loFormSet.lcObjStatus
**********

*** Call the scareen program
SELECT GLTRNSHD
*** If quitting from the program release the child window
*** and erase the temp file

*- Input mask
WITH loFormset.Ariaform1
  .laData2.KeyTextbox.InputMask = 'X'+REPLICATE('9',FSIZE('CTRANNO','GLTRNSHD')-1)
  .laData4.MaxLength = FSIZE('CTRNDESC','GLTRNSHD')
  .laData5.MaxLength = FSIZE('CTRNREFER','GLTRNSHD')
ENDWITH 

*- End of lfFormInit.

************************************************************
*! Name      : lfFormDestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/28/2012
*! Purpose   : Form Destroy event
************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormSet

lc_TmpTrDt = loFormSet.lc_TmpTrDt
gcWorkDir  = oAriaApplication.WorkDir
IF USED(lc_TmpTrDt)
  USE IN ALIAS(lc_TmpTrDt)
ENDIF
ERASE &gcWorkDir.&lc_TmpTrDt..DBF

*- End of lfFormDestroy.
************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/27/2012
*! Purpose   : Activate method 
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet
IF TYPE('loFormSet.llFirstlog')<>'U'
  *loFormSet.llFirstlog = .F.
ENDIF   
IF loFormSet.ActiveMode $ 'SV' AND loFormSet.llFromBat .AND. loFormSet.llViewMode   
  WITH loFormSet.oToolBar
    .cmdAdd.Enabled = .F.

    .cmdTop.Enabled = .T.
    .cmdPrev.Enabled = .T.
    .cmdNext.Enabled = .T.
    .cmdEnd.Enabled = .T.
    loFormSet.CheckNavigation()
    loFormSet.oToolBar.Navrefresh()  
  ENDWITH 
ENDIF

=lfPostCase(loFormSet)


*- End of lfFormActivate.

************************************************************
*! Name      : lfPostCase
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 06/06/2012
*! Purpose   : Do not edit posted transaction
************************************************************
FUNCTION lfPostCase
PARAMETERS loFormSet
WITH loFormSet
IF .ActiveMode = 'V'
  llEnbl = EMPTY(.laData[8]) OR !.laData[8] $ 'PA'
  .oToolBar.cmdEdit.Enabled = llEnbl 
  .oToolBar.cmdDelete.Enabled = llEnbl 
ENDIF 
ENDWITH 

*disable edit,delete button if called from batch screen in view mode
IF loFormSet.llFromBat AND loFormSet.ActiveMode $ 'SV'
  loFormSet.oToolBar.ActiveMode = loFormSet.ActiveMode 
  WITH loFormSet.oToolBar
    IF loFormSet.llViewMode
      .cmdDelete.Enabled = .F.
      .cmdEdit.Enabled = .F.    
    ELSE
      .cmdAdd.Enabled = .T.
    ENDIF 
  ENDWITH 
ENDIF 

*- End of lfPostCase.

************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/14/2012
*! Purpose   : change mode function
***************************************************************
FUNCTION lfChangeMode

*- End of lfChangeMode.
PARAMETERS loFormSet
oFrm = loFormSet.Ariaform1
IF TYPE('loFormSet.lcBaseFile')='U'
  RETURN 
ENDIF    
gcDataDir = oAriaApplication.DataDir

lcScFields = loFormSet.lcScFields
DIMENSION laData[ALEN(loFormSet.laData)]

SELECT GLTRNSHD
IF loFormSet.ActiveMode = 'V'
  loFormSet.lcBStamp  = DTOC(glTrnsHd.dAdd_Date) + glTrnsHd.cAdd_Time 
  SCATTER FIELDS &lcScFields TO loFormSet.laData
ENDIF  
ACOPY(loFormSet.laData,laData)

*** laData[1] will carry the batch no or 0
laData[1]  = loFormSet.lcBatchno

=lfCheckRec() 

laData[18] = oariaapplication.ActiveModuleID
laData[21] = oariaapplication.activecompanyid
laData[20] = "N" 

lcVewStat=IIF(loFormSet.llViewMode,'DISABLE','ENABLE')
*** Automatic base doller or percent have to be reset with each new recrd
loFormSet.lcAutBase  = ''

*** if add mode then use Exist batch no. else no change
*** We have in laStatus
*** 'E' For 'Empty'
*** 'U' For ' Balanced'
*** 'O' For 'Out Of Balance '
*** # for dummy element of other screens
lc_TmpTrDt = loFormSet.lc_TmpTrDt 
gcWorkDir = oAriaApplication.WorkDir
DO CASE 
  *** Select mode
  CASE loFormSet.ActiveMode = 'S'   
     
    loFormSet.lnTotalDr  = 0 
    loFormSet.lnTotalCr  = 0
    loFormSet.lnTranBalD = 0.00                       &&  Transation Balance Dr.
    loFormSet.puSourcJor = 0
    loFormSet.lnTranBalC = 0
    loFormSet.lnBalance  = 0
    loFormSet.lcDrOrCr   = ' '
    loFormSet.lcAutDes   = ' '
    loFormSet.lcSourcJor = ' '
    ****
    loFormSet.lcNewSt  = 'DISABLE'
    loFormSet.lcRemSt  = 'DISABLE'
    loFormSet.lcAdjSt  = 'DISABLE'
    loFormSet.lcAccSt  = 'DISABLE'    
    loFormSet.lcDrCrSt = 'DISABLE'
    loFormSet.lcOthSt  = 'DISABLE'
    loFormSet.lcPostSt = 'DISABLE'    
    loFormSet.Ariaform1.laData2.Enabled = .T.  
    loFormSet.Ariaform1.pbPost.Enabled = .F.
    
    loFormSet.lcTrdtexp = ' '    
    IF loFormSet.llFromBat .AND. loFormSet.llViewMode   
      loFormSet.oToolBar.cmdAdd.Enabled = .F.
    ELSE
      loFormSet.oToolBar.cmdAdd.Enabled = .T.
    ENDIF
      
    loFormSet.Ariaform1.cbHold.Enabled = .F.
    loFormSet.Ariaform1.laSourcJor.Enabled = .F.
    SELECT (lc_TmpTrDt)
    ZAP
    SELECT GLTRNSHD
    *** Cahnge the delete botton to void
    loFormSet.lcObjStatus = 'DISABLE'
    loFormSet.laCtrStat[8] = "DISABLE"
    
    SCATTER FIELDS &lcScFields TO loFormSet.laData BLANK 
    ACOPY(loFormSet.laData,laData)
    lfPopulateFields(loFormSet)

  *** View mode
  CASE loFormSet.ActiveMode = 'V' .OR. loFormSet.ActiveMode = 'E'
    loFormSet.lcAutDes = GLAUTHD.cAutDes
    oFrm.laData2.Enabled = .F.
    IF loFormSet.llFirstlog
      *** Get the right discription of the source jornal code comming from
      *** the transaction header record
    lcExactSet = SET("EXACT")
    IF !EMPTY(loFormSet.laSourcJor)
      SET EXACT ON
      loFormSet.lnSourcNo  = ASCAN(loFormSet.laSourcJor,laData[11])
      IF loFormSet.lnSourcNo > 0
        loFormSet.lcSourcJor = loFormSet.laSourcJor [ASUBSCRIPT(loFormSet.laSourcJor,loFormSet.lnSourcNo,1),1]
      ELSE
        loFormSet.lcSourcJor = ' '
        loFormSet.lnSourcNo  = ASCAN(loFormSet.laSourcJor,loFormSet.lcSJ_Def)
      ENDIF
      loFormSet.puSourcJor = ASUBSCRIPT(loFormSet.laSourcJor,loFormSet.lnSourcNo,1)
      SET EXACT &lcExactSet
    ENDIF
    *** Collect all transaction lines for this header
    SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus';
            FROM &gcDataDir.GLTRNSDT ;
            INTO DBF &gcWorkDir.&lc_TmpTrDt;
            WHERE GLTRNSDT.cBatchno+GLTRNSDT.cTranno = laData[1]+laData[2]
    loFormSet.lnTrLines  = _TALLY
    loFormSet.lsTrnDet   = 1    
    *** Get the type of accounts
    loFormSet.lnTotalDr  = MAX( laData[9], laData[10])
    loFormSet.lnTotalCr  = loFormSet.lnTotalDr
    lnTempBal  = ABS(laData[9] - laData[10])
    loFormSet.lnBalance  = lnTempBal
    loFormSet.lcDrOrCr   = IIF(loFormSet.lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
    loFormSet.lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
    loFormSet.lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)
    loFormSet.lcCodeType = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,&lc_TmpTrDt..cAcctcode,;
                   GLACCHAR.cAcctCode,'ACCTCODE'),1)="Y","S","T")
    loFormSet.lcAcctCode = &lc_TmpTrDt..cAcctcode
    loFormSet.lcAcctDesc = LOOKUP(GLACCHAR.cAccnldes,&lc_TmpTrDt..cAcctcode,;
                        GLACCHAR.cAcctcode,'ACCTCODE')
    IF &lc_TmpTrDt..cDrOrCr ="D"
      loFormSet.lnDebit  = &lc_TmpTrDt..nAmount
      loFormSet.lnCredit = 0
    ELSE
      loFormSet.lnDebit  = 0
      loFormSet.lnCredit = &lc_TmpTrDt..nAmount
    ENDIF

    * This line was added by HS to fix the Comment field [Begin]
    loFormSet.lcTrdtexp = &lc_TmpTrDt..cTrDtExp

    IF loFormSet.llFromBat
      loFormSet.lnOldTotDr = laData[9]
      loFormSet.lnOldTotCr = laData[10]
    ENDIF            
    loFormSet.cbHold     = IIF(laData[8]='H',1,0)
    loFormSet.lcHold     = IIF(laData[8] $ 'UH','ENABLE','DISABLE') 
    lctempStat = IIF(loFormSet.lnTrLines= 0    ,'ENABLE','DISABLE')
    loFormSet.lcADJStat  = IIF(laData[9]=laData[10],'DISABLE','ENABLE')
    loFormSet.lcAdjSt = loFormSet.lcADJStat
    loFormSet.lcOthSt = lcTempStat
    
    *- Populate the fields on the screen
    =lfPopulateFields(loFormSet)
    
  ELSE
    loFormSet.llFirstlog  = .T.
  ENDIF
  IF loFormSet.ActiveMode = 'V'
      IF laData[8] $ 'PA'
        loFormSet.lcPostSt = 'DISABLE'
        loFormSet.Ariaform1.pbPost.Enabled = loFormSet.lcPostSt = 'ENABLE'
        loFormSet.laCtrStat[7] = "DISABLE"
        
        loFormSet.lcObjStatus = 'DISABLE'     
        loFormSet.laCtrStat[8] = "DISABLE"
        
      ELSE
        IF laData[19]='Y'
          loFormSet.lcPostSt   = IIF(laData[8]='U','ENABLE','DISABLE') 
        ELSE
          loFormSet.lcPostSt   = IIF(laData[8] $ 'UO','ENABLE','DISABLE') 
        ENDIF
        loFormSet.Ariaform1.pbPost.Enabled = loFormSet.lcPostSt = 'ENABLE'
        
        IF loFormSet.llFromBat   
          IF loFormSet.llViewMode  
            loFormSet.lcObjStatus = 'DISABLE'
            loFormSet.laCtrStat[8] = "DISABLE"
            
            loFormSet.laCtrStat[7] = "DISABLE"
          ELSE
            loFormSet.lcObjStatus = 'ENABLE'
            loFormSet.laCtrStat[8] = "ENABLE"
            
            loFormSet.laCtrStat[7] = "ENABLE"
          ENDIF   
        ELSE
          loFormSet.lcObjStatus = 'ENABLE'
          loFormSet.laCtrStat[8] = "ENABLE"
          loFormSet.laCtrStat[7] = "ENABLE"
        ENDIF
      ENDIF

      loFormSet.lcOthSt = 'DISABLE'
      loFormSet.ariaform1.pbEntries.Enabled = .T.
      loFormSet.Ariaform1.cbHold.Enabled = .F.

      loFormSet.lcNewSt  = 'DISABLE'
      loFormSet.lcRemSt  = 'DISABLE'
      loFormSet.lcOthSt  = 'DISABLE'
      loFormSet.lcAdjSt  = 'DISABLE'
      *** Cahnge the delete botton to void

      * call the screen activate, this is useful in navigation to enable/disable Edit,Del buttons correctly
      =lfFormActivate(loFormSet)

  ELSE 
     lcstat  = IIF(loFormSet.lnTrLines = 0,"DISABLE","ENABLE")
     loFormSet.lcRemSt = lcStat
     loFormSet.lcOthSt  = IIF(loFormSet.lnTrLines <> 0,"DISABLE","ENABLE")
     loFormSet.lcOthSt = lctempStat
     loFormSet.lcPostSt = 'DISABLE'
     loFormSet.Ariaform1.pbPost.Enabled = loFormSet.lcPostSt = 'ENABLE'
     loFormSet.lcAdjSt = loFormSet.lcADJStat
     *** Cahnge the delete botton to void
     loFormSet.lcObjStatus = 'DISABLE'
     loFormSet.laCtrStat[8] = "DISABLE"
   ENDIF
  =lfControl()
  
  
  *** Add mode 
  CASE loFormSet.ActiveMode = 'A'
    oFrm.laData2.KeyTextbox.Value = ' '
    IF loFormSet.llFirstlog  .OR. loFormSet.llNew
      loFormSet.llNew=.F.
      loFormSet.lnAmount   = 0
      loFormSet.lnTotalDr  = 0 
      loFormSet.lnTotalCr  = 0
      loFormSet.puSourcJor = 0
      loFormSet.lnTranBalD = 0.00                       &&  Transation Balance Dr.
      loFormSet.lnTranBalC = 0
      loFormSet.lnTrLines  = 0
      loFormSet.lnDebit    = 0 
      loFormSet.lnCredit   = 0 
      loFormSet.lnBalance  = 0
      loFormSet.lcAutDes   = ' '
      loFormSet.lcCodeType = "A"
      loFormSet.lcAutBase  = ''    
      loFormSet.lcAcctCode = REPLICATE("0",loFormSet.lnAcsSegSz) 
      loFormSet.lcAcctDesc = ""
      loFormSet.lcDrOrCr   = ''
      laData[2]  = SPACE(FSIZE(glTrnsHd.ctranNo))
      laData[8]  = 'E'             && init. status with 'E'==empty      
      loFormSet.Ariaform1.txtStatus.Value = loFormSet.laTStatus[AT(laData[8],'EOUHPA')]
      
      lcExactSet = SET("EXACT")
      SET EXACT ON
    
      loFormSet.lnSourcNo  = ASCAN(loFormSet.laSourcJor,loFormSet.lcSJ_Def)
      loFormSet.puSourcJor = ASUBSCRIPT(loFormSet.laSourcJor,loFormSet.lnSourcNo,1)
      laData[11] = ' '
      IF loFormSet.lnSourcNo > 0
        loFormSet.lcSourcJor = loFormSet.laSourcJor [ASUBSCRIPT(loFormSet.laSourcJor,loFormSet.lnSourcNo,1),1]
        laData[11] = loFormSet.laSourcJor [ASUBSCRIPT(loFormSet.laSourcJor,loFormSet.lnSourcNo,1),2]
      ELSE
        loFormSet.lcSourcJor = ' '
      ENDIF      
      loFormSet.Ariaform1.laSourcJor.Value = laData[11]

      SET EXACT &lcExactSet
      laData[15] = 'N'             && reverse 
      laData[5]  = SUBSTR("On " + IIF(SET('CENTURY')='ON',DTOC(oAriaApplication.SystemDate),;
                         LEFT(DTOC(oAriaApplication.SystemDate),6)+STR(YEAR(oAriaApplication.SystemDate),4))+SPACE(15),1,15)
      loFormSet.Ariaform1.laData5.Value = laData[5]
      laData[3]   = oAriaApplication.SystemDate
      loFormSet.Ariaform1.laData3.Value = laData[3]
      
      loFormSet.ldOldTDate  = {}
      laData[4]  = "Created by " + LOOKUP(SYUUSER.cUsr_Name,oAriaApplication.User_ID,SYUUSER.cUser_Id)      
      laData[4]  =SUBSTR(laData[4],1,40)
      loFormSet.Ariaform1.laData4.Value = laData[4]

      loFormSet.lcAdjSt = 'DISABLE'      

      loFormSet.lcPostSt = 'DISABLE'

      loFormSet.Ariaform1.pbPost.Enabled = loFormSet.lcPostSt = 'ENABLE'

      loFormSet.lcRemSt = 'DISABLE'
      loFormSet.lcOthSt = 'ENABLE'
      
      loFormSet.lcAccSt  = 'DISABLE'    
      loFormSet.lcDrCrSt = 'DISABLE'
      loFormSet.Ariaform1.cbHold.Enabled = .F.
      loFormSet.lcObjStatus = 'DISABLE'      
      loFormSet.laCtrStat[8] = "DISABLE"

      *initialize debit & credit to 0
      laData[ 9] = 0
      laData[10] = 0
      *E303161,4 TMI 08/08/2012 [Start] empty laData[16]
      laData[16] = ''
      *E303161,4 TMI 08/08/2012 [End  ] 

      *- Enable/Disable fields
      WITH loFormset.Ariaform1
        .laData2.Enabled = .F.
        .laData3.Enabled = .T.
        .txtStatus.Enabled = .T.
        .laData5.Enabled = .T.
        .laData12.Enabled = .T.
        .laData7.Enabled = .F.
        .laData6.Enabled = .F.
        .laData4.Enabled = .T.
        .laSourcJor.Enabled = .T.
        .laData16.Enabled = .T.
        .lcAutDes.Enabled = .T.
        .laData9.Enabled = .T.
        .laData10.Enabled = .T.
        .lnbalance.Enabled = .T.
      ENDWITH 
      
      *E303161,4 TMI 08/08/2012 [Start]
      loFormSet.Ariaform1.laData16.Keytextbox.Value = ''
      loFormset.Ariaform1.lcAutDes.Value = ''
      loFormSet.Ariaform1.Refresh()
      *E303161,4 TMI 08/08/2012 [End  ] 
      
    ELSE
      loFormSet.lcPostSt = 'DISABLE'
      loFormSet.Ariaform1.pbPost.Enabled = loFormSet.lcPostSt = 'ENABLE'
      loFormSet.llFirstlog  = .T.  
      loFormSet.llNew       = .F.
    ENDIF
    
 ENDCASE
*** If comming from the batch program with spesific transaction entry
*** select this entry and activat the child window
IF loFormSet.lcEntOrTr = 'E'
  loFormSet.lcEntOrTr =''
  =lfControl()
  =lfShowDtl(loFormSet)  &&'CWRGLTRDT',loFormSet.lc_Title)
  SELECT (lc_TmpTrDt)

  LOCATE FOR nRecNo = loFormset.lnEntNo

  loFormSet.lsTrnDet = RECNO()
ENDIF

loFormSet.cbHold     = IIF(laData[8]='H',1,0)
IF loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A'
  loFormSet.lcHold     = IIF(laData[8] $ 'UH','ENABLE','DISABLE') 

  loFormSet.Ariaform1.cbHold.Enabled = loFormSet.lcHold = 'ENABLE'
ELSE

  loFormSet.Ariaform1.cbHold.Enabled = .F.
ENDIF

*** Refresh the list object
IF loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A'
  lctempStat = IIF(loFormSet.lnTrLines= 0    ,'ENABLE','DISABLE')

  lfShowGet(loFormSet,'laData3',lctempStat)

  lfShowGet(loFormSet,'laData16',lctempStat)

  loFormSet.lcOthSt = lctempStat
ENDIF  

ACOPY(laData,loFormSet.laData)
=lfAfterShowDtl(loFormSet)

loFormSet.llBrowse  = .F.
SELECT GLTRNSHD

*B610426,1 TMI 07/09/2013 [Start] set the focus to laData3
IF loFormSet.ActiveMode = 'A'
  loFormSet.Ariaform1.laData3.Text1.SetFocus()
ENDIF 
*B610426,1 TMI 07/09/2013 [End  ] 

************************************************************
*! Name      : lfPopulateFields
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/28/2012
*! Purpose   : Populate Fields
************************************************************
FUNCTION lfPopulateFields
PARAMETERS loFormSet
WITH loFormset.Ariaform1
  .laData2.KeyTextbox.Value = loFormset.laData[2]
  .laData3.Text1.Value = loformset.laData[3]
  .txtStatus.Value = IIF(laData[8] $ 'EOUHPA',loFormSet.laTStatus[AT(laData[8],'EOUHPA')],'')
  .laData5.Value = loformset.laData[5]
  .laData12.Value = loformset.laData[12]
  .laData7.Value = loformset.laData[7]
  .laData6.Value = loformset.laData[6]
  .laData4.Value = loformset.laData[4]
  .laSourcJor.Value = loformset.laData[11]
  .laData16.KeyTextbox.Value  = loformset.laData[16]
  .lcAutDes.Value = IIF(loFormSet.ActiveMode <> 'S' ,  GLAUTHD.cAutDes , '' )
  .laData9.Value = loformset.laData[9]
  .laData10.Value = loformset.laData[10]
  .lnbalance.Value = loformset.laData[9] - loformset.laData[10]      
ENDWITH 
*- End of lfPopulateFields.

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
* Validation of the transaction no key field
FUNCTION lfvData_2
PARAMETERS loFormSet,loFld

SELECT GLTRNSHD

lcScFields = loFormSet.lcScFields
DIMENSION laData[ALEN(loFormSet.laData)]

laData[1] = loFormSet.lcBatchno
laData[2] = loFormSet.Ariaform1.laData2.KeyTextbox.Value
IF loFormSet.llBrowse

  laData[2] = '?'

ENDIF

*In case of coming from the view mode in the batch screen.
IF loFormSet.llFromBat .AND. loFormSet.llViewMode
  *If type "?" in the key field.
  IF LEFT(laData[2],1) = '?'
    lcOldData = laData[2]

    DIMENSION laGetData[2]
    laGetData = ''
    =gfBrows('laData[1]',lcScFields,'laGetData')

    IF !EMPTY(laGetData)
      loFormSet.ChangeMode('V')
    ELSE
      laData[2] = ""
    ENDIF
  
  ELSE
  
    laData[2]=RIGHT("00000000"+ALLTRIM(laData[2]),8)
    IF SEEK(laData[1]+laData[2])
      loFormSet.ChangeMode('V')
      SCATTER FIELDS &lcScFields TO laData
    ELSE  
      *If not found give the user the option to renter or browse
      *Messsage : " � is not found in the data file. "
      *Button   : "  < Browse > - < Reenter > "
      IF gfModalGen("QRM00001B00014","DIALOG","GL Batch Number\Transaction Number : "+laData[1]+laData[2]) = 1

        lcOldData = laData[2]

        DIMENSION laGetData[2]
        laGetData = ''
        =gfBrows('laData[1]',lcScFields,'laGetData')
        *Compare the key field with its old value after coming
        *from the browse to know if return with select or cancel.
        *if data is choosen from browse

        IF !EMPTY(laGetData)
          *If coming with selecting record, go to the view 
          *mode to refresh the data
          loFormSet.ChangeMode('V')
        ELSE
          *If coming with a cancel from the browse, clear the
          *key field to back to select mode.
          laData[2] = ""
        ENDIF
      ELSE
        *If press reenter in the dialog box, clear the key field 
        *to back to select mode.
        laData[2] = ""
      ENDIF
    ENDIF
  ENDIF

ELSE
  *-------------
  
  IF loFormSet.llBrowse .OR. !EMPTY(laData[2])
    IF LEFT(laData[2],1) <> '?' 
      laData[2]=RIGHT("00000000"+ALLTRIM(laData[2]),8)
      loFld.KeyTextbox.Value = laData[2]
    ENDIF  
     
    IF RECCOUNT() <> 1 .AND. RECNO() <= RECCOUNT()
      GOTO RECNO()  
    ENDIF 
    =lfvKeyField(loFormSet,loFld)
  ENDIF
ENDIF

ACOPY(laData,loFormSet.laData)

loFormSet.llBrowse = .F.

************************************************************
*! Name      : lfvPlant
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/29/2012
*! Purpose   : Valid function for Plant_ID
************************************************************
FUNCTION lfvKeyField
PARAMETERS loFormset,loFld
LOCAL lnSlct
lnSlct = SELECT(0)

=ACOPY(laData,loFormSet.laData)
lcFile_Ttl = loFormSet.BrowseTitle
lcBrFields = loFormSet.ariaBrFields.edtBrowseFields.Value 
llView = .F.
lcBaseFile = loFormSet.lcBaseFile
llBrowse = loFld.Selectedfrombrowse 

SELECT (lcBaseFile)
IF llBrowse .OR. !gfSEEK(loFormSet.lcBatchno+loFld.KeyTextBox.VALUE) .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0  
  IF llBrowse .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0  
    IF loFormSet.oToolBar.cmdFind.Click()
      llView = .T.
    ELSE
      loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
    ENDIF
  ELSE
    lnOption  = gfModalGen('QRM00001B00001','Dialog',;
                   'GL Batch Number-GL Transaction Number:'+loFormSet.lcBatchno+'-'+ALLTRIM(loFld.KeyTextBox.VALUE))  
      
    DO CASE
      CASE lnOption = 1
      
        IF loFormSet.oToolBar.cmdFind.Click()
          llView = .T.
        ELSE
          loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        ENDIF
      CASE lnOption = 2
        SELECT gltrnshd
        lcKey = EVALUATE(KEY())
        lcFlt = FILTER()
        SET FILTER TO 
        LOCATE   
        IF SEEK(loFormSet.lcBatchno+loFld.KeyTextBox.VALUE,'GLTRNSHD','BATCHTRN') AND GLTRNSHD.CTRNSTAT = 'V'
          =gfModalGen('INM00096B00000','DIALOG')
          SET FILTER TO &lcFlt
          LOCATE 
          SEEK(lcKey)
          loFld.KeyTextBox.VALUE = ''
          SELECT (lnSlct)
          RETURN .F.
        ENDIF 
        
        SET FILTER TO &lcFlt
        LOCATE 
        SEEK(lcKey)
        SELECT (lnSlct)

        loFormSet.oToolbar.cmdAdd.Click()
        *E303161,4 TMI 10/07/2012 [End  ] 
        RETURN
        
      CASE lnOption = 3
        loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        RETURN .F.
    ENDCASE
  ENDIF
ELSE
  loFld.KeyTextBox.VALUE = &lcBaseFile..CTRANNO
  llView = .T.
ENDIF

IF llView = .T.
  loFormSet.CHangeMode('V')
  loFormSet.CheckNavigation()
  loFormSet.oToolBar.Navrefresh()  
ENDIF 

SELECT (lnSlct)
*- End of lfvPlant.

*!**************************************************************************
*!
*!      Function: lfvVldDate
*!
*!**************************************************************************
*
* Transaction date  Or reverse date validation
*

FUNCTION lfvVldDate
PARAMETERS loFormSet,loFld

lcScFields = loFormSet.lcScFields
DIMENSION laData[ALEN(loFormSet.laData)]
ACOPY(loFormSet.laData,laData)

*locate the batch record on GLBATCH
=SEEK(laData[1],'GLBATCH','BATCHNO')

lcObjName  = IIF(UPPER(loFld.name)='LADATA3','LADATA(3)','LADATA(12)')

ldChekDate = loFld.Value
lcTrOrRev  = IIF(lcObjName='LADATA(3)','Transaction','Reverse')

loFormSet.llValidDat = .T.
*** Get the period of the sending date & its ***
*** begining date & ending date...  
lnPeriod  = 0
lcPrdYear = ''
llLockSt  = .F.
lnPeriod  = gfPeriod(ldChekDate,oariaapplication.prntcompanyid,@lcPrdYear,'','',@llLockSt)

loFormSet.lcPrdYear = lcPrdYear
loFormSet.lcPeriod  = RIGHT("0"+ALLTRIM(STR(lnPeriod)),2)
IF lcObjName='LADATA(3)'
  loFormSet.Ariaform1.laData7.Value = loFormSet.lcPeriod
  loFormSet.Ariaform1.laData6.Value = loFormSet.lcPrdYear
ENDIF   

IF loFormSet.llFromBat .AND. lcObjName='LADATA(3)'
  IF !BETWEEN(ldChekDate,glbatch.dbatpbeg,glbatch.dbatpend) 
    =gfModalGen("TRM02010B00000","Dialog",DTOC(glbatch.dbatpbeg)+'   '+DTOC(glbatch.dbatpend))
    loFormSet.llValidDat = .F.
    =lfVldDate()      
    RETURN loFormSet.llValidDat
  ENDIF
ENDIF

IF (!EMPTY(laData[12]) .OR. lcObjName='LADATA(3)') .AND. loFormSet.llValidDat
  DO CASE
    CASE !BETWEEN(VAL(lcPrdYear),loFormSet.lnCurr_yer-1,loFormSet.lnCurr_yer+1)
      *** Transaction/Revers date must fall within the posting window
      =gfModalGen("TRM02011B00000","Dialog",lcTrOrRev)
      loFormSet.llValidDat = .F.
      =lfVldDate()      
      RETURN loFormSet.llValidDat     

    CASE lnPeriod > 0 .AND. llLockSt
      *** This date fall in a locked period
      =gfModalGen("TRM02158B00000","Dialog")
      loFormSet.llValidDat = .F.
      =lfVldDate()      
      RETURN loFormSet.llValidDat       
    ENDCASE
 
  IF ldChekDate < glSetup.dSetBBDat
    IF glSetup.lSetPBBBd 
      *** Transaction/Reverce date must not fall before the Beginning balance date ---
      =gfModalGen("TRM02159B00000","Dialog",lcTrOrRev+"|"+DTOC(glSetup.dSetBBDat))
      loFormSet.llValidDat = .T.
      =lfVldDate()      
      RETURN loFormSet.llValidDat           
    ELSE
      *** Transaction/Reverce date must not fall before the Beginning balance date ---
      *** Posting before the Beginning balance date is not allowed.
      =gfModalGen("TRM02202B00000","Dialog",lcTrOrRev+"|"+DTOC(glSetup.dSetBBDat))
      loFormSet.llValidDat = .F.    
      =lfVldDate()      
      RETURN loFormSet.llValidDat      
    ENDIF  
  ELSE
    =lfVldDate()      
  ENDIF
ELSE

  =lfVldDate()      

ENDIF
RETURN loFormSet.llValidDat
*!**************************************************************************
*!
*!      Function: lfVldDate
*!
*!**************************************************************************
*
* Transaction date  Or reverse date validation
FUNCTION lfVldDate

IF loFormSet.llValidDat
  IF lcObjName='LADATA(3)'  
    laData[6] = loFormSet.lcPrdYear
    laData[7] = loFormSet.lcPeriod

    IF loFormSet.lnTrLines > 0
       SELECT (loFormSet.lc_TmpTrDt)
       REPLACE ALL dTrnpDate WITH laData [3] ;
                   cTrnpYr   WITH laData [6] ;
                   cTrnpPrd  WITH laData [7] ;
                   cStatus   WITH SUBSTR("MAM",AT(cStatus,"MAS"),1)
    ENDIF      
    IF loFormSet.lcPeriod+loFormSet.lcPrdYear <> ALLTRIM(loFormSet.lcCurr_prd)+ALLTRIM(loFormSet.lccurr_yer) 
      *** Transaction date dose not belong to the current period
      =gfModalGen("TRM02027B00000","Dialog")
    ENDIF
  ELSE
    IF EMPTY(laData[12])
      laData[15]='N'
      laData[13]=' '
      laData[14]=' '
    ELSE
      laData[15]='Y'
      laData[13]=loFormSet.lcPrdYear
      laData[14]=loFormSet.lcPeriod
    ENDIF 
  ENDIF
ELSE

  loFld.Value = loFld.OldValue

ENDIF

ACOPY(laData,loFormSet.laData)

SELECT GLTRNSHD
 

*!**************************************************************************
*!
*!      Function: lfvData_16
*!
*!**************************************************************************
*
* Templete code validation
*
FUNCTION lfvData_16
PARAMETERS loFormSet,loFld

lcScFields = loFormSet.lcScFields
DIMENSION laData[ALEN(loFormSet.laData)]
ACOPY(loFormSet.laData,laData)

laDaTa[16] = loFld.KeyTextbox.Value

IF !loFormSet.llBrowse .AND. !EMPTY(laData[16])

  laData[16] = PADR(ALLTRIM(laDaTa[16]),8)

ENDIF

IF loFormSet.llBrowse .OR. (laData[16] <> loFld.KeyTextbox.OldValue .AND. !EMPTY(laData[16])) 
  laData[19]=IIF(loFormSet.llFromBat,IIF(GLBATCH.cBatType='S','N','Y'),' ')
  SELECT GLAUTHD
  lcOldTag=SYS(22)
  SET ORDER TO TAG TYPECODE
  SET ORDER TO TAG TYPCODACC IN GLAUTDT
  SET RELATION OFF INTO GLAUTDT 
  SET RELATION TO glauthd.cauttype + glauthd.cautcode INTO GLAUTDT ADDITIVE

  SELECT GLAUTDT
  SET ORDER TO TAG ACCTCODE  IN GLACCHAR
  SET RELATION OFF INTO GLACCHAR
  SET RELATION TO glautdt.cacctcode INTO GLACCHAR ADDITIVE

  SELECT GLAUTHD
  IF SEEK('T')
    IF loFormSet.llBrowse .OR. !SEEK('T'+laData[16]) .OR. ATC("?",laData[16]) > 0
      DECLARE laRetInfo  [2]
      IF RECNO(0) > 0 .AND. RECNO(0) < RECCOUNT("GLAUTHD")
         GO RECNO(0)
      ELSE
        GO TOP
      ENDIF
      lcBrFields       = "cAutCode:H='Automated entry code',cAutDes:H='Description'"

      laRetInfo[1]     = ' '

      lcFile_Ttl       = 'Automated code file' 

      =gfBrows(["T"],'cAutCode,cAutDes',"laRetInfo",.T.)  

      IF !EMPTY(laRetInfo[1])
         laData[16] = laRetInfo[1]
         loFld.KeyTextbox.Value = laRetInfo[1]
         loFormSet.lcAutDes   = laRetInfo[2]
      ELSE
         laData[16] = loFld.KeyTextbox.OldValue
         loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue
         RETURN .F.
      ENDIF
    ENDIF
  ELSE
    =gfModalGen("TRM00052B00000",'DIALOG')
    SELECT GLAUTHD
    SET ORDER TO TAG &lcOldTag
    SET RELATION TO
    SELECT GLAUTDT
    SET RELATION TO 
    laData[16] = loFld.KeyTextbox.OldValue
    loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue
    RETURN    
  ENDIF
  *** Check all accounts in the template are active and allowed for posting
  llValidtmp = .T.
  SELECT  GLAUTDT
  SCAN REST WHILE cauttype + cautcode = 'T'+glauthd.cautcode
    DO CASE
      CASE GLACCHAR.cSegActiv='I'
        *** This templet has one or more inactiv account 
        =gfModalGen("TRM02164B00000",'DIALOG','template')
        llValidtmp = .F.
        EXIT
      CASE GLACCHAR.cSegAlPos='N'
        *** This templet has one or more account(s) not allowed for posting
        =gfModalGen("TRM02163B00000",'DIALOG','template')
        llValidtmp = .F.
        EXIT
      CASE loFormSet.llFromBat .AND. GLACCHAR.cStandard <> laData[19]
        IF laData[19]='N'  
          =gfModalGen("TRM02030B00000",'DIALOG',GLACCHAR.cAcctCode )
        ELSE
          =gfModalGen("TRM02031B00000",'DIALOG',GLACCHAR.cAcctCode )
        ENDIF
        llValidtmp = .F.
        EXIT
     ENDCASE  
  ENDSCAN
  SET RELATION TO
  IF !llValidtmp 
    laData[16] = loFld.KeyTextbox.OldValue
    loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue  
  ELSE
     SELECT GLAUTHD
     IF !EMPTY(laData[16])
       loFormSet.lcAutDes  =GLAUTHD.cAutDes
     ENDIF
     loFormSet.lcAutBase = GLAUTHD.cAutBase
  ENDIF

  SELECT GLAUTHD
  SET ORDER TO TAG &lcOldTag
  SET RELATION TO
  
  IF WVISIBLE("CWRGLTRDT")
    IF EMPTY(laData[3])
      *** You have to enter transaction date  first
      =gfModalGen( "TRM02015B00000" , "Dialog")

    ELSE
      *** If the user add valid new template code in the add mode
      IF !EMPTY(laData[16]) .AND. loFormSet.lnTrLines = 0 .AND. (loFormSet.ActiveMode = 'A' .OR. loFormSet.ActiveMode = 'E')
        *** Generat automatic transaction details and activate child window
        IF lfvTemp()
          loFormSet.lcOldCode = laData [16]
          =lfControl()
          IF (VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) = 0  .AND. loFormSet.lnTrlines <> 0 )

            loFormSet.lcNewSt = 'DISABLE'
          ENDIF
          =lfShowDtl(loFormSet)  &&'CWRGLTRDT',loFormSet.lc_Title)
          loFormSet.lnAmount=0
          =lfStat()
        ENDIF  
      ELSE
        *** If vew or edit existed lines
           =lfControl()
           IF (VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) = 0  .AND. loFormSet.lnTrlines <> 0 )

             loFormSet.lcNewSt = 'DISABLE'
           ENDIF
          =lfShowDtl(loFormSet)  &&'CWRGLTRDT',loFormSet.lc_Title)
      ENDIF
    ENDIF   
  ENDIF
ELSE
  IF EMPTY(laData[16])

    loFormSet.lcAutDes  =' '

  ENDIF
ENDIF

loFormSet.Ariaform1.lcAutDes.Value = loFormSet.lcAutDes
ACOPY(laData,loFormSet.laData)
=lfAfterShowDtl(loFormSet)

loFormSet.llBrowse    = .F.
SELECT GLTRNSHD

************************************************************
*! Name      : lfvPopup
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Source jornal validation
************************************************************
FUNCTION  lfvPopup
PARAMETERS loFormSet,loFld

loFormSet.laData[11] = ALLTRIM(loFormSet.laSourcJor[loFormSet.puSourcJor,2])

*!**************************************************************************
*!
*!      Function: lfvHold
*!
*!**************************************************************************
*
* Hold transaction check box validation
* This object will only be displaied
FUNCTION lfvHold
PARAMETERS loFormSet,loFld

loFormSet.cbHold = loFormset.Ariaform1.cbHold.Value 
loFormSet.laData[8] = IIF(loFormSet.cbHold=1,'H','U')
loFormSet.Ariaform1.txtStatus.Value = IIF(loFormSet.laData[8] $ 'EOUHPA',loFormSet.laTStatus[AT(loFormSet.laData[8],'EOUHPA')],'')

*!**************************************************************************
*!
*!      Function: lfvEntries
*!
*!**************************************************************************
*
*
FUNCTION lfvEntries
PARAMETERS loFormSet

lcScFields = loFormSet.lcScFields
DIMENSION laData[ALEN(loFormSet.laData)]
ACOPY(loFormSet.laData,laData)

IF EMPTY(laData[3])
  *** You have to enter transaction date  first
  =gfModalGen( "TRM02015B00000" , "Dialog")
  loFormSet.Ariaform1.laData3.Setfocus()

ELSE
  IF ! lfCheckRec()
    RETURN
  ENDIF  

  *** If the user add valid new template code in the add mode
  IF !EMPTY(laData[16]) .AND. loFormSet.lnTrLines = 0 .AND. (loFormSet.ActiveMode = 'A' .OR. loFormSet.ActiveMode = 'E')
    *** Generat automatic transaction details and activate child window

    IF lfvTemp() 
      loFormSet.lcOldCode = laData [16]
      =lfControl()
      IF (VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) = 0  .AND. loFormSet.lnTrlines <> 0 )
        loFormSet.lcNewSt = 'DISABLE'        
      ENDIF
      =lfShowDtl(loFormSet)  &&'CWRGLTRDT',loFormSet.lc_Title)
      loFormSet.lnAmount=0
      =lfStat()
    ENDIF  
  ELSE
    *** If vew or edit existed lines
     =lfControl()
       IF (VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) = 0  .AND. loFormSet.lnTrlines <> 0 )
        loFormSet.lcNewSt = 'DISABLE'        
      ENDIF
      =lfShowDtl(loFormSet)  &&'CWRGLTRDT',loFormSet.lc_Title)
  ENDIF
ENDIF   

ACOPY(laData,loFormSet.laData)
=lfAfterShowDtl(loFormSet)

SELECT GLTRNSHD

*!**************************************************************************
*!
*!      FUNCTION : lfvPost
*!
*!**************************************************************************
*
FUNCTION lfvPost
PARAMETERS loFormSet

lcScFields = loFormSet.lcScFields
DIMENSION laData[ALEN(loFormSet.laData)]
ACOPY(loFormSet.laData,laData)

IF ! lfCheckRec()
  RETURN
ENDIF  

IF ASCAN(loFormSet.laFisYear,glTrnsHd.cTrnpyr) <> 0
  IF gfModalGen("TRM02149B02009","DIALOG",'transaction') = 1 
     SELECT GLTRNSHD
     IF gfObj_Lock(.T.)
       IF ! lfCheckRec()
         RETURN
       ENDIF 
       IF lfTBPost(loFormset,"Transaction") > 0 
         loFormSet.ChangeMode('S')
       ENDIF
       =gfObj_Lock(.F.)  
     ENDIF
  ENDIF      

ELSE   
  ***  The Transaction posting year is out of the posting window.    
  =gfModalGen("TRM02147B00000","Dialog","transaction|transaction")     
ENDIF                        
ACOPY(laData,loFormSet.laData)

SELECT GLTRNSHD

*!**************************************************************************
*!
*!      FUNCTION : lfvPrint
*!
*!**************************************************************************
*
*** 

FUNCTION lfvPrint

IF ! lfCheckRec() 
  RETURN
ENDIF  

*!**************************************************************************
*!                             Child window validation
*!**************************************************************************

 
*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
* 
*    VALID function for push button "New" (pbNew).
*
FUNCTION lfvNew
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet

LOCAL oCnt
oCnt = loDetFormSet.Ariaform1
*** Are there any empty records in the file?
*** If there are,find them and replace them with the new values
*** else Insert a new record and prepare it to be filled by the user ,
*** initializing it with type ("D"),distribution code and status="A" 
*** ( for addition )

lc_TmpTrDt = loFormSet.lc_TmpTrDt
gcWorkDir = oAriaApplication.WorkDir

SELECT  (lc_TmpTrDt)

INSERT INTO &gcWorkDir.&lc_TmpTrDt ;
       (cBatchno,cTranno,dTrnpDate,cTrnpYr,cTrnpPrd,cStatus);
       VALUES (laData[1],laData[2],laData [3],laData [6],laData [7],'A')
*** Add Audit Information to the newly created record
=gfAdd_Info("&lc_TmpTrDt")

*** The following fields are blanked,waiting for an entry
*** When they are entered,their valid functions take care of their saving 

loFormSet.lcAcctCode = REPLICATE("0",loFormSet.lnAcsSegSz) 
oCnt.lcAcctCode.KeyTextbox.Value = loFormSet.lcAcctCode

* Blank both account desc. & the comment if add new account.
loFormSet.lcAcctDesc = ""
loFormSet.lcTrdtexp  = ""

loFormSet.lnDebit    = 0
oCnt.lnDebit.Refresh()
loFormSet.lnCredit   = 0
oCnt.lnCredit.Refresh()

*** Increase number of records in temporary file
loFormSet.lnTrLines  = loFormSet.lnTrLines + 1

*** Select the new record from the list
loFormSet.lsTrnDet   = loFormSet.lnTrLines

*** Refresh objects
loDetFormSet.Ariaform1.lcAcctcode.Enabled = .T.

loFormSet.lcAccSt  = 'ENABLE'    

loDetFormSet.Ariaform1.lcTrdtexp.Enabled = .T.

loFormSet.lcDrCrSt = 'DISABLE'

loDetFormSet.Ariaform1.lnDebit.Enabled = .F.

loDetFormSet.Ariaform1.lnCredit.Enabled = .F.

loDetFormSet.Ariaform1.pbAdj.Enabled = .F.

loFormSet.lcAdjSt = 'DISABLE'

IF loFormSet.lnTrLines=1

  loDetFormSet.Ariaform1.pbRem.Enabled = .T.
  loFormSet.lcRemSt = 'ENABLE'
  loFormSet.lcOthSt = 'DISABLE'

ENDIF

lctempStat = IIF(loFormSet.lnTrLines= 0 ,'ENABLE' ,'DISABLE')

loFormSet.Ariaform1.laData3.Enabled = lctempStat = 'ENABLE'

loFormSet.Ariaform1.laData16.Enabled = lctempStat = 'ENABLE'

loFormSet.lcOthSt = lctempStat

*** Disable New button until a valid account is ebtered

loFormSet.lcNewSt = 'DISABLE'
loDetFormSet.Ariaform1.pbNew.Enabled = loFormSet.lcNewSt = 'ENABLE'

*** Prepare the user for entry by moving the cursor
*** (activating object) to the cAcctCode field (loFormSet.lcAcctCode object) 
loDetFormset.Ariaform1.lcAcctcode.Keytextbox.SetFocus
loDetFormset.Ariaform1.Refresh()

SELECT  GLTRNSHD

***********************************************************
*! Name      : lfGetCtr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/29/2012
*! Purpose   : Get the Control reference on the screen based on its control source
************************************************************
FUNCTION lfGetCtr
PARAMETERS loContainer,lcSrc
lcSrc = UPPER(lcSrc)
LOCAL lnI,o
FOR lnI = 1 TO loContainer.ControlCount
  o = loContainer.Controls(lnI)
  IF UPPER(o.ControlSource) == lcSrc
    RETURN o
  ENDIF 
ENDFOR 
*- End of lfGetCtr.

*!**************************************************************************
*!
*!      Function: lfvRem
*!
*!**************************************************************************
*
*    VALID function for push button "Remove" (pbRem).
*  
FUNCTION lfvRem
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet
lc_TmpTrDt = loFormSet.lc_TmpTrDt

*** Confirm Removing of the record
IF gfModalGen("QRM00007B00007","ALERT") = 1
 
  SELECT  (lc_TmpTrDt)
  loFormSet.lnOldRec   = loFormSet.lsTrnDet
  *** If the record is previously modified,"M---->D"
  ***   delete it.
  *** If it is a new entry                 "A---->S"
  ***   skip it when saving
  *** else (a "Same" record )              "S---->D" 
  ***   delete it
  loFormSet.lcStatus         = SUBSTR('DDS',AT(cStatus,'SMA'),1)   
  REPLACE cStatus WITH loFormSet.lcStatus
  
  *** Decrement number of records in list
  loFormSet.lnTrLines   = loFormSet.lnTrLines-1
  IF loFormSet.lnTrLines = 0
    loFormSet.lcCodeType = "A" 
    laData[16] = SPACE(8)
    loFormSet.lcOldCode  = SPACE(8)
    loFormSet.lcAutDes   = SPACE(40)    
    laData[8]  = 'E'
  ENDIF 
  *** Adjust totals
  IF cDrOrCr ="D"
    laData[9]    = laData[9]  - nAmount
  ELSE
    laData[10]   = laData[10] - nAmount
  ENDIF
  *** Delete the current record (to be removed )
  *** If the removed record is the last one,go top 
  DELETE
  *** Check if you have to go to next record or the top one
  SKIP 
  IF EOF(lc_TmpTrDt)
    GO TOP
  ENDIF
  loFormSet.lsTrnDet   = loFormSet.lnOldRec
  loFormSet.lnTotalDr  = MAX( laData[9], laData[10])
  loFormSet.lnTotalCr  = loFormSet.lnTotalDr
  lnTempBal  = ABS( laData[9] - laData[10])
  loFormSet.lnBalance = lnTempBal
  loFormSet.lcDrOrCr   = IIF(loFormSet.lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
  loFormSet.lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
  loFormSet.lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)
  loFormSet.lnDebit    = IIF(loFormSet.lnTrLines=0 .OR. cDrOrCr="C",0,nAmount)
  loFormSet.lnCredit   = IIF(loFormSet.lnTrLines=0 .OR. cDrOrCr="D",0,nAmount)
  loFormSet.lcTrdtexp  = IIF(loFormSet.lnTrLines=0,SPACE(40),&lc_TmpTrDt..cTrdtexp) 
  loFormSet.lcAcctCode = IIF(loFormSet.lnTrLines=0,REPLICATE ("0",loFormSet.lnAcsSegSz),cAcctCode)
  loFormSet.lcAcctDesc = IIF(loFormSet.lnTrLines=0,SPACE(60),LOOKUP(GLACCHAR.cAccnlDes,;
                        &lc_TmpTrDt..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE'))
  loFormSet.lcObjStat  = IIF(loFormSet.lnTrLines = 0,"DISABLE","ENABLE")
  lctempStat = IIF(loFormSet.lnTrLines= 0 ,'ENABLE' ,'DISABLE')

  loFormSet.lcOthSt = lctempStat
  
  loFormSet.lcDrCrSt = loFormSet.lcObjStat
  loFormSet.lcAccSt  = loFormSet.lcObjStat
  loFormSet.lcNewSt = 'ENABLE'
  loDetformset.ariaform1.pbNew.Enabled = loFormSet.lcNewSt = 'ENABLE'
  loFormSet.lcRemSt = loFormSet.lcObjStat
  loFormSet.lcOthSt = IIF(loFormSet.lnTrLines <> 0,"DISABLE","ENABLE")

  loFormSet.lcObjStat  = IIF(laData[9]<>laData[10],"ENABLE","DISABLE")
  loFormSet.lcAdjSt = loFormSet.lcObjStat

  =lfStat()

ENDIF

loDetFormset.Ariaform1.Refresh()

SELECT  GLTRNSHD

*- End of lfvRem.
*!**************************************************************************
*!
*!      Function: lfvAccCode
*!
*!**************************************************************************
*
*  Valid function for the field loFormSet.lcAcctCode
* 
FUNCTION lfvAccCode
PARAMETERS loDetFormSet,loFld
loFormSet = loDetFormSet.loFormSet

lc_TmpTrDt = loFormSet.lc_TmpTrDt

PRIVATE lcCodeT

IF LEFT(LTRIM(loFormSet.lcAcctCode),1)<>'?'.AND. !ISDIGIT(LTRIM(loFormSet.lcAcctCode))
  =gfModalGen("TRM02061B00000","Dialog")
  loFormSet.lcAcctCode       = loFld.KeyTextbox.OldValue  

  RETURN .F.
ENDIF

*E303161,4 TMI 08/06/2012 [Start] define llValid
LOCAL llValid
llValid = .T.
*E303161,4 TMI 08/06/2012 [End  ] 
    
*E303161,4 TMI 08/06/2012 [Start] replace this line with another one better
*IF loFormSet.lcAcctCode = loFld.KeyTextbox.OldValue .AND. !loFormSet.llFromBton 
IF loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue .AND. !loFld.Selectedfrombrowse
  *E303161,4 TMI 08/06/2012 [End  ] 
  *** No need for validation
  RETURN
ELSE
  *E303161,4 TMI 08/06/2012 [Start] no need for this part
  *IF loFormSet.llFromBton
  *  lcOldAcct  = lcOldAcct1
  *  loFormSet.llFromBton = .F.
  *ENDIF
  IF loFld.Selectedfrombrowse
    loFld.KeyTextbox.Value = '?'+SUBSTR(loFld.KeyTextbox.Value,2)
  ENDIF 
  *E303161,4 TMI 08/06/2012 [End  ] 
  
  
  *** This condition is true only if the account code had an old entry
  *** and now it is emptied,just ignore the entry.
  IF LEFT(LTRIM(loFormSet.lcAcctCode),1)<>'?'.AND.VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) = 0 
    loFormSet.lcAcctCode     = loFld.KeyTextbox.OldValue
  ELSE
    lcCodeT        = '' 
    *** If there is only one record in the temporary file,
    *** The Account Type may be changed.
    *** This condition applies for the following cases :
    *** a. A NEW entry ( Add mode, or Edit mode) in an empty list.
    *** b. Removing all records except one.
    *** c. Removing all records ( handled from NEW )
    IF loFormSet.lnTrLines  = 1 
      IF !loFormSet.llFromBat
        loFormSet.lcCodeType   = "A" 
     ELSE
        loFormSet.lcCodeType  =IIF(GLBATCH.cBatType='S','S','T')
     ENDIF 
    ENDIF
    
    *define account description
    *E303161,4 TMI 08/06/2012 [Start] move the definition of llValid above
    *LOCAL llValid,laTmpArr
    LOCAL laTmpArr
    *E303161,4 TMI 08/06/2012 [End  ] 
    lcAcctDesc = ''    
    DIMENSION laTmpArr[ALEN(laData)]
    ACOPY(laData,laTmpArr)
    llValid = .T.

    *check if the return account is empty
    IF lfVldAccnt(loFormSet,loFld,loFormSet.lcCodeType,"C","L",.T.,@lcAcctDesc,@lcCodeT,"") .AND. !EMPTY(CHRTRAN(loFormSet.lcAcctCode,'-0',''))
    
      ACOPY(laTmpArr,laData)  
      
      IF loFormSet.lcCodeType="A"
        loFormSet.lcCodeType = IIF(LEFT(lcCodeT,1)="Y","S","T")
        laData[19]=IIF(loFormSet.lcCodeType="S",'N','Y')
      ENDIF    

      SELECT (lc_TmpTrDt) 

      *** If previously modified,"M---->M"
      *** If a new entry,        "A---->A"
      *** else                   "S---->M"       

      loFormSet.lcStatus     = SUBSTR("MAM",AT(cStatus,"MAS"),1)
      REPLACE &lc_TmpTrDt..cAcctcode WITH loFormSet.lcAcctCode ,;
              &lc_TmpTrDt..cStatus   WITH loFormSet.lcStatus

      IF EMPTY(&lc_TmpTrDt..cDrOrCr)
        REPLACE &lc_TmpTrDt..cDrOrCr   WITH "D"
      ENDIF

      SELECT GLTRNSHD
      
      *** In this screen, both debit and credit fields may have
      *** zeroes in the same record,hence,pbNew may be enabled now..
      *** provided that, if in percent mode,totals do not exceed 100% 

      lcNewSt = 'ENABLE'
      lcDrCrSt= 'ENABLE'

      loFormSet.lcObjState   = IIF(laData[9]<>laData[10],"ENABLE","DISABLE")

      loFormSet.lcAdjSt   = loFormSet.lcObjStat
      
      WITH loDetFormSet.Ariaform1
       .pbNew.Enabled = .T.
      
        loFormSet.lcNewSt = 'ENABLE'
        loFormSet.lcDrCrSt= 'ENABLE'

        .lnDebit.Enabled = .T.

        .lnCredit.Enabled = .T.

        .lcTrdtexp.Enabled = .T.

        .lcTrdtexp.Enabled = .T.        
        .pbAdj.Enabled = loFormSet.lcObjState = 'ENABLE'
        .grdTmpTrDt.Refresh()     

        .lcAcctDesc.Value = lcAcctDesc      

      ENDWITH 
      
      
     ELSE       
      loFormSet.lcAcctCode   = loFld.KeyTextbox.OldValue
      loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue
      *- restore the laData array,do not leave the control
      ACOPY(laTmpArr,laData)  
      llValid = .F.

    ENDIF
  ENDIF
ENDIF

SELECT GLTRNSHD
* return the validation result
RETURN llValid

*!**************************************************************************
*!
*!      Function:  lfvDebit
*!
*!**************************************************************************
*    VALID function for get field "lnDebit".
*    laData[9] field is used for totals
*   
FUNCTION lfvDebit
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet

Private lnOldCredit

lnOldCredit = loFormSet.lnCredit
lc_TmpTrDt = loFormSet.lc_TmpTrDt           
IF loFormSet.lnDebit <> loFormSet.lnOldDr
  *** Reject negative entries
  IF loFormSet.lnDebit < 0
    =gfModalGen("TRM02036B00000","DIALOG")
    loFormSet.lnDebit  = loFormSet.lnOldDr  
  ELSE 
    SELECT (lc_TmpTrDt)
    *** If the record has been previously saved as a credit,
    *** and now there is a debit entry,
    IF loFormSet.lnDebit > 0 .AND. cDrOrCr="C"
      *** Ignore the credit entry,blank it and adjust Credit totals. 
      loFormSet.lnCredit     = 0      
      laData[10]   = laData[10]-nAmount  
      laData[9]    = laData[9]+loFormSet.lnDebit
    ELSE 
      laData[9]   = laData[9]-nAmount+loFormSet.lnDebit
    ENDIF

    loFormSet.lcStatus   = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH loFormSet.lnDebit ;
            cStatus  WITH loFormSet.lcStatus;
            cDrOrCr  WITH "D" 

     IF loFormSet.lnTrLines = 1 .AND. lnOldCredit=0
       =lfDist() 
     ENDIF  

    loFormSet.lnTotalDr  = MAX( laData[9], laData[10])
    loFormSet.lnTotalCr  = loFormSet.lnTotalDr
    lnTempBal  = ABS( laData[9] - laData[10])
    loFormSet.lnBalance = lnTempBal
    loFormSet.lcDrOrCr   = IIF(loFormSet.lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
    loFormSet.lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
    loFormSet.lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)

    *** Adjust controls
    loFormSet.lcObjState     = IIF(laData[9]<>laData[10],"ENABLE","DISABLE")
    loFormSet.lcAdjSt = loFormSet.lcObjState
    loDetformset.Ariaform1.pbAdj.Enabled = loFormSet.lcObjState = 'ENABLE'
  ENDIF
  loFormSet.lcObjState         = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-',''))>0;
                      .AND.(loFormSet.lnDebit>0 .OR. loFormSet.lnCredit>0),"ENABLE","DISABLE")

  loFormSet.lsTrnDet   = RECNO()
 
  =lfStat()
  
ENDIF
loDetFormSet.Refresh()

lnOldCredit=0
SELECT GLTRNSHD
 
*!**************************************************************************
*!
*!      Function:  lfvCredit
*!
*!
*!**************************************************************************
*    VALID function for get field "lnCredit".
*    laData[10] field is used for totals
*   
FUNCTION lfvCredit
PARAMETERS loDetFormSet

loFormSet = loDetFormSet.loFormSet

Private lnOldDebit
lnOldDebit=loFormSet.lndebit

lc_TmpTrDt = loFormSet.lc_TmpTrDt
IF loFormSet.lnCredit <> loFormSet.lnOldCr     
  
  *** Reject negative entries
  IF loFormSet.lnCredit < 0
    =gfModalGen("TRM02036B00000","DIALOG") 
    loFormSet.lnCredit       = loFormSet.lnOldCr  

  ELSE 

    SELECT (lc_TmpTrDt)
    IF loFormSet.lnCredit > 0 .AND. cDrOrCr="D"
        *** If there is a Debit entry for the same record,ignore it,
        *** blank it and adjust Debit totals. 
        loFormSet.lnDebit    = 0
        laData[9]    = laData[9]-nAmount  
        laData[10]    = laData[10]+loFormSet.lnCredit
    ELSE
        laData[10]    = laData[10]-nAmount+loFormSet.lnCredit
    ENDIF

    loFormSet.lcStatus   = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH loFormSet.lnCredit ;
            cStatus  WITH loFormSet.lcStatus;
            cDrOrCr  WITH "C" 
    
    IF loFormSet.lnTrLines = 1 .AND. lnOldDebit=0
      = lfDist()
    ENDIF  

    loFormSet.lnTotalDr  = MAX( laData[9], laData[10])
    loFormSet.lnTotalCr  = loFormSet.lnTotalDr
    lnTempBal  = ABS( laData[9] - laData[10])
    loFormSet.lnBalance = lnTempBal
    loFormSet.lcDrOrCr   = IIF(loFormSet.lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
    loFormSet.lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
    loFormSet.lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)

    *** Adjust controls
    loFormSet.lcObjState     = IIF(laData[9]<>laData[10],"ENABLE","DISABLE")
    loFormSet.lcAdjSt = loFormSet.lcObjState
    loDetformset.Ariaform1.pbAdj.Enabled = loFormSet.lcObjState = 'ENABLE'

  ENDIF
  loFormSet.lcObjState         = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-',''))>0;
                     .AND.(loFormSet.lnDebit>0 .OR. loFormSet.lnCredit>0),"ENABLE","DISABLE")

  loFormSet.lsTrnDet   = RECNO()
 
  =lfStat()
  
ENDIF

loDetFormSet.Refresh()

lnOldDebit=0
SELECT GLTRNSHD


*!**************************************************************************
*!
*!      Function: lfvTrdtexp
*!
*!**************************************************************************
*
FUNCTION lfvTrdtexp
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet

lc_TmpTrDt = loFormSet.lc_TmpTrDt
*B610453,1 tmi [start] put the in (lc_TmpTrDt)
*REPLACE &lc_TmpTrDt..cTrdtexp WITH loFormSet.lcTrdtexp ;
        &lc_TmpTrDt..cStatus  WITH SUBSTR('MMA',AT(&lc_TmpTrDt..cStatus,'SMA'),1)   
REPLACE &lc_TmpTrDt..cTrdtexp WITH loFormSet.lcTrdtexp ;
        &lc_TmpTrDt..cStatus  WITH SUBSTR('MMA',AT(&lc_TmpTrDt..cStatus,'SMA'),1)   IN (lc_TmpTrDt)
*B610453,1 tmi [end  ] put the in (lc_TmpTrDt)
SELECT GLTRNSHD

*!**************************************************************************
*!
*!      Function: lfvAdjust
*!
*!**************************************************************************
*
*    VALID function for push button "Adjust" (pbAdj).
*  

FUNCTION lfvAdjust
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet

lc_TmpTrDt = loFormSet.lc_TmpTrDt

SELECT (lc_TmpTrDt)

laData[9]  = laData[9]  - loFormSet.lnDebit 
laData[10] = laData[10] - loFormSet.lnCredit

lnDefr     = laData[9] - laData[10]
lnSign     = SIGN(lnDefr)
lnDefr     = ABS (lnDefr)

*** If the difference is positive,i.e. Debit is greater,

IF lnSign = 1
  *** Add difference to Credit field
  laData[10]   = laData[10] + lnDefr
  loFormSet.lnCredit     = lnDefr
  loFormSet.lnDebit      = 0 
ELSE
  *** If the difference is negative,i.e.Credit is greater,
  IF lnSign = -1
    laData[9]   = laData[9] + lnDefr 
    loFormSet.lnDebit     = lnDefr
    loFormSet.lnCredit    = 0
  *** If the difference is zero,
  ELSE
    loFormSet.lnDebit     = 0
    loFormSet.lnCredit    = 0
  ENDIF  
ENDIF

loFormSet.lnTotalDr  = MAX( laData[9], laData[10])
loFormSet.lnTotalCr  = loFormSet.lnTotalDr
lnTempBal  = ABS( laData[9] - laData[10])
loFormSet.lnBalance = lnTempBal
loFormSet.lcDrOrCr   = IIF(loFormSet.lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
loFormSet.lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
loFormSet.lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)

REPLACE  cDrOrCr WITH IIF(loFormSet.lnDebit>0,"D",IIF(loFormSet.lnCredit>0,"C"," "));
         nAmount WITH lnDefr ;
         cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)    

loFormSet.lcAdjSt = 'DISABLE'
loDetFormSet.Ariaform1.pbAdj.Enabled = loFormSet.lcAdjSt = 'ENABLE'

=lfStat()

SELECT GLTRNSHD

IF VAL(STRTRAN(loFormSet.lcAcctCode,'-',''))>0
  loFormSet.lcNewSt = 'ENABLE'
ENDIF  

loDetFormSet.Refresh()

SELECT GLTRNSHD
  
************************************************************
*! Name      : lpSavScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/14/2012
*! Purpose   : save func.
************************************************************
FUNCTION lpSavScr
PARAMETERS loFormSet
PRIVATE lcOrder,lcRecNo,lnOldCr,lnOldDr

*B610426,1 TMI 07/09/2013 [Start] if the laData6 or laData7 is empty then do not save
IF EMPTY(loFormset.Ariaform1.laData6.Value) OR EMPTY(loFormset.Ariaform1.laData7.Value)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Fiscal year or fiscal period field is empty, can't save")
  RETURN .F.
ENDIF 
*B610426,1 TMI 07/09/2013 [End  ] 

*B610167,1 TMI 12/05/2012 [Start]  add a function to get the loFormSet.laData values from the screen
=lfGetData(loFormset)
*B610167,1 TMI 12/05/2012 [End  ] 

lcScFields = loFormSet.lcScFields
DIMENSION laData[ALEN(loFormSet.laData)]
ACOPY(loFormSet.laData,laData)

lc_TmpTrDt = loFormSet.lc_TmpTrDt

SELECT FSPRD
SET ORDER TO COMFYRPRDI

SEEK (oariaapplication.prntcompanyid+laData[6]+laData[7])

IF lfsplocks
  SELECT GLTRNSHD
  =gfModalGen("TRM02210B00000","Dialog",laData[7]+'-'+laData[6])
  llcSave=.F.

  RETURN llcSave
ENDIF

SELECT GLTRNSHD

DO CASE
  *** User have to add lines with amounts > 0 
  CASE laData[9] = 0 .AND. laData[10] = 0
    *** Can`t save an empty transaction
    =gfModalGen("TRM02012B00000","Dialog")
    llCSave  = .F.
    RETURN llcSave
  CASE VAL(STRTRAN(loFormSet.lcAcctCode,'-',''))=0
    =gfModalGen("TRM02022B00000","DIALOG")  
    llCSave        = .F.

    RETURN llcSave

  OTHERWISE
    *** If total debit = total credit transaction will be balanced;
    *** else out of balance
    IF laData[9]=laData[10]
      laData[8]=IIF(loFormSet.cbHold=0,'U','H')
    ELSE
      laData[8]='O'
      loFormSet.cbhold   = 0
    ENDIF

    SELECT GLTRNSHD
    IF loFormSet.ActiveMode = 'A'

      *** Creating transaction  sequence

      laData[2]  = gfSequence('CTRANNO')
      
      *** Update the lines with batch and transaction no. 

      SELECT GLTRNSHD
      *** Save header to master
      APPEND BLANK
      GATHER FROM laData FIELDS &lcScFields                              
      =gfAdd_Info('GLTRNSHD')

      SELECT (lc_TmpTrDt)      
      REPLACE ALL cbatchno  WITH laData[1],;
                  cTranno   WITH laData[2],;
                  cAdd_User WITH oAriaApplication.User_ID,;
                  dAdd_Date WITH DATE(),;
                  cAdd_Time WITH gfGetTime()

      *** Save lines to master
      =gfTmp2Mast('GLTRNSDT',lc_TmpTrDt) 

      *** Update total batch debit and credit if comming from batch program 
      IF loFormSet.llFromBat

        SELECT glTrnshd
        lcRecNo=RECNO()
        lcOrder=SYS(22)
        SET ORDER TO TRANSTAT

        IF !SEEK('O'+loFormSet.lcBatchno) 
          SELECT glBatch  
          REPLACE nBaTotDr WITH nBaTotDr+laData[9];
                  nBaTotCr WITH nBaTotCr+laData[10];
                  cBatStat WITH 'U'
          ELSE
          SELECT glBatch  
          REPLACE nBaTotDr WITH nBaTotDr+laData[9];
                  nBaTotCr WITH nBaTotCr+laData[10];
                  cBatStat WITH 'O'
        ENDIF
 
        SELECT glTrnshd
        SET ORDER TO lcORDER
        GOTO lcRecNo
      ENDIF

      *** Transaction saved with new seq. no ---  
      =gfModalGen("TRM02014B00000","DIALOG",laData[2])
     
    ELSE

        SELECT GLTRNSHD
        *** Save header record to master
        loFormSet.lnOldCr=gltrnshd.nTrntotCr
        loFormSet.lnOldDr=gltrnshd.nTrntotDr
        
        GATHER FROM laData FIELDS &lcScFields 
        =gfAdd_Info('GLTRNSHD')

        *** Save lines to master
        SELECT (lc_TmpTrDt)      
        =gfTmp2Mast('GLTRNSDT',lc_TmpTrDt) 

       *** Update total batch debit and credit if comming from batch program   
       IF loFormSet.llFromBat
         SELECT glTrnshd
         lcRecNo=RECNO()
         lcOrder=SYS(22)
         SET ORDER TO TRANSTAT
         IF !SEEK('O'+loFormSet.lcBatchno) 
           SELECT glBatch  
           REPLACE nBaTotDr WITH nBaTotDr-loFormSet.lnOldDr+laData[9];
                   nBaTotCr WITH nBaTotCr-loFormSet.lnOldCr+laData[10];
                   cBatStat WITH 'U'
           ELSE
           SELECT glBatch  
           REPLACE nBaTotDr WITH nBaTotDr-loFormSet.lnOldDr+laData[9];
                   nBaTotCr WITH nBaTotCr-loFormSet.lnOldCr+laData[10];
                   cBatStat WITH 'O'
         ENDIF
         SELECT glTrnshd
         SET ORDER TO lcORDER
         GOTO lcRecNo
        ENDIF
      ENDIF

ENDCASE
SELECT GLTRNSHD  

*- End of lpSavScr.


************************************************************
*! Name      : lfFormSavefiles
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/29/2012
*! Purpose   : Update files by gfTableUpdate
************************************************************
FUNCTION lfFormSavefiles
PARAMETERS loformSet

LOCAL i,lnSlct
lnSlct = SELECT(0)
FOR i= 1 TO ALEN(oAriaApplication.laRemotetable)
  lcFile = UPPER(ALLTRIM(oAriaApplication.laRemotetable[i].lcTableName))
  IF !EMPTY(lcFile) AND LEFT(lcFile,2) <> 'SY' AND USED(lcFile)
    SELECT (oAriaApplication.laRemotetable[i].lcTableName)
    gfTableUpdate()
  ENDIF
ENDFOR 
SELECT (lnSlct)
*- End of lfFormSavefiles.

************************************************************
*! Name      : lfGetData
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/05/2012
*! Purpose   : a function to get the loFormSet.laData values from the screen
************************************************************=lfGetData(loFormset)
*B610167,1 TMI 12/05/2012 
FUNCTION lfGetData
PARAMETERS loFormset
LOCAL o
o = loFormSet.Ariaform1
*- the commented fields are updated by the code, there are no related fields on the interface
WITH loFormset
  *.laData[1] = o.                               && CBATCHNO
  *.laData[2] = o.                               && CTRANNO
  .laData[3] = o.laData3.Text1.Value             && DTRNPDATE
  .laData[4] = o.laData4.Value                   && CTRNDESC
  .laData[5] = o.laData5.Value                   && CTRNREFER
  .laData[6] = o.laData6.Value                   && CTRNPYR
  .laData[7] = o.laData7.Value                   && CTRNPPRD
  *laData[8] = o.                                && CTRNSTAT
  .laData[9] = o.laData9.Value                   && NTRNTOTDR
  .laData[10] = o.laData10.Value                 && NTRNTOTCR
  .laData[11] = o.laSourcJor.Value               && CSRCJRNL
  .laData[12] = o.laData12.Text1.Value           && DTRNREVDT
  IF !EMPTY(.laData[12])
    *B610276,1 TMI 03/21/2013 [Start] calculate the period and year based on the fiscal year
    *.laData[13] = STR(YEAR(.laData[12]),4)       && CTRNREVYR
    *.laData[14] = PADL(MONTH(.laData[12]),2,'0') && CTRNREVPR
    lnPeriod  = 0
    lcPrdYear = ''
    llLockSt  = .F.
    lnPeriod  = gfPeriod(.laData[12],oariaapplication.prntcompanyid,@lcPrdYear,'','',@llLockSt)
    .laData[13] = lcPrdYear
    .laData[14] = PADL(lnPeriod,2,'0') 
    *B610276,1 TMI 03/21/2013 [End  ] 
  ENDIF 
  *.laData[15] = o.                              && CTRNREVER
  .laData[16] = o.laData16.KeyTextbox.Value      && CAUTCODE
  *.laData[17] = o.                              && CAUTTYPE
  *.laData[18] = o.                              && CSRCMODUL
  *.laData[19] = o.                              && CSTANDARD
  *.laData[20] = o.                              && CTRNTYPE
  *.laData[21] = o.                              && CCOMP_ID
ENDWITH 

*- End of lfGetData.

***********************************************************
*! Name      : lpDelScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/14/2012
*! Purpose   : No deletion just void this transaction
*************************************************************
FUNCTION lpDelScr
PARAMETERS loFormSet
PRIVATE lcOrder,lcRecNo

lcScFields = loFormSet.lcScFields
DIMENSION laData[ALEN(loFormSet.laData)]
ACOPY(loFormSet.laData,laData)

IF lfCheckRec()
  loFormSet.ChangeMode('S')
  REPLACE GLTRNSHD.cTrnStat WITH 'V'
  =gfAdd_Info('GLTRNSHD')
  *- Saving
  SELECT GLTRNSHD
  =gfTableUpdate()

  SELECT GLTRNSDT
  =gfTableUpdate() 
  =gfObj_Lock(.F.) 
 
  IF loFormSet.llFromBat
    SELECT glTrnshd
    lcRecNo=RECNO()
    lcOrder=SYS(22)
    SET ORDER TO TRANSTAT
    IF !SEEK('O'+loFormSet.lcBatchno) 
      SELECT glBatch  
      REPLACE nBaTotDr WITH nBaTotDr-laData[9];
              nBaTotCr WITH nBaTotCr-laData[10];
             cBatStat WITH IIF(nBaTotDr=0,'E','U')
    ELSE
       SELECT glBatch  
       REPLACE nBaTotDr WITH nBaTotDr-laData[9];
               nBaTotCr WITH nBaTotCr-laData[10];
              cBatStat WITH 'O'
    ENDIF

    SELECT glTrnshd
    SET ORDER TO lcORDER
    GOTO lcRecNo
   ENDIF

 SELECT GLTRNSHD

ELSE
  RETURN .F.
ENDIF

RETURN 

*- End of lpDelScr.

*!**************************************************************************
*!
*!      FUNCTION: lfvTemp
*!
*!**************************************************************************
* This function will be called if adding new transaction and 
* user select a template code to generat from
* teplate could be based on amounts or percentage
*
FUNCTION lfvTemp
*** Get the ammoubt to be distributed accourding to percentages

IF loFormSet.lcAutBase='P'
  DO (gcScrDir + gcWinAppl + '\GLAUTDS.SPR')
  IF loFormSet.lnAmount <= 0  
    RETURN .F.
  ENDIF
ENDIF  

gcDataDir = oAriaApplication.DataDir
lc_TmpTrDt = loFormSet.lc_TmpTrDt
gcWorkDir = oAriaApplication.WorkDir

*** Creat transaction lines from automatic detail file
SELECT  laData[1] AS 'cBatchno',SPACE(8) AS 'cTranno',;
        GLAUTDT.cAcctCode,SPACE(40) AS 'cTrdtexp',GLAUTDT.cDrOrCr, ;
        GLAUTDT.nAmount,laData[3] AS 'dTrnpDate',laData[6] AS 'ctrnpyr',;
        laData[7] AS 'ctrnpprd',;
        GLAUTDT.cAdd_User,GLAUTDT.dAdd_Date,GLAUTDT.cAdd_time,;
        00000 AS 'nRecNo' , "A" AS 'cStatus';
   FROM &gcDataDir.GLAUTDT;
   INTO DBF &gcWorkDir.&lc_TmpTrDt;
  WHERE cAutType+cAutCode='T'+laData[16] 


loFormSet.lnTrLines  = _TALLY
loFormSet.lsTrnDet   = 1    
loFormSet.lcAcctCode = &lc_TmpTrDt..cAcctcode   
loFormSet.lcCodeType = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,&lc_TmpTrDt..cAcctcode,;
                GLACCHAR.cAcctCode,'ACCTCODE'),1)="Y","S","T")
laData[19]=IIF(loFormSet.lcCodeType="S",'N','Y')

IF loFormSet.lcAutBase='P'
  REPLACE ALL &lc_TmpTrDt..nAmount WITH ;
               ( &lc_TmpTrDt..NAmount * loFormSet.lnAmount ) / 100
ENDIF

SELECT (lc_TmpTrDt)

GO TOP 

*** Calculate total debit and total credit
SUM IIF(CDrOrCr="D",nAmount,0),IIF(cDrOrCr="C",nAmount,0) ;
    TO laData[9],laData[10]


loFormSet.lnTotalDr  = MAX( laData[9], laData[10])
loFormSet.lnTotalCr  = loFormSet.lnTotalDr
lnTempBal  = ABS( laData[9] - laData[10])
loFormSet.lnBalance  = lnTempBal
loFormSet.lcDrOrCr   = IIF(loFormSet.lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
loFormSet.lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
loFormSet.lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)


IF loFormSet.lnTrLines > 0

  loFormSet.lcRemSt = 'ENABLE'
  loFormSet.lcOthSt = 'DISABLE'
  loFormSet.lcDrCrSt = 'ENABLE'
  loFormSet.lcAccSt  = 'ENABLE'    
  
  IF laData[9] <> laData[10]

    loFormSet.lcAdjSt = 'ENABLE'
    
  ENDIF  

ENDIF

loFormSet.lcOthSt = 'DISABLE'

SELECT GLTRNSHD

*!**************************************************************************
*!
*!      FUNCTION: lfDist
*!
*!**************************************************************************
*
FUNCTION lfDist

PRIVATE llDrorCr,lcOldTag,lc_cSegAutDs
lc_TmpTrDt = loFormSet.lc_TmpTrDt

SELECT (lc_TmpTrDt)
lcDstCode = LOOKUP(GLACCHAR.cSegAutDs,&lc_TmpTrDt..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE')
gcDataDir = oAriaApplication.DataDir
gcWorkDir = oAriaApplication.WorkDir

IF !EMPTY(lcDstCode)

  IF  gfModalGen('QRM02166B00006','DIALOG')=1

    llValidtmp = .T.
    SELECT  GLAUTDT
    SET ORDER TO TAG TYPCODACC IN GLAUTDT

    IF SEEK('D'+lcDstCode)

    SET ORDER TO TAG ACCTCODE  IN GLACCHAR
    SET RELATION OFF INTO GLACCHAR 
    SET RELATION TO glautdt.cacctcode INTO GLACCHAR ADDITIVE
    SCAN REST WHILE cauttype + cautcode = 'D'+lcDstCode
      DO CASE
        CASE GLACCHAR.cSegActiv='I'
          *** This templet has one or more inactiv account 
          =gfModalGen("TRM02164B00000",'DIALOG','distribution')
          llValidtmp = .F.
          EXIT
        CASE GLACCHAR.cSegAlPos='N'
          *** This templet has one or more account(s) not allowed for posting
          =gfModalGen("TRM02163B00000",'DIALOG','distribution')
          llValidtmp = .F.
          EXIT
        CASE GLACCHAR.cStandard <> laData[19]
          IF laData[19]='N'  
            =gfModalGen("TRM02030B00000",'DIALOG',GLACCHAR.cAcctCode )
          ELSE
            =gfModalGen("TRM02031B00000",'DIALOG',GLACCHAR.cAcctCode )
          ENDIF
          llValidtmp = .F.
          EXIT
      ENDCASE  
    ENDSCAN

    SELECT  GLAUTDT
    SET RELATION TO

    IF llValidtmp 
      SELECT (lc_TmpTrDt)
      SCATTER MEMVAR 
      lnActAmnt = nAmount
      llDrorCr  = (cDrorCr="D")
      *** Creat transaction lines from automatic detail file
      ***
      SELECT  laData[1] AS 'cBatchno',SPACE(8) AS 'cTranno',;
              GLAUTDT.cAcctCode,SPACE(40) AS 'cTrdtexp',GLAUTDT.cDrOrCr, ;
              GLAUTDT.nAmount,laData[3] AS 'dTrnpDate',laData[6] AS 'ctrnpyr',;
              laData[7] AS 'ctrnpprd',;
              GLAUTDT.cAdd_User,GLAUTDT.dAdd_Date,GLAUTDT.cAdd_time,;
              00000 AS 'nRecNo' , "A" AS 'cStatus';
         FROM &gcDataDir.GLAUTDT;
         INTO DBF &gcWorkDir.&lc_TmpTrDt;
        WHERE cAutType+cAutCode='D'+lcDstCode
      loFormSet.lnTrLines  = _TALLY+1
      loFormSet.lsTrnDet   = 1    
      SELECT (lc_TmpTrDt)
      REPLACE ALL nAmount WITH (nAmount * lnActAmnt) / 100;
                  cDrorCr WITH IIF(llDrorCr,'C','D')
      IF llDrorCr
        SUM nAmount TO laData[10]
      ELSE
        SUM nAmount TO laData[9]
      ENDIF  
      APPEND BLANK
      GATHER MEMVAR
      loFormSet.lcCodeType = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,&lc_TmpTrDt..cAcctcode,;
                     GLACCHAR.cAcctCode,'ACCTCODE'),1)="Y","S","T")
      laData[19]=IIF(loFormSet.lcCodeType="S",'N','Y')
     ENDIF 
    ELSE
      =gfModalGen('TRM02214B00000','ALERT',lcDstCode)
    ENDIF 
  ENDIF
ENDIF

SELECT GLTRNSHD

*!**************************************************************************
*!
*!      FUNCTION: lfStat
*!
*!**************************************************************************
*

FUNCTION lfStat

PRIVATE lcOldStat,lnOldHold,lcHoldStat
lcOldStat=laData[8]
lnOldHold=loFormSet.cbHold

IF laData[9]<>laData[10]
  laData[8]='O'  
ELSE
  laData[8]=IIF(ladata[9]<>0,'U','E')
ENDIF

lcHoldStat=IIF(laData[8]='U','ENABLE','DISABLE')

loFormSet.cbHold=IIF(lcOldStat=laData[8],lnOldHold,0)

loFormSet.Ariaform1.cbHold.Enabled = lcHoldStat='ENABLE'


*!**************************************************************************
*!
*!      FUNCTION: lfControl
*!
*!**************************************************************************
*

FUNCTION lfControl

lcNewMode=IIF(!loFormSet.ActiveMode = 'V' ,'ENABLE','DISABLE')
loFormSet.lcNewSt = lcNewMode
loFormSet.lcRemSt = lcNewMode

lcSavError=ON("ERROR")
ON ERROR X=1
lc_TmpTrDt = loFormSet.lc_TmpTrDt
SELECT (lc_TmpTrDt)
ON ERROR &lcSavError
SELECT GLTRNSHD


*!**************************************************************************
*!
*!      FUNCTION: lfCheckRec
*!
*!**************************************************************************
*

function lfCheckRec

lcLoclShow = "lpShow"
loFormSet.laCtrStat[8] = loFormSet.lcObjStatus

IF ! loFormSet.ActiveMode = 'V' 
  RETURN
ENDIF  

IF EMPTY(laData[1])
  RETURN
ENDIF

IF RECNO() <= RECCOUNT()
  GOTO RECNO()  
ENDIF  

IF loFormSet.lcBatchno+laData[2] <> glTrnsHd.cBatchNo+glTrnsHd.cTranno
  =gfModalGen("TRM02187B00000","DIALOG",;
              "Transaction"+'|'+"voided")
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK

  loFormSet.ChangeMode('S')

  RETURN .F.
ENDIF


IF ( loFormSet.lcBStamp = DTOC(glTrnsHd.dAdd_Date) + glTrnsHd.cAdd_Time ) ;
   .AND. ( laData[8] = glTrnsHd.cTrnStat )
  RETURN .T.
ENDIF

lcAdd_User =ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glTrnsHd.cAdd_User,SYUUSER.cUser_Id))
lcAdd_Time =glTrnsHd.cAdd_Time
lcAdd_Date =dtoc(glTrnsHd.dAdd_Date)
lcPost_User=ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glTrnsHd.cPostUser,SYUUSER.cUser_Id))
lcPost_Time=glTrnsHd.cPostTime
lcPost_Date=dtoc(glTrnsHd.dPostDate)


IF glTrnsHd.cTrnStat = 'V' 
  =gfModalGen("TRM02167B00000","DIALOG",;
     "Transaction"+'|'+"voided"+'|'+;
     lcAdd_User+'|'+lcAdd_Date +' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK

  loFormSet.ChangeMode('S')
  =gfObj_Lock(.F.) 

  RETURN .F.
ENDIF

IF glTrnsHd.cTrnStat = 'A'
  =gfModalGen("TRM02167B00000","DIALOG",;
     "Transaction"+'|'+"approved"+'|'+;
     lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData 
  loFormSet.lcBStamp  = DTOC(glTrnsHd.dAdd_Date) + glTrnsHd.cAdd_Time 

  loFormSet.ChangeMode('E')
  =gfObj_Lock(.F.) 

  RETURN .F.
ENDIF

IF glTrnsHd.cTrnStat = 'P'
  =gfModalGen("TRM02167B00000","DIALOG",;
     "Transaction"+'|'+"posted"+'|'+;
     lcPost_User+'|'+lcPost_Date+' '+lcPost_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData 
  loFormSet.lcBStamp  = DTOC(glTrnsHd.dAdd_Date) + glTrnsHd.cAdd_Time 

  loFormSet.ChangeMode('E')
  =gfObj_Lock(.F.) 

  RETURN .F.
ENDIF

IF glTrnsHd.cTrnStat $ 'HOEU'
  =gfModalGen("TRM02181B00000","DIALOG",;
  "Transaction"+'|'+;
   lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time+'|'+"Transaction")
  SCATTER FIELDS &lcScFields MEMO TO laData 
  loFormSet.lcBStamp  = DTOC(glTrnsHd.dAdd_Date) + glTrnsHd.cAdd_Time 
  =gfObj_Lock(.F.) 
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      FUNCTION: gfCpEdit
*!
*!**************************************************************************
*

FUNCTION gfCpEdit

IF RECNO() <= RECCOUNT()
  GOTO RECNO()  
ENDIF  


IF glTrnsHd.cTrnStat $ 'APV'

ELSE

  DO gfCpEdit IN AAS_WIN.EXE

ENDIF


************************************************************
*! Name      : lfShowDtl
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/28/2012
*! Purpose   : Run the detail screen
************************************************************
FUNCTION lfShowDtl
LPARAMETERS loFormSet

*- Call the screen
lcRunScx = lfGetScx("GL\GLTRDT.scx")
DO FORM (lcRunScx) WITH loFormSet
*- End of lfShowDtl.
************************************************************
*! Name      : lfAfterShowDtl
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/06/2012
*! Purpose   : Update screen fields After calling lfShowDtl function
************************************************************
FUNCTION lfAfterShowDtl
LPARAMETERS loFormSet

WITH loFormSet.Ariaform1
  .laData9.Value = loFormSet.laData[9]
  .laData10.Value = loFormSet.laData[10]
  .lnBalance.Value = loFormSet.laData[9]-loFormSet.laData[10]
  .laData16.Enabled = loFormSet.lcOthSt = 'ENABLE'
  IF !EMPTY(loFormSet.laData[8])
    .txtStatus.Value = loFormSet.laTStatus[AT(loFormSet.laData[8],'EOUHPA')]
  ENDIF 
  .cbHold.Value = loFormSet.cbHold
ENDWITH 
*- End of lfAfterShowDtl.


************************************************************
*! Name      : lfFormSetDetProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/29/2012
*! Purpose   : Set Form Det Prop
************************************************************
FUNCTION lfFormSetDetProp
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet
SELECT (loFormSet.lc_TmpTrDt)
WITH loDetFormSet.Ariaform1.grdTmpTrDt
  .RecordSource = loFormSet.lc_TmpTrDt
  .Column1.Header1.Caption = loFormset.lcAcsegDes
  .Column1.ControlSource = 'CACCTCODE'
  .Column2.Header1.Caption = 'Description'
  .Column2.ControlSource = "SUBSTR(LOOKUP(GLACCHAR.cAccnldes,cAcctcode,GLACCHAR.cAcctcode,'ACCTCODE'),1,24)"
  .Column3.Header1.Caption = 'Debit'
  .Column3.ControlSource = 'IIF(CDRORCR="D",NAMOUNT,0.00)'
  .Column4.Header1.Caption = 'Credit'
  .Column4.ControlSource = 'IIF(CDRORCR="C",NAMOUNT,0.00)'
  .ReadOnly = .T.
ENDWITH 

WITH loDetFormSet.Ariaform1
  .lcAcctcode.calledfromglmodule = .T.
  .lcAcctcode.KeyTextbox.ControlSource = 'Thisformset.loFormSet.lcAcctCode'
  .lcAcctDesc.ControlSource = 'Thisformset.loFormSet.lcAcctDesc'  && it was commented
  .lnDebit.ControlSource = 'Thisformset.loFormSet.lnDebit'
  .lnCredit.ControlSource = 'Thisformset.loFormSet.lnCredit'
  .lnDebit.InputMask = '999999999999.99'
  .lnCredit.InputMask = '999999999999.99'
  .lcTrdtexp.ControlSource = 'Thisformset.loFormSet.lcTrdtexp'
  .lcTrdtexp.MaxLength = FSIZE('CTRDTEXP','GLTRNSDT')
  .lcAcSegDes.Caption = ALLTRIM(loFormSet.lcAcSegDes)
ENDWITH 


*- Enable / Disable
WITH loDetFormSet.Ariaform1
  .pbNew.Enabled = loFormSet.lcNewSt = 'ENABLE' AND loFormSet.ActiveMode $ 'AE'
  .pbRem.Enabled = loFormSet.lcRemSt = 'ENABLE' AND loFormSet.ActiveMode $ 'AE'
  .pbAdj.Enabled = loFormSet.lcAdjSt = 'ENABLE' AND loFormSet.ActiveMode $ 'AE'
  .lcAcctcode.Enabled = loFormSet.lcAccSt = 'ENABLE' AND loFormSet.ActiveMode $ 'AE'
  *! B610266,1 HIA 03/06/2013 Aria4xp - I am not able to add a comment to the GL Entries [T20130207.0022][Start]
  *.lcTrdtexp.Enabled = loFormSet.lcDrCrSt = 'ENABLE' AND loFormSet.ActiveMode $ 'AE'
  *.lnDebit.Enabled = loFormSet.lcDrCrSt = 'ENABLE' AND loFormSet.ActiveMode $ 'AE'
  *.lnCredit.Enabled = loFormSet.lcDrCrSt = 'ENABLE' AND loFormSet.ActiveMode $ 'AE'
  .lcTrdtexp.Enabled = loFormSet.ActiveMode $ 'AE'
  .lnDebit.Enabled   = loFormSet.ActiveMode $ 'AE'
  .lnCredit.Enabled  = loFormSet.ActiveMode $ 'AE'
    
  *! B610266,1 HIA 03/06/2013 Aria4xp - I am not able to add a comment to the GL Entries [T20130207.0022][End]
  
ENDWITH   

*- refresh the browse
LOCAL lnRec
lnRec = RECNO(loFormSet.lc_TmpTrDt)
LOCATE
loDetFormSet.Ariaform1.grdTmpTrDt.Refresh()
TRY 
GOTO (lnRec) IN (loFormSet.lc_TmpTrDt)
CATCH
ENDTRY 
lfFormDetAfterRowColChange(loDetFormSet)
loDetFormSet.Ariaform1.grdTmpTrDt.Refresh()

*- End of lfFormSetDetProp.

************************************************************
*! Name      : lfFormDetRefresh
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/31/2012
*! Purpose   : Refresh
************************************************************
FUNCTION lfFormDetRefresh
PARAMETERS loDetFormSet
loFormSet = loDetFormSet.loFormSet
WITH loDetFormSet.Ariaform1
  .lnTranBalD.Value = loFormSet.lnTranBalD
  .lnTranBalC.Value = loFormSet.lnTranBalC
  .lnTotaldr.Value = loFormSet.lnTotaldr
  .lnTotalcr.Value = loFormSet.lnTotalcr
  .lcAcctcode.Enabled = loFormSet.lnTrLines>0 AND loFormSet.ActiveMode $ 'AE'
  .pbRem.Enabled = loFormSet.lnTrLines>0 AND loFormSet.ActiveMode $ 'AE'
ENDWITH 

*- End of lfFormDetRefresh.


************************************************************
*! Name      : lfFormDetAfterRowColChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/31/2012
*! Purpose   : Detail Form AfterRowColChange method
************************************************************
FUNCTION lfFormDetAfterRowColChange
PARAMETERS loDetFormSet
loformset = loDetFormSet.loformset 
WITH loDetFormSet.Ariaform1
  lc_TmpTrDt = loFormSet.lc_TmpTrDt
  .lcAcctcode.KeyTextbox.Value = &lc_TmpTrDt..CACCTCODE
  .lcAcctDesc.Value = SUBSTR(LOOKUP(GLACCHAR.cAccnldes,&lc_TmpTrDt..cAcctcode,GLACCHAR.cAcctcode,'ACCTCODE'),1,24)
  .lnDebit.Value = IIF(CDRORCR="D" ,&lc_TmpTrDt..NAMOUNT,0.00)
  .lnCredit.Value = IIF(CDRORCR="C",&lc_TmpTrDt..NAMOUNT,0.00)
  .lcTrdtexp.Value = &lc_TmpTrDt..CTRDTEXP
ENDWITH 
*- End of lfFormDetAfterRowColChange.

************************************************************
*! Name      : lfShowGet
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/31/2012
*! Purpose   : Replaces the SHOW GET command 
************************************************************
FUNCTION lfShowGet
PARAMETERS loFormSet,lcCnt,lcProp
loFormSet.Ariaform1.&lcCnt..Enabled = lcProp = 'ENABLE'
*- End of lfShowGet.
