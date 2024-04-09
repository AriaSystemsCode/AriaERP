*:*********************************************************************************
*: Program file  : EBSNDCN.PRG
*: Program desc. : Send Order Confirmation Request to CIT
*: For screen    : Menu
*:        System : Aria4 XP.
*:        Module : Accounts Receivable (AR).
*:     Developer : Waleed Hamed (WLD)
*:     Entry     : C#101915 convert from aria27 to Aria4
*:*********************************************************************************
*: Passed Parameters  : None
*:*********************************************************************************
*: Modifications :
*:*********************************************************************************
#INCLUDE R:\ARIA4XP\PRGS\AR\EBSNDCN.H

DO FORM oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+ "\EBSNDCN.SCX"

*!*************************************************************
*! Name      : lfvProceed
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Validate Proceed
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfvProceed()
*!*************************************************************
FUNCTION lfvProceed
LPARAMETERS loFormSet
lcOutFile = loFormSet.lcOutFile

*-- Client Information
lcClientID = SycFact.cClientID
lcClientNo = SycFact.cClientNo
lcBatchId  = 'HC' + lcClientNo 
lcPassWord = SycFact.cPassWord

IF EMPTY(lcClientID) .OR. EMPTY(lcClientNo) .OR. EMPTY(lcPassWord)
  =gfModalGen('TRM00000B00000','ALERT','','',LANG_MAVMREF_FACINFO)
  RETURN
ENDIF
*-- Check if the output file allready exists.
IF FILE(lcOutFile)
  IF gfModalGen('QRM00000B00006','ALERT','','',LANG_MAVMREF_OUTFILE + lcOutFile + LANG_MAVMREF_EXIST) = 2
    RETURN
  ENDIF
ENDIF
*-- Open the output file
lnOutFile = FCREATE(lcOutFile,0)
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','',LANG_MAVMREF_NOPROCEED)
  RETURN
ENDIF

lcSegLine = '$$REQUEST'
*-- Client ID
lcSegLine = lcSegLine + SPACE(1) + 'ID=' + SUBSTR(lcClientID,1,6)
*-- BATCH ID
lcSegLine = lcSegLine + SPACE(1) + "BATCHID='" + SUBSTR(lcBatchId,1,6)+"'"
*-- Password
lcSegLine = lcSegLine + SPACE(1) + "PASSWORD=" + SUBSTR(lcPassWord,1,4)
= FPUTS(lnOutFile,lcSegLine)

=FCLOSE(lnOutFile)
CLEAR READ
=gfModalGen('TRM00000B00000','ALERT','','',LANG_MAVMREF_REQFILE + lcOutFile + LANG_MAVMREF_CREATED)

*!*************************************************************
*! Name      : lfvFactor
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Validate Factors
*!*************************************************************
*! Calls     : ARIABROW,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfvFactor()
*!*************************************************************
FUNCTION lfvFactor
LPARAMETERS loFormSet, llBrowse

PRIVATE lcFactor, lnAlias
lcFactor = loFormSet.AriaForm1.kbFactor.KeyTextBox.Value

IF llBrowse OR (!EMPTY(lcFactor) .AND. !GFSEEK(lcFactor,'SycFact'))
  lcBrFields = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  lcFactor = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,'','',;
                          'cFacCode','laBrowArr',.F., .F., .F., 'SycFact'),SycFact.cFacCode,SPACE(6))
ENDIF

IF EMPTY(lcFactor)
  loFormSet.AriaForm1.cmdProceed.Enabled = .F.
ELSE
  loFormSet.AriaForm1.cmdProceed.Enabled = .T.
ENDIF

loFormSet.AriaForm1.txtFactName.Value = IIF(!EMPTY(lcFactor),SycFact.cFacComp,loFormSet.AriaForm1.txtFactName.Value)

RETURN !EMPTY(lcFactor)
