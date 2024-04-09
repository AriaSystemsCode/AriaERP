*!*************************************************************
*! Name      : EBSNDCN
*! Developer : Wael Ali Mohamed
*! Date      : 30/12/1999
*! Purpose   : Send Outbound Order Confirmation Request to CIT
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************

STORE .F. TO llBrowse
lcWindTitl = 'Order Confirmation Request Transmission'
=gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')

*-- Initialize variables
lcFactor   = SPACE(6)
lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
lcProceed  = gcBmpHome + "proceed.bmp"
lcClose    = gcBmpHome + "Close2.bmp"

lcOutFile = 'CIT'+PADL(MONTH(gdSysDate),2,'0')+PADL(DAY(gdSysDate),2,'0')+'.NEW'
DO (gcScrDir+gcWinAppl+"\EBSNDCN.SPX")

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

*-- Client Information
lcClientID = SycFact.cClientID
lcClientNo = SycFact.cClientNo
lcBatchId  = 'HC' + lcClientNo 
lcPassWord = SycFact.cPassWord

IF EMPTY(lcClientID) .OR. EMPTY(lcClientNo) .OR. EMPTY(lcPassWord)
  =gfModalGen('TRM00000B00000','ALERT','','','Factor information not complete. Cannot proceed.')
  RETURN
ENDIF
*-- Check if the output file allready exists.
IF FILE(lcOutFile)
  IF gfModalGen('QRM00000B00006','ALERT','','','Output file '+lcOutFile+' already exist. Overwrite it?') = 2
    RETURN
  ENDIF
ENDIF
*-- Open the output file
lnOutFile = FCREATE(lcOutFile,0)
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot open output file. Cannot proceed.')
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
=gfModalGen('TRM00000B00000','ALERT','','','Output order confirmation request file <'+lcOutFile+'> has been created.')

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
PRIVATE lnAlias

IF llBrowse OR (!EMPTY(lcFactor) .AND. !SEEK(lcFactor,'SycFact'))
  lcBrFields = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  lcFactor = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,'','',;
                          'cFacCode','laBrowArr'),SycFact.cFacCode,SPACE(6))
ENDIF
IF EMPTY(lcFactor)
  SHOW GET pbProceed DISABLE
ELSE
  SHOW GET pbProceed ENABLE
ENDIF
=lfRefresh('EBSNDCN')
llBrowse = .F.
