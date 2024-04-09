*:**************************************************************************
*: Name        : GLIMPGL.PRG  
*: TITLE       : Importing GL journal entries to Aria27   
*: ENTRY       : C102632
*: System      : Aria Apparel System (A27).
*: Module      : General Entries (GL)
*: Developer   : ASH - Ashraf Sherif Mohammad
*: Date        : 06/30/2002
*:***************************************************************************
*: Calls : FUNCTIONS  : lfTxtFile()
*:                    : lfvOK()
*:                    : lfOpenFile()
*:                    : lfEscap()
*:                    : lfVPrnt()
*:***************************************************************************

lcTxtFile  = SPACE(25)
ON KEY LABEL ESC DO lfEscap
DO (gcScrDir + 'GL\GLIMPGL.SPR')
ON KEY LABEL ESC


*!*************************************************************
*! Name      : lfEscap
*! Developer : Ashraf Sherif (ASH)
*! Date      : 06/30/2002
*! Purpose   : Traping of Esacp
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfEscap()
*!*************************************************************
*!
FUNCTION lfEscap

CLEAR READ


FUNCTION lfGetFile

lcTxtFile =GetFile('TXT','Select the text file.')

=lfTxtFile()

SHOW GET lcTxtFile

*!*************************************************************
*! Name      : lfTxtFile
*! Developer : Ashraf Sherif (ASH)
*! Date      : 06/30/2002
*! Purpose   : Valid Function for the Text file.
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfTxtFile()
*!*************************************************************
*!

FUNCTION lfTxtFile

IF !EMPTY(lcTxtFile)
  IF ATC('.TXT',lcTxtFile) = 0 
    IF ATC('.',lcTxtFile) <> 0 
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'You have to select a text file.')
      lcTxtFile = ""
      SHOW GET lcTxtFile
      RETURN
    ELSE
      lcTxtFile = ALLTRIM(lcTxtFile) + '.TXT'
    ENDIF
  ENDIF
  IF !FILE(lcTxtFile)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'This file does not exist.') 
    lcTxtFile = ""
  ENDIF
  SHOW GET lcTxtFile
ENDIF

*!*************************************************************
*! Name      : lfvOK
*! Developer : Ashraf Sherif (ASH)
*! Date      : 06/30/2002
*! Purpose   : Valid for Ok button. (Start to import data)
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   : lfvOK()
*!*************************************************************
*!

FUNCTION lfvOK

IF EMPTY(lcTxtFile)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,;
              'You must select a text file.')  
  RETURN              
ENDIF

=lfOpenFile()

lcGLDTTmp = gfTempName()
lcGLHDTmp = gfTempName()

CREATE CURSOR (lcGLHDTmp) (cTranNo C(8), dTrnpDate D, cTrnPYr C(4), cTrnPPrd C(2), nBal N(12,2), cSrcJrnl C(2), cOrgTrN C(8))
INDEX ON cTranNo TAG (lcGLHDTmp) OF (lcGLHDTmp)


CREATE CURSOR (lcGLDTTmp) (cBatchNo C(6), cTranNo C(8), cAcctCode C(24), cTrdtExp C(40), cDrOrCr C(1), nAmount N(15,2), dTrnpDate D)
INDEX ON cTranNo TAG (lcGLDTTmp) OF (lcGLDTTmp)

lnFileHNDL = ""
lnFileHNDL = FCREAT('&gcWorkDir.ErrReprt.txt')
CREATE CURSOR lcMemFile (FMemo M(10))
APPEND BLANK
APPEND MEMO FMemo FROM &lcTxtFile
SET MEMOWIDTH TO 600

STORE '' TO lcYear,lcPrd,lcTranNo,lcOldPrd
STORE {} TO ldBegin,ldEnd
llCont = .T.

FOR lnI = 1 TO MEMLINES(lcMemFile.FMEMO)
  STORE 0  TO lnPos, lnAmount
  STORE '' TO lcLine, lcAccCode, lcSign, lcComment
  STORE {} TO ldDate
  
  lcLine = MLINE(lcMemFile.FMEMO,lnI)
  IF ATC('*',lcLine,1) = 0
    LOOP
  ENDIF
  
  **- Transaction number
  lcOldTr = lcTranNo 
  lnPos = ATC('*',lcLine,1)
  IF lnPos > 1
    lcTranNo = SUBSTR(lcLine,1,lnPos-1)   
    lcTranNo = PADL(lcTranNo,8,'0')
  ENDIF
  
  ** Journal Code
  lcJrCode = SUBSTR(lcLine,lnPos+1,ATC('*',lcLine,2)-(lnPos+1))
  IF !SEEK(lcJrCode,'GLSubJor')
    =FPUTS(lnFileHNDL,'Transaction : '+ lcTranNo + ' will not be imported because Journal code in line '+ALLTRIM(STR(lnI))+' is invalid.')
    llCont = .F.
  ENDIF
  
  ** Account Code
  lnPos = ATC('*',lcLine,2)
  IF lnPos > 1
    lcAccCode = SUBSTR(lcLine,lnPos+1,ATC('*',lcLine,3)-(lnPos+1))
    =SEEK(lcAccCode,'GLAcchar')
    IF EOF('GLAcchar') OR GLAcchar.cSegActiv <> 'A' OR GLAcchar.cSegaLPos <> 'Y'
      =FPUTS(lnFileHNDL,'Transaction : '+ lcTranNo + ' will not be imported because Account code in line '+ALLTRIM(STR(lnI))+' is invalid.')
      llCont = .F.
    ENDIF
  ENDIF
  
  ** Sign
  lnPos = ATC('*',lcLine,3)
  IF lnPos > 1
    lcSign = SUBSTR(lcLine,lnPos+1,ATC('*',lcLine,4)-(lnPos+1))
    lcSign = IIF(lcSign='+','D','C')
  ENDIF
  
  ** Amount
  lnPos = ATC('*',lcLine,4)
  IF lnPos > 1
    lnAmount = SUBSTR(lcLine,lnPos+1,ATC('*',lcLine,5)-(lnPos+1))
  ENDIF
  
  ** Comments
  lnPos = ATC('*',lcLine,5)
  IF lnPos > 1
    lcComment = SUBSTR(lcLine,lnPos+1,ATC('*',lcLine,6)-(lnPos+1))
  ENDIF
 
  ** Date
  lnPos = ATC('*',lcLine,6)
  IF lnPos > 1
    
    ldDate = CTOD(SUBSTR(lcLine,lnPos+1,ATC('*',lcLine,7)-(lnPos+1)))
    SELECT FSPRD
    SCAN
      IF BETWEEN(ldDate,dFsppBgDt,dFsppEnDt)
        ldBegin = dFsppBgDt
        ldEnd   = dFsppEnDt
        lcYear  = cFisfYear
        lcPrd   = cFspPrdId
        EXIT
      ENDIF
    ENDSCAN
    
    IF !EMPTY(lcOldPrd) OR EMPTY(ldDate)
      IF lcPRd <> lcOldPrd OR EMPTY(ldDate)
        =FPUTS(lnFileHNDL,'Transaction : '+ lcTranNo + ' will not be imported because line '+ALLTRIM(STR(lnI))+' has different Period.') 
        llCont = .F.
      ELSE
        lcOldPrd = lcPrd
      ENDIF
    ELSE
      lcOldPrd = lcPrd    
    ENDIF
  ENDIF

  SELECT (lcGLDTTmp)
  APPEND BLANK
  REPLACE cTranNo    WITH lcTranNo   ,;
          cAcctCode  WITH lcAccCode  ,;
          cTrdtExp   WITH lcComment  ,;
          cDrOrCr    WITH lcSign     ,;
          nAmount    WITH VAL(lnAmount) ,;
          dTrnpDate  WITH ldDate     
 
  SELECT (lcGLHDTmp)
  IF !SEEK(lcTranNo)
    APPEND BLANK
    REPLACE cTranNo    WITH lcTranNo   ,;
            dTrnpDate  WITH ldDate     ,;             
            cTrnPYr    WITH lcYear     ,;
            cTrnPPrd   WITH lcPrd      ,;
            cSrcJrnl   WITH lcJrCode   
  ENDIF
  REPLACE nBal WITH nBal + IIF(&lcGLDTTmp..cDrOrCr='D',VAL(lnAmount),VAL(lnAmount)*-1)

ENDFOR

STORE 0 TO lnTotCr, lnTotDb     && Total debit & credit amount to be saved in the batch file.
SELECT (lcGLHDTmp)
GO TOP

** Generate Batch Number
IF !EOF() AND llCont
  lcGLBatch = gfSequence('CBATCHNO')
ELSE
  llCont = .F.  
ENDIF

SCAN 
  IF nBal = 0 AND llCont
    *ash1 begin
    lcTrn = gfSequence('CTRANNO')
    REPLACE cOrgTrN WITH lcTrn 
    *ash1 end
    SELECT (lcGLDTTmp)
    =SEEK(&lcGLHDTmp..cTranNo)
    SCAN WHILE cTranNo = &lcGLHDTmp..cTranNo
      SCATTER MEMVAR
      SELECT GLTrnsDT
      APPEND BLANK
      GATHER MEMVAR
      REPLACE cBatchNo   WITH lcGLBatch  ,;
              cTranNo    WITH &lcGLHDTmp..cOrgTrN ,;
              nEntryNo   WITH 0          ,;
              cTrnPYr    WITH &lcGLHDTmp..cTrnPYr,;
              cTrnPPrd   WITH &lcGLHDTmp..cTrnPPrd ,;
              cAdd_User  WITH 'ARIA'     ,;
              dAdd_Date  WITH DATE()     ,;
              cAdd_Time  WITH TIME()
  
      SELECT GLTrnsHD
      IF !SEEK(lcGLBatch+&lcGLHDTmp..cOrgTrN)          
        APPEND BLANK
        REPLACE cBatchNo   WITH lcGLBatch  ,;
                cTranNo    WITH &lcGLHDTmp..cOrgTrN ,;
                cTrnDesc   WITH 'Created by GL Module' ,;
                cTrnRefer  WITH 'On ' + DTOC(DATE()) ,;
                dTrnpDate  WITH &lcGLHDTmp..dTrnpDate  ,;             
                cTrnStat   WITH 'U'        ,;
                cTrnType   WITH 'N'        ,;     
                cTrnReVer  WITH 'N'        ,;     
                cSrcModul  WITH 'GL'       ,;
                cTrnPYr    WITH &lcGLHDTmp..cTrnPYr    ,;
                cTrnPPrd   WITH &lcGLHDTmp..cTrnPPrd   ,;
                cSrcJrnl   WITH &lcGLHDTmp..cSrcJrnl   ,;
                cComp_Id   WITH gcAct_Comp ,;
                cAdd_User  WITH 'ARIA'     ,;     
                dAdd_Date  WITH DATE()     ,;
                cAdd_Time  WITH TIME()     ,;
                lLok_Stat  WITH .F.
   
      ENDIF
  
      IF GLTrnsDT.cDrOrCr = 'D'
        REPLACE nTrnTotDr WITH nTrnTotDr + GLTrnsDT.nAmount
        lnTotDb = lnTotDb + GLTrnsDT.nAmount
      ELSE
        REPLACE nTrnTotCr WITH nTrnTotCr + GLTrnsDT.nAmount
        lnTotCr = lnTotCr + GLTrnsDT.nAmount
      ENDIF
    ENDSCAN
  ELSE
    IF llCont
      llCont = .F.
      =FPUTS(lnFileHNDL,'This Batch is not Balanced.')
    ENDIF
  ENDIF
ENDSCAN

IF llCont
  SELECT GLBATCH
  APPEND BLANK
  REPLACE cBatchNo   WITH lcGLBatch  ,;
          cBatStat   WITH 'U'        ,;
          cBatType   WITH 'N'        ,;     
          cBatPYr    WITH lcYear     ,;
          dBatPBeg   WITH ldBegin    ,;
          dBatPEnd   WITH ldEnd      ,;
          cBatRefer  WITH 'On ' + DTOC(DATE()) ,;
          cBatDesc   WITH 'Created by Aria' ,;
          nBaTotDr   WITH lnTotDb    ,;
          nBaTotCr   WITH lnTotCr    ,;
          nBatcnTot  WITH 0          ,;
          cSrcModul  WITH 'GL'       ,;
          cComp_id   WITH gcAct_Comp ,;
          cAdd_user  WITH 'ARIA'     ,;
          dAdd_Date  WITH DATE()     ,;
          cAdd_Time  WITH TIME()     ,;
          lLok_Stat  WITH .F.        ,;
          cEdit_User WITH 'ARIA'     ,;
          dEdit_Date WITH DATE()     ,;
          cEdit_Time WITH TIME()     
ENDIF

=FFLUSH(lnFileHNDL)
*-- Close the text file
DO WHILE !FCLOSE(lnFileHNDL)
ENDDO
CREATE CURSOR TMPSTR (mStrRep M(10))
APPEND BLANK
APPEND MEMO mStrRep FROM (gcWorkDir+"ErrReprt.txt") OVERWRITE
lcWinTitl  = "Error Report"
IF !EMPTY(mStrRep)
  DO (gcScrDir + 'SM\SMSTRREP.SPR')
ELSE
  IF llCont
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Batch # '+ lcGLBatch + ' has been generated successfully.') 
  ELSE
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Failed to import GL Transactions.') 
  ENDIF
ENDIF  
USE IN TMPSTR
lcTxtFile = ""
SHOW GET lcTxtFile

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Ashraf Sherif (ASH)
*! Date      : 06/30/2002
*! Purpose   : Open needed files.
*!*************************************************************
FUNCTION lfOpenFile

= gfOpenFile(gcDataDir+'GLBATCH',gcDataDir+'Batchno','SH')
= gfOpenFile(gcDataDir+'GLTRNSHD',gcDataDir+'Batchtrn','SH')
= gfOpenFile(gcDataDir+'GLTRNSDT',gcDataDir+'Batchtrn','SH')
= gfOpenFile(gcDataDir+'GLSubJor',gcDataDir+'Srcjrnl','SH')
= gfOpenFile(gcDataDir+'GLAcchar',gcDataDir+'Acctcode','SH')
= gfOpenFile(gcDataDir+'FSPRD','','SH')



*!*************************************************************
*! Name      : lfVPrnt
*! Developer : Ashraf Sherif (ASH)
*! Date      : 06/30/2002
*! Purpose   : Print the error log.
*!*************************************************************

FUNCTION lfVPrnt
IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*!*************************************************************
*! Name      : lfDel
*! Developer : Ashraf Sherif (ASH)
*! Date      : 06/30/2002
*! Purpose   : Delete the UnImported transactions from the temp. files.
*!*************************************************************

FUNCTION lfDel

SELECT (lcGLDTTmp)
DELETE ALL FOR cTranNo = lcTranNo 
SELECT (lcGLHDTmp)
DELETE ALL FOR cTranNo = lcTranNo 
