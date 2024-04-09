*:************************************************************
*: Program file  : ICFXSJL.PRG
*: Program desc. : Fix for STYINVJL.DBF file 
*:        System : Aria Advantage Series.
*:        Module : INVENTORY CONTROL (IC).
*:     Developer : tarek Mohammed Ibrahim TMI
*:     Entry     : B606649,1
*:************************************************************

lnRespns = gfModalGen('QRM00000B00006','DIALOG',.F.,.F.,;
                      'This program will update STYINVJL file, Are you sure to continue?')
IF lnRespns # 1
  RETURN
ENDIF

*--Open needed files
=gfOpenFile(gcDataDir+'RETHDR','RETHDR','SH')
=gfOpenFile(gcDataDir+'RETLINE','RETLINE','SH')
=gfOpenFile(gcDataDir+'INVHDR','INVHDR','SH')
=gfOpenFile(gcDataDir+'INVLINE','INVLINE','SH')
=gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
=gfOpenFile(gcDataDir+'STYINVJL','MFGOPR','SH')

lcTempName = gfTempName()
lcFilHandl = FCREATE(gcWorkDir+lcTempName+'.txt')
lcLine     = '--------------------------------------------------------------'
lcWriteStr = 'STYLE            WARECODE  TRAN.DATE  TRAN.TYPE  TRAN.# TOTQTY'
=FPUTS(lcFilHandl,lcWriteStr)
=FPUTS(lcFilHandl,lcLine)

SELECT STYINVJL
GO TOP

STORE .F. TO llRet,llInv

*-- Fix Credit memos  *:type '7' (+) 

SELECT RETHDR
GO TOP

SCAN 
  WAIT WINDOW NOWAIT 'Checking Credit memos..'
  IF !SEEK(RETHDR.CRMEMO,'STYINVJL') AND SEEK(RETHDR.CRMEMO,'RETLINE')
    lcRISessn = gfSequence('GLSESSION')    
    =SEEK('M'+RETHDR.ACCOUNT,'CUSTOMER')
    SELECT RETLINE
    WAIT WINDOW NOWAIT 'Updating STYINVJL file with Credit Memo #:'+RETHDR.CRMEMO
    
    SCAN REST WHILE RETLINE.CRMEMO = RETHDR.CRMEMO      
      SELECT STYINVJL
      APPEND BLANK
      REPLACE cSession   WITH lcRISessn          ,;
              Style      WITH RETLINE.STYLE      ,;
              cWareCode  WITH RETHDR.CWARECODE   ,;
              cDyelot    WITH RETLINE.DYELOT     ,;
              dTrDate    WITH RETLINE.CRDATE     ,;
              cTrType    WITH '7'                ,;
              cTrCode    WITH RETHDR.CRMEMO      ,;
              nCost      WITH RETLINE.COST       ,;
              cIRType    WITH 'R'                ,;
              nStk1      WITH RETLINE.QTY1       ,;
              nStk2      WITH RETLINE.QTY2       ,;
              nStk3      WITH RETLINE.QTY3       ,;
              nStk4      WITH RETLINE.QTY4       ,;
              nStk5      WITH RETLINE.QTY5       ,;
              nStk6      WITH RETLINE.QTY6       ,;
              nStk7      WITH RETLINE.QTY7       ,;
              nStk8      WITH RETLINE.QTY8       ,;
              nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
              nStkVal    WITH nTotStk*nCost      ,;                               
              Reference  WITH 'CUST# '+ Customer.Account + "-" + Customer.BTName,;              
              lLockFlg   WITH .F.                ,;                            
              cAdjReason WITH ' '                ,;
              cAdjAcct   WITH ' '                ,;
              cISession  WITH ' '                ,;
              cRSession  WITH cSession           ,;
              LineNo     WITH VAL(RETLINE.CRET_LINNO),;
              nPrvSQty   WITH 0                  ,;
              nPrvSVal   WITH 0
              
      =gfAdd_Info('STYINVJL')
      llRet = .T.      
      
      SCATTER MEMVAR 
      lcWriteStr = M.STYLE+' '+M.CWARECODE+' '+DTOC(M.DTRDATE)+' '+;
                   IIF(M.CTRTYPE='3','Invoice    #','')+IIF(M.CTRTYPE='7','Credit Memo#','')+;
                   M.cTrCode+' '+STR(M.nTotStk,5)                               
      =FPUTS(lcFilHandl,lcWriteStr)
    ENDSCAN
  ENDIF
ENDSCAN
=FPUTS(lcFilHandl,"")

*<<<:***************************************:>>>*

*-- Fix Invoices  *:type '3' (-) , '4' (+)
SELECT INVHDR
GO TOP

SCAN FOR !EMPTY(INVHDR.STATUS)
  WAIT WINDOW NOWAIT 'Checking Invoices..'
  IF !SEEK(INVHDR.INVOICE,'STYINVJL')
    IF SEEK(INVHDR.INVOICE,'INVLINE')
      
      lcRISessn1 = gfSequence('GLSESSION')    
      IF INVHDR.STATUS = 'V'
        lcRISessn2 = gfSequence('GLSESSION')    
      ENDIF

      =SEEK('M'+RETHDR.ACCOUNT,'CUSTOMER')
      WAIT WINDOW NOWAIT 'Updating STYINVJL file with Invoice #:'+INVHDR.INVOICE
      SELECT INVLINE
      SCAN REST WHILE INVLINE.INVOICE = INVHDR.INVOICE
        SELECT STYINVJL
        FOR lnJ = 1 TO IIF(INVHDR.STATUS='C',1,2)
          lcX = STR(lnJ,1)
          APPEND BLANK
          REPLACE cSession   WITH lcRISessn&lcX,;
                  Style      WITH INVLINE.STYLE,;
                  cWareCode  WITH INVHDR.CWARECODE,;
                  cDyelot    WITH INVLINE.DYELOT,;
                  dTrDate    WITH INVHDR.INVDATE,;
                  cTrType    WITH STR(2+lnJ,1),;
                  cTrCode    WITH INVHDR.INVOICE,; 
                  nCost      WITH INVLINE.COST,;
                  cIRType    WITH IIF(lnJ=1,'I','R') ,;
                  nStk1      WITH IIF(lnJ=1,-1,1)*INVLINE.QTY1,;
                  nStk2      WITH IIF(lnJ=1,-1,1)*INVLINE.QTY2,;
                  nStk3      WITH IIF(lnJ=1,-1,1)*INVLINE.QTY3,;
                  nStk4      WITH IIF(lnJ=1,-1,1)*INVLINE.QTY4,;
                  nStk5      WITH IIF(lnJ=1,-1,1)*INVLINE.QTY5,;
                  nStk6      WITH IIF(lnJ=1,-1,1)*INVLINE.QTY6,;
                  nStk7      WITH IIF(lnJ=1,-1,1)*INVLINE.QTY7,;
                  nStk8      WITH IIF(lnJ=1,-1,1)*INVLINE.QTY8,;
                  nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
                  nStkVal    WITH nTotStk*nCost      

          REPLACE Reference  WITH 'CUST# '+ INVHDR.Account + " Sales order " + INVHDR.Order ,;
                  lLockFlg   WITH .F.                ,;              
                  cAdjReason WITH ' '                ,;
                  cAdjAcct   WITH ' '                ,;
                  cISession  WITH IIF(lnJ=1,cSession,' '),;
                  cRSession  WITH IIF(lnJ=2,cSession,' '),;
                  LineNo     WITH INVLINE.LINENO     ,;
                  nPrvSQty   WITH 0                  ,;
                  nPrvSVal   WITH 0
         
          =gfAdd_Info('STYINVJL')
          llInv = .T.

          SCATTER MEMVAR 
          lcWriteStr = M.STYLE+' '+M.CWARECODE+' '+DTOC(M.DTRDATE)+' '+;
                       IIF(M.CTRTYPE$'3','Invoice    #','')+;
                       IIF(M.CTRTYPE$'4','Void Inv   #','')+;
                       IIF(M.CTRTYPE='7','Credit Memo#','')+;
                       M.cTrCode+' '+STR(M.nTotStk,5)
          =FPUTS(lcFilHandl,lcWriteStr)

        ENDFOR        
      ENDSCAN
    ENDIF
    
  ENDIF
ENDSCAN

WAIT CLEAR
=FPUTS(lcFilHandl,lcLine)

IF llInv OR llRet
  lnResp = gfModalGen('QRM00000B00006','DIALOG',.F.,.F.,'STYINVJL updated successfully. Do you want to print the added records?')
  IF lnResp = 1
    =lfViewRLog()
  ENDIF
ELSE
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'No lines added to the file STYINVJL.')
ENDIF
=FCLOSE(lcFilHandl)
ERASE (gcWorkDir+lcTempName+'.txt')

*:**************************************************************************
*:* Name        : lfViewRLog
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/18/2002
*:* Purpose     : Show a log screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfViewRLog()
*:***************************************************************************
FUNCTION lfViewRLog

=FFLUSH(lcFilHandl)
DO WHILE !FCLOSE(lcFilHandl)
ENDDO
lcWinTitl = 'Show styles added to STYINVJL file'
CREATE CURSOR TMPSTR (mStrRep M(10))
APPEND BLANK
APPEND MEMO mStrRep FROM (gcWorkDir+lcTempName+".txt") OVERWRITE
DO (gcScrDir + 'SM\SMSTRREP.SPR')
USE IN TMPSTR

*:**************************************************************************
*:* Name        : lfvPrnt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/18/2002
*:* Purpose     : print 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvPrnt()
*:***************************************************************************
FUNCTION lfvPrnt

IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'  
ENDIF
SET DEVICE TO SCREEN

*-- end of lfvPrnt.0