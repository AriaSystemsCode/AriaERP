*:***************************************************************************
*: Program file  : ARFIXAR.PRG
*: Program desc. : Fix the worng files that related to ar problem.
*: System        : Aria Apparel System (A27).
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : ABDOU ELGENDI - (ABD) Due to B#606315,1
*: Date          : 12/24/2002
*:***************************************************************************
*: Calls :  
*:         Procedures : 
*:               -----------------------------------------------
*:          Functions : 
*:***************************************************************************
*: Passed Parameters  : None.
*:***************************************************************************
*: Notes   : .... 
*:***************************************************************************
*: Example : DO ARFIXAR
*:***************************************************************************
*:Modifications :
*:B606578,1 BWA 12/26/2002 Fix the bug of not calculating all the charges.
*:***************************************************************************

STORE .F. TO llHasInDat
IF !lfOpenFile()
  RETURN
ENDIF

*-- Give the user Message if he Sure to do that.
*-- Message Text : Are you sure you want to run the Fix Program now?
IF gfModalGen('INM00000B32000','F','ALERT',' ','Are you sure you want to run the check program now?') = 2
  RETURN
ENDIF

lcOldPath = FullPath('')
lcPath = GETDIR(lcOldPath,'Select check data file path')
SET DEFA TO &lcOldPath

lcFilHandl = FCREAT('&lcPath.Check.txt')
=FPUTS(lcFilHandl,REPLICATE('*',68))
=FPUTS(lcFilHandl,"*                  FILE VERIFICATION STATUS REPORT                  *")
=FPUTS(lcFilHandl,REPLICATE('*',68))
=FPUTS(lcFilHandl,"                   TEXT DESCRIPTION                                 *")
=FPUTS(lcFilHandl,REPLICATE('*',68))
=FPUTS(lcFilHandl,' ')
=FPUTS(lcFilHandl,' ')

= lfCheck()
=FCLOSE(lcFilHandl)

IF llHasInDat .AND. gfModalGen('INM00000B32000','F','ALERT',' ','The check procedure found incorrect data, Are you sure you want to Fix the data now?') = 1
  = lfFixData()
  WAIT CLEAR 
  *-- Give the user Message if he Sure to do that.
  *-- Message Text : 
  =gfModalGen('INM00000B00000','F','ALERT',' ','Fix process completed successfully.')
ELSE
  =gfModalGen('INM00000B00000','F','ALERT',' ','No problem detected.')
ENDIF

=lfClosFile()

*-- End OF Code
*:***************************************************************************
FUNCTION lfCheck
Private lnAlias ,lnAmount
lnAlias = SELECT (0)
lnAmount = 0

SELECT INVHDR
LOCATE

SCAN FOR STATUS # 'V'
  WAIT WINDOW 'Check Invoices # ' + Invoice NOWAIT

  *B606578,1 BWA 12/26/2002 Fix the bug of not calculating all the charges.[START]
  *lnAmount = shipamt+Discount+Tax_amt+ncharges
  lnAmount = shipamt+Freight+Insur+COD+Discount+Tax_amt+nPstAmt+ncharges+nHstAmt
  *B606578,1 BWA 12/26/2002.[END]
  
  IF totalChg # lnAmount
    =FPUTS(lcFilHandl,'Invoice # '+ Invoice+' has total charge ='+ALLTRIM(STR(totalChg))+' & should be ='+ALLTRIM(STR(lnAmount))+' .')
    llHasInDat = .T.

    *-- Check if found at the arhist file.
    IF SEEK(Account+Invoice,'ARHIST')
      =FPUTS(lcFilHandl,'You should reverse this keyed off invoice before the fix program can repair it.')
      =FPUTS(lcFilHandl,'This invoice has been keyed off in history batch #'+ARHIST.History+' .')
    ENDIF
    
  ENDIF
ENDSCAN

SELECT (lnAlias )
*- End OF lfCheck
*:***************************************************************************
FUNCTION lfFixData
PRIVATE lnAmount
lnAmount = 0 
lcGLBatch = ''

SELECT INVHDR
SCAN FOR STATUS # 'V'

  *B606578,1 BWA 12/26/2002 Fix the bug of not calculating all the charges.[START]
  *lnAmount = shipamt+Discount+Tax_amt+ncharges
  lnAmount = shipamt+Freight+Insur+COD+Discount+Tax_amt+nPstAmt+ncharges+nHstAmt
  *B606578,1 BWA 12/26/2002.[END]

  IF totalChg # lnAmount

    *-- Loop in invhdr in case this record found at arhst file.
    IF SEEK(Account+Invoice,'ARHIST')
      LOOP
    ENDIF
  
    WAIT WINDOW 'Update Invoices # ' + Invoice  NOWAIT
    
    *-- Update the invoice header file.
    REPLACE totalChg WITH lnAmount
    
    *-- Update the Debit file
    IF SEEK(Account+Invoice,'DEBIT')
      SELECT DEBIT
      REPLACE amount WITH lnAmount
    ELSE
      *-- Update the arhist file.
      IF SEEK(Account+Invoice,'ARHIST')
        SELECT ARHIST
        REPLACE amount WITH lnAmount
      ENDIF
    ENDIF
       
    IF SEEK(InvHdr.Invoice + "IN",'GLDist')
      SELECT GLDist
      lcGLBatch = GLDist.GLBatch
      SCAN REST WHILE Tran_no + Tran_Type + GLSession + Catg_Key = InvHdr.Invoice + "IN"
        REPLACE Posted     WITH ''  ,;
                Printed    WITH .F. ,;
                glbatch    WITH ''  ,;
                ctrnsledn  WITH ''  ,; 
                nEntryNo   WITH 0   ,;
                glComp     WITH ''
        IF Catg_Key = '001'
          lnEqAmt   = 0
          lcUntSin  = ''
          lcExRSin  = gfGetExSin(@lcUntSin,GLDIST.cCurrCode)
          lnEqAmt   = ROUND(lnAmount &lcExRSin Gldist.nExRate &lcUntSin Gldist.nCurrUnit,2)
          REPLACE nglamount WITH lnAmount,;
                  nEqvAmnt  WITH lnEqAmt
        ENDIF     
      ENDSCAN   
    ENDIF
    
    *-- Sameh
    IF SEEK(InvHdr.Invoice + "IN",'GLDist')
      lcOldSet = SET('DELETE')
      SET DELETE ON
      IF !EMPTY(lcGLBatch) AND SEEK(lcGLBatch,'GLBatch')
    
        SELECT GLBatch
        DELETE REST WHILE cBatchNo = lcGLBatch
    
        IF SEEK(lcGLBatch,'GLTrnsDt')
          SELECT GLTrnsDt
          DELETE REST WHILE cBatchNo + cTranNo = lcGLBatch
        ENDIF

        IF SEEK(lcGLBatch,'GLTrnsHd')
          SELECT GLTrnsHd
          DELETE REST WHILE cBatchNo + cTranNo = lcGLBatch        
        ENDIF

        IF SEEK(lcGLBatch,'GLPTrnDt')
          SELECT GLPTrnDt
          DELETE REST WHILE cBatchNo + cTranNo = lcGLBatch
        ENDIF

        IF SEEK(lcGLBatch,'GLPTrnHd')
          SELECT GLPTrnHd
          DELETE REST WHILE cBatchNo + cTranNo = lcGLBatch        
        ENDIF
      ENDIF
      
      *-- Update the gldist file.
      
      SELECT GLDist
      SCAN FOR !EMPTY(GLBatch) .AND. GLBatch = lcGLBatch
        WAIT WINDOW 'Batch # ' + GLBatch NOWAIT
        REPLACE Posted    WITH '' , ;
                Printed   WITH .F. ,;
                GLBatch   WITH '' , ;
                cTrnsledn WITH '' , ;
                nEntryNo  WITH 0 , ;
                GLComp    WITH ''    
      ENDSCAN

      
      
      SET DELETE &lcOldSet
    ENDIF
    *- Sameh
  ENDIF
ENDSCAN


*- End Of lfFixData.
*:***************************************************************************
FUNCTION lfOpenFile
PRIVATE lReturnFlg
lReturnFlg =.T.

llReturn1 =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
llReturn2 =gfOpenFile(gcDataDir+'DEBIT',gcDataDir+'DEBIT','SH')

llReturn3 =gfOpenFile(gcDataDir+'GLDIST',gcDataDir+'GLDistNo','SH')
llReturn4 =gfOpenFile(gcDataDir+'ARHIST',gcDataDir+'ARHISTT','SH')

llReturn5 =gfOpenFile(gcDataDir+'GLBatch',gcDataDir+'BatchNo','SH')
llReturn6 =gfOpenFile(gcDataDir+'GlTrnsHd',gcDataDir+'BatchTrn','SH')

llReturn7 =gfOpenFile(gcDataDir+'GLTrnsDt',gcDataDir+'BatchTrn','SH')

llReturn8 =gfOpenFile(gcDataDir+'GLPTrnHd',gcDataDir+'BatchTrn','SH')
llReturn9 =gfOpenFile(gcDataDir+'GLPTrnDt',gcDataDir+'BatchTrn','SH')





IF !llReturn1 .OR. !llReturn2 .OR. !llReturn3 .OR. !llReturn4 .OR. ;
   !llReturn5 .OR. !llReturn6 .OR. !llReturn7 .OR. !llReturn8 .OR. ;
   !llReturn9
   lReturnFlg = .F.
ENDIF
 
RETURN lReturnFlg
*-- End Of lfOpenFile
*:***************************************************************************
FUNCTION lfClosFile

=gfCloseFile('INVHDR')
=gfCloseFile('DEBIT')

=gfCloseFile('GLDIST')
=gfCloseFile('ARHIST')

=gfCloseFile('GLBatch')
=gfCloseFile('GlTrnsHd')

=gfCloseFile('GLTrnsDt')
=gfCloseFile('GLPTrnHd')
=gfCloseFile('GLPTrnDt')

*-- End Of lfOpenFile
*:***************************************************************************