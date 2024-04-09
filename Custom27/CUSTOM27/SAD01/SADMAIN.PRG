*!**************************************************************************
*! Name      : SADMAIN.PRG
*! Developer : Nader Anis (NAD)
*! Date      : 06/30/2001
*! Purpose   : Sadimara Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C101796,1
*!**************************************************************************
*C200218,1 BWA 30/08/2001 Modify the custom invoice for sademara.
*C200234,1 ADEL 09/23/01 SAD10 wants to add piktkts to Pre_build orders.
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.


*!**************************************************************************
*! Name      : lfSadSav
*! Developer : Nader Anis (NAD)
*! Date      : 30/06/2001
*! Purpose   : 
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************

FUNCTION lfSadSav

lnInvCount = 1 
SELECT (lcInvHdr)
SCAN
  SCATTER MEMVAR MEMO
  M.NPICKED=&lcInvHdr..Picked
  INSERT INTO (lcSadInvH) FROM MEMVAR
ENDSCAN  


SELECT (lcSadInvH) 
SCAN
  DIMENSION laInv[lnInvCount] 

  *-- Get The order number manualy or using gfsequence depend on the invoice setup
  IF EMPTY(&lcSadInvH..CSadInv)
  *-- Get The Invoice number on the factor sequence if the invoice was factored 
    lcInvNo=IIF(EMPTY(&lcSadInvH..cFacCode),;
            gfSequence('INVOICE','','',&lcSadInvH..cDivision),;
            gfSequence('CFINVOICE' ,'','',&lcSadInvH..cDivision)) 
  
  
    laInv[lnInvCount] = lcInvNo
    lnInvCount = lnInvCount + 1
  
  ELSE
    lcInvNo=&lcSadInvH..cSadInv
    laInv[lnInvCount] = lcInvNo
  ENDIF
  
  SELECT (lcSadInvH)
  *-- If the invoice was consolidated invoice give the lines the same invoice number
  IF Consol='Y'
    lcAccount = Account
    REPLACE REST cSadInv WITH lcInvNo WHILE Account+Order+Store+PikTkt=lcAccount
    =SEEK(lcAccount)
  ENDIF  
  REPLACE nExRate   WITH IIF(nExRate=0,1,nExRate) ,;
          nCurrUnit WITH IIF(nCurrUnit=0,1,nCurrUnit) ,;
          LastLine  WITH IIF(SEEK('O'+Order,'ORDLINE'),LastLine,0),;
          CSadInv   WITH lcInvNo 
  llConsSad = &lcSadInvH..Direct_Inv OR &lcSadInvH..Consol='Y' OR ;
              EMPTY(&lcSadInvH..Flag)        


  SELECT(lcInvLine)
  
  =SEEK(&lcSadInvH..Account+&lcSadInvH..Order+&lcSadInvH..Store+&lcSadInvH..PikTkt)
  WAIT 'Updating Invoice Lines...' WINDOW NOWAIT
  
  SCAN REST WHILE Account+Order+Store+PikTkt = ;
              &lcSadInvH..Account+&lcSadInvH..Order+&lcSadInvH..Store+&lcSadInvH..PikTkt
                
    SCATTER MEMVAR MEMO
    m.cSadInv= &lcSadInvH..cSadInv
    INSERT INTO (lcSadInvL) FROM MEMVAR
  ENDSCAN  
     
  IF !EMPTY(lcUpsBox) AND USED(lcUpsBox)
    WAIT 'Updating UPS Box File ' WINDOW NOWAIT
    SELECT (lcUpsBox)
    lcStoreFld = IIF(&lcSadInvH..Consol='Y',&lcSadInvH..cConStore,&lcSadInvH..Store)
    IF SEEK(&lcSadInvH..Order+lcStoreFld+&lcSadInvH..PikTkt)
      SCAN REST WHILE Order+Store+PikTkt+STR(CARTONS,5)=;
                    &lcSadInvH..Order+lcStoreFld+&lcSadInvH..PikTkt
        SCATTER MEMVAR
        m.cSadInv = lcInvNo
        INSERT INTO (lcSadUpsB) FROM MEMVAR
         
      ENDSCAN
    ENDIF
  ENDIF
  
  IF llConsSad
    IF !EMPTY(lcInstHdr) AND !EMPTY(lcInstLin) AND ;
      SEEK(&lcSadInvH..Order + &lcSadInvH..Store + &lcSadInvH..PikTkt,lcInstHdr) .AND. ;
          SEEK(&lcSadInvH..Order + &lcSadInvH..Store + &lcSadInvH..PikTkt,lcInstLin)
        
        SELECT (lcInstHdr)
        SCATTER MEMVAR MEMO
        m.cSadInv=lcInvNo
        INSERT INTO (lcSadInsH) FROM MEMVAR
        
        SELECT (lcInstLin)
        *-- update all the lines in the instalment detail file and the debit file 
        SCAN REST WHILE Order+Store+PikTkt+cInstalNo =;
                      &lcSadInvH..Order+&lcSadInvH..Store+&lcSadInvH..PikTkt
          SCATTER MEMVAR MEMO
          m.cSadInv=lcInvNo
          INSERT INTO (lcSadInsL) FROM MEMVAR
        
        ENDSCAN
    ENDIF
  ENDIF  
  
  
  IF !EMPTY(lcOrdCanLn) AND &lcSadInvH..Consol<>'Y' AND SEEK('O'+&lcSadInvH..ORDER,lcOrdCanLn)
    SELECT (lcOrdCanLn)
    SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+&lcSadInvH..ORDER
      *-- Update the order canceld Quantity
      SCATTER MEMVAR
      m.cSadInv=lcInvNo
      INSERT INTO (lcSadOrdC)  FROM MEMVAR
    ENDSCAN
  ENDIF
                       
ENDSCAN

=gfOpenFile(gcDataDir+'INVHDRS',gcDataDir+'INVHDRS','SH')
=gfOpenFile(gcDataDir+'INVLINES',gcDataDir+'INVLINES','SH')
=gfOpenFile(gcDataDir+'UPSBOXS',gcDataDir+'UPSBOXS','SH')
=gfOpenFile(gcDataDir+'ORDCANSD',gcDataDir+'ORDCANSD','SH')
=gfOpenFile(gcDataDir+'SDINSTMH',gcDataDir+'SDINSTMH','SH')
=gfOpenFile(gcDataDir+'SDINSTMD',gcDataDir+'SDINSTMD','SH')

SELECT (lcSadInvH)
SCAN
  SCATTER MEMVAR MEMO
  SELECT INVHDRS
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

SELECT (lcSadInvL)
SCAN
  SCATTER MEMVAR MEMO
  SELECT INVLINES
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

IF USED(lcSadUpsB)
  SELECT (lcSadUpsB)
  SCAN
    SCATTER MEMVAR MEMO
    SELECT UPSBOXS
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
ENDIF

SELECT (lcSadOrdC) 
SCAN
  SCATTER MEMVAR MEMO
  SELECT ORDCANSD
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
  
SELECT(lcInstLin)
SCAN
  SCATTER MEMVAR MEMO
  SELECT SDINSTMD
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

SELECT (lcSadInsH)
 SCAN
  SCATTER MEMVAR MEMO
  SELECT SDINSTMH
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

lcSydKey=''

SELECT unCmSess
  IF SEEK('O'+PADR('IINVOICE',10)+PADR(gcUser_id,10)+lcSession)
    REPLACE STATUS WITH 'C'
    llContinue = .F.
    UNLOCK
  ENDIF
  SELECT (lcSadInvH)
lnInvCount = ALEN(laInv)

DO CASE
  CASE EMPTY(laInv)
    llCSave = .F.
  CASE lnInvCount=1
    *E300817,1 Message : 40005
    *E300817,1 Invoice has been saves as.
    *E300817,1 Button : 00000 
    *E300817,1 Ok
    =gfModalGen('INM00000B00000','ALERT','','', 'Pre_Billed invoice has been saved as '+laInv[1] )
  OTHERWISE
    *E300817,1 Message : 40118
    *E300817,1 Invoices 999999 Through 999999.
    *E300817,1 Button : 00000 
    *E300817,1 Ok
    =gfModalGen('INM00000B00000','ALERT','','', 'Pre_Billed invoices '+laInv[1] +' Through ' + laInv[lnInvCount])
    
ENDCASE
SELECT (lcInvHdr)




*!**************************************************************************
*! Name      : lfReSave
*! Developer : Nader Anis (NAD)
*! Date      : 30/06/2001
*! Purpose   : 
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************

FUNCTION lfReSave


SELECT (lcInvHdr)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]

laFileStru[lnFileStru+1,1] = 'NPICKED'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 7
laFileStru[lnFileStru+1,4] = 0


DECLARE laIndex[3,2]
laIndex[1,1] = 'Account+Order+Store+PikTkt'
laIndex[1,2] =  lcSadInvH 
laIndex[2,1] = 'cSelect+Account+Order+Store+PikTkt'
laIndex[2,2] = 'Select'
laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode'
laIndex[3,2] = 'Consol'

=gfCrtTmp(lcSadInvH,@laFileStru,@laIndex)

SELECT (lcInvLine)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'cSadInv'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0

DECLARE laIndex[4,2]
laIndex[1,1] = 'Account+Order+Store+PikTkt+STR(LineNo,6)'
laIndex[1,2] = lcSadInvL
laIndex[2,1] = 'Account+Order+Store+PikTkt+Style'
laIndex[2,2] = 'Styles'
laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode+Style'
laIndex[3,2] = 'Consol'
laIndex[4,1] = 'Account+Order+Store+PikTkt+cDyeFlag+Dyelot'
laIndex[4,2] = 'Dyelot'

=gfCrtTmp(lcSadInvL,@laFileStru,@laIndex)

IF USED(lcUpsBox)
  SELECT (lcUpsBox)
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'cSadInv'  
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  =gfCrtTmp(lcSadUpsB,@laFileStru,[Order+Store+PikTkt+STR(CARTONS,5)],lcSadUpsB)
ENDIF

IF !EMPTY(lcInstHdr) AND !EMPTY(lcInstLin)  
  
  =gfCrtTmp(lcSadInsH,[(cSadInv C(6) ,Order C(6),Store C(8),PikTkt C(6),;
            cInstmType C(1), nInstmFreq N(3),nInstIAmnt N(11,2),dInstmStDt D,;
            nNoInstm N(3), cInstmRef C(30),nInstIPcnt N(6,2),nInstmAmnt N(10,2))],;
            [Order+Store+PikTkt],lcSadInsH)

  =gfCrtTmp(lcSadInsL,[(cSadInv C(6) ,Order C(6),Store C(8),PikTkt C(6),;
              cInstalNo C(3),nInstmAmnt N(10,2),DueDate D,nInstmPcnt N(6,2),cInstmNote C(30))],;
              [Order+Store+PikTkt+cInstalNo],lcSadInsL)

ENDIF

IF !EMPTY(lcOrdCanLn)
  
  =gfCrtTmp(lcSadOrdC ,[( cSadInv C(6),cOrdType C(1),Order C(6),LineNo N(6),Qty1 N(6),;
            Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
            Qty8 N(6),TotQty N(7),Cancelled D,cCancReson C(6))],;
            [CORDTYPE+ORDER+STR(LINENO,6)],lcSadOrdC )

ENDIF 

SELECT (lcInvHdr)
SCAN
  SCATTER MEMVAR MEMO
  M.NPICKED=&lcInvHdr..Picked
  INSERT INTO (lcSadInvH) FROM MEMVAR
ENDSCAN  

SELECT (lcSadInvH) 

SCAN  
 *-- Get The order number manualy or using gfsequence depend on the invoice setup
  IF EMPTY(&lcSadInvH..CSadInv)
  *-- Get The Invoice number on the factor sequence if the invoice was factored 
    lcInvNo=IIF(EMPTY(&lcSadInvH..cFacCode),;
            gfSequence('INVOICE','','',&lcSadInvH..cDivision),;
            gfSequence('CFINVOICE' ,'','',&lcSadInvH..cDivision)) 
 
  ELSE
    lcInvNo =&lcSadInvH..CSadInv
    IF SEEK (lcInvNo ,'INVHDRS')
      SELECT INVHDRS 
      DELETE REST WHILE cSadInv=lcInvNo 
     ENDIF
    
    IF SEEK (lcInvNo,'INVLINES')  
      SELECT INVLINES 
      DELETE REST WHILE cSadInv=lcInvNo            
    ENDIF
        
    IF SEEK (lcInvNo,'UPSBOXS') 
      SELECT UPSBOXS
      DELETE REST WHILE cSadInv=lcInvNo  
    ENDIF
         
    IF SEEK (lcInvNo ,'ORDCANSD') 
      SELECT ORDCANSD
      DELETE REST WHILE cSadInv=lcInvNo  
    ENDIF 
      
    IF SEEK (lcInvNo ,'SDINSTMD') 
      SELECT SDINSTMD
      DELETE REST WHILE cSadInv=lcInvNo  
    ENDIF 
      
    IF SEEK (lcInvNo ,'SDINSTMH') 
      SELECT SDINSTMH
      DELETE REST WHILE cSadInv=lcInvNo  
    ENDIF 
  ENDIF
  
  SELECT (lcSadInvH)
  *-- If the invoice was consolidated invoice give the lines the same invoice number
  IF Consol='Y'
    lcAccount = Account
    REPLACE REST cSadInv WITH lcInvNo WHILE Account+Order+Store+PikTkt=lcAccount
    =SEEK(lcAccount)
  ENDIF  
  REPLACE nExRate   WITH IIF(nExRate=0,1,nExRate) ,;
          nCurrUnit WITH IIF(nCurrUnit=0,1,nCurrUnit) ,;
          LastLine  WITH IIF(SEEK('O'+Order,'ORDLINE'),LastLine,0),;
          CSadInv   WITH lcInvNo 
  llConsSad = &lcSadInvH..Direct_Inv OR &lcSadInvH..Consol='Y' OR ;
              EMPTY(&lcSadInvH..Flag)        


  SELECT(lcInvLine)
  
  =SEEK(&lcSadInvH..Account+&lcSadInvH..Order+&lcSadInvH..Store+&lcSadInvH..PikTkt)
  WAIT 'Updating Invoice Lines...' WINDOW NOWAIT
  
  SCAN REST WHILE Account+Order+Store+PikTkt = ;
                &lcSadInvH..Account+&lcSadInvH..Order+&lcSadInvH..Store+&lcSadInvH..PikTkt
                
    SCATTER MEMVAR MEMO
    m.cSadInv= &lcSadInvH..cSadInv
    INSERT INTO (lcSadInvL) FROM MEMVAR
  
  ENDSCAN  
   
    
    
  IF !EMPTY(lcUpsBox) AND USED(lcUpsBox)
    WAIT 'Updating UPS Box File ' WINDOW NOWAIT
    SELECT (lcUpsBox)
    lcStoreFld = IIF(&lcSadInvH..Consol='Y',&lcSadInvH..cConStore,&lcSadInvH..Store)
    IF SEEK(&lcSadInvH..Order+lcStoreFld+&lcSadInvH..PikTkt)
      SCAN REST WHILE Order+Store+PikTkt+STR(CARTONS,5)=;
                   &lcSadInvH..Order+lcStoreFld+&lcSadInvH..PikTkt
        SCATTER MEMVAR
        m.cSadInv = lcInvNo
        INSERT INTO (lcSadUpsB) FROM MEMVAR
          
      ENDSCAN
    ENDIF
  ENDIF
  
  IF llConsSad
    IF !EMPTY(lcInstHdr) AND !EMPTY(lcInstLin) AND ;
      SEEK(&lcSadInvH..Order + &lcSadInvH..Store + &lcSadInvH..PikTkt,lcInstHdr) .AND. ;
          SEEK(&lcSadInvH..Order + &lcSadInvH..Store + &lcSadInvH..PikTkt,lcInstLin)
        
        SELECT (lcInstHdr)
        SCATTER MEMVAR MEMO
        m.cSadInv=lcInvNo
        INSERT INTO (lcSadInsH) FROM MEMVAR
        
        SELECT (lcInstLin)
        *-- update all the lines in the instalment detail file and the debit file 
        SCAN REST WHILE Order+Store+PikTkt+cInstalNo =;
                      &lcSadInvH..Order+&lcSadInvH..Store+&lcSadInvH..PikTkt
          
          SCATTER MEMVAR MEMO
          m.cSadInv=lcInvNo
          INSERT INTO (lcSadInsL) FROM MEMVAR
        
        ENDSCAN
     
    ENDIF
  ENDIF  
  
  IF !EMPTY(lcOrdCanLn) AND &lcSadInvH..Consol<>'Y' AND SEEK('O'+&lcSadInvH..ORDER,lcOrdCanLn)
    SELECT (lcOrdCanLn)
    SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+&lcSadInvH..ORDER
    *-- Update the order canceld Quantity
      SCATTER MEMVAR
      m.cSadInv=lcInvNo
      INSERT INTO (lcSadOrdC)  FROM MEMVAR
    ENDSCAN
  ENDIF
     
                         
ENDSCAN

=gfOpenFile(gcDataDir+'INVHDRS',gcDataDir+'INVHDRS','SH')
=gfOpenFile(gcDataDir+'INVLINES',gcDataDir+'INVLINES','SH')
=gfOpenFile(gcDataDir+'UPSBOXS',gcDataDir+'UPSBOXS','SH')
=gfOpenFile(gcDataDir+'ORDCANSD',gcDataDir+'ORDCANSD','SH')
=gfOpenFile(gcDataDir+'SDINSTMH',gcDataDir+'SDINSTMH','SH')
=gfOpenFile(gcDataDir+'SDINSTMD',gcDataDir+'SDINSTMD','SH')


SELECT (lcSadInvH)
SCAN
  SCATTER MEMVAR MEMO
  SELECT INVHDRS
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

SELECT (lcSadInvL)
SCAN
  SCATTER MEMVAR MEMO
  SELECT INVLINES
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

SELECT (lcSadUpsB)
SCAN
  SCATTER MEMVAR MEMO
  SELECT UPSBOXS
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

SELECT (lcSadOrdC) 
SCAN
  SCATTER MEMVAR MEMO
  SELECT ORDCANSD
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
  
SELECT (lcInstLin)
SCAN
  SCATTER MEMVAR MEMO
  SELECT SDINSTMD
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

SELECT (lcSadInsH)
 SCAN
  SCATTER MEMVAR MEMO
  SELECT SDINSTMH
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN


lcSydKey=''

SELECT unCmSess
  IF SEEK('O'+PADR('IINVOICE',10)+PADR(gcUser_id,10)+lcSession)
    REPLACE STATUS WITH 'C'
    llContinue = .F.
    UNLOCK
  ENDIF
  SELECT (lcSadInvH)

lnInvCount = ALEN(laInv)

DO CASE
  CASE EMPTY(laInv)
    llCSave = .F.
  CASE lnInvCount=1
    =gfModalGen('INM00000B00000','ALERT','','', ' Pre_Billed invoice has been saved as '+laInv[1] )
  OTHERWISE
    =gfModalGen('INM00000B00000','ALERT','','', ' Pre_Billed invoices '+laInv[1]+ ' Through '+laInv[lnInvCount] )
ENDCASE

SELECT (lcInvHdr)



*!**************************************************************************
*! Name      : lfSadSeq
*! Developer : Nader Anis (NAD)
*! Date      : 30/06/2001
*! Purpose   : 
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************


FUNCTION lfSadSeq

 lcSAlias=ALIAS()
 lnSRecno=RECNO('INVHDR') 
 lcInvNo=&lcHdrFile..CsadInv
 SELECT InvHdr
 APPEND BLANK
 =RLOCK()
 REPLACE Invoice with lcInvNo
 UNLOCK

 SELECT INVHDRS
 IF SEEK (lcInvNo)
   DELETE REST WHILE cSadInv=lcinvNo  
 ENDIF

 IF RECCOUNT('InvHdr') >= lnSRecno
   GO lnSRecno IN InvHdr
 ENDIF

 SELECT (lcSAlias) 
 
 RETURN

*!**************************************************************************
*! Name      : lfGetOrd
*! Developer : Nader Anis (NAD)
*! Date      : 30/06/2001
*! Purpose   : 
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
 FUNCTION lfGetOrd

lcSdAlias=ALIAS()
=gfOpenFile(gcDataDir+'INVHDRS', gcDataDir+'SDORDER','SH')
*C200234,1 (Begin) Make the check upon the detalis not header.
*IF SEEK (OrdHdr.ORDER,'INVHDRS')
*  =gfModalGen('INM00000B00000','DIALOG','','', 'Order # '+lcOrderNo + ' has been Pre_Billed! Cannot ship.')
*  SELECT (lcSdAlias)
*  RETURN (.F.)
*ELSE
*  SELECT (lcSdAlias)
*  RETURN (.T.)  
*ENDIF
=gfOpenFile(gcDataDir+'invlines', gcDataDir+'OrdPik','SH')
llNFound = .T.
IF SEEK (IIF(ORDLINE.PICKED,OrdHdr.ORDER+ORDLINE.PIKTKT,OrdHdr.ORDER),'invlines')
  IF !ORDLINE.PICKED OR ORDHDR.LASTLINE = 1
    =gfModalGen('INM00000B00000','DIALOG','','', 'Order # '+lcOrderNo + ' has been Pre_Billed! Cannot ship.')
  ELSE
    =gfModalGen('INM00000B00000','DIALOG','','', 'Piktkt # '+ORDLINE.PIKTKT+ ' on Order # '+lcOrderNo + ' has been Pre_Billed! Cannot ship.')
  ENDIF  
  llNFound = .F.
ENDIF
SELECT (lcSdAlias)
RETURN (llNFound)  
*C200234,1 (Begin)

*!**************************************************************************
*! Name      : lfCRTMP
*! Developer : Nader Anis (NAD)
*! Date      : 30/06/2001
*! Purpose   : 
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
FUNCTION lfCRTMP

SELECT (lcInvHdr)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,4]
laFileStru[lnFileStru+1,1] = 'cSadInv'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'NPICKED'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 7
laFileStru[lnFileStru+2,4] = 0

DECLARE laIndex[3,2]
laIndex[1,1] = 'Account+Order+Store+PikTkt'
laIndex[1,2] =  lcSadInvH 
laIndex[2,1] = 'cSelect+Account+Order+Store+PikTkt'
laIndex[2,2] = 'Select'
laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode'
laIndex[3,2] = 'Consol'
=gfCrtTmp(lcSadInvH,@laFileStru,@laIndex)

SELECT (lcInvLine)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'cSadInv'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0

DECLARE laIndex[4,2]
laIndex[1,1] = 'Account+Order+Store+PikTkt+STR(LineNo,6)'
laIndex[1,2] = lcSadInvL
laIndex[2,1] = 'Account+Order+Store+PikTkt+Style'
laIndex[2,2] = 'Styles'
laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode+Style'
laIndex[3,2] = 'Consol'
laIndex[4,1] = 'Account+Order+Store+PikTkt+cDyeFlag+Dyelot'
laIndex[4,2] = 'Dyelot'

=gfCrtTmp(lcSadInvL,@laFileStru,@laIndex)

IF USED(lcUpsBox)
  SELECT (lcUpsBox)
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'cSadInv'  
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  =gfCrtTmp(lcSadUpsB,@laFileStru,[Order+Store+PikTkt+STR(CARTONS,5)],lcSadUpsB)
ENDIF

IF !EMPTY(lcInstHdr) AND !EMPTY(lcInstLin)  
  
  =gfCrtTmp(lcSadInsH,[(cSadInv C(6) ,Order C(6),Store C(8),PikTkt C(6),;
            cInstmType C(1), nInstmFreq N(3),nInstIAmnt N(11,2),dInstmStDt D,;
            nNoInstm N(3), cInstmRef C(30),nInstIPcnt N(6,2),nInstmAmnt N(10,2))],;
            [Order+Store+PikTkt],lcSadInsH)
  =gfCrtTmp(lcSadInsL,[(cSadInv C(6) ,Order C(6),Store C(8),PikTkt C(6),;
              cInstalNo C(3),nInstmAmnt N(10,2),DueDate D,nInstmPcnt N(6,2),cInstmNote C(30))],;
              [Order+Store+PikTkt+cInstalNo],lcSadInsL)

ENDIF
IF !EMPTY(lcOrdCanLn)
    
  =gfCrtTmp(lcSadOrdC ,[( cSadInv C(6),cOrdType C(1),Order C(6),LineNo N(6),Qty1 N(6),;
            Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
            Qty8 N(6),TotQty N(7),Cancelled D,cCancReson C(6))],;
            [CORDTYPE+ORDER+STR(LINENO,6)],lcSadOrdC )

ENDIF 




*!**************************************************************************
*! Name      : lfInvOvr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 07/29/2001
*! Purpose   : Cancel Over allocated from the open qty when backing order.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*!*C102318,1
FUNCTION lfInvOvr

SCAN REST WHILE Account+Order+Store+PikTkt = ;
          &lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..Order,SPACE(6))+;
          &lcHdrFile..Store+&lcHdrFile..PikTkt
  SCATTER MEMVAR MEMO
  *C102318,1 (Begin) Get the over-allocated Qty.
  lnOver = 0
  IF SEEK('O'+m.Order+STR(m.LineNo,6),'ORDLINE') AND ORDLINE.lSubstsz
    FOR lnFrom = 1 TO 8
      lcFrom = STR(lnFrom,1)
      IF ORDLINE.PIK&lcFrom > ORDLINE.QTY&lcFrom
        lnOver  = lnOver + (ORDLINE.PIK&lcFrom - ORDLINE.QTY&lcFrom)
      ENDIF
    ENDFOR
  ENDIF
  *C102318,1(End)

  =SEEK(m.Style,'Style')
  =SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
  *-- the stock current value to use it in calculating the style cost 
  lnStkVal = Style.nStkVal    
  *-- Calculate the avarage cost of the style due to the cost method and warehouse   
  m.Cost   = IIF(laInvSetup[3,2]='A',;
             IIF(laInvSetup[2,2]='Y',StyDye.Ave_Cost,Style.Ave_Cost),Style.TotCost)
  *-- Get the GL Cost ,GL Sales link code 
  IF laInvSetup[1,2]='Y' .AND. llConsAcc
    =SEEK('O'+m.Order+STR(m.LineNo,6),'ORDLINE')
    m.Gl_Cost  = IIF(!EMPTY(STYLE.Link_Code),STYLE.Link_Code,'DEFDEF')
    m.Gl_Cost  = IIF(laInvSetup[2,2]='Y' .AND. !EMPTY(STYDYE.Gl_Link),;
                   STYDYE.Gl_Link,m.Gl_Cost)
    *-- Concatinate customer sales link code and style sales like code
    m.Gl_Sales = IIF(EMPTY(OrdLine.Gl_Sales),lcCustSales+Style.cSlsGlLink,OrdLine.Gl_Sales)

    *-- Update the G/L Distribution file 
    DO GLDIST WITH m.Gl_Sales,'003',-(m.TotQty *m.Price),'IN',lcInvNo,;
                   &lcHdrFile..dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
                   &lcHdrFile..cCurrCode,&lcHdrFile..nCurrUnit,&lcHdrFile..nExRate
    m.cSalesAcnt = &lcDistFile..GlAccount   && Get the sales Revenus account 
    IF &lcHdrFile..ShipAmt # 0
      *-- Update the G/L Distribution file 
      DO GLDIST WITH m.Gl_Sales,'005',(m.TotQty * m.Price * ABS(&lcHdrFile..Discount)/&lcHdrFile..ShipAmt),;
                    'IN',lcInvNo,&lcHdrFile..dPostDate,lcGlYear,lcGlPeriod,;
                     lcDistFile,'',&lcHdrFile..cCurrCode,&lcHdrFile..nCurrUnit,&lcHdrFile..nExRate
    ENDIF
    *--  Don't update cDicAcnt field in InvLine file if there no discount 
    IF (m.TotQty * m.Price * ABS(&lcHdrFile..Discount)/&lcHdrFile..ShipAmt) > 0
      m.cDiscAcnt = &lcDistFile..GlAccount
    ENDIF
  ENDIF  
  * Update style dyelot Shipped quantities
  IF &lcDetFile..nSteps < 1 AND laInvSetup[4,2]='Y' AND llConsAcc AND ;
      Style.cDye_Flg = 'Y' AND SEEK(m.Style+m.cWareCode+m.Dyelot,'StyDye')
    SELECT StyDye
    =RLOCK()
    REPLACE SHP1   WITH SHP1   + m.QTY1  ,;
            SHP2   WITH SHP2   + m.QTY2  ,;
            SHP3   WITH SHP3   + m.QTY3  ,;
            SHP4   WITH SHP4   + m.QTY4  ,;
            SHP5   WITH SHP5   + m.QTY5  ,;
            SHP6   WITH SHP6   + m.QTY6  ,;
            SHP7   WITH SHP7   + m.QTY7  ,;
            SHP8   WITH SHP8   + m.QTY8  ,;
            TOTSHP WITH TOTSHP + m.TotQTY
    UNLOCK
    =gfTraceKey('STYDYE',m.Style+m.cWareCode+m.Dyelot,'M')
    SELECT (lcDetFile)
    =RLOCK()
    *-- flag to say that this line was updated in the last session or not
    REPLACE nSteps WITH 1    
    UNLOCK
  ENDIF
  IF &lcDetFile..nSteps < 2 .AND. llConsAcc .AND. ;
    SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
    SELECT StyDye
    =RLOCK()
    REPLACE SHP1   WITH SHP1   + m.QTY1  ,;
            SHP2   WITH SHP2   + m.QTY2  ,;
            SHP3   WITH SHP3   + m.QTY3  ,;
            SHP4   WITH SHP4   + m.QTY4  ,;
            SHP5   WITH SHP5   + m.QTY5  ,;
            SHP6   WITH SHP6   + m.QTY6  ,;
            SHP7   WITH SHP7   + m.QTY7  ,;
            SHP8   WITH SHP8   + m.QTY8  ,;
            TOTSHP WITH TOTSHP + m.TotQTY
    UNLOCK
    =gfTraceKey('STYDYE',m.Style+m.cWareCode+SPACE(10),'M')
    SELECT (lcDetFile)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  IF &lcDetFile..nSteps < 3 .AND. llConsAcc
    SELECT STYLE
    =RLOCK()
    REPLACE SHP1   WITH SHP1   + m.QTY1 ,;
            SHP2   WITH SHP2   + m.QTY2 ,;
            SHP3   WITH SHP3   + m.QTY3 ,;
            SHP4   WITH SHP4   + m.QTY4 ,;
            SHP5   WITH SHP5   + m.QTY5 ,;
            SHP6   WITH SHP6   + m.QTY6 ,;
            SHP7   WITH SHP7   + m.QTY7 ,;
            SHP8   WITH SHP8   + m.QTY8 ,;
            TOTSHP WITH TOTSHP + m.TotQTY
    UNLOCK
    =gfTraceKey('STYLE',m.Style,'M')
    SELECT (lcDetFile)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF
  IF laInvSetup[6,2]='Y'
    lnNetAmnt = m.TotQty*m.Price*(1-&lcHdrFile..DiscPcnt/100)*(1-&lcHdrFile..Trde_Disc/100)
    lnComm1   = lnComm1 + (lnNetAmnt * m.Comm1 / 100)
    lnComm2   = lnComm2 + (lnNetAmnt * m.Comm2 / 100)
  ENDIF
  m.Invoice  = &lcHdrFile..Invoice
  m.Order    = &lcHdrFile..Order
  *-- Calculate the equivlant amount by the company currency code
  m.nEqvAmnt = ROUND(m.Price*m.TotQty &lcExRSin &lcHdrFile..nExRate &lcUntSin &lcHdrFile..nCurrUnit,2)
  m.InvDate  = &lcHdrFile..InvDate
  IF Consol <> 'Y' AND &lcDetFile..nSteps < 12
    *-- if new order line or the line not found ad new line to the ORDLINE File 
    IF (!&lcHdrFile..Direct_Inv AND &lcDetFile..lNewLine) OR ; 
       !SEEK('O'+m.Order+STR(m.LineNo,6),'ORDLINE')
      IF SEEK('O'+lcOrdNo,'ORDHDR')
        *-- This is a new sales oreder line so update the order header 
        SELECT 'ORDHDR'
        =RLOCK()
        *-- update the order header with the new line
        REPLACE LastLine WITH LastLine + 1 ,;
                Book     WITH Book     + m.TotQty ,;
                BookAmt  WITH BookAmt  + m.TotQty*m.Price ,;
                Ship     WITH Ship     + m.TotQty ,;
                ShipAmt  WITH ShipAmt  + m.TotQty*m.Price
        UNLOCK
      ELSE  
        SELECT (lcHdrFile)
        =RLOCK()
        REPLACE LastLine WITH LastLine + 1
        UNLOCK
      ENDIF
      m.LineNo=LastLine
      *-- in case of direct invoice add new line in the order line 
      INSERT INTO ORDLINE ;
        (cOrdType,Order,Account,LineNo,Style,Scale,Group,Gros_Price,Disc_Pcnt,;
        Price,Start,Complete,Comm1,Comm2,Note_Mem,Desc1,Book1,Book2,Book3,Book4,;
        Book5,Book6,Book7,Book8,TotBook,Season,PrePak,PPQty,cWareCode,Store,Pack_id) VALUES ;
        ('O',m.Order,m.Account,m.LineNo,m.Style,m.Scale,m.Group,m.Gros_Price,;
         m.Disc_Pcnt,m.Price,&lcHdrFile..InvDate,&lcHdrFile..InvDate,m.Comm1,;
         m.Comm2,m.Note_Mem,m.Desc1,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
         m.Qty7,m.Qty8,m.TotQty,m.Season,m.PrePak,m.PPQty,m.cWareCode,m.Store,m.Pack_Id)
      SELECT (lcDetFile)
      =RLOCK()      
      REPLACE nSteps WITH 12
      UNLOCK
    ELSE                
      IF laInvSetup[4,2]='Y' .AND. Style.cDye_Flg='Y' .AND. ;
        !EMPTY(OrdLine.Dyelot) AND SEEK(m.Style+m.cWareCode+OrdLine.Dyelot,'StyDye')          
        IF &lcDetFile..nSteps < 5 
          SELECT StyDye
          =RLOCK()
          IF !m.lBackOrd OR (!EMPTY(OrdLine.Dyelot) AND OrdLine.Dyelot<>m.Dyelot)
          *-- this is a sales order invoice and no Back order so decrease the ordered quantity
          *-- in the stydye file with the total ordered quantity for this Style
            REPLACE Ord1   WITH MAX(Ord1 - OrdLine.Qty1,0),;
                    Ord2   WITH MAX(Ord2 - OrdLine.Qty2,0),;
                    Ord3   WITH MAX(Ord3 - OrdLine.Qty3,0),;
                    Ord4   WITH MAX(Ord4 - OrdLine.Qty4,0),;
                    Ord5   WITH MAX(Ord5 - OrdLine.Qty5,0),;
                    Ord6   WITH MAX(Ord6 - OrdLine.Qty6,0),;
                    Ord7   WITH MAX(Ord7 - OrdLine.Qty7,0),;
                    Ord8   WITH MAX(Ord8 - OrdLine.Qty8,0),;
                    TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
          ELSE
            *-- (Back order) so decrease the ordered quantity with the the shipped quantity
            *-- for this Style            
            REPLACE Ord1   WITH Ord1 - MIN(OrdLine.Qty1,m.Qty1),;
                    Ord2   WITH Ord2 - MIN(OrdLine.Qty2,m.Qty2),;
                    Ord3   WITH Ord3 - MIN(OrdLine.Qty3,m.Qty3),;
                    Ord4   WITH Ord4 - MIN(OrdLine.Qty4,m.Qty4),;
                    Ord5   WITH Ord5 - MIN(OrdLine.Qty5,m.Qty5),;
                    Ord6   WITH Ord6 - MIN(OrdLine.Qty6,m.Qty6),;
                    Ord7   WITH Ord7 - MIN(OrdLine.Qty7,m.Qty7),;
                    Ord8   WITH Ord8 - MIN(OrdLine.Qty8,m.Qty8),;
                    TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
          ENDIF
          UNLOCK
          SELECT (lcDetFile)
          =RLOCK()
          REPLACE nSteps WITH 5
          UNLOCK
        ENDIF
        *--decrease the Alocated quantitys in the stydye file
        *-- with the order line Picked Quantities
        IF OrdLine.Picked .AND. &lcDetFile..nSteps < 6
          SELECT StyDye
          =RLOCK()
          REPLACE ALO1   WITH ALO1   - OrdLine.PIK1,;
                  ALO2   WITH ALO2   - OrdLine.PIK2,;
                  ALO3   WITH ALO3   - OrdLine.PIK3,;
                  ALO4   WITH ALO4   - OrdLine.PIK4,;
                  ALO5   WITH ALO5   - OrdLine.PIK5,;
                  ALO6   WITH ALO6   - OrdLine.PIK6,;
                  ALO7   WITH ALO7   - OrdLine.PIK7,;
                  ALO8   WITH ALO8   - OrdLine.PIK8,;
                  TOTALO WITH TOTALO - OrdLine.TOTPIK
          UNLOCK
          SELECT (lcDetFile)
          =RLOCK()        
          REPLACE nSteps WITH 6
          UNLOCK
        ENDIF
        =gfTraceKey('STYDYE',m.Style+m.cWareCode+OrdLine.Dyelot,'M')
      ENDIF
      IF SEEK(IIF(!EMPTY(m.AltStyle),m.AltStyle,m.Style)+m.cWareCode+SPACE(10),'StyDye')
        IF &lcDetFile..nSteps < 7
          SELECT StyDye
          =RLOCK()
          *-- before saving any issue invoice if the style was changed by the user 
          *-- the old style will be saved in the AltStyle field 
          IF !m.lBackOrd OR (!EMPTY(m.AltStyle) AND m.AltStyle<>m.Style)
          *-- this is a sales order invoice with no Back order or there is an altrernative 
          *-- style so decrease the ordered quantity in the stydye file with 
          *-- the total ordered quantity for the AltStyle
          REPLACE Ord1   WITH MAX(Ord1 - OrdLine.Qty1,0),;
                    Ord2   WITH MAX(Ord2 - OrdLine.Qty2,0),;
                    Ord3   WITH MAX(Ord3 - OrdLine.Qty3,0),;
                    Ord4   WITH MAX(Ord4 - OrdLine.Qty4,0),;
                    Ord5   WITH MAX(Ord5 - OrdLine.Qty5,0),;
                    Ord6   WITH MAX(Ord6 - OrdLine.Qty6,0),;
                    Ord7   WITH MAX(Ord7 - OrdLine.Qty7,0),;
                    Ord8   WITH MAX(Ord8 - OrdLine.Qty8,0),;
                    TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
          ELSE
            *-- (Back order) so decrease the ordered quantity with the the shipped quantity
            *-- for this Style 
            *C102318,1 (Begin) If no substition happened, go as usual.
            =lfCutOver()
            *C102318,1 (End)
          ENDIF  
          UNLOCK
          SELECT (lcDetFile)
          =RLOCK()
          REPLACE nSteps WITH 7
          UNLOCK
        ENDIF
        IF OrdLine.Picked .AND. &lcDetFile..nSteps < 8
          *--decrease the Alocated quantitys in the stydye file
          *-- with the order line Picked Quantities
          SELECT StyDye
          =SEEK(IIF(!EMPTY(m.AltStyle),m.AltStyle,m.Style)+m.cWareCode+SPACE(10))
          =RLOCK()
          REPLACE ALO1   WITH ALO1   - OrdLine.PIK1,;
                  ALO2   WITH ALO2   - OrdLine.PIK2,;
                  ALO3   WITH ALO3   - OrdLine.PIK3,;
                  ALO4   WITH ALO4   - OrdLine.PIK4,;
                  ALO5   WITH ALO5   - OrdLine.PIK5,;
                  ALO6   WITH ALO6   - OrdLine.PIK6,;
                  ALO7   WITH ALO7   - OrdLine.PIK7,;
                  ALO8   WITH ALO8   - OrdLine.PIK8,;
                  TOTALO WITH TOTALO - OrdLine.TOTPIK
          UNLOCK
          SELECT (lcDetFile)
          =RLOCK()        
          REPLACE nSteps WITH 8
          UNLOCK
        ENDIF
        =gfTraceKey('STYDYE',IIF(!EMPTY(m.AltStyle),m.AltStyle,m.Style)+m.cWareCode+SPACE(10),'M')
      ENDIF  
      =SEEK(IIF(!EMPTY(m.AltStyle),m.AltStyle,m.Style),'Style')
      IF &lcDetFile..nSteps < 9
        SELECT Style
        =RLOCK()
        IF !m.lBackOrd .OR. (!EMPTY(m.AltStyle) AND m.AltStyle<>m.Style)
          *-- this is a sales order invoice with no Back order or there is an altrernative 
          *-- style so decrease the ordered quantity in the style file with 
          *-- the total ordered quantity for the AltStyle
          *-- Cancel the remaining quantities
          REPLACE Ord1   WITH MAX(Ord1 - OrdLine.Qty1,0),;
                  Ord2   WITH MAX(Ord2 - OrdLine.Qty2,0),;
                  Ord3   WITH MAX(Ord3 - OrdLine.Qty3,0),;
                  Ord4   WITH MAX(Ord4 - OrdLine.Qty4,0),;
                  Ord5   WITH MAX(Ord5 - OrdLine.Qty5,0),;
                  Ord6   WITH MAX(Ord6 - OrdLine.Qty6,0),;
                  Ord7   WITH MAX(Ord7 - OrdLine.Qty7,0),;
                  Ord8   WITH MAX(Ord8 - OrdLine.Qty8,0),;
                  TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
        ELSE
          *-- (Back order) so decrease the ordered quantity with the the shipped quantity
          *-- for this Style 
          *C102318,1 (Begin) If no substition happened, go as usual.
          =lfCutOver()
          *C102318,1 (End)
        ENDIF
        UNLOCK
        SELECT (lcDetFile)
        =RLOCK()
        REPLACE nSteps WITH 9
        UNLOCK
      ENDIF
      IF OrdLine.Picked .AND. &lcDetFile..nSteps < 10
        *--decrease the Alocated quantitys in the style file
        *-- with the order line Picked Quantities
        SELECT Style
        =RLOCK()
        REPLACE ALO1   WITH ALO1   - OrdLine.PIK1,;
                ALO2   WITH ALO2   - OrdLine.PIK2,;
                ALO3   WITH ALO3   - OrdLine.PIK3,;
                ALO4   WITH ALO4   - OrdLine.PIK4,;
                ALO5   WITH ALO5   - OrdLine.PIK5,;
                ALO6   WITH ALO6   - OrdLine.PIK6,;
                ALO7   WITH ALO7   - OrdLine.PIK7,;
                ALO8   WITH ALO8   - OrdLine.PIK8,;
                TOTALO WITH TOTALO - OrdLine.TOTPIK
        UNLOCK
        SELECT (lcDetFile)
        =RLOCK()      
        REPLACE nSteps WITH 10
        UNLOCK
      ENDIF
      =gfTraceKey('STYLE',IIF(!EMPTY(m.AltStyle),m.AltStyle,m.Style),'M')
      IF &lcDetFile..nSteps < 11 .AND. SEEK('O'+m.Order,'ORDHDR')
        SELECT ORDHDR
        *-- update the order header with the canceled quantity and amount
        =RLOCK()
        lnOrdShip=0
        FOR lnX= 1 TO 8
          lcSize = STR(lnX,1)
          lnOrdShip= lnOrdShip + MIN(m.Qty&lcSize,OrdLine.Qty&lcSize)
        ENDFOR
        *C102318,1 (Begin) Add the over-allocated Qty.
        lnOrdShip = lnOrdShip + lnOver
        *C102318,1 (End)
        REPLACE Cancel    WITH Cancel+IIF(m.lBackOrd,0,MAX(OrdLine.TotQty-m.TotQty,0)),;
                CancelAmt WITH CancelAmt + ;
                IIF(m.lBackOrd,0,MAX(OrdLine.TotQty-m.TotQty,0))*OrdLine.Price ,;
                Ship      WITH Ship + m.TotQty ,;
                ShipAmt   WITH ShipAmt + m.TotQty*m.Price ,;
                Open      WITH MAX(Open-IIF(m.lBackOrd, lnOrdShip,OrdLine.TotQty),0),;
                OpenAmt   WITH MAX(OpenAmt - ;
                IIF(m.lBackOrd, lnOrdShip,OrdLine.TotQty)*OrdLine.Price,0) ,;
                Status    WITH IIF(Open > 0,Status,'C')
        UNLOCK
        SELECT (lcDetFile)
        =RLOCK()      
        REPLACE nSteps WITH 11
        UNLOCK
      ENDIF
      IF OrdLine.Picked .AND. OrdLine.PikTkt <> '******' .AND. ;
        !SEEK(OrdLine.PikTkt+OrdLine.Order+STR(OrdLine.LineNo,6),'PIKLINE')
        SELECT OrdLine
        COPY NEXT 1 TO (gcWorkDir+lcTmpPikLn)
        IF ASCAN(laEvntTrig , PADR('UPDPIKLN',10)) <> 0
          = gfDoTriger('ARIINV',PADR('UPDPIKLN',10))
        ENDIF
        SELECT PIKLINE
        APPEND FROM (gcWorkDir+lcTmpPikLn)
        =gfTraceKey('PIKLINE',m.PIKTKT+m.ORDER+STR(m.LINENO,6),'A')
      ENDIF
      IF OrdLine.Picked .AND. SEEK(OrdLine.Order+OrdLine.PikTkt,'PikTkt')
        SELECT PikTkt
        =RLOCK()
        REPLACE Status WITH 'C'
        UNLOCK
        =gfTraceKey('PikTkt',OrdLine.PIKTKT,'M')
      ENDIF
      IF &lcDetFile..nSteps < 12
        SELECT OrdLine
        =RLOCK()
        *-- replace the quantities  in the Ordline file with 0 or the remaning quantities
        *-- if back order is yes
        *-- and the picked quantities with 0
        *C102318,1 (Begin) Deduct the over-allocated Qty.
        ln2Over = lnOver
        FOR lnFrom = 1 TO 8
          lcFrom = STR(lnFrom,1)
          lnCurrQty = OrdLine.Qty&lcFrom
          REPLACE Qty&lcFrom  WITH Qty&lcFrom - IIF(m.lBackOrd,;
                  IIF(ln2Over >= (OrdLine.Qty&lcFrom - m.Qty&lcFrom),Qty&lcFrom,m.Qty&lcFrom+ln2Over),Qty&lcFrom)
         ln2Over = MAX(ln2Over - IIF(lnCurrQty - m.Qty&lcFrom>=0,IIF(ln2Over >= (lnCurrQty - m.Qty&lcFrom),(lnCurrQty - m.Qty&lcFrom),ln2Over),0),0)
        ENDFOR
        REPLACE lSubstsz WITH .F.
        *C102318,1 (End)
        REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
                PICKED WITH .F.,;
                PIK1   WITH 0  ,;
                PIK2   WITH 0  ,;
                PIK3   WITH 0  ,;
                PIK4   WITH 0  ,;
                PIK5   WITH 0  ,;
                PIK6   WITH 0  ,;
                PIK7   WITH 0  ,;
                PIK8   WITH 0  ,;
                TOTPIK WITH 0  ,;
                PIKDATE WITH {} ,;
                PIKTKT WITH '' ,;
                DYELOT WITH ''
        REPLACE nPck1 WITH 0,;
                nPck2 WITH 0,;
                nPck3 WITH 0,;
                nPck4 WITH 0,;
                nPck5 WITH 0,;
                nPck6 WITH 0,;
                nPck7 WITH 0,;
                nPck8 WITH 0
        UNLOCK
        SELECT (lcDetFile)
        =RLOCK()      
        REPLACE nSteps WITH 12
        UNLOCK
        =gfTraceKey('OrdLine','O'+OrdLine.ORDER+STR(OrdLine.LINENO,6),'M')
      ENDIF
    ENDIF
  ENDIF  
  IF laInvSetup[1,2]='Y' .AND. llConsAcc      
    laGlArray[1,1] = m.Gl_Sales
    laGlArray[1,2] = '008'
    laGlArray[1,3] = -1
    laGlArray[2,1] = m.Gl_Cost
    laGlArray[2,2] = '006'
    laGlArray[2,3] = 1
    =SEEK(m.Gl_Sales+'008','GL_LINK')
    m.cCOGSAcnt = GL_LINK.GlAcnt      && Cost of goods sold account
    =SEEK(m.Gl_Cost+'006','GL_LINK')
    m.cICAcnt = GL_LINK.GlAcnt        && Inventory control account 
  ENDIF
  SELECT (lcDetFile)
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laAdjStk
  PRIVATE lcRefer
  IF EMPTY(&lcHdrFile..Order) AND &lcHdrFile..DIRECT_INV
    lcRefer = 'CUST# '+ &lcHdrFile..Account + "-" + Customer.BTName
  ELSE
    lcRefer = 'CUST# '+ &lcHdrFile..Account + " Sales order " + &lcHdrFile..Order
  ENDIF
  lnNextSt  = IIF(m.TotQty=0 OR !llConsAcc,13,;
              gfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,;
              @laAdjStk,m.Cost,lcRefer,lcGlSession,'',13,lcDetFile,'nSteps',@laGlArray,m.LineNo))
  *-- Get difference between the old stock value and the new stock value
  lnStkVal  = lnStkVal - Style.nStkVal 
  IF laInvSetup[3,2]='S'
    m.Cost = Style.TotCost
  ELSE
    m.Cost    = IIF(m.TotQty=0,0,lnStkVal/m.TotQty)
  ENDIF
  *-- Cost of goods sold amount
  lnCOGSAmt = lnCOGSAmt + m.TotQty*m.Cost 
  IF &lcDetFile..nSteps < lnNextSt .AND. llConsAcc .AND. ;
    SEEK(m.Style+lcGlYear,'icStyHst')
    *-- Shipped amount 
    lnShipAmnt = m.TotQty*m.Price &lcExRSin &lcHdrFile..NEXRATE &lcUntSin &lcHdrFile..NCURRUNIT
    *--Discount amount
    lnDiscAmnt = m.TotQty*m.Price*&lcHdrFile..DiscPcnt/100 &lcExRSin &lcHdrFile..NEXRATE &lcUntSin &lcHdrFile..NCURRUNIT
    SELECT icStyHst
    *-- update the ic style history file  for this gl period 
    =RLOCK()
    REPLACE nSlsQty&lcGlPeriod  WITH nSlsQty&lcGlPeriod  + m.TotQty   ,;
            nSlsQty             WITH nSlsQty             + m.TotQty   ,;
            nSlsAmt&lcGlPeriod  WITH nSlsAmt&lcGlPeriod  + lnShipAmnt ,;
            nSlsAmt             WITH nSlsAmt             + lnShipAmnt ,;
            nDisAmt&lcGlPeriod  WITH nDisAmt&lcGlPeriod  + lnDiscAmnt ,;
            nDisAmt             WITH nDisAmt             + lnDiscAmnt ,;
            nCOGSAmt&lcGlPeriod WITH nCOGSAmt&lcGlPeriod + m.TotQty*m.Cost  ,;
            nCOGSAmt            WITH nCOGSAmt            + m.TotQty*m.Cost  
    UNLOCK
    SELECT (lcDetFile)
    =RLOCK()
    REPLACE nSteps WITH lnNextSt
    UNLOCK
    =gfTraceKey('icStyHst',m.Style+lcGlYear,'M')
  ENDIF
  m.Store = IIF(m.Consol='Y',m.cConStore,m.Store)
  IF llConsAcc .AND. m.TotQty > 0 .AND. ;
    !SEEK(m.Invoice+STR(m.LineNo,6),'INVLINE')
    *-- update the invline with the new line
    INSERT INTO INVLINE FROM MEMVAR
  ENDIF
  IF !llConsAcc .AND. m.TotQty > 0 .AND. ;  
    !SEEK(m.Invoice+m.Store+m.Order+m.Style+STR(m.LineNo,6),'CONSINVL')
    *-- update the conslidated invoice line file
    INSERT INTO CONSINVL FROM MEMVAR
  ENDIF
ENDSCAN


*!**************************************************************************
*! Name      : lfCutOver
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 07/29/2001
*! Purpose   : Decut over allocated from open.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*!*C102318,1
FUNCTION lfCutOver

IF !ORDLINE.lSubstsz OR lnOver = 0
  REPLACE Ord1   WITH Ord1 - MIN(OrdLine.Qty1,m.Qty1),;
          Ord2   WITH Ord2 - MIN(OrdLine.Qty2,m.Qty2),;
          Ord3   WITH Ord3 - MIN(OrdLine.Qty3,m.Qty3),;
          Ord4   WITH Ord4 - MIN(OrdLine.Qty4,m.Qty4),;
          Ord5   WITH Ord5 - MIN(OrdLine.Qty5,m.Qty5),;
          Ord6   WITH Ord6 - MIN(OrdLine.Qty6,m.Qty6),;
          Ord7   WITH Ord7 - MIN(OrdLine.Qty7,m.Qty7),;
          Ord8   WITH Ord8 - MIN(OrdLine.Qty8,m.Qty8),;
          TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
ELSE
  ln2Over = lnOver
  FOR lnFrom = 1 TO 8
    lcFrom = STR(lnFrom,1)
    IF m.Qty&lcFrom >= ORDLINE.QTY&lcFrom 
      REPLACE Ord&lcFrom  WITH Ord&lcFrom - MIN(OrdLine.Qty&lcFrom,m.Qty&lcFrom)
    ELSE
      REPLACE Ord&lcFrom  WITH Ord&lcFrom - IIF(ln2Over >= (OrdLine.Qty&lcFrom - m.Qty&lcFrom),;
              Ord&lcFrom,m.Qty&lcFrom+ln2Over)
      ln2Over = ln2Over - IIF(ln2Over >= (OrdLine.Qty&lcFrom - m.Qty&lcFrom),(OrdLine.Qty&lcFrom - m.Qty&lcFrom),ln2Over)
    ENDIF
  ENDFOR
  REPLACE TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8              
ENDIF


*C200218,1 BWA 30/08/2001 Modify the custom invoice for sademara.[START]
*!*************************************************************
*! Name      : LFRECALC
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/27/2001
*! Purpose   : Recalculate the quntaties to update the invoice files.
*!*************************************************************
*! Called from  : SADMAIN.PRG
*! Program name : ALAUTPC.PRG (AUTOMATIC PACKING LIST SCREEN)
*!              : ALPLIST.PRG (Manual Packing List)
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO LFRECALC
*!*************************************************************
FUNCTION LFRECALC
PRIVATE lcAlias , lcKeyHdr , lcKeyLin , lcOrdrKey , lcKeyHtmp , lcDeletSd

lcAlias   = SELECT(0)
lcRecolct = gfTempName()
lcInvHdrs = gfTempName()
lcInvLins = gfTempName()
lcOrdLine = gfTempName()
lcCustomr = gfTempName()
lcDeletSd = SET('DELETE')
SET DELETE ON

IF !USED(lcInvHdrs)
  =gfOpenFile(gcDataDir+"INVHDRS","SDORDER", 'SH', @lcInvHdrs, .T.)
ENDIF

IF lcParam = "AUTO"
  SELECT (lcPakHdr)
  LOCATE
  GO TOP IN (lcInvHdrs)
  IF SEEK(&lcPakHdr..ORDER , lcInvHdrs)
    SELECT (lcInvHdrs)
    llPiktkt = .F.
    SCAN REST WHILE ORDER = &lcPakHdr..ORDER FOR &lcInvHdrs..Piktkt = &lcPakHdr..PACK_NO
      llPiktkt = .T.
    ENDSCAN
    IF !llPiktkt
      SELECT(lcAlias)
      RETURN
    ENDIF
  ELSE
    SELECT(lcAlias)
    RETURN
  ENDIF
ELSE
  SELECT PACK_HDR
  GO TOP IN (lcInvHdrs)
  IF SEEK(PACK_HDR.ORDER , lcInvHdrs)
    SELECT (lcInvHdrs)
    llPiktkt = .F.
    SCAN REST WHILE ORDER = PACK_HDR.ORDER FOR &lcInvHdrs..Piktkt = PACK_HDR.PACK_NO
      llPiktkt = .T.
    ENDSCAN
    IF !llPiktkt
      SELECT(lcAlias)
      RETURN
    ENDIF
  ELSE
    SELECT(lcAlias)
    RETURN
  ENDIF
ENDIF

IF !USED(lcCustomr)
  =gfOpenFile(gcDataDir+"CUSTOMER","CUSTOMER", 'SH', @lcCustomr, .T.)
ENDIF

IF !USED(lcOrdLine)
  =gfOpenFile(gcDataDir+"ORDLINE","ORDLINE", 'SH', @lcOrdLine, .T.)
ENDIF

IF !USED(lcInvLins)
  =gfOpenFile(gcDataDir+"INVLINES","INVLINES", 'SH', @lcInvLins, .T.)
ENDIF

SELECT (lcInvHdrs)
LOCATE
SET RELATION TO &lcInvHdrs..csadinv INTO (lcInvLins) ADDITIVE
SET RELATION TO 'M' + Account INTO (lcCustomr) ADDITIVE

CREATE DBF (gcWorkDir + lcRecolct) ;
        (Pack_no C(6) , Order C(6) , Piktkt C(6) , Style C(19) , Qty1 N(6) , Qty2 N(6)   , Qty3 N(6)     ,;
         Qty4 N(6)    , Qty5 N(6)  , Qty6 N(6)   , Qty7 N(6)   , Qty8 N(6) , Totqty N(7) , ChkTotal N(7) ,;
         NoCart N(5)  , Wght N(13,2))

INDEX ON Pack_no TAG Pack_no OF (gcWorkDir + lcRecolct)
INDEX ON Style + Pack_no TAG Style OF (gcWorkDir + lcRecolct)

IF lcParam = "AUTO"
  SELECT (lcPakLin)
  lcKeyLin = EVAL(KEY())

  SELECT (lcPakHdr)
  lcKeyHdr = EVAL(KEY())
  LOCATE
  SET RELATION TO 'N' + PACK_NO INTO (lcPakLin) ADDITIVE

  SCAN
    SELECT (lcPakLin)
    SCAN REST WHILE CZEROQTY + PACK_NO + STYLE + STR(NORDLINENO,6) = 'N' + &lcPakHdr..Pack_No
      SELECT (lcRecolct)
      IF SEEK(&lcPakLin..STYLE + &lcPakHdr..Pack_No , lcRecolct)
        REPLACE &lcRecolct..Qty1   WITH &lcRecolct..Qty1 + &lcPakLin..Qty1 ,;
                &lcRecolct..Qty2   WITH &lcRecolct..Qty2 + &lcPakLin..Qty2 ,;
                &lcRecolct..Qty3   WITH &lcRecolct..Qty3 + &lcPakLin..Qty3 ,;
                &lcRecolct..Qty4   WITH &lcRecolct..Qty4 + &lcPakLin..Qty4 ,;
                &lcRecolct..Qty5   WITH &lcRecolct..Qty5 + &lcPakLin..Qty5 ,;
                &lcRecolct..Qty6   WITH &lcRecolct..Qty6 + &lcPakLin..Qty6 ,;
                &lcRecolct..Qty7   WITH &lcRecolct..Qty7 + &lcPakLin..Qty7 ,;
                &lcRecolct..Qty8   WITH &lcRecolct..Qty8 + &lcPakLin..Qty8 ,;
                &lcRecolct..Totqty WITH &lcRecolct..Totqty + &lcPakLin..Qty1 + &lcPakLin..Qty2 + &lcPakLin..Qty3 + &lcPakLin..Qty4 +;
                                                             &lcPakLin..Qty5 + &lcPakLin..Qty6 + &lcPakLin..Qty7 + &lcPakLin..Qty8
      ELSE
        APPEND BLANK
        REPLACE &lcRecolct..Pack_no  WITH &lcPakHdr..Pack_No  ,;
                &lcRecolct..Order    WITH &lcPakHdr..Order    ,;
                &lcRecolct..Piktkt   WITH &lcPakHdr..Pack_No  ,;
                &lcRecolct..Style    WITH &lcPakLin..Style    ,;
                &lcRecolct..Qty1     WITH &lcPakLin..Qty1     ,;
                &lcRecolct..Qty2     WITH &lcPakLin..Qty2     ,;
                &lcRecolct..Qty3     WITH &lcPakLin..Qty3     ,;
                &lcRecolct..Qty4     WITH &lcPakLin..Qty4     ,;
                &lcRecolct..Qty5     WITH &lcPakLin..Qty5     ,;
                &lcRecolct..Qty6     WITH &lcPakLin..Qty6     ,;
                &lcRecolct..Qty7     WITH &lcPakLin..Qty7     ,;
                &lcRecolct..Qty8     WITH &lcPakLin..Qty8     ,;
                &lcRecolct..ChkTotal WITH &lcPakHdr..TOT_PCS  ,;
                &lcRecolct..NoCart   WITH &lcPakHdr..TOT_Cart ,;
                &lcRecolct..Wght     WITH &lcPakHdr..TOT_Wght ,;
                &lcRecolct..Totqty   WITH &lcRecolct..Totqty + &lcPakLin..Qty1 + &lcPakLin..Qty2 + &lcPakLin..Qty3 + &lcPakLin..Qty4 +;
                                                               &lcPakLin..Qty5 + &lcPakLin..Qty6 + &lcPakLin..Qty7 + &lcPakLin..Qty8
      ENDIF
    ENDSCAN
  ENDSCAN

  SELECT (lcPakHdr)
  SET RELATION OFF INTO (lcPakLin)

  =SEEK(lcKeyLin,lcPakLin)
  =SEEK(lcKeyHdr,lcPakHdr)
ELSE

  SELECT PACK_LIN
  lcKeyLin = EVAL(KEY())

  SELECT PACK_HDR
  lcSekPckno = PACK_HDR.PACK_NO     &&this is the value of the packing slip I get 
                                    &&it directly from the record that comes from 
                                    &&the manual packing list which stand on the 
                                    &&packing list that saved on the screen therefor 
                                    &&I mustn't change the record before that code.
  lcKeyHdr = EVAL(KEY())
  LOCATE
  SET RELATION TO PACK_NO INTO PACK_LIN ADDITIVE

  =SEEK(lcSekPckno , 'PACK_HDR')
  SCAN REST WHILE PACK_NO = lcSekPckno
    SELECT PACK_LIN
    
    SCAN REST WHILE PACK_NO + STR(NO_CART,4) + STYLE = PACK_HDR.Pack_No
      SELECT (lcRecolct)
      IF SEEK(PACK_LIN.STYLE + PACK_HDR.Pack_No , lcRecolct)
        REPLACE &lcRecolct..Qty1   WITH &lcRecolct..Qty1 + PACK_LIN.Qty1 ,;
                &lcRecolct..Qty2   WITH &lcRecolct..Qty2 + PACK_LIN.Qty2 ,;
                &lcRecolct..Qty3   WITH &lcRecolct..Qty3 + PACK_LIN.Qty3 ,;
                &lcRecolct..Qty4   WITH &lcRecolct..Qty4 + PACK_LIN.Qty4 ,;
                &lcRecolct..Qty5   WITH &lcRecolct..Qty5 + PACK_LIN.Qty5 ,;
                &lcRecolct..Qty6   WITH &lcRecolct..Qty6 + PACK_LIN.Qty6 ,;
                &lcRecolct..Qty7   WITH &lcRecolct..Qty7 + PACK_LIN.Qty7 ,;
                &lcRecolct..Qty8   WITH &lcRecolct..Qty8 + PACK_LIN.Qty8 ,;
                &lcRecolct..Totqty WITH &lcRecolct..Totqty + PACK_LIN.Qty1 + PACK_LIN.Qty2 + PACK_LIN.Qty3 + PACK_LIN.Qty4 +;
                                                             PACK_LIN.Qty5 + PACK_LIN.Qty6 + PACK_LIN.Qty7 + PACK_LIN.Qty8
      ELSE
        APPEND BLANK
        REPLACE &lcRecolct..Pack_no  WITH PACK_HDR.Pack_No  ,;
                &lcRecolct..Order    WITH PACK_HDR.Order    ,;
                &lcRecolct..Piktkt   WITH PACK_HDR.Pack_No  ,;
                &lcRecolct..Style    WITH PACK_LIN.Style    ,;
                &lcRecolct..Qty1     WITH PACK_LIN.Qty1     ,;
                &lcRecolct..Qty2     WITH PACK_LIN.Qty2     ,;
                &lcRecolct..Qty3     WITH PACK_LIN.Qty3     ,;
                &lcRecolct..Qty4     WITH PACK_LIN.Qty4     ,;
                &lcRecolct..Qty5     WITH PACK_LIN.Qty5     ,;
                &lcRecolct..Qty6     WITH PACK_LIN.Qty6     ,;
                &lcRecolct..Qty7     WITH PACK_LIN.Qty7     ,;
                &lcRecolct..Qty8     WITH PACK_LIN.Qty8     ,;
                &lcRecolct..ChkTotal WITH PACK_HDR.TOT_PCS  ,;
                &lcRecolct..NoCart   WITH PACK_HDR.TOT_Cart ,;
                &lcRecolct..Wght     WITH PACK_HDR.TOT_Wght ,;
                &lcRecolct..Totqty   WITH &lcRecolct..Totqty + PACK_LIN.Qty1 + PACK_LIN.Qty2 + PACK_LIN.Qty3 + PACK_LIN.Qty4 +;
                                                               PACK_LIN.Qty5 + PACK_LIN.Qty6 + PACK_LIN.Qty7 + PACK_LIN.Qty8
      ENDIF
    ENDSCAN
  ENDSCAN

  SELECT PACK_HDR
  SET RELATION OFF INTO PACK_LIN

  =SEEK(lcKeyLin,'PACK_LIN')
  =SEEK(lcKeyHdr,'PACK_HDR')
ENDIF

SELECT (lcRecolct)
SET RELATION TO "O" + &lcRecolct..ORDER INTO (lcOrdLine) ADDITIVE
LOCATE
SET ORDER TO TAG Pack_no
PRIVATE lcSadI
lcSadi=""
lcSadIA=""
SCAN
  SELECT (lcInvHdrs)
  IF SEEK(&lcRecolct..ORDER , lcInvHdrs)
    lcSadi=&lcInvHdrs..cSadInv
    SCAN REST WHILE ORDER = &lcRecolct..ORDER
      IF &lcRecolct..Piktkt = &lcInvHdrs..Piktkt
        lnPrice = &lcInvHdrs..ShipAmt/&lcInvHdrs..SHIP
        lnShip  = &lcInvHdrs..SHIP
        lnCart  = &lcInvHdrs..Cartons
        lnWght  = &lcInvHdrs..Weight
        lnSpamt = &lcInvHdrs..ShipAmt

        REPLACE &lcInvHdrs..SHIP     WITH &lcRecolct..ChkTotal             ,;
                &lcInvHdrs..lcompups WITH .T.                              ,;
                &lcInvHdrs..Cartons  WITH &lcRecolct..NoCart               ,;
                &lcInvHdrs..Weight   WITH &lcRecolct..Wght                 ,;
                &lcInvHdrs..ShipAmt  WITH (&lcRecolct..ChkTotal * lnPrice)

        lnShip  = &lcInvHdrs..SHIP    - lnShip
        lnCart  = &lcInvHdrs..Cartons - lnCart
        lnWght  = &lcInvHdrs..Weight  - lnWght
        lnSpamt = &lcInvHdrs..ShipAmt - lnSpamt
        lcKeyHtmp = EVAL(KEY())
        lcSadnv = &lcInvHdrs..CsadInv

                  *--Case consolidated invoice.>>Case the invhdrs
        IF &lcCustomr..CONSOL = "Y"
          SELECT (lcInvHdrs)
          IF SEEK(&lcRecolct..ORDER , lcInvHdrs)
            SCAN REST WHILE ORDER = &lcRecolct..ORDER FOR &lcInvHdrs..CsadInv = lcSadnv AND &lcInvHdrs..CONSOL = "Y" AND EMPTY(&lcInvHdrs..FLAG)
              REPLACE &lcInvHdrs..SHIP     WITH &lcInvHdrs..SHIP    + lnShip  ,;
                      &lcInvHdrs..lcompups WITH .T.                           ,;
                      &lcInvHdrs..Cartons  WITH &lcInvHdrs..Cartons + lnCart  ,;
                      &lcInvHdrs..Weight   WITH &lcInvHdrs..Weight  + lnWght  ,;
                      &lcInvHdrs..ShipAmt  WITH &lcInvHdrs..ShipAmt + lnSpamt
            ENDSCAN
          ENDIF
        ENDIF
        *=SEEK(lcKeyHtmp,lcInvHdrs)
        *LOCATE REST FOR &lcInvHdrs..Piktkt = &lcRecolct..Piktkt
*--End of Invhdr

        SELECT (lcInvLins)
        SCAN REST WHILE CSADINV = &lcInvHdrs..CSADINV FOR &lcInvLins..Piktkt = &lcRecolct..Piktkt AND &lcInvLins..STYLE = &lcRecolct..STYLE
          lnQty1 = &lcInvLins..Qty1
          lnQty2 = &lcInvLins..Qty2
          lnQty3 = &lcInvLins..Qty3
          lnQty4 = &lcInvLins..Qty4
          lnQty5 = &lcInvLins..Qty5
          lnQty6 = &lcInvLins..Qty6
          lnQty7 = &lcInvLins..Qty7
          lnQty8 = &lcInvLins..Qty8
          lnTQty = &lcInvLins..Totqty

          REPLACE &lcInvLins..Qty1   WITH &lcRecolct..Qty1 ,;
                  &lcInvLins..Qty2   WITH &lcRecolct..Qty2 ,;
                  &lcInvLins..Qty3   WITH &lcRecolct..Qty3 ,;
                  &lcInvLins..Qty4   WITH &lcRecolct..Qty4 ,;
                  &lcInvLins..Qty5   WITH &lcRecolct..Qty5 ,;
                  &lcInvLins..Qty6   WITH &lcRecolct..Qty6 ,;
                  &lcInvLins..Qty7   WITH &lcRecolct..Qty7 ,;
                  &lcInvLins..Qty8   WITH &lcRecolct..Qty8 ,;
                  &lcInvLins..Totqty WITH &lcRecolct..Totqty

          lnQty1 = &lcInvLins..Qty1   - lnQty1
          lnQty2 = &lcInvLins..Qty2   - lnQty2
          lnQty3 = &lcInvLins..Qty3   - lnQty3
          lnQty4 = &lcInvLins..Qty4   - lnQty4
          lnQty5 = &lcInvLins..Qty5   - lnQty5
          lnQty6 = &lcInvLins..Qty6   - lnQty6
          lnQty7 = &lcInvLins..Qty7   - lnQty7
          lnQty8 = &lcInvLins..Qty8   - lnQty8
          lnTQty = &lcInvLins..Totqty - lnTQty
          lcKeyTtmp = EVAL(KEY())

                  *--Case consolidated invoice.>>Case the invlines
          IF &lcCustomr..CONSOL = "Y"
            SELECT (lcInvLins)
            IF SEEK(&lcInvHdrs..CSADINV , lcInvLins)
              SCAN REST WHILE CSADINV = &lcInvHdrs..CSADINV FOR &lcInvLins..STYLE = &lcRecolct..STYLE AND EMPTY(&lcInvLins..FLAG) AND &lcInvLins..CONSOL = "Y"
                REPLACE &lcInvLins..Qty1   WITH &lcInvLins..Qty1   + lnQty1 ,;
                        &lcInvLins..Qty2   WITH &lcInvLins..Qty2   + lnQty2 ,;
                        &lcInvLins..Qty3   WITH &lcInvLins..Qty3   + lnQty3 ,;
                        &lcInvLins..Qty4   WITH &lcInvLins..Qty4   + lnQty4 ,;
                        &lcInvLins..Qty5   WITH &lcInvLins..Qty5   + lnQty5 ,;
                        &lcInvLins..Qty6   WITH &lcInvLins..Qty6   + lnQty6 ,;
                        &lcInvLins..Qty7   WITH &lcInvLins..Qty7   + lnQty7 ,;
                        &lcInvLins..Qty8   WITH &lcInvLins..Qty8   + lnQty8 ,;
                        &lcInvLins..Totqty WITH &lcInvLins..Totqty + lnTQty
              ENDSCAN
            ENDIF
          ENDIF
          *=SEEK(lcKeyTtmp,lcInvLins)
          *LOCATE REST FOR &lcInvLins..Piktkt = &lcRecolct..Piktkt
*--End of Invlines

          SELECT (lcOrdLine)
          lcOrdrKey = EVAL(KEY())
          SET ORDER TO TAG ORDLINES
          IF SEEK(&lcRecolct..STYLE , lcOrdLine)
            SCAN REST WHILE STYLE+DTOS(complete)+cordtype+order+store+STR(lineno,6) = &lcRecolct..STYLE FOR &lcOrdLine..ORDER = &lcRecolct..ORDER AND &lcOrdLine..PIKTKT = &lcRecolct..Piktkt

              lnpicked = &lcInvHdrs..Npicked
              lnTOTPIK = &lcInvLins..TOTPIK
              lnPik1   = &lcInvLins..Pik1
              lnPik2   = &lcInvLins..Pik2
              lnPik3   = &lcInvLins..Pik3
              lnPik4   = &lcInvLins..Pik4
              lnPik5   = &lcInvLins..Pik5
              lnPik6   = &lcInvLins..Pik6
              lnPik7   = &lcInvLins..Pik7
              lnPik8   = &lcInvLins..Pik8

              REPLACE &lcInvHdrs..Npicked WITH &lcOrdLine..TOTPIK ,;
                      &lcInvLins..TOTPIK  WITH &lcOrdLine..TOTPIK ,;
                      &lcInvLins..Pik1    WITH &lcOrdLine..Pik1   ,;
                      &lcInvLins..Pik2    WITH &lcOrdLine..Pik2   ,;
                      &lcInvLins..Pik3    WITH &lcOrdLine..Pik3   ,;
                      &lcInvLins..Pik4    WITH &lcOrdLine..Pik4   ,;
                      &lcInvLins..Pik5    WITH &lcOrdLine..Pik5   ,;
                      &lcInvLins..Pik6    WITH &lcOrdLine..Pik6   ,;
                      &lcInvLins..Pik7    WITH &lcOrdLine..Pik7   ,;
                      &lcInvLins..Pik8    WITH &lcOrdLine..Pik8

              lnpicked = &lcInvHdrs..Npicked - lnpicked
              lnTOTPIK = &lcInvLins..TOTPIK  - lnTOTPIK
              lnPik1   = &lcInvLins..Pik1    - lnPik1
              lnPik2   = &lcInvLins..Pik2    - lnPik2
              lnPik3   = &lcInvLins..Pik3    - lnPik3
              lnPik4   = &lcInvLins..Pik4    - lnPik4
              lnPik5   = &lcInvLins..Pik5    - lnPik5
              lnPik6   = &lcInvLins..Pik6    - lnPik6
              lnPik7   = &lcInvLins..Pik7    - lnPik7
              lnPik8   = &lcInvLins..Pik8    - lnPik8

    *--Case the change in the invhdrs for the piktkt value.
              SELECT (lcInvHdrs)
              lcKeyH2tmp = EVAL(KEY())
              IF &lcCustomr..CONSOL = "Y"
                SELECT (lcInvHdrs)
                IF SEEK(&lcRecolct..ORDER , lcInvHdrs)
                  SCAN REST WHILE ORDER = &lcRecolct..ORDER FOR &lcInvHdrs..CsadInv = lcSadnv AND &lcInvHdrs..CONSOL = "Y" AND EMPTY(&lcInvHdrs..FLAG)
                    REPLACE &lcInvHdrs..Npicked WITH &lcInvHdrs..Npicked + lnpicked
                  ENDSCAN
                ENDIF
              ENDIF
             * =SEEK(lcKeyH2tmp,lcInvHdrs)
             * LOCATE REST FOR &lcInvHdrs..Piktkt = &lcRecolct..Piktkt

    *--Case the change in the invlines for the piktkt values.
              SELECT (lcInvLins)
              lcKeyT2tmp = EVAL(KEY())
              IF &lcCustomr..CONSOL = "Y"
                SELECT (lcInvLins)
                IF SEEK(&lcInvHdrs..CSADINV , lcInvLins)
                  SCAN REST WHILE CSADINV = &lcInvHdrs..CSADINV FOR &lcInvLins..STYLE = &lcRecolct..STYLE AND EMPTY(&lcInvLins..FLAG) AND &lcInvLins..CONSOL = "Y"
                    REPLACE &lcInvLins..Pik1   WITH &lcInvLins..Pik1   + lnPik1 ,;
                            &lcInvLins..Pik2   WITH &lcInvLins..Pik2   + lnPik2 ,;
                            &lcInvLins..Pik3   WITH &lcInvLins..Pik3   + lnPik3 ,;
                            &lcInvLins..Pik4   WITH &lcInvLins..Pik4   + lnPik4 ,;
                            &lcInvLins..Pik5   WITH &lcInvLins..Pik5   + lnPik5 ,;
                            &lcInvLins..Pik6   WITH &lcInvLins..Pik6   + lnPik6 ,;
                            &lcInvLins..Pik7   WITH &lcInvLins..Pik7   + lnPik7 ,;
                            &lcInvLins..Pik8   WITH &lcInvLins..Pik8   + lnPik8 ,;
                            &lcInvLins..TOTPIK WITH &lcInvLins..TOTPIK + lnTOTPIK
                  ENDSCAN
                ENDIF
              ENDIF
              *=SEEK(lcKeyT2tmp,lcInvLins)
              *LOCATE REST FOR &lcInvLins..Piktkt = &lcRecolct..Piktkt

            ENDSCAN
          ENDIF
          SET ORDER TO TAG ORDLINE
          =SEEK(lcOrdrKey,lcOrdLine)
 
        ENDSCAN
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN

*++NAD
SELECT (lcInvHDRS)
  SET ORDER TO 1
  
  *IF SEEK (PACK_HDR.order)
  *  SCAN REST WHILE ORDER= PACK_HDR.ORDER FOR STORE=PACK_HDR.STORE
  *   lcSadi=cSadInv
  *  ENDSCAN
  *ENDIF  

IF lcParam = "AUTO"
  SELECT (lcPakHdr)
   lnPNo=RECNO()
   SCAN 
     SELECT (lcInvHDRS)
     SET ORDER TO 1
     IF SEEK (&lcPakHDr..order)
       SCAN REST WHILE ORDER= &lcPakHDr..ORDER FOR STORE=&lcPakHDr..STORE
        lcSadIA=cSadInv
       ENDSCAN
     ENDIF    
     SELECT (lcRecolct)
     SET ORDER TO 2  
     SELECT (lcInvLins)
     IF SEEK(lcSadiA , lcInvLins)
       SCAN REST WHILE CSADINV = lcSadIA
         *IF !SEEK(STYLE,lcRecolct)
         IF !SEEK(STYLE+PIKTKT,lcRecolct)
          * DELETE
           REPLACE QTY1 WITH 0
           REPLACE QTY2 WITH 0
           REPLACE QTY3 WITH 0
           REPLACE QTY4 WITH 0
           REPLACE QTY5 WITH 0
           REPLACE QTY6 WITH 0
           REPLACE QTY7 WITH 0
           REPLACE QTY8 WITH 0
           REPLACE TOTQTY WITH 0
         ENDIF
       ENDSCAN
     ENDIF
   ENDSCAN
   IF BETWEEN (lnPNo,1,RECCOUNT(lcPakHdr))
     GOTO lnPNo IN (lcPakHdr)
   ENDIF
*++NAD   

ELSE
  select (lcRecolct)
  set ORDER TO 2
  Select(lcOrdLine)
  SET ORDER TO 3
  SELECT (lcInvHDRS)
  SET ORDER TO 1
  
  llNodel=.t.
  SELECT (lcInvLins)
  IF SEEK(lcSadi , lcInvLins)
    SCAN REST WHILE CSADINV = lcSadi
      IF SEEK("O"+ &lcInvLins..order+&lcInvLins..store+&lcInvLins..style+STR(&lcInvLins..lineno,6) , lcOrdLine)
        *  SELECT (lcOrdLine)
        * LOCATE FOR STYLE+DTOS(complete)+cordtype+order+store+STR(lineno,6) = &lcInvLins..STYLE AND &lcOrdLine..ORDER = &lcInvLins..ORDER AND &lcOrdLine..PIKTKT = &lcInvLins..Piktkt
        * llDel = FOUND() AND  &lcOrdLine..totpik = 0
        llNoDel = &lcOrdLine..picked
      ENDIF
      SELECT (lcInvLins)
      IF ! llNODel
        *DELETE
        REPLACE QTY1 WITH 0
        REPLACE QTY2 WITH 0
        REPLACE QTY3 WITH 0
        REPLACE QTY4 WITH 0
        REPLACE QTY5 WITH 0
        REPLACE QTY6 WITH 0
        REPLACE QTY7 WITH 0
        REPLACE QTY8 WITH 0
        REPLACE TOTQTY WITH 0
        
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

SET DELETE &lcDeletSd

SELECT(lcAlias)
*--End of LFRECALC.
*C200218,1 BWA 30/08/2001.[END]