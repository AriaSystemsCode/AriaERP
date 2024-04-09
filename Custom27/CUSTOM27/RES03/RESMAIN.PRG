*!**************************************************************************
*! Name      : RESMAIN.PRG
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 03/07/2005
*! Purpose   : "The Restex Company, Inc." Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*C126361,1 TMI 04/17/2005 Check if this order has a release order
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
lcFromPO = ''

*:**************************************************************************
*:* Name        : lfAUTUPAPL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/07/2005
*:* Purpose     : Add a trigger to allow to update the apply field automatically for Restex
*:***************************************************************************
*:* Called from : gfMatCrl.prg ( from ARMINV )
*:***************************************************************************
*C126726,1 
FUNCTION lfAUTUPAPL
PRIVATE lnI,llAutUpd

llAutUpd = .F.
lnI = 0
DO WHILE !EMPTY(PROGRAM(lnI))
  lnI = lnI + 1
  IF PROGRAM(lnI) = 'LFWBRWRO'   && do not call this program if the refresh is called from the apply qty field valid function.
    llAutUpd = ( PROGRAM(lnI-1) $ 'MALOTRO | LFLOTROBRW' )
  ENDIF
ENDDO
IF llAutUpd .AND. NAPPLY = 0 .AND. lnTotApply < lnUsrApply
  lnOldVal = lnAppRolQt
  lnAppRolQt = MIN( lnCurRolBa , ABS(lnTotApply - lnUsrApply) )
  =lfvApply()
ENDIF

*-- end of lfAUTUPAPL.
*!**************************************************************************
*! Name      : lfCrtCursr
*! Developer : Nader NABIL (NNA)
*! Date      : 03/16/2005
*! Purpose   : Create new Cursor
*!**************************************************************************
*! Example   : lfCrtCursr()
*!**************************************************************************
*! C126356,1 NADER 03/16/2005
*!**************************************************************************
FUNCTION lfCrtCursr
lcTrgCursr = lcTmpPo + 'A'
IF !USED(lcTrgCursr)
  CREATE CURSOR (lcTrgCursr) (CREATED L(1),PO_NO C(6),INVOICE C(10),PREVINV C(10))
  INSERT INTO (lcTrgCursr)(CREATED,PO_NO,INVOICE,PREVINV) VALUES (.T.,'','','')
ENDIF
RETURN ''
*--End of Function lfCrtCursr.
*!**************************************************************************
*! Name      : lfSavinvno
*! Developer : Nader NABIL (NNA)
*! Date      : 03/16/2005
*! Purpose   : Hold the Invoice Section from the Roll Id to be Defaulted with
*!           : the Next Lines in the Same PO
*!**************************************************************************
*! Example   : lfSavinvno()
*!**************************************************************************
*! C126356,1 NADER 03/16/2005
*!**************************************************************************
FUNCTION lfSavinvNo
PRIVATE lnOldAlias , lcPoNo , lcInvoice
lnOldAlias = SELECT(0)
IF gfGetMemVar('M_GENROLID')='Y'
  lcTrgCursr = lcTmpPo + 'A'
  SELECT(lcTrgCursr)
  IF !EMPTY(&lcTrgCursr..INVOICE) AND &lcTrgCursr..INVOICE<>SUBSTR(lcRollID,1,10)
    REPLACE &lcTrgCursr..PREVINV WITH &lcTrgCursr..INVOICE
  ENDIF
  IF EVAL(lcTrgCursr+'.PO_NO') <> EVAL(lcTmpPo+'.Pomat')
    REPLACE &lcTrgCursr..PO_NO 		WITH EVAL(lcTmpPo+'.Pomat') ,;
            &lcTrgCursr..INVOICE 		WITH SUBSTR(lcRollID,1,10)
  ELSE
    IF !(EVAL(lcTrgCursr+'.INVOICE') == SUBSTR(lcRollID,1,10))
      REPLACE &lcTrgCursr..INVOICE 	WITH SUBSTR(lcRollID,1,10)
    ENDIF
  ENDIF
ENDIF  
IF USED(lcTmpRoll) AND ALLTRIM(EVAL(lcTmpRoll+'.CRollId'))='-'
  SELECT (lcTmpRoll)
  REPLACE CRollId WITH ''
  REPLACE &lcTrgCursr..INVOICE 	WITH ''
ENDIF
IF ALLTRIM(lcRollID)='-'
  lcRollID = SPACE(0)
  REPLACE &lcTrgCursr..INVOICE 	WITH ''
ENDIF
SELECT(lnOldAlias)
*--End of Function lfSavinvno.
*!**************************************************************************
*! Name      : lfRepetInv
*! Developer : Nader NABIL (NNA)
*! Date      : 03/16/2005
*! Purpose   : in the material Po receiving (by roll) -	the Roll_ID Mask'll be
*!           : "Invoice No(10 Chr)-Roll Id (9 Chr)" or (XXXXXXXXXX-XXXXXXXXX)
*!           : so The user will enter the invoice # for the first line. Then 
*!           : for the rest of the lines of the same PO the invoice # will be 
*!           : defaulted from the first line. If the PO has been changed then
*!           : the user will enter the invoice # and so on.
*!**************************************************************************
*! Example   : lfRepetInv()
*!**************************************************************************
*! C126356,1 NADER 03/16/2005
*!**************************************************************************
FUNCTION lfRepetInv
PRIVATE lnOldAlias 
lnOldAlias = SELECT(0)
lcTrgCursr = lcTmpPo + 'A'
IF gfGetMemVar('M_GENROLID')='Y'
  IF EVAL(lcTrgCursr+'.PO_NO') == EVAL(lcTmpPo+'.Pomat')
    lcRollID = EVAL(lcTrgCursr+'.INVOICE')
  ELSE
    lcRollID = ''
  ENDIF
ENDIF
IF USED(lcTmpRoll) AND ALLTRIM(EVAL(lcTmpRoll+'.CRollId'))='-'
  SELECT (lcTmpRoll)
  REPLACE CRollId WITH SPACE(0)
  lcRollID = SPACE(0)
ENDIF
SELECT(lnOldAlias)
*--End of Function lfRepetInv.

*!**************************************************************************
*! Name      : lfRemovInv
*! Developer : Nader NABIL (NNA)
*! Date      : 03/16/2005
*! Purpose   : Remove the invoice Section in the Roll_ID if the Roll_id Record
*!           : has removed 
*!**************************************************************************
*! Example   : lfRemovInv()
*!**************************************************************************
*! C126356,1 NADER 03/16/2005
*!**************************************************************************
FUNCTION lfRemovInv
PRIVATE lnOldAlias 
lnOldAlias = SELECT(0)
lcTrgCursr = lcTmpPo + 'A'
IF USED(lcTmpRoll) AND (RECCOUNT(lcTmpRoll)=0 OR EOF(lcTmpRoll))
  SELECT(lcTrgCursr)
  REPLACE &lcTrgCursr..INVOICE 	WITH ''
ENDIF
IF USED(lcTmpRoll) AND (RECCOUNT(lcTmpRoll)=0 OR EOF(lcTmpRoll))
  SELECT(lcTrgCursr)
  REPLACE &lcTrgCursr..INVOICE 	WITH ''
ENDIF
SELECT(lnOldAlias)
*--End of Function lfRemovInv
*!**************************************************************************
*! Name      : lfPrevInv
*! Developer : Nader NABIL (NNA)
*! Date      : 03/16/2005
*! Purpose   : Get the invoice Section for the Previous Roll_ID if we Removed 
*!           : the current Rollid
*!**************************************************************************
*! Example   : lfPrevInv()
*!**************************************************************************
*! C126356,1 NADER 03/16/2005
*!**************************************************************************
FUNCTION lfPrevInv
PRIVATE lnOldAlias 
lnOldAlias = SELECT(0)
lcTrgCursr = lcTmpPo + 'A'
IF USED(lcTmpRoll) AND !EOF(lcTmpRoll))
  GOTO BOTTOM IN (lcTmpRoll)
  SELECT(lcTrgCursr)
  REPLACE &lcTrgCursr..INVOICE WITH SUBSTR(EVAL(lcTmpRoll+'.cRollID'),1,10)
ENDIF
SELECT(lnOldAlias)
*--End of Function lfPrevInv


*:**************************************************************************
*:* Name        : lfRELORD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/17/2005
*:* Purpose     : Check if this order has a release order , if so , 
*:*             : do not save it from invoice sales order
*:***************************************************************************
*:* Called from : lpsavscr IN arminv.prg
*:***************************************************************************
*C126361,1
FUNCTION lfRELORD
PRIVATE lnSlct,llProceed
lnSlct = SELECT()
llProceed = .T.
IF !USED('MRELHDR')
  =gfOpenFile(gcDataDir+'MRELHDR','MRELHDRO','SH')
ENDIF
SET ORDER TO MRELHDRO IN MRELHDR
SELECT &lcInvHdr
GO TOP
SCAN FOR cSelect='»' 
  IF SEEK(&lcInvHdr..CMORDER+&lcInvHdr..STORE,'MRELHDR')
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The order '+&lcInvHdr..CMORDER+IIF(!EMPTY(&lcInvHdr..STORE),' / '+&lcInvHdr..STORE,'')+' has a release order , can not generate an invoice from it.')
    llProceed = .F.
    SELECT &lcInvHdr
    REPLACE cSelect WITH ' '
  ENDIF
ENDSCAN
GO TOP IN &lcInvHdr
LOCATE FOR cSelect='»' 
IF FOUND()
  IF !llProceed
    llProceed = (gfModalGen('INM00000B00006',.F.,.F.,.F.,'Some Orders has release orders, Ok to proceed with other orders !') = 1 )
    IF !llProceed
      llCSave = .F.  
    ENDIF
  ENDIF
ELSE
  llCSave = .F.  
ENDIF

GO TOP IN &lcInvHdr
IF EOF(lcInvLine)
  GO TOP IN &lcInvLine
ENDIF  

SELECT (lnSlct)
RETURN llProceed
*-- end of lfRELORD.

*:**************************************************************************
*:* Name        : lfADDINVHD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/19/2005
*:* Purpose     :  Add lines to MINVHDR file ( material invoice )
*:***************************************************************************
*:* Called from : ARMTINV.PRG
*:***************************************************************************
FUNCTION lfADDINVHD
PRIVATE lcSvOrd 
lcSvOrd = ORDER(lcHdrFile)

  lnInvCount = 0   && Variable to hold the number of invoices       
  SELECT (lcHdrFile)
  SET ORDER TO
  LOCATE
  SCAN
    SELECT MINVHDR
    APPEND BLANK
    =RLOCK()
    REPLACE cMatInv WITH &lcHdrFile..cMatInv
    UNLOCK
    
    lnInvCount = lnInvCount + 1
    DIMENSION &laInvoices[lnInvCount] 
    &laInvoices[lnInvCount] = &lcHdrFile..cMatInv
    
    SELECT &lcHdrFile
    REPLACE cMatInv WITH ''
  ENDSCAN  
  
  SET ORDER TO &lcSvOrd
  LOCATE
  
*-- end of lfADDINVHD.

*:**************************************************************************
*:* Name        : lfDFRELORD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/12/2005
*:* Purpose     : create a release order from the Material Invoice Sales order screen
*:***************************************************************************
*:* Called from : ARMINV.PRG
*:***************************************************************************
FUNCTION lfDFRELORD
PRIVATE lnBarNo 
lnBarNo = CNTBAR('_INQURYPOP') + 1
DEFINE BAR lnBarNo OF _INQURYPOP PROMPT 'Create Relea\<se Orders' SKIP FOR laScrMode[1] .OR. lnActFolder<>1
ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfCrRelOrd IN RESMAIN.PRG

*-- end of lfDFRELORD.

*:**************************************************************************
*:* Name        : lfCrRelOrd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/12/2005
*:* Purpose     : Create a release order
*:***************************************************************************
FUNCTION lfCrRelOrd
PRIVATE lnSlct
lnSlct = SELECT()

IF !USED('MRELHDR')
  =gfOpenFile(gcDataDir+'MRELHDR','MRELHDRO','SH')
ENDIF
SET ORDER TO MRELHDRO IN MRELHDR
SELECT &lcInvHdr
GO TOP
SCAN FOR cSelect='»'
  IF SEEK(&lcInvHdr..CMORDER+&lcInvHdr..STORE,'MRELHDR')
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The order '+&lcInvHdr..CMORDER+IIF(!EMPTY(&lcInvHdr..STORE),' / '+&lcInvHdr..STORE,'')+' has a release order , can not regenerate.')
    llProceed = .F.
    SELECT &lcInvHdr
    REPLACE cSelect WITH ' '
  ENDIF
ENDSCAN
GO TOP IN &lcInvHdr

LOCATE FOR cSelect='»'
IF !FOUND()
  SELECT (lnSlct)
  IF EOF(lcInvLine)
    GO TOP IN (lcInvLine)
  ENDIF
  _CUROBJ=OBJNUM(IBFOLDER(1))
  RETURN
ENDIF

*- Check line qty's > 0
SELECT &lcInvLine
GO TOP
LOCATE FOR FABQTY = 0
IF FOUND()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Please check that all line qty's are greater than zero.")
  RETURN
ENDIF

SET ORDER TO TAG 'DYELOT' IN (lcInvLine)
SELECT (lcInvHdr)

*-- scan only for the selected orders
PRIVATE lnPos
lnPos=RECNO(lcInvHdr)

IF BETWEEN (lnPos,1,RECCOUNT(lcInvHdr))
  GOTO lnPos IN (lcInvHDr)
ENDIF

DELETE ALL FOR cSelect+Account+cMOrder+Store+PikTkt = SPACE(1)
SET KEY TO 

*-- Array to hold invoice number
DECLARE laInv[1]
STORE '' TO laInv

*-- save the invoice
=lfReClcShp()

DO lfSavRel IN RESMAIN.PRG WITH ;
    lcInvHdr,lcInvLine,IIF(USED(lcUpsBox),lcUpsBox,''),IIF(USED(lcEngChrg),lcEngChrg,''),;
    IIF(USED(lcInstHdr),lcInstHdr,''),IIF(USED(lcInstLin),lcInstLin,''),;
    IIF(USED(lcOrdCanLn),lcOrdCanLn,''),lcGlSession,'laInv'   

SELECT unCmSess
IF SEEK('O'+PADR('MIINVOICE',10)+PADR(gcUser_id,10)+lcSession)
  REPLACE STATUS WITH 'C'
  llContinue = .F.
  UNLOCK
ENDIF

lnInvCount = ALEN(laInv)
DO CASE
  CASE EMPTY(laInv)
    llCSave = .F.
  CASE lnInvCount=1
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Material Release Sales Order has been saved as '+ laInv[1])
  OTHERWISE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Material Release Sales Orders has been saved as '+ laInv[1] + ' to ' + laInv[lnInvCount])
ENDCASE
SELECT (lcInvHdr)

*llCSave = .F.

laScrMode    = .F.
laScrMode[1] = .T.
SHOW GETS
*_CUROBJ = OBJNUM(laData[1])
_CUROBJ=OBJNUM(IBFOLDER(1))

*-- end of lfCrRelOrd.

*:**************************************************************************
*:* Name        : lfSavRel
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/12/2005
*:* Purpose     : Save release orders
*:***************************************************************************
FUNCTION lfSavRel
PARAMETERS lcHdrFile,lcDetFile,lcUpsFile,lcEngChrg,lcInstHdr,;
           lcInstLine,lcOrdCanLn,lcGlSession,laInvoices,llFromEdi

PRIVATE laInvSetup,laDRltFld,lcCustLink,lcCustSales,lcUntSin,lnInvCount,;
        lcExRSin,lcGlYear,lcGlPeriod,lnNetAmnt,lnComm1,lnComm2,lnNetShipAmnt,;
        lnCOGSAmt,lnHdrComm1,lnHdrComm2,lnAlias,;
        lcInvNo,lcOrdNo,llOrdExist,laAdjStk,lnUseQty
PRIVATE laOtherPar , lnRecCount , lnCurrRec 
lnRecCount = RECCOUNT(lcDetFile)
lnCurrRec  = 0
lnLineNo = 0

DIMENSION laOtherPar[4,2]
laOtherPar[1,1] = 'lnLineNo'
laOtherPar[1,2] = &lcDetFile..LINENO
laOtherPar[2,1] = ''
laOtherPar[2,2] = ''
laOtherPar[3,1] = 'llLastRec'
laOtherPar[3,2] = (lnRecCount = lnCurrRec)
laOtherPar[4,1] = ''
laOtherPar[4,2] = ""

lcSysType = gfGetMemVar('M_SYSTYPE',gcAct_Comp)

lnAlias = SELECT()
DECLARE laInvSetup[11,2]
laInvSetup[1,1] = 'M_LINK_GL'          &&  Check for Gl link
laInvSetup[2,1] = 'M_WareHouse'        &&  use multiple locations Y Or N  Ic Setup
laInvSetup[3,1] = 'M_MATCSTMT'         &&  Get the Fabric cost method          
laInvSetup[4,1] = 'M_DYELOT'           &&  Use Dylot Y Or N      
laInvSetup[5,1] = 'M_DIV_LINK'         &&  GL link codes at Division level
laInvSetup[6,1] = 'M_STY_COM'          &&  Commision at Style level  
laInvSetup[7,1] = 'M_TAX'              &&  use Taxes Y or N
laInvSetup[8,1] = 'XPOSTFINV'          &&  Post Factored invoice to customer
laInvSetup[9,1] = 'XAGINGTYPE'         &&  Aging AR by Date\Terms
laInvSetup[10,1]= 'M_GenOrNum'         &&  Enter order # Manually Y Or N
laInvSetup[11,1]= 'M_TrkRolls'         &&  Keep track of rolls.

=gfGetMemVar(@laInvSetup,gcAct_Comp)

IF laInvSetup[5,2]='Y'
  DECLARE laDRltFld[2,2]               && array to hold the division related fields
  laDRltFld[1,1] = 'LINK_CODE'           
  laDRltFld[1,2] = 'lcCustLink'        && Customer link   
  laDRltFld[2,1] = 'CSLSGLLINK'        
  laDRltFld[2,2] = 'lcCustSales'       && Custmer sales link code 
ENDIF

=gfOpenFile(gcDataDir+'REPCOMM', gcDataDir+'REPCOMM','SH')
=gfOpenFile(gcDataDir+'SALESREP', gcDataDir+'SALESREP','SH')

=gfOpenFile(gcDataDir+'MUPSBOX', gcDataDir+'MUPSBOX','SH')

*C126361,3  TMI [Start] 
=gfOpenFile(gcDataDir+'MRELHDR', gcDataDir+'MRELHDR','SH')
=gfOpenFile(gcDataDir+'MRELLINE', gcDataDir+'MRELLINE','SH')
*C126361,3  TMI [End  ] 

*-- Temp name for the pick line temp file
SET ORDER TO TAG MaSoLin IN MaSoLin
lnInvCount = 1    && Variable to hold the number of invoices       

SELECT (lcHdrFile)
SCAN
  DIMENSION &laInvoices[lnInvCount] 

  *-- Get The order number manualy or using gfsequence depend on the invoice setup
  lcOrdNo = IIF(EMPTY(&lcHdrFile..cMOrder) .AND. DIRECT_INV,;
            IIF(laInvSetup[10,2]='Y',lfGtOrder(),;
            gfSequence('CMORDER','','',&lcHdrFile..cDivision)),&lcHdrFile..cMOrder)
  
  IF EMPTY(&lcHdrFile..cMatInv)
    *-- Get The Invoice number on the factor sequence if the invoice was factored 
    lcInvNo=IIF(EMPTY(&lcHdrFile..cFacCode),;
            gfSequence('CMATINV','','',&lcHdrFile..cDivision),;
            gfSequence('CFMATINV' ,'','',&lcHdrFile..cDivision))
    
    lcSAlias=ALIAS()
    lnSRecno=RECNO('MINVHDR')
    
    DO WHILE SEEK(lcInvNo,'MInvHdr')
      lcInvNo=IIF(EMPTY(&lcHdrFile..cFacCode),;
           gfSequence('CMATINV','','',&lcHdrFile..cDivision),;
           gfSequence('CFMATINV' ,'','',&lcHdrFile..cDivision))             
    ENDDO 
    SELECT MRELHDR
    APPEND BLANK
    =RLOCK()
    REPLACE cMatInv with lcInvNo
    UNLOCK
    IF RECCOUNT('MInvHdr') >= lnSRecno
       GO lnSRecno IN MInvHdr
    ENDIF
    SELECT (lcSAlias)

    &laInvoices[lnInvCount] = lcInvNo
    lnInvCount = lnInvCount + 1
  ELSE
    IF TYPE("lcInvNo") # "C"
      lcInvNo=&lcHdrFile..cMatInv &&>>> 
    ELSE
      lcInvNo =IIF(&lcHdrFile..cMatInv < lcInvNo,lcInvNo,&lcHdrFile..cMatInv)
    ENDIF  
    &laInvoices[lnInvCount] = lcInvNo
  ENDIF
  
  SELECT (lcHdrFile)
  
  *-- If the invoice was consolidated invoice give the lines the same invoice number
  IF Consol='Y'
    lcAccount = Account
    lcKeyVal  = EVAL(KEY())
    REPLACE REST cMatInv WITH lcInvNo WHILE Account+cMOrder+Store+PikTkt=lcAccount
    =SEEK(lcKeyVal)
    
    LOCATE REST WHILE EVAL(KEY()) = lcKeyVal
  ENDIF  
  =RLOCK()
  REPLACE nExRate   WITH IIF(nExRate=0,1,nExRate) ,;
          nCurrUnit WITH IIF(nCurrUnit=0,1,nCurrUnit) ,;
          LastLine  WITH IIF(SEEK('O'+lcOrdNo,'MASOLIN'),LastLine,0),;
          cMatInv   WITH lcInvNo ,;
          cMOrder   WITH lcOrdNo
  UNLOCK

  STORE 0  TO lnNetAmnt,lnComm1,lnComm2,lnNetShipAmnt,lnCOGSAmt
  STORE '' TO lcGlYear,lcGlPeriod
  STORE '' TO lcCustLink,lcCustSales
  =SEEK('M'+&lcHdrFile..Account,'Customer')
  
  llConsAcc = &lcHdrFile..Direct_Inv OR &lcHdrFile..Consol='Y' OR ;
              EMPTY(&lcHdrFile..Flag)

  PRIVATE lcCDefLnk,lcSDefLnk     
  IF laInvSetup[5,2]='Y' 
  *-- the GL link was at the division level get the link code of the division     
    =gfRltFld(&lcHdrFile..cDivision,@laDRltFld,'CDIVISION')
    lcCDefLnk=IIF (EMPTY(Customer.Link_Code),'DEFDEF',Customer.Link_Code)
    lcSDefLnk=IIF (EMPTY(Customer.cSlsGlLink),'DEF',Customer.cSlsGlLink)
    lcCustLink  = IIF(EMPTY(lcCustLink),lcCDefLnk,PADR(lcCustLink,6))
    lcCustSales = IIF(EMPTY(lcCustSales),lcSDefLnk,PADR(lcCustSales,3))
  ELSE   
    lcCustLink  = IIF(EMPTY(Customer.Link_Code),'DEFDEF',Customer.Link_Code)
    lcCustSales = IIF(EMPTY(Customer.cSlsGlLink),'DEF',Customer.cSlsGlLink)
  ENDIF
  
  IF !EMPTY(&lcHdrFile..cFacCode) 
    =gfOpenFile(gcDataDir+'Factor',gcDataDir+'Factor','SH')
    IF SEEK(&lcHdrFile..cFacCode,'Factor')      
      lcCustLink =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,lcCustLink)
    ENDIF  
  ENDIF   
  
  lcUntSin   = ''  
  *-- lcExRSin  Exchange Rate sign "/" Or "*"  
  lcExRSin   = gfGetExSin(@lcUntSin,&lcHdrFile..cCurrCode)
  llOrdExist = SEEK('O'+lcOrdNo,'MaSoHdr') && In case of sales order invoice 
  *-- get the customer link code from the order header in case of sales order invoice
  lcCustLink = IIF(EMPTY(MaSoHdr.Link_Code),lcCustLink,MaSoHdr.Link_Code) 
  
  SELECT (lcDetFile)
  =SEEK(&lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..cMOrder,SPACE(6))+;
            &lcHdrFile..Store+&lcHdrFile..PikTkt)
  WAIT 'Updating Material Release Lines...' WINDOW NOWAIT

  SCAN REST WHILE Account+cMOrder+Store+PikTkt = ;
            &lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..cMOrder,SPACE(6))+;
            &lcHdrFile..Store+&lcHdrFile..PikTkt

    IF &lchdrfile..consol ='Y'
      LOCATE REST WHILE Account+cMOrder+Store+PikTkt = ;
            &lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..cMOrder,SPACE(6))+;
            &lcHdrFile..Store+&lcHdrFile..PikTkt
    ENDIF
    
    SCATTER MEMVAR MEMO
    =SEEK(m.Fabric+m.Color,'Fabric')
    =SEEK(m.Fabric+m.Color+m.cWareCode+SPACE(10),'FabDye')
    
    *-- the stock current value to use it in calculating the Fabric cost
    lnStkVal = Fabric.nStkVal

    m.CostUse   = IIF(laInvSetup[3,2]='A',;
                 IIF(laInvSetup[2,2]='Y',FabDye.nFAve_Cost,Fabric.nFAve_Cost),Fabric.CostUse)

    *-- Get the GL Cost ,GL Sales link code 
    IF laInvSetup[1,2]='Y' .AND. llConsAcc
      *-- Get Sales and Cost link codes
      =SEEK('O'+m.cMOrder+STR(m.LineNo,6),'MASOLIN')
      
      m.Gl_Cost  = IIF(!EMPTY(Fabric.Link_Code),Fabric.Link_Code,'DEFDEF')
      m.Gl_Cost  = IIF(laInvSetup[2,2]='Y' .AND. !EMPTY(FabDye.Gl_Link),FabDye.Gl_Link,m.Gl_Cost)
      *-- Concatinate customer sales link code and Fabric sales like code
      m.Gl_Sales = IIF(EMPTY(MaSoLin.Gl_Sales),lcCustSales+Fabric.cSlsGlLink,MaSoLin.Gl_Sales)

    ENDIF  
    *-- Accumulate Salesreps Commission Amount.
    IF laInvSetup[6,2]='Y'
      lnNetAmnt = ROUND(m.FabQty*m.nSellNetPr,2)*(1-&lcHdrFile..DiscPcnt/100)*(1-&lcHdrFile..Trde_Disc/100)
      lnComm1   = lnComm1 + (lnNetAmnt * m.Comm1 / 100)
      lnComm2   = lnComm2 + (lnNetAmnt * m.Comm2 / 100)
    ENDIF

    *-- Add new ivoice line
    m.Invoice  = &lcHdrFile..cMatInv
    m.cMatInv  = &lcHdrFile..cMatInv
    m.Order    = &lcHdrFile..cMOrder
    m.cMOrder  = &lcHdrFile..cMOrder
    
    *-- Calculate the equivlant amount by the company currency code
    m.nEqvAmnt = ROUND(m.nSellNetPr * m.FabQty &lcExRSin &lcHdrFile..nExRate &lcUntSin &lcHdrFile..nCurrUnit,2)
    m.InvDate  = &lcHdrFile..InvDate
    
    SELECT (lcDetFile)
    SCATTER FIELDS FabQty TO lnUseQty
    lnUseQty = lnUseQty * -1 /IIF(nSellconv=0,1,nSellconv)
    
    PRIVATE lcRefer
    IF EMPTY(&lcHdrFile..cMOrder) AND &lcHdrFile..DIRECT_INV
      lcRefer = 'CUST# '+ &lcHdrFile..Account + "-" + Customer.BTName
    ELSE
      lcRefer = 'CUST# '+ &lcHdrFile..Account + " Mat S.Order " + &lcHdrFile..cMOrder
    ENDIF

    laOtherPar[1,2] = &lcDetFile..LINENO
    lnCurrRec  = lnCurrRec + 1

    *-- Get difference between the old stock value and the new stock value
    lnStkVal  = lnStkVal - Fabric.nStkVal
    
    *-- calculate the new cost
    IF laInvSetup[3,2]='S'
      m.CostUse = Fabric.CostUse
    ELSE
      *C126361,1  TMI [Start] set costuse to 0 in case of release order
      *m.CostUSe = IIF(m.FabQty=0,0,(lnStkVal/IIF(laInvSetup[2,2]='Y',FabDye.OnHand,Fabric.onHand))/IIF(m.nSellConv=0,1,m.nSellConv))
      m.CostUSe = 0
      *C126361,1  TMI [End  ] 
    ENDIF

    *-- Cost of goods sold amount
    lnCOGSAmt = lnCOGSAmt + (m.FabQty * m.CostUse)
    
    m.Store = IIF(m.Consol='Y',m.cConStore,m.Store)
    IF llConsAcc .AND. m.FabQty > 0 
      IF !SEEK(m.cMatInv+STR(m.LineNo,6),'MRELLINE')
        INSERT INTO MRELLINE FROM MEMVAR
      ELSE
        SELECT MRELLINE
        GATHER MEMVAR MEMO
      ENDIF
    ENDIF
        
  ENDSCAN
  
  =gfTraceKey('MRELLINE',lcInvNo,'A')   
  lcStoreFld = IIF(&lcHdrFile..Consol='Y',&lcHdrFile..cConStore,&lcHdrFile..Store)       
    
  WAIT 'Updating Material Invoice Header...' WINDOW NOWAIT
  *-- shipped amount - discount - Trade discount
  lnNetShipAmnt = &lcHdrFile..FabShipAmt*(1-&lcHdrFile..DiscPcnt/100)*(1-&lcHdrFile..Trde_Disc/100)
  *-- sales rep commission 
  lnHdrComm1 = &lcHdrFile..Comm1 
  lnHdrComm2 = &lcHdrFile..Comm2
  IF laInvSetup[6,2]='Y' .AND. lnNetShipAmnt <> 0
  *-- Commission  is at style level
    lnHdrComm1 = lnComm1/lnNetShipAmnt*100
    lnHdrComm2 = lnComm2/lnNetShipAmnt*100
  ENDIF

  SELECT (lcHdrFile)
  SCATTER MEMVAR
  m.Store = IIF(m.Consol='Y',m.cConStore,m.Store)
  m.Comm1     = lnHdrComm1
  m.Comm2     = lnHdrComm2
  *-- Sales rep Commission at Style level or at invoice level   
  m.COMMAMT1  = IIF(laInvSetup[6,2]='Y',lnComm1,lnHdrComm1*lnNetShipAmnt/100)
  m.COMMAMT2  = IIF(laInvSetup[6,2]='Y',lnComm2,lnHdrComm2*lnNetShipAmnt/100)
  m.Link_Code = lcCustLink
  
  m.Invoice   = lcInvNo
  m.cMatInv   = lcInvNo
  
  IF llConsAcc .AND. SEEK(m.cMatInv,'MRELHDR')   &&.AND. EMPTY(MRELHdr.Account)
    lcSVAlias = Alias()
  
    *-- update the invoice header  
    m.Status = 'C'    
    SELECT MRELHdr
    GATHER MEMVAR MEMO
    SELECT (lcSVAlias)    
    =gfTraceKey('MINVHDR',m.cMatInv,'A')
  ENDIF
  
  IF !llConsAcc AND SEEK(m.cMatInv,'MRELHDR')
    IF !EMPTY(MaSoHdr.CustPO)
      PRIVATE lnCurAlias
      lnCurAlias = SELECT(0)
      SELECT MrelHdr
      REPLACE CustPO WITH m.CustPO
      SELECT (lnCurAlias)
    ENDIF
  ENDIF
  
ENDSCAN

WAIT CLEAR
SELECT (lnAlias)
*-- end of lfSavRel.

*:**************************************************************************
*:* Name        : lfREMRLORD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/19/2005
*:* Purpose     : if material sales order has a material release order  , then do not cancel
*:***************************************************************************
FUNCTION lfREMRLORD
PRIVATE llRet
llRet = .T.
=gfOpenFile(gcDataDir+'MRELHDR','MRELHDRO','SH')
SET ORDER TO MRELHDRO IN MRELHDR
IF SEEK(laData[1]+laData[3],'MRELHDR')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This Order has a release order, Can not cancel.')
  llRet = .F.
ENDIF

RETURN llRet
*-- end of lfREMRLORD.