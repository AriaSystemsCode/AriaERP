*!**************************************************************************
*! Name      : gpSaveInv
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/19/2002
*! Purpose   : Save new Material Invoice
*!**************************************************************************
*! Calls     : gfMatCrl(),
*!**************************************************************************
*! Passed Parameters  :  lcHdrFile  : Invoice temp. Header File
*!                       lcDetFile  : Invoice temp. Details File
*!                       lcUpsFile  : UPS Box temp. File
*!                       lcEngChrg  : Invoice Charges temp File
*!                       lcInstHdr  : Installment Header temp file 
*!                       lcInstLine : Installment Line temp file 
*!                       lcOrdCanLn : Order line canceled quantity temp file
*!                       lcGlSession: GL Session Number 
*!                       lcInvNo    : Invoice Number
*!**************************************************************************
*! Returns            :  lcInvNo    : Invoice Number
*!**************************************************************************
*! Example            :  =gpSaveInv(lcInvHdr,lcInvLine,lcUpsFile,lcEngChrg,;
*!                                  '000123','')
*!**************************************************************************
*! Modifications      :
*N000388,1 SSE 05/16/2002 New standard Material Invoice Sales Order screen Saving.
*E126684,1 TMI 03/02/2005 Round the fabric amount up ( no to use the round function )
*C126361,1 TMI 05/19/2005 Add a line for invoice from the temp file to "minvhdr" file
*!****************************************************************************************

FUNCTION gpSaveInv
PARAMETERS lcHdrFile,lcDetFile,lcUpsFile,lcEngChrg,lcInstHdr,;
           lcInstLine,lcOrdCanLn,lcGlSession,laInvoices,llFromEdi

PRIVATE laInvSetup,laDRltFld,lcCustLink,lcCustSales,lcUntSin,lnInvCount,;
        lcExRSin,lcGlYear,lcGlPeriod,lnNetAmnt,lnComm1,lnComm2,lnNetShipAmnt,;
        lnCOGSAmt,lnHdrComm1,lnHdrComm2,lnAlias,lcDistFile,;
        lcInvNo,lcOrdNo,llOrdExist,laGlArray,laAdjStk

*N000388,1 ABD Define variable used in Lot costing method. [Begin]
PRIVATE lnUseQty
*N000388,1 ABD Define variable used in Lot costing method. [End]

*N000388,1 Define 2 variables for MatInvJl & Rolls. [Begin]
PRIVATE llExtCall 
llExtCall = .T.

PRIVATE laOtherPar , lnRecCount , lnCurrRec 
lnRecCount = RECCOUNT(lcDetFile)
lnCurrRec  = 0
*B606163,1 ABD - Define the line No. [Begin]
lnLineNo = 0
*B606163,1 ABD - [End]

DIMENSION laOtherPar[4,2]
laOtherPar[1,1] = 'lnLineNo'
laOtherPar[1,2] = &lcDetFile..LINENO
laOtherPar[2,1] = ''
laOtherPar[2,2] = ''
laOtherPar[3,1] = 'llLastRec'
laOtherPar[3,2] = (lnRecCount = lnCurrRec)
laOtherPar[4,1] = ''
laOtherPar[4,2] = ""
*N000388,1 Define 2 variables for MatInvJl & Rolls. [End]

lcSysType = gfGetMemVar('M_SYSTYPE',gcAct_Comp)

DECLARE laGlArray[2,13]

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

*N000388,1 Commented out. [Begin]
*=IIF('AL' $ gcCmpModules,gfOpenFile(gcDataDir+'PIKLINE',gcDataDir+'PIKLINE','SH'),.T.)
*N000388,1 Commented out. [End]

=IIF(laInvSetup[1,2]='Y',gfOpenFile(gcDataDir+'GLDIST', gcDataDir+'GLDISTNO', 'SH'),.T.)

*N000388,1 Commented out. (There is no Material History file) [Begin]
*=gfOpenFile(gcDataDir+'icStyHst', gcDataDir+'Styhst','SH')
*N000388,1 Commented out. (There is no Material History file) [End]

=gfOpenFile(gcDataDir+'arCusHst', gcDataDir+'Acthst','SH')
=gfOpenFile(gcDataDir+'DEBIT', gcDataDir+'DEBIT','SH')
=gfOpenFile(gcDataDir+'MCONSINH', gcDataDir+'MCONSINH','SH')
=gfOpenFile(gcDataDir+'MCONSINL', gcDataDir+'MCONSINL','SH')
=gfOpenFile(gcDataDir+'REPCOMM', gcDataDir+'REPCOMM','SH')
=gfOpenFile(gcDataDir+'SALESREP', gcDataDir+'SALESREP','SH')
IF !EMPTY(lcOrdCanLn)
  =gfOpenFile(gcDataDir+'MORDCNLN', gcDataDir+'MORDCNLN','SH')
ENDIF
=gfOpenFile(gcDataDir+'MUPSBOX', gcDataDir+'MUPSBOX','SH')
=gfOpenFile(gcDataDir+'GL_LINK', gcDataDir+'GL_LINK','SH')

*N000388,1 Commented out. [Begin]
*-- flag to Check if the edi files was open or not
*llEdiAcPrt = ('EB' $ gcCmpModules OR 'NC' $ gcCmpModules ) .AND. gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
*llEdiPD    = ('EB' $ gcCmpModules OR 'NC' $ gcCmpModules ) .AND. gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
*llEdiTrans = ('EB' $ gcCmpModules OR 'NC' $ gcCmpModules ) .AND. gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
*llPackHdr = 'EB' $ gcCmpModules .AND. gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH')
*llBolHdr  = 'EB' $ gcCmpModules .AND. gfOpenFile(gcDataDir+'BOL_HDR',gcDataDir+'BOL_HDR','SH')
*N000388,1 Commented out. [End]

*-- Temp name for the pick line temp file
lcTmpPikLn = gfTempName()
SET ORDER TO TAG MaSoLin IN MaSoLin
SET ORDER TO TAG DEBIT IN DEBIT
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
    SELECT MInvHdr
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
      lcInvNo=&lcHdrFile..cMatInv
    ELSE
      lcInvNo =IIF(&lcHdrFile..cMatInv < lcInvNo,lcInvNo,&lcHdrFile..cMatInv)
    ENDIF  
    &laInvoices[lnInvCount] = lcInvNo
  ENDIF
   
  *C126361,1  TMI [Start] Add a line for invoice from the temp file to "minvhdr" file
  IF ASCAN(laEvntTrig , PADR('ADDINVHD',10)) <> 0
    =gfDoTriger('ARMREL',PADR('ADDINVHD',10))
  ENDIF
  *C126361,1  TMI [End  ]

  SELECT (lcHdrFile)
  
  *-- If the invoice was consolidated invoice give the lines the same invoice number
  IF Consol='Y'
    lcAccount = Account
    lcKeyVal  = EVAL(KEY())
    *lcDiv     = cDivision
    *REPLACE REST cMatInv WITH lcInvNo WHILE Account+cMOrder+Store+PikTkt=lcAccount;
    *        FOR  cDivision = lcDiv
    REPLACE REST cMatInv WITH lcInvNo WHILE Account+cMOrder+Store+PikTkt=lcAccount
    =SEEK(lcKeyVal)
    
    *LOCATE REST WHILE EVAL(KEY()) = lcKeyVal FOR cDivision = lcDiv
    LOCATE REST WHILE EVAL(KEY()) = lcKeyVal
  ENDIF  
  =RLOCK()
  REPLACE nExRate   WITH IIF(nExRate=0,1,nExRate) ,;
          nCurrUnit WITH IIF(nCurrUnit=0,1,nCurrUnit) ,;
          LastLine  WITH IIF(SEEK('O'+lcOrdNo,'MASOLIN'),LastLine,0),;
          cMatInv   WITH lcInvNo ,;
          cMOrder   WITH lcOrdNo
  UNLOCK
  IF &lcHdrFile..Direct_inv AND USED (lcUpsFile)
    PRIVATE lcLastFile,lcUPSOrd
    lcUPSOrd   = ORDER(lcUpsFile)
    lcLastFile = ALIAS()
    SELECT (lcUpsFile)
    SET ORDER TO
    REPLACE ALL cMOrder WITH lcOrdNo
    SET ORDER TO &lcUPSOrd
    SELECT  (lcLastFile)
  ENDIF

  STORE '' TO lcGlYear,lcGlPeriod
  *--Check Post Date if valid  in the gl period or not
  =CHECKPRD(&lcHdrFile..dPostDate,'lcGlYear','lcGlPeriod','IN',.T.)
  
  IF laInvSetup[1,2]='Y'
  *-- the system linked to GL creat temp GL file
    lcDistFile = gfTempName()
    SELECT GLDIST
    =AFIELDS(laFileStru)
    =gfCrtTmp(lcDistFile,@laFileStru,.F.,'')
  ENDIF
  *-- lnNetAmnt  total amount per line to calculate the sales rep commission
  *-- lnComm1,lnComm2 sales rep commissions
  STORE 0  TO lnNetAmnt,lnComm1,lnComm2,lnNetShipAmnt,lnCOGSAmt
  STORE '' TO lcCustLink,lcCustSales
  =SEEK('M'+&lcHdrFile..Account,'Customer')
  
  *-- Direct invoice
  *--   In the invhdr file you will find 
  *--     One line  
  *--     Fields   Direct_inv = .t.  ,Consol = N  ,Flag = ' '  llConsAcc=.T.
  *--   In the invline 
  *--     One line for each Style    
  *-- Invoice from sales order 		
  *--   1- Consolidated invoice
  *--	  In the invhdr file you will find 
  *--       One line with the total invoice 
  *--       Fields   Direct_inv = .f.  ,Consol = Y ,Flag = ' '  llConsAcc=.T.
  *--       One line for each store 
  *--       Fields   Direct_inv = .f.  ,Consol = N ,Flag ='0'   llConsAcc=.F.
  *--       in this case that the transaction will not  be updated with these lines 
  *--     In the invline 
  *--       One line for each Style 
  *--       One line for each Store + Style
  *--   2- Not Consolidated
  *--     In the invhdr file you will find 
  *--       One line for each store
  *--       Fields  Direct_inv = .f.  , Consol = Y, Flag = ' '     llconsacc=.T.
  *--     In the invline 
  *--       One line for each Store + Style
  *-- Note: if llConsAcc =.T. This means that the transaction will be updated with these lines 
  
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
  
  STORE '' TO laGlArray       && Arry to hold GL information needed by gfStyCntl function
  IF laInvSetup[1,2]='Y' .AND. llConsAcc
    STORE 'MI'                  TO laGlArray[1,4],laGlArray[2,4]
    STORE lcInvNo               TO laGlArray[1,5],laGlArray[2,5]   && invoice number
    STORE &lcHdrFile..dPostDate TO laGlArray[1,6],laGlArray[2,6]   && Post Date
    STORE lcGlYear              TO laGlArray[1,7],laGlArray[2,7]   && Year
    STORE lcGlPeriod            TO laGlArray[1,8],laGlArray[2,8]   && GlPeriod
    STORE lcDistFile            TO laGlArray[1,9],laGlArray[2,9]   && glDist temp file
    STORE ''                    TO laGlArray[1,10],laGlArray[2,10] && 
    STORE &lcHdrFile..cCurrCode TO laGlArray[1,11],laGlArray[2,11] && Currency 
    STORE &lcHdrFile..nCurrUnit TO laGlArray[1,12],laGlArray[2,12] && Currency unit
    STORE &lcHdrFile..nExRate   TO laGlArray[1,13],laGlArray[2,13] && Exchange rate 
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
  WAIT 'Updating Material Invoice Lines...' WINDOW NOWAIT
  
  *N000388,1 SSE Commented out. [Begin]
  *-- If the EDI module is installed and the invoice is a Direct invoice
  *IF 'EB' $ gcCmpModules AND !&lcHdrFile..Direct_Inv AND ;
  *  SEEK('A'+&lcHdrFile..Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'856','EDIPD') AND ;
  *  SEEK(&lcHdrFile..cMOrder+&lcHdrFile..Store+&lcHdrFile..PikTkt,'PACK_HDR') AND ;
  *  SEEK(Pack_Hdr.Bill_Ladg,'BOL_HDR')
  *  IF BOL_HDR.Status <> "C"
  *    SELECT BOL_HDR
  *    REPLACE Status    WITH 'C' ,;
  *            Ship_Date WITH gdSysDate ,;
  *            cShipTime WITH TIME()
  *    SELECT EDITRANS
  *    IF !SEEK('856'+PADR(PACK_HDR.Bill_Ladg,20)+'A'+&lcHdrFile..Account)
  *      INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER) VALUES ;
  *      ('856',PACK_HDR.Bill_Ladg,'A',&lcHdrFile..Account)
  *    ENDIF
  *    REPLACE cStatus WITH 'N'
  *    =gfAdd_Info('EDITRANS')
  *  ENDIF
  *  SELECT PACK_HDR
  *  REPLACE Ship_Date WITH gdSysDate ,;
  *          Status    WITH 'C'
  *  SELECT (lcDetFile)
  *ENDIF
  *N000388,1 SSE Commented out. [End]

  SCAN REST WHILE Account+cMOrder+Store+PikTkt = ;
            &lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..cMOrder,SPACE(6))+;
            &lcHdrFile..Store+&lcHdrFile..PikTkt

    IF &lchdrfile..consol ='Y'
      *LOCATE REST WHILE Account+cMOrder+Store+PikTkt = ;
      *      &lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..cMOrder,SPACE(6))+;
      *      &lcHdrFile..Store+&lcHdrFile..PikTkt;
      *      FOR cDivision = &lcHdrFile..cDivision
      LOCATE REST WHILE Account+cMOrder+Store+PikTkt = ;
            &lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..cMOrder,SPACE(6))+;
            &lcHdrFile..Store+&lcHdrFile..PikTkt
    ENDIF
    
    SCATTER MEMVAR MEMO
    =SEEK(m.Fabric+m.Color,'Fabric')
    =SEEK(m.Fabric+m.Color+m.cWareCode+SPACE(10),'FabDye')
    *-- the stock current value to use it in calculating the Fabric cost
    lnStkVal = Fabric.nStkVal

    *-- Calculate the avarage cost of the Fabric due to the cost method and warehouse   
    *m.Cost   = IIF(laInvSetup[3,2]='A',;
    *             IIF(laInvSetup[2,2]='Y',StyDye.Ave_Cost,Style.Ave_Cost),Style.TotCost)
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

      *-- Sales Revenus <CREDIT> Category key '003'
      *-- Update the G/L Distribution file 
      *E126684,1  TMI [Start] round the line amount up not to take the round only  (Sales Revenue)
      *DO GLDIST WITH m.Gl_Sales,'003',-ROUND((m.FabQty * m.nSellNetPr),2),'MI',lcInvNo,;
                     &lcHdrFile..dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
                     &lcHdrFile..cCurrCode,&lcHdrFile..nCurrUnit,&lcHdrFile..nExRate
      DO GLDIST WITH m.Gl_Sales,'003',-(CEILING(m.FabQty * m.nSellNetPr * 100)/100),'MI',lcInvNo,;
                     &lcHdrFile..dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
                     &lcHdrFile..cCurrCode,&lcHdrFile..nCurrUnit,&lcHdrFile..nExRate
      *E126684,1  TMI [End  ] 
      m.cSalesAcnt = &lcDistFile..GlAccount   && Get the sales Revenus account 
 
      IF &lcHdrFile..FabShipAmt # 0
        *-- Update the G/L Distribution file 
        *E126684,1  TMI [Start] round the value "m.FabQty * m.nSellNetPr" then take the discount  (Discount)
        *DO GLDIST WITH m.Gl_Sales,'005',(ROUND(m.FabQty * m.nSellNetPr * ABS(&lcHdrFile..Discount),2)/&lcHdrFile..FabShipAmt),;
                       'MI',lcInvNo,&lcHdrFile..dPostDate,lcGlYear,lcGlPeriod,;
                       lcDistFile,'',&lcHdrFile..cCurrCode,&lcHdrFile..nCurrUnit,&lcHdrFile..nExRate
        DO GLDIST WITH m.Gl_Sales,'005',(ROUND((CEILING(m.FabQty * m.nSellNetPr*100)/100) * ABS(&lcHdrFile..Discount),2)/&lcHdrFile..FabShipAmt),;
                       'MI',lcInvNo,&lcHdrFile..dPostDate,lcGlYear,lcGlPeriod,;
                       lcDistFile,'',&lcHdrFile..cCurrCode,&lcHdrFile..nCurrUnit,&lcHdrFile..nExRate
        *E126684,1  TMI [End  ] 
      ENDIF
      
      *--  Don't update cDicAcnt field in InvLine file if there no discount 
      IF (ROUND(m.FabQty * m.nSellNetPr * ABS(&lcHdrFile..Discount),2)/&lcHdrFile..FabShipAmt) > 0
        m.cDiscAcnt = &lcDistFile..GlAccount
      ENDIF
    ENDIF  
    IF &lcDetFile..nSteps < 1 AND laInvSetup[4,2]='Y' AND llConsAcc AND ;
        Fabric.cDye_Flg = 'Y' AND SEEK(m.Fabric+m.Color+m.cWareCode+m.Dyelot,'FabDye')
      SELECT FabDye
      =RLOCK()
      REPLACE nSellShip WITH nSellShip + (m.FabQty*IIF(m.nSellconv=0,1,m.nSellconv))
      UNLOCK
      =gfTraceKey('FABDYE',m.Fabric+m.Color+m.cWareCode+m.Dyelot,'M')
      SELECT (lcDetFile)
      =RLOCK()
      *-- flag to say that this line was updated in the last session or not
      REPLACE nSteps WITH 1    
      
      UNLOCK
    ENDIF
    IF &lcDetFile..nSteps < 2 .AND. llConsAcc .AND. ;
       SEEK(m.Fabric+m.Color+m.cWareCode+SPACE(10),'FabDye')
      SELECT FabDye
      =RLOCK()
      REPLACE nSellShip WITH nSellShip + (m.FabQty*IIF(m.nSellconv=0,1,m.nSellconv))
      UNLOCK
      =gfTraceKey('FABDYE',m.Fabric+m.Color+m.cWareCode+SPACE(10),'M')
      SELECT (lcDetFile)
      =RLOCK()
      REPLACE nSteps WITH 2
      
      UNLOCK
      
    ENDIF
    IF &lcDetFile..nSteps < 3 .AND. llConsAcc
      SELECT Fabric
      =RLOCK()
      REPLACE nSellShip WITH nSellShip + (m.FabQty*IIF(m.nSellconv=0,1,m.nSellconv))
      UNLOCK
      =gfTraceKey('Fabric',m.Fabric+m.Color,'M')
      SELECT (lcDetFile)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
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
    
    
    IF Consol <> 'Y' AND &lcDetFile..nSteps < 12
      *-- if new order line or the line not found ad new line to the MASOLIN File 
      IF (!&lcHdrFile..Direct_Inv AND &lcDetFile..lNewLine) OR ; 
         !SEEK('O'+m.cMOrder+STR(m.LineNo,6),'MASOLIN')
        IF SEEK('O'+lcOrdNo,'MaSoHdr')
        *-- This is a new sales oreder line so update the order header 
          SELECT 'MaSoHdr'
          =RLOCK()
          *-- update the order header with the new line
          REPLACE LastLine WITH LastLine + 1 ,;
                  FabBook     WITH FabBook     + m.FabQty ,;
                  FabBookAmt  WITH FabBookAmt  + ROUND(m.FabQty*m.nSellNetPr,2) ,;
                  FabShip  WITH FabShip   + m.FabQty ,;
                  FabShipAmt  WITH FabShipAmt  + ROUND(m.FabQty*m.nSellNetPr,2)
          UNLOCK
        ELSE  
          SELECT (lcHdrFile)
          =RLOCK()
          REPLACE LastLine WITH LastLine + 1
          UNLOCK
        ENDIF
        m.LineNo=LastLine
        *-- in case of direct invoice add new line in the order line 
        INSERT INTO MaSoLin ;
        (cOrdType,cMOrder,Account,LineNo,Fabric,Color,Group,nSellPrice,Disc_Pcnt,;
        nSellNetPr,Start,Complete,Comm1,Comm2,Note_Mem,Desc1,FabBook,;
        PrePak,PPQty,cWareCode,Store) VALUES ;
        ('O',m.cMOrder,m.Account,m.LineNo,m.Fabric,m.Color,m.Group,m.nSellPrice,;
         m.Disc_Pcnt,m.nSellNetPr,&lcHdrFile..InvDate,&lcHdrFile..InvDate,m.Comm1,;
         m.Comm2,m.Note_Mem,m.Desc1,m.FabQty,m.PrePak,m.PPQty,m.cWareCode,m.Store)
        SELECT (lcDetFile)
        =RLOCK()      
        REPLACE nSteps WITH 12
        UNLOCK
      ELSE                
        IF laInvSetup[4,2]='Y' .AND. Fabric.cDye_Flg='Y' .AND. ;
          !EMPTY(MaSoLin.Dyelot) AND SEEK(m.Fabric+m.Color+m.cWareCode+MaSoLin.Dyelot,'FabDye')
          IF &lcDetFile..nSteps < 5 
            SELECT FabDye
            =RLOCK()
            IF !m.lBackOrd OR (!EMPTY(MaSoLin.Dyelot) AND MaSoLin.Dyelot<>m.Dyelot)
            *-- this is a sales order invoice and no Back order so decrease the ordered quantity
            *-- in the FabDye file with the total ordered quantity for this Style
              REPLACE nSellOrder WITH MAX(nSellOrder - (MaSoLin.FabQty*IIF(m.nSellconv=0,1,m.nSellconv)),0)
            ELSE
            *-- (Back order) so decrease the ordered quantity with the the shipped quantity
            *-- for this Style            
              REPLACE nSellOrder WITH nSellOrder - (MIN(MaSoLin.FabQty,m.FabQty)*IIF(m.nSellconv=0,1,m.nSellconv))
            ENDIF
            UNLOCK
            SELECT (lcDetFile)
            =RLOCK()
            REPLACE nSteps WITH 5
            UNLOCK
          ENDIF

          *--decrease the Alocated quantitys in the FabDye file
          *-- with the order line Picked Quantities
          *N000388,1 SSE Commented out. [Begin]
          *IF MaSoLin.Picked .AND. &lcDetFile..nSteps < 6
          *  SELECT FabDye
          *  =RLOCK()
          *  REPLACE ALO1   WITH ALO1   - OrdLine.PIK1,;
          *          ALO2   WITH ALO2   - OrdLine.PIK2,;
          *          ALO3   WITH ALO3   - OrdLine.PIK3,;
          *          ALO4   WITH ALO4   - OrdLine.PIK4,;
          *          ALO5   WITH ALO5   - OrdLine.PIK5,;
          *          ALO6   WITH ALO6   - OrdLine.PIK6,;
          *          ALO7   WITH ALO7   - OrdLine.PIK7,;
          *          ALO8   WITH ALO8   - OrdLine.PIK8,;
          *          TOTALO WITH TOTALO - OrdLine.TOTPIK
          *  UNLOCK
          *  SELECT (lcDetFile)
          *  =RLOCK()        
          *  REPLACE nSteps WITH 6
          *  UNLOCK
          *ENDIF
          *=gfTraceKey('FABDYE',m.Fabric+m.Color+m.cWareCode+MaSoLin.Dyelot,'M')
          *N000388,1 SSE Commented out. [Begin]
        ENDIF

        IF SEEK(IIF(!EMPTY(m.AltFabric),m.AltFabric,m.Fabric)+IIF(!EMPTY(m.AltColor),m.AltColor,m.Color)+m.cWareCode+SPACE(10),'FabDye')
          IF &lcDetFile..nSteps < 7
            SELECT FabDye
            =RLOCK()
            *-- before saving any issue invoice if the Fabric was changed by the user 
            *-- the old style will be saved in the AltFabric field 
            IF !m.lBackOrd OR (!EMPTY(m.AltFabric) AND !EMPTY(m.AltColor) AND (m.AltFabric<>m.Fabric OR m.AltColor<>m.Color))
            *-- this is a sales order invoice with no Back order or there is an altrernative 
            *-- Fabric so decrease the ordered quantity in the FabDye file with 
            *-- the total ordered quantity for the AltFabric
              REPLACE nSellOrder WITH MAX(nSellOrder - (MaSoLin.FabQty*IIF(m.nSellconv=0,1,m.nSellconv)),0)
            ELSE
            *-- (Back order) so decrease the ordered quantity with the the shipped quantity
            *-- for this Style 
              REPLACE nSellOrder WITH nSellOrder - (MIN(MaSoLin.FabQty,m.FabQty)*IIF(m.nSellconv=0,1,m.nSellconv))
            ENDIF
            UNLOCK
            SELECT (lcDetFile)
            =RLOCK()
            REPLACE nSteps WITH 7
            UNLOCK
          ENDIF
          
          *N000388,1 SSE Commented out. [Begin]
          *IF MaSoLin.Picked .AND. &lcDetFile..nSteps < 8
            *--decrease the Alocated quantitys in the FabDye file
            *-- with the order line Picked Quantities
            
            *SELECT FabDye
            *=SEEK(IIF(!EMPTY(m.AltFabric),m.AltFabric,m.Fabric)+IIF(!EMPTY(m.AltColor),m.AltColor,m.Color)+m.cWareCode+SPACE(10))
            *=RLOCK()

            *REPLACE ALO1   WITH ALO1   - OrdLine.PIK1,;
            *        ALO2   WITH ALO2   - OrdLine.PIK2,;
            *        ALO3   WITH ALO3   - OrdLine.PIK3,;
            *        ALO4   WITH ALO4   - OrdLine.PIK4,;
            *        ALO5   WITH ALO5   - OrdLine.PIK5,;
            *        ALO6   WITH ALO6   - OrdLine.PIK6,;
            *        ALO7   WITH ALO7   - OrdLine.PIK7,;
            *        ALO8   WITH ALO8   - OrdLine.PIK8,;
            *        TOTALO WITH TOTALO - OrdLine.TOTPIK
            *UNLOCK
            *SELECT (lcDetFile)
            *=RLOCK()        
            *REPLACE nSteps WITH 8
            *UNLOCK
          *ENDIF          
          *=gfTraceKey('FABDYE',IIF(!EMPTY(m.AltFabric),m.AltFabric,m.Fabric)+IIF(!EMPTY(m.AltColor),m.AltColor,m.Color)+m.cWareCode+SPACE(10),'M')
          *N000388,1 SSE Commented out. [Begin]
        ENDIF  
        =SEEK(IIF(!EMPTY(m.AltFabric),m.AltFabric,m.Fabric)+IIF(!EMPTY(m.AltColor),m.AltColor,m.Color),'Fabric')
        IF &lcDetFile..nSteps < 9
          SELECT Fabric
          =RLOCK()
          IF !m.lBackOrd .OR. (!EMPTY(m.AltFabric) AND !EMPTY(m.AltColor) AND (m.AltFabric<>m.Fabric OR m.AltColor<>m.Color))
            *-- this is a sales order invoice with no Back order or there is an altrernative 
            *-- Fabric so decrease the ordered quantity in the Fabric file with 
            *-- the total ordered quantity for the AltFabric
            *-- Cancel the remaining quantities
            REPLACE nSellOrder WITH MAX(nSellOrder - (MaSoLin.FabQty*IIF(m.nSellconv=0,1,m.nSellconv)),0)
          ELSE
            *-- (Back order) so decrease the ordered quantity with the the shipped quantity
            *-- for this Fabric 
            REPLACE nSellOrder WITH nSellOrder - (MIN(MaSoLin.FabQty,m.FabQty)*IIF(m.nSellconv=0,1,m.nSellconv))
          ENDIF
          UNLOCK
          SELECT (lcDetFile)
          =RLOCK()
          REPLACE nSteps WITH 9
          UNLOCK
        ENDIF
        *N000388,1 SSE Commented out. [Begin]
        *IF MaSoLin.Picked .AND. &lcDetFile..nSteps < 10
          *--decrease the Alocated quantitys in the style file
          *-- with the order line Picked Quantities

          *=SEEK(IIF(!EMPTY(m.AltFabric),m.AltFabric,m.Fabric)+IIF(!EMPTY(m.AltColor),m.AltColor,m.Color),'Fabric')
          
          *SELECT Fabric
          =RLOCK()
          *REPLACE ALO1   WITH ALO1   - OrdLine.PIK1,;
          *        ALO2   WITH ALO2   - OrdLine.PIK2,;
          *        ALO3   WITH ALO3   - OrdLine.PIK3,;
          *        ALO4   WITH ALO4   - OrdLine.PIK4,;
          *        ALO5   WITH ALO5   - OrdLine.PIK5,;
          *        ALO6   WITH ALO6   - OrdLine.PIK6,;
          *        ALO7   WITH ALO7   - OrdLine.PIK7,;
          *        ALO8   WITH ALO8   - OrdLine.PIK8,;
          *        TOTALO WITH TOTALO - OrdLine.TOTPIK
          *UNLOCK
          *SELECT (lcDetFile)
          *=RLOCK()      
          *REPLACE nSteps WITH 10
          *UNLOCK
        *ENDIF
        *=gfTraceKey('Fabric',IIF(!EMPTY(m.AltFabric),m.AltFabric,m.Fabric)++IIF(!EMPTY(m.AltColor),m.AltColor,m.Color),'M')
        *N000388,1 SSE Commented out. [End]
        IF &lcDetFile..nSteps < 11 .AND. SEEK('O'+m.cMOrder,'MaSoHdr')        
          SELECT MaSoHdr
          *-- update the order header with the canceled quantity and amount
          =RLOCK()
          lnOrdShip=0
          lnOrdShip = lnOrdShip + MIN(m.FabQty,MaSoLin.FabQty)
          *E126684,1  TMI [Start] round the value "m.FabQty*m.nSellNetPr" up
          *REPLACE FabCancel    WITH FabCancel+IIF(m.lBackOrd,0,MAX(MaSoLin.FabQty-m.FabQty,0)),;
                  FabCanAmt WITH FabCanAmt + ;
                  ROUND(IIF(m.lBackOrd,0,MAX(MaSoLin.FabQty-m.FabQty,0))*MaSoLin.nSellNetPr,2) ,;
                  FabShip      WITH FabShip + m.FabQty ,;
                  FabShipAmt   WITH FabShipAmt + ROUND(m.FabQty*m.nSellNetPr,2) ,;
                  FabOpen      WITH MAX(FabOpen-IIF(m.lBackOrd, lnOrdShip,MaSoLin.FabQty),0),;
                  FabOpenAmt   WITH MAX(FabOpenAmt - ;
                  ROUND(IIF(m.lBackOrd, lnOrdShip,MaSoLin.FabQty)*MaSoLin.nSellNetPr,2),0) ,;
                  Status    WITH IIF(FabOpen > 0,Status,'C')
          REPLACE FabCancel    WITH FabCancel+IIF(m.lBackOrd,0,MAX(MaSoLin.FabQty-m.FabQty,0)),;
                  FabCanAmt WITH FabCanAmt + ;
                  CEILING(IIF(m.lBackOrd,0,MAX(MaSoLin.FabQty-m.FabQty,0))*MaSoLin.nSellNetPr*100)/100 ,;
                  FabShip      WITH FabShip + m.FabQty ,;
                  FabShipAmt   WITH FabShipAmt + CEILING(m.FabQty*m.nSellNetPr*100)/100 ,;
                  FabOpen      WITH MAX(FabOpen-IIF(m.lBackOrd, lnOrdShip,MaSoLin.FabQty),0),;
                  FabOpenAmt   WITH MAX(FabOpenAmt - ;
                  CEILING(IIF(m.lBackOrd, lnOrdShip,MaSoLin.FabQty)*MaSoLin.nSellNetPr*100)/100,0) ,;
                  Status    WITH IIF(FabOpen > 0,Status,'C')
          *E126684,1  TMI [End  ] 
          UNLOCK
          
          SELECT (lcDetFile)
          =RLOCK()      
          REPLACE nSteps WITH 11
          UNLOCK
        ENDIF
        
        *N000388,1 Commented out. (There is not PikTkt). [Begin]
        *IF MaSoLin.Picked .AND. MaSoLin.PikTkt <> '******' .AND. ;
        *  !SEEK(MaSoLin.PikTkt+MaSoLin.cMOrder+STR(MaSoLin.LineNo,6),'PIKLINE')
        *  SELECT MaSoLin
        *  COPY NEXT 1 TO (gcWorkDir+lcTmpPikLn)
        *  *-- Call a custom function if found  
        *  SELECT PIKLINE
        *  APPEND FROM (gcWorkDir+lcTmpPikLn)
        *  =gfTraceKey('PIKLINE',m.PIKTKT+m.cMORDER+STR(m.LINENO,6),'A')
        *ENDIF
        *IF MaSoLin.Picked .AND. SEEK(MaSoLin.cMOrder+MaSoLin.PikTkt,'PikTkt')
        *  SELECT PikTkt
        *  =RLOCK()
        *  *-- Change the status of the pick ticket to  complete
        *  REPLACE Status WITH 'C'
        *  UNLOCK
        *  =gfTraceKey('PikTkt',MaSoLin.PIKTKT,'M')
        *ENDIF
        *N000388,1 Commented out. (There is not PikTkt). [End]

        IF &lcDetFile..nSteps < 12        
          SELECT MaSoLin
          =RLOCK()
          *-- replace the quantities  in the Ordline file with 0 or the remaning quantities
          *-- if back order is yes
          *-- and the picked quantities with 0
          REPLACE FabQty WITH FabQty - IIF(m.lBackOrd,MIN(FabQty,m.FabQty),FabQty),;
                  DYELOT WITH ''
          UNLOCK
          SELECT (lcDetFile)
          =RLOCK()      
          REPLACE nSteps WITH 12
          UNLOCK
          =gfTraceKey('MaSoLin','O'+MaSoLin.cMORDER+STR(MaSoLin.LINENO,6),'M')
        ENDIF
      ENDIF
    ENDIF  
    IF laInvSetup[1,2]='Y' .AND. llConsAcc
      
      *-- Cost of Goods Sold <DEBIT> Category key '008'
      laGlArray[1,1] = m.Gl_Sales
      laGlArray[1,2] = '008'
      laGlArray[1,3] = -1
      
      *-- Inventory Control <CREDIT> Category key '015'
      laGlArray[2,1] = m.Gl_Cost
      laGlArray[2,2] = '015'
      laGlArray[2,3] = 1

      =SEEK(m.Gl_Sales+'008','GL_LINK')
      m.cCOGSAcnt = GL_LINK.GlAcnt      && Cost of goods sold account
      =SEEK(m.Gl_Cost+'015','GL_LINK')
      m.cICAcnt = GL_LINK.GlAcnt        && Inventory control account 
    ENDIF
    SELECT (lcDetFile)
    
    *N000388,1 ABD change the variable to lnUseQty. [Begin]
    *SCATTER FIELDS FabQty TO laAdjStk
    *laAdjStk[1] = laAdjStk[1] * -1 /IIF(nSellconv=0,1,nSellconv)
    SCATTER FIELDS FabQty TO lnUseQty
    lnUseQty = lnUseQty * -1 /IIF(nSellconv=0,1,nSellconv)
    *N000388,1 ABD change the variable to lnUseQty. [End]
    
    
    PRIVATE lcRefer
    IF EMPTY(&lcHdrFile..cMOrder) AND &lcHdrFile..DIRECT_INV
      lcRefer = 'CUST# '+ &lcHdrFile..Account + "-" + Customer.BTName
    ELSE
      lcRefer = 'CUST# '+ &lcHdrFile..Account + " Mat S.Order " + &lcHdrFile..cMOrder
    ENDIF

    *-- Update the  Stock in the FabDye , MatInvJL , Fabric and get the next step number
    *-- for the uncomplete Session 
    
    *B606163,1 ABD - [Begin]
    *-- we made a function to send the needed record from the lcTmpJour to save the
    *-- correct recored to the material inventory journal.
    IF laInvSetup[3,2] = 'L'
      = lfUpdTmpJr ()
    ENDIF
    laOtherPar[1,2] = &lcDetFile..LINENO
    *B606163,1 ABD - [End]
    *N000388,1 we need to change
    
    lnCurrRec  = lnCurrRec + 1
    lnNextSt = IIF(m.FabQty=0 OR !llConsAcc,13,;
                gfMatCrl('5',m.Fabric,m.Color,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,&lcHdrFile..dPostDate,;
                lcInvNo,lnUseQty,m.CostUse,'','',0,'','',@laGlArray,lcSession,'','','','','','','',@laOtherPar))

    *-- Get difference between the old stock value and the new stock value
    lnStkVal  = lnStkVal - Fabric.nStkVal
    
    *-- calculate the new cost
    *N000388,1 Change the cost to be on Fabric
    IF laInvSetup[3,2]='S'
      *m.Cost = Style.TotCost
      m.CostUse = Fabric.CostUse
    ELSE
      m.CostUSe = IIF(m.FabQty=0,0,(lnStkVal/IIF(laInvSetup[2,2]='Y',FabDye.OnHand,Fabric.onHand))/IIF(m.nSellConv=0,1,m.nSellConv))
    ENDIF

    *-- Cost of goods sold amount
    lnCOGSAmt = lnCOGSAmt + (m.FabQty * m.CostUse)
    
    *N000388,1 Commented out. (we don't use Material History file) [Begin]   
    *IF &lcDetFile..nSteps < lnNextSt .AND. llConsAcc .AND. ;
    *  SEEK(m.Fabric+m.Color+lcGlYear,'icStyHst')
    *  *-- Shipped amount 
    *  lnShipAmnt = m.FabQty*m.nSellNetPr &lcExRSin &lcHdrFile..NEXRATE &lcUntSin &lcHdrFile..NCURRUNIT
    *  *--Discount amount
    *  lnDiscAmnt = m.FabQty*m.nSellNetPr*&lcHdrFile..DiscPcnt/100 &lcExRSin &lcHdrFile..NEXRATE &lcUntSin &lcHdrFile..NCURRUNIT
    *  SELECT icStyHst
    *  *-- update the ic style history file  for this gl period 
    *  =RLOCK()
    *  REPLACE nSlsQty&lcGlPeriod  WITH nSlsQty&lcGlPeriod  + m.FabQty   ,;
    *          nSlsQty             WITH nSlsQty             + m.FabQty   ,;
    *          nSlsAmt&lcGlPeriod  WITH nSlsAmt&lcGlPeriod  + lnShipAmnt ,;
    *          nSlsAmt             WITH nSlsAmt             + lnShipAmnt ,;
    *          nDisAmt&lcGlPeriod  WITH nDisAmt&lcGlPeriod  + lnDiscAmnt ,;
    *          nDisAmt             WITH nDisAmt             + lnDiscAmnt ,;
    *          nCOGSAmt&lcGlPeriod WITH nCOGSAmt&lcGlPeriod + m.FabQty*m.CostUse  ,;
    *          nCOGSAmt            WITH nCOGSAmt            + m.FabQty*m.CostUse  
    *  UNLOCK
    *  SELECT (lcDetFile)
    *  =RLOCK()
    *  REPLACE nSteps WITH lnNextSt
    *  UNLOCK
    *  =gfTraceKey('icStyHst',m.Style+lcGlYear,'M')
    *ENDIF
    *N000388,1 Commented out. (we don't use Material History file) [End]

    m.Store = IIF(m.Consol='Y',m.cConStore,m.Store)
    IF llConsAcc .AND. m.FabQty > 0 .AND. ;
      !SEEK(m.cMatInv+STR(m.LineNo,6),'MINVLINE')
      *-- update the invline with the new line
      INSERT INTO MINVLINE FROM MEMVAR
    ENDIF
    IF !llConsAcc .AND. m.FabQty > 0 .AND. ;
      !SEEK(m.cMatInv+m.Store+m.cMOrder+m.Fabric+m.Color+STR(m.LineNo,6),'MCONSINL')
      *-- update the conslidated invoice line file
      INSERT INTO MCONSINL FROM MEMVAR
    ENDIF
  ENDSCAN
  =IIF(&lcHdrFile..DIRECT_INV,gfTraceKey('MaSoLin','O'+lcOrdNo,'A'),.T.)
  =gfTraceKey('MINVLINE',lcInvNo,'A')
  lcStoreFld = IIF(&lcHdrFile..Consol='Y',&lcHdrFile..cConStore,&lcHdrFile..Store)       
  =IIF(!llConsAcc AND SEEK(lcInvNo+lcStoreFld,'MCONSINL'),;
       gfTraceKey('MCONSINL',lcInvNo+lcStoreFld,'A'),.T.)
  IF !EMPTY(lcOrdCanLn) AND &lcHdrFile..Consol<>'Y' AND ;
       SEEK('O'+&lcHdrFile..cMORDER,lcOrdCanLn)
    
    PRIVATE lnInvPrice
    lnInvPrice = IIF(TYPE('m.nSellNetPr')<>'U',m.nSellNetPr,0)

    SELECT (lcOrdCanLn)
    SCAN REST WHILE cOrdType+cMOrder+STR(LineNo,6) = 'O'+&lcHdrFile..CMORDER
      IF !SEEK(cOrdType+cMOrder+STR(LineNo,6),'MORDCNLN')
      *-- Update the order canceld Quantity
        SCATTER MEMVAR
        INSERT INTO 'MORDCNLN' FROM MEMVAR
      ENDIF
    ENDSCAN
    m.nSellNetPr = lnInvPrice
    =gfTraceKey('MORDCNLN','O'+&lcHdrFile..CMORDER,'A'),.T.)
  ENDIF
    
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
  *-- sales rep Commission at Style level or at invoice level   
  m.COMMAMT1  = IIF(laInvSetup[6,2]='Y',lnComm1,lnHdrComm1*lnNetShipAmnt/100)
  m.COMMAMT2  = IIF(laInvSetup[6,2]='Y',lnComm2,lnHdrComm2*lnNetShipAmnt/100)
  m.Link_Code = lcCustLink
  
  m.Invoice   = lcInvNo
  m.cMatInv   = lcInvNo
  
  IF laInvSetup[1,2]='Y'
    *-- Freight income <CREDIT> Category key '004'
    IF UPPER(ALLTRIM(gcContCode))='ENG' AND !EMPTY(lcEngChrg)
      m.cFrgtAcnt = ''
      IF m.Consol<>'Y' AND SEEK(IIF(m.DIRECT_INV,SPACE(6),m.cMOrder)+m.Store+m.PikTkt,lcEngChrg)
        *-- the company is england so update the Gl with the english charges
        *--In case of england charges taxes is at style level
        SELECT (lcEngChrg)
        SCAN REST WHILE cMOrder+Store+PikTkt+cchrgcode = ;
        IIF(m.DIRECT_INV,SPACE(6),m.cMOrder)+m.Store+m.PikTkt
          DO GLDIST WITH lcCustLink,'004',-&lcEngChrg..nChrgAmnt,'MI',;
                     m.cMatInv,m.dPostDate,lcGlYear,lcGlPeriod,;
                     lcDistFile,&lcEngChrg..cFrgtAcnt,m.cCurrCode,m.nCurrUnit,m.nExRate
        ENDSCAN
      ENDIF
    ELSE
      IF llConsAcc
      *-- other charges
        DO GLDIST WITH lcCustLink,'004',-(m.Freight+m.Insur+m.Cod),'MI',;
                       m.cMatInv,m.dPostDate,lcGlYear,lcGlPeriod,;
                       lcDistFile,'',m.cCurrCode,m.nCurrUnit,m.nExRate
        IF (m.Freight+m.Insur+m.Cod) <> 0
		  m.cFrgtAcnt = &lcDistFile..GlAccount   && Frieght account
        ELSE
          m.cFrgtAcnt = ''
        ENDIF
      ENDIF  
    ENDIF
    IF llConsAcc AND laInvSetup[7,2]='Y'
     *-- use taxes in the company setup is yes so update the Gl with income tax 
      DO GLDIST WITH lcCustLink,'014',-(m.Tax_Amt+m.nPstAmt+m.nHstAmt),'MI',m.cMatInv,;
                     m.dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
                     m.cCurrCode,m.nCurrUnit,m.nExRate
      IF m.Tax_Amt+m.nPstAmt+m.nHstAmt <> 0
        m.cTaxAcnt = &lcDistFile..GlAccount && Tax Account
      ELSE
        m.cTaxAcnt = ""
      ENDIF  
    ENDIF  
    IF llConsAcc
   
      *-- Account receivable <DEBIT> Category key '001'
      DO GLDIST WITH lcCustLink,'001',m.TotalChg,'MI',m.cMatInv,m.dPostDate,;
                     lcGlYear,lcGlPeriod,lcDistFile,'',m.cCurrCode,m.nCurrUnit,m.nExRate
      IF m.TotalChg <> 0
        lnEqv001=0
        SUM nEqvAmnt FOR Tran_No = m.cMatInv AND (Catg_Key='003' OR Catg_Key='004' OR Catg_Key='005' OR Catg_Key='014') TO  lnEqv001
        LOCATE FOR  Tran_No = m.cMatInv AND Catg_Key='001'
        REPLACE nEqvAmnt WITH  ABS(lnEqv001)
        m.cArAcnt = &lcDistFile..GlAccount   && Account recievable account
      ELSE
        m.cArAcnt = ''
      ENDIF
    ENDIF
  ENDIF
  
  IF ( gcAct_Appl="PS" .AND. USED("postran") .AND. llConsAcc AND !SEEK(m.cMatInv,'MINVHDR')) ;
    .OR.( llConsAcc AND SEEK(m.cMatInv,'MINVHDR') AND EMPTY(MInvHdr.Account))
  
    *-- update the invoice header  
    m.Status = 'C'
    
    lcSVAlias=Alias()
    SELECT MInvHdr
    IF gcAct_Appl="PS" .AND. USED("postran")
      APPEND BLANK
    ENDIF
    GATHER MEMVAR MEMO
    SELECT (lcSVAlias)    
    =gfTraceKey('MINVHDR',m.cMatInv,'A')
  ENDIF
  
  IF !llConsAcc AND SEEK(m.cMatInv,'MINVHDR')
    IF !EMPTY(MaSoHdr.CustPO)
      PRIVATE lnCurAlias
      lnCurAlias = SELECT(0)
      SELECT MInvHdr
      REPLACE CustPO WITH m.CustPO
      SELECT (lnCurAlias)
    ENDIF
  ENDIF  
  
  IF !llConsAcc .AND. !SEEK(m.cMatInv+m.Store+m.cMOrder,'MCONSINH')
    m.Status = 'C'
    *-- update the conslidated invoice Header file
    INSERT INTO MCONSINH FROM MEMVAR
    =gfTraceKey('MCONSINH',m.cMatInv+m.Store+m.cMOrder,'A')
  ENDIF
  IF m.CONSOL<>'Y'
    IF !SEEK('O'+m.cMOrder,'MaSoHdr')
    *-- direct invoice so create order 
      SELECT MaSoHdr
      INSERT INTO MaSoHdr (cOrdType,cMOrder,Status,Account,Store,CustPo) VALUES ;
      ('O',m.cMOrder,'C',m.Account,m.Store,m.CustPo)

      lcFactAcct = ''
      =SEEK(IIF(EMPTY(m.Store),'M'+m.Account,'S'+m.Account+m.Store),'CUSTOMER')
      SELECT MaSoHdr
      *-- Add the customer defaults to the order header
      =SEEK('O'+m.cMOrder)
      =RLOCK()  
      REPLACE Dept      WITH m.Dept  ,;
              Note1     WITH m.Note1 ,;
              Note2     WITH m.Note2 ,;
              Priority  WITH Customer.Priority ,; 
              Season    WITH m.Season ,;
              cDivision WITH m.cDivision ,;
              Bulk      WITH 'N' ,;
              cReOrder  WITH 'N' ,; 
              Consol    WITH 'N' ,;   
              Multi     WITH 'N' ,;  
              cTermCode WITH m.cTermCode   ,;
              ShipVia   WITH m.ShipVia ,;
              SpcInst   WITH m.SpcInst ,;
              cInsur    WITH IIF(m.lupsins,'Y','N') ,;
              Buyer     WITH Customer.Buyer  ,;
              Phone     WITH Customer.Phone1 ,;
              cFacCode  WITH m.cFacCode ,;
              FactAcct  WITH lcFactAcct ,; 
              Approval  WITH m.Approval ,;
              ApprAmt   WITH m.ApprAmt
      REPLACE Rep1       WITH m.Rep1     ,;
              Comm1      WITH lnHdrComm1 ,;
              Rep2       WITH m.Rep2     ,;
              Comm2      WITH lnHdrComm2 ,;
              Entered    WITH m.InvDate  ,;
              Start      WITH m.InvDate  ,;
              Complete   WITH m.InvDate  ,;
              Disc       WITH m.DiscPcnt ,;
              FabBook    WITH m.FabShip  ,;
              FabBookAmt WITH m.FabShipAmt  ,;
              FabShip    WITH m.FabShip     ,;
              FabShipAmt WITH m.FabShipAmt  ,;
              LastLine   WITH m.LastLine ,;
              Link_Code  WITH lcCustLink ,;
              dAdd_Date WITH gdSysDate   ,;
              cAdd_Time WITH TIME()      ,;
              cAdd_User WITH gcUser_id   ,;
              Distrb_No  WITH Customer.Dist_Ctr ,;
              DIRECT_INV WITH .T.
      REPLACE cWareCode  WITH m.cWareCode   ,;
              Alt_ShpTo  WITH !EMPTY(m.stName) ,; 
              stName     WITH m.stName    ,;
              cAddress1  WITH m.cAddress1 ,;
              cAddress2  WITH m.cAddress2 ,;
              cAddress3  WITH m.cAddress3 ,;
              cAddress4  WITH m.cAddress4 ,;
              cAddress5  WITH m.cAddress5 ,;
              cCurrCode  WITH m.cCurrCode ,;
              nExRate    WITH m.nExRate ,;
              nCurrUnit  WITH m.nCurrUnit
      UNLOCK
      =gfTraceKey('MaSoHdr','O'+m.cMOrder,'A')
    ELSE
    *-- sales order invoice so decrease the approved amount with total Charges 
      IF &lcHdrFile..nSteps < 1
        SELECT MaSoHdr
        =RLOCK()
        REPLACE ApprAmt WITH MAX(ApprAmt - m.TotalChg,0)
        UNLOCK
        SELECT (lcHdrFile)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
        =gfTraceKey('MaSoHdr','O'+m.cMOrder,'M')
      ENDIF
    ENDIF  
  ENDIF
  WAIT 'Updating Sales Representative Information...' WINDOW NOWAIT
  IF Consol<>'Y' .AND. &lcHdrFile..nSteps < 2 .AND. SEEK(m.Rep1,'SALESREP')
    SELECT SALESREP
    
    lnEqvAmnt = ROUND(m.COMMAMT1 &lcExRSin m.nExRate &lcUntSin m.nCurrUnit,2)
    IF lnEqvAmnt > 0
      =RLOCK()
    
      *-- accumelate the sales rep commission
      REPLACE CURRENT WITH CURRENT + lnEqvAmnt ,;
              BALANCE WITH BALANCE + lnEqvAmnt
      UNLOCK
      =gfTraceKey('SALESREP',m.Rep1,'M')
      SELECT REPCOMM
      *-- add new line to rep commission file
       APPEND BLANK
      =RLOCK()
      REPLACE REPCODE   WITH m.Rep1      ,;
              TRANTYPE  WITH '1'         ,;
              STATUS    WITH 'O'         ,;
              TRAN      WITH m.cMatInv   ,;
              ACCOUNT   WITH m.Account   ,;
              ORDER     WITH m.cMOrder   ,;
              CUSTPO    WITH m.CustPo    ,;
              STORE     WITH m.Store     ,;
              DATE      WITH m.InvDate   ,;
              DESC      WITH 'MATERIAL INVOICE'   ,;
              AMOUNT    WITH lnEqvAmnt   ,;
              COMMPCNT  WITH m.Comm1     ,;
              nForAmnt  WITH m.CommAmt1  ,;
              cCurrCode WITH m.cCurrCode ,;
              nCurrUnit WITH m.nCurrUnit ,;
              nExRate   WITH m.nExRate   ,;
              dAdd_Date WITH gdSysDate   ,;
              cAdd_Time WITH TIME()      ,;
              cAdd_User WITH gcUser_id
      UNLOCK
      =gfTraceKey('REPCOMM',m.Rep1+DTOS(m.InvDate)+m.cMatInv+'1','A')

    ENDIF

    SELECT (lcHdrFile)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  IF m.Consol<>'Y' .AND. &lcHdrFile..nSteps < 3 .AND. SEEK(m.Rep2,'SALESREP')
    SELECT SALESREP
    lnEqvAmnt = ROUND(m.COMMAMT2 &lcExRSin m.nExRate &lcUntSin m.nCurrUnit,2)

    IF lnEqvAmnt > 0

      =RLOCK()
      REPLACE CURRENT WITH CURRENT + lnEqvAmnt ,;
              BALANCE WITH BALANCE + lnEqvAmnt
      UNLOCK
      =gfTraceKey('SALESREP',m.Rep2,'M')
      SELECT REPCOMM
      APPEND BLANK
      =RLOCK()
      REPLACE REPCODE   WITH m.Rep2      ,;
              TRANTYPE  WITH '1'         ,;
              STATUS    WITH 'O'         ,;
              TRAN      WITH m.cMatInv   ,;
              ACCOUNT   WITH m.Account   ,;
              ORDER     WITH m.cMOrder   ,;
              CUSTPO    WITH m.CustPo    ,;
              STORE     WITH m.Store     ,;
              DATE      WITH m.InvDate   ,;
              DESC      WITH 'MATERIAL INVOICE'   ,;
              AMOUNT    WITH lnEqvAmnt   ,;
              COMMPCNT  WITH m.Comm2     ,;
              nForAmnt  WITH m.CommAmt2  ,;
              cCurrCode WITH m.cCurrCode ,;
              nCurrUnit WITH m.nCurrUnit ,;
              nExRate   WITH m.nExRate   ,;
              dAdd_Date WITH gdSysDate   ,;
              cAdd_Time WITH TIME()      ,;
              cAdd_User WITH gcUser_id
      UNLOCK
      =gfTraceKey('REPCOMM',m.Rep2+DTOS(m.InvDate)+m.cMatInv+'1','A')
    
    ENDIF
      
    SELECT (lcHdrFile)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF
  IF llConsAcc .AND. &lcHdrFile..nSteps < 4
  *-- Update the instalment heder and detail and the debit file
    *-- variable to check if it was sales order invoice or not
    llOrder = SEEK('O'+m.cMOrder,'MASOHDR') .AND. !MASOHDR.DIRECT_INV
    IF !EMPTY(lcInstHdr) AND !EMPTY(lcInstLine) AND ;
      SEEK(IIF(llOrder,m.cMOrder,SPACE(6))+m.Store+m.PikTkt,lcInstHdr) .AND. ;
      SEEK(IIF(llOrder,m.cMOrder,SPACE(6))+m.Store+m.PikTkt,lcInstLine)
      *-- there is instalments in the invoice so open the files to update it
      =gfOpenFile(gcDataDir+'ARMINSTH', gcDataDir+'ArInstmh','SH')
      =gfOpenFile(gcDataDir+'ARMINSTD', gcDataDir+'ArInstmd','SH')

      IF !SEEK(m.cMatInv,'ARMINSTH')
      *-- update the instalment Header
        SELECT ARMINSTH
        APPEND BLANK
        REPLACE cMatInv    WITH m.cMatInv ,;
                cInstmType WITH &lcInstHdr..cInstmType ,;
                nInstmFreq WITH &lcInstHdr..nInstmFreq ,;
                nInstIAmnt WITH &lcInstHdr..nInstIAmnt ,;
                nInstmAmnt WITH &lcInstHdr..nInstmAmnt ,;
                nInstIPcnt WITH &lcInstHdr..nInstIPcnt ,;
                dInstmStDt WITH &lcInstHdr..dInstmStDt ,;
                nNoInstm   WITH &lcInstHdr..nNoInstm   ,;
                cInstmRef  WITH &lcInstHdr..cInstmRef  ,;
                cAdd_User  WITH gcUser_id ,;
                cAdd_Time  WITH TIME() ,;
                dAdd_Date  WITH gdSysDate
      ENDIF
      *-- update the instalment Detail
      IF &lcInstHdr..nInstIAmnt > 0 
        IF !SEEK(m.cMatInv+SPACE(3),'ARMINSTD')
          SELECT ARMINSTD
          APPEND BLANK
          *-- add the first line with the initial amount
          REPLACE cMatInv    WITH m.cMatInv ,;
                  cinstalno  WITH SPACE(3)  ,;
                  nInstmAmnt WITH &lcInstHdr..nInstIAmnt ,;
                  DueDate    WITH m.InvDate ,;
                  nInstmPcnt WITH &lcInstHdr..nInstIAmnt*100/m.TotalChg ,;
                  cInstmNote WITH ''        ,;
                  cAdd_User  WITH gcUser_id ,;
                  cAdd_Time  WITH TIME()    ,;
                  dAdd_Date  WITH gdSysDate
        ENDIF        
        IF (EMPTY(m.cFacCode) .OR. laInvSetup[8,2]='Y') .AND. ;
          !SEEK(m.Account+m.cMatInv+SPACE(3)+DTOS(m.InvDate),'DEBIT')
          *-- the invoice is not factored or post factored invoice = Yes 
          *-- update the debit file 
          
          SELECT DEBIT
          APPEND BLANK
          =RLOCK()
          REPLACE cInstalNO WITH SPACE(3)    ,;
                  DUEDATE   WITH m.InvDate   ,;
                  dPostDate WITH m.dPostDate ,;
                  REFERENCE WITH m.CustPo    ,;
                  TRANTYPE  WITH 'M'         ,;
                  STORE     WITH m.Store     ,;
                  ACCOUNT   WITH m.Account   ,;
                  cFacCode  WITH m.cFacCode  ,;
                  TRAN      WITH m.cMatInv   ,;
                  TRANDATE  WITH m.InvDate   ,;
                  AMOUNT    WITH &lcInstHdr..nInstIAmnt ,;
                  DSC_AMT   WITH (m.FabShipAmt+m.Discount)*m.Trde_Disc/100 ,;
                  cCurrCode WITH m.cCurrCode ,;
                  nCurrUnit WITH m.nCurrUnit ,;
                  nExRate   WITH m.nExRate   ,;
                  cArGlAcc  WITH m.cArAcnt   ,;
                  dAdd_Date WITH gdSysDate   ,;
                  cAdd_Time WITH TIME()      ,;
                  cAdd_User WITH gcUser_id   ,;
                  DESC      WITH IIF(m.DIRECT_INV,'MATERIAL DIRECT INV.','MATERIAL S.ORDER INV')
          UNLOCK
        ENDIF
      ENDIF
      SELECT (lcInstLine)
      *-- update all the lines in the instalment detail file and the debit file
      SCAN REST WHILE cMOrder+Store+PikTkt+cInstalNo =;
                      IIF(llOrder,m.cMOrder,SPACE(6))+m.Store+m.PikTkt
        IF !SEEK(m.cMatinv+cinstalno,'ARMINSTD')
        *-- update the instalment lines if it was not updated in the last uncomplete session        
          SELECT ARMINSTD
          APPEND BLANK
          REPLACE cMatInv    WITH m.cMatInv ,;
                  cinstalno  WITH &lcInstLine..cinstalno ,;
                  nInstmAmnt WITH &lcInstLine..nInstmAmnt ,;
                  DueDate    WITH &lcInstLine..DueDate ,;
                  nInstmPcnt WITH &lcInstLine..nInstmPcnt ,;
                  cInstmNote WITH &lcInstLine..cInstmNote ,;
                  cAdd_User  WITH gcUser_id ,;
                  cAdd_Time  WITH TIME() ,;
                  dAdd_Date  WITH gdSysDate
        ENDIF  
        IF (EMPTY(m.cFacCode) .OR. laInvSetup[8,2]='Y') .AND. ;
          !SEEK(m.Account+m.cMatInv+&lcInstLine..cInstalNo+DTOS(m.InvDate),'DEBIT')
          SELECT DEBIT
          APPEND BLANK
          =RLOCK()

          REPLACE cInstalNO WITH &lcInstLine..cInstalNO ,;
                  DUEDATE   WITH &lcInstLine..DueDate   ,;
                  dPostDate WITH m.dPostDate ,;
                  REFERENCE WITH m.CustPo    ,;
                  TRANTYPE  WITH 'M'         ,;
                  STORE     WITH m.Store     ,;
                  ACCOUNT   WITH m.Account   ,;
                  cFacCode  WITH m.cFacCode  ,;
                  TRAN      WITH m.cMatInv   ,;
                  TRANDATE  WITH m.InvDate   ,;
                  AMOUNT    WITH &lcInstLine..nInstmAmnt ,;
                  cCurrCode WITH m.cCurrCode ,;
                  nCurrUnit WITH m.nCurrUnit ,;
                  nExRate   WITH m.nExRate   ,;
                  cArGlAcc  WITH m.cArAcnt   ,;
                  dAdd_Date WITH gdSysDate   ,;
                  cAdd_Time WITH TIME()      ,;
                  cAdd_User WITH gcUser_id   ,;
                  DESC      WITH IIF(m.DIRECT_INV,'MATERIAL DIRECT INV.','MATERIAL S.ORDER INV')
          UNLOCK
        ENDIF
      ENDSCAN
      IF (EMPTY(m.cFacCode) .OR. laInvSetup[8,2]='Y') .AND. ;
         &lcInstHdr..nInstIAmnt = 0 
        SELECT DEBIT
        =RLOCK()
        REPLACE DSC_AMT WITH (m.FabShipAmt+m.Discount)*m.Trde_Disc/100
        UNLOCK
      ENDIF
      =gfTraceKey('ARMINSTH',m.cMatInv,'A')
      =gfTraceKey('ARMINSTD',m.cMatInv,'A')
    ELSE
      *-- there is no instalments so update the debit file only
      IF (EMPTY(m.cFacCode) .OR. laInvSetup[8,2]='Y') .AND. ;
        !SEEK(m.Account+m.cMatInv+SPACE(3)+DTOS(m.InvDate),'DEBIT')
        *-- post factored invoice is yes
        SELECT DEBIT
        APPEND BLANK
        =RLOCK()
        
        REPLACE DUEDATE   WITH m.DueDate   ,;
                REFERENCE WITH m.CustPo    ,;
                dPostDate WITH m.dPostDate ,;
                TRANTYPE  WITH 'M'         ,;
                STORE     WITH m.Store     ,;
                ACCOUNT   WITH m.Account   ,;
                cFacCode  WITH m.cFacCode  ,;
                TRAN      WITH m.cMatInv   ,;
                TRANDATE  WITH m.InvDate   ,;
                AMOUNT    WITH m.TotalChg  ,;
                cCurrCode WITH m.cCurrCode ,;
                nCurrUnit WITH m.nCurrUnit ,;
                nExRate   WITH m.nExRate   ,;
                cArGlAcc  WITH m.cArAcnt   ,;
                DSC_AMT   WITH (m.FabShipAmt+m.Discount)*m.Trde_Disc/100,;
                DESC      WITH IIF(m.DIRECT_INV,'MATERIAL DIRECT INV.','MATERIAL S.ORDER INV') ,;
                dAdd_Date WITH gdSysDate   ,;
                cAdd_Time WITH TIME()      ,;
                cAdd_User WITH gcUser_id
        UNLOCK
      ENDIF
    ENDIF
    SELECT (lcHdrFile)
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
    
    =IIF(EMPTY(m.cFacCode) OR laInvSetup[8,2]='Y',gfTraceKey('DEBIT','M'+m.cMatInv,'A'),.T.)
  ENDIF  
  IF &lcHdrFile..nSteps < 5 AND llConsAcc AND ;
     (EMPTY(m.cFacCode) OR laInvSetup[8,2]='Y')
     *-- post factored invoice to customer = yes
    SELECT CUSTOMER
    =SEEK('M'+m.Account)
    =RLOCK()
    lnEqvAmnt = ROUND(m.TOTALCHG &lcExRSin m.NEXRATE &lcUntSin m.NCURRUNIT,2)
    *-- Aging Type
    IF laInvSetup[9,2] = "D" 
    *-- Aging by date  
      REPLACE Current WITH Current + lnEqvAmnt,;
              TotAge  WITH Current + Age30 + Age60 + Age90 + Age120    
    *-- Aging by Terms
    ELSE
      REPLACE TerCurrent WITH TerCurrent + lnEqvAmnt,;
              TotAge     WITH TerCurrent + TerAge30 + TerAge60 + TerAge90 + TerAge120
    ENDIF  
    REPLACE NetBal  WITH TotAge  - ChgBack + OpenCr,;
            YTDBal  WITH YTDBal  + lnEqvAmnt,;
            CrAvail WITH IIF(CrLimit > 0,CrAvail - lnEqvAmnt, CrAvail)
    UNLOCK
    
    =lfHgWUpdat() 

    =gfTraceKey('CUSTOMER','M'+m.Account,'M')
    SELECT (lcHdrFile)
    =RLOCK()
    REPLACE nSteps WITH 5
    UNLOCK
  ENDIF
  IF laInvSetup[1,2]='Y'
    WAIT 'Updating GL Distribution File' WINDOW NOWAIT
    SELECT GLDIST
    IF !SEEK(m.cMatInv+'MI'+lcGlSession)
      WAIT 'Updating General Ledger Distribution File ' WINDOW NOWAIT
      SELECT (lcDistFile)
      SCAN
        REPLACE GLSESSION WITH lcGlSession
        =gfAdd_Info(lcDistFile)  
      ENDSCAN
      USE
      SELECT GLDIST
      APPEND FROM (gcWorkDir+lcDistFile)
      ERASE (gcWorkDir+lcDistFile+".DBF")
    ENDIF
    =gfTraceKey('GLDIST',m.cMatInv+'MI'+lcGlSession,'A')
  ENDIF
  *-- Update the ups box File 
  IF !EMPTY(lcUpsFile) AND USED(lcUpsFile)
    WAIT 'Updating UPS Box File ' WINDOW NOWAIT
    SELECT (lcUpsFile)
    lcStoreFld = IIF(&lcHdrFile..Consol='Y',&lcHdrFile..cConStore,&lcHdrFile..Store)
    IF SEEK(&lcHdrFile..cMOrder+lcStoreFld+&lcHdrFile..PikTkt)
      SCAN REST WHILE cMOrder+Store+PikTkt+STR(CARTONS,5)=;
                      &lcHdrFile..cMOrder+lcStoreFld+&lcHdrFile..PikTkt
        IF !SEEK(lcInvNo+Store+STR(CARTONS,5),'MUPSBOX')
          SCATTER MEMVAR
          m.Invoice = lcInvNo
          INSERT INTO 'MUPSBOX' FROM MEMVAR
        ENDIF
      ENDSCAN
      =gfTraceKey('MUPSBOX',lcInvNo,'A')
    ENDIF
  ENDIF
  
  *-- Update the english charges
  IF !EMPTY(lcEngChrg) AND USED(lcEngChrg)
    WAIT 'Updating Invoice Charges File ' WINDOW NOWAIT
    =gfOpenFile(gcDataDir+'mInvChrg',gcDataDir+'mInvChrg','SH')
    SELECT (lcEngChrg)
    lcStoreFld = IIF(&lcHdrFile..Consol='Y',&lcHdrFile..cConSTore,&lcHdrFile..Store)
    IF SEEK(IIF(&lcHdrFile..DIRECT_INV,SPACE(6),&lcHdrFile..cMOrder)+;
            lcStoreFld+&lcHdrFile..PikTkt)
      SCAN REST WHILE cMOrder+cStore+PikTkt+cchrgcode = ;
      IIF(&lcHdrFile..DIRECT_INV,SPACE(6),&lcHdrFile..cMOrder)+lcStoreFld+&lcHdrFile..PikTkt
        IF !SEEK(lcInvNo+cStore+cChrgCode,'mInvChrg')
          SCATTER MEMVAR
          m.Invoice = lcInvNo
         INSERT INTO 'mInvChrg' FROM MEMVAR
        ENDIF
      ENDSCAN
      =gfTraceKey('mInvChrg',lcInvNo,'A')
    ENDIF
    =gfCloseFile('mInvChrg')
  ENDIF
  
  *N000388,1 Customer history will be updated later. [Begin]
  *-- Update the customer history file
  *IF llConsAcc AND SEEK(m.Account+lcGlYear,'arCusHst') AND ;
  *   &lcHdrFile..nSteps < 6
  *  lnShipAmnt = m.FabShipAmt  &lcExRSin m.NEXRATE &lcUntSin m.NCURRUNIT
  *  lnDiscAmnt = ABS(m.Discount) &lcExRSin m.NEXRATE &lcUntSin m.NCURRUNIT
  *  SELECT arCusHst
  *  =RLOCK()
  *  REPLACE nSlsQty&lcGlPeriod  WITH nSlsQty&lcGlPeriod  + m.FabShip ,;
  *          nSlsQty             WITH nSlsQty             + m.FabShip ,;
  *          nSlsAmt&lcGlPeriod  WITH nSlsAmt&lcGlPeriod  + lnShipAmnt ,;
  *          nSlsAmt             WITH nSlsAmt             + lnShipAmnt ,;
  *          nDisAmt&lcGlPeriod  WITH nDisAmt&lcGlPeriod  + lnDiscAmnt ,;
  *          nDisAmt             WITH nDisAmt             + lnDiscAmnt ,;
  *          nCOGSAmt&lcGlPeriod WITH nCOGSAmt&lcGlPeriod + lnCOGSAmt ,;
  *          nCOGSAmt            WITH nCOGSAmt            + lnCOGSAmt
  *  UNLOCK
  *  =gfTraceKey('arCusHst',m.Account+lcGlYear,'M')
  *  SELECT (lcHdrFile)
  *  =RLOCK()
  *  REPLACE nSteps WITH 6
  *  UNLOCK
  *ENDIF
  *N000388,1 Customer history will be updated later. [End]
  
  IF llConsAcc AND &lcHdrFile..nSteps < 8 .AND. &lcHdrFile..lKeyOff
    =lfvKeyOff(m.cMatInv)
    SELECT (lcHdrFile)
    =RLOCK()
    REPLACE nSteps WITH 8
    UNLOCK
  ENDIF
  
  *N000388,1 SSE Commented out. [Begin]
  *IF !llFromEdi AND 'EB' $ gcCmpModules
  *  IF SEEK('A'+m.Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'810','EDIPD')
  *    SELECT EDITRANS
  *    IF !SEEK('810'+PADR(m.cMatInv,20)+'A'+m.Account)
  *      INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,lInterComp) VALUES ;
  *      ('810',m.cMatInv,'A',m.Account,EdiAcPrt.lInterComp)
  *    ENDIF
  *    REPLACE cStatus WITH 'N'
  *    =gfAdd_Info('EDITRANS')
  *  ENDIF
  *  IF SEEK('F'+m.cFacCode,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'810','EDIPD')
  *    SELECT EDITRANS
  * 	  IF !SEEK('810'+PADR(m.cMatInv,20)+'F'+m.cFacCode)
  *      INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,lInterComp) VALUES ;
  *      ('810',m.cMatInv,'F',m.cFacCode,EdiAcPrt.lInterComp)
  *    ENDIF
  *    REPLACE cStatus WITH 'N'
  *    =gfAdd_Info('EDITRANS')
  *  ENDIF
  *ENDIF
  *IF !llFromEdi AND 'NC' $ gcCmpModules AND lcSysType = 'P'
  *  SELECT CODES
  *  SET ORDER TO TAG Idrltfname
  *  =SEEK('NYCSITEID')
  *  LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
  *              FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
  *  IF FOUND()
  *    lcSiteId = Codes.cCode_No
  *    SELECT EDiAcPrt
  *    LOCATE FOR cSiteId = lcSiteId
  *    IF FOUND('EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'810','EDIPD')
  *      SELECT EDITRANS
  *      IF !SEEK('810'+PADR(m.cMatInv,20)+'A'+EdiAcPrt.cPartner)
  *        INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,lInterComp) VALUES ;
  *        ('810',m.cMatInv,'A',EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
  *      ENDIF
  *      REPLACE cStatus WITH 'N'
  *      =gfAdd_Info('EDITRANS')
  *    ENDIF
  *  ENDIF
  *ENDIF
  *N000388,1 SSE Commented out. [Begin]
  
ENDSCAN

*N000388,1 SSE Commented Out. [Begin]
*IF llPackHdr
*  USE IN PACK_HDR
*ENDIF
*IF llBolHdr
*  USE IN BOL_HDR
*ENDIF
*N000388,1 SSE Commented Out. [End]

ERASE (gcWorkDir+lcTmpPikLn+".DBF")
ERASE (gcWorkDir+lcTmpPikLn+".FPT")
WAIT CLEAR
SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : gpUpsChrg
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Caculate invoice charges
*!*************************************************************
*! Calls     : lfGetFright(),lfUpsBox()
*!*************************************************************
*! Parameters: lcAccount  : Invoice Account
*!             lcStore    : Invoice Store
*!             lcOrderNo  : Order Number
*!             lcPikTKt   : PikTKt Number
*!             lcWareCode : Warehouse
*!             lnShipAmt  : Invoice Ship Amount
*!             llCod      : Invoice use COD
*!             llupsinsur : Invoice use UPS insurance
*!             lnDscPrcnt : Discount Pecent
*!             lcTaxRate  : GST Tax Rate
*!             lcPstRule  : PST tax Rule
*!             lnPstRate  : PST Tax Rate
*!             lcUpsFile  : UPS Temp File Name
*!             lcShipVia  : Invoice ShipVia
*!             lcCOD_AMT  : COD Amount
*!             lcFreight  : Freight Charge
*!             lcInsur    : Insurance Charge
*!             lcCOD      : COD Charge
*!             lcCartons  : Invoice Cartons
*!             lcWeight   : Invoice Weight
*!             lcTaxAmnt  : GST Tax Amount
*!             lcPstAmnt  : PST Tax Amount
*!             lnTaxDue   : Amount applied to tax
*!             lcChrgType : Charge Type: Freight, Insurance, Cod, All
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =gpUpsChrg()
*!*************************************************************
FUNCTION gpUpsChrg

PARAMETERS lcAccount,lcStore,lcOrderNo,lcPikTKt,lcWareCode,lnShipAmt,llCod,;
           llupsinsur,lnDscPrcnt,lcTaxRate,lcPstRule,lnPstRate,lcUpsFile,;
           lcShipVia,lcCOD_AMT,lcFreight,lcInsur,lcCOD,lcCartons,lcWeight,;
           lcTaxAmnt,lcPstAmnt,lnTaxDue,lcChrgType
PRIVATE laShpViaFld,lcUpsType,lnCodCharge,lnInsCharge,lnFxdPrcnt,lcUpsIncr,;
        llCompTax,lcTaxRule,getf,llUpsBox

IF lnShipAmt = 0
  RETURN           
ENDIF
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp)) 
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
  *-- get the from zone from the wharehouse if use multiple locations = y
  lcFromZone = IIF(SEEK(lcWareCode,'warehous'),WareHous.UPS,lcFromZone)
  =gfCloseFile('WAREHOUS')
ENDIF
*-- get the company setup information
llCompTax = (gfGetMemVar('M_TAX',gcAct_Comp)='Y')   && Use taxes = Y 
lcTaxRule = ALLTRIM(gfGetMemVar('M_TAX_METH',gcAct_Comp)) && tax method 
llUpsBox  = (gfGetMemVar('M_UPSBOX',gcAct_Comp)='Y')      && Track boxes for UPS manifactring
*-- ShipVia related fields
DECLARE laShpViaFld[4,2]
laShpViaFld[1,1] = 'CUPS'
laShpViaFld[1,2] = 'lcUpsType'    && Ups type
laShpViaFld[2,1] = 'NCODCHARGE'   
laShpViaFld[2,2] = 'lnCodCharge'  && Cash on delivery charge
laShpViaFld[3,1] = 'NINSCHARGE'
laShpViaFld[3,2] = 'lnInsCharge'  && insurance Charges
laShpViaFld[4,1] = 'NFXDPRCNT'
laShpViaFld[4,2] = 'lnFxdPrcnt'        
STORE '' TO lcUpsType
STORE 0 TO lnCodCharge,lnInsCharge,lnFxdPrcnt

=SEEK('M'+lcAccount,'Customer')
lcUpsIncr=Customer.Ups_Incr   && Percentage added to the customer Frieght Charges
=SEEK(IIF(EMPTY(lcStore),'M','S')+lcAccount+lcStore,'Customer')
IF Customer.nBrkWeight <> 0 .AND. &lcWeight > Customer.nBrkWeight
*-- if the wieght greater than customer Break Weight get the alternative ship via
  &lcShipVia = Customer.cAltShpVia
ENDIF
*-- get the ship via related fields
=gfRltFld(&lcShipVia,@laShpViaFld,'SHIPVIA')
*-- if no ups type so use Other
lcUpsType = IIF(EMPTY(lcUpsType),'OTHER',PADR(lcUpsType,7))
IF ALLTRIM(lcUpsType) <> 'OTHER'
  *-- Calculate freights             
  =lfGetFright(lcFromZone,SUBSTR(CUSTOMER.caddress5,1,3),lcUpsType  ,;
               llCod,llupsinsur,lcFreight,lcInsur,lcCOD,lcCartons,lcWeight,;
               lnCodCharge,lnInsCharge,lnShipAmt,lcChrgType)
  
  IF (llUpsBox .AND. SUBSTR(lcUpsType,1,5)='USUPS' AND !EMPTY(lcUpsFile))
    *-- Add records for each carton in the MUPSBox file
    =lfUpsBox(lcAccount,lcStore,lcOrderNo,lcPikTKt,llCod,llupsinsur,;
              &lcFreight,&lcInsur,&lcCOD,&lcCartons,&lcWeight,;
              lnShipAmt,lcUpsFile)
    IF llCod
    *-- calculate the cash on delivery amount
      &lcCOD_AMT = lnShipAmt+&lcFreight+&lcInsur+&lcCOD-lnShipAmt*lnDscPrcnt/100
      *-- ladata[1] = Invoice Number
      IF &lcCartons=1 .AND. SEEK(laData[1]+lcStore,lcUpsFile)
        *-- Update TotCod Field with the total amount because there is only one carton
        =lfUpCdFild()
      ENDIF
    ENDIF  
  ELSE
  *-- any other ups type 
    IF llCod
      &lcCOD_AMT = lnShipAmt+&lcFreight+&lcInsur+&lcCOD-lnDscPrcnt*lnShipAmt/100
    ENDIF
    &lcFreight = &lcFreight*(100+lcUpsIncr)/100 
  ENDIF
ELSE
*-- UPS TYPE =OTHER
  IF llCod
    &lcCOD_AMT = lnShipAmt-lnShipAmt*lnDscPrcnt/100
  ENDIF
  IF lnFxdPrcnt > 0
    &lcFreight = lnShipAmt*lnFxdPrcnt/100
  ENDIF
ENDIF
IF llCompTax
*-- CALCULATE TAXES
  &lcTaxAmnt = ROUND(lcTaxRate*(IIF(lcTaxRule='M',lnTaxDue,lnTaxDue+;
               &lcFreight+&lcInsur+&lcCOD)-ABS(lnTaxDue*lnDscPrcnt/100) )/100,2)
  *-- Calculate pst by the canadian method or the american one
  &lcPstAmnt = IIF(llIsCanada,(lnShipAmt+&lcFreight+&lcInsur+&lcCOD-;
                   ABS(lnShipAmt*lnDscPrcnt/100)+;
              IIF(VAL(lcPstRule)=1,0,&lcTaxAmnt))*lnPstRate/100,0)
  IF llCod
    *-- add the tax amount and the  pst amount to the cod amount  
    &lcCOD_AMT = &lcCOD_AMT+&lcTaxAmnt+&lcPstAmnt
    
    *-- laData[1] =invoice ,laData[5]=Store  
    IF llUpsBox .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. &lcCartons=1 ;
      .AND. !EMPTY(lcUpsFile) AND SEEK(laData[1]+laData[5],lcUpsFile)
    *-- if there is no more than one carton so total amount = cod amount 
      =lfUpCdFild()
    ENDIF
  ENDIF
ENDIF
             
*!*************************************************************
*! Name      : lfUpsBox
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Add records for each carton in the UPSBox file
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcAccount
*!             lcStore
*!             lcOrderNo
*!             lcPikTKt
*!             llCod
*!             llupsinsur
*!             lnFreight
*!             lnInsure
*!             lnCodAmt
*!             lnCartons
*!             lnWeight
*!             lnTotShpAmt
*!             lcUpsFile
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfUpsBox()
*!*************************************************************
FUNCTION lfUpsBox

PARAMETERS lcAccount,lcStore,lcOrderNo,lcPikTKt,llCod,llupsinsur,lnFreight,;
           lnInsure,lnCodAmt,lnCartons,lnWeight,lnTotShpAmt,lcUpsFile
lnAlias = SELECT()
lnCartons=ROUND(lnCartons,0)

*--  Get the no of carton from back_hdr incase packed pikTkt.
IF !EMPTY(lcPikTkt) AND SEEK(lcPikTkt,"Pack_Lin")
  PRIVATE lnCurPLTag,lnCartNo,lnCrtWgh
  lnCurPLTag = ORDER('Pack_Lin')  && save the Pick line order tag
  SET ORDER TO Packstyle IN Pack_Lin
  lnCartNo = 0    && Carton no 
  lnCrtWgh = 0    && Carton Weight
  SELECT (lcUpsFile)
  DELETE ALL
  SELECT Pack_Lin
  SCAN WHILE Pack_No+STR(No_Cart,4)+Style = lcPikTkt
    IF No_Cart # lnCartNo 
      lnCrtWgh = Pack_Lin.Weight
      SELECT (lcUpsFile)
      APPEND BLANK    && New line for each Carton
    ELSE
      lnCrtWgh = lnCrtWgh + Pack_Lin.Weight
    ENDIF
    REPLACE Account    WITH lcAccount ,;
            Order      WITH lcOrderNo ,;
            PikTkt     WITH lcPikTKt  ,;
            Store      WITH lcStore   ,;
            Cartons    WITH Pack_Lin.No_Cart  ,;
            Weight     WITH lnCrtWgh  ,;
            Decl_Value WITH IIF(llupsinsur,lnTotShpAmt/lnCartons,0),;
            Freight    WITH lnFreight/lnCartons ,;
            Insur      WITH IIF(llupsinsur,lnInsure/lnCartons,0)   ,;
            Cod        WITH IIF(llCod,lnCodAmt/lnCartons,0)      ,;
            TotalCod   WITH Decl_Value+Freight+Insur+Cod ,;
            InsurYn    WITH IIF(llupsinsur,'Y','N') ,;
            CodYn      WITH IIF(llCod,'Y','N')
  ENDSCAN
  SET ORDER TO (lnCurPLTag) IN Pack_Lin  && Restore the old tag
ELSE
*-- if there is no pick ticket
  SELECT (lcUpsFile)
  =SEEK(lcOrderNo+lcStore+lcPikTKt+STR(lnCartons+1,5))
  DELETE REST WHILE cMOrder+Store+PikTkt+STR(CARTONS,5)= lcOrderNo+lcStore+lcPikTKt
  FOR lnCount = 1 TO lnCartons
    IF !SEEK(lcOrderNo+lcStore+lcPikTKt+STR(lnCount,5))
    *-- if not  found  in the upsfile then add a new line
    *-- else replace it with the new values  
      APPEND BLANK
      REPLACE Account    WITH lcAccount ,;
              cMOrder    WITH lcOrderNo ,;
              PikTkt     WITH lcPikTKt  ,;
              Store      WITH lcStore   ,;
              Cartons    WITH lnCount
    ENDIF 
    REPLACE Weight     WITH lnWeight/lnCartons  ,;
            Decl_Value WITH IIF(llupsinsur,lnTotShpAmt/lnCartons,0),;
            Freight    WITH lnFreight/lnCartons ,;
            Insur      WITH IIF(llupsinsur,lnInsure/lnCartons,0)   ,;
            Cod        WITH IIF(llCod,lnCodAmt/lnCartons,0)      ,;
            TotalCod   WITH Decl_Value+Freight+Insur+Cod ,;
            InsurYn    WITH IIF(llupsinsur,'Y','N') ,;
            CodYn      WITH IIF(llCod,'Y','N')
  ENDFOR
ENDIF

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfGetFright
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/19996
*! Purpose   : Calculate freights
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: lcUpsFrom
*!             lcToZip
*!             lcUpsType
*!             llCod
*!             llupsinsur
*!             lcFreight
*!             lcInsur
*!             lcCOD
*!             lcCartons
*!             lcWeight
*!             lnCodCharge
*!             lnInsCharge
*!             lcChrgType : Charge Type: Freight, Insurance, Cod, All
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetFright()
*!*************************************************************
FUNCTION lfGetFright
PARAMETERS lcUpsFrom,lcToZip,lcUpsType,llCod,llupsinsur,lcFreight,lcInsur,;
           lcCOD,lcCartons,lcWeight,lnCodCharge,lnInsCharge,lnTotShpAmt,lcChrgType
PRIVATE lcCurrExac,lcCurrNear

IF INLIST(lcChrgType,'A','C')
*--  Recalculate the cod 
  &lcCOD   = IIF(llCod,IIF('USUPS' $ lcUpsType,lnCodCharge*&lcCartons,&lcCOD),0)
ENDIF
IF INLIST(lcChrgType,'A','I')
*--  Recalculate the insurance
  &lcInsur = IIF(llupsinsur,IIF('USUPS' $ lcUpsType AND lnTotShpAmt >=100,;
                 FLOOR(lnTotShpAmt/100)*lnInsCharge,0),0)               
ENDIF
IF lcUpsType = 'USUPSC' OR INLIST(lcChrgType,'I','C')
  RETURN
ENDIF

=gfOpenFile(gcSysHome+'sysRates', gcSysHome+'Frtrates','SH')
=gfOpenFile(gcSysHome+'syszones', gcSysHome+'Frtzones','SH')

IF !SEEK(lcUpsType+lcUpsFrom+lcToZip,'syszones') .AND. ;
   IIF(SUBSTR(lcUpsType,1,2)='CA',;
   !SEEK(lcUpsType+lcUpsFrom+SUBSTR(lcToZip,1,1),'syszones'),.T.)
  *-- Message : 40027
  *-- Invalid shipper postal zone or shipto zone! Unable to calculate freight.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM40027B00000','ALERT')
  RETURN
ENDIF
DO CASE
  CASE 'USUPS' $ lcUpsType
    &lcWeight    = IIF(&lcWeight > 0,&lcWeight,1)
    lnMaxWeight = 150
  CASE 'CAX' $ lcUpsType
    &lcWeight    = MAX(&lcWeight,0.05)
    lnMaxWeight = 150
  CASE 'CAC' $ lcUpsType
    &lcWeight    = IIF(&lcWeight > 0,&lcWeight,1)
    lnMaxWeight = 30
ENDCASE 
&lcCartons = IIF(&lcCartons <= 0,1,CEILING(&lcCartons))
IF (lnMaxWeight > 0) .AND. (&lcWeight/&lcCartons > lnMaxWeight)
  *-- Message : 40028
  *-- Weight cannot exceed xxx/carton! Carton count has been adjusted.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM40028B00000','ALERT',ALLTRIM(STR(lnMaxWeight)))
  &lcCartons = CEILING(&lcWeight/lnMaxWeight)
  RETURN
ENDIF
lcCurrExac = SET('EXAC')
lcCurrNear = SET('NEAR')
SET EXACT OFF
SET NEAR ON
DO CASE
  CASE 'USUPS' $ lcUpsType
    lcWghtPCrtn  = PADL(CEILING(&lcWeight/&lcCartons),6)
  CASE lcUpsType = 'CACOM  ' .OR. lcUpsType = 'CAXPR  '
    lcWghtPCrtn  = STR(&lcWeight/&lcCartons,6,2)
  OTHERWISE
    lnWghtPCrtn = IIF(INLIST(lcUpsType,'CASTAN ','CAECON '),;
                      CEILING(&lcWeight),CEILING(&lcWeight/&lcCartons))
    DO CASE
      CASE lnWghtPCrtn > 150 .AND. lnWghtPCrtn <= 500
        lnWghtPCrtn = 151
      CASE lnWghtPCrtn > 500
        lnWghtPCrtn = 501
    ENDCASE
    lcWghtPCrtn = PADL(lnWghtPCrtn,6)
ENDCASE 
IF INLIST(lcUpsType,'CASTAN ','CAECON ')
  IF &lcCartons > 1
    =SEEK(LEFT(lcUpsType,6)+'U'+syszones.Zone+lcWghtPCrtn,'sysRates')
  ELSE
    =SEEK(LEFT(lcUpsType,6)+'S'+syszones.Zone+lcWghtPCrtn,'sysRates')
  ENDIF
  &lcFreight = CEILING(&lcWeight) * sysRates.cFrtRate
ELSE
  =SEEK(lcUpsType+syszones.Zone+lcWghtPCrtn,'sysRates')
  &lcFreight = &lcCartons * sysRates.cFrtRate
ENDIF
SET EXACT &lcCurrExac
SET NEAR &lcCurrNear
IF 'USUPS' $ lcUpsType 
  &lcCOD   = IIF(llCod,lnCodCharge*&lcCartons,&lcCOD)
  &lcInsur = IIF(lnTotShpAmt >=100 .AND. llupsinsur,;
                 FLOOR(lnTotShpAmt/100)*lnInsCharge,&lcInsur)
ENDIF
RETURN
             
*!*************************************************************
*! Name      : gpUpsBox
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Invoice Cartons Details (UPS Box)
*!*************************************************************
*! Calls     : ARUps.SPX
*!*************************************************************
*! Parameters: lcUpsFile : UPS Box Temp. file name
*!             lcInvoice : Invoice#
*!             lcAccount : Account
*!             lcOrderNo : Order Number
*!             lcStore   : Store 
*!             lcPikTkt  : PikTkt Number
*!             llUpsInsur: UPS Insurance
*!             llCod     : COD
*!             lnShipAmt : Ship Amount
*!             lcShipVia : ShipVia
*!             lcWareCode: Warehouse
*!             lcCartons : Cartons Number
*!             lcWeight  : Weight
*!             lcFreight : Freight
*!             lcInsur   : Insurance
*!             lcCod     : Cod Charge
*!             lcCodAmnt : Cod Amount
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =gpUpsBox()
*!*************************************************************
FUNCTION gpUpsBox
PARAMETERS lcUpsFile,lcInvoice,lcAccount,lcOrderNo,lcStore,lcPikTkt,;
           llUpsInsur,llCod,lnShipAmt,lcShipVia,lcWareCode,lcCartons,;
           lcWeight,lcFreight,lcInsur,lcCod,lcCodAmnt
PRIVATE lnIWeight,lnIFreight,lnIInsure,lnICod,lnICodAmnt,lnICartons,lcBrowUps,;
        lnAlias,lnWeight,lnDecValue,lnFreight,lnInsure,lnCod,lnCodAmnt,;
        laShpViaFld,lcUpsType,lnCodCharge,lnFxdPrcnt,lcFromZone,lnInsCharge,lnUpsMark


lnAlias = SELECT()
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
  lcFromZone = IIF(SEEK(lcWareCode,'warehous'),WareHous.UPS,lcFromZone)
  =gfCloseFile('WAREHOUS')
ENDIF

=SEEK(IIF(EMPTY(lcStore),'M','S')+lcAccount+lcStore,'Customer')
DECLARE laShpViaFld[4,2]
laShpViaFld[1,1] = 'CUPS'
laShpViaFld[1,2] = 'lcUpsType'
laShpViaFld[2,1] = 'NCODCHARGE'
laShpViaFld[2,2] = 'lnCodCharge'
laShpViaFld[3,1] = 'NINSCHARGE'
laShpViaFld[3,2] = 'lnInsCharge'
laShpViaFld[4,1] = 'NFXDPRCNT'
laShpViaFld[4,2] = 'lnFxdPrcnt'
STORE '' TO lcUpsType
STORE 0  TO lnCodCharge,lnFxdPrcnt,lnInsCharge,lnUpsMark
=gfRltFld(lcShipVia,@laShpViaFld,'SHIPVIA')
lcUpsType = IIF(EMPTY(lcUpsType),'OTHER',PADR(lcUpsType,7))

lnICartons = &lcCartons
lnIWeight  = &lcWeight
lnIFreight = &lcFreight
lnIInsure  = &lcInsur
lnICod     = &lcCod
lnICodAmnt = &lcCodAmnt
SELECT (lcUpsFile)
IF laScrMode[4]
  =SEEK(lcOrderNo+lcStore+lcPikTkt)
  *-- get the weight ,cod ,frieght for each carton
  SUM REST Weight,Decl_Value,Freight,Insur,Cod,TotalCod ;
  TO       lnWeight,lnDecValue,lnFreight,lnInsure,lnCod,lnCodAmnt ;
  WHILE cMOrder+Store+PikTkt+STR(CARTONS,5) = lcOrderNo+lcStore+lcPikTkt
  =SEEK(lcOrderNo+lcStore+lcPikTkt)
ELSE
  =SEEK(lcInvoice)
  SUM REST Weight,Decl_Value,Freight,Insur,Cod,TotalCod ;
  TO       lnWeight,lnDecValue,lnFreight,lnInsure,lnCod,lnCodAmnt ;
  WHILE cMatInv+store+STR(cartons,5) = lcInvoice
  =SEEK(lcInvoice)
ENDIF
llNewUps = .F.
SCATTER MEMVAR
DO (gcScrDir+gcWinAppl+"\ARUPS.SPX")
IF laScrMode[4]     && Add mode
  SELECT (lcUpsFile)
  =SEEK(lcOrderNo+lcStore+lcPikTkt)
  SUM REST Weight,Decl_Value,Freight,Insur,Cod,TotalCod ;
  TO       &lcWeight,lnDecValue,&lcFreight,&lcInsur,&lcCod,&lcCodAmnt ;
  WHILE cMOrder+Store+PikTkt+STR(CARTONS,5) = lcOrderNo+lcStore+lcPikTkt
  &lcCartons = _TALLY   && Number of cartons
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfUpsBrow
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Browse Invoice Cartons Details (UPS Box)
*!*************************************************************
*! Calls     : lfShowUPS()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfUpsBrow()
*!*************************************************************
FUNCTION lfUpsBrow
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnUpsMark,'>',' '):H=' ' :R :1 :W=.F.,;
       Cartons :H='Box'  :P= '99999'  :R ,;
       Weight  :H='Weight' :P= '999999' :R ,;
       Decl_Value :H='Declared Value' :P= '9999999.99' :R ,;
       Freight :H='Freight '  :P= '999999999.99' :R ,;
       Insur   :H='Insurance' :P= '999999999.99' :R ,;
       Cod     :H='COD Charge':P= '999999999.99' :R ,;
       Dummy = Freight+Insur+Cod :H='Total Ship' :P= '9999999999.99' :R,;
       TotalCod  :H='COD Amount' :P='9999999999.99' :R ;
       WINDOW ARUPS1     ;
       IN WINDOW ARUPS   ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WHEN lfShowUPS() ;
       KEY IIF(laScrMode[4],lcOrderNo+lcStore+lcPikTkt,lcInvoice) ;
       TITLE lcBrowUPS

*!*************************************************************
*! Name      : lfDUPSBrow
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Deactivate function for Cartons details screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfDUPSBrow()
*!*************************************************************
FUNCTION lfDUPSBrow

IF WONTOP() = lcBrowUps
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTab WITH 'ARUPS2','m.Weight'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'ARUPS2','pbClsUps'
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfShowUps
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show function for Cartons details screen
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfShowUps()
*!*************************************************************
FUNCTION lfShowUps

lnUpsMark = RECNO()
SHOW WINDOW (lcBrowUps) REFRESH SAME
SCATTER MEMVAR
SHOW GETS WINDOW 'ARUPS2' ONLY
=lfRefresh('ARUPS2')

*!*************************************************************
*! Name      : lfvNewCrtn
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Add new invoice cartons to the UPS Box
*!*************************************************************
*! Calls     : lfShowUps()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvNewCrtn()
*!*************************************************************
FUNCTION lfvNewCrtn
PRIVATE lnNBoxNo

SELECT (lcUpsFile)
=SEEK(laData[1]+laData[5])
COUNT REST TO lnNBoxNo ;
WHILE cMOrder+Store+PikTkt+STR(CARTONS,5) = lcOrderNo+lcStore+lcPikTkt
INSERT INTO (lcUpsFile) (Account,cMOrder,PikTkt,Store,Cartons,InsurYn,CodYn) ;
       VALUES  (lcAccount,lcOrderNo,lcPikTkt,lcStore,lnNBoxNo+1,;
                IIF(llUpsInsur,'Y','N'),IIF(llCod,'Y','N'))
SHOW GET m.Weight ENABLE
SHOW GET pbRemCrtn ENABLE
IF llCod
  SHOW GET m.TotalCod ENABLE
ENDIF
IF llUpsInsur
  SHOW GET m.Decl_Value ENABLE
ENDIF
*-- Show function for Cartons details screen
=lfShowUps()
_CUROBJ = OBJNUM(m.Weight)

*!*************************************************************
*! Name      : lfvRemCrtn
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Remove invoice carton from the UPS Box
*!*************************************************************
*! Calls     : lfShowUps()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvRemCrtn()
*!*************************************************************
FUNCTION lfvRemCrtn
PRIVATE lnNBoxNo

lnWeight   = lnWeight   - m.Weight
lnDecValue = lnDecValue - m.Decl_Value
lnFreight  = lnFreight  - m.Freight
lnInsure   = lnInsure   - m.Insur
lnCod      = lnCod      - m.Cod
lnCodAmnt  = lnCodAmnt  - m.TotalCod
SELECT (lcUpsFile)
DELETE
lnNBoxNo = 1
=SEEK(lcOrderNo+lcStore+lcPikTkt)
SCAN REST WHILE cMOrder+Store+PikTkt+STR(CARTONS,5) = lcOrderNo+lcStore+lcPikTkt
  =RLOCK()
  REPLACE Cartons WITH lnNBoxNo
  UNLOCK
  lnNBoxNo = lnNBoxNo + 1
ENDSCAN
IF !SEEK(lcOrderNo+lcStore+lcPikTkt)
  SHOW GETS WINDOW 'ARUPS2' DISABLE ONLY
  SHOW GET ibBrowUps ENABLE
  SHOW GET pbNewCrtn ENABLE
  SHOW GET pbcLSuPS  ENABLE
ENDIF
*-- Show function for Cartons details screen
=lfShowUps()

*!*************************************************************
*! Name      : lfvCrtChrg
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Recalculate charges per carton
*!*************************************************************
*! Calls     : lfGetFright(),lfShowUps()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvCrtChrg()
*!*************************************************************
FUNCTION lfvCrtChrg
PRIVATE lnOneCrtn
lnOneCrtn=1
IF UPDATE()
  *--Calculate freights             
  =lfGetFright(lcFromZone,SUBSTR(CUSTOMER.caddress5,1,3),lcUpsType,;
               llCod,llUpsInsur,'m.Freight','m.Insur','m.Cod',;
               'lnOneCrtn','m.Weight',lnCodCharge,lnInsCharge,m.Decl_Value, 'A')
  
  lnWeight   = lnWeight   - &lcUpsFile..Weight     + m.Weight
  lnDecValue = lnDecValue - &lcUpsFile..Decl_Value + m.Decl_Value
  lnFreight  = lnFreight  - &lcUpsFile..Freight    + m.Freight
  lnInsure   = lnInsure   - &lcUpsFile..Insur      + m.Insur
  lnCod      = lnCod      - &lcUpsFile..Cod        + m.Cod
  lnCodAmnt  = lnCodAmnt  - &lcUpsFile..TotalCod   + m.TotalCod
  SELECT (lcUpsFile)
  =RLOCK()
  GATHER MEMVAR
  UNLOCK
  =lfShowUps()
ENDIF

*!*************************************************************
*! Name      : lfvcrtTag
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Validate Carton COD TAG#
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvcrtTag()
*!*************************************************************
FUNCTION lfvcrtTag
SELECT (lcUpsFile)
=RLOCK()
REPLACE CODTAG WITH m.CODTAG
UNLOCK

*!*************************************************************
*! Name      : gpCharges
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : England Charges Screen
*!*************************************************************
*! Calls     : ARChrg.SPX
*!*************************************************************
*! Parameters: lcChrgFile: Charges Temp. file name
*!             lcInvoice : Invoice Number 
*!             lcAccount : Account
*!             lcOrderNo : Order Number
*!             lcStore   : Store 
*!             lcPikTkt  : PikTkt Number
*!             lnTrdDsc  : Trade Discount Percent
*!             lcTotChrg : Invoice total charges
*!             lcTotTax  : Invoice total charges tax
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =gpCharges()
*!*************************************************************
FUNCTION gpCharges
PARAMETERS lcChrgFile,lcInvoice,lcAccount,lcOrderNo,lcStore,lcPikTkt,;
           lnTrdDsc,lcTotChrg,lcTotTax
PRIVATE lnAlias,laLCodes,laCharges,lnChrgMark,lnTotChrg,lnTotTax,llCompTax,;
        laChRltFld
PRIVATE lnOldChrg
STORE 0 TO lnOldChrg
lnAlias = SELECT()
*-- company setup information
llCompTax = (gfGetMemVar('M_TAX',gcAct_Comp)='Y')  && Use Taxes = Y
llGlLink  = (gfGetMemVar('M_LINK_GL',gcAct_Comp)='Y')  && GL link
lnChrgMark = 0
DECLARE laLCodes[1,10],laCharges[1,2]
STORE '' TO laCharges
lnTotChrg = &lcTotChrg
=IIF(laScrMode[2] OR laScrMode[3] ,gfOpenFile(gcDataDir+'mInvChrg',gcDataDir+'mInvChrg','SH'),.T.)
SELECT (lcChrgFile)
IF laScrMode[4]  && Add Mode
  =SEEK(lcOrderNo+lcStore+lcPikTkt)
  SUM REST nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 TO lnTotTax ;
  WHILE cMOrder+Store+PikTkt+cchrgcode = lcOrderNo+lcStore+lcPikTkt
  =SEEK(lcOrderNo+lcStore+lcPikTkt)
ELSE
  =SEEK(lcInvoice+lcStore)
  SUM REST nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 TO lnTotTax ;
  WHILE cMatInv+cstore+cchrgcode = lcInvoice+lcStore
  =SEEK(lcInvoice+lcStore)
ENDIF  
llNewChrg = .F.
SCATTER MEMVAR
laLCodes[1,1]  = 'CCHRGCODE'
laLCodes[1,2]  = 'laCharges'    
laLCodes[1,3]  = 'puCharges'
laLCodes[1,4]  = ''
laLCodes[1,5]  = .F.
laLCodes[1,6]  = .F.
laLCodes[1,7]  = lcChrgFile
laLCodes[1,8]  = lcChrgFile
laLCodes[1,9]  = IIF(laScrMode[2],'lcInvoice+lcStore+m.cchrgcode',;
                     'lcOrderNo+lcStore+lcPikTkt+m.cchrgcode')
laLCodes[1,10] = 'CCHRGCODE'
=gfwCodePop(@laLCodes,'CCHRGCODE','N')
DO (gcScrDir+gcWinAppl+"\ARChrg.SPX")
IF laScrMode[4]
  SELECT (lcChrgFile)
  =SEEK(IIF(laScrMode[2],lcInvoice,lcOrderNo+lcStore+lcPikTkt))
  SUM REST nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 TO lnTotTax ;
  WHILE cMOrder+Store+PikTkt+cchrgcode = lcOrderNo+lcStore+lcPikTkt
  &lcTotChrg = lnTotChrg
  &lcTotTax  = lnTotTax
ENDIF
=IIF(laScrMode[2] OR laScrMode[3] ,gfCloseFile('mInvChrg'),.T.)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfChrgBrow
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Browse England Charges
*!*************************************************************
*! Calls     : lfShowChrg
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfChrgBrow()
*!*************************************************************
FUNCTION lfChrgBrow
PRIVATE lcFields

lcFields = "cMarker =IIF(RECNO()=lnChrgMark,'>',' '):H=' ':R:1:W=.F.,"+;
           "cDesc=gfCodDes(cChrgCode,'CCHRGCODE') :H= 'Charge' :30 :R,"+;
           "nChrgAmnt :H= 'Amount' :9 :R"
lcFields = lcFields + IIF(llCompTax,",nTaxRate :H='Tax Rate' :R,;
nTaxAmt=nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 :H='Tax Amount' :P='99999999.99' :R",'')
lcFields = lcFields + IIF(llGlLink,",cFrgtAcnt :H='G.L. Account' :R",'')

lnChrgMark = RECNO()
BROWSE FIELDS &lcFields  ;
       WINDOW ARChrg1    ;
       IN WINDOW ARChrg  ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShowChrg() ;
       KEY IIF(laScrMode[2],lcInvoice+lcStore,lcOrderNo+lcStore+lcPikTkt) ;
       TITLE lcBrowChrg

*!*************************************************************
*! Name      : lfShowChrg
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Show England Charges
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfShowChrg()
*!*************************************************************
FUNCTION lfShowChrg

lnChrgMark = RECNO()
SHOW WINDOW (lcBrowChrg) REFRESH SAME
SCATTER MEMVAR
=gfwCodePop(@laLCodes,'CCHRGCODE','T')
SHOW GETS WINDOW 'ARCHRG2'

*!*************************************************************
*! Name      : lfvNewChrg
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Add new charge
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvNewChrg()
*!*************************************************************
FUNCTION lfvNewChrg
PRIVATE lcCurrKey,lnChrgCount

SELECT (lcChrgFile)
lcCurrKey = cMOrder+Store+PikTkt+cchrgcode 
COUNT TO lnChrgCount 
=SEEK(lcCurrKey)
IF lnChrgCount = 10
  *-- Message : 40026
  *-- You can enter upto 10 differennt charge codes.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM40026B00000','ALERT')
  RETURN
ENDIF
SCATTER MEMVAR BLANK
SHOW GETS WINDOW 'ARCHRG2' DISABLE
SHOW GET puCharges  ENABLE
_CUROBJ = OBJNUM(puCharges)
llNewChrg = .T.
  
*!*************************************************************
*! Name      : lfDChrgBrow
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Deactivate function for Charges screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfDChrgBrow()
*!*************************************************************
FUNCTION lfDChrgBrow

IF WONTOP() = lcBrowChrg
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTab WITH 'ARCHRG2','puCharges'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'ARCHRG2','pbClsChrg'
ENDIF  
RETURN .F.
  
*!*************************************************************
*! Name      : lfvChrgCode
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Validate England Charge Codes
*!*************************************************************
*! Calls     : gfRltFld
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvChrgCode()
*!*************************************************************
FUNCTION lfvChrgCode

m.cChrgCode = ALLTRIM(laCharges[puCharges,2])
IF !llNewChrg .AND. m.cChrgCode = cChrgCode
  RETURN
ENDIF
lcOldChrg = cChrgCode
*-- Message : 40025
*-- Charge code xxx has already been selected!
*-- Button : 00000
*-- Ok
IF SEEK(lcOrderNo+lcStore+lcPikTkt+m.cChrgCode,lcChrgFile) .AND. ;
  gfModalGen('INM40025B00000','ALERT',m.cChrgCode)=1
  SCATTER MEMVAR
  SHOW GETS WINDOW 'ARCHRG2' ENABLE
  RETURN
ENDIF
=SEEK(lcOrderNo+lcStore+lcPikTkt+lcOldChrg,lcChrgFile)
IF llCompTax
  DECLARE laChRltFld[1,2]
  STORE '' TO lcTaxRate
  laChRltFld[1,1] = 'CTAXCODE'
  laChRltFld[1,2] = 'lcTaxRate'
  =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
  IF !EMPTY(lcTaxRate)
    laChRltFld[1,1] = 'NTAXRATE'
    laChRltFld[1,2] = 'lcTaxRate'
    =gfRltFld(ALLTRIM(lcTaxRate),@laChRltFld,'CTAXCODE')
    m.nTaxRate = lcTaxRate
  ENDIF
  SELECT (lcChrgFile)  
ENDIF
IF llGlLink
  DECLARE laChRltFld[1,2]
  STORE '' TO lcFrgtAcnt
  laChRltFld[1,1] = 'CFRGTACNT'
  laChRltFld[1,2] = 'lcFrgtAcnt'
  =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
  m.cFrgtAcnt= lcFrgtAcnt
  SELECT (lcChrgFile)  
ENDIF

IF llNewChrg
  INSERT INTO (lcChrgFile) (cMatInv,cMOrder,PikTkt,cStore) ;
  VALUES (lcInvoice,lcOrderNo,lcPikTkt,lcStore)
  llNewChrg = .F.
ENDIF
lnTotTax = lnTotTax - (nTaxRate-m.nTaxRate)*m.nChrgAmnt*(1-lnTrdDsc/100)/100
=RLOCK()

REPLACE cChrgCode WITH m.cChrgCode ,;
        nTaxRate  WITH m.nTaxRate  ,;
        cFrgtAcnt WITH m.cFrgtAcnt

UNLOCK
SHOW WINDOW (lcBrowChrg) REFRESH SAME
SHOW GETS WINDOW 'ARCHRG2' ENABLE
SHOW GETS WINDOW 'ARCHRG0' OFF
RETURN

*!*************************************************************
*! Name      : lfvChrgCode
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Remove England Charge Code
*!*************************************************************
*! Calls     : gfRltFld
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvChrgCode()
*!*************************************************************
FUNCTION lfvRemChrg

*-- Message : 40011
*-- Are you sure you want to remove this xxx?
*-- Button : 40000
*-- Yes No
IF gfModalGen('QRM40011B40000','ALERT','charge code')= 1
  lnTotChrg = lnTotChrg - nChrgAmnt
  lnTotTax  = lnTotTax  - nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100
  DELETE
  GO TOP
  SCATTER MEMVAR
  IF EOF()
    =gfwCodePop(@laLCodes,'CCHRGCODE','N')
    SHOW GETS WINDOW 'ARCHRG2' DISABLE
    SHOW GET pbNewChrg ENABLE
    SHOW GET pbClsChrg ENABLE
  ELSE
    SHOW GETS WINDOW 'ARCHRG2' ENABLE
  ENDIF
  SHOW WINDOW (lcBrowChrg) REFRESH SAME
  =lfRefresh()
ENDIF

*!*************************************************************
*! Name      : lfvChrgAmnt
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : England Charge Code Amount
*!*************************************************************
*! Calls     : gfRltFld
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvChrgAmnt()
*!*************************************************************
FUNCTION lfvChrgAmnt
IF m.nChrgAmnt <0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Buttfon  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.nChrgAmnt=lnOldChrg
  _CUROBJ = _CUROBJ  
  RETURN
ENDIF
lnTotChrg = lnTotChrg - nChrgAmnt + m.nChrgAmnt
lnTotTax  = lnTotTax  - (nChrgAmnt-m.nChrgAmnt)*(1-lnTrdDsc/100)*nTaxRate/100
=RLOCK()
REPLACE nChrgAmnt WITH m.nChrgAmnt
UNLOCK
SHOW GETS WINDOW 'ARCHRG0' OFF
SHOW GETS WINDOW 'ARCHRG2' OFF

*!*************************************************************
*! Name      : lfvChrgAcnt
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : England Charge Code Gl Account
*!*************************************************************
*! Calls     : gfRltFld
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvChrgAcnt()
*!*************************************************************
FUNCTION lfvChrgAcnt
llFrmBrow = llBrowse

REPLACE cFrgtAcnt WITH m.cFrgtAcnt
=lfvGlAccnt()
llBrowse=.F.
=RLOCK()
REPLACE cFrgtAcnt WITH m.cFrgtAcnt
UNLOCK

*!*************************************************************
*! Name      : lfTaxDesc
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : England Charge Code tax description
*!*************************************************************
*! Calls     : gfRltFld,gfCodDes
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfTaxDesc()
*!*************************************************************
FUNCTIO lfTaxDesc

DECLARE laChRltFld[1,2]
STORE '' TO lcTaxRate
laChRltFld[1,1] = 'CTAXCODE'
laChRltFld[1,2] = 'lcTaxRate'
=gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
RETURN(gfCodDes(ALLTRIM(lcTaxRate),'CTAXCODE'))

*!*************************************************************
*! Name      : lfGtOrder
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Read order number manually
*!*************************************************************
*! Calls     : ARORDER.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  Order#
*!*************************************************************
*! Example            :  =lfGtOrder()
*!*************************************************************
FUNCTION lfGtOrder
PRIVATE lcOrderNo
STORE SPACE(6) TO lcOrderNo
DO (gcScrDir+"ARORDER.SPX") WITH 'O','lcOrderNo','O'
RETURN(lcOrderNo)

*!*************************************************************
*! Name      : gpInvInstm
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice Installment
*!*************************************************************
*! Calls     : ARINST.SPX
*!*************************************************************
*! Passed Parameters  :  lcInvoice  : Invoice Number
*!                       lcOrder    : Order Number
*!                       lcStore    : Store Number
*!                       lcPikTkt   : PikTkt Number
*!                       lnTotalChg : Invoice Total Charge
*!                       ldInvDate  : Invoice Date,lcInstHdr,lcInstLine
*!                       lcInstHdr  : Installment temp header file
*!                       lcInstLine : Installment temp details file
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            : DO gpInvInstm WITH  lcInvoice,lcOrder,lcStore,;
*!                      lcPikTkt,lnTotalChg,ldInvDate,lcInstHdr,lcInstLine
*!*************************************************************
FUNCTION gpInvInstm
PARAMETERS lcInvoice,lcOrder,lcStore,lcPikTkt,lnTotalChg,ldInvDate,;
           lcInstHdr,lcInstLine
PRIVATE lnInstMark,puInstType,lnInitAmnt,lnInitPcnt,lnInstFreq,lnBalance,;
        lcInstRef,lnINstNo,lcOldValue,lcBalColor,lnInstmAmnt

STORE 0 TO lnBalance
IF laScrMode[4] AND !SEEK(lcOrder+lcStore+lcPikTkt,lcInstHdr)
*-- Add the istalment header line
  INSERT INTO (lcInstHdr) (cMOrder,Store,PikTkt,cInstmType,dInstmStDt,nInstmFreq) VALUES ;
              (lcOrder,lcStore,lcPikTKt,'D',ldInvDate,1)
  STORE lnTotalChg TO lnBalance
ENDIF
lcBalColor = IIF(lnBalance>0,"RGB(255,255,255,255,0,0)","RGB(255,255,255,0,128,0)")
IF laScrMode[2] OR  laScrMode[3]
  =gfOpenFile(gcDataDir+'ARMINSTH', gcDataDir+'ARINSTMH','SH')
  =gfOpenFile(gcDataDir+'ARMINSTD', gcDataDir+'ARINSTMD','SH')
  =SEEK(lcInvoice,lcInstHdr)
ENDIF
STORE '' TO lcOldValue
STORE  1 TO lnInstMark
puInstType = AT(&lcInstHdr..cInstmType,'DNM')
STORE &lcInstHdr..nInstIAmnt TO lnInitAmnt    && Initial  amount 
STORE &lcInstHdr..nInstIPcnt TO lnInitPcnt    && Initial Percent
STORE &lcInstHdr..nInstmFreq TO lnInstFreq    &&
STORE &lcInstHdr..dInstmStDt TO ldStartDate   && Start Date
STORE &lcInstHdr..cInstmRef  TO lcInstRef     &&  
STORE &lcInstHdr..nNoInstm   TO lnInstNo      && Instalment No
STORE &lcInstHdr..nInstmAmnt TO lnInstmAmnt   && Instalment Amount
DO (gcScrDir+gcWinAppl+"\ARINST.SPX")
IF laScrMode[2] OR  laScrMode[3]
  =gfCloseFile('ARMINSTH')
  =gfCloseFile('ARMINSTD')
ENDIF

*!*************************************************************
*! Name      : lfInstBrow
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse invoice installments
*!*************************************************************
*! Calls     : lfShowInst,lfwDueDate,lfvIDueDate,lfwAmount,lfvAmount,
*!             lfwPercent,lfvPercent
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfInstBrow()
*!*************************************************************
FUNCTION lfInstBrow

SELECT (lcInstLine)
lnInstMark = RECNO()
IF laScrMode[4]
  BROWSE FIELDS cMarker=IIF(RECNO()=lnInstMark ,'>',' '):H=' ':R:1:W=.F.,;
                cInstalNo  :H='Installment#' :R,;
                DueDate    :H='Due Date' :W=lfwDueDate() :V=lfvIDueDate(),;
                nInstmAmnt :H='Amount'   :W=lfwAmount()  :V=lfvAmount()  ,;
                nInstmPcnt :H='Percent'  :W=lfwPercent() :V=lfvPercent() ,;
                cInstmNote :H='Note' ;
         WINDOW ARINST1    ;
         IN WINDOW ARINST  ;
         NOMENU            ;        
         NOAPPEND          ;
         NODELETE          ;        
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowInst() ;
         KEY lcOrder+lcStore+lcPikTkt ;
         VALID :F lfvInst();
         TITLE lcBrowInst
ELSE
  BROWSE FIELDS cMarker=IIF(RECNO()=lnInstMark ,'>',' '):H=' ':R:1:W=.F.,;
                cInstalNo  :H='Installment#' :R,;
                DueDate    :H='Due Date' :R,;
                nInstmAmnt :H='Amount'   :R,;
                nInstmPcnt :H='Percent'  :R,;
                cInstmNote :H='Note'     :R ;
         WINDOW ARINST1    ;
         IN WINDOW ARINST  ;
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfShowInst() ;
         KEY lcInvoice     ;
         TITLE lcBrowInst
ENDIF

*!*************************************************************
*! Name      : lfDInvInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Deactivate invoice installments screen
*!*************************************************************
*! Calls     : lpTab,lpBackTab
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDInvInst()
*!*************************************************************
FUNCTION lfDInvInst
IF WONTOP()=lcBrowInst
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB DO lpTab WITH 'ARINST2','pbNewInst'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'ARINST0','lcInstRef'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfvInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice installments browse validation
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInst()
*!*************************************************************
FUNCTION lfvInst
IF EMPTY(DueDate) .OR. nInstmAmnt=0
  RETURN(.F.)
ENDIF

*!*************************************************************
*! Name      : lfShowInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Show Invoice installments
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowInst()
*!*************************************************************
FUNCTION lfShowInst
SELECT (lcInstLine)
lnInstMark = RECNO()
SHOW WINDOW (lcBrowInst) REFRESH SAME

*!*************************************************************
*! Name      : lfvClrInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Clear Invoice installments
*!*************************************************************
*! Calls     : lfShowInst,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvClrInst()
*!*************************************************************
FUNCTION lfvClrInst

SELECT (lcInstLine)
DELETE ALL
STORE 0  TO lnInstNo,lnInitAmnt,lnInitPcnt,lnInstmAmnt
STORE lnTotalChg TO lnBalance
STORE '' TO lcInstRef
STORE 1  TO puInstType,lnInstFreq
lcBalColor = "RGB(255,255,255,255,0,0)"
SELECT (lcInstHdr)
=RLOCK()
REPLACE cInstmType WITH SUBSTR('DNM',puInstType,1) ,;
        nInstIAmnt WITH lnInitAmnt  ,;
        nInstmAmnt WITH lnInstmAmnt ,;        
        nInstIPcnt WITH lnInitPcnt  ,;
        nInstmFreq WITH lnInstFreq  ,;
        dInstmStDt WITH ldStartDate ,;
        cInstmRef  WITH lcInstRef   ,;
        nNoInstm   WITH lnInstNo
UNLOCK
SHOW GET puInstType  ENABLE
SHOW GET lnInstFreq  ENABLE
SHOW GET lnInstNo    ENABLE
SHOW GET lnInitAmnt  ENABLE
SHOW GET lnInitPcnt  ENABLE
SHOW GET lcInstRef   ENABLE
SHOW GET ldStartDate ENABLE
SHOW GET pbGenInst   ENABLE
SHOW GET pbNewInst   DISABLE
SHOW GET pbRemInst   DISABLE
SHOW GET pbClrInst   DISABLE
=lfShowInst()
=lfRefresh('ARINST0')

*!*************************************************************
*! Name      : lfvClsInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Close Invoice installments Screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvClsInst()
*!*************************************************************
FUNCTION lfvClsInst

IF laScrMode[4] 
  IF !SEEK(lcOrder+lcStore+lcPikTkt,lcInstLine)
  *-- if there is no lines so delete the header
    SELECT (lcInstHdr)
    DELETE
  ELSE
    IF lnBalance > 0
      RETURN
    ENDIF
  ENDIF
ENDIF  
CLEAR READ

*!*************************************************************
*! Name      : lfvNewInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Add new Invoice installment
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNewInst()
*!*************************************************************
FUNCTION lfvNewInst

IF lnBalance <= 0
  RETURN
ENDIF
lnInstNo=lnInstNo+1
*-- insert new instalment line
INSERT INTO (lcInstLine) ;
(cMOrder,Store,PikTkt,cInstalNo,nInstmAmnt,nInstmPcnt) VALUES ;
(lcOrder,lcStore,lcPikTkt,PADL(lnInstNo,3,'0'),lnBalance,lnBalance*100/lnTotalChg)
lnBalance = 0
lnInstmAmnt=lnInstmAmnt+&lcInstLine..nInstmAmnt  && add the new line amount to the total amount
lcBalColor = "RGB(255,255,255,0,128,0)"

=lfRefresh('ARINST0')
SHOW GET lnInstNo   DISABLE
SHOW GET pbRemInst  ENABLE
_CUROBJ = OBJNUM(ibInvInst)

*!*************************************************************
*! Name      : lfvRemInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Remove new Invoice installment
*!*************************************************************
*! Calls     : lfRefresh,lfShowInst
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemInst()
*!*************************************************************
FUNCTION lfvRemInst

SELECT (lcInstLine)
lnBalance   = lnBalance + nInstmAmnt
lnInstmAmnt = lnInstmAmnt - nInstmAmnt
DELETE
STORE 0 TO lnInstNo
=SEEK(lcOrder+lcStore+lcPikTkt)
SCAN REST WHILE cMOrder+Store+PikTkt+cInstalNo = lcOrder+lcStore+lcPikTkt
  lnInstNo=lnInstNo+1
  REPLACE cInstalNo WITH PADL(lnInstNo,3,'0')
ENDSCAN
lcBalColor = IIF(lnBalance>0,"RGB(255,255,255,255,0,0)","RGB(255,255,255,0,128,0)")
=SEEK(lcOrder+lcStore+lcPikTkt)
SHOW GET lnInstNo DISABLE
=lfShowInst()
=lfRefresh('ARINST0')
IF lnInstNo= 0
  SHOW GET pbRemInst DISABLE
ENDIF

*!*************************************************************
*! Name      : lfwAmount
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice installment Amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwAmount()
*!*************************************************************
FUNCTION lfwAmount
IF puInstType=3
  lcOldValue = nInstmAmnt
ELSE
  RETURN(.F.)
ENDIF  

*!*************************************************************
*! Name      : lfvAmount
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice installment Amount
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAmount()
*!*************************************************************
FUNCTION lfvAmount

=RLOCK()
IF lnBalance+lcOldValue-nInstmAmnt < 0
  *-- Message : 40123
  *-- Installments amount cannot be greater than invoice total charge!
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM40123B00000','ALERT')
  REPLACE nInstmAmnt WITH lcOldValue
ELSE
  REPLACE nInstmPcnt WITH nInstmAmnt*100/lnTotalChg
ENDIF
UNLOCK
lnBalance = lnBalance + lcOldValue - nInstmAmnt
lnInstmAmnt  = lnInstmAmnt  - lcOldValue + nInstmAmnt
lcBalColor = IIF(lnBalance>0,"RGB(255,255,255,255,0,0)","RGB(255,255,255,0,128,0)")
=lfRefresh('ARINST0')

*!*************************************************************
*! Name      : lfwPercent
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice installment Amount Percent
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwPercent()
*!*************************************************************
FUNCTION lfwPercent
IF puInstType=3
  lcOldValue = nInstmPcnt
ELSE
  RETURN(.F.)
ENDIF  

*!*************************************************************
*! Name      : lfvPercent
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice installment Amount Percent
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPercent()
*!*************************************************************
FUNCTION lfvPercent

=RLOCK()
IF lnBalance+(lcOldValue-nInstmPcnt)*lnTotalChg/100 < 0
  *-- Message : 40123
  *-- Installments amount cannot be greater than invoice total charge!
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM40123B00000','ALERT')
  REPLACE nInstmPcnt WITH lcOldValue
ELSE
  REPLACE nInstmAmnt WITH lnTotalChg*nInstmPcnt/100
ENDIF
UNLOCK
lnBalance = lnBalance + (lcOldValue-nInstmPcnt)*lnTotalChg/100
lnInstmAmnt  = lnInstmAmnt  - (lcOldValue-nInstmPcnt)*lnTotalChg/100
lcBalColor = IIF(lnBalance>0,"RGB(255,255,255,255,0,0)","RGB(255,255,255,0,128,0)")
=lfRefresh('ARINST0')

*!*************************************************************
*! Name      : lfwDueDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice installment Due Date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwDueDate()
*!*************************************************************
FUNCTION lfwDueDate
IF puInstType=3
  lcOldValue = DueDate
ELSE
  RETURN(.F.)
ENDIF  

*!*************************************************************
*! Name      : lfvIDueDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice installment Due Date
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvIDueDate()
*!*************************************************************
FUNCTION lfvIDueDate
PRIVATE ldPrvDate

SKIP -1
IF BOF() .OR. (cMOrder+Store+PikTkt <> lcOrder+lcStore+lcPikTkt)
  ldPrvDate = ldStartDate
  =SEEK(lcOrder+lcStore+lcPikTkt)
ELSE
  ldPrvDate = DueDate
  SKIP
ENDIF
=RLOCK()
IF DueDate < ldPrvDate
  *-- Message : 40126
  *-- Installment due date should be greater than xx/xx/xxxx
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM40126B00000','ALERT',DTOS(ldPrvDate))
  REPLACE DueDate WITH lcOldValue
ENDIF
UNLOCK

*!*************************************************************
*! Name      : lfvInstFreq
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice installment period
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInstFreq()
*!*************************************************************
FUNCTION lfvInstFreq
ldStartDate = IIF(puInstType=1,ldInvDate+lnInstFreq,GOMONTH(ldInvDate,lnInstFreq))
SHOW GET ldStartDate 

*!*************************************************************
*! Name      : lfvInstType
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Invoice installment Method
*!*************************************************************
*! Calls     : lfShowInst,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInstType()
*!*************************************************************
FUNCTION lfvInstType

IF lcOldValue<>puInstType
  IF puInstType=3
    STORE 0 TO lnInstFreq,lnInstNo
    SHOW GET lnInstFreq DISABLE
    SHOW GET lnInstNo   DISABLE
  ELSE
    STORE 1 TO lnInstFreq
    SHOW GET lnInstFreq ENABLE
    SHOW GET lnInstNo   ENABLE
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfvStartDate
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : First installment Date
*!*************************************************************
*! Calls     : lfReInstall
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStartDate()
*!*************************************************************
FUNCTION lfvStartDate

IF ldStartDate < ldInvDate
  *-- Message : 40122
  *-- Installments start date cannot be less than invoice date!
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM40122B00000','ALERT')
  ldStartDate =lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*!*************************************************************
*! Name      : lfvInitAmnt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Installment Initial amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInitAmnt()
*!*************************************************************
FUNCTION lfvInitAmnt

IF lnInitAmnt > lnTotalChg
  *-- Message : 40123
  *-- Installments amount cannot be greater than invoice total charge!
  *-- Button : 00000
  *-- Ok
  =gfModalGen('TRM40123B00000','ALERT')
  lnInitAmnt=lcOldValue
  RETURN
ENDIF
lnInitPcnt=lnInitAmnt*100/lnTotalChg   && Calculate the initial percent
STORE lnTotalChg-lnInitAmnt TO lnBalance,lnInstmAmnt
SHOW GET lnInitPcnt
=lfRefresh('ARINST0')

*!*************************************************************
*! Name      : lfvInitPcnt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Installment Initial Percent
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInitPcnt()
*!*************************************************************
FUNCTION lfvInitPcnt

lnInitAmnt = lnTotalChg*lnInitPcnt/100 && Calculate the initial amount
STORE lnTotalChg-lnInitAmnt TO lnBalance,lnInstmAmnt  
SHOW GET lnInitAmnt
=lfRefresh('ARINST0')

*!*************************************************************
*! Name      : lfvGenInst
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Generate installments automatically
*!*************************************************************
*! Calls     : lfShowInst,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvGenInst()
*!*************************************************************
*! Modifications      :
*!*************************************************************
FUNCTION lfvGenInst
PRIVATE ldDueDate,lnAmount,lnCount,lnPercent

IF puInstType = 3
  SHOW GET pbNewInst ENABLE
  STORE 0 TO lnInstmAmnt
ELSE
IF lnInstNo=0
  RETURN
ENDIF
SELECT (lcInstLine)
DELETE ALL
lnAmount  = (lnTotalChg-lnInitAmnt)/lnInstNo
lnPercent = lnAmount*100/lnTotalChg
ldDueDate = ldStartDate

PRIVATE lnTotInst
lnTotInst = 0
FOR lnCount = 1 TO lnInstNo
  INSERT INTO (lcInstLine) ;
  (cMOrder,Store,PikTkt,cInstalNo,nInstmAmnt,DueDate,nInstmPcnt) VALUES ;
  (lcOrder,lcStore,lcPikTkt,PADL(lnCount,3,'0'),lnAmount,ldDueDate,lnPercent)
  lnTotInst = lnTotInst + &lcInstLine..nInstmAmnt
  DO CASE
    CASE puInstType=1
      ldDueDate = ldDueDate + lnInstFreq
    CASE puInstType=2
      ldDueDate = GOMONTH(ldDueDate,lnInstFreq)
    OTHERWISE
      ldDueDate = ldDueDate + lnInstFreq
  ENDCASE
ENDFOR
REPLACE nInstmAmnt WITH nInstmAmnt + ((lnTotalChg-lnInitAmnt)-lnTotInst)
GO TOP
lnBalance = 0
lcBalColor = "RGB(255,255,255,0,128,0)"
=lfShowInst()
ENDIF
=lfRefresh('ARINST0')
SHOW GETS WINDOW 'ARINST0' DISABLE ONLY
SHOW GET pbClrInst ENABLE
SHOW GET pbGenInst DISABLE
SELECT (lcInstHdr)
=RLOCK()
REPLACE cInstmType WITH SUBSTR('DNM',puInstType,1) ,;
        nInstmFreq WITH lnInstFreq  ,;
        nInstIAmnt WITH lnInitAmnt  ,;
        nInstmAmnt WITH lnInstmAmnt ,;
        nInstIPcnt WITH lnInitPcnt  ,;
        dInstmStDt WITH ldStartDate ,;
        nNoInstm   WITH lnInstNo    ,;
        cInstmRef  WITH lcInstRef
UNLOCK

*!*************************************************************
*! Name      : gpOpnCredits
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Apply Invoice to customer credits
*!*************************************************************
*! Calls     : ARAPCRD.SPX
*!*************************************************************
*! Passed Parameters  :  lcAccount  : Account
*!                       lcOrder    : Order#
*!                       lcStore    : Store
*!                       lcPikTkt   : PikTKt#
*!                       lnTotalChg : Invoice Total Charge
*!                       lcKeyOff   : Key Off Invoice
*!                       lcAppCrdt  : Applied Credit tmp file name 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =gpOpnCredits(lcAccount,lcOrder,lcStore,lcPikTkt,lnTotalChg,lcAppCrdt)
*!*************************************************************
FUNCTION gpOpnCredits
PARAMETERS lcAccount,lcOrder,lcStore,lcPikTkt,lnTotalChg,lcKeyOff,lcAppCrdt,lcCurrency
PRIVATE llCredit,lcCredits,lnApply,lnOpenMark,lnAppMark,llKeyOffIn,lcOldValue

lcOldValue = ''
llKeyOffIn = &lcKeyOff
lcCredits  = gfTempName()
=gfOpenFile(gcDataDir+'CREDIT',gcDataDir+'CREDIT','SH')
SELECT * FROM CREDIT WHERE Account+Tran+DTOS(TranDate) = lcAccount AND ;
cCurrCode = lcCurrency INTO DBF (gcWorkDir+lcCredits)
IF _TALLY > 0
  INDEX ON Tran+DTOS(trandate) TAG (lcCredits)
  STORE 0 TO lnOpenMark,lnAppMark
  SCAN
    SELECT (lcAppCrdt)
    =SEEK(lcAccount)
    SUM REST Amount TO lnApply WHILE ;
    Account+cMOrder+cStore+PikTkt = lcAccount FOR Tran =&lcCredits..Tran
    SELECT (lcCredits)
    REPLACE Amount WITH Amount - lnApply
  ENDSCAN
  GO TOP
  SELECT (lcAppCrdt)
  =SEEK(lcAccount+lcOrder+lcStore+lcPikTkt)
  SUM REST Amount TO lnAppCrdt WHILE Account+cMOrder+cStore+PikTkt=;
                     lcAccount+lcOrder+lcStore+lcPikTkt
  =SEEK(lcAccount+lcOrder+lcStore+lcPikTkt)
  DO (gcScrDir+gcWinAppl+"\ARAPCRD.SPX")
  &lcKeyOff = llKeyOffIn
ENDIF
=gfCloseFile('CREDIT')
USE IN (lcCredits)
ERASE (gcWorkDir+lcCredits+".DBF")

*!*************************************************************
*! Name      : lfCrdtBrow
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Browse customer open and applied credits
*!*************************************************************
*! Calls     : lfShowOpen,lfShowApp
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCrdtBrow()
*!*************************************************************
FUNCTION lfCrdtBrow

SELECT (lcCredits)
lnOpenMark = RECNO()
BROWSE FIELDS cMarker=IIF(RECNO()=lnOpenMark ,'>',' '):H=' ':R:1:W=.F.,;
              Tran   :H='Credit'  :R,;
              Batch  :H='Batch'   :R,;
              TranDate :H='Date'  :R,;
              Desc    :H='Description' :R,;
              Reference :H='Reference' :R :30,;
              Store  :H='Chk/Str#.' :R,;
              Amount :H='Amount' :P='99999999999.99' :R;
       WINDOW ARAPCRD1   ;
       IN WINDOW ARAPCRD ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShowOpen() ;
       TITLE lcBrowCrd
SELECT (lcAppCrdt)
lnAppMark = RECNO()
BROWSE FIELDS cMarker=IIF(RECNO()=lnAppMark ,'>',' '):H=' ':R:1:W=.F.,;
              Tran     :H='Credit' :R,;
              Batch    :H='Batch'  :R,;
              TranDate :H='Date'   :R,;
              Desc     :H='Description' :R,;
              Reference :H='Reference' :R :30,;
              Store    :H='Chk/Str#.' :R,;
              Amount   :H='Amount' :P='99999999999.99' :W=lfwAppAmnt() :V=lfvAppAmnt();
       WINDOW ARAPCRD3   ;
       IN WINDOW ARAPCRD ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       KEY lcAccount+lcOrder+lcStore+lcPikTkt ;
       WHEN lfShowApp()  ;
       TITLE lcBrowApp

*!*************************************************************
*! Name      : lfDApCrdt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Deactivate customer open and applied credits screen
*!*************************************************************
*! Calls     : lfShowOpen,lfShowApp
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDApCrdt()
*!*************************************************************
FUNCTION lfDApCrdt
IF WONTOP() = lcBrowCrd .OR. WONTOP() = lcBrowApp
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTab WITH ;
  IIF(WONTOP()=lcBrowCrd,'ARAPCRD2','ARAPCRD4'),IIF(WONTOP()=lcBrowCrd,'pbNewCrdt','pbOkOpnCrd')
  ON KEY LABEL BACKTAB DO lpBackTab WITH ;
  IIF(WONTOP()=lcBrowCrd,'ARAPCRD4','ARAPCRD2'),IIF(WONTOP()=lcBrowCrd,'pbOkOpnCrd','pbNewCrdt')
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfShowOpen
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Show customer open credits
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowOpen()
*!*************************************************************
FUNCTION lfShowOpen
lnOpenMark = RECNO(lcCredits)
SHOW WINDOW (lcBrowCrd) REFRESH SAME

*!*************************************************************
*! Name      : lfShowApp
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Show applied credits
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowApp()
*!*************************************************************
FUNCTION lfShowApp
lnAppMark = RECNO(lcAppCrdt)
SHOW WINDOW (lcBrowApp) REFRESH SAME

*!*************************************************************
*! Name      : lfwAppAmnt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Applied credit amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwAppAmnt()
*!*************************************************************
FUNCTION lfwAppAmnt
lcOldValue = &lcAppCrdt..Amount

*!*************************************************************
*! Name      : lfvAppAmnt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Applied credit amount
*!*************************************************************
*! Calls     : lfShowOpen,lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAppAmnt()
*!*************************************************************
FUNCTION lfvAppAmnt

=RLOCK()
REPLACE Amount WITH -ABS(Amount)
IF &lcAppCrdt..Amount <> lcOLdValue .AND. ;
   SEEK(&lcAppCrdt..Tran+DTOS(&lcAppCrdt..trandate),lcCredits)
  IF (ABS(&lcCredits..Amount+lcOldValue) >= ABS(&lcAppCrdt..Amount)) .AND. ;
      ABS(lnAppCrdt-lcOldValue+&lcAppCrdt..Amount) <=  ABS(lnTotalChg)
    lnAppCrdt=lnAppCrdt-lcOldValue+&lcAppCrdt..Amount
    SELECT (lcCredits)
    REPLACE Amount WITH Amount + lcOldValue - &lcAppCrdt..Amount
    IF lnAppCrdt=0
      llKeyOffIn = .F.
      SHOW GET llKeyOffIn DISABLE
    ELSE
      SHOW GET llKeyOffIn ENABLE
    ENDIF  
    =lfShowOpen()
    =lfRefresh('ARAPCRD2')
    SELECT (lcAppCrdt)
    IF Amount = 0
      DELETE
      =SEEK(lcAccount+lcOrder+lcStore+lcPikTkt)
      =lfShowApp()
    ENDIF
  ELSE
    REPLACE Amount WITH lcOldValue
  ENDIF  
ENDIF
UNLOCK

*!*************************************************************
*! Name      : lfvNewCrdt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Apply credits to COD Invoice
*!*************************************************************
*! Calls     : lfRefresh,lfShowApp,lfShowOpen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNewCrdt()
*!*************************************************************
FUNCTION lfvNewCrdt
PRIVATE lnAmount

lnAmount = -MIN(ABS(&lcCredits..Amount),lnTotalChg-ABS(lnAppCrdt))
IF ABS(lnAmount) > 0
  IF !SEEK(lcAccount+lcOrder+lcStore+lcPikTkt+&lcCredits..Tran,lcAppCrdt)
    SELECT (lcCredits)
    SCATTER MEMVAR
    m.nTrnNewAmn = m.Amount - lnAmount
    m.cShToOpn   = IIF(m.nTrnNewAmn<0,'N','Y')
    m.Amount   = lnAmount
    m.cMOrder  = lcOrder
    m.PikTkt   = lcPikTkt
    m.cStore   = lcStore
    INSERT INTO (lcAppCrdt) FROM MEMVAR
  ELSE
    SELECT (lcAppCrdt)
    REPLACE Amount WITH Amount + lnAmount
  ENDIF
  lnAppCrdt=lnAppCrdt+lnAmount
  IF lnAppCrdt=0
    llKeyOffIn = .F.
    SHOW GET llKeyOffIn DISABLE
  ELSE
    SHOW GET llKeyOffIn ENABLE
  ENDIF
  =lfRefresh('ARAPCRD2')
  =lfShowApp()
  SELECT (lcCredits)
  REPLACE Amount WITH Amount - lnAmount
  =lfShowOpen()
ENDIF

*!*************************************************************
*! Name      : lfvRemCrdt
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : UnApply credits to COD Invoice
*!*************************************************************
*! Calls     : lfRefresh,lfShowApp,lfShowOpen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemCrdt()
*!*************************************************************
FUNCTION lfvRemCrdt
IF SEEK(&lcAppCrdt..Tran+DTOS(&lcAppCrdt..trandate),lcCredits)
  lnAppCrdt=lnAppCrdt-&lcAppCrdt..Amount
  SELECT (lcCredits)
  REPLACE Amount WITH Amount + &lcAppCrdt..Amount
  =lfShowOpen()
  SELECT (lcAppCrdt)
  DELETE
  =SEEK(lcAccount+lcOrder+lcStore+lcPikTkt)
  IF lnAppCrdt=0
    llKeyOffIn = .F.
    SHOW GET llKeyOffIn DISABLE
  ELSE
    SHOW GET llKeyOffIn ENABLE
  ENDIF  
  =lfRefresh('ARAPCRD2')
  =lfShowApp()
ENDIF

*!*************************************************************
*! Name      : lfvKeyOff
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Key Off COD invoice with applied credits
*!*************************************************************
*! Calls     : lfKeyOff
*!*************************************************************
*! Passed Parameters  :  lcInvoice : Invoice Number
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvKeyOff()
*!*************************************************************
FUNCTION lfvKeyOff
PARAMETERS lcInvoice
PRIVATE lnAppCrdt,lnAlias

lnAlias = SELECT()
lnUnCmSeRc = RECNO('UNCMSESS')   && save the uncomplete session record number
=SEEK(lcInvoice,'MINVHDR')
SELECT (lcAppCrdt)
=SEEK(MInvHdr.Account+IIF(MInvHdr.DIRECT_INV,SPACE(6),MInvHdr.cMOrder)+;
      MInvHdr.Store+MInvHdr.PikTkt)
SUM REST Amount TO lnAppCrdt WHILE Account+cMOrder+cStore+PikTkt=;
MInvHdr.Account+IIF(MInvHdr.DIRECT_INV,SPACE(6),MInvHdr.cMOrder)+MInvHdr.Store+MInvHdr.PikTkt
IF ABS(lnAppCrdt) > 0
  SELECT DEBIT
  =SEEK(MInvHdr.Account+lcInvoice+SPACE(3)+DTOS(InvHdr.InvDate))
  SCATTER MEMVAR
  m.nTrnNewAmn = m.Amount - ABS(lnAppCrdt)   
  m.cShToOpn   = IIF(m.nTrnNewAmn>0,'N','Y')
  m.cStore     = MInvHdr.Store
  INSERT INTO (lcAppCrdt) FROM MEMVAR

  DO lfKeyOff IN (gcapphome+'ARKEYOF.PRG') WITH ;
  MInvHdr.Account,MInvHdr.InvDate,Debit.Amount,lnAppCrdt,lcAppCrdt
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : gfChkSavInv
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Validate Invoice before save
*!*************************************************************
*! Calls     : gfModalGen,
*!*************************************************************
*! Passed Parameters  :  lcAccount  : Account
*!                       lcOrder    : Order #
*!                       lcStore    : Store
*!                       lcPikTKt   : Piktkt #
*!                       lcInvFile  : Invoice Header temp. file
*!                       lcInstFile : Installment header temp. file
*!                       lcCrdtFile : Applied credits temp file
*!                       lcUpsFile  : UPS Box temp file name
*!                       lcSaveInv  : Return value
*!*************************************************************
*! Returns            :  Returns .F. when :
*!                       - Invoice with zero amount
*!                       - Invoice installment amount exceeds total charge
*!                       - Installment Start date preior to invoice date
*!                       - Credits applied to COD invoice exceeds total charge
*!                       - Invoice Amount Exceeds Order Approved Amount
*!*************************************************************
*! Example            :  =gfChkSavInv()
*!*************************************************************
FUNCTION gfChkSavInv
PARAMETERS lcAccount,lcOrder,lcStore,lcPikTKt,lcInvFile,lcInstFile,;
           lcCrdtFile,lcUpsFile,lcSaveInv
PRIVATE lnInstAmnt,lnAppCrdt,lnAlias

lnAlias = SELECT()   
&lcSaveInv = .T.     && if all the next checks is valid Save the invoice    
IF Account <> lcAccount OR cMOrder <> lcOrder OR Store <> lcStore OR pikTkt <> lcPikTKt
  =SEEK(lcAccount+lcOrder+lcStore+lcPikTKt,lcInvFile)
ENDIF

IF &lcInvFile..FabShip = 0
  *-- Message : 40033
  *-- No invoice lines found! Cannot save.
  *-- Button : 00000
  *-- Ok
  =gfModalGen('INM40033B00000','ALERT')
  &lcSaveInv = .F.
ENDIF
IF &lcSaveInv AND !EMPTY(lcInstFile) AND SEEK(lcOrder+lcStore+lcPikTKt,lcInstFile)
  IF &lcSaveInv .AND. &lcInstFile..nInstmAmnt > &lcInvFile..TotalChg
    *-- Message : 40123
    *-- Installments amount cannot be greater than invoice total charge!
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM40123B00000','ALERT')
    &lcSaveInv = .F.
  ENDIF
  IF &lcSaveInv .AND. &lcInstFile..dInstmStDt < &lcInvFile..InvDate
    *-- Message : 40122
    *-- Installments start date cannot be less than invoice date!
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM40122B00000','ALERT')
    &lcSaveInv = .F.
  ENDIF
ENDIF
IF &lcSaveInv AND &lcInvFile..Cod_Flag='Y' 
  =gfOpenFile(gcDataDir+'CREDIT', gcDataDir+'CREDIT','SH')
  =SEEK(&lcInvFile..Account)
  *-- Search in the credit file for the account with the same currency 
  LOCATE REST WHILE Account+Tran+DTOS(TranDate) = &lcInvFile..Account FOR ;
  cCurrCode = &lcInvFile..cCurrCode
  IF FOUND()
    IF EMPTY(lcCrdtFile) OR !SEEK(lcAccount+lcOrder+lcStore+lcPikTKt,lcCrdtFile)
    *-- lines for that account in the temp credit file
      *-- Message : 40127
      *-- Invoice has COD amount while account xxxxx has open credits.
      *-- Are you sure you do not want to apply open credits?
      *-- Button : 40003
      *-- Proceed Cancel
      IF gfModalGen('QRM40127B40003','ALERT',lcAccount)=2  && if cancel don't save the invoice
        &lcSaveInv = .F.
      ENDIF
    ELSE
    *-- there is already applied credits
      SELECT (lcCrdtFile)
      SUM REST Amount TO lnAppCrdt WHILE Account+cMOrder+Store+PikTkt=;
                        lcAccount+lcOrder+lcStore+lcPikTKt
      IF &lcSaveInv AND lnAppCrdt > &lcInvFile..TotalChg
      *-- Applied credits greater than total charges 
        *-- Message : 40124
        *-- Applied credits amount cannot be greater than invoice total charge!
        *-- Button : 00000
        *-- Ok
        =gfModalGen('TRM40124B00000','ALERT')
        &lcSaveInv = .F.
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF &lcSaveInv AND UPPER(ALLTRIM(gcContCode))<>'ENG' .AND. &lcInvFile..lCompUps
*--Calculate non english charges 
  SELECT (lcInvFile)
  SCATTER MEMVAR 
  *-- Calculate charges
  DO gpUpsChrg WITH ;
  m.Account,m.Store,m.cMOrder,m.PikTkt,m.cWareCode,m.FabShipAmt,m.Cod_Flag='Y',;
  m.lUpsIns,m.DISCPCNT,m.TAX_RATE,m.cTaxRule,m.nPstRate,lcUpsFile,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT',;
  'm.TAX_AMT','m.nPstAmt',m.nTaxDue,'A'
  m.nHstAmt = ROUND((m.nHstRate*m.FabShipAmt)/100,2)
  m.TotalChg = m.FabShipAmt+m.Freight+m.Insur+m.Cod-ABS(m.FabShipAmt*m.DiscPcnt/100)+;
               m.Tax_Amt+m.nPstAmt+m.nHstAmt

  SELECT (lcInvFile)
  m.lCompUps = .F.
  =RLOCK()
  GATHER MEMVAR
  UNLOCK
ENDIF

IF &lcSaveInv AND SEEK('O'+cMOrder,'MaSoHdr') .AND. MaSoHdr.ApprAmt > 0 .AND. ;
   &lcInvFile..TotalChg > MaSoHdr.ApprAmt
  *-- Message : 40119
  *-- Invoice Amount Exceeds Order Approved Amount By 999999.
  *-- Button : 40003
  *-- Proceed Cancel
  IF gfModalGen('TRM40119B40003','ALERT',ALLTRIM(STR(&lcInvFile..TotalChg- MaSoHdr.ApprAmt,9,2)))=2
    &lcSaveInv = .F.
  ENDIF
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : gfAcOpCrdt
*! Developer : WAM
*! Date      : 02/19/1999
*! Purpose   : Check if customer has open credits
*!*************************************************************
*! Calls     : gpOpnCredits
*!*************************************************************
*! Passed Parameters  :  lcAccount : Invoice Account
*!                       lcCurrency: Invoice Currency
*!                       llAcOpCrdt: Open Credit found
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =gfAcOpCrdt()
*!*************************************************************
FUNCTION gfAcOpCrdt
PARAMETERS lcAccount,lcCurrency,llAcOpCrdt
PRIVATE lnAlias
lnAlias = SELECT()

&llAcOpCrdt = .F.
=gfOpenFile(gcDataDir+'CREDIT', gcDataDir+'CREDIT','SH')
SELECT CREDIT
=SEEK(lcAccount)
LOCATE REST WHILE Account+Tran+DTOS(TranDate)=lcAccount FOR cCurrCode=lcCurrency
&llAcOpCrdt = FOUND()
=gfCloseFile('CREDIT')
SELECT (lnAlias)
RETURN

*!**************************************************************************
*! Name      : lfHgWUpdat
*! Developer : Sameh (SSE)
*! Date      : 06/09/1999
*! Purpose   : Update nHgWtrMark field (Customer) with NETBAL, if NETBAL is greater
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfHgWUpdat()
*!******l******************************************************************
*
FUNCTION lfHgWUpdat
=RLOCK()
REPLACE nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
UNLOCK
*-- End of lfHgWUpdat.

*!**************************************************************************
*! Name      : lfUpCdFild
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/29/1999
*! Purpose   : Update TotCod Field avoiding Alias not found bug.
*!*************************************************************************
*! Example   : =lfUpCdFild()
*!************************************************************************
*
FUNCTION lfUpCdFild
PRIVATE lnCurrAlis
lnCurrAlis = SELECT(0)
SELECT (lcUpsFile)
REPLACE TotalCod WITH &lcCOD_AMT
SELECT (lnCurrAlis)
*-- end of lfUpCdFild.

*:*************************************************************
*: Name       : lfUpdTmpJr
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 06/25/2002
*: Purpose    : Create temp file lcTmpJour.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfUpdTmpJr()
*:*************************************************************
*B606163,1 [Begin]
FUNCTION lfUpdTmpJr
PRIVATE lnAlias , lcScnExp , lcOldOrder
lcScnExp = ''
lnAlias  = SELECT(0)

SELECT (lcTmpJour)
DELETE ALL

SELECT (lcTempOrdr)

*-- Seek for the current recored
lcScnExp = &lcDetFile..cMorder + &lcDetFile..Fabric + &lcDetFile..Color + &lcDetFile..cWareCode + ;
             &lcDetFile..dyelot

IF SEEK(lcScnExp)
  SCAN REST WHILE cMorder + cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession = lcScnExp ;
            FOR     lineno = &lcDetFile..lineno .AND. napply # 0
    SCATTER MEMVAR MEMO
    SELECT (lcTmpJour)
    APPEND BLANK
    GATHER MEMVAR MEMO
    SELECT (lcTempOrdr)
  ENDSCAN
ENDIF

*-- Get the Rolls For Saving.
IF Fabric.lTrkRolls .AND. USED(lcRollTmp)
  SELECT (lcTmpRoll)
  DELETE ALL  
  *-- Check in the temp detail file for the current record.
  IF SEEK(&lcDetFile..cMorder,lcRollTmp)
    SELECT (lcRollTmp)
    lcOldOrder = ORDER()
    SET ORDER TO lcRollTmp3
     = SEEK(lcScnExp)
    *-- Scan For Rolls.
    SCAN REST WHILE cMorder+cFabric+cColor+cWareCode+cDyelot+cRsession = lcScnExp ;
            FOR     lineno = &lcDetFile..lineno .AND. napply # 0
      SCATTER MEMVAR MEMO
      SELECT (lcTmpRoll)
      APPEND BLANK
      GATHER MEMVAR MEMO
      SELECT (lcRollTmp)
    ENDSCAN
    
    SET ORDER TO &lcOldOrder
  ENDIF

ENDIF

SELECT (lnAlias)
*-- End OF lfUpdTmpJr
*B606163,1 [Begin]
*:*************************************************************
