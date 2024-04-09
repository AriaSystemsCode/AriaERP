*!**************************************************************************
*! Name      : gpSaveInv
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Save new Invoice
*!**************************************************************************
*! Calls     : gfStyCrl(),
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
*!
*!E300834,1 WAM 08/12/1998 Update style dyelot stock for Inventory styles only
*!E300956,1 WAM 08/16/1998 Keep history of order line calceled quantity.
*!E301038,1 TAK 10/12/1998 Change the sign of the Invoice
*!E301038,1                record in the STYINVJL file to be -ve.
*!B602270,1 WAM 11/26/1998 Do not Style inventory jornal with zero shipped
*!B602278,1 WAM 11/26/1998 Elimenate price roundation
*!B602295,1 WAM 11/30/1998 Call KEYOFF procedure from the root directory
*!B602322,1 TAK 12/03/1998 Pass GL Session sequence as a parameter to 
*!B602322,1                the GFSTYCRL function to make sure that the record
*!B602322,1                in journal with the same session no.
*!E301086,1 WAM 12/09/1998 Mark generated invoice to be send
*!B602446,1 WAM 01/11/1999 Elimenate Ship amount roundation
*!B602544,1 WAM 02/18/1999 Add new invoice lines to order lines
*!E301077,50 WAM 02/18/1999 Inhance openning files to speed up transaction
*E300883,1 WAM 02/22/1999 Add GL Account as related field to Non-merch. chages
*B801882,1 WAM 03/31/1999 Recompute USA charges when needed.
*B602775,1 MAN 04/11/1999 Wrong AR GL Account saved in the Debit file if the
*B602775,1 MAN            AR Value is 0, Wrong Fright GL Account saved in the 
*B602775,1 MAN            Invoice Header if the Frieght Value is 0, Wrong tax  
*B602775,1 MAN            GL Account saved in the Invoice Header if the tax is 0
*B602767,1 MAN 04/13/99 Fix Alias Not found 
*E301200,1 AHM 04/13/1999 Add invoice line No in calling gfStyCrl function
*E301200,1                to update the added field (lineNo) in StyInvJl file
*E301192,1 AHM 04/19/1999 Update reference field in styinvjl with descriptive text
*B602850,1 RENEE 04/29/99 Fix 'Operator/Operand tyoe mismatch' in weight 
*B602850,1 				  validation.
*B802231,1 WAM 05/06/1999 Consider order type when enter order# manualy
*B602891,1 AMM 05/11/1999 Fix the bug of 'Datatype mismatch'
*B602958,1 AMM 05/30/1999 Accept zero amount invoices
*B602971   AHM 06/08/1999 wrong update for lineno field in styinvjl file
*E301245,1 SSE 06/09/1999 Added a new field nHgWtrMark and calcualte its value
*                         from NETBAL field , if less than NETBAL update with 
*                         NETBAL else leave it with existing value
*B802336,1 WAM 06/10/1999 Fix updating stock and inventory journal for consolidated invoice
*B603022,1 MAN 07/20/1999 Replace PikTkt Status with C instead of X
*B802456,1 MAB 07/29/1999 Fix Alias not Found Bug.
*B603089,1 AHM 08/03/1999 Add the fractions difference to the last installement
*B602937,1 AHM 08/05/1999 Update Audit fields in GLDist File
*B602877,1 AHM 08/11/1999 Don't update cDicAcnt field in InvLine file if there
*B602877,1                no discount
*B802603,1 AHM 09/23/1999 Fix the bug of not updating Field InvHdr.CustPO in case 
*B802603,1                Single CustPO Sales order
*B603257,1 ABD 11/10/1999 Fix the bug of miss Calculation in the Ordhdr.Appramt. 
*B802778,1 WAM 11/09/1999 Ship BOL generated for invoiced packing list
*B802837,1 SSH 11/29/1999 Get the no of carton from back_hdr incase 
*B802837,1 SSH            packed pikTkt.
*B603318,1 TAK 12/04/1999 Added custom process to update Pikline with the 
*B603318,1                adornment informaion and clear the link with Cutpick.
*B802865,1 WAB 12/20/1999 Fix the bug of get an infinite loop at saving the invoice
*B802865,1                in case of the Ship To address is a "distribution center"
*B802853,1 NAD 03/28/2000 Round tax amount
*B603119,1 NAD 03/28/2000 Fix invoice number is empty in the uncomplete session
*B603668,1 NAD 05/30/2000 Problem with the stock update when using dyelots.
*B803174,1 NAD 06/06/2000 Wrong update for the order open quantities in case of backorder. 
*E301429,1 NAD 06/25/2000 Adjust the amount field width in the browse of the open credits screen
*B803397,1 NAD 07/13/2000 Fix bug Edit Installments error Alias not found.             
*B603751,1 NAD 07/18/2000 Fix bug ORDCANLN not found in case sales order module was not instaled.
*B603728,1 NAD 08/08/2000 Get the style cost from style file if cost method is standard.
*E500374,1 NAD 08/15/2000 Post the AR Account of the factor instead of the customer if there
*E500374,1                is a factor code in the transaction.  
*B603923,1 NAD 09/26/2000 1-Direct invoice does not save the Pack_Id in the order line.
*B603923,1                2-Direct invoice does not update upsbox file.  
*B803571,1 NAD 10/25/2000 Not to allow users to add negative values in the charges fields.
*B603989,1 NAD 01/29/2000 On Currency sales invoices there are small difference between Catg_Key='003' AND  Catg_Key='001'   
*E301509,1 WAM 11/16/2000 Add new parameter setup with .T. if called from EDI
*E301509,1                Do not send Received invocies
*B604256,1 ADEL 03/08/2001 Zero out the packed Qty as it belonged to a relaesed PK and
*B604256,1                 causes problms when creating packing list for a new PK on the same order.
*C102212,1 ADEL 03/14/2001 Update HST Tax to sales tax account.1
*C102239,1 ADEL 04/01/2001 Add custom Carton information screen for BEL05.
*C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.
*B604451,1 NAD 06/19/2001 Fix Bug dublicte invoce number
*C102321,1 NAD 07/01/2001 Pre-Billing selection screen.
*C102318,1 ADEL 07/30/2001 Manipulate Over allocated orders for SAD10.
*B804358,1 NAD  08/16/2001 Wrong UPS insurance in the  charges folder.
*B604891,1 SSH Fix bug of not saving invoice in case of calling from PS
*B604932,1 ADEL 09/16/01 Generate 2 invoices for orders based on 2 divisions for a cosolidiate account.
*B604955,1 NAD  09/24/01 BOL DATE CHANGES WHEN INVOICING         
*C102424,1 ADEL 10/15/01 Add custom Carton information screen for OGIO.
*B603998,4 SSE 11/21/2001 Fix bug of not inserting line if Sales Rep commission is zero.
*B604607,1 SSE 11/29/2001 Fix bug of not updating the price in Order Cancellation file.
*B604339,1 SSE 12/06/2001 Fix bug of wrong updating the allocation Qtys in Style file.
*B605717,1 SSE 07/16/2002 Fix bug of calculating Order approval amount in a wrong way.
*B606035,1 SSE 07/17/2002 Fix bug of not calculating the freight increase from Customer screen.
*C102676,1 SSE 09/03/2002 Custom modifications add 3rd Rep and comm for OrdHdr file.
*B606165,1 SSE 09/03/2002 Fix bug of updating Repcomm with CommAmt1 for 2nd Sales rep.
*B606343,1 SSE 10/09/2002 Fix bug of not updating StyDye right in case of using dyelot.
*B605832,1 SSE 10/10/2002 Fix bug of closing Pick ticket for unshipped line.
*B606392,1 SSE 10/20/2002 Fix bug of not locking the order which is selected for invoice.
*B606075,1 SSE 10/24/2002 Fix bug of too many files open.
*C102761,1 SSE 11/28/2002 Add custom triggers for Pre-Billed Invoices for Rena Lange.
*!****************************************************************************************

FUNCTION gpSaveInv
*E301509,1 Add new parameter setup with .T. if called from EDI
*PARAMETERS lcHdrFile,lcDetFile,lcUpsFile,lcEngChrg,lcInstHdr,;
           lcInstLine,lcOrdCanLn,lcGlSession,laInvoices
PARAMETERS lcHdrFile,lcDetFile,lcUpsFile,lcEngChrg,lcInstHdr,;
           lcInstLine,lcOrdCanLn,lcGlSession,laInvoices,llFromEdi
*E301509,1 (End)

PRIVATE laInvSetup,laDRltFld,lcCustLink,lcCustSales,lcUntSin,lnInvCount,;
        lcExRSin,lcGlYear,lcGlPeriod,lnNetAmnt,lnComm1,lnComm2,lnNetShipAmnt,;
        lnCOGSAmt,lnHdrComm1,lnHdrComm2,lnAlias,lcDistFile,;
        lcInvNo,lcOrdNo,llOrdExist,laGlArray,laAdjStk

lcSysType = gfGetMemVar('M_SYSTYPE',gcAct_Comp)

DECLARE laGlArray[2,13]

lnAlias = SELECT()
DECLARE laInvSetup[10,2]
laInvSetup[1,1] = 'M_LINK_GL'          &&  Check for Gl link
laInvSetup[2,1] = 'M_WareHouse'        &&  use maltiple locations Y Or N  Ic Setup
laInvSetup[3,1] = 'M_COST_METH'        &&  Get the style cost method          
laInvSetup[4,1] = 'M_DYELOT'           &&  Use Dylot Y Or N      
laInvSetup[5,1] = 'M_DIV_LINK'         &&  GL link codes at Division level
laInvSetup[6,1] = 'M_STY_COM'          &&  Commision at Style level  
laInvSetup[7,1] = 'M_TAX'              &&  use Taxes Y or N
laInvSetup[8,1] = 'XPOSTFINV'          &&  Post Factored invoice to customer
laInvSetup[9,1] = 'XAGINGTYPE'         &&  Aging AR by Date\Terms
laInvSetup[10,1]= 'M_GenOrNum'         &&  Enter order # Manually Y Or N
=gfGetMemVar(@laInvSetup,gcAct_Comp)
IF laInvSetup[5,2]='Y'
  DECLARE laDRltFld[2,2]               && array to hold the division related fields
  laDRltFld[1,1] = 'LINK_CODE'           
  laDRltFld[1,2] = 'lcCustLink'        && Customer link   
  laDRltFld[2,1] = 'CSLSGLLINK'        
  laDRltFld[2,2] = 'lcCustSales'       && Custmer sales link code 
ENDIF

*E301077,50 Inhance openning files to speed up transaction
=IIF('AL' $ gcCmpModules,gfOpenFile(gcDataDir+'PIKLINE',gcDataDir+'PIKLINE','SH'),.T.)
=IIF(laInvSetup[1,2]='Y',gfOpenFile(gcDataDir+'GLDIST', gcDataDir+'GLDISTNO', 'SH'),.T.)
=gfOpenFile(gcDataDir+'icStyHst', gcDataDir+'Styhst','SH')
=gfOpenFile(gcDataDir+'arCusHst', gcDataDir+'Acthst','SH')
=gfOpenFile(gcDataDir+'DEBIT', gcDataDir+'DEBIT','SH')
=gfOpenFile(gcDataDir+'CONSINVH', gcDataDir+'CONSINVH','SH')
=gfOpenFile(gcDataDir+'CONSINVL', gcDataDir+'CONSINVL','SH')
=gfOpenFile(gcDataDir+'REPCOMM', gcDataDir+'REPCOMM','SH')
=gfOpenFile(gcDataDir+'SALESREP', gcDataDir+'SALESREP','SH')
*B603751,1 (Start) Open the ORDCANLN file if the order cancelation temp file
*B603751,1         was passed as a parameter to the saving procedure.
IF !EMPTY(lcOrdCanLn)
*B603751,1 (End)
  =gfOpenFile(gcDataDir+'ORDCANLN', gcDataDir+'ORDCANLN','SH')
*B603751,1 (Start) Add the ENDIF
ENDIF
*B603751,1 (End) 
=gfOpenFile(gcDataDir+'UPSBOX', gcDataDir+'UPSBOX','SH')
*B602767,1 MAN 04/13/99 Fix Alias Not found 
=gfOpenFile(gcDataDir+'GL_LINK', gcDataDir+'GL_LINK','SH')
*B602767,1 End
*E301077,50 (End)

*E301086,1 Mark generated invoice to be send
*-- flag to Check if the edi files was open or not
llEdiAcPrt = ('EB' $ gcCmpModules OR 'NC' $ gcCmpModules ) .AND. gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
llEdiPD    = ('EB' $ gcCmpModules OR 'NC' $ gcCmpModules ) .AND. gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
llEdiTrans = ('EB' $ gcCmpModules OR 'NC' $ gcCmpModules ) .AND. gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

*B802778,1 Ship BOL generated for invoiced packing list
llPackHdr = 'EB' $ gcCmpModules .AND. gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH')
llBolHdr  = 'EB' $ gcCmpModules .AND. gfOpenFile(gcDataDir+'BOL_HDR',gcDataDir+'BOL_HDR','SH')
*B802778,1 (End)

*E301086,1 (End)
*-- Temp name for the pick line temp file
lcTmpPikLn = gfTempName()
SET ORDER TO TAG ORDLINE IN ORDLINE
SET ORDER TO TAG DEBIT IN DEBIT
lnInvCount = 1    && Variable to hold the number of invoices       
SELECT (lcHdrFile)
SCAN

  *B603119,1 (Start)  dimension the array in the begaining of the scan
  DIMENSION &laInvoices[lnInvCount] 
  *B603119,1 (End)

  *-- Get The order number manualy or using gfsequence depend on the invoice setup
  lcOrdNo = IIF(EMPTY(&lcHdrFile..Order) .AND. DIRECT_INV,;
            IIF(laInvSetup[10,2]='Y',lfGtOrder(),;
            gfSequence('ORDER','','',&lcHdrFile..cDivision)),&lcHdrFile..Order)

   
  IF EMPTY(&lcHdrFile..Invoice)
  *-- Get The Invoice number on the factor sequence if the invoice was factored 
    *C102321,1 NAD 07/01/2001 (Start) give the invoice the same invoice no. for the pre-Billed one.
    IF ASCAN(laEvntTrig , PADR('SADSEQ',10)) <> 0 
       lcInvNo=""
      =gfDoTriger('SDIINV',PADR('SADSEQ',10))    
    ELSE
    *C102321,1 NAD 07/01/2001 (End)
    
      *C102761,1 Add Rena Lange Trigger. [Begin]
      IF ASCAN(laEvntTrig , PADR('RENSEQ',10)) <> 0 
         lcInvNo=""
        =gfDoTriger('REIINV',PADR('RENSEQ',10))    
      ELSE      
      *C102761,1 Add Rena Lange Trigger. [End]
       
        lcInvNo=IIF(EMPTY(&lcHdrFile..cFacCode),;
                gfSequence('INVOICE','','',&lcHdrFile..cDivision),;
                gfSequence('CFINVOICE' ,'','',&lcHdrFile..cDivision))
   
    
        *B604451,1 NAD 06,19,2001 (Start) Invoice number got created for 2 different orders      
        lcSAlias=ALIAS()
        lnSRecno=RECNO('INVHDR')
        DO WHILE SEEK(lcInvNo,'InvHdr')
          lcInvNo=IIF(EMPTY(&lcHdrFile..cFacCode),;
               gfSequence('INVOICE','','',&lcHdrFile..cDivision),;
               gfSequence('CFINVOICE' ,'','',&lcHdrFile..cDivision))             
 
        ENDDO 
        SELECT InvHdr
        APPEND BLANK
        =RLOCK()
        REPLACE Invoice with lcInvNo
        UNLOCK
        IF RECCOUNT('InvHdr') >= lnSRecno
           GO lnSRecno IN InvHdr
        ENDIF
        SELECT (lcSAlias)
        *B604451,1 NAD 06,19,2001 (End) Invoice number got created for 2 different orders  
        
      *C102761,1 Add Rena Lange Trigger. [Begin]
      ENDIF 
      *C102761,1 Add Rena Lange Trigger. [End]

    *C102321,1 NAD 07/01/2001 (Start) Added the end if
    ENDIF
    *C102321,1 NAD 07/01/2001 (End)
    
    *B603119,1 Commented out
    *DIMENSION &laInvoices[lnInvCount] 
    *B603119,1 (End)
    
    &laInvoices[lnInvCount] = lcInvNo
    
    lnInvCount = lnInvCount + 1
    
  ELSE
    *B604932,1 (Begin) As the file not indexted on invoice, it goes down and up.
    *B604932,1         So don't get an previously updated invoice no.
    *lcInvNo=&lcHdrFile..Invoice
    *MAN
    *lcInvNo =IIF(&lcHdrFile..Invoice < lcInvNo,lcInvNo,&lcHdrFile..Invoice)
    IF TYPE("lcInvNo") # "C"
      lcInvNo=&lcHdrFile..Invoice
    ELSE
      lcInvNo =IIF(&lcHdrFile..Invoice < lcInvNo,lcInvNo,&lcHdrFile..Invoice)
    ENDIF  
    *MAN
    *B604932,1 (End)

    *B603119,1 Store invoice number
    &laInvoices[lnInvCount] = lcInvNo
    *B603119,1 (End)
  ENDIF
  
  SELECT (lcHdrFile)
  
  *-- If the invoice was consolidated invoice give the lines the same invoice number
  IF Consol='Y'
    *B604932,1 (Begin) Replace the invoice for its division records only
    *lcAccount = Account
    *REPLACE REST Invoice WITH lcInvNo WHILE Account+Order+Store+PikTkt=lcAccount
    *REPLACE REST Invoice WITH lcInvNo WHILE Account+Order+Store+PikTkt=lcAccount
    *=SEEK(lcAccount)
    lcAccount = Account
    lcKeyVal  = EVAL(KEY())
    lcDiv     = cDivision
    REPLACE REST Invoice WITH lcInvNo WHILE Account+Order+Store+PikTkt=lcAccount;
            FOR  cDivision = lcDiv        
    =SEEK(lcKeyVal)
    LOCATE REST WHILE EVAL(KEY()) = lcKeyVal FOR cDivision = lcDiv
    *B604932,1 (End)
  ENDIF  
  =RLOCK()
  REPLACE nExRate   WITH IIF(nExRate=0,1,nExRate) ,;
          nCurrUnit WITH IIF(nCurrUnit=0,1,nCurrUnit) ,;
          LastLine  WITH IIF(SEEK('O'+lcOrdNo,'ORDLINE'),LastLine,0),;
          Invoice   WITH lcInvNo ,;
          Order     WITH lcOrdNo
  UNLOCK
  *B603923,1 NAD 09/26/2000 (Start) Update the order no. in the UPSBOX temp file.
  IF &lcHdrFile..Direct_inv AND USED (lcUpsFile)
    PRIVATE lcLastFile,lcUPSOrd
    lcUPSOrd   = ORDER(lcUpsFile)
    lcLastFile = ALIAS()
    SELECT (lcUpsFile)
    SET ORDER TO
    REPLACE ALL Order WITH lcOrdNo
    SET ORDER TO &lcUPSOrd
    SELECT  (lcLastFile)
  ENDIF
  *B603923,1 NAD (End)
  STORE '' TO lcGlYear,lcGlPeriod
  *--Check Post Date if valid  in the gl period or not
  =CHECKPRD(&lcHdrFile..dPostDate,'lcGlYear','lcGlPeriod','IN',.T.)
  
  IF laInvSetup[1,2]='Y'

    *B606075,1 Close Temp Dist File before opening a new one. [Begin]
    IF TYPE('lcDistFile') = 'C' AND USED(lcDistFile)
      USE IN (lcDistFile)
    ENDIF
    *B606075,1 Close Temp Dist File before opening a new one. [End]
  
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
    *E500374,1 (Start) If there is no link code for division use the customer one.
    *lcCustLink  = IIF(EMPTY(lcCustLink),'DEFDEF',PADR(lcCustLink,6))
    *lcCustSales = IIF(EMPTY(lcCustSales),'DEF',PADR(lcCustSales,3))
    lcCDefLnk=IIF (EMPTY(Customer.Link_Code),'DEFDEF',Customer.Link_Code)
    lcSDefLnk=IIF (EMPTY(Customer.cSlsGlLink),'DEF',Customer.cSlsGlLink)
    lcCustLink  = IIF(EMPTY(lcCustLink),lcCDefLnk,PADR(lcCustLink,6))
    lcCustSales = IIF(EMPTY(lcCustSales),lcSDefLnk,PADR(lcCustSales,3))
    *E500374,1 (End)
  ELSE   
    lcCustLink  = IIF(EMPTY(Customer.Link_Code),'DEFDEF',Customer.Link_Code)
    lcCustSales = IIF(EMPTY(Customer.cSlsGlLink),'DEF',Customer.cSlsGlLink)
  ENDIF
  
  *E500374,1 (Start) Open the factor file and go to the current factor record.
  IF !EMPTY(&lcHdrFile..cFacCode) 
    =gfOpenFile(gcDataDir+'Factor',gcDataDir+'Factor','SH')
    IF SEEK(&lcHdrFile..cFacCode,'Factor')      
      lcCustLink =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,lcCustLink)
    ENDIF  
  ENDIF   
  *E500374,1 (End)
  
  STORE '' TO laGlArray       && Arry to hold GL information needed by gfStyCntl function
  IF laInvSetup[1,2]='Y' .AND. llConsAcc
    STORE 'IN'                  TO laGlArray[1,4],laGlArray[2,4]
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
  llOrdExist = SEEK('O'+lcOrdNo,'ORDHDR') && In case of sales order invoice 
  *-- get the customer link code from the order header in case of sales order invoice
  lcCustLink = IIF(EMPTY(ORDHDR.Link_Code),lcCustLink,ORDHDR.Link_Code) 
  
  SELECT (lcDetFile)
  =SEEK(&lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..Order,SPACE(6))+;
            &lcHdrFile..Store+&lcHdrFile..PikTkt)
  WAIT 'Updating Invoice Lines...' WINDOW NOWAIT

  *B802778,1 Ship BOL generated for invoiced packing list
  *-- If the EDI module is installed and the invoice is a Direct invoice
  IF 'EB' $ gcCmpModules AND !&lcHdrFile..Direct_Inv AND ;
    SEEK('A'+&lcHdrFile..Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'856','EDIPD') AND ;
    SEEK(&lcHdrFile..Order+&lcHdrFile..Store+&lcHdrFile..PikTkt,'PACK_HDR') AND ;
    SEEK(Pack_Hdr.Bill_Ladg,'BOL_HDR')
    *B604955,1 NAD  09/24/01 (START) BOL DATE CHANGES WHEN INVOICING 
    *IF !BOL_HDR.lSent 
    IF BOL_HDR.Status <> "C"
    *B604955,1 NAD  (END)
      SELECT BOL_HDR
      REPLACE Status    WITH 'C' ,;
              Ship_Date WITH gdSysDate ,;
              cShipTime WITH TIME()
      SELECT EDITRANS
      IF !SEEK('856'+PADR(PACK_HDR.Bill_Ladg,20)+'A'+&lcHdrFile..Account)
        INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER) VALUES ;
        ('856',PACK_HDR.Bill_Ladg,'A',&lcHdrFile..Account)
      ENDIF
      REPLACE cStatus WITH 'N'
      =gfAdd_Info('EDITRANS')
    ENDIF
    SELECT PACK_HDR
    REPLACE Ship_Date WITH gdSysDate ,;
            Status    WITH 'C'
    SELECT (lcDetFile)
  ENDIF
  *B802778,1 (End)

  SCAN REST WHILE Account+Order+Store+PikTkt = ;
            &lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..Order,SPACE(6))+;
            &lcHdrFile..Store+&lcHdrFile..PikTkt

    *B604932,1 (Begin) Get the invoice records based on division.
    *C102424,1 (Begin) Fix it.
    *IF m.Consol='Y'
    IF &lchdrfile..consol ='Y'
    *C102424,1 (End)
      LOCATE REST WHILE Account+Order+Store+PikTkt = ;
            &lcHdrFile..Account+IIF(llOrdExist,&lcHdrFile..Order,SPACE(6))+;
            &lcHdrFile..Store+&lcHdrFile..PikTkt;
            FOR cDivision = &lcHdrFile..cDivision
    *B604932,1 (End)
    ENDIF
    *C102318,1 (Begin) 
    IF ASCAN(laEvntTrig , PADR('INVOVR',10)) <> 0 
      =gfDoTriger('ARIINV',PADR('INVOVR',10))    
      EXIT
    ENDIF
    *C102318,1 (End)

    
    SCATTER MEMVAR MEMO
    =SEEK(m.Style,'Style')
    =SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
    *-- the stock current value to use it in calculating the style cost 
    lnStkVal = Style.nStkVal    
    *-- Calculate the avarage cost of the style due to the cost method and warehouse   
    m.Cost   = IIF(laInvSetup[3,2]='A',;
                 IIF(laInvSetup[2,2]='Y',StyDye.Ave_Cost,Style.Ave_Cost),Style.TotCost)
    *B602278,1 Elimenate price roundation
    *B602446,1 Elimenate Ship amount roundation (Commented out)
    *m.Price = m.Gros_Price*(100-Disc_Pcnt)/100
    *B602446,1 (End)
    *B602278,1 (End)
    *-- Get the GL Cost ,GL Sales link code 
    IF laInvSetup[1,2]='Y' .AND. llConsAcc
      *E300817,1  Get Sales and Cost link codes
      =SEEK('O'+m.Order+STR(m.LineNo,6),'ORDLINE')
      m.Gl_Cost  = IIF(!EMPTY(STYLE.Link_Code),STYLE.Link_Code,'DEFDEF')
      m.Gl_Cost  = IIF(laInvSetup[2,2]='Y' .AND. !EMPTY(STYDYE.Gl_Link),;
                     STYDYE.Gl_Link,m.Gl_Cost)
      *-- Concatinate customer sales link code and style sales like code
      m.Gl_Sales = IIF(EMPTY(OrdLine.Gl_Sales),lcCustSales+Style.cSlsGlLink,OrdLine.Gl_Sales)

      *E300817,1  Sales Revenus <CREDIT> Category key '003'
      *-- Update the G/L Distribution file 
      DO GLDIST WITH m.Gl_Sales,'003',-(m.TotQty *m.Price),'IN',lcInvNo,;
                     &lcHdrFile..dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
                     &lcHdrFile..cCurrCode,&lcHdrFile..nCurrUnit,&lcHdrFile..nExRate
      m.cSalesAcnt = &lcDistFile..GlAccount   && Get the sales Revenus account 
 
      *E300817,1  Discount <DEBIT> Category key '005'  
      *B602958,1 AMM If shipment amount equal zero, don't make GL transaction
      IF &lcHdrFile..ShipAmt # 0
      *B602958,1 AMM end
      *-- Update the G/L Distribution file 
      DO GLDIST WITH m.Gl_Sales,'005',(m.TotQty * m.Price * ABS(&lcHdrFile..Discount)/&lcHdrFile..ShipAmt),;
                     'IN',lcInvNo,&lcHdrFile..dPostDate,lcGlYear,lcGlPeriod,;
                     lcDistFile,'',&lcHdrFile..cCurrCode,&lcHdrFile..nCurrUnit,&lcHdrFile..nExRate
      *B602958,1 AMM END IF
      ENDIF
      *B602958,1 AMM end
      
      *B602877 (Start) 
      *--  Don't update cDicAcnt field in InvLine file if there no discount 
      IF (m.TotQty * m.Price * ABS(&lcHdrFile..Discount)/&lcHdrFile..ShipAmt) > 0
      *B602877 (End)
        m.cDiscAcnt = &lcDistFile..GlAccount
      *B602877 (Start)
      ENDIF
      *B602877 (End)
    ENDIF  
    *E300817,1 Update style dyelot Shipped quantities
    
    *B606343,1 Update the dyelot line. [Begin]
    *IF &lcDetFile..nSteps < 1 AND laInvSetup[4,2]='Y' AND llConsAcc AND ;
    *    Style.cDye_Flg = 'Y' AND SEEK(m.Style+m.cWareCode+m.Dyelot,'StyDye')
    IF &lcDetFile..nSteps < 1 AND laInvSetup[4,2]='Y' AND !EMPTY(m.Dyelot) AND ;
        Style.cDye_Flg = 'Y' AND SEEK(m.Style+m.cWareCode+m.Dyelot,'StyDye')
    *B606343,1 Update the dyelot line. [End]

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
    *E300817,1  Update warehouse stock and shipped quantities
    *E300834,1 Update warehouse stock for Inventory styles only
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
    *E300817,1  Update Style stock and shipped quantities
    *E300834,1 Update style stock for Inventory styles only
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

    *E300817,1  Accumulate Salesreps Commission Amount.
    IF laInvSetup[6,2]='Y'
      lnNetAmnt = m.TotQty*m.Price*(1-&lcHdrFile..DiscPcnt/100)*(1-&lcHdrFile..Trde_Disc/100)
      lnComm1   = lnComm1 + (lnNetAmnt * m.Comm1 / 100)
      lnComm2   = lnComm2 + (lnNetAmnt * m.Comm2 / 100)
    ENDIF

    *E300817,1  Add new ivoice line
    m.Invoice  = &lcHdrFile..Invoice
    m.Order    = &lcHdrFile..Order
    *-- Calculate the equivlant amount by the company currency code
    m.nEqvAmnt = ROUND(m.Price*m.TotQty &lcExRSin &lcHdrFile..nExRate &lcUntSin &lcHdrFile..nCurrUnit,2)
    m.InvDate  = &lcHdrFile..InvDate
    
    
    IF Consol <> 'Y' AND &lcDetFile..nSteps < 12
      *B602544,1 Add new invoice lines to order lines
      *IF !SEEK('O'+m.Order+STR(m.LineNo,6),'ORDLINE')
      *-- if new order line or the line not found ad new line to the ORDLINE File 
      IF (!&lcHdrFile..Direct_Inv AND &lcDetFile..lNewLine) OR ; 
         !SEEK('O'+m.Order+STR(m.LineNo,6),'ORDLINE')
      *B602544,1 (End)
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
        *B603923,1 NAD 09/26/2000 (Start) Added the pack_id to the INSERT INTO statment.
        *INSERT INTO ORDLINE ;
        (cOrdType,Order,Account,LineNo,Style,Scale,Group,Gros_Price,Disc_Pcnt,;
        Price,Start,Complete,Comm1,Comm2,Note_Mem,Desc1,Book1,Book2,Book3,Book4,;
         Book5,Book6,Book7,Book8,TotBook,Season,PrePak,PPQty,cWareCode,Store) VALUES ;
        ('O',m.Order,m.Account,m.LineNo,m.Style,m.Scale,m.Group,m.Gros_Price,;
         m.Disc_Pcnt,m.Price,&lcHdrFile..InvDate,&lcHdrFile..InvDate,m.Comm1,;
         m.Comm2,m.Note_Mem,m.Desc1,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
         m.Qty7,m.Qty8,m.TotQty,m.Season,m.PrePak,m.PPQty,m.cWareCode,m.Store)
       
        INSERT INTO ORDLINE ;
        (cOrdType,Order,Account,LineNo,Style,Scale,Group,Gros_Price,Disc_Pcnt,;
        Price,Start,Complete,Comm1,Comm2,Note_Mem,Desc1,Book1,Book2,Book3,Book4,;
         Book5,Book6,Book7,Book8,TotBook,Season,PrePak,PPQty,cWareCode,Store,Pack_id) VALUES ;
        ('O',m.Order,m.Account,m.LineNo,m.Style,m.Scale,m.Group,m.Gros_Price,;
         m.Disc_Pcnt,m.Price,&lcHdrFile..InvDate,&lcHdrFile..InvDate,m.Comm1,;
         m.Comm2,m.Note_Mem,m.Desc1,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
         m.Qty7,m.Qty8,m.TotQty,m.Season,m.PrePak,m.PPQty,m.cWareCode,m.Store,m.Pack_Id)
        *B603923,1 NAD  (End)
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
            REPLACE nSteps WITH 7
            UNLOCK
          ENDIF
          IF OrdLine.Picked .AND. &lcDetFile..nSteps < 8
            *--decrease the Alocated quantitys in the stydye file
            *-- with the order line Picked Quantities
            
            
            SELECT StyDye
            *B603668,1 (Start) Seek to be sure that this is the corret record to update.
            =SEEK(IIF(!EMPTY(m.AltStyle),m.AltStyle,m.Style)+m.cWareCode+SPACE(10))
            *B603668,1 (End)
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
          REPLACE nSteps WITH 9
          UNLOCK
        ENDIF
        IF OrdLine.Picked .AND. &lcDetFile..nSteps < 10
          *--decrease the Alocated quantitys in the style file
          *-- with the order line Picked Quantities
          
          *B604339,1 Seek to get the Alternative style if exist. [Begin]
          =SEEK(IIF(!EMPTY(m.AltStyle),m.AltStyle,m.Style),'Style')
          *B604339,1 Seek to get the Alternative style if exist. [End]
          
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
          *B803174,1  (Start) In case of backorder decrease the open amount with the booked amount only. 
          *REPLACE Cancel    WITH Cancel+IIF(m.lBackOrd,0,MAX(OrdLine.TotQty-m.TotQty,0)),;
                  CancelAmt WITH CancelAmt + ;
                  IIF(m.lBackOrd,0,MAX(OrdLine.TotQty-m.TotQty,0))*OrdLine.Price ,;
                  Ship      WITH Ship + m.TotQty ,;
                  ShipAmt   WITH ShipAmt + m.TotQty*m.Price ,;
                  Open      WITH MAX(Open-IIF(m.lBackOrd,m.TotQty,OrdLine.TotQty),0),;
                  OpenAmt   WITH MAX(OpenAmt - ;
                  IIF(m.lBackOrd,m.TotQty,OrdLine.TotQty)*OrdLine.Price,0) ,;
                  Status    WITH IIF(Open > 0,Status,'C')
          lnOrdShip=0
          FOR lnX= 1 TO 8
            lcSize = STR(lnX,1)
            lnOrdShip= lnOrdShip + MIN(m.Qty&lcSize,OrdLine.Qty&lcSize)
          ENDFOR
          REPLACE Cancel    WITH Cancel+IIF(m.lBackOrd,0,MAX(OrdLine.TotQty-m.TotQty,0)),;
                  CancelAmt WITH CancelAmt + ;
                  IIF(m.lBackOrd,0,MAX(OrdLine.TotQty-m.TotQty,0))*OrdLine.Price ,;
                  Ship      WITH Ship + m.TotQty ,;
                  ShipAmt   WITH ShipAmt + m.TotQty*m.Price ,;
                  Open      WITH MAX(Open-IIF(m.lBackOrd, lnOrdShip,OrdLine.TotQty),0),;
                  OpenAmt   WITH MAX(OpenAmt - ;
                  IIF(m.lBackOrd, lnOrdShip,OrdLine.TotQty)*OrdLine.Price,0) ,;
                  Status    WITH IIF(Open > 0,Status,'C')
          
          *B606392,1 Lock the Order Header record. [Begin]
          REPLACE lLok_Stat WITH .F. ,;
                  cLok_User WITH ''  ,;
                  dLok_Date WITH {}  ,;
                  cLok_Time WITH ''
          *B606392,1 Lock the Order Header record. [End]
          
          *B803174,1 (End)
          
          
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
          *B603318,1 Start, Update pik line and cutpick.
          *-- Call a custom function if found  
          IF ASCAN(laEvntTrig , PADR('UPDPIKLN',10)) <> 0
            = gfDoTriger('ARIINV',PADR('UPDPIKLN',10))
          ENDIF
          *B603318,1 End.
          SELECT PIKLINE
          APPEND FROM (gcWorkDir+lcTmpPikLn)
          =gfTraceKey('PIKLINE',m.PIKTKT+m.ORDER+STR(m.LINENO,6),'A')
        ENDIF
        
        *B605832,1 Always check if this pick ticket was shipped. [Begin]
        *IF OrdLine.Picked .AND. SEEK(OrdLine.Order+OrdLine.PikTkt,'PikTkt')
        IF OrdLine.Picked AND SEEK(OrdLine.Order+OrdLine.PikTkt,'PikTkt') AND (&lcHdrFile..ShipAmt > 0 OR !m.lBackOrd)
        *B605832,1 Always check if this pick ticket was shipped. [End]

          SELECT PikTkt
          =RLOCK()
          *B603022,1 MAN 07/20/1999 Replace PikTkt Status with C instead of X
          *REPLACE Status WITH 'X'
          *-- Change the status of the pick ticket to  complete
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
          REPLACE Qty1   WITH Qty1 - IIF(m.lBackOrd,MIN(Qty1,m.Qty1),Qty1),;
                  Qty2   WITH Qty2 - IIF(m.lBackOrd,MIN(Qty2,m.Qty2),Qty2),;
                  Qty3   WITH Qty3 - IIF(m.lBackOrd,MIN(Qty3,m.Qty3),Qty3),;
                  Qty4   WITH Qty4 - IIF(m.lBackOrd,MIN(Qty4,m.Qty4),Qty4),;
                  Qty5   WITH Qty5 - IIF(m.lBackOrd,MIN(Qty5,m.Qty5),Qty5),;
                  Qty6   WITH Qty6 - IIF(m.lBackOrd,MIN(Qty6,m.Qty6),Qty6),;
                  Qty7   WITH Qty7 - IIF(m.lBackOrd,MIN(Qty7,m.Qty7),Qty7),;
                  Qty8   WITH Qty8 - IIF(m.lBackOrd,MIN(Qty8,m.Qty8),Qty8),;
                  TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
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
          *B604256,1 (Begin) Zero out any packed qty done on this piktkt
          REPLACE nPck1 WITH 0,;
                  nPck2 WITH 0,;
                  nPck3 WITH 0,;
                  nPck4 WITH 0,;
                  nPck5 WITH 0,;
                  nPck6 WITH 0,;
                  nPck7 WITH 0,;
                  nPck8 WITH 0
          *B604256,1 (End)
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
      *E300817,1  Cost of Goods Sold <DEBIT> Category key '008'
      laGlArray[1,1] = m.Gl_Sales
      laGlArray[1,2] = '008'
      *E301038,1 Change the Gl transaction sign.
      *laGlArray[1,3] = 1
      laGlArray[1,3] = -1
      
      *E300817,1  Inventory Control <CREDIT> Category key '006'
      laGlArray[2,1] = m.Gl_Cost
      laGlArray[2,2] = '006'
      *E301038,1 Change the Gl transaction sign.
      *laGlArray[2,3] = -1
      laGlArray[2,3] = 1

      =SEEK(m.Gl_Sales+'008','GL_LINK')
      m.cCOGSAcnt = GL_LINK.GlAcnt      && Cost of goods sold account
      =SEEK(m.Gl_Cost+'006','GL_LINK')
      m.cICAcnt = GL_LINK.GlAcnt        && Inventory control account 
    ENDIF
    SELECT (lcDetFile)
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laAdjStk

    *B602270,1 Do not Style inventory jornal with zero shipped
    *lnNextSt  = gfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,;
                @laAdjStk,m.Cost,'',.F.,'',13,lcDetFile,'nSteps',@laGlArray)
    *B602322,1 Pass GL Session sequence to this function to make sure that the record
    *          in journal is with the same session no.
    *lnNextSt  = IIF(m.TotQty=0,13,;
                gfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,;
                @laAdjStk,m.Cost,'',.F.,'',13,lcDetFile,'nSteps',@laGlArray))

    *E301200 (AAMER) Start
    *lnNextSt  = IIF(m.TotQty=0,13,;
                gfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,;
                @laAdjStk,m.Cost,'',lcGlSession,'',13,lcDetFile,'nSteps',@laGlArray))

    *E301192 AAMER (Start)
    *lnNextSt  = IIF(m.TotQty=0,13,;
                gfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,;
                @laAdjStk,m.Cost,'',lcGlSession,'',13,lcDetFile,'nSteps',@laGlArray,LineNo))
    
    PRIVATE lcRefer
    IF EMPTY(&lcHdrFile..Order) AND &lcHdrFile..DIRECT_INV
      lcRefer = 'CUST# '+ &lcHdrFile..Account + "-" + Customer.BTName
    ELSE
      lcRefer = 'CUST# '+ &lcHdrFile..Account + " Sales order " + &lcHdrFile..Order
    ENDIF

    *B602971 (Start)
    *lnNextSt  = IIF(m.TotQty=0,13,;
                gfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,;
                @laAdjStk,m.Cost,lcRefer,lcGlSession,'',13,lcDetFile,'nSteps',@laGlArray,LineNo))
    *B802336,1 Fix updating stock and inventory journal for consolidated invoice
    *lnNextSt  = IIF(m.TotQty=0,13,;
                gfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,;
                @laAdjStk,m.Cost,lcRefer,lcGlSession,'',13,lcDetFile,'nSteps',@laGlArray,m.LineNo))
    
    *-- Update the  Stock in the STYDYE , STYINVJL , STYLE and get the next step number
    *-- for the uncomplete Session 

    *B606343,1 Don't execute Style control function in consolidated line with consol = 'Y'. [Begin]
    *lnNextSt  = IIF(m.TotQty=0 OR !llConsAcc,13,;
    *            gfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,;
    *            @laAdjStk,m.Cost,lcRefer,lcGlSession,'',13,lcDetFile,'nSteps',@laGlArray,m.LineNo))
    lnNextSt = IIF(m.TotQty=0 OR Consol = 'Y',13,;
               gfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,;
               @laAdjStk,m.Cost,lcRefer,lcGlSession,'',13,lcDetFile,'nSteps',@laGlArray,m.LineNo))
    *B606343,1 Don't execute Style control function in consolidated line with consol = 'Y'. [End]

    *B802336,1 (End)


    *B602971 (End)
    *E301192 (End)
    *E301200 (AAMER) End
    
    *B602322,1 (End).
    *!B602270,1 (End)
    
    *-- Get difference between the old stock value and the new stock value
    lnStkVal  = lnStkVal - Style.nStkVal 
    *-- calculate the new cost
    *B603728,1 NAD 08/08/2000 (Begin) Get the style cost from style file if cost method is standard.
    IF laInvSetup[3,2]='S'
      m.Cost = Style.TotCost
    ELSE
    *B603728,1 NAD 08/08/2000 (End)
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
    *B802865,1 WAB - in case of consolidate the store code saved in cconStore field
    m.Store = IIF(m.Consol='Y',m.cConStore,m.Store)
    *B802865,1 WAB - END
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
  =IIF(&lcHdrFile..DIRECT_INV,gfTraceKey('ORDLINE','O'+lcOrdNo,'A'),.T.)
  =gfTraceKey('INVLINE',lcInvNo,'A')
  *B802865,1 WAB - use filed cConStore when search in lcUpsFile in case of consolidate
  *=IIF(!llConsAcc AND SEEK(lcInvNo+&lcHdrFile..Store,'CONSINVL'),;
       gfTraceKey('CONSINVL',lcInvNo+&lcHdrFile..Store,'A'),.T.)
  lcStoreFld = IIF(&lcHdrFile..Consol='Y',&lcHdrFile..cConStore,&lcHdrFile..Store)       
  =IIF(!llConsAcc AND SEEK(lcInvNo+lcStoreFld,'CONSINVL'),;
       gfTraceKey('CONSINVL',lcInvNo+lcStoreFld,'A'),.T.)
  *B802865,1 WAB - END
  *E300956,1 Update order line calceled quantity.
  IF !EMPTY(lcOrdCanLn) AND &lcHdrFile..Consol<>'Y' AND ;
       SEEK('O'+&lcHdrFile..ORDER,lcOrdCanLn)
    
    *B604607,1 Save the price memory variable. [Begin]
    PRIVATE lnInvPrice
    lnInvPrice = IIF(TYPE('m.Price')<>'U',m.Price,0)
    *B604607,1 Save the price memory variable. [End]
    
    SELECT (lcOrdCanLn)
    SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+&lcHdrFile..ORDER
      IF !SEEK(cOrdType+Order+STR(LineNo,6),'ORDCANLN')
      *-- Update the order canceld Quantity
        SCATTER MEMVAR
        INSERT INTO 'ORDCANLN' FROM MEMVAR
      ENDIF
    ENDSCAN
    
    *B604607,1 Get the price. [Begin]
    m.Price = lnInvPrice
    *B604607,1 Get the price. [End]
    
    =gfTraceKey('ORDCANLN','O'+&lcHdrFile..ORDER,'A'),.T.)
  ENDIF
  *E300956,1 (End)
    
  WAIT 'Updating Invoice Header...' WINDOW NOWAIT
  *-- shipped amount - discount - Trade discount
  lnNetShipAmnt = &lcHdrFile..ShipAmt*(1-&lcHdrFile..DiscPcnt/100)*(1-&lcHdrFile..Trde_Disc/100)
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
  *B802865,1 WAB - in case of consolidate the store code saved in cconStore field
  m.Store = IIF(m.Consol='Y',m.cConStore,m.Store)
  *B802865,1 WAB - END
  m.Comm1     = lnHdrComm1
  m.Comm2     = lnHdrComm2
  *-- sales rep Commission at Style level or at invoice level   
  m.COMMAMT1  = IIF(laInvSetup[6,2]='Y',lnComm1,lnHdrComm1*lnNetShipAmnt/100)
  m.COMMAMT2  = IIF(laInvSetup[6,2]='Y',lnComm2,lnHdrComm2*lnNetShipAmnt/100)
  
  *C102676,1 Custom process for A.S.T. sportwear. [Begin]
  *-- This line we check for TYPE of laEvntTrig in case of EDI module.
  IF TYPE('laEvntTrig') <> 'U' AND ASCAN(laEvntTrig , PADR('CALCAMT',10)) <> 0
    =gfDoTriger('ARIINV',PADR('CALCAMT',10))
  ENDIF  
  *C102676,1 Custom process for A.S.T. sportwear. [End]  
  
  m.Link_Code = lcCustLink
  m.Invoice   = lcInvNo

  IF laInvSetup[1,2]='Y'
    *E300817,1  Freight income <CREDIT> Category key '004'
    IF UPPER(ALLTRIM(gcContCode))='ENG' AND !EMPTY(lcEngChrg)
      m.cFrgtAcnt = ''
      IF m.Consol<>'Y' AND SEEK(IIF(m.DIRECT_INV,SPACE(6),m.Order)+m.Store+m.PikTkt,lcEngChrg)
        *-- the company is england so update the Gl with the english charges
        *--In case of england charges taxes is at style level
        SELECT (lcEngChrg)
        SCAN REST WHILE Order+Store+PikTkt+cchrgcode = ;
        IIF(m.DIRECT_INV,SPACE(6),m.Order)+m.Store+m.PikTkt
          DO GLDIST WITH lcCustLink,'004',-&lcEngChrg..nChrgAmnt,'IN',;
                     m.Invoice,m.dPostDate,lcGlYear,lcGlPeriod,;
                     lcDistFile,&lcEngChrg..cFrgtAcnt,m.cCurrCode,m.nCurrUnit,m.nExRate
        ENDSCAN
      ENDIF
    ELSE
      IF llConsAcc
      *-- other charges
        DO GLDIST WITH lcCustLink,'004',-(m.Freight+m.Insur+m.Cod),'IN',;
                       m.Invoice,m.dPostDate,lcGlYear,lcGlPeriod,;
                       lcDistFile,'',m.cCurrCode,m.nCurrUnit,m.nExRate
        *B602775 MAN Added If Condition to prevent the case where the 
        *B602775 MAN Frieght is 0 and no GL entry is created (Therefore
        *B602775 MAN the GLDIST record pointer could be anything) (Start)
        *m.cFrgtAcnt = &lcDistFile..GlAccount
        IF (m.Freight+m.Insur+m.Cod) <> 0
		  m.cFrgtAcnt = &lcDistFile..GlAccount   && Frieght account
        ELSE
          m.cFrgtAcnt = ''
        ENDIF
        *B602775 MAN (End)
      ENDIF  
    ENDIF
    IF llConsAcc AND laInvSetup[7,2]='Y'
     *-- use taxes in the company setup is yes so update the Gl with income tax 
      *E300817,1  Tax Income <CREDIT> Category key '014'
      *C102212,1 (Begin) Update HST Tax.
      *DO GLDIST WITH lcCustLink,'014',-(m.Tax_Amt+m.nPstAmt),'IN',m.Invoice,;
                     m.dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
                     m.cCurrCode,m.nCurrUnit,m.nExRate
      DO GLDIST WITH lcCustLink,'014',-(m.Tax_Amt+m.nPstAmt+m.nHstAmt),'IN',m.Invoice,;
                     m.dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
                     m.cCurrCode,m.nCurrUnit,m.nExRate
      *C102212,1 (End)
      *B602775 MAN Added If Condition to prevent the case where the 
      *B602775 MAN Tax is 0 and no GL entry is created (Therefore
      *B602775 MAN the GLDIST record pointer could be anything) 
      *B602775 MAN Note: (Start)
      *m.cTaxAcnt = &lcDistFile..GlAccount
      *C102212,1 (Begin) Add HST Tax to check.
      *IF m.Tax_Amt+m.nPstAmt <> 0
      IF m.Tax_Amt+m.nPstAmt+m.nHstAmt <> 0
      *C102212,1 (End)
        m.cTaxAcnt = &lcDistFile..GlAccount && Tax Account
      ELSE
        m.cTaxAcnt = ""
      ENDIF  
    ENDIF  
    IF llConsAcc
   
      *E300817,1  Account receivable <DEBIT> Category key '001'
      DO GLDIST WITH lcCustLink,'001',m.TotalChg,'IN',m.Invoice,m.dPostDate,;
                     lcGlYear,lcGlPeriod,lcDistFile,'',m.cCurrCode,m.nCurrUnit,m.nExRate
      *B602775 MAN Added If Condition to prevent the case where the 
      *B602775 MAN AR is 0 and no GL entry is created (Therefore
      *B602775 MAN the GLDIST record pointer could be anything) 
      *B602775 MAN Note: (Start)
      *m.cArAcnt = &lcDistFile..GlAccount
      IF m.TotalChg <> 0
      
        *B603989,1 NAD 01/29/2000 (Start) On Currency sales invoices there are pence difference between Catg_Key='003' AND  Catg_Key='001'   
        lnEqv001=0
        SUM nEqvAmnt FOR Tran_No =m.Invoice AND (Catg_Key='003' OR Catg_Key='004' OR Catg_Key='005' OR Catg_Key='014') TO  lnEqv001
        LOCATE FOR  Tran_No =m.Invoice AND Catg_Key='001'
        REPLACE nEqvAmnt WITH  ABS(lnEqv001)
        *B603989,1 NAD 01/29/2000 (End)
                
        m.cArAcnt = &lcDistFile..GlAccount   && Account recievable account
      ELSE
        m.cArAcnt = ''
      ENDIF
      *B602775 MAN (End)
    ENDIF
  ENDIF
  
  *B604451,1 NAD 06,19,2001 (Start) Invoice number got created for 2 different orders
  *IF llConsAcc AND !SEEK(m.Invoice,'INVHDR')
  *B604891,1 SSH Fix bug of not saving invoice in case of calling from PS
  *IF llConsAcc AND SEEK(m.Invoice,'INVHDR') AND EMPTY(InvHdr.Account)
  IF ( gcAct_Appl="PS" .AND. USED("postran") .AND. llConsAcc AND !SEEK(m.Invoice,'INVHDR')) ;
    .OR.( llConsAcc AND SEEK(m.Invoice,'INVHDR') AND EMPTY(InvHdr.Account))
  *B604891,1 SSH Fix bug of not saving invoice in case of calling from PS
  *B604451,1 NAD 06,19,2001 (End) Invoice number got created for 2 different orders
  
    *-- update the invoice header  
    m.Status = 'C'
    
    *B604451,1 NAD 06,19,2001 (Start) Invoice number got created for 2 different orders
    lcSVAlias=Alias()
    *INSERT INTO INVHDR FROM MEMVAR 
    SELECT InvHdr
    *B604891,1 SSH Fix bug of not saving invoice in case of calling from PS
    IF gcAct_Appl="PS" .AND. USED("postran")
      APPEND BLANK
    ENDIF
    *B604891,1 SSH Fix bug of not saving invoice in case of calling from PS
    GATHER MEMVAR MEMO
    SELECT (lcSVAlias)
    *B604451,1 NAD 06,19,2001 (END)
    
    =gfTraceKey('INVHDR',m.Invoice,'A')
    
   
  *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
  IF ASCAN(laEvntTrig , PADR('JLINSERT',10)) <> 0
    =gfDoTriger('ARIINV',PADR('JLINSERT',10))
  ENDIF
  *C102314,1 BWA 06/10/2001 [END]

  *B802603 (Start)
  *B604451,1 NAD 06,19,2001 (Start) Commented out and re-type it under the if condition.
    * ELSE
  *-- sales order invoice so replace order customer po With the invoice header customer po 
  *  IF !EMPTY(OrdHdr.CustPO)
  *    PRIVATE lnCurAlias
  *    lnCurAlias = SELECT(0)
  *   SELECT InvHdr
  *    REPLACE CustPO WITH m.CustPO
  *    SELECT (lnCurAlias)
  *  ENDIF
  *B604451,1 NAD 06,19,2001 (End) 
    
  *B802603 (End)
  ENDIF
  *B604451,1 NAD 06,19,2001 (Start) re-type the function . 
  IF !llConsAcc AND SEEK(m.Invoice,'INVHDR')
    IF !EMPTY(OrdHdr.CustPO)
      PRIVATE lnCurAlias
      lnCurAlias = SELECT(0)
      SELECT InvHdr
      REPLACE CustPO WITH m.CustPO
      SELECT (lnCurAlias)
    ENDIF
  ENDIF  
  *B604451,1 NAD 06,19,2001 (End)
  
  IF !llConsAcc .AND. !SEEK(m.Invoice+m.Store+m.Order,'CONSINVH')
    m.Status = 'C'
    *-- update the conslidated invoice Header file
    INSERT INTO CONSINVH FROM MEMVAR
    =gfTraceKey('CONSINVH',m.Invoice+m.Store+m.Order,'A')
  ENDIF
  IF m.CONSOL<>'Y'
    IF !SEEK('O'+m.Order,'ORDHDR')
    *-- direct invoice so create order 
      SELECT ORDHDR
      INSERT INTO ORDHDR (cOrdType,Order,Status,Account,Store,CustPo) VALUES ;
      ('O',m.Order,'C',m.Account,m.Store,m.CustPo)

      lcFactAcct = ''
      =SEEK(IIF(EMPTY(m.Store),'M'+m.Account,'S'+m.Account+m.Store),'CUSTOMER')
      SELECT ORDHDR
      *-- Add the customer defaults to the order header
      =SEEK('O'+m.Order)
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
              Book       WITH m.Ship     ,;
              BookAmt    WITH m.ShipAmt  ,;
              Ship       WITH m.Ship     ,;
              ShipAmt    WITH m.ShipAmt  ,;
              LastLine   WITH m.LastLine ,;
              Link_Code  WITH lcCustLink ,;
              dAdd_Date WITH gdSysDate   ,;
              cAdd_Time WITH TIME()      ,;
              cAdd_User WITH gcUser_id   ,;
              Distrb_No  WITH Customer.Dist_Ctr ,;
              DIRECT_INV WITH .T.
      
      *C102676,1 Custom process for A.S.T. sportwear. [Begin]
      *-- This line we check for TYPE of laEvntTrig in case of EDI module.
      IF TYPE('laEvntTrig') <> 'U' AND ASCAN(laEvntTrig , PADR('BUILDORD',10)) <> 0
        =gfDoTriger('ARDINV',PADR('BUILDORD',10))
      ENDIF  
      *C102676,1 Custom process for A.S.T. sportwear. [End]  
             
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
      =gfTraceKey('ORDHDR','O'+m.Order,'A')
    ELSE
    *-- sales order invoice so decrease the approved amouunt with total Charges 
      IF &lcHdrFile..nSteps < 1
        SELECT ORDHDR
        =RLOCK()
        
        *B605717,1 Descrease the Approved amount with shipped amount only. [Begin]        
        *REPLACE ApprAmt WITH MAX(ApprAmt - m.TotalChg,0)
        REPLACE ApprAmt WITH MAX(ApprAmt - m.ShipAmt,0)
        *B605717,1 Descrease the Approved amount with shipped amount only. [End]
        
        UNLOCK
        SELECT (lcHdrFile)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
        =gfTraceKey('ORDHDR','O'+m.Order,'M')
      ENDIF
    ENDIF  
  ENDIF
  WAIT 'Updating Sales Representative Information...' WINDOW NOWAIT
  IF Consol<>'Y' .AND. &lcHdrFile..nSteps < 2 .AND. SEEK(m.Rep1,'SALESREP')
    SELECT SALESREP
    
    *B603998,4 Add this check so as not to add a zero record. [Begin]
    *=RLOCK()
    *lnEqvAmnt = ROUND(m.COMMAMT1 &lcExRSin m.nExRate &lcUntSin m.nCurrUnit,2)
    lnEqvAmnt = ROUND(m.COMMAMT1 &lcExRSin m.nExRate &lcUntSin m.nCurrUnit,2)
    IF lnEqvAmnt > 0
      =RLOCK()
    *B603998,4 Add this check so as not to add a zero record. [En
    
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
              TRAN      WITH m.Invoice   ,;
              ACCOUNT   WITH m.Account   ,;
              ORDER     WITH m.Order     ,;
              CUSTPO    WITH m.CustPo    ,;
              STORE     WITH m.Store     ,;
              DATE      WITH m.InvDate   ,;
              DESC      WITH 'INVOICE'   ,;
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
      =gfTraceKey('REPCOMM',m.Rep1+DTOS(m.InvDate)+m.Invoice+'1','A')
    
    *B603998,4 Add this check so as not to add a zero record. [Begin]
    ENDIF
    *B603998,4 Add this check so as not to add a zero record. [End]  
    
    SELECT (lcHdrFile)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  IF m.Consol<>'Y' .AND. &lcHdrFile..nSteps < 3 .AND. SEEK(m.Rep2,'SALESREP')
    SELECT SALESREP
    
    *B606165,1 Make m.COMMAMT2 instead of m.COMMAMT1. [Begin]
    *lnEqvAmnt = ROUND(m.COMMAMT1 &lcExRSin m.nExRate &lcUntSin m.nCurrUnit,2)  
    lnEqvAmnt = ROUND(m.COMMAMT2 &lcExRSin m.nExRate &lcUntSin m.nCurrUnit,2)
    *B606165,1 Make m.COMMAMT2 instead of m.COMMAMT1. [End]
    
    *B603998,4 Add this check so as not to add a zero record. [Begin]
    IF lnEqvAmnt > 0
    *B603998,4 Add this check so as not to add a zero record. [End]
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
              TRAN      WITH m.Invoice   ,;
              ACCOUNT   WITH m.Account   ,;
              ORDER     WITH m.Order     ,;
              CUSTPO    WITH m.CustPo    ,;
              STORE     WITH m.Store     ,;
              DATE      WITH m.InvDate   ,;
              DESC      WITH 'INVOICE'   ,;
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
      =gfTraceKey('REPCOMM',m.Rep2+DTOS(m.InvDate)+m.Invoice+'1','A')
    
    *B603998,4 Add this check so as not to add a zero record. [Begin]
    ENDIF
    *B603998,4 Add this check so as not to add a zero record. [End]
      
    SELECT (lcHdrFile)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF

  *C102676,1 Custom process for A.S.T. sportwear. [Begin]
  IF TYPE('laEvntTrig') <> 'U' AND ASCAN(laEvntTrig , PADR('CALCREP3',10)) <> 0
    =gfDoTriger('ARIINV',PADR('CALCREP3',10))
  ENDIF  
  *C102676,1 Custom process for A.S.T. sportwear. [End]

  IF llConsAcc .AND. &lcHdrFile..nSteps < 4
  *-- Update the instalment heder and detail and the debit file
    *-- variable to check if it was sales order invoice or not
    llOrder = SEEK('O'+m.Order,'ORDHDR') .AND. !ORDHDR.DIRECT_INV
    IF !EMPTY(lcInstHdr) AND !EMPTY(lcInstLine) AND ;
      SEEK(IIF(llOrder,m.Order,SPACE(6))+m.Store+m.PikTkt,lcInstHdr) .AND. ;
      SEEK(IIF(llOrder,m.Order,SPACE(6))+m.Store+m.PikTkt,lcInstLine)
      *-- there is instalments in the invoice so open the files to update it
      *E301077,50 Inhance openning files to speed up transaction
      =gfOpenFile(gcDataDir+'ARINSTMH', gcDataDir+'Arinstmh','SH')
      =gfOpenFile(gcDataDir+'ARINSTMD', gcDataDir+'Arinstmd','SH')
      *E301077,50 (End)

      IF !SEEK(m.invoice,'ARINSTMH')
      *-- update the instalment Header
        SELECT ARINSTMH
        APPEND BLANK
        REPLACE Invoice    WITH m.Invoice ,;
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
        IF !SEEK(m.invoice+SPACE(3),'ARINSTMD')
          SELECT ARINSTMD
          APPEND BLANK
          *-- add the first line with the initial amount
          REPLACE Invoice    WITH m.Invoice ,;
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
          !SEEK(m.Account+m.Invoice+SPACE(3)+DTOS(m.InvDate),'DEBIT')
          *-- the invoice is not factored or post factored invoice = Yes 
          *-- update the debit file 
          SELECT DEBIT
          APPEND BLANK
          =RLOCK()
          REPLACE cInstalNO WITH SPACE(3)    ,;
                  DUEDATE   WITH m.InvDate   ,;
                  dPostDate WITH m.dPostDate ,;
                  REFERENCE WITH m.CustPo    ,;
                  TRANTYPE  WITH '1'         ,;
                  STORE     WITH m.Store     ,;
                  ACCOUNT   WITH m.Account   ,;
                  cFacCode  WITH m.cFacCode  ,;
                  TRAN      WITH m.Invoice   ,;
                  TRANDATE  WITH m.InvDate   ,;
                  AMOUNT    WITH &lcInstHdr..nInstIAmnt ,;
                  DSC_AMT   WITH (m.ShipAmt+m.Discount)*m.Trde_Disc/100 ,;
                  cCurrCode WITH m.cCurrCode ,;
                  nCurrUnit WITH m.nCurrUnit ,;
                  nExRate   WITH m.nExRate   ,;
                  cArGlAcc  WITH m.cArAcnt   ,;
                  dAdd_Date WITH gdSysDate   ,;
                  cAdd_Time WITH TIME()      ,;
                  cAdd_User WITH gcUser_id   ,;
                  DESC      WITH IIF(m.DIRECT_INV,'DIRECT INVOICE','SALES ORDER INVOICE')
          UNLOCK
        ENDIF
      ENDIF
      SELECT (lcInstLine)
      *-- update all the lines in the instalment detail file and the debit file 
      SCAN REST WHILE Order+Store+PikTkt+cInstalNo =;
                      IIF(llOrder,m.Order,SPACE(6))+m.Store+m.PikTkt
        IF !SEEK(m.invoice+cinstalno,'ARINSTMD')
        *-- update the instalment lines if it was not updated in the last uncomplete session        
          SELECT ARINSTMD
          APPEND BLANK
          REPLACE Invoice    WITH m.Invoice ,;
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
          !SEEK(m.Account+m.Invoice+&lcInstLine..cInstalNo+DTOS(m.InvDate),'DEBIT')
          SELECT DEBIT
          APPEND BLANK
          =RLOCK()
          REPLACE cInstalNO WITH &lcInstLine..cInstalNO ,;
                  DUEDATE   WITH &lcInstLine..DueDate   ,;
                  dPostDate WITH m.dPostDate ,;
                  REFERENCE WITH m.CustPo    ,;
                  TRANTYPE  WITH '1'         ,;
                  STORE     WITH m.Store     ,;
                  ACCOUNT   WITH m.Account   ,;
                  cFacCode  WITH m.cFacCode  ,;
                  TRAN      WITH m.Invoice   ,;
                  TRANDATE  WITH m.InvDate   ,;
                  AMOUNT    WITH &lcInstLine..nInstmAmnt ,;
                  cCurrCode WITH m.cCurrCode ,;
                  nCurrUnit WITH m.nCurrUnit ,;
                  nExRate   WITH m.nExRate   ,;
                  cArGlAcc  WITH m.cArAcnt   ,;
                  dAdd_Date WITH gdSysDate   ,;
                  cAdd_Time WITH TIME()      ,;
                  cAdd_User WITH gcUser_id   ,;
                  DESC      WITH IIF(m.DIRECT_INV,'DIRECT INVOICE','SALES ORDER INVOICE')
          UNLOCK
        ENDIF
      ENDSCAN
      IF (EMPTY(m.cFacCode) .OR. laInvSetup[8,2]='Y') .AND. ;
         &lcInstHdr..nInstIAmnt = 0 
        SELECT DEBIT
        =RLOCK()
        REPLACE DSC_AMT WITH (m.ShipAmt+m.Discount)*m.Trde_Disc/100
        UNLOCK
      ENDIF
      =gfTraceKey('ARINSTMH',m.Invoice,'A')
      =gfTraceKey('ARINSTMD',m.Invoice,'A')
    ELSE
      *-- there is no instalments so update the debit file only
      IF (EMPTY(m.cFacCode) .OR. laInvSetup[8,2]='Y') .AND. ;
        !SEEK(m.Account+m.Invoice+SPACE(3)+DTOS(m.InvDate),'DEBIT')
        *-- post factored invoice is yes
        SELECT DEBIT
        APPEND BLANK
        =RLOCK()
        REPLACE DUEDATE   WITH m.DueDate   ,;
                REFERENCE WITH m.CustPo    ,;
                dPostDate WITH m.dPostDate ,;
                TRANTYPE  WITH '1'         ,;
                STORE     WITH m.Store     ,;
                ACCOUNT   WITH m.Account   ,;
                cFacCode  WITH m.cFacCode  ,;
                TRAN      WITH m.Invoice   ,;
                TRANDATE  WITH m.InvDate   ,;
                AMOUNT    WITH m.TotalChg  ,;
                cCurrCode WITH m.cCurrCode ,;
                nCurrUnit WITH m.nCurrUnit ,;
                nExRate   WITH m.nExRate   ,;
                cArGlAcc  WITH m.cArAcnt   ,;
                DSC_AMT   WITH (m.ShipAmt+m.Discount)*m.Trde_Disc/100,;
                DESC      WITH IIF(m.DIRECT_INV,'DIRECT INVOICE','SALES ORDER INVOICE') ,;
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
    =IIF(EMPTY(m.cFacCode) OR laInvSetup[8,2]='Y',gfTraceKey('DEBIT','1'+m.Invoice,'A'),.T.)
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
    
    *E301245,1 function to update nHgWtrMark Field with NETBAL field [Begin.]
    =lfHgWUpdat() 
    *E301245,1 function to update nHgWtrMark Field with NETBAL field [End.]

    =gfTraceKey('CUSTOMER','M'+m.Account,'M')
    SELECT (lcHdrFile)
    =RLOCK()
    REPLACE nSteps WITH 5
    UNLOCK
  ENDIF
  IF laInvSetup[1,2]='Y'
    WAIT 'Updating GL Distribution File' WINDOW NOWAIT
    SELECT GLDIST
    IF !SEEK(m.Invoice+'IN'+lcGlSession)
      WAIT 'Updating General Ledger Distribution File ' WINDOW NOWAIT
      SELECT (lcDistFile)
      *B602937 (Start)
      *REPLACE ALL GLSESSION WITH 
      SCAN
        REPLACE GLSESSION WITH lcGlSession
        =gfAdd_Info(lcDistFile)  
      ENDSCAN
      *B602937 (End)
      USE
      SELECT GLDIST
      APPEND FROM (gcWorkDir+lcDistFile)
      ERASE (gcWorkDir+lcDistFile+".DBF")
    ENDIF
    =gfTraceKey('GLDIST',m.Invoice+'IN'+lcGlSession,'A')
  ENDIF
  *-- Update the ups box File 
  IF !EMPTY(lcUpsFile) AND USED(lcUpsFile)
    WAIT 'Updating UPS Box File ' WINDOW NOWAIT
    SELECT (lcUpsFile)
    *B802865,1 WAB - use filed cConStore when search in lcUpsFile in case of consolidate
    *IF SEEK(&lcHdrFile..Order+&lcHdrFile..Store+&lcHdrFile..PikTkt)
    *  SCAN REST WHILE Order+Store+PikTkt+STR(CARTONS,5)=;
    *                  &lcHdrFile..Order+&lcHdrFile..Store+&lcHdrFile..PikTkt
    lcStoreFld = IIF(&lcHdrFile..Consol='Y',&lcHdrFile..cConStore,&lcHdrFile..Store)
    IF SEEK(&lcHdrFile..Order+lcStoreFld+&lcHdrFile..PikTkt)
      SCAN REST WHILE Order+Store+PikTkt+STR(CARTONS,5)=;
                      &lcHdrFile..Order+lcStoreFld+&lcHdrFile..PikTkt
    *B802865,1 WAB - END
        IF !SEEK(lcInvNo+Store+STR(CARTONS,5),'UPSBOX')
          SCATTER MEMVAR
          m.Invoice = lcInvNo
          INSERT INTO 'UPSBOX' FROM MEMVAR
        ENDIF
      ENDSCAN
      =gfTraceKey('UPSBOX',lcInvNo,'A')
    ENDIF
  ENDIF
  *C102239,1 (Begin) Save artons in artclpshp for BEL05.
  *NAD
  *IF !EMPTY(laData[3]) AND ASCAN(laEvntTrig , PADR('SAVINV',10)) <> 0
  IF !EMPTY(&lcHdrFile..PikTkt) AND ASCAN(laEvntTrig , PADR('SAVINV',10)) <> 0
  *+NAD
    =gfDoTriger('ARIINV',PADR('SAVINV',10))
  ENDIF  
  *C102239,1 (End)

  *C102424,1 (Begin) Save artons in artclpshp for BEL05.
  IF !EMPTY(&lcHdrFile..PikTkt) AND ASCAN(laEvntTrig , PADR('SAVIT',10)) <> 0
    =gfDoTriger('ARIINV',PADR('SAVIT',10))
  ENDIF  
  *C102424,1 (End)
  
  *-- Update the english charges
  IF !EMPTY(lcEngChrg) AND USED(lcEngChrg)
    WAIT 'Updating Invoice Charges File ' WINDOW NOWAIT
    =gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH')
    SELECT (lcEngChrg)
    *B802865,1 WAB - use filed cConStore when search in lcUpsFile in case of consolidate
    *IF SEEK(IIF(&lcHdrFile..DIRECT_INV,SPACE(6),&lcHdrFile..Order)+;
    *        &lcHdrFile..Store+&lcHdrFile..PikTkt)
    *  SCAN REST WHILE Order+cStore+PikTkt+cchrgcode = ;
    *  IIF(&lcHdrFile..DIRECT_INV,SPACE(6),&lcHdrFile..Order)+&lcHdrFile..Store+&lcHdrFile..PikTkt

    lcStoreFld = IIF(&lcHdrFile..Consol='Y',&lcHdrFile..cConSTore,&lcHdrFile..Store)
    IF SEEK(IIF(&lcHdrFile..DIRECT_INV,SPACE(6),&lcHdrFile..Order)+;
            lcStoreFld+&lcHdrFile..PikTkt)
      SCAN REST WHILE Order+cStore+PikTkt+cchrgcode = ;
      IIF(&lcHdrFile..DIRECT_INV,SPACE(6),&lcHdrFile..Order)+lcStoreFld+&lcHdrFile..PikTkt
    *B802865,1 WAB - END

        IF !SEEK(lcInvNo+cStore+cChrgCode,'InvChrg')
          SCATTER MEMVAR
          m.Invoice = lcInvNo
         INSERT INTO 'InvChrg' FROM MEMVAR
        ENDIF
      ENDSCAN
      =gfTraceKey('InvChrg',lcInvNo,'A')
    ENDIF
    =gfCloseFile('InvChrg')
  ENDIF
  *-- Update the customer history file
  IF llConsAcc AND SEEK(m.Account+lcGlYear,'arCusHst') AND ;
     &lcHdrFile..nSteps < 6
    lnShipAmnt = m.ShipAmt  &lcExRSin m.NEXRATE &lcUntSin m.NCURRUNIT
    lnDiscAmnt = ABS(m.Discount) &lcExRSin m.NEXRATE &lcUntSin m.NCURRUNIT
    SELECT arCusHst
    =RLOCK()
    REPLACE nSlsQty&lcGlPeriod  WITH nSlsQty&lcGlPeriod  + m.Ship ,;
            nSlsQty             WITH nSlsQty             + m.Ship ,;
            nSlsAmt&lcGlPeriod  WITH nSlsAmt&lcGlPeriod  + lnShipAmnt ,;
            nSlsAmt             WITH nSlsAmt             + lnShipAmnt ,;
            nDisAmt&lcGlPeriod  WITH nDisAmt&lcGlPeriod  + lnDiscAmnt ,;
            nDisAmt             WITH nDisAmt             + lnDiscAmnt ,;
            nCOGSAmt&lcGlPeriod WITH nCOGSAmt&lcGlPeriod + lnCOGSAmt ,;
            nCOGSAmt            WITH nCOGSAmt            + lnCOGSAmt
    UNLOCK
    =gfTraceKey('arCusHst',m.Account+lcGlYear,'M')
    SELECT (lcHdrFile)
    =RLOCK()
    REPLACE nSteps WITH 6
    UNLOCK
  ENDIF
  *B603257,1 ABD 11/10/1999 [ Begin ] 
  *B603257,1 ABD  Commet the following lines becouse the apprAmt is subtracted 
  *B603257,1 ABD  before in the nSteps No. 1 
  *IF m.Consol <> 'Y' AND &lcHdrFile..nSteps < 7 .AND. SEEK('O'+m.Order,'ORDHDR')
  *  SELECT ORDHDR
  *  =RLOCK()
  *  REPLACE ApprAmt WITH MAX(ApprAmt-m.TotalChg,0)
  *  UNLOCK
  *  SELECT (lcHdrFile)
  *  =RLOCK()
  *  REPLACE nSteps WITH 7
  *  UNLOCK
  *ENDIF
  *B603257,1 ABD 11/10/1999 [ End ]
  IF llConsAcc AND &lcHdrFile..nSteps < 8 .AND. &lcHdrFile..lKeyOff
    =lfvKeyOff(m.Invoice)
    SELECT (lcHdrFile)
    =RLOCK()
    REPLACE nSteps WITH 8
    UNLOCK
  ENDIF
  *E301086,1 Mark generated invoice to be send

  *E301509,1 Do not send Received invocies
  *IF 'EB' $ gcCmpModules
  IF !llFromEdi AND 'EB' $ gcCmpModules
  *E301509,1 (End)
  
    IF SEEK('A'+m.Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'810','EDIPD')
      SELECT EDITRANS
      IF !SEEK('810'+PADR(m.Invoice,20)+'A'+m.Account)
        INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,lInterComp) VALUES ;
        ('810',m.Invoice,'A',m.Account,EdiAcPrt.lInterComp)
      ENDIF
      REPLACE cStatus WITH 'N'
      =gfAdd_Info('EDITRANS')
    ENDIF
    IF SEEK('F'+m.cFacCode,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'810','EDIPD')
      SELECT EDITRANS
   	  IF !SEEK('810'+PADR(m.Invoice,20)+'F'+m.cFacCode)
        INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,lInterComp) VALUES ;
        ('810',m.Invoice,'F',m.cFacCode,EdiAcPrt.lInterComp)
      ENDIF
      REPLACE cStatus WITH 'N'
      =gfAdd_Info('EDITRANS')
    ENDIF
  ENDIF
  *E301086,1 (End)
  IF !llFromEdi AND 'NC' $ gcCmpModules AND lcSysType = 'P'
    SELECT CODES
    SET ORDER TO TAG Idrltfname
    =SEEK('NYCSITEID')
    LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
                FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
    IF FOUND()
      lcSiteId = Codes.cCode_No
      SELECT EDiAcPrt
      LOCATE FOR cSiteId = lcSiteId
      IF FOUND('EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'810','EDIPD')
        SELECT EDITRANS
        IF !SEEK('810'+PADR(m.Invoice,20)+'A'+EdiAcPrt.cPartner)
          INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,lInterComp) VALUES ;
          ('810',m.Invoice,'A',EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
        ENDIF
        REPLACE cStatus WITH 'N'
        =gfAdd_Info('EDITRANS')
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

*E301086,1 Mark generated invoice to be send
*E301509,1 Do not close EDI files used by EDI programs
*IF llEdiAcPrt 
*  USE IN EDIACPRT
*ENDIF
*IF llEdiPD
*  USE IN EDIPD
*ENDIF
*IF llEdiTrans
*  USE IN EDITRANS
*ENDIF
*E301509,1 (End)

*B802778,1 Ship BOL generated for invoiced packing list
IF llPackHdr
  USE IN PACK_HDR
ENDIF
IF llBolHdr
  USE IN BOL_HDR
ENDIF
*B802778,1 (End)

*E301086,1 (End)

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

*B801882,1 Remove total charge parameter and add new parameter for charge type
*PARAMETERS lcAccount,lcStore,lcOrderNo,lcPikTKt,lcWareCode,lnShipAmt,llCod,;
           llupsinsur,lnDscPrcnt,lcTaxRate,lcPstRule,lnPstRate,lcUpsFile,;
           lcShipVia,lcCOD_AMT,lcFreight,lcInsur,lcCOD,lcCartons,lcWeight,;
           lcTaxAmnt,lcPstAmnt,lcTotChrg,lnTaxDue
PARAMETERS lcAccount,lcStore,lcOrderNo,lcPikTKt,lcWareCode,lnShipAmt,llCod,;
           llupsinsur,lnDscPrcnt,lcTaxRate,lcPstRule,lnPstRate,lcUpsFile,;
           lcShipVia,lcCOD_AMT,lcFreight,lcInsur,lcCOD,lcCartons,lcWeight,;
           lcTaxAmnt,lcPstAmnt,lnTaxDue,lcChrgType
*B801882,1 (End)

PRIVATE laShpViaFld,lcUpsType,lnCodCharge,lnInsCharge,lnFxdPrcnt,lcUpsIncr,;
        llCompTax,lcTaxRule,getf,llUpsBox

*B801882,1 Do not reinitialize charges
*STORE 0 TO &lcFreight,&lcInsur,&lcCOD,&lcCOD_AMT,&lcTaxAmnt,&lcPstAmnt,&lcTotChrg
*B801882,1 (End)

IF lnShipAmt = 0
  RETURN           
ENDIF
*E301077,50 Inhance openning files to speed up transaction
*lcFromZone = IIF(gfGetMemVar('M_WareHouse',gcAct_Comp)='Y' .AND. ;
                 SEEK(lcWareCode,'warehous'),WareHous.UPS,;
                 ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp)))
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp)) 
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
  *-- get the from zone from the wharehouse if use multiple locations = y
  lcFromZone = IIF(SEEK(lcWareCode,'warehous'),WareHous.UPS,lcFromZone)
  =gfCloseFile('WAREHOUS')
ENDIF
*E301077,50 (End)
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
  *B801882,1 Add new parameter for charge type
  *=lfGetFright(lcFromZone,SUBSTR(CUSTOMER.caddress5,1,3),lcUpsType  ,;
               llCod,llupsinsur,lcFreight,lcInsur,lcCOD  ,;
               lcCartons,lcWeight,lnCodCharge,lnInsCharge,lnShipAmt)
  *-- Calculate freights             
  =lfGetFright(lcFromZone,SUBSTR(CUSTOMER.caddress5,1,3),lcUpsType  ,;
               llCod,llupsinsur,lcFreight,lcInsur,lcCOD,lcCartons,lcWeight,;
               lnCodCharge,lnInsCharge,lnShipAmt,lcChrgType)
  *B801882,1 (End)
  
  IF (llUpsBox .AND. SUBSTR(lcUpsType,1,5)='USUPS' AND !EMPTY(lcUpsFile))
    *-- Add records for each carton in the UPSBox file
    =lfUpsBox(lcAccount,lcStore,lcOrderNo,lcPikTKt,llCod,llupsinsur,;
              &lcFreight,&lcInsur,&lcCOD,&lcCartons,&lcWeight,;
              lnShipAmt,lcUpsFile)
    IF llCod
    *-- calculate the cash on delivery amount
      &lcCOD_AMT = lnShipAmt+&lcFreight+&lcInsur+&lcCOD-lnShipAmt*lnDscPrcnt/100
      *-- ladata[1] = Invoice Number
      IF &lcCartons=1 .AND. SEEK(laData[1]+lcStore,lcUpsFile)
        *B802456,1 Fix Alias not Found Bug. [Begin]
        *REPLACE &lcUpsFile..TotalCod WITH &lcCOD_AMT
        *-- Update TotCod Field with the total amount because there is only one carton
        =lfUpCdFild()
        *B802456,1 Fix Alias not Found Bug. [End  ]
      ENDIF
    ENDIF  
  ELSE
  *-- any other ups type 
    IF llCod
      &lcCOD_AMT = lnShipAmt+&lcFreight+&lcInsur+&lcCOD-lnDscPrcnt*lnShipAmt/100
    ENDIF
    
    *B606035,1 SSE Commented out, get the line out. [Begin]
    *&lcFreight = &lcFreight*(100+lcUpsIncr)/100 
    *B606035,1 SSE Commented out, get the line out. [End]

  ENDIF
  
  *B606035,1 Calculate the freight here. [Begin]
  &lcFreight = ROUND(&lcFreight*(100+lcUpsIncr)/100,2)
  *B606035,1 Calculate the freight here. [End]
  
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
  *B802853,1 NAD (Start)
  *&lcTaxAmnt = lcTaxRate*(IIF(lcTaxRule='M',lnTaxDue,lnTaxDue+;
               &lcFreight+&lcInsur+&lcCOD)-ABS(lnTaxDue*lnDscPrcnt/100) )/100
 
  &lcTaxAmnt = ROUND(lcTaxRate*(IIF(lcTaxRule='M',lnTaxDue,lnTaxDue+;
               &lcFreight+&lcInsur+&lcCOD)-ABS(lnTaxDue*lnDscPrcnt/100) )/100,2)
  *B802853,1 NAD (End)
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
      *B802456,1 Fix Alias not Found Bug. [Begin]
      *REPLACE &lcUpsFile..TotalCod WITH &lcCOD_AMT
      =lfUpCdFild()
      *B802456,1 Fix Alias not Found Bug. [End  ]
      
    ENDIF
  ENDIF
ENDIF
*B801882,1 Remove total charge parameter
*&lcTotChrg = lnShipAmt+&lcFreight+&lcInsur+&lcCOD-ABS(lnShipAmt*lnDscPrcnt/100)+;
             &lcTaxAmnt+&lcPstAmnt
*B801882,1 (End)
             
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

*B802837,1 (Start)
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
*B802837,1 (End)
  SELECT (lcUpsFile)
  =SEEK(lcOrderNo+lcStore+lcPikTKt+STR(lnCartons+1,5))
  DELETE REST WHILE Order+Store+PikTkt+STR(CARTONS,5)= lcOrderNo+lcStore+lcPikTKt
  FOR lnCount = 1 TO lnCartons
    IF !SEEK(lcOrderNo+lcStore+lcPikTKt+STR(lnCount,5))
    *-- if not  found  in the upsfile then add a new line
    *-- else replace it with the new values  
      APPEND BLANK
      REPLACE Account    WITH lcAccount ,;
              Order      WITH lcOrderNo ,;
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
*B802837,1 (Start)
ENDIF
*B802837,1 (End)  

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
*PARAMETERS lcUpsFrom,lcToZip,lcUpsType,llCod,llupsinsur,lcFreight,lcInsur,;
           lcCOD,lcCartons,lcWeight,lnCodCharge,lnInsCharge,lnTotShpAmt
PARAMETERS lcUpsFrom,lcToZip,lcUpsType,llCod,llupsinsur,lcFreight,lcInsur,;
           lcCOD,lcCartons,lcWeight,lnCodCharge,lnInsCharge,lnTotShpAmt,lcChrgType
PRIVATE lcCurrExac,lcCurrNear

*STORE 0 TO &lcFreight,&lcInsur,&lcCOD,lnMaxWeight

*B801882,1 Recompute charge when needed
*IF lcUpsType = 'USUPSC'
*  &lcCOD   = IIF(llCod,lnCodCharge*&lcCartons,&lcCOD)
*  &lcInsur = IIF(lnTotShpAmt >=100 .AND. llupsinsur,;
*                 FLOOR(lnTotShpAmt/100)*lnInsCharge,&lcInsur)
*  RETURN
*ENDIF

IF INLIST(lcChrgType,'A','C')
*--  Recalculate the cod 
  &lcCOD   = IIF(llCod,IIF('USUPS' $ lcUpsType,lnCodCharge*&lcCartons,&lcCOD),0)
ENDIF
IF INLIST(lcChrgType,'A','I')
*--  Recalculate the insurance
  *B804358  NAD Wrong UPS insurance in the  charges folder   (START)
  *&lcInsur = IIF(llupsinsur,IIF('USUPS' $ lcUpsType AND lnTotShpAmt >=100,;
                 FLOOR(lnTotShpAmt/100)*lnInsCharge,&lcCOD),0)
  &lcInsur = IIF(llupsinsur,IIF('USUPS' $ lcUpsType AND lnTotShpAmt >=100,;
                 FLOOR(lnTotShpAmt/100)*lnInsCharge,0),0)               
 *B804358  NAD Wrong UPS insurance in the  charges folder   (END)         
ENDIF
IF lcUpsType = 'USUPSC' OR INLIST(lcChrgType,'I','C')
  RETURN
ENDIF
*B801882,1 (End)

*E301077,50 Inhance openning files to speed up transaction
=gfOpenFile(gcSysHome+'sysRates', gcSysHome+'Frtrates','SH')
=gfOpenFile(gcSysHome+'syszones', gcSysHome+'Frtzones','SH')
*E301077,50 (End)

IF !SEEK(lcUpsType+lcUpsFrom+lcToZip,'syszones') .AND. ;
   IIF(SUBSTR(lcUpsType,1,2)='CA',;
   !SEEK(lcUpsType+lcUpsFrom+SUBSTR(lcToZip,1,1),'syszones'),.T.)
  *E300817,1  Message : 40027
  *E300817,1  Invalid shipper postal zone or shipto zone! Unable to calculate freight.
  *E300817,1  Button : 00000
  *E300817,1  Ok
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
  *E300817,1  Message : 40028
  *E300817,1  Weight cannot exceed xxx/carton! Carton count has been adjusted.
  *E300817,1  Button : 00000
  *E300817,1  Ok
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
*E301077,50 Inhance openning files to speed up transaction
*lcFromZone = IIF(gfGetMemVar('M_WareHouse',gcAct_Comp)='Y' .AND. ;
                 SEEK(lcWareCode,'warehous'),WareHous.UPS,;
                 ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp)))
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
  lcFromZone = IIF(SEEK(lcWareCode,'warehous'),WareHous.UPS,lcFromZone)
  =gfCloseFile('WAREHOUS')
ENDIF
*E301077,50 (End)

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
  WHILE Order+Store+PikTkt+STR(CARTONS,5) = lcOrderNo+lcStore+lcPikTkt
  =SEEK(lcOrderNo+lcStore+lcPikTkt)
ELSE
  =SEEK(lcInvoice)
  SUM REST Weight,Decl_Value,Freight,Insur,Cod,TotalCod ;
  TO       lnWeight,lnDecValue,lnFreight,lnInsure,lnCod,lnCodAmnt ;
  WHILE invoice+store+STR(cartons,5) = lcInvoice
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
  WHILE Order+Store+PikTkt+STR(CARTONS,5) = lcOrderNo+lcStore+lcPikTkt
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
WHILE Order+Store+PikTkt+STR(CARTONS,5) = lcOrderNo+lcStore+lcPikTkt
INSERT INTO (lcUpsFile) (Account,Order,PikTkt,Store,Cartons,InsurYn,CodYn) ;
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
SCAN REST WHILE Order+Store+PikTkt+STR(CARTONS,5) = lcOrderNo+lcStore+lcPikTkt
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
  *B602850,1 Pass charge type to lfGetFright() to avoid an 'Operator/
  *=lfGetFright(lcFromZone,SUBSTR(CUSTOMER.caddress5,1,3),lcUpsType,;
               llCod,llUpsInsur,'m.Freight','m.Insur','m.Cod',;
               'lnOneCrtn','m.Weight',lnCodCharge,lnInsCharge,m.Decl_Value)
  *--Calculate freights             
  =lfGetFright(lcFromZone,SUBSTR(CUSTOMER.caddress5,1,3),lcUpsType,;
               llCod,llUpsInsur,'m.Freight','m.Insur','m.Cod',;
               'lnOneCrtn','m.Weight',lnCodCharge,lnInsCharge,m.Decl_Value, 'A')
  *B602850,1 end
  
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
*B803571,1 NAD 10/25/2000 (Start) Variable to hold the old charges
PRIVATE lnOldChrg
STORE 0 TO lnOldChrg
*B803571,1 NAD 10/25/2000 (End)
lnAlias = SELECT()
*-- company setup information
llCompTax = (gfGetMemVar('M_TAX',gcAct_Comp)='Y')  && Use Taxes = Y
llGlLink  = (gfGetMemVar('M_LINK_GL',gcAct_Comp)='Y')  && GL link
lnChrgMark = 0
DECLARE laLCodes[1,10],laCharges[1,2]
STORE '' TO laCharges
lnTotChrg = &lcTotChrg
*B803397,1 (Start) Open the InvChrg file in the edit mode too.
*=IIF(laScrMode[2],gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH'),.T.)
=IIF(laScrMode[2] OR laScrMode[3] ,gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH'),.T.)
*B803397,1 (End)
SELECT (lcChrgFile)
IF laScrMode[4]  && Add Mode
  =SEEK(lcOrderNo+lcStore+lcPikTkt)
  SUM REST nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 TO lnTotTax ;
  WHILE Order+Store+PikTkt+cchrgcode = lcOrderNo+lcStore+lcPikTkt
  =SEEK(lcOrderNo+lcStore+lcPikTkt)
ELSE
  =SEEK(lcInvoice+lcStore)
  SUM REST nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 TO lnTotTax ;
  WHILE invoice+cstore+cchrgcode = lcInvoice+lcStore
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
  WHILE Order+Store+PikTkt+cchrgcode = lcOrderNo+lcStore+lcPikTkt
  &lcTotChrg = lnTotChrg
  &lcTotTax  = lnTotTax
ENDIF
*B803397,1 (Start) Close the  InvChrg file in the edit mode.
*=IIF(laScrMode[2],gfCloseFile('InvChrg'),.T.)
=IIF(laScrMode[2] OR laScrMode[3] ,gfCloseFile('InvChrg'),.T.)
*B803397,1 (End)
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
lcCurrKey = Order+Store+PikTkt+cchrgcode 
COUNT TO lnChrgCount 
=SEEK(lcCurrKey)
IF lnChrgCount = 10
  *E300817,1  Message : 40026
  *E300817,1  You can enter upto 10 differennt charge codes.
  *E300817,1  Button : 00000
  *E300817,1  Ok
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
*E300817,1  Message : 40025
*E300817,1  Charge code xxx has already been selected!
*E300817,1  Button : 00000
*E300817,1  Ok
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
  *B602891,1 AMM Move this lile inside the IF condition
  *ENDIF
  *m.nTaxRate = lcTaxRate
    m.nTaxRate = lcTaxRate
  ENDIF
  *B602891,1 AMM end
  SELECT (lcChrgFile)  
ENDIF
*E300883,1 Get Non-merch. chage GL Account related field.
IF llGlLink
  DECLARE laChRltFld[1,2]
  STORE '' TO lcFrgtAcnt
  laChRltFld[1,1] = 'CFRGTACNT'
  laChRltFld[1,2] = 'lcFrgtAcnt'
  =gfRltFld(m.cChrgCode,@laChRltFld,'CCHRGCODE')
  m.cFrgtAcnt= lcFrgtAcnt
  SELECT (lcChrgFile)  
ENDIF
*E300883,1 (End)

IF llNewChrg
  INSERT INTO (lcChrgFile) (Invoice,Order,PikTkt,cStore) ;
  VALUES (lcInvoice,lcOrderNo,lcPikTkt,lcStore)
  llNewChrg = .F.
ENDIF
lnTotTax = lnTotTax - (nTaxRate-m.nTaxRate)*m.nChrgAmnt*(1-lnTrdDsc/100)/100
=RLOCK()

*E300883,1 Get Non-merch. chage GL Account related field.
*REPLACE cChrgCode WITH m.cChrgCode ,;
        nTaxRate  WITH m.nTaxRate
REPLACE cChrgCode WITH m.cChrgCode ,;
        nTaxRate  WITH m.nTaxRate  ,;
        cFrgtAcnt WITH m.cFrgtAcnt
*E300883,1 (End)

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

*E300817,1  Message : 40011
*E300817,1  Are you sure you want to remove this xxx?
*E300817,1  Button : 40000
*E300817,1  Yes No
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
*B803571,1 NAD 10/25/2000 (Start) Not to allow users to add negative values in the charges fields.
IF m.nChrgAmnt <0 
  * Message : 42000
  * Negative values are not allowed.
  * Buttfon  : 40011
  * Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.nChrgAmnt=lnOldChrg
  _CUROBJ = _CUROBJ  
  RETURN
ENDIF
*B803571,1 NAD 10/25/2000  (End)
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
*B802231,1 Consider order type when enter order# manualy
*DO (gcScrDir+"ARORDER.SPX") WITH 'O','lcOrderNo'
DO (gcScrDir+"ARORDER.SPX") WITH 'O','lcOrderNo','O'
*B802231,1 (End)
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
  INSERT INTO (lcInstHdr) (Order,Store,PikTkt,cInstmType,dInstmStDt,nInstmFreq) VALUES ;
              (lcOrder,lcStore,lcPikTKt,'D',ldInvDate,1)
  STORE lnTotalChg TO lnBalance
ENDIF
lcBalColor = IIF(lnBalance>0,"RGB(255,255,255,255,0,0)","RGB(255,255,255,0,128,0)")
*B803397,1 (Start) open the instalmet files in the edit mode too.
*IF laScrMode[2]
IF laScrMode[2] OR  laScrMode[3]
*B803397,1 (End)
  *E301077,50 Inhance openning files to speed up transaction
  =gfOpenFile(gcDataDir+'ARINSTMH', gcDataDir+'ARINSTMH','SH')
  =gfOpenFile(gcDataDir+'ARINSTMD', gcDataDir+'ARINSTMD','SH')
  *E301077,50 (End)
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
*B803397,1 (Start) Close the instalment files in the edit mode.
*IF laScrMode[2]  
IF laScrMode[2] OR  laScrMode[3]
*B803397,1 (End)
  *E301077,50 Inhance openning files to speed up transaction
  =gfCloseFile('ARINSTMH')
  =gfCloseFile('ARINSTMD')
  *E301077,50 (End)
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
(Order,Store,PikTkt,cInstalNo,nInstmAmnt,nInstmPcnt) VALUES ;
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
SCAN REST WHILE Order+Store+PikTkt+cInstalNo = lcOrder+lcStore+lcPikTkt
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
  *E300817,1  Message : 40123
  *E300817,1  Installments amount cannot be greater than invoice total charge!
  *E300817,1  Button : 00000
  *E300817,1  Ok
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
  *E300817,1  Message : 40123
  *E300817,1  Installments amount cannot be greater than invoice total charge!
  *E300817,1  Button : 00000
  *E300817,1  Ok
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
IF BOF() .OR. (Order+Store+PikTkt <> lcOrder+lcStore+lcPikTkt)
  ldPrvDate = ldStartDate
  =SEEK(lcOrder+lcStore+lcPikTkt)
ELSE
  ldPrvDate = DueDate
  SKIP
ENDIF
=RLOCK()
IF DueDate < ldPrvDate
  *E300817,1  Message : 40126
  *E300817,1  Installment due date should be greater than xx/xx/xxxx
  *E300817,1  Button : 00000
  *E300817,1  Ok
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
  *E300817,1  Message : 40122
  *E300817,1  Installments start date cannot be less than invoice date!
  *E300817,1  Button : 00000
  *E300817,1  Ok
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
  *E300817,1  Message : 40123
  *E300817,1  Installments amount cannot be greater than invoice total charge!
  *E300817,1  Button : 00000
  *E300817,1  Ok
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
*! B603089 AHM 08/03/1999 Add the fractions difference to the
*!                        last installement
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

*B603089 (Start)
PRIVATE lnTotInst
lnTotInst = 0
*B603089 (End)
FOR lnCount = 1 TO lnInstNo
  INSERT INTO (lcInstLine) ;
  (Order,Store,PikTkt,cInstalNo,nInstmAmnt,DueDate,nInstmPcnt) VALUES ;
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
*B603089 (Start)
REPLACE nInstmAmnt WITH nInstmAmnt + ((lnTotalChg-lnInitAmnt)-lnTotInst)
*B603089 (End)
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
    Account+Order+cStore+PikTkt = lcAccount FOR Tran =&lcCredits..Tran
    SELECT (lcCredits)
    REPLACE Amount WITH Amount - lnApply
  ENDSCAN
  GO TOP
  SELECT (lcAppCrdt)
  =SEEK(lcAccount+lcOrder+lcStore+lcPikTkt)
  SUM REST Amount TO lnAppCrdt WHILE Account+Order+cStore+PikTkt=;
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
*E301429,1 (Start) Adjust the Amount field width 

*BROWSE FIELDS cMarker=IIF(RECNO()=lnOpenMark ,'>',' '):H=' ':R:1:W=.F.,;
              Tran   :H='Credit'  :R,;
              Batch  :H='Batch'   :R,;
              TranDate :H='Date'  :R,;
              Desc    :H='Description' :R,;
              Reference :H='Reference' :R :30,;
              Store  :H='Chk/Str#.' :R,;
              Amount :H='Amount' :P='99999999.99' :R;
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


*E301429,1 (End) 
SELECT (lcAppCrdt)
lnAppMark = RECNO()
*E301429,1 (Start) Adjust the Amount field width 
*BROWSE FIELDS cMarker=IIF(RECNO()=lnAppMark ,'>',' '):H=' ':R:1:W=.F.,;
              Tran     :H='Credit' :R,;
              Batch    :H='Batch'  :R,;
              TranDate :H='Date'   :R,;
              Desc     :H='Description' :R,;
              Reference :H='Reference' :R :30,;
              Store    :H='Chk/Str#.' :R,;
              Amount   :H='Amount' :P='99999999.99' :W=lfwAppAmnt() :V=lfvAppAmnt();
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
*E301429,1 (End) 
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
    m.Order    = lcOrder
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
=SEEK(lcInvoice,'INVHDR')
SELECT (lcAppCrdt)
=SEEK(InvHdr.Account+IIF(InvHdr.DIRECT_INV,SPACE(6),InvHdr.Order)+;
      InvHdr.Store+InvHdr.PikTkt)
SUM REST Amount TO lnAppCrdt WHILE Account+Order+cStore+PikTkt=;
InvHdr.Account+IIF(InvHdr.DIRECT_INV,SPACE(6),InvHdr.Order)+InvHdr.Store+InvHdr.PikTkt
IF ABS(lnAppCrdt) > 0  
  SELECT DEBIT
  =SEEK(InvHdr.Account+lcInvoice+SPACE(3)+DTOS(InvHdr.InvDate))
  SCATTER MEMVAR
  m.nTrnNewAmn = m.Amount - ABS(lnAppCrdt)   
  m.cShToOpn   = IIF(m.nTrnNewAmn>0,'N','Y')
  m.cStore     = InvHdr.Store
  INSERT INTO (lcAppCrdt) FROM MEMVAR

  *B602295,1 Call KEYOFF procedure from the root directory
  *DO lfKeyOff IN (gcapphome+gcwinappl+'\ARKEYOF.PRG') WITH ;
  *InvHdr.Account,InvHdr.InvDate,Debit.Amount,lnAppCrdt,lcAppCrdt
  DO lfKeyOff IN (gcapphome+'ARKEYOF.PRG') WITH ;
  InvHdr.Account,InvHdr.InvDate,Debit.Amount,lnAppCrdt,lcAppCrdt
  *B602295,1 (End)
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
*B802865,1 WAB - commet this seek and only seek if the pointer is not in the
*B802865,1 WAB - correct record
*=SEEK(lcAccount+lcOrder+lcStore+lcPikTKt,lcInvFile)
IF Account <> lcAccount OR Order <> lcOrder OR Store <> lcStore OR pikTkt <> lcPikTKt
  =SEEK(lcAccount+lcOrder+lcStore+lcPikTKt,lcInvFile)
ENDIF
*B802865,1 WAB - END

*B602958,1 AMM Allow saving zero amount
*IF &lcInvFile..SHIPAMT=0
IF &lcInvFile..SHIP = 0
*B602958,1 AMM end

  *E300817,1  Message : 40033
  *E300817,1  No invoice lines found! Cannot save.
  *E300817,1  Button : 00000
  *E300817,1  Ok
  =gfModalGen('INM40033B00000','ALERT')
  &lcSaveInv = .F.
ENDIF
IF &lcSaveInv AND !EMPTY(lcInstFile) AND SEEK(lcOrder+lcStore+lcPikTKt,lcInstFile)
  IF &lcSaveInv .AND. &lcInstFile..nInstmAmnt > &lcInvFile..TotalChg
    *E300817,1  Message : 40123
    *E300817,1  Installments amount cannot be greater than invoice total charge!
    *E300817,1  Button : 00000
    *E300817,1  Ok
    =gfModalGen('TRM40123B00000','ALERT')
    &lcSaveInv = .F.
  ENDIF
  IF &lcSaveInv .AND. &lcInstFile..dInstmStDt < &lcInvFile..InvDate
    *E300817,1  Message : 40122
    *E300817,1  Installments start date cannot be less than invoice date!
    *E300817,1  Button : 00000
    *E300817,1  Ok
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
      *E300817,1  Message : 40127
      *E300817,1  Invoice has COD amount while account xxxxx has open credits.
      *E300817,1  Are you sure you do not want to apply open credits?
      *E300817,1  Button : 40003
      *E300817,1  Proceed Cancel
      IF gfModalGen('QRM40127B40003','ALERT',lcAccount)=2  && if cancel don't save the invoice
        &lcSaveInv = .F.
      ENDIF
    ELSE
    *-- there is already applied credits
      SELECT (lcCrdtFile)
      SUM REST Amount TO lnAppCrdt WHILE Account+Order+Store+PikTkt=;
                        lcAccount+lcOrder+lcStore+lcPikTKt
      IF &lcSaveInv AND lnAppCrdt > &lcInvFile..TotalChg
      *-- Applied credits greater than total charges 
        *E300817,1  Message : 40124
        *E300817,1  Applied credits amount cannot be greater than invoice total charge!
        *E300817,1  Button : 00000
        *E300817,1  Ok
        =gfModalGen('TRM40124B00000','ALERT')
        &lcSaveInv = .F.
      ENDIF
    ENDIF
  ENDIF
ENDIF
*C102239,1 (Begin) Calculate non english charges for BEL05.
IF !EMPTY(laData[3]) AND ASCAN(laEvntTrig , PADR('CHKINV',10)) <> 0
  =gfDoTriger('ARIINV',PADR('CHKINV',10))
  RETURN
ENDIF  
*C102239,1 (End)

*C102424,1 (Begin) Save artons in artclpshp for BEL05.
IF !EMPTY(laData[3]) AND ASCAN(laEvntTrig , PADR('CHKIT',10)) <> 0
  =gfDoTriger('ARIINV',PADR('CHKIT',10))
  RETURN
ENDIF  
*C102424,1 (End)


*C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
*I force the code to get in this code to calculate the chargs.
IF ASCAN(laEvntTrig , PADR('JLINSERT',10)) <> 0
  llGetInJl = .T.
ENDIF
*C102314,1 BWA 06/10/2001 [END]

IF &lcSaveInv AND UPPER(ALLTRIM(gcContCode))<>'ENG' .AND. &lcInvFile..lCompUps
*--Calculate non english charges 
  SELECT (lcInvFile)
  SCATTER MEMVAR 
  *B801882,1 Add new parameters for charge type
  *DO gpUpsChrg WITH ;
  m.Account,m.Store,m.Order,m.PikTkt,m.cWareCode,m.ShipAmt,m.Cod_Flag='Y',;
  m.lUpsIns,m.DISCPCNT,m.TAX_RATE,m.cTaxRule,m.nPstRate,lcUpsFile,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT',;
  'm.TAX_AMT','m.nPstAmt','m.TOTALCHG',m.nTaxDue
  *-- Calculate charges
  DO gpUpsChrg WITH ;
  m.Account,m.Store,m.Order,m.PikTkt,m.cWareCode,m.ShipAmt,m.Cod_Flag='Y',;
  m.lUpsIns,m.DISCPCNT,m.TAX_RATE,m.cTaxRule,m.nPstRate,lcUpsFile,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT',;
  'm.TAX_AMT','m.nPstAmt',m.nTaxDue,'A'

  *C102212,1 (Begin) Update HST Tax.
  *m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod-ABS(m.ShipAmt*m.DiscPcnt/100)+;
               m.Tax_Amt+m.nPstAmt
  m.nHstAmt = ROUND((m.nHstRate*m.ShipAmt)/100,2)
  m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod-ABS(m.ShipAmt*m.DiscPcnt/100)+;
               m.Tax_Amt+m.nPstAmt+m.nHstAmt
  *C102212,1 (End)
  *B801882,1 (End)


  SELECT (lcInvFile)

  *C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
  *m.lCompUps = .F.
  IF ASCAN(laEvntTrig , PADR('LASTSCRN',10)) <> 0
    =gfDoTriger('ARIINV',PADR('LASTSCRN',10))
  ELSE
    m.lCompUps = .F.
  ENDIF
  *C102314,1 BWA 06/10/2001 [END]


  =RLOCK()
  GATHER MEMVAR
  UNLOCK
ENDIF

*C102314,1 BWA 06/10/2001 Add a triger functions for the custome charges screen.[START]
IF ASCAN(laEvntTrig , PADR('LASTSCRN',10)) <> 0
  IF llGetInJl
    =gfDoTriger('ARIINV',PADR('LASTSCRN',10))
    lcAliasB = SELECT(0)
    SELECT(lcInvFile)
    REPLACE &lcInvFile..TOTALCHG WITH M.TOTALCHG
    SELECT(lcAliasB)
  ENDIF
ENDIF
*C102314,1 BWA 06/10/2001 [END]



IF &lcSaveInv AND SEEK('O'+Order,'ORDHDR') .AND. OrdHdr.ApprAmt > 0 .AND. ;
   &lcInvFile..TotalChg > OrdHdr.ApprAmt
  *E300817,1  Message : 40119
  *E300817,1  Invoice Amount Exceeds Order Approved Amount By 999999.
  *E300817,1  Button : 40003
  *E300817,1  Proceed Cancel
  
  *B605717,1 check the Approved amount against shipped amount only. [Begin]
  *IF gfModalGen('TRM40119B40003','ALERT',ALLTRIM(STR(&lcInvFile..TotalChg-OrdHdr.ApprAmt,9,2)))=2
  IF gfModalGen('TRM40119B40003','ALERT',ALLTRIM(STR(&lcInvFile..ShipAmt-OrdHdr.ApprAmt,9,2)))=2
  *B605717,1 check the Approved amount against shipped amount only. [End]

    &lcSaveInv = .F.
  ENDIF
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : gfAcOpCrdt
*! Developer : WAM
*! Date      : 02/19/1999
*! Purpose   : Check if customer has open credits    *E301077,50
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
*E301245,1
FUNCTION lfHgWUpdat
=RLOCK()
REPLACE nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
UNLOCK
*-- End of lfHgWUpdat.

*!**************************************************************************
*! Name      : lfUpCdFild
*! Developer : Mohamed Atia Badran Sameh (MAB)
*! Date      : 07/29/1999
*! Purpose   : Update TotCod Field avoiding Alias not found bug.
*!*************************************************************************
*! Example   : =lfUpCdFild()
*!******l******************************************************************
*B802456,1
FUNCTION lfUpCdFild
PRIVATE lnCurrAlis
lnCurrAlis = SELECT(0)
SELECT (lcUpsFile)
REPLACE TotalCod WITH &lcCOD_AMT
SELECT (lnCurrAlis)
*-- end of lfUpCdFild.



