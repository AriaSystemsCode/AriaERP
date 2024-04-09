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
*!                       lcAppCrdt  : Credits applied againest this invoice
*!                       lcGlSession: GL Session Number
*!                       laInvoices : Invoice Numbers
*!                       llFromEdi  : Caled From Edi Module
*!**************************************************************************
*! Returns            :  lcInvNo    : Invoice Number
*!**************************************************************************
*! Example            :  =gpSaveInv(lcInvHdr,lcInvLine,lcUpsFile,lcEngChrg,;
*!                                  '000123','')
*!**************************************************************************
*! Modifications      :
*!
*!* N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char
*!* B130201,1 HBG 14/02/2006 Update cost in INVLINE for consolidated invoices
*!* C200725,1 MMT 12/13/2006 Add Customization A'dd more charges screen'
*!* E302350,1 MMT 01/11/2007 Enhancement to add Deposit Screen
*!* B607983,1 AYM 22/02/2007  -T20070108.0145
*!* B607983,1 AYM *Problem of not saving to pack hdr in some cases (EB NOT INSTALLED,BOL TYPE IS '856)
*!* B608097,1 MMT 05/24/2007 fix bug of error while saving Invoice Sales Order[T20070522.0022]
*!* B608253,1 MMT 09/05/07 add log file to check the problem of not saving debit record[T20070820.0010]
*!* B608258,1 MMT 09/06/2007 don't Add record in debit file for Zero Amount invoices   [T20070711.0003]
*!* B608267,1 WAM 09/16/07 Stop updae customer balance after void invoice. It is already updated by the KEYOFF class
*!* B608288,1 MMT 09/25/2007 Update Invoice and Invdate fields in pikline file         [T20070822.0002]
*!* B608314,1 MMT 10/11/2007 fix bug of wrong Ship Amt In Commession report			   [T20071002.0014]
*!* B608326,1 WAM 10/23/2007 Do not add records for unshiped lines in PIKLINE file     [T20071022.0054]
*!* B608449,1 MMT 02/24/2008 fix bug of wrong total charges value					   [T20080107.0005]
*!* C200876,1 TMI 05/29/2008 Adding the triggers used by BIN Location custom
*!* B608576,1 WAM 05/29/2008 When void invoice, Key-off only debited invoices [T20080523.0004]
*!* B608664,1 MMT 08/26/2008 Fix bug of updating npck fields in Ordline File  [T20080821.0023]
*!* C201063,1 MMT 11/26/2008 Convert Custom Cartons Program of MEM10 to Aria4 [T20080908.0 001]
*!* B608826,1 TMI 18/03/2009 modify trigger places to meet the consolidated invoice case, remove the 'GFSTYCRL' trigger
*!* E302590,1 MMT 03/31/2009 Update TrnHist file when invoice created [T20070214.0006]
*!* B608846,1 MMT 04/14/2009 change position of MEMO trigger as it causes wrong Saved Qty in invline[T20090316.0009]
*!* B608846,1 MMT 04/14/2009 Make invoice saving update npck Fields in ordline[T20090317.0068]
*!* B608892,1 WAM 06/11/2009 Fix sales rep commission percent calculation for consolidated invoices [T20090514.0030]
*:* E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[T20090605.0009]
*!* B608948,1 MMT 07/28/2009 Update Trnhist Table when Invoice is Voided[T20090715.0001]
*!* B608989,1 MMT 08/30/2009 invline table is not update in case of consolidate invoice{T20090827.0018}
*!* B609102,1 HES 12/03/2009 Fix bug of Invoice due date not calculated correctly when terms EOM set to Yes [T20091117.0007]
*!* B609104,1 HES 12/06/2009 PO receipt update GLDIST incorrectly using Standard Cost [T20090818.0006]
*!* B609102,3 TMI 02/17/2010 take into account the case of Feb when it be 28 days and the EOM = 30
*!* B609162,1 TMI 03/08/2010 fix problem STYLE object not found that caused by the fix # B609104
*!* B609168,1 HES 03/14/2010 NEW UPS RATES FOR SYSTEM dbf [T20100131.0027]
*!* B609269,1 HES 05/25/2010 CARTONS Button displays data not related to the selecte PACK line! [T20100316.0028]
*!* B609302,1 MMT 06/15/2010 Fix bug of Error while voiding Invoice from Voiding multiple invoice program[T20100526.0015]
*!* E302726,1 MMT 07/28/2010 Empty Field CCARTRCKNO in case of Consolidated invoice[T20100205.0005]
*!* E302780,1 MMT 10/27/2010 Modify Invoice and Invoice SO screen to apply trade discount per line[T20100120.0060]
*!* B609505,1 TMI 01/18/2011 do not calculate chanrges for type USPS [T20101213.0020]
*!* B609521,1 MMT 02/22/2011 Update Comm1 & Comm2 fields in invline for consoldiated inv.[T20101119.0005]
*!* B609557,1 WAM 03/24/2011 Fix updating GST Tax GL Account when Void invoice for Canadian customers [T20110323.0018]
*!* E302947,1 WAM 07/27/2011 Calculate UPS charges only if the ship via related field CARRIER ID is set to UPS and the related field
*!* E302947,1 WAM 07/27/2011  "Include Freight charges" is set to 'YES' [T20101207.0006]
*!* B609676,1 MMT 09/27/2011 Fix bug of Error 'alias CARRIER_SHIPMENT_T is not found' at customers that are not using Carriers Intergration[T20101207.0006]
*!* B609723,1 WAM 11/01/2011 Fix updating status in CONSINVH file while voiding consolidated invoice [T20111024.0029]
*!* B609730,1 WAM 11/14/2011 Fix updating sales rep commission percent for consolidated invoices in INVHDR table [T20111017.0039]
*!* B609763,1 MMT 12/08/2011 Change Invoice saving to change PL status to complete[T20111025.0013]
*!* B609784,1 MMT 12/26/2011 Fix bug of updating Dsc_amt in debit file based on Shipped amount not total charges[T20111210.0001]
*!* B609803,1 MMT 01/24/2012 Packing list status is complete even after voiding invoice  [T20120110.0018]
*!* E303048,1 MMT 01/29/2012 Add triggers in Invoice saving prg for [T20110621.0057]
*!* B609880,1 HIA 03/28/2012 update wght..., in case of llcarrchagrs [T20120308.0017]
*!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[T20120402.0004]
*!* B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[T20120206.0037]
*!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035]
*!* B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035]
*!* B610183,1 HIA 12/30/2012 Commission calculation error [T20121130.0008]
*!* B610259,1 MMT 02/26/2013 Invoice SO saving hanges if more than consolidated invoice for different divisions[T20130226.0001]
*!* B610282,1 HIA 03/24/2013 Aria XP - Failing to create and save invoices in Aria 4XP, T20130305.0011
*!* B610283,1 HIA 03/24/2013 Aria XP - Consolidated invoice [T20130109.0024]
*!* E303386,1 TMI 05/09/2013 include the CHAGES in VAT calculation for UK [t20130214.0010]
*!* B610344,1 HIA 05/29/13 T20130509.0009 Trade Discount Miscalculations
*!* B610455,1 HIA 07/31/13 T20130620.0006 - CRM/AR - Transhst records not being deleted when an invoice is voided
*!* B610526,1 TMI 09/24/2013 Problem while opening SQL table [T20130916.0004]
*!* E303420,1 MMT 09/26/2013 Add option to Consolidate invoices by order - store in Invoice sales order screen[T20130814.0004]
*!* E610617,1 TMI 12/09/2013 Modify function gpUpsChrg to pad the variable lcFromZone to 3 characters after filling it from the setup XUPSFROM [T20131018.0006]
*!* E303504,1 MMT 09/07/2014 Add triggers to consolidate invoice by contract reference[T20140819.0035]
*!* B610963,1 MMT 03/11/2015 Invoice updates neqvamnt incorrectly in GLDIST and wrong invline record in case of consolidated invoice[T20150310.0013]
*!* B610989,1 MMT 04/20/2015 Fix the issue of wrong VAT calculations in invoice screen[T20150319.0010]
*!* B611015,1 MMT 06/10/2015 Incorrect GL account for PST taxes while voiding invoices[T20150528.0014]
*!* B611016,1 MMT 06/15/2015 Invoice saving calculates Trade discount incorrectly for UK companies[T20150602.0002]
*!* B611089,1 MMT 11/30/2015 Incorrect Style Cost value saved in INVLINE after creating invoice[T20151029.0011]
*!* B611419,1 MHM 11/10/2017 Incorrect Totqty of repeated Styles in Consolidated invoice [T20170728.0004]
*!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
*!* B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045]
*!* B611475,1 MHM  ISSUE #10: FREIGHT AND TRACKING NOT GETTING ADDED WHEN PROCESSING WHILE INVOICING
*!* B611487,1 AHH 12/12/2017  ISSUE #9 Insurance and Freight Disappear After Processing [T20171114.0031]
*!* E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL 
*!* B611614,1 Es 07/11/2018 Modify this issue -> "User cannot re-ship voided cartons that have voided tracking#" [T20180704.0004]
*!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL 
*!* B611719,1 Es 02/19/2019 Modify issue "Duplicated GL entries for Category 001 in GLDIST" [T20190215.0002]
*!* B611772,1 Es 05/02/2019 Direct invoice voiding adds repeated record in GLDIST and STYINVJL if voiding crashed and user tried to void more than once. [T20190320.0001]
*!* B611924,1 ES 10/17/2019 Bug in Invoice Saving program	[T20190409.0005]
*!* E611821,1 MMT 11/13/2019 Call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement]
*!* B611827,1  Es 03/22/2020 Random error while creating invoice sales order, Alias not found. [T20200226.0007]
*!* B612262,1 MMT 11/23/2020 While voiding in Invoice screen, User can enter Void date less than the Invoice date[T20201110.0001]
*!* E612339,1 MMT 02/10/2021 Invoice Saving does not update Entity table when Invoice is created/Voided [T20201008.0003]
*!* B612427,1 MMT 07/06/2021 Random Error While Invoicing SO while reading UPS charges[T20210603.0003]
*!* E612469,1 MMT 09/13/2021 Add dropship record in EDITRANS[P20210827.0001]
*!* B612564,1 MMT 05/18/2022 When the same user edits SO has a Picking ticket, then Invoiced this SO, and then saved the SO, the SO status became open again while it should be completed[T20220511.0004]
*!***************************************************************************************************************************
FUNCTION gpSaveInv
  *T20060804.0022,MMT,08/30/2006 Convert UPS integration to Aria4xp[Start]
  PARAMETERS lcHdrFile,lcDetFile,lcUpsFile,lcEngChrg,lcInstHdr,;
    lcInstLine,lcOrdCanLn,lcAppCrdt,lcGlSession,laInvoices,llFromEdi,llConsByDc,loFormSet
  *PARAMETERS lcHdrFile,lcDetFile,lcUpsFile,lcEngChrg,lcInstHdr,;
  lcInstLine,lcOrdCanLn,lcAppCrdt,lcGlSession,laInvoices,llFromEdi,llConsByDc
  *T20060804.0022,MMT,08/30/2006 Convert UPS integration to Aria4xp[End]
  *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
  #Include R:\aria4xp\screens\ar\ardinv.h
 
 
  IF oAriaApplication.oActivelang.cLang_ID <> "EN"
    LOCAL lcInvHeaderAlias
    lcInvHeaderAlias=oAriaApplication.GetClassHeaderFile(ADDBS(UPPER(ALLTRIM(oAriaApplication.LangPath))) + "SCREENS\AR\" + ALLTRIM("ARDINV")+"_"+"H" +".XML")
  ENDIF
  *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
  *B128066,1 AMH Fix bug of file is used by another user [Start]
  LOCAL lnRePro
  lnRePro = SET("Reprocess")
  SET REPROCESS TO -1
  *B128066,1 AMH [End]

  PRIVATE laInvSetup,laDRltFld,lcCustLink,lcCustSales,lcUntSin,lnInvCount,;
    lcExRSin,lcGlYear,lcGlPeriod,lnNetAmnt,lnComm1,lnComm2,lnNetShipAmnt,;
    lnCOGSAmt,lnAlias,lcDistFile,;
    lcInvNo,lcOrdNo,llOrdExist,laGlArray,laAdjStk
  lcSysType = gfGetMemVar('M_SYSTYPE',oAriaApplication.ActiveCompanyID)
  DECLARE laGlArray[2,13]
  lnAlias = SELECT()
  *!* E302780,1 MMT 10/27/2010 Modify Invoice and Invoice SO screen to apply trade discount per line[Start]
  *DECLARE laInvSetup[10,2]
  DECLARE laInvSetup[11,2]
  *!* E302780,1 MMT 10/27/2010 Modify Invoice and Invoice SO screen to apply trade discount per line[ENd]
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
  *!* E302780,1 MMT 10/27/2010 Modify Invoice and Invoice SO screen to apply trade discount per line[Start]
  laInvSetup[11,1]= "M_TRDDISCL"		 && Apply Tarde Discount per line
  *!* E302780,1 MMT 10/27/2010 Modify Invoice and Invoice SO screen to apply trade discount per line[End]
  =gfGetMemVar(@laInvSetup,oAriaApplication.ActiveCompanyID)
  *-- array to hold the division related fields
  IF laInvSetup[5,2]='Y'
    DECLARE laDRltFld[2,2]
    laDRltFld[1,1] = 'LINK_CODE'
    laDRltFld[1,2] = 'lcCustLink'        && Customer link
    laDRltFld[2,1] = 'CSLSGLLINK'
    laDRltFld[2,2] = 'lcCustSales'       && Custmer sales link code
  ENDIF


  *!* B608253,1 MMT 09/05/07 add log file to check the problem of not saving debit record[Start]
  lcTmpTxtFl = gfTempName()
  lcStrVar = ""
  *!* B608253,1 MMT 09/05/07 add log file to check the problem of not saving debit record[End]

  *-- Open neccessary files
  =IIF('AL' $ oAriaApplication.CompanySetupModules,gfOpenFile(oAriaApplication.DataDir+'PIKLINE',oAriaApplication.DataDir+'PIKLINE','SH'),.T.)
*!* E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL 
*  =IIF(laInvSetup[1,2]='Y',gfOpenFile(oAriaApplication.DataDir+'GLDIST', oAriaApplication.DataDir+'GLDISTNO', 'SH'),.T.)
 =IIF(laInvSetup[1,2]='Y',gfOpenTable(oAriaApplication.DataDir+'GLDIST', oAriaApplication.DataDir+'GLDISTNO', 'SH'),.T.)
*!* E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL 

  *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
  *=gfOpenFile(oAriaApplication.DataDir+'icStyHst', oAriaApplication.DataDir+'Styhst','SH')
  =gfOpenTable('ICSTYHST','STYHST','SH')
  *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]

  =gfOpenFile(oAriaApplication.DataDir+'arCusHst', oAriaApplication.DataDir+'Acthst','SH')
  =gfOpenFile(oAriaApplication.DataDir+'DEBIT'   , oAriaApplication.DataDir+'DEBIT','SH')
  =gfOpenFile(oAriaApplication.DataDir+'CONSINVH', oAriaApplication.DataDir+'CONSINVH','SH')
  =gfOpenFile(oAriaApplication.DataDir+'CONSINVL', oAriaApplication.DataDir+'CONSINVL','SH')
  =gfOpenFile(oAriaApplication.DataDir+'REPCOMM' , oAriaApplication.DataDir+'REPCOMM','SH')
  =gfOpenFile(oAriaApplication.DataDir+'SALESREP', oAriaApplication.DataDir+'SALESREP','SH')
  IF !EMPTY(lcOrdCanLn)
    =gfOpenFile(oAriaApplication.DataDir+'ORDCANLN', oAriaApplication.DataDir+'ORDCANLN','SH')
  ENDIF
  =gfOpenFile(oAriaApplication.DataDir+'UPSBOX', oAriaApplication.DataDir+'UPSBOX','SH')
  =gfOpenFile(oAriaApplication.DataDir+'GL_LINK', oAriaApplication.DataDir+'GL_LINK','SH')
  IF ('EB' $ oAriaApplication.CompanySetupModules OR 'NC' $ oAriaApplication.CompanySetupModules )
    =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT','SH')
    =gfOpenFile(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
    =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
  ENDIF
  IF 'EB' $ oAriaApplication.CompanySetupModules
    =gfOpenFile(oAriaApplication.DataDir+'PACK_HDR',oAriaApplication.DataDir+'Orderpck','SH')
    =gfOpenFile(oAriaApplication.DataDir+'BOL_HDR',oAriaApplication.DataDir+'BOL_HDR','SH')
  ENDIF
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET ORDER TO TAG DEBIT IN DEBIT
  lnInvCount = 1    && Variable to hold the number of invoices
  SET ORDER TO TAG (lcDetFile) IN (lcDetFile)
  *HBG 09/07/2004 Clear any filter on the installemnt files before save [Begin]
  IF !EMPTY(lcInstHdr) AND USED(lcInstHdr)
    SELECT (lcInstHdr)
    SET KEY TO
  ENDIF
  IF !EMPTY(lcInstLine) AND USED(lcInstLine)
    SELECT (lcInstLine)
    SET KEY TO
  ENDIF
  *HBG [End]
  lcTmpPikLn = gfTempName()
  SELECT (lcHdrFile)
  *HBG 08/25/2004 If consalidated by DC use the correct index [Begin]
  IF llConsByDc
    SET ORDER TO TAG ConsDist
  ELSE
    SET ORDER TO TAG (lcHdrFile)
  ENDIF
  *HBG [End]

  SCAN
    *-- Direct invoice
    *--   In the invhdr file you will find
    *--     One line
    *--     Fields   Direct_inv = .T.  ,Consol = N  ,Flag = ' '  llConsAcc=.T.
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
    llConsAcc = &lcHdrFile..Direct_Inv OR &lcHdrFile..CONSOL='Y' OR EMPTY(&lcHdrFile..FLAG)
    IF llConsAcc
      *-- Get The Invoice number on the factor sequence if the invoice was factored
      *-- give the invoice the same invoice no. for the pre-Billed one.
      IF ASCAN(oAriaApplication.laEvntTrig , PADR('SADSEQ',10)) <> 0
        lcInvNo=""
        =gfDoTriger('SDIINV',PADR('SADSEQ',10))
      ELSE
        lcInvNo = IIF(EMPTY(&lcHdrFile..cFacCode),;
          gfSequence('INVOICE'  ,'','',&lcHdrFile..cDivision),;
          gfSequence('CFINVOICE','','',&lcHdrFile..cDivision))
      ENDIF
      DIMENSION &laInvoices[lnInvCount]
      &laInvoices[lnInvCount] = lcInvNo
      lnInvCount = lnInvCount + 1

      *-- Get The order number manualy or using gfsequence depend on the invoice setup
      IF Direct_Inv
        lcOrdNo = IIF(laInvSetup[10,2]='Y',&lcHdrFile..ORDER,;
          gfSequence('ORDER','','',&lcHdrFile..cDivision))
        REPLACE ORDER WITH '' IN (lcHdrFile)
      ELSE
        lcOrdNo = &lcHdrFile..ORDER
      ENDIF
    ELSE
      lcOrdNo = &lcHdrFile..ORDER
    ENDIF
    SELECT (lcHdrFile)
    =RLOCK()
    REPLACE nExRate   WITH IIF(nExRate=0,1,nExRate) ,;
      nCurrUnit WITH IIF(nCurrUnit=0,1,nCurrUnit) ,;
      LastLine  WITH IIF(SEEK('O'+IIF(!EMPTY(lcOrdNo),lcOrdNo,&lcHdrFile..ORDER),'ORDLINE'),LastLine,0),;
      Invoice   WITH lcInvNo
    UNLOCK

    *--Check Post Date if valid  in the gl period or not
    STORE '' TO lcGlYear,lcGlPeriod
    =CHECKPRD(&lcHdrFile..dPostDate,'lcGlYear','lcGlPeriod','IN',.T.)

    *-- the system linked to GL creat temp GL file
    IF laInvSetup[1,2]='Y'
      lcDistFile = gfTempName()
      SELECT GLDIST
      =AFIELDS(laFileStru)
      
*B611924,1 ES 10/17/2019 Bug in Invoice Saving program	[T20190409.0005][Start]
      *=gfCrtTmp(lcDistFile,@laFileStru,.F.,'')
       =gfCrtTmp(lcDistFile,@laFileStru,'Tran_no+catg_key',lcDistFile)
*B611924,1 ES 10/17/2019 Bug in Invoice Saving program	[T20190409.0005][End]

    ENDIF
    *-- lnNetAmnt  total amount per line to calculate the sales rep commission
    *-- lnComm1,lnComm2 sales rep commissions
    STORE 0  TO lnNetAmnt,lnComm1,lnComm2,lnNetShipAmnt,lnCOGSAmt
    STORE '' TO lcCustLink,lcCustSales
    *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
    =SEEK('M'+&lcHdrFile..Account,'Customer')
    lnTotAmntComm = 0
    *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
    *-- Get Customer & Sales Link codes
    PRIVATE lcCDefLnk,lcSDefLnk
    lcCDefLnk = IIF(EMPTY(Customer.Link_Code) ,'DEFDEF',Customer.Link_Code)
    lcSDefLnk = IIF(EMPTY(Customer.cSlsGlLink),'DEF'   ,Customer.cSlsGlLink)
    IF laInvSetup[5,2]='Y'
      *-- the GL link was at the division level get the link code of the division
      =gfRltFld(&lcHdrFile..cDivision,@laDRltFld,'CDIVISION')
      lcCustLink  = IIF(EMPTY(lcCustLink) ,lcCDefLnk,PADR(lcCustLink,6))
      lcCustSales = IIF(EMPTY(lcCustSales),lcSDefLnk,PADR(lcCustSales,3))
    ELSE
      lcCustLink  = lcCDefLnk
      lcCustSales = lcSDefLnk
    ENDIF
    *-- Get Factor customer link code
    IF !EMPTY(&lcHdrFile..cFacCode)
      =gfOpenFile(oAriaApplication.DataDir+'Factor',oAriaApplication.DataDir+'Factor','SH')
      IF SEEK(&lcHdrFile..cFacCode,'Factor')
        lcCustLink =IIF(!EMPTY(Factor.Link_Code),Factor.Link_Code,lcCustLink)
      ENDIF
    ENDIF
    *-- Define Arry to hold GL information needed by gfStyCntl function
    STORE '' TO laGlArray

    IF laInvSetup[1,2]='Y' .AND. llConsAcc
      STORE 'IN'                  TO laGlArray[1,4] ,laGlArray[2,4]
      STORE lcInvNo               TO laGlArray[1,5] ,laGlArray[2,5]  && invoice number
      STORE &lcHdrFile..dPostDate TO laGlArray[1,6] ,laGlArray[2,6]  && Post Date
      STORE lcGlYear              TO laGlArray[1,7] ,laGlArray[2,7]  && Year
      STORE lcGlPeriod            TO laGlArray[1,8] ,laGlArray[2,8]  && GlPeriod
      STORE lcDistFile            TO laGlArray[1,9] ,laGlArray[2,9]  && glDist temp file
      STORE ''                    TO laGlArray[1,10],laGlArray[2,10] &&
      STORE &lcHdrFile..cCurrCode TO laGlArray[1,11],laGlArray[2,11] && Currency
      STORE &lcHdrFile..nCurrUnit TO laGlArray[1,12],laGlArray[2,12] && Currency unit
      STORE &lcHdrFile..nExRate   TO laGlArray[1,13],laGlArray[2,13] && Exchange rate
    ENDIF

    lcUntSin   = ''
    *-- lcExRSin  Exchange Rate sign "/" Or "*"
    lcExRSin   = gfGetExSin(@lcUntSin,&lcHdrFile..cCurrCode)
    *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[START]
    *llOrdExist = SEEK('O'+lcOrdNo,'ORDHDR') && In case of sales order invoice
    llOrdExist = SEEK('O'+lcOrdNo,'ORDHDR','ORDHDR') && In case of sales order invoice
    *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[END]
    *-- get the customer link code from the order header in case of sales order invoice
    lcCustLink = IIF(EMPTY(ORDHDR.Link_Code),lcCustLink,ORDHDR.Link_Code)


    *B611334MHM - move it before bol update (begin)
    IF !USED('PACK_HDR')
      =gfOpenFile(oAriaApplication.DataDir+'PACK_HDR',oAriaApplication.DataDir+'Orderpck','SH')
    ENDIF
    IF !&lcHdrFile..Direct_Inv AND ;
        SEEK(&lcHdrFile..ORDER+&lcHdrFile..STORE+&lcHdrFile..PikTkt,'PACK_HDR')
      *!* B609763,1 MMT 12/08/2011 Change Invoice saving to change PL status to complete[End]
      *B608097,1 MMT 05/24/2007 fix bug of error while saving Invoice Sales Order [End]
      *B611334 MHM - Update Packing list ship date and status if its status is not complete only (begin)
      IF PACK_HDR.STATUS <> "C"
        *B611334 HM - (end)
        REPLACE Ship_Date WITH oAriaApplication.SystemDate ,;
          STATUS    WITH 'C' IN PACK_HDR
      ENDIF
    ENDIF
    *B611334 - (end)
    *-- Ship BOL generated for invoiced packing list
    *-- If the EDI module is installed and the invoice is a Direct invoice
    IF 'EB' $ oAriaApplication.CompanySetupModules AND !&lcHdrFile..Direct_Inv AND ;
        SEEK('A'+&lcHdrFile..Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'856','EDIPD') AND ;
        SEEK(&lcHdrFile..ORDER+&lcHdrFile..STORE+&lcHdrFile..PikTkt,'PACK_HDR') AND ;
        SEEK(PACK_HDR.Bill_Ladg,'BOL_HDR')
      IF BOL_HDR.STATUS <> "C"
        *B611334 - update the BOL Status and ship date only if the BOL status is not complete and all Packing lists included in the BOL are invoiced. (begin)
        =gfOpenFile(oAriaApplication.DataDir+'BOL_LIN',oAriaApplication.DataDir+'BOL_LIN','SH')
        IF SEEK(BOL_HDR.BOL_No,'BOL_LIN')
          SELECT BOL_LIN
          llPack_Comp = .T.
          SCAN REST WHILE BOL_No = BOL_HDR.BOL_No
            IF SEEK(Pack_No,'Pack_hdr','Pack_hdr')
              IF PACK_HDR.STATUS <> "C"
                llPack_Comp = .F.
                EXIT
              ENDIF
            ENDIF
          ENDSCAN
        ENDIF
        IF llPack_Comp = .T.
          *B611334 - end
          REPLACE STATUS    WITH 'C' ,;
            Ship_Date WITH oAriaApplication.SystemDate ,;
            cShipTime WITH TIME() IN BOL_HDR
          IF !SEEK('856'+PADR(PACK_HDR.Bill_Ladg,40)+'A'+&lcHdrFile..Account,'EDITRANS')
            *N037401,1 [End]
            INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER) VALUES ;
              ('856',PACK_HDR.Bill_Ladg,'A',&lcHdrFile..Account)
          ENDIF
          REPLACE cStatus WITH 'N' IN EDITRANS
          =gfAdd_Info('EDITRANS')
          *B611334 MHM - begin
        ENDIF
        *B611334 MHM - end
        *N037401,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
        *IF !SEEK('856'+PADR(PACK_HDR.Bill_Ladg,20)+'A'+&lcHdrFile..Account,'EDITRANS')

      ENDIF
      *B607983,1 -T20070108.0145 AYM 22/02/2007
      *Problem of not saving to pack hdr in some cases (EB NOT INSTALLED,BOL TYPE IS '856) [Begin]

      *!*	    REPLACE Ship_Date WITH oAriaApplication.SystemDate ,;
      *!*	            Status    WITH 'C' IN PACK_HDR
      *B607983,1 -T20070108.0145 AYM 22/02/2007
      *Problem of not saving to pack hdr in some cases (EB NOT INSTALLED,BOL TYPE IS '856) [END]

    ENDIF
    *B607983,1 -T20070108.0145 AYM 22/02/2007
    *Problem of not saving to pack hdr in some cases (EB NOT INSTALLED,BOL TYPE IS '856) [Begin]
    *B608097,1 MMT 05/24/2007 fix bug of error while saving Invoice Sales Order [Start]
    *IF !&lcHdrFile..Direct_Inv AND ;
    SEEK(&lcHdrFile..Order+&lcHdrFile..Store+&lcHdrFile..PikTkt,'PACK_HDR') AND ;
    SEEK(Pack_Hdr.Bill_Ladg,'BOL_HDR')
    *!* B609763,1 MMT 12/08/2011 Change Invoice saving to change PL status to complete[Start]
    *!*    IF !&lcHdrFile..Direct_Inv AND USED('PACK_HDR') AND  ;
    *!*      SEEK(&lcHdrFile..Order+&lcHdrFile..Store+&lcHdrFile..PikTkt,'PACK_HDR') AND USED('BOL_HDR') AND ;
    *!*      SEEK(Pack_Hdr.Bill_Ladg,'BOL_HDR')
    *ash - move it before bol update (begin)
    *!*	  IF !USED('PACK_HDR')
    *!*	    =gfOpenFile(oAriaApplication.DataDir+'PACK_HDR',oAriaApplication.DataDir+'Orderpck','SH')
    *!*	  ENDIF
    *!*	  IF !&lcHdrFile..Direct_Inv AND ;
    *!*	      SEEK(&lcHdrFile..ORDER+&lcHdrFile..STORE+&lcHdrFile..PikTkt,'PACK_HDR')
    *!*	    *!* B609763,1 MMT 12/08/2011 Change Invoice saving to change PL status to complete[End]
    *!*	    *B608097,1 MMT 05/24/2007 fix bug of error while saving Invoice Sales Order [End]

    *!*	    REPLACE Ship_Date WITH oAriaApplication.SystemDate ,;
    *!*	      STATUS    WITH 'C' IN PACK_HDR
    *!*	  ENDIF
    *ash (end)
    *B607983,1 -T20070108.0145 AYM 22/02/2007
    *Problem of not saving to pack hdr in some cases (EB NOT INSTALLED,BOL TYPE IS '856) [END]
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
    *WAIT 'Updating Invoice Lines...' WINDOW NOWAIT
    WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATEINVLINE,oAriaApplication.GetHeaderText("LANG_UPDATEINVLINE",lcInvHeaderAlias)) WINDOW NOWAIT
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
    *HBG 08/01/2004 Include case of consalidated by DC [Begin]
    *!*	  SELECT (lcDetFile)
    *!*	  =SEEK(&lcHdrFile..Account+&lcHdrFile..Order+&lcHdrFile..Store+&lcHdrFile..PikTkt)
    *!*	  SCAN REST WHILE Account+Order+Store+PikTkt = &lcHdrFile..Account+&lcHdrFile..Order+&lcHdrFile..Store+&lcHdrFile..PikTkt
    *E303420,1 MMT 09/26/2013 Add option to Consolidate invoices by order - store in Invoice sales order screen[T20130814.0004][Start]
    *llConsDist = !&lcHdrFile..Direct_Inv AND SEEK('M'+&lcHdrFile..Account,'Customer') AND Customer.ConByDc ='Y'
    llConsDist = !&lcHdrFile..Direct_Inv AND SEEK('M'+&lcHdrFile..Account,'Customer') AND Customer.ConByDc $ 'SY'
    llConsStore = !&lcHdrFile..Direct_Inv AND SEEK('M'+&lcHdrFile..Account,'Customer') AND Customer.ConByDc ='S'
    *E303420,1 MMT 09/26/2013 Add option to Consolidate invoices by order - store in Invoice sales order screen[T20130814.0004][End]
    *!* E303504,1 MMT 09/07/2014 Add triggers to consolidate invoice by contract reference[T20140819.0035][Start]
    IF ASCAN(loFormSet.laEvntTrig,PADR('CHCKCONFLG',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
      =loFormSet.mDoTrigger(PADR('CHCKCONFLG',10))
    ENDIF
    *!* E303504,1 MMT 09/07/2014 Add triggers to consolidate invoice by contract reference[T20140819.0035][End]
    SELECT (lcDetFile)
    IF llConsDist
      SET ORDER TO ConsDist
      lcExpr    = "Account+Dist_Ctr+Order+Store+PikTkt"
      lcSeekKey = &lcHdrFile..Account+&lcHdrFile..Dist_Ctr+&lcHdrFile..ORDER+&lcHdrFile..STORE+&lcHdrFile..PikTkt
    ELSE
      SET ORDER TO (lcDetFile)
      lcExpr    = "Account+Order+Store+PikTkt"
      lcSeekKey = &lcHdrFile..Account+&lcHdrFile..ORDER+&lcHdrFile..STORE+&lcHdrFile..PikTkt
    ENDIF
    SET KEY TO


    *!* B608449,1 MMT 02/24/2008 fix bug of wrong total charges value[Start]

    *B608892,1 WAM 06/11/2009 Use another variable name since lnShipAmnt already used
    *lnShipAmnt = 0
    lnLShipAmnt = 0
    *B608892,1 WAM 06/11/2009 (End)

    lnShipQty = 0
    *!* B608449,1 MMT 02/24/2008 fix bug of wrong total charges value[End]
    *!* B609784,1 MMT 12/26/2011 Fix bug of updating Dsc_amt in debit file based on Shipped amount not total charges[Start]
    lnDiscAmt = 0
    *!* B609784,1 MMT 12/26/2011 Fix bug of updating Dsc_amt in debit file based on Shipped amount not total charges[END]
    =SEEK(lcSeekKey)
    SCAN REST WHILE &lcExpr = lcSeekKey
      *HBG [End]
      IF ASCAN(oAriaApplication.laEvntTrig , PADR('INVOVR',10)) <> 0
        =gfDoTriger('ARIINV',PADR('INVOVR',10))
        EXIT
      ENDIF

      *B608826,1 TMI 25/03/2009 [Start] move this after the calling of gfStyCrl to use the lines added to STYINVJL
      *-                                also include within its process the functionality of DLARBIN
      **C200876,1 TMI [Start] Update Data to Bin Location Files( WHBINLOC,PKBINLOC,IVBINLOC )
      *IF ASCAN(loFormSet.laEvntTrig , PADR('ALSAVINV',10)) <> 0
      *  =loFormSet.mDoTrigger(PADR('ALSAVINV',10))
      *ENDIF
      **C200876,1 TMI [End  ]
      *B608826,1 TMI 25/03/2009 [End  ]

      SCATTER MEMVAR MEMO
      *!* B609521,1 MMT 02/22/2011 Update Comm1 & Comm2 fields in invline for consoldiated inv.[Start]
      *m.Comm1 = IIF(laInvSetup[6,2]='Y',m.Comm1,&lcHdrFile..Comm1)
      *m.Comm2 = IIF(laInvSetup[6,2]='Y',m.Comm2,&lcHdrFile..Comm2)
      *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
      *!*	    m.Comm1 = IIF(laInvSetup[6,2]='Y' OR m.CONSOL = 'Y',m.Comm1,&lcHdrFile..Comm1)
      *!*	    m.Comm2 = IIF(laInvSetup[6,2]='Y' OR m.CONSOL = 'Y',m.Comm2,&lcHdrFile..Comm2)
      *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
      *!* B609521,1 MMT 02/22/2011 Update Comm1 & Comm2 fields in invline for consoldiated inv.[End]

      *!* B608989,1 MMT 08/30/2009 invline table is not update in case of consolidate invoice{Start}
      IF m.CONSOL = 'Y'
        lnRcNum = RECNO()
        *!* B610963,1 MMT 03/11/2015 Invoice updates neqvamnt incorrectly in GLDIST and wrong invline record in case of consolidated invoice[T20150310.0013][Start]
        *!*	      SUM Totqty FOR CONSOL+Account+cDivision+cCurrCode+STYLE+Dyelot+Dist_Ctr=;
        *!*	        'N'+m.Account+m.cDivision+m.cCurrCode+m.Style+m.Dyelot+IIF(llConsByDc,m.Dist_Ctr,"") TO m.Totqty
        *B611419 Problem with Aria AP/G
        *!*	      SUM Totqty FOR CONSOL+Account+cDivision+cCurrCode+STYLE+Dyelot+Dist_Ctr=;
        *!*	        'N'+m.Account+m.cDivision+m.cCurrCode+m.Style+m.Dyelot+IIF(llConsByDc,m.Dist_Ctr,"")  AND cWareCode = m.cWareCode TO m.Totqty
        SUM Totqty FOR CONSOL+Account+cDivision+cCurrCode+STYLE+Dyelot+Dist_Ctr=;
          'N'+m.Account+m.cDivision+m.cCurrCode+m.Style+m.Dyelot+IIF(llConsByDc,m.Dist_Ctr,"")  AND cWareCode = m.cWareCode AND Price = m.Price AND ;
          Gros_Price = m.Gros_Price AND Disc_Pcnt  = m.Disc_Pcnt   TO m.Totqty
        *B611419 Problem with Aria AP/G
        *!* B610963,1 MMT 03/11/2015 Invoice updates neqvamnt incorrectly in GLDIST and wrong invline record in case of consolidated invoice[T20150310.0013][End]
        IF BETWEEN(lnRcNum,1,RECCOUNT())
          GO RECORD lnRcNum
        ENDIF
      ENDIF
      *!* B609784,1 MMT 12/26/2011 Fix bug of updating Dsc_amt in debit file based on Shipped amount not total charges[Start]
      IF laInvSetup[11,2]
        lnDiscAmt = lnDiscAmt + ROUND((m.Totqty * m.Gros_Price* m.trde_disc)/100,2)
      ENDIF
      *!* B609784,1 MMT 12/26/2011 Fix bug of updating Dsc_amt in debit file based on Shipped amount not total charges[END]
      *!* B608989,1 MMT 08/30/2009 invline table is not update in case of consolidate invoice{End}
      *!* B608449,1 MMT 02/24/2008 fix bug of wrong total charges value[Start]
      *B608892,1 WAM 06/11/2009 Use another variable name since lnShipAmnt already used
      *lnShipAmnt = lnShipAmnt + m.price * m.TOTqty
      lnLShipAmnt = lnLShipAmnt + m.Price * m.Totqty
      *B608892,1 WAM 06/11/2009 (End)

      lnShipQty = lnShipQty + m.Totqty
      *!* B608449,1 MMT 02/24/2008 fix bug of wrong total charges value[End]


      =SEEK(m.Style,'Style')
      *B607667,1 WAM 07/20/2005 Keep Original style descriuption
      *m.Desc1 = Style.desc1
      *B607667,1 WAM 07/20/2005 (End)

      =SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
      *-- the stock current value to use it in calculating the style cost
      lnStkVal = STYLE.nStkVal
      *-- Calculate the avarage cost of the style due to the cost method and warehouse
      m.Cost = IIF(laInvSetup[3,2]='A',;
        IIF(laInvSetup[2,2]='Y',StyDye.Ave_Cost,STYLE.Ave_Cost),STYLE.TotCost)
      *-- Get the GL Cost ,GL Sales link code

      IF laInvSetup[1,2]='Y' .AND. llConsAcc

        *--  Get Sales and Cost link codes
        =SEEK('O'+m.Order+STR(m.LineNo,6),'ORDLINE')
        m.Gl_Cost = IIF(!EMPTY(STYLE.Link_Code),STYLE.Link_Code,'DEFDEF')
        m.Gl_Cost = IIF(laInvSetup[2,2]='Y' .AND. !EMPTY(StyDye.Gl_Link),StyDye.Gl_Link,m.Gl_Cost)
        *-- Concatinate customer sales link code and style sales like code
        m.Gl_Sales = IIF(EMPTY(ORDLINE.Gl_Sales),lcCustSales+STYLE.cSlsGlLink,ORDLINE.Gl_Sales)

        *-- Sales Revenus <CREDIT> Category key '003'
        *-- Update the G/L Distribution file
        DO GLDIST WITH m.Gl_Sales,'003',-(m.Totqty *m.Price),'IN',lcInvNo,;
          &lcHdrFile..dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
          &lcHdrFile..cCurrCode,&lcHdrFile..nCurrUnit,&lcHdrFile..nExRate
        m.cSalesAcnt = &lcDistFile..GlAccount   && Get the sales Revenus account

        *-- Discount <DEBIT> Category key '005'
        *-- Update the G/L Distribution file
        IF &lcHdrFile..ShipAmt > 0
          DO GLDIST WITH m.Gl_Sales,'005',(m.Totqty * m.Price * ABS(&lcHdrFile..Discount)/&lcHdrFile..ShipAmt),;
            'IN',lcInvNo,&lcHdrFile..dPostDate,lcGlYear,lcGlPeriod,;
            lcDistFile,'',&lcHdrFile..cCurrCode,&lcHdrFile..nCurrUnit,&lcHdrFile..nExRate
        ENDIF
        *--  Don't update cDicAcnt field in InvLine file if there no discount
        IF &lcHdrFile..ShipAmt > 0 .AND. (m.Totqty * m.Price * ABS(&lcHdrFile..Discount)/&lcHdrFile..ShipAmt) > 0
          m.cDiscAcnt = &lcDistFile..GlAccount
        ENDIF
      ENDIF
      *-- Update style dyelot Shipped quantities
      IF laInvSetup[4,2]='Y' AND m.CONSOL <> 'Y' AND ; &&llConsAcc AND
        STYLE.cDye_Flg = 'Y' AND SEEK(m.Style+m.cWareCode+m.Dyelot,'StyDye')
        =RLOCK('StyDye')
        REPLACE SHP1   WITH SHP1   + m.QTY1  ,;
          SHP2   WITH SHP2   + m.QTY2  ,;
          SHP3   WITH SHP3   + m.QTY3  ,;
          SHP4   WITH SHP4   + m.QTY4  ,;
          SHP5   WITH SHP5   + m.QTY5  ,;
          SHP6   WITH SHP6   + m.QTY6  ,;
          SHP7   WITH SHP7   + m.QTY7  ,;
          SHP8   WITH SHP8   + m.QTY8  ,;
          TOTSHP WITH TOTSHP + m.Totqty IN StyDye
        UNLOCK IN StyDye
      ENDIF
      *-- Update warehouse stock and shipped quantities
      *-- Update warehouse stock for Inventory styles only
      IF m.CONSOL <> 'Y' AND SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
        =RLOCK('StyDye')
        REPLACE SHP1   WITH SHP1   + m.QTY1  ,;
          SHP2   WITH SHP2   + m.QTY2  ,;
          SHP3   WITH SHP3   + m.QTY3  ,;
          SHP4   WITH SHP4   + m.QTY4  ,;
          SHP5   WITH SHP5   + m.QTY5  ,;
          SHP6   WITH SHP6   + m.QTY6  ,;
          SHP7   WITH SHP7   + m.QTY7  ,;
          SHP8   WITH SHP8   + m.QTY8  ,;
          TOTSHP WITH TOTSHP + m.Totqty IN StyDye
        UNLOCK IN StyDye
      ENDIF
      *-- Update Style stock and shipped quantities
      *-- Update style stock for Inventory styles only
      IF m.CONSOL <> 'Y'
        =RLOCK('STYLE')
        REPLACE SHP1   WITH SHP1   + m.QTY1 ,;
          SHP2   WITH SHP2   + m.QTY2 ,;
          SHP3   WITH SHP3   + m.QTY3 ,;
          SHP4   WITH SHP4   + m.QTY4 ,;
          SHP5   WITH SHP5   + m.QTY5 ,;
          SHP6   WITH SHP6   + m.QTY6 ,;
          SHP7   WITH SHP7   + m.QTY7 ,;
          SHP8   WITH SHP8   + m.QTY8 ,;
          TOTSHP WITH TOTSHP + m.Totqty IN STYLE
        UNLOCK IN STYLE
      ENDIF

      *-- Sales Rep Commission per line
      *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
      *IF laInvSetup[6,2]='Y'
      IF m.Comm1 > 0 OR m.Comm2 > 0
        lnTotAmntComm = lnTotAmntComm +  m.Totqty*m.Price*(1-&lcHdrFile..DiscPcnt/100)*(1-&lcHdrFile..trde_disc/100)
        *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
        *-- Accumulate Salesreps Commission Amount.
        lnNetAmnt = m.Totqty*m.Price*(1-&lcHdrFile..DiscPcnt/100)*(1-&lcHdrFile..trde_disc/100)
        lnComm1   = lnComm1 + (lnNetAmnt * m.Comm1 / 100)
        lnComm2   = lnComm2 + (lnNetAmnt * m.Comm2 / 100)
      ENDIF
      *-- Add new ivoice line
      m.Invoice = lcInvNo
      m.Order   = lcOrdNo
      *-- Calculate the equivlant amount by the company currency code
      m.nEqvAmnt = ROUND(m.Price*m.Totqty &lcExRSin &lcHdrFile..nExRate &lcUntSin &lcHdrFile..nCurrUnit,2)
      m.InvDate  = &lcHdrFile..InvDate
      IF CONSOL <> 'Y'
        *-- if new order line or the line not found ad new line to the ORDLINE File
        IF ((!&lcHdrFile..Direct_Inv AND &lcDetFile..lNewLine) OR ;
            !SEEK('O'+m.Order+STR(m.LineNo,6),'ORDLINE')) AND &lcDetFile..Totqty <> 0
          *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[START]
          *IF SEEK('O'+lcOrdNo,'ORDHDR')
          IF SEEK('O'+lcOrdNo,'ORDHDR','ORDHDR')
            *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[END]
            *-- This is a new sales oreder line so update the order header
            =RLOCK('ORDHDR')
            *-- update the order header with the new line
            REPLACE LastLine WITH LastLine + 1 ,;
              Book     WITH Book     + m.Totqty ,;
              BookAmt  WITH BookAmt  + m.Totqty*m.Price ,;
              Ship     WITH Ship     + m.Totqty ,;
              ShipAmt  WITH ShipAmt  + m.Totqty*m.Price IN 'ORDHDR'
            m.LineNo= ORDHDR.LastLine
            *!* B612564,1 MMT 05/18/2022 When the same user edits SO has a Picking ticket, then Invoiced this SO, and then saved the SO, the SO status became open again while it should be completed[T20220511.0004][Stat]
            =gfAdd_Info('ORDHDR')
            *!* B612564,1 MMT 05/18/2022 When the same user edits SO has a Picking ticket, then Invoiced this SO, and then saved the SO, the SO status became open again while it should be completed[T20220511.0004][End]
            UNLOCK IN 'ORDHDR'
          ELSE
            REPLACE LastLine WITH LastLine + 1 IN (lcHdrFile)
            m.LineNo= &lcHdrFile..LastLine
          ENDIF
          *HBG 12/21/2004 Fix bug of saving empty Qty in Ordline for the new lines entered while creating the invoice [Begin]
          *!*	        INSERT INTO ORDLINE ;
          *!*	                   (cOrdType,Order,Account,LineNo,Style,Dyelot,Scale,Group,Gros_Price,Disc_Pcnt,;
          *!*	                    Price,Start,Complete,Comm1,Comm2,Note_Mem,Desc1,Book1,Book2,Book3,Book4,;
          *!*	                    Book5,Book6,Book7,Book8,TotBook,Season,PrePak,PPQty,cWareCode,Store,Pack_id);
          *!*	         VALUES ('O',m.Order,m.Account,m.LineNo,m.Style,m.Dyelot,m.Scale,m.Group,m.Gros_Price,;
          *!*	                 m.Disc_Pcnt,m.Price,&lcHdrFile..InvDate,&lcHdrFile..InvDate,m.Comm1,;
          *!*	                 m.Comm2,m.Note_Mem,m.Desc1,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
          *!*	                 m.Qty7,m.Qty8,m.TotQty,m.Season,m.PrePak,m.PPQty,m.cWareCode,m.Store,m.Pack_Id)
          INSERT INTO ORDLINE ;
            (cOrdType,ORDER,Account,LINENO,STYLE,Dyelot,SCALE,GROUP,Gros_Price,Disc_Pcnt,;
            Price,START,COMPLETE,Comm1,Comm2,Note_Mem,Desc1,Book1,Book2,Book3,Book4,;
            Book5,Book6,Book7,Book8,TotBook,Season,PrePak,PPQty,cWareCode,STORE,Pack_id,;
            QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,Totqty);
            VALUES ('O',m.Order,m.Account,m.LineNo,m.Style,m.Dyelot,m.Scale,m.Group,m.Gros_Price,;
            m.Disc_Pcnt,m.Price,&lcHdrFile..InvDate,&lcHdrFile..InvDate,m.Comm1,;
            m.Comm2,m.Note_Mem,m.Desc1,m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,;
            m.QTY7,m.QTY8,m.Totqty,m.Season,m.PrePak,m.PPQty,m.cWareCode,m.Store,m.Pack_id,;
            m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8,m.TotBook)
          *HBG [End]
          *!* E302780,1 MMT 10/27/2010 Modify Invoice and Invoice SO screen to apply trade discount per line[Start]
          IF laInvSetup[11,2]
            REPLACE trde_disc  WITH m.trde_disc IN ORDLINE
          ENDIF
          *!* E302780,1 MMT 10/27/2010 Modify Invoice and Invoice SO screen to apply trade discount per line[End]
          SELECT (lcDetFile)
          =gfDoTriger('ARDINV',PADR('UPDTORDD',10))
        ELSE
          IF laInvSetup[4,2]='Y' .AND. STYLE.cDye_Flg='Y' .AND. ;
              !EMPTY(ORDLINE.Dyelot) AND SEEK(IIF(!EMPTY(m.AltStyle),m.AltStyle,m.Style)+m.cWareCode+ORDLINE.Dyelot,'StyDye')
            =RLOCK('StyDye')
            *-- Decrease order Original dyelot ordered quantity
            IF !m.lBackOrd OR (!EMPTY(ORDLINE.Dyelot) AND ORDLINE.Dyelot<>m.Dyelot)
              *-- this is a sales order invoice and no Back order so decrease the ordered quantity
              *-- in the stydye file with the total ordered quantity for this Style
              REPLACE Ord1   WITH MAX(Ord1 - ORDLINE.QTY1,0),;
                Ord2   WITH MAX(Ord2 - ORDLINE.QTY2,0),;
                Ord3   WITH MAX(Ord3 - ORDLINE.QTY3,0),;
                Ord4   WITH MAX(Ord4 - ORDLINE.QTY4,0),;
                Ord5   WITH MAX(Ord5 - ORDLINE.QTY5,0),;
                Ord6   WITH MAX(Ord6 - ORDLINE.QTY6,0),;
                Ord7   WITH MAX(Ord7 - ORDLINE.QTY7,0),;
                Ord8   WITH MAX(Ord8 - ORDLINE.QTY8,0),;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
            ELSE
              *-- (Back order) so decrease the ordered quantity with the the shipped quantity
              *-- for this Style
              REPLACE Ord1   WITH Ord1 - MIN(ORDLINE.QTY1,m.QTY1),;
                Ord2   WITH Ord2 - MIN(ORDLINE.QTY2,m.QTY2),;
                Ord3   WITH Ord3 - MIN(ORDLINE.QTY3,m.QTY3),;
                Ord4   WITH Ord4 - MIN(ORDLINE.QTY4,m.QTY4),;
                Ord5   WITH Ord5 - MIN(ORDLINE.QTY5,m.QTY5),;
                Ord6   WITH Ord6 - MIN(ORDLINE.QTY6,m.QTY6),;
                Ord7   WITH Ord7 - MIN(ORDLINE.QTY7,m.QTY7),;
                Ord8   WITH Ord8 - MIN(ORDLINE.QTY8,m.QTY8),;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
            ENDIF
            UNLOCK IN StyDye
          ENDIF
          *--decrease the Alocated quantitys in the stydye file
          *-- with the order line Picked Quantities
          *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[Start]
          *!*	        IF OrdLine.Picked AND !EMPTY(OrdLine.Dyelot) AND SEEK(m.Style+m.cWareCode+OrdLine.Dyelot,'StyDye')
          *!*	          REPLACE ALO1   WITH ALO1   - OrdLine.PIK1,;
          *!*	                  ALO2   WITH ALO2   - OrdLine.PIK2,;
          *!*	                  ALO3   WITH ALO3   - OrdLine.PIK3,;
          *!*	                  ALO4   WITH ALO4   - OrdLine.PIK4,;
          *!*	                  ALO5   WITH ALO5   - OrdLine.PIK5,;
          *!*	                  ALO6   WITH ALO6   - OrdLine.PIK6,;
          *!*	                  ALO7   WITH ALO7   - OrdLine.PIK7,;
          *!*	                  ALO8   WITH ALO8   - OrdLine.PIK8,;
          *!*	                  TOTALO WITH TOTALO - OrdLine.TOTPIK IN StyDye
          *!*	          UNLOCK IN StyDye
          *!*	        ENDIF
          *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[END]
          IF SEEK(IIF(!EMPTY(m.AltStyle),m.AltStyle,m.Style)+m.cWareCode+SPACE(10),'StyDye')
            =RLOCK('StyDye')
            *-- before saving any issue invoice if the style was changed by the user
            *-- the old style will be saved in the AltStyle field
            IF !m.lBackOrd
              *-- this is a sales order invoice with no Back order or there is an altrernative
              *-- style so decrease the ordered quantity in the stydye file with
              *-- the total ordered quantity for the AltStyle
              REPLACE Ord1   WITH MAX(Ord1 - ORDLINE.QTY1,0),;
                Ord2   WITH MAX(Ord2 - ORDLINE.QTY2,0),;
                Ord3   WITH MAX(Ord3 - ORDLINE.QTY3,0),;
                Ord4   WITH MAX(Ord4 - ORDLINE.QTY4,0),;
                Ord5   WITH MAX(Ord5 - ORDLINE.QTY5,0),;
                Ord6   WITH MAX(Ord6 - ORDLINE.QTY6,0),;
                Ord7   WITH MAX(Ord7 - ORDLINE.QTY7,0),;
                Ord8   WITH MAX(Ord8 - ORDLINE.QTY8,0),;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
            ELSE
              *-- (Back order) so decrease the ordered quantity with the the shipped quantity
              *-- for this Style
              REPLACE Ord1   WITH Ord1 - MIN(ORDLINE.QTY1,m.QTY1),;
                Ord2   WITH Ord2 - MIN(ORDLINE.QTY2,m.QTY2),;
                Ord3   WITH Ord3 - MIN(ORDLINE.QTY3,m.QTY3),;
                Ord4   WITH Ord4 - MIN(ORDLINE.QTY4,m.QTY4),;
                Ord5   WITH Ord5 - MIN(ORDLINE.QTY5,m.QTY5),;
                Ord6   WITH Ord6 - MIN(ORDLINE.QTY6,m.QTY6),;
                Ord7   WITH Ord7 - MIN(ORDLINE.QTY7,m.QTY7),;
                Ord8   WITH Ord8 - MIN(ORDLINE.QTY8,m.QTY8),;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN StyDye
            ENDIF
            UNLOCK IN StyDye
          ENDIF
          *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[Start]
          *!*	        IF OrdLine.Picked AND SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
          *!*	          =RLOCK('StyDye')
          *!*	          *--decrease the Alocated quantitys in the stydye file
          *!*	          *-- with the order line Picked Quantities
          *!*	          REPLACE ALO1   WITH ALO1   - OrdLine.PIK1,;
          *!*	                  ALO2   WITH ALO2   - OrdLine.PIK2,;
          *!*	                  ALO3   WITH ALO3   - OrdLine.PIK3,;
          *!*	                  ALO4   WITH ALO4   - OrdLine.PIK4,;
          *!*	                  ALO5   WITH ALO5   - OrdLine.PIK5,;
          *!*	                  ALO6   WITH ALO6   - OrdLine.PIK6,;
          *!*	                  ALO7   WITH ALO7   - OrdLine.PIK7,;
          *!*	                  ALO8   WITH ALO8   - OrdLine.PIK8,;
          *!*	                  TOTALO WITH TOTALO - OrdLine.TOTPIK IN StyDye
          *!*	          UNLOCK IN StyDye
          *!*	        ENDIF
          *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[End]
          IF SEEK(IIF(!EMPTY(m.AltStyle),m.AltStyle,m.Style),'Style')
            =RLOCK('Style')
            IF !m.lBackOrd
              *-- this is a sales order invoice with no Back order or there is an altrernative
              *-- style so decrease the ordered quantity in the style file with
              *-- the total ordered quantity for the AltStyle
              *-- Cancel the remaining quantities
              REPLACE Ord1   WITH MAX(Ord1 - ORDLINE.QTY1,0),;
                Ord2   WITH MAX(Ord2 - ORDLINE.QTY2,0),;
                Ord3   WITH MAX(Ord3 - ORDLINE.QTY3,0),;
                Ord4   WITH MAX(Ord4 - ORDLINE.QTY4,0),;
                Ord5   WITH MAX(Ord5 - ORDLINE.QTY5,0),;
                Ord6   WITH MAX(Ord6 - ORDLINE.QTY6,0),;
                Ord7   WITH MAX(Ord7 - ORDLINE.QTY7,0),;
                Ord8   WITH MAX(Ord8 - ORDLINE.QTY8,0),;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN STYLE
            ELSE
              *-- (Back order) so decrease the ordered quantity with the the shipped quantity
              *-- for this Style
              REPLACE Ord1   WITH Ord1 - MIN(ORDLINE.QTY1,m.QTY1),;
                Ord2   WITH Ord2 - MIN(ORDLINE.QTY2,m.QTY2),;
                Ord3   WITH Ord3 - MIN(ORDLINE.QTY3,m.QTY3),;
                Ord4   WITH Ord4 - MIN(ORDLINE.QTY4,m.QTY4),;
                Ord5   WITH Ord5 - MIN(ORDLINE.QTY5,m.QTY5),;
                Ord6   WITH Ord6 - MIN(ORDLINE.QTY6,m.QTY6),;
                Ord7   WITH Ord7 - MIN(ORDLINE.QTY7,m.QTY7),;
                Ord8   WITH Ord8 - MIN(ORDLINE.QTY8,m.QTY8),;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8 IN STYLE
            ENDIF
            UNLOCK IN STYLE
          ENDIF
          *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[Start]
          *!*	        IF OrdLine.Picked AND SEEK(m.Style,'Style')
          *!*	          *--decrease the Alocated quantitys in the style file
          *!*	          *-- with the order line Picked Quantities
          *!*	          REPLACE ALO1   WITH ALO1   - OrdLine.PIK1,;
          *!*	                  ALO2   WITH ALO2   - OrdLine.PIK2,;
          *!*	                  ALO3   WITH ALO3   - OrdLine.PIK3,;
          *!*	                  ALO4   WITH ALO4   - OrdLine.PIK4,;
          *!*	                  ALO5   WITH ALO5   - OrdLine.PIK5,;
          *!*	                  ALO6   WITH ALO6   - OrdLine.PIK6,;
          *!*	                  ALO7   WITH ALO7   - OrdLine.PIK7,;
          *!*	                  ALO8   WITH ALO8   - OrdLine.PIK8,;
          *!*	 	              TOTALO WITH TOTALO - OrdLine.TOTPIK IN Style
          *!*	    	  UNLOCK IN Style
          *!*	        ENDIF
          *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[END]
          *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[START]
          *IF SEEK('O'+m.Order,'ORDHDR')
          IF SEEK('O'+m.Order,'ORDHDR','ORDHDR')
            *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[END]
            lcAlias = ALIAS()
            SELECT ORDHDR
            *-- update the order header with the canceled quantity and amount
            =RLOCK('ORDHDR')
            lnOrdShip=0
            FOR lnCount = 1 TO 8
              lcSize = STR(lnCount,1)
              lnOrdShip= lnOrdShip + MIN(m.Qty&lcSize,ORDLINE.Qty&lcSize)
            ENDFOR
            REPLACE CANCEL    WITH CANCEL+IIF(m.lBackOrd,0,MAX(ORDLINE.Totqty-m.Totqty,0)),;
              CancelAmt WITH CancelAmt + ;
              IIF(m.lBackOrd,0,MAX(ORDLINE.Totqty-m.Totqty,0))*ORDLINE.Price ,;
              Ship      WITH Ship + m.Totqty ,;
              ShipAmt   WITH ShipAmt + m.Totqty*m.Price ,;
              OPEN      WITH MAX(OPEN-IIF(m.lBackOrd, lnOrdShip,ORDLINE.Totqty),0),;
              OpenAmt   WITH MAX(OpenAmt - ;
              IIF(m.lBackOrd, lnOrdShip,ORDLINE.Totqty)*ORDLINE.Price,0) ,;
              STATUS    WITH IIF(OPEN > 0,STATUS,'C') IN 'ORDHDR'
              
            *!* B612564,1 MMT 05/18/2022 When the same user edits SO has a Picking ticket, then Invoiced this SO, and then saved the SO, the SO status became open again while it should be completed[T20220511.0004][Stat]
            =gfAdd_Info('ORDHDR')
            *!* B612564,1 MMT 05/18/2022 When the same user edits SO has a Picking ticket, then Invoiced this SO, and then saved the SO, the SO status became open again while it should be completed[T20220511.0004][End]

              
            UNLOCK IN 'ORDHDR'
            SELECT (lcAlias)
          ENDIF
          *B608326,1 WAM 10/23/2007 Do not add records for unshiped lines in PIKLINE file
          *IF OrdLine.Picked .AND. OrdLine.PikTkt <> '******' .AND. ;
          !SEEK(OrdLine.PikTkt+OrdLine.Order+STR(OrdLine.LineNo,6),'PIKLINE')
          IF ORDLINE.Picked .AND. ORDLINE.PikTkt <> '******' .AND. ;
              !SEEK(ORDLINE.PikTkt+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'PIKLINE') AND m.Totqty > 0
            *B608326,1 WAM 10/23/2007 (End)

            SELECT ORDLINE
            COPY NEXT 1 TO (oAriaApplication.WorkDir+lcTmpPikLn)
            *-- Start, Update pik line and cutpick.
            *-- Call a custom function if found
            IF ASCAN(oAriaApplication.laEvntTrig , PADR('UPDPIKLN',10)) <> 0
              = gfDoTriger('ARIINV',PADR('UPDPIKLN',10))
            ENDIF
            SELECT PIKLINE
            APPEND FROM (oAriaApplication.WorkDir+lcTmpPikLn)
            =RLOCK()
            REPLACE PIK1   WITH M.QTY1 ,;
              PIK2   WITH M.QTY2 ,;
              PIK3   WITH M.QTY3 ,;
              PIK4   WITH M.QTY4 ,;
              PIK5   WITH M.QTY5 ,;
              PIK6   WITH M.QTY6 ,;
              PIK7   WITH M.QTY7 ,;
              PIK8   WITH M.QTY8 ,;
              TotPIK WITH M.Totqty

            *!* B608288,1 MMT 09/25/2007 Update Invoice and Invdate fields in pikline file[Start]
            REPLACE Invoice WITH lcInvNo,;
              InvDate WITH &lcHdrFile..InvDate

            =gfAdd_Info('PIKLINE')
            *!* B608288,1 MMT 09/25/2007 Update Invoice and Invdate fields in pikline file[End]
            UNLOCK
            ERASE (oAriaApplication.WorkDir+lcTmpPikLn+".DBF")
          ENDIF


          *!* C201063,1 MMT 11/26/2008 Convert Custom Cartons Program of MEM10 to Aria4 [Start]
          *!* B608846,1 MMT 04/14/2009 change position of MEMO trigger as it causes wrong Saved Qty in invline[Start]
          *IF ASCAN(loFormSet.laEvntTrig , PADR('ARINVSAV',10)) <> 0
          *  =loFormSet.mDoTrigger(PADR('ARINVSAV',10))
          *ENDIF
          *!* B608846,1 MMT 04/14/2009 change position of MEMO trigger as it causes wrong Saved Qty in invline[End]
          *!* C201063,1 MMT 11/26/2008 Convert Custom Cartons Program of MEM10 to Aria4 [End]

          *B131241,1 WAM 02/19/2006 Set the proper order
          *IF ORDLINE.Picked  .AND. SEEK(ORDLINE.ORDER+ORDLINE.PikTkt,'PikTkt') AND (&lcHdrFile..Ship > 0 OR !m.lBackOrd)
          *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[Start]
          *!*	        IF ORDLINE.Picked  .AND. SEEK(ORDLINE.ORDER+ORDLINE.PikTkt,'PikTkt','ORDPIK') AND (&lcHdrFile..Ship > 0 OR !m.lBackOrd)
          *!*	        *B131241,1 WAM 02/19/2006 (End)
          *!*	          =RLOCK('PikTkt')
          *!*	          REPLACE Status WITH 'C' IN 'PikTkt'
          *!*	          UNLOCK IN 'PikTkt'
          *!*	        ENDIF
          *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[End]
          =RLOCK('ORDLINE')
          *-- replace the quantities  in the Ordline file with 0 or the remaning quantities
          *-- if back order is yes
          *-- and the picked quantities with 0
          *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[Start]
          *REPLACE Qty1   WITH Qty1 - IIF(m.lBackOrd,MIN(Qty1,m.Qty1),Qty1),;
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
          PIKTKT WITH '' IN ORDLINE
          REPLACE QTY1   WITH QTY1 - IIF(m.lBackOrd,MIN(QTY1,m.QTY1),QTY1),;
            QTY2   WITH QTY2 - IIF(m.lBackOrd,MIN(QTY2,m.QTY2),QTY2),;
            QTY3   WITH QTY3 - IIF(m.lBackOrd,MIN(QTY3,m.QTY3),QTY3),;
            QTY4   WITH QTY4 - IIF(m.lBackOrd,MIN(QTY4,m.QTY4),QTY4),;
            QTY5   WITH QTY5 - IIF(m.lBackOrd,MIN(QTY5,m.QTY5),QTY5),;
            QTY6   WITH QTY6 - IIF(m.lBackOrd,MIN(QTY6,m.QTY6),QTY6),;
            QTY7   WITH QTY7 - IIF(m.lBackOrd,MIN(QTY7,m.QTY7),QTY7),;
            QTY8   WITH QTY8 - IIF(m.lBackOrd,MIN(QTY8,m.QTY8),QTY8),;
            Totqty WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8  IN ORDLINE
          *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[END]
          *!* B608664,1 MMT 08/26/2008 Fix bug of updating npck fields in Ordline File[Start]
          *B608846,1 MMT 04/14/2009 Make invoice saving update npck Fields in ordline[Start]
          REPLACE nPck1 WITH 0 ,;
            nPck2 WITH 0 ,;
            nPck3 WITH 0 ,;
            nPck4 WITH 0 ,;
            nPck5 WITH 0 ,;
            nPck6 WITH 0 ,;
            nPck7 WITH 0 ,;
            nPck8 WITH 0 ,;
            AltStyle WITH IIF(EMPTY(m.AltStyle),AltStyle,m.Style),;
            Dyelot   WITH m.Dyelot IN ORDLINE
          *REPLACE  AltStyle WITH IIF(EMPTY(m.AltStyle),AltStyle,m.Style),;
          Dyelot   WITH m.Dyelot IN ORDLINE
          *B608846,1 MMT 04/14/2009 Make invoice saving update npck Fields in ordline[End]
          *!* B608664,1 MMT 08/26/2008 Fix bug of updating npck fields in Ordline File[End]

          UNLOCK
        ENDIF
      ENDIF
      IF laInvSetup[1,2]='Y' .AND. llConsAcc
        *--  Cost of Goods Sold <DEBIT> Category key '008'
        laGlArray[1,1] = m.Gl_Sales
        laGlArray[1,2] = '008'
        laGlArray[1,3] = -1

        *-- Inventory Control <CREDIT> Category key '006'
        laGlArray[2,1] = m.Gl_Cost
        laGlArray[2,2] = '006'
        laGlArray[2,3] = 1

        =SEEK(m.Gl_Sales+'008','GL_LINK')
        m.cCOGSAcnt = Gl_Link.GlAcnt      && Cost of goods sold account
        =SEEK(m.Gl_Cost+'006','GL_LINK')
        m.cICAcnt = Gl_Link.GlAcnt        && Inventory control account
      ENDIF
      SELECT (lcDetFile)
      SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,Totqty TO laAdjStk
      PRIVATE lcRefer
      IF EMPTY(&lcHdrFile..ORDER) AND &lcHdrFile..Direct_Inv
        *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
        *lcRefer = 'CUST# '+ &lcHdrFile..Account + "-" + Customer.BTName
        lcRefer = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUSTNO,oAriaApplication.GetHeaderText("LANG_CUSTNO",lcInvHeaderAlias)) +' '+ &lcHdrFile..Account + "-" + Customer.BTName
        *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
      ELSE
        *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
        *lcRefer = 'CUST# '+ &lcHdrFile..Account + " Sales order " + &lcHdrFile..ORDER
        lcRefer = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUSTNO,oAriaApplication.GetHeaderText("LANG_CUSTNO",lcInvHeaderAlias)) +' '+ &lcHdrFile..Account + " "+;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SO,oAriaApplication.GetHeaderText("LANG_SO",lcInvHeaderAlias))+" " + &lcHdrFile..ORDER
        *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
      ENDIF
      *-- Update the  Stock in the STYDYE , STYINVJL , STYLE
      *!* E303048,1 MMT 01/29/2012 Add triggers in Invoice saving prg for [T20110621.0057][Start]
      IF ASCAN(loFormSet.laEvntTrig , PADR('UPDCSTCN',10)) <> 0
        =loFormSet.mDoTrigger(PADR('UPDCSTCN',10))
      ENDIF
      *!* E303048,1 MMT 01/29/2012 Add triggers in Invoice saving prg for [T20110621.0057][End]
      *B130201,1 HBG Call style control for consolidated records only
      *IF m.TotQty <> 0 AND m.CONSOL <> 'Y'
      IF m.Totqty <> 0 AND llConsAcc
        *B130201,1 HBG (End)

        *B608826,1 TMI 21/03/2009 [Start] remove the added trigger,update the STYINVJL using the standard gfStyCrl global function, do not use the custom copy
        **C200876,1 TMI [Start] Call the special GFSTYCRL function
        *IF ASCAN(loFORMSET.laEvntTrig,'GFSTYCRL')<>0
        *  lnNextSt = loFormSet.mDoTrigger(PADR('GFSTYCRL',10))
        *ELSE
        *  *C200876,1 TMI [End  ]
        *B608826,1 TMI 21/03/2009 [End  ]

        =gfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,@laAdjStk,;
          m.Cost,lcRefer,lcGlSession,'',13,lcDetFile,'nSteps',@laGlArray,m.LineNo))

        *B608826,1 TMI 21/03/2009 [Start] remove the added trigger
        *  *C200876,1 TMI [Start] close the above if statm.
        *ENDIF
        **C200876,1 TMI [End  ]
        *B608826,1 TMI 21/03/2009 [End  ]

      ENDIF

      *-- Get difference between the old stock value and the new stock value
      lnStkVal  = lnStkVal - STYLE.nStkVal
      *-- calculate the new cost
      *-- Get the style cost from style file if cost method is standard.
      *!* B611089,1 MMT 11/30/2015 Incorrect Style Cost value saved in INVLINE after creating invoice[T20151029.0011][Start]
      *!*	    IF laInvSetup[3,2]='S'
      *!*	      m.Cost = STYLE.TotCost
      *!*	    ELSE
      *!*	      m.Cost = IIF(m.TotQty=0,0,lnStkVal/m.TotQty)
      *!*	    ENDIF
      =SEEK(m.Style+m.cWareCode+SPACE(10),'StyDye')
      m.Cost = IIF(laInvSetup[3,2]='A',;
        IIF(laInvSetup[2,2]='Y',StyDye.Ave_Cost,STYLE.Ave_Cost),STYLE.TotCost)
      *!* B611089,1 MMT 11/30/2015 Incorrect Style Cost value saved in INVLINE after creating invoice[T20151029.0011][End]

      *!* E303048,1 MMT 01/29/2012 Add triggers in Invoice saving prg for [T20110621.0057][Start]
      IF ASCAN(loFormSet.laEvntTrig , PADR('UPDCSTVA',10)) <> 0
        =loFormSet.mDoTrigger(PADR('UPDCSTVA',10))
      ENDIF
      *!* E303048,1 MMT 01/29/2012 Add triggers in Invoice saving prg for [T20110621.0057][END]

      *-- Cost of goods sold amount
      lnCOGSAmt = lnCOGSAmt + m.Totqty*m.Cost
      *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
      *IF llConsAcc .AND. SEEK(m.Style+lcGlYear,'icStyHst')
      old_alias = ALIAS()
      SELECT icStyHst
      *!* B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][
      IF !gfSEEK(m.Style + lcGlYear)
        =gfOpenTable('ICSTYHST','STYHST','SH','ICSTYHSX')
        SELECT ICSTYHSX

        APPEND BLANK
        REPLACE STYLE WITH m.Style
        REPLACE CFISFYEAR  WITH lcGlYear
        = GFREPLACE('')
        =gfTABLEUPDATE()

        *USE IN ICSTYHSX
        =gfcloseTable('ICSTYHSX')
        SELECT icStyHst
      ENDIF
      *!* B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][
      IF llConsAcc .AND. gfSEEK(m.Style+lcGlYear)
        SELECT (old_alias)
        *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
        *-- Shipped amount
        lnShipAmnt = m.Totqty*m.Price &lcExRSin &lcHdrFile..nExRate &lcUntSin &lcHdrFile..nCurrUnit
        *--Discount amount
        lnDiscAmnt = m.Totqty*m.Price*&lcHdrFile..DiscPcnt/100 &lcExRSin &lcHdrFile..nExRate &lcUntSin &lcHdrFile..nCurrUnit

        *-- update the ic style history file  for this gl period
        *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
        *=RLOCK('icStyHst')
        *REPLACE nSlsQty&lcGlPeriod  WITH nSlsQty&lcGlPeriod  + m.TotQty   ,;
        *        nSlsQty             WITH nSlsQty             + m.TotQty   ,;
        *        nSlsAmt&lcGlPeriod  WITH nSlsAmt&lcGlPeriod  + lnShipAmnt ,;
        *        nSlsAmt             WITH nSlsAmt             + lnShipAmnt ,;
        *        nDisAmt&lcGlPeriod  WITH nDisAmt&lcGlPeriod  + lnDiscAmnt ,;
        *        nDisAmt             WITH nDisAmt             + lnDiscAmnt ,;
        *        nCOGSAmt&lcGlPeriod WITH nCOGSAmt&lcGlPeriod + m.TotQty*m.Cost  ,;
        *        nCOGSAmt            WITH nCOGSAmt            + m.TotQty*m.Cost  IN 'icStyHst'
        *UNLOCK IN 'icStyHst'

        old_alias = ALIAS()
        SELECT icStyHst

        REPLACE nSlsQty&lcGlPeriod  WITH nSlsQty&lcGlPeriod  + m.Totqty   ,;
          nSlsQty             WITH nSlsQty             + m.Totqty   ,;
          nSlsAmt&lcGlPeriod  WITH nSlsAmt&lcGlPeriod  + lnShipAmnt ,;
          nSlsAmt             WITH nSlsAmt             + lnShipAmnt ,;
          nDisAmt&lcGlPeriod  WITH nDisAmt&lcGlPeriod  + lnDiscAmnt ,;
          nDisAmt             WITH nDisAmt             + lnDiscAmnt ,;
          nCOGSAmt&lcGlPeriod WITH nCOGSAmt&lcGlPeriod + m.Totqty*m.Cost  ,;
          nCOGSAmt            WITH nCOGSAmt            + m.Totqty*m.Cost
        =GFREPLACE('')
        SELECT (old_alias)
        *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
      ENDIF
      m.Store = IIF(m.CONSOL='Y',&lcHdrFile..cConStore,m.Store)
      m.Invoice = lcInvNo
      m.InvDate = &lcHdrFile..InvDate
      IF llConsAcc .AND. m.Totqty > 0 .AND. !SEEK(m.Invoice+STR(m.LineNo,6),'INVLINE')
        *-- update the invline with the new line
        INSERT INTO INVLINE FROM MEMVAR
      ENDIF
      *B130201,1 HBG Update cost in INVLINE for consolidated invoices
      *    IF !llConsAcc .AND. m.TotQty > 0 .AND. SEEK(m.Style+m.Invoice,'INVLINE','Invlines')
      *      REPLACE Cost WITH IIF(Cost = 0,m.Cost,Cost) IN INVLINE
      *    ENDIF
      *B130201,1 HBG (End)

      IF !llConsAcc .AND. m.Totqty > 0 .AND. ;
          !SEEK(m.Invoice+m.Store+m.Order+m.Style+STR(m.LineNo,6),'CONSINVL')
        *-- update the conslidated invoice line file
        *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
        *WAIT WINDOW "Update Consolidated Invoice line " NOWAIT
        WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATECONSINVLINE,oAriaApplication.GetHeaderText("LANG_UPDATECONSINVLINE",lcInvHeaderAlias)) NOWAIT
        *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
        INSERT INTO CONSINVL FROM MEMVAR
        WAIT CLEAR
      ENDIF

      *B608826,1 TMI 25/03/2009 [Start] Move the functionality of DLARBIN into ALSAVINV to make one loop from within the
      *-                                PKBINLOC file
      *C200876,1 TMI [Start] update the BININVJL while saving an invoice
      *IF ASCAN(loFORMSET.laEvntTrig,PADR('DLARBIN',10)) <> 0
      *  =loFORMSET.mDoTrigger(PADR('DLARBIN',10))
      *ENDIF
      *C200876,1 TMI [End  ]
      IF ASCAN(loFormSet.laEvntTrig , PADR('ALSAVINV',10)) <> 0
        =loFormSet.mDoTrigger(PADR('ALSAVINV',10))
      ENDIF
      *B608826,1 TMI 25/03/2009 [End  ]

      *!* B608846,1 MMT 04/14/2009 change position of MEMO trigger as it causes wrong Saved Qty in invline[Start]
      IF !EMPTY(ORDLINE.PikTkt) AND ASCAN(loFormSet.laEvntTrig , PADR('ARINVSAV',10)) <> 0
        =loFormSet.mDoTrigger(PADR('ARINVSAV',10))
      ENDIF
      *!* B608846,1 MMT 04/14/2009 change position of MEMO trigger as it causes wrong Saved Qty in invline[End]


    ENDSCAN
    *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[Start]
    *B609997,2 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[Start]
    *IF &lcHdrFile..Ship > 0
    IF &lcHdrFile..Ship > 0  AND !EMPTY(&lcHdrFile..PikTkt)
      *B609997,2 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[End]
      lfReleasePickTicket(&lcHdrFile..ORDER,&lcHdrFile..STORE,&lcHdrFile..PikTkt)
    ENDIF
    *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[END]
    *-- Update order line calceled quantity.
    IF !EMPTY(lcOrdCanLn) AND &lcHdrFile..CONSOL<>'Y' AND ;
        SEEK('O'+&lcHdrFile..ORDER,lcOrdCanLn)
      SELECT (lcOrdCanLn)
      SCAN REST WHILE cOrdType+ORDER+STR(LINENO,6) = 'O'+&lcHdrFile..ORDER
        IF !SEEK(cOrdType+ORDER+STR(LINENO,6),'ORDCANLN')
          *-- Update the order canceld Quantity
          SCATTER MEMVAR
          INSERT INTO 'ORDCANLN' FROM MEMVAR
        ENDIF
      ENDSCAN
    ENDIF

    **************************************************************
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
    *WAIT 'Updating Invoice Header...' WINDOW NOWAIT
    WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATEINVHEADER,oAriaApplication.GetHeaderText("LANG_UPDATEINVHEADER",lcInvHeaderAlias)) WINDOW NOWAIT
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
    SELECT (lcHdrFile)
    SCATTER MEMVAR


    *!* B608449,1 MMT 02/24/2008 fix bug of wrong total charges value[Start]
    *B608892,1 WAM 06/11/2009 Use another variable name since lnShipAmnt already used
    *m.ShipAmnt = lnShipAmnt
    m.ShipAmnt = lnLShipAmnt
    *B608892,1 WAM 06/11/2009 (End)

    m.Ship = lnShipQty
    m.TOTALCHG = m.ShipAmt+m.COD+m.INSUR+m.FREIGHT+m.NCHARGES+m.Discount+m.TAX_AMT+m.NPSTAMT+m.NHSTAMT
    *!* B608449,1 MMT 02/24/2008 fix bug of wrong total charges value[End]
    *!* B609784,1 MMT 12/26/2011 Fix bug of updating Dsc_amt in debit file based on Shipped amount not total charges[Start]
    IF laInvSetup[11,2]
      m.trddscamnt = lnDiscAmt

      *!B610344,1 HIA 05/29/13 T20130509.0009 Trade Discount Miscalculations [Begin]
      *m.trde_disc =  IIF(m.TOTALCHG <>0,(m.trddscamnt/m.TOTALCHG )*100,m.trde_disc)
      m.trde_disc =  IIF(m.ShipAmt <>0,(m.trddscamnt/m.ShipAmt )*100,m.trde_disc)
      *!B610344,1 HIA 05/29/13 T20130509.0009 Trade Discount Miscalculations [End]

    ENDIF
    *!* B609784,1 MMT 12/26/2011 Fix bug of updating Dsc_amt in debit file based on Shipped amount not total charges[END]
    *-- shipped amount - discount - Trade discount
    lnNetShipAmnt = m.ShipAmt*(1-m.DiscPcnt/100)*(1-m.trde_disc/100)
    *!* B610183,1 HIA 12/30/2012 Commission calculation error [T20121130.0008][Start]
    IF laInvSetup[6,2]='Y' AND laInvSetup[11,2]
      lnComm1  = 0
      lnComm2  = 0
      SELECT (lcDetFile)
      = SEEK(lcSeekKey)
      SCAN REST WHILE &lcExpr = lcSeekKey
        lnLineTotqty = &lcDetFile..Totqty
        IF &lcDetFile..CONSOL = 'Y'
          lcKeyFlds = &lcDetFile..Account+&lcDetFile..cDivision+&lcDetFile..cCurrCode+&lcDetFile..STYLE+&lcDetFile..Dyelot+IIF(llConsByDc,&lcDetFile..Dist_Ctr,"")
          lnRcNum = RECNO()
          SUM Totqty FOR CONSOL+Account+cDivision+cCurrCode+STYLE+Dyelot+Dist_Ctr =;
            'N'+lcKeyFlds  TO lnLineTotqty

          IF BETWEEN(lnRcNum,1,RECCOUNT())
            GO RECORD lnRcNum
          ENDIF
        ENDIF

        lnNetAmnt = lnLineTotqty *&lcDetFile..Price*(1-m.DiscPcnt/100)*(1-m.trde_disc/100)
        lnComm1   = lnComm1 + (lnNetAmnt * &lcDetFile..Comm1 / 100)
        lnComm2   = lnComm2 + (lnNetAmnt * &lcDetFile..Comm2 / 100)
      ENDSCAN
    ENDIF
    *!* B610183,1 HIA 12/30/2012 Commission calculation error [T20121130.0008][End]
    *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
    *  IF laInvSetup[6,2]='Y' .AND. lnNetShipAmnt <> 0
    IF lnTotAmntComm <> 0
      *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
      *-- Commission  is at style level
      m.Comm1 = 0
      m.Comm2 = 0
      *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
      *!*	    IF lnNetShipAmnt > 0
      *!*	      m.Comm1 = lnComm1/lnNetShipAmnt*100
      *!*	      m.Comm2 = lnComm2/lnNetShipAmnt*100
      IF lnTotAmntComm > 0
        m.Comm1 = lnComm1/lnTotAmntComm *100
        m.Comm2 = lnComm2/lnTotAmntComm *100
        *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
      ENDIF
    ENDIF
    m.Store = IIF(m.CONSOL='Y',m.cConStore,m.Store)
    *-- sales rep Commission at Style level or at invoice level
    *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
    *!*	  m.CommAmt1 = IIF(laInvSetup[6,2]='Y',lnComm1,m.Comm1*lnNetShipAmnt/100)
    *!*	  m.CommAmt2 = IIF(laInvSetup[6,2]='Y',lnComm2,m.Comm2*lnNetShipAmnt/100)
    m.CommAmt1 = lnComm1
    m.CommAmt2 = lnComm2
    *!* B611437,1 MHM 16/10/2017 When order has non-commissionable items, the commission percentage on Comm. statements is misleading[T20171002.0034]
    *!* E302726,1 MMT 07/28/2010 Empty Field CCARTRCKNO in case of Consolidated invoice[Start]
    IF m.CONSOL='Y'
      m.CCARTRCKNO = ''
    ENDIF
    *!* E302726,1 MMT 07/28/2010 Empty Field CCARTRCKNO in case of Consolidated invoice[End]

    *-- Custom process for A.S.T. sportwear. [Begin]
    IF ASCAN(oAriaApplication.laEvntTrig , PADR('CALCAMT',10)) <> 0
      =gfDoTriger('ARIINV',PADR('CALCAMT',10))
    ENDIF
    m.Link_Code = lcCustLink
    m.Invoice   = lcInvNo
    m.Order     = lcOrdNo
    m.Status = 'C'

    *-- Add GL Entries
    STORE '' TO m.cFrgtAcnt, m.cTaxAcnt, m.cArAcnt
    IF laInvSetup[1,2]='Y' AND llConsAcc

      *-- Freight income <CREDIT> Category key '004'
      IF UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='ENG' AND !EMPTY(lcEngChrg)
        IF SEEK(&lcHdrFile..ORDER+m.Store+m.PikTkt,lcEngChrg)
          *-- the company is england so update the Gl with the english charges
          *--In case of england charges taxes is at style level
          SELECT (lcEngChrg)
          SCAN REST WHILE ORDER+STORE+PikTkt+cchrgcode = &lcHdrFile..ORDER+m.Store+m.PikTkt
            DO GLDIST WITH lcCustLink,'004',-&lcEngChrg..nChrgAmnt,'IN',;
              m.Invoice,m.dPostDate,lcGlYear,lcGlPeriod,;
              lcDistFile,&lcEngChrg..cFrgtAcnt,m.cCurrCode,m.nCurrUnit,m.nExRate
          ENDSCAN
        ENDIF
      ELSE
        *-- other charges
        *C200725,1 MMT 12/13/2006 Add Customization A'dd more charges screen' [Start]
        IF ASCAN(loFormSet.laEvntTrig,PADR("UPFRGHT",10)) <> 0
          =loFormSet.mDoTrigger(PADR("UPFRGHT",10))
        ELSE
          *C200725,1 MMT 12/13/2006 Add Customization A'dd more charges screen' [End]
          IF (m.FREIGHT+m.INSUR+m.COD) <> 0
            DO GLDIST WITH lcCustLink,'004',-(m.FREIGHT+m.INSUR+m.COD),'IN',;
              m.Invoice,m.dPostDate,lcGlYear,lcGlPeriod,;
              lcDistFile,'',m.cCurrCode,m.nCurrUnit,m.nExRate

            m.cFrgtAcnt = &lcDistFile..GlAccount   && Frieght account
          ENDIF
          *C200725,1 MMT 12/13/2006 Add Customization A'dd more charges screen' [Start]
        ENDIF
        *C200725,1 MMT 12/13/2006 Add Customization A'dd more charges screen' [End]
      ENDIF


      IF laInvSetup[7,2]='Y'
        *-- use taxes in the company setup is yes so update the Gl with income tax
        *-- Tax Income <CREDIT> Category key '014'
        IF m.TAX_AMT+m.NPSTAMT+m.NHSTAMT <> 0

          *:* E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[Start]
          *DO GLDIST WITH lcCustLink,'014',-(m.Tax_Amt+m.nPstAmt+m.nHstAmt),'IN',m.Invoice,;
          m.dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
          m.cCurrCode,m.nCurrUnit,m.nExRate

          IF UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='CANADA'
            =SEEK(IIF(EMPTY(m.Store),'M'+m.Account,'S'+m.Account+m.Store),'CUSTOMER')
            IF lfChkStatHst(LEFT(Customer.cAddress4,6))
              DO GLDIST WITH lcCustLink,'030',- m.NHSTAMT,'IN',m.Invoice,;
                m.dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
                m.cCurrCode,m.nCurrUnit,m.nExRate
            ELSE

              DO GLDIST WITH lcCustLink,'014',- m.TAX_AMT,'IN',m.Invoice,;
                m.dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
                m.cCurrCode,m.nCurrUnit,m.nExRate

              DO GLDIST WITH lcCustLink,'029',- m.NPSTAMT,'IN',m.Invoice,;
                m.dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
                m.cCurrCode,m.nCurrUnit,m.nExRate
            ENDIF
          ELSE
            DO GLDIST WITH lcCustLink,'014',-(m.TAX_AMT+m.NPSTAMT+m.NHSTAMT),'IN',m.Invoice,;
              m.dPostDate,lcGlYear,lcGlPeriod,lcDistFile,'',;
              m.cCurrCode,m.nCurrUnit,m.nExRate
          ENDIF
          *:* E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[End]

          m.cTaxAcnt = &lcDistFile..GlAccount && Tax Account
        ENDIF
      ENDIF
      *-- Account receivable <DEBIT> Category key '001'
      IF m.TOTALCHG <> 0
        DO GLDIST WITH lcCustLink,'001',m.TOTALCHG,'IN',m.Invoice,m.dPostDate,;
          lcGlYear,lcGlPeriod,lcDistFile,'',m.cCurrCode,m.nCurrUnit,m.nExRate
        lnEqv001=0

        *:* E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[Start]
        *SUM nEqvAmnt FOR Tran_No = m.Invoice AND (Catg_Key='003' OR Catg_Key='004' OR Catg_Key='005' OR Catg_Key='014') TO  lnEqv001
        *!* B610963,1 MMT 03/11/2015 Invoice updates neqvamnt incorrectly in GLDIST and wrong invline record in case of consolidated invoice[T20150310.0013][Start]
        *SUM nEqvAmnt FOR Tran_No = m.Invoice AND (Catg_Key='003' OR Catg_Key='004' OR Catg_Key='005' OR Catg_Key='014' OR Catg_Key='029' OR Catg_Key='030') TO  lnEqv001
        *!* B610963,1 MMT 03/11/2015 Invoice updates neqvamnt incorrectly in GLDIST and wrong invline record in case of consolidated invoice[T20150310.0013][End]
        *:* E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[End]
		
        LOCATE FOR  Tran_No =m.Invoice AND Catg_Key='001'
        *!* B610963,1 MMT 03/11/2015 Invoice updates neqvamnt incorrectly in GLDIST and wrong invline record in case of consolidated invoice[T20150310.0013][Start]
        *REPLACE nEqvAmnt WITH  ABS(lnEqv001)
        *!* B610963,1 MMT 03/11/2015 Invoice updates neqvamnt incorrectly in GLDIST and wrong invline record in case of consolidated invoice[T20150310.0013][End]
        m.cArAcnt = &lcDistFile..GlAccount   && Account recievable account
      ENDIF
    ENDIF

    *C200725,1 MMT 12/13/2006 Add Customization A'dd more charges screen' [Start]
    IF ASCAN(loFormSet.laEvntTrig,PADR("UPFRGHT",10)) <> 0
      =loFormSet.mDoTrigger(PADR("UPFRGHT",10))
    ENDIF
    *C200725,1 MMT 12/13/2006 Add Customization A'dd more charges screen' [End]


    IF llConsAcc
      *B609730,1 WAM 11/14/2011 Zero out commission percent and amount since it will be accumulated later for each store record
      IF m.CONSOL = 'Y'
        STORE 0 TO m.CommAmt1,m.CommAmt2,m.Comm1 ,m.Comm2
        *E303420,1 MMT 09/26/2013 Add option to Consolidate invoices by order - store in Invoice sales order screen[T20130814.0004][Start]
        IF llConsStore
          m.Dist_Ctr = ''
        ENDIF
        *E303420,1 MMT 09/26/2013 Add option to Consolidate invoices by order - store in Invoice sales order screen[T20130814.0004][End]
      ENDIF
      *B609730,1 WAM 11/14/2011 (End)
      *!* E303504,1 MMT 09/07/2014 Add triggers to consolidate invoice by contract reference[T20140819.0035][Start]
      IF ASCAN(loFormSet.laEvntTrig,PADR('CLRDISTCTR',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
        =loFormSet.mDoTrigger(PADR('CLRDISTCTR',10))
      ENDIF
      *!* E303504,1 MMT 09/07/2014 Add triggers to consolidate invoice by contract reference[T20140819.0035][End]
      INSERT INTO InvHdr FROM MEMVAR
      *!* E612339,1 MMT 02/10/2021 Invoice Saving does not update Entity table when Invoice is created/Voided [T20201008.0003][Start]
      IF TYPE('loFormSet') ="O"
        loFormSet.mupdateentitytable()
      ENDIF
      *!* E612339,1 MMT 02/10/2021 Invoice Saving does not update Entity table when Invoice is created/Voided [T20201008.0003][End]
      *-- Add a triger functions for the custome charges screen.
      IF ASCAN(oAriaApplication.laEvntTrig , PADR('JLINSERT',10)) <> 0
        =gfDoTriger('ARIINV',PADR('JLINSERT',10))
      ENDIF
    ELSE
      *-- update the conslidated invoice Header file
      INSERT INTO CONSINVH FROM MEMVAR

      *B608892,1 WAM 06/11/2009 Fix sales rep commission percent calculation for consolidated invoices
      *B608314,1 MMT 10/11/2007 fix bug of wrong Ship Amt In Commession report[Start]
      *REPLACE Commamt1 WITH Commamt1+m.commamt1,;
      Commamt2 WITH Commamt2+m.Commamt2,;
      Comm1    WITH  (Commamt1/ShipAmt*(1-DiscPcnt/100)*(1-Trde_Disc/100))*100,;
      Comm2    WITH  (Commamt2/ShipAmt*(1-DiscPcnt/100)*(1-Trde_Disc/100))*100 IN InvHdr
      *B608314,1 MMT 10/11/2007 fix bug of wrong Ship Amt In Commession report[End]
      *B609899,1 MMT 04/30/2012 Error while creating consolidated invoice with style price 0[Start]
      *!*      REPLACE Commamt1 WITH Commamt1+m.commamt1,;
      *!*      		Commamt2 WITH Commamt2+m.Commamt2,;
      *!*       		Comm1    WITH  Commamt1/(ShipAmt*(1-DiscPcnt/100)*(1-Trde_Disc/100))*100,;
      *!*       		Comm2    WITH  Commamt2/(ShipAmt*(1-DiscPcnt/100)*(1-Trde_Disc/100))*100 IN InvHdr
      REPLACE CommAmt1 WITH CommAmt1+m.CommAmt1,;
        CommAmt2 WITH CommAmt2+m.CommAmt2,;
        Comm1    WITH  IIF(ShipAmt<> 0,CommAmt1/(ShipAmt*(1-DiscPcnt/100)*(1-trde_disc/100))*100,0),;
        Comm2    WITH  IIF(ShipAmt<> 0,CommAmt2/(ShipAmt*(1-DiscPcnt/100)*(1-trde_disc/100))*100,0) IN InvHdr
      *B609899,1 MMT 04/30/2012 Error while creating consolidated invoice with style price 0[END]
      *B608892,1 WAM 06/11/2009 (End)

    ENDIF
    IF m.CONSOL <> 'Y'
      *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[START]
      *IF !SEEK('O'+m.Order,'ORDHDR')
      IF !SEEK('O'+m.Order,'ORDHDR','ORDHDR')
        *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[End]
        *-- direct invoice so create order
        SELECT ORDHDR
        INSERT INTO ORDHDR (cOrdType,ORDER,STATUS,Account,STORE,CustPo) VALUES ;
          ('O',m.Order,'C',m.Account,m.Store,m.CustPo)

        lcFactAcct = ''
        =SEEK(IIF(EMPTY(m.Store),'M'+m.Account,'S'+m.Account+m.Store),'CUSTOMER')
        =RLOCK()
        REPLACE Dept      WITH m.Dept  ,;
          Note1     WITH m.Note1 ,;
          Note2     WITH m.Note2 ,;
          Priority  WITH Customer.Priority ,;
          Season    WITH m.Season ,;
          cDivision WITH m.cDivision ,;
          Bulk      WITH 'N' ,;
          cReOrder  WITH 'N' ,;
          CONSOL    WITH 'N' ,;
          MULTI     WITH 'N' ,;
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
          Comm1      WITH m.Comm1 ,;
          Rep2       WITH m.Rep2     ,;
          Comm2      WITH m.Comm2 ,;
          Entered    WITH m.InvDate  ,;
          START      WITH m.InvDate  ,;
          COMPLETE   WITH m.InvDate  ,;
          Disc       WITH m.DiscPcnt ,;
          Book       WITH m.Ship     ,;
          BookAmt    WITH m.ShipAmt  ,;
          Ship       WITH m.Ship     ,;
          ShipAmt    WITH m.ShipAmt  ,;
          LastLine   WITH m.LastLine ,;
          Link_Code  WITH lcCustLink ,;
          dAdd_Date  WITH oAriaApplication.SystemDate   ,;
          cAdd_Time  WITH TIME()      ,;
          cAdd_User  WITH oAriaApplication.User_ID   ,;
          Distrb_No  WITH Customer.Dist_Ctr ,;
          Direct_Inv WITH .T.

        *-- Custom process for A.S.T. sportwear.
        IF ASCAN(oAriaApplication.laEvntTrig , PADR('BUILDORD',10)) <> 0
          =gfDoTriger('ARDINV',PADR('BUILDORD',10))
        ENDIF
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
      ELSE
        *-- sales order invoice so decrease the approved amouunt with total Charges
        SELECT ORDHDR
        =RLOCK()
        REPLACE ApprAmt WITH MAX(ApprAmt - m.ShipAmt,0)
        *!* B612564,1 MMT 05/18/2022 When the same user edits SO has a Picking ticket, then Invoiced this SO, and then saved the SO, the SO status became open again while it should be completed[T20220511.0004][Stat]
        =gfAdd_Info('ORDHDR')
        *!* B612564,1 MMT 05/18/2022 When the same user edits SO has a Picking ticket, then Invoiced this SO, and then saved the SO, the SO status became open again while it should be completed[T20220511.0004][End]

        UNLOCK
      ENDIF
    ENDIF
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
    *WAIT 'Updating Sales Representative Information...' WINDOW NOWAIT
    WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATESRINFO,oAriaApplication.GetHeaderText("LANG_UPDATESRINFO",lcInvHeaderAlias)) WINDOW NOWAIT
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
    IF m.CONSOL<>'Y' .AND. SEEK(m.Rep1,'SALESREP')
      SELECT SALESREP
      lnEqvAmnt = ROUND(m.CommAmt1 &lcExRSin m.nExRate &lcUntSin m.nCurrUnit,2)
      IF lnEqvAmnt > 0
        =RLOCK()
        *-- accumelate the sales rep commission
        REPLACE CURRENT WITH CURRENT + lnEqvAmnt ,;
          BALANCE WITH BALANCE + lnEqvAmnt
        UNLOCK
        SELECT REPCOMM
        *-- add new line to rep commission file
        APPEND BLANK
        =RLOCK()
        *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
        *!*	      REPLACE REPCODE   WITH m.Rep1      ,;
        *!*	        TRANTYPE  WITH '1'         ,;
        *!*	        STATUS    WITH 'O'         ,;
        *!*	        TRAN      WITH m.Invoice   ,;
        *!*	        ACCOUNT   WITH m.Account   ,;
        *!*	        ORDER     WITH m.Order     ,;
        *!*	        CUSTPO    WITH m.CustPo    ,;
        *!*	        STORE     WITH m.Store     ,;
        *!*	        DATE      WITH m.InvDate   ,;
        *!*	        DESC      WITH 'INVOICE'   ,;
        *!*	        AMOUNT    WITH lnEqvAmnt   ,;
        *!*	        COMMPCNT  WITH m.Comm1     ,;
        *!*	        nForAmnt  WITH m.CommAmt1  ,;
        *!*	        cCurrCode WITH m.cCurrCode ,;
        *!*	        nCurrUnit WITH m.nCurrUnit ,;
        *!*	        nExRate   WITH m.nExRate   ,;
        *!*	        dAdd_Date WITH oAriaApplication.SystemDate   ,;
        *!*	        cAdd_Time WITH TIME()      ,;
        *!*	        cAdd_User WITH oAriaApplication.User_ID
        REPLACE REPCODE   WITH m.Rep1      ,;
          TRANTYPE  WITH '1'         ,;
          STATUS    WITH 'O'         ,;
          TRAN      WITH m.Invoice   ,;
          Account   WITH m.Account   ,;
          ORDER     WITH m.Order     ,;
          CustPo    WITH m.CustPo    ,;
          STORE     WITH m.Store     ,;
          DATE      WITH m.InvDate   ,;
          DESC      WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVOICEDESC,oAriaApplication.GetHeaderText("LANG_INVOICEDESC",lcInvHeaderAlias))   ,;
          AMOUNT    WITH lnEqvAmnt   ,;
          COMMPCNT  WITH m.Comm1     ,;
          nForAmnt  WITH m.CommAmt1  ,;
          cCurrCode WITH m.cCurrCode ,;
          nCurrUnit WITH m.nCurrUnit ,;
          nExRate   WITH m.nExRate   ,;
          dAdd_Date WITH oAriaApplication.SystemDate   ,;
          cAdd_Time WITH TIME()      ,;
          cAdd_User WITH oAriaApplication.User_ID
        *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
        *B608314,1 MMT 10/11/2007 fix bug of wrong Ship Amt In Commession report[Start]
        REPLACE PikTkt WITH m.PikTkt
        *B608314,1 MMT 10/11/2007 fix bug of wrong Ship Amt In Commession report[End]
        UNLOCK
      ENDIF
    ENDIF
    IF m.CONSOL<>'Y' .AND. SEEK(m.Rep2,'SALESREP')
      SELECT SALESREP
      lnEqvAmnt = ROUND(m.CommAmt2 &lcExRSin m.nExRate &lcUntSin m.nCurrUnit,2)
      IF lnEqvAmnt > 0
        =RLOCK()
        REPLACE CURRENT WITH CURRENT + lnEqvAmnt ,;
          BALANCE WITH BALANCE + lnEqvAmnt
        *-- Amin
        UNLOCK
        *-- Amin
        SELECT REPCOMM
        APPEND BLANK
        =RLOCK()
        *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
        *!*	      REPLACE REPCODE   WITH m.Rep2      ,;
        *!*	        TRANTYPE  WITH '1'         ,;
        *!*	        STATUS    WITH 'O'         ,;
        *!*	        TRAN      WITH m.Invoice   ,;
        *!*	        ACCOUNT   WITH m.Account   ,;
        *!*	        ORDER     WITH m.Order     ,;
        *!*	        CUSTPO    WITH m.CustPo    ,;
        *!*	        STORE     WITH m.Store     ,;
        *!*	        DATE      WITH m.InvDate   ,;
        *!*	        DESC      WITH 'INVOICE'   ,;
        *!*	        AMOUNT    WITH lnEqvAmnt   ,;
        *!*	        COMMPCNT  WITH m.Comm2     ,;
        *!*	        nForAmnt  WITH m.CommAmt2  ,;
        *!*	        cCurrCode WITH m.cCurrCode ,;
        *!*	        nCurrUnit WITH m.nCurrUnit ,;
        *!*	        nExRate   WITH m.nExRate   ,;
        *!*	        dAdd_Date WITH oAriaApplication.SystemDate   ,;
        *!*	        cAdd_Time WITH TIME()      ,;
        *!*	        cAdd_User WITH oAriaApplication.User_ID
        REPLACE REPCODE   WITH m.Rep2      ,;
          TRANTYPE  WITH '1'         ,;
          STATUS    WITH 'O'         ,;
          TRAN      WITH m.Invoice   ,;
          Account   WITH m.Account   ,;
          ORDER     WITH m.Order     ,;
          CustPo    WITH m.CustPo    ,;
          STORE     WITH m.Store     ,;
          DATE      WITH m.InvDate   ,;
          DESC      WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVOICEDESC,oAriaApplication.GetHeaderText("LANG_INVOICEDESC",lcInvHeaderAlias))  ,;
          AMOUNT    WITH lnEqvAmnt   ,;
          COMMPCNT  WITH m.Comm2     ,;
          nForAmnt  WITH m.CommAmt2  ,;
          cCurrCode WITH m.cCurrCode ,;
          nCurrUnit WITH m.nCurrUnit ,;
          nExRate   WITH m.nExRate   ,;
          dAdd_Date WITH oAriaApplication.SystemDate   ,;
          cAdd_Time WITH TIME()      ,;
          cAdd_User WITH oAriaApplication.User_ID
        *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
        *B608314,1 MMT 10/11/2007 fix bug of wrong Ship Amt In Commession report[Start]
        REPLACE PikTkt WITH m.PikTkt
        *B608314,1 MMT 10/11/2007 fix bug of wrong Ship Amt In Commession report[End]


        UNLOCK
      ENDIF
    ENDIF
    *-- Custom process for A.S.T. sportwear.
    IF ASCAN(oAriaApplication.laEvntTrig , PADR('CALCREP3',10)) <> 0
      =gfDoTriger('ARIINV',PADR('CALCREP3',10))
    ENDIF

    IF llConsAcc
      *-- Update the instalment heder and detail and the debit file
      *-- variable to check if it was sales order invoice or not
      IF !EMPTY(lcInstHdr) AND !EMPTY(lcInstLine) AND ;
          SEEK(&lcHdrFile..ORDER+m.Store+m.PikTkt,lcInstHdr) .AND. ;
          SEEK(&lcHdrFile..ORDER+m.Store+m.PikTkt,lcInstLine)
        *-- there is instalments in the invoice so open the files to update it
        =gfOpenFile(oAriaApplication.DataDir+'ARINSTMH', oAriaApplication.DataDir+'Arinstmh','SH')
        =gfOpenFile(oAriaApplication.DataDir+'ARINSTMD', oAriaApplication.DataDir+'Arinstmd','SH')

        IF !SEEK(m.Invoice,'ARINSTMH')
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
            cAdd_User  WITH oAriaApplication.User_ID ,;
            cAdd_Time  WITH TIME() ,;
            dAdd_Date  WITH oAriaApplication.SystemDate
        ENDIF
        *-- update the instalment Detail
        IF &lcInstHdr..nInstIAmnt > 0
          IF !SEEK(m.Invoice+SPACE(3),'ARINSTMD')
            SELECT ARINSTMD
            APPEND BLANK
            *-- add the first line with the initial amount
            *HBG 12/26/2004 Fix bug of due date in initial record updated with invoice date [Begin]
            *!*	          REPLACE Invoice    WITH m.Invoice ,;
            *!*	                  cinstalno  WITH SPACE(3)  ,;
            *!*	                  nInstmAmnt WITH &lcInstHdr..nInstIAmnt ,;
            *!*	                  DueDate    WITH m.InvDate ,;
            *!*	                  nInstmPcnt WITH IIF(m.TotalChg>0,&lcInstHdr..nInstIAmnt*100/m.TotalChg ,0),;
            *!*	                  cInstmNote WITH ''        ,;
            *!*	                  cAdd_User  WITH oAriaApplication.User_ID ,;
            *!*	                  cAdd_Time  WITH TIME()    ,;
            *!*	                  dAdd_Date  WITH oAriaApplication.SystemDate
            REPLACE Invoice    WITH m.Invoice ,;
              cinstalno  WITH SPACE(3)  ,;
              nInstmAmnt WITH &lcInstHdr..nInstIAmnt ,;
              DueDate    WITH m.DueDate ,;
              nInstmPcnt WITH IIF(m.TOTALCHG>0,&lcInstHdr..nInstIAmnt*100/m.TOTALCHG ,0),;
              cInstmNote WITH ''        ,;
              cAdd_User  WITH oAriaApplication.User_ID ,;
              cAdd_Time  WITH TIME()    ,;
              dAdd_Date  WITH oAriaApplication.SystemDate
            *HBG [End]
          ENDIF
          IF (EMPTY(m.cFacCode) .OR. laInvSetup[8,2]='Y') .AND. ;
              !SEEK(m.Account+m.Invoice+SPACE(3)+DTOS(m.InvDate),'DEBIT')
            *-- the invoice is not factored or post factored invoice = Yes
            *-- update the debit file
            *!* B608258,1 MMT 09/06/2007 don't Add record in debit file for Zero Amount invoices[Start]
            IF &lcInstHdr..nInstIAmnt > 0
              *!* B608258,1 MMT 09/06/2007 don't Add record in debit file for Zero Amount invoices[End]
              SELECT DEBIT
              APPEND BLANK
              =RLOCK()
              *!* B609784,1 MMT 12/26/2011 Fix bug of updating Dsc_amt in debit file based on Shipped amount not total charges[Start]
              *!*            REPLACE cInstalNO WITH SPACE(3)    ,;
              *!*                    DUEDATE   WITH m.InvDate   ,;
              *!*                    dPostDate WITH m.dPostDate ,;
              *!*                    REFERENCE WITH m.CustPo    ,;
              *!*                    TRANTYPE  WITH '1'         ,;
              *!*                    STORE     WITH m.Store     ,;
              *!*                    ACCOUNT   WITH m.Account   ,;
              *!*                    cFacCode  WITH m.cFacCode  ,;
              *!*                    TRAN      WITH m.Invoice   ,;
              *!*                    TRANDATE  WITH m.InvDate   ,;
              *!*                    AMOUNT    WITH &lcInstHdr..nInstIAmnt ,;
              *!*                    DSC_AMT   WITH (m.ShipAmt+m.Discount)*m.Trde_Disc/100 ,;
              *!*                    cCurrCode WITH m.cCurrCode ,;
              *!*                    nCurrUnit WITH m.nCurrUnit ,;
              *!*                    nExRate   WITH m.nExRate   ,;
              *!*                    cArGlAcc  WITH m.cArAcnt   ,;
              *!*                    dAdd_Date WITH oAriaApplication.SystemDate   ,;
              *!*                    cAdd_Time WITH TIME()      ,;
              *!*                    cAdd_User WITH oAriaApplication.User_ID   ,;
              *!*                    DESC      WITH IIF(m.DIRECT_INV,'DIRECT INVOICE','SALES ORDER INVOICE')
              *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
              *!*	            REPLACE cInstalNO WITH SPACE(3)    ,;
              *!*	              DUEDATE   WITH m.InvDate   ,;
              *!*	              dPostDate WITH m.dPostDate ,;
              *!*	              REFERENCE WITH m.CustPo    ,;
              *!*	              TRANTYPE  WITH '1'         ,;
              *!*	              STORE     WITH m.Store     ,;
              *!*	              ACCOUNT   WITH m.Account   ,;
              *!*	              cFacCode  WITH m.cFacCode  ,;
              *!*	              TRAN      WITH m.Invoice   ,;
              *!*	              TRANDATE  WITH m.InvDate   ,;
              *!*	              AMOUNT    WITH &lcInstHdr..nInstIAmnt ,;
              *!*	              DSC_AMT   WITH m.TOTALCHG*m.Trde_Disc/100 ,;
              *!*	              cCurrCode WITH m.cCurrCode ,;
              *!*	              nCurrUnit WITH m.nCurrUnit ,;
              *!*	              nExRate   WITH m.nExRate   ,;
              *!*	              cArGlAcc  WITH m.cArAcnt   ,;
              *!*	              dAdd_Date WITH oAriaApplication.SystemDate   ,;
              *!*	              cAdd_Time WITH TIME()      ,;
              *!*	              cAdd_User WITH oAriaApplication.User_ID   ,;
              *!*	              DESC      WITH IIF(m.DIRECT_INV,'DIRECT INVOICE','SALES ORDER INVOICE')

              *!B610344,1 HIA 05/29/13 T20130509.0009 Trade Discount Miscalculations [Begin]
              *!*	            REPLACE cInstalNO WITH SPACE(3)    ,;
              *!*	              DUEDATE   WITH m.InvDate   ,;
              *!*	              dPostDate WITH m.dPostDate ,;
              *!*	              REFERENCE WITH m.CustPo    ,;
              *!*	              TRANTYPE  WITH '1'         ,;
              *!*	              STORE     WITH m.Store     ,;
              *!*	              ACCOUNT   WITH m.Account   ,;
              *!*	              cFacCode  WITH m.cFacCode  ,;
              *!*	              TRAN      WITH m.Invoice   ,;
              *!*	              TRANDATE  WITH m.InvDate   ,;
              *!*	              AMOUNT    WITH &lcInstHdr..nInstIAmnt ,;
              *!*	              DSC_AMT   WITH m.TOTALCHG*m.Trde_Disc/100 ,;
              *!*	              cCurrCode WITH m.cCurrCode ,;
              *!*	              nCurrUnit WITH m.nCurrUnit ,;
              *!*	              nExRate   WITH m.nExRate   ,;
              *!*	              cArGlAcc  WITH m.cArAcnt   ,;
              *!*	              dAdd_Date WITH oAriaApplication.SystemDate   ,;
              *!*	              cAdd_Time WITH TIME()      ,;
              *!*	              cAdd_User WITH oAriaApplication.User_ID   ,;
              *!*	              DESC      WITH IIF(m.DIRECT_INV,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DIRECTINV,oAriaApplication.GetHeaderText("LANG_DIRECTINV",lcInvHeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVSO,oAriaApplication.GetHeaderText("LANG_INVSO",lcInvHeaderAlias)))


              REPLACE cinstalno WITH SPACE(3)    ,;
                DueDate   WITH m.InvDate   ,;
                dPostDate WITH m.dPostDate ,;
                REFERENCE WITH m.CustPo    ,;
                TRANTYPE  WITH '1'         ,;
                STORE     WITH m.Store     ,;
                Account   WITH m.Account   ,;
                cFacCode  WITH m.cFacCode  ,;
                TRAN      WITH m.Invoice   ,;
                TRANDATE  WITH m.InvDate   ,;
                AMOUNT    WITH &lcInstHdr..nInstIAmnt ,;
                DSC_AMT   WITH m.ShipAmt * m.trde_disc/100 ,;
                cCurrCode WITH m.cCurrCode ,;
                nCurrUnit WITH m.nCurrUnit ,;
                nExRate   WITH m.nExRate   ,;
                cArGlAcc  WITH m.cArAcnt   ,;
                dAdd_Date WITH oAriaApplication.SystemDate   ,;
                cAdd_Time WITH TIME()      ,;
                cAdd_User WITH oAriaApplication.User_ID   ,;
                DESC      WITH IIF(m.Direct_Inv,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DIRECTINV,oAriaApplication.GetHeaderText("LANG_DIRECTINV",lcInvHeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVSO,oAriaApplication.GetHeaderText("LANG_INVSO",lcInvHeaderAlias)))

              *!B610344,1 HIA 05/29/13 T20130509.0009 Trade Discount Miscalculations [End]

              *E303386,1 TMI 05/08/2013 [Start] calculate trade discount based on ship amnt and chage
              IF UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='ENG'
                *!* B611016,1 MMT 06/15/2015 Invoice saving calculates Trade discount incorrectly for UK companies[T20150602.0002][Start]
                *REPLACE DSC_AMT   WITH (m.SHIPAMT+m.NCHARGES+m.Discount)*m.Trde_Disc/100
                REPLACE DSC_AMT   WITH (m.ShipAmt+m.NCHARGES+m.TAX_AMT+m.Discount)*m.trde_disc/100
                *!* B611016,1 MMT 06/15/2015 Invoice saving calculates Trade discount incorrectly for UK companies[T20150602.0002][End]
              ENDIF
              *E303386,1 TMI 05/08/2013 [End  ]

              *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
              *!* B609784,1 MMT 12/26/2011 Fix bug of updating Dsc_amt in debit file based on Shipped amount not total charges[END]
              UNLOCK
              *!* B608258,1 MMT 09/06/2007 don't Add record in debit file for Zero Amount invoices[Start]
            ENDIF
            *!* B608258,1 MMT 09/06/2007 don't Add record in debit file for Zero Amount invoices[End]
            *!* B608253,1 MMT 09/05/07 add log file to check the problem of not saving debit record[Start]
          ELSE
            lcStrVar = "llConsAcc= "+IIF(llConsAcc ,"TRUE","False")+" m.cFacCode = "+m.cFacCode +" laInvSetup[8,2] = "+laInvSetup[8,2]+;
              "  m.Invoice = "+ m.Invoice
            =STRTOFILE(lcStrVar ,oAriaApplication.Defaultpath+lcTmpTxtFl+".txt")
            *!* B608253,1 MMT 09/05/07 add log file to check the problem of not saving debit record[End]
          ENDIF
        ENDIF
        SELECT (lcInstLine)
        *-- update all the lines in the instalment detail file and the debit file
        SCAN REST WHILE ORDER+STORE+PikTkt+cinstalno = &lcHdrFile..ORDER+m.Store+m.PikTkt
          IF !SEEK(m.Invoice+cinstalno,'ARINSTMD')
            *-- update the instalment lines if it was not updated in the last uncomplete session
            SELECT ARINSTMD
            APPEND BLANK
            REPLACE Invoice    WITH m.Invoice ,;
              cinstalno  WITH &lcInstLine..cinstalno ,;
              nInstmAmnt WITH &lcInstLine..nInstmAmnt ,;
              DueDate    WITH &lcInstLine..DueDate ,;
              nInstmPcnt WITH &lcInstLine..nInstmPcnt ,;
              cInstmNote WITH &lcInstLine..cInstmNote ,;
              cAdd_User  WITH oAriaApplication.User_ID ,;
              cAdd_Time  WITH TIME() ,;
              dAdd_Date  WITH oAriaApplication.SystemDate
          ENDIF
          IF (EMPTY(m.cFacCode) .OR. laInvSetup[8,2]='Y') .AND. ;
              !SEEK(m.Account+m.Invoice+&lcInstLine..cinstalno+DTOS(m.InvDate),'DEBIT')
            SELECT DEBIT
            APPEND BLANK
            =RLOCK()
            *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
            *!*	          REPLACE cInstalNO WITH &lcInstLine..cInstalNO ,;
            *!*	            DUEDATE   WITH &lcInstLine..DueDate   ,;
            *!*	            dPostDate WITH m.dPostDate ,;
            *!*	            REFERENCE WITH m.CustPo    ,;
            *!*	            TRANTYPE  WITH '1'         ,;
            *!*	            STORE     WITH m.Store     ,;
            *!*	            ACCOUNT   WITH m.Account   ,;
            *!*	            cFacCode  WITH m.cFacCode  ,;
            *!*	            TRAN      WITH m.Invoice   ,;
            *!*	            TRANDATE  WITH m.InvDate   ,;
            *!*	            AMOUNT    WITH &lcInstLine..nInstmAmnt ,;
            *!*	            cCurrCode WITH m.cCurrCode ,;
            *!*	            nCurrUnit WITH m.nCurrUnit ,;
            *!*	            nExRate   WITH m.nExRate   ,;
            *!*	            cArGlAcc  WITH m.cArAcnt   ,;
            *!*	            dAdd_Date WITH oAriaApplication.SystemDate   ,;
            *!*	            cAdd_Time WITH TIME()      ,;
            *!*	            cAdd_User WITH oAriaApplication.User_ID   ,;
            *!*	            DESC      WITH IIF(m.DIRECT_INV,'DIRECT INVOICE','SALES ORDER INVOICE')
            REPLACE cinstalno WITH &lcInstLine..cinstalno ,;
              DueDate   WITH &lcInstLine..DueDate   ,;
              dPostDate WITH m.dPostDate ,;
              REFERENCE WITH m.CustPo    ,;
              TRANTYPE  WITH '1'         ,;
              STORE     WITH m.Store     ,;
              Account   WITH m.Account   ,;
              cFacCode  WITH m.cFacCode  ,;
              TRAN      WITH m.Invoice   ,;
              TRANDATE  WITH m.InvDate   ,;
              AMOUNT    WITH &lcInstLine..nInstmAmnt ,;
              cCurrCode WITH m.cCurrCode ,;
              nCurrUnit WITH m.nCurrUnit ,;
              nExRate   WITH m.nExRate   ,;
              cArGlAcc  WITH m.cArAcnt   ,;
              dAdd_Date WITH oAriaApplication.SystemDate   ,;
              cAdd_Time WITH TIME()      ,;
              cAdd_User WITH oAriaApplication.User_ID   ,;
              DESC      WITH IIF(m.Direct_Inv,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DIRECTINV,oAriaApplication.GetHeaderText("LANG_DIRECTINV",lcInvHeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVSO,oAriaApplication.GetHeaderText("LANG_INVSO",lcInvHeaderAlias)))
            *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
            UNLOCK
          ENDIF
        ENDSCAN
        IF (EMPTY(m.cFacCode) .OR. laInvSetup[8,2]='Y') .AND. ;
            &lcInstHdr..nInstIAmnt = 0
          SELECT DEBIT
          =RLOCK()
          REPLACE DSC_AMT WITH (m.ShipAmt+m.Discount)*m.trde_disc/100
          *E303386,1 TMI 05/08/2013 [Start] calculate trade discount based on ship amnt and chage
          IF UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='ENG'
            *!* B611016,1 MMT 06/15/2015 Invoice saving calculates Trade discount incorrectly for UK companies[T20150602.0002][Start]
            *REPLACE DSC_AMT   WITH (m.SHIPAMT+m.NCHARGES+m.Discount)*m.Trde_Disc/100
            REPLACE DSC_AMT   WITH (m.ShipAmt+m.NCHARGES+m.TAX_AMT+m.Discount)*m.trde_disc/100
            *!* B611016,1 MMT 06/15/2015 Invoice saving calculates Trade discount incorrectly for UK companies[T20150602.0002][End]
          ENDIF
          *E303386,1 TMI 05/08/2013 [End  ]
          UNLOCK
        ENDIF
      ELSE
        *-- there is no instalments so update the debit file only
        IF (EMPTY(m.cFacCode) .OR. laInvSetup[8,2]='Y') .AND. ;
            !SEEK(m.Account+m.Invoice+SPACE(3)+DTOS(m.InvDate),'DEBIT')
          *!* B608258,1 MMT 09/06/2007 don't Add record in debit file for Zero Amount invoices[Start]
          IF m.TOTALCHG  > 0
            *!* B608258,1 MMT 09/06/2007 don't Add record in debit file for Zero Amount invoices[End]
            *-- post factored invoice is yes
            SELECT DEBIT
            APPEND BLANK
            =RLOCK()
            *!* B609784,1 MMT 12/26/2011 Fix bug of updating Dsc_amt in debit file based on Shipped amount not total charges[Start]
            *!*          REPLACE DUEDATE   WITH m.DueDate   ,;
            *!*                  REFERENCE WITH m.CustPo    ,;
            *!*                  dPostDate WITH m.dPostDate ,;
            *!*                  TRANTYPE  WITH '1'         ,;
            *!*                  STORE     WITH m.Store     ,;
            *!*                  ACCOUNT   WITH m.Account   ,;
            *!*                  cFacCode  WITH m.cFacCode  ,;
            *!*                  TRAN      WITH m.Invoice   ,;
            *!*                  TRANDATE  WITH m.InvDate   ,;
            *!*                  AMOUNT    WITH m.TotalChg  ,;
            *!*                  cCurrCode WITH m.cCurrCode ,;
            *!*                  nCurrUnit WITH m.nCurrUnit ,;
            *!*                  nExRate   WITH m.nExRate   ,;
            *!*                  cArGlAcc  WITH m.cArAcnt   ,;
            *!*                  DSC_AMT   WITH (m.ShipAmt+m.Discount)*m.Trde_Disc/100,;
            *!*                  DESC      WITH IIF(m.DIRECT_INV,'DIRECT INVOICE','SALES ORDER INVOICE') ,;
            *!*                  dAdd_Date WITH oAriaApplication.SystemDate   ,;
            *!*                  cAdd_Time WITH TIME()      ,;
            *!*                  cAdd_User WITH oAriaApplication.User_ID
            *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
            *!*	          REPLACE DUEDATE   WITH m.DueDate   ,;
            *!*	            REFERENCE WITH m.CustPo    ,;
            *!*	            dPostDate WITH m.dPostDate ,;
            *!*	            TRANTYPE  WITH '1'         ,;
            *!*	            STORE     WITH m.Store     ,;
            *!*	            ACCOUNT   WITH m.Account   ,;
            *!*	            cFacCode  WITH m.cFacCode  ,;
            *!*	            TRAN      WITH m.Invoice   ,;
            *!*	            TRANDATE  WITH m.InvDate   ,;
            *!*	            AMOUNT    WITH m.TotalChg  ,;
            *!*	            cCurrCode WITH m.cCurrCode ,;
            *!*	            nCurrUnit WITH m.nCurrUnit ,;
            *!*	            nExRate   WITH m.nExRate   ,;
            *!*	            cArGlAcc  WITH m.cArAcnt   ,;
            *!*	            DSC_AMT   WITH m.TOTALCHG*m.Trde_Disc/100,;
            *!*	            DESC      WITH IIF(m.DIRECT_INV,'DIRECT INVOICE','SALES ORDER INVOICE') ,;
            *!*	            dAdd_Date WITH oAriaApplication.SystemDate   ,;
            *!*	            cAdd_Time WITH TIME()      ,;
            *!*	            cAdd_User WITH oAriaApplication.User_ID
            *!B610344,1 HIA 05/29/13 T20130509.0009 Trade Discount Miscalculations [Begin]
            *!*	          REPLACE DUEDATE   WITH m.DueDate   ,;
            *!*	            REFERENCE WITH m.CustPo    ,;
            *!*	            dPostDate WITH m.dPostDate ,;
            *!*	            TRANTYPE  WITH '1'         ,;
            *!*	            STORE     WITH m.Store     ,;
            *!*	            ACCOUNT   WITH m.Account   ,;
            *!*	            cFacCode  WITH m.cFacCode  ,;
            *!*	            TRAN      WITH m.Invoice   ,;
            *!*	            TRANDATE  WITH m.InvDate   ,;
            *!*	            AMOUNT    WITH m.TotalChg  ,;
            *!*	            cCurrCode WITH m.cCurrCode ,;
            *!*	            nCurrUnit WITH m.nCurrUnit ,;
            *!*	            nExRate   WITH m.nExRate   ,;
            *!*	            cArGlAcc  WITH m.cArAcnt   ,;
            *!*	            DSC_AMT   WITH m.TOTALCHG*m.Trde_Disc/100,;
            *!*	            DESC      WITH IIF(m.DIRECT_INV,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DIRECTINV,oAriaApplication.GetHeaderText("LANG_DIRECTINV",lcInvHeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVSO,oAriaApplication.GetHeaderText("LANG_INVSO",lcInvHeaderAlias))) ,;
            *!*	            dAdd_Date WITH oAriaApplication.SystemDate   ,;
            *!*	            cAdd_Time WITH TIME()      ,;
            *!*	            cAdd_User WITH oAriaApplication.User_ID




            REPLACE DueDate   WITH m.DueDate   ,;
              REFERENCE WITH m.CustPo    ,;
              dPostDate WITH m.dPostDate ,;
              TRANTYPE  WITH '1'         ,;
              STORE     WITH m.Store     ,;
              Account   WITH m.Account   ,;
              cFacCode  WITH m.cFacCode  ,;
              TRAN      WITH m.Invoice   ,;
              TRANDATE  WITH m.InvDate   ,;
              AMOUNT    WITH m.TOTALCHG  ,;
              cCurrCode WITH m.cCurrCode ,;
              nCurrUnit WITH m.nCurrUnit ,;
              nExRate   WITH m.nExRate   ,;
              cArGlAcc  WITH m.cArAcnt   ,;
              DSC_AMT   WITH m.ShipAmt*m.trde_disc/100,;
              DESC      WITH IIF(m.Direct_Inv,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DIRECTINV,oAriaApplication.GetHeaderText("LANG_DIRECTINV",lcInvHeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVSO,oAriaApplication.GetHeaderText("LANG_INVSO",lcInvHeaderAlias))) ,;
              dAdd_Date WITH oAriaApplication.SystemDate   ,;
              cAdd_Time WITH TIME()      ,;
              cAdd_User WITH oAriaApplication.User_ID

            *!B610344,1 HIA 05/29/13 T20130509.0009 Trade Discount Miscalculations [End]
            *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]

            *E303386,1 TMI 05/08/2013 [Start] calculate trade discount based on ship amnt and chage
            IF UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='ENG'
              *!* B611016,1 MMT 06/15/2015 Invoice saving calculates Trade discount incorrectly for UK companies[T20150602.0002][Start]
              *REPLACE DSC_AMT   WITH (m.SHIPAMT+m.NCHARGES+m.Discount)*m.Trde_Disc/100
              REPLACE DSC_AMT   WITH (m.ShipAmt+m.TAX_AMT+m.NCHARGES+m.Discount)*m.trde_disc/100
              *!* B611016,1 MMT 06/15/2015 Invoice saving calculates Trade discount incorrectly for UK companies[T20150602.0002][End]
            ENDIF
            *E303386,1 TMI 05/08/2013 [End  ]

            *!* B609784,1 MMT 12/26/2011 Fix bug of updating Dsc_amt in debit file based on Shipped amount not total charges[END]
            UNLOCK
            *!* B608258,1 MMT 09/06/2007 don't Add record in debit file for Zero Amount invoices[Start]
          ENDIF
          *!* B608258,1 MMT 09/06/2007 don't Add record in debit file for Zero Amount invoices[End]
          *!* B608253,1 MMT 09/05/07 add log file to check the problem of not saving debit record[Start]
        ELSE
          lcStrVar = "llConsAcc= "+IIF(llConsAcc ,"TRUE","False")+" m.cFacCode = "+m.cFacCode +" laInvSetup[8,2] = "+laInvSetup[8,2]+;
            "  m.Invoice = "+ m.Invoice
          =STRTOFILE(lcStrVar ,oAriaApplication.Defaultpath+lcTmpTxtFl+".txt")
          *!* B608253,1 MMT 09/05/07 add log file to check the problem of not saving debit record[End]
        ENDIF
      ENDIF
    ENDIF
    IF llConsAcc AND (EMPTY(m.cFacCode) OR laInvSetup[8,2]='Y')
      *-- post factored invoice to customer = yes
      SELECT Customer
      =SEEK('M'+m.Account)
      =RLOCK()
      lnEqvAmnt = ROUND(m.TOTALCHG &lcExRSin m.nExRate &lcUntSin m.nCurrUnit,2)
      *-- Aging Type
      IF laInvSetup[9,2] = "D"
        *-- Aging by date
        REPLACE CURRENT WITH CURRENT + lnEqvAmnt,;
          TotAge  WITH CURRENT + Age30 + Age60 + Age90 + Age120
        *-- Aging by Terms
      ELSE
        REPLACE TerCurrent WITH TerCurrent + lnEqvAmnt,;
          TotAge     WITH TerCurrent + TerAge30 + TerAge60 + TerAge90 + TerAge120
      ENDIF
      REPLACE NetBal     WITH TotAge  - ChgBack + OpenCr,;
        YTDBal     WITH YTDBal  + lnEqvAmnt,;
        CrAvail    WITH IIF(CrLimit > 0,CrAvail - lnEqvAmnt, CrAvail) ,;
        nHgWtrMark WITH IIF(NetBal>nHgWtrMark,NetBal,nHgWtrMark)
      UNLOCK
      *!* B608253,1 MMT 09/05/07 add log file to check the problem of not saving debit record[Start]
    ELSE
      lcStrVar = "llConsAcc= "+IIF(llConsAcc ,"TRUE","False")+" m.cFacCode = "+m.cFacCode +" laInvSetup[8,2] = "+laInvSetup[8,2]+;
        "  m.Invoice = "+ m.Invoice
      =STRTOFILE(lcStrVar ,oAriaApplication.Defaultpath+lcTmpTxtFl+".txt")
      *!* B608253,1 MMT 09/05/07 add log file to check the problem of not saving debit record[End]
    ENDIF
    IF laInvSetup[1,2]='Y'
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
      *WAIT 'Updating GL Distribution File' WINDOW NOWAIT
      WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATEGLDIST,oAriaApplication.GetHeaderText("LANG_UPDATEGLDIST",lcInvHeaderAlias)) WINDOW NOWAIT
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
      SELECT GLDIST
            *E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]
     * IF !SEEK(m.Invoice+'IN'+lcGlSession)
       IF !gfSEEK(m.Invoice+'IN'+lcGlSession)
         *E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL [End]
         *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
         *WAIT 'Updating General Ledger Distribution File ' WINDOW NOWAIT
         WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATEGLDIST,oAriaApplication.GetHeaderText("LANG_UPDATEGLDIST",lcInvHeaderAlias)) WINDOW NOWAIT
         *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
         *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*!*	         SELECT (lcDistFile)
*!*	         SCAN
*!*	           REPLACE GLSESSION WITH lcGlSession
*!*	           =gfAdd_Info(lcDistFile)
*!*	         ENDSCAN
         lcGLDISTCursorUpdate = gfGetRemoteProp('lcCursorUpdate','GLDIST')
         *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[End] 
         *E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]
         *USE
    *B611924,1 ES 10/17/2019 Bug in Invoice Saving program	[T20190409.0005][Start]
       SELECT (lcDistFile)
    IF SEEK(INVHDR.INVOICE+'005')
      SUM nglamount TO lnDiscAmt REST WHILE Tran_no+catg_key =  INVHDR.INVOICE+'005'
      IF ABS(lnDiscAmt) <> ABS(INVHDR.DISCOUNT)
        lnDiscDiff = ABS(lnDiscAmt) - ABS(INVHDR.DISCOUNT)
        =SEEK(INVHDR.INVOICE+'005')
        REPLACE nglamount WITH nglamount - lnDiscDiff IN (lcDistFile)
        LNEQVAMNT  = &lcDistFile..nglamount 
        IF &lcDistFile..CCURRCODE <> OARIAAPPLICATION.BASECURRENCY
                  IF &lcDistFile..ncurrunit > 0 AND &lcDistFile..nexrate > 0
                    LCUNTSIN = ''
                    LNEXRATE = &lcDistFile..nexrate 
                    LNCURRUNIT = &lcDistFile..ncurrunit
                    LCEXRSIN = GFGETEXSIN(@LCUNTSIN, &lcDistFile..CCURRCODE)
                    LNEQVAMNT  = ROUND(LNEQVAMNT &LCEXRSIN LNEXRATE &LCUNTSIN LNCURRUNIT, 2)
                  ENDIF  
            ENDIF
            REPLACE nEqvAmnt WITH LNEQVAMNT  IN (lcDistFile) 
      ENDIF
    ENDIF
    *B611924,1 ES 10/17/2019 Bug in Invoice Saving program	[T20190409.0005][End]

   
      
         SELECT (lcDistFile)
         SCAN 
           *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
           REPLACE GLSESSION WITH lcGlSession
           =gfAdd_Info(lcDistFile)
		   *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]           
           SCATTER MEMVAR MEMO
           *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
           *m.Oid = gfTempName()
           *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
           SELECT GLDIST
           * APPEND FROM (oAriaApplication.WorkDir+lcDistFile)
           APPEND BLANK
           GATHER MEMVAR MEMO 
           *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
           *=gfreplace('')
           IF !EMPTY(lcGLDISTCursorUpdate)
             SELECT (lcGLDISTCursorUpdate)
             APPEND BLANK
             GATHER MEMVAR MEMO 
           ENDIF
           *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
         ENDSCAN
	     *E611821,1 MMT 11/13/2019 Call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
         =gfCreateGLEntries(lcDistFile,INVHDR.Account)
         *E611821,1 MMT 11/13/2019 Call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
         
         SELECT (lcDistFile)
         USE 
         ERASE (oAriaApplication.WorkDir+lcDistFile+".DBF")
         SELECT GLDIST
         =gfTableUpdate()
      ENDIF
    ENDIF
    *E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL [End]

    *-- Update the ups box File
    IF !EMPTY(lcUpsFile) AND USED(lcUpsFile)
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
      *WAIT 'Updating UPS Box File ' WINDOW NOWAIT
      WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATEUPSBOX,oAriaApplication.GetHeaderText("LANG_UPDATEUPSBOX",lcInvHeaderAlias)) WINDOW NOWAIT
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
      SELECT (lcUpsFile)
      lcStoreFld = IIF(m.CONSOL='Y',m.cConStore,m.Store)
      IF SEEK(&lcHdrFile..ORDER+lcStoreFld+m.PikTkt)
        SCAN REST WHILE ORDER+STORE+PikTkt+STR(CARTONS,5)= &lcHdrFile..ORDER+lcStoreFld+m.PikTkt
          IF !SEEK(lcInvNo+STORE+STR(CARTONS,5),'UPSBOX')
            SCATTER MEMVAR
            m.Invoice = lcInvNo
            INSERT INTO 'UPSBOX' FROM MEMVAR
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    *-- Save artons in artclpshp for BEL05.
    IF !EMPTY(m.PikTkt) AND ASCAN(oAriaApplication.laEvntTrig , PADR('SAVINV',10)) <> 0
      =gfDoTriger('ARIINV',PADR('SAVINV',10))
    ENDIF
    *-- Save artons in artclpshp for BEL05.

    *T20060804.0022,MMT,08/30/2006 Convert UPS integration to Aria4xp[Start]
    *!*	  IF !EMPTY(m.PikTkt) AND ASCAN(oAriaApplication.laEvntTrig , PADR('SAVIT',10)) <> 0
    *!*	    =gfDoTriger('ARIINV',PADR('SAVIT',10))
    *!*	  ENDIF
    IF !EMPTY(m.PikTkt)  AND ASCAN(loFormSet.laEvntTrig,PADR("SAVIT",10)) <> 0
      =loFormSet.mDoTrigger(PADR("SAVIT",10))
    ENDIF
    *T20060804.0022,MMT,08/30/2006 Convert UPS integration to Aria4xp[End]

    *-- Update the english charges
    IF !EMPTY(lcEngChrg) AND USED(lcEngChrg)
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
      *WAIT 'Updating Invoice Charges File ' WINDOW NOWAIT
      WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATECHARGESFILE,oAriaApplication.GetHeaderText("LANG_UPDATECHARGESFILE",lcInvHeaderAlias)) WINDOW NOWAIT
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
      =gfOpenFile(oAriaApplication.DataDir+'InvChrg',oAriaApplication.DataDir+'InvChrg','SH')
      SELECT (lcEngChrg)
      lcStoreFld = IIF(m.CONSOL='Y',m.cConStore,m.Store)
      IF SEEK(&lcHdrFile..ORDER+lcStoreFld+m.PikTkt)
        SCAN REST WHILE ORDER+cStore+PikTkt+cchrgcode = &lcHdrFile..ORDER+lcStoreFld+m.PikTkt
          IF !SEEK(lcInvNo+cStore+cchrgcode,'InvChrg')
            SCATTER MEMVAR
            m.Invoice = lcInvNo
            INSERT INTO 'InvChrg' FROM MEMVAR
            *!B610283,1 HIA 03/24/2013 Aria XP - Consolidated invoice [T20130109.0024][Start]
          ELSE
            REPLACE nChrgAmnt WITH nChrgAmnt +&lcEngChrg..nChrgAmnt IN InvChrg
            *!B610283,1 HIA 03/24/2013 Aria XP - Consolidated invoice [T20130109.0024][End]
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    *-- Update the customer history file
    IF llConsAcc AND SEEK(m.Account+lcGlYear,'arCusHst')
      lnShipAmnt = m.ShipAmt &lcExRSin m.nExRate &lcUntSin m.nCurrUnit
      lnDiscAmnt = ABS(m.Discount) &lcExRSin m.nExRate &lcUntSin m.nCurrUnit
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
    ENDIF
    *-- Key off invoice againset selected open credits
    IF llConsAcc .AND. &lcHdrFile..lKeyOff
      =lfvKeyOff(m.Invoice,lcAppCrdt)

      *E302350,1 MMT 01/11/2007 Enhancement to add Deposit Screen[Start]
      lnOldAlias = SELECT(0)
      IF !USED('DEPOSITS')
        =gfOpenTable(oAriaApplication.DataDir+'DEPOSITS',oAriaApplication.DataDir+'DEPOSITS','SH')
      ENDIF
      SELECT(lcAppCrdt)
      SCAN
        IF gfSEEK(&lcAppCrdt..TRAN+m.Order,'DEPOSITS')
          SELECT DEPOSITS
          GFREPLACE([lpaidflag with .T.])
        ENDIF
      ENDSCAN
      =gfTABLEUPDATE(.T.,'DEPOSITS')
      SELECT(lnOldAlias)
      *E302350,1 MMT 01/11/2007 Enhancement to add Deposit Screen[End]

    ENDIF
    *-- Mark generated invoice to be send
    IF !llFromEdi AND 'EB' $ oAriaApplication.CompanySetupModules

      IF SEEK('A'+m.Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'810','EDIPD')
        SELECT EDITRANS
        IF !SEEK('810'+PADR(m.Invoice,40)+'A'+m.Account)
          INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
            ('810',m.Invoice,'A',m.Account,EDIACPRT.lInterComp)
        ENDIF
        REPLACE cStatus WITH 'N'
        =gfAdd_Info('EDITRANS')
      ENDIF
      IF SEEK('F'+m.cFacCode,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'810','EDIPD')
        SELECT EDITRANS
        IF !SEEK('810'+PADR(m.Invoice,40)+'F'+m.cFacCode)
          INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
            ('810',m.Invoice,'F',m.cFacCode,EDIACPRT.lInterComp)
        ENDIF
        REPLACE cStatus WITH 'N'
        =gfAdd_Info('EDITRANS')
      ENDIF
      *!* E612469,1 MMT 09/13/2021 Add dropship record in EDITRANS[P20210827.0001][Start]
      IF SEEK('D'+m.Account,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'810','EDIPD')
        SELECT EDITRANS
        IF !SEEK('810'+PADR(m.Invoice,40)+'D'+m.Account)
          INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
            ('810',m.Invoice,'D',m.Account,EDIACPRT.lInterComp)
        ENDIF
        REPLACE cStatus WITH 'N'
        =gfAdd_Info('EDITRANS')
      ENDIF
      *!* E612469,1 MMT 09/13/2021 Add dropship record in EDITRANS[P20210827.0001][End]
    ENDIF
    *-- Send Point of sale invoice to back office
    IF !llFromEdi AND 'NC' $ oAriaApplication.CompanySetupModules AND lcSysType = 'P'
      SELECT CODES
      SET ORDER TO TAG Idrltfname
      =SEEK('NYCSITEID')
      LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
        FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
      IF FOUND()
        lcSiteId = CODES.cCode_No
        SELECT EDIACPRT
        LOCATE FOR cSiteId = lcSiteId
        IF FOUND('EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'810','EDIPD')
          SELECT EDITRANS
          IF !SEEK('810'+PADR(m.Invoice,40)+'A'+EDIACPRT.CPARTNER)
            INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
              ('810',m.Invoice,'A',EDIACPRT.CPARTNER,EDIACPRT.lInterComp)
          ENDIF
          REPLACE cStatus WITH 'N'
          =gfAdd_Info('EDITRANS')
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  WAIT CLEAR

  *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
  SELECT icStyHst
  =gfTABLEUPDATE()
  *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]

  *!* E302590,1 MMT 03/31/2009 Update TrnHist file when invoice created [Start]
  IF !USED('TRNHIST')
    =gfOpenTable('TRNHIST','TRANTYPE','SH')
  ENDIF
  SELECT (lcHdrFile)
  SCAN FOR !(&lcHdrFile..Direct_Inv)
    SELECT (lcDetFile)
    IF llConsDist
      SET ORDER TO ConsDist
      lcExpr    = "Account+Dist_Ctr+Order+Store+PikTkt"
      lcSeekKey = &lcHdrFile..Account+&lcHdrFile..Dist_Ctr+&lcHdrFile..ORDER+&lcHdrFile..STORE+&lcHdrFile..PikTkt
    ELSE
      SET ORDER TO (lcDetFile)
      lcExpr    = "Account+Order+Store+PikTkt"
      lcSeekKey = &lcHdrFile..Account+&lcHdrFile..ORDER+&lcHdrFile..STORE+&lcHdrFile..PikTkt
    ENDIF
    SET KEY TO
    =SEEK(lcSeekKey)
    SCAN REST WHILE &lcExpr = lcSeekKey
      =SEEK('O'+ORDER+STR(LINENO,6), 'ORDLINE')
      IF !EMPTY(ORDLINE.EMPLOYEE)
        IF !gfSEEK('I'+&lcHdrFile..Invoice+ORDLINE.EMPLOYEE+STYLE,'TRNHIST')
          SELECT TRNHIST
          APPEND BLANK
        ENDIF
        SELECT TRNHIST
        REPLACE TRANTYPE WITH 'I'                 ,;
          TRANNO   WITH &lcHdrFile..Invoice  ,;
          DTRANDT  WITH &lcHdrFile..InvDate  ,;
          Account  WITH &lcDetFile..Account ,;
          STORE    WITH &lcDetFile..STORE   ,;
          EMPLOYEE WITH ORDLINE.EMPLOYEE    ,;
          STYLE    WITH &lcDetFile..STYLE   ,;
          QTY1     WITH &lcDetFile..QTY1    ,;
          QTY2     WITH &lcDetFile..QTY2    ,;
          QTY3     WITH &lcDetFile..QTY3    ,;
          QTY4     WITH &lcDetFile..QTY4    ,;
          QTY5     WITH &lcDetFile..QTY5    ,;
          QTY6     WITH &lcDetFile..QTY6    ,;
          QTY7     WITH &lcDetFile..QTY7    ,;
          QTY8     WITH &lcDetFile..QTY8    ,;
          Totqty   WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8

        =gfAdd_Info('TRNHIST')
        =GFREPLACE()
      ENDIF
    ENDSCAN
  ENDSCAN
  SELECT TRNHIST
  =gfTABLEUPDATE()
  gfcloseTable("TRNHIST")
  *!* E302590,1 MMT 03/31/2009 Update TrnHist file when invoice created [End]


  SELECT (lnAlias)

  *B128066,1 AMH Fix bug of file is used by another user [Start]
  SET REPROCESS TO lnRePro
  *B128066,1 AMH [End]

  RETURN

  *!*************************************************************
  *! Name      : gpVoidInv
  *! Developer : Ahmed Shoukry Mohammed (ASM)
  *! Date      : 12/16/2002
  *! Purpose   : Void an Invoice
  *!*************************************************************
  *! Parameters:  laSetups        : Array holding the Setups
  *!              GlSession       : GL Session
  *!              llIsEngland     : Whether England Customer or Not
  *!              lcTmpAr         : File Name
  *!              loFormSet       : Formset calling the function [Optional]
  *!              llMultiVoid     : Called from the multi void formset [Optional]
  *!              ldVoidDate      : Date to Void On ( in case of Multi Void Only)
  *!              llYesToAll      : Hold Yes to Rebulid All Orders ( in case of Direct Invoice Multi Void Only)
  *!              llNoToAll       : Hold No to Rebuild All Orders  ( in case of Direct Invoice Multi Void Only)
  *!*************************************************************
  *! Returns   :  True/False
  *!*************************************************************
FUNCTION gpVoidInv
  *HBG 3/20/2005 Fix bug of not updating the Session # in GLDiST in case of void [Begin]
  *!*	LPARAMETERS laSetups, GlSession, llIsEngland, lcTmpAr, loFormSet, ;
  *!*	            llMultiVoid, ldVoidDate, llYesToAll, llNoToAll, lcCanReason
  LPARAMETERS laSetups, lcGlSession, llIsEngland, lcTmpAr, loFormSet, ;
    llMultiVoid, ldVoidDate, llYesToAll, llNoToAll, lcCanReason
  *HBG [End]

  #Include R:\aria4xp\screens\ar\ardinv.h
  *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
  IF oAriaApplication.oActivelang.cLang_ID <> "EN"
    LOCAL lcInvHeaderAlias
    lcInvHeaderAlias=oAriaApplication.GetClassHeaderFile(ADDBS(UPPER(ALLTRIM(oAriaApplication.LangPath))) + "SCREENS\AR\" + ALLTRIM("ARDINV")+"_"+"H" +".XML")
  ENDIF
  *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
  *B128066,1 AMH Fix bug of file is used by another user [Start]
  LOCAL lnRePro
  lnRePro = SET("Reprocess")
  SET REPROCESS TO -1
  *B128066,1 AMH [End]

  *!* B609302,1 MMT 06/15/2010 Fix bug of Error while voiding Invoice from Voiding multiple invoice program[Start]
  lcStyCstMeth = ALLTRIM(gfGetMemVar('M_COST_MET',oAriaApplication.ActiveCompanyID))
  *!* B609302,1 MMT 06/15/2010 Fix bug of Error while voiding Invoice from Voiding multiple invoice program[End]

  *!* B608948,1 MMT 07/28/2009 Update Trnhist Table when Invoice is Voided[start]
  IF !USED('TRNHIST')
    =gfOpenTable('TRNHIST','TRANTYPE','SH')
  ENDIF
  *!* B608948,1 MMT 07/28/2009 Update Trnhist Table when Invoice is Voided[End]

  *ASM, This is the Before Delete checking [Start]
  =gfOpenFile(oAriaApplication.DataDir+'OrdLine', oAriaApplication.DataDir+'OrdLine','SH')

  *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
  *=gfOpenFile(oAriaApplication.DataDir+'icStyHst', oAriaApplication.DataDir+'Styhst','SH')
  =gfOpenTable('ICSTYHST','STYHST','SH')
  *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]

  =gfOpenFile(oAriaApplication.DataDir+'arCusHst', oAriaApplication.DataDir+'Acthst','SH')
  =gfOpenFile(oAriaApplication.DataDir+'DEBIT', oAriaApplication.DataDir+'DEBIT','SH')
  =gfOpenFile(oAriaApplication.DataDir+'REPCOMM', oAriaApplication.DataDir+'REPCOMM','SH')
  IF InvHdr.CONSOL = 'Y'
    =gfOpenFile(oAriaApplication.DataDir+'CONSINVH', oAriaApplication.DataDir+'CONSINVH','SH')
    =gfOpenFile(oAriaApplication.DataDir+'CONSINVL', oAriaApplication.DataDir+'CONSINVL','SH')
  ENDIF
  =gfOpenFile(oAriaApplication.DataDir+'ARHIST', oAriaApplication.DataDir+'ARHISTT','SH')
  IF EMPTY(InvHdr.cFacCode) OR laSetups[17,2]='Y'
    =gfOpenFile(oAriaApplication.DataDir+'CREDIT', oAriaApplication.DataDir+'CREDIT','SH')
    SELECT ARHIST
    =SEEK(InvHdr.Account+InvHdr.Invoice)
    *--  Check if the invoice was paid or not
    LOCATE REST WHILE Account+TRAN+cinstalno = InvHdr.Account+InvHdr.Invoice FOR TRANTYPE='1'
    llPaid = FOUND()
    SELECT InvHdr
    IF llPaid
      *-- Message : 40130
      *-- This invoice has already been paid. Cannot void.
      *-- Button : 00000
      *-- Ok
      =gfModalGen('TRM40130B00000','ALERT',IIF(!llMultiVoid,'','# '+InvHdr.Invoice))
      llRetValue = .F.
      *B128066,1 AMH Fix bug of file is used by another user [Start]
      SET REPROCESS TO lnRePro
      *B128066,1 AMH [End]

      RETURN .F.
    ENDIF
  ENDIF
  IF 'RM' $ oAriaApplication.CompanySetupModules
    *-- if the return mechendise module installed
    *B610526,1 TMI 09/24/2013 [Start] use gfopentable instead
    *=gfOpenFile(oAriaApplication.DataDir+'RETLINE',oAriaApplication.DataDir+'RETLINEI','SH')
    *=gfOpenFile(oAriaApplication.DataDir+'RETHDR',oAriaApplication.DataDir+'RETHDR','SH')
    =gfOpenTable('RETLINE','RETLINEI','SH')
    =gfOpenTable('RETHDR','RETHDR','SH')
    *B610526,1 TMI 09/24/2013 [End  ]
    *B610526,1 TMI 09/24/2013 [End  ]
    *-- seek for the invoice  in the return merchandise files
    *B610526,1 TMI 09/24/2013 [Start] use gfseek istead
    *llRetInv  = SEEK(InvHdr.Account+InvHdr.Invoice,'RETLINE') AND ;
    SEEK(RETLINE.CrMemo,'RETHDR') AND RETHDR.STATUS <> 'V'
    llRetInv  = gfSEEK(InvHdr.Account+InvHdr.Invoice,'RETLINE') AND ;
      gfSEEK(RETLINE.CrMemo,'RETHDR') AND RETHDR.STATUS <> 'V'
    *B610526,1 TMI 09/24/2013 [End  ]

    SELECT InvHdr
    IF llRetInv
      IF !llMultiVoid
        *-- Message : 40134
        *-- Return merchandise invoice. Cannot void
        *-- Button : 00000
        *-- Ok
        =gfModalGen('TRM40134B00000','ALERT','')
      ELSE
        *-- Message : 40195
        *-- Return merchandise invoice. Cannot void
        *-- Button : 00000
        *-- Ok
        =gfModalGen('TRM40195B00000','ALERT','# '+InvHdr.Invoice)
      ENDIF
      llRetValue = .F.

      *B128066,1 AMH Fix bug of file is used by another user [Start]
      SET REPROCESS TO lnRePro
      *B128066,1 AMH [End]

      RETURN .F.
    ENDIF
  ENDIF
  IF !SEEK(InvHdr.Invoice,'InvLine')
    *-- Message : 40002
    *-- invoice lines are not found
    *-- Button : 00000
    *-- Ok
    =gfModalGen('INM40002B00000','ALERT',IIF(!llMultiVoid,'','# '+InvHdr.Invoice))
    llRetValue = .F.

    *B128066,1 AMH Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 AMH [End]

    RETURN .F.
  ENDIF
  IF InvHdr.CONSOL = 'Y' .AND. (!SEEK(InvHdr.Invoice,'CONSINVH') OR ;
      !SEEK(InvHdr.Invoice,'CONSINVL') )
    *-- Message : 40002
    *-- invoice lines are not found
    *-- Button : 00000
    *-- Ok
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen('INM40002B00000','ALERT',LANG_InvoiceFile+IIF(!llMultiVoid,'',' # '+InvHdr.Invoice))
    =gfModalGen('INM40002B00000','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_InvoiceFile,oAriaApplication.GetHeaderText("LANG_InvoiceFile",lcInvHeaderAlias))+IIF(!llMultiVoid,'',' # '+InvHdr.Invoice))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    llRetValue = .F.

    *B128066,1 AMH Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 AMH [End]

    RETURN .F.
  ENDIF

  IF !llMultiVoid && in case of multi void, the void date will be entered before calling this procedure
    ldVDate = {}
    *-- get the void date from the user
    *B612262,1 MMT 11/23/2020 While voiding in Invoice screen, User can enter Void date less than the Invoice date[T20201110.0001][Start]
    *DO FORM (oAriaApplication.ScreenHome+"AR\ARVDATE") TO ldVDate
    DO FORM (oAriaApplication.ScreenHome+"AR\ARVDATE") TO ldVDate WITH InvHdr.dPostDate
    *B612262,1 MMT 11/23/2020 While voiding in Invoice screen, User can enter Void date less than the Invoice date[T20201110.0001][End]
    IF EMPTY(ldVDate)
      llRetValue = .F.

      *B128066,1 AMH Fix bug of file is used by another user [Start]
      SET REPROCESS TO lnRePro
      *B128066,1 AMH [End]

      RETURN .F.
    ENDIF
    *-- check if the void data is valid or not
    STORE '' TO lcSysYear,lcSysPeriod
    IF !CHECKPRD(ldVDate,'lcSysYear','lcSysPeriod','VI1')
      llRetValue = .F.
      *B128066,1 AMH Fix bug of file is used by another user [Start]
      SET REPROCESS TO lnRePro
      *B128066,1 AMH [End]

      RETURN .F.
    ENDIF
  ELSE
    ldVDate = ldVoidDate
  ENDIF
  STORE '' TO lcInvYear,lcInvPeriod
  IF !CHECKPRD(InvHdr.dPostDate,'lcInvYear','lcInvPeriod','VI2',.T.)
    llRetValue = .F.

    *B128066,1 AMH Fix bug of file is used by another user [Start]
    SET REPROCESS TO lnRePro
    *B128066,1 AMH [End]

    RETURN .F.
  ENDIF
  *ASM, This is the Before Delete checking [End]

  llRebuild = .T.
  DO CASE
    CASE InvHdr.Direct_Inv AND llMultiVoid AND llYesToAll
      llRebuild = .T.
    CASE InvHdr.Direct_Inv AND llMultiVoid AND llNoToAll
      llRebuild = .F.
    CASE InvHdr.Direct_Inv AND llMultiVoid
      *-- Message : 40196
      *-- <Invoice# lcInvoice is a direct invoice, Would you like to rebuild the order>
      *-- Button : 00001
      *-- <             <Yes>      <Yes to All>      <No>     <No to all>             >
      lnAnswer = 0
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
      *lnAnswer=gfModalGen('TRM40196B40016','ALERT','','','Invoice# ' + InvHdr.Invoice + ' is a direct invoice. Would you like to rebuild the order')
      lnAnswer=gfModalGen('TRM40196B40016','ALERT','','',;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVNO,oAriaApplication.GetHeaderText("LANG_INVNO",lcInvHeaderAlias))+' ' +;
        InvHdr.Invoice + ' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DIRECTINVREBUILDORDER,oAriaApplication.GetHeaderText("LANG_DIRECTINVREBUILDORDER",lcInvHeaderAlias)))
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
      DO CASE
        CASE lnAnswer = 1
          llRebuild = .T.
        CASE lnAnswer = 2
          llYesToAll = .T.
          llRebuild = .T.
        CASE lnAnswer = 3
          llRebuild = .F.
        CASE lnAnswer = 4
          llNoToAll = .T.
          llRebuild = .F.
      ENDCASE

    CASE InvHdr.Direct_Inv AND !llMultiVoid
      *-- Message : 40003
      *-- This is a direct invoice. Would you like to rebuild the order
      *-- Button : 00001
      *-- Yes No
      IF gfModalGen('INM40003B40000','ALERT') = 2
        llRebuild = .F.
      ENDIF
  ENDCASE
  *-- Create temporary History file which used key off process
  =lfCreatHst(@lcTmpAr)

  *-- Prepare Array used by Style COntrol function
  DECLARE laGlArray[2,13]
  STORE '' TO laGlArray
  *-- Linked to GL
  IF laSetups[4,2]='Y'
    *!* E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL [start]
    * =gfOpenFile(oAriaApplication.DataDir+'GLDIST', oAriaApplication.DataDir+'GLDISTNO', 'SH')
    =gfOpenTable(oAriaApplication.DataDir+'GLDIST', oAriaApplication.DataDir+'GLDISTNO', 'SH')
    *!* E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL [end]

    SELECT GLDIST
    lcGlDist = gfTempName()
    COPY STRUCTURE TO (oAriaApplication.WorkDir+lcGlDist)
    =gfOpenFile(oAriaApplication.WorkDir+lcGlDist, '', 'EX')
    
*B611924,1 ES 10/17/2019 Bug in Invoice Saving program	[T20190409.0005][Start]
    SELECT (lcGlDist)
    INDEX on Tran_no+catg_key TAG (lcGlDist)
*B611924,1 ES 10/17/2019 Bug in Invoice Saving program	[T20190409.0005][End]

  
    STORE 'VI'           TO laGlArray[1,4],laGlArray[2,4]
    STORE InvHdr.Invoice TO laGlArray[1,5],laGlArray[2,5]   && Invoice no
    STORE ldVDate        TO laGlArray[1,6],laGlArray[2,6]   && Void Date
    STORE lcSysYear      TO laGlArray[1,7],laGlArray[2,7]   && Year
    STORE lcSysPeriod    TO laGlArray[1,8],laGlArray[2,8]   && Period
    STORE lcGlDist       TO laGlArray[1,9],laGlArray[2,9]
    STORE ''             TO laGlArray[1,11],laGlArray[2,11]
    STORE ''             TO laGlArray[1,12],laGlArray[2,12]
    STORE ''             TO laGlArray[1,13],laGlArray[2,13]
  ENDIF
  SET ORDER TO TAG DRTRAN IN DEBIT
  *-- Get invoice currency unit sign and exchange rate sign
  lcUntSin = ''
  lcExRSin = gfGetExSin(@lcUntSin, InvHdr.cCurrCode)

  lcInvHFile = IIF(InvHdr.CONSOL='Y','CONSINVH','INVHDR')
  lcInvLines = IIF(InvHdr.CONSOL='Y','CONSINVL','INVLINE')
  lcInvoice  = InvHdr.Invoice
  lcTmpInvL = gfTempName()
  DIMENSION laFileStru[4,18]
  laFileStru[1,1] = 'Account'
  laFileStru[1,2] = 'C'
  laFileStru[1,3] = 5
  laFileStru[1,4] = 0
  laFileStru[2,1] = 'Order'
  laFileStru[2,2] = 'C'
  laFileStru[2,3] = 6
  laFileStru[2,4] = 0
  laFileStru[3,1] = 'Store'
  laFileStru[3,2] = 'C'
  laFileStru[3,3] = 8
  laFileStru[3,4] = 0
  laFileStru[4,1] = 'lineNO'
  laFileStru[4,2] = 'N'
  laFileStru[4,3] = 6
  laFileStru[4,4] = 0
  FOR lnCount = 1 TO 4
    STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
      laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
      laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
      laFileStru[lnCount,16]
    STORE 0  TO laFileStru[lnCount,17],  laFileStru[lnCount,18]
  ENDFOR
  DECLARE laIndex[1,2]
  laIndex[1,1] = 'Account+Order+Store+STR(LineNo,6)'
  laIndex[1,2] = lcTmpInvL
  =gfCrtTmp(lcTmpInvL ,@laFileStru,@laIndex)

  SELECT (lcInvHFile)
  SCAN REST WHILE Invoice = lcInvoice
    *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[Start]
    *IF SEEK('O'+Order,'OrdHdr')
    IF SEEK('O'+ORDER,'OrdHdr','OrdHdr')
      *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[End]
      *-- Rebuild order lines
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *WAIT LANG_RebuildOrderMessage +ORDER+' ...' WINDOW NOWAIT
      WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RebuildOrderMessage,oAriaApplication.GetHeaderText("LANG_RebuildOrderMessage",lcInvHeaderAlias)) +ORDER+' ...' WINDOW NOWAIT
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      lnDifValue = 0
      SELECT (lcInvLines)
      =SEEK(InvHdr.Invoice+IIF(InvHdr.CONSOL='Y',&lcInvHFile..STORE,''))
      SCAN REST WHILE Invoice+IIF(InvHdr.CONSOL='Y',STORE,'') = ;
          &lcInvHFile..Invoice+IIF(InvHdr.CONSOL='Y',&lcInvHFile..STORE,'') ;
          FOR   ORDER = &lcInvHFile..ORDER
        IF !SEEK(Account+ORDER+STORE+STR(LINENO,6),lcTmpInvL)
          =SEEK('O'+ORDER+STR(LINENO,6),'ORDLINE')
          =RLOCK('ORDLINE')
          *-- update the order line quantity
          REPLACE QTY1   WITH QTY1 + &lcInvLines..QTY1 ,;
            QTY2   WITH QTY2 + &lcInvLines..QTY2 ,;
            QTY3   WITH QTY3 + &lcInvLines..QTY3 ,;
            QTY4   WITH QTY4 + &lcInvLines..QTY4 ,;
            QTY5   WITH QTY5 + &lcInvLines..QTY5 ,;
            QTY6   WITH QTY6 + &lcInvLines..QTY6 ,;
            QTY7   WITH QTY7 + &lcInvLines..QTY7 ,;
            QTY8   WITH QTY8 + &lcInvLines..QTY8 ,;
            Totqty WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 IN 'ORDLINE'
          *B130472,1 02/13/2006 Fix bug of not updating nPAckNo in ORDLINE with the actually shipped qty[Begin]
          =gfDoTriger('ARDINV',PADR('UPORDSH',10))
          *B130472,1 [End]
          UNLOCK IN 'ORDLINE'
          *-- get the diferent between the price in ordline and the price in invline
          lnDifValue = lnDifValue + ( (&lcInvLines..Totqty * &lcInvLines..Price) - ;
            (&lcInvLines..Totqty * ORDLINE.Price) )
          INSERT INTO (lcTmpInvL) (Account,ORDER,STORE,LINENO) VALUES ;
            (&lcInvLines..Account,&lcInvLines..ORDER,&lcInvLines..STORE,&lcInvLines..LINENO)
        ENDIF
      ENDSCAN
      IF llRebuild
        *-- update the order header
        *-- open amount =open amount + shipped amount- difference between invine price
        *-- and order line price
        SELECT ORDHDR
        =RLOCK()
        REPLACE STATUS  WITH 'O'  ,;
          Ship    WITH Ship    - &lcInvHFile..Ship,;
          ShipAmt WITH ShipAmt - &lcInvHFile..ShipAmt ,;
          OPEN    WITH OPEN    + &lcInvHFile..Ship,;
          OpenAmt WITH OpenAmt + &lcInvHFile..ShipAmt - lnDifValue,;
          ApprAmt WITH IIF((InvHdr.CONSOL='Y' OR (&lcInvHFile..STORE <> STORE)),ApprAmt+ROUND(&lcInvHFile..ShipAmt,0),;
          ROUND(&lcInvHFile..ShipAmt,0))
          
          *!* B612564,1 MMT 05/18/2022 When the same user edits SO has a Picking ticket, then Invoiced this SO, and then saved the SO, the SO status became open again while it should be completed[T20220511.0004][Stat]
          =gfAdd_Info('ORDHDR')
          *!* B612564,1 MMT 05/18/2022 When the same user edits SO has a Picking ticket, then Invoiced this SO, and then saved the SO, the SO status became open again while it should be completed[T20220511.0004][End]
         
          
        UNLOCK
      ELSE
        *-- Cancel order remaining quantity
        IF EMPTY(lcCanReason)
          lcCanReason = ''
          DO FORM (oAriaApplication.ScreenHome+"ARORDER") WITH 'X' TO lcCanReason
        ENDIF
        SELECT ORDHDR
        =RLOCK()
        REPLACE CANCEL     WITH Ship       ,;
          CancelAmt  WITH ShipAmt    ,;
          ShipAmt    WITH 0          ,;
          Ship       WITH 0          ,;
          CANCELLED  WITH ldVDate    ,;
          CCANCRESON WITH lcCanReason ,;
          STATUS     WITH 'X'
          
          *!* B612564,1 MMT 05/18/2022 When the same user edits SO has a Picking ticket, then Invoiced this SO, and then saved the SO, the SO status became open again while it should be completed[T20220511.0004][Stat]
          =gfAdd_Info('ORDHDR')
          *!* B612564,1 MMT 05/18/2022 When the same user edits SO has a Picking ticket, then Invoiced this SO, and then saved the SO, the SO status became open again while it should be completed[T20220511.0004][End]
          
        UNLOCK
      ENDIF
    ENDIF

    *-- Update first sales rep. commission
    IF (!EMPTY(&lcInvHFile..Rep1) .AND. &lcInvHFile..CommAmt1 > 0) .AND. ;
        SEEK(&lcInvHFile..Rep1,'SALESREP')
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *WAIT LANG_ChargeBackSalesRep WINDOW NOWAIT
      WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChargeBackSalesRep,oAriaApplication.GetHeaderText("LANG_ChargeBackSalesRep",lcInvHeaderAlias)) WINDOW NOWAIT
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      *-- Calculate the equivlant commission amount in Base currency
      lnEqvAmnt = -1* &lcInvHFile..CommAmt1 &lcExRSin InvHdr.nExRate &lcUntSin InvHdr.nCurrUnit
      SELECT REPCOMM
      APPEND BLANK
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
      *!*	    REPLACE STATUS     WITH 'O',;
      *!*	      REPCODE    WITH &lcInvHFile..REP1   ,;
      *!*	      ORDER      WITH &lcInvHFile..ORDER  ,;
      *!*	      TRAN       WITH &lcInvHFile..Invoice,;
      *!*	      DATE       WITH ldVdate,;
      *!*	      TRANTYPE   WITH '6',;
      *!*	      DESC       WITH 'CHARGE BACK - VOID',;
      *!*	      CUSTPO     WITH &lcInvHFile..CustPo  ,;
      *!*	      ACCOUNT    WITH &lcInvHFile..Account ,;
      *!*	      STORE      WITH &lcInvHFile..STORE   ,;
      *!*	      AMOUNT     WITH lnEqvAmnt      ,;
      *!*	      BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
      *!*	      nForAmnt   WITH -1 * &lcInvHFile..COMMAMT1  ,;
      *!*	      cCurrCode  WITH InvHdr.cCurrCode,;
      *!*	      nCurrUnit  WITH InvHdr.nCurrUnit,;
      *!*	      nExRate    WITH InvHdr.nExRate  ,;
      *!*	      dAdd_Date  WITH oAriaApplication.SystemDate ,;
      *!*	      cAdd_Time  WITH TIME(),;
      *!*	      cAdd_User  WITH oAriaApplication.User_ID
      REPLACE STATUS     WITH 'O',;
        REPCODE    WITH &lcInvHFile..Rep1   ,;
        ORDER      WITH &lcInvHFile..ORDER  ,;
        TRAN       WITH &lcInvHFile..Invoice,;
        DATE       WITH ldVDate,;
        TRANTYPE   WITH '6',;
        DESC       WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CHARGEBACK,oAriaApplication.GetHeaderText("LANG_CHARGEBACK",lcInvHeaderAlias)),;
        CustPo     WITH &lcInvHFile..CustPo  ,;
        Account    WITH &lcInvHFile..Account ,;
        STORE      WITH &lcInvHFile..STORE   ,;
        AMOUNT     WITH lnEqvAmnt      ,;
        BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
        nForAmnt   WITH -1 * &lcInvHFile..CommAmt1  ,;
        cCurrCode  WITH InvHdr.cCurrCode,;
        nCurrUnit  WITH InvHdr.nCurrUnit,;
        nExRate    WITH InvHdr.nExRate  ,;
        dAdd_Date  WITH oAriaApplication.SystemDate ,;
        cAdd_Time  WITH TIME(),;
        cAdd_User  WITH oAriaApplication.User_ID
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
      *B608314,1 MMT 10/11/2007 fix bug of wrong Ship Amt In Commession report[Start]
      REPLACE PikTkt WITH &lcInvHFile..PikTkt
      *B608314,1 MMT 10/11/2007 fix bug of wrong Ship Amt In Commession report[end]


      =RLOCK('SALESREP')
      REPLACE CURRENT WITH CURRENT + lnEqvAmnt,;
        BALANCE WITH CURRENT + Age30+Age60+Age90+Age120 IN 'SALESREP'
      UNLOCK IN 'SALESREP'
    ENDIF
    *-- Update second sales rep. commission
    IF (!EMPTY(&lcInvHFile..Rep2) .AND. &lcInvHFile..CommAmt2 > 0) .AND. ;
        SEEK(&lcInvHFile..Rep2,'SALESREP')
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *WAIT LANG_ChargeBackSalesRep WINDOW NOWAIT
      WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ChargeBackSalesRep,oAriaApplication.GetHeaderText("LANG_ChargeBackSalesRep",lcInvHeaderAlias)) WINDOW NOWAIT
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      lnEqvAmnt = -1 * &lcInvHFile..CommAmt2 &lcExRSin InvHdr.nExRate &lcUntSin InvHdr.nCurrUnit
      SELECT REPCOMM
      APPEND BLANK
      =RLOCK()
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
      *!*	    REPLACE STATUS     WITH 'O'           ,;
      *!*	      REPCODE    WITH &lcInvHFile..REP2   ,;
      *!*	      ORDER      WITH &lcInvHFile..ORDER  ,;
      *!*	      TRAN       WITH &lcInvHFile..Invoice,;
      *!*	      DATE       WITH ldVDate     ,;
      *!*	      TRANTYPE   WITH '6'           ,;
      *!*	      DESC       WITH 'CHARGE BACK - VOID',;
      *!*	      CUSTPO     WITH &lcInvHFile..CustPo ,;
      *!*	      ACCOUNT    WITH &lcInvHFile..Account,;
      *!*	      STORE      WITH &lcInvHFile..STORE  ,;
      *!*	      AMOUNT     WITH lnEqvAmnt     ,;
      *!*	      BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
      *!*	      nForAmnt   WITH -1 *  &lcInvHFile..COMMAMT2 ,;
      *!*	      cCurrCode  WITH InvHdr.cCurrCode,;
      *!*	      nCurrUnit  WITH InvHdr.nCurrUnit,;
      *!*	      nExRate    WITH InvHdr.nExRate  ,;
      *!*	      dAdd_Date  WITH oAriaApplication.SystemDate  ,;
      *!*	      cAdd_Time  WITH TIME()  ,;
      *!*	      cAdd_User  WITH oAriaApplication.User_ID
      REPLACE STATUS     WITH 'O'           ,;
        REPCODE    WITH &lcInvHFile..Rep2   ,;
        ORDER      WITH &lcInvHFile..ORDER  ,;
        TRAN       WITH &lcInvHFile..Invoice,;
        DATE       WITH ldVDate     ,;
        TRANTYPE   WITH '6'           ,;
        DESC       WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CHARGEBACK,oAriaApplication.GetHeaderText("LANG_CHARGEBACK",lcInvHeaderAlias)),;
        CustPo     WITH &lcInvHFile..CustPo ,;
        Account    WITH &lcInvHFile..Account,;
        STORE      WITH &lcInvHFile..STORE  ,;
        AMOUNT     WITH lnEqvAmnt     ,;
        BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
        nForAmnt   WITH -1 *  &lcInvHFile..CommAmt2 ,;
        cCurrCode  WITH InvHdr.cCurrCode,;
        nCurrUnit  WITH InvHdr.nCurrUnit,;
        nExRate    WITH InvHdr.nExRate  ,;
        dAdd_Date  WITH oAriaApplication.SystemDate  ,;
        cAdd_Time  WITH TIME()  ,;
        cAdd_User  WITH oAriaApplication.User_ID
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
      *B608314,1 MMT 10/11/2007 fix bug of wrong Ship Amt In Commession report[Start]
      REPLACE PikTkt WITH &lcInvHFile..PikTkt
      *B608314,1 MMT 10/11/2007 fix bug of wrong Ship Amt In Commession report[End]


      UNLOCK
      SELECT SALESREP
      =RLOCK()
      REPLACE CURRENT  WITH CURRENT + lnEqvAmnt,;
        BALANCE  WITH CURRENT + Age30+Age60+Age90+Age120
      UNLOCK
    ENDIF
    *-- Custom process for A.S.T. sportwear.
    IF TYPE('loFormSet')='O'
      =loFormSet.mDoTrigger(PADR('VOIDREP3',10))
    ENDIF
  ENDSCAN
  =SEEK(lcInvoice,'InvHdr')
  *-- Update cut & sold
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT LANG_UpdateCutandSold WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UpdateCutandSold,oAriaApplication.GetHeaderText("LANG_UpdateCutandSold",lcInvHeaderAlias)) WINDOW NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  lnCOGSAmt = 0
  SELECT INVLINE
  =SEEK(lcInvoice)
  SCAN REST WHILE Invoice = lcInvoice
    IF !SEEK(STYLE,'Style')
      LOOP
    ENDIF
    =SEEK('O'+INVLINE.ORDER+STR(LINENO,6),'ORDLINE')
    *!* B608948,1 MMT 07/28/2009 Update Trnhist Table when Invoice is Voided[start]
    *IF !EMPTY(ORDLINE.EMPLOYEE)
    *  IF gfSEEK('I'+lcInvoice+ORDLINE.EMPLOYEE+STYLE,'TRNHIST')
    *    SELECT TRNHIST
    *    gfDelete()
    *  ENDIF
    *  SELECT INVLINE
    *ENDIF
    *!* B608948,1 MMT 07/28/2009 Update Trnhist Table when Invoice is Voided[End]

    *-- Update style dyelot Shipped Quantity
    IF laSetups[8,2]='Y' .AND. STYLE.cDye_Flg = 'Y' .AND. !EMPTY(INVLINE.Dyelot)
      IF llRebuild AND ;
          SEEK(IIF(EMPTY(INVLINE.AltStyle),INVLINE.STYLE,INVLINE.AltStyle)+IIF(EMPTY(INVLINE.cWareCode),ORDLINE.cWareCode,INVLINE.cWareCode)+INVLINE.Dyelot,'StyDye')
        *-- Update Dyelot Ordered Quantity
        SELECT StyDye
        =RLOCK()
        REPLACE Ord1   WITH Ord1 + INVLINE.QTY1,;
          Ord2   WITH Ord2 + INVLINE.QTY2,;
          Ord3   WITH Ord3 + INVLINE.QTY3,;
          Ord4   WITH Ord4 + INVLINE.QTY4,;
          Ord5   WITH Ord5 + INVLINE.QTY5,;
          Ord6   WITH Ord6 + INVLINE.QTY6,;
          Ord7   WITH Ord7 + INVLINE.QTY7,;
          Ord8   WITH Ord8 + INVLINE.QTY8,;
          TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
        UNLOCK
      ENDIF
      *-- Update style dyelot Shipped Quantity
      IF SEEK(INVLINE.STYLE+IIF(EMPTY(INVLINE.cWareCode),ORDLINE.cWareCode,INVLINE.cWareCode)+INVLINE.Dyelot,'StyDye')
        SELECT StyDye
        =RLOCK()
        REPLACE SHP1   WITH SHP1 - INVLINE.QTY1 ,;
          SHP2   WITH SHP2 - INVLINE.QTY2 ,;
          SHP3   WITH SHP3 - INVLINE.QTY3 ,;
          SHP4   WITH SHP4 - INVLINE.QTY4 ,;
          SHP5   WITH SHP5 - INVLINE.QTY5 ,;
          SHP6   WITH SHP6 - INVLINE.QTY6 ,;
          SHP7   WITH SHP7 - INVLINE.QTY7 ,;
          SHP8   WITH SHP8 - INVLINE.QTY8 ,;
          TOTSHP WITH SHP1+SHP2+SHP3+SHP4+SHP5+SHP6+SHP7+SHP8
        UNLOCK
      ENDIF
    ENDIF
    *-- Update Warehouse Ordered Quantity
    IF llRebuild AND ;
        SEEK(IIF(EMPTY(INVLINE.AltStyle),INVLINE.STYLE,INVLINE.AltStyle)+IIF(EMPTY(INVLINE.cWareCode),ORDLINE.cWareCode,INVLINE.cWareCode)+SPACE(10),'StyDye')
      SELECT StyDye
      =RLOCK()
      REPLACE Ord1   WITH Ord1 + INVLINE.QTY1,;
        Ord2   WITH Ord2 + INVLINE.QTY2,;
        Ord3   WITH Ord3 + INVLINE.QTY3,;
        Ord4   WITH Ord4 + INVLINE.QTY4,;
        Ord5   WITH Ord5 + INVLINE.QTY5,;
        Ord6   WITH Ord6 + INVLINE.QTY6,;
        Ord7   WITH Ord7 + INVLINE.QTY7,;
        Ord8   WITH Ord8 + INVLINE.QTY8,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
      UNLOCK
    ENDIF
    *-- Update warehouse Shipped Quantity
    IF SEEK(INVLINE.STYLE+IIF(EMPTY(INVLINE.cWareCode),ORDLINE.cWareCode,INVLINE.cWareCode)+SPACE(10),'StyDye')
      SELECT StyDye
      =RLOCK()
      REPLACE SHP1   WITH SHP1 - INVLINE.QTY1 ,;
        SHP2   WITH SHP2 - INVLINE.QTY2 ,;
        SHP3   WITH SHP3 - INVLINE.QTY3 ,;
        SHP4   WITH SHP4 - INVLINE.QTY4 ,;
        SHP5   WITH SHP5 - INVLINE.QTY5 ,;
        SHP6   WITH SHP6 - INVLINE.QTY6 ,;
        SHP7   WITH SHP7 - INVLINE.QTY7 ,;
        SHP8   WITH SHP8 - INVLINE.QTY8 ,;
        TOTSHP WITH SHP1+SHP2+SHP3+SHP4+SHP5+SHP6+SHP7+SHP8
      UNLOCK
    ENDIF
    IF llRebuild AND SEEK(IIF(EMPTY(INVLINE.AltStyle),INVLINE.STYLE,INVLINE.AltStyle),'STYLE')
      SELECT STYLE
      =RLOCK()
      REPLACE Ord1   WITH Ord1 + INVLINE.QTY1 ,;
        Ord2   WITH Ord2 + INVLINE.QTY2 ,;
        Ord3   WITH Ord3 + INVLINE.QTY3 ,;
        Ord4   WITH Ord4 + INVLINE.QTY4 ,;
        Ord5   WITH Ord5 + INVLINE.QTY5 ,;
        Ord6   WITH Ord6 + INVLINE.QTY6 ,;
        Ord7   WITH Ord7 + INVLINE.QTY7 ,;
        Ord8   WITH Ord8 + INVLINE.QTY8 ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
      UNLOCK
    ENDIF
    *-- Update style Shipped Quantity
    SELECT STYLE
    =SEEK(INVLINE.STYLE)
    =RLOCK()
    REPLACE SHP1   WITH SHP1 - INVLINE.QTY1 ,;
      SHP2   WITH SHP2 - INVLINE.QTY2 ,;
      SHP3   WITH SHP3 - INVLINE.QTY3 ,;
      SHP4   WITH SHP4 - INVLINE.QTY4 ,;
      SHP5   WITH SHP5 - INVLINE.QTY5 ,;
      SHP6   WITH SHP6 - INVLINE.QTY6 ,;
      SHP7   WITH SHP7 - INVLINE.QTY7 ,;
      SHP8   WITH SHP8 - INVLINE.QTY8 ,;
      TOTSHP WITH SHP1+SHP2+SHP3+SHP4+SHP5+SHP6+SHP7+SHP8
    UNLOCK

    lnCOGSAmt = lnCOGSAmt + INVLINE.Totqty*INVLINE.Cost
    IF laSetups[4,2]='Y'
      *-- Sales Revenus <DEBIT> Category key '003'
      DO GLDIST WITH INVLINE.Gl_Sales,'003',(INVLINE.Totqty *INVLINE.Price),'VI',INVLINE.Invoice,;
        ldVDate,lcSysYear,lcSysPeriod,lcGlDist,INVLINE.cSalesAcnt,;
        InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
      IF InvHdr.ShipAmt <> 0
        *-- Discount <CREDIT> Category key '005'
        DO GLDIST WITH INVLINE.Gl_Sales,'005',-(INVLINE.Totqty*INVLINE.Price*ABS(InvHdr.Discount)/InvHdr.ShipAmt),;
          'VI',INVLINE.Invoice,ldVDate,lcSysYear,lcSysPeriod,;
          lcGlDist,INVLINE.cDiscAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
          InvHdr.nExRate

      ENDIF
    ENDIF
    SELECT INVLINE
    =SEEK('O'+INVLINE.ORDER+STR(LINENO,6),'ORDLINE')
    IF !EMPTY(ORDLINE.AltStyle) .AND. (ORDLINE.AltStyle <> INVLINE.STYLE)
      *DO lpSwchSty WITH Ordline.Style,Ordline.AltStyle
    ENDIF
    SELECT icStyHst
    *!* B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][Be
    IF !gfSEEK(INVLINE.STYLE + lcInvYear)
      =gfOpenTable('ICSTYHST','STYHST','SH','ICSTYHSX')
      SELECT ICSTYHSX

      APPEND BLANK
      REPLACE STYLE     WITH INVLINE.STYLE
      REPLACE CFISFYEAR WITH lcInvYear
      = GFREPLACE('')
      =gfTABLEUPDATE()

      *USE IN ICSTYHSX
      =gfcloseTable('ICSTYHSX')
      SELECT icStyHst
    ENDIF
    *!* B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][En

    *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
    *IF SEEK(INVLINE.Style+lcInvYear)
    IF gfSEEK(INVLINE.STYLE+lcInvYear)
      *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
      lnShipAmt = ROUND(INVLINE.Totqty*INVLINE.Price &lcExRSin InvHdr.nExRate ;
        &lcUntSin InvHdr.nCurrUnit, 2)
      lnDiscount = ROUND(INVLINE.Totqty*INVLINE.Price*InvHdr.DiscPcnt/100 &lcExRSin InvHdr.nExRate ;
        &lcUntSin InvHdr.nCurrUnit, 2)
      *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
      *=RLOCK()
      *REPLACE nSlsQty&lcInvPeriod  WITH nSlsQty&lcInvPeriod  - InvLine.TotQty ,;
      *        nSlsQty              WITH nSlsQty              - InvLine.TotQty ,;
      *        nSlsAmt&lcInvPeriod  WITH nSlsAmt&lcInvPeriod  - lnShipAmt  ,;
      *        nSlsAmt              WITH nSlsAmt              - lnShipAmt  ,;
      *        nDisAmt&lcInvPeriod  WITH nDisAmt&lcInvPeriod  - lnDiscount ,;
      *        nDisAmt              WITH nDisAmt              - lnDiscount ,;
      *        nCOGSAmt&lcInvPeriod WITH nCOGSAmt&lcInvPeriod - InvLine.TotQty*InvLine.Cost  ,;
      *        nCOGSAmt             WITH nCOGSAmt             - InvLine.TotQty*InvLine.Cost
      *UNLOCK
      REPLACE nSlsQty&lcInvPeriod  WITH nSlsQty&lcInvPeriod  - INVLINE.Totqty ,;
        nSlsQty              WITH nSlsQty              - INVLINE.Totqty ,;
        nSlsAmt&lcInvPeriod  WITH nSlsAmt&lcInvPeriod  - lnShipAmt  ,;
        nSlsAmt              WITH nSlsAmt              - lnShipAmt  ,;
        nDisAmt&lcInvPeriod  WITH nDisAmt&lcInvPeriod  - lnDiscount ,;
        nDisAmt              WITH nDisAmt              - lnDiscount ,;
        nCOGSAmt&lcInvPeriod WITH nCOGSAmt&lcInvPeriod - INVLINE.Totqty*INVLINE.Cost  ,;
        nCOGSAmt             WITH nCOGSAmt             - INVLINE.Totqty*INVLINE.Cost
      =GFREPLACE('')
      *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]
    ENDIF

    SELECT INVLINE
    SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,Totqty TO laInvQty
    IF laSetups[4,2]='Y'
      *-- Cost of Goods Sold <CREDIT> Category key '008'
      laGlArray[1,1]  = Gl_Sales
      laGlArray[1,2]  = '008'
      laGlArray[1,3]  = -1
      laGlArray[1,10] = cCOGSAcnt

      *-- Inventory Control <DEBIT> Category key '006'
      laGlArray[2,1]  = Gl_Cost
      laGlArray[2,2]  = '006'
      laGlArray[2,3]  = 1
      laGlArray[2,10] = cICAcnt
    ENDIF
    IF InvHdr.Direct_Inv
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[START]
      *lcRefer = 'CUST# '+ Customer.Account + "-" + Customer.BTName
      lcRefer = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUSTNO,oAriaApplication.GetHeaderText("LANG_CUSTNO",lcInvHeaderAlias))+' '+ Customer.Account + "-" + Customer.BTName
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
    ELSE
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
      *lcRefer = 'CUST# '+ Customer.Account + " Sales order " + InvHdr.ORDER
      lcRefer = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUSTNO,oAriaApplication.GetHeaderText("LANG_CUSTNO",lcInvHeaderAlias))+' '+ Customer.Account +;
        " "+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SO,oAriaApplication.GetHeaderText("LANG_SO",lcInvHeaderAlias))+" " + InvHdr.ORDER
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
    ENDIF
    *HBG 3/20/2005 Fix bug of not updating the Session # in GLDiST in case of void [Begin]
    *=gfStyCrl('4',Style,IIF(EMPTY(Invline.cWareCode),OrdLine.cWareCode,InvLine.cWareCode),Dyelot,ldVDate,InvHdr.Invoice,;
    @laInvQty,Cost,lcRefer,GlSession,'',8,'','',@laGlArray,InvLine.LineNo)

    *!* B609104,1 HES 12/06/2009 PO receipt update GLDIST incorrectly using Standard Cost [Start]
    *!*	  =gfStyCrl('4',Style,IIF(EMPTY(Invline.cWareCode),OrdLine.cWareCode,InvLine.cWareCode),Dyelot,ldVDate,InvHdr.Invoice,;
    *!*	            @laInvQty,Cost,lcRefer,lcGlSession,'',8,'','',@laGlArray,InvLine.LineNo)
    *B609162,1 TMI [START] fix problem STYLE object not found that caused by the fix # B609104
    *=gfStyCrl('4',Style,IIF(EMPTY(Invline.cWareCode),OrdLine.cWareCode,InvLine.cWareCode),Dyelot,ldVDate,InvHdr.Invoice,;
    @laInvQty,IIF(loFormSet.AriaForm1.Ariapageframe1.Page2.Invoiceeditregion1.CostingMethod = 'S',;
    IIF(STYLE.SEEK(INVLINE.Style),Style.TotCost,0),Cost),;
    lcRefer,lcGlSession,'',8,'','',@laGlArray,InvLine.LineNo)
    *!* B609302,1 MMT 06/15/2010 Fix bug of Error while voiding Invoice from Voiding multiple invoice program[Start]
    *!*	  =gfStyCrl('4',Style,IIF(EMPTY(Invline.cWareCode),OrdLine.cWareCode,InvLine.cWareCode),Dyelot,ldVDate,InvHdr.Invoice,;
    *!*	            @laInvQty,IIF(loFormSet.AriaForm1.Ariapageframe1.Page2.Invoiceeditregion1.CostingMethod = 'S',;
    *!*	            IIF(SEEK(INVLINE.Style,'STYLE','STYLE'),Style.TotCost,0),Cost),;
    *!*	            lcRefer,lcGlSession,'',8,'','',@laGlArray,InvLine.LineNo)
   
*B611772,1 Es Direct invoice voiding adds repeated record in GLDIST and STYINVJL if voiding crashed and user tried to void more than once. [Start]
   * =gfStyCrl('4',STYLE,IIF(EMPTY(INVLINE.cWareCode),ORDLINE.cWareCode,INVLINE.cWareCode),Dyelot,ldVDate,InvHdr.Invoice,;
      @laInvQty,IIF(lcStyCstMeth = 'S',;
      IIF(SEEK(INVLINE.STYLE,'STYLE','STYLE'),STYLE.TotCost,0),Cost),;
      lcRefer,lcGlSession,'',8,'','',@laGlArray,INVLINE.LINENO)
  =gfStyCrl('4',STYLE,IIF(EMPTY(INVLINE.cWareCode),ORDLINE.cWareCode,INVLINE.cWareCode),Dyelot,ldVDate,InvHdr.Invoice,;
      @laInvQty,IIF(lcStyCstMeth = 'S',;
      IIF(SEEK(INVLINE.STYLE,'STYLE','STYLE'),STYLE.TotCost,0),Cost),;
      lcRefer,lcGlSession,'',8,'','',@laGlArray,INVLINE.LINENO,.f.,.f.,.f.,.f.,IIF(!llMultiVoid,.T.,.F.))       
*B611772,1 Es Direct invoice voiding adds repeated record in GLDIST and STYINVJL if voiding crashed and user tried to void more than once. [End]      
    
    *!* B609302,1 MMT 06/15/2010 Fix bug of Error while voiding Invoice from Voiding multiple invoice program[End]
    *B609162,1 TMI [END] fix problem STYLE object not found that caused by the fix # B609104
    *!* B609104,1 HES 12/06/2009 PO receipt update GLDIST incorrectly using Standard Cost [End]

    *HBG [End]
    *C200876,1 TMI [Start] void invoice related bin locations files
    IF ASCAN(loFormSet.laEvntTrig , PADR('DLVODINV',10)) <> 0
      =loFormSet.mDoTrigger(PADR('DLVODINV',10))
    ENDIF
    *C200876,1 TMI [End  ]
  ENDSCAN

  *B610455,1 HIA 07/31/13 T20130620.0006 - CRM/AR - Transhst records not being deleted when an invoice is voided [Begin]
  IF gfSEEK('I'+lcInvoice,'TRNHIST')
    SELECT TRNHIST
    SCAN REST WHILE TRANTYPE+TRANNO+EMPLOYEE+STYLE = 'I'+lcInvoice
      GFDELETE()
    ENDSCAN
  ENDIF
  SELECT INVLINE
  *B610455,1 HIA 07/31/13 T20130620.0006 - CRM/AR - Transhst records not being deleted when an invoice is voided [End]

  IF 'AL' $ oAriaApplication.CompanySetupModules

    =gfOpenFile(oAriaApplication.DataDir+'PikTkt',oAriaApplication.DataDir+'OrdPik','SH')
    =gfOpenFile(oAriaApplication.DataDir+'Pack_Lin',oAriaApplication.DataDir+'Pack_Lin','SH')
    =gfOpenFile(oAriaApplication.DataDir+'PikLine',oAriaApplication.DataDir+'PikLine','SH')
    *!* B609803,1 MMT 01/24/2012 Packing list status is complete even after voiding invoice  [T20120110.0018][Start]
    =gfOpenFile(oAriaApplication.DataDir+'PACK_HDR',oAriaApplication.DataDir+'PACK_HDR','SH')
    *!* B609803,1 MMT 01/24/2012 Packing list status is complete even after voiding invoice  [T20120110.0018][END]
    *-- Update the alo. qty in Style , StyDye and Pik qty in Ordline.
    SELECT (lcInvHFile)
    =SEEK (lcInvoice)
    SCAN REST WHILE Invoice = lcInvoice
      IF SEEK('O'+ORDER,'ORDLINE')
        IF SEEK(ORDLINE.ORDER+&lcInvHFile..PikTkt,'PikTkt')
          SELECT PikTkt
          =RLOCK()
          REPLACE STATUS WITH "O"
          UNLOCK
        ENDIF
        *!* B609803,1 MMT 01/24/2012 Packing list status is complete even after voiding invoice  [T20120110.0018][Start]
        IF SEEK(&lcInvHFile..PikTkt,'Pack_hdr')
          SELECT PACK_HDR
          =RLOCK()
          REPLACE STATUS WITH " "
          UNLOCK
        ENDIF
        *!* B609803,1 MMT 01/24/2012 Packing list status is complete even after voiding invoice  [T20120110.0018][End]
        IF SEEK(&lcInvHFile..PikTkt+ORDLINE.ORDER,'PikLine')
          SELECT PIKLINE
          SCAN REST WHILE PikTkt + ORDER = &lcInvHFile..PikTkt+ORDLINE.ORDER
            IF SEEK('O'+ORDER+STR(LINENO,6),'OrdLine')
              SELECT ORDLINE
              =RLOCK()
              REPLACE PIK1    WITH PIK1 + PIKLINE.PIK1 ,;
                PIK2    WITH PIK2 + PIKLINE.PIK2 ,;
                PIK3    WITH PIK3 + PIKLINE.PIK3 ,;
                PIK4    WITH PIK4 + PIKLINE.PIK4 ,;
                PIK5    WITH PIK5 + PIKLINE.PIK5 ,;
                PIK6    WITH PIK6 + PIKLINE.PIK6 ,;
                PIK7    WITH PIK7 + PIKLINE.PIK7 ,;
                PIK8    WITH PIK8 + PIKLINE.PIK8 ,;
                TotPIK  WITH TotPIK + PIKLINE.TotPIK ,;
                PikTkt  WITH PIKLINE.PikTkt ,;
                PikDate WITH PIKLINE.PikDate ,;
                Picked  WITH .T. ,;
                Dyelot  WITH  PIKLINE.Dyelot

              IF SEEK(ORDLINE.PikTkt,'Pack_Lin')
                SELECT Pack_Lin
                LOCATE REST WHILE Pack_No = ORDLINE.PikTkt FOR nOrdLineNo = ORDLINE.LINENO
                IF FOUND()
                  SELECT ORDLINE
                  *!* B608664,1 MMT 08/26/2008 Fix bug of updating npck fields in Ordline File[Start]
                  *B608846,1 MMT 04/14/2009 Make invoice saving update npck Fields in ordline[Start]
                  REPLACE nPck1 WITH nPck1 + Pack_Lin.QTY1 ,;
                    nPck2 WITH nPck2 + Pack_Lin.QTY2 ,;
                    nPck3 WITH nPck3 + Pack_Lin.QTY3 ,;
                    nPck4 WITH nPck4 + Pack_Lin.QTY4 ,;
                    nPck5 WITH nPck5 + Pack_Lin.QTY5 ,;
                    nPck6 WITH nPck6 + Pack_Lin.QTY6 ,;
                    nPck7 WITH nPck7 + Pack_Lin.QTY7 ,;
                    nPck8 WITH nPck8 + Pack_Lin.QTY8
                  *B608846,1 MMT 04/14/2009 Make invoice saving update npck Fields in ordline[End]
                  *!* B608664,1 MMT 08/26/2008 Fix bug of updating npck fields in Ordline File[End]
                ENDIF
              ENDIF
              UNLOCK

              IF laSetups[8,2]='Y' AND SEEK(ORDLINE.STYLE,'Style') AND STYLE.cDye_Flg = 'Y' AND ;
                  SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+ORDLINE.Dyelot,'StyDye')
                SELECT StyDye
                =RLOCK()
                REPLACE Alo1   WITH Alo1 + ORDLINE.PIK1 ,;
                  Alo2   WITH Alo2 + ORDLINE.PIK2 ,;
                  Alo3   WITH Alo3 + ORDLINE.PIK3 ,;
                  Alo4   WITH Alo4 + ORDLINE.PIK4 ,;
                  Alo5   WITH Alo5 + ORDLINE.PIK5 ,;
                  Alo6   WITH Alo6 + ORDLINE.PIK6 ,;
                  Alo7   WITH Alo7 + ORDLINE.PIK7 ,;
                  Alo8   WITH Alo8 + ORDLINE.PIK8 ,;
                  TotAlo WITH TotAlo + ORDLINE.TotPIK
                UNLOCK
              ENDIF
              IF SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')
                SELECT StyDye
                =RLOCK()
                REPLACE Alo1   WITH Alo1 + ORDLINE.PIK1 ,;
                  Alo2   WITH Alo2 + ORDLINE.PIK2 ,;
                  Alo3   WITH Alo3 + ORDLINE.PIK3 ,;
                  Alo4   WITH Alo4 + ORDLINE.PIK4 ,;
                  Alo5   WITH Alo5 + ORDLINE.PIK5 ,;
                  Alo6   WITH Alo6 + ORDLINE.PIK6 ,;
                  Alo7   WITH Alo7 + ORDLINE.PIK7 ,;
                  Alo8   WITH Alo8 + ORDLINE.PIK8 ,;
                  TotAlo WITH TotAlo + ORDLINE.TotPIK
                UNLOCK
              ENDIF
              IF SEEK(ORDLINE.STYLE,'Style')
                SELECT STYLE
                =RLOCK()
                REPLACE Alo1   WITH Alo1 + ORDLINE.PIK1 ,;
                  Alo2   WITH Alo2 + ORDLINE.PIK2 ,;
                  Alo3   WITH Alo3 + ORDLINE.PIK3 ,;
                  Alo4   WITH Alo4 + ORDLINE.PIK4 ,;
                  Alo5   WITH Alo5 + ORDLINE.PIK5 ,;
                  Alo6   WITH Alo6 + ORDLINE.PIK6 ,;
                  Alo7   WITH Alo7 + ORDLINE.PIK7 ,;
                  Alo8   WITH Alo8 + ORDLINE.PIK8 ,;
                  TotAlo WITH TotAlo + ORDLINE.TotPIK
                UNLOCK
              ENDIF
            ENDIF
            SELECT PIKLINE
            DELETE
          ENDSCAN
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF

  =SEEK(lcInvoice,'InvHdr')
  *-- update the customer history file
  SELECT arCusHst
  IF SEEK(InvHdr.Account+lcInvYear)
    lnShipAmt = ROUND(InvHdr.ShipAmt &lcExRSin InvHdr.nExRate &lcUntSin InvHdr.nCurrUnit,2)
    lnDiscount= ROUND(ABS(InvHdr.Discount) &lcExRSin InvHdr.nExRate &lcUntSin InvHdr.nCurrUnit,2)
    =RLOCK()
    REPLACE nSlsQty&lcInvPeriod  WITH nSlsQty&lcInvPeriod  - InvHdr.Ship ,;
      nSlsQty              WITH nSlsQty              - InvHdr.Ship ,;
      nSlsAmt&lcInvPeriod  WITH nSlsAmt&lcInvPeriod  - lnShipAmt   ,;
      nSlsAmt              WITH nSlsAmt              - lnShipAmt   ,;
      nDisAmt&lcInvPeriod  WITH nDisAmt&lcInvPeriod  - lnDiscount  ,;
      nDisAmt              WITH nDisAmt              - lnDiscount  ,;
      nCOGSAmt&lcInvPeriod WITH nCOGSAmt&lcInvPeriod - lnCOGSAmt   ,;
      nCOGSAmt             WITH nCOGSAmt             - lnCOGSAmt
    UNLOCK
  ENDIF
  SELECT InvHdr
  IF laSetups[4,2]='Y'
    *-- Freight income <DEBIT> Category key '004'
    IF llIsEngland
      =gfOpenFile(oAriaApplication.DataDir+'InvChrg',oAriaApplication.DataDir+'InvChrg','SH')
      IF SEEK(InvHdr.Invoice,'INVCHRG')
        SELECT InvChrg
        SCAN REST WHILE Invoice= InvHdr.Invoice
          DO GLDIST WITH InvHdr.Link_Code,'004',InvChrg.nChrgAmnt,'VI',;
            InvHdr.Invoice,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
            InvChrg.cFrgtAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
        ENDSCAN
      ENDIF
    ELSE
      DO GLDIST WITH InvHdr.Link_Code,'004',(InvHdr.FREIGHT+InvHdr.INSUR+InvHdr.COD),;
        'VI',InvHdr.Invoice,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
        InvHdr.cFrgtAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
    ENDIF
    IF laSetups[9,2]='Y'
      *-- Tax Income <DEBIT> Category key '014'

      *:* E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[Start]
      *DO GLDIST WITH InvHdr.LINK_CODE,'014',InvHdr.TAX_AMT+InvHdr.nPstAmt+InvHdr.nHstAmt,;
      'VI',InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,;
      lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
      InvHdr.nExRate

      IF UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='CANADA'
        =SEEK(IIF(EMPTY(InvHdr.STORE),'M'+InvHdr.Account,'S'+InvHdr.Account+InvHdr.STORE),'CUSTOMER')
        IF lfChkStatHst(LEFT(Customer.cAddress4,6))
          *!* B611015,1 MMT 06/10/2015 Incorrect GL account for PST taxes while voiding invoices[T20150528.0014][Start]
          *!*	        DO GLDIST WITH InvHdr.LINK_CODE,'030',InvHdr.nHstAmt,'VI',InvHdr.Invoice,;
          *!*	          ldVDate,lcSysYear,lcSysPeriod,lcGlDist,InvHdr.cTaxAcnt,;
          *!*	          InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
          DO GLDIST WITH InvHdr.Link_Code,'030',InvHdr.NHSTAMT,'VI',InvHdr.Invoice,;
            ldVDate,lcSysYear,lcSysPeriod,lcGlDist,'',;
            InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
          *!* B611015,1 MMT 06/10/2015 Incorrect GL account for PST taxes while voiding invoices[T20150528.0014][End]
        ELSE
          *B609557,1 WAM 03/24/2011 Fix updating GST Tax GL Account when Void invoice for Canadian customers
          *DO GLDIST WITH InvHdr.LINK_CODE,'014',InvHdr.TAX_AMT+InvHdr.nPstAmt+InvHdr.nHstAmt,;
          'VI',InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,;
          lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
          InvHdr.nExRate
          *!* B611015,1 MMT 06/10/2015 Incorrect GL account for PST taxes while voiding invoices[T20150528.0014][Start]
          *!*	        DO GLDIST WITH InvHdr.LINK_CODE,'014',InvHdr.TAX_AMT,;
          *!*	          'VI',InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,;
          *!*	          lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
          *!*	          InvHdr.nExRate
          *!*	        *B609557,1 WAM (End)

          *!*	        DO GLDIST WITH InvHdr.LINK_CODE,'029',InvHdr.nPstAmt,'VI',InvHdr.Invoice,;
          *!*	          ldVDate,lcSysYear,lcSysPeriod,lcGlDist,InvHdr.cTaxAcnt,;
          *!*	          InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
          DO GLDIST WITH InvHdr.Link_Code,'014',InvHdr.TAX_AMT,;
            'VI',InvHdr.Invoice,ldVDate,lcSysYear,lcSysPeriod,;
            lcGlDist,'',InvHdr.cCurrCode,InvHdr.nCurrUnit,;
            InvHdr.nExRate

          DO GLDIST WITH InvHdr.Link_Code,'029',InvHdr.NPSTAMT,'VI',InvHdr.Invoice,;
            ldVDate,lcSysYear,lcSysPeriod,lcGlDist,'',;
            InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
          *!* B611015,1 MMT 06/10/2015 Incorrect GL account for PST taxes while voiding invoices[T20150528.0014][End]
        ENDIF
      ELSE
        DO GLDIST WITH InvHdr.Link_Code,'014',InvHdr.TAX_AMT+InvHdr.NPSTAMT+InvHdr.NHSTAMT,;
          'VI',InvHdr.Invoice,ldVDate,lcSysYear,lcSysPeriod,;
          lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
          InvHdr.nExRate
      ENDIF
      *:* E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[End]

    ENDIF
    *-- Account receivable <CREDIT> Category key '001'
    DO GLDIST WITH InvHdr.Link_Code,'001',-(InvHdr.TOTALCHG),'VI', ;
      InvHdr.Invoice,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
      InvHdr.cArAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
      InvHdr.nExRate
  ENDIF
  SELECT InvHdr
  =RLOCK()
  *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[START]
  *!*	REPLACE STATUS     WITH 'V',;
  *!*	  CUSTPO     WITH '*VOID*',;
  *!*	  VDATE      WITH ldVDate
  REPLACE STATUS     WITH 'V',;
    CustPo     WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOIDCUSTPO,oAriaApplication.GetHeaderText("LANG_VOIDCUSTPO",lcInvHeaderAlias)),;
    VDATE      WITH ldVDate
  *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
  REPLACE VCOMMAMT1  WITH IIF(VCOMMAMT1=0,CommAmt1,VCOMMAMT1),;
    VCOMMAMT2  WITH IIF(VCOMMAMT2=0,CommAmt2,VCOMMAMT2),;
    VSHIP      WITH IIF(VSHIP=0,Ship,VSHIP),;
    VSHIPAMT   WITH IIF(VSHIPAMT=0,ShipAmt,VSHIPAMT),;
    VCOD       WITH IIF(VCOD=0,COD,VCOD),;
    VCOD_AMT   WITH IIF(VCOD_AMT=0,COD_AMT,VCOD_AMT),;
    VINSUR     WITH IIF(VINSUR=0,INSUR,VINSUR),;
    VFREIGHT   WITH IIF(VFREIGHT=0,FREIGHT,VFREIGHT),;
    VDISCOUNT  WITH IIF(VDISCOUNT=0,Discount,VDISCOUNT),;
    VTOTALCHG  WITH IIF(VTOTALCHG=0,TOTALCHG,VTOTALCHG),;
    CommAmt1   WITH 0,;
    CommAmt2   WITH 0,;
    Ship       WITH 0,;
    ShipAmt    WITH 0,;
    COD        WITH 0,;
    COD_AMT    WITH 0,;
    INSUR      WITH 0,;
    FREIGHT    WITH 0,;
    Discount   WITH 0,;
    TOTALCHG   WITH 0 ,;
    nVPstAmt   WITH IIF(nVPstAmt=0,NPSTAMT,nVPstAmt)  ,;
    NPSTAMT    WITH 0        ,;
    VTAX_AMT   WITH IIF(VTAX_AMT=0,TAX_AMT,VTAX_AMT)  ,;
    TAX_AMT    WITH 0        ,;
    nvCharges  WITH IIF(nvCharges=0,NCHARGES,nvCharges) ,;
    NCHARGES   WITH 0 ,;
    nVHstAmt   WITH IIF(nVHstAmt=0,NHSTAMT,nVHstAmt)  ,;
    NHSTAMT    WITH 0

  *-- Custom process for A.S.T. sportwear.
  IF TYPE('loFormSet')='O'
    =loFormSet.mDoTrigger(PADR('VOIDCOM3',10))
  ENDIF
  UNLOCK
  *!* E612339,1 MMT 02/10/2021 Invoice Saving does not update Entity table when Invoice is created/Voided [T20201008.0003][Start]
  IF TYPE('loFormSet') ="O"
    loFormSet.mupdateentitytable()
  ENDIF
  *!* E612339,1 MMT 02/10/2021 Invoice Saving does not update Entity table when Invoice is created/Voided [T20201008.0003][End]

  *-- Update consolidated invoice header
  IF InvHdr.CONSOL='Y'
    SELECT CONSINVH
    =SEEK(InvHdr.Invoice)
    SCAN REST WHILE Invoice = InvHdr.Invoice
      *B609723,1 WAM 11/01/2011 Fix updating status in CONSINVH file while voiding consolidated invoice
      *REPLACE REST STATUS WITH 'V',;
      CUSTPO WITH '*VOID*'
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[START]
      *!*	    REPLACE STATUS WITH 'V',;
      *!*	      CUSTPO WITH '*VOID*'
      REPLACE STATUS WITH 'V',;
        CustPo WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOIDCUSTPO,oAriaApplication.GetHeaderText("LANG_VOIDCUSTPO",lcInvHeaderAlias))
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
      *B609723,1 WAM 11/01/2011 (End)

    ENDSCAN
  ENDIF
  *-- Update general ledger distribution file
  *HBG 3/20/2005 Fix bug of not updating the Session # in GLDiST in case of void [Begin]
  *IF laSetups[4,2]='Y' AND !SEEK(InvHdr.Invoice+'VI'+GlSession,'GLDIST')
 


  *!* E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL [start]
  * IF laSetups[4,2]='Y' AND !SEEK(InvHdr.Invoice+'VI'+lcGlSession,'GLDIST')
  IF laSetups[4,2]='Y' AND !gfSEEK(InvHdr.Invoice+'VI'+lcGlSession,'GLDIST')
  *!* E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL [end]
    *HBG [End]
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[START]
    *WAIT 'Updating the general ledger distribution file ...' WINDOW NOWAIT
    WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATEGLDIST,oAriaApplication.GetHeaderText("LANG_UPDATEGLDIST",lcInvHeaderAlias)) WINDOW NOWAIT
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
    *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start] 
*!*	    SELECT (lcGlDist)
*!*	    SCAN
*!*	      *HBG 3/20/2005 Fix bug of not updating the Session # in GLDiST in case of void [Begin]
*!*	      *REPLACE GLSESSION WITH GlSession
*!*	      REPLACE GLSESSION WITH lcGlSession
*!*	      *HBG [End]
*!*	      =gfAdd_Info(lcGlDist)
*!*	    ENDSCAN
    lcGLDISTCursorUpdate = gfGetRemoteProp('lcCursorUpdate','GLDIST')
    *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[End] 
    *!* E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL [start]
    *USE
 
 *B611924,1 ES 10/17/2019 Bug in Invoice Saving program	[T20190409.0005] [Start]
  SELECT (lcGlDist)
    IF SEEK(INVHDR.INVOICE+'005')
      SUM nglamount TO lnDiscAmt REST WHILE Tran_no+catg_key =  INVHDR.INVOICE+'005'
      IF ABS(lnDiscAmt) <> ABS(INVHDR.VDISCOUNT)
        lnDiscDiff = ABS(lnDiscAmt) - ABS(INVHDR.VDISCOUNT)
        =SEEK(INVHDR.INVOICE+'005')
        REPLACE nglamount WITH nglamount + lnDiscDiff IN (lcGlDist)
        LNEQVAMNT  = &lcGlDist..nglamount 
        IF &lcGlDist..CCURRCODE <> OARIAAPPLICATION.BASECURRENCY
                  IF &lcGlDist..ncurrunit > 0 AND &lcGlDist..nexrate > 0
                    LCUNTSIN = ''
                    LNEXRATE = &lcGlDist..nexrate 
                    LNCURRUNIT = &lcGlDist..ncurrunit
                    LCEXRSIN = GFGETEXSIN(@LCUNTSIN, &lcGlDist..CCURRCODE)
                    LNEQVAMNT  = ROUND(LNEQVAMNT &LCEXRSIN LNEXRATE &LCUNTSIN LNCURRUNIT, 2)
                  ENDIF  
            ENDIF
            REPLACE nEqvAmnt WITH LNEQVAMNT  IN (lcGlDist) 
      ENDIF
    ENDIF
 *B611924,1 ES 10/17/2019 Bug in Invoice Saving program	[T20190409.0005][End]
 
    SELECT (lcGlDist)
    SCAN 
      *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start] 
      REPLACE GLSESSION WITH lcGlSession
      =gfAdd_Info(lcGlDist)
      *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[End] 
      SCATTER MEMVAR MEMO
      *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
      *m.Oid = gfTempName()
      *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
      SELECT GLDIST
      *APPEND FROM (oAriaApplication.WorkDir+lcGlDist)
      APPEND BLANK
      GATHER MEMVAR MEMO
      *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start] 

*!* B611719,1 Es 02/19/2019 Modify issue "Duplicated GL entries for Category 001 in GLDIST"  [Start]  
     * =gfreplace('')
*!* B611719,1 Es 02/19/2019 Modify issue "Duplicated GL entries for Category 001 in GLDIST" [End]
      IF !EMPTY(lcGLDISTCursorUpdate)
        SELECT (lcGLDISTCursorUpdate)
        APPEND BLANK
        GATHER MEMVAR MEMO
      ENDIF
      *!* E303951,2 MMT 12/02/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]       
    ENDSCAN
    *E611821,1 MMT 11/13/2019 Call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
    =gfCreateGLEntries(lcGlDist,INVHDR.Account)
    *E611821,1 MMT 11/13/2019 Call the new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
    SELECT (lcGlDist)
    USE 
   *B611772,1 Es Direct invoice voiding adds repeated record in GLDIST and STYINVJL if voiding crashed and user tried to void more than once. [Start]
    IF llMultiVoid
   *B611772,1 Es Direct invoice voiding adds repeated record in GLDIST and STYINVJL if voiding crashed and user tried to void more than once. [End]
    
    SELECT GLDIST
    =gfTableUpdate()
   
   *B611772,1 Es Direct invoice voiding adds repeated record in GLDIST and STYINVJL if voiding crashed and user tried to void more than once. [Start]
   ENDIF
   *B611772,1 Es Direct invoice voiding adds repeated record in GLDIST and STYINVJL if voiding crashed and user tried to void more than once. [End]

    
  ENDIF
  *!* E303951,1 AHH 29/03/2018 use the GLDIST table remotely not native, because of conversion to SQL [End]
  
  *-- Update customer A/R
  IF EMPTY(InvHdr.cFacCode) .OR. laSetups[17,2]='Y'
    SELECT DEBIT
    *B608576,1 WAM 05/29/2008 Key-off invoice only if have record in debit file
    *=SEEK('1'+InvHdr.Invoice)
    IF SEEK('1'+InvHdr.Invoice)
      *B608576,1 WAM 05/29/2008 (End)
      lnCreAmt = 0
      *-- Scan debit records.
      SCAN REST WHILE TRANTYPE+TRAN+cinstalno='1'+InvHdr.Invoice
        SCATTER MEMVAR MEMO
        m.cShToOpn  = 'Y'
        INSERT INTO (lcTmpAr) FROM MEMVAR
        lnCreAmt = lnCreAmt - m.AMOUNT
      ENDSCAN
      *-- Add Credit Data to master credit file[Begin]
      m.Batch     = gfSequence('BATCH')
      m.Tran      = InvHdr.Invoice
      m.TRANTYPE  = 'I'
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[START]
      *m.Desc      = 'CREDIT ADJ./VOID INV'
      m.Desc      = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CRVOIDINV,oAriaApplication.GetHeaderText("LANG_CRVOIDINV",lcInvHeaderAlias))
      *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[END]
      m.Reference = 'Void Invoice'
      m.AMOUNT    = lnCreAmt
      m.cinstalno = ''
      m.TRANDATE  = ldVDate
      SELECT CREDIT
      INSERT INTO ('CREDIT') FROM MEMVAR
      =gfAdd_Info('CREDIT')

      *-- Add Credit Data to temporary history file[Begin]
      INSERT INTO (lcTmpAr) FROM MEMVAR

      *-- Call key off program.
      oKeyOff    = NEWOBJECT('ArKeyOff',oAriaApplication.classdir+'Ar.vcx')
      llSaveComp = oKeyOff.mKeyOff(InvHdr.Account,ldVDate,ABS(lnCreAmt),lnCreAmt,lcTmpAr)
      oKeyOff    = NULL
      *B608576,1 WAM 05/29/2008 Key-off invoice only if have record in debit file
    ENDIF
    *B608576,1 WAM 05/29/2008 (End)

    *B608267,1 WAM 09/16/07 Stop updae customer balance after void invoice. It is already updated by the KEYOFF class
    *!*	  SELECT CUSTOMER
    *!*	  IF SEEK('M'+InvHdr.Account)
    *!*	    lnEqvAmnt = ABS(InvHdr.VTotalChg) &lcExRSin INVHDR.nExRate &lcUntSin InvHdr.nCurrUnit
    *!*	    =RLOCK()
    *!*	    *-- Aging Type
    *!*	    IF laSetups[16,2] = "D"
    *!*	    *-- Aging by date
    *!*	      REPLACE Current WITH Current - lnEqvAmnt,;
    *!*	              TotAge  WITH Current + Age30 + Age60 + Age90 + Age120
    *!*	    *-- Aging by Terms
    *!*	    ELSE
    *!*	      REPLACE TerCurrent WITH TerCurrent - lnEqvAmnt,;
    *!*	              TotAge     WITH TerCurrent + TerAge30 + TerAge60 + TerAge90 + TerAge120
    *!*	    ENDIF
    *!*	    REPLACE NetBal     WITH TotAge  - ChgBack + OpenCr,;
    *!*	            YTDBal     WITH YTDBal  - lnEqvAmnt,;
    *!*	            CrAvail    WITH IIF(CrLimit > 0,CrAvail + lnEqvAmnt, CrAvail) ,;
    *!*	            nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
    *!*	    UNLOCK
    *!*	  ENDIF
    *B608267,1 WAM 09/16/07 (End)
  ENDIF
  *-- When void invoice we should not send 810
  IF 'EB' $ oAriaApplication.CompanySetupModules
    =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
    SELECT EDITRANS
    IF SEEK('810'+PADR(InvHdr.Invoice,40)+'A'+InvHdr.Account)
      DELETE
    ENDIF
    IF SEEK('810'+PADR(InvHdr.Invoice,40)+'F'+InvHdr.cFacCode)
      DELETE
    ENDIF
  ENDIF

  *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][Begin]
  SELECT icStyHst
  =gfTABLEUPDATE()
  *!* E303218,1 HIA 08/22/2012 Convert ICSTYHST to SQL [T20120813.0035][End]

  *!* B608948,1 MMT 07/28/2009 Update Trnhist Table when Invoice is Voided[start]
  SELECT TRNHIST
  =gfTABLEUPDATE()
  gfcloseTable("TRNHIST")
  *!* B608948,1 MMT 07/28/2009 Update Trnhist Table when Invoice is Voided[End]

  SELECT InvHdr
  WAIT CLEAR

  *B128066,1 AMH Fix bug of file is used by another user [Start]
  SET REPROCESS TO lnRePro
  *B128066,1 AMH [End]

  RETURN
  *--end of gpVoidInv

  *!*************************************************************
  *! Name      : lfCreatHst
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 12/16/2002
  *! Purpose   : Create temporary History file which used key off process
  *!*************************************************************
  *! Parameters:  lcTmpAr
  *!*************************************************************
  *! Returns   :  None
  *!*************************************************************
FUNCTION lfCreatHst
  LPARAMETERS lcTmpAr

  SELECT ARHIST
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+4,18]
  laFileStru[lnFileStru+1,1] = "cYear"
  laFileStru[lnFileStru+1,2] = "C"
  laFileStru[lnFileStru+1,3] = 4
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = "cPrd"
  laFileStru[lnFileStru+2,2] = "C"
  laFileStru[lnFileStru+2,3] = 2
  laFileStru[lnFileStru+2,4] = 0
  laFileStru[lnFileStru+3,1] = "cShToOpn"
  laFileStru[lnFileStru+3,2] = "C"
  laFileStru[lnFileStru+3,3] = 1
  laFileStru[lnFileStru+3,4] = 0
  laFileStru[lnFileStru+4,1] = "nTrnNewAmn"
  laFileStru[lnFileStru+4,2] = "N"
  laFileStru[lnFileStru+4,3] = 11
  laFileStru[lnFileStru+4,4] = 2
  FOR lnCount = 1 TO 4
    STORE '' TO laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
      laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
      laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
      laFileStru[lnFileStru+lnCount,16]
    STORE 0  TO laFileStru[lnFileStru+lnCount,17],  laFileStru[lnFileStru+lnCount,18]
  ENDFOR
  DIMENSION laTags[3,2]
  laTags[1,1] = 'Account+TranType+Tran+cInstalNo'
  laTags[1,2] = lcTmpAr
  laTags[2,1] = 'Account+Tran+cInstalNo'
  laTags[2,2] = 'Tran'
  laTags[3,1] = 'cShToOpn'
  laTags[3,2] = 'Show'
  *--Call Create temp file.
  =gfCrtTmp(lcTmpAr,@laFileStru,@laTags)
  SET ORDER TO TAG (lcTmpAr) IN (lcTmpAr)

  RETURN
  *-- end of lfCreatHst.

  *!*************************************************************
  *! Name      : gfAcOpCrdt
  *! Developer : WAM
  *! Date      : 10/10/2002
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
  PARAMETERS lcAccount,lcCurrency, lcOpenCredits
  PRIVATE lnAlias
  lnAlias = SELECT()
  &lcOpenCredits = .F.
  =gfOpenFile(oAriaApplication.DataDir+'CREDIT', oAriaApplication.DataDir+'CREDIT','SH')
  SELECT CREDIT
  =SEEK(lcAccount)
  LOCATE REST WHILE Account+TRAN+DTOS(TRANDATE)=lcAccount FOR cCurrCode=lcCurrency
  &lcOpenCredits = FOUND()
  SELECT (lnAlias)
  RETURN

  *!*************************************************************
  *! Name      : gpOpnCredits
  *! Developer : WAM
  *! Date      : 10/10/2002
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
  LPARAMETERS lcAccount,lcOrder,lcStore,lcPikTkt,lnTotalChg,lcKeyOff,lcAppCrdt,lcCurrency
  DO FORM (oAriaApplication.ScreenHome+"AR\ARAPCRD") WITH ;
    lcAccount,lcOrder,lcStore,lcPikTkt,lnTotalChg,lcKeyOff,lcAppCrdt,lcCurrency

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
  *HBG 09/01/2004 Send the invoice DueDate as a parameter [Begin]
  *!*	PARAMETERS lcInvoice,lcOrder,lcStore,lcPikTkt,lnTotalChg,ldInvDate,;
  *!*	           lcInstHdr,lcInstLine, lcActiveMode
  *!*	DO FORM (oAriaApplication.ScreenHome+"AR\ARINST") WITH ;
  *!*	 lcInvoice,lcOrder,lcStore,lcPikTkt,lnTotalChg,ldInvDate,lcInstHdr,lcInstLine, lcActiveMode

  PARAMETERS lcInvoice,lcOrder,lcStore,lcPikTkt,lnTotalChg,ldInvDate,ldDueDate,;
    lcInstHdr,lcInstLine, lcActiveMode
  DO FORM (oAriaApplication.ScreenHome+"AR\ARINST") WITH ;
    lcInvoice,lcOrder,lcStore,lcPikTkt,lnTotalChg,ldInvDate,ldDueDate,lcInstHdr,lcInstLine, lcActiveMode
  *HBg [End]
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
  LPARAMETERS lcUpsFile,lcInvoice,lcAccount,lcOrderNo,lcStore,lcPikTkt,llUpsInsur,llCod,;
    lnShipAmt,lcShipVia,lcWareCode,lcCartons,lcWeight,lcFreight,lcInsur,lcCod,;
    lcCodAmnt, lcZonesFile, lcRatesFile

  lnAlias = SELECT()
  DO FORM (oAriaApplication.ScreenHome+"AR\ARUPS") WITH ;
    lcUpsFile,lcInvoice,lcAccount,lcOrderNo,lcStore,lcPikTkt,llUpsInsur,llCod,;
    lnShipAmt,lcShipVia,lcWareCode,&lcCartons,&lcWeight,&lcFreight,&lcInsur,&lcCod,;
    &lcCodAmnt, lcZonesFile, lcRatesFile

  IF EMPTY(lcInvoice)     && Add mode
    SELECT (lcUpsFile)
    =SEEK(lcOrderNo+lcStore+lcPikTkt)
    SUM REST Weight,Decl_Value,FREIGHT,INSUR,COD,TotalCod ;
      TO       &lcWeight,lnDecValue,&lcFreight,&lcInsur,&lcCod,&lcCodAmnt ;
      WHILE ORDER+STORE+PikTkt+STR(CARTONS,5) = lcOrderNo+lcStore+lcPikTkt
    &lcCartons = _TALLY   && Number of cartons
  ENDIF
  SELECT (lnAlias)

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
  *!             lcZonesFile: Zones File name
  *!             lcRatesFile: Rates File name
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfGetFright()
  *!*************************************************************
FUNCTION lfGetFright
  PARAMETERS lcUpsFrom,lcToZip,lcUpsType,llCod,llUpsInsur,lcFreight,lcInsur,;
    lcCod,lcCartons,lcWeight,lnCodCharge,lnInsCharge,lnTotShpAmt,lcChrgType,;
    lcZonesFile, lcRatesFile
  PRIVATE lcCurrExac,lcCurrNear

  IF INLIST(lcChrgType,'A','C')
    *--  Recalculate the cod
    *!* B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045][start]
    *!*	  &lcCod = IIF(llCod,IIF('USUPS' $ lcUpsType,lnCodCharge*&lcCartons,&lcCod),0)
    &lcCod = IIF(llCod,IIF('USUPS' $ lcUpsType OR INLIST(ALLTRIM(lcCarrierId),'UPS','FEDEX'),lnCodCharge*&lcCartons,&lcCod),0)
    *!* B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045][End]
  ENDIF
  IF INLIST(lcChrgType,'A','I')
    *--  Recalculate the insurance
    *!* B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [Start]
    *!*	  &lcInsur = IIF(llUpsInsur,IIF('USUPS' $ lcUpsType AND lnTotShpAmt >=100,;
    *!*	    FLOOR(lnTotShpAmt/100)*lnInsCharge,0),0)
    &lcInsur = IIF(llUpsInsur,IIF(('USUPS' $ lcUpsType OR  INLIST(ALLTRIM(lcCarrierId),'UPS','FEDEX')) AND lnTotShpAmt >=100,;
      FLOOR(lnTotShpAmt/100)*lnInsCharge,0),0)
    *!* B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [Start]

  ENDIF

  IF lcUpsType = 'USUPSC' OR INLIST(lcChrgType,'I','C')
    RETURN
  ENDIF
  IF !SEEK(lcUpsType+lcUpsFrom+lcToZip,lcZonesFile) .AND. ;
      IIF(SUBSTR(lcUpsType,1,2)='CA',;
      !SEEK(lcUpsType+lcUpsFrom+SUBSTR(lcToZip,1,1),lcZonesFile),.T.)

    IF !gfDoTriger('ARDINV',PADR('DISABMSG',10))
      *-- Message : 40027
      *-- Invalid shipper postal zone or shipto zone! Unable to calculate freight.
      *-- Button : 00000
      *-- Ok
      =gfModalGen('INM40027B00000','ALERT')
    ENDIF
    RETURN
  ENDIF
  DO CASE
      *!* B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [Start]
      *!*	CASE 'USUPS' $ lcUpsType
    CASE 'USUPS' $ lcUpsType  OR  INLIST(ALLTRIM(lcCarrierId),'UPS','FEDEX')
      *!* B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [END]

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
      *!*	B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [Start]
      *!*	CASE 'USUPS' $ lcUpsType
    CASE 'USUPS' $ lcUpsType OR  INLIST(ALLTRIM(lcCarrierId),'UPS','FEDEX')
      *!*	B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [Start]

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
      =SEEK(LEFT(lcUpsType,6)+'U'+&lcZonesFile..Zone+lcWghtPCrtn,lcRatesFile)
    ELSE
      =SEEK(LEFT(lcUpsType,6)+'S'+&lcZonesFile..Zone+lcWghtPCrtn,lcRatesFile)
    ENDIF
    &lcFreight = CEILING(&lcWeight) * &lcRatesFile..cFrtRate
  ELSE
    =SEEK(lcUpsType+&lcZonesFile..Zone+lcWghtPCrtn,lcRatesFile)
    &lcFreight = &lcCartons * &lcRatesFile..cFrtRate
  ENDIF
  SET EXACT &lcCurrExac
  SET NEAR &lcCurrNear
  *!*	B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [Start]
  *!*	IF 'USUPS' $ lcUpsType
  IF 'USUPS' $ lcUpsType OR  INLIST(ALLTRIM(lcCarrierId),'UPS','FEDEX')
    *!*	B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [End]

    &lcCod   = IIF(llCod,lnCodCharge*&lcCartons,&lcCod)
    &lcInsur = IIF(lnTotShpAmt >=100 .AND. llUpsInsur,;
      FLOOR(lnTotShpAmt/100)*lnInsCharge,&lcInsur)
  ENDIF
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
  *!             lcZonesFile: Name of UPS Zones file
  *!             lcRatesFile: Name of UPS Rates file
  *!             lCCARTRCKNO: Cariier Carton Tracking number
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =gpUpsChrg()
  *!*************************************************************
FUNCTION gpUpsChrg

  *E302947,1 WAM 07/27/2011 Add new parameter to return Cariier Carton Tracking number
  *PARAMETERS lcAccount,lcStore,lcOrderNo,lcPikTKt,lcWareCode,lnShipAmt,llCod,;
  llupsinsur,lnDscPrcnt,lcTaxRate,lcPstRule,lnPstRate,lcUpsFile,;
  lcShipVia,lcCOD_AMT,lcFreight,lcInsur,lcCOD,lcCartons,lcWeight,;
  lcTaxAmnt,lcPstAmnt,lnTaxDue,lcChrgType,lcZonesFile, lcRatesFile
  PARAMETERS lcAccount,lcStore,lcOrderNo,lcPikTkt,lcWareCode,lnShipAmt,llCod,;
    llUpsInsur,lnDscPrcnt,lcTaxRate,lcPstRule,lnPstRate,lcUpsFile,;
    lcShipVia,lcCOD_AMT,lcFreight,lcInsur,lcCod,lcCartons,lcWeight,;
    lcTaxAmnt,lcPstAmnt,lnTaxDue,lcChrgType,lcZonesFile, lcRatesFile,lCCARTRCKNO
  *E302947,1 WAM 07/27/2011 (End)

  PRIVATE laShpViaFld,lcUpsType,lnCodCharge,lnInsCharge,lnFxdPrcnt,lcUpsIncr,;
    llCompTax,lcTaxRule,GETF,llUpsBox
  IF lnShipAmt = 0
    RETURN
  ENDIF
  *!* B612427,1 MMT 07/06/2021 Random Error While Invoicing SO while reading UPS charges[T20210603.0003][Start]
*!*    lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',oAriaApplication.ActiveCompanyID))
*!*    *E610617,1 TMI 12/09/2013 12:54 [Start] pad the variable lcFromZone to 3 characters
*!*    lcFromZone = PADR(lcFromZone,3)
*!*    *E610617,1 TMI 12/09/2013 12:54 [End  ]
*!*    IF gfGetMemVar('M_WareHouse',oAriaApplication.ActiveCompanyID)='Y'
*!*      =gfOpenFile(oAriaApplication.DataDir+'WAREHOUS',oAriaApplication.DataDir+'WAREHOUS','SH')
*!*      *-- get the from zone from the wharehouse if use multiple locations = y
*!*      lcFromZone = IIF(SEEK(lcWareCode,'warehous'),WareHous.UPS,lcFromZone)
*!*    ENDIF
*!*    *-- get the company setup information
*!*    llCompTax = (gfGetMemVar('M_TAX',oAriaApplication.ActiveCompanyID)='Y')
*!*    lcTaxRule = ALLTRIM(gfGetMemVar('M_TAX_METH',oAriaApplication.ActiveCompanyID))
*!*    llUpsBox  = (gfGetMemVar('M_UPSBOX',oAriaApplication.ActiveCompanyID)='Y')
  DECLARE laUpsChrgSetups[5,2]
  laUpsChrgSetups[1,1]  = 'XUPSFROM'       
  laUpsChrgSetups[2,1]  = 'M_WAREHOUSE'    
  laUpsChrgSetups[3,1]  = 'M_TAX'    
  laUpsChrgSetups[4,1]  = 'M_TAX_METH'  
  laUpsChrgSetups[5,1]  = 'M_UPSBOX'
  lcVarCompanyID = oAriaApplication.ActiveCompanyID
  =gfGetMemVar(@laUpsChrgSetups,lcVarCompanyID)
  
  
  lcFromZone = ALLTRIM(laUpsChrgSetups[1,2])
  lcFromZone = PADR(lcFromZone,3)
  IF laUpsChrgSetups[2,2]='Y'
    =gfOpenFile(oAriaApplication.DataDir+'WAREHOUS',oAriaApplication.DataDir+'WAREHOUS','SH')
    *-- get the from zone from the wharehouse if use multiple locations = y
    lcFromZone = IIF(SEEK(lcWareCode,'warehous'),WareHous.UPS,lcFromZone)
  ENDIF
  *-- get the company setup information
  llCompTax = (laUpsChrgSetups[3,2]='Y')
  lcTaxRule = ALLTRIM(laUpsChrgSetups[4,2])
  llUpsBox  = (laUpsChrgSetups[5,2]='Y')
  *!* B612427,1 MMT 07/06/2021 Random Error While Invoicing SO while reading UPS charges[T20210603.0003][End]
  
  *-- ShipVia related fields

  *E302947,1 WAM 07/27/2011 Retrieve new related fields
  *DECLARE laShpViaFld[4,2]
  DECLARE laShpViaFld[6,2]
  STORE '' TO lcCarrierId
  STORE .T. TO llIncFrght
  *E302947,1 WAM 07/27/2011 (End)

  laShpViaFld[1,1] = 'CUPS'
  laShpViaFld[1,2] = 'lcUpsType'    && Ups type
  laShpViaFld[2,1] = 'NCODCHARGE'
  laShpViaFld[2,2] = 'lnCodCharge'  && Cash on delivery charge
  laShpViaFld[3,1] = 'NINSCHARGE'
  laShpViaFld[3,2] = 'lnInsCharge'  && insurance Charges
  laShpViaFld[4,1] = 'NFXDPRCNT'
  laShpViaFld[4,2] = 'lnFxdPrcnt'

  *E302947,1 WAM 07/27/2011 Retrieve new related fields
  laShpViaFld[5,1] = 'CCARRIERID'  && Carrier ID
  laShpViaFld[5,2] = 'lcCarrierId'
  laShpViaFld[6,1] = 'LINCFRGHT'  && "Include Freight charges Yes/No"
  laShpViaFld[6,2] = 'llIncFrght'
  *E302947,1 WAM 07/27/2011 (End)

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
  *!*	B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [Start]
  lcUpsType=lfSTCNVRT(ALLTRIM(lcUpsType))
  *!*	B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [End]
  *-- if no ups type so use Other
  lcUpsType = IIF(EMPTY(lcUpsType),'OTHER',PADR(lcUpsType,7))

  *B609505,1 TMI 01/18/2011 [Start] do not calculate chanrges for type USPS
  *IF ALLTRIM(lcUpsType) <> 'OTHER'

  *E302947,1 WAM 07/27/2011 Calculate UPS charges only if the ship via related field CARRIER ID is set to UPS and the related field ""Include Freight charges" is set to 'YES'
  *IF ALLTRIM(lcUpsType) <> 'OTHER' AND LEFT(ALLTRIM(lcUpsType),4) <> 'USPS'
  *!* B609676,1 MMT 09/27/2011 Fix bug of Error 'alias CARRIER_SHIPMENT_T is not found' at customers that are not using Carriers Intergration[START]
  llIncFrght = IIF(TYPE('llIncFrght') ='L',llIncFrght,.T.)
  *!* B609676,1 MMT 09/27/2011 Fix bug of Error 'alias CARRIER_SHIPMENT_T is not found' at customers that are not using Carriers Intergration[END]
  *!*	B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [Start]

  *!*	IF !lfCarrCharges(lcAccount, lcStore, lcOrderNo, lcPikTkt,lnShipAmt,llCod ,llUpsInsur,lnCodCharge,lcUpsFile, lcFreight, lcWeight, lcCartons, lCCARTRCKNO, lcCarrierId, llIncFrght) AND ;
  *!*	    ALLTRIM(lcCarrierId) ='UPS' AND llIncFrght
  *B611475,1 MHM  ISSUE #10: FREIGHT AND TRACKING NOT GETTING ADDED WHEN PROCESSING WHILE INVOICING
  *!*	            IF !lfCarrCharges(lcAccount, lcStore, lcOrderNo, lcPikTkt,lnShipAmt,llCod ,llUpsInsur,lnCodCharge,lcUpsFile, lcFreight, lcWeight, lcCartons, lCCARTRCKNO, lcCarrierId, llIncFrght) AND ;
  *!*	             llIncFrght
  IF !lfCarrCharges(lcAccount, lcStore, lcOrderNo, lcPikTkt,lnShipAmt,llCod ,llUpsInsur,lnCodCharge,lcUpsFile, lcFreight, lcWeight, lcCartons, lCCARTRCKNO, lcCarrierId, llIncFrght,&lcInsur) AND ;
      llIncFrght
    *B611475,1 MHM  ISSUE #10: FREIGHT AND TRACKING NOT GETTING ADDED WHEN PROCESSING WHILE INVOICING
    *!*	B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [Start]
    *E302947,1 WAM 07/27/2011 (End)

    *B609505,1 TMI 01/18/2011 [End  ]
    IF !USED(lcZonesFile) OR !SEEK(lcUpsType+lcFromZone,lcZonesFile)
      IF oAriaApplication.RemoteSystemData.Execute("Select * from syszones WHERE shiptype+shipfrom='"+;
          lcUpsType+lcFromZone + "' ORDER BY shiptype,shipfrom,shipto",'',lcZonesFile,"",oAriaApplication.SystemConnectionString,3,'',SET("Datasession")) <= 0
        RETURN .F.
      ENDIF
      SELECT (lcZonesFile)
      lnCurBuff = CURSORGETPROP("Buffering")
      =CURSORSETPROP("Buffering" ,3,lcZonesFile)
      INDEX ON shiptype+shipfrom+shipto TAG (lcZonesFile)
      =CURSORSETPROP("Buffering" ,lnCurBuff ,lcZonesFile)
    ENDIF
    IF !USED(lcRatesFile) OR !SEEK(LEFT(lcUpsType,6),lcRatesFile)
      IF oAriaApplication.RemoteSystemData.Execute("Select * from sysRates WHERE shiptype='"+;
          LEFT(lcUpsType,6) + "' ORDER BY shiptype,zone,cshpweight",'',lcRatesFile,"",oAriaApplication.SystemConnectionString,3,'',SET("Datasession")) <= 0
        RETURN .F.
      ENDIF
      SELECT (lcRatesFile)
      lnCurBuff = CURSORGETPROP("Buffering")
      =CURSORSETPROP("Buffering" ,3,lcRatesFile)
      INDEX ON shiptype+Zone+cshpweight TAG (lcRatesFile)
      =CURSORSETPROP("Buffering" ,lnCurBuff ,lcRatesFile)
    ENDIF

    *-- Calculate freights
    =lfGetFright(lcFromZone,SUBSTR(Customer.cAddress5,1,3),lcUpsType  ,;
      llCod,llUpsInsur,lcFreight,lcInsur,lcCod,lcCartons,lcWeight,;
      lnCodCharge,lnInsCharge,lnShipAmt,lcChrgType,lcZonesFile, lcRatesFile)

*B611827,1  Es 03/22/2020 Random error while creating invoice sales order, Alias not found. [T20200226.0007] [Start]
*!*	 IF (llUpsBox .AND. SUBSTR(lcUpsType,1,5)='USUPS' AND !EMPTY(lcUpsFile))
     IF (llUpsBox .AND. SUBSTR(lcUpsType,1,5)='USUPS' AND !EMPTY(lcUpsFile) AND USED(lcUpsFile))
*B611827,1  Es 03/22/2020 Random error while creating invoice sales order, Alias not found. [T20200226.0007] [End]

      *-- Add records for each carton in the UPSBox file
      =lfUpsBox(lcAccount,lcStore,lcOrderNo,lcPikTkt,llCod,llUpsInsur,;
        &lcFreight,&lcInsur,&lcCod,&lcCartons,&lcWeight,lnShipAmt,lcUpsFile)

      *!* B609168,1 HES 03/14/2010 NEW UPS RATES FOR SYSTEM dbf [Start]
      SELECT (lcUpsFile)
      SUM FREIGHT TO m.FREIGHT
      LOCATE
      *!* B609168,1 HES 03/14/2010 NEW UPS RATES FOR SYSTEM dbf [End]

      IF llCod
        *-- calculate the cash on delivery amount
        &lcCOD_AMT = lnShipAmt+&lcFreight+&lcInsur+&lcCod-lnShipAmt*lnDscPrcnt/100
      ELSE
        &lcCOD_AMT = 0
      ENDIF
      *-- ladata[1] = Invoice Number
      IF &lcCartons=1 .AND. SEEK(lcOrderNo+lcStore+lcPikTkt,lcUpsFile)
        *-- Update TotCod Field with the total amount because there is only one carton
        REPLACE TotalCod WITH &lcCOD_AMT IN (lcUpsFile)
      ENDIF
    ELSE
      *-- any other ups type
      IF llCod
        &lcCOD_AMT = lnShipAmt+&lcFreight+&lcInsur+&lcCod-lnDscPrcnt*lnShipAmt/100
      ENDIF
    ENDIF
    &lcFreight = ROUND(&lcFreight*(100+lcUpsIncr)/100,2)
  ELSE
    *-- UPS TYPE =OTHER
    IF llCod
      &lcCOD_AMT = lnShipAmt-lnShipAmt*lnDscPrcnt/100
    ENDIF
    IF lnFxdPrcnt > 0  
      &lcFreight = lnShipAmt*lnFxdPrcnt/100
    ENDIF
    *B611487,1 AHH 12/12/2017 Insurance and Freight Disappear After Processing [T20171114.0031] [Start]
    IF INLIST(lcChrgType,'A','I')
      *-- Recalculate the insurance
      &lcInsur = IIF(llUpsInsur,IIF(('USUPS' $ lcUpsType OR INLIST(ALLTRIM(lcCarrierId),'UPS','FEDEX')) AND lnShipAmt>=100,;
        FLOOR(lnShipAmt/100)*lnInsCharge,0),0)
    ENDIF
    *B611487,1 AHH 12/12/2017 Insurance and Freight Disappear After Processing [T20171114.0031] [End]
  ENDIF
  IF llCompTax
    *-- CALCULATE TAXES
    llIsCanada = IIF(UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='CANADA',.T.,.F.)
    &lcTaxAmnt = ROUND(lcTaxRate*(IIF(lcTaxRule='M',lnTaxDue,lnTaxDue+;
      &lcFreight+&lcInsur+&lcCod)-ABS(lnTaxDue*lnDscPrcnt/100) )/100,2)
    *-- Calculate pst by the canadian method or the american one
    &lcPstAmnt = IIF(llIsCanada,(lnShipAmt+&lcFreight+&lcInsur+&lcCod-;
      ABS(lnShipAmt*lnDscPrcnt/100)+;
      IIF(VAL(lcPstRule)=1,0,&lcTaxAmnt))*lnPstRate/100,0)
    IF llCod
      *-- add the tax amount and the  pst amount to the cod amount
      &lcCOD_AMT = &lcCOD_AMT+&lcTaxAmnt+&lcPstAmnt

      *-- laData[1] =invoice ,laData[5]=Store
     
*B611827,1  Es 03/22/2020 Random error while creating invoice sales order, Alias not found. [T20200226.0007] [Start]
*!*	 IF llUpsBox .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. &lcCartons=1 ;
*!*	          .AND. !EMPTY(lcUpsFile) AND SEEK(lcOrderNo+lcStore+lcPikTkt,lcUpsFile)
           IF llUpsBox .AND. SUBSTR(lcUpsType,1,5)='USUPS' .AND. &lcCartons=1 ;
          .AND. !EMPTY(lcUpsFile) AND USED(lcUpsFile)  AND SEEK(lcOrderNo+lcStore+lcPikTkt,lcUpsFile) 
 *B611827,1  Es 03/22/2020 Random error while creating invoice sales order, Alias not found. [T20200226.0007] [End]

        *-- if there is no more than one carton so total amount = cod amount
        REPLACE TotalCod WITH &lcCOD_AMT IN (lcUpsFile)
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
  PARAMETERS lcAccount,lcStore,lcOrderNo,lcPikTkt,llCod,llUpsInsur,lnFreight,;
    lnInsure,lnCodAmt,lnCartons,lnWeight,lnTotShpAmt,lcUpsFile
  lnAlias = SELECT()
  lnCartons=ROUND(lnCartons,0)

  *--  Get the no of carton from back_hdr incase packed pikTkt.
  IF !EMPTY(lcPikTkt) AND SEEK(lcPikTkt,"Pack_Lin")
    PRIVATE lnCurPLTag,lnCartNo,lnCrtWgh
    lnCurPLTag = ORDER('Pack_Lin')  && save the Pick line order tag
    SET ORDER TO Packstyle IN Pack_Lin
    SELECT (lcUpsFile)

    *!* B609269,1 HES 05/25/2010 CARTONS Button displays data not related to the selecte PACK line! [Start]
    *!*	  DELETE ALL
    =SEEK(lcOrderNo+lcStore+lcPikTkt)
    DELETE REST WHILE ORDER+STORE+PikTkt+STR(CARTONS,5)= lcOrderNo+lcStore+lcPikTkt
    *!* B609269,1 HES 05/25/2010 CARTONS Button displays data not related to the selecte PACK line! [End  ]

    *!* B609168,1 HES 03/14/2010 NEW UPS RATES FOR SYSTEM dbf [Start]
    PRIVATE M.CARTONS,m.Weight,lnFreight
    *!* B609168,1 HES 03/14/2010 NEW UPS RATES FOR SYSTEM dbf [End]

    SELECT Pack_Lin
    DO WHILE Pack_No+STR(No_Cart,4)+STYLE = lcPikTkt
      lnCartNo = No_Cart
      lnCrtWgh = 0
      SCAN REST WHILE Pack_No+STR(No_Cart,4)+STYLE = lcPikTkt+STR(lnCartNo,4)
        lnCrtWgh = lnCrtWgh + Pack_Lin.Weight
      ENDSCAN

      *!* B609168,1 HES 03/14/2010 NEW UPS RATES FOR SYSTEM dbf [Start]
      m.CARTONS = 1
      m.Weight = lnCrtWgh
      lnFreight = 0
      =lfGetFright(lcFromZone,SUBSTR(Customer.cAddress5,1,3),lcUpsType  ,;
        llCod,llUpsInsur,'lnFreight',lcInsur,lcCod,lcCartons,lcWeight,;
        lnCodCharge,lnInsCharge,lnShipAmt,lcChrgType,lcZonesFile, lcRatesFile)
      *!* B609168,1 HES 03/14/2010 NEW UPS RATES FOR SYSTEM dbf [End]

      SELECT (lcUpsFile)
      APPEND BLANK

      *!* B609168,1 HES 03/14/2010 NEW UPS RATES FOR SYSTEM dbf [Start]
      *REPLACE Account    WITH lcAccount ,;
      Order      WITH lcOrderNo ,;
      PikTkt     WITH lcPikTKt  ,;
      Store      WITH lcStore   ,;
      Cartons    WITH lnCartNo  ,;
      Weight     WITH lnCrtWgh  ,;
      Decl_Value WITH IIF(llupsinsur,lnTotShpAmt/lnCartons,0),;
      Freight    WITH lnFreight/lnCartons ,;
      Insur      WITH IIF(llupsinsur,lnInsure/lnCartons,0)   ,;
      Cod        WITH IIF(llCod,lnCodAmt/lnCartons,0)      ,;
      TotalCod   WITH Decl_Value+Freight+Insur+Cod ,;
      InsurYn    WITH IIF(llupsinsur,'Y','N') ,;
      CodYn      WITH IIF(llCod,'Y','N')

      REPLACE Account    WITH lcAccount ,;
        ORDER      WITH lcOrderNo ,;
        PikTkt     WITH lcPikTkt  ,;
        STORE      WITH lcStore   ,;
        CARTONS    WITH lnCartNo  ,;
        Weight     WITH lnCrtWgh  ,;
        Decl_Value WITH IIF(llUpsInsur,lnTotShpAmt/lnCartons,0),;
        FREIGHT    WITH lnFreight ,;
        INSUR      WITH IIF(llUpsInsur,lnInsure/lnCartons,0)   ,;
        COD        WITH IIF(llCod,lnCodAmt/lnCartons,0)      ,;
        TotalCod   WITH Decl_Value+FREIGHT+INSUR+COD ,;
        InsurYn    WITH IIF(llUpsInsur,'Y','N') ,;
        CodYn      WITH IIF(llCod,'Y','N')
      *!* B609168,1 HES 03/14/2010 NEW UPS RATES FOR SYSTEM dbf [End]

      SELECT Pack_Lin
    ENDDO
    SET ORDER TO (lnCurPLTag) IN Pack_Lin  && Restore the old tag
  ELSE
    *-- if there is no pick ticket
    SELECT (lcUpsFile)
    =SEEK(lcOrderNo+lcStore+lcPikTkt+STR(lnCartons+1,5))
    DELETE REST WHILE ORDER+STORE+PikTkt+STR(CARTONS,5)= lcOrderNo+lcStore+lcPikTkt
    FOR lnCount = 1 TO lnCartons
      IF !SEEK(lcOrderNo+lcStore+lcPikTkt+STR(lnCount,5))
        *-- if not  found  in the upsfile then add a new line
        *-- else replace it with the new values
        INSERT INTO (lcUpsFile) (Account  , ORDER    , PikTkt  ,  STORE  , CARTONS) VALUES ;
          (lcAccount, lcOrderNo, lcPikTkt, lcStore , lnCount)
      ENDIF
      REPLACE Weight     WITH lnWeight/lnCartons  ,;
        Decl_Value WITH IIF(llUpsInsur,lnTotShpAmt/lnCartons,0),;
        FREIGHT    WITH lnFreight/lnCartons ,;
        INSUR      WITH IIF(llUpsInsur,lnInsure/lnCartons,0)   ,;
        COD        WITH IIF(llCod,lnCodAmt/lnCartons,0)      ,;
        TotalCod   WITH Decl_Value+FREIGHT+INSUR+COD ,;
        InsurYn    WITH IIF(llUpsInsur,'Y','N') ,;
        CodYn      WITH IIF(llCod,'Y','N')
    ENDFOR
  ENDIF
  SELECT (lnAlias)
  RETURN

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

  lnAlias = SELECT()
  DO FORM (oAriaApplication.ScreenHome+"AR\ARChrg") WITH ;
    lcChrgFile,lcInvoice,lcAccount,lcOrderNo,lcStore,lcPikTkt,lnTrdDsc

  IF EMPTY(lcInvoice)
    SELECT (lcChrgFile)
    =SEEK(lcOrderNo+lcStore+lcPikTkt)
    *!* B610989,1 MMT 04/20/2015 Fix the issue of wrong VAT calculations in invoice screen[T20150319.0010][Start]
    *!*	  SUM REST nChrgAmnt,nChrgAmnt*(1-lnTrdDsc/100)*nTaxRate/100 TO lnTotChrg,lnTotTax ;
    *!*	    WHILE ORDER+cStore+PikTkt+cchrgcode = lcOrderNo+lcStore+lcPikTkt
    SUM REST nChrgAmnt,nChrgAmnt*nTaxRate/100 TO lnTotChrg,lnTotTax ;
      WHILE ORDER+cStore+PikTkt+cchrgcode = lcOrderNo+lcStore+lcPikTkt
    *!* B610989,1 MMT 04/20/2015 Fix the issue of wrong VAT calculations in invoice screen[T20150319.0010][End]
    &lcTotChrg = lnTotChrg
    &lcTotTax  = lnTotTax
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
  *! Modifications:
  *!*************************************************************
FUNCTION gfChkSavInv

  *T20060804.0022,MMT,08/30/2006 Convert UPS integration to Aria4xp[Start]
  PARAMETERS lcAccount,lcOrder,lcStore,lcPikTkt,lcInvFile,lcInstFile,;
    lcCrdtFile,lcUpsFile,lcSaveInv,lcDivision,llCustomerConsByDc,lcDistCtr,loFormSet
  *PARAMETERS lcAccount,lcOrder,lcStore,lcPikTKt,lcInvFile,lcInstFile,;
  lcCrdtFile,lcUpsFile,lcSaveInv,lcDivision,llCustomerConsByDc,lcDistCtr
  *T20060804.0022,MMT,08/30/2006 Convert UPS integration to Aria4xp[End]

  LOCAL lnInstAmnt,lnAppCrdt,lnAlias

  lnAlias = SELECT()
  &lcSaveInv = .T.     && if all the next checks is valid Save the invoice
  *HBG 08/29/2004 Seek by Distrpition center in case of ConsBy DC [Begin]
  *=SEEK(lcAccount+lcOrder+lcStore+lcPikTKt,lcInvFile)

  *!B610282,1 HIA 03/24/2013 Aria XP - Failing to create and save invoices in Aria 4XP, T20130305.0011[Start]
  lcDivision = IIF(TYPE('lcDivision') ='C',lcDivision,'')
  *!B610282,1 HIA 03/24/2013 Aria XP - Failing to create and save invoices in Aria 4XP, T20130305.0011[End]

  IF llCustomerConsByDc
    SET ORDER TO ConsDist
    *!* B610259,1 MMT 02/26/2013 Invoice SO saving hanges if more than consolidated invoice for different divisions[Start]
    *=SEEK(lcAccount+lcDistCtr+lcOrder+lcStore+lcPikTKt,lcInvFile)
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
    *=SEEK(lcAccount+lcDistCtr+lcOrder+lcStore+lcPikTKt+lcDivision,lcInvFile)
    =SEEK(lcAccount+lcDistCtr+lcOrder+lcStore+lcPikTkt+IIF(TYPE('lcDivision')='C',lcDivision,''),lcInvFile)
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[end]
    *!* B610259,1 MMT 02/26/2013 Invoice SO saving hanges if more than consolidated invoice for different divisions[End]
  ELSE
    SET ORDER TO (lcInvFile)
    *!* B610259,1 MMT 02/26/2013 Invoice SO saving hanges if more than consolidated invoice for different divisions[Start]
    *=SEEK(lcAccount+lcOrder+lcStore+lcPikTKt,lcInvFile)
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
    * =SEEK(lcAccount+lcOrder+lcStore+lcPikTKt+lcDivision,lcInvFile)
    =SEEK(lcAccount+lcOrder+lcStore+lcPikTkt+IIF(TYPE('lcDivision')='C',lcDivision,''),lcInvFile)
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[end]
    *!* B610259,1 MMT 02/26/2013 Invoice SO saving hanges if more than consolidated invoice for different divisions[End]
  ENDIF
  *HBG [End]
  IF &lcInvFile..Ship = 0
    *-- Message : 40033
    *-- No invoice lines found! Cannot save.
    *-- Button : 00000
    *-- Ok
    =gfModalGen('INM40033B00000','ALERT')
    &lcSaveInv = .F.
  ENDIF
  IF &lcSaveInv AND !EMPTY(lcInstFile) AND SEEK(lcOrder+lcStore+lcPikTkt,lcInstFile)
    *HBG 08/29/2004 Applay the check of the installment if the installment <> the total charge not only grater than [Begin]
    *IF &lcInstFile..nInstmAmnt > &lcInvFile..TotalChg
    IF (&lcInstFile..nInstIAmnt+&lcInstFile..nInstmAmnt) <> &lcInvFile..TOTALCHG
      *HBG [End]
      *-- Message : 40123
      *-- Installments amount cannot be greater than invoice total charge!
      *-- Button : 00000
      *-- Ok
      *HBG 08/29/2004 cahnge the message to applay the check of the installment if the
      *HBG            installment <> the total charge not only grater than [Begin]
      *=gfModalGen('TRM40123B00000','ALERT')
      =gfModalGen('TRM40193B00000','ALERT')
      *HBG [End]
      &lcSaveInv = .F.
    ENDIF
    IF &lcInstFile..nInstmAmnt < &lcInvFile..TOTALCHG
      REPLACE nInstIAmnt WITH &lcInvFile..TOTALCHG - &lcInstFile..nInstmAmnt IN (lcInstFile)
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
  IF &lcSaveInv AND &lcInvFile..Cod_Flag = 'Y'
    llHasCredits = .F.
    =gfAcOpCrdt(lcAccount,&lcInvFile..cCurrCode, 'llHasCredits')
    IF llHasCredits
      IF EMPTY(lcCrdtFile) OR !SEEK(lcAccount+lcOrder+lcStore+lcPikTkt,lcCrdtFile)
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
        SUM REST AMOUNT TO lnAppCrdt ;
          WHILE Account+ORDER+STORE+PikTkt = lcAccount+lcOrder+lcStore+lcPikTkt
        IF lnAppCrdt > &lcInvFile..TOTALCHG
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
  *-- Calculate non english charges for BEL05.
  IF !EMPTY(lcOrder) AND ASCAN(oAriaApplication.laEvntTrig , PADR('CHKINV',10)) <> 0
    =gfDoTriger('ARIINV',PADR('CHKINV',10))
    RETURN
  ENDIF
  *-- Save artons in artclpshp for BEL05.

  *T20060804.0022,MMT,08/30/2006 Convert UPS integration to Aria4xp[Start]
  *!*	IF !EMPTY(lcOrder) AND ASCAN(oAriaApplication.laEvntTrig , PADR('CHKIT',10)) <> 0
  *!*	  =gfDoTriger('ARIINV',PADR('CHKIT',10))
  *!*	  RETURN
  *!*	ENDIF
  IF !EMPTY(lcOrder) AND ASCAN(loFormSet.laEvntTrig,PADR("CHKIT",10)) <> 0
    =loFormSet.mDoTrigger(PADR("CHKIT",10))
    RETURN
  ENDIF
  *T20060804.0022,MMT,08/30/2006 Convert UPS integration to Aria4xp[End]

  *-- Add a triger functions for the custome charges screen.[START]
  IF ASCAN(oAriaApplication.laEvntTrig , PADR('LASTSCRN',10)) <> 0
    =gfDoTriger('ARIINV',PADR('LASTSCRN',10))
  ENDIF
  *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[Start]
  *!*  IF &lcSaveInv AND SEEK('O'+Order,'ORDHDR') .AND. OrdHdr.ApprAmt > 0 .AND. ;
  *!*     &lcInvFile..ShipAmt > OrdHdr.ApprAmt
  IF &lcSaveInv AND SEEK('O'+ORDER,'ORDHDR','ORDHDR') .AND. ORDHDR.ApprAmt > 0 .AND. ;
      &lcInvFile..ShipAmt > ORDHDR.ApprAmt
    *!* B609899,1 MMT 04/30/2012 Seek in Ordhdr with the ORDHDR order[END]
    *-- Message : 40119
    *-- Invoice Amount Exceeds Order Approved Amount By 999999.
    *-- Button : 40003
    *-- Proceed Cancel
    IF gfModalGen('TRM40119B40003','ALERT',ALLTRIM(STR(&lcInvFile..ShipAmt-ORDHDR.ApprAmt,9,2)))=2
      &lcSaveInv = .F.
    ENDIF
  ENDIF
  SELECT (lnAlias)

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
  PARAMETERS lcInvoice, lcAppCrdt
  PRIVATE lnAppCrdt,lnAlias
  lnAlias = SELECT()
  lcOldOrd = ORDER('INVHDR')
  SET ORDER TO InvHdr IN InvHdr
  =SEEK(lcInvoice,'INVHDR')
  SELECT (lcAppCrdt)
  =SEEK(InvHdr.Account+IIF(InvHdr.Direct_Inv,SPACE(6),InvHdr.ORDER)+InvHdr.STORE+InvHdr.PikTkt)
  SUM REST AMOUNT TO lnAppCrdt WHILE Account+ORDER+cStore+PikTkt=;
    InvHdr.Account+IIF(InvHdr.Direct_Inv,SPACE(6),InvHdr.ORDER)+InvHdr.STORE+InvHdr.PikTkt
  IF ABS(lnAppCrdt) > 0
    SELECT DEBIT
    **
    =SEEK(InvHdr.Account+lcInvoice)
    lnTotCredit = ABS(lnAppCrdt)
    SCAN REST WHILE lnTotCredit > 0 AND Account+TRAN+cinstalno+DTOS(TRANDATE) = InvHdr.Account+lcInvoice
      SCATTER MEMVAR
      m.nTrnNewAmn = m.AMOUNT - lnTotCredit
      m.cShToOpn   = IIF(m.nTrnNewAmn>0,'N','Y')
      m.cStore     = InvHdr.STORE
      INSERT INTO (lcAppCrdt) FROM MEMVAR
      lnTotCredit = lnTotCredit - m.AMOUNT
    ENDSCAN
    **
    *  =SEEK(InvHdr.Account+lcInvoice+SPACE(3)+DTOS(InvHdr.InvDate))
    *  SCATTER MEMVAR
    *  m.nTrnNewAmn = m.Amount - ABS(lnAppCrdt)
    *  m.cShToOpn   = IIF(m.nTrnNewAmn>0,'N','Y')
    *  m.cStore     = InvHdr.Store
    *  INSERT INTO (lcAppCrdt) FROM MEMVAR

    *-- Call KEYOFF procedure
    *HBG 09/09/2004 Fix bug of wrong updating in ArHist [Begin]
    =SEEK(InvHdr.Account+lcInvoice)
    *HBG [End]
    oKeyOff = NEWOBJECT("ARKEYOFF",oAriaApplication.classdir+"AR.VCX")
    oKeyOff.mKeyOff(InvHdr.Account,InvHdr.InvDate,DEBIT.AMOUNT,lnAppCrdt,lcAppCrdt)
    RELEASE oKeyOff
  ENDIF
  SET ORDER TO (lcOldOrd) IN InvHdr
  SELECT (lnAlias)

  *!*************************************************************
  *! Name      : lfGtOrder
  *! Developer : Wael Ali Mohamed
  *! Date      : 08/01/2002
  *! Purpose   : Get Order number manullay
  *!*************************************************************
  *! Calls     : Form ARORDER
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfCanReason()
  *!*************************************************************
FUNCTION lfGtOrder
  LOCAL lcOrderNo
  lcOrderNo = ''
  DO FORM (oAriaApplication.ScreenHome+"ARORDER") WITH 'O','O' TO lcOrderNo
  RETURN lcOrderNo

  *:* E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[Start]
  *!*************************************************************
  *! Name      : lfChkStatHst
  *! Developer : Mariam Mazhar
  *! Date      : 06/17/2009
  *! Purpose   : Check if State is HST Tax
  *!*************************************************************
FUNCTION lfChkStatHst
  PARAMETERS lcCustState
  DIMENSION laTaxHstRl[1,2]
  laTaxHstRl[1,1] = "LHSTTAX"
  laTaxHstRl[1,2] = "llISHSt"
  STORE .F. TO llISHSt
  *-- Fill the related GL information from the codes file.
  llNoThing = gfRltFld(lcCustState, @laTaxHstRl, "STATE")
  RETURN llISHSt
  *:* E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[End]

  *!* B609102,1 HES 12/03/2009 Fix bug of Invoice due date not calculated correctly when terms EOM set to Yes [Start]
  *!*************************************************************
  *! Name      : lfgetDueDate
  *! Developer : Hesham Elmasry
  *! Date      : 06/17/2009
  *! Purpose   : Calculate the due date for invoice
  *!*************************************************************
FUNCTION lfgetDueDate
  PARAMETERS ldShpDate, lcpTEOM, lnpEOMDay, lnpTDaysDue

  lnInvD = DAY  (ldShpDate)
  lnInvM = MONTH(ldShpDate)
  lnInvY = YEAR (ldShpDate)

  *B609102,3 TMI 02/17/2010 04:08:18 PM [Start] take into account the case of Feb when it be 28 days and the EOM = 30
  IF lnInvM = 2 AND INLIST(lnpEOMDay,30,29)
    DO WHILE EMPTY(DATE(lnInvY,lnInvM,lnpEOMDay))
      lnpEOMDay = lnpEOMDay - 1
    ENDDO
  ENDIF
  *B609102,3 TMI 02/17/2010 04:08:23 PM [End  ] take into account the case of Feb when it be 28 days and the EOM = 30

  IF lcpTEOM = 'Y'
    DO CASE
      CASE lnInvD > lnpEOMDay AND lnpEOMDay <> 0
        lnInvD    = lnpEOMDay
        ldEMODate = DATE(lnInvY,lnInvM,lnInvD)
        ldDate    = GOMONTH(ldEMODate,1)
        ldMyDueDate = ldDate + lnpTDaysDue
      CASE lnInvD <= lnpEOMDay
        lnInvD = lnpEOMDay
        ldDate = DATE(lnInvY,lnInvM,lnInvD)
        ldMyDueDate = ldDate + lnpTDaysDue
      CASE lnpEOMDay = 0
        ldFDate = DATE(lnInvY,lnInvM,01)
        ldDate = GOMONTH(ldFDate,1)-1
        ldMyDueDate = ldDate + lnpTDaysDue
    ENDCASE
  ELSE
    ldMyDueDate = ldShpDate + lnpTDaysDue
  ENDIF
  * End of lfgetDueDate()
  *!* B609102,1 HES 12/03/2009 Fix bug of Invoice due date not calculated correctly when terms EOM set to Yes [End]


  *!*************************************************************
  *! Name      : lfCarrCharges (E302947)
  *! Developer : WAM
  *! Date      : 07/27/2011
  *! Purpose   : Get cartons charges and tracking#
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: lcAccount  : Invoice Account
  *!             lcStore    : Invoice Store
  *!             lcOrderNo  : Order Number
  *!             lcPikTKt   : PikTKt Number
  *!             lnShipAmt  : Invoice Ship Amount
  *!             llCod      : Invoice use COD
  *!             llupsinsur : Invoice use UPS insurance
  *!             lnCodCharge: COD Charges per carton
  *!             lcUpsFile  : UPS Temp File Name
  *!             lcFreight  : Freight Charge
  *!             lcWeight   : Invoice Weight
  *!             lcCartons  : Invoice Cartons
  *!             lCCARTRCKNO: Carrier Carton Tracking number
  *!             lcCarrierId: Carrier ID
  *!             llIncFrght : Include charges
  *!*************************************************************
  *! Returns            :  None
  *!*************************************************************
  *! Example            :  =lfvKeyOff()
  *!*************************************************************
FUNCTION lfCarrCharges
  *B611475,1 MHM  ISSUE #10: FREIGHT AND TRACKING NOT GETTING ADDED WHEN PROCESSING WHILE INVOICING
  *LPARAMETERS lcAccount, lcStore, lcOrderNo, lcPikTkt,lnShipAmt,llCod ,llUpsInsur,lnCodCharge,lcUpsFile, lcFreight, lcWeight, lcCartons, lCCARTRCKNO,lcCarrierId,llIncFrght
  LPARAMETERS lcAccount, lcStore, lcOrderNo, lcPikTkt,lnShipAmt,llCod ,llUpsInsur,lnCodCharge,lcUpsFile, lcFreight, lcWeight, lcCartons, lCCARTRCKNO,lcCarrierId,llIncFrght,lnInsure
  LOCAL lnAlias, lnInvFreight, lnInvWeight, lnCartons, llCarrCharges, llUpdUPSFile, lnCartNo
  *B611475,1 MHM  ISSUE #10: FREIGHT AND TRACKING NOT GETTING ADDED WHEN PROCESSING WHILE INVOICING
  STORE 0 TO lnInvFreight, lnInvWeight, lnCartons, lnCartNo
  llCarrCharges = .F.
  lnAlias = SELECT()

  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.Execute("SELECT CARRIER_ID FROM CARRIER_LOGIN_INFORMATION_T WHERE [CARRIER_ID]='"+lcCarrierId+"'" ,'',;
    "CARRIER_LOGIN_INFORMATION_T","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
  IF lnConnectionHandlar = 1
    SELECT CARRIER_LOGIN_INFORMATION_T
    GO TOP
    IF !EOF()
    
    
*B611614,1 Es 07/11/2018 Modify this issue -> "User cannot re-ship voided cartons that have voided tracking#" [Start]

     * lcSelectStat = "SELECT TRACKING_NO,CDECL_VAL,NFREIGHT,CCOD,CCOD_AMT,BILLING_WEIGHT FROM CARRIER_SHIPMENT_T WHERE [ORDER]='"+lcOrderNo+"' AND STORE = '"+lcStore+"' AND PICK_TICKET= '"+lcPikTkt+"'"
       lcSelectStat = "SELECT TRACKING_NO,CDECL_VAL,NFREIGHT,CCOD,CCOD_AMT,BILLING_WEIGHT FROM CARRIER_SHIPMENT_T WHERE [ORDER]='"+lcOrderNo+"' AND STORE = '"+lcStore+"' AND PICK_TICKET= '"+lcPikTkt+"' AND STATUS <> 'V' "  
*B611614,1 Es 07/11/2018 Modify this issue -> "User cannot re-ship voided cartons that have voided tracking#" [End]

      lnConnectionHandlar = oAriaApplication.RemoteCompanyData.Execute(lcSelectStat,'',;
        "CARRIER_SHIPMENT_T","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
      IF lnConnectionHandlar = 1
        llUpdUPSFile = llUpsBox AND !EMPTY(lcUpsFile) AND USED(lcUpsFile)
        SELECT CARRIER_SHIPMENT_T
        COUNT TO lnCartons
        SCAN
          llCarrCharges = .T.

          lnInvFreight= lnInvFreight + NFREIGHT
          lnInvWeight = lnInvWeight + BILLING_WEIGHT
          &lCCARTRCKNO = TRACKING_NO
          lnCartNo = lnCartNo + 1

          IF llUpdUPSFile
            SELECT (lcUpsFile)
            APPEND BLANK
            REPLACE Account    WITH lcAccount,;
              ORDER      WITH lcOrderNo,;
              PikTkt     WITH lcPikTkt,;
              STORE      WITH lcStore ,;
              CARTONS    WITH lnCartNo  ,;
              Weight     WITH CARRIER_SHIPMENT_T.BILLING_WEIGHT ,;
              FREIGHT    WITH CARRIER_SHIPMENT_T.NFREIGHT,;
              CCARTRCKNO WITH CARRIER_SHIPMENT_T.TRACKING_NO,;
              Decl_Value WITH IIF(llUpsInsur,lnShipAmt/lnCartons,0),;
              INSUR      WITH IIF(llUpsInsur,lnInsure/lnCartons,0)   ,;
              COD        WITH IIF(llCod,lnCodCharge,0)      ,;
              TotalCod   WITH Decl_Value+FREIGHT+INSUR+COD ,;
              InsurYn    WITH IIF(llUpsInsur,'Y','N') ,;
              CodYn      WITH IIF(llCod,'Y','N')
          ENDIF
        ENDSCAN
        *!* B609880,1 HIA 03/28/2012 update wght..., in case of llcarrchagrs [BEGIN]
        *IF llIncFrght
        IF llIncFrght AND llCarrCharges
          *!* B609880,1 HIA 03/28/2012 update wght..., in case of llcarrchagrs [END]
          &lcFreight = lnInvFreight
          &lcWeight  = lnInvWeight
          &lcCartons = lnCartons
        ENDIF
      ELSE
        lcResult = oAriaApplication.RemoteSystemData.CheckRetResult('Execute',lnConnectionHandlar,.F.)
      ENDIF
    ENDIF
  ELSE
    lcResult = oAriaApplication.RemoteSystemData.CheckRetResult('Execute',lnConnectionHandlar,.F.)
  ENDIF
  *!* B609676,1 MMT 09/27/2011 Fix bug of Error 'alias CARRIER_SHIPMENT_T is not found' at customers that are not using Carriers Intergration[Start]
  IF USED('CARRIER_LOGIN_INFORMATION_T')
    *!* B609676,1 MMT 09/27/2011 Fix bug of Error 'alias CARRIER_SHIPMENT_T is not found' at customers that are not using Carriers Intergration[END]
    USE IN CARRIER_LOGIN_INFORMATION_T
    *!* B609676,1 MMT 09/27/2011 Fix bug of Error 'alias CARRIER_SHIPMENT_T is not found' at customers that are not using Carriers Intergration[Start]
  ENDIF
  IF USED('CARRIER_SHIPMENT_T')
    *!* B609676,1 MMT 09/27/2011 Fix bug of Error 'alias CARRIER_SHIPMENT_T is not found' at customers that are not using Carriers Intergration[END]
    USE IN CARRIER_SHIPMENT_T
    *!* B609676,1 MMT 09/27/2011 Fix bug of Error 'alias CARRIER_SHIPMENT_T is not found' at customers that are not using Carriers Intergration[Start]
  ENDIF
  *!* B609676,1 MMT 09/27/2011 Fix bug of Error 'alias CARRIER_SHIPMENT_T is not found' at customers that are not using Carriers Intergration[END]
  SELECT (lnAlias)

  RETURN llCarrCharges

  *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[Start]
  *!*************************************************************
  *! Name      : lfReleasePickTicket
  *! Developer : MMT
  *! Date      : 07/11/2012
  *! Purpose   : Release Invoice related Pick ticket
  *!*************************************************************
FUNCTION lfReleasePickTicket
  LPARAMETERS lcOrderNumber,lcStoreNumber,lcPickTicket
  LOCAL lcOldSelect,lcOldOrder,lcKeyValue
  lcOldSelect = SELECT()
  SELECT ORDLINE
  lcOldOrder = ORDER()
  lcKeyValue = EVALUATE(KEY())
  SET ORDER TO ORDLINST   && CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
  =SEEK('O'+lcOrderNumber+lcStoreNumber)
  SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)= 'O'+lcOrderNumber+lcStoreNumber FOR PikTkt =lcPickTicket
    *SCATTER MEMO MEMVAR
    IF ORDLINE.Picked AND !EMPTY(ORDLINE.Dyelot) AND SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+ORDLINE.Dyelot,'StyDye')
      REPLACE Alo1   WITH Alo1   - ORDLINE.PIK1,;
        Alo2   WITH Alo2   - ORDLINE.PIK2,;
        Alo3   WITH Alo3   - ORDLINE.PIK3,;
        Alo4   WITH Alo4   - ORDLINE.PIK4,;
        Alo5   WITH Alo5   - ORDLINE.PIK5,;
        Alo6   WITH Alo6   - ORDLINE.PIK6,;
        Alo7   WITH Alo7   - ORDLINE.PIK7,;
        Alo8   WITH Alo8   - ORDLINE.PIK8,;
        TotAlo WITH TotAlo - ORDLINE.TotPIK IN StyDye
      UNLOCK IN StyDye
    ENDIF
    IF ORDLINE.Picked AND SEEK(ORDLINE.STYLE+ORDLINE.cWareCode+SPACE(10),'StyDye')
      =RLOCK('StyDye')
      *--decrease the Alocated quantitys in the stydye file
      *-- with the order line Picked Quantities
      REPLACE Alo1   WITH Alo1   - ORDLINE.PIK1,;
        Alo2   WITH Alo2   - ORDLINE.PIK2,;
        Alo3   WITH Alo3   - ORDLINE.PIK3,;
        Alo4   WITH Alo4   - ORDLINE.PIK4,;
        Alo5   WITH Alo5   - ORDLINE.PIK5,;
        Alo6   WITH Alo6   - ORDLINE.PIK6,;
        Alo7   WITH Alo7   - ORDLINE.PIK7,;
        Alo8   WITH Alo8   - ORDLINE.PIK8,;
        TotAlo WITH TotAlo - ORDLINE.TotPIK IN StyDye
      UNLOCK IN StyDye
    ENDIF
    IF ORDLINE.Picked AND SEEK(ORDLINE.STYLE,'Style')
      *--decrease the Alocated quantitys in the style file
      *-- with the order line Picked Quantities
      =RLOCK('Style')
      REPLACE Alo1   WITH Alo1   - ORDLINE.PIK1,;
        Alo2   WITH Alo2   - ORDLINE.PIK2,;
        Alo3   WITH Alo3   - ORDLINE.PIK3,;
        Alo4   WITH Alo4   - ORDLINE.PIK4,;
        Alo5   WITH Alo5   - ORDLINE.PIK5,;
        Alo6   WITH Alo6   - ORDLINE.PIK6,;
        Alo7   WITH Alo7   - ORDLINE.PIK7,;
        Alo8   WITH Alo8   - ORDLINE.PIK8,;
        TotAlo WITH TotAlo - ORDLINE.TotPIK IN STYLE
      UNLOCK IN STYLE
    ENDIF

    IF !SEEK(ORDLINE.PikTkt+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'PIKLINE')
      SELECT ORDLINE
      COPY NEXT 1 TO (oAriaApplication.WorkDir+lcTmpPikLn)
      SELECT PIKLINE
      APPEND FROM (oAriaApplication.WorkDir+lcTmpPikLn)
      ERASE (oAriaApplication.WorkDir+lcTmpPikLn+".DBF")
    ENDIF
    SELECT ORDLINE
    REPLACE Picked WITH .F.,;
      PIK1   WITH 0  ,;
      PIK2   WITH 0  ,;
      PIK3   WITH 0  ,;
      PIK4   WITH 0  ,;
      PIK5   WITH 0  ,;
      PIK6   WITH 0  ,;
      PIK7   WITH 0  ,;
      PIK8   WITH 0  ,;
      TotPIK WITH 0  ,;
      PikDate WITH {} ,;
      PikTkt WITH ''

  ENDSCAN
  IF SEEK(lcOrderNumber+lcPickTicket,'PikTkt','ORDPIK')
    =RLOCK('PikTkt')
    REPLACE STATUS WITH 'C' IN 'PikTkt'
    UNLOCK IN 'PikTkt'
  ENDIF
  SELECT ORDLINE
  IF !EMPTY(lcOldOrder)
    SET ORDER TO (lcOldOrder)
    =SEEK(lcKeyValue)
  ENDIF
  SELECT(lcOldSelect)
  *B609997,1 MMT 07/11/2012 Invoicing PL release the related Pick ticket while it still has open lines in Ordline[End]
  *!*	B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [Start]
  *!*************************************************************
  *! Name      : lfSTCNVRT
  *! Developer : AHH
  *! Date      : 07/11/2017
  *! Purpose   : convert the numeric service type to string one as below list:
  *! 			 : U.S.A. UPS 2nd Day Air        ---------- 02   >>> USUPS2
  *! 			 : U.S.A. UPS 3 Days Select      ---------- 12   >>> USUPS3
  *! 			 : U.S.A. UPS 2nd Day Air A.M.   ---------- 59 >>> USUPSE
  *! 			 : U.S.A. UPS Ground             ---------- 03  >>>  USUPSG
  *! 			 : U.S.A. UPS Next Day Air       ---------- 01 >>> USUPSN
  *! 			 : U.S.A. UPS Next Day Air Saver ---------- 13 >>> USUPST
  *!*************************************************************
FUNCTION lfSTCNVRT
  LPARAMETERS lcSrvType
  LOCAL lcServiceType
  lcServiceType =lcSrvType

  *! 			 : U.S.A. UPS Next Day Air       ---------- 01 >>> USUPSN
  *! 			 : U.S.A. UPS 2nd Day Air        ---------- 02 >>> USUPS2
  *! 			 : U.S.A. UPS Ground             ---------- 03 >>> USUPSG
  *! 			 : U.S.A. UPS 3 Days Select      ---------- 12 >>> USUPS3
  *! 			 : U.S.A. UPS Next Day Air Saver ---------- 13 >>> USUPST
  *! 			 : U.S.A. UPS 2nd Day Air A.M.   ---------- 59 >>> USUPSE
  DO CASE
    CASE lcSrvType = '01'
      lcServiceType = "USUPSN"
    CASE lcSrvType = '02'
      lcServiceType = "USUPS2"
    CASE lcSrvType = '03'
      lcServiceType = "USUPSG"
    CASE lcSrvType = '12'
      lcServiceType = "USUPS3"
    CASE lcSrvType = '13'
      lcServiceType = "USUPST"
    CASE lcSrvType = '59'
      lcServiceType = "USUPSE"

  ENDCASE

  RETURN lcServiceType
ENDFUNC
*!*	B611454,1 AHH 05/11/2017 the COD Charges do not automatically appear when we go to invoice those orders [T20171016.0045] [End]

