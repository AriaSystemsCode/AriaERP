*!**************************************************************************
*: Name      : CAMMAIN.PRG
*: Developer : ABDOU ELGENDI - (ABD) Due to C#102782,1
*: Date      : 12/30/2002
*: Purpose   : CAM05 Custom Process Program .
*:**************************************************************************
*: Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*:             lcFunPars -> Process function parameters, sent as a string.
*:**************************************************************************
*: Returns   : Logical value.       C102782,1
*:**************************************************************************
*: B606936,1 ASH 02/06/03 1- Display sequentiola number for the cartons field.
*: B606936,1              2- Update the status in ARUPSSHP.
*: B120421,1 ABD 12/10/2003 Fix bug that the total balance to pay show wrong numbers in
*: B120421,1                Case consolidated invoice for more than one store.
*: B122053,1 NNA 03/15/2004 fix bug 1 - that the UPS Tracking Numbers are missing the last digits
*: B122053,1 NNA                    2 - we can't add decimal value in any of the boxes when we 
*: B122053,1 NNA                   		add a carton manually (these bugs in Arcamups2.scx & cammain.prg)
*: E123512,1 NNA 07/11/2004 Update the INVHDR.cCodTrckNo with the ARUPSSHP.CTrack_No if we are invoicing
*: E123512,1 NNA            a Pick Ticket that has a shipment record in the ARUPSSHP file and increase
*: E123512,1 NNA            the length of INVHDR.cCodTrckNo to 30 Chr. to match with ARUPSSHP.CTrack_No
*: B123213,1 NNA 07/13/2004 Fix bug that the Shipment Details Displays all Records For a Piktkt even
*: B123213,1 NNA            there are Voided Records that has status ['Y'] . So We Must Show Only
*: B123213,1 NNA            the Final Ship. Info. for this PikTkt
*: C124347,1 NNA 09/02/2004 Add A Trigger to Add the Customer. nhand_charg (used Define field) Value 
*: C124347,1 NNA            to the Freight in the Invoice SO for customer SYU03
*: B128218,1 EIH 08/31/2005 Fix bug that the Shipment Details Displays wrong Records For a Piktkt 
*: B128218,1 EIH            in case of the piktkt generated with new tracking number.
*: B129997,1 NNA 10/13/2005 Fix bug that invoice takes alot of time while saving
*: B130704,1 NNA 12/13/2005 Fix bug that invoice takes alot of time while saving if the order has no piktkt
*: B607870,1 TMI 12/10/2006 Fix a bug that the freight of the cons. inv. is not the sum of that of indivedulal invoices ( Ticket # T20060830.0007 ) 
*: B608458,1 WAM 02/28/2008 Restore COD Tracking#, cartons, weight and freight from UPS while invoicing   (Ticket # T20071205.0013) 
*:**************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'
*--Run the function.
llRetValue = EVAL(lcFunToRun)
RETURN llRetValue
*:**************************************************************************
*: Name      : lfTMPUPS
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Update Charges.
*:**************************************************************************
*: Parameters: 
*:**************************************************************************
*: Returns   : 
*:**************************************************************************
FUNCTION lfTMPUPS

=lfTMPUPSS(llCalld,lcChrgTyp)

FUNCTION lfTMPUPSS
PARAMETERS llCalled,lcChrgType

*B607870,1 TMI [Start] be sure that the lcChrgType variable has a non empty value
*lcChrgType=IIF(TYPE('lcChrgType')='C',lcChrgType,'A')
lcChrgType=IIF(TYPE('lcChrgType')#'C'.OR.EMPTY(lcChrgType),'A',lcChrgType)
*B607870,1 TMI [End  ] 

WAIT 'Computing Invoice Charges...' WINDOW NOWAIT
SELECT (lcInvHdr)
SET ORDER TO TAG CONSOL
lcKey = Account+cDivision+cCurrCode
IF Consol='Y'
  *B607870,1 TMI [Start] 
  *- Assign Zero to the following fields to recalculate them again.
  llConsold = .T.
  IF llFirstTim 
    llFirstTim = .F.
    IF SEEK('Y'+lcKey)
      =RLOCK()
      REPLACE Freight    WITH 0,;
              INSUR      WITH 0,;
              COD        WITH 0,;
              CARTONS    WITH 0,;
              WEIGHT     WITH 0,;
              nPstAmt    WITH 0,;
              Cod_amt    WITH 0,;
              TotalChg   WITH 0,;
              nHstAmt    WITH 0,;
              Discount   WITH 0,;
              DiscPcnt   WITH 0,;
              Weight     WITH 0,;
              Cartons    WITH 0,;
              nMerchTax  WITH 0,;
              Tax_Amt    WITH 0,;
              TAX_RATE   WITH 0,;
              nTaxDue    WITH 0,;
              ShipAmt    WITH 0
      UNLOCK
    ENDIF
  ENDIF
  *B607870,1 TMI [End  ] 

  =SEEK('N'+lcKey)
  *B607870,1 TMI [Start] Scan only for selected recored not all recored. 
  *SCAN REST WHILE Consol+Account+cDivision+cCurrCode='N'+lcKey
  SCAN REST WHILE Consol+Account+cDivision+cCurrCode='N'+lcKey FOR cSelect = '»'
    *B607870,1 TMI [End  ] 
    SCATTER FIELDS &lcScFields TO laData
    
    *B607870,1 TMI [Start] comment this scatter command and add more fields
    *SCATTER MEMVAR FIELDS SHIPAMT,DISCPCNT,TAX_RATE,cTaxRule,nPstRate,nHstRate,;
    *ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,nPstAmt,nHstAmt,TOTALCHG
    *=lfvUpsChrg(.T.,lcChrgType)
    
    *- Always check for Store.
    laData[5] = IIF(&lcInvHdr..Consol='Y' AND !EMPTY(&lcInvHdr..cConStore),&lcInvHdr..cConStore,&lcInvHdr..Store)
    
    SCATTER MEMVAR FIELDS SHIPAMT,DISCPCNT,TAX_RATE,cTaxRule,nPstRate,nHstRate,;
    ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,nPstAmt,nHstAmt  ,;
    TOTALCHG,Discount,nMerchTax,Trde_Disc,TAX_AMT,nTaxDue

    =lfTMPUPSS(.T.,lcChrgType)
    *B607870,1 TMI [End  ] 
  ENDSCAN
  =SEEK('Y'+lcKey)
  =RLOCK()

  *B607870,1 TMI [Start] Update the Discount Prcentage.
  llConsold = .F.
  REPLACE DiscPcnt WITH IIF(ShipAmt=0,0,ABS(Discount)*100/ShipAmt)
  *B607870,1 TMI [End  ] 

  lnTaxAmount = IIF(UPPER(ALLTRIM(gcContCode))='USA',nTaxDue,ShipAmt)
  lnGstChrg = IIF(laSetups[11,2]='M',lnTaxAmount,;
                  lnTaxAmount+Freight+Insur+COD)-DiscPcnt*lnTaxAmount/100
  lnPstChrg = ShipAmt+Freight+Insur+COD+Discount+IIF(VAL(m.cTaxRule)=1,0,Tax_Amt)
  REPLACE lCompUps WITH .F.        ,;
          Cod_Flag WITH m.Cod_Flag ,;
          lUpsIns  WITH m.lUpsIns  ,;
          Tax_Rate WITH IIF(lnGstChrg=0,0,Tax_Amt*100/lnGstChrg) ,;
          nPstRate WITH IIF(lnPstChrg=0,0,nPstAmt*100/lnPstChrg)
  REPLACE nHstRate WITH IIF(lnPstChrg=0,0,nHstAmt*100/lnPstChrg)
  
  *B120421,1 ABD - Fix bug that the total balance to pay show wrong numbers in
  *B120421,1 ABD - Case consolidated invoice for more than one store,
  *B120421,1 ABD - Recalculate the total balance to pay again after saving the record. [Begin]
  REPLACE TotalChg  WITH ShipAmt+Freight+Insur+COD+Discount+Tax_Amt+nPstAmt+IIF(lnPstRule=2,m.nHstAmt,0) 
  *B120421,1 ABD - [End]
  
  UNLOCK
  SCATTER MEMVAR FIELDS &lcFlFields
  SCATTER FIELDS &lcScFields TO laData

  *B607870,1 TMI [Start] 
  *- Always check for Store
  laData[5] = IIF(&lcInvHdr..Consol='Y' AND !EMPTY(&lcInvHdr..cConStore),&lcInvHdr..cConStore,&lcInvHdr..Store)
  
  *- Call function to calculate the ship ammount and Qty.  [Begin]
  llFirstTim = .T.
  =lfReClcShp(.T.)
  M.ShipAmt = ShipAmt
  *B607870,1 TMI [End  ] 

ELSE

  *B607870,1 TMI [Start] define m.nTaxDue and m.Tax_rate variables
  M.nTaxDue = nTaxDue
  m.TAX_RATE = EVAL(LCINVHDR + '.NTRUTAXRAT')
  *B607870,1 TMI [End  ] 

  DO lpUpsChrg WITH ;
  laData[4],laData[5],laData[1],ladata[3],laData[7],m.SHIPAMT,m.Cod_Flag='Y',;
  m.lUpsIns,m.DISCPCNT,m.TAX_RATE,m.cTaxRule,m.nPstRate,lcUpsTmp,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT','m.TAX_AMT',;
  'm.nPstAmt',IIF(UPPER(ALLTRIM(gcContCode))='USA',&lcInvHdr..nTaxDue,;
  m.SHIPAMT),lcChrgType
  
  m.nHstAmt  = IIF(llIsCanada,(ShipAmt+m.COD+m.Insur+m.Freight-;
                   M.ShipAmt*m.DiscPcnt/100+IIF(VAL(cTaxRule)=1,0,m.Tax_Amt))*nHstRate/100,0)
  *B607870,1 TMI [Start] no need for this line , it is repeated 
  *m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod+m.Discount+m.Tax_Amt+m.nPstAmt+m.nHstAmt
  *B607870,1 TMI [End  ] 
  m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod+m.Discount+m.Tax_Amt+m.nPstAmt+IIF(lnPstRule=2,m.nHstAmt,0) 
  SELECT (lcInvHdr)
  lnFreight  = Freight
  lnInsur    = INSUR
  lnCod      = COD
  lnOldCart  = CARTONS
  lnWeight   = WEIGHT
  lnTaxAmnt  = TAX_AMT
  lnPstAmt   = nPstAmt
  lnHstAmt   = nHstAmt
  lnCodAmt   = Cod_amt
  lnTotalChg = TotalChg
  lnRecNo = RECNO()
  IF SEEK('Y'+lcKey)
    =RLOCK()
    *B607870,1 TMI [Start] check if the invoice is not consolidated
    IF !llConsold
      *B607870,1 TMI [End  ] 
      REPLACE Freight  WITH Freight - lnFreight + m.Freight ,;
              INSUR    WITH INSUR   - lnInsur   + m.INSUR   ,;
              COD      WITH COD     - lnCod     + m.COD     ,;
              CARTONS  WITH CARTONS - lnOldCart + m.CARTONS ,;
              WEIGHT   WITH WEIGHT  - lnWeight  + m.WEIGHT  ,;
              TAX_AMT  WITH TAX_AMT - lnTaxAmnt + m.TAX_AMT ,;
              nPstAmt  WITH nPstAmt - lnPstAmt  + m.nPstAmt ,;
              Cod_amt  WITH Cod_Amt - lnCodAmt  + m.Cod_amt ,;
              TotalChg WITH TotalChg- lnTotalChg+ m.TotalChg
      REPLACE nHstAmt WITH nHstAmt - lnHstAmt  + m.nHstAmt
      *B607870,1 TMI [Start] case of consolidated invoice
    ELSE
      REPLACE Freight   WITH Freight  + m.Freight ,;
              INSUR     WITH INSUR    + m.INSUR   ,;
              COD       WITH COD      + m.COD     ,;
              CARTONS   WITH CARTONS  + m.CARTONS ,;
              WEIGHT    WITH WEIGHT   + m.WEIGHT  ,;
              TAX_AMT   WITH TAX_AMT  + m.TAX_AMT ,;
              nPstAmt   WITH nPstAmt  + m.nPstAmt ,;
              Cod_amt   WITH Cod_Amt  + m.Cod_amt ,;
              TotalChg  WITH TotalChg + m.TotalChg,;              
              nHstAmt   WITH  nHstAmt + m.nHstAmt ,;
              Discount  WITH Discount - m.ShipAmt*m.DiscPcnt/100,;
              DiscPcnt  WITH IIF(ShipAmt=0,0,ABS(Discount)*100/ShipAmt) ,;
              nMerchTax WITH nMerchTax + m.nMerchTax ,;
              nTaxDue   WITH nTaxDue   + M.nTaxDue   ,;
              ShipAmt   WITH ShipAmt   + M.ShipAmt
      *- Calculate discount
      IF CONSOL='Y'
        lnCurRec = RECNO()
        LOCATE
        STORE 0 TO lnDiscount
        SUM ALL DISCOUNT TO lnDiscount FOR CSELECT='»' AND CONSOL<>'Y'
        GOTO lnCurRec
        REPLACE Discount  WITH lnDiscount  , DiscPcnt WITH IIF(ShipAmt=0,0,ABS(Discount)*100/ShipAmt)
      ENDIF

    ENDIF    
    *B607870,1 TMI [End  ] 
    UNLOCK
  ENDIF
  GO lnRecNo
  m.lCompUps = .F.
  =RLOCK()

  *B607870,1 TMI [Start] Remark the next fields and add new fields.
  *GATHER MEMVAR FIELDS SHIPAMT,Cod_Flag,lUpsIns,DISCPCNT,TAX_RATE,cTaxRule,;
  *nPstRate,ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,;
  *nPstAmt,nHstAmt,TOTALCHG,lCompUps
  GATHER MEMVAR FIELDS SHIPAMT,Cod_Flag,lUpsIns,DISCPCNT,TAX_RATE,cTaxRule,;
  nPstRate,ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,;
  nPstAmt,nHstAmt,TOTALCHG,lCompUps,nTaxDue,Discount,nMerchTax
  *B607870,1 TMI [End  ] 
  UNLOCK
ENDIF

IF !llCalled
  SET ORDER TO TAG (lcInvHdr)
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
  WAIT CLEAR
ENDIF

*:**************************************************************************
*: Name      : lfCRTTMP
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Create temp file.
*:**************************************************************************
*: Parameters: 
*:**************************************************************************
*: Returns   : 
*:**************************************************************************
FUNCTION lfCRTTMP
=gfOpenFile(gcDataDir+'ARUPSSHP',gcDataDir+'ARUPSSHP','SH')
=AFIELDS(laFileStru)
=gfCrtTmp(lcUpsTmp,@laFileStru,[PikTkt+STR(CARTONS,5)],lcUpsTmp)

*:**************************************************************************
*: Name      : lpUpsChrg
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Calculate charges.
*:**************************************************************************
*: Parameters: 
*:**************************************************************************
*: Returns   : 
*:**************************************************************************
FUNCTION lpUpsChrg

PARAMETERS lcAccount,lcStore,lcOrderNo,lcPikTKt,lcWareCode,lnShipAmt,llCod,;
           llupsinsur,lnDscPrcnt,lcTaxRate,lcPstRule,lnPstRate,lcUpsFile,;
           lcShipVia,lcCOD_AMT,lcFreight,lcInsur,lcCOD,lcCartons,lcWeight,;
           lcTaxAmnt,lcPstAmnt,lnTaxDue,lcChrgType
PRIVATE laShpViaFld,lcUpsType,lnCodCharge,lnInsCharge,lnFxdPrcnt,lcUpsIncr,;
        llCompTax,lcTaxRule,getf,llUpsBox

IF lnShipAmt = 0
  RETURN           
ENDIF

*B608458,1 WAM 02/28/2008 Get the order record in ORDHDR file
=SEEK('O'+lcOrderNo,'ORDHDR')
*B608458,1 WAM 02/28/2008 (End)

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
  *-- Add records for each carton in the UPSBox file
  =lfUpsBox(lcAccount,lcStore,lcOrderNo,lcPikTKt,llCod,llupsinsur,;
            &lcFreight,&lcInsur,&lcCOD,&lcCartons,&lcWeight,;
            lnShipAmt,lcUpsFile,lcChrgType)
  *-- Calculate freights             
  =lfGetFright(lcFromZone,SUBSTR(CUSTOMER.caddress5,1,3),lcUpsType  ,;
               llCod,llupsinsur,lcFreight,lcInsur,lcCOD,lcCartons,lcWeight,;
               lnCodCharge,lnInsCharge,lnShipAmt,lcChrgType)
  IF llCod
    *-- calculate the cash on delivery amount
    &lcCOD_AMT = lnShipAmt+&lcFreight+&lcInsur+&lcCOD-lnShipAmt*lnDscPrcnt/100

    *B123213,1 NNA 07/13/2004(BEGIN) Stop function that divide Cfreight per Carton to get freight from the ArupsShp
    *=lfUPDCOD(lcUpsFile)
    *B123213,1 NNA (End)

  ENDIF  
ENDIF
*C124347,1 NNA  09/02/2004 (BEGIN) Add the Customer.nhand_charg Value to the Freight for SYU03
IF ASCAN(laEvntTrig , PADR('HNDFRGHT',10)) <> 0
  =gfDoTriger('ARIINV',PADR('HNDFRGHT',10))
ENDIF  
*C124347,1 NNA (END)

IF llCompTax
  *-- CALCULATE TAXES
  &lcTaxAmnt = ROUND(lcTaxRate*(IIF(lcTaxRule='M',lnTaxDue,lnTaxDue+;
               &lcFreight+&lcInsur+&lcCOD)-ABS(lnTaxDue*lnDscPrcnt/100) )/100,2)
  IF llCod
    *-- add the tax amount and the  pst amount to the cod amount  
    &lcCOD_AMT = &lcCOD_AMT+&lcTaxAmnt+&lcPstAmnt

    *B123213,1 NNA 07/13/2004(BEGIN) Stop function that divide Cfreight per Carton to get freight from the ArupsShp
    *=lfUPDCOD(lcUpsFile)
    *B123213,1 NNA (End)

  ENDIF
ENDIF

*:*************************************************************
*: Name      : lfGetFright
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Calculate freights
*:*************************************************************
*: Calls     : gfModalGen
*:*************************************************************
*: Parameters: lcUpsFrom
*:             lcToZip
*:             lcUpsType
*:             llCod
*:             llupsinsur
*:             lcFreight
*:             lcInsur
*:             lcCOD
*:             lcCartons
*:             lcWeight
*:             lnCodCharge
*:             lnInsCharge
*:             lcChrgType : Charge Type: Freight, Insurance, Cod, All
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfGetFright()
*:*************************************************************
FUNCTION lfGetFright
 PARAMETERS lcUpsFrom,lcToZip,lcUpsType,llCod,llupsinsur,lcFreight,lcInsur,;
           lcCOD,lcCartons,lcWeight,lnCodCharge,lnInsCharge,lnTotShpAmt,lcChrgType
PRIVATE lcCurrExac,lcCurrNear

IF INLIST(lcChrgType,'A','C')
  *--  Recalculate the cod 
  *&lcCOD   = IIF(llCod,IIF('USUPS' $ lcUpsType,lnCodCharge*&lcCartons,&lcCOD),0)
ENDIF
IF INLIST(lcChrgType,'A','I')
  *--  Recalculate the insurance
  &lcInsur = IIF(llupsinsur,IIF('USUPS' $ lcUpsType AND lnTotShpAmt >=100,;
                 FLOOR(lnTotShpAmt/100)*lnInsCharge,0),0)               
ENDIF
RETURN

*:**************************************************************************
*: Name      : lfUpsBox
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Add cartons.
*:**************************************************************************
*: Parameters: 
*:**************************************************************************
*: Returns   : 
*:**************************************************************************
FUNCTION lfUpsBox

PARAMETERS lcAccount,lcStore,lcOrderNo,lcPikTKt,llCod,llupsinsur,lnFreight,;
           lnInsure,lnCodAmt,lnCartons,lnWeight,lnTotShpAmt,lcUpsFile,lcChrgType
lnAlias = SELECT()

*B607870,1 TMI [Start] hide the lcKey variable to preserve it value in the outer function
PRIVATE lcKey
*B607870,1 TMI [End  ] 

*B123213,1 NNA 07/13/2004 (Begin) be Sure that Variable [lcInvHdr] used because if we Saved Invoice 
*B123213,1 NNA            before openning the Charge Folder ,this file will be used with [lcInvFile]
IF TYPE('lcInvHdr') = 'U' 
  PRIVATE lcInvHdr 
  lcInvHdr = lcInvFile
ENDIF
*-- Cleaning the ARUPSSHP File From Voided Records Before do Browsing
 *B129997,1 NNA 10/13/2005 (Begin) add the Piktkt No. as a Parameter to scan by it
 = lfChkUpsSh(lcPikTKt)
 *B129997,1 NNA (End)
*B123213,1 NNA (End)

IF lcChrgType = 'C'
  SELECT (lcUpsFile)
  =SEEK(lcPikTkt)
  IF llCod
    SUM VAL(CCOD) TO &lcCOD WHILE PIKTKT = lcPikTkt
  ENDIF
  REPLACE ALL Cod_Flag WITH IIF(llCod,'Y','N') FOR lcPikTkt  = lcPikTkt
  SELECT (lnAlias)
  RETURN
ENDIF
lcOldFlag = m.Cod_Flag
*--  Get the no of carton from back_hdr incase packed pikTkt.
IF !EMPTY(lcPikTkt) AND SEEK(lcPikTkt,"ARUPSSHP")
  lcOldPik = lcPikTkt
  lnCartNo = 0    && Carton no 
  lnCrtWgh = 0    && Carton Weight
  &lcFreight = 0
  &lcWeight  = 0
  &lcCartons = 0
  &lcCOD_AMT = 0
  &lcCOD     = 0
  lnCodAmt   = 0
  SELECT (lcUpsFile)
  DELETE ALL
  SELECT (lcInvHdr)  

  *B608458,1 WAM 02/28/2008 Restore COD Tracking# UPS table
  REPLACE cCodTrckNo WITH ARUPSSHP.cTrack_No
  m.cCodTrckNo = ARUPSSHP.cTrack_No
  *B608458,1 WAM 02/28/2008 (End)

  lcOldOrd = ORDER()
  SET ORDER TO TAG (lcInvHdr)
  lcKey = EVAL(KEY())
  SET ORDER TO TAG &lcOldOrd 
  IF ORDHDR.MULTI = 'Y'
    SELECT (lcInvHdr)
    SCAN
      lcPikTkt = PIKTKT
      SELECT ARUPSSHP
      =SEEK(lcPikTkt)
      IF EOF() AND lcPikTkt = lcOldPik
        *--PIKTKT NOT FOUND  
        lcStatus = "Shipment information for Picking ticket # " +lcPikTkt+" not found. Cannot calculate charges."
        =gfModalGen("QRM00000B00000",.F.,.F.,.F.,lcStatus)
      ENDIF
      SCAN WHILE PikTkt = lcPikTkt
        lnOldCrt = &lcCartons
        SCATTER MEMVAR MEMO
        &lcCartons = lnOldCrt
        SELECT (lcUpsFile)
        APPEND BLANK    && New line for each Carton
        GATHER MEMVAR MEMO
        REPLACE CARTONS WITH ARUPSSHP.CARTONS

        *B123213,1 NNA 07/13/2004(BEGIN) Replace cFreight with (cFreight - cCod)
        lcFrgt = ALLTRIM(STR(ROUND((VAL(cFreight) - VAL(cCod)),2),10,2))
        IF FIELD(5)='CFREIGHT'
          REPLACE CFREIGHT WITH lcFrgt
        ELSE
          REPLACE FREIGHT WITH VAL(cFrgt)
        ENDIF
        *B123213,1 NNA (End)
    
        IF lcPikTkt  = lcOldPik
          &lcFreight = &lcFreight + VAL(cfreight)
          &lcWeight  = &lcWeight  + VAL(cWeight)

          *B608458,1 WAM 02/28/2008 Restore Number of Cartons
          *&lcCartons = &lcCartons + 1
          &lcCartons = Cartons
          *B608458,1 WAM 02/28/2008 (End)
        ENDIF  
      ENDSCAN
    ENDSCAN
    SELECT (lcUpsFile)
    =SEEK(lcOldPik)
    IF llCod
      SUM VAL(CCOD) TO &lcCOD WHILE PIKTKT = lcOldPik
    ENDIF
    REPLACE ALL Cod_Flag WITH IIF(llCod,'Y','N') FOR lcPikTkt  = lcOldPik    
    SELECT (lcInvHdr)
    lcPikTkt = lcOldPik
    REPLACE Cod_Flag WITH &lcUpsFile..Cod_Flag
  ELSE
    SELECT ARUPSSHP
    SCAN WHILE PikTkt = lcPikTkt
      lnOldCrt = &lcCartons
      SCATTER MEMVAR MEMO
      &lcCartons = lnOldCrt
      SELECT (lcUpsFile)
      APPEND BLANK    && New line for each Carton
      GATHER MEMVAR MEMO
      REPLACE CARTONS WITH ARUPSSHP.CARTONS      

      *B123213,1 NNA 07/13/2004(BEGIN) Replace cFreight with (cFreight - cCod)
      lcFrgt = ALLTRIM(STR(ROUND((VAL(cFreight) - VAL(cCod)),2),10,2))
      IF FIELD(5)='CFREIGHT'
        REPLACE CFREIGHT WITH lcFrgt
      ELSE
        REPLACE FREIGHT WITH VAL(lcFrgt)
      ENDIF
      *B123213,1 NNA (End)

      &lcFreight  = &lcFreight + VAL(cfreight)
      &lcWeight   = &lcWeight  + VAL(cWeight)

      *B608458,1 WAM 02/28/2008 Restore Number of Cartons
      *&lcCartons  = &lcCartons + 1
      &lcCartons  = Cartons
      *B608458,1 WAM 02/28/2008 (End)

    ENDSCAN

    *B122053,1 NNA 03/15/2004 (Begin) Convert the memory Variables from text to numeric to accept Decimal
	=lfConvVar(.T.)
    *B122053,1 NNA (End)

    SELECT (lcUpsFile)
    =SEEK(lcOldPik)
    IF llCod
      SUM VAL(CCOD) TO &lcCOD WHILE PIKTKT = lcOldPik
    ENDIF

    *B123213,1 NNA 07/13/2004 (Begin) if we save Invoice before openning the Charge Folder [Cod_Flag]
    *B123213,1 NNA             Field will Not be Found because file lcUpsFile Created in this Case from
    *B123213,1 NNA             'UPSBOX' File but with the Charge Folder it Creating from the ArUpsShp file    
    IF FIELD(10)='COD_FLAG'
    *B123213,1 (End)

      REPLACE ALL Cod_Flag WITH IIF(llCod,'Y','N') FOR lcPikTkt  = lcOldPik    
      lcPikTkt = lcOldPik
      REPLACE Cod_Flag WITH &lcUpsFile..Cod_Flag

    *B123213,1 NNA (Begin)
    ENDIF
    *B123213,1 (End)
  ENDIF
  SELECT (lcInvHdr)  
  lcOldOrd = ORDER()
  SET ORDER TO TAG (lcInvHdr)
  =SEEK(lcKey)
  SET ORDER TO TAG &lcOldOrd 
ELSE
  *--PIKTKT NOT FOUND  
  lcStatus = "Shipment information for Picking ticket # " +lcPikTkt+" not found. Cannot calculate charges."
  =gfModalGen("QRM00000B00000",.F.,.F.,.F.,lcStatus)
ENDIF
m.Cod_Flag = lcOldFlag
SELECT (lnAlias)
RETURN


*:*****************************************************************
*: Name      : lfEDTFRT
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Modify Freight AND COD
*:*************************************************************
FUNCTION lfEDTFRT

lnAlias = SELECT()
SELECT (lcUpsTmp)
=SEEK(laData[3])
IF (m.FREIGHT = 0 AND VARREAD() = 'FREIGHT') OR (m.Cod= 0 AND VARREAD() = 'COD')
  IF VARREAD() = 'FREIGHT'
    REPLACE ALL WHILE PikTkt = laData[3] CFREIGHT WITH ""
  ELSE
    *ADEL
    *REPLACE ALL WHILE PikTkt = laData[3] CCOD_AMT WITH ""
    REPLACE ALL WHILE PikTkt = laData[3] CCOD WITH ""
  ENDIF  
ELSE
  IF lnOldChrg <> IIF(VARREAD() = 'FREIGHT',m.FREIGHT,m.COD)
    COUNT FOR PikTkt = laData[3] TO lnCrtOfNo
    =SEEK(laData[3])
    IF VARREAD() = 'FREIGHT'
      REPLACE REST WHILE PikTkt = laData[3] cFREIGHT WITH ALLTRIM(STR(m.FREIGHT/lnCrtOfNo,3))
    ELSE
      *ADEL
      *REPLACE REST WHILE PikTkt = laData[3] cCod_Amt WITH STR(m.Cod/lnCrtOfNo,10)    
      REPLACE REST WHILE PikTkt = laData[3] cCod WITH ALLTRIM(STR(m.Cod/lnCrtOfNo,9))    
    ENDIF
  ENDIF                                                    
ENDIF
SELECT (lnAlias)


*:*****************************************************************
*: Name      : lfUPDCOD
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Update COD amount.
*:*************************************************************
FUNCTION lfUPDCOD
PARAMETERS lcFile
lnAlias = SELECT()
lcFile = IIF(EMPTY(lcFile),lcUpsTmp,lcFile)
SELECT (lcFile)
COUNT FOR PikTkt = laData[3] TO lnCrtOfNo
REPLACE ALL &lcFile..CCOD_AMT WITH ALLTRIM(STR(m.Cod_Amt/lnCrtOfNo)) ;
        FOR PikTkt = laData[3]
SELECT (lnAlias)

*:******************************************************************
*: Name      : lfOgiCRT
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Call Custom carton information screen.
*:******************************************************************
FUNCTION lfOgiCRT

=lfCARTON(lcUpsTmp,laData[11],laData[4],laData[1],laData[5],laData[3],m.lUpsIns,;
(m.Cod_Flag='Y'),m.SHIPAMT,m.ShipVia,laData[7],'m.Cartons','m.Weight',;
'm.Freight','m.Insur','m.Cod','m.COD_AMT')
SELECT (lcInvHdr)
=RLOCK()
GATHER MEMVAR FIELDS &lcFlFields
REPLACE  WEIGHT  WITH M.WEIGHT,;
         FREIGHT WITH M.FREIGHT,;
         COD     WITH M.COD,;
         COD_AMT WITH M.COD_AMT
UNLOCK
SHOW GETS WINDOW (lcWinChrg) ONLY
=lfRefresh(lcWinChrg)


*:*****************************************************************
*: Name      : CARTON
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Invoice Cartons Details.
*:*************************************************************
*: Calls     : ARCAMUPS.SPX
*:*************************************************************
*: Parameters: lcUpsFile : UPS Box Temp. file name
*:             lcInvoice : Invoice#
*:             lcAccount : Account
*:             lcOrderNo : Order Number
*:             lcStore   : Store 
*:             lcPikTkt  : PikTkt Number
*:             llUpsInsur: UPS Insurance
*:             llCod     : COD
*:             lnShipAmt : Ship Amount
*:             lcShipVia : ShipVia
*:             lcWareCode: Warehouse
*:             lcCartons : Cartons Number
*:             lcWeight  : Weight
*:             lcFreight : Freight
*:             lcInsur   : Insurance
*:             lcCod     : Cod Charge
*:             lcCodAmnt : Cod Amount
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =gpUpsBox()
*:*************************************************************
FUNCTION lfCARTON

PARAMETERS lcUpsFile,lcInvoice,lcAccount,lcOrderNo,lcStore,lcPikTkt,;
           llUpsInsur,llCod,lnShipAmt,lcShipVia,lcWareCode,lcCartons,;
           lcWeight,lcFreight,lcInsur,lcCod,lcCodAmnt
PRIVATE lnIWeight,lnIFreight,lnIInsure,lnICod,lnICodAmnt,lnICartons,lcBrowUps,;
        lnAlias,lnWeight,lnDecValue,lnFreight,lnInsure,lnCod,lnCodAmnt,;
        laShpViaFld,lcUpsType,lnCodCharge,lnFxdPrcnt,lcFromZone,lnInsCharge,lnUpsMark,lcShipViaD;
        lnTotCrt,lnTotShip,lcBol
lnAlias = SELECT()
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
lnInsure = 0
SELECT (lcUpsTmp)
IF laScrMode[4]
  =SEEK(lcPikTkt)
  *-- get the weight ,cod ,frieght for each carton
  SUM REST VAL(cWeight),VAL(cDecl_Val),VAL(cFreight),VAL(cCod),VAL(cCod_Amt);
  TO       lnWeight,lnDecValue,lnFreight,lnCod,lnCodAmnt ;
  WHILE PikTkt = lcPikTkt
  =SEEK(lcPikTkt)
  COUNT TO lnTotCrt WHILE PikTkt = lcPikTkt
  &lcCartons  = lnTotCrt
  =SEEK(lcPikTkt)
ELSE
  =SEEK(lcInvoice)
  SUM REST Weight,Decl_Value,Freight,Insur,Cod,TotalCod ;
  TO       lnWeight,lnDecValue,lnIFreight,lnInsure,lnCod,lnCodAmnt ;
  WHILE invoice+store+STR(cartons,5) = lcInvoice
  =SEEK(lcInvoice)
  COUNT TO lnTotCrt WHILE invoice+store+STR(cartons,5) = lcInvoice
  &lcCartons  = lnTotCrt
  =SEEK(lcInvoice)
ENDIF
lnTotShip  = m.ShipAmt
lcShipViaD = gfCodDes(M.SHIPVIA , 'SHIPVIA')
lcBol      = &lcInvHdr..Bol_No
llNewUps = .F.
SCATTER MEMVAR
*B122053,1 NNA 03/15/2004 (Begin) Convert the memory Variables from text to numeric to accept Decimal
=lfConvVar(.T.)
*B122053,1 NNA (End)
lcEOFStatu = IIF(EOF(),"DISABLE","ENABLE")
lcRemStatus= IIF(EOF(),"DISABLE","ENABLE")
lcInsStatus= IIF(EOF(),"DISABLE","ENABLE")

DO (gcScrDir+gcWinAppl+"\ARCAMUPS.SPX")
IF laScrMode[4]     && Add mode
  SELECT (lcUpsTmp)
  =SEEK(lcPikTkt)
  SUM REST VAL(CWeight),VAL(CFreight) ;
  TO       &lcWeight,&lcFreight ;
  WHILE PikTkt = lcPikTkt;
  FOR Weight <> 0 AND  Freight <> 0
  *--For :  is added for ignoring incorrect cratons (not counted :  _Tally)
  &lcCartons = _TALLY   && Number of cartons
  m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod+m.Discount+m.Tax_Amt+m.nPstAmt+m.nHstAmt
ENDIF
SELECT (lnAlias)

*:*************************************************************
*: Name      : lfUpsBrow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Browse Invoice Cartons Details.
*:*************************************************************
*: Calls     : lfShowUPS()
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfUpsBrow()
*:*************************************************************
FUNCTION lfUpsBrow

*B606936,1 ASH 02/06/03 (Begin) Display a sequential number in the carton field.
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnUpsMark,'>',' '):H=' ' :R :1 :W=.F.,;
       Cartons   :H='Carton#'         :P= '99999'  :R    ,;
       cWeight   :H='Weight  '        :P= '999999' :R    ,;
       cFreight  :H='Freight     '    :P= '999999999' :R, ;       
       CTrack_NO :H='Package ID #               '  :P= 'XXXXXXXXXXXXXXXXX' :R ,;
       Cdecl_val :H='Declared Value'  :P= 'XXXXXXXXX' :R ,;
       CCod      :H='COD'  :P= '999999999' :R ,;
       CCod_AMT  :H='COD Amount'  :P= '999999999' :R ;
       WINDOW ARCAMUP1   ;
       IN WINDOW ARCAMUPS;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WHEN lfShowUPS() ;
       KEY IIF(laScrMode[4],lcPikTkt,INVHDR.PIKTKT) ;
       TITLE lcBrowUPS
*B122053,1 NNA 03/15/2004 (Begin) Correct the browse picture for (CTrack_NO) to show all the digits
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnUpsMark,'>',' '):H=' ' :R :1 :W=.F.,;
       lnCartons=lfCartNInv()  :H='Carton#'         :P= '99999'  :R    ,;
       cWeight   :H='Weight  '        :P= '999999' :R    ,;
       cFreight  :H='Freight     '    :P= '999999999' :R, ;       
       CTrack_NO :H='Package ID #               '  :P= 'XXXXXXXXXXXXXXXXX' :R ,;
       Cdecl_val :H='Declared Value'  :P= 'XXXXXXXXX' :R ,;
       CCod      :H='COD'  :P= '999999999' :R ,;
       CCod_AMT  :H='COD Amount'  :P= '999999999' :R ;
       WINDOW ARCAMUP1   ;
       IN WINDOW ARCAMUPS;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WHEN lfShowUPS() ;
       KEY IIF(laScrMode[4],lcPikTkt,INVHDR.PIKTKT) ;
       TITLE lcBrowUPS

BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnUpsMark,'>',' '):H=' ' :R :1 :W=.F.,;
       lnCartons=lfCartNInv()  :H='Carton#'         :P= '99999'  :R    ,;
       cWeight   :H='Weight  '        :P= '999999' :R    ,;
       cFreight  :H='Freight     '    :P= '999999999' :R, ;       
       CTrack_NO :H='Package ID #               '  :P= 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX' :R ,;
       Cdecl_val :H='Declared Value'  :P= 'XXXXXXXXX' :R ,;
       CCod      :H='COD'  :P= '999999999' :R ,;
       CCod_AMT  :H='COD Amount'  :P= '999999999' :R ;
       WINDOW ARCAMUP1   ;
       IN WINDOW ARCAMUPS;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WHEN lfShowUPS() ;
       KEY IIF(laScrMode[4],lcPikTkt,INVHDR.PIKTKT) ;
       TITLE lcBrowUPS
*B122053,1 NNA (End)

*B606936,1 ASH 02/06/03 (End)

*:*************************************************************
*: Name      : lfShowUps
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Show function for Cartons details screen
*:*************************************************************
*: Calls     : lfRefresh()
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfShowUps()
*:*************************************************************
FUNCTION lfShowUps

lnUpsMark = RECNO()
SHOW WINDOW (lcBrowUps) REFRESH SAME
SCATTER MEMVAR
*B122053,1 NNA 03/15/2004 (Begin) Convert the memory Variables from text to numeric to accept Decimal
=lfConvVar(.T.)
*B122053,1 NNA (End)
SHOW GETS WINDOW 'ARCAMUP2' ONLY
=lfRefresh('ARCAMUP0')
=lfRefresh('ARCAMUP2')

*:*************************************************************
*: Name      : lfDUPSBrow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Deactivate function for Cartons details screen
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfDUPSBrow()
*:*************************************************************
FUNCTION lfDUPSBrow

IF WONTOP() = lcBrowUps
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTab WITH 'ARCAMUP2','m.Weight'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'ARCAMUP2','pbClsUps'
ENDIF  
RETURN .F.

*:*************************************************************
*: Name      : lfvCrtChrg
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Recalculate charges per carton
*:*************************************************************
*: Calls     : lfGetFright(),lfShowUps()
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfvCrtChrg()
*:*************************************************************
FUNCTION lfvCrtChrg

*B607870,1 TMI [Start] hide the lcKey variable to preserve it value in the outer function
PRIVATE lcKey
*B607870,1 TMI [End  ] 

lnOneCrtn=1
IF UPDATE()
  SELECT (lcUpsTmp)
  lcKey = EVAL(KEY())
  =RLOCK()
  *B122053,1 NNA 03/15/2004 (Begin) Convert the memory Variables from text to numeric to accept Decimal
  = lfConvVar(.T.)
  *B122053,1 NNA (End)
  GATHER MEMVAR
  *B122053,1 NNA 03/15/2004 (Begin) Convert the memory Variables from numeric to Text and replace then in the temp file
  =lfConvVar(.F.)
  *B122053,1 NNA (End)
  UNLOCK
    =SEEK(lcPikTkt)
  *-- get the weight ,cod ,frieght for each carton
  SUM REST VAL(CWeight),VAL(CFreight);
      TO   lnIWeight,lnIFreight ;
      WHILE PikTkt = lcPikTkt
  =SEEK(lcKey)
  =lfShowUps()
ENDIF

*:*************************************************************
*: Name      : lfvNewCrtn
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Add new cartons.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfvNewCrtn()
*:*************************************************************
FUNCTION lfvNewCrtn
SELECT (lcUpsTmp)
=SEEK(lcPikTkt)
COUNT REST TO lnNBoxNo WHILE PikTkt = lcPikTkt
INSERT INTO   (lcUpsTmp) (PIKTKT,Cartons);
       VALUES            (lcPikTkt,lnNBoxNo+1)
lnTotCrt = lnNBoxNo+1
SHOW GET m.cWeight    ENABLE
SHOW GET m.cFreight   ENABLE
SHOW GET m.cTrack_no  ENABLE
SHOW GET pbRemCrtn    ENABLE
SHOW GET m.CDecl_Val  ENABLE
SHOW GET m.CCod       ENABLE
SHOW GET m.ccod_amt   ENABLE
*B122053,1 NNA 03/15/2004 (Begin) Convert the memory Variables from text to numeric to accept Decimal
=lfConvVar(.T.)
*B122053,1 NNA (End)
SHOW GET lnTotCrt

*-- Show function for Cartons details screen
=lfShowUps()
_CUROBJ = OBJNUM(m.CWeight)

*:*************************************************************
*: Name      : lfvRemCrtn
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Remove invoice carton from the UPS Box
*:*************************************************************
*: Calls     : lfShowUps()
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfvRemCrtn()
*:*************************************************************
FUNCTION lfvRemCrtn
PRIVATE lnNBoxNo
*B122053,1 NNA 03/15/2004 (Begin) Convert the memory Variables from text to numeric if it is text
*B122053,1 NNA                    Also Remove the C.O.D Charge And amount 
*lnIWeight   = lnIWeight   - VAL(m.CWeight)
*lnIFreight  = lnIFreight  - VAL(m.CFreight)
*lnDecValue  = lnDecValue  - VAL(CDECL_VAL)
lnIWeight  = lnIWeight   - IIF(TYPE('m.CWeight')  ='N', m.CWeight,   VAL(m.CWeight))
lnIFreight = lnIFreight  - IIF(TYPE('m.CFreight') ='N', m.CFreight,  VAL(m.CFreight))
lnDecValue = lnDecValue  - IIF(TYPE('m.cDecl_Val')='N', m.cDecl_Val, VAL(m.cDecl_Val))
lnCod      = lnCod       - IIF(TYPE('m.cCod')     ='N', m.cCod ,     VAL(m.cCod))
lnCodAmnt  = lnCodAmnt   - IIF(TYPE('m.cCod_Amt') ='N', m.cCod_amt,  VAL(m.cCod_Amt))

*B122053,1 NNA (End)
SELECT (lcUpsTmp)
DELETE
lnNBoxNo = 1
=SEEK(lcPikTkt)
SCAN REST WHILE PikTkt = lcPikTkt
  =RLOCK()
  REPLACE Cartons WITH lnNBoxNo
  UNLOCK
  lnNBoxNo = lnNBoxNo + 1
ENDSCAN
lnTotCrt = lnNBoxNo-1
SHOW GET lnTotCrt
IF !SEEK(lcPikTkt)
  SHOW GETS WINDOW 'ARCAMUP2' DISABLE ONLY
  SHOW GET ibBrowUps ENABLE
  SHOW GET pbNewCrtn ENABLE
  SHOW GET pbcLSuPS  ENABLE
ENDIF
*-- Show function for Cartons details screen
=lfShowUps()

*:*****************************************************************
*: Name      : lfCloseB
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Check for lines with Freight or Weight = 0
*:*************************************************************
FUNCTION lfCloseB

*B607870,1 TMI [Start] hide the lcKey variable to preserve it value in the outer function
PRIVATE lcKey
*B607870,1 TMI [End  ] 

IF (!laScrMode[3] AND !laScrMode[4]) OR (TYPE('lcUpsTmp') = 'U')
  RETURN
ENDIF 

lnAlias = SELECT()
SELECT (lcUpsTmp)
lcKey = EVAL(KEY())
=SEEK(lcPikTkt)
lnNoOfBad = 0
lnNoOfCrt = 0
SCAN REST WHILE PikTkt = lcPikTkt
  IF  (VAL(CWEIGHT) = 0 )
    lnNoOfBad = lnNoOfBad + 1
  ENDIF  
  lnNoOfCrt = lnNoOfCrt +1
ENDSCAN
IF lnNoOfBad <> 0
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Cartons with weight values equal zero will be ignored upon saving.")
ENDIF  
&lcCartons = lnNoOfCrt
SELECT ORDLINE
=SEEK(lcKey)
SELECT (lnAlias)

*:******************************************************************
*: Name      : lfSAVIT
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Save invoice.
*:******************************************************************
FUNCTION lfSAVIT

*-- Update the arclpshp File 
lnNoOfCrt = 0

*E123512,1 NNA 07/11/2004 (Begin) Get the Piktkt Number To Search in ARUPSSHP File
*lcPiktkt = &lcHdrFile..PIKTKT
lnOldAls = SELECT(0)
SELECT (lcHdrFile)
IF CONSOL = 'Y'			&& in this case the Piktkt field is Empty So I Locate for the first
  lnRecNo = RECNO()     && Piktkt to hold it's CTrack_No and save it only in the INVHDR file
  LOCATE FOR !EMPTY(PIKTKT)
  IF FOUND()
    lcPiktkt = PIKTKT
  ELSE
    GOTO lnRecNo
    lcPiktkt = PIKTKT  
  ENDIF
ELSE
  lcPiktkt = PIKTKT
ENDIF
SELECT(lnOldAls)
*E123512,1 NNA (End)

IF !EMPTY(lcUpsTmp) AND USED(lcUpsTmp)
  WAIT 'Updating Carton Box File ' WINDOW NOWAIT
  SELECT ARUPSSHP
  =SEEK(lcPiktkt)

  *E123512,1 NNA 07/11/2004 (Begin) Update the INVHDR.cCodTrckNo with the ARUPSSHP.CTrack_No
  *-- if there are more than one piktkt (if Consolidated invoice) we Update INVHDR.cCodTrckNo
  *-- with the first ARUPSSHP.CTrack_No
  IF EMPTY(INVHdr.CcodTrckNo)    
    REPLACE INVHdr.CcodTrckNo WITH cTrack_No
  ENDIF
  *E123512,1 NNA (End)
  
  DELETE REST WHILE PIKTKT = lcPiktkt
  SELECT (lcUpsTmp)
  =SEEK(lcPiktkt)
  SCAN REST WHILE PIKTKT = lcPiktkt  FOR WEIGHT <> 0 
    SCATTER MEMVAR MEMO
    INSERT INTO ARUPSSHP FROM MEMVAR
  ENDSCAN
  *B606936,1 ASH 02/06/03 (Begin) Fix the problem of wrong updating the status field in ARUPSSHP file.
  SELECT ARUPSSHP 
  REPLACE ALL STATUS WITH 'C' FOR PIKTKT = lcPiktkt
  SELECT (lcUpsTmp)
  lnNoOfCrt = 0
  *B606936,1 ASH 02/06/03 (End)
ENDIF

*:******************************************************************
*: Name      : lfCHKIT
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Check invoice.
*:******************************************************************
FUNCTION lfCHKIT

IF &lcSaveInv AND UPPER(ALLTRIM(gcContCode))<>'ENG' .AND. &lcInvFile..lCompUps
  *--Calculate non english charges 
  SELECT (lcInvFile)
  SCATTER MEMVAR 
  *-- Calculate charges
  DO lpUpsChrg WITH ;
  m.Account,m.Store,m.Order,m.PikTkt,m.cWareCode,m.ShipAmt,m.Cod_Flag='Y',;
  m.lUpsIns,m.DISCPCNT,m.TAX_RATE,m.cTaxRule,m.nPstRate,lcUpsFile,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT',;
  'm.TAX_AMT','m.nPstAmt',m.nTaxDue,'A'
  m.nHstAmt = ROUND((m.nHstRate*m.ShipAmt)/100,2)
  m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod-ABS(m.ShipAmt*m.DiscPcnt/100)+;
               m.Tax_Amt+m.nPstAmt+m.nHstAmt
  SELECT (lcInvFile)
  m.lCompUps = .F.   
  =RLOCK()
  GATHER MEMVAR
  UNLOCK
ENDIF
IF &lcSaveInv AND SEEK('O'+Order,'ORDHDR') .AND. OrdHdr.ApprAmt > 0 .AND. ;
   &lcInvFile..TotalChg > OrdHdr.ApprAmt
  *--  Message : 40119
  *--  Invoice Amount Exceeds Order Approved Amount By 999999.
  *--  Button : 40003
  *--  Proceed Cancel
  IF gfModalGen('TRM40119B40003','ALERT',ALLTRIM(STR(&lcInvFile..TotalChg-OrdHdr.ApprAmt,9,2)))=2
    &lcSaveInv = .F.
  ENDIF
ENDIF
SELECT (lnAlias)

*:******************************************************************
*: Name      : lfOGIBOX
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Call Custom carton information screen.
*:******************************************************************
FUNCTION lfOGIBOX

=lfCARTBOX(laData[1],laData[2],laData[3],laData[5],laData[30],llupsinsur,llCod,;
          laData[31],laData[10],laData[21],'laData[25]',;
          'laData[26]','laData[34]','laData[35]','laData[36]','laData[28]')

*:*****************************************************************
*: Name      : lfCARTBOX
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Invoice Cartons Details.
*:*************************************************************
*: Calls     : ARCRTBOX.SPX
*:*************************************************************
*: Parameters: lcUpsFile : UPS Box Temp. file name
*:             lcInvoice : Invoice#
*:             lcAccount : Account
*:             lcOrderNo : Order Number
*:             lcStore   : Store 
*:             lcPikTkt  : PikTkt Number
*:             llUpsInsur: UPS Insurance
*:             llCod     : COD
*:             lnShipAmt : Ship Amount
*:             lcShipVia : ShipVia
*:             lcWareCode: Warehouse
*:             lcCartons : Cartons Number
*:             lcWeight  : Weight
*:             lcFreight : Freight
*:             lcInsur   : Insurance
*:             lcCod     : Cod Charge
*:             lcCodAmnt : Cod Amount
*:*************************************************************
*: Returns   : None 
*:*************************************************************
FUNCTION lfCARTBOX

PARAMETERS lcInvoice,lcAccount,lcOrderNo,lcStore,lcPikTkt,;
           llUpsInsur,llCod,lnShipAmt,lcShipVia,lcWareCode,lcCartons,;
           lcWeight,lcFreight,lcInsur,lcCod,lcCodAmnt
PRIVATE lnIWeight,lnIFreight,lnIInsure,lnICod,lnICodAmnt,lnICartons,lcBrowUps,;
        lnAlias,lnWeight,lnDecValue,lnFreight,lnInsure,lnCod,lnCodAmnt,;
        laShpViaFld,lcUpsType,lnCodCharge,lnFxdPrcnt,lcFromZone,lnInsCharge,lnUpsMark,lcShipViaD;
        lnTotCrt,lnTotShip,lcBol
STORE 0 TO lnUpsMark

lnAlias = SELECT()
lnICartons = &lcCartons
lnIWeight  = &lcWeight
lnIFreight = &lcFreight
lnIInsure  = &lcInsur
lnICod     = &lcCod
lnICodAmnt = &lcCodAmnt
lnInsure   = 0
=gfOpenFile(gcDataDir+'ARUPSSHP',gcDataDir+'ARUPSSHP','SH')
=SEEK(INVHDR.PikTkt)
*-- get the weight ,cod ,frieght for each carton
SUM REST VAL(cWeight),VAL(cDecl_Val),VAL(cFreight),VAL(cCod),VAL(cCod_Amt) ;
  TO       lnWeight,lnDecValue,lnFreight,lnCod,lnCodAmnt ;
  WHILE PikTkt = INVHDR.PIKTKT
=SEEK(INVHDR.PIKTKT)
COUNT TO lnTotCrt WHILE PikTkt = INVHDR.PIKTKT
&lcCartons  = lnTotCrt
=SEEK(INVHDR.PIKTKT)
lnTotShip  = INVHDR.ShipAmt
lcShipViaD = gfCodDes(INVHDR.SHIPVIA , 'SHIPVIA')
M.Bol_No   = INVHDR.Bol_No
llNewUps = .F.
SCATTER MEMVAR
lcOldInv  = lcInvoice
lcinvoice =  Invhdr.Piktkt
DO (gcScrDir+gcWinAppl+"\ARCAMUPS.SPX")
lcinvoice = lcOldInv
SELECT (lnAlias)

*:*************************************************************
*: Name      : lfCAMADBAR
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/30/2002
*: Purpose   : Bulid a new menu pad [Options]
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : lfDispBar()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lf..()
*:*************************************************************
*:
FUNCTION lfCAMADBAR

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions'
ON PAD _Option OF _msysmenu ACTIVATE POPUP _lPopOpt

DEFINE POPUP _lPopOpt MARGIN SHADOW
DEFINE BAR 1 OF _lPopOpt PROMPT "\<Shipment details" SKIP FOR EMPTY(laData[1])

ON SELECTION POPUP _lPopOpt DO lpOptions IN CAMMAIN
ON KEY LABEL ALT+P ACTIVATE POPUP _lPopOpt

RETURN
*-- End OF lfCAMADBAR
*:*************************************************************
*: Name      : PROCEDURE
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/30/2002
*: Purpose   : function to run the option bar.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : lfDispBar()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lf..()
*:*************************************************************
*:
PROCEDURE lpOptions

lnBarNo=BAR()

DO CASE
  CASE BAR() =1
  *-- function to call the prepar the tables & the UPS shipment detail screen.
  = lfUpsDetil ()
ENDCASE

RETURN
*-- End OF lpOptions.
*:*************************************************************
*: Name      : lfUpsDetil
*: Developer : Abdou Elgendy (ABD)
*: Date      : 12/30/2002
*: Purpose   : Bulid a new menu pad [Options]
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lf..()
*:*************************************************************
*:
FUNCTION lfUpsDetil

PRIVATE lnIWeight,lnIFreight,lnIInsure,lnICod,lnICodAmnt,lnAlias ;
        lnWeight,lnDecValue,lnFreight,lnCod,lnCodAmnt

STORE 0 TO lnIWeight,lnIFreight,lnIInsure,lnICod,lnICodAmnt,lnAlias,lnShipAmt,lnUpsMarks,;
           lnWeight,lnDecValue,lnFreight,lnCod,lnCodAmnt

STORE '' TO lcUpsTmp , lcBrowUpss
STORE .F. To llUpsInsur , llCod
lnalias = SELECT(0)

*B123213,1 NNA 07/13/2004 (Begin) Cleaning the ARUPSSHP File From Voided Records Before do Browsing
 *B129997,1 NNA 10/13/2005 (Begin) add the Piktkt No. as a Parameter to scan by it
 = lfChkUpsSh(laDATA[1])
 *B129997,1 NNA (END)
*B123213,1 NNA (End)

lcUpsStatu = 'Pending'
lcStatColor = "RGB(255,255,255,0,128,0)"

*-- Create the temp file.
llOpenUps  = gfOpenFile(gcDataDir+'ARUPSSHP',gcDataDir+'ARUPSSHP','SH')
llOpnInvhd = gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
llOpnInvln = gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINEO','SH')
SET RELATION TO Invoice INTO INVHDR ADDI

IF !USED('ARUPSSHP')
  RETURN
ENDIF

SELECT ARUPSSHP
lnInsure = 0
llFound = SEEK(laData[1],'ARUPSSHP')
IF llFound
  *-- get the weight ,cod ,frieght for each carton
  SUM REST VAL(cWeight),VAL(cDecl_Val),VAL(cFreight),VAL(cCod),VAL(cCod_Amt) ;
    TO       lnWeight,lnDecValue,lnFreight,lnCod,lnCodAmnt ;
    WHILE PikTkt = ladata[1]

  = SEEK(laData[1],'ARUPSSHP')
  COUNT TO lnTotCrt WHILE PikTkt = ladata[1]
  lcUpsStatu = 'Shipped'
  lcStatColor = "RGB(255,255,255,255,0,0)"
ENDIF

*-- get the Cod_Flag From the invoice header.
IF SEEK(ladata[3],'INVLINE') .AND. INVHDR.Status # 'V'
  lnInsure  = InvHdr.Insur
ELSE
  LOCATE REST WHILE Order+STR(lineno,6)+Invoice = ladata[3] ;
  FOR INVHDR.Status # 'V'
  IF FOUND()
    lnInsure  = InvHdr.Insur
  ENDIF
ENDIF

SELECT ARUPSSHP
Locate

STORE SPACE(0) TO lcEscTrap , lcCtrETrap , lcCtrHTrap , lcCtrNTrap

PUSH KEY
ON KEY LABEL ALT+A ACTIVATE WINDOW (lcBrowUpss)
ON KEY LABEL TAB
ON KEY LABEL BACKTAB

lcHldEsc = ON('KEY','ESC')
ON KEY LABEL ESC DO lpDetEsc
lcEscTrap = ON('KEY','ESC')
DO (gcScrDir+gcWinAppl+"\ALCAMUPS.SPX")
POP KEY

ON KEY LABEL ESC     &lcHldEsc

IF llOpenUps
  = gfCloseFile('ARUPSSHP')
ENDIF


IF llOpnInvhd
  = gfCloseFile('INVHDR')
ENDIF

IF llOpnInvln
  = gfCloseFile('INVLINE')
ENDIF

SELECT(lnalias)
*-- End OF lfUpsDetil
*:*************************************************************
*: Name      : lfUpsBrws
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Browse Invoice Cartons Details.
*:*************************************************************
*: Calls     : lfShowUPS()
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfUpsBrws()
*:*************************************************************
FUNCTION lfUpsBrws

SELECT ARUPSSHP

LOCATE
*B606936,1 ASH 02/06/03 (Begin) Display a sequential number in the carton field.
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnUpsMarks,'>',' '):H=' ' :R :1 :W=.F.,;
       Cartons   :H='Carton#'         :P= '99999'  :R    ,;
       cWeight   :H='Weight  '        :P= '999999' :R    ,;
       cFreight  :H='Freight     '    :P= '999999999' :R, ;       
       CTrack_NO :H='UPS Tracking #               '  :P= 'XXXXXXXXXXXXXXXXX' :R ,;
       Cdecl_val :H='Declared Value'  :P= 'XXXXXXXXX' :R ,;
       CCod      :H='COD'  :P= '999999999' :R ,;
       CCod_AMT  :H='COD Amount'  :P= '999999999' :R ;
       WINDOW ALCAMUP1  ;
       IN WINDOW ALCAMUPS;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
	   VALID lfvBrows()  ;
       WHEN lfShowUPSs() ;
       Key ladata[1]     ;
       TITLE lcBrowUpss

BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnUpsMarks,'>',' '):H=' ' :R :1 :W=.F.,;
       lnCartons=lfCartNo()   :H='Carton#'         :P= '99999'  :R    ,;
       cWeight   :H='Weight  '        :P= '999999' :R    ,;
       cFreight  :H='Freight     '    :P= '999999999' :R, ;       
       CTrack_NO :H='UPS Tracking #               '  :P= 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX' :R ,;
       Cdecl_val :H='Declared Value'  :P= 'XXXXXXXXX' :R ,;
       CCod      :H='COD'  :P= '999999999' :R ,;
       CCod_AMT  :H='COD Amount'  :P= '999999999' :R ;
       WINDOW ALCAMUP1  ;
       IN WINDOW ALCAMUPS;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
	   VALID lfvBrows()  ;
       WHEN lfShowUPSs() ;
       Key ladata[1]     ;
       TITLE lcBrowUpss
*B606936,1 ASH 02/06/03 (End)
*-- end of lfUpsBrws
*:*************************************************************
*: Name      : lfShowUPSs
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Show function for Cartons details screen
*:*************************************************************
*: Calls     : lfRefresh()
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfShowUPSs()
*:*************************************************************
FUNCTION lfShowUPSs
PRIVATE lnalias


SELECT ARUPSSHP
lnUpsMarks = RECNO()
SHOW WINDOW (lcBrowUpss) REFRESH SAME
SHOW GETS WINDOW 'ALCAMUP0' ONLY
=lfRefresh('ALCAMUP0')

RETURN .F.

*-- End OF lfShowUPSs
*:*************************************************************
*: Name      : lfBrowUPS
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/30/2002
*: Purpose   : Deactivate function for Cartons details screen
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfBrowUPS()
*:*************************************************************
FUNCTION lfBrwUnTrp


*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcBrowUpss) .AND. llFromBrow
  llFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+A ACTIVATE WINDOW (lcBrowUpss)
  ON KEY LABEL ESC &lcEscTrap
  ON KEY LABEL CTRL+END &lcCtrETrap
  ON KEY LABEL CTRL+HOME &lcCtrHTrap
  ON KEY LABEL CTRL+ENTER &lcCtrNTrap
ENDIF

RETURN .F.

*-- End OF lfBrowUPS.
*:*************************************************************
*: Name      : lpTab
*: Developer : Wael Aly Mohamed
*: Date      : 12/30/2002
*: Purpose   : Trap of tab key.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpTab WITH 'ARDINV0', 'pbClose'
*:*************************************************************
*:
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*-- ENd OF lpTab.
*:*************************************************************
*: Name      : lfvBrows
*: Developer : Abdou Elgendy
*: Date      : 12/30/2002
*: Purpose   : VALID Function for the Style Color Size browse
*:*************************************************************
*: Called from : None.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfvBrows

llFromBrow = .T.

*-- End OF lfvBrows
*:*************************************************************
*: Name      : lpDetEsc
*: Developer : Abdou Elgendy
*: Date      : 12/30/2002
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpDetEsc
*:*************************************************************
*
PROCEDURE lpDetEsc

ACTIVATE WINDOW ALCAMUP0
_CUROBJ = OBJNUM(PbClose)
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpDetEsc
*:*************************************************************
*: Name      : lfBrowTrap
*: Developer : Abdou Elgendy
*: Date      : 12/30/2002
*: Purpose   : Function to trap the keys for the Browse
*:*************************************************************
*: Called from :  None.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfBrowTrap

*-- IF The window on top is the Browse
IF WONTOP(lcBrowUpss)
  llFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH 1
  ON KEY LABEL BACKTAB DO lfTraps WITH 2
  ON KEY LABEL ALT+A
  lcEscTrap = ON('KEY' , 'ESC')                && Varible to hold the ESC key traping
  lcCtrETrap = ON('KEY' , 'CTRL+END')          && Varible to hold the CTRL+END key traping
  lcCtrHTrap = ON('KEY' , 'CTRL+HOME')         && Varible to hold the CTRL+HOME key traping
  lcCtrNTrap = ON('KEY' , 'CTRL+ENTER')        && Varible to hold the CTRL+ENTER key traping
  ON KEY LABEL ESC DO lfTraps WITH 3
  ON KEY LABEL CTRL+END DO lfTraps WITH 4
  ON KEY LABEL CTRL+HOME DO lfTraps WITH 5
  ON KEY LABEL CTRL+ENTER DO lfTraps WITH 6
ENDIF    && End of IF

RETURN .F.

*-- End Of lfBrowTrap
*:*************************************************************
*: Name      : lfTraps
*: Developer : Abdou Elgendy
*: Date      : 12/30/2002
*: Purpose   : Function to be called from some traped keys
*:*************************************************************
*: Called from : lfBrowTrap()
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : 1 , 2
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfTraps

PARAMETERS lnTrap

DO CASE 
  *-- Case TAB
  CASE lnTrap = 1
    IF WONTOP()= lcBrowUpss
      glFromBrow = .T.
      ACTI WINDOW ALCAMUP0 TOP
    ELSE
      _CUROBJ = _CUROBJ +1
    ENDIF

  *-- Case BACKTAB
  CASE lnTrap = 2
    ACTI WINDOW ALCAMUP0 TOP
    
  *-- Case ESC
  CASE lnTrap = 3
   DO  lpDetEsc
    
  *-- Case CTRL+END
  CASE lnTrap = 4
    GO BOTTOM
  
  *-- Case CTRL+HOME
  CASE lnTrap = 5
    GO TOP
  
  *-- Case CTRL+ENTER
  CASE lnTrap = 6
    RETURN
  
ENDCASE

*-- End OF lfTraps.
*:*************************************************************
*: Name      : lfRfrhFld
*: Developer : Abdou Elgendy
*: Date      : 12/30/2002
*: Purpose   : Function to be called from some traped keys
*:*************************************************************
*: Called from : Screen arcamup3.scx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : Filed name  , Variable on the screen.
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfRfrhFld
PARAMETERS lcField , lcVariable
PRIVATE lnRecNo
IF EOF()
  RETURN
ENDIF
*B122053,1 NNA 03/15/2004 (Begin) Convert the memory Variables from text to numeric to accept Decimal
=lfConvVar(.T.)
*B122053,1 NNA (End)
GATHER MEMVAR MEMO
*B122053,1 NNA 03/15/2004 (Begin) Convert the memory Variables from numeric to Text and replace then in the temp file
=lfConvVar(.F.)
*B122053,1 NNA (End)
lnRecNo = RECNO()
lcPiktkt = piktkt
LOCATE
=SEEK(lcPiktkt)
SUM REST Val(EVal(lcField));
  TO    lnSumFlds;
  WHILE PikTkt = lcPiktkt

&lcVariable. = lnSumFlds

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF

=lfRefresh()

*-- END Of lfRfrhFld.
*:**************************************************************************
*: Name      : lfCAMRLBAR
*: Developer : Abdou Elgendy
*: Date      : 12/30/2002
*: Purpose   : release the option menu.
*:**************************************************************************
*: Example   : = lfCAMRLBAR()
*:*************************************************************
*: Called from : Piktkt Program.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfCAMRLBAR

RELEASE PAD _Option OF _MSYSMENU

*-- End of lfCAMRLBAR.

*:*************************************************************
*: Name      : lfCartNo
*: Developer : Abdou Elgendy         **B606936,1 
*: Date      : 02/06/2003
*: Purpose   : display sequential number for the cartons
*:*************************************************************
*: Example   : = lfCartNo()
*:*************************************************************
*: Called from : Piktkt Program.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*:

FUNCTION lfCartNo
PRIVATE lnRecNo , lncurrt, lnCarton 
SELECT ARUPSSHP
lnRecNo = RECNO()
=SEEK(ladata[1])
lncurrt = RECNO()
lnCarton = lnRecNo - lncurrt 
IF BETWEEN(lnRecNo ,1,RECCOUNT())
  GOTO lnRecNo 
ENDIF

RETURN lnCarton + 1

*:*************************************************************
*: Name      : lfCartNInv
*: Developer : Abdou Elgendy         **B606936,1 
*: Date      : 02/06/2003
*: Purpose   : display sequential number for the cartons
*:*************************************************************
*: Example   : = lfCartNo()
*:*************************************************************
*: Called from : Piktkt Program.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************

FUNCTION lfCartNInv
lnalias=alias()
SELECT (lcUpsTmp)
lnRecNo = RECNO()
=SEEK(IIF(laScrMode[4],lcPikTkt,INVHDR.PIKTKT))
lncurrt = RECNO()
lnCarton = lnRecNo - lncurrt 

IF BETWEEN(lnRecNo ,1,RECCOUNT())
  GOTO lnRecNo 
ENDIF
SELECT (lnalias)
RETURN lnCarton + 1


*:*************************************************************
*: Name      : lfConvVar
*: Developer : Nader Nabil NNA          
*: Date      : 03/15/2004
*: Purpose   : Convert the ups Charges Variables From Text to Numeric
*:*************************************************************
*: Example   : = lfConvVar(.T.)
*:*************************************************************
*: Called from : Piktkt Program.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : lloperate
*:*************************************************************
*: Return      : None
*:*************************************************************
*:B122053,1

FUNCTION lfConvVar
PARAMETERS llOperate

IF llOperate= .T.
  m.CFreight = IIF(TYPE('m.CFreight') ='N', m.CFreight, VAL(m.CFreight))
  m.cWeight  = IIF(TYPE('m.cWeight')  ='N', m.cWeight, VAL(m.cWeight))
  m.cCod     = IIF(TYPE('m.cCod')     ='N', m.cCod , VAL(m.cCod))
  m.cCod_Amt = IIF(TYPE('m.cCod_Amt') ='N', m.cCod_amt, VAL(m.cCod_Amt))
  m.cDecl_Val= IIF(TYPE('m.cDecl_Val')='N', m.cDecl_Val, val(m.cDecl_Val))
ELSE
  NCFreight  = ALLTRIM(STR(m.CFreight,10,2))
  NcWeight   = ALLTRIM(STR(m.cWeight,6,2))
  NCCod      = ALLTRIM(STR(m.CCod,9,2))
  NcCod_Amt  = ALLTRIM(STR(m.Ccod_amt,9,2))
  NcDecl_Val = ALLTRIM(STR(m.cDecl_Val,9,2))

  REPLACE cFreight      with NcFreight
  REPLACE cWeight       with NcWeight
  REPLACE cCod          with NCCod
  REPLACE cCod_Amt      with NcCod_Amt
  REPLACE cDecl_Val     with NcDecl_Val
ENDIF

*-- End Of Function lfConvVar
*:*************************************************************
*: Name      : lfChkUpsSh
*: Developer : Nader Nabil NNA         
*: Date      : 07/13/2004
*: Purpose   : Delete any Piktkt Ship. Info. that has voided
*:             and let only the final info. for this piktkt.
*:*************************************************************
*: Example      : = lfChkUpsSh()
*:*************************************************************
*: Called from  : Piktkt Program.
*:*************************************************************
*: Calls        : None
*:*************************************************************
*: Passed Param.: None
*:*************************************************************
*: Return       : None
*:*************************************************************
*:B123213,1

FUNCTION lfChkUpsSh

*B129997,1 NNA 10/13/2005 (Begin) add the Piktkt No. as a Parameter to scan by it
PARAMETERS lcCurPkTKT
*B129997,1 NNA (END)

PRIVATE lcPkTkt , lnRecNo , lcTrackNo , llFound
STORE ''  TO lcPkTkt , lcTrackNo 
STORE 0   TO lnRecNo
STORE .F. TO llFound
IF !USED('ARUPSSHP')
  =gfOpenFile(gcDataDir+'ARUPSSHP',gcDataDir+'ARUPSSHP','SH')
ENDIF
SELECT ARUPSSHP
*B129997,1 NNA 10/13/2005 (Begin) Seek and Scan for a Specific Piktkt
*IF RECCOUNT()>1 
*  SCAN FOR STATUS = 'N'

*B130704,1 NNA 12/13/2005 (Begin) check for !empty(PikTkt)
*IF RECCOUNT()>1 AND SEEK(lcCurPkTKT)
IF RECCOUNT()>1 AND !EMPTY(lcCurPkTKT) AND SEEK(lcCurPkTKT)
*B130704,1 NNA (END)

  SCAN REST WHILE PIKTKT = lcCurPkTKT FOR STATUS = 'N'
*B129997,1 NNA (END)

    lnRecNo   = RECNO()
    lcPkTkt  = PIKTKT
    lcTrackNo = cTrack_NO

    *B128218,1 EIH 08/31/2005 Scan for all tracking numbers [End]
    *SCAN REST WHILE PIKTKT = lcPikTkt FOR STATUS = 'Y' AND cTrack_NO = lcTrackNo
    SCAN REST WHILE PIKTKT = lcPikTkt FOR STATUS = 'Y' 
    *B128218,1 EIH 08/31/2005 [End]

      STORE .T. TO llFound
      REPLACE Cod_Flag WITH 'X'
    ENDSCAN
    GOTO lnRecNo
    IF llFound
      REPLACE Cod_Flag WITH 'X'
    ENDIF
    STORE .F. TO llFound
  ENDSCAN
ELSE
  RETURN
ENDIF
DELETE ALL FOR Cod_Flag = 'X'
LOCATE
RETURN

*-- End Of Function lfChkUpsSh.
