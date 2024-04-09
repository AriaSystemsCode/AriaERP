*!**************************************************************************
*! Name      : BELLMAIN.PRG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : BELL'S custom Cartons' information program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*! Modifications:
*! B804354,1 ADEL 09/04/01   Let the user change the freight.
*! B605704,1 ASH  03/19/2002 Fix the bug of wrong saving address in case of DC.
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*!******************************************************************
*! Name      : lfCALLCRT
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Call Custom carton information screen.
*!******************************************************************
FUNCTION lfCALLCRT

=lfCARTON(lcCrtTmp,laData[11],laData[4],laData[1],laData[5],laData[3],m.lUpsIns,;
(m.Cod_Flag='Y'),m.SHIPAMT,m.ShipVia,laData[7],'m.Cartons','m.Weight',;
'm.Freight','m.Insur','m.Cod','m.COD_AMT')
SELECT (lcInvHdr)
=RLOCK()
GATHER MEMVAR FIELDS &lcFlFields
UNLOCK
SHOW GETS WINDOW (lcWinChrg) ONLY
=lfRefresh(lcWinChrg)


*!*****************************************************************
*! Name      : CARTON
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Invoice Cartons Details.
*!*************************************************************
*! Calls     : ARCRTBOX.SPX
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
SELECT (lcCrtTmp)
IF laScrMode[4]
  =SEEK(lcPikTkt)
  *-- get the weight ,cod ,frieght for each carton
  SUM REST Weight,Freight;
  TO       lnIWeight,lnIFreight ;
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
DO (gcScrDir+gcWinAppl+"\ARCRTBOX.SPX")
IF laScrMode[4]     && Add mode
  SELECT (lcCrtTmp)
  =SEEK(lcPikTkt)
  *B804354,1 (Begin) User can enter non-frieghted cartons.
  *  SUM REST Weight,Freight ;
  TO       &lcWeight,&lcFreight ;
  WHILE PikTkt = lcPikTkt;
  FOR Weight <> 0 AND  Freight <> 0
  SUM REST Weight,Freight ;
  TO       &lcWeight,&lcFreight ;
  WHILE PikTkt = lcPikTkt;
  FOR Weight <> 0
  *B804354,1 (End)
  *--For :  is added for ignoring incorrect cratons (not counted :  _Tally)
  &lcCartons = _TALLY   && Number of cartons
  m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod+m.Discount+m.Tax_Amt+m.nPstAmt+m.nHstAmt
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfUpsBrow
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Browse Invoice Cartons Details.
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
       Cartons :H='Carton#'         :P= '99999'  :R ,;
       Weight  :H='Weight  '        :P= '999999' :R ,;
       CTRCKNO :H='Package ID #               '  :P= 'XXXXXXXXXXXXXXXXX' :R ,;
       Freight :H='Freight     '    :P= '999999999.99' :R ;
       WINDOW ARCRTBO1   ;
       IN WINDOW ARCRTBOX;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WHEN lfShowUPS() ;
       KEY IIF(laScrMode[4],lcPikTkt,lcInvoice) ;
       TITLE lcBrowUPS

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
SHOW GETS WINDOW 'ARCRTBO2' ONLY
=lfRefresh('ARCRTBO0')
=lfRefresh('ARCRTBO2')

*!*****************************************************************
*! Name      : UPDTPACK
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Update ALPAKINF file
*!*************************************************************
FUNCTION lfUPDTPACK

PRIVATE lnAlias
lnAlias = SELECT()
=gfOpenFile(gcDataDir+'ALPAKINF',gcDataDir+'ALPAKINF','SH')
=IIF(EMPTY(&lcPakHdr..STORE),SEEK('M'+&lcPakHdr..Account,'Customer'),SEEK('S'+&lcPakHdr..Account+&lcPakHdr..STORE,'Customer'))
lcShipVia = IIF(!EMPTY(&lcPakHdr..SHIPVIA),&lcPakHdr..SHIPVIA,CUSTOMER.SHIPVIA)
*B605704,1 ASH 03/19/2002 (Begin) Fix the bug of wrong saving address in case of DC.
lcDistCntr = ""
IF !EMPTY(Customer.Dist_Ctr) 
  lcDistCntr = ALLTRIM(Customer.Dist_Ctr)
  =SEEK('S' + &lcPakHdr..Account + lcDistCntr,'Customer')        
ENDIF
*B605704,1 ASH 03/19/2002 (End)
IF !SEEK(&lcPakHdr..Pack_No)
  APPEND BLANK
ELSE
  REPLACE CTOTAL WITH 0  
ENDIF
REPLACE PIKTKT     WITH &lcPakHdr..Pack_No ,;
        ACCOUNT    WITH &lcPakHdr..ACCOUNT ,;
        STORE      WITH &lcPakHdr..STORE   ,;
        CUSTPO     WITH IIF(SEEK('O'+&lcPakHdr..Order,'OrdHdr'),ORDHDR.CUSTPO,'')  ,;        
        STNAME     WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.STNAME,CUSTOMER.STNAME)       ,;
        CADDRESS1  WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.CADDRESS1,CUSTOMER.CADDRESS1) ,;
        CADDRESS2  WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.CADDRESS2,CUSTOMER.CADDRESS2) ,;
        CADDRESS3  WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.CADDRESS3,CUSTOMER.CADDRESS3) ,;
        CADDRESS4  WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.CADDRESS4,CUSTOMER.CADDRESS4) ,;
        CADDRESS5  WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.CADDRESS5,CUSTOMER.CADDRESS5) ,;
        PHONE1     WITH CUSTOMER.PHONE1    ,;
        CSHIPVIA   WITH lcShipVia          ,;
        ALT_SHPTO  WITH ORDHDR.ALT_SHPTO

=gfAdd_Info()        
=lfUPDTOT1()
SELECT (lnAlias)

*!*****************************************************************
*! Name      : UPSHPVIA
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/05/2001
*! Purpose   : Update shipvia
*!*************************************************************
FUNCTION lfUPSHPVIA

m.shipVia =IIF(SEEK("O"+ m.Order , "ORDHDR"),ORDHDR.SHIPVIA,'')

*!*****************************************************************
*! Name      : lfCRTCHRG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/08/2001
*! Purpose   : Compute invoice chrages
*!*************************************************************
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvUpsChrg()
*!*************************************************************
FUNCTION lfCRTCHRG

=lfCRTCHRGE(llCalld,lcChrgTyp)

*!*****************************************************************
*! Name      : lfCRTCHRGE
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/08/2001
*! Purpose   : Compute invoice chrages
*!*************************************************************
*! Passed Parameters  : llCalled   : Called for recursion
*!                      lcChrgType : Charge Type: Freight, Insurance, Cod, All
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvUpsChrg()
*!*************************************************************
FUNCTION lfCRTCHRGE

PARAMETERS llCalled,lcChrgType
lcChrgType=IIF(TYPE('lcChrgType')='C',lcChrgType,'A')
WAIT 'Computing Invoice Charges...' WINDOW NOWAIT
SELECT (lcInvHdr)
SET ORDER TO TAG CONSOL
lcKey = Account+cDivision+cCurrCode
IF Consol='Y'
  =SEEK('N'+lcKey)
  SCAN REST WHILE Consol+Account+cDivision+cCurrCode='N'+lcKey
    SCATTER FIELDS &lcScFields TO laData
    *B804354,1 (Begin) Empty it.
    *laData[3] = ' '
    *B804354,1 (End)
    SCATTER MEMVAR FIELDS SHIPAMT,DISCPCNT,TAX_RATE,cTaxRule,nPstRate,;
    ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,nPstAmt,nHstAmt,TOTALCHG
    =lfCRTCHRGE(.T.,lcChrgType)
  ENDSCAN
  =SEEK('Y'+lcKey)
  =RLOCK()
  lnTaxAmount = IIF(UPPER(ALLTRIM(gcContCode))='USA',nTaxDue,ShipAmt)
  lnGstChrg = IIF(laSetups[11,2]='M',lnTaxAmount,;
                  lnTaxAmount+Freight+Insur+COD)-DiscPcnt*lnTaxAmount/100
  lnPstChrg = ShipAmt+Freight+Insur+COD+Discount+IIF(VAL(m.cTaxRule)=1,0,Tax_Amt)
  REPLACE lCompUps WITH .F.        ,;
          Cod_Flag WITH m.Cod_Flag ,;
          lUpsIns  WITH m.lUpsIns  ,;
          Tax_Rate WITH IIF(lnGstChrg=0,0,Tax_Amt*100/lnGstChrg) ,;
          nPstRate WITH IIF(lnPstChrg=0,0,nPstAmt*100/lnPstChrg)
  UNLOCK
  SCATTER MEMVAR FIELDS &lcFlFields
  SCATTER FIELDS &lcScFields TO laData
ELSE
  DO gpCrtChrg WITH ;
  laData[4],laData[5],laData[1],ladata[3],laData[7],m.SHIPAMT,m.Cod_Flag='Y',;
  m.lUpsIns,m.DISCPCNT,m.TAX_RATE,m.cTaxRule,m.nPstRate,lcUpsBox,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT','m.TAX_AMT',;
  'm.nPstAmt',IIF(UPPER(ALLTRIM(gcContCode))='USA',&lcInvHdr..nTaxDue,;
  m.SHIPAMT),lcChrgType
  m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod+m.Discount+m.Tax_Amt+m.nPstAmt+m.nHstAmt
  SELECT (lcInvHdr)
  lnFreight  = Freight
  lnInsur    = INSUR
  lnCod      = COD
  lnOldCart  = CARTONS
  lnWeight   = WEIGHT
  lnTaxAmnt  = TAX_AMT
  lnPstAmt   = nPstAmt
  lnCodAmt   = Cod_amt
  lnTotalChg = TotalChg
  lnRecNo = RECNO()
  IF SEEK('Y'+lcKey)
    =RLOCK()
    REPLACE Freight  WITH Freight - lnFreight + m.Freight ,;
            INSUR    WITH INSUR   - lnInsur   + m.INSUR   ,;
            COD      WITH COD     - lnCod     + m.COD     ,;
            CARTONS  WITH CARTONS - lnOldCart + m.CARTONS ,;
            WEIGHT   WITH WEIGHT  - lnWeight  + m.WEIGHT  ,;
            TAX_AMT  WITH TAX_AMT - lnTaxAmnt + m.TAX_AMT ,;
            nPstAmt  WITH nPstAmt - lnPstAmt  + m.nPstAmt ,;
            Cod_amt  WITH Cod_Amt - lnCodAmt  + m.Cod_amt ,;
            TotalChg WITH TotalChg- lnTotalChg+ m.TotalChg
    UNLOCK
  ENDIF
  GO lnRecNo
  m.lCompUps = .F.
  =RLOCK()
  GATHER MEMVAR FIELDS SHIPAMT,Cod_Flag,lUpsIns,DISCPCNT,TAX_RATE,cTaxRule,;
  nPstRate,ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,;
  nPstAmt,nHstAmt,TOTALCHG,lCompUps
  UNLOCK
ENDIF
IF !llCalled
  SET ORDER TO TAG (lcInvHdr)
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
  WAIT CLEAR
ENDIF


*!*************************************************************
*! Name      : 
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/08/2001
*! Purpose   : Caculate invoice charges
*!*************************************************************
*! Calls     : lfGetFright(),lfCrtBox()
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
*!
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
*! Example   :  =gpCrtChrg()
*!*************************************************************
PROCEDURE gpCrtChrg

PARAMETERS lcAccount,lcStore,lcOrderNo,lcPikTKt,lcWareCode,lnShipAmt,llCod,;
           llupsinsur,lnDscPrcnt,lcTaxRate,lcPstRule,lnPstRate,lcUpsFile,;
           lcShipVia,lcCOD_AMT,lcFreight,lcInsur,lcCOD,lcCartons,lcWeight,;
           lcTaxAmnt,lcPstAmnt,lnTaxDue,lcChrgType
PRIVATE laShpViaFld,lcUpsType,lnCodCharge,lnInsCharge,lnFxdPrcnt,lcUpsIncr,;
        llCompTax,lcTaxRule,getf,llUpsBox
IF lnShipAmt = 0
  RETURN           
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
*-- Calculate freights             
*-- Add records for each carton in the UPSBox file
=lfCrtBox(lcPikTKt,lcUpsFile)
=lfGetFright(SUBSTR(CUSTOMER.caddress5,1,3),lcUpsType  ,;
             llCod,llupsinsur,lcFreight,lcInsur,lcCOD,lcCartons,lcWeight,;
             lnCodCharge,lnInsCharge,lnShipAmt,lcChrgType)
IF llCod
  *-- calculate the cash on delivery amount
  &lcCOD_AMT = lnShipAmt+&lcFreight+&lcInsur+&lcCOD-lnShipAmt*lnDscPrcnt/100
  *-- ladata[1] = Invoice Number
ENDIF
IF llCompTax
*-- CALCULATE TAXES
  &lcTaxAmnt = ROUND(lcTaxRate*(IIF(lcTaxRule='M',lnTaxDue,lnTaxDue+;
               &lcFreight+&lcInsur+&lcCOD)-ABS(lnTaxDue*lnDscPrcnt/100) )/100,2)
  *-- Calculate pst by the canadian method or the american one
  IF llCod
    *-- add the tax amount and the  pst amount to the cod amount  
    &lcCOD_AMT = &lcCOD_AMT+&lcTaxAmnt
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfGetFright
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
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

PARAMETERS lcToZip,lcUpsType,llCod,llupsinsur,lcFreight,lcInsur,;
           lcCOD,lcCartons,lcWeight,lnCodCharge,lnInsCharge,lnTotShpAmt,lcChrgType
           
PRIVATE lcCurrExac,lcCurrNear

IF INLIST(lcChrgType,'A','C')
*--  Recalculate the cod 
  &lcCOD   = IIF(llCod,IIF('USUPS' $ lcUpsType,lnCodCharge*&lcCartons,&lcCOD),0)
ENDIF
IF INLIST(lcChrgType,'A','I')
*--  Recalculate the insurance
  &lcInsur = IIF(llupsinsur,IIF('USUPS' $ lcUpsType AND lnTotShpAmt >=100,;
                 FLOOR(lnTotShpAmt/100)*lnInsCharge,&lcCOD),0)
ENDIF
*B804354,1 (Begin) Calc cartons charges.
IF VARREAD() <> 'FREIGHT' AND VARREAD() <> 'WEIGHT'
  &lcCartons = 0
  IF USED(lcCrtTmp)
    SELECT (lcCrtTmp)
  ELSE
    IF !USED('ARCLPSHP')
      =gfOpenFile(gcDataDir+'arclpshp',gcDataDir+'arclpshp','SH')
    ENDIF
    SELECT ARCLPSHP
  ENDIF  
  =SEEK(lcPikTkt)
  *-- get the weight ,cod ,frieght for each carton
  SUM REST Weight,Freight;
     TO   &lcWeight,&lcFreight ;
     WHILE PikTkt = lcPikTkt
  =SEEK(lcPikTkt)
  COUNT REST WHILE PikTkt = lcPikTkt TO &lcCartons
ENDIF  
*B804354,1 (End)
&lcCOD   = IIF(llCod,lnCodCharge*&lcCartons,&lcCOD)
&lcInsur = IIF(lnTotShpAmt >=100 .AND. llupsinsur,;
               FLOOR(lnTotShpAmt/100)*lnInsCharge,&lcInsur)
RETURN

*!*************************************************************
*! Name      : lfCrtBox
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
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
*! Example   : =lfCrtBox()
*!*************************************************************
FUNCTION lfCrtBox

PARAMETERS lcPikTKt,lcUpsFile

IF TYPE('lcInvHdr') = 'U'
  RETURN
ENDIF
lnAlias = SELECT()
IF !USED(lcCrtTmp)
  IF USED(lcUpsFile)
    USE IN (lcUpsFile)
  ENDIF
  =gfOpenFile(gcDataDir+'arclpshp',gcDataDir+'arclpshp','SH')
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'OldCrtNo'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 5
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'cSel'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0
  =gfCrtTmp(lcCrtTmp,@laFileStru,[PikTkt+STR(CARTONS,5)],lcCrtTmp)
ENDIF
*--  Get the no of carton from back_hdr incase packed pikTkt.
IF (!EMPTY(lcPikTkt) AND SEEK(lcPikTkt,"arclpshp"))
  lcOldPik = lcPikTkt
  lnCartNo = 0    && Carton no 
  lnCrtWgh = 0    && Carton Weight
  &lcFreight = 0
  &lcWeight  = 0
  &lcCartons = 0
  SELECT (lcCrtTmp)
  REPLACE ALL CSEL  WITH 'N'
  DELETE ALL
  IF ORDHDR.MULTI = 'Y'
    SELECT (lcInvHdr)
    *B804354,1 (Begin) Use Recno.
    *lcKey = EVAL(KEY())
    lnRecNo = RECNO()
    *B804354,1 (End)
    SCAN
      lcPikTkt = PIKTKT
      SELECT arclpshp
      =SEEK(lcPikTkt)
      SCAN WHILE PikTkt = lcPikTkt FOR STATUS <> 'V'
        SELECT (lcCrtTmp)
        APPEND BLANK    && New line for each Carton
        SET DELE ON  
        REPLACE PIKTKT   WITH arclpshp.PIKTKT ,;
                WEIGHT   WITH arclpshp.WEIGHT ,;
                FREIGHT  WITH arclpshp.FREIGHT,;
                STATUS   WITH arclpshp.STATUS ,;
                CARTONS  WITH arclpshp.CARTONS,;
                CTRCKNO  WITH arclpshp.CTRCKNO,;
                OldCrtNo WITH STR(arclpshp.CARTONS,5),;
                cSel     WITH 'S'
        &lcFreight = &lcFreight + freight 
        &lcWeight  = &lcWeight  + Weight
        &lcCartons = &lcCartons + 1
      ENDSCAN
    ENDSCAN
    SELECT (lcCrtTmp)
    *B804354,1 (Begin) Calculate feeight and weight for each piktkt.
    *=SEEK(lcOldPik)
    *COUNT REST WHILE PikTkt = lcOldPik TO &lcCartons
    IF !EMPTY(laData[3])
      =SEEK(lcOldPik)
      COUNT REST WHILE PikTkt = lcOldPik TO &lcCartons
      =SEEK(lcOldPik)
      SUM REST Weight,Freight ;
      TO       &lcWeight,&lcFreight ;
               WHILE PikTkt = lcOldPik
    ENDIF
    *B804354,1 (End)
    SELECT (lcInvHdr)
    *B804354,1 (Begin) Use Recno
    *=SEEK(lcKey)
    GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,RECNO())
    *B804354,1 (End)
    lcPikTkt = lcOldPik
  ELSE
    SELECT arclpshp
    SCAN WHILE PikTkt = lcPikTkt FOR STATUS <> 'V'
      SELECT (lcCrtTmp)
      APPEND BLANK    && New line for each Carton
      SET DELE ON  
      REPLACE PIKTKT   WITH arclpshp.PIKTKT ,;
              WEIGHT   WITH arclpshp.WEIGHT ,;
              FREIGHT  WITH arclpshp.FREIGHT,;
              STATUS   WITH arclpshp.STATUS ,;
              CARTONS  WITH arclpshp.CARTONS,;
              CTRCKNO  WITH arclpshp.CTRCKNO,;
              OldCrtNo WITH STR(arclpshp.CARTONS,5),;
              cSel     WITH 'S'
      &lcFreight = &lcFreight + freight 
      &lcWeight  = &lcWeight  + Weight
      &lcCartons = &lcCartons + 1
    ENDSCAN
  ENDIF
ELSE  
  *-- if there is no pack lines
  SELECT (lcCrtTmp)
  =SEEK(lcPikTKt)
  DELETE REST WHILE PikTkt+STR(CARTONS,5)= lcPikTKt
  FOR lnCount = 1 TO m.Cartons
    IF !SEEK(lcPikTKt+STR(lnCount,4))
      *-- if not  found  in the upsfile then add a new line
      *-- else replace it with the new values  
      APPEND BLANK
      REPLACE PIKTKT  WITH lcPIKTKT ,;
              WEIGHT  WITH m.Weight/m.Cartons ,;
              FREIGHT WITH m.Freight/m.Cartons,;
              CARTONS WITH lnCount,;
              cSel    WITH 'N'
    ENDIF 
  ENDFOR
ENDIF
SELECT (lnAlias)
RETURN

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
  IF VARREAD() = 'FREIGHT' OR VARREAD() = 'WEIGHT'
    =lfGetFright(SUBSTR(CUSTOMER.caddress5,1,3),lcUpsType,llCod,llUpsInsur,;
    'm.Freight','m.Insur','m.Cod','lnOneCrtn','m.Weight',lnCodCharge,lnInsCharge,IIF(llupsinsur,lnShipAmt/lnCartons,0),'A')
  ENDIF  
  SELECT (lcCrtTmp)
  lcKey = EVAL(KEY())
  =RLOCK()
  GATHER MEMVAR
  UNLOCK
  =SEEK(lcPikTkt)
  *-- get the weight ,cod ,frieght for each carton
  SUM REST Weight,Freight;
      TO   lnIWeight,lnIFreight ;
      WHILE PikTkt = lcPikTkt
  =SEEK(lcKey)
  REPLACE cSel WITH IIF(cSel <> 'N','M',cSel)
  =lfShowUps()
ENDIF

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
  ON KEY LABEL TAB     DO lpTab WITH 'ARCRTBO2','m.Weight'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'ARCRTBO2','pbClsUps'
ENDIF  
RETURN .F.

*!*****************************************************************
*! Name      : ENBLCRT
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/10/2001
*! Purpose   : Always enable Cartons button.
*!*************************************************************
FUNCTION lfENBLCRT

SHOW GET pbUpsBox ENABLE
*B804354,1 (Begin) User can enter non-frieghted cartons.
*SHOW GET m.Freight DISABLE
*B804354,1 (End)

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

SELECT (lcCrtTmp)
=SEEK(lcPikTkt)
COUNT REST TO lnNBoxNo ;
WHILE PikTkt+STR(CARTONS,5) = lcPikTkt
INSERT INTO   (lcCrtTmp) (PIKTKT,Cartons,cSel);
       VALUES            (lcPikTkt,lnNBoxNo+1,'N')
lnTotCrt = lnNBoxNo+1
SHOW GET m.Weight  ENABLE
SHOW GET m.Freight ENABLE
SHOW GET m.CTRCKNO ENABLE
SHOW GET pbRemCrtn ENABLE
SHOW GET lnTotCrt
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

lnIWeight   = lnIWeight   - m.Weight
lnIFreight  = lnIFreight  - m.Freight
SELECT (lcCrtTmp)
*REPLACE OldCrtNo WITH IIF(cSel <> 'N',STR(CARTONS,5),OldCrtNo)
DELETE
lnNBoxNo = 1
=SEEK(lcPikTkt)
SCAN REST WHILE PikTkt+STR(CARTONS,5) = lcPikTkt
  =RLOCK()
  REPLACE Cartons WITH lnNBoxNo
  UNLOCK
  lnNBoxNo = lnNBoxNo + 1
ENDSCAN
lnTotCrt = lnNBoxNo-1
SHOW GET lnTotCrt
IF !SEEK(lcPikTkt)
  SHOW GETS WINDOW 'ARCRTBO2' DISABLE ONLY
  SHOW GET ibBrowUps ENABLE
  SHOW GET pbNewCrtn ENABLE
  SHOW GET pbcLSuPS  ENABLE
ENDIF
*-- Show function for Cartons details screen
=lfShowUps()

*!******************************************************************
*! Name      : lfCHKINV
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/15/2001
*! Purpose   : Check invoice.
*!******************************************************************
FUNCTION lfCHKINV

IF &lcSaveInv AND UPPER(ALLTRIM(gcContCode))<>'ENG' .AND. &lcInvFile..lCompUps
  *--Calculate non english charges 
  SELECT (lcInvFile)
  SCATTER MEMVAR 
  *-- Calculate charges
  DO gpCrtChrg WITH ;
  m.Account,m.Store,m.Order,m.PikTkt,m.cWareCode,m.ShipAmt,m.Cod_Flag='Y',;
  m.lUpsIns,m.DISCPCNT,m.TAX_RATE,m.cTaxRule,m.nPstRate,lcUpsFile,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT',;
  'm.TAX_AMT','m.nPstAmt',m.nTaxDue,'A'
  m.nHstAmt = ROUND((m.nHstRate*m.ShipAmt)/100,2)
  m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod-ABS(m.ShipAmt*m.DiscPcnt/100)+;
               m.Tax_Amt+m.nPstAmt+m.nHstAmt
  SELECT (lcInvFile)
  *B804354,1 (Begin) May the user cancel to Proceed to save and go to charge folder where (lcCrtTmp)
  *B804354,1         not yet ctreated.
  *m.lCompUps = .F.
  *B804354,1 (End)
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

*!******************************************************************
*! Name      : lfSAVINV
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/15/2001
*! Purpose   : Save invoice.
*!******************************************************************
FUNCTION lfSAVINV

*-- Update the arclpshp File 
lnNoOfCrt = 0
lcPiktkt = &lcHdrFile..PIKTKT
IF !EMPTY(lcCrtTmp) AND USED(lcCrtTmp)
  WAIT 'Updating Carton Box File ' WINDOW NOWAIT
  SELECT (lcCrtTmp)
  LOCATE
  SET DELETE OFF
  =SEEK(lcPiktkt)
  *B804354,1 (Begin) User can enter non-frieghted cartons.
  *SCAN REST WHILE PIKTKT = lcPiktkt  FOR WEIGHT <> 0 AND FREIGHT <> 0
  SCAN REST WHILE PIKTKT = lcPiktkt  FOR WEIGHT <> 0
  *B804354,1 (End)
    DO CASE
      CASE DELETED() AND cSel <> 'N' AND SEEK(lcPiktkt+OldCrtNo,'arclpshp')
        SELECT arclpshp
        =RLOCK()
        DELETE
        UNLOCK
      CASE !DELETED() AND cSel = 'S'
         SELECT arclpshp
         IF SEEK(lcPiktkt+&lcCrtTmp..OldCrtNo) AND !DELETED()
           REPLACE arclpshp.CARTONS WITH &lcCrtTmp..CARTONS
         ELSE
           SELECT (lcCrtTmp)
           SCATTER MEMVAR
           INSERT INTO 'arclpshp' FROM MEMVAR
         ENDIF  
      CASE !DELETED() AND cSel <> 'S'
        IF cSel = 'N'
          SCATTER MEMVAR
          INSERT INTO 'arclpshp' FROM MEMVAR          
        ELSE
          IF SEEK(lcPiktkt+OldCrtNo,'arclpshp')
            SCATTER MEMVAR
            m.Cartons = VAL(OldCrtNo)
            SELECT arclpshp
            =RLOCK()
            GATHER MEMVAR
            UNLOCK
          ENDIF
        ENDIF
    ENDCASE
  ENDSCAN
  SET DELETE ON
ENDIF

*!*****************************************************************
*! Name      : ENBLCAR
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/10/2001
*! Purpose   : Always enable Cartons button.
*!*************************************************************
FUNCTION lfENBLCAR

SHOW GET pbUpsBox ENABLE

*!******************************************************************
*! Name      : lfCARBOX
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Call Custom carton information screen.
*!******************************************************************
FUNCTION lfCARBOX

=lfCARTBOX(laData[1],laData[2],laData[3],laData[5],laData[30],llupsinsur,llCod,;
          laData[31],laData[10],laData[21],'laData[25]',;
          'laData[26]','laData[34]','laData[35]','laData[36]','laData[28]')

*!*****************************************************************
*! Name      : lfCARTBOX
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Invoice Cartons Details.
*!*************************************************************
*! Calls     : ARCRTBOX.SPX
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
=gfOpenFile(gcDataDir+'arclpshp',gcDataDir+'arclpshp','SH')
=SEEK(INVHDR.PikTkt)
*-- get the weight ,cod ,frieght for each carton

SUM REST Weight,Freight;
TO       lnIWeight,lnIFreight ;
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
DO (gcScrDir+gcWinAppl+"\ARCRTBOX.SPX")
lcinvoice = lcOldInv
SELECT (lnAlias)

*!*****************************************************************
*! Name      : UPDPACK
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Update ALPAKINF file
*!*************************************************************
FUNCTION lfUPDPACK

PRIVATE lnAlias
lnAlias = SELECT()
=gfOpenFile(gcDataDir+'ALPAKINF',gcDataDir+'ALPAKINF','SH')
=IIF(EMPTY(laData[5]),SEEK('M'+laData[4],'Customer'),SEEK('S'+laData[4]+laData[5],'Customer'))
lcShipVia = laData[7]
*B605704,1 ASH 03/19/2002 (Begin) Fix the bug of wrong saving address in case of DC.
lcDistCntr = ""
IF !EMPTY(Customer.Dist_Ctr) 
  lcDistCntr = ALLTRIM(Customer.Dist_Ctr)
  =SEEK('S' + laData[4] + lcDistCntr,'Customer')        
ENDIF
*B605704,1 ASH 03/19/2002 (End)
IF !SEEK(laData[1])
  APPEND BLANK
ELSE
  REPLACE CTOTAL WITH 0  
ENDIF
REPLACE PIKTKT     WITH laData[3]          ,;
        ACCOUNT    WITH laData[4]          ,;
        STORE      WITH laData[5]          ,;
        CUSTPO     WITH IIF(SEEK('O'+laData[2],'OrdHdr'),ORDHDR.CUSTPO,'')         ,;        
        STNAME     WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.STNAME,CUSTOMER.STNAME)       ,;
        CADDRESS1  WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.CADDRESS1,CUSTOMER.CADDRESS1) ,;
        CADDRESS2  WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.CADDRESS2,CUSTOMER.CADDRESS2) ,;
        CADDRESS3  WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.CADDRESS3,CUSTOMER.CADDRESS3) ,;
        CADDRESS4  WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.CADDRESS4,CUSTOMER.CADDRESS4) ,;
        CADDRESS5  WITH IIF(OrdHdr.Alt_ShpTo ,OrdHdr.CADDRESS5,CUSTOMER.CADDRESS5) ,;
        PHONE1     WITH CUSTOMER.PHONE1    ,;
        CSHIPVIA   WITH lcShipVia          ,;
        ALT_SHPTO  WITH ORDHDR.ALT_SHPTO
=gfAdd_Info()     
SELECT (lnAlias)

*!*****************************************************************
*! Name      : lfUPDTOT
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Update Total shipment 
*!*************************************************************
FUNCTION lfUPDTOT

IF USED('ALPAKINF') AND SEEK(laData[1],'ALPAKINF')
  REPLACE ALPAKINF.CTOTAL WITH ALPAKINF.CTOTAL + ORDLINE.TOTPIK*ORDLINE.PRICE
ENDIF

*!*****************************************************************
*! Name      : lfUPDTOT1
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Update Total shipment 
*!*************************************************************
FUNCTION lfUPDTOT1

lnAlias = SELECT()
SELECT ORDLINE
lcKey = EVAL(KEY())
=SEEK('O'+m.Order)
SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+m.Order;
          FOR !EMPTY(PIKTKT) AND TOTPIK > 0
  IF USED('ALPAKINF') AND SEEK(&lcPakHdr..Pack_No,'ALPAKINF')
    REPLACE ALPAKINF.CTOTAL WITH ALPAKINF.CTOTAL + ORDLINE.TOTPIK*ORDLINE.PRICE
  ENDIF
ENDSCAN
SELECT ORDLINE
=SEEK(lcKey)
SELECT (lnAlias)

*!*****************************************************************
*! Name      : lfCloseB
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Check for lines with Freight or Weight = 0
*!*************************************************************
FUNCTION lfCloseB

IF (!laScrMode[3] AND !laScrMode[4]) OR (TYPE('lcCrtTmp') = 'U')
  RETURN
ENDIF 
lnAlias = SELECT()
SELECT (lcCrtTmp)
lcKey = EVAL(KEY())
=SEEK(lcPikTkt)
lnNoOfBad = 0
lnNoOfCrt = 0
SCAN REST WHILE PikTkt+STR(CARTONS,5) = lcPikTkt
  *B804354,1 (Begin) User can enter non-frieghted cartons.
  *IF  (WEIGHT = 0 OR FREIGHT = 0)
  IF  (WEIGHT = 0)
  *B804354,1 (End)
    lnNoOfBad = lnNoOfBad + 1
  ENDIF  
  lnNoOfCrt = lnNoOfCrt +1
ENDSCAN
IF lnNoOfBad <> 0
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Cartons with zero value in weight will be ignored upon saving.")
ENDIF  
&lcCartons = lnNoOfCrt
SELECT ORDLINE
=SEEK(lcKey)
SELECT (lnAlias)

*!*****************************************************************
*! Name      : lfModiFrt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/05/2001
*! Purpose   : Modify Freight
*!*************************************************************
*! Refer to :B804354,1
FUNCTION lfModiFrt

lnAlias = SELECT()
SELECT (lcCrtTmp)
=SEEK(laData[3])
IF m.FREIGHT = 0
  REPLACE ALL WHILE PikTkt+STR(CARTONS,5) = laData[3] FREIGHT WITH 0 ,;
                                                      cSel WITH IIF(cSel <> 'N','M',cSel)
ELSE
  IF lnOldChrg <> m.FREIGHT
    COUNT FOR PikTkt = laData[3] TO lnCrtOfNo
    =SEEK(laData[3])
    REPLACE REST WHILE PikTkt+STR(CARTONS,5) = laData[3] FREIGHT WITH m.FREIGHT/lnCrtOfNo,;
                                                      cSel WITH IIF(cSel <> 'N','M',cSel)
  ENDIF                                                    
ENDIF
SELECT (lnAlias)