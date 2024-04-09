*!**************************************************************************
*! Name      : OGIOMAIN.PRG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/08/2001
*! Purpose   : Ogio Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C102424,1
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*!**************************************************************************
*! Name      : lfUpdShp
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/08/2001
*! Purpose   : Update shipmemnt file  from Al by Order.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfUpdShp

*--If not generating a Piktkt no, return
IF (!llGenPkTk AND cStatus <> 'R') OR (!EMPTY(cStatus) AND cStatus <> 'R');
   OR (cStatus = 'R' AND '*' $ lcCrPkTkt)
  RETURN
ENDIF

PRIVATE lnAlias
lnAlias = SELECT()

IF !USED('ALSHPINF')
  =gfOpenFile(gcDataDir+"ALSHPINF", "ALSHPINF", "SH")  
ENDIF
SELECT ALSHPINF
IF !SEEK(IIF(cStatus = 'R',lcCrPkTkt,lcPikTktNo),'ALSHPINF')
  APPEND BLANK 
ELSE
  IF cStatus = 'R' AND SEEK(laData[1] + IIF(cStatus = 'R',lcCrPkTkt,lcPikTktNo),'PIKTKT');
    AND PIKTKT.STATUS = 'X'
    DELETE
  ENDIF  
  SELECT (lnAlias) 
  RETURN
ENDIF  
STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
STORE '' TO lcWName,lcWAddr1,lcWAddr2,lcWAddr3,lcWAddr4,lcWAddr5,lcWAddr6,lcUpsCode
=lfGetAddr('O')
SELECT ALSHPINF
REPLACE PIKTKT     WITH IIF(cStatus = 'R',lcCrPkTkt,lcPikTktNo),;
        STATUS     WITH 'O',;
        ORDER      WITH M.ORDER,;
        ACCOUNT    WITH laData[2],;
        STORE      WITH M.STORE,;
        CUPSCODE   WITH ALLTRIM(lcUpsCode)
REPLACE CUPSTYPE   WITH lfGetTypeD(ALLTRIM(lcUpsCode)),;
        STNAME     WITH lcStName,;
        CSTADDR1   WITH lcStAddr1,;
        CSTADDR2   WITH lcStAddr2,;
        CSTCITY1   WITH lcStAddr3,;
        CSTSTAT1   WITH lcStAddr4,;
        CSTZIP1    WITH lcStAddr5,;
        CSTCOUNTRY WITH lcStAddr6,;
        CSTPHONE   WITH IIF(OrdHdr.Alt_ShpTo,'',CUSTOMER.PHONE1)
REPLACE cSFNAME    WITH lcWName,;
        CSFADDR1   WITH lcWAddr1,;
        CSFADDR2   WITH lcWAddr2,;
        CSFCITY    WITH lcWAddr3,;
        CSFSTATE   WITH lcWAddr4,;
        CSFZIP     WITH lcWAddr5,;
        CSFCOUNTRY WITH lcWAddr6,;
        CSFPHONE   WITH WAREHOUS.CPHONE,;
        CUPSACCT   WITH gfGetMemVar('XUPSACCT')
=gfAdd_Info()

SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfGetAddr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/08/2001
*! Purpose   : Get address.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfGetAddr
PARAMETERS lcFrom

IF OrdHdr.Alt_ShpTo
  lcStName  = OrdHdr.STNAME
  lcStAddr1 = OrdHdr.cAddress1
  lcStAddr2 = OrdHdr.cAddress2
  lcStAddr3 = OrdHdr.cAddress3
  lcStAddr4 = OrdHdr.cAddress4
  lcStAddr5 = OrdHdr.cAddress5
ELSE
  SELECT CUSTOMER
  DO CASE
    CASE lcFrom = 'O'
      =IIF(EMPTY(M.Store),SEEK('M'+laData[2],'Customer'),SEEK('S'+laData[2]+m.Store,'Customer'))
    CASE lcFrom = 'S'
      =IIF(EMPTY(ORDLINE.Store),SEEK('M'+ORDLINE.Account,'Customer'),SEEK('S'+ORDLINE.Account+ORDLINE.Store,'Customer'))    
    CASE lcFrom = 'A'
      =IIF(EMPTY(&lcTmpOrdLn..Store),SEEK('M'+&lcTmpOrdLn..Account,'Customer'),SEEK('S'+&lcTmpOrdLn..Account+&lcTmpOrdLn..Store,'Customer'))
  ENDCASE  
  IF !EMPTY(DIST_CTR)
    =SEEK('S'+DIST_CTR) 
  ENDIF
  lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
  lcStAddr1 = Customer.cAddress1
  lcStAddr2 = Customer.cAddress2
  lcStAddr3 = Customer.cAddress3
  lcStAddr4 = Customer.cAddress4
  lcStAddr5 = Customer.cAddress5
  lcStAddr6 = Customer.cAddress6
ENDIF
DO CASE
  CASE lcFrom  = 'O'
    lcWareCode = laWareHous[puWareHous, 2] 
  CASE lcFrom  = 'S'
    lcWareCode = lcPikWare 
  CASE lcFrom  = 'A' 
    lcWareCode = &lcTmpOrdLn..cWareCode 
ENDCASE
IF !USED('WAREHOUS')
  =gfOpenFile(gcDataDir+"WAREHOUS", "Warehous", "SH")
ENDIF
SELECT WAREHOUS
IF SEEK(lcWareCode)
  lcWName= SUBSTR(CDESC,1,30)
  lcWAddr1 = cAddress1
  lcWAddr2 = cAddress2
  lcWAddr3 = cAddress3
  lcWAddr4 = cAddress4
  lcWAddr5 = cAddress5
  lcWAddr6 = cAddress5
ENDIF

lcUpsCode = ''
DIMENSION laUpsCode[1,2] 
laUpsCode[1,1] = 'CUPS      '     && Array to get the Division long name
laUpsCode[1,2] = 'lcUpsCode'
=gfRltFld(OrdHdr.ShipVia,@laUpsCode,'SHIPVIA')


*!**************************************************************************
*! Name      : lfGetTypeD
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/08/2001
*! Purpose   : Get UPS Types.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfGetTypeD
PARAMETERS lcType

DO CASE 
  CASE lcType = 'USUPSN'
    RETURN 'Next Day Air'
  CASE lcType = 'USUPST'
    RETURN 'Next Day Air Saver'
  CASE lcType = 'USUPSE'
    RETURN '2ND Day Air AM'
  CASE lcType = 'USUPS2'
    RETURN '2ND Day Air'
  CASE lcType = 'USUPS3'
    RETURN '3 Day Select'
  CASE lcType = 'USUPSG'
    RETURN 'Ground'
  OTHERWISE
    RETURN ''
ENDCASE


*!**************************************************************************
*! Name      : lfUpdShps
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/08/2001
*! Purpose   : Update shipmemnt file from Al by Style.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfUpdShps

PRIVATE lnAlias
lnAlias = SELECT()
IF !USED('ALSHPINF')
  =gfOpenFile(gcDataDir+"ALSHPINF", "ALSHPINF", "SH")  
ENDIF
DO CASE
  *--If Allocat not Pick return.
  CASE lnProcess = 4
    SELECT (lnAlias)
    RETURN
  *--If Pick.
  CASE lnProcess = 1
    SELECT ALSHPINF
    IF !SEEK(lcPikTkt,'ALSHPINF')
      APPEND BLANK 
    ENDIF  
    STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
    STORE '' TO lcWName,lcWAddr1,lcWAddr2,lcWAddr3,lcWAddr4,lcWAddr5,lcWAddr6,lcUpsCode
    =lfGetAddr('S')
    SELECT ALSHPINF
    REPLACE PIKTKT     WITH lcPikTkt,;
            STATUS     WITH 'O',;
            ORDER      WITH ORDLINE.ORDER,;
            ACCOUNT    WITH ORDLINE.ACCOUNT,;
            STORE      WITH ORDLINE.STORE,;
            CUPSCODE   WITH ALLTRIM(lcUpsCode)
    REPLACE CUPSTYPE   WITH lfGetTypeD(ALLTRIM(lcUpsCode)),;
            STNAME     WITH lcStName,;
            CSTADDR1   WITH lcStAddr1,;
            CSTADDR2   WITH lcStAddr2,;
            CSTCITY1   WITH lcStAddr3,;
            CSTSTAT1   WITH lcStAddr4,;
            CSTZIP1    WITH lcStAddr5,;
            CSTCOUNTRY WITH lcStAddr6,;
            CSTPHONE   WITH IIF(OrdHdr.Alt_ShpTo,'',CUSTOMER.PHONE1)
    REPLACE cSFNAME    WITH lcWName,;
            CSFADDR1   WITH lcWAddr1,;
            CSFADDR2   WITH lcWAddr2,;
            CSFCITY    WITH lcWAddr3,;
            CSFSTATE   WITH lcWAddr4,;
            CSFZIP     WITH lcWAddr5,;
            CSFCOUNTRY WITH lcWAddr6,;
            CSFPHONE   WITH WAREHOUS.CPHONE,;
            CUPSACCT   WITH gfGetMemVar('XUPSACCT')
     =gfAdd_Info()
  *--If Release.     
  CASE lnProcess = 3 AND PADR(lcPikTkt,6) <> "******"
    IF SEEK(lcPikTkt,'ALSHPINF') AND SEEK(lcPikTkt,'PIKTKT') AND PIKTKT.STATUS = 'X'
      SELECT ALSHPINF
      DELETE
    ENDIF  
ENDCASE
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfUpdShpA
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/08/2001
*! Purpose   : Update shipmemnt file from Automatic Al.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfUpdShpA

PRIVATE lnAlias
lnAlias = SELECT()

IF !USED('ALSHPINF')
  =gfOpenFile(gcDataDir+"ALSHPINF", "ALSHPINF", "SH")  
ENDIF

IF llRelease
  *--Release
  IF SEEK(Piktkt.PikTkt,'ALSHPINF')
    SELECT ALSHPINF
    DELETE
  ENDIF
  SELECT (lnAlias)   
  RETURN
ENDIF   

SELECT ALSHPINF
IF !SEEK(lcPikTkt,'ALSHPINF')
  APPEND BLANK 
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  STORE '' TO lcWName,lcWAddr1,lcWAddr2,lcWAddr3,lcWAddr4,lcWAddr5,lcWAddr6,lcUpsCode
  =lfGetAddr('A')
  SELECT ALSHPINF
  REPLACE PIKTKT     WITH lcPikTkt,;
          STATUS     WITH 'O',;
          ORDER      WITH ORDLINE.ORDER,;
          ACCOUNT    WITH ORDLINE.ACCOUNT,;
          STORE      WITH ORDLINE.STORE,;
          CUPSCODE   WITH ALLTRIM(lcUpsCode)
  REPLACE CUPSTYPE   WITH lfGetTypeD(ALLTRIM(lcUpsCode)),;
          STNAME     WITH lcStName,;
          CSTADDR1   WITH lcStAddr1,;
          CSTADDR2   WITH lcStAddr2,;
          CSTCITY1   WITH lcStAddr3,;
          CSTSTAT1   WITH lcStAddr4,;
          CSTZIP1    WITH lcStAddr5,;
          CSTCOUNTRY WITH lcStAddr6,;
          CSTPHONE   WITH IIF(OrdHdr.Alt_ShpTo,'',CUSTOMER.PHONE1)
  REPLACE cSFNAME    WITH lcWName,;
          CSFADDR1   WITH lcWAddr1,;
          CSFADDR2   WITH lcWAddr2,;
          CSFCITY    WITH lcWAddr3,;
          CSFSTATE   WITH lcWAddr4,;
          CSFZIP     WITH lcWAddr5,;
          CSFCOUNTRY WITH lcWAddr6,;
          CSFPHONE   WITH WAREHOUS.CPHONE,;
          CUPSACCT   WITH gfGetMemVar('XUPSACCT')
  =gfAdd_Info()
ENDIF  
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfTMPUPS
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/15/2001
*! Purpose   : Update Charges.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfTMPUPS

=lfTMPUPSS(llCalld,lcChrgTyp)

FUNCTION lfTMPUPSS
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
    SCATTER MEMVAR FIELDS SHIPAMT,DISCPCNT,TAX_RATE,cTaxRule,nPstRate,nHstRate,;
    ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,nPstAmt,nHstAmt,TOTALCHG
    =lfvUpsChrg(.T.,lcChrgType)
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
  REPLACE nHstRate WITH IIF(lnPstChrg=0,0,nHstAmt*100/lnPstChrg)
  UNLOCK
  SCATTER MEMVAR FIELDS &lcFlFields
  SCATTER FIELDS &lcScFields TO laData
ELSE
  DO lpUpsChrg WITH ;
  laData[4],laData[5],laData[1],ladata[3],laData[7],m.SHIPAMT,m.Cod_Flag='Y',;
  m.lUpsIns,m.DISCPCNT,m.TAX_RATE,m.cTaxRule,m.nPstRate,lcUpsTmp,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT','m.TAX_AMT',;
  'm.nPstAmt',IIF(UPPER(ALLTRIM(gcContCode))='USA',&lcInvHdr..nTaxDue,;
  m.SHIPAMT),lcChrgType
  
  m.nHstAmt  = IIF(llIsCanada,(ShipAmt+m.COD+m.Insur+m.Freight-;
                   M.ShipAmt*m.DiscPcnt/100+IIF(VAL(cTaxRule)=1,0,m.Tax_Amt))*nHstRate/100,0)
  m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod+m.Discount+m.Tax_Amt+m.nPstAmt+m.nHstAmt
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

*!**************************************************************************
*! Name      : lfCRTTMP
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/15/2001
*! Purpose   : Create temp file.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfCRTTMP

=gfOpenFile(gcDataDir+'ARUPSSHP',gcDataDir+'ARUPSSHP','SH')
=AFIELDS(laFileStru)
=gfCrtTmp(lcUpsTmp,@laFileStru,[PikTkt+STR(CARTONS,5)],lcUpsTmp)


*!**************************************************************************
*! Name      : lpUpsChrg
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/15/2001
*! Purpose   : Calculate charges.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
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
    =lfUPDCOD(lcUpsFile)
  ENDIF  
ENDIF
IF llCompTax
  *-- CALCULATE TAXES
  &lcTaxAmnt = ROUND(lcTaxRate*(IIF(lcTaxRule='M',lnTaxDue,lnTaxDue+;
               &lcFreight+&lcInsur+&lcCOD)-ABS(lnTaxDue*lnDscPrcnt/100) )/100,2)
  IF llCod
    *-- add the tax amount and the  pst amount to the cod amount  
    &lcCOD_AMT = &lcCOD_AMT+&lcTaxAmnt+&lcPstAmnt
    =lfUPDCOD(lcUpsFile)
  ENDIF
ENDIF

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
  *&lcCOD   = IIF(llCod,IIF('USUPS' $ lcUpsType,lnCodCharge*&lcCartons,&lcCOD),0)
ENDIF
IF INLIST(lcChrgType,'A','I')
  *--  Recalculate the insurance
  &lcInsur = IIF(llupsinsur,IIF('USUPS' $ lcUpsType AND lnTotShpAmt >=100,;
                 FLOOR(lnTotShpAmt/100)*lnInsCharge,0),0)               
ENDIF
RETURN

*!**************************************************************************
*! Name      : lfUpsBox
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/15/2001
*! Purpose   : Add cartons.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfUpsBox

PARAMETERS lcAccount,lcStore,lcOrderNo,lcPikTKt,llCod,llupsinsur,lnFreight,;
           lnInsure,lnCodAmt,lnCartons,lnWeight,lnTotShpAmt,lcUpsFile,lcChrgType
lnAlias = SELECT()
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
        IF lcPikTkt  = lcOldPik
          &lcFreight = &lcFreight + VAL(cfreight)
          &lcWeight  = &lcWeight  + VAL(cWeight)
          &lcCartons = &lcCartons + 1
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
      &lcFreight  = &lcFreight + VAL(cfreight)
      &lcWeight   = &lcWeight  + VAL(cWeight)
      &lcCartons  = &lcCartons + 1
    ENDSCAN
    SELECT (lcUpsFile)
    =SEEK(lcOldPik)
    IF llCod
      SUM VAL(CCOD) TO &lcCOD WHILE PIKTKT = lcOldPik
    ENDIF
    REPLACE ALL Cod_Flag WITH IIF(llCod,'Y','N') FOR lcPikTkt  = lcOldPik    
    lcPikTkt = lcOldPik
    REPLACE Cod_Flag WITH &lcUpsFile..Cod_Flag
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


*!*****************************************************************
*! Name      : lfEDTFRT
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/15/2001
*! Purpose   : Modify Freight AND COD
*!*************************************************************
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


*!*****************************************************************
*! Name      : lfUPDCOD
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/15/2001
*! Purpose   : Update COD amount.
*!*************************************************************
FUNCTION lfUPDCOD
PARAMETERS lcFile
lnAlias = SELECT()
lcFile = IIF(EMPTY(lcFile),lcUpsTmp,lcFile)
SELECT (lcFile)
COUNT FOR PikTkt = laData[3] TO lnCrtOfNo
REPLACE ALL &lcFile..CCOD_AMT WITH ALLTRIM(STR(m.Cod_Amt/lnCrtOfNo)) ;
        FOR PikTkt = laData[3]
SELECT (lnAlias)

*!******************************************************************
*! Name      : lfOgiCRT
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Call Custom carton information screen.
*!******************************************************************
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


*!*****************************************************************
*! Name      : CARTON
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Invoice Cartons Details.
*!*************************************************************
*! Calls     : AROGIBOX.SPX
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
lnInsure = 0
SELECT (lcUpsTmp)
IF laScrMode[4]
  =SEEK(lcPikTkt)
  *-- get the weight ,cod ,frieght for each carton
  SUM REST VAL(cWeight),VAL(cDecl_Val),VAL(cFreight),VAL(cCod),VAL(cCod_Amt) ;
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
DO (gcScrDir+gcWinAppl+"\AROGIBOX.SPX")
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
       Cartons   :H='Carton#'         :P= '99999'  :R    ,;
       cWeight   :H='Weight  '        :P= '999999' :R    ,;
       cFreight  :H='Freight     '    :P= '999999999' :R, ;       
       CTrack_NO :H='Package ID #               '  :P= 'XXXXXXXXXXXXXXXXX' :R ,;
       Cdecl_val :H='Declared Value'  :P= 'XXXXXXXXX' :R ,;
       CCod      :H='COD'  :P= '999999999' :R ,;
       CCod_AMT  :H='COD Amount'  :P= '999999999' :R ;
       WINDOW AROGIBO1   ;
       IN WINDOW AROGIBOX;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WHEN lfShowUPS() ;
       KEY IIF(laScrMode[4],lcPikTkt,INVHDR.PIKTKT) ;
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
SHOW GETS WINDOW 'AROGIBO2' ONLY
=lfRefresh('AROGIBO0')
=lfRefresh('AROGIBO2')

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
  ON KEY LABEL TAB     DO lpTab WITH 'AROGIBO2','m.Weight'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'AROGIBO2','pbClsUps'
ENDIF  
RETURN .F.

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
  SELECT (lcUpsTmp)
  lcKey = EVAL(KEY())
  =RLOCK()
  GATHER MEMVAR
  UNLOCK
  =SEEK(lcPikTkt)
  *-- get the weight ,cod ,frieght for each carton
  SUM REST VAL(CWeight),VAL(CFreight);
      TO   lnIWeight,lnIFreight ;
      WHILE PikTkt = lcPikTkt
  =SEEK(lcKey)
  =lfShowUps()
ENDIF

*!*************************************************************
*! Name      : lfvNewCrtn
*! Developer : WAM 
*! Date      : 07/01/1996
*! Purpose   : Add new cartons.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvNewCrtn()
*!*************************************************************
FUNCTION lfvNewCrtn

PRIVATE lnNBoxNo

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
SHOW GET lnTotCrt
*-- Show function for Cartons details screen
=lfShowUps()
_CUROBJ = OBJNUM(m.CWeight)

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

lnIWeight   = lnIWeight   - VAL(m.CWeight)
lnIFreight  = lnIFreight  - VAL(m.CFreight)
lnDecValue  = lnDecValue  - VAL(CDECL_VAL)
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
  SHOW GETS WINDOW 'AROGIBO2' DISABLE ONLY
  SHOW GET ibBrowUps ENABLE
  SHOW GET pbNewCrtn ENABLE
  SHOW GET pbcLSuPS  ENABLE
ENDIF
*-- Show function for Cartons details screen
=lfShowUps()

*!*****************************************************************
*! Name      : lfCloseB
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Check for lines with Freight or Weight = 0
*!*************************************************************
FUNCTION lfCloseB

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

*!******************************************************************
*! Name      : lfSAVIT
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/15/2001
*! Purpose   : Save invoice.
*!******************************************************************
FUNCTION lfSAVIT

*-- Update the arclpshp File 
lnNoOfCrt = 0
lcPiktkt = &lcHdrFile..PIKTKT
IF !EMPTY(lcUpsTmp) AND USED(lcUpsTmp)
  WAIT 'Updating Carton Box File ' WINDOW NOWAIT
  SELECT ARUPSSHP
  =SEEK(lcPiktkt)
  DELETE REST WHILE PIKTKT = lcPiktkt
  SELECT (lcUpsTmp)
  =SEEK(lcPiktkt)
  SCAN REST WHILE PIKTKT = lcPiktkt  FOR WEIGHT <> 0 
    SCATTER MEMVAR MEMO
    INSERT INTO ARUPSSHP FROM MEMVAR
  ENDSCAN
  REPLACE ALL ARUPSSHP.STATUS WITH 'C' FOR ARUPSSHP.PIKTKT = lcPiktkt
ENDIF

*!******************************************************************
*! Name      : lfCHKIT
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/15/2001
*! Purpose   : Check invoice.
*!******************************************************************
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

*!******************************************************************
*! Name      : lfOGIBOX
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Call Custom carton information screen.
*!******************************************************************
FUNCTION lfOGIBOX

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
DO (gcScrDir+gcWinAppl+"\AROGIBOX.SPX")
lcinvoice = lcOldInv
SELECT (lnAlias)
