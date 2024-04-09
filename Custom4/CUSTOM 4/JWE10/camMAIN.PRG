*!**************************************************************************
*! Name      : CAMMAIN.PRG
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006 (T20060804.0022)
*! Purpose   : CAM01 Custom Process Program 
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars


lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:*************************************************************
*: Name      : lfCAMADBAR
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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

*--Check if the Options Pad Menu was already defined
IF !lfFoundPad('Option')
  DEFINE PAD _Option OF (loFormSet.chostformname) PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF (loFormSet.chostformname) ACTIVATE POPUP _OPTIONPOP
  
  *--Define option pasd bars.
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF
lnBarNo = CNTBAR('_OPTIONPOP') + 1

*--Add menu Bar for Serial Lookup Screen
IF lnBarNo > 1
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT '\-'
  lnBarNo = lnBarNo + 1
ENDIF

DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Shipment details" SKIP FOR IIF(TYPE('_screen.ActiveForm.Parent') = 'O',_screen.ActiveForm.Parent.activemode = 'S',.F.)
ON SELECTION BAR lnBarNo OF _OPTIONPOP _SCREEN.ActiveForm.parent.mDoTrigger(PADR('SHPMTDTL',10))

RETURN

*!*************************************************************
*! Name      : lfCAMRLBAR
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*! Purpose   : Release Options Menu
*!*************************************************************
*! Example   : =lfCAMRLBAR()
*!*************************************************************
FUNCTION lfCAMRLBAR

*--Check if the Options Pad Menu was not Release before
IF lfFoundPad('Option')
  RELEASE PAD _Option OF (loFormSet.chostformname)
ENDIF
*-- end of lfCAMRLBAR.

*!*************************************************************
*! Name      : lfFoundPad C131527
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*! Purpose   : check if any pad menu is exit in _sysmenu
*!*************************************************************
*! Example   : =lfFoundPad(lcPadName)
*!*************************************************************
FUNCTION lfFoundPad
LPARAMETER lcPadName

LOCAL llFound
llFound = .F.

FOR lnCount = 1 TO CNTPAD(loFormSet.chostformname)    && Number of pads
  IF PRMPAD(loFormSet.chostformname, GETPAD(loFormSet.chostformname, LnCount)) = lcPadName
    llfound = .T.
    EXIT
  ENDIF
ENDFOR

RETURN(llFound)
*-- end of lfFoundPad.

*:*************************************************************
*: Name      : lfUpsDetil
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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
= lfChkUpsSh()
*B123213,1 NNA (End)

lcUpsStatu = 'Pending'
lcStatColor = "G"

llOpenUps  = .F.
llOpnInvhd = .F.
llOpnInvln = .F.
*-- Create the temp file.

*llOpenUps  = gfOpenFile(oAriaApplication.DataDir+'ARUPSSHP',oAriaApplication.DataDir+'ARUPSSHP','SH')
loDBFARUPSSHP = CreateObject("RemoteTable",'ARUPSSHP','ARUPSSHP','ARUPSSHP',SET("DATASESSION"))
loDBFINVHDR   = CreateObject("RemoteTable",'INVHDR','INVHDR','INVHDR',SET("DATASESSION"))
loDBFINVLINE  = CreateObject("RemoteTable",'INVLINE','INVLINEO','INVLINE',SET("DATASESSION"))

IF TYPE('loDBFARUPSSHP') = 'O'
  llOpenUps  = .T.
ENDIF 

IF  TYPE('loDBFINVHDR') = 'O'
  llOpnInvhd = .T.
ENDIF  

IF TYPE('loDBFINVLINE') = 'O'
  llOpnInvln = .T.
ENDIF 

*!*  llOpnInvhd = gfOpenFile(oAriaApplication.DataDir+'INVHDR',oAriaApplication.DataDir+'INVHDR','SH')
*!*  llOpnInvln = gfOpenFile(oAriaApplication.DataDir+'INVLINE',oAriaApplication.DataDir+'INVLINEO','SH')
*!*  SET RELATION TO Invoice INTO INVHDR ADDI

IF !USED('ARUPSSHP')
  RETURN
ENDIF

SELECT ARUPSSHP
lnInsure = 0
llFound = loDBFARUPSSHP.SEEK(piktkt.piktkt,'ARUPSSHP')
IF llFound
  *-- get the weight ,cod ,frieght for each carton
  SUM REST VAL(cWeight),VAL(cDecl_Val),VAL(cFreight),VAL(cCod),VAL(cCod_Amt) ;
    TO       lnWeight,lnDecValue,lnFreight,lnCod,lnCodAmnt ;
    WHILE PikTkt = piktkt.piktkt

  = loDBFARUPSSHP.SEEK(piktkt.piktkt,'ARUPSSHP')
  COUNT TO lnTotCrt WHILE PikTkt = piktkt.piktkt
	lcUpsStatu = 'Shipped'
	lcStatColor = "R"
ENDIF

*-- get the Cod_Flag From the invoice header.
IF loDBFINVLINE.SEEK(piktkt.order,'INVLINEO') .AND. loDBFINVHDR.Seek(INVLINE.Invoice) AND  INVHDR.Status # 'V'
  lnInsure  = InvHdr.Insur
ELSE
  SELECT INVLINE
  LOCATE REST WHILE Order+STR(lineno,6)+Invoice = piktkt.order ;
  FOR loDBFINVHDR.Seek(INVLINE.Invoice) AND INVHDR.Status # 'V'
  IF FOUND()
    lnInsure  = InvHdr.Insur
  ENDIF
ENDIF

SELECT ARUPSSHP
Locate

STORE SPACE(0) TO lcEscTrap , lcCtrETrap , lcCtrHTrap , lcCtrNTrap


DO FORM (oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+"\ALCAMUPS.SCX") WITH Piktkt.piktkt,lcUpsStatu,lcStatColor,;
lnWeight,lnDecValue,lnFreight,lnInsure,lnCod,lnCodAmnt


IF llOpenUps
*  USE IN ('ARUPSSHP')
  loDBFARUPSSHP = null
ENDIF


IF llOpnInvhd
 * USE IN ('INVHDR')
  loDBFINVHDR = null
ENDIF

IF llOpnInvln
  *USE IN ('INVLINE')
  loDBFINVLINE = null
ENDIF

SELECT(lnalias)
*-- End OF lfUpsDetil
*:*************************************************************
*: Name      : lfChkUpsSh
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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

FUNCTION lfChkUpsSh

PRIVATE lcPikTkt , lnRecNo , lcTrackNo , llFound
STORE ''  TO lcPikTkt , lcTrackNo 
STORE 0   TO lnRecNo
STORE .F. TO llFound
*!USED('ARUPSSHP') AND 
IF TYPE('loDBFARUPSSHP') <> 'O'
  loDBFARUPSSHP = CreateObject("RemoteTable",'ARUPSSHP','ARUPSSHP','ARUPSSHP',SET("DATASESSION"))
*  =gfOpenFile(oAriaApplication.DataDir+'ARUPSSHP',oAriaApplication.DataDir+'ARUPSSHP','SH')
ENDIF
SELECT ARUPSSHP
IF RECCOUNT()>1
  SCAN FOR STATUS = 'N'
    lnRecNo   = RECNO()
    lcPikTkt  = PIKTKT
    lcTrackNo = cTrack_NO
    SCAN REST WHILE PIKTKT = lcPikTkt FOR STATUS = 'Y' AND cTrack_NO = lcTrackNo
      STORE .T. TO llFound
      REPLACE Cod_Flag WITH 'X'
    ENDSCAN
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GOTO lnRecNo
    ENDIF 
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
DIMENSION laTableUpdate[1]
laTableUpdate[1] = loDBFARUPSSHP
 =lfTableUpdate()
RETURN
*-- End Of Function lfChkUpsSh.
*:*************************************************************
*: Name      : lfBrowUps
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : display sequential number for the cartons
*:*************************************************************
FUNCTION lfBrowUps
PARAMETERS lcPiktkt,loParFormset
loDBFARUPSSHP = CreateObject("RemoteTable",'ARUPSSHP','ARUPSSHP','ARUPSSHP',SET("DATASESSION"))
SELECT ARUPSSHP
LOCATE 
SET KEY TO lcPiktkt
LOCATE 
WITH loParFormset.ariaForm1.grdDetails
  .RecordSource = 'ARUPSSHP'
  .column1.ControlSource = 'lfCartNo("&lcPiktkt")'
  .column2.ControlSource = 'cWeight'
  .column3.ControlSource = 'cFreight'
  .column4.ControlSource = 'CTrack_NO'
  .column5.ControlSource = 'Cdecl_val'
  .column6.ControlSource = 'CCod'
  .column7.ControlSource = 'CCod_AMT'
  .AfterRowColChange ()
ENDWITH 

*:*************************************************************
*: Name      : lfCartNInv
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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
lnalias = ALIAS()
SELECT (loFormset.lcUpsTmp)
lnRecNo = RECNO()
=SEEK(IIF(loFormSet.ActiveMode = 'A',lcPikTkt,INVHDR.PIKTKT))
lncurrt = RECNO()
lnCarton = lnRecNo - lncurrt 

IF BETWEEN(lnRecNo ,1,RECCOUNT())
  GOTO lnRecNo 
ENDIF
SELECT (lnalias)
RETURN lnCarton + 1


*:*************************************************************
*: Name      : lfCartNo
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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
PARAMETERS lcPikTk
PRIVATE lnRecNo , lncurrt, lnCarton 
SELECT ARUPSSHP
lnRecNo = RECNO()
=loDBFARUPSSHP.SEEK(lcPikTk)
lncurrt = RECNO()
lnCarton = lnRecNo - lncurrt 
IF BETWEEN(lnRecNo ,1,RECCOUNT())
  GOTO lnRecNo 
ENDIF

RETURN lnCarton + 1

*:**************************************************************************
*: Name      : lfTMPUPS
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Update Charges.
*:**************************************************************************
*: Parameters: 
*:**************************************************************************
*: Returns   : 
*:**************************************************************************
FUNCTION lfTMPUPS

lcChrgType=IIF(TYPE('loformset.lcchrgtyp ')='C',loformset.lcchrgtyp ,'A')
lnPstRule  = 1
IF loformset.laSetups[9,2] = 'Y' AND UPPER(ALLTRIM(oAriaApplication.DefaultCountry)) ='CANADA'
  lnPstRule = loformset.ARIAForm1.ARIApageframe1.PAGe4.CNTInvoicesummary.CNTTaxes.CBOPstRule.Value 
ENDIF   

WAIT 'Computing Invoice Charges...' WINDOW NOWAIT
lcInvHdr = loformset.lcInvHdr
SELECT (loformset.lcInvHdr)
SET ORDER TO TAG CONSOL
lcKey = Account+cDivision+cCurrCode
IF Consol='Y'
  =SEEK('N'+lcKey)
  SCAN REST WHILE Consol+Account+cDivision+cCurrCode='N'+lcKey
  *  SCATTER FIELDS &lcScFields TO laData
    SCATTER MEMVAR FIELDS SHIPAMT,DISCPCNT,TAX_RATE,cTaxRule,nPstRate,nHstRate,;
    ShipVia,COD_AMT,FREIGHT,INSUR,COD,CARTONS,WEIGHT,TAX_AMT,nPstAmt,nHstAmt,TOTALCHG
    loformset.ariaForm1.ariapageframe1.page4.cntInvoicesummary.mupscharges (lcChrgType,.T.)
    
  ENDSCAN
  =SEEK('Y'+lcKey)
  =RLOCK()
  lnTaxAmount = IIF(UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='USA',nTaxDue,ShipAmt)
  lnGstChrg = IIF(loformset.laSetups[11,2]='M',lnTaxAmount,;
                  lnTaxAmount+Freight+Insur+COD)-DiscPcnt*lnTaxAmount/100
  lnPstChrg = ShipAmt+Freight+Insur+COD+Discount+IIF(VAL(m.cTaxRule)=1,0,Tax_Amt)
  REPLACE lCompUps WITH .F.        ,;
          Cod_Flag WITH m.Cod_Flag ,;
          lUpsIns  WITH m.lUpsIns  ,;
          Tax_Rate WITH IIF(lnGstChrg=0,0,Tax_Amt*100/lnGstChrg) ,;
          nPstRate WITH IIF(lnPstChrg=0,0,nPstAmt*100/lnPstChrg)
  REPLACE nHstRate WITH IIF(lnPstChrg=0,0,nHstAmt*100/lnPstChrg)
  

  REPLACE TotalChg  WITH ShipAmt+Freight+Insur+COD+Discount+Tax_Amt+nPstAmt+IIF(lnPstRule=2,m.nHstAmt,0) 

  
  UNLOCK
  SCATTER MEMVAR FIELDS &lcFlFields
*  SCATTER FIELDS &lcScFields TO laData
ELSE
  SCATTER MEMVAR FIELDS ShipVia, Cod_Amt, Freight, Insur, Cod, Cartons, Weight, Tax_Amt, nPstAmt,Cod_flag
  DO lpUpsChrg WITH ;
  &lcInvHdr..Account,&lcInvHdr..Store,&lcInvHdr..Order,&lcInvHdr..PikTkt,&lcInvHdr..cwarecode,&lcInvHdr..ShipAmt,loFormSet.ariaForm1.ariapageframe1.page4.cntInvoicesummary.cntCharges.chkCodCharge.Value,;
  loFormSet.ariaForm1.ariapageframe1.page4.cntInvoicesummary.cntCharges.chkInsurance.Value,&lcInvHdr..DISCPCNT,&lcInvHdr..TAX_RATE,&lcInvHdr..cTaxRule,&lcInvHdr..nPstRate,loformset.lcUpsTmp,'m.ShipVia',;
  'm.COD_AMT','m.FREIGHT','m.INSUR','m.COD','m.CARTONS','m.WEIGHT','m.TAX_AMT',;
  'm.nPstAmt',IIF(UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='USA',&lcInvHdr..nTaxDue,;
  &lcInvHdr..SHIPAMT),lcChrgType
  
  m.nHstAmt  = IIF(loFormSet.llIsCanada,(&lcInvHdr..ShipAmt+m.COD+m.Insur+m.Freight-;
                   &lcInvHdr..ShipAmt*m.DiscPcnt/100+IIF(VAL(cTaxRule)=1,0,m.Tax_Amt))*nHstRate/100,0)
  m.TotalChg = &lcInvHdr..ShipAmt+m.Freight+m.Insur+m.Cod+&lcInvHdr..Discount+m.Tax_Amt+m.nPstAmt+m.nHstAmt
  m.TotalChg = &lcInvHdr..ShipAmt+m.Freight+m.Insur+m.Cod+&lcInvHdr..Discount+m.Tax_Amt+m.nPstAmt+IIF(lnPstRule=2,m.nHstAmt,0) 
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

*!*	IF !llCalled
*!*	  SET ORDER TO TAG (lcInvHdr)
*!*	  SHOW GETS WINDOW (lcWinChrg) ONLY
*!*	  =lfRefresh(lcWinChrg)
*!*	  WAIT CLEAR
*!*	ENDIF

*:******************************************************************
*: Name      : lfOgiCRT
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Call Custom carton information screen.
*:******************************************************************
FUNCTION lfOgiCRT

lcInvHdr = loFormSet.lcInvHdr 

lnCartons  = &lcInvHdr..Cartons
lnWeights  = &lcInvHdr..Weight
lnFreights = &lcInvHdr..Freight
lnInsur    = &lcInvHdr..Insur
lnCods     = &lcInvHdr..Cod
lnCod_Amt  = &lcInvHdr..Cod_Amt

=lfCARTON(loFormSet.lcUpsTmp,&lcInvHdr..Invoice,&lcInvHdr..Account,&lcInvHdr..Order,;
 &lcInvHdr..Store,&lcInvHdr..PikTkt,;
 loFormSet.ariaForm1.ariapageframe1.page4.cntInvoicesummary.cntCharges.chkInsurance.Value,;
 loFormSet.ariaForm1.ariapageframe1.page4.cntInvoicesummary.cntCharges.chkCodCharge.Value,;
 &lcInvHdr..ShipAmt,&lcInvHdr..ShipVia,&lcInvHdr..cWareCode,'lnCartons','lnWeights',;
 'lnFreights','lnInsur','lnCods','lnCod_Amt')

SELECT (lcInvHdr)
IF loFormSet.ActiveMode = 'A'
  REPLACE Cartons WITH lnCartons ,;
          Weight  WITH lnWeights  ,;
          Freight WITH lnFreights ,;
          Insur   WITH lnInsur   ,;
          Cod     WITH lnCods     ,;
          Cod_Amt WITH lnCod_Amt
ENDIF

*:*****************************************************************
*: Name      : CARTON
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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

SELECT(loFormSet.lcupstmp)
IF loFormSet.ActiveMode = 'A'
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
lnTotShip  = lnShipAmt
lcShipViaD = gfCodDes(lcShipVia , 'SHIPVIA')
lcBol      = &lcInvHdr..Bol_No
llNewUps = .F.
SCATTER MEMVAR

=lfConvVar(.T.)

DO FORM (oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+"\ARCAMUP.SCX") WITH lnIWeight,lnDecValue,;
         lnIFreight,lnIInsure,lnICod,IIF(llCod,lnICodAmnt,0),loFormSet.ActiveMode,lnICartons,;
         loFormSet.lcupstmp,llUpsInsur,lnCod,lnCodAmnt
         
         
*!*  *------------
*!*  IF EMPTY(lcInvoice)     && Add mode
*!*    SELECT (lcUpsFile)
*!*    =SEEK(lcOrderNo+lcStore+lcPikTkt)
*!*    SUM REST Weight,Decl_Value,Freight,Insur,Cod,TotalCod ;
*!*    TO       &lcWeight,lnDecValue,&lcFreight,&lcInsur,&lcCod,&lcCodAmnt ;
*!*    WHILE Order+Store+PikTkt+STR(CARTONS,5) = lcOrderNo+lcStore+lcPikTkt
*!*    &lcCartons = _TALLY   && Number of cartons
*!*  ENDIF
*!*  SELECT (lnAlias)

*!*  *------------
IF loFormSet.ActiveMode = 'A' && Add mode
  SELECT(loFormSet.lcupstmp)
  =SEEK(lcPikTkt)
  SUM REST VAL(CWeight),VAL(CFreight) ;
  TO       &lcWeight,&lcFreight ;
  WHILE PikTkt = lcPikTkt
  *;
  FOR Weight <> 0 AND  Freight <> 0
  *--For :  is added for ignoring incorrect cratons (not counted :  _Tally)
  &lcCartons = _TALLY   && Number of cartons
  m.TotalChg = &lcInvHdr..ShipAmt+&lcInvHdr..Freight+&lcInvHdr..Insur+&lcInvHdr..Cod+&lcInvHdr..Discount+&lcInvHdr..Tax_Amt+&lcInvHdr..nPstAmt+&lcInvHdr..nHstAmt
ENDIF
SELECT (lnAlias)
*:**************************************************************************
*: Name      : lfCRTTMP
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Create temp file.
*:**************************************************************************
*: Parameters: 
*:**************************************************************************
*: Returns   : 
*:**************************************************************************
FUNCTION lfCRTTMP
=gfOpenFile(oAriaApplication.DataDir+'ARUPSSHP',oAriaApplication.DataDir+'ARUPSSHP','SH')
=AFIELDS(laFileStru)
=gfCrtTmp(loFormSet.lcUpsTmp,@laFileStru,[PikTkt+STR(CARTONS,5)],loFormSet.lcUpsTmp)

*:*************************************************************
*: Name      : lfConvVar
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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


*:*****************************************************************
*: Name      : lfCloseB
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Check for lines with Freight or Weight = 0
*:*************************************************************
FUNCTION lfCloseB

lcUpsTmp = loFormSet.lcupstmp
IF (!(loFormSet.ActiveMode $ 'AE') OR (TYPE('lcUpsTmp') = 'U'))
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
*:*************************************************************
*: Name      : lfvNewCrtn
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Add new cartons.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfvNewCrtn()
*:*************************************************************
FUNCTION lfvNewCrtn
PARAMETERS loParFormset
SELECT (loformset.lcUpsTmp)
=SEEK(lcPikTkt)
COUNT REST TO lnNBoxNo WHILE PikTkt = lcPikTkt
INSERT INTO   (loformset.lcUpsTmp) (PIKTKT,Cartons);
       VALUES            (lcPikTkt,lnNBoxNo+1)
lnTotCrt = lnNBoxNo+1

=lfConvVar(.T.)
loParFormset.ariaForm1.txtCarton.Value = lnTotCrt 

*:*************************************************************
*: Name      : lfAddContolSource
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Function to be called from some traped keys
*:*************************************************************
FUNCTION lfAddContolSource
PARAMETERS loParFormset

WITH loParFormset.ariaForm1.grdCharges
  .RecordSource = loformSet.lcUpsTmp
  .column1.ControlSource = 'lfCartNInv()'
  .column2.ControlSource = 'cWeight'
  .column3.ControlSource = 'cFreight'
  .column4.ControlSource = 'CTrack_NO'
  .column5.ControlSource = 'Cdecl_val'
  .column6.ControlSource = 'CCod'
  .column7.ControlSource = 'CCod_AMT'
ENDWITH 

*:*************************************************************
*: Name      : lfRfrhFld
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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
PARAMETERS lcField , lcVariable,lcValue

PRIVATE lnRecNo
SELECT (loformset.lcUpsTmp)
IF EOF()
  RETURN
ENDIF
REPLACE &lcField WITH lcValue
lnRecNo = RECNO()
lcPiktkt = piktkt
LOCATE
=SEEK(lcPiktkt)
SUM REST Val(EVal(lcField));
  TO    lnSumFlds;
  WHILE PikTkt = lcPiktkt

lcVariable.Value = lnSumFlds

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
*:*************************************************************
*: Name      : lfvRemCrtn
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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
PARAMETERS loParFormset
PRIVATE lnNBoxNo
lcTempUps = loFormSet.lcUpsTmp

loParFormset.AriaForm1.txtweight.Value  = loParFormset.AriaForm1.txtweight.Value  - VAL(EVALUATE(lcTempUps +'.CWeight'))
loParFormset.AriaForm1.txtfreight.Value = loParFormset.AriaForm1.txtfreight.Value - VAL(EVALUATE(lcTempUps +'.CFreight'))
loParFormset.AriaForm1.txtDeclare.Value = loParFormset.AriaForm1.txtDeclare.Value - VAL(EVALUATE(lcTempUps +'.cDecl_Val'))
loParFormset.AriaForm1.txtCod.Value     = loParFormset.AriaForm1.txtCod.Value     - VAL(EVALUATE(lcTempUps +'.cCod'))
loParFormset.AriaForm1.txtcodamt.Value  = loParFormset.AriaForm1.txtcodamt.Value  - VAL(EVALUATE(lcTempUps +'.cCod_Amt'))


SELECT (lcTempUps)
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

*:*************************************************************
*: Name      : lfGrdRefresh
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Remove invoice carton from the UPS Box
*:*************************************************************
FUNCTION lfGrdRefresh
PARAMETERS loParFormset

lcTempUps = loFormSet.lcUpsTmp
WITH loParFormset.ariaForm1
  .txtCarton.value   = EVALUATE(lcTempUps +'.Cartons')
  .txtFrght.value    = VAL(EVALUATE(lcTempUps +'.CFreight'))
  .txttotship.value  = VAL(EVALUATE(lcTempUps +'.CFreight'))+VAL(EVALUATE(lcTempUps +'.CCod'))
  .txtwght.value     = VAL(EVALUATE(lcTempUps +'.CWeight'))
  .txtcodc.value     = VAL(EVALUATE(lcTempUps +'.CCod'))
  .txttrack.value    = EVALUATE(lcTempUps +'.CTrack_NO')
  .txtdec.value      = VAL(EVALUATE(lcTempUps +'.CDecl_Val'))
  .txtcodamount.value= VAL(EVALUATE(lcTempUps +'.ccod_amt'))
ENDWITH 


FUNCTION lfvUpsChrg
PARAMETERS llTest,lcCharge
*:**************************************************************************
*: Name      : lpUpsChrg
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp)) 
IF gfGetMemVar('M_WareHouse',oAriaApplication.ActiveCompanyID)='Y'
  =gfOpenFile(oAriaApplication.DataDir+'WAREHOUS',oAriaApplication.DataDir+'WAREHOUS','SH')
  *-- get the from zone from the wharehouse if use multiple locations = y
  lcFromZone = IIF(SEEK(lcWareCode,'warehous'),WareHous.UPS,lcFromZone)
  *=gfCloseFile('WAREHOUS')
ENDIF
*-- get the company setup information
llCompTax = (gfGetMemVar('M_TAX',oAriaApplication.ActiveCompanyID)='Y')   && Use taxes = Y 
lcTaxRule = ALLTRIM(gfGetMemVar('M_TAX_METH',oAriaApplication.ActiveCompanyID)) && tax method 
llUpsBox  = (gfGetMemVar('M_UPSBOX',oAriaApplication.ActiveCompanyID)='Y')      && Track boxes for UPS manifactring
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

   
  ENDIF  
ENDIF
*!*	*C124347,1 NNA  09/02/2004 (BEGIN) Add the Customer.nhand_charg Value to the Freight for SYU03
*!*	IF ASCAN(laEvntTrig , PADR('HNDFRGHT',10)) <> 0
*!*	  =gfDoTriger('ARIINV',PADR('HNDFRGHT',10))
*!*	ENDIF  
*!*	*C124347,1 NNA (END)

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

*:**************************************************************************
*: Name      : lfUpsBox
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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
IF TYPE('lcInvHdr') = 'U' 
  PRIVATE lcInvHdr 
  lcInvHdr = lcInvFile
ENDIF
*-- Cleaning the ARUPSSHP File From Voided Records Before do Browsing
= lfChkUpsSh()

IF TYPE('loDBFARUPSSHP') <> 'O'
  loDBFARUPSSHP = CreateObject("RemoteTable",'ARUPSSHP','ARUPSSHP','ARUPSSHP',SET("DATASESSION"))
ENDIF

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
      &lcCartons  = &lcCartons + 1
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
DIMENSION laTableUpdate[1]
laTableUpdate[1] = loDBFARUPSSHP
=lfTableUpdate()
SELECT (lnAlias)
RETURN
*!*************************************************************
*! Name      : lfTableUpdate
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*! Purpose   : function to Update Sql Tables.
*!*************************************************************
FUNCTION lfTableUpdate

*--Open Dictionary files.
LOCAL lnAlias,lnConnectionHandlar,lcTranCode,lnI,llUpdate
lnAlias = SELECT(0)

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  SELECT (lnAlias)
  RETURN .F.
ENDIF

FOR lnI = 1 TO ALEN(laTableUpdate,1)
  llUpdate = laTableUpdate[lnI].TableUpdate(lcTranCode)
  IF !llUpdate
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDFOR

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  SELECT(lnAlias)
  RETURN .F.
ENDIF
SELECT(lnAlias)
*--end of lfTableUpdate.
*!*************************************************************
*! Name      : lfGetFright
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*! Purpose   : function to get Freight
*!*************************************************************
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

*:******************************************************************
*: Name      : lfSAVIT
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Save invoice.
*:******************************************************************
FUNCTION lfSAVIT

*-- Update the arclpshp File 
lnNoOfCrt = 0

IF TYPE('loDBFARUPSSHP') <> 'O'
  loDBFARUPSSHP = CreateObject("RemoteTable",'ARUPSSHP','ARUPSSHP','ARUPSSHP',SET("DATASESSION"))
ENDIF

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

IF !EMPTY(loFormSet.lcUpsTmp) AND USED(loFormSet.lcUpsTmp)
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
  SELECT (loFormSet.lcUpsTmp)
  =SEEK(lcPiktkt)
  SCAN REST WHILE PIKTKT = lcPiktkt  FOR WEIGHT <> 0 
    SCATTER MEMVAR MEMO
    INSERT INTO ARUPSSHP FROM MEMVAR
  ENDSCAN
  *B606936,1 ASH 02/06/03 (Begin) Fix the problem of wrong updating the status field in ARUPSSHP file.
  SELECT ARUPSSHP 
  REPLACE ALL STATUS WITH 'C' FOR PIKTKT = lcPiktkt
  SELECT (loFormSet.lcUpsTmp)
  lnNoOfCrt = 0
  *B606936,1 ASH 02/06/03 (End)
ENDIF
DIMENSION laTableUpdate[1]
laTableUpdate[1] = loDBFARUPSSHP
 =lfTableUpdate()


*:******************************************************************
*: Name      : lfCHKIT
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Check invoice.
*:******************************************************************
FUNCTION lfCHKIT

lnAlias = SELECT()
IF &lcSaveInv AND UPPER(ALLTRIM(oAriaApplication.DefaultCountry))<>'ENG' .AND. &lcInvFile..lCompUps
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

*:*****************************************************************
*: Name      : lfEDTFRT
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Modify Freight AND COD
*:*************************************************************
FUNCTION lfEDTFRT

lnAlias = SELECT()
lnOldChrg = loFormSet.ariaForm1.currentControl.oldValue
lcInvHdr = loFormSet.lcInvHdr
SELECT (loFormSet.lcUpsTmp)
=SEEK(&lcInvHdr..PikTkt)
IF (&lcInvHdr..FREIGHT = 0 AND loFormSet.ariaForm1.currentControl.name = 'txtFreightAmount') OR (&lcInvHdr..Cod= 0 AND loFormSet.ariaForm1.currentControl.name = 'txtCodCharge')
  IF loFormSet.ariaForm1.currentControl.name = 'txtFreightAmount'
    REPLACE ALL WHILE PikTkt = &lcInvHdr..PikTkt CFREIGHT WITH ""
  ELSE
    REPLACE ALL WHILE PikTkt = &lcInvHdr..PikTkt CCOD WITH ""
  ENDIF  
ELSE
  IF lnOldChrg <> IIF(loFormSet.ariaForm1.currentControl.name = 'txtFreightAmount',&lcInvHdr..FREIGHT,&lcInvHdr..COD)
    COUNT FOR PikTkt = &lcInvHdr..PikTkt TO lnCrtOfNo
    =SEEK(&lcInvHdr..PikTkt)
    IF loFormSet.ariaForm1.currentControl.name = 'txtFreightAmount'
      REPLACE REST WHILE PikTkt = &lcInvHdr..PikTkt cFREIGHT WITH ALLTRIM(STR(&lcInvHdr..FREIGHT/lnCrtOfNo,3))
    ELSE
      REPLACE REST WHILE PikTkt = &lcInvHdr..PikTkt cCod WITH ALLTRIM(STR(&lcInvHdr..Cod/lnCrtOfNo,9))    
    ENDIF
  ENDIF                                                    
ENDIF
SELECT (lnAlias)


*:*****************************************************************
*: Name      : lfUPDCOD
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Update COD amount.
*:*************************************************************
FUNCTION lfUPDCOD
PARAMETERS lcFile
lcInvHdr = loFormSet.lcInvHdr
lnAlias = SELECT()
lcFile = IIF(EMPTY(lcFile),loFormSet.lcUpsTmp,lcFile)
SELECT (lcFile)
COUNT FOR PikTkt = &lcInvHdr..PikTkt TO lnCrtOfNo
REPLACE ALL &lcFile..CCOD_AMT WITH ALLTRIM(STR(&lcInvHdr..Cod_Amt/lnCrtOfNo)) ;
        FOR PikTkt = &lcInvHdr..PikTkt 
SELECT (lnAlias)

*:******************************************************************
*: Name      : lfOGIBOX
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
*: Purpose   : Call Custom carton information screen.
*:******************************************************************
FUNCTION _lfOGIBOX

lcInvHdr = IIF(loFormSet.activemode = 'A', loFormSet.lcInvHdr ,'INVHDR')
lnCartons  = &lcInvHdr..Cartons
lnWeights  = &lcInvHdr..Weight
lnFreights = &lcInvHdr..Freight
lnInsur    = &lcInvHdr..Insur
lnCods     = &lcInvHdr..Cod
lnCod_Amt  = &lcInvHdr..Cod_Amt

=lfCARTBOX(&lcInvHdr..Invoice,&lcInvHdr..Account,&lcInvHdr..Order,&lcInvHdr..Store,;
		   &lcInvHdr..PikTkt,;
		   loFormSet.ariaForm1.ariapageframe1.page3.cntInvoicesummary.cntCharges.chkInsurance.Value,;
  	       loFormSet.ariaForm1.ariapageframe1.page3.cntInvoicesummary.cntCharges.chkCodCharge.Value,;
           &lcInvHdr..ShipAmt,&lcInvHdr..ShipVia,&lcInvHdr..cWareCode,'lnCartons',;
          'lnWeights','lnFreights','lnInsur','lnCods','lnCod_Amt')
          

*:*****************************************************************
*: Name      : lfCARTBOX
*! Developer : MMT (Mariam Mazhar)
*! Date      : 08/24/2006
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
FUNCTION _lfCARTBOX

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

=lfCRTTMP()
IF TYPE('loDBFARUPSSHP') <> 'O'
  loDBFARUPSSHP = CreateObject("RemoteTable",'ARUPSSHP','ARUPSSHP','ARUPSSHP',SET("DATASESSION"))
ENDIF 

SELECT ARUPSSHP 
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
SELECT (loFormSet.lcupstmp)
APPEND BLANK 
GATHER MEMVAR
lcOldInv  = lcInvoice
lcinvoice =  Invhdr.Piktkt

DO FORM (oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+"\ARCAMUP.SCX") WITH lnIWeight,lnDecValue,;
         lnIFreight,lnIInsure,lnICod,IIF(llCod,lnICodAmnt,0),loFormSet.ActiveMode,lnICartons,;
         loFormSet.lcupstmp,llUpsInsur,lnCod,lnCodAmnt


lcinvoice = lcOldInv
SELECT (lnAlias)


