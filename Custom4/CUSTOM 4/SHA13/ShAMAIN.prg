*!**************************************************************************
*! Name      : SHAMAIN.PRG
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2010
*! Purpose   : Sharon Young Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C201235,1
*!**************************************************************************
*! Modifications
*! C201235,2 MMT 06/14/2010 Fix bug of not updating CtktBom Table[T20100323.0033]
*! C201271,1 MMT 10/10/2010 Add New Triggers to Update HTSUS# in PO Lines when updated in style screen[T20100915.0032]
*! C201413,1 MMT 11/03/2011 Add Trigger to Invoice SO Screen to Update Freight and Taxes [T20110506.0009]
*! C201907,1 Sara.O 01/04/2017 Add triggers to Customer screen for Divisions screens[T20160616.0013]
*! C201909,1 MAA  01/04/2017 Add triggers to Sales screen to read Customer Divisions [T20160616.0013]
*! C201909,2 MMT  01/18/2017 Division Sales rep. name is not displayed in SO screen[T20160616.0013]
*! B611259,1 MMT 02/01/2017 Issue5 and 6:Custom Division Sales rep. screen crash when called from Customer screen and term code is not read from customer when it is empty for Division[P20160623.0001]
*! C201949,1 MMT 02/20/2017 Issue#2: Add trigger to Direct invoice screen to read Division\Sales Rep[P20160623.0001]
*! E303754,1 MMT 02/20/2017 Issue#3: Add trigger to Return Merchinat. screen to read Division\Sales Rep[P20160623.0001]
*! C201960,1 SARA.O 03/05/2017  Issue#3: Return Authorization. Enabling Salesrep by Customer / Division [T20160616.0013]
*! B611520,1 HMS 2/4/2018 Problems with eCommerce charges getting carried in.. [T20180117.0007][Begin]
*! C202192,1 HMS, 08/06/2018 Issue 18 -Sharon [P20170705.0004 - Issue#18]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars
#include W:\Aria4xp\Screens\AR\ariinv.h 
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfCHKPOUPDT
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/12/2010
*! Purpose   : Ask user if he wants to Update POs or not
*!**************************************************************************
FUNCTION lfCHKPOUPDT
  IF loFormSet.ActiveMode <> 'E'
    RETURN
  ENDIF

  IF STYLE.ldetcost
    RETURN
  ENDIF

  IF loFormSet.llMsgShow
    RETURN
  ENDIF
  IF loFormSet.lnOldTotCost <> loFormSet.ariaForm1.pgfStyleInfo.page6.txttotCost.VALUE
    IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you like to update Costing for All Open/Hold POs') = 1
      loFormSet.llUpPoCost = .T.
    ELSE
      loFormSet.llUpPoCost  = .F.
    ENDIF
    loFormSet.llMsgShow = .T.
  ENDIF

  *!**************************************************************************
  *! Name      : lfADDCSTPRP
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 05/12/2010
  *! Purpose   : Add Properties to Style screen
  *!**************************************************************************
FUNCTION lfADDCSTPRP

  IF TYPE('loFormSet.llUpPoCost') = 'U'
    loFormSet.ADDPROPERTY('llUpPoCost',.F.)
  ENDIF



  IF TYPE('loFormSet.llMsgShow') = 'U'
    loFormSet.ADDPROPERTY('llMsgShow',.F.)
  ENDIF


  IF TYPE('loFormSet.lnOldTotCost') = 'U'
    loFormSet.ADDPROPERTY('lnOldTotCost',0)
  ENDIF

  *!**************************************************************************
  *! Name      : lfRSTCSTPROP
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 05/12/2010
  *! Purpose   : Reset Style Screen new Properties values
  *!**************************************************************************
FUNCTION lfRSTCSTPROP

  IF TYPE('loFormSet.llUpPoCost') = 'U'
    loFormSet.ADDPROPERTY('llUpPoCost',.F.)
  ELSE
    loFormSet.llUpPoCost = .F.
  ENDIF
  IF TYPE('loFormSet.llMsgShow') = 'U'
    loFormSet.ADDPROPERTY('llMsgShow',.F.)
  ELSE
    loFormSet.llMsgShow = .F.
  ENDIF

  IF TYPE('loFormSet.lnOldTotCost') = 'U'
    loFormSet.ADDPROPERTY('lnOldTotCost',0)
  ELSE
    IF loFormSet.ActiveMode <> 'E'
      loFormSet.lnOldTotCost = 0
    ENDIF
  ENDIF

  *!**************************************************************************
  *! Name      : lfGETORGCOST
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 05/12/2010
  *! Purpose   : save Orginial Style Cost
  *!**************************************************************************
FUNCTION lfGETORGCOST
  IF STYLE.ldetcost
    RETURN
  ENDIF
  IF !INLIST(loFormSet.ActiveMode,'E','V')
    RETURN
  ENDIF
  loFormSet.lnOldTotCost = loFormSet.ariaForm1.pgfStyleInfo.page6.txttotCost.VALUE

  *!**************************************************************************
  *! Name      : lfUPDPOCST
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 05/12/2010
  *! Purpose   : Update POs Cost
  *!**************************************************************************
FUNCTION lfUPDPOCST

  IF loFormSet.ActiveMode $ 'SA'
    RETURN
  ENDIF
  lnSeleOldAlis = SELECT()
  IF loFormSet.llUpPoCost
    IF !USED('POSLN_PO')
      =gfOpenTable('POSLN','POSLN','SH','POSLN_PO')
    ENDIF
    IF !USED('BOMLINE_PO')
      =gfOpenTable('BOMLINE','BOMLINE','SH','BOMLINE_PO')
    ENDIF
    IF !USED('CTKTBOM_PO')
      =gfOpenTable('CTKTBOM','CTKTBOM','SH','CTKTBOM_PO')
    ENDIF

    IF !USED('POSHDR_PO')
      =gfOpenTable('POSHDR','POSHDR','SH','POSHDR_PO')
    ENDIF
    SELECT STYLE
    =SEEK(loFormSet.lcStyleKey)
    SCAN REST WHILE STYLE = loFormSet.lcStyleKey
      IF STYLE.ldetcost
        LOOP
      ENDIF
      lcStyleToScan = STYLE.STYLE
      SELECT POSLN_PO
      IF gfSqlRun("Select POSLN.*,POSHDR.Status AS POSTAT From POSLN INNER JOIN "+;
          " POSHDR ON POSHDR.cStytype = POSLN.cStytype and POSHDR.CBusDocu = POSLN.cBusDocu "+;
          " and POSHDR.PO = POSLN.PO Where POSLN.CINVTYPE= '0001' and POSLN.Style = '"+lcStyleToScan+;
          "' AND POSLN.TRANCD = '1' AND POSHDR.Status IN ('O','H')"+;
          " AND POSLN.cStytype ='P' and POSHDR.CBusDocu = 'P'")
        SELECT POSLN_PO
        SCAN
          =gfSeek(POSLN_PO.CBUSDOCU+POSLN_PO.CSTYTYPE+POSLN_PO.PO,'POSHDR_PO')

          FOR lnT = 1 TO 7
            lcT = STR(lnT,1)
            lnNewVal = POSLN_PO.TotQty * STYLE.nicost&lcT.
            lnOldVal = POSLN_PO.TotQty * POSLN_PO.nicost&lcT.

            lnfNewVal = POSLN_PO.TotQty * STYLE.nicost&lcT.
            lnfOldVal = POSLN_PO.TotQty * POSLN_PO.nfCost&lcT.

            IF lnT = 1 AND POSLN_PO.Disc_pcnt > 0
              lnNewVal = POSLN_PO.TotQty * (STYLE.nicost1-(STYLE.nicost1*POSLN_PO.Disc_pcnt/100))
              lnfNewVal = POSLN_PO.TotQty * (STYLE.nicost1-(STYLE.nicost1*POSLN_PO.Disc_pcnt/100))
            ENDIF

            SELECT POSHDR_PO
            gfReplace("niCost&lcT. with niCost&lcT. - lnOldVal + lnNewVal")
            gfReplace("nfCost&lcT. with nfCost&lcT. - lnfOldVal + lnfNewVal")

          ENDFOR
          SELECT POSHDR_PO
          gfReplace("POTOTAL With nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7")

          SELECT POSLN_PO
          IF POSTAT = 'O'
            SELECT BOMLINE_PO
            =gfSeek('I'+'1'+POSLN_PO.PO+STR(POSLN_PO.LINENO,6))
            SCAN REST WHILE CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE = 'I'+'1'+POSLN_PO.PO+STR(POSLN_PO.LINENO,6)
              lcCstItm = ALLTRIM(BOMLINE_PO.CBOMTYP)
              gfReplace('UnitCost with Style.nicost&lcCstItm.')
              gfReplace('ItemAmt with itemQty * UnitCost')
            ENDSCAN
            SELECT CTKTBOM_PO
            =gfSeek('I'+POSLN_PO.PO)
            FOR lnZ = 1 TO 7
              lcZ =STR(lnZ,1)
              *! C201235,2 MMT 06/14/2010 Fix bug of not updating CtktBom Table[Start]
              =SEEK('I'+POSLN_PO.PO)
              *! C201235,2 MMT 06/14/2010 Fix bug of not updating CtktBom Table[End]
              SCAN REST WHILE CIMTYP+CUTTKT+TYP+CINVTYPE+ITEM+MFGCODE+DYELOT = 'I'+POSLN_PO.PO FOR MFGCODE = PADR('*'+lcZ,LEN(MFGCODE))
                lcCstItm = ALLTRIM(CTKTBOM_PO.TYP)
                lnNwVal = POSLN_PO.TotQty * STYLE.nicost&lcCstItm.
                lnOldVal = POSLN_PO.TotQty * POSLN_PO.nicost&lcCstItm.
                gfReplace('EST_COST with EST_COST - lnOldVal+lnNwVal')
                gfReplace('UNTCOST with Est_Cost/Req_Qty')
              ENDSCAN
            ENDFOR
          ENDIF

          SELECT POSHDR_PO
          =gfTableUpdate()
          SELECT CTKTBOM_PO
          =gfTableUpdate()
          FOR lnT = 1 TO 7
            lcT = STR(lnT,1)
            SELECT POSLN_PO
            gfReplace("niCost&lcT. with Style.nicost&lcT. ")
            gfReplace("nfCost&lcT. with Style.nicost&lcT. ")
          ENDFOR
          SELECT POSLN_PO
          gfReplace("gros_price with Style.nicost1")
          gfReplace("niCost1 with Style.nicost1-(Style.nicost1*Disc_pcnt/100)")
          gfReplace("nfCost1 with Style.nicost1-(Style.nicost1*Disc_pcnt/100)")
        ENDSCAN
      ENDIF

    ENDSCAN
    SELECT POSLN_PO
    =gfTableUpdate()
    SELECT BOMLINE_PO
    =gfTableUpdate()

    IF USED('POSLN_PO')
      =gfCloseTable('POSLN_PO')
    ENDIF
    IF USED('POSHDR_PO')
      =gfCloseTable('POSHDR_PO')
    ENDIF

    IF USED('CTKTBOM_PO')
      =gfCloseTable('CTKTBOM_PO')
    ENDIF
    IF USED('BOMLINE_PO')
      =gfCloseTable('BOMLINE_PO')
    ENDIF
  ENDIF
  SELECT(lnSeleOldAlis)

  *! C201271,1 MMT 10/10/2010 Add New Triggers to Update HTSUS# in PO Lines when updated in style screen[Start]
  *!**************************************************************************
  *! Name      : lfADDHTSPRP
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 10/10/2010
  *! Purpose   : Add Htsus update Properties
  *!**************************************************************************
FUNCTION lfADDHTSPRP

  IF TYPE('loFormSet.llUpPoHTS') = 'U'
    loFormSet.ADDPROPERTY('llUpPoHTS',.F.)
  ENDIF

  IF TYPE('loFormSet.llHTSMsgShow') = 'U'
    loFormSet.ADDPROPERTY('llHTSMsgShow',.F.)
  ENDIF

  IF TYPE('loFormSet.lcOldHTS') = 'U'
    loFormSet.ADDPROPERTY('lcOldHTS',SPACE(15))
  ENDIF

  *!**************************************************************************
  *! Name      : lfRSTHTSPROP
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 10/10/2010
  *! Purpose   : reSet Htsus update Properties
  *!**************************************************************************
FUNCTION lfRSTHTSPROP

  IF TYPE('loFormSet.llUpPoHTS') = 'U'
    loFormSet.ADDPROPERTY('llUpPoHTS',.F.)
  ELSE
    loFormSet.llUpPoHTS = .F.
  ENDIF
  IF TYPE('loFormSet.llHTSMsgShow') = 'U'
    loFormSet.ADDPROPERTY('llHTSMsgShow',.F.)
  ELSE
    loFormSet.llHTSMsgShow= .F.
  ENDIF

  IF TYPE('loFormSet.lcOldHTS') = 'U'
    loFormSet.ADDPROPERTY('lcOldHTS',SPACE(15))
  ELSE
    loFormSet.lcOldHTS = SPACE(15)
  ENDIF

  *!**************************************************************************
  *! Name      : lfGETORGHTS
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 10/10/2010
  *! Purpose   : Get Htsus Old Value
  *!**************************************************************************
FUNCTION lfGETORGHTS
  loFormSet.lcOldHTS  =loFormSet.ariaForm1.pgfStyleInfo.page3.txtHTSUS.VALUE

  *!**************************************************************************
  *! Name      : lfPOUPDTHTS
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 10/10/2010
  *! Purpose   : Ask user if wants to update Htsus
  *!**************************************************************************
FUNCTION lfPOUPDTHTS
  IF loFormSet.ActiveMode <> 'E'
    RETURN
  ENDIF

  IF loFormSet.llHTSMsgShow
    RETURN
  ENDIF
  IF loFormSet.lcOldHTS  <> loFormSet.ariaForm1.pgfStyleInfo.page3.txtHTSUS.VALUE
    IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Would you like to apply the new style HTSUS# to all open and hold purchase orders') = 1
      loFormSet.llUpPoHTS= .T.
    ELSE
      loFormSet.llUpPoHTS= .F.
    ENDIF
    loFormSet.llHTSMsgShow= .T.
  ENDIF
  *!**************************************************************************
  *! Name      : lfUPDPOHTS
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 10/10/2010
  *! Purpose   : update Htsus in POs Line table
  *!**************************************************************************
FUNCTION lfUPDPOHTS
  IF loFormSet.ActiveMode $ 'SA'
    RETURN
  ENDIF
  lnSeleOldAlis = SELECT()
  IF loFormSet.llUpPoHTS
    IF !USED('POSLN_PO')
      =gfOpenTable('POSLN','POSLN','SH','POSLN_PO')
    ENDIF
    SELECT STYLE
    =SEEK(loFormSet.lcStyleKey)
    SCAN REST WHILE STYLE = loFormSet.lcStyleKey
      lcStyleToScan = STYLE.STYLE
      SELECT POSLN_PO
      IF gfSqlRun("Select POSLN.* From POSLN INNER JOIN "+;
          " POSHDR ON POSHDR.cStytype = POSLN.cStytype and POSHDR.CBusDocu = POSLN.cBusDocu "+;
          " and POSHDR.PO = POSLN.PO Where POSLN.CINVTYPE= '0001' and POSLN.Style = '"+lcStyleToScan+;
          "' AND POSHDR.Status IN ('O','H')"+;
          " AND POSLN.cStytype ='P' and POSHDR.CBusDocu = 'P'")
        SELECT POSLN_PO
        SCAN
          gfReplace("CHTSNO WITH '"+STYLE.chtsno+"'")
        ENDSCAN
      ENDIF
    ENDSCAN
    SELECT POSLN_PO
    =gfTableUpdate()
    IF USED('POSLN_PO')
      =gfCloseTable('POSLN_PO')
    ENDIF
  ENDIF
  *! C201271,1 MMT 10/10/2010 Add New Triggers to Update HTSUS# in PO Lines when updated in style screen[End]

  *! C201413,1 MMT 11/03/2011 Add Trigger to Invoice SO Screen to Update Freight and Taxes [Start]
  *!**************************************************************************
  *! Name      : lfUPDINVCH
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 10/10/2010
  *! Purpose   : update charges from Order charges table
  *!**************************************************************************
FUNCTION lfUPDINVCH
  lnSelAlias = SELECT()
  lcInvHead = loFormSet.ariaForm1.ariapageframe1.page4.cntInvoicesummary.InvoiceHeaderFile
  llCompanyTax = (gfGetMemVar('M_TAX',oAriaApplication.ActiveCompanyID)='Y')
  DECLARE laShpViaFld[2,2]
  STORE '' TO lcCarrierId
  STORE .T. TO llIncFrght
  laShpViaFld[1,1] = 'CCARRIERID'  && Carrier ID
  laShpViaFld[1,2] = 'lcCarrierId'
  laShpViaFld[2,1] = 'LINCFRGHT'  && "Include Freight charges Yes/No"
  laShpViaFld[2,2] = 'llIncFrght'
  =gfRltFld(&lcInvHead..ShipVia,@laShpViaFld,'SHIPVIA')
  llIncFrght = IIF(TYPE('llIncFrght') ='L',llIncFrght,.T.)
  IF !USED('ORDERCHG')
    =gfOpenTable('ORDERCHG','ORDERCHG')
  ELSE
    SELECT ORDERCHG
    gfSetOrder('ORDERCHG')
  ENDIF
  SELECT ORDERCHG
  lnTotalChrge = 0
  lnTaxTot = 0

  lcOrderNum = EVALUATE(lcInvHead +'.Order')
  IF gfSeek('O'+lcOrderNum)
    SELECT ORDERCHG
    SUM nchrgamnt TO lnTotalChrge  REST WHILE CORDTYPE+ORDER+STR(LINENO,6)+CCHRGCODE= 'O'+lcOrderNum FOR CCHRGTYPE = 'C'
    =gfSeek('O'+lcOrderNum)
    SUM nchrgamnt TO lnTaxTot REST WHILE CORDTYPE+ORDER+STR(LINENO,6)+CCHRGCODE= 'O'+lcOrderNum FOR CCHRGTYPE = 'T'
    *B611520 , HMS,Problems with eCommerce charges getting carried in.. [T20180117.0007][Begin]
    ELSE 
      RETURN  
   *B611520 , HMS,Problems with eCommerce charges getting carried in.. [T20180117.0007][END]   
  ENDIF
  REPLACE TotalChg  WITH TotalChg - Freight - Tax_Amt IN (lcInvHead)
  llUpdCod = .F.


  IF &lcInvHead..Cod_Flag='Y'
    IF  ALLTRIM(lcCarrierId) ='UPS' AND llIncFrght
      REPLACE Cod_Amt   WITH Cod_Amt - Freight IN (lcInvHead)
    ENDIF
    IF llCompanyTax
      REPLACE Cod_Amt   WITH Cod_Amt - Tax_Amt IN (lcInvHead)
    ENDIF
  ENDIF
  REPLACE Freight   WITH lnTotalChrge  ,;
    Tax_Amt   WITH lnTaxTot  IN  (lcInvHead)

  REPLACE TotalChg  WITH TotalChg + Freight + Tax_Amt IN (lcInvHead)
  IF &lcInvHead..Cod_Flag= 'Y'
    IF  ALLTRIM(lcCarrierId) ='UPS' AND llIncFrght
      REPLACE Cod_Amt   WITH Cod_Amt + Freight IN (lcInvHead)
    ENDIF
    IF llCompanyTax
      REPLACE Cod_Amt   WITH Cod_Amt + Tax_Amt IN (lcInvHead)
    ENDIF
  ENDIF
  SELECT(lnSelAlias)
  *! C201413,1 MMT 11/03/2011 Add Trigger to Invoice SO Screen to Update Freight and Taxes [END]
  *! C201907,1 Sara.O 01/04/2017 Add triggers to Customer screen for Divisions screens[T20160616.0013][Start]
  *!**************************************************************************
  *! Name      : lfMODMENUDIV
  *! Developer : SARA OSAMA [SARA.O]
  *! Date      : 01/04/2017
  *! Purpose   : Modify menu by adding division option
  *!**************************************************************************
FUNCTION lfMODMENUDIV
  lcHostForm= '[' + loFormSet. cHostFormName + ']'
  lnBarDef = CNTBAR('_INQURYPOP') +1
  DEFINE BAR lnBarDef  OF _INQURYPOP PROMPT "Divisions" SKIP FOR gfFormIsActive(&lcHostForm) AND ;
    TYPE('_SCREEN.ActiveForm.parent.ActiveMode') # 'C'

  ON SELECTION BAR lnBarDef  OF _INQURYPOP lfvDivisions(_SCREEN.ACTIVEFORM.PARENT)
  *!**************************************************************************
  *! Name      : lfvDivisions
  *! Developer : SARA OSAMA [SARA.O]
  *! Date      : 01/04/2017
  *! Purpose   : Show Divisions screen
  *!**************************************************************************
FUNCTION lfvDivisions
  LPARAMETERS loFormSet
  lnDataSession = SET("Datasession")
  SET DATASESSION TO loFormSet.DATASESSIONID
  IF !USED('CUSTDIVS')
    =gfOpenTable("CUSTOMER_SALESREP_DIVISION",'CUSTDIVS','SH','CUSTDIVS')
  ENDIF
  lcAccount = ALLTRIM(loFormSet.ariaForm1.keyAccount.keytextbox.VALUE )
  ****** Create cursor
  SELECT CUSTDIVS
  IF !SEEK(lcAccount) OR (TYPE('loFormSet.llDivFrmCalled') = 'L' AND !loFormSet.llDivFrmCalled)
    =gfSeek(lcAccount)
  ENDIF
  LOCATE
  *! B611259,1 MMT 02/01/2017 Issue5 and 6:Custom Division Sales rep. screen crash when called from Customer screen and term code is not read from customer when it is empty for Division[P20160623.0001][Start]
  *DO FORM (oAriaApplication.ClientScreenHome+"arcdiv.scx") WITH lcAccount ,loFormSet.activemode
  =GFCALLFORM("arcdiv",'',"'"+lcAccount+"','"+loFormSet.ActiveMode+"'")
  *! B611259,1 MMT 02/01/2017 Issue5 and 6:Custom Division Sales rep. screen crash when called from Customer screen and term code is not read from customer when it is empty for Division[P20160623.0001][End]
  IF TYPE('loFormSet.llDivFrmCalled') = 'L'
    loFormSet.llDivFrmCalled = .T.
  ENDIF

  SET DATASESSION TO &lnDataSession.
  *!**************************************************************************
  *! Name      : lfSAVEDATADV
  *! Developer : SARA OSAMA [SARA.O]
  *! Date      : 01/04/2017
  *! Purpose   : Saving customer Divisions
  *!**************************************************************************
FUNCTION lfSAVEDATADV
  lnSelectOld = SELECT()

  IF USED('CUSTDIVS')
    lcAccount = ALLTRIM(loFormSet.ariaForm1.keyAccount.keytextbox.VALUE)
    IF !USED('CDivision_up')
      =gfOpenTable("CUSTOMER_SALESREP_DIVISION",'CUSTDIVS','SH','CDivision_up')
    ENDIF

    SELECT CDivision_up
    =gfSeek(lcAccount)
    SCAN REST WHILE Account+cdivision = lcAccount
      =gfDelete()
    ENDSCAN
    =gfTableUpdate()


    SELECT CUSTDIVS
    IF SEEK(lcAccount)
      SCAN REST WHILE Account+cdivision = lcAccount
        =gfAdd_Info('CUSTDIVS')
        IF gfSeek(CUSTDIVS.Account+CUSTDIVS.cdivision,'CDivision_up')
          SELECT CDivision_up
          =gfReplace('')
        ELSE
          SELECT  CUSTDIVS
          SCATTER MEMO MEMVAR
          m.REC_NO = ''
          SELECT CDivision_up
          APPEND BLANK
          GATHER MEMO MEMVAR
          =SEEK(m.Account+m.cdivision)
          =gfReplace('')
        ENDIF
      ENDSCAN
    ENDIF
    SELECT CDivision_up
    =gfTableUpdate()
  ENDIF

  SELECT(lnSelectOld)
  *!**************************************************************************
  *! Name      : lfCHNGMODE
  *! Developer : SARA OSAMA [SARA.O]
  *! Date      : 01/04/2017
  *! Purpose   : customer screen change mode trigger
  *!**************************************************************************
FUNCTION lfCHNGMODE
  IF TYPE('loFormSet.llDivFrmCalled') <> 'L'
    loFormSet.ADDPROPERTY ('llDivFrmCalled',.F.)
  ELSE
    loFormSet.llDivFrmCalled = .F.
  ENDIF
  *! C201907,1 Sara.O 01/04/2017 Add triggers to Customer screen for Divisions screens[T20160616.0013][End]
  *! C201909,1 MAA  01/04/2017 Add triggers to Sales screen to read Customer Divisions [T20160616.0013][Start]
  *!**************************************************************************
  *! Name      : lfACCTREPDIV
  *! Developer : MOUSTAFA ABOUSHADY [MAA]
  *! Date      : 01/04/2017
  *! Purpose   : Get Customer/Division Sales rep.
  *!**************************************************************************
FUNCTION lfACCTREPDIV

  IF !USED('CustRepDiv')
    =gfOpenTable('customer_salesrep_division', 'custdivs', 'SH', 'CustRepDiv')
  ENDIF
  lcOrdHdrFile = loFormSet.OFORMENVIRONMENT.lcOrdHdr
  IF gfSeek(&lcOrdHdrFile..Account + &lcOrdHdrFile..cdivision, 'CustRepDiv')
    REPLACE Rep1 WITH CustRepDiv.Rep1,Rep2 WITH CustRepDiv.Rep2, ctermcode WITH CustRepDiv.ctermcode IN (lcOrdHdrFile)
    *! B611259,1 MMT 02/01/2017 Issue5 and 6:Custom Division Sales rep. screen crash when called from Customer screen and term code is not read from customer when it is empty for Division[P20160623.0001][Start]
    IF EMPTY(ALLTRIM(CustRepDiv.ctermcode))
      IF !USED('Customer_DIV')
        =gfOpenTable('Customer', 'Customer', 'SH', 'Customer_DIV')
      ENDIF
      =gfSeek('M'+&lcOrdHdrFile..Account,'Customer_DIV','Customer')
      REPLACE ctermcode WITH Customer_DIV.ctermcode IN (lcOrdHdrFile)
    ENDIF
    *! B611259,1 MMT 02/01/2017 Issue5 and 6:Custom Division Sales rep. screen crash when called from Customer screen and term code is not read from customer when it is empty for Division[P20160623.0001][End]
    STORE CustRepDiv.comm1 TO lnComm1
    STORE CustRepDiv.comm2 TO lnComm2
    WITH loFormSet.ariaForm1.ariapageframe1.Page1
      .spnComm1.VALUE = loFormSet.mCommissionDiscount(&lcOrdHdrFile..Rep1,lnComm1)
      .spnComm2.VALUE = loFormSet.mCommissionDiscount(&lcOrdHdrFile..Rep2,lnComm2)
      *! C201909,2 MMT  01/18/2017 Division Sales rep. name is not displayed in SO screen[T20160616.0013][Start]
      .txtSalesRep1Name.VALUE=IIF(EMPTY(&lcOrdHdrFile..Rep1),'',LOOKUP(SalesRep.NAME,&lcOrdHdrFile..Rep1,SalesRep.REPCODE,'SALESREP'))
      .txtSalesRep2Name.VALUE=IIF(EMPTY(&lcOrdHdrFile..Rep2),'',LOOKUP(SalesRep.NAME,&lcOrdHdrFile..Rep2,SalesRep.REPCODE,'SALESREP'))
      .keySalesRep1.keytextbox.REFRESH()
      .keySalesRep2.keytextbox.REFRESH()
      *! C201909,2 MMT  01/18/2017 Division Sales rep. name is not displayed in SO screen[T20160616.0013][End]
    ENDWITH
    *! C201909,2 MMT  01/18/2017 Division Sales rep. name is not displayed in SO screen[T20160616.0013][Start]
  ELSE
    IF !USED('Customer_DIV')
      =gfOpenTable('Customer', 'Customer', 'SH', 'Customer_DIV')
    ENDIF
    =gfSeek('M'+&lcOrdHdrFile..Account,'Customer_DIV','Customer')
    REPLACE Rep1 WITH Customer_DIV.SalesRep,Rep2 WITH Customer_DIV.Rep2, ctermcode WITH Customer_DIV.ctermcode IN (lcOrdHdrFile)
    STORE Customer_DIV.COMM TO lnComm1
    STORE Customer_DIV.comm2 TO lnComm2
    WITH loFormSet.ariaForm1.ariapageframe1.Page1
      .spnComm1.VALUE = loFormSet.mCommissionDiscount(&lcOrdHdrFile..Rep1,lnComm1)
      .spnComm2.VALUE = loFormSet.mCommissionDiscount(&lcOrdHdrFile..Rep2,lnComm2)
      .txtSalesRep1Name.VALUE=IIF(EMPTY(&lcOrdHdrFile..Rep1),'',LOOKUP(SalesRep.NAME,&lcOrdHdrFile..Rep1,SalesRep.REPCODE,'SALESREP'))
      .txtSalesRep2Name.VALUE=IIF(EMPTY(&lcOrdHdrFile..Rep2),'',LOOKUP(SalesRep.NAME,&lcOrdHdrFile..Rep2,SalesRep.REPCODE,'SALESREP'))
      .keySalesRep1.keytextbox.REFRESH()
      .keySalesRep2.keytextbox.REFRESH()
    ENDWITH
    *! C201909,2 MMT  01/18/2017 Division Sales rep. name is not displayed in SO screen[T20160616.0013][End]
  ENDIF
  *! C201909,1 MAA  01/04/2017 Add triggers to Sales screen to read Customer Divisions [T20160616.0013][End]
  *! C201949,1 MMT 02/20/2017 Issue#2: Add trigger to Direct invoice screen to read Division\Sales Rep[P20160623.0001][Start]
  *!**************************************************************************
  *! Name      : lfACTREPDIV
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 02/20/2017
  *! Purpose   : Get Customer/Division Sales rep. in Direct Invoice
  *!**************************************************************************
FUNCTION lfACTREPDIV

  IF !USED('CustRepDiv')
    =gfOpenTable('customer_salesrep_division', 'custdivs', 'SH', 'CustRepDiv')
  ENDIF
  lcInvhdr = loFormSet.lcInvhdr
  IF gfSeek(&lcInvhdr..Account + &lcInvhdr..cdivision, 'CustRepDiv')
    REPLACE Rep1 WITH CustRepDiv.Rep1,Rep2 WITH CustRepDiv.Rep2, ctermcode WITH CustRepDiv.ctermcode IN (lcInvhdr)
    IF EMPTY(ALLTRIM(CustRepDiv.ctermcode))
      IF !USED('Customer_DIV')
        =gfOpenTable('Customer', 'Customer', 'SH', 'Customer_DIV')
      ENDIF
      =gfSeek('M'+&lcInvhdr..Account,'Customer_DIV','Customer')
      REPLACE ctermcode WITH Customer_DIV.ctermcode IN (lcInvhdr)
    ENDIF
    STORE CustRepDiv.comm1 TO lnComm1,loFormSet.lnSouComm1
    STORE CustRepDiv.comm2 TO lnComm2,loFormSet.lnSouComm2

    WITH loFormSet.ariaForm1.ariapageframe1.Page1
      .spnComm1.VALUE = loFormSet.mCommissionDiscount(&lcInvhdr..Rep1,lnComm1,loFormSet.lnSouComm1,&lcInvhdr..DiscPcnt)
      .spnComm2.VALUE = loFormSet.mCommissionDiscount(&lcInvhdr..Rep2,lnComm2,loFormSet.lnSouComm2,&lcInvhdr..DiscPcnt)

      .txtSalesRep1Name.VALUE=IIF(EMPTY(&lcInvhdr..Rep1),'',LOOKUP(SalesRep.NAME,&lcInvhdr..Rep1,SalesRep.REPCODE,'SALESREP'))
      .txtSalesRep2Name.VALUE=IIF(EMPTY(&lcInvhdr..Rep2),'',LOOKUP(SalesRep.NAME,&lcInvhdr..Rep2,SalesRep.REPCODE,'SALESREP'))

      .keySalesRep1.keytextbox.REFRESH()
      .keySalesRep2.keytextbox.REFRESH()
      *X
      .cboTermCode.REFRESH()
      *X
    ENDWITH
  ELSE
    IF !USED('Customer_DIV')
      =gfOpenTable('Customer', 'Customer', 'SH', 'Customer_DIV')
    ENDIF
    =gfSeek('M'+&lcInvhdr..Account,'Customer_DIV','Customer')
    REPLACE Rep1 WITH Customer_DIV.SalesRep,Rep2 WITH Customer_DIV.Rep2, ctermcode WITH Customer_DIV.ctermcode IN (lcInvhdr)
    STORE Customer_DIV.COMM TO lnComm1,loFormSet.lnSouComm1
    STORE Customer_DIV.comm2 TO lnComm2,loFormSet.lnSouComm2

    WITH loFormSet.ariaForm1.ariapageframe1.Page1
      .spnComm1.VALUE = loFormSet.mCommissionDiscount(&lcInvhdr..Rep1,lnComm1,loFormSet.lnSouComm1,&lcInvhdr..DiscPcnt)
      .spnComm2.VALUE = loFormSet.mCommissionDiscount(&lcInvhdr..Rep2,lnComm2,loFormSet.lnSouComm2,&lcInvhdr..DiscPcnt)
      .txtSalesRep1Name.VALUE=IIF(EMPTY(&lcInvhdr..Rep1),'',LOOKUP(SalesRep.NAME,&lcInvhdr..Rep1,SalesRep.REPCODE,'SALESREP'))
      .txtSalesRep2Name.VALUE=IIF(EMPTY(&lcInvhdr..Rep2),'',LOOKUP(SalesRep.NAME,&lcInvhdr..Rep2,SalesRep.REPCODE,'SALESREP'))
      .keySalesRep1.keytextbox.REFRESH()
      .keySalesRep2.keytextbox.REFRESH()
      *X
      .cboTermCode.REFRESH()
      *X
    ENDWITH
  ENDIF
  *! C201949,1 MMT 02/20/2017 Issue#2: Add trigger to Direct invoice screen to read Division\Sales Rep[P20160623.0001][End]


  *!* C201960,1 SARA.O 03/05/2017  Issue#3: Return Authorization. Enabling Salesrep by Customer / Division [T20160616.0013]. [Begin]
  *!**************************************************************************
  *! Name      : lfACTREDVR
  *! Developer : Mariam Mazhar[MMT]
  *! Date      : 02/21/2017
  *! Purpose   : Get Customer/Division Sales rep. in Return Merchant.
  *!**************************************************************************
FUNCTION lfACTREPDVR

  IF !USED('CustRepDiv')
    =gfOpenTable('customer_salesrep_division', 'custdivs', 'SH', 'CustRepDiv')
  ENDIF
  IF   gfSeek(lcAccount + lcDivision, 'CustRepDiv')
    loFormSet.ariaForm1.pgfReCrmem.pgHeader.cntHeader.kbRep1.keytextbox.VALUE = CustRepDiv.Rep1
    loFormSet.ariaForm1.pgfReCrmem.pgHeader.cntHeader.spnRepComm1.VALUE = CustRepDiv.comm1
    loFormSet.ariaForm1.pgfReCrmem.pgHeader.cntHeader.kbRep2.keytextbox.VALUE = CustRepDiv.Rep2
    loFormSet.ariaForm1.pgfReCrmem.pgHeader.cntHeader.spnRepComm2.VALUE = CustRepDiv.comm2
    loFormSet.ariaForm1.pgfReCrmem.pgHeader.cntHeader.cboterms.VALUE = CustRepDiv.ctermcode
    IF EMPTY(ALLTRIM(CustRepDiv.ctermcode))
      IF !USED('Customer_DIV')
        =gfOpenTable('Customer', 'Customer', 'SH', 'Customer_DIV')
      ENDIF
      =gfSeek('M'+lcAccount ,'Customer_DIV','Customer')
      loFormSet.ariaForm1.pgfReCrmem.pgHeader.cntHeader.cboterms.VALUE = Customer_DIV.ctermcode
    ENDIF
  ELSE
    IF !USED('Customer_DIV')
      =gfOpenTable('Customer', 'Customer', 'SH', 'Customer_DIV')
    ENDIF
    =gfSeek('M'+lcAccount ,'Customer_DIV','Customer')
    loFormSet.ariaForm1.pgfReCrmem.pgHeader.cntHeader.kbRep1.keytextbox.VALUE = Customer_DIV.SalesRep
    loFormSet.ariaForm1.pgfReCrmem.pgHeader.cntHeader.spnRepComm1.VALUE = Customer_DIV.COMM
    loFormSet.ariaForm1.pgfReCrmem.pgHeader.cntHeader.kbRep2.keytextbox.VALUE = Customer_DIV.Rep2
    loFormSet.ariaForm1.pgfReCrmem.pgHeader.cntHeader.spnRepComm2.VALUE = Customer_DIV.comm2
    loFormSet.ariaForm1.pgfReCrmem.pgHeader.cntHeader.cboterms.VALUE = Customer_DIV.ctermcode
  ENDIF
  = loThis.mvrepcomm(1)
  = loThis.mvrepcomm(2)
  *!* C201952,1 SARA.O 03/05/2017  Issue#3: Return Authorization. Enabling Salesrep by Customer / Division [T20160616.0013]. [End]
  **C202060 MHM 08/29/2017 add Custom screen for shar13 [start]
  *********************************************************************************************************************************
  *! Name      : lfCALMNU
  *! Developer : Mohamedhamdy[MHM]
  *! Date      : 07/20/2017
  *! Purpose   : Call menu.
  *!**************************************************************************
FUNCTION  lfCALMNU
  loFormSet.lld=.F.
  loFormSet.moptionsmenupad()

  *********************************************************************************************************************************
  *! Name      : lfCALSCR
  *! Developer : Mohamedhamdy[MHM]
  *! Date      : 07/20/2017
  *! Purpose   : Call screen.
  *!**************************************************************************
FUNCTION lfCALSCR
  DO CASE
    CASE BAR() = loFormSet.CarrierBarPosition
      *! B609818,1 MMT 02/05/2012 Modify screen to display carrier option only if client has carriers[T20120118.0002][END]
      lcfile=loFormSet.lcInvhdr
      IF  EMPTY(&lcfile..PikTkt)
          lcExeKeyValue = "'O','"+ORDHDR.ORDER+"'"
      ELSE 
         lcExeKeyValue = "'P','"+&lcfile..PikTkt+"'"
      ENDIF 
       oAriaApplication.DoProgram("AWRALCARIN",lcExeKeyValue,.F.,'AL')
  ENDCASE
  *E302945,1 WAM 07/26/2011 (End)
  ***************************************************************
  *! Name      : lfADOPTION
  *! Developer : Mohamedhamdy[MHM]
  *! Date      : 07/20/2017
  *! Purpose   : Add option to option menu.
  *!**************************************************************************
FUNCTION lfADOPTION

  loFormSet.CarrierBarPosition = loFormSet.NextOptionBar
  loFormSet.mDefineCarrierBar()
  loFormSet.NextOptionBar = loFormSet.NextOptionBar + 1
  loFormSet.lld=.T.
  ***************************************************************
  *! Name      : lfDEFNWBAR
  *! Developer : Mohamedhamdy[MHM]
  *! Date      : 07/20/2017
  *! Purpose   : Define new bar in option menu.
  *!**************************************************************************
FUNCTION lfDEFNWBAR

  loFormSet.mDefineCarrierBar()
  ***************************************************************
  *! Name      : lfGTCARRID
  *! Developer : Mohamedhamdy[MHM]
  *! Date      : 07/20/2017
  *! Purpose   : Get Carrier ID.
  *!**************************************************************************
FUNCTION lfGTCARRID
  *E302945,1 WAM 07/26/2011 Get carrier ID related to SHip Via code
  SET STEP ON
 lcFile= loFormSet.lcInvhdr

  lcCarrier = ''
  lcHostFormName = '[' + loFormSet.cHostFormName + ']'
  loFormSet.lldo = loFormSet.ActiveMode = 'A' OR (loFormSet.ActiveMode = 'S' AND loFormSet.lld=.F. )
  lcShipVia = ORDHDR.ShipVia
  Lcsearch='S'+&lcFile..Account+&lcFile..Store
  IF(ORDHDR.multi=='Y')
    IF SEEK(Lcsearch,'CUSTOMER','CUSTOMER')
    	lcShipVia = Customer.ShipVIA
    ENDIF 
      
  ENDIF 
  IF loFormSet.lldo
    DECLARE laRltFld[1,2]
    laRltFld[1,1] = 'CCARRIERID'
    laRltFld[1,2] = 'lcCarrier'
    =gfRltFld( lcShipVia ,@laRltFld,'SHIPVIA')
  ENDIF
  *E302945,1 WAM 07/26/2011 Change bar prompt
  *B609706,1 WAM 10/20/2011 Save bar position
  *!*	IF !EMPTY(lcCarrier) AND UPPER(lcCarrier) <> 'OTHERS'
  *!*	  DEFINE BAR This.NextOptionBar OF _INQURYPOP PROMPT STRTRAN(LANG_ProcessCarrierShipment,LANG_Carrier,ALLTRIM(lcCarrier)) SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_screen.ActiveForm.Parent.ActiveMode <> 'V')
  *!*	ELSE
  *!*	  DEFINE BAR This.NextOptionBar OF _INQURYPOP PROMPT LANG_ProcessCarrierShipment SKIP FOR .T.
  *!*	ENDIF

  IF loFormSet.CarrierBarPosition > 0

    RELEASE BAR loFormSet.CarrierBarPosition OF _INQURYPOP

    *! B609818,1 MMT 02/05/2012 Modify screen to display carrier option only if client has carriers[T20120118.0002][Start]
    *IF !EMPTY(lcCarrier) AND UPPER(lcCarrier) <> 'OTHERS'
    IF !EMPTY(ALLTRIM(lcCarrier)) AND UPPER(lcCarrier) <> 'OTHERS' AND lfMODSCR(ALLTRIM(lcCarrier))
      *IF .t. OR (!EMPTY(lcCarrier) AND UPPER(lcCarrier) <> 'OTHERS' AND This.mclienthascarrier(lcCarrier))
      *! B609818,1 MMT 02/05/2012 Modify screen to display carrier option only if client has carriers[T20120118.0002][END]

      *DEFINE BAR This.CarrierBarPosition OF _INQURYPOP PROMPT STRTRAN(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ProcessCarrierShipment,ThisFormSet.GetHeaderText("LANG_ProcessCarrierShipment",ThisFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Carrier,ThisFormSet.GetHeaderText("LANG_Carrier",ThisFormSet.HeaderAlias)),ALLTRIM(lcCarrier)) SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_screen.ActiveForm.Parent.ActiveMode <> 'A')
      DEFINE BAR loFormSet.CarrierBarPosition OF _INQURYPOP PROMPT STRTRAN(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ProcessCarrierShipment,loFormSet.GetHeaderText("LANG_ProcessCarrierShipment",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Carrier,loFormSet.GetHeaderText("LANG_Carrier",loFormSet.HeaderAlias)),ALLTRIM(lcCarrier)) SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (!_SCREEN.ACTIVEFORM.PARENT.lldo)

      *! B609818,1 MMT 02/05/2012 Modify screen to display carrier option only if client has carriers[T20120118.0002][Start]
      *ELSE
      *DEFINE BAR This.CarrierBarPosition OF _INQURYPOP PROMPT LANG_ProcessCarrierShipment SKIP FOR .T.
      *! B609818,1 MMT 02/05/2012 Modify screen to display carrier option only if client has carriers[T20120118.0002][END]
      **C202060 MHM 08/29/2017 add Custom screen for shar13
    ENDIF
  ENDIF
  *B609706,1 WAM 10/20/2011 (End)
  ***************************************************************
  *! Name      : lfMODSCR
  *! Developer : Mohamedhamdy[MHM]
  *! Date      : 07/20/2017
  *! Purpose   : modify screen to display carrier option.
  *!**************************************************************************
FUNCTION lfMODSCR
  LPARAMETERS lcCarrier
  LOCAL lcClientID
  lcClientID  = oAriaApplication.ReadXML()
  llHasCarrier = .F.
  IF EMPTY(lcCarrier) OR UPPER(ALLTRIM(lcCarrier)) = 'OTHER'
    RETURN .F.
  ENDIF
  lcSqlStat= "Select * from CLIENTS_CARRIER_T WHERE CARRIER_ID ='"+ALLTRIM(lcCarrier)+"' AND CLIENT_ID = '"+lcClientID+"'"
  lnAccCarrier = oAriaApplication.RemoteCompanyData.execute(lcSqlStat,'',;
    "CLIENTS_CARRIERS_T","",oAriaApplication.sqlsysfilesconnectionstring,3,"",SET("Datasession"))
  IF lnAccCarrier > 0 AND RECCOUNT("CLIENTS_CARRIERS_T") > 0
    llHasCarrier =  .T.
  ENDIF
  RETURN llHasCarrier
  **C202060 MHM 08/29/2017 add Custom screen for shar13
  *B611415 ups issue 2
 ***************************************************************
  *! Name      : lfCAMN
  *! Developer : Mohamedhamdy[MHM]
  *! Date      : 07/20/2017
  *! Purpose   : Define new bar in option menu.
  *!**************************************************************************
FUNCTION lfCAMN 
loFormSet.mOptionsMenuPad()
  **C202060 MHM 08/29/2017 add Custom screen for shar13
  *B611415 ups issue 2
 ***************************************************************
  *! Name      :   **C202060 MHM 08/29/2017 add Custom screen for shar13
  *B611415 ups issue 2

*C202192, 1 HMS, Issue 18 Sharon [Begin]
 **************************************************************
  *! Name      : lfCAMN
  *! Developer : Heba Selim [HMS]
  *! Date      : 08/06/2018
  *! Purpose   : Define a variable and set its value to ' ' 
  *!***********************************************************
FUNCTION lfCLEARDESC 
m.Goods_Description = ''
ENDFUNC
*C202192, 1 HMS, Issue 18 Sharon [Begin]
