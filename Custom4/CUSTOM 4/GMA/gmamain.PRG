*!***************************************************************************************************************************************
*! Name      : GMAMAIN.PRG
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : GMA Main Program .
*!***************************************************************************************************************************************
*! Parameters: loFormSet -> FormSet object
*!       lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!***************************************************************************************************************************************
*! Returns   : Logical value.     
*!***************************************************************************************************************************************
*! Modifcations :
*!******************
*!* C#037922,1 HBG 03/23/2004 Convert customs (200404 , 200425) 
*!* C#037922,1                 "Add new Bar to Allow Modify # of ordered pack"
*!* C#038119,1 HBG 05/25/2004 Convert customs (102566) 
*!*  C#038128,1 MMT 05/26/2004 Convert Pack screen for GMA
*!* C#038193,1 HBG 08/22/2004 Add triggers to The invoice sales order screen
*!* B#124548,1 HBG 09/19/2004 Add trigger to The invoice sales order screen To allow edit carton & weight for all stores
*!* B#124549,1 HBG 09/19/2004 Add trigger to The invoice sales order screen To allow ship/UnShip for All store
*!* B#125046,1 HBG 10/28/2004 Fix bug of always displaying the meassge of Carton/Weight 
*!* B#124399,1 AMH 10/30/2004 Fix bug of Softseek in browse when copy from existing pack.
*!* E#038463,1 HBG 11/21/2004 replace configuration for multible lines in SO and Invoice SO screen
*!* E#038463,1 HBG 12/12/2004 Enable the configuration field in case the line belong to pack [begin]
*!* E#125004,1 HBG 12/12/2004 Enable the option "Modify # of ordered packs" in case of bulk order [Begin]
*!* B#124535,1 HBG 12/22/2004 Fix bug of incremental search not working [Begin]
*!* C#038945,1 WSH 01/16/2005 Add triggers to update IC Department Detail file on saving and deleting styles
*!* HBG 1/24/2005 Modify code to apply the new interface
*!* C126727,1 HBG 04/06/2005 Add option for Customer packing information to style screen
*!* B128324,1 WSH 06/02/2005 Don't refresh the FOB and Origin code fields on the PO screens until Option Grid is closed
*!* B128682,1 HBG 06/28/2005 Fix bug of not updating location in detail [Begin]
*!* b129096,1 MMT 07/26/2005 FIX bug in sku screen
*!* B129117,1 HBG 07/27/2005 fix bug of popup have been not defined
*!* C128356,1 HBG 08/23/2005 Update L\C fields in sales orde lines
*!* B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Invoice Rebalance Performance.
*!* C200938,1 MMT 02/13/2008 Add CustPo and Retail price screen to So Screen[T20080205.0028]
*!* B608434,1 WAM 02/14/2008 Use order header temporary file instead of buffered order header for all triggers called from the sales orer screen
*!* B608477,1 WAM 03/10/2008 Fix updating the invoiec lines with pack information 
*!* B608488,1 MHM 03/19/2008 Fix deleting SKU lines not deletd 
*!* C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA[T20080107.0013]
*!* C200995,1 MMT 05/12/2008 Bin location screen for GMA 
*!* B608827,1 WAM 03/26/2009 Don't overwrite pack information in ORDLINE while invoicing [T20080730.0017]
*!* B610301,1 HIA 04/15/2013 Upgrade GMA project - Issue # 5
*!****************************************************************************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars


#INCLUDE r:\aria4xp\GMAMAIN.h 
#INCLUDE r:\aria4xp\screens\so\soord.h
#INCLUDE r:\aria4xp\screens\AR\ARDINV.h
#INCLUDE r:\aria4xp\screens\IC\ICPACKG.h

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!*************************************************************
*! Name      : lfIntSord
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : Intiate variables
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Due to C037922,1
*!*************************************************************
*! Example   : =lfINTSORD()
*!*************************************************************
*!
FUNCTION lfIntSord

loFormSet.AddProperty('llRngPack',.F.)
*HBG 01/17/2005 Disable Show lines by pack option if there is no Packs [Begin]
loFormSet.AddProperty('llNtPacks',.F.)
loFormSet.AddProperty('llFromSO',.T.)
*HBG [End]

*E125004,1 HBG 12/12/2004 Enable the option "Modify # of ordered packs" in case of bulk order [Begin]
*loFormSet.AddProperty('llFromBulk',.F.)
*E125004,1 [End]
loFormSet.AddProperty('llShowPack',.F.)
loFormSet.AddProperty('llRangePck',.F.)
loFormSet.AddProperty('lcPackSum',gfTempName())
=gfOpenFile(oAriaApplication.DataDir+'STYLEUPC','GMAPKUPC','SH')
=gfOpenFile(oAriaApplication.DataDir+'SPCK_HDR','SPCK_HDRST','SH')
=gfOpenFile(oAriaApplication.DataDir+'SPCK_LIN','SPCK_LINVR','SH')

*C128356,1 HBG 08/23/2005 Add L\C fields to header folder [Begin]
IF TYPE("loFormSet.ariaform1.ariapageframe1.page1.lblLCNo") <> 'O'
  *--Add object for display Pack Container
  loFormSet.ariaform1.ariapageframe1.page1.AddObject("lblLCNo","lblLCNocls")
ENDIF 
WITH loFormSet.ariaform1.ariapageframe1.page1.lblLCNo
  .Visible =.T.
  .left= loFormSet.ariaform1.ariapageframe1.page1.Arialabel10.left
  *khm
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.top + loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.height + 5
  .Top = loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.top + loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.height + 8
  *khm
  
  WITH loFormSet.ariaform1.ariapageframe1.page1
    loFormSet.ariaform1.oResizer.SaveCoordinates(.lblLCNo)
  ENDWITH
ENDWITH 

IF TYPE("loFormSet.ariaform1.ariapageframe1.page1.Lbllcnodot") <> 'O'
  *--Add object for display Pack Container
  loFormSet.ariaform1.ariapageframe1.page1.AddObject("Lbllcnodot","lbldotcls")
ENDIF 
WITH loFormSet.ariaform1.ariapageframe1.page1.Lbllcnodot
  .Visible =.T.
  .left= loFormSet.ariaform1.ariapageframe1.page1.Arialabel5.left
  *khm
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.top + loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.height + 5
  .Top = loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.top + loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.height + 8
  *khm
  
  WITH loFormSet.ariaform1.ariapageframe1.page1
    loFormSet.ariaform1.oResizer.SaveCoordinates(.Lbllcnodot)
  ENDWITH
ENDWITH 

IF TYPE("loFormSet.ariaform1.ariapageframe1.page1.txtLCno") <> 'O'
  *--Add object for display Pack Container
  loFormSet.ariaform1.ariapageframe1.page1.AddObject("txtLCno","txtLCnocls")
ENDIF 
WITH loFormSet.ariaform1.ariapageframe1.page1.txtLCno
  .Visible =.T.
  .left= loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.left
  .Top = loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.top + loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.height + 5
  *khm
  .Width = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.width - 100
  *khm
  .enabled = .F.
  WITH loFormSet.ariaform1.ariapageframe1.page1
    loFormSet.ariaform1.oResizer.SaveCoordinates(.txtLCno)
  ENDWITH
ENDWITH 

IF TYPE("loFormSet.ariaform1.ariapageframe1.page1.lblLCBank") <> 'O'
  *--Add object for display Pack Container
  loFormSet.ariaform1.ariapageframe1.page1.AddObject("lblLCBank","lblLCBankcls")
ENDIF 
WITH loFormSet.ariaform1.ariapageframe1.page1.lblLCBank
  .Visible =.T.
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.left + 50
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.top + loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.height + 5

  .left= loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.left + 118
  .Top = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.top + loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.height + 8
  *khm
  
  WITH loFormSet.ariaform1.ariapageframe1.page1
    loFormSet.ariaform1.oResizer.SaveCoordinates(.lblLCBank)
  ENDWITH
ENDWITH 

IF TYPE("loFormSet.ariaform1.ariapageframe1.page1.Lbllcbankdot") <> 'O'
  *--Add object for display Pack Container
  loFormSet.ariaform1.ariapageframe1.page1.AddObject("Lbllcbankdot","lbldotcls")
ENDIF 
WITH loFormSet.ariaform1.ariapageframe1.page1.Lbllcbankdot
  .Visible =.T.
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.left + 142
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.top + loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.height + 5
  .left= loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.left + 210
  .Top = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.top + loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.height + 8
  *khm
  
  WITH loFormSet.ariaform1.ariapageframe1.page1
    loFormSet.ariaform1.oResizer.SaveCoordinates(.Lbllcbankdot)
  ENDWITH
ENDWITH 

IF TYPE("loFormSet.ariaform1.ariapageframe1.page1.txtLCBank") <> 'O'
  *--Add object for display Pack Container
  loFormSet.ariaform1.ariapageframe1.page1.AddObject("txtLCBank","txtLCbankcls")
ENDIF 
WITH loFormSet.ariaform1.ariapageframe1.page1.txtLCBank
  .Visible =.T.
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.left + 151
  .left= loFormSet.ariaform1.ariapageframe1.page1.Lbllcbankdot.left + 5
  *khm
  
  .Top = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.top + loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.height + 5
  *khm
  .Width = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.width - 100
  *khm

  .enabled = .F.
  WITH loFormSet.ariaform1.ariapageframe1.page1
    loFormSet.ariaform1.oResizer.SaveCoordinates(.txtLCBank)
  ENDWITH
ENDWITH 

IF TYPE("loFormSet.ariaform1.ariapageframe1.page1.lblLCDate") <> 'O'
  *--Add object for display Pack Container
  loFormSet.ariaform1.ariapageframe1.page1.AddObject("lblLCDate","lblLCDatecls")
ENDIF 
WITH loFormSet.ariaform1.ariapageframe1.page1.lblLCDate
  .Visible =.T.
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.lblCurrency.left
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.top + loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.height + 5
  .left= loFormSet.ariaform1.ariapageframe1.page1.lblExRate.left - 15
  .Top = loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.top + loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.height + 8
  *khm
 
  WITH loFormSet.ariaform1.ariapageframe1.page1
    loFormSet.ariaform1.oResizer.SaveCoordinates(.lblLCDate)
  ENDWITH
ENDWITH 

IF TYPE("loFormSet.ariaform1.ariapageframe1.page1.LbllcDatedot") <> 'O'
  *--Add object for display Pack Container
  loFormSet.ariaform1.ariapageframe1.page1.AddObject("LbllcDatedot","lbldotcls")
ENDIF 
WITH loFormSet.ariaform1.ariapageframe1.page1.LbllcDatedot
  .Visible =.T.
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.lblCurrency.left + 96
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.top + loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.height + 5
  .left= loFormSet.ariaform1.ariapageframe1.page1.lblLCDate.left + 90  
  .Top = loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.top + loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.height + 8
  *khm
 
  WITH loFormSet.ariaform1.ariapageframe1.page1
    loFormSet.ariaform1.oResizer.SaveCoordinates(.LbllcDatedot)
  ENDWITH
ENDWITH 

IF TYPE("loFormSet.ariaform1.ariapageframe1.page1.Dtlcdate") <> 'O'
  *--Add object for display Pack Container
  loFormSet.ariaform1.ariapageframe1.page1.AddObject("Dtlcdate","Dtlcdatecls")
ENDIF 
WITH loFormSet.ariaform1.ariapageframe1.page1.Dtlcdate
  .Visible =.T.
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.lblCurrency.left + 103  
  .left= loFormSet.ariaform1.ariapageframe1.page1.LbllcDatedot.left + 5
  *khm
  .Top = loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.top + loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.height + 5

  *khm
  .Width = loFormSet.ariaform1.ariapageframe1.page1.txtEntered.Width
  *khm

  .enabled = .F.
  .text1.enabled = .F.
  WITH loFormSet.ariaform1.ariapageframe1.page1
    loFormSet.ariaform1.oResizer.SaveCoordinates(.Dtlcdate)
  ENDWITH
ENDWITH 
*C128356,1 [End]

*!*************************************************************
*! Name      : lfADJPOSS
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Due to C037922,1
*!*************************************************************
*! Example   : =lfADJPOSS()
*!*************************************************************
*!
FUNCTION lfADJPOSS


WITH loFormSet.ariaform1.ariapageframe1.page1.lblLCNo
  .left= loFormSet.ariaform1.ariapageframe1.page1.Arialabel10.left
  *khm
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.top + loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.height + 5
  .Top = loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.top + loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.height + 8
  *khm
ENDWITH 

WITH loFormSet.ariaform1.ariapageframe1.page1.Lbllcnodot
  .left= loFormSet.ariaform1.ariapageframe1.page1.Arialabel5.left
  *khm
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.top + loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.height + 5
  .Top = loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.top + loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.height + 8
  *khm
ENDWITH 

WITH loFormSet.ariaform1.ariapageframe1.page1.txtLCno
  .left= loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.left
  .Top = loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.top + loFormSet.ariaform1.ariapageframe1.page1.KeyDepartment.height + 5

ENDWITH 

WITH loFormSet.ariaform1.ariapageframe1.page1.lblLCBank
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.left + 50
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.top + loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.height + 5
  .left= loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.left + 118
  .Top = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.top + loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.height + 8
  *khm
  
ENDWITH 

WITH loFormSet.ariaform1.ariapageframe1.page1.Lbllcbankdot
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.left + 142
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.top + loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.height + 5
  .left= loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.left + 210
  .Top = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.top + loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.height + 8
  *khm
  
ENDWITH 

WITH loFormSet.ariaform1.ariapageframe1.page1.txtLCBank
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.left + 151
  .left= loFormSet.ariaform1.ariapageframe1.page1.Lbllcbankdot.left + 5
  *khm
  
  .Top = loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.top + loFormSet.ariaform1.ariapageframe1.page1.txtDepartName.height + 5
ENDWITH 

WITH loFormSet.ariaform1.ariapageframe1.page1.lblLCDate
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.lblCurrency.left
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.top + loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.height + 5
  .left= loFormSet.ariaform1.ariapageframe1.page1.lblExRate.left - 15
  .Top = loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.top + loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.height + 8
  *khm
ENDWITH 

WITH loFormSet.ariaform1.ariapageframe1.page1.LbllcDatedot
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.lblCurrency.left + 96
  *.Top = loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.top + loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.height + 5
  .left= loFormSet.ariaform1.ariapageframe1.page1.lblLCDate.left + 90  
  .Top = loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.top + loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.height + 8
  *khm
ENDWITH 

WITH loFormSet.ariaform1.ariapageframe1.page1.Dtlcdate
  *khm
  *.left= loFormSet.ariaform1.ariapageframe1.page1.lblCurrency.left + 103
  .left= loFormSet.ariaform1.ariapageframe1.page1.LbllcDatedot.left + 5
  *khm
  .Top = loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.top + loFormSet.ariaform1.ariapageframe1.page1.keyCurrency.height + 5
ENDWITH 

*!*************************************************************
*! Name      : lfIntDINV
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : Intiate variables
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Due to C037922,1
*!*************************************************************
*! Example   : =lfINTSORD()
*!*************************************************************
*!
FUNCTION lfINTDINV

loFormSet.AddProperty('llRngPack',.F.)
*HBG 01/17/2005 Disable Show lines by pack option if there is no Packs [Begin]
loFormSet.AddProperty('llNtPacks',.F.)
loFormSet.AddProperty('llFromSO',.F.)
loFormSet.AddProperty('llFromARS',.T.)
*HBG [End]

loFormSet.AddProperty('llShowPack',.F.)
loFormSet.AddProperty('llSummaryByPack',.F.)
loFormSet.AddProperty('llRangePck',.F.)
loFormSet.AddProperty('lcPackSum',gfTempName())
=gfOpenFile(oAriaApplication.DataDir+'STYLEUPC','GMAPKUPC','SH')
=gfOpenFile(oAriaApplication.DataDir+'SPCK_HDR','SPCK_HDRST','SH')
=gfOpenFile(oAriaApplication.DataDir+'SPCK_LIN','SPCK_LINVR','SH')



*!*************************************************************
*! Name      : lfINTSINV
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : Intiate variables
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Due to C037922,1
*!*************************************************************
*! Example   : =lfINTSINV()
*!*************************************************************
*!
FUNCTION lfINTSINV

loFormSet.AddProperty('llRangePck',.F.)
*HBG 01/17/2005 Disable Show lines by pack option if there is no Packs [Begin]
loFormSet.AddProperty('llNtPacks',.F.)
loFormSet.AddProperty('llFromSO',.F.)
loFormSet.AddProperty('llFromARS',.F.)
*HBG [End]

loFormSet.AddProperty('llShowPack',.F.)
loFormSet.AddProperty('lcGMAInvH',gfTempName())
loFormSet.AddProperty('lcTmpFle',gfTempName())
loFormSet.AddProperty('lnShipAmnt',0)
loFormSet.AddProperty('lcPack_id',"")
loFormSet.AddProperty('lnPackQty',0)
loFormSet.AddProperty('llBackOrd',.T.)
loFormSet.AddProperty('lcNewPackId',"")
loFormSet.AddProperty('lcNewPkColor',"")
loFormSet.AddProperty('lcNewPkSize',"")
loFormSet.AddProperty('lcNewPkVersion',"")

CREATE CURSOR (loFormSet.lcTmpFle) (cPack_id C(34))
INDEX ON cPack_id TAG (loFormSet.lcTmpFle)

=gfOpenFile(oAriaApplication.DataDir+'STYLEUPC','GMAPKUPC','SH')
=gfOpenFile(oAriaApplication.DataDir+'SPCK_HDR','SPCK_HDRST','SH')
=gfOpenFile(oAriaApplication.DataDir+'SPCK_LIN','SPCK_LINVR','SH')
*C127844,1 HBG 08/23/2005 Add total Assistcost field to summary folder [Begin]
IF TYPE("loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.lblTotAss") <> 'O'
  *--Add object for display Pack Container
  loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.AddObject("lblTotAss","lblTotAssist")
ENDIF 
WITH loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.lblTotAss
  .Visible =.T.
  .left= loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.lblCartons.left
  .Top = loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.top+;
       loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.height + 5
       
  WITH loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary
    loFormSet.ariaform1.oResizer.SaveCoordinates(.lblTotAss)
  ENDWITH
ENDWITH 


IF TYPE("loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.lblTotAssDot") <> 'O'
  *--Add object for display Pack Container
  loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.AddObject("lblTotAssDot","lbldotcls")
ENDIF 
WITH loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.lblTotAssDot
  .Visible =.T.
  .left= loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.Arialabel34.left 
  .Top = loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.top +;
       loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.height + 5
 
  WITH loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary
    loFormSet.ariaform1.oResizer.SaveCoordinates(.lblTotAssDot)
  ENDWITH
ENDWITH 

IF TYPE("loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtTotAss") <> 'O'
  *--Add object for display Pack Container
  loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.AddObject("txtTotAss","txtTotAssist")
ENDIF 
WITH loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtTotAss
  .Visible =.T.
  .left= loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.left
  .Top = loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.top+;
       loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.height + 5
  .enabled = .F.
  .ReadOnly = .T.     
  WITH loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary
    loFormSet.ariaform1.oResizer.SaveCoordinates(.txtTotAss)
  ENDWITH
ENDWITH 
*C127844,1 [End]

*!*************************************************************
*! Name      : lfADJARPS
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Due to C037922,1
*!*************************************************************
*! Example   : =lfADJARPS()
*!*************************************************************
*C127844
FUNCTION lfADJARPS

IF TYPE('loFormSet.AriaForm1') = 'O' AND loFormSet.AriaForm1 <> .NULL.
  WITH loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.lblTotAss
    .left= loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.lblCartons.left
    .Top = loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.top+;
         loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.height + 5
  ENDWITH 

  WITH loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.lblTotAssDot
    .left= loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.Arialabel34.left 
    .Top = loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.top +;
         loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.height + 5
  ENDWITH 

  WITH loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtTotAss
    .left= loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.left
    .Top = loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.top+;
       loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary.txtCartons.height + 5
  ENDWITH 
ENDIF
*!**************************************************************************
*! Name      : lfOrAddBar
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : Add New options to otion menu
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
FUNCTION lfOrAddBar
lnNextBar = loFormSet.NextOptionBar

lcHostFormName = '[' + loFormSet.cHostFormName + ']'
*HBG 01/18/2005 Disable Style inquire option in case of Show pack summary [Begin]
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*DEFINE BAR 6  OF _INQURYPOP PROMPT LANG_OptionStyleInquire    SKIP  FOR  IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.ariapageframe1.ActivePage<>2) .OR. EMPTY(_screen.ActiveForm.AriaPageFrame1.Page2.AriaEditRegion1.KeyStyle.Value) .OR. ((_screen.ActiveForm.Parent.lnSortBy=3 OR _screen.ActiveForm.Parent.llShowPack) .AND. !_screen.ActiveForm.Parent.llDetails),.T.)
DEFINE BAR 6  OF _INQURYPOP PROMPT LANG_OptionStyleInquire    SKIP  FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.ariapageframe1.ActivePage<>2) .OR. EMPTY(_screen.ActiveForm.AriaPageFrame1.Page2.AriaEditRegion1.KeyStyle.Value) .OR. ((_screen.ActiveForm.Parent.lnSortBy=3 OR _screen.ActiveForm.Parent.llShowPack) .AND. !_screen.ActiveForm.Parent.llDetails),.T.)
*HBG [End]
*HBG [End]
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*DEFINE BAR 11 OF _INQURYPOP PROMPT LANG_OptionShowSummary  SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.ariapageframe1.ActivePage<>2) .OR. (_Screen.ActiveForm.Parent.lnSortBy=2 AND !_screen.ActiveForm.chkMultiStore.Value) .OR. (_Screen.ActiveForm.Parent.lnSortBy=1 AND !_screen.ActiveForm.Parent.llShowPack) OR INLIST(_Screen.ActiveForm.Parent.lnOrdTrans,4,5,6,7) OR _screen.ActiveForm.Parent.lEditLines,.T.)
DEFINE BAR 11 OF _INQURYPOP PROMPT LANG_OptionShowSummary  SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.ariapageframe1.ActivePage<>2) .OR. (_Screen.ActiveForm.Parent.lnSortBy=2 AND !_screen.ActiveForm.chkMultiStore.Value) .OR. (_Screen.ActiveForm.Parent.lnSortBy=1 AND !_screen.ActiveForm.Parent.llShowPack) OR INLIST(_Screen.ActiveForm.Parent.lnOrdTrans,4,5,6,7) OR _screen.ActiveForm.Parent.lEditLines,.T.)
*HBG [End]
*-- Update EDI Fields
DEFINE BAR lnNextBar OF _INQURYPOP PROMPT LANG_GMA_UpdateEDIFields
*-- Show order lines per pack
*HBG 01/17/2005 Disable Show lines by pack option if there is no Packs [Begin]
*DEFINE BAR lnNextBar+1  OF _INQURYPOP PROMPT LANG_GMA_ShowLinesbyPack  SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.ActiveMode='S'),.T.)
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*!*  DEFINE BAR lnNextBar+1  OF _INQURYPOP PROMPT LANG_GMA_ShowLinesbyPack  SKIP IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",;
*!*      (_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.llNtPacks) OR;
*!*      (_screen.ActiveForm.Parent.ActiveMode='S'),.T.)
DEFINE BAR lnNextBar+1  OF _INQURYPOP PROMPT LANG_GMA_ShowLinesbyPack  SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",;
    (_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.llNtPacks) OR;
    (_screen.ActiveForm.Parent.ActiveMode='S'),.T.)
*HBG [End]
*HBG [End]
*-- Sort by Pack
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*DEFINE BAR lnNextBar+2  OF _INQURYPOP PROMPT LANG_GMA_SortLinesbyPack SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.ActiveMode='S') OR !(_screen.ActiveForm.Parent.llShowPack),.T.)
DEFINE BAR lnNextBar+2  OF _INQURYPOP PROMPT LANG_GMA_SortLinesbyPack SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.ActiveMode='S') OR !(_screen.ActiveForm.Parent.llShowPack),.T.)
*HBG [End]
*-- Modify # of ordered packs
*E125004,1 HBG 12/12/2004 Enable the option "Modify # of ordered packs" in case of bulk order [Begin]
*!*  DEFINE BAR lnNextBar+3 OF _INQURYPOP PROMPT LANG_GMA_ModifyPacksOrd SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage <> 2) .OR.;
*!*         (_screen.ActiveForm.Parent.ActiveMode = 'V') .OR. ;
*!*         (_screen.ActiveForm.Parent.llRngPack) .OR. (_screen.ActiveForm.Parent.llFromBulk),.T.)
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*!*  DEFINE BAR lnNextBar+3 OF _INQURYPOP PROMPT LANG_GMA_ModifyPacksOrd SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage <> 2) .OR.;
*!*         (_screen.ActiveForm.Parent.ActiveMode = 'V') .OR. ;
*!*         (_screen.ActiveForm.Parent.llRngPack),.T.)
DEFINE BAR lnNextBar+3 OF _INQURYPOP PROMPT LANG_GMA_ModifyPacksOrd SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage <> 2) .OR.;
       (_screen.ActiveForm.Parent.ActiveMode = 'V') .OR. ;
       (_screen.ActiveForm.Parent.llRngPack),.T.)
*HBG [End]
*E125004,1 [End]
*!*  IF _screen.ActiveForm.Parent.llSortByPack     
*!*    SET MARK OF BAR loFormSet.NextOptionBar+2 OF _INQURYPOP TO .F.
*!*    _screen.ActiveForm.Parent.llSortByPack   = .F.
*!*  ENDIF

*C128356,1 HBG 08/23/2005 Add option to update L\C fields in ine level [Begin]
DEFINE BAR lnNextBar+4 OF _INQURYPOP PROMPT LANG_GMA_OptionUpdLC SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage <> 2) .OR.;
       (_screen.ActiveForm.Parent.ActiveMode = 'V'),.T.)
*C128356,1 [End]


*!* C200938,1 MMT 02/13/2008 Add CustPo and Retail price screen to So Screen[Start]
DEFINE BAR lnNextBar+5 OF _INQURYPOP PROMPT LANG_GMA_EDTCST  SKIP FOR gfFormIsActive(&lcHostFormName) AND ;
											IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage <> 2)OR ;
                      						_screen.ActiveForm.Parent.ActiveMode = 'S' OR (_screen.ActiveForm.Parent.ActiveMode = 'V' AND _screen.ActiveForm.cboStatus.value = 'C'),.T.)




*!* C200938,1 MMT 02/13/2008 Add CustPo and Retail price screen to So Screen[End]




*!**************************************************************************
*! Name      : lfOrRunBar
*! Developer : Hend Ghanem (HBG)
*! Date      : 08/23/2005
*! Purpose   : Add New options to option menu
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
FUNCTION lfOrRunBar
DO CASE 
  CASE BAR() = loFormSet.NextOptionBar     && Update EDI Fields
    DO FORM (oAriaApplication.ScreenHome+"SO\SOEDIFLD") WITH _screen.ActiveForm.Parent.ActiveMode<>'V'
  CASE BAR() = loFormSet.NextOptionBar+1   && Show order lines per pack
    loFormSet.llShowPack = !loFormSet.llShowPack
    IF loFormSet.llShowPack
      SET MARK OF BAR loFormSet.NextOptionBar+1 OF _INQURYPOP TO .T.
    ELSE
      SET MARK OF BAR loFormSet.NextOptionBar+1 OF _INQURYPOP TO .F.
      IF loFormSet.lnSortBy = 4
        loFormSet.lnSortBy = 1
        loFormSet.llDetails = .T.
        SET MARK OF BAR 11 OF _INQURYPOP TO .F.
        SET MARK OF BAR 7  OF _INQURYPOP TO .T.
        SET MARK OF BAR loFormSet.NextOptionBar+2 OF _INQURYPOP TO .F.
      ENDIF
    ENDIF
    loFormSet.mGetBrowse 
  CASE BAR() = loFormSet.NextOptionBar+2   && Sort by Pack
    loFormSet.lnSortBy = 4
    SET MARK OF BAR 7 OF _INQURYPOP TO .F.  
    SET MARK OF BAR 8 OF _INQURYPOP TO .F.
    SET MARK OF BAR 9 OF _INQURYPOP TO .F.
    SET MARK OF BAR loFormSet.NextOptionBar+2 OF _INQURYPOP TO .T.
    loFormSet.mGetBrowse 
  CASE BAR() = loFormSet.NextOptionBar+3   && Modify # of ordered packs
    =lfModfyVld()
  *C128356,1 HBG 08/23/2005 Add option to update L\C fields in ine level [Begin]  
  CASE BAR() = loFormSet.NextOptionBar+4   && Update L\C fields
    =lfUpdLCFld()
  *C128356,1 [End]  
  
  
  *!* C200938,1 MMT 02/13/2008 Add CustPo and Retail price screen to So Screen[Start]
  CASE BAR() = loFormSet.NextOptionBar+5   
    lfEDTSCRSO()
  *!* C200938,1 MMT 02/13/2008 Add CustPo and Retail price screen to So Screen[End]
  
ENDCASE

*!**************************************************************************
*! Name      : lfSHOWLCFL  
*! Developer : Hend Ghanem (HBG)
*! Date      : 08/23/2005
*! Purpose   : refresh L\C fields in header floder 
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
FUNCTION lfShowLCFL  

WITH loFormSet.ariaform1.ariapageframe1.page1
  DO CASE
    CASE  loFormSet.Activemode = 'S'
      STORE .F. TO .txtLCNo.enabled , .txtLCBank.Enabled , .dtLCDate.enabled
    CASE  loFormSet.Activemode = 'V'
      STORE .F. TO .txtLCNo.enabled , .txtLCBank.Enabled , .dtLCDate.enabled
    CASE  loFormSet.Activemode = 'E'
      STORE .T. TO .txtLCNo.enabled , .txtLCBank.Enabled , .dtLCDate.enabled
    CASE  loFormSet.Activemode = 'A'
      STORE .T. TO .txtLCNo.enabled , .txtLCBank.Enabled , .dtLCDate.enabled
  ENDCASE      
ENDWITH   

*!**************************************************************************
*! Name      : lfUpdLCFld
*! Developer : Hend Ghanem (HBG)
*! Date      : 08/22/2005
*! Purpose   : Call Update L\C fields screen
*!**************************************************************************
*! Example   : = lfUpdLCFld()
*!**************************************************************************
*! Due to C128356,1
*!**************************************************************************
FUNCTION lfUpdLCFld

llStorStat  = loFormSet.Ariaform1.chkMultiStore.Value
llCstPOStat = loFormSet.Ariaform1.chkMultiPo.Value
lcOrder     = loFormSet.Ariaform1.keyOrder.KeytextBox.value
SELECT (loFormSet.ariaForm1.Ariapageframe1.Page2.Ariaeditregion1.DetailFile)
LOCATE
IF EOF()
  =gfModalGen("INM00000B00000","Dialog",.F.,.F.,'Order have no lines.')
  RETURN 
ENDIF
DO FORM (oAriaApplication.ScreenHome+loFormSet.AriaForm1.moduleid+"\SOUPDLC") ;
    WITH loFormSet,loFormSet.lcOrdType,lcOrder,llStorStat,llCstPOStat

*!**************************************************************************
*! Name      : lfModfyVld
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : Call Modify # of ordered Packs screen
*!**************************************************************************
*! Example   : = lfModfyVld()
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
FUNCTION lfModfyVld

lcModPack  = gfTempName()

=lfGetPckDt()

SELECT (lcModPack)

*B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
*llEdiOrd = IIF(ORDHDR.cOrdType = 'T',.F.,.T.)
llEdiOrd = IIF(loFormSet.lcOrdType = 'T',.F.,.T.)
*B608434,1 WAM 02/14/2008 (End)

DIMENSION laStores[1] , laNewStr[1]   
STORE "" TO laStores , laNewStr
=gfSubStr(EVALUATE(lcModPack+'.MStore'),@laStores,"|")

IF !(loFormSet.AriaForm1.chkMultiStore.Value) AND !EMPTY(laStores[1])
  SELECT (lcModPack)
  SCAN
    IF !EMPTY(laStores[1])
      REPLACE MNewStr WITH EVALUATE(lcModPack+'.MNewStr') + laStores[1]
    ENDIF    
  ENDSCAN
ENDIF
SELECT (lcModPack)
LOCATE

IF EOF()
 =gfModalGen("INM00000B00000","Dialog",.F.,.F.,LANG_GMA_NoPacks)
 RETURN
ENDIF

DO FORM (oAriaApplication.ScreenHome+loFormSet.AriaForm1.moduleid+"\SOUPPCK") ;
    WITH loFormSet , loFormSet.DataSessionID , lcModPack , llEdiOrd 

loFormSet.AriaForm1.Closable = .F.
loFormSet.AriaForm1.Closable = .T.

    
*!**************************************************************************
*! Name      : lfGetPckDt
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfGetPckDt()
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
FUNCTION lfGetPckDt

CREATE DBF (oAriaApplication.WorkDir+lcModPack) ;
        (Type C(1),Account C(5),MStore M(10),Pack_Id C(16),cPkVersion C(4),nPkSlPrice N(8,2),;
         PackQty N(6),cPkColor C(6),cPckSize C(3),cPckSzeDsc C(6),UPC C(16),SKU C(16),PckNum N(6),;
         NewPkNum N(7),NewPrice N(8,2),NewPack C(16),NewColor C(6),NewSize C(3),NewSizDsc C(6),NewVer C(4),;
         MNewStr M(10),NewAcc C(5),lPrPerPiec L(1),llNew L(1),lPiktkt L(1))

INDEX ON Account+Pack_Id+cPkColor+cPckSize+cPkVersion TAG (lcModPack)

llFound = .F.
lcPrvAls = ALIAS()
lcPrvOrd = ORDER('SPCK_HDR')
SET ORDER TO SPCK_HDRVr IN SPCK_HDR
lcDetailFile = loFormSet.AriaForm1.Ariapageframe1.Page2.AriaEditRegion1.Detailfile

SELECT (lcDetailFile)
SCAN
  IF !EMPTY(Pack_ID)
    *E125004,1 HBG 12/12/2004 Enable the option "Modify # of ordered packs" in case of bulk order [Begin] 
    *!*      IF SEEK('P'+EVALUATE(lcDetailFile+'.cPckAcc')+EVALUATE(lcDetailFile+'.Pack_Id')+;
    *!*              EVALUATE(lcDetailFile+'.cpkcolor')+EVALUATE(lcDetailFile+'.cpcksize')+;
  *!*              EVALUATE(lcDetailFile+'.cPkVersion'),'SPCK_HDR') AND;
  *!*         !(EVALUATE(lcDetailFile+'.lRange')) AND EMPTY(EVALUATE(lcDetailFile+'.cFromOrder'))
    IF SEEK('P'+EVALUATE(lcDetailFile+'.cPckAcc')+EVALUATE(lcDetailFile+'.Pack_Id')+;
            EVALUATE(lcDetailFile+'.cpkcolor')+EVALUATE(lcDetailFile+'.cpcksize')+;
            EVALUATE(lcDetailFile+'.cPkVersion'),'SPCK_HDR') AND;
       !(EVALUATE(lcDetailFile+'.lRange'))
    *E125004,1 [End]
      llFound = .T.
      IF !SEEK(EVALUATE(lcDetailFile+'.cPckAcc')+EVALUATE(lcDetailFile+'.Pack_Id')+;
             EVALUATE(lcDetailFile+'.cpkcolor')+EVALUATE(lcDetailFile+'.cpcksize')+;
             EVALUATE(lcDetailFile+'.cPkVersion'),lcModPack) 
        INSERT INTO (lcModPack) (Type,Account,MStore,Pack_Id,cPkVersion,nPkSlPrice,;
                                 cPkColor,cPckSize,cPckSzeDsc,lPrPerPiec,lPiktkt);
                         VALUES ('P',EVALUATE(lcDetailFile+'.cPckAcc'),;
                                EVALUATE(lcDetailFile+'.Store'),;
                                 EVALUATE(lcDetailFile+'.Pack_Id'),;
                                 EVALUATE(lcDetailFile+'.cPkVersion'),;
                                 EVALUATE(lcDetailFile+'.nPkSlPrice'),;
                                 EVALUATE(lcDetailFile+'.cpkcolor'),;
                                 EVALUATE(lcDetailFile+'.cpcksize'),;
                                 lfGetGmSz(EVALUATE(lcDetailFile+'.cpcksize')),;
                                 EVALUATE(lcDetailFile+'.lPckPrPiec'),;
                                 EVALUATE(lcDetailFile+'.Picked'))
      ELSE
        SELECT (lcModPack)
        IF ATC( EVALUATE(lcDetailFile+'.Store'),MStore) = 0
          REPLACE MStore  WITH EVALUATE(lcModPack+'.MStore')+'|'+ EVALUATE(lcDetailFile+'.Store')
        ENDIF  
        REPLACE lPiktkt WITH IIF(lPiktkt,lPiktkt,EVALUATE(lcDetailFile+'.Picked'))
      ENDIF     
    ENDIF
  ENDIF
ENDSCAN
IF llFound 
  =lfGtPckNo()
  =lfGetPkInf()
ENDIF

SET ORDER TO (lcPrvOrd) IN SPCK_HDR
SELECT (lcPrvAls)              

*!**************************************************************************
*! Name      : lfGtPckNo
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfGetPkInf()
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
FUNCTION lfGtPckNo

SELECT (lcModPack)
LOCATE 
SCAN
  lcKey = Account+Pack_Id+cpkcolor+cpcksize+cPkVersion
  =SEEK('P'+lcKey,'SPCK_HDR')
  DIMENSION laStores[1]
  STORE "" TO laStores 
  =gfSubStr(EVALUATE(lcModPack+'.MStore'),@laStores,"|")
  FOR lnI =1 TO ALEN(laStores,1)
    SELECT (loFormSet.AriaForm1.Ariapageframe1.Page2.AriaEditRegion1.Detailfile)
    SUM TOTQTY FOR Store+cPckAcc+Pack_Id+cpkcolor+cpcksize+cPkVersion = PADR(laStores[lnI],8)+lcKey TO lnTotQty
    *SSH ?!?
    lnPackNo = lnTotQty/SPCK_hdr.nPckQty
    
    SELECT (lcModPack)
    REPLACE PckNum    WITH PckNum + lnPackNo 

  ENDFOR
ENDSCAN


*!**************************************************************************
*! Name      : lfGetPkInf
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfGetPkInf()
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
FUNCTION lfGetPkInf

=gfOpenFile(oAriaApplication.DataDir+'STYLEUPC',oAriaApplication.DataDir+'Gmapkupc','SH') 
SELECT (lcModPack)
SCAN
  SET ORDER TO Spck_hdrst IN SPCK_HDR
  IF SEEK('S'+EVALUATE(lcModPack+'.Account')+PADR(EVALUATE(lcModPack+'.Pack_Id'),19)+EVALUATE(lcModPack+'.cpkcolor')+;
          EVALUATE(lcModPack+'.cpcksize'),'SPCK_HDR')
    REPLACE SKU WITH SPCK_HDR.Pack_ID
  ENDIF
  
  SET ORDER TO Gmapkupc IN STYLEUPC
  IF SEEK(EVALUATE(lcModPack+'.Account')+PADR(EVALUATE(lcModPack+'.Pack_Id'),19)+EVALUATE(lcModPack+'.cpkcolor')+;
          EVALUATE(lcModPack+'.cpcksize'),'STYLEUPC')
    REPLACE UPC WITH (STYLEUPC.cupcnum1 + STYLEUPC.cupcnum2 + STYLEUPC.cupcnum3)
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : HEnd Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : Function To Size discreption
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Due to C037922,1
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfGetGmSz()
*!*************************************************************
FUNCTION lfGetGmSz
PARAMETER lcPackSize

*!*  IF ALLTRIM(lcPackSize) = ""
*!*    lcPackSize = " "
*!*  ENDIF
IF !EMPTY(lcPackSize) AND SEEK('S'+LEFT(lcPackSize,1),'SCALE')
  lcSiz = RIGHT(lcPackSize,1)
  lcSize = SCALE.Sz&lcSiz
ELSE
  lcSize = '*****'    
ENDIF  
RETURN lcSize 


*!**************************************************************************
*! Name      : lfAddOIndx
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : Add new index to temp ordline file
*!**************************************************************************
*! Example   : = lfAddOIndx()
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
FUNCTION lfAddOIndx

lnIndex = ALEN(laIndex,1)
DIMENSION laIndex[lnIndex+3,2]

laIndex[lnIndex+1,1] = 'cOrdType+Order+Store+PikTkt+Pack_id+cPkColor+cPckSize+cPkVersion'
laIndex[lnIndex+1,2] = 'Packs'
laIndex[lnIndex+2,1] = 'cPckAcc+Store+Pack_id+cPkColor+cPckSize+cPkVersion+Style'
laIndex[lnIndex+2,2] = 'AccPacks'
laIndex[lnIndex+3,1] = 'cordtype+order+pack_id+cpkcolor+cpcksize+cpkversion+STR(lineno,6)'
laIndex[lnIndex+3,2] = 'PACK_ID'

*!*************************************************************
*! Name      : lfGMAPACKS
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!************************************************************
*! Due to C037922,1
*!************************************************************
*! Example   : =lfGMAPACKS()
*!*************************************************************
*!
FUNCTION lfGMAPACKS
LOCAL lcPackHdr, lcDivision, lcSeason, lcAccount


loFormSet.AddProperty('lcTmpFle',gfTempName())
loFormSet.AddProperty('lnShipAmnt',0)
loFormSet.AddProperty('lcPack_id',"")
loFormSet.AddProperty('lnPackQty',0)

CREATE CURSOR (loFormSet.lcTmpFle) (cPack_id C(34))
INDEX ON cPack_id TAG (loFormSet.lcTmpFle)

lcHdrFile  = loFormSet.lcHdrFile
lcDivision = &lcHdrFile..cDivision
lcSeason   = &lcHdrFile..Season
lcAccount  = &lcHdrFile..Account

=gfOpenFile(oAriaApplication.DataDir+'StyleUpc',oAriaApplication.DataDir+'StyleUpc','SH')
SET ORDER TO TAG SPCK_LINVR IN SPCK_LIN 
SET ORDER TO TAG SPCK_HDRVR IN SPCK_HDR

lcPackHdr = loFormSet.lcPackHdr
CREATE DBF (oAriaApplication.WorkDir+lcPackHdr) ;
        (Type C(1),Account C(5),Pack_Id C(16),Desc C(20),cPkVersion C(4),nPkSlPrice N(8,2),;
         Season C(6),cDivision C(6),TotWip N(6),TotStk N(6),TotOrd N(6),;
         TotQty N(6),PackQty N(6),cPkColor C(6),cPckSize C(3),cPckSzeDsc C(5),lpckprpiec L(1),lRange L(1), cUpc C(13))
*B124535,1 HBG 12/22/2004 Fix bug of incremental search not working [Begin]
INDEX ON Pack_Id +cpkcolor+cpcksize+ cPkVersion TAG 'PackID'
*B124535,1 [End]
INDEX ON Account + Pack_Id +cpkcolor+cpcksize+ cPkVersion TAG (lcPackHdr)
lcVersion = IIF(loFormSet.ActiveMode ='V',OrdLine.Pack_ID+OrdLine.cPkColor+OrdLine.cpcksize+OrdLine.cPkVersion,'')
SELECT SPCK_HDR
=SEEK('P'+lcAccount+lcVersion)
SCAN REST WHILE Type+Account+Pack_id+cpkcolor+cpcksize+cpkversion = 'P'+lcAccount+lcVersion
  IF IIF(lcSeason='*',.T.,Season=lcSeason) .AND. cDivision= lcDivision
    IF SEEK(Type+Account+Pack_Id+cpkcolor+cpcksize+cPkVersion,'Spck_Lin')
      SCATTER MEMVAR
      SELECT Spck_Lin
      STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
      SCAN REST WHILE Type+Account+Pack_Id+cpkcolor+cpcksize+ cPkVersion+Style = ;
                      m.Type+m.Account+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion
        lnPackQty = lnPackQty + Spck_Lin.TotQty
        lnPackWip = lnPackWip + Style.TotWip
        lnPackOrd = lnPackOrd + Style.TotOrd
        lnPackStk = lnPackStk + Style.TotStk
      ENDSCAN
      lcUPc = ''
      IF SEEK(lcAccount+m.Pack_Id+SPACE(3)+m.cPkColor+m.cPckSize,'StyleUPc','GMAPKUPC')
        lcUPc = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
      ENDIF
      INSERT INTO (lcPackHdr) ;
      (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,;
       TotQty,cPkColor,cPckSize,cPckSzeDsc,lpckprpiec,lRange,cUpc) VALUES ;
      ('P',lcAccount,m.Pack_Id,m.Desc,m.Season,m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,;
       lnPackStk,lnPackOrd,lnPackQty,m.cPkColor,m.cPckSize,lfGetGmSz(m.cPckSize),m.lpckprpiec, m.lRange,lcUPc)
    ENDIF
  ENDIF
ENDSCAN  
=SEEK('P'+'*****'+lcVersion)
SCAN REST WHILE Type+Account+Pack_id+cpkcolor+cpcksize+cpkversion = 'P'+'*****'+lcVersion
  IF IIF(lcSeason='*',.T.,Season=lcSeason) .AND. cDivision= lcDivision
    IF SEEK(Type+Account+Pack_Id+cpkcolor+cpcksize+cPkVersion,'Spck_Lin')
      SCATTER MEMVAR
      SELECT Spck_Lin
      STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
      SCAN REST WHILE Type+Account+Pack_Id+cpkcolor+cpcksize+ cPkVersion+Style = ;
                      m.Type+m.Account+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion
        lnPackQty = lnPackQty+Spck_Lin.TotQty
        lnPackWip = lnPackWip+Style.TotWip
        lnPackOrd = lnPackOrd+Style.TotOrd
        lnPackStk = lnPackStk+Style.TotStk
      ENDSCAN
      lcUPc = ''
      IF SEEK('*****'+m.Pack_Id+SPACE(3)+m.cPkColor+m.cPckSize,'StyleUPc','GMAPKUPC')
        lcUPc = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
      ENDIF
      INSERT INTO (lcPackHdr) ;
      (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,;
       TotQty,cPkColor,cPckSize,cPckSzeDsc,lpckprpiec,lRange,cUPc) VALUES ;
      ('P','*****',m.Pack_Id,m.Desc,m.Season,m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,;
       lnPackStk,lnPackOrd,lnPackQty,m.cPkColor,m.cPckSize,lfGetGmSz(m.cPckSize),m.lpckprpiec, m.lRange,lcUPc)
    ENDIF
  ENDIF
ENDSCAN
SET ORDER TO TAG SPCK_LINVR IN SPCK_LIN
SELECT (lcPackHdr)
SET RELATION TO Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion INTO SPck_Lin
GO TOP
IF EOF()
  *-- Message : 32037
  *-- There are neither generic packs nor packs for account xxx.
  *-- Button : 00000
  *-- Ok
  *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
  *=gfModalGen('TRM32037B00000','ALERT',OrdHdr.Account)
  =gfModalGen('TRM32037B00000','ALERT',lcAccount)
  *B608434,1 WAM 02/14/2008 (End)
  RETURN .F.
ELSE
  
  WITH loFormSet.AriaForm1.Ariagrid1
    .RecordSource = lcPackHdr
    *B124535,1 HBG 12/22/2004 Fix bug of incremental search not working [Begin]
    .cSeekIndex = 'PackID'
    *B124535,1 [End]
    .ColumnCount  = 15
    .Account.ControlSource  = lcPackHdr+'.Account'
    .Pack_Id.ControlSource  = lcPackHdr+'.Pack_Id'
    .Desc.ControlSource     = lcPackHdr+'.Desc'
    .Pack.ControlSource     = lcPackHdr+'.PackQty'
    .WIP.ControlSource      = lcPackHdr+'.TotWip'
    .Stock.ControlSource    = lcPackHdr+'.TotStk'
    .Ordered.ControlSource  = lcPackHdr+'.TotOrd'
    .Quantity.ControlSource = lcPackHdr+'.TotQty'
    .Columns(11).Controlsource   = lcPackHdr+'.cPkColor'
    .Columns(11).Header1.Caption = 'Color'
    .Columns(11).AddObject('TEXT2','GRIDCOLUMN')
    .Columns(11).RemoveObject('TEXT1')
    .Columns(11).CurrentControl = 'TEXT2'
    .Columns(11).Text2.Visible = .T.
    .Columns(11).ReadOnly = .T.
    .Columns(12).Controlsource   = lcPackHdr+'.cPckSzeDsc'
    .Columns(12).Header1.Caption = 'Size'
    .Columns(12).AddObject('TEXT2','GRIDCOLUMN')
    .Columns(12).RemoveObject('TEXT1')
    .Columns(12).CurrentControl = 'TEXT2'
    .Columns(12).Text2.Visible = .T.
    .Columns(12).ReadOnly = .T.
    .Columns(13).Controlsource   = lcPackHdr+'.cPkVersion'
    .Columns(13).Header1.Caption = 'Version'
    .Columns(13).AddObject('TEXT2','GRIDCOLUMN')
    .Columns(13).RemoveObject('TEXT1')
    .Columns(13).CurrentControl = 'TEXT2'
    .Columns(13).Text2.Visible = .T.
    .Columns(13).ReadOnly = .T.
    .Columns(14).Controlsource   = lcPackHdr+'.nPkSlPrice'
    .Columns(14).Header1.Caption = 'Selling Price'
    .Columns(14).AddObject('TEXT2','GRIDCOLUMN')
    .Columns(14).RemoveObject('TEXT1')
    .Columns(14).CurrentControl = 'TEXT2'
    .Columns(14).Text2.Visible = .T.
    .Columns(14).ReadOnly = .T.
    .Columns(15).Controlsource   = lcPackHdr+'.cUpc'
    .Columns(15).Header1.Caption = 'UPC'
    .Columns(15).Width = 100
    .Columns(15).AddObject('TEXT2','GRIDCOLUMN')
    .Columns(15).RemoveObject('TEXT1')
    .Columns(15).CurrentControl = 'TEXT2'
    .Columns(15).Text2.Visible = .T.
    .Columns(15).ReadOnly = .T.
    .Account.ColumnOrder = 1
    .Pack_Id.ColumnOrder = 2
    .Desc.ColumnOrder = 3
    .Columns(11).ColumnOrder = 4
    .Columns(12).ColumnOrder = 5
    .Columns(13).ColumnOrder = 6
    .Columns(14).ColumnOrder = 7
    .Pack.ColumnOrder = 8
    .Columns(15).ColumnOrder = 9
    .WIP.ColumnOrder = 10
    .Stock.ColumnOrder = 11
    .Ordered.ColumnOrder = 12
    .quantity.ColumnOrder = 13
    .Season.ColumnOrder = 14
    .Division.ColumnOrder = 15
  ENDWITH 
ENDIF

*!**************************************************************************
*! Name      : lfUpdPkAc
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : Update order lines with pack information
*!**************************************************************************
*! Example   : = lfUpdPkAc()
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
FUNCTION lfUpdPkAc
*HBG 01/27/2005 Fix bug of not geting the style price if the price in pack is 0 [Begin]
*REPLACE Gros_Price WITH IIF(lContract,Gros_Price,Spck_Lin.npck_price) ,;
        Price      WITH Gros_Price*(100-Disc_Pcnt)/100 ,;
        cPkVersion WITH &lcPackHdr..cPkVersion ,;
        cPkColor   WITH &lcPackHdr..cPkColor   ,; 
        cPckSize   WITH &lcPackHdr..cPckSize   ,;
        nPkSlPrice WITH &lcPackHdr..nPkSlPrice ,;
        lRange     WITH &lcPackHdr..lRange     ,;
        lPckPrPiec WITH &lcPackHdr..lPckPrPiec ,; 
        cPckAcc    WITH &lcPackHdr..Account    ,;
        nPackNo    WITH &lcPackHdr..PackQty    ,;
        Dyelot     WITH Spck_LIn.Dyelot
REPLACE cPkVersion WITH &lcPackHdr..cPkVersion ,;
        cPkColor   WITH &lcPackHdr..cPkColor   ,; 
        cPckSize   WITH &lcPackHdr..cPckSize   ,;
        nPkSlPrice WITH &lcPackHdr..nPkSlPrice ,;
        lRange     WITH &lcPackHdr..lRange     ,;
        lPckPrPiec WITH &lcPackHdr..lPckPrPiec ,; 
        cPckAcc    WITH &lcPackHdr..Account    ,;
        nPackNo    WITH &lcPackHdr..PackQty    ,;
        Dyelot     WITH Spck_LIn.Dyelot
*N039487,1 KHM 11/14/2005 [Start]
IF llShipVal
  REPLACE lShip    WITH (TotQty <> 0)
ENDIF
*N039487,1 KHM 11/14/2005 [End]
*HBG [End]                

*!**************************************************************************
*! Name      : lfBRWSVAR
*! Developer : Wael Ali Mohamed
*! Date      : 04/11/2004
*! Purpose   : Display pack / Sort by Pack
*!**************************************************************************
*! Example   : = lfBRWSVAR()
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************

FUNCTION lfBRWSVAR

*B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
lcOrdHdr = IIF(INLIST(loFormSet.ActiveMode,'S','V'),'ORDHDR', loFormSet.OFORMENVIRONMENT.lcOrdHdr)
*B608434,1 WAM 02/14/2008 (End)

IF PAD('_INQUIRY') AND loFormSet.lnSortBy <> 4
  SET MARK OF BAR loFormSet.NextOptionBar+2 OF _INQURYPOP TO .F.
  *loFormSet.llSortByPack = .T.
ENDIF
DO CASE
   *C128356,1 HBG 08/23/2005 Add L\C fields to detail grid [Begin]
   CASE INLIST(loFormSet.lnSortBy,1,2) AND loFormSet.llDetails AND !loFormSet.llShowPack
     WITH loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1
      IF loFormSet.llQtyPSize
        .columnCount = 29
        .Columns(26).ControlSource = "cloc_no"
        .Columns(26).WIDTH = 170
        .Columns(26).Header1.Caption = LANG_GMA_LabelLCNo
        .Columns(27).ControlSource = "clocbank"
        .Columns(27).WIDTH = 170
        .Columns(27).Header1.Caption = LANG_GMA_LabelLCBank
        .Columns(28).ControlSource = "dlocissue"
        .Columns(28).WIDTH = 100
        .Columns(28).Header1.Caption = LANG_GMA_LabelLCDate
        .Columns(29).ControlSource = "nasscost"
        .Columns(29).WIDTH = 100
        .Columns(29).Header1.Caption = LANG_GMA_LabelAssist
      ELSE
        .columnCount = 18
        .Columns(15).ControlSource = "cloc_no"
        .Columns(15).WIDTH = 170
        .Columns(15).Header1.Caption = LANG_GMA_LabelLCNo
        .Columns(16).ControlSource = "clocbank"
        .Columns(16).WIDTH = 170
        .Columns(16).Header1.Caption = LANG_GMA_LabelLCBank
        .Columns(17).ControlSource = "dlocissue"
        .Columns(17).WIDTH = 100
        .Columns(17).Header1.Caption = LANG_GMA_LabelLCDate
        .Columns(18).ControlSource = "nasscost"
        .Columns(18).WIDTH = 100
        .Columns(18).Header1.Caption = LANG_GMA_LabelAssist
      ENDIF
     ENDWITH
    
   *-- Sort by Store
   CASE loFormSet.lnSortBy = 3 AND loFormSet.llDetails AND !loFormSet.llShowPack  
     WITH loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1
      IF loFormSet.llQtyPSize
        .columnCount = 32
        .Columns(29).ControlSource = "cloc_no"
        .Columns(29).WIDTH = 170
        .Columns(29).Header1.Caption = LANG_GMA_LabelLCNo
        .Columns(30).ControlSource = "clocbank"
        .Columns(30).WIDTH = 170
        .Columns(30).Header1.Caption = LANG_GMA_LabelLCBank
        .Columns(31).ControlSource = "dlocissue"
        .Columns(31).WIDTH = 100
        .Columns(31).Header1.Caption = LANG_GMA_LabelLCDate
        .Columns(32).ControlSource = "nasscost"
        .Columns(32).WIDTH = 100
        .Columns(32).Header1.Caption = LANG_GMA_LabelAssist        
      ELSE
        .columnCount = 18
        .Columns(15).ControlSource = "cloc_no"
        .Columns(15).WIDTH = 170
        .Columns(15).Header1.Caption = LANG_GMA_LabelLCNo
        .Columns(16).ControlSource = "clocbank"
        .Columns(16).WIDTH = 170
        .Columns(16).Header1.Caption = LANG_GMA_LabelLCBank
        .Columns(17).ControlSource = "dlocissue"
        .Columns(17).WIDTH = 100
        .Columns(17).Header1.Caption = LANG_GMA_LabelLCDate
        .Columns(18).ControlSource = "nasscost"
        .Columns(18).WIDTH = 100
        .Columns(18).Header1.Caption = LANG_GMA_LabelAssist
      ENDIF
     ENDWITH
  *C128356,1 [End]
  *-- Sort by line# or Sort by Style
  CASE INLIST(loFormSet.lnSortBy,1,2) AND loFormSet.llDetails AND loFormSet.llShowPack
    WITH loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1
      .ADDCOLUMN(3)
      .Columns(.ColumnCount).ControlSource = "PACK_ID+'-'+CPKCOLOR+'-'+lfGetGmSz(cPckSize)+'-'+CPKVERSION"
      .Columns(.ColumnCount).WIDTH = 200
      .Columns(.ColumnCount).Header1.Caption = LANG_GMA_PackTitle
      *C128356,1 HBG 08/23/2005 Add L\C fields to detail grid [Begin]
      IF loFormSet.llQtyPSize
        .ColumnCount = 30
        .Columns(27).ControlSource = "cloc_no"
        .Columns(27).WIDTH = 170
        .Columns(27).Header1.Caption = LANG_GMA_LabelLCNo
        .Columns(28).ControlSource = "clocbank"
        .Columns(28).WIDTH = 170
        .Columns(28).Header1.Caption = LANG_GMA_LabelLCBank
        .Columns(29).ControlSource = "dlocissue"
        .Columns(29).WIDTH = 100
        .Columns(29).Header1.Caption = LANG_GMA_LabelLCDate
        .Columns(30).ControlSource = "nasscost"
        .Columns(30).WIDTH = 100
        .Columns(30).Header1.Caption = LANG_GMA_LabelAssist
      ELSE
        .ColumnCount = 19
        .Columns(16).ControlSource = "cloc_no"
        .Columns(16).WIDTH = 170
        .Columns(16).Header1.Caption = LANG_GMA_LabelLCNo
        .Columns(17).ControlSource = "clocbank"
        .Columns(17).WIDTH = 170
        .Columns(17).Header1.Caption = LANG_GMA_LabelLCBank
        .Columns(18).ControlSource = "dlocissue"
        .Columns(18).WIDTH = 100
        .Columns(18).Header1.Caption = LANG_GMA_LabelLCDate
        .Columns(19).ControlSource = "nasscost"
        .Columns(19).WIDTH = 100
        .Columns(19).Header1.Caption = LANG_GMA_LabelAssist
      ENDIF
      *C128356,1 [End]
    ENDWITH
    
  *-- Sort by Store
  CASE loFormSet.lnSortBy = 3 AND loFormSet.llDetails AND loFormSet.llShowPack  
    WITH loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1
      .ADDCOLUMN(4)
      .Columns(.ColumnCount).ControlSource = "PACK_ID+'-'+CPKCOLOR+'-'+lfGetGmSz(cPckSize)+'-'+CPKVERSION"
      .Columns(.ColumnCount).WIDTH = 200
      .Columns(.ColumnCount).Header1.Caption = LANG_GMA_PackTitle
      *C128356,1 HBG 08/23/2005 Add L\C fields to detail grid [Begin]
      IF loFormSet.llQtyPSize
        .ColumnCount = 33
        .Columns(30).ControlSource = "cloc_no"
        .Columns(30).WIDTH = 170
        .Columns(30).Header1.Caption = LANG_GMA_LabelLCNo
        .Columns(31).ControlSource = "clocbank"
        .Columns(31).WIDTH = 170
        .Columns(31).Header1.Caption = LANG_GMA_LabelLCBank
        .Columns(32).ControlSource = "dlocissue"
        .Columns(32).WIDTH = 100
        .Columns(32).Header1.Caption = LANG_GMA_LabelLCDate
        .Columns(33).ControlSource = "nasscost"
        .Columns(33).WIDTH = 100
        .Columns(33).Header1.Caption = LANG_GMA_LabelAssist
      ELSE
        .ColumnCount = 18
        .Columns(16).ControlSource = "cloc_no"
        .Columns(16).WIDTH = 170
        .Columns(16).Header1.Caption = LANG_GMA_LabelLCNo
        .Columns(17).ControlSource = "clocbank"
        .Columns(17).WIDTH = 170
        .Columns(17).Header1.Caption = LANG_GMA_LabelLCBank
        .Columns(18).ControlSource = "dlocissue"
        .Columns(18).WIDTH = 100
        .Columns(18).Header1.Caption = LANG_GMA_LabelLCDate
        .Columns(19).ControlSource = "nasscost"
        .Columns(19).WIDTH = 100
        .Columns(19).Header1.Caption = LANG_GMA_LabelAssist
      ENDIF
      *C128356,1 [End]
    ENDWITH
  *-- Sort by pack
  CASE loFormSet.lnSortBy = 4 OR loFormSet.llShowPack
    IF loFormSet.llDetails
      *-- Show Pack details
      lcOrderFile = IIF(loFormSet.ActiveMode ='V','ORDLINE',loFormSet.oFormEnvironment.lcOrdLine)
      WITH loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1
        lcStyHeader = .Parent.Ariaeditregion1.keyStyle.lcItemHeader
        .ColumnCount = 0
        .RecordSource = lcOrderFile
        SELECT (lcOrderFile)
        SET ORDER TO TAG PACK_ID IN (lcOrderFile)
        *-- Rebuild browse columns
        IF loFormSet.llQtyPSize
          *-- Show Sizes
          *C128356,1 HBG 08/23/2005 Add L\C fields to detail grid [Begin] 
          *.ColumnCount = 26
          .ColumnCount = 30
          *C128356,1 [End]
          .Columns(1).ControlSource = lcOrderFile+".PACK_ID+'-'+"+lcOrderFile+".CPKCOLOR+'-'+lfGetGmSz("+lcOrderFile+".cPckSize)+'-'+"+lcOrderFile+'.CPKVERSION'
          .Columns(1).WIDTH = 200
          .Columns(1).Header1.Caption = LANG_GMA_PackTitle
          .Columns(2).ControlSource = lcOrderFile+'.Style'
          .Columns(2).WIDTH = 120
          .Columns(2).Header1.Caption = lcStyHeader
          .Columns(3).ControlSource = lcOrderFile+'.Dyelot'
          .Columns(3).Header1.Caption = IIF(loFormSet.oFormEnvironment.laSetups[22,2]="Y",LANG_GMA_LabelCOnfiguration,lang_LabelDyelot)
          .Columns(3).Name = 'Dyelot'
          .Columns(4).ControlSource = lcOrderFile+'.Store'
          .Columns(4).Header1.Caption = lang_LabelStore
          .Columns(5).ControlSource = lcOrderFile+'.CustPo'
          .Columns(5).WIDTH = 100
          .Columns(5).Header1.Caption = lang_LabelCustPo
          .Columns(6).ControlSource = lcOrderFile+'.TOTQTY'
          .Columns(6).WIDTH = 60
          .Columns(6).Header1.Caption = lang_LabelQuantity
          .Columns(7).ControlSource = lcOrderFile+'.PRICE*'+lcOrderFile+'.TOTQTY'
          .Columns(7).WIDTH = 60
          *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
          *.Columns(7).Header1.Caption = IIF(ORDHDR.cOrdType='T',lang_LabelWholeSale,lang_LabelAmount)
          .Columns(7).Header1.Caption = IIF(&lcOrdHdr..cOrdType='T',lang_LabelWholeSale,lang_LabelAmount)
          *B608434,1 WAM 02/14/2008 (End)

          .Columns(8).ControlSource = lcOrderFile+'.TOTPIK'
          .Columns(8).Header1.Caption = lang_LabelPicked
          .Columns(8).WIDTH = 60
          .Columns(9).ControlSource = lcOrderFile+'.QTY1'
          .Columns(9).Header1.Caption = lang_LabelQty1
          .Columns(9).WIDTH = 50
          .Columns(10).ControlSource = lcOrderFile+'.QTY2'
          .Columns(10).Header1.Caption = lang_LabelQty2
          .Columns(10).WIDTH = 50
          .Columns(11).ControlSource = lcOrderFile+'.QTY3'
          .Columns(11).Header1.Caption = lang_LabelQty3
          .Columns(11).WIDTH = 50
          .Columns(12).ControlSource = lcOrderFile+'.QTY4'
          .Columns(12).Header1.Caption = lang_LabelQty4
          .Columns(12).WIDTH = 50
          .Columns(13).ControlSource = lcOrderFile+'.QTY5'
          .Columns(13).Header1.Caption = lang_LabelQty5
          .Columns(13).WIDTH = 50
          .Columns(14).ControlSource = lcOrderFile+'.QTY6'
          .Columns(14).Header1.Caption = lang_LabelQty6
          .Columns(14).WIDTH = 50
          .Columns(15).ControlSource = lcOrderFile+'.QTY7'
          .Columns(15).Header1.Caption = lang_LabelQty7
          .Columns(15).WIDTH = 50
          .Columns(16).ControlSource = lcOrderFile+'.QTY8'
          .Columns(16).Header1.Caption = lang_LabelQty8
          .Columns(16).WIDTH = 50
          .Columns(17).ControlSource = lcOrderFile+'.PIK1'
          .Columns(17).Header1.Caption = lang_LabelPik1
          .Columns(17).WIDTH = 50
          .Columns(18).ControlSource = lcOrderFile+'.PIK2'
          .Columns(18).Header1.Caption = lang_LabelPik2
          .Columns(18).WIDTH = 50
          .Columns(19).ControlSource = lcOrderFile+'.PIK3'
          .Columns(19).Header1.Caption = lang_LabelPik3
          .Columns(19).WIDTH = 50
          .Columns(20).ControlSource = lcOrderFile+'.PIK4'
          .Columns(20).Header1.Caption = lang_LabelPik4
          .Columns(20).WIDTH = 50
          .Columns(21).ControlSource = lcOrderFile+'.PIK5'
          .Columns(21).Header1.Caption = lang_LabelPik5
          .Columns(21).WIDTH = 50
          .Columns(22).ControlSource = lcOrderFile+'.PIK6'
          .Columns(22).Header1.Caption = lang_LabelPik6
          .Columns(22).WIDTH = 50
          .Columns(23).ControlSource = lcOrderFile+'.PIK7'
          .Columns(23).Header1.Caption = lang_LabelPik7
          .Columns(23).WIDTH = 50
          .Columns(24).ControlSource = lcOrderFile+'.PIK8'
          .Columns(24).Header1.Caption = lang_LabelPik8
          .Columns(24).WIDTH = 50
          .Columns(25).ControlSource = lcOrderFile+'.Comm1'
          .Columns(25).Header1.Caption = lang_LabelComm1
          .Columns(25).WIDTH = 50
          .Columns(26).ControlSource = lcOrderFile+'.Comm2'
          .Columns(26).Header1.Caption = lang_LabelComm2
          .Columns(26).WIDTH = 50
          *C128356,1 HBG 08/23/2005 Add L\C fields to detail grid [Begin]
           Columns(27).ControlSource = lcOrderFile+'.cloc_no'
          .Columns(27).Header1.Caption = LANG_GMA_LabelLCNo
          .Columns(27).WIDTH = 170
          .Columns(28).ControlSource = lcOrderFile+'.clocbank'
          .Columns(28).Header1.Caption = LANG_GMA_LabelLCBank
          .Columns(28).WIDTH = 170
          .Columns(29).ControlSource = lcOrderFile+'.dlocissue'
          .Columns(29).Header1.Caption = LANG_GMA_LabelLCDate
          .Columns(29).WIDTH = 100         
          .Columns(30).ControlSource = "nasscost"
          .Columns(30).WIDTH = 100
          .Columns(30).Header1.Caption = LANG_GMA_LabelAssist
          *C128356,1 [End]
          IF loFormSet.oFormEnvironment.laSetups[14,2] <> "Y"
            .RemoveObject ('Dyelot')
          ENDIF
        ELSE
          *-- Total quantity
          *C128356,1 HBG 08/23/2005 Add L\C fields to detail grid [Begin]
          *.ColumnCount = 16
          .ColumnCount = 20
          *hbg
          .Columns(1).ControlSource = lcOrderFile+".PACK_ID+'-'+"+lcOrderFile+".CPKCOLOR+'-'+lfGetGmSz("+lcOrderFile+".cPckSize)+'-'+"+lcOrderFile+'.CPKVERSION'
          .Columns(1).WIDTH = 200
          .Columns(1).Header1.Caption = LANG_GMA_PackTitle
          .Columns(2).ControlSource = lcOrderFile+'.Style'
          .Columns(2).WIDTH = 120
          .Columns(2).Header1.Caption = lcStyHeader
          .Columns(3).ControlSource = lcOrderFile+'.Dyelot'
          .Columns(3).Header1.Caption = IIF(loFormSet.oFormEnvironment.laSetups[22,2]="Y",LANG_LabelCOnfiguration,lang_LabelDyelot)
          .Columns(3).Name = 'Dyelot'
          *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
          *.Columns(4).ControlSource = IIF(OrdHDr.Multi='Y',lcOrderFile+'.Store','ordhdr.Store')
          .Columns(4).ControlSource = IIF(&lcOrdHdr..Multi='Y',lcOrderFile+'.Store',lcOrdHdr+'.Store')
          *B608434,1 WAM 02/14/2008 (End)
          .Columns(4).Name = 'Store'
          .Columns(4).Header1.Caption = lang_LabelStore
          .Columns(5).ControlSource = lcOrderFile+'.CustPo'
          .Columns(5).WIDTH = 100
          .Columns(5).Header1.Caption = lang_LabelCustPo
          .Columns(5).Name = 'CustPo'
          .Columns(6).ControlSource = lcOrderFile+'.Group'
          .Columns(6).Header1.Caption = lang_LabelGroup
          .Columns(6).WIDTH = 35
          .Columns(7).ControlSource = lcOrderFile+'.Gros_Price'
          .Columns(7).WIDTH = 70
          .Columns(7).Header1.Caption = lang_LabelGrossPrice
          .Columns(8).ControlSource = lcOrderFile+'.Disc_Pcnt'
          .Columns(8).WIDTH = 50
          .Columns(8).Header1.Caption = lang_LabelDisc
          .Columns(9).ControlSource = lcOrderFile+'.Price'
          .Columns(9).WIDTH = 60
          *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
          *.Columns(9).Header1.Caption = IIF(ORDHDR.cOrdType='T',lang_LabelTransPrice,lang_LabelPrice)
          .Columns(9).Header1.Caption = IIF(&lcOrdHdr..cOrdType='T',lang_LabelTransPrice,lang_LabelPrice)
          *B608434,1 WAM 02/14/2008 (End)
          .Columns(10).ControlSource = lcOrderFile+'.TOTQTY'
          .Columns(10).WIDTH = 60
          .Columns(10).Header1.Caption = lang_LabelQuantity
          .Columns(11).ControlSource = lcOrderFile+'.PRICE*'+lcOrderFile+'.TOTQTY'
          .Columns(11).WIDTH = 60
          *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
          *.Columns(11).Header1.Caption = IIF(OrdHdr..cOrdType='T',lang_LabelWholeSale,lang_LabelAmount)
          .Columns(11).Header1.Caption = IIF(&lcOrdHdr..cOrdType='T',lang_LabelWholeSale,lang_LabelAmount)
          *B608434,1 WAM 02/14/2008 (End)
          .Columns(12).ControlSource = 'loFormSet.lfedistat()'
          .Columns(12).Header1.Caption = lang_LabelStatus
          .Columns(12).Name = 'Status'
          .Columns(13).ControlSource = lcOrderFile+'.PIKTKT'
          .Columns(13).Header1.Caption = lang_LabelPickTicket
          .Columns(14).ControlSource = lcOrderFile+'.TOTPIK'
          .Columns(14).Header1.Caption = lang_LabelPicked
          .Columns(15).ControlSource = lcOrderFile+'.PIKDATE'
          .Columns(15).Header1.Caption = lang_LabelPickDate
          .Columns(16).ControlSource   = 'lfClrDes()'
          .Columns(16).Header1.Caption = lang_LabelColorDesc
          .Columns(16).Name = 'ClrDesc'
          *C128356,1 [End]
          .Columns(17).ControlSource = lcOrderFile+'.cloc_no'
          .Columns(17).Header1.Caption = LANG_GMA_LabelLCNo
          .Columns(17).WIDTH = 170
          .Columns(18).ControlSource = lcOrderFile+'.clocbank'
          .Columns(18).Header1.Caption = LANG_GMA_LabelLCBank
          .Columns(18).WIDTH = 170
          .Columns(19).ControlSource = lcOrderFile+'.dlocissue'
          .Columns(19).Header1.Caption = LANG_GMA_LabelLCDate
          .Columns(19).WIDTH = 100  
          .Columns(20).ControlSource = "nasscost"
          .Columns(20).WIDTH = 100
          .Columns(20).Header1.Caption = LANG_GMA_LabelAssist       
          *C128356,1 [End]
          
          IF loFormSet.oFormEnvironment.laSetups[14,2] <> "Y"
            .RemoveObject('Dyelot')
          ENDIF
          IF !.Parent.AriaEditRegion1.UseItemColor
            .RemoveObject (ClrDesc)
          ENDIF
          *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
          *IF OrdHdr.cOrdType <> 'T'
          IF &lcOrdHdr..cOrdType <> 'T'
          *B608434,1 WAM 02/14/2008 (End)
            .RemoveObject('Status')
          ENDIF
        ENDIF
        .SETALL('ReadOnly',.T.,'COLUMN')
        .Refresh
      ENDWITH
    ELSE
      *-- Show PAck Summary 
      *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
      *IF !USED(loFormSet.lcPackSum) .OR. !SEEK(ordHdr.cOrdType+ordhdr.order,loFormSet.lcPackSum)
      *  lcOrdType = ordhdr.cordtype
      *  lcorder   = ordhdr.order
      IF !USED(loFormSet.lcPackSum) .OR. !SEEK(&lcOrdHdr..cOrdType+&lcOrdHdr..order,loFormSet.lcPackSum)
        lcOrdType = &lcOrdHdr..cordtype
        lcorder   = &lcOrdHdr..order
      *B608434,1 WAM 02/14/2008 (End)
        lcFile    = IIF(loFormSet.Activemode ='V','ORDLINE',loFormSet.oFormEnvironment.lcOrdLine)
        SELECT cOrdType,&lcFile..Order,STORE,PACK_ID,CPKCOLOR,cPckSize,CPKVERSION,nPkSlPrice,nPackNo ;
               FROM (lcFile) WHERE cOrdType+&lcFile..ORDER+STR(LINENO,6)= lcOrdType+lcorder ;
               GROUP BY cOrdType,&lcFile..Order,STORE,PACK_ID,CPKCOLOR,cPckSize,CPKVERSION,nPkSlPrice,nPackNo  INTO CURSOR (loFormSet.lcPackSum)
        lcPackSum = loFormSet.lcPackSum
        SELECT cOrdType,Order,&lcPackSum..PACK_ID,&lcPackSum..CPKCOLOR,&lcPackSum..cPckSize,&lcPackSum..CPKVERSION,&lcPackSum..nPkSlPrice,Spck_hdr.Desc,SUM(nPackNo) AS nPackNo;
               FROM (lcPackSum),Spck_hdr ;
               WHERE Spck_hdr.pack_id+Spck_hdr.cpkcolor+Spck_hdr.cpcksize+Spck_hdr.cpkversion = ;
               &lcPackSum..pack_id+&lcPackSum..cpkcolor+&lcPackSum..cpcksize+&lcPackSum..cpkversion ;
               GROUP BY cOrdType,Order,&lcPackSum..PACK_ID,&lcPackSum..CPKCOLOR,&lcPackSum..cPckSize,&lcPackSum..CPKVERSION,Spck_hdr.Desc,&lcPackSum..nPkSlPrice,nPackNo INTO CURSOR (loFormSet.lcPackSum)
        INDEX ON cOrdType+ORDER+PACK_ID+CPKCOLOR+cPckSize+CPKVERSION TAG (loFormSet.lcPackSum)
      ENDIF
      lcOrderFile = loFormSet.lcPackSum
      WITH loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1
        .ColumnCount=0
        .RecordSource = lcOrderFile
         SELECT (lcOrderFile)
        .columncount = 5
        .Columns(1).ControlSource = lcOrderFile+".PACK_ID+'-'+"+lcOrderFile+".CPKCOLOR+'-'+lfGetGmSz("+lcOrderFile+".cPckSize)+'-'+"+lcOrderFile+'.CPKVERSION'
        .Columns(1).WIDTH = 200
        .Columns(1).Header1.Caption = LANG_GMA_PackTitle
        .Columns(2).ControlSource = 'Desc'
        .Columns(2).WIDTH = 120
        .Columns(2).Header1.Caption = lang_LabelStyleDesc
        .Columns(3).ControlSource = lcOrderFile+'.nPackNo'
        .Columns(3).WIDTH = 60
        .Columns(3).Header1.Caption = LANG_GMA_PackQty
        .Columns(4).ControlSource = lcOrderFile+'.nPkSlPrice'
        .Columns(4).WIDTH = 70
        .Columns(4).Header1.Caption = LANG_GMA_PackPrice
        .Columns(5).ControlSource = lcOrderFile+'.nPkSlPrice*'+lcOrderFile+'.nPackNo'
        .Columns(5).WIDTH = 60
        .Columns(5).Header1.Caption = lang_LabelAmount
        .SetAll('ReadOnly',.T.,'COLUMN')
        .Refresh
      ENDWITH
    ENDIF
    SELECT (lcOrderFile)
    SET KEY TO 
    *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
    *SET KEY TO OrdHdr.cordtype+OrdHdr.order
    *=SEEK(OrdHdr.cordtype+OrdHdr.order)
    SET KEY TO &lcOrdHdr..cordtype+&lcOrdHdr..order
    =SEEK(&lcOrdHdr..cordtype+&lcOrdHdr..order)
    *B608434,1 WAM 02/14/2008 (End)
ENDCASE

*!*************************************************************
*! Name      : lfClrDes
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Due to C037922,1
*!*************************************************************
*! Example   : =lfClrDes()
*!*************************************************************
*!
FUNCTION lfClrDes

WITH _SCREEN.ActiveForm.Parent.AriaForm1.Ariapageframe1.Page2.AriaEditRegion1
RETURN gfCodDes(SUBSTR(Style,.ItemColorPosition,.ItemColorLength),"COLOR")
ENDWITH

*!**************************************************************************
*! Name      : lfPRCSHOW1
*! Developer : Wael Ali Mohamed
*! Date      : 04/11/2004
*! Purpose   : Control objects in sales order edit region
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
*! Example   : = lfPRCSHOW1()
*!**************************************************************************
FUNCTION lfPRCSHOW1

IF TYPE('_screen.ActiveForm.parent') = 'O'
  IF SEEK('P'+Account+pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPck_LIN','SPCK_LINVR') OR;
     SEEK('P'+'*****'+pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPck_LIN','SPCK_LINVR') 
    IF EMPTY(loFormSet.FormHasToolBar)
      WITH loFormSet.AriaForm1.AriaEditRegion1
        *E038463,1 HBG 12/12/2004 Enable the configuration field in case the line belong to pack [begin]
        *STORE .F. TO .keyStyle.Enabled, .KeyPack.Keytextbox.Enabled, .KeyPack.KeyCmd.Enabled,; 
                     .keyConfiguration.keytextbox.Enabled, .keyConfiguration.keyCmd.Enabled
        STORE .F. TO .keyStyle.Enabled, .KeyPack.Keytextbox.Enabled, .KeyPack.KeyCmd.Enabled
            
        *E038463,1 [End]                          
        IF !lPckPrPiec AND !lRange AND SPck_LIN.nPck_price <> 0 AND nPkSlPrice <> 0
          STORE .F. TO .txtGrossPrice.Enabled, .txtNetPrice.Enabled,.spnDiscount.Enabled,.txtRetailPrice.Enabled
        ENDIF  
        STORE lRange TO llRange
        IF !lRange
          WITH .cntQuantity
            STORE .F. TO .txtQty1.Enabled, .txtQty2.Enabled, .txtQty3.Enabled, .txtQty4.Enabled,;
                         .txtQty5.Enabled, .txtQty6.Enabled, .txtQty7.Enabled, .txtQty8.Enabled
          ENDWITH
          STORE .F. TO .txtQuantity.Enabled, .KeyPrepack.Keytextbox.Enabled, .KeyPrepack.KeyCmd.Enabled,;
                 .txtpQty.Enabled , .cmdPaste.Enabled
        ENDIF
      ENDWITH
    ELSE
      WITH loFormSet.AriaForm1.AriaPageFrame1.Page2.AriaEditRegion1
        *E038463,1 HBG 12/12/2004 Enable the configuration field in case the line belong to pack [begin]
        *STORE .F. TO .keyStyle.Enabled, .KeyPack.Keytextbox.Enabled, .KeyPack.KeyCmd.Enabled,; 
                     .keyConfiguration.keytextbox.Enabled, .keyConfiguration.keyCmd.Enabled
        STORE .F. TO .keyStyle.Enabled, .KeyPack.Keytextbox.Enabled, .KeyPack.KeyCmd.Enabled
        *E038463,1 [End]
        IF !lPckPrPiec AND !lRange AND SPck_LIN.nPck_price <> 0 AND nPkSlPrice <> 0
          STORE .F. TO .txtGrossPrice.Enabled, .txtNetPrice.Enabled,.spnDiscount.Enabled,.txtRetailPrice.Enabled
        ENDIF  
        STORE lRange TO loFormSet.llRangePck , llRange
        IF !lRange
          WITH .cntQuantity
            STORE .F. TO .txtQty1.Enabled, .txtQty2.Enabled, .txtQty3.Enabled, .txtQty4.Enabled,;
                         .txtQty5.Enabled, .txtQty6.Enabled, .txtQty7.Enabled, .txtQty8.Enabled
          ENDWITH
          STORE .F. TO .txtQuantity.Enabled, .KeyPrepack.Keytextbox.Enabled, .KeyPrepack.KeyCmd.Enabled,;
                 .txtpQty.Enabled , .cmdPaste.Enabled
        ENDIF
      ENDWITH
      *HBG 1/24/2005 Modify code to apply the new interface [Begin]
      *!*      DEFINE BAR 15 OF _INQURYPOP PROMPT LANG_OptionPasteLine  SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",;
    *!*            (_screen.ActiveForm.ariapageframe1.ActivePage<>2) .OR.;
    *!*            (_screen.ActiveForm.Parent.ActiveMode='V') .OR. ;
    *!*            EMPTY(_screen.ActiveForm.AriaPageFrame1.Page2.AriaEditRegion1.KeyStyle.Value) .OR.;
    *!*            _screen.ActiveForm.Parent.llZoom .OR. !(_Screen.ActiveForm.Parent.llRangePck),.T.)    
      lcHostFormName = '[' + loFormSet.cHostFormName + ']'
      DEFINE BAR 15 OF _INQURYPOP PROMPT LANG_OptionPasteLine  SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",;
            (_screen.ActiveForm.ariapageframe1.ActivePage<>2) .OR.;
            (_screen.ActiveForm.Parent.ActiveMode='V') .OR. ;
            EMPTY(_screen.ActiveForm.AriaPageFrame1.Page2.AriaEditRegion1.KeyStyle.Value) .OR.;
            _screen.ActiveForm.Parent.llZoom .OR. !(_Screen.ActiveForm.Parent.llRangePck),.T.)    
      *HBG [End]  
    ENDIF  
  ENDIF
  IF loFormSet.ActiveMode $ 'EA' 
    WITH loFormSet.AriaForm1.AriaPageFrame1.Page2.AriaEditRegion1
      STORE .T. TO .txtCustPo.Enabled , .txtRetailPrice.Enabled
    ENDWITH  
  ENDIF  
ENDIF

*!**************************************************************************
*! Name      : lfShowGet
*! Developer : Wael Ali Mohamed
*! Date      : 04/11/2004
*! Purpose   : Adjust direct invoice edit region custom controls
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
*! Example   : = lfShowGet()
*!**************************************************************************
FUNCTION lfShowGet

lcHostFormName = '[' + loFormSet.cHostFormName + ']'
IF SEEK('P'+Account+pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPck_LIN','SPCK_LINVR') OR;
   SEEK('P'+'*****'+pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPck_LIN','SPCK_LINVR') 
  WITH loFormSet.AriaForm1.AriaPageFrame1.Page2.InvoiceEditRegion1
    *E038463,1 HBG 12/12/2004 Enable the configuration field in case the line belong to pack [begin]
    *STORE .F. TO .keyStyle.Enabled, .KeyPack.Keytextbox.Enabled, .KeyPack.KeyCmd.Enabled,; 
                 .keyConfiguration.keytextbox.Enabled, .keyConfiguration.keyCmd.Enabled
    STORE .F. TO .keyStyle.Enabled, .KeyPack.Keytextbox.Enabled, .KeyPack.KeyCmd.Enabled
    *E038463,1 [End]                          
    IF !lPckPrPiec AND !lRange AND SPck_LIN.nPck_price <> 0 AND nPkSlPrice <> 0
      STORE .F. TO .txtGrossPrice.Enabled, .txtNetPrice.Enabled,.spnDiscount.Enabled
    ENDIF  
    STORE lRange TO loFormSet.llRangePck , llRange
    IF !lRange
      WITH .cntQuantity
        STORE llRange TO .txtQty1.Enabled, .txtQty2.Enabled, .txtQty3.Enabled, .txtQty4.Enabled,;
                     .txtQty5.Enabled, .txtQty6.Enabled, .txtQty7.Enabled, .txtQty8.Enabled
      ENDWITH
    ENDIF
    STORE llRange TO .txtQuantity.Enabled, .KeyPrepack.Keytextbox.Enabled, .KeyPrepack.KeyCmd.Enabled,;
                     .txtpQty.Enabled , .cmdPaste.Enabled
    
  ENDWITH
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*    DEFINE BAR 10 OF _INQURYPOP PROMPT LANG_OptionPasteLine SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",;
  *!*              (_screen.ActiveForm.ariapageframe1.ActivePage<>2) .OR.;
  *!*               _Screen.ActiveForm.Parent.ActiveMode = 'V' .OR.;
  *!*               _screen.ActiveForm.Parent.llZoom .OR. !(_Screen.ActiveForm.Parent.llRangePck),.T.) && .OR. EMPTY(m.Style) 
  DEFINE BAR 10 OF _INQURYPOP PROMPT LANG_OptionPasteLine SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",;
            (_screen.ActiveForm.ariapageframe1.ActivePage<>2) .OR.;
             _Screen.ActiveForm.Parent.ActiveMode = 'V' .OR.;
             _screen.ActiveForm.Parent.llZoom .OR. !(_Screen.ActiveForm.Parent.llRangePck),.T.) && .OR. EMPTY(m.Style) 
  *HBG [End] 
ENDIF

*!**************************************************************************
*! Name      : lfShowGetS
*! Developer : Wael Ali Mohamed
*! Date      : 04/11/2004
*! Purpose   : Adjust Sales order invoice edit region custom controls
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
*! Example   : = lfShowGetS()
*!**************************************************************************
FUNCTION lfShowGetS

lcHostFormName = '[' + loFormSet.cHostFormName + ']'
PRIVATE llEnable , llRange 
IF SEEK('P'+Account+pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPck_LIN','SPCK_LINVR') OR;
   SEEK('P'+'*****'+pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPck_LIN','SPCK_LINVR') 
  WITH loFormSet.AriaForm1.AriaPageFrame1.Page3.InvoiceEditRegion1
    *E038463,1 HBG 12/12/2004 Enable the configuration field in case the line belong to pack [begin]
    *STORE .F. TO .keyStyle.Enabled, .KeyPack.Keytextbox.Enabled, .KeyPack.KeyCmd.Enabled,; 
                 .keyConfiguration.keytextbox.Enabled, .keyConfiguration.keyCmd.Enabled
    STORE .F. TO .keyStyle.Enabled, .KeyPack.Keytextbox.Enabled, .KeyPack.KeyCmd.Enabled
    *E038463,1 [End]
    llEnable = !lPckPrPiec AND !lRange AND SPck_LIN.nPck_price <> 0 AND nPkSlPrice <> 0
    STORE !llEnable TO .txtGrossPrice.Enabled, .txtNetPrice.Enabled,.spnDiscount.Enabled
    STORE lRange TO loFormSet.llRangePck, llRange
    WITH .cntQuantity
      STORE llRange TO .txtQty1.Enabled, .txtQty2.Enabled, .txtQty3.Enabled, .txtQty4.Enabled,;
                       .txtQty5.Enabled, .txtQty6.Enabled, .txtQty7.Enabled, .txtQty8.Enabled
                       
    ENDWITH
    STORE llRange TO .txtQuantity.Enabled, .KeyPrepack.Keytextbox.Enabled, .KeyPrepack.KeyCmd.Enabled,;
                     .txtpQty.Enabled, .cmdPaste.Enabled
  ENDWITH
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*    DEFINE BAR 7 OF _INQURYPOP  PROMPT LANG_OptionPasteLine  SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_Screen.ActiveForm.Parent.AriaForm1.AriaPageFrame1.ActivePage <> 3) ;
  *!*                  .OR. EMPTY(_screen.ActiveForm.Parent.AriaForm1.AriaPageFrame1.Page3.InvoiceEditRegion1.KeyStyle.Value) .OR.;
  *!*                  _Screen.ActiveForm.Parent.llZoom .OR. !(_Screen.ActiveForm.Parent.llRangePck),.T.)
  DEFINE BAR 7 OF _INQURYPOP  PROMPT LANG_OptionPasteLine  SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_Screen.ActiveForm.Parent.AriaForm1.AriaPageFrame1.ActivePage <> 3) ;
                .OR. EMPTY(_screen.ActiveForm.Parent.AriaForm1.AriaPageFrame1.Page3.InvoiceEditRegion1.KeyStyle.Value) .OR.;
                _Screen.ActiveForm.Parent.llZoom .OR. !(_Screen.ActiveForm.Parent.llRangePck),.T.)
  *HBG [End] 
ENDIF

*!*************************************************************
*! Name      : lfBRWFLDS
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Due to C037922,1
*!**************************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfBRWFLDS()
*!*************************************************************
*!
FUNCTION lfBRWFLDS

WITH loFormSet.AriaForm1.AriaMultiSelectionGrid1.grdMultiSelectionGrid
  lnColumn = .ColumnCount+1
  .ADDCOLUMN(lnColumn)
  .Columns(lnColumn).ControlSource = "PACK_ID+'-'+CPKCOLOR+'-'+lfGetGmSz(cPckSize)+'-'+CPKVERSION"
  .Columns(lnColumn).WIDTH = 200
  .Columns(lnColumn).Header1.Caption = LANG_GMA_PackTitle
ENDWITH

*!**************************************************************************
*! Name      : lfGMARemov
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/31/2001
*! Purpose   : Remove all the Pack ID lines if one line is removed. 
*! Reference : C101946,1
*!**************************************************************************
*! Due to C037922,1
*!**************************************************************************
*! Example   :  =lfGMARemov()
*!**************************************************************************
*
FUNCTION lfGMARemov
LOCAL oEditRegion

IF EMPTY(Pack_Id)
  RETURN .F.
ENDIF
*-- Message < This line is included in a pack, do you want to remove the entire pack ? >
*-- Buttons <                                Yes     No                                >
IF gfModalGen('TRM00000B32000','DIALOG','','', LANG_GMA_InAPack) = 2
  RETURN
ENDIF
IF EMPTY(loFormSet.FormHasToolBar)
  oEditRegion = loFormSet.AriaForm1.AriaEditRegion1
ELSE
  oEditRegion = loFormSet.AriaForm1.Ariapageframe1.Page2.AriaEditRegion1
ENDIF
SELECT (oEditRegion.Detailfile)
lcCurStore = Store
lcCurPckID = Pack_id + cPkColor + cPckSize + cPkVersion
lnRecNo = RECNO()
llContenu = .T.
SCAN FOR Store + Pack_id + cPkColor + cPckSize + cPkVersion = lcCurStore + lcCurPckID
  IF !EMPTY(Piktkt)
    *-- Message < Style ..... in this pack has already been picked. In order to remove, release the pick ticket first. >
    *-- Buttons <                                Ok  
    =gfModalGen('TRM00000B00000','ALERT',' ',' ',oEditRegion.keyStyle.lblItemHeader.Caption + ' : ' + Style + LANG_GMA_PickedStyle)
    llContenu = .F.
    EXIT
  ENDIF
ENDSCAN
IF llContenu 
  SCAN FOR Store + Pack_id + cPkColor + cPckSize + cPkVersion = lcCurStore + lcCurPckID
    IF !oEditRegion.MultiStores 
      REPLACE FLAG WITH IIF(FLAG='N',FLAG,'M')
      SCATTER MEMVAR 
      IF m.TotCut > 0 
        SELECT CutPick
        =SEEK(IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6),'CutPick','CUTORD')
        SCAN REST WHILE TranCd+Order+cOrdLine = ;
                        IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6)
          IF !SEEK(CutPick.TranCd+CutPick.cTktNo+CutPick.cTktLineNo+;
                   CutPick.Order+CutPick.cOrdLine,loFormSet.lcAlocated,'CUTPKORD')
            INSERT INTO (loFormSet.lcAlocated) ;
             (cTktno,TranCd,cTktLineNo,Order,cOrdLine,Style) VALUES ;
             (CutPick.cTktNo,CutPick.Trancd,CutPick.cTktLineNo,CutPick.Order,;
             CutPick.cOrdLine,CutPick.Style)
          ENDIF
          REPLACE Qty1   WITH 0 ,;
                  Qty2   WITH 0 ,;
                  Qty3   WITH 0 ,;
                  Qty4   WITH 0 ,;
                  Qty5   WITH 0 ,;
                  Qty6   WITH 0 ,;
                  Qty7   WITH 0 ,;
                  Qty8   WITH 0 ,;
                  TotQty WITH 0 IN (loFormSet.lcAlocated)
          *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
          *REPLACE TotCut WITH TotCut - CutPick.TotQty IN OrdHDr
          REPLACE TotCut WITH TotCut - CutPick.TotQty IN (oEditRegion.HeaderFile)
          *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
        ENDSCAN
      ENDIF
      IF loFormSet.oFormEnvironment.ActiveMode='A' .AND. !EMPTY(m.cFromOrder)
        IF SEEK(STR(m.BulkLineNo,6),loFormSet.oFormEnvironment.lcBulkOrd) 
          SELECT (loFormSet.oFormEnvironment.lcBulkOrd)
          REPLACE USED1 WITH USED1 - m.Qty1 ,;
                  USED2 WITH USED2 - m.Qty2 ,;
                  USED3 WITH USED3 - m.Qty3 ,;
                  USED4 WITH USED4 - m.Qty4 ,;
                  USED5 WITH USED5 - m.Qty5 ,;
                  USED6 WITH USED6 - m.Qty6 ,;
                  USED7 WITH USED7 - m.Qty7 ,;
                  USED8 WITH USED8 - m.Qty8 ,;
                  TOTUSED WITH TOTUSED - m.TotQty
        ENDIF
      ENDIF
      *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
      *SELECT ORDHDR
      SELECT (oEditRegion.HeaderFile)
      *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header

      IF m.Flag = 'N' OR oEditRegion.llUpdBook
        REPLACE BOOK      WITH BOOK      - m.TotQty  ,;
                BOOKAMT   WITH BOOKAMT   - m.TotQty * m.Price ,;
                OPEN      WITH Open      - m.TotQty ,;
                OPENAMT   WITH OpenAmt   - m.TotQty * m.Price
      ELSE
        REPLACE CANCEL    WITH CANCEL    + m.TotQTy ,;
                CANCELAMT WITH CancelAmt + m.TotQTy* m.Price ,;
                OPEN      WITH Open      - m.TotQty ,;
                OPENAMT   WITH OpenAmt   - m.TotQty * m.Price
      ENDIF
      IF loFormSet.oFormEnvironment.ActiveMode='E' AND !oEditRegion.llUpdBook
        *-- Read Cancel date and reason
        ldCancelled = oAriaApplication.SystemDate
        lcCancReson = ''
        DO FORM (oAriaApplication.ScreenHome+"SOORDCLN") WITH 'ldCancelled','lcCancReson'
        
        INSERT INTO (loFormSet.lcOrdCanLn) ;
         (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
          Style,Account,Store,Dyelot,Price) VALUES ;
         (m.cOrdType,m.Order,m.LineNo,ldCancelled,lcCancReson,m.Qty1,m.Qty2,m.Qty3,;
          m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TOtQty,m.Style,m.Account,m.Store,m.Dyelot,m.Price)
      ENDIF 
    ENDIF
    IF oEditRegion.PriceAdder
      SELECT (oEditRegion.lcT_BomVar)
      =SEEK("SO"+m.Order+STR(m.LineNo,6))
      REPLACE REST cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1) ;
      WHILE   cIdType+cCost_Id+STR(LineNo,6) = "SO" + m.Order + STR(m.LineNo,6)

      =SEEK("SO"+m.Order+STR(m.LineNo,6))
      DELETE REST WHILE cIdType+cCost_Id+STR(LineNo,6) = "SO" + m.Order + STR(m.LineNo,6)
    ENDIF
    SELECT (oEditRegion.Detailfile) 
    DELETE
  ENDSCAN
ELSE
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF  
ENDIF
*!**************************************************************************
*! Name      : lfDIFBARD
*! Developer : Wael Ali Mohamed
*! Date      : 04/10/2004
*! Purpose   : Define custom bars for Direct Invoice
*! Reference : 
*!**************************************************************************
*! Example   :  =lfDIFBARD()
*!**************************************************************************
*
FUNCTION lfDIFBARD

lcHostFormName = '[' + loFormSet.cHostFormName + ']'
lnNextBar = loFormSet.NextOptionBar

*!* B610301,1 HIA 04/15/2013 Upgrade GMA project - Issue # 5 [Start]
lnNextBar = CNTBAR('_INQURYPOP')+1
*!* B610301,1 HIA 04/15/2013 Upgrade GMA project - Issue # 5 [End]

*-- Show order lines per pack
*HBG 01/18/2005 Disable Style inquire option in case of Show pack summary [Begin]
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*!*  DEFINE BAR 7  OF _INQURYPOP PROMPT LANG_OptionStyleInquire      SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.llShowPack AND _screen.ActiveForm.Parent.llSummaryByPack),.T.) 
DEFINE BAR 7  OF _INQURYPOP PROMPT LANG_OptionStyleInquire      SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.llShowPack AND _screen.ActiveForm.Parent.llSummaryByPack),.T.) 
*HBG [End]
*HBG [End]
*HBG 01/17/2005 Disable Show lines by pack option if there is no Packs [Begin]
*DEFINE BAR lnNextBar   OF _INQURYPOP PROMPT LANG_GMA_ShowLinesbyPack  SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.ActiveMode='S'),.T.)
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*!*  DEFINE BAR lnNextBar   OF _INQURYPOP PROMPT LANG_GMA_ShowLinesbyPack  SKIP FOR;
*!*    IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR;
*!*     (_screen.ActiveForm.Parent.llNtPacks) OR (_screen.ActiveForm.Parent.ActiveMode='S'),.T.)
DEFINE BAR lnNextBar   OF _INQURYPOP PROMPT LANG_GMA_ShowLinesbyPack  SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
  IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR;
   (_screen.ActiveForm.Parent.llNtPacks) OR (_screen.ActiveForm.Parent.ActiveMode='S'),.T.)
*HBG [End]
*HBG [End]
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*!*  DEFINE BAR lnNextBar+1 OF _INQURYPOP PROMPT LANG_GMA_SummarizebPack  SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.ActiveMode='S') OR !(_screen.ActiveForm.Parent.llShowPack),.T.)
*!*  DEFINE BAR 11 OF _INQURYPOP PROMPT lang_OptionZoomInvoiceLines   SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.ActiveMode='S') OR _screen.ActiveForm.Parent.llSummaryByPack,.T.)
DEFINE BAR lnNextBar+1 OF _INQURYPOP PROMPT LANG_GMA_SummarizebPack  SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.ActiveMode='S') OR !(_screen.ActiveForm.Parent.llShowPack),.T.)
DEFINE BAR 11 OF _INQURYPOP PROMPT lang_OptionZoomInvoiceLines   SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage<>2) OR (_screen.ActiveForm.Parent.ActiveMode='S') OR _screen.ActiveForm.Parent.llSummaryByPack,.T.)
*HBG [End]

*!**************************************************************************
*! Name      : lfDIFBARS
*! Developer : Hend Ghanem
*! Date      : 08/19/2004
*! Purpose   : Define custom bars for Sales order Invoice
*! Reference : 
*!**************************************************************************
*! Example   :  =lfDIFBARS()
*!**************************************************************************
*
FUNCTION lfDIFBARS

lcHostFormName = '[' + loFormSet.cHostFormName + ']'
lnNextBar = loFormSet.NextOptionBar
*-- Show order lines per pack
*HBG 01/17/2005 Disable Show lines by pack option if there is no Packs [Begin]
*!*  DEFINE BAR lnNextBar OF _INQURYPOP PROMPT LANG_GMA_ShowLinesbyPack  SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage <> 3) OR;
*!*                                         (_screen.ActiveForm.Parent.ActiveMode = 'S'),.T.)
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*!*  DEFINE BAR lnNextBar OF _INQURYPOP PROMPT LANG_GMA_ShowLinesbyPack  SKIP FOR IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage <> 3) OR;
*!*                                         (_screen.ActiveForm.Parent.llNtPacks) OR;
*!*                                         (_screen.ActiveForm.Parent.ActiveMode = 'S'),.T.)
DEFINE BAR lnNextBar OF _INQURYPOP PROMPT LANG_GMA_ShowLinesbyPack  SKIP FOR gfFormIsActive(&lcHostFormName) .AND. IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage <> 3) OR;
                                       (_screen.ActiveForm.Parent.llNtPacks) OR;
                                       (_screen.ActiveForm.Parent.ActiveMode = 'S'),.T.)
*HBG [End]
*HBG [End]

*!**************************************************************************
*! Name      : lfVldBarS
*! Developer : Hend Ghanem
*! Date      : 08/19/2004
*! Purpose   : Validate custom bars for Sales order Invoice
*! Reference : 
*!**************************************************************************
*! Example   :  =lfVldBarS()
*!**************************************************************************
*
FUNCTION lfVldBarS

DO CASE 
  CASE BAR() = loFormSet.NextOptionBar  && Show invoice lines per pack
    loFormSet.llShowPack = !loFormSet.llShowPack
    IF loFormSet.llShowPack
      SET MARK OF BAR loFormSet.NextOptionBar OF _INQURYPOP TO .T.
      IF TYPE('loFormSet.AriaForm1.Ariapageframe1.page3.Ariagrid1.PackVersion') <> 'O'
        WITH loFormSet.AriaForm1.Ariapageframe1.page3.Ariagrid1
          .ADDCOLUMN(3)
          .Columns(.ColumnCount).Name = 'PackVersion'
          .Columns(.ColumnCount).ControlSource = "PACK_ID+'-'+CPKCOLOR+'-'+lfGetGmSz(cPckSize)+'-'+CPKVERSION"
          .Columns(.ColumnCount).WIDTH = 200
          .Columns(.ColumnCount).Header1.Caption = LANG_GMA_PackTitle
        ENDWITH
      ENDIF  
    ELSE
      loFormSet.AriaForm1.Ariapageframe1.page3.Ariagrid1.RemoveObject('PackVersion')
      SET MARK OF BAR loFormSet.NextOptionBar  OF _INQURYPOP TO .F.
    ENDIF
ENDCASE  


*!*************************************************************
*! Name      : lfBRWFSINV
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Due to C037922,1
*!**************************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfBRWPFLD()
*!*************************************************************
*!
FUNCTION lfBRWFSINV
*C128356,1 HBG 08/23/2005 Add L\C fields to invoice lines grid[begin]
WITH loFormSet.AriaForm1.Ariapageframe1.page3.Ariagrid1        
  .columnCount = 14
  .Columns(11).ControlSource = "cloc_no"
  .Columns(11).WIDTH = 170
  .Columns(11).Header1.Caption = LANG_GMA_LabelLCNo
  .Columns(12).ControlSource = "clocbank"
  .Columns(12).WIDTH = 170
  .Columns(12).Header1.Caption = LANG_GMA_LabelLCBank
  .Columns(13).ControlSource = "dlocissue"
  .Columns(13).WIDTH = 100
  .Columns(13).Header1.Caption = LANG_GMA_LabelLCDate
  .Columns(14).ControlSource = "nasscost"
  .Columns(14).WIDTH = 100
  .Columns(14).Header1.Caption = LANG_GMA_LabelAssist
ENDWITH        
*C128356,1 [End]
IF loFormSet.llShowPack
  lcCurrAlias = ALIAS()
  SELECT (loFormSet.lcInvLine)
  .Ariagrid1.PackVersion.ControlSource = "PACK_ID+'-'+CPKCOLOR+'-'+lfGetGmSz(cPckSize)+'-'+CPKVERSION"
  SELECT (lcCurrAlias)
ELSE
  IF TYPE('loFormSet.AriaForm1.Ariapageframe1.page3.Ariagrid1.PackVersion') = 'O'
    loFormSet.AriaForm1.Ariapageframe1.page3.Ariagrid1.RemoveObject('PackVersion')
  ENDIF  
ENDIF

*!**************************************************************************
*! Name      : lfCSMDIBAR
*! Developer : Wael Ali Mohamed
*! Date      : 04/10/2004
*! Purpose   : Runn custom bars for Direct Invoice
*! Reference : 
*!**************************************************************************
*! Example   :  =lfCSMDIBAR()
*!**************************************************************************
FUNCTION lfCSMDIBAR


DO CASE 
  CASE BAR() = loFormSet.NextOptionBar  && Show invoice lines per pack
    loFormSet.llShowPack = !loFormSet.llShowPack
    IF loFormSet.llShowPack
      SET MARK OF BAR loFormSet.NextOptionBar OF _INQURYPOP TO .T.
      WITH loFormSet.AriaForm1.Ariapageframe1.page2.Ariagrid1
        SELECT (.RecordSource) 
        .ADDCOLUMN(3)
        .Columns(.ColumnCount).Name = 'PackVersion'
        .Columns(.ColumnCount).ControlSource = "PACK_ID+'-'+CPKCOLOR+'-'+lfGetGmSz(cPckSize)+'-'+CPKVERSION"
        .Columns(.ColumnCount).WIDTH = 200
        .Columns(.ColumnCount).Header1.Caption = LANG_GMA_PackTitle
      ENDWITH
    ELSE
      loFormSet.llSummaryByPack = .F.
      SET MARK OF BAR loFormSet.NextOptionBar  OF _INQURYPOP TO .F.
      SET MARK OF BAR loFormSet.NextOptionBar+1 OF _INQURYPOP TO .F.
      loFormSet.mInvoiceLines
    ENDIF
  CASE BAR() = loFormSet.NextOptionBar+1   && Sort by Pack
    loFormSet.llSummaryByPack = !loFormSet.llSummaryByPack
    IF loFormSet.llSummaryByPack
      SET MARK OF BAR loFormSet.NextOptionBar+1 OF _INQURYPOP TO .T.
      lcInvoice = EVALUATE(IIF(loFormSet.Activemode = 'A',loFormSet.lcInvHdr,'InvHdr')+'.Invoice')
      IF !USED(loFormSet.lcPackSum) .OR. !SEEK(lcInvoice,loFormSet.lcPackSum)
        lcFile = IIF(loFormSet.Activemode='A',loFormSet.lcInvLine,'InvLine')
        SELECT Invoice,&lcFile..PACK_ID,&lcFile..CPKCOLOR,&lcFile..cPckSize,&lcFile..CPKVERSION,&lcFile..nPkSlPrice,Spck_hdr.Desc,nPackNo;
               FROM (lcFile),Spck_hdr ;
               WHERE Spck_hdr.pack_id+Spck_hdr.cpkcolor+Spck_hdr.cpcksize+Spck_hdr.cpkversion = ;
               &lcFile..pack_id+&lcFile..cpkcolor+&lcFile..cpcksize+&lcFile..cpkversion AND &lcFile..Invoice = INVHDR.Invoice;
               GROUP BY Invoice,&lcFile..PACK_ID,&lcFile..CPKCOLOR,&lcFile..cPckSize,&lcFile..CPKVERSION,Spck_hdr.Desc,&lcFile..nPkSlPrice,nPackNo ;
               INTO CURSOR (loFormSet.lcPackSum)
        INDEX ON Invoice+PACK_ID+CPKCOLOR+cPckSize+CPKVERSION TAG (loFormSet.lcPackSum)
      ENDIF
      lcOrderFile = loFormSet.lcPackSum
      WITH loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1
        .ColumnCount=0
        .RecordSource = lcOrderFile
         SELECT (lcOrderFile)
        .columncount = 5
        .Columns(1).ControlSource = lcOrderFile+".PACK_ID+'-'+"+lcOrderFile+".CPKCOLOR+'-'+lfGetGmSz("+lcOrderFile+".cPckSize)+'-'+"+lcOrderFile+'.CPKVERSION'
        .Columns(1).WIDTH = 200
        .Columns(1).Header1.Caption = LANG_GMA_PackTitle
        .Columns(2).ControlSource = 'Desc'
        .Columns(2).WIDTH = 120
        .Columns(2).Header1.Caption = lang_LabelStyleDesc
        .Columns(3).ControlSource = lcOrderFile+'.nPackNo'
        .Columns(3).WIDTH = 60
        .Columns(3).Header1.Caption = LANG_GMA_PackQty
        .Columns(4).ControlSource = lcOrderFile+'.nPkSlPrice'
        .Columns(4).WIDTH = 70
        .Columns(4).Header1.Caption = LANG_GMA_PackPrice
        .Columns(5).ControlSource = lcOrderFile+'.nPkSlPrice*'+lcOrderFile+'.nPackNo'
        .Columns(5).WIDTH = 60
        .Columns(5).Header1.Caption = lang_LabelAmount
        .SetAll('ReadOnly',.T.,'COLUMN')
        .Refresh
      ENDWITH
      loFormSet.llZoom = .T.
      loFormSet.mZoomBrowse(loFormSet.llZoom)
      SET MARK OF BAR 11 OF _INQURYPOP TO .T.
    ELSE
      SET MARK OF BAR loFormSet.NextOptionBar+1 OF _INQURYPOP TO .F.
      loFormSet.mInvoiceLines
      WITH loFormSet.AriaForm1.Ariapageframe1.page2.Ariagrid1
      SELECT (.RecordSource) 
      .ADDCOLUMN(3)
      .Columns(.ColumnCount).Name = 'PackVersion'
      .Columns(.ColumnCount).ControlSource = "PACK_ID+'-'+CPKCOLOR+'-'+lfGetGmSz(cPckSize)+'-'+CPKVERSION"
      .Columns(.ColumnCount).WIDTH = 200
      .Columns(.ColumnCount).Header1.Caption = LANG_GMA_PackTitle
      ENDWITH
    ENDIF  
ENDCASE

*!*************************************************************
*! Name      : lfREMPACK
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfREMPACK()
*!*************************************************************
*!
FUNCTION lfREMPACK
IF EMPTY(Pack_ID) OR EMPTY(cPkColor) OR EMPTY(cPkVersion)
  RETURN .F.
ENDIF
IF !EMPTY(cPckSize) AND SEEK('S'+LEFT(cPckSize,1),'SCALE')
  lcSize = EVALUATE('SCALE.Sz'+RIGHT(cPckSize,1))
ELSE
  lcSize = '*****'    
ENDIF  
IF gfModalGen('QRM00000B40000','ALERT',.F.,.F.,;
                LANG_GMA_RemovePack1+ALLTRIM(Pack_ID)+'-'+ALLTRIM(cPkColor)+;
                '-'+ALLTRIM(lcSize)+'-'+ALLTRIM(cPkVersion)+LANG_GMA_RemovePack2) = 2
  RETURN
ENDIF
WITH oInvoiceEditRegion
  lcInvHdr = .HeaderFile
  SELECT (.DetailFile)
  lcPackKey   = Pack_ID+cPkColor+cPckSize+cPkVersion
  SCAN FOR Pack_ID+cPkColor+cPckSize+cPkVersion = lcPackKey

    *--decrese the quantity per carton with the style quantity
    .TotalCartons = .TotalCartons  - IIF(Style.Qty_Ctn>0,TotQty/Style.Qty_Ctn,0)
    m.Cartons = CEILING(.TotalCartons)
    m.Cartons = MAX(m.Cartons,1)

    *-- decrease the total wieght with style wieght
    m.Weight = &lcInvHdr..Weight - TotQty*Style.nStyWeight
    *-- decrease the shipped amount 
    m.ShipAmt = &lcInvHdr..ShipAmt - TotQty*Price
    *-- decrease shipped quantyties
    m.Ship = &lcInvHdr..Ship - TotQty
    *-- if this style was taxable then decrease the tax amount
    .TaxDueAmount = IIF(lTaxable,.TaxDueAmount-TotQty*Price,.TaxDueAmount)
    SELECT (lcInvHdr)
    REPLACE CARTONS  WITH m.Cartons ,;
            WEIGHT   WITH m.Weight  ,;
            SHIPAMT  WITH m.ShipAmt ,;
            Discount WITH -ROUND(m.ShipAmt*DiscPcnt/100,2) ,;
            SHIP     WITH m.Ship  ,;
            APPRAMT  WITH m.ShipAmt ,;
            nTaxDue  WITH .TaxDueAmount ,;
            lCompUps WITH .T.    
    SELECT (.DetailFile)
    IF .UseTaxes .AND. loFormSet.llIsEngland .AND. !.VatExempted .AND. Style.nTaxBreak <> 0
      lnQuantity = 0
      FOR lnCount = Style.nTaxBreak TO 8
      *-- Sum the quantity for only the sizes that is Greater than the tax break weight
        lnQuantity = lnQuantity+EVAL('Qty'+STR(lnCount,1))
      ENDFOR
      *-- Decrease Merchandise tax
      .MerchandiseTax = .MerchandiseTax - lnQuantity * Price * nTaxRate/100
    ENDIF
    DELETE
  ENDSCAN
  GO TOP
ENDWITH

*!*************************************************************
*! Name      : lfUPDDINV
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfUPDDINV()
*!*************************************************************
*!
FUNCTION lfUPDDINV

IF loFormSet.Activemode = 'E' AND InvHdr.Consol <> "Y"
  =gfOpenFile(oAriaApplication.DataDir+'DEBIT',oAriaApplication.DataDir+'DEBIT','SH')
  SELECT DEBIT
  IF SEEK(InvHdr.Account+InvHdr.Invoice+SPACE(3)+DTOS(InvHdr.InvDate))
    =RLOCK()
    REPLACE STORE WITH InvHdr.Store
    UNLOCK
  ELSE
    =gfOpenFile(oAriaApplication.DataDir+'ARHIST',oAriaApplication.DataDir+'ARHISTT','SH')
    SELECT ARHIST
    IF SEEK(InvHdr.Account+InvHdr.Invoice+SPACE(3))
      =RLOCK()
      REPLACE STORE WITH InvHdr.Store
      UNLOCK
    ENDIF  
  ENDIF
  SELECT InvLine
  IF SEEK (InvHdr.Invoice)
    REPLACE REST STORE WITH InvHdr.Store WHILE Invoice=InvHdr.Invoice
  ENDIF
  =gfOpenFile(oAriaApplication.DataDir+'REPCOMM','REPCOMM','SH')
  SELECT REPCOMM 
  IF SEEK(InvHdr.REP1+DTOS(InvHdr.INVDATE)+InvHdr.Invoice+'1')
    REPLACE STORE WITH InvHdr.Store
  ENDIF
  IF SEEK(InvHdr.REP2+DTOS(InvHdr.INVDATE)+InvHdr.Invoice+'1')
    REPLACE STORE WITH InvHdr.Store
  ENDIF        
  =gfOpenFile(oAriaApplication.DataDir+'UPSBOX','UPSBOX','SH')
  SELECT UPSBOX
  IF SEEK(InvHdr.Invoice)
    REPLACE REST STORE WITH InvHdr.Store WHILE Invoice=InvHdr.Invoice
  ENDIF
ENDIF
=lfUpdUPS()

*!**************************************************************************
*! Name      : lfUpdUPS
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdUPS()
*!**************************************************************************
*! Due to C200352,1
*!**************************************************************************
FUNCTION lfUpdUPS
LOCAL lnCount

=gfOpenFile(oAriaApplication.DataDir+'SHDAEXPR','SHDAEXPR','SH')
=gfOpenFile(oAriaApplication.DataDir+'CONNECT','CUSTOMER','SH')
=gfOpenFile(oAriaApplication.DataDir+'CODES','CODES','SH')

llUpdUPS = .F.
STORE "" TO lcUPSType , lcUPSBill , lcShpName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5, lcShpCunt ,;
            lcShpTel  , lcShpFax  , lcCstEmail

*-- Get Ship Via Information
IF SEEK('N'+InvHdr.ShipVia+'Y'+'SHIPVIA','CODES')
  SELECT CODES
  lnCount = 0
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+InvHdr.ShipVia+'Y'+'SHIPVIA'
    lnCount = lnCount + 1
    DIMENSION laShpRlt[lnCount,2]
    laShpRlt[lnCount,1] = CODES.crltd_nam
    laShpRlt[lnCount,2] = ALLTRIM(CODES.cRltd_vlu)
  ENDSCAN
  lnFound = ASCAN(laShpRlt,'CUPS')
  IF lnFound > 0
    lnFound  = ASUBSCRIPT(laShpRlt,lnFound,1)
    llUpdUPS = SUBSTR(laShpRlt[lnFound,2],1,5) = 'USUPS'
    IF llUpdUPS
      lcUPSType = laShpRlt[lnFound,2]
    ENDIF
  ENDIF
  IF llUpdUPS
    lnFound = ASCAN(laShpRlt,'CUPSBILL')
    IF lnFound > 0
      lnFound   = ASUBSCRIPT(laShpRlt,lnFound,1)
      lcUPSBill = laShpRlt[lnFound,2]
    ENDIF
  ENDIF
ENDIF
*-- Get Customer Information  
IF llUpdUPS
  IF SEEK('O'+INVHDR.Order,'ORDHDR') AND ORDHDR.Alt_ShpTo 
    lcCustPO   = ORDHDR.CustPO
    lcShpName  = ORDHDR.STName
    lcShipAdd1 = ORDHDR.caddress1
    lcShipAdd2 = ORDHDR.caddress2
    lcShipAdd3 = ORDHDR.caddress3
    lcShipAdd4 = ORDHDR.caddress4
    lcShipAdd5 = ORDHDR.caddress5
    =SEEK('M'+INVHDR.Account,'CUSTOMER')
    lcShpCunt  = CUSTOMER.cCont_Code
    lcShpTel   = CUSTOMER.Phone1       
    lcShpFax   = CUSTOMER.Fax  
    lcCstEmail = CUSTOMER.cEmail_Add      
  ELSE
    lcCustPO   = INVHDR.CustPO
    IF SEEK(IIF(EMPTY(INVHDR.Store),'M'+INVHDR.Account,'S'+INVHDR.Account+INVHDR.Store),'CUSTOMER')
      lcDistCntr = CUSTOMER.Dist_Ctr
      =SEEK('S' + INVHDR.Account + IIF(EMPTY(lcDistCntr),INVHDR.Store,lcDistCntr),'CUSTOMER')            
      IF SEEK(INVHDR.Account+IIF(EMPTY(lcDistCntr),INVHDR.Store,lcDistCntr),'CONNECT')
        lcShpName   = CONNECT.StName
        lcShipAdd1  = CONNECT.caddress1
        lcShipAdd2  = CONNECT.caddress2
        lcShipAdd3  = CONNECT.caddress3
        lcShipAdd4  = CONNECT.caddress4
        lcShipAdd5  = CONNECT.caddress5
        lcShpCunt   = CUSTOMER.cCont_Code
        lcShpTel    = CUSTOMER.Phone1       
        lcShpFax    = CUSTOMER.Fax  
        lcCstEmail  = CUSTOMER.cEmail_Add
      ELSE
        DECLARE laAddress[6,3]
        lcShpName  =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
        =gfGetAdr('CUSTOMER','','','',1,'')
        FOR lnCount = 1 TO ALEN(laAddress,1)
          lcCount = STR(lnCount,1)
          lcShipAdd&lcCount = laAddress[lnCount,2]
        ENDFOR
        lcShpCunt  = CUSTOMER.cCont_Code
        lcShpTel   = CUSTOMER.Phone1  
        lcShpFax   = CUSTOMER.Fax  
        lcCstEmail = CUSTOMER.cEmail_Add
      ENDIF 
    ENDIF
  ENDIF  
  *-- Update OnLine WorldShip Shipment Data Export file 'SHDAEXPR'
  IF !SEEK(INVHDR.Invoice,'SHDAEXPR')
    INSERT INTO SHDAEXPR (cSiisVoid,cpkpkgrf1,cpkpkgrf2,cpkpkgrf3,cpkpkgrf4,cpkpkgrf5,cpksnt1op,cpksnt1ty,cstdepart) VALUES;
    ('N',lcCustPO,INVHDR.Invoice,InvHdr.Dept,ALLTRIM(InvHdr.Invoice+' / '+InvHdr.Dept),'N/A','N','Email','')
  ENDIF
  REPLACE cSiservtyp WITH lcUPSType,;
          cSibilopt  WITH lcUPSBill,;
          cStcustid  WITH ALLTRIM(INVHDR.Account+INVHDR.STORE),;
          cStcmpnam  WITH lcShpName,;
          cStstrad   WITH lcShipAdd1,;
          cStromflr  WITH lcShipAdd2,;
          cStcontry  WITH lcShpCunt,;
          cStcty     WITH lcShipAdd3,;
          cStStat    WITH lcShipAdd4,;
          cstZipcod  WITH lcShipAdd5,;
          cStteleph  WITH lcShpTel,;
          cStFax     WITH lcShpFax,;
          npkgweght  WITH ALLTRIM(STR(CEILING(INVHDR.Weight/INVHDR.Cartons))),;
          cpksntfe   WITH lcCstEmail IN SHDAEXPR
ELSE
  IF SEEK(INVHDR.Invoice,'SHDAEXPR')
    DELETE IN SHDAEXPR
  ENDIF
ENDIF               

*!**************************************************************************
*! Name      : lfEDTDINV
*! Developer : Wael Ali Mohamed
*! Date      : 04/15/2002
*! Purpose   : Edit Invoice
*!**************************************************************************
*! Example   : = lfEDTDINV()
*!**************************************************************************
*! Due to 
*!**************************************************************************
FUNCTION lfEDTDINV
IF loFormSet.Activemode = 'E'
  IF InvHdr.Consol <> 'Y'
    WITH loFormSet.AriaForm1.KeyStore
      STORE .T. TO .KeyTextBox.Enabled, .KeyCmd.Enabled
    ENDWITH  
  ENDIF  
  WITH loFormSet.AriaForm1.AriaPageFrame1
    STORE .T. TO .Page1.cboShipVia.Enabled, .Page3.cntInvoiceSummary.txtWeight.Enabled,;
                 .Page3.cntInvoiceSummary.txtCartons.Enabled      
    STORE .F. TO .Page1.KeyDepartment.KeyTextBox.Enabled, .Page1.KeyDepartment.KeyTextBox.Enabled
  ENDWITH
ENDIF

*!*************************************************************
*! Name      : lfSTOREADD
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfSTOREADD()
*!*************************************************************
*!
FUNCTION lfSTOREADD
loFormSet.mShippingAddress

*!**************************************************************************
*! Name      : lfDELDINV
*! Developer : Wael Ali Mohamed
*! Date      : 04/15/2002
*! Purpose   : Void Invoice
*!**************************************************************************
*! Example   : = lfDELDINV()
*!**************************************************************************
*! Due to 
*!**************************************************************************
FUNCTION lfDELDINV
lcCurAlis = SELECT(0)
=gfOpenFile(oAriaApplication.DataDir+'SHDAEXPR','SHDAEXPR','SH')
IF SEEK(InvHdr.Invoice,'SHDAEXPR')
  REPLACE cSiisVoid WITH 'Y' IN SHDAEXPR 
  DELETE IN SHDAEXPR 
ENDIF
SELECT (lcCurAlis)

*!*************************************************************
*! Name      : lfAddHFld
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfAddFld()
*!*************************************************************
*C127844
FUNCTION lfAddHFld

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,18]
laFileStru[lnFileStru+1,1] = 'TotAssist'
laFileStru[lnFileStru+1,2] = 'N'
*khm
*laFileStru[lnFileStru+1,3] = 5
laFileStru[lnFileStru+1,3] = 11
*khm
laFileStru[lnFileStru+1,4] = 3
FOR lnCount = 1 TO 1
  STORE '' TO laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
              laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
              laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
              laFileStru[lnFileStru+lnCount,16]
  STORE 0  TO laFileStru[lnFileStru+lnCount,17],  laFileStru[lnFileStru+lnCount,18]
ENDFOR

*!*************************************************************
*! Name      : lfAddFld
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfAddFld()
*!*************************************************************
*!
FUNCTION lfAddFld

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'lFrmpack'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 0
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lShip'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 0
laFileStru[lnFileStru+2,4] = 0
FOR lnCount = 1 TO 2
  STORE '' TO laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
              laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
              laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
              laFileStru[lnFileStru+lnCount,16]
  STORE 0  TO laFileStru[lnFileStru+lnCount,17],  laFileStru[lnFileStru+lnCount,18]
ENDFOR

*!*************************************************************
*! Name      : lfAddIIndx
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfAddIIndx()
*!*************************************************************
*!
FUNCTION lfAddIIndx

lnIndex = ALEN(laIndex,1)
DIMENSION laIndex[lnIndex+1,2]
laIndex[lnIndex+1,1] = 'Account+Order+Store+PikTkt+Pack_id+cPkColor+cPckSize+cPkVersion'
laIndex[lnIndex+1,2] = 'Packs'
IF ASCAN(laIndex,'ConsDist') > 0
  DIMENSION laIndex[lnIndex+1,2]
  laIndex[lnIndex+1,1] = 'Consol+Account+cDivision+cCurrCode+Style+Dyelot+Pack_id+cPkColor+cPckSize+cPkVersion+Dist_Ctr'
  laIndex[lnIndex+1,2] = 'PkConsol'
ENDIF

*:**************************************************************************
*:* Name        : lfUPDTORDD
*:* Developer   : Hend Ghanem (HBG)
*:* Date        : 03/05/2003
*:* Purpose     : Update order generated from direct invoice
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUPDTORDD()
*:***************************************************************************
FUNCTION lfUPDTORDD
REPLACE cPkColor   WITH m.cPkColor   ,;
        cPckSize   WITH m.cPckSize   ,;
        cPkVersion WITH m.cPkVersion ,;
        nPkSlPrice WITH m.nPkSlPrice ,;
        nPackNo    WITH m.nPackNo    ,;
        lRange     WITH m.lRange     ,;
        lPckPrPiec WITH m.lPckPrPiec IN ORDLINE

*:**************************************************************************
*:* Name        : lfUpOrdSh
*:* Developer   : Hend Ghanem (HBG)
*:* Date        : 02/13/2006
*:* Purpose     : Updating nPAckNo in ORDLINE with the actually shipped qty[Begin]
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpdOrdSh()
*:***************************************************************************
FUNCTION lfUpOrdSh
REPLACE nPackNo    WITH ORDLINE.nPackNo + &lcInvLines..nPackNO IN ORDLINE


*!**************************************************************************
*! Name      : lfUpdtOrdS
*! Developer : Hend Ghanem (HBG)
*! Date      : 08/19/2004
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdtOrdS()
*!**************************************************************************
*! Due to C038193
*!**************************************************************************
FUNCTION lfUpdtOrdS

lcCurrAlias = ALIAS()
lcInv = ""
FOR lnInv = 1 TO ALEN(laInv)
  lcInv = lcInv + laInv[lnInv]+'|'
ENDFOR
*B130472,1 02/13/2006 Fix bug of not updating nPAckNo in ORDLINE with the actually shipped qty[Begin]
*wael
*lcInvLin = IIF(CUSTOMER.Consol = 'Y' , 'CONSINVL','INVLINE')
lcInvLin = IIF(INVHDR.Consol = 'Y' , 'CONSINVL','INVLINE')
*wael
*B130472,1 [End]

*B608827,1 WAM 03/26/2009 Don't overwrite pack information in ORDLINE
*B608827,1 WAM Following lines are commented out
*!*	SELECT ORDLINE
*!*	SET ORDER TO ORDLINE
*!*	SELECT INVLINE
*!*	SET RELATION TO 'O'+InvLine.Order+STR(LineNo,6) INTO ORDLINE ADDI
*!*	SELECT INVHDR
*!*	SET ORDER TO INVHDR
*!*	FOR lnInv = 1 TO ALEN(laInv)
*!*	  IF SEEK(laInv[lnInv])
*!*	    SCAN REST WHILE Invoice =laInv[lnInv]
*!*	      lcInvoice = INVHDR.Invoice
*!*	      IF SEEK(lcInvoice,'INVLINE')
*!*	        =lfUpdOrdl()
*!*	      ENDIF  
*!*	    ENDSCAN  
*!*	  ENDIF  
*!*	ENDFOR
*!*	SELECT INVHDR
*!*	=SEEK(laInv[1])
*!*	SELECT INVLINE
*!*	SET RELATION TO
*B608827,1 WAM 03/26/2009 (End)

=lfUPDUPSS()
SELECT (lcCurrAlias)

*!**************************************************************************
*! Name      : lfUpdOrdL
*! Developer : Hend Ghanem (HBG)
*! Date      : 08/19/2004
*! Purpose   : Updatefields of Pack in ORDLINE file
*!**************************************************************************
*! Example   : = lfUpdOrdL()
*!**************************************************************************
*! Due to 
*!**************************************************************************
FUNCTION lfUpdOrdL
                            
SELECT INVLINE
SCAN REST WHILE invoice+STR(lineno,6) = lcInvoice
  REPLACE ORDLINE.cPkColor   WITH INVLINE.cPkColor  ,;
          ORDLINE.cPckSize   WITH INVLINE.cPckSize  ,;
          ORDLINE.cPkVersion WITH INVLINE.cPkVersion,;
          ORDLINE.nPkSlPrice WITH INVLINE.nPkSlPrice,;
          ORDLINE.nPackNo    WITH INVLINE.nPackNo,;
          ORDLINE.lRange     WITH INVLINE.lRange,;              
          ORDLINE.lPckPrPiec WITH INVLINE.lPckPrPiec 
ENDSCAN


*!**************************************************************************
*! Name      : lfUpdUPSS
*! Developer : Hend Ghanem (HBG)
*! Date      : 08/19/2004
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdUPSS()
*!**************************************************************************
*! Due to C038193
*!**************************************************************************
FUNCTION lfUpdUPSS

=gfOpenFile(oAriaApplication.DataDir+'SHDAEXPR','SHDAEXPR','SH')
=gfOpenFile(oAriaApplication.DataDir+'CONNECT','CUSTOMER','SH')
=gfOpenFile(oAriaApplication.DataDir+'CODES','CODES','SH')
=gfOpenFile(oAriaApplication.DataDir+'ORDHDR','ORDHDR','SH')
=gfOpenFile(oAriaApplication.DataDir+'CUSTOMER','CUSTOMER','SH')
=gfOpenFile(oAriaApplication.DataDir+'CUSTDEPT','CUSTDEPT','SH')
=gfOpenFile(oAriaApplication.DataDir+'CONSINVH','CONSINVH','SH')

lcInv = ""
FOR lnInv = 1 TO ALEN(laInv)
  lcInv = lcInv + laInv[lnInv]+'|'
ENDFOR
SELECT INVHDR
*!* B610301,1 HIA 04/15/2013 Upgrade GMA project - Issue # 6 [Start]
SET ORDER TO INVHDR
*!* B610301,1 HIA 04/15/2013 Upgrade GMA project - Issue # 6 [Start]

=SEEK(laInv[1])

FOR lnInv = 1 TO ALEN(laInv)
  IF SEEK(laInv[lnInv])
    SCAN REST WHILE Invoice =laInv[lnInv]
      lcShipVia = INVHDR.ShipVia
      =lfUpdUPS()
    ENDSCAN  
  ENDIF  
ENDFOR

SELECT INVHDR
=SEEK(laInv[1])


*:**************************************************************************
*:* Name        : lfDisabMsg
*:* Developer   : Hend Ghanem (HBG)
*:* Date        : 03/05/2003
*:* Purpose     : Disable message "Invalid Shipper Or Ship To Zone"
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSHOWIGST()
*:***************************************************************************
FUNCTION lfDisabMsg

RETURN .T.

*:*************************************************************
*:* Name        : lfDisabMsg
*:* Developer   : Hend Ghanem (HBG)
*:* Date        : 03/05/2003
*:* Purpose      : 
*:*************************************************************
*:* Calls     : None
*:*************************************************************
*:* Parameters: None
*:*************************************************************
*:* Returns   : None 
*:**************************************************************
*:* Example   : =lfGRSPRCE()
*:**************************************************************
FUNCTION lfGRSPRCE

*HBG 01/27/2005 IF the price in pack is 0 get the style price [Begin]
*m.Gros_Price = Spck_Lin.npck_price
m.Gros_Price  = IIF(Spck_Lin.npck_price = 0 ,gfGetprice(Spck_Lin.Style,lcPriceLvl,TotQty*&lcPackHdr..PackQty,&lcHdrFile..cCurrCode),Spck_Lin.npck_price)
IF m.Gros_Price = 0 
  IF lcPriceLvl = 'Q'
    DO CASE
      CASE Style.nAtQtyC > 0 AND lnTotQty > Style.nAtQtyC
        lcLevel = 'C'
    CASE Style.nAtQtyB > 0 AND lnTotQty > Style.nAtQtyB
        lcLevel = 'B'
      OTHERWISE
        lcLevel = 'A'
    ENDCASE
  ELSE
    lcLevel=IIF(INLIST(lcPriceLvl,'A','B','C'),lcPriceLvl,'A')
  ENDIF
 
  *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
  *m.Gros_Price = lfCheckPri(Spck_Lin.Style,lcLevel,OrdHDr.cCurrCode)
  m.Gros_Price = lfCheckPri(Spck_Lin.Style,lcLevel,&lcHdrFile..cCurrCode)
  *B608434,1 WAM 02/14/2008 (End)

ENDIF  
*HBG [End]

*!**************************************************************************
*! Name        : lfUpdtPrcD
*! Developer   : Hend Ghanem (HBG)
*! Date        : 05/12/2004
*! Purpose     : 
*!**************************************************************************
*! Example   : = lfUpdtPrcD()
*!**************************************************************************
FUNCTION lfUpdtPrcD

=gfOpenFile(oAriaApplication.DataDir+'SPck_Hdr',oAriaApplication.DataDir+'SPCK_HDRVR','SH') 
=lfUpdPric()
SET ORDER TO TAG (loFormSet.lcInvLine) IN (loFormSet.lcInvLine)
SELECT INVHDR

*!**************************************************************************
*! Name      : lfUpdtPrcS
*! Developer : Hend Ghanem (HBG)
*! Date      : 08/19/2004
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdtPrcS()
*!**************************************************************************
*! Due to C0381931
*!**************************************************************************
FUNCTION lfUpdtPrcS

=gfOpenFile(oAriaApplication.DataDir+'SPck_Hdr',oAriaApplication.DataDir+'SPCK_HDRVR','SH') 
SELECT (loFormSet.lcInvHdr)
SCAN FOR Consol='N'
  =lfUpdPric()
ENDSCAN  
LOCATE
SET ORDER TO TAG (loFormSet.lcInvLine) IN (loFormSet.lcInvLine)

*!**************************************************************************
*! Name      : lfUpdPric
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/12/2004
*! Purpose   : Update Pack price in INVLINE file
*!**************************************************************************
*! Example   : = lfUpdPric()
*!**************************************************************************
*! Due to 
*!**************************************************************************
FUNCTION lfUpdPric
                            
SELECT (loFormSet.lcInvLine)
SCAN FOR Account+Order+Store+PikTkt+STR(LineNo,6) = ;
         EVALUATE(loFormSet.lcInvHdr+'.Account')+EVALUATE(loFormSet.lcInvHdr+'.Order')+;
         EVALUATE(loFormSet.lcInvHdr+'.Store')+EVALUATE(loFormSet.lcInvHdr+'.Piktkt')
  llGetPack = SEEK('P'+Account+Pack_Id+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')  
  IF !llGetPack 
    llGetPack = SEEK('P*****'+Pack_Id+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')  
  ENDIF 
  IF llGetPack 
    REPLACE nPkSlPrice WITH SPCK_HDR.nPkSlPrice
  ENDIF  
ENDSCAN

*!**************************************************************************
*! Name      : lfChkPack
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/12/2004
*! Purpose   : Check if the pack exist change only the qty of it
*!**************************************************************************
*! Example   : = lfUpdPric()
*!**************************************************************************
*! Due to 037922
*!**************************************************************************
FUNCTION lfChkPack

lcAlias = ALIAS()
lcOldOrder = ORDER(loFormSet.lcOrdLines)
SET ORDER TO AccPacks IN (loFormSet.lcOrdLines)

lcStore = PADR(loFormSet.AriaForm1.KeyStores.Keytextbox.Value,8)
IF SEEK(&lcPackHdr..Account+lcStore+&lcPackHdr..Pack_id+&lcPackHdr..cPkColor+;
        &lcPackHdr..cPckSize+&lcPackHdr..cPkVersion,loFormSet.lcOrdLines)
  lcPackID = &lcPackHdr..Account+&lcPackHdr..Pack_id+&lcPackHdr..cPkColor+;
             &lcPackHdr..cPckSize+&lcPackHdr..cPkVersion
  lnChoose =gfModalGen("INM00000B32000","Dialog",.F.,.F.,LANG_GMA_Pack+lcPackID+LANG_GMA_EnteredBefore+;
                               IIF(EMPTY(lcStore),"",LANG_GMA_ForSameStore+lcStore)+LANG_GMA_AddAnyway)       
  IF lnChoose = 1
    lnPackQty = EVALUATE(loFormSet.lcOrdLines+'.nPackNo') + &lcPackHdr..PackQty
    =lfUpdpck()
  ENDIF
  SET ORDER TO (lcOldOrder) IN (loFormSet.lcOrdLines)
  SELECT (lcAlias)
  RETURN .T.      
ELSE
  SET ORDER TO (lcOldOrder) IN (loFormSet.lcOrdLines)
  SELECT (lcAlias)
  RETURN .F.        
ENDIF        


*!**************************************************************************
*! Name      : lfUpdpck
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/12/2004
*! Purpose   : Udate the new pack Qty
*!**************************************************************************
*! Example   : = lfUpdpck()
*!**************************************************************************
*! Due to 037922
*!**************************************************************************
FUNCTION lfUpdpck

SELECT (loFormSet.lcOrdLines)
SCAN REST WHILE cPckAcc+Store+Pack_id+cPkColor+cPckSize+cPkVersion =;
                &lcPackHdr..Account+lcStore+&lcPackHdr..Pack_id+&lcPackHdr..cPkColor+&lcPackHdr..cPckSize+;
                &lcPackHdr..cPkVersion
  =SEEK(Style,'STYLE')              
  lnWeight   = lnWeight   - TotQty*Style.nStyWeight
  lnCartons  = lnCartons  - IIF(Style.Qty_Ctn > 0,TotQty/Style.Qty_Ctn,0)
  lnQuantity = lnQuantity - TotQty
  lnAmount   = lnAmount   - TotQty*Price
  lnTaxDueAmount = lnTaxDueAmount -  IIF(STYLE.lTaxAble,TotQty*Price,0)
  IF llUseTax .AND. llIsEngland .AND. Style.nTaxBreak <> 0
    *-- Sum the quantity for only the sizes that is Greater than the tax break weight 
    lnTaxQuantity = 0
    FOR lnCount = Style.nTaxBreak TO 8
      lnTaxQuantity = lnTaxQuantity+EVAL('Qty'+STR(lnCount,1))
    ENDFOR
    lnMerchTax = lnMerchTax - lnTaxQuantity * Price * nTaxRate/100
  ENDIF  
ENDSCAN

SELECT (loFormSet.lcOrdLines)
=SEEK(&lcPackHdr..Account+lcStore+&lcPackHdr..Pack_id+&lcPackHdr..cPkColor+;
      &lcPackHdr..cPckSize+&lcPackHdr..cPkVersion,loFormSet.lcOrdLines)
SCAN REST WHILE cPckAcc+Store+Pack_id+cPkColor+cPckSize+cPkVersion =;
                &lcPackHdr..Account+lcStore+&lcPackHdr..Pack_id+&lcPackHdr..cPkColor+&lcPackHdr..cPckSize+;
                &lcPackHdr..cPkVersion
  =SEEK(Style,'STYLE')              
  =SEEK('P'+EVALUATE(loFormSet.lcOrdLines+'.cPckAcc') +EVALUATE(loFormSet.lcOrdLines+'.pack_id') +;
            EVALUATE(loFormSet.lcOrdLines+'.cpkcolor')+EVALUATE(loFormSet.lcOrdLines+'.cpcksize')+;
            EVALUATE(loFormSet.lcOrdLines+'.cpkversion')+EVALUATE(loFormSet.lcOrdLines+'.Style'),'SPCK_LIN')        
  REPLACE Qty1 WITH SPCK_LIN.Qty1 * lnPackQty,;     
          Qty2 WITH SPCK_LIN.Qty2 * lnPackQty,;     
          Qty3 WITH SPCK_LIN.Qty3 * lnPackQty,;     
          Qty4 WITH SPCK_LIN.Qty4 * lnPackQty,;     
          Qty5 WITH SPCK_LIN.Qty5 * lnPackQty,;     
          Qty6 WITH SPCK_LIN.Qty6 * lnPackQty,;     
          Qty7 WITH SPCK_LIN.Qty7 * lnPackQty,;     
          Qty8 WITH SPCK_LIN.Qty8 * lnPackQty
          
  REPLACE TotQty WITH (EVALUATE(loFormSet.lcOrdLines+'.Qty1')+EVALUATE(loFormSet.lcOrdLines+'.Qty2')+;
                       EVALUATE(loFormSet.lcOrdLines+'.Qty3')+EVALUATE(loFormSet.lcOrdLines+'.Qty4')+;
                       EVALUATE(loFormSet.lcOrdLines+'.Qty5')+EVALUATE(loFormSet.lcOrdLines+'.Qty6')+;
                       EVALUATE(loFormSet.lcOrdLines+'.Qty7')+EVALUATE(loFormSet.lcOrdLines+'.Qty8'))
                                           
  REPLACE Book1 WITH SPCK_LIN.Qty1 * lnPackQty,;     
          Book2 WITH SPCK_LIN.Qty2 * lnPackQty,;     
          Book3 WITH SPCK_LIN.Qty3 * lnPackQty,;     
          Book4 WITH SPCK_LIN.Qty4 * lnPackQty,;     
          Book5 WITH SPCK_LIN.Qty5 * lnPackQty,;     
          Book6 WITH SPCK_LIN.Qty6 * lnPackQty,;     
          Book7 WITH SPCK_LIN.Qty7 * lnPackQty,;     
          Book8 WITH SPCK_LIN.Qty8 * lnPackQty
        
  REPLACE TotBook WITH (EVALUATE(loFormSet.lcOrdLines+'.Book1')+EVALUATE(loFormSet.lcOrdLines+'.Book2')+;
                        EVALUATE(loFormSet.lcOrdLines+'.Book3')+EVALUATE(loFormSet.lcOrdLines+'.Book4')+;
                        EVALUATE(loFormSet.lcOrdLines+'.Book5')+EVALUATE(loFormSet.lcOrdLines+'.Book6')+;
                        EVALUATE(loFormSet.lcOrdLines+'.Book7')+EVALUATE(loFormSet.lcOrdLines+'.Book8'))
      
  REPLACE nPackNo WITH lnPackQty,;
          Flag    WITH 'M'
  lnWeight   = lnWeight   + TotQty*Style.nStyWeight
  lnCartons  = lnCartons  + IIF(Style.Qty_Ctn > 0,TotQty/Style.Qty_Ctn,0)
  lnQuantity = lnQuantity + TotQty
  lnAmount   = lnAmount   + TotQty*Price
  lnTaxDueAmount = lnTaxDueAmount +  IIF(STYLE.lTaxAble,TotQty*Price,0)
  IF llUseTax .AND. llIsEngland .AND. Style.nTaxBreak <> 0
    *-- Sum the quantity for only the sizes that is Greater than the tax break weight 
    lnTaxQuantity = 0
    FOR lnCount = Style.nTaxBreak TO 8
      lnTaxQuantity = lnTaxQuantity+EVAL('Qty'+STR(lnCount,1))
    ENDFOR
    lnMerchTax = lnMerchTax + lnTaxQuantity * Price * nTaxRate/100
  ENDIF  
ENDSCAN

*!**************************************************************************



*!**************************************************************************
*! Name      : lfBRWCHNG
*! Developer : Mariam Mazhar (MMT)
*! Date      : 05/26/2004
*! Purpose   : To make the icspcopy returns the whole key of the selected
*!             pack to copy from
*!**************************************************************************
*! Example   : = lfBRWCHNG()
*!**************************************************************************
*! Due to  038128
*!**************************************************************************
FUNCTION lfBRWCHNG
DECLARE laTmpDat[1]
laTmpDat[1] = ' '
=gfOpenFile(oAriaApplication.DataDir+'STYLEUPC','gmapkupc','SH')
SELECT SPCK_HDR
SET RELATION TO  Account+Pack_Id+'   '+cPkColor+cPckSize INTO STYLEUPC 
lcFields    = 'Account,pack_id,cpkColor,cpckSize,cpkversion'
=gfOpenFile(oAriaApplication.DataDir+'SCALE','SCALE','SH')
lcBrFields = [Account :H=LANG_AccountHeader : 16,Pack_Id :H=LANG_PACK_ID:16,;
              SPck_Hdr.Desc :H=LANG_DescHeader :30,;
              cDivision :R :H=LANG_DivisionHeader :12,Season   :R :H=LANG_SeasonHeader :12 ,;
              cPkVersion :H=LANG_VersionHeader :12,cPkColor :H=LANG_ColorHeader :16,]
lcBrFields = lcBrFields + [lcGmSize=lfGetGmSz(SPck_Hdr.cpcksize):H=LANG_SizeHeader  :16,]
lcBrFields = lcBrFields + [NPKSLPRICE :R :H=LANG_SELHeader :20 ,;
              STYLEUPC.CUPCNUM1:R :H=LANG_UPCHeader :13,STYLEUPC.CUPCNUM2:R :H='' :10,STYLEUPC.CUPCNUM3:R :H='' :10]

SELECT SPCK_HDR
LOCATE 
IF EOF('SPCK_HDR')
  *--There are no records to browse.
  *-- OK
  = gfModalGen("INM42097B42001","Dialog")  
  STORE SPACE(0) TO loformset.ariaform1.keytextbox.Value
 RETURN .F.
ELSE
  SELECT SPCK_HDR
  
  *B124399,1 AMH Use the correct order to help on softseek [Start]
  *SET ORDER TO SPCK_HDRVR
  *=gfBrows("'P'",lcFields,"laTmpDat",LANG_BrowseTitle,'','',.F.)  
  SET ORDER TO SPCK_HDRTP
  =SEEK('P'+PADR(loFormSet.AriaForm1.kbFromPack.keytextbox.Value,16))
  =gfBrows("'P'",lcFields,"laTmpDat",LANG_BrowseTitle,'','',.F.)
  *B124399,1 AMH [End]
  
  llValue = .F.
  IF !EMPTY(laTmpDat)
    loFormSet.AriaForm1.kbFromPack.keytextbox.Value      = laTmpDat[2]
    lcCopyFRom  =  laTmpDat[2]
    lcCopyVer   =  laTmpDat[5]
    lcCopysize  =  laTmpDat[4]
    lcCopyColor =  laTmpDat[3]
    lcAccount   =  laTmpDat[1]
    llValue = .T.
  ELSE 
    
    *B124399,1 AMH Restore the old value [Start]
    loFormSet.AriaForm1.kbFromPack.keytextbox.Value = loFormSet.AriaForm1.kbFromPack.keytextbox.OldValue
    *B124399,1 AMH [End]
    
    llValue = .F.
  ENDIF 
ENDIF 
IF USED('SCALE')
  USE IN SCALE
ENDIF

*lcKey = "'P'"+IIF(loFormSet.lcAccount='*****'," ",loFormSet.lcAccount)

*!**************************************************************************
*! Name      : lfAddObj
*! Developer : Hend Ghanem (HBG)
*! Date      : 06/03/2004
*! Purpose   : initiate some objects&variables used in SKU screen
*!**************************************************************************
*! Example   : = lfAddObj()
*!**************************************************************************
*! Due to C038119,1
*************************************************************************
FUNCTION lfAddObj
PRIVATE lnSelAlias

IF TYPE("loFormSet.ariaForm1.cntStyPck") <> 'O'
  *--Add object for display ComboBox  
  loFormSet.ariaForm1.AddObject("cntStyPck","cntStyPck")
ENDIF 
IF TYPE("loFormSet.ariaForm1.cntPackId") <> 'O'
  *--Add object for display Pack Container
  loFormSet.ariaForm1.AddObject("CntPackId","CntICSkuPackId")
ENDIF 

WITH loFormSet.ariaForm1.cntStyPck
  .Visible =.T.
  .left= 151
  .Top = 3
  .Height=40
  .Width = 85
  .Enabled = .F.
  
  *!B999999,1 WSH 02/06/2005, Save Initial Coordinates of Objects to resize correct. [Start]
  .BorderWidth = 0

  WITH loFormSet.AriaForm1
    .oResizer.SaveCoordinates(.cntStyPck)
    .oResizer.SaveCoordinates(.cntStyPck.AriaLabel1)
    .oResizer.SaveCoordinates(.cntStyPck.CboStylePack)
  ENDWITH
  *!B999999,1 WSH 02/06/2005, [End]
  
  .CboStylePack.BoundColumn =2
  .CboStylePack.ColumnCount =2
  .CboStylePack.ColumnLines =.F.
  .CboStylePack.ColumnWidths ="40,0"
  .CboStylePack.RowSource ='Style,S,Pack,P'
  .CboStylePack.RowSourceType = 1
  .CboStylePack.Value = 'S'
  .CboStylePack.OldValue = 'S'
  .CboStylePack.Enabled = .F.
ENDWITH 
 
WITH loFormSet.ariaForm1.CntPackId
  .left= 245
  .Top = 5
  .Height=39
  .Width =300
  .TabIndex =loFormSet.ariaForm1.kbStyle.TabIndex
  .Enabled = .F.

  *!B999999,1 WSH 02/06/2005, Save Initial Coordinates of Objects to resize correct. [Start]
  .BorderWidth = 0

  WITH loFormSet.AriaForm1
    .oResizer.SaveCoordinates(.cntPackId)
    .oResizer.SaveCoordinates(.cntPackId.lblPack)
    .oResizer.SaveCoordinates(.cntPackId.kbPack)
    .oResizer.SaveCoordinates(.cntPackId.kbPack.KeyTextBox)
    .oResizer.SaveCoordinates(.cntPackId.kbPack.KeyCmd)
    .oResizer.SaveCoordinates(.cntPackId.lblPackClr)
    .oResizer.SaveCoordinates(.cntPackId.txtPackClr)
    .oResizer.SaveCoordinates(.cntPackId.lblPackSize)
    .oResizer.SaveCoordinates(.cntPackId.txtPackSize)
    
    .Resize()
  ENDWITH
  *!B999999,1 WSH 02/06/2005, [End]

ENDWITH

WITH loFormSet.ariaForm1
  lnTabIndex = .kbAccountCode.TabIndex
  .cntStyPck.TabIndex      = lnTabIndex + 1
  .kbStyle.TabIndex        = lnTabIndex + 2
  .ariaconfig1.TabIndex    = lnTabIndex + 3    
  .kBSKUID.TabIndex        = lnTabIndex + 4
  .txtPackName.TabIndex    = lnTabIndex + 5
  .txTSCALEID.TabIndex     = lnTabIndex + 6
  .txtScaleDesc.TabIndex   = lnTabIndex + 7
  .cntScaleNav.TabIndex    = lnTabIndex + 8
  .cboDivision.TabIndex    = lnTabIndex + 9
  .cboSeason.TabIndex      = lnTabIndex + 10
  .chkPrePack.TabIndex     = lnTabIndex + 11
  .kbPrePack.TabIndex      = lnTabIndex + 12
  .txtPrePackDesc.TabIndex = lnTabIndex + 13
  .cntSkuqty.TabIndex      = lnTabIndex + 14
ENDWITH 

*--initialize some variables.
WITH loFormSet
  .AddProperty('lcGmSizeV',"")
  .AddProperty('lnGmaNo',1)
  .AddProperty('lcGmaSz',"")
  .AddProperty('lcPckAlias',gfTempname())
  .AddProperty('llSeekGma',.F.)
  .AddProperty('lcAccCode',"")
ENDWITH 
lcPckAlias =loFormSet.lcPckAlias
*--Open Some Files  for GMA 
=gfOpenFile(oAriaApplication.DataDir+'STYLEUPC','GMAPKUPC','SH')
=gfOpenFile(oAriaApplication.DataDir+'SPCK_HDR','SPCK_HDRST','SH',@lcPckAlias,.T.)

SELECT SPCK_HDR
SET FILTER TO
SET FILTER TO cSkuType = 'S'
lnSelAlias=SELECT()
SELECT(lcPckAlias)
SET FILTER TO (cpkversion = "1   ")
SET RELATION TO 'S'+LEFT(cPckSize,1) INTO Scale
*--Open STYLEUPC
SET RELATION TO Account+Pack_Id+'   '+cPkColor+cPckSize INTO STYLEUPC  ADDI

SELECT(lnSelAlias)

*!**************************************************************************
*! Name      : lfCONTSHW
*! Developer : Hend Ghanem (HBG)
*! Date      : 06/03/2004
*! Purpose   : Cotrol show as GMA logic
*!**************************************************************************
*! Example   : = lfAddObj()
*!**************************************************************************
*! Due to C038119,1
*************************************************************************
FUNCTION lfCONTSHW

IF TYPE("_screen.ActiveForm.CntPackId") = 'O' AND TYPE("_screen.ActiveForm.cntStyPck") = 'O'
  WITH _screen.ActiveForm
    DO CASE
      CASE .parent.ActiveMode = 'S'
        STORE .T. TO .cntStyPck.ENABLED , .cntStyPck.CboStylePack.ENABLED , .CntPackId.ENABLED , .CntPackId.KbPack.KeyTextBox.ENABLED ,;
                 .CntPackId.KbPack.keyCmd.ENABLED
        IF EMPTY(.KbAccountCode.keytextbox.VALUE)
          STORE "" TO  .txtAccountName.Value , .txtStyleDesc.Value , .txtPackName.Value 
          .cntStyPck.CboStylePack.value = .cntStyPck.CboStylePack.OldValue
        ENDIF
      CASE .parent.ActiveMode = 'V'
        STORE .F. TO .cntStyPck.ENABLED , .cntStyPck.CboStylePack.ENABLED , .CntPackId.ENABLED , .CntPackId.KbPack.KeyTextBox.ENABLED ,;
               .CntPackId.KbPack.keyCmd.ENABLED , .CntPackId.txtPackClr.ENABLED , .CntPackId.TxtPackSize.ENABLED
      CASE .parent.ActiveMode = 'E'
        STORE .F. TO .cntStyPck.ENABLED , .cntStyPck.CboStylePack.ENABLED , .CntPackId.ENABLED , .CntPackId.KbPack.KeyTextBox.ENABLED ,;
               .CntPackId.KbPack.keyCmd.ENABLED , .CntPackId.txtPackClr.ENABLED , .CntPackId.TxtPackSize.ENABLED 

      CASE .parent.ActiveMode = 'A' 
        STORE .F. TO .cntStyPck.ENABLED , .cntStyPck.CboStylePack.ENABLED , .CntPackId.ENABLED , .CntPackId.KbPack.KeyTextBox.ENABLED ,;
               .CntPackId.KbPack.keyCmd.ENABLED , .CntPackId.txtPackClr.ENABLED , .CntPackId.TxtPackSize.ENABLED 
    ENDCASE
    IF .cntStyPck.CboStylePack.Value = 'P'
      m.Cnt = loFormSet.lnGmaNo
      m.Sz1 = loFormSet.lcGmaSz
      .chkPrePack.ENABLED     = .F.
      .kbPrePack.ENABLED      = .F.
      .txtPrePackDesc.ENABLED = .F.    
    ENDIF
  ENDWITH    
ENDIF
*--End lfCONTSHW


*!*************************************************************
*! Name      : lfVALDSKU
*! Developer : Hend Ghanem (HBG)
*! Date      : 06/03/2004
*! Purpose   : To Validate SKU to avoid making same sku for style and pack
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Due to C038119,1
*!*************************************************************
*! Example   : DO lfVALDSKU
*!*************************************************************

FUNCTION lfVALDSKU

lnCurAlias = ALIAS()
lcAccount = ALLTRIM(loFormSet.AriaForm1.KbAccountCode.keytextbox.VALUE)
lcStyle   = IIF(loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S',loFormSet.AriaForm1.kbStyle.Value ,loFormSet.ariaForm1.CntPackId.kbPack.keytextbox.Value)
lcSkuId   = ALLTRIM(loFormSet.AriaForm1.kbSkuId.keytextbox.VALUE)
llStyle   = loFormSet.ariaForm1.cntStyPck.CboStylePack.Value = 'S'
lcConfig  = IIF(loFormSet.llUseConfg,loFormSet.AriaForm1.Ariaconfig1.keytextbox.Value,"")
*B129096,1 MMT 07/26/2005 fix bug of screen ability to enter different sku for same account/style[Start]
*IF !loFormSet.AriaForm1.kbSkuId.selectedfrombrowse AND (EMPTY(lcAccount) OR EMPTY(lcStyle) OR IIF(llStyle,EMPTY(lcConfig) AND (STYLE.cDye_Flg='Y'),.F.))
IF !loFormSet.AriaForm1.kbSkuId.selectedfrombrowse AND (EMPTY(lcAccount) OR EMPTY(lcStyle) OR IIF(llStyle,IIF(loFormSet.llUseConfg,EMPTY(lcConfig) AND (STYLE.cDye_Flg='Y'),.F.),.F.))
*B129096,1 MMT 07/26/2005 fix bug of screen ability to enter different sku for same account/style[End]
  llReturn = .T.
  RETURN 
ENDIF  
lcPckAlias = loFormSet.lcPckAlias
lcGmaColor = PADR(loFormSet.AriaForm1.CntPackId.txtPackClr.Value,6)
lcConfig  = IIF(loFormSet.llUseConfg,loFormSet.AriaForm1.Ariaconfig1.keytextbox.Value,"")
loFormSet.AriaForm1.kbSkuId.keytextbox.OldValue = IIF(EMPTY(loFormSet.AriaForm1.kbSkuId.keytextbox.OldValue)," ",loFormSet.AriaForm1.kbSkuId.keytextbox.OldValue)
IF loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S'
  lcSeekValue = 'S'+lcAccount+PADR(lcStyle,19)+PADR(lcConfig,10)+PADR(lcSkuId,16)
ELSE  
  lcSeekValue = 'S'+lcAccount+PADR(lcStyle,19)+lcGmaColor+loFormSet.lcGmSizeV+PADR(lcSkuId,16)
ENDIF  
IF loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S'
  SET ORDER TO SKUSTYCNFG IN SPCK_HDR
ELSE
  SET ORDER TO SPCK_HDRST IN SPCK_HDR
ENDIF

loFormSet.AriaForm1.kbSkuId.selectedfrombrowse = loFormSet.AriaForm1.kbSkuId.selectedfrombrowse  OR ALLTRIM(lcSkuId) = "?"

IF loFormSet.AriaForm1.kbSkuId.selectedfrombrowse 
  lcSkuId = loFormSet.mSkuBrowse() 
  IF !EMPTY(lcSkuId)
    loFormSet.llSkuValid = .T.
    loFormSet.Recordchanged(5)
    loFormSet.Activemode = 'V'
    loFormSet.changemode('V')
  ENDIF  
ELSE
  IF !EMPTY(lcSkuID) AND !(lcSkuID == loFormSet.AriaForm1.kbSkuId.keytextbox.OldValue)
    *--LANG_SKU_Message       "This SKU"
    lcMessage = LANG_GMA_SKU_Message
    loFormSet.llSkuValid = .F.
    lnReturn = loFormSet.SeekRecord(lcSeekValue,lcMessage)
    loFormSet.AriaForm1.kbStyle.REFRESH
    IF lnReturn  <> 1 AND !loFormSet.llSkuValid
      loFormSet.AriaForm1.kbSkuId.keytextbox.VALUE = ""
    ENDIF      
  ENDIF  
ENDIF
IF loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='P'
  lcSize = lfGetGmSz(loFormSet.lcGmSizeV)
  IF !EMPTY(loFormSet.AriaForm1.kbSkuId.keytextbox.VALUE)
    loFormSet.ARiaForm1.cntSkuqty.txtSizeLbl1.Value = ALLTRIM(lcSize)
  ENDIF  
ENDIF  
loFormSet.AriaForm1.kbSkuId.selectedfrombrowse  = .F.
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfSkuBrows
*! Developer : Hend Ghanem (HBG)
*! Date      : 06/03/2004
*! Purpose   : To Validate SKU to avoid making same sku for style and pack
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Due to C038119,1
*!*************************************************************
*! Example   : DO lfSkuBrows
*!*************************************************************

FUNCTION lfSkuBrows

PRIVATE lcBrFields ,lcBrTitle,lcKey,lnCurAlias,laRetData ,llShowGet ,lcSkuID
lnCurAlias = SELECT(0)
lcAccount  = PADR(loFormSet.ariaForm1.kbAccountCode.keytextbox.Value,5)
lnPadRght  = IIF(loFormSet.llextsize,loFormSet.lnscalepos,19)
lcStyPck   = IIF(loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S',PADR(loFormSet.AriaForm1.kbStyle.VALUE,lnPadRght),;
                 PADR(loFormSet.ariaForm1.CntPackId.kbPack.keytextbox.Value,16))
lcPckAlias = loFormSet.lcPckAlias
lcGmaColor = PADR(loFormSet.AriaForm1.CntPackId.txtPackClr.Value,6)
lcSkuID    = loFormSet.AriaForm1.kbSkuId.keytextbox.VALUE
lcConfig   = IIF(loFormSet.llUseConfg,PADR(loFormSet.AriaForm1.Ariaconfig1.keytextbox.Value,10),SPACE(10))

SET ORDER TO SPCK_HDRVR IN (lcPckAlias)
IF loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S'
  SET ORDER TO SKUSTYCNFG  IN SPCK_HDR
ELSE
  SET ORDER TO SPCK_HDRST IN SPCK_HDR
ENDIF

DECLARE laTmpDat[1]
laTmpDat[1] = ' '

lcFields    = 'Account,pack_id'

lcStyleHeader = IIF(loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S',loFormSet.AriaForm1.kbStyle.lcimjrheader,LANG_GMA_PackHeader)

lcBrFields = [Account :R :H=LANG_GMA_AccountHeader :16,Style     :R :H=lcStyleHeader :19 ,]
IF loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S' AND loFormSet.lluseconfg 
  lcBrFields = lcBrFields + [Dyelot :R :H=LANG_GMA_Configuration :14 ,]
ENDIF
lcBrFields = lcBrFields + [Pack_ID :R :H=LANG_GMA_SKU_ID :16 ,Desc    :R :H=LANG_GMA_DescHeader :30,;
cDivision :R :H=LANG_GMA_DivisionHeader :12,Season   :R :H=LANG_GMA_SeasonHeader  :12]

SELECT Spck_Hdr
=SEEK('S')
COUNT REST WHILE TYPE+Account+STYLE+Pack_Id='S' FOR Pack_Id=lcSkuID TO lnNoSku
llFound   = .F.
DO CASE
  CASE EMPTY(lcAccount) AND EMPTY(lcStyPck)
    IF SEEK('S','Spck_Hdr')
      llFound = .T.
      lcKey = "FOR Type = 'S' "
    ELSE
      llFound = .F.
    ENDIF
  CASE EMPTY(lcAccount) AND !EMPTY(lcStyPck)
    SELECT Spck_Hdr
    LOCATE FOR TYPE = 'S' AND STYLE + Dyelot = lcStyPck + lcConfig
    IF FOUND()
      llFound = .T.
      lcKey = "FOR Type+Style+Dyelot = 'S' + lcStyPck + lcConfig"
    ELSE
      llFound = .F.
    ENDIF
  CASE !EMPTY(lcAccount) AND EMPTY(lcStyPck)
    IF SEEK('S'+lcAccount,'Spck_Hdr')
      llFound = .T.
      lcKey = "FOR Type+Account = 'S' + lcAccount"
    ELSE
      llFound = .F.
    ENDIF
  CASE !EMPTY(lcAccount) AND !EMPTY(lcStyPck)
    lcSeekExpr = IIF(loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S','S'+lcAccount+PADR(lcStyPck,19)+lcConfig,;
                     'S'+lcAccount+PADR(lcStyPck,19)+lcGmaColor+loFormSet.lcGmSizeV)
    IF SEEK(lcSeekExpr,'Spck_Hdr')
      llFound=.T.
      lcKey = IIF(loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S',"'S' + lcAccount+lcStyPck+lcConfig",;
                  "'S'+lcAccount+PADR(lcStyPck,19)+lcGmaColor+loFormSet.lcGmSizeV")
    ELSE
      llFound=.F.
    ENDIF
ENDCASE
lcSkuCode = ''
IF !llFound
  *--There are no records to browse.
  *-- OK
  = gfModalGen("INM42097B42001","Dialog")
  loFormSet.AriaForm1.kbSkuId.keytextbox.VALUE = ""
ELSE
  SELECT Spck_Hdr
  *--LANG_BrowseTitle    'Sku #'
  =gfBrows(lcKey,"Account,Style,Pack_ID,Desc,cDivision,Season","laTmpDat",LANG_GMA_BrowseTitle,'','',.F.)
  IF !EMPTY(laTmpDat)
    loFormSet.AriaForm1.kbAccountCode.keytextbox.VALUE = laTmpDat[1]
    loFormSet.AriaForm1.kbStyle.VALUE = laTmpDat[2]
    loFormSet.AriaForm1.kbSkuId.keytextbox.VALUE       = laTmpDat[3]
    lcSkuCode = laTmpDat[3]
  ENDIF
ENDIF

SELECT(lnCurAlias)
RETURN lcSkuCode 


*!*************************************************************
*! Name      : lfNewRecrd
*! Developer : Hend Ghanem (HBG)
*! Date      : 06/03/2004
*! Purpose   : To Add details of the SKU
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Due to C038119,1
*!*************************************************************
*! Example   : DO lfNewRecrd
*!*************************************************************
FUNCTION lfNewRecrd

lnCurrAlias = SELECT()
lcStyPck    = IIF(loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S',PADR(loFormSet.AriaForm1.kbStyle.VALUE,19),;
                  PADR(loFormSet.ariaForm1.CntPackId.kbPack.keytextbox.Value,16))
lcGmaColor  = PADR(loFormSet.AriaForm1.CntPackId.txtPackClr.Value,6)                  
lcAccount   = PADR(loFormSet.ariaForm1.kbAccountCode.keytextbox.Value,5)
lnScalePos  = loFormSet.lnScalePos
lcConfig    = IIF(loFormSet.llUseConfg,PADR(loFormSet.AriaForm1.Ariaconfig1.keytextbox.Value,10),SPACE(10))
IF loFormSet.llextsize AND loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S'
  lcStyPck = PADR(PADR(lcStyPck,lnScalePos)+loFormSet.lcscale,19)
ENDIF
lcPckAlias    = loFormSet.lcPckAlias
SET ORDER TO SPCK_HDRVR IN (lcPckAlias)
llSeek = IIF(loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S',SEEK(lcStyPck,'Style'),;
             SEEK('P'+lcAccount+PADR(lcStyPck,16)+lcGmaColor+loFormSet.lcGmSizeV,(lcPckAlias)))
IF llSeek
  *-- in case of extended size scale we have to create one record for each scale
  IF loFormSet.llextsize
    SELECT STYLE
    =SEEK(PADR(lcStyPck,lnScalePos))
    lnRecNO = 1
    SCAN REST WHILE PADR(STYLE.STYLE,lnScalePos) = PADR(lcStyPck,lnScalePos)
      SELECT (loFormSet.lcPackLines)
      APPEND BLANK
      REPLACE STYLE   WITH STYLE.STYLE ,;
              Dyelot  WITH lcConfig          ,;
              Account WITH lcAccount         ,;
              TYPE    WITH 'S'               ,;
              Pack_id WITH Spck_hdr.Pack_id + IIF(Customer.Skutmpl='JCP',"*ONLY","")
      IF SUBSTR(STYLE.STYLE,lnScalePos+1) == loFormSet.lcscale
        lnRecNO = RECNO()
      ENDIF
    ENDSCAN
    =SEEK(lcStyPck,'Style')
    SELECT (loFormSet.lcPackLines)
    loFormSet.lnLastRec = RECNO()
    IF BETWEEN(lnRecNO,1,RECCOUNT())
      GO lnRecNO
    ENDIF
  ELSE    && not extended size scale
    SELECT (loFormSet.lcPackLines)
    APPEND BLANK
    REPLACE TYPE    WITH 'S'     ,;
            Account WITH lcAccount        ,;
            Pack_id WITH Spck_hdr.Pack_id +IIF(Customer.Skutmpl='JCP',"*ONLY","") ,;
            STYLE   WITH lcStyPck,;
            Dyelot  WITH lcConfig
  ENDIF
  SELECT Spck_hdr
  REPLACE SEASON    WITH STYLE.SEASON    ,;
          cDivision WITH STYLE.cDivision ,;
          SKU       WITH Pack_id         ,;
          Pack_id   WITH Pack_id +IIF(Customer.Skutmpl='JCP',"*ONLY",""),;
          Dyelot    WITH lcConfig
  
  WITH loFormSet.ARiaForm1
    IF loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S'
      .txtScaleId.VALUE = STYLE.SCALE
      .cntSkuqty.SCALE  = STYLE.SCALE
    ELSE
      .txtScaleId.VALUE = PADR(loFormSet.lcGmSizeV,1)
      .cntSkuqty.SCALE  = PADR(loFormSet.lcGmSizeV,1)
    ENDIF
    IF !EMPTY(.txtScaleId.VALUE) AND SEEK('S'+.txtScaleId.VALUE,'SCALE')
      .txtScaleDesc.Value = Scale.cScl_Desc
    ELSE
      .txtScaleDesc.Value = ''
    ENDIF    
    .cntSkuqty.mdisableunused
  ENDWITH
ENDIF

loFormSet.mrefreshall
SELECT (lnCurrAlias)


*!*************************************************************
*! Name      : lfGETDATA
*! Developer : Hend Ghanem (HBG)
*! Date      : 06/03/2004
*! Purpose   : To get details of the SKU
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Due to C038119,1
*!*************************************************************
*! Example   : DO lfGETDATA
*!*************************************************************
FUNCTION lfGETDATA


loFormSet.mGetFldsValu()

IF loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='P'
  loFormSet.AriaForm1.CntPackId.kBPACK.keytextbox.Value = Spck_hdr.style
  loFormSet.AriaForm1.CntPackId.txtPackClr.Value  = SPCK_HDR.cPkColor
  loFormSet.lcGmSizeV = SPCK_HDR.cPckSize
  loFormSet.AriaForm1.CntPackId.TxtPackSize.Value = lfGetGmSz(Spck_Hdr.cPckSize)
ENDIF
lcStyle    = IIF(loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S',loFormSet.AriaForm1.kbStyle.VALUE,;
                  loFormSet.ariaForm1.CntPackId.kbPack.keytextbox.Value)
lcGmaColor = PADR(loFormSet.AriaForm1.CntPackId.txtPackClr.Value,6)                  
lcAccount  = PADR(loFormSet.ariaForm1.kbAccountCode.keytextbox.Value,5)
lcSkuID    = loFormSet.AriaForm1.kbSkuId.keytextbox.VALUE
lcConfig   = IIF(loFormSet.llUseConfg,PADR(loFormSet.AriaForm1.Ariaconfig1.keytextbox.Value,10),SPACE(10))
IF loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S'
  SET ORDER TO SKUSTYCNFG IN SPCK_HDR
  =SEEK('S'+lcAccount+PADR(lcStyle,19)+lcConfig+PADR(lcSkuID,16),'SPCK_HDR')
ELSE
  SET ORDER TO SPCK_HDRST IN SPCK_HDR
  =SEEK('S'+lcAccount+PADR(lcStyle,19)+lcGmaColor+loFormSet.lcGmSizeV+PADR(lcSkuID,16),'SPCK_HDR')
ENDIF  
lcPckAlias   = loFormSet.lcPckAlias
SET ORDER TO SPCK_HDRVR IN (lcPckAlias)
=SEEK('P'+lcAccount+PADR(lcStyle,16)+lcGmaColor+loFormSet.lcGmSizeV,lcPckAlias)
WITH loFormSet.ARiaForm1
  IF loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S'
    IF !EMPTY(lcStyle) AND SEEK(lcStyle,'Style')
      .txtScaleId.VALUE   = STYLE.SCALE
      .TxtStyleDesc.VALUE = STYLE.DESC
      .cntSkuqty.SCALE    = STYLE.SCALE
    ELSE
      .txtScaleId.VALUE   = ''
    ENDIF
  ELSE
    .txtScaleId.VALUE   = PADR(loFormSet.lcGmSizeV,1)
    .TxtStyleDesc.VALUE = &lcPckAlias..DESC
    .cntSkuqty.SCALE    = PADR(loFormSet.lcGmSizeV,1)
  ENDIF  
  IF !EMPTY(.txtScaleId.VALUE) AND SEEK('S'+.txtScaleId.VALUE,'SCALE')
    .txtScaleDesc.Value = Scale.cScl_Desc
  ELSE
    .txtScaleDesc.Value = ''
  ENDIF    
 
  IF !EMPTY(lcAccount) AND SEEK('M'+lcAccount,'Customer')
    .txtAccountName.VALUE = Customer.Btname
  ENDIF
ENDWITH

lnScalePos = IIF(loFormSet.llextsize,loFormSet.lnScalePos,19)
lcSKUHdr = ALLTRIM(SUBSTR(lcSkuID,1,LEN(ALLTRIM(Spck_Hdr.Pack_ID))))
*--get the record depending on the Style file to handle the case of extended size scale
IF loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S'
  SELECT STYLE
  SCAN REST WHILE STYLE = PADR(lcStyle,lnScalePos)
    lcGetStyle=STYLE.STYLE
    SELECT Spck_Lin
    SET ORDER TO SPKLNSTCN 
    IF SEEK('S'+lcAccount+lcGetStyle+lcConfig+ALLTRIM(lcSKUHdr))
      SCATTER MEMVAR MEMO
      STORE "" TO m.Size1,m.Size2,m.Size3,m.Size4,m.Size5,m.Size6,m.Size7,m.Size8
      m.llPrePack = .F.
      *-- if it is a Prepack the Record in the Spck_lin will hold only the prepack code so
      *-- we have to get the sku for all sizes from the Scale Table
      IF !EMPTY(m.PrePak)    && prepack
        IF SEEK('P'+STYLE.SCALE+m.PrePak,'SCALE')
          FOR lnCount = 1 TO 8
            lcCount = ALLTRIM(STR(lnCount))
            m.SIZE&lcCount = IIF(SCALE.pp&lcCount>0,ALLTRIM(STR(SCALE.pp&lcCount)),'')
          ENDFOR
        ENDIF
        FOR lnI =1 TO 8
          lcI = STR(lnI,1)
          loFormSet.ariaForm1.cntSkuqty.txtQty&lcI..Value = m.SIZE&lcI
        ENDFOR
        loFormSet.ariaForm1.cntSkuqty.txtTotQty.Value = m.SIZE1
        m.llPrePack = .T.
      ELSE          && sku
        *-- the Sku per size saved in the spck_lin ( one record for each size ) and in the Temp
        *-- file will hold all the sizes in one record so we have to retrive the data from 8 records
        *-- (the spck_lin ) to be one record in the temp. file
        SCAN REST WHILE TYPE+Account+STYLE+DYELOT+Pack_ID = 'S'+lcAccount+lcGetStyle+lcConfig+ALLTRIM(lcSKUHdr)
          FOR lnCount = 1 TO 8
            lcCount = ALLTRIM(STR(lnCount))
            IF !EMPTY(QTY&lcCount)
              m.SIZE&lcCount = ALLTRIM(SUBSTR(Pack_ID,LEN(ALLTRIM(Spck_Hdr.Pack_ID))+1))
            ENDIF
          ENDFOR
        ENDSCAN
        FOR lnI =1 TO 8
          lcI = STR(lnI,1)
          loFormSet.ariaForm1.cntSkuqty.txtQty&lcI..Value = m.SIZE&lcI
        ENDFOR
        loFormSet.ariaForm1.cntSkuqty.txtTotQty.Value = m.SIZE1        
      ENDIF
    ELSE
      *--if  the record is not exist in the temp file , we have to create it becasue this is
      *-- the case of extended size scale
      SCATTER MEMVAR MEMO BLANK
      m.Type    = 'S'
      m.Style   = STYLE.STYLE
      m.Pack_ID = lcSkuID
      m.Account = lcAccount
      STORE "" TO m.Size1,m.Size2,m.Size3,m.Size4,m.Size5,m.Size6,m.Size7,m.Size8
    ENDIF
    INSERT INTO (loFormSet.lcPackLines) FROM MEMVAR
  ENDSCAN
ELSE
  lcGetStyle=PADR(loFormSet.ariaForm1.CntPackId.kbPack.keytextbox.Value,19)+lcGmaColor+loFormSet.lcGmSizeV
  SELECT Spck_Lin
  SET ORDER TO SPCK_LINST
  IF SEEK('S'+lcAccount+lcGetStyle+ALLTRIM(lcSKUHdr))
    SCATTER MEMVAR MEMO
    STORE "" TO m.Size1,m.Size2,m.Size3,m.Size4,m.Size5,m.Size6,m.Size7,m.Size8
    m.llPrePack = .F.
    SCAN REST WHILE TYPE+Account+STYLE+cPkColor+cpckSize+Pack_ID = 'S'+lcAccount+lcGetStyle+ALLTRIM(lcSKUHdr)
      FOR lnCount = 1 TO 8
        lcCount = ALLTRIM(STR(lnCount))
        IF !EMPTY(QTY&lcCount)
          m.SIZE&lcCount = ALLTRIM(SUBSTR(Pack_ID,LEN(ALLTRIM(Spck_Hdr.Pack_ID))+1))
        ENDIF
      ENDFOR
    ENDSCAN
    FOR lnI =1 TO 8
      lcI = STR(lnI,1)
      loFormSet.ariaForm1.cntSkuqty.txtQty&lcI..Value = m.SIZE&lcI
    ENDFOR
    loFormSet.ariaForm1.cntSkuqty.txtTotQty.Value = m.SIZE1          
  ELSE
    *--if  the record is not exist in the temp file , we have to create it becasue this is
    *-- the case of extended size scale
    SCATTER MEMVAR MEMO BLANK
    m.Type    = 'S'
    m.Style   = PADR(loFormSet.ariaForm1.CntPackId.kbPack.keytextbox.Value,19)
    m.Pack_ID = lcSkuID
    m.Account = lcAccount
    STORE "" TO m.Size1,m.Size2,m.Size3,m.Size4,m.Size5,m.Size6,m.Size7,m.Size8
  ENDIF
  INSERT INTO (loFormSet.lcPackLines) FROM MEMVAR
ENDIF  
SELECT (loFormSet.lcPackLines)
GO BOTTOM
loFormSet.lnlastrec = RECNO()
LOCATE
loFormSet.mrefreshall


*!*************************************************************
*! Name      : lfSAVSRGMA
*! Developer : Hend Ghanem (HBG)
*! Date      : 06/03/2004
*! Purpose   : To make local Save for GMA
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Due to C038119,1
*!*************************************************************
*! Example   : DO lfSAVSRGMA
*!*************************************************************
FUNCTION lfSAVSRGMA
PRIVATE lnCurAlias,lcCurNdx,lnChoice
lcAccount = PADR(loFormSet.ariaForm1.kbAccountCode.keytextbox.Value,5)
lcStyle   = loFormSet.Ariaform1.kbStyle.VALUE
lcSku     = loFormSet.Ariaform1.kbSkuId.keytextbox.VALUE
lcPack    = loFormSet.ariaForm1.CntPackId.kbPack.keytextbox.Value
lcConfig  = IIF(loFormSet.llUseConfg,PADR(loFormSet.AriaForm1.Ariaconfig1.keytextbox.Value,10),SPACE(10))      
lcGmaColor  = PADR(loFormSet.AriaForm1.CntPackId.txtPackClr.Value,6)
llStyle = (loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S')
lcLinFil  = loFormSet.lcPackLines
*--B99999,1 MMT 02/10/05,Fixing the bug of saving same SKU twice [Begin]
IF loFormSet.ActiveMode = 'A'
   IF SEEK('S'+lcAccount+lcSku,'SPCK_HDR','SPCK_HDR')
     *B128017,1 HBG 5/17/2005 Fix bug of not checking in for SKU in size level [Begin]
   *!*       =gfModalGen('TRM00437B00000', 'DIALOG')
   *!*       llReturn = .F.
   *!*       RETURN llReturn
     FOR lnI = 1 TO 8
       lcI = STR(lnI,1) 
       lcSiz = ALLTRIM(loFormSet.ariaForm1.cntSkuqty.txtQty&lcI..value)
       IF !EMPTY(lcSiz)
         lcKey = "S"+lcAccount+ALLTRIM(lcSku)+lcSiz
         lcPrvOrd = ORDER('SPCK_LIN')
         SET ORDER TO SPCK_LIN IN SPCK_LIN
         IF SEEK(lcKey,'SPCK_LIN')
           =gfModalGen('TRM00437B00000', 'DIALOG')
           llReturn = .F.
           RETURN llReturn
         ENDIF
       ENDIF  
     ENDFOR  
     *B128017,1 [End]     
  ENDIF 
ENDIF 
*--B99999,1 MMT 02/10/05,Fixing the bug of saving same SKU twice [End]
SELECT SPCK_HDR
lcFiltHdr = FILTER()
SET FILTER TO 
SELECT SPCK_LIN
lcFiltLin = FILTER()
SET FILTER TO 
SELECT (lcLinFil)
lnToTQty = loFormSet.ariaForm1.cntSkuqty.txtTotQty.Value
*--check frist if there is at least one sku create or prepack
IF EOF() OR BOF() OR DELETED() OR EMPTY(lnToTQty)
  *-- You have to enter at least one SKU or create prepak
  *-- <Ok>
  = gfModalGen("INM42263B42001","Dialog")
  llReturn = .F.
ELSE
  SCAN
    *--if it is a prepack and the user didn't select the prepack code
    IF !EMPTY(&lcLinFil..SKU) AND EMPTY(&lcLinFil..Prepak)
      *-- You have to select prepak
      *-- <OK>
      = gfModalGen("QRM42100B42001","Dialog")
      llReturn = .F.
      EXIT
    ENDIF
  ENDSCAN
  IF llReturn
    *--delete the existing records in the Spck_lin
    SELECT Spck_Lin
    IF llStyle
      SET ORDER TO SPKLNSTCN 
    ELSE
      SET ORDER TO Spck_linst
    ENDIF
    lcSeekExp = IIF(llStyle,"TYPE+Account+STYLE+DYELOT+Pack_ID","TYPE+Account+STYLE+cPkColor+cPckSize+Pack_ID")
    lcSeekVal = IIF(llStyle,"S"+lcAccount+lcStyle+lcConfig,"S"+lcAccount+PADR(lcPack,19)+lcGmaColor+loFormSet.lcGmSizeV)
    IF SEEK(lcSeekVal,'Spck_Lin')
      SCAN REST FOR &lcSeekExp = lcSeekVal
        IF ALLTRIM(Pack_ID) = ALLTRIM(lcSku)
          DELETE
        ENDIF
      ENDSCAN
    ENDIF
    SELECT(lcLinFil)
    lnTotRec  = 0
    COUNT  TO lnTotRec
    *--LANG_ProgCap   = "Updating Style Pack Detail file"
    *-- Initialize the progress bar needed variables.
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    *!*      loToolBarWindow = oAriaApplication.oToolBar.oWindParent 
    loToolBarWindow = loFormSet.oToolBar.oWindParent 
    *HBG [End]
    loFormSet.oProgress.TotalProgress = lnTotRec
    
    loFormSet.oProgress.lblFirstLabel.Caption = LANG_GMA_ProgCap
    loFormSet.oProgress.Show()
    *-- Make this incermental variable private to be shown
    lnCurRec  = 1
    *--all the specified sku(S) for all sizes saved in the temp file in one Record
    *--so in the master file we have to create a record for each size
    *-- but in case of prepack we have to create only one record
    *-- this scan loop to handel the case of Extended size scale
    SELECT(lcLinFil)
    SCAN
      SCATTER MEMVAR
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.totqty
      IF lnTotRec > 1
        loFormSet.oProgress.CurrentProgress(lnCurRec)
      ENDIF
      lnCurRec   = lnCurRec + 1
      lnScaleCnt = 1
      IF llStyle AND SEEK(m.Style,'Style') AND SEEK('S'+STYLE.SCALE,'SCALE')
        lnScaleCnt = SCALE.CNT
      ENDIF
      FOR lnCount = 1 TO lnScaleCnt
        lcCount = ALLTRIM(STR(lnCount))
        IF !EMPTY(&lcLinFil..SIZE&lcCount)
          m.Account  = lcAccount
          m.Dyelot   = loFormSet.ariaform1.ariaconfig1.keytextbox.Value
          m.Pack_ID  = ALLTRIM(lcSku)+ALLTRIM(&lcLinFil..SIZE&lcCount)
          REPLACE PAck_id WITH ALLTRIM(lcSku)+ALLTRIM(&lcLinFil..SIZE&lcCount)
          m.cPkColor = IIF(llStyle,"",lcGmaColor)
          m.cPckSize = IIF(llStyle,"",loFormSet.lcGmSizeV)
          m.cPkVersion = IIF(llStyle,"","1   ")
          m.cSkuType   = IIF(llStyle,'S','P')
          m.Qty&lcCount = 1
          m.totqty      = 1
          INSERT INTO Spck_Lin FROM MEMVAR 
          m.Qty&lcCount = 0
        ENDIF
      ENDFOR
    ENDSCAN
    SELECT(lcLinFil)
    LOCATE
    IF llStyle
      SET ORDER TO SKUSTYCNFG IN SPCK_HDR
      =SEEK("S"+lcAccount+lcStyle+lcConfig,'SPCK_HDR')    
    ELSE
      SET ORDER TO Spck_HdrSt IN SPCK_HDR
      =SEEK("S"+lcAccount+PADR(lcPack,19)+lcGmaColor+loFormSet.lcGmSizeV,'SPCK_HDR') 
    ENDIF  
    
    lnScalePos = loFormSet.lnScalePos
    IF llStyle AND loFormSet.llextsize
      lcStyle = PADR(PADR(lcStyle,lnScalePos)+loFormSet.lcscale,19)
    ENDIF
    =SEEK(lcStyle,'Style')
    SELECT Spck_hdr
    REPLACE type       WITH 'S'             ,;
            Account    WITH lcAccount       ,;
          Style      WITH lcStyle         ,;  
          Desc       WITH loFormSet.ariaForm1.txtPackName.Value ,;
          Pack_id    WITH lcSku           ,;
            SEASON     WITH loFormSet.ariaForm1.cboSeason.Value   ,;
            cDivision  WITH loFormSet.ariaForm1.cboDivision.Value ,;
            SKU        WITH Pack_id         ,;
            Dyelot     WITH lcConfig        ,;
            cSkuType   WITH IIF(llStyle,'S','P'),;
            cPkColor   WITH IIF(llStyle,"",lcGmaColor),;
            cPckSize   WITH IIF(llStyle,"",loFormSet.lcGmSizeV),;
            cPkVersion WITH IIF(llStyle,"","1   ")
    *HBG 10/23/2004 Save the user define field in SPCK_HDR [Begin]
    lnFldPos = ASCAN(loFormSet.lauserfields ,UPPER('cSkuType'))
    IF lnFldPos > 0
      lnFldPos = ASUBSCRIPT(loFormSet.lauserfields , lnFldPos , 1)
      loFormSet.lauserfields[lnFldPos,6] = IIF(llStyle,'S','P')
    ENDIF
    lnFldPos = ASCAN(loFormSet.lauserfields ,UPPER('cPkColor'))
    IF lnFldPos > 0
      lnFldPos = ASUBSCRIPT(loFormSet.lauserfields , lnFldPos , 1)
      loFormSet.lauserfields[lnFldPos,6] = IIF(llStyle,"",lcGmaColor)
    ENDIF
    lnFldPos = ASCAN(loFormSet.lauserfields ,UPPER('cPckSize'))
    IF lnFldPos > 0
      lnFldPos = ASUBSCRIPT(loFormSet.lauserfields , lnFldPos , 1)
      loFormSet.lauserfields[lnFldPos,6] = IIF(llStyle,"",loFormSet.lcGmSizeV)
    ENDIF
    lnFldPos = ASCAN(loFormSet.lauserfields ,UPPER('cPkVersion'))
    IF lnFldPos > 0
      lnFldPos = ASUBSCRIPT(loFormSet.lauserfields , lnFldPos , 1)
      loFormSet.lauserfields[lnFldPos,6] = IIF(llStyle,"","1   ")
    ENDIF
    *HBG [End]       
    loFormSet.oProgress.finishprogress
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    *oAriaApplication.oToolBar.oWindParent = loToolBarWindow 
    loFormSet.oToolBar.oWindParent = loToolBarWindow 
    *HBG [End]
  ENDIF
ENDIF
SELECT SPCK_HDR
SET FILTER TO &lcFiltHdr
SELECT SPCK_LIN
SET FILTER TO &lcFiltLin

RETURN llReturn


*!*************************************************************
*! Name      : lfChBrwFld
*! Developer : Hend Ghanem (HBG)
*! Date      : 06/03/2004
*! Purpose   : Change the browse fields
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Due to C038119,1
*!*************************************************************
*! Example   : DO lfChBrwFld
*!*************************************************************
FUNCTION lfChBrwFld

lcStyleHeader = IIF(loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S',loFormSet.AriaForm1.kbStyle.lcimjrheader,LANG_GMA_PackHeader)

lcBrFields = [Account :R :H=LANG_GMA_AccountHeader :16,Style     :R :H=lcStyleHeader :19 ,]
IF loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S' AND loFormSet.lluseconfg 
  lcBrFields = lcBrFields + [Dyelot :R :H=LANG_GMA_Configuration :14 ,]
ENDIF
lcBrFields = lcBrFields + [Pack_ID :R :H=LANG_GMA_SKU_ID :16 ,Desc    :R :H=LANG_GMA_DescHeader :30,;
cDivision :R :H=LANG_GMA_DivisionHeader :12,Season   :R :H=LANG_GMA_SeasonHeader  :12]

loFormSet.AriaBrFields.edtBrowseFields.value = lcBrFields 

*!*************************************************************
*! Name      : lfDisplSiz
*! Developer : Hend Ghanem (HBG)
*! Date      : 06/03/2004
*! Purpose   : Change the filter of the browse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Due to C038119,1
*!*************************************************************
*! Example   : DO lfDisplSiz
*!*************************************************************
FUNCTION lfDisplSiz

IF TYPE("_screen.ActiveForm.cntStyPck") = 'O' AND loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='P'
  lnSzCount = 1
ENDIF  

*!*************************************************************
*! Name      : lfDELSRGMA
*! Developer : Hend Ghanem (HBG)
*! Date      : 06/03/2004
*! Purpose   : Change the filter of the browse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Due to C038119,1
*!*************************************************************
*! Example   : DO lfDELSRGMA
*!*************************************************************
FUNCTION lfDELSRGMA

llStyle = (loFormSet.ariaForm1.cntStyPck.CboStylePack.Value ='S')
lcAccount = loFormSet.Ariaform1.kbAccountCode.keytextbox.VALUE
lcStyle   = IIF(llStyle,PADR(loFormSet.Ariaform1.kbStyle.VALUE,19),;
                        PADR(loFormSet.ariaForm1.CntPackId.kbPack.keytextbox.Value,19)+;
                        PADR(loFormSet.AriaForm1.CntPackId.txtPackClr.Value,6)+loFormSet.lcGmSizeV)
lcSku     = loFormSet.Ariaform1.kbSkuId.keytextbox.VALUE
lcConfig  = IIF(loFormSet.lluseconfg,loFormSet.Ariaform1.ariaconfig1.keytextbox.Value,"")

IF llStyle
  lcKey = 'S'+lcAccount+PADR(lcStyle,19)+PADR(lcConfig,10)+ALLTRIM(lcSku)
  lcExpr = 'type+account+style+dyelot+ALLTRIM(SUBSTR(Pack_id,1,LEN(ALLTRIM(Spck_Hdr.Pack_ID))))'
ELSE
  lcKey = 'S'+lcAccount+lcStyle+ALLTRIM(lcSku)
  lcExpr = 'type+account+style+cpkcolor+cpcksize+ALLTRIM(SUBSTR(Pack_id,1,LEN(ALLTRIM(Spck_Hdr.Pack_ID))))'
ENDIF

IF llStyle
  SET ORDER TO SKUSTYCNFG IN SPCK_HDR
ELSE
  SET ORDER TO SPCK_HDRST IN SPCK_HDR
ENDIF  
IF llStyle
  SET ORDER TO SPKLNSTCN  IN SPCK_LIN
ELSE
  SET ORDER TO SPCK_LINST IN SPCK_LIN
ENDIF  
*!* B608488,1 MHM 03/19/2008 Fix deleting SKU lines not deletd [Start]
lcCurrAlias = ALIAS()
SELECT SPCK_LIN
IF SEEK(lcKey)
  DELETE REST FOR &lcExpr = lcKey
ENDIF  
SELECT (lcCurrAlias)
*!* B608488,1 MHM 03/19/2008 Fix deleting SKU lines not deletd [End]

*!*************************************************************
*! Name      : lfGETREL1      (C102570,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfGETREL1()
*!*************************************************************
*!
FUNCTION lfGETREL1

SET RELATION OFF INTO SPck_Hdr
SET RELATION TO Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion INTO SPck_Hdr
lcBrFields = [Account :H="Account",Pack_Id :H="Pack Id",cPkColor :H="Color",lcGmSize=lfGetGmSz(SPck_Hdr.cpcksize):H="Size",cPkVersion :H="Version",SPck_Hdr.Desc :H='Description',TotQty :H='Quantity']

*!*************************************************************
*! Name      : lfADDMPAR      (C102452,1)
*! Developer : Wael Abo-Shawareb (WSH)
*! Date      : 07/08/2004
*! Purpose   : Add a menu PAR in the Options Menu for Style 
*!             Classification screen
*!*************************************************************
*! Calls     : lfFoundPad()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfADDMPAR()
*!*************************************************************
*!
FUNCTION lfADDMPAR

LOCAL lnBarNo

*--Check if the Options Pad Menu was already defined
IF !lfFoundPad('Option')
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*    DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  *!*    ON PAD _Option OF _MSYSMENU ACTIVATE POPUP _lPopOpt
  DEFINE PAD _Option OF (loFormSet.chostformname) PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF (loFormSet.chostformname) ACTIVATE POPUP _lPopOpt
  *HBG [End]
  *--Define option pasd bars.
  DEFINE POPUP _lPopOpt MARGIN SHADOW
ENDIF

lnBarNo = CNTBAR('_lPopOpt') + 1

*--Add menu Bar for Style Classification Screen
IF lnBarNo > 1
  DEFINE BAR lnBarNo OF _lPopOpt PROMPT '\-'
  lnBarNo = lnBarNo + 1
ENDIF

DEFINE BAR lnBarNo OF _lPopOpt PROMPT 'Style Cla\<ssification'
ON SELECTION BAR lnBarNo OF _lPopOpt _SCREEN.ActiveForm.parent.mDoTrigger(PADR('RUNSTYCLS',10))

*C126727,1 HBG 04/06/2005 Add option for Customer packing information [Begin]

*B129029,1 KHM 07/17/2005 Check if the form is an object [Begin]
*DEFINE BAR lnBarNo+1 OF _lPopOpt PROMPT 'Customer Pac\<king Info' SKIP FOR gfFormIsActive(_screen.ActiveForm.Parent.chostformname) AND (_SCREEN.ActiveForm.parent.ActiveMode = 'S')
DEFINE BAR lnBarNo+1 OF _lPopOpt PROMPT 'Customer Pac\<king Info' SKIP FOR TYPE('_screen.ActiveForm.Parent') = 'O' AND ;
TYPE("_screen.ActiveForm.Parent.cHostFormName") = "C" AND (_SCREEN.ActiveForm.parent.ActiveMode = 'S')
ON SELECTION BAR lnBarNo+1 OF _lPopOpt _SCREEN.ActiveForm.parent.mDoTrigger(PADR('CSTPKINF',10))
*B129029,1 KHM 07/17/2005 [End]

ON SELECTION BAR lnBarNo+1 OF _lPopOpt _SCREEN.ActiveForm.parent.mDoTrigger(PADR('CSTPKINF',10))
*C126727,1 [End]

*--Activate Options Menu
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*SET SKIP OF PAD _Option OF _MSYSMENU _SCREEN.ActiveForm.parent.ActiveMode = 'S'

*B129029,1 KHM 07/17/2005 Check if the form is an object [Begin]
*SET SKIP OF PAD _Option OF (loFormSet.chostformname) _SCREEN.ActiveForm.parent.ActiveMode = 'S'
SET SKIP OF PAD _Option OF (loFormSet.chostformname)  TYPE('_screen.ActiveForm.Parent') = 'O' AND ;
TYPE("_screen.ActiveForm.Parent.cHostFormName") = "C" AND _SCREEN.ActiveForm.parent.ActiveMode = 'S'
*B129029,1 KHM 07/17/2005 [End]

*HBG [End]

RETURN

*!*************************************************************
*! Name      : lfRUNSTYCLS      (C102452,1)
*! Developer : Wael Abo-Shawareb (WSH)
*! Date      : 07/08/2004
*! Purpose   : Runs Style Classification screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRUNSTYCLS()
*!*************************************************************
*!
FUNCTION lfRUNSTYCLS

DO FORM (oAriaApplication.ScreenHome + "IC\ICSTYCLS") ;
        WITH loFormSet, 'Style', loFormSet.lcStyleKey, loFormSet.lcColorFil

*!*************************************************************
*! Name      : lfRemoveOpt      (C102452,1)
*! Developer : Wael Abo-Shawareb (WSH)
*! Date      : 07/08/2004
*! Purpose   : Release Options Menu
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRemoveOpt()
*!*************************************************************
*!
FUNCTION lfRemoveOpt

*--Check if the Options Pad Menu was not Release before
IF lfFoundPad('Option')
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *RELEASE PAD _Option OF _MSYSMENU
  RELEASE PAD _Option OF (loFormSet.chostformname)
  *HBG [End]
ENDIF

*!*************************************************************
*! Name      : lfDISABEGRP      (C102452,1)
*! Developer : Wael Abo-Shawareb (WSH)
*! Date      : 07/08/2004
*! Purpose   : Disable Group Combo in Style Screen as it is 
*!             Modified from Style Classification Screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfDISABEGRP()
*!*************************************************************
*!
FUNCTION lfDISABEGRP

*--Disable Group Field in Style Screen
IF loFormSet.ActiveMode $ 'AE'
  loFormSet.AriaForm1.pgfStyleInfo.Page1.cboGroup.Enabled = .F.
ENDIF

*--Assign Departmrent to Item According to Item Group in Edit Mode
IF loFormSet.ActiveMode $ 'E'
  IF !USED('ICDEPTHD')
    =gfOpenFile(oAriaApplication.DataDir + 'ICDEPTHD', 'CGROUP', 'SH')
  ENDIF

  IF !EMPTY(Style.cStyGroup)
    SELECT ICDEPTHD
    SET ORDER TO CGROUP
    =SEEK(Style.cStyGroup)
    REPLACE Dept WITH ICDEPTHD.Dept IN STYLE
  ENDIF
ENDIF

*--Activate Options Menu
*HBG
*SET SKIP OF PAD _Option OF _MSYSMENU _SCREEN.ActiveForm.parent.ActiveMode = 'S'
SET SKIP OF PAD _Option OF (loFormSet.chostformname) _SCREEN.ActiveForm.parent.ActiveMode = 'S'
*HBG

RETURN

*!*************************************************************
*! Name      : lfvDeptGrp      (C102452,1)
*! Developer : Wael Abo-Shawareb (WSH)
*! Date      : 07/08/2004
*! Purpose   : Disable Group Combo in Style Screen as it is 
*!             Modified from Style Classification Screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDeptGrp()
*!*************************************************************
*!
FUNCTION lfvDeptGrp

LOCAL lnAlias
lnAlias = SELECT()

IF EMPTY(Style.Dept) OR EMPTY(Style.cStyGroup)
  =gfModalGen('TRM00000B00000', 'DIALOG', .F., .F., LANG_GMA_EnterDeptAndGroup)
  llAcceptStySav = .F.
  SELECT (lnAlias)
  RETURN .F.
ENDIF

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfFoundPad
*! Developer : Wael Abo-Shawareb (WSH)
*! Date      : 07/08/2004
*! Purpose   : check if any pad menu is exit in _sysmenu
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*C102452,1 WSH
FUNCTION lfFoundPad
PARAMETER lcPadName

LOCAL llFound
llFound = .F.

*!*  FOR lnCount = 1 TO CNTPAD('_MSYSMENU')    && Number of pads
*!*    IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = lcPadName
FOR lnCount = 1 TO CNTPAD(loFormSet.chostformname)    && Number of pads
  IF PRMPAD(loFormSet.chostformname, GETPAD(loFormSet.chostformname, LnCount)) = lcPadName

    llfound = .T.
    EXIT
  ENDIF
ENDFOR
RETURN(llFound)
*-- end of lfFoundPad.

*!*************************************************************
*! Name      : lfChkRgPck
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : Check if there are range packs in this sales order
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Due to C037922,1
*!*************************************************************
*! Example   : =lfChkRgPck()
*!*************************************************************
FUNCTION lfChkRgPck

IF loFormSet.ActiveMode <> 'S'
  loFormSet.llRngPack = .F.
  *E125004,1 HBG 12/12/2004 Enable the option "Modify # of ordered packs" in case of bulk order [Begin]
  *loFormSet.llFromBulk = .F.
  *E125004,1 [End]
  lcPrvAls = ALIAS()
  oEditRegion = loFormSet.AriaForm1.Ariapageframe1.Page2.AriaEditRegion1

  lcPrvOrd = ORDER('SPCK_HDR')
  SET ORDER TO SPCK_HDRVr IN SPCK_HDR
  SELECT (oEditRegion.Detailfile)
  lnRecNo = RECNO()
  COUNT FOR !EMPTY(Pack_ID) AND lRange TO lnRange
  COUNT FOR !EMPTY(Pack_ID) AND !lRange TO lnNtRange
  loFormSet.llRngPack = (lnRange <> 0 )
  loFormSet.llRngPack = (lnNtRange = 0 )
  COUNT FOR !EMPTY(Pack_ID) AND !EMPTY(cFromOrder) TO lnFromBlk
  COUNT FOR !EMPTY(Pack_ID) AND EMPTY(cFromOrder) TO lnNtFromBlk
  *E125004,1 HBG 12/12/2004 Enable the option "Modify # of ordered packs" in case of bulk order [Begin]
  *!*  loFormSet.llFromBulk = (lnFromBlk <> 0 )
  *!*  loFormSet.llFromBulk = (lnNtFromBlk = 0)
  *E125004,1 [End]
  SELECT (oEditRegion.Detailfile)
  IF BETWEEN(lnRecNo, 1, RECCOUNT())
    GO lnRecNo
  ENDIF
  SET ORDER TO (lcPrvOrd) IN SPCK_HDR
  SELECT (lcPrvAls)              
ENDIF

*!*************************************************************
*! Name      : lfPckBrow
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : Pack Browse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Due to C037922,1
*!*************************************************************
*! Example   : =lfPckBrow()
*!*************************************************************
FUNCTION lfPckBrow

PRIVATE lcBrFields ,lcBrTitle,lcKey,lnCurAlias,laRetData

STORE .F. TO loFormSet.AriaForm1.CntPackId.llEmptyAcc
lnCurAlias = SELECT(0)
lcBrTitle  = 'Available Packs'

*!B999999,1 WSH 02/05/2005, Select the Pack Cursor to work with. "Solve Bug of Variable not found" [Start]
SELECT (loFormSet.lcPckAlias)
*!B999999,1 WSH 02/05/2005, [End]


SET RELATION OFF INTO STYLEUPC ADDITIVE  
SET RELATION TO Account+Pack_Id+'   '+cPkColor+cPckSize INTO STYLEUPC ADDITIVE 

lcBrFields = [Account :H=LANG_AccountHeader : 16,Pack_Id :H=LANG_PACK_ID:16,cPkColor :H=LANG_ColorHeader :16,;
            lcGmSize=lfGetGmSz(SPck_Hdr.cpcksize):H=LANG_SizeHeader  :16,;
             Desc :H=LANG_DescHeader :30,]
lcBrFields = lcBrFields + [cDivision :R :H=LANG_DivisionHeader :12,Season   :R :H=LANG_SeasonHeader :12 ,]
lcBrFields = lcBrFields + [NPKSLPRICE :R :H=LANG_SELHeader :20 ,;
              STYLEUPC.CUPCNUM1:R :H=LANG_UPCHeader :13,STYLEUPC.CUPCNUM2:R :H='' :10,STYLEUPC.CUPCNUM3:R :H='' :10]


lcAcct     = loFormSet.AriaForm1.KbAccountCode.KeyTextBox.Value
lcPackVal  = loFormSet.AriaForm1.CntPackId.KbPack.KeyTextBox.Value
lcGmaColor = loFormSet.AriaForm1.CntPackId.txtPackClr.Value
lcGmaSize  = loFormSet.AriaForm1.CntPackId.TxtPackSize.Value 
IF !SEEK('P'+lcAcct,loFormSet.lcPckAlias) && if there is no packs for that Account.
  *--There are no records to browse.
  *-- OK
  loFormSet.AriaForm1.CntPackId.llEmptyAcc = .T.
  = gfModalGen("INM42097B42001","Dialog")  
  loFormSet.AriaForm1.CntPackId.KbPack.KeyTextBox.Value = ""  
  lcPackVal = ""
ELSE
  lcKey = "'P'" +' + lcAcct' 
  SELECT (loFormSet.lcPckAlias)
  lcOrder = SET('ORDER')
  SET ORDER TO TAG SPCK_HDRVR
  *--B999999,1 MMT , 03/09/05 Fix bug of cursor position in browse[Start]
  LOCATE 
  *--B999999,1 MMT , 03/09/05 Fix bug of cursor position in browse[End]
  PRIVATE laBrow
  DIMENSION laBrow[10]
  laBrow = ''
  = gfBrows(lcKey,"Account, Pack_ID   , Desc     ,cDivision  ,Season,;
                  LRANGE , NPKSLPRICE, CPKVERSION, CPKCOLOR , CPCKSIZE" ;  
          ,"laBrow",lcBrTitle)
  IF !EMPTY(laBrow)
    lcPackVal  = ALLTRIM(laBrow[2])
    loFormSet.AriaForm1.CntPackId.KbPack.KeyTextBox.Value =lcPackVal
    lcGmaSize  = loFormSet.AriaForm1.CntPackId.mGetSize(laBrow[10])
    lcGmaColor = laBrow[9]
    loFormSet.AriaForm1.CntPackId.txtPackClr.Value=lcGmaColor
    loFormSet.lcGmSizeV = laBrow[10]
    loFormSet.AriaForm1.CntPackId.TxtPackSize.Value =lcGmaSize
    loFormSet.AriaForm1.CboDivision.Value = laBrow[4]
    loFormSet.AriaForm1.CboDivision.requery
    loFormSet.AriaForm1.CboSeason.Value   = laBrow[5]
    loFormSet.AriaForm1.CboSeason.requery
    SET ORDER TO &lcOrder.  
  ELSE
    loFormSet.AriaForm1.CntPackId.KbPack.KeyTextBox.Value = ""   
    lcPackVal = "" 
  ENDIF 
ENDIF

SELECT(lnCurAlias)


*!*************************************************************
*! Name      : lfCRTTMPH        (C038193,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfCRTTMPH()
*!*************************************************************
*!
FUNCTION lfCRTTMPH       

=gfCrtTmp(loFormSet.lcGMAInvH ,@laFileStru,[Account+Order+Store+PikTkt+CDIVISION],loFormSet.lcGMAInvH)

*!**************************************************************************
*! Name      : lfUPDTMPH
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUPDTMPH()
*!**************************************************************************
*! Due to C038193,1
*!**************************************************************************
FUNCTION lfUPDTMPH

lcAlias = ALIAS()

SELECT (loFormSet.lcGMAInvH)

llInvPck = !EMPTY(m.PikTkt) AND SEEK(m.Order+m.Store+m.PikTkt,'Pack_Hdr')    
IF !SEEK(m.Account+m.Order+m.Store+m.PikTkt)
  APPEND BLANK
  REPLACE Order     WITH m.Order ,;
          Store     WITH m.Store ,;
          PikTkt    WITH m.PikTkt ,;
          CustPo    WITH IIF(EMPTY(m.CustPo),OrdHdr.CustPo,m.CustPo),;
          Dist_Ctr  WITH lcDist_Ctr ,;
          Account   WITH OrdHdr.Account ,;
          cTermCode WITH OrdHdr.cTermCode  ,;
          SpcInst   WITH OrdHdr.SpcInst ,;
          lUpsIns   WITH (OrdHdr.CINSUR='Y') ,;
          Rep1      WITH lcSaleRep1 ,;
          Comm1     WITH lnComm1    ,;
          Rep2      WITH lcSaleRep2 ,;
          Comm2     WITH lnComm2    ,;
          Note1     WITH OrdHdr.Note1 ,;
          Note2     WITH OrdHdr.Note2 ,;
          lCompUps  WITH .T. ,;
          Consol    WITH 'N' ,;
          LastLine  WITH OrdHdr.LastLine

  *-- Compute invoice due date based on BOL shipping date  
  IF llInvPck AND !EMPTY(Pack_Hdr.Ship_Date)
    IF loFormSet.llIsEngland
      ldDueDate = IIF(lcTEOM <> 'Y',Pack_Hdr.Ship_Date+lnTDaysDue,;
                      CTOD('01'+SUBSTR(DTOC(GOMONTH(Pack_Hdr.Ship_Date,1)),3))-1+lnTDaysDue)
    ELSE                
      ldDueDate = IIF(lcTEOM <> 'Y',Pack_Hdr.Ship_Date+lnTDaysDue,;
                      GOMONTH(CTOD(SUBSTR(DTOC(Pack_Hdr.Ship_Date),1,3)+'10'+;
                      SUBSTR(DTOC(Pack_Hdr.Ship_Date),6,5)),IIF(DAY(Pack_Hdr.Ship_Date) > lnEOMDay,2,1))+lnTDaysDue)
    ENDIF      
  ENDIF

  REPLACE DiscPcnt   WITH OrdHdr.Disc                                       ,;
          InvDate    WITH loFormSet.DefaultInvoiceDate                      ,;
          ShipDate   WITH IIF(llInvPck AND !EMPTY(Pack_Hdr.Ship_Date)        ;
                          ,Pack_Hdr.Ship_Date,loFormSet.DefaultInvoiceDate) ,;
          dPostDate  WITH loFormSet.DefaultPostingDate                      ,;
          DueDate    WITH ldDueDate                                         ,;
          Dept       WITH OrdHdr.Dept                                       ,; 
          cFacCode   WITH OrdHdr.cFacCode                                   ,;
          Approval   WITH OrdHdr.Approval                                   ,;
          Appramt    WITH OrdHdr.ApprAmt                                    ,;
          Season     WITH OrdHdr.Season                                     ,;
          cDivision  WITH OrdHdr.cDivision                                  ,;
          UpsZone    WITH Customer.UpsZone                                  ,;
          Phone      WITH Customer.Phone1                                   ,;
          cWareCode  WITH IIF(llPicked .AND. SEEK(m.Order+m.PikTkt,'PikTkt'),;
                              PikTkt.cWareCode,OrdHdr.cWareCode)            ,;
          Trde_Disc  WITH lnTerDiscR                                        ,;
          Tax_Rate   WITH IIF(loFormSet.laSetups[9,2]='Y',                  ;
                          IIF(loFormSet.llIsCanada,loFormSet.laSetups[10,2],Customer.nTaxRate),0) ,;
          nPstRate   WITH IIF(loFormSet.laSetups[9,2]='Y' AND loFormSet.llIsCanada,Customer.nTaxRate,0) ,;
          cTaxRule   WITH IIF(loFormSet.laSetups[9,2]='Y' AND loFormSet.llIsCanada,Customer.cTaxRule,'') ,;
          nHstRate   WITH IIF(loFormSet.laSetups[9,2]='Y' AND loFormSet.llIsCanada,loFormSet.laSetups[26,2],0) ,;
          Cod_Flag   WITH IIF(lcTCod='Y','Y','N'),;
          Status     WITH OrdHdr.Status ,;
          cCurrCode  WITH lcCurrCode ,;
          nExRate    WITH lnExRate   ,;
          nCurrUnit  WITH lnCurrUnit ,;
          dAdd_Date  WITH oAriaApplication.SystemDate   ,;
          cAdd_Time  WITH TIME()     ,;
          cAdd_User  WITH oAriaApplication.User_ID
          
ENDIF

lnCartons  = nCartons + IIF(Style.Qty_Ctn>0,IIF(llPicked,m.TotPik,m.TotQty)/Style.Qty_Ctn,0)
lnAllCartn = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
    
REPLACE Ordered   WITH Ordered  + EVALUATE(loFormSet.lcInvLine+'.TotBook') ,;
        Ship      WITH Ship     + IIF(llPicked,m.TotPik,m.TotQty) ,;
        Weight    WITH Weight   + IIF(llInvPck,0,IIF(llPicked,m.TotPik,m.TotQty)*Style.nStyWeight) ,;
        nCartons  WITH IIF(llInvPck,nCartons,lnCartons) ,;
        Cartons   WITH IIF(llInvPck,Cartons,lnAllCartn) ,;
        Picked    WITH Picked  + m.TotPik ,;
        ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia,;
                       IIF(llInvPck,IIF(ALLTRIM(Pack_Hdr.ShipVia)='*',Customer.ShipVia,Pack_Hdr.ShipVia),IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia))),;
        nMerchTax WITH nMerchTax + lnTaxQty * m.Price * lnTaxRate/100 ,;
        Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
        nTaxDue   WITH nTaxDue + IIF(m.lTaxable,IIF(llPicked,m.TotPik,m.TotQty)*m.Price,0)

IF SEEK(m.Order+m.Store,'Pack_Hdr')    
  REPLACE Weight   WITH Pack_Hdr.Tot_Wght ,;
          nCartons WITH Pack_Hdr.Tot_Cart ,;
          Cartons  WITH Pack_Hdr.Tot_Cart ,;
          BOL_No   WITH Pack_Hdr.Bill_ladg
ENDIF
*C127844 HBG 08/25/2005 Update total assist cost field[Begin]
REPLACE TotAssist WITH TotAssist + (nasscost * TotQty)
*C127844 [End]

*B125046,1 HBG 10/28/2004 Update Old Value of Cartons and wight to fix bug of always displaying the;
*B125046,1                message of asking if appling to all stores [Begin]
SELECT (loFormSet.lcGMAInvH)
REPLACE nOldCarton WITH Cartons,;
        nOldWight  WITH Weight    
*B125046,1 [End]
      
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfUPTOTASS
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfUPTOTASS()
*!*************************************************************
*C127844
FUNCTION lfUPTOTASS
REPLACE TotAssist WITH TotAssist + m.TotAssist

*!*************************************************************
*! Name      : lfMDTOTASS
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfMDTOTASS()
*!*************************************************************
*C127844
FUNCTION lfMDTOTASS

REPLACE TotAssist WITH TotAssist - (EVALUATE(loformset.lcinvline+'.nasscost') *lnOldQuantity) + (EVALUATE(loformset.lcinvline+'.nasscost')*lnQuantity) IN (loformset.lcinvhdr)

*!*************************************************************
*! Name      : lfAssCntSr
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfAssCntSr()
*!*************************************************************
*C127844
FUNCTION lfAssCntSr

lcFile = loFormSet.lcInvhdr
WITH loFormSet.AriaForm1.AriaPageFrame1.Page4.cntInvoiceSummary
  IF loFormSet.ActiveMode = 'S'
    .txttotAss.Controlsource   = ''
  ELSE
    .txttotAss.Controlsource   = lcFile+'.TotAssist'
  ENDIF
ENDWITH

*!*************************************************************
*! Name      : lfAddTotAss
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   : =lfAddTotAss()
*!*************************************************************
*C127844 
FUNCTION lfGMASUMM

WITH loFormSet.AriaForm1.AriaPageFrame1.Page1.MultiSelectionGrid
  lcSelected  = .SelectionFile
  llSelectAll = .llSelect 
ENDWITH
STORE 0 TO lnAllInvs, lnAllShip, lnAllAmnt, lnAllCtns, lnAllWght , lnAllAsst
SELECT (loFormSet.lcInvHdr)
lcKey = Account+Order+Store+PikTkt
WITH loFormSet.AriaForm1.Ariapageframe1.page4.cntInvoiceSummary
  *-- Compute summary of selected invoices
  SCAN FOR (Consol='Y' OR EMPTY(Flag)) AND ;
      IIF(SEEK(RECNO(),lcSelected), EVALUATE(lcSelected+'.lSelected'),llSelectAll)
    *-- Compute total charges
    =IIF(lCompUps,IIF(loFormSet.llIsEngland,.mComputeCharges(),.mUpsCharges('A')),.T.)
    lnAllInvs = lnAllInvs + 1
    lnAllShip = lnAllShip + Ship
    lnAllAmnt = lnAllAmnt + TotalChg
    lnAllCtns = lnAllCtns + Cartons
    lnAllWght = lnAllWght + Weight
    lnAllAsst = lnAllAsst + TotAssist
  ENDSCAN
ENDWITH
=SEEK(lcKey,loFormSet.lcInvHdr,loFormSet.lcInvHdr)
lcReturnValue = .T.

DO FORM (oAriaApplication.ScreenHome+"AR\GMAINVS") ;
   WITH lnAllInvs,lnAllShip,lnAllAmnt,lnAllCtns,lnAllWght,lnAllAsst,llGMASave TO lcReturnValue

*!**************************************************************************
*! Name      : lfGetShpAm
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfGetShpAm()
*!**************************************************************************
*! Due to C038193,1
*!**************************************************************************
FUNCTION lfGetShpAm

lcAlias = ALIAS()
SET ORDER TO (loFormSet.lcTmpFle) IN (loFormSet.lcTmpFle)
IF !SEEK(ORDLINE.Account+ORDLINE.Order+ORDLINE.Store+ORDLINE.PikTkt,loFormSet.lcInvHdr)
  loFormSet.lnShipAmnt = 0
ENDIF
loFormSet.lcPack_id = ORDLINE.Pack_id+ORDLINE.cPkColor+ORDLINE.cPckSize+ORDLINE.cPkVersion 
IF !EMPTY(ORDLINE.Pack_id+ORDLINE.cPkColor+ORDLINE.cPckSize+ORDLINE.cPkVersion)
  = SEEK('P'+ORDLINE.cPckAcc+ORDLINE.Pack_id+ORDLINE.cPkColor+ORDLINE.cPckSize+;
         ORDLINE.cPkVersion,'SPCK_HDR') 
  IF SEEK(ORDLINE.cPckAcc+ORDLINE.Pack_id+ORDLINE.cPkColor+ORDLINE.cPckSize+;
          ORDLINE.cPkVersion,loFormSet.lcTmpFle)
    IF ORDLINE.nPkSlPrice = 0 OR ORDLINE.lpckprpiec OR ORDLINE.lrange
      *B130472,1 HBG 11/21/2005 Fix bug of wrong ship ampunt if order have a Piktkt [Begin]
      *loFormSet.lnShipAmnt = loFormSet.lnShipAmnt + (ORDLINE.TotQty * ORDLINE.Price) 
      loFormSet.lnShipAmnt = loFormSet.lnShipAmnt + (IIF(llPicked,ORDLINE.TotPik,ORDLINE.TotQty) * ORDLINE.Price) 
      *B130472,1 [End]
    ENDIF
    RETURN
  ELSE
    INSERT INTO (loFormSet.lcTmpFle) (cPack_id) VALUE (ORDLINE.cPckAcc+ORDLINE.Pack_id+ORDLINE.cPkColor+;
                                             ORDLINE.cPckSize+ORDLINE.cPkVersion)
  ENDIF 
  IF ORDLINE.nPkSlPrice = 0 OR ORDLINE.lpckprpiec OR ORDLINE.lrange
    *B130472,1 HBG 11/21/2005 Fix bug of wrong ship ampunt if order have a Piktkt [Begin]
    *loFormSet.lnShipAmnt = loFormSet.lnShipAmnt + (ORDLINE.TotQty * ORDLINE.Price) 
    loFormSet.lnShipAmnt = loFormSet.lnShipAmnt + (IIF(llPicked,ORDLINE.TotPik,ORDLINE.TotQty)* ORDLINE.Price) 
    *B130472,1 [End]
  ELSE
    *B130472,1 HBG 11/21/2005 Fix bug of wrong ship ampunt if order have a Piktkt [Begin]
    *loFormSet.lnShipAmnt = loFormSet.lnShipAmnt + (ORDLINE.nPackNo * ORDLINE.nPkSlPrice)
    lnPackNo = IIF(ORDLINE.TotPik <> 0,(ORDLINE.TotPik / ORDLINE.TotQty) * ORDLINE.nPackNo,ORDLINE.nPackNo)
    loFormSet.lnShipAmnt = loFormSet.lnShipAmnt + (lnPackNo * ORDLINE.nPkSlPrice)
    *B130472,1 [End]
  ENDIF
ELSE  
  loFormSet.lnShipAmnt = loFormSet.lnShipAmnt + (IIF(llPicked,ORDLINE.TotPik,ORDLINE.TotQty) * ORDLINE.Price) 
ENDIF  
SELECT (lcAlias )
*!**************************************************************************
*! Name      : lfGetShpAm
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfGetShpAm()
*!**************************************************************************
*! Due to C038193,1
*!**************************************************************************
FUNCTION lfGtShpAms

PRIVATE lcOrdline 
lcOrdline = loFormSet.lcOrdLines

lcAlias = ALIAS()
SET ORDER TO (loFormSet.lcTmpFle) IN (loFormSet.lcTmpFle)

loFormSet.lcPack_id = Spck_Lin.Pack_id+Spck_Lin.cPkColor+Spck_Lin.cPckSize+Spck_Lin.cPkVersion 
IF !EMPTY(loFormSet.lcPack_id)
  IF SEEK(loFormSet.lcPack_id ,loFormSet.lcTmpFle)
    IF &lcPackHdr..nPkSlPrice = 0 OR &lcPackHdr..lpckprpiec OR &lcPackHdr..lrange
      loFormSet.lnShipAmnt = loFormSet.lnShipAmnt + ((Spck_Lin.TotQty*&lcPackHdr..PackQty)* &lcOrdline..Price) 
    ENDIF
    RETURN
  ELSE
    INSERT INTO (loFormSet.lcTmpFle) (cPack_id) VALUE (loFormSet.lcPack_id)
  ENDIF 
  IF &lcPackHdr..nPkSlPrice = 0 OR &lcPackHdr..lpckprpiec OR &lcPackHdr..lrange
    loFormSet.lnShipAmnt = loFormSet.lnShipAmnt + ((Spck_Lin.TotQty*&lcPackHdr..PackQty) * &lcOrdline..Price) 
  ELSE
    loFormSet.lnShipAmnt = loFormSet.lnShipAmnt + (&lcPackHdr..PackQty * &lcPackHdr..nPkSlPrice)
  ENDIF
ELSE  
  loFormSet.lnShipAmnt = loFormSet.lnShipAmnt + ((Spck_Lin.TotQty*&lcPackHdr..PackQty) * &lcOrdline..Price) 

ENDIF  
SELECT (lcAlias )

*!**************************************************************************
*! Name      : lfUpdPkHdr
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdPkHdr()
*!**************************************************************************
*! Due to C038193,1
*!**************************************************************************
FUNCTION lfUpdPkHdr

REPLACE BOOKAMT WITH BookAmt - lnAmount,;
        OPENAMT WITH OPenAmt - lnAmount
        
REPLACE BOOKAMT WITH BookAmt + loFormSet.lnShipAmnt,;
        OPENAMT WITH OPenAmt + loFormSet.lnShipAmnt
        
*!**************************************************************************
*! Name      : lfUpInvHdr
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpInvHdr()
*!**************************************************************************
*! Due to C038193,1
*!**************************************************************************
FUNCTION lfUpInvHdr

REPLACE SHIPAMT  WITH ShipAmt - lnAmount,;
        APPRAMT  WITH APPRAMT - lnAmount
        
REPLACE SHIPAMT  WITH ShipAmt + loFormSet.lnShipAmnt,;
        Discount WITH -ROUND(ShipAmt*DiscPcnt/100,2),;
        APPRAMT  WITH APPRAMT + loFormSet.lnShipAmnt
          
*!**************************************************************************
*! Name      : lfUpdShip
*! Developer : Hend Ghanem (HBG)
*! Date      : 08/22/2004
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdShip()
*!**************************************************************************
*! Due to C038193
*!**************************************************************************
FUNCTION lfUpdShip

lcCurAlias = ALIAS()
SELECT (loFormSet.lcGMAInvH)
REPLACE ShipAmt   WITH loFormSet.lnShipAmnt,;
        Discount  WITH -ShipAmt * DiscPcnt/100,;
        Cod_Amt   WITH IIF(Cod_Flag='Y' AND loFormSet.llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,; 
        TotalChg  WITH IIF(loFormSet.llIsEngland,ShipAmt+Tax_Amt+Discount,0)
SELECT (lcCurAlias)               


*!**************************************************************************
*! Name      : lfUpdHdr
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdHdr()
*!**************************************************************************
*! Due to C038193,1
*!**************************************************************************
FUNCTION lfUpdHdr

lcAlias = ALIAS()
SELECT (loFormSet.lcInvHdr)
DELETE ALL
PACK
SELECT (loFormSet.lcGMAInvH)
LOCATE
SCAN 
  SCATTER MEMVAR MEMO
  INSERT INTO (loFormSet.lcInvHdr) FROM MEMVAR
ENDSCAN
SELECT (lcAlias)

*!**************************************************************************
*! Name      : lfDisaPck
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfDisaPck()
*!**************************************************************************
*! Due to C038193,1
*!**************************************************************************
FUNCTION lfDisaPck

loFormSet.ARiaForm1.Ariapageframe1.page3.invoiceeditregion1.keyPack.keytextbox.Enabled = .F.
loFormSet.ARiaForm1.Ariapageframe1.page3.invoiceeditregion1.keyPack.keyCmd.Enabled = .F.


*!**************************************************************************
*! Name      : lfOpenFile
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfOpenFile()
*!**************************************************************************
*! Due to C102574,1 , C038193,1
*!**************************************************************************
FUNCTION lfOpenFile

=gfOpenFile(oAriaApplication.DataDir+'SPck_Hdr','SPCK_HDRVR','SH')
=gfOpenFile(oAriaApplication.DataDir+'SPCK_LIN','SPCK_LINVR','SH')

*!*************************************************************
*! Name      : lfSETSVAR1          (C038193,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSETSVAR1()
*!*************************************************************
*!
FUNCTION lfSETSVAR1                

lcAlias = ALIAS()
loFormSet.lnShipAmnt = 0
loFormSet.lnPackQty  = 0
loFormSet.lcPack_id  = &lcAlias..Pack_id + &lcAlias..cPkColor + &lcAlias..cPckSize + &lcAlias..cPkVersion 

SELECT (loFormSet.lcTmpFle)
ZAP
SELECT (lcAlias)

*!**************************************************************************
*! Name      : lfUpdSInv
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : =lfUpdSInv()
*!**************************************************************************
*! Due to C038193,1
*!**************************************************************************
FUNCTION lfUpdSInv

*B130472,1 HBG 12/12/2005 Fix bug of wrong ship ampunt if order have a Piktkt [Begin]
*!*	REPLACE Pack_id    WITH m.Pack_id,;
*!*	        cPkColor   WITH m.cPkColor,;
*!*	        cPckSize   WITH m.cPckSize,;
*!*	        nPkSlPrice WITH m.nPkSlPrice,;
*!*	        cPkVersion WITH m.cPkVersion,;        
*!*	        nPackNo    WITH m.nPackNo,;
*!*	        lRange     WITH m.lRange,;
*!*	        lPckPrPiec WITH m.lPckPrPiec,;
*!*	        lShip      WITH .T. IN (lcDetFile)       


*B608477,1 WAM 03/10/2008 Update the invoiec lines with pack information 
*=gfOpenFile(oAriaApplication.DataDir+'SPck_Hdr',oAriaApplication.DataDir+'SPCK_HDRVR','SH') 
*=SEEK('P'+m.Account+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion,'SPCK_HDR') OR;
 SEEK('P*****'+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion,'SPCK_HDR') 
*lnPackNo = IIF(m.TotPik <> 0,m.TotPik /SPCK_HDR.nPckQty,m.nPackNo)

=gfOpenFile(oAriaApplication.DataDir+'SPCK_LIN',oAriaApplication.DataDir+'SPCK_LINVR','SH') 
=SEEK('P'+m.Account+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion+m.Style,'SPCK_LIN') OR;
 SEEK('P*****'+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion+m.Style,'SPCK_LIN') 
lnPackNo = IIF(SPCK_LIN.TotQty <> 0,m.TotQty /SPCK_LIN.TotQty, m.nPackNo)
*B608477,1 WAM 03/10/2008 (End)

REPLACE Pack_id    WITH m.Pack_id,;
        cPkColor   WITH m.cPkColor,;
        cPckSize   WITH m.cPckSize,;
        nPkSlPrice WITH m.nPkSlPrice,;
        cPkVersion WITH m.cPkVersion,;        
        nPackNo    WITH lnPackNo,;
        lRange     WITH m.lRange,;
        lPckPrPiec WITH m.lPckPrPiec,;
        lShip      WITH .T. IN (lcDetFile)       
*B130472,1 [End]

*C128356,1 HBG 08/23/2005 Update L\C fields in invoice lines[begin]
REPLACE cloc_no   WITH m.cloc_no  ,;
        clocbank  WITH m.clocbank ,;
        dlocissue WITH m.dlocissue,;
        nasscost  WITH m.nasscost IN (lcDetFile) 
*C128356,1 [End]    

*!*************************************************************
*! Name      : lfChkPkPrc
*! Developer : Hend Ghanem
*! Date      : 15/09/2004
*! Purpose   : Compare price againset pack price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfChkPkPrc()
*!*************************************************************
*!
FUNCTION lfChkPkPrc

lcPack_Id = EVALUATE(oEditRegion.Detailfile+'.Pack_ID')+EVALUATE(oEditRegion.Detailfile+'.cPkColor')+;
            EVALUATE(oEditRegion.Detailfile+'.cPckSize')+EVALUATE(oEditRegion.Detailfile+'.cPkVersion')
WITH oEditRegion 
  IF !EMPTY(lcPack_Id)
    IF TYPE('.spnDiscount.OldValue') <> 'N'
      .spnDiscount.OldValue = 0
    ENDIF
    IF TYPE('.txtGrossPrice.OldValue') <> 'N'
      .txtGrossPrice.OldValue = 0
    ENDIF
    lnDisc_Pcnt  = .spnDiscount.Value
    lnOldDiscPct = .spnDiscount.OldValue
    lnGros_Price = .txtGrossPrice.Value
    lnOldGrosPrc = .txtGrossPrice.OldValue
    lnOldPrice   = ROUND(lnOldGrosPrc*(100-lnOldDiscPct)/100,2)
    lnPrice      = ROUND(lnGros_Price*(100-lnDisc_Pcnt)/100,2) 
    IF lnPrice <> lnOldPrice
      IF gfModalGen('TRM00000B32000','DIALOG','','',LANG_GMA_UpdPrice) = 2
        .txtGrossPrice.Value = lnOldGrosPrc
        .spnDiscount.Value   = lnOldDiscPct
        RETURN .F. 
      ELSE
        .txtGrossPrice.OldValue = .txtGrossPrice.Value  
        .spnDiscount.OldValue   = .spnDiscount.Value
      ENDIF    
    ENDIF  
  ENDIF  
ENDWITH  
RETURN

*!*************************************************************
*! Name      : lfSetContS
*! Developer : Hend Ghanem
*! Date      : 15/09/2004
*! Purpose   : Set control source
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSetContS()
*!*************************************************************
*!124447
FUNCTION lfSetContS

oEditRegion = loFormSet.AriaForm1.Ariapageframe1.Page2.AriaEditRegion1
oEditRegion.txtCustPo.ControlSource = oEditRegion.Detailfile+'.CustPo'


*!*************************************************************
*! Name      : lfSetContS
*! Developer : Hend Ghanem
*! Date      : 15/09/2004
*! Purpose   : Set control source
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSetContS()
*!*************************************************************
*!124447
FUNCTION lfSoCustPO

lcOrderFile = IIF(INLIST(loFormSet.ActiveMode,'S','V'),'OrdLine',loFormSet.oformenvironment.lcordline)
loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1.Columns(4).ControlSource = lcOrderFile+'.CustPo'


*!*************************************************************
*! Name      : lfUpCustPo  
*! Developer : Hend Ghanem
*! Date      : 15/09/2004
*! Purpose   : Set control source
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfUpCustPo()
*!*************************************************************
*!124447
FUNCTION lfUpCustPo  

IF EMPTY(loFormSet.FormHasToolBar)
  oEditRegion = loFormSet.AriaForm1.AriaEditRegion1
ELSE
  oEditRegion = loFormSet.AriaForm1.Ariapageframe1.Page2.AriaEditRegion1
ENDIF
*-- Message : 32046
*-- Change Custpo# for all lines in store xxxxx.
*-- Button : 32000
*-- Yes;No

SELECT (oEditRegion.DetailFile)
*B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
*IF (ORDHDR.Multi = "Y" OR !EMPTY(ORDHDR.Store)) AND gfModalGen('QRM32046B32000','ALERT',lcStrCode)=1
lcORDHDR = oEditRegion.HeaderFile
IF (&lcORDHDR..Multi = "Y" OR !EMPTY(&lcORDHDR..Store)) AND gfModalGen('QRM32046B32000','ALERT',lcStrCode)=1
*B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header

  lnCurrTag = VAL(SYS(21))
  lcCurrExp = EVAL(SYS(14,lnCurrTag))
  SET ORDER TO TAG ORDLINST
  *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
  *=SEEK(OrdHdr.cOrdType+OrdHdr.Order+lcStrCode)
  *SCAN REST WHILE cOrdType+ORDER+STORE = OrdHdr.cOrdType+OrdHdr.Order+lcStrCode
  =SEEK(&lcORDHDR..cOrdType+&lcORDHDR..Order+lcStrCode)
  SCAN REST WHILE cOrdType+ORDER+STORE = &lcORDHDR..cOrdType+&lcORDHDR..Order+lcStrCode
  *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header

    REPLACE CustPo WITH lcCstPoCde ,;
            FLAG   WITH IIF(FLAG <> 'N','M',FLAG)
  ENDSCAN
  SET ORDER TO TAG lnCurrTag
  =SEEK(lcCurrExp)
ELSE
  REPLACE CustPo WITH lcCstPoCde ,;
          FLAG   WITH IIF(FLAG <> 'N','M',FLAG)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfCustPoEd
*! Developer : Hend Ghanem
*! Date      : 15/09/2004
*! Purpose   : Set control source
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSetContS()
*!*************************************************************
*!124447
FUNCTION lfCustPoEd

lcOrdLine = loFormSet.oformenvironment.lcOrdLine
loFormSet.AriaForm1.Ariapageframe1.Page2.grdEditLines.CustPo.ControlSource = lcOrdLine+'.CustPo'


*!**************************************************************************
*! Name      : lfALBrwOrd
*! Developer : Hend Ghanem
*! Date      : 18/09/2004
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfALBrwOrd()
*!**************************************************************************
*! Due to C038556,1
*!**************************************************************************
FUNCTION lfALBrwOrd

WITH loFormSet.Ariaform1.grdOrdDtaile
  IF TYPE('.PackVersion') = 'U'
    .ADDCOLUMN(3)
    .Columns(.ColumnCount).Name = 'PackVersion'
  ENDIF  
  IF loFormSet.activemode = 'S'
    .PackVersion.ControlSource = ""
  ELSE
    .PackVersion.ControlSource = "PACK_ID+'-'+CPKCOLOR+'-'+lfGetGmSz(cPckSize)+'-'+CPKVERSION"
  ENDIF
  .PackVersion.WIDTH = 200
  .PackVersion.Header1.Caption = LANG_GMA_PackTitle
ENDWITH

*!*************************************************************
*! Name      : lfvUdCrtWgt (C200484,1)
*! Developer : ALBERT RAIF (ALB)
*! Date      : 01/13/2003
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvUdCrtWgt()
*!*************************************************************
*!*B124548,1
FUNCTION lfvUdCrtWgt

lcCurrAlis = ALIAS()
LOCAL lnCurCrtns , lnCurWght  
lnCurCrtns = EVALUATE(loFormSet.lcInvhdr+'.Cartons')
lnCurWght  = EVALUATE(loFormSet.lcInvhdr+'.Weight')

IF gfModalGen('QRM00000B40000','DIALOG',"","",LANG_GMA_CopyToStores) = 1
  SELECT (loFormSet.lcInvhdr)
  lcKey = EVALUATE(KEY())
  SCAN FOR Consol <> 'Y'
    REPLACE Cartons WITH lnCurCrtns,;
            Weight  WITH lnCurWght
    *B125046,1 HBG 10/28/2004 Fix bug of always displaying the meassge of Carton/Weight [Begin]
    REPLACE nOldCarton WITH lnCurCrtns,;
            nOldWight  WITH lnCurWght
    *B125046,1 [End]
     loFormSet.AriaForm1.ariapageframe1.Page4.cntInvoicesummary.mUpsCharges('A')
  ENDSCAN   
  =SEEK(lcKey) 
ELSE
  REPLACE Cartons WITH lnCurCrtns,;
          Weight  WITH lnCurWght
  REPLACE nOldCarton WITH lnCurCrtns,;
          nOldWight  WITH lnCurWght
  loFormSet.AriaForm1.ariapageframe1.Page4.cntInvoicesummary.mUpsCharges('A')
ENDIF
SELECT (lcCurrAlis)

*!*************************************************************
*! Name      : lfShpStrLn
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/19/2004
*! Purpose   : Ship/UnShip Invoice Line
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfShpStrLn()
*!*************************************************************
*!B124549,1
FUNCTION lfShpStrLn
PRIVATE lnTaxQty,lcInvKey

lcInvLine  = loFormSet.AriaForm1.ariapageframe1.page3.invoiceeditregion1.detailfile 
lcInvHdr  = loFormSet.AriaForm1.ariapageframe1.page3.invoiceeditregion1.headerfile 
SELECT (lcInvLine)
lcInvKey=Account+Order+Store+PikTkt+STR(LineNo,6)
SET KEY TO
IF SEEK('O'+&lcInvLine..Order,'ORDHDR') AND ORDHDR.Multi = 'Y' 
  SELECT (lcInvHdr)
  llConsDist = SEEK('M'+&lcInvHdr..Account,'Customer') AND Customer.ConByDc ='Y'
  IF llConsDist 
    lcKey = &lcInvHdr..Account+&lcInvHdr..Dist_ctr+&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt
  ELSE
    lcKey = &lcInvHdr..Account+&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt  
  ENDIF  
  lnStores = 0
  SCAN FOR Order = &lcInvLine..Order AND Consol = 'N'
    lnStores = lnStores + 1
  ENDSCAN
  =SEEK(lcKey,lcInvHdr)
  IF lnStores > 1  
    IF gfModalGen('QRM00000B32000','ALERT','','',LANG_GMA_MessageShip+IIF(!(&lcInvLine..TotQty <> 0),LANG_GMA_Ship,LANG_GMA_UnShip)+LANG_GMA_Style+;
      &lcInvLine..Style+LANG_GMA_ForAllStores) = 1
      llShip = (&lcInvLine..TotQty <> 0)
      SELECT (lcInvLine)
      lcStyle   = &lcInvLine..Style
      lcDyelot  = &lcInvLine..Dyelot
      lcPack_id = &lcInvLine..Pack_id+&lcInvLine..cPkColor+&lcInvLine..cPckSize+&lcInvLine..cPkVersion
      lcPrvOrd = ORDER()
      SET ORDER TO InvLines
      llAllPack = .F.
      IF !EMPTY(&lcInvLine..Pack_ID)
        llAllPack = gfModalGen('QRM00000B32000','ALERT','','',LANG_GMA_StyleInPack+&lcInvLine..Pack_id+". "+;
                     IIF(!llShip ,LANG_GMA_Ship,LANG_GMA_UnShip)+LANG_GMA_TheWholePack) = 1
        IF !llAllPack 
          RETURN             
        ENDIF  
      ENDIF             
      SELECT (lcInvLine)
      lcStore = " "
      IF llAllPack
        lcExpr = 'Account+Order+PikTkt+Pack_id+cPkColor+cPckSize+cPkVersion =&lcInvHdr..Account+&lcInvHdr..Order+&lcInvHdr..PikTkt+lcPack_id;
                    AND IIF(llShip,TotQty <> 0,TotQty = 0)'
      ELSE
        lcExpr = 'Account+Order+PikTkt+Style+Dyelot+Pack_id+cPkColor+cPckSize+cPkVersion = &lcInvHdr..Account+&lcInvHdr..Order+&lcInvHdr..PikTkt+lcStyle+lcDyelot+lcPack_id;
                    AND IIF(llShip,TotQty <> 0,TotQty = 0)'                      
      ENDIF      
      loFormSet.llDspCanMsg = .T.      
      loFormSet.llBackOrd   = .T.      
      SCAN FOR &lcExpr AND Consol = 'N'
        lnRecNo = RECNO()
        IF Store <> lcStore 
          lcStore  = Store
          SELECT (lcInvHdr)
          IF llConsDist 
            =SEEK(&lcInvLine..Account+&lcInvLine..Dist_ctr+&lcInvLine..Order+&lcInvLine..Store+&lcInvLine..PikTkt)
          ELSE
            =SEEK(&lcInvLine..Account+&lcInvLine..Order+&lcInvLine..Store+&lcInvLine..PikTkt)
          ENDIF  
          SCATTER MEMVAR MEMO
        ENDIF  
        loFormSet.AriaForm1.ariapageframe1.page3.invoiceeditregion1.mshipline(&lcInvLine..TotQty = 0) 
        SET KEY TO
        REPLACE lShip WITH (TotQty <> 0)
        loFormSet.llDspCanMsg = .F.
      ENDSCAN
      SELECT (lcInvLine)
      SET ORDER TO &lcPrvOrd 
      SELECT (lcInvHdr)
      =SEEK(lcKey,lcInvHdr)
      IF !EOF()
       SKIP
       SKIP -1 
      ENDIF
    ELSE
      =lfShipLine()
      REPLACE lShip WITH (TotQty <> 0)
    ENDIF  
  ELSE
    =lfShipLine()
    REPLACE lShip WITH (TotQty <> 0)
  ENDIF  
ELSE
  =lfShipLine()
  REPLACE lShip WITH (TotQty <> 0)
ENDIF
SELECT (lcInvLine)
SET KEY TO
IF &lcInvHdr..Consol = 'Y'
  SET ORDER TO TAG ConsDiv
  SET KEY TO &lcInvHdr..Account + &lcInvHdr..Order + &lcInvHdr..Store + &lcInvHdr..PikTkt+;
             &lcInvHdr..consol+&lcInvHdr..cDivision+&lcInvHdr..Dist_Ctr
ELSE
  SET KEY TO &lcInvHdr..Account + &lcInvHdr..Order + &lcInvHdr..Store + &lcInvHdr..PikTkt
ENDIF
LOCATE
loFormSet.llDspCanMsg = .T.
loFormSet.llBackOrd   = .T. 
loFormSet.AriaForm1.ariapageframe1.page3.Ariagrid1.Refresh 
loFormSet.AriaForm1.ariapageframe1.page3.ariagrid1.AfterRowColChange 


*!*************************************************************
*! Name      : lfShipLine
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/19/2004
*! Purpose   : Ship/UnShip Invoice Line
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfShpStrLn()
*!*************************************************************
*!B124549,1
FUNCTION lfShipLine

llShip = (&lcInvLine..TotQty <> 0)
IF !EMPTY(&lcInvLine..Pack_ID) AND gfModalGen('QRM00000B32000','ALERT','','',LANG_GMA_StyleInPack+&lcInvLine..Pack_id+". "+;
                                                  IIF(!llShip,LANG_GMA_Ship,LANG_GMA_UnShip)+LANG_GMA_TheWholePack) = 1
  SELECT (lcInvLine)
  lcPack_id = &lcInvLine..Pack_id+&lcInvLine..cPkColor+&lcInvLine..cPckSize+&lcInvLine..cPkVersion
  loFormSet.llDspCanMsg = .T.
  loFormSet.llBackOrd   = .T.      
  SCAN FOR Account+Order+PikTkt+Pack_id+cPkColor+cPckSize+cPkVersion =;
           &lcInvHdr..Account+&lcInvHdr..Order+&lcInvHdr..PikTkt+lcPack_id;
            AND IIF(llShip,TotQty <> 0,TotQty = 0) AND Consol = 'N' AND store = &lcInvHdr..store
    loFormSet.AriaForm1.ariapageframe1.page3.invoiceeditregion1.mshipline(&lcInvLine..TotQty = 0) 
    loFormSet.llDspCanMsg = .F.
  ENDSCAN 
ELSE
  IF EMPTY(&lcInvLine..Pack_ID)
    loFormSet.AriaForm1.ariapageframe1.page3.invoiceeditregion1.mshipline(&lcInvLine..TotQty = 0) 
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfCanclMsg
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/19/2004
*! Purpose   : Display cancel msg only once
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfCanclMsg()
*!*************************************************************
*!B124549,1
FUNCTION lfCanclMsg

IF loFormSet.llDspCanMsg AND (lcChkBkOrd = "N" OR (lcChkBkOrd="I" AND (!&lcInvLine..lBackOrd OR gfModalGen('QRM40154B40000','ALERT')=1)))
  loFormSet.llBackOrd = .F.    
ENDIF 
m.lBackOrd = loFormSet.llBackOrd


*!*************************************************************
*! Name      : lfCanclMsg
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/19/2004
*! Purpose   : Display cancel msg only once
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfCanclMsg()
*!*************************************************************
*!C038193,1
FUNCTION lfUpdCnsPk

loFormSet.lcNewPackId    = m.Pack_id
loFormSet.lcNewPkColor   = m.cPkColor
loFormSet.lcNewPkSize    = m.cPckSize
loFormSet.lcNewPkVersion = m.cPkVersion

*!*************************************************************
*! Name      : lfCanclMsg
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/19/2004
*! Purpose   : Display cancel msg only once
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfCanclMsg()
*!*************************************************************
*!C038193,1
FUNCTION lfUpPkInfo

REPLACE Pack_ID    WITH loFormSet.lcNewPackId ,;
        cPkColor   WITH loFormSet.lcNewPkColor,;
        cPckSize   WITH loFormSet.lcNewPkSize ,;
        cPkVersion WITH loFormSet.lcNewPkVersion
        
*!*************************************************************
*! Name      : lfSeekPck
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/19/2004
*! Purpose   : Display cancel msg only once
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfCanclMsg()
*!*************************************************************
*!C038193,1
FUNCTION lfSeekPck

lcSeekExpr = 'Y'+lcAccount+lcDivision+lcCurrCode+lcStyleID+loFormSet.lcNewPackId+loFormSet.lcNewPkColor+;
             loFormSet.lcNewPkSize+loFormSet.lcNewPkVersion+lcDcCode
lcSeekIndx = 'PkConsol'


*!*************************************************************
*! Name      : lfSetFlag
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/19/2004
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSetFlag()
*!*************************************************************
*!C124447,1
FUNCTION lfSetFlag

*B128682,1 HBG 06/28/2005 Fix bug of not updating location in detail [Begin]
*!*  llModifyAllLines = cWareCode <> OrdHdr.cWareCode OR ;
*!*                    (OrdHDr.Multi <> 'Y' AND Store <> OrdHDr.Store) OR ;
*!*                    (!llLineComission AND (Comm1 <> ORdHDr.Comm1 OR Comm2 <> OrdHDr.Comm2)) OR ;
*!*                    (!llCompleteDate  AND Complete <> OrdHdr.Complete)

*B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
*llModifyAllLines = cWareCode <> lcWareCode OR ;
                  (OrdHDr.Multi <> 'Y' AND Store <> OrdHDr.Store) OR ;
                  (!llLineComission AND (Comm1 <> ORdHDr.Comm1 OR Comm2 <> OrdHDr.Comm2)) OR ;
                  (!llCompleteDate  AND Complete <> OrdHdr.Complete)
llModifyAllLines = cWareCode <> lcWareCode OR ;
                  (&lcOrdHDr..Multi <> 'Y' AND Store <> &lcOrdHDr..Store) OR ;
                  (!llLineComission AND (Comm1 <> &lcOrdHDr..Comm1 OR Comm2 <> &lcOrdHDr..Comm2)) OR ;
                  (!llCompleteDate  AND Complete <> &lcOrdHDr..Complete)
*B608434,1 WAM 02/14/2008 (End)
*B128682,1 [End]              

*!*************************************************************
*! Name      : lfSavCstPO
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/19/2004
*! Purpose   : Update Cust PO field
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSavCstPO()
*!*************************************************************
*!C124447,1
FUNCTION lfSavCstPO

*B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
*REPLACE cWareCode WITH OrdHdr.cWareCode ,;
        Store     WITH IIF(OrdHdr.Multi="Y",Store   ,OrdHdr.Store) ,;
        CustPo    WITH IIF(!OrdHdr.MultiPo AND EMPTY(CustPo), OrdHdr.CustPo , CustPo) ,;
        Complete  WITH IIF(llCompleteDate  ,Complete,OrdHdr.Complete) ,;
        Comm1     WITH IIF(llLineComission ,Comm1   ,OrdHDr.Comm1) ,;
        Comm2     WITH IIF(llLineComission ,Comm2   ,OrdHDr.Comm2)
REPLACE cWareCode WITH &lcOrdHdr..cWareCode ,;
        Store     WITH IIF(&lcOrdHdr..Multi="Y",Store   ,&lcOrdHdr..Store) ,;
        CustPo    WITH IIF(!&lcOrdHdr..MultiPo AND EMPTY(CustPo), &lcOrdHdr..CustPo , CustPo) ,;
        Complete  WITH IIF(llCompleteDate  ,Complete,&lcOrdHdr..Complete) ,;
        Comm1     WITH IIF(llLineComission ,Comm1   ,&lcOrdHdr..Comm1) ,;
        Comm2     WITH IIF(llLineComission ,Comm2   ,&lcOrdHdr..Comm2)
*B608434,1 WAM 02/14/2008 (End)

*!*************************************************************
*! Name      : lfPoToSnd()
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 10/20/2004
*! Purpose   : Add Record to PoToSend .
*! Passed Parameters  : ....
*!*************************************************************
FUNCTION lfPoToSnd

LOCAL lcMastPoHdr, llMultiWareH, lnAlias
lnAlias      = SELECT()
llMultiWareH = loFormSet.Ariaform1.MainWorkOrder.lMultiWarehous
lcMastPoHdr  = loFormSet.Ariaform1.MainWorkOrder.cPosHdr
 
IF SEEK(EVALUATE(lcMastPoHdr+'.Vendor') ,'APVENDOR') AND ;
  IIF(llMultiWareH,.T.,SEEK(EVALUATE(lcMastPoHdr+'.cWareCode'),'WAREHOUS'))

  IF APVENDOR.LUSECARGO AND IIF(llMultiWareH,.T.,WAREHOUS.LUSECARGO)
    =gfOpenFile(oAriaApplication.DataDir+'POTOSEND','POTOSEND','SH')
    SELECT POTOSEND
    IF !SEEK(EVALUATE(lcMastPoHdr+'.cStyType')+EVALUATE(lcMastPoHdr+'.PO'),'POTOSEND')
      APPEND BLANK
      REPLACE CSTYTYPE WITH EVALUATE(lcMastPoHdr+'.cStyType'),;
              PO       WITH EVALUATE(lcMastPoHdr+'.PO')
    ENDIF
    IF loFormSet.ActiveMode = "A"
      REPLACE TYPE WITH "A"
    ELSE
      IF EVALUATE(lcMastPoHdr+'.Status') = 'X'
        REPLACE TYPE WITH "D"
      ELSE
        REPLACE TYPE WITH "C"
      ENDIF
    ENDIF
    REPLACE CSTATUS  WITH "N"
  ENDIF
ENDIF
SELECT(lnAlias)
*-- end of lfPoToSnd.

*!*************************************************************
*! Name      : lfDisaFOOR
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 10/21/2004
*! Purpose   : Adding the following function in order to disable the
*!             FOB and Origin fields.
*!*************************************************************
*!E038631,1 KHM 10/21/2004
*:*************************************************************
FUNCTION lfDisaFOOR

*-- If Style Purchase Order and its a new mode or edit mode.
IF loFormSet.cBusdocumnt+loFormSet.cWorkOrderType = 'PP' AND loFormSet.ActiveMode $ 'AE'
  STORE .F. To loFormSet.ariaForm1.pgfPOStyle.page1.cntheaderFolder.txtFOB.Enabled ,;
               loFormSet.ariaForm1.pgfPOStyle.page1.cntheaderFolder.txtorigin.Enabled  
ENDIF

*!*************************************************************
*! Name      : lfvFob
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 10/21/2004
*! Purpose   : Valid function of Fob Code. When the user selets
*!          an FOB code the description will be displayed
*!         in the FOB code in the screen     
*!*************************************************************
*!E038631,1 KHM 10/21/2004
*:*************************************************************
FUNCTION lfvFob
PARAMETERS lcRetVal

PRIVATE lnFobPos, lcFob
lcRetVal = .T.
IF TYPE('laUserFields') = "U"
  RETURN lcRetVal
ENDIF
lnFobPos  = ASUBSCRIPT(laUserFields,ASCAN(laUserFields,'CFOBCODE'),1)
lcFob     = laOgFxFlt[lnFobPos,6]

*AMH Fix bug of Fob code not saved [Start]
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*loFormSet = oariaapplication.otoolbar.owindparent
*HBG [End]
loFormSet = oariaapplication.otoolbar.owindparent
*AMH [End]

*B128324,1 WSH 06/02/2005 Don't refresh the FOB Code field on the screen until Option Grid is closed [Start]
*loFormSet.ariaForm1.pgfPOStyle.page1.cntheaderFolder.txtFOB.Value = ;
           IIF(EMPTY(lcFob),'',gfCodDes(lcFob,'CFOBCODE'))
*B128324,1 WSH 06/02/2005 [End]

RETURN lcRetVal
*-- end of lfvFob.

*!*************************************************************
*! Name      : lfvOrigin
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 10/21/2004
*! Purpose   : Valid function of Origin Code. When the user selets
*!          an Origin code the description will be displayed
*!         in the Origin code in the screen     
*!*************************************************************
*!E038631,1 KHM 10/21/2004
*:*************************************************************
FUNCTION lfvOrigin
PARAMETERS lcRetVal

PRIVATE lnOrgnPos, lcOrigin
lcRetVal = .T.
IF TYPE('laUserFields') = "U"
  RETURN lcRetVal
ENDIF
lnOrgnPos = ASUBSCRIPT(laUserFields,ASCAN(laUserFields,'CORIGINCOD'),1)
lcOrigin  = laOgFxFlt[lnOrgnPos,6]

*AMH Fix bug of Origin code not saved [Start]
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*loFormSet = oariaapplication.otoolbar.owindparent
*HBG [End]
loFormSet = oariaapplication.otoolbar.owindparent
*AMH [End]

*B128324,1 WSH 06/02/2005 Don't refresh the Origin Code field on the screen until Option Grid is closed [Start]
*loFormSet.ariaForm1.pgfPOStyle.page1.cntheaderFolder.txtOrigin.Value = ;
           IIF(EMPTY(lcOrigin),'',gfCodDes(lcOrigin,'CORIGINCOD'))
*B128324,1 WSH 06/02/2005 [End]

RETURN lcRetVal
*-- end of lfvOrigin.

*!*************************************************************
*! Name      : lfwGmaCode
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 10/21/2004
*! Purpose   : When function of Fob & Origin Codes.     
*!*************************************************************
*!E038631,1 KHM 10/21/2004
*:*************************************************************
FUNCTION lfwGmaCode
PARAMETERS lcRetVal

*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*loFormSet = oariaapplication.otoolbar.owindparent
*HBG [End]
IF ASCAN(loFormSet.laEvntTrig , PADR('WGMACODE',10)) <> 0
  lcRetVal = .T.
ELSE
  lcRetVal = .F.
ENDIF
RETURN lcRetVal
*-- end of lfwGmaCode.

*!*************************************************************
*! Name      : lfADDPKFLD
*! Developer : Hend Ghanem
*! Date      : 11/21/2004
*! Purpose   : Add fields of pack   
*!*************************************************************
*!E038463,1 HBG 11/21/2004
*:*************************************************************
FUNCTION lfADDPKFLD

laFileStru[1,1] = 'Pack_ID'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 30
laFileStru[1,4] = 0

laFileStru[2,1] = 'Pack_Desc'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 35
laFileStru[2,4] = 0

*!*************************************************************
*! Name      : lfPackCode
*! Developer : Hend Ghanem
*! Date      : 11/21/2004
*! Purpose   : Set the pack key
*!*************************************************************
*!E038463,1 HBG 11/21/2004
*:*************************************************************
FUNCTION lfPackCode

lcPack_ID = m.Pack_id+m.cpkColor+m.cpckSize+m.cpkversion
lcPackUpc = m.Pack_id+m.cpkColor+m.cpckSize

*!*************************************************************
*! Name      : lfPackInfo
*! Developer : Hend Ghanem
*! Date      : 11/21/2004
*! Purpose   : Set the pack key and open SPCK_HDR with the correct key
*!*************************************************************
*!E038463,1 HBG 11/21/2004
*:*************************************************************
FUNCTION lfPackInfo

m.Pack_Desc  = &lcTmpOrdLn..Pack_id+'-'+&lcTmpOrdLn..cpkColor+'-'+lfGetGmSz(&lcTmpOrdLn..cPckSize)+'-'+&lcTmpOrdLn..cpkversion
m.Pack_ID    = &lcTmpOrdLn..Pack_id+&lcTmpOrdLn..cpkColor+&lcTmpOrdLn..cpckSize+&lcTmpOrdLn..cpkversion
m.NPKSLPRICE = &lcSPCK_HDR..NPKSLPRICE
IF lfopenFox(loFormSet,'SPCK_HDR',lcSPCK_HDR,"type+account+pack_id+cpkcolor+cpcksize+cpkversion = 'P"+lcAccount+m.Pack_ID+"' OR "+;
                                 "type+account+pack_id+cpkcolor+cpcksize+cpkversion = 'P*****"+m.Pack_ID+"'") ;
                                 AND !EOF(lcSPCK_HDR)
  llRange = EVALUATE(lcSPCK_HDR+'.lRange')
ENDIF                                  

*!*************************************************************
*! Name      : lfPackCde2
*! Developer : Hend Ghanem
*! Date      : 11/21/2004
*! Purpose   : Set the pack key and open SPCK_HDR with the correct key
*!*************************************************************
*!E038463,1 HBG 11/21/2004
*:*************************************************************
FUNCTION lfOpenFxFl

=lfopenFox(loFormSet,'SPCK_HDR',lcSPCK_HDR,"type+account+pack_id+cpkcolor+cpcksize+cpkversion = 'P"+lcAccount+lcPack_ID+"' OR "+;
                                 "type+account+pack_id+cpkcolor+cpcksize+cpkversion = 'P*****"+lcPack_ID+"'") 
=lfopenFox(loFormSet,'STYLEUPC',lcStyleUPC,"account+style+cpkcolor+cpksize = '"+lcAccount+lcPackUpc+"'")


*!*************************************************************
*! Name      : lfSeekExpr
*! Developer : Hend Ghanem
*! Date      : 11/21/2004
*! Purpose   : Seek using the correct pack key
*!*************************************************************
*!E038463,1 HBG 11/21/2004
*:*************************************************************
FUNCTION lfSeekExpr

lcSeek = "SEEK(Pack_id+cpkColor+cpckSize+cPkversion,lcPackFile)"

*!*************************************************************
*! Name      : lfAdjGrid
*! Developer : Hend Ghanem
*! Date      : 11/21/2004
*! Purpose   : Set the width of the pack field
*!*************************************************************
*!E038463,1 HBG 11/21/2004
*:*************************************************************
FUNCTION lfAdjGrid

loFormSet.AriaForm1.grdOrdLines.Pack_Id.Width = 213 

*!**************************************************************************
*! Name      : lfUPDDEPFL
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 01/16/2005
*! Purpose   : To assing new styles or delete the existing one according
*!             to the entered style's group.
*!**************************************************************************
*! Example   : lfUPDDEPFL()
*!**************************************************************************
*! C038945,1 WSH 01/16/2004
*!**************************************************************************
FUNCTION lfUPDDEPFL

LOCAL lnAlias, lnRecNo, lcColorFil, lnBuffering, lnConn
lnAlias    = SELECT()
lcColorFil = loFormSet.lcColorFil

SELECT Style
=SEEK(loFormSet.lcStylekey)

*--Open the Department Detail file remotely
lnConn = oAriaApplication.RemoteCompanyData.SQLRun("SELECT * FROM ICDeptDt WHERE STYLE+DEPT = '" + loFormSet.lcStylekey + "'",;
              'ICDeptDt', '', oAriaApplication.cAriaNativeDataFilesConStr, 3, 'SAVE', SET("Datasession"))

IF lnConn = 1
  *--If Query Successfully executed, Create Indexes if needed for the result cursor
  lnBuffering = CURSORGETPROP('Buffering', 'ICDeptDt')
  SELECT ICDeptDt
  =CURSORSETPROP("Buffering", 3)
  INDEX ON STYLE+DEPT TAG DEPTDTS
  =CURSORSETPROP("Buffering", lnBuffering)
  SET ORDER TO DEPTDTS
ELSE
  *--Query Execution Error
  =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConn, .T.)
  RETURN .F.
ENDIF

*--Update Department Detail file for all Style Colors
IF loFormSet.llAllColors && If in All Color Mode
  SELECT (lcColorFil)
  LOCATE
  SCAN
    SELECT ICDeptDt
    IF !SEEK(&lcColorFil..Style)
      APPEND BLANK
      REPLACE Style WITH &lcColorFil..Style
    ENDIF
    REPLACE Dept      WITH &lcColorFil..Dept;
            cStyGroup WITH &lcColorFil..cStyGroup
    =gfAdd_info('ICDeptDt', loFormSet)
  ENDSCAN
ELSE
  SELECT ICDeptDt
  IF !SEEK(Style.Style)
    APPEND BLANK
    REPLACE Style WITH Style.Style
  ENDIF
  REPLACE Dept      WITH Style.Dept;
          cStyGroup WITH Style.cStyGroup
  =gfAdd_info('ICDeptDt', loFormSet)
ENDIF

*--Update Table
*-- Begin Updating Transaction
LOCAL lcTranCode
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.cAriaNativeDataFilesConStr, 3, '', .T.)

*-- Check Resule for Begin Transaction
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran", lcTranCode, .T.)
  RETURN .F.
ENDIF

*-- Start update the Table with the deleted records
lnConn = oAriaApplication.RemoteCompanyData.SQLUpdate('ICDeptDt', lcTranCode, SET("Datasession"), "STYLE", "ICDeptDt", "DEPTDTS")

*-- Check Result for Update Process
IF lnConn <> 1 AND lnConn <> 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUpdate", lnConn, .T.)
  lnConn = oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode, .T.)
  IF lnConn <> 1
    =oAriaApplication.RemoteCompanyData.CheckRetResult("RollBackTran", lnConn, .T.)
  ENDIF
  RETURN .F.
ENDIF

*-- Commit Changes and Check Result
lnConn = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode, .T.)
IF lnConn <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran", lnConn, .T.)
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  Return .F.
ENDIF

USE IN ICDeptDt

SELECT(lnAlias)
RETURN .T.

*!**************************************************************************
*! Name      : lfDELDPSTY
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 01/16/2005
*! Purpose   : To delete the style form IcDeptDt when delete the style 
*!             from the file.
*!**************************************************************************
*! Example   : lfDELDPSty()
*!**************************************************************************
*! C038945,1 WSH 01/16/2004
*!**************************************************************************
FUNCTION lfDELDPSTY

LOCAL lnAlias, lnConn
lnAlias    = SELECT()

*--Open the Department Detail file remotely
lnConn = oAriaApplication.RemoteCompanyData.SQLRun("SELECT * FROM ICDeptDt WHERE STYLE+DEPT = '" + loFormSet.lcStylekey + "'",;
              'ICDeptDt', '', oAriaApplication.cAriaNativeDataFilesConStr, 3, 'SAVE', SET("Datasession"))

IF lnConn = 1
  *--If Query Successfully executed, Create Indexes if needed for the result cursor
  lnBuffering = CURSORGETPROP('Buffering', 'ICDeptDt')
  SELECT ICDeptDt
  =CURSORSETPROP("Buffering", 3)
  INDEX ON STYLE+DEPT TAG DEPTDTS
  =CURSORSETPROP("Buffering", lnBuffering)
  SET ORDER TO DEPTDTS
ELSE
  *--Query Execution Error
  =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnConn, .T.)
  RETURN .F.
ENDIF

LOCAL lnDelStys, lcColor, lcStyle, lnI
lnDelStys = IIF(loFormSet.llAllColors, ALEN(laTarget), 1)

FOR lnI = 1 TO lnDelStys
  lcColor = IIF(loFormSet.llAllcolors , laTarget(lnI), SUBSTR(loFormSet.lcStylekey, loFormSet.lnStylewid + 2, loFormSet.lncolorwid))
  lcStyle = loFormSet.lcStyMajor + loFormSet.lcSepart + lcColor
  
  IF SEEK(lcStyle, 'IcDeptDt')
    SELECT IcDeptDt
    DELETE
  ENDIF
ENDFOR

*--Update Table
*-- Begin Updating Transaction
LOCAL lcTranCode
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.cAriaNativeDataFilesConStr, 3, '', .T.)

*-- Check Resule for Begin Transaction
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran", lcTranCode, .T.)
  RETURN .F.
ENDIF

*-- Start update the Table with the deleted records
lnConn = oAriaApplication.RemoteCompanyData.SQLUpdate('ICDeptDt', lcTranCode, SET("Datasession"), "STYLE", "ICDeptDt", "DEPTDTS")

*-- Check Result for Update Process
IF lnConn <> 1 AND lnConn <> 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUpdate", lnConn, .T.)
  lnConn = oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode, .T.)
  IF lnConn <> 1
    =oAriaApplication.RemoteCompanyData.CheckRetResult("RollBackTran", lnConn, .T.)
  ENDIF
  RETURN .F.
ENDIF

*-- Commit Changes and Check Result
lnConn = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode, .T.)
IF lnConn <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran", lnConn, .T.)
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  Return .F.
ENDIF

USE IN IcDeptDt
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfChkPacks
*! Developer : Hend Ghanem (HBG)
*! Date      : 03/23/2004
*! Purpose   : Check if there are packs in this sales order
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Due to C037922,1
*!*************************************************************
*! Example   : =lfChkPacks()
*!*************************************************************
FUNCTION lfChkPacks

LOCAL lcPrvOrd , lcKeyexpr 
lcPrvAls = ALIAS()

lcPrvOrd = ORDER('SPCK_HDR')
SET ORDER TO SPCK_HDRVr IN SPCK_HDR
SELECT (oEditRegion.Detailfile)
lcCurOrder = ORDER()
lcKeyExpr = EVALUATE(KEY())
IF !loFormSet.llFromSO
  lcInvHdr = oEditRegion.headerfile
ENDIF  
lnRecNo = RECNO()
IF loFormSet.llFromSO
  SET ORDER TO ORDLINE

  *B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
  *=SEEK(OrdHdr.cOrdType+OrdHdr.Order)
  *COUNT FOR cOrdType+ORDER+STR(LINENO,6) = ORDHDR.cOrdType+ORDHDR.Order AND !EMPTY(Pack_ID) TO lnNtPacks
  lcOrdHdr = oEditRegion.HeaderFile 
  =SEEK(&lcOrdHdr..cOrdType+&lcOrdHdr..Order)
  COUNT FOR cOrdType+ORDER+STR(LINENO,6) = &lcOrdHdr..cOrdType+&lcOrdHdr..Order AND !EMPTY(Pack_ID) TO lnNtPacks
  *B608434,1 WAM 02/14/2008 (End)

ELSE
  IF loFormSet.llFromARS
    COUNT FOR Account+Order+Store = &lcInvHdr..Account + &lcInvHdr..Order + &lcInvHdr..Store AND !EMPTY(Pack_ID) TO lnNtPacks
  ELSE
    SET ORDER TO InvLines
    =SEEK(&lcInvHdr..Account + &lcInvHdr..Order + &lcInvHdr..Store)
    COUNT FOR Account+Order+Store+PikTkt+Style+Dyelot = &lcInvHdr..Account + &lcInvHdr..Order + &lcInvHdr..Store AND !EMPTY(Pack_ID) TO lnNtPacks
  ENDIF  
ENDIF  
SET ORDER TO (lcCurOrder)
loFormSet.llNtPacks = (lnNtPacks = 0 )

IF TYPE('loFormSet.NextOptionBar') = 'N'
  *-- Refresh the grid according to the selection in option grid and if there are packs or not
  IF loFormSet.llNtPacks
    loFormSet.llShowPack = .F.
    IF loFormSet.llFromSO   && case of sales order screen
      *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
      IF TYPE('_INQURYPOP') = 'O'
      *B129117,1 [End]
        SET MARK OF BAR loFormSet.NextOptionBar+1 OF _INQURYPOP TO .F.
        DO CASE 
          CASE loFormSet.lnSortBy = 1
            SET MARK OF BAR 7 OF _INQURYPOP TO .T.
          CASE loFormSet.lnSortBy = 2
            SET MARK OF BAR 8 OF _INQURYPOP TO .T.    
          CASE loFormSet.lnSortBy = 3
            SET MARK OF BAR 9 OF _INQURYPOP TO .T.        
        ENDCASE  
        SET MARK OF BAR loFormSet.NextOptionBar+2 OF _INQURYPOP TO .F.
      *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
      ENDIF
      *B129117,1 [End]
        
      IF !loFormSet.llDetails
        loFormSet.llZoom = .T.
      ENDIF  
      loFormSet.llDetails = .T.
      *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
      IF TYPE('_INQURYPOP') = 'O'
      *B129117,1 [End]      
        SET MARK OF BAR 11 OF _INQURYPOP TO !loFormSet.llDetails
      *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
      ENDIF
      *B129117,1 [End]        
      loFormSet.mGetBrowse
    ELSE
      *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
      IF TYPE('_INQURYPOP') = 'O'
      *B129117,1 [End]    
        SET MARK OF BAR loFormSet.NextOptionBar OF _INQURYPOP TO .F.    
      *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
      ENDIF
      *B129117,1 [End]    
      IF loFormSet.llFromARS  && case of Direct invoice screen
        *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
        IF TYPE('_INQURYPOP') = 'O'
        *B129117,1 [End]    
          SET MARK OF BAR loFormSet.NextOptionBar+1 OF _INQURYPOP TO .F. 
        *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
        ENDIF
        *B129117,1 [End]    
        IF loFormSet.llSummaryByPack 
          loFormSet.llZoom = .T.
        ENDIF  
        loFormSet.llSummaryByPack = .F.  
        =lfRefGrid(loFormSet)
      ENDIF  
    ENDIF
  ELSE
    IF loFormSet.llZoom
      IF loFormSet.llFromSO  && case of sales order screen
        IF loFormSet.llShowPack
          loFormSet.llDetails = .F.
          *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
          IF TYPE('_INQURYPOP') = 'O'
          *B129117,1 [End]    
            SET MARK OF BAR 11 OF _INQURYPOP TO !loFormSet.llDetails
          *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
          ENDIF 
          *B129117,1 [End]    
            
        ENDIF  
        loFormSet.mGetBrowse
      ELSE
        IF loFormSet.llFromARS  && case of Direct invoice screen
          IF loFormSet.llShowPack
            loFormSet.llSummaryByPack = .T. 
            *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
            IF TYPE('_INQURYPOP') = 'O'
            *B129117,1 [End]    
              SET MARK OF BAR loFormSet.NextOptionBar+1 OF _INQURYPOP TO .T.
            *B129117,1 HBG 07/27/2005 fix bug of popup have been not defined [Begin]
            ENDIF
            *B129117,1 [End]    
          ENDIF   
          =lfRefGrid(loFormSet)
        ENDIF
      ENDIF
    ENDIF
  ENDIF  
ENDIF  
SELECT (oEditRegion.Detailfile)
=SEEK(lcKeyExpr)
IF BETWEEN(lnRecNo, 1, RECCOUNT())
  GO lnRecNo
ENDIF
SET ORDER TO (lcPrvOrd) IN SPCK_HDR
SELECT (lcPrvAls)              


*!**************************************************************************
*! Name      : lfRefGrid
*! Developer : Wael Ali Mohamed
*! Date      : 04/10/2004
*! Purpose   : Runn custom bars for Direct Invoice
*! Reference : 
*!**************************************************************************
*! Example   :  =lfCSMDIBAR()
*!**************************************************************************
FUNCTION lfRefGrid
LPARAMETERS loFormSet

IF loFormSet.llShowPack
  SET MARK OF BAR loFormSet.NextOptionBar OF _INQURYPOP TO .T.
  WITH loFormSet.AriaForm1.Ariapageframe1.page2.Ariagrid1
    SELECT (.RecordSource) 
    .ADDCOLUMN(3)
    .Columns(.ColumnCount).Name = 'PackVersion'
    .Columns(.ColumnCount).ControlSource = "PACK_ID+'-'+CPKCOLOR+'-'+lfGetGmSz(cPckSize)+'-'+CPKVERSION"
    .Columns(.ColumnCount).WIDTH = 200
    .Columns(.ColumnCount).Header1.Caption = LANG_GMA_PackTitle
  ENDWITH
ELSE
  loFormSet.llSummaryByPack = .F.
  SET MARK OF BAR loFormSet.NextOptionBar  OF _INQURYPOP TO .F.
  SET MARK OF BAR loFormSet.NextOptionBar+1 OF _INQURYPOP TO .F.
  loFormSet.mInvoiceLines
ENDIF
IF loFormSet.llSummaryByPack
  SET MARK OF BAR loFormSet.NextOptionBar+1 OF _INQURYPOP TO .T.
  lcInvoice = EVALUATE(IIF(loFormSet.Activemode = 'A',loFormSet.lcInvHdr,'InvHdr')+'.Invoice')
  IF !USED(loFormSet.lcPackSum) .OR. !SEEK(lcInvoice,loFormSet.lcPackSum)
    lcFile = IIF(loFormSet.Activemode='A',loFormSet.lcInvLine,'InvLine')
    SELECT Invoice,&lcFile..PACK_ID,&lcFile..CPKCOLOR,&lcFile..cPckSize,&lcFile..CPKVERSION,&lcFile..nPkSlPrice,Spck_hdr.Desc,nPackNo;
            FROM (lcFile),Spck_hdr ;
            WHERE Spck_hdr.pack_id+Spck_hdr.cpkcolor+Spck_hdr.cpcksize+Spck_hdr.cpkversion = ;
             &lcFile..pack_id+&lcFile..cpkcolor+&lcFile..cpcksize+&lcFile..cpkversion AND &lcFile..Invoice = INVHDR.Invoice;
             GROUP BY Invoice,&lcFile..PACK_ID,&lcFile..CPKCOLOR,&lcFile..cPckSize,&lcFile..CPKVERSION,Spck_hdr.Desc,&lcFile..nPkSlPrice,nPackNo ;
           INTO CURSOR (loFormSet.lcPackSum)
    INDEX ON Invoice+PACK_ID+CPKCOLOR+cPckSize+CPKVERSION TAG (loFormSet.lcPackSum)
  ENDIF
  lcOrderFile = loFormSet.lcPackSum
  WITH loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1
    .ColumnCount=0
    .RecordSource = lcOrderFile
     SELECT (lcOrderFile)
     .columncount = 5
    .Columns(1).ControlSource = lcOrderFile+".PACK_ID+'-'+"+lcOrderFile+".CPKCOLOR+'-'+lfGetGmSz("+lcOrderFile+".cPckSize)+'-'+"+lcOrderFile+'.CPKVERSION'
    .Columns(1).WIDTH = 200
    .Columns(1).Header1.Caption = LANG_GMA_PackTitle
    .Columns(2).ControlSource = 'Desc'
    .Columns(2).WIDTH = 120
    .Columns(2).Header1.Caption = lang_LabelStyleDesc
    .Columns(3).ControlSource = lcOrderFile+'.nPackNo'
    .Columns(3).WIDTH = 60
    .Columns(3).Header1.Caption = LANG_GMA_PackQty
    .Columns(4).ControlSource = lcOrderFile+'.nPkSlPrice'
    .Columns(4).WIDTH = 70
    .Columns(4).Header1.Caption = LANG_GMA_PackPrice
    .Columns(5).ControlSource = lcOrderFile+'.nPkSlPrice*'+lcOrderFile+'.nPackNo'
    .Columns(5).WIDTH = 60
    .Columns(5).Header1.Caption = lang_LabelAmount
    .SetAll('ReadOnly',.T.,'COLUMN')
    .Refresh
  ENDWITH
  loFormSet.llZoom = .T.
  loFormSet.mZoomBrowse(loFormSet.llZoom)
  SET MARK OF BAR 11 OF _INQURYPOP TO .T.
ELSE
  SET MARK OF BAR loFormSet.NextOptionBar+1 OF _INQURYPOP TO .F.
  loFormSet.mInvoiceLines
  IF loFormSet.llShowPack
    WITH loFormSet.AriaForm1.Ariapageframe1.page2.Ariagrid1
      SELECT (.RecordSource) 
      .ADDCOLUMN(3)
      .Columns(.ColumnCount).Name = 'PackVersion'
      .Columns(.ColumnCount).ControlSource = "PACK_ID+'-'+CPKCOLOR+'-'+lfGetGmSz(cPckSize)+'-'+CPKVERSION"
      .Columns(.ColumnCount).WIDTH = 200
      .Columns(.ColumnCount).Header1.Caption = LANG_GMA_PackTitle
    ENDWITH
  ENDIF  
ENDIF  

*!**************************************************************************
*! Name      : lfClearPck
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 02/06/2005
*! Purpose   : Clear Pack Code if Account Code Changed in SKU Screen
*! Reference : B999999,1
*!**************************************************************************
*! Example   :  =lfClearPck()
*!**************************************************************************
FUNCTION lfClearPck

WITH loFormSet.AriaForm1
  IF .kbAccountCode.SelectedFromBrowse OR EMPTY(.kbAccountCode.KeyTextBox.Value) OR .kbAccountCode.KeyTextBox.Value <> .kbAccountCode.KeyTextBox.OldValue
    .CntPackId.kbPack.KeyTextBox.Value = ''
    .CntPackId.kbPack.KeyTextBox.Valid()
  ENDIF
ENDWITH

*!*************************************************************
*! Name      : lfClcWipGma
*! Developer : AMH - AHMED MAHER
*! Date      : 04/06/2005
*! Purpose   : caluclate the WIP depen on the budjet qty only 
*!*************************************************************
FUNCTION lfClcWipGma

RETURN


*:**************************************************************************
*:* Name        : lfStyInit                                   *C126727,1 
*:* Developer   : Hend Ghanem
*:* Date        : 
*:* Purpose     : Inisialize method of Style screen
*:***************************************************************************
*:* Called from : icstyle.scx
*:***************************************************************************
FUNCTION lfStyInit

loFormSet.AddProperty('lcCstPkInf',gfTempName())
loFormSet.AddProperty('lcTempCur',gfTempName())
loFormSet.AddProperty('lcTmpName',gfTempName())
loFormSet.AddProperty('lnQtyCrt',0)
loFormSet.AddProperty('lnWghtUnt',0)
loFormSet.AddProperty('lnWidth',0)
loFormSet.AddProperty('lnDepth',0)
loFormSet.AddProperty('lnLnght',0)
loFormSet.AddProperty('lcSlctCust',"")

*:**************************************************************************
*:* Name        : lfAddCstPk                                      *C126727,1 
*:* Developer   : Hend Ghanem
*:* Date        : 
*:* Purpose     : Open the screen "Customer Packing info" 
*:***************************************************************************
*:* Called from : icstyle.scx
*:***************************************************************************
FUNCTION lfCstPkInf
PRIVATE lnAlias,lcTempCur,lcStyPkTtl,lnStyPk,lcSlctd,lcDesc,lcSlctCust,lcCusts;
        laFlds,lcOldValue,lcTmpName,laBrow,llBrowse,;
        lcUpdStat
        
PRIVATE lcSvVrFle,lcCstPkInf

lcCstPkInf = loformSet.lcCstPkInf 
lcTempCur  = loformset.lcTempCur  
lcTmpName  = loformset.lcTmpName  
lnAlias    = SELECT()
=lfOpnFls()      &&Open needed files
=lfCrTmpFls(lcCstPkInf)    &&Create needed temp files

lcSlctd    = loFormSet.ariaForm1.kbstyleMajor.keytextbox.Value
lcNonMjr   = loFormSet.ariaForm1.kbNonMajor.keytextbox.Value
lcDesc     = loFormSet.ariaForm1.txtshortDesc.Value
loFormSet.lcSlctCust = '' 
lnMAjLen   = LEN(gfItemMask('PM', '', '0001'))

IF '*' $ lcNonMjr 
  SET ORDER TO CSTYLE IN (loFormSet.lcTmpName)
  =SEEK(PADR(lcSlctd,lnMAjLen),loFormSet.lcTmpName)
ELSE
  SET ORDER TO STYLE IN (loFormSet.lcTmpName)
  lnNMAjLen = LEN(gfItemMask('PN', '', '0001'))
  lcSepart  = SUBSTR(gfItemMask('PI', '', '0001'),lnMAjLen+1,1)
  lcStyle   = PADR(lcSlctd,lnMAjLen)+lcSepart+PADR(lcNonMjr,lnNMAjLen)
  =SEEK(lcStyle,loFormSet.lcTmpName)
ENDIF

loFormSet.lnQtyCrt   = EVALUATE(loFormSet.lcTmpName+'.QTY_CTN')
loFormSet.lnWghtUnt  = EVALUATE(loFormSet.lcTmpName+'.NSTYWEIGHT')
loFormSet.lnWidth    = EVALUATE(loFormSet.lcTmpName+'.NMPCUBE')
loFormSet.lnDepth    = EVALUATE(loFormSet.lcTmpName+'.NMPDEPTH')
loFormSet.lnLnght    = EVALUATE(loFormSet.lcTmpName+'.NMPDIM')
llUpdStat  = IIF(loFormSet.ActiveMode = 'V',.F.,.T.)

SELECT CUSTOMER
GO TOP
*-- Opens "Customer Packing Info" screen
DO FORM (oAriaApplication.ScreenHome+'IC\ICPKINF') WITH loformset,lcSlctd,lcNonMjr,lcDesc,llUpdStat,lcTempCur

USE IN (loFormSet.lcTmpName)

SELECT (lnAlias)
*-- end of lfCstPkInf.

*:**************************************************************************
*:* Name        : lfOpnFls                                      *C126727,1
*:* Developer   : Hend Ghanem
*:* Purpose     : Open needed files
*:***************************************************************************
FUNCTION lfOpnFls
IF !USED('SPCK_HDR')
  =gfOpenFile(oAriaApplication.DataDir+'SPCK_HDR','SPCK_HDRVR','SH')  
ENDIF
IF !USED('SPCK_LIN')
  =gfOpenFile(oAriaApplication.DataDir+'SPCK_LIN','SPCK_LINVR','SH')  
ENDIF
IF !USED('CUSTOMER')
  =gfOpenFile(oAriaApplication.DataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF  
IF !USED('CSTPKINF')
  =gfOpenFile(oAriaApplication.DataDir+'CSTPKINF','CSTPKINF','SH')
ENDIF  
IF USED(loFormSet.lcTmpName)
  USE IN (loFormSet.lcTmpName)
ENDIF
USE (oAriaApplication.DataDir+'STYLE') ORDER 'STYLE' AGAIN ALIAS (loFormSet.lcTmpName) IN 0
SET ORDER TO CSTYLE IN (loFormSet.lcTmpName)
*-- end of lfOpnFls.

*:**************************************************************************
*:* Name        : lfCrTmpFls                                      *C126727,1
*:* Developer   : Hend Ghanem
*:* Date        : 04/08/2003
*:* Purpose     : Create needed temp files
*:**************************************************************************
FUNCTION lfCrTmpFls
LPARAMETERS lcCstPkInf
PRIVATE laFlds,lnI

IF !USED(lcCstPkInf)
  SELECT CSTPKINF
  =AFIELDS(laFlds)
  lnI = ALEN(laFlds,1)+1
  DIMENSION laFlds[lnI,18]
  laFlds[lnI,1] = 'STATUS'
  laFlds[lnI,2] = 'C'
  laFlds[lnI,3] = 1
  laFlds[lnI,4] = 0
  FOR lnI =1 TO ALEN(laFlds,1)
    STORE '' TO laFlds[lnI,7],laFlds[lnI,8],laFlds[lnI,9],;
                laFlds[lnI,10],laFlds[lnI,11],laFlds[lnI,12],;
                laFlds[lnI,13],laFlds[lnI,14],laFlds[lnI,15],;
                laFlds[lnI,16]
    STORE 0  TO laFlds[lnI,17],  laFlds[lnI,18]
  ENDFOR
  CREATE DBF (oAriaApplication.WorkDir+lcCstPkInf) FROM ARRAY laFlds
  INDEX ON TYPE+ACCOUNT+STYLE TAG (lcCstPkInf)
  INDEX ON TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION TAG (lcCstPkInf+'P')
ENDIF
*-- end of lfCrTmpFls.

*:**************************************************************************
*:* Name        : lfEXPCTOLD                                      *C126727,1
*:* Developer   : Hend Ghanem
*:* Date        : 04/08/2003
*:* Purpose     : Save data to CSTPKINF file
*:***************************************************************************
*:* Called from : ICSTYLE.lpSavScr
*:***************************************************************************
FUNCTION lfEXPCTOLD
PRIVATE lcCstPkInf,lcSeekKey

lcCstPkInf = loformSet.lcCstPkInf 

IF USED(lcCstPkInf)
  SELECT &lcCstPkInf
  SCAN
    IF &lcCstPkInf..TYPE = 'S'
      SET ORDER TO 'CSTPKINF' IN CSTPKINF
      lcSeekKey = TYPE+ACCOUNT+STYLE
    ELSE
      SET ORDER TO 'CSTPKINFP' IN CSTPKINF
      lcSeekKey = TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION
    ENDIF
    IF STATUS = 'U'
      SCATTER TO laMemVar
      IF !SEEK(lcSeekKey,'CSTPKINF')
        INSERT INTO CSTPKINF FROM ARRAY laMemVar
      ELSE
        SELECT CSTPKINF
        GATHER FROM laMemVar
      ENDIF
    ENDIF
  ENDSCAN  
  USE IN (lcCstPkInf)
  ERASE (oAriaApplication.WorkDir+lcCstPkInf+'.DBF')
  ERASE (oAriaApplication.WorkDir+lcCstPkInf+'.CDX')
ENDIF
*-- end of lfEXPCTOLD.

*khalid
*!**************************************************************************
*! Name      : lfGETVAR
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 05/03/2005
*! Purpose   : Get fields from style file and update POSLN file
*!**************************************************************************
*! Example   : lfGETVAR()
*!**************************************************************************
*! B039291,1 KHM 05/03/2005 Added
*!**************************************************************************
FUNCTION lfGETVAR

REPLACE Ninpack   WITH STYLE.Ninpack;
        Nmpack    WITH STYLE.Nmpack;
        Nmpdim    WITH STYLE.Nmpdim;
        NmPDepth  WITH STYLE.NmPDepth;
        NmPcube   WITH STYLE.NmPcube;
        NmpWeight WITH STYLE.NmpWeight,;
        cHTSNo    WITH STYLE.cHTSNo   

m.Ninpack          = STYLE.Ninpack
m.Nmpack           = STYLE.Nmpack
m.Nmpdim           = STYLE.Nmpdim
m.NmPDepth         = STYLE.NmPDepth
m.NmPcube          = STYLE.NmPcube
m.NmpWeight        = STYLE.NmpWeight
m.lnMPcube         = (m.Nmpdim*m.NmPDepth*m.NmPcube)/1728
m.cHTSNo           =STYLE.cHTSNo

*!**************************************************************************
*! Name      : lfCHANGWHS
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/12/2005
*! Purpose   : Enable Warehouse code if it is allocated to PO or CT.
*!**************************************************************************
*! Example   : lfGETVAR()
*!**************************************************************************
*! C036929,1 
*!**************************************************************************
FUNCTION lfCHANGWHS

llEditWarehouse = (OrdHdr.Ship = 0)
*-- Copy order lines to temporary file
SELECT ORDLINE
=SEEK(ordHdr.cOrdType+OrdHdr.Order)
SCAN REST WHILE cordtype+order+STR(lineno,6) = ordHdr.cOrdType+OrdHdr.Order
  *-- Warehouse code cannot be changed if order has been picked
  llEditWarehouse = IIF(llEditWarehouse, ORDLINE.TotPik = 0, llEditWarehouse)
ENDSCAN

*!**************************************************************************
*! Name      : lfInvRebal
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/13/2005
*! Purpose   : Rebalance "Balance to Pay" & "Shipment Amount" fields in the 
*!             Invoice Header and Consalidated Invoice Header files...
*!**************************************************************************
*! Example   : = lfInvRebal()
*!**************************************************************************
*! Due to C102574,1 , C102575,1 , 127969
*!**************************************************************************
FUNCTION lfInvRebal

LOCAL lnAlias
lnAlias = SELECT(0)

SELECT (lcRebalHdr)
SET ORDER TO 'Itemhdr'
IF SEEK('LCRPITEM14', lcRebalHdr) AND EVALUATE(lcRebalHdr + '.cUpdVryIgn') $ 'UV'
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *PRIVATE loInvHdr, lcInvHdr, loInvLine, lcInvLine, loCONSINVH, lcCONSINVH, loCONSINVL, lcCONSINVL
  *PRIVATE loSPck_Lin, lcSPCK_Lin, loOrdLine, lcOrdLine, loPikTkt, lcPikTkt, loPikLine, lcPikLine
  PRIVATE lcInvHdr, lcInvLine, lcCONSINVH, lcCONSINVL
  PRIVATE lcSPCK_Lin, lcOrdLine, lcPikTkt, lcPikLine
  
  LOCAL lcSeekCond
  DIMENSION laFileStr[1,9]
  laFileStr = .F.
  *B128464,2 WSH 08/21/2005 [End]
  
  lcInvHdr   = gfTempName()
  lcInvLine  = gfTempName()
  lcCONSINVH = gfTempName()
  lcCONSINVL = gfTempName()
  lcSPCK_Lin = gfTempName()
  lcOrdLine  = gfTempName()
  lcPikTkt   = gfTempName()
  lcPikLine  = gfTempName()
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *loInvHdr   = CREATEOBJECT("RemoteTable", "INVHDR", "INVHDR", lcInvHdr, SET("Datasession"), lcCurrComp_ID, .T.)
  *loInvLine  = CREATEOBJECT("RemoteTable", "INVLINE", "INVLINE", lcInvLine, SET("Datasession"), lcCurrComp_ID, .T.)
  *loCONSINVH = CREATEOBJECT("RemoteTable", "CONSINVH", "CONSINVH", lcCONSINVH, SET("Datasession"), lcCurrComp_ID, .T.)
  *loCONSINVL = CREATEOBJECT("RemoteTable", "CONSINVL", "CONSINVL", lcCONSINVL, SET("Datasession"), lcCurrComp_ID, .T.)
  *loSPck_Lin = CREATEOBJECT("RemoteTable", "SPCK_LIN", "SPCK_lINVR", lcSPCK_Lin, SET("Datasession"), lcCurrComp_ID, .T.)
  *loOrdLine  = CREATEOBJECT("RemoteTable", "OrdLine", "ORDLINE", lcOrdLine, SET("Datasession"), lcCurrComp_ID, .T.)
  *loPikTkt   = CREATEOBJECT("RemoteTable", "PikTkt", "PIKTKT", lcPikTkt, SET("Datasession"), lcCurrComp_ID, .T.)
  *loPikLine  = CREATEOBJECT("RemoteTable", "PikLine", "PIKLINE", lcPikLine, SET("Datasession"), lcCurrComp_ID, .T.)
  = lfFilesStr("INVHDR", "INVHDR", lcInvHdr, '', '', .T.)
  = lfFilesStr("INVLINE", "INVLINE", lcInvLine, '', '', .F.)
  = lfFilesStr("CONSINVH", "CONSINVH", lcCONSINVH, '', '', .T.)
  = lfFilesStr("CONSINVL", "CONSINVL", lcCONSINVL, '', '', .F.)
  = lfFilesStr("SPCK_LIN", "SPCK_lINVR", lcSPCK_Lin, '', '', .F.)
  = lfFilesStr("OrdLine", "ORDLINE", lcOrdLine, '', '', .F.)
  = lfFilesStr("PikTkt", "PIKTKT", lcPikTkt, '', '', .F.)
  = lfFilesStr("PikLine", "PIKLINE", lcPikLine, '', '', .F.)
  
  = lfOpenFls()
  *B128464,2 WSH 08/21/2005 [End]
  
  LOCAL lcHdrFile, llScanInFilt, llExitLoop
  llExitLoop = .F.
  
  IF FILE(oAriaApplication.WorkDir + lcSelInvNo + '.DBF') AND !USED(lcSelInvNo)
    USE (oAriaApplication.WorkDir + lcSelInvNo + '.DBF') IN 0 ALIAS (lcSelInvNo)
  ENDIF
  
  IF USED(lcSelInvNo) AND RECCOUNT(lcSelInvNo) > 0
    llScanInFilt = .T.
  ENDIF
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *lcHdrFile  = loInvHdr.lcCursorView
  lcHdrFile  = lcInvHdr
  *B128464,2 WSH 08/21/2005 [End]
  
  IF llScanInFilt
    SELECT (lcSelInvNo)
    LOCATE
  ELSE
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *llExitLoop = !loInvHdr.GOTOP()
    llExitLoop = !lfGOTOP(lcHdrFile)
    *B128464,2 WSH 08/21/2005 [End]
    
    SELECT (lcHdrFile)
  ENDIF
  
  *-- Check Resule for Begin Transaction
  IF TYPE('lcTranCode') = 'N'
    SELECT (lnAlias)
    RETURN
  ENDIF
  
  DO WHILE !llExitLoop
    IF llScanInFilt
      *-- Seek for current PO or order number in the master header file...
      SELECT (lcSelInvNo)
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *IF !loInvHdr.SEEK(Invoice)
      m.Invoice = Invoice
      IF !lfSEEK(lcInvHdr, "Invoice = ?m.Invoice", m.Invoice, .T.)
      *B128464,2 WSH 08/21/2005 [End]
      
        SKIP
        llExitLoop = EOF()
        LOOP
      ENDIF
    ENDIF
    
    *-- If it is a consolidated Invoice, loop, as its quantities will be calculated from ConsInvL file
    SELECT (lcHdrFile)
    IF Consol = 'Y'
      IF llScanInFilt
        SELECT (lcSelInvNo)
        SKIP
        llExitLoop = EOF()
      ELSE
        SELECT (lcHdrFile)
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *llExitLoop = !loInvHdr.GONEXT()
        llExitLoop = !lfGONEXT(lcInvHdr)
        *B128464,2 WSH 08/21/2005 [End]
        
      ENDIF
      LOOP
    ENDIF
    
    WAIT WINDOW "Replacing Invoice : " + Invoice NOWAIT
    
    lnShipAmnt = 0
    DIMENSION  laPackId[1]
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *IF loInvLine.SEEK(Invoice)
    *  SELECT (loInvLine.lcCursorView)
    m.Invoice = Invoice
    IF lfSEEK(lcInvLine, "Invoice = ?m.Invoice", m.Invoice)
      SELECT (lcInvLine)
    *B128464,2 WSH 08/21/2005 [End]
    
      SCAN REST WHILE invoice+STR(lineno,6) = EVALUATE(lcHdrFile + '.Invoice')
        IF !EMPTY(Pack_id + cPkColor + cPckSize + cPkVersion)
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          *llGetPack  = loSPck_Lin.SEEK('P'+Account+Pack_id+cPkColor+cPckSize+cPkVersion+Style) OR;
                       loSPck_Lin.SEEK('P*****'+Pack_id+cPkColor+cPckSize+cPkVersion+Style)
          SCATTER MEMVAR
          lcSeekCond = "Type = 'P' AND Account = ?m.Account AND Pack_Id = ?m.Pack_Id AND " +;
                       "cPkColor = ?m.cPkColor AND cPckSize = ?m.cPckSize AND " +;
                       "cPkVersion = ?m.cPkVersion AND Style = ?m.Style"
          llGetPack  = lfSEEK(lcSPCK_Lin, lcSeekCond, 'P'+Account+Pack_id+cPkColor+cPckSize+cPkVersion+Style)
          m.Account  = '*****'
          llGetPack  = llGetPack OR lfSEEK(lcSPCK_Lin, lcSeekCond, 'P*****'+Pack_id+cPkColor+cPckSize+cPkVersion+Style)
          *B128464,2 WSH 08/21/2005 [End]
          
          IF lpckprpiec OR lRange OR nPkSlPrice = 0
            lnShipAmnt = lnShipAmnt + (TotQty * Price)
          ELSE
            IF ASCAN(laPackId, Pack_id + cPkColor + cPckSize + cPkVersion) = 0
              lnPackNo   = IIF(EVALUATE(lcSPCK_LIN + '.TotQty') = 0, 0, TotQty / EVALUATE(lcSPCK_LIN + '.TotQty'))
              lnShipAmnt = lnShipAmnt + (lnPackNo * nPkSlPrice)
              lnPckInd   = ALEN(laPackId, 1)
              
              DIMENSION laPackId[lnPckInd+1]
              laPackId[lnPckInd+1]= Pack_id + cPkColor + cPckSize + cPkVersion
            ENDIF
          ENDIF
        ELSE
          lnShipAmnt = lnShipAmnt + (TotQty * Price)
        ENDIF
      ENDSCAN
    ENDIF
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *loINVHDR.REPLACE(IIF(loINVHDR.llNative, "", "Rec_No WITH Rec_No"))
    *SELECT (loINVHDR.lcCursorUpdate)
    SELECT (lcInvHdr)
    *B128464,2 WSH 08/21/2005 [End]
    
    REPLACE ShipAmt   WITH IIF(Status # 'V', lnShipAmnt, 0),;
            vShipAmt  WITH IIF(Status = 'V', lnShipAmnt, 0),;
            TotalChg  WITH IIF(Status # 'V', lnShipAmnt + FREIGHT + INSUR + COD + DISCOUNT + TAX_AMT + nPstAmt, 0),;
            vTotalChg WITH IIF(Status = 'V', lnShipAmnt + FREIGHT + INSUR + COD + DISCOUNT + TAX_AMT + nPstAmt, 0)
    
    IF llViewLog AND (ShipAmt <> OLDVAL("ShipAmt") OR vShipAmt <> OLDVAL("vShipAmt") OR TotalChg <> OLDVAL("TotalChg") OR vTotalChg <> OLDVAL("vTotalChg"))
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      laRebMsg[2] = "Company " + lcCurrComp_ID + ": Invoice # " + Invoice + " has quantity or amount different than its lines."
      laRebMsg[3] = " "
      =lfVryRport()
    ENDIF
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfMasterUpd('loINVHDR', EVALUATE(lcRebalHdr + '.cUpdVryIgn') = 'U')
    = lfMasterUpd('lcInvHdr', EVALUATE(lcRebalHdr + '.cUpdVryIgn') = 'U')
    *B128464,2 WSH 08/21/2005 [End]
    
    IF llScanInFilt
      SELECT (lcSelInvNo)
      SKIP
      llExitLoop = EOF()
    ELSE
      SELECT (lcHdrFile)
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *llExitLoop = !loInvHdr.GONEXT()
      llExitLoop = !lfGONEXT(lcInvHdr)
      *B128464,2 WSH 08/21/2005 [End]
      
    ENDIF
  ENDDO
  
  llExitLoop = .F.
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *lcHdrFile  = loCONSINVH.lcCursorView
  lcHdrFile  = lcCONSINVH
  *B128464,2 WSH 08/21/2005 [End]
  
  IF llScanInFilt
    SELECT (lcSelInvNo)
    LOCATE
  ELSE
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *llExitLoop = !loCONSINVH.GOTOP()
    llExitLoop = !lfGOTOP(lcCONSINVH)
    *B128464,2 WSH 08/21/2005 [End]
    
    SELECT (lcHdrFile)
  ENDIF
  
  LOCAL llFirstRec, llFirstInv, llFoundError, lcWhileCond, lcScanAlias, lcForeCond, llFound
  llFirstInv   = .T.
  llFirstRec   = .T.
  lcInvoice    = Invoice
  lnInvShip    = 0
  llFoundError = .F.
  DO WHILE !llExitLoop
    IF llScanInFilt
      *-- Seek for current PO or order number in the master header file...
      SELECT (lcSelInvNo)
      IF llFirstRec
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *IF !loCONSINVH.SEEK(Invoice)
        m.Invoice = Invoice
        IF !lfSEEK(lcCONSINVH, "Invoice = ?m.Invoice", m.Invoice, .T.)
        *B128464,2 WSH 08/21/2005 [End]
        
          SKIP
          llExitLoop = EOF()
          LOOP
        ELSE
          llFirstRec = .F.
        ENDIF
      ELSE
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *IF !(loCONSINVH.GONEXT() AND EVALUATE(lcHdrFile + '.Invoice') = Invoice)
        IF !(lfGONEXT(lcCONSINVH) AND EVALUATE(lcHdrFile + '.Invoice') = Invoice)
        *B128464,2 WSH 08/21/2005 [End]
        
          llFirstRec = .T.
          SKIP
          llExitLoop = EOF()
          LOOP
        ENDIF
      ENDIF
    ENDIF
    
    SELECT (lcHdrFile)
    
    IF llFirstInv
      lcInvoice  = Invoice
      llFirstInv = .F.
    ENDIF
    
    SELECT (lcHdrFile)
    IF lcInvoice <> Invoice
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *IF loInvHdr.SEEK(lcInvoice)
      *  loInvHdr.REPLACE(IIF(loInvHdr.llNative, "", "Rec_No WITH Rec_No"))
      *  SELECT (loInvHdr.lcCursorUpdate)
      m.Invoice = lcInvoice
      IF lfSEEK(lcInvHdr, "Invoice = ?m.Invoice", m.Invoice, .T.)
        SELECT (lcInvHdr)
      *B128464,2 WSH 08/21/2005 [End]
      
        REPLACE ShipAmt   WITH IIF(Status # 'V', lnInvShip, 0),;
                vShipAmt  WITH IIF(Status = 'V', lnInvShip, 0),;
                TotalChg  WITH IIF(Status # 'V', lnInvShip + FREIGHT + INSUR + COD + DISCOUNT + TAX_AMT + nPstAmt, 0),;
                vTotalChg WITH IIF(Status = 'V', lnInvShip + FREIGHT + INSUR + COD + DISCOUNT + TAX_AMT + nPstAmt, 0)
                
        llFoundError = llViewLog AND (llFoundError OR ShipAmt <> OLDVAL("ShipAmt") OR vShipAmt <> OLDVAL("vShipAmt") OR TotalChg <> OLDVAL("TotalChg") OR vTotalChg <> OLDVAL("vTotalChg"))
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *= lfMasterUpd('loINVHDR', EVALUATE(lcRebalHdr + '.cUpdVryIgn') = 'U')
        = lfMasterUpd('lcINVHDR', EVALUATE(lcRebalHdr + '.cUpdVryIgn') = 'U')
        *B128464,2 WSH 08/21/2005 [End]
        
      ENDIF
      IF llFoundError
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        laRebMsg[2] = "Company " + lcCurrComp_ID + ": Invoice # " + lcInvoice + " has quantity or amount different than its lines."
        laRebMsg[3] = " "
        =lfVryRport()
      ENDIF
      
      llFoundError = .F.
      lnInvShip    = 0
      lcInvoice    = EVALUATE(lcHdrFile + '.Invoice')
    ENDIF
    
    SELECT (lcHdrFile)
    WAIT WINDOW "Replacing Consalidated Invoice : " + Invoice NOWAIT
    
    lnShipAmnt = 0
    lcPack_Id  = ""
    lcForeCond = ".T."
    llFound    = .T.
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *IF loCONSINVL.SEEK(invoice+store+order)
    *  IF EMPTY(Store) AND !EMPTY(PikTkt) AND loPikTkt.SEEK(PikTkt)
    *    = loOrdLine.SEEK('O' + EVALUATE(lcPikTkt + '.Order'))
    m.Invoice = Invoice
    m.Store   = Store
    m.Order   = Order
    m.PikTkt  = PikTkt
    IF lfSEEK(lcCONSINVL, "Invoice = ?m.Invoice AND Store = ?m.Store AND Order = ?m.Order",m.Invoice + m.Store + m.Order)
      IF EMPTY(Store) AND !EMPTY(PikTkt) AND lfSEEK(lcPikTkt, "PikTkt = ?m.PikTkt", m.PikTkt)
        m.Order = EVALUATE(lcPikTkt + '.Order')
        = lfSEEK(lcOrdLine, "CORDTYPE = 'O' AND Order = ?m.Order", 'O' + m.Order)
    *B128464,2 WSH 08/21/2005 [End]
    
        lcScanAlias = lcOrdLine
        lcWhileCond = "CORDTYPE+ORDER+STR(LINENO,6) = 'O'+'" + EVALUATE(lcPikTkt + '.Order') + "'"
        SELECT (lcScanAlias)
        LOCATE REST WHILE &lcWhileCond. FOR PikTkt = EVALUATE(lcPikTkt + '.PikTkt')
        llFound = FOUND()
        
        IF !llFound
          
          *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
          *= loPikLine.SEEK(EVALUATE(lcPikTkt + '.PikTkt') + EVALUATE(lcPikTkt + '.Order'))
          m.Order  = EVALUATE(lcPikTkt + '.Order')
          m.PikTkt = EVALUATE(lcPikTkt + '.PikTkt')
          = lfSEEK(lcPikLine, "PikTkt = ?m.PikTkt AND Order = ?m.Order", m.PikTkt + m.Order)
          *B128464,2 WSH 08/21/2005 [End]
          
          lcScanAlias = lcPikLine
          lcWhileCond = "PIKTKT+ORDER+STR(LINENO,6) = '" + EVALUATE(lcPikTkt + '.PikTkt') + "'+'" + EVALUATE(lcPikTkt + '.Order') + "'"
          SELECT (lcScanAlias)
          LOCATE REST WHILE &lcWhileCond. FOR PikTkt = EVALUATE(lcPikTkt + '.PikTkt')
          llFound = FOUND()
        ENDIF
        
        lcForeCond = "Style = " + lcScanAlias + ".Style"
      ENDIF
      
      DO WHILE llFound
        
        *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
        *SELECT (loCONSINVL.lcCursorView)
        SELECT (lcCONSINVL)
        *B128464,2 WSH 08/21/2005 [End]
        
        SCAN REST WHILE INVOICE+STORE+ORDER+STYLE+STR(LINENO,6) = EVALUATE(lcHdrFile + '.Invoice') + EVALUATE(lcHdrFile + '.Store') + EVALUATE(lcHdrFile + '.Order');
                  FOR &lcForeCond.
          IF !EMPTY(Pack_id + cPkColor + cPckSize + cPkVersion)
            
            *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
            *llGetPack = loSPck_Lin.SEEK('P'+Account+Pack_id+cPkColor+cPckSize+cPkVersion+Style) OR;
                        loSPck_Lin.SEEK('P*****'+Pack_id+cPkColor+cPckSize+cPkVersion+Style)
            SCATTER MEMVAR
            lcSeekCond = "Type = 'P' AND Account = ?m.Account AND Pack_Id = ?m.Pack_Id AND " +;
                         "cPkColor = ?m.cPkColor AND cPckSize = ?m.cPckSize AND " +;
                         "cPkVersion = ?m.cPkVersion AND Style = ?m.Style"
            llGetPack  = lfSEEK(lcSPCK_Lin, lcSeekCond, 'P'+Account+Pack_id+cPkColor+cPckSize+cPkVersion+Style)
            m.Account  = '*****'
            llGetPack  = llGetPack OR lfSEEK(lcSPCK_Lin, lcSeekCond, 'P*****'+Pack_id+cPkColor+cPckSize+cPkVersion+Style)
            *B128464,2 WSH 08/21/2005 [End]
            
            IF lpckprpiec OR lRange OR nPkSlPrice = 0
              lnShipAmnt = lnShipAmnt + (TotQty * Price)
            ELSE
              IF lcPack_id <> Pack_id + cPkColor + cPckSize + cPkVersion
                lnPackNo   = IIF(EVALUATE(lcSPCK_LIN + '.TotQty') = 0, 0, TotQty / EVALUATE(lcSPCK_LIN + '.TotQty'))
                lnShipAmnt = lnShipAmnt + (lnPackNo * nPkSlPrice)
                lcPack_id  = Pack_id + cPkColor + cPckSize + cPkVersion
              ENDIF
            ENDIF
          ELSE
            lnShipAmnt = lnShipAmnt + (TotQty * Price)
          ENDIF  
        ENDSCAN
        
        IF EMPTY(EVALUATE(lcHdrFile + '.Store')) AND !EMPTY(EVALUATE(lcHdrFile + '.PikTkt'))
          SELECT (lcScanAlias)
          SKIP
          IF !EOF() AND &lcWhileCond. AND PikTkt = EVALUATE(lcPikTkt + '.PikTkt')
            LOOP
          ENDIF
        ENDIF
        EXIT
      ENDDO
    ENDIF
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *loCONSINVH.REPLACE(IIF(loCONSINVH.llNative, "", "Rec_No WITH Rec_No"))
    *SELECT (loCONSINVH.lcCursorUpdate)
    SELECT (lcCONSINVH)
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    
    REPLACE ShipAmt  WITH lnShipAmnt ,;
            TotalChg WITH lnShipAmnt + FREIGHT + INSUR + COD +;
                          DISCOUNT + TAX_AMT + nPstAmt
    
    llFoundError = llViewLog AND (llFoundError OR ShipAmt <> OLDVAL("ShipAmt") OR TotalChg <> OLDVAL("TotalChg"))
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfMasterUpd('loCONSINVH', EVALUATE(lcRebalHdr + '.cUpdVryIgn') = 'U')
    = lfMasterUpd('lcCONSINVH', EVALUATE(lcRebalHdr + '.cUpdVryIgn') = 'U')
    *B128464,2 WSH 08/21/2005 [End]
    
    lnInvShip = lnInvShip + lnShipAmnt
    
    IF !llScanInFilt
      
      *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
      *llExitLoop = !loCONSINVH.GONEXT()
      llExitLoop = !lfGONEXT(lcCONSINVH)
      *B128464,2 WSH 08/21/2005 [End]
      
      SELECT (lcHdrFile)
    ENDIF
  ENDDO
  
  *-- Update Last Invoice Header...
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *IF !llFirstInv AND loInvHdr.SEEK(lcInvoice)
  *  loInvHdr.REPLACE(IIF(loInvHdr.llNative, "", "Rec_No WITH Rec_No"))
  *  SELECT (loInvHdr.lcCursorUpdate)
  m.Invoice = lcInvoice
  IF !llFirstInv AND lfSEEK(lcInvHdr, "Invoice = ?m.Invoice", m.Invoice, .T.)
    SELECT (lcInvHdr)
  *B128464,2 WSH 08/21/2005 [End]
  
    REPLACE ShipAmt   WITH IIF(Status # 'V', lnInvShip, 0),;
            vShipAmt  WITH IIF(Status = 'V', lnInvShip, 0),;
            TotalChg  WITH IIF(Status # 'V', lnInvShip + FREIGHT + INSUR + COD + DISCOUNT + TAX_AMT + nPstAmt,0),;
            vTotalChg WITH IIF(Status = 'V', lnInvShip + FREIGHT + INSUR + COD + DISCOUNT + TAX_AMT + nPstAmt,0)
                
    llFoundError = llViewLog AND (llFoundError OR ShipAmt <> OLDVAL("ShipAmt") OR vShipAmt <> OLDVAL("vShipAmt") OR TotalChg <> OLDVAL("TotalChg") OR vTotalChg <> OLDVAL("vTotalChg"))
    
    *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
    *= lfMasterUpd('loINVHDR', EVALUATE(lcRebalHdr + '.cUpdVryIgn') = 'U')
    = lfMasterUpd('lcINVHDR', EVALUATE(lcRebalHdr + '.cUpdVryIgn') = 'U')
    *B128464,2 WSH 08/21/2005 [End]
    
    IF llFoundError
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      laRebMsg[2] = "Company " + lcCurrComp_ID + ": Invoice # " + lcInvoice + " has quantity or amount different than its lines."
      laRebMsg[3] = " "
      =lfVryRport()
    ENDIF
  ENDIF
  
  *B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
  *STORE .NULL. TO loInvHdr, loInvLine, loCONSINVH, loCONSINVL, loSPck_Lin, loOrdLine, loPikTkt, loPikLine
  =lfCloseFls()
  *B128464,2 WSH 08/21/2005 [End]
  
ENDIF

SELECT (lnAlias)
RETURN

*!**************************************************************************
*! Name      : lfCHANGWHS
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/02/2005
*! Purpose   : Save FOB and Origin Codes to POSHDR file in PO Screens
*!**************************************************************************
*! Example   : lfSAVEUDF()
*!**************************************************************************
*! B128324,1 
*!**************************************************************************
FUNCTION lfSAVEUDF

LOCAL lcBaseFile
lcBaseFile = loFormSet.DataEnvironment.InitialSelectedAlias

WITH loFormSet.ariaForm1.pgfPOStyle.page1.cntheaderFolder
  .txtFOB.Value = IIF(EMPTY(EVALUATE(lcBaseFile + '.CFOBCODE')),'',gfCodDes(EVALUATE(lcBaseFile + '.CFOBCODE'), 'CFOBCODE'))
  .txtOrigin.Value = IIF(EMPTY(EVALUATE(lcBaseFile + '.CORIGINCOD')),'',gfCodDes(EVALUATE(lcBaseFile + '.CORIGINCOD'),'CORIGINCOD'))
ENDWITH

loFormSet.AriaForm1.Refresh()

RETURN

*!**************************************************************************
*! Name      : lfEnaConfg
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 07/11/2005
*! Purpose   : Enable the configuration check box in the style screen
*!**************************************************************************
*! Example   : lfEnaConfg()
*!**************************************************************************
*! E039464
*!**************************************************************************
FUNCTION lfEnaConfg

IF loFormSet.llUseStyleConfiguration
  IF loFormSet.ActiveMode $ 'AE'
    loFormSet.ariaForm1.pgfStyleInfo.page1.chkDyelot.Enabled = .T.
  ELSE
    loFormSet.ariaForm1.pgfStyleInfo.page1.chkDyelot.Enabled = .F.
  ENDIF  
ELSE
  loFormSet.ariaForm1.pgfStyleInfo.page1.chkDyelot.Enabled = .F.
ENDIF


*!**************************************************************************
*! Name      : lfUpdAssFld
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 09/07/2005
*! Purpose   : To update the assist cost fields from header when adding lines
*!**************************************************************************
*! Example   : lfUpdAssFld()
*!**************************************************************************
*! 128356,1
*!**************************************************************************
FUNCTION lfUpdAssFld

*B608434,1 WAM 02/14/2008 Use header temporary file instead of buffered order header
*m.cLoc_No   = OrdHdr.cLoc_No
*m.cLocBank  = OrdHdr.cLocBank
*m.dLocIssue = OrdHdr.dLocIssue

m.cLoc_No   = &lcOrdHdr..cLoc_No
m.cLocBank  = &lcOrdHdr..cLocBank
m.dLocIssue = &lcOrdHdr..dLocIssue
*B608434,1 WAM 02/14/2008 (End)

GATHER MEMVAR FIELDS cLoc_No, cLocBank, dLocIssue

*:**************************************************************************
*:* Name        : lfUPDPKFLD
*:* Developer   : Hend Ghanem (HBG)
*:* Date        : 11/21/2005
*:* Purpose     : Update Pack fields in ORdline
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUPDPKFLD()
*:***************************************************************************
*: 130467,1
*:***************************************************************************
FUNCTION lfUPDPKFLD

lcOrdLine = loFormSet.ariaForm1.Ariapageframe1.Page2.Ariaeditregion1.DetailFile
IF !EMPTY(lcPackId)
  lcAlias = ALIAS()
  SELECT (lcOrdLine)
  SET ORDER TO SPCK_HDRVR IN SPCK_HDR
  llUpdate = .T.
  =SEEK('S'+&lcOrdLine..Scale,'SCALE')
  FOR lnI =  1 TO SCALE.cnt 
    lcI = ALLTRIM(STR(lnI))
    IF MOD(&lcOrdLine..Qty&lcI / SPCK_LIN.Qty&lcI,1) <> 0
      llUpdate = .F.
      EXIT
    ENDIF
  ENDFOR
  IF llUpdate 
    REPLACE &lcOrdLine..Price      WITH SPCK_LIN.npck_price,;
            &lcOrdLine..Gros_Price WITH SPCK_LIN.npck_price,;
            &lcOrdLine..cPkColor   WITH SPCK_LIN.cPkColor  ,;
            &lcOrdLine..cPckSize   WITH SPCK_LIN.cPckSize  ,;
            &lcOrdLine..cPkVersion WITH SPCK_LIN.cPkVersion,;
            &lcOrdLine..cPckAcc    WITH SPCK_LIN.Account    
    =SEEK('P'+SPCK_LIN.Account+SPCK_LIN.Pack_Id+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize+SPCK_LIN.cPkVersion,'SPCK_HDR')
    REPLACE &lcOrdLine..lRange     WITH SPCK_HDR.lRange,;
            &lcOrdLine..lPckPrPiec WITH SPCK_HDR.lPckPrPiec ,;
            &lcOrdLine..nPkSlPrice WITH SPCK_HDR.nPkSlPrice,;
            &lcOrdLine..nPackNo    WITH &lcOrdLine..TotQty/SPCK_LIN.TotQty
  ELSE
    =gfModalGen("INM00000B00000","Dialog",.F.,.F.,"Qty of this line not divisable by pack Qty.Cann't select this pack.")  
    lcPackId = " "
  ENDIF  
  =lfChkRgPck()
  SELECT (lcAlias)
ENDIF
SELECT (lcOrdLine)



*:**************************************************************************
*:* Name        : lfCHKUDFGMA
*:* Developer   : Mariam Mazhar(MMT)
*:* Date        : 08/02/2006
*:* Purpose     : check user defind fields for GMA
*:***************************************************************************
FUNCTION lfCHKUDFGMA 
LOCAL lcBaseFile
lcBaseFile = loFormSet.DataEnvironment.InitialSelectedAlias
IF EMPTY(&lcBaseFile..CFOBCODE)
  =gfModalGen("INM00000B00000","Dialog",.F.,.F.,"You have to select a FOB Code.")
  RETURN  .F.
ELSE
  IF EMPTY(&lcBaseFile..CORIGINCOD)
    =gfModalGen("INM00000B00000","Dialog",.F.,.F.,"You have to select an Origin Code.")
    RETURN .F.
  ELSE
    IF EMPTY(&lcBaseFile..CFNLDST)
      =gfModalGen("INM00000B00000","Dialog",.F.,.F.,"You have to select a Final Destination.")
      RETURN .F.
    ENDIF 
  ENDIF 
ENDIF 


*!* C200938,1 MMT 02/13/2008 Add CustPo and Retail price screen to So Screen[Start]
*!**************************************************************************
*! Name      : lfVOkSo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/28/2001        
*! Purpose   : validate Ok Button
*!**************************************************************************
*! Example   : =lfVOkSo()
*!**************************************************************************
FUNCTION lfVOkSo
PARAMETERS lcCustPO,lnRetPrice
REPLACE custpo     WITH lcCustPO;
        nsugretpri WITH lnRetPrice,;
        Flag WITH "M" IN SELECT (loFormSet.AriaForm1.AriaPageFrame1.Page2.AriaEditRegion1.detailfile)


*!**************************************************************************
*! Name      : lfEDTSCRSO
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/28/2001        
*! Purpose   : Display Screen to Edit the pack information.
*!*************************************************************
*! Example   : =lfEDTSCRSO
*!*************************************************************
*!C200424,1 MHM
PROCEDURE lfEDTSCRSO
DO FORM  (oAriaApplication.ScreenHome+"SO\SOEDTCST.SCX") WITH loFormSet
*!* C200938,1 MMT 02/13/2008 Add CustPo and Retail price screen to So Screen[End]


*C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA [Start]
*!*************************************************************
*! Name      : lfADDNOCLK   C200967
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/18/2008
*! Purpose   : Add Notes butten of Edit Debit\credit Adj. Reason Screen
*!*************************************************************
FUNCTION lfADDNOTEB

loFormSet.ariaForm1.AddObject("cmdNotes","ariacommandbutton")
WITH loFormSet.ariaForm1.cmdNotes
  .Visible =.T.
  .left= loFormSet.ariaform1.cboDReason.left + loFormSet.ariaform1.cboDReason.width + 10
  .Top = loFormSet.ariaform1.cboDReason.top 
  .caption = 'Notes'
  .Width = 85
  .Enabled = .F.
ENDWITH 
loFormSet.ariaForm1.ariaGrid1.AddColumn(6)
loFormSet.ariaForm1.ariaGrid1.Column5.Columnorder = 6
loFormSet.ariaForm1.ariaGrid1.Column6.Columnorder = 5
loFormSet.ariaForm1.ariaGrid1.Column6.Header1.caption = 'Amount'
loFormSet.ariaform1.ariagrid1.RecordSource =""
lfCrtTmpFls()
loFormSet.ASSigncontrols ('D',.t.)

BINDEVENT(loFormSet.ariaForm1.cmdNotes,"Click",loFormSet,"delete")  

*!*************************************************************
*! Name      : lfADDNOCLK   C200967
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/18/2008
*! Purpose   : Open Notes screen of Edit Debit\credit Adj. Reason Screen
*!*************************************************************
FUNCTION lfADDNOCLK  

lcAls = SELECT()
DO FORM (oAriaApplication.ScreenHome + 'Arlnotes') WITH IIF(loFormSet.ariaform1.Ariaoptiongroup2.Value =1,;
														LOFORMSET.lcTmpFiled,LOFORMSET.lcTmpFilec),;
														loFormSet.Activemode <> 'V'
									
IF loFormSet.Activemode <> 'V'															
  SELECT (IIF(loFormSet.ariaform1.Ariaoptiongroup2.Value =1,;
														LOFORMSET.lcTmpFiled,LOFORMSET.lcTmpFilec))															
  REPLACE Status WITH 'M'															
ENDIF 
SELECT(lcAls)  

*!*************************************************************
*! Name               : lfCrtTmpFls C200967
*! Developer          : Mariam Mazhar[MMT]
*! Date               : 03/17/2008
*! Purpose            : Create Temp Files
*!*************************************************************
*!
PROCEDURE lfCrtTmpFls

PRIVATE lnCurAlias 
lnCurAlias = SELECT(0)

DIMENSION laFileStru[12,18]
STORE '' TO laFileStru
laFileStru[1,1]   = 'Account'              &&-- COMMON
laFileStru[1,2]   = 'C'
laFileStru[1,3]   = 5
laFileStru[1,4]   = 0
      
laFileStru[2,1]   = 'Trancode'             &&-- DEBIT
laFileStru[2,2]   = 'C'
laFileStru[2,3]   = 6
laFileStru[2,4]   = 0

laFileStru[3,1]   = 'Desc'                &&-- COMMON
laFileStru[3,2]   = 'C'
laFileStru[3,3]   = 20
laFileStru[3,4]   = 0

laFileStru[4,1]   = 'Reference'            &&COMM
laFileStru[4,2]   = 'C'
laFileStru[4,3]   = 30
laFileStru[4,4]   = 0

laFileStru[5,1]   = 'Ccreditcod'       && CREDIT
laFileStru[5,2]   = 'C'
laFileStru[5,3]   = 6
laFileStru[5,4]   = 0

laFileStru[6,1]   = 'Tran'        &&COMMON
laFileStru[6,2]   = 'C'
laFileStru[6,3]   = 6
laFileStru[6,4]   = 0

laFileStru[7,1]   = 'Trandate'      &&COM
laFileStru[7,2]   = 'D'
laFileStru[7,3]   = 8
laFileStru[7,4]   = 0

laFileStru[8,1]   = 'Status'        && NEW
laFileStru[8,2]   = 'C'
laFileStru[8,3]   = 1
laFileStru[8,4]   = 0

laFileStru[9,1]   = 'Trantype'       && COMMON
laFileStru[9,2]   = 'C'
laFileStru[9,3]   = 1
laFileStru[9,4]   = 0

laFileStru[10,1]   = 'Cinstalno'   &&-- Debit
laFileStru[10,2]   = 'C'
laFileStru[10,3]   = 3
laFileStru[10,4]   = 0

laFileStru[11,1]   = 'Amount'   
laFileStru[11,2]   = 'N'
laFileStru[11,3]   = 14
laFileStru[11,4]   = 2

laFileStru[12,1]   = 'Note_Mem'   
laFileStru[12,2]   = 'M'
laFileStru[12,3]   = 10
laFileStru[12,4]   = 0




=gfCrtTmp(LOFORMSET.lcTmpFiled ,@laFileStru,"account+tran+DTOS(trandate)",LOFORMSET.lcTmpFiled,.f.)
=gfCrtTmp(LOFORMSET.lcTmpFilec ,@laFileStru,"account+tran+DTOS(trandate)",LOFORMSET.lcTmpFilec,.f.)

*!*************************************************************
*! Name      : lfADDCNTSRC  C200967
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/18/2008
*! Purpose   : Add Cntrl Src of Edit Debit\credit Adj. Reason Screen
*!*************************************************************
FUNCTION lfADDCNTSRC

IF TYPE('LOFORMSET.ariaform1.ariagrid1.column6') = 'O'
  LOFORMSET.ariaform1.ariagrid1.column6.ControlSource= "Amount"     
  LOFORMSET.ariaform1.ariagrid1.column6.ReadOnly = .T. 
ENDIF 

*!*************************************************************
*! Name      : lfGETNOTES C200967
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/18/2008
*! Purpose   : Get Notes of Edit Debit\credit Adj. Reason Screen
*!*************************************************************
FUNCTION lfGETNOTES

IF Upper(ALIAS()) = 'DEBIT'
  REPLACE note_mem WITH Debit.note_mem ,;
   		  Amount   WITH Debit.amount  IN (loformset.lcTmpFiled)
  
ELSE
  REPLACE note_mem WITH credit.note_mem ,;
          Amount   WITH Credit.amount  IN (loformset.lcTmpFilec)
ENDIF 

*!*************************************************************
*! Name      : lfSAVNOTES   C200967
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/18/2008
*! Purpose   : Save Notes of Edit Debit\credit Adj. Reason Screen
*!*************************************************************
FUNCTION lfSAVNOTES
IF UPPER(Alias()) = loformset.lcTmpFileD
  lcDepit = loformset.lcTmpFileD
  REPLACE Debit.Note_Mem   WITH &lcDepit..Note_Mem  
ELSE
  lcCredit = loformset.lcTmpFilec
  REPLACE  credit.Note_Mem   WITH &lcCredit..Note_Mem
ENDIF 
*!*************************************************************
*! Name      : lfCHNGMODE   C200967
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/18/2008
*! Purpose   : Change mode modifications of Edit Debit\credit Adj. Reason Screen
*!*************************************************************
FUNCTION lfCHNGMODE  
IF TYPE('loFormSet.ariaForm1.cmdNotes') = 'O'
  IF LOFORMSET.activeMode <> 'S'
    IF RECCOUNT(IIF(loFormSet.ariaform1.Ariaoptiongroup2.Value =1,LOFORMSET.lcTmpFiled,LOFORMSET.lcTmpFilec)) > 0
      loFormSet.ariaForm1.cmdNotes.Enabled = .T.
    ELSE
      loFormSet.ariaForm1.cmdNotes.Enabled = .F.
    ENDIF    
  ELSE
    loFormSet.ariaForm1.cmdNotes.Enabled = .F.
  ENDIF 
ENDIF
*C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA [Start] 

*C200995,1 MMT 05/12/2008 Bin location screen for GMA [Start] 
*!*************************************************************
*! Name      : lfADDOPT    C200995
*! Developer : Mariam Mazhar (MMT)
*! Date      : 05/12/2008
*! Purpose   : add option menu to locatin screen
*!*************************************************************
FUNCTION lfADDOPT    

lcHostFormName = '[' + loFormSet.cHostFormName + ']'

IF !lfFoundPad('Option')
  DEFINE PAD _Option OF (loFormSet.chostformname) PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF (loFormSet.chostformname) ACTIVATE POPUP _OPTIONPOP
  
  *--Define option pasd bars.
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
  
  
  lnBarNo = CNTBAR('_OPTIONPOP') + 1

*--Add menu Bar for Serial Lookup Screen
IF lnBarNo > 1
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT '\-'
  lnBarNo = lnBarNo + 1
ENDIF

DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Warehouse \<Structure'   SKIP FOR IIF(TYPE('_screen.ActiveForm.Parent') = 'O',_screen.ActiveForm.Parent.activemode $ 'AS',.F.)
ON SELECTION BAR lnBarNo OF _OPTIONPOP lfCallScr()

ENDIF

RETURN

*!*************************************************************
*! Name      : lfCallScr C200995
*! Developer : Mariam Mazhar (MMT)
*! Date      : 05/12/2008
*! Purpose   : call levels screen
*!*************************************************************
FUNCTION lfCallScr

DO FORM (oAriaApplication.ScreenHome+"\icloclvl.scx") WITH _screen.ActiveForm.Parent
*!* B608580,1 MMT 06/09/2008 Fix Bugs of screen refresh and Status Value[Start]
_screen.ActiveForm.Parent.AriaForm1.GrdBins.refresh
*!* B608580,1 MMT 06/09/2008 Fix Bugs of screen refresh and Status Value[End]


*!*************************************************************
*! Name      : lfGETLVLS C200995
*! Developer : Mariam Mazhar (MMT)
*! Date      : 05/12/2008
*! Purpose   : get levels data
*!*************************************************************
FUNCTION lfGETLVLS


IF loFormSet.DataSessionId <> SET("Datasession") 
  SET DATASESSION TO loFormSet.DataSessionId
ENDIF   


lcKey  = iif(loFormSet.ActiveMode = 'E',loFormSet.Ariaform1.kblocation.keytextbox.value,Warehous.cwarecode)

IF TYPE('loFormSet.lctemplvl') = 'U'
  loFormSet.AddProperty('lctemplvl')
ENDIF  
loFormSet.lctemplvl = gfTempName()
SET MULTILOCKS ON 

IF !USED('WHSLVSTR')
 =gfOpenTable('WHSLVSTR','PSTRGAREA')
ENDIF 
IF !USED('WHSLOC_LVL')
  =gfOpenTable('WHSLOC','WHSLOC',"SH",'WHSLOC_LVL')
ENDIF 


SELECT WHSLVSTR
lnLenght = AFIELDS(laFileStruct)
DIMENSION laFileStruct[lnLenght +2,18]
laFileStruct[lnLenght +1,1] = 'lBin'
laFileStruct[lnLenght +1,2] = 'L'
laFileStruct[lnLenght +1,3] = 1
laFileStruct[lnLenght +1,4] = 0

laFileStruct[lnLenght +2,1] = 'ladded'
laFileStruct[lnLenght +2,2] = 'L'
laFileStruct[lnLenght +2,3] = 1
laFileStruct[lnLenght +2,4] = 0

FOR lnI = 1 TO 2
  STORE '' TO laFileStruct[lnLenght +lnI ,7],laFileStruct[lnLenght +lnI ,8],laFileStruct[lnLenght +lnI ,9],;
          laFileStruct[lnLenght +lnI ,10],laFileStruct[lnLenght +lnI ,11],laFileStruct[lnLenght +lnI ,12],;
          laFileStruct[lnLenght +lnI ,13],laFileStruct[lnLenght +lnI ,14],laFileStruct[lnLenght +lnI ,15],;
          laFileStruct[lnLenght +lnI ,16]
  STORE 0 TO  laFileStruct[lnLenght +lnI ,17],laFileStruct[lnLenght +lnI ,18]
ENDFOR 

lnPos  = ASCAN(laFileStruct,'CSTRGAREA')
IF  lnPos  > 0
  lnPos = ASUBSCRIPT(laFileStruct,lnPos,1)
  laFileStruct[lnPos ,3] = 10
ENDIF 


gfcrttmp (loFormSet.lctemplvl ,@laFileStruct,"CWARECODE+CPSTRGAREA+CSTRGAREA",loFormSet.lctemplvl)

Sele(loFormSet.lctemplvl)
INDEX on CWARECODE+CSTRGAREA +CPSTRGAREA TAG 'LvlIndex'
SET ORDER TO (loFormSet.lctemplvl)



if gfSeek(lcKey , 'WHSLVSTR')
  SELECT WHSLVSTR
  SCAN REST WHILE CWARECODE+CPSTRGAREA+CSTRGAREA = lcKey   
    SCATTER MEMO MEMVAR 
    INSERT INTO (loFormSet.lctemplvl) FROM MEMVAR 
  ENDSCAN  
ENDIF 

*!*************************************************************
*! Name      : lfSAVLVLS C200995
*! Developer : Mariam Mazhar (MMT)
*! Date      : 05/12/2008
*! Purpose   : get levels data
*!*************************************************************
FUNCTION lfSAVLVLS


lcAlias =ALIAS()
lcKey  = loFormSet.Ariaform1.kblocation.keytextbox.value
Sele(loFormSet.lctemplvl)
lcDeleted = SET("Deleted")
SET DELETED OFF 
SCAN 
  IF LBIN
    IF DELETED()
      IF gfSeek(lcKey  +CSTRGAREA+SPACE(19),'WHSLOC_LVL')
        SELECT WHSLOC_LVL
        gfDelete()
        SELECT(loFormSet.lctemplvl)
      ENDIF 
    ELSE
      *!* B608580,1 MMT 06/09/2008 Fix Bugs of screen refresh and Status Value[Start]
      *IF gfSeek(lcKey  +CSTRGAREA+SPACE(19),'WHSLOC_LVL') 
      IF gfSeek(lcKey  +CSTRGAREA+SPACE(19),'WHSLOC_LVL') AND !DELETED('WHSLOC_LVL')
      *!* B608580,1 MMT 06/09/2008 Fix Bugs of screen refresh and Status Value[End]
        IF WHSLOC_LVL.CSTRGAREA <> CPSTRGAREA 
          SELECT WHSLOC_LVL
          =gfReplace("CSTRGAREA with '"+EVALUATE(loFormSet.lctemplvl+'.CPSTRGAREA')+"'")
          = gfAdd_Info('WHSLOC_LVL')
          gfReplace()
          SELECT(loFormSet.lctemplvl)
        ENDIF   
      ELSE
        m.CWARECODE = lcKey  
        m.CLOCATION = CSTRGAREA
        m.cstrgarea = CPSTRGAREA 
        SELECT WHSLOC_LVL
        APPEND BLANK 
        GATHER memo MEMVAR 
        = gfAdd_Info('WHSLOC_LVL')
        gfReplace()
        SELECT(loFormSet.lctemplvl)
      ENDIF 
    ENDIF 
  ELSE
    IF DELETED()
*!*	      SELECT WHSLVSTR
*!*	      gfSetORder('strgarea')
*!*	      SELECT(loFormSet.lctemplvl)
      IF gfSeek(lcKey+cpstrgarea+cstrgarea,'WHSLVSTR')
        SELECT WHSLVSTR
        gfDelete()
        SELECT(loFormSet.lctemplvl)
      ENDIF 
*!*	      SELECT WHSLVSTR
*!*	      gfSetORder('pstrgarea')
      SELECT(loFormSet.lctemplvl)
    ELSE
      IF gfSeek(lcKey+CPSTRGAREA+cstrgarea,'WHSLVSTR')
        SCATTER MEMO MEMVAR 
        SELECT WHSLVSTR
        GATHER MEMO MEMVAR 
        = gfAdd_Info('WHSLVSTR')
        gfReplace()
        SELECT(loFormSet.lctemplvl)
      ELSE
        SCATTER MEMO MEMVAR 
        SELECT WHSLVSTR
        APPEND BLANK 
        GATHER MEMO MEMVAR 
        = gfAdd_Info('WHSLVSTR')
        gfReplace()
        SELECT(loFormSet.lctemplvl)
      ENDIF 
    ENDIF 
  ENDIF 
ENDSCAN 
SELECT WHSLVSTR
gfTableUpdate()
SELECT WHSLOC_LVL 
gfTableUpdate()
SET DELETED &lcDeleted 
SELECT(lcAlias)
*C200995,1 MMT 05/12/2008 Bin location screen for GMA [End] 