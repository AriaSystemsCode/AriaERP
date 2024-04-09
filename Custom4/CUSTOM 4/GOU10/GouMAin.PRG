*!**************************************************************************
*! Name      : GOUMAIN.PRG
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/24/2013
*! Purpose   : Gordini Custom Process Program. C201584.122,C201585.EXE,E303398.EXE [T20130206.0014]
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
*! Modifications
*! C201585,1 MMT 09/01/2013 Update Sales reps commission if user changed Sales ID[T20130206.0014-Issue#1]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfvNote
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/25/2013
*! Purpose   : Validate Notes field added by user and check he wants to apply it on all stores
*!**************************************************************************
FUNCTION lfvNote
lPARAMETERS lcRetValue
IF TYPE('loArCustScreen') <> 'O' OR loArCustScreen = null 
  RETURN .F. 
ENDIF 
lnOldOgDs = SET("Datasession") 
SET DATASESSION TO loArCustScreen.DATASESSIONID
IF Customer.type ='M' 
  IF !USED('Customer_Str')
    =gfOpenTable('Customer','Customer','SH','Customer_Str')
  ENDIF
  IF TYPE('loArCustScreen.llUpdateStoreNote') = 'U'
    loArCustScreen.AddProperty('llUpdateStoreNote',.F.)
  ENDIF
  IF TYPE('loArCustScreen.llNoteMsgDisplayed') = 'U'
    loArCustScreen.AddProperty('llNoteMsgDisplayed',.F.)
  ENDIF
  
  IF gfSeek('S'+Customer.Account,'Customer_Str','Customer')
    lcControl = loOGScroll.FocusControl
    lcCurVar  = loOGScroll.&lcControl.
    lcCurVal  = lcCurVar.Value
    IF !ALLTRIM(lcCurVal) == ALLTRIM(lcCurVar.OldValue)
      IF !loArCustScreen.llNoteMsgDisplayed 
        IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you like to update notes for All Stores') = 1
          loArCustScreen.llUpdateStoreNote = .T.
        ELSE
          loArCustScreen.llUpdateStoreNote = .F.
        ENDIF  
        loArCustScreen.llNoteMsgDisplayed= .T.
      ENDIF   
    ENDIF 
  ENDIF   
ENDIF
SET DATASESSION TO lnOldOgDs
*!**************************************************************************
*! Name      : lfDFNGLBVAR
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/25/2013
*! Purpose   : Define Global Variable to hold reference to customer screen
*!**************************************************************************
FUNCTION lfDFNGLBVAR
PUBLIC loArCustScreen
loArCustScreen = loFormSet
*!**************************************************************************
*! Name      : lfRLSGLBVAR 
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/25/2013
*! Purpose   : Release Global Variable to hold reference to customer screen
*!**************************************************************************
FUNCTION lfRLSGLBVAR 
RELEASE loArCustScreen
*!**************************************************************************
*! Name      : lfSHPVIAMSG
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/25/2013
*! Purpose   : Validate Shipvia field to check if he wants to apply it on all stores
*!**************************************************************************
FUNCTION lfSHPVIAMSG
IF TYPE('loFormSet.llUpdateStoreShipVia') = 'U'
  loFormSet.AddProperty('llUpdateStoreShipVia',.F.)
ENDIF
IF Customer.type ='M' 
  IF !USED('Customer_Str')
    =gfOpenTable('Customer','Customer','SH','Customer_Str')
  ENDIF
ELSE
  RETURN   
ENDIF

IF  gfSeek('S'+Customer.Account,'Customer_Str','Customer') AND loFormSet.ariaForm1.ariapageframe1.page1.cboShipVia.Value <> loFormSet.ariaForm1.ariapageframe1.page1.cboShipVia.OldValue 
  IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you like to update Ship Via for All Stores') = 1
    loFormSet.llUpdateStoreShipVia =.T.
  ELSE
    loFormSet.llUpdateStoreShipVia =.F.
  ENDIF 
ENDIF   
*!**************************************************************************
*! Name      : lfSLSRP1MSG 
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/25/2013
*! Purpose   : Validate rep1 field to check if he wants to apply it on all stores
*!**************************************************************************
FUNCTION lfSLSRP1MSG 
IF TYPE('loFormSet.llUpdateStoreRep1') = 'U'
  loFormSet.AddProperty('llUpdateStoreRep1',.F.)
ENDIF
IF Customer.type ='M' 
  IF !USED('Customer_Str')
    =gfOpenTable('Customer','Customer','SH','Customer_Str')
  ENDIF
ELSE
  RETURN   
ENDIF

IF  gfSeek('S'+Customer.Account,'Customer_Str','Customer') AND loFormSet.ariaForm1.ariapageframe1.page1.keyRep1.keytextbox.Value <> loFormSet.ariaForm1.ariapageframe1.page1.keyRep1.keytextbox.oldValue 
  IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you like to update Rep1 for All Stores') = 1
    loFormSet.llUpdateStoreRep1=.T.
  ELSE
    loFormSet.llUpdateStoreRep1=.F.
  ENDIF 
ENDIF 

*!**************************************************************************
*! Name      : lfSLSRP2MSG 
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/25/2013
*! Purpose   : Validate rep2 field to check if he wants to apply it on all stores
*!**************************************************************************
FUNCTION lfSLSRP2MSG 
IF TYPE('loFormSet.llUpdateStoreRep2') = 'U'
  loFormSet.AddProperty('llUpdateStoreRep2',.F.)
ENDIF
IF Customer.type ='M' 
  IF !USED('Customer_Str')
    =gfOpenTable('Customer','Customer','SH','Customer_Str')
  ENDIF
ELSE
  RETURN   
ENDIF
IF  gfSeek('S'+Customer.Account,'Customer_Str','Customer') AND loFormSet.ariaForm1.ariapageframe1.page1.keyRep2.keytextbox.Value <> loFormSet.ariaForm1.ariapageframe1.page1.keyRep2.keytextbox.oldValue 
  IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Do you like to update Rep2 for All Stores') = 1
    loFormSet.llUpdateStoreRep2=.T.
  ELSE
    loFormSet.llUpdateStoreRep2=.F.
  ENDIF 
ENDIF 
*!**************************************************************************
*! Name      : lfSAVESTORE
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/25/2013
*! Purpose   : saving applied data on all stores
*!**************************************************************************
FUNCTION lfSAVESTORE
IF Customer.type ='M' 
  IF !USED('Customer_Str')
    =gfOpenTable('Customer','Customer','SH','Customer_Str')
  ENDIF
ELSE
  RETURN   
ENDIF
lcNotes = Customer.CNOTES    
lcShipVia = Customer.ShipVia
lcRep1 = Customer.salesrep 
lcRep2 = Customer.rep2
*! C201585,1 MMT 09/01/2013 Update Sales reps commission if user changed Sales ID[T20130206.0014-Issue#1][Start]
lnRepComm1 = customer.comm
lnRepComm2 = customer.comm2
*! C201585,1 MMT 09/01/2013 Update Sales reps commission if user changed Sales ID[T20130206.0014-Issue#1][END]

IF (TYPE('loFormSet.llUpdateStoreRep2') ='L' AND loFormSet.llUpdateStoreRep2) OR ;
   (TYPE('loFormSet.llUpdateStoreRep1') ='L' AND loFormSet.llUpdateStoreRep1) OR ;
   (TYPE('loFormSet.llUpdateStoreNote') ='L' AND loFormSet.llUpdateStoreNote) OR ;
   (TYPE('loFormSet.llUpdateStoreShipVia') ='L' AND loFormSet.llUpdateStoreShipVia) 
  lnOldSelAls = SELECT(0)
  SELECT Customer_Str
  =gfSeek('S'+Customer.Account)
  SCAN REST WHILE TYPE+ACCOUNT+STORE = 'S'+Customer.Account
    IF TYPE('loFormSet.llUpdateStoreRep2') ='L' AND loFormSet.llUpdateStoreRep2
      gfREPLACE ('rep2 WITH lcRep2')
      *! C201585,1 MMT 09/01/2013 Update Sales reps commission if user changed Sales ID[T20130206.0014-Issue#1][Start]
      gfREPLACE ('comm2 WITH lnRepComm2')
      *! C201585,1 MMT 09/01/2013 Update Sales reps commission if user changed Sales ID[T20130206.0014-Issue#1][END]
    ENDIF 
    IF TYPE('loFormSet.llUpdateStoreRep1') ='L' AND loFormSet.llUpdateStoreRep1
      gfREPLACE ('salesrep WITH lcRep1')
      *! C201585,1 MMT 09/01/2013 Update Sales reps commission if user changed Sales ID[T20130206.0014-Issue#1][Start]
      gfREPLACE ('comm WITH lnRepComm1')
      *! C201585,1 MMT 09/01/2013 Update Sales reps commission if user changed Sales ID[T20130206.0014-Issue#1][END]
    ENDIF 
    IF TYPE('loFormSet.llUpdateStoreNote') ='L' AND loFormSet.llUpdateStoreNote
      gfREPLACE ('CNOTES WITH lcNotes')
    ENDIF 
    IF TYPE('loFormSet.llUpdateStoreShipVia') ='L' AND loFormSet.llUpdateStoreShipVia
      gfREPLACE ('ShipVia WITH lcShipVia')  
    ENDIF 
  ENDSCAN 
  SELECT Customer_Str
  =gfTableUpdate()
  SELECT (lnOldSelAls)
ENDIF 

IF TYPE('loFormSet.llUpdateStoreRep2') ='L' 
 loFormSet.llUpdateStoreRep2 = .F.
ENDIF  
IF TYPE('loFormSet.llUpdateStoreRep1') ='L' 
 loFormSet.llUpdateStoreRep1 = .F.
ENDIF
IF TYPE('loFormSet.llUpdateStoreNote') ='L' 
  loFormSet.llUpdateStoreNote =.F.
ENDIF
IF TYPE('loFormSet.llUpdateStoreShipVia') ='L' 
  loFormSet.llUpdateStoreShipVia =.F.
ENDIF 
IF TYPE('loFormSet.llNoteMsgDisplayed') ='L' 
 loFormSet.llNoteMsgDisplayed = .F.
ENDIF  
*!**************************************************************************
*! Name      : lfCHNGMODE  
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/25/2013
*! Purpose   : reset properties in change mode
*!**************************************************************************
FUNCTION lfCHNGMODE  
IF !'SAVEFILES' $ UPPER(lfGtCallFn())
  IF TYPE('loFormSet.llUpdateStoreRep2') ='L' 
   loFormSet.llUpdateStoreRep2 = .F.
  ENDIF  
  IF TYPE('loFormSet.llUpdateStoreRep1') ='L' 
   loFormSet.llUpdateStoreRep1 = .F.
  ENDIF
  IF TYPE('loFormSet.llUpdateStoreNote') ='L' 
    loFormSet.llUpdateStoreNote =.F.
  ENDIF
  IF TYPE('loFormSet.llUpdateStoreShipVia') ='L' 
    loFormSet.llUpdateStoreShipVia =.F.
  ENDIF 
  IF TYPE('loFormSet.llNoteMsgDisplayed') ='L' 
   loFormSet.llNoteMsgDisplayed = .F.
  ENDIF  
ENDIF
*!**************************************************************************
*! Name      : lfUPDORDNOTE
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/25/2013
*! Purpose   : copy customer/Store Notes to Sales order notes
*!**************************************************************************
FUNCTION lfUPDORDNOTE
IF loFormSet.ActiveMode <> 'A' OR Ordhdr.cOrdType <> 'O'
  RETURN
ENDIF
lnAliasSel = SELECT()
IF !USED('Customer_Str')
  =gfOpenTable('Customer','Customer','SH','Customer_Str')
ENDIF
lcNotes = ''
IF EMPTY(Ordhdr.Store) OR ordhdr.multi = 'Y'
  =gfSeek('M'+ORDHDR.ACCOUNT,'Customer_Str')
  lcNotes = Customer_Str.cNotes
ELSE
  =gfSeek('S'+ORDHDR.ACCOUNT+ORDHDR.Store,'Customer_Str')
  lcNotes = Customer_Str.cNotes
ENDIF 
IF !EMPTY(ALLTRIM(lcNotes))
  SELECT NOTEPAD
  APPEND BLANK
  REPLACE type WITH 'B',;
          key WITH lcOrderNo,;
          cDesc WITH "Notes For Order Number : "+lcOrderNo,;
          mnotes WITH SPACE(3) + IIF(!EMPTY(oAriaApplication.user_id),oAriaApplication.user_id,"");
                                          + SPACE(1);
                                          + dTOC(oAriaApplication.SystemDate);
                                          + CHR(10)+lcNotes
  gfAdd_Info('NOTEPAD',loFormSet.DataSessionID)                                        
ENDIF 
SELECT(lnAliasSel )  