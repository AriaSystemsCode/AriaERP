*!**************************************************************************
*! Name      : OSPMAIN.PRG
*! Developer : Mariam Mazhar[MMT]
*! Date      : 11/25/2010
*! Purpose   : Custom triggers for OSP00 customer (C201291(A40),C201292(A27),C201293(A40)) 
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Modifications:
PARAMETER loFormSet,lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:**************************************************************************
*: Name        : lfDFNOPTNMNU
*! Developer   : Mariam Mazhar[MMT]
*! Date        : 11/25/2010
*: Purpose     : Add the custom options to the RA screen 
*:***************************************************************************
FUNCTION lfDFNOPTNMNU
DEFINE PAD _INQUIRY OF (loFormSet.chostformname) PROMPT 'Options' KEY ALT+P , ' '
*SET SKIP OF PAD _INQUIRY OF (loFormSet.chostformname) (_screen.ActiveForm.Parent.ActiveMode='S')
ON PAD _INQUIRY OF (loFormSet.chostformname) ACTIVATE POPUP _INQURYPOP
lcHostFormName = '[' + loFormSet.cHostFormName + ']'
DEFINE POPUP _INQURYPOP MARGIN SHADOW

DEFINE BAR 1  OF _INQURYPOP PROMPT '\<Update Vendor PO#'  SKIP FOR ;
									gfFormIsActive(&lcHostFormName) .AND.;
									(INLIST(_screen.ActiveForm.Parent.ActiveMode,'S','V')) ;
									OR ;
									(IIF(TYPE('_screen.ActiveForm.pageFrame') = "O",;
									(_screen.ActiveForm.pageFrame.ActivePage<>2) ,.T.)) OR;
									(IIF(TYPE('_screen.ActiveForm.pageFrame') = "O" AND _screen.ActiveForm.pageFrame.ActivePage =2,EMPTY(_screen.ActiveForm.Parent.ariaForm1.pageFrame.page2.cntStyle.value),.T.))
									
ON SELECTION BAR 1 OF _INQURYPOP DO lfCallScr IN ospmain.fxp 

*:**************************************************************************
*: Name        : lfCallScr
*! Developer   : Mariam Mazhar[MMT]
*! Date        : 11/25/2010
*: Purpose     : call the custom screen from the options menu of the RA screen 
*:***************************************************************************
FUNCTION lfCallScr

IF oAriaApplication.MULTIINST 
  =gfCallForm('RMRAPO','RM',_Screen.ActiveForm.Parent)
ELSE
  DO FORM (oAriaApplication.ScreenHome + 'RM\rmrapo.scx') WITH _Screen.ActiveForm.Parent
ENDIF  

*:**************************************************************************
*: Name        : lfFormPOInit
*! Developer   : Mariam Mazhar[MMT]
*! Date        : 11/25/2010
*: Purpose     : init of the custom screen called from the options menu of the RA screen 
*:***************************************************************************
FUNCTION lfFormPOInit
PARAMETERS loBranchFormSet
SET DATASESSION TO loBranchFormSet.loParentForm.DataSessionId
IF !USED('POSHDR')
  =gfOpenTable('POSHDR','POSHDR','SH')
ENDIF

IF !USED('APVENDOR')
  =gfOpenTable('APVENDOR','VENCODE','SH')
ENDIF
IF !EMPTY(loBranchFormSet.loParentForm.prg.lcTmpRetLn) AND  USED(loBranchFormSet.loParentForm.prg.lcTmpRetLn)
  IF !EMPTY(EVALUATE(loBranchFormSet.loParentForm.prg.lcTmpRetLn+'.PO'))
    loBranchFormSet.ariaForm1.kbpoNo.keytextbox.Value = EVALUATE(loBranchFormSet.loParentForm.prg.lcTmpRetLn+'.PO')
    IF !EMPTY(loBranchFormSet.ariaForm1.kbpoNo.keytextbox.Value)
      IF gfSeek('PP'+loBranchFormSet.ariaForm1.kbpoNo.keytextbox.Value,'POSHDR')  
        =gfSeek(POSHDR.VENDOR,'APVENDOR')
        loBranchFormSet.ariaForm1.txtVendName.Value = apvendor.cvencomp 
      ENDIF   
    ENDIF
  ENDIF
ENDIF
*:**************************************************************************
*: Name        : lfvPONum
*! Developer   : Mariam Mazhar[MMT]
*! Date        : 11/25/2010
*: Purpose     : Validate PO# Entered by user
*:***************************************************************************
FUNCTION lfvPONum
LPARAMETERS loBranchFormSet

IF (!EMPTY(loBranchFormSet.ariaForm1.kbpoNo.keytextbox.Value) AND !gfSeek('PP'+loBranchFormSet.ariaForm1.kbpoNo.keytextbox.Value,'POSHDR')) OR loBranchFormSet.ariaForm1.kbpoNo.selectedfrombrowse 
  SELECT POSHDR
  =gfSeek('PP')
  DIMENSION laSelecPO[1]
  lcBrFields = "PO :R :H= 'PO#',Status:R :H= 'Status'"+;
      			   ",Vendor :R :H= 'Vendor',Entered :R :H= 'Entered',Complete :R :H= 'Complete'"	


  =AriaBrow("'PP' FOR CSTYTYPE = 'P' and CBUSDOCU = 'P'",'PO', gnbrfsrow1, gnbrfscol1,;
            gnbrfsrow2, gnbrfscol2, '','',;
            "PO",'laSelecPO')
    
  loBranchFormSet.ariaForm1.kbpoNo.selectedfrombrowse  = .F.
  IF !EMPTY(laSelecPO[1])
    loBranchFormSet.ariaForm1.kbpoNo.keytextbox.Value = laSelecPO[1]
  ELSE
    loBranchFormSet.ariaForm1.kbpoNo.keytextbox.Value = ''
  ENDIF   			   
ENDIF 

IF !EMPTY(loBranchFormSet.ariaForm1.kbpoNo.keytextbox.Value)
  =gfSeek(POSHDR.VENDOR,'APVENDOR')
  loBranchFormSet.ariaForm1.txtVendName.Value = apvendor.cvencomp 
ENDIF

*:**************************************************************************
*: Name        : lfADDPOCOL
*! Developer   : Mariam Mazhar[MMT]
*! Date        : 11/25/2010
*: Purpose     : Add PO# Column to the RA Screen detail grid
*:***************************************************************************
FUNCTION lfADDPOCOL
lnCol = loFormSet.ariaForm1.pageFrame.page2.grdRALines.ColumnCount
loFormSet.ariaForm1.pageFrame.page2.grdRALines.AddColumn (loFormSet.ariaForm1.pageFrame.page2.grdRALines.ColumnCount+1)
loFormSet.Prg.lfAddGridCol(@lnCol,"PO","Vendor PO#",,100)

*:**************************************************************************
*: Name        : lfvUpPONum
*! Developer   : Mariam Mazhar[MMT]
*! Date        : 11/25/2010
*: Purpose     : Update user entered PO# in RA Screen temp. RALINE
*:***************************************************************************
FUNCTION lfvUpPONum
LPARAMETERS loBranchFormSet
REPLACE PO WITH loBranchFormSet.ariaForm1.kbpoNo.keytextbox.Value,;
		CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS) IN (loBranchFormSet.loParentForm.prg.lcTmpRetLn)

*:**************************************************************************
*: Name        : lfCALLPOSCR
*! Developer   : Mariam Mazhar[MMT]
*! Date        : 11/25/2010
*: Purpose     : Call the customer screen after user Sizes qty entery
*:***************************************************************************
FUNCTION lfCALLPOSCR
IF oAriaApplication.MULTIINST 
  =gfCallForm('RMRAPO','RM',_Screen.ActiveForm.Parent)
ELSE
  DO FORM (oAriaApplication.ScreenHome + 'RM\rmrapo.scx') WITH _Screen.ActiveForm.Parent
ENDIF  
_Screen.ActiveForm.Parent.ariaForm1.pageFrame.page2.cmdNew.SetFocus()