*!**************************************************************************
*! Name      : RELMAIN.PRG
*! Developer : MMT (Mariam Mazhar)
*! Date      : 12/07/2006
*! Purpose   : REL01 Custom Process Program . (T20060927.0066)
*! C200725
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
*:**************************************************************************
*:* Name        : lfMORCHG
*:* Developer   : Mariam Mazhar [MMT]
*:* Date        : 12/07/2006
*:* Purpose     : Define bar More Charges for REL01
*:***************************************************************************
FUNCTION lfMORCHG
PRIVATE lcInvHdr ,llConsInvoice 

IF TYPE('_Screen.ActiveForm.Parent.llConslated') = 'U'
  _Screen.ActiveForm.Parent.AddProperty ('llConslated')
ENDIF 

lcInvHdr =loFormSet.lcinvhdr

_Screen.ActiveForm.Parent.llConslated = IIF(_Screen.ActiveForm.Parent.ActiveMode $ 'A',&lcInvHdr..CONSOL = 'Y',InvHdr.CONSOL = 'Y')



DEFINE BAR loFormSet.NextOptionBar OF _INQURYPOP PROMPT 'Addit\<ional Charge' SKIP FOR ;
    IIF(TYPE('_screen.ActiveForm.Parent') = 'O',(_Screen.ActiveForm.Parent.ActiveMode = 'S' ),.F.)  .OR. ;
     IIF(TYPE('_screen.ActiveForm.ariapageframe1') = "O",(_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.ActivePage <>_screen.ActiveForm.Parent.AriaForm1.ariapageframe1.pageCount),.F.) 

ON SELECTION BAR loFormSet.NextOptionBar OF _INQURYPOP _SCREEN.ActiveForm.parent.mDoTrigger(PADR('CALLSCREEN',10))
*-- end of lfMORCHG.

*:**************************************************************************
*:* Name        : lfAdChgScr
*:* Developer   : Mariam Mazhar [MMT]
*:* Date        : 12/07/2006
*:* Purpose     : Add more charges screen
*:***************************************************************************
FUNCTION lfAdChgScr
PARAMETERS loFormSetParenet

DO FORM (oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+'\ARADCHG.SCX') WITH loFormSetParenet

RETURN 
*-- end of lfAdChgScr.
*:**************************************************************************
*:* Name        : lfCALLSCREEN
*:* Developer   : Mariam Mazhar [MMT]
*:* Date        : 12/07/2006
*:* Purpose     : Add more charges screen
*:***************************************************************************
FUNCTION lfCALLSCREEN
lfAdChgScr(loFormSet)

*:**************************************************************************
*:* Name        : lfUPFRGHT
*:* Developer   : Mariam Mazhar [MMT]
*:* Date        : 12/07/2006
*:* Purpose     : Update freight
*:***************************************************************************
FUNCTION lfUPFRGHT
PRIVATE lnSlct , lnRecno


lnSlct = SELECT()
*-Update the user field value with the correct additional more charges
SELECT (lcHdrFile)
lnRecno = RECNO()
GO TOP
m.NMORECHRGS = 0
m.ADCHGDSC = ''

SCAN FOR INVOICE = m.Invoice .AND. IIF(M.CONSOL='Y', CONSOL <> 'Y' , IIF(!&lcHdrFile..Direct_Inv,ORDER = M.ORDER,.T.))
  m.NMORECHRGS = m.NMORECHRGS + &lcHdrFile..NMORECHRGS
  m.ADCHGDSC =  IIF(M.CONSOL = 'Y' , m.ADCHGDSC + '/'  , '' ) + ALLT(&lcHdrFile..ADCHGDSC)
ENDSCAN  

lnPos = ASUBSCRIPT(loformset.lauserfields , ASCAN(loformset.lauserfields, 'NMORECHRGS' ) , 1 )
IF lnPos > 0&&EMPTY(loformset.lauserfields[lnPos,6])
  loformset.lauserfields[lnPos,6] = m.NMORECHRGS
ENDIF  

lnPos = ASUBSCRIPT(loformset.lauserfields, ASCAN(loformset.lauserfields, 'ADCHGDSC' ) , 1 )
IF lnPos > 0 &&EMPTY(loformset.lauserfields[lnPos,6])
  loformset.lauserfields[lnPos,6] = m.ADCHGDSC 
ENDIF  

IF laInvSetup[1,2]='Y'
  DO GLDIST WITH lcCustLink,'004',-(m.Freight+m.Insur+m.Cod+m.NMORECHRGS),'IN',;
    m.Invoice,m.dPostDate,lcGlYear,lcGlPeriod,;
    lcDistFile,'',m.cCurrCode,m.nCurrUnit,m.nExRate

  IF (m.Freight+m.Insur+m.Cod) <> 0
    m.cFrgtAcnt = &lcDistFile..GlAccount   && Frieght account
  ENDIF   
ENDIF

        
SELECT &lcHdrFile
IF lnRecno > 0 .AND. lnRecno <= RECCOUNT()
  GOTO (lnRecno)
ENDIF
*-- end of lfUPFRGHT.
*:**************************************************************************
*:* Name        : lfADCHRG
*:* Developer   : Mariam Mazhar [MMT]
*:* Date        : 12/07/2006
*:* Purpose     :  Update total charges field with the Additional charges custom field for REL01
*:***************************************************************************
*:* Called from : lpSavScr
*:***************************************************************************
*C037814,1
FUNCTION lfADCHRG
PRIVATE lnSlct
lnSlct = SELECT()
SELECT (loformset.lcInvHdr)
REPLACE TotalChg WITH TotalChg + NMORECHRGS

IF TYPE('loformset.ariaForm1.ariapageframe1.page4') <> 'O'
  loformset.ariaForm1.ariapageframe1.page3.cntInvoiceSummary.cntTotals.txtTotalCharge.refresh()
ELSE
  loformset.ariaForm1.ariapageframe1.page4.cntInvoiceSummary.cntTotals.txtTotalCharge.refresh()
ENDIF   
SELECT (lnSlct)
*-- end of lfADCHRG.

