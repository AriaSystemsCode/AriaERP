*!**************************************************************************
*! Name      : RELMAIN.PRG                           
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 03/29/2004
*! Purpose   : Main program for RELIQ, INC.( REL01 )                   
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*! Modifications : 
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:**************************************************************************
*:* Name        : lfADCHRG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/08/2004
*:* Purpose     :  Update total charges field with the Additional charges custom field for REL01
*:***************************************************************************
*:* Called from : lpSavScr
*:***************************************************************************
*C037814,1
FUNCTION lfADCHRG
PRIVATE lnSlct
lnSlct = SELECT()
SELECT (lcInvHdr)
DO CASE
CASE lfChkCuPrg('ARDINV')
  laData[38] = laData[38] + &lcInvHdr..NMORECHRGS
  SHOW GET laData[38]

CASE lfChkCuPrg('ARIINV')
  M.TotalChg = M.TotalChg + &lcInvHdr..NMORECHRGS
  SHOW GET M.TotalChg
  
ENDCASE
SELECT (lnSlct)
*-- end of lfADCHRG.

*:**************************************************************************
*:* Name        : lfChkCuPrg
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/08/2004
*:* Purpose     : Check that passed program/function name is in the stack of calling programs/fucntions
*:***************************************************************************
*C037814,1
FUNCTION lfChkCuPrg
PARAMETERS lcPrg
PRIVATE lnK,llDInv
lnK = 1
llDInv = .F.
DO WHILE !EMPTY(PROGRAM(lnK))
  IF PROGRAM(lnK) = lcPrg
    llDInv = .T.
    EXIT
  ENDIF
  lnK = lnK + 1
ENDDO  
RETURN llDInv
*-- end of lfChkCuPrg.

*:**************************************************************************
*:* Name        : lfRGHTVRNM
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/08/2004
*:* Purpose     : Remove the 'm.' from the variable name lcObject
*:***************************************************************************
*:* Called from : lfNegChrg IN ARIINV.PRG
*:***************************************************************************
*C037814,1
FUNCTION lfRGHTVRNM
lcObject = STRTRAN(lcObject,'m.','')
*-- end of lfRGHTVRNM.


*:**************************************************************************
*:* Name        : lfMORCHG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/21/2004
*:* Purpose     : Define bar More Charges for REL01
*:***************************************************************************
*:* Called from : ARIINV.PRG
*:***************************************************************************
FUNCTION lfMORCHG
PRIVATE lnCntBar
lnCntBar = CNTBAR('_INQURYPOP')
*C037814,4  TMI [Start] Disable Additional charges screen if charges folder not opened
*DEFINE BAR lnCntBar+1 OF _INQURYPOP PROMPT 'Addit\<ional Charge' SKIP FOR (laScrMode[1] .OR. laScrMode[3]) .OR. &lcInvHdr..CONSOL = 'Y'
DEFINE BAR lnCntBar+1 OF _INQURYPOP PROMPT 'Addit\<ional Charge' SKIP FOR (laScrMode[1] .OR. laScrMode[3]) .OR. &lcInvHdr..CONSOL = 'Y' .OR. lnActFolder<ALEN(laFolders,1)
*C037814,4  TMI [End  ] 
ON SELECTION BAR lnCntBar+1 OF _INQURYPOP DO lfAdChgScr IN RELMAIN.PRG
*-- end of lfMORCHG.

*:**************************************************************************
*:* Name        : lfAdChgScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/22/2004
*:* Purpose     : Add more charges screen
*:***************************************************************************
FUNCTION lfAdChgScr
PRIVATE llOkPrsd
llOkPrsd = .F.
DO (gcScrDir+'AR\ARADCHG.SPX')

IF laScrMode[4] .AND. llOkPrsd

  DO CASE
  CASE lfChkCuPrg('ARDINV')  
    DO lfvFreight IN (gcAppHome+'AR\ARDINV')
    
  CASE lfChkCuPrg('ARIINV')
    llOgioTmp = .F.  &&- Define the variable llOgioTmp so that prevent program from crash
    DO lfvInvChrg IN (gcAppHome+'AR\ARIINV')
    
  ENDCASE
  
  SHOW GETS WINDOW (lcWinChrg) ONLY
  =lfRefresh(lcWinChrg)
ENDIF  


*-- end of lfAdChgScr.
*:**************************************************************************
*:* Name        : lfUpdChg
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/21/2004
*:* Purpose     : Valid function for NMORECHRGS field
*:***************************************************************************
*:* Called from : ARADCHG.SPR
*:***************************************************************************
FUNCTION lfUpdChg
PRIVATE lnSlct
lnSlct = SELECT()

IF M.NMORECHRGS < 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Negative value not allowed.')
  RETURN
ENDIF
SELECT &lcInvHdr
REPLACE NMORECHRGS WITH M.NMORECHRGS
*C037814,4  TMI [Start] Add description to additional charges
REPLACE ADCHGDSC   WITH M.ADCHGDSC
*C037814,4  TMI [End  ] 
llOkPrsd = .T.

SELECT (lnSlct)
*-- end of lfUpdChg.

*:**************************************************************************
*:* Name        : lfUPFRGHT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/21/2004
*:* Purpose     : Update freight
*:***************************************************************************
FUNCTION lfUPFRGHT
PRIVATE lnSlct , lnRecno
lnSlct = SELECT()
*-Update the user field value with the correct additional more charges
SELECT &lcHdrFile
lnRecno = RECNO()
GO TOP
m.NMORECHRGS = 0
m.ADCHGDSC = ''
*C037814,4  TMI [Start] look at order # to save correctly in consinvh file
*SCAN FOR INVOICE = m.Invoice .AND. CONSOL <> 'Y' 
SCAN FOR INVOICE = m.Invoice .AND. IIF(M.CONSOL='Y', CONSOL <> 'Y' , ORDER = M.ORDER )
  *C037814,4  TMI [End  ] 
  m.NMORECHRGS = m.NMORECHRGS + &lcHdrFile..NMORECHRGS
  m.ADCHGDSC =  IIF(M.CONSOL = 'Y' , m.ADCHGDSC + '/'  , '' ) + ALLT(&lcHdrFile..ADCHGDSC)
ENDSCAN  
lnPos = ASUBSCRIPT(laUsrFields , ASCAN(laUsrFields , 'NMORECHRGS' ) , 1 )
IF EMPTY(laUsrFields[lnPos,6])
  laUsrFields[lnPos,6] = m.NMORECHRGS
ENDIF  

*C037814,4  TMI [Start] Add 'More charges description' field
lnPos = ASUBSCRIPT(laUsrFields , ASCAN(laUsrFields , 'ADCHGDSC' ) , 1 )
IF EMPTY(laUsrFields[lnPos,6])
  laUsrFields[lnPos,6] = m.ADCHGDSC 
ENDIF  
*C037814,4  TMI [End  ] 

*C037814,4  TMI [Start] case if company is linked to GL
IF laInvSetup[1,2]='Y'
  *C037814,4  TMI [End  ] 
  DO GLDIST WITH lcCustLink,'004',-(m.Freight+m.Insur+m.Cod+m.NMORECHRGS),'IN',;
    m.Invoice,m.dPostDate,lcGlYear,lcGlPeriod,;
    lcDistFile,'',m.cCurrCode,m.nCurrUnit,m.nExRate
  *C037814,4  TMI [Start] Close the above If statement
ENDIF
*C037814,4  TMI [End  ] 

SELECT &lcHdrFile
IF lnRecno > 0 .AND. lnRecno <= RECCOUNT()
  GOTO (lnRecno)
ENDIF
  
SELECT (lnSlct)
*-- end of lfUPFRGHT.