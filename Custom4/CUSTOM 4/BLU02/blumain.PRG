*!***************************************************************************************************************************************
*! Name      : BLUMAIN.PRG  C131527
*! Developer : Khalid Mohi El-Din
*! Date      : 05/31/2006
*! Purpose   : BLUM & FINK 
*!***************************************************************************************************************************************
*! Parameters: loFormSet -> FormSet object
*!             lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!***************************************************************************************************************************************
*! Returns   : Logical value.     
*!***************************************************************************************************************************************
*! Modifcations :
*:* E303486,1 HES get the phone value from Ordhdr.Note2 for BLU02 for only account# NOR02 [T20140414.0004]
*:* B610772,1 MMT 07/10/2014 Error while browsing serial number in style screen[T20140702.0018]
*!***************************************************************************************************************************************
PARAMETER loFormSet, lcEvntFun, lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C', lcFunPars, '')
lcFunToRun = 'lf' + ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVALUATE(lcFunToRun)

RETURN llRetValue

*!*************************************************************
*! Name      : lfPOTran C131527
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To call the serial control screen when receive PO
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfPOTran
LPARAMETERS lcGlSession

lcGlSession = PADL(lcGlSession,6,'0')
PRIVATE lnAlias
lnAlias = SELECT(0)

*WSH
*lcParam = "'R','6','" + lcGlSession + "'"
DIMENSION laSessNos[1,3]
laSessNos[1,1] = 'R'
laSessNos[1,2] = lcGlSession
laSessNos[1,3] = ''

lcParam = "'R','6','laSessNos',.F."
*WSH

oAriaApplication.DoProgram('AWRICSRCTR', lcParam , .F., 'IC')

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfARTRAN C131527
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To call the serial control screen from Invoice
*!*************************************************************
*! Parameters: lcSessionNo : Session number
*!*************************************************************
FUNCTION lfARTRAN
LPARAMETERS lcSessionNo

lcSessionNo = PADL(lcSessionNo,6,'0')

LOCAL lnAlias
lnAlias = SELECT(0)

DIMENSION laSessNos[1,3]
laSessNos[1,1] = 'I'
laSessNos[1,2] = lcSessionNo
laSessNos[1,3] = ''

lcParam = "'I','3','laSessNos',.F."
oAriaApplication.DoProgram('AWRICSRCTR', lcParam , .F., 'IC')

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfARVOID C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : To call the serial control screen from Void Invoice
*!*************************************************************
*! Parameters: lcSessionNo : Session number
*!*************************************************************
FUNCTION lfARVOID
LPARAMETERS lcSessNo

lcSessNo = PADL(lcSessNo,6,'0')

LOCAL lnAlias
lnAlias = SELECT(0)

DIMENSION laSessNos[1,3]
laSessNos[1,1] = 'R'
laSessNos[1,2] = lcSessNo
laSessNos[1,3] = ''

lcParam = "'R','4','laSessNos',.T."
oAriaApplication.DoProgram('AWRICSRCTR', lcParam , .F., 'IC')

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfCRTRAN C131527
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To call the serial control screen from Credit Memo
*!*************************************************************
*! Parameters: lcSessionNo : Session number
*!*************************************************************
FUNCTION lfCRTRAN
LPARAMETERS lcSessionNo

lcSessionNo = PADL(lcSessionNo,6,'0')

LOCAL lnAlias
lnAlias = SELECT(0)

DIMENSION laSessNos[1,3]
laSessNos[1,1] = 'R'
laSessNos[1,2] = lcSessionNo
laSessNos[1,3] = ''

lcParam = "'R','7','laSessNos',.F."
oAriaApplication.DoProgram('AWRICSRCTR', lcParam , .F., 'IC')

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfCRVOID C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : To call the serial control screen from Void Credit Memo
*!*************************************************************
*! Parameters: lcSessionNo : Session number
*!*************************************************************
FUNCTION lfCRVOID
LPARAMETERS lcSessionNo

lcSessionNo = PADL(lcSessionNo,6,'0')

LOCAL lnAlias
lnAlias = SELECT(0)

DIMENSION laSessNos[1,3]
laSessNos[1,1] = 'I'
laSessNos[1,2] = lcSessionNo
laSessNos[1,3] = ''

lcParam = "'I','8','laSessNos',.T."
oAriaApplication.DoProgram('AWRICSRCTR', lcParam , .F., 'IC')

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfRATran C131527
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 05/30/2006
*! Purpose   : To call the serial control screen save RA
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfRATran
LPARAMETERS lcRaNo

LOCAL lnAlias
lnAlias = SELECT(0)

DIMENSION laSessNos[1,3]
laSessNos[1,1] = 'R'
laSessNos[1,2] = ''
laSessNos[1,3] = PADL(lcRaNo, 6, '0')

lcParam = "'R','0','laSessNos',.F."
oAriaApplication.DoProgram('AWRICSRCTR', lcParam , .F., 'IC')

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfICTran C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : To call the serial control screen from IC
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfICTran
LPARAMETERS lcICCursor

LOCAL lnAlias, lcTrnType, lnArrIndx
lnAlias   = SELECT(0)
lnRecNo   = IIF(EOF(lcICCursor), 0, RECNO(lcICCursor))
lnArrIndx = 0

DIMENSION laSessNos[1,3]
STORE '' TO laSessNos

SELECT (lcICCursor)
LOCATE

DO CASE
  CASE Type $ 'AT'
    lcTrnType = '1'
  CASE Type = 'P'
    lcTrnType = '2'
ENDCASE

SELECT (lcICCursor)
SCAN
  lnArrIndx = lnArrIndx + 1
  DIMENSION laSessNos[lnArrIndx,3]
  
  DO CASE
    CASE Type = 'A'
      laSessNos[lnArrIndx,1] = IIF(TotAdj < 0, 'I', 'R')
      laSessNos[lnArrIndx,2] = cSession
      laSessNos[lnArrIndx,3] = ''
      
    CASE Type = 'T'
      laSessNos[lnArrIndx,1] = 'I'
      laSessNos[lnArrIndx,2] = cIssSess
      laSessNos[lnArrIndx,3] = ''
      
      lnArrIndx = lnArrIndx + 1
      DIMENSION laSessNos[lnArrIndx,3]
      
      laSessNos[lnArrIndx,1] = 'R'
      laSessNos[lnArrIndx,2] = cSession
      laSessNos[lnArrIndx,3] = ''
      
    CASE Type = 'P'
      laSessNos[lnArrIndx,1] = 'B'
      laSessNos[lnArrIndx,2] = cSession
      laSessNos[lnArrIndx,3] = ''
      
  ENDCASE
ENDSCAN

SELECT (lcICCursor)
IF !EMPTY(lnRecNo)
  GOTO (lnRecNo)
ENDIF

lcParam = "'B','" + lcTrnType + "','laSessNos'," + IIF(lcTrnType = '2', ".T.", ".F.")
oAriaApplication.DoProgram('AWRICSRCTR', lcParam , .F., 'IC')

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfADDMPAR C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Add a menu PAR in the Options Menu for Serial
*!             Lookup screen
*!*************************************************************
*! Example   : =lfADDMPAR()
*!*************************************************************
FUNCTION lfADDMPAR

LOCAL lnBarNo

*--Check if the Options Pad Menu was already defined
IF !lfFoundPad('Option')
  DEFINE PAD _Option OF (loFormSet.chostformname) PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF (loFormSet.chostformname) ACTIVATE POPUP _lPopOpt
  
  *--Define option pasd bars.
  DEFINE POPUP _lPopOpt MARGIN SHADOW
ENDIF
lnBarNo = CNTBAR('_lPopOpt') + 1

*--Add menu Bar for Serial Lookup Screen
IF lnBarNo > 1
  DEFINE BAR lnBarNo OF _lPopOpt PROMPT '\-'
  lnBarNo = lnBarNo + 1
ENDIF

DEFINE BAR lnBarNo OF _lPopOpt PROMPT '\<Serial Lookup'
ON SELECTION BAR lnBarNo OF _lPopOpt _SCREEN.ActiveForm.parent.mDoTrigger(PADR('RUNSRLKP',10))

*--Activate Options Menu
SET SKIP OF PAD _Option OF (loFormSet.chostformname)  TYPE('_screen.ActiveForm.Parent') = 'O' AND ;
         TYPE("_screen.ActiveForm.Parent.cHostFormName") = "C" AND _SCREEN.ActiveForm.parent.ActiveMode = 'S'

RETURN

*!*************************************************************
*! Name      : lfRemoveOpt C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Release Options Menu
*!*************************************************************
*! Example   : =lfRemoveOpt()
*!*************************************************************
FUNCTION lfRemoveOpt

*--Check if the Options Pad Menu was not Release before
IF lfFoundPad('Option')
  RELEASE PAD _Option OF (loFormSet.chostformname)
ENDIF
*-- end of lfRemoveOpt.

*!*************************************************************
*! Name      : lfFoundPad C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
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

*!*************************************************************
*! Name      : lfRUNSRLKP C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Runs Serial Lookup screen
*!*************************************************************
*! Example   : =lfRUNSRLKP()
*!*************************************************************
FUNCTION lfRUNSRLKP
*:* B610772,1 MMT 07/10/2014 Error while browsing serial number in style screen[T20140702.0018][Start]
*DO (oAriaApplication.ProgramHome + "ICSRLKUP" ) WITH loFormSet
DO (oAriaApplication.clientprogramhome + "ICSRLKUP" ) WITH loFormSet
*:* B610772,1 MMT 07/10/2014 Error while browsing serial number in style screen[T20140702.0018][End]
*-- end of lfRUNSRLKP.

*:**************************************************************************
*: Name      : lfStyInit C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*: Purpose   : Inisialize method of Style screen
*:***************************************************************************
*: Example   : =lfStyInit()
*:***************************************************************************
FUNCTION lfStyInit

=gfOpenTable(oAriaApplication.DataDir+'ICSTYSER',oAriaApplication.DataDir+'CSTYLE','SH')

loFormSet.AddProperty('llSRMaintain', .F.)
*-- end of lfStyInit.

*!**************************************************************************
*! Name      : lfUPDDEPFL C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Update Maintain Serial File
*!**************************************************************************
*! Example   : lfUPDDEPFL()
*!**************************************************************************
FUNCTION lfUPDDEPFL

LOCAL lnAlias
lnAlias = SELECT(0)

SELECT ICSTYSER
IF !gfSEEK(loFormSet.lcStyMajor)
  m.cStyMajor = loFormSet.lcStyMajor
  =gfAppend("ICSTYSER", .T.)
ENDIF
REPLACE lSRMainTan WITH loFormSet.llSRMaintain

=gfAdd_info('ICSTYSER', loFormSet)
=gfReplace()
=gfTableUpdate()

SELECT (lnAlias)
*-- end of lfUPDDEPFL.

*!**************************************************************************
*! Name      : lfDELDPSTY C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : To delete the Maintain Serial Record
*!**************************************************************************
*! Example   : lfDELDPSty()
*!**************************************************************************
FUNCTION lfDELDPSTY

LOCAL lnAlias
lnAlias = SELECT(0)

SELECT ICSTYSER
IF !SEEK(loFormSet.lcStyMajor, "STYLE") AND gfSEEK(loFormSet.lcStyMajor)
  =gfDelete()
  =gfTableUpdate()
ENDIF

SELECT (lnAlias)
*-- end of lfDELDPSTY.

*!*************************************************************
*! Name      : lfDISABEGRP C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : ChangeMode event in Style Screen
*!*************************************************************
*! Example   : =lfDISABEGRP()
*!*************************************************************
FUNCTION lfDISABEGRP

*-- Activate Options Menu
SET SKIP OF PAD _Option OF (loFormSet.chostformname) _SCREEN.ActiveForm.parent.ActiveMode = 'S'
*-- end of lfDISABEGRP.

*!*************************************************************
*! Name      : lfGETSTYINFO C131527
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/19/2006
*! Purpose   : Get Style Information
*!*************************************************************
*! Example   : =lfGETSTYINFO()
*!*************************************************************
FUNCTION lfGETSTYINFO

LOCAL lnAlias
lnAlias = SELECT(0)

SELECT ICSTYSER
IF gfSEEK(loFormSet.lcStyMajor)
  loFormSet.llSRMaintain = lSRMainTan
ELSE
  loFormSet.llSRMaintain = .F.
ENDIF

SELECT (lnAlias)
*-- end of lfDISABEGRP.

*:* E303486,1 HES get the phone value from Ordhdr.Note2 for BLU02 for only account# NOR02 [START]
*:****************************************************************************************
*! Name      : lfGETPHONE
*! Developer : Hesham Elmasry (HES)
*! Date      : 29-06-2014
*! Purpose   : Get phone# from Ordhdr.Note2 instead of Ordhdr.phone
*:****************************************************************************************
*! Called from : Process-carrier-integration class
*:****************************************************************************************
FUNCTION lfGETPHONE
  IF m.Account = "NOR02"
    m.SHIPTO_PHONE = ALLTRIM(STRTRAN(ORDHDR.note2,"Telephone Number :",""))
  ENDIF
ENDFUNC
*:* E303486,1 HES get the phone value from Ordhdr.Note2 for BLU02 for only account# NOR02 [END  ]

