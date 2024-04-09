*:************************************************************************
*:
*: Procedure file: APAPLDB.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Renee Ezzat
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfDataShow
*:               : lfvData
*:               : lfActBrow
*:               : lfwBroWhen
*:               : lfvBrowse
*:               : lfSeekRec
*:               : lfvAppFull
*:               : lfApply
*:               : lfvAppPart
*:               : lfvAppAll
*:               : lfApplDbMm
*:               : lfvApply
*:               : lfwDates
*:               : lfvAppDate
*:               : lfwAmnt
*:               : lfwOldVals
*:               : lfvAplAmnt
*:               : lfvDscAmnt
*:               : lfwDisAcct
*:               : lfvDisAcct
*:               : lfvInvScp
*:               : lfvInvRef
*:               : lfvInvOk
*:               : lfvPayPr
*:               : lfvDivision
*:               : lfvPayMeth
*:               : lfvDueDate
*:               : lfvDscDate
*:               : lfvDates
*:               : lfVldAppDt
*:               : lfRefrObj
*:               : lpTab
*:               : lpShiftTab
*:               : lfBrowTrap
*:               : lfBrowUnTrap
*:               : lpCtrlEntr
*:               : lpBrowKeyTrap
*:               : lfActBrows
*:               : lfDefinePad
*:               : lfActivate
*:               : lfDeact
*:               : lfGetPayMeth
*:               :
*:
*:  Documented      /  /
*:************************************************************************
*B600492,1 Reh  06/22/95 Change the browse fields width under windows.
*E300296,1 M.H  10/10/95 Add the currency to the AP module.
*E300316,1 HISH 11/28/95 Used global function gfGetExSin to get currency 
*E300316,1               equation signs.
*E300316,1 HISH 01/08/96 Passed pointer parameter to get Unit sgin.
*B600928,1 M.H 01/14/96  1-Fix the bug of the pointer in the detail brow always point to the first record.
*B600928,1 M.H 01/14/96  2-Fix the bug of the disable the apply buttons when there is no records to display.
*B601061,1 RENEE 05/09/96. 1. Locking fields of an applied debit memo and
*B601061,1                    a target invoice are not cleared if apply fully.
*B601061,1                 2. If an invoice is already locked, its message
*B601061,1                    does not show the invoice number   
*B800702,1 M.H 08/04/96 Clear the bank,checking account and theGL account 
*B800702,1              when clearing the invoice and the debit memo.
*B601304,1 RENEE 10/29/96. Add the following statement :
*B601304,1                   #WCLAUSES IN WINDOW Fndation 
*B601304,1                 in the screen setup code of APAPLDB.SCX
*B800815,1 Haytham 11/14/96 Adding confirm dialogs to apply debits
*B601526,1  HS  01/19/97  Add the new field [nInvFAAp] in the APINVHDR file 
*B601526,1                [the approved amount in the approve currency] 
*E300643,1  HS 04/14/97 1)Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300643,1              2)Make some changes for we have change the function
*E300643,1              [gfCodDes]
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,1 AMM 06/04/97 Add screens directory path to the calling of SPRS
*B801196,4 AMM 08/11/97 Optimizing Speed
*B601954,4 RENEE 08/06/1998 Attempt to fix the problem of not updating
*B601954,4 					the debit memo records in some cases.
*E301077,79 AMM 03/04/99 Enhance opening and closing files.
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
*B603676,1 SSE 06/08/2000 Apply Debit doesn't add record in ApDist file in case
*B603676,1                currency exch. difference between Debit Memo and Invoice
*B602006,1 AAN 04/09/2000 Move the pointer to the next line in the detail brow after the user click apply fully
*B608103,1 NNA 05/28/2007 Fix bug that Vendor Balance Doesn't Take Account of Exchange Differencecs , if there is a difference
*B608103,1 NNA            between the Ap invoice Exch. Rate and the Debit Memo Exch. Rate.
*B608348,1 NNA 11/12/2007 fix bug of that system is leaving Payable invoices open even it's actually paid that by
*B608348,1 NNA            changing the message “You are editing the same record in another session”  to remove the overwrite
*B608348,1 NNA            Choice and replace it with the other massege that have choices “Retry” & “Cancel”
*B608942,1 TMI 07/22/2009 if a record is used by a user check that this user is in SYUSTATC of both A27 and A4XP [T20090224.0001]
*B608942,3 TMI 08/19/2009 adjust the locking process to meet the ticket requerments
*B609026,1 TMI 10/01/2009 fix locking problems arised after program installed at the customer[T20090224.0001]
*:************************************************************************
*
EXTERNAL ARRAY laData
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
* I incresed the laBrHdrs1 from 6 to 7 to add the currency code to the browse fields.
DECLARE laPayMeth[1,2],laBrHdrs1[7],laBrHdrs2[8],laRemitTo[3],laData[3,1]
** lcApDbWin1   Variable hold to child window name no 1
** lcApDbWin2   Variable hold to child window name no 2
** lcApDbWin3   Variable hold to child window name no 3
** lcApDbWin4   Variable hold to child window name no 4
** lcBrTtl1     Variable hold to browse title
** lcBrTtl2     Variable hold to browse title
** lcVnInqTtl   Variable hold to  
** lcDmInqTtl   Variable hold to 
** lcInInqTtl   Variable hold to 
** lcApDbSess   Debit session number
** lc_InvHdr    Alias name for the APINVHDR file
** lcInvFlt     Filter expression for invoices browse 
** lcInvTagEx   Variable hold to 
** lcOldInFlt   Variable hold to 
** lcDateFlt    Variable hold to 
** lcPMethOpt   Variable hold to 
** lcInvFlt_1   Variable hold to 
** lcScFields   Variable hold to 
** lcPayPrior   Variable hold to 
** lcPPrFltEx   Variable hold to 
** lcPPrEx      Variable hold to 
** lcRefFltEx   Variable hold to 
** lcDisabClr   Variable hold to 
** lcDivOpt     Variable hold to 
** lcTBrowse    Variable hold to 
** lcDiscAcct   discount account
** lcAccDesc    discount account description
** lcFisFYear   fiscal year
** lcFspPrdId   fiscal period
** lcDebMem     current debit memo number
** lcInvoice    current invoice number
** lcOldVal     Variable hold to 
** lcDivision   Variable hold to 
** lcInvRef     Variable hold to 
** lcPayMeth    Variable hold to 
** lcSayCol     Variable hold to 
** lcColPair    Color of Get fields of 'User Winds'
** lcTopLeft    Variable hold to 
** lcBotRight   Variable hold to 
** lcBackGr     Variable hold to 
** lcTopLine    Variable hold to 
** lcBotLine    Variable hold to 
*E300316,1 HISH 11/28/95 Added currency equation sign variables. (Begin)
** lcExSin1             Variable to hold the first sign in the equation.
** lcExSin2             Variable to hold the second sign in the equation.
*E300316,1 (End)
** 
** lnApplAmnt   amount to be applied
** lnDiscAmnt   discount to be applied
** lnPAplAmnt   partial amount to be applied is now 0.
** lnPAplDisc   partial disacount to be applied is now 0.
** lnBrRecN1    Variable hold to  
** lnBrRecN2    Variable hold to 
** lnPayMLen    Variable hold to 
** lnYTDTotPay  Variable hold to 
** lnOldrbScp   Variable hold to 
**
** ldAppDate    Variable hold to 
** ldOldDate    Variable hold to 
** ldOldAppDt   Variable hold to 
** ldCurrDate   Variable hold to 
** ldDueFrom    Variable hold to 
** ldDueTo      Variable hold to 
** ldDiscFrom   Variable hold to 
** ldDiscTo     Variable hold to 
**
** llBrowse     Variable hold to 
** llPassDate   .T. date validation has been passed at least once.
** llDisObj     Variable hold to 
** llDisVen     Variable hold to 
** llDisScp     Variable hold to 
** llDisDM      Variable hold to 
** llDisInv     Variable hold to 
** llNoContrl   Variable hold to 
**
** puPayMeth    Variable hold to 
** puDivision   Variable hold to 
** puDivision   Variable hold to 
**
** rbDates      Variable hold to 
** laCtrStat    Variable hold to 

lcTopLeft   = 'RGB(128,128,128,128,128,128)'   &&Dark grey
lcBotRight  = 'RGB(255,255,255,255,255,255)'   &&White 
lcBackGr    = 'RGB(192,192,192,192,192,192)'   &&Light grey

STORE .F.    TO llBrowse   , llPassDate

STORE .T.    TO llDisObj   , llDisVen   , llDisScp   , llDisDM    ,;
                llDisInv   , llNoContrl

STORE 0      TO lnApplAmnt , lnDiscAmnt , lnPAplAmnt , lnPAplDisc ,;
                lnBrRecN1  , lnBrRecN2  , lnPayMLen  , lnYTDTotPay

STORE 1      TO lnOldrbScp , rbDates    , puPayMeth  , puDivision

STORE {}     TO ldAppDate  , ldOldDate  , ldOldAppDt , ldCurrDate ,;
                ldDueFrom  , ldDueTo    , ldDiscFrom , ldDiscTo

STORE ""     TO puDivision

STORE " "    TO lcApDbWin1 , lcApDbWin2 , lcApDbWin3 , lcApDbWin4 ,;
                lcBrTtl1   , lcBrTtl2   , lcVnInqTtl , lcDmInqTtl ,;
                lcInInqTtl , lcApDbSess , lc_InvHdr  , lcInvFlt   ,;
                lcInvTagEx , lcOldInFlt , lcDateFlt  , lcPMethOpt ,;
                lcInvFlt_1 , lcScFields , lcPayPrior , lcPPrFltEx ,;
                lcPPrEx    , lcRefFltEx , lcDisabClr , lcExSin1   ,;
                lcExSin2

STORE SPACE(2)    TO lcFspPrdId
STORE SPACE(4)    TO lcFisFYear
STORE SPACE(8)    TO lcOldVal
STORE SPACE(12)   TO lcDebMem   , lcInvoice
STORE SPACE(15)   TO lcInvRef
STORE SPACE(30)   TO lcDivision , lcPayMeth
STORE SPACE(65)   TO lcAccDesc
STORE "*"         TO lcDivOpt
STORE lcBackGr    TO lcTopLine  , lcBotLine
STORE "\<Browse"  TO lcTBrowse
STORE 'DISABLE'   TO laCtrStat 
STORE lcEmptyAcc  TO lcDiscAcct
STORE lcHidObjNrm TO lcSayCol
STORE SCHEME(1,2) TO lcColPair

IF !gfSetup()
  RETURN
ENDIF

IF !EMPTY(laArrayes[1,2])
  *E301077,79 AMM Open file
  *MAN Added Private and used gfOpenFile instead of gfSysOpen
  PRIVATE llOpfld
  *llOpfld = gfSysOpen(gcSysHome+'SYDFIELD','CFLD_NAME','SH')
  llOpfld = gfOpenFile(gcSysHome+'SYDFIELD','CFLD_NAME','SH')
  *E301077,79 AMM end
  FOR lnArayNo = 1 TO ALEN(laArrayes,1)
    DECLARE &laArrayes[lnArayNo,2] [1,2]
    &laArrayes[lnArayNo,2] = " " 
    =SEEK(laArrayes[lnArayNo,1],"SYDFIELD")
    lcVString = ALLTRIM(sydfield.mventries)
    =gfSubstr(lcVString,@&laArrayes[lnArayNo,2],"|~")
  ENDFOR
  *E301077,79 AMM close file
  IF llOpfld
    =gfCloseFile('SYDFIELD')
  ENDIF
  *E301077,79 AMM end
  
ENDIF

*MAN Added Private and used gfOpenFile instead of gfSysOpen
PRIVATE llOpComp
*E301077,79 AMM Open file
*llOpComp = gfSysOpen(gcSysHome+'SYCCOMP','CCOMP_ID','SH')
llOpComp = gfOpenFile(gcSysHome+'SYCCOMP','CCOMP_ID','SH')
*E301077,79 AMM end

lcCurrYear  = IIF(SEEK(gcPrnt_Cmp,'SYCCOMP'),SYCCOMP.cCurr_Yer," ")               && current year
*E301077,79 AMM Close file
IF llOpComp
  =gfCloseFile('SYCCOMP')
ENDIF
*E301077,79 AMM end

** Array laFields is formed as follows :
** A row for every field to be browsed
** Column 1 : the object name corresponding to the field
** Column 2 : the physical field name
DECLARE laFields[3,3]
laFields[1,1] = 'laData[1]'         &&'lcVendCode'
laFields[1,2] = 'cVendCode'
laFields[1,3] = 'VENCODE'
laFields[2,1] = 'laData[2]'         && 'lcVenComp'
laFields[2,2] = 'cVenComp'
laFields[2,3] = 'VENCOMP'
laFields[3,1] = 'laData[3]'         &&'lcPhoneNo'
laFields[3,2] = 'cPhoneNo'
laFields[3,3] = 'VENPHONE'

DECLARE laWndObj   [5,3]

IF !WEXIST(gcBaseWind)	
  ** Create temporary names for child windows, and aliases.
  lcApDbWin1 = gfTempName()
  lcApDbWin2 = gfTempName()
  lcApDbWin3 = gfTempName()
  lcApDbWin4 = gfTempName()
  lc_InvHdr  = gfTempName()

  ** Get payment methods array
  ** Prepare Payment method array from SYDFIELD and get its maximum width
  ** Remove 'Credit cards' (C) option from the array
  lnPayMLen  = gfGetVld('CVENPMETH',@laPayMeth,.T.)
  ** Remove 'Credit cards' (C) option from the array
  lcSetExact = SET('EXACT')
  SET EXACT ON
  lnElem = ASCAN(laPayMeth, 'C')
  IF lnElem > 0 
    =ADEL(laPayMeth, ASUBSCRIPT(laPayMeth, lnElem, 1))
    DIMENSION laPayMeth[ALEN(laPayMeth,1)-1,2]  
  ENDIF
  SET EXACT &lcSetExact

  lcPayMeth  = laPayMeth[1,1]

  *E300643,1 Change this line for the changes we have made 
  *          to (gfCodDes) [Begin]
  *lcDivision = gfCodDes('*')
  lcDivision = gfCodDes('*' , ' ')
  *E300643,1 Change this line [End]  
  
  ** Parent company ID ( same if the current company is a parent company)
  ** variable gcPrnt_Cmp holds the parent company IS if gcAct_Comp
  ** is a child, or is empty if gcAct_Comp is itself a parent.
  ** This company is then used for date validations from its
  ** fiscal calendar.
  ldCurrDate = DATE()
  
  ** Prepare a debit memo application session number for the 
  ** current session if one does not already exist.
  IF EMPTY(lcApDbSess)

    *E300663,1 Change this line for the changes we have made to 
    *          (gfSequence) [Begin]
    *lcApDbSess = PADL(ALLTRIM(STR(gfSequence('APSESS',1))),;
    *                          FSIZE('cApSessNo','APDIST'),'0')
    lcApDbSess = gfSequence('CAPSESSNO')
    *E300663,1 Change this line [End]
    
  ENDIF

  ** Tag expression of APVENHST.DBF (VENDYEAR)
  ** is Vendor code + fiscal year (cVendCode + cFisfYear)
  SET ORDER TO TAG VENDYEAR IN APVENHST

  ** Use APINVHDR file again in another work area using alias (lc_InvHdr)
  SELECT 0  
  USE &gcDataDir.APINVHDR AGAIN ALIAS (lc_InvHdr) ORDER TAG VENDINV
  lcInvTagEx       = SYS(14,VAL(SYS(21)))
    
  SELECT APINVHDR
  
  SET ORDER TO TAG VENDINV
  
  ** Set relations between vendors file (APVENDOR) as parent, 
  ** with the invoice file and its used again copy as children
  SELECT APVENDOR

  ** Default filter is on 'All invoices'
  lcDateFlt  = ' '
  lcInvFlt_1 = [ ((nInvAmnt - nInvPaid - nInvDistK - nInvAdj) > 0 ) ]+;
               [.AND. cInvStat <> 'V'] 
               
  lcInvFlt   = lcInvFlt_1
  lcOldInFlt = lcInvFlt
  lcScFields = "cVendCode, cVenComp, cPhoneNo"
  SCATTER FIELDS &lcScFields TO laData BLANK
ELSE
  rbInvScope      = lnOldrbScp
ENDIF  

laWndObj [1,1] = lcApDbWin1
laWndObj [1,2] = "ibLaData1"
laWndObj [1,3] = "pbClose"

laWndObj [2,1] = lcApDbWin4
laWndObj [2,2] = "ibLaData1"
laWndObj [2,3] = "pbClose"

laWndObj [3,1] = "CWRAPAPLVN"
laWndObj [3,2] = "pbVnClose"
laWndObj [3,3] = "pbVnClose"

laWndObj [4,1] = "CWRAPAPLDM"
laWndObj [4,2] = "pbDmClose"
laWndObj [4,3] = "pbDmClose"

laWndObj [5,1] = "CWRAPAPLIN"
laWndObj [5,2] = "pbInClose"
laWndObj [5,3] = "pbInClose"

SELECT APINVHDR
SET ORDER TO 0
SET FILTER TO
SET FILTER TO &lcInvTagEx = laData(1) .AND. ;
               ((nInvAmnt - nInvPaid - nInvDistK - nInvAdj) < 0 ) ;
               .AND. cInvStat <> 'V'
LOCATE
SET ORDER TO TAG VENDINV
SELECT (lc_InvHdr)

SET ORDER TO 0

*E300296,1 M.H 10/10/95 Add the currency to the AP module.
* Add the Currency code to the filter Exp.
*SET FILTER TO &lcInvTagEx = laData(1) .AND. EVALUATE(lcInvFlt)
SET FILTER TO &lcInvTagEx = laData(1) .AND. APINVHDR.CCURRCODE = &lc_InvHdr..CCURRCODE ;
              .AND. EVALUATE(lcInvFlt)
LOCATE
SET ORDER TO TAG VENDINV

lcDispObj   = IIF(EOF('APINVHDR') .OR. EOF(lc_InvHdr),"DISABLE","ENABLE")
lcVenObj    = IIF(EMPTY(laData[1]), "DISABLE", "ENABLE")
lcInvObj    = IIF(EMPTY(laData[1]) .OR. EOF(lc_InvHdr),"DISABLE","ENABLE")
lcDMObj     = IIF(EMPTY(laData[1]) .OR. EOF('APINVHDR'),"DISABLE","ENABLE")
  
*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

*E300789,4  AMM Adjust to fit the new structure
*SET FILTER TO (CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp+'N'+'CDIVISION') OR;
              (CCOMP_ID+CRLTFIELD+CFLD_NAME ='  '+'N'+'N/A') OR;
              (CCOMP_ID+CRLTFIELD+CFLD_NAME ='  '+'N'+'ALL')
SET FILTER TO (cDefCode+CRLTFIELD+CFLD_NAME = 'N'+'N'+'CDIVISION') OR;
              (cDefCode+CRLTFIELD+CFLD_NAME = 'N'+'N'+'N/A') OR;
              (cDefCode+CRLTFIELD+CFLD_NAME = 'N'+'N'+'ALL')
*E300789,4  AMM end
LOCATE

IF _WINDOWS

  *E300643,1 Change this line for the changes we have made to 
  *          SYCCODES [Begin]
  *DEFINE POPUP puDivision prompt field SYCCODES.cdiscrep scroll;
  *FROM 3.65,19.63 TO 7.95,52.1;
  *MESSAGE gfObj_msg()
  
  DEFINE POPUP puDivision prompt field CODES.cdiscrep scroll;
  FROM 3.65,19.63 TO 7.95,52.1;
  MESSAGE gfObj_msg()
  
  *E300643,1 Change this line [End]

  ON SELECTION POPUP puDivision DO lfvDivision
ENDIF   

*** The main file for this screen is APVENDOR
SELECT APVENDOR

*** Store keys :
PUSH KEY
lcEscOnKey = ON("KEY","ESC")
lcCtrlEntr = ON("KEY","CTRL+ENTER")
ON KEY LABEL TAB         DO lpTab
ON KEY LABEL BACKTAB     DO lpShiftTab

*E300683,1 Call *.SPR from screens directory
* DO APAPLDB.SPR 
DO (gcScrDir + gcWinAppl + '\APAPLDB.SPR')
*E300683,1 end          

POP KEY

RELEASE PAD _BROWSE OF _MSYSMENU

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SET FILTER TO 

SELECT APINVHDR
SET FILTER TO
SELECT (lc_InvHdr)
SET FILTER TO

IF glQuitting
  IF USED (lc_InvHdr)
    USE IN (lc_InvHdr)
  ENDIF
  IF WVISIBLE( 'CWRAPAPLVN')
    HIDE WINDOW CWRAPAPLVN
  ENDIF  
  IF WVISIBLE( 'CWRAPAPLIN')
    HIDE WINDOW CWRAPAPLIN
  ENDIF  
  IF WVISIBLE( 'CWRAPAPLDM')
    HIDE WINDOW CWRAPAPLDM
  ENDIF  
  HIDE WINDOW  (gcBaseWind)
  RELEASE WINDOWS CWRAPAPLVN,CWRAPAPLIN,CWRAPAPLDM
                  
ENDIF  
RELEASE POPUPS puDivision
RELEASE WINDOWS (lcBrTtl1),(lcBrTtl2) 
glFromBrow  = .F.

*!**************************************************************************
*!
*!        Function : lfDataShow
*!
*!**************************************************************************
*
FUNCTION lfDataShow
PARAMETERS llShowGets
lnYTDTotPay = IIF(SEEK(laData[1] + lcCurrYear,'APVENHST'),APVENHST. nVnHTotPa, 0)
                     
*** reset Scope on invoices button
rbInvScope = 1 
*** reset filter parameters, and filter expreseeion
*B801196,4 AMM start
*lcDateFlt = ' '
*SET ORDER TO 0 IN (lc_InvHdr)
*lcInvFlt   = lcInvFlt_1
*SET ORDER TO TAG VENDINV IN (lc_InvHdr)
*B801196,4 AMM end 
IF llShowGets

  *** reset date to default system date
  ldAppDate = gdSysDate

* SHOW GETS ENABLE
  =lfRefrObj()
  
* STORE "ENABLE" TO lcDispObj, lcVenObj, lcInvObj, lcDMObj 
* STORE .F. TO llDisObj, llDisVen, llDisScp, llDisDM, llDisInv
* lcSayCol  = lcDisCont
* lcTopLine = lcTopLeft
* lcBotLine = lcBotRight

  _CUROBJ  = OBJNUM(ldAppDate)

ELSE
  SCATTER FIELDS &lcScFields TO laData BLANK
  *B801196,4 AMM start
  *GO TOP IN APINVHDR
  lnCurAlias = SELECT(0)
  SELECT APINVHDR
  lcOldTag=TAG()
  SET ORDER TO 
  LOCATE
  SET ORDER TO TAG &lcOldTag.  
  
  *GO TOP IN (lc_InvHdr)
  SELECT (lc_InvHdr)
  lcOldTag=TAG()
  SET ORDER TO
  LOCATE
  SET ORDER TO TAG &lcOldTag.  
  SELECT (lnCurAlias)
  *B801196,4 AMM end  
  ldAppDate = {}
  SHOW GETS WINDOW (lcApDbWin1) ENABLE
  SHOW GET  ldAppDate           DISABLE
  SHOW GETS WINDOW (lcApDbWin4) DISABLE
  STORE "DISABLE" TO lcDispObj, lcVenObj, lcInvObj, lcDMObj 
  STORE .T. TO llDisObj, llDisVen, llDisScp, llDisDM, llDisInv
  lcSayCol  = lcHidObjNrm
  STORE lcBackGr TO lcTopLine, lcBotLine
 
  SHOW GET pbClose ENABLE
  _CUROBJ   = OBJNUM(laData[1])
  SHOW GET laData[1]
  IF WVISIBLE('CWRAPAPLVN')
    =gfChClose('CWRAPAPLVN')
  ENDIF
  IF WVISIBLE('CWRAPAPLDM')
    =gfChClose('CWRAPAPLDM')
  ENDIF
  IF WVISIBLE('CWRAPAPLIN')
    =gfChClose('CWRAPAPLIN')
  ENDIF

  lnBrRecN1   = RECNO('APINVHDR') 
  lnBrRecN2   = RECNO(lc_InvHdr) 

  *B801196,4 AMM start
  lnCurAlias=SELECT(0)
  SELECT APINVHDR
  lcCurFilt = FILTER()
  SET FILTER TO
  SHOW WINDOW (lcBrTtl1) REFRESH SAME
  SET FILTER TO &lcCurFilt.
  LOCATE
  SELECT (lnCurAlias)
  SHOW WINDOW (lcBrTtl1) REFRESH SAME
  *B801196,4 AMM end  
    
  SHOW WINDOW (lcBrTtl2) REFRESH SAME
  SHOW WINDOW (lcApDbWin1) REFRESH 
  =lfRefresh(lcApDbWin1+IIF(WVISIBLE('CWRAPAPLVN'), ",CWRAPAPLVN", "")+;
                        IIF(WVISIBLE('CWRAPAPLDM'), ",CWRAPAPLDM", "")+;
                        IIF(WVISIBLE('CWRAPAPLIN'), ",CWRAPAPLIN", ""))
ENDIF

SELECT APVENDOR

*!**************************************************************************
*!
*!        Function : lfvData
*!
*!**************************************************************************
FUNCTION lfvData
PRIVATE llFound
IF lfSekVnd('APVENDOR',lcOldVal, llBrowse)
  *B801196,4 AMM start
  lcDateFlt = ' '
  lcInvFlt   = lcInvFlt_1
  *B801196,4 AMM end

  llFound = .T.

  *B801196,4 AMM start 
  *GO TOP IN APINVHDR
  lnCurAlias = SELECT(0)
  SELECT APINVHDR
  lcOldTag=TAG()
  SET ORDER TO
  LOCATE
  SET ORDER TO TAG &lcOldTag.

  *GO TOP IN (lc_InvHdr)
  SELECT (lc_InvHdr)
  lcOldTag=TAG()
  SET ORDER TO
  LOCATE
  SET ORDER TO TAG &lcOldTag.
  *B801196,4 AMM end   
  
  *E300296,1 Ren 10/22/95 Add the currency to the AP module.
  * Fill Debit memo and invoice variables.
  lcDebMem  = APINVHDR.cInvNo
  lcInvoice = &lc_InvHdr..cInvNo
  *E300296,1 Ren End.

  DO CASE
    *** If the vendor's priority = 0, present
    *** the following message and exit.
    CASE APVENDOR.cVenPrior = "0"
      *** Message : "  Vendor ð has payment priority 0. This vendor is on hold. "
      ***                  <  OK  >
      =gfModalGen("TRM04060B00000","DIALOG",ALLTRIM(laData[1]))
      llFound = .F.

    *** If there are no debit memos for this vendor, present
    *** the following message and exit.
    CASE EOF('APINVHDR')
      *** Message : "   No ð to apply for vendor ð. "
      ***                  <  OK  >
      =gfModalGen("TRM04026B00000","DIALOG",lcDebtMemo+"|"+ALLTRIM(laData[1]))
      llFound = .F.

    *** If there are no invoices for this vendor, present
    *** the following message and exit.
    CASE EOF(lc_InvHdr) .AND. !gfGetMemVar('LLMULCURR')
      *** Message : "  No ð to apply for vendor ð."
      ***                  <  OK  >
      =gfModalGen("TRM04026B00000","DIALOG",lcOpenInv+"|"+ALLTRIM(laData[1]))
      llFound = .F.
  ENDCASE   
  =lfDataShow(llFound)
ELSE
  llFound = .F.
ENDIF
llBrowse   = .F.
lcVenObj   = IIF(!EMPTY(laData[1]), "ENABLE", "DISABLE")

*!**************************************************************************
*!
*!        Function : lfActBrow
*!
*!**************************************************************************
*
FUNCTION lfActBrow
PARAMETERS lcFilter
PRIVATE lcClrSchm

lcClrSchm = IIF(_DOS," COLOR SCHEME 13","NOMENU")
DO CASE 
  CASE RIGHT(lcFilter,1) = '1'
    SELECT APINVHDR 
    lnBrRecN1  = RECNO('APINVHDR')
    *B600492,1 Change the width of cInvNo & cInvRef.
    *E300296,1 Ren 10/22/95 Add the currency field to the browse.

    *E300643,1 Change this line for the changes we have made 
    *          to (gfCodDes) [Begin]
    *BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecN1,'',' '):1:H=" ":W=.F.,; 
    *              cInvNo   :12: H=laBrHdrs1[1],;
    *              dInvDate : H=laBrHdrs1[2]:10,;
    *              cInvRef  :11: H=laBrHdrs1[3],;
    *              nOpenBal  = nInvAmnt - nInvPaid - nInvDistK - nInvAdj ;
    *                          :H=laBrHdrs1[4]:15,;
    *              nTotApprv = nInvAdjAp + nInvDisAp + nInvAmtAp ;
    *                          :H=laBrHdrs1[5]:15,;
    *              cDivDesc = gfCodDes(cDivision):H=laBrHdrs1[6]:30,;
    *              cCurrCode :5 :H=laBrHdrs1[7];
    *       WINDOW (lcApDbWin2) ;
    *       IN WINDOW (gcBaseWind) ;
  	*       LOCK 0;
    *       NOAPPEND;
    *       NOEDIT;
	*       NOCLEAR;
    *       NODELETE;
    *       NOWAIT;
    *       SAVE;
 	*       TITLE lcBrTtl1;
 	*       WHEN lfwBroWhen(); 
 	*       VALID :F lfvBrowse() ;
    *       &lcClrSchm
    *B801196,4 AMM start   add REST
    BROWSE REST FIELDS cMarker=IIF(RECNO()=lnBrRecN1,'',' '):1:H=" ":W=.F.,; 
                  cInvNo   :12: H=laBrHdrs1[1],;
                  dInvDate : H=laBrHdrs1[2]:10,;
                  cInvRef  :11: H=laBrHdrs1[3],;
                  nOpenBal  = nInvAmnt - nInvPaid - nInvDistK - nInvAdj ;
                              :H=laBrHdrs1[4]:15,;
                  nTotApprv = nInvAdjAp + nInvDisAp + nInvAmtAp ;
                              :H=laBrHdrs1[5]:15,;
                  cDivDesc = gfCodDes(cDivision , 'CDIVISION'):H=laBrHdrs1[6]:30,;
                  cCurrCode :5 :H=laBrHdrs1[7];
           WINDOW (lcApDbWin2) ;
           IN WINDOW (gcBaseWind) ;
  	       LOCK 0;
           NOAPPEND;
           NOEDIT;
	       NOCLEAR;
           NODELETE;
           NOWAIT;
    	   SAVE;
 	       TITLE lcBrTtl1;
 	       WHEN lfwBroWhen(); 
 	       VALID :F lfvBrowse() ;
           &lcClrSchm
    *B801196,4 AMM end
    *E300643,1 Change this line [End]
    
  CASE RIGHT(lcFilter,1) = '2'
    SELECT (lc_InvHdr)  
    lnBrRecN2  = RECNO(lc_InvHdr)
    *B600492,1 Change the width of the cInvNo.

    *E300643,1 Change this line for the changes we have made 
    *          to (gfCodDes) [Begin]
    *BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecN2,'',' '):1:H=" ":W=.F.,; 
    *              cInvNo:12:H=laBrHdrs2[1],;
    *              dInvDate:H=laBrHdrs2[2]:10,;
    *              dInvDuDat:H=laBrHdrs2[3],;
    *              cVenPrior:H=laBrHdrs2[4],;
    *              nOpenBal= nInvAmnt - nInvPaid - nInvDistK - nInvAdj ;
    *              :H=laBrHdrs2[5]:15,;
    *              nTotApprv=nInvAdjAp + nInvDisAp + nInvAmtAp ;
    *              :H=laBrHdrs2[6]:15,;
    *              cPayMeth = lfGetPayMeth(cVenPMeth);
    *                         :H=laBrHdrs2[7]:18,;
    *              cDivDesc = gfCodDes(cDivision):H=laBrHdrs2[8]:30;
    *       WINDOW (lcApDbWin3) ;
    *       IN WINDOW (gcBaseWind) ;
  	*       LOCK 0;
    *       NOAPPEND;
    *       NOEDIT;
	*       NOCLEAR;
    *       NODELETE;
    *       NOWAIT;
    *       SAVE;
 	*       TITLE lcBrTtl2;
	*       WHEN lfwBroWhen();  	       
 	*       VALID :F lfvBrowse() ;
    *       &lcClrSchm

    BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecN2,'',' '):1:H=" ":W=.F.,; 
                  cInvNo:12:H=laBrHdrs2[1],;
                  dInvDate:H=laBrHdrs2[2]:10,;
                  dInvDuDat:H=laBrHdrs2[3],;
                  cVenPrior:H=laBrHdrs2[4],;
                  nOpenBal= nInvAmnt - nInvPaid - nInvDistK - nInvAdj ;
                  :H=laBrHdrs2[5]:15,;
                  nTotApprv=nInvAdjAp + nInvDisAp + nInvAmtAp ;
                  :H=laBrHdrs2[6]:15,;
                  cPayMeth = lfGetPayMeth(cVenPMeth);
                             :H=laBrHdrs2[7]:18,;
                  cDivDesc = gfCodDes(cDivision , 'CDIVISION'):H=laBrHdrs2[8]:30;
           WINDOW (lcApDbWin3) ;
           IN WINDOW (gcBaseWind) ;
  	       LOCK 0;
           NOAPPEND;
           NOEDIT;
	       NOCLEAR;
           NODELETE;
           NOWAIT;
    	   SAVE;
 	       TITLE lcBrTtl2;
	       WHEN lfwBroWhen();  	       
 	       VALID :F lfvBrowse() ;
           &lcClrSchm

    *E300643,1 Change this line [End]

ENDCASE
SELECT APVENDOR

*!**************************************************************************
*!
*!        Function : lfvBrowse
*!
*!**************************************************************************
* 
FUNCTION lfvBrowse
IF WONTOP(lcBrTtl1) .OR. WONTOP(lcBrTtl2)
  IF !WEXIST(lcApDbWin2) .OR. !WEXIST(gcBaseWind) 
    glFromBrow = .F.
    glQuitting = !WEXIST(lcApDbWin2)
    CLEAR READ
    KEYBOARD CHR(13)
    RETURN TO APAPLDB.SPR
  ENDIF
ELSE
  =lfBrowUnTrap()
  glQuitting = !WVISIBLE(gcBaseWind)
  =gfStopBrow()
ENDIF    

*!**************************************************************************
*!
*!        Function : lfwBroWhen
*!
*!**************************************************************************
* 
FUNCTION lfwBroWhen
IF !EMPTY(laData[1])
  *E300296,1 Ren 10/22/95 its moved to the lfRefrObj function to fill it after refreshing the filter.
  *lcDebMem   = APINVHDR.cInvNo
  *lcInvoice  = &lc_InvHdr..cInvNo
  =lfRefrObj()
ENDIF  

*!**************************************************************************
*!
*!        Function : lfvAppFull
*!
*!**************************************************************************
*  Valid function for push button : < Apply fully >
*
FUNCTION lfvAppFull
PRIVATE lcCurAlias
lcDebMem    = APINVHDR.cInvNo
lcInvoice   = &lc_InvHdr..cInvNo

*B800815,1 Adding confirm dialog (Bigin)
*B800815,1 Message :     " Are you sure you wish to apply Debit Memo No. ð [(lcDebMem)]"
*B800815,1               "to ð [Invoice No. (lcInvoice)] ?                              "
*B800815,1                <  Apply  >               <  Cancel  >         
lnAprApp = gfModalGen("QRM04159B04007","DIALOG",ALLTRIM(lcDebMem)+'|'+ 'Invoice No. ' +ALLTRIM(lcInvoice))
*B800815,1 Adding confirm dialog (End)

*B800815,1 IF Statment to chick if <  Cancel  >
IF lnAprApp = 2
  RETURN
ENDIF
*B800815,1 IF Statment (End)

*** If application date is valid, proceed with the application process.
IF lfVldAppDt() .AND. lfRecLock() 
  *E300296,1 Ren 10/22/95
  *=lfVldAppl() .AND. lfApply() .AND. lfRefrObj()
  IF lfVldAppl() .AND. lfApply() 
    
    *--B602006 AAN Begin
    IF !EOF(lc_InvHdr)
      SKIP IN (lc_InvHdr)
    ENDIF
    *--B602006 AAN END
    
    =lfRefrObj()   
  ENDIF
  *E300296,1 REN end.

  lcCurAlias = ALIAS()
  SELECT APINVHDR
  =gfObj_Lock(.F.)
  SELECT (lc_InvHdr)
  =gfObj_Lock(.F.)
  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
ENDIF

*!**************************************************************************
*!
*!        Function : lfApply
*!
*!**************************************************************************
*  Application process
*
FUNCTION lfApply

PARAMETERS llApplyAll
PRIVATE lnDM_OpnAm, lnDM_AprAm, lnIn_OpnAm, lnIn_AprAm,;
        lnOption, llClearApr, llClearDM, llWillApply 
*** lnDM_OpnAm  : debit memo open amount 
*** lnDM_AprAm  : debit memo approved amount 
*** lnIn_OpnAm  : invoice open amount 
*** lnIn_AprAm  : invoice approved amount 
*** lnOption    : return value of the message dialog if presented
*** llWillApply : reset it application process is cancelled by user.

lnDM_OpnAm   = APINVHDR.nInvAmnt - APINVHDR.nInvPaid ;
                   - APINVHDR.nInvDisTk - APINVHDR.nInvAdj
lnDM_AprAm   = APINVHDR.nInvAdjAp + APINVHDR.nInvDisAp ;
                     + APINVHDR.nInvAmtAp
lnIn_OpnAm   = &lc_InvHdr..nInvAmnt - &lc_InvHdr..nInvPaid ;
                     - &lc_InvHdr..nInvDisTk - &lc_InvHdr..nInvAdj
lnIn_AprAm   = &lc_InvHdr..nInvAdjAp + &lc_InvHdr..nInvDisAp ;
                     + &lc_InvHdr..nInvAmtAp
STORE .F. TO llClearApr, llClearDM
llWillApply  = .T.
*** Amount to be applied is the minimum of the debit memo open
*** absolute value and the invoice open amount
lnApplAmnt   = MIN(ABS(lnDM_OpnAm), lnIn_OpnAm)
*** No discount is to be applied
lnDiscAmnt   = 0     

*** Check if the debit memo has approved amounts thath may me
*** cleared by application.
*** If it does, present the following message 
*** Present the following dialog :
*** Message :  "   Debit memo No. ð has a total approved amount   "
***            "   of ð.  Applying this debit memo to invoice     "
***            "   No. ð will clear its approved amount.          "
***                       < Apply >   < Cancel >
*** Continue with the process if the user selects to apply
IF lnDM_AprAm <> 0 .AND. lnApplAmnt > ABS(lnDM_OpnAm - lnDM_AprAm)
  lnOption    = gfModalGen(IIF(llApplyAll, "QRM04143B04009","QRM04143B04007"),;
               "DIALOG",ALLTRIM(APINVHDR.cInvNo) + '|' ;
                + ALLTRIM(STR(lnDM_AprAm, 16, 2)) + '|' +;
                ALLTRIM(&lc_InvHdr..cInvNo)) 
  llApplyAll  = llApplyAll .AND. !(lnOption = 3)    && Cancel application (all)
  llWillApply = lnOption = 1       && .f. if Skip or Cancel
  llClearDM   = llWillApply
ENDIF 
IF llWillApply
  *** Check if the invoice has approved amounts that may be cleared
  *** by application.
  *** IF the debit memo open amount is greater than the difference
  *** between between the invoice open amount and its approved amount
  IF lnIn_AprAm > 0 .AND. ;
    lnApplAmnt > (lnIn_OpnAm - lnIn_AprAm)
    *** If the open invoice amount is totally approved,
    *** present the following dialog :
    *** Message :  "  Invoice No. ð is totally approved.          "
    ***            "  Applying to this invoice will clear its     "
    ***            "  approved amount. Do you wish to apply to    "
    ***            "  the full open amount,ð or cancel            "
    ***            "  application ?                               " 
    ***            < Apply >     [ < Skip > ]   < Cancel >
    *** else, present the following dialog :
    *** Message :  "  Applying to invoice no. ð will clear its    "
    ***            "  approved amount. Do you wish to apply to    "
    ***            "  the full open amount, to the open           "
    ***            "  unapproved amount,ð or cancel appplication? " 
    ***      < Full amount >  < Unapproved amount > [ <Skip> ] < Cancel >
    lnOption = IIF(llApplyAll,;
               gfModalGen(IIF(lnIn_OpnAm = lnIn_AprAm,;
                          "QRM04144B04009", "QRM04027B04008"),;
               "DIALOG", ALLTRIM(&lc_InvHdr..cInvNo)+'|'+lcTSkip),;            
               gfModalGen(IIF(lnIn_OpnAm = lnIn_AprAm,;
                        "QRM04144B04007","QRM04027B04000"),;
                        "DIALOG", ALLTRIM(&lc_InvHdr..cInvNo)+'||'))
    DO CASE
      *** If applying unapproved amount, adjust the amount to be 
      *** applied value 
      CASE lnOption = 1
        llClearApr = .T.      && Clear approval fields
      CASE lnOption = 2 .AND. lnIn_OpnAm <> lnIn_AprAm
        lnApplAmnt = lnIn_OpnAm - lnIn_AprAm 
      OTHERWISE
        llWillApply = .F.
        llApplyAll  = !(lnIn_OpnAm = lnIn_AprAm .AND. lnOption = 3 ;
                       .OR. lnOption = 4)
    ENDCASE  
  ENDIF  
ENDIF

*B601061,1 Clear locking fields whether we apply or not.
*** If the applied amount is greater than zero, proceed with the 
*** application process
*RETURN llWillApply .AND. ;
       (lnApplAmnt > 0 .AND. lfApplDbMm(llClearApr, llClearDM))
llWillApply = llWillApply .AND. ;
       (lnApplAmnt > 0 .AND. lfApplDbMm(llClearApr, llClearDM))
lnCurALias = SELECT()
SELECT APINVHDR
=gfObj_Lock(.F.)
SELECT (lc_InvHdr)
=gfObj_Lock(.F.)
SELECT (lnCurAlias)
RETURN llWillApply 
*B601061,1 end.
  
*!**************************************************************************
*!
*!        Function : lfvAppPart
*!
*!**************************************************************************
*  Valid function for push button : < Apply partially... >
*
FUNCTION lfvAppPart
PRIVATE lcCurAlias, lcOldAcct, lnDM_OpnAm, lnDM_AprAm, lnIn_OpnAm,;
        lnIn_AprAm, lnPAplAmnt, lnOldVal, lnPAplDisc, lnOption,;
        llLastBrWin, llClearApr, llClearDM

*** lcCurAlias : current alias
*** lnDM_OpnAm : debit memo open amount 
*** lnDM_AprAm : debit memo approved amount
*** lnIn_OpnAm : invoice open amount 
*** lnIn_AprAm : invoice approved amount 
*** lnPAplAmnt : Partial amount to be applied
*** lnPAplDisc : Partial discount to be applied
*** lnOption   : return value of the message dialog if presented

llLastBrWin = WONTOP(lcBrTtl1) .OR. WONTOP(lcBrTtl2)
*B601061,1 Fill Debit memo/invoice variables for messages
lcDebMem    = APINVHDR.cInvNo
lcInvoice   = &lc_InvHdr..cInvNo
*B601061,1 end.

STORE .F. TO llClearApr, llClearDM
IF lfVldAppDt() .AND. lfRecLock()
  IF lfVldAppl()
    lcOldAcct = lcEmptyAcc
    lcOldDesc = SPACE(65)
    lcAccDesc = SPACE(65)

    lnDM_OpnAm  = APINVHDR.nInvAmnt - APINVHDR.nInvPaid ;
                   - APINVHDR.nInvDisTk - APINVHDR.nInvAdj
    lnDM_AprAm  = APINVHDR.nInvAdjAp + APINVHDR.nInvDisAp ;
                   + APINVHDR.nInvAmtAp
    lnIn_OpnAm  = &lc_InvHdr..nInvAmnt - &lc_InvHdr..nInvPaid ;
                   - &lc_InvHdr..nInvDisTk - &lc_InvHdr..nInvAdj
    lnIn_AprAm  = &lc_InvHdr..nInvAdjAp + &lc_InvHdr..nInvDisAp ;
                   + &lc_InvHdr..nInvAmtAp
    lnOldVal    = 0
    *lnPAplAmnt  = MIN(ABS(lnDM_OpnAm), lnIn_OpnAm)
                              && partial amount to be applied default
                              && by the available application amount
    lnPAplAmnt  = 0          && partial amount to be applied default by 0
    lnPAplDisc  = 0          && partial disacount to be applied is now 0
    llApply     = .F.

    *E301077,79 AMM Open file
    =gfOpenFile(gcDataDir+'APSETUP','','SH')
    =gfOpenFile(gcDataDir+'APDIV','DIVISION','SH')
    *E301077,79 AMM end
    *** Get the discount account of the division related to the
    *** current invoice if one exists, otherwise, use the discount
    *** account in the AP setup.
    GO TOP IN APSETUP
    SET ORDER TO TAG DIVISION IN APDIV
    lcDiscAcct  = IIF(SEEK(&lc_InvHdr..cDivision,'APDIV') ;
                    .AND. !EMPTY(APDIV.cDiscAcct), ;
                      APDIV.cDiscAcct, APSETUP.cDiscAcct) 
    IF EMPTY(lcDiscAcct)  
      lcDiscAcct = lcEmptyAcc
      lcAccDesc  = SPACE(60)
    ELSE
      lcAccDesc  = IIF(llApGlLink, ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,;
                       lcDiscAcct,lcLinkChar.CACCTCODE,"ACCTCODE")),' ')
    ENDIF     
    
    PUSH KEY
    ON KEY
    *E300683,1 Call *.SPR from screens directory
    * DO APAPLDBP.SPR 
    DO (gcScrDir + gcWinAppl + '\APAPLDBP.SPR')
    *E300683,1 end          
    
    POP KEY
  
    IF llApply 
      lcDebMem   = APINVHDR.cInvNo
      lcInvoice  = &lc_InvHdr..cInvNo
      lnApplAmnt = lnPAplAmnt
      lnDiscAmnt = lnPAplDisc
      ** REN
      *=lfApplDbMm(llClearApr, llClearDM) .AND. lfRefrObj()
      IF lfApplDbMm(llClearApr, llClearDM)
        =lfRefrObj()   
      ENDIF
      ** REN end.
    ENDIF  
  ENDIF    
  *** Release object locks
  lcCurAlias = ALIAS()
  SELECT APINVHDR
  =gfObj_Lock(.F.)
  SELECT (lc_InvHdr)
  =gfObj_Lock(.F.)
  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
ENDIF

*!**************************************************************************
*!
*!        Function : lfvAppAll
*!
*!**************************************************************************
*  Valid function for push button : < Apply All >
*
FUNCTION lfvAppAll

PRIVATE lcCurAlias, lnCurTag, llApplied, llApplyAll

*B800815,1 Move this two lines (Bigin)  
*llApplied  = .F.
*llApplyAll = .T.
*B800815,1 Move this two lines (End)
lcDebMem   = APINVHDR.cInvNo
lcInvoice  = &lc_InvHdr..cInvNo

*B800815,1 Adding confirm dialog (Bigin)
*B800815,1 Message :     " Are you sure you wish to apply Debit Memo No. ð [(lcDebMem)]"
*B800815,1               "to ð [all invoices] ?                                        "
*B800815,1                   <  Apply  >               <  Cancel  >         
lnAprApp = gfModalGen("QRM04159B04007","DIALOG",ALLTRIM(lcDebMem)+'|'+ 'all invoices')
*B800815,1 Adding confirm dialog (End)

*B800815,1 IF Statment to chick if <  Cancel  >
IF lnAprApp = 2
  RETURN
ENDIF
*B800815,1 IF Statment (End)

*B800815,1 Replace the two lines (Bigin)
llApplied  = .F.
llApplyAll = .T.
*B800815,1 Replace the two lines (End)

*** If application date is empty, return .F. and present
*** the following message.
=lfModalShow()
IF lfVldAppDt()
  lcCurAlias = ALIAS()
  *** Attempt to lock debit memo record
  *B609026,1 TMI [START] check if any of the lines in apinvhdr or (lc_InvHdr) are used in Aria4 then do not continue
  IF RECNO('APINVHDR')>RECCOUNT('APINVHDR') OR RECNO(lc_Invhdr)>RECCOUNT(lc_Invhdr) 
    RETURN
  ENDIF
  SELECT APINVHDR  
  lcSvFlt = FILTER()
  lnRecno = RECNO()
  DO WHILE .T.  
    SET FILTER TO
    LOCATE
    =SEEK(laData[1])
    LOCATE REST WHILE CVENDCODE = laData[1];
            FOR !EMPTY(CLOK_USER) 
    lcLOK_USER = CLOK_USER
    SET FILTER TO &lcSvFlt
    LOCATE
    GOTO (lnRecno)
    IF FOUND()
      IF gfModalGen("INM00028B00015","ALERT",lcLOK_USER) = 1
        LOOP
      ENDIF
      RETURN
    ELSE
      EXIT
    ENDIF
  ENDDO
  *B609026,1 TMI [ END ] check if any of the lines in apinvhdr or (lc_InvHdr) are used in Aria4 then do not continue  

  SELECT APINVHDR
  *** If locking succeeds, proceed
  IF gfObj_Lock(.T.)
    DO CASE
      *** Check if the debit memo has just been paid by another user
      CASE APINVHDR.nInvAmnt - APINVHDR.nInvPaid - ;
           APINVHDR.nInvDisTk - APINVHDR.nInvAdj = 0
        *** Message :     "   ð No. ð has just been ð by another user!  "
        ***               "   Cannot apply Debit Memo No. ð to Invoice  "
        ***               "   No. ð.                                    "
        ***                                   <  OK  >
        =gfModalGen("TRM04071B00000","DIALOG",;
                       lcUDebtMem + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                       lcTPaid + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                       ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** Check if the debit memo has just been voided by another user
      CASE APINVHDR.cInvStat = 'V'
        *** Message :     "   ð No. ð has just been ð by another user!  "
        ***               "   Cannot apply Debit Memo No. ð to Invoice  "
        ***               "   No. ð.                                    "
        ***                                   <  OK  >
        =gfModalGen("TRM04071B00000","DIALOG",;
                    lcUDebtMem + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                    lcTVoided + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                     ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** Check if the current debit memo has a zero priority (on hold)
      CASE APINVHDR.cVenPrior = "0" 
        lcDebitMem = SUBSTR(lcBrTtl1,1,LEN(lcBrTtl1)-1)
        *** Message : "  ð has payment priority 0. ð is on hold. "
        ***                  <  OK  >
        =gfModalGen("TRM04059B00000","DIALOG",;
        lcDebitMem+" "+ALLTRIM(APINVHDR.cInvNo)+"|"+;
        lcDebitMem+" "+ALLTRIM(APINVHDR.cInvNo))

      *** If application date is less than the debit memo date, 
      *** return .F. and present the following message
      CASE ldAppDate < APINVHDR.dInvDate
        *** Message : "  The application date cannot be less than the    " 
        ***           "  ð date. Cannot apply debit memo ð to invoice ð. "
        ***                              <  OK  >
        =gfModalGen("TRM04036B00000","DIALOG",lcDebtMemo+'|'+;
                      ALLTRIM(APINVHDR.cInvNo)+'|'+lcTAllInv)

      OTHERWISE
        SELECT (lc_InvHdr)
        lnCurTag   = VAL(SYS(21))
  
        *** Tag expression is DTOS(dInvDuDat)
        SET ORDER TO TAG DUEDATE
        GO TOP
 
        lcDbMem    = APINVHDR.cInvNo
        SCAN WHILE llApplyAll .AND. APINVHDR.cInvNo = lcDbMem .AND.;
                    ((APINVHDR.nInvAmnt - APINVHDR.nInvPaid - ;
                    APINVHDR.nInvDisTk - APINVHDR.nInvAdj ) < 0)
          IF gfObj_Lock(.T.)
            =lfRefresh(lcApDbWin1+IIF(WVISIBLE('CWRAPAPLVN'), ",CWRAPAPLVN", "")+;
                      IIF(WVISIBLE('CWRAPAPLDM'), ",CWRAPAPLDM", "")+;
                      IIF(WVISIBLE('CWRAPAPLIN'), ",CWRAPAPLIN", ""))
            lcInv = SUBSTR(lcBrTtl2,1,LEN(lcBrTtl2)-1)
            DO CASE
              *** Check if the invoice has just been paid by another user
              CASE &lc_InvHdr..nInvAmnt - &lc_InvHdr..nInvPaid - ;
                   &lc_InvHdr..nInvDisTk - &lc_InvHdr..nInvAdj = 0
                *** Message : "  ð No. ð has just been ð by another user!  "
                ***           "  Cannot apply Debit Memo No. ð to Invoice  "
                ***           "  No. ð.                                    "
                ***                                   <  OK  >
                =gfModalGen("TRM04071B00000","DIALOG",;
                     lcUInv + '|' + ALLTRIM(&lc_InvHdr..cInvNo) + '|' +;
                     lcTPaid + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                     ALLTRIM(&lc_InvHdr..cInvNo)) = 0
         
              *** Check if the invoice has just been voided by another user
              CASE &lc_InvHdr..cInvStat = 'V'
                *** Message : "   ð No. ð has just been ð by another user!  "
                ***           "   Cannot apply Debit Memo No. ð to Invoice  "
                ***           "   No. ð.                                    "
                ***                                   <  OK  >
                =gfModalGen("TRM04071B00000","DIALOG",;
                       lcUInv + '|' + ALLTRIM(&lc_InvHdr..cInvNo) + '|' +;
                       lcTVoided + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                       ALLTRIM(&lc_InvHdr..cInvNo)) = 0

              *** Check if the current invoice has a zero priority (on hold)
              *** Message : "  ð has zero payment priority.  ð is on hold.  "
              ***           "  Do you wish to continue applying debit memo  "
              ***           "  ð to the rest of the invoices?               "
              ***                      < Continue >    < Stop >
              CASE &lc_InvHdr..cVenPrior = "0" 
                IF gfModalGen("QRM04063B04002","DIALOG",;
                   lcInv +" "+ALLTRIM(&lc_InvHdr..cInvNo)+"|"+;
                   lcInv +" "+ALLTRIM(&lc_InvHdr..cInvNo)+"|"+;
                   ALLTRIM(APINVHDR.cInvNo)) = 2
                  EXIT      
                ENDIF

              *** If application date is less than the invoice date, 
              *** return .F. and present the following message
              *** Message : "   The application date cannot be less than     " 
              ***           "   the ð date. Cannot apply debit memo ð to     "
              ***           "   invoice ð. Do you want to continue applying  "
              ***           "   debit memo ð to the rest of the invoices ?   "
              ***                      < Continue >    < Stop >
              CASE (ldAppDate < &lc_InvHdr..dInvDate) 
                IF gfModalGen("QRM04037B04002","DIALOG",lcInv +'|'+;
                             ALLTRIM(APINVHDR.cInvNo)+'|'+;
                             ALLTRIM(&lc_InvHdr..cInvNo)+'|'+;
                             ALLTRIM(APINVHDR.cInvNo)) = 2
                  EXIT
                ENDIF
              OTHERWISE
                llApplied = lfApply(@llApplyAll) .OR. llApplied 
            ENDCASE
            =gfObj_Lock(.F.)
          ENDIF    
        ENDSCAN
        SET ORDER TO lnCurTag
        GO TOP
        =lfRefresh(lcApDbWin1+IIF(WVISIBLE('CWRAPAPLVN'), ",CWRAPAPLVN", "")+;
                      IIF(WVISIBLE('CWRAPAPLDM'), ",CWRAPAPLDM", "")+;
                      IIF(WVISIBLE('CWRAPAPLIN'), ",CWRAPAPLIN", ""))
    ENDCASE
    SELECT APINVHDR 
    =gfObj_Lock(.F.)  
  ENDIF  
  IF llApplied
    =lfRefrObj()
  ENDIF  
  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
ENDIF
RELEASE PAD _BROWSE OF _MSYSMENU

*!**************************************************************************
*!
*!        Function : lfApplDbMm
*!
*!**************************************************************************
*  File updates, called from application process
*
FUNCTION lfApplDbMm

PARAMETERS llClearApr, lfClearDM
PRIVATE lcCurAlias, llApplied

lcCurAlias  = ALIAS()
llApplied   = .F.
*** Attempt to lock the follosing records in the following files:
IF gfRLock("APVENDOR,APINVHDR,&lc_InvHdr.",.T.) ;
   .AND. IIF(SEEK(laData[1] + lcFisFYear,'APVENHST'),;
           gfRLock('APVENHST',.T.),.T.)
  
  *E300316,1 HISH  11/28/95. Got the equation signs. (Begin)
  *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,APInvHdr.cCurrCode)
  *lcExSin2 = IIF(lcExSin1 = '*','/','*')
  *E300316,1 (End)

  ** Update the  current vendor's open debit field.
  *E300296,1 M.H 10/22/95 Add the Currency Rate & Unit to the replace.
  SELECT APVENDOR

  *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)
  *REPLACE APVENDOR.nVenOpnDr WITH APVENDOR.nVenOpnDr - ROUND(lnApplAmnt * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT,2),;
          APVENDOR.cAdd_User  WITH  gcUser_ID,;
          APVENDOR.dAdd_Date  WITH  ldCurrDate,;
          APVENDOR.cAdd_Time  WITH  gfGetTime()
                                                
  REPLACE APVENDOR.nVenOpnDr  WITH  APVENDOR.nVenOpnDr - ROUND(lnApplAmnt &lcExSin1 APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2),;
          APVENDOR.cAdd_User  WITH  gcUser_ID,;
          APVENDOR.dAdd_Date  WITH  ldCurrDate,;
          APVENDOR.cAdd_Time  WITH  gfGetTime()                                                
  *E300316,1 (End)          

  *** Update the vendor's history file for the fiscal year
  *** of the application date with the applied amount and 
  *** the discount amount.
  SELECT APVENHST
  REPLACE APVENHST.nVnHDisTkn WITH APVENHST.nVnHDisTkn + ROUND(lnDiscAmnt * &lc_InvHdr..NEXRATE / &lc_InvHdr..NCURRUNIT,2),;
          APVENHST.nVnHDMApp WITH APVENHST.nVnHDMApp + ROUND(lnApplAmnt * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT,2),;
          APVENHST.cAdd_User  WITH  gcUser_ID,;
          APVENHST.dAdd_Date  WITH  ldCurrDate,;
          APVENHST.cAdd_Time  WITH  gfGetTime()                                      

  *** Update the current debit memo paid amount field in the debit memo file 
  *** (APINVHDR) (this is always a negative value)
  SELECT APINVHDR
  IF llClearDM
*B800702,1 M.H Begin.

*    REPLACE APINVHDR.nInvPaid WITH APINVHDR.nInvPaid - lnApplAmnt,;
            APINVHDR.nInvAmtAp WITH 0 ,;
            APINVHDR.nInvAdjAp WITH 0 ,; 
            APINVHDR.nInvDisAp WITH 0 ,;
            APINVHDR.cAdd_User  WITH  gcUser_ID,;
            APINVHDR.dAdd_Date  WITH  ldCurrDate,;
            APINVHDR.cAdd_Time  WITH  gfGetTime()                                      

    *B601526,1 Add the new field (nInvFAAp) [Begin]   
    *REPLACE APINVHDR.nInvPaid WITH APINVHDR.nInvPaid - lnApplAmnt,;
    *        APINVHDR.nInvAmtAp  WITH 0 ,;
    *        APINVHDR.nInvAdjAp  WITH 0 ,; 
    *        APINVHDR.nInvDisAp  WITH 0 ,;
    *        APINVHDR.cBnkCode   WITH '',;
    *        APINVHDR.cChkAcct   WITH '',;
    *        APINVHDR.cChkGlAcc  WITH '',;
    *        APINVHDR.cAdd_User  WITH  gcUser_ID,;
    *        APINVHDR.dAdd_Date  WITH  ldCurrDate,;
    *        APINVHDR.cAdd_Time  WITH  gfGetTime()

    *B601954,4 Attempt to fix the problem of not updating
	*B601954,4 the debit memo records in some cases.
    *REPLACE APINVHDR.nInvPaid WITH APINVHDR.nInvPaid - lnApplAmnt,;
            APINVHDR.nInvAmtAp  WITH 0 ,;
            APINVHDR.nInvAdjAp  WITH 0 ,; 
            APINVHDR.nInvDisAp  WITH 0 ,;
            APINVHDR.cBnkCode   WITH '',;
            APINVHDR.cChkAcct   WITH '',;
            APINVHDR.cChkGlAcc  WITH '',;
            APINVHDR.cAdd_User  WITH  gcUser_ID,;
            APINVHDR.dAdd_Date  WITH  ldCurrDate,;
            APINVHDR.cAdd_Time  WITH  gfGetTime(),;
            APINVHDR.nInvFAAp   WITH 0 
            
    REPLACE nInvPaid WITH nInvPaid - lnApplAmnt,;
            nInvAmtAp  WITH 0 ,;
            nInvAdjAp  WITH 0 ,; 
            nInvDisAp  WITH 0 ,;
            cBnkCode   WITH '',;
            cChkAcct   WITH '',;
            cChkGlAcc  WITH '',;
            cAdd_User  WITH  gcUser_ID,;
            dAdd_Date  WITH  ldCurrDate,;
            cAdd_Time  WITH  gfGetTime(),;
            nInvFAAp   WITH 0 
    *B601954,4 end         
    *B601526,1 Add the new field (nInvFAAp) [End]   

*B800702,1 M.H End.  
  ELSE
    *B601954,4 Attempt to fix the problem of not updating
	*B601954,4 the debit memo records in some cases.
    *REPLACE APINVHDR.nInvPaid WITH APINVHDR.nInvPaid - lnApplAmnt,;
            APINVHDR.cAdd_User  WITH  gcUser_ID,;
            APINVHDR.dAdd_Date  WITH  ldCurrDate,;
            APINVHDR.cAdd_Time  WITH  gfGetTime()                                      
    REPLACE nInvPaid WITH nInvPaid - lnApplAmnt,;
            cAdd_User  WITH  gcUser_ID,;
            dAdd_Date  WITH  ldCurrDate,;
            cAdd_Time  WITH  gfGetTime()                                      
    *B601954,4 end
  
  ENDIF
  *** Update the current invoice record in the invoices file 
  *** (lc_InvHdr) 
  SELECT (lc_InvHdr)
  *** If llClearApr, clear the approval fields
  IF llClearApr
*B800702,1 M.H Begin.
*    REPLACE nInvDisTk WITH nInvDisTk + lnDiscAmnt ,;
            nInvAdj   WITH nInvAdj   + lnApplAmnt ,; 
            nInvAmtAp WITH 0 ,;
            nInvAdjAp WITH 0 ,; 
            nInvDisAp WITH 0 ,;
            cAdd_User WITH  gcUser_ID,;
            dAdd_Date WITH  ldCurrDate,;
            cAdd_Time WITH  gfGetTime()                                      

    *B601526,1 Add the new field (nInvFAAp) [Begin]   
    *REPLACE nInvDisTk WITH nInvDisTk + lnDiscAmnt ,;
    *        nInvAdj   WITH nInvAdj   + lnApplAmnt ,; 
    *        nInvAmtAp WITH 0 ,;
    *        nInvAdjAp WITH 0 ,; 
    *        nInvDisAp WITH 0 ,;
    *        cBnkCode  WITH '',;
    *        cChkAcct  WITH '',;
    *        cChkGlAcc WITH '',;
    *        cAdd_User WITH  gcUser_ID,;
    *        dAdd_Date WITH  ldCurrDate,;
    *        cAdd_Time WITH  gfGetTime()                                      

    REPLACE nInvDisTk WITH nInvDisTk + lnDiscAmnt ,;
            nInvAdj   WITH nInvAdj   + lnApplAmnt ,; 
            nInvAmtAp WITH 0 ,;
            nInvAdjAp WITH 0 ,; 
            nInvDisAp WITH 0 ,;
            cBnkCode  WITH '',;
            cChkAcct  WITH '',;
            cChkGlAcc WITH '',;
            cAdd_User WITH  gcUser_ID,;
            dAdd_Date WITH  ldCurrDate,;
            cAdd_Time WITH  gfGetTime(),;
            nInvFAAp  WITH 0
            
    *B601526,1 Add the new field (nInvFAAp) [End]   

*B800702,1 M.H End.
  ELSE
    REPLACE nInvDisTk WITH nInvDisTk + lnDiscAmnt ,;
            nInvAdj   WITH nInvAdj   + lnApplAmnt ,; 
            cAdd_User WITH  gcUser_ID,;
            dAdd_Date WITH  ldCurrDate,;
            cAdd_Time WITH  gfGetTime()                                      
  ENDIF
  *E301077,79 AMM Open File

  =gfOpenFile(gcDataDir+'APDIST','INVVEND','SH')
  *E301077,79 AMM end
  
  ** Append records corresponding to the applied debit memo,
  ** as well as discounts if a discount has occured.
  *E300296,1 M.H 10/22/95 Add the Currency Rate & Unit to the replace.
  *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)
  INSERT INTO APDIST ;
          (cVendCode,cInvNo,cAPDTrTyp,dAPDTrDat, ;
           lAPDPost,cAPDRef,cAPDGLAct,nAPDAmnt, ;
           cCurrCode,nExrate,nCurrUnit,nEqvAmnt,;
           cAPDActID,cFisFYear,cFspPrdId,cAPSessNo, ;
           cAdd_User,dAdd_Date,cAdd_Time);
    VALUES(APINVHDR.cVendCode,APINVHDR.cInvNo,'A',ldAppDate,;
           .F., &lc_InvHdr..cInvNo, &lc_InvHdr..cAPAcct,lnApplAmnt + lnDiscAmnt,;
           &lc_InvHdr..cCurrCode,&lc_InvHdr..nExrate,&lc_InvHdr..nCurrUnit,;
           ROUND((lnApplAmnt+lnDiscAmnt) &lcExSin1 &lc_InvHdr..NEXRATE &lcExSin2 &lc_InvHdr..NCURRUNIT,2),;
           'A', lcFisFYear,lcFspPrdId,lcApDbSess,gcUser_ID,ldCurrDate,;
           gfGetTime())
           *ROUND((lnApplAmnt+lnDiscAmnt) * &lc_InvHdr..NEXRATE / &lc_InvHdr..NCURRUNIT,2),;
  *E300316,1 (End)           
  *E300296,1 M.H 10/22/95 Add the Currency Rate & Unit to the replace.
  *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)

  *B603676,1 06/08/2000 Define 2 var. to hold Equiv. amount for 2 Trans. 
  *B603676,1            Debit Memo and Invoice [Begin]
  PRIVATE lnDiffLin1 , lnDiffLin2
  lnDiffLin1 = ROUND((lnApplAmnt+lnDiscAmnt) &lcExSin1 &lc_InvHdr..NEXRATE &lcExSin2 &lc_InvHdr..NCURRUNIT,2)
  *B603676,1 06/08/2000 [End]
  
  INSERT INTO APDIST ;
            (cVendCode,cInvNo,cAPDTrTyp,dAPDTrDat,;
            lAPDPost,cAPDRef,cAPDGLAct,nAPDAmnt,;
            cCurrCode,nExrate,nCurrUnit,nEqvAmnt,;
            cAPDActID,cFisFYear,cFspPrdId,cAPSessNo ,;
            cAdd_User,dAdd_Date,cAdd_Time );
      VALUES(APINVHDR.cVendCode,APINVHDR.cInvNo,;
             'A',ldAppDate,.F.,&lc_InvHdr..cInvNo,;
             APINVHDR.cAPAcct,0 - lnApplAmnt,APINVHDR.cCurrCode,;
             APINVHDR.nExrate, APINVHDR.nCurrUnit,;
             0 - ROUND(lnApplAmnt &lcExSin1 APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2),;
             'A', lcFisFYear,lcFspPrdId,lcApDbSess,gcUser_ID,;
             ldCurrDate,gfGetTime())
             *0 - ROUND(lnApplAmnt * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT,2),;
  *E300316,1 (End)
  *E300296,1 M.H 10/22/95 Add the Currency Rate & Unit to the replace.
  * Variable to calculate the diffrence of the exchange rate amount.
  *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)
  *lnExDifAmnt = ROUND((APINVHDR.NEXRATE / APINVHDR.NCURRUNIT - &lc_InvHdr..NEXRATE / &lc_InvHdr..NCURRUNIT ) * lnApplAmnt,2)

  *B603676,1 06/08/2000 Calculate the Currency Exch. difference [Begin]
  *lnExDifAmnt = ROUND((APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT - &lc_InvHdr..NEXRATE &lcExSin2 &lc_InvHdr..NCURRUNIT ) &lcExSin1 lnApplAmnt,2)
  lnDiffLin2 = 0 - ROUND(lnApplAmnt &lcExSin1 APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2)
  lnExDifAmnt = -(lnDiffLin1 + lnDiffLin2)
  *B603676,1 06/08/2000 [End]
  *E300316,1 (End)
  
  *E300296,1 M.H 10/22/95 Add the Currency Rate & Unit to the replace.
  * Add a new record in the APDIST file if there is a diffrence between the exchange rates.
  IF lnExDifAmnt <> 0 
    INSERT INTO APDIST ;
            (cVendCode,cInvNo,cAPDTrTyp,dAPDTrDat,;
            lAPDPost,cAPDRef,cAPDGLAct,nAPDAmnt,;
            cCurrCode,nExrate,nCurrUnit,nEqvAmnt,;
            cAPDActID,cFisFYear,cFspPrdId,cAPSessNo,;
            cAdd_User,dAdd_Date,cAdd_Time);
      VALUES(APINVHDR.cVendCode,APINVHDR.cInvNo,'A',ldAppDate,;
             .F.,&lc_InvHdr..cInvNo,lcExDifAcc,lnExDifAmnt,;
             gcBaseCurr,1,1,lnExDifAmnt,'J',lcFisFYear,;
             lcFspPrdId,lcApDbSess,gcUser_ID,ldCurrDate,gfGetTime())

    *B608103,1 NNA 05/28/2007 (Begin) Update the Vendor Balance with the difference amount
    SELECT APVENDOR
    REPLACE Nvenbal   WITH Nvenbal  + lnExDifAmnt,;
            Nvencpur  WITH Nvencpur + lnExDifAmnt
    SELECT APDIST
    *B608103,1 NNA (End)

  ENDIF

  *E300296,1 M.H 10/22/95 Add the Currency Rate & Unit to the replace.
  IF lnDiscAmnt > 0
    *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)
    INSERT INTO APDIST ;
              (cVendCode,cInvNo,cAPDTrTyp,dAPDTrDat,lAPDPost,cAPDRef,;
               cAPDGLAct,nAPDAmnt,cCurrCode,nExrate,nCurrUnit,nEqvAmnt,;
               cAPDActID,cFisFYear,cFspPrdId,cAPSessNo,cAdd_User,;
               dAdd_Date,cAdd_Time);
        VALUES(APINVHDR.cVendCode,APINVHDR.cInvNo,'A',ldAppDate,;
               .F., &lc_InvHdr..cInvNo,lcDiscAcct,0-lnDiscAmnt,;
               &lc_InvHdr..cCurrCode,&lc_InvHdr..nExrate,&lc_InvHdr..nCurrUnit,;
               0-ROUND(lnDiscAmnt &lcExSin1 &lc_InvHdr..NEXRATE &lcExSin2 &lc_InvHdr..NCURRUNIT,2),;
               'S', lcFisFYear,lcFspPrdId,lcApDbSess,gcUser_ID,ldCurrDate,;
               gfGetTime())
               *0-ROUND(lnDiscAmnt * &lc_InvHdr..NEXRATE / &lc_InvHdr..NCURRUNIT,2),;
    *E300316,1 (End)             
  ENDIF 
  llApplied = .T.
  
ENDIF
=gfRLock("APVENDOR,APINVHDR,&lc_InvHdr.,APVENHST",.F.)
SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
RETURN llApplied

*!**************************************************************************
*!
*!        Function : lfvApply
*!
*!**************************************************************************
*  Valid function for push button : < Apply > from partial application 
*  screen
*
FUNCTION lfvApply

PRIVATE lcMessg, lnDivision

*** proceed with the application process
llApply    = .T.
DO CASE
  *** If the amount to be applied is less than or equal to zero,
  *** present the following message and do not apply.
  CASE lnPAplAmnt <= 0
    *** Message :  "     The amount to be applied should be greater    "
    ***            "     than zero.                                    " 
    =gfModalGen("TRM04029B00000","DIALOG")
    llApply = .F.
  
  *** If a GL company exists in the setup file (APSETUP),
  *** Look for the discount account in its chart of accounts
  *** file
  CASE lnPAplDisc > 0 .AND. llApGlLink
    *** If no account is entered, present the following message
    IF VAL(STRTRAN(lcDiscAcct,'-','0')) = 0 .OR.;
       EMPTY(STRTRAN(lcDiscAcct,'-'))
       *** Message :  "  You have to enter ð.     "
       ***                   <   OK   >
       =gfModalGen("TRM04066B00000","DIALOG",lcTDscAcc)
       llApply = .F.
       _CUROBJ = OBJNUM(lcDiscAcct)
    ELSE
      *** If the acccount is not found in the chart of accounts
      *** file of the link company, present the following message.
      *** Message :  "    The discount account in ð  does not      "
      ***            "    exist in the chart of accounts. You      "
      ***            "    have to enter another discount account,  "
      ***            "    or clear the discount amount.            "
      ***                        <   OK   >
  
      IF llApGlLink .AND. !SEEK(lcDiscAcct, 'lcLinkChar')
        *E301077,79 AMM Open file 
        =gfOpenFile(gcDataDir+'APDIV','DIVISION','SH')
        *E301077,79 AMM end
        IF  SEEK(&lc_InvHdr..cDivision,'APDIV') ;
           .AND. !EMPTY(APDIV.cDiscAcct) 

          *E300643,1 Change this line for the changes we have made
          *          to (gfCodDes) [Begin]
          *lcMessg    = lcTDiv + ' ' + gfCodDes(&lc_InvHdr..cDivision)
          lcMessg    = lcTDiv + ' ' + gfCodDes(&lc_InvHdr..cDivision , 'CDIVISION')
          *E300643,1 Change this line [End]
          
        ELSE
          lcMessg    = lcTApSetup    
        ENDIF            
        
        =gfModalGen("TRM04038B00000","DIALOG",lcMessg)
        llApply = .F.
        _CUROBJ = OBJNUM(lcDiscAcct)
      ENDIF
    ENDIF    
    
  *** Check if the debit memo has approved amounts that may be
  *** cleared by application.
  *** If it does, present the following message 
  *** Present the following dialog :
  *** Message :  "   Debit memo No. ð has a total approved amount   "
  ***            "   of ð.  Applying this debit memo to invoice     "
  ***            "   No. ð will clear its approved amount.          "
  ***                       < Apply >   < Cancel >
  CASE lnDM_AprAm <> 0 .AND. ;
    lnPAplAmnt > ABS(lnDM_OpnAm - lnDM_AprAm) 

    IF gfModalGen("QRM04143B04007","DIALOG",;
              ALLTRIM(APINVHDR.cInvNo) + '|' + ;
              ALLTRIM(STR(lnDM_AprAm, 16, 2)) + '|' +;
              ALLTRIM(&lc_InvHdr..cInvNo)) = 1
      llClearDM = .T.
    ELSE
      llApply = .F.
    ENDIF
ENDCASE

IF llApply 
  CLEAR READ
ENDIF

*!**************************************************************************
*!
*!        Function : lfwDates
*!
*!**************************************************************************
*  WHEN function for get field pplication date (ldAppDate)
*
FUNCTION lfwDates

ldOldDate = EVALUATE(SYS(18))

*!**************************************************************************
*!
*!        Function : lfvAppDate
*!
*!**************************************************************************
*  VALID function for get field application date (ldAppDate)
*
FUNCTION lfvAppDate
PRIVATE lcYear, lcPeriod

*** This validation function should be executed right after the user 
*** selects a valid vendor.
IF !EMPTY(ldAppDate) .AND. ldAppDate <> ldOldAppDt
  lcYear   = lcFisFYear
  lcPeriod = lcFspPrdId 
  *** If the date is invalid, empty the date field, 
  *** as well as fiscal year, and fiscal periods variables.
  IF !lfvDtMsg(gcPrnt_Cmp, @lcPeriod, @lcYear)
    lcFisFYear  = SPACE(4)
    lcFspPrdId  = SPACE(2)
    ldAppDate  = {}     
    _CUROBJ = OBJNUM(ldAppDate)
  ELSE 
    lcFspPrdId = lcPeriod 
    lcFisFYear =  lcYear 
  ENDIF  
  ldOldAppDt = ldAppDate
  llPassDate = .T.
ENDIF

*!**************************************************************************
*!
*!        Function : lfwAmnt
*!
*!**************************************************************************
*  WHEN function for get fields applied amount, discount amount 
*  from partial application screen
*
FUNCTION lfwAmnt

lnOldVal  = EVALUATE(SYS(18))
 
*!**************************************************************************
*!
*!        Function : lfwOldVals
*!
*!**************************************************************************
*  WHEN function for get fields applied amount, discount amount 
*  from partial application screen
*
FUNCTION lfwOldVals

lcOldVal  = EVALUATE(SYS(18))
SHOW GET laData[1]
SHOW GET laData[2]
SHOW GET laData[3] 
SHOW GET ldAppDate 
 
*!**************************************************************************
*!
*!        Function : lfvAplAmnt
*!
*!**************************************************************************
*  Valid function for get fields applied amount, discount amount 
*  from partial application screen
*
FUNCTION lfvAplAmnt

PRIVATE llValid
IF lnPAplAmnt <> lnOldVal
  llValid = .T.
  DO CASE
    *** If the applied amount is negative, present a message
    *** and return
    CASE lnPAplAmnt < 0
      *** Message : "   Negative values are not allowed.   "  
      ***                      <   OK   >
      =gfModalGen("TRM04087B00000","DIALOG")
      llValid = .F.  
      
    *** If the amount to be applied is greater than the open
    *** debit memo amount, present a message and do not accept.
    CASE lnPAplAmnt > ABS(lnDM_OpnAm)
      *** Message : " ð amount cannot be greater than the open ðamount. "
      ***                      <   OK   >
      =gfModalGen("TRM04015B00000","DIALOG",lcAplDbMm)
      llValid = .F.  
      
    *** If the sum of the amount to be applied and the discount
    *** is greater than the invoice open amount, present a message
    *** and do not accept
    CASE (lnPAplAmnt + lnPAplDisc) > lnIn_OpnAm 
      *** Message : " ð amount cannot be greater than the open ðamount.
      ***                      <   OK   >
      =gfModalGen("TRM04015B00000","DIALOG",lcAplInv)
      llValid = .F.  
      
    *** If the sum of the amount to be applied and the discount
    *** is greater than the difference between the invoice open amount, 
    *** and the invoioce approved amount, present a message
    *** and proceed
    CASE lnIn_AprAm > 0 ;
         .AND. (lnPAplAmnt + lnPAplDisc) > (lnIn_OpnAm - lnIn_AprAm) 
      *** Message :  "  Total amount applied to this invoice will   " 
      ***            "  clear its approved amounts.                 "
      ***                      <   OK   >
      =gfModalGen("TRM04030B00000","DIALOG")
      llClearApr  = .T.
   ENDCASE
   IF !llValid
     lnPAplAmnt = lnOldVal
     _CUROBJ    = OBJNUM(lnPAplAmnt)
   ENDIF
ENDIF  
 
*!**************************************************************************
*!
*!        Function : lfvDscAmnt
*!
*!**************************************************************************
*  Valid function for get fields applied amount, discount amount 
*  from partial application screen
*
FUNCTION lfvDscAmnt
PRIVATE llValid

IF lnPAplDisc <> lnOldVal
  llValid = .T.
  DO CASE

    *** If the discount amount is negative, present a message
    *** and return
    CASE lnPAplDisc < 0
      *** Message : "   Negative values are not allowed.   "  
      ***                      <   OK   >
      =gfModalGen("TRM04087B00000","DIALOG")
      llValid = .F.  
      
    *** If the sum of the amount to be applied and the discount
    *** is greater than the invoice open amount, present a message
    *** and do not accept
    CASE (lnPAplAmnt + lnPAplDisc) > lnIn_OpnAm 
      *** Message : " ð amount cannot be greater than the open ðamount.
      ***                      <   OK   >
      =gfModalGen("TRM04015B00000","DIALOG",lcAplInv)
      llValid = .F.  
      
    *** If the sum of the amount to be applied and the discount
    *** is greater than the difference between the invoice open amount, 
    *** and the invoioce approved amount, present a message
    *** and proceed
    CASE (lnPAplAmnt + lnPAplDisc) > (lnIn_OpnAm - lnIn_AprAm) 
      *** Message :  "  Total amount applied to this invoice will   " 
      ***            "  clear its approved amounts.                 "
      ***                      <   OK   >
      =gfModalGen("TRM04030B00000","DIALOG")
  ENDCASE
  IF !llValid
    lnPAplDisc = lnOldVal
    _CUROBJ    = OBJNUM(lnPAplDisc)
  ENDIF
  IF lnPAplDisc > 0 
    SHOW GET lcDiscAcct ENABLE
    SHOW GET lcAccDesc  ENABLE
    SHOW GET ibAccts    ENABLE
  ELSE
    SHOW GET lcDiscAcct DISABLE
    SHOW GET lcAccDesc  DISABLE
    SHOW GET ibAccts    DISABLE
  ENDIF   
  RETURN 1
ENDIF  

 
*!**************************************************************************
*!
*!      Function: lfwDisAcct
*!
*!**************************************************************************
*  WHEN function for the get field lcDiscAcct
*
FUNCTION lfwDisAcct

lcOldAcct = lcDiscAcct
lcOldDesc = lcAccDesc

*!**************************************************************************
*!
*!      Function: lfvDisAcct
*!
*!**************************************************************************
*
FUNCTION lfvDisAcct

IF llBrowse .OR. lcDiscAcct <> lcOldAcct 
  IF llApGlLink
    ** Get a new account and its description
    IF !lfApAcs(@lcAccDesc,llBrowse)  
      lcDiscAcct = lcOldAcct
      lcAccDesc  = lcOldDesc 
    ENDIF
    SHOW GET lcDiscAcct
    SHOW GET lcAccDesc
  ELSE
    IF '?' $ lcDiscAcct .OR. EMPTY(STRTRAN(STRTRAN(lcDiscAcct,'-'),'0'))
      lcDiscAcct = lcOldAcct
      SHOW GET lcDiscAcct
    ENDIF 
  ENDIF
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfvInvScp
*!
*!**************************************************************************
*
FUNCTION lfvInvScp

PRIVATE lcOldDiv, lcOldDOpt, lcOldInvRef, lcOldPPr, lcOldPMeth, lcOldPOpt;
        ldOldDuF, ldOldDuT, ldOldDsF, ldOldDsT, lnOldrbDat,;
        lnOldpuPM, lnOldpuDv     

** If all invoices are to be selected, 
lcDebMem   = APINVHDR.cInvNo
lcInvoice  = &lc_InvHdr..cInvNo
IF rbInvScope = 1 
  IF lnOldrbScp = 2
    *** Store filter parameters
    SET ORDER TO 0 IN (lc_InvHdr)
    lcInvFlt   = lcInvFlt_1
    SET ORDER TO TAG VENDINV IN (lc_InvHdr)
  ENDIF  

** Else, if a filter is to be selected, 
ELSE
  lcDisabClr =  SCHEME(5,1)     && Color of normal text in dialogs
  lcDisabClr = STRTRAN(SUBSTR(lcDisabClr,AT('/',lcDisabClr)+1),'*','+');
               +SUBSTR(lcDisabClr,AT('/',lcDisabClr))
 
  llInvOk   = .F.
 
  ** Save old data so as to restore it if the user cancels
  lcOldDiv    = lcDivision
  lcOldDOpt   = lcDivOpt
  lcOldInvRef = lcInvRef
  lcOldPPr    = lcPayPrior
  lcOldPMeth  = lcPayMeth
  lcOldPOpt   = lcPMethOpt  
  ldOldDuF    = ldDueFrom
  ldOldDuT    = ldDueTo
  ldOldDsF    = ldDiscFrom
  ldOldDsT    = ldDiscTo
  lcOldDtFlt  = lcDateFlt
  lnOldrbDat  = rbDates
  lnOldpuPM   = puPayMeth
  lnOldpuDv   = puDivision
  
  ** Restore old filter parameters if the previous selection is 
  ** 'All invoices'
  IF lnOldrbScp = 1
    SET ORDER TO 0 IN (lc_InvHdr)
    lcInvFlt   = lcOldInFlt
    SET ORDER TO TAG VENDINV IN (lc_InvHdr)
    *** Refresh the previous filter
    SHOW WINDOW (lcBrTtl2) REFRESH SAME
  ENDIF
  
  IF rbDates = 1
    lcDueState = "ENABLE"
    lcDscState = "DISABLE"
  ELSE
    lcDscState = "ENABLE"
    lcDueState = "DISABLE"
  ENDIF  
  lcHidObjClr = SUBSTR(SCHEME(5,1),ATC('/',SCHEME(5,1))+1)
  lcHidObjClr = lcHidObjClr + '/' + STRTRAN(lcHidObjClr, '*', '+')
  PUSH KEY
  ON KEY
  *E300683,1 Call *.SPR from screens directory
  * DO APAPLDBI.SPR 
  DO (gcScrDir + gcWinAppl + '\APAPLDBI.SPR')
  *E300683,1 end          
  
  RELEASE PAD _BROWSE OF _MSYSMENU
  POP KEY
    
  ** If OK, store the current filter for latter use.
  IF llInvOk 
    *** Refresh the previous filter
    SHOW WINDOW (lcBrTtl2) REFRESH SAME
    lcOldInFlt = lcInvFlt
  ** If Cancelled, restore old data
  ELSE  
    lcDivision = lcOldDiv    
    lcDivOpt   = lcOldDOpt
    lcInvRef   = lcOldInvRef 
    lcPayPrior = lcOldPPr    
    lcPayMeth  = lcOldPMeth  
    lcPMethOpt = lcOldPOpt
    ldDueFrom  = ldOldDuF    
    ldDueTo    = ldOldDuT    
    ldDiscFrom = ldOldDsF    
    ldDiscTo   = ldOldDsT    
    lcDateFlt  = lcOldDtFlt  
    SET ORDER TO 0 IN (lc_InvHdr)
    lcInvFlt   = lcOldInFlt
    SET ORDER TO TAG VENDINV IN (lc_InvHdr)
    rbDates    = lnOldrbDat
    puPayMeth  = lnOldpuPM
    puDivision = lnOldpuDv
  ENDIF

ENDIF  

=lfRefrObj()

** Save the current radio button selection
lnOldrbScp = rbInvScope

*!**************************************************************************
*!
*!      Function: lfvInvRef
*!
*!**************************************************************************
*  Valid function for invoice reference field in APAPLDBI screen ( Select invoices)
*
FUNCTION lfvInvRef
lcInvRef = PADR(ALLTRIM(lcInvRef),FSIZE('cInvRef','APINVHDR'))
SHOW GET lcInvRef

*!**************************************************************************
*!
*!      Function: lfvInvOk
*!
*!**************************************************************************
*  Valid function for push button OK in APAPLDBI screen ( Select invoices)
*
FUNCTION lfvInvOk

*** Prepare the date filter according to the final selection :
IF rbDates = 1
  IF !EMPTY(ldDueFrom)
    lcDateFlt = IIF(!EMPTY(ldDueTo),;
                  [.AND. BETWEEN(dInvDuDat, ldDueFrom, ldDueTo)],;
                  [.AND. dInvDuDat >= ldDueFrom])
   ELSE
     lcDateFlt = IIF(!EMPTY(ldDueTo),;
                     [.AND. dInvDuDat <= ldDueTo], " ")
   ENDIF
ELSE
  IF !EMPTY(ldDiscFrom)
    lcDateFlt = IIF(!EMPTY(ldDiscTo),;
                  [.AND. BETWEEN(dInvDate+nTerDiscD, ldDiscFrom, ldDiscTo)],;
                  [.AND. dInvDate+nTerDiscD >= ldDiscFrom])
  ELSE
    lcDateFlt = IIF(!EMPTY(ldDiscTo),;
                    [.AND. dInvDate+nTerDiscD <= ldDiscTo], " ")
  ENDIF
ENDIF  
    
*** Prepare the rest of the filter expression
SET ORDER TO 0 IN (lc_InvHdr)
lcInvFlt   = lcInvFlt_1 + ;
             IIF(!EMPTY(lcInvRef),;
                 [.AND. LIKE(STRTRAN(UPPER(lcInvRef),' ','?'),UPPER(cInvRef))],"") +;
             IIF(!EMPTY(lcPayPrior),[.AND. cVenPrior = lcPayPrior],"") +;
             IIF(!('*'$ lcDivOpt), [.AND. cDivision = lcDivOpt],"") + ;
             IIF(!EMPTY(lcPMethOpt),[.AND. cVenPMeth = lcPMethOpt],"") + ;
             lcDateFlt  
SET ORDER TO TAG VENDINV IN (lc_InvHdr)
*** Check if this filter produces invoices
*** If it is not, present the following message and return:
*** Message :  "    No open invoices matching the selected invoices    "  
***            "    criteria for vendor ð.                             "
***                               <   OK   >
*B801196,4 AMM start
*GO TOP IN (lc_InvHdr)
lnCurAlias = SELECT(0)
SELECT (lc_InvHdr)
lcOldTag=TAG()
SET ORDER TO
LOCATE
SET ORDER TO TAG &lcOldTag.
SELECT (lnCurAlias)
*B801196,4 AMM end
IF EOF(lc_InvHdr) 
  =gfModalGen("TRM04035B00000","DIALOG",ALLTRIM(laData[1]))
ELSE  
  llInvOk = .T. 
  CLEAR READ  
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvPayPr
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvPayPr

IF lcPayPrior = '0' 
  lcPayPrior = ' '
  lcPriorFlt = '*'
  SHOW GET lcPayPrior
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvDivision
*!
*!**************************************************************************
*
FUNCTION lfvDivision

DO CASE
  CASE _DOS

    *E300643,1 Change this line for the changes we have made to 
    *          SYCCODES [Begin]
    *lcDivOpt =gfActPop(2,28,8,61,'SYCCODES','cCode_No','cDiscrep',@lcDivision)
    lcDivOpt =gfActPop(2,28,8,61,'CODES','cCode_No','cDiscrep',@lcDivision)
    *E300643,1 Change this line [End]

    SHOW GETS
  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made to 
    *          SYCCODES [Begin]
    *lcDivision = SYCCODES.cdiscrep
    *lcDivOpt   = SYCCODES.cCode_No
    lcDivision = CODES.cdiscrep
    lcDivOpt   = CODES.cCode_No
    *E300643,1 Change this lines [End]
    
    SHOW GET lcDivision
ENDCASE

IF _WINDOWS
  DEACTIVATE POPUP puDivision
ENDIF  


*!**************************************************************************
*!
*!      Function: lfvPayMeth
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvPayMeth

PRIVATE lnPopLen

DO CASE
  CASE _DOS
    lnPopLen     = ALEN(laPayMeth,1)
    lcPMethOpt   = gfActPop(4, 48, IIF(lnPopLen>5,11,6+lnPopLen), 68,;
                            'laPayMeth',2,1,@lcPayMeth)
    SHOW GETS
  CASE _WINDOWS
    lcPMethOpt   = laPayMeth[puPayMeth,2]
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvDueDate
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvDueDate

PRIVATE lcCurrObj, lcCentSet

IF !EMPTY(ldDueFrom) .AND. !EMPTY(ldDueTo) ;
  .AND. (ldDueFrom > ldDueTo)
  *** If due from date (ldDueFrom) > due to date(ldDueTo)
  *** present the following message and return the old dalue.
  *** Message : "  The ð date cannot be less than the ð date.  "
  ***                           <   OK   >
  lcCentSet = SET('CENTURY')
  SET CENTURY ON
  =gfModalGen("TRM04028B00000","DIALOG",lcDueTo+'|'+lcDueFrom)
  SET CENTURY &lcCentSet             
  lcCurrObj = SYS(18)
  &lcCurrObj = ldOldDate
  SHOW GET &lcCurrObj
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvDscDate
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvDscDate
PRIVATE lcCurrObj, lcCentSet

IF !EMPTY(ldDiscFrom) .AND. !EMPTY(ldDiscTo);
  .AND. ldDiscFrom > ldDiscTo 
  *** If discount from date (ldDiscFrom) > discount to date(ldDiscTo)
  *** present the following message and return the old dalue.
  *** Message : "  The ð date cannot be less than the ð date.  "
  ***                           <   OK   >
  lcCentSet = SET('CENTURY')
  SET CENTURY ON
  =gfModalGen("TRM04028B00000","DIALOG", lcDiscTo+'|'+lcDiscFrom)
  SET CENTURY &lcCentSet                                
  lcCurrObj = SYS(18)
  &lcCurrObj = ldOldDate
  SHOW GET &lcCurrObj
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvDates
*!
*!**************************************************************************
*  Valid function for push bitton OK in AOAOLDBI screen ( Select invoices)
*
FUNCTION lfvDates
IF rbDates = 1
  SHOW GET ldDueFrom   ENABLE
  SHOW GET ldDueTo     ENABLE
  SHOW GET ldDiscFrom DISABLE 
  SHOW GET ldDiscTo   DISABLE 
ELSE
  SHOW GET ldDueFrom  DISABLE 
  SHOW GET ldDueTo    DISABLE 
  SHOW GET ldDiscFrom  ENABLE
  SHOW GET ldDiscTo    ENABLE
ENDIF  
SHOW GETS

*!**************************************************************************
*!
*!      Function: lfVldAppDt
*!
*!**************************************************************************
*  Application date validation
*
FUNCTION lfVldAppDt

PRIVATE llVldDate
llVldDate = .T.

DO CASE
  *** If application date is empty, return .F. and present
  *** the following message.
  CASE EMPTY(ldAppDate)
    llVldDate = .F.
    *** Message :  "  You have to enter ð.     "
    ***                   <   OK   >
    =gfModalGen("TRM04066B00000","DIALOG",lcTAppDate)
    ACTIVATE WINDOW (lcApDbWin1)
    _CUROBJ = OBJNUM(ldAppDate)

  *** If application date has not been validated, and is now found
  *** to be invalid, return .F. and present the following message.
  CASE !llPassDate .AND. ;
       !lfvDtMsg(gcPrnt_Cmp, @lcFspPrdId, @lcFisFYear, ldAppDate)
    llVldDate  = .F.
    ldAppDate  = {}
    lcFisFYear = SPACE(4)
    lcFspPrdId = SPACE(2)
    ACTIVATE WINDOW (lcApDbWin1)
    _CUROBJ    = OBJNUM(ldAppDate)

ENDCASE

IF !llVldDate
  lcCurAlias = ALIAS()
  SELECT APINVHDR
  =gfObj_Lock(.F.)
  SELECT (lc_InvHdr)
  =gfObj_Lock(.F.)
  SELECT (lcCurAlias)
ENDIF
RETURN llVldDate

*!**************************************************************************
*!
*!      Function : lfVldAppl  
*!
*!**************************************************************************
*  Application validation
*
FUNCTION lfVldAppl
PRIVATE llVldDate

llVldDate = .T.
DO CASE
  *** Check if the current debit memo has a zero priority (on hold)
  CASE APINVHDR.cVenPrior = "0" 
    lcDebitMem = SUBSTR(lcBrTtl1,1,LEN(lcBrTtl1)-1)
    *** Message : "  ð has payment priority 0. ð is on hold. "
    ***                  <  OK  >
    =gfModalGen("TRM04059B00000","DIALOG",;
    lcDebitMem+" "+ALLTRIM(APINVHDR.cInvNo)+"|"+;
    lcDebitMem+" "+ALLTRIM(APINVHDR.cInvNo))
    llVldDate = .F.

  *** Check if the current invoice has a zero priority (on hold)
  CASE &lc_InvHdr..cVenPrior = "0" 
    lcInv = SUBSTR(lcBrTtl2,1,LEN(lcBrTtl2)-1)
    *** Message : "  ð has payment priority 0. ð is on hold. "
    ***                  <  OK  >
    =gfModalGen("TRM04059B00000","DIALOG",;
    lcInv +" "+ALLTRIM(&lc_InvHdr..cInvNo)+"|"+;
    lcInv +" "+ALLTRIM(&lc_InvHdr..cInvNo))
    llVldDate = .F.

  *** If application date is less than the debit memo date, 
  *** return .F. and present the following message
  CASE ldAppDate < APINVHDR.dInvDate
    llVldDate = .F.
    *** Message : "  The application date cannot be less than the    " 
    ***           "  ð date. Cannot apply debit memo ð to ð. "
    ***                              <  OK  >
    =gfModalGen("TRM04036B00000","DIALOG",lcDebtMemo+'|'+;
                 ALLTRIM(APINVHDR.cInvNo)+'|'+lcInv+' '+ALLTRIM(&lc_InvHdr..cInvNo))
  
  *** If application date is less than the invoice date, 
  *** return .F. and present the following message
  CASE ldAppDate < &lc_InvHdr..dInvDate
    llVldDate = .F.
    *** Message : "  The application date cannot be less than the    " 
    ***           "  ð date. Cannot apply debit memo ð to invoice ð. "
    ***                              <  OK  >
    =gfModalGen("TRM04036B00000","DIALOG",lcInv+'|'+;
                  ALLTRIM(APINVHDR.cInvNo)+'|'+ALLTRIM(&lc_InvHdr..cInvNo))
ENDCASE
RETURN llVldDate    

*!**************************************************************************
*!
*!      Function: lfRefrObj
*!
*!**************************************************************************
*  Function to refresh objects according to available invoices and/or
*  debit memos
*
FUNCTION lfRefrObj

PRIVATE lcOldFlt 
** If there are no debit memos for this vendor,
** which may occur in 3 cases :
** 1. Selecting a vendor having no debit memos
** 2. Applying until there are no more debit memos
** 3. Debit memos getting out of scope as a result of an operation from
**    another session.

** If there are no open invoices for this vendor,
** which may occur in 4 cases :
** 1. Selecting a vendor having no open invoices
** 2. Applying until there are no more open invoices
** 3. No invoices matching a selection critera created
**    through the 'Select invoices...' screen 
** 4. Invoices getting out of scope as a result of an operation from
**    another session.


*GO TOP IN APINVHDR    && REN Remarked
*B600928,1 M.H 01/14/96 Fix the bug of the pointer in the detail brow always point to the first record.
*GO TOP IN (lc_InvHdr)
*B600928,1 End.

** REN Fill lcInvoice variable here
lcDebMem   = APINVHDR.cInvNo
lcInvoice  = &lc_InvHdr..cInvNo
** REN end.

*B600928,1 M.H 01/14/96 Fix the bug of the disable the apply buttons when there is no records to display.
** Reposition record pointers :

*B801196,4 AMM start
*IF !SEEK(laData[1] + lcDebMem, 'APINVHDR')
  *GO TOP IN APINVHDR
  *lnCurAlias = SELECT(0)
  *SELECT APINVHDR
  *lcOldTag=TAG()
  *SET ORDER TO  
  *LOCATE
  *SET ORDER TO TAG &lcOldTag.
  *SELECT (lnCurAlias)    
*ENDIF  

*IF !SEEK(laData[1] + lcInvoice, lc_InvHdr)
  *GO TOP IN (lc_InvHdr)
  *lnCurAlias = SELECT(0)
  *SELECT (lc_InvHdr)
  *lcOldTag=TAG()
  *SET ORDER TO
  *LOCATE
  *SET ORDER TO TAG &lcOldTag.
  *SELECT (lnCurAlias)
*ENDIF  
*B801196,4 AMM end

*B600928,1 M.H End.

** REN end
llDisObj   = EOF(lc_InvHdr) .OR. EOF('APINVHDR') 
** REN 
*llDisVen   = .F.
llDisVen   = !EMPTY(laData[1])
*REN end
llDisDM    = EOF('APINVHDR')
llDisInv   = EOF(lc_InvHdr)
  
lcDMObj    = IIF(llDisDM ,"DISABLE","ENABLE")             
lcInvObj   = IIF(llDisInv,"DISABLE","ENABLE")             
** REN
lcVenObj   = IIF(EMPTY(laData[1]), 'DISABLE', 'ENABLE')
** REN end.
IF llDisObj
  lcDispObj = "DISABLE"
  lcSaycol  = lcHidObjNrm
  lcTopLine = lcBackGr 
  lcBotLine = lcBAckGr 
ELSE
  lcDispObj = "ENABLE"
  lcSaycol  = lcDisCont
  lcTopLine = lcTopLeft
  lcBotLine = lcBotRight
ENDIF

IF EOF('APINVHDR') .AND. WVISIBLE('CWRAPAPLDM')
  =gfChClose('CWRAPAPLDM')
ENDIF

IF EOF(lc_InvHdr) .AND. WVISIBLE('CWRAPAPLIN')
  =gfChClose('CWRAPAPLIN')
ENDIF

SHOW GET  pbVenSum   &lcVenObj
SHOW GET  pbInvSum   &lcInvObj 
SHOW GET  pbDbMemSum &lcDMObj 
SHOW GET  rbInvScope &lcVenObj
SHOW GET  ldAppDate  &lcDispObj
SHOW GET  pbApFully  &lcDispObj
SHOW GET  pbApPart   &lcDispObj
SHOW GET  pbApAll    &lcDispObj
SHOW GET  pbClose    ENABLE

*B600928,1 M.H 01/14/96 Fix the bug of the disable the apply buttons when there is no records to display.
** Reposition record pointers :
*IF !SEEK(laData[1] + lcDebMem, 'APINVHDR')
*  GO TOP IN APINVHDR
*ENDIF  
*IF !SEEK(laData[1] + lcInvoice, lc_InvHdr)
*  GO TOP IN (lc_InvHdr)
*ENDIF  
*B600928,1 M.H End.

lnBrRecN1   = RECNO('APINVHDR') 
lnBrRecN2   = RECNO(lc_InvHdr) 
SHOW WINDOW (lcBrTtl1) REFRESH SAME
SHOW WINDOW (lcBrTtl2) REFRESH SAME
*SHOW WINDOW (lcApDbWin1) REFRESH 
=lfRefresh(lcApDbWin1+IIF(WVISIBLE('CWRAPAPLVN'), ",CWRAPAPLVN", "")+;
                      IIF(WVISIBLE('CWRAPAPLDM'), ",CWRAPAPLDM", "")+;
                      IIF(WVISIBLE('CWRAPAPLIN'), ",CWRAPAPLIN", ""))

*!**************************************************************************
*!
*!      Procedure: lpTab
*!
*!*******************************************************************************
*  This procedure is called whenever the Tab key is pressed to control
*  objects flow with the browses
*
PROCEDURE lpTab
IF _DOS
  HIDE MENU _MSYSMENU 
  SHOW MENU _MSYSMENU 
ENDIF  
ON KEY LABEL TAB lnDummmi = 1
DO CASE
  CASE WONTOP(lcApDbWin1) 
    DO CASE
      CASE INLIST(_CUROBJ, OBJNUM(laData[1]), OBJNUM(laData[2]))
        IF EVALUATE(SYS(18)) <> lcOldVal
          =lfvData()
        ENDIF
        _CUROBJ = IIF(_CUROBJ = OBJNUM(laData[1]), ;
                       OBJNUM(laData[2]), OBJNUM(laData[3]))
      CASE _CUROBJ = OBJNUM(laData[3])
        IF EVALUATE(SYS(18)) <> lcOldVal
         =lfvData()
        ENDIF
        IF llDisObj
          ACTIVATE WINDOW (lcBrTtl1)
        ELSE
          _CUROBJ = OBJNUM(ldAppDate)
        ENDIF  
      CASE _CUROBJ = OBJNUM(ldAppDate)
        =lfvAppDate()
        ACTIVATE WINDOW (lcBrTtl1)
    ENDCASE
       
  CASE WONTOP(lcBrTtl1)   
    ACTIVATE WINDOW (lcBrTtl2)
    
  CASE WONTOP(lcBrTtl2)   
    ACTIVATE WINDOW (lcApDbWin4)
    _CUROBJ = OBJNUM(pbApFully)

  CASE WONTOP(lcApDbWin4) .AND. _CUROBJ  = OBJNUM(pbClose)
    ACTIVATE WINDOW (lcApDbWin1)
    _CUROBJ        = OBJNUM(laData[1])    

  *** Otherwise, go to the next object
  OTHERWISE
    _CUROBJ    	   = _CUROBJ + 1
ENDCASE
ON KEY LABEL TAB DO lpTab


*!**************************************************************************
*!
*!      Procedure: lpShiftTab
*!
*!*******************************************************************************
*
*  This procedure is called whenever the Tab key is pressed to control
*  objects flow with the browses
*
PROCEDURE lpShiftTab
IF _DOS
  HIDE MENU _MSYSMENU SAVE
  SHOW MENU _MSYSMENU SAVE
ENDIF  
ON KEY LABEL BACKTAB lnDummmi = 1

DO CASE
  
  CASE WONTOP(lcApDbWin1)
    DO CASE
      CASE _CUROBJ = OBJNUM(ldAppDate)
        =lfvAppDate()
        _CUROBJ = OBJNUM(laData[3])
        
      CASE INLIST(_CUROBJ, OBJNUM(laData[2]), OBJNUM(laData[3]))
        IF EVALUATE(SYS(18)) <> lcOldVal
          =lfvData()
        ENDIF
        _CUROBJ = IIF(_CUROBJ = OBJNUM(laData[3]), ;
                       OBJNUM(laData[2]), OBJNUM(laData[1]))
      CASE _CUROBJ = OBJNUM(laData[1])
        IF EVALUATE(SYS(18)) <> lcOldVal
         =lfvData()
        ENDIF
        ACTIVATE WINDOW (lcApDbWin4)
        _CUROBJ   = OBJNUM(pbClose)
    ENDCASE

  CASE WONTOP(lcBrTtl1)   
    ACTIVATE WINDOW (lcApDbWin1)
   _CUROBJ        = IIF(llDisObj, OBJNUM(laData[3]), OBJNUM(ldAppDate))

  CASE WONTOP(lcBrTtl2)   
    ACTIVATE WINDOW (lcBrTtl1)
   
  CASE WONTOP(lcApDbWin4) 
    DO CASE
      CASE _CUROBJ = OBJNUM(pbClose)
        IF EMPTY(laData[1]) .OR. llDisInv .AND. llDisScp .AND. llDisObj
          ACTIVATE WINDOW (lcBrTtl2)
        ELSE
          _CUROBJ = IIF(llDisInv, IIF(llDisDM, OBJNUM(pbVenSum),; 
                        OBJNUM(pbDbMemSum)) ,OBJNUM(pbInvSum))
        ENDIF                               

      CASE _CUROBJ = OBJNUM(pbInvSum)
        IF EMPTY(laData[1]) .OR. llDisScp .AND. llDisObj
          ACTIVATE WINDOW (lcBrTtl2)
        ELSE
          _CUROBJ = IIF(llDisDM, OBJNUM(pbVenSum),OBJNUM(pbDbMemSum)) 
        ENDIF                               

      CASE _CUROBJ = OBJNUM(pbApFully) .OR.;
        _CUROBJ = OBJNUM(pbDbMemSum) .AND. ;
        (EMPTY(laData[1]) .OR. llDisScp .AND. llDisObj) .OR. ;
        _CUROBJ = OBJNUM(pbVenSum) .AND. llDisScp .OR.;
        _CUROBJ = OBJNUM(rbInvScope) .AND. llDisObj
        ACTIVATE WINDOW (lcBrTtl2)
      OTHERWISE  
        _CUROBJ = _CUROBJ - 1
    ENDCASE
ENDCASE
ON KEY LABEL BACKTAB DO lpShiftTab

*!**************************************************************************
*!
*!      Function: lfBrowTrap
*!
*!*******************************************************************************
*  Sets key traps when a browse is active.
*  function : to enable calling the outer objects on an ALT+hot key presss
*
FUNCTION lfBrowTrap
ON KEY LABEL ESC   DO lpCtrlEntr
ON KEY LABEL Ctrl+ENTER DO lpCtrlEntr
ON KEY LABEL Alt-Y DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-P DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-A DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-L DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-S DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-V DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-D DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-I DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-C DO lpBrowKeyTrap WITH LASTKEY()

*!**************************************************************************
*!
*!      Function: lfBrowUnTrap
*!
*!*******************************************************************************
*  Removes key traps that are set when a browse is activeLT+hot key presss
*
FUNCTION lfBrowUnTrap
ON KEY LABEL ESC        &lcEscOnKey
ON KEY LABEL Ctrl+ENTER &lcCtrlEntr
ON KEY LABEL Alt-Y 
ON KEY LABEL Alt-P 
ON KEY LABEL Alt-A 
ON KEY LABEL Alt-L 
ON KEY LABEL Alt-S 
ON KEY LABEL Alt-V 
ON KEY LABEL Alt-D 
ON KEY LABEL Alt-I 
ON KEY LABEL Alt-C 

*!**************************************************************************
*!
*!      Procedure: lpCtrlEntr
*!
*!**************************************************************************
*
PROCEDURE lpCtrlEntr
IF _DOS
  HIDE MENU _MSYSMENU 
  SHOW MENU _MSYSMENU 
ENDIF  
ACTIVATE WINDOW (lcApDbWin4)
_CUROBJ  = OBJNUM(pbClose)
KEYBOARD CHR(13)

*!**************************************************************************
*!
*!      Procedure: lpBrowKeyTrap
*!
*!*******************************************************************************
*  Key traps when browsing
*
PROCEDURE lpBrowKeyTrap
PARAMETERS lnLastKey

KEYBOARD "{Z}"       && dummy key press to release menu access

DO CASE
  CASE lnLastKey = 21 .AND. !llDisObj   && Alt-Y   <   Apply fully   >  
    =lfvAppFull()
  CASE lnLastKey = 25 .AND. !llDisObj   && Alt-P   < Apply partially >
    =lfvAppPart()
  CASE lnLastKey = 30 .AND. !llDisObj   && Alt-A   <    Apply all    >
    =lfvAppAll()
  CASE lnLastKey = 38 .AND. !llDisScp   && Alt-L   () All invoices
    rbInvScope = 1
    SHOW GET rbInvScope
    =lfvInvScp()
  CASE lnLastKey = 31 .AND. !llDisScp   && Alt-S   () Selected invoices
    rbInvScope = 2
    SHOW GET rbInvScope
   =lfvInvScp()
  CASE lnLastKey = 47 .AND. !EMPTY(laData[1])   && Alt-V   <   Vendor   >
    ACTIVATE WINDOW (lcApDbWin4)
    =gfActWind('CWRAPAPLVN',lcVnInqTtl,gcBaseWind)
  CASE lnLastKey = 32 .AND. !llDisDM    && Alt-D   < Debit memo >
    ACTIVATE WINDOW (lcApDbWin4)
    =gfActWind('CWRAPAPLDM',lcDmInqTtl,gcBaseWind)
  CASE lnLastKey = 23 .AND. !llDisInv   && Alt-I   <   Invoice  >
    ACTIVATE WINDOW (lcApDbWin4)
    =gfActWind('CWRAPAPLIN',lcInInqTtl,gcBaseWind)
  CASE lnLastKey = 46    && Alt-C   <    Close   >
    ACTIVATE WINDOW (lcApDbWin4)
    _CUROBJ = OBJNUM(pbClose)
    KEYBOARD "{ENTER}"
ENDCASE

*!**************************************************************************
*!
*!      Function: lfActBrows
*!
*!**************************************************************************
*
FUNCTION lfActBrows
IF WPARENT(WOUTPUT()) = gcBaseWind
  RELEASE PAD _BROWSE OF _MSYSMENU
  ACTIVATE WINDOW (lcBrTtl1) TOP
ENDIF
*!**************************************************************************
*!
*!      Function: lfDefinePad
*!
*!**************************************************************************
* Defines an additional 'Browse' pad when one does not exist (i.e. when
* a browse is not active)
*
FUNCTION lfDefinePad
DEFINE PAD _BROWSE OF _MSYSMENU PROMPT lcTBrowse  KEY ALT+B
ON SELECTION PAD _BROWSE OF _MSYSMENU DO lfActBrows


*!**************************************************************************
*!
*!      Function: lfActivate
*!
*!**************************************************************************
* gcBaseWind's deactivate function
* releases the browse pad if a browse window is on top, and traps keys
* and vice versa.
*
FUNCTION lfActivate
SELECT APVENDOR
IF _DOS
  =lfDefinePad()
ENDIF
IF glFromBrow     
  =lfBrowUnTrap()
  =gfStopBrow()
ENDIF


*!**************************************************************************
*!
*!      Function: lfShow
*!
*!**************************************************************************
* gcBaseWind's deactivate function
* releases the browse pad if a browse window is on top, and traps keys
* and vice versa.
*
FUNCTION lfShow
IF EOF('APINVHDR') .AND. WVISIBLE( 'CWRAPAPLDM')
  =gfChClose('CWRAPAPLDM')
ENDIF 
IF EOF(lc_InvHdr) .AND. WVISIBLE('CWRAPAPLIN')
  =gfChClose('CWRAPAPLIN')
ENDIF  


*!**************************************************************************
*!
*!      Function: lfDeact
*!
*!**************************************************************************
* gcBaseWind's deactivate function
* releases the browse pad if a browse window is on top, and traps keys
* and vice versa.
*
FUNCTION lfDeact
IF WONTOP(lcBrTtl1) .OR. WONTOP(lcBrTtl2)
  SHOW GET ldAppDate
  glFromBrow  = .T.
  RELEASE PAD _BROWSE OF _MSYSMENU
  =lfBrowTrap()
ENDIF


*!**************************************************************************
*!
*!      Function: lfGetPayMeth
*!
*!**************************************************************************
* gets payment method description
*
FUNCTION lfGetPayMeth
PARAMETERS lcPMeth
PRIVATE lnCount
FOR lnCount = 1 TO ALEN(laPayMeth,1)
  IF laPayMeth[lnCount,2] = lcPMeth
    RETURN laPayMeth[lnCount,1]
  ENDIF
ENDFOR
RETURN ' '

*!**************************************************************************
*!
*!        Function : lfModalShow
*!
*!**************************************************************************
* Show function called from the modal screen of 'Selected invoices',
* called upon selection from radio button scope in the main window
FUNCTION lfModalShow
IF _DOS
  RELEASE PAD _BROWSE OF _MSYSMENU
  DEFINE PAD _BROWSE OF _MSYSMENU PROMPT lcTBrowse
ENDIF

*!**************************************************************************
*!
*!        Function : lfRecLock
*!
*!**************************************************************************
*  Attempt to object lock current debit memo and current invoice
*  after checking for their validity
FUNCTION lfRecLock
PRIVATE lcCurAlias, lcOldFilter, llRecLock, llInvLock

*** lcCurAlias  : current alias
*** lcOldFilter : current invoice filter value
*** llRecLock   : .T. if object lock is possible.
*** llInvLock   : .T. if invoice is within scope
 
*** Message :     "   ð No. ð has just been ð by another user.                         "
***               "   Cannot apply Debit Memo No. ð to Invoice No. ð.  "
***                                   <  OK  >

lcCurAlias = ALIAS()
llInvLock  = .T.
SELECT APINVHDR
IF !EOF()
  GO RECNO()
ENDIF  

*B608348,1 NNA 11/12/2007 (Begin) Get the global function to be local function and make the needed changes
*IF gfObj_lock(.T.)  
IF lfObj_lock(.T.)  
*B608348,1 NNA (End)

  SELECT (lc_Invhdr)
  IF !EOF()
    GO RECNO()
  ENDIF  

  *B608348,1 NNA 11/12/2007 (Begin) Get the global function to be local function and make the needed changes
  *IF gfObj_lock(.T.)  
  IF lfObj_lock(.T.)  
  *B608348,1 NNA (End)

    llRecLock = .T.
    DO CASE
      *** If the current debit memo has just been paid
      CASE APINVHDR.nInvAmnt - APINVHDR.nInvPaid - ;
           APINVHDR.nInvDisTk - APINVHDR.nInvAdj = 0
        llRecLock = gfModalGen("TRM04071B00000","DIALOG",;
                       lcUDebtMem + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                       lcTPaid + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                       ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** If the current debit memo has just been voided 
      CASE APINVHDR.cInvStat = 'V'
        llRecLock = gfModalGen("TRM04071B00000","DIALOG",;
                    lcUDebtMem + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                    lcTVoided + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                     ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** If the current invoice has just been paid
      CASE &lc_InvHdr..nInvAmnt - &lc_InvHdr..nInvPaid - ;
           &lc_InvHdr..nInvDisTk - &lc_InvHdr..nInvAdj = 0
        llRecLock = gfModalGen("TRM04071B00000","DIALOG",;
                     lcUInv + '|' + ALLTRIM(&lc_InvHdr..cInvNo) + '|' +;
                     lcTPaid + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                     ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** If the current invoice has just been voided 
      CASE &lc_InvHdr..cInvStat = 'V'
        llRecLock = gfModalGen("TRM04071B00000","DIALOG",;
                       lcUInv + '|' + ALLTRIM(&lc_InvHdr..cInvNo) + '|' +;
                       lcTVoided + '|' + ALLTRIM(APINVHDR.cInvNo) + '|' +;
                       ALLTRIM(&lc_InvHdr..cInvNo)) = 0

      *** If the current invoice falls out of the current invoice scope due
      *** to other user's recent editing, unlock the record and present
      *** the following message 
      *** Message :   "   Invoice No. ð has just been modified   "
      ***             "   by another user.  This invoice is now  "    
      ***             "   out of scope.                          "
      ***                           <  OK  >
      CASE !SEEK(laData[1]+lcInvoice, lc_InvHdr)
        llInvLock   = gfModalGen("TRM04150B00000", "DIALOG",;
                                  ALLTRIM(lcInvoice)) = 0
        llRecLock   = .F.             
        SELECT (lc_InvHdr)
        lcOldFilter = lcInvFlt
        lcInvFlt    = ".T."
        =SEEK(laData[1]+lcInvoice) .AND. gfObj_Lock(.F.)
        lcInvFlt    = lcOldFilter
        *B801196,4 AMM start
        *GO TOP
        lcOldTag=TAG()
        SET ORDER TO
        LOCATE
        SET ORDER TO TAG &lcOldTag.
        *B801196,4 AMM end        
    ENDCASE    
    IF !llRecLock .AND. llInvLock
      =gfObj_lock(.F.)
    ENDIF
    IF !llRecLock      
      SELECT APINVHDR
      =gfObj_lock(.F.)
      *B601061,1 Refreshing objects if not applying
      =lfRefrObj()
      *B601061,1 end.
    ENDIF
  ELSE
    SELECT APINVHDR
    =gfObj_lock(.F.)  
    *B608942,3 TMI [START] Unlock the lc_Invhdr alias file
    *B609026,1 TMI [START] no need for this line
    *SELECT (lc_Invhdr)
    *=gfObj_lock(.F.)  
    *B609026,1 TMI [END  ] no need for this line
    *B608942,3 TMI [END  ] Unlock the lc_Invhdr alias file    
    llRecLock = .F.
  ENDIF  
ELSE
  *B608942,3 TMI [START] Unlock the APINVHDR file
  *B609026,1 TMI [START] no need for this line
  *SELECT APINVHDR
  *=gfObj_lock(.F.)  
  *B609026,1 TMI [END  ] no need for this line
  *B608942,3 TMI [END  ] Unlock the APINVHDR file
  llRecLock = .F.
ENDIF
*B601061,1 Removed refreshing objects here for speed
*B601061,1 end.
SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)  
RETURN llRecLock .AND. llInvLock
*!***************************************************************************
*!* Name        : lfObj_Lock
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 11/12/2007
*!* Module      : Account Payable (AP)
*!* Purpose     : To issue lock or unlock to any record in any file
*!***************************************************************************
*!* Called from : ApaplDB.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfObj_Lock()
*!***************************************************************************
*! B#608348,1
FUNCTION lfObj_Lock
PARAMETERS lLok_Set
PRIVATE lnRecNo,lRet_Flag
PRIVATE lnOldrpSt
lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

DO WHILE .T.
  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
   llLocked = RLOCK() 
   IF DELETED()
     UNLOCK
     =gfModalGen('INM00095B00000','ALERT')
     laScrMode     = .F.
     laScrMode [1] = .T.

     SHOW GETS
     RETURN .F.
   ENDIF
  ENDIF  

  *B608942,1 TMI 07/20/2009 06:14:25 PM [Start] 
  IF glRunFrmA4
    lnSlct = SELECT(0)
    IF !USED('A4USTATC')
      =gfOpenFile(gcA4SYSFLDR+'SYUSTATC','CUSER_ID','SH','A4USTATC')
    ENDIF
    SELECT (lnSlct)
  ENDIF    
  *B608942,1 TMI 07/20/2009 06:14:25 PM [End  ] 

  *** Chek if the record is in use by another user
  IF lLok_Set 
    *** Chek if the field cLok_User in the structur
    IF !lLok_Stat .AND. llLocked
      *** Record is not locked you may lock it
      lLok_It   = .T.
    ELSE
      IF !EMPTY(cLok_User)
        IF ALLTRIM(cLok_User) = ALLTRIM(gcUser_ID)
          *!600396,1 Messaging the user that he cannot edit the same record
          *!600396,1 from more than one session and permit him from editing
          *!600396,1 the same record
          IF gfModalGen("INM04205B00015","ALERT") = 1
            LOOP
          ENDIF  
          lLok_It    = .F.
          lRet_Flag  = .F.
        ELSE  
          lnOldrpSt = SET('REPROCESS')
          SET REPROCESS TO 1
        *B608942,1 TMI 07/20/2009 07:23:22 PM [Start] check if the user recorded in cLok_user field lies in SYUSTATC file of either A27 or A4xp
        *IF SEEK ('INI'+'OLDVARS'+cLok_User,'syuStatc') 
          IF SEEK ('INI'+'OLDVARS'+cLok_User,'syuStatc') .OR. IIF(glRunFrmA4,SEEK('INI'+'OLDVARS'+cLok_User,'A4USTATC'),.F.)
          *B608942,1 TMI 07/20/2009 07:23:29 PM [End  ] 
            *B608942,3 TMI [START] comment the lock of the SYUSTATIC file, it is not used in other AP files that uses this function
            *IF RLOCK('syuStatc')
            *  UNLOCK IN  syuStatc 
            *  GO gnMyStRec IN syuStatc 
            *  =RLOCK('syuStatc')
            *  lLok_It    = .T.
            *ELSE
            *B608942,3 TMI [END  ] comment the lock of the SYUSTATIC file, it is not used in other AP files that uses this function
              UNLOCK
              *** Display the message "Record is in use by user AAAA"
              lnSavRec   = IIF(RECNO('SYUUSER')>RECCOUNT('SYUUSER'),0,;
                           RECNO('SYUUSER'))
              lcLok_User = ALLTRIM(PROPER(LOOKUP(syuUser.cUsr_name,cLok_User,;
                           syuUser.cUser_id,'cUser_id')))
              IF lnSavRec > 0
                GO lnSavRec IN SYUUSER
              ENDIF  
          
              *** Record is in use by user ????    
              IF  gfModalGen("INM00028B00015","ALERT",lcLok_User) = 1
                LOOP
              ENDIF  
              lLok_It    = .F.
              lRet_Flag  = .F.
            *B608942,3 TMI [START] comment the lock of the SYUSTATIC file, it is not used in other AP files that uses this function
            *ENDIF
            *B608942,3 TMI [END  ] comment the lock of the SYUSTATIC file, it is not used in other AP files that uses this function
          ELSE
            lLok_It    = .T. 
          ENDIF          
          SET REPROCESS TO  lnOldrpSt
        ENDIF
      ELSE
        *** Display the message "Record is in use by another"
        IF gfModalGen("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF  
        lLok_It    = .F.
        lRet_Flag  = .F.
      ENDIF   
    ENDIF
  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
       TYPE ('dLok_Date') <> "U" .AND. ;
       TYPE ('cLok_Time') <> "U" 

      *** Unlock the record
      REPLACE lLok_Stat WITH .F. , ;   
              cLok_User WITH ""  , ;
              dLok_Date WITH {}  , ;
              cLok_Time WITH ""
      lRet_Flag  = .T.
    ENDIF  
  ENDIF

  EXIT
ENDDO
*** Chek if you have to lock the record or not
IF lLok_It  
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
     TYPE ('dLok_Date') <> "U" .AND. ;
     TYPE ('cLok_Time') <> "U" 
    *** Lock the record for this user with date and time
    REPLACE lLok_Stat WITH .T.       , ;   
             cLok_User WITH gcUser_ID , ;
             dLok_Date WITH DATE()    , ;
             cLok_Time WITH gfGetTime()

    lRet_Flag  = .T.    
  ENDIF
ENDIF
UNLOCK
RETURN lRet_Flag

*--> End Of Function lfObj_Lock.



