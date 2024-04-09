*:************************************************************************
*:
*: Procedure file: APGNRCR.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Generate recurring invoices
*:         Author: Renee Ezzat
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfBrowInv
*:               : lfShowInv
*:               : lfwOldVals
*:               : lfvGenDate
*:               : lfwRcrInv
*:               : lfvRcrInv
*:               : lfvCancel
*:               : lfvGenerate
*:               : lfvUpdate
*:               : lfVldNextDate
*:               : lfGetNextDate
*:               : lfVldGlAccts
*:               : lfvSelect
*:               : lfvSelAll
*:               : lfvInvert
*:               : lfwBrowRcr
*:               : lfvInvAmnt
*:               : lfvContinue
*:               : lfGetArrElem
*:               : lfMousClk
*:               : lpMClkSel
*:               : lfClrMous
*:               : lpCtrlEntr
*:               : lpEsc
*:               : lpTab
*:               : lpShiftTab
*:               : lfBrowTrap
*:               : lfBrowUnTrap
*:               : lpBrowKeyTrap
*:               : lfwBroRcr
*:               : lfwBroWhen
*:               : lfActBrows
*:               : lfDefinePad
*:               : lfDeact
*:               : lfClearGen
*:               : lfSelAct
*:
*:  Documented      /  /
*:************************************************************************
*B600492,1 Reham 06/22/95 Change the browse fields width under windows.
*B600602,1 RENEE 08/03/95 Update more fields in the vendor's file to
*B600602,1                reflect the generated invoice amount on the
*B600602,1                vendor's purchase fields
*B600604,1 RENEE 08/03/95 Change the period calculation expression so as 
*B600604,1                not to mix between period '01' and period '10', 
*B600604,1                considering both of them as period '1' when  
*B600604,1                updating APVENHST purchase fields corresponding 
*B600604,1                to the invoice period. (eg nVnHPur1, nVnHPur10..)
*B600650,1 ORABY 08/17/95 In case of generating every period (s) in some cases the system skip a period
*E300296,4 RENEE 10/22/95 Add currency exchange checks (multi currency).
*E300316,1 HISH  11/28/95 Used global function gfGetExSin to get currency 
*E300316,1                equation signs.
*E300316,1 HISH  01/08/96 Passed pointer parameter to get Unit sgin.
*C100728,1 M.H   12/08/96 Add the posting date field to the APINVHDR record.
*B601526,1  HS  01/19/97  Add the new field [nInvFAAp] in the APINVHDR file 
*B601526,1                [the approved amount in the approve currency] 
*E300643,1  HS 04/15/97 1)Make some changes for we have change the function
*E300643,1              [gfCodDes].
*E300643,1              2)Make some changes for we have change the function
*E300643,1              [gfRltFld].
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E300692,1 ESSMAT 06/29/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E301077,79 AMM 03/04/99 Enhance opening and closing files.
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
*:************************************************************************
*
EXTERNAL ARRAY laData,laKeyField

** lcGnRcrCh1    Variable to hold
** lcGnRcrCh2    Variable to hold
** lcGnRcrCh3    Variable to hold
** lcBrInvTtl    browse window title
** lcBrRcrTtl    browse window title7
** lc_Invoices   temporary invoices file
** lc_SelInv     Variable to hold
** lcSession     Variable to hold
** lcGenPrd      Variable to hold
** lcGenYear     Variable to hold
** lcBrowWind    Variable to hold
** lcOldSlPr     Variable to hold
** lcEscOnKey    Variable to hold
*E300316,1 HISH  11/28/95. Added currency equation sign variables. (Begin)
** lcExSin1             Variable to hold the first sign in the equation.
** lcExSin2             Variable to hold the second sign in the equation.
*E300316,1 (End)
**
** lnOldVal      Variable to hold
** lnTime        Variable to hold
** lnBrRecNo     browse record number 
** lnOldrbRcr    Variable to hold
** lnOldBrRec    Variable to hold
**
** llCancel      Variable to hold
** llSkip        Variable to hold
** llUpdated     Variable to hold
** llFromModal   Variable to hold
** llMayGenr     .T. if generate button is enabled, and update button is disabled, .f. otherwise
** llNoContrl    Variable to hold
**
** ldOldVal      Variable to hold
** ldGenDate     generation date
**
** rbRcrInv      Variable to hold

STORE 0          TO lnOldVal   , lnTime     , lnBrRecNo  , lnOldBrRec , lnCurrUnt
STORE 1          TO rbRcrInv   , lnOldrbRcr
STORE {}         TO ldOldVal   , ldGenDate
STORE .F.        TO llCancel   , llSkip     , llUpdated
STORE .T.        TO llFromModal, llMayGenr  , llNoContrl
STORE 'DISABLE'  TO lcUpdStat  , laCtrStat
STORE 'ENABLE'   TO lcGenStat   
STORE '\<Select' TO lcSlPr     
STORE ''         TO lcGnRcrCh1 , lcGnRcrCh2 , lcGnRcrCh3 , lcBrInvTtl ,;
                    lcBrRcrTtl , lc_Invoices, lc_SelInv  , lcSession  ,;
                    lcGenPrd   , lcGenYear  , lcBrowWind , lcOldSlPr  ,;
                    lcEscOnKey , lcExSin1 , lcExSin2
                    
IF !gfSetup()
  RETURN
ENDIF

ldCurDate = DATE()

*** Base file for the current screen
SELECT APINVAHD
SET ORDER TO TAG HTYPCOD                 && should already be set

*** Check if there are any recurring codes in the automatic header file
*** If there are not, present the following message and exit the program
IF !SEEK('R')
  *** Message :  "       There are no ð.    "
  ***                      <   OK   >
  =gfModalGen("TRM04101B00000","DIALOG", "recurring payables to generate")
  glQuitting = .T.
  RETURN
ENDIF
  
lcInvAHdEx     = SYS(14,VAL(SYS(21))) 

DECLARE laBrHdrs[7]   , laBrSelHdr[5], laRltFlDat[3,3],;
        laRemitTo[1,2], laType[2,2]  , laTermAry[3,2] 
        
*** Prepare Remit to array from SYDFIELD 
=gfGetVld('cInvRemit',@laRemitTo)
=gfGetVld('cAutMBase',@laType)
 
*B600492,1 Change the width of the include field from 3 to 1.
lcCheck        = 'û'
lcUncheck      = SPACE(1) 

*** Set the tag of the payable invoices file to VENDINV
*** tag expression : cVendCode + cInvNo
SET ORDER TO TAG VENDINV IN APINVHDR

SELECT APINVADT
SET ORDER TO TAG DTYPCOD
lcInvADtEx      = SYS(14,VAL(SYS(21)))   && current index expression

IF !WEXIST(gcBaseWind)
  laTermAry[1,1] = 'NTERDUED'
  laTermAry[1,2] = 'lnTerDueD'  
  laTermAry[2,1] = 'NTERDISCD'
  laTermAry[2,2] = 'lnTerDiscD'  
  laTermAry[3,1] = 'NTERDISCR'
  laTermAry[3,2] = 'lnTerDiscR'  
  
  lcGnRcrCh1     = gfTempName()
  lcGnRcrCh2     = gfTempName()  
  lcGnRcrCh3     = gfTempName()  
  lc_SelInv      = gfTempName()  
  lc_Invoices    = gfTempName()  
    
  *** Create a temporary file (lc_SelInv) with the same structure
  *** as the payable invoices header, + 2 more fields for selection marks.
  SET ORDER TO 0 IN APINVAHD
  SELECT *, SPACE(1) AS cInclude, SPACE(3) AS cOldIncl;
      FROM &gcDataDir.APINVAHD;
      WHERE &lcInvAHdEx = 'R';
      ORDER BY cAutMCode;
      INTO DBF &gcWorkDir.&lc_SelInv
  INDEX ON cAutMType + cAutMCode TAG HTYPCOD    
  SET ORDER TO TAG HTYPCOD IN APINVAHD

  *** Get the file structure of the Payable invoices file (APINVHDR)
  DECLARE laFileStru[1]
  SELECT APINVHDR
  =AFIELDS(laFileStru)

  *** Add four extra fields to the structure as follows
  lnFileStru    = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+3,4]

  laFileStru[lnFileStru+1,1] = 'CINCLUDE'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  
  laFileStru[lnFileStru+2,1] = 'CAUTMBASE'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0
  
  laFileStru[lnFileStru+3,1] = 'DNEXTDATE'
  laFileStru[lnFileStru+3,2] = 'D'
  laFileStru[lnFileStru+3,3] = 8
  laFileStru[lnFileStru+3,4] = 0

  *** Create a temporary file (lc_Invoices) with the above structure
  *** and add an index tag on CAUTMCODE  
  CREATE TABLE (gcWorkDir+lc_Invoices) FROM ARRAY laFileStru
  SELECT (lc_Invoices)
  INDEX ON cAutMCode TAG INCLCODE
  
  *** Prepare a ssion if one does not already exist.

  *E300663,1 Change this line for the changes we have 
  *          made to (gfSequence) [Begin]
  *lcSession   = PADL(ALLTRIM(STR(gfSequence('APSESS',1))),;
  *                                FSIZE('cApSessNo','APDIST'),'0')
  lcSession   = gfSequence('CAPSESSNO')
  *E300663,1 Change this line [End]
  
  *** Default the generation date with the system date
  ldGenDate = gdSysDate  
  *E300789,4  AMM Open FSPRD of the parent company
  *MAN Added PRIVATE
  PRIVATE llOpCmp, lcDataDir
  llOpCmp = gfSysOpen(gcSysHome+'SYCCOMP','Ccomp_id','SH')
  lcDataDir = ALLTRIM(IIF(SEEK(gcPrnt_Cmp, 'SYCCOMP'),gfGetDataDir(ALLT(SYCCOMP.cCom_DDir)), gcDataDir))
  =gfOpenFile(lcDataDir+'FSPRD','COMFYRPRDI','SH')
  IF llOpCmp
    *MAN Used gfSysClose
    =gfSysClose('SYCCOMP')
    *=gfCloseFile('SYCCOMP')
  ENDIF
  *E300789,4  AMM end

ELSE
  rbRcrInv = lnOldrbRcr
ENDIF

SELECT APINVAHD
SET FILTER TO
SET FILTER TO cAutMType = "R"          && filter on recurring 

*** Prepare key settings for the browse
PUSH KEY
lcEscOnKey = ON("KEY","ESC")
ON KEY LABEL TAB         DO lpTab      
ON KEY LABEL BACKTAB     DO lpShiftTab 

*E300683,1 Call *.SPR from screens directory
* DO APGNRCR.SPR 
DO (gcScrDir + gcWinAppl + '\APGNRCR.SPR')
*E300683,1 end          


RELEASE PAD _BROWSE OF _MSYSMENU
POP KEY

SELECT APINVAHD
SET FILTER TO 

IF glQuitting
  IF USED(lc_SelInv)
    USE IN (lc_SelInv)
  ENDIF
  IF USED(lc_Invoices)
    USE IN (lc_Invoices)
  ENDIF

  HIDE WINDOW (gcBaseWind)
  RELEASE WINDOW (lcBrInvTtl)

  ERASE (gcWorkDir+lc_SelInv+'.DBF')
  ERASE (gcWorkDir+lc_SelInv+'.CDX')
  ERASE (gcWorkDir+lc_SelInv+'.FPT')
  ERASE (gcWorkDir+lc_Invoices+'.DBF')
  ERASE (gcWorkDir+lc_Invoices+'.CDX')    
  ERASE (gcWorkDir+lc_Invoices+'.FPT')

ENDIF  
glFromBrow  = .F.


*!**************************************************************************
*!
*!        Function : lfBrowInv
*!
*!**************************************************************************
* Browse function fro the main window's browse.
*
FUNCTION lfBrowInv
PRIVATE lcClrSchm

lcClrSchm  = IIF(_DOS," COLOR SCHEME 13","NOMENU")

SELECT (lc_Invoices)
lnBrRecNo  = RECNO(lc_Invoices)

*E300296,4 Add currency to the browse
*BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,; 
              cInclude  :1:H=laBrHdrs[1]:R,;
              cAutMcode :8:H=laBrHdrs[2]:R,;
              dInvDate  :H=laBrHdrs[3]:R:10,;
              dInvDuDat :H=laBrHdrs[4]:R:10,;
              nInvAmnt  :H=laBrHdrs[5]:R,;
              cInvRef   :10:H=laBrHdrs[6]:R,; 
              cType = lfGetArrElem(cAutMBase, 'laType');
                        :H=laBrHdrs[7] ;
         WINDOW (lcGnRcrCh2) ;
         IN WINDOW (gcBaseWind) ;
         LOCK 0;
         NOAPPEND;
         NOEDIT;
	     NOCLEAR;
         NODELETE;
         NOWAIT;
         SAVE;
 	     TITLE lcBrInvTtl;
         WHEN lfwBroWhen('lc_Invoices', 'lcBrInvTtl'); 
 	     VALID :F lfvBrowse();
         &lcClrSchm

*E300296,4 Add currency code field to the browse.
BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,; 
              cInclude  :1:H=laBrHdrs[1]:R,;
              cAutMcode :8:H=laBrHdrs[2]:R,;
              dInvDate  :H=laBrHdrs[3]:R:10,;
              dInvDuDat :H=laBrHdrs[4]:R:10,;
              nInvAmnt  :H=laBrHdrs[5]:R,;
              cCurrCode :H='Cur':R,;	
              cInvRef   :10:H=laBrHdrs[6]:R,; 
              cType = lfGetArrElem(cAutMBase, 'laType');
                        :H=laBrHdrs[7] ;
         WINDOW (lcGnRcrCh2) ;
         IN WINDOW (gcBaseWind) ;
         LOCK 0;
         NOAPPEND;
         NOEDIT;
	     NOCLEAR;
         NODELETE;
         NOWAIT;
         SAVE;
 	     TITLE lcBrInvTtl;
         WHEN lfwBroWhen('lc_Invoices', 'lcBrInvTtl'); 
 	     VALID :F lfvBrowse();
         &lcClrSchm

SELECT APINVAHD

 
*!**************************************************************************
*!
*!        Function : lfvBrowse
*!
*!**************************************************************************
* 
FUNCTION lfvBrowse
IF WONTOP(lcBrInvTtl)
  IF !WEXIST(lcGnRcrCh2) 
    glFromBrow = .F.
    glQuitting = .T.
    CLEAR READ
    KEYBOARD CHR(13)
    RETURN TO APGNRCR.SPR
  ENDIF
ELSE
  =lfBrowUnTrap() .AND. lfClrMous() 
  IF !WVISIBLE(gcBaseWind)
    glQuitting = .T.
  ENDIF  
  =gfStopBrow()
ENDIF    

 
*!**************************************************************************
*!
*!        Function : lfShowInv
*!
*!**************************************************************************
* shows the following objects, 
FUNCTION lfShowInv
PARAMETERS lcStatOfDisp

lcSlPr  = lcTSelect

SHOW GET pbUpdate    &lcStatOfDisp
SHOW GET pbSelect,1  &lcStatOfDisp PROMPT lcTSelect
SHOW GET pbSelAll    &lcStatOfDisp
SHOW GET pbSelNone   &lcStatOfDisp
SHOW GET pbInvert    &lcStatOfDisp
*SHOW GET pbView      &lcStatOfDisp

*!**************************************************************************
*!
*!        Function : lfwOldVals
*!
*!**************************************************************************
FUNCTION lfwOldVals
PARAMETERS lcOldObjNm

&lcOldObjNm = EVALUATE(SYS(18))

*!**************************************************************************
*!
*!        Function : lfvGenDate
*!
*!**************************************************************************
* Valid function for get field ldGenDate (generation date)
*
FUNCTION lfvGenDate
IF ldGenDate <> ldOldVal .AND. WVISIBLE(gcBaseWind)
  IF !EMPTY(ldGenDate) .AND. !lfVlDate(gcPrnt_Cmp)
    *** If the date is invalid, present the following message
    *** and blank the field
    *** Message :   "   The ð date should be within the posting window.  "
    ***                                 <    OK    >    
    =gfModalGen('TRM04113B00000', 'DIALOG', lcTGen)
    ldGenDate = IIF(RECCOUNT(lc_Invoices) > 0, ldOldVal, {})
    _CUROBJ   = OBJNUM(ldGenDate)
  ELSE
    *** If there are generated invoices, and the 
    *** date is changed, present a warning message that
    *** generated invoices are going to be disacrded.
    *** Message :   "  The generated invoices will be discarded!  "
    ***                     <   Ok   >    < Cancel >
    IF RECCOUNT(lc_Invoices) > 0 
      IF gfModalGen("TRM04004B04004", "DIALOG") = 1
        =lfClearGen()
      ELSE
        ldGenDate = ldOldVal
        _CUROBJ   = OBJNUM(ldGenDate)
      ENDIF
    ENDIF
  ENDIF
  ldOldVal = ldGenDate
ENDIF


*!**************************************************************************
*!
*!      Function: lfwRcrInv
*!
*!**************************************************************************
*  When function for radio button rbRcrInv
*
FUNCTION lfwRcrInv

SHOW GET ldGenDate

*!**************************************************************************
*!
*!      Function: lfvRcrInv
*!
*!**************************************************************************
*  Valid function for radio button rbRcrInv
*
FUNCTION lfvRcrInv
PRIVATE lcCurAlias

STORE .F. TO llCancel, llUpdated
lcCurAlias = ALIAS()

*** If there are generated invoices, and the 
*** selected invoice are changed, present a warning message that
*** generated invoices are going to be disacrded.
*** Message :   "  The generated invoices will be discarded!  "
***                     <   Ok   >    < Cancel >
*** else, validate as usual
DO CASE

  CASE rbRcrInv = 1 .AND. rbRcrInv <> lnOldrbRcr
    llUpdated   = .T.

  CASE rbRcrInv = 2   
    SELECT (lc_SelInv)
    REPLACE ALL cOldIncl WITH cInclude
    GO TOP
    PUSH KEY
    RELEASE PAD _BROWSE OF _MSYSMENU
    ON KEY LABEL ALT+B
    =lfBrowTrap() .AND. lfMousClk('lc_SelInv', 'lcBrSelTtl')
    llFromModal = .F.
    lnOldBrRec  = lnBrRecNo
    lcOldSlPr   = lcSlPr
    *E300683,1 Call *.SPR from screens directory
    * DO APSELINV.SPR 
    DO (gcScrDir + gcWinAppl + '\APSELINV.SPR')
    *E300683,1 end          
    lcSlPr      = lcOldSlPr
    lnBrRecNo   = lnOldBrRec    
    POP KEY
    
    IF llCancel
      SELECT (lc_SelInv)
      REPLACE ALL cInclude WITH cOldIncl 
    ENDIF
ENDCASE

IF llUpdated .AND. !llCancel .AND. RECCOUNT(lc_Invoices) > 0 
  IF gfModalGen("TRM04004B04004", "DIALOG") = 1
    =lfClearGen(rbRcrInv = 2)
  ELSE
    IF rbRcrInv = 2
      SELECT (lc_SelInv)
      REPLACE ALL cInclude WITH cOldIncl
    ENDIF  
    rbRcrInv = lnOldrbRcr
    SHOW GET rbRcrInv
  ENDIF  
ENDIF
SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
lnOldrbRcr  = rbRcrInv

*!**************************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************************
*  Valid function for push button 'Cancel' 
*
FUNCTION lfvCancel
PARAMETERS llCancel

llCancel  = .T.

*!**************************************************************************
*!
*!      Function: lfvGenerate
*!
*!**************************************************************************
*  Valid function for push button update
*
FUNCTION lfvGenerate
PRIVATE lcCurAlias, lcScanCmd1, lcScanCmd2, lcOutComp, lcOutAddr1,;
        lcOutAddr2,lcOutAddr3, lnTotRec, lnCurRec, lnCount,;
        lnTerDiscR , lnTerDiscD, lnTerDueD ,lnInvAmnt, ldNextDate
        
*** If the generation date is empty, present the following message
*** and do not generate, else continue with the generaton process.
IF EMPTY(ldGenDate)
  *** Message : "   You have to enter the ð.  "
  ***                 <  OK  >
  =gfModalGen("TRM04066B00000", "DIALOG", lcTGenDate)
  _CUROBJ = OBJNUM(ldGenDate)
ELSE
  lcCurAlias = ALIAS()

  SELECT (lc_SelInv)
  SET RELATION TO cAutMType + cAutMCode INTO APINVADT ADDITIVE

  IF rbRcrInv = 1
    lcScanCmd1 = ""
    lnTotRec   = RECCOUNT(lc_SelInv)
  ELSE
    lcScanCmd1 = "!EMPTY(&lc_SelInv..cInclude) .AND. "
    COUNT FOR !EMPTY(&lc_SelInv..cInclude) TO lnTotRec
  ENDIF
                   
  IF lnTotRec > 0
    lnTotRec     = lnTotRec + 1
    lcScanCmd2   = IIF(llApGlLink, ".AND. lfVldGlAccts(lc_SelInv, cAutMCode)","")

    *** Prepare thermometer variables
    lnCurRec     = 0
    llCancel     = .F.        && *E300296,4  Reset Cancel flag

    *** Create (lc_Invoices) file as follows :
    SCAN FOR &lcScanCmd1 !EMPTY(dAutNxGen) .AND. dAutNxGen <= ldGenDate ;
              .AND. dAutNxGen <= dAutEnGen;
              .AND. lfVldNextDate(dAutNxGen) &lcScanCmd2
  
      *** Increment thermometer step
      lnCurRec = lnCurRec + 1
      =gfThermo(lnTotRec,lnCurRec,lcTGenRcr, lcTRcrCode + ALLTRIM(cAutMCode))
  
      *** Get the next date variable
      ldNextDate = dAutNxGen
      STORE 0 TO lnTerDiscR , lnTerDiscD, lnTerDueD 

      *E300643,1 Change this line for the changes we have made 
      *          to (gfRltFld) [Begin]
      *=gfRltFld(cTermCode,@laTermAry)    
      =gfRltFld(cTermCode , @laTermAry , 'CTERMCODE')    
      *E300643,1 Change this line [End]
      
      *** If the invoice has a percent base, branch to the following 
      *** screen to get the actual invoice amount
      IF cAutMBase = 'P'
        lnInvAmnt  = 0
        STORE .F. TO llSkip, llCancel
        PUSH KEY
        ON KEY
        *E300683,1 Call *.SPR from screens directory
        * DO APINVAM.SPR 
        DO (gcScrDir + gcWinAppl + '\APINVAM.SPR')
        *E300683,1 end          
        POP KEY
        lnInvDisOf = lnInvAmnt  * lnTerDiscR / 100
        lnInvAmtAp = lnInvAmnt  * nInvAmtAp  / 100
        lnInvDisAp = lnInvAmnt  * nInvDisAp  / 100
        lnInvAdjAp = lnInvAmnt  * nInvAdjAp  / 100
        lnInvA1099 = lnInvAmnt  * nInvA1099  / 100
      ELSE
        lnInvAmnt  = nInvAmnt
        lnInvDisOf = nInvDisOf
        lnInvAmtAp = nInvAmtAp
        lnInvDisAp = nInvDisAp
        lnInvAdjAp = nInvAdjAp
        lnInvA1099 = nInvA1099 
      ENDIF  
  
      IF !(llSkip .OR. llCancel)
        *** Get the remit to fields
        STORE SPACE(40) TO lcOutComp, lcOutAddr1, lcOutAddr2, lcOutAddr3
        IF cInvRemit <> 'O' 
          =lfRemit(cInvRemit, .F., cVendCode, cFacCode, " ",;
          'lcOutComp', 'lcOutAddr1', 'lcOutAddr2', 'lcOutAddr3',0,0,'laRemitTo')
        ELSE
          lcOutComp  = cOutComp
          lcOutAddr1 = cOutAddr1
          lcOutAddr2 = cOutAddr2
          lcOutAddr3 = cOutAddr3
        ENDIF  
  
        *** Create invoices for every recurring invoice as long as the 
        *** next generation date is less or equal to the generation date
        *E300296,4 Add Cancel flag to the loop condition so as not
        *E300296,4 to generate if the user Cancels generation
        *DO WHILE ldNextDate <= dAutEnGen .AND. ldNextDate <= ldGenDate 
        DO WHILE ldNextDate <= dAutEnGen .AND. ldNextDate <= ldGenDate ;
                 .AND. !llCancel
        *E300296,4 end.
               
          *E300296,4 Add a check for currency rates if the currency
          *E300296,4 is not the base currency.
          *E300296,4 If the current date does not return a valid 
          *E300296,4 exchange rate, do not generate.
          IF cCurrCode = gcBaseCurr
            lnCurrUnt  = 1
            lnExchRate = 1
          ELSE
            lnExchRate = gfChkRate('lnCurrUnt', cCurrCode, ldNextDate)
          ENDIF
          SELECT (lc_SelInv)             
          IF lnExchRate = 0
            *E300296,4 If an exchange rate is not found, 
            *E300296,4 present teh following message, ask the user
            *E300296,4 whether to continue or to cancel generation.
            *E300296,4 Message :
            *E300296,4  " Recurring invoice ð cannot be generated  "
            *E300296,4  " on ð. A valid ð to ð exchange rate could "
            *E300296,4  " not be found for that date. You need to  "
            *E300296,4  " setup a valid exchange rate before       "
            *E300296,4  " generation on ð.                         "
            *E300296,4         < Continue >  < Cancel >
            llCancel = gfModalGen('QRM04154B00023', 'DIALOG',;
                       ALLTRIM(cAutMCode) + '|' + DTOC(ldNextDate) + '|' + ;
                       ALLTRIM(cCurrCode) + '|' + ALLTRIM(gcBaseCurr) + '|' + ;
                       DTOC(ldNextDate)) = 2           
          ELSE
            *E300296,4 end.
*C100728,1 M.H Begin.
*            INSERT INTO (lc_Invoices);
                 (cInclude, cAutMBase, cAutMType,;
                  cAutMCode, cVendCode, cDivision,;
                  dInvDate,  cInvRef, cInvRemit,; 
                  cOutComp, cOutAddr1, cOutAddr2, cOutAddr3,;  
                  nInvAmnt, nInvDisOf, nInvAmtAp, nInvDisAp, nInvAdjAp, nInvA1099,;
                  cVenPrior, cVenPMeth, cTermCode,;
                  nTerDueD, nTerDiscD, nTerDiscR, dInvDuDat,;
                  cBnkCode, cChkAcct, cChkGlAcc,;
                  cApAcct, cFisFYear, cFspPrdId, cFacCode);
             VALUES(lcUnCheck, &lc_SelInv..cAutMBAse, &lc_SelInv..cAutMType,;
                  &lc_SelInv..cAutMCode, &lc_SelInv..cVendCode, &lc_SelInv..cDivision,;
                  ldNextDate, &lc_SelInv..cInvRef, &lc_SelInv..cInvRemit,;
                  lcOutComp, lcOutAddr1, lcOutAddr2, lcOutAddr3,;  
                  lnInvAmnt, lnInvDisOf, lnInvAmtAp, lnInvDisAp, lnInvAdjAp, lnInvA1099,;           
                  &lc_SelInv..cVenPrior, &lc_SelInv..cVenPMeth, &lc_SelInv..cTermCode,;
                  lnTerDueD, lnTerDiscD, lnTerDiscR, ldNextDate + lnTerDueD, ;
                  &lc_SelInv..cBnkCode, &lc_SelInv..cChkAcct, &lc_SelInv..cChkGlAcc,;
                  &lc_SelInv..cApAcct, lcGenYear, lcGenPrd, &lc_SelInv..cFacCode)

            IF TYPE('APINVHDR.DPOSTDATE') = 'D'

              *B601526,1 Add the new field (nInvFAAp) [Begin]   
              *INSERT INTO (lc_Invoices);
              *      (cInclude, cAutMBase, cAutMType,;
              *       cAutMCode, cVendCode, cDivision,;
              *       dInvDate,  cInvRef, cInvRemit,; 
              *       cOutComp, cOutAddr1, cOutAddr2, cOutAddr3,;  
              *       nInvAmnt, nInvDisOf, nInvAmtAp, nInvDisAp, nInvAdjAp, nInvA1099,;
              *       cVenPrior, cVenPMeth, cTermCode,;
              *       nTerDueD, nTerDiscD, nTerDiscR, dInvDuDat,;
              *       cBnkCode, cChkAcct, cChkGlAcc,;
              *       cApAcct, cFisFYear, cFspPrdId, cFacCode,dPOstDate);
              *VALUES(lcUnCheck, &lc_SelInv..cAutMBAse, &lc_SelInv..cAutMType,;
              *       &lc_SelInv..cAutMCode, &lc_SelInv..cVendCode, &lc_SelInv..cDivision,;
              *       ldNextDate, &lc_SelInv..cInvRef, &lc_SelInv..cInvRemit,;
              *       lcOutComp, lcOutAddr1, lcOutAddr2, lcOutAddr3,;  
              *       lnInvAmnt, lnInvDisOf, lnInvAmtAp, lnInvDisAp, lnInvAdjAp, lnInvA1099,;           
              *       &lc_SelInv..cVenPrior, &lc_SelInv..cVenPMeth, &lc_SelInv..cTermCode,;
              *       lnTerDueD, lnTerDiscD, lnTerDiscR, ldNextDate + lnTerDueD, ;
              *       &lc_SelInv..cBnkCode, &lc_SelInv..cChkAcct, &lc_SelInv..cChkGlAcc,;
              *       &lc_SelInv..cApAcct, lcGenYear, lcGenPrd, &lc_SelInv..cFacCode,ldNextDate)

              INSERT INTO (lc_Invoices);
                    (cInclude, cAutMBase, cAutMType,;
                     cAutMCode, cVendCode, cDivision,;
                     dInvDate,  cInvRef, cInvRemit,; 
                     cOutComp, cOutAddr1, cOutAddr2, cOutAddr3,;  
                     nInvAmnt, nInvDisOf, nInvAmtAp, nInvDisAp, nInvAdjAp, nInvA1099,;
                     cVenPrior, cVenPMeth, cTermCode,;
                     nTerDueD, nTerDiscD, nTerDiscR, dInvDuDat,;
                     cBnkCode, cChkAcct, cChkGlAcc,;
                     cApAcct, cFisFYear, cFspPrdId, cFacCode,dPOstDate,;
                     nInvFAAp);
              VALUES(lcUnCheck, &lc_SelInv..cAutMBAse, &lc_SelInv..cAutMType,;
                     &lc_SelInv..cAutMCode, &lc_SelInv..cVendCode, &lc_SelInv..cDivision,;
                     ldNextDate, &lc_SelInv..cInvRef, &lc_SelInv..cInvRemit,;
                     lcOutComp, lcOutAddr1, lcOutAddr2, lcOutAddr3,;  
                     lnInvAmnt, lnInvDisOf, lnInvAmtAp, lnInvDisAp, lnInvAdjAp, lnInvA1099,;           
                     &lc_SelInv..cVenPrior, &lc_SelInv..cVenPMeth, &lc_SelInv..cTermCode,;
                     lnTerDueD, lnTerDiscD, lnTerDiscR, ldNextDate + lnTerDueD, ;
                     &lc_SelInv..cBnkCode, &lc_SelInv..cChkAcct, &lc_SelInv..cChkGlAcc,;
                     &lc_SelInv..cApAcct, lcGenYear, lcGenPrd, &lc_SelInv..cFacCode,ldNextDate,;
                     lnInvAmtAp)

              *B601526,1 Add the new field (nInvFAAp) [End]   

            ELSE

              *B601526,1 Add the new field (nInvFAAp) [Begin]   
              *INSERT INTO (lc_Invoices);
              *      (cInclude, cAutMBase, cAutMType,;
              *       cAutMCode, cVendCode, cDivision,;
              *       dInvDate,  cInvRef, cInvRemit,; 
              *       cOutComp, cOutAddr1, cOutAddr2, cOutAddr3,;  
              *       nInvAmnt, nInvDisOf, nInvAmtAp, nInvDisAp, nInvAdjAp, nInvA1099,;
              *       cVenPrior, cVenPMeth, cTermCode,;
              *       nTerDueD, nTerDiscD, nTerDiscR, dInvDuDat,;
              *       cBnkCode, cChkAcct, cChkGlAcc,;
              *       cApAcct, cFisFYear, cFspPrdId, cFacCode);
              *VALUES(lcUnCheck, &lc_SelInv..cAutMBAse, &lc_SelInv..cAutMType,;
              *       &lc_SelInv..cAutMCode, &lc_SelInv..cVendCode, &lc_SelInv..cDivision,;
              *       ldNextDate, &lc_SelInv..cInvRef, &lc_SelInv..cInvRemit,;
              *       lcOutComp, lcOutAddr1, lcOutAddr2, lcOutAddr3,;  
              *       lnInvAmnt, lnInvDisOf, lnInvAmtAp, lnInvDisAp, lnInvAdjAp, lnInvA1099,;           
              *       &lc_SelInv..cVenPrior, &lc_SelInv..cVenPMeth, &lc_SelInv..cTermCode,;
              *       lnTerDueD, lnTerDiscD, lnTerDiscR, ldNextDate + lnTerDueD, ;
              *       &lc_SelInv..cBnkCode, &lc_SelInv..cChkAcct, &lc_SelInv..cChkGlAcc,;
              *       &lc_SelInv..cApAcct, lcGenYear, lcGenPrd, &lc_SelInv..cFacCode)

              INSERT INTO (lc_Invoices);
                    (cInclude, cAutMBase, cAutMType,;
                     cAutMCode, cVendCode, cDivision,;
                     dInvDate,  cInvRef, cInvRemit,; 
                     cOutComp, cOutAddr1, cOutAddr2, cOutAddr3,;  
                     nInvAmnt, nInvDisOf, nInvAmtAp, nInvDisAp, nInvAdjAp, nInvA1099,;
                     cVenPrior, cVenPMeth, cTermCode,;
                     nTerDueD, nTerDiscD, nTerDiscR, dInvDuDat,;
                     cBnkCode, cChkAcct, cChkGlAcc,;
                     cApAcct, cFisFYear, cFspPrdId, cFacCode,;
                     nInvFAAp);
              VALUES(lcUnCheck, &lc_SelInv..cAutMBAse, &lc_SelInv..cAutMType,;
                     &lc_SelInv..cAutMCode, &lc_SelInv..cVendCode, &lc_SelInv..cDivision,;
                     ldNextDate, &lc_SelInv..cInvRef, &lc_SelInv..cInvRemit,;
                     lcOutComp, lcOutAddr1, lcOutAddr2, lcOutAddr3,;  
                     lnInvAmnt, lnInvDisOf, lnInvAmtAp, lnInvDisAp, lnInvAdjAp, lnInvA1099,;           
                     &lc_SelInv..cVenPrior, &lc_SelInv..cVenPMeth, &lc_SelInv..cTermCode,;
                     lnTerDueD, lnTerDiscD, lnTerDiscR, ldNextDate + lnTerDueD, ;
                     &lc_SelInv..cBnkCode, &lc_SelInv..cChkAcct, &lc_SelInv..cChkGlAcc,;
                     &lc_SelInv..cApAcct, lcGenYear, lcGenPrd, &lc_SelInv..cFacCode,;
                     lnInvAmtAp)

              *B601526,1 Add the new field (nInvFAAp) [End]   

            ENDIF
*C100728,1 M.H End.
            *E300296,4 Add new fields for currency :
            *E300296,4 cCurrCode , nCurrUnit , nExRate, cAprCurCod 
            *E300296,4 as that of the recurring code 
            *E300296,4 Update nAprCurUnt = nAprExRat = 1    
            SELECT (lc_Invoices)
            IF lnInvAmtAp > 0
              REPLACE cCurrCode  WITH &lc_SelInv..cCurrCode,;
                      nCurrUnit  WITH lnCurrUnt,;
                      nExRate    WITH lnExchRate,;
                      cAprCurCod WITH &lc_SelInv..cCurrCode,; 
                      nAprCurUnt WITH 1,;
                      nAprExRat  WITH 1
            ELSE
              REPLACE cCurrCode  WITH &lc_SelInv..cCurrCode,;
                      nCurrUnit  WITH lnCurrUnt,;
                      nExRate    WITH lnExchRate
            ENDIF         

          *E300296,4   END IF lnExchRate = 0 
          ENDIF
          SELECT (lc_SelInv)                  
          *E300296,4 end.
    
          ldNextDate = IIF(cAutFUnit = 'D', ldNextDate + nAutFreq, ;
                       lfGetNextDate(ldNextDate, nAutFreq))

          *E300296,4 If not cancelled, update the next generation
          *E300296,4 date in (lc_Invoices)
          IF !llCancel
          *E300296,4 end
          
            SELECT (lc_Invoices)
            REPLACE &lc_Invoices..dNextDate WITH ldNextDate
            SELECT (lc_SelInv)
            
            IF EMPTY(ldNextDate) .OR. !lfVldNextDate(ldNextDate)
              EXIT
            ENDIF
            
          *E300296,4 END IF !llCancel
          ENDIF  
          *E300296,4  end.
        ENDDO
      *E300296,4 Close the IF condition, remove the ELSE
      *E300296,4 Check if Cancelled anyway.
      *ELSE
      ENDIF
      IF llCancel
      *E300296,4 end.
        SELECT (lc_Invoices)
        ZAP
        =lfBrowInv()
        EXIT
      ENDIF
    ENDSCAN
  
    *** Close the thermometer if not already closed
    IF lnTotRec > lnCurRec
      FOR lnCount = lnCurRec TO lnTotRec
        =gfThermo(lnTotRec, lnCount, lcTGenRcr, "")
      ENDFOR
    ENDIF  

    IF RECCOUNT(lc_Invoices) > 0
      lcUpdStat = 'ENABLE'
      lcGenStat = 'DISABLE'
      llMayGenr = .F.
      =lfShowInv(lcUpdStat)      
      SHOW GET pbGenerate DISABLE
      GO TOP IN (lc_Invoices)
      SHOW WINDOW (lcBrInvTtl) REFRESH 
    ELSE  
      *E300296,4 Display the message if the generation process
      *E300296,4 is not cancelled by the user
      *E300296,4 Add an IF .. ENDIF condition
      IF llCancel
      *E300296,4 end.
        *** Message :  "       There are no ð.    "
        ***                      <   OK   >
        =gfModalGen("TRM04101B00000","DIALOG", lcTNoGen)
      ENDIF         && *E300296,4 END IF llCancel
    ENDIF 

  *** No selected invoices
  ELSE
    *** Message :  "       There are no ð.    "
    ***                      <   OK   >
    =gfModalGen("TRM04101B00000","DIALOG", lcTNoSRcr)
  ENDIF
  SELECT (lc_SelInv)
  SET RELATION OFF INTO APINVADT
  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
ENDIF

*!**************************************************************************
*!
*!      Function: lfvUpdate
*!
*!**************************************************************************
* Valid function for push button update
*
FUNCTION lfvUpdate
PRIVATE lcCurAlias, lcPrd, lcSetExact, lnAutNxtNo, lnInvNoSiz, lnTotRec,;
        lnCurRec, lnCount, lnTotal, lnTotEqAmnt, lnAmount

*** Check if there are any selected records for update
*** If there are not, present the following message and return
lcCurALias = ALIAS()
lcSetExact = SET('EXACT')
lnInvNoSiz = FSIZE('cInvNo', 'APINVHDR')

SET EXACT ON

*** Set relations between the temporary file (lc_Invoices) as parent
*** with the following files as children :
*** APINVAHD on recurring type + recurring code 
*** APINVADT on recurring type + recurring code 
*** APVENDOR on the vendor code
*** APVENHST on the vendor code + the invoice fiscal year

SELECT (lc_Invoices)

LOCATE FOR !EMPTY(cInclude)

IF FOUND()
  *E301077,79 AMM Open files
  =gfOpenFile(gcDataDir+'APVENHST','VENDYEAR','SH')
  =gfOpenFile(gcDataDir+'APVENDOR','VENCODE','SH')
  =gfOpenFile(gcDataDir+'APDIST','','SH')
  SELECT (lc_Invoices)
  *E301077,79 AMM end
  
  SET RELATION TO cAutMType + cAutMCode INTO APINVAHD ADDITIVE
  SET RELATION TO cAutMType + cAutMCode INTO (lc_SelInv) ADDITIVE
  SET RELATION TO cAutMType + cAutMCode INTO APINVADT ADDITIVE
  SET RELATION TO cVendCode             INTO APVENDOR ADDITIVE
  SET RELATION TO cVendCode + cFisFYear INTO APVENHST ADDITIVE

  lnTotRec  = RECCOUNT(lc_Invoices) + 1
  lnCurRec  = 0
  
  SCAN FOR !EMPTY(cInclude)
    *E300316,1 HISH  11/28/95. Got the equation signs. (Begin)
    *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)   
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,cCurrCode)
    *lcExSin2 = IIF(lcExSin1 = '*','/','*')
    *E300316,1 (End)
   
    *** Lock the corresponding records in the files
    IF gfRLock("APINVAHD,APVENDOR,APVENHST",.T.)   
  
      *** Check if the next generation number already exists in the
      *** payable invoices file for the current vendor. If it does, 
      *** increment the next generation number until we get one
      *** that does not exist in the invoices file for the vendor.
      lnAutNxtNo = APINVAHD.nAutNxtNo
      DO WHILE SEEK(cVendCode + ALLTRIM(STR(lnAutNxtNo)), 'APINVHDR')
        lnAutNxtNo = lnAutNxtNo + 1
      ENDDO

      IF lnAutNxtNo < VAL(REPLICATE('9', FSIZE('nAutNxtNo', 'APINVAHD')))
        *** Increment thermometer step
        lnCurRec = lnCurRec + 1
        =gfThermo(lnTotRec, lnCurRec, lcTUpdRcr,;
                  lcTVendor + cVendCode + SPACE(3) +;
                  lcTInvoice + ALLTRIM(STR(lnAutNxtNo, 12)))

        REPLACE;
          &lc_Invoices..cInvNo WITH PADR(lnAutNxtNo, lnInvNoSiz)
  
        SELECT APINVAHD
        REPLACE;
          APINVAHD.nAutNxtNo WITH lnAutNxtNo + 1,;
          APINVAHD.dAutNxGen WITH &lc_Invoices..dNextDate,;
          APINVAHD.dAutLGen  WITH ldGenDate,;
          APINVAHD.cAdd_User WITH gcUser_ID,;
          APINVAHD.dAdd_Date WITH ldCurDate,;
          APINVAHD.cAdd_Time WITH gfGetTime()                            
    
        SELECT (lc_SelInv)
        REPLACE;
          nAutNxtNo WITH lnAutNxtNo + 1,;
          dAutNxGen WITH &lc_Invoices..dNextDate,;
          dAutLGen  WITH ldGenDate
        
        *** Copy the current invoice to payable invoices file
        SELECT (lc_Invoices)
        SCATTER MEMVAR MEMO
        m.cAdd_User = gcUser_ID
        m.dAdd_Date = ldCurDate
        m.cAdd_Time = gfGetTime()
        INSERT INTO APINVHDR FROM MEMVAR  
        
        *E300296,4 Get the total invoice amount in base currency
        *E300296,4 Amoutn in base currency = 
        *E300296,4 invoice amount * exchange rate /currency units
        *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)
        *lnTotEqAmnt = ROUND(APINVHDR.nInvAmnt * APINVHDR.nExRate ;
                             / APINVHDR.nCurrUnit, 2)
        lnTotEqAmnt = ROUND(APINVHDR.nInvAmnt &lcExSin1 APINVHDR.nExRate ;
                             &lcExSin2 APINVHDR.nCurrUnit, 2)
        *E300316,1 (End)
        *** Append a record in the distribution file for the invoice (header)
        *E300296,4 Update equivalent currency fields as well.
        *INSERT INTO APDIST;
                (cVendCode, cInvNo, cApDTrTyp, dApDTrDat,;
                 cApDGlAct, nApDAmnt, cApDActID, cApdRef,;              
                 cFisFYear, cFspPrdId, cApSessNo, nApDLinNo,;
                 cAdd_User, dAdd_Date, cAdd_Time);
          VALUES(APINVHDR.cVendCode, APINVHDR.cInvNo, 'I', APINVHDR.dInvDate,;
                 APINVHDR.cApAcct, 0 - APINVHDR.nInvAmnt, 'A',;
                 APINVHDR.cInvNo,;
                 APINVHDR.cFisFYear, APINVHDR.cFspPrdId, lcSession, 0,;
                 gcUser_ID, ldCurDate, gfGetTime())
        
        INSERT INTO APDIST;
                (cVendCode, cInvNo   , cApDTrTyp, dApDTrDat,;
                 cApDGlAct, nApDAmnt , cApDActID, cApdRef,;              
                 cFisFYear, cFspPrdId, cApSessNo, nApDLinNo,;
                 cCurrCode, nExRate  , nEqvAmnt,;
                 cAdd_User, dAdd_Date, cAdd_Time);
          VALUES(APINVHDR.cVendCode, APINVHDR.cInvNo, 'I', APINVHDR.dInvDate,;
                 APINVHDR.cApAcct, 0 - m.nInvAmnt, 'A',;
                 APINVHDR.cInvNo,;
                 APINVHDR.cFisFYear, APINVHDR.cFspPrdId, lcSession, 0,;
                 APINVHDR.cCurrCode, APINVHDR.nExRate, 0 - lnTotEqAmnt,;
                 gcUser_ID, ldCurDate, gfGetTime())
        
        *E300296,4 Get the total of lines in equivalent currency
        lnTotal = 0
        *E300296,4 end
        
        *** Append a record in the distribution file for every line
        *** of the details found in APINVADT for this recurring invoice
        SELECT APINVADT
        SCAN REST WHILE &lcInvADtEx = 'R' + &lc_Invoices..cAutMCode
 
          *E300296,4 Get line amount according to the recurring
          *E300296,4 code type whether 'A'mounts or 'P'ercent
          lnAmount = IIF(&lc_Invoices..cAutMBase = 'A',;
                               APINVADT.nApDAmnt,;
                               APINVADT.nApDAmnt * APINVHDR.nInvAmnt / 100)
          *E300296,4 Update equivalent currency fields as well
          *E300296,4 with equivalent amounts in base currency.
          *INSERT INTO APDIST;
                  (cVendCode, cInvNo, cApDTrTyp, dApDTrDat,;
                   cApDGlAct, nApDAmnt, cApDActID, cApdRef,;              
                   cFisFYear, cFspPrdId, cApSessNo,;
                   nApDLinNo, cTaxCode,;
                   cAdd_User, dAdd_Date, cAdd_Time);
            VALUES(APINVHDR.cVendCode, APINVHDR.cInvNo, 'I',;
                   APINVHDR.dInvDate, APINVADT.cApDGlAct,;
                   IIF(&lc_Invoices..cAutMBase = 'A',APINVADT.nApDAmnt,;
                       APINVADT.nApDAmnt * APINVHDR.nInvAmnt / 100 ), 'D',;
                   APINVHDR.cInvNo,;      
                   APINVHDR.cFisFYear, APINVHDR.cFspPrdId, lcSession,;
                   APINVADT.nApDLinNo, APINVADT.cTaxCode,;
                   gcUser_ID, ldCurDate, gfGetTime())
  
          *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin) 
          INSERT INTO APDIST;
                  (cVendCode, cInvNo, cApDTrTyp, dApDTrDat,;
                   cApDGlAct, nApDAmnt, cApDActID, cApdRef,;              
                   cFisFYear, cFspPrdId, cApSessNo,;
                   nApDLinNo, cTaxCode,;
                   cCurrCode, nExRate  , nEqvAmnt,;
                   cAdd_User, dAdd_Date, cAdd_Time);
            VALUES(APINVHDR.cVendCode, APINVHDR.cInvNo, 'I',;
                   APINVHDR.dInvDate, APINVADT.cApDGlAct,;
                   lnAmount, 'D',;
                   APINVHDR.cInvNo,;      
                   APINVHDR.cFisFYear, APINVHDR.cFspPrdId, lcSession,;
                   APINVADT.nApDLinNo, APINVADT.cTaxCode,;
                   APINVHDR.cCurrCode, APINVHDR.nExRate,;
                   ROUND(lnAmount &lcExSin1 APINVHDR.nExRate ;
                                  &lcExSin2 APINVHDR.nCurrUnit, 2),;
                   gcUser_ID, ldCurDate, gfGetTime())
                   
                   *ROUND(lnAmount * APINVHDR.nExRate ;
                              / APINVHDR.nCurrUnit, 2),;
          *E300316,1 (End)
                   
          *E300296,4 Sum the equivalent amoutn for all fields
          lnTotal  = lnTotal + APDIST.nEqvAmnt
          *E300296,4 end.
        ENDSCAN
     
        *E300296,4 Compare the total equivalent amount (lnTotEqAmnt)
        *E300296,4 and the total equivalent amount of the invoice lines,
        *E300296,4 If there is any difference, add the difference to 
        *E300296,4 the equivalent amount of the current line in APDIST
        IF lnTotEqAmnt <> lnTotal
          SELECT APDIST
          REPLACE nEqvAmnt WITH nEqvAmnt + (lnTotEqAmnt - lnTotal)
        ENDIF
        *E300296,4 end.     
        
        *** Then update the current vendor information in the vendor's file
        *** (APVENDOR)
        SELECT APVENDOR
        **B600602,1 Update the following fileds in APVENDOR file as well 
        **B600602,1 - the vendor's cumulative purchase APVENDOR.nVenCPur
        **B600602,1 - the vendor's last purchase amount APVENDOR.nVenLPurA 
        **B600602,1 - the vendor's last purchase date   APVENDOR.dVenLPurD
        *REPLACE;
          APVENDOR.nVenBal  WITH APVENDOR.nVenBal  + APINVHDR.nInvAmnt,;
   
        *E300296,4 Update vendor amounts using the base currency
        *E300296,4 equivalent amount and not the invoice amount
        *E300296,4 itself.
        *REPLACE;
          APVENDOR.nVenBal   WITH APVENDOR.nVenBal  + APINVHDR.nInvAmnt,;
          APVENDOR.nVenCPur  WITH APVENDOR.nVenCPur + APINVHDR.nInvAmnt,;
          APVENDOR.nVenLPurA WITH APINVHDR.nInvAmnt                    ,;
          APVENDOR.dVenLPurD WITH APINVHDR.dInvDate
        **B600602,1 end.
        
        REPLACE;
          APVENDOR.nVenBal   WITH APVENDOR.nVenBal  + lnTotEqAmnt,;
          APVENDOR.nVenCPur  WITH APVENDOR.nVenCPur + lnTotEqAmnt,;
          APVENDOR.nVenLPurA WITH lnTotEqAmnt                    ,;
          APVENDOR.dVenLPurD WITH APINVHDR.dInvDate
        *E300296,4 end.
        
        *** And update his/her information in the vendor's history file
        *** (APVENHST)
        SELECT APVENHST
        
        *B600604,1 Change the period calculation expression so as not to
        *B600604,1 mix between period '01' and period '10', considering
        *B600604,1 both of them as period '1'.
        *lcPrd = ALLTRIM(STRTRAN(APINVHDR.cFspPrdId, '0'))
        lcPrd = ALLTRIM(STR(VAL(APINVHDR.cFspPrdId)))
        *B600604,1 end.
        
        *E300296,4 Update vendor amounts using the base currency
        *E300296,4 equivalent amount and not the invoice amount
        *E300296,4 itself.
        *REPLACE;
          APVENHST.nVnHDisOf     WITH APVENHST.nVnHDisOf     + APINVHDR.nInvDisOf,;
          APVENHST.nVnHPurch     WITH APVENHST.nVnHPurch     + APINVHDR.nInvAmnt,;
          APVENHST.nVnHPur&lcPrd WITH APVENHST.nVnHPur&lcPrd + APINVHDR.nInvAmnt

        REPLACE;
          APVENHST.nVnHDisOf     WITH APVENHST.nVnHDisOf     +  ;
                                      ROUND(APINVHDR.nInvDisOf  ;
                                      * APINVHDR.nExRate        ;
                                      / APINVHDR.nCurrUnit ,2 ),;
          APVENHST.nVnHPurch     WITH APVENHST.nVnHPurch     + lnTotEqAmnt,;
          APVENHST.nVnHPur&lcPrd WITH APVENHST.nVnHPur&lcPrd + lnTotEqAmnt
        *E300296,4 end.
          
        ENDIF   
        SELECT (lc_Invoices)
      ENDIF
      
      *** Unlock any previous locking
      =gfRLock("APINVAHD,APVENDOR,APVENHST",.F.) 
    ENDSCAN
  
  *** Close the thermometer if not already closed
  IF lnTotRec > lnCurRec
    FOR lnCount = lnCurRec TO lnTotRec
      =gfThermo(lnTotRec, lnCount, lcTUpdRcr, "")
    ENDFOR
  ENDIF  

  *** Set relations off in (lc_Invoices) file
  SELECT (lc_Invoices)
  SET RELATION TO 

  *** Reset all variables and files, to be ready for a new selection
  llMayGenr = .T.
  =lfClearGen()
ELSE
  *** Message :  "       There are no ð.    "
  ***                      <   OK   >
  =gfModalGen("TRM04101B00000","DIALOG", lcTNoSInv)
ENDIF

SET EXACT &lcSetExact
SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)

*!**************************************************************************
*!
*!      Function: lfVldNextDate
*!
*!**************************************************************************
*
FUNCTION lfVldNextDate
PARAMETERS ldNxtDate
PRIVATE lcPeriod, lcYear, llVldDate

lcPeriod = SPACE(2)
lcYear   = SPACE(4)
llVldDate = .F.

*** Check if the date lies within the posting window. 
*** If it does not, present the following message and return .f.
*** Message :  "   Recuuring invoice ð cannot be generated on ð and   "
***            "   afterwards, because this date lies in a locked     "
***            "   period.                                            "
***                                 <   OK   >
*** else, if it lies in a locked period, present the follwoing
*** message and return .f.
*** Message :  "   Recuuring invoice ð cannot be generated on ð and   "
***            "   afterwards, because this date lies in a locked     "
***            "   period.                                            "
***                                 <   OK   >

llVldDate = ! ( (!lfVlDate(gcPrnt_Cmp, @lcPeriod, @lcYear, ldNxtDate, @llVldDate);
             .AND. gfModalGen('TRM04114B00000', 'DIALOG', ;
                ALLTRIM(cAutMCode) + '|' + DTOC(ldNxtDate)) > 0);
             .OR. ;
             (llVldDate;
             .AND. gfModalGen('TRM04115B00000', 'DIALOG', ;
             ALLTRIM(cAutMCode) + '|' + DTOC(ldNxtDate))> 0) )
lcGenPrd  = lcPeriod
lcGenYear = lcYear

RETURN llVldDate
  
*!**************************************************************************
*!
*!      Function: lfGetNextDate
*!
*!**************************************************************************
*
FUNCTION lfGetNextDate
PARAMETERS ldNxtDate, lnNumOfPrds
PRIVATE lcCurAlias, lnCountDays, lnCountPrds

lcCurAlias  = ALIAS()
STORE 0 TO lnCountDays, lnCountPrds

*E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
*SELECT SYCFSPRD
SELECT FSPRD
*E300692,1 end
*E300789,4  AMM Adjust to fit the new structure
*IF SEEK(gcPrnt_Cmp)
GO TOP
IF !EOF()
*E300789,4  AMM end
  *B600650,1  Move the record pointer to the right period
  LOCATE REST FOR BETWEEN(ldNxtDate, dFspPBgDt, dFspPEnDt)
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
  *lnCur_Pdr = VAL(SYCFSPRD.cFspPrdID)
  lnCur_Pdr = VAL(FSPRD.cFspPrdID)
  *E300692,1 end
  
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
  *SCAN REST WHILE SYCFSPRD.cComp_ID = gcPrnt_Cmp;
            .AND. lnCountPrds < lnNumOfPrds
    *lnCountDays = lnCountDays  + ;
                   (SYCFSPRD.dFspPEnDt - SYCFSPRD.dFspPBgDt) + 1
                   
  *E300789,4  AMM Adjust to fit the new structure
  *SCAN REST WHILE FSPRD.cComp_ID = gcPrnt_Cmp;
            .AND. lnCountPrds < lnNumOfPrds
  SCAN REST WHILE lnCountPrds < lnNumOfPrds
  *E300789,4  AMM end
    lnCountDays = lnCountDays  + ;
                   (FSPRD.dFspPEnDt - FSPRD.dFspPBgDt) + 1
  *E300692,1 end                
    lnCountPrds = lnCountPrds + 1
  ENDSCAN            
ENDIF

ldNxtDate = IIF(lnCountPrds = lnNumOfPrds, ;
                ldNxtDate + lnCountDays, {})

*B600650,1 Make sure that we did not jump a period while calculating the next date
*E300789,4  AMM Adjust to fit the new structure
*= SEEK(gcPrnt_Cmp)
GO TOP
*E300789,4  AMM end
LOCATE REST FOR BETWEEN(ldNxtDate, dFspPBgDt, dFspPEnDt)
*E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
*DO WHILE lnCur_Pdr + lnNumOfPrds < VAL(SYCFSPRD.cFspPrdID)
DO WHILE lnCur_Pdr + lnNumOfPrds < VAL(FSPRD.cFspPrdID)
*E300692,1 end
  SKIP -1
  ldNxtDate =   dFspPEnDt
ENDDO
SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
              
RETURN ldNxtDate   

*!**************************************************************************
*!
*!      Function: lfVldGlAccts
*!
*!**************************************************************************
*
FUNCTION lfVldGlAccts
PARAMETERS lcAutHdrFl, lcRcrCode
PRIVATE lcCurAlias, llVldAcct

lcCurAlias = ALIAS()
llVldAcct  = .T.

SELECT (lcAutHdrFl)

*** lcTChkGl = GL checking account or cash payment account ?
*** Message :  "   Recurring invoice ð has ð account ð that is not     "
***            "   found in the chart of accounts of company ð. This   "
***            "   recurring invoice cannot be generated.              "
***                                <   OK   >
llVldAcct =  (SEEK(cApAcct, 'lcLinkChar') ;
                .OR. gfModalGen('TRM04116B00000', 'DIALOG',;
                      ALLTRIM(lcRcrCode) + '|' + lcTAP + '|' + ;
                      ALLTRIM(cApAcct) + '|' + ALLTRIM(gcPrnt_Cmp)) < 0);
             .AND. ;
              (EMPTY(cChkGlAcc);
               .OR. (SEEK(cChkGlAcc, 'lcLinkChar') ; 
                     .OR. gfModalGen('TRM04116B00000', 'DIALOG',;
                      ALLTRIM(lcRcrCode) + '|' + ;
                      IIF(cVenPMeth = 'H', lcTCashP, lcTChkGl) + '|' + ;
                      ALLTRIM(cChkGlAcc) + '|' + ALLTRIM(gcPrnt_Cmp)) < 0) )   


SELECT APINVADT
SCAN REST WHILE llVldAcct ;
          .AND. &lcInvADtEx = 'R' + lcRcrCode
  llVldAcct = SEEK(cApDGlAct, 'lcLinkChar') ;
              .OR. gfModalGen('TRM04116B00000', 'DIALOG',;
                ALLTRIM(lcRcrCode) + '|' + lcTDist + '|' + ;
                ALLTRIM(cApDGlAct) + '|' + ALLTRIM(gcPrnt_Cmp)) < 0
ENDSCAN          

SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)

RETURN llVldAcct

*!**************************************************************************
*!
*!      Function: lfvSelect
*!
*!**************************************************************************
*
FUNCTION lfvSelect
PARAMETERS lcFileName, lcBrTtl

IF EMPTY(&lcFileName..cInclude)
  REPLACE &lcFileName..cInclude WITH lcCheck
  lcSlPr  = lcTUnSelect
  SHOW GET pbSelect,1 PROMPT lcTUnSelect
ELSE
  REPLACE &lcFileName..cInclude WITH lcUnCheck
  lcSlPr  = lcTSelect
  SHOW GET pbSelect,1 PROMPT lcTSelect
ENDIF
llUpdated   = .T.
SHOW WINDOW (lcBrTtl) REFRESH SAME

*!**************************************************************************
*!
*!      Function: lfvSelAll
*!
*!**************************************************************************
*
FUNCTION lfvSelAll
PARAMETERS lcFileName, lcBrTtl, lcCheck
PRIVATE lcCurAlias, lnRecNo

lcCurAlias = ALIAS()
SELECT (lcFileNAme)
lnRecNo    = RECNO()
REPLACE ALL cInclude WITH lcCheck
IF lnRecNo <=RECCOUNT()
  GO lnRecNo 
ELSE
  GO TOP 
ENDIF  
IF !EMPTY(cInclude)
  lcSlPr  = lcTUnSelect
  SHOW GET pbSelect,1 PROMPT lcTUnSelect
ELSE
  lcSlPr  = lcTSelect
  SHOW GET pbSelect,1 PROMPT lcTSelect
ENDIF
llUpdated   = .T.
SHOW WINDOW (lcBrTtl) REFRESH SAME
SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)

*!**************************************************************************
*!
*!      Function: lfvInvert
*!
*!**************************************************************************
*
FUNCTION lfvInvert
PARAMETERS lcFileName, lcBrTtl
PRIVATE lcCurAlias, lnRecNo

lcCurAlias = ALIAS()
SELECT (lcFileName)
lnRecNo    = RECNO()
REPLACE ALL cInclude WITH ;
        IIF(EMPTY(cInclude), lcCheck, lcUncheck) 
IF lnRecNo <=RECCOUNT()
  GO lnRecNo  
ELSE
  GO TOP
ENDIF  
IF !EMPTY(cInclude)
  lcSlPr  = lcTUnSelect
  SHOW GET pbSelect,1 PROMPT lcTUnSelect
ELSE
  lcSlPr  = lcTSelect
  SHOW GET pbSelect,1 PROMPT lcTSelect
ENDIF
llUpdated   = .T.
SHOW WINDOW (lcBrTtl) REFRESH SAME
SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)

*!**************************************************************************
*!
*!      Function: lfwBrowRcr
*!
*!**************************************************************************
*
FUNCTION lfwBrowRcr
PRIVATE lcClrSchm

lcClrSchm  = IIF(_DOS," COLOR SCHEME 13","NOMENU")

SELECT (lc_SelInv)
lnBrRecNo  = RECNO(lc_SelInv)

*E300296,4 Add currency code field to the browse.

*E300643,1 Change this line for the changes we have made to (gfCodDes) [Begin]
*BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,; 
*              cAutMCode :H=laBrSelHdr[1]:R,;
*              cCurrCode :H='Curr.':R,;	
*              cInclude  :H=laBrSelHdr[2]:R:1,;
*              cType = lfGetArrElem(cAutMBase, 'laType');
*                        :H=laBrSelHdr[3]:R,;
*              cDiv  = gfCodDes(cDivision):H=laBrSelHdr[4]:R,;
*              cInvRef   :H=laBrSelHdr[5]:R;
*         WINDOW APSELIN1 ;
*         IN WINDOW AWDAPSELINV ;
*         LOCK 0;
*         NOAPPEND;
*         NOEDIT;
*         NOCLEAR;
*         NODELETE;
*         NOWAIT;
*         SAVE;
* 	      TITLE lcBrSelTtl;
* 	      WHEN lfwBroRcr();
*         &lcClrSchm

BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,; 
              cAutMCode :H=laBrSelHdr[1]:R,;
              cCurrCode :H='Curr.':R,;	
              cInclude  :H=laBrSelHdr[2]:R:1,;
              cType = lfGetArrElem(cAutMBase, 'laType');
                        :H=laBrSelHdr[3]:R,;
              cDiv  = gfCodDes(cDivision , 'CDIVISION'):H=laBrSelHdr[4]:R,;
              cInvRef   :H=laBrSelHdr[5]:R;
         WINDOW APSELIN1 ;
         IN WINDOW AWDAPSELINV ;
         LOCK 0;
         NOAPPEND;
         NOEDIT;
	     NOCLEAR;
         NODELETE;
         NOWAIT;
         SAVE;
 	     TITLE lcBrSelTtl;
 	     WHEN lfwBroRcr();
 	     &lcClrSchm

*E300643,1 Change this line for the changes we have made to (gfCodDes) [End]

*!**************************************************************************
*!
*!        Function : lfvInvAmnt
*!
*!**************************************************************************
*  Valid function for lnInvAmnt (invoice amount)
*
FUNCTION lfvInvAmnt

*** if the entered value is not greater than zero,
***     Message :  "   ð should be greater than ð.   "
***                            <   OK   >
*** restore the old value of the field
IF lnInvAmnt <> lnOldVal .AND.;
   (( !(lnInvAmnt > 0 ) .AND. ;
     gfModalGen("TRM04072B00000","DIALOG",lcTInvAmnt+'|'+lcTZero) > 0)) 
  lnInvAmnt = lnOldVal
  _CUROBJ   = OBJNUM(lnInvAmnt)
  SHOW GET lnInvAmnt
ENDIF

*!**************************************************************************
*!
*!        Function : lfvContinue
*!
*!**************************************************************************
*  Valid function for lnInvAmnt (invoice amount)
*
FUNCTION lfvContinue

*** if the invoice amount is not greater than zero,
***     Message :  "   ð should be greater than ð.   "
***                            <   OK   >
*** restore the old value of the field
IF (( !(lnInvAmnt > 0 ) .AND. ;
     gfModalGen("TRM04072B00000","DIALOG",lcTInvAmnt+'|'+lcTZero) > 0)) 
  _CUROBJ = OBJNUM(lnInvAmnt)
ELSE
  CLEAR READ
ENDIF

*!**************************************************************************
*!
*!      Function: lfGetArrElem
*!
*!**************************************************************************
*
FUNCTION lfGetArrElem
PARAMETERS lcSrchCode,laSrchArr

PRIVATE lnArrElem

lnArrElem = 1

DO WHILE lcSrchCode <> &laSrchArr[lnArrElem, 2] .AND. ;
         lnArrElem < ALEN((laSrchArr),1)
  lnArrElem = lnArrElem + 1
ENDDO  

RETURN &laSrchArr[lnArrElem,1]

*!**************************************************************************
*!
*!      Function: lfMousClk
*!
*!**************************************************************************
* mouse trap 
*
FUNCTION lfMousClk
PARAMETERS lcFileName, lcBrTtl   

ON KEY LABEL LEFTMOUSE DO lpMClkSel WITH &lcFileName, &lcBrTtl

*!**************************************************************************
*!
*!      Procedure: lpMClkSel
*!
*!**************************************************************************
*If mouse double click select record.
*
FUNCTION lpMClkSel
PARAMETERS lcFileName, lcBrTtl

IF SECONDS() < lnTime .AND. RECCOUNT(lcFileName) > 0
  =lfvSelect(lcFileName, lcBrTtl)
ENDIF
lnTime = _DBLCLICK+SECONDS()

*!**************************************************************************
*!
*!      Function: lfClrMous
*!
*!**************************************************************************
*Clear mouse trap. 
FUNCTION lfClrMous
ON KEY LABEL LEFTMOUSE
           
*!**************************************************************************
*!
*!      Procedure: lpCtrlEntr
*!
*!**************************************************************************
*
PROCEDURE lpCtrlEntr
IF _DOS
  HIDE MENU _MSYSMENU SAVE
  SHOW MENU _MSYSMENU SAVE
ENDIF  
IF WPARENT() = 'AWDAPSELIN'    
  ACTIVATE WINDOW ('APSELIN2')
  _CUROBJ=OBJNUM(pbOk)
ELSE
  ACTIVATE WINDOW (lcGnRcrCh3)
  _CUROBJ=OBJNUM(pbClose)    
ENDIF
KEYBOARD CHR(13)

*!**************************************************************************
*!
*!      Procedure: lpEsc
*!
*!**************************************************************************
*
PROCEDURE lpEsc
IF _DOS
  HIDE MENU _MSYSMENU SAVE
  SHOW MENU _MSYSMENU SAVE
ENDIF  
IF WPARENT() = 'AWDAPSELIN'    
  ACTIVATE WINDOW ('APSELIN2')
  _CUROBJ=OBJNUM(pbCancel)
ELSE
  ACTIVATE WINDOW (lcGnRcrCh3)
  _CUROBJ=OBJNUM(pbClose)    
ENDIF
KEYBOARD CHR(13)

*!**************************************************************************
*!
*!      Procedure: lpTab
*!
*!**************************************************************************
*
PROCEDURE lpTab
IF _DOS
  HIDE MENU _MSYSMENU 
  SHOW MENU _MSYSMENU 
ENDIF  
ON KEY LABEL TAB lnDummmi = 1
DO CASE
  *** Cases of the main screen (gcBaseWind)
  CASE WONTOP(lcGnRcrCh1) .AND. _CUROBJ = OBJNUM(ldGenDate) 
    =lfvGenDate()
    _CUROBJ = _CUROBJ + 1

  CASE WONTOP(lcGnRcrCh1) .AND.;
    (llMayGenr .AND. _CUROBJ = OBJNUM(pbGenerate);
    .OR. !llMayGenr .AND. _CUROBJ = OBJNUM(pbUpdate))     
    ACTIVATE WINDOW (lcBrInvTtl)

  CASE WONTOP(lcBrInvTtl)
    ACTIVATE WINDOW (lcGnRcrCh3)
*    _CUROBJ = IIF(llMayGenr, OBJNUM(pbClose), OBJNUM(pbSelect))
     _CUROBJ = OBJNUM(pbSelect)
  CASE WONTOP(lcGnRcrCh3) .AND. _CUROBJ = OBJNUM(pbClose)
    ACTIVATE WINDOW (lcGnRcrCh1)
    _CUROBJ = OBJNUM(ldGenDate)
    
  *** Cases of 'Select recurring invoices' (AWDSELINV) screen  
  CASE WONTOP(lcBrSelTtl) 
    ACTIVATE WINDOW APSELIN2
    _CUROBJ = OBJNUM(pbSelect)

  CASE WONTOP('APSELIN2') .AND. _CUROBJ = OBJNUM(pbCancel)    
    ACTIVATE WINDOW (lcBrSelTtl)

  OTHERWISE
    _CUROBJ = _CUROBJ + 1
ENDCASE  
ON KEY LABEL TAB DO lpTab

*!**************************************************************************
*!
*!      Procedure: lpShiftTab
*!
*!**************************************************************************
*
PROCEDURE lpShiftTab
IF _DOS
  HIDE MENU _MSYSMENU SAVE
  SHOW MENU _MSYSMENU SAVE
ENDIF  
ON KEY LABEL BACKTAB lnDummmi = 1

DO CASE
  *** Cases of the main screen (gcBaseWind)
  CASE WONTOP(lcGnRcrCh1) .AND. _CUROBJ = OBJNUM(ldGenDate) 
    =lfvGenDate()
    ACTIVATE WINDOW (lcGnRcrCh3)
    _CUROBJ = OBJNUM(pbClose)

  CASE WONTOP(lcBrInvTtl)
    ACTIVATE WINDOW (lcGnRcrCh1)
    _CUROBJ = IIF(llMayGenr, OBJNUM(pbGenerate), OBJNUM(pbUpdate))

  CASE WONTOP(lcGnRcrCh3) .AND. ;
     (llMayGenr .AND. _CUROBJ = OBJNUM(pbClose);
     .OR. !llMayGenr .AND. _CUROBJ = OBJNUM(pbSelect))
    ACTIVATE WINDOW (lcBrInvTtl)
    
  *** Cases of 'Select recurring invoices' (AWDSELINV) screen  
  CASE WONTOP(lcBrSelTtl) 
    ACTIVATE WINDOW APSELIN2
    _CUROBJ = OBJNUM(pbCancel)

  CASE WONTOP('APSELIN2') .AND. _CUROBJ = OBJNUM(pbSelect)    
    ACTIVATE WINDOW (lcBrSelTtl)

  OTHERWISE
    _CUROBJ = _CUROBJ - 1
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
ON KEY LABEL ESC        DO lpEsc
ON KEY LABEL Ctrl+ENTER DO lpCtrlEntr
ON KEY LABEL Alt-L      DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-P      DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-G      DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-D      DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-S      DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-A      DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-N      DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-I      DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-V      DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-K      DO lpBrowKeyTrap WITH LASTKEY()
ON KEY LABEL Alt-C      DO lpBrowKeyTrap WITH LASTKEY()

*!**************************************************************************
*!
*!      Function: lfBrowUnTrap
*!
*!*******************************************************************************
*  Sets key traps when a browse is active.
*  function : to enable calling the outer objects on an ALT+hot key presss
*
FUNCTION lfBrowUnTrap
ON KEY LABEL ESC &lcEscOnKey
ON KEY LABEL CTRL+ENTER
ON KEY LABEL Alt-L 
ON KEY LABEL Alt-P 
ON KEY LABEL Alt-G 
ON KEY LABEL Alt-D 
ON KEY LABEL Alt-S 
ON KEY LABEL Alt-A 
ON KEY LABEL Alt-N 
ON KEY LABEL Alt-I 
ON KEY LABEL Alt-V 
ON KEY LABEL Alt-K 
ON KEY LABEL Alt-C 

*!**************************************************************************
*!
*!      Procedure: lpBrowKeyTrap
*!
*!*******************************************************************************
*  Key traps when browsing
*
PROCEDURE lpBrowKeyTrap
PARAMETERS lnLastKey

KEYBOARD "{Z}"             && dummy key press to release menu access
DO CASE
  CASE WONTOP(lcBrInvTtl)
    DO CASE 
      CASE lnLastKey = 38    && Alt-L   () All recur. payables
        rbRcrInv = 1
        SHOW GET rbRcrInv
        =lfvRcrInv()
      CASE lnLastKey = 25    && Alt-P   () Select recur. payables
        rbRcrInv = 2
        SHOW GET rbRcrInv
        =lfvRcrInv()
    ENDCASE
    IF llMayGenr    
      IF lnLastKey = 34      && Alt-G   < Generate >
        =lfvGenerate()
      ENDIF
    ELSE
      DO CASE    
        CASE lnLastKey = 32  && Alt-D   < Update >
          =lfvUpdate()
        CASE lnLastKey = 31  && Alt-S   < Select >
          =lfvSelect(lc_Invoices, lcBrInvTtl)
        CASE lnLastKey = 30    && Alt-A   < Select all >
          =lfvSelAll(lc_Invoices, lcBrInvTtl, lcCheck)
        CASE lnLastKey = 49    && Alt-N   < Select none >
          =lfvSelAll(lc_Invoices, lcBrInvTtl, lcUnCheck)
        CASE lnLastKey = 23    && Alt-I   < Invert >
          =lfvInvert(lc_Invoices, lcBrInvTtl)
        CASE lnLastKey = 46    && Alt-C   < Cancel > / < Close >
          ACTIVATE WINDOW (lcGnRcrCh3)
          _CUROBJ=OBJNUM(pbClose)    
          KEYBOARD CHR(13)        
      ENDCASE  
    ENDIF  

  CASE WONTOP(lcBrSelTtl)
    
    IF RECCOUNT(lc_SelInv) > 0
      DO CASE
        CASE lnLastKey = 31  && Alt-S   < Select >
          =lfvSelect(lc_SelInv, lcBrSelTtl)
        CASE lnLastKey = 30    && Alt-A   < Select all >
          =lfvSelAll(lc_SelInv, lcBrSelTtl, lcCheck)
        CASE lnLastKey = 49    && Alt-N   < Select none >
          =lfvSelAll(lc_SelInv, lcBrSelTtl, lcUnCheck)
        CASE lnLastKey = 23     && Alt-I   < Invert >
          =lfvInvert(lc_SelInv, lcBrSelTtl)
      ENDCASE
    ENDIF    
    DO CASE
      CASE lnLastKey = 37     && Alt-K   <   Ok   >
        ACTIVATE WINDOW ('APSELIN2')
        _CUROBJ=OBJNUM(pbOk)
        KEYBOARD CHR(13)    
  
      CASE lnLastKey = 46    && Alt-C   < Cancel > / < Close >
        ACTIVATE WINDOW ('APSELIN2')
        _CUROBJ=OBJNUM(pbCancel)
        KEYBOARD CHR(13)
    ENDCASE  
ENDCASE

*!**************************************************************************
*!
*!        Function : lfwBroRcr
*!
*!**************************************************************************
* 
FUNCTION lfwBroRcr
lnBrRecNo  = RECNO(lc_SelInv)
SHOW GET pbSelect,1 PROMPT IIF(!EMPTY(cInclude),lcTUnSelect, lcTSelect)
SHOW WINDOW (WONTOP()) REFRESH

*!**************************************************************************
*!
*!        Function : lfwBroWhen
*!
*!**************************************************************************
* 
FUNCTION lfwBroWhen
PARAMETERS lcFileName, lcBrTtl

glFromBrow  = .T.
lnBrRecNo   = RECNO(&lcFileName)

IF !EMPTY(cInclude)
  lcSlPr  = lcTUnSelect
  SHOW GET pbSelect,1 PROMPT lcTUnSelect
ELSE
  lcSlPr  = lcTSelect
  SHOW GET pbSelect,1 PROMPT lcTSelect
ENDIF
=lfBrowTrap() .AND. lfMousClk(lcFileName, lcBrTtl)
SHOW WINDOW (&lcBrTtl) REFRESH

*!**************************************************************************
*!
*!      Function: lfActBrows
*!
*!**************************************************************************
*
FUNCTION lfActBrows
PARAMETERS lcBrTtl

RELEASE PAD _BROWSE OF _MSYSMENU
ON KEY LABEL ALT+B
ACTIVATE WINDOW (lcBrTtl) TOP

*!**************************************************************************
*!
*!      Function: lfDefinePad
*!
*!**************************************************************************
* Defines an additional 'Browse' pad when one does not exist (i.e. when
* a browse is not active)
*
FUNCTION lfDefinePad
PARAMETERS lcBrTtl

DEFINE PAD _BROWSE OF _MSYSMENU PROMPT lcTBrowse
ON SELECTION PAD _BROWSE OF _MSYSMENU DO lfActBrows WITH &lcBrTtl
ON KEY LABEL ALT+B DO lfActBrows WITH &lcBrTtl 

*!**************************************************************************
*!
*!      Function: lfDeact
*!
*!**************************************************************************
* Current window's deactivate function
* releases the browse pad if a browse window is on top, and traps keys
* and vice versa.
*
FUNCTION lfDeact
PARAMETERS lcFilename, lcBrTtl 

IF WONTOP(&lcBrTtl)
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY LABEL ALT+B
  glFromBrow  = .T.
  =lfBrowTrap() .AND. lfMousClk(lcFileName, lcBrTtl)
ELSE
  =lfBrowUnTrap() .AND. lfClrMous()
  IF _DOS
    ON KEY LABEL ALT+B DO lfActBrows WITH &lcBrTtl
  ENDIF  
ENDIF
RETURN llFromModal

*!**************************************************************************
*!
*!        Function : lfClearGen
*!
*!**************************************************************************
* 
FUNCTION lfClearGen
PARAMETERS llFromRcrBt

PRIVATE lcCurAlias

lcCurAlias = ALIAS()
SELECT (lc_Invoices)
ZAP
=lfBrowInv()
lcGenStat  = 'ENABLE'
lcUpdStat  = 'DISABLE'
IF !llFromRcrBt
  STORE 1 TO rbRcrInv, lnOldrbRcr
  SHOW GET rbRcrInv
  SELECT (lc_SelInv)
  REPLACE ALL cInclude WITH lcUnCheck, cOldIncl WITH lcUncheck
ENDIF
=lfShowInv(lcUpdStat)
SHOW GET pbGenerate ENABLE

SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)

*!**************************************************************************
*!
*!        Function : lfSelAct
*!
*!**************************************************************************
* 
FUNCTION lfSelAct
IF WONTOP('APSELIN2') .AND. _DOS
  =lfDefinePad('lcBrSelTtl')
ELSE
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY LABEL ALT+B
ENDIF