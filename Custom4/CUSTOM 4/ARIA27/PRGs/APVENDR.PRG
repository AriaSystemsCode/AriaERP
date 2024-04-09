*:************************************************************************
*:
*: Procedure file: APVENDR.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Malak Hanna Aziz
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lpShow       *** Show proc. for home screen ***
*:               : lfwOldValue  
*:               : lfwCodeFilt  
*:               : lfvData_1
*:               : lfvPhonComp
*:               : lfvData_18
*:               : lfvDivision
*:               : lfvSelWind
*:               : lfv1099Type
*:               : lfvTermCode
*:               : lfvPayMethd
*:               : lfvData_43
*:               : lfvData_32
*:               : lfvData_33
*:               : lfvBnkChk
*:               : lfvTaxType
*:               : lpEscPred
*:               : lpTrapKeys
*:               : lfvTaxPerc
*:               : lpvInquiry
*:               : lfvFiscYear
*:               : lpSavScr
*:               : lpDelScr
*:               : lfDispAcsDes
*:               : lfwAdFormat
*:               : lfvAdFormat
*:               : lfvNextWind
*:               : lfBrowse
*:               : lfwData_49
*:               : lfvData_49
*:
*:      Documented 25/06/1994
*:************************************************************************
*B600345,1 YMO 05/28/95 Add an extra SKIP expression to the _INQUIRY PAD in the window when clause
*B600345,1              and move the window when clause to the PRG from the screen
*E300258,4 MAN 06/29/95 Add the check on supressing 1099 processing
*B600578,1 YMO 07/26/95 The Cash payment method is save as Credit under Win
*B600590,1 YMO 07/30/95 In view mode the Inquire menu pad is not enable
*B600599,1 YMO 08/03/95 The addresses field width are set to USA not 30 in the add mode
*B600608,1 YMO 08/08/95 The system hangs when you press left arrow in the control panel as
*B600608,1              the end invisable button of the child screens was disabled
*E300266,5 MAN 08/08/95 Default the address code with company default in the SCX.
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*B800275,1 M.H 11/01/95 Fix the history screen because when we change
*B800275,1 M.H 11/01/95 the fiscal year the history screen disapeare 
*B800275,1 M.H 11/01/95 behind the vendor screen.
*E300331,1 YMO 12/27/95 In the vendor/Inquiry/Payables, we should see by default only the "Open Payables" and not everything 
*E300331,1              Add a balance field in the browse.
*E500061,1 M.H 02/06/96 Rearrange the browse fields.
*B601013,1 RENEE 04/02/96 Add the currency code fields to the vendor's 
*B601013,1                payments and payables browse.
*B601066,1 Hesham El-Sheltawi 05/16/96
*B601066,1 change the header of the CPAYSTAT for payment state
*B601066,1 to Payment Status
*B601602,1 HS 02/16/97 1)Fix the function [lfGetAddress] to adjust the
*B601602,1               address fields width.
*B601602,1             2)Change the validation function of the Invisible
*B601602,1               Button [ibAddress] to call the new function
*B601602,1               [lfvContCod] instead of the function [lfvAddress]
*B601602,1               to be able to adjust the address fields width and
*B601602,1               the fields labels when the user change the Country
*B601602,1               code.
*B601578,1 RENEE 03/12/97 Add soft seek to the vendor company and phone company browse.
*E300643,1  HS 04/15/97 1)Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300643,1              2)Make some changes for we have change the function
*E300643,1              [gfCodDes]
*E300643,1              3)Make some changes for we have change the function
*E300643,1              [gfRltFld]
*E300683,1 AHMED & RENEE 06/04/97 Add screens directory path to the calling of SPRS
*B801121,1 MAN   06/12/97 Handling Delay on Payables Inq.
*E300692,5 RENEE 07/03/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,5 					SYCFSPRD, SYCFSHLD
*B801165,1 RENEE 07/10/97 Inquiry browse optimization
*E300824,1 AMM 03/02/98 Add some variables and adjust the way of calling 
*E300824,1 AMM          function to be able to call this program from any 
*E300824,1 AMM          module, not AP only.
*B601992,1 RENEE 03/29/98 If 1099 is suppressed from the APSETUP, inquire
*B601992,1 			      on payments, select a payment from the borwse, you get
*B601992,1 			      an 'unrecongnized command or keyword'
*E300872,4 AMM 05/04/98 Update the communication 'cmtrace' file with required records.
*E300798,1 AMM 08/01/98 Add two check boxes to choose vendor supplier type, 
*E300798,1 AMM          vendor invoice type in the vendor screen.
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*B602483,1 Hesham 02/11/99
*B602483,1 define variable llfuncall before calling any functions in the ap appl. to
*B602483,1 stop the cleanup function from working
*E301077,71 AMM 02/17/99 Reduce opened files
*B801867,1 AMM 02/17/99 Replace the POPUP's of DIVISION, Terms, 1099 Types,
*B801867,1 AMM          country addresses with a new ones with the new technique 
*B801867,1 AMM          of the code POPUP's
*B801920,1 AMM 02/18/99 For the 'Vendor supplies', make sure that the module 
*B801920,1 AMM          relevant to an option is installed before giving 
*B801920,1 AMM          that option to the user.
*B802103,1 RENEE 03/31/99 Alias SYCCOMP not found when called from PO or MA
*B602789,1 AMM 04/14/99 Add two settings related to vendor supplies type (contractor)
*E301303,1 AKA 07/27/99 Add new field nVenAdmin to Vendor master table
*E301119,1 MAB 10/31/1999 Add salutation screen to menu options.
*B802697,1 AMM 12/13/1999 Add 'Style' to 'Vendor supplies' if PS installed
*B603585,1 AME 06/06/2000 Country Discription will appear depending on Country POPUP 
*E301436,1 NAD 07/11/2000 Add ability to have multiple contacts for vendor.  
*B803677,1 SSE 12/04/2000 Fix bug "File lpClsScr.prg does not exist" when pressing Cancel
*B803920,1 AME 12/24/2000 Fix bug "File APINVHST does not exist" when trying to delete vendor 
*B803920,1 AME            while the client does not have AP module installed.  
*C200178,1 AAN 03/15/2001 Add a user define field "CVENPYTYPE" in ApVendor for Cathy Daniles 
*B605942,1 ALB 06/30/2002 Refresh payment methode fields
*B606434,1 ALB 09/25/2002 Fix the contact screen 
*C102740,1 ALB 10/12/2002 Attach warehouse to the vendor
*B606427,1 ALB 10/22/2002 Fix the bug "Syntax error" in Multisession after bank/Gl Account brows 
*B605547,1 ALB 11/10/2002 Add Orders to vendor option screen
*B606681,1 ALB 12/31/2002 add service contractor if PO is also installed.
*B607422,1 ALB 07/20/2003 Add Matireal Order to Vendor option menun
*B038431,1 NNA 08/23/2004 fix bug that in the Vendor screen if the MA Module not installed and You 
*B038431,1 NNA            click on the Vendor Supplies you will see the target mover have 'Material'
*B038431,1 NNA            and all buttons are Disable
*C124836,1 MHM 11/25/2004 Custom auto create material PO from Sales Order
*:**********************************************************************************************

PARAMETERS lcVenCode
PRIVATE laBankObjs 
llFromInvo = IIF(TYPE('lcVenCode') $ 'UL',.F.,.T.)

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc,laFactorAd,laCtrStat
DECLARE laKeyField[1,4],laWindName[5,3],laPayMethd[1,2],laWndObj[8,3],;    
        laFiscYear[1,3],laFactorAd[8,2],laTermAry[3,2],laHistory[1],;
        laBankObjs[2,3]    
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'VENCODE'
laKeyField[1,4] = 1

*E301436,1   (Start)  variable to be a mark for the current record in the contact browse. 
lnMarker = 0
llContChng=.F.
STORE SPACE(0) TO lcDefCont
STORE SPACE(0) TO LcOldCont,lcOldVend
*E301436,1 (End) 
** laFiscYear     : Variable to hold 
** laPayMethd     : Variable to hold 
** laHistory      : Variable to hold 
** laFactorAd     : Variable to hold 
**
** lcOldYear      : Variable to hold 
** lcWinVenC0     : Variable to hold 
** lcWinVenC1     : Variable to hold 
** lcWinVenC2     : Variable to hold 
** lcWinVenC3     : Variable to hold 
** lcWinVenC4     : Variable to hold 
** lcWinVenC5     : Variable to hold 
** lcWinshow5     : Variable to hold 
** lcDefaAddr     : Variable to hold 
** lcFiltExp      : Variable to hold 
** lcModal        : Variable to hold 
** lcYearName     : Variable to hold 
** lcHistory      : Variable to hold 
** lcCodeFilt     : Variable to hold 
** lcDivision     : Variable to hold 
** lcWindow       : Variable to hold 
** lcExisWind     : Variable to hold 
** lcExAcDes      : Variable to hold 
** lcNextWin      : Variable to hold 
** lcApAcDes      : Variable to hold 
** lc1099Type     : Variable to hold 
** lcTermCode     : Variable to hold 
** lcBankDes      : Variable to hold 
** lcChecDes      : Variable to hold 
** lcPayMethd     : Variable to hold 
** lcOldAcct      : Variable to hold 
** lcOldDesc      : Variable to hold 
** lgOldValue     : Variable to hold 
** lcGroup1       : Variable to hold 
** lcGroup2       : Variable to hold 
** lcGroup3       : Variable to hold 
** lcPercStat     : Variable to hold 
** lcChkStat      : Variable to hold state of displayof checking account
** lc_Title       : Variable to hold 
** lcAddHed1      : Variable to hold 
** lcAddHed2      : Variable to hold 
** lcAddHed3      : Variable to hold 
** lcAddHed4      : Variable to hold 
** lcAddHed5      : Variable to hold 
** lcAddHed6      : Variable to hold 
** lcFisYear      : Variable to hold 
** lcCusScheme    : Variable to hold 
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
** lcOldCurr      : Variable to hold the old Currency.  && Added by Mohamed Hassan
**
** pu1099type     : Variable to hold 
** puTermCode     : Variable to hold 
** puDivision     : Variable to hold 
** puAddress      : Variable to hold 
**
** lnPrintChe     : Variable to hold 
** lnManulChe     : Variable to hold 
** lnOfferd       : Variable to hold 
** lnTaken        : Variable to hold 
** lnNonChecks    : Variable to hold 
** lnCredCard     : Variable to hold 
** lnApplDeb      : Variable to hold 
** lnApplAdj      : Variable to hold 
** lnTotPurc      : Variable to hold 
** lnCashPay      : Variable to hold 
** lnTotPay       : Variable to hold 
** lnAvgPurc      : Variable to hold 
** lnAvgPay       : Variable to hold 
** lnCrAvail      : Variable to hold 
** laArltFlDat    : Variable to hold 
** lnDueDays      : Variable to hold 
** lnDiscDays     : Variable to hold 
** lnPriority     : Variable to hold 
** lnWindPos      : Variable to hold 
** lnDisPerc      : Variable to hold 
**
** llDoLocal      : Variable to hold 
** llBrowse       : Variable to hold 
** llAddMode      : Variable to hold 
*B803677,1 SSE Commented out [Begin]
*++NAd
*laDefProc[10] = .F.
*+++nad
*B803677,1 SSE Commented out [End]
STORE ''    TO laFiscYear, lcOldYear , laPayMethd, laHistory , laFactorAd ,;
               lcWinVenC0, lcWinVenC1, lcWinVenC2, lcWinVenC3, lcWinVenC4 ,;
               lcWinVenC5, lcWinShow1, lcWinShow2, lcWinShow3, lcWinShow4 ,;
               lcWinshow5, lcDefaAddr, pu1099type, puTermCode, puDivision ,;
               puAddress , lcFiltExp , lcModal   , lcYearName, lcHistory  ,;
               lcCodeFilt, lcDivision, lcWindow  , lcExisWind, lcExAcDes  ,;
               lcNextWin , lcApAcDes , lc1099Type, lcTermCode, lcBankDes  ,;
               lcChecDes , lcPayMethd, lcOldAcct , lcOldDesc , lgOldValue ,;
               lcGroup1  , lcGroup2  , lcGroup3  , lcPercStat, lc_Title   ,;
               lcAddHed1 , lcAddHed2 , lcAddHed3 , lcAddHed4 , lcAddHed5  ,;
               lcAddHed6 , lcFisYear , lcCusScheme, lcChkStat, lcOldCurr

*B605547,1 ALB Initialize lcWarHStat [Begin]
STORE ''    TO lcWarHStat
*B605547,1 ALB Initialize lcWarHStat [End]
STORE 0.000 TO lnDisPerc

STORE 0     TO lnPrintChe, lnManulChe, lnOfferd  , lnTaken   , lnNonChecks,; 
               lnCredCard, lnApplDeb , lnApplAdj , lnTotPurc , lnCashPay  ,;
               lnTotPay  , lnAvgPurc , lnAvgPay  , lnCrAvail , laArltFlDat,;
               lnDueDays , lnDiscDays, lnPriority, lnWindPos

*E300824,1 AMM start, Add three variable declaration llApGlLink,llApS1099, llReturnVal 
*E300824,1 llApGlLink Variable indicates if there is a link between AP and GL or not.
*E300824,1 llReturnVal Variable Indicate if I opened lcLinkChar myself or it is already opened.
*STORE .F.   TO llBrowse  , llAddMode , laDefProc[7] , laDefProc [9]      
STORE .F.   TO llBrowse  , llAddMode , laDefProc[7] , laDefProc [9] , llApGlLink,llApS1099 , llReturnVal   
*E300824,1 Variable to indicate if some tables are opened by this program or not.

*B801867,1 AMM Remove unneeded variables.
*STORE .F. TO llsetup, llBanks, llChecks, llInvHdr, llInvHst, llDist, llInvAhd, llPaymnt  
STORE .F. TO llsetup, llInvHdr, llDist, llPaymnt  
*B801867,1 AMM end

*E300824,1 end
*E300798,1 AMM Initialize, this variable determine the check box status [cbVenInvT]
lcVITStat = 'DISABLE'
*E300798,1 AMM end
STORE .T.   TO llDoLocal
*B801867,1 AMM Initialize necessary arrays for code POPUP's
DIMENSION laDiv[1], laCodInfo[3,10], laTermCode[1],la1099T[1],laAddHed[1],;
          laOldAdd[1,1]
STORE SPACE(0) TO laDiv, laCodInfo, laTermCode,la1099T,laAddHed,laOldAdd
*B801867,1 AMM Fill code POPUP's array
laCodInfo[1,01] = "CDIVISION"               && Field Name
laCodInfo[1,02] = "laDiv"                   && Array Name
laCodInfo[1,03] = "lnDiv"                   && Popup Name
laCodInfo[1,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
laCodInfo[1,05] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[1,06] = .T.                       && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[1,07] = "APVENDOR"                && Alternative File (For default val.)
laCodInfo[1,08] = "VENCODE"                 && Use this index for the Alternative file.
laCodInfo[1,09] = "laData[1]"               && Seek this expretion.
laCodInfo[1,10] = "cDivision"               && Alternative Field Name

laCodInfo[2,01] = "CTERMCODE"               && Field Name
laCodInfo[2,02] = "laTermCode"              && Array Name
laCodInfo[2,03] = "lnTermCode"              && Popup Name
laCodInfo[2,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
laCodInfo[2,05] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[2,06] = .T.                       && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[2,07] = "APVENDOR"                && Alternative File (For default val.)
laCodInfo[2,08] = "VENCODE"                 && Use this index for the Alternative file.
laCodInfo[2,09] = "laData[1]"               && Seek this expretion.
laCodInfo[2,10] = "cTermCode"               && Alternative Field Name

laCodInfo[3,01] = "CVEN1099T"               && Field Name
laCodInfo[3,02] = "la1099T"                 && Array Name
laCodInfo[3,03] = "ln1099T"                 && Popup Name
laCodInfo[3,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
laCodInfo[3,05] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[3,06] = .T.                       && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[3,07] = "APVENDOR"                && Alternative File (For default val.)
laCodInfo[3,08] = "VENCODE"                 && Use this index for the Alternative file.
laCodInfo[3,09] = "laData[1]"               && Seek this expretion.
laCodInfo[3,10] = "cven1099t"               && Alternative Field Name
*B801867,1 AMM end
*E301436,1 (Start) temp name for the contacts temp file 
lcContFile  = gfTempName()
*E301436,1  (End)
*B606434,1 ALB Fix the contact screen [Begin]
STOR '' to lcContRole,lcWareHouse,lcOldValue
lcContRole = gfTempName()
*B606434,1 ALB Fix the contact screen [end]

IF ! gfSetup()
  RETURN
ENDIF

*C102740,1 ALB Attach warehouse to the vendor [Begin]
STOR '' to lcWareHouse,lcOldValue
=gfOpenFile(gcDataDir+'warehous' ,'warehous','SH') 
lcScFields = lcScFields + ',cwarecode'
*C102740,1 ALB Attach warehouse to the vendor [end]

*E300824,1 AMM variable to hold if the AP installed or not.
llAPINST = 'AP' $ gcCmpModules
*E300824,1 AMM Open the files related to the AP in case of AP installed.
IF llAPINST
  llsetup  =gfOpenFile(gcDataDir+'ApSetup'  ,'','SH')
  *E301077,71 AMM This part moved from below .
  llApS1099=lApS1099
  llApGlLink = (CAPSGLLINK = 'Y')
  *E301077,71 AMM end
  =gfOpenFile(gcDataDir+'ApBanks'  ,'BANKCODE','SH') 
  =gfOpenFile(gcDataDir+'ApChecks' ,'BANKCHECK','SH') 
  *B801867,1 AMM Open them when needed
  *llInvHst =gfOpenFile(gcDataDir+'ApInvHst' ,'VENDINV','SH') 
  *llDist   =gfOpenFile(gcDataDir+'ApDist'   ,'PAYMNTS','SH') 
  *llInvAhd =gfOpenFile(gcDataDir+'ApInvAhd' ,'HTYPCOD','SH')
  *llPaymnt =gfOpenFile(gcDataDir+'ApPaymnt' ,'TYPCLNO','SH') 
  *B801867,1 AMM end
  
  llInvHdr =gfOpenFile(gcDataDir+'ApInvHdr' ,'INVVEND','SH') 
  
  *E300824,1 AMM If GL installed and the file not opened, Open it.
  IF 'GL' $ gcCmpModules AND !USED('lcLinkChar')
    llFunCall = .T.
    DO (gcAppHome+"AP.APP") WITH "lfOpenCh", .T. , .T.
    
    *B038431,1 NNA    restore lcBasewind value after calling AP program[Start]
    lcBaseWind = gcBaseWind
    *B038431,1 NNA    [Start]

  ENDIF
ENDIF
*E300824,1 AMM end

*E301077,71 AMM These files is Opened with program files, Open them here 
*E301077,71 AMM and don't depend on the program files
=gfOpenFile(gcDataDir+'APVENDOR' ,'VENCODE','SH')
=gfOpenFile(gcDataDir+'APVENHST' ,'VENDYEAR','SH')
=gfOpenFile(gcSysHome+'SYCINT'   ,'CCONTCODE','SH')
*E301077,71 AMM end

*E300824,1 AMM start, When this program called from any module but not AP,
*E300824,1 AMM the following variable was not defined (lcEmptyAcc,lcBtmpExt,
*E300824,1 AMM lcBtmpCls,lnApsAcLen,ApglLink,llAps1099) SO, define it.

*E300824,1 AMM Define the variables lcBtMpExt , lcBtMpCls 
lcBtMpExt = gcBMPHome + "EXTKEY.BMP"
lcBtMpCls = gcBMPHome + "CLS.BMP"

*E300824,1 AMM Define the variables lnApsAcLen , lcEmptyAcc 
llUsd_ACOD = .F.
llU_APSET  = .F.

*E301077,71 AMM use gfSysOpen() to open SYCCOMP
*SELECT SYCCOMP
*SET ORDER TO CCOMP_ID
llUsd_SCmp = gfSysOpen(gcSysHome+"SYCCOMP",'CCOMP_ID','SH')
*E301077,71 end

*E300824,1 AMM Get the company information for gcAct_Comp from the company 
*E300824,1 AMM files, instead of the global variables becausethe global variables 
*E300824,1 AMM company, if the AP.PRG are not changed so as to suit a selected
*E300824,1 AMM is called from SM.PRG, company information screen.
SEEK(gcAct_Comp)
IF !EMPTY(SYCCOMP.cCompPrnt)
  lcCompPrnt = SYCCOMP.cCompPrnt
  =SEEK(lcCompPrnt,"SYCCOMP")
ELSE
  lcCompPrnt = SYCCOMP.cComp_ID
ENDIF  

*E301098,1 Hesham (Start)
*lcPrntDDir =  ALLTRIM(SYCCOMP.cCom_DDir)
lcPrntDDir =  gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir))
*E301098,1 Hesham (End)

*E301077,71 AMM use gfopenfile() instead of USE command.
*IF !USED("ACCOD")
  *E300824,1 AMM Checking to close files opened by the prg.
  *llUsd_ACOD = .T. 
  *SELECT 0
  *USE (lcPrntDDir+"ACCOD") ORDER TAG ACCSEGNO
*ELSE
  *SELECT ACCOD
  *SET ORDER TO TAG ACCSEGNO
*ENDIF  
*=SEEK(lcCompPrnt) 
llUsd_ACOD = gfSysOpen(lcPrntDDir+"ACCOD",'ACCSEGNO','SH')
GO TOP
*E301077,71 AMM end

lcApsAcMas = ACCOD.cAcsMask
*lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(llApInst .AND. APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))
lcEmptyAcc = REPLICATE('0',lnApsAcLen)

*E300824,1 AMM Define the variables llApS1099, llApGlLink 
*E301077,71 AMM Move this part of code above.
*IF llAPINST
  *IF !USED("APSETUP") 
    *llU_APSET = .T.
    *SELECT 0
    *USE (gcDataDir+"APSETUP")
  *ELSE 
    *SELECT APSETUP
  *ENDIF
  *SELECT APSETUP
  *llApS1099=lApS1099
  *** This line is very important to refresh the station buffer with new 
  *** information from the disk
  *GO TOP
  *IF !EOF()
    *GO 1
    *llApGlLink = (CAPSGLLINK = 'Y')
  *ENDIF  
  *IF USED('APSETUP') .AND. llU_APSET
  *  USE IN APSETUP
  *ENDIF
*ENDIF
*E301077,71 AMM end
*E300824,1 AMM Close the files if opened by me.
*E301077,71 AMM use gfclosefile instead of USE IN command
*IF USED('ACCOD') .AND. llUsd_ACOD
  *USE IN ACCOD
*ENDIF
IF llUsd_ACOD
  =gfSysClose('ACCOD')
ENDIF
*E301077,71 AMM end
llFromInvo = IIF(TYPE('lcVenCode') $ 'UL',.F.,.T.)
SELECT APVENDOR
SET ORDER TO TAG VENCODE

IF llFromInvo
  laScrMode       = .F.
  *** B600259 Line was changed by YI-Yasser in 4-6-95
  lcModal         = 'MODAL WITH (GCBASEWIND)'
  IF SEEK(lcVenCode)
    laScrMode[2]  = .T.
    laCtrStat     = 'DISABLE'
    laCtrStat[12] = 'ENABLE'  
  ELSE
    laScrMode[4]  = .T.
    laCtrStat[10] = 'DISABLE'
    laCtrStat[11] = 'ENABLE'  
  ENDIF  
ENDIF

lcColPair  = IIF(laScrMode[1] .OR. laScrMode[2] ,lcDisCont,lcEnbCont)
lcColor    = IIF(laScrMode[1]                   ,lcDisCont,lcEnbCont)

IF WEXIST(gcBaseWind)
  lcInvStat1 = IIF(!laScrMode[1] .AND. lnWindPos = 1,"ENABLE","DISABLE")
  lcInvStat2 = IIF(!laScrMode[1] .AND. lnWindPos = 2,"ENABLE","DISABLE")
  lcInvStat3 = IIF(!laScrMode[1] .AND. lnWindPos = 3,"ENABLE","DISABLE")
  lcInvStat4 = IIF(!laScrMode[1] .AND. lnWindPos = 4,"ENABLE","DISABLE")
  lcInvStat5 = IIF(!laScrMode[1] .AND. lnWindPos = 5,"ENABLE","DISABLE")

  lcWinStat1 = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnWindPos = 1,"ENABLE","DISABLE")
  lcWinStat2 = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnWindPos = 2,"ENABLE","DISABLE")
  lcWinStat3 = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnWindPos = 3,"ENABLE","DISABLE")
  lcWinStat4 = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnWindPos = 4,"ENABLE","DISABLE")
  lcWinStat5 = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnWindPos = 5,"ENABLE","DISABLE")
  lcAltKeySt = IIF(laScrMode[2] ,"DISABLE","ENABLE")  
  rbTaxType  = IIF(laData[40]='L',1,2)
  lcPercStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. laData[40]='T','ENABLE','DISABLE')
  lcChkStat  = IIF(lcWinStat2 = "ENABLE" .AND. !EMPTY(laData[34]), "ENABLE", "DISABLE")

  *B605547,1 ALB Initialize lcWarHStat according to the screen mode [Begin]
  lcWarHStat = IIF(laScrMode [3] AND 'C' $ laData[50],"ENABLE", "DISABLE")
  *B605547,1 ALB Initialize lcWarHStat according to the screen mode [end]
  *B801867,1 AMM Restore values
  IF !EMPTY(laData[12])
    lnDiv      = ASUBSCRIPT(laDiv,ASCAN(laDiv,laData[12]),1)
  ENDIF
  IF !EMPTY(laData[35])
    lnTermCode = ASUBSCRIPT(laTermCode,ASCAN(laTermCode,laData[35]),1)
  ENDIF
  IF !EMPTY(laData[42])
    ln1099T    = ASUBSCRIPT(la1099T,ASCAN(la1099T,laData[42]),1)
  ENDIF
  *B801867,1 AMM end
          
ELSE
  lc_Title   = 'Vendor history' 
  STORE lcEmptyAcc TO laData[32], laData[33], laData[47]
  lcBlankTxt = IIF(llFromInvo AND SEEK(lcVenCode,'APVENDOR'),'','BLANK')
  SCATTER FIELDS &lcScFields MEMO TO laData &lcBlankTxt
  =gfGetVld('CVENPMETH',@laPayMethd)

  laTermAry[1,1] = 'NTERDUED'
  laTermAry[1,2] = 'lnDueDays'  
  laTermAry[2,1] = 'NTERDISCD'
  laTermAry[2,2] = 'lnDiscDays'  
  laTermAry[3,1] = 'NTERDISCR'
  laTermAry[3,2] = 'lnDisPerc'  

  *** Prepare an array to hold bank objects to be used for
  *** global bank and checking accounts validations as follows :
  *** One row for every object, such that 
  *** row no. 1 holds bank object names,
  *** row no. 2 holds checking account object names
  *** row no. 3 holds the corresponding G/L account object names,(optional)
  *** Columns are ordered as follows :
  *** Column no. 1 : invisible button name for corresponding object
  *** Column no. 2 : object name (e.g. bank object name)
  *** Column no. 3 : object description name(if required)
  laBankObjs  = ' '
  laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
  laBankObjs[1,2] = 'laData[34]'     && Bank code 
  laBankObjs[1,3] = 'lcBankDes'      && Bank code short description
  laBankObjs[2,1] = 'ibCheck'        && Checking account invisible button
  laBankObjs[2,2] = 'laData[36]'     && Checking account 
  laBankObjs[2,3] = 'lcChecDes'      && Checking account short description  

  lcWinVenC0  = gfTempName()
  lcWinVenC1  = gfTempName()
  lcWinVenC2  = gfTempName()
  lcWinVenC3  = gfTempName()
  lcWinVenC4  = gfTempName()
  lcWinVenC5  = gfTempName()

  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES [Begin]
  *SELECT SYCCODES
  *B801867,1 AMM Comment out
  *SELECT CODES
  *B801867,1 AMM end
  *E300643,1 Change this line [End]
  
  lcValComp  = ALLTRIM(IIF(EMPTY(gcPrnt_Cmp),gcAct_Comp,gcPrnt_Cmp))
  lcDefaAddr = ALLTRIM(LOOKUP(SYCCOMP.CCONT_CODE,lcValComp,SYCCOMP.CCOMP_ID,"CCOMP_ID"))
  
  *E300824,1 AMM Check if AP installed before getting values from APSETUP.
  *lcGroup1   = IIF(EMPTY(apSetup.cApSvGr1D),'Group 1',apSetup.cApSvGr1D)
  *lcGroup2   = IIF(EMPTY(apSetup.cApSvGr2D),'Group 2',apSetup.cApSvGr2D)
  *lcGroup3   = IIF(EMPTY(apSetup.cApSvGr3D),'Group 3',apSetup.cApSvGr3D)
  lcGroup1   = IIF(llApInst .AND. !EMPTY(apSetup.cApSvGr1D),apSetup.cApSvGr1D,'Group 1')
  lcGroup2   = IIF(llApInst .AND. !EMPTY(apSetup.cApSvGr2D), apSetup.cApSvGr2D, 'Group 2')
  lcGroup3   = IIF(llApInst .AND. !EMPTY(apSetup.cApSvGr3D),apSetup.cApSvGr3D,'Group 3')
  *E300824,1 AMM end


  *E300692,5 Use FISHD instead of SYCFISHD
  *SELECT cFisfYear,cFisYStat,cFisNoPrd  ;
  FROM (gcSYSHOME+'SYCFISHD')  ;
  WHERE cComp_ID = gcPrnt_Cmp  ;
  INTO ARRAY laFiscYear
  
  *E301098,1 Hesham (Start)
  *lcDataDir = ALLTRIM(IIF(SEEK(gcPrnt_Cmp, 'SYCCOMP'), SYCCOMP.cCom_DDir, gcDataDir))
  lcDataDir = ALLTRIM(IIF(SEEK(gcPrnt_Cmp, 'SYCCOMP'),gfGetDataDir(ALLT(SYCCOMP.cCom_DDir)), gcDataDir))
  *E301098,1 Hesham (end)
  *E301077,71 AMM Open the file here, it's not opened with program files 
  *E301077,71 AMM any more.
  llOpFisHD = gfSysOpen(lcDataDir+'FISHD','','SH')
  *E301077,71 AMM end
  SELECT cFisfYear,cFisYStat,cFisNoPrd  ;
  FROM (lcDataDir + 'FISHD')  ;
  INTO ARRAY laFiscYear
  *E300692,5 end
  *E301077,71 AMM Close the file, It's not used any more
  IF llOpFisHD
    =gfSysClose('FISHD')
  ENDIF
  *E301077,71 AMM end
  FOR lnCounter = 1 TO ALEN(laFiscYear,1) 
    DO CASE 
      CASE laFiscYear[lnCounter,2] = 'P'
        laFiscYear[lnCounter,2] = 'Previous' 
      CASE laFiscYear[lnCounter,2] = 'C'
        laFiscYear[lnCounter,2] = 'Current' 
      CASE laFiscYear[lnCounter,2] = 'N'
        laFiscYear[lnCounter,2] = 'Next' 
      CASE laFiscYear[lnCounter,2] = 'H'
        laFiscYear[lnCounter,2] = 'History'         
    ENDCASE   
  ENDFOR

  laWndObj [1,1] = lcWinVenC0
  laWndObj [1,2] = "laData[1]"
  laWndObj [1,3] = "pbNotes"

  laWndObj [2,1] = "GWCCONTRL1"
  laWndObj [2,2] = "PBTOP"
  laWndObj [2,3] = "PBCLS"

  laWndObj [3,1] = lcWinVenC1
  laWndObj [3,2] = "laData[1]"
  laWndObj [3,3] = IIF(_DOS,"pbNextWin1",'LADATA[33]')
 
  laWndObj [4,1] = lcWinVenC2
  laWndObj [4,2] = "laData[1]"
  laWndObj [4,3] = IIF(_DOS,"pbNextWin2",'LADATA[47]')

  laWndObj [5,1] = lcWinVenC3
  laWndObj [5,2] = "laData[1]"
  laWndObj [5,3] = IIF(_DOS,"pbNextWin3",'ib1099type')

  laWndObj [6,1] = lcWinVenC4
  laWndObj [6,2] = "laData[1]"
  laWndObj [6,3] = IIF(_DOS,"pbNextWin4",'laData[43]')

  laWndObj [7,1] = lcWinVenC5
  laWndObj [7,2] = "laData[1]"
  laWndObj [7,3] = IIF(_DOS,"pbNextWin5",'ibWindow51')

  laWndObj [8,1] = "CWDAPVENHD"
  laWndObj [8,2] = IIF(_DOS,"ibFiscYear","lcFisYear")
  laWndObj [8,3] = "PBCLOSE"

  lcExisWind = lcWinVenC1
  lcNextWin  = 'Payments'
  lcAltKeySt = "ENABLE"
  lcInvStat1 = IIF(llFromInvo,"ENABLE","DISABLE")
  lcWinStat1 = IIF(llFromInvo,IIF(SEEK(lcVenCode,'APVENDOR'),"DISABLE","ENABLE"),"DISABLE")

  STORE "DISABLE" TO lcInvStat2 , lcInvStat3 , lcInvStat4 , lcInvStat5 ,; 
                     lcWinStat2 , lcWinStat3 , lcWinStat4 , lcWinStat5 ,;
                     lcPercStat , lcChkStat  
  *B605547,1 ALB Initialize lcWarHStat [Begin]
  STORE "DISABLE" TO lcWarHStat
  *B605547,1 ALB Initialize lcWarHStat [end]
ENDIF

*E301077,71 Close SYCCOMP if used by the program
IF llUsd_SCmp
  =gfSysClose("SYCCOMP")
ENDIF  
*E301077,71 AMM end

*E300798,1 AMM define status of the two check boxes.
lcVITStat = IIF(EMPTY(laData[50]),'DISABLE','ENABLE')
cbVenSup  = !EMPTY(laData[50])
cbVenInvT = !EMPTY(laData[51])
DIMENSION laVenSupT[1,2],laVenInvT[1,2]
=gfGetVld('cVenSupTyp',@laVenSupT)
=gfGetVld('cVenInvTyp',@laVenInvT)
*E300798,1 AMM end

IF _WINDOWS

  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES [Begin]
  *DEFINE POPUP pu1099type prompt field SYCCODES.cdiscrep scroll;
  *FROM 2.80,13.45 TO 7.10,46.65;
  *MESSAGE gfObj_msg()
  
  *B801867,1 AMM Comment out, no need
  *DEFINE POPUP pu1099type prompt field CODES.cdiscrep scroll;
  FROM 2.80,13.45 TO 7.10,46.65;
  MESSAGE gfObj_msg()
  *E300643,1 Change this line [End]
  *ON SELECTION POPUP pu1099type DO lfv1099Type
  *B801867,1 AMM end
  
  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES [Begin]
  *DEFINE POPUP puTermCode prompt field SYCCODES.cdiscrep scroll;
  *FROM 3.60,35.00 TO 7.90,67.55;
  *MESSAGE gfObj_msg()
  
  *B801867,1 AMM Comment out, no need
  *DEFINE POPUP puTermCode prompt field CODES.cdiscrep scroll;
  FROM 3.60,35.00 TO 7.90,67.55;
  MESSAGE gfObj_msg()
  *E300643,1 Change this line [End]
  *ON SELECTION POPUP puTermCode DO lfvTermCode
  *B801867,1 AMM end
  
  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES [Begin]
  *DEFINE POPUP puDivision prompt field SYCCODES.cdiscrep scroll;
  *FROM 5.55,38.00 TO 9.85,69.91;
  *MESSAGE gfObj_msg()
  *B801867,1 AMM Comment out, no need
  *DEFINE POPUP puDivision prompt field CODES.cdiscrep scroll;
  FROM 5.55,38.00 TO 9.85,69.91;
  MESSAGE gfObj_msg()
  *E300643,1 Change this line [End]
  *ON SELECTION POPUP puDivision DO lfvDivision
  *B801867,1 AMM end
  *B801867,1 AMM the way of displaying Change Country POPUP
  *lcAddHed1 = SYCINT.CCONT_DESC
  *DEFINE POPUP puAddress prompt field SYCINT.CCONT_DESC scroll;
  FROM 5.95,1.00 TO 10.25,30.0;
  MESSAGE gfObj_msg()
  *ON SELECTION POPUP puAddress DO lfvAdFormat
  *B801867,1 AMM Fill the Country's arrays
  SELECT '*'+cCont_Desc,cCont_Code FROM SYCINT INTO ARRAY laOldAdd
  SELECT cCont_Desc FROM SYCINT INTO ARRAY laAddHed
  IF ASCAN(laOldAdd,laData[44])-1 > 0
    lcAddHed = SUBSTR(laOldAdd[ASCAN(laOldAdd,laData[44])-1],2)
  ENDIF
  *B801867,1 AMM end
  
ENDIF  

lcWinShow1  = IIF(lcExisWind = lcWinVenC1 ,'','NOSHOW')
lcWinShow2  = IIF(lcExisWind = lcWinVenC2 ,'','NOSHOW')
lcWinShow3  = IIF(lcExisWind = lcWinVenC3 ,'','NOSHOW')
lcWinShow4  = IIF(lcExisWind = lcWinVenC4 ,'','NOSHOW')
lcWinShow5  = IIF(lcExisWind = lcWinVenC5 ,'','NOSHOW')
lcCusScheme = IIF(_DOS, ',,,,,,,,,' + lcHidObjNrm,',,,,,,,,,'+lcBackGrnd)

SET ORDER TO TAG VENDYEAR   IN APVENHST
*E300824,1 AMM Set order to these files in case of AP installed
*SET ORDER TO TAG PAYMNTS    IN APDIST
*SET ORDER TO TAG INVVEND    IN APINVHDR
*SET ORDER TO TAG VENDINV    IN APINVHST
*E301077,71 AMM Move this part above.
*IF llApInst
  *SET ORDER TO TAG PAYMNTS    IN APDIST
  *SET ORDER TO TAG INVVEND    IN APINVHDR
  *SET ORDER TO TAG VENDINV    IN APINVHST
  *SET ORDER TO TAG BANKCODE   IN APBANKS
  *SET ORDER TO TAG BANKCHECK  IN APCHECKS
*ENDIF
*E301077,71 AMM end
*E300824,1 AMM end

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SET ORDER TO TAG IDRLTFNAME IN SYCCODES
*B801867,1 AMM Comment out, not used
*SET ORDER TO TAG IDRLTFNAME IN CODES
*B801867,1 AMM end

*E300643,1 Change this line for the changes we have made to SYCCODES [End]

*E300824,1 AMM These settings are done in the paragraph above 
*SET ORDER TO TAG BANKCODE   IN APBANKS
*SET ORDER TO TAG BANKCHECK  IN APCHECKS
*E300824,1 AMM end

*E301077,71 AMM This file is not opened until now, it is opened in the valid 
*E301077,71 AMM function of the factor. 
*SET ORDER TO TAG CFACCODE   IN SYCFACT
*E301077,71 AMM end
*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
*B801867,1 AMM Comment out , not used
*SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]
*SET FILTER TO (CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp+'N'+lcCodeFilt) OR cFld_Name ='N/A'
*B801867,1 AMM end

SELECT APVENDOR 

*E300683,1 Call *.SPR from screens directory
*DO APVendr.SPR
DO (gcScrDir +  '\APVendr.SPR')
*E300683,1 end

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
*B801867,1 AMM Comment out , not used
*SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]
*SET FILTER TO 
*B801867,1 AMM end

RELEASE POPUPS _INQURYPOP
RELEASE PAD _INQUIRY OF _MSYSMENU

*B801867,1 AMM Don't release them cos they are not created
*IF _WINDOWS
  *RELEASE POPUPS pu1099type,puTermCode,puDivision,puAddress
*ENDIF 
RELEASE POPUPS puAddress
*B801867,1 AMM end

IF glQuitting AND WEXIST('CWDAPVENHD')
  RELEASE WINDOW CWDAPVENHD
ENDIF

*E300824,1 CLose tables if opened by this program.
IF USED('lcLinkChar') .AND. llReturnVal  
  USE IN lcLinkChar
ENDIF
IF USED('ApSetup') .AND. llsetup  
  USE IN ApSetup
ENDIF
IF USED('ApDist') .AND. llDist
  USE IN ApDist
ENDIF
IF USED('ApInvHdr') .AND. llInvHdr 
  USE IN ApInvHdr
ENDIF
IF USED('ApPaymnt') .AND. llPaymnt
  USE IN ApPaymnt
ENDIF

*B801867,1 AMM Comment out, they are already closed
*IF USED('ApBanks') .AND. llBanks  
  *USE IN ApBanks
*ENDIF
*IF USED('ApChecks') .AND. llChecks 
  *USE IN ApChecks
*ENDIF
*IF USED('ApInvHst') .AND. llInvHst 
  *USE IN ApInvHst
*ENDIF
*IF USED('ApInvAhd') .AND. llInvAhd 
  *USE IN ApInvAhd
*ENDIF
*B801867,1 AMM end
*E300824,1 AMM end
*E301436,1 (Start) Close and Erase the temp file
IF USED  (lcContFile)
  USE IN (lcContFile)
  ERASE (gcWorkDir+lcContFile+'.DBF')
  ERASE (gcWorkDir+lcContFile+'.CDX')
ENDIF  
*E301436,1 (End)

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

llDoLocal  = .F.

IF llFromInvo 
  IF laScrMode[1]
    glQuitting = .T.
    CLEAR READ
    RETURN .F.
  ELSE  
    laData[1] = lcVenCode
    lnPosition = ASUBSCRIPT(laFiscYear,ASCAN(laFiscYear,'Current'),1)
    lcFisYear  = laFiscYear[lnPosition,1]
    lcYearName = laFiscYear[lnPosition,2]
  ENDIF
ENDIF  

SELECT APVENDOR


DO CASE 
  CASE laScrMode [1]           && Select Mode
  
  *++nad
  =lfDelTmp()
  *++nad
  
    =lfGetAddress(lcDefaAddr)

    STORE '' TO lcExAcDes , lcApAcDes  , lcPayMethd , lcBankDes , lcChecDes,;
                lcHistory  

    STORE 0  TO lnPrintChe, lnManulChe , lnNonChecks, lnCredCard, lnOfferd ,;
                lnTaken   , lnApplDeb  , lnApplAdj  , lnTotPurc , lnCashPay,;
                lnTotPay  , lnAvgPurc  , lnAvgPay   , lnCrAvail , lnDueDays,;
                lnDiscDays, laArltFlDat  

    STORE lcEmptyAcc TO laData[32], laData[33], laData[47]
    *E300798,1 AMM Initialize check box values in select mode.
    lcVITStat = 'DISABLE'
    STORE .F. TO cbVenSup, cbVenInvT
    SHOW GET cbVenInvT DISABLE
    *E300798,1 AMM end
    lnDisPerc  = 0.000
    lnPriority = 1  
    lnPosition = ASUBSCRIPT(laFiscYear,ASCAN(laFiscYear,'Current'),1)
    lcFisYear  = laFiscYear[lnPosition,1]
    lcYearName = laFiscYear[lnPosition,2]
    *E300824,1 If AP is not installed, default the vendor's tax type .
    *E300824,1 with 'L'ine item tax
    *laData[40] = APSETUP.cTaxType
    laData[40] = IIF(llApInst,APSETUP.cTaxType,'L')
    *E300824,1 end    
    llBrowse   = .F.

    *B801867,1 AMM Display the POPUP's with 'N/A' value
    = gfwCodePop(@laCodInfo , "CDIVISION" , "N")
    lnDiv = 1
    SHOW GET lnDiv
    = gfwCodePop(@laCodInfo , "CTERMCODE" , "N")
    lnTermCode = 1
    SHOW GET lnTermCode
    IF !llApS1099
      = gfwCodePop(@laCodInfo , "CVEN1099T" , "N")
      ln1099T = 1
      SHOW GET ln1099T
    ENDIF
    *B801867,1 AMM end
    *E301436,1 (Start) Disable th contact push buttom  in the select mode
    SHOW GET pbCont  DISABLE
    *E301436,1 (End)
    =lfvData_43(.F.)     && refresh factor information

    
    IF WVISIBLE('CWDAPVENHD')
      =gfChClose('CWDAPVENHD')
    ENDIF  


  CASE laScrMode[2] .OR. laScrMode[3] 
    
    =lfGetAddress(laData[44])   
    =lfDispAcsDes()           && Display the account discription.
    
    lnPriority = INT(VAL(laData[38]))
    lnWindPos  = ASUBSCRIPT(laWindName,ASCAN(laWindName,lcExisWind),1)
    lcNextBut  = 'pbNextWin' + STR(lnWindPos,1)
    *E300824,1 AMM Check if AP installed before getting values from APBANKS,APCHECKS
    *lcBankDes  = IIF(SEEK(laData[34],'APBANKS'),APBANKS.CBNKSHDES,'')
    *lcChecDes  = IIF(SEEK(laData[34]+laData[36],'APCHECKS'),APCHECKS.CCHKSHDES,'')
    IF llApInst
      lcBankDes  = IIF(SEEK(laData[34],'APBANKS'),APBANKS.CBNKSHDES,'')
      lcChecDes  = IIF(SEEK(laData[34]+laData[36],'APCHECKS'),APCHECKS.CCHKSHDES,'')
    ELSE
      STORE '' TO lcBankDes, lcChecDes
    ENDIF  
    *E300824,1 AMM end
    llBrowse   = .F.
    *E300798,1 AMM define status of the two check boxes.
    lcVITStat = IIF(EMPTY(laData[50]),'DISABLE','ENABLE')
    cbVenSup  = !EMPTY(laData[50])
    cbVenInvT = !EMPTY(laData[51])
    SHOW GET cbVenInvT &lcVITStat
    *E300798,1 AMM end
    =lfvData_43(.F.)         && refresh factor information
    *B801867,1 AMM Fill codes popups with its current values.
    = gfwCodePop(@laCodInfo, "CDIVISION", "T")
    SHOW GET lnDiv
    = gfwCodePop(@laCodInfo, "CTERMCODE", "T")
    SHOW GET lnTermCode
    IF !llApS1099
      = gfwCodePop(@laCodInfo, "CVEN1099T", "T")
      SHOW GET ln1099T
    ENDIF
    *B801867,1 AMM end

    SHOW GET (lcNextBut) ENABLE
    SHOW GET ibFiscYear  ENABLE
    SHOW GET pbClose     ENABLE
    SHOW GET lcHistory   ENABLE
    *E301436,1 (Start) Enable thE contact push buttom  in the view mode
    SHOW GET pbCont      ENABLE 
    *E301436,1  (End)
    =lfvFiscYear(.F.)

    lcPayMethd = IIF(laData[37] $ 'PMNCH',laPayMethd[AT(laData[37],'PMNCH'),1],'')
    lnCrAvail  = APVENDOR.NVENCRLIM - ROUND(APVENDOR.NVENBAL,0)

    IF laScrMode [2]  
      SHOW GET laData[2]  DISABLE
      SHOW GET laData[3]  DISABLE
    ELSE
      SHOW GET laData[2]  ENABLE
      SHOW GET laData[3]  ENABLE
    ENDIF
    *C102740,1 ALB Attach warehouse to the vendor [Begin]
    IF laScrMode [2]
      SHOW GET laData[53] DISABLE
      SHOW GET pbWareH DISABLE
    ELSE
      SHOW GET laData[53]ENABLE
      SHOW GET pbWareH ENABLE
    ENDIF

    IF laScrMode [3] AND 'C' $ laData[50]
      SHOW GET laData[53] ENABLE
      SHOW GET pbWareH ENABLE
    ELSE
      SHOW GET laData[53] DISABLE
      SHOW GET pbWareH DISABLE
    ENDIF
    STORE IIF('C' $ laData[50],laData[53],'') TO laData[53]
    SHOW GET laData[53]
    *C102740,1 ALB Attach warehouse to the vendor [end]

    *B801867,1 AMM Get the 
    lcAddHed = lcAddHed1
    *B801867,1 AMM end
  
  *E301436,1 (Start)  Restore the old default contact if the user cancel the saving
  
  IF laScrMode[2]
   =lfDelTmp()
  ENDIF
  IF llContChng AND lcOldVend=laData[1]
    laData[17]=lcOldCont
  ENDIF  
  *E301436,1 (End)
  
  CASE laScrMode[4]               && Add Mode
    *C200178,1 AAN Check for the trigger (Cathy Daniel) if found add defa value to the
    * custom field cvenpytype[Begin]
    IF ASCAN(laEvntTrig , PADR('APVENTYPE',10)) <> 0
      =lfvUsrFld()
    ENDIF
    *C200178,1 AAN [End]
    
    =lfGetAddress(lcDefaAddr)     && In case coming from payable invoice in add mode
    laData[2]  = IIF(EMPTY(laData[2]),SPACE(16),laData[2])
    laData[3]  = IIF(EMPTY(laData[3]),SPACE(30),laData[3])
    laData[37] = laPayMethd[1,2] 
    lcPayMethd = laPayMethd[1,1]     
    *E300824,1 If AP is not installed, default the vendor's tax type 
    *E300824,1 with 'L'ine item tax
    *laData[40] = APSETUP.cTaxType
    laData[40] = IIF(llApInst,APSETUP.cTaxType,'L')
    *E300824,1 end    
    *E300798,1 AMM Initialize the two check boxes in add mode.
    lcVITStat = 'DISABLE'
    cbVenSup  = .F.
    cbVenInvT = .F.
    *E300798,1 AMM end
    *B801867,1 AMM If the user chose value, diplay it, else display 'N/A' in the POPUP's
    lcPopMode = IIF(EMPTY(laData[12]),'N',"V,"+laData[12])
    = gfwCodePop(@laCodInfo, "CDIVISION" , lcPopMode)
    laData[12] = laDiv[lnDiv,2]
    SHOW GET lnDiv
    lcPopMode = IIF(EMPTY(laData[35]),'N',"V,"+laData[35])
    = gfwCodePop(@laCodInfo, "CTERMCODE" , lcPopMode)
    laData[35] = laTermCode[lnTermCode,2]
    SHOW GET lnTermCode
    lcPopMode = IIF(EMPTY(laData[42]),'N',"V,"+laData[42])
    IF !llApS1099
      = gfwCodePop(@laCodInfo, "CVEN1099T" , lcPopMode)
      laData[42] = la1099T[ln1099T,2]
      SHOW GET ln1099T
    ENDIF
   
    *B801867,1 AMM end
    
    *E301436,1 (Start) Enable thE contact push buttom  in the EDIT mode
    SHOW GET pbCont      ENABLE
    *E301436,1 (End) 
    
    laData[44] = lcDefaAddr

    STORE lcEmptyAcc TO laData[32], laData[33], laData[47]

    STORE '' TO  lcExAcDes , lcApAcDes , lcBankDes  , lcChecDes , lcFisYear ,;
                 lcYearName, lcHistory 

    STORE 0  TO  lnPrintChe, lnManulChe, lnNonChecks, lnCredCard, lnOfferd  ,;
                 lnTaken   , lnApplDeb , lnApplAdj  , lnTotPurc , lnCashPay ,;
                 lnTotPay  , lnAvgPurc , laArltFlDat, lnCrAvail , lnDueDays ,;
                 lnDiscDays, lnAvgPay  
  
    lnDisPerc  = 0.000
    lnPriority = 1 
    lnWindPos  = ASUBSCRIPT(laWindName,ASCAN(laWindName,lcExisWind),1)
    lcNextBut  = 'pbNextWin' + STR(lnWindPos,1)

    SHOW GET ibInquiry   DISABLE
    SHOW GET laData[2]   ENABLE
    SHOW GET laData[3]   ENABLE
    SHOW GET (lcNextBut) ENABLE

    *C102740,1 ALB Attach warehouse to the vendor [Begin]
    IF 'C' $ laData[50]
      SHOW GET laData[53] ENABLE
      SHOW GET pbWareH ENABLE
    ELSE
      SHOW GET laData[53] DISABLE
      SHOW GET pbWareH DISABLE
    ENDIF
    *C102740,1 ALB Attach warehouse to the vendor [end]

ENDCASE

SELECT APVENDOR
lcButName  = "ibWindow"+STR(lnWindPos,1)
rbTaxType  = IIF(laData[40]='L',1,2)
lcCodeFilt = ''

*E300643,1 Change this lines for the changes we have made 
*          to (gfCodDes) and (gfRltFld) [Begin]
*lcDivision = gfCodDes(laData[12])
*lc1099Type = gfCodDes(laData[42])
*lcTermCode = gfCodDes(laData[35])
*=gfRltFld(laData[35],@laTermAry)
lcDivision = gfCodDes(laData[12] , 'CDIVISION')
lc1099Type = gfCodDes(laData[42] , 'CVEN1099T')
lcTermCode = gfCodDes(laData[35] , 'CTERMCODE')
=gfRltFld(laData[35] , @laTermAry , 'CTERMCODE')
*E300643,1 Change this lines [End]

*B600608,1 stop the showing of the invisable button here and move it to the end of the procedure
*SHOW GET ibEndDumi1 &lcInvStat1
*SHOW GET ibEndDumi2 &lcInvStat2
*SHOW GET ibEndDumi3 &lcInvStat3
*SHOW GET ibEndDumi4 &lcInvStat4
*SHOW GET ibEndDumi5 &lcInvStat5

SHOW GET ibAddrType DISABLE

IF laScrMode[1]
  SHOW GET (lcButName) DISABLE
ELSE
  SHOW GET (lcButName) ENABLE
ENDIF

IF laData[40] = 'T' .AND. (laScrMode[3] .OR. laScrMode[4]) .AND. lcExisWind = laWindName[3,2]
  SHOW GET laData[41] ENABLE 
ELSE
  SHOW GET laData[41] DISABLE 
ENDIF  

IF !EMPTY(laData[34]) .AND. (laScrMode[3] .OR. laScrMode[4]) .AND. lcExisWind = laWindName[2,2]
  SHOW GET ibCheck    ENABLE 
  SHOW GET laData[36] ENABLE 
ELSE
  SHOW GET ibCheck    DISABLE
  SHOW GET laData[36] DISABLE
ENDIF  

*lcChkStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  !EMPTY(laData[34]),;
                "ENABLE", "DISABLE")   
lcColPair= IIF(laScrMode[1] .OR. laScrMode[2] ,lcDisCont,lcEnbCont)
lcColor  = IIF(laScrMode[1]                   ,lcDisCont,lcEnbCont)

IF laScrMode[1] .OR. laScrMode[4]
  SET ORDER TO TAG VENCODE
  SET SKIP OF PAD _INQUIRY OF _MSYSMENU .F.
ELSE
  SET SKIP OF PAD _INQUIRY OF _MSYSMENU .T.
ENDIF  

*B600608,1 setting all invisable buttons to disable
STORE "DISABLE" TO lcInvStat1,lcInvStat2,lcInvStat3,;
                   lcInvStat4,lcInvStat5



FOR lnWNomber = 1 TO 5
  IF lnWNomber <> lnWindPos
    lcWindNam = 'lcWinVenC'+STR(lnWNomber,1)
    SHOW GETS WINDOW (&lcWindNam) DISABLE ONLY 
  ELSE
    *B600608,1 enabling only the invisable button of the active child screen
    lcWnStt  = "lcInvStat"+ALLTRIM(STR(lnWNomber))
    &lcWnStt = "ENABLE"
  ENDIF
ENDFOR
*B600608,1 
IF lnWindPos = 0
  lcInvStat1 = "ENABLE"
ENDIF
*E301436,1 (Start) Always disable the contact field in the vendor screen.
SHOW GET LADATA[17] DISABLE
*E301436,1 (End)

*B600608,1 Changing the status of the child windows endding invisable buttons
SHOW GET ibEndDumi1 &lcInvStat1
SHOW GET ibEndDumi2 &lcInvStat2
SHOW GET ibEndDumi3 &lcInvStat3
SHOW GET ibEndDumi4 &lcInvStat4
SHOW GET ibEndDumi5 &lcInvStat5

*B600590,1 Refresh the menu 
SHOW MENU _MSYSMENU 
ACTIVATE MENU _MSYSMENU NOWAIT


*!**************************************************************************
*!
*!      Function: lfwOldValue
*!
*!**************************************************************************
*
FUNCTION lfwOldValue

lgOldValue = EVALUATE(SYS(18))

*!**************************************************************************
*!
*!      Function: lfwCodeFilt
*!
*!**************************************************************************
*
*B801867,1 AMM Not used any more
FUNCTION lfwCodeFilt
PARAMETERS lcCodeField
PRIVATE  lnCurAlias

*E300824,1 AMM Use SELECT(0) instead to not cause an error.
*lcCurAlias = ALIAS()
lnCurAlias = SELECT(0)
*E300824,1 AMM end

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

lcCodeFilt = lcCodeField
LOCATE
SELECT (lnCurAlias)

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

SELECT APVENDOR
SET ORDER TO TAG VENCODE
laData[1] = PADR(ALLTRIM(laData[1]),8)
IF llBrowse .OR. ATC("?",laData[1]) > 0
  =gfBrows()  
ELSE
  IF !EMPTY(laData[1]) .AND. LASTKEY() = 13 
    IF llAddMode
      IF SEEK(laData[1]) 
        *** MESSAGE : " Vendor code ð already exists."
        ***           " < View >   < Reenter>        "
        lnOption = gfModalGen('QRM04148B04010','Dialog',ALLTRIM(laData[1]))
    
        DO CASE
          CASE lnOption = 1
            SCATTER FIELDS &lcScFields MEMO TO laData
            laScrMode    = .F.
            laScrMode[2] = .T.
            SHOW GETS

          CASE lnOption = 2
            ladata[1] = SPACE(8)
            _CUROBJ   = _CUROBJ
        ENDCASE                    
      ELSE
        laScrMode    = .F.
        laScrMode[4] = .T.
        SHOW GETS
      ENDIF  
    ELSE
      =gfSeekRec()
    ENDIF  
  ELSE
    ladata[1] = SPACE(8)
    laData[2] = SPACE(16)
    laData[3] = SPACE(30)  
    SHOW GET laData[2]  
    SHOW GET laData[3]    
    llAddMode = .F.
  ENDIF
ENDIF

IF laScrMode[4]
  laData[49]   = gcBaseCurr
  SHOW GET laData[49]  
ENDIF

SHOW GET laData[1]
llBrowse  = .F.

*!**************************************************************************
*!
*!      Function: lfvPhonComp
*!
*!**************************************************************************
*
FUNCTION lfvPhonComp

IF !llBrowse .AND. (lgOldValue = EVALUATE(SYS(18)) .OR. EMPTY(EVALUATE(SYS(18))))
  RETURN
ENDIF

SELECT APVENDOR
lnOldRecNo = RECNO()
lcExactSet = SET("EXACT")
lcObjValue = ALLTRIM(UPPER(EVALUATE(SYS(18))))
lcObjName  = SYS(18)
lcOldData1 = laData[1]
lcMessage  =     IIF(lcObjName='LADATA(2)','Phone No.','Company')
SET ORDER TO TAG IIF(lcObjName='LADATA(2)','VENPHONE' ,'VENCOMP')
lcIndxExp  = SYS(14,VAL(SYS(21)))
SET EXACT ON

IF laScrMode[3] .OR. laScrMode[4]
  LOCATE FOR EVALUATE(lcIndxExp) = lcObjValue .AND. laData[1] <> apVendor.cVendCode
  IF FOUND()
    *** MESSAGE : " ð exists for vendor ð. "
    ***           ®  OK  ¯
    =gfModalGen("TRM04024B00000","DIALOG",;
    lcMessage+' '+lcObjValue+"|"+ALLTRIM(cVendCode))
  ENDIF  
ELSE
  *B601578,1 Remove brackets frmo condition
  *IF (llBrowse .OR. ATC("?",lcObjValue) > 0) .AND. LASTKEY() = 13
  IF llBrowse .OR. ATC("?",lcObjValue) > 0 .AND. LASTKEY() = 13
  *B601578,1 end
    *B601578,1 IF '?', go top
    IF llBrowse .OR. LEFT(lcObjValue,1)='?'
      GO TOP
    ELSE
      *B601578,1 IF '?'in the expression, perform a softseek
      IF '?' $ lcObjValue 
        =SEEK(lcObjValue)
        lnCloseMatch = RECNO(0)
        IF BETWEEN(lnCloseMatch,1,RECCOUNT())
          GO lnCloseMatch
        ELSE
          GO TOP
        ENDIF
      ENDIF  
    ENDIF
    *B601578,1 end.  
    =gfBrows()
    IF laScrMode[1]
      _CUROBJ = _CUROBJ
    ENDIF  
  ELSE
    llKeyFound   = SEEK(lcObjValue)
    lnCloseMatch = RECNO(0)

    IF llKeyFound
      *** MESSAGE : " ð already exists for one or more vendor. "
      ***           " < Browse >   < Add >  < Reenter> "
      lnOption = gfModalGen('QRM04149B00001','Dialog',lcMessage+' '+lcObjValue)
    ELSE
      *** MESSAGE : " This record is not found in the data file."
      ***           " < Browse >   < Add >  < Reenter> "
      lnOption = gfModalGen('QRM00001B00001','Dialog',lcMessage+' '+lcObjValue)
    ENDIF
    
    DO CASE
      CASE lnOption = 1
        IF llKeyFound 
          =gfBrows(lcIndxExp)
        ELSE
          IF BETWEEN(lnCloseMatch,1,RECCOUNT())
            GO lnCloseMatch
          ELSE
            GO TOP
          ENDIF
          =gfBrows() 
        ENDIF 

        IF laScrMode[1]
          _CUROBJ = _CUROBJ
        ENDIF  

      CASE lnOption = 2
        IF EMPTY(laData[1])
          llAddMode = .T.
          _CUROBJ   = OBJNUM(laData[1])
        ENDIF  

      CASE lnOption = 3
        &lcObjName  = SPACE(LEN(EVALUATE(SYS(18))))
        _CUROBJ     = _CUROBJ
    ENDCASE
  ENDIF
ENDIF

SET EXACT &lcExactSet  
SELECT APVENDOR
IF lcOldData1 = laData[1] .AND. lnOldRecNo <= RECCOUNT()
  GO lnOldRecNo 
ENDIF

llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfvData_18
*!
*!**************************************************************************
*
FUNCTION lfvData_18

IF laData[18] < 0 
  *** MESSAGE : "Negative values are not allowed. "
  **            ®  OK  ¯
  =gfModalGen("TRM04087B00000","DIALOG")
  laData[18] = lgOldValue
  SHOW GET laData[18]
ENDIF

*!**************************************************************************
*!
*!      Function: lfvDivision
*!
*!**************************************************************************
*
*B801867,1 AMM This function not used any more.
FUNCTION lfvDivision

DO CASE
  CASE _DOS
    laData[12] =gfActPop(2,41,8,74,'SYCCODES','cCode_No','cDiscrep',@lcDivision)

  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    *lcDivision = SYCCODES.cdiscrep
    *laData[12] = SYCCODES.cCode_No
    lcDivision = CODES.cdiscrep
    laData[12] = CODES.cCode_No
    *E300643,1 Change this lines [End]
    
    SHOW GET lcDivision
ENDCASE

=lfRefresh()   && refresh says filed on the screen

IF _WINDOWS
  =gfUpdate()
  DEACTIVATE POPUP puDivision
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvSelWind
*!
*!**************************************************************************
*
FUNCTION lfvSelWind

DO CASE
  CASE _DOS
    lcWindPos =gfActPop(0,2,7,14,'laWindName',3,1,@lcWindow)
    lnWindPos = INT(VAL(lcWindPos))

    IF lnWindPos = 5
      lcNextWin = laWindName[1,1] 
    ELSE
      lcNextWin = laWindName[lnWindPos+1,1]
    ENDIF  

    IF lnWindPos > 0 .AND. lcExisWind <> laWindName[lnWindPos,2]
      =lfRefresh()    && refresh says filed on the screen
      lcObjName = 'ibWindow'  + lcWindPos
      lcNextBut = 'pbNextWin' + lcWindPos

      IF laScrMode[2]
        SHOW GET (lcObjName) ENABLE
      ELSE
        SHOW GETS WINDOW (laWindName[lnWindPos,2])  ENABLE  ONLY
      ENDIF  

      SHOW GET (lcNextBut),1 PROMPT lcNextWin 
      _CUROBJ = OBJNUM(&lcObjName)
      SHOW GETS WINDOW (lcExisWind) DISABLE ONLY 
      lcExisWind = laWindName[lnWindPos,2]
    ELSE
      _CUROBJ = _CUROBJ
    ENDIF

  CASE _WINDOWS
    lnWindPos  = INT(VAL(SUBSTR(SYS(18),10,1)))
    lnNewPos   = IIF(lnWindPos= 5,1,lnWindPos+1) 
  
    IF laScrMode[2]
      FOR lnCounter = 1 TO 5 
	    lcObjName = 'ibWindow'  + SUBSTR(SYS(18),10,1) +STR(lnCounter,1)
        SHOW GET (lcObjName) ENABLE
      ENDFOR  
    lcObjName = 'ibWindow'+STR(lnWindPos ,1)+STR(lnNewPos,1)
  
    ELSE
	  SHOW GETS WINDOW (laWindName[lnWindPos ,2])  ENABLE  ONLY
      lcObjName = 'ibWindow'+STR(lnWindPos,1)+STR(lnNewPos,1)
    ENDIF  

    _CUROBJ   = OBJNUM(&lcObjName)
  
    SHOW GETS WINDOW (lcExisWind) DISABLE ONLY 
  
    lcExisWind = laWindName[lnWindPos,2]
ENDCASE  

IF laScrMode[3] .OR. laScrMode[4]
  IF lnWindPos = 3 .AND. laData[40] = 'T' .AND. lcExisWind = laWindName[3,2]
    SHOW GET laData[41] ENABLE 
  ELSE
    SHOW GET laData[41] DISABLE
  ENDIF
  IF lnWindPos = 2 .AND. !EMPTY(laData[34]) .AND. lcExisWind = laWindName[2,2]
    SHOW GET ibCheck    ENABLE 
    SHOW GET laData[36] ENABLE 
  ELSE
    SHOW GET ibCheck    DISABLE
    SHOW GET laData[36] DISABLE
  ENDIF  
ELSE
  IF lnWindPos = 3 .AND. laData[40] = 'T' .AND. lcExisWind = laWindName[3,2]
    SHOW GET laData[41] DISABLE
  ENDIF
  IF lnWindPos = 2 .AND. !EMPTY(laData[34]) .AND. lcExisWind = laWindName[2,2]
    SHOW GET ibCheck    DISABLE
    SHOW GET laData[36] DISABLE
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function: lfv1099Type
*!
*!**************************************************************************
*
*B801867,1 AMM This function is not used any more.
FUNCTION lfv1099Type

DO CASE
  CASE _DOS
    laData[42]  =gfActPop(4,15,9,48,'SYCCODES','cCode_No','cDiscrep',@lc1099Type)

  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    *lc1099Type = SYCCODES.cdiscrep
    *laData[42] = SYCCODES.cCode_No
    lc1099Type = CODES.cdiscrep
    laData[42] = CODES.cCode_No
    *E300643,1 Change this lines [End]
    
   SHOW GET lc1099Type 
ENDCASE

=lfRefresh()    && refresh says filed on the screen

IF _WINDOWS
  =gfUpdate()
  DEACTIVATE POPUP pu1099Type
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvTermCode
*!
*!**************************************************************************
*
*B801867,1 AMM This function not used any more
FUNCTION lfvTermCode

DO CASE
  CASE _DOS
    laData[35]  =gfActPop(1,40,6,73,'SYCCODES','cCode_No','cDiscrep',@lcTermCode)

  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    *lcTermCode = SYCCODES.cdiscrep
    *laData[35] = SYCCODES.cCode_No
    lcTermCode = CODES.cdiscrep
    laData[35] = CODES.cCode_No
    *E300643,1 Change this lines [End]
    
    SHOW GET lcTermCode
ENDCASE

*E300643,1 Change this line for the changes we have made to (gfRltFld) [Begin]
*=gfRltFld(laData[35],@laTermAry)
=gfRltFld(laData[35] , @laTermAry , 'CTERMCODE')
*E300643,1 Change this line for the changes we have made to (gfRltFld) [End]

=lfRefresh()    && refresh says filed on the screen
  
IF _WINDOWS
  =gfUpdate()
  DEACTIVATE POPUP puTermCode
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvPayMethd
*!
*!**************************************************************************
*
FUNCTION lfvPayMethd

DO CASE
  CASE _DOS
    laData[37] =gfActPop(3,20,10,43,'laPayMethd',2,1,@lcPayMethd)
    =lfRefresh()    && refresh says filed on the screen
  CASE _WINDOWS
     *B600578,1 IF Cash payment code = "H" not "C"
     laData[37] = laPayMethd(ASUB(laPayMethd,ASCAN(laPayMethd,lcPayMethd),1),2)
    *laData[37] = SUBSTR(lcPayMethd,1,1)
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvData_43
*!
*!**************************************************************************
* Valid function for get field laData[43] representing factor
*
FUNCTION lfvData_43
PARAMETERS llBrwFact
PRIVATE lcAdrCode
*** llBrwFact is .T. if factor is to be browsed, 
***              .F. if only the factor information is to be extracted
*E301077,71 AMM Open the file here, it's not opened with program files.
=gfOpenFile(gcSysHome+'SYCFACT'  ,'CFACCODE','SH')
*E301077,71 AMM end
laData[43] = PADR(ALLTRIM(laData[43]),FSIZE('cFacCode','SYCFACT'))

IF llBrwFact .AND. (llBrowse .OR. laData[43] <> lgOldValue) .AND. lfGetFac(lgOldValue, llBrowse) ;
   .OR. !llBrwFact .AND. SEEK(laData[43], 'SYCFACT')
  laData[43]      = SYCFACT.cFacCode
  lcAdrCode       = SYCFACT.cCont_Code
  laFactorAd      = " " 
  laFactorAd[7,2] = SYCFACT.cPhoneNo
  laFactorAd[8,2] = SYCFACT.cFaxNo
  laFactorAd[1,2] = SYCFACT.cAddress1
  laFactorAd[2,2] = SYCFACT.cAddress2
  laFactorAd[3,2] = SYCFACT.cAddress3
  laFactorAd[4,2] = SYCFACT.cAddress4
  laFactorAd[5,2] = SYCFACT.cAddress5
  laFactorAd[6,2] = SYCFACT.cAddress6
  *** Look for the factor's address code in SYCINT file to 
  *** get its format
  *** If the factor's address code is empty, use the default
  *** company address code
  IF !EMPTY(lcAdrCode) .AND. SEEK(lcAdrCode, 'SYCINT')
    laFactorAd[1,1] = SYCINT.cPart1Lab
    laFactorAd[2,1] = SYCINT.cPart2Lab
    laFactorAd[3,1] = SYCINT.cPart3Lab
    laFactorAd[4,1] = SYCINT.cPart4Lab
    laFactorAd[5,1] = SYCINT.cPart5Lab
    laFactorAd[6,1] = SYCINT.cPart6Lab
  ELSE   
    laFactorAd[1,1] = lcAddHed1
    laFactorAd[2,1] = lcAddHed2
    laFactorAd[3,1] = lcAddHed3
    laFactorAd[4,1] = lcAddHed4
    laFactorAd[5,1] = lcAddHed5
    laFactorAd[6,1] = lcAddHed6
  ENDIF
  SHOW GET laFactorAd[1,2]
  SHOW GET laFactorAd[2,2]
  SHOW GET laFactorAd[3,2]
  SHOW GET laFactorAd[4,2]
  SHOW GET laFactorAd[5,2]
  SHOW GET laFactorAd[6,2]
  SHOW GET laFactorAd[7,2]
  SHOW GET laFactorAd[8,2]
  =lfRefresh()
ELSE
  IF !EMPTY(laData[43])
    laData[43] = lgOldValue
    SHOW GET laData[43]
  ELSE
    laFactorAd      = " "
    laFactorAd[1,1] = lcAddHed1
    laFactorAd[2,1] = lcAddHed2
    laFactorAd[3,1] = lcAddHed3
    laFactorAd[4,1] = lcAddHed4
    laFactorAd[5,1] = lcAddHed5
    laFactorAd[6,1] = lcAddHed6
    SHOW GET laFactorAd[1,2]
    SHOW GET laFactorAd[2,2]
    SHOW GET laFactorAd[3,2]
    SHOW GET laFactorAd[4,2]
    SHOW GET laFactorAd[5,2]
    SHOW GET laFactorAd[6,2]
    SHOW GET laFactorAd[7,2]
    SHOW GET laFactorAd[8,2]
    =lfRefresh()
  ENDIF   
ENDIF
llBrowse = .F.
SELECT APVENDOR

*!**************************************************************************
*!
*!      Function: lfGetAcct
*!
*!**************************************************************************
*
FUNCTION lfGetAcct
PARAMETERS lcAcctVal,lcAcctDesc
*E300824,1 AMM If AP not installed, Return without validation.
IF !llApInst
  RETURN
ENDIF

PRIVATE llValidAcs
llValidAcs = .F.
*E300824,1 AMM end
IF EMPTY(STRTRAN(EVALUATE(lcAcctVal),'-',' '))
  &lcAcctVal  = lcEmptyAcc
  &lcAcctDesc = SPACE(60)
ELSE
  *E300824,1 AMM Change the way of calling the function, to be able to call 
  *E300824,1 AMM it withen its application.
  *IF !lfApAcs(@&lcAcctDesc,llBrowse)

  *B602483,1 Hesham (Start)
  *B602483,1 define variable llfuncall before calling any functions in the ap appl. to
  *B602483,1 stop the cleanup function from working
  llFunCall = .T.
  *B602483,1 Hesham (End)

  *B606427,1 ALB Saving the value of lcBaseWin in lcTBaseWind, because in the multisession 
  *B606427,1 ALB after bank/Gl Account brows a syntax error occurs. [Begin]
  lcTBaseWind = lcBaseWind
  *B606427,1 ALB Fix the Multisession after bank/Gl Account brows [end]

  DO (gcAppHome+"AP.APP") WITH "lfApAcs", "&lcAcctDesc, llBrowse, llValidAcs" , .T.

  *B606427,1 ALB Restoring the old value of lcBaseWin [Begin]
  lcBaseWind = IIF(EMPTY(ALLTRIM(lcTBaseWind)),gcBaseWind,lcTBaseWind)
  *B606427,1 ALB [end]

  *E300824,1 AMM If the function returned false, get the old values.

  IF !llValidAcs
  *E300824,1 AMM end
    &lcAcctVal  = lgOldValue
    &lcAcctDesc = lcOldDesc 
  ENDIF    
ENDIF
*B606427,1 ALB Fix the Multisession after bank/Gl Account brows [Begin]
lcBaseWind = IIF(EMPTY(ALLTRIM(lcTBaseWind)),gcBaseWind,lcTBaseWind)
*B606427,1 ALB Fix the Multisession after bank/Gl Account brows [end]
SHOW GET (lcAcctVal)
SHOW GET (lcAcctDesc)
llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfvBnkChk
*!
*!**************************************************************************
* Valid function for get fields laData[34] (bank code), and laData[36] 
* (checking account code)
*
FUNCTION lfvBnkChk
*E300824,1 AMM start, This variable is the return variable from the function 
*E300824,1 AMM lfBnkChk, Declare it here to get it after calling the function.
llVldBnk = .T.
*E300824,1 AMM end

IF EVALUATE(SYS(18)) <> lgOldValue
  =gfUpdate()
ENDIF
*E300824,1 AMM start, Call the function by another way, to run from any module.
*IF !lfBnkChk(@laBankObjs, lgOldValue, @llBrowse, @lcChkStat)
  *RETURN 1
*ENDIF  

IF llAPINST
  *B602483,1 Hesham (Start)
  *B602483,1 define variable llfuncall before calling any functions in the ap appl. to
  *B602483,1 stop thection from working
  llFunCall = .T.
  *B602483,1 Hesham (End)

  *B606427,1 ALB Fix the Multisession after bank/Gl Account brows [Begin]
  lcTBaseWind = lcBaseWind
  *B606427,1 ALB Fix the Multisession after bank/Gl Account brows [end]

  DO (gcAppHome+"AP.APP") WITH "LFBnkChk" ,"laBAnkObjs,lgOldValue, llBrowse, lcChkStat,'','','', llVldBnk" , .T.

  *B606427,1 ALB Restoring the old value of lcBaseWin [Begin]
  lcBaseWind = IIF(EMPTY(ALLTRIM(lcTBaseWind)),gcBaseWind,lcTBaseWind)
  *B606427,1 ALB [end]

  
  IF !llVldBnk
    RETURN 1
  ENDIF
ENDIF
*E300824,1 AMM end
*!**************************************************************************
*!
*!      Function: lfvTaxType
*!
*!**************************************************************************
*
FUNCTION lfvTaxType

IF rbTaxType = 1 
  laData[40] = 'L'
  SHOW GET laData[41] DISABLE 
ELSE
  laData[40] = 'T'
  SHOW GET laData[41] ENABLE
ENDIF

=lfRefresh()    && refresh says filed on the screen

*!**************************************************************************
*!
*!      Function: lpEscPred
*!
*!**************************************************************************
*
FUNCTION lpEscPred

ACTIVATE WINDOW APVENPA3
_CUROBJ = OBJNUM(pbClose)
KEYBOARD "{ENTER}"

*!**************************************************************************
*!
*!      Function: lpTrapKeys
*!
*!**************************************************************************
*
FUNCTION lpTrapKeys

IF WONTOP() = 'APVENPA3'
  ACTIVATE WINDOW (lcBrowsTtl)
ELSE
  ACTIVATE WINDOW APVENPA3
  _CUROBJ = OBJNUM(pbClose)
ENDIF

*!**************************************************************************
*!
*!      Function: lfvTaxPerc
*!
*!**************************************************************************
*
FUNCTION lfvTaxPerc

IF laData[41] < 0 
  *** MESSAGE : "Negative values are not allowed. "
  ***                ®  OK  ¯
  =gfModalGen("TRM04087B00000","DIALOG")
  laData[41] = lgOldValue
  SHOW GET laData[41]
ENDIF

*!**************************************************************************
*!
*!      Function: lpvInquiry
*!
*!**************************************************************************
*
FUNCTION lpvInquiry

*B801867,1 AMM Open files
IF llAPINST
  llDist   = gfOpenFile(gcDataDir+'ApDist'   ,'PAYMNTS','SH') 
  llPaymnt =gfOpenFile(gcDataDir+'ApPaymnt' ,'TYPCLNO','SH') 
ENDIF
*B801867,1 AMM end

DO CASE
  CASE BAR() = 1     && Payable Invoices
    SELECT APINVHDR
    SET ORDER TO TAG VENDINV

    IF SEEK(laData[1])
      DIMENSION laTemp[2]
      laTemp     = ''
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      lcFile_Ttl = "Payable Invoices"
      *E300258,4 MAN Add the check on supressing 1099 processing
      *E300331,1 YMO Add THE BALANCE FIELD (OPEN AMOUNT) TO THE BROWSE
      *lcBrFields = "CVENDCODE :R:H= 'Vendor code',"               +;
                   "CINVNO    :R:H= 'Invoice number',"            +;
                   "CDIVISION=LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION,SYCCODES.cCode_No,'CODES') :R:H='Division',"  +;
                   "DINVDATE  :R:H= 'Invoice date',"              +;
                   "CINVREF   :R:H= 'Reference',"                 +;
                   "NINVAMNT  :R:H= 'Invoice amount',"            +;
                   "NINVDISOF :R:H= 'Amount of discount offere'," +;
                   "NINVAMTAP :R:H= 'Approved to pay',"           +;
                   "NINVDISAP :R:H= 'Discount approved',"         +;
                   "NINVADJAP :R:H= 'Adjustment approved',"       +;
                   "NINVPAID  :R:H= 'Paid amount',"               +;
                   "NINVDISTK :R:H= 'Discount taken',"            +;
                   "NINVADJ   :R:H= 'Adjustment amount',"         +;
                   IIF(llApS1099,"","NINV1099A :R:H= 'Amount of 1099 payment',")    +;
                   "CVENPRIOR :R:H= 'Payment priority',"          +;
                   "CBNKCODE  :R:H= 'Bank code',"                 +;
                   "CCHKACCT  :R:H= 'Bank checking account',"     +;
                   "CCHKGLACC :R:H= 'GL checking account'"
                   
*E500061,1 M.H 02/06/96 Begin.
       *lcBrFields = "CVENDCODE :R:H= 'Vendor code',"               +;
                   "CINVNO    :R:H= 'Invoice number',"            +;
                   "CBALANCE=(NINVAMNT - NINVPAID - NINVADJ) :R:H= 'Open amount',"              +;
                   "CDIVISION=LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION,SYCCODES.cCode_No,'CODES') :R:H='Division',"  +;
                   "DINVDATE  :R:H= 'Invoice date',"              +;
                   "CINVREF   :R:H= 'Reference',"                 +;
                   "NINVAMNT  :R:H= 'Invoice amount',"            +;
                   "NINVDISOF :R:H= 'Amount of discount offere'," +;
                   "NINVAMTAP :R:H= 'Approved to pay',"           +;
                   "NINVDISAP :R:H= 'Discount approved',"         +;
                   "NINVADJAP :R:H= 'Adjustment approved',"       +;
                   "NINVPAID  :R:H= 'Paid amount',"               +;
                   "NINVDISTK :R:H= 'Discount taken',"            +;
                   "NINVADJ   :R:H= 'Adjustment amount',"         +;
                   IIF(llApS1099,"","NINV1099A :R:H= 'Amount of 1099 payment',")    +;
                   "CVENPRIOR :R:H= 'Payment priority',"          +;
                   "CBNKCODE  :R:H= 'Bank code',"                 +;
                   "CCHKACCT  :R:H= 'Bank checking account',"     +;
                   "CCHKGLACC :R:H= 'GL checking account'"
*E300331,1 ORABY SHOW ONLY UNPAID INVOICES
*B601013,1 Add currency code fields to the browse fields
*B601013,1 "cCurrCode :R:H = 'Curr.':5,"+;
*B601013,1 "nExRate   :R:H = 'Ex. Rate':9,"+;
*B601013,1 "cAprCurCod :R:H = 'Apr. Curr.':10,"+;
*B601013,1 "nAprExRat  :R:H = 'Apr. Rate.':9,"

      *E300643,1 Change this line for the changes we have made 
      *          to (gfCodDes) [Begin]
      *lcBrFields = "CINVNO    :R:H= 'Invoice No.',"            +;
      *             "NINVAMNT  :R:H= 'Invoice amount',"            +;
      *             "NINVDISTK :R:H= 'Disc. taken':12,"            +;
      *             "NINVADJ   :R:H= 'Adjustment' :12,"         +;
      *             "CBALANCE=(NINVAMNT - NINVPAID - NINVADJ- NINVDISTK) :R:H= 'Open amount'," +;
      *             "cCurrCode :R:H = 'Curr.':5,"                  +;
      *             "nExRate   :R:H = 'Ex. Rate':9,"               +;
      *             "DINVDATE  :R:H= 'Invoice date',"              +;
      *             "NINVDISOF :R:H= 'Discount offere',"           +;
      *             "NINVAMTAP :R:H= 'Approved to pay',"           +;
      *             "NINVDISAP :R:H= 'Discount approved',"         +;
      *             "NINVADJAP :R:H= 'Adjustment approved',"       +;
      *             "cAprCurCod:R:H = 'Apr. Curr.':10,"            +;
      *             "nAprExRat :R:H = 'Apr. Rate.':9,"            +;
      *             "NINVPAID  :R:H= 'Paid amount',"               +;
      *             IIF(llApS1099,"","NINV1099A :R:H= '1099 payment',")    +;
      *             "CVENPRIOR :R:H= 'Payment priority',"          +;
      *             "CBNKCODE  :R:H= 'Bank code',"                 +;
      *             "CCHKACCT  :R:H= 'Bank checking account',"     +;
      *             "CCHKGLACC :R:H= 'GL checking account',"       +;
      *             "CCHKNO    :R:H= 'Last check #',"              +;
      *             "DCHKDATE  :R:H= 'Last payment date',"         +;
      *             "CINVREF   :R:H= 'Reference',"                 +;
      *             "CDIVISION=gfCodDes(CDIVISION):R:H='Division'"

      lcBrFields = "CINVNO    :R:H= 'Invoice No.',"            +;
                   "NINVAMNT  :R:H= 'Invoice amount',"            +;
                   "NINVDISTK :R:H= 'Disc. taken':12,"            +;
                   "NINVADJ   :R:H= 'Adjustment' :12,"         +;
                   "CBALANCE=(NINVAMNT - NINVPAID - NINVADJ- NINVDISTK) :R:H= 'Open amount'," +;
                   "cCurrCode :R:H = 'Curr.':5,"                  +;
                   "nExRate   :R:H = 'Ex. Rate':9,"               +;
                   "DINVDATE  :R:H= 'Invoice date',"              +;
                   "NINVDISOF :R:H= 'Discount offere',"           +;
                   "NINVAMTAP :R:H= 'Approved to pay',"           +;
                   "NINVDISAP :R:H= 'Discount approved',"         +;
                   "NINVADJAP :R:H= 'Adjustment approved',"       +;
                   "cAprCurCod:R:H = 'Apr. Curr.':10,"            +;
                   "nAprExRat :R:H = 'Apr. Rate.':9,"            +;
                   "NINVPAID  :R:H= 'Paid amount',"               +;
                   IIF(llApS1099,"","NINV1099A :R:H= '1099 payment',")    +;
                   "CVENPRIOR :R:H= 'Payment priority',"          +;
                   "CBNKCODE  :R:H= 'Bank code',"                 +;
                   "CCHKACCT  :R:H= 'Bank checking account',"     +;
                   "CCHKGLACC :R:H= 'GL checking account',"       +;
                   "CCHKNO    :R:H= 'Last check #',"              +;
                   "DCHKDATE  :R:H= 'Last payment date',"         +;
                   "CINVREF   :R:H= 'Reference',"                 +;
                   "CDIVISION=gfCodDes(CDIVISION , 'CDIVISION'):R:H='Division'"

      *E300643,1 Change this line [End]                  
      
*B601013,1 Substituted the following line with a call to gfCodDes
*B601013,1 as "CDIVISION=gfCodDes(CDIVISION):R:H='Division'" because, 
*B601013,1 1. After adding the currency fields, the compiled line for
*B601013,1    variable lcBrFields has become too long.
*B601013,1 2. It is a kind of standard that descriptions from the codes
*B601013,1    file should be fetched by gfCodDes() function
             *"CDIVISION=IIF(EMPTY(CDIVISION),'N/A'+SPACE(27),LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION,SYCCODES.cCode_No,'CODES')) :R:H='Division'"
*B601013,1 end.
*E500061,1 M.H 02/06/96 End.
      *=gfBrows([FOR CVENDCODE = laData(1) AND CINVSTAT <> 'V'],'CVENDCODE,CINVNO','laTemp')
      *B801121,1 MAN  removed 'AND cVendCode = laData[1]'
      *B801121,1 MAN  from the for Clause and added laData[1] to the key cluse
      *=gfBrows([FOR  (NINVAMNT - NINVPAID - NINVADJ) <> 0 AND CVENDCODE = laData(1) AND CINVSTAT <> 'V'],'CVENDCODE,CINVNO','laTemp')
      =gfBrows("laData[1] FOR  (NINVAMNT - NINVPAID - NINVADJ - NINVDISTK) <> 0 AND CINVSTAT <> 'V'",'CVENDCODE,CINVNO','laTemp')
      *B801121,1 end

      lcBrFields = lcSavBrFld
      lcFile_Ttl = lcSavTitle
      IF !EMPTY(laTemp[1])
        *** B600262 Line was added by YI-Yasser in 4-6-95
        =gfstatic()
        *E300683,1 Add parameters to GPDOPROG
        *DO gpDoProg WITH 'AWRAPPYINV WITH "&laData[1]","&laTemp[2]"'," ",'AP'
        DO gpDoProg WITH 'AWRAPPYINV', .F., 'AP',"'"+laData(1)+"','"+laTemp(2)+"'"
        *E300683,1 end
      ENDIF
    ELSE
      ** MESSAGE : " No ð found for vendor ð "
      **           "     ® Ok ¯              "
      =gfModalGen("TRM04042B00000","DIALOG","invoices|"+ALLTRIM(ladata[1]))
    ENDIF  
    SET ORDER TO TAG INVVEND    IN APINVHDR
  ****************************************************************

  CASE BAR() = 2     && Payments
    SELECT APPAYMNT
    IF SEEK('P'+laData[1])
      DIMENSION laTemp[2]
      laTemp     = ''
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      lcFile_Ttl = "Payments"
      *B601013,1 Add currency code fields to the browse fields
      *B601013,1 "cCurrCode :R:H = 'Curr.':5,"+;
      *B601013,1 "nExRate   :R:H = 'Ex. Rate':9,"+;
      *lcBrFields = "CPAYMETHD ="                                +;
                   "IIF(CPAYMETH = 'P','Printed checks',"       +;
                   "IIF(CPAYMETH = 'M','Manual checks',"        +;
                   "IIF(CPAYMETH = 'N','Non check payments',"   +;
                   "'Cash payment'))) :R:H='Payment method',"   +;  
                   "CBNKCODE  :R:H= 'Bank code',"               +;
                   "CCHKACCT  :R:H= 'Bank checking account',"   +;
                   "CPAYDOCNO :R:H= 'Payment no.',"             +;
                   "CPAYSTAT  :R:H= 'Payment state',"           +;
                   "DPAYDATE  :R:H= 'Payment date',"            +;
                   "CFISFYEAR :R:H= 'Fiscal year',"             +;
                   "CFSPPRDID :R:H= 'Period',"                  +;
                   "DPAYVDATE :R:H= 'Void date',"               +;
                   "cAdvPay   =IIF(lPayAdvan,'Yes','No '):R:H= 'Advanced payment',"+;
                   "CPAYCOMP  :R:H= 'Vendor name',"             +;
                   "NPAYAMNT  :R:H= 'Payment amount',"          +;
                   "NPAYDISC  :R:H= 'Discount',"                +;
                   "NPAYADJ   :R:H= 'Adjustment',"              +;
                   IIF(llApS1099,"","NINV1099A :R:H= 'Amount of 1099 payment',")  +;
                   "CPAYRECST :R:H= 'Reconciliation state',"    +;
                   "DPAYRECDT :R:H= 'Reconciliation date',"     +;
                   "CPAYRECRF :R:H= 'Reconciliation Reference'"
      *B601066,1 Hesham El-Sheltawi (Start)
      *B601066,1 change the header of the CPAYSTAT for payment state
      *B601066,1 to Payment Status
*      lcBrFields = "CPAYMETHD ="                                +;
                   "IIF(CPAYMETH = 'P','Printed checks',"       +;
                   "IIF(CPAYMETH = 'M','Manual checks',"        +;
                   "IIF(CPAYMETH = 'N','Non check payments',"   +;
                   "'Cash payment'))) :R:H='Payment method',"   +;  
                   "CBNKCODE  :R:H= 'Bank code',"               +;
                   "CCHKACCT  :R:H= 'Bank checking account',"   +;
                   "CPAYDOCNO :R:H= 'Payment no.',"             +;
                   "CPAYSTAT  :R:H= 'Payment state',"           +;
                   "DPAYDATE  :R:H= 'Payment date',"            +;
                   "CFISFYEAR :R:H= 'Fiscal year',"             +;
                   "CFSPPRDID :R:H= 'Period',"                  +;
                   "DPAYVDATE :R:H= 'Void date',"               +;
                   "cAdvPay   =IIF(lPayAdvan,'Yes','No '):R:H= 'Advanced payment',"+;
                   "CPAYCOMP  :R:H= 'Vendor name',"             +;
                   "NPAYAMNT  :R:H= 'Payment amount',"          +;
                   "cCurrCode :R:H = 'Curr.':5,"+;
                   "nExRate   :R:H = 'Ex. Rate':9,"+;
                   "NPAYDISC  :R:H= 'Discount',"                +;
                   "NPAYADJ   :R:H= 'Adjustment',"              +;
                   IIF(llApS1099,"","NINV1099A :R:H= 'Amount of 1099 payment',")  +;
                   "CPAYRECST :R:H= 'Reconciliation state',"    +;
                   "DPAYRECDT :R:H= 'Reconciliation date',"     +;
                   "CPAYRECRF :R:H= 'Reconciliation Reference'"
      *B601013,1 end.
      lcBrFields = "CPAYMETHD ="                                +;
                   "IIF(CPAYMETH = 'P','Printed checks',"       +;
                   "IIF(CPAYMETH = 'M','Manual checks',"        +;
                   "IIF(CPAYMETH = 'N','Non check payments',"   +;
                   "'Cash payment'))) :R:H='Payment method',"   +;  
                   "CBNKCODE  :R:H= 'Bank code',"               +;
                   "CCHKACCT  :R:H= 'Bank checking account',"   +;
                   "CPAYDOCNO :R:H= 'Payment no.',"             +;
                   "CPAYSTAT  :R:H= 'Payment status',"           +;
                   "DPAYDATE  :R:H= 'Payment date',"            +;
                   "CFISFYEAR :R:H= 'Fiscal year',"             +;
                   "CFSPPRDID :R:H= 'Period',"                  +;
                   "DPAYVDATE :R:H= 'Void date',"               +;
                   "cAdvPay   =IIF(lPayAdvan,'Yes','No '):R:H= 'Advanced payment',"+;
                   "CPAYCOMP  :R:H= 'Vendor name',"             +;
                   "NPAYAMNT  :R:H= 'Payment amount',"          +;
                   "cCurrCode :R:H = 'Curr.':5,"+;
                   "nExRate   :R:H = 'Ex. Rate':9,"+;
                   "NPAYDISC  :R:H= 'Discount',"                +;
                   "NPAYADJ   :R:H= 'Adjustment',"              +;
                   IIF(llApS1099,"","NINV1099A :R:H= 'Amount of 1099 payment',")  +;
                   "CPAYRECST :R:H= 'Reconciliation state',"    +;
                   "DPAYRECDT :R:H= 'Reconciliation date',"     +;
                   "CPAYRECRF :R:H= 'Reconciliation Reference'"
      *B601066,1 Hesham El-Sheltawi (End)
      
      *B801165,1 Browse KEY instead of browse FOR
      *=gfBrows([FOR CPAYTYPE = 'P' .AND. CPAYCLNO = laData(1)],'CPAYDOCNO','laTemp')
      =gfBrows(['P'+ laData(1)],'CPAYDOCNO','laTemp')
      *B801165,1 end
      lcBrFields = lcSavBrFld
      lcFile_Ttl = lcSavTitle
      
      IF !EMPTY(laTemp[1])
        llAdvance  = APPAYMNT.lPayAdvan
        lcPayMeth  = APPAYMNT.cPayMeth
        lcVendName = APPAYMNT.CPAYCOMP
        lcBankCode = APPAYMNT.CBNKCODE
        lcChkAcct  = APPAYMNT.CCHKACCT
        lcPayNo    = APPAYMNT.CPAYDOCNO
        ldPayDate  = APPAYMNT.DPAYDATE
        lcPeriod   = APPAYMNT.CFSPPRDID
        lcPayYear  = APPAYMNT.CFISFYEAR
        lnPayAmnt  = APPAYMNT.NPAYAMNT
        lnDiscAmnt = APPAYMNT.NPAYDISC
        lnAdjAmnt  = APPAYMNT.NPAYADJ
        ln1099Amnt = APPAYMNT.NINV1099A
        llPayVoid  = IIF(APPAYMNT.CPAYSTAT = 'V',.T.,.F.)

        IF ! llAdvance  
          PUSH KEY
          ON KEY LABEL CTRL+W     lnDummy = 1
          ON KEY LABEL Ctrl+Q     lnDummy = 1
          ON KEY LABEL CTRL+HOME  lnDummy = 1 
          ON KEY LABEL CTRL+END   lnDummy = 1 
          ON KEY LABEL Ctrl+ENTER lnDummy = 1
          ON KEY LABEL ESC     DO lpEscPred
          ON KEY LABEL TAB     DO lpTrapKeys
          ON KEY LABEL BACKTAB DO lpTrapKeys 
          
          *=============
          lcInvoice = ' '
          DECLARE laFileStru[1]
          SELECT APINVHDR
          =AFIELDS(laFileStru)
          lnFileStru = ALEN(laFileStru,1)
          DIMENSION laFileStru[lnFileStru+5,4]

          laFileStru[lnFileStru+1,1] = 'N1099'
          laFileStru[lnFileStru+1,2] = 'N'
          laFileStru[lnFileStru+1,3] = 15
          laFileStru[lnFileStru+1,4] = 2
  
          laFileStru[lnFileStru+2,1] = 'NPAID'
          laFileStru[lnFileStru+2,2] = 'N'
          laFileStru[lnFileStru+2,3] = 15
          laFileStru[lnFileStru+2,4] = 2
  
          laFileStru[lnFileStru+3,1] = 'NADJUST'
          laFileStru[lnFileStru+3,2] = 'N'
          laFileStru[lnFileStru+3,3] = 15
          laFileStru[lnFileStru+3,4] = 2

          laFileStru[lnFileStru+4,1] = 'NDISCOUNT'
          laFileStru[lnFileStru+4,2] = 'N'
          laFileStru[lnFileStru+4,3] = 10
          laFileStru[lnFileStru+4,4] = 2

          laFileStru[lnFileStru+5,1] = 'NTOTAL'
          laFileStru[lnFileStru+5,2] = 'N'
          laFileStru[lnFileStru+5,3] = 15
          laFileStru[lnFileStru+5,4] = 2

          *** Creat temp  file from the array.
          lcTempInv = gfTempName()
          CREATE TABLE &gcWorkDir.&lcTempInv FROM ARRAY laFileStru

          SELECT APPAYMNT
          SET RELATION TO APPAYMNT.CPAYCLNO INTO APVENDOR ADDITIVE
          SET RELATION TO APPAYMNT.CPAYCLNO+APPAYMNT.CFISFYEAR INTO APVENHST ADDITIVE
          SET RELATION TO APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO INTO APDIST ADDITIVE
          SELECT APDIST
          SET RELATION TO APDIST.CINVNO+APDIST.CVENDCODE INTO APINVHDR ADDITIVE

          SCAN REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF = ;
               APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO
            IF lcInvoice <> APDIST.CINVNO
              SELECT APINVHDR
              SCATTER MEMVAR MEMO

              SELECT(lcTempInv)
              APPEND BLANK
              GATHER MEMVAR MEMO
              lcInvoice = APDIST.CINVNO
            ENDIF

            SELECT(lcTempInv)
  
            DO CASE
              CASE APDIST.CAPDACTID = 'A'
                REPLACE NTOTAL    WITH APDIST.NAPDAMNT
              CASE APDIST.CAPDACTID = 'B'
                REPLACE N1099     WITH APDIST.NAPDAMNT
              CASE APDIST.CAPDACTID = 'C'
                REPLACE NPAID     WITH APDIST.NAPDAMNT
              CASE APDIST.CAPDACTID = 'J'
                REPLACE NADJUST   WITH APDIST.NAPDAMNT
              CASE APDIST.CAPDACTID = 'S'
                REPLACE NDISCOUNT WITH APDIST.NAPDAMNT
            ENDCASE
            SELECT APDIST
          ENDSCAN

          SELECT APPAYMNT
          SET RELATION TO 
          SELECT APDIST
          SET RELATION TO 
          SELECT (lcTempInv)
          GO TOP           
          *=============
        ENDIF 

        lcBrowsTtl = 'Payments'
        *E300683,1 Call *.SPR from screens directory
		*DO APVENPAY.SPR      
		DO (gcScrDir + '\APVENPAY.SPR')
		*E300683,1 end

        IF ! llAdvance  
          POP KEY
          IF USED(lcTempInv) 
            USE IN &lcTempInv
          ENDIF

          ERASE (gcWorkdir+lcTempInv+".DBF")  && Erase the Temp file.
          ERASE (gcWorkdir+lcTempInv+".CDX")  && Erase the Temp index.
          ERASE (gcWorkdir+lcTempInv+".FPT")  && Erase the Temp FPT.  
        ENDIF
      ENDIF
    ELSE
      ** MESSAGE : " No ð found for vendor ð "
      **           "                  ® Ok ¯ "
      =gfModalGen("TRM04042B00000","DIALOG","payments|"+ALLTRIM(ladata[1]))
    ENDIF  
*****************************************************************************************
  *B605547,1 ALB Add Orders to vendor option screen [Begin]
  CASE BAR() = 3     && Orders

    IF !USED('POSHDR')
      =gfOpenFile(gcDataDir+'POSHDR' ,'POSHDRV','SH') 
    ENDIF
    SELECT POSHDR
    SET ORDER TO TAG POSHDRV
    IF SEEK(laData[1])
      DIMENSION laTemp[2]
      laTemp     = ''
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      lcFile_Ttl = "Orders"
      lcFltrExp  = "laData[1] FOR CSTYTYPE $ 'PRCD' "
      lcBrFields = "CSTYTYP = IIF(CSTYTYPE='D','Dye Order','Purchase Order')  :R:H= 'Type',"+;
                   "PO        :R:H= 'Order No.',"            +;
                   "ENTERED   :R:H= 'Entered Date',"         +;
                   "COMPLETE  :R:H= 'Complete Date',"        +;
                   "AVAILABLE :R:H= 'Available Date',"       +;
                   "NSTYORDER :R:H= 'Ordered',"              +;
                   "RECEIVE   :R:H= 'Received',"             +;
                   "DAMAGE    :R:H= 'Damaged',"              +;
                   "CANCEL    :R:H= 'Canceled',"             +;
                   "OPEN      :R:H= 'Open'" 

      =ARIABROW(lcFltrExp,lcFile_Ttl,20,.F.,20,.F.,.F.,.F.,'CSTYTYPE,PO,POTYPE','laTemp',.F.,.F.,.F.)
      lcBrFields = lcSavBrFld
      lcFile_Ttl = lcSavTitle
      
      IF !EMPTY(laTemp[1]) AND laTemp[1] = 'P'
        lcBaseWind = gcBaseWind
        =gfstatic()
        DO gpDoProg WITH 'AWRPOSTYLE', .F., 'PO' ,"'"+laTemp(1)+"','"+laTemp(2)+"'"
      ENDIF
    ELSE
      ** MESSAGE : " No ð found for vendor ð "
      **           "     ® Ok ¯              "
      =gfModalGen("TRM04042B00000","DIALOG","orders|"+ALLTRIM(ladata[1]))
    ENDIF  
    IF llAPINST
      SET ORDER TO TAG INVVEND    IN APINVHDR
    ENDIF  
*******************************************************
  CASE BAR() = 4     && Reception
    IF !USED('POSLN')
      =gfOpenFile(gcDataDir+'POSLN' ,'POSLN','SH') 
    ENDIF
    IF !USED('POSHDR')
      =gfOpenFile(gcDataDir+'POSHDR' ,'POSHDRV','SH') 
    ENDIF
    SELECT POSHDR
    SET ORDER TO TAG POSHDRV

    IF SEEK(laData[1])
      SELECT POSLN
      SET ORDER TO TAG POSLN
      DIMENSION laTemp[2]
      laTemp     = ''
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      lcFile_Ttl = "Receipts"
      lcStyTitle = gfItemMask('HI')
      lcFltrExp = "FOR VENDOR = laData[1] AND CSTYTYPE $ 'PD' AND TRANCD='2' "
      lcBrFields = "CSTYTYPE = IIF(CSTYTYPE='D','Dye Order','Purchase Order')  :R:H= 'Type',"+;
                   "PO        :R:H= 'Order No.',"            +;
                   "STYLE     :R:H= lcStyTitle ,"          +;
                   "SCALE     :R:H= 'Scale',"                +;
                   "DYELOT    :R:H= 'Dyelot',"               +;
                   "SHIPNO    :R:H= 'Shipment No',"          +;
                   "CWARECODE :R:H= 'Warehouse',"            +;
                   "CRSESSION :R:H= 'Receiving Session',"          +;
                   "DATE      :R:H= 'Receiving Date'"
      =ARIABROW(lcFltrExp,lcFile_Ttl,20,.F.,20,.F.,.F.,.F.,'CSTYTYPE,PO','laTemp',.F.,.F.,.F.)
      lcBrFields = lcSavBrFld
      lcFile_Ttl = lcSavTitle
      
    ELSE
      ** MESSAGE : " No ð found for vendor ð "
      **           "     ® Ok ¯              "
      =gfModalGen("TRM04042B00000","DIALOG","orders|"+ALLTRIM(ladata[1]))
    ENDIF  
    IF llAPINST
      SET ORDER TO TAG INVVEND    IN APINVHDR
    ENDIF  

*B605547,1 ALB Add Orders to vendor option screen [end]
**************************************************************************************


  CASE BAR() = 5
    =gfActWind('CWDAPVENHD',lc_Title,gcBaseWind)
    SHOW GET lcFisYear ENABLE

  *E301119,1 MAB 10/31/1999 Add salutation screen to menu options. [Begin]
  CASE BAR() = 6
    DO (gcAppHome+"ARSALUT.FXP") WITH "APVENDOR"
  *E301119,1 MAB 10/31/1999 Add salutation screen to menu options. [Begin]
ENDCASE
*B801867,1 AMM Close files
IF USED('ApDist') .AND. llDist
  =gfCloseFile('ApDist')
ENDIF
IF USED('ApPaymnt') .AND. llPaymnt
  =gfCloseFile('ApPaymnt')
ENDIF
*B801867,1 AMM end
SELECT APVENDOR

*!**************************************************************************
*!
*!      Function: lfvFiscYear
*!
*!**************************************************************************
*
FUNCTION lfvFiscYear
PARAMETERS llDispPop

IF llDispPop
  DO CASE
    CASE _DOS
      lcYearName=gfActPop(0,16,5,24,'laFiscYear',2,1,@lcFisYear)
    CASE _WINDOWS
      lcYearName=laFiscYear[ASUBSCRIPT(laFiscYear,ascan(laFiscyear,lcFisYear),1),2]
  ENDCASE  

  IF lcOldYear = lcFisYear
    RETURN
  ENDIF
ENDIF
  
SELECT APVENHST

IF SEEK(laData[1]+lcFisYear)
  lnYeArPos = ASUBSCRIPT(laFiscYear,ASCAN(laFiscYear,lcFisYear),1)
  DIMENSION laHistory[INT(VAL(laFiscYear[lnYeArPos,3])),1] 
  lnAvgPurc = 0.00
  lnAvgPay  = 0.00
	
  FOR lnCounter = 1 TO INT(VAL(laFiscYear[lnYeArPos,3]))
    lcPurPrd  = 'nVnHPur' + ALLTRIM(STR(lnCounter,2))
    lcPayPrd  = 'nVnHPay' + ALLTRIM(STR(lnCounter,2))
    lnAvgPurc = lnAvgPurc + EVALUATE(lcPurPrd)
    lnAvgPay  = lnAvgPay  + EVALUATE(lcPayPrd)

    laHistory[lnCounter] = PADL(ALLTRIM(STR(lnCounter)),2,'0') +;
                           IIF(_WINDOWS,' ','')+'³ '+;
                           STR(EVALUATE(lcPurPrd),15,2)        +' ³ '+;
                           STR(EVALUATE(lcPayPrd),15,2)
  ENDFOR

  lnTotPurc  = apvenhst.nvnhpurch
  lnCashPay  = apvenhst.nvnhCasHp
  lnTotPay   = apvenhst.nvnhtotpa
  lnAvgPurc  = lnAvgPurc / INT(VAL(laFiscYear[lnYeArPos,3]))
  lnAvgPay   = lnAvgPay  / INT(VAL(laFiscYear[lnYeArPos,3]))
  lnPrintChe = apvenhst.nvnhpchkp
  lnManulChe = apvenhst.nvnhmchkp
  lnNonChecks= apvenhst.nvnhnchkp
  lnCredCard = apvenhst.nvnhccpay
  lnOfferd   = apvenhst.nvnhdisof
  lnTaken    = apvenhst.nvnhdistkn
  lnApplDeb  = apvenhst.nvnhdmapp
  lnApplAdj  = apvenhst.nvnhadj
  SHOW GET laData[45]  
  SHOW GET laData[46]    
ELSE
  IF WVISIBLE('CWDAPVENHD')
    *** MESSAGE : "No vendor history for fiscal year ð...! "
    ***           ®  OK  ¯
    =gfModalGen("TRM04018B00000","DIALOG",lcFisYear)
  ENDIF  

  STORE 0 TO lnTotPurc , lnCashPay , lnTotPay   , lnAvgPurc , lnAvgPay ,;
             lnPrintChe, lnManulChe, lnNonChecks, lnCredCard, lnOfferd ,;
             lnTaken   , lnApplDeb , lnApplAdj   
  laHistory   = ''
ENDIF

IF llDispPop
  SHOW GET lnTotPurc  
  SHOW GET lnCashPay
  SHOW GET lnTotPay   
  SHOW GET lnAvgPurc
  SHOW GET lnAvgPay
  SHOW GET lnPrintChe 
  SHOW GET lnManulChe 
  SHOW GET lnNonChecks
  SHOW GET lnCredCard 
  SHOW GET lnOfferd   
  SHOW GET lnTaken    
  SHOW GET lnApplDeb  
  SHOW GET lnApplAdj  
  SHOW GET laData[45]  
  SHOW GET laData[46]    
  SHOW GET lcHistory
  SHOW GET lcYearName
  *B800275,1 M.H 11/01/95  Fix the history screen because when we change
  *B800275,1 M.H 11/01/95  the fiscal year the history screen disapeare 
  *B800275,1 M.H 11/01/95  behind the vendor screen.
  IF _DOS
    =lfRefresh("CWDAPVENHD")    && refresh says filed on the screen
  ENDIF
  *B800275,1 M.H End.
ENDIF
  
SELECT APVENDOR

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr

SELECT APVENDOR
laData[32] = IIF(EMPTY(STRTRAN(STRTRAN(laData[32],'-'),'0')),'',laData[32])
laData[33] = IIF(EMPTY(STRTRAN(STRTRAN(laData[33],'-'),'0')),'',laData[33])
laData[47] = IIF(EMPTY(STRTRAN(STRTRAN(laData[47],'-'),'0')),'',laData[47])
laData[41] = IIF(laData[40] = 'L',0.0,laData[41])
laData[38] = STR(lnPriority,1)

IF laScrMode[4]     
  APPEND BLANK
  GATHER FROM laData FIELDS &lcScFields MEMO
  
  *E300872,4 AMM start, Update the cmtrace file with the added record.
  =gfTraceKey('APVENDOR',APVENDOR.cVendCode,'A')
  *E300872,4 AMM end
  =gfAdd_Info('APVENDOR')

  SELECT APVENHST
  FOR lnCounter = 1 TO ALEN(laFiscYear,1) 
    IF SUBSTR(laFiscYear[lnCounter,2],1) <> 'H'
      INSERT INTO &gcDataDir.APVENHST   ;
      (cVendCode  ,cFisfYear) ;
      VALUES                  ;
      (laData[1]  ,laFiscYear[lnCounter,1])
    *E300872,4 AMM start, update the cmtrace file with the added record.
    =gfTraceKey('APVENHST',laData[1] + laFiscYear[lnCounter,1],'A')
    *E300872,4 AMM end
    =gfAdd_Info('APVENHST')      
    ENDIF 
  ENDFOR
ELSE    
  *** refresh the unmodifyable data
  IF gfRLock("APVENDOR",.T.) 
    laData[22] = apvendor.nvenbal
    laData[23] = apvendor.dvenlpurd
    laData[24] = apvendor.nvenlpura
    laData[25] = apvendor.nvenopndr
    laData[26] = apvendor.dvenlpord
    laData[27] = apvendor.nvenlpora
    laData[28] = apvendor.nvenopnpo
    laData[29] = apvendor.dvenlpayd
    laData[30] = apvendor.nvenlpaya
    laData[31] = apvendor.nven1099b 
    laData[48] = apvendor.cvenlpayn     
  
    GATHER FROM laData FIELDS &lcScFields MEMO

    *E300872,4 AMM start, Update the cmtrace file with the modified record.
    =gfTraceKey('APVENDOR',APVENDOR.cVendCode,'M')
    *E300872,4 AMM end
    
    =gfAdd_Info('APVENDOR')
    =gfObj_Lock(.F.)     
    =gfRLock("APVENDOR",.F.)     

  ELSE
    llcSave = .F.  
    RETURN
  ENDIF  
ENDIF

*C124836,1 MHM 11/25/2004 Custom auto create material PO from Sales Order[Start]
IF ASCAN(laEvntTrig,'SAVEMAIL') <> 0
  =gfDoTriger('APVENDR','SAVEMAIL')
ENDIF
*C124836,1 MHM 11/25/2004 [End]

*E301436,1 (Start) Save the contacts to the master file.

IF USED(lcContFile)
  GO TOP IN (lcContFile)
  IF SEEK(&lcContFile..cContType+&lcContFile..cCont_Id,'CONTACT')
    SELECT Contact
    DELETE REST WHILE gfTraceKey('CONTACT',cContType+cCont_Id+Store+Contact,'D') AND cContType+cCont_Id+Store +Contact = "V" + &lcContFile..cCont_Id
  *B606434,1 ALB Fix the contact screen [Begin]
  ELSE
    SELECT Contact
    =SEEK("V" + laData[1])
    DELETE REST WHILE gfTraceKey('CONTACT',cContType+cCont_Id+Store+Contact,'D') AND cContType+cCont_Id+Store +Contact = "V" + laData[1]
  *B606434,1 ALB Fix the contact screen [end]
  ENDIF
  SELECT (lcContFile)
  SCAN  FOR cContType+cCont_Id+Store + Contact = "V" + laData[1] 
    SCATTER MEMVAR MEMO 
    SELECT CONTACT
    APPEND BLANK    
    GATHER MEMVAR MEMO
    =gfTraceKey('CONTACT',m.cContType+m.cCont_Id+m.Store+m.Contact,'A')
  ENDSCAN
ENDIF
llContChng=.F. && reset the flag after saving.
*E301436,1 (End) 
*B606434,1 ALB Fix the contact screen [Begin]
IF USED(lcContRole)
  GO TOP IN (lcContRole)
  IF SEEK("V" + laData[1],'Controle')
    SELECT Controle
    DELETE REST WHILE gfTraceKey('Controle',cContType+cCont_Id+Store+Contact,'D') AND;
    cContType+cCont_Id+Store +Contact = "V" + laData[1]
  ENDIF
 
  SELECT (lcContRole)
  SCAN  FOR cContType+cCont_Id+Store + Contact = "V" + laData[1] AND !INLIST(cmark,'D')
    SCATTER MEMVAR MEMO 
    SELECT Controle
    APPEND BLANK    
    GATHER MEMVAR MEMO
    =gfTraceKey('Controle',m.cContType+m.cCont_Id+m.Store+m.Contact,'A')
  ENDSCAN  
  ERASE(gcWorkDir+lcContRole)
ENDIF
*B606434,1 ALB Fix the contact screen [end]

SELECT APVENDOR

IF llFromInvo
  CLEAR READ
  glQuitting = .T.
  RETURN .T.
ENDIF

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
*
PROCEDURE lpDelScr

IF laData[22] > 0 
  *** MESSAGE : "You cannot delete this vendor. ð."
  ***           ®  OK  ¯
  lcMessage = 'Vendor current balance is greater than zero'
  =gfModalGen("TRM04019B00000","DIALOG",lcMessage)  
  RETURN
ENDIF  

IF laData[28] > 0 
  *** MESSAGE : "You cannot delete this vendor. ð."
  ***           ®  OK  ¯
  lcMessage = 'The vendor has open P/Os'
  =gfModalGen("TRM04019B00000","DIALOG",lcMessage)  
  RETURN
ENDIF  
*E300824,1 AMM Set the order in case of AP installed.
*SET ORDER TO TAG VENDINV IN APINVHDR 
*B801867,1 AMM Open file
*IF llApInst 
  *SET ORDER TO TAG VENDINV IN APINVHDR 
*ENDIF

*B803920,1 AME [Start]
*llInvHst =gfOpenFile(gcDataDir+'ApInvHst' ,'VENDINV','SH') 
IF llApInst
  llInvHst =gfOpenFile(gcDataDir+'ApInvHst' ,'VENDINV','SH') 
ENDIF
*B803920,1 AME [End]

*B801867,1 AMM end

*E300824,1 AMM Check first that AP installed.
*IF SEEK(laData[1],'APINVHDR') .OR. SEEK(laData[1],'APINVHST')
IF llApInst .AND. (SEEK(laData[1],'APINVHDR') .OR. SEEK(laData[1],'APINVHST'))
*E300824,1 AMM end
  *** MESSAGE : "You cannot delete this vendor. ð."
  ***           ®  OK  ¯
  lcMessage = 'The vendor has invoices'
  =gfModalGen("TRM04019B00000","DIALOG",lcMessage)  
  SET ORDER TO TAG INVVEND IN APINVHDR 
    
  *B801867,1 AMM Close file
  IF USED('ApInvHst') .AND. llInvHst 
    USE IN ApInvHst
  ENDIF
  *B801867,1 AMM end

  *khalid
  SELECT APVENDOR
  *khalid

  RETURN
ENDIF

*E300824,1 AMM Check for invoices only in case of AP installed.
IF llApInst
*E300824,1 AMM end
  *B801867,1 AMM Open file
  *SET ORDER TO TAG INVVEND IN APINVHDR 
  *SELECT APINVAHD
  llInvAhd =gfOpenFile(gcDataDir+'ApInvAhd' ,'HTYPCOD','SH')
  *B801867,1 AMM end
  LOCATE FOR APINVAHD.cVendCode = laData[1]
  IF FOUND()
    *** MESSAGE : "You cannot delete this vendor. ð."
    ***           ®  OK  ¯
    lcMessage = 'The vendor has recurring invoices'
    =gfModalGen("TRM04019B00000","DIALOG",lcMessage)  
    *B801867,1 AMM  Close file
    IF USED('ApInvAhd') .AND. llInvAhd
      =gfCloseFile('ApInvAhd')
    ENDIF
    *B801867,1 AMM end
  
    *khalid
    SELECT APVENDOR
    *khalid

    RETURN
  ENDIF
*E300824,1 AMM start
ENDIF
*E300824,1 AMM end

SELECT APVENHST
DELETE FOR APVENHST.cVendCode = laData[1]

*E300872,4 AMM start, Update the cmtrace file with the deleted record.
=gfTraceKey('APVENHST', laData[1], 'D')
*E300872,4 AMM end


SELECT APVENDOR
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE 

*E300872,4 AMM start, Update the cmtrace file with the deleted record.
=gfTraceKey('APVENDOR', laData[1], 'D')
*E300872,4 AMM end
*B801867,1 AMM Close files
IF USED('ApInvHst') .AND. llInvHst 
  USE IN ApInvHst
ENDIF
IF USED('ApInvAhd') .AND. llInvAhd
  =gfCloseFile('ApInvAhd')
ENDIF
*B801867,1 AMM end

laScrMode        = .F.
laScrMode[1]     = .T.

*!**************************************************************************
*!
*!      Function: lfDispAcsDes
*!
*!**************************************************************************
*Get Account description calling from view mode.
*
FUNCTION lfDispAcsDes

*E300824,1 AMM Check if AP installed before selecting APSETUP.
IF llApInst
*E300824,1 AMM end
  SELECT APSETUP
  GO TOP
*E300824,1 AMM start
ENDIF
*E300824,1 AMM end

IF EMPTY(laData[32])
  laData[32]  = lcEmptyAcc
  lcExAcDes   = ''
ELSE  
  *E300824,1 AMM Check first if AP installed.
  *lcExAcDes = IIF(APSETUP.CAPSGLLINK = 'Y',ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,laData[32],lcLinkChar.CACCTCODE,"ACCTCODE")),' ')
  lcExAcDes = IIF(llApInst .AND. APSETUP.CAPSGLLINK = 'Y',ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,laData[32],lcLinkChar.CACCTCODE,"ACCTCODE")),' ')
  *E300824,1 AMM
ENDIF  

IF EMPTY(laData[33])
  laData[33] = lcEmptyAcc
  lcApAcDes  = ''
ELSE  
  *E300824,1 AMM Check first if AP installed.
  *lcApAcDes = IIF(APSETUP.CAPSGLLINK = 'Y',ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,laData[33],lcLinkChar.CACCTCODE,"ACCTCODE")),' ')
  lcApAcDes = IIF(llApInst .AND. APSETUP.CAPSGLLINK = 'Y',ALLTRIM(LOOKUP(lcLinkChar.CACCNLDES,laData[33],lcLinkChar.CACCTCODE,"ACCTCODE")),' ')
  *E300824,1 AMM end
  ENDIF  

IF EMPTY(laData[47])
  laData[47]  = lcEmptyAcc
ENDIF  

SHOW GET lcExAcDes
SHOW GET lcApAcDes
SELECT APVENDOR

*!**************************************************************************
*!
*!      Function: lfvNextWind
*!
*!**************************************************************************
*
FUNCTION lfvNextWind

lnWindPos = ASUBSCRIPT(laWindName,ASCAN(laWindName,lcNextWin),1)
lcWindow  = laWindName[lnWindPos,1]
lcNextWin = IIF(lnWindPos=5,laWindName[1,1],laWindName[lnWindPos+1,1])

IF lnWindPos > 0 .AND. lcExisWind <> laWindName[lnWindPos,2]
  =lfRefresh()    && refresh says filed on the screen
  lcObjName = 'ibWindow'  + STR(lnWindPos,1)
  lcNextBut = 'pbNextWin' + STR(lnWindPos,1)

  IF laScrMode[2]
    SHOW GET (lcObjName) ENABLE
    _CUROBJ = OBJNUM(&lcNextBut)
  ELSE
    SHOW GETS WINDOW (laWindName[lnWindPos,2])  ENABLE  ONLY
    _CUROBJ = OBJNUM(&lcObjName) + 1
  ENDIF  

  SHOW GET (lcNextBut),1 PROMPT lcNextWin 
  SHOW GETS WINDOW (lcExisWind) DISABLE ONLY 
  lcExisWind = laWindName[lnWindPos,2]
ELSE
  _CUROBJ = _CUROBJ
ENDIF

IF laScrMode[3] .OR. laScrMode[4]
  IF lnWindPos = 3 .AND. laData[40] = 'T' .AND. lcExisWind = laWindName[3,2]
    SHOW GET laData[41] ENABLE 
  ELSE
    SHOW GET laData[41] DISABLE
  ENDIF
  IF lnWindPos = 2 .AND. !EMPTY(laData[34]) .AND. lcExisWind = laWindName[2,2]
    SHOW GET ibCheck    ENABLE 
    SHOW GET laData[36] ENABLE 
  ELSE
    SHOW GET ibCheck    DISABLE
    SHOW GET laData[36] DISABLE
  ENDIF  
ELSE
  IF lnWindPos = 3 .AND. laData[40] = 'T' .AND. lcExisWind = laWindName[3,2]
    SHOW GET laData[41] DISABLE
  ENDIF
  IF lnWindPos = 2 .AND. !EMPTY(laData[34]) .AND. lcExisWind = laWindName[2,2]
    SHOW GET ibCheck    DISABLE
    SHOW GET laData[36] DISABLE
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function : lfBrowse
*!
*!**************************************************************************
*
FUNCTION lfBrowse
PRIVATE lcClrSchm

lcClrSchm = IIF(_DOS," COLOR SCHEME 13","NOMENU")

SELECT(lcTempInv)

lcBrwStr = "CINVNO   :R:H='Inv. no.',"               +;
           "DINVDATE :R:H='Inv. date',"              +;
           "CINVREF  :R:H='Reference',"              +;
           "CINVREMIT = IIF(CINVREMIT = 'V','Vendor',IIF(CINVREMIT = 'F','Factor','Other')) :6 :R :H='Remit',"+;
           "CVENPMETH = IIF(CVENPMETH = 'P',"        +;
           "'Printed checks',IIF(CVENPMETH = 'M',"   +;
           "'Manual checks',IIF(CVENPMETH  = 'N',"   +;
           "'Non check payments','Cash payment'))) :R :H='Payment method',"+;
           "NINVDISOF :R:H='Disc. Offerd',"          +;
           "NINVAMTAP :R:H='Appr. to pay',"          +;
           "NINVDISAP :R:H='Disc. appr.'"
*E300258,4 MAN Add the check on supressing 1099 processing           
*B601992,1 Fix lcBrwStr1 in case lAps1099 is .T.
*lcBrwStr1= "NINVADJAP :R:H='Adj. appr.',"            +;
           "CVENPRIOR :R:H='Payment priority',"      +;
           "NTERDUED  :R:H='Net due days',"          +;
           "NTERDISCD :R:H='Disc. days',"            +;
           "NTERDISCR :R:H='Disc. percent',"         +;
           "DINVDUDAT :R:H='Due date',"              +;
           "CBNKCODE  :R:H='Bank code',"             +;
           "CCHKACCT  :R:H='Bank checking account'," +;
           "CCHKGLACC :R:H='GL checking account',"   +;
           "CCHKNO    :R:H='Check number',"          +;
           "DCHKDATE  :R:H='Check date',"            +;
           "CVENCCVEN :R:H='Credit card vendor',"    +;
           "CVENCCINV :R:H='Credit card invoice',"   +;
           "CAPACCT   :R:H='AP account',"            +;
           "NTOTAL    :R:H='Total',"                 +;
           IIF(llApS1099,"","N1099     :R:H='1099 amount',")           +;
           "NPAID     :R:H='Amount paid',"           +;
           "NADJUST   :R:H='Adj. applied',"          +;
           "NDISCOUNT :R:H='Disc. taken',"           +;
           "NINVPAID  :R:H='Total paid',"            +;
           "NINVDISTK :R:H='Total Discount',"        +;
           "NINVADJ   :R:H='Total Adjustment',"      +;
           IIF(llApS1099,"", "NINV1099A :R:H='Total 1099 amount'")

lcBrwStr1= "NINVADJAP :R:H='Adj. appr.',"            +;
           "CVENPRIOR :R:H='Payment priority',"      +;
           "NTERDUED  :R:H='Net due days',"          +;
           "NTERDISCD :R:H='Disc. days',"            +;
           "NTERDISCR :R:H='Disc. percent',"         +;
           "DINVDUDAT :R:H='Due date',"              +;
           "CBNKCODE  :R:H='Bank code',"             +;
           "CCHKACCT  :R:H='Bank checking account'," +;
           "CCHKGLACC :R:H='GL checking account',"   +;
           "CCHKNO    :R:H='Check number',"          +;
           "DCHKDATE  :R:H='Check date',"            +;
           "CVENCCVEN :R:H='Credit card vendor',"    +;
           "CVENCCINV :R:H='Credit card invoice',"   +;
           "CAPACCT   :R:H='AP account',"            +;
           "NTOTAL    :R:H='Total',"                 +;
           IIF(llApS1099,"","N1099     :R:H='1099 amount',")           +;
           "NPAID     :R:H='Amount paid',"           +;
           "NADJUST   :R:H='Adj. applied',"          +;
           "NDISCOUNT :R:H='Disc. taken',"           +;
           "NINVPAID  :R:H='Total paid',"            +;
           "NINVDISTK :R:H='Total Discount',"        +;
           "NINVADJ   :R:H='Total Adjustment'"      +;
           IIF(llApS1099,"", ",NINV1099A :R:H='Total 1099 amount'")
*B601992,1 end

BROWSE FIELDS &LcBrwStr+&lcBrwStr1;
           WINDOW  APVENPA2 IN WINDOW APVENPAY ;
           LOCK 0   ;
           NOAPPEND ;
           NOCLEAR  ;
           NODELETE ;
           NOWAIT   ;
           SAVE     ;
           TITLE lcBrowsTtl;
           NOEDIT &lcClrSchm

*!**************************************************************************
*!
*!      Function : lfGetAddress
*!
*!**************************************************************************
*
FUNCTION lfGetAddress
PARAMETERS lcAddrCode
           
IF SEEK(lcAddrCode,'SYCINT')
**B600599,1 stop the fields width adjusting
*  laData[8]  = SUBSTR(laData[8] +SPACE(20),1,SYCINT.NPART3LEN)
*  laData[9]  = SUBSTR(laData[9] +SPACE(10),1,SYCINT.NPART4LEN)
*  laData[10] = SUBSTR(laData[10]+SPACE(10),1,SYCINT.NPART5LEN)

*B601602,1 Add this lines to adjust the address fields width [Begin] 
  laData[6]  = SUBSTR(laData[6]  + SPACE(30) , 1 , SYCINT.NPART1LEN)
  laData[7]  = SUBSTR(laData[7]  + SPACE(30) , 1 , SYCINT.NPART2LEN)
  laData[8]  = SUBSTR(laData[8]  + SPACE(30) , 1 , SYCINT.NPART3LEN)
  laData[9]  = SUBSTR(laData[9]  + SPACE(30) , 1 , SYCINT.NPART4LEN)
  laData[10] = SUBSTR(laData[10] + SPACE(10) , 1 , SYCINT.NPART5LEN)
  *B603585,1 Get the Country DEscription AME[Start] 
  *laData[11] = SUBSTR(laData[11] + SPACE(20) , 1 , SYCINT.NPART6LEN)
  laData[11] = SYCINT.Ccont_Desc
  *B603585,1 AME[End]
*B601602,1 Add this lines [End] 

  lcAddHed1  = SYCINT.CPART1LAB
  lcAddHed2  = SYCINT.CPART2LAB
  lcAddHed3  = SYCINT.CPART3LAB
  lcAddHed4  = SYCINT.CPART4LAB
  lcAddHed5  = SYCINT.CPART5LAB
  lcAddHed6  = SYCINT.CPART6LAB
  SHOW GET laData[8] 
  SHOW GET laData[9] 
  SHOW GET laData[10] 

*B601602,1 Add this lines to adjust the address fields width [Begin] 
  SHOW GET laData[6] 
  SHOW GET laData[7] 
  SHOW GET laData[11] 
*B601602,1 Add this lines [End] 

ENDIF

*!**************************************************************************
*!
*!      Function : lfvAdFormat
*!
*!**************************************************************************
*
FUNCTION lfvAdFormat


*!**************************************************************************
*!
*!      Function : lfwVndr
*!
*!**************************************************************************
*B600345,1 Move the window when clause to the PRG from the screen
*

FUNCTION lfwVndr
*B600345,1  ADD the (TYPE("laScrMode[1]")="U" )  expression so the other side would not be evaluated it laScrMode is released

*B605547,1 ALB Changing the hot key [Begin]
*E301119,1 Change bar Inquiry to be Options [Begin]
*DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT '\<Inquiry'  SKIP FOR (TYPE("laScrMode[1]")="U" .OR. (laScrMode[1] .OR. laScrMode[4])) KEY "ALT+I" 
*DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT '\<Options'  SKIP FOR (TYPE("laScrMode[1]")="U" .OR. (laScrMode[1] .OR. laScrMode[4])) KEY "ALT+I" 
DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'Opt\<ions'  SKIP FOR (TYPE("laScrMode[1]")="U" .OR. (laScrMode[1] .OR. laScrMode[4])) KEY "ALT+I"
*E301119,1 Change bar Inquiry to be Options [End  ]
*B605547,1 ALB Changing the hot key [end]

DEFINE POPUP _INQURYPOP MARGIN SHADOW
*E300824,1 AMM Disable the Payables and Payments bars of the Inquiry menu.
*DEFINE BAR 1 OF _INQURYPOP PROMPT IIF(llFromInvo,'\Payables','\<Payables') && SKIP FOR (laScrMode[1] .OR. laScrMode[4])
*DEFINE BAR 2 OF _INQURYPOP PROMPT 'Pay\<ments' && SKIP FOR (laScrMode[1] .OR. laScrMode[4])
DEFINE BAR 1 OF _INQURYPOP PROMPT IIF(llFromInvo,'\Payables','\<Payables') SKIP FOR !llApInst
DEFINE BAR 2 OF _INQURYPOP PROMPT 'Pay\<ments'  SKIP FOR !llApInst
*E300824,1 AMM end

*B605547,1 ALB Enabeling the bars [Begin]
*DEFINE BAR 3 OF _INQURYPOP PROMPT '\Orders'   && SKIP FOR (laScrMode[1] .OR. laScrMode[4])
*DEFINE BAR 4 OF _INQURYPOP PROMPT '\Receipts' && SKIP FOR (laScrMode[1] .OR. laScrMode[4])
DEFINE BAR 3 OF _INQURYPOP PROMPT '\<Orders'   && SKIP FOR (laScrMode[1] .OR. laScrMode[4])
DEFINE BAR 4 OF _INQURYPOP PROMPT '\<Receipts' && SKIP FOR (laScrMode[1] .OR. laScrMode[4])
*B605547,1 ALB Enabeling the bars [end]

*B607422,1 ALB Add Matireal Order to Vendor option menun [BEGIN]
ON BAR 3 OF _INQURYPOP ACTIVATE POPUP Orders
DEFINE POPUP Orders MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF orders PROMPT "Style PO"
DEFINE BAR 2 OF orders PROMPT "Material PO"
ON SELECTION POPUP orders DO lpvOrders

ON BAR 4 OF _INQURYPOP ACTIVATE POPUP receipts
DEFINE POPUP receipts MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF receipts PROMPT "Style PO"
DEFINE BAR 2 OF receipts PROMPT "Material PO"
ON SELECTION POPUP receipts DO lpvReceipts
*B607422,1 ALB Add Matireal Order to Vendor option menun [END]

DEFINE BAR 5 OF _INQURYPOP PROMPT '\<History'  && SKIP FOR (laScrMode[1] .OR. laScrMode[4])
*E301119,1 MAB 10/31/1999 Add salutation screen to menu options. [Begin]
DEFINE BAR 6 OF _INQURYPOP PROMPT '\<Salutations' SKIP FOR laScrMode[4]
*E301119,1 MAB 10/31/1999 Add salutation screen to menu options. [Begin]

*C124836,1 MHM 11/25/2004 update mail for vendor[Start]
IF ASCAN(laEvntTrig,'APVENMAL') <> 0
  =gfDoTriger('APVENDR','APVENMAL')
ENDIF
*C124836,1 MHM 11/25/2004 [End]

ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP
ON SELECTION POPUP _INQURYPOP DO lpvInquiry

RETURN glReadWhen

*!**************************************************************************
*!
*!      Procedure: lfwData_49
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
PROCEDURE lfwData_49

lcOldCurr = laData[49]

*!**************************************************************************
*!
*!      Procedure: lfvData_49
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
PROCEDURE lfvData_49
*E300824,1 AMM start, Add this variable to detect if this function opened 
*E300824,1 AMM the file. 
PRIVATE llOpen
llOpen = .F.
*E300824,1 AMM start, Open the file if it is not opened.
IF !USED('SYCCURR')
  =gfOpenFile(gcSYSHome+'SYCCURR' ,'Ccurrcode','SH')
  llOpen = .T.
ENDIF
*E300824,1 AMM end

*B038431,1 NNA 08/23/2004 (Begin) Seek for Alltrim of the Currency Code
*IF llBrowse .OR. !SEEK(laData[49],'SYCCURR') .OR. ATC("?",laData[49]) > 0 .AND. LASTKEY() = 13
IF llBrowse .OR. !SEEK(ALLTRIM(laData[49]),'SYCCURR') .OR. ATC("?",laData[49]) > 0 .AND. LASTKEY() = 13
*B038431,1 NNA (End)

  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    laData[49] = lcOldCurr
  ELSE
    laData[49] = laTemp[1]
  ENDIF
ENDIF
*E300824,1 AMM Close the file if it is opened by this function
IF llOpen
  =gfclosefile('SYCCURR')
ENDIF
*E300824,1 AMM end
SELECT APVENDOR
SHOW GET laData[49]

*!********************************************************************************
*! Name      : lfvContCod
*! Developer : Haytham El_Sheltawi
*! Date      : 02/16/1997
*! Purpose   : Valid function for the Invisible Button [ibAddress] 
*!*************************************************************
*! Calls     : lfvAddress()
*!*************************************************************
*! Called From  : Vendors screen [APVENDC0]
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvContCod()
*!*************************************************************
*B601602,1 This function was added for the bug by HS
*!********************************************************************************
*B801867,1 AMM This function is not used any ore, because we have already 
*B801867,1 AMM deleted that invisible button
FUNCTION lfvContCod

PRIVATE lcOldCode , lcOldDes , lcOldPup , llShrtFild
 
lcOldCode = laData[44]   && Save the old Country code
lcOldDes = laData[11]    && Save the old address line #6 
lcOldPup = lcAddHed1     && Save the old selection in the popup 
llShrtFild = .F.         && Flag to Know if the new address fields width will not halld the old data

=lfvAddress('LADATA[44]','LADATA[11]',4.417,1.125,10.417,23.125,'lcAddHed1')

*IF Statment to check if the user have changed the Country code 
IF laData[44] <> lcOldCode

  *IF Statment to check if the new Country code exist in the file 
  IF SEEK(laData[44],'SYCINT')
    llShrtFild = IIF(LEN(RTRIM(laData[6])) > SYCINT.NPART1LEN , .T. , llShrtFild)
    llShrtFild = IIF(LEN(RTRIM(laData[7])) > SYCINT.NPART2LEN , .T. , llShrtFild)
    llShrtFild = IIF(LEN(RTRIM(laData[8])) > SYCINT.NPART3LEN , .T. , llShrtFild)
    llShrtFild = IIF(LEN(RTRIM(laData[9])) > SYCINT.NPART4LEN , .T. , llShrtFild)
    llShrtFild = IIF(LEN(RTRIM(laData[10])) > SYCINT.NPART5LEN , .T. , llShrtFild)
    llShrtFild = IIF(LEN(RTRIM(laData[11])) > SYCINT.NPART6LEN , .T. , llShrtFild)
  ENDIF    && End of IF

  *IF Statment to check if the new address fields width will halld the 
  *   old data and if not display the message 
  *** MESSAGE : " Changing the country setting will eliminate some of "
  ***           " the characters in the address field(s).             "
  ***           "               < OK >         < Cancel >             "
  *   And check if the user Select cancel 
  IF llShrtFild .AND. gfModalGen('QRM04161B04004','Dialog','') = 2
    laData[44] = lcOldCode   && Restore the old value of the country code
    laData[11] = lcOldDes    && Restore the old value of the address line #5
    lcAddHed1 = lcOldPup     && Restore the old value of the popup
    SHOW GET laData[11]
    SHOW GET lcAddHed1
  ELSE     && Else
  =lfGetAddress(laData[44])   && Set the new address fields width and labels
  ENDIF      && End of IF
ENDIF      && End of IF


*!********************************************************************************
*! Name      : lfvVenSup
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 08/01/1998
*! Purpose   : Valid function for the Vendor supplies button [pbVenSup]
*! REFERENCE : *E300798,1
*! Notes     :
*!     Source array elements : 'Style'                  : 'S'
*!                             'Material'               : 'M'
*!                             'Contractor    Services' : 'C'
*!                             'Miscellaneous Services' : 'V'  
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Vendors screen [APVENDC0]
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvVenSup()
*!*************************************************************
FUNCTION lfvVenSup
PRIVATE lnPos, lnCount, lcExact
DIMENSION laSrce[4], laVSupp[1]
STORE SPACE(0) TO laVSupp, laSrce
*E300798,1 AMM Vendor supplier types source array.
*B801920,1 AMM Initialize
lnC = 1
*B801920,1 AMM end
FOR lnCount = 1 TO ALEN(laVenSupT,1)
  *B801920,1 AMM If the module of relevant to the option is not installed, skip it
  *laSrce[lnCount] = laVenSupT[lnCount,1]
  *B606681,1 ALB add service contractor if PO is also installed [Begin]
  *B802697,1 AMM Add 'Style' if PS installed
  *IF (laVenSupT[lnCount,2] = 'S' .AND. !('PO'$gcCmpModules) )  .OR. ;
     (laVenSupT[lnCount,2] = 'M' .AND. !('MA'$gcCmpModules) ) .OR.  ;
     (laVenSupT[lnCount,2] = 'C' .AND. !('MF'$gcCmpModules) )
  *IF (laVenSupT[lnCount,2] = 'S' .AND. !('PO'$gcCmpModules .OR. 'PS'$gcCmpModules)) .OR. ;
     (laVenSupT[lnCount,2] = 'M' .AND. !('MA'$gcCmpModules) ) .OR.  ;
     (laVenSupT[lnCount,2] = 'C' .AND. !('MF'$gcCmpModules) )
  IF (laVenSupT[lnCount,2] = 'S' .AND. !('PO'$gcCmpModules .OR. 'PS'$gcCmpModules)) .OR. ;
     (laVenSupT[lnCount,2] = 'M' .AND. !('MA'$gcCmpModules) ) .OR.  ;
     (laVenSupT[lnCount,2] = 'C' .AND. !(('PO'$gcCmpModules) OR ('MF'$gcCmpModules)))
  *B802697,1 AMM end
  *B606681,1 ALB add service contractor if SO installed not MF [end]
    LOOP
  ENDIF
  
  *B038431,1 NNA 08/23/2004 (Begin) Redim. the Array because if any module of (MA,MF,PO) not installed
  *B038431,1 NNA            we'll get a blank line at the Source Mover
  DIMENSION laSrce[lnC]
  *B038431,1 NNA (End)
  
  laSrce[lnC] = laVenSupT[lnCount,1]
  lnC = lnC + 1
  *B801920,1 AMM end
ENDFOR

laData[50] = ALLTRIM(laData[50])
*E300798,1 AMM Get the stored vendor supplier types .
lnCount = 0
lcExact = SET('EXACT')
SET EXACT ON
IF !EMPTY(laData[50])
  *E300798,1 AMM For each type in laData[50] search for its title in laVenSupT 
  *E300798,1 AMM array and put it in laVenSupT to display in the mover screen
  FOR lnC = 1 TO LEN(laData[50])
    lnPos =  ASCAN(laVenSupT,SUBSTR(laData[50],lnC,1))
    IF lnPos # 0

      *B038431,1 NNA 08/23/2004 Fill the Target Array with Only the Elements that have installed Modules
      IF (laVenSupT[lnPos] = 'S' .AND. !('PO'$gcCmpModules .OR. 'PS'$gcCmpModules)) .OR. ;
         (laVenSupT[lnPos] = 'M' .AND. !('MA'$gcCmpModules) ) .OR.  ;
         (laVenSupT[lnPos] = 'C' .AND. !(('PO'$gcCmpModules) OR ('MF'$gcCmpModules)))
        LOOP  
      ELSE
      *B038431,1 NNA (End)
      
        lnCount = lnCount + 1      
        DIMENSION laVSupp[lnCount]
        laVSupp[lnCount] = laVenSupT[lnPos-1]

      *B038431,1 (Begin)
      ENDIF 
      *B038431,1 (End)  

    ENDIF
  ENDFOR
ENDIF
SET EXACT &lcExact


*E300798,1 AMM Display the mover screen.

*B038431,1 NNA 08/23/2004 (Begin) Enable the Mover Buttons because they disabled all
*=gfMover(@laSrce,@laVSupp,"Vendor supplies...")
=gfMover(@laSrce,@laVSupp,"Vendor supplies...",.T.)
*B038431,1 NNA (End)

*E300798,1 AMM Collect the chosen types into laData[50]

laData[50]=SPACE(0)
lcExact = SET('EXACT')
SET EXACT ON
*E300798,1 AMM Fill laData[50] with the chosen vendor supplier types.
FOR lnCount = 1 TO ALEN(laVSupp)
  lnPos = ASCAN(laVenSupT,laVSupp[lnCount])
  IF lnPos # 0
    laData[50] = laData[50]+laVenSupT[lnPos+1]
  ENDIF
ENDFOR

*E300798,1 AMM If any vendor supplier type removed , remove its related 
*E300798,1 AMM invoice types from laData[51].
*B602789,1 AMM Initialize
llRmvI = .F.
*B602789,1 AMM end      
FOR lnCount = 1 TO ALEN(laVenSupT,1)
  lnPos = ASCAN(laVSupp,laVenSupT[lnCount,1])
  IF lnPos = 0
    DO CASE 
      *E300798,1 AMM If 'S' is not in APVENDOR.cVenSupTyp laData[50] ,
      *E300798,1 AMM Remove following items from the  the second mover source array
      *E300798,1 AMM Style P/O Shipments ,   Style P/Os  
      CASE laVenSupT[lnCount,2] = 'S'
        laData[51] = STRTRAN(laData[51],'S')
        *laData[51] = STRTRAN(laData[51],'I')
        *laData[51] = STRTRAN(laData[51],'R')     
        IF llRmvI
          laData[51] = STRTRAN(laData[51],'I')
          laData[51] = STRTRAN(laData[51],'R')     
        ELSE
          llRmvI = .T.
        ENDIF
        
     *E300798,1 AMM If 'M' is not in APVENDOR.cVenSupTyp laData[50],
     *E300798,1 AMM Remove the following items from the second mover source array
     *E300798,1 AMM Material P/Os  
      CASE laVenSupT[lnCount,2] = 'M'
        laData[51] = STRTRAN(laData[51],'L')
        laData[51] = STRTRAN(laData[51],'F')
      *E300798,1 AMM If 'C' is not in APVENDOR.cVenSupTyp
      *E300798,1 AMM Remove the following items from  the second mover source array 
      *E300798,1 AMM Cuting Tickets, Material Manufacturing Orders
      CASE laVenSupT[lnCount,2] = 'C'
        laData[51] = STRTRAN(laData[51],'M')
        laData[51] = STRTRAN(laData[51],'T')
        *B602789,1 AMM If vendor type "style" is removed
        IF llRmvI
          laData[51] = STRTRAN(laData[51],'I')
          laData[51] = STRTRAN(laData[51],'R')
        ELSE
          llRmvI = .T.
        ENDIF
        *B602789,1 AMM end
        
      *E300798,1 AMM If 'V' is not in  APVENDOR.cVenSupTyp laData[50]
      *E300798,1 AMM Remove the following items from the second mover source array
      *E300798,1 AMM Miscellaneous Expenses
      CASE laVenSupT[lnCount,2] = 'V'      
        laData[51] = STRTRAN(laData[51],'G')
    ENDCASE
  ENDIF
ENDFOR
SET EXACT &lcExact
*E300798,1 AMM If no vendor supplier type selected, uncheck the two check 
*E300798,1 AMM boxes and disable the vendor invoice type check box.
IF EMPTY(laData[50])
  cbVenSup  = .F.
  cbVenInvT = .F.
  lcVITStat = 'DISABLE'
  SHOW GET cbVenInvT DISABLE
  SHOW GET cbVenSup
ELSE
  lcVITStat = 'ENABLE'
  SHOW GET cbVenInvT ENABLE
  cbVenSup  = .T.
  SHOW GET cbVenSup
ENDIF
*E300798,1 AMM If no vendor invoice type selected, uncheck the check box.
IF EMPTY(laData[51])
  cbVenInvT = .F.
  SHOW GET cbVenInvT
ENDIF
*C102740,1 ALB Attach warehouse to the vendor [Begin]
IF 'C' $ laData[50]
 SHOW GET laData[53] ENABLED
 SHOW GET pbWareH ENABLED
ELSE
  STORE '' TO laData[53]
  SHOW GET laData[53]
  SHOW GET laData[53] DISABLED
  SHOW GET pbWareH DISABLED
ENDIF
*C102740,1 ALB Attach warehouse to the vendor [end]
*!********************************************************************************
*! Name      : lfvVenInv
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 08/01/1998
*! Purpose   : Valid function for the Vendor Invoice type button [pbVenInvT]
*! REFERENCE : *E300798,1
*! Notes     :
*!   Source array elements : Style P/O Shipments           : 'S'
*!                           Style P/Os                    : 'I'
*!                           Cuting Tickets                : 'M'
*!                           Material P/Os                 : 'L'
*!                           Material Manufacturing Orders : 'T'
*!                           Miscellaneous Expenses        : 'G' 
*!                           Returned Material P/O         : 'F'
*!                           Returned Style P/O            : 'R'
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Vendors screen [APVENDC0]
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvVenInv()
*!*************************************************************
FUNCTION lfvVenInv
PRIVATE lnCount, lcExact, lnPos0,lnPos1,lnPos,lnC

DIMENSION laSrce[1], laVInvt[1]
STORE SPACE(0) TO laSrce, laVInvT
laData[50] = ALLTRIM(laData[50])
laData[51] = ALLTRIM(laData[51])
lnCount = 0
*E300798,1 AMM Collect the stored vendor invoice types , related to vendor 
*E300798,1 AMM supplier types into array laSrce to display as source types 
*E300798,1 AMM in the mover screen

lcExact = SET('EXACT')
SET EXACT ON
IF !EMPTY(laData[50])
  FOR lnC = 1 TO LEN(laData[50])
    lnPos0 =  ASCAN(laVenSupT,SUBSTR(laData[50],lnC,1))
    IF lnPos0 # 0
      DO CASE
        CASE laVenSupT[lnPos0] = 'S'
          lnPos1 = ASCAN(laVenInvT,'S')
          IF lnPos1 # 0 
            lnCount = lnCount + 1      
            DIMENSION laSrce[lnCount]
            laSrce[lnCount] = laVenInvT[lnPos1-1]
          ENDIF
          lnPos1 = ASCAN(laVenInvT,'I')
          *B602789,1 AMM If the setting is not exist in the source array, add it.
          *IF lnPos1 # 0 
          IF lnPos1 # 0 .AND. ASCAN(laSrce,laVenInvT[lnPos1-1]) = 0
          *B602789,1 AMM end
            lnCount = lnCount + 1      
            DIMENSION laSrce[lnCount]
            laSrce[lnCount] = laVenInvT[lnPos1-1]
          ENDIF
          lnPos1 = ASCAN(laVenInvT,'R')
          *B602789,1 AMM If the setting is not exist in the source array, add it.
          *IF lnPos1 # 0 
          IF lnPos1 # 0 .AND. ASCAN(laSrce,laVenInvT[lnPos1-1]) = 0
          *B602789,1 AMM end
            lnCount = lnCount + 1      
            DIMENSION laSrce[lnCount]
            laSrce[lnCount] = laVenInvT[lnPos1-1]
          ENDIF

        CASE laVenSupT[lnPos0] = 'M'
          lnPos1 = ASCAN(laVenInvT,'L')
          IF lnPos1 # 0 
            lnCount = lnCount + 1      
            DIMENSION laSrce[lnCount]
            laSrce[lnCount] = laVenInvT[lnPos1-1]
          ENDIF
          lnPos1 = ASCAN(laVenInvT,'F')
          IF lnPos1 # 0 
            lnCount = lnCount + 1      
            DIMENSION laSrce[lnCount]
            laSrce[lnCount] = laVenInvT[lnPos1-1]
          ENDIF

        CASE laVenSupT[lnPos0] = 'C'
          lnPos1 = ASCAN(laVenInvT,'M')
          IF lnPos1 # 0 
            lnCount = lnCount + 1      
            DIMENSION laSrce[lnCount]
            laSrce[lnCount] = laVenInvT[lnPos1-1]
          ENDIF
          lnPos1 = ASCAN(laVenInvT,'T')
          IF lnPos1 # 0 
            lnCount = lnCount + 1      
            DIMENSION laSrce[lnCount]
            laSrce[lnCount] = laVenInvT[lnPos1-1]
          ENDIF
          *B602789,1 AMM Add the two setting if the "contractor" type is chosed
          lnPos1 = ASCAN(laVenInvT,'I')
          IF lnPos1 # 0 .AND. ASCAN(laSrce,laVenInvT[lnPos1-1]) = 0
            lnCount = lnCount + 1      
            DIMENSION laSrce[lnCount]
            laSrce[lnCount] = laVenInvT[lnPos1-1]
          ENDIF
          lnPos1 = ASCAN(laVenInvT,'R')
          IF lnPos1 # 0 .AND. ASCAN(laSrce,laVenInvT[lnPos1-1]) = 0
            lnCount = lnCount + 1      
            DIMENSION laSrce[lnCount]
            laSrce[lnCount] = laVenInvT[lnPos1-1]
          ENDIF
          *B602789,1 AMM end
          
        CASE laVenSupT[lnPos0] = 'V'
          lnPos1 = ASCAN(laVenInvT,'G')
          IF lnPos1 # 0 
            lnCount = lnCount + 1      
            DIMENSION laSrce[lnCount]
            laSrce[lnCount] = laVenInvT[lnPos1-1]
          ENDIF
      ENDCASE
    ENDIF
  ENDFOR
ENDIF
SET EXACT &lcExact


*E300798,1 AMM Collect the stored vendor invoice types into array laVInvT[] 
*E300798,1 AMM to display as destination types in the mover screen
lnCount = 0
lcExact = SET('EXACT')
SET EXACT ON
IF !EMPTY(laData[51])
  FOR lnC = 1 TO LEN(laData[51])
    lnPos =  ASCAN(laVenInvT,SUBSTR(laData[51],lnC,1))
    IF lnPos # 0
      lnCount = lnCount + 1      
      DIMENSION laVInvT[lnCount]
      laVInvT[lnCount] = laVenInvT[lnPos-1]
    ENDIF
  ENDFOR
ENDIF
SET EXACT &lcExact

*E300798,1 AMM Display the mover screen.
=gfMover(@laSrce,@laVInvT,"Record Original Invoice Details for...")

*E300798,1 AMM Store the chosen types into laData[51]
laData[51]=SPACE(0)
*E300798,1 AMM Fill laData [51] with the vendor invoice types chosen by the mover screen
lcExact = SET('EXACT')
SET EXACT ON
FOR lnCount = 1 TO ALEN(laVInvT)
  lnPos = ASCAN(laVenInvT,laVInvT[lnCount])
  IF lnPos # 0
    laData[51] = laData[51]+laVenInvT[lnPos+1]
  ENDIF
ENDFOR
SET EXACT &lcExact
IF EMPTY(laData[51])
  cbVenInvT = .F.
ELSE
  cbVenInvT = .T.
ENDIF
SHOW GET cbVenInvT

*!*************************************************************
*! Name      : lfvDiv
*! Developer : Ahmed Mohamed Ibrahim(AMM)
*! Date      : 08/31/1998
*! Purpose   : Valid function of the division popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDiv()
*!*************************************************************

FUNCTION lfvDiv

*-- Validate the header division.
laData[12] = laDiv[lnDiv,2]


*!*************************************************************
*! Name      : lfvTerms
*! Developer : Ahmed Mohamed Ibrahim(AMM)
*! Date      : 08/31/1998
*! Purpose   : Valid function of the Terms popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvTerms()
*!*************************************************************

FUNCTION lfvTerms
*-- Validate the header division.
laData[35] = laTermCode[lnTermCode,2]
*B605942,1 ALB 06/30/2002 [Start] Refresh payment methode fields
=gfRltFld(laData[35] , @laTermAry , 'CTERMCODE')
SHOW GET lnDueDays
SHOW GET lnDiscDays
SHOW GET lnDisPerc
*B605942,1 ALB 06/30/2002 [End] Refresh payment methode fields

*!*************************************************************
*! Name      : lfv1099T
*! Developer : Ahmed Mohamed Ibrahim(AMM)
*! Date      : 02/17/99
*! Purpose   : Valid function of the Vendor 1099 Type popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfv1099T()
*!*************************************************************

FUNCTION lfv1099T

*-- Validate the header division.
laData[42] = la1099T[ln1099T,2]

*!*************************************************************
*! Name      : lfwContpop
*! Developer : Ahmed Mohamed Ibrahim(AMM)
*! Date      : 03/23/99
*! Purpose   : When function of the country addresses popup.
*! Reference : *B801867,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwContpop()
*!*************************************************************
FUNCTION lfwContpop

IF ASCAN(laOldAdd,laData[44])-1 > 0
  lcAddHed = SUBSTR(laOldAdd[ASCAN(laOldAdd,laData[44])-1],2)
ENDIF
SHOW GET lcAddHed

*!*************************************************************
*! Name      : lfvContpop
*! Developer : Ahmed Mohamed Ibrahim(AMM)
*! Date      : 03/23/99
*! Purpose   : Valid function of the country addresses popup.
*! Reference : *B801867,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvContpop()
*!*************************************************************
FUNCTION lfvContpop
llShrtFild = .F.
*B801867,1 AMM IF Statment to check if the user have changed the Country code 
IF laData[44] # laOldAdd[ASCAN(laOldAdd,'*'+lcAddHed)+1]

  *B801867,1 AMM IF Statment to check if the new Country code exist in the file 
  IF SEEK(laOldAdd[ASCAN(laOldAdd,'*'+lcAddHed)+1],'SYCINT')
  
    llShrtFild = LEN(RTRIM(laData[6]))  > SYCINT.NPART1LEN .OR. ;
                 LEN(RTRIM(laData[7]))  > SYCINT.NPART2LEN .OR. ;
                 LEN(RTRIM(laData[8]))  > SYCINT.NPART3LEN .OR. ;
                 LEN(RTRIM(laData[9]))  > SYCINT.NPART4LEN .OR. ;
                 LEN(RTRIM(laData[10])) > SYCINT.NPART5LEN .OR. ;
                 LEN(RTRIM(laData[11])) > SYCINT.NPART6LEN 
  ENDIF    && End of IF

  *IF Statment to check if the new address fields width will halld the 
  *   old data and if not display the message 
  *** MESSAGE : " Changing the country setting will eliminate some of "
  ***           " the characters in the address field(s).             "
  ***           "               < OK >         < Cancel >             "
  *   And check if the user Select cancel 
  laData[44] = laOldAdd[ASCAN(laOldAdd,'*'+lcAddHed)+1]
  
  IF llShrtFild .AND. gfModalGen('QRM04161B04004','Dialog','') = 2
    laData[44] = lcOldCode   && Restore the old value of the country code
  ENDIF      && End of IF
  =lfGetAddress(laData[44])   && Set the new address fields width and labels
  =lfRefresh()
ENDIF      && End of IF
*B801867,1 AMM Get first addresses to display instead of the country desription
lcAddHed = lcAddHed1
_CUROBJ = _CUROBJ + 1
SHOW GET lcAddHed


*!**************************************************************************
*!
*!      Function: lfvData_52
*!
*!**************************************************************************
**E301303,1 AKA 
FUNCTION lfvData_52
IF laData[52] < 0 
  *** MESSAGE : "Negative values are not allowed. "
  **            ®  OK  ¯
  =gfModalGen("TRM04087B00000","DIALOG")
  laData[52] = lgOldValue
  SHOW GET laData[52]
ENDIF



*!*************************************************************
*! Name      : lfvpbCont
*! Developer : NAD
*! Date      : 07/01/2000
*! Purpose   : Browse Vendor Contacts Screen
*! Ref       :*E301436,1 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvpbCont()
*!*************************************************************

FUNCTION lfvpbCont
DO lfVendCont IN (gcapphome+'\APVCONT.PRG')          
SHOW GET laData[17] 

*!*************************************************************
*! Name      : 
*! Developer : NAD
*! Date      : 07/01/2000
*! Purpose   : Browse Vendor Contacts Screen
*! Ref       :*E301436,1 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvpbCont()
*!*************************************************************
FUNCTION lfDelTmp 
IF USED  (lcContFile)
  USE IN (lcContFile)
  ERASE (gcWorkDir+lcContFile+'.DBF')
  ERASE (gcWorkDir+lcContFile+'.CDX')
ENDIF  


*!**************************************************************************
*!
*!      Function: lfvUsrFld
*!
*!**************************************************************************
*C200178,1 AAN Function for update user field array with the vendor payment type.
FUNCTION lfvUsrFld
*-- Get position of packs in laUsrField
PRIVATE lnVenPYPs
lnVenPYPs = ASCAN(laUsrField,"CVENPYTYPE")
IF lnVenPYPs  > 0
  lnVenPYPs = ASUBSCRIPT(laUsrField,lnVenPYPs ,1)
  laUsrField[lnVenPYPs ,6] = 'A'
ENDIF

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Albert Raif
*! Date      : 10/12/2002
*! Purpose   : Browse Warehouse Screen
*! Ref       :*C102740,1 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOldValue()
*!*************************************************************
*!C102740,1 ALB Attach warehouse to the vendor
FUNCTION lfOldValue
lcOldValue = EVAL(SYS(18))
*!*************************************************************
*! Name      : lfvWareH
*! Developer : Albert Raif
*! Date      : 10/12/2002
*! Purpose   : Browse Warehouse Screen
*! Ref       :*C102740,1 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvWareH()
*!*************************************************************
*!C102740,1 ALB Attach warehouse to the vendor
FUNCTION lfvWareH

IF !llBrowse AND !EMPTY(lcOldValue) AND laData[53]=lcOldValue 
  RETURN
ENDIF
IF llBrowse OR !SEEK(laData[53],'WAREHOUS')
  laData[53]= gfBrowWare( .F.,.F.,.F.,.F.,.F.,'M' )
ENDIF
llBrowse = .F.





*!*************************************************************
*! Name      : lpvOrders
*! Developer : Albert Raif
*! Date      : 7/20/2003
*! Purpose   : On selection for orders option.
*! Ref       : B607422,1 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpvOrders
*!*************************************************************
*!B607422,1 ALB Add Matireal Order to Vendor option menun 
FUNCTION lpvOrders

DO CASE
  CASE BAR() = 1     && Style PO Orders

    IF !USED('POSHDR')
      =gfOpenFile(gcDataDir+'POSHDR' ,'POSHDRV','SH') 
    ENDIF
    SELECT POSHDR
    SET ORDER TO TAG POSHDRV
    IF SEEK(laData[1])
      DIMENSION laTemp[2]
      laTemp     = ''
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      lcFile_Ttl = "Orders"
      lcFltrExp  = "laData[1] FOR CSTYTYPE $ 'PRCD' "
      lcBrFields = "CSTYTYP = IIF(CSTYTYPE='D','Dye Order','Purchase Order')  :R:H= 'Type',"+;
                   "PO        :R:H= 'Order No.',"            +;
                   "ENTERED   :R:H= 'Entered Date',"         +;
                   "COMPLETE  :R:H= 'Complete Date',"        +;
                   "AVAILABLE :R:H= 'Available Date',"       +;
                   "NSTYORDER :R:H= 'Ordered',"              +;
                   "RECEIVE   :R:H= 'Received',"             +;
                   "DAMAGE    :R:H= 'Damaged',"              +;
                   "CANCEL    :R:H= 'Canceled',"             +;
                   "OPEN      :R:H= 'Open'" 

      =ARIABROW(lcFltrExp,lcFile_Ttl,20,.F.,20,.F.,.F.,.F.,'CSTYTYPE,PO','laTemp',.F.,.F.,.F.)
      lcBrFields = lcSavBrFld
      lcFile_Ttl = lcSavTitle
      
      IF !EMPTY(laTemp[1]) AND laTemp[1] = 'P'
        lcBaseWind = gcBaseWind
        =gfstatic()
        DO gpDoProg WITH 'AWRPOSTYLE', .F., 'PO' ,"'"+laTemp(1)+"','"+laTemp(2)+"'"
      ENDIF
    ELSE
      ** MESSAGE : " No ð found for vendor ð "
      **           "     ® Ok ¯              "
      =gfModalGen("TRM04042B00000","DIALOG","orders|"+ALLTRIM(ladata[1]))
    ENDIF  
    IF llAPINST
      SET ORDER TO TAG INVVEND    IN APINVHDR
    ENDIF  
*******************************************************
  CASE BAR() = 2     && Reception
    IF !USED('POFHDR')
      =gfOpenFile(gcDataDir+'POFHDR' ,'POFHDRV','SH') 
    ENDIF
    SELECT POFHDR
    SET ORDER TO TAG POFHDRV
    IF SEEK(laData[1])
      DIMENSION laTemp[2]
      laTemp     = ''
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      lcFile_Ttl = "Orders"
      lcFltrExp  = "laData[1] FOR CMATTYPE $ 'PRCD' "
      lcBrFields = "CMATTYP = 'MAT. Purchase Order' :R:H= 'Type',POMAT :R:H= 'Order No.',ENTERED :R:H='Entered Date',"+;
                   "COMPLETE :R:H= 'Complete Date',NFABORDER :R:H='Ordered',NFBRECEIVE :R:H= 'Received'," +;
                   "NFABDAMAGE :R:H='Damaged',NFABCANCEL :R:H= 'Canceled',NPO_OPEN    :R:H= 'Open'" 

      =ARIABROW(lcFltrExp,lcFile_Ttl,20,.F.,20,.F.,.F.,.F.,'CMATTYPE,POMAT','laTemp',.F.,.F.,.F.)
      lcBrFields = lcSavBrFld
      lcFile_Ttl = lcSavTitle
      
      IF !EMPTY(laTemp[1]) AND laTemp[1] = 'P'
        lcBaseWind = gcBaseWind
        =gfstatic()
        DO gpDoProg WITH 'AWRMAPRCAM', .F., 'MA' ,"'"+laTemp(1)+"','"+laTemp(2)+"'"

      ENDIF
    ELSE
      ** MESSAGE : " No ð found for vendor ð "
      **           "     ® Ok ¯              "
      =gfModalGen("TRM04042B00000","DIALOG","orders|"+ALLTRIM(ladata[1]))
    ENDIF  
    IF llAPINST
      SET ORDER TO TAG INVVEND    IN APINVHDR
    ENDIF  
    
ENDCASE

*!*************************************************************
*! Name      : lpvReceipts
*! Developer : Albert Raif
*! Date      : 7/20/2003
*! Purpose   : On selection for orders option.
*! Ref       : B607422,1 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpvOrders
*!*************************************************************
*!B607422,1 ALB Add Matireal Order to Vendor option menun 
FUNCTION lpvReceipts


DO CASE
  CASE BAR() = 1     && Style PO Orders
    IF !USED('POSLN')
      =gfOpenFile(gcDataDir+'POSLN' ,'POSLN','SH') 
    ENDIF
    IF !USED('POSHDR')
      =gfOpenFile(gcDataDir+'POSHDR' ,'POSHDRV','SH') 
    ENDIF
    SELECT POSHDR
    SET ORDER TO TAG POSHDRV
    IF SEEK(laData[1])
      SELECT POSLN
      SET ORDER TO TAG POSLN
      DIMENSION laTemp[2]
      laTemp     = ''
      lcStyTitle = gfItemMask('HI')
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      lcFile_Ttl = "Receipts"
      lcFltrExp = "FOR VENDOR = laData[1] AND CSTYTYPE $ 'PD' AND TRANCD='2' "
      lcBrFields = "CSTYTYPE = IIF(CSTYTYPE='D','Dye Order','Purchase Order')  :R:H= 'Type',PO :R:H= 'Order No.',"+;
                   "STYLE :R:H= lcStyTitle,SCALE :R:H= 'Scale',DYELOT :R:H= 'Dyelot',SHIPNO :R:H= 'Shipment No',"+;
                   "CWARECODE :R:H= 'Warehouse',CRSESSION :R:H= 'Receiving Session',DATE :R:H= 'Receiving Date'"

      =ARIABROW(lcFltrExp,lcFile_Ttl,20,.F.,20,.F.,.F.,.F.,'CSTYTYPE,PO','laTemp',.F.,.F.,.F.)
      lcBrFields = lcSavBrFld
      lcFile_Ttl = lcSavTitle
      
      IF !EMPTY(laTemp[1]) AND laTemp[1] = 'P'
        lcBaseWind = gcBaseWind
        =gfstatic()
        DO gpDoProg WITH 'AWRPOSTYLE', .F., 'PO' ,"'"+laTemp(1)+"','"+laTemp(2)+"'"
      ENDIF
    ELSE
      ** MESSAGE : " No ð found for vendor ð "
      **           "     ® Ok ¯              "
      =gfModalGen("TRM04042B00000","DIALOG","orders|"+ALLTRIM(ladata[1]))
    ENDIF  
    IF llAPINST
      SET ORDER TO TAG INVVEND    IN APINVHDR
    ENDIF  
*******************************************************
  CASE BAR() = 2     && Reception
    IF !USED('POFLN')
      =gfOpenFile(gcDataDir+'POFLN' ,'POFLN','SH') 
    ENDIF
    IF !USED('POFHDR')
      =gfOpenFile(gcDataDir+'POFHDR' ,'POFHDRV','SH') 
    ENDIF
    SELECT POFHDR
    SET ORDER TO TAG POFHDRV
    IF SEEK(laData[1])
      SELECT POFLN
      SET ORDER TO TAG POFLN
      DIMENSION laTemp[2]
      laTemp     = ''
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      lcFile_Ttl = "Receipts"
      lcFltrExp = "FOR VENDOR = laData[1]  AND CMATTYPE $ 'P' AND TRANCD='2' "
      lcBrFields = "CMATTYP = 'MAT. Purchase Order' :R:H= 'Type',POMAT :R:H= 'Order No.',Fabric :R:H= 'Fabric',"+;
                   "Color :R:H= 'Color',DYELOT :R:H= 'Dyelot',SHIPNO :R:H= 'Shipment No',CWARECODE :R:H= 'Warehouse',"+;
                   "CRSESSION :R:H= 'Receiving Session',DATE :R:H= 'Receiving Date'"

      =ARIABROW(lcFltrExp,lcFile_Ttl,20,.F.,20,.F.,.F.,.F.,'CMATTYPE,POMAT','laTemp',.F.,.F.,.F.)
      lcBrFields = lcSavBrFld
      lcFile_Ttl = lcSavTitle
      
      IF !EMPTY(laTemp[1]) AND laTemp[1] = 'P'
        lcBaseWind = gcBaseWind
        =gfstatic()
        DO gpDoProg WITH 'AWRMAPRCAM', .F., 'MA' ,"'"+laTemp(1)+"','"+laTemp(2)+"'"

      ENDIF
    ELSE
      ** MESSAGE : " No ð found for vendor ð "
      **           "     ® Ok ¯              "
      =gfModalGen("TRM04042B00000","DIALOG","orders|"+ALLTRIM(ladata[1]))
    ENDIF  
    IF llAPINST
      SET ORDER TO TAG INVVEND    IN APINVHDR
    ENDIF  
    
ENDCASE
