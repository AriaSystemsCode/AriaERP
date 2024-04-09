*:************************************************************************
*: Program file  : POLETCR.PRG
*: Program desc. : Letter Of Credit
*: For screen    : POLETCR.SCX (0,1,2,3,4,5,6)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Shared between Style Purchase orders & Material modules.
*:      Developer: Reham Al-Allamy
*:           Date: 02/05/1998
*:************************************************************************
*: Documented *E300801,1 Rewrite the program to work in the 2.7 version
*:************************************************************************
*: Calls         : 
*:         Programs   : None
*:         Screens    : POLETCR.Spx (Letter of Credit Screen)
*:                      POLCSMR.Spx (Letter of Credit Summary Screen)
*:                      POLCPAY.Spx (Letter of Credit Payment Screen)
*:                      POLCAMD.Spx (Letter of Credit Amendment Screen)
*:                      POLCADD.Spx (Screen to Add Po to the current LC)
*:         Procedures : lpTab
*:                      lpShTab
*:                      lpShow
*:         Functions  : gfSetup
*:                      lfActPad
*:                      lpOptions
*:                      lfActFolder
*:                      lfRefPg1
*:                      lfvUpd_LC
*:                      lfSetChkBx
*:                      lfvData_2
*:                      lfwData_3
*:                      lfvData_3
*:                      lfvData_8
*:                      lfwData_9
*:                      lfvData_9
*:                      lfDrft2
*:                      lfDrft3
*:                      lfvChkBox
*:                      lfvBank
*:                      lfvAdvBank
*:                      lfPOBrow
*:                      lfCrtUnComp
*:                      lfPoOpnAmt
*:                      lfReadAct
*:                      lfDactMain
*:                      lfvBrow
*:                      lfwBrow
*:                      lfvNewPO
*:                      lfvDelPO
*:                      lfvAdd2LC
*:                      lpSavScr
*:                      lpDelScr
*:                      lpClsScr
*:                      lfvLCNotes
*:                      lfvSummary
*:                      lfvJournal
*:                      lfvAmend
*:                      lfvAmdAmt
*:                      lfvAmdOk
*:                      lfvPayment
*:                      lfvPayShip
*:                      lfvPayAmt
*:                      lfvPayAcpt
*:                      lfvPayCan
*:                      lfvShip
*:                      
*:************************************************************************
*: Passed Parameters  : lcFromMod : 'S' Called from Style P/O module.
*:                                  'M' Called from material module.
*:************************************************************************
*: Example            : 
*:   DO gpDoProg WITH 'AWRPOLETCR' , .F. , 'PO'
*:************************************************************************
*:  Modifications :
*B602179,1  AMM 11/25/98 Erase temporary files when quitting the program
*E301077,18 Reham On 12/30/98
*E301077,18 Reduce the opened file on the program level:
*E301077,18 1- For the main record :
*E301077,18    From : LC,CODES,LCJOURNL,POSHDR,POSLN,SYCINT,APVENDOR,SHPMTHDR,POFLN,POFHDR,UNCMSESS,NOTEPAD|LC,CCODE_NO,LCJOURNL,POSHDRV,POSLN,CCONTCODE,VENCODE,SHPMTHDR,POFLN,POFHDR,TRANS,NOTEPAD
*E301077,18    To   : LC,CODES,LCJOURNL,POSHDR,POSLN,SYCINT,APVENDOR,POFLN,POFHDR,UNCMSESS|LC,CCODE_NO,LCJOURNL,POSHDRV,POSLN,CCONTCODE,VENCODE,POFLN,POFHDR,TRANS
*E301077,18 2- For style purchase order letter of credit program, change the files in SydObjct:
*E301077,18    From : LC,CODES,LCJOURNL,POSHDR,POSLN,SYCINT,APVENDOR,SHPMTHDR,UNCMSESS,NOTEPAD|LC,CCODE_NO,LCJOURNL,POSHDRV,POSLN,CCONTCODE,VENCODE,SHPMTHDR,TRANS,NOTEPAD
*E301077,18    To   : LC,CODES,LCJOURNL,POSHDR,POSLN,SYCINT,APVENDOR,UNCMSESS|LC,CCODE_NO,LCJOURNL,POSHDRV,POSLN,CCONTCODE,VENCODE,TRANS
*E301077,18 3- For material letter of credit program, change the files in SydObjct:
*E301077,18    From : LC,CODES,LCJOURNL,SYCINT,APVENDOR,POFLN,POFHDR,UNCMSESS,NOTEPAD|LC,CCODE_NO,LCJOURNL,CCONTCODE,VENCODE,POFLN,POFHDR,TRANS,NOTEPAD
*E301077,18    To   : LC,CODES,LCJOURNL,SYCINT,APVENDOR,POFLN,POFHDR,UNCMSESS|LC,CCODE_NO,LCJOURNL,CCONTCODE,VENCODE,POFLN,POFHDR,TRANS
*E301077,18 4- Open the removed files from SydObjct in the current program.
*B602685,1  Reham On 03/18/99
*B602685,1  Validate the state field from the codes file if the address in
*B602685,1  USA or Canada...
*E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*                         as it's now controlled globally
*B602753,1 HDM 04/07/1999 Stop Calling NotePad Program In lpSavScr as the global save
*                         Will Call it
*B602774,1 Reham On 05/03/99
*B602774,1 Update expired date in POSHDR file if change it in the screen.
*B603348,1 ABD 03/29/2000 Increase the picture of open amount.
*B603348,1 ABD 03/04/2000 Increase the picture of nTrnAmunt up on the width
*B603348,1 ABD            of the filed,and picture of other filed in screen polcadd.scx.
*B604509,1 KHM 06/05/2001 Fix the bug of numeric overflow in case of single 
*B604509,1                currency.
*C123659,1 NNA 08/26/2004 Add a Custom menu Bar in Option pad for Customer[NIKNAK] to Show 
*C123659,1 NNA            a Cut-Off Shipment Scrren
*:**************************************************************************************
*
PARAMETERS lcFromMod
EXTERNAL ARRAY laKeyField , laDefProc

*-- Array to behave the working of gfSeekRec.
DECLARE laKeyField [2,4]
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'LC'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] = .T.
laKeyField[2,3] = 'LC'
laKeyField[2,4] = 2


*-- Search if there is AP module link or not.
llApLink   = OCCURS("AP", gcCmpModules) # 0

*-- Add more buttons to the control pannel.
* E301176,1 HDM 03/22/1999[Start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally

*DECLARE laPanelObj[IIF(lcFromMod = "S" , 5 , 4),3]
DECLARE laPanelObj[IIF(lcFromMod = "S" , 4 , 3),3]

*laPanelObj[1,1] = "pbNotes"     && Add notes button to the toolbar.
*laPanelObj[1,2] = gcBmpHome + "NOTES2.BMP"
*laPanelObj[1,3] = "VALID lfvLCNotes() MESSAGE 'Letter Of Credit Notes'"

laPanelObj[1,1] = "pbSummary"   && Add summary button to the toolbar.
laPanelObj[1,2] = gcBmpHome + "ORDERS.BMP"
laPanelObj[1,3] = "VALID lfvSummary() MESSAGE 'Summary'"
laPanelObj[2,1] = "pbJournal"   && Add journal button to the toolbar.
laPanelObj[2,2] = gcBmpHome + "ORDERS1.BMP"
laPanelObj[2,3] = "VALID lfvJournal() MESSAGE 'Journal'"
laPanelObj[3,1] = "pbAmendmnt"  && Add amendments button to the toolbar.
laPanelObj[3,2] = gcBmpHome + "COSTING1.BMP"
laPanelObj[3,3] = "VALID lfvAmend()   MESSAGE 'Amendments'"
IF lcFromMod = "S"
  laPanelObj[4,1] = "pbShipmnt"   && Add shipments button to the toolbar.
  laPanelObj[4,2] = gcBmpHome + "SHIPMNT.BMP"
  laPanelObj[4,3] = "VALID lfvShip()    MESSAGE 'Shipments'"
ENDIF

* E301176,1 HDM 03/22/1999[End]

IF llAPLink
  DIMENSION laPanelObj[ALEN(laPanelObj,1)+1 , 3]
  laPanelObj[ALEN(laPanelObj,1),1] = "pbPayments"  && Add payments button to the toolbar.
  laPanelObj[ALEN(laPanelObj,1),2] = gcBmpHome + "COMM.BMP"
  laPanelObj[ALEN(laPanelObj,1),3] = "VALID lfvPayment() MESSAGE 'Payments'"
ENDIF

*-- Variables hold the temp. names of the screen set.
lcLetCr0   = ""
lcLetCr1   = ""
lcLetCr2   = ""
lcLetCr3   = ""
lcLetCr4   = ""
lcLetCr4A  = ""
lcLetCr4B  = ""

*-- Array hold the folders info.
DECLARE laFoldWinds[4,2]
laFoldWinds = ""
lcfoldprnt  = ""           && window parent name for the folder
lnActfolder = 1            && Var. hold the current folder.
lcfolder    = ""
lcWFoldChng = ""           && function to control shows after change the folder
lcfoldpush  = ""           && push button name for the next folder
*-- Position of the folders on the base window.
lnFolderCen = 0
lnFolderRen = 0

laDefProc[7]  = .F.     && Force to local delete procedure  (lpDelScr)
laDefProc[9]  = .F.     && Force to local Save   procedure  (lpSavScr)
laDefProc[10] = .F.     && Force to local close  procedure  (lpClsScr)

*-- Arrays for the codes info.
DECLARE laCodInfo[1,10] , laCodInfo1[1,10] , laBank[1] , laAdvBank[1]

*-- Array hold the valid entries of the transaction type in LCJournal file.
DECLARE laTrnType[1,1]

*-- Get the prompt of the available pictures on the screen.
lcBrowKey  = gcBmpHome + "ExtKey.BMP"
lcLCOkBmp  = gcBmpHome + "OK.BMP"
lcAcptBmp  = gcBmpHome + "Accept.BMP"
lcClsBmp   = gcBmpHome + "Close1.BMP"
lcCanBmp   = gcBmpHome + "Can.BMP"

****************** Initialeze the needed variables. ************************

lcPOHdrFil = IIF(lcFromMod = "S" , "POSHDR" , "POFHDR")
lcPOLinFil = IIF(lcFromMod = "S" , "POSLN"  , "POFLN")

llAddMode  = .F.    && Flag to know if entered the add mode or not.
llContinue = .F.    && Flag to know if there is uncomplete session.

*-- Define variable used in the cost labels variables according 
*-- to the called module.
lcCstVar = IIF(lcFromMod = "S" , "I" , "T")

*-- Define the variables that hold cost labels.
FOR lnCount = 1 TO IIF(lcFromMod = "S" , 5 , 4)
  lcCount   = STR(lnCount,1)
  STORE "" TO lc&lcCstVar.SLBL&lcCount
ENDFOR

*-- Variables hold the cost items labels.
STORE " " TO lcCost1 , lcCost2 , lcCost3 , lcCost4 , lcCost5
STORE .F. TO cbCost1 , cbCost2 , cbCost3 , cbCost4 , cbCost5

*-- Define the browse title.
lcPOBrTitl = 'Purchase Orders'

*-- Var. hold the screen fields.
lcScFields = ""

*-- Var. hold the current mode.
lcScrMode  = "S"

*-- Assign names for the temp. files.
lcTmpTrans = ""            && Temp Transaction.
lcTmpJornl = ""            && Temp. Journal file.
lcTmpPOSHd = ""            && Temp. purchase order file.
lcTmpPayTr = ""            && Temp. Payment file.
lcTmp_LC   = ""            && Temp. Letter of credit file.

*-- Define the currency variables.
lnCurrUnit = 1             && Hold currency unit.
lcUntSin   = '/'           && Hold the unit sign.
lcExRSin   = '/'           && Hold the exhange rate sign.

*-- Variables used to deal with the uncomplete program session concept.
lcProgID   = PADR('LC',10) && Hold key of the program in the uncomplete session file.
lcSession  = SPACE(1)      && Hold session no.
lnUnCmSeRc = 0             && Hold record no. in the uncompleted session file.
lnThisSess = gnProgCopy    && Hold current session in the uncompleted session file.
llGoAndChk = .T.           && Flag to know if checked uncompleted session record.

*-- Variable to tell if are we going to check the existance of
*-- any incomplete session in the screen's setup or not.
llChkUnCom = .T.

*-- Flag to force entering the lpClsScr procedure.
llCUpDate  = .F.

*-- Array hold the needed variables if lost the current session.
DECLARE laVars[4]
*-- Fill the variables array.
laVars[1]  = 'lcScrMode'
laVars[2]  = 'lcUntSin'
laVars[3]  = 'lcExRSin'
laVars[4]  = 'lnUnCmSeRc'

*-- Define the setup flags ffor currency code.
STORE .F. TO llMulCurr

*-- Define the check boxes used in the screen.
STORE .F. TO cbAdv1 , cbAdv2 , cbAdv3 , cbAdv4 , cbAdv5 , ;
             cbDrft1 , cbDrft2 , cbDrft3 , cbFob  , cbFad , ;
             cbC_F , cbFAS , cbCif , cbOther1 , cbMd_Out_1 , ;
             cbFrt_Pre , cbFrt_Col , cbFrt_Pre2 , cbFrt_Col2 , ;
             cbIns_Pol , cbIns_PolM , cbIns_PolA , cbMAW_Ins , ;
             cbPck_Lst , cbCert_Org , cbbnk_chk

*-- Define the logical variables used in the screen.
STORE .F. TO llBrowse , llOpenCurr , llOpenExch

*-- Define the numeric variables used in the screen.
STORE  0 TO lnMarker , lnOldValue , lnPORec

*-- Define the charcter variables used in the screen
STORE "" TO lcVendName , lcOldValue , lcAddHed1 , lcOldPo

*-- Define the variables hold the status "Disable" or "Enable"
STORE "DISABLE" TO lcDrf2Stat , lcDrf3Stat , lcLinStat , lcExRStat , ;
                   lcVendStat , lcCurStat

*B602774,1 Reham On 05/03/99    *** Begin ***
*B602774,1 Define variable hold the old expired date.
ldOldDate = {}
*B602774,1 Reham On 05/03/99    *** End   ***

*-- Call global function in the main program to do the following : _
*-- Intialise all the variables & open all the files needed in
*-- this session and controling disabling and enabling of the
*-- menu bars and writting the screen names in the window bars.
IF !gfSetup()
  RETURN
ENDIF

*-- Array to hold the Lc fields used in the screen.
DECLARE laData[85]
lcScField1 = "cLCType   , cLcNo     , Vendor    , cLcBank   , Created  , "+;
             "Bank      , Expire    , cCurrCode , nExRate   , Frn_Amt  , "+;
             "US_Amt    , Contact   , Phone     , cCont_Code, cAddress1, "+;
             "cAddress2 , cAddress3 , cAddress4 , cAddress5 , cAddress6, "+;
             "Comm_Ins1 , Comm_Ins2 , Comm_Ins3 , Comm_Ins4 , Comm_Ins5, "+;
             "cLcFob    , Fas       , C_F       , Cif       , Other1   , "+;
             "Other1_1  , Adv_1     , Adv_2     , Adv_3     , Adv_4    , "+;
             "Adv_5     , Draft_1   , Draft_2   , Draft_2_1 , Draft_3  , "
lcScField2 = "Draft_3_1 , Draft_3_2 , Spc_Custom, Brd_Mrn   , Md_Out_1 , "+;
             "Md_Out_2  , Md_Out_3  , Mkd_Not_1 , Mkd_Not_2 , Mkd_Not_3, "+;
             "Frt_Pre   , Frt_Col   , Air_Way_1 , Air_Way_2 , Air_Way_3, "+;
             "Mkd_Not2_1, Mkd_Not2_2, Mkd_Not2_3, Frt_Pre_2 , Frt_Col_2, "+;
             "Adv_Bank  , Ins_Pol   , Ins_Pol_M , Ins_Pol_A , M_A_W_Ins, "+;
             "Pck_Lst   , Pck_Lst1  , Cert_Org  , Cert_Org1 , Oth_Doc1 , "+;
             "Oth_Doc2  , Oth_Doc3  , see_att   , Doc_Evd   , Doc_Evd1 , "+;
             "From      , Latest    , To        , Part_Shp  , Tran_Shp , "+;
             "Bnk_Chk   , Ben_Bank  , nLCBalance , Branch , nCurrUnit"

lcScFields = lcScField1 + lcScField2

llAlowNew  = .T.     && Flag to allow adding new credit memos.
llNoShow   = .F.     && Flag to force the execution of the show procedure.

*E301077,18 Reham On 12/30/98   *** Begin ***
*E301077,18 Open the company file.
=gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_Id','SH')
*E301077,18 Reham On 12/30/98   *** End   ***

*-- If entering the screen for the first time.
IF !WEXIST(gcBaseWind)
  *-- If called from style purchase order module.
  IF lcFromMod = "S"
    *-- Get all the labels for the different cost items from the setup file.
    FOR lnCount = 1 TO 5
      lcCount   = STR(lnCount,1)
      lc&lcCstVar.SLBL&lcCount = gfGetMemVar('M_C'+lcCstVar+'SLBL'+lcCount , gcAct_Comp)
      *-- Fill the check boxes of cost items with its labels.
      lcCost&lcCount           = lc&lcCstVar.SLBL&lcCount
    ENDFOR 
  ELSE
    *-- If called from materials module.
    lcCost1 = "Price  "
    lcCost2 = "Freight"
    lcCost3 = "Tax    "
    lcCost4 = "Quota  "
  ENDIF
  
  *-- Fill the LC Journal transaction type from the dictionary.
  lnArryLen  = gfGetVld("cLCTrnTyp" , @laTrnType)
  
  *-- Get the default address.
  lcAddHed1  = PROP(LOOKUP(SYCINT.CPARt1LAB,ALLTRIM(gcContCode),sycint.ccont_code,'CCONTCODE'))
  
  *-- Multi Currency.
  llMulCurr  = gfGetMemVar('llMulCurr',gcAct_Comp)
  
  *-- If multi currency.
  *IF llMulCurr
    IF !USED("SycCurr")
      llOpenCurr = .T.
      *-- Open the currency file if was not open.
      =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
    ELSE
      llOpenCurr = .F.
    ENDIF
  *ENDIF
  
  *-- Array hold the bank codes info.
  laCodInfo[1,01] = "BANK"                 && Field Name
  laCodInfo[1,02] = "laBank"               && Array Name
  laCodInfo[1,03] = "lnBank"               && Popup Name
  laCodInfo[1,04] = ""                     && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[1,05] = .F.                    && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[1,06] = .F.                    && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[1,07] = "LC"                   && Alternative File (For default val.)
  laCodInfo[1,08] = "LC"                   && Use this index for the Alternative file.
  laCodInfo[1,09] = "laData[1]+laData[2]"  && Seek this expretion.
  laCodInfo[1,10] = "BANK"                 && Alternative Field Name
  
  *-- Fill the bank code popup with N/A.
  = gfwCodePop(@laCodInfo , "BANK" , "N")
  
  *-- Array hold the bank codes info.
  laCodInfo1[1,01] = "BANK"                 && Field Name
  laCodInfo1[1,02] = "laAdvBank"            && Array Name
  laCodInfo1[1,03] = "lnAdvBank"            && Popup Name
  laCodInfo1[1,04] = ""                     && Popup Status  ("D"->Default,"A"->All)
  laCodInfo1[1,05] = .F.                    && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo1[1,06] = .F.                    && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo1[1,07] = "LC"                   && Alternative File (For default val.)
  laCodInfo1[1,08] = "LC"                   && Use this index for the Alternative file.
  laCodInfo1[1,09] = "laData[1]+laData[2]"  && Seek this expretion.
  laCodInfo1[1,10] = "ADV_BANK"             && Alternative Field Name

  *-- Fill the Advise bank code popup with N/A.
  = gfwCodePop(@laCodInfo1 , "BANK" , "N")
  
  *-- Get Temp. names for the screen set.
  lcLetCr0  = gfTempName()
  lcLetCr1  = gfTempName()
  lcLetCr2  = gfTempName()
  lcLetCr3  = gfTempName()
  lcLetCr4  = gfTempName()
  lcLetCr4A = gfTempName()
  lcLetCr4B = gfTempName()
  
  lcfoldprnt  = gcBaseWind        && window parent name for the folder
  lnActfolder = 1                 && Var. hold the current folder.
  lcfolder    = gfTempName()
  lcWFoldChng = '=lfActFolder()'  && function to control shows after change the folder
  lcfoldpush  = 'pbFolders'       && push button name for the next folder
  
  *-- Position of the folders on the base window.
  lnFolderCen = 102.00
  lnFolderRen = 2
  
  laFoldWinds[1,1] = "Page 1"
  laFoldWinds[1,2] = lcLetCr1
  laFoldWinds[2,1] = "Page 2"
  laFoldWinds[2,2] = lcLetCr2
  laFoldWinds[3,1] = "Page 3"
  laFoldWinds[3,2] = lcLetCr3
  laFoldWinds[4,1] = "Assign POs"
  laFoldWinds[4,2] = lcLetCr4
  
  *-- Temp. name for the header file.
  lcTmpTrans = gfTempName()  && Temp Transaction.
  lcTmpJornl = gfTempName()  && Temp. Letter of credit journal.
  lcTmpPOSHd = gfTempName()  && Temp. style purchase order header file.
  lcTmpPayTr = gfTempName()  && Temp. Payment file.
  lcTmp_LC   = gfTempName()  && Temp. letter of credit file.
  
  *-- Get a session number, to be updated in the uncomplete session
  *-- number file for each session.
  lcSession  = gfsequence('CSESSION')
  
  *-- Fill the main array with the LC fields blank.
  SELECT LC
  SCATTER FIELDS &lcScFields TO laData BLANK
  
  *-- Go to the select mode at the begining.
  laScrMode    = .F.
  laScrMode[1] = .T.
ENDIF

*B604509,1 KHM 06/05/2001 (Begin) Initialize the following variables in case
*B604509,1                of signle currency. laData[8] = Currency code, 
*B604509,1                laData[9] = nExRate, laData[85] = Currency unit.
IF !llMulCurr
  laData[8]  = gcBaseCurr
  STORE 1 TO laData[9] , laData[85]
ENDIF
*B604509,1 KHM 06/05/2001 (End)

*-- Variable controls the enabling and disabling of the different 
*-- objects in the different folder.
lcObjStat  = IIF(laScrMode[1] .OR. laScrMode[2] , "DISABLE" , "ENABLE")
lcExRStat  = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  laData[8] <> gcBaseCurr , "ENABLE" , "DISABLE")
lcDrf2Stat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. laData[38] = "Y" , "ENABLE" , "DISABLE")
lcDrf3Stat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. laData[40] = "Y" , "ENABLE" , "DISABLE")
lcVendStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  lnPORec = 0 , "ENABLE" , "DISABLE")

*B604509,1 KHM 06/05/2001 (Begin) Add the checking of multi-currency.
*lcCurStat  = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  lnPORec = 0 , "ENABLE" , "DISABLE")
lcCurStat  = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  lnPORec = 0 AND llMulCurr, "ENABLE" , "DISABLE")
*B604509,1 KHM 06/05/2001 (End)

*-- Define the browses bars.
PUSH MENU _MSYSMENU
DEFINE BAR 100 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 101 OF P01PU01 PROMPT lcPOBrTitl KEY ALT+B

*-- Activate the browses when selecting there bars.
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcPOBrTitl)

*-- Set realtion between temp. POs header & master POs lines 
*-- according to the called module.
IF USED(lcTmpPOSHd)
  SELECT (lcTmpPOSHd)
  IF lcFromMod = "S"
    SET RELATION TO cStyType + Po INTO (lcPOLinFil) ADDITIVE
  ELSE
    SET RELATION TO cMatType + PoMat INTO (lcPOLinFil) ADDITIVE
  ENDIF
ENDIF

*-- Set filter to the type in the letter of credit file according 
*-- to the called module.
SELECT LC
SET FILTER TO cLCType = lcFromMod

*-- Call the screen.
DO (gcScrDir+"POLETCR.SPX")

*-- Clear the letter of credit file filter.
SELECT LC
SET FILTER TO

*-- Clear the relation between temp. POs header & master POs lines.
IF USED(lcTmpPOSHd)
  SELECT (lcTmpPOSHd)
  SET RELATION TO
ENDIF

*-- Release the browse window.
RELEASE WINDOW (lcPOBrTitl)
POP MENU _MSYSMENU

*-- Release the Options pad from the system menu.
RELEASE PAD _OPTIONS OF _MSYSMENU

*-- If quitting the program erase the temp. files.
IF glQuitting
  *-- Close the currency code file if was opened in this program.
  IF llOpenCurr
    USE IN SycCurr
  ENDIF
  
  IF USED(lcTmpTrans)
    USE IN (lcTmpTrans)
  ENDIF
  ERASE &gcWorkDir.&lcTmpTrans..DBF
  ERASE &gcWorkDir.&lcTmpTrans..CDX
  
  IF USED(lcTmpJornl)
    USE IN (lcTmpJornl)
  ENDIF
  ERASE &gcWorkDir.&lcTmpJornl..DBF

  IF USED(lcTmpPOSHd)
    USE IN (lcTmpPOSHd)
  ENDIF
  ERASE &gcWorkDir.&lcTmpPOSHd..DBF
  ERASE &gcWorkDir.&lcTmpPOSHd..CDX

  IF USED(lcTmpPayTr)
    USE IN (lcTmpPayTr)
  ENDIF
  ERASE &gcWorkDir.&lcTmpPayTr..DBF
  ERASE &gcWorkDir.&lcTmpPayTr..CDX
  *B602179,1 AMM Erase temporary files when quitting the program
  IF USED(lcTmp_LC)
    USE IN (lcTmp_LC)
  ENDIF
  ERASE &gcWorkDir.&lcTmp_LC..DBF
  ERASE &gcWorkDir.&lcTmp_LC..CDX
  *B602179,1 AMM end
  
ENDIF

*!*************************************************************
*! Name      : lfActPad
*! Developer : Reham Al-Allamy
*! Date      : 02/05/1998
*! Purpose   : Bulid a new menu pad [Option] in the when of the
*!           : main screen
*!*************************************************************
*! Calls     : lpOptions
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfActPad()
*!*************************************************************
*
FUNCTION lfActPad

*-- Define a pad called options have 5 bars for the different calling 
*-- in the main screen.
DEFINE PAD _OPTIONS OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
SET SKIP OF PAD _OPTIONS OF _MSYSMENU (laScrMode[1])
ON PAD _OPTIONS OF _MSYSMENU ACTIVATE POPUP _OPTIONSPOP

DEFINE POPUP    _OPTIONSPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONSPOP PROMPT "\<Summary"    SKIP FOR laScrMode[1]
DEFINE BAR 2 OF _OPTIONSPOP PROMPT "\<Payments"   SKIP FOR (laScrMode[1] OR laScrMode[2])
DEFINE BAR 3 OF _OPTIONSPOP PROMPT "\<Amendments" SKIP FOR (laScrMode[1] OR laScrMode[2])
DEFINE BAR 4 OF _OPTIONSPOP PROMPT "\<Journal"    SKIP FOR laScrMode[1]
*-- If called from style purchase order module.
IF lcFromMod = "S"
  DEFINE BAR 5 OF _OPTIONSPOP PROMPT "\<Shipments"  SKIP FOR laScrMode[1]
ENDIF

*C123659,1 NNA 07/18/2004 (Begin) Add a Custom menu Bar in Option pad for Customer[NIKNAK]
IF ASCAN(laEvntTrig,PADR('ADDMPAD',10))<>0 
  =gfDoTriger(lcProgName,PADR('ADDMPAD',10))
ENDIF
*C123659,1 NNA (End)

ON SELECTION POPUP _OPTIONSPOP DO lpOptions

*!*************************************************************
*! Name      : lpOptions
*! Developer : Reham Al-Allamy
*! Date      : 02/05/1998
*! Purpose   : To be excecuted upon selecting a bar from option pad.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpOptions
*!*************************************************************
*
FUNCTION lpOptions

DO CASE
  *-- Call the summary screen.
  CASE BAR() = 1
    =lfvSummary()
  *-- Call the payments screen.
  CASE BAR() = 2
    =lfvPayment()
  *-- Call the amendments screen.
  CASE BAR() = 3
    =lfvAmend()
  *-- Call the journal screen.
  CASE BAR() = 4
    =lfvJournal()
  *-- Call the shipments screen.
  CASE BAR() = 5
    =lfvShip()
ENDCASE

*!*************************************************************
*! Name      : lpShow
*! Developer : Reham Al-Allamy
*! Date      : 02/05/1998
*! Purpose   : Show procedure.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************
*
PROCEDURE lpShow

*-- The lc type will be "S" in case of style purchase order module.
*-- The lc type will be "M" in case of material module.
laData[1] = lcFromMod

DO CASE
  CASE laScrMode[1]
    *-- Variable hold the records count in the PO header file
    lnPORec = 0
    
    *-- Check if there is previos uncompleted session.
    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF
    
    *-- Var. hold the current mode.
    lcScrMode  = "S"
    
    laData[14] = gcContcode 
    lcAddHed1  = PROP(LOOKUP(SYCINT.CPARt1LAB,laData[14],sycint.ccont_code,'CCONTCODE')) 
    
    *-- Call function to recreate the temp. files used in this program.
    =lfCrtUnComp()
    
    *-- Fill the bank code & the advise bank code popup with N/A.
    =gfwCodePop(@laCodInfo  , "BANK" , "N")
    =gfwCodePop(@laCodInfo1 , "BANK" , "N")
    *-- Control the enabling & disabling of the added buttons in the toolbar.
    SHOW GET pbNotes    DISABLE
    SHOW GET pbPayments DISABLE
    SHOW GET pbAmendmnt DISABLE
    SHOW GET pbSummary  DISABLE
    SHOW GET pbJournal  DISABLE
    *-- If called from style purchase order module.
    IF lcFromMod = "S"
      SHOW GET pbShipmnt  DISABLE
    ENDIF
  CASE laScrMode[2]
    *-- Var. hold the current mode.
    lcScrMode  = "V"
    
    *-- Collect the LcJournl data in temp. file.
    SELECT * , "S" AS cStatus , RECNO() AS nRecNo ;
      FROM LcJournl ;
     WHERE cLCType + cLcNo = laData[1] + laData[2] ;
      INTO DBF (gcWorkDir+lcTmpJornl)
    
    *E301077,18 Reham On 01/20/99   *** Begin ***
    *-- Close the temp. file to open it again shared.
    *USE IN (lcTmpJornl)
    *=gfOpenFile(gcWorkDir+lcTmpJornl , "" , "EX")
    *E301077,18 Reham On 01/20/99   *** End   ***
    
    *-- Collect the assigned Style POs or Material POs according to 
    *-- the called module.
    SELECT * , "S" AS cStatus , RECNO() AS nRecNo ;
      FROM &lcPOHdrFil ;
     WHERE cLcNo = laData[2] ;
      INTO DBF (gcWorkDir+lcTmpPOSHd)
    
    *-- Variable hold the records count in the PO header file
    lnPORec = RECCOUNT(lcTmpPOSHd)
    
    *-- Create an index for the temp style POs header or material POs header.
    IF lcFromMod = "S"
      INDEX ON cLcNo + Po Tag (lcTmpPOSHd)
    ELSE
      INDEX ON cLcNo + PoMat Tag (lcTmpPOSHd)
    ENDIF
    
    *E301077,18 Reham On 01/20/99   *** Begin ***
    *-- Close the temp. file to open it again shared.
    *USE IN (lcTmpPOSHd)
    *=gfOpenFile(gcWorkDir+lcTmpPOSHd , lcTmpPOSHd , "EX")
    *E301077,18 Reham On 01/20/99   *** End   ***
        
    *-- Fill the bank code & the advise bank code popup with its values.
    =gfwCodePop(@laCodInfo  , "BANK" , "T")
    =gfwCodePop(@laCodInfo1 , "BANK" , "T")
    
    *-- Control the enabling & disabling of the added buttons in the toolbar.
    SHOW GET pbNotes    ENABLE
    SHOW GET pbPayments DISABLE
    SHOW GET pbAmendmnt DISABLE
    SHOW GET pbSummary  ENABLE
    SHOW GET pbJournal  ENABLE
    *-- If called from style purchase order module.
    IF lcFromMod = "S"
      SHOW GET pbShipmnt  ENABLE
    ENDIF
  CASE laScrMode[3]
    *-- Var. hold the current mode.
    lcScrMode  = "E"

    IF !llAddMode .AND. !llContinue
      *-- Flag to know if enter the add mode before or not.
      llAddMode  = .T.
      
      *-- Flag to force entering the lpClsScr procedure.
      llCUpDate  = .T.
      
      *-- Add record in the temp. letter of credit file.
      SELECT (lcTmp_LC)
      IF RECCOUNT() = 0
        APPEND BLANK
      ENDIF
      *-- Save the old record to the objects on the screen.
      GATHER FIELDS &lcScFields FROM laData
    ENDIF
    *-- Add record in the uncomplete session file.
    llNoThing  = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)
  
    *-- Control the enabling & disabling of the added buttons in the toolbar.
    SHOW GET pbNotes    ENABLE
    SHOW GET pbPayments ENABLE
    SHOW GET pbAmendmnt ENABLE
    SHOW GET pbSummary  ENABLE
    SHOW GET pbJournal  ENABLE
    *-- If called from style purchase order module.
    IF lcFromMod = "S"
      SHOW GET pbShipmnt  ENABLE
    ENDIF
    _CUROBJ = IIF(lnPORec > 0 , OBJNUM(lnBank) , OBJNUM(laData[3]))
  CASE laScrMode[4]
    *-- Var. hold the current mode.
    lcScrMode  = "A"
    IF !llAddMode .AND. !llContinue
      *-- Set default for entered date.
      laData[5]  = gdSysDate
      
      *-- Set default for the exchange rate.
      laData[9]  = 1
      
      *B604509,1 KHM 06/05/2001 (Begin) Initializing the currency unit
      laData[85] = 1
      *B604509,1 KHM 06/05/2001 (End)
      
      *-- Default to the current country code.
      laData[14] = gcContcode 
      
      *-- Flag to force entering the lpClsScr procedure.
      llCUpDate  = .T.
      
      *-- Flag to know if enter the add mode before or not.
      llAddMode  = .T.
      
      *-- Call function to recreate the temp. files used in this program.
      =lfCrtUnComp()
      
      *-- Add record in the temp. letter of credit file.
      SELECT (lcTmp_LC)
      IF RECCOUNT() = 0
        APPEND BLANK
      ENDIF
      BLANK
    ENDIF
    *-- Add record in the uncomplete session file.
    llNoThing  = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)
    
    *-- Fill the bank code popup with the default value.
    =gfwCodePop(@laCodInfo  , "BANK" , "D")
    laData[6]  = laBank[lnBank,2]
    *-- Fill the Advise bank code popup with the default value.
    =gfwCodePop(@laCodInfo1 , "BANK" , "D")
    laData[61] = laAdvBank[lnAdvBank,2]
    
    *-- Variable hold the records count in the PO header file
    lnPORec = 0
    
    *-- Control the enabling & disabling of the added buttons in the toolbar.
    SHOW GET pbNotes    DISABLE
    SHOW GET pbPayments ENABLE
    SHOW GET pbAmendmnt ENABLE
    SHOW GET pbSummary  ENABLE
    SHOW GET pbJournal  ENABLE
    *-- If called from style purchase order module.
    IF lcFromMod = "S"
      SHOW GET pbShipmnt  ENABLE
    ENDIF
    
    _CUROBJ = OBJNUM(laData[3])
ENDCASE

*-- Disable the option pad in the select mode.
SET SKIP OF PAD _OPTIONS OF _MSYSMENU (laScrMode[1])

*-- Disable the payments & amendments bars in the option pad if view mode.
SET SKIP OF BAR 2 OF PAD _OPTIONS OF _MSYSMENU (laScrMode[1] OR laScrMode[2])
SET SKIP OF BAR 3 OF PAD _OPTIONS OF _MSYSMENU (laScrMode[1] OR laScrMode[2])

*-- Call local function to adjust the check boxes values.
=lfSetChkBx()

*-- Get the vendor name.
lcVendName = LOOKUP(APVENDOR.CVENCOMP,laData[3],APVENDOR.CVENDCODE,'VENCODE')

*-- Define header for address popup.
lcAddHed1 = PROP(LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[14]),ALLTRIM(gcContCode),laData[14]),sycint.ccont_code,'CCONTCODE'))
SHOW GET lcAddHed1

*B604509,1 KHM 06/05/2001 (Begin) Initialize the following variables in case
*B604509,1                of signle currency. laData[8] = Currency code, 
*B604509,1                laData[9] = nExRate, laData[85] = Currency unit.
IF !llMulCurr
  laData[8]  = gcBaseCurr
  STORE 1 TO laData[9] , laData[85]
ENDIF
*B604509,1 KHM 06/05/2001 (End)

*-- Variable controls the enabling and disabling of the different 
*-- objects in the different folder.
lcObjStat  = IIF(laScrMode[1] .OR. laScrMode[2] , "DISABLE" , "ENABLE")
lcDrf2Stat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. laData[38] = "Y" , "ENABLE" , "DISABLE")
lcDrf3Stat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. laData[40] = "Y" , "ENABLE" , "DISABLE")
lcExRStat  = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  laData[8] <> gcBaseCurr , "ENABLE" , "DISABLE")
lcVendStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  lnPORec = 0 , "ENABLE" , "DISABLE")

*B604509,1 KHM 06/05/2001 (Begin) Add the checking of multi-currency.
*lcCurStat  = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  lnPORec = 0 , "ENABLE" , "DISABLE")
lcCurStat  = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  lnPORec = 0 AND llMulCurr, "ENABLE" , "DISABLE")
*B604509,1 KHM 06/05/2001 (End)

SHOW GET laData[3] &lcVendStat
SHOW GET laData[8] &lcCurStat
SHOW GET ibCur     &lcCurStat

*-- Call the change folder local function.
=lfActFolder()

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Reham Al-Allamy
*! Date      : 02/05/1998
*! Purpose   : Function to be executed between changing folders.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfActFolder()
*!*************************************************************
*
FUNCTION lfActFolder

DO CASE
  CASE lnActFolder = 1
    *-- Disable all windows except the window of the first folder.
    SHOW GETS WINDOW (lcLetCr2)  DISABLE ONLY
    SHOW GETS WINDOW (lcLetCr3)  DISABLE ONLY
    SHOW GETS WINDOW (lcLetCr4B) DISABLE ONLY
    *-- Disable the browse invisable button.
    SHOW GET ibTab4b_1 DISABLE
    *-- Refresh the objects in window of the current folder.
    =lfRefPg1()
  CASE lnActFolder = 2
    *-- Disable all windows except the window of the first folder.
    SHOW GETS WINDOW (lcLetCr1)  DISABLE ONLY
    SHOW GETS WINDOW (lcLetCr3)  DISABLE ONLY
    SHOW GETS WINDOW (lcLetCr4B) DISABLE ONLY
    *-- Disable the browse invisable button.
    SHOW GET ibTab4b_1 DISABLE
    *-- Enable the window of the current folder.
    SHOW GETS WINDOW (lcLetCr2) &lcObjStat ONLY
  CASE lnActFolder = 3
    *-- Disable all windows except the window of the first folder.
    SHOW GETS WINDOW (lcLetCr1)  DISABLE ONLY
    SHOW GETS WINDOW (lcLetCr2)  DISABLE ONLY
    SHOW GETS WINDOW (lcLetCr4B) DISABLE ONLY
    *-- Disable the browse invisable button.
    SHOW GET ibTab4b_1 DISABLE
    *-- Enable the window of the current folder.
    SHOW GETS WINDOW (lcLetCr3) &lcObjStat ONLY
  CASE lnActFolder = 4
    *-- Disable all windows except the window of the first folder.
    SHOW GETS WINDOW (lcLetCr1) DISABLE ONLY
    SHOW GETS WINDOW (lcLetCr2) DISABLE ONLY
    SHOW GETS WINDOW (lcLetCr3) DISABLE ONLY
    *-- Enable the browse invisable button.
    SHOW GET ibTab4b_1 ENABLE
    *-- Call the POs Browse.
    =lfPOBrow()
    *-- Call the browse when function.
    =lfwBrow()
ENDCASE

*!*************************************************************
*! Name      : lfSetChkBx
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Function to adjust the check boxes values.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfSetChkBx()
*!*************************************************************
*
FUNCTION lfSetChkBx

*-- Adjust the check boxes values according to the its assigned 
*-- array elements values.
cbAdv1     = (laData[32] = "Y")
cbAdv2     = (laData[33] = "Y")
cbAdv3     = (laData[34] = "Y")
cbAdv4     = (laData[35] = "Y")
cbAdv5     = (laData[36] = "Y")
cbDrft1    = (laData[37] = "Y")
cbDrft2    = (laData[38] = "Y")
cbDrft3    = (laData[40] = "Y")
cbFOB      = (laData[26] = "Y")
cbFAS      = (laData[27] = "Y")
cbC_F      = (laData[28] = "Y")
cbCIF      = (laData[29] = "Y")
cbOther1   = (laData[30] = "Y")
cbMd_Out_1 = (laData[45] = "Y")
cbFrt_Pre  = (laData[51] = "Y")
cbFrt_Col  = (laData[52] = "Y")
cbFrt_Pre2 = (laData[59] = "Y")
cbFrt_Col2 = (laData[60] = "Y")
cbIns_Pol  = (laData[62] = "Y")
cbIns_PolM = (laData[63] = "Y")
cbIns_PolA = (laData[64] = "Y")
cbMAW_Ins  = (laData[65] = "Y")
cbPck_Lst  = (laData[66] = "Y")
cbCert_Org = (laData[68] = "Y")
cbbnk_chk  = (laData[81] = "Y")

*!*************************************************************
*! Name      : lfRefPg1
*! Developer : Reham Al-Allamy
*! Date      : 02/22/1998
*! Purpose   : Function to refresh objects in page 1 in folder 1.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRefPg1()
*!*************************************************************
*
FUNCTION lfRefPg1

*-- Refresh the objects in page 1 in folder 1 according to the current mode.
SHOW GET lnBank     &lcObjStat
SHOW GET ibAddress  &lcObjStat
SHOW GET lcAddHed1  &lcObjStat
SHOW GET cbFob      &lcObjStat
SHOW GET cbFas      &lcObjStat
SHOW GET cbC_F      &lcObjStat
SHOW GET cbCif      &lcObjStat
SHOW GET cbOther1   &lcObjStat

FOR lnCount = 1 TO 5
  lcCount = ALLTRIM(STR(lnCount))
  SHOW GET cbAdv&lcCount &lcObjStat
ENDFOR

FOR lnCount = 1 TO 3
  lcCount = ALLTRIM(STR(lnCount))
  SHOW GET cbDrft&lcCount &lcObjStat
ENDFOR

SHOW GET ibCur      &lcCurStat
SHOW GET laData[8]  &lcCurStat

SHOW GET laData[4]  &lcObjStat
SHOW GET laData[5]  &lcObjStat
SHOW GET laData[7]  &lcObjStat
SHOW GET laData[9]  &lcExRStat
SHOW GET laData[31] &lcObjStat
SHOW GET laData[39] &lcDrf2Stat
SHOW GET laData[41] &lcDrf3Stat
SHOW GET laData[42] &lcDrf3Stat
SHOW GET laData[84] &lcObjStat

FOR lnCount = 10 TO 13
  SHOW GET laData[lnCount] &lcObjStat
ENDFOR

FOR lnCount = 15 TO 25
  SHOW GET laData[lnCount] &lcObjStat
ENDFOR

*!*************************************************************
*! Name      : lfvUpd_LC
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Valid function for LC fields.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvUpd_LC()
*!*************************************************************
*
FUNCTION lfvUpd_LC
PRIVATE lcLCAlias

*-- Save the current file.
lcLCAlias = ALIAS()

*-- Save the current laData into the Letter of Credit file.
SELECT (lcTmp_LC)
*-- Lock the record to grantee the phiscal update.
= RLOCK()
GATHER FIELDS &lcScFields FROM laData
UNLOCK
*-- Set to true to know that there is anything updated.
llCUpDate = .T.

*C123659,1 NNA 07/18/2004 (Begin) Clean the (lcTmpPoShd) File if the Price Currency Changed
*C123659,1 NNA            for Customer[NIKNAK]
IF ASCAN(laEvntTrig,PADR('CLNTMP',10))<>0 
  =gfDoTriger(lcProgName,PADR('CLNTMP',10))
ENDIF
*C123659,1 NNA (End)

*NADER
*-- Restore previous file.
SELECT (lcLCAlias)

*!*************************************************************
*! Name      : lfWhenAll
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : When function for all objects in the screen.
*!*************************************************************
*! Calls     : gfCurrBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfWhenAll()
*!*************************************************************
*
FUNCTION lfWhenAll

*-- Save the current value of the current object.
lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvData_2
*! Developer : Reham Al-Allamy
*! Date      : 02/05/1998
*! Purpose   : Valid function of the lc #.
*!*************************************************************
*! Calls     : gfSeekRec
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_2()
*!*************************************************************
*
FUNCTION lfvData_2

*-- Fill with the LC type.
laData[1] = lcFromMod

*-- Fill the LC # with zeros on the left if not browsing.
llBrowse  = IIF((!llBrowse .AND. "?" $ laData[2]) .OR. llBrowse , .T. , .F.)

IF (llBrowse .OR. !EMPTY(laData[2])) .AND. LASTKEY() = 13
  laData[2] = IIF(llBrowse .OR. EMPTY(laData[2]) , laData[2] , PADL(ALLTRIM(laData[2]) , 6 , "0"))
  SHOW GET laData[2]
  *-- Call global function to handle the action & the screen mode 
  *-- according to this action.
  =gfSeekRec()
  *-- Blank the LC# array element to get a seq. no. upon saving.
  laData[2] = IIF(laScrMode[4] , SPACE(6) , laData[2])
ENDIF
llBrowse = .F.

*-- Fill with the LC type.
laData[1] = lcFromMod

SELECT LC

*!*************************************************************
*! Name      : lfwData_3
*! Developer : Reham Al-Allamy
*! Date      : 02/05/1998
*! Purpose   : When function of the vendor code.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwData_3()
*!*************************************************************
*
FUNCTION lfwData_3

*-- Store the current vendor code.
lcOldValue = laData[3]

*!*************************************************************
*! Name      : lfvData_3
*! Developer : Reham Al-Allamy
*! Date      : 02/05/1998
*! Purpose   : Valid function of the vendor code.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_3()
*!*************************************************************
*
FUNCTION lfvData_3
PRIVATE lcbrfields , lcFile_ttl

*-- If entered differnt vendor code.
IF !llBrowse .AND. laData[3]=lcOldValue
  RETURN
ENDIF

IF llBrowse .OR. !EMPTY(laData[3])
  SELECT APVENDOR
  IF llBrowse .OR. !SEEK(laData[3] , "APVENDOR")
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    
    *-- Set browse fields for browsing vendors.
    lcbrfields = "CVENDCODE :9:H='Vendor' , CVENCOMP  :30:H='Name',"+;
                 "cPhoneNo  :17:H='Phone' , cVenOurAc :30:H='Our Account' "
    
    *-- Assign browse window name for vendors.
    lcFile_ttl = "Vedors"
    
    *-- Browse the available vendors.
    IF gfBrows('','cVendCode,cVenComp', 'laVendors','Vendors')
      laData[3]  = APVENDOR.CVENDCODE
      lcVendName = APVENDOR.CVENCOMP
    ELSE
      laData[3] = lcOldValue
    ENDIF
  ELSE
    lcVendName = APVENDOR.CVENCOMP
  ENDIF
ELSE
  laData[3]  = SPACE(8)
  lcVendName = ""
ENDIF

*-- Call local function to update the letter of credit file.
=lfvUpd_LC()

llBrowse = .F.

*-- Refresh the vendor code & its name objects.
SHOW GET laData[3]
SHOW GET lcVendName

*!*************************************************************
*! Name      : lfwData_7
*! Developer : Reham Al-Allamy
*! Date      : 05/03/1999
*! Purpose   : When function for expire date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwData_7()
*!*************************************************************
*B602774,1 Reham On 05/03/99
*B602774,1 Add when function for the expired date.
*!*************************************************************
*
FUNCTION lfwData_7

*-- Save the current expire date.
ldOldDate = laData[7]

*!*************************************************************
*! Name      : lfvData_7
*! Developer : Reham Al-Allamy
*! Date      : 05/03/1999
*! Purpose   : Valid function for expire date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_7()
*!*************************************************************
*B602774,1 Reham On 05/03/99
*B602774,1 Add valid function for the expired date.
*!*************************************************************
*
FUNCTION lfvData_7

IF ldOldDate <> laData[7]
  SELECT(lcTmpPOSHd)
  GO TOP
  IF !EOF()
    *-- Lock the file before replacing to grantee the phiscal update of the expire date.
    =RLOCK()
    REPLACE ALL lcExpire  WITH laData[7] ;
                cStatus   WITH 'M'
    UNLOCK
  ENDIF
  *-- Call local function to update the letter of credit file.
  =lfvUpd_LC()
ENDIF

*!*************************************************************
*! Name      : lfvData_8
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Valid function for currency code.
*!*************************************************************
*! Calls     : gfCurrBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_8()
*!*************************************************************
*
FUNCTION lfvData_8
*-- Added condition to prevent user from editing currency 
IF (llBrowse .OR. (!(laData[8] == lcOldValue)))
  lcCurCode = laData[8]
  *-- Call global function to browse the available currencies.
  IF !gfCurrBrow(@lcCurCode)
    laData[8] = lcOldValue
  ELSE
    laData[8] = lcCurCode
  ENDIF

  IF !EMPTY(laData[8])
    *-- Get the exchange rate for this currency.
    laData[9] = gfChkRate('lnCurrUnit' , laData[8] , laData[5],;
                         .F. , gcAct_Comp , .F. , .T.)
    IF laData[8] = gcBaseCurr
      STORE 1 TO laData[9] , laData[85]
    ELSE
      *-- Get the unit for the current currency code.
      laData[85] = IIF(lnCurrUnit = 0 , 1 , lnCurrUnit)
      laData[9]  = IIF(laData[9] = 0 , 1 , laData[9])
    ENDIF
    *-- Got exchange rate sign and unit sign.
    lcUntSin = ' '
    lcExRSin = gfGetExSin(@lcUntSin, laData[8])
    laData[11] = ROUND(laData[83] &lcExRSin laData[9] &lcUntSin laData[85] , 2)
    SHOW GET laData[11]
  ENDIF
ENDIF

*-- Disable the exchange rate if choose the base currency.
lcExRStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  laData[8] <> gcBaseCurr , "ENABLE" , "DISABLE")
laData[9] = IIF(laData[8] = gcBaseCurr , 1 , laData[9])

*-- Refresh the currency & its exchange rate objects.
SHOW GET laData[8]
SHOW GET laData[9] &lcExRStat

llBrowse = .F.

*-- Call local function to update the letter of credit file.
=lfvUpd_LC()

*!*************************************************************
*! Name      : lfwData_9
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : When function for currency exchange rate.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwData_9()
*!*************************************************************
*
FUNCTION lfwData_9

*-- Save the old exchange rate.
lnOldValue = laData[9]

*!*************************************************************
*! Name      : lfvData_9
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Valid function for currency exchange rate.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_9()
*!*************************************************************
*
FUNCTION lfvData_9

IF lnOldValue <>  laData[9] .OR. laData[9] <=0
  *-- The exchange rate should not be equal or less than zero.
  IF laData[9] <=0
    *** The exchange rate must be greater than zero. ***
    *** <  Ok  > ***
    =gfModalGen("INM34091B00000" , "DIALOG")
    laData[9] = IIF(lnOldValue > 0 , lnOldValue , laData[9])
    RETURN (IIF(lnOldValue > 0 , .T. , .F.))
  ELSE
    *-- Replace the forign amount with the balance amount.
    laData[10] = laData[83]
    *-- US Amount will the balance value divided by rate value.
    *laData[11] = (laData[83]/laData[9])
    laData[11] = ROUND(laData[83] &lcExRSin laData[9] &lcUntSin laData[85] , 2)
    
    RETURN .T.
  ENDIF
  SHOW GET laData[10]
  SHOW GET laData[11]
ENDIF

*-- Call local function to update the letter of credit file.
=lfvUpd_LC()

*!*************************************************************
*! Name      : lfDrft2
*! Developer : Reham Al-Allamy
*! Date      : 02/22/1998
*! Purpose   : Function called from check box by draft 2 to enable
*!           : or disable the days sight object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfDrft2()
*!*************************************************************
*
FUNCTION lfDrft2

*-- If check box cbDrft2 was checked , enable the days sight accoding 
*-- to its value.
lcDrf2Stat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. laData[38] = "Y" , "ENABLE" , "DISABLE")
laData[39] = IIF(laData[38] = "Y" , laData[39] , "")
SHOW GET laData[39] &lcDrf2Stat

*!*************************************************************
*! Name      : lfDrft3
*! Developer : Reham Al-Allamy
*! Date      : 02/22/1998
*! Purpose   : Function called from check box by draft 3 to enable
*!           : or disable the days sight & from days objects.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfDrft3()
*!*************************************************************
*
FUNCTION lfDrft3

*-- If check box cbDrft3 was checked , enable the days sight & from days 
*-- objects accoding to its value.
lcDrf3Stat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. laData[40] = "Y" , "ENABLE" , "DISABLE")
laData[41] = IIF(laData[40] = "Y" , laData[41] , "")
laData[42] = IIF(laData[40] = "Y" , laData[42] , "")

SHOW GET laData[41] &lcDrf3Stat
SHOW GET laData[42] &lcDrf3Stat

*!*************************************************************
*! Name      : lfvChkBox
*! Developer : Reham Al-Allamy
*! Date      : 02/10/1998
*! Purpose   : Valid function for all check boxes in the main screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcCurAryEl -> Current array element.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvChkBox()
*!*************************************************************
*
FUNCTION lfvChkBox
PARAMETERS lcCurAryEl
PRIVATE lcCurAryEl , llCurCheck

*-- Get the current check box value.
llCurCheck = EVALUATE(SYS(18))

*-- Save the current check box in the current array element
&lcCurAryEl = IIF(llCurCheck , "Y" , "N") 

*-- Call local function to update the letter of credit file.
=lfvUpd_LC()

*!*************************************************************
*! Name      : lfvBank
*! Developer : Reham Al-Allamy
*! Date      : 02/08/1998
*! Purpose   : Valid function of the bank popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvBank()
*!*************************************************************
*
FUNCTION lfvBank

*-- Validate the Bank code.
laData[6] = laBank[lnBank,2]

*-- Call local function to update the letter of credit file.
=lfvUpd_LC()

*!*************************************************************
*! Name      : lfvAdvBank
*! Developer : Reham Al-Allamy
*! Date      : 02/09/1998
*! Purpose   : Valid function of the Adv. bank popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvAdvBank()
*!*************************************************************
*
FUNCTION lfvAdvBank

*-- Validate the Adv. Bank code.
laData[61] = laAdvBank[lnAdvBank,2]

*-- Call local function to update the letter of credit file.
=lfvUpd_LC()

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Reham Al-Allamy
*! Date      : 02/11/1998
*! Purpose   : READ Activate function of the main screen (POLETCR).
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfReadAct()
*!*************************************************************
*
FUNCTION lfReadAct

*-- If coming from the browse screen, call global function to stop the browse.
IF glFromBrow
  = gfStopBrow()
  *-- Set browse flag to false.
  glFromBrow = .F.
ENDIF

*-- If the screen browse is not active then clear the trapped keys.
IF !INLIST(WONTOP(), lcPOBrTitl) 
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL CTRL+Q
  ON KEY LABEL CTRL+W
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL Ctrl+ENTER
ENDIF

*!*************************************************************
*! Name      : lfDactMain
*! Developer : Reham Al-allamy
*! Date      : 02/11/1998
*! Purpose   : Deactivate function for the base screen. (POLETCR).
*!*************************************************************
*! Calls     : lpTab, lpShTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDactMain()
*!*************************************************************
*
FUNCTION lfDactMain

*-- Set the global flag "glFromBrow" to true only if one of the
*-- screen's browses is active.
glFromBrow = (WONTOP() = lcPOBrTitl)

*-- If the screen browse is active then trap the Tab, ShiftTab, 
*-- Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpShTab
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : Reham Al-Allamy
*! Date      : 02/11/1998
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
*
PROCEDURE lpTab

ON KEY LABEL TAB

DO CASE
  *-- In select or view mode, activate the first enable object in the toolbar.
  CASE laScrMode[1] .OR. laScrMode[2]
    ACTIVATE WINDOW gwcContrl1
    _CUROBJ = OBJNUM(pbTop)
  *-- If edit or add mode, activate push button < NEW PO > in the 4th folder.
  CASE laScrMode[3] .OR. laScrMode[4]
    ACTIVATE WINDOW (lcLetCr4B)
    _CUROBJ = OBJNUM(pbNewPo)
ENDCASE

ON KEY LABEL TAB DO lpTab

*!*************************************************************
*! Name      : lpShTab
*! Developer : Reham Al-Allamy
*! Date      : 02/11/1998
*! Purpose   : Trap of backtab key.
*!*************************************************************
*! Calls     : lpShTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpShTab
*!*************************************************************
*
PROCEDURE lpShTab

ON KEY LABEL BACKTAB

*-- In all the modes activate the close button in the toolbar.
ACTIVATE WINDOW gwcContrl1
_CUROBJ = OBJNUM(pbCls)

ON KEY LABEL BACKTAB DO lpShTab

*!*************************************************************
*! Name      : lfvBrow
*! Developer : Reham Al-Allamy
*! Date      : 02/11/1998
*! Purpose   : Valid function for the PO browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvBrow()
*!*************************************************************
*
FUNCTION lfvBrow

*-- If the current window is not the browse, call global function, stop the browse.
IF !INLIST(WONTOP(), lcPOBrTitl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Reham Al-Allamy
*! Date      : 02/11/1998
*! Purpose   : When function for the PO browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwBrow()
*!*************************************************************
*
FUNCTION lfwBrow
PRIVATE lcCount , lnCount

*-- Get the record pointer of the current record.
SELECT (lcTmpPOSHd)
lnMarker = RECNO(lcTmpPOSHd)

lcEditable = IIF(laScrMode[1] .OR. laScrMode[2] , "DISABLE" , "ENABLE")

*-- Control the new button status according to the current mode.
SHOW GET pbNewPo  &lcEditable

*-- Control the delete & Add to Lc buttons status according to the current mode.
IF BOF() .OR. EOF()
  lcPoNo = ""
  SHOW GET pbRemPo   DISABLE
  SHOW GET pbAdd2LC  DISABLE
  SHOW GET ibPO      DISABLE
  SHOW GET lcPoNo    DISABLE
  FOR lnCount = 1 TO IIF(lcFromMod = "S" , 5 , 4)
    lcCount = ALLTRIM(STR(lnCount))
    cbCost&lcCount  = .F.
    SHOW GET cbCost&lcCount DISABLE
  ENDFOR
ELSE
  lcPoNo = IIF(lcFromMod = "S" , Po , PoMat)
  SHOW GET pbRemPo   &lcEditable
  SHOW GET ibPO      &lcEditable
  SHOW GET lcPoNo    &lcEditable
  FOR lnCount = 1 TO IIF(lcFromMod = "S" , 5 , 4)
    lcCount = ALLTRIM(STR(lnCount))
    IF !EMPTY(lcPoNo)
      cbCost&lcCount = IIF(lcCount $ cLCCstItm , .T. , .F. )
      SHOW GET cbCost&lcCount &lcEditable
    ELSE
      cbCost&lcCount = .F.
      SHOW GET cbCost&lcCount DISABLE
    ENDIF
  ENDFOR
  
  IF &lcTmpPOSHd..cLcStatus = "A"
    SHOW GET pbAdd2LC,1 PROMPT '\<Assign'    &lcEditable
  ELSE
    SHOW GET pbAdd2LC,1 PROMPT '\<Add to LC' &lcEditable
  ENDIF
ENDIF

*-- Refresh the browse screen.
SHOW WINDOW (lcPOBrTitl) REFRESH SAME

*!*************************************************************
*! Name      : lfPOBrow
*! Developer : Reham Al-Allamy
*! Date      : 02/10/1998
*! Purpose   : The purchase orders browse in the 4th folder.
*!*************************************************************
*! Calls     : lfwPOBrow, lfvBrows
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfPOBrow()
*!*************************************************************
*
FUNCTION lfPOBrow

SELECT (lcTmpPOSHd)
lnMarker = RECNO()

*-- Browse the current assigned POs.
*B603348,1 ABD Increase the picture of open amount.[begin]
*BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F. , ;
       PO = IIF(lcFromMod = "S" , PO , POMAT) :H = 'PO#' :10  :R, ;
       Status=IIF(cLcStatus="A","Added   ","Assigned"):H = 'LC Status' :17 :R, ; 
       Entered :H = 'Entered'  :11 :R, ;
       Complete:H = 'Complete' :11 :R, ;
       lnOpn = lfPoOpnAmt() :H = 'Open Amt.' :16:R:P='9999999.999' ;
       WINDOW (lcLetCr4A)  ;
       IN WINDOW (lcLetCr4);
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOEDIT            ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfwBrow()    ;
       VALID :F lfvBrow() ;
       TITLE lcPOBrTitl

BROWSE FIELDS ;
       cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F. , ;
       PO = IIF(lcFromMod = "S" , PO , POMAT) :H = 'PO#' :10  :R, ;
       Status=IIF(cLcStatus="A","Added   ","Assigned"):H = 'LC Status' :17 :R, ; 
       Entered :H = 'Entered'  :11 :R, ;
       Complete:H = 'Complete' :11 :R, ;
       lnOpn = lfPoOpnAmt() :H = 'Open Amt.' :16:R:P='9999999999.999' ;
       WINDOW (lcLetCr4A)  ;
       IN WINDOW (lcLetCr4);
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOEDIT            ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfwBrow()    ;
       VALID :F lfvBrow() ;
       TITLE lcPOBrTitl
       
*B603348,1 ABD 
*!*************************************************************
*! Name      : lfvNewPo
*! Developer : Reham Al-Allamy
*! Date      : 02/24/1998
*! Purpose   : Valid function for push button < New PO > in the
*!           : 4th folder (Assign POs)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvNewPo()
*!*************************************************************
*
FUNCTION lfvNewPo

*-- You have to enter the currency unit first.
IF EMPTY(laData[8]) .AND. llMulCurr
  =gfModalGen("INM34121B00000" , "DIALOG")
  RETURN
ENDIF

*-- Enable the PO object to select a new PO.
lcPoNo  = SPACE(6)
SHOW GET ibPo   ENABLE
SHOW GET lcPoNo ENABLE

*-- Blank & disable all the cost items check boxes.
FOR lnCount = 1 TO IIF(lcFromMod = "S" , 5 , 4)
  lcCount        = ALLTRIM(STR(lnCount))
  cbCost&lcCount = .F.
  SHOW GET cbCost&lcCount DISABLE
ENDFOR

_CUROBJ = OBJNUM(lcPoNo)

*!*************************************************************
*! Name      : lfvRemPo
*! Developer : Reham Al-Allamy
*! Date      : 02/24/1998
*! Purpose   : Valid function for push button < Delete PO > in the
*!           : 4th folder (Assign POs)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvRemPo()
*!*************************************************************
*
FUNCTION lfvRemPo

SELECT (lcTmpPOSHd)
*** Are you sure you want to Remove this PO? ***
*** < Yes > - < No > ***
IF gfModalGen("QRM34100B00006" , "DIALOG") = 1
  *-- Flag to know if the deleted PO was assigned to the current LC or not.
  llAdd = (cLcStatus = 'A')
  REPLACE cLCNo     WITH SPACE(6) ;
          lcExpire  WITH {} ;
          cLcStatus WITH "" ;
          cLcCstItm WITH "" ;
          cStatus   WITH 'M'
  IF llAdd
     =lfLc2Po('D')
  ENDIF
  
  *-- Delete the PO from the temp POs header file.
  SELECT (lcTmpPOSHd)
  DELETE
  GO TOP
  
  *-- Variable hold the records count in the PO header file
  lnPORec = lnPORec - 1
  
  *-- Change the status of the vendor object.
  lcVendStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  lnPORec = 0 , "ENABLE" , "DISABLE")
  SHOW GET laData[3] &lcVendStat
  *-- Change the status of the currency code.

  *B604509,1 KHM 06/05/2001 (Begin) Adding the checking of the multi-currency.
  *lcCurStat  = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  lnPORec = 0 , "ENABLE" , "DISABLE")
  lcCurStat  = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  lnPORec = 0 AND llMulCurr, "ENABLE" , "DISABLE")
  *B604509,1 KHM 06/05/2001 (End)
  
  SHOW GET laData[8] &lcCurStat
  SHOW GET ibCur     &lcCurStat
  
  *-- Refresh the 4th folder objects.
  =lfwBrow()
ENDIF

*!*************************************************************
*! Name      : lfvAdd2LC
*! Developer : Reham Al-Allamy
*! Date      : 02/24/1998
*! Purpose   : Valid function for push button < Add To LC > in the
*!           : 4th folder (Assign POs)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvAdd2LC()
*!*************************************************************
*
FUNCTION lfvAdd2LC

SELECT (lcTmpPOSHd)
IF cLcStatus = 'S'
  *-- Change the status from Assigned to Added.
  =lfLc2Po('A')
ELSE
  *** This is an added PO.  Change the status to be assigned? ***
  *** <  Yes  > - <  No  > ***
  IF gfModalGen("INM34104B00006" , "DIALOG") = 1
    *-- Change the status from added to be assigned.
    =lfLc2Po('S')
  ENDIF
ENDIF

*-- Refresh the POs browse.
=lfwBrow()

*!*************************************************************
*! Name      : lfwPoNo
*! Developer : Reham Al-Allamy
*! Date      : 02/25/1998
*! Purpose   : When function the PO # in the 4th folder.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfwPoNo()
*!*************************************************************
*
FUNCTION lfwPoNo

*-- save old Po.
lcOldPo = lcPoNo

*!*************************************************************
*! Name      : lfvPoNo
*! Developer : Reham Al-Allamy
*! Date      : 02/24/1998
*! Purpose   : Valid function the PO # in the 4th folder.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvPoNo()
*!*************************************************************
*
FUNCTION lfvPoNo

IF llBrowse .OR. (lcOldPo <> lcPoNo .AND. LASTKEY() = 13)
  *-- Validate the entered PO # from the 
  SELECT (lcPOHdrFil)
  *-- Change the order to the vendor + type + PO#.
  IF lcFromMod = "S"
    SET ORDER TO Poshdrv
  ELSE
    SET ORDER TO Pofhdrv
  ENDIF
  
  IF llBrowse .OR. !EMPTY(lcPoNo)
    lcPoNo = PADL(ALLTRIM(lcPoNo) , 6 , "0")
    *-- If the entered PO # not valid.
    IF llBrowse .OR. !SEEK(laData[3] + "P" + lcPoNo)
      lcPoNo = SPACE(6)
      lcVend = laData[3]
      IF lcFromMod = "S"
        *-- If called from style purchase order module, 
        *-- browse available Style POs.
        DO POSBROW WITH lcPoNo , lcVend , "P"
      ELSE
        *-- If called from material purchase order module, 
        *-- browse available material POs.
        =POFBROW ("P" , @lcVend , @lcPoNo)
      ENDIF
    ENDIF
    
    *-- If PO selected.
    IF !EMPTY(lcPoNo)
      DO CASE
        *-- Select only PO with status open.
        CASE &lcPOHdrFil..Status <> 'O'
          *** Only the open POs can be accepted. ***
          *** <  Ok  > ***
          =gfModalGen("INM34101B00000" , "DIALOG")
          lcPoNo = SPACE(6)
        *-- Select POs has the same LC currency.
        CASE llMulCurr .AND. (!(&lcPOHdrFil..cPriceCur = laData[8]) .OR. !(&lcPOHdrFil..cDutyCur = laData[8]))
          *** Price currency and duty currency for the ***
          *** selected PO has to be the same like the  ***
          *** current LC currency : {laData[8]}        ***
          *** <  Ok  > ***
          =gfModalGen("INM34106B00000" , "DIALOG" , laData[8])
          lcPoNo = SPACE(6)
        *-- Do not select PO used in another PO.
        CASE !EMPTY(&lcPOHdrFil..cLcNo) .AND. ;
             !SEEK(SPACE(6)+IIF(lcFromMod = "S" , &lcPOHdrFil..Po ,&lcPOHdrFil..PoMat) ,lcTmpPOSHd)
          *** This PO is already used in LC : {&lcPOHdrFil..cLcNo} ***
          *** <  Ok  > ***
          =gfModalGen("INM34102B00000" , "DIALOG" , &lcPOHdrFil..cLcNo)
          lcPoNo = SPACE(6)
        *-- Do not select PO already used in the current LC.
        CASE SEEK(laData[2] + &lcPOHdrFil..cLcNo , lcTmpPOSHd) .AND. !EMPTY(&lcTmpPOSHd..cLcStatus)
          *** This PO is already added to the current LC. ***
          *** <  Ok  > ***
          =gfModalGen("INM34103B00000" , "DIALOG")
          lcPoNo = SPACE(6)
        *-- If valid PO, Add its record in the temp. PO header file.
        OTHERWISE
          SELECT (lcPOHdrFil)
          DECLARE laPO[1]
          SCATTER TO laPO MEMO
          *-- Ad the PO if it was not exist.
          SELECT (lcTmpPOSHd)
          IF !SEEK(laData[2]+lcPoNo)
            IF lcFromMod = "S"
              LOCATE FOR EMPTY(Po)
            ELSE
              LOCATE FOR EMPTY(PoMat)
            ENDIF
            IF !FOUND()
              APPEND BLANK
            ENDIF
            GATHER MEMO FROM laPO 
          ENDIF
          REPLACE cLCNo     WITH laData[2] ;
                  lcExpire  WITH laData[7] ;
                  cLcStatus WITH 'S'       ;
                  cLCCstItm WITH IIF(EMPTY(cLCCstItm) , "1" , cLCCstItm) ;
                  cStatus   WITH 'M'       ;
                  nRecNo    WITH RECNO(lcPOHdrFil)
          
          *-- Refresh the cost items check boxes.
          FOR lnCount = 1 TO IIF(lcFromMod = "S" , 5 , 4)
            lcCount        = ALLTRIM(STR(lnCount))
            cbCost&lcCount = IIF(lcCount $ cLCCstItm , .T. , .F.)
            SHOW GET cbCost&lcCount ENABLE
          ENDFOR
          *-- Refresh the POs browse.
          =lfwBrow()
          
          *-- Variable hold the records count in the PO header file
          lnPORec = lnPORec + 1
          
          *-- Change the status of the currency code.
          
          *B604509,1 KHM 06/05/2001 (Begin) Adding the checking of the multi-currency
          *lcCurStat  = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  lnPORec = 0 , "ENABLE" , "DISABLE")
          lcCurStat  = IIF((laScrMode[3] .OR. laScrMode[4]) .AND.  lnPORec = 0 AND llMulCurr, "ENABLE" , "DISABLE")
          *B604509,1 KHM 06/05/2001 (End)
          
          SHOW GET laData[8] &lcCurStat
          SHOW GET ibCur     &lcCurStat
          
          *-- Change ths status of the vendor code.
          lcVendStat = "DISABLE"
          SHOW GET laData[3] DISABLE
      ENDCASE
    ENDIF
  ENDIF
ELSE
  lcPoNo = lcOldPo
ENDIF

SHOW GET lcPoNo
llBrowse = .F.

*!*************************************************************
*! Name      : lfvCostItm
*! Developer : Reham Al-Allamy
*! Date      : 02/24/1998
*! Purpose   : Valid function for all the cost items check boxes
*!           : to determine which cost items will be included in
*!           : the PO amount.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : lcCostNo -> Which cost item check box.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvCostItm()
*!*************************************************************
*
FUNCTION lfvCostItm
PARAMETERS lcCostNo
PRIVATE lcCostNo

SELECT (lcTmpPOSHd)
IF cbCost&lcCostNo
  *-- If check the cost item check box, include its cost item to the 
  *-- assigned PO.
  REPLACE cLCCstItm WITH IIF(lcCostNo $ ALLTRIM(cLCCstItm) , ;
                             cLCCstItm , ALLTRIM(cLCCstItm)+lcCostNo)
ELSE
  *-- If uncheck the cost item check box, truncate its cost item from the
  *-- assigned PO.
  REPLACE cLCCstItm WITH IIF(lcCostNo $ ALLTRIM(cLCCstItm) , ;
                            STRTRAN(cLCCstItm , lcCostNo , "") , cLCCstItm)
ENDIF

*-- you can not add or assign a PO without at least one cost item.
IF EMPTY(cLCCstItm)
  *** You have to check at least one cost item. ***
  *** <  Ok  > ***
  =gfModalGen("INM34105B00000" , "DIALOG")
  cbCost&lcCostNo = .T.
  REPLACE cLCCstItm WITH lcCostNo
  SHOW GET cbCost&lcCostNo
ENDIF

*-- Refresh the browse screen to refresh the amount object.
SHOW WINDOW (lcPOBrTitl) REFRESH SAME

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Local function to execute the save procedure.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpSavScr()
*!*************************************************************
*
FUNCTION lpSavScr

*-- Do not save letter of credit without vendor code.
IF EMPTY(laData[3])
  *** Vendor code cannot be empty. ***
  *** < OK > ***
  =gfModalGen("INM34092B00000" , "DIALOG")
  llCSave = .F.
  _CUROBJ = OBJNUM(laData[3])
  RETURN
ENDIF

*-- Do not save empty entered date.
IF EMPTY(laData[5])
  *** You cannot leave the entered date empty. ***
  *** < OK > ***
  =gfModalGen("INM34093B00000" , "DIALOG")
  llCSave = .F.
  RETURN
ENDIF

*-- Check to be sure that there is a bank code entered.
IF EMPTY(laData[6])
  *** Bank code cannot be empty. ***
  *** < OK > ***
  =gfModalGen("INM34094B00000" , "DIALOG")
  llCSave = .F.
  RETURN
ENDIF

*-- The exchange rate should not be equal or less than zero.
IF laData[9] <=0
  *** The exchange rate must be greater than zero. ***
  *** <  Ok  > ***
  =gfModalGen("INM34091B00000" , "DIALOG")
  llCSave = .F.
  RETURN
ENDIF

*-- Save the save push button in the session processes.
SELECT UNCMSESS
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE cCurrObj WITH 'pbSav'
  UNLOCK
  =RLOCK()
ENDIF

*-- Update the needed variables.
llNoThing = lfUpdVars()

*-- Check if updated the LC data or not.
IF &lcTmp_LC..nSteps < 1
  *-- If add mode, assign a LC no.
  SELECT LC
  IF laScrMode[4]
    laData[2] = gfSequence('cLcNo')
    APPEND BLANK
  ELSE
    =SEEK(laData[1] + laData[2] , "LC")
  ENDIF
  *-- Lock the file before replacing to grantee the phiscal update.
  =RLOCK()
  GATHER FIELDS &lcScFields FROM laData
  *-- Call global function to add audit fields info.
  =gfAdd_Info('LC')
  UNLOCK

  SELECT (lcTmp_LC)
  *-- Lock the file to grantee the phiscal update.
  =RLOCK()
  REPLACE nSteps WITH 1
  UNLOCK
ENDIF

*-- Check if updated the payments & the POs with the LC # or not.
IF &lcTmp_LC..nSteps < 2
  IF laScrMode[4]
    *-- Assign LC # to the assigned POs.
    SELECT (lcTmpPOSHd)
    *-- Lock the file before replacing to grantee the phiscal update.
    =RLOCK()
    REPLACE ALL cLcNo WITH laData[2]
    UNLOCK
    
    *-- Assign LC # to the LC journal records.
    SELECT (lcTmpJornl)
    *-- Lock the file before replacing to grantee the phiscal update.
    =RLOCK()
    REPLACE ALL cLcNo WITH laData[2]
    UNLOCK
  ENDIF
  
  SELECT (lcTmp_LC)
  *-- Lock the file to grantee the phiscal update.
  =RLOCK()
  REPLACE nSteps WITH 2
  UNLOCK
ENDIF

*-- Check if updated the POs in its header file or not.
IF &lcTmp_LC..nSteps < 3
  *-- Update the style purchase order header or Material purchase order header 
  *-- file according to the called module.
  =gfTmp2Mast(lcPOHdrFil , lcTmpPOSHd , 'Update Assigned '+IIF(lcFromMod = 'S' , 'Style' , 'Material')+' Purchase Order With LC# ' + laData[2] + '...')
  
  SELECT (lcTmp_LC)
  *-- Lock the file to grantee the phiscal update.
  =RLOCK()
  REPLACE nSteps WITH 3
  UNLOCK
ENDIF

*-- Check if updated the payments in its journal file or not.
IF &lcTmp_LC..nSteps < 4
  *-- Update Letter of credit journal file.
  =gfTmp2Mast('LcJournl' , lcTmpJornl , 'Update Letter of Credit Journal file...')

  SELECT (lcTmp_LC)
  *-- Lock the file to grantee the phiscal update.
  =RLOCK()
  REPLACE nSteps WITH 4
  UNLOCK
ENDIF

*-- Check if updated the notepad with the current LC notes.
IF &lcTmp_LC..nSteps < 5
  *--B602753,1 HDM 04/07/1999[Start] Stop Calling NotePad Program In lpSavScr as the global save
  *--                         Will Call it

  *-- If add notes for the current letter of credit.
  *** Do you want to add notes for the current letter of credit? ***
  
  *** < Yes > - < No > ***
  *IF laScrMode[4] .AND. gfModalGen("QRM34119B00006" , "DIALOG") = 1
  *  *E301077,18 Reham On 12/30/98   *** Begin ***
  *  *E301077,18 Open the note pad file.
  *  =gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
  *  *E301077,18 Reham On 12/30/98   *** End   ***
  *  
  *  =NOTEPAD('L',laData[2])
  *  
  *  *E301077,18 Reham On 12/30/98   *** Begin ***
  *  =gfCloseFile('NotePad')
  *  *E301077,18 Reham On 12/30/98   *** End   ***
  *ENDIF
  *--B602753,1 HDM 04/07/1999[Start]
  SELECT (lcTmp_LC)
  *-- Lock the file to grantee the phiscal update.
  =RLOCK()
  REPLACE nSteps WITH 5
  UNLOCK
ENDIF

*-- Finish the whole session.
SELECT UNCMSESS
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE STATUS WITH 'C'
  llContinue = .F.
  UNLOCK
ENDIF
lnUnCmSeRc = 0

*-- Inform the user with the created letter of credit no.
IF laScrMode[4]
  *** Letter of Credit has been saved as : {laData[2]}. ***
  *** <   OK   > ***
  =gfModalGen('INM34120B00000','DIALOG' , laData[2])
ENDIF

*-- Flag to know if enter the add mode before or not.
llAddMode  = .F.
llContinue = .F.

SELECT LC

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Local function to execute the delete procedure.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpDelScr()
*!*************************************************************
*
FUNCTION lpDelScr

*-- If there is any POs assigned to the current LC, do not approve deleting.
IF SEEK(laData[2] , lcTmpPOSHd)
  *** POs has been assigned to the current LC.  Cannot delete. ***
  *** < Ok > ***
  =gfModalGen("INM34095B00000" , "DIALOG")
ELSE
  *-- If not completing a previous uncompleting session.
  IF !llContinue
    *-- Create temp. LC file.
    SELECT LC
    =AFIELDS(laTmp_LC)
    lnTmp_LC = ALEN(laTmp_LC,1)
    *-- Add field to know at which step the saving has done.
    DIMENSION laTmp_LC [lnTmp_LC+1,4]
    laTmp_LC[lnTmp_LC + 1 , 1] = 'NSTEPS'
    laTmp_LC[lnTmp_LC + 1 , 2] = 'N'
    laTmp_LC[lnTmp_LC + 1 , 3] = 7
    laTmp_LC[lnTmp_LC + 1 , 4] = 0
    *-- Call global function to create the temp. LC file.
    =gfCrtTmp(lcTmp_LC , @laTmp_LC)
    
    SELECT (lcTmp_LC)
    APPEND BLANK
    GATHER FIELDS &lcScFields FROM laData
    
    *-- Create an "Open" record in the uncomplete session file.
    llNoThing  = IIF(lnUnCmSeRc=0, lfAdUnCmSR("pbDlt"), .T.)
  ENDIF
  
  IF &lcTmp_LC..nSteps < 1
    *-- Blank the LC # in the temp. POs file.
    SELECT(lcTmpPOSHd)
    *-- Lock the file before replacing to grantee the phiscal update.
    =RLOCK()
    REPLACE ALL cLCNo     WITH SPACE(6) ;
                lcExpire  WITH {} ;
                cLcStatus WITH "" ;
                clccstitm WITH "" ;
                cStatus   WITH 'M'
    UNLOCK
    
    SELECT (lcTmp_LC)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
  
  IF &lcTmp_LC..nSteps < 2
    *-- Remove assigning POs from current deleted LC.
    =gfTmp2Mast(lcPOHdrFil , lcTmpPOSHd , 'Remove Assigning '+IIF(lcFromMod = 'S' , 'Style' , 'Material')+' Purchase Order from deleted LC...')
    
    SELECT (lcTmp_LC)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  
  IF &lcTmp_LC..nSteps < 3
    *-- Replace all the records with status "D" to delete the records.
    SELECT(lcTmpJornl)
    *-- Lock the file before replacing to grantee the phiscal update.
    =RLOCK()
    REPLACE ALL cStatus  WITH 'D'
    DELETE ALL
    UNLOCK
    
    SELECT (lcTmp_LC)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF
  
  IF &lcTmp_LC..nSteps < 4
    *-- Delete Letter of credit journal file.
    =gfTmp2Mast('LcJournl' , lcTmpJornl , 'Delete Letter of Credit Journal Data...')
    
    SELECT (lcTmp_LC)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF
  
  IF &lcTmp_LC..nSteps < 5
    *E301077,18 Reham On 12/30/98   *** Begin ***
    *E301077,18 Open the note pad file.
    =gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
    *E301077,18 Reham On 12/30/98   *** End   ***

    *-- Delete the current LC notes from the notepad file.
    SELECT NOTEPAD
    IF SEEK('L'+laData[2])
      BLANK
      DELETE
    ENDIF
    
    *E301077,18 Reham On 12/30/98   *** Begin ***
    =gfCloseFile('NotePad')
    *E301077,18 Reham On 12/30/98   *** End   ***
    
    SELECT (lcTmp_LC)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 5
    UNLOCK
  ENDIF
  
  IF &lcTmp_LC..nSteps < 6
    *-- Delete the current LC record.
    SELECT LC
    IF SEEK(laData[1] + laData[2])
      BLANK
      DELETE
    ENDIF
    
    SELECT (lcTmp_LC)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 6
    UNLOCK
  ENDIF
  
  *-- Finish the whole session.
  SELECT UNCMSESS
  IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
    GOTO lnUnCmSeRc
    REPLACE STATUS WITH 'C'
    llContinue = .F.
    UNLOCK
  ENDIF
  lnUnCmSeRc = 0
  
  STORE .F. TO laScrMode
  STORE .T. TO laScrMode[1]
  SELECT LC
ENDIF

llAddMode  = .F.

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Cancel new credit memo.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpClsScr()
*!*************************************************************
*
FUNCTION lpClsScr

*-- Flag to know if enter the add mode before or not.
llAddMode  = .F.
llContinue = .F.

*-- Save status "I" for the ignored record in the uncompleted session file.
SELECT unCmSess
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE STATUS WITH 'I'
  UNLOCK
ENDIF

*-- Set the record poniter in the uncomplete session file to zero.
lnUnCmSeRc = 0

SELECT LC

*!*************************************************************
*! Name      : lfvLCNotes
*! Developer : Reham Al-Allamy
*! Date      : 02/22/1998
*! Purpose   : Function to call the notepad program.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvLCNotes()
*!*************************************************************
*
FUNCTION lfvLCNotes

*E301077,18 Reham On 12/30/98   *** Begin ***
*E301077,18 Open the note pad file.
=gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
*E301077,18 Reham On 12/30/98   *** End   ***

*-- Call the notepad program to add notes for the current LC.
=NOTEPAD('L',laData[2])

*E301077,18 Reham On 12/30/98   *** Begin ***
=gfCloseFile('NotePad')
*E301077,18 Reham On 12/30/98   *** End   ***

*-- Select the main screen file.
SELECT LC

*!*************************************************************
*! Name      : lfPoOpnAmt
*! Developer : Reham Al-Allamy
*! Date      : 02/10/1998
*! Purpose   : Function to calculate 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  lnOpnAmnt -> Open Amount.
*!*************************************************************
*! Example   :  =lfPoOpnAmt()
*!*************************************************************
*
FUNCTION lfPoOpnAmt
PRIVATE lnOpnAmnt

*-- Select master POs lines.
SELECT (lcPOLinFil)
STORE 0 TO lnOpnAmnt , lnPydAmt

*-- If called from style purchase order module.
DO CASE
  *-- If called from style purchase order module.
  CASE lcFromMod = "S"
    IF SEEK("P" + &lcTmpPOSHd..Po)
      *-- Calculate the open amount from style purchase order line.
      SCAN WHILE cStyType + Po = "P" + &lcTmpPOSHd..Po FOR TranCd <> '3'
        lnOpnAmnt = lnOpnAmnt + IIF(TranCd='1',1,-1) * (PosLn.TotQty * ;
                    ((PosLn.nCost1 * IIF("1" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                     (PosLn.nCost2 * IIF("2" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                     (PosLn.nCost3 * IIF("3" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                     (PosLn.nCost4 * IIF("4" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                     (PosLn.nCost5 * IIF("5" $ &lcTmpPOSHd..cLCCstItm , 1 , 0))))
      ENDSCAN
    ENDIF
    *-- Calculate the paid amount from the open amount.
    SELECT (lcTmpJornl)
    SUM nTrnAmunt FOR cPono = &lcTmpPOSHd..Po .AND. cLCTrnTyp = '5' TO lnPydAmt
  *-- If called from material module.
  CASE lcFromMod = "M"
    IF SEEK("P" + &lcTmpPOSHd..PoMat)
      *-- Calculate the open amount from material purchase order line.
      SCAN WHILE cMatType + PoMat = "P" + &lcTmpPOSHd..PoMat FOR TranCd <> '3'
        lnOpnAmnt = lnOpnAmnt + IIF(TranCd='1',1,-1)*(PofLn.nFabTotQty * ;
                    ((PofLn.nCost1 * IIF("1" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                     (PofLn.nCost2 * IIF("2" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                     (PofLn.nCost3 * IIF("3" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                     (PofLn.nCost4 * IIF("4" $ &lcTmpPOSHd..cLCCstItm , 1 , 0))))
      ENDSCAN
    ENDIF
    *-- Calculate the paid amount from the open amount.
    SELECT (lcTmpJornl)
    SUM nTrnAmunt FOR cPono = &lcTmpPOSHd..PoMat .AND. cLCTrnTyp = '5' TO lnPydAmt
ENDCASE

*-- Subtract the paid amount from the open amount.
lnOpnAmnt = lnOpnAmnt - lnPydAmt

*-- Return with the remain open amount.
SELECT (lcTmpPOSHd)
RETURN IIF(lnOpnAmnt < 0 , 0 , lnOpnAmnt)

*!*************************************************************
*! Name      : lfvSummary
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Call the summary screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSummary()
*!*************************************************************
*
FUNCTION lfvSummary

lnLcBal   = laData[83]   && Total LC Balance.
lnPOAssn  = 0            && Total POs Assigned.
lnPOAdded = 0            && Total POs Added.
lnShpInt  = 0            && Total Shipments in Transit.
lnAvlAmt  = 0            && LC available amount.

SELECT (lcTmpPOSHd)
SCAN
  DO CASE
    *-- If the PO was added.
    CASE cLCStatus = 'A'
      *-- Get all open POs amount for added POs.
      lnPOAdded = lnPOAdded + lfPoOpnAmt()
      
      DO CASE
        *-- If called from style purchase order module.
        CASE lcFromMod = "S"
          *-- Get Shipment in transit amount.
          IF SEEK("P" + &lcTmpPOSHd..Po , "POSLN")
            SELECT POSLN
            SUM (TotQty * ((nCost1 * IIF("1" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                           (nCost2 * IIF("2" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                           (nCost3 * IIF("3" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                           (nCost4 * IIF("4" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                           (nCost5 * IIF("5" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)))) ;
                TO lnShptAmt ;
                REST WHILE cStyType + Po = "P" + &lcTmpPOSHd..Po ;
                       FOR !EMPTY(ShipNo)
            lnShpInt = lnShpInt + lnShptAmt
          ENDIF
        *-- If called from material purchase order module.
        CASE lcFromMod = "M"
          *-- Get the open amount from pofln file.
          IF SEEK("P" + &lcTmpPOSHd..PoMat , "POFLN")
            SELECT POFLN
            SUM (nFabTotQty * ((nCost1 * IIF("1" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                               (nCost2 * IIF("2" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                               (nCost3 * IIF("3" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                               (nCost4 * IIF("4" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)))) ;
                TO lnAllAmt ;
                REST WHILE cMatType + PoMat = "P" + &lcTmpPOSHd..PoMat ;
                 FOR TranCd = "1"
            SUM (nFabTotQty * ((nCost1 * IIF("1" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                               (nCost2 * IIF("2" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                               (nCost3 * IIF("3" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)) + ;
                               (nCost4 * IIF("4" $ &lcTmpPOSHd..cLCCstItm , 1 , 0)))) ;
                TO lnRecAmt ;
                REST WHILE cMatType + PoMat = "P" + &lcTmpPOSHd..PoMat ;
                 FOR TranCd <> "1"
            lnShpInt = lnAllAmt - lnRecAmt
          ENDIF
      ENDCASE
    *-- If the PO was assigned.
    CASE cLCStatus='S'
      *-Get all open POs amount for assigned POs.
      lnPOassn = lnPOassn + lfPoOpnAmt()
  ENDCASE
ENDSCAN

*-- Get available and amendments amounts.
lnAvlAmt = (LnLcBal - lnPOAdded)

*-- Clear the trapped keys.
PUSH KEY
ON KEY

*-- Call LC Summary Screen.
DO (gcScrDir+"POLCSMR.SPX")

*-- Restore the trapped keys.
POP KEY

*-- Select the main screen file.
SELECT LC

*!*************************************************************
*! Name      : lfvPayment
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Call the payments screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPayment()
*!*************************************************************
*
FUNCTION lfvPayment

*-- If called from style purchase order module.
IF lcFromMod = "S"
  *E301077,18 Reham On 12/30/98   *** Begin ***
  *E301077,18 Open the shipment header file.
  =gfOpenFile(gcDataDir+'ShpmtHdr',gcDataDir+'ShpmtHdr','SH')
  *E301077,18 Reham On 12/30/98   *** End   ***

  *-- Collect the shipments for the added POs to the current LC with its payments.
  SELECT PosLn
  SELECT PosLn.Po,PosLn.Shipno,SUM(PosLn.totqty*;
        ((PosLn.nCost1 * IIF("1"$&lcTmpPOSHd..cLCCstItm,1,0))+;
         (PosLn.nCost2 * IIF("2"$&lcTmpPOSHd..cLCCstItm,1,0))+;
         (PosLn.nCost3 * IIF("3"$&lcTmpPOSHd..cLCCstItm,1,0))+;
         (PosLn.nCost4 * IIF("4"$&lcTmpPOSHd..cLCCstItm,1,0))+;
         (PosLn.nCost5 * IIF("5"$&lcTmpPOSHd..cLCCstItm,1,0)))) AS nAmt,;
         SUM(PosLn.totqty * ((PosLn.nCost1 * IIF("1"$&lcTmpPOSHd..cLCCstItm,1,0))+;
         (PosLn.nCost2 * IIF("2"$&lcTmpPOSHd..cLCCstItm,1,0))+;
         (PosLn.nCost3 * IIF("3"$&lcTmpPOSHd..cLCCstItm,1,0))+;
         (PosLn.nCost4 * IIF("4"$&lcTmpPOSHd..cLCCstItm,1,0))+;
         (PosLn.nCost5 * IIF("5"$&lcTmpPOSHd..cLCCstItm,1,0)))) AS nShpAmt,;
         ShpmtHdr.Status,ShpmtHdr.Entered,ShpmtHdr.Cartons,;
         ShpmtHdr.Eta,ShpmtHdr.cVessel;
    FROM PosLn,(lcTmpPOSHd),ShpmtHdr;
   WHERE PosLn.Po = &lcTmpPOSHd..Po AND !EMPTY(PosLn.shipno) AND ;
         &lcTmpPOSHd..clcStatus = 'A' AND PosLn.ShipNo = ShpmtHdr.ShipNo ;
   GROUP BY PosLn.Shipno ;
    INTO DBF (gcWorkDir+lcTmpPayTr)
  
  *-- Set index in the shipment #.
  INDEX ON Shipno TAG (lcTmpPayTr) UNIQUE
  
  *-- If there was no records collected.
  IF _TALLY = 0
    *** No shipments found for the added POs in this LC. ***
    *** <  Ok  > ***
    =gfModalGen("INM34097B00000" , "DIALOG")
    SELECT LC
    RETURN
  ENDIF
  
  *-- Update the shipment payments from the LC journal file.
  SELECT (lcTmpJornl)
  SCAN FOR cLCTrnTyp = '5'
    IF SEEK(&lcTmpJornl..cShipNo , lcTmpPayTr)
      SELECT (lcTmpPayTr)
      REPLACE nAmt WITH IIF(nAmt > &lcTmpJornl..nTrnAmunt , ;
                            (nAmt - &lcTmpJornl..nTrnAmunt) , 0)
    ENDIF
  ENDSCAN
  SELECT (lcTmpPayTr)
  GO TOP
ELSE
  *-- If called from material module.
  SELECT (lcTmpPOSHd)
  GO TOP
  *-- If there was no records collected.
  IF BOF() .OR. EOF() .OR. EMPTY(&lcTmpPOSHd..PoMat)
    *** No added POs for the current LC. ***
    *** <  Ok  > ***
    =gfModalGen("INM34118B00000" , "DIALOG")
    SELECT LC
    RETURN
  ELSE
    USE (gcWorkDir+lcTmpPOSHd) ALIAS lcTmpPOFH IN 0 AGAIN ORDER lcTmpPOSHd
  ENDIF
ENDIF

lcShipNo   = SPACE(6)      && Hold shipment #.
ldPayDate  = {}            && Hold the payment date.
lcPayRefer = SPACE(10)     && Hold reference.
lnPayAmt   = 0             && Hold the payment amount.
lnTotPo    = 0             && Hold total POs amount.
llCancel   = .F.           && Hold the cancel flag.

*-- Clear the trapped keys.
PUSH KEY
ON KEY

*-- Call LC Payment Screen.
DO (gcScrDir+"POLCPAY.SPX")

*-- Restore the trapped keys.
POP KEY

*-- If called from materials module.
IF lcFromMod = "M" .AND. USED("lcTmpPOFH")
  USE IN lcTmpPOFH
ENDIF

*-- Replace the forign amount with the balance amount.
laData[10] = laData[83]
*-- US Amount will the balance value divided by rate value.
*laData[11] = (laData[83]/laData[9])
laData[11]  = ROUND(laData[83] &lcExRSin laData[9] &lcUntSin laData[85] , 2)

SHOW GET laData[10]
SHOW GET laData[11]

*-- Select the main screen file.
SELECT LC

*!*************************************************************
*! Name      : lfvAmend
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Call the Amendments screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAmend()
*!*************************************************************
*
FUNCTION lfvAmend

lnAmdAmt    = 0           && Amendment amount.
lcAmdRefer  = SPACE(10)   && Reference variable.
ldAmdDate   = gdSysDate   && Amendment date.
*-- Store the current LC balance to current LC balance variable 
*-- and new LC balance.
STORE  laData[83] TO lnLcBal , lnLcNBal

*-- Clear the trapped keys.
PUSH KEY
ON KEY

*-- Call LC Amendments Screen.
DO (gcScrDir+"POLCAMD.SPX")

*-- Restore the trapped keys.
POP KEY

*-- Replace the forign amount with the balance amount.
laData[10] = laData[83]
*-- US Amount will the balance value divided by rate value.
*laData[11] = (laData[83]/laData[9])
laData[11] = ROUND(laData[83] &lcExRSin laData[9] &lcUntSin laData[85] , 2)

SHOW GET laData[10]
SHOW GET laData[11]

*-- Select the main screen file.
SELECT LC

*!*************************************************************
*! Name      : lfvJournal
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Call Journal browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvJournal()
*!*************************************************************
*
FUNCTION lfvJournal
PRIVATE lcBrFields

SELECT (lcTmpJornl)
lcBrFields = "cPOno    :8  :H='Po #',"+;
             "cShipno  :12 :H='ShipNo',"+;
             "lcDummy = laTrnType[ASUBSCRIPT(laTrnType , ASCAN(laTrnType,cLCTrnTyp) , 1 ) , 1] :15 :H='Tran. Type',"+;
             "dTranDate :11 :H='Date',"+;
             "cReference :15 :H='Reference',"+; 
             "nTrnAmunt :P='99999999.99' :14 :H='Amount',"+;
             "nLCBal    :P='99999999.99' :14 :H='Balance'"

*-- Call global aria browse to view the available LC Journal records.
=ARIABROW('','LC Journal Transactions' , gnBrHsRow1, gnBrHsCol1 , ;
          gnBrHSRow2 , gnBrHSCol2 , '' , ;
          'Fi\<nd; Or\<der by; \<Descending; Fi\<lter; ; \!\?\<Ok')

*-- Select the main screen file.
SELECT LC

*!*************************************************************
*! Name      : lfvShip
*! Developer : Reham Al-Allamy
*! Date      : 02/19/1998
*! Purpose   : Call shipments browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShip()
*!*************************************************************
*
FUNCTION lfvShip
PRIVATE lcBrFields

*E301077,18 Reham On 12/30/98   *** Begin ***
*E301077,18 Open the shipment header file.
=gfOpenFile(gcDataDir+'ShpmtHdr',gcDataDir+'ShpmtHdr','SH')
*E301077,18 Reham On 12/30/98   *** End   ***

*-- Collect the style purchase order lines & its related data from 
*-- the shipment file for all the assigned POs.
SELECT PosLn.Po , PosLn.Shipno , SUM(PosLn.totqty) AS TotQty , ;
       SUM(PosLn.totqty * ;
       ((PosLn.nCost1 * IIF("1"$&lcTmpPOSHd..cLCCstItm,1,0))+;
        (PosLn.nCost2 * IIF("2"$&lcTmpPOSHd..cLCCstItm,1,0))+;
        (PosLn.nCost3 * IIF("3"$&lcTmpPOSHd..cLCCstItm,1,0))+;
        (PosLn.nCost4 * IIF("4"$&lcTmpPOSHd..cLCCstItm,1,0))+;
        (PosLn.nCost5 * IIF("5"$&lcTmpPOSHd..cLCCstItm,1,0)))) AS nAmt , ;
       000000.00 AS nShipAmt , ShpmtHdr.Status , ShpmtHdr.Entered , ;
       ShpmtHdr.Cartons , ShpmtHdr.Eta , ShpmtHdr.cVessel ;
  FROM POSLN , (lcTmpPOSHd) , ShpmtHdr ;
 WHERE PosLn.Po = &lcTmpPOSHd..Po   .AND. ;
       !EMPTY(PosLn.ShipNo)         .AND. ;
       &lcTmpPOSHd..cLcStatus = 'A' .AND. ;
       PosLn.ShipNo = ShpmtHdr.ShipNo ;
 GROUP BY PosLn.Shipno ; 
  INTO DBF (gcWorkDir+lcTmpTrans)

*-- Set an index on shimpment # in the temp. transaction file.
INDEX ON Shipno TAG (lcTmpTrans) UNIQUE

*-- If there is no shipment in transit for the assigned POs.
IF _TALLY = 0
  *** No shipment in transit to browse. ***
  *** <  Ok  > ***
  =gfModalGen("INM34096B00000" , "DIALOG")
  RETURN
ENDIF

*-- For all the paid lc journal replace its amount for its related shipment no.
SELECT (lcTmpJornl)
SCAN FOR cLCTrnTyp = '5'
  IF SEEK(&lcTmpJornl..cShipNo , lcTmpTrans)
    SELECT (lcTmpTrans)
    REPLACE nShipAmt WITH nShipAmt + &lcTmpJornl..nTrnAmunt
  ENDIF
  SELECT (lcTmpJornl)
ENDSCAN

*-- Prepare the browse fields for the temp. transaction file.
SELECT (lcTmpTrans)
GO TOP
lcBrFields = "ShipNo   :12 :H='ShipNo'," +;
             "Status   :10 :H='Status',"+;
             "TotQty   :16:P='99999999.99' :H='Open Qty.',"+;
             "nAmt     :16:P='99999999.99' :H='Open Amount',"+;
             "nShipAmt :16:P='99999999.99' :H='Paid Amount',"+;
             "cVessel  :30:H='Airline/Vessel'"

*-- Call global aria browse to view the available shipments.
=ARIABROW('','Shipments In Transit', gnBrHsRow1, gnBrHsCol1, ;
          gnBrHSRow2, gnBrHSCol2, '' , ;
          'Fi\<nd; Or\<der by; \<Descending; Fi\<lter; ; \!\?\<Ok')

*-- Select the base file.
SELECT LC

**************************************************************************
*******************  Functions Related To POLCPAY.SCX ********************
**************************************************************************

*!*************************************************************
*! Name      : lfvPayShip
*! Developer : Reham Al-Allamy
*! Date      : 02/24/1998
*! Purpose   : Function to validate the shipment # in the 
*!           : payment screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvPayShip()
*!*************************************************************
*
FUNCTION lfvPayShip
PRIVATE lcBrFields , lcFil_ttl , lnPoFAmt

*-- If called from style purchase order module.
IF lcFromMod = "S"
  IF llBrowse .OR. !EMPTY(lcShipNo)
    SELECT (lcTmpPayTr)
    IF llBrowse .OR. !SEEK(lcShipNo)
      IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
        GO RECNO(0)
      ELSE
        GO TOP
      ENDIF
      
      lcBrFields = "ShipNo :9 :H='ShipNo',"+;
                   "Status :8 :H='Status',"+;
                   "Entered :12 :H='Entered',"+;
                   "Cartons :7 :P='99999' :H='Carton',"+; 
                   "Eta :12 :H='ETA',"+;
                   "cVessel :20 :H='Airline/Vessel',"+;
                   "nShpAmt :16 :P='99999999.99' :H='Shipmnt Amt',"+;
                   "nAmt    :16 :P='99999999.99' :H='Amount'"
      
      *-- Assign browse window name for vendors.
      lcFile_ttl = "Shipments"
      *-- Array will hold the selected shipment# & its amount that will 
      *-- return from the browse.
      DECLARE laShipNo[2]
      
      *-- Call global function to browse the available shipment.
      IF gfBrows('','ShipNo,nAmt', 'laShipNo',lcFile_ttl)
        lcShipNo   = laShipNo[1]
        lnTotPo    = laShipNo[2]
        lnPayAmt   = lnTotPo
        lcPayRefer = SPACE(10)
      ELSE
        *-- If nothing was selected from the shipment browse.
        lcShipNo   = SPACE(6)
      ENDIF
    ELSE
      *-- If enter valid shipment no.
      lnTotPo    = nAmt
      lnPayAmt   = lnTotPo
      lcPayRefer = SPACE(10)
    ENDIF
  ELSE
    *-- If nothing was entered in the shipment # object.
    lcShipNo = SPACE(6)
  ENDIF
ELSE
  *-- If called from material module.
  IF llBrowse .OR. !EMPTY(lcShipNo)
    SELECT lcTmpPOFH
    IF llBrowse .OR. !SEEK(laData[1] + lcShipNo)
      IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
        GO RECNO(0)
      ELSE
        GO TOP
      ENDIF
      
      lcBrFields = "POMat :9 :H='Po no',"+;
                   "Vendor :9 : H='Vendor',"+;
                   "Status :7 :H='Status',"+;
                   "Entered :12 :H='Entered',"+;
                   "cLcStatus=IIF(cLcStatus='A','Added   ','Assigned') :9 :H='LC Status',"+;
                   "nCost1 :15 :P='999999999.999' :H='Price Cost',"+;
                   "nCost2 :15 :P='999999999.999' :H='Freight Cost',"+;
                   "nCost3 :15 :P='999999999.999' :H='Tax Cost',"+;
                   "nCost4 :15 :P='999999999.999' :H='Quota Cost'"
      
      *-- Assign browse window name for vendors.
      lcFile_ttl = "Style Purchase Orders"
      *-- Array will hold the selected shipment# & its amount that will 
      *-- return from the browse.
      DECLARE laShipNo[5]
      
      *-- Call global function to browse the available shipment.
      IF gfBrows('','nCost1,nCost2,nCost3,nCost4,PoMat', 'laShipNo',lcFile_ttl)
        lcShipNo   = laShipNo[5]
        lnTotPo    = (laShipNo[1] * IIF("1" $ cLCCstItm , 1 , 0)) + ;
                     (laShipNo[2] * IIF("2" $ cLCCstItm , 1 , 0)) + ;
                     (laShipNo[3] * IIF("3" $ cLCCstItm , 1 , 0)) + ;
                     (laShipNo[4] * IIF("4" $ cLCCstItm , 1 , 0))
        lnPayAmt   = lnTotPo
        lcPayRefer = SPACE(10)
      ELSE
        *-- If nothing was selected from the shipment browse.
        lcShipNo   = SPACE(6)
      ENDIF
    ELSE
      *-- If enter valid shipment no.
      lnTotPo    = (nCost1 * IIF("1" $ cLCCstItm , 1 , 0)) + ;
                   (nCost2 * IIF("2" $ cLCCstItm , 1 , 0)) + ;
                   (nCost3 * IIF("3" $ cLCCstItm , 1 , 0)) + ;
                   (nCost4 * IIF("4" $ cLCCstItm , 1 , 0))
      lnPayAmt   = lnTotPo
      lcPayRefer = SPACE(10)
    ENDIF
    *-- If there is material PO selected 
    IF !EMPTY(lcShipNo)
      *-- Only added POs should be selected.
      IF lcTmpPOFH.cLCStatus = "A"
        SELECT (lcTmpJornl)
        lnPoFAmt = 0
        *-- Calculate the payments for the selected material PO #.
        SUM nTrnAmunt FOR cLCTrnTyp = '5' .AND. cPoNo = lcShipNo TO lnPoFAmt
        *-- Subtract the payments amounts from the PO total amount.
        lnTotPo  = MAX(0 , lnTotPo - lnPoFAmt)
        lnPayAmt = lnTotPo
        SELECT lcTmpPOFH
      ELSE
        *** This is an assigned PO.  Only added POs are allowed. ***
        *** <  OK  > ***
        =gfModalGen("INM34117B00000" , "DIALOG")
        lcShipNo = SPACE(6)
      ENDIF
    ENDIF
  ELSE
    *-- If nothing was entered in the shipment # object.
    lcShipNo = SPACE(6)
  ENDIF
ENDIF

llBrowse = .F.

IF EMPTY(lcShipNo)
  *-- If there was nothing entered.
  llCancel   = .F.    && Flag to know that there is data entered.
  *-- Blank the payment objects screen till enter valid shipment #.
  lcShipNo   = SPACE(6)
  ldPayDate  = {}
  lcPayRefer = SPACE(10)
  lnPayAmt   = 0
  SHOW GET pbPayCls,1 PROMPT lcClsBmp ENABLE
  SHOW GET pbPayAcpt  DISABLE
  SHOW GET ldPayDate  DISABLE
  SHOW GET lnPayAmt   DISABLE
  SHOW GET lcPayRefer DISABLE
ELSE
  *-- If there was valid shipment no. entered.
  llCancel   = .T.    && Flag to know that there is data entered.
  ldPayDate  = gdSysDate
  SHOW GET pbPayCls,1 PROMPT lcCanBmp ENABLE
  SHOW GET pbPayAcpt  ENABLE
  SHOW GET ldPayDate  ENABLE
  SHOW GET lnPayAmt   ENABLE
  SHOW GET lcPayRefer ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvPayAmt
*! Developer : Reham Al-Allamy
*! Date      : 02/24/1998
*! Purpose   : Function to validate the payment amount in the 
*!           : payment screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvPayAmt()
*!*************************************************************
*
FUNCTION lfvPayAmt

*-- Payment amount should not equal 0 and should not exceed total added Po amount.
IF lnPayAmt <= 0 .OR. lnPayAmt > lnTotPo
  *** The paid amount has to be greater than zero and ***
  *** not exceeding the total amount of added POs. ***
  *** <  Ok  > ***
  =gfModalGen("INM34098B00000" , "DIALOG")
ENDIF

*!*************************************************************
*! Name      : lfvPayAcpt
*! Developer : Reham Al-Allamy
*! Date      : 02/24/1998
*! Purpose   : Valid function of the < Accept > button in the 
*!           : payment screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvPayAcpt()
*!*************************************************************
*
FUNCTION lfvPayAcpt

*-- Payment amount should not equal 0 and should not exceed total added Po amount.
IF lnPayAmt <= 0 .OR. lnPayAmt > lnTotPo
  *** The paid amount has to be greater than zero and ***
  *** not exceeding the total amount of added POs. ***
  *** <  Ok  > ***
  =gfModalGen("INM34098B00000" , "DIALOG")
  RETURN
ENDIF

*-- Update LC Balance To be saved in the Lc file.
laData[83] = laData[83] - lnPayAmt

*-- Add record in the LC journal file.
SELECT (lcTmpJornl)
APPEND BLANK
*-- Lock the file before replacing to grantee the phiscal update.
=RLOCK()
REPLACE cLCType    WITH laData[1]  ;
        cLcNo      WITH laData[2]  ;
        cShipNo    WITH IIF(lcFromMod = "S" , lcShipNo , "") ;
        cPoNo      WITH IIF(lcFromMod = "M" , lcShipNo , "") ;
        cLCTrnTyp  WITH '5'        ;
        dTranDate  WITH ldPayDate  ;
        nTrnAmunt  WITH lnPayAmt   ;
        nLcBal     WITH laData[83] ;
        cReference WITH lcPayRefer ;
        cStatus    WITH 'A'
*-- Call global function to add audit fields info.
=gfAdd_Info()
UNLOCK

*-- If called from style purchase order module.
IF lcFromMod = "S"
  *-- Update the shipment temp. file.
  SELECT (lcTmpPayTr)
  REPLACE nAmt WITH IIF(nAmt > lnPayAmt , (nAmt - lnPayAmt) , 0)
ENDIF

*-- Prepare payment screen objects to accept new entry.
lcShipNo   = SPACE(6)
ldPayDate  = {}
lcPayRefer = SPACE(10)
lnPayAmt   = 0

*-- Disable the payments objects till enter new shipment #.
SHOW GET pbPayCls,1 PROMPT lcClsBmp ENABLE
SHOW GET pbPayAcpt  DISABLE
SHOW GET ldPayDate  DISABLE
SHOW GET lnPayAmt   DISABLE
SHOW GET lcPayRefer DISABLE
_CUROBJ = OBJNUM(lcShipNo)

*!*************************************************************
*! Name      : lfvPayCan
*! Developer : Reham Al-Allamy
*! Date      : 02/24/1998
*! Purpose   : Valid function of the < Close > button in the 
*!           : payment screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvPayCan()
*!*************************************************************
*
FUNCTION lfvPayCan

*-- If current object displayed is < Cancel > , cancel entering 
*-- the current shipment #.
IF llCancel
  *-- Prepare payment screen objects to accept new entry.
  lcShipNo   = SPACE(6)
  ldPayDate  = {}
  lcPayRefer = SPACE(10)
  lnPayAmt   = 0
  *-- Disable the payments objects till enter new shipment #.
  SHOW GET pbPayCls,1 PROMPT lcClsBmp ENABLE
  SHOW GET pbPayAcpt  DISABLE
  SHOW GET ldPayDate  DISABLE
  SHOW GET lnPayAmt   DISABLE
  SHOW GET lcPayRefer DISABLE
  _CUROBJ = OBJNUM(lcShipNo)
ELSE
  *-- If current object display is < Close > , Close the payment screen.
  CLEAR READ
ENDIF


**************************************************************************
*******************  Functions Related To POLCAMD.SCX ********************
**************************************************************************

*!*************************************************************
*! Name      : lfvAmdAmt
*! Developer : Reham Al-Allamy
*! Date      : 02/24/1998
*! Purpose   : Valid function for the amendment value in the
*!           : amendment screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvAmdAmt()
*!*************************************************************
*
FUNCTION lfvAmdAmt

*-- Add the amendment value to the current LC balance.
lnLcNBal = lnLcBal + lnAmdAmt

*-- Refresh the new LC balance object.
SHOW GET lnLcNBal

*-- If the new LC balance different from the current, enable the
*-- < Ok > button to give the ability to accept the new balance.
IF LnLcNBal = LnLcBal
  SHOW GET pbAmdOk DISABLE
ELSE
  SHOW GET pbAmdOk ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvAmdOk
*! Developer : Reham Al-Allamy
*! Date      : 02/24/1998
*! Purpose   : Valid function for push button < OK > in the
*!           : amendment screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvAmdOk()
*!*************************************************************
*
FUNCTION lfvAmdOk

*-- Update the current LC balance with the amendment value.
laData[83] = laData[83] + lnAmdAmt

*-- Add the current amendment to the LC journal file.
SELECT (lcTmpJornl)
APPEND BLANK
*-- Lock the file before replacing to grantee the phiscal update.
=RLOCK()
REPLACE cLCType    WITH laData[1] ;
        cLcNo      WITH laData[2] ;
        cLCTrnTyp  WITH '6'       ;
        dTranDate  WITH ldAmdDate ;
        nTrnAmunt  WITH ABS(lnAmdAmt) ;
        nLcBal     WITH laData[83]    ;
        cReference WITH lcAmdRefer    ;
        cStatus    WITH 'A'
*-- Call global function to add audit fields info.
=gfAdd_Info()
UNLOCK

*-- Blank the amendments objects to prepare entering another amendment.
lnAmdAmt    = 0
lcAmdRefer  = SPACE(10)
ldAmdDate   = gdSysDate
STORE  laData[83] TO lnLcBal , lnLcNBal

*-- Refresh the amendments objects.
SHOW GET pbAmdOk   DISABLE
SHOW GET lnLcBal
SHOW GET lnLcNBal
SHOW GET ldAmdDate
SHOW GET lnAmdAmt
_CUROBJ = OBJNUM(ldAmdDate)

*!*************************************************************
*! Name      : lfLc2Po
*! Developer : Reham Al-Allamy
*! Date      : 02/25/1998
*! Purpose   : Function to assign or add PO to the current LC.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : lcPoType -> Po transaction type.
*!                    : A -> For adding PO.
*!                    : S -> For assigning PO.
*!                    : D -> For removing PO.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfLc2Po()
*!*************************************************************
*
FUNCTION lfLc2Po
PARAMETERS lcPoType
PRIVATE lnHdrRec , lnLinRec

*-- Get PO amount and balance.
lnPOamt  = lfPoOpnAmt()
lnLcBal  = laData[83]
lnTPOamt = 0

*-- Save the current record pointers in the PO header.
lnHdrRec = IIF(!EOF() , RECNO(lcTmpPOSHd) , 0)

*-- Get the total Po amount.
SELECT (lcTmpPOSHd)
SCAN FOR cLCStatus = 'A'
  lnTPOamt = lnTPOamt + lfPoOpnAmt()
ENDSCAN

*-- Restore header record pointer.
IF lnHdrRec > 0 .AND. lnHdrRec <= RECCOUNT()
  GOTO lnHdrRec
ENDIF

*-- Get available and amendments amounts.
lnAvlAmt = (lnLcBal - lnTPOamt)
lcRefer  = SPACE(10)
DO CASE
  *-- Added PO.
  CASE lcPoType = 'A'
    lnAmmdAmt = IIF(lnPOamt > lnAvlAmt , (lnPOamt - lnAvlAmt) , 0)
    lcAmdTitl = "Add PO"
  *-- Reassigned PO.
  CASE lcPoType = 'S'
    lnAmmdAmt = -(lfPoOpnAmt())
    lcAmdTitl = "Assign PO"
  *-- Deleted PO.
  CASE lcPoType = 'D'
    lnAmmdAmt = -(lfPoOpnAmt())
    lcAmdTitl = "Delete PO"
ENDCASE

*-- Clear the trapped keys.
PUSH KEY
ON KEY

*-- Call LC Amendments Screen.
DO (gcScrDir+"POLCADD.SPX")

*-- Restore the trapped keys.
POP KEY

*-- Replace the forign amount with the balance amount.
laData[10] = laData[83]
*-- US Amount will the balance value divided by rate value.
*laData[11] = (laData[83]/laData[9])
laData[11]  = ROUND(laData[83] &lcExRSin laData[9] &lcUntSin laData[85] , 2)

SHOW GET laData[10]
SHOW GET laData[11]

**************************************************************************
*******************  Functions Related To POLCADD.SCX ********************
**************************************************************************

*!*************************************************************
*! Name      : lfvLCAmAm
*! Developer : Reham Al-Allamy
*! Date      : 02/25/1998
*! Purpose   : Valid function for the amendment amount.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvLCAmAm()
*!*************************************************************
*
FUNCTION lfvLCAmAm

*-- Enable the ok button if enter an amendment amount.
IF lnAmmdAmt = 0
  SHOW GET pbLcOk DISABLE
ELSE
  SHOW GET pbLcOk ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvLcOk
*! Developer : Reham Al-Allamy
*! Date      : 02/25/1998
*! Purpose   : Valid function for push button < Ok >.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvLcOk()
*!*************************************************************
*
FUNCTION lfvLcOk

*-- Change the status of the current PO.
SELECT (lcTmpPOSHd)
REPLACE cStatus   WITH 'M' ;
        cLcStatus WITH IIF(lcPoType = 'D' , ' ' , lcPoType)

*-- Add the amendment to the LC Balance To be saved in the Lc file.
laData[83] = laData[83] + lnAmmdAmt

*-- Add record in the temp LC journal file.
SELECT (lcTmpJornl)
APPEND BLANK
REPLACE cLCType    WITH laData[1] ;
        clcNo      WITH laData[2] ;
        dTranDate  WITH gdSysDate ;
        nTrnAmunt  WITH ABS(lnAmmdAmt) ;
        cReference WITH lcRefer ;
        cPoNo      WITH IIF(lcFromMod = "S" , &lcTmpPOSHd..Po , &lcTmpPOSHd..PoMat) ;
        cLCTrnTyp  WITH IIF(lcPoType = 'A' , '1' , '2') ;
        nLcBal     WITH laData[83] ;
        cStatus    WITH 'A'
*-- Call global function to add audit fields info.
=gfAdd_Info()

CLEAR READ


*!*************************************************************
*! Name      : lfCrtUnComp
*! Developer : Reham Al-Allamy
*! Date      : 02/05/1998
*! Purpose   : Create all the needed temp. files.
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfCrtUnComp()
*!*************************************************************
*
FUNCTION lfCrtUnComp

*-- Create temp. LC file.
SELECT LC
=AFIELDS(laTmp_LC)
lnTmp_LC = ALEN(laTmp_LC,1)
*-- Add field to know at which step the saving has done.
DIMENSION laTmp_LC [lnTmp_LC+1,4]
laTmp_LC[lnTmp_LC + 1 , 1] = 'NSTEPS'
laTmp_LC[lnTmp_LC + 1 , 2] = 'N'
laTmp_LC[lnTmp_LC + 1 , 3] = 7
laTmp_LC[lnTmp_LC + 1 , 4] = 0
*-- Call global function to create the temp. LC file.
=gfCrtTmp(lcTmp_LC , @laTmp_LC)

*-- Create temp. LC Journal file.
SELECT LcJournl
=AFIELDS(laTmpJornl)
lnTmpJornl = ALEN(laTmpJornl , 1)
*-- Add field to know at which step the saving has done.
DIMENSION laTmpJornl [lnTmpJornl+2,4]
laTmpJornl[lnTmpJornl + 1 , 1] = 'CSTATUS'
laTmpJornl[lnTmpJornl + 1 , 2] = 'C'
laTmpJornl[lnTmpJornl + 1 , 3] = 1
laTmpJornl[lnTmpJornl + 1 , 4] = 0
laTmpJornl[lnTmpJornl + 2 , 1] = 'NRECNO'
laTmpJornl[lnTmpJornl + 2 , 2] = 'N'
laTmpJornl[lnTmpJornl + 2 , 3] = 7
laTmpJornl[lnTmpJornl + 2 , 4] = 0
*-- Call global function to create the temp. LC Journal file.
=gfCrtTmp(lcTmpJornl , @laTmpJornl)

*-- Create temp. Style purchase order header file.
SELECT (lcPOHdrFil)
=AFIELDS(laTmpPOSHd)
lnTmpPOSHd = ALEN(laTmpPOSHd , 1)
*-- Add field to know at which step the saving has done.
DIMENSION laTmpPOSHd [lnTmpPOSHd + 2 , 4]
laTmpPOSHd[lnTmpPOSHd + 1 , 1] = 'CSTATUS'
laTmpPOSHd[lnTmpPOSHd + 1 , 2] = 'C'
laTmpPOSHd[lnTmpPOSHd + 1 , 3] = 1
laTmpPOSHd[lnTmpPOSHd + 1 , 4] = 0
laTmpPOSHd[lnTmpPOSHd + 2 , 1] = 'NRECNO'
laTmpPOSHd[lnTmpPOSHd + 2 , 2] = 'N'
laTmpPOSHd[lnTmpPOSHd + 2 , 3] = 7
laTmpPOSHd[lnTmpPOSHd + 2 , 4] = 0

*-- Call global function to create the temp. Style POs header or material 
*-- POs header file.
=gfCrtTmp(lcTmpPOSHd , @laTmpPOSHd , IIF(lcFromMod = "S" , [cLcNo+Po] , [cLcNo+PoMat]) , lcTmpPOSHd)

*-- Set relation between the PO# in the temp. header file & its lines in 
*-- the master POSLN Lines or POFLN Lines according to the main passed 
*-- parameters.
SELECT (lcTmpPOSHd)
IF lcFromMod = "S"
  SET RELATION TO cStyType + Po    INTO (lcPOLinFil) ADDITIVE
ELSE
  SET RELATION TO cMatType + PoMat INTO (lcPOLinFil) ADDITIVE
ENDIF

*!*************************************************************
*! Name      : lfChkUnComS
*! Developer : Reham Al-Allamy
*! Date      : 02/16/1998
*! Purpose   : Check if there is previous uncompleted session.
*!*************************************************************
*! Calls     : gfUnCompSession
*!*************************************************************
*! Parameters: llFrmSetup -> From screen setup or lpShow
*!*************************************************************
*! Returns   : llFondSess -> .T. if there is session found.
*!*************************************************************
*! Example   :  =lfChkUnComS()
*!*************************************************************
*
FUNCTION lfChkUnComS
PARAMETERS llFrmSetup
PRIVATE    llFondSess

*-- Flag to know if found uncompleted session or not.
llFondSess = .F.

*-- Flag to know if check the uncompleted session.
llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)

*-- If coming from the screen setup & should check the uncompleted session 
*-- Or coming from the lpshow procedure.
IF (llFrmSetup .AND. llChkUnCom) .OR. !llFrmSetup
  llChkUnCom = .F.
  *-- Call global function to know if there is uncompleted session was saved in
  *-- the uncompleted session file.
  IF gfUnCompSession(lcProgID, lnThisSess, "Letter of Credit")
    *-- Flag to execute the activate folders.
    llFromSPR  = .T.
    *-- Restore the previous mode.
    STORE .F. TO laScrMode
    laScrMode[ATC(lcScrMode,"SVEA")] = .T.
    
    STORE .T. TO llContinue, llCUpDate
    lcSession = UnCmSess.cSession
    *-- Get the object should be pressed.
    lcCurObj  = UPPER(ALLTRIM(UnCmSess.cCurrObj))
    
    *-- Get the header fields values in the screen array.
    SELECT (lcTmp_LC)
    SCATTER FIELDS &lcScFields  TO laData
    
    *-- If there is no object was saved and not coming from setup.
    IF EMPTY(lcCurObj) .OR. !llFrmSetup
      *-- Set the found session flag to true.
      llFondSess = .T.
      *-- Go to refresh the screen.
      SHOW GETS
    ELSE
      DO CASE
        *-- If the saved object was the <SAVE> button.
        CASE lcCurObj  = "PBSAV"
          *-- Call local procedure save.
          =lpSavScr()
          *-- Set the found session flag to false.
          llFondSess   = .F.
          *-- Set the current mode to be select mode.
          laScrMode    = .F.
          laScrMode[IIF(lcScrMode="E" ,2,1)] = .T.
          *-- Call the show procedure.
          DO lpShow
        *-- If the saved object was the <DELETE> button.
        CASE lcCurObj  = "PBDLT"
          *-- Call local procedure void.
          =lpDelScr()
          *-- Set the found session flag to false.
          llFondSess   = .F.
          *-- Set the current mode to be select mode.
          laScrMode    = .F.
          laScrMode[1] = .T.
          *-- Call the show procedure.
          DO lpShow
        OTHERWISE
          *-- Set the found session flag to true.
          llFondSess = .T.
          *-- Refresh the screen.
          SHOW GETS
      ENDCASE
    ENDIF
  ELSE
    *-- Call local function to create the needed temp. files.
    =lfCrtUnComp()
  ENDIF
ENDIF

*-- Return with flag if found uncompleted session or not.
RETURN (llFondSess)


*!*************************************************************
*! Name      : lfAdUnCmSR
*! Developer : Reham Al-Allamy
*! Date      : 07/13/97
*! Purpose   : Add record in the uncompleted session file.
*!*************************************************************
*! Calls     : lfUpdVars
*!*************************************************************
*! Passed Parameters  :  lcAddObj-> Current object pressed.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAdUnCmSR()
*!*************************************************************
*
FUNCTION lfAdUnCmSR
PARAMETERS lcAddObj
PRIVATE lcCurAlias , lcAddObj

*-- If the current object was not sent as a parameter, define it.
IF TYPE("lcAddObj") $ "UL"
  lcAddObj = SPACE(10)
ENDIF

*-- Save current alias.
lcCurAlias = ALIAS()

*-- Add record in the uncompleted session file.
SELECT 'UNCMSESS'
IF !SEEK('I')
  APPEND BLANK
ENDIF
lnUnCmSeRc = RECNO()
BLANK
REPLACE Status     WITH 'O'       ;
        cUTranType WITH lcProgID  ;
        cUserId    WITH gcUser_id ;
        cSession   WITH lcSession ;
        cProgram   WITH "POLETCR" ;
        cCurrScr   WITH "POLETCR" ;
        dTranDate  WITH gdSysDate ;
        cCurrObj   WITH lcAddObj  ;
        cTranTime  WITH TIME()

*-- Lock the current record in the uncompleted session file.
=RLOCK()

*-- Call local function to update the temp. files & needed variables 
*-- fields in the uncompleted session file record.
=lfUpdVars()

*-- Restore the previous alias.
SELECT (lcCurAlias)

*!*************************************************************
*! Name      : lfUpdVars
*! Developer : Reham Al-Allamy
*! Date      : 07/13/98
*! Purpose   : Update the comment field with the available variables.
*!*************************************************************
*! Calls     : gfSavSess
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdVars()
*!*************************************************************
*
FUNCTION lfUpdVars

*-- Concatinate the files variable.
lcFiles = 'lcTmpPOSHd,'+lcTmpPOSHd+','+lcTmpPOSHd+';'+ ;
          'lcTmpJornl,'+lcTmpJornl+',;'+ ;
          'lcTmp_LC,'+lcTmp_LC+',;'

*-- Call global function to update the uncompleted session record in the
*-- uncompleted session file with the files names & screen variables.
llNoThing = IIF(lnUnCmSeRc=0, .T., gfSavSess(lcProgID, lcFiles, @laVars, lcSession))


*!*************************************************************
*! Name      : lfvStates 
*! Developer : Reham Al-Allamy
*! Date      : 03/18/98
*! Purpose   : Validate the state code
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  =lfvStates()
*!*************************************************************
*
FUNCTION lfvState
PRIVATE lcFile_Ttl, lcBrfields

*B602685,1 Reham On 03/18/99   *** Begin ***
*B602685,1 Validate the state field from codes file if the address in USA or Canada...
IF ALLTRIM(laData[14])='USA' OR ALLTRIM(laData[14])='CANADA'
  lnCurAlias = SELECT(0)
  SELECT CODES
  SET ORDER TO TAG Codes
  *B602685,1  Seek for the entered state value.
  IF !EMPTY(laData[18]) .AND. !SEEK("N"+PADL(laData[18],6)+'N'+'STATE','CODES')
    SELECT CODES
    DECLARE laCodeFld[2]
    lcFile_Ttl = 'State Codes'
    lcBrfields = 'cCode_No,cDiscrep'
    *B602685,1 Browse all the available states.
    IF gfBrows('FOR cDefCode+cfld_name+ccode_no+cdiscrep = ;
      "N"+"STATE" .AND. cRltField="N"','cCode_No,cDiscrep','laCodeFld')
      laData[18] = laCodeFld[1]
    ELSE
      laData[18] = lcOldValue
    ENDIF
  ENDIF
  *B602685,1 Restore the codes tag.
  SET ORDER TO TAG CCODE_NO
  SELECT (lnCurAlias)
  
  SHOW GET laData[18]
ENDIF
*B602685,1 Reham On 03/18/99   *** End   ***
