*:************************************************************************
*: Program file  : RMRTATH.PRG
*: Program desc. : RETURN AUTHORIZATION FILE MAINTENANCE.
*: For screen    : RMRTATH.SCX (0,1,2,2A,2B)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Returns Merchandise Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *E300507,1 Rewrite the program to work in the 2.7 version
*:************************************************************************
*: Calls         : 
*:         Procedures : None
*:         Programs   : GetObj      (Object Linking Program)
*:                      NotePad     (Note Pad Program)
*:         Screens    : RMRtAth.SPX (Main Return Authorization screen)
*:         Functions  : lpShow
*:                      gfSetup
*:                      lfvData_1
*:                      lfActFolder
*:                      lfWhenAll
*:                      lfvData_2
*:                      lfvData_3
*:                      lfwData_6
*:                      lfvData_6
*:                      lfvData_7
*:                      lfvInqInv
*:                      lfvData_10
*:                      lfvData_11
*:                      lfvData_14
*:                      lfwExRate
*:                      lfvData_16
*:                      lfvData_17
*:                      lfvReson1
*:                      lfvDiv
*:                      lfActBrow
*:                      lfDeactBrw
*:                      lfBrLine
*:                      lfvBrows
*:                      lfWhenBrow
*:                      lfvNewLine
*:                      lfvRemLine
*:                      lfvStyle
*:                      lfStyCan
*:                      lfvReson2
*:                      lfvTotQty
*:                      lfvPrice
*:                      lfvTaxRate
*:                      lfvQty
*:                      lfvDyelot
*:                      lpDelScr
*:                      lpSavScr
*:                      lfvRaNotes
*:                      lfvLinkTo
*:*************************************************************
*: Passed Parameters  : lcRetAth : R/A #
*:*************************************************************
*: Example            :
*:  DO gpDoProg WITH 'AWRRMRTATH',.F.,'RM',lcRaNo
*:*************************************************************
*:  Modifications :
*E301090,1 Reham On 12/13/98
*E301090,1 Do not accept any voided invoices...
*E301077,4 Reham On 12/17/98
*E301077,4 Reduce the opened file on the program level:
*E301077,4 1- Change the files in SydObjct:
*E301077,4    From : RETAUTH,RALINE,CUSTOMER,CODES,NOTEPAD,CONSINVH,CONSINVL,INVHDR,INVLINE,ORDHDR,SCALE,STYDYE,STYLE,WAREHOUS,SYCINT|RETAUTH,RALINE,CUSTOMER,CCODE_NO,NOTEPAD,CONSINVH,CONSINVL,INVHDR,INVLINE,ORDACCT,SCALE,STYDYE,STYLE,WAREHOUS,CCONTCODE
*E301077,4    To   : RETAUTH,RALINE,CUSTOMER,CODES,SCALE,STYLE,WAREHOUS|RETAUTH,RALINE,CUSTOMER,CCODE_NO,SCALE,STYLE,WAREHOUS
*E301077,4 2- Open the removed files from SydObjct in the current program.
*B602637,1 Reham On 03/04/98
*B602637,1 Give the ability to add notes upon saving new R/A.
*B602648,1 Reham On 03/04/99
*B602648,1 Fix the error of Variable "lnCurrUnit" not found.
*E301176,1 HDM 03/22/1999 
*E301176,1 Prevent programs from displaying notepad icon
*E301176,1 as it's now controlled globally
*B802088,1 Reham On 04/07/99
*B802088,1 Pick the store address in the from address if the store field 
*B802088,1 is not empty.
*B602753,1 HDM 04/07/1999 Stop Calling NotePad Program In lpSavScr as the global save
*B602753,1 Will Call it
*B602771,1 Reham On 04/11/99
*B602771,1 1- Change the way of using style price level if there is price level Q 
*B602771,1    or the current price level is zero.
*B602771,1 2- Enable the soft seek in the style browse.
*B602771,1 3- Put all the uses of the currency under condition of llMulCurr.
*B802213,1 Reham On 05/02/99
*B802213,1 If the customer tax rate is zero, do not use the tax rate of the company setup.
*B602783,1 Reham On 05/03/99
*B602783,1 Force to push button new if last size.
*B802222,1 Reham On 05/05/99
*B802222,1 Point to the first line in the browse if collecting data.
*B602863,1 Reham on 05/12/1999
*B602863,1 1- Do not check style price if from invoice.
*B602863,1 2- Do not repeate the dicount message in each qty.
*B602863,1 3- Disable the division if entered style.
*B602863,1 4- Change the scan condition in the style validation.
*B602750,1 Reham On 05/25/1999
*B602750,1 1- Disable the detail folder in the select mode
*B602750,1 2- Point to the header folder after save or cancel.
*B602932,1 Reham On 05/26/1999
*B602932,1 Do not change style price if change the qty. if this style was 
*B602932,1 shipped to this customer before.
*B602959,1 Reham On 06/09/1999
*B602959,1 Enable the warehouse popup even there is invoice.
*B603019,1 Reham On 06/28/1999
*B603019,1 1- Adjust the open qty. if delete lines.
*B603019,1 2- Fix error in invoice validation "Subscript out of bounds".
*B603020,1 Reham On 06/28/1999
*B603020,1 Fix error in qty. sizes validation "Variable lshipped not found".
*B802379,1 Reham On 07/19/1999
*B802379,1 Adjust the qty. objects in the screen to void accepting negative values.
*B603084,1 RAMY On 08/29/1999
*B603084,1 When the customer is not active should not Proceed dealing with
*B603084,1 him
*B603133,1 RAMY On 08/29/1999
*B603133,1 When the customer account is empty should not make error on store
*B603133,1 validation
*B603185,1 HDM 10/06/1999 include discount percentage when
*                         calculating price & the total amount
*B603238,1 HDM 10/25/1999 - Allow Saving even there were no lines
*                         - Enhance the invoice browsing speed
*                         - Add Note1, Note2 to invoice browse
*B603277,1 Ramy 12/08/1999 Calculate the cusromer discount if the style not
*                          shipped to that customer before
*B603397,1 HDM  26/01/2000 Invoice field should be enabled when
*                           add/edit modes considering
*                           - If the user checked the return entire invoice
*                             then changed the invoice # we will not change
*                             any of the collected lines and the invoice #
*                             will not affect any collected data
*B803124,1 Reham On 04/27/2000
*B803124,1 Do not adjust the authorized qty. & amount if there is no lines
*B803124,1 in the current RA, save them as they entered due to a 2.6 feature.
*B603416,1 Reham On 04/27/2000
*B603416,1 Apply the header reason to all lines' reason if not entered the detail folder.
*B802948,1 Reham On 04/30/2000
*B802948,1 Give the ability to browse RAs by customer.
*B603986,1 KAM 11/2/2000 to allow dealing with non active customer
*B603986,1 KAM 11/6/2000 to disallow dealing canceled or potential account
*B603713,7 KAM 10/11/2000 change width of fields price and amount,also 
*B603713,7 KAM             we change the oject in screen rmrtath.scx
*B604218,1 KAM 10/03/2001 display message if use select location not assign to style
*E301643,1 MHM 07/25/2001 Add new type (Electronic) to get R/A from CRM module
*B605151,1 ADEL 11/28/01 Modify the message as it's being called now from Credit Memo also..
*B605776,1 ASH 05/13/2002 Enable the cartons field to let the user change it's numer to be used while printing the stickers.
*B605970,1 ASH 05/22/2002 Fix the bug 'alias not found' when editing an existing RA, and try to change the warehouse.
*B605881,1 ABD 05/23/2002 Get the main account addresses or main store addresses not 
*B605881,1 ABD            the alternatives addresses for the account or the store.
*B605816,1 ADEL 07/16/02 Don't allowed editing Completed or Cancelled R/A using CTRL+E
*B606357,1 ADEL 08/25/02 Fix the bug of wrongly updting qty when modifying the RA and 
*B606357,1               changing the style. 
*B606480,1 ADEL 09/19/02 When the system use dyelots but the style doesn't, care its update.
*B038431,1 NNA 10/03/2004 Fix bug that you can save without selecting an account
*C123847,1 TMI 12/28/2004 Add an option "Select Employee" to the option menu pad
*E131400,1 NNA 04/12/2006 get the default warehous as saved in the file that if there is no default warehouse assigned 
*E131400,1 NNA            to the invoice screen
*B608481,1 NNA 03/15/2008 (T20071213.0008) Fix bug that when you make a Return Authorisation assigned to invoice for a 
*B608481,1 NNA            customer that has more than one invoice the price get incorrect because it get the price for the last invoice
*:******************************************************************************************
PARAMETERS lcRetAth
EXTERNAL ARRAY laData , laKeyField , laDefProc

DECLARE laKeyField [1,4]
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'RetAuth'
laKeyField[1,4] = 1

*E301176,1 HDM 03/22/1999[Start] Prevent programs from displaying notepad icon
*E301176,1 as it's now controlled globally

*-- Add more buttons to the control pannel.
*DECLARE laPanelObj[2,3]
DECLARE laPanelObj[1,3]

*laPanelObj[1,1] = "pbNotes"
*laPanelObj[1,2] = gcBmpHome + "NOTES2.BMP"
*laPanelObj[1,3] = "VALID lfvRaNotes() MESSAGE 'Return Authorization Notes'"

laPanelObj[1,1] = "pbLinkTo"
laPanelObj[1,2] = gcBmpHome + "Relate.BMP"
laPanelObj[1,3] = "VALID lfvLinkTo() MESSAGE 'Object Link'"

*E301176,1 HDM 03/22/1999[End] Prevent programs from displaying notepad icon
*E301176,1 as it's now controlled globally

*-- Var. hold the temp. window names.
lcRtAth0   = ""
lcRtAth1   = ""
lcRtAth2   = ""
lcRtAth2A  = ""
lcRtAth2B  = ""

*-- Initialize the Folders array.
DIMENSION laFoldWinds[2,2]
lcFolder     = SPACE(0)
lcwFoldChng  = SPACE(0)
lcFoldPush   = SPACE(0)
laFoldWind   = SPACE(0)
lcFoldPrnt   = SPACE(0)
lnActFolder  = 1
lnFolderCEnd = 0
lnFolderREnd = 0

*--E301643,1 mhm  add variable for checking if CR module is installed [Start]
llCrmIns = IIF('CR' $ gcCmpModules,.T.,.F.)
IF llCrmIns 
  =gfOpenFile(gcDataDir+'CRMesag',gcDataDir+'TransType','SH')
ENDIF  
*--E301643,1 mhm  [End]

*-- Array hold the status text
*--E301643,1 mhm increase array to get new Status "Electronic"[Start]
*DECLARE laStatus[3]
DECLARE laStatus[4]
*--E301643,1 mhm [End]
laStatus[1] = "Open"
laStatus[2] = "Completed"
laStatus[3] = "Canceled"
*--E301643,1 mhm [Start]
laStatus[4] = "Electronic"
*--E301643,1 mhm [End]

*-- Arrays for the codes info.
DECLARE laCodInfo[2,10] , laReson1[1] , laDiv[1] , ;
        laCodInfo1[1,10] , laReson2[1]

lcDelMesag    = 'Cancel'  && Change the message bar of the delete button.
laDefProc[7]  = .F.       && Force to local void procedure  (lpDelScr)
laDefProc[9]  = .F.       && Force to local Save procedure  (lpSavScr)

*-- Get the prompt of the available pictures on the screen.
lcBrowBmp = gcBmpHome + "ExtKey.BMP"
lcNwRABMP = gcBmpHome + "New1.BMP"
lcRmRABMP = gcBmpHome + "Rem1.BMP"
lcCancel  = gcBmpHome + "trash.bmp"
lcUncan   = gcBmpHome + "untrash.bmp"
lcPromp   = lcCancel

lcScFields = ""                 && Var. hold the screen fields.

*-- Var. hold the browse titles.
lcBrowttl  = "R/A Lines"

*-- Var. hold the temp. R/A lines file.
lcTmpRetLn = ""

lcRCurAlis = IIF(laScrMode[2] , 'RALINE'  , lcTmpRetLn)
*lcKeyRet   = IIF(laScrMode[4] , "KEY laData[2]" , "KEY laData[1]")
lcKeyRet   = IIF(laScrMode[2] , "KEY laData[1]" , "")

lnPstRate  = 0         && To hold the PST rate.
lnCusPstRt = 0         && To hold the default PST rate.

STORE ' ' TO lcOldValue , lcName , lcFName , lcStatus , ;
             lcFaddres1 , lcFaddres2 , lcFaddres3 ,;
             lcFaddres4 , lcFaddres5 , lcTaddres1 ,;
             lcTaddres2 , lcTaddres3 , lcTaddres4 ,;
             lcTaddres5 , lcStyle , lcDesc , lcTaxName ,;
             lcReason , lcCustPLvl , lcDyeLot ,;
             lcConsol , lcDefWare , lcRa_LinNo

STORE "DISABLE" TO lcWarStat , lcInqStat , lcNewStat , lcLinStat ,;
                   lcDivStat , lcCurStat , lcExRStat , lcEntrStat ,;
                   lcRelStat , lcDyeStat , lcObjStat

*B802948,1 Reham On 04/30/2000   *** Begin ***
*B802948,1 Define variable to control the enabling & disabling of the account field.
lcAccStat = "ENABLE"
*B802948,1 Reham On 04/30/2000   *** End   ***

STORE .F. TO llTaxYes , llMultiWH , llDyelot ,;
             llGlLink , llBased , llIsCanada , llMulCurr ,;
             llEditExRt

*B605816,1 (Begin) As Edit button is Disable, also disable Edit option in Record menu to 
*B605816,1         prevent editing Complete or Cancelled RA useing CRTL+E
DEFINE BAR 9  OF P03PU03 PROMPT '\<Edit' SKIP FOR !(laScrMode[2] .AND. INLIST(laData[4],'O','E'))
*B605816,1 (End)

*-- Array hold all the available warehouses.
DECLARE laWareHous[1,2]
STORE 1 TO puWareHous
DECLARE laSize[8] , laQtyStk[8]
laSize     = ""
laQtyStk   = 0
lcStyHdr   = ""
lnStyLngth = 0

STORE 0   TO lnTotQty , lnprice , lnTotal , lnMarker ,lnLine , ;
             lnNoOfLines , lnScale , lnDefTaxRt , lnTaxRate , ;
             lnPstRate , lnOldValue , lnDiscPCnt , lnRa_LinNo , lnTaxzamt

STORE .F. TO llBrowse , cbComplete , llUpdate , llOldValue

*B603416,1 Reham On 04/27/2000   *** Begin ***
*B603416,1 Define a flag to know if entered the detail folder or not yet.
llEnterDet = .F.
*B603416,1 Reham On 04/27/2000   *** End   ***

STORE {}  TO ldOldValue

*B602648,1 Reham On 03/04/99  *** Begin ***
lcCurrCode = gcBaseCurr &&  Variable to hold currency code.
lnExRate   = 1          &&  Variable to hold exchange rate.
lnCurrUnit = 1          &&  Variable to hold currency unit.
lcUntSin   = '/'        &&  Variable to hold unit sign.
lcExRSin   = '/'        &&  Variable to hold exchange rate sign.
*B602648,1 Reham On 03/04/99  *** End   ***

llReBrowse = .T.    && Flag to know if execute the browse or not.
llAddMode  = .F.    && Flag to know if entered the add mode or not.
llCUpDate  = .F.    && Flag to determine if there is something changed in the screen.

STORE 1 TO lnReson2

*-- Call global function in the main program to do the following : _
*-- Intialise all the variables & open all the files needed in
*-- this session and controling disabling and enabling of the
*-- menu bars and writting the screen names in the window bars.
IF !gfSetup()
  RETURN
ENDIF

*-- Array for the header data, & variable hold the header fields.
DECLARE laData[31]
lcScFields = "rano , account , store , status , radate , void , "+;
             "cwarecode , auth , authamt , invoice , order , "+;
             "custpo , cartons , ccurrcode , ncurrunit , nexrate , "+;
             "cintr_inv , reason , cDivision , retdate , return , "+;
             "returnamt , tran , flag , nreta_bud , nreta_rec , "+;
             "nreta_can , nreta_opn , cretnote1 , cretnote2 , nRtOpnAmt"

llAlowNew = .T.     && Flag to allow adding new credit memos.
llNoShow  = .F.     && Flag to force the execution of the show procedure.

*E301077,4 Reham On 12/17/98   *** Begin ***
*E301077,4 Open the company file.
=gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_Id','SH')
*E301077,4 Reham On 12/17/98   *** End   ***

*-- If entering the screen for the first time.
IF !WEXIST(gcBaseWind)
  
  lcTaxName  = gfGetMemVar('M_TAX_DESC' , gcAct_Comp)
  lcTaxName  = IIF(EMPTY(lcTaxName) , 'Gst Tax' , lcTaxName)

  *-- Var. hold the style header.
  lcStyHdr   = gfItemMask("HI")
  
  *-- Var. hold the length of the style.
  lnStyLngth = LEN(lcStyHdr)
  
  *-- If calculate tax.
  llTaxYes   = ALLTRIM(gfGetMemVar('M_TAX',gcAct_Comp))       = 'Y'
  
  *-- Get the default tax rate from the setup.
  lnDefTaxRt = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
  
  *-- Multi warehouse.
  llMultiWH  = ALLTRIM(gfGetMemVar('M_WareHouse',gcAct_Comp)) = 'Y'
  
  *-- Use Dyelot or not.
  llDyelot   = ALLTRIM(gfGetMemVar('M_DYELOT',gcAct_Comp))    = 'Y'
  
  *-- See if there is GL link or not.
  llGlLink   = ALLTRIM(gfGetMemVar('M_LINK_GL',gcAct_Comp))   = 'Y'
  
  *-- Sequence on Division or not.
  llBased    = ALLTRIM(gfGetMemVar('M_Div_Seq',gcAct_Comp))   = 'Y'
  
  *-- If country is Canada
  llIsCanada = IIF(UPPER(ALLTRIM(gcContCode)) = 'CANADA', .T., .F.)
  
  *-- Multi Currency.
  llMulCurr  = gfGetMemVar('llMulCurr',gcAct_Comp)
  
  *-- Edit Exchange rate.
  llEditExRt = gfGetMemVar('LLEDITEXRA',gcAct_Comp)
  
  *-- If multi currency.
  IF llMulCurr
    lcCurrCode = SPACE(3)   &&  Variable to hold currency code.
    lnExRate   = 0          &&  Variable to hold exchange rate.
    lnCurrUnit = 0          &&  Variable to hold currency unit.
    lcUntSin   = '/'        &&  Variable to hold unit sign.
    lcExRSin   = '/'        &&  Variable to hold exchange rate sign.
    IF !USED("SycCurr")
      *-- Open the currency file if was not open.
      =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
    ENDIF
    IF !USED("SycExch")
      *-- Open the exchange file if was not open.
      =gfOpenFile(gcSysHome+'SycExch','','SH')
    ENDIF
  ELSE
    lcCurrCode = gcBaseCurr &&  Variable to hold currency code.
    lnExRate   = 1          &&  Variable to hold exchange rate.
    lnCurrUnit = 1          &&  Variable to hold currency unit.
    lcUntSin   = '/'        &&  Variable to hold unit sign.
    lcExRSin   = '/'        &&  Variable to hold exchange rate sign.
  ENDIF
  *-- Fill the warehouse array.
  IF !llMultiWH
    SELECT WAREHOUS
    GO TOP

    *E131400,1 NNA 04/12/2006 (Begin) Open Warehous file to get the default warehous as saved at ldefware field then
    *E131400,1 NNA             close file again
    LOCATE FOR ldefware
    IF !FOUND()
      GO TOP
    ENDIF
    *E131400,1 NNA (End)

    lcDefWare       = WAREHOUS.cWareCode
    laWareHous[1,1] = WAREHOUS.cDesc
    laWareHous[1,2] = WAREHOUS.cWareCode
  ELSE
    lcDefWare       = ""
    SELECT cDesc , cWareCode ;
      FROM WAREHOUS ;
      INTO ARRAY laWareHous
  ENDIF
  
  *-- Get Temp. names for the screen set.
  lcRtAth0  = gfTempName() &&SYS(2015)
  lcRtAth1  = gfTempName() &&SYS(2015)
  lcRtAth2  = gfTempName() &&SYS(2015)
  lcRtAth2A = gfTempName() &&SYS(2015)
  lcRtAth2B = gfTempName() &&SYS(2015)

  lcwFoldChng      = "= lfActFolder()"   && function to control shows after change the folder
  lcFoldPush       = "pbFolders"         && push button name for the next folder
  lnFolderCEnd     = 102.00
  lnFolderREnd     = 2.00
  lcFolder         = gfTempName()
  lcFoldPrnt       = gcBaseWind          && window parent name for the folder
  lnActFolder      = 1                   && Var. hold the current folder.

  *-- Array hold the folders info.
  laFoldWinds[1,1] = "Header"
  laFoldWinds[1,2] = lcRtAth1
  laFoldWinds[2,1] = "Detail"
  laFoldWinds[2,2] = lcRtAth2
  
  *-- Array hold the header's codes info.
  laCodInfo[1,01] = "REASON"       && Field Name
  laCodInfo[1,02] = "laReson1"     && Array Name
  laCodInfo[1,03] = "lnReson1"     && Popup Name
  laCodInfo[1,04] = ""             && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[1,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[1,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[1,07] = "RETAUTH"      && Alternative File (For default val.)
  laCodInfo[1,08] = "RETAUTH"      && Use this index for the Alternative file.
  laCodInfo[1,09] = "laData[1]"    && Seek this expretion.
  laCodInfo[1,10] = "REASON"       && Alternative Field Name

  laCodInfo[2,01] = "CDIVISION"      && Field Name
  laCodInfo[2,02] = "laDiv"          && Array Name
  laCodInfo[2,03] = "lnDiv"          && Popup Name
  laCodInfo[2,04] = ""               && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[2,05] = .F.              && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[2,06] = .F.              && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[2,07] = "RETAUTH"        && Alternative File (For default val.)
  laCodInfo[2,08] = "RETAUTH"         && Use this index for the Alternative file.
  laCodInfo[2,09] = "laData[1]"      && Seek this expretion.
  laCodInfo[2,10] = "cDivision"       && Alternative Field Name

  *-- Array hold the lines' codes info.
  laCodInfo1[1,01] = "REASON"       && Field Name
  laCodInfo1[1,02] = "laReson2"     && Array Name
  laCodInfo1[1,03] = "lnReson2"     && Popup Name
  laCodInfo1[1,04] = ""             && Popup Status  ("D"->Default,"A"->All)
  laCodInfo1[1,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo1[1,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo1[1,07] = "RALINE"       && Alternative File (For default val.)
  laCodInfo1[1,08] = "RALINE"       && Use this index for the Alternative file.
  laCodInfo1[1,09] = "laData[1] + PADR(lcStyle,19) + lcRa_LinNo"    && Seek this expretion.
  laCodInfo1[1,10] = "REASON"       && Alternative Field Name
  
  *-- Fill the codes popups with N/A. (Header reason , division , detail reason)
  = gfwCodePop(@laCodInfo , "REASON"   , "N")
  = gfwCodePop(@laCodInfo , "CDIVISION", "N")
  = gfwCodePop(@laCodInfo1, "REASON"   , "N")

  *-- Prepare the R/A line temp. file.
  lcTmpRetLn = gfTempName()
  lcRCurAlis = IIF(laScrMode[2] , 'RALINE'  , lcTmpRetLn)
  
  SELECT RALINE
  DIMENSION laFileStru[1]
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  *B602932,1 Reham On 05/26/1999   *** Begin ***
  *B602932,1 Increase the array dimension to add new logical field to the temp. file.
  *DIMENSION laFileStru[lnFileStru+2,4]
  DIMENSION laFileStru[lnFileStru+3,4]
  *B602932,1 Reham On 05/26/1999   *** End   ***
  laFileStru[lnFileStru+1,1] = 'CSTATUS'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'NRECNO'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 5
  laFileStru[lnFileStru+2,4] = 0
  *B602932,1 Reham On 05/26/1999   *** Begin ***
  *B602932,1 Add logical field to know if this style was shipped to this customer before or not.
  laFileStru[lnFileStru+3,1] = 'LSHIPPED'
  laFileStru[lnFileStru+3,2] = 'L'
  laFileStru[lnFileStru+3,3] = 1
  laFileStru[lnFileStru+3,4] = 0
  *B602932,1 Reham On 05/26/1999   *** End   ***
  CREATE TABLE (gcWorkDir + lcTmpRetLn) FROM ARRAY laFileStru
  SELECT RetAuth
  *-- If the program was called from another program, force to view mode.
  IF TYPE('lcRetAth') = 'C' .AND. SEEK(lcRetAth,'RetAuth')
    SCATTER FIELDS &lcScFields TO laData
    laScrMode    = .F.
    laScrMode[2] = .T.
  ELSE
    *-- Fill the header array with the fields features.
    SCATTER FIELDS &lcScFields TO laData BLANK
    *-- Go to the select mode at the begining.
    laScrMode    = .F.
    laScrMode[1] = .T.
  ENDIF
  *-- RE-Calculate laData[31]
  *laData[31] = (nTotOpnQty * (InvLine.price-(InvLine.price*(lnDiscPcnt/100))))
  
ENDIF

*B802948,1 Reham On 04/30/2000   *** Begin ***
*B802948,1 Define variable to control the enabling & disabling of the account field.
lcAccStat = IIF(laScrMode[2] , "DISABLE" , "ENABLE")
*B802948,1 Reham On 04/30/2000   *** End   ***

*-- Default the status varibales of the screen objects to its right values.
lcObjStat = IIF(laScrMode[3] .OR. laScrMode[4] , "ENABLE" , "DISABLE")
*B602959,1 Reham On 06/09/1999    *** Begin ***
*B602959,1 Enable the warehouse popup even there is invoice.
*lcWarStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. llMultiWH .AND. EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcWarStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. llMultiWH , "ENABLE" , "DISABLE")
*B602959,1 Reham On 06/09/1999    *** End   ***
lcInqStat = IIF(!EMPTY(laData[10]) .AND. !laScrMode[1] , "ENABLE" , "DISABLE")

*B603397,1 [START] Don't include lnNoOfLines when determining the invoice object status
*                  as it should be always enabled in Add/Edit modes
*lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0 , "ENABLE" , "DISABLE")
lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]) , "ENABLE" , "DISABLE")
*B603397,1 [End]

*B602863,1 Reham On 05/12/1999   *** Begin ***
*B602863,1 Disable the division if entered any styles.
*lcDivStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcDivStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) .AND. lnNoOfLines = 0 , "ENABLE" , "DISABLE")
*B602863,1 Reham On 05/12/1999   *** End   ***

lcCurStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcExRStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) .AND. !EMPTY(laData[14]) .AND. llEditExRt .AND. (laData[14] <> gcBaseCurr) , "ENABLE" , "DISABLE")

*B603397,1 [START] Don't include lnNoOfLines when determining the invoice object status
*                  as it should be always enabled in Add/Edit modes

*lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0  .AND.;
                                                !EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcTmpRetLn) .AND.;
                                                !EMPTY(laData[10]) , "ENABLE" , "DISABLE")
*B603397,1 [End]
lcRelStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) , "ENABLE" ,"DISABLE")

*--E301643,1 mhm Activiate Option in menu bar [Start]
*C123847,1  TMI [Start] let lfActpad be a public funtion for the program , ;
*                       and move the if condition of the cm module to the function itself
*IF llCrmIns
*C123847,1  TMI [End  ] 
  PUSH KEY
  =lfActPad()
*C123847,1  TMI [Start] comment the endif of the above if condition
*ENDIF  
*C123847,1  TMI [End  ] 
*--E301643,1 mhm [End]

*-- Define the browse bar.
PUSH MENU _MSYSMENU
DEFINE BAR 100 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 101 OF P01PU01 PROMPT lcBrowttl KEY ALT+B

*-- Activate the browse when selecting its bar.
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcBrowttl)

SELECT RETAUTH
*-- set realtion between R/A header & customer file.
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER

*-- Call the screen.
DO (gcScrDir+gcWinAppl+"\RMRtAth.SPX")

*--E301643,1 mhm Retrive Key[Start]
*C123847,1  TMI [Start] remove this check
*IF llCrmIns
*C123847,1  TMI [End  ] 
  POP KEY
*C123847,1  TMI [Start] comment the endif of the above if condition
*ENDIF  
*C123847,1  TMI [End  ] 
*--E301643,1 mhm [End]

*-- Release the browse window.
RELEASE WINDOW (lcBrowttl)
POP MENU _MSYSMENU

*--E301643,1 mhm release option [Start]
*C123847,1  TMI [Start] always release this option pad
*IF llCrmIns
*C123847,1  TMI [End  ] 
  RELEASE PAD _Option OF _MSYSMENU
*C123847,1  TMI [Start] comment the endif of the above if condition
*ENDIF  
*C123847,1  TMI [End  ] 
*--E301643,1 mhm [End]

*-- Clear the relation.
SELECT RETAUTH
SET RELATION TO
*B605816,1 (Begin) Realease the bar
DEFINE BAR 9  OF P03PU03 PROMPT '\<Edit' SKIP FOR .T.
*B605816,1 (End)

*-- If quitting the program erase the temp. files.
IF glQuitting
  IF USED(lcTmpRetLn)
    USE IN &lcTmpRetLn
  ENDIF
  ERASE &gcWorkDir.&lcTmpRetLn..DBF
  ERASE &gcWorkDir.&lcTmpRetLn..CDX
  ERASE &gcWorkDir.&lcTmpRetLn..FPT
ENDIF

*!*************************************************************
*! Name      : lpShow
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : To refrash all the screen object with proper mode
*!*************************************************************
*! Calls     : gfwCodePop, lfWhenBrow, lfActFolder
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpShow
*!*************************************************************
*
FUNCTION lpShow

*-- Change the prompt of the delete bar in the edit pad to be canceled 
*-- instead of delete.
lcCanBar = "IIF(laData[4]='X' , '\<Uncancel' , '\<Cancel')"
DEFINE BAR 10 OF P03PU03 PROMPT &lcCanBar SKIP FOR !laScrMode[2]

*B603416,1 Reham On 04/27/2000   *** Begin ***
*B603416,1 Define a flag to know if entered the detail folder or not yet.
llEnterDet = .F.
*B603416,1 Reham On 04/27/2000   *** End   ***

SELECT RETAUTH
DO CASE
  CASE laScrMode[1]
    *-- Blank the R/A lines temp. file.
    SELECT (lcTmpRetLn)
    ZAP
    *-- Initialize the needed variables.
    llAddMode = .F.
    llCUpDate = .F.
    STORE ' ' TO lcOldValue , lcName , lcFName , lcStatus , ;
                 lcFaddres1 , lcFaddres2 , lcFaddres3 ,;
                 lcFaddres4 , lcFaddres5 , lcTaddres1 ,;
                 lcTaddres2 , lcTaddres3 , lcTaddres4 ,;
                 lcTaddres5 , lcStyle , lcDesc ,;
                 lcReason , lcCustPLvl , lcDyeLot ,;
                 lcConsol , lcRa_LinNo
    
    STORE 0 TO lnTotQty , lnprice , lnTotal ,;
               lnNoOfLines , lnTaxRate , lnPstRate , lnDiscPCnt , ;
               lnRa_LinNo , lnTaxzamt
    
    puWareHous = 1

    *E131400,1 NNA 04/12/2006 (Begin) Open Warehous file to get the default warehous as saved at ldefware field then
    *E131400,1 NNA             close file again
    lnOldAlias = SELECT(0)
    SELECT WAREHOUS
    LOCATE FOR ldefware
    IF FOUND()
      puWareHous = ASCAN(laWareHouses,WAREHOUS.CWARECODE)
      puWareHous = IIF(puWareHous=0,1,ASUBSCRIPT(laWareHouses,puWareHous,1))
    ENDIF  
    SELECT(lnOldAlias)
    *E131400,1 NNA (End)

    laSize     = ""
    laQtyStk   = 0
    cbComplete = .F.
    lnReson2   = 1
    
    *-- Fill the codes popups with N/A. (Header reason , division , detail reason)
    = gfwCodePop(@laCodInfo, "REASON", "N")
    = gfwCodePop(@laCodInfo, "CDIVISION", "N")
    = gfwCodePop(@laCodInfo1, "REASON", "N")
    
    *-- Disable the notes & the object linking buttons in the control pannel.
    SHOW GET pbNotes  DISABLE
    SHOW GET pbLinkTo DISABLE
    
    *-- Disable the delete button with the cancel icon.
    SHOW GET pbDlt,1 DISABLE PROMPT lcCancel

    *B602750,1 Reham On 05/25/1999   *** Begin ***
    *B602750,1 Point to the header folder after save or cancel.
    lnlastfold = lnActFolder
    lnActFolder= 1
    =lfchngfolder(lnActFolder)
    *B602750,1 Disable the detail folder in the select mode
    SHOW GET ibFolder(2) DISABLE
    *B602750,1 Reham On 05/25/1999   *** End   ***
    
  CASE laScrMode[2]
    *-- Define the browse titles.
    *E301643,1 mhm Add new status "Electronic " [Start]
    *lcStatus  = IIF(AT(laData[4],"OCX") > 0 , laStatus[AT(laData[4],"OCX")] , "")
    lcStatus  = IIF(AT(laData[4],"OCXE") > 0 , laStatus[AT(laData[4],"OCXE")] , "")
    *E301643,1 mhm  [End]
    
    *-- Get the tax rate from the lines file.
    lnTaxRate = IIF(SEEK(laData[1] , "RALINE") , RALINE.Tax_Rate , 0)
    
    *B802088,1 Reham On 04/07/99  *** Begin ***
    *B802088,1 Pick the store address in the from address if the store field 
    *B802088,1 is not empty.
    IF (!EMPTY(laData[3]) .AND. SEEK('S'+laData[2]+laData[3] , 'CUSTOMER')) .OR. ;
       (EMPTY(laData[3]) .AND.  SEEK('M'+laData[2],'CUSTOMER'))
    *-- Get the current customer info (name & address).
    *IF SEEK('M'+laData[2],'CUSTOMER')
    *B802088,1 Reham On 04/07/99  *** End   ***
      *lcName     = CUSTOMER.BTname
      *B605881,1 ABD Get the main account addresses or main store addresses not 
      *B605881,1 ABD the alternatives addresses for the account or the store. [Begin]
      *lcFName    = CUSTOMER.BTNAME      
      *lcFaddres1 = CUSTOMER.cAddress12
      *lcFaddres2 = CUSTOMER.cAddress22
      *lcFaddres3 = CUSTOMER.cAddress32
      *lcFaddres4 = ALLTRIM(CUSTOMER.cAddress42)+', '+ALLTRIM(CUSTOMER.cAddress52)
      *lcFaddres5 = CUSTOMER.cAddress62
      lcFName    = CUSTOMER.STNAME
      lcFaddres1 = CUSTOMER.cAddress1
      lcFaddres2 = CUSTOMER.cAddress2
      lcFaddres3 = CUSTOMER.cAddress3
      lcFaddres4 = ALLTRIM(CUSTOMER.cAddress4)+', '+ALLTRIM(CUSTOMER.cAddress5)
      lcFaddres5 = CUSTOMER.cAddress6
      *B605881,1 ABD [End]
      
    ELSE
      STORE SPACE(30) TO lcName , lcFName , lcFaddres1 ,;
                         lcFaddres2 , lcFaddres3 , lcFaddres4 ,;
                         lcFaddres5
    ENDIF
    *B603019,1 Reham On 06/28/1999   *** Begin ***
    *B603019,1 Adjust the popup returned row.
    *-- Fill the warehouse popup with the right value.
    *puWareHous = IIF(ASCAN(laWareHous,laData[7]) > 0 , ASCAN(laWareHous,laData[7])/2 , 0)
    puWareHous = IIF(ASCAN(laWareHous,laData[7]) > 0 , ASUBSCRIPT(laWareHous , ASCAN(laWareHous,laData[7]) , 1) , 0)
    *B603019,1 Reham On 06/28/1999   *** End   ***
    *-- Get the warehouse address.
    IF SEEK(laData[7],'WAREHOUS')
      lcTaddres1 = WAREHOUS.cAddress1
      lcTaddres2 = WAREHOUS.cAddress2
      lcTaddres3 = WAREHOUS.cAddress3
      lcTaddres4 = ALLTRIM(WAREHOUS.cAddress4)+', '+ALLTRIM(WAREHOUS.cAddress5)
      lcTaddres5 = WAREHOUS.cAddress6
    ELSE
      STORE SPACE(30) TO lcTaddres1 , lcTaddres2 , lcTaddres3 ,;
                         lcTaddres4 , lcTaddres5
    ENDIF
    *-- Define check box "[] return entire invoice".
    cbComplete = IIF(laData[17]='Y',.T.,.F.)
    
    *-- Fill the codes popups with its right values in the master files.
    *-- (Header reason , division , detail reason)
    = gfwCodePop(@laCodInfo, "REASON", "T")
    = gfwCodePop(@laCodInfo, "CDIVISION", "T")
    = gfwCodePop(@laCodInfo1, "REASON", "T")
    
    *-- Search for the first R/A line in the master file.
    =SEEK(laData[1] , "RALINE")
    
    *-- Change the delete prompt in the toolbar & the delete message.
    lcPromp    = IIF(laData[4] = 'X' , lcUnCan , lcCancel )
    lcDelMesag = IIF(laData[4] = 'X' , "uncancel" , "cancel" )
    SHOW GET pbDlt,1 PROMPT lcPromp
    
    *-- Control the enabling & disabling of the edit button in the control pannel.
    IF laData[4] $ 'XC'
      SHOW GET pbEdt DISABLE
    ELSE
      SHOW GET pbEdt ENABLE
    ENDIF
    
    *-- Enable the notes & the object linking buttons in the control pannel.
    SHOW GET pbNotes  ENABLE
    SHOW GET pbLinkTo ENABLE
  CASE laScrMode[3]
    *-- Define the browse titles.
    llCUpDate = .T.
    _TALLY    = 0
    
    *B603020,1 Reham On 06/28/1999   *** Begin ***
    *B603020,1 Add lShipped field in the temp lines file.
    *-- Select the current R/A lines in the temp. file.
    *SELECT * , 'S' AS 'CSTATUS' , RECNO() AS 'NRECNO' ;
      FROM (gcDataDir + "Raline") ;
     WHERE Raline.rano = laData[1] ;
      INTO DBF (gcWorkDir + lcTmpRetLn)
    *B606357,1 (Begin) Get the line's original style also.
    *SELECT * , 'S' AS 'CSTATUS' , RECNO() AS 'NRECNO' , ;
           IIF(!EMPTY(laData[7]) , .T. , .F.) AS 'LSHIPPED' ;
      FROM (gcDataDir + "Raline") ;
     WHERE Raline.rano = laData[1] ;
      INTO DBF (gcWorkDir + lcTmpRetLn)
    SELECT * , 'S' AS 'CSTATUS' , RECNO() AS 'NRECNO' , ;
           IIF(!EMPTY(laData[7]) , .T. , .F.) AS 'LSHIPPED', ;
           style as OldSty ;
      FROM (gcDataDir + "Raline") ;
     WHERE Raline.rano = laData[1] ;
      INTO DBF (gcWorkDir + lcTmpRetLn)
    *B606357,1 (End)
    *B603020,1 Reham On 06/28/1999   *** End   ***
    
    lnNoOfLines = _TALLY
    GO TOP
    
    *-- Enable the notes & the object linking buttons in the control pannel.
    SHOW GET pbNotes  ENABLE
    SHOW GET pbLinkTo ENABLE
  CASE laScrMode[4]
    *-- Define the needed variables incase of add mode.
    IF !llAddMode
      *-- Flag to know if enter the add mode before or not.
      llAddMode  = .T.
      llCUpDate  = .T.
      *-- Blank the temp. R/A lines file.
      SELECT (lcTmpRetLn)
      ZAP
      *-- Initialize the needed variables.
      STORE ' ' TO lcOldValue , lcName , lcFName , ;
                   lcFaddres1 , lcFaddres2 , lcFaddres3 ,;
                   lcFaddres4 , lcFaddres5 , lcStyle , lcDesc ,;
                   lcReason , lcCustPLvl , lcDyeLot ,;
                   lcConsol , lcRa_LinNo
      
      STORE 0 TO lnTotQty,lnprice,lnTotal,;
                 lnNoOfLines,lnTaxRate,lnPstRate,lnDiscPCnt ,;
                 lnRa_LinNo , lnTaxzamt
      
      *-- Default the tax rate from the company setup.
      lnTaxRate = lnDefTaxRt
            
      *-- Set R/A status to be open.
      laData[4]  = 'O'
      *E301643,1 mhm Add new status 'Electronic' [Start]
      *lcStatus   = IIF(AT(laData[4],"OCX") > 0 , laStatus[AT(laData[4],"OCX")] , "")
      lcStatus   = IIF(AT(laData[4],"OCXE") > 0 , laStatus[AT(laData[4],"OCXE")] , "")
      *E301643,1 mhm  [End]
      
      laData[5]  = gdSysDate
      laData[6]  = IIF(DOW(gdSysDate+10) <> 7 , gdSysDate+10 , gdSysDate+12)
      laData[13] = 1
      laData[17] = 'N'
      
      *-- Get the warehouse value & its address.
      *E131400,1 NNA 04/12/2006 (Begin) Open Warehous file to get the default warehous as saved at ldefware field then
      *E131400,1 NNA             close file again
      lnOldAlias = SELECT(0)
      SELECT WAREHOUS
      LOCATE FOR ldefware
      IF FOUND()
        puWareHous = ASCAN(laWareHouses,WAREHOUS.CWARECODE)
        puWareHous = IIF(puWareHous=0,1,ASUBSCRIPT(laWareHouses,puWareHous,1))
        laData[7]  = laWareHous[puWareHous,2]
      ELSE
      *E131400,1 NNA (End)

        laData[7]  = laWareHous[1,2]
        puWareHous = 1

      *E131400,1 NNA (Begin)
      ENDIF  
      SELECT(lnOldAlias)
      *E131400,1 NNA (End)

      IF SEEK(laData[7],'WAREHOUS')
        lcTaddres1 = WAREHOUS.cAddress1
        lcTaddres2 = WAREHOUS.cAddress2
        lcTaddres3 = WAREHOUS.cAddress3
        lcTaddres4 = ALLTRIM(WAREHOUS.cAddress4)+', '+ALLTRIM(WAREHOUS.cAddress5)
        lcTaddres5 = WAREHOUS.cAddress6
      ELSE
        STORE SPACE(30) TO lcTaddres1 , lcTaddres2 , lcTaddres3 ,;
                           lcTaddres4 , lcTaddres5
      ENDIF
      laSize     = ""
      laQtyStk   = 0
      cbComplete = .F.
      
      *-- Fill the codes popups with the default values.
      *-- (Header reason , Division , Detail Reason).
      lnReson2   = 1
      = gfwCodePop(@laCodInfo, "REASON", "D")
      laData[18] = laReson1[lnReson1,2]
      = gfwCodePop(@laCodInfo, "CDIVISION", "D")
      laData[19] = laDiv[lnDiv,2]
      = gfwCodePop(@laCodInfo1, "REASON", "N")
      
      _CUROBJ = OBJNUM(laData[2])
    ENDIF
    *-- Assign the check box value.
    cbComplete = IIF(laData[17] = 'Y' , .T. , .F.)
    *-- Assign the codes popup values.
    = gfwCodePop(@laCodInfo, "REASON"    , "V,"+laData[18])
    = gfwCodePop(@laCodInfo, "CDIVISION" , "V,"+laData[19])
    
    *-- Disable the notes button in the control pannel.
    SHOW GET pbNotes DISABLE
ENDCASE

*B602750,1 Reham On 05/25/1999   *** Begin ***
IF !laScrMode[1]
  *B602750,1 Enable the detail folder.
  SHOW GET ibFolder(2) ENABLE
ENDIF
*B602750,1 Reham On 05/25/1999   *** End   ***

*-- Assign the status of each object on the screen does not use var. "lcObjStat"
lcObjStat = IIF(laScrMode[3] .OR. laScrMode[4] , "ENABLE" , "DISABLE")
*B602959,1 Reham On 06/09/1999    *** Begin ***
*B602959,1 Enable the warehouse popup even there is invoice.
*lcWarStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. llMultiWH .AND. EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcWarStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. llMultiWH , "ENABLE" , "DISABLE")
*B602959,1 Reham On 06/09/1999    *** End   ***
lcInqStat = IIF(!EMPTY(laData[10]) .AND. !laScrMode[1] , "ENABLE" , "DISABLE")

*B603397,1 [START] Don't include lnNoOfLines when determining the invoice object status
*                  as it should be always enabled in Add/Edit modes

*lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0 , "ENABLE" , "DISABLE")
lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]), "ENABLE" , "DISABLE")
*B603397,1 [End]

*B602863,1 Reham On 05/12/1999   *** Begin ***
*B602863,1 Disable the division if entered any styles.
*lcDivStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcDivStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) .AND. lnNoOfLines = 0 , "ENABLE" , "DISABLE")
*B602863,1 Reham On 05/12/1999   *** End   ***

lcCurStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcExRStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) .AND. !EMPTY(laData[14]) .AND. llEditExRt .AND. (laData[14] <> gcBaseCurr) , "ENABLE" , "DISABLE")

*B603397,1 [START] Don't include lnNoOfLines when determining the invoice object status
*                  as it should be always enabled in Add/Edit modes
*lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0  .AND. !EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcTmpRetLn) .AND. !EMPTY(laData[10]) , "ENABLE" , "DISABLE")
*B603397,1 [END]

lcRelStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) , "ENABLE" ,"DISABLE")

*B802948,1 Reham On 04/30/2000   *** Begin ***
*B802948,1 Define variable to control the enabling & disabling of the account field.
lcAccStat = IIF(laScrMode[2] , "DISABLE" , "ENABLE")
*B802948,1 Reham On 04/30/2000   *** End   ***

*-- Set flag to rebrowse in the detail folder.
llReBrowse = .T.

*-- Call local function to change between the header & the detail folder.
= lfActFolder()

*-- Select the main file.
SELECT RETAUTH

*!*************************************************************
*! Name      : lfRefHdr
*! Developer : Reham Al-Allamy
*! Date      : 02/15/1998
*! Purpose   : Function to refresh the objects in the header folder.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfRefHdr()
*!*************************************************************
*
FUNCTION lfRefHdr
PRIVATE lnCount , lcCount

*B802948,1 Reham On 04/30/2000   *** Begin ***
*B802948,1 Refresh the account objects by the variable defined.
SHOW GET ibAccount &lcAccStat
SHOW GET laData[2] &lcAccStat
*B802948,1 Reham On 04/30/2000   *** End   ***

*-- Refresh the objects in screen : (lcRtAth1)
SHOW GET laData[5] &lcObjStat
SHOW GET laData[6] &lcObjStat
SHOW GET laData[8] &lcObjStat
SHOW GET laData[9] &lcObjStat
SHOW GET lnReson1  &lcObjStat
SHOW GET lcFName   &lcObjStat
SHOW GET lnTaxRate &lcObjStat

FOR lnCount = 25 TO 31
  SHOW GET laData[lnCount] &lcObjStat
ENDFOR

*-- Refresh the address objects.
FOR lnCount = 1 TO 5
  lcCount = ALLTRIM(STR(lnCount))
  SHOW GET lcFAddres&lcCount &lcObjStat
  SHOW GET lcTAddres&lcCount &lcObjStat
ENDFOR

*B602863,1 Reham On 05/12/1999   *** Begin ***
*B602863,1 Disable the division if entered any styles.
lcDivStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) .AND. lnNoOfLines = 0 , "ENABLE" , "DISABLE")
*B602863,1 Reham On 05/12/1999   *** End   ***

SHOW GET pbInqInv   &lcInqStat
SHOW GET ibInvoice  &lcInvStat
SHOW GET laData[10] &lcInvStat
SHOW GET cbComplete &lcEntrStat
SHOW GET ibOrder    &lcRelStat
SHOW GET laData[11] &lcRelStat
SHOW GET laData[12] &lcRelStat
*B605776,1 ASH 05/13/2002 (Begin) Enable the cartons field to let the user change it's numer to be used while printing the stickers.
*SHOW GET laData[13] &lcRelStat
SHOW GET laData[13] &lcInvStat
*B605776,1 ASH 05/13/2002 (End)
SHOW GET puWareHous &lcWarStat
SHOW GET lnDiv      &lcDivStat
SHOW GET ibCurCode  &lcCurStat
SHOW GET laData[14] &lcCurStat
SHOW GET laData[16] &lcExRStat

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Func. to switch between folders.
*!*************************************************************
*! Calls     : lfBrLine
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActFolder()
*!*************************************************************
*
FUNCTION lfActFolder

SELECT RETAUTH
DO CASE
  CASE lnActFolder = 1
    SHOW GETS WINDOW (lcRtAth2B) DISABLE ONLY
    *-- Call local function to refresh the header objects.
    =lfRefHdr()
    
  CASE lnActFolder = 2
    
    *E301077,4 Reham On 12/17/98   *** Begin ***
    *E301077,4 Open the Invoice header, Invoice lines & style dyelot file.
    =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
    =gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH')
    =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
    *E301077,4 Reham On 12/17/98   *** End   ***
    
    SHOW GETS WINDOW (lcRtAth1) DISABLE ONLY
    SHOW GET ibTab2b_1 ENABLE
    *-- Call the browse function if the rebrowse flag is set on.
    IF llReBrowse
      =lfBrLine()
      llReBrowse = .F.
    ENDIF
    *-- Call the when fuction to refresh the objects.
    =lfWhenBrow()

    *B603416,1 Reham On 04/27/2000   *** Begin ***
    *B603416,1 Set flag to true to prove entering the detail folder.
    llEnterDet = .T.
    *B603416,1 Reham On 04/27/2000   *** End   ***
ENDCASE
SELECT RETAUTH

*!*************************************************************
*! Name      : lfWhenAll
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : When function for all the string objects.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfWhenAll()
*!*************************************************************
*
FUNCTION lfWhenAll

*-- Save the current value of the current object.
lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for the R/A object.
*!*************************************************************
*! Calls     : gfSeekRec
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfvData_1()
*!*************************************************************
*
FUNCTION lfvData_1

*-- Fill the R/A with zeros on the left if not browsing.
llBrowse  = IIF((!llBrowse .AND. "?" $ laData[1]) .OR. llBrowse , .T. , .F.)
laData[1] = IIF(llBrowse .OR. EMPTY(laData[1]) , laData[1] , PADL(ALLTRIM(laData[1]) , 6 , "0"))


IF (llBrowse .OR. !EMPTY(laData[1])) .AND. LASTKEY() = 13
  *-- Save the current browse field var.
  lcSavBrFld = lcBrFields
  *-- Fill the browse fields.
  lcBrFields = "Retauth.rano:7 :H='R/A#', Retauth.status:2 :H='S', "+;
               "Retauth.account:6 :H='Acct#', "+;
               "lcDummy=LOOKUP(CUSTOMER.BtName,'M'+Retauth.account,CUSTOMER.ACCOUNT,'CUSTOMER') :H='Name':15, "+;
               "Retauth.store:9 :H='Store', Retauth.radate:8 :H='Issued', "+;
               "Retauth.void:8 :H='Void', Retauth.auth:7 :H='Auth.', "+;
               "Retauth.tran:7 :H='Credit #'"
  
  *B802948,1 Reham On 04/30/2000   *** Begin ***
  *B802948,1 Give the ability to browse RAs by customer.
  IF !EMPTY(laData[1]) AND !EMPTY(laData[2]) AND !SEEK(laData[1] , "RETAUTH")
    SELECT RETAUTH
    SET ORDER TO TAG Retautha
    
    DECLARE laRA[1]
    laRA[1]   = laData[1]
    =gfBrows("laData[2]" , "RANO" , "laRA")
    laData[1] = IIF(laData[1] = laRA[1] , SPACE(6) , laRA[1])
    
    SET ORDER TO TAG Retauth
    
    llBrowse = .F.
  ENDIF
  *B802948,1 Reham On 04/30/2000   *** End   ***
  
  SHOW GET laData[1]
  
  *B802948,1 Reham On 04/30/2000   *** Begin ***
  *B802948,1 If the RA object not empty, call global function to handle the modes.
  IF llBrowse OR !EMPTY(laData[1])
  *B802948,1 Reham On 04/30/2000   *** End   ***
    *-- Call global function to handle the action & the screen mode 
    *-- according to this action.
    =gfSeekRec()
    *-- Blank the credit memo field to get a seq. no. upon saving.
    laData[1] = IIF(laScrMode[4] , SPACE(6) , laData[1])
  *B802948,1 Reham On 04/30/2000   *** Begin ***
  ENDIF
  *B802948,1 Reham On 04/30/2000   *** End   ***
  
  *-- Restore the browse fields var.
  lcBrFields = lcSavBrFld
ENDIF
llBrowse = .F.

SELECT RETAUTH

*!*************************************************************
*! Name      : lfvData_2
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for the customer object.
*!*************************************************************
*! Calls     : CUSBROWM
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvData_2()
*!*************************************************************
*
FUNCTION lfvData_2

IF (llBrowse .OR. !(laData[2] == lcOldValue))
  IF !SEEK ("M" + laData[2] , "CUSTOMER") .OR. llBrowse
    *-- Call the main customer browse.
    XACCOUNT  = laData[2]
    DO CUSBROWM WITH XACCOUNT
    laData[2] = XACCOUNT
  ENDIF
  *B603986,1 KAM 11/05/2000 we still validition for cancelled and potentail
  *B603986,1                customer[start]
  *B603084,1 RAMY [START]
  *IF THE CUSTOMER IS NON-ACTIVE
  *IF !EMPTY(laData[2]) .AND. CUSTOMER.Status <> 'A'
    *B603084,1 Message : 46036
    *B603084,1                "This is a non-active account."
    *B603084,1 Button  : 00000
    *B603084,1                         <  Ok  >
    *=gfModalGen('TRM46036B00000','ALERT')
  *IF THE CUSTOMER IS CANCELED OR POTENTAIL
  IF !EMPTY(laData[2]) .AND. ! CUSTOMER.Status $ 'AH'
    *B603986,1 Message : 46038
    *B603986,1 "Unable to create a new R/A, customer status might be canceled ;" 
    *B603986,1  or potential."
    *B603986,1 Button  : 00000
    *B603986,1                         <  Ok  >

    *B605151,1 (Begin) Modify the message as it's being called now from Credit Memo also..
    *=gfModalGen('TRM46038B00000','ALERT')
    =gfModalGen('TRM46038B00000','ALERT','R/A')
    *B605151,1 (End)

    laData[2] = ''
    laData[3] = ''
    lcName    = ''
    SHOW GET laData[3]
    SHOW GET lcName 
   RETURN
  ENDIF && END (IF !EMPTY(laData[2]) .AND. CUSTOMER NOT POTENTIAL OR CANCELDED
  *B603084,1 RAMY [END]
  *B603986,1 KAM [END]

  SHOW GET laData[2]

  *B802088,1 Reham On 04/07/99  *** Begin ***
  *B802088,1 Pick the store address in the from address if the store field 
  *B802088,1 is not empty.
  IF (!EMPTY(laData[3]) .AND. SEEK('S'+laData[2]+laData[3] , 'CUSTOMER')) .OR. ;
     (EMPTY(laData[3]) .AND.  SEEK('M'+laData[2],'CUSTOMER'))
    *-- Get the current customer address.
    

    *B605881,1 ABD Get the main account addresses or main store addresses not 
    *B605881,1 ABD the alternatives addresses for the account or the store. [Begin]
    *lcFName    = CUSTOMER.BTNAME    
    *lcFaddres1 = CUSTOMER.cAddress12
    *lcFaddres2 = CUSTOMER.cAddress22
    *lcFaddres3 = CUSTOMER.cAddress32
    *lcFaddres4 = ALLTRIM(CUSTOMER.cAddress42)+','+ALLTRIM(CUSTOMER.cAddress52)
    *lcFaddres5 = CUSTOMER.cAddress62
    lcFName    = CUSTOMER.STNAME
    lcFaddres1 = CUSTOMER.cAddress1
    lcFaddres2 = CUSTOMER.cAddress2
    lcFaddres3 = CUSTOMER.cAddress3
    lcFaddres4 = ALLTRIM(CUSTOMER.cAddress4)+','+ALLTRIM(CUSTOMER.cAddress5)
    lcFaddres5 = CUSTOMER.cAddress6
    *B605881,1 ABD [End]
    
  ELSE
    STORE SPACE(1) TO lcFName,lcFaddres1,lcFaddres2,lcFaddres3,lcFaddres4,lcFaddres5
  ENDIF
  *B802088,1 Reham On 04/07/99  *** End   ***
  
  IF SEEK("M" + laData[2] , "CUSTOMER")
    lcName     = CUSTOMER.BTname
    lcCustPLvl = CUSTOMER.PRICELVL
    lnCusPstRt = CUSTOMER.nTaxRate
    lnPstRate  = IIF(llIsCanada , CUSTOMER.nTaxRate , 0)
    
    *B802213,1 Reham On 05/02/99   *** Begin ***
    *lnTaxRate  = IIF(!EMPTY(CUSTOMER.nTaxRate) .AND. !llIsCanada , CUSTOMER.nTaxRate , lnDefTaxRt)
    lnTaxRate  = IIF(!llIsCanada , CUSTOMER.nTaxRate , 0)
    *B802213,1 Reham On 05/02/99   *** End   ***
    
    laData[14] = CUSTOMER.cCurrCode
    *B602771,1 Reham On 04/13/99   *** Begin ***
    IF llMulCurr
    *B602771,1 Reham On 04/13/99   *** End   ***
      IF laData[14] = gcBaseCurr
        STORE 1 TO laData[15] , laData[16]
      ELSE
        *-- If the currency is different from the base currency, get the rate & the unit.
        laData[16] = gfChkRate('lnCurrUnit' , laData[14] , gdSysDate,;
                               .T. , gcAct_Comp , .F. , llEditExRt)
        laData[15] = lnCurrUnit
      ENDIF
    *B602771,1 Reham On 04/13/99   *** Begin ***
    ELSE
      laData[14] = gcBaseCurr
      STORE 1 TO laData[15] , laData[16]
    ENDIF
    *B602771,1 Reham On 04/13/99   *** End   ***
  ELSE
    *-- If the customer not found, blank the related variable.
    STORE SPACE(5) TO laData[2]
    STORE SPACE(1) TO lcName , lcCustPLvl
    lnCusPstRt = 0
    lnPstRate  = 0
    
    *B802948,1 Reham On 05/08/2000   *** Start ***
    *lnTaxRate  = lnDefTaxRt
    *laData[14] = gcBaseCurr
    *STORE 1 TO laData[15] , laData[16]
    lnTaxRate  = IIF(laScrMode[1] , 0 , lnDefTaxRt)
    laData[14] = IIF(laScrMode[1] , "" , gcBaseCurr)
    laData[15] = IIF(laScrMode[1] , 0 , 1)
    laData[16] = IIF(laScrMode[1] , 0 , 1)
    *B802948,1 Reham On 05/08/2000   *** Start ***
  ENDIF
  lcExRStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) .AND. !EMPTY(laData[14]) .AND. llEditExRt .AND. (laData[14] <> gcBaseCurr) , "ENABLE" , "DISABLE")
  
  *-- Refresh the related objects.
  SHOW GET lcName
  SHOW GET lcFName
  SHOW GET laData[14]
  SHOW GET lnTaxRate
  IF lnActFolder = 1
    SHOW GET laData[16] &lcExRStat
  ENDIF
  *-- Refresh the address objects.
  FOR lnCount = 1 TO 5
    lcCount = ALLTRIM(STR(lnCount))
    SHOW GET lcFaddres&lcCount
  ENDFOR
  
  *B802948,1 Reham On 04/30/2000   *** Begin ***
  *B802948,1 If the RA object is empty, go to fill it.
  IF laScrMode[1] AND EMPTY(ladata[1])
    _CUROBJ = OBJNUM(laData[1])
  ENDIF
  *B802948,1 Reham On 04/30/2000   *** End   ***
ENDIF
llBrowse   = .F.
SELECT RETAUTH

*!*************************************************************
*! Name      : lfvData_3
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid func. for the store object.
*!*************************************************************
*! Calls     : CUSBROWS
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvData_3()
*!*************************************************************
*
FUNCTION lfvData_3

* IF ACCOUNT CODE IS EMPTY AND ENTRING STORE CODE
IF EMPTY(laData[2]) .AND. !EMPTY(laData[3])
  *B603133,1 Message : 02040
  *B603133,1              "You have to enter an account code."
  *B603133,1 Button  : 00000
  *B603133,1                            <  Ok  >
  =gfModalGen('TRM02040B00000','ALERT')
  laData[3] = ''
  RETURN
ENDIF  && END (IF EMPTY(laData[2]) .AND. !EMPTY(laData[3]))
*B603133,1 RAMY [END]

IF (llBrowse .OR. !(laData[3] == lcOldValue))
  IF !SEEK ('S'+laData[2]+laData[3] , "CUSTOMER") .OR. llBrowse
    xStore = laData[3]
    *-- Call function to browse the available stores for the current customer.
    IF !CUSBROWS(laData[2],.T.)
      llBrowse = .F.
      laData[3] = SPACE(8)
    ELSE
      laData[3] = xStore
    ENDIF
  ENDIF
  SHOW GET laData[3]
  IF !(laData[3] == lcOldValue)
    *-- If there is valid store entered, get its address.
    *B605881,1 ABD Get the main account addresses or main store addresses not 
    *B605881,1 ABD the alternatives addresses for the account or the store. [Begin]
    *IF !EMPTY(laData[3])
    *  *B802088,1 Reham On 04/07/99   *** Begin ***
    *  *lcName     = CUSTOMER.STname
    *  lcFName     = CUSTOMER.BTname
    *  *B802088,1 Reham On 04/07/99   *** End   ***
    *  lcFaddres1 = CUSTOMER.cAddress12
    *  lcFaddres2 = CUSTOMER.cAddress22
    *  lcFaddres3 = CUSTOMER.cAddress32
    *  lcFaddres4 = ALLTRIM(CUSTOMER.cAddress42)+','+ALLTRIM(CUSTOMER.cAddress52)
    *  lcFaddres5 = CUSTOMER.cAddress62
    *ELSE
    *  *-- If there is no store entered, get the customer address.
    *  IF SEEK("M" + laData[2] , "CUSTOMER")
    *    *B802088,1 Reham On 04/07/99   *** Begin ***
    *    *lcName     = CUSTOMER.STname
    *    lcFName     = CUSTOMER.BTname
    *    *B802088,1 Reham On 04/07/99   *** End   ***
    *    lcFaddres1 = CUSTOMER.cAddress12
    *    lcFaddres2 = CUSTOMER.cAddress22
    *    lcFaddres3 = CUSTOMER.cAddress32
    *    lcFaddres4 = ALLTRIM(CUSTOMER.cAddress42)+','+ALLTRIM(CUSTOMER.cAddress52)
    *    lcFaddres5 = CUSTOMER.cAddress62
    *    
    IF !EMPTY(laData[3])
      lcFName    = CUSTOMER.STNAME
      lcFaddres1 = CUSTOMER.cAddress1
      lcFaddres2 = CUSTOMER.cAddress2
      lcFaddres3 = CUSTOMER.cAddress3
      lcFaddres4 = ALLTRIM(CUSTOMER.cAddress4)+','+ALLTRIM(CUSTOMER.cAddress5)
      lcFaddres5 = CUSTOMER.cAddress6
    ELSE
      *-- If there is no store entered, get the customer address.
      IF SEEK("M" + laData[2] , "CUSTOMER")
        lcFName    = CUSTOMER.STNAME
        lcFaddres1 = CUSTOMER.cAddress1
        lcFaddres2 = CUSTOMER.cAddress2
        lcFaddres3 = CUSTOMER.cAddress3
        lcFaddres4 = ALLTRIM(CUSTOMER.cAddress4)+','+ALLTRIM(CUSTOMER.cAddress5)
        lcFaddres5 = CUSTOMER.cAddress6
        *B605881,1 ABD [End]
        
      ELSE
        *-- If there is no customer , blank the address fields.
        STORE SPACE(5) TO laData[2]
        STORE SPACE(1) TO lcFName,lcFaddres1,lcFaddres2,;
                          lcFaddres3,lcFaddres4,lcFaddres5
      ENDIF
    ENDIF
    *-- Refresh the customer name & address objects.
    SHOW GET lcFName
    FOR lnCount = 1 TO 5
      lcCount = ALLTRIM(STR(lnCount))
      SHOW GET lcFaddres&lcCount
    ENDFOR
  ENDIF
ENDIF

llBrowse = .F.
SELECT RETAUTH

*!*************************************************************
*! Name      : lfvData_7
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid func. for the warehouse object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvData_7()
*!*************************************************************
*
FUNCTION lfvData_7

*B604218,1 KAM 10/03/2001 display message if user select location not assign to style[start]
PRIVATE lnRecNo
lcPrvAlis = ALIAS()      && Save current alias.
lnRecNo=RECNO()
llFirst   = .T.          && flag to display the message only once.
llAssign  = .F.          && Flag to assign all styles to changed warehouse.
SELECT (lcTmpRetLn)      && Select temp. file hold return auth lines.
IF RECCOUNT()>0
  *--if there is one or more style assign to location
  *B605970,1 ASH 05/22/2002 (Begin) Open the stydye file to avoid error 'alias not found' when editing an existing RA, and try to change the warehouse.
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
  SELECT (lcTmpRetLn)
  *B605970,1 ASH 05/22/2002 (End)
  SCAN
  *-- Check if there is any styles not assigned to the changed warehouse.
    IF !SEEK(PADR(&lcTmpRetLn..Style,19) + laWareHous[puWareHous,2] + SPACE(10) , 'StyDye')
      IF llFirst
        llFirst = .F.
        *** There is one or more styles not assigned to location: laWareHous[puWareHous,2]. ***
        *** <  Assign  > - < Cancel > ***
        IF gfModalGen("QRM46034B46003" , "DIALOG" , laWareHous[puWareHous,2]) = 1
          *--Set flag to assign the unsaaigned styles to the current warehouse.
          llAssign = .T.
        ENDIF
      ENDIF
      *--Assign the styles to the selected location.
      IF llAssign
        laData[7] = laWareHous[puWareHous,2]
        =SEEK(laData[7],'WareHous')
        DO gpAdStyWar WITH PADR(&lcTmpRetLn..Style,19) , SPACE(10) , laWareHous[puWareHous,2]
      ELSE
        *-- Do not assign & restore the old location.
        puWareHous = lnOldValue
        =SEEK(laData[7],'WareHous')
        *B604218,4 KAM 29/03/2001 [start]
        *--refresh oldvalue
        SHOW GET puWareHous
        *B604218,4 KAM[end]
        SELECT(lcPrvAlis)        
        RETURN
      ENDIF
    ENDIF
    SELECT (lcTmpRetLn)
  ENDSCAN
ENDIF &&RECCOUNT()>0
SELECT(lcPrvAlis)
*B604218,1 KAM [end]
*-- If enetered valid warehouse, get its address.
laData[7] = laWareHous[puWareHous,2]
IF SEEK(laData[7] , 'WareHous')
  lcTaddres1 = WAREHOUS.cAddress1
  lcTaddres2 = WAREHOUS.cAddress2
  lcTaddres3 = WAREHOUS.cAddress3
  lcTaddres4 = ALLTRIM(WAREHOUS.cAddress4)+', '+ALLTRIM(WAREHOUS.cAddress5)
  lcTaddres5 = WAREHOUS.cAddress6
  *-- Refersh the warehouse address.
  FOR lnCount = 1 TO 5
    lcCount = ALLTRIM(STR(lnCount))
    SHOW GET lcTaddres&lcCount
  ENDFOR
ENDIF
*!*************************************************************
*! Name      : lfwData_6
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : When function for the void date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfwData_6()
*!*************************************************************
*
FUNCTION lfwData_6

*-- Save the old void date.
ldOldValue = laData[6]

*!*************************************************************
*! Name      : lfvData_6
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for the void date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvData_6()
*!*************************************************************
*
FUNCTION lfvData_6

*-- Validate void date to be greater or equal the transaction date.
IF laData[6] < laData[5]
  *** Void after cannot be less than the entered date. ***
  *** <  Ok  > ***
  =gfModalGen("INM46016B00000" , "DIALOG")
  laData[6] = ldOldValue
  SHOW GET laData[6]
  RETURN
ENDIF

*!*************************************************************
*! Name      : lfvInqInv
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Inquire the current invoice
*!*************************************************************
*! Calls     : gpDoProg, gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvInqInv()
*!*************************************************************
*
FUNCTION lfvInqInv

*E301077,4 Reham On 12/17/98   *** Begin ***
*E301077,4 Open the invoice header file.
=gfOpenFile(gcDataDir+'InvHdr',gcDataDir+'InvHdr','SH')
*E301077,4 Reham On 12/17/98   *** Begin ***

SELECT INVHDR
SET ORDER TO TAG INVHDR
*-- If there is valid invoice, inquire with the invoice no.
IF SEEK(laData[10] , "INVHDR")
  *-- Clear the trapped key.
  PUSH KEY
  ON KEY
  
  *-- Call the invoice inquire screen.
  lcInv = "'"+laData[10]+"'"
  DO gpDoProg WITH 'AWRARDINV' , .F. , 'AR' , lcInv
  
  *-- Trap the needed keys.
  POP KEY
ELSE
  *** Invoice # laData[10] is missing, Unable to inquire. ***
  *** <  Ok  > ***
  =gfModalGen("TRM46011B00000" , "DIALOG" , laData[10])
ENDIF

*!*************************************************************
*! Name      : lfvData_10
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for the invoice object.
*!*************************************************************
*! Calls     : gfBrows, gfwCodePop
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvData_10()
*!*************************************************************
*
FUNCTION lfvData_10

*E301077,4 Reham On 12/17/98   *** Begin ***
*E301077,4 Open the invoice header file & the consolidated invoice header file.
=gfOpenFile(gcDataDir+'InvHdr',gcDataDir+'InvHdr','SH')
=gfOpenFile(gcDataDir+'ConsInvH',gcDataDir+'ConsInvH','SH')
*E301077,4 Reham On 12/17/98   *** End   ***

IF (llBrowse .OR. !(laData[10] == lcOldValue))
  lcInvExp = IIF(EMPTY(laData[3]) , ".T." , "((INVHDR.CONSOL='Y' AND SEEK(laData[10]+laData[3],'CONSINVH')) OR INVHDR.STORE=laData[3])")
  IF !EMPTY(laData[10]) .OR. llBrowse
    *-- Add zeros to the right of the invoice no.
    laData[10] = IIF(llBrowse .OR. EMPTY(laData[10]) , laData[10] , PADL(ALLTRIM(laData[10]) , 6 , "0"))
    SELECT INVHDR
    SET ORDER TO TAG INVHDRA
    IF SEEK(laData[2] + laData[10] , "INVHDR") .AND. &lcInvExp .AND. !llBrowse
      *E301090,1 Reham On 13/12/98   *** Begin ***
      *E301090,1 Do not accept any voided invoices.
      IF InvHdr.Status = 'V'
        *** Cannot accept voided invoices. ***
        *** < Ok > ***
        =gfModalGen("INM46032B00000" , "Dialog")
        laData[10]  = lcOldValue
      ENDIF
      *E301090,1 Reham On 13/12/98   *** End   ***
    ELSE
      *E301090,1 Reham On 13/12/98   *** Begin ***
      *LOCATE FOR Account = laData[2] .AND. ;
                 IIF(EMPTY(laData[3]),.T.,;
                   ((CONSOL='Y' AND SEEK(INVHDR.INVOICE+laData[3],'CONSINVH')) ;
                   OR STORE=laData[3]))
      *B603238,1 [Start]enhance the invoice browsing speed
      IF SEEK(laData[2])
        *LOCATE FOR Account = laData[2] .AND. ;
                   IIF(EMPTY(laData[3]),Invhdr.status<>'V',;
                     ((Invhdr.status<>'V' AND CONSOL='Y' AND SEEK(INVHDR.INVOICE+laData[3],'CONSINVH')) ;
                     OR (Invhdr.status<>'V' AND STORE=laData[3])))

        LOCATE REST WHILE Account = laData[2] FOR Invhdr.status<>'V' AND ;
                          IIF(EMPTY(laData[3]),.T.,STORE=laData[3] OR ;
                          (CONSOL='Y' AND SEEK(INVHDR.INVOICE+laData[3],'CONSINVH') ))

      *E301090,1 Reham On 13/12/98   *** End   ***
      ENDIF
      *B603238,1 [End]
      
      IF !FOUND()
        *** There is no records to display. ***
        *** <  Ok  > ***
        =gfModalGen("INM00052B00000" , "Dialog")
        laData[10]  = lcOldValue
      ELSE
        *-- Do the soft seek to get the first available record 
        *-- similar to the entered invoice.
        IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
          GO RECNO(0)
        ELSE
          GO TOP
        ENDIF
        lcSaveBrow = lcBrFields
        *B603238,1 [Start] Include note1 and note2 in browse
        *-- Set the browse fields.
        *lcBrfields = "INVOICE :H='Invoice' , STATUS :H='S' ,"+;
                     "INVDATE :H='Date' , STORE :H='Store' ,"+;
                     "ORDER :H='Order' ,"+;
                     "lcDummy = LOOKUP(CUSTOMER.StName,'M'+laData[2],CUSTOMER.Account,'CUSTOMER'):H='Name':22 ,"+;
                     "lcTrmDesc = gfCodDes(cTermCode,'CTERMCODE'):23:R:H='Terms' ,"+;
                     "Rep1 :H='Rp1' , Rep2 :H='Rp2', TOTALCHG :H='Amount'"

        lcBrfields = "INVOICE :H='Invoice' , STATUS :H='S' ,"+;
                     "INVDATE :H='Date' , STORE :H='Store' ,"+;
                     "ORDER :H='Order' ,"+;
                     "lcDummy = LOOKUP(CUSTOMER.StName,'M'+laData[2],CUSTOMER.Account,'CUSTOMER'):H='Name':22 ,"+;
                     "lcTrmDesc = gfCodDes(cTermCode,'CTERMCODE'):23:R:H='Terms' ,"+;
                     "Rep1  :H='Rp1' , Rep2 :H='Rp2', TOTALCHG :H='Amount',"+;
                     "Note1 :H='Note1' , Note2 :H='Note2'"
        *B603238,1 [End]
        *E301090,1 Reham On 13/12/98   *** Begin ***
        *E301090,1 Do not accept any voided messages..
        *lcKey      = "laData[2] FOR IIF(EMPTY(laData[3]),.T.,;
                     ((CONSOL='Y' AND SEEK(INVHDR.INVOICE+laData[3],'CONSINVH')) ;
                     OR STORE=laData[3]))"
        lcKey      = "laData[2] FOR IIF(EMPTY(laData[3]),Invhdr.status<>'V',;
                     ((Invhdr.status<>'V' AND CONSOL='Y' AND SEEK(INVHDR.INVOICE+laData[3],'CONSINVH')) ;
                     OR (Invhdr.status<>'V' AND STORE=laData[3])))"
        *E301090,1 Reham On 13/12/98   *** End   ***
        
        lcOld_ttl  = lcFile_ttl
        lcFile_ttl = "Invoices"
        DECLARE laInvoice[1]
        laInvoice[1] = laData[10]
        
        *-- Call the global browse.
        =gfBrows(lcKey,"invoice","laInvoice")
        
        lcBrFields   = lcSaveBrow
        lcFile_ttl   = lcOld_ttl
        
        laData[10]  = IIF(laData[10] = laInvoice[1] , lcOldValue , laInvoice[1])
      ENDIF
    ENDIF
  ENDIF
  *-- Get the default values for the current customer+invoice.
  *E301090,1 Reham On 13/12/98   *** Begin ***
  *E301090,1 Do not accept any voided invoices.
  *IF SEEK(laData[2] + laData[10] , "INVHDR") .AND. !EMPTY(laData[10])
  IF SEEK(laData[2] + laData[10] , "INVHDR") .AND. InvHdr.Status <> 'V' .AND. !EMPTY(laData[10])
    *E301090,1 Reham On 13/12/98   *** End   ***
    
    *B603397,1 [START] Don't re collect invoice data if there
    *                  is lines in the detail folder
    *                  otherwise
    *                  enavle Return Entire INV. Check Box
    IF EOF(lcTmpRetLn)
      cbComplete = .F.
    *B603397,1 [ENDIF]
      *-- If multi warehouse & the invoice consoloidated & exist in the 
      *-- consolidated invoice header.
      IF llMultiWh .AND. INVHDR.Consol = 'Y' .AND. SEEK(laData[10] + laData[3] , 'CONSINVH')
        laData[7]   = IIF(llMultiWH , CONSINVH.cWareCode , lcDefWare)
        *B603019,1 Reham On 06/28/1999   *** Begin ***
        *B603019,1 Adjust the popup returned row.
        *puWareHous  = IIF(ASCAN(laWareHous,laData[7]) > 0 , ASCAN(laWareHous,laData[7])/2 , 1)
        puWareHous  = IIF(ASCAN(laWareHous,laData[7]) > 0 , ASUBSCRIPT(laWareHous , ASCAN(laWareHous,laData[7]) , 1) , 1)
        *B603019,1 Reham On 06/28/1999   *** End   ***
        laData[11]  = CONSINVH.ORDER
        laData[12]  = CONSINVH.CustPo
        laData[13]  = CONSINVH.Cartons
        laData[14]  = INVHDR.cCurrCode
        laData[15]  = INVHDR.nCurrUnit
        laData[16]  = INVHDR.nExRate
        laData[19]  = CONSINVH.cDivision
        = gfwCodePop(@laCodInfo, "CDIVISION" , "V,"+laData[19])
        lnTaxRate   = CONSINVH.TAX_RATE
        lnPstRate   = CONSINVH.nPstRate
        lnDiscPCnt  = CONSINVH.DiscPCnt
        lcCustPLvl  = IIF(LEN(ALLTRIM(lcCustPLvl))=0,'A',lcCustPLvl)
        lcConsol    = INVHDR.Consol
      ELSE
        *-- If single or multi warehouse or the invoice not consoloidated or
        *-- the invoice does not exist in the consolidated invoice header.
        laData[7]   = IIF(llMultiWH , INVHDR.cWareCode , lcDefWare)
        *B603019,1 Reham On 06/28/1999   *** Begin ***
        *B603019,1 Adjust the popup returned row.
        *puWareHous  = IIF(ASCAN(laWareHous,laData[7]) > 0 , ASCAN(laWareHous,laData[7])/2 , 1)
        puWareHous  = IIF(ASCAN(laWareHous,laData[7]) > 0 , ASUBSCRIPT(laWareHous , ASCAN(laWareHous,laData[7]) , 1) , 1)
        *B603019,1 Reham On 06/28/1999   *** End   ***
        laData[11]  = INVHDR.ORDER
        laData[12]  = INVHDR.CustPo
        laData[13]  = INVHDR.Cartons
        laData[14]  = INVHDR.cCurrCode
        laData[15]  = INVHDR.nCurrUnit
        laData[16]  = INVHDR.nExRate
        laData[19]  = INVHDR.cDivision
        = gfwCodePop(@laCodInfo, "CDIVISION" , "V,"+laData[19])
        lnTaxRate   = INVHDR.TAX_RATE
        lnPstRate   = INVHDR.nPstRate
        lnDiscPCnt  = INVHDR.DiscPCnt
        lcCustPLvl  = IIF(LEN(ALLTRIM(lcCustPLvl))=0,'A',lcCustPLvl)
        lcConsol    = INVHDR.Consol
      ENDIF
    *B603397,1 [START] Else IF EOF() Get invoice order # and CUSTPO#
    ELSE
      laData[11]  = INVHDR.ORDER
      laData[12]  = INVHDR.CustPo
    ENDIF
    *B603397,1 [End]
  ELSE
    *-- Blank the related info.
    laData[10] = SPACE(6)
    laData[11] = SPACE(6)
    laData[12] = SPACE(15)
    laData[13] = 1
    laData[14] = CUSTOMER.cCurrCode
    laData[15] = 0
    laData[16] = 0
    lnPstRate  = IIF(llIsCanada , CUSTOMER.nTaxRate , 0)
    *B802213,1 Reham On 05/02/99   *** Begin ***
    *lnTaxRate  = IIF(!EMPTY(CUSTOMER.nTaxRate) .AND. !llIsCanada , CUSTOMER.nTaxRate , lnDefTaxRt)
    lnTaxRate  = IIF(!llIsCanada , CUSTOMER.nTaxRate , 0)
    *B802213,1 Reham On 05/02/99   *** End   ***
    lnDiscPCnt = 0
    lcCustPLvl = IIF(LEN(ALLTRIM(lcCustPLvl))=0,'A',lcCustPLvl)
    lcConsol   = "N"
  ENDIF
  SELECT INVHDR
  SET ORDER TO TAG INVHDR
ENDIF

*-- Update the status variables for all the objects in the main screen.
lcObjStat = IIF(laScrMode[3] .OR. laScrMode[4] , "ENABLE" , "DISABLE")
*B602959,1 Reham On 06/09/1999    *** Begin ***
*B602959,1 Enable the warehouse popup even there is invoice.
*lcWarStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. llMultiWH .AND. EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcWarStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. llMultiWH , "ENABLE" , "DISABLE")
*B602959,1 Reham On 06/09/1999    *** End   ***
lcInqStat = IIF(!EMPTY(laData[10]) .AND. !laScrMode[1] , "ENABLE" , "DISABLE")

*B603397,1 [START] Don't include lnNoOfLines when determining the invoice object status
*                  as it should be always enabled in Add/Edit modes

*lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0 , "ENABLE" , "DISABLE")
lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]) , "ENABLE" , "DISABLE")
*B603397,1 [END]

*B602863,1 Reham On 05/12/1999   *** Begin ***
*B602863,1 Disable the division if entered any styles.
*lcDivStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcDivStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) .AND. lnNoOfLines = 0 , "ENABLE" , "DISABLE")
*B602863,1 Reham On 05/12/1999   *** End   ***

lcCurStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcExRStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) .AND. !EMPTY(laData[14]) .AND. llEditExRt .AND. (laData[14] <> gcBaseCurr) , "ENABLE" , "DISABLE")

*B603397,1 [START] Don't include lnNoOfLines when determining the invoice object status
*                  as it should be always enabled in Add/Edit modes

*lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0  .AND. !EMPTY(laData[10]) , "ENABLE" , "DISABLE")
lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcTmpRetLn) .AND. !EMPTY(laData[10]) , "ENABLE" , "DISABLE")
*B603397,1 [END]

lcRelStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) , "ENABLE" ,"DISABLE")

*-- Refresh the objects in the main screen.
SHOW GET lnTaxRate  &lcObjStat
SHOW GET ibInvoice  &lcInvStat
SHOW GET laData[10] &lcInvStat
SHOW GET ibOrder    &lcRelStat
SHOW GET laData[11] &lcRelStat
SHOW GET laData[12] &lcRelStat
*B605776,1 ASH 05/13/2002 (Begin) Enable the cartons field to let the user change it's numer to be used while printing the stickers.
*SHOW GET laData[13] &lcRelStat
SHOW GET laData[13] &lcInvStat
*B605776,1 ASH 05/13/2002 (End)

SHOW GET lnDiv      &lcDivStat
SHOW GET ibCurrency &lcCurStat
SHOW GET laData[14] &lcCurStat
SHOW GET laData[16] &lcExRStat
SHOW GET puWareHous &lcWarStat
SHOW GET cbComplete &lcEntrStat
SHOW GET pbInqInv   &lcInqStat

llBrowse = .F.
SELECT RETAUTH

*!*************************************************************
*! Name      : lfvData_11
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for the order object.
*!*************************************************************
*! Calls     : gfBrows
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvData_11()
*!*************************************************************
*
FUNCTION lfvData_11

*E301077,4 Reham On 12/17/98   *** Begin ***
*E301077,4 Open the Order header file.
=gfOpenFile(gcDataDir+'OrdHdr',gcDataDir+'OrdAcct','SH')
*E301077,4 Reham On 12/17/98   *** Begin ***

IF (llBrowse .OR. !(laData[11] == lcOldValue))
  laData[11] = IIF(!llBrowse .AND. !EMPTY(laData[11]) .AND. !("?" $ laData[11]) , PADL(ALLTRIM(laData[11]) , 6 , "0") , laData[11])
  SELECT ORDHDR
  SET ORDER TO TAG ORDACCT
  *-- If the entered invalid order # , browse the available orders.
  IF (!EMPTY(laData[11]) .AND. !SEEK(laData[2]+"O"+laData[11] , "ORDHDR")) .OR. llBrowse
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    lcSaveBrow = lcBrFields
    *-- Set the browse fields.
    lcBrFields = [Order:H="Order#",status:1:H="S",Season:H="SE",cDivision:H="DI",]+;
                 [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
                 [ACCOUNT:H="Acct",store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
                 [Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",Ship:H="Ship.Qty.",Shipamt:H="Ship.Amt.",]+;
                 [start:H="Start",Complete:H="Complete"]
    lcOld_ttl   = lcFile_ttl
    lcFile_ttl  = "Orders"
    DECLARE laOrders[1]
    laOrders[1] = laData[11]
    *-- call the global browse to browse the available orders.
    =gfBrows("laData[2]" , "Order" , "laOrders")
    lcBrFields  = lcSaveBrow
    lcFile_ttl  = lcOld_ttl
    laData[11]   = IIF(laData[11] = laOrders[1] , lcOldValue , laOrders[1])
  ENDIF
  SHOW GET laData[11]
ENDIF

llBrowse = .F.

SELECT RETAUTH

*!*************************************************************
*! Name      : lfvData_14
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for currency fields
*!*************************************************************
*! Calls     : gfCurrBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvData_14()
*!*************************************************************
*
FUNCTION lfvData_14

*-- Added condition to prevent user from editing currency 
*-- if the this RA has lines or return by invioce.
IF llBrowse .OR. !(laData[14] == lcOldValue)
  lcCurCode = laData[14]
  *-- Call global function to browse the available currencies.
  IF !gfCurrBrow(@lcCurCode)
    laData[14] = lcOldValue
  ELSE
    laData[14] = lcCurCode
  ENDIF
  
  IF !EMPTY(laData[14])
    laData[16] = gfChkRate('lnCurrUnit' , laData[14] , gdSysDate,;
                         .T. , gcAct_Comp , .F. , llEditExRt)
    laData[15] = lnCurrUnit
    
    IF !(laData[14] == gcBaseCurr)
      IF laData[16] = 0 .AND. llEditExRt
        *** A valid ALLTRIM(laData[14]) to ALLTRIM(gcBaseCurr) ***
        *** exchange rate could not be found on DTOC(gdSysDate) ***
        *** <  Ok  > ***
        lcTmpTxt = ALLTRIM(laData[14]) + "|" + ALLTRIM(gcBaseCurr) + "|" + DTOC(gdSysDate)
        =gfModalGen("INM46000B00000" , "DIALOG" , lcTmpTxt)
      ENDIF
    ELSE
      STORE 1 TO laData[16] , laData[15]
    ENDIF
    *-- Got exchange rate sign and unit sign.
    lcUntSin = ' '
    lcExRSin = gfGetExSin(@lcUntSin, laData[14])
  ENDIF
ELSE
  laData[14] = lcOldValue
ENDIF

lcExRStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EMPTY(laData[10]) .AND. !EMPTY(laData[14]) .AND. llEditExRt .AND. (laData[14] <> gcBaseCurr) , "ENABLE" , "DISABLE")

*-- Refresh the currency & its exchange rate objects.
SHOW GET laData[14]
SHOW GET laData[16] &lcExRStat

llBrowse = .F.

*!*************************************************************
*! Name      : lfwExRate
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : When function for currency exchange rate.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwExRate()
*!*************************************************************
*
FUNCTION lfwExRate

*-- Save the old exchange rate.
lnOldValue = laData[16]

*!*************************************************************
*! Name      : lfvData_16
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for currency exchange rate.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_16()
*!*************************************************************
*
FUNCTION lfvData_16

IF lnOldValue <>  laData[16] .OR. laData[16] <=0
  *-- The exchange rate should not be equal or less than zero.
  IF laData[16] <=0
    *** The exchange rate must be greater than zero. ***
    *** <  Ok  > ***
    =gfModalGen("INM46001B00000" , "DIALOG")
    laData[16] = IIF(lnOldValue > 0 , lnOldValue , laData[16])
    RETURN (IIF(lnOldValue > 0 , .T. , .F.))
  ELSE
    RETURN .T.
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvData_17
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for check bo [ ] Return entire invoice.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvData_17()
*!*************************************************************
*
FUNCTION lfvData_17

*E301077,4 Reham On 12/17/98   *** Begin ***
*E301077,4 Open the invoice line & the consolidated invoice line file.
=gfOpenFile(gcDataDir+'ConsInvL',gcDataDir+'ConsInvL','SH')
=gfOpenFile(gcDataDir+'InvLine',gcDataDir+'InvLine','SH')
*E301077,4 Reham On 12/17/98   *** End   ***

IF !(llOldValue == cbComplete)
  laData[17] = IIF(cbComplete , 'Y' , 'N')
  DO CASE
    *-- If the Returne entire invoice check box was checked.
    CASE cbComplete
      STORE 0 TO lnNoOfLines , _TALLY , laData[25] , laData[28] , laData[31]
      IF llMultiWh .AND. lcConsol = 'Y'
        lnRa_LinNo = 1
        
        SELECT Consinvl
        =SEEK(laData[10]+laData[3])
        SCAN REST WHILE invoice + store = laData[10] + laData[3]
          SELECT (lcTmpRetLn)
          APPEND BLANK
          *-- HDM B603185,1[Start] include discount percentage when
          *--                      calculating the price and total amount
          *REPLACE Account    WITH Consinvl.account ;
                  Style      WITH Consinvl.style ;
                  cRA_LinNo  WITH ALLTRIM(STR(lnRa_LinNo)) ;
                  Dyelot     WITH Consinvl.dyelot ;
                  REASON     WITH laData[18] ;
                  Price      WITH Consinvl.price ;
                  Qty1       WITH Consinvl.qty1 ;
                  Qty2       WITH Consinvl.qty2 ;
                  Qty3       WITH Consinvl.qty3 ;
                  Qty4       WITH Consinvl.qty4 ;
                  Qty5       WITH Consinvl.qty5 ;
                  Qty6       WITH Consinvl.qty6 ;
                  Qty7       WITH Consinvl.qty7 ;
                  Qty8       WITH Consinvl.qty8 ;
                  TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
                  nOpnQty1   WITH Consinvl.qty1 ;
                  nOpnQty2   WITH Consinvl.qty2 ;
                  nOpnQty3   WITH Consinvl.qty3 ;
                  nOpnQty4   WITH Consinvl.qty4 ;
                  nOpnQty5   WITH Consinvl.qty5 ;
                  nOpnQty6   WITH Consinvl.qty6 ;
                  nOpnQty7   WITH Consinvl.qty7 ;
                  nOpnQty8   WITH Consinvl.qty8 ;
                  nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8 ;
                  TAX_Rate   WITH IIF(llTaxYes , lnTaxRate , 0) ;
                  nPstRate   WITH IIF(llTaxYes .AND. llIsCanada , lnPstRate , 0) ;
                  Amount     WITH Consinvl.price * Consinvl.totqty ;
                  CSTATUS    WITH "A"

          REPLACE Account    WITH Consinvl.account ;
                  Style      WITH Consinvl.style ;
                  cRA_LinNo  WITH ALLTRIM(STR(lnRa_LinNo)) ;
                  Dyelot     WITH Consinvl.dyelot ;
                  REASON     WITH laData[18] ;
                  Price      WITH Consinvl.price * (1 - lnDiscPcnt / 100) ;
                  Qty1       WITH Consinvl.qty1 ;
                  Qty2       WITH Consinvl.qty2 ;
                  Qty3       WITH Consinvl.qty3 ;
                  Qty4       WITH Consinvl.qty4 ;
                  Qty5       WITH Consinvl.qty5 ;
                  Qty6       WITH Consinvl.qty6 ;
                  Qty7       WITH Consinvl.qty7 ;
                  Qty8       WITH Consinvl.qty8 ;
                  TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
                  nOpnQty1   WITH Consinvl.qty1 ;
                  nOpnQty2   WITH Consinvl.qty2 ;
                  nOpnQty3   WITH Consinvl.qty3 ;
                  nOpnQty4   WITH Consinvl.qty4 ;
                  nOpnQty5   WITH Consinvl.qty5 ;
                  nOpnQty6   WITH Consinvl.qty6 ;
                  nOpnQty7   WITH Consinvl.qty7 ;
                  nOpnQty8   WITH Consinvl.qty8 ;
                  nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8 ;
                  TAX_Rate   WITH IIF(llTaxYes , lnTaxRate , 0) ;
                  nPstRate   WITH IIF(llTaxYes .AND. llIsCanada , lnPstRate , 0) ;
                  Amount     WITH Consinvl.price * (1 - lnDiscPcnt / 100) * Consinvl.totqty ;
                  CSTATUS    WITH "A"
          *-- HDM B603185,1[End]

          *B602932,1 Reham On 05/26/1999   *** Begin ***
          *B602932,1 Add true in the logical field to know that this line was 
          *B602932,1 shipped to the customer befor.
          REPLACE lShipped  WITH .T.
          *B602932,1 Reham On 05/26/1999   *** End   ***
          
          *-- Call global function to add audit fields info.
          =gfAdd_Info(lcTmpRetLn)

          *-- Total Budget.
          laData[25] = laData[25] + TotQty
          *-- Total Open.
          laData[28] = laData[28] + nTotOpnQty

          laData[31] = laData[31] + (nTotOpnQty * Consinvl.price)
          
          lnRa_LinNo = lnRa_LinNo + 1
          SELECT Consinvl
        ENDSCAN

        *-- HDM B603185,1[Start] include discount percentage & taxes & charges when
        *--                      calculating the total amount
        llIsEnglnd = IIF(UPPER(ALLTRIM(gcContCode)) = 'ENG', .T., .F.)
        IF llIsEnglnd
          lnOther = INVHDR.ncharges
        ELSE
          lnOther = INVHDR.freight + INVHDR.cod + INVHDR.insur
        ENDIF

        lnDiscAmt = laData[31] * (lnDiscPcnt / 100)
        
        lnTaxzamt = (laData[31] - lnDiscAmt + lnOther ) * (lnTaxRate / 100)
        laData[31] = laData[31] + lnOther + lnTaxzamt - lnDiscAmt
        *-- HDM B603185,1[End]




        lnNoOfLines = lnRa_LinNo
        *B802222,1 Reham On 05/05/99   *** Begin ***
        SELECT (lcTmpRetLn)
        GO TOP
        *B802222,1 Reham On 05/05/99   *** End   ***
      ELSE
        lnRa_LinNo = 1
        
        SELECT INVLINE
        =SEEK(laData[10])
        SCAN REST WHILE Invoice = laData[10]
          SELECT (lcTmpRetLn)
          APPEND BLANK

          *-- HDM B603185,1[Start] include discount percentage when
          *--                      calculating the price and total amount
          *REPLACE Account    WITH Invline.account ;
                  Style      WITH Invline.style ;
                  cRA_LinNo  WITH ALLTRIM(STR(lnRa_LinNo)) ;
                  Dyelot     WITH Invline.dyelot ;
                  REASON     WITH laData[18] ;
                  Price      WITH Invline.price;
                  Qty1       WITH Invline.qty1 ;
                  Qty2       WITH Invline.qty2 ;
                  Qty3       WITH Invline.qty3 ;
                  Qty4       WITH Invline.qty4 ;
                  Qty5       WITH Invline.qty5 ;
                  Qty6       WITH Invline.qty6 ;
                  Qty7       WITH Invline.qty7 ;
                  Qty8       WITH Invline.qty8 ;
                  TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
                  nOpnQty1   WITH Invline.qty1 ;
                  nOpnQty2   WITH Invline.qty2 ;
                  nOpnQty3   WITH Invline.qty3 ;
                  nOpnQty4   WITH Invline.qty4 ;
                  nOpnQty5   WITH Invline.qty5 ;
                  nOpnQty6   WITH Invline.qty6 ;
                  nOpnQty7   WITH Invline.qty7 ;
                  nOpnQty8   WITH Invline.qty8 ;
                  nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8 ;
                  TAX_Rate   WITH IIF(llTaxYes , lnTaxRate , 0) ;
                  nPstRate   WITH IIF(llTaxYes .AND. llIsCanada , lnPstRate , 0) ;
                  Amount     WITH Invline.price * Invline.totqty ;
                  CSTATUS    WITH "A"

          REPLACE Account    WITH Invline.account ;
                  Style      WITH Invline.style ;
                  cRA_LinNo  WITH ALLTRIM(STR(lnRa_LinNo)) ;
                  Dyelot     WITH Invline.dyelot ;
                  REASON     WITH laData[18] ;
                  Price      WITH Invline.price  * (1 - lnDiscPcnt / 100) ;
                  Qty1       WITH Invline.qty1 ;
                  Qty2       WITH Invline.qty2 ;
                  Qty3       WITH Invline.qty3 ;
                  Qty4       WITH Invline.qty4 ;
                  Qty5       WITH Invline.qty5 ;
                  Qty6       WITH Invline.qty6 ;
                  Qty7       WITH Invline.qty7 ;
                  Qty8       WITH Invline.qty8 ;
                  TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
                  nOpnQty1   WITH Invline.qty1 ;
                  nOpnQty2   WITH Invline.qty2 ;
                  nOpnQty3   WITH Invline.qty3 ;
                  nOpnQty4   WITH Invline.qty4 ;
                  nOpnQty5   WITH Invline.qty5 ;
                  nOpnQty6   WITH Invline.qty6 ;
                  nOpnQty7   WITH Invline.qty7 ;
                  nOpnQty8   WITH Invline.qty8 ;
                  nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8 ;
                  TAX_Rate   WITH IIF(llTaxYes , lnTaxRate , 0) ;
                  nPstRate   WITH IIF(llTaxYes .AND. llIsCanada , lnPstRate , 0) ;
                  Amount     WITH (Invline.price * (1 - lnDiscPcnt / 100)) * Invline.totqty ;
                  CSTATUS    WITH "A"
          *-- HDM B603185,1[End]

          *B602932,1 Reham On 05/26/1999   *** Begin ***
          *B602932,1 Add true in the logical field to know that this line was 
          *B602932,1 shipped to the customer befor.
          REPLACE lShipped  WITH .T.
          *B602932,1 Reham On 05/26/1999   *** End   ***
          
          *-- Call global function to add audit fields info.
          =gfAdd_Info(lcTmpRetLn)
          *-- Total Budget.
          
          laData[25] = laData[25] + TotQty
          
          *-- Total Open.
          laData[28] = laData[28] + nTotOpnQty
          
          laData[31] = laData[31] + (nTotOpnQty * InvLine.price)
          
          lnRa_LinNo = lnRa_LinNo + 1
          SELECT InvLine
        ENDSCAN
        *-- HDM B603185,1[Start] include discount percentage & taxes & charges when
        *--                      calculating the total amount
        
        llIsEnglnd = IIF(UPPER(ALLTRIM(gcContCode)) = 'ENG', .T., .F.)
        IF llIsEnglnd
          lnOther = INVHDR.ncharges
        ELSE
          lnOther = INVHDR.freight + INVHDR.cod + INVHDR.insur
        ENDIF

        lnDiscAmt = laData[31] * (lnDiscPcnt / 100)
        
        lnTaxzamt = (laData[31] - lnDiscAmt + lnOther ) * (lnTaxRate / 100)
        laData[31] = laData[31] + lnOther + lnTaxzamt - lnDiscAmt

        *-- HDM B603185,1[End]
        
        lnNoOfLines = lnRa_LinNo
        *B802222,1 Reham On 05/05/99   *** Begin ***
        SELECT (lcTmpRetLn)
        GO TOP
        *B802222,1 Reham On 05/05/99   *** End   ***
      ENDIF
    *-- If the Returne entire invoice check box was unchecked.
    CASE !cbComplete
      *-- Blank the temp. R/A lines file.
      SELECT (lcTmpRetLn)
      ZAP
      *-- Set the lines counter to zero.
      lnNoOfLines = 0
  ENDCASE
  
  *-- Update the status variable to refresh the objects.
  *B603397,1 [START] Don't include lnNoOfLines when determining the invoice object status
  *                  as it should be always enabled in Add/Edit modes
  *lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0  .AND. !EMPTY(laData[10]) , "ENABLE" , "DISABLE")
  lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcTmpRetLn) .AND. !EMPTY(laData[10]) , "ENABLE" , "DISABLE")
  *B603397,1 [END]
  
  SHOW GET cbComplete &lcEntrStat
  *B603397,1 [START] Don't include lnNoOfLines when determining the invoice object status
  *                  as it should be always enabled in Add/Edit modes
  *lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0 , "ENABLE" , "DISABLE")
  lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]), "ENABLE" , "DISABLE")
  *B603397,1 [END]
  
  SHOW GET ibInvoice  &lcInvStat
  SHOW GET laData[10] &lcInvStat
  
  SHOW GET laData[25]
  SHOW GET laData[28]
  SHOW GET laData[31]
ENDIF

SELECT RETAUTH

*!*************************************************************
*! Name      : lfvTaxRate
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid func. for tax rate object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvTaxRate()
*!*************************************************************
*
FUNCTION lfvTaxRate
PRIVATE lnCurRecNo

SELECT (lcTmpRetLn)
lnCurRecNo = RECNO()

*-- Update the tax rate in the R/A lines temp. files.
REPLACE ALL TAX_RATE WITH lnTaxRate ;
            CSTATUS  WITH IIF(CSTATUS = 'S','M',CSTATUS)
*-- Restore the lines pointer.
IF lnCurRecNo > 0 .AND. lnCurRecNo <= RECCOUNT()
  GOTO lnCurRecNo
ENDIF

SHOW GET lnTaxRate

SELECT RETAUTH

*!*************************************************************
*! Name      : lfvReson1
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for the reason popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvReson1()
*!*************************************************************
*
FUNCTION lfvReson1

*-- Set the selected reason value.
laData[18] = laReson1[lnReson1,2]

*B603416,1 Reham On 04/27/2000   *** Begin ***
*B603416,1 If not entered the detail folder, appy header reason to all lines reason
IF !llEnterDet AND lnNoOfLines > 0
  SELECT (lcTmpRetLn)
  REPLACE ALL REASON  WITH laData[18] ;
              CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS)
  
  *-- Select the main file.
  SELECT RETAUTH
ENDIF
*B603416,1 Reham On 04/27/2000   *** End   ***

*!*************************************************************
*! Name      : lfvDiv
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid func. for division popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDiv()
*!*************************************************************
*
FUNCTION lfvDiv

*-- Set the selected division value.
laData[19] = laDiv[lnDiv,2]

*!*************************************************************
*! Name      : lfActBrow
*! Developer : Reham Al-Allamy
*! Date      : 02/15/1998
*! Purpose   : Activate the browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfActBrow()
*!*************************************************************
*
FUNCTION lfActBrow

*-- If coming from the browse, Call global function to stop the browse.
IF glFromBrow
  = gfStopBrow()
  *-- Set the browse flag to false.
  glFromBrow = .F.
ENDIF

*-- If the screen browse is not active then clear the trapped keys.
IF !INLIST(WONTOP(), lcBrowttl)
  ON KEY LABEL Alt+N
  ON KEY LABEL Alt+V
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL CTRL+Q
  ON KEY LABEL CTRL+W
ENDIF

*!*************************************************************
*! Name      : lfDeactBrw
*! Developer : Reham Al-Allamy
*! Date      : 02/15/1998
*! Purpose   : Function executed upon entering the browse.
*!*************************************************************
*! Calls     : lpTrapKey , lpTab , lpShTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfDeactBrw()
*!*************************************************************
*
FUNCTION lfDeactBrw

*-- Set the global flag "glFromBrow" to true only the screen browse is active.
glFromBrow = INLIST(WONTOP(), lcBrowttl)

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL Alt+N      DO lpTrapKey WITH lcRtAth2B , "PBNEWLN" , .T.
  ON KEY LABEL Alt+V      DO lpTrapKey WITH lcRtAth2B , "PBREMLN" , .T.
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpShTab
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : Reham Al-Allamy
*! Date      : 02/15/1998
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

*-- Clear the TAB trap.
ON KEY LABEL TAB
DO CASE
  *-- In select & view modes, activate push button <TOP> in the toolbar.
  CASE laScrMode[1] .OR. laScrMode[2]
    ACTIVATE WINDOW gwcContrl1
    _CUROBJ = OBJNUM(pbTop)
  *-- In edit & add mode, activate push button <NEW> in the detail folder.
  CASE laScrMode[3] .OR. laScrMode[4]
    ACTIVATE WINDOW (lcRtAth2B)
    _CUROBJ = OBJNUM(pbNewLn)
ENDCASE
*-- Trap the TAB key.
ON KEY LABEL TAB DO lpTab

*!*************************************************************
*! Name      : lpShTab
*! Developer : Reham Al-Allamy
*! Date      : 02/15/1998
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

*-- Clear the backtab key.
ON KEY LABEL BACKTAB
DO CASE
  *-- In case of select mode, activate the R/A# object in the main screen.
  CASE laScrMode[1]
    ACTIVATE WINDOW (lcRtAth0)
    _CUROBJ = OBJNUM(laData[1])
  *-- In view, activate the <CLOSE> push button in the toolbar.
  CASE laScrMode[2]
    ACTIVATE WINDOW gwcContrl1
    _CUROBJ = OBJNUM(PBCLS)
  *-- In edit or add mode, activate the store field.
  CASE laScrMode[3] .OR. laScrMode[4]
    ACTIVATE WINDOW (lcRtAth0)
    _CUROBJ = OBJNUM(laData[3])
ENDCASE
*-- Trap the BACKTAB key.
ON KEY LABEL BACKTAB DO lpShTab

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Trap of keys
*!*************************************************************
*! Passed Parameters  :  lcWindName : Window name to activate
*!                       lcObjName  : object to focus.
*!                       llExecEntr : Execute enter or not.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTrapKey WITH 'gwcContrl1','pbCls'
*!*************************************************************
*
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName , llExecEntr

IF laScrMode[3] .OR. laScrMode[4]
  ACTIVATE WINDOW (lcWindNAme)
  _CUROBJ = OBJNUM(&lcObjName)
  *-- If enter was send to the trap, send CHR(13) to the buffer.
  IF llExecEntr
    KEYBOARD CHR(13) CLEAR
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfBrLine
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Browse lines function.
*!*************************************************************
*! Calls     : lfWhenBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrLine()
*!*************************************************************
*
FUNCTION lfBrLine

*-- Prepare the alias, key, browse title for the current browse.
lcRCurAlis = IIF(laScrMode[2] , 'RALINE'  , lcTmpRetLn)
lcKeyRet   = IIF(laScrMode[2] , "KEY laData[1]" , "")

*-- Define the browse fields.
*B603713,7 KAM 10/11/2000 change width of price,amount  [START]
*lcBrLines =  "cMarker  =IIF(RECNO(lcRCurAlis)=lnMarker ,'>',' '):H=' ':R:1:W=.F. ,"+;
             "Style :29:R:H='Style',"+;
             "cDesc = IIF(SEEK(Style,'Style'),Style.Desc,''):R:H='Desription':25,"+;
             "lcResDesc = gfCodDes(Reason,'REASON'):20:R:H='Reason',"+;
             "Totqty :8:R:H= 'QTY',"+;
             "Price :9:R:H='Price',"+;
             "Amount :10:R :H='Total'"


lcBrLines =  "cMarker  =IIF(RECNO(lcRCurAlis)=lnMarker ,'>',' '):H=' ':R:1:W=.F. ,"+;
             "Style :29:R:H='Style',"+;
             "cDesc = IIF(SEEK(Style,'Style'),Style.Desc,''):R:H='Desription':25,"+;
             "lcResDesc = gfCodDes(Reason,'REASON'):20:R:H='Reason',"+;
             "Totqty :8:R:H= 'QTY',"+;
             "Price :12:R:H='Price',"+;
             "Amount :13:R :H='Total'"
*B603713,7 KAM [END]



*-- Change the browse pointer.
SELECT (lcRCurAlis)
lnMarker = RECNO(lcRCurAlis)

*-- Release the old browse window.
RELEASE WINDOW (lcBrowttl)

BROWSE FIELDS &lcBrLines   ;
       &lcKeyRet.          ;
       WINDOW (lcRtAth2A)  ;
       IN WINDOW (lcRTAth2);
       LOCK 0              ;
       NOMENU              ;
       NOAPPEND            ;
       NODELETE            ;
       NOEDIT              ;
       NOWAIT              ;
       SAVE                ;
       NOCLEAR             ;
       WHEN lfWhenBrow()   ;
       VALID :F lfvBrows() ;
       TITLE lcBrowttl

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for the Browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvBrows()
*!*************************************************************
*
FUNCTION lfvBrows

*-- Valid function executed inside the browse. to stop the browse if the 
*-- current window not the browse window.

IF !WONTOP(lcBrowttl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfWhenBrow
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : When func. for the browse.
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Parameters: llRefBrow -> If true refresh the browse.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfWhenBrow()
*!*************************************************************
*
FUNCTION lfWhenBrow

*-- Set the browse file according to the mode.
lcRCurAlis = IIF(laScrMode[2] , 'RALINE' , lcTmpRetLn)

*-- Refresh the browse pointer.
SELECT (lcRCurAlis)
lnMarker   = RECNO(lcRCurAlis)

*-- If there is no lines in the browse.
IF BOF() .OR. EOF()
  *-- Store zero to qty. array, & blank its headers.
  laQtyStk = 0
  laSize   = ""
  
  *-- Set the status variables to disable.
  lcLinStat = "DISABLE"
  lcDyeStat = "DISABLE"
  
  *-- Set the needed variables.
  lcStyle    = ""
  lcRa_LinNo = ""
  lcDesc     = ""
  lcReason   = ""
  lcDyelot   = ""
  lnTotQty   = 0
  lnPrice    = 0
  lnTotal    = 0
  lcScale    = ""
  lnScale    = 0
  
  *-- Display N/A in the reason popup.
  =gfwCodePop(@laCodInfo1, "REASON"    , "V,"+lcReason)
  
  *-- Refresh the scale sizes & qty.
  FOR lnCount = 1 TO 8
    SHOW GET laSize  [lnCount] DISABLE
    SHOW GET laQtyStk[lnCount] DISABLE
  ENDFOR
ELSE
  *-- Update the status variables (Enable -or- Disable)
  lcLinStat = IIF(laScrMode[3] .OR. laScrMode[4] , "ENABLE" , "DISABLE")
  lcDyeStat = IIF(llDyelot .AND. SEEK(PADR(lcStyle,19) , "STYLE") .AND. ;
                  STYLE.cdye_flg = 'Y' , "ENABLE" , "DISABLE")
  
  *-- Set the needed variables.
  lcStyle    = Style
  lcRa_LinNo = cRa_LinNo
  lcDesc     = IIF(SEEK(Style,'Style'),Style.Desc,"")
  lcReason  = REASON
  lcDyelot  = Dyelot
  lnTotQty  = Totqty
  lnPrice  = Price
  lnTotal   = Amount
  lcScale   = IIF(!EMPTY(lcStyle) .AND. SEEK(PADR(lcStyle,19) , "STYLE") , Style.Scale , "")
  lnScale   = IIF(!EMPTY(lcScale) .AND. SEEK("S" + lcScale , "SCALE") , SCALE.Cnt , 0)
  
  *-- Fill the reason popup with its data.
  =gfwCodePop(@laCodInfo1, "REASON"    , "V,"+lcReason)
  *-- Refresh the scale sizes & qty.
  FOR lnCount = 1 TO 8
    lcCount   = STR(lnCount,1)
    IF lnCount <= lnScale
      laQtyStk[lnCount] = Qty&lcCount
      laSize  [lnCount] = IIF(lnScale = 0 , "" , SCALE.SZ&lcCount)
      SHOW GET laSize  [lnCount] &lcLinStat
      SHOW GET laQtyStk[lnCount] &lcLinStat
    ELSE
      laQtyStk[lnCount] = 0
      laSize  [lnCount] = ""
      SHOW GET laSize  [lnCount] DISABLE
      SHOW GET laQtyStk[lnCount] DISABLE
    ENDIF

    *B606357,1 (Begin) In case modifying the style, and the scale of the new style has sizese
    *B606357,1         less than the old one, recalculate qtys and amount.
    IF !laScrMode[2]       
      REPLACE QTY&lcCount     WITH laQtyStk[lnCount] ;
              Totqty          WITH laQtyStk[1]+laQtyStk[2]+laQtyStk[3]+laQtyStk[4]+laQtyStk[5]+laQtyStk[6]+laQtyStk[7]+laQtyStk[8] ;
              Amount          WITH TOTQTY * PRICE ;
              nOpnQty&lcCount WITH IIF(lnCount <= lnScale,nOpnQty&lcCount,0);
              nTotOpnQty      WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8 ;
              CSTATUS         WITH IIF(CSTATUS = 'S','M',CSTATUS)
              
    ENDIF
    *B606357,1 (End)
  ENDFOR
ENDIF
*-- Set the new button status.
lcNewStat = IIF(laScrMode[3] .OR. laScrMode[4] , "ENABLE", "DISABLE") 

*-- Refresh the objects in screen : (lcRtAth2B)
SHOW GET ibStyle   &lcLinStat
SHOW GET lcStyle   &lcLinStat
SHOW GET lcDesc    &lcLinStat
SHOW GET lnReson2  &lcLinStat
SHOW GET lcDyeLot  &lcDyeStat
SHOW GET lnTotQty  &lcLinStat
SHOW GET lnPrice   &lcLinStat
SHOW GET lnTotal   &lcLinStat
SHOW GET pbRemLn   &lcLinStat
SHOW GET pbNewLn   &lcNewStat
*-- Refresh the lines browse.
SHOW WINDOW (lcBrowttl) REFRESH

*!*************************************************************
*! Name      : lfvNewLine
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid funcion for the new button.
*!*************************************************************
*! Calls     : gfwCodePop
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNewLine()
*!*************************************************************
*
FUNCTION lfvNewLine

llAddLine = .T.

*-- Fill the detail reason popup with the right value.
lcReason = laData[18]
=gfwCodePop(@laCodInfo1, "REASON"    , "V,"+lcReason)

*-- Add new line in the temp. R/A lines file.
SELECT (lcTmpRetLn)
LOCATE FOR EMPTY(STYLE)
IF !FOUND()
  APPEND BLANK
ENDIF
BLANK
REPLACE RaNo    WITH IIF(laScrMode[3] , laData[1] , SPACE(6)) ;
        Account WITH laData[2] ;
        Reason  WITH lcReason ;
        CSTATUS WITH 'A'

*-- Call global function to add audit fields info.
=gfAdd_Info(lcTmpRetLn)

*-- Add 1 to the lines records counter.
lnNoOfLines = lnNoOfLines + 1

*-- Refresh the detail folder.
=lfWhenBrow()
_CUROBJ = OBJNUM(lcStyle)

*!*************************************************************
*! Name      : lfvRemLine
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for the remove button.
*!*************************************************************
*! Calls     : gfModalGen, lfWhenBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRemLine()
*!*************************************************************
*
FUNCTION lfvRemLine

*-- Confirm Removing current line.
*** Are you sure you want to "Remove" this record? ***
*** < Yes > - < No > ***
IF gfModalGen("QRM00002B00006","ALERT") = 1
  
  *-- Decrease the lines with 1.
  lnNoOfLines = lnNoOfLines - 1
  
  *-- Get the original Qty. & open qty. from the RALINE file if not new record.
  IF &lcTmpRetLn..cStatus = "A"
    lnDeduct   = 0
  ELSE
    IF SEEK(laData[1] + PADR(&lcTmpRetLn..Style,19) , 'RALINE')
      lnOldRAOpn = RALINE.nTotOpnQty
      lnCurRAOpn = MAX(0 , &lcTmpRetLn..nTotOpnQty + &lcTmpRetLn..TotQty - RALINE.TotQty)
      lnDeduct   = IIF(&lcTmpRetLn..cStatus = "S" , lnOldRAOpn , ;
                       IIF(lnCurRAOpn < lnOldRAOpn , ;
                           lnOldRAOpn - lnCurRAOpn , 0))
    ELSE
      lnDeduct   = 0
    ENDIF
  ENDIF
  
  *-- Total Canceled Qty.
  laData[27] = laData[27] + lnDeduct
  *-- Total Open Qty.
  laData[28] = laData[28] - &lcTmpRetLn..TotQty
  *-- Total budget qty.
  laData[25] = laData[27] + laData[28]
  *-- Total Open Amount.
  laData[31] = laData[31] - (&lcTmpRetLn..TotQty * &lcTmpRetLn..Price)
  
  *-- Delete the current line.
  SELECT (lcTmpRetLn)
  REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
  DELETE
  GO TOP
  
  *-- If there is no lines in the R/A temp. lines file.
  IF lnNoOfLines = 0
    
    *B603397,1 [START] Don't include lnNoOfLines when determining the invoice object status
    *                  as it should be always enabled in Add/Edit modes
    *lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0  .AND. !EMPTY(laData[10]) , "ENABLE" , "DISABLE")
    lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcTmpRetLn) .AND. !EMPTY(laData[10]) , "ENABLE" , "DISABLE")
    *B603397,1 [END]
    
    SHOW GET cbComplete &lcEntrStat
    
    *B603397,1 [START] Don't include lnNoOfLines when determining the invoice object status
    *                  as it should be always enabled in Add/Edit modes
    *lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0 , "ENABLE" , "DISABLE")
    lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]), "ENABLE" , "DISABLE")
    *B603397,1 [END]
    
    SHOW GET ibInvoice  &lcInvStat
    SHOW GET laData[10] &lcInvStat
  ENDIF
  
  *-- Refresh the updated objects.
  SHOW GET laData[25]
  SHOW GET laData[27]
  SHOW GET laData[28]
  SHOW GET laData[31]
  
  *-- Call the when browse function.
  =lfWhenBrow()
ENDIF

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid func. for the style object.
*!*************************************************************
*! Calls     : gfStyBrw, lfBrLine, gfModalGen, lfStyCan
*!           : gpAdStyWar, lfWhenBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvStyle()
*!*************************************************************
*
FUNCTION lfvStyle

*-- If do not change the style & do not press the browse icon.
IF (lcOldValue = lcStyle .OR. EMPTY(lcStyle)) .AND. !llBrowse
  RETURN
ENDIF

SELECT STYLE

IF llBrowse .OR. !SEEK(PADR(lcStyle,19) , 'STYLE')
  *B602771,1 Reham On 04/11/99   *** Begin ***
  *B602771,1 Enable the soft seek in the style browse.
  *-- Call the global style browse.
  *lcStyle = gfStyBrw("I" , "" , "" , .F.)
  lcStyle = PADR(gfStyBrw("I" , lcStyle , "" , .F.),19)
  *B602771,1 Reham On 04/11/99   *** End   ***
  SELECT (lcTmpRetLn)
  *-- If the browse window was released, call the browse function.
  IF !WEXIST(lcBrowttl)
    =lfBrLine()
  ENDIF
ENDIF

llBrowse = .F.

*-- If enter valid style.
IF !EMPTY(lcStyle) .AND. SEEK(PADR(lcStyle,19) , "STYLE")
  *-- Get the selected style discount code..
  lcDiscCode = Style.cDiscCode
  
  *-- If the style division different from the header division, reenter the style.
  IF STYLE.cDivision <> laData[19]
    *** Style / Division confilct! ***
    *** <  Ok  > ***
    =gfModalGen("INM46003B00000" , "DIALOG")
    =lfStyCan()
    RETURN
 *B604218,1 KAM 10/03/2001 [start]
  *--program enter this path if style.cdivision equal ladata[19]
  *-- we put endif instead of else to allow the program alwayse enter
  *ELSE
  ENDIF &&STYLE.cDivision <> laData[19]
 *B604218,1 KAM  [end]
    *-- Check if the entered style assigned to the header warehouse.
    IF !SEEK(PADR(lcStyle,19) + laData[7] + SPACE(10) , 'StyDye')
      *** Style: ALLTRIM(lcStyle) is not assigned ***
      *** to warehouse: ALLTRIM(laData[7])   ***
      *** <  Add  > - < Reenter > ***
      lcTmpTxt = ALLTRIM(lcStyle) + "|" + ALLTRIM(laData[7])
      IF gfModalGen("QRM46004B00031" , "DIALOG" , lcTmpTxt) = 1
        *-- Call global function to assign the warehouse.
        DO gpAdStyWar WITH PADR(lcStyle,19) , SPACE(10) , laData[7]
      ELSE
        *-- Reenter the style.
        =lfStyCan()
        RETURN
      ENDIF
    ENDIF &&!SEEK(PADR(lcStyle,19) + laData[7] + SPACE(10) , 'StyDye')
    *-- Get the style scale.
    =SEEK(PADR(lcStyle,19) ,'Style')
    IF SEEK('S' + STYLE.SCALE , 'SCALE')
      lnScale     = SCALE.CNT
      FOR lnCount = 1 TO 8
        lcCount = ALLTRIM(STR(lnCount))
        laSize[lnCount] = SCALE.SZ&lcCount
        IF lnCount <= lnScale
          SHOW GET laQtyStk[lnCount] ENABLE
        ELSE
          SHOW GET laQtyStk[lnCount] DISABLE
        ENDIF
      ENDFOR
    ENDIF && SEEK('S' + STYLE.SCALE , 'SCALE')
    *-- Get the customer price level.
    lcCustPLvl = IIF(LEN(ALLTRIM(lcCustPLvl))=0,'A',lcCustPLvl)
    
    *B602771,1 Reham On 04/11/99     *** Begin ***
    *lnPrice    = STYLE.PRICE&lcCustPLvl
    *IF llMulCurr 
    *  IF &lcTmpRetLn..cStatus = 'A'
    *    lnLastPrice = gfStyPrice(PADR(lcStyle,19) , lcCustPLvl , laData[14])
    *    lnPrice     = IIF(lnLastPrice = -1 , 0 , lnLastPrice)
    *  ELSE
    *    IF SEEK(laData[1] + PADR(lcStyle,19) , 'RALINE')
    *      lnPrice = RALine.Price
    *    ELSE
    *      lnPrice = 0 
    *    ENDIF
    *  ENDIF
    *ENDIF
    
    *B602932,1 Reham On 05/26/1999   *** Begin ***
    *B602932,1 Move this line after checking if this style was shipped to the customer or not.
    *lnPrice = lfGetprice(lcStyle,lcCustPLvl,lnTotQty)
    *B602771,1 Reham On 04/11/99     *** End   ***
    *B602932,1 Reham On 05/26/1999   *** End   ***
    
    lcDyelot  = SPACE(10)
    llRetry   = .F.
    llShipped = .F.
    *-- Get the tax rate from the invoice header file.
    IF !EMPTY(laData[10])
      =SEEK(laData[10] , "INVHDR")
      SELECT INVLINE
      SET ORDER TO INVLINES
      
      *B608481,1 NNA 03/15/2008 (Begin) Correct the next Seek because ladata[7] is for the warehouse and we have to seek
      *B608481,1 NNA            by style and invoice so I changed ladata[7] to ladata[10]
      *IF SEEK(PADR(lcStyle,19) + laData[7] , "INVLINE")
      IF SEEK(PADR(lcStyle,19) + laData[10] , "INVLINE")
      *B608481,1 NNA (End)
      
        lnPrice = InvLine.PRICE * (1-lnDiscPCnt/100)
        IF llDyelot
          lcDyelot = InvLine.Dyelot
        ENDIF
        llShipped  = .T.
      ELSE
        llRetry    = .T.
      ENDIF
    ELSE && !EMPTY(laData[10])
      *-- If no invoice was entered then search the lines for price.
      llRetry = .T.
    ENDIF && !EMPTY(laData[10])
    
    *-- Search for the style in the invoice header file if there is no invoice #.
    IF llRetry
      SELECT INVLINE
      SET ORDER TO INVLINES
      SELECT INVHDR
      *-- Make invhdra the active index tag
      SET ORDER TO TAG INVHDRA DESCENDING
      =SEEK(laData[2] , "INVHDR")
      *-- Searching for the last invoice this style was shipped on using
      *-- the same current currency.
      WAIT WINDOW 'Searching for the last invoice this style was shipped on ....' NOWAIT
      
      *B602863,1 Reham On 05/24/1999  *** Begin ***
      *B602863,1 Change the scan condition to find the related styles.
      *E301090,1 Reham On 13/12/98    *** Begin ***
      *E301090,1 Do not accept any voided invoices...
      *SCAN WHILE ACCOUNT = laData[2]
      *SCAN WHILE ACCOUNT = laData[2] AND Status <> 'V'
      SCAN REST WHILE ACCOUNT = laData[2] FOR Status <> 'V'
      *E301090,1 Reham On 13/12/98    *** End   ***
      *B602863,1 Reham On 05/24/1999  *** End   ***
        
        *B602771,1 Reham On 04/13/99   *** Begin ***
        *IF INVHDR.cCurrCode = laData[14]
        IF (llMulCurr .AND. (INVHDR.cCurrCode = laData[14])) .OR. !llMulCurr
        *B602771,1 Reham On 04/13/99   *** End   ***
          lcSrchInv = INVHDR.INVOICE
          SELECT INVLINE
          IF SEEK(PADR(lcStyle,19) + lcSrchInv , "INVLINE")
            lnPrice   = PRICE * (1-INVHDR.DISCPCNT/100)
            llShipped = .T.
            *lntaxrate = IIF(llTaxYes , INVHDR.TAX_RATE , 0)
            IF llDyelot
              lcDyelot = IIF(InvHdr.cWareCode=laData[7],InvLine.Dyelot,SPACE(10))
            ENDIF
            *B602863,1 Reham On 05/25/1999   *** Begin ***
            *B602863,1 Exit for the scan to take the latest invoice price.
            EXIT
            *B602863,1 Reham On 05/25/1999   *** End   ***
          ENDIF &&SEEK(PADR(lcStyle,19) + lcSrchInv , "INVLINE")
        ENDIF &&(llMulCurr .AND. (INVHDR.cCurrCode = laData[14])) .OR. !llMulCurr
        SELECT INVHDR
      ENDSCAN
      WAIT CLEAR
      SELECT INVLINE
      SET ORDER TO INVLINE
      SELECT INVHDR
      SET ORDER TO TAG INVHDR
 
    ENDIF && llRetry
 
    IF !llShipped
      *** This style was not shipped to this customer! continue? ***
      *** <  Yes  > - <  No  > ***
      IF gfModalGen("QRM46005B00006" , "DIALOG") = 2
        =lfStyCan()
        RETURN
      ENDIF
      lnPstRate = lnCusPstRt
    ENDIF && !llShipped
*B604218,1 KAM 10/03/2001 [start]
*--theis endif for STYLE.cDivision <> laData[19] we stop it
*  ENDIF  &&STYLE.cDivision <> laData[19]
*B604218,1 KAM [end]
  *B602863,1 Reham On 05/25/1999   *** Begin ***
  *B602863,1 Do not use the discount price if the price from latest invoice.
  IF !llShipped
  *B602863,1 Reham On 05/25/1999   *** End   ***
    
    *B602932,1 Reham On 05/26/1999   *** Begin ***
    *B602932,1 Move this line after checking if this style was shipped to the customer or not.
    lnPrice = lfGetprice(lcStyle,lcCustPLvl,lnTotQty)
    *B602932,1 Reham On 05/26/1999   *** End   ***
    
    *-- If the discount code for the selected style not empty.
    IF !EMPTY(lcDiscCode)
      *-- Get the discount percentage for this discount code from the codes file.
      DECLARE laStyDisc[1,2]
      laStyDisc[1,1] = 'DISCPCNT'
      laStyDisc[1,2] = 'lnDiscPcnt'
      =gfRltFld(lcDiscCode , @laStyDisc , "CDISCCODE")
      *-- If the discount percentage greater than zero...
      IF lnDiscPcnt > 0
        *B602863,1 Reham On 05/24/1999   *** Begin ***
        *lcTmpStr = ALLTRIM(STR(lnDiscPcnt)) +"|" + ALLTRIM(STR(lnPrice)) + ;
                   "|" + ALLTRIM(STR(lnPrice * lnDiscPcnt /100)) + "|" + ;
                   ALLTRIM(STR(lnPrice - (lnPrice * lnDiscPcnt / 100)))
        lcTmpStr = ALLTRIM(STR(lnDiscPcnt)) +"|" + ALLTRIM(STR(lnPrice,10,3)) + ;
                   "|" + ALLTRIM(STR(lnPrice * lnDiscPcnt /100,10,3)) + "|" + ;
                   ALLTRIM(STR(lnPrice - (lnPrice * lnDiscPcnt / 100),10,3))
        *B602863,1 Reham On 05/24/1999   *** End   ***
        *** There is discount percentage on this style : {lnDiscPcnt} ***
        *** Gross Price is : {lnPrice}
        *** Discount amount is : {lnPrice * lnDiscPcnt /100}
        *** Net Price is : {lnPrice - (lnPrice * lnDiscPcnt / 100)}
        =gfModalGen("INM46030B00000" , "DIALOG" , lcTmpStr)
        
        *-- Get a discount on the total gross price, & inform the user.
        lnPrice = lnPrice - (lnPrice * lnDiscPcnt / 100)
      ENDIF && lnDiscPcnt > 0
    ENDIF && !EMPTY(lcDiscCode)
    *B603397,1 HDM Comment these line as B603277,1 still in tesing
    **B603277,1 Ramy add this line to recalculate the customer discount if 
    **               there is [START]
    *lnDiscPcnt = CUSTOMER.Disc
    *lnPrice = lnPrice - (lnPrice * lnDiscPcnt / 100)
    **B603277,1 Ramy [END]
    *B603397,1 HDM[end]
  *B602863,1 Reham On 05/25/1999    *** Begin ***
  ENDIF && !llShipped
  *B602863,1 Reham On 05/25/1999    *** End   ***
  *B606357,1 (Begin) Adjust Open and Open Amount for case of changing the style.
  IF !laScrMode[2]       
    *-- Total Open Qty.
    laData[28] = laData[28] - &lcTmpRetLn..TotQty
    *-- Total Open Amount.
    laData[31] = laData[31] - (&lcTmpRetLn..TotQty * &lcTmpRetLn..Price)
  ENDIF  
  *B606357,1 (End)
  
  *-- Add line into the R/A temp. file. 
  SELECT (lcTmpRetLn)
  REPLACE STYLE    WITH PADR(lcStyle,19) ;
          PRICE    WITH lnPrice ;
          REASON   WITH laData[18] ;
          TAX_RATE WITH IIF(llTaxYes , lnTaxRate , 0) ;
          nPstRate WITH IIF(llTaxYes .AND. llIsCanada , lnPstRate , 0) ;
          Dyelot   WITH lcDyelot ;
          CSTATUS  WITH IIF(CSTATUS = 'S','M',CSTATUS)
          
  *B602932,1 Reham On 05/26/1999   *** Begin ***
  *B602932,1 Add true in the logical field if this style was shipped to this customer befor.
  REPLACE lShipped  WITH llShipped
  *B602932,1 Reham On 05/26/1999   *** End   ***
ELSE && !EMPTY(lcStyle) .AND. SEEK(PADR(lcStyle,19) , "STYLE")
  *-- Reenter the style.
  =lfStyCan()
  RETURN
ENDIF && !EMPTY(lcStyle) .AND. SEEK(PADR(lcStyle,19) , "STYLE")

SELECT (lcTmpRetLn)
SHOW GET lcStyle

*-- Call the when browse function.
=lfWhenBrow()
*B606357,1 (Begin) Adjust Open and Open Amount for case of changing the style.
IF !laScrMode[2]       
  *-- Total Open Qty.
  laData[28] = laData[28] + &lcTmpRetLn..TotQty
  *-- Total Open Amount.
  laData[31] = laData[31] + (&lcTmpRetLn..TotQty * &lcTmpRetLn..Price)
  *-- Total budget Qty.
  laData[25] = laData[27] + laData[28]
  lnTotQty  = Totqty
  lnTotal   = Amount
  SHOW GET laData[25]    
  SHOW GET laData[28]
  SHOW GET laData[31]
  SHOW GET lnTotQty  &lcLinStat
  SHOW GET lnTotal   &lcLinStat
ENDIF
*B606357,1 (End)


*!*************************************************************
*! Name      : lfStyCan
*! Developer : Reham Al-Allamy
*! Date      : 09/09/1997
*! Purpose   : Function to cancel adding a style.
*!*************************************************************
*! Calls     : lfWhenBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfStyCan()
*!*************************************************************
*
FUNCTION lfStyCan

SELECT (lcTmpRetLn)
IF &lcTmpRetLn..CSTATUS = 'A'
  *-- If the record was new, change the status to be "S->Same"
  REPLACE CSTATUS WITH 'S'
  *-- Delete the current record.
  DELETE
  *-- Go to the first record in the R/A temp. file.
  GO TOP
  lcStyle     = &lcTmpRetLn..STYLE
  *-- Subtract 1 from the records counter.
  lnNoOfLines = lnNoOfLines - 1
  IF lnNoOfLines = 0
    *B603397,1 [START] Don't include lnNoOfLines when determining the invoice object status
    *                  as it should be always enabled in Add/Edit modes

    *lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0  .AND. !EMPTY(laData[10]) , "ENABLE" , "DISABLE")
    lcEntrStat= IIF((laScrMode[3] .OR. laScrMode[4]) .AND. EOF(lcTmpRetLn) .AND. !EMPTY(laData[10]) , "ENABLE" , "DISABLE")

    *lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnNoOfLines = 0 , "ENABLE" , "DISABLE")
    lcInvStat = IIF((laScrMode[3] .OR. laScrMode[4]) , "ENABLE" , "DISABLE")
    *B603397,1 [END]
  ENDIF
ELSE
  lcStyle = &lcTmpRetLn..STYLE
ENDIF
SHOW GET lcStyle

*-- Call the when browse function.
=lfWhenBrow()

*!*************************************************************
*! Name      : lfvReson2
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for the line reason.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvReson2()
*!*************************************************************
*
FUNCTION lfvReson2

*-- Select the detail reason.
lcReason = laReson2[lnReson2,2]

*-- Update the R/A lines temp. file.
SELECT (lcTmpRetLn)
REPLACE REASON  WITH lcReason ;
        CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS)
*-- Refresh the lines browse.
SHOW WINDOW (lcBrowttl) REFRESH

*!*************************************************************
*! Name      : lfvTotQty
*! Developer : Reham Al-Allamy
*! Date      : 07/13/1996
*! Purpose   : Valid function for the total Qty. object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvTotQty()
*!*************************************************************
*
FUNCTION lfvTotQty

*-- Update the amount in the R/A lines temp. files.
SELECT (lcTmpRetLn)
REPLACE Amount  WITH lnTotqty * Price ;
        CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS)

lnTotal  = Amount
SHOW GET lnTotal
*-- Refresh the browse lines.
SHOW WINDOW (lcBrowttl) REFRESH

*!*************************************************************
*! Name      : lfvPrice
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for the price object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvPrice()
*!*************************************************************
*
FUNCTION lfvPrice

*B802213,1 Reham On 05/03/99   *** Begin ***
*B802213,1 Update the open amounts in the header folder if change the price.
*-- Total Open Amount.
laData[31] = laData[31] - (&lcTmpRetLn..TotQty * &lcTmpRetLn..Price)
*B802213,1 Reham On 05/03/99   *** End   ***

*-- Update the price & the amount in the R/A lines temp. files.
SELECT (lcTmpRetLn)
REPLACE Price   WITH lnPrice ;
        Amount  WITH Totqty * Price ;
        CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS)

*B802213,1 Reham On 05/03/99   *** Begin ***
*B802213,1 Update the open amounts in the header folder if change the price.
*-- Total Open Amount.
laData[31] = laData[31] + (&lcTmpRetLn..TotQty * &lcTmpRetLn..Price)
SHOW GET laData[31]
*B802213,1 Reham On 05/03/99   *** End   ***

*-- Refresh the objects in the detail screen.
lnPrice  = Price
lnTotal  = Amount
SHOW GET lnPrice
SHOW GET lnTotal
*-- Refresh the browse lines.
SHOW WINDOW (lcBrowttl) REFRESH

*!*************************************************************
*! Name      : lfvQty
*! Developer : Reham Al-Allamy
*! Date      : 07/10/1996
*! Purpose   : Valid function for the qty. in all the sizes.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lnFrom -> No. to know the qty. no.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvQty()
*!*************************************************************
*
FUNCTION lfvQty
PARAMETERS lnFrom

lcFrom = ALLTRIM(STR(lnFrom))

IF &lcTmpRetLn..QTY&lcFrom <> laQtyStk[lnFrom]

  *-- Total Open Qty.
  laData[28] = laData[28] - &lcTmpRetLn..TotQty
  *-- Total Open Amount.
  laData[31] = laData[31] - (&lcTmpRetLn..TotQty * &lcTmpRetLn..Price)
  
  IF SEEK(laData[1] + PADR(&lcTmpRetLn..Style,19) , 'RALINE') AND &lcTmpRetLn..cStatus <> "A"
    lnOldRAOpn = RALINE.nTotOpnQty - &lcTmpRetLn..TotQty
  ENDIF
  
  *-- Update the qty., total qty. and amount in the R/A lines temp. files.
  SELECT (lcTmpRetLn)
  REPLACE QTY&lcFrom WITH laQtyStk[lnFrom] ;
          Totqty     WITH laQtyStk[1]+laQtyStk[2]+laQtyStk[3]+laQtyStk[4]+laQtyStk[5]+laQtyStk[6]+laQtyStk[7]+laQtyStk[8] ;
          Amount     WITH TOTQTY * PRICE ;
          CSTATUS    WITH IIF(CSTATUS = 'S','M',CSTATUS)
  
  *-- If this record is added new in this session, Default the open qty. with 
  *-- the entered qty.
  IF &lcTmpRetLn..cStatus = "A"
    REPLACE nOpnQty&lcFrom WITH laQtyStk[lnFrom] ;
            nTotOpnQty     WITH laQtyStk[1]+laQtyStk[2]+laQtyStk[3]+laQtyStk[4]+laQtyStk[5]+laQtyStk[6]+laQtyStk[7]+laQtyStk[8]
  ENDIF
  
  *-- Get the original Qty. & open qty. from the RALINE file if not new record.
  IF &lcTmpRetLn..cStatus = "A"
    lnOldRAOpn = 0
    lnCurRAOpn = 0
  ELSE
    IF SEEK(laData[1] + PADR(&lcTmpRetLn..Style,19) , 'RALINE')
      lnCurRAOpn = MAX(0 , RALINE.nTotOpnQty - &lcTmpRetLn..TotQty)
    ELSE
      lnOldRAOpn = 0
      lnCurRAOpn = 0
    ENDIF
  ENDIF
  
  *B602932,1 Reham On 05/26/1999   *** Begin ***
  *B602932,1 Check if this style was shipped to this customer before, do not change the price.
  *B602863,1 Reham On 05/12/99   *** Begin ***
  *B602863,1 Validate the at Qty. price only if not from invoice.
  *SET ORDER TO Invlines IN InvLine
  *IF !EMPTY(laData[10]) .AND. laData[17] = "Y" .AND. SEEK(&lcTmpRetLn..Style+laData[10] , "InvLine")
  *ELSE
  *B602863,1 Reham On 05/12/99   *** End   ***
  *B602932,1 Check if this style was shipped to this customer before.
  IF !&lcTmpRetLn..lShipped
  *B602932,1 Reham On 05/26/1999   *** End   ***
    *B602771,1 Reham On 03/08/99   *** Begin ***
    *B602771,1 Get the price level per qty.
    lcCustPLvl = IIF(EMPTY(CUSTOMER.PRICELVL) , "A" , CUSTOMER.PRICELVL)
    IF lcCustPLvl = "Q"
      lnCurPrice = lfGetprice(lcStyle,lcCustPLvl,&lcTmpRetLn..TOTQTY)
      
      *B602771,1 Calculate the discount for the current style price.
      lnCurPrice = lfCalcDisc(lcStyle , lnCurPrice)
      
      *B602771,1 Update the price.
      SELECT (lcTmpRetLn)
      REPLACE Price   WITH lnCurPrice ;
              Amount  WITH Totqty * Price
    ENDIF
    lnPrice = Price
    SHOW GET lnPrice
    *B602771,1 Reham On 03/08/99   *** End   ***
  *B602863,1 Reham On 05/12/99   *** Begin ***
  ENDIF
  SET ORDER TO Invline IN InvLine
  *B602863,1 Reham On 05/12/99   *** End   ***
  
  *-- Total Canceled Qty.
  *laData[27] = laData[27] + IIF(&lcTmpRetLn..cStatus <> "A" .AND. ;
                lnCurRAOpn < lnOldRAOpn , lnOldRAOpn - lnCurRAOpn , 0)
  laData[27] = laData[27] - lnOldRAOpn + lnCurRAOpn
  
  *-- Total Open Qty.
  laData[28] = laData[28] + &lcTmpRetLn..TotQty
  
  *-- Total budget Qty.
  laData[25] = laData[27] + laData[28]
  
  *-- Total Open Amount.
  laData[31] = laData[31] + (&lcTmpRetLn..TotQty * &lcTmpRetLn..Price)
  
  lnTotQty = Totqty
  lnTotal  = Amount
  
  *-- Refresh the updated objects in the detail screen.
  SHOW GET laQtyStk[lnFrom]
  SHOW GET lnTotQty
  SHOW GET lnTotal
  SHOW GET laData[25]
  SHOW GET laData[27]
  SHOW GET laData[28]
  SHOW GET laData[31]
  
  *-- Refresh the browse lines.
  SHOW WINDOW (lcBrowttl) REFRESH
ENDIF
*B602783,1 Reham On 05/03/99   *** Begin ***
*B602783,1 Force to push button new if last size.
IF lnFrom = lnScale
  _CUROBJ = OBJNUM(pbNewLn)
ENDIF
*B602783,1 Reham On 05/03/99   *** End   ***

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Valid function for the dyelot object.
*!*************************************************************
*! Calls     : SDYEBROW, gfModalGen, lfStyCan
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDyelot()
*!*************************************************************
*
FUNCTION lfvDyelot

*-- If entered new dyelot.
IF !EMPTY(lcDyelot) .AND. lcOldValue <> lcDyelot .AND. LASTKEY() = 13
  *-- If entered invalid dyelot, browse the available dyelots in the system.
  IF !SEEK(PADR(lcStyle,19) + laData[7] + lcDyelot , 'StyDye') .OR. "?" $ lcDyelot
    lcDyelot = ''
    IF !SDYEBROW(PADR(lcStyle,19) , @lcDyelot , .T. , laData[29])
      IF EMPTY(lcDyelot)
        *** The dyelot cannot be empty.  This line will be removed. ***
        *** <  OK  > ***
        =gfModalGen("INM46007B00000" , "DIALOG")
        =lfStyCan()
        RETURN
      ENDIF
    ENDIF
  ENDIF
  *-- Update the dyelot in the R/A lines temp. files.
  SELECT (lcTmpRetLn)
  REPLACE DYELOT  WITH lcDyelot ;
          CSTATUS WITH IIF(CSTATUS = 'S','M',CSTATUS)
  SHOW GET lcDyelot
ENDIF

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Delete the current R/A
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpDelScr()
*!*************************************************************
*
FUNCTION lpDelScr

*E301077,4 Reham On 12/29/98   *** Begin ***
*E301077,4 Open the style dyelot file.
=gfOpenFile(gcDataDir+'StyDye',gcDataDir+'StyDye','SH')
*E301077,4 Reham On 12/29/98   *** End   ***

SELECT RETAUTH
*E301643,1 MHM Check for status 'Electronic' And cancelled IF come from CRM module , 
*               check for not EMPTY allneeded filed in updated file CRMESG [Start]
IF (llCrmIns AND RETAUTH.lFromWeb) .AND.  laData[4] $ "EX"
  lcMto       = IIF(SEEK('M'+RetAuth.Account,'Customer'),EVALUATE('Customer.' + gfGetMemVar('M_CONFMAIL')),'')
  lCLetterid  = gfGetMemVar('M_RAAPR' , gcAct_Comp)

  IF EMPTY(lcMto)
    *The Confirmation E-mail Address on the setup screen is blank. 
    *<OK>
    =gfModalGen('TRM46039B00000','DIALOG')
    RETURN
  ENDIF  

  DO CASE
    *--mhm2000
    CASE laData[4] = "E"
      *IF EMPTY(lCLetterid)
      *  *The Approved ID on the setup screen is blank. 
      *  *<OK>
      *  =gfModalGen('QRM46040B00000','DIALOG')
      *  RETURN
      *ENDIF  
    *CASE   laData[4] = "X"
      IF EMPTY(lCLetterid)
        *The Cancelled ID on the setup screen is blank. Are you sure you want to continue?
        *<OK>
        IF gfModalGen('QRM46041B32000','DIALOG') = 2
          RETURN
        ENDIF
      ENDIF  
  ENDCASE
ENDIF  
*E301643,1 MHM [End]

DO CASE
  *-- You caccnot cancel a completed R/A.
  CASE laData[4] = "C"
    *** R/A has been received.  Cannot cancel it! ***
    *** < Ok > ***
    =gfModalGen("INM46019B00000" , "DIALOG")
    RETURN
  *-- If the R/A status is opened.
  CASE laData[4]  = "O"
    *** There is a partially receive had happened for the ***
    *** current R/A.  Do you wish to cancel the rest of ***
    *** the current R/A? ***
    *** < Yes > - < No > ***
    IF (laData[26] = 0) .OR. (laData[26] > 0 .AND. gfModalGen("INM46021B00006" , "DIALOG") = 1)
      lnTotalCan = 0
      SELECT RALINE
      =SEEK(laData[1])
      SCAN FOR RANO = laData[1]
        *-- Consider the open qty. as canceled qty.
        lnTotalCan = lnTotalCan + nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
        *-- Update the Ra values in the style file with the canceled qty.
        =SEEK(RALINE.Style , 'Style')
        
        IF Style.lInvSty
          REPLACE STYLE.RA1   WITH STYLE.RA1 - RALINE.nOpnQty1 ;
                  STYLE.RA2   WITH STYLE.RA2 - RALINE.nOpnQty2 ;
                  STYLE.RA3   WITH STYLE.RA3 - RALINE.nOpnQty3 ;
                  STYLE.RA4   WITH STYLE.RA4 - RALINE.nOpnQty4 ;
                  STYLE.RA5   WITH STYLE.RA5 - RALINE.nOpnQty5 ;
                  STYLE.RA6   WITH STYLE.RA6 - RALINE.nOpnQty6 ;
                  STYLE.RA7   WITH STYLE.RA7 - RALINE.nOpnQty7 ;
                  STYLE.RA8   WITH STYLE.RA8 - RALINE.nOpnQty8 ;
                  STYLE.TotRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8
          *-- Update the Ra values for each style+warehouse with the canceled qty.
          IF SEEK(RALINE.Style + laData[7] + SPACE(10) , 'STYDYE')
            REPLACE STYDYE.RA1   WITH STYDYE.RA1 - RALINE.nOpnQty1 ;
                    STYDYE.RA2   WITH STYDYE.RA2 - RALINE.nOpnQty2 ;
                    STYDYE.RA3   WITH STYDYE.RA3 - RALINE.nOpnQty3 ;
                    STYDYE.RA4   WITH STYDYE.RA4 - RALINE.nOpnQty4 ;
                    STYDYE.RA5   WITH STYDYE.RA5 - RALINE.nOpnQty5 ;
                    STYDYE.RA6   WITH STYDYE.RA6 - RALINE.nOpnQty6 ;
                    STYDYE.RA7   WITH STYDYE.RA7 - RALINE.nOpnQty7 ;
                    STYDYE.RA8   WITH STYDYE.RA8 - RALINE.nOpnQty8 ;
                    STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
          ENDIF
          *-- Update the Ra values for each style+warehouse+dyelot with the canceled qty.
          *B606480,1 (Begin) Make sure the style uses Dylots
          *IF llDyelot .AND. SEEK(RALINE.Style + laData[7] + RALINE.Dyelot , 'STYDYE')
          IF llDyelot AND !EMPTY(RALINE.Dyelot) .AND. SEEK(RALINE.Style + laData[7] + RALINE.Dyelot , 'STYDYE')
          *B606480,1 (End)
            REPLACE STYDYE.RA1   WITH STYDYE.RA1 - RALINE.nOpnQty1 ;
                    STYDYE.RA2   WITH STYDYE.RA2 - RALINE.nOpnQty2 ;
                    STYDYE.RA3   WITH STYDYE.RA3 - RALINE.nOpnQty3 ;
                    STYDYE.RA4   WITH STYDYE.RA4 - RALINE.nOpnQty4 ;
                    STYDYE.RA5   WITH STYDYE.RA5 - RALINE.nOpnQty5 ;
                    STYDYE.RA6   WITH STYDYE.RA6 - RALINE.nOpnQty6 ;
                    STYDYE.RA7   WITH STYDYE.RA7 - RALINE.nOpnQty7 ;
                    STYDYE.RA8   WITH STYDYE.RA8 - RALINE.nOpnQty8 ;
                    STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
          ENDIF
        ENDIF
        *-- Blank the open qty.
        *IF laData[26] = 0
        *  REPLACE RALINE.nOpnQty1   WITH 0 ;
        *          RALINE.nOpnQty2   WITH 0 ;
        *          RALINE.nOpnQty3   WITH 0 ;
        *          RALINE.nOpnQty4   WITH 0 ;
        *          RALINE.nOpnQty5   WITH 0 ;
        *          RALINE.nOpnQty6   WITH 0 ;
        *          RALINE.nOpnQty7   WITH 0 ;
        *          RALINE.nOpnQty8   WITH 0 ;
        *          RALINE.nTotOpnQty WITH 0
        *ENDIF
      ENDSCAN
      SELECT RETAUTH
      *-- Instead of deleting the RA, just cancel it.
      REPLACE Status    WITH IIF(laData[26] = 0 , "X" , "C") ;
              nRetA_Can WITH lnTotalCan ;
              nRetA_Opn WITH 0
    ENDIF
  *-- If the R/A status was canceled, change it to be open.
  CASE laData[4] = 'X'
    lnTotalOpn = 0
    SELECT RALINE
    =SEEK(laData[1])
    SCAN FOR RANO = laData[1]
      *-- Calculate the opened qty.
      lnTotalOpn = lnTotalOpn + nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
      *-- Update the opened qty. in the R/A lines file.
      SELECT RALINE
      REPLACE RALINE.nOpnQty1   WITH RALINE.Qty1 ;
              RALINE.nOpnQty2   WITH RALINE.Qty2 ;
              RALINE.nOpnQty3   WITH RALINE.Qty3 ;
              RALINE.nOpnQty4   WITH RALINE.Qty4 ;
              RALINE.nOpnQty5   WITH RALINE.Qty5 ;
              RALINE.nOpnQty6   WITH RALINE.Qty6 ;
              RALINE.nOpnQty7   WITH RALINE.Qty7 ;
              RALINE.nOpnQty8   WITH RALINE.Qty8 ;
              RALINE.nTotOpnQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      *-- Update the Ra values in the style file with the opened qty.
      =SEEK(RALINE.Style , 'Style')
      IF Style.lInvSty
        REPLACE STYLE.RA1   WITH STYLE.RA1 + QTY1 ;
                STYLE.RA2   WITH STYLE.RA2 + QTY2 ;
                STYLE.RA3   WITH STYLE.RA3 + QTY3 ;
                STYLE.RA4   WITH STYLE.RA4 + QTY4 ;
                STYLE.RA5   WITH STYLE.RA5 + QTY5 ;
                STYLE.RA6   WITH STYLE.RA6 + QTY6 ;
                STYLE.RA7   WITH STYLE.RA7 + QTY7 ;
                STYLE.RA8   WITH STYLE.RA8 + QTY8 ;
                STYLE.TotRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8
        *-- Update the Ra values for each style+warehouse with the opened qty.
        IF SEEK(RALINE.Style + laData[7] + SPACE(10) , 'STYDYE')
          REPLACE STYDYE.RA1   WITH STYDYE.RA1 + QTY1 ;
                  STYDYE.RA2   WITH STYDYE.RA2 + QTY2 ;
                  STYDYE.RA3   WITH STYDYE.RA3 + QTY3 ;
                  STYDYE.RA4   WITH STYDYE.RA4 + QTY4 ;
                  STYDYE.RA5   WITH STYDYE.RA5 + QTY5 ;
                  STYDYE.RA6   WITH STYDYE.RA6 + QTY6 ;
                  STYDYE.RA7   WITH STYDYE.RA7 + QTY7 ;
                  STYDYE.RA8   WITH STYDYE.RA8 + QTY8 ;
                  STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
        ENDIF
        *-- Update the Ra values for each style+warehouse+dyelot with the opened qty.
        *B606480,1 (Begin) Make sure the style uses Dylots
        *IF llDyelot .AND. SEEK(RALINE.Style + laData[7] + RALINE.Dyelot , 'STYDYE')
        IF llDyelot AND !EMPTY(RALINE.Dyelot) .AND. SEEK(RALINE.Style + laData[7] + RALINE.Dyelot , 'STYDYE')
        *B606480,1 (End)
          REPLACE STYDYE.RA1   WITH STYDYE.RA1 + QTY1 ;
                  STYDYE.RA2   WITH STYDYE.RA2 + QTY2 ;
                  STYDYE.RA3   WITH STYDYE.RA3 + QTY3 ;
                  STYDYE.RA4   WITH STYDYE.RA4 + QTY4 ;
                  STYDYE.RA5   WITH STYDYE.RA5 + QTY5 ;
                  STYDYE.RA6   WITH STYDYE.RA6 + QTY6 ;
                  STYDYE.RA7   WITH STYDYE.RA7 + QTY7 ;
                  STYDYE.RA8   WITH STYDYE.RA8 + QTY8 ;
                  STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
        ENDIF
      ENDIF
    ENDSCAN
    SELECT RETAUTH
    *-- Instead of deleting the RA, just cancel it.
    REPLACE Status    WITH "O" ;
            nRetA_Can WITH 0 ;
            nRetA_Opn WITH lnTotalOpn
  *E301643,1 mhm  we will Search on the custom table if the mail sent we append new one 
  *               else we replace all with approved and the remanning text according to it[Start]
    IF (llCrmIns AND RETAUTH.lFromWeb)
      SELECT CRMesag
      =SEEK('R' + RetAuth.RaNo,'CRMesag')
      lcMfrom     = gfGetMemVar('M_NOTEMAIL' , gcAct_Comp)
      lcMto       = IIF(SEEK('M'+RetAuth.Account,'Customer'),EVALUATE('Customer.' + gfGetMemVar('M_CONFMAIL')),'')

      *--if sent or not found add new one
      IF lSent OR EOF()
        APPEND BLANK
        =gfAdd_Info('CRMesag')
        REPLACE cTransType   WITH 'R',;
                cTransNo     WITH RetAuth.RaNo,;
                cMailFrom    WITH lcMfrom,;
                dTransDate   WITH gdSysDate,;
                cMailTo      WITH lcMto,;
                lSent        WITH .F.
 
        lCMsgSbjct  = 'Your R/A Request #:'+ ladata[1] + 'has been approved.'
        lCLetterid  = gfGetMemVar('M_RAAPR' , gcAct_Comp)
    
        REPLACE lApprove WITH .T. , cMsgSubjct WITH lCMsgSbjct , ; 
                cLetterId WITH lCLetterid    
     
      ELSE
        REPLACE cTransType   WITH 'R' ,;
                cTransNo     WITH RetAuth.RaNo , ;
                cMailFrom    WITH lcMfrom ,;
                dTransDate   WITH gdSysDate,;
                cMailTo      WITH    lcMto, ;
                lSent        WITH .F.
 
        lCMsgSbjct  = 'Your R/A Request #:'+ ladata[1] + 'has been approved.'
        lCLetterid  = gfGetMemVar('M_RAAPR' , gcAct_Comp)
        REPLACE lApprove   WITH .T. ,;
                cMsgSubjct WITH lCMsgSbjct , ; 
                cLetterId  WITH lCLetterid    
  
      ENDIF  
    ENDIF  
  *-in case of cancel Electronic one
  CASE laData[4] = 'E'
      IF llCrmIns
        lnTotalCan = 0
        SELECT RALINE
        =SEEK(laData[1])
        SCAN FOR RANO = laData[1]
          *-- Consider the open qty. as canceled qty.
          lnTotalCan = lnTotalCan + nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
        
        ENDSCAN
        SELECT RETAUTH
        *-- Instead of deleting the RA, just cancel it.
        REPLACE Status    WITH IIF(laData[26] = 0 , "X" , "C") ;
                nRetA_Can WITH lnTotalCan ;
                nRetA_Opn WITH 0
      ENDIF       
      *--get variables
      lcMfrom     = gfGetMemVar('M_NOTEMAIL' , gcAct_Comp)
      lCMsgSbjct  = 'Your R/A Request #:'+ ladata[1] + 'has not been approved.'
      
      SELECT CRMESAG
      APPEND BLANK
      REPLACE CTransType WITH 'R',;
              CtransNo   WITH laData[1],;
              LApprove  WITH .F.,;
              CMailFrom  WITH lcMfrom,;
              CMailTo    WITH lcMto ,;
              CMsgSubjct WITH lCMsgSbjct,;
              CLetterid  WITH lCLetterid,;
              LSent      WITH .F.
  *E301643,1 mhm  [End]
            
ENDCASE
*-- Refresh laData array that hold the header info with the updated values.
SELECT RETAUTH
SCATTER FIELDS &lcScFields TO laData

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Reham Al-Allamy
*! Date      : 07/03/1996
*! Purpose   : Save the current R/A.
*!*************************************************************
*! Calls     : gfModalGen, gfGetExSin, gfTmp2Mast, gfSequence
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpSavScr()
*!*************************************************************
*
FUNCTION lpSavScr

*B603238,1 HDM [Start] Stop Checking the details entry
**-- Do not save if there is no lines entered for the current R/A.
*SELECT (lcTmpRetLn)
*LOCATE FOR !EMPTY(Style)
*IF !FOUND()
*  *** No lines entered! Cannot save. ***
*  *** <  Ok  > ***
*  =gfModalGen('INM46008B00000','ALERT')
*  llCSave = .F.
*  RETURN
*ENDIF

**-- Check to remove the blank styles.
*LOCATE FOR EMPTY(Style)
*IF FOUND()
*  *** There is one or more empty style(s).  Would you ***
*  *** like to remove them manually or automatically?  ***
*  *** < Manually > - < Automatically > ***
*  IF gfModalGen("QRM46027B46002" , "DIALOG") = 1
*    llCSave = .F.
*    RETURN
*  ENDIF
*ENDIF
*B603238,1 HDM [End]

*B038431,1 NNA 10/03/2004 (Begin) Check if Empty account or not to not proceed
IF EMPTY(laData[2])
  *** No account Selected . Cannot Proceed! ***
  *** <  Ok  > ***
  =gfModalGen('INM42080B00000','ALERT','No account Selected')
  IF lnActFolder = 1
    _CUROBJ = OBJNUM(laData[2])
  ENDIF
  llCSave = .F.
  RETURN
ENDIF
*B038431,1 NNA (End)

*-- Do not save if the warehouse code was not entered.
IF llMultiWH
  IF EMPTY(laData[7])
    *** You have to enter a warehouse. Cannot save! ***
    *** <  Ok  > ***
    =gfModalGen('INM46013B00000','ALERT')
    IF lnActFolder = 1
      _CUROBJ = OBJNUM(puWareHous)
    ENDIF
    llCSave = .F.
    RETURN
  ENDIF
ELSE
  laData[7] = lcDefWare
ENDIF

*-- Do not save if the system was multi currency & the 
*-- currency info was not entered.
IF llMulCurr
  DO CASE
    CASE EMPTY(laData[14])
      *** You cannot leave the currency code empty! ***
      *** <  Ok  > ***
      =gfModalGen("INM46015B00000" , "DIALOG")
      IF lnActFolder = 1
        _CUROBJ = OBJNUM(laData[14])
      ENDIF
      llCSave = .F.
      RETURN
    CASE EMPTY(laData[16])
      *** The exchange rate must be greater than zero. ***
      *** <  Ok  > ***
      =gfModalGen("INM46001B00000" , "DIALOG")
      IF lnActFolder = 1
        _CUROBJ = OBJNUM(laData[16])
      ENDIF
      llCSave = .F.
      RETURN
  ENDCASE
  *-- Got exchange rate sign and unit sign.
  lcUntSin = ' '
  lcExRSin = gfGetExSin(@lcUntSin, laData[14])
ELSE
  laData[14] = gcBaseCurr &&  Variable to hold currency code.
  laData[15] = 1          &&  Variable to hold currency unit.
  laData[16] = 1          &&  Variable to hold exchange rate.
  lcUntSin   = '/'        &&  Variable to hold unit sign.
  lcExRSin   = '/'        &&  Variable to hold exchange rate sign.
ENDIF

*-- Flag to know if enter the add mode before or not.
llAddMode  = .F.

*-- Check the authorized amount.
IF laData[9] = 0
  *** Total authorization amount is zero.  Continue? ***
  *** < Yes > - < No > ***
  IF gfModalGen("INM46017B00006" , "DIALOG") = 2
    IF lnActFolder = 1
      _CUROBJ = OBJNUM(laData[9])
    ENDIF
    llCSave = .F.
    RETURN
  ENDIF
ENDIF

*B803124,1 Reham On 04/27/2000    *** Begin ***
*B803124,1 Do not adjust the authorized qty. & amount if there is no lines
*B803124,1 in the current RA, save them as they entered due to a 2.6 feature.
IF lnNoOfLines > 0
*B803124,1 Reham On 04/27/2000    *** End   ***
  *-- Count total qty. & total amount for al valid lines.
  SELECT (lcTmpRetLn)
  SUM TotQty , Amount FOR (cStatus <> "D" .OR. !DELETED()) ;
                          .AND. !EMPTY(STYLE) TO lnAllQty , lnAllAmont
  
  *-- Compare the total Qty. & total amount for all the lines with 
  *-- the total Qty. & total amount in the header info.
  IF (lnAllAmont <> laData[9]) .OR. (lnAllQty <> laData[8])
    *** Authorized pieces and amount do not match the line items!  Continue? ***
    *** < Yes > - < No > ***
    IF gfModalGen("INM46018B00006" , "DIALOG") = 2
      IF lnActFolder = 1
        _CUROBJ = IIF((lnAllQty <> laData[8]) , OBJNUM(laData[8]) , OBJNUM(laData[9]))
      ENDIF
      llCSave = .F.
      RETURN
    ENDIF
  ENDIF
  
  *-- Assign the total Qty. & total amount for all the lines to 
  *-- the total Qty. & total amount in the header info.
  laData[9] = lnAllAmont
  STORE lnAllQty TO laData[8] , laData[28]
*B803124,1 Reham On 04/27/2000    *** Begin ***
ENDIF
*B803124,1 Reham On 04/27/2000    *** End   ***

*-- If add mode, assign a R/A no. for the current R/A & add it for all lines.
IF laScrMode[4]
  IF llBased
    laData[1] = gfSequence('RANO','','',laData[19])
  ELSE
    laData[1] = gfSequence('RANO')
  ENDIF
  SELECT (lcTmpRetLn)
  REPLACE ALL RANO WITH laData[1]
ENDIF

*-- Update the header file.
SELECT RETAUTH
lcSavOrder = SET('ORDER')
SET ORDER TO RETAUTH
IF laScrMode[4]
  APPEND BLANK
ELSE
  =SEEK(laData[1],'RETAUTH')
ENDIF
GATHER FIELDS &lcScFields FROM laData
*-- Call global function to add audit fields info.
=gfAdd_Info('RETAUTH')

SET ORDER TO &lcSavOrder

*B602637,1 Reham On 03/04/98   *** Begin ***
IF laScrMode[4]
  *B602753,1 HDM 04/07/1999[Start] Stop Calling NotePad Program In lpSavScr as the global save
  *                         Will Call it
  **-- If add notes for the R/A.
  **** Do you want to add notes? ***
  **** < Yes > - < No > ***
  *IF gfModalGen("QRM00271B00006" , "DIALOG") = 1
  *  *-- Open the note pad file.
  *  =gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
  *  *-- Call the notepad program to add notes for the current R/A.
  *  =NOTEPAD('A',laData[1])
  *  *-- Close the notepad file.
  * =gfCloseFile('NotePad')
  *ENDIF
  *B602753,1 HDM 04/07/1999[End]
ENDIF
*B602637,1 Reham On 03/04/98   *** End   ***

*-- R/A line no.
lnRa_LinNo = 1

*-- Calculate the total 
STORE 0 TO lnTotalOpn , lnTotalCan
*E301077,4 Reham On 12/29/98   *** Begin ***
*E301077,4 Open the style dyelot file.
=gfOpenFile(gcDataDir+'StyDye',gcDataDir+'StyDye','SH')
*E301077,4 Reham On 12/29/98   *** End   ***

*-- Change the status for empty styles records to be "SAME" to 
*-- prevent it from processing.
SELECT (lcTmpRetLn)
REPLACE ALL cStatus WITH "S" FOR EMPTY(Style)
DELETE  ALL FOR EMPTY(Style) .AND. cStatus = "S"

*-- Process the R/A lines.
lcSavDelete = SET('DELETE')
SET DELETE OFF

lnCurRec  = 1                     && Var. hold current record no.
lnTotRec  = RECCOUNT(lcTmpRetLn)  && Var. hold total lines record count.

*-- process only the lines that its status not equal "S"
SCAN FOR !EMPTY(STYLE)
  *-- Call the global function that execute the thermometer.
  =gfTherm(lnTotRec,lnCurRec,"Saving return authorization # : "+laData[1])
  lnCurRec = lnCurRec + 1
  
  SELECT (lcTmpRetLn)
  IF cStatus <> 'S'
    =SEEK(&lcTmpRetLn..Style , 'Style')
    DO CASE
      *-- If the record was added.
      CASE cStatus = "A"

        *E301643,1 mhm Check for status 'Electronic' [Start]  
        IF RetAuth.status <> "E"
        *--mhm2000
          IF Style.lInvSty
            *-- Update the Ra values in the style file.
            REPLACE STYLE.RA1   WITH STYLE.RA1 + QTY1 ;
                    STYLE.RA2   WITH STYLE.RA2 + QTY2 ;
                    STYLE.RA3   WITH STYLE.RA3 + QTY3 ;
                    STYLE.RA4   WITH STYLE.RA4 + QTY4 ;
                    STYLE.RA5   WITH STYLE.RA5 + QTY5 ;
                    STYLE.RA6   WITH STYLE.RA6 + QTY6 ;
                    STYLE.RA7   WITH STYLE.RA7 + QTY7 ;
                    STYLE.RA8   WITH STYLE.RA8 + QTY8 ;
                    STYLE.TotRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8
            *-- Update the Ra values for each style+warehouse with the canceled qty.
            IF SEEK(&lcTmpRetLn..Style + laData[7] + SPACE(10) , 'STYDYE')
              REPLACE STYDYE.RA1   WITH STYDYE.RA1 + QTY1 ;
                      STYDYE.RA2   WITH STYDYE.RA2 + QTY2 ;
                      STYDYE.RA3   WITH STYDYE.RA3 + QTY3 ;
                      STYDYE.RA4   WITH STYDYE.RA4 + QTY4 ;
                      STYDYE.RA5   WITH STYDYE.RA5 + QTY5 ;
                      STYDYE.RA6   WITH STYDYE.RA6 + QTY6 ;
                      STYDYE.RA7   WITH STYDYE.RA7 + QTY7 ;
                      STYDYE.RA8   WITH STYDYE.RA8 + QTY8 ;
                      STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
            ENDIF
            *B606480,1 (Begin) Make sure the style uses Dylots
            *IF llDyelot .AND. SEEK(&lcTmpRetLn..Style + laData[7] + &lcTmpRetLn..Dyelot , 'STYDYE')
            IF llDyelot AND !EMPTY(&lcTmpRetLn..Dyelot) .AND. SEEK(&lcTmpRetLn..Style + laData[7] + &lcTmpRetLn..Dyelot , 'STYDYE')
            *B606480,1 (End)
            
              REPLACE STYDYE.RA1   WITH STYDYE.RA1 + QTY1 ;
                      STYDYE.RA2   WITH STYDYE.RA2 + QTY2 ;
                      STYDYE.RA3   WITH STYDYE.RA3 + QTY3 ;
                      STYDYE.RA4   WITH STYDYE.RA4 + QTY4 ;
                      STYDYE.RA5   WITH STYDYE.RA5 + QTY5 ;
                      STYDYE.RA6   WITH STYDYE.RA6 + QTY6 ;
                      STYDYE.RA7   WITH STYDYE.RA7 + QTY7 ;
                      STYDYE.RA8   WITH STYDYE.RA8 + QTY8 ;
                      STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
            ENDIF
          ENDIF
        *E301643,1 mhm[Start]  
        ENDIF
        *E301643,1 mhm[End]  
        *-- Calculate the open qty.
        lnTotalOpn = lnTotalOpn + (nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8)
      *-- If the record was modified.
      CASE cStatus = "M"
        *-- Update the R/A line open qty.
        *B606357,1 (Begin) Seek with the line's original style as it might be modified.
        *=SEEK(&lcTmpRetLn..RaNo + &lcTmpRetLn..Style + &lcTmpRetLn..cra_linno , 'RALINE')
        =SEEK(&lcTmpRetLn..RaNo + &lcTmpRetLn..oldSty + &lcTmpRetLn..cra_linno , 'RALINE')
        *B606357,1 (End)
        REPLACE nOpnQty1   WITH MAX(0 , (nOpnQty1 + Qty1 - RALINE.Qty1)) ;
                nOpnQty2   WITH MAX(0 , (nOpnQty2 + Qty2 - RALINE.Qty2)) ;
                nOpnQty3   WITH MAX(0 , (nOpnQty3 + Qty3 - RALINE.Qty3)) ;
                nOpnQty4   WITH MAX(0 , (nOpnQty4 + Qty4 - RALINE.Qty4)) ;
                nOpnQty5   WITH MAX(0 , (nOpnQty5 + Qty5 - RALINE.Qty5)) ;
                nOpnQty6   WITH MAX(0 , (nOpnQty6 + Qty6 - RALINE.Qty6)) ;
                nOpnQty7   WITH MAX(0 , (nOpnQty7 + Qty7 - RALINE.Qty7)) ;
                nOpnQty8   WITH MAX(0 , (nOpnQty8 + Qty8 - RALINE.Qty8)) ;
                nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
        *E301643,1 mhm Check for status 'Electronic' [Start]  
        IF RetAuth.status <> "E"
        *E301643,1 mhm [End]  
          IF Style.lInvSty
            *-- Update the Ra values in the style file for each R/A line.
            REPLACE STYLE.RA1   WITH STYLE.RA1 + QTY1 - RALINE.QTY1 ;
                    STYLE.RA2   WITH STYLE.RA2 + QTY2 - RALINE.QTY2 ;
                    STYLE.RA3   WITH STYLE.RA3 + QTY3 - RALINE.QTY3 ;
                    STYLE.RA4   WITH STYLE.RA4 + QTY4 - RALINE.QTY4 ;
                    STYLE.RA5   WITH STYLE.RA5 + QTY5 - RALINE.QTY5 ;
                    STYLE.RA6   WITH STYLE.RA6 + QTY6 - RALINE.QTY6 ;
                    STYLE.RA7   WITH STYLE.RA7 + QTY7 - RALINE.QTY7 ;
                    STYLE.RA8   WITH STYLE.RA8 + QTY8 - RALINE.QTY8 ;
                    STYLE.TotRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8
            
            *-- Seek the RA values for each style+warehouse in the style dyelot file.
            IF SEEK(&lcTmpRetLn..Style + laData[7] + SPACE(10) , 'STYDYE')
              REPLACE STYDYE.RA1   WITH STYDYE.RA1 + QTY1 - RALINE.QTY1 ;
                      STYDYE.RA2   WITH STYDYE.RA2 + QTY2 - RALINE.QTY2 ;
                      STYDYE.RA3   WITH STYDYE.RA3 + QTY3 - RALINE.QTY3 ;
                      STYDYE.RA4   WITH STYDYE.RA4 + QTY4 - RALINE.QTY4 ;
                      STYDYE.RA5   WITH STYDYE.RA5 + QTY5 - RALINE.QTY5 ;
                      STYDYE.RA6   WITH STYDYE.RA6 + QTY6 - RALINE.QTY6 ;
                      STYDYE.RA7   WITH STYDYE.RA7 + QTY7 - RALINE.QTY7 ;
                      STYDYE.RA8   WITH STYDYE.RA8 + QTY8 - RALINE.QTY8 ;
                      STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
            ENDIF
            *-- If the system & style dyelot yes, Update the Ra values for each
            *-- style+warehouse+dyelot in the style dyelot file.
            *B606480,1 (Begin) Make sure the style uses Dylots
            *IF llDyelot .AND. SEEK(&lcTmpRetLn..Style + laData[7] + &lcTmpRetLn..Dyelot , 'STYDYE')
            IF llDyelot AND !EMPTY(&lcTmpRetLn..Dyelot) .AND. SEEK(&lcTmpRetLn..Style + laData[7] + &lcTmpRetLn..Dyelot , 'STYDYE')
            *B606480,1 (End)
            
              REPLACE STYDYE.RA1   WITH STYDYE.RA1 + QTY1 - RALINE.QTY1 ;
                      STYDYE.RA2   WITH STYDYE.RA2 + QTY2 - RALINE.QTY2 ;
                      STYDYE.RA3   WITH STYDYE.RA3 + QTY3 - RALINE.QTY3 ;
                      STYDYE.RA4   WITH STYDYE.RA4 + QTY4 - RALINE.QTY4 ;
                      STYDYE.RA5   WITH STYDYE.RA5 + QTY5 - RALINE.QTY5 ;
                      STYDYE.RA6   WITH STYDYE.RA6 + QTY6 - RALINE.QTY6 ;
                      STYDYE.RA7   WITH STYDYE.RA7 + QTY7 - RALINE.QTY7 ;
                      STYDYE.RA8   WITH STYDYE.RA8 + QTY8 - RALINE.QTY8 ;
                      STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
            ENDIF
          ENDIF
        *E301643,1 mhm Check for status 'Electronic' [Start]  
        ENDIF
        *E301643,1 mhm [End]  
        lnTotalOpn = lnTotalOpn + (nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8)
        *lnTotalCan = lnTotalCan + IIF(nTotOpnQty < RALINE.nTotOpnQty , (RALINE.nTotOpnQty - nTotOpnQty) , 0)
      *-- If the record was deleted.
      CASE cStatus = "D"
        *-- Update the R/A values for each line in the style file.
        *E301643,1 mhm Check for status 'Electronic' [Start]  
        IF RetAuth.status <> "E"
        *E301643,1 mhm [end]  
          =SEEK(&lcTmpRetLn..RaNo + &lcTmpRetLn..Style + &lcTmpRetLn..cra_linno , 'RALINE')
          IF Style.lInvSty
            REPLACE STYLE.RA1   WITH STYLE.RA1 - RALINE.QTY1 ;
                    STYLE.RA2   WITH STYLE.RA2 - RALINE.QTY2 ;
                    STYLE.RA3   WITH STYLE.RA3 - RALINE.QTY3 ;
                    STYLE.RA4   WITH STYLE.RA4 - RALINE.QTY4 ;
                    STYLE.RA5   WITH STYLE.RA5 - RALINE.QTY5 ;
                    STYLE.RA6   WITH STYLE.RA6 - RALINE.QTY6 ;
                    STYLE.RA7   WITH STYLE.RA7 - RALINE.QTY7 ;
                    STYLE.RA8   WITH STYLE.RA8 - RALINE.QTY8 ;
                    STYLE.TotRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8
          
            *-- Seek the RA values for each style+warehouse in the style dyelot file.
            IF SEEK(&lcTmpRetLn..Style + laData[7] + SPACE(10) , 'STYDYE')
              REPLACE STYDYE.RA1   WITH STYDYE.RA1 - RALINE.QTY1 ;
                      STYDYE.RA2   WITH STYDYE.RA2 - RALINE.QTY2 ;
                      STYDYE.RA3   WITH STYDYE.RA3 - RALINE.QTY3 ;
                      STYDYE.RA4   WITH STYDYE.RA4 - RALINE.QTY4 ;
                      STYDYE.RA5   WITH STYDYE.RA5 - RALINE.QTY5 ;
                      STYDYE.RA6   WITH STYDYE.RA6 - RALINE.QTY6 ;
                      STYDYE.RA7   WITH STYDYE.RA7 - RALINE.QTY7 ;
                      STYDYE.RA8   WITH STYDYE.RA8 - RALINE.QTY8 ;
                      STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
            ENDIF
          
            *-- If the system & style dyelot yes, Update the Ra values for each
            *-- style+warehouse+dyelot in the style dyelot file.
            *B606480,1 (Begin) Make sure the style uses Dylots
            *IF llDyelot .AND. SEEK(&lcTmpRetLn..Style + laData[7] + &lcTmpRetLn..Dyelot , 'STYDYE')
            IF llDyelot AND !EMPTY(&lcTmpRetLn..Dyelot) .AND. SEEK(&lcTmpRetLn..Style + laData[7] + &lcTmpRetLn..Dyelot , 'STYDYE')
            *B606480,1 (End)
              REPLACE STYDYE.RA1   WITH STYDYE.RA1 - RALINE.QTY1 ;
                      STYDYE.RA2   WITH STYDYE.RA2 - RALINE.QTY2 ;
                      STYDYE.RA3   WITH STYDYE.RA3 - RALINE.QTY3 ;
                      STYDYE.RA4   WITH STYDYE.RA4 - RALINE.QTY4 ;
                      STYDYE.RA5   WITH STYDYE.RA5 - RALINE.QTY5 ;
                      STYDYE.RA6   WITH STYDYE.RA6 - RALINE.QTY6 ;
                      STYDYE.RA7   WITH STYDYE.RA7 - RALINE.QTY7 ;
                      STYDYE.RA8   WITH STYDYE.RA8 - RALINE.QTY8 ;
                      STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
            ENDIF
          ENDIF
        *E301643,1 MHM [Start]  
        ENDIF  
        *E301643,1 MHM [End]  
        *-- Calculate the total canceled qty.
        *lnTotalCan = lnTotalCan + (nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8)
    ENDCASE
  ELSE
    *B603019,1 Reham On 06/28/1999   *** Begin ***
    *B603019,1 Detect addng open qty. for the deleted lines in the add mode.
    IF !DELETED() .OR. !laScrMode[4]
    *B603019,1 Reham On 06/28/1999   *** End   ***
      *-- Calculate the total open qty.
      lnTotalOpn = lnTotalOpn + (nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8)
    *B603019,1 Reham On 06/28/1999   *** Begin ***
    ENDIF
    *B603019,1 Reham On 06/28/1999   *** End   ***
  ENDIF
  IF cStatus <> "D"
    *-- Save the R/A line #.
    REPLACE cRa_LinNo WITH ALLTRIM(STR(lnRa_LinNo))
    *-- Adjust the line no. for each line.
    lnRa_LinNo = lnRa_LinNo + 1
  ENDIF
ENDSCAN
*-- If the thermometer was not closed, call global function to close it.
*B603238,1 [Start] Don't call gfTherm if the total record is 0
*IF lnCurRec # lnTotRec
*B603238,1 [End]
IF lnCurRec # lnTotRec AND lnTotRec > 0
  =gfTherm(lnTotRec , lnTotRec , "Saving return authorization # : "+laData[1])
ENDIF

SET DELETE &lcSavDelete

*-- Update the lines file.
=gfTmp2Mast('RALINE' , lcTmpRetLn , 'Saving the lines for R/A # : ' + laData[1] + ' ...')

*-- Clear the R/A temp. lines file.
SELECT (lcTmpRetLn)
ZAP

*-- Update the header file with the opened, canceled & budget qty.
SELECT RETAUTH
*REPLACE nRetA_Can WITH nRetA_Can + lnTotalCan ;
        nRetA_Opn WITH lnTotalOpn ;
        nRetA_Bud WITH nRetA_Can + nRetA_Opn
REPLACE nRetA_Opn WITH lnTotalOpn ;
        nRetA_Bud WITH nRetA_Can + nRetA_Opn

*-- Inform the user with the return authorization no.
IF laScrMode[4]
  *** Return Authorization has been saved as : {laData[1]}. ***
  *** <   OK   > ***
  =gfModalGen('INM46029B00000','DIALOG' , laData[1])
ENDIF

*!*************************************************************
*! Name      : lfvRaNotes
*! Developer : Reham Al-Allamy
*! Date      : 06/30/1997
*! Purpose   : Valid function to display the current record notes.
*!*************************************************************
*! Calls     : NOTEPAD
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRaNotes()
*!*************************************************************
*
Function lfvRaNotes

*-- Clear the trapped key.
PUSH KEY
ON KEY

*E301077,4 Reham On 12/17/98   *** Begin ***
*E301077,4 Open the note pad file.
=gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
*E301077,4 Reham On 12/17/98   *** End   ***

*-- Call the notepad program to add notes for the current R/A.
=NOTEPAD('A',laData[1])

*E301077,4 Reham On 12/17/98   *** Begin ***
=gfCloseFile('NotePad')
*E301077,4 Reham On 12/17/98   *** End   ***

*-- Trap the needed keys.
POP KEY

*!*************************************************************
*! Name      : lfvLinkTo
*! Developer : Reham Al-Allamy
*! Date      : 04/24/1997
*! Purpose   : Valid function of the link to button.
*!*************************************************************
*! Calls     : GetObj
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvLinkTo()
*!*************************************************************
*
Function lfvLinkTo
PRIVATE lnAlias

*-- Save the current alias.
lnAlias = SELECT()

*-- Clear the trapped key.
PUSH KEY

*-- Call the object linking program.
DO GetObj WITH 'R',laData[1]

*-- Trap the needed keys.
POP KEY

*-- Restore the old alias.
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGetprice
*! Developer : Reham Al-Allamy
*! Date      : 04/11/1999
*! Purpose   : Get Style Price
*!*************************************************************
*! Calls     : lfCheckPri()
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!                       lnQuantity : Total Quantity
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfGetprice(lcStyle,'A',100)
*!*************************************************************
*
FUNCTION lfGetprice
PARAMETERS lcStyle,lcLevel,lnQuantity
PRIVATE lnStyPric,lcLevel,lnQuantity

*B602771,1 Reham On 03/08/99
*B602771,1 Get the style price level.
=SEEK(lcStyle,'Style')
IF lcLevel = 'Q'
  DO CASE
    CASE Style.nAtQtyC > 0 AND lnQuantity > Style.nAtQtyC
      lcLevel = 'C'
    CASE Style.nAtQtyB > 0 AND lnQuantity > Style.nAtQtyB
      lcLevel = 'B'
    OTHERWISE
      lcLevel = 'A'
  ENDCASE
ELSE
  lcLevel=IIF(INLIST(lcLevel,'A','B','C'),lcLevel,'A')
ENDIF

lnStyPric = IIF(!llMulCurr OR laData[14] = gcBaseCurr,Style.Price&lcLevel,;
                      gfStyPrice(lcStyle,lcCustPLvl,laData[14]))

*B602771,1 Call global function to get the price level.
*B602771,1 Call local function to select between the different existing 
*B602771,1 price levels if the selected one is zero.
RETURN(IIF(lnStyPric = 0 , lfCheckPric(lcStyle,lcLevel) , lnStyPric))

*!*************************************************************
*! Name      : lfCheckPric
*! Developer : Reham Al-Allamy
*! Date      : 04/11/1999
*! Purpose   : Select alternative price level
*!*************************************************************
*! Calls     : gfStyPrice(),SOSTYPRI.SPX
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfCheckPri('A')
*!*************************************************************
*
FUNCTION lfCheckPric
PARAMETERS lcStyle,lcLevel
PRIVATE lcTitle,lcPrompt,lcPrompt1,lcPrompt2,lcPrompt3,lnStyPric,lnStyPric1,;
        lnStyPric2,lnStyPric3, lnPrice

*B602771,1 Seek in the style file to get the right price level.
=SEEK(lcStyle,'Style')
lcTitle  = PROPER(lcStyHdr)+': '+lcStyle
lnStyPric1 = IIF(!llMulCurr OR laData[14]=gcBaseCurr,Style.PriceA,;
                       gfStyPrice(lcStyle,'A',laData[14]))
lnStyPric2 = IIF(!llMulCurr OR laData[14]=gcBaseCurr,Style.PriceB,;
                       gfStyPrice(lcStyle,'B',laData[14]))
lnStyPric3 = IIF(!llMulCurr OR laData[14]=gcBaseCurr,Style.PriceC,;
                       gfStyPrice(lcStyle,'C',laData[14]))
lnStyPric  = IIF(lnStyPric1 >0,1,IIF(lnStyPric2 >0,2,AT(lcLevel,'ABC')))
lcPrompt   = "Price level '"+lcLevel+"' is zero. Proceed with"
lcPrompt1  = 'Price level A, '+ALLTRIM(STR(lnStyPric1,12,2))
lcPrompt2  = 'Price level B, '+ALLTRIM(STR(lnStyPric2,12,2))
lcPrompt3  = 'Price level C, '+ALLTRIM(STR(lnStyPric3,12,2))

lnPrice    = 1

*-- Clear the trapped keys.
PUSH KEY
ON KEY

*B602771,1 Call screen to select price levels.
DO (gcScrDir+"SOSTYPRI.SPX")

*-- Restore the trapped keys.
POP KEY

*RETURN(EVAL('lnStyPric'+STR(lnStyPric,1)))
RETURN(EVAL('lnStyPric'+STR(lnPrice,1)))

*!*************************************************************
*! Name      : lfCalcDisc
*! Developer : Reham Al-Allamy
*! Date      : 04/11/1999
*! Purpose   : Calculate style dicount for the selected price.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcStyle   : Style
*!                       lnCurPric : Price
*!*************************************************************
*! Returns            :  Price after discount
*!*************************************************************
*! Example            :  =lfCalcDisc(lcStyle,lnCurPric)
*!*************************************************************
*
FUNCTION lfCalcDisc
PARAMETERS lcStyle , lnCurPric
PRIVATE lcStyle , lnCurPric

*B602771,1 Seek to find there is a discount for the current style price.
IF SEEK(lcStyle , 'Style')
  lcDiscCode = Style.cDiscCode
  *-- If the discount code for the selected style not empty.
  IF !EMPTY(lcDiscCode)
    *-- Get the discount percentage for this discount code from the codes file.
    DECLARE laStyDisc[1,2]
    laStyDisc[1,1] = 'DISCPCNT'
    laStyDisc[1,2] = 'lnDiscPcnt'
    =gfRltFld(lcDiscCode , @laStyDisc , "CDISCCODE")
    *-- If the discount percentage greater than zero...
    IF lnDiscPcnt > 0
      *B602863,1 Reham On 05/12/99   *** Begin ***
      *B602863,1 Add condition to display the message only if the price changed.
      IF !(&lcTmpRetLn..PRICE == (lnCurPric - (lnCurPric * lnDiscPcnt / 100)))
        *lcTmpStr = ALLTRIM(STR(lnDiscPcnt)) +"|" + ALLTRIM(STR(lnCurPric)) + ;
                   "|" + ALLTRIM(STR(lnCurPric * lnDiscPcnt /100)) + "|" + ;
                   ALLTRIM(STR(lnCurPric - (lnCurPric * lnDiscPcnt / 100)))
        lcTmpStr = ALLTRIM(STR(lnDiscPcnt)) +"|" + ALLTRIM(STR(lnCurPric,10,3)) + ;
                   "|" + ALLTRIM(STR(lnCurPric * lnDiscPcnt /100,10,3)) + "|" + ;
                   ALLTRIM(STR(lnCurPric - (lnCurPric * lnDiscPcnt / 100),10,3))
        *B602863,1 Reham On 05/24/1999   *** End   ***
        *** There is discount percentage on this style : {lnDiscPcnt} ***
        *** Gross Price is : {lnCurPric}
        *** Discount amount is : {lnCurPric * lnDiscPcnt /100}
        *** Net Price is : {lnCurPric - (lnCurPric * lnDiscPcnt / 100)}
        =gfModalGen("INM46030B00000" , "DIALOG" , lcTmpStr)
        
      *B602863,1 Reham On 05/12/99   *** Begin ***
      ENDIF
      *B602863,1 Reham On 05/12/99   *** End   ***
      *-- Get a discount on the total gross price, & inform the user.
      lnCurPric = lnCurPric - (lnCurPric * lnDiscPcnt / 100)
    ENDIF
  ENDIF
ENDIF
*B602771,1 Return with the current price.
RETURN lnCurPric


*!*************************************************************
*! Name      : lfwData_7
*! Developer : Abdel-rahim koura
*! Date      : 10/03/2001
*! Purpose   : When function of the warehouse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwData_7()
*!*************************************************************
*
FUNCTION lfwData_7
*-- Save the old warehouse.
lnOldValue = puWareHous
*--end function lfwData_7()



*!*************************************************************
*! Name        : lfActPad
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 07/25/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : activate a new pad with the option
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*E301643,1 MHM
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
DEFINE POPUP _OPTIONPOP MARGIN SHADOW

*C123847,1  TMI [Start] show this menu bar if CR module is installed
IF llCrmIns
  *C123847,1  TMI [End  ] 
  DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Approve R/A'   SKIP FOR (laData[4] <> 'E')OR laScrMode[1] OR laScrMode[3] OR laScrMode[4]
  *C123847,1  TMI [Start] close the if condition
ENDIF
*C123847,1  TMI [End  ] 

*C123847,1  TMI [Start] Add an option "Select Employee" to the option menu pad
IF ASCAN(laEvntTrig , PADR('SELEMPL',10)) <> 0
  =gfDoTriger('RMRTATH',PADR('SELEMPL',10))
ENDIF     
*C123847,1  TMI [End  ] 

ON SELECTION POPUP _OPTIONPOP DO lpOptions WITH BAR()
ON KEY LABEL ALT+P ACTIVATE POPUP _OPTIONPOP

*!*************************************************************
*! Name        : lpOptions
*! Developer   : Mohamed shokry (MHM)
*! Date        : 07/25/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : define the actions done when selecting one of the option bars
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*E301643,1 MHM
PROCEDURE lpOptions
PARAMETERS lnBar
DO CASE
  CASE lnBar = 1
    IF gfModalGen("QRM00002B00006","ALERT",'','','Are you sure you want to approve this R/A ?') = 1  
      =lfApprov()
    ENDIF
ENDCASE    

*!*************************************************************
*! Name        : lfApprov
*! Developer   : Mohamed shokry (MHM)
*! Date        : 07/25/2001
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Purpose     : Validate approve for RA 
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Calls       : 
*!*************************************************************
*E301643,1 MHM
FUNCTION lfApprov
PRIVATE lnCurrALs

lnCurrALs = SELECT()
*--Change to Open
SELECT Retauth
lcMto = IIF(SEEK('M' + RetAuth.Account,'Customer'), EVALUATE('Customer.' + gfGetMemVar('M_CONFMAIL')) ,'')
lCLetterid  = gfGetMemVar('M_RAAPR' , gcAct_Comp)

IF EMPTY(lcMto)
  *The Confirmation E-mail Address on the setup screen is blank. We can not Approve your R/M.
  *<OK>
  =gfModalGen('TRM46039B00000','DIALOG')
  SELECT (lnCurrALs)
  RETURN
ENDIF  

IF EMPTY(lCLetterid)
  *The Approved ID on the setup screen is blank. Are you sure you want to continue?.
  *<YES> , <NO>
  IF gfModalGen('QRM46040B32000','DIALOG') = 2
    SELECT (lnCurrALs)
    RETURN
  ENDIF  
ENDIF  

REPLACE Retauth.status   WITH "O",;
        Retauth.lFromWeb WITH .F.
laData[4]= "O"
lcStatus = laStatus[1]
SHOW GET lcStatus

=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
SELECT RALINE
=SEEK(laData[1])
SCAN FOR RANO = laData[1]
  =SEEK(RALINE.Style , 'Style')
  IF Style.lInvSty
    *-- Update the Ra values in the style file.
    REPLACE STYLE.RA1   WITH STYLE.RA1 + QTY1 ;
            STYLE.RA2   WITH STYLE.RA2 + QTY2 ;
            STYLE.RA3   WITH STYLE.RA3 + QTY3 ;
            STYLE.RA4   WITH STYLE.RA4 + QTY4 ;
            STYLE.RA5   WITH STYLE.RA5 + QTY5 ;
            STYLE.RA6   WITH STYLE.RA6 + QTY6 ;
            STYLE.RA7   WITH STYLE.RA7 + QTY7 ;
            STYLE.RA8   WITH STYLE.RA8 + QTY8 ;
            STYLE.TotRA WITH STYLE.RA1+STYLE.RA2+STYLE.RA3+STYLE.RA4+STYLE.RA5+STYLE.RA6+STYLE.RA7+STYLE.RA8
  *-- Update the Ra values for each style+warehouse with the canceled qty.
    IF SEEK(&lcTmpRetLn..Style + laData[7] + SPACE(10) , 'STYDYE')
      REPLACE STYDYE.RA1   WITH STYDYE.RA1 + QTY1 ;
              STYDYE.RA2   WITH STYDYE.RA2 + QTY2 ;
              STYDYE.RA3   WITH STYDYE.RA3 + QTY3 ;
              STYDYE.RA4   WITH STYDYE.RA4 + QTY4 ;
              STYDYE.RA5   WITH STYDYE.RA5 + QTY5 ;
              STYDYE.RA6   WITH STYDYE.RA6 + QTY6 ;
              STYDYE.RA7   WITH STYDYE.RA7 + QTY7 ;
              STYDYE.RA8   WITH STYDYE.RA8 + QTY8 ;
              STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
    ENDIF
    *B606480,1 (Begin) Make sure the style uses Dylots
    *IF llDyelot .AND. SEEK(&lcTmpRetLn..Style + laData[7] + &lcTmpRetLn..Dyelot , 'STYDYE')
    IF llDyelot AND !EMPTY(&lcTmpRetLn..Dyelot) .AND. SEEK(&lcTmpRetLn..Style + laData[7] + &lcTmpRetLn..Dyelot , 'STYDYE')
    *B606480,1 (End)
    
      REPLACE STYDYE.RA1   WITH STYDYE.RA1 + QTY1 ;
              STYDYE.RA2   WITH STYDYE.RA2 + QTY2 ;
              STYDYE.RA3   WITH STYDYE.RA3 + QTY3 ;
              STYDYE.RA4   WITH STYDYE.RA4 + QTY4 ;
              STYDYE.RA5   WITH STYDYE.RA5 + QTY5 ;
              STYDYE.RA6   WITH STYDYE.RA6 + QTY6 ;
              STYDYE.RA7   WITH STYDYE.RA7 + QTY7 ;
              STYDYE.RA8   WITH STYDYE.RA8 + QTY8 ;
              STYDYE.TotRA WITH STYDYE.RA1+STYDYE.RA2+STYDYE.RA3+STYDYE.RA4+STYDYE.RA5+STYDYE.RA6+STYDYE.RA7+STYDYE.RA8
    ENDIF
  ENDIF
ENDSCAN
*--add record to CRMESAG
*--get variable from CRM module setups
lcMfrom     = gfGetMemVar('M_NOTEMAIL' , gcAct_Comp)
lCMsgSbjct  = 'Your R/A Request #:'+ ladata[1] + 'has been approved.'
*lcMto       = IIF(SEEK('M'+RetAuth.Account,'Customer'),EVALUATE('Customer.' + gfGetMemVar('M_CONFMAIL')),'')
*lCLetterid  = gfGetMemVar('M_RAAPR' , gcAct_Comp)
SELECT CRMESAG
APPEND BLANK
=gfAdd_Info('CRMesag')
REPLACE CTransType WITH 'R',;
        CtransNo   WITH laData[1],;
        LApprove   WITH .T.,;
        CMailFrom  WITH lcMfrom,;
        CMailTo    WITH lcMto ,;
        CMsgSubjct WITH lCMsgSbjct,;
        CLetterid  WITH lCLetterid,;
        LSent      WITH .F.
SELECT (lnCurrALs)

