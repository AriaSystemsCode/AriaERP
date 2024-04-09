*:************************************************************************
*: Program file  : ALORDAL.PRG
*: Program desc. : Allocation by order
*: Module        : Allocation
*:         System: Aria Apparel System
*:      Developer: RENEE  
*:************************************************************************
*: Calls 		 : ALORDAL.SPR
*: 				   ALORDALA.SPR 		
*:				   ALORDALS.SPR	 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Modifications      :
*E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*                           as it's now controlled globally
*B602852,1 RENEE 04/30/99 Browse order lines ordered by line number
*E301227,1 RENEE 05/17/99 1. Generate picking tickets optionally
*E301227,1 				  2. Always force allocation optionally	
*B802358,1 RENEE 06/29/99 1. Orders browse like the Sales orders browse
*B802358,1 				  2. Sometimes, pick all does not pick all
*B802358,1 				  3. Browse refreshing when selecting an order
*C200084,1 MAB   07/26/99 Add FP Logic.
*B306084,1 RAM   08/02/99 Validate customer status if ON HOLD or not 
*E301225,1 MAB 08/23/1999 Allow Force allocation option.
*E301310,1 MAB 08/24/1999 Check Approved Amount.
*B603240,1 RENEE 10/28/99 Several bugs. please refer to the entry number
*E301340,1 WAB 10/28/1999 Add an option to allow none/automatic/manual shaving.
*B802756,1 WAB 11/08/1999 The program should display the message (The allocated 
*B802756,1 WAB            qty equals zero). as soon as the user chooses to 
*B802756,1 WAB            allocate the style.
*B802756,4 RENEE          If 'generate picking tickets' and we click 'Pick All'
*B802756,4                the picking ticket column display is reversed
*E500304,1 WAB 11/25/1999 calculate the avaliable qty depend on setting in IC Module
*E500304,1 WAB            ( STQ - Alocated qty ) or ( Wip+stq - alocated qty )
*E500304,1 WAB            and Add Butt to display O.T.S 
*B603318,1 TAK 12/01/1999 Added custom process to not allow to edit piktkt if it was adornment.
*B802823,1 ABD 12/01/1999 Fix bug THAT FOR MULTI-STOER MULTI-PO#, THE PT DO NOT
*B802823,1                 SAVE & PRINT PO FOR ALL PTs.
*B802829,1 WAB 12/15/1999 Fix Bug OF 'the AVAILABLE quantity did not decrease by 
*B802829,1 WAB            what was allocated IN CASE OF DYELOT AND IN BROWSE DYELOT
*B802938,1 WAB 01/20/2000 1.fix the bug of display message to allocate manually 
*B802938,1 WAB             the lines that have no dyelots assigned to it no matter 
*B802938,1 WAB             if there is a dyelot assigned to the style. 
*B802938,1 WAB            2. the program allocate all lines excpet records with 
*B802938,1 WAB             empty dyelots Without giving any message.
*B802938,1 WAB            3. give the user choice to continue or not , if the 
*B802938,1 WAB             order on hold.
*B802938,1 WAB            4. fix the bug bug of removed the '******' from filed picktkt 
*B802938,1 WAB             if the line is alocated before and we choose cenerate picktkt. 
*B802829,1 KHM 03/06/2000 Fix the bug of not adjusting the available and 
*B802829,1                allocated qty per dyelot when allocating and 
*B802829,1                browsing the same dyelot in a multi store order.
*B802559,1 KHM 03/07/2000 Fix the bug of warehouse popup that shows nothing
*B802559,1                when calling another program from option pad such
*B802559,1                as style inquire.
*B603495,1 KHM 03/07/2000 Fix the bug of variable lcStyDTemp not found.
*B603332,1 KHM 03/12/2000 Fix the bug of not replacing the correct allocated
*B603332,1                qtys when modifying it in the style and stydy files.
*B803064,1 KHM 03/12/2000 Fix the bug not saving the P/T# when releasing an
*B803064,1                existance one and regenerate it in the same session.
*B603566,1 ABD 04/23/2000 Fix bug that the program didn't go back to the same
*B603566,1 ABD            Rcord inside the browse.
*B603506,1 ABD 06/12/2000 Increase amount sizes in Browse fields to fit currencies.
*B603506,1 ABD            [BookAmt,ShipAmt,CancelAmt,Openamt]
*          MAB 06/13/2000 Save Memvar into memo file not into array which cause a strange problem
*B803358,1 AMH 06/21/2000 Checking Order Approved Amount
*B803380,1 HBG 07/05/2000 Passed parameter to on hold message must be Null.
*B803406,1 HBG 07/26/2000 bug of "The a vailabale qut. of the
*B803406,1                first line in the browes showes for the rest of the lines" 
*B803365,1 HBG 07/26/2000 Bug of If we have order of 2 lines and first one is allocatted, and 
*B803365,1                choose this order then select line #2 while GENERATE PICKING TICKET 
*B803365,1                option is checked , the information of the lower part of the screen reflects 
*B803365,1                option the information of the first line.
*B603826,1 ADEL 08/21/2000 Fix the bug of removing astrisks from PIKTKT field in the browse and 
*B603826,1                 removing thge date from Picking date field in the browse when allocating
*B603826,1                 with qtys geater than those in the order and accept to modify the order qtys.
*B803675,1 HBG 09/21/2000 Update the avilabel column in the browse
*C102282,1 ABD 05/23/2001 Custom change to prevent the user to allocat order from the program
*B804197,1 HBG 19/09/2001 Fix bug of:
*B804197,1                - getting an unneeded message in case of Force allocation setup = 'No'
*B804197,1                - Create Pick ticket with zero Pick Qty for the line which have no stock.
*B804468,1 HBG 10/17/2001 Fix bug of wrong saving for order Qty in STYDYE file if changing the Warehouse [Begin]
*C102424,1 ADEL 10/15/01 Add record for each piktkt in ALSHPINF for Ogio.
*B804502,1 HBG 29/10/2001 Fix bug of zero total available in the browse.
*B605075,1 HBG 01/11/2001 Fix bug of In case the IC setup is " Include WIP (YES) ",
*B605075,1                Wrong AVA.QTY per size. 
*B605084,1 HBG 01/11/2001 Fix bug of not picking the style even if there is stock for it.
*B804004,1 ADEL 11/12/01  Fix the bug of "There are no dye lots for style xxxx-xxxx-color in location xxxxx." 
*C102520,1 BWA 08/01/2001 Modify the browse to be sorted with PACK_ID and add the field Pack_id to it.
*B605561,1 BWA 02/17/2002 Add a new index to the ordline file and comment the Temp. index instead.
*C200333,1 MHM 05/01/2002 Modify Browse to work with the new modification of GMA Project
*B605971,1 ASH 05/22/2002 Stop working with CTRL+D to prevent deleting the ORDHDR record.
*B804269,1 TMI 08/28/2002 If Account is on hold prevent allocation to his order.
*B605083,1 TMI 08/29/2002 Fix the bug :Wrong SO Qty if pick From is changed and Shave applied in a piktkt.
*B606448,1 ADEL 09/23/02 Update style field in OrdCanLn file when generating P\K using Order
*B606448,1               shaving Option menu so that these cancelled lines appear in the SO detail
*B606448,1               folder when browsing cancled lines.
*B606652,1 ABD 12/15/2002 Fix bug that the program get 0 qty for the second style the program 
*B606652,1 ABD            Picked qty for first style color has been released which is wrong. 
*B606625,1 NAD 12/19/2002 Fix Bug Record is edited by another User.
*B606778,1 TMI 01/20/2003 fix the bug when modify a piktkt by decreasing qtys then no decrease in same sizes in style file
*C200489,1 WAB 03/26/2003 Add the ability of picked by Cartones
*C102810,1 AMH 05/05/2003 Add trigger for Falcon Bay to customize the style browse.
*B607290,1 KHM 07/28/2003 Fix the bug of record is edited by another user.
*B037363,1 HBG 25/12/2003 The main style did not come back when releasing the Piktkt
*B037363,4 HBG 25/12/2003 Fix the follwoing bugs:
*B037363,4					- Wrong saveing booked qty incase of increasing order qty
*B037363,4					- Not saveing modified Qty incase of generating Piktkt for Allocated line
*B037363,4					- bug of not saveing modification Qty incase of 
*B037363,4                    increase Order qty and generating Piktkt for Allocated line
*B122202,1 NNA 03/25/2004 Fix bug of that a warning massage Should appeared (as the hold orders)
*B122202,1 NNA            That by give the user choice to continue or not , if the order Bulk.
*C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*E123070,1 WAM 07/12/2004 Generate 940 EDI transaction for pick tickets directed to 3PL providers.
*B123702,1 TMI 08/04/2004 Fix a problem that if ALORDAL screen is open it prevents any other screen to be activated 
*C123616,1 TMI 10/18/2004 Call trigers for allocation by carton for nik nak / memo
*C123847,1 TMI 01/13/2005 Call the alordal screen from soupdate for DIR03 to allocate order
*E126905,1 EIH 04/11/2005 Update in new field 'CALLOCATB' by 'O'in case of allocated or picking by order  
*E126905,1                ' ' for cancel allocation or picking
*B607537,1 HBG 11/27/2005 1 - Validate if order is hold in case navigation from tool bar
*                         2 - Fix bug of if open scope screen and without selecting anything press close it won't close the screen
*B130690,1 TMI 12/21/2005 Fix a bug at DCC that when release a picktickit and repick in the same session the data in whbinloc goes wrong
*E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt.
*B039660,1 NNA 02/14/2006 Transfer all the bin location's Programs To Binmain.prg instead of Davmain.prg and Check first if
*B039660,1 NNA            there is enough stock or not before making update in the Bin location Files
*B132130,1 TMI 05/21/2006 redefine release _OPTIONPOP , this happens when module is changed and return back
*T20060817.0014 TMI 09/19/2006 Enhance saving speed at GPS
*:********************************************************************************************
*C123847,1  TMI [Start] Add prameter variable to call from other programs
PARAMETERS lcOrderNo
*C123847,1  TMI [End  ] 

*-- Variable declarations
*B603495,1 KHM 03/07/2000 (Begin) Adding the initialization of lcStyDTemp
*B603495,1                to the STORE command.
*STORE '' TO lcCustName, lcOldVal  , lcStatus  , lcStore   , lcStyle   ,;
            lcGroup   , lcWareCode, lcScFields, ;
            lcDefWare , lcNdxExp  , lcGlPeriod, lcGlYear  ,; 
		    lcStkAlias, lc_TmpOrdL, lc_TmpPkLn, lc_TmpStyD, lcAlOrdal0,;
			lcAlOrdal1, lcAlOrdal2, lcAlOrdal3, lcStyDesc , lcBrowExp ,;
			lcStyHdr  , lcStyMsk  , lcStyleWid, lcAltStyDsc, lcStorDesc,;
			lcSame    , lcSession , lcOrderBr , lcOrderBr2 , lcLastMode

*B803406,1 HBG 07/30/2000 add variable lcOrderBr3 [Begin]			
*STORE '' TO lcCustName, lcOldVal  , lcStatus  , lcStore   , lcStyle   ,;
            lcGroup   , lcWareCode, lcScFields, ;
            lcDefWare , lcNdxExp  , lcGlPeriod, lcGlYear  ,; 
		    lcStkAlias, lc_TmpOrdL, lc_TmpPkLn, lc_TmpStyD, lcAlOrdal0,;
			lcAlOrdal1, lcAlOrdal2, lcAlOrdal3, lcStyDesc , lcBrowExp ,;
			lcStyHdr  , lcStyMsk  , lcStyleWid, lcAltStyDsc, lcStorDesc,;
			lcSame    , lcSession , lcOrderBr , lcOrderBr2 , lcLastMode,;
			lcStyDTemp

*C102520,1 BWA 08/01/2001 Add variable "lcPackIndx" to hold the new index.[START]
*STORE '' TO lcCustName, lcOldVal  , lcStatus  , lcStore   , lcStyle   ,;
            lcGroup   , lcWareCode, lcScFields, ;
            lcDefWare , lcNdxExp  , lcGlPeriod, lcGlYear  ,; 
		    lcStkAlias, lc_TmpOrdL, lc_TmpPkLn, lc_TmpStyD, lcAlOrdal0,;
			lcAlOrdal1, lcAlOrdal2, lcAlOrdal3, lcStyDesc , lcBrowExp ,;
			lcStyHdr  , lcStyMsk  , lcStyleWid, lcAltStyDsc, lcStorDesc,;
			lcSame    , lcSession , lcOrderBr , lcOrderBr2 , lcLastMode,;
			lcStyDTemp ,lcOrderBr3

STORE '' TO lcCustName , lcOldVal   , lcStatus   , lcStore   , lcStyle      ,;
            lcGroup    , lcWareCode , lcScFields ,                           ;
            lcDefWare  , lcNdxExp   , lcGlPeriod , lcGlYear    ,             ; 
		    lcStkAlias , lc_TmpOrdL , lc_TmpPkLn , lc_TmpStyD  , lcAlOrdal0 ,;
			lcAlOrdal1 , lcAlOrdal2 , lcAlOrdal3 , lcStyDesc   , lcBrowExp  ,;
			lcStyHdr   , lcStyMsk   , lcStyleWid , lcAltStyDsc , lcStorDesc ,;
			lcSame     , lcSession  , lcOrderBr  , lcOrderBr2  , lcLastMode ,;
			lcStyDTemp , lcOrderBr3 , lcPackIndx
*C102520,1 BWA 08/01/2001.[END]




*B803406,1 [End]						
*B804197,1 HBG 19/09/2001 Flag to check if it is Pick/Allocate All [Begin]
llPAll = .F.
*B804197,1 [End]

			
*B603495,1 KHM 03/07/2000 (End)

*E301225,1 Allow Force allocation option [Begin]
*E301310,1
lcAlwForce = ''
llChkAprov = .F.
STORE 0 TO lnForceBar , lnPickBar , lnChkAprov
llForceBar = .F.
*E301225,1 Allow Force allocation option [End  ]

*E301340,1 WAB - intialise variables
*E301340,1 WAB - START
*---lnShavBar ----->to hold the sequence no of the bar in option menu
*---lnOrdShv  ----->Shave Unalocated Qty( Automatic/Ask/Never)
*---lnReson   ----->shaving reason (popup)
*---llSaveDef ----->Set As Default field
*---llOpenFile----->flag to know if the files nedded is opened
*---llRestore ----->flag to know if the (default Order Shaving) is restored from mem file
*---laSavMem  ----->Array hold the default Order Shaving to resotre into mem file
*					laSavMem[1] --->lnOrdShv
*                   laSavMem[2] --->lnReson 
*---laUnAloSh ----->hold the element of the lnOrdShv popup 
*---laShReson ----->hold the elemnt of the lnReson  popup
*---laCodInfo ----->hold the inf. needed for gfwCodePop()
*---lcDispReas----->hold 'disable'/'enable' for reason Popup
*---lcDispFld ----->hold 'disable'/'enable' for ok butt. in order shaving screen
*---lcReason  ----->hold the reason code
PRIVATE lnShavBar,lnOrdShv,lnReson, llSaveDef, llOpenFile, llRestore
STORE 0   TO lnShavBar,lnOrdShv,lnReson 
STORE .F. TO llSaveDef, llOpenFile, llRestore

DECLARE laSavMem[2], laUnAloSh[3], laShReson[1], laCodInfo[1,10]
STORE 'DISABLE' TO lcDispReas, lcDispFld 
STORE '' TO lcReason

*- restore arrys from mem file (default Order Shaving)
IF FILE(gcDataDir+'ORDSHAV.MEM')
  RESTORE FROM (gcDataDir+'ORDSHAV.MEM') ADDITIVE
  llRestore = .T.
ENDIF
laUnAloSh[1] ='Automatic'
laUnAloSh[2] ='Manual'
laUnAloSh[3] ='Never'

laCodInfo[1,01] = "cCancReson"       && Field Name
laCodInfo[1,02] = "laShReson"      && Array Name
laCodInfo[1,03] = "lnReson"        && Popup Name
laCodInfo[1,04] = ""               && Popup Status  ("D"->Default,"A"->All)
laCodInfo[1,05] = .F.              && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[1,06] = .T.              && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[1,07] = ''               && Alternative File (For default val.)
laCodInfo[1,08] = ''               && Use this index for the Alternative file.
laCodInfo[1,09] = ''               && Seek this expretion.
laCodInfo[1,10] = ''               && Alternative Field Name
*E301340,1 WAB - END

*-- Add Variables for FP Code. [Begin]
lcTmpStore = ''		    
laStorStru = ''
*-- Add Variables for FP Code. [End  ]

*-- lnUnCmSeRc : Incomplete session record number in UNCMSESS               && The incomplete session record number.
*-- lnStep	   : Step number in saving, using for an incomplete session saving 	
STORE 0  TO lnMarker, lnScaleCnt, lnMinAlo, puWareHous, puPickWare,;
            lnMinAlo, lnStyleWid, lnMinWid, lnUnCmSeRc, lnStep

*Ren Add llGenPkTk, llForceAlc
STORE .F. TO llBrowse, cbMultiStore, llZoomed, llOrdLStr,;
			 m.lVldWare, m.lVldDye , m.cStatus, m.nForcdQty, llChngOrd, llForcLAlc,;
			 llGenPkTk, llForceAlc

llCreateTmp = .T.

STORE 'DISABLE' TO lcPickStat, lcRelStat, lcStyStat, lcForcStat

*B802559,1 KHM 03/07/2000  (Begin) Initializing the laWareHous[1,2] and 
*B802559,1                 laPanelObj[4,3] before calling gfSetup() function 
*B802559,1                 in order to save them when you call another program 
*B802559,1                 during the same session. As when you choose the 
*B802559,1                 style inquire from the option then return back. 
*DECLARE laStatus[5], laKeyField[2,4], laData[9], laScrMode[5]
DECLARE laStatus[5], laKeyField[2,4], laData[9], laScrMode[5],;
        laWareHous[1,2], laPanelObj[4,3]
*B802559,1 KHM 03/07/2000 (End)

*C102520,1 BWA 08/01/2001 Create var. to know if the customer is GMA or not.[START]
PRIVATE lcPackIndx , llGma
STORE .F. TO llGma

IF gfDoTriger('MAMATRQ',PADR('CLCWIPGMA ',10))
  llGma = .T.
ENDIF
*C102520,1 BWA 08/01/2001.[END]

IF !gfSetup()
  RETURN
ENDIF  
*E500304,1 WAB - get llTotAvlbl setting 
*E500304,1 WAB - llTotAvlbl = .F. --> avlbel qty = stq - alocated qty
*E500304,1 WAB - llTotAvlbl = .T. --> avlbel qty = WIP + stq - alocated qty
*E500304,1 WAB - START

llTotAvlbl = gfGetMemVar('M_TOTAVLBL')  

*E500304,1 WAB - END

*E301340,1 WAB - call gfwcodepop to fill the popup filed
*E301340,1 WAB - Start
=gfwCodePop(@laCodInfo ,"cCancReson" , "N")
=gfwCodePop(@laCodInfo ,"cCancReson" , "L")
*--set the default value to the ord shave popup and the default reason popup
lnOrdShv = IIF(llRestore,laSavMem[1],3)
lnReson  = IIF(llRestore,laSavMem[2],1)
*E301340,1 WAB - END

*C200489,1 WAB (Start)
IF ASCAN(laEvntTrig,PADR("ALOPNFLS",10)) <> 0
  =gfDoTriger("ALORDAL",PADR("ALOPNFLS",10))
ENDIF
*C200489,1 WAB (End)


*E130074,1 EIH 01/02/2006 Check if the field name 'CSNDPKT' exist in the PIKTKT file. [Begin]
STORE .F. TO llSndPkFld
SELECT PIKTKT
FOR I = 1 TO FCOUNT('PIKTKT')
  IF UPPER(FIELD(I))='CSNDPKT'
    llSndPkFld = .T.
	EXIT
  ENDIF 
ENDFOR
*E130074,1 EIH 01/02/2006 [End]


*-- Following variables are used to deal with the uncomplete program 
STORE .F. TO llChkUnCm, llContinue
lcFiles = ['lc_TmpOrdL,' + lc_TmpOrdL + ',' + ORDER(lc_TmpOrdL) + ';' +]+;
 		  ['lc_TmpPkLn,' + lc_TmpPkLn + ',' + ORDER(lc_TmpPkLn) + ';' +]+;
		  ['lc_TmpStyD,' + lc_TmpStyD + ',' + ORDER(lc_TmpStyD) + ';']
lcProgID   = "ALORDAL"        	&& The program ID in the incomplete session file.

*C200084,1 Skip Generate pick ticket bar if its flag = .F. [Begin]
llGenBar = .T.
IF ASCAN(laEvntTrig,PADR("NOPICKNOW",10)) <> 0
  = gfDoTriger("ALORDAL",PADR("NOPICKNOW",10))
ENDIF  
*C200084,1 Skip Generate pick ticket bar if its flag = .F. [End  ]

*:E301176,1 HDM 03/22/1999[Start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally

*DECLARE laStatus[4], laWareHous[1,2], laPanelObj[5,3]
*B802559,1 KHM 03/07/2000  (Begin) The following line is commented out
*B802559,1                 as its been declared before the gfSetup function.
*DECLARE laStatus[5], laWareHous[1,2], laPanelObj[4,3]
*B802559,1 KHM 03/07/2000  (End)
*:E301176,1 HDM 03/22/1999[End]
laKeyField[1,1] = 'laData[9]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'ORDHDR'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[1]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'ORDHDR'
laKeyField[2,4] = 2

*-- Variables that need to be saved and restored when detecting an
*-- uncomplete program session.
DECLARE laVars[3]
laVars[01] = "laData[1]"
laVars[02] = "lcBrowExp"
laVars[03] = "llOrdLStr"
*laVars[04] = "lnTapCount"
*laVars[05] = "lnActCount"
*laVars[06] = "lnTapTotal"
*laVars[07] = "lnActTotal"
*laVars[08] = "lnDiff"
*laVars[09] = "lcBankCode"
*laVars[10] = "lcChkAcnt"
*laVars[11] = "lcCurCode"
*laVars[12] = "lnExRate"
*laVars[13] = "lnCurrUnit"
*laVars[14] = "lcUntSin"
*laVars[15] = "lcExRSin"
*laVars[16] = "lcGLAcct2"
*laVars[17] = "lnUnCmSeRc"

lcOrdBrTtl   = 'Order Lines'
*-- Screen fields variable
lcScFields  = 'Order, Account, Status, Multi, Entered, Start, Complete, cWareCode, cOrdType'

laStatus[1]  = 'Open'   
laStatus[2]  = 'On Hold'
laStatus[3]  = 'Complete'
laStatus[4]  = 'Canceled'
laStatus[5]  = 'Bid     '

lcPanStat = IIF(laScrMode[1], 'DISABLE', 'ENABLE')
*:E301176,1 HDM 03/22/1999[Start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally

*laPanelObj[1,1] = 'pbOrdNote'
*laPanelObj[1,2] = gcBmpHome+'NOTES2.BMP'
*laPanelObj[1,3] = [VALID lfvOrdNote() MESSAGE 'Order Notes' &lcPanStat]

laPanelObj[1,1] = 'pbObjLink'
laPanelObj[1,2] = gcBmpHome+"Relate.bmp"
laPanelObj[1,3] = [VALID lfvObjLink() MESSAGE 'Object Link' &lcPanStat]

laPanelObj[2,1] = 'pbCustInq'
laPanelObj[2,2] = gcBmpHome+"CUSTOMER.bmp"
laPanelObj[2,3] = [VALID lfvCustInq() MESSAGE 'Customer Inquire' &lcPanStat]

laPanelObj[3,1] = 'pbStyInq'
laPanelObj[3,2] = gcBmpHome+"STYLE.bmp"
laPanelObj[3,3] = [VALID lfvStyInq() MESSAGE 'Style Inquire' &lcPanStat]

laPanelObj[4,1] = 'pbScope'
laPanelObj[4,2] = gcBmpHome+"SCOPE.bmp"
laPanelObj[4,3] = [VALID lfvScope() MESSAGE 'Select a scope of order lines' &lcPanStat]

*:E301176,1 HDM 03/22/1999[End]

*-- Restore company settings
=gfOpenFile(gcSysHome+"SYCCOMP", "CCOMP_ID", "SH")
llMultWare = ALLTRIM(gfGetMemVar('M_WareHouse')) = 'Y'
llUseDyes  = ALLTRIM(gfGetMemVar('M_DYELOT'))    = 'Y'

*E301225,1 Allow Force allocation option [Begin]
*E301310,1
lcAlwForce = gfGetMemVar('M_FORCEALO')
llChkAprov = gfGetMemVar('M_CHKAPROV')
*E301225,1 Allow Force allocation option [End  ]

=gfCloseFile("SYCCOMP")

lnSizOffst = 0

*-- Disable New, Edit and Delete buttons
llAlowNew     = .F.
laCtrStat[8]  = "DISABLE"               && Disable Delete button
*-- Use a local save and close procedure.
STORE .F. TO laDefProc[9], laDefProc[10]
*B607537,1 HBG 11/27/2005 define local function for navigation buttons to validate if order is hold [Begin]
STORE .F. TO laDefProc[1] , laDefProc[2] , laDefProc[3] , laDefProc[4]
*B607537,1 [End]

*-- Force allocation BMPs
lcForcBmp  = gcBmpHome + "FORCALO.BMP"
lcNForcBmp = gcBmpHome + "NFORCALO.BMP"

*-- If there are no open or on hold orders, present a message
*-- and exit
*Ren
SET ORDER TO TAG CUSTOMER IN CUSTOMER
SELECT ORDHDR
*SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
SET RELATION TO IIF(STORE=SPACE(8),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER

*B603506,1 ABD Increase amount sizes in Browse fields to fit currencies.
*B603506,1 ABD [BookAmt,ShipAmt,CancelAmt,Openamt] [Begin]

*lcBrFields = [Order:H="Order#",]+;
             [status:1:H="S",]+;
             [Account:H="Acct",]+;
             [store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
             [Customer.stname:30:H="Name",]+;
             [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
             [Open:H="Open.Qty.",]+;
             [OpenAmt:H="Open.Amt.",]+;
             [Ship:H="Ship.Qty.",]+;
             [Shipamt:H="Ship.Amt.",]+;
             [start:H="Start",]+;
             [Complete:H="Complete",]+;
             [lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",]+;
             [lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
             [Note1:6:H="Notes"]


lcBrFields = [Order:H="Order#",]+;
             [status:1:H="S",]+;
             [Account:H="Acct",]+;
             [store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
             [Customer.stname:30:H="Name",]+;
             [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
             [Open:H="Open.Qty.",]+;
             [OpenAmt:P= '9999999999.99' :H="Open.Amt.",]+;
             [Ship:H="Ship.Qty.",]+;
             [Shipamt:P= '9999999999.99' :H="Ship.Amt.",]+;
             [start:H="Start",]+;
             [Complete:H="Complete",]+;
             [lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",]+;
             [lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
             [Note1:6:H="Notes"]

*B603506,1 ABD- [End]



*EndRen

SET ORDER TO 
*SET FILTER TO cordtype+order = 'O' AND Status $ 'OH'
SET FILTER TO cordtype+order = 'O' 
LOCATE
IF !FOUND()
  =gfModalGen('INM44042B00000', 'DIALOG')
  glQuitting = .T.
  RETURN
ENDIF  
SET ORDER TO TAG ORDHDR 
SET RELATION TO cOrdType + Order INTO ORDLINE ADDITIVE

SELECT ORDLINE
SCATTER MEMVAR MEMO BLANK

IF !WEXIST(gcBaseWind)
  llNoShow     = .F.
  lcStyHdr     = gfItemMask("HI")
  lcStyMsk     = gfItemMask("PI")
  lnStyleWid   = LEN(lcStyHdr)

  *-- Temp name for child windows
  lcAlOrdal0   = gfTempName()
  lcAlOrdal1   = gfTempName()
  lcAlOrdal2   = gfTempName()
  lcAlOrdal3   = gfTempName()

  *C102520,1 BWA 08/01/2001 Index the ORDLINE with the new index of the Pack_id.[START]

  *B605561,1 BWA 02/17/2002 Comment this code due to I made custom index for GMA in the ordline.[START]
  *IF llGma
  *  lcPackIndx   = gfTempName()
  *  PRIVATE lcAliasPk , lcSavIndx
  *  lcAliasPk = SELECT(0)
  *  SELECT ORDLINE
  *  lcSavIndx = ORDER()
  *  INDEX ON CORDTYPE + ORDER + PACK_ID + STR(LINENO,6) TAG PACK_ID OF (gcWorkDir + lcPackIndx + '.CDX')
  *  SET ORDER TO TAG &lcSavIndx
  *  SELECT(lcAliasPk)
  *ENDIF
  *B605561,1 BWA 02/17/2002.[END]

  *C102520,1 BWA 08/01/2001.[END]

  *-- Initialize store scope expression with the field name
  lcBrowExp    = ''
  
  *-- Temporary file name
  lc_TmpOrdL   = gfTempName()    &&temporary file for order lines
  lc_TmpPkLn   = gfTempName()    &&temporary file for released lines
  lc_TmpStyD   = gfTempName()    &&temporary file for STYDYE records
  lcTmpStore   = gfTempName()    &&temporary file for Order + Store.
  *B802829,1 WAB - temp name for temp file for browse dyelot
  *B802829,1 WAB - START
  lcStyDTemp   = gfTempName()    &&temporary file for browse dyelot records
  *B802829,1 WAB - END
  *-- Default warehouse is the first warehouse in the file.
  =gfOpenFile(gcDataDir+"WAREHOUS", "", "SH")
  *-- Browse style warehouses only
  lcDefWare  = WAREHOUS.cWareCode 
  *-- Browse style warehouses only
  LOCATE FOR lStyInv
  IF FOUND() .AND. llMultWare
    SELECT IIF(EMPTY(cDesc),PADR(cWareCode,35),cDesc) AS cDescrip, cWareCode ;
      FROM WAREHOUS ;
     WHERE lStyInv;
      INTO ARRAY laWareHous;
     ORDER BY cDescrip
  ENDIF
  =gfCloseFile("WAREHOUS")
  puWareHous = 0   
    
  *-- Create a new session number
  lcSession = gfSequence('cSession')
  
  *E123070,1 WAM 07/12/2004 Generate 940 EDI transaction for pick tickets directed to 3PL providers.
  IF 'AS' $ gcCmpModules 
    =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
    =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
    =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
  ENDIF
  *E123070,1 WAM 07/12/2004 (End)
ELSE
  =lfSetRelations()
  SELECT ORDLINE
  SET ORDER TO TAG IIF(llOrdLStr, 'ORDLINST', 'ORDLINE') 
  SELECT ORDHDR
  SEEK laData[9] + laData[1]
  IF ORDHDR.Status $ 'CXB'
    laCtrStat[7] = 'DISABLE'
  ENDIF  
  SELECT ORDLINE
  *-- Adjust warehouse popups with the current values
  IF llMultWare
    lnElem       = ASCAN(laWareHous, lcWareCode)
    puWareHous   = IIF(lnElem <> 0, ASUBSCRIPT(laWareHous, lnElem, 1), 0)

    *B802559,1 KHM 03/07/2000 (Begin) Getting the warehouse from the ordline
    *B802559,1                instead of m.cWareCode.
    *lnElem       = ASCAN(laWareHous, lcWareCode)
    lnElem       = ASCAN(laWareHous, OrdLine.cWareCode)
    *B802559,1 KHM 03/07/2000 (End)

    puPickWare   = IIF(lnElem <> 0, ASUBSCRIPT(laWareHous, lnElem, 1), 0)
  ENDIF
ENDIF  

*-- select the screen's base file
SELECT ORDHDR

*-- Store current key settings
PUSH KEY
*-- Trap Alt+B so that it activates the browse screen
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcOrdBrTtl)
PUSH MENU _MSYSMENU

*-- Set the SHOW state variable for window (lcalordal3) if the window
*-- is zoomed or not

*C102282,1 ABD - Add flage to prvent user to allocate qty from this program. [Begin]
*-- This flage to detrimined if this program run into this customer.
llPikNow = .T.
IF ASCAN(laEvntTrig,PADR("LLPIKNOW",10)) <> 0
  llPikNow = .F.
ENDIF  
*C102282,1 ABD [End]

*Ren : if Generating picking tickets, prepare button prompts.
IF llGenPkTk
  lcPkPrmpt  = 'Pi\<ck'
  lcPAlPrmpt = 'Pick \<All'
ELSE
  lcPkPrmpt  = 'Allo\<cate'
  lcPAlPrmpt = 'Allocate \<All'
ENDIF  

lcShowStat = IIF(llZoomed, 'NOSHOW', 'SAME')

DO (gcScrDir + gcWinAppl + '\ALORDAL.SPR')

*-- Release th browse window
RELEASE WINDOWS (lcOrdBrTtl)
*-- Restore menu and key traps
POP MENU _MSYSMENU
POP KEY

*-- Reset filters and relations
SELECT ORDHDR
SET FILTER TO 
SET RELATION TO
SELECT ORDLINE
SET RELATION TO
SELECT STYLE
SET RELATION TO

*-- Close and erase temporary files if quitting
IF glQuitting
  IF USED(lc_TmpOrdL)
    USE IN (lc_TmpOrdL)
  ENDIF
  IF USED(lc_TmpPkLn)
    USE IN (lc_TmpPkLn)
  ENDIF
  IF USED(lc_TmpStyD)
    USE IN (lc_TmpStyD)
  ENDIF
  *B802829,1 WAB - DELETE TEMP FILE
  *B802829,1 WAB - START
  IF USED(lcStyDTemp)
    USE IN (lcStyDTemp)
  ENDIF
  ERASE (gcWorkDir + lcStyDTemp + '.DBF')
  ERASE (gcWorkDir + lcStyDTemp + '.FPT')
  ERASE (gcWorkDir + lcStyDTemp + '.CDX')
  *B802829,1 WAB - END
  ERASE (gcWorkDir + lc_TmpOrdL + '.DBF')
  ERASE (gcWorkDir + lc_TmpOrdL + '.FPT')
  ERASE (gcWorkDir + lc_TmpOrdL + '.CDX')  
  ERASE (gcWorkDir + lc_TmpPkLn + '.DBF')
  ERASE (gcWorkDir + lc_TmpPkLn + '.FPT')
  ERASE (gcWorkDir + lc_TmpPkLn + '.CDX')
  ERASE (gcWorkDir + lc_TmpStyD + '.DBF')
  ERASE (gcWorkDir + lc_TmpStyD + '.FPT')
  ERASE (gcWorkDir + lc_TmpStyD + '.CDX')

  *C102520,1 BWA 08/01/2001 Close the index of the pack_id.[START]

  *B605561,1 BWA 02/17/2002 Comment this code due to I made custom index for GMA in the ordline.[START]
  *IF llGma
  *  SELECT ORDLINE
  *  CLOSE INDEX
  *  ERASE (gcWorkDir + lcPackIndx + '.CDX')
  *ENDIF
  *B605561,1 BWA 02/17/2002.[END]

  *C102520,1 BWA 08/01/2001.[END]

  *C200084,1
  IF ASCAN(laEvntTrig,PADR("CLSSTORE",10)) <> 0
    = gfDoTriger("ALORDAL",PADR("CLSSTORE",10))
  ENDIF  

  *C102810,1 AMH Trigger to clear filter for style file [Start]
  IF ASCAN(laEvntTrig , PADR('CLRFLTER',10)) <> 0
    =gfDoTriger('ALORDAL',PADR('CLRFLTER',10))
  ENDIF
  *C102810,1 AMH [End]
  
  *C123616,1  TMI [Start] Call triger to delete the Temp. files 
  IF ASCAN(laEvntTrig,PADR("CLOSFILS",10)) <> 0
    =gfDoTriger("POSTREC",PADR("CLOSFILS",10))
  ENDIF
  *C123616,1  TMI [End  ] 

ENDIF  

*-- Release defined popups and pads
RELEASE POPUPS _OPTIONPOP
RELEASE PAD _OPTION OF _MSYSMENU

*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Browse order lines and transation applied to the order
*!*************************************************************
*! Calls     : lfPrepBrow
*!*************************************************************
*! Passed Parameters  :  llRebuild : .T. to rebuild data files
*!                       lcBrow    : 'O' browse order lines only
*!                                   'T' browse transactions only
*!                                   'B' browse both
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOrdBrow()
*!*************************************************************
FUNCTION lfOrdBrow
*B603240,1 Add a private variable to save RESOURCE setting
PRIVATE lcSetReso
*B603240,1 end

SELECT ORDLINE

*C102520,1 BWA 08/01/2001 Select the index of the pack_id in case GMA.[START]
IF llGma
  PRIVATE lcOldOrdrPk
  lcOldOrdPk = ORDER()
  SET ORDER TO TAG PACK_ID
ENDIF
*C102520,1 BWA 08/01/2001.[END]

IF !BETWEEN(lnMarker, 1, RECCOUNT())
  lnMarker = RECNO()
ENDIF
*BROWSE FIELDS ;
    &lcOrderBr;
    nAvail=lfGetTotStk():H='Avail.':P='999999':R, &lcOrderBr2 ;
	FOR &lcNdxExp = laData[9] + laData[1] + IIF(EMPTY(lcStore), STORE, lcStore);
				    + ALLTRIM(lcStyle);
				    .AND. Group = ALLTRIM(lcGroup);
	WINDOW (lcAlOrdal1);
    IN WINDOW (gcBaseWind);
    NOMENU           ;         
    NOAPPEND         ;
    NOEDIT           ;
    NODELETE         ;
    SAVE             ;
    NOWAIT           ;
    NOCLEAR          ;    
    WHEN lnMarker <> RECNO() .AND. lfChkEntry() .AND. lfwOrdBrow();
    VALID :F WONTOP()=lcOrdBrTtl .OR. gfStopBrow() ;
    TITLE lcOrdBrTtl          

*BROWSE FIELDS ;
    &lcOrderBr;
    nAvail=lfGetTotStk():H='Avail.':P='999999':R, &lcOrderBr2 ;
	FOR &lcNdxExp = laData[9] + laData[1] + ALLTRIM(lcStore) + ALLTRIM(lcStyle);
				    .AND. Group = ALLTRIM(lcGroup);
	WINDOW (lcAlOrdal1);
    IN WINDOW (gcBaseWind);
    NOMENU           ;         
    NOAPPEND         ;
    NOEDIT           ;
    NODELETE         ;
    SAVE             ;
    NOWAIT           ;
    NOCLEAR          ;    
    WHEN lnMarker <> RECNO() .AND. lfChkEntry() .AND. lfwOrdBrow();
    VALID :F WONTOP()=lcOrdBrTtl .OR. gfStopBrow() ;
    TITLE lcOrdBrTtl          

*	FOR &lcNdxExp = laData[9] + laData[1] + store + ALLTRIM(lcStyle);
				    .AND. Group = ALLTRIM(lcGroup);


*B603240,1 Set resource off before browsing to avoid fields missing from the browse
lcSetReso = SET("RESOURCE")
SET RESOURCE OFF
*B603240,1 end

*MAB
*BROWSE FIELDS ;
    &lcOrderBr;
    nAvail=lfGetTotStk():H='Avail.':P='999999':R, &lcOrderBr2 ;
	FOR &lcNdxExp = laData[9] + laData[1] &lcBrowExp;
	WINDOW (lcAlOrdal1);
    IN WINDOW (gcBaseWind);
    NOMENU           ;         
    NOAPPEND         ;
    NOEDIT           ;
    NODELETE         ;
    SAVE             ;
    NOWAIT           ;
    NOCLEAR          ;    
    WHEN lnMarker <> RECNO() .AND. lfChkEntry() .AND. lfwOrdBrow();
    VALID :F WONTOP()=lcOrdBrTtl .OR. gfStopBrow() ;
    TITLE lcOrdBrTtl 

lcAvlAlias = IIF(EOF(lc_TmpStyD),"STYDYE",lc_TmpStyD)
*B803406,1 HBG 07/30/2000 change the position of the calculated field "nAvail" in the browes[Begin]
*BROWSE FIELDS ;
    &lcOrderBr;
    nAvail=MAX(&lcAvlAlias..TotStk - &lcAvlAlias..TotAlo +;
             IIF(llTotAvlbl,IIF(EMPTY(&lcAvlAlias..Dyelot), &lcAvlAlias..TotWIP,;
             lfGetWIP('9')),0),0) :H='Avail.':P='999999':R ,&lcOrderBr2 ;
  FOR &lcNdxExp = laData[9] + laData[1] &lcBrowExp;
	WINDOW (lcAlOrdal1);
    IN WINDOW (gcBaseWind);
    NOMENU           ;         
    NOAPPEND         ;
    NOEDIT           ;
    NODELETE         ;
    SAVE             ;
    NOWAIT           ;
    NOCLEAR          ;    
    WHEN lnMarker <> RECNO() .AND. lfChkEntry() .AND. lfwOrdBrow();
    VALID :F WONTOP()=lcOrdBrTtl .OR. gfStopBrow() ;
    TITLE lcOrdBrTtl 
*B803675,1 HBG 09/21/2000 update the avaliabel field by calling 
*B803675,1                "Get total stock function 'lfGetTotStk()'" [Begin]   
*BROWSE FIELDS ;
    &lcOrderBr;
    &lcOrderBr2,;
    nAvail=MAX(&lcAvlAlias..TotStk - &lcAvlAlias..TotAlo +;
             IIF(llTotAvlbl,IIF(EMPTY(&lcAvlAlias..Dyelot), &lcAvlAlias..TotWIP,;
             lfGetWIP('9')),0),0) :H='Avail.':P='999999':R ,&lcOrderBr3 ;
    FOR &lcNdxExp = laData[9] + laData[1] &lcBrowExp;
	WINDOW (lcAlOrdal1);
    IN WINDOW (gcBaseWind);
    NOMENU           ;         
    NOAPPEND         ;
    NOEDIT           ;
    NODELETE         ;
    SAVE             ;
    NOWAIT           ;
    NOCLEAR          ;    
    WHEN lnMarker <> RECNO() .AND. lfChkEntry() .AND. lfwOrdBrow();
    VALID :F WONTOP()=lcOrdBrTtl .OR. gfStopBrow() ;
    TITLE lcOrdBrTtl
 
 **Remove lfChkEntry() from thw when of the browse
 *BROWSE FIELDS ;
    &lcOrderBr;
    &lcOrderBr2,;
    nAvail=lfGetTotStk():H='Avail.':P='999999':R ,&lcOrderBr3 ;
  FOR &lcNdxExp = laData[9] + laData[1] &lcBrowExp;
	WINDOW (lcAlOrdal1);
    IN WINDOW (gcBaseWind);
    NOMENU           ;         
    NOAPPEND         ;
    NOEDIT           ;
    NODELETE         ;
    SAVE             ;
    NOWAIT           ;
    NOCLEAR          ;    
    WHEN lnMarker <> RECNO() .AND. lfChkEntry() .AND. lfwOrdBrow();
    VALID :F WONTOP()=lcOrdBrTtl .OR. gfStopBrow() ;
    TITLE lcOrdBrTtl  

  *B804502,1 HBG 29/10/2001 call lfGetTotStk() with parameter to calculate ;
  *B804502,1                total availabel in case of brows[Begin]
  *BROWSE FIELDS ;
    &lcOrderBr;
    &lcOrderBr2,;
    nAvail=lfGetTotStk():H='Avail.':P='999999':R ,&lcOrderBr3 ;
  FOR &lcNdxExp = laData[9] + laData[1] &lcBrowExp;
	WINDOW (lcAlOrdal1);
    IN WINDOW (gcBaseWind);
    NOMENU           ;         
    NOAPPEND         ;
    NOEDIT           ;
    NODELETE         ;
    SAVE             ;
    NOWAIT           ;
    NOCLEAR          ;    
    WHEN lnMarker <> RECNO() .AND. lfwOrdBrow();
    VALID :F WONTOP()=lcOrdBrTtl .OR. gfStopBrow() ;
    TITLE lcOrdBrTtl  
  BROWSE FIELDS ;
    &lcOrderBr;
    &lcOrderBr2,;
    nAvail=lfGetTotStk('T'):H='Avail.':P='999999':R ,&lcOrderBr3 ;
  FOR &lcNdxExp = laData[9] + laData[1] &lcBrowExp;
	WINDOW (lcAlOrdal1);
    IN WINDOW (gcBaseWind);
    NOMENU           ;         
    NOAPPEND         ;
    NOEDIT           ;
    NODELETE         ;
    SAVE             ;
    NOWAIT           ;
    NOCLEAR          ;    
    WHEN lnMarker <> RECNO() .AND. lfwOrdBrow();
    VALID :F WONTOP()=lcOrdBrTtl .OR. gfStopBrow() ;
    TITLE lcOrdBrTtl    
  *B804502,1[End] 
*B803675,1 [End]      
*B803406,1 [End] 

*B603240,1 Restore RESOURCE setting
SET RESOURCE &lcSetReso
*B603240,1 end

IF BETWEEN(lnMarker, 1, RECCOUNT('ORDLINE'))
  GO lnMarker IN ORDLINE
ENDIF  
IF EOF(lc_TmpOrdL)
  SELECT ORDLINE
  SCATTER MEMVAR MEMO
*Ren/Gehan
*  m.lForceAlc = .F.
m.lForceAlc = .F.
*Ren/Gehan
ELSE
  SELECT (lc_TmpOrdL)
  SCATTER MEMVAR MEMO
ENDIF
SELECT ORDHDR

*!*************************************************************
*! Name      : lpShow
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Validate order number
*!*************************************************************
*! Calls     : ARIABROW,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvOrder()
*!*************************************************************
PROCEDURE lpShow


*MAB HERE [Begin]
ACTIVATE WINDOW gwcContrl1		&& Activate control panel window
SHOW GET PBUSRFIELD DISABLE
*MAB HERE [End  ]

DO CASE
  CASE laScrMode[1]
    *-- Store current screen mode in a variable
    DEFINE BAR 4  OF _OPTIONPOP PROMPT 'Sort by Style' SKIP FOR laScrMode[1]
    
    *-- Clear all variables
    STORE '' TO lcCustName, lcOldVal, lcStatus,lcWareCode 
   
    STORE 0   TO lnMarker  
    STORE .F. TO cbMultiStore 
    *-- Adjust warehouses
    STORE lcDefWare TO lcWareCode
    IF llMultWare
      lnElem     = ASCAN(laWareHous, lcDefWare)
      STORE IIF(lnElem <> 0, ASUBSCRIPT(laWareHous, lnElem, 1), puWareHous);
         TO puWareHous, puPickWare
    ENDIF
   
    laData[9]  = 'O'
    
    GO BOTTOM IN ORDHDR
    SKIP

    SELECT (lc_TmpOrdL)
    SCATTER MEMVAR MEMO BLANK
    
    *-- Refresh display
    SHOW GET pbPick DISABLE
    SHOW GET pbRel  DISABLE
    SHOW GET pbOrdNote DISABLE
    SHOW GET pbObjLink DISABLE
    SHOW GET pbCustInq DISABLE
    SHOW GET pbStyInq  DISABLE
    SHOW GET pbScope   DISABLE
    =lfRefresh()
    SHOW WINDOW (lcOrdBrTtl) REFRESH 
    lcSame  = '' 
    
    *-- Check for an incomplete session every 'Select mode' except if coming from the setup 
    IF llChkUnCm AND lfChkUnCmS()
      RETURN
    ENDIF

    *C102810,1 AMH Trigger to clear filter for style file [Start]
    IF ASCAN(laEvntTrig , PADR('CLRFLTER',10)) <> 0
      =gfDoTriger('ALORDAL',PADR('CLRFLTER',10))
    ENDIF
    *C102810,1 AMH [End]
    
    *C123616,1  TMI [Start] Triggre to zap file lcCrtnDet for Nik Nak
    IF ASCAN(laEvntTrig , PADR('ZPCRTDET',10)) <> 0
      =gfDoTriger('ALORDAL',PADR('ZPCRTDET',10))
    ENDIF
    *C123616,1  TMI [End  ] 
  
  CASE laScrMode[2]
    
    *-- The next time we get into Select mode, we should check for an incomplete session
    llChkUnCm  = .T.

    *-- refresh relations
    SELECT ORDHDR
    IF BETWEEN(RECNO(),1,RECCOUNT())
      GO RECNO()
    ENDIF  
    *lcCustName    = IIF(SEEK(IIF(STORE=SPACE(8),'M'+ACCOUNT,'S'+ACCOUNT+STORE),;
                       'CUSTOMER'), CUSTOMER.STName, '')
    lcCustName     = CUSTOMER.STName
    *lcStatus      = IIF(ORDHDR.Status $ 'OH', laStatus[AT(ORDHDR.Status,'OH')], '')
    lcStatus       = IIF(ORDHDR.Status $ 'OHCXB', laStatus[AT(ORDHDR.Status,'OHCXB')], '')
    IF ORDHDR.Status $ 'CXB'
      SHOW GET pbEdt DISABLE
    ELSE
      SHOW GET pbEdt ENABLE
    ENDIF
    lcWareCode     = IIF(llMultWare, ORDHDR.cWareCode, lcDefWare)
    IF llMultWare
      lnElem       = ASCAN(laWareHous, lcWareCode)
      puWareHous   = IIF(lnElem <> 0, ASUBSCRIPT(laWareHous, lnElem, 1), 0)
    ENDIF
    *lcStkAlias     = 'STYDYE'

    SHOW GET cbMultiStore

    *-- Refresh display
    SHOW GET pbPick DISABLE
    SHOW GET pbRel  DISABLE
    SHOW GET pbOrdNote ENABLE
    SHOW GET pbObjLink ENABLE
    SHOW GET pbCustInq ENABLE
    SHOW GET pbStyInq  ENABLE
    SHOW GET pbScope   ENABLE
  
    STORE 'DISABLE' TO lcPickStat, lcRelStat, lcStyStat, lcForcStat

    IF ORDHDR.Multi = 'Y'
      cbMultiStore = .T.
      DEFINE BAR 4  OF _OPTIONPOP PROMPT 'Sort by Store' SKIP FOR laScrMode[1] 
    ELSE
      cbMultiStore = .F.
      DEFINE BAR 4  OF _OPTIONPOP PROMPT 'Sort by Style' SKIP FOR laScrMode[1] 
    ENDIF  
    SET MARK OF BAR 4 OF _OPTIONPOP TO llOrdLStr
    *MAN
    *STORE .F. TO llGenPkTk, llForceAlc

    *-- E301225,1 Allow Force allocation option. [Begin]
    *SET MARK OF BAR 7 OF _OPTIONPOP TO llForceAlc
    IF llForceBar
      SET MARK OF BAR lnForceBar OF _OPTIONPOP TO llForceAlc
    ENDIF  
    *-- E301225,1 Allow Force allocation option. [End  ]

    *C200084,1 Do this line if llGenBar was hited. [Begin]

    *C102282,1 ABD - Remark the next line and allow force alloction 
    *C102282,1 ABD - If the llPinNow flage is .T.  [Begin]
    *IF llGenBar
    IF llGenBar .AND. llPikNow
      *C102282,1 ABD - [End]

      *-- E301225,1 Allow Force allocation option. [Begin]
      *SET MARK OF BAR 8 OF _OPTIONPOP TO llGenPkTk
        SET MARK OF BAR lnPickBar OF _OPTIONPOP TO llGenPkTk
      *-- E301225,1 Allow Force allocation option. [End  ]
    ENDIF  
    *C200084,1 Do this line if llGenBar was hited. [End  ]

    =lfShowPkBt()
    IF lcLastMode = 'E' 
      lcLastMode = ''
      *lcSame     = 'SAME'        
      lcSame  = '' 
      =lfwOrdBrow()  
    ELSE  
      IF !EMPTY(lcBrowExp)
        =lfClrScope()
        =lfOrdBrow()
      ELSE
        *lcSame     = 'SAME'        
        lcSame  = '' 
        *B804004,1 (Begin) Get the dyelots for the new order got by pressing Next butt.
        =lfGetStyDy()
        *B804004,1 (End)
        =lfwOrdBrow()  
      ENDIF  
    ENDIF
       
    *C102810,1 AMH Trigger to set filter for style file [Start]
    IF ASCAN(laEvntTrig , PADR('STYFLTER',10)) <> 0
      =gfDoTriger('ALORDAL',PADR('STYFLTER',10))
    ENDIF
    *C102810,1 AMH [End]
    
  CASE laScrMode[3]
    *B603318,1 Start, 
    *--Custom Process in editing to allocate.
    IF ASCAN(laEvntTrig,PADR("UPDTHDR",10)) <> 0
      llOrdPktk =.F.  && Some lines already has piktkt.
      = gfDoTriger("ALORDAL",PADR("UPDTHDR",10))
      IF llOrdPktk
        RETURN .F.
      ENDIF 
    ENDIF  
    *B603318,1 End.

    lcLastMode = 'E'
    *-- Create an "Open" record in the uncomplete session file.
    IF lnUnCmSeRc = 0
      =lfAdUnCmSR()
    ENDIF  
    IF BETWEEN(RECNO('ORDHDR'),1,RECCOUNT())
      GO RECNO('ORDHDR') IN ORDHDR
    ENDIF  
    IF BETWEEN(lnMarker, 1, RECCOUNT('ORDLINE'))
      GO lnMarker IN ORDLINE
    ENDIF  
    =lfwOrdBrow()  

    *-- Insert a record with the new session in UNCMSESS,
    *-- if one does not exist.
    lnMinAlo  = 0
    
    *C200084,1
    IF ASCAN(laEvntTrig,PADR("FILLSTORE",10)) <> 0
      = gfDoTriger("ALORDAL",PADR("FILLSTORE",10))
    ENDIF  

    *E301310,1 Save Approval Values before go on [Begin]
    IF llChkAprov
      lcHdrKey   = OrdHdr.cOrdType + OrdHdr.Order
      lnChkAprov = lfChkAprov(lcHdrKey)
    ENDIF  
    *E301310,1 Save Approval Values before go on [End  ]

    *B802756,4 Select unallocated lines if "Picking All"
    =lfSelectLn()
    *B802756,4 end
  
    *C200489,1 WAB (Start)
    IF ASCAN(laEvntTrig,PADR("OLDPICKD",10)) <> 0
      =gfDoTriger("ALORDAL",PADR("OLDPICKD",10))
    ENDIF
    *C200489,1 WAB (End)

      
ENDCASE  

*-- Disable Delete buttons always
SHOW GET pbDlt DISABLE
*B605971,1 ASH 05/22/2002 (Begin) Stop using CTRL+D to prevent deleting the ORDHDR record.
DEFINE BAR 10 OF P03PU03 PROMPT "\<Delete" KEY CTRL+D,"^D" SKIP FOR .T.
*B605971,1 ASH 05/22/2002 (End)
SELECT ORDHDR

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Validate order number
*!*************************************************************
*! Calls     : gfBrows()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvData_1()
*!*************************************************************
FUNCTION lfvData_1
IF ISDIGIT(LEFT(ALLTRIM(laData[1]), 1))
  laData[1] =  PADL(ALLTRIM(laData[1]), 6, '0')
ENDIF  
IF !llBrowse 
  IF EMPTY(laData[1])
    RETURN
  ELSE
    IF !EMPTY(laData[2]) 
      lcSeekExp = laData[2] + laData[9] + laData[1]
      SET ORDER TO TAG ORDACCT 
    ELSE
      lcSeekExp = laData[9] + laData[1]
    ENDIF     
    IF !SEEK(lcSeekExp)
      llBrowse   = .T.
      lnSoftSeek = RECNO(0)
      IF BETWEEN(lnSoftSeek, 1, RECCOUNT())
        GO lnSoftSeek
      ENDIF
      *B804269,1 TMI [Start] if account is Hold do not accept his orders
    ELSE
      IF SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER') AND CUSTOMER.STATUS = 'H'
        lcStatus = 'on Hold' + '|' + 'Cannot Pick or Allocate'
        laData[2] = CUSTOMER.ACCOUNT
        SHOW GET laData[2]
        =gfModalGen("INM44098B00000" , "DIALOG" , lcStatus)        
        laScrMode    = .F.
        laScrMode[1] = .T.
        SHOW GETS
        llBrowse = .F.
        RETURN
      ENDIF
      *B804269,1 TMI [End  ] if account is Hold do not accept his orders    
    ENDIF  
  ENDIF  
ENDIF
IF llBrowse 
  IF !EMPTY(laData[2]) 
    SET ORDER TO TAG ORDACCT
   =gfBrows([laData(2) + laData(9)])  
  ELSE
    =gfBrows([laData(9)])  
    *B804269,1 TMI [Start] if account is Hold do not accept his orders
    IF SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER') AND CUSTOMER.STATUS = 'H'
      lcStatus = 'on Hold' + '|' + 'Cannot Pick or Allocate'
      =gfModalGen("INM44098B00000" , "DIALOG" , lcStatus)
      laScrMode    = .F.
      laScrMode[1] = .T.
      SHOW GETS
      llBrowse = .F.
      RETURN
    ENDIF
    *B804269,1 TMI [End  ] if account is Hold do not accept his orders
  ENDIF
ELSE
  IF !EMPTY(laData[1])
    SCATTER FIELDS &lcScFields MEMO TO laData
    laScrMode    = .F.
    laScrMode[2] = .T.
    SHOW GETS
  ENDIF
ENDIF  
*B802938,1 WAB - check if the order status on hold and display message 
*B802938,1 WAB - "the status on hold continue yes/no? " 
IF laData[3] = 'H'
  *Message : 44096 ==> Order status on hold ! Continue ?.
  *Buttons : 44009 ==> < Yes > ,  < No >
  
  *B803380,1 Passed parameter to on hold message must be Null. [Begin]
  *lnAnsr=gfModalGen('INM44096B44009','DIALOG',ALLTRIM(laData[2]))
  lnAnsr=gfModalGen('INM44096B44009','DIALOG',"")
  *B803380,1 Passed parameter to on hold message must be Null. [End  ]
  
  *--if the user choose to not continue , return to select mode
  IF lnAnsr = 2     
    laScrMode    = .F.
    laScrMode[1] = .T.
    SHOW GETS
  ENDIF
ENDIF
*B802938,1 WAB - END
*B122202,1 NNA 03/25/2004 (Begin) check if the order is Bulk or not and display message 
IF OrdHdr.Bulk='Y'
  *Message : 44121 ==> Order is a bulk ! Continue ?.
  *Buttons : 44009 ==> < Yes > ,  < No >
  lnAnsr=gfModalGen('INM44121B44009','DIALOG',OrdHdr.order)
  *--if the user choose to not continue , return to select mode
  IF lnAnsr = 2     
    laScrMode    = .F.
    laScrMode[1] = .T.
    SHOW GETS
  ENDIF
ENDIF
*B122202,1 NNA (End)


SET ORDER TO TAG ORDHDR
llBrowse = .F.
*B802829,1 KHM 03/06/2000 (Begin) Calling lfGetStyDy function in order
*B802829,1                to get all the dyelots of the styles that are 
*B802829,1                included in the selected order and do have dyelots.
=lfGetStyDy()
*B802829,1 KHM 03/06/2000 (End)

*!*************************************************************
*! Name      : lfvData_2
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Validate customer code
*!*************************************************************
*! Calls     : CUSBROWM,gfGetAdr,ARIABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvAccount()
*!*************************************************************
*!B306084,1 RAM   08/02/99 Validate customer status if ON HOLD or not
*!*************************************************************
FUNCTION lfvData_2
PRIVATE lcAccount
IF !llBrowse .AND. EMPTY(laData[2])
  lcCustName = ''
  =lfRefresh(lcAlOrdAl0)
  RETURN
ENDIF  

laData[2] = PADR(ALLTRIM(laData[2]), 5)
IF '?' $ laData[2]
  laData[2] = ''
  llBrowse  = .T.
ENDIF  
IF llBrowse .OR. laData[2]<>lcOldVal
  lcAccount = laData[2]
  *Ren
  SELECT ORDHDR
  SET RELATION OFF INTO CUSTOMER

  IF (llBrowse  .OR. !EMPTY(laData[2])) .AND. CUSBROW(@lcAccount, 'M', '', .T.)

    *B306084,1 RAM [BIGEN]
    *SELECT CUSTOMER
    
    ** Loop looks for the customer status
    *SCAN FOR Account = (lcAccount) 
    *  LOCATE WHILE Account = (lcAccount) FOR Status <> 'A'
    *  IF FOUND()
    *  =gfModalGen('TRM32023B00000','ALERT','account') && Message box 
    *  _CUROBJ = _CUROBJ
    *  RETURN
    *ENDIF
    *ENDSCAN
    *B306084,1 RAM [END]
    *B804269,1 TMI [Start] if account is hold , then refuse
    IF CUSTOMER.STATUS = 'H'
      laData[2] = CUSTOMER.ACCOUNT
      SHOW GET laData[2]
      lcStatus = 'on Hold' + '|' + 'Cannot Pick or Allocate'
      =gfModalGen("INM44098B00000" , "DIALOG" , lcStatus)
      laScrMode    = .F.
      laScrMode[1] = .T.
      SHOW GETS
      llBrowse = .F.
      RETURN
    ENDIF
    *B804269,1 TMI [End  ] if account is hold , then refuse
    
    SELECT ORDHDR
    SET RELATION TO IIF(STORE=SPACE(8),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE

    laData[2] = lcAccount
    SHOW GET laData[2]
    SET ORDER TO TAG ORDACCT IN ORDHDR
    IF !SEEK(laData[2], 'ORDHDR')
      SET ORDER TO TAG ORDHDR IN ORDHDR
      *-- Message : No open orders found for account# ð.
      =gfModalGen('INM44038B00000','DIALOG',ALLTRIM(laData[2]))
      laData[2] = SPACE(5)
      _CUROBJ   = _CUROBJ
    ELSE  
      lcCustName = CUSTOMER.StName  
      llBrowse = .T.
      SET ORDER TO TAG ORDHDR IN ORDHDR
      =lfvData_1()
    ENDIF
  ELSE  
    SELECT ORDHDR
    SET RELATION TO IIF(STORE=SPACE(8),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE

    laData[2] = lcOldVal
    _CUROBJ   = _CUROBJ      
  ENDIF  
  llBrowse     = .F.
  =lfRefresh(lcAlOrdAl0)
ENDIF  
RETURN

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Activate function of INV700
*!*************************************************************
*! Calls     : gfClearKey.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
*-- Restore key settings
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcOrdBrTtl)
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
*-- Clear browse as you are coming out of a browse window
KEYBOARD '{ALT+B}' CLEAR PLAIN
IF glFromBrow
  =gfStopBrow()
  SHOW WINDOW (lcOrdBrTtl) REFRESH
ENDIF  
SELECT ORDHDR
*IF WEXIST(lcOrdBrTtl)
*ENDIF   

*C123847,1  TMI [Start] open the screen with Pick button activated for DIR03
IF ASCAN(laEvntTrig,'PKBTN')<>0
  =gfDoTriger('ALORDAL','PKBTN')
ENDIF  
*C123847,1  TMI [End  ] 

*B132130,1  TMI [Start] redefine release _OPTIONPOP , this happens when module is changed and return back
PRIVATE lcError,llDefPop
lcError = ON('ERROR')
llDefPop = .F.
ON ERROR llDefPop = .T.
=GETBAR('_OPTIONPOP',1)
ON ERROR &lcError
IF llDefPop
  =lfActPad()
ENDIF
*B132130,1  TMI [End  ] 


*-- end of lfReadAct.


*!*************************************************************
*! Name      : lfReadDAct
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : READ Deactivate function of screen Ord100
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfReadDAct()
*!*************************************************************
FUNCTION lfReadDAct
ON KEY LABEL ALT+B
IF WONTOP() = lcOrdBrTtl
  *-- Set browsing variable
  glFromBrow  = .T.
  *-- Trap keys
  ON KEY LABEL TAB         DO lpTab WITH lcAlOrdal2, OBJNUM(pbPick)
  IF laScrMode[2] .OR. laScrMode[3] .AND. !llMultWare
    ON KEY LABEL BACKTAB   DO lpTab WITH 'gwcContrl1', OBJNUM(pbCls)
  ELSE
    ON KEY LABEL BACKTAB   DO lpTab WITH lcAlOrdAl0, ;
    						  IIF(laScrMode[3] .AND. llMultWare,;
    						    OBJNUM(puWareHous), OBJNUM(laData[2]))
  ENDIF 
ENDIF

*wait window "Deactiviate" timeout 3
*B123702,1  TMI [Start] Comment these lines, it prevents any other screen to be activated if Alordal is open
*KEYBOARD '{ALT+B}' CLEAR PLAIN
*RETURN .F.
*B123702,1  TMI [End  ] 

*!*************************************************************
*! Name      : lpTab
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'ORD100P', 'pbApply'
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lnObjNum
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum


*!*************************************************************
*! Name      : lfvZoom
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Zoom function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvZoom()
*!*************************************************************
FUNCTION lfvZoom
IF llZoomed
  llZoomed = .F.
  *-- Change the size of the browse window
  ZOOM WINDOW (lcAlOrdal1) NORM;
  		 SIZE WROWS(lcAlOrdal1) - lnSizOffst , WCOLS(lcAlOrdal1)  	  		 
  ZOOM WINDOW (lcOrdBrTtl) NORM;
  		 SIZE WROWS(lcAlOrdAl1) + 1.3,  WCOLS(lcAlOrdal1) + 3.3  	  		 
  =lfRefresh(gcBaseWind)
  *-- Move buttons window up
  MOVE WINDOW (lcAlOrdAl2) BY - (lnSizOffst + .9) - 0.083, 0
  SHOW WINDOW (lcAlOrdAl3)  
  =lfwOrdBrow()
ELSE
  llZoomed = .T.
  *-- Hide details window
  SHOW GETS WINDOW (lcAlOrdAl3) DISABLE ONLY
  STORE 'DISABLE' TO lcPickStat, lcRelStat, lcStyStat, lcForcStat
  
  HIDE WINDOW (lcAlOrdAl3)
  *-- Move buttons window down
  MOVE WINDOW (lcAlOrdAl2) BY lnSizOffst + .9, 0
  *-- Change the size of the browse window
  ZOOM WINDOW (lcAlOrdal1) NORM;
  		 SIZE WROWS(lcAlOrdal1) + lnSizOffst , WCOLS(lcAlOrdal1)  	  
  ZOOM WINDOW (lcOrdBrTtl) NORM;
 		 SIZE WROWS(lcAlOrdAl1) + 1.3, WCOLS(lcAlOrdal1) + 3.3  	  		 
  =lfRefresh(gcBaseWind) 		 
ENDIF  

*!*************************************************************
*! Name      : lfwOrdBrow
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : When function for order lines and transations browses
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwOrdBrow()
*!*************************************************************
FUNCTION lfwOrdBrow

lnCurAlias = SELECT(0)
*-- If there are no records in the temp table,
IF EOF(lc_TmpOrdL)
  SELECT ORDLINE
  SCATTER MEMVAR MEMO
  m.lVldWare  = !llMultWare
  m.lVldDye   = !llUseDyes .OR. STYLE.cDye_flg = 'N'
  *Ren/Gehan
  *m.lForceAlc = .F.
  m.lForceAlc = .F.
  *Ren/Gehan
  m.nForcdQty = 0                         
ELSE
  SELECT (lc_TmpOrdL)
  SCATTER MEMVAR MEMO
ENDIF
*m.lForceAlc = lfIsForced()
IF llMultWare
  lnElem     = ASCAN(laWareHous, m.cWareCode)
  puPickWare = IIF(lnElem <> 0, ASUBSCRIPT(laWareHous, lnElem, 1), 0)
ENDIF  

lcAltStyDsc  = IIF(EMPTY(m.AltStyle), '',;
                  LOOKUP(STYLE.Desc1, m.AltStyle, STYLE.Style, 'STYLE'))
=SEEK(m.Style,'STYLE')                  
SELECT (lnCurAlias)


IF laScrMode[3]
  lcStkAlias = IIF(EOF(lc_TmpStyD), 'STYDYE', lc_TmpStyD)
*  m.lForceAlc = lfIsForced()
  =lfShowLines()
ELSE
*  m.lForceAlc = .F.
  STORE 'DISABLE' TO lcPickStat, lcRelStat, lcStyStat, lcForcStat
  lcStkAlias = 'STYDYE'
  SHOW GETS WINDOW (lcAlOrdAl3) DISABLE ONLY
  =lfRefresh()
ENDIF
STORE .F. TO llChngOrd, llForcLAlc
lnMarker   = RECNO('ORDLINE')  
*LCSTORE = ['STORE']
SHOW WINDOW (lcOrdBrTtl) REFRESH &lcSame

*!*************************************************************
*! Name      : lfActPad
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : lpvInquiry
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActPad()
*!*************************************************************
FUNCTION lfActPad
*-- Options menu pad definition
DEFINE PAD _OPTION OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P,SPACE(1)
ON PAD _OPTION OF _MSYSMENU ACTIVATE POPUP _OPTIONPOP

*-- Options popup definition
DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1  OF _OPTIONPOP PROMPT 'Customer Inquire' SKIP FOR laScrMode[1]
DEFINE BAR 2  OF _OPTIONPOP PROMPT 'Style Inquire' SKIP FOR laScrMode[1]
DEFINE BAR 3  OF _OPTIONPOP PROMPT 'Sort by Line#' SKIP FOR laScrMode[1] .OR. !EMPTY(lcBrowExp) 
DEFINE BAR 4  OF _OPTIONPOP PROMPT 'Sort by Store' SKIP FOR laScrMode[1] 
DEFINE BAR 5  OF _OPTIONPOP PROMPT 'Scope on...'   SKIP FOR laScrMode[1]
DEFINE BAR 6  OF _OPTIONPOP PROMPT 'Zoom Order Lines'       

*-- E301225,1 Allow Force allocation option. [Begin]
*DEFINE BAR 7  OF _OPTIONPOP PROMPT 'Always Force Allocation' SKIP FOR !laScrMode[3]      
*C200084,1 Do this line if llGenBar was hited. [Begin]
*IF llGenBar
*  DEFINE BAR 8  OF _OPTIONPOP PROMPT 'Generate Picking Ticket' SKIP FOR !laScrMode[3]
*ENDIF  
*SET MARK OF BAR 7 OF _OPTIONPOP TO llForceAlc
*IF llGenBar
*  SET MARK OF BAR 8 OF _OPTIONPOP TO llGenPkTk
*ENDIF  
*C200084,1 Do this line if llGenBar was hited. [Begin]

STORE 7 TO lnForceBar , lnPickBar
llForceBar = lfForceOpt()
IF llForceBar
  
  *B039660,1 NNA 02/14/2006 (BEGIN) Disable "Always Force Allocation" Option if use bin location/Yes
  IF ASCAN(laEvntTrig,'CHKFORCE')<>0 AND gfDoTriger('ALORDAL','CHKFORCE')
    DEFINE BAR lnForceBar OF _OPTIONPOP PROMPT 'Always Force Allocation' SKIP FOR !laScrMode[3] OR gfGetMemVar('M_DLUSEBIN')
  ELSE
  *B039660,1 NNA (END)

    DEFINE BAR lnForceBar OF _OPTIONPOP PROMPT 'Always Force Allocation' SKIP FOR !laScrMode[3]      

  *B039660,1 NNA (BEGIN)
  ENDIF
  *B039660,1 NNA (END)
  
  SET MARK OF BAR lnForceBar OF _OPTIONPOP TO llForceAlc
  lnPickBar  = lnPickBar + 1
ELSE
  lnForceBar = 0
ENDIF

*C102282,1 ABD - Remark the next line and allow force alloction 
*C102282,1 ABD - The llPinNow flage is .T.If  [Begin]
*IF llGenBar
IF llGenBar .AND. llPikNow
  *C102282,1 ABD - [End]

  DEFINE BAR lnPickBar OF _OPTIONPOP PROMPT 'Generate Picking Ticket' SKIP FOR !laScrMode[3]
  SET MARK OF BAR lnPickBar OF _OPTIONPOP TO llGenPkTk
ELSE
  lnPickBar = 0
ENDIF  
*E301340,1 WAB - set the sequence bar no (order shaving bar)
*E301340,1 WAB - Start
lnShavBar = IIF(lnPickBAr=0,IIF(lnForceBar=0,7,lnForceBar+1),lnPickBar+1)
DEFINE BAR lnShavBar OF _OPTIONPOP PROMPT 'Order Shaving' 
*E301340,1 WAB - END

*C123616,1  TMI [Start] Define bar 'Pick Cartons' for Nik Nak
IF ASCAN(laEvntTrig,'ADBRPICK')<>0
  =gfDoTriger('ALORDAL','ADBRPICK')
ENDIF  
*C123616,1  TMI [End  ] 

*C123847,1  TMI [Start] Define bar 'Open order' for Dir03
IF ASCAN(laEvntTrig,'OPNORDBR')<>0
  =gfDoTriger('ALORDAL','OPNORDBR')
ENDIF  
*C123847,1  TMI [End  ] 

*-- E301225,1 Allow Force allocation option. [Begin]

SET MARK OF BAR 3 OF _OPTIONPOP TO !llOrdLStr
SET MARK OF BAR 4 OF _OPTIONPOP TO llOrdLStr
SET MARK OF BAR 6 OF _OPTIONPOP TO llZoomed

ON SELECTION POPUP _OPTIONPOP DO lfvOptions WITH BAR()

*!*************************************************************
*! Name      : lfvOptions
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : lpvInquiry,lfvSortBy,lfvQtyPSize,lfvDetails,lfvLNote,;
*!             lfvCopyQty,lfvPasteQty,lfvBrowZoom
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpvInquiry()
*!*************************************************************
FUNCTION lfvOptions
PARAMETERS lnBarNo
PRIVATE lnCurAlias

lnCurALias = SELECT(0)
DO CASE
  CASE lnBarNo = 1      && Customer Inquire
    =lfvCustInq()
    
  CASE lnBarNo = 2      && Style Inquire
    =lfvStyInq()
  
  CASE lnBarNo = 3 .AND. llOrdLStr 
    SET MARK OF BAR 3 OF _OPTIONPOP TO .T.  
    SET MARK OF BAR 4 OF _OPTIONPOP TO .F.
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINE 
    llOrdLStr  = .F.
    SHOW WINDOW (lcOrdBrTtl) REFRESH SAME  
    
  CASE lnBarNo = 4 .AND. !llOrdLStr 
    SET MARK OF BAR 3 OF _OPTIONPOP TO .F.  
    SET MARK OF BAR 4 OF _OPTIONPOP TO .T.
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINST 
    llOrdLStr  = .T.
    SHOW WINDOW (lcOrdBrTtl) REFRESH SAME  
    
  *-- Scope option
  CASE lnBarNo = 5
    =lfvScope()

  *-- Zoom option
  CASE lnBarNo = 6
    SET MARK OF BAR 6 OF _OPTIONPOP TO !llZoomed
    =lfvZoom()

  *-- Generate Picking Tickets options option
  *CASE lnBarNo = 7
  CASE lnBarNo = lnForceBar

    llForceAlc = !llForceAlc

    *-- E301225,1 Allow Force allocation option. [Begin]
    *SET MARK OF BAR 7 OF _OPTIONPOP TO llForceAlc
    SET MARK OF BAR lnForceBar OF _OPTIONPOP TO llForceAlc
    *-- E301225,1 Allow Force allocation option. [Begin]

  *Ren ADD
  *-- Generate Picking Tickets options option
  *CASE lnBarNo = 8
  CASE lnBarNo = lnPickBar
  
    llGenPkTk = !llGenPkTk

    *-- E301225,1 Allow Force allocation option. [Begin]
    *SET MARK OF BAR 8 OF _OPTIONPOP TO llGenPkTk
    SET MARK OF BAR lnPickBar OF _OPTIONPOP TO llGenPkTk
    *-- E301225,1 Allow Force allocation option. [End  ]

    =lfShowPkBt()
    =lfSelectLn()
  *E301340,1 WAB - call function to display order shaving screee
  *E301340,1 WAB - Start
  CASE lnBarNo = lnShavBar
    =lfShowShav()  
  *E301340,1 WAB - END
    
ENDCASE
SELECT (lnCurALias)
*-- end of lfvOptions.

*Ren
FUNCTION lfSelectLn
PRIVATE lnCurAlias, lnRecNo , lcMemFile


IF llGenPkTk
  lnCurAlias = SELECT(0)
  SELECT ORDLINE
  lnRecNo  = RECNO('ORDLINE')
  *lnCurTag = VAL(SYS(21))
  *SET ORDER TO TAG ORDLINST
  lcScanExp = IIF(!EMPTY(lcBrowExp),;
       		      [FOR ] + lcNdxExp + [ = laData(9) + laData(1) + ] + lcBrowExp ,;
       		      [REST WHILE cOrdType + Order = laData(9) + laData(1)];
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ FOR Group = lcGroup],[]))
 
  *E300511.1 Scan all records of the temp ordline file
  IF SEEK(laData[9] + laData[1], 'ORDLINE')

    *B803365,1 HBG 07/26/2000 Save Memvar into memo file [Begin]
    lcMemFile = gfTempName() + ".MEM"
    SAVE ALL LIKE * TO &gcWorkDir.&lcMemFile
    *B803365,1 [End  ]

    SCAN &lcScanExp
      IF EOF(lc_TmpOrdL) 
        SELECT ORDLINE
        IF Picked .AND. PikTkt=REPLICATE("*", FSIZE("PIKTKT","PIKTKT"))
          *B802756,4 Add a line with the order line warehouse
          *=lfAddLine(lcWareCode)
          =lfAddLine()
          *B802756,4 end
        ENDIF  
      *B802756,4 Do not add a line if the line already exists in the tenporary
      *B802756,4 file
      *ELSE
      *  SELECT (lc_TmpOrdL)
      *  IF Picked
      *    =lfAddLine(lcWareCode)
      *  ENDIF  
      ENDIF
      *B802756,4 end  
    ENDSCAN

    *B803365,1 HBG 07/26/2000 Restore Memvar from memo file [Begin]
    RESTORE FROM &gcWorkDir.&lcMemFile ADDITIVE
    ERASE &gcWorkDir.&lcMemFile
    *B803365,1 [End  ]


  ENDIF  

  *SET ORDER TO (lnCurTag) 
  IF BETWEEN(lnRecNo, 1, RECCOUNT())
    GO lnRecNo
    
  ELSE
    GO BOTTOM
  ENDIF  


  SELECT (lnCurAlias)
ENDIF  


*!*************************************************************
*! Name      : lfShowPkBt
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : lpvInquiry,lfvSortBy,lfvQtyPSize,lfvDetails,lfvLNote,;
*!             lfvCopyQty,lfvPasteQty,lfvBrowZoom
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowPkBt()
*!*************************************************************
*Ren Add
FUNCTION lfShowPkBt
PRIVATE lcPickAlSt
lcPickAlSt = IIF(laScrMode[3], 'ENABLE', 'DISABLE')
IF llGenPkTk
    SHOW GET pbPick,1 PROMPT 'Pi\<ck' 	   &lcPickStat
  SHOW GET pbPickAll,1 PROMPT 'Pick \<All' &lcPickAlSt
ELSE
  SHOW GET pbPick,1 PROMPT 'Allo\<cate' 	   &lcPickStat 
  SHOW GET pbPickAll,1 PROMPT 'Allocate \<All' &lcPickAlSt 
ENDIF   

*!*************************************************************
*! Name      : lfvCustInq
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : lpvInquiry,lfvSortBy,lfvQtyPSize,lfvDetails,lfvLNote,;
*!             lfvCopyQty,lfvPasteQty,lfvBrowZoom
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCustInq()
*!*************************************************************
FUNCTION lfvCustInq
lcParameter = [']+laData[2]+[']+IIF(EMPTY(m.Store),[],[,']+m.Store+['])
DO gpDoProg WITH 'AWRARCUST',.F.,'AR',lcParameter

*!*************************************************************
*! Name      : lfvStyInq
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : lpvInquiry,lfvSortBy,lfvQtyPSize,lfvDetails,lfvLNote,;
*!             lfvCopyQty,lfvPasteQty,lfvBrowZoom
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStyInq()
*!*************************************************************
FUNCTION lfvStyInq
lcParameter = "'"+m.Style+"'"
DO gpDoProg WITH 'AWRICSTYLE',.F.,'IC',lcParameter

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : RENEE - Renee Ezzat
*! Date      : 09/06/1995
*! Purpose   : Valid function for push button < Next > (pbNext).
*!             Displays the next record on the file.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNext()
*!*************************************************************
FUNCTION lfwOldVal
lcOldVal = EVALUATE('m.'+SYS(18))  

*!*************************************************************
*! Name      : lfwOldVar
*! Developer : RENEE - Renee Ezzat
*! Date      : 09/06/1995
*! Purpose   : Valid function for push button < Next > (pbNext).
*!             Displays the next record on the file.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNext()
*!*************************************************************
FUNCTION lfwOldVar
lcOldVal = EVALUATE(SYS(18))  
  
*!*************************************************************
*! Name      : lfvPick
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Pick a selected line
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfvPick 
PARAMETERS llPickAll


*C123616,1  TMI [Start] If user will pick using cartons , then do not pick a single line , allow only pick all , for [Nik Nak]
IF ASCAN(laEvntTrig,PADR("ALPCKALL",10)) <> 0
  IF gfDoTriger("ALORDAL",PADR("ALPCKALL",10))
    RETURN
  ENDIF
ENDIF
*C123616,1  TMI [End  ] 

lnCurAlias = SELECT(0)
*-- Pick a line, 
SELECT ORDLINE

IF lfVldPick(llPickAll)
  IF !EOF(lc_TmpPkLn)
    SELECT (lc_TmpPkLn)
    DELETE
  ENDIF  

  *-- If this line has not been picked before, allocate
  *-- available quantities by default, and force allocation if set to
  *-- 'Always force allocation'
  *IF EMPTY(ORDLINE.PikTkt) .OR. lfLinIsChg() .OR. m.lForceAlc
  *E500304,1 WAB - in case of no change or case in change recaluclate default qty
  *E500304,1 WAB - so we put (lfLinIsChg() .OR. !lfLinIsChg()) 
  *E500304,1 WAB - START
  *IF EMPTY(ORDLINE.PikTkt) .OR. lfLinIsChg() .OR. (lfMayForcAlc() AND llForceAlc)
  IF EMPTY(ORDLINE.PikTkt) .OR. (lfLinIsChg() .OR. !lfLinIsChg())  .OR. (lfMayForcAlc() AND llForceAlc)
  *E500304,1 WAB - END
    *-- If first pick
    m.cStatus   = ' '
    m.Picked    =  .T.   
    *-- Insert a record in temp StyDye file if one is not found
    IF !SEEK(m.Style + m.cWareCode + SPACE(10), lc_TmpStyD) .AND.;
       SEEK(m.Style + m.cWareCode + SPACE(10), 'STYDYE')
      SELECT STYDYE 
      SCATTER TO laStyDye 
      INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye
    ENDIF
    
    *-- Insert a record in temp StyDye file if one is not found
    IF llUseDyes .AND. STYLE.cDye_flg = 'Y' .AND. ;
       !SEEK(m.Style + m.cWareCode + m.Dyelot, lc_TmpStyD) .AND.;
       SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
      SELECT STYDYE 
      SCATTER TO laStyDye 
      INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye
    ENDIF

    *-- Allocate available quantities by default.
    SELECT (lc_TmpStyD)
    *-- If allocation is to be forced, take the ordered quantity
    *-- as it is, otherwise, take available quantity
    *Ren : 
    m.lForceAlc = llForceAlc   &&.OR. m.lForceAlc
    *Ren : end

    IF m.lForceAlc

      *E301310,1 Check Approved Amount.
      IF llChkAprov AND !lfAprvLine(m.TotQty - m.TotPik)
       RETURN
      ENDIF
      *E301310,1 Check Approved Amount.

      m.Pik1      = m.Qty1
      m.Pik2      = m.Qty2
      m.Pik3      = m.Qty3
      m.Pik4      = m.Qty4
      m.Pik5      = m.Qty5
      m.Pik6      = m.Qty6
      m.Pik7      = m.Qty7
      m.Pik8      = m.Qty8
      m.TotPik    = m.Pik1 + m.Pik2 + m.Pik3 + m.Pik4 +;
                      m.Pik5 + m.Pik6 + m.Pik7 + m.Pik8
      m.nForcdQty = m.TotPik

    ELSE

      *C200084,1 Distribute Multi Store Quantity [Begin]
      PRIVATE laAllocPer
      DIMENSION laAllocPer[8]
      laAllocPer = 1
      
      IF ASCAN(laEvntTrig,"BYORDPRCNT") <> 0
        = gfDoTriger("ALORDAL","BYORDPRCNT")
      ENDIF  

      *m.Pik1      = MIN(m.Qty1, MAX(Stk1-Alo1, 0))
      *m.Pik2      = MIN(m.Qty2, MAX(Stk2-Alo2, 0))
      *m.Pik3      = MIN(m.Qty3, MAX(Stk3-Alo3, 0))
      *m.Pik4      = MIN(m.Qty4, MAX(Stk4-Alo4, 0))
      *m.Pik5      = MIN(m.Qty5, MAX(Stk5-Alo5, 0))
      *m.Pik6      = MIN(m.Qty6, MAX(Stk6-Alo6, 0))
      *m.Pik7      = MIN(m.Qty7, MAX(Stk7-Alo7, 0))
      *m.Pik8      = MIN(m.Qty8, MAX(Stk8-Alo8, 0))

      *E301310,1 Check Approved Amount.
      lnAloQty = lfGetAlo()
      IF llChkAprov AND !lfAprvLine(lnAloQty - m.TotPik)
        RETURN
      ENDIF
      *E301310,1 Check Approved Amount.
      *E500304,1 WAB - Calculate the avialble qty (stk-alo) by calling function
      *E500304,1 WAB - lftotavlbl() to get the available qty
      *E500304,1 WAB - START
      *m.Pik1      = ROUND(MIN(m.Qty1, MAX((Stk1-Alo1)*laAllocPer[1], 0)),0)
      *m.Pik2      = ROUND(MIN(m.Qty2, MAX((Stk2-Alo2)*laAllocPer[2], 0)),0)
      *m.Pik3      = ROUND(MIN(m.Qty3, MAX((Stk3-Alo3)*laAllocPer[3], 0)),0)
      *m.Pik4      = ROUND(MIN(m.Qty4, MAX((Stk4-Alo4)*laAllocPer[4], 0)),0)
      *m.Pik5      = ROUND(MIN(m.Qty5, MAX((Stk5-Alo5)*laAllocPer[5], 0)),0)
      *m.Pik6      = ROUND(MIN(m.Qty6, MAX((Stk6-Alo6)*laAllocPer[6], 0)),0)
      *m.Pik7      = ROUND(MIN(m.Qty7, MAX((Stk7-Alo7)*laAllocPer[7], 0)),0)
      *m.Pik8      = ROUND(MIN(m.Qty8, MAX((Stk8-Alo8)*laAllocPer[8], 0)),0)
      m.Pik1      = ROUND(MIN(m.Qty1, MAX(lfTotAvlbl('1')*laAllocPer[1], 0)),0)
      m.Pik2      = ROUND(MIN(m.Qty2, MAX(lfTotAvlbl('2')*laAllocPer[2], 0)),0)
      m.Pik3      = ROUND(MIN(m.Qty3, MAX(lfTotAvlbl('3')*laAllocPer[3], 0)),0)
      m.Pik4      = ROUND(MIN(m.Qty4, MAX(lfTotAvlbl('4')*laAllocPer[4], 0)),0)
      m.Pik5      = ROUND(MIN(m.Qty5, MAX(lfTotAvlbl('5')*laAllocPer[5], 0)),0)
      m.Pik6      = ROUND(MIN(m.Qty6, MAX(lfTotAvlbl('6')*laAllocPer[6], 0)),0)
      m.Pik7      = ROUND(MIN(m.Qty7, MAX(lfTotAvlbl('7')*laAllocPer[7], 0)),0)
      m.Pik8      = ROUND(MIN(m.Qty8, MAX(lfTotAvlbl('8')*laAllocPer[8], 0)),0)
      *E500304,1 WAB - END
      *C200084,1 Distribute Multi Store Quantity [End  ]

      m.TotPik    = m.Pik1 + m.Pik2 + m.Pik3 + m.Pik4 +;
    	  	        m.Pik5 + m.Pik6 + m.Pik7 + m.Pik8

      m.nForcdQty = 0	  	        

    ENDIF
    
    m.PikTkt  = SPACE(6)
    m.PikDate = {}
    =lfAdjStyDye('A')    		   
    SELECT (lc_TmpOrdL)
    GATHER MEMVAR MEMO
  ELSE
    SELECT ORDLINE
    SCATTER MEMVAR MEMO
    *E301310,1 Subtract pre allocated Amount again. [Begin]
    IF llChkAprov
      lnChkAprov = lnChkAprov - m.TotPik * m.Price   
    ENDIF
    *E301310,1 Subtract pre allocated Amount again. [End  ]

    =lfAdjStyDye('A')
    SELECT (lc_TmpOrdL)
    DELETE
  ENDIF
  
  *B802756,1 WAB - Checks if the alocated qty of the current record entry 
  *B802756,1 WAB - is valid  ( more than zero )
  *B802756,1 WAB - START
  llChcked = lfChkEntry()
  *B802756,1 WAB - END
  
  *B605084,1 HBG 01/11/2001 If user chose not ignoring this Piktkt , pick it [Begin]
  IF !llChcked
    m.Pik1      = ROUND(MIN(m.Qty1, MAX(lfGetAvlbl('1')*laAllocPer[1], 0)),0)
    m.Pik2      = ROUND(MIN(m.Qty2, MAX(lfGetAvlbl('2')*laAllocPer[2], 0)),0)
    m.Pik3      = ROUND(MIN(m.Qty3, MAX(lfGetAvlbl('3')*laAllocPer[3], 0)),0)
    m.Pik4      = ROUND(MIN(m.Qty4, MAX(lfGetAvlbl('4')*laAllocPer[4], 0)),0)
    m.Pik5      = ROUND(MIN(m.Qty5, MAX(lfGetAvlbl('5')*laAllocPer[5], 0)),0)
    m.Pik6      = ROUND(MIN(m.Qty6, MAX(lfGetAvlbl('6')*laAllocPer[6], 0)),0)
    m.Pik7      = ROUND(MIN(m.Qty7, MAX(lfGetAvlbl('7')*laAllocPer[7], 0)),0)
    m.Pik8      = ROUND(MIN(m.Qty8, MAX(lfGetAvlbl('8')*laAllocPer[8], 0)),0)
      
    m.TotPik = m.Pik1 + m.Pik2 + m.Pik3 + m.Pik4 +;
               m.Pik5 + m.Pik6 + m.Pik7 + m.Pik8

    m.nForcdQty = 0	  	        
    m.PikTkt  = SPACE(6)
    m.PikDate = {}
    =lfAdjStyDye('A')
     
    SELECT (lc_TmpOrdL)
    GATHER MEMVAR MEMO
  ENDIF  
  *B605084,1 [End]
    
  *E301340,1 WAB-1-Check if there is shaving in current order line by check 
  *E301340,1       if any qty? > pik?
  *E301340,1     2-if there is shaving and the setting of order shaving is 
  *E301340,1       not 'Never'	-------> Enable reason popup  ELSE DISABLE
  *E301340,1     3-IF the Setting of order shaving is Automatic set the reason 
  *E301340,1       popup with the Default reason popup and replace the reason filed in
  *E301340,1       orderline temp. file with reason code
  llShave = lfCanShave()
  *B802756,1 WAB - we must shave only in case the user agree of continue with 
  *B802756,1 WAB - alocated qty = 0
  *B802756,1 WAB - START
  *IF llShave
  IF llShave .AND. llChcked
  *B802756,1 WAB - END
    lcDispReas = IIF(lnOrdShv <> 3,'ENABLE','DISABLE')
    IF lnOrdShv = 1
      lnSelRsn   = lnReson
      SELECT (lc_TmpOrdL) 
      REPLACE cReason WITH laShReson[lnSelRsn , 2]
    ELSE
      lnSelRsn   = 1
    ENDIF  
  ELSE
    lcDispReas = 'DISABLE'
    lnSelRsn   = 1
  ENDIF
  SHOW GET lnSelRsn &lcDispReas
  *E301340,1 WAB - END
  
  *lfChkEntry()
  
 
  IF !llPickAll 
    *B802756,1 WAB - if the alocated qty of the current record entry is valid  
    *B802756,1 WAB - START
    IF llChcked
    *B802756,1 WAB - END
      =gfUpdate() 
      *B603240,1 Skip to the next record, and activate the brose
      SELECT ORDLINE
      SKIP
      IF ORDLINE.Order <> laData[1]
        SKIP -1
      ENDIF
      KEYBOARD "{ALT+B}"
      *=lfShowLines() 
    *B802756,1 WAB - endif for the if that we added ( if tyhe alocated is valid)
    *B802756,1 WAB -START
    ENDIF
    *B802756,1 WAB - END
    =lfwOrdBrow()
  ENDIF  
  *B603240,1 end
  *SHOW WINDOW (lcOrdBrTtl) REFRESH SAME  
  
ENDIF  
SELECT (lnCurAlias)



*!*************************************************************
*! Name      : lfvMinAlo
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Close program or cancel order modification
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfvMinAlo
IF lnMinAlo < 0
  lnMinAlo = 0
ENDIF  

*!*************************************************************
*! Name      : lfvCancel
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Close program or cancel order modification
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfvCancel
PARAMETERS llCanceled
llCanceled  = .F.


*!*************************************************************
*! Name      : lfvPickAll
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Close program or cancel order modification
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfvPickAll
PRIVATE lnCurAlias, lnOldAlo, llPickAll, lcCurStore, lcScanExp, lnCurTag, lnStrQty
lnCurAlias = SELECT(0)

*B603566,1 ABD- Give the user massage if press allocate all Or Pick all.[Begin]
*B603566,1  Message No. : 40169
*B603566,1  Message     : Are you sure you want to XXXXXXX?
*B603566,1  Button No.  : 44009
*B603566,1  Button      : \<Yes;\<No


IF gfModalGen('INM40169B44009', 'DIALOG',IIF(llGenPkTk,'Pick All','Allocate All')) = 1
*B603566,1 ABD- [End]
  llPickAll       = .T.
  *B804197,1 HBG 19/09/2001 Flag to check if it is Pack/Allocate All [Begin]
  llPAll = llPickAll       
  *B804197,1 [End]
  
  IF ORDHDR.Multi = 'Y'
    lnOldAlo   = lnMinAlo
    *DO (gcScrDir + gcWinAppl + '\ALORDALA.SPR')
    *IF !llPickAll
    *  lnMinAlo  = lnOldAlo 
    *ENDIF
  ENDIF  
  IF llPickAll
    lnRecNo  = RECNO('ORDLINE')
    SELECT ORDLINE
    lnCurTag = VAL(SYS(21))
    SET ORDER TO TAG ORDLINST
    IF BETWEEN(RECNO('ORDHDR'),1,RECCOUNT())
      GO RECNO('ORDHDR') IN ORDHDR
    ENDIF  
   
    lcCurStore  = ' '
    *lcScanExp = IIF(!EMPTY(lcBrowExp),;
       		      [FOR ] + lcNdxExp + [ = laData(9) + laData(1) + ] +;
       		      IIF(EMPTY(lcStore), [STORE ], [lcStore]) + ;
       		      IIF(EMPTY(lcStyle), [], [+ lcStyle]);
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ AND Group = lcGroup],[]),;
       		      [REST WHILE cOrdType + Order = laData(9) + laData(1)];
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ FOR Group = lcGroup],[]))

    *lcScanExp = IIF(!EMPTY(lcBrowExp),;
         		      [FOR ] + lcNdxExp + [ = laData(9) + laData(1) + ] +;
         		      [ALLTRIM(lcStore + lcStyle)];
         	  	      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ AND Group = lcGroup],[]),;
       	    	      [REST WHILE cOrdType + Order = laData(9) + laData(1)];
         		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ FOR Group = lcGroup],[]))

     lcScanExp = IIF(!EMPTY(lcBrowExp),;
         		      [FOR ] + lcNdxExp + [ = laData(9) + laData(1) + ] + lcBrowExp ,;
         		      [REST WHILE cOrdType + Order = laData(9) + laData(1)];
         		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ FOR Group = lcGroup],[]))
    
    *B804197,1 HBG 19/09/2001 Flag to check if there is lines ignored or not [Begin]
    llIgnore = .F.
    *B804197,1 [End]
    *B605084,1 HBG 01/11/2001 Flages to be used in case of Pick all , to display MSG only one time[Begin]
    llFirst = .T.
    lnMsg = 0
    llFLine = .T.
    *B605084,1 [End]
    *E300511.1 Scan all records of the temp ordline file
    SCAN &lcScanExp
      *E300511.1 If multi store order
      IF ORDHDR.Multi = 'Y'
        *-- Check for minimun required QUANTITY before picking
        IF STORE<>lcCurStore
          lcCurStore = STORE
          SUM REST WHILE cOrdType + ORDER + STORE = laData[9] + laData[1] + lcCurStore;
              TOTQTY TO lnStrQty 
          IF lnStrQty < lnMinAlo
            SKIP -1
            LOOP
          ELSE  
            SEEK laData[9] + laData[1] + lcCurStore
          ENDIF
        ENDIF  
      ENDIF
      IF !EOF(lc_TmpOrdL)
        SELECT(lc_TmpOrdL)
      ENDIF  
      *IF !Picked .AND. TotQty <> 0 .AND. (EVAL(lc_TmpOrdL+'.lForceAlc') .OR. lfGetTotStk() <> 0)
      *B605084,1 HBG 01/11/2001 Remove the condition to make Piak all work same as Pick [Begin]
      *IF !Picked .AND. TotQty <> 0 .AND. (llForceAlc .OR. lfGetTotStk() <> 0)
      IF !Picked .AND. TotQty <> 0 
      *B605084,1 [End]
        *B802938,1 WAB - in case of 'allocate all' the m.dyelot hold the dyelot 
        *B802938,1 WAB - of the current record , so we used the field dyelot
        *IF llUseDyes .AND. STYLE.cDye_flg = 'Y' .AND. EMPTY(m.Dyelot)
        IF llUseDyes .AND. STYLE.cDye_flg = 'Y' .AND. EMPTY(Dyelot)
        *B802938,1 WAB - END
          *-- Message : 44024
          *-- 'Style _ comes in dyelots.  You need to pick this 
          *-- 'line manually to select a dyelot.'
          *-- Button  : OK
          =gfModalGen('INM44024B00000', 'DIALOG', ALLTRIM(Style))
        ELSE           
          llFailAprv = .F.  && E301310,1 To exit loop if one line fail.
          =lfvPick(.T.)
          *E301310,1 If you fail in allocation due to approval test
          *E301310,1 then exit loop. [Begin]
          IF llFailAprv
            EXIT
          ENDIF 
          *E301310,1 then exit loop. [End  ]
        ENDIF 
      *B804197,1 HBG 19/09/2001 if this line will be ignored check if not force allocation 
      *B804197,1                and there is no stock available set the flag to true [Begin]
      *B605084,1 HBG 01/11/2001 Remove else of the condition to make Piak all work same as Pick [Begin]
      *ELSE
      *  IF !llForceBar AND lfGetTotStk() = 0
      *    llIgnore = .T.
      *  ENDIF
      *B605084,1 [End]
      *B804197,1 [End]

      ENDIF
      SELECT ORDLINE
    ENDSCAN
    *B804197,1 HBG 19/09/2001 if this line will be ignored because there is no stock available 
    *B804197,1                get a massage : "There is no stock available for one or more sizes 
    *B804197,1                on some lines. Cannot allocate these lines." [Begin]
    *B605084,1 HBG 01/11/2001 Move This block of code to valid function of Pick [Begin]
    *IF llIgnore
    *  = gfModalGen('QRM44109B00000','DIALOG',;
                    'some lines'+"|"+IIF(llGenPkTk,'Pick these lines.','Allocate these lines.'))
    *ENDIF
    *B605084,1 [End]
    *B804197,1 [End]

    *C123616,1  TMI [Start] Update the temp allocation file lc_tmpOrdL from pocrtnmf [for Nik Nak]
    IF ASCAN(laEvntTrig,PADR("UPDTMAL",10)) <> 0
      =gfDoTriger("ALORDAL",PADR("UPDTMAL",10))
    ENDIF
    *C123616,1  TMI [End  ] 
    
    SELECT ORDLINE
    SET ORDER TO (lnCurTag) 
    IF BETWEEN(lnRecNo, 1, RECCOUNT())
      GO lnRecNo
    ELSE
      GO BOTTOM
    ENDIF  
    IF !EOF(lc_TmpOrdL)
      SELECT (lc_TmpOrdL)
    ENDIF  
    SCATTER MEMVAR MEMO
    lcStkAlias = IIF(EOF(lc_TmpStyD), 'STYDYE', lc_TmpStyD)
    

    *=lfShowLines()
    *SHOW WINDOW (lcOrdBrTtl) REFRESH SAME


    SELECT (lnCurAlias)
  ENDIF  
  
  *B603566,1 ABD- End for if statement. [Begin]
ENDIF
*B603566,1 ABD- [End]
*B804197,1 HBG 19/09/2001 Flag to check if it is Pick/Allocate All [Begin]
llPAll = .F.
*B804197,1 [End]
*B803365,1 HBG Refresh the browse [Begin]
=lfShowLines()
SHOW WINDOW (lcOrdBrTtl) REFRESH SAME
*B803365,1 [End]

*!*************************************************************
*! Name      : lfvPikQty
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Validation for m.Pik fields
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfvPikQty
lcObjName  = 'm.'+SYS(18)
lnObjVal   = EVALUATE(lcObjName)

*B039660,1 NNA 02/14/2006 (BEGIN) If we're in the edit mode I'll check if this piktkt has a packing list or not
*B039660,1 NNA            to prevent user from changing the piktkt Qty.
IF ASCAN(laEvntTrig , PADR('CHKPACK',10)) <> 0 AND !gfDoTriger('ALORDAL',PADR('CHKPACK',10))
  RETURN
ENDIF
*B039660,1 NNA (End)

*E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt. [Begin]
IF 'AS' $ gcCmpModules AND llSndPkFld
  IF PIKTKT.csndpkt = 'Y' AND lnObjVal <> lcOldVal
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' can not edit ' )
    &lcObjName = lcOldVal
    _CUROBJ    = _CUROBJ
    RETURN
  ENDIF
ENDIF
*E130074,1 EIH 01/02/2006 [End]

*-- MAB 09/01/1999 Force user to values greater than or equal zero. [begin]
IF lnObjVal < 0
  *Message : 44081 ==> A negative value is not allowed.
  *Button  : 00000 ==> < Ok >
  =gfModalGen('INM44081B00000', 'DIALOG')
  &lcObjName = lcOldVal
  _CUROBJ    = _CUROBJ
  RETURN
ENDIF
*-- MAB 09/01/1999 Force user to values greater than or equal zero. [end  ]


*B606652,1 ABD - Fix bug that the program get 0 qty for the second style the program 
*B606652,1 ABD - Picked qty for first style color has been released which is wrong,
*B606652,1 ABD - So if the user set 0 for all sizes we will relese this line. [Begin]
IF (m.TotPik - lcOldVal + lnObjVal) = 0
  lnOption = gfModalGen('QRM44006B00006','DIALOG',;
             IIF(EMPTY(ORDLINE.PIKTKT), 'ignore', 'release'))
 IF lnOption = 1
   =lfvRel()
 ELSE
   &lcObjName = lcOldVal
   _CUROBJ    = _CUROBJ
 ENDIF 
 RETURN
ENDIF
*B606652,1 ABD - [End]

*-- If the value is changed,
IF lnObjVal <> lcOldVal

  lcObjNum   = RIGHT(SYS(18),1)
  lnStk      = MAX(EVAL(lcStkAlias+'.Stk'+lcObjNum+'-'+ lcStkAlias+'.Alo'+lcObjNum), 0)
  *-- Compare allocated quantity with ordered quantity
  IF lnObjVal <= m.Qty&lcObjNum .OR. lfChngOrd(@llChngOrd)
    *-- Check if there is available stock
    
    *E301310,1 Check Approved Amount. [Begin]
    *IF lnObjVal-lcOldVal <= lnStk .OR. lfForcAl(@llForcLAlc, lnObjVal - lnStk)
    IF (lnObjVal-lcOldVal <= lnStk .OR. ;
       lfForcAl(@llForcLAlc, lnObjVal - lnStk)) .AND. ;
       (!llChkAprov .OR. lfAprvLine(lnObjVal - lcOldVal))
    *E301310,1 Check Approved Amount. [End  ]

      m.TotPik   = m.TotPik - lcOldVal + lnObjVal
      lnCurAlias = SELECT(0)
      IF m.cStatus = 'M' .AND. lnObjVal > m.Qty&lcObjNum 
         m.TotQty        = m.TotQty - m.Qty&lcObjNum + lnObjVal
         m.TotBook       = m.TotBook - m.Qty&lcObjNum + lnObjVal
         *B037363,4 HBG 25/12/2003 Fix bug of wrong saveing booked qty incase of increasing order qty[Begin]
         m.Book&lcObjNum = m.Book&lcObjNum  + (lnObjVal-m.Qty&lcObjNum)
         *B037363,4 [End]
         m.Qty&lcObjNum  = lnObjVal
         *B037363,4 HBG 25/12/2003 Comment this line to fix bug of wrong saveing booked qty incase of increasing order qty[Begin]
         m.Book&lcObjNum = lnObjVal
         *B037363,4 [End]
   	  ENDIF
      IF EOF(lc_TmpOrdL)
        STORE .T. TO m.lVldWare, m.lvldDye
        m.cStatus    = IIF(m.cStatus='M', m.cStatus, ' ')
        m.lAllocated = llGenPkTk
        INSERT INTO (lc_TmpOrdL) FROM MEMVAR 
      ELSE  
        SELECT (lc_TmpOrdL)
        GATHER MEMVAR MEMO
      ENDIF
        
      IF !SEEK(m.Style + m.cWareCode + SPACE(10), lc_TmpStyD) .AND.;
        SEEK(m.Style + m.cWareCode + SPACE(10), 'STYDYE')
 		SELECT STYDYE
        SCATTER TO laStyDye MEMVAR
        INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye 
      ENDIF
      SELECT (lc_TmpStyD)
      REPLACE Alo&lcObjNum WITH Alo&lcObjNum - lcOldVal + lnObjVal,;
              TotAlo       WITH TotAlo       - lcOldVal + lnObjVal
      *-- Update temp stydye
      IF llUseDyes .AND. STYLE.cDye_flg = 'Y' 
        IF !SEEK(m.Style + m.cWareCode + m.Dyelot, lc_TmpStyD) .AND.;
          SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
          SELECT STYDYE
          SCATTER TO laStyDye MEMVAR
          INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye 
		ENDIF
		SELECT (lc_TmpStyD)
        REPLACE Alo&lcObjNum WITH Alo&lcObjNum - lcOldVal + lnObjVal,;
                TotAlo       WITH TotAlo       - lcOldVal + lnObjVal
        
        *B802829,1 KHM 03/06/2000 (Begin) Calling this function in order
        *B802829,1                to adjust the allocated qty per size.
        =lfAdjTStyD('',lcObjNum,lcOldVal,lnObjVal)
        *B802829,1 KHM 03/06/2000 (End)
        
      ENDIF
      lcStkAlias = lc_TmpStyD
      SHOW WINDOW (lcOrdBrTtl) REFRESH SAME
      SELECT (lnCurAlias)
      =lfRefresh(lcAlOrdAl3)    
      *E301340,1 WAB-1-Check if there is shaving in current order line by checking 
      *E301340,1       if any qty? > pik?
      *E301340,1     2-if there is shaving and the setting of order shaving is 
      *E301340,1       not 'Never'	-------> Enable reason popup  ELSE DISABLE
      *E301340,1     3-IF the Setting of order shaving is Automatic set the reason 
      *E301340,1       popoup with the Default popup and replace the reason filed in
      *E301340,1       orderline temp. file with reason code
      *E301340,1 WAB - START
      llShave = lfCanShave()
      *-- If the popup is diabled, then, the user has just decreased
      *-- the allocated quantities.
      IF llShave 
        IF lcDispReas = "DISABLE"
          lcDispReas = IIF(lnOrdShv <> 3,'ENABLE','DISABLE')
          IF lnOrdShv = 1
            lnSelRsn   = lnReson
            SELECT (lc_TmpOrdL) 
            REPLACE cReason WITH laShReson[lnSelRsn , 2]
          ELSE
            lnSelRsn   = 1
          ENDIF  
        ENDIF
      ELSE
        lcDispReas = "DISABLE"
        lnSelRsn   = 1 
        SELECT (lc_TmpOrdL)         
        REPLACE &lc_TmpOrdL..cReason WITH ' '  
      ENDIF
      SHOW GET lnSelRsn &lcDispReas
      *E301340,1 WAB - END
    ELSE
      &lcObjName = lcOldVal
      _CUROBJ    = _CUROBJ
    ENDIF
  ELSE
    &lcObjName = lcOldVal
    _CUROBJ    = _CUROBJ
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Close program or cancel order modification
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lpSavScr
PRIVATE lcPikTktNo, lcChgPkTKt, lnCurTag

=gfOpenFile(gcDataDir+"WAREHOUS", gcDataDir+"WAREHOUS", "SH")
IF lfChkEntry()
 
  *C123616,1  TMI [Start] Comment this trigger , the screen is added in option menu [ for Nik Nak ]
  *                       The way I proceed is to update temp files and they are saved automatically
  **C200489,1 WAB (Start)
  *IF ASCAN(laEvntTrig,PADR("ALCARTON",10)) <> 0
  *  IF !gfDoTriger("ALORDAL",PADR("ALCARTON",10))
  *    RETURN
  *  ENDIF
  *ENDIF
  **C200489,1 WAB (End)
  *C123616,1  TMI [End  ] 
  
  *C123847,1  TMI [Start] *-----
  *- if ORDLINE.PIKTKT <> ' ' and Ordline.qty > Ordline.Pik - and there is enough inventory 
  *- ask the user if he wants to a) Add to existing Picking ticket  
  *-                             b) Create a new Picking Ticket 
  *ash commented
  *IF ASCAN(laEvntTrig,PADR("CHKAVL",10)) <> 0
  *  =gfDoTriger("ALORDAL",PADR("CHKAVL",10))
  *ENDIF  
  *C123847,1  TMI [End  ] 
  
  *B039660,1 NNA 02/14/2006 Check first if there is enough stock or not before making update in the Bin location Files
  IF ASCAN(laEvntTrig,PADR("CHKFIRST",10)) <> 0 AND !gfDoTriger("ALORDAL",PADR("CHKFIRST",10))
    llCSave = .F.
    RETURN
  ENDIF  
  *B039660,1 NNA (End)

  SET ORDER TO TAG PIKORD IN (lc_TmpPkLn)
  SELECT ORDLINE
  SET RELATION OFF INTO (lc_TmpOrdL)
  lnCurTag = VAL(SYS(21))
  SET ORDER TO TAG ORDLINE
  SELECT (lc_TmpOrdL)
  SET ORDER TO TAG ORDLINST
  SET RELATION TO cOrdType+order+STR(lineno,6) INTO ORDLINE ADDITIVE

  STORE ' ' TO lcChgPkTKt
  *-- Scan only the records that were not saved
  *SCAN FOR nSteps < 4   && number
  *-- Update incomplete session with the current object number
  =lfUpdUnCmS()
  *E301340,1 WAB - Initialize variable nedded in update files 
  *E301340,1 WAB - START
  llAcordMsg = .F.		&& flag to know if release po/ct   message is diplayed
  llAlocMsg  = .F.		&& flag to know if alocate message is diplayed
  llOkCancel = .F.		&& flag to know if user agree to cancel alocation
  llUpdPoCt  = .F.		&& flag to know if user agree to update po/ct
  *E301340,1 WAB - END
  SCAN 
    SCATTER MEMVAR MEMO
    
    *B603318,1 Start, Commented. 
    *C200084,1 Update OrdHdr Net Alocate Quantity. [Begin]
    *IF ASCAN(laEvntTrig,PADR("UPDTHDR",10)) <> 0
    *  = gfDoTriger("ALORDAL",PADR("UPDTHDR",10))
    *ENDIF  
    *C200084,1 Update OrdHdr Net Alocate Quantity. [End  ]
    *B603318,1 End.
    DO CASE
      CASE cStatus = 'R'
      
        *-- Refresh variables with the original ORDLINE
        SELECT ORDLINE
        SCATTER MEMVAR MEMO FIELDS LIKE Style, cWareCode, Dyelot,;
        					AltStyle, Scale
        
        *-- Update allocated quantities in STYDYE 
        SELECT STYDYE
        IF SEEK(m.Style + ORDLINE.cWareCode + SPACE(10)) .AND. gfObj_Lock(.T.)
          REPLACE Alo1   WITH MAX(Alo1 - ORDLINE.Pik1, 0),;
                  Alo2   WITH MAX(Alo2 - ORDLINE.Pik2, 0),;
        	      Alo3   WITH MAX(Alo3 - ORDLINE.Pik3, 0),;
       		      Alo4   WITH MAX(Alo4 - ORDLINE.Pik4, 0),;
        		  Alo5   WITH MAX(Alo5 - ORDLINE.Pik5, 0),;
        		  Alo6   WITH MAX(Alo6 - ORDLINE.Pik6, 0),;
      	 	      Alo7   WITH MAX(Alo7 - ORDLINE.Pik7, 0),;
      		      Alo8   WITH MAX(AlO8 - ORDLINE.Pik8, 0),;
      		      TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                              Alo5 + Alo6 + Alo7 + Alo8                          
          =gfObj_Lock(.F.)
        ENDIF
        
        *-- Update allocated quantities in STYDYE if dyelots
        IF llUseDyes .AND. Style.cDye_flg = 'Y' .AND.;
          SEEK(m.Style + ORDLINE.cWareCode + ORDLINE.Dyelot) .AND.;
          gfObj_Lock(.T.)
          REPLACE Alo1   WITH MAX(Alo1 - ORDLINE.Pik1, 0),;
            	  Alo2   WITH MAX(Alo2 - ORDLINE.Pik2, 0),;
      		      Alo3   WITH MAX(Alo3 - ORDLINE.Pik3, 0),;
      		      Alo4   WITH MAX(Alo4 - ORDLINE.Pik4, 0),;
      		      Alo5   WITH MAX(Alo5 - ORDLINE.Pik5, 0),;
      		      Alo6   WITH MAX(Alo6 - ORDLINE.Pik6, 0),;
      	 	      Alo7   WITH MAX(Alo7 - ORDLINE.Pik7, 0),;
      		      Alo8   WITH MAX(AlO8 - ORDLINE.Pik8, 0),;
      		      TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                              Alo6 + Alo7 + Alo8                          
          =gfObj_Lock(.F.)
        ENDIF  
      
        *-- Update the style  record in STYLE file.
        SELECT STYLE
        IF SEEK(m.Style) .AND. gfObj_Lock(.T.)
          REPLACE Alo1   WITH MAX(Alo1 - ORDLINE.Pik1, 0),;
          	      Alo2   WITH MAX(Alo2 - ORDLINE.Pik2, 0),;
        		  Alo3   WITH MAX(Alo3 - ORDLINE.Pik3, 0),;
        		  Alo4   WITH MAX(Alo4 - ORDLINE.Pik4, 0),;
        		  Alo5   WITH MAX(Alo5 - ORDLINE.Pik5, 0),;
        		  Alo6   WITH MAX(Alo6 - ORDLINE.Pik6, 0),;
        	 	  Alo7   WITH MAX(Alo7 - ORDLINE.Pik7, 0),;
        		  Alo8   WITH MAX(AlO8 - ORDLINE.Pik8, 0),;
        		  TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                              Alo5 + Alo6   + Alo7 + Alo8                          
           =gfObj_Lock(.F.)
         ENDIF
        *C037816,1 MHM 04/06/2004 Custom Relase Bin Location For David Luke[Start]
        *B039660,1 NNA 02/14/2006 (START) Transfer all the bin location's Programs To Binmain.prg instead of Davmain.prg
        IF ASCAN(laEvntTrig,PADR("ALRELORD",10)) <> 0
          = gfDoTriger("ALORDAL",PADR("ALRELORD",10))
        ENDIF 
        *B039660,1 NNA (END)
        *C037816,1 MHM 04/06/2004 [End]
         
        IF !EMPTY(m.AltStyle)
          =lpSwchSty(m.Style, m.AltStyle, m.cWareCode, ORDHDR.cWareCode) 
        ELSE
          *-- Check of the released pick ticket's warehouse
          *-- is different from the warehouse of the order,
          *-- if so, switch the ordered quantities to the 
          *-- order's warehouse.
          IF m.cWareCode <> ORDHDR.cWareCode 
            *B037363,1 HBG 25/12/2003 Add parammeter to fix bug of The main style did not come back when releasing the Piktkt [Begin]
            *=lpMoveOrd(m.Style, m.Style, m.cWareCode, ORDHDR.cWareCode)
            =lpMoveOrd(m.Style, m.Style, m.cWareCode, ORDHDR.cWareCode,'R')
            *B037363,1 [End]
          ENDIF
        ENDIF
  
        *-- Update ORDLINE
        SELECT ORDLINE
        lcCrPkTkt   = ORDLINE.PikTkt
		m.cWareCode = ORDHDR.cWareCode
		*B603240,1 Do not clear the dyelot field. Comment the line
		*m.Dyelot    = IIF(EMPTY(CutTkt), SPACE(10), ORDLINE.Dyelot)
		*B603240,1 end

        *E126905,1 EIH 04/11/2005 [Begin] Case of Release .
        m.cAllocatBy  = ' '
        *E126905,1 EIH 04/11/2005 [End]
        GATHER MEMVAR MEMO

        *-- Check for any released line to be inserted in PIKLINE file 
        =lfRelPktkt(lcCrPkTkt)
  
      *Ren
      CASE llGenPkTk .AND. EMPTY(&lc_TmpOrdL..cStatus) .AND. &lc_TmpOrdL..lAllocated;
        .AND. ORDLINE.PikTkt = REPLICATE("*",FSIZE("PIKTKT"))
        IF lcChgPkTKt <> m.Order + m.Store + m.cWareCode 
          lcChgPkTKt = m.Order + m.Store + m.cWareCode 
          lcPikTktNo = lfGetPkTkt(m.Order, ORDHDR.cDivision, m.Store, m.cWareCode)
          
          
          *E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt. [Begin]
          IF 'AS' $ gcCmpModules AND llSndPkFld AND !EMPTY(lcPikTktNo)
            =SEEK(ALLTRIM(m.Order+lcPikTktNo),'PIKTKT')
            IF PIKTKT.csndpkt = 'Y'
              =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' can not add ')
              llCSave = .F.
              *-- Restore relations
			  SET ORDER TO TAG ORDLINE IN (lc_TmpPkLn)
			  SELECT (lc_TmpOrdL)
			  SET RELATION TO
			  SET ORDER TO TAG ORDLINE
			  SELECT ORDLINE
			  SET RELATION TO cOrdType + Order + STR(LineNo,6) INTO (lc_TmpOrdL) ADDITIVE
			  SET ORDER TO (lnCurTag)
              SELECT ORDHDR
              RETURN
            ENDIF
          ENDIF
          *E130074,1 EIH 01/02/2006 [End]


        ENDIF               
        IF !SEEK(m.Order + lcPikTktNo, 'PIKTKT')
          *B802823,1  Get Cust PO from ORDLINE if the order is multi PO. [ Begin ]
          *INSERT INTO PIKTKT;
          *(Piktkt, Account, Store, Order, Date, cWareCode, CustPo, Status);
          *VALUES;
          *(lcPikTktNo, laData[2], m.Store, m.Order, gdSysDate, m.cWarecode, ORDHDR.CustPo, 'O')
          INSERT INTO PIKTKT;
               (Piktkt, Account, Store, Order, Date, cWareCode, CustPo, Status);
               VALUES;
              (lcPikTktNo, laData[2], m.Store, m.Order, gdSysDate, m.cWarecode,IIF(ORDHDR.MultiPO,Ordline.CustPo,ORDHDR.CustPo), 'O')
          *B802823,1  [ End ]
          =gfAdd_Info('PIKTKT')
          *E123070,1 WAM 07/12/2004 Generate 940 EDI transaction for pick tickets directed to 3PL providers.
          IF 'AS' $ gcCmpModules AND SEEK(m.cWareCode,'WareHous') AND  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT') AND ;
             SEEK(EDIACPRT.cPartCode+'940','EDIPD')
            SELECT EDITRANS
            IF !SEEK('940'+PADR(lcPikTktNo,40)+'W'+WareHous.cThrdPLPr)
              INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
                                     ('940',lcPikTktNo,'W',WareHous.cThrdPLPr,EDIACPRT.lInterComp)
            ENDIF
            REPLACE cStatus WITH 'N'
            =gfAdd_Info('EDITRANS')
          ENDIF
          *E123070,1 WAM 07/12/2004 (End)
        ENDIF  
        *-- Update ORDLINE
        SELECT ORDLINE
        *B603240,1 Update order line pick date with that of the picking ticket
        *REPLACE PikTkt WITH lcPikTktNo 
        *B037363,4 HBG 25/12/2003 Fix bug of not saveing modified Qty incase of generating Piktkt for Allocated line[Begin]
        REPLACE PikTkt  WITH lcPikTktNo,;
        		PikDate WITH PIKTKT.Date
        m.PikTkt  = lcPikTktNo
        m.PikDate = PIKTKT.Date
	    GATHER MEMVAR MEMO
        *B037363,4 [End]        		
        *B603240,1 Update order line pick date with that of the picking ticket
  	    *E126905,1 EIH 04/11/2005 [Begin] Case of picking .
        REPLACE cAllocatBy      WITH  IIF( TotPik > 0 , 'O' , ' ')
        *E126905,1 EIH 04/11/2005 [End]
        *---
      CASE Picked
        *-- Check it stock is still available for allocation
        IF lfChkStock() 
          *-- If this is a new pick, generate a new picking ticket
           
          IF !ORDLINE.Picked .OR. lfLinIsChg()
            *-- Generate a different picking ticket per store/warehouse
            *Ren 
            IF llGenPkTk 
              IF lcChgPkTKt <> m.Order + m.Store + m.cWareCode 
                lcChgPkTKt = m.Order + m.Store + m.cWareCode 
                lcPikTktNo = IIF(EMPTY(m.Piktkt),;
	                            lfGetPkTkt(m.Order, ORDHDR.cDivision, m.Store, m.cWareCode),;
 	  	                     m.PikTkt)
 	  	        
 	  	        *E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt. [Begin]
                IF 'AS' $ gcCmpModules AND llSndPkFld AND !EMPTY(lcPikTktNo)
                  =SEEK(ALLTRIM(m.Order+lcPikTktNo),'PIKTKT')
                  IF PIKTKT.csndpkt = 'Y'
                    llCSave = .F.                                     
                    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' can not add ')
                    *-- Restore relations
					SET ORDER TO TAG ORDLINE IN (lc_TmpPkLn)
					SELECT (lc_TmpOrdL)
					SET RELATION TO
					SET ORDER TO TAG ORDLINE
					SELECT ORDLINE
					SET RELATION TO cOrdType + Order + STR(LineNo,6) INTO (lc_TmpOrdL) ADDITIVE
					SET ORDER TO (lnCurTag)
                    SELECT ORDHDR
                    RETURN
                  ENDIF
                ENDIF
                *E130074,1 EIH 01/02/2006 [End]


 	  	      ENDIF               
              IF !SEEK(m.Order + lcPikTktNo, 'PIKTKT')
                *B802823,1  Get The custom PO from ORDLINE if the order is multi PO. [ Begin ]
                *INSERT INTO PIKTKT;
                *(Piktkt, Account, Store, Order, Date, cWareCode, CustPo, Status);
                *VALUES;
                *(lcPikTktNo, laData[2], m.Store, m.Order, gdSysDate, m.cWarecode, ORDHDR.CustPo, 'O')
                INSERT INTO PIKTKT;
                (Piktkt, Account, Store, Order, Date, cWareCode, CustPo, Status);
                VALUES;
                (lcPikTktNo, laData[2], m.Store, m.Order, gdSysDate, m.cWarecode,IIF(ORDHDR.MultiPO,Ordline.CustPo,ORDHDR.CustPo), 'O')                
                *B802823,1 [ End]
                =gfAdd_Info('PIKTKT')
              ENDIF  
 	  	    ELSE
 	  	      lcPikTktNo = REPLICATE("*", FSIZE("PIKTKT","PIKTKT"))
 	  	      SET ORDER TO TAG PIKTKT IN PIKTKT
 	  	      IF !SEEK(lcPikTktNo, 'PIKTKT')
                INSERT INTO PIKTKT (Piktkt) VALUES (lcPikTktNo)
                =gfAdd_Info('PIKTKT')
              ENDIF  
              SET ORDER TO TAG ORDPIK IN PIKTKT
 	  	    ENDIF
 	  	    *Ren end
          ELSE
            lcPikTktNo = m.Piktkt  
          ENDIF   &&EndIF !ORDLINE.Picked
          
          *-- Check if the style has changed
          IF lfLinIsChg()
            *-- If the style, warehouse, or dyelot has changed,
            *-- move the ordered quantities to the new selection
            *B603240,1 Do not pass dyelots since dyelots ordered
            *B603240,1 are not changed
            *=lpMoveOrd(ORDLINE.Style, m.Style, ORDLINE.cWareCode, m.cWareCode,;
            		   ORDLINE.Dyelot, m.Dyelot)
            *B037363,1 HBG 25/12/2003 Add parammeter to fix bug of The main style did not come back when releasing the Piktkt [Begin]		   
			*=lpMoveOrd(ORDLINE.Style, m.Style, ORDLINE.cWareCode, m.cWareCode,'A')
			=lpMoveOrd(ORDLINE.Style, m.Style, ORDLINE.cWareCode, m.cWareCode,'A')
			*B037363,1 [End]

            *B603240,1 end		     
          ENDIF
          *-- If the ordered quantities have changed, update them
          *-- in the order header
          IF cStatus = 'M'
            SELECT ORDHDR
            lnDiffQty   = m.TotQty - ORDLINE. TOTQTY
            lnDiffAmnt  = lnDiffQty * ORDLINE.PRICE 
            REPLACE BOOK    WITH BOOK    + lnDiffQty  ,;
                    BOOKAMT WITH BOOKAMT + lnDiffAmnt,;
                    OPEN    WITH OPEN    + lnDiffQty ,;
                    OPENAMT WITH OPENAMT + lnDiffAmnt
            
            *-- Update allocated and ordered quantities in STYDYE
            *-- Update Warehouse record in STYDYE file.
            SELECT STYDYE
            IF SEEK(m.Style + m.cWareCode + SPACE(10), 'STYDYE')
              *B606778,1 TMI [Start] Fix the replace for alo only
              *REPLACE Ord1   WITH Ord1   + MAX(m.Qty1  -ORDLINE.Qty1,   0),;
	                  Ord2   WITH Ord2   + MAX(m.Qty2  -ORDLINE.Qty2,   0),;
            		  Ord3   WITH Ord3   + MAX(m.Qty3  -ORDLINE.Qty3,   0),;
             	      Ord4   WITH Ord4   + MAX(m.Qty4  -ORDLINE.Qty4,   0),;
        		      Ord5   WITH Ord5   + MAX(m.Qty5  -ORDLINE.Qty5,   0),;
        		      Ord6   WITH Ord6   + MAX(m.Qty6  -ORDLINE.Qty6,   0),;
      	 	          Ord7   WITH Ord7   + MAX(m.Qty7  -ORDLINE.Qty7,   0),;
      		          Ord8   WITH Ord8   + MAX(m.Qty8  -ORDLINE.Qty8,   0),;
      		          TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 +   ;
                                  Ord5 + Ord6   + Ord7 + Ord8,;
                      Alo1   WITH Alo1   + MAX(m.Pik1  -ORDLINE.Pik1,   0),;
        	          Alo2   WITH Alo2   + MAX(m.Pik2  -ORDLINE.Pik2,   0),;
      		          Alo3   WITH Alo3   + MAX(m.Pik3  -ORDLINE.Pik3,   0),;
      		          Alo4   WITH Alo4   + MAX(m.Pik4  -ORDLINE.Pik4,   0),;
      		          Alo5   WITH Alo5   + MAX(m.Pik5  -ORDLINE.Pik5,   0),;
      		          Alo6   WITH Alo6   + MAX(m.Pik6  -ORDLINE.Pik6,   0),;
      	 	          Alo7   WITH Alo7   + MAX(m.Pik7  -ORDLINE.Pik7,   0),;
      		          Alo8   WITH Alo8   + MAX(m.Pik8  -ORDLINE.Pik8,   0),;
      		          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                  Alo5 + Alo6 + Alo7 + Alo8                          
              REPLACE Ord1   WITH Ord1   + MAX(m.Qty1  -ORDLINE.Qty1,   0),;
	                  Ord2   WITH Ord2   + MAX(m.Qty2  -ORDLINE.Qty2,   0),;
            		  Ord3   WITH Ord3   + MAX(m.Qty3  -ORDLINE.Qty3,   0),;
             	      Ord4   WITH Ord4   + MAX(m.Qty4  -ORDLINE.Qty4,   0),;
        		      Ord5   WITH Ord5   + MAX(m.Qty5  -ORDLINE.Qty5,   0),;
        		      Ord6   WITH Ord6   + MAX(m.Qty6  -ORDLINE.Qty6,   0),;
      	 	          Ord7   WITH Ord7   + MAX(m.Qty7  -ORDLINE.Qty7,   0),;
      		          Ord8   WITH Ord8   + MAX(m.Qty8  -ORDLINE.Qty8,   0),;
      		          TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 +   ;
                                  Ord5 + Ord6   + Ord7 + Ord8
              REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1 ,  0),;
        	          Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2 ,  0),;
      		          Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3 ,  0),;
      		          Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4 ,  0),;
      		          Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5 ,  0),;
      		          Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6 ,  0),;
      	 	          Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7 ,  0),;
      		          Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8 ,  0),;
      		          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                  Alo5 + Alo6 + Alo7 + Alo8    
              *B606778,1 TMI [End  ]Fix the replace for alo                                                                                           
            ENDIF
            *-- If dyelots, update dyelot record in STYDYE file
            *-- with the allocated quantities only
            IF llUseDyes .AND. STYLE.cDye_flg = 'Y' .AND.;
              SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
              *B606778,1 TMI [Start] Fix the replace for alo
              *REPLACE Alo1   WITH Alo1   + MAX(m.Pik1  -ORDLINE.Pik1,   0),;
    		          Alo2   WITH Alo2   + MAX(m.Pik2  -ORDLINE.Pik2,   0),;
      		          Alo3   WITH Alo3   + MAX(m.Pik3  -ORDLINE.Pik3,   0),;
      		          Alo4   WITH Alo4   + MAX(m.Pik4  -ORDLINE.Pik4,   0),;
      		          Alo5   WITH Alo5   + MAX(m.Pik5  -ORDLINE.Pik5,   0),;
      		          Alo6   WITH Alo6   + MAX(m.Pik6  -ORDLINE.Pik6,   0),;
      	 	          Alo7   WITH Alo7   + MAX(m.Pik7  -ORDLINE.Pik7,   0),;
      		          Alo8   WITH Alo8   + MAX(m.Pik8  -ORDLINE.Pik8,   0),;
      		          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                  Alo5 + Alo6   + Alo7 + Alo8                          
              REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1,  0),;
    		          Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2,  0),;
      		          Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3,  0),;
      		          Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4,  0),;
      		          Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5,  0),;
      		          Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6,  0),;
      	 	          Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7,  0),;
      		          Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8,  0),;
      		          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                  Alo5 + Alo6   + Alo7 + Alo8                          
              *B606778,1 TMI [End  ] Fix the replace for alo
            ENDIF     
            *-- Update the style  record in STYLE file.
            SELECT STYLE
            *B606778,1 TMI [Start] Fix the replace for alo
            *REPLACE Ord1   WITH Ord1   + MAX(m.Qty1  -ORDLINE.Qty1,   0),;
             		Ord2   WITH Ord2   + MAX(m.Qty2  -ORDLINE.Qty2,   0),;
            	    Ord3   WITH Ord3   + MAX(m.Qty3  -ORDLINE.Qty3,   0),;
            		Ord4   WITH Ord4   + MAX(m.Qty4  -ORDLINE.Qty4,   0),;
            		Ord5   WITH Ord5   + MAX(m.Qty5  -ORDLINE.Qty5,   0),;
            		Ord6   WITH Ord6   + MAX(m.Qty6  -ORDLINE.Qty6,   0),;
            	 	Ord7   WITH Ord7   + MAX(m.Qty7  -ORDLINE.Qty7,   0),;
            		Ord8   WITH Ord8   + MAX(m.Qty8  -ORDLINE.Qty8,   0),;
            		TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + ;
                                Ord5 + Ord6   + Ord7 + Ord8,;
                    Alo1   WITH Alo1   + MAX(m.Pik1  -ORDLINE.Pik1,   0),;
    	      	    Alo2   WITH Alo2   + MAX(m.Pik2  -ORDLINE.Pik2,   0),;
            		Alo3   WITH Alo3   + MAX(m.Pik3  -ORDLINE.Pik3,   0),;
              		Alo4   WITH Alo4   + MAX(m.Pik4  -ORDLINE.Pik4,   0),;
      		  		Alo5   WITH Alo5   + MAX(m.Pik5  -ORDLINE.Pik5,   0),;
      		  		Alo6   WITH Alo6   + MAX(m.Pik6  -ORDLINE.Pik6,   0),;
      	 	  		Alo7   WITH Alo7   + MAX(m.Pik7  -ORDLINE.Pik7,   0),;
      		  		Alo8   WITH Alo8   + MAX(m.Pik8  -ORDLINE.Pik8,   0),;
      		  		TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                Alo5 + Alo6   + Alo7 + Alo8                          
            REPLACE Ord1   WITH Ord1   + MAX(m.Qty1  -ORDLINE.Qty1,   0),;
             		Ord2   WITH Ord2   + MAX(m.Qty2  -ORDLINE.Qty2,   0),;
            	    Ord3   WITH Ord3   + MAX(m.Qty3  -ORDLINE.Qty3,   0),;
            		Ord4   WITH Ord4   + MAX(m.Qty4  -ORDLINE.Qty4,   0),;
            		Ord5   WITH Ord5   + MAX(m.Qty5  -ORDLINE.Qty5,   0),;
            		Ord6   WITH Ord6   + MAX(m.Qty6  -ORDLINE.Qty6,   0),;
            	 	Ord7   WITH Ord7   + MAX(m.Qty7  -ORDLINE.Qty7,   0),;
            		Ord8   WITH Ord8   + MAX(m.Qty8  -ORDLINE.Qty8,   0),;
            		TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + ;
                                Ord5 + Ord6   + Ord7 + Ord8
            REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1,  0),;
    	      	    Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2,  0),;
            		Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3,  0),;
              		Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4,  0),;
      		  		Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5,  0),;
      		  		Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6,  0),;
      	 	  		Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7,  0),;
      		  		Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8,  0),;
      		  		TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                Alo5 + Alo6   + Alo7 + Alo8                          
            *B606778,1 TMI [End  ] Fix the replace for alo
                                  
            *-- Update style history for ordered quantities  
            *-- Get the year/period of ORDHDR entered date so as to update
            *-- style history.
	    	IF CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod ','',.T.)
              *-- Get the year/period of ORDHDR entered date so as to update
              =gfOpenFile(gcDataDir+"ICSTYHST", "STYHST", "SH")
              IF SEEK(m.Style + lcGlYear)
                REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + lnDiffQty,;
          	            nOrdQty            WITH nOrdQty            + lnDiffQty,;
                    	nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnDiffAmnt,;
		                nOrdAmt            WITH nOrdAmt            + lnDiffAmnt
	          ENDIF     && IF SEEK(m.Style 
			  =gfOpenFile(gcDataDir+"ARCUSHST", "ACTHST", "SH")
              IF SEEK(m.Account + lcGlYear)
                 REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod ;
                        					    + lnDiffQty,;
                        nOrdQty            WITH nOrdQty            ;
						 					    + lnDiffQty,;
                        nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod ;
				  		                        + lnDiffAmnt,;
                        nOrdAmt            WITH nOrdAmt            ;
				  								+ lnDiffAmnt
              ENDIF
            ENDIF                                  
          ELSE
            *-- Update allocated and ordered quantities in STYDYE
            *-- Update Warehouse record in STYDYE file.
            SELECT STYDYE
            *B603240,1 Position STYDYE record on the warehouse record
            IF SEEK(m.Style + m.cWareCode + SPACE(10)) 
            *B603240,1 end
              
              *B603332,1 KHM 03/12/2000 (Begin) Changing the replacement
              *B603332,1                of the allocation qtys by adding 
              *B603332,1                replacing MAX(Alox+m.Pikx-OrdLine.Pikx)
              *B603332,1                 instead of Alox+MAX(m.Pikx  -ORDLINE.Pikx,0) 
              *B603332,1                in order to handle the case of 
              *B603332,1                modifying the allocated qty.
              *REPLACE Alo1   WITH Alo1   + MAX(m.Pik1  -ORDLINE.Pik1,   0),;
        	          Alo2   WITH Alo2   + MAX(m.Pik2  -ORDLINE.Pik2,   0),;
      		          Alo3   WITH Alo3   + MAX(m.Pik3  -ORDLINE.Pik3,   0),;
      		          Alo4   WITH Alo4   + MAX(m.Pik4  -ORDLINE.Pik4,   0),;
        		      Alo5   WITH Alo5   + MAX(m.Pik5  -ORDLINE.Pik5,   0),;
        		      Alo6   WITH Alo6   + MAX(m.Pik6  -ORDLINE.Pik6,   0),;
      	 	          Alo7   WITH Alo7   + MAX(m.Pik7  -ORDLINE.Pik7,   0),;
      		          Alo8   WITH Alo8   + MAX(m.Pik8  -ORDLINE.Pik8,   0),;
      		          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                  Alo5 + Alo6 + Alo7 + Alo8                          
              REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1,0),;
        	          Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2,0),;
      		          Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3,0),;
      		          Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4,0),;
        		      Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5,0),;
        		      Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6,0),;
      	 	          Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7,0),;
      		          Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8,0),;
      		          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                  Alo5 + Alo6 + Alo7 + Alo8                          
            *B603332,1 KHM 03/12/2000 (End)

            *B603240,1 Add an ENDIF
            ENDIF  
            *B603240,1 end
            *-- If dyelots, update dyelot record in STYDYE file
            IF llUseDyes .AND. STYLE.cDye_flg = 'Y' .AND.;
              SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
              *B606778,1 TMI [Start] Fix the replace for alo
              *REPLACE Alo1   WITH Alo1   + MAX(m.Pik1  -ORDLINE.Pik1,   0),;
    		          Alo2   WITH Alo2   + MAX(m.Pik2  -ORDLINE.Pik2,   0),;
      		          Alo3   WITH Alo3   + MAX(m.Pik3  -ORDLINE.Pik3,   0),;
      		          Alo4   WITH Alo4   + MAX(m.Pik4  -ORDLINE.Pik4,   0),;
      		          Alo5   WITH Alo5   + MAX(m.Pik5  -ORDLINE.Pik5,   0),;
      		          Alo6   WITH Alo6   + MAX(m.Pik6  -ORDLINE.Pik6,   0),;
      	 	          Alo7   WITH Alo7   + MAX(m.Pik7  -ORDLINE.Pik7,   0),;
      		          Alo8   WITH Alo8   + MAX(m.Pik8  -ORDLINE.Pik8,   0),;
      		          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                  Alo5 + Alo6   + Alo7 + Alo8                          
              REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1,  0),;
    		          Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2,  0),;
      		          Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3,  0),;
      		          Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4,  0),;
      		          Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5,  0),;
      		          Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6,  0),;
      	 	          Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7,  0),;
      		          Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8,  0),;
      		          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                  Alo5 + Alo6   + Alo7 + Alo8                          
              *B606778,1 TMI [End  ] Fix the replace for alo
	        ENDIF
       
            *-- Update the style  record in STYLE file.
            SELECT STYLE
            *B603332,1 KHM 03/12/2000 (Begin) Changing the replacement
            *B603332,1                of the allocation qtys by adding 
            *B603332,1                replacing MAX(Alox+m.Pikx-OrdLine.Pikx)
            *B603332,1                 instead of Alox+MAX(m.Pikx  -ORDLINE.Pikx,0) 
            *B603332,1                in order to handle the case of 
            *B603332,1                modifying the allocated qty.
            *REPLACE Alo1   WITH Alo1   + MAX(m.Pik1  -ORDLINE.Pik1,   0),;
    	      	    Alo2   WITH Alo2   + MAX(m.Pik2  -ORDLINE.Pik2,   0),;
            		Alo3   WITH Alo3   + MAX(m.Pik3  -ORDLINE.Pik3,   0),;
              		Alo4   WITH Alo4   + MAX(m.Pik4  -ORDLINE.Pik4,   0),;
      		  		Alo5   WITH Alo5   + MAX(m.Pik5  -ORDLINE.Pik5,   0),;
      		  		Alo6   WITH Alo6   + MAX(m.Pik6  -ORDLINE.Pik6,   0),;
      	 	  		Alo7   WITH Alo7   + MAX(m.Pik7  -ORDLINE.Pik7,   0),;
      		  		Alo8   WITH Alo8   + MAX(m.Pik8  -ORDLINE.Pik8,   0),;
      		  		TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                Alo5 + Alo6   + Alo7 + Alo8                          
            REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1,0),;
    	      	    Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2,0),;
            		Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3,0),;
              		Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4,0),;
      		  		Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5,0),;
      		  		Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6,0),;
      	 	  		Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7,0),;
      		  		Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8,0),;
      		  		TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + ;
                                Alo5 + Alo6   + Alo7 + Alo8                          
          *B603332,1 KHM 03/12/2000 (End)
          ENDIF
              
          *-- Update ORDLINE
          SELECT ORDLINE
          lcCrPkTkt   = ORDLINE.PikTkt

          *B803064,1 KHM 03/12/2000 (Begin) Changing the assingment of the
          *B803064,1                m.Piktkt by assigning the lcCrPkTkt in 
          *B803064,1                cas of empty lcPikTktNo becaue when 
          *B803064,1                generating a P/T then release it the 
          *B803064,1                regenerate it in the same session the 
          *B803064,1                lcPikTktNo variable is empty.
          *m.PikTkt    = lcPikTktNo
          m.PikTkt = IIF(EMPTY(lcPikTktNo),lcCrPkTkt,lcPikTktNo)
          *B803064,1 KHM 03/12/2000 (End)

          *B603240,1 Update order line pick date with that of the picking ticket
          *m.PikDate   = gdSysDate
          m.PikDate = IIF(m.PikTkt <> "******", PIKTKT.Date, gdSysDate)
          *B603240,1 Update order line pick date with that of the picking ticket

          *C037816,1 MHM 04/06/2004 Custom Save Bin Location For David Luke[Start]
          *IF ASCAN(laEvntTrig,PADR("ALSAVORD",10)) <> 0
          *  = gfDoTriger("ALORDAL",PADR("ALSAVORD",10))
          *ENDIF 
          *C037816,1 MHM [End]
          *B130690,1  TMI [Start] Fix a bug at DCC that when release a picktickit and repick in the same session the data in whbinloc goes wrong
          IF ASCAN(laEvntTrig,PADR("ALRELOR2",10)) <> 0
            = gfDoTriger("ALORDAL",PADR("ALRELOR2",10))
          ENDIF 
          *B130690,1  TMI [End  ] 

          GATHER MEMVAR MEMO
          *E126905,1 EIH 04/11/2005 [Begin] Case of Allocated .
          REPLACE cAllocatBy      WITH  IIF( TotPik > 0 , 'O' , ' ')
          *E126905,1 EIH 04/11/2005 [End]

          *E123070,1 WAM 07/12/2004 Generate 940 EDI transaction for pick tickets directed to 3PL providers.
          
          IF lcPikTktNo <> '******' AND 'AS' $ gcCmpModules AND SEEK(m.cWareCode,'WareHous') AND ;
            SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT') AND  SEEK(EDIACPRT.cPartCode+'940','EDIPD')
            SELECT EDITRANS
            IF !SEEK('940'+PADR(lcPikTktNo,40)+'W'+WareHous.cThrdPLPr)
              INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
                                     ('940',lcPikTktNo,'W',WareHous.cThrdPLPr,EDIACPRT.lInterComp)
            ENDIF
            REPLACE cStatus WITH 'N'
            =gfAdd_Info('EDITRANS')
          ENDIF
          *E123070,1 WAM 07/12/2004 (End)

          *-- Check for any released line to be inserted in PIKLINE file 
          =lfRelPktkt(lcCrPkTkt)
        
        ENDIF
    ENDCASE
    *C102424,1 (Begin) Add record for each piktkt in ALSHPINF for Ogio.
    IF ASCAN(laEvntTrig,PADR("UPDSHP",10)) <> 0
      = gfDoTriger("ALORDAL",PADR("UPDSHP",10))
    ENDIF  
    *C102424,1 (Begin)
    *C037816,1 MHM 04/06/2004 Custom Save Bin Location For David Luke[Start]
    *B039660,1 NNA 02/14/2006 (START) Transfer all the bin location's Programs To Binmain.prg instead of Davmain.prg
    *T20060817.0014   TMI [Start] move this trigger out the loop to enhance speed
    *IF ASCAN(laEvntTrig,PADR("ALSAVORD",10)) <> 0
    *  = gfDoTriger("ALORDAL",PADR("ALSAVORD",10))
    *ENDIF 
    *T20060817.0014   TMI [End  ] 
    *B039660,1 NNA (END)
    *C037816,1 MHM [End]
    
    *E301340,1 WAB-1-Check if there is shaving in current order line by check 
    *E301340,1       if any qty? > pik?
    *E301340,1     2-if there is shaving and the setting of order shaving is 
    *E301340,1       not 'Never'	-------> call Function to update files
    *E301340,1 WAB - START
    llShave = lfCanShave()
    IF llShave AND lnOrdShv <> 3 AND !EMPTY(m.cReason)
      =lfUpdCanl()
    ENDIF
    *E301340,1 WAB - END
  ENDSCAN
  
  *T20060817.0014   TMI [Start] bin location pik process
  IF ASCAN(laEvntTrig,PADR("ALSAVORD",10)) <> 0
    = gfDoTriger("ALORDAL",PADR("ALSAVORD",10))
  ENDIF 
  *T20060817.0014   TMI [End  ] 

  *-- Present the user with generated pick tickets (in a dialog or browse))
  *-- Go back to view mode:

  *-- Restore relations
  SET ORDER TO TAG ORDLINE IN (lc_TmpPkLn)
  SELECT (lc_TmpOrdL)
  SET RELATION TO
  SET ORDER TO TAG ORDLINE
  SELECT ORDLINE
  SET RELATION TO cOrdType + Order + STR(LineNo,6) INTO (lc_TmpOrdL) ADDITIVE
  SET ORDER TO (lnCurTag)

  SELECT ORDHDR
  *-- Mark the incomplete session record as Complete
  =lfClsUnCmS("C")

  *C200489,1 WAB (Start)
  IF ASCAN(laEvntTrig,PADR("ALCRTSAV",10)) <> 0
    =gfDoTriger("ALORDAL",PADR("ALCRTSAV",10))
  ENDIF
  *C200489,1 WAB (End)
 
ELSE
  *-- Cancel saving process
  llCSave   = .F.      
ENDIF


SELECT ORDHDR

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Cancel allocation
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpClsScr()
*!*************************************************************
FUNCTION lpClsScr
PRIVATE lnCurAlias

*-- This function is called only from Edit mode
*-- Clear temporary files if there are any records
*-- Update UNCMSESS with 'I'nitial
=lfClsUnCmS("I")

*-- Select base file, and refresh laData elements
SELECT ORDHDR
SCATTER FIELDS &lcScFields TO laData 

*!*************************************************************
*! Name      : lfvPick
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Close program or cancel order modification
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfGetScale
lnScaleCnt = SCALE.Cnt
IF !EMPTY(m.AltStyle) .AND.;
  SEEK(m.AltStyle, 'STYLE')
  lnScaleCnt = MIN(SCALE.Cnt, lnScaleCnt)
ENDIF


*!*************************************************************
*! Name      : lfvPick
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Close program or cancel order modification
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfvForceAl
lnCurAlias = SELECT(0)
*-- If the line is already picked, change allocated quantities
*-- according to the status of the button
IF m.Picked
  *-- Insert a record in temp StyDye file if one is not found
  IF !SEEK(m.Style + m.cWareCode + SPACE(10), lc_TmpStyD) .AND.;
    SEEK(m.Style + m.cWareCode + SPACE(10), 'STYDYE')
 	SELECT STYDYE
    SCATTER TO laStyDye MEMVAR
    INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye 
  ENDIF
  IF llUseDyes .AND. STYLE.cDye_flg = 'Y' .AND.;
    !SEEK(m.Style + m.cWareCode + m.Dyelot, lc_TmpStyD) .AND.;
    SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
    SELECT STYDYE
    SCATTER TO laStyDye MEMVAR
    INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye 
  ENDIF  
  
  IF m.lForceAlc
    lnPik1      = MAX(m.Qty1 - m.Pik1, 0)
    lnPik2      = MAX(m.Qty2 - m.Pik2, 0)
    lnPik3      = MAX(m.Qty3 - m.Pik3, 0)
    lnPik4      = MAX(m.Qty4 - m.Pik4, 0)
    lnPik5      = MAX(m.Qty5 - m.Pik5, 0)
    lnPik6      = MAX(m.Qty6 - m.Pik6, 0)
    lnPik7      = MAX(m.Qty7 - m.Pik7, 0)
    lnPik8      = MAX(m.Qty8 - m.Pik8, 0)

    *E301310,1 Check for approval amount. [Begin]
    lnTotPkQty = lnPik1 + lnPik2 + lnPik3 + lnPik4 + ;
                 lnPik5 + lnPik6 + lnPik7 + lnPik8
    IF llChkAprov AND !lfAprvLine(m.TotQty - m.TotPik)
      RETURN
    ENDIF
    *E301310,1 Check for approval amount. [End  ]

    m.nForcdQty = lnPik1 + lnPik2 + lnPik3 + lnPik4 + ;
                  lnPik5 + lnPik6 + lnPik7 + lnPik8
  *-- Else reduce allocated quantities to available quantities
  ELSE
    SELECT (lc_TmpStyD)
    *IF EMPTY(m.PikTkt)
      *E500304,1 WAB - Calculate the avialble qty (stk-alo) by calling function
      *E500304,1 WAB - lftotavlbl() to get the available qty
      *E500304,1 WAB - START
      *lnPik1      = - (m.Pik1 - MIN(m.Qty1, MAX(Stk1- Alo1+m.Pik1, 0)))
	  *lnPik2      = - (m.Pik2 - MIN(m.Qty2, MAX(Stk2- Alo2+m.Pik2, 0)))
	  *lnPik3      = - (m.Pik3 - MIN(m.Qty3, MAX(Stk3- Alo3+m.Pik3, 0)))
	  *lnPik4      = - (m.Pik4 - MIN(m.Qty4, MAX(Stk4- Alo4+m.Pik4, 0)))
	  *lnPik5      = - (m.Pik5 - MIN(m.Qty5, MAX(Stk5- Alo5+m.Pik5, 0)))
	  *lnPik6      = - (m.Pik6 - MIN(m.Qty6, MAX(Stk6- Alo6+m.Pik6, 0)))
	  *lnPik7      = - (m.Pik7 - MIN(m.Qty7, MAX(Stk7- Alo7+m.Pik7, 0)))
	  *lnPik8      = - (m.Pik8 - MIN(m.Qty8, MAX(Stk8- Alo8+m.Pik8, 0)))
      lnPik1      = - (m.Pik1 - MIN(m.Qty1, MAX(lfTotAvlbl('1')+m.Pik1, 0)))
	  lnPik2      = - (m.Pik2 - MIN(m.Qty2, MAX(lfTotAvlbl('2')+m.Pik2, 0)))
	  lnPik3      = - (m.Pik3 - MIN(m.Qty3, MAX(lfTotAvlbl('3')+m.Pik3, 0)))
	  lnPik4      = - (m.Pik4 - MIN(m.Qty4, MAX(lfTotAvlbl('4')+m.Pik4, 0)))
	  lnPik5      = - (m.Pik5 - MIN(m.Qty5, MAX(lfTotAvlbl('5')+m.Pik5, 0)))
	  lnPik6      = - (m.Pik6 - MIN(m.Qty6, MAX(lfTotAvlbl('6')+m.Pik6, 0)))
	  lnPik7      = - (m.Pik7 - MIN(m.Qty7, MAX(lfTotAvlbl('7')+m.Pik7, 0)))
	  lnPik8      = - (m.Pik8 - MIN(m.Qty8, MAX(lfTotAvlbl('8')+m.Pik8, 0)))
      *E500304,1 WAB - END
      
      *E301310,1 Check for approval amount. [Begin]
      lnTotPkQty = (lnPik1 + lnPik2 + lnPik3 + lnPik4 + ;
                   lnPik5 + lnPik6 + lnPik7 + lnPik8)
      IF llChkAprov AND lnTotPkQty < 0
        lnChkAprov = lnChkAprov - lnTotPkQty * m.Price
      ENDIF
      *E301310,1 Check for approval amount. [End  ]

    *ELSE
      *-- If it has been picked before, restore the old picking
    *  lnPik1      = - MAX(m.Pik1 - ORDLINE.Pik1, 0)  
    *  lnPik2      = - MAX(m.Pik2 - ORDLINE.Pik2, 0)  
    *  lnPik3      = - MAX(m.Pik3 - ORDLINE.Pik3, 0)  
    *  lnPik4      = - MAX(m.Pik4 - ORDLINE.Pik4, 0)  
    *  lnPik5      = - MAX(m.Pik5 - ORDLINE.Pik5, 0)  
    *  lnPik6      = - MAX(m.Pik6 - ORDLINE.Pik6, 0)  
    *  lnPik7      = - MAX(m.Pik7 - ORDLINE.Pik7, 0)  
    *  lnPik8      = - MAX(m.Pik8 - ORDLINE.Pik8, 0)  
    *ENDIF  
    m.nForcdQty = 0
  ENDIF  
  m.Pik1   = m.Pik1 + lnPik1
  m.Pik2   = m.Pik2 + lnPik2
  m.Pik3   = m.Pik3 + lnPik3
  m.Pik4   = m.Pik4 + lnPik4
  m.Pik5   = m.Pik5 + lnPik5
  m.Pik6   = m.Pik6 + lnPik6
  m.Pik7   = m.Pik7 + lnPik7
  m.Pik8   = m.Pik8 + lnPik8
  m.TotPik = m.Pik1 + m.Pik2 + m.Pik3 + m.Pik4 + m.Pik5 + m.Pik6 + m.Pik7 + m.Pik8
  
  IF EOF(lc_TmpOrdL) 
    STORE .T. TO m.lVldWare, m.lvldDye
    *m.cStatus   = IIF(m.cStatus='M', m.cStatus, ' ')
    m.cStatus    = 'M'
    m.lAllocated = llGenPkTk
    INSERT INTO (lc_TmpOrdL)  FROM MEMVAR
  ELSE  
    SELECT (lc_TmpOrdL) 
    GATHER MEMVAR MEMO
  ENDIF  

  *-- Update temp stydye
  SELECT (lc_TmpStyD)
  IF SEEK(m.Style + m.cWareCode + SPACE(10), lc_TmpStyD) 
    REPLACE Alo1 WITH Alo1 + lnPik1,;
            Alo2 WITH Alo2 + lnPik2,;
            Alo3 WITH Alo3 + lnPik3,;
            Alo4 WITH Alo4 + lnPik4,;
            Alo5 WITH Alo5 + lnPik5,;
            Alo6 WITH Alo6 + lnPik6,;
            Alo7 WITH Alo7 + lnPik7,;
            Alo8 WITH Alo8 + lnPik8,;
            TotAlo WITH TotAlo + lnPik1 + lnPik2 + lnPik3  + lnPik4  + lnPik5;
          	  	     	       + lnPik6 + lnPik7 + lnPik8  
  ENDIF
  IF llUseDyes .AND. STYLE.cDye_flg = 'Y' .AND.;
    SEEK(m.Style + m.cWareCode + m.Dyelot, lc_TmpStyD) 
    REPLACE Alo1 WITH Alo1 + lnPik1,;
            Alo2 WITH Alo2 + lnPik2,;
            Alo3 WITH Alo3 + lnPik3,;
            Alo4 WITH Alo4 + lnPik4,;
            Alo5 WITH Alo5 + lnPik5,;
            Alo6 WITH Alo6 + lnPik6,;
            Alo7 WITH Alo7 + lnPik7,;
            Alo8 WITH Alo8 + lnPik8,;
            TotAlo WITH TotAlo + lnPik1 + lnPik2 + lnPik3  + lnPik4  + lnPik5;
          	  	     	       + lnPik6 + lnPik7 + lnPik8  
          	  	     	         
  ENDIF  
  IF !llZoomed
    SHOW GETS WINDOW (lcAlOrdAl3) ONLY 
    =lfRefresh()
  ENDIF  
  SHOW GETS WINDOW (lcAlOrdAl3) ONLY
  SHOW WINDOW (lcOrdBrTtl) REFRESH SAME
*-- else, just keep the selection  
ENDIF
*ELSE
*  IF EOF(lc_TmpOrdL) 
*    STORE .T. TO m.lVldWare, m.lvldDye
*    *m.cStatus  = IIF(m.cStatus='M', m.cStatus, ' ')
*    m.cStatus  = 'M'
*    INSERT INTO (lc_TmpOrdL)  FROM MEMVAR
*  ELSE  
*    SELECT (lc_TmpOrdL) 
*    GATHER MEMVAR MEMO
*  ENDIF  
*ENDIF
SELECT (lncurAlias)

*!*************************************************************
*! Name      : lpMoveOrd
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Switch ordered quantities between a style + warehouse
*!             and another style + warehouse
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: lcFrmSty  : From style
*!             lcToSty   : To Style
*!             lcFrmWare : From warehouse
*!             lcToWare  : To warehouse
*!*************************************************************
*! Returns   :  .T. if switching is successful
*!*************************************************************
*! Example   :  =lpMoveOrd(m.Style, m.cWareCode, ;
*!                         m.AltStyle, m.cWareCode)
*!*************************************************************
PROCEDURE lpMoveOrd
*B603240,1 Remove dyelots parameters since they do not update ordered
*B603240,1 quantities in STYDYE
*PARAMETERS lcFrmSty, lcToSty, lcFrmWare, lcToWare, lcFrmDye, lcToDye 
*B037363,1 HBG 25/12/2003 Add parammeter to fix bug of The main style did not come back when releasing the Piktkt [Begin]
*PARAMETERS lcFrmSty, lcToSty, lcFrmWare, lcToWare
PARAMETERS lcFrmSty, lcToSty, lcFrmWare, lcToWare , lcProc
*B037363,1 [End]
*B603240,1 end
PRIVATE lnCurAlias, llRetVal, llSavCont

lnCurAlias = SELECT(0)
llRetVal   =.F.

SELECT STYDYE

*B603240,1 Remove dyelots since they do not update ordered quantities in 
*B603240,1 STYDYE
*-- If using dyelots, attempt to lock  
*IF llUseDyes .AND. STYLE.cDye_flg = 'Y'
*  llSavCont = SEEK(lcToSty + lcToWare  + lcToDye) .AND. gfObj_Lock(.T.);
*             .AND. SEEK(lcFrmSty + lcFrmWare + lcFrmDye)  .AND. gfObj_Lock(.T.)
*ELSE
*  llSavCont = .T.  
*ENDIF  
*IF llSavCont 
*  IF SEEK(lcToSty + lcToWare  + SPACE(10)) .AND. gfObj_Lock(.T.);
*    .AND. ((lcToSty + lcToWare = lcFrmSty + lcFrmWare) .OR. (SEEK(lcFrmSty + lcFrmWare + SPACE(10))  .AND. gfObj_Lock(.T.)))
IF lcToWare <> lcFrmWare
  IF SEEK(lcToSty + lcToWare  + SPACE(10)) .AND. gfObj_Lock(.T.);
    .AND. SEEK(lcFrmSty + lcFrmWare + SPACE(10))  .AND. gfObj_Lock(.T.))
*B603240,1 end
    
    IF lcToSty <> lcFrmSty
      *-- Get the year/period of ORDHDR entered date so as to update
      *-- style history.
      IF CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod ','',.T.)
        =gfOpenFile(gcDataDir+"ICSTYHST", "STYHST", "SH")
        IF SEEK(lcToSty + lcGlYear) .AND. gfObj_Lock(.T.);
           .AND. SEEK(lcFrmSty + lcGlYear) .AND. gfObj_Lock(.T.)
  
            SELECT STYLE
            IF SEEK(lcToSty) .AND. gfObj_Lock(.T.);
              .AND. SEEK(lcFrmSty) .AND. gfObj_Lock(.T.)
  
              REPLACE Ord1   WITH MAX(Ord1 - m.Qty1, 0),;
                      Ord2   WITH MAX(Ord2 - m.Qty2, 0),;
                      Ord3   WITH MAX(Ord3 - m.Qty3, 0),;
                      Ord4   WITH MAX(Ord4 - m.Qty4, 0),;
                      Ord5   WITH MAX(Ord5 - m.Qty5, 0),;
                      Ord6   WITH MAX(Ord6 - m.Qty6, 0),;
                      Ord7   WITH MAX(Ord7 - m.Qty7, 0),;
                      Ord8   WITH MAX(Ord8 - m.Qty8, 0),;
                      TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8
              
              SELECT STYDYE
              REPLACE Ord1   WITH MAX(Ord1 - m.Qty1, 0),;
                      Ord2   WITH MAX(Ord2 - m.Qty2, 0),;
                      Ord3   WITH MAX(Ord3 - m.Qty3, 0),;
                      Ord4   WITH MAX(Ord4 - m.Qty4, 0),;
                      Ord5   WITH MAX(Ord5 - m.Qty5, 0),;
                      Ord6   WITH MAX(Ord6 - m.Qty6, 0),;
                      Ord7   WITH MAX(Ord7 - m.Qty7, 0),;
                      Ord8   WITH MAX(Ord8 - m.Qty8, 0),;
                      TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8
                      
            SELECT ICSTYHST
            REPLACE nOrdQty&lcGlPeriod WITH MAX(nOrdQty&lcGlPeriod - m.TotQty, 0),;
                    nOrdQty            WITH MAX(nOrdQty            - m.TotQty, 0),;
                    nOrdAmt&lcGlPeriod WITH MAX(nOrdAmt&lcGlPeriod - m.TotQty * ORDLINE.Price, 0) ,;
                    nOrdAmt            WITH nOrdAmt                - MAX(nOrdAmt&lcGlPeriod - m.TotQty * ORDLINE.Price, 0)

            SELECT STYLE
            IF SEEK(lcToSty)
              REPLACE Ord1   WITH Ord1 + m.Qty1,;
                      Ord2   WITH Ord2 + m.Qty2,;
                      Ord3   WITH Ord3 + m.Qty3,;
                      Ord4   WITH Ord4 + m.Qty4,;
                      Ord5   WITH Ord5 + m.Qty5,;
                      Ord6   WITH Ord6 + m.Qty6,;
                      Ord7   WITH Ord7 + m.Qty7,;
                      Ord8   WITH Ord8 + m.Qty8,;
                      TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8
    
            ENDIF
            SELECT STYDYE
            IF SEEK(lcToSty + lcToWare  + SPACE(10)) 
              REPLACE Ord1   WITH Ord1 + m.Qty1,;
                      Ord2   WITH Ord2 + m.Qty2,;
                      Ord3   WITH Ord3 + m.Qty3,;
                      Ord4   WITH Ord4 + m.Qty4,;
                      Ord5   WITH Ord5 + m.Qty5,;
                      Ord6   WITH Ord6 + m.Qty6,;
                      Ord7   WITH Ord7 + m.Qty7,;
                      Ord8   WITH Ord8 + m.Qty8,;
                      TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8
       
            ENDIF
            SELECT ICSTYHST
            IF SEEK(lcToSty + lcGlYear)
              REPLACE nOrdQty&lcGlPeriod WITH MAX(nOrdQty&lcGlPeriod + m.TotQty, 0),;
                      nOrdQty            WITH MAX(nOrdQty            + m.TotQty, 0),;
                      nOrdAmt&lcGlPeriod WITH MAX(nOrdAmt&lcGlPeriod + m.TotQty * ORDLINE.Price, 0) ,;
                      nOrdAmt            WITH nOrdAmt                + MAX(nOrdAmt&lcGlPeriod - m.TotQty * ORDLINE.Price, 0)
            ENDIF
  
            llRetVal   =.T.
          ENDIF &&ENDIF SEEK(..,'STYLE')
          SELECT STYLE
          =SEEK(lcFrmSty) .AND. gfObj_Lock(.F.) 
          =SEEK(lcToSty) .AND. gfObj_Lock(.F.)
        ENDIF  &&ENDIF SEEK(..,'ICSTYHST')
        SELECT ICSTYHST    
        =SEEK(lcFrmSty + lcGlYear) .AND. gfObj_Lock(.F.);
        =SEEK(lcToSty) .AND. gfObj_Lock(.F.)
        *B037363,1 HBG 25/12/2003 Restore the old style when releasing Piktkt[Begin]
        *-- If Release PIktkt
        IF lcProc = 'R'
          m.AltStyle = ""
          m.Style    = lcToSty
        ENDIF  
        *B037363,1 [End]

      ENDIF  &&ENDIF CHECKPRD
    ELSE
      SELECT STYDYE
      REPLACE Ord1   WITH MAX(Ord1 - m.Qty1, 0),;
              Ord2   WITH MAX(Ord2 - m.Qty2, 0),;
              Ord3   WITH MAX(Ord3 - m.Qty3, 0),;
              Ord4   WITH MAX(Ord4 - m.Qty4, 0),;
              Ord5   WITH MAX(Ord5 - m.Qty5, 0),;
              Ord6   WITH MAX(Ord6 - m.Qty6, 0),;
              Ord7   WITH MAX(Ord7 - m.Qty7, 0),;
              Ord8   WITH MAX(Ord8 - m.Qty8, 0),;
              TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8
      
       IF SEEK(lcToSty + lcToWare  + SPACE(10)) 
          REPLACE Ord1   WITH Ord1 + m.Qty1,;
                  Ord2   WITH Ord2 + m.Qty2,;
                  Ord3   WITH Ord3 + m.Qty3,;
                  Ord4   WITH Ord4 + m.Qty4,;
                  Ord5   WITH Ord5 + m.Qty5,;
                  Ord6   WITH Ord6 + m.Qty6,;
                  Ord7   WITH Ord7 + m.Qty7,;
                  Ord8   WITH Ord8 + m.Qty8,;
                  TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8
       
      ENDIF
    ENDIF
    SELECT STYDYE
  ENDIF
  *-- Unlock all records
  =SEEK(lcFrmSty + lcFrmWare + SPACE(10))  .AND. gfObj_Lock(.F.)
  =SEEK(lcToSty + lcToWare  + SPACE(10)) .AND. gfObj_Lock(.F.)
ENDIF  

SELECT (lnCurAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lpSwchSty    
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Switch ordered quantities between a style + warehouse
*!             and another style + warehouse
*!*************************************************************
*! Calls     : lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: lcFrmSty  : From style
*!             lcToSty   : To Style
*!             lcFrmWare : From warehouse
*!             lcToWare  : To warehouse
*!*************************************************************
*! Returns   :  .T. if switching is successful
*!*************************************************************
*! Example   :  =lpSwchSty(m.Style, m.cWareCode, ;
*!                         m.AltStyle, m.cWareCode)
*!*************************************************************
PROCEDURE lpSwchSty    
PARAMETERS lcFrmStyle, lcToStyle, lcFrmWare, lcToWare 
PRIVATE lcSize, lnSize, lnFrmCnt, lnToCnt, lnCurAlias, lnCanc, lnCancAmt

lnCurAlias = SELECT(0)

*-- Get the scale of the target style
IF SEEK(lcToStyle, 'STYLE') .AND. SEEK('S'+STYLE.Scale, 'SCALE')
  lnToCnt   = SCALE.Cnt
  m.Scale   = SCALE.Scale
  =SEEK(lcFrmStyle, 'STYLE')   
  lnFrmCnt   = SCALE.Cnt
  *-- If the scale is greater, cancel any extra ordered amounts
  STORE 0 TO lnCanc, lnCancAmt
  IF lnFrmCnt > lnToCnt
  
    *-- Cancel any extra ordered amounts in ORDLINE
    SELECT ORDLINE
    FOR lnSize = lnToCnt + 1 TO lnFrmCnt
      lcSize    = STR(lnSize, 1)
      lnQty     = ORDLINE.Qty&lcSize
      lnCanc    = lnCanc    + lnQty
      lnCancAmt = lnCancAmt + lnQty * ORDLINE.Price
      REPLACE     Qty&lcSize  WITH 0,;
			      TotQty      WITH TotQty      - lnQty,;
				  Book&lcSize WITH Book&lcSize - lnQty,;
			      TotBook     WITH TotBook     - lnQty
      
      *-- Cancel any extra ordered amounts in STYDYE
      SELECT STYDYE
      REPLACE Ord&lcSize WITH MAX(Ord&lcSize - lnQty, 0),;
              TotOrd     WITH TotOrd - Ord&lcSize 

      *-- Cancel any extra ordered amounts in STYLE
      SELECT STYLE
      REPLACE Ord&lcSize WITH MAX(Ord&lcSize - lnQty, 0),;
              TotOrd     WITH TotOrd - Ord&lcSize 
    ENDFOR
  ENDIF  
  *B037363,1 HBG 25/12/2003 Add parammeter to fix bug of The main style did not come back when releasing the Piktkt [Begin]
  *=lpMoveOrd(lcFrmStyle, lcToStyle, lcFrmWare, lcToWare)
  =lpMoveOrd(lcFrmStyle, lcToStyle, lcFrmWare, lcToWare,'R')
  *B037363,1 [End]

  *-- Update the ordered quantities in ARCUSHST (customer history)
  *-- style and its ordered quantities
  IF lnCanc <> 0
    *-- Cancel any extra ordered amounts in ORDHDR
    SELECT ORDHDR
    REPLACE Open      WITH Open    - lnCanc ,;
            OpenAmt   WITH OpenAmt - lnCancAmt ,;
            Book      WITH Book    - lnCanc,;
            BookAmt   WITH BookAmt - lnCancAmt

    SELECT ARCUSHST
    IF SEEK(m.Account + lcGlYear)
      REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod ;
                        			  -ORDLINE.TotQty + m.TotQty,;
              nOrdQty            WITH nOrdQty            ;
						 			  -ORDLINE.TotQty + m.TotQty,;
              nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod -;
				                      (ORDLINE.TotQty - m.TotQty) * ORDLINE.Price,;
              nOrdAmt            WITH nOrdAmt            -;
								      (ORDLINE.TotQty - m.TotQty) * ORDLINE.Price
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfVldStyle
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfVldStyle
PARAMETERS lcStyle

*E30051,1 If browsig for a style
IF llBrowse .OR. '?' $ lcStyle
  lcStyle    = gfStyBrw("I" , "" , "" , .F.)
  lcStyle    = IIF(EMPTY(lcStyle), lcOldVal, lcStyle)
  llBrowse   = .F.
ELSE
  *E3005111,1 IF contents of the field has changed,
  IF lcStyle <> lcOldVal 
    *E3005111,1 If the field is emptied, restore the old value
    IF !lfStyEmpty(lcStyle)
      IF !SEEK(lcStyle, 'STYLE') 
        lcStyle    = gfStyBrw("I" , "" , "" , .F.)
        lcStyle    = IIF(EMPTY(lcStyle), lcOldVal, lcStyle)
      ENDIF
	ENDIF
  ENDIF  
ENDIF
RETURN !(EMPTY(lcStyle) .OR. lfStyEmpty(lcStyle))

*!*************************************************************
*! Name      : lfStyEmpty
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .t. if the style is empty
*!*************************************************************
*! Example   :  IF lfStyEmpty(lcStyle)  
*!*************************************************************
FUNCTION lfStyEmpty
PARAMETERS lcStyle
PRIVATE lcStySep, lnCount
lcStySep = STRTRAN(lcStyMsk, 'X')
FOR lnCount = 1 TO LEN(lcStySep)
  lcStyle = STRTRAN(lcStyle, SUBSTR(lcStySep, lnCount, 1))
ENDFOR
RETURN EMPTY(lcStyle)

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
PRIVATE lcOldScale, lnOldScCnt

*-- Store original scale information
lcOldScale = SCALE.Scale
lnOldScCnt = SCALE.Cnt
IF lfVldStyle(@m.Style)
  *-- Now, if the style is changed, check if it may be used as a 
  *-- substitute or not
  IF m.Style <> lcOldVal .AND. SEEK(m.Style, 'STYLE')
    
    *-- Check if the selected style belongs to the same division
    *** Message : "ð / ð confilct!                               "
    ***           "                        < Ok >                    "
    *-- If the order is not multi season, check if the selected 
    *-- style has to the same season
    *** Message : "Style / ð confilct!                               "
    ***           "                        < Ok >                    "
    *-- Check if the selected style scale has les number of sizes
    *** Message : "The number of sizes for ð/Scale ð is less than the"
    ***           "number of sizes for ð/Scale ð . Cannot proceed    "
    ***           "                        < Ok >                    "

    IF STYLE.cDivision <> ORDHDR.cDivision .AND. ;
      gfModalGen('TRM42086B00000','DIALOG' , lcStyHdr + '|Division')=1;
      .OR.;
      ORDHDR.Season <> '*' .AND. STYLE.Season <> ORDHDR.Season .AND.;
      gfModalGen('TRM42086B00000','DIALOG' , lcStyHdr + '|Season')=1  ;
      .OR.;
      SCALE.Cnt < lnOldScCnt .AND. lfSclNotVld(SCALE.Cnt+1, lnOldScCnt ) 
      
      m.Style = lcOldVal          && Restore the old value
      *-- Reposition the 'STYLE' file pointer
      =SEEK(m.Style, 'STYLE')
      _CUROBJ = _CUROBJ
    ELSE   
      *-- Check if the new style is assigned to the warehouse
	  IF SEEK(m.Style + m.cWareCode + SPACE(10) , 'STYDYE') ;
	    .OR.  gfModalGen('TRM42001B44004','DIALOG' , lcStyHdr + ' ' + ;
 	          ALLTRIM(m.Style) + '|' + ALLTRIM(m.cWareCode)) = 1      ;
        .AND. gpAdStyWar(m.Style , SPACE(10) , m.cWareCode)
        *-- Update  
        =lfSubstitute()    
      ELSE
        m.Style = lcOldVal  
        *-- Reposition the 'STYLE' file pointer
        =SEEK(m.Style, 'STYLE')
        _CUROBJ = _CUROBJ
      ENDIF
    ENDIF
  ENDIF
ELSE
  m.Style   = lcOldVal
  _CUROBJ   = _CUROBJ 
ENDIF
   
*!*************************************************************
*! Name      : lfvOrdNote
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Order notepad
*!*************************************************************
*! Calls     : NOTEPAD
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvOrdNote()
*!*************************************************************
FUNCTION lfvOrdNote
=NOTEPAD('B', laData[1])

*!*************************************************************
*! Name      : lfvObjLink
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Call the object link screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvObjLink()
*!*************************************************************
FUNCTION lfvObjLink
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
DO GetObj WITH 'O',laData[1]
SELECT (lnCurAlias)


*!*************************************************************
*! Name      : lfvRel
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Close program or cancel order modification
*!*************************************************************
*! Calls     : gfDialog,lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfvRel
PARAMETERS llRelAll 
PRIVATE llReleased
lnCurAlias = SELECT(0)

llReleased = .F.

*-- If this line has been picked during this session
IF EMPTY(ORDLINE.PikTkt)
  IF !EOF(lc_TmpOrdL)
    SELECT (lc_TmpOrdL)
    SCATTER MEMVAR MEMO
    *-- Release allocated stock and restore original ORDLine
    llReleased = lfRelease(.T.)
  ENDIF  
*-- If this line has an existing picking ticket
ELSE
  IF EVAL(lc_TmpOrdl+'.cStatus') <> 'R' .AND. lfCanRelease(ORDLINE.PikTkt)
    SELECT ORDLINE
    SCATTER MEMVAR MEMO
    llReleased = lfRelease()
  ELSE
    llReleased = .F.
  ENDIF
ENDIF
SELECT (lnCurAlias)

*-- If released, refresh display
IF !llRelAll .AND. llReleased
  =gfUpdate() 
  =lfShowLines() 
  SHOW WINDOW (lcOrdBrTtl) REFRESH SAME  
ENDIF

*!*************************************************************
*! Name      : lfRelease
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Close program or cancel order modification
*!*************************************************************
*! Calls     : gfDialog,lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION  lfRelease
PARAMETERS llRelTmpPik

*-- Deallocate stydye quantities
=lfAdjStyDye('S')

*-- If this line is picked in this session,
STORE .F. TO m.lVldWare, m.lVldDye, m.lForceAlc
IF llRelTmpPik
  *-- Delete this line from the temp file
  *-- Release allocated stock and restore original ORDLine
  SELECT (lc_TmpPkLn)
  DELETE

  SELECT (lc_TmpOrdL)
  *E301310,1 Return pik lines to apporove variable [Begin]
  IF llChkAprov AND TotPik > 0
    lnChkAprov = lnChkAprov + (TotPik*Price)
  ENDIF
  *E301310,1 Return pik lines to apporove variable [End  ]
  DELETE

  *-- Restore the original line
  SELECT ORDLINE
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GO RECNO()
  ENDIF  
  SCATTER MEMVAR MEMO
  IF m.cWareCode <> lcWareCode
    =lfAddLine(lcWareCode, m.Dyelot)
  ENDIF  
  
*-- else, if this line is an already picked line
ELSE
  
  
  *E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt. [Begin]
  IF 'AS' $ gcCmpModules AND llSndPkFld
    IF PIKTKT.csndpkt = 'Y'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' can not release ' )
      RETURN
    ENDIF
  ENDIF
  *E130074,1 EIH 01/02/2006 [End]
  
  
  *-- Add a record in (lc_TmpPkLn)
  INSERT INTO (lc_TmpPkLn) FROM MEMVAR
  *E301310,1 Return pik lines to apporove variable [Begin]
  lnTmpRele = IIF(EOF(lc_TmpOrdL),&lc_TmpPkLn..TotPik*&lc_TmpPkLn..Price,;
                                  &lc_TmpOrdL..TotPik*&lc_TmpOrdL..Price)
  IF llChkAprov AND lnTmpRele > 0
    lnChkAprov = lnChkAprov + lnTmpRele
  ENDIF
  *E301310,1 Return pik lines to apporove variable [End]

  m.cStatus   = 'R'

  m.PikTkt    = SPACE(6)
  m.Picked    = .F.
  m.PikDate   = {}
  *-- DeAllocate picked quantities 
  STORE 0 TO m.Pik1, m.Pik2, m.Pik3, m.Pik4, m.Pik5,;
             m.Pik6, m.Pik7, m.Pik8, m.TotPik
  *-- Restore substitution 
  IF !EMPTY(m.AltStyle)
    m.Style      = m.AltStyle
    m.AltStyle   = SPACE(19)
    m.Scale      = IIF(SEEK(m.Style, 'STYLE'), SCALE.Scale, ' ')
    lcAltStyDsc  = ''
  ENDIF  
  m.cWareCode = ORDHDR.cWareCode
  *B603240,1 Do not clear the dyelot field. Comment the line.
  *m.Dyelot    = SPACE(10)
  *B603240,1 end
  m.nForcdQty = 0
  m.lForceAlc = .F.
  IF EOF(lc_TmpOrdL)
    INSERT INTO (lc_TmpOrdL) FROM MEMVAR
  ELSE
    SELECT (lc_TmpOrdL)
    GATHER MEMVAR MEMO
  ENDIF  
ENDIF




*!*************************************************************
*! Name      : lfvRelAll
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Close program or cancel order modification
*!*************************************************************
*! Calls     : gfDialog,lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfvRelAll
PRIVATE lcScanExp, llReleased

lnRecNo    = RECNO('ORDLINE')
IF BETWEEN(RECNO('ORDHDR'),1,RECCOUNT())
  GO RECNO('ORDHDR') IN ORDHDR
ENDIF  

SELECT ORDLINE

llReleased = .F.
IF SEEK(laData[9] + laData[1])
*  lcScanExp = IIF(!EMPTY(lcBrowExp),;
       		      [FOR ] + lcNdxExp + [ = laData(9) + laData(1) + ] +;
       		      IIF(EMPTY(lcStore), [STORE ], [lcStore]) + ;
       		      IIF(EMPTY(lcStyle), [], [+ lcStyle]);
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ AND Group = lcGroup],[]),;
       		      [REST WHILE cOrdType + Order = laData(9) + laData(1)];
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ FOR Group = lcGroup],[]))

*   lcScanExp = IIF(!EMPTY(lcBrowExp),;
       		      [FOR ] + lcNdxExp + [ = laData(9) + laData(1) + ] +;
       		      [ALLTRIM(lcStore + lcStyle)];
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ AND Group = lcGroup],[]),;
       		      [REST WHILE cOrdType + Order = laData(9) + laData(1)];
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ FOR Group = lcGroup],[]))
 lcScanExp = IIF(!EMPTY(lcBrowExp),;
       		      [FOR ] + lcNdxExp + [ = laData(9) + laData(1) + ] + lcBrowExp ,;
       		      [REST WHILE cOrdType + Order = laData(9) + laData(1)];
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ FOR Group = lcGroup],[]))

 
  SCAN &lcScanExp
    =lfvRel(.T.)
  ENDSCAN
  IF BETWEEN(lnRecNo, 1, RECCOUNT())
    GO lnRecNo
  ELSE
    GO BOTTOM
  ENDIF  
  *-- If released, refresh display
  IF EOF(lc_TmpOrdL) .OR. DELETED(lc_TmpOrdL) 
    SCATTER MEMVAR MEMO
    =lfShowLines()
  ELSE  
    *IF &lc_TmpOrdL..cStatus = 'R'
      SELECT (lc_TmpOrdL)
      SCATTER MEMVAR MEMO
      =lfShowLines()
    *ENDIF  
  ENDIF
  SHOW WINDOW (lcOrdBrTtl) REFRESH SAME 
ENDIF  

*!*************************************************************
*! Name      : lfvWareCode
*! Developer : RENEE - Renee Ezzat
*! Date      : 07/06/1995
*! Purpose   : Valid function for get field 'cWareCode' (Warehouse)
*!             Validates an entered warehouse or browses for one.
*!             Checks if Fabric + Color is assigned to the warhouse
*!             If not, adds a record in FABDYE
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvWareHouse()
*!*************************************************************
FUNCTION lfvWareCode
PRIVATE lnCurTag
IF puWareHous <>lcOldVal
  *-- If a picking ticket exists for the current line, 
  *-- use its pick from warehouse, 
  *-- Else, use the default warehouse for picking.  
  
  *-- Use the selected warehouse for any line that is not picked
  lcWareCode = laWareHous[puWareHous, 2] 
  lnCurAlias = SELECT(0)
  SELECT ORDLINE
  lnCurTag = VAL(SYS(21))
  SET ORDER TO TAG ORDLINST
  lnRecNo = RECNO()
  IF SEEK(laData[9] + laData[1])
    *lcScanExp = IIF(!EMPTY(lcBrowExp),;
       		      [WHILE ] + lcNdxExp + [ = laData(9) + laData(1) + ] +;
       		      IIF(EMPTY(lcStore), [STORE ], [lcStore]) + ;
       		      IIF(EMPTY(lcStyle), [], [+ lcStyle]);
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ AND Group = lcGroup],[]),;
       		      [REST WHILE cOrdType + Order = laData(9) + laData(1)];
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ WHILE Group = lcGroup],[]))

   *lcScanExp = IIF(!EMPTY(lcBrowExp),;
       		      [WHILE ] + lcNdxExp + [ = laData(9) + laData(1) + ] +;
       		      lcBrowExp + ;
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ AND Group = lcGroup],[]),;
       		      [REST WHILE cOrdType + Order = laData(9) + laData(1)];
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ WHILE Group = lcGroup],[]))
   *lcScanExp = IIF(!EMPTY(lcBrowExp),;
       		      [FOR ] + lcNdxExp + [ = laData(9) + laData(1) + ] + lcBrowExp ,;
       		      [REST WHILE cOrdType + Order = laData(9) + laData(1)];
       		      + IIF(!EMPTY(ALLTRIM(lcGroup)), [ FOR Group = lcGroup],[]))
   *lcScanExp = IIF(!EMPTY(lcBrowExp), 
   lcScanExp = IIF(!EMPTY(lcBrowExp),; 
                  [FOR ] + lcNdxExp + [ = laData(9) + laData(1) ] + lcBrowExp +; 
  				  IIF(EMPTY(lcGroup), [], [.AND. Group = lcGroup]) + [.AND. !lfLinIsPck()],;
  				  [REST WHILE cOrdType + Order = laData(9) + laData(1)];
       		      + IIF(!EMPTY(lcGroup),;
       		      [ FOR Group = lcGroup .AND. !lfLinIsPck()], [ FOR !lfLinIsPck()]))
    *SCAN REST &lcScanExp ;
    *         FOR !lfLinIsPck()
    SCAN &lcScanExp 
      =lfAddLine(lcWareCode)
    ENDSCAN
  ENDIF  
  SET ORDER TO (lnCurTag)
  IF BETWEEN(lnRecNo, 1, RECCOUNT())
    GO lnRecNo
  ELSE
    GO TOP
  ENDIF
  IF !EOF(lc_TmpOrdL)
    SELECT (lc_TmpOrdL)
  ENDIF  
  SCATTER MEMVAR MEMO  
  IF !lfLinIsPck()
    puPickWare = puWareHous
    SHOW GET puPickWare
  ENDIF
  SELECT (lnCurAlias)
  SHOW WINDOW (lcOrdBrTtl) REFRESH SAME
  =lfRefresh(lcAlOrdAl3)
ENDIF

*!*************************************************************
*! Name      : lfvDyelots
*! Developer : RENEE - Renee Ezzat
*! Date      : 07/10/1995
*! Purpose   : Valid function for m.dyelot field.
*!*************************************************************
*! Calls     : gfvDyelots()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDyelots()
*!**!*************************************************************
FUNCTION lfvDyelots
*B603240,1 Add a parameter for a return value
PARAMETERS llRetVal
*B603240,1 end
PRIVATE lcDyelot, lnCurAlias

IF !llBrowse 
  IF !EMPTY(m.Dyelot)
    IF !SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE') 
      llBrowse = .T. 
    ELSE
      m.lVldDye = .T.
    ENDIF
  ENDIF
ENDIF
IF llBrowse      
  lcDyelot    = m.Dyelot 
  lnCurAlias  = SELECT(0)
  *B603240,1 Pass a parameter to SDYEBROW to view the browse in the upeer half
  *B603240,1 of the screen, and to show available quantity
  *IF SDYEBROW(m.Style, @lcDyelot, .T., m.cWareCode)
  *B802829,1 WAB - call local function to browse dyelot
  *B802829,1 WAB - START
  *IF SDYEBROW(m.Style, @lcDyelot, .T., m.cWareCode, .T., .T.)
  
  *B802829,1 KHM 03/06/2000 (Begin) Using the global function StyBrow instead
  *B802829,1                of local function lfDyeBrow.
  *IF lfDyeBrow(m.Style, @lcDyelot, m.cWareCode)
  IF SDYEBROW(m.Style, @lcDyelot, .T., m.cWareCode, .T., .T.,lcStyDTemp,lcStyDTemp)
  *B802829,1 KHM 03/06/2000 (End)
  
  *B802829,1 WAB - END
  *B603240,1 end
    m.Dyelot  = lcDyelot
    m.lVldDye = .T.
  ELSE
    m.Dyelot  = lcOldVal
    *B603240,1 Do not trap the user on the dyelot field
    *_CUROBJ   = _CUROBJ
    *B603240,1 end
  ENDIF
  SELECT(lnCurAlias)
  llBrowse    = .F.
ENDIF
IF m.lVldDye
  =lfAddLine(.F., m.Dyelot)  
  *B603240,1 Remove
  * SHOW WINDOW (lcOrdBrTtl) REFRESH SAME
  =lfRefresh(lcAlOrdAl3)
ENDIF
*B603240,1 Return if a valid dyelot is selected
SHOW WINDOW (lcOrdBrTtl) REFRESH 
llRetVal = m.lVldDye
*B603240,1 end

*!*************************************************************
*! Name      : lfVldPick
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Close program or cancel order modification
*!*************************************************************
*! Calls     : gfDialog,lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfVldPick
PARAMETERS llPickAll
PRIVATE llRetVal

IF llPickAll
  =lfAddLine()
  llRetVal = m.lVldWare .AND. m.lVldDye
  *B605084,1 HBG 01/11/2001 In case of Pick all , if there is style not assigned ;
  *B605084,1                to the location which pick from. this line will be ignored [Begin]
  IF !SEEK(OrdLine.Style + IIF(llMultWare,laWareHous[puPickWare, 2],OrdLine.cWareCode) + SPACE(10), 'STYDYE') AND llFLine
    =gfModalGen('TRM44111B00000', 'DIALOG',IIF(llMultWare,ALLTRIM(laWareHous[puPickWare, 2]),ALLTRIM(OrdLine.cWareCode)))
    llFLine = .F.
  ENDIF
  *B605084,1 [End]
ELSE  

  *-- Before picking a line, check it the current style\color 
  *-- is assigned to the current warehouse
  llRetVal = .T.
  
  *-- If the warehouse is valid, add a line
  IF m.lVldWare
    =lfAddLine()
  *-- Otherwise, validate the warehouse
  ELSE
    llRetVal = lfVldWare(m.cWareCode)
  ENDIF
  SELECT ORDLINE 
  IF llRetVal
    *-- Check if dyelot is not right
    IF !m.lVldDye
	  *B603240,1 Check if any dyelots exist for this style/color
	  *B603240,1 If none exist, the browse becomes the current object
	  IF !lfDyeExist()
        llRetVal = .F.
	    KEYBOARD "{ALT+B}" 
	  ELSE
        *B603240,1 If the dyelot is empty, give the user a message 
        *B603240,1 that he has to select a dyelot
        IF EMPTY(m.Dyelot)
        *B603240,1 end
          *=gfModalGen('TRM44019B00000', 'DIALOG')
          *SHOW WINDOW (lcOrdBrTtl) REFRESH SAME  
          *_CUROBJ = OBJNUM(m.Dyelot)
          *llRetVal = .F.
          *B603240,1 Perform dyelots validations here
          llBrowse = .T.
          lcOldVal = m.Dyelot
          =lfvDyelots(@llRetVal)
        *B603240,1 Add an ENDIF
        ELSE
          *B603240,1 Perform dyelots validations here
          llBrowse = .T.
          lcOldVal = m.Dyelot
          =lfvDyelots(@llRetVal)
        ENDIF  
      ENDIF  
      *B603240,1 end
    ENDIF
  ELSE
    _CUROBJ = OBJNUM(puPickWare)  
  ENDIF
ENDIF  

RETURN llRetVal  

*B603240,1 Check for dyelots existance
FUNCTION lfDyeExist
PRIVATE lnCurAlias, lnCurRec, llFound
lnCurAlias = SELECT(0)
SELECT STYDYE
lnCurRec = RECNO()
IF SEEK(m.Style + m.cWareCode)
  LOCATE REST WHILE Style + cWareCode = m.Style + m.cWareCode;
  		 FOR !EMPTY(Dyelot)
  llFound = FOUND() 		 
ELSE
  llFound = .F.
ENDIF      
IF !llFound
  =gfModalGen("INM00277B00000" , "DIALOG" , ALLTRIM(m.Style) + ;
               IIF(llMultWare, " in location " + ALLTRIM(m.cWareCode),""))
ENDIF

IF BETWEEN(lnCurRec, 1, RECCOUNT())
  GO lnCurRec
ENDIF
SELECT (lnCurAlias)
RETURN llFound
  

*!*************************************************************
*! Name      : lfCanRelease
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Close program or cancel order modification
*!*************************************************************
*! Calls     : gfDialog,lfvSelect,lfGetOrdInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvClose()
*!*************************************************************
FUNCTION lfCanRelease
PARAMETERS lcCurPikTkt
=gfOpenFile(gcDataDir+"PACK_HDR", "PACK_HDR", "SH")
*-- Message : 44003  
*-- Picking ticket# ð  is ð.  Do you wish to release 
*-- this picking ticket anyway?
*-- If the picking ticket is printed, give a message to the user.
*-- If the picking ticket has a packing list, do not release it.
RETURN EMPTY(m.PikTkt) .OR. m.PikTkt='******' .OR. (PIKTKT.PrtFlag<>'P' .OR. gfModalGen('TRM44003B00006','DIALOG', lcCurPikTkt+'|'+'printed') = 1);
        .AND.;
	   (PIKTKT.Status ='O' .OR. gfModalGen('TRM44003B00006','DIALOG', ;
	     lcCurPikTkt+'|'+ IIF(PIKTKT.Status $ 'HCX', laStatus[AT(PIKTKT.Status, 'OHCX')], 'on hold')) = 1);
        .AND.;
  	   (!SEEK(lcCurPikTkt, 'PACK_HDR');
	    .OR.;
  	   gfModalGen('TRM44001B00000', 'DIALOG',ALLTRIM(lcCurPikTkt)) <> 1)

*!*************************************************************
*! Name      : lfChkEntry
*! Developer : RENEE - Renee Ezzat
*! Date      : 07/13/1995
*! Purpose   : Checks if the current record entry is valid so 
*!             that we may proceed to another function or not.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  .T. if the entry is valid,
*!                       .F. otherwise.
*!*************************************************************
*! Example            :  IF lfChkEntry
*!*************************************************************
FUNCTION lfChkEntry
PRIVATE llRetVal, lnCurrRec

llRetVal = .T.
*-- Check if the total picked quantity is zero.
IF laScrMode[3] .AND. m.Picked .AND. m.TotPik <= 0  
  lnCurAlias  = SELECT(0)
  SELECT ORDLINE
  lnCurrRec   = RECNO('ORDLINE')
  *B605084,1 HBG 01/11/2001 Comment this line to get the correct line[Begin]
  *GO lnMarker IN ORDLINE
  *B605084,1 [End]

  *-- If this line has been picked, but its quantities are zeros,
  *-- Message : 44006 
  *-- The allocated quantity for this line is zero. Do you wish 
  *-- to ð this picking?
  *-- Button  : 
  
  *B804197,1 HBG 19/09/2001 If the setup of Force Allocation = 'No' there is no need to get 
  *B804197,1                the message of 'The allocated quantity for this line is zero. 
  *B804197,1                Do you want ignore this picking?'. instead we will show the message of
  *B804197,1                "There is no stock avaliable for the required sizes. Cannot Pick."
  *B804197,1                This is in case of Pick or Allocate All.

  *lnOption = gfModalGen('QRM44006B00006','DIALOG',;
              IIF(EMPTY(ORDLINE.PIKTKT), 'ignore', 'release'))
              
  IF !llForceBar
    *-- If not Pick All
    IF !llPAll       
      lnOption = gfModalGen('QRM44109B00000','DIALOG',;
                  "the selected line"+"|"+IIF(llGenPkTk,'Pick this line.','Allocate this line.'))
    ELSE
      *B605084,1 HBG 01/11/2001 If pick all display the message as case of Pick but only one time[Begin]
      *lnOption = 1
      IF llFirst
        lnOption = gfModalGen('QRM44109B00000','DIALOG',;
                      'some lines'+"|"+IIF(llGenPkTk,'Pick these lines.','Allocate these lines.'))
        llFirst = .F.              
      ELSE
        lnOption = 1
      ENDIF                
      *B605084,1 [End]                  
    ENDIF              
  ELSE
    *B605084,1 HBG 01/11/2001 If pick all and this is the first time display the 
    *B605084,1                MSG to ask if ignore the line or not[Begin]
    IF llPAll AND llFirst
    *B605084,1 [End]

      lnOption = gfModalGen('QRM44110B00006','DIALOG',;
                  IIF(EMPTY(ORDLINE.PIKTKT), 'ignore', 'release'))
    *B605084,1 HBG 01/11/2001 Reset the flag to not displaying the MSG again inf case of Pick All,
    *B605084,1                and if pick line by line display mesg for each one[Begin]
      llFirst = .F.
      lnMsg = lnOption 
    ELSE
      IF !llPAll
        lnOption = gfModalGen('QRM44006B00006','DIALOG',;
                    IIF(EMPTY(ORDLINE.PIKTKT), 'ignore', 'release'))     
      ELSE
        lnOption = lnMsg 
      ENDIF              
    ENDIF
    *B605084,1 [End]             
  ENDIF            
  *B804197,1 [End]
  IF lnOption = 1
    =lfvRel()
    GO lnCurrRec 
    llRetVal = .T.
  ELSE     
    llRetVal =  .F.
  ENDIF
  SELECT (lnCurAlias)
ENDIF
RETURN llRetVal

*!*************************************************************
*! Name      : lfvPickWare
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Valid function for puPickWare popup 
*!*************************************************************
*! Called From : ALORDAL.SPR
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPickWare()
*!*************************************************************
FUNCTION lfvPickWare
*-- Check if the current value has changed, and,
IF puPickWare <>lcOldVal 
  *-- Check if the current allocation may be changed
  IF m.Picked
    IF lfCanRelease(m.PikTkt) .AND. lfVldWare(laWareHous[puPickWare, 2])
      =lfExchange(laWareHous[puPickWare, 2])
      SHOW WINDOW (lcOrdBrTtl) REFRESH SAME
      =lfRefresh(lcAlOrdAl3)
    ELSE  
      puPickware = lcOldVal
      SHOW GET puPickware
    ENDIF          
  ELSE
    IF lfVldWare(laWareHous[puPickWare, 2])
      SHOW WINDOW (lcOrdBrTtl) REFRESH SAME
      =lfRefresh(lcAlOrdAl3)
    ELSE
      puPickware = lcOldVal
      SHOW GET puPickware
    ENDIF  
    REPLACE &lc_TmpOrdL..cStatus WITH 'U'
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfExchange
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Valid function for puPickWare popup 
*!*************************************************************
*! Called From : ALORDAL.SPR
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPickWare()
*!*************************************************************
FUNCTION lfExchange
PARAMETERS lcNewWare
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
*-- check if this line exists in the temporary ordline file.

*-- Release the current line allocated stock
IF !EOF(lc_TmpStyD)
  SELECT (lc_TmpStyD)
  REPLACE Alo1   WITH MAX(Alo1 - m.Pik1, 0),;
          Alo2   WITH MAX(Alo2 - m.Pik2, 0),;
          Alo3   WITH MAX(Alo3 - m.Pik3, 0),;
          Alo4   WITH MAX(Alo4 - m.Pik4, 0),;
          Alo5   WITH MAX(Alo5 - m.Pik5, 0),;
          Alo6   WITH MAX(Alo6 - m.Pik6, 0),;
          Alo7   WITH MAX(Alo7 - m.Pik7, 0),;
          Alo8   WITH MAX(Alo8 - m.Pik8, 0),;
          TotAlo WITH MAX(TotAlo - m.TotPik, 0)
ENDIF  
*-- Refresh STYDYE relation
m.cWareCode = lcNewWare
SELECT (lc_TmpOrdL)
GATHER MEMVAR MEMO
GO RECNO('ORDLINE') IN ORDLINE
lcStkAlias = IIF(EOF(lc_TmpStyD), 'STYDYE', lc_TmpStyD)  
IF EOF(lc_TmpStyD)
  SELECT STYDYE 
  SCATTER TO laStyDye MEMVAR
  INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye
ENDIF
SELECT (lc_TmpStyD) 
REPLACE Alo1   WITH Alo1 + m.Pik1,;
        Alo2   WITH Alo2 + m.Pik2,;
        Alo3   WITH Alo3 + m.Pik3,;
        Alo4   WITH Alo4 + m.Pik4,;
        Alo5   WITH Alo5 + m.Pik5,;
        Alo6   WITH Alo6 + m.Pik6,;
        Alo7   WITH Alo7 + m.Pik7,;
        Alo8   WITH Alo8 + m.Pik8,;
        TotAlo WITH TotAlo + m.TotPik 
  
SELECT (lnCurAlias)


*!*************************************************************
*! Name      : lfVldWare
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Valid function for puPickWare popup 
*!*************************************************************
*! Called From : ALORDAL.SPR
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfVldWare(m.cWareCode)
*!*************************************************************
FUNCTION lfVldWare
PARAMETERS lcWareHous
PRIVATE llRetVal

llRetVal = .T.
*IF !m.lVldWare
  *-- Check if the current style is assigned to the selected warehouse
  IF !SEEK(m.Style + lcWareHous + SPACE(10), 'STYDYE')
    *Message 44012 Style: ð is not assigned to warehouse : ð.
    *Message Button 44004 : Add/Reenter
    IF gfModalGen('QRM44012B44004', 'DIALOG', ALLTRIM(m.Style)+'|'+;
                  ALLTRIM(lcWareHous)) = 1
      DO gpAdStyWar WITH m.Style, SPACE(10), lcWareHous
      m.lVldWare  = .T.
    ELSE
      llRetVal = .F.
    ENDIF             
  ENDIF
*ENDIF  
RETURN llRetVal .AND. lfAddLine(lcWareHous) 


*!*************************************************************
*! Name      : lfGetTotStk
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Calculates total stock per order line
*!*************************************************************
*! Called From : BROWSE command
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetTotStk()
*!*************************************************************
*B804502,1 HBG 29/10/2001 add parameter to calculate total availabel in case of brows[Begin]
FUNCTION lfGetTotStk
PARAMETERS lcFrmBrw
*B804502,1 [End]
*E500304,1 WAB - Calculate the avialble qty (stk-alo) by calling function
*E500304,1 WAB - lftotavlbl() to get the available qty
*E500304,1 WAB - START
*RETURN IIF(EOF(lc_TmpStyD),;
           MAX(STYDYE.Stk1-STYDYE.Alo1,0)+;
           MAX(STYDYE.Stk2-STYDYE.Alo2,0)+;
           MAX(STYDYE.Stk3-STYDYE.Alo3,0)+;
           MAX(STYDYE.Stk4-STYDYE.Alo4,0)+;
           MAX(STYDYE.Stk5-STYDYE.Alo5,0)+;
           MAX(STYDYE.Stk6-STYDYE.Alo6,0)+;
           MAX(STYDYE.Stk7-STYDYE.Alo7,0)+;
           MAX(STYDYE.Stk8-STYDYE.Alo8,0),;
           MAX(EVAL(lc_TmpStyD+'.Stk1-'+lc_TmpStyD+'.Alo1'), 0)+;
           MAX(EVAL(lc_TmpStyD+'.Stk2-'+lc_TmpStyD+'.Alo2'), 0)+;
           MAX(EVAL(lc_TmpStyD+'.Stk3-'+lc_TmpStyD+'.Alo3'), 0)+;
           MAX(EVAL(lc_TmpStyD+'.Stk4-'+lc_TmpStyD+'.Alo4'), 0)+;
           MAX(EVAL(lc_TmpStyD+'.Stk5-'+lc_TmpStyD+'.Alo5'), 0)+;
           MAX(EVAL(lc_TmpStyD+'.Stk6-'+lc_TmpStyD+'.Alo6'), 0)+;
           MAX(EVAL(lc_TmpStyD+'.Stk7-'+lc_TmpStyD+'.Alo7'), 0)+;
           MAX(EVAL(lc_TmpStyD+'.Stk8-'+lc_TmpStyD+'.Alo8'), 0))
PRIVATE lnTotal, lnCurAlias
*lnTotal =lfTotAvlbl('9',.T.,Ordline.STYLE,Ordline.cWareCode,Ordline.Dyelot) 
*B804502,1 HBG 29/10/2001 call lfTotAvlbl() with parameter 'T' to calculate ;
*B804502,1                total availabel in case of browse[Begin]
IF TYPE('lcFrmBrw ')='L'
  lcFrmBrw = 'N'
ENDIF
IF lcFrmBrw = 'T'
  lnTotal =lfTotAvlbl('T') 
ELSE
*B804502,1 [End]
  lnTotal =lfTotAvlbl('9') 
*B804502,1 HBG 29/10/2001 end if cthe parameter is 'T' to calculate ;
*B804502,1                total availabel in case of browse[Begin]  
ENDIF  
*B804502,1 [End]
RETURN lnTotal


*+ lfTotAvlbl('2',.T.) + lfTotAvlbl('3',.T.) + ;
         lfTotAvlbl('4',.T.) + lfTotAvlbl('5',.T.) + lfTotAvlbl('6',.T.) + ;
         lfTotAvlbl('7',.T.) + lfTotAvlbl('8',.T.)  
RETURN(lnTotal)
*E500304,1 WAB - END

*!*************************************************************
*! Name      : lfAddLine
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Controls show status of WINDOW (lcAlOrdAl3)
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAddLine()
*!*************************************************************
FUNCTION lfAddLine
PARAMETERS lcWareCode, lcDyelot
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
IF EOF(lc_TmpOrdL)
  SELECT ORDLINE
  SCATTER MEMVAR MEMO
  m.cWareCode  = IIF(TYPE('lcWareCode')='C', lcWareCode, m.cWareCode)
  m.Dyelot     = IIF(TYPE('lcDyelot')='C', lcDyelot, m.Dyelot)
  m.lForceAlc  = .F.
  m.cStatus    = ' '
  *-- Check if the warehouse is a valid one
  m.lVldWare   = !llMultWare .OR. ;
                 SEEK(m.Style + m.cWareCode + SPACE(10), 'STYDYE')
  m.lVldDye    = !llUseDyes .OR. Style.cDye_flg = 'N'  ;
                            .OR. !EMPTY(m.Dyelot) .AND. SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
  m.nForcdQty  = 0              
  m.lAllocated = llGenPkTk
  INSERT INTO (lc_TmpOrdL) FROM MEMVAR
ELSE
  SELECT (lc_TmpOrdL)
  SCATTER MEMVAR MEMO
  m.cWareCode = IIF(TYPE('lcWareCode')='C', lcWareCode, m.cWareCode)
  m.Dyelot    = IIF(TYPE('lcDyelot')='C', lcDyelot, m.Dyelot)
  *-- Check if the warehouse is a valid one
  m.lVldWare  = !llMultWare .OR. ;
                SEEK(m.Style + m.cWareCode + SPACE(10), 'STYDYE')
  m.lVldDye   = !llUseDyes .OR. Style.cDye_flg = 'N'  ;
                         .OR. !EMPTY(m.Dyelot) .AND. SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
  m.lAllocated = llGenPkTk                         
  GATHER MEMVAR MEMO
ENDIF  

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfShowLines
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Controls show status of WINDOW (lcAlOrdAl3)
*!*************************************************************
*! Calls     : lfGetScale(), lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowLines()
*!*************************************************************
FUNCTION lfShowLines
*-- If the browse window is not zoomed
IF !llZoomed   
  IF BETWEEN(RECNO('ORDLINE'), 1, RECCOUNT('ORDLINE'))
    GO RECNO('ORDLINE') IN ORDLINE
  ENDIF     
  IF llMultWare
    lnElem     = ASCAN(laWareHous, m.cWareCode)
    puPickWare = IIF(lnElem <> 0, ASUBSCRIPT(laWareHous, lnElem, 1), 0)
  ENDIF  
  *-- If the current line is picked
  IF lfLinIsPck()
    =lfGetScale()
    SHOW GET m.Pik1 	 ENABLE
    SHOW GET m.Pik2 	 ENABLE
    SHOW GET m.Pik3 	 ENABLE
    SHOW GET m.Pik4 	 ENABLE
    SHOW GET m.Pik5 	 ENABLE
    SHOW GET m.Pik6 	 ENABLE
    SHOW GET m.Pik7 	 ENABLE
    SHOW GET m.Pik8 	 ENABLE
    SHOW GET pbPick      DISABLE
    SHOW GET pbRel       ENABLE
    *E301340,1 WAB-1-Check if there is shaving in current order line by check 
    *E301340,1       if any qty? > pik?
    *E301340,1     2-if there is shaving and the setting of order shaving is 
    *E301340,1       not 'Never'	-------> Enable reason popup  ELSE DISABLE
    *E301340,1     3-IF there is no reason in reason filed and the Setting of order shaving is Automatic set the reason 
    *E301340,1       popoup with the Default popup else get the reason disc of the reson code
    llShave    = lfCanShave()
    lcDispReas = IIF(llShave AND lnOrdShv <> 3,'ENABLE','DISABLE')
    IF !EOF(lc_TmpOrdL) AND !EMPTY(&lc_TmpOrdL..cReason) 
      lnElem  = ASCAN(laShReson, &lc_TmpOrdL..cReason)
      IF lnElem > 0
        lnSelRsn = ASUBSCRIPT(laShReson, lnElem, 1)
      ELSE
        lnSelRsn = 1
      ENDIF  
    ELSE
      lnSelRsn = IIF(llShave AND lnOrdShv = 1,lnReson,1) 
    ENDIF
    
    SHOW GET lnSelRsn &lcDispReas
    
    *E301340,1 WAB - END
    *-- If stock is not available, enable force allocation button
    IF lfMayForcAlc()

      *-- MAB 09/01/1999 Show get only is force check is found. [Begin]
      IF llForceBar
        SHOW GET m.lForceAlc ENABLE
        lcForcStat = "ENABLE"
      ENDIF
      *-- MAB 09/01/1999 Show get only is force check is found. [End  ]
        
    ELSE

      *-- MAB 09/01/1999 Show get only is force check is found. [Begin]
      IF llForceBar
        SHOW GET m.lForceAlc DISABLE
        lcForcStat = "DISABLE"
      ENDIF  
      *-- MAB 09/01/1999 Show get only is force check is found. [End  ]

    ENDIF  
    lcPickStat  = 'DISABLE'
    lcRelStat   = 'ENABLE'
    *-- Disable all options on the child window
    *IF EMPTY(m.Piktkt)
    *  SHOW GET puPickWare  ENABLE
    *  SHOW GET ibDyelot    ENABLE
    *ELSE
      SHOW GET puPickWare  DISABLE
      SHOW GET ibDyelot    DISABLE
    *ENDIF  
    SHOW GET m.Dyelot    DISABLE
    SHOW GET ibStyle     DISABLE
    SHOW GET m.Style     DISABLE
    lcStyStat  = 'DISABLE' 
  ELSE
    SHOW GET m.Pik1 	 DISABLE
    SHOW GET m.Pik2 	 DISABLE
    SHOW GET m.Pik3 	 DISABLE
    SHOW GET m.Pik4 	 DISABLE
    SHOW GET m.Pik5 	 DISABLE
    SHOW GET m.Pik6 	 DISABLE
    SHOW GET m.Pik7 	 DISABLE
    SHOW GET m.Pik8 	 DISABLE
    *E301340,1 WAB - show the popup reason disabled
    *E301340,1 WAB - START
    lnSelRsn   =  1
    SHOW GET lnSelRsn DISABLE
    *E301340,1 WAB - END

    *-- MAB 09/01/1999 Show get only is force check is found. [Begin]
    IF llForceBar
      SHOW GET m.lForceAlc DISABLE
    ENDIF
    *-- MAB 09/01/1999 Show get only is force check is found. [End  ]
      
    SHOW GET pbPick      ENABLE
    SHOW GET pbRel       DISABLE
    lcPickStat  = 'ENABLE'    
    lcRelStat   = 'DISABLE'
    lcForcStat  = "DISABLE"

    SHOW GET puPickWare  ENABLE
    IF llUseDyes 
      IF Style.cDye_flg = 'Y' 
        SHOW GET ibDyelot ENABLE
        SHOW GET m.Dyelot ENABLE
      ELSE
        SHOW GET ibDyelot DISABLE
        SHOW GET m.Dyelot DISABLE
      ENDIF
      SHOW GET ibStyle    DISABLE
      SHOW GET m.Style    DISABLE
      lcStyStat  = 'DISABLE' 
    ELSE
      SHOW GET ibStyle    ENABLE
      SHOW GET m.Style    ENABLE
      lcStyStat  = 'ENABLE' 
    ENDIF  
  ENDIF
  *E500304,1 WAB - show the O.T.S butt Enabled
  *E500304,1 WAB - START
  SHOW GET pbPbEx11    ENABLE
  *E500304,1 WAB - END

  =lfRefresh(lcAlOrdAl3)  
ELSE
  *-- If the current line is picked
  IF lfLinIsPck()
    SHOW GET pbPick     DISABLE
    SHOW GET pbRel      ENABLE
    lcPickStat  = 'DISABLE'
    lcRelStat   = 'ENABLE'    
  ELSE
    SHOW GET pbPick     ENABLE
    SHOW GET pbRel      DISABLE
    lcPickStat  = 'ENABLE'    
    lcRelStat   = 'DISABLE'
  ENDIF
  lcForcStat = "DISABLE"
ENDIF


*!*************************************************************
*! Name      : lfLinIsPck
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Returns .t. if the current line is Picked
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowLines()
*!*************************************************************
FUNCTION lfLinIsPck
*B037363,4 HBG 25/12/2003 Fix bug of not saveing modification Qty incase of 
*B037363,4                increase Order qty and generating Piktkt for Allocated line[Begin]
*RETURN (EOF(lc_TmpOrdL) .OR. DELETED(lc_TmpOrdL)) .AND. ORDLINE.Picked ;
        .OR. EVAL(lc_TmpOrdL + '.Picked') 
RETURN (EOF(lc_TmpOrdL) .OR. DELETED(lc_TmpOrdL)) .AND. ORDLINE.Picked ;
        .OR. EVAL(lc_TmpOrdL + '.Picked')  OR EVAL(lc_TmpOrdL + '.cStatus') = 'M'
*B037363,4 [End]
*!*************************************************************
*! Name      : lfSclNotVld
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Check if extra size scales contain ordered quantities
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  .T. if the scale is not valid
*!*************************************************************
*! Example            :  =lfShowLines()
*!*************************************************************
FUNCTION lfSclNotVld
PARAMETERS lnStartCnt, lnEndCnt
PRIVATE lcCount, lnCount, llRetVal

llRetVal     = .F.
FOR lnCount  = lnStartCnt TO lnEndCnt
  lcCount    = STR(lnCount,1)
  IF EVALUATE('m.Qty' + lcCount) > 0
    =gfModalGen('TRM44009B00000','DIALOG' , lcStyHdr + '|' + ;
      		      ALLTRIM(m.Style) + '/' + ALLTRIM(SCALE.Scale) + '|' +;
      		      lcStyHdr + '|' + ALLTRIM(lcOldVal) + '/' + ;
      		      ALLTRIM(lcOldScale)) 
    llRetVal = .T.
    EXIT
  ENDIF
ENDFOR
RETURN llRetVal
    
*!*************************************************************
*! Name      : lfSubstitute
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Substitute style values
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowLines()
*!*************************************************************
FUNCTION lfSubstitute
PRIVATE lcStyle, lnCurAlias, lnOrdCurRc

lnCurAlias = SELECT(0)
*-- Save the selected new style
lcStyle    = m.Style
m.Style    = lcOldVal

=lfAddLine()
SELECT (lc_TmpOrdL)
IF EMPTY(m.AltStyle)
  m.AltStyle = m.Style
ELSE
  IF lcStyle = ORDLINE.Style
    m.AltStyle = SPACE(19)
  ENDIF    
ENDIF  
m.Style    = lcStyle
m.Scale    = STYLE.Scale
m.cStatus  = 'U'
GATHER MEMVAR MEMO
IF !EOF('ORDLINE')
  GO RECNO('ORDLINE') IN ORDLINE
ENDIF  
SELECT ORDLINE
lnOrdCurRc = RECNO()
=lfOrdBrow()
IF BETWEEN(lnOrdCurRc, 1, RECCOUNT('ORDLINE'))
  GO lnOrdCurRc IN ORDLINE
  =lfwOrdBrow()
  GO lnOrdCurRc IN ORDLINE
ENDIF
SELECT (lnCurAlias)


*!*************************************************************
*! Name      : lfAdjStyDye
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Adjust stydye file
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowLines()
*!*************************************************************
FUNCTION lfAdjStyDye
PARAMETERS lcSign
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
*-- Update (lc_TmpStyD) file
IF !SEEK(m.Style + m.cWareCode + SPACE(10), lc_TmpStyD) .AND.;
    SEEK(m.Style + m.cWareCode + SPACE(10), 'STYDYE')
  SELECT STYDYE 
  SCATTER TO laStyDye 
  INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye
ENDIF
SELECT (lc_TmpStyD)

*-- Allocate available quantities by default. (adding)
IF lcSign = 'A'
  REPLACE Alo1   WITH Alo1 + m.Pik1,;
          Alo2   WITH Alo2 + m.Pik2,;
          Alo3   WITH Alo3 + m.Pik3,;
          Alo4   WITH Alo4 + m.Pik4,;
          Alo5   WITH Alo5 + m.Pik5,;
          Alo6   WITH Alo6 + m.Pik6,;
          Alo7   WITH Alo7 + m.Pik7,;
          Alo8   WITH Alo8 + m.Pik8,;
          TotAlo WITH TotAlo + m.TotPik
  IF llUseDyes .AND. Style.cDye_flg = 'Y' 
    IF !SEEK(m.Style + m.cWareCode + m.Dyelot, lc_TmpStyD);
      .AND. SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
      SELECT STYDYE
      SCATTER TO laStyDye MEMVAR
      INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye
    ENDIF  &&EndIF !SEEK(..)
    SELECT (lc_TmpStyD)
    REPLACE Alo1   WITH Alo1 + m.Pik1,;
            Alo2   WITH Alo2 + m.Pik2,;
            Alo3   WITH Alo3 + m.Pik3,;
            Alo4   WITH Alo4 + m.Pik4,;
            Alo5   WITH Alo5 + m.Pik5,;
            Alo6   WITH Alo6 + m.Pik6,;
            Alo7   WITH Alo7 + m.Pik7,;
            Alo8   WITH Alo8 + m.Pik8,;
            TotAlo WITH TotAlo + m.TotPik
    *B802829,1 KHM 03/06/2000 (Begin) Calling this function in order
    *B802829,1                to adjust the allocated qty per dyelot.
    =lfAdjTStyD('A','')
    *B802829,1 KHM 03/06/2000 (End)
    
  ENDIF  &&EndIf llUseDyes 
*-- DeAllocate available quantities by default. (subtracting)
ELSE
  REPLACE Alo1   WITH MAX(Alo1 - m.Pik1, 0),;
          Alo2   WITH MAX(Alo2 - m.Pik2, 0),;
          Alo3   WITH MAX(Alo3 - m.Pik3, 0),;
          Alo4   WITH MAX(Alo4 - m.Pik4, 0),;
          Alo5   WITH MAX(Alo5 - m.Pik5, 0),;
          Alo6   WITH MAX(Alo6 - m.Pik6, 0),;
          Alo7   WITH MAX(Alo7 - m.Pik7, 0),;
          Alo8   WITH MAX(Alo8 - m.Pik8, 0),;
          TotAlo WITH MAX(TotAlo - m.TotPik, 0)

  IF llUseDyes .AND. Style.cDye_flg = 'Y' 
    IF !SEEK(m.Style + m.cWareCode + m.Dyelot, lc_TmpStyD);
      .AND. SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
      SELECT STYDYE
      SCATTER TO laStyDye MEMVAR
      INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye
    ENDIF  &&EndIF !SEEK(..)
    SELECT (lc_TmpStyD)
    REPLACE Alo1   WITH MAX(Alo1 - m.Pik1, 0),;
            Alo2   WITH MAX(Alo2 - m.Pik2, 0),;
            Alo3   WITH MAX(Alo3 - m.Pik3, 0),;
            Alo4   WITH MAX(Alo4 - m.Pik4, 0),;
            Alo5   WITH MAX(Alo5 - m.Pik5, 0),;
            Alo6   WITH MAX(Alo6 - m.Pik6, 0),;
            Alo7   WITH MAX(Alo7 - m.Pik7, 0),;
            Alo8   WITH MAX(Alo8 - m.Pik8, 0),;
            TotAlo WITH MAX(TotAlo - m.TotPik, 0)
  ENDIF  &&EndIf llUseDyes 
  *B802829,1 KHM 03/06/2000 (Begin) Calling this function in order to 
  *B802829,1                decrease the allocated qty when releasing 
  *B802829,1                the style that has dyelot.
  =lfAdjTStyD('R','')
  *B802829,1 KHM 03/06/2000 (End)
  
ENDIF  
lcStkAlias  = lc_TmpStyD
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfChngOrd
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Adjust stydye file
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowLines()
*!*************************************************************
FUNCTION lfChngOrd
PARAMETERS llChngAll 
PRIVATE lnOption

lnOption = 1 
IF !llChngAll 
  lnOption = gfModalGen('TRM44002B44002', 'DIALOG') 
  IF lnOption < 3
    llChngAll = lnOption = 2
    m.cStatus = 'M'
  ENDIF   
ENDIF  
RETURN lnOption < 3

*!*************************************************************
*! Name      : lfForcAl
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Adjust stydye file
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowLines()
*!*************************************************************
FUNCTION lfForcAl
PARAMETERS llForcLAlc, lnForcdQty      
PRIVATE lnOption, llRetVal
lnOption = 1 
IF !llForceAlc .AND. lnObjVal > lcOldVal  .AND. !llForcLAlc
  
  *-- E301225,1 Allow Force allocation option. [Begin]
  IF llForceBar
    lcButton  = "44002"
    lcOptMsg  = "Do you want to force allocation?"
    lcMsgType = "QR"
  ELSE
    lcButton = "00000"
    lcOptMsg = " "
    lcMsgType = "TR"
  ENDIF
  
  *lnOption = gfModalGen('TRM44000B44002', 'DIALOG',;
             'The allocated quantity|the available quantity')
  
  lnOption = gfModalGen(lcMsgType+'M44000B'+lcButton, 'DIALOG',;
             'The allocated quantity|the available quantity'+;
             "|"+lcOptMsg)

  IF !llForceBar
    lnOption = 3
  ENDIF
  *-- E301225,1 Allow Force allocation option. [End  ]

  IF lnOption < 3
    llForcLAlc = lnOption = 2
  ENDIF   
ENDIF
IF lnOption < 3
  m.nForcdQty = m.nForcdQty - lcOldVal + lnForcdQty 
  *m.lForceAlc = .T.
  *llRetVal    = .T.
  STORE .T. TO m.lForceAlc, llRetVal
  SHOW GET m.lForceAlc
ELSE
  llRetVal    = .F.
ENDIF
RETURN llRetVal


*!*************************************************************
*! Name      : lfvScope
*! Developer : RENEE
*! Date      : 07/01/1996
*! Purpose   : Adjust stydye file
*!*************************************************************
*! Calls     : lfRefresh()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowLines()
*!*************************************************************
FUNCTION lfvScope
PRIVATE lcOldStyle, lcOldStore, lcOldGroup, lcOldScope, llUpdated

*-- Store old variables:
lcOldStyle = lcStyle
lcOldStore = lcStore
lcOldGroup = lcGroup
lcOldScope = lcBrowExp

*-- Get descriptions...
lcStyDesc  = IIF(EMPTY(lcStyle), '', LOOKUP(STYLE.Desc1, lcStyle, STYLE.Style, 'STYLE'))
lcStorDesc = IIF(EMPTY(lcStore), '',;
				 LOOKUP(CUSTOMER.StName, 'S'+laData[2] + lcStore,;
				 CUSTOMER.Type, 'CUSTOMER')) 
lcStorStat = IIF(ORDHDR.Multi = 'Y', 'ENABLE', 'DISABLE')				 
llUpdated  = .F.
PUSH KEY
ON KEY
DO (gcScrDir + gcWinAppl + '\ALORDALS.SPR')
POP KEY

*!*************************************************************
*! Name      : lfvSetScp
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvSetScp
PRIVATE lnOrdRecNo, lnCurAlias, lnCurTag

IF llUpdated
  *-- If the scope has not been cleared
  *-- Check for the validity of the seleted scope option
  IF !EMPTY(lcStore + lcStyle + lcGroup) 
    lnCurAlias = SELECT (0)
    SELECT ORDLINE
    lnOrdRecNo = RECNO()
    lnCurTag   = VAL(SYS(21))
    SET ORDER TO TAG ORDLINST 
    lcNdxExp   = SYS(14,VAL(SYS(21)))
    IF SEEK(laData[9] + laData[1] + IIF(EMPTY(lcStore), STORE, lcStore)+;
      ALLTRIM(lcStyle))
      IF !EMPTY(lcGroup)  
        LOCATE REST ;
              WHILE &lcNdxExp = laData[9] + laData[1] + ;
      			    IIF(EMPTY(lcStore), STORE, lcStore) +  ALLTRIM(lcStyle);
    		    FOR GROUP = lcGroup
      ENDIF 		    
    ENDIF  
    IF BETWEEN(lnOrdRecNo, 1, RECCOUNT('ORDLINE'))
      GO lnOrdRecNo IN ORDLINE
    ENDIF  
    SET ORDER TO (lnCurTag)
    IF !FOUND()
      =gfModalGen('TRM44051B00000','DIALOG')
    ELSE
      CLEAR READ
      *-- Set 
      IF !EMPTY(lcStore + lcStyle) 
        lcBrowExp = IIF(!EMPTY(lcStore), [+ lcStore], [ + STORE]) +;
        		    IIF(!EMPTY(lcStyle), [ + lcStyle], []) 
        IF !llOrdLStr
          SET MARK OF BAR 3 OF _OPTIONPOP TO .F.  
          SET MARK OF BAR 4 OF _OPTIONPOP TO .T.
          SET ORDER TO TAG ORDLINST 
          lcNdxExp = SYS(14,VAL(SYS(21)))
          llOrdLStr  = .T.
        ENDIF  
      ELSE
        *lcBrowExp  =  ['STORE']
        lcBrowExp  =  ''
      ENDIF
      IF !EMPTY(lcGroup)
        lcBrowExp = lcBrowExp + [.AND. Group = lcGroup]
      ENDIF
      =lfOrdBrow()  
      *SHOW WINDOW (lcOrdBrTtl) REFRESH SAME
    ENDIF  		
    SELECT (lnCurAlias)
  ELSE
    CLEAR READ
    *lcBrowExp  =  ['STORE']      
    lcBrowExp  =  ''      
    *SHOW WINDOW (lcOrdBrTtl) REFRESH SAME
    =lfOrdBrow()  
  ENDIF    		
ELSE
  CLEAR READ
ENDIF  

*!*************************************************************
*! Name      : lfvClrScp
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvClrScp
*-- Store old variables:
IF !EMPTY(lcStyle + lcStore + lcGroup)
  =lfClrScope()
  llUpdated = .T.
  SHOW GETS
ENDIF

FUNCTION lfClrScope
lcStyle   = SPACE(lnStyleWid)
lcStore   = SPACE(8)
lcGroup   = SPACE(1)
lcStyDesc  = ''
lcStorDesc = ''
lcBrowExp =  ''
llOrdLStr   = .F.
SET ORDER TO TAG ORDLINE IN ORDLINE
*B607537,1 HBG 12/04/2005 Fix bug of if open scope screen and without selecting anything press close it won't close the screen [Begin]
=lfOrdBrow()
*B607537,1 [End]



*!*************************************************************
*! Name      : lfvClsScope
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvClsScope
*-- Store old variables:
*B607537,1 HBG 12/04/2005 Fix bug of if open scope screen and without selecting anything press close it won't close the screen [Begin]
*IF llUpdated 
*B607537,1 [End]
  CLEAR READ
*B607537,1 HBG 12/04/2005 Fix bug of if open scope screen and without selecting anything press close it won't close the screen [Begin]
*  =lfOrdBrow()
*ENDIF  
*B607537,1 [End]
  
*!*************************************************************
*! Name      : lfvStyScp
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyScp
IF !lfVldStyle(@lcStyle)
  *-- Now, if the style is changed, check if it may be used as a 
  *-- substitute or not
  lcStyle = SPACE(lnStyleWid)
ENDIF    
lcStyDesc = IIF(lfStyEmpty(lcStyle), '', LOOKUP(STYLE.Desc1, lcStyle, STYLE.Style, 'STYLE'))
=lfRefresh()
llUpdated  = llUpdated .OR. !(lcStyle == lcOldStyle)

*!*************************************************************
*! Name      : lfvStore
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Validate store code
*!*************************************************************
*! Calls     : CUSBROW,gfDialog,gfGetAdr
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStore()
*!*************************************************************
FUNCTION lfvStore
lcStore  = PADR(ALLTRIM(lcStore), 8)
llBrowse = llBrowse .OR. '?' $ lcStore
IF llBrowse .OR. lcStore<>lcOldVal
  IF llBrowse  .OR. !EMPTY(lcStore) 
    IF CUSBROW(laData[2], 'S', @lcStore, .T.)
      lcStorDesc = CUSTOMER.StName  
    ELSE  
      lcStore    = lcOldVal
    ENDIF  
    llBrowse     = .F.
  ELSE
    lcStorDesc = ''
  ENDIF
  =lfRefresh()
ENDIF  
lcStore    = PADR(ALLTRIM(lcStore), 8)
llUpdated  = llUpdated .OR. !(lcStore == lcOldStore)

*!*************************************************************
*! Name      : lfvStore
*! Developer : Renee
*! Date      : 01/01/1996
*! Purpose   : Validate store code
*!*************************************************************
*! Calls     : CUSBROW,gfDialog,gfGetAdr
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStore()
*!*************************************************************
FUNCTION lfvGroup
llUpdated  = llUpdated .OR. !(lcGroup == lcOldGroup)

*!*************************************************************
*! Name      : lfLinIsChg
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfLinIsChg
RETURN ORDLINE.Style <> EVAL(lc_TmpOrdL + '.Style') .OR.;
	   ORDLINE.cWareCode <> EVAL(lc_TmpOrdL + '.cWareCode') .OR.;
       ORDLINE.Dyelot <> EVAL(lc_TmpOrdL + '.Dyelot') 
       
       
*.OR. m.lForceAlc
              
*!*************************************************************
*! Name      : lfRelPktkt
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: A picking ticket number
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfRelPktkt(ORDLINE.PikTkt)
*!*************************************************************
FUNCTION lfRelPktkt
PARAMETERS lcCurPick
PRIVATE lnCurAlias, lnRecNo

*Ren
IF lcCurPick <> REPLICATE("*", FSIZE("PIKTKT","PIKTKT"))
  lnCurAlias = SELECT(0)
  SELECT ORDLINE
  lnRecNo    = RECNO()
  *-- Look for other lines for the same pick ticket in ORDLINE
  IF SEEK(laData[9] + laData[1])
    *-- check if there are any more lines picked on the same pickticket
    LOCATE REST WHILE cOrdType + Order = laData[9] + laData[1] ;
           FOR PikTkt = lcCurPick  
    IF !FOUND()
      *-- Copy the line to PIKLINE and Cancel Piktkt
      SELECT (lc_TmpPkLn)
      IF SEEK(laData[1] + lcCurPick)

        *MAB 06/13/2000 Save Memvar into memo file [Begin]
        lcActvMemo = gfTempName() + ".MEM"
        SAVE ALL LIKE * TO &gcWorkDir.&lcActvMemo
        *MAB 06/13/2000 Save Memvar into memo file [End  ]

        SCAN REST WHILE Order + Piktkt = laData[1] + lcCurPick
          *MAB 06/13/2000 Insert into table from memvars [Begin]
          *SCATTER TO laPikLine
          *INSERT INTO PIKLINE FROM ARRAY laPikLine
          SCATTER MEMVAR MEMO
          INSERT INTO PIKLINE FROM MEMVAR
          *MAB 06/13/2000 Insert into table from memvars [End  ]
        ENDSCAN

        *MAB 06/13/2000 Restore Memvar from memo file [Begin]
        RESTORE FROM &gcWorkDir.&lcActvMemo ADDITIVE
        ERASE &gcWorkDir.&lcActvMemo
        *MAB 06/13/2000 Restore Memvar from memo file [End  ]

      ENDIF  
      SELECT PIKTKT
      IF SEEK(laData[1] + lcCurPick)
        *-- Replace picking ticket status with 'X' for cancelled
        REPLACE Status WITH 'X'
        *E123070,1 WAM 07/12/2004 Generate 940 EDI transaction for pick tickets directed to 3PL providers.
        IF 'AS' $ gcCmpModules AND SEEK(cWareCode,'WareHous') AND  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT') AND ;
             SEEK(EDIACPRT.cPartCode+'940','EDIPD') AND SEEK('940'+PADR(lcCurPick,40)+'W'+WareHous.cThrdPLPr,'EDITRANS')
          SELECT 'EDITRANS'
          DELETE
        ENDIF
        *E123070,1 WAM 07/12/2004 (End)
      ENDIF
    ENDIF
  ENDIF
  SELECT ORDLINE
  IF BETWEEN(lnRecNo, 1, RECCOUNT())
    GO lnRecNo
  ENDIF  
  SELECT (lnCurAlias)
ENDIF


*!*************************************************************
*! Name      : lfChkStock
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: A picking ticket number
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfRelPktkt(ORDLINE.PikTkt)
*!*************************************************************
FUNCTION lfChkStock
RETURN .T.
PARAMETERS lnOption
PRIVATE lnStock, llRetVal 

*-- 
lnStock = 0
IF lfStkAvail(@lnStock) > 0
   *-- If allocation is forced
  IF m.nForcdQty > 0 
      *-- Message : 44008
      *-- 'Style\warehouseð ð stock has decreased since the beginning of'
      *-- 'the session. Forced pieces have increased by ð. Force anyway?'
      *-- Button   : 44003
      *-- <Force> <Deallocate>
      lnOption = gfModalGen('QRM44008B44003', 'DIALOG',;
      						  IIF(llUseDyes .AND. STYLE.cDye_flg = 'Y', '\Dyelot', '');
      						  +'|'+ALLTRIM(m.Style)+'\'+ALLTRIM(m.cWareCode)+;
      						  IIF(llUseDyes .AND. STYLE.cDye_flg = 'Y', '\'+ALLTRIM(m.Dyelot), '');
      						  +'|'+ALLTRIM(STR(lnStock)))
  *-- Else, if allocation is forced
  ELSE
      *-- If no stock is available, give a message of Force Allocation?
      IF  lnStock = 0
         *-- Message : 44026
         *-- 'Stock is no longer available for style\warehouseð ð. '
         *-- 'Force allocation?'
         *-- Button   : 44003
         *-- <Force> <Deallocate>
         lnOption = gfModalGen('QRM44026B44003', 'DIALOG',;
      						        IIF(llUseDyes .AND. STYLE.cDye_flg = 'Y', '\Dyelot', '');
      						        +'|'+ALLTRIM(m.Style)+'\'+ALLTRIM(m.cWareCode)+;
      						        IIF(llUseDyes .AND. STYLE.cDye_flg = 'Y', '\'+ALLTRIM(m.Dyelot), ''))
      ELSE
         *-- Message : 44025
         *-- 'Style\warehouseð ð stock has decreased since
          *-- the beginning of the session by ð pieces. Force allocation ?'
         *-- Button   : 44005
         *-- <Force all> <force available> <Deallocate>
          lnOption = gfModalGen('QRM44025B44005', 'DIALOG',;
                					  IIF(llUseDyes .AND. STYLE.cDye_flg = 'Y', '\Dyelot', '');
      						         +'|'+ALLTRIM(m.Style)+'\'+ALLTRIM(m.cWareCode)+;
      						         IIF(llUseDyes .AND. STYLE.cDye_flg = 'Y', '\'+ALLTRIM(m.Dyelot), '');
      						         +'|'+ALLTRIM(STR(lnStock)))

      ENDIF  
  ENDIF
  llRetVal  = .T.
ELSE
  llRetVal  = .F.
ENDIF  

*!*************************************************************
*! Name      : lfStkAvail
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: A picking ticket number
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfRelPktkt(ORDLINE.PikTkt)
*!*************************************************************
FUNCTION lfStkAvail
PARAMETERS lnStkAvail
PRIVATE lcStr, lnCurAlias, llRetVal

lnCurAlias = SELECT(0)
SELECT (lc_TmpStyD)
FOR lnCount = 1 TO SCALE.Cnt
  lcStr = STR(lnCount,1)
  IF STYDYE.Stk&lcStr - STYDYE.Alo&lcStr < m.Alo&lcStr ;
    lnStkAvail = STYDYE.Stk&lcStr - STYDYE.Alo&lcStr 
    EXIT
  ENDIF
ENDFOR
SELECT (lnCurAlias)
RETURN lnCount 

*!*************************************************************
*! Name      : lfMayForcAlc
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: A picking ticket number
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfRelPktkt(ORDLINE.PikTkt)
*!*************************************************************
FUNCTION lfMayForcAlc
*RETURN m.TotQty > m.TotPik AND lfGetTotStk() < m.TotQty
RETURN lfGetTotStk() < m.TotQty

FUNCTION lfIsForced
*RETURN m.TotPik > lfGetTotStk()
RETURN m.TotPik > lfGetTotStk() .AND. m.TotPik > m.TotQty


*!*************************************************************
*! Name      : lfChkUnCmS
*! Developer : Renee
*! Date      : 09/10/98
*! Purpose   : Check if there is uncomplete session
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : llFromSetUp
*! 						.T. only if called from the screen setup
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfChkUnCmS()
*!*************************************************************
FUNCTION lfChkUnCmS
PRIVATE lnAlias, llFoundUnS, lnReprocess

llFoundUnS   = .F.

*-- Function gfUnCompSession checks for an incomplete session of the
*-- passed program ID. 
*-- If one is found, it attempts to open all temp files.
*--   If it succeeds in opening the temp files, it restores variables as well,
*--   and returns .T.,
*--   Else, it erases the temp files, and returns .f.
*--   and marks the record in UNCMSES as complete  	
*--   and returns .f.	
*-- Else, it returns .f.
IF gfUnCompSession(lcProgID, gnProgCopy, "Allocation by Order")
  *-- the next time you pass that function, you may create temp files
  llCreateTmp = .T.
  =lfSetRelations()
  llFoundUnS   = .T.
  *-- Set screen mode according to the incomplete session mode.
  STORE .F. TO laScrMode
  laScrMode[3] = .T.

  *-- Restore session number
  lcSession = UnCmSess.cSession
  *-- Restore current object
  lcCurObj  = ALLTRIM(UnCmSess.cCurrObj)

  SELECT ORDLINE
  SET ORDER TO TAG IIF(llOrdLStr, 'ORDLINST', 'ORDLINE') IN ORDLINE
  *-- Get index expression of the current index
  lcNdxExp   = SYS(14,VAL(SYS(21)))
 
  *-- Position record pointer on the incomplete session
  *-- situaion. This action should set all record pointers
  *-- since all files are related.
  IF SEEK('O' + laData[1], 'ORDHDR')
	SELECT ORDHDR
	SCATTER FIELDS &lcScFields MEMO TO laData 
  ENDIF
  =lfDefVars()
  =lfOrdBrow()
  SHOW GETS
  =lfwOrdBrow()  
  
  *-- Restore current object
  IF !EMPTY(lcCurObj)
    _CUROBJ   = OBJNUM(&lcCurObj)
    DO lpSavscr
  ENDIF
*-- Else, if it is a new session : 
ELSE
  IF llCreateTmp
    llCreateTmp = .F.
    *-- Create temporary files
    =lfCrtUnCmp()
  
    *-- Set necessary relations between files
    *-- Browse
    =lfSetRelations()
    *-- Prepare variables
    SELECT ORDLINE
  
    *B602852,1 Default order for Order Lines is by LineNo
    *llOrdLStr   = .T.
    *SET ORDER TO TAG ORDLINST 
	llOrdLStr   = .F.
    SET ORDER TO TAG ORDLINE
    *B602852,1 end
    
    *-- Get the index expression
    lcNdxExp   = SYS(14,VAL(SYS(21)))
 
    *-- Prepare details screen blank variables
    SELECT (lc_TmpOrdL)
    SCATTER MEMVAR MEMO BLANK
      
    SELECT ORDHDR
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK
    GO BOTTOM
    SKIP
    =lfDefVars()
    =lfOrdBrow()
  
    *C123847,1  TMI [Start] Update laData array with the passed order as a parameter
    IF ASCAN(laEvntTrig,PADR("ALDIROP",10)) <> 0
      = gfDoTriger("ALORDAL",PADR("ALDIROP",10))
    ENDIF  
    *C123847,1  TMI [End  ] 
  
  
  ENDIF  
ENDIF

RETURN llFoundUnS

*!*************************************************************
*! Name      : lfCrtUnCmp
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Validate New style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: A picking ticket number
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfRelPktkt(ORDLINE.PikTkt)
*!*************************************************************
FUNCTION lfCrtUnCmp
PRIVATE lnCurAlias, laIndeces

lnCurAlias = SELECT(0)

*-- Create temporaroy released lines file
SELECT PIKLINE
=AFIELDS(laFileStru)
*-- store indeces expression in array such that,
*-- there is one row for each index, stored as :
*-- index expression in the first column, and tag name in the other.
DECLARE laIndeces[2,2]
laIndeces[1,1] = 'ORDER  + PikTkt'
laIndeces[1,2] = 'PIKORD'
laIndeces[2,1] = 'ORDER + STR(LINENO, 6)'
laIndeces[2,2] = 'ORDLINE'
=gfCrtTmp(lc_TmpPkLn, @laFileStru, @laIndeces) 
  
*-- Create temporaroy STYDYE file
SELECT STYDYE
=AFIELDS(laFileStru)
*B802829,1 WAB - CREate temp filr to browse dyelot
*B802829,1 WAB - START
=gfCrtTmp(lcStyDTemp, @laFileStru, [Style + cWareCode + Dyelot], lcStyDTemp) 
IF USED(lcStyDTemp)
  lcStytag = ORDER()  
  USE IN (lcStyDTemp)
ENDIF
=gfOpenFile(gcWorkDir+lcStyDTemp,lcStyDTemp, "EX")
*B802829,1 WAB - END
=gfCrtTmp(lc_TmpStyD, @laFileStru, [Style + cWareCode + Dyelot], 'STYDYE') 

*-- Create temporaroy ORDLINE file
SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
*E301340,1 WAB - Add one element to the array for add filed to hold the shaving reason
*E301340,1 WAB - START
*DIMENSION laFileStru[lnFileStru + 7, 4]
DIMENSION laFileStru[lnFileStru + 8, 4]
*E301340,1 WAB - END
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'lForceAlc'
laFileStru[lnFileStru ,2] = 'L'
laFileStru[lnFileStru ,3] = 1
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cStatus'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 1
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nForcdQty'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru, 1] = 'lVldWare'
laFileStru[lnFileStru, 2] = 'L'
laFileStru[lnFileStru, 3] = 1
laFileStru[lnFileStru, 4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru, 1] = 'lVldDye'
laFileStru[lnFileStru, 2] = 'L'
laFileStru[lnFileStru, 3] = 1
laFileStru[lnFileStru, 4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru, 1] = 'nSteps'
laFileStru[lnFileStru, 2] = 'N'
laFileStru[lnFileStru, 3] = 2
laFileStru[lnFileStru, 4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru, 1] = 'lAllocated'
laFileStru[lnFileStru, 2] = 'L'
laFileStru[lnFileStru, 3] = 1
laFileStru[lnFileStru, 4] = 0
*E301340,1 WAB - fill the elemnt with the field data( add filed to hold the shaving reason)
*E301340,1 WAB - START
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru, 1] = 'cReason'
laFileStru[lnFileStru, 2] = 'C'
laFileStru[lnFileStru, 3] = 6
laFileStru[lnFileStru, 4] = 0
*E301340,1 WAB - END

*-- Prepare indeces array for temporary order line file
DECLARE laIndeces[2,2]
laIndeces[1,1] = 'cOrdType + ORDER  + STORE + CWARECODE + STYLE'
laIndeces[1,2] = 'ORDLINST'
laIndeces[2,1] = 'cOrdType + ORDER  + STR(LineNo,6)'
laIndeces[2,2] = 'ORDLINE'
=gfCrtTmp(lc_TmpOrdL, @laFileStru, @laIndeces) 

IF ASCAN(laEvntTrig,PADR("CRTSTORE",10)) <> 0
  = gfDoTriger("ALORDAL",PADR("CRTSTORE",10))
ENDIF  

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfSetRelations
*! Developer : Renee
*! Date      : 07/01/1998
*! Purpose   : Set relations between master files, and temporary 
*! 			   files
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: A picking ticket number
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfRelPktkt(ORDLINE.PikTkt)
*!*************************************************************
FUNCTION lfSetRelations
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

SET ORDER TO TAG ORDLINE IN (lc_TmpPkLn)
SET ORDER TO TAG ORDLINE IN (lc_TmpOrdL)
SET ORDER TO TAG STYDYE  IN (lc_TmpStyD)

*IF !EMPTY(SET('RELATION')) check for relations already existing
*-- Set relation between ORDLINE (parent)
*-- and STYLE, STYDYE, (lc_TmpStyD), (lc_TmpOrdL), (lc_TmpPkLn), and PIKTKT (children)
SELECT ORDLINE
SET RELATION TO IIF(EOF(lc_TmpOrdL), Style, &lc_TmpOrdL..Style) INTO STYLE ADDITIVE
SET RELATION TO cOrdType + Order + STR(LineNo,6) INTO (lc_TmpOrdL) ADDITIVE
SET RELATION TO IIF(EOF(lc_TmpOrdL), Style+cWareCode+Dyelot,;
				    &lc_TmpOrdL..Style + &lc_TmpOrdL..cWareCode + &lc_TmpOrdL..Dyelot) ;
                	INTO STYDYE ADDITIVE
SET RELATION TO IIF(EOF(lc_TmpOrdL), Style+cWareCode+Dyelot,;
				    &lc_TmpOrdL..Style + &lc_TmpOrdL..cWareCode + &lc_TmpOrdL..Dyelot) ;
				    INTO (lc_TmpStyD) ADDITIVE

SET RELATION TO Order + PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO ORDER + STR(LINENO, 6) INTO (lc_TmpPkLn) ADDITIVE

*-- Set relation between STYLE (parent) and SCALE (child)
SELECT STYLE
SET RELATION TO 'S'+Scale INTO SCALE

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfAdUnCmSR
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Create an 'O'pen incomplete session record
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfAdUnCmSR()
*!*************************************************************
FUNCTION lfAdUnCmSR
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
SELECT UNCMSESS

*B606625,1 NAD (Start)
*IF SEEK('I')
lnReprocesX = SET('REPROCESS')
SET REPROCESS TO 1

*B607290,1 KHM 07/28/2003 (Begin) Change the seek expression.
*IF SEEK('I') AND RLOCK()
IF SEEK('I'+PADR(lcProgID,10)+PADR(gcUser_id,10)) AND RLOCK()
*B607290,1 KHM 07/28/2003 (End)

*B606625,1 NAD (END)
  BLANK
  REPLACE Status     WITH 'O',;
          cUTranType WITH lcProgID,;
          cUserID    WITH gcUser_id,;
          cSession   WITH lcSession,;
          cProgram   WITH lcProgID,;
          cCurrScr   WITH lcProgID,;
          dTranDate  WITH gdSysDate,;
          cTranTime  WITH TIME()
ELSE
  INSERT INTO UNCMSESS ;
       (Status  , cUTranType, cUserId  , cSession ,;
        cProgram, cCurrScr  , dTranDate, cTranTime);
     VALUES ;
       ('O'     , lcProgID  , gcUser_id, lcSession,;
        lcProgID, lcProgID  , gdSysDate, TIME())
ENDIF
*B606625,1 NAD (Start)
SET REPROCESS TO lnReprocesX
*B606625,1 NAD (End)
*-- Store the Incomplete session record number
lnUnCmSeRc = RECNO()
=gfSavSess(lcProgID, EVAL(lcFiles), @laVars, lcSession)
=RLOCK('UNCMSESS')
llCUpDate  = .T.
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfUpdUnCmS
*! Developer : Renee
*! Date      : 09/10/97
*! Purpose   : Update the current object 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfUpdUnCmS()
*!*************************************************************
FUNCTION lfUpdUnCmS
PRIVATE lnCurAlias
IF lnUnCmSeRc <> 0
  lnCurAlias  = SELECT(0)
  SELECT UnCmSess
  GOTO lnUnCmSeRc
  REPLACE cCurrObj WITH VARREAD()
  SELECT(lnCurAlias)          
ENDIF

*!*************************************************************
*! Name      : lfClsUnCmS
*! Developer : Renee
*! Date      : 09/10/97
*! Purpose   : Update the current object and the status of the session
*!*************************************************************
*! Called From : 
*!             Procedures : LPSAVSCR, LPCLSSCR
*!*************************************************************
*! Passed Parameters  : Status ('C', or 'I')
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfClsUnCmS('C')
*!*************************************************************
FUNCTION lfClsUnCmS
PARAMETERS lcStatus
PRIVATE lnCurAlias

IF RECCOUNT(lc_TmpOrdL) > 0
  *-- Clear temp files of their contents
  SELECT (lc_TmpOrdL)
  DELETE ALL
  SELECT (lc_TmpStyD)
  DELETE ALL
  SELECT (lc_TmpPkLn)
  DELETE ALL
ENDIF

IF lnUnCmSeRc <> 0
  lnCurAlias  = SELECT(0)
  SELECT UnCmSess
  GOTO lnUnCmSeRc
  REPLACE cCurrObj WITH SPACE(0),;
          Status   WITH lcStatus
  *-- Reset record number
  lnUnCmSeRc = 0
  UNLOCK
  SELECT(lnCurAlias)          
ENDIF

*!*************************************************************
*! Name      : lfDefVars
*! Developer : Renee
*! Date      : 09/10/97
*! Purpose   : Define variables for browse, etc.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfDefVars()
*!*************************************************************
FUNCTION lfDefVars

lcStkAlias   = IIF(EOF(lc_TmpStyD), 'STYDYE', lc_TmpStyD)


*C200333,1 MHM 05/01/2002 Modify Browse to work with the new modification of GMA Project [Start]
*-- AS per last modification we will do the folloing modification
*-- we will call any trigger from main program only so we will comment it
*C102520,1 BWA 08/01/2001 Add the Pack_id field to the browsing.[START]
*IF llGma
*  
*  *lcOrderBr    = [cMarker= IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+; 
*	    		 [cPicked=IIF(EVAL(lc_TmpOrdL + '.Picked') .OR. EOF(lc_TmpOrdL) .AND. ORDLINE.Picked,]+;
*			            ['>>','  '):R:W=.F.:H='  ':2,]+;
*			     [STORE:H='Store':8,]+;
*  			     [PACK_ID:H='Pack_Id':20,]+;
*			     [STYLE=IIF(EOF(lc_TmpOrdL), Style, &lc_TmpOrdL..Style):H=lcStyHdr:R,]+;
*			     [GROUP:H='G':P='!':1:R,]+; 
*			      IIF(llMultWare,;
*			     [cWareCode=IIF(EOF(lc_TmpOrdL), cWareCode, &lc_TmpOrdL..cWareCode):H='Pick from':10:R,],[])+;
*			      IIF(llUseDyes,;
*			     [DYELOT=IIF(EOF(lc_TmpOrdL), Dyelot, &lc_TmpOrdL..Dyelot):H='Dyelot':10:R,],[])
*
*  lcOrderBr    = [cMarker= IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+; 
*	    		 [cPicked=IIF(EVAL(lc_TmpOrdL + '.Picked') .OR. EOF(lc_TmpOrdL) .AND. ORDLINE.Picked,]+;
*			            ['>>','  '):R:W=.F.:H='  ':2,]+;
*			     [STORE:H='Store':8,]+;
*                 [ pack_id =pack_id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :35:R,]+;
*			     [STYLE=IIF(EOF(lc_TmpOrdL), Style, &lc_TmpOrdL..Style):H=lcStyHdr:R,]+;
*			     [GROUP:H='G':P='!':1:R,]+; 
*			      IIF(llMultWare,;
*			     [cWareCode=IIF(EOF(lc_TmpOrdL), cWareCode, &lc_TmpOrdL..cWareCode):H='Pick from':10:R,],[])+;
*			      IIF(llUseDyes,;
*			     [DYELOT=IIF(EOF(lc_TmpOrdL), Dyelot, &lc_TmpOrdL..Dyelot):H='Dyelot':10:R,],[])
*ELSE
*  lcOrderBr    = [cMarker= IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+; 
*	 		     [cPicked=IIF(EVAL(lc_TmpOrdL + '.Picked') .OR. EOF(lc_TmpOrdL) .AND. ORDLINE.Picked,]+;
*			            ['>>','  '):R:W=.F.:H='  ':2,]+;
*			     [STORE:H='Store':8,]+;
*			     [STYLE=IIF(EOF(lc_TmpOrdL), Style, &lc_TmpOrdL..Style):H=lcStyHdr:R,]+;
*			     [GROUP:H='G':P='!':1:R,]+; 
*			      IIF(llMultWare,;
*			     [cWareCode=IIF(EOF(lc_TmpOrdL), cWareCode, &lc_TmpOrdL..cWareCode):H='Pick from':10:R,],[])+;
*			      IIF(llUseDyes,;
*			     [DYELOT=IIF(EOF(lc_TmpOrdL), Dyelot, &lc_TmpOrdL..Dyelot):H='Dyelot':10:R,],[])
*ENDIF
*C102520,1 BWA 08/01/2001.[END]

lcOrderBr    = [cMarker= IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+; 
               [cPicked=IIF(EVAL(lc_TmpOrdL + '.Picked') .OR. EOF(lc_TmpOrdL) .AND. ORDLINE.Picked,]+;
			   ['>>','  '):R:W=.F.:H='  ':2,]+;
			   [STORE:H='Store':8,]+;
			   [STYLE=IIF(EOF(lc_TmpOrdL), Style, &lc_TmpOrdL..Style):H=lcStyHdr:R,]+;
			   [GROUP:H='G':P='!':1:R,]+; 
			    IIF(llMultWare,;
			   [cWareCode=IIF(EOF(lc_TmpOrdL), cWareCode, &lc_TmpOrdL..cWareCode):H='Pick from':10:R,],[])+;
			    IIF(llUseDyes,;
			   [DYELOT=IIF(EOF(lc_TmpOrdL), Dyelot, &lc_TmpOrdL..Dyelot):H='Dyelot':10:R,],[])

IF ASCAN(laEvntTrig , PADR('ALBRWORD',10)) <> 0
  =gfDoTriger('ALORDAL',PADR('ALBRWORD',10))
ENDIF
*C200333,1 MHM [End]

*B603240,1 Write a "******" if allocated instead of writing nothing
*lcOrderBr2   = [TOTQTY=IIF(EOF(lc_TmpOrdL), TotQty, &lc_TmpOrdL..TotQty)]+;
                     [:H='Open':P='999999' :R,]+;
		       [OpnAmt=IIF(EOF(lc_TmpOrdL), TotQty, &lc_TmpOrdL..TotQty)*Price]+;
			         [:H='Opn Amnt.':P='99999999.99' :R,]+;
			   [TOTPIK=IIF(EOF(lc_TmpOrdL), TotPik, &lc_TmpOrdL..TotPik)]+;
		 		     [:H='Picked':P='999999' :R,]+;
		 	   [PikAmt=IIF(EOF(lc_TmpOrdL), TotPik, &lc_TmpOrdL..TotPik)*Price]+;
		    	     [:H='Pik Amnt.':P='99999999.99':R,]+;	        
			   [PIKTKT=IIF(EOF(lc_TmpOrdL) .OR. DELETED() .OR. !&lc_TmpOrdL..cStatus $ 'RU' ,PIKTKT,SPACE(6))]+;
          			 [:H='Piktkt':7:R,]+;
		       [PIKDATE=IIF(EOF(lc_TmpOrdL) .OR. DELETED() .OR. !&lc_TmpOrdL..cStatus $ 'RU' ,PIKDATE,{}):H='Pik Date':8:R]
*B802938,1 WAB - don 't change the contents of field piktkt if it is = '******' 
*lcOrderBr2   = [TOTQTY=IIF(EOF(lc_TmpOrdL), TotQty, &lc_TmpOrdL..TotQty)]+;
                     [:H='Open':P='999999' :R,]+;
		       [OpnAmt=IIF(EOF(lc_TmpOrdL), TotQty, &lc_TmpOrdL..TotQty)*Price]+;
			         [:H='Opn Amnt.':P='99999999.99' :R,]+;
			   [TOTPIK=IIF(EOF(lc_TmpOrdL), TotPik, &lc_TmpOrdL..TotPik)]+;
		 		     [:H='Picked':P='999999' :R,]+;
		 	   [PikAmt=IIF(EOF(lc_TmpOrdL), TotPik, &lc_TmpOrdL..TotPik)*Price]+;
		    	     [:H='Pik Amnt.':P='99999999.99':R,]+;	        
  		   [PIKTKT=IIF(EOF(lc_TmpOrdL) .OR. DELETED() .OR. &lc_TmpOrdL..cStatus="M",PIKTKT,]+;
			   [IIF(EVAL(lc_TmpOrdL + '.Picked') .AND. EMPTY(&lc_TmpOrdL..PikTkt), "******",SPACE(6)))]+;
          	   [:H='Piktkt':7:R,]+;
		       [PIKDATE=IIF(EOF(lc_TmpOrdL) .OR. DELETED() .OR. &lc_TmpOrdL..cStatus="M" ,PIKDATE,]+;
		       [IIF(EVAL(lc_TmpOrdL + '.Picked') .AND. EMPTY(&lc_TmpOrdL..PikTkt), gdSysDate,{})):H='Pik Date':8:R]

*B603506,1 ABD Increase amount sizes in Browse fields to fit currencies.
*B603506,1 ABD [BookAmt,ShipAmt,CancelAmt,Openamt] [Begin] 
*lcOrderBr2   = [TOTQTY=IIF(EOF(lc_TmpOrdL), TotQty, &lc_TmpOrdL..TotQty)]+;
                     [:H='Open':P='999999' :R,]+;
		       [OpnAmt=IIF(EOF(lc_TmpOrdL), TotQty, &lc_TmpOrdL..TotQty)*Price]+;
			         [:H='Opn Amnt.':P= '99999999.99'  :R,]+;
			   [TOTPIK=IIF(EOF(lc_TmpOrdL), TotPik, &lc_TmpOrdL..TotPik)]+;
		 		     [:H='Picked':P='999999' :R,]+;
		 	   [PikAmt=IIF(EOF(lc_TmpOrdL), TotPik, &lc_TmpOrdL..TotPik)*Price]+;
		    	     [:H='Pik Amnt.':P='99999999.99':R,]+;	        
			   [PIKTKT=IIF(EOF(lc_TmpOrdL) .OR. DELETED() .OR. &lc_TmpOrdL..cStatus="M",PIKTKT,]+;
			   [IIF(EVAL(lc_TmpOrdL + '.Picked') .AND. (EMPTY(&lc_TmpOrdL..PikTkt) .OR. ]+;
			   [&lc_TmpOrdL..PikTkt = "******"),"******",SPACE(6)))]+;
          	   [:H='Piktkt':7:R,]+;
		       [PIKDATE=IIF(EOF(lc_TmpOrdL) .OR. DELETED() .OR. &lc_TmpOrdL..cStatus="M" ,PIKDATE,]+;
		       [IIF(EVAL(lc_TmpOrdL + '.Picked') .AND. EMPTY(&lc_TmpOrdL..PikTkt), gdSysDate,{})):H='Pik Date':8:R]

*B803406,1 HBG 07/30/2000 add another variable to hold the field of the browes to change
*B803406,1                 the position of the calculated field [Begin]
*lcOrderBr2   = [TOTQTY=IIF(EOF(lc_TmpOrdL), TotQty, &lc_TmpOrdL..TotQty)]+;
                     [:H='Open':P='999999' :R,]+;
		       [OpnAmt=IIF(EOF(lc_TmpOrdL), TotQty, &lc_TmpOrdL..TotQty)*Price]+;
			         [:H='Opn Amnt.':P= '9999999999.99' :R,]+;
			   [TOTPIK=IIF(EOF(lc_TmpOrdL), TotPik, &lc_TmpOrdL..TotPik)]+;
		 		     [:H='Picked':P='999999' :R,]+;
		 	   [PikAmt=IIF(EOF(lc_TmpOrdL), TotPik, &lc_TmpOrdL..TotPik)*Price]+;
		    	     [:H='Pik Amnt.':P= '9999999999.99' :R,]+;	        
			   [PIKTKT=IIF(EOF(lc_TmpOrdL) .OR. DELETED() .OR. &lc_TmpOrdL..cStatus="M",PIKTKT,]+;
			   [IIF(EVAL(lc_TmpOrdL + '.Picked') .AND. (EMPTY(&lc_TmpOrdL..PikTkt) .OR. ]+;
			   [&lc_TmpOrdL..PikTkt = "******"),"******",SPACE(6)))]+;
          	   [:H='Piktkt':7:R,]+;
		       [PIKDATE=IIF(EOF(lc_TmpOrdL) .OR. DELETED() .OR. &lc_TmpOrdL..cStatus="M" ,PIKDATE,]+;
		       [IIF(EVAL(lc_TmpOrdL + '.Picked') .AND. EMPTY(&lc_TmpOrdL..PikTkt), gdSysDate,{})):H='Pik Date':8:R]
		       
lcOrderBr2   = [TOTQTY=IIF(EOF(lc_TmpOrdL), TotQty, &lc_TmpOrdL..TotQty)]+;
                     [:H='Open':P='999999' :R,]+;
		       [OpnAmt=IIF(EOF(lc_TmpOrdL), TotQty, &lc_TmpOrdL..TotQty)*Price]+;
			         [:H='Opn Amnt.':P= '9999999999.99' :R,]+;
			   [TOTPIK=IIF(EOF(lc_TmpOrdL), TotPik, &lc_TmpOrdL..TotPik)]+;
		 		     [:H='Picked':P='999999' :R,]+;
		 	   [PikAmt=IIF(EOF(lc_TmpOrdL), TotPik, &lc_TmpOrdL..TotPik)*Price]+;
		    	     [:H='Pik Amnt.':P= '9999999999.99' :R]		       

*B603826,1 (Begin) Remark the following line and remove the condition of &lc_TmpOrdL..cStatus="M"
*lcOrderBr3  =  [PIKTKT=IIF(llGenPkTk AND (EOF(lc_TmpOrdL) .OR. DELETED() .OR. &lc_TmpOrdL..cStatus="M"),PIKTKT,]+;
			   [IIF(EVAL(lc_TmpOrdL + '.Picked') .AND. (EMPTY(&lc_TmpOrdL..PikTkt) .OR. ]+;
			   [&lc_TmpOrdL..PikTkt = "******"),"******",SPACE(6)))]+;
          	   [:H='Piktkt':7:R,]+;
		       [PIKDATE=IIF(EOF(lc_TmpOrdL) .OR. DELETED() .OR. &lc_TmpOrdL..cStatus="M" ,PIKDATE,]+;
		       [IIF(EVAL(lc_TmpOrdL + '.Picked') .AND. EMPTY(&lc_TmpOrdL..PikTkt), gdSysDate,{})):H='Pik Date':8:R]

lcOrderBr3  =  [PIKTKT=IIF(EOF(lc_TmpOrdL) .OR. DELETED() ,PIKTKT,]+;
			   [IIF(EVAL(lc_TmpOrdL + '.Picked') .AND. (EMPTY(&lc_TmpOrdL..PikTkt) .OR. ]+;
			   [&lc_TmpOrdL..PikTkt = "******"),"******",SPACE(6)))]+;
          	   [:H='Piktkt':7:R,]+;
		       [PIKDATE=IIF(EOF(lc_TmpOrdL) .OR. DELETED() ,PIKDATE,]+;
		       [IIF(EVAL(lc_TmpOrdL + '.Picked') .AND. EMPTY(&lc_TmpOrdL..PikTkt), gdSysDate,{})):H='Pik Date':8:R]
*B603826,1 (End)		       

*B803406,1 [End]

*B603506,1 ABD [End]
*B802938,1 WAB - END
*B603240,1 end

*-- end of lfDefVars.

*!*************************************************************
*! Name      : lfForceOpt
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/22/99
*! Purpose   : Force allocation Check box.
*!*************************************************************
*!E301225,1 Allow Force allocation option.
*!
FUNCTION lfForceOpt
PRIVATE llAlwForce
llAlwForce = .T.
IF lcAlwForce <> "Y"
  *-- No Force allocation done.
  IF lcAlwForce = "N"
    llAlwForce = .F.

  ELSE  && User Prev.

    *-- Call user defined process.  
    llAlwForce = gfUserPriv('AL','ALORDAL','FORCING')

  ENDIF
ENDIF
RETURN llAlwForce
*-- end of lfForceOpt.

*!*************************************************************
*! Name      : lfChkAprov
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/22/99
*! Purpose   : Check Order Approve Amount.
*!*************************************************************
*!E301310,1 Check Approved Amount.
*!
FUNCTION lfChkAprov
PARAMETERS lcHdrKey
PRIVATE lcLinesOrd , lcLinesKey , lnCurrAlis , lcHdrKey , lnAllocAmt
lnAllocAmt = 0
*-- Save used Environment [Begin]
lnCurrAlis = SELECT(0)
SELECT ORDLINE
lcLinesOrd = ORDER()
*-- if no Active index save Record Number.
IF EMPTY(lcLinesOrd)
  lcLinesKey = RECNO()
ELSE  && else if there is active index save current key.
  lcLinesKey = EVALUATE(KEY())
ENDIF
*-- Save used Environment [End  ]

*-- Check processing [Begin]
SET ORDER TO
SUM totpik*price TO lnAllocAmt FOR cOrdType + Order =  lcHdrKey

*-- Restore Old Environment [Begin]
SET ORDER TO &lcLinesOrd  && Restore Old Order.

*-- Restore Record pointer. [Begin]
IF EMPTY(lcLinesOrd)
  IF BETWEEN(lcLinesKey,1,RECCOUNT())
    GO lcLinesKey
  ENDIF
ELSE
  =SEEK(lcLinesKey)
ENDIF
*-- Restore Record pointer. [End  ]

SELECT (lnCurrAlis)
*-- Restore Old Environment [End  ]
*B803358,1 AMH 06/21/2000 Checking Order Approved Amount (Start)
*RETURN (OrdHdr.ApprAmt - (OrdHdr.ShipAmt + lnAllocAmt))
RETURN (OrdHdr.ApprAmt - lnAllocAmt)
*B803358,1 AMH 06/21/2000 Checking Order Approved Amount (End)
*-- end of lfChkAprov.

*!*************************************************************
*! Name      : lfAprvLine
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/24/99
*! Purpose   : Current line pass approved check
*!*************************************************************
*!E301310,1 Check Approved Amount.
*!
FUNCTION lfAprvLine
PARAMETERS lnDiferent

IF lnDiferent = 0
  RETURN .T.
ENDIF

PRIVATE llCanAloct
llCanAloct = .T.
lnDiferent = lnDiferent * m.Price

DO CASE
  CASE (lnChkAprov - lnDiferent) < 0
    *Message 44080 : The approved amount has covered some order lines only .
    *                Cannot allocate the rest.
    =gfModalGen("TRM44080B00000" , "DIALOG")
    llCanAloct = .F.
    llFailAprv = .T.
  CASE (lnChkAprov - lnDiferent) >= 0
    lnChkAprov = lnChkAprov - lnDiferent
ENDCASE

RETURN llCanAloct
*-- end of lfAprvLine.

*!*************************************************************
*! Name      : lfGetAlo
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/24/99
*! Purpose   : Get Quantity to allocate.
*!*************************************************************
*!E301310,1 Check Approved Amount.
*!
FUNCTION lfGetAlo
*E500304,1 WAB - Calculate the avialble qty (stk-alo) by calling function
*E500304,1 WAB - lftotavlbl() to get the available qty
*E500304,1 WAB - START
*RETURN  ROUND(MIN(m.Qty1, MAX((Stk1-Alo1)*laAllocPer[1], 0)),0) +;
        ROUND(MIN(m.Qty2, MAX((Stk2-Alo2)*laAllocPer[2], 0)),0) +;
        ROUND(MIN(m.Qty3, MAX((Stk3-Alo3)*laAllocPer[3], 0)),0) +;
        ROUND(MIN(m.Qty4, MAX((Stk4-Alo4)*laAllocPer[4], 0)),0) +;
        ROUND(MIN(m.Qty5, MAX((Stk5-Alo5)*laAllocPer[5], 0)),0) +;
        ROUND(MIN(m.Qty6, MAX((Stk6-Alo6)*laAllocPer[6], 0)),0) +;
        ROUND(MIN(m.Qty7, MAX((Stk7-Alo7)*laAllocPer[7], 0)),0) +;
        ROUND(MIN(m.Qty8, MAX((Stk8-Alo8)*laAllocPer[8], 0)),0)
PRIVATE lnTotal
lnTotal = ROUND(MIN(m.Qty1, MAX(lfTotAvlbl('1')*laAllocPer[1], 0)),0) +;
          ROUND(MIN(m.Qty2, MAX(lfTotAvlbl('2')*laAllocPer[2], 0)),0) +;
          ROUND(MIN(m.Qty3, MAX(lfTotAvlbl('3')*laAllocPer[3], 0)),0) +;
          ROUND(MIN(m.Qty4, MAX(lfTotAvlbl('4')*laAllocPer[4], 0)),0) +;
          ROUND(MIN(m.Qty5, MAX(lfTotAvlbl('5')*laAllocPer[5], 0)),0) +;
          ROUND(MIN(m.Qty6, MAX(lfTotAvlbl('6')*laAllocPer[6], 0)),0) +;
          ROUND(MIN(m.Qty7, MAX(lfTotAvlbl('7')*laAllocPer[7], 0)),0) +;
          ROUND(MIN(m.Qty8, MAX(lfTotAvlbl('8')*laAllocPer[8], 0)),0)
RETURN(lnTotal)
*E500304,1 WAB - END
*-- end of lfGetAlo.
*!**************************************************************************
*! Func. Name: lfShowShav()
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/28/1999
*! Purpose   : Call Shave order Screen
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfShowShav()
*E301340,1 WAB 
*!**************************************************************************
FUNCTION lfShowShav
*-- save fileds value before calling sahve order screen
lnOldShv = lnOrdShv
lnOldRsn = lnReson
lcOldRsn = lcReason
llSaveDef = .F.
*--OK butt is disabled
lcDispFld = 'DISABLE'
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\ALORDSHV.SPX")
POP KEY

*!**************************************************************************
*! Func. Name: lfvOrdShv()
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/28/1999
*! Purpose   : valid Shave Unalocated Qty popup
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvOrdShv()
*E301340,1 WAB 
*!**************************************************************************
FUNCTION lfvOrdShv
*--enable ok butt. if there is any changed in fields values
lcDispFld = IIF(lnOrdShv <> lnOldShv ,'ENABLE',IIF(lnOldRsn = lnReson AND ;
                !llSaveDef,'DISABLE','ENABLE'))
*-- if automatic shaving is selected -->ok butt, still disabled until select
*-- default reason
IF lnOrdShv = 1 AND lnReson = 1
  lcDispFld = 'DISABLE'
ENDIF
SHOW GET pbAmdOk &lcDispFld

*!**************************************************************************
*! Func. Name: lfvShvRsn()
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/28/1999
*! Purpose   : valid default shaving reason popup
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvShvRsn()
*E301340,1 WAB 
*!**************************************************************************
FUNCTION lfvShvRsn
*--get reason code
lcReason = laShReson[lnReson , 2]

*--enable ok butt. if there is any changed in fields values
lcDispFld = IIF( lnOldRsn <> lnReson,'ENABLE',IIF( lnOrdShv = lnOldShv AND ;
                 !llSaveDef,'DISABLE','ENABLE'))

*-- if automatic shaving is selected -->ok butt, still disabled until select
*-- default reason
IF lnOrdShv = 1 AND lnReson = 1
  lcDispFld = 'DISABLE'
ENDIF
SHOW GET pbAmdOk &lcDispFld

*!**************************************************************************
*! Func. Name: lfvSavDef()
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/28/1999
*! Purpose   : valid set as default check box field
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvSavDef()
*E301340,1 WAB 
*!**************************************************************************
FUNCTION lfvSavDef
*--enable ok butt. if there is any changed in fields values
lcDispFld = IIF( llSaveDef ,'ENABLE',IIF(lnOrdshv = lnOldShv And ;
                 lnOldRsn = lnReson,'DISABLE','ENABLE'))
*-- if automatic shaving is selected -->ok butt, still disabled until select
*-- default reason
IF lnOrdShv = 1 AND lnReson = 1
  lcDispFld = 'DISABLE'
ENDIF
SHOW GET pbAmdOk &lcDispFld

*!**************************************************************************
*! Func. Name: lfvOkShv()
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/28/1999
*! Purpose   : valid Ok butt. in sahving order screen
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfvOkShv()
*E301340,1 WAB 
*!**************************************************************************
FUNCTION lfvOkShv
*-- is user select set as default 
IF llSaveDef
  *-- 1- save order shaving setting to lasavmem array
  *-- 2- delete the mem file if it is exist
  *-- 3- save the arrray to mem 
  laSavMem[1] = lnOrdShv
  laSavMem[2] = lnReson
  IF FILE(gcDataDir+'ORDSHAV.MEM')
    ERASE(gcDataDir+'ORDSHAV.MEM')
  ENDIF
  SAVE TO (gcDataDir+'ORDSHAV.MEM') ALL LIKE laSavMem
ENDIF
RETURN

*!**************************************************************************
*! Func. Name: lfvCanShv()
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/28/1999
*! Purpose   : valid Cancel butt. in sahving order screen
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfvCanShv()
*E301340,1 WAB 
*!**************************************************************************
FUNCTION lfvCanShv
*--return old value to fileds 
lnOrdShv = lnOldShv
lnReson  = lnOldRsn
lcReason = lcOldRsn 

*!**************************************************************************
*! Func. Name: lfvSelShv()
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/28/1999
*! Purpose   : valid sahving order reason fields
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfvSelShv()
*E301340,1 WAB 
*!**************************************************************************
FUNCTION lfvSelShv
  lcReason = laShReson[lnSelRsn , 2]
  *-- call function to addline to temp file fi ist not added b4
  =lfAddLine()
  IF lnReson = 1
    REPLACE &lc_TmpOrdL..cReason WITH  ' '
  ELSE
    REPLACE &lc_TmpOrdL..cReason WITH lcReason
  ENDIF

*!**************************************************************************
*! Func. Name: lfUpdCanl()
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/28/1999
*! Purpose   : 1- Cancelling the shaving qty for order 
*!             2- Filling the ORDCANLN File with shaving qty
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfUpdCanl()
*E301340,1 WAB 
*!**************************************************************************
FUNCTION lfUpdCanl  
*--open needed files if first time calling this function 
IF !llOpenFile
  =gfOpenFile(gcDataDir+"CUTTKTH", "CUTTKTH", "SH")
  =gfOpenFile(gcDataDir+"CUTTKTL", "Cutlin", "SH")
  =gfOpenFile(gcDataDir+"CUTPICK", "CUTORD", "SH")
  =gfOpenFile(gcDataDir+"ORDCANLN", "", "SH")    
  =gfOpenFile(gcDataDir+"POSHDR", "POSHDR", "SH")
  =gfOpenFile(gcDataDir+"POSLN", "POSLN", "SH")  
  IF !USED('icStyHst')		&& may be file is already opened
    =gfOpenFile(gcDataDir+"ICSTYHST", "STYHST", "SH")
  ENDIF
  IF !USED('ARCUSHST')		&& may be file is already opened
    =gfOpenFile(gcDataDir+"ARCUSHST", "ACTHST", "SH")
  ENDIF
  *-- store .t. to open file flag to determin that files is already opened
  llOpenFile = .T.
ENDIF
lnCanc = 0  			&& variable hold total qty shaved
=lfChkAlloc()			&& Check SO Allocation Status
SELECT CUTPICK
lcCutPkOrd = ORDER()
SET ORDER TO TAG CUTORD
IF SEEK('1' + M.ORDER ,'CutPick') .OR. SEEK('2' + M.ORDER ,'CutPick')
  *--Releas the Allocation in CT/PO
  =lfRelAllOc()
ENDIF
SELECT CUTPICK
SET ORDER TO TAG (lcCutPkOrd)
*-- Collect Qty1 ==> Qty8 for each line For updating Header file
For lnCount = 1 TO 8
  lcCount = Str(lnCount,1)
  lnCanc =  lnCanc + IIF(m.qty&lcCount>m.pik&lcCount,m.qty&lcCount-m.pik&lcCount,0) 
ENDFOR
*-update order headr open qty,open amount,cancel qty,cancel amount,cancelled date
SELECT ORDHDR

=SEEK('O' + m.ORDER , 'ORDHDR')
=RLOCK()
REPLACE OPEN       WITH (OPEN   - lnCanc) ;
        CANCEL     WITH (CANCEL + lnCanc) ;
        CANCELAMT  WITH (CANCELAMT + (lnCanc * M.PRICE));
        OPENAMT    WITH (OPENAMT   - (lnCanc * M.PRICE));
        CANCELLED  WITH gdSysDate                         
UNLOCK
*--update style history & customer history
IF CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod ','',.T.)
  *-- Get the year/period of ORDHDR entered date so as to update
  SELECT ICSTYHST
  IF SEEK(m.Style + lcGlYear)
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - lnCanc,;
          	nOrdQty            WITH nOrdQty            - lnCanc,;
           	nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - (lnCanc * M.PRICE),;
		    nOrdAmt            WITH nOrdAmt            - (lnCanc * M.PRICE)
    UNLOCK
  ENDIF     && IF SEEK(m.Style 
  SELECT ARCUSHST
  IF SEEK(m.Account + lcGlYear)
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - lnCanc,;
            nOrdQty            WITH nOrdQty            - lnCanc,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - (lnCanc * M.PRICE),;
            nOrdAmt            WITH nOrdAmt            - (lnCanc * M.PRICE)
    UNLOCK
  ENDIF
ENDIF                                  



*--Filling the ORDCANLN File with shaving qty
SELECT  ORDCANLN
APPEND  BLANK
=RLOCK()
REPLACE CORDTYPE   WITH 'O'                                  ;                                  
        ORDER      WITH M.ORDER                              ;
        LINENO     WITH M.LINENO                             ;
        QTY1       WITH IIF(m.qty1 > m.pik1,m.qty1-m.pik1,0) ;
        QTY2       WITH IIF(m.qty2 > m.pik2,m.qty2-m.pik2,0) ;
        QTY3       WITH IIF(m.qty3 > m.pik3,m.qty3-m.pik3,0) ;
        QTY4       WITH IIF(m.qty4 > m.pik4,m.qty4-m.pik4,0) ;
        QTY5       WITH IIF(m.qty5 > m.pik5,m.qty5-m.pik5,0) ;
        QTY6       WITH IIF(m.qty6 > m.pik6,m.qty6-m.pik6,0) ;
        QTY7       WITH IIF(m.qty7 > m.pik7,m.qty7-m.pik7,0) ;
        QTY8       WITH IIF(m.qty8 > m.pik8,m.qty8-m.pik8,0) ;
        TOTQTY     WITH lnCanc ;
        CANCELLED  WITH gdSysDate                         ;
        CCANCRESON WITH &lc_TmpOrdL..cReason
*B606448,1 (Begin) Update Style field
REPLACE STYLE WITH m.Style        
*B606448,1 (End)
*-- add the audit information
=gfAdd_Info()
UNLOCK
*-update order line qty with (the order line qty - the sahving qty )
SELECT ORDLINE

*C102520,1 BWA 08/01/2001 Use the index of the Pack_id in case GMA.[START]
*=SEEK('O'+m.ORDER+STR(m.LINENO,6))
IF llGma
  PRIVATE lcOrderPk
  lcOrderPk = ORDER()
  SET ORDER TO TAG ORDLINE
  =SEEK('O'+m.ORDER+STR(m.LINENO,6))
  SET ORDER TO TAG &lcOrderPk
ELSE
  =SEEK('O'+m.ORDER+STR(m.LINENO,6))
ENDIF
*C102520,1 BWA 08/01/2001.[END]

=RLOCK()
REPLACE QTY1   WITH (QTY1 - IIF(m.qty1 > m.pik1,m.qty1-m.pik1,0));
        QTY2   WITH (QTY2 - IIF(m.qty2 > m.pik2,m.qty2-m.pik2,0));
        QTY3   WITH (QTY3 - IIF(m.qty3 > m.pik3,m.qty3-m.pik3,0));
        QTY4   WITH (QTY4 - IIF(m.qty4 > m.pik4,m.qty4-m.pik4,0));
        QTY5   WITH (QTY5 - IIF(m.qty5 > m.pik5,m.qty5-m.pik5,0));
        QTY6   WITH (QTY6 - IIF(m.qty6 > m.pik6,m.qty6-m.pik6,0));
        QTY7   WITH (QTY7 - IIF(m.qty7 > m.pik7,m.qty7-m.pik7,0));
        QTY8   WITH (QTY8 - IIF(m.qty8 > m.pik8,m.qty8-m.pik8,0));
        TOTQTY WITH (TOTQTY - lnCanc ) 
         
UNLOCK
*-update Style order qty with (the order qty - the sahving qty )
SELECT STYLE
SET ORDER TO TAG STYLE
SEEK m.STYLE
=RLOCK()
REPLACE ORD1   WITH (ORD1 - IIF(m.qty1 > m.pik1,m.qty1-m.pik1,0));
        ORD2   WITH (ORD2 - IIF(m.qty2 > m.pik2,m.qty2-m.pik2,0));
        ORD3   WITH (ORD3 - IIF(m.qty3 > m.pik3,m.qty3-m.pik3,0));
        ORD4   WITH (ORD4 - IIF(m.qty4 > m.pik4,m.qty4-m.pik4,0));
        ORD5   WITH (ORD5 - IIF(m.qty5 > m.pik5,m.qty5-m.pik5,0));
        ORD6   WITH (ORD6 - IIF(m.qty6 > m.pik6,m.qty6-m.pik6,0));
        ORD7   WITH (ORD7 - IIF(m.qty7 > m.pik7,m.qty7-m.pik7,0));
        ORD8   WITH (ORD8 - IIF(m.qty8 > m.pik8,m.qty8-m.pik8,0));
        TOTORD WITH (TOTORD -  lnCanc ) 
UNLOCK
*-update StyDye order qty with (the order qty - the sahving qty )
SELECT STYDYE
SET ORDER TO TAG STYDYE
*B804468,1 HBG 10/17/2001 Seek for the line of the current Warehouse [Begin]
*SEEK m.STYLE
*B605083,1 TMI [Start] Locate the correct line in stydye with a correct warehous
*SEEK m.STYLE + laWareHous[puPickWare, 2]
SEEK m.STYLE + &lc_TmpOrdL..cWareCode
*B605083,1 TMI [End  ] 
*B804468,1 [End]
=RLOCK()
REPLACE ORD1   WITH (ORD1 - IIF(m.qty1 > m.pik1,m.qty1-m.pik1,0));
        ORD2   WITH (ORD2 - IIF(m.qty2 > m.pik2,m.qty2-m.pik2,0));
        ORD3   WITH (ORD3 - IIF(m.qty3 > m.pik3,m.qty3-m.pik3,0));
        ORD4   WITH (ORD4 - IIF(m.qty4 > m.pik4,m.qty4-m.pik4,0));
        ORD5   WITH (ORD5 - IIF(m.qty5 > m.pik5,m.qty5-m.pik5,0));
        ORD6   WITH (ORD6 - IIF(m.qty6 > m.pik6,m.qty6-m.pik6,0));
        ORD7   WITH (ORD7 - IIF(m.qty7 > m.pik7,m.qty7-m.pik7,0));
        ORD8   WITH (ORD8 - IIF(m.qty8 > m.pik8,m.qty8-m.pik8,0));
        TOTORD WITH (TOTORD -  lnCanc ) 
UNLOCK

*!**************************************************************************
*! Name      : lfChkAlloc
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/28/1999
*! Purpose   : Function to Check the allocation status
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfChkAlloc()
*E301340,1 WAB 
*!**************************************************************************
FUNCTION lfChkAlloc
PRIVATE lnRespons
SELECT CUTPICK
lcCutPkOrd = ORDER()
SET ORDER TO TAG CUTORD
*--scan the cutpick file for any selected orders
SCAN FOR ORDER  = m.ORDER .AND. ALLTRIM(CUTPICK.CORDLINE) = ALLTRIM(STR(m.LINENO))
  *--If we didn't display the releas allocation message
  *--Check if the allocation is found
  IF !llAlocMsg
    lcMsg = 'These Sales Order lines have allocated quantities,'+;
    'canceling these lines will release the allocation, would you like to continue?'
    lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
    llAlocMsg    = .T.		&& the releas allocation message is displayed
    llOkCancel   = IIF(lnRespons = 1,.T.,.F.)
  ENDIF
  *--Check for CT/PO updates only if on Hold
  IF !llUpdPoCt .AND. !llAcordMsg AND llOkCancel
    IF CUTPICK.TRANCD = '1'
      IF SEEK(CUTPICK.CTKTNO,'CUTTKTH') .AND. CUTTKTH.STATUS = 'H'
        lcMsg = 'CT/PO is Hold, Would You like to update the generated CT/PO accordingly?'
        lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
        llAcordMsg = .T.
        llUpdPoCt  = IIF(lnRespons = 1,.T.,.F.)        
      ENDIF
    ELSE
      IF (SEEK('P'+CUTPICK.CTKTNO,'POSHDR') .OR. SEEK('C'+CUTPICK.CTKTNO,'POSHDR')) .AND. POSHDR.STATUS = 'H'
        lcMsg = 'CT/PO is Hold, Would You like to update the generated CT/PO accordingly?'
        lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
        llAcordMsg = .T.
        llUpdPoCt  = IIF(lnRespons = 1,.T.,.F.)                
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

*!**************************************************************************
*! Name      : lfRelAllOc
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/28/1999
*! Purpose   : Function to release the allocation and update CT/PO  
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfRelAllOc()
*E301340,1 WAB 
*!**************************************************************************
FUNCTION lfRelAllOc
PRIVATE lnCanTot
lnCanTot = 0
For lnCount = 1 TO 8
  lcCount = Str(lnCount,1)
  lnCanTot=  lnCanTot + IIF(m.qty&lcCount>m.pik&lcCount,m.qty&lcCount-m.pik&lcCount,0) 
  lnDef&lcCount = IIF(m.qty&lcCount>m.pik&lcCount,m.qty&lcCount-m.pik&lcCount,0)
ENDFOR
SELECT CUTPICK
lcCutPkOrd = ORDER()
SET ORDER TO TAG CUTORD
GOTO TOP
SCAN FOR ORDER = m.ORDER .AND. ALLTRIM(CUTPICK.CORDLINE) = ALLTRIM(STR(m.LINENO))
  IF TRANCD = '1'
    IF SEEK(CUTPICK.CTKTNO,'CUTTKTH')
      IF CUTTKTH.STATUS = 'H'
        IF llUpdPoCt
          PRIVATE lcStyOdr ,lcDyOrd
          *--Update Cut Ticket
          SELECT CUTTKTL
          lcOldCTOrd = ORDER()
          SET ORDER TO TAG Cutlin
          IF SEEK (CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.cTktLineNo + '1')
            WAIT WINDOW 'Updating CT# '+ ALLTRIM(CUTPICK.CTKTNO) NOWAIT
            =RLOCK()
          REPLACE QTY1 WITH QTY1 - IIF(Qty1>lnDef1,lnDef1,0),;
                  QTY2 WITH QTY2 - IIF(Qty2>lnDef2,lnDef2,0),;
                  QTY3 WITH QTY3 - IIF(Qty3>lnDef3,lnDef3,0),;
                  QTY4 WITH QTY4 - IIF(Qty4>lnDef4,lnDef4,0),;
                  QTY5 WITH QTY5 - IIF(Qty5>lnDef5,lnDef5,0),;
                  QTY6 WITH QTY6 - IIF(Qty6>lnDef6,lnDef6,0),;
                  QTY7 WITH QTY7 - IIF(Qty7>lnDef7,lnDef7,0),;
                  QTY8 WITH QTY8 - IIF(Qty8>lnDef8,lnDef8,0) 
            REPLACE TOTQTY WITH (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)
            UNLOCK
            *--Update Style File ( working in process )
            SELECT STYLE
            lcStyOdr = ORDER()
            SET ORDER TO STYLE
            IF SEEK(CUTTKTL.STYLE,'STYLE')
              =RLOCK()
              REPLACE WIP1   WITH WIP1 - IIF(WIP1>lnDef1,lnDef1,0),;
                      WIP2   WITH WIP2 - IIF(WIP2>lnDef2,lnDef2,0),;
                      WIP3   WITH WIP3 - IIF(WIP3>lnDef3,lnDef3,0),;
                      WIP4   WITH WIP4 - IIF(WIP4>lnDef4,lnDef4,0),;
                      WIP5   WITH WIP5 - IIF(WIP5>lnDef5,lnDef5,0),;
                      WIP6   WITH WIP6 - IIF(WIP6>lnDef6,lnDef6,0),;
                      WIP7   WITH WIP7 - IIF(WIP7>lnDef7,lnDef7,0),;
                      WIP8   WITH WIP8 - IIF(WIP8>lnDef8,lnDef8,0),;
                      NWO1   WITH NWO1 - IIF(WIP1>lnDef1,lnDef1,0),;
                      NWO2   WITH NWO2 - IIF(WIP2>lnDef2,lnDef2,0),;
                      NWO3   WITH NWO3 - IIF(WIP3>lnDef3,lnDef3,0),;
                      NWO4   WITH NWO4 - IIF(WIP4>lnDef4,lnDef4,0),;
                      NWO5   WITH NWO5 - IIF(WIP5>lnDef5,lnDef5,0),;
                      NWO6   WITH NWO6 - IIF(WIP6>lnDef6,lnDef6,0),;
                      NWO7   WITH NWO7 - IIF(WIP7>lnDef7,lnDef7,0),;
                      NWO8   WITH NWO8 - IIF(WIP8>lnDef8,lnDef8,0) 
              REPLACE TOTWIP WITH (WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8)                                            
              REPLACE NTOTWO WITH (NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8)                      
              UNLOCK
              SET ORDER TO (lcStyOdr)
              *--Update StyDYE File ( working in process )              
              SELECT STYDYE
              lcDyOrd = ORDER()
              SET ORDER TO STYDYE
              IF SEEK(CUTTKTL.STYLE+CUTTKTL.CWARECODE+PADR(CUTTKTL.DYELOT,10),'STYDYE')
                =RLOCK()
              REPLACE WIP1   WITH WIP1 - IIF(WIP1>lnDef1,lnDef1,0),;
                      WIP2   WITH WIP2 - IIF(WIP2>lnDef2,lnDef2,0),;
                      WIP3   WITH WIP3 - IIF(WIP3>lnDef3,lnDef3,0),;
                      WIP4   WITH WIP4 - IIF(WIP4>lnDef4,lnDef4,0),;
                      WIP5   WITH WIP5 - IIF(WIP5>lnDef5,lnDef5,0),;
                      WIP6   WITH WIP6 - IIF(WIP6>lnDef6,lnDef6,0),;
                      WIP7   WITH WIP7 - IIF(WIP7>lnDef7,lnDef7,0),;
                      WIP8   WITH WIP8 - IIF(WIP8>lnDef8,lnDef8,0),;
                      NWO1   WITH NWO1 - IIF(WIP1>lnDef1,lnDef1,0),;
                      NWO2   WITH NWO2 - IIF(WIP2>lnDef2,lnDef2,0),;
                      NWO3   WITH NWO3 - IIF(WIP3>lnDef3,lnDef3,0),;
                      NWO4   WITH NWO4 - IIF(WIP4>lnDef4,lnDef4,0),;
                      NWO5   WITH NWO5 - IIF(WIP5>lnDef5,lnDef5,0),;
                      NWO6   WITH NWO6 - IIF(WIP6>lnDef6,lnDef6,0),;
                      NWO7   WITH NWO7 - IIF(WIP7>lnDef7,lnDef7,0),;
                      NWO8   WITH NWO8 - IIF(WIP8>lnDef8,lnDef8,0) 
              REPLACE TOTWIP WITH (WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8)                                            
              REPLACE NTOTWO WITH (NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8)                      
                UNLOCK
              ENDIF
              SET ORDER TO (lcDyOrd)
              SELECT CUTTKTL
            ENDIF
          ENDIF
          SET ORDER TO TAG &lcOldCTOrd
          *--Update Cut ticket header 
          SELECT CUTTKTH
          =SEEK (CUTPICK.CTKTNO)
          lnRestOpn = PCS_OPN - lnCanTot
          =RLOCK()
          REPLACE PCS_OPN WITH IIF(PCS_OPN > 0 , PCS_OPN - lnCanTot  , PCS_OPN),;
                  TOTORD  WITH IIF(TOTORD > 0 , TOTORD - lnCanTot  , TOTORD),;
                  PCS_CAN WITH PCS_CAN + lnCanTot 
          REPLACE nEst_Cost1 WITH (CUTTKTL.nCost1 * lnRestOpn),;
                  nEst_Cost2 WITH (CUTTKTL.nCost2 * lnRestOpn),;
                  nEst_Cost3 WITH (CUTTKTL.nCost3 * lnRestOpn),;
                  nEst_Cost4 WITH (CUTTKTL.nCost4 * lnRestOpn),;
                  nEst_Cost5 WITH (CUTTKTL.nCost5 * lnRestOpn)
          IF PCS_OPN = 0
            REPLACE STATUS WITH IIF(PCS_REC > 0 , 'C' , 'X')
          ENDIF
          UNLOCK
        ENDIF
      ENDIF
      *--Releas The Allocation
      SELECT ORDHDR
      =SEEK('O'+M.ORDER)
      
      REPLACE TOTCUT WITH TOTCUT - CutPick.TotQty
      SELECT ORDLINE

      *C102520,1 BWA 08/01/2001 Use the index of the Pack_id in case GMA.[START]
      IF llGma
        PRIVATE lcOrderPk
        lcOrderPk = ORDER()
        SET ORDER TO TAG ORDLINE
        =SEEK('O' + CutPick.Order + CutPick.CORDLINE)
        SET ORDER TO TAG &lcOrderPk
      ELSE
        =SEEK('O' + CutPick.Order + CutPick.CORDLINE)
      ENDIF
      *C102520,1 BWA 08/01/2001.[END]

      *--Zero out Cut1,.....TotCut Fields in OrdLine
      =RLOCK()
      REPLACE CUT1   WITH 0,;
              CUT2   WITH 0,;
              CUT3   WITH 0,;
              CUT4   WITH 0,;
              CUT5   WITH 0,;
              CUT6   WITH 0,;
              CUT7   WITH 0,;
              CUT8   WITH 0,;
              TOTCUT WITH 0
      UNLOCK
      *-- Releas the allocated QTYs From CutTktl file
      SELECT CUTTKTL
      lcOldCTOrd = ORDER()
      SET ORDER TO TAG Cutlin
      IF SEEK (CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.cTktLineNo + '1')
        =RLOCK()
        REPLACE ORD1   WITH 0,;
                ORD2   WITH 0,;
                ORD3   WITH 0,;
                ORD4   WITH 0,;
                ORD5   WITH 0,;
                ORD6   WITH 0,;
                ORD7   WITH 0,;
                ORD8   WITH 0,;
                TOTORD WITH 0
        UNLOCK
      ENDIF
      SET ORDER TO TAG &lcOldCTOrd
      SELECT CUTPICK
      BLANK
      DELETE
    ENDIF
  ENDIF

  IF TRANCD = '2'

    IF SEEK('P' + CUTPICK.CTKTNO,'POSHDR') OR SEEK('C' + CUTPICK.CTKTNO,'POSHDR')
      IF POSHDR.STATUS = 'H'
        IF llUpdPoCt AND (SEEK ('P' + CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.CtktLineNo + '1','POSLN' ) OR SEEK('C' + CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.CtktLineNo + '1','POSLN'))
          *--Update PO
          WAIT WINDOW 'Updating PO# '+ ALLTRIM(CUTPICK.CTKTNO) NOWAIT
          SELECT POSLN
          =RLOCK()
          REPLACE QTY1 WITH QTY1 - IIF(Qty1>lnDef1,lnDef1,0),;
                  QTY2 WITH QTY2 - IIF(Qty2>lnDef2,lnDef2,0),;
                  QTY3 WITH QTY3 - IIF(Qty3>lnDef3,lnDef3,0),;
                  QTY4 WITH QTY4 - IIF(Qty4>lnDef4,lnDef4,0),;
                  QTY5 WITH QTY5 - IIF(Qty5>lnDef5,lnDef5,0),;
                  QTY6 WITH QTY6 - IIF(Qty6>lnDef6,lnDef6,0),;
                  QTY7 WITH QTY7 - IIF(Qty7>lnDef7,lnDef7,0),;
                  QTY8 WITH QTY8 - IIF(Qty8>lnDef8,lnDef8,0) 
          REPLACE TOTQTY WITH (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)
          UNLOCK
          PRIVATE lcStyOdr ,lcDyOrd
          SELECT STYLE
          lcStyOdr = ORDER()
          SET ORDER TO STYLE
          IF SEEK(POSLN.STYLE,'STYLE')
            *--update style file (working in process)
            =RLOCK()
              REPLACE WIP1   WITH WIP1 - IIF(WIP1>lnDef1,lnDef1,0),;
                      WIP2   WITH WIP2 - IIF(WIP2>lnDef2,lnDef2,0),;
                      WIP3   WITH WIP3 - IIF(WIP3>lnDef3,lnDef3,0),;
                      WIP4   WITH WIP4 - IIF(WIP4>lnDef4,lnDef4,0),;
                      WIP5   WITH WIP5 - IIF(WIP5>lnDef5,lnDef5,0),;
                      WIP6   WITH WIP6 - IIF(WIP6>lnDef6,lnDef6,0),;
                      WIP7   WITH WIP7 - IIF(WIP7>lnDef7,lnDef7,0),;
                      WIP8   WITH WIP8 - IIF(WIP8>lnDef8,lnDef8,0),;
                      NWO1   WITH NWO1 - IIF(WIP1>lnDef1,lnDef1,0),;
                      NWO2   WITH NWO2 - IIF(WIP2>lnDef2,lnDef2,0),;
                      NWO3   WITH NWO3 - IIF(WIP3>lnDef3,lnDef3,0),;
                      NWO4   WITH NWO4 - IIF(WIP4>lnDef4,lnDef4,0),;
                      NWO5   WITH NWO5 - IIF(WIP5>lnDef5,lnDef5,0),;
                      NWO6   WITH NWO6 - IIF(WIP6>lnDef6,lnDef6,0),;
                      NWO7   WITH NWO7 - IIF(WIP7>lnDef7,lnDef7,0),;
                      NWO8   WITH NWO8 - IIF(WIP8>lnDef8,lnDef8,0) 
              REPLACE TOTWIP WITH (WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8)                                            
              REPLACE NTOTWO WITH (NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8)                      
            UNLOCK
            SET ORDER TO (lcStyOdr)
            *--Update StyDYE File ( working in process )              
            SELECT STYDYE
            lcDyOrd = ORDER()
            SET ORDER TO STYDYE
            IF SEEK(POSLN.STYLE+POSLN.CWARECODE+PADR(POSLN.DYELOT,10),'STYDYE')
              =RLOCK()
              REPLACE WIP1   WITH WIP1 - IIF(WIP1>lnDef1,lnDef1,0),;
                      WIP2   WITH WIP2 - IIF(WIP2>lnDef2,lnDef2,0),;
                      WIP3   WITH WIP3 - IIF(WIP3>lnDef3,lnDef3,0),;
                      WIP4   WITH WIP4 - IIF(WIP4>lnDef4,lnDef4,0),;
                      WIP5   WITH WIP5 - IIF(WIP5>lnDef5,lnDef5,0),;
                      WIP6   WITH WIP6 - IIF(WIP6>lnDef6,lnDef6,0),;
                      WIP7   WITH WIP7 - IIF(WIP7>lnDef7,lnDef7,0),;
                      WIP8   WITH WIP8 - IIF(WIP8>lnDef8,lnDef8,0),;
                      NWO1   WITH NWO1 - IIF(WIP1>lnDef1,lnDef1,0),;
                      NWO2   WITH NWO2 - IIF(WIP2>lnDef2,lnDef2,0),;
                      NWO3   WITH NWO3 - IIF(WIP3>lnDef3,lnDef3,0),;
                      NWO4   WITH NWO4 - IIF(WIP4>lnDef4,lnDef4,0),;
                      NWO5   WITH NWO5 - IIF(WIP5>lnDef5,lnDef5,0),;
                      NWO6   WITH NWO6 - IIF(WIP6>lnDef6,lnDef6,0),;
                      NWO7   WITH NWO7 - IIF(WIP7>lnDef7,lnDef7,0),;
                      NWO8   WITH NWO8 - IIF(WIP8>lnDef8,lnDef8,0) 
              REPLACE TOTWIP WITH (WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8)                                            
              REPLACE NTOTWO WITH (NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8)                      
              UNLOCK
            ENDIF
            SET ORDER TO (lcDyOrd)
            SELECT POSLN
          ENDIF
          *--Update PURCHASE ORDER header file          
          SELECT POSHDR
          IF SEEK ('P' + CUTPICK.CTKTNO) OR SEEK(('C' + CUTPICK.CTKTNO))
            =RLOCK()
            REPLACE OPEN   WITH (OPEN - lnCanTot),;
                    CANCEL WITH (CANCEL+ lnCanTot)
            IF OPEN <= 0
              REPLACE STATUS WITH IIF(RECEIVE > 0 , 'C' , 'X')
            ENDIF
          ENDIF
          REPLACE nICost1 WITH (POSLN.nCost1 * POSHDR.OPEN),;
                  nICost2 WITH (POSLN.nCost2 * POSHDR.OPEN),;
                  nICost3 WITH (POSLN.nCost3 * POSHDR.OPEN),;
                  nICost4 WITH (POSLN.nCost4 * POSHDR.OPEN),;
                  nICost5 WITH (POSLN.nCost5 * POSHDR.OPEN)
          UNLOCK
        ENDIF
      ENDIF
      SELECT POSHDR
      *-- RELEASE The allocation from poshdr File
      IF SEEK ('P' + CUTPICK.CTKTNO) OR SEEK(('C' + CUTPICK.CTKTNO))
        REPLACE TOTORD WITH 0
      ENDIF
      IF SEEK ('P' + CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.CtktLineNo + '1','POSLN' ) OR SEEK('C' + CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.CtktLineNo + '1','POSLN')
        WAIT WINDOW 'Updating PO# '+ ALLTRIM(CUTPICK.CTKTNO) NOWAIT
        *-- Releas the allocated QTYs From POSLN file
        SELECT POSLN
        REPLACE ORD1   WITH 0,;
                ORD2   WITH 0,;
                ORD3   WITH 0,;
                ORD4   WITH 0,;
                ORD5   WITH 0,;
                ORD6   WITH 0,;
                ORD7   WITH 0,;
                ORD8   WITH 0,;
                TOTORD WITH 0
      ENDIF
    ENDIF
    *--Releas The Allocation
    SELECT ORDHDR
    =SEEK ('O'+M.ORDER)
    REPLACE TOTCUT WITH TOTCUT - CutPick.TotQty
    SELECT ORDLINE

    *C102520,1 BWA 08/01/2001 Use the index of the Pack_id in case GMA.[START]
    *=SEEK('O'+ CutPick.Order + CutPick.CORDLINE)
    IF llGma
      PRIVATE lcOrderPk2
      lcOrderPk2 = ORDER()
      SET ORDER TO TAG ORDLINE
      =SEEK('O' + CutPick.Order + CutPick.CORDLINE)
      SET ORDER TO TAG &lcOrderPk2
    ELSE
      =SEEK('O'+ CutPick.Order + CutPick.CORDLINE)
    ENDIF
    *C102520,1 BWA 08/01/2001.[END]

    *--Zero out Cut1,.....TotCut Fields in OrdLine
    REPLACE CUT1   WITH 0,;
            CUT2   WITH 0,;
            CUT3   WITH 0,;
            CUT4   WITH 0,;
            CUT5   WITH 0,;
            CUT6   WITH 0,;
            CUT7   WITH 0,;
            CUT8   WITH 0,;
            TOTCUT WITH 0
    SELECT CUTPICK
    BLANK
    DELETE
  ENDIF
ENDSCAN
SELECT CUTPICK
SET ORDER TO TAG (lcCutPkOrd)

 


*!**************************************************************************
*! Name      : lfCanShave
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/28/1999
*! Purpose   : Check it a line may be shaved or not
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None.
*!**************************************************************************
*! Example   : =lfRelAllOc()
*E301340,1 WAB 
*!**************************************************************************
FUNCTION lfCanShave
PRIVATE llShave, lnCount
llShave = .F.
For lnCount = 1 TO 8
  lcCount = STR(lnCount,1)
  IF m.pik&lcCount < m.qty&lcCount
     llShave = .T.
     EXIT
  ENDIF
ENDFOR
RETURN llShave

*!*************************************************************
*! Name      : lfTotAvlbl
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/25/1999
*! Purpose   : calclulate the total availble qty or the avalble qty
*!             by size 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcSize -> size No
*!								   lcSize = '9'  get total available 
*!								   lcSize =('1'-->'8' ) get by size
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfTotAvlbl(lcSize)
*!*************************************************************
**E500304,1 WAB 
*!*************************************************************
FUNCTION lfTotAvlbl
PARAMETER lcSize

PRIVATE lnTotal,lnRecNo,lnWIP,lnCurAlias
*------------- this function is to calculate the availble qty depend on setting
*------------- in IC module  ( the availble = STQ - ALO ) or
*------------- (the availble qty = WIP + STQ - ALO )
*--lcSize  --> hold ('1'->'8') called to get availble by size
*--lnTotal --> hold the availble qty
*--lnRecNo --> hold the current record no
*--lnWIP   --> hold the WIP from stydye
*--lcSeekFile -> hold file name to be select from
lnCurAlias = SELECT(0)
IF EOF(lc_TmpStyD)
  SELECT STYDYE
ELSE
  SELECT (lc_TmpStyD)
ENDIF
*-- if WIP is not included in calculating available

IF !llTotAvlbl		&& the avaible = stq - alocated
  *B804197,1 HBG 19/09/2001 Check the avilable stock for size by size, not the total [Begin]
  *lnTotal = MAX(IIF(lcsize = '9',;
  					TotStk - TotAlo, Stk&lcSize - Alo&lcSize) ,0)
  
  *B804502,1 HBG 29/10/2001 if lcsize is 'T' we calculate total availabel in case of browse[Begin]
  IF lcsize = 'T'					
    lnTotal = MAX(TotStk - TotAlo,0)
  ELSE 	
  *B804502,1 [End]				
    IF lcsize = '9'					
      lnTotal = 0
      FOR lnI = 1 TO SCALE.Cnt
        lcI = STR(lnI,1)
        *B605084,1 HBG 01/11/2001 Calculate the avaliable qty to be picked Size by size [Begin]
        *lnAvl = MAX(Stk&lcI - Alo&lcI ,0)
        *IF lnAvl  = 0
        *  SELECT(lnCurAlias)
        *  RETURN 0
        *ELSE
        *  lnTotal = lnTotal + lnAvl  
        *ENDIF
        lnTotal = lnTotal + MAX(Stk&lcI - Alo&lcI ,0)
        *B605084,1 [End]
      ENDFOR
    ELSE
      lnTotal = MAX(Stk&lcSize - Alo&lcSize,0)
    ENDIF
  *B804502,1 HBG 29/10/2001 end if lcsize is 'T' we calculate total availabel in case of browse[Begin]
  ENDIF
  *B804502,1 [End]
  *B804197,1 [End]
ELSE					   && the avialble = wip + stq - alocated
  *B804197,1 HBG 19/09/2001 Check the avilable stock for size by size, not the total [Begin]
  *lnTotal = MAX(IIF(lcsize = '9',;
  					TotStk - TotAlo + IIF(EMPTY(Dyelot), TotWIP, lfGetWIP('9')),;
  				    Stk&lcSize - Alo&lcSize + IIF(EMPTY(Dyelot), WIP&lcSize, lfGetWIP(lcSize))) ,0)
  *B804502,1 HBG 29/10/2001 If lcsize is 'T' we calculate total availabel in case of browse[Begin]
  IF lcsize = 'T'					
    lnTotal = MAX(TotStk - TotAlo + IIF(EMPTY(Dyelot), TotWIP, lfGetWIP('9')),0)
  ELSE 	
  *B804502,1 [End]
    IF lcsize = '9'					
      lnTotal = 0
      FOR lnI = 1 TO SCALE.Cnt
        lcI = STR(lnI,1)
        *B605084,1 HBG 01/11/2001 Calculate the avaliable qty to be picked Size by size [Begin]
        *lnAvl = MAX(Stk&lcI - Alo&lcI ,0)
        *IF lnAvl  = 0
        *  SELECT(lnCurAlias)
        *  RETURN 0
        *ELSE
        *  lnTotal = lnTotal + lnAvl  
        *ENDIF
        lnTotal = lnTotal + MAX(Stk&lcI - Alo&lcI ,0) + WIP&lcI
        *B605084,1 [End]
      ENDFOR
    ELSE
      *B605075,1 HBG 01/11/2001 Fix bug of In case the IC setup is " Include WIP (YES) ",
      *B605075,1                Wrong AVA.QTY per size. [Begin]
      *lnTotal = MAX(IIF(EMPTY(Dyelot), TotWIP, lfGetWIP('9')),;
    				    Stk&lcSize - Alo&lcSize + IIF(EMPTY(Dyelot), WIP&lcSize, lfGetWIP(lcSize)),0)
 	  lnTotal = MAX(Stk&lcSize - Alo&lcSize + IIF(EMPTY(Dyelot), WIP&lcSize, lfGetWIP(lcSize)),0)
 	  *B605075,1 [End] 
    ENDIF  
  *B804502,1 HBG 29/10/2001 end if lcsize is 'T' we calculate total availabel in case of browse[Begin]
  ENDIF
  *B804502,1 [End]
  *B804197,1 [End]				    
ENDIF
SELECT(lnCurAlias)
RETURN lnTotal

FUNCTION lfGetWIP
PARAMETERS lcSize
RETURN 0

*!*************************************************************
*! Name      : lfDyeBrow
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/15/1999
*! Purpose   : PROGRAM TO BROWSE THE STYLE DYELOTS TO SELECT ONE
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcSize -> size No
*!								   lcSize = '9'  get total available 
*!								   lcSize =('1'-->'8' ) get by size
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfTotAvlbl(lcSize)
*!*************************************************************
*B802829,1 WAB
*!*************************************************************
PROCEDURE lfDyeBrow
PARAMETER XSTYLE,XDYELOT, lcFromWare 
PRIVATE lcBrFields,lcAlias, lcFromWare, lnStyOrd, lnAlias,laData,;
        lnBrHSRow1, lnBrHSCol1, lnBrHSRow2, lnBrHSCol2
DECLARE laData[3] && array to get values from browse
laData     = ' '
llBrowse   = IIF(TYPE('llBrowse')='U',.F.,llBrowse) && variable to determine forcing browse or not
lnBrHSRow1 = gnBrFSRow1
lnBrHSCol1 = gnBrFSCol1
lnBrHSRow2 = gnBrHSRow1
lnBrHSCol2 = gnBrHSCol1
xStyle     = PADR(xStyle, 19)
lcTitle    = "Style "+ALLTRIM(xStyle)+" Dyelots"

lcBrFields = "DYELOT:R :H='Dyelot #':10," +; 
	         "A1=lfAvlbl('1')  :R :H='Avl1':6,"+;	         	           	         
             "A2=lfAvlbl('2')  :R :H='Avl2':6,"+;
	         "A3=lfAvlbl('3')  :R :H='Avl3':6,"+;
	         "A4=lfAvlbl('4')  :R :H='Avl4':6,"+;
	         "A5=lfAvlbl('5')  :R :H='Avl5':6,"+;
	         "A6=lfAvlbl('6')  :R :H='Avl6':6,"+;
	         "A7=lfAvlbl('7')  :R :H='Avl7':6,"+;
	         "A8=lfAvlbl('8')  :R :H='Avl8':6,"+;
	         "A9=lfAvlbl('9')    :R :H='TotAvl':7,"

lcBrFields = lcBrFields +; 
             "ALO1  :R :H='Alo1':6,"+;
  	         "ALO2  :R :H='Alo2':6,"+;
	         "ALO3  :R :H='Alo3':6,"+;
	         "ALO4  :R :H='Alo4':6,"+;
	         "ALO5  :R :H='Alo5':6,"+;
	         "ALO6  :R :H='Alo6':6,"+;
	         "ALO7  :R :H='Alo7':6,"+;
	         "ALO8  :R :H='Alo8':6,"+;
             "TOTALO:R :H='TotAlo':7,"+;
	         "STK1  :R :H='Stk1':6,"+;	         	           	         
             "STK2  :R :H='Stk2':6,"+;
	         "STK3  :R :H='Stk3':6,"+;
	         "STK4  :R :H='Stk4':6,"+;
	         "STK5  :R :H='Stk5':6,"+;
	         "STK6  :R :H='Stk6':6,"+;
	         "STK7  :R :H='Stk7':6,"+;
	         "STK8  :R :H='Stk8':6,"+;
	         "TOTSTK:R :H='TotStk':7"
lnAlias    = SELECT()
SELECT STYDYE
LOCATE REST FOR !EMPTY(DYELOT) WHILE STYLE + cWareCode = XSTYLE + lcFromWare
=lfGetStyDy()
SELECT (lcStyDTemp)
GO TOP
IF !FOUND()
  *** There are no dyelots for style ALLTRIM(xStyle) ***
  *** <  Ok  > ***
  lcTmpStr = ALLTRIM(xStyle) + ;
             IIF(EMPTY(lcFromWare) , ;
                 ' on the file' , ;
                 ' in location ' + ALLTRIM(lcFromWare))
  =gfModalGen("INM00277B00000" , "DIALOG" , lcTmpStr)
  llWasSel = .F.
ELSE
  llWasSel= ARIABROW('XSTYLE + lcFromWare'+[FOR !EMPTY(DYELOT)],lcTitle,;
            lnBrHSRow1, lnBrHSCol1, lnBrHSRow2, lnBrHSCol2,'','',"Style,Dyelot","laData")
  IF llWasSel
    XSTYLE = laData[1]
    XDYELOT= laData[2]
  ELSE
    XDYELOT= SPACE(10)
  ENDIF  
ENDIF
SELECT (lnAlias)
RETURN llWasSel
*!*************************************************************
*! Name      : lfGetStyDy
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/25/1999
*! Purpose   : coloect data from temp stydy or from stydye 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetStyDy
*!*************************************************************
*B802829,1 WAB
*!*************************************************************
*B802829,1 KHM 03/06/2000 Changing the name of the functio in order
*B802829,1                to use it in different way.
FUNCTION lfGetStyD
SELECT (lcStyDTemp)
ZAP
lnTmpRecNo= RECNO(lc_TmpStyD)
SELECT STYDYE
lnStDRecNo= RECNO()
=SEEK(XSTYLE+lcFromWare)
SCAN WHILE  STYLE + cWareCode = XSTYLE + lcFromWare
  IF !SEEK(STYLE+CWARECODE+Dyelot,lc_TmpStyD)
    SCATTER TO laStyDye 
  ELSE
    SELECT (lc_TmpStyD)
    SCATTER TO laStyDye 
  ENDIF
  INSERT INTO (lcStyDTemp) FROM ARRAY laStyDye
ENDSCAN
IF BETWEEN(lnTmpRecNo, 1, RECCOUNT(lc_TmpStyD))
  GO  lnTmpRecNo IN &lc_TmpStyD
ENDIF
IF BETWEEN(lnStDRecNo, 1, RECCOUNT('STYDYE'))
  GO  lnStDRecNo IN STYDYE
ENDIF

*!*************************************************************
*! Name      : lfAvlbl
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/15/1999
*! Purpose   : calclulate the total availble qty or the avalble qty
*!             by size 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcSize -> size No
*!								   lcSize = '9'  get total available 
*!								   lcSize =('1'-->'8' ) get by size
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAvlbl(lcSize)
*!*************************************************************
*B802829,1 WAB
*!*************************************************************
FUNCTION lfAvlbl
PARAMETER lcSize
PRIVATE lnTotal,lnRecNo,lnWIP

IF !llTotAvlbl		&& the avaible = stq - alocated
  lnTotal = MAX(IIF(lcsize = '9',;
  					TotStk - TotAlo, Stk&lcSize - Alo&lcSize) ,0)
ELSE					   && the avialble = wip + stq - alocated
  *B802938,1 WAB - wrong position of '()'
  *lnTotal = MAX(IIF(lcsize = '9',;
  					TotStk - TotAlo + lfGetWIP('9')),;
  				    Stk&lcSize - Alo&lcSize + lfGetWIP(lcSize))) ,0)
  lnTotal = MAX(IIF(lcsize = '9',;
  					TotStk - TotAlo + lfGetWIP('9'),;
  				    Stk&lcSize - Alo&lcSize + lfGetWIP(lcSize)) ,0)
  *B802938,1 WAB - END
ENDIF
RETURN(lnTotal)


*!*************************************************************
*! Name      : lfGetStyDy
*! Developer : Khalid Mohi El-Din
*! Date      : 03/06/2000
*! Purpose   : To get all the dyelots of the styles in the
*!             selected order.
*!*************************************************************
*! Example            :  =lfGetStyDy
*!*************************************************************
*!B802829,1 KHM 03/06/2000 
*!*************************************************************
FUNCTION lfGetStyDy
PRIVATE lnOrdRecNo,lnStyRecNo,lnSDyeRcNo,lnAlias

lnAlias    = SELECT(0)
lnOrdRecNo = RECNO('OrdLine')
lnStyRecNo = RECNO('Style')
lnSDyeRcNo = RECNO('StyDye')
SELECT OrdLine
SEEK 'O'+laData[1]
SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+laData[1]
  =SEEK(Style,'Style')
  IF llUseDyes .AND. Style.cDye_flg = 'Y' AND;
    SEEK(Style+cWareCode, 'STYDYE')
    SELECT StyDye
    SCAN REST WHILE Style+cWareCode+Dyelot = OrdLine.Style+OrdLine.cWareCode;
              FOR !EMPTY(Dyelot)      
      SCATTER TO laStyDye
      IF !SEEK(Style+cWareCode+Dyelot,lcStyDTemp)
        INSERT INTO (lcStyDTemp) FROM ARRAY laStyDye
      ENDIF
    ENDSCAN    
  ENDIF    
ENDSCAN

IF BETWEEN(lnOrdRecNo, 1, RECCOUNT('OrdLine'))
  GO  lnOrdRecNo IN OrdLine
ENDIF
IF BETWEEN(lnStyRecNo, 1, RECCOUNT('Style'))
  GO  lnStyRecNo IN Style
ENDIF
IF BETWEEN(lnSDyeRcNo, 1, RECCOUNT('StyDye'))
  GO  lnSDyeRcNo IN StyDye
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfAdjTStyD
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 03/06/2000
*! Purpose   : To adjust the allocated qty per dyelot
*!*************************************************************
*! Parameters         :  lcType    : 'R' To release the allocated qty.
*!                                   'A' To allocate per dyelot.
*!                       lcSize    : The qty per size
*!                       lnOldVal1 : Old allocated qty. per size.
*!                       lnNewVal1 : New allocated qty. per size.
*!*************************************************************
*! Example            :  =lfAdjTStyD()
*!*************************************************************
*B802829,1 KHM 03/06/2000 Added
*!*************************************************************
FUNCTION lfAdjTStyD
PARAMETER lcType,lcSizeNo,lnOldVal1,lnNewVal1
PRIVATE lcType,lcSizeNo,lnOldVal1,lnNewVal1,lnAlias

lnAlias = SELECT(0)

IF SEEK(m.Style + m.cWareCode + m.Dyelot,lcStyDTemp)
  SELECT(lcStyDTemp)
  IF EMPTY(lcSizeNo)
    FOR lnStkCntr = 1 TO 8
      lcStkCnt = STR(lnStkCntr,1)      
      IF lcType='R'
        REPLACE Alo&lcStkCnt WITH MAX(Alo&lcStkCnt-m.Pik&lcStkCnt,0)
      ELSE      
        REPLACE Alo&lcStkCnt WITH Alo&lcStkCnt+m.Pik&lcStkCnt
      ENDIF
    ENDFOR
    REPLACE TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
  ELSE
    REPLACE Alo&lcObjNum WITH MAX(Alo&lcObjNum - lnOldVal1 + lnNewVal1,0),;
            TotAlo       WITH MAX(TotAlo - lnOldVal1+lnNewVal1,0)
  ENDIF
ENDIF
SELECT(lnAlias)



*!*************************************************************
*! Name      : lfGetAvlbl
*! Developer : HEND GHANEM (HBG)
*! Date      : 11/06/2001
*! Purpose   : calclulate the total availble qty or the avalble qty
*!             by size 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcSize -> size No
*!								   lcSize =('1'-->'8' ) get by size
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetAvlbl(lcSize)
*!*************************************************************

FUNCTION lfGetAvlbl
PARAMETER lcSize

IF !llTotAvlbl
  lnTotAvl = m.Qty&lcSize
ELSE
  lnTotAvl = m.Qty&lcSize + IIF(EMPTY(Dyelot), IIF(EOF(lc_TmpStyD),STYDYE.WIP&lcSize,&lc_TmpStyD..WIP&lcSize), lfGetWIP(&lcSize))
ENDIF

RETURN lnTotAvl

*!*************************************************************
*! Name      : lpTopScr
*! Developer : Hend Ghanem
*! Date      : 11/27/2005
*! Purpose   : Validation of the top button in the pannel
*!*************************************************************
*! Calls     : 
*!          Calls: LPTOPSCR.PRG             
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* The valid function for the control panel Top push button
*:-> laDefProc[1]
*:B607537,1
FUNCTION lpTopScr

SCATTER FIELDS &lcScFields MEMO TO laData
DO WHILE laData[3] = 'H'
  *Message : 44096 ==> Order status on hold ! Continue ?.
  *Buttons : 44009 ==> < Yes > ,  < No >
  lnAnsr=gfModalGen('INM44096B44009','DIALOG',laData[1])
  *--if the user choose to not continue , return to select mode
  IF lnAnsr = 2     
    SELECT (lcBaseFile)
    SKIP 1
    SCATTER FIELDS &lcScFields MEMO TO laData
  ENDIF
ENDDO
 
*!*************************************************************
*! Name      : lpBtmScr
*! Developer : Hend Ghanem
*! Date      : 11/27/2005 
*! Purpose   : Validation of the bottom button in the pannel
*!*************************************************************
*! Calls     : 
*!          Calls: LPBTMSCR.PRG             
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* The valid function for the control panel Top push bottom
*:-> laDefProc[2]
*:B607537,1
FUNCTION lpBtmScr

SCATTER FIELDS &lcScFields MEMO TO laData
DO WHILE laData[3] = 'H'
  *Message : 44096 ==> Order status on hold ! Continue ?.
  *Buttons : 44009 ==> < Yes > ,  < No >
  lnAnsr=gfModalGen('INM44096B44009','DIALOG',laData[1])
  *--if the user choose to not continue , return to select mode
  IF lnAnsr = 2     
    SELECT (lcBaseFile)
    SKIP -1
    SCATTER FIELDS &lcScFields MEMO TO laData
  ENDIF
ENDDO


*!*************************************************************
*! Name      : lpNxtScr
*! Developer : Hend Ghanem
*! Date      : 11/27/2005 
*! Purpose   : Validation of next button in the pannel
*!*************************************************************
*! Calls     : 
*!           Calls: LPNXTSCR.PRG             
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* The valid function for the control panel Top push Next
*:-> laDefProc[3]
*:B607537,1
FUNCTION lpNxtScr

SCATTER FIELDS &lcScFields MEMO TO laData
IF laData[3] = 'H'
  *Message : 44096 ==> Order status on hold ! Continue ?.
  *Buttons : 44009 ==> < Yes > ,  < No >
  lnAnsr=gfModalGen('INM44096B44009','DIALOG',laData[1])
  *--if the user choose to not continue , return to select mode
  IF lnAnsr = 2     
    SELECT (lcBaseFile)
    SKIP -1
    SCATTER FIELDS &lcScFields MEMO TO laData
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lpPrvScr
*! Developer : Hend Ghanem
*! Date      : 11/27/2005 
*! Purpose   : Validation of the prvios button in the pannel
*!*************************************************************
*! Calls     : 
*!          Calls: LPPRVSCR.PRG             
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* The valid function for the control panel Top push previous
*:->laDefProc[4]
*:B607537,1
FUNCTION lpPrvScr

SCATTER FIELDS &lcScFields MEMO TO laData

IF laData[3] = 'H'
  *Message : 44096 ==> Order status on hold ! Continue ?.
  *Buttons : 44009 ==> < Yes > ,  < No >
  lnAnsr=gfModalGen('INM44096B44009','DIALOG',laData[1])
  *--if the user choose to not continue , return to select mode
  IF lnAnsr = 2     
    SELECT (lcBaseFile)
    SKIP 1
    SCATTER FIELDS &lcScFields MEMO TO laData
  ENDIF
ENDIF
