*:************************************************************************
*: Program file  : ALSTYAL.PRG
*: Program desc. : Program To Allocate By Style.
*: For screen    : ALSTYAL.SCX (0,1,2)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Allocation Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *E300511,1 Rewrite the program to work in the 2.7 version
*:************************************************************************
*: Calls         : 
*:         Procedures : None
*:         Programs   : None
*:         Screens    : ALSTYAL.SPX
*:                      ALMDLIN.SPX
*:         Functions  : gfSetup
*:                      lfActPad
*:                      lpPicMethd
*:                      lfwOldCVal
*:                      lfvData_1
*:                      lfvOrdWare
*:                      lfvPikWare
*:                      lfSayTotal
*:                      lfwBrowOrd
*:                      lfwBrwRef
*:                      lfvBrows
*:                      lfActMain
*:                      lfClrTrap
*:                      lfDactMain
*:                      lfvLink
*:                      lfvProcess
*:                      lfAloLine
*:                      lfRelLine
*:                      lfModiLine
*:                      lfvDyelot
*:                      lfvPik
*:                      lfvClsMod
*:                      lfAloQty
*:                      lfNoDye
*:                      lfModiOrd
*:                      lfOrdQty
*:                      lfOnlyLine
*:                      lfSqlTherm
*:                      lfPikAlo
*:                      lfRecLock
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Example            :
*:  DO gpDoProg WITH 'AWRALSTYAL',.F.,'AL'
*:*************************************************************
*:  Modifications :
*:
*E301077,15 Reham On 12/29/1998
*E301077,15 Reduce the opened file on the program level:
*E301077,15 1- Change the files in SydObjct:
*E301077,15    From : STYLE,STYDYE,SEQUENCE,SCALE,WAREHOUS,PIKTKT,ORDLINE,CODES,PIKLINE,ORDHDR|STYLE,STYDYE,CSEQ_TYPE,SCALE,WAREHOUS,PIKTKT,ORDLINES,CCODE_NO,PIKLINE,ORDHDR
*E301077,15    To   : STYLE,STYDYE,PIKTKT,ORDLINE,ORDHDR|STYLE,STYDYE,PIKTKT,ORDLINES,ORDHDR
*E301077,15 2- Open the removed files from SydObjct in the current program.
*B602419,1  Reham On 02/07/1999
*B602419,1  Open style dyelot browse upon Allocating any line if style 
*B602419,1  dyelot <Yes> & the style has dyelots. 
*B602545,1  Reham On 02/21/1999
*B602545,1  Do not release a pick ticket have a packing list.
*B602663,1  Reham On 03/15/1999
*B602663,1  Capitalize the dyelot field in AlMdLin.Scx.
*B602904,1  Reham On 05/16/1999
*B602904,1  Make the screen more fast upon openning at the begining 
*B602904,1  and when change styles or locations.
*B802282,1  Reham On 05/19/1999
*B802282,1  Add the CustPo from the order header file to the PikTkt file.
*B802134,1  Reham On 05/24/1999
*B802134,1  Point to the first record in the order browse.
*C200084,1 MAB 
*E301225,1 MAB 08/23/1999 Allow Force allocation option.
*E301310,1 MAB 08/24/1999 Check Approved Amount.
*B802643,1 Reham On 09/29/1999
*B802643,1 1- Display the proper orders upon selecting new style.
*B802643,1 2- Enable the soft seek in the style browse.
*B802643,1 3- Fix 'Alias Codes not found' that sometimes happen upon
*B802643,1    browsing from the golbao browse.
*B802624,1 Reham On 11/22/1999
*B802624,1 1- Fix the bug of releasing an order line if there is more order lines in the piktkt.
*B802624,1 2- Save the order info before releasing to be saved into the Pikline file.
*E500304,1 WAB 11/25/1999 calculate the avaliable qty depend on setting in AL Module
*E500304,1 WAB            ( STQ - Alocated qty ) or ( Wip+stq - alocated qty )
*E500304,1 WAB            and Add Butt to display O.T.S 
*B802501,1 WAB 01/10/2000 fix the bug of Release P/T does not update files properly  
*B603464,1 KHM 02/21/2000 Fix the bug of not updating the ordline warehouse
*B603464,1                with the picked from warehouse and not restoring 
*B603464,1                the default warehouse of the ordline when releasing.
*B603529,1 KHM 03/26/2000 Fix the bug of not displaying the dyelot in the 
*B603529,1                browse, not displaying the availabe fields in when 
*B603529,1                browsing the dyelots, not moving the cursor to the 
*B603529,1                next record when allocating or picking, and fix the 
*B603529,1                bug of allowing to allocate or pick ordlines that 
*B603529,1                has a non-active account.
*B803358,1 AMH 06/25/2000 Checking Order Approved Amount
*B803380,1 HBG 07/03/2000 If user trying to pick or allocate an order with status on hold
*B803380,1                get a warning message asking him if he/she wants to contiunue.
*B603719,1 HBG 07/03/2000 The EDI temp order shouldn't show in the browes of the orders 
*B603719,1                to be allocated. 
*B603804,1 MAB 08/10/2000 Do not apply packing condition upon release if it is allocated line
*B603804,1                only, i.e. No Pick tickets...
*B803667,1 HBG 09/21/2000 If releasing as allocated order line , change the message
*B803741,1 HBG 11/12/2000 Fix bug of If the style has stock greater than order qty at 
*B803741,1                the 1st size only and the other sizes stock less than the 
*B803741,1                order qty the system does not display the force allocation  
*B803741,1                message and pick for the 1st size only
*B803845,1 HBG 11/16/2000 Order Qty not saved correctly
*B803668,1 MAB 11/30/2000 Avoid Point to a wrong record in the Styles Browse. 
*B804033,1 AMM 02/18/2001 Fix the bug of subscript out of bounds
*C102282,1 ABD 05/23/2001 Custom change to prevent the user to allocat order from the program
*B604851,1 ADEL 09/05/01  Fix the bug of showing wrong available when modifying a previously 
*B604851,1                allocated line. Note : style/color should have more than one order.
*C102424,1 ADEL 10/15/01 Add record for each piktkt in ALSHPINF for Ogio.
*B603575,1 ADEL 10/30/01 Fix (Variable ' LCBRTTL ' not found) bug.
*B604759,1 TMI 08/27/2002 Fix some bugs in alstyal screen related to updating total quantities
*B605047,1 TMI 08/28/2002 make the ordqty depend on the OrdWarehous
*B606699,1 TMI 12/23/2002 Fix the bug that Over allocating when force allocation set to "No"
*C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*E126905,1 EIH 04/11/2005 Update in new field 'CALLOCATB' by 'S'in case of allocated or picking by style  
*                         ' ' for cancel allocation or picking
*B039660,1 NNA 09/04/2005 Transfer all the bin location's Programs To Binmain.prg instead of Davmain.prg and Check first if
*B039660,1 NNA            there is enough stock or not before making update in the Bin location Files
*B129738,1 MMR 10/13/2005 Fixing bug of not Generate 940 EDI transaction for 
*B129738,1 MMR            pick tickets directed to 3PL providers.
*E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt.
*:*******************************************************************************************
*:*
EXTERNAL ARRAY laData , laKeyField , laDefProc

*-- Define the text variables.

lcAloBrwTl = 'Orders'      && Variable hold the browse title.
lcLinkMesg = 'Object Link' && Variable hold the object linking button message.

DECLARE laKeyField [1,4]
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'Style'
laKeyField[1,4] = 1

*-- Add more buttons to the control pannel.
DECLARE laPanelObj[1,3]
laPanelObj[1,1] = "pbLink"
laPanelObj[1,2] = gcBmpHome + "Relate.BMP"
laPanelObj[1,3] = "VALID lfvLink() MESSAGE lcLinkMesg"

*-- Array hold all the available warehouses.
DECLARE laWareHous[1,2]

*B802624,1 Reham On 11/22/1999    *** Begin ***
*B802624,1 Save the order qty. before releasing to be saved into the Pikline file.
DECLARE laReleased[13]
laReleased = ""
*B802624,1 Reham On 11/22/1999    *** End   ***

*-- Get the prompt of the available pictures on the screen.
lcBrowBmp = gcBmpHome + "ExtKey.BMP"
lcClosBmp = gcBmpHome + "CLS.BMP"
lcPikBmp  = gcBmpHome + "Pick1.BMP"
lcModBmp  = gcBmpHome + "Modify.Bmp"
lcRelBmp  = gcBmpHome + "Release1.Bmp"
lcAloBmp  = gcBmpHome + "Allocate.Bmp"

*-- Initialaize the needed variables.
lcScFields = ""
lcAlStyAl0 = ""
lcAlStyAl1 = ""
lcAlStyAl2 = ""
llFrstRun  = .T.
lcCodesOrd = ""  && Var. to hold the old codes order.

*B803668,1 MAB Define the While expression variable so that I can use it [Begin]
*B803668,1     when move to next record
lcWhilExpr = ""
*B803668,1 MAB Define the While expression variable so that I can use it [End  ]

STORE .F. TO llBrowse , llMultiWH , llDyelot

*E301225,1 Allow Force allocation option [Begin]
lcAlwForce = ''
*E301225,1 Allow Force allocation option [End  ]

*E301310,1 Check Approved Amount [Begin]
llChkAprov = .F.
*E301310,1 Check Approved Amount [End  ]

STORE "DISABLE" TO lcAloStat , lcEdtStat , lcPikStat , lcAloStat , lcViewStat

STORE 0 TO lnTotal , lnCurRec , lnScaleCnt , lnTotStk , ;
           lnTotAlo , lnTotAvl , lnTotWip , lnTotUnalo  , lnTotOrd ,;
           lnRecCode  , lnRecStyle , lnRecScale , lnRecPkTk , ;
           lnRecOrdHd , lnRecOrdLn , lnRecStyDy , lnRecWrHs , ;
           lnRecPikLn , lnMarker

STORE " " TO lcSty_Desc , lcDefWare , lcOrdWare , lcPikWare , lcOldValue

lcBrFields = ""
lcStyHdr   = ""
lnStyLngth = 0
lnPicMethd = 1   && Var. to determine the picking ticket method.

*B802134,1 Reham On 05/24/1999   *** Begin ***
*B802134,1 Define variable hold the record pointer in the order lines browse.
lnCurMark = 0
*B802134,1 Reham On 05/24/1999   *** End   ***

*-- Array for the header data, & variable hold the header fields.
*DECLARE laData[1]
*laData     = ""

*E301077,15 Reham On 02/22/99   *** Begin ***
*E301225,1 Allow Force allocation option [Begin]
*E301310,1 And Check approved amount
*DECLARE laMainSetp[2,2]
DECLARE laMainSetp[4,2]
*E301225,1 Allow Force allocation option [Begin]
*E301077,15 Reham On 02/22/99   *** End   ***


*-- Call global function in the main program to do the following : _
*-- Intialise all the variables & open all the files needed in
*-- this session and controling disabling and enabling of the
*-- menu bars and writting the screen names in the window bars.
IF !gfSetup()
  RETURN
ENDIF
*E500304,1 WAB - get llTotAvlbl setting 
*E500304,1 WAB - llTotAvlbl = .F. --> avlbel qty = stq - alocated qty
*E500304,1 WAB - lcTotAvlbl = .T. --> avlbel qty = WIP + stq - alocated qty
*E500304,1 WAB - START
llTotAvlbl = gfGetMemVar('M_TOTAVLBL')  
*E500304,1 WAB - END


*E130074,1 EIH 01/02/2006 Check if the field name 'CSNDPKT' exist in the PIKTKT file. [Begin]
lnAlias = SELECT(0) 
STORE .F. TO llSndPkFld
SELECT PIKTKT
FOR I = 1 TO FCOUNT('PIKTKT')
  IF UPPER(FIELD(I))='CSNDPKT'
    llSndPkFld = .T.
	EXIT
  ENDIF 
ENDFOR
SELECT (lnAlias)
*E130074,1 EIH 01/02/2006 [End]


*C102282,1 ABD - Add flage to prvent user to allocate qty from this program. [Begin]
*-- This flage to detrimined if this program run into this customer.
llPikNow = .T.
IF ASCAN(laEvntTrig,PADR("LLPIKNOW",10)) <> 0
  llPikNow = .F.
ENDIF  
*C102282,1 ABD [End]

*lcBrFields = [Style:19,DESC:20:H="Description",]+;
             [lcSesDesc=gfCodDes(Season,'SEASON'):20:H="Season",lcDivDesc=gfCodDes(division,'CDIVISION'):20:H="Division",pricea]+;
             [:6:h="Price",totWip:7:h="WIP",totstk:7:h="Stock",]+;
             [totord:7:h="Orders",OTS=(TOTWIP+TOTSTK-TOTORD):7:H="O.T.S.",]+;
             [Fabric:9:h="Fabric"]

lcScFields = "Style,MAKE,lInvSty"

llNoShow  = .F.     && Flag to force the execution of the show procedure.

*B802643,1 Reham On 09/29/1999   *** Begin ***
=gfOpenFile(gcDataDir+'Codes','CCODE_NO','SH')
*B802643,1 Reham On 09/29/1999   *** End   ***

*E301077,15 Reham On 12/29/98   *** Begin ***
*E301077,15 Open the warehouse file.
=gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_Id','SH')
*E301077,15 Reham On 12/29/98   *** End   ***

*-- If entering the screen for the first time.
IF !WEXIST(gcBaseWind)
  SCATTER FIELDS &lcScFields TO laData BLANK

  lcStyHdr   = gfItemMask("HI")
  lnStyLngth = LEN(lcStyHdr)        && Var. hold the length of the style.
  
  *E301077,15 Reham On 02/22/99   *** Begin ***
  *llMultiWH  = ALLTRIM(gfGetMemVar('M_WareHouse',gcAct_Comp)) = 'Y'  && Multi warehouse
  *llDyelot   = ALLTRIM(gfGetMemVar('M_DYELOT',gcAct_Comp))    = 'Y'  && Use Dyelot or not
  laMainSetp[1,1] = 'M_WareHouse'
  laMainSetp[2,1] = 'M_DYELOT'
  
  *E301225,1 Allow Force allocation option [Begin]
  laMainSetp[3,1] = 'M_FORCEALO'
  *E301225,1 Allow Force allocation option [End  ]

  *E301310,1 Check Approved Amount [Begin]
  laMainSetp[4,1] = 'M_CHKAPROV'
  *E301310,1 Check Approved Amount [End  ]

  =gfGetMemVar(@laMainSetp , gcAct_Comp)
  llMultiWH  = ALLTRIM(laMainSetp[1,2]) = 'Y'
  llDyelot   = ALLTRIM(laMainSetp[2,2]) = 'Y'

  *E301225,1 Allow Force allocation option. [Begin]
  lcAlwForce = ALLTRIM(laMainSetp[3,2])
  *E301225,1 Allow Force allocation option. [End  ]

  *E301310,1 Check Approved Amount [Begin]
  llChkAprov = laMainSetp[4,2]
  *E301310,1 Check Approved Amount [End  ]

  *E301077,15 Reham On 02/22/99   *** End   ***
  
  *E301077,15 Reham On 12/29/98   *** Begin ***
  *E301077,15 Open the warehouse file.
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
  *E301077,15 Reham On 12/29/98   *** End   ***

  *-- Fill the warehouse array.
  IF !llMultiWH
    SELECT WAREHOUS
    GO TOP
    lcDefWare       = WAREHOUS.cWareCode
    laWareHous[1,1] = WAREHOUS.cDesc
    laWareHous[1,2] = WAREHOUS.cWareCode
  ELSE
    SELECT cDesc , cWareCode ;
      FROM WAREHOUS ;
      INTO ARRAY laWareHous
    lcDefWare = laWareHous[1,2]
  ENDIF
  STORE lcDefWare TO lcOrdWare , lcPikWare
  *B129738,1 MMR 10/13/2005 Fixing bug of not Generate 940 EDI transaction for 
  *B129738,1 MMR            pick tickets directed to 3PL providers.[Start]
  *E301077,15 Reham On 12/29/98   *** Begin ***
  *=gfCloseFile('WAREHOUS')
  *E301077,15 Reham On 12/29/98   *** End   ***
  *B129738,1 MMR.[MMR]
  *-- Get Temp. names for the screen set.
  *lcAlStyAl0  = 'CW'+gfTempName() &&SYS(2015)
  *lcAlStyAl1  = 'CW'+gfTempName() &&SYS(2015)
  *lcAlStyAl2  = 'CW'+gfTempName() &&SYS(2015)
  lcAlStyAl0  = gfTempName()
  lcAlStyAl1  = gfTempName()
  lcAlStyAl2  = gfTempName()
ENDIF
*B129738,1 MMR 10/13/2005 Fixing bug of not Generate 940 EDI transaction for 
*B129738,1 MMR            pick tickets directed to 3PL providers.[Start]
IF 'AS' $ gcCmpModules 
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
ENDIF
*B129738,1 MMR.[End]
*-- Assign the right refreshing to the objects.
lcViewStat = IIF(!EMPTY(laData[1])     , "ENABLE" , "DISABLE")
lcAloStat  = IIF(!EMPTY(OrdLine.Style) , "ENABLE" , "DISABLE")
lcEdtStat  = IIF(!EMPTY(OrdLine.Style) .AND. !EMPTY(OrdLine.PikTkt) , "ENABLE" , "DISABLE")

*C200084,1 Call function to enable/disable to call FPMAIN Once [Begin]
*lcPikStat  = IIF(!EMPTY(OrdLine.Style) .AND. (EMPTY(OrdLine.PikTkt) .OR. "*" $ OrdLine.PikTkt) , "ENABLE" , "DISABLE")
=lfPickStat()
*C200084,1 Call function to enable/disable to call FPMAIN Once [End]

lcAloStat  = IIF(!EMPTY(OrdLine.Style) .AND.  EMPTY(OrdLine.PikTkt) , "ENABLE" , "DISABLE")

*-- Save the current order for the codes file & change the order to tag codes.
*SELECT CODES
*lcCodesOrd = ORDER('CODES')
*SET ORDER TO TAG CODES

*-- Set relation between order header & line to be used in the browse.
SELECT OrdLine
SET RELATION TO cOrdType+Order INTO OrdHdr

*B603529,1 KHM 03/26/2000 (Begin) setting a relation between the ordline
*B603529,1                and the customer file in order to check the 
*B603529,1                status of the customer when allocating or picking 
*B603529,1                a line in order to not allow picking or allocating an ordline that its account is not active.
SET RELATION TO IIF(EMPTY(Store),'M'+Account,'S'+Account+Store);
                               INTO Customer ADDITIVE
*B603529,1 KHM 03/26/2000 

SELECT STYLE

*-- Define the browse bar.
PUSH MENU _MSYSMENU
DEFINE BAR 100 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 101 OF P01PU01 PROMPT lcAloBrwTl KEY ALT+B

*-- Activate the browse when selecting its bar.
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcAloBrwTl)

*-- Trap a key to enter the browse.
*B603575,1 (Begin) Unremark the following line.
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcAloBrwTl)
*B603575,1 (End)
*-- Call the screen.
SELECT Style
DO (gcScrDir+gcWinAppl+"\ALSTYAL.SPX")

*ON KEY LABEL ALT+B

*-- Release the browse window.
RELEASE WINDOW (lcAloBrwTl)
POP MENU _MSYSMENU

*-- Restore the old tag for codes file.
*SELECT CODES
*SET ORDER TO TAG &lcCodesOrd

*-- Release the relation.
SELECT OrdLine
SET RELATION TO

*-- If quitting the program erase the temp. files.
IF glQuitting
  *-- Release the browse window.
  RELEASE WINDOW (lcAloBrwTl)
ENDIF

*-- Release the pick method pad from the system menu.
RELEASE PAD _PICK OF _MSYSMENU

*!*************************************************************
*! Name      : lfActPad
*! Developer : Reham Al-Allamy
*! Date      : 10/06/1997
*! Purpose   : Bulid a new menu pad [Pick]
*!*************************************************************
*! Calls     : lpPicMethd
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActPad()
*!*************************************************************
*
FUNCTION lfActPad
*C200084,1 Make Menu Pad Under Pick Flag [Begin]
llPickOpt = .T.  && Pick Option is in menu (Default Value)

IF ASCAN(laEvntTrig,PADR("STYPKSTAT",10)) <> 0
  =gfDoTriger('ALSTYAL',PADR("STYPKSTAT",10))
ENDIF

*C102282,1 ABD - Remark the next line and allow force alloction if 
*C102282,1 ABD - The llPinNow flage is .T. [Begin]
*IF llPickOpt
IF llPickOpt .AND. llPikNow
*C102282,1 ABD - [End]

  DEFINE PAD _PICK OF _MSYSMENU PROMPT '\<Pick' KEY ALT+P , ' '
  SET SKIP OF PAD _PICK OF _MSYSMENU (laScrMode[1])
  ON PAD _PICK OF _MSYSMENU ACTIVATE POPUP _PICKPOP

  DEFINE POPUP    _PICKPOP MARGIN SHADOW
  DEFINE BAR 1 OF _PICKPOP PROMPT 'Pick Automatically'
  DEFINE BAR 2 OF _PICKPOP PROMPT 'Pick Manualy'
  SET MARK OF  BAR 1 OF _PICKPOP TO .T.
  ON SELECTION POPUP    _PICKPOP DO lpPicMethd
ENDIF  
*C200084,1 Make Menu Pad Under Pick Flag [Begin]
*-- end of lfActPad.

*!*************************************************************
*! Name      : lpPicMethd
*! Developer : Reham Al-Allamy
*! Date      : 10/06/1997
*! Purpose   : Choose the picking tickets method.
*!           : ( Automatically Or Manulay )
*!*************************************************************
*! Calls     : lpPicMethd
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpPicMethd
*!*************************************************************
*
FUNCTION lpPicMethd

IF BAR() = lnPicMethd
  RETURN
ENDIF

SET MARK OF BAR lnPicMethd OF _PICKPOP TO .F.
lnPicMethd = BAR()
SET MARK OF BAR lnPicMethd OF _PICKPOP TO .T.

*!*************************************************************
*! Name      : lpShow
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
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
SELECT STYLE

laCtrStat[7]  = "DISABLE"    && Edit   button
laCtrStat[8]  = "DISABLE"    && Delete button
DO CASE
  *-- Select Mode.
  CASE laScrMode[1]
    *-- Initialize all the data needed for a select mode.
    STORE .F. TO llBrowse
    STORE "DISABLE" TO lcAloStat , lcViewStat , lcEdtStat , lcPikStat , lcAloStat
    STORE " " TO lcSty_Desc , lcOldValue
    STORE 0   TO lnTotStk , lnTotAlo , lnTotAvl , lnTotWip , lnTotUnalo  , lnTotOrd
    STORE lcDefWare TO lcOrdWare , lcPikWare
    STORE 1 TO puOrdWare , puPikWare
    
    *-- Call the browse command to refresh the browse.
    =lfwBrowOrd()
    
    _CUROBJ = OBJNUM(laData[1])
    *E500304,1 WAB - show the O.T.S butt. disable in select mode
    *E500304,1 WAB - START
    SHOW GET pbPbEx11 DISABLE
    *E500304,1 WAB - END
  *-- View Mode.
  CASE laScrMode[2]
    *-- Get the main data to be displayed.
    SELECT STYLE
    lcSty_Desc = IIF(!EMPTY(laData[1]),STYLE.Desc,"")
    laData[2]  = Style.Make
    laData[3]  = Style.lInvSty
    
    IF llMultiWH
      *-- Get the default warehouse for the current style.
      lcOrdWare = IIF(EMPTY(lcOrdWare) .OR. !SEEK(PADR(laData[1],19)+lcOrdWare+SPACE(10) , 'StyDye') , STYLE.cDefWare , lcOrdWare)
      lcPikWare = IIF(EMPTY(lcPikWare) .OR. !SEEK(PADR(laData[1],19)+lcPikWare+SPACE(10) , 'StyDye') , STYLE.cDefWare , lcPikWare)
    ELSE
      *-- If single warehouse use the only existing warehouse.
      STORE lcDefWare TO lcOrdWare , lcPikWare
    ENDIF
    
    *B804033,1 AMM get setting of exact and set it to on to reach the exact element
    lcSetEx = SET("EXACT")
    SET EXACT ON
    *B804033,1 AMM end
    *-- Update the warehouse popups with the right values.
    *puOrdWare = IIF(ASCAN(laWareHous,lcOrdWare) > 0 , ASCAN(laWareHous,lcOrdWare)/2 , 0)
    *puPikWare = IIF(ASCAN(laWareHous,lcPikWare) > 0 , ASCAN(laWareHous,lcPikWare)/2 , 0)
    puOrdWare = IIF(ASCAN(laWareHous,lcOrdWare) > 0 , ASUBSCRIPT(laWareHous,ASCAN(laWareHous,lcOrdWare),1) , 0)
    *B604759,1 TMI [Start] comment this line , it is calculated in lfwBrwRef
    *puPikWare = IIF(ASCAN(laWareHous,lcPikWare) > 0 , ASUBSCRIPT(laWareHous,ASCAN(laWareHous,lcPikWare),1) , 0)
    *B604759,1 TMI [End  ] 
    *B804033,1 AMM return setting of exact as it was.
    SET EXACT &lcSetEx
    *B804033,1 AMM end
    
    *-- Get the inventory totals.
    *B604759,1 TMI [Start] Move this line after calling lfwBrwRef,to get correct puPikWare
    ***=lfSayTotal()
    *B604759,1 TMI [End  ] 

    *-- Call the when browse.
    =lfwBrowOrd()
    
    *B802134,1 Reham On 05/24/1999   *** Begin ***
    *B802134,1 Restore the record pointer in the order lines browse.
    IF lnCurMark > 0 .AND. lnCurMark <= RECCOUNT("OrdLine")
      GO lnCurMark IN OrdLine
    ENDIF
    =lfwBrwRef()   
    *B802134,1 Reham On 05/24/1999   *** End   ***
    
    *-- Adjust the enabling & disabling of the processes buttons.
    *B604759,1 TMI [Start] Comment these lines since the comming variables are updated in lfwBrwRef
    * lcViewStat = IIF(!EMPTY(laData[1])     , "ENABLE" , "DISABLE")
    * lcAloStat  = IIF(!EMPTY(OrdLine.Style) , "ENABLE" , "DISABLE")
    * lcEdtStat  = IIF(!EMPTY(OrdLine.Style) .AND. !EMPTY(OrdLine.PikTkt) , "ENABLE" , "DISABLE")

    *C200084,1 Call function to enable/disable to call FPMAIN Once [Begin]
    **lcPikStat  = IIF(!EMPTY(OrdLine.Style) .AND. (EMPTY(OrdLine.PikTkt) .OR. "*" $ OrdLine.PikTkt) , "ENABLE" , "DISABLE")
    *=lfPickStat()
    *C200084,1 Call function to enable/disable to call FPMAIN Once [End]

    * lcAloStat  = IIF(!EMPTY(OrdLine.Style) .AND. EMPTY(OrdLine.PikTkt) , "ENABLE" , "DISABLE")
    *B604759,1 TMI [End  ] 

    *E500304,1 WAB - show the O.T.S butt. ENABLE
    *E500304,1 WAB - START
    SHOW GET pbPbEx11 ENABLE
    *E500304,1 WAB - END
    
ENDCASE

*-- Refresh the Objects.
SHOW GET lcSty_Desc


SHOW GET puOrdWare  &lcViewStat
SHOW GET puPikWare  &lcViewStat
*C102282,1 ABD - Show the pick button disable if llPikNow is true .[Begin]
IF llPikNow
  *C102282,1 ABD - [End]
  SHOW GET pbPick     &lcPikStat
  *C102282,1 ABD - Else for if statment. [Begin]
ELSE
  SHOW GET pbPick   DISABLE
ENDIF
*C102282,1 ABD - [End]

SHOW GET pbAloct    &lcAloStat
SHOW GET pbLink     &lcEdtStat
SHOW GET pbReles    &lcEdtStat
SHOW GET pbEdtLin   &lcEdtStat
SHOW GET pbEdt      DISABLE
SHOW GET pbDlt      DISABLE
SET SKIP OF PAD _PICK OF _MSYSMENU (laScrMode[1])

SELECT STYLE

*!*************************************************************
*! Name      : lfwOldCVal
*! Developer : Reham Al-allamy
*! Date      : 07/20/1997
*! Purpose   : Function to store old value of the current string filed.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfwOldCVal()
*!*************************************************************
*
FUNCTION lfwOldCVal

*-- Save the old value of current object.
lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Validate the passed style.
*!*************************************************************
*! Calls     : gfStyBrow, lfwBrowOrd, lfSayTotal
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvData_1()
*!*************************************************************
*
FUNCTION lfvData_1

*-- If press the browse icon or entered new value.
IF llBrowse .OR. !(lcOldValue == laData[1])
  SELECT STYLE
  IF !SEEK(PADR(laData[1],19)) .OR. llBrowse
    *B802643,1 Reham On 09/29/1999   *** Begin ***
    *B802643,1 Enable the soft seek in the style browse.
    *-- Call the style browse.
    *laData[1] = gfStyBrw("I" , "" , "" , .F.)
    IF !USED("SCALE")
      =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
    ENDIF  

    laData[1] = gfStyBrw("I" , laData[1] , "" , .F.)
    *B802643,1 Reham On 09/29/1999   *** End   ***
  ENDIF
  
  IF !WEXIST(lcAloBrwTl)
     =lfwBrowOrd()
  ENDIF

  *-- Refresh the objects.
  SHOW GET laData[1]
  *-- Get the descriptions.
  lcSty_Desc = IIF(!EMPTY(laData[1]),STYLE.Desc,"")
  SHOW GET lcSty_Desc
  *-- If change the style.
  IF !(lcOldValue = laData[1])
    STORE lcDefWare TO lcOrdWare , lcPikWare
    STORE 1 TO puOrdWare , puPikWare
    IF !EMPTY(laData[1])
      laScrMode    = .F.
      laScrMode[2] = .T.
      SHOW GETS
    ENDIF
  ENDIF
  *-- Get the inventory totals.
  =lfSayTotal()
ENDIF
llBrowse = .F.

lcViewStat = IIF(!EMPTY(laData[1]) , "ENABLE" , "DISABLE")
SHOW GET puOrdWare  &lcViewStat
SHOW GET puPikWare  &lcViewStat

SELECT STYLE

*!*************************************************************
*! Name      : lfvOrdWare
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Validate the Order Warehouse
*!*************************************************************
*! Calls     : lfwBrowOrd
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvOrdWare()
*!*************************************************************
*
FUNCTION lfvOrdWare

IF !(lcOldValue == puOrdWare)
  IF !SEEK(PADR(laData[1],19)+laWareHous[puOrdWare,2]+SPACE(10),'StyDye')
    *** Style : ALLTRIM(laData[1]) ***
    *** is not assigned to"+" warehouse : ALLTRIM(laWareHous[puOrdWare,2])   ***
    *** <  Ok  > ***
    lcTmpStr = ALLTRIM(laData[1]) + "|" + ALLTRIM(laWareHous[puOrdWare,2]) + "-"+ ALLTRIM(laWareHous[puOrdWare,1])
    =gfModalGen("INM44012B00000" , "DIALOG" , lcTmpStr)
    puOrdWare = lcOldValue
    SHOW GET puOrdWare
    RETURN
  ENDIF
  lcOrdWare = laWareHous[puOrdWare,2]
  SHOW GET puOrdWare
  
  *-- Call the when browse.
  =lfwBrowOrd()
  *B604759,1 TMI [Start] call the when browse 
  IF lnCurMark > 0 .AND. lnCurMark <= RECCOUNT("OrdLine")
    GO lnCurMark IN OrdLine  
  ENDIF  
  =lfwBrwRef()
  *B604759,1 TMI [End  ] call the when browse 
ENDIF

SELECT STYLE

*!*************************************************************
*! Name      : lfvPikWare
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Validate the Picked From Warehouse
*!*************************************************************
*! Calls     : gpAdStyWar, lfSayTotal
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPikWare()
*!*************************************************************
*
FUNCTION lfvPikWare

IF !(lcOldValue == puPikWare)
  *-- Do not accept any empty or invalid warehouse.
  IF !SEEK(PADR(laData[1],19)+laWareHous[puPikWare,2]+SPACE(10),'StyDye')
    *** Style: ALLTRIM(laData[1]) ***
    *** is not assigned to"+" warehouse : ALLTRIM(laWareHous[puPikWare,2]) ***
    *** <  Add  > - < Cancel > ***
    lcTmpStr = ALLTRIM(laData[1]) + "|" + ALLTRIM(laWareHous[puPikWare,2]) + "-" + ALLTRIM(laWareHous[puPikWare,1])
    IF gfModalGen("INM44012B44001" , "DIALOG" , lcTmpStr) = 1
      *-- Call global function to assign the selected 
      *-- style to the picked warehouse.
      lcPikWare = laWareHous[puPikWare,2]
      DO gpAdStyWar WITH PADR(laData[1],19) , SPACE(10) , lcPikWare
    ELSE
      puPikWare = lcOldValue
      SHOW GET puPikWare
      RETURN
    ENDIF
  ENDIF

  lcPikWare = laWareHous[puPikWare,2]
  SHOW GET puPikWare
  
  *-- Get the inventory totals.
  =lfSayTotal()
ENDIF

SELECT STYLE

*!*************************************************************
*! Name      : lfSayTotal
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Function to get the inventory totals.
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSayTotal()
*!*************************************************************
*
FUNCTION lfSayTotal

*-- Choose the file to get the right inventory totals according
*-- to the multi warehouse flag.
lcSelFile = "STYDYE"

*-- According to the multi warehouse flag, choose the right seek expression.
*B604759,1 TMI [Start] The variable lcPickWare is not yet updated ,so replace it 
*B604759,1 TMI         in the seek expr with laWareHous[puPikWare]
*lcSeekExp = "PADR(laData[1],19)+lcPikWare+SPACE(10)"
lcSeekExp = "PADR(laData[1],19)+laWareHous[puPikWare,2]+ORDLINE.DYELOT"  && <tmi> instead of "Space(10)" after last "+"
*B604759,1 TMI [End  ] 
IF SEEK(&lcSeekExp , lcSelFile)
  *-- If found style in the file, Get the inventory totals.
  lnTotStk   = &lcSelFile..TotStk
  lnTotAlo   = &lcSelFile..TotAlo
  *lnTotAvl   = IIF(llMultiWH , &lcSelFile..TotStk , &lcSelFile..TotStk+&lcSelFile..TotWip)
  *E500304,1 WAB - call function to calculate the total available qty
  *E500304,1 WAB - START
  *lnTotAvl   = &lcSelFile..TotStk
  lnTotAvl   = lfTotAvlbl('9')
  *E500304,1 WAB - END
  *B604759,1 TMI [Start] Relocate the pointer in the file stydye
  *lnTotWip   = &lcSelFile..TotWip  &&  commented by tmi , <lnTotWip> will be assigned in lfTotAvlbl fn.
  =SEEK(&lcSeekExp , lcSelFile)
  *B604759,1 TMI [End  ] Relocate the pointer in the file stydye 
  lnTotUnalo = &lcSelFile..TotStk - &lcSelFile..TotAlo
  *B605047,1 TMI [Start] make the ordqty depend on the OrdWarehous
  IF SEEK(PADR(laData[1],19)+lcOrdWare+SPACE(10) , lcSelFile)
  *B605047,1 TMI [End  ] 
    lnTotOrd = &lcSelFile..TotOrd
  *B605047,1 TMI [Start] 
  ENDIF
  *B605047,1 TMI [End  ] 
ELSE
  STORE 0 TO lnTotStk , lnTotAlo , lnTotAvl , lnTotWip , lnTotUnalo  , lnTotOrd  
ENDIF

*-- Refresh the totals.
=lfRefresh()

*!*************************************************************
*! Name      : lfwBrowOrd
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Function call the browse called from main screen
*!           : Setup.
*!*************************************************************
*! Calls     : lfwBrwRef, lfvBrows
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwBrowOrd()
*!*************************************************************
*
FUNCTION lfwBrowOrd
*_CUROBJ=OBJNUM(pbCls)
*-- Display the transactions lines in the browse from the temp. files
SELECT OrdLine
lnMarker = RECNO()

*B603529,1 KHM 03/26/2000 (Begin) Changing the browse field by adding the
*B603529,1                pick from warehouse and dyelot. Changing the 
*B603529,1                lnDummy to lnDumy because lnDummy is used in 
*B603529,1                the traping function.
*lcAloBrFld = 'cMarker=IIF(RECNO()=lnMarker ,">"," "):H=" ":R:1:W=.F. ,'+;
       'OrdHdr.Order :H="Order#":9 , OrdHdr.Account :H="Acct." :9 ,'+;
       'OrdLine.Store :H="Store":14 , OrdLine.Complete :H="Complete":10 ,'+;
       'OrdHdr.Status :H="S":2 , OrdLine.TotQty :H="OrdQty":9 ,'+;
       'OrdLine.PikTkt :H="PikTkt":10 , OrdLine.Price :H="Price":11 ,'+;
       'OrdLine.TotPik :H="PikQty":8 ,'+;
       'lnDummy = OrdLine.TotQty - OrdLine.TotPik :H="Unallo.Qty":12'
lcAloBrFld = 'cMarker=IIF(RECNO()=lnMarker ,">"," "):H=" ":R:1:W=.F. ,'+;
       'OrdHdr.Order :H="Order#":8 , OrdHdr.Account :H="Acct." :7 ,'+;
       'OrdLine.Store :H="Store":14 ,OrdLine.cWareCode:H="Pick from":14,'+;
       'OrdLine.Dyelot:H="Dyelot":14, OrdLine.Complete :H="Complete":10 ,'+;
       'OrdHdr.Status :H="S":2 , OrdLine.TotQty :H="OrdQty":8 ,'+;
       'OrdLine.PikTkt :H="PikTkt":7, OrdLine.Price :H="Price":10,'+;
       'OrdLine.TotPik :H="PikQty":8 ,'+;
       'lnDumy=(OrdLine.TotQty-OrdLine.TotPik):H="Unallo.Qty":10'
*B603529,1 KHM 03/26/2000 (End)

*B602904,1 Reham On 05/16/1999   *** Begin ***
*B602904,1 Define flag to know if browse from order line file or browse empty.

*C200084,1 FP Make all locate and browse depends on variables [Begin]
*C200084,1 So we can call FPMain to change variables only.
lcForExpr  = "Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = PADR(laData[1],19)"

*B603464,1 KHM 02/21/2000 (Begin) Changing the filter expression.
*lcWareExpr = [OrdLine.cWareCode = lcOrdWare]
lcWareExpr = [OrdHdr.cWareCode = lcOrdWare]
*B603464,1 KHM 02/21/2000 (End)


*B603719,1 HBG 07/03/2000 The EDI temp order shouldn't show in the browes of the orders 
*B603719,1                to be allocated. [Begin]
*lcBForExpr = [OrdHdr.Status $ "OH" AND OrdLine.TotQty <> 0 ]
lcBForExpr = [OrdHdr.Status $ "OH" AND TotQty <> 0 AND cOrdType <> 'T']

*B603719,1 HBG 07/03/2000 The EDI temp order shouldn't show in the browes of the orders 
*B603719,1                to be allocated. [End  ]

IF ASCAN(laEvntTrig,PADR("ADJBROW",10)) <> 0
  =gfDoTriger('ALSTYAL',PADR("ADJBROW",10))
ENDIF

lcWhilExpr = lcBForExpr + [ AND ] + lcWareExpr
*C200084,1 FP Make all locate and browse depends on variables [End  ]

llLinFound = .F.
*B602904,1 Check if the style exists in the order line file.
IF SEEK(PADR(laData[1],19) , "OrdLine")
  *B602904,1 Locate if there is at least 1 order line match the criteria of including
  *B602904,1 the current style, current order location and the order line qty. not
  *B602904,1 equal zero & the order header status open or on hold.

  *C200084,1 Change Locate Expression. [Begin]
  *LOCATE REST WHILE OrdLine.style = PADR(laData[1],19) ;
  *              FOR (OrdHdr.Status = 'O' .OR. OrdHdr.Status = 'H') .AND. ;
  *                   OrdLine.TotQty <> 0 .AND. OrdLine.cWareCode = lcOrdWare

  *B802643,1 Reham On 09/29/1999    *** Begin ***
  *B802643,1 1- Display the proper orders upon selecting new style.
  *LOCATE FOR &lcForExpr REST WHILE &lcWhilExpr
  LOCATE REST WHILE &lcForExpr FOR &lcWhilExpr
  *C200084,1 Change Locate Expression. [End  ]
  *B802643,1 Reham On 09/29/1999    *** End   ***
  
  *B602904,1 If there is at least 1 order line found, set the flag to true.
  llLinFound = IIF(FOUND() , .T. , .F.)
ELSE
  *B602904,1 If there is no orders for the current style, set the flag to false.
  llLinFound = .F.
ENDIF

*B802134,1 Reham On 05/24/1999   *** Begin ***
*B802134,1 Define variable hold the record pointer in the order lines browse.
*B604759,1 TMI [Start] if no line found let lnCurMark be 0
*lnCurMark = IIF(llLinFound , RECNO("OrdLine") , lnMarker)
lnCurMark = IIF(llLinFound , RECNO("OrdLine") , 0)
*B604759,1 TMI [End  ] if no line found let lnCurMark be 0
*B802134,1 Reham On 05/24/1999   *** End   ***

*-- This is the browse command to display the orders with the proper fields.
*IF llFrstRun
*B602904,1 If entering the screen for 1st time, the current style is empty or there is
*B602904,1 no order lines for the current style in the current order location.
IF llFrstRun .OR. EMPTY(laData[1]) .OR. !llLinFound
*B602904,1 Reham On 05/16/1999   *** End   ***
  *-- Empty browse for first time.
  llFrstRun = .F.
  BROWSE FIELDS &lcAloBrFld ;
     KEY CHR(255) ;
     WINDOW  (lcAlStyAl1) ;
     IN WINDOW (gcBaseWind) ;
     NOMENU           ;
     NOEDIT           ;
     NOAPPEND         ;
     NODELETE         ;
     NOWAIT           ;
     SAVE             ;
     NOCLEAR          ;
     LOCK 0           ;
     WHEN lfwBrwRef() ;
     VALID :F lfvBrows() ;
     TITLE lcAloBrwTl
ELSE
  IF !EMPTY(lcOrdWare)
    *-- Browse for the selected warehouse.

    *C200084,1 Add For Condition instead of direct write code [Begin]
    *BROWSE FIELDS &lcAloBrFld ;
    *   KEY PADR(laData[1],19) ;
    *   FOR (OrdHdr.Status = 'O' .OR. OrdHdr.Status = 'H') .AND. ;
    *        OrdLine.TotQty <> 0 .AND. OrdLine.cWareCode = lcOrdWare ;
    *   WINDOW  (lcAlStyAl1) ;
    *   IN WINDOW (gcBaseWind) ;
    *   NOMENU           ;
    *   NOEDIT           ;
    *   NOAPPEND         ;
    *   NODELETE         ;
    *   NOWAIT           ;
    *   SAVE             ;
	*   NOCLEAR          ;
    *   LOCK 0           ;
    *   WHEN lfwBrwRef() ;
    *   VALID :F lfvBrows() ;
    *   TITLE lcAloBrwTl

    BROWSE FIELDS &lcAloBrFld ;
       KEY PADR(laData[1],19) ;
       FOR &lcWhilExpr        ;
       WINDOW  (lcAlStyAl1)   ;
       IN WINDOW (gcBaseWind) ;
       NOMENU           ;
       NOEDIT           ;
       NOAPPEND         ;
       NODELETE         ;
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       LOCK 0           ;
       WHEN lfwBrwRef() ;
       VALID :F lfvBrows() ;
       TITLE lcAloBrwTl

    *C200084,1 Add For Condition instead of direct write code [End  ]

  ELSE

    *C200084,1 Add For Condition instead of direct write code [Begin]
    *BROWSE FIELDS &lcAloBrFld ;
    *   KEY PADR(laData[1],19) ;
    *   FOR (OrdHdr.Status = 'O' .OR. OrdHdr.Status = 'H') .AND. ;
    *       (OrdLine.TotQty <> 0) ;
    *   WINDOW  (lcAlStyAl1) ;
    *   IN WINDOW (gcBaseWind) ;
    *   NOMENU           ;
    *   NOEDIT           ;
    *   NOAPPEND         ;
    *   NODELETE         ;
    *   NOWAIT           ;
    *   SAVE             ;
	*   NOCLEAR          ;
    *   LOCK 0           ;
    *   WHEN lfwBrwRef() ;
    *   VALID :F lfvBrows() ;
    *   TITLE lcAloBrwTl

    BROWSE FIELDS &lcAloBrFld ;
       KEY PADR(laData[1],19) ;
       FOR &lcBForExpr        ;
       WINDOW  (lcAlStyAl1)   ;
       IN WINDOW (gcBaseWind) ;
       NOMENU           ;
       NOEDIT           ;
       NOAPPEND         ;
       NODELETE         ;
       NOWAIT           ;
       SAVE             ;
	   NOCLEAR          ;
       LOCK 0           ;
       WHEN lfwBrwRef() ;
       VALID :F lfvBrows() ;
       TITLE lcAloBrwTl

    *C200084,1 Add For Condition instead of direct write code [End  ]
  ENDIF
  *=SEEK(PADR(laData[1],19) , "ORDLINE")
ENDIF

SHOW WINDOW (lcAloBrwTl) REFRESH
=lfwBrwRef()
SELECT STYLE

*!*************************************************************
*! Name      : lfwBrwRef
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Function to refresh the modify button according 
*!           : to the existance of the pick ticket.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwBrwRef()
*!*************************************************************
*
FUNCTION lfwBrwRef

*-- Change the record pointer.
lnMarker   = RECNO("ORDLINE")
SHOW WINDOW (lcAloBrwTl) REFRESH SAME

*glFromBrow = .T.

*-- Change the status of the process buttons.
lcEdtStat = IIF(!EMPTY(OrdLine.PikTkt) .AND. !EMPTY(OrdLine.Style) , "ENABLE" , "DISABLE")

*C200084,1 Call function to enable/disable to call FPMAIN Once [End]
*lcPikStat = IIF((EMPTY(OrdLine.PikTkt) .OR. "*" $ OrdLine.PikTkt) .AND. !EMPTY(OrdLine.Style) , "ENABLE" , "DISABLE")
=lfPickStat()
*C200084,1 Call function to enable/disable to call FPMAIN Once [End]

lcAloStat = IIF( EMPTY(OrdLine.PikTkt) .AND. !EMPTY(OrdLine.Style) , "ENABLE" , "DISABLE")

*B603464,1 KHM 02/21/2000 (Begin) Refreshing the pick from warehouse
*B603464,1                according to the one in the ordline.
lnElem     = ASCAN(laWareHous, OrdLine.cWareCode)
*B604759,1 TMI [Start] if puPikWare not found , let it equal to puOrdWare
*puPikWare  = IIF(lnElem = 0 , 0 , ASUBSCRIPT(laWareHous, lnElem, 1))
puPikWare  = IIF(lnElem = 0 , puOrdWare , ASUBSCRIPT(laWareHous, lnElem, 1))
*B604759,1 TMI [End  ] 
SHOW GET puPikWare
*B604851,1 (Begin) Get the order warehouse code.
lcPikWare = ORDLINE.CWARECODE
*B604851,1 (End)
*B603464,1 KHM 02/21/2000  (End)

*-- Refresh the process buttons.
SHOW GET pbLink    &lcEdtStat
SHOW GET pbEdtLin  &lcEdtStat
SHOW GET pbReles   &lcEdtStat

*C102282,1 ABD - The llPinNow flage is .T. [Begin]
IF llPikNow
*C102282,1 ABD - [End]
  SHOW GET pbPick    &lcPikStat
  *C102282,1 ABD - Else for if statment. [Begin]
ELSE
  SHOW GET pbPick     DISABLE
ENDIF
*C102282,1 ABD - [End]

*B604759,1 TMI [Start] calculate totals
=lfSayTotal()
*B604759,1 TMI [End  ] 

SHOW GET pbAloct   &lcAloStat

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Reham Al-Allamy
*! Date      : 04/27/1997
*! Purpose   : Valid function for the browse.
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

IF !WONTOP(lcAloBrwTl)
  glFromBrow = .T.
  =gfStopBrow()
ELSE
*  IF !WEXIST(gcBaseWind) 
*    glFromBrow = .F.
*    glQuitting = .T.
*    CLEAR READ
*    KEYBOARD CHR(13)
*    RETURN TO ALSTYAL.SPR
*  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfActMain
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Activate function for ALO500
*!*************************************************************
*! Calls     : lfClrTrap, gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActMain()
*!*************************************************************
*
FUNCTION lfActMain

*-- Set a hot key for the browse if outside the browse.
*ON KEY LABEL ALT+B ACTIVATE WINDOW (lcAloBrwTl)
*=lfClrTrap()

*-- Clear browse as you are coming out of a browse window
IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF  

IF WONTOP() <> lcAloBrwTl
  =lfClrTrap()
ENDIF

*!*************************************************************
*! Name      : lfClrTrap
*! Developer : Reham Al-Allamy
*! Date      : 05/25/1997
*! Purpose   : Clear the trapped key.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfClrTrap()
*!*************************************************************
*
FUNCTION lfClrTrap

*-- Clear all the trapped keys.
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+M
ON KEY LABEL CTRL+E
ON KEY LABEL CTRL+P
ON KEY LABEL CTRL+A
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL Ctrl+ENTER
ON KEY LABEL Ctrl+HOME
ON KEY LABEL Ctrl+END

*!*************************************************************
*! Name      : lfDactMain
*! Developer : Reham Al-allamy
*! Date      : 07/20/1997
*! Purpose   : Deactivate function for ALO500
*!*************************************************************
*! Calls     : lfClrTrap
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDactMain()
*!*************************************************************
*
FUNCTION lfDactMain

*-- If deactivate the main screen to be inside the browse.
*-- 1- Set hot keys for the process buttons.
*-- 2- Trap the main keys : Tab - BackTab.
*ON KEY LABEL ALT+B

IF WONTOP() = lcAloBrwTl
  ON KEY LABEL CTRL+Q  lnDummy = 1
  ON KEY LABEL CTRL+W  lnDummy = 1
  ON KEY LABEL CTRL+M  DO lpTrap WITH lcALStyAl2 , 'pbEdtLin'  , IIF(lcEdtStat = "ENABLE" , .T. , .F.)
  ON KEY LABEL CTRL+E  DO lpTrap WITH lcALStyAl2 , 'pbReles'   , IIF(lcEdtStat = "ENABLE" , .T. , .F.)
  ON KEY LABEL CTRL+P  DO lpTrap WITH lcALStyAl2 , 'pbPick'    , IIF(lcPikStat = "ENABLE" , .T. , .F.)
  ON KEY LABEL CTRL+A  DO lpTrap WITH lcALStyAl2 , 'pbAloct'   , IIF(lcAloStat = "ENABLE" , .T. , .F.)
  ON KEY LABEL TAB     DO lpTrap WITH lcALStyAl2 , 'pbPick'    , .F.
  ON KEY LABEL BACKTAB DO lpTrap WITH lcALStyAl0 , 'puPikWare' , .F.
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  glFromBrow = .T.
ELSE
  =lfClrTrap()  
  glFromBrow = .F.
ENDIF
*RETURN .F.

*!*************************************************************
*! Name      : lpTrap 
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Trap of tab key or backtab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lcWindName -> Window name to be activated.
*!             lcObjName  -> Current object.
*!             llEnter    -> Press enter or not.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTrap  WITH 'Window name','Current Object','Press Enter'
*!*************************************************************
*
PROCEDURE lpTrap 
PARAMETERS lcWindName, lcObjName , llEnter
PRIVATE lcWindName, lcObjName , llEnter

*-- Activate the window first then point the current object to the object
*-- sent as a parameter..
ACTIVATE WINDOW (lcWindName)
_CUROBJ = OBJNUM(&lcObjName)

*-- If send enter , execute the validation of the current button.
IF llEnter
  KEYBOARD "{ENTER}" CLEAR
ENDIF

*!*************************************************************
*! Name      : lfvLink
*! Developer : Reham Al-allamy
*! Date      : 07/20/1997
*! Purpose   : Valid function for push button < Link To > (pbLink).
*!*************************************************************
*! Calls              :  lfClrTrap, GetObj 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvBrowse()
*!*************************************************************
*
FUNCTION lfvLink

*-- Clear the trapped keys.
ON KEY LABEL ALT+B
=lfClrTrap()

*-- Call the object linking program with parameter "T" for picking tickets.
DO GetObj WITH 'T' , OrdLine.PikTkt

*-- Restore the trapped keys.
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcAloBrwTl)

SELECT STYLE

*!*************************************************************
*! Name      : lfvProcess
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Function to validate the Edit button
*!*************************************************************
*! Calls     : lfRecLock, lfPikAlo, lfAloLine, lfModiLine
*!           : lfRelLine, lfSayTotal, lfwBrwRef
*!*************************************************************
*! Passed Parameters  :  lnProcess -> 1 for Picking
*!                                    2 for Modifying
*!                                    3 for release
*!                                    4 for allocate
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvProcess(lnProcess)
*!*************************************************************
*
FUNCTION lfvProcess
PARAMETERS lnProcess
PRIVATE lnProcess , lnChoise
*-- lnProcess = 1 -> Pick a line.
*-- lnProcess = 2 -> Edit a line.
*-- lnProcess = 3 -> Release a line.
*-- lnProcess = 4 -> Allocate without generating pick ticket.
*-- If the pick button was pressed & there is no quantities to allocate.
SELECT OrdLine
*B603529,1 KHM 03/26/2000 (Begin) If the process is Allocate or Pick then
*B603529,1                check the status of the customer if it was 
*B603529,1                non-active customer do not allow allocating or picking.
IF (lnProcess = 1 .OR. lnProcess = 4) AND Customer.Status <> 'A'
  DO CASE
    CASE Customer.Status = 'H'
      lcStatus = 'on Hold' + '|' + IIF(lnProcess=1,'Cannot Pick','Cannot Allocate')
    CASE Customer.Status = 'X'
      lcStatus = 'on Cancel' + '|' + IIF(lnProcess=1,'Cannot Pick','Cannot Allocate')
    CASE Customer.Status = 'P'
      lcStatus = 'on Potential' + '|' + IIF(lnProcess=1,'Cannot Pick','Cannot Allocate')
  ENDCASE
  =gfModalGen("INM44098B00000" , "DIALOG" , lcStatus)
  RETURN
ENDIF
*B603529,1 KHM 03/26/2000 (End) 

*B803380,1 HBG 07/03/2000 If user trying to pick or allocate an order with status on hold
*B803380,1                Get a message :
*B803380,1                "Order status on hold! continue?"
*B803380,1                <Yes><No>
IF (lnProcess = 1 .OR. lnProcess = 4) AND Ordhdr.Status = 'H'
  lnChoise = gfModalGen("INM44096B44009" , "DIALOG" , Order )
  *-- If user chose "No" return.
  IF lnChoise = 2
    RETURN
  ENDIF 
ENDIF
*B803380,1 HBG 07/03/2000 [End] 

IF (lnProcess = 1 .OR. lnProcess = 4) .AND. OrdLine.TotQty = 0
  *** No pieces found on order.  Cannot allocate! ***
  *** <  Ok  > ***
  =gfModalGen("INM44013B00000" , "DIALOG")
  SELECT STYLE
  RETURN
ENDIF

*** Are you sure you want to release this picking ticket? ***
*** <  Yes  > - <  No  > ***
*B803667,1 HBG 09/21/2000 Diferent messages if releasing allocated 
*B803667,1                order line or releasing Pick tiket [Begin]
*IF lnProcess = 3 .AND. gfModalGen("QRM44023B00006" , "DIALOG") = 2
IF lnProcess = 3  
  PRIVATE lcRelMesge
  lcRelMesge = IIF(ORDLINE.PIKTKT = "******","QRM44103B00006","QRM44023B00006")
  IF gfModalGen(lcRelMesge, "DIALOG") = 2
  *B803667,1 [End]  
    SELECT STYLE
    RETURN
  *B803667,1 HBG 09/21/2000 End if Releasing allocated order line or Pick tiket [Begin]
  ENDIF  
  *B803667,1 [End]  
ENDIF

lcDyelot = OrdLine.Dyelot
lcPikTkt = OrdLine.PikTkt

*-- Lock All necessary records.
SET MULTILOCKS ON

*-- Get the pick for the picked warehouse.
llModiWH  = IIF(SEEK(PADR(laData[1],19)+lcPikWare+SPACE(10),'StyDye'),;
                RLOCK('StyDye') .OR. lfRecLock('StyDye', 'Style Dyelot' ),.T.)

*-- Check if update the dyelot record or not and lock it.
llModiDye = IIF(llDyelot .AND. Style.cDye_Flg = 'Y' .AND. SEEK(PADR(laData[1],19)+lcPikWare+lcDyelot,'StyDye') , .T. , .F.)

*-- Lock the needed records in different files.
llLock0 = IIF(llModiDye , RLOCK('StyDye') .OR. lfRecLock('StyDye' , 'Style Dyelot') , .T.)
llLock1 = RLOCK('Style')   .OR. lfRecLock('Style' , 'Style')
llLock2 = RLOCK('OrdHdr')  .OR. lfRecLock('OrdHdr' , 'Order Header')
llLock3 = RLOCK('OrdLine') .OR. lfRecLock('OrdLine' , 'Order Line')
llLokedAll = llModiWH .AND. llLock0 .AND. llLock1 .AND. llLock2 .AND. llLock3

*-- If we could not lock all the required records.
IF !llLokedAll
  *-- Unlock all the locked files.
  SET MULTILOCKS OFF
  *** Unable to lock the needed files.  Cannot process this line. ***
  *** <  Ok  > ***
  =gfModalGen("INM44014B00000" , "DIALOG")
ELSE
  DO CASE
    CASE lnProcess = 1 .OR. lnProcess = 4
      *-- If picking a previous allocated picking ticket, just assign
      *-- a picking ticket # & add it to the piktkt file.
      IF lnProcess = 1 .AND. "*" $ lcPikTkt
        =lfPikAlo()
      ELSE
        *-- Call local function to allocate the current line.

        *B039660,1 NNA 09/04/2005 Check first if there is enough stock or not before making update in the Bin location Files
        IF ASCAN(laEvntTrig,PADR("CHKFIRST",10)) <> 0 AND !gfDoTriger("ALSTYAL",PADR("CHKFIRST",10))
          RETURN
        ENDIF  
        *B039660,1 NNA (End)
        
        =lfAloLine(llModiDye , PADR(laData[1],19) , lcPikWare)
        *-- If succeed to pick automatically & the picking method was "Maulay".
        IF !EMPTY(OrdLine.PikTkt) .AND. lnPicMethd = 2
          =lfModiLine(llModiDye , PADR(laData[1],19) , lcPikWare , lcDyelot)
        ENDIF
      ENDIF
      *B603529,1 KHM 03/26/2000 (Begin) If allocating or picking then move
      *B603529,1                the cursor to the next record in the browse.

      *C037816,1 MHM 04/06/2004 Custom Save Bin Location For David Luke [Start]
      IF ASCAN(laEvntTrig,PADR("ALSAVSTY",10)) <> 0 
        =gfDoTriger("ALSTYAL",PADR("ALSAVSTY",10))
      ENDIF
      *C037816,1 MHM  [End]

      IF !EMPTY(OrdLine.PikTkt)
        SELECT ORDLINE
        *B803668,1 MAB Locate the next record instead of Skip  [Begin]
        *B803668,1     Note that you no filter on data and the browse is done
        *B803668,1     for lcWhilExpr.
        *SKIP
        *IF ORDLINE.Style <> laData[1]
        *  SKIP -1
        *ENDIF
        PRIVATE lnActvRec
        lnActvRec =  RECNO()  && Save Current record.
        *-- Scan the next records in the table for the same style and the same while expression.
        SCAN REST WHILE Style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = laData[1] ;
               FOR &lcWhilExpr

          *-- if it is the next record exit the scan loop because it is the next browse record.
          IF RECNO() <> lnActvRec
            EXIT
          ENDIF  

        ENDSCAN       
        
        *-- if the style was changed it means another style out of our criteria.
        IF (ORDLINE.Style <> laData[1]) AND BETWEEN(lnActvRec,1,RECCOUNT())
          GO lnActvRec  && Restore the current record because it is the last record.
        ENDIF
        *B803668,1 MAB Locate the next record instead of Skip  [End  ]
        
        KEYBOARD "{ALT+B}"
        SHOW WINDOW (lcAloBrwTl) REFRESH
      ENDIF
      *B603529,1 KHM 03/26/2000 (End

    CASE lnProcess = 2
    
      *E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt. [Begin]
      IF 'AS' $ gcCmpModules AND llSndPkFld AND !EMPTY(PIKTKT)
        =SEEK(PIKTKT,'PIKTKT')
        IF PIKTKT.csndpkt = 'Y'
          =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' can not edit ')
          RETURN
        ENDIF
      ENDIF
      *E130074,1 EIH 01/02/2006 [End]
      
      
      *-- Call local function to modify the current line.
      IF "*" $ lcPikTkt
        lcOrgWare = lcPikWare
      ELSE
        lcOrgWare = IIF(SEEK(PADR(lcPikTkt,6) , "PIKTKT") , PIKTKT.cWareCode , lcPikWare)
      ENDIF
      *B602545,1 Reham On 03/01/99   *** Begin ***
      *B602545,1 Open the packing header file.
      =gfOpenFile(gcDataDir+'Pack_hdr',gcDataDir+'Pack_hdr','SH')
      *B602545,1 Variable to know if release current line or not.
      llModCurLin = .T.
      *B602545,1 Seek in the packing header file to know if the current 
      *B602545,1 picking ticket has a packing list or not.

      *B603804,1 Do not apply packing condition upon modify if it is allocated line only [Start
      *IF SEEK(PADR(lcPikTkt,6) , "Pack_hdr") .AND. PADR(Pack_hdr.piktkt,6) = PADR(lcPikTkt,6)
      IF PADR(lcPikTkt,6) <> "******" AND SEEK(PADR(lcPikTkt,6) , "Pack_hdr") AND ;
         PADR(Pack_hdr.piktkt,6) = PADR(lcPikTkt,6)
      *B603804,1 Do not apply packing condition upon modify if it is allocated line only [End

        *** Picking ticket #: {PADR(lcPikTkt,6)} has a packing list. Cannot modify! ***
        *** < Ok > ***
        =gfModalGen("INM44060B00000" , "DIALOG" , PADR(lcPikTkt,6))
        *B602545,1 Set the modify flag to false to stop release.
        llModCurLin = .F.
      ENDIF

      =gfCloseFile('Pack_hdr')
      *B602545,1 If approve release, execute the release function.
      IF llModCurLin
      *B602545,1 Reham On 03/01/99   *** End   ***

        *C037816,1 MHM 04/06/2004 Custom Save Bin Location For David Luke [Start]
        *B039660,1 NNA 09/04/2005 (Begin) Comment the next lines because it release the piktkt if you made a modification
        *IF ASCAN(laEvntTrig,PADR("ALRELORD",10)) <> 0 
        *  =gfDoTriger("ALSTYAL",PADR("ALRELORD",10))
        *ENDIF
        *B039660,1 NNA (End)
        *C037816,1 MHM  [End]
        
        =lfModiLine(llModiDye , PADR(laData[1],19) , lcOrgWare , lcDyelot)

        *B039660,1 NNA 09/04/2005  (Begin) Transfer all the bin location's Programs To Binmain.prg instead of Davmain.prg and Check first if
        *B039660,1 NNA            there is enough stock or not before making update in the Bin location Files
        IF ASCAN(laEvntTrig,PADR("CHKFIRST",10)) <> 0 AND !gfDoTriger("ALSTYAL",PADR("CHKFIRST",10))
          RETURN
        ENDIF  
        *B039660,1 NNA (End)

        *C037816,1 MHM 04/06/2004 Custom Save Bin Location For David Luke [Start]
        IF ASCAN(laEvntTrig,PADR("ALSAVSTY",10)) <> 0 
          =gfDoTriger("ALSTYAL",PADR("ALSAVSTY",10))
        ENDIF
        *C037816,1 MHM  [End]
        
      *B602545,1 Reham On 03/01/99   *** Begin ***
      ENDIF
      *B602545,1 Reham On 03/01/99   *** End   ***
    CASE lnProcess = 3
      *-- Call local function to release the current line.
      IF "*" $ lcPikTkt
        lcOrgWare = lcPikWare
      ELSE
        lcOrgWare = IIF(SEEK(PADR(lcPikTkt,6) , "PIKTKT") , PIKTKT.cWareCode , lcPikWare)
        *C037816,1 MHM 04/06/2004 Custom Save Bin Location For David Luke [Start]
        IF ASCAN(laEvntTrig,PADR("ALRELORD",10)) <> 0 
          =gfDoTriger("ALSTYAL",PADR("ALRELORD",10))
        ENDIF
        *C037816,1 MHM  [End]
        
      ENDIF
      *B602545,1 Reham On 02/22/99   *** Begin ***
      *B602545,1 Variable to know if release current line or not.
      llRelCurLin = .T.
      *B602545,1 Open the packing header file.
      =gfOpenFile(gcDataDir+'Pack_hdr',gcDataDir+'Pack_hdr','SH')
      *B602545,1 Seek in the packing header file to know if the current 
      *B602545,1 picking ticket has a packing list or not.

      *B603804,1 Do not apply packing condition upon release if it is allocated line only [Start
      *IF SEEK(PADR(lcPikTkt,6) , "Pack_hdr") .AND. PADR(Pack_hdr.piktkt,6) = PADR(lcPikTkt,6)
      IF PADR(lcPikTkt,6) <> "******" AND SEEK(PADR(lcPikTkt,6) , "Pack_hdr") AND ;
         PADR(Pack_hdr.piktkt,6) = PADR(lcPikTkt,6)
      *B603804,1 Do not apply packing condition upon release if it is allocated line only [End

        *** Picking ticket #: {PADR(lcPikTkt,6)} has a packing list. Cannot release! ***
        *** < Ok > ***
        =gfModalGen("INM44057B00000" , "DIALOG" , PADR(lcPikTkt,6))
        *B602545,1 Set the release flag to false to stop release.
        llRelCurLin = .F.
      ENDIF
      =gfCloseFile('Pack_hdr')
      *B602545,1 If approve release, execute the release function.
      IF llRelCurLin
      *B602545,1 Reham On 02/22/99   *** End   ***
        
        *E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt. [Begin]
        IF 'AS' $ gcCmpModules AND llSndPkFld
          IF PIKTKT.csndpkt = 'Y'
            =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' can not release ')
            RETURN
          ENDIF
        ENDIF
        *E130074,1 EIH 01/02/2006 [End]
        
        
        =lfRelLine(llModiDye , PADR(laData[1],19) , lcOrgWare)
      *B602545,1 Reham On 02/22/99   *** Begin ***
      ENDIF
      *B602545,1 Reham On 02/22/99   *** End   ***
  ENDCASE
  *C102424,1 (Begin) Add record for each piktkt in ALSHPINF for Ogio.
  IF ASCAN(laEvntTrig,PADR("UPDSHPS",10)) <> 0
    = gfDoTriger("ALSTYAL",PADR("UPDSHPS",10))
  ENDIF  
  *C102424,1 (Begin)
  
  *-- Unlock all the locked files.
  SET MULTILOCKS OFF
  
  *-- Get the inventory totals.
  *B604759,4 TMI [Start] comment lfsaytotal, it is called from lfwBrwRef
  *=lfSayTotal()  
  *B604759,4 TMI [End  ] 
  
  *-- Execute the browse when function to refresh the control buttons.
  =lfwBrwRef()     

ENDIF
SELECT STYLE

*!*************************************************************
*! Name      : lfAloLine
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Allocate a line
*!*************************************************************
*! Calls     : lfRecLock, lfAloQty, lfModiOrd
*!*************************************************************
*! Passed Parameters : llModiDye -> Flag to modify the style dyelot record.
*!                     laData[1] -> Style code.
*!                     lcPikWare -> Pick warehouse code.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfAloLine(llModiDye,laData[1],lcPikWare)
*!*************************************************************
*
FUNCTION lfAloLine

PARAMETERS llModiDye , lcStyle , lcPikWare

PRIVATE llModiDye , lcStyle , lcPikWare

DIMENSION laPikQty[9]
*B803741,1 HBG 11/12/2000 Declare arrays and variabel to be used in checking 
*B803741,1                if we will increase order Qty to mach the available stock [Begin]
DIMENSION laIncrsOrd[8]   && logical array to check if we will increase order Qty

lnAllocate = 0         
*-- Open Style & Scale files to get the Sizes of this style
IF !USED("STYLE")
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
ENDIF
IF !USED("SCALE")
  =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
ENDIF  
*B803741,1 [End]

*B602419,1 Reham On 02/22/99  *** Begin ***
*B602419,1 Open style dyelot browse upon Allocating any line if style 
*B602419,1 dyelot <Yes> & the style has dyelots. 
lcDyelot = '.F.'
*B602419,1 Reham On 02/22/99  *** End   ***
STORE 0 TO laPikQty

*-- Call local function to edit dyelot.
IF llModiDye .AND. lfNoDye(PADR(lcStyle,19) , lcPikWare , @lcDyelot)
  *** No dyelots selected.  Cannot allocation. ***
  *** <  OK  > ***
  =gfModalGen("INM44015B00000" , "DIALOG")
  =SEEK(PADR(lcStyle,19)+lcPikWare+SPACE(10) , 'StyDye')
  lcDyelot = ""
  RETURN
ELSE
  IF (!RLOCK('StyDye') .OR. !lfRecLock('StyDye' , 'Style Dyelot'))
    RETURN
  ENDIF
ENDIF

*B802624,1 Reham On 11/22/1999    *** Begin ***
*B802624,1 Blank the dyelot variable if there is false in its value.
lcDyelot = IIF(lcDyelot='.F.' , '' , lcDyelot)
*B802624,1 Reham On 11/22/1999    *** End   ***

lcFrmFile  = "STYDYE"
llForce    = .F.

*-- Check the available stock for allocation.
IF (&lcFrmFile..TotStk - &lcFrmFile..TotAlo) <= 0
  
  *E301225,1 Allow Force allocation option [Begin]
  *** No stock available for this line.  Force allocation? ***
  *** <  Yes  > - <  No  > ***
  *IF gfModalGen("QRM44016B00006" , "DIALOG") = 2
  lnPressed    = lfForceMsg("M44016")
  IF lnPressed = 2
    IF llModiDye
      =SEEK(PADR(lcStyle,19)+lcPikWare+SPACE( 10 ), 'StyDye' )
    ENDIF  
    RETURN
  ENDIF
  *E301225,1 Allow Force allocation option [Begin]

  llForce = .T.
ENDIF

IF llForce
  *-- If force allocation, Get the default qty. from ordline.
  *B803845,1 HBG CORRECT FIELDS ORDER [Begin]
  *SCATTER FIELDS OrdLine.Qty1, OrdLine.Qty2, OrdLine.Qty3, OrdLine.Qty4,;
                 OrdLine.Qty5, OrdLine.Qty6, OrdLine.Qty8, OrdLine.Qty7,;
                 OrdLine.TotQty TO laPikQty
  SCATTER FIELDS OrdLine.Qty1, OrdLine.Qty2, OrdLine.Qty3, OrdLine.Qty4,;
                 OrdLine.Qty5, OrdLine.Qty6, OrdLine.Qty7, OrdLine.Qty8,;
                 OrdLine.TotQty TO laPikQty
  *B803845,1 [END]

ELSE
  *B803741,1 HBG 11/12/2000 Check if Avilabel stock > order Qty for each size [Begin]
  *-- If not force allocation, get the available qty from the style dyelot file.
  *llPicked = .T.
  *FOR lnCount2 = 1 TO 8
   * lcCount2  = STR(lnCount2,1)
   * lnAvilQty = MAX(0 , &lcFrmFile..Stk&lcCount2 - &lcFrmFile..Alo&lcCount2)
   * laPikQty[lnCount2]  = IIF( lnAvilQty >= OrdLine.Qty&lcCount2 , OrdLine.Qty&lcCount2 , lnAvilQty )
   * llPicked       = IIF(laPikQty[lnCount2] > 0 , .T. , llPicked)
   * laPikQty[9]    = laPikQty[9] + laPikQty[lnCount2]
  *ENDFOR
  

  IF SEEK(&lcFrmFile..Style,'STYLE') AND SEEK('S'+Style.Scale,"SCALE")
    DECLARE laIncrsOrd[Scale.cnt]
  ENDIF  

  FOR lnI = 1 To ALEN(laIncrsOrd,1)
   lcI = STR(lnI,1)
   *B606699,1 TMI [Start] When allocating from a size take in consideration the previously allocated qtys.
   *laIncrsOrd[lnI] = &lcFrmFile..Stk&lcI - OrdLine.Qty&lcI >= 0
   laIncrsOrd[lnI] = &lcFrmFile..Stk&lcI - &lcFrmFile..Alo&lcI - OrdLine.Qty&lcI >= 0
   *B606699,1 TMI [End  ] 
  ENDFOR
  
  FOR lnI = 1 To ALEN(laIncrsOrd,1)
    lcI = STR(lnI,1)
    *-- If Avilabel stock > order Qty & there is Qty to be Pik , ask the user if he/she want to 
    *-- increase Ordered Quantity
    IF !laIncrsOrd[lnI] AND OrdLine.Qty&lcI > 0
      *-- Message : "The allocated quantity is greater than the avilabel quantity for size X. 
      *--         " Do you want to force allocation?"
      *-- Buttons : <Yes><Yes to All><No>
      lcSize = "for size " + SCALE.Sz&lcI      
      *B606699 TMI [Start] Consider the case when "Allow Force Allocation" in setup is "No"
      *lnAllocate = gfModalGen('TRM44104B44002', 'DIALOG',lcSize)
      lnAllocate = IIF(lfForceOpt() , gfModalGen('TRM44104B44002', 'DIALOG',lcSize) , 3 )
      *B606699 TMI [End  ] 
      
      DO CASE
        CASE lnAllocate = 1      && User select YES for this Size
          laPikQty[lnI] = OrdLine.Qty&lcI
          laPikQty[9]    = laPikQty[9] + laPikQty[lnI]
        CASE lnAllocate = 2      && User select YES for all Sizes
          FOR lnJ = lnI To 8
            lcJ = STR(lnJ,1)
            laPikQty[lnJ] = OrdLine.Qty&lcJ
            laPikQty[9]    = laPikQty[9] + laPikQty[lnJ]
          ENDFOR
          EXIT         
        CASE lnAllocate = 3      && User select NO for this Size
          *B606699 TMI [Start] Allocate availabel qty or 0
          *laPikQty[lnI] = &lcFrmFile..Stk&lcI
          laPikQty[lnI] = MAX(&lcFrmFile..Stk&lcI - &lcFrmFile..Alo&lcI,0)
          *B606699 TMI [End  ] 
          laPikQty[9]    = laPikQty[9] + laPikQty[lnI]
      ENDCASE  
    ELSE
      laPikQty[lnI] = OrdLine.Qty&lcI  
      laPikQty[9]    = laPikQty[9] + laPikQty[lnI]
    ENDIF 
  ENDFOR
  *-- IF total Pik > 0 , so this line will be allocate or pick
  llPicked = laPikQty[9] > 0
  *B803741,1 [End]
  
  IF !llPicked
     *** No avilable pieces to allocate. ***
     *** < Ok > ***
    =gfModalGen("INM44017B00000" , "DIALOG")
    RETURN
  ENDIF
ENDIF

*E301310,1 Check Approved Amount. [Begin]
lcHdrKey  = OrdHdr.cOrdType + OrdHdr.Order
lnDiffAmt = (laPikQty[9] - OrdLine.TotPik)*Ordline.Price
*
IF !lfChkAprov(lcHdrKey , lnDiffAmt)
  RETURN
ENDIF
*E301310,1 Check Approved Amount. [End  ]

*-- Assign a picking ticket no. for the current order line.
IF lnProcess = 1
  lcPikTkt = lfGetPkTkt(OrdLine.order , OrdHdr.cDivision , OrdLine.Store , lcPikWare)
  
  *E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt. [Begin]
  IF 'AS' $ gcCmpModules AND llSndPkFld AND !EMPTY(lcPikTkt)
    =SEEK(ALLTRIM(lcPikTkt),'PIKTKT')
    IF PIKTKT.csndpkt = 'Y'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' can not add ')
      RETURN
    ENDIF
  ENDIF
  *E130074,1 EIH 01/02/2006 [End]
  
ELSE
  lcPikTkt = "******"
ENDIF

*-- Add record for the current picking ticket in the picking ticket file.
SELECT PikTkt
IF !SEEK(lcPikTkT)
  APPEND BLANK
  *B802282,1  Reham On 05/19/1999   *** Begin ***
  *B802282,1  Add the CustPo from the order header file to the PikTkt file.
  *REPLACE PikTkt    WITH lcPikTkT ;
          cWareCode WITH lcPikWare ;
          Account   WITH OrdLine.Account ;
          Store     WITH OrdLine.Store ;
          Order     WITH OrdLine.Order ;
          Status    WITH "O" ;
          Date      WITH DATE() ;
          cAdd_User WITH gcUser_id ;
          cAdd_Time WITH gfGetTime() ;
          dAdd_Date WITH DATE()
  REPLACE PikTkt    WITH lcPikTkT ;
          cWareCode WITH lcPikWare ;
          Account   WITH OrdLine.Account ;
          Store     WITH OrdLine.Store ;
          Order     WITH OrdLine.Order ;
          CustPo    WITH OrdHdr.CustPo ;
          Status    WITH "O" ;
          Date      WITH DATE() ;
          cAdd_User WITH gcUser_id ;
          cAdd_Time WITH gfGetTime() ;
          dAdd_Date WITH DATE()
  *B802282,1  Reham On 05/19/1999   *** End   ***
  *B129738,1 MMR 10/13/2005 Fixing bug of not Generate 940 EDI transaction for 
  *B129738,1 MMR            pick tickets directed to 3PL providers.[Start]
  IF !USED("WAREHOUS")
    =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
  ENDIF  
  cAlis=ALIAS()
  SELECT WAREHOUS
  SET ORDER TO Warehous
  SEEK lcPikWare
  SELECT (cAlis)
  IF 'AS' $ gcCmpModules AND SEEK(lcPikWare,'WareHous') AND  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT') AND ;
    SEEK(EDIACPRT.cPartCode+'940','EDIPD')
    SELECT EDITRANS
    IF !SEEK('940'+PADR(lcPikTkt,40)+'W'+WareHous.cThrdPLPr)
       INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
                              ('940',lcPikTkt,'W',WareHous.cThrdPLPr,EDIACPRT.lInterComp)
    ENDIF
    REPLACE cStatus WITH 'N'
    =gfAdd_Info('EDITRANS')
  ENDIF
  =gfCloseFile('WAREHOUS')
  *B129738,1 MMR (End)
ENDIF

IF llModiDye
  *-- Adjust the record pointer in the stydye file.
  =SEEK(PADR(lcStyle,19)+lcPikWare+lcDyelot ,'StyDye')
  *-- Call function to calculate the allocated pieces & adjust the passed file.
  =lfAloQty('StyDye' , 'laPikQty[&lcCnt]' , +1)
ENDIF

*-- Adjust the record pointer in the stydye file.
=SEEK(PADR(lcStyle,19)+lcPikWare+SPACE(10) ,'StyDye')
*-- Call function to calculate the allocated pieces & adjust the passed file.
=lfAloQty('StyDye' , 'laPikQty[&lcCnt]' , +1)

*-- Call function to calculate the allocated pieces & adjust the passed file.
=lfAloQty('Style' , 'laPikQty[&lcCnt]' , +1)

*-- Call func. to adjust the order line allocated qty.
=lfModiOrd(@laPikQty , .T. , lcPikTkT , IIF(EMPTY(lcDyelot) , SPACE(10) , lcDyelot) , DATE())
*B603464,1 KHM 02/21/2000 (Begin) While allocating check if the pick from
*B603464,1                warehouse is different than the order warehouse 
*B603464,1                then replace the ordline warehouse with the
*B603464,1                pick from warehouse
IF OrdHdr.cWareCode <> lcPikWare
  REPLACE OrdLine.cWareCode WITH lcPikWare
ENDIF
*B603464,1 KHM 02/21/2000 (End)
*-- end of lfAloLine.



*!*************************************************************
*! Name      : lfRelLine
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Release an allocated line.
*!*************************************************************
*! Calls     : lfAloQty, lfModiOrd, lfOnlyLine
*!*************************************************************
*! Passed Parameters : llModiDye -> Flag to modify the style dyelot record.
*!                     lcStyle   -> Style code.
*!                     lcPikWare -> Pick warehouse code.
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfRelLine(llModiDye,laData[1],lcPikWare)
*!*************************************************************
*
FUNCTION lfRelLine

PARAMETERS llModiDye, lcStyle, lcPikWare
PRIVATE llModiDye, lcStyle, lcPikWare

DIMENSION laDumy[9]
PRIVATE laDumy
laDumy = 0

IF llModiDye
  *-- Adjust the record pointer in the stydye file.
  =SEEK(PADR(lcStyle,19)+lcPikWare+lcDyelot , 'StyDye')
  *-- Call function to calculate the allocated pieces & adjust the passed file.
  =lfAloQty('StyDye' , 'OrdLine.Pik&lcCnt' , -1)
ENDIF

*-- Adjust the record pointer in the stydye file.
=SEEK(PADR(lcStyle,19)+lcPikWare+SPACE(10) , 'StyDye')
*-- Call function to calculate the allocated pieces & adjust the passed file.
=lfAloQty('StyDye' , 'OrdLine.Pik&lcCnt' , -1)

*-- Call function to calculate the allocated pieces & adjust the passed file.
=lfAloQty('Style' , 'OrdLine.Pik&lcCnt' , -1)

*-- Modify the order line with the allocated qty.
=lfModiOrd(@laDumy , .F. , SPACE(6) , SPACE(10) , {})

*B603464,1 KHM 02/21/2000 (Begin) When releasing the allocated line
*B603464,1                set the warehouse to the default one.
REPLACE OrdLine.cWareCode WITH OrdHdr.cWareCode
*B603464,1 KHM 02/21/2000 (End)

=lfOnlyLine(lcPikTkt)
*-- end of lfRelLine.


*!*************************************************************
*! Name      : lfModiLine.                      
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Modify an allocated line.
*!*************************************************************
*! Calls      : lfClrTrap, ALMDLIN.SPX
*!*************************************************************
*! Passed Parameters : llModiDye -> Flag to modify the style dyelot record.
*!                     laData[1] -> Style code.
*!                     lcPikWare -> Pick warehouse code.
*!                     lcDyelot  -> Dyelot code.
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*! Example           :  =lfModiLine()
*!*************************************************************
*
FUNCTION lfModiLine
PARAMETERS llModiDye, lcStyle, lcPikWare, lcDyelot
PRIVATE llModiDye, lcStyle, lcPikWare, lcDyelot, lcCurAlias


*E301077,15 Reham On 12/29/98   *** Begin ***
*E301077,15 Save the current file in a variable.
lcCurAlias = ALIAS()

*E301077,15 Open the warehouse file.
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')

*E301077,15 Restore the previous file.
IF !EMPTY(lcCurAlias)
  SELECT (lcCurAlias)
ENDIF
*E301077,15 Reham On 12/29/98   *** End   ***

DECLARE laAvl[9] , laPik[9] , laOrd[9]
STORE 0 TO laAvl , laPik , laOrd

*-- Flag to know if we will adjust the order or not.
llModiOrd  = .F.

lcSelFile  = "STYDYE"

*-- Get the scale info.
=Seek("S"+Style.Scale , "SCALE")

*-- Save the scale count.
lnScaleCnt = Scale.Cnt
*-- Prepare the data to be displayed in ALMDLIN screen.
FOR lnCount = 1 TO 8
  lcCount = STR( lnCount,1 )
  lcSize&lcCount = IIF(lnCount <= lnScaleCnt , Scale.Sz&lcCount , "")
  *E500304,1 WAB - Call function to calculate the available qty by size
  *E500304,1 WAB - START  
  *laAvl[lnCount] = MAX((&lcSelFile..Stk&lcCount - &lcSelFile..Alo&lcCount) , 0 )
  *B604851,1 (Begin) No-Negative available.
  *laAvl[lnCount] = lfTotAvlbl(lcCount)
  laAvl[lnCount] = MAX(lfTotAvlbl(lcCount),0)
  *B604851,1 (End)
  *E500304,1 WAB - END
  laOrd[lnCount] = OrdLine.Qty&lcCount
  laPik[lnCount] = OrdLine.Pik&lcCount
  laAvl[9] = laAvl[9] + laAvl[lnCount]
  laOrd[9] = laOrd[9] + laOrd[lnCount]
  laPik[9] = laPik[9] + laPik[lnCount]
  lcPikStat&lcCount = IIF(lnCount <= lnScaleCnt , "ENABLE" , "DISABLE")
ENDFOR

*-- Clear the trapped keys.
ON KEY LABEL ALT+B 
=lfClrTrap()

*E301310,1 Save Approval Values before go on [Begin]
lcHdrKey   = OrdHdr.cOrdType + OrdHdr.Order
lnChkAprov = lfChkAprov(lcHdrKey , 0.00 , .T.)
DIMENSION laOldPik[ALEN(laPik,1)]
=ACOPY(laPik,laOldPik)
*E301310,1 Check Approved Amount. [End  ]

*E301310,1 Save Approval Values before go on [End  ]

*-- Call screen to be able to modify the current line.
DO (gcScrDir+gcWinAppl+"\ALMDLIN.SPX")

*-- Restore the trapped keys.
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcAloBrwTl)

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Valid function for the dyelot object in ALMDLIN screen
*!*************************************************************
*! Calls      : lfAloQty, lfModiOrd
*!*************************************************************
*! Passed parameters : None
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*! Example           :  =lfvDyelot()
*!*************************************************************
*
FUNCTION lfvDyelot

*-- If allowed to modify dyelots.
IF llModiDye
  lcOrdWare = IIF(EMPTY(lcOrdWare) , lcDefWare , lcOrdWare)
  lcOldDye = OrdLine.Dyelot
  IF EMPTY(lcDyelot)
    *** Dyelot# cannot be empty. ***
    *** < Ok > ***
    =gfModalGen("INM44018B00000" , "DIALOG")
    lcDyelot = lcOldDye
  ELSE
    llPicked = OrdLine.Picked
    *B604759,1 TMI [START] Let the validation depends on the lcPikWare
    *IF lfNoDye(PADR(lcStyle,19) , lcOrdWare , @lcDyelot)
    IF lfNoDye(PADR(lcStyle,19) , lcPikWare , @lcDyelot)
    *B604759,1 TMI [END  ]
      *** You have to select a dyelot for this line. ***
      *** < Ok > ***
      =gfModalGen("INM44019B00000" , "DIALOG")
      lcDyelot = lcOldDye
      RETURN
    ELSE
      IF lcOldDye <> lcDyelot
        *-- Check for the selected dyelot if has avilable stock to 
        *-- allocate or not
        IF (StyDye.TotStk - StyDye.TotAlo) <= 0
          *** Stock not available to allocate for dyelot ***
          *** ALLTRIM(lcDyelot). Dyelot has not been changed. ***
          *** <  Ok  > ***
          =gfModalGen("INM44020B00000" , "DIALOG" , lcDyelot)
          lcDyelot = lcOldDye            
        ELSE
          *-- Subtract allocated qty. for old dyelot.
          =SEEK(PADR(lcStyle,19)+lcPikWare+lcOldDye , 'StyDye')
          *-- Call function to calculate the allocated pieces & adjust the passed file.
          =lfAloQty('StyDye' , 'laPik[&lcCnt]' , -1)
          
          *-- Change the record pointer in style dyelot file to the new dyelot.
          =SEEK(PADR(lcStyle,19)+lcPikWare+lcDyelot , 'StyDye')

          IF RLOCK('StyDye') .OR. lfRecLock('StyDye' , 'Style Dyelot')
            laPik[9] = 0
            FOR lnCount = 1 TO 8 
              lcCount        = STR( lnCount,1 )
              lnAvilQty      = MAX(0 , StyDye.Stk&lcCount - StyDye.Alo&lcCount)
              laPik[lnCount] = IIF(lnAvilQty >= OrdLine.Qty&lcCount , OrdLine.Qty&lcCount , lnAvilQty )
              llPicked       = IIF(laPik[lnCount] > 0 , .T. , llPicked)
              laPik[9]       = laPik[9] + laPik[lnCount]
              
              *B603529,1 KHM 03/26/2000 Recalculating the avaliable qty when
              *B603529,1 KHM 03/26/2000 changing the dyelot.
              *B604759,4 TMI [Start] include The allocated qty in calculating laAvl
              *laAvl[lnCount] = StyDye.Stk&lcCount - laPik[lnCount]
              *laAvl[9] = StyDye.TotStk - laPik[9]
              laAvl[lnCount] = StyDye.Stk&lcCount - StyDye.Alo&lcCount - laPik[lnCount]
              laAvl[9] = StyDye.TotStk - StyDye.TotAlo - laPik[9]
              *B604759,4 TMI [End  ] 
              *B603529,1 KHM 03/26/2000 (End)
            ENDFOR
            *-- Add the allocated qtys. for the new dyelot.
            =lfAloQty('StyDye' , 'laPik[&lcCnt]' , +1)
            
            *-- Modify the order line qty. with the allocated qty.
            =lfModiOrd(@laPik , llPicked , IIF(laPik[9] > 0 ,;
                       OrdLine.PikTkt , SPACE(6)) , lcDyelot , DATE())
            
            *-- Refresh the qty.            
            FOR lnCount = 1 TO lnScaleCnt + 1
              lcCount = STR( lnCount,1 )
              SHOW GET laPik[lnCount]
               *B603529,1 KHM 03/26/2000 (Begin) Refreshing the avaliable qty.               
               SHOW GET laAvl[lnCount]
               *B603529,1 KHM 03/26/2000 (End)
            ENDFOR
            *B603529,1 KHM 03/26/2000 (Begin) Refereshing the screen.
            =lfRefresh('awdalmdlin')
            *B603529,1 KHM 03/26/2000 (End)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF
SHOW GET lcDyelot

*!*************************************************************
*! Name      : lfvPik
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Valid function for the Pick object in ALMDLIN screen
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed parameters : lnPikNo -> Current Qty. no.
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*! Example           :  =lfvPik(lnPikNo)
*!*************************************************************
*
FUNCTION lfvPik
PARAMETERS lnPikNo
PRIVATE lnPikNo

*-- MAB 09/01/1999 Force user to values greater than or equal zero. [begin]
IF laPik[lnPikNo] < 0
  *Message : 44081 ==> A negative value is not allowed.
  *Button  : 00000 ==> < Ok >
  =gfModalGen('INM44081B00000', 'DIALOG')
  laPik[lnPikNo] = lcOldValue
  _CUROBJ    = _CUROBJ
  RETURN
ENDIF
*-- MAB 09/01/1999 Force user to values greater than or equal zero. [end  ]

lcPikNo   = ALLTRIM(STR(lnPikNo))
lcSelFile = 'StyDye'

*-- If the allocated qty. > qty entered in the ordline file & > available stock.
*E500304,1 WAB - the avaliable qty it is get by lftotAvlbl  function
*E500304,1 WAB - START
*IF laPik[lnPikNo] > OrdLine.Pik&lcPikNo .AND. ;
   (laPik[lnPikNo] - OrdLine.Pik&lcPikNo) > &lcSelFile..Stk&lcPikNo - &lcSelFile..Alo&lcPikNo
IF laPik[lnPikNo] > OrdLine.Pik&lcPikNo .AND. ;
   (laPik[lnPikNo] - OrdLine.Pik&lcPikNo) > lfTotAvlbl(lcPikNo)
*E500304,1 WAB - END
  *E301225,1 Allow Force allocation option [Begin]
  *** No available pieces could be allocated ***
  *** for this size, force allocation?       ***
  *** < Yes > - < No > ***
  *IF gfModalGen("QRM44027B00006" , "DIALOG") = 1
  lnPressed    = lfForceMsg("M44027")
  IF lnPressed = 1
    llModiOrd = .T.
    laPik[9]  = laPik[1] + laPik[2] + laPik[3] + laPik[4] + laPik[5] + laPik[6] + laPik[7] + laPik[8]
  ELSE
    *-- Get the order line qty.
    laPik[lnPikNo] = OrdLine.Pik&lcPikNo
  ENDIF
  *E301225,1 Allow Force allocation option [End  ]

ELSE
  *-- If allocated qty > order line qty., warn the user.
  IF laPik[lnPikNo] > OrdLine.Qty&lcPikNo
    *** Quantity allocated is greater than ordered!  Increase order? ***
    *** <  Yes  > - <  No  > ***
    IF gfModalGen("INM44002B00006" , "DIALOG") = 1
      llModiOrd = .T.
    ELSE
      laPik[lnPikNo] = OrdLine.Pik&lcPikNo
      RETURN
    ENDIF  
  ENDIF
  
  laPik[9]  = laPik[1] + laPik[2] + laPik[3] + laPik[4] + laPik[5] + laPik[6] + laPik[7] + laPik[8]
ENDIF

*E301310,1 Check for approval amount [Begin]
IF llChkAprov AND (laPik[lnPikNo] <> laOldPik[lnPikNo])
  =lfSetAprv()
ENDIF  
*E301310,1 Check for approval amount [End  ]
*B603529,1 KHM 03/26/2000 (Begin) Recalculating the avaliable qty when
*B603529,1                changing the picket qty.
*B604851,1 (Begin) Take the allocated qty for other onders into consideration.
*laAvl[lnPikNo] = MAX(StyDye.Stk&lcPikNo - laPik[lnPikNo],0)
*B604759,4 TMI [Start] 
*lnRecno = RECNO('STYDYE')
*=SEEK(PADR(laData[1],19)+laWareHous[puPikWare,2]+SPACE(10),'STYDYE')
*B604759,4 TMI [End  ] 
lnWipQ = IIF(llTotAvlbl,StyDye.wip&lcPikNo,0)
*B604759,4 TMI [Start] 
*IF BETWEEN(lnRecno,1,RECCOUNT('STYDYE'))
*  GOTO (lnRecno) IN STYDYE
*ENDIF
*B604759,4 TMI [End  ] 
laAvl[lnPikNo] = MAX(StyDye.Stk&lcPikNo + lnWipQ - StyDye.Alo&lcPikNo + ordline.pik&lcPikNo - laPik[lnPikNo],0)
*B604851,1 (End)

laAvl[9] = laAvl[1]+laAvl[2]+laAvl[3]+laAvl[4]+laAvl[5]+laAvl[6]+laAvl[7]+laAvl[8]
*B603529,1 KHM 03/26/2000 (End)
SHOW GET laPik[lnPikNo]
SHOW GET laPik[9]

*B603529,1 KHM 03/26/2000 (Begin) Refereshing the screen.
=lfRefresh('awdalmdlin')
*B603529,1 KHM 03/26/2000 (End)

*

*!*************************************************************
*! Name      : lfvClsMod                      
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Valid function for ALMDLIN close button.
*!*************************************************************
*! Calls     : lfAloQty   , lfOrdQty , lfModiOrd , lfOnlyLine 
*!*************************************************************
*! Passed parameters : None
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*! Example           :  =lfvClsMod()
*!*************************************************************
*
FUNCTION lfvClsMod
*** This line will be removed from this picking ticket.  Continue? *** 
*** <  Yes  > - <  No  > ***
IF laPik[9] <= 0 .AND. gfModalGen("QRM44021B00006" , "DIALOG") = 2
  _CUROBJ = OBJNUM(laPik[1])
  RETURN
ELSE
  *-- Adjust the order header amounts with the allocated qty.
  IF llModiOrd
    laOrd[9] = 0
    FOR lnCount = 1 TO lnScaleCnt
      laOrd[lnCount] = IIF(laPik[lnCount] > laOrd[lnCount], laPik[lnCount], laOrd[lnCount])
      laOrd[9]       = laOrd[9] + laOrd[lnCount]
    ENDFOR
    IF laOrd[9] <> OrdLine.TotQty
      SELECT OrdHdr
      REPLACE Book    WITH Book    + ( laOrd[9] - OrdLine.TotQty) ;
              BookAmt WITH BookAmt + ((laOrd[9] - OrdLine.TotQty) * OrdLine.Price) ;
              Open    WITH Open    + ( laOrd[9] - OrdLine.TotQty) ;
              OpenAmt WITH OpenAmt + ((laOrd[9] - OrdLine.TotQty) * OrdLine.Price)
    ENDIF
  ENDIF
  
  IF llModiDye
    *-- Adjust the record pointer in the stydye file.
    =SEEK(PADR(lcStyle,19)+lcPikWare+lcDyelot , 'StyDye')
    *-- Call function to calculate the allocated pieces & adjust the passed file.
    =lfAloQty('StyDye' , 'laPik[&lcCnt] - OrdLine.Pik&lcCnt' , +1)
    
    IF llModiOrd
      *-- Adjust the order qty.
      =lfOrdQty('StyDye' , @laOrd)
    ENDIF
  ENDIF
  
  *-- Adjust the record pointer in the stydye file.
  =SEEK(PADR(lcStyle,19)+lcPikWare+SPACE(10) , 'StyDye')
  *-- Call function to calculate the allocated pieces & adjust the passed file.
  =lfAloQty('StyDye' , 'laPik[&lcCnt] - OrdLine.Pik&lcCnt' , +1)
  
  IF llModiOrd
    *-- Adjust the order qty.
    =lfOrdQty('StyDye' , @laOrd)
  ENDIF
  
  *-- Call function to calculate the allocated pieces & adjust the passed file.
  =lfAloQty('Style' , 'laPik[&lcCnt] - OrdLine.Pik&lcCnt' , +1 )
  
  IF llModiOrd
    *-- Adjust the order qty.
    =lfOrdQty('Style' , @laOrd)
  ENDIF
  
  *-- Adjust the order qty.
  IF laPik[9] <= 0
    =lfModiOrd(@laPik , .F. , SPACE(6) , SPACE(10) , {})
  ELSE
    =lfModiOrd(@laPik , .T. , lcPikTkt , lcDyelot , OrdLine.PikDate)
  ENDIF
  
  IF llModiOrd
    SELECT OrdLine
    GATHER FROM laOrd FIELDS Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , ;
                                Qty6 , Qty7 , Qty8 , TotQty
  ENDIF
  IF laOrd[9] <= 0
    =lfOnlyLine(lcPikTkt)
  ENDIF
ENDIF

CLEAR READ

*!*************************************************************
*! Name      : lfAloQty
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Calculate and replace the alocated qtys in the
*!           : passed file.
*!*************************************************************
*! Calls             : None
*!*************************************************************
*! Passed Parameters : lcToFile   -> File to be updated
*!                     lcWatField -> Field to be updated
*!                     lnAction   -> Value to be added or subtracted
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfAloQty()
*!*************************************************************
*
FUNCTION lfAloQty
PARAMETERS lcToFile, lcWatField, lnAction
PRIVATE laAloTemp , lcToFile, lcWatField, lnAction

*-- Array to read the picked field.
DIMENSION laAloTemp[9]
STORE 0 TO laAloTemp

SELECT &lcToFile
FOR lnCnt = 1 To 8
  lcCnt = STR(lnCnt,1)
  laAloTemp[lnCnt] = &lcToFile..Alo&lcCnt + (lnAction * &lcWatField)
  laAloTemp[lnCnt] = IIF(laAloTemp[lnCnt] < 0 , 0 , laAloTemp[lnCnt])
  laAloTemp[9]     = laAloTemp[9] + laAloTemp[lnCnt]
ENDFOR

*-- If the current style is an inventory style, update the style or stydye files..
IF laData[3]
  GATHER FROM laAloTemp FIELDS Alo1, Alo2, Alo3, Alo4,Alo5, Alo6, Alo7, Alo8, TotAlo
  
ENDIF
*!*************************************************************
*! Name      : lfNoDye
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Check for the passed style dyelot.
*!*************************************************************
*! Calls     : SDYEBROW
*!*************************************************************
*! Passed Parameters  :lcStyle   -> Style code. 
*!                     lcOrdWare -> Order Line warehouse code.
*!                     lcDyelot  -> Dyelot code.
*!*************************************************************
*! Returns           :  llGoOut
*!*************************************************************
*! Example           :  =lfNoDye()
*!*************************************************************
*
FUNCTION lfNoDye
PARAMETERS lcStyle, lcOrdWare, lcDyelot
PRIVATE llGoOut , lcStyle, lcOrdWare, lcDyelot

lcOrdWare = IIF(EMPTY(lcOrdWare) , lcDefWare , lcOrdWare)
llGoOut   = .F.

IF !SEEK(PADR(lcStyle,19) + lcOrdWare + lcDyelot, 'StyDye' )
  *-- Call the global dyelot browse.

  *B603529,1 KHM 03/26/2000 (Begin) Adding the parameter that allow 
  *B603529,1                displying the avaliabl fields.
  *IF !SDYEBROW(PADR(lcStyle,19) , @lcDyelot, .T., lcOrdWare)
  IF !SDYEBROW(PADR(lcStyle,19) , @lcDyelot, .T., lcOrdWare,.T.)
  *B603529,1 KHM 03/26/2000 (End)
    llGoOut = .T.
  ENDIF
ENDIF 
RETURN llGoOut

*!*************************************************************
*! Name      : lfModiOrd
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Update the order line record.
*!*************************************************************
*! Calls             : None
*!*************************************************************
*! Passed Parameters : laPikd    -> Array hold the picked pieces
*!                     llStatus  -> Current action.
*!                     lcPikTkt  -> Picking ticket no.
*!                     lcDyelot  -> Dyelot code.
*!                     ldActDate -> Saving date.
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example : =lfModiOrd(Pik array, Status, PikTkt, Dyelot, Date)
*!*************************************************************
*
FUNCTION lfModiOrd
PARAMETERS laPikd, llStatus, lcPikTkt, lcDyelot, ldActDate
PRIVATE laPikd, llStatus, lcPikTkt, lcDyelot, ldActDate

*C200084,1 FP Increase/Decrease Net Alo Qty [Begin]
IF ASCAN(laEvntTrig,PADR("UPDTORDHDR",10)) <> 0
  =gfDoTriger('ALSTYAL',PADR("UPDTORDHDR",10))
ENDIF
*C200084,1 FP Increase/Decrease Net Alo Qty [End  ]

*-- Adjust the order line with the allocated qtys.
SELECT OrdLine

*B802624,1 Reham On 11/22/1999    *** Begin ***
*B802624,1 Save the order qty. before releasing to be saved into the Pikline file.
laReleased = ""
SCATTER TO laReleased FIELDS Pik1,Pik2,Pik3,Pik4,Pik5,Pik6,Pik7,Pik8,TotPik,Picked,PikTkt,PikDate,Dyelot
*B802624,1 Reham On 11/22/1999    *** End   ***

GATHER FROM laPikd FIELDS Pik1, Pik2, Pik3, Pik4, Pik5, Pik6, Pik7, Pik8, TotPik
*B802501,1 WAB - update field dyelot 
*REPLACE Picked  WITH llStatus  ;
         PikTkt  WITH lcPikTkt  ;
         PikDate WITH ldActDate
REPLACE Picked  WITH llStatus  ;
        PikTkt  WITH lcPikTkt  ;
        PikDate WITH ldActDate ;
        Dyelot WITH lcDyelot
*B802501,1 WAB -END
*E126905,1 EIH 04/11/2005 [Begin] Case by style.
REPLACE cAllocatBy      WITH  IIF( TotPik > 0 , 'S' , ' ')
*E126905,1 EIH 04/11/2005 [End]
*-- end of lfModiOrd.

*!*************************************************************
*! Name      : lfOrdQty
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Calculate and replace the order qtys .
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : lcToFile -> File to be updated.
*!                     laOrdQty -> Array hold the order line qty.
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfOrdQty(lcToFile , laOrdQty)
*!*************************************************************
*
FUNCTION lfOrdQty
PARAMETERS lcToFile, laOrdQty
PRIVATE laTemp, lnCount1 , lcToFile, laOrdQty

DECLARE laTemp[9]
STORE 0 TO laTemp

SELECT &lcToFile
laTemp[9] = 0
FOR lnCount1 = 1 TO lnScaleCnt
  lcCount1         = STR( lnCount1,1)
  laTemp[lnCount1] = &lcToFile..Ord&lcCount1 + ;
                     IIF(laOrdQty[lnCount1] > OrdLine.Qty&lcCount1 , ;
                         laOrdQty[lnCount1] - OrdLine.Qty&lcCount1 , 0)
  laTemp[9] = laTemp[9] + laTemp[lnCount1]
ENDFOR

*-- If the current style is an inventory style, update the style or stydye files..
IF laData[3]
  GATHER FROM laTemp FIELDS Ord1, Ord2, Ord3, Ord4, Ord5, Ord6, Ord7, Ord8, TotOrd
ENDIF

*!*************************************************************
*! Name      : lfOnlyLine
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Check if the released line is the only line in the
*!             picking ticket then delete the picking ticket record
*!             from the picking ticket file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : lcPikTkt -> Picking ticket no.
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfOnlyLine(lcPikTkt)
*!*************************************************************
*
FUNCTION lfOnlyLine
PARAMETERS lcPikTkt
PRIVATE lnCurRec , lcPikTkt

*-- Set the delete flag to true.
llDelete   = .T.

*E301077,15 Reham On 12/29/98   *** Begin ***
*E301077,15 Open the pik line file.
=gfOpenFile(gcDataDir+'PikLine',gcDataDir+'PikLine','SH')
*E301077,15 Reham On 12/29/98   *** End   ***

*-- Save the record no. in the order line file.
SELECT OrdLine
lnCurRec   = RECNO()
lcCurOrd   = OrdLine.Order

*B802624,1 Reham On 11/22/1999   *** Begin ***
SET ORDER TO TAG ORDLINE
*B802624,1 Reham On 11/22/1999   *** End   ***

*-- Check if there is any order line use the same picking ticket.
SCAN REST WHILE Order = lcCurOrd FOR PikTkt = lcPikTkt
  *-- Set the delete flag to false.
  llDelete = .F.
  EXIT
ENDSCAN

*B802624,1 Reham On 11/22/1999   *** Begin ***
SET ORDER TO TAG ORDLINES
*B802624,1 Reham On 11/22/1999   *** End   ***

*-- Restore the record no.
IF lnCurRec <= RECCOUNT("OrdLine") .AND. lnCurRec > 0
  GOTO lnCurRec
ENDIF
IF llDelete .AND. !("*" $ lcPikTkt)
  *-- Change the piktkt status to be canceled.
  IF SEEK(lcPikTkt , "PikTkt")
    REPLACE PikTkt.Status WITH "X"
    *B129738,1 MMR 10/13/2005 Fixing bug of not Generate 940 EDI transaction for 
    *B129738,1 MMR            pick tickets directed to 3PL providers.[Start]
    IF !USED("WAREHOUS")
      =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
    ENDIF  
    cAlis=ALIAS()
    SELECT WAREHOUS
    SET ORDER TO Warehous
    SEEK PikTkt.cWareCode
    SELECT (cAlis)
    IF 'AS' $ gcCmpModules AND SEEK(PikTkt.cWareCode,'WareHous') AND  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT') AND ;
      SEEK(EDIACPRT.cPartCode+'940','EDIPD') AND SEEK('940'+PADR(lcPikTkt,40)+'W'+WareHous.cThrdPLPr,'EDITRANS')
      SELECT 'EDITRANS'
      DELETE
    ENDIF
    =gfCloseFile('WAREHOUS')
    *B129738,1 MMR (End)
  ENDIF
  IF !SEEK(lcPikTkt + lcCurOrd , "PikLine")
    *-- Add record in the released pick line file have the order line info.
    SELECT OrdLine
    SCATTER MEMVAR MEMO
    SELECT PikLine
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cAdd_User WITH gcUser_id ;
            cAdd_Time WITH gfGetTime() ;
            dAdd_Date WITH DATE()
    *B802624,1 Reham On 11/22/1999    *** Begin ***
    *B802624,1 Save the order info into the Pikline file.
    GATHER FROM laReleased FIELDS Pik1,Pik2,Pik3,Pik4,Pik5,Pik6,Pik7,Pik8,TotPik,Picked,PikTkt,PikDate,Dyelot
    *B802624,1 Reham On 11/22/1999    *** End   ***
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfSqlTherm
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Function to call the gfTherm() to be displayed
*!           : while collecting the needed orders.
*!*************************************************************
*! Calls     : gfTherm
*!*************************************************************
*! Passed Parameters : lnSqlTotal -> Total records.
*!                     lnSqlCount -> Current processed record.
*!                     lcSqlMsg   -> Message to be displayed.
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfSqlTherm(100 , 1 , "Update File..")
*!*************************************************************
*
FUNCTION lfSqlTherm
PARAMETERS lnSqlTotal , lnSqlCount , lcSqlMsg
PRIVATE lnSqlTotal , lnSqlCount , lcSqlMsg

lcSqlMsg    = IIF(TYPE('lcSqlMsg')='C' , lcSqlMsg , 'Collecting Data...')
&lnSqlCount = &lnSqlCount + 1

*-- Call global function to execute the thermometer.
IF &lnSqlCount <= lnSqlTotal
  =gfTherm(lnSqlTotal ,&lnSqlCount , lcSqlMsg , ' ')
ENDIF  

*!*************************************************************
*! Name      : lfPikAlo
*! Developer : Reham Al-Allamy
*! Date      : 01/15/1998
*! Purpose   : Function to pick the allocated picking ticket.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfPikAlo()
*!*************************************************************
*
FUNCTION lfPikAlo

*-- Assign a picking ticket no. for the current order line.
lcPikTkt = lfGetPkTkt(OrdLine.order , OrdHdr.cDivision , OrdLine.Store , lcPikWare)

*E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt. [Begin]
IF 'AS' $ gcCmpModules AND llSndPkFld AND !EMPTY(lcPikTkt)
  =SEEK(ALLTRIM(lcPikTkt),'PIKTKT')
  IF PIKTKT.csndpkt = 'Y'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' can not add ')
    RETURN
  ENDIF
ENDIF
*E130074,1 EIH 01/02/2006 [End]


*-- Add record for the current picking ticket in the picking ticket file.
SELECT PikTkt
IF !SEEK(lcPikTkT)
  APPEND BLANK
  REPLACE PikTkt    WITH lcPikTkT ;
          cWareCode WITH lcPikWare ;
          Account   WITH OrdLine.Account ;
          Store     WITH OrdLine.Store ;
          Order     WITH OrdLine.Order ;
          Status    WITH "O" ;
          Date      WITH DATE() ;
          cAdd_User WITH gcUser_id ;
          cAdd_Time WITH gfGetTime() ;
          dAdd_Date WITH DATE()
ENDIF
*-- Adjust the order line with the allocated qtys.
SELECT OrdLine
REPLACE PikTkt  WITH lcPikTkt  ;
        PikDate WITH DATE()
*B129738,1 MMR 10/13/2005 Fixing bug of not Generate 940 EDI transaction for 
*B129738,1 MMR            pick tickets directed to 3PL providers.[Start]
IF !USED("WAREHOUS")
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
ENDIF  
cAlis=ALIAS()
SELECT WAREHOUS
SET ORDER TO Warehous
SEEK lcPikWare
SELECT (cAlis)
IF lcPikTkt <> '******' AND 'AS' $ gcCmpModules AND SEEK(lcPikWare,'WareHous') AND ;
  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT') AND  SEEK(EDIACPRT.cPartCode+'940','EDIPD')
  SELECT EDITRANS
  IF !SEEK('940'+PADR(lcPikTkt,40)+'W'+WareHous.cThrdPLPr)
     INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
                            ('940',lcPikTkt,'W',WareHous.cThrdPLPr,EDIACPRT.lInterComp)
  ENDIF
  REPLACE cStatus WITH 'N'
  =gfAdd_Info('EDITRANS')
ENDIF
=gfCloseFile('WAREHOUS')
*B129738,1 MMR (End)
*!*************************************************************
*! Name      : lfRecLock
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1997
*! Purpose   : Try to place a physical lock.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcFileName -> File name 
*!                       lcFileDesc -> File Description
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfRecLock(lcFileName , lcFileDesc)
*!*************************************************************
*
FUNCTION lfRecLock
PARAMETERS lcFileName, lcFileDesc
PRIVATE  llLocked , lnTemp , lnChoice , lcFileName, lcFileDesc

*-- Flag to know if succeed in locking the files.
llLocked   = .F.
*-- Set a no. of trials.
lnHowMany  = 10
SET REPROCESS TO 1

*-- Try to execute the locking till succeed.
DO WHILE !llLocked
  llLocked = RLOCK(lcFileName)
  *-- If fail to lock the needed files.
  IF !llLocked
    IF lnHowMany = 0
      *** &lcFileDesc is in use by another user. ***
      *** <  Retry  > - <  Cancel > ***
      IF gfModalGen("QRM44022B00016" , "DIALOG" , lcFileDesc) = 2
        llLocked = .F.
        EXIT
      ELSE
        lnHowMany = 10
      ENDIF
    ENDIF
    lnHowMany = lnHowMany - 1
    LOOP
  ELSE
    EXIT
  ENDIF
ENDDO

SET REPROCESS TO 0

RETURN llLocked


*!*************************************************************
*! Name      : lfPickStat
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/22/99
*! Purpose   : Enable / Disable for Pick Button.
*!*************************************************************
*!
FUNCTION lfPickStat
lcPikStat  = IIF(!EMPTY(OrdLine.Style) AND ;
             (EMPTY(OrdLine.PikTkt) OR "*" $ OrdLine.PikTkt) ,;
              "ENABLE" , "DISABLE")

IF ASCAN(laEvntTrig,PADR("STYPKSTAT",10)) <> 0
  =gfDoTriger('ALSTYAL',PADR("STYPKSTAT",10))
ENDIF
*-- end of lfPickStat.

*!*************************************************************
*! Name      : lfForceMsg
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/22/99
*! Purpose   : Force allocation message.
*!*************************************************************
*!E301225,1 Allow Force allocation option.
*!
FUNCTION lfForceMsg
PARAMETERS lcMessage
PRIVATE lcButton , lnAction , lcMsgParm , llForceMe
STORE '' TO lcButton , lcMsgParm
lnAction = 0

llForceMe = lfForceOpt()

IF llForceMe
  lcMsgParm = "Do you want to force allocation?"
  lcButton  = "B00006"
  lcMessage = "TR"+lcMessage

ELSE

  lcButton  = "B00000"
  lcMessage = "QR"+lcMessage

ENDIF

lnAction = gfModalGen(lcMessage+lcButton , "DIALOG" , lcMsgParm)
lnAction = IIF(llForceMe,lnAction,2)
RETURN lnAction
*-- end of lfForceMsg.

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
    llAlwForce = gfUserPriv('AL','ALSTYAL','FORCING')

  ENDIF
ENDIF

*B039660,1 NNA 09/04/2005 (BEGIN) Stop the force alloction if use bin location set to yes.
IF ASCAN(laEvntTrig,PADR("CHKFORCE",10)) <> 0 AND gfDoTriger('ALSTYAL',PADR("CHKFORCE",10))
  llAlwForce = .F.
ENDIF
*B039660,1 NNA (END)

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
PARAMETERS lcHdrKey , lnDiferenc , llGetNum
PRIVATE llCanAloct
llCanAloct = .T.

IF llChkAprov
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

  lnAllocAmt = lnAllocAmt + lnDiferenc
  
  *-- if Get Compared number only.
  IF llGetNum
    SELECT (lnCurrAlis)
    *B803358,1 AMH 06/25/2000 Checking Order Approved Amount (Start)
    *RETURN (OrdHdr.ApprAmt - (OrdHdr.ShipAmt + lnAllocAmt))
    RETURN (OrdHdr.ApprAmt - lnAllocAmt)
    *B803358,1 AMH 06/25/2000 Checking Order Approved Amount (End)
  ELSE
    *-- if you are greater than approval amount.
    *B803358,1 AMH 06/25/2000 Checking Order Approved Amount (Start)
    *IF (OrdHdr.ApprAmt - (OrdHdr.ShipAmt + lnAllocAmt)) < 0
    IF (OrdHdr.ApprAmt - lnAllocAmt) < 0
    *B803358,1 AMH 06/25/2000 Checking Order Approved Amount (End)
      =gfModalGen("TRM44079B00000" , "DIALOG")
      llCanAloct = .F.
    ENDIF
  ENDIF  

  *-- Check processing [End  ]

  SELECT (lnCurrAlis)
  *-- Restore Old Environment [End  ]
ENDIF  
RETURN llCanAloct
*-- end of lfChkAprov.

*!*************************************************************
*! Name      : lfSetAprv
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/24/99
*! Purpose   : Set variable hold Approve Amount, and make approprate checks.
*!*************************************************************
*!E301310,1 Check Approved Amount.
*!
FUNCTION lfSetAprv
IF laPik[lnPikNo] > laOldPik[lnPikNo]
  IF (lnChkAprov - (laPik[lnPikNo] - laOldPik[lnPikNo]) * OrdLine.Price) < 0
    =gfModalGen("TRM44079B00000" , "DIALOG")
    laPik[lnPikNo] = laOldPik[lnPikNo]
  ELSE
    lnChkAprov = lnChkAprov - (laPik[lnPikNo] - laOldPik[lnPikNo]) * OrdLine.Price
    laOldPik[lnPikNo] = laPik[lnPikNo]
    laOldPik[9]  = laPik[9]
  ENDIF

ELSE  && less than.

  lnChkAprov = lnChkAprov - (laPik[lnPikNo] - laOldPik[lnPikNo]) * OrdLine.Price
  laOldPik[lnPikNo] = laPik[lnPikNo]
  laOldPik[9]  = laPik[9]

ENDIF  
*-- end of lfSetAprv.
*
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
PRIVATE lnTotal
*B804033,1 AMM Initialize
lnTotal = 0
*B804033,1 AMM end
*------------- this function is to calculate the availble qty depend on setting
*------------- in AL module  ( the availble = STQ - ALO ) or
*------------- (the availble qty = WIP + STQ - ALO )
*--lcSize  --> hold '9' if the function is called to get total availble qty
*--			   hold ('1'->'8') called to get availble by size
*--lnTotal --> hold the availble qty
*B604851,1 (Begin) Get the right record in the stydye.
*-- According to the multi warehouse flag, choose the right seek expression.
*B604759,1 TMI [Start] Include the dylot in the seek expression
*lcSeekExp = "PADR(laData[1],19)+lcPikWare+SPACE(10)"
PRIVATE lcSeekExp  && Hide variable , it is used in the calling fn.
lcSeekExp = "PADR(laData[1],19)+laWareHous[puPikWare,2]+ORDLINE.DYELOT"
*B604759,1 TMI [End  ] Include the dylot in the seek expression
=SEEK(&lcSeekExp , lcSelFile)
*B604851,1 (End)

lcSize = ALLTRIM(lcSize)
IF !llTotAvlbl 		&& the avaible = stq - alocated
  IF lcSize = '9'			&& the total qty
    lnTotal = MAX((&lcSelFile..TotStk - &lcSelFile..TotALO),0)
  ELSE						&& by size
    lnTotal = MAX((&lcSelFile..Stk&lcSize - &lcSelFile..Alo&lcSize) , 0 )
  ENDIF
ELSE					   && the avialble = wip + stq - alocated
  *--to get the WIP we must get the firts record of the style in the stydye
  *--file to get the wip cuse there is no wip for dyelot
  *--after thar return to the current record

  lnRecNo = RECNO(lcSelFile)
  *B804033,1 AMM this variable may be zero, so check.
  IF puPikWare # 0
  *B804033,1 AMM end
    =SEEK(laData[1]+laWareHous[puPikWare,2]+SPACE(10),lcSelFile)
    lnWip = IIF(lcSize = '9',&lcSelFile..TotWIP,&lcSelFile..WIP&lcSize)
    *B604759,4 TMI [Start] Get lnTotWip from the style line with(out) dyelots
    lnTotWip = IIF(EMPTY(ORDLINE.DYELOT),&lcSelFile..TotWIP,0)
    *B604759,4 TMI [End  ] 
    IF BETWEEN(lnRecNo, 1, RECCOUNT(lcSelFile))
      GO  lnRecNo IN &lcSelFile
    ENDIF
    *B604759,4 TMI [Start] do not include wip in case of dyelot
    IF EMPTY(ORDLINE.DYELOT)    
    *B604759,4 TMI [End  ] do not include wip in case of dyelot
      IF lcSize = '9'		   && the total qty
        lnTotal = lnWip + &lcSelFile..TotStk - &lcSelFile..TotALO
      ELSE					   && by size
        lnTotal = lnWip + &lcSelFile..Stk&lcSize - &lcSelFile..Alo&lcSize
      ENDIF
    *B604759,4 TMI [Start] do not include wip in case of dyelot
    ELSE
      IF lcSize = '9'		   && the total qty
        lnTotal = &lcSelFile..TotStk - &lcSelFile..TotALO
      ELSE					   && by size
        lnTotal = &lcSelFile..Stk&lcSize - &lcSelFile..Alo&lcSize
      ENDIF
    ENDIF
    *B604759,4 TMI [End  ] do not include wip in case of dyelot
  *B804033,1 AMM END IF
  ENDIF
  *B804033,1 AMM end
ENDIF
RETURN(lnTotal)


