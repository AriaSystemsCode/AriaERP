*:************************************************************************
*: Program file  : ALSADAL.PRG
*: Program desc. : Custom Program To Allocate By Style for SAD10.
*: For screen    : ALSADAL.SCX (0,1,2,3,4)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Allocation Module
*:      Developer: Adel Mohammed el Gazzar
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
*:***********************************************************************************
* Refer to C(102318)
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
*--102318
DECLARE laAvl[9] , laPik[9] , laOrd[9]
STORE 0 TO laAvl , laPik , laOrd
STORE '' TO lcSize1,lcSize2,lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8
STORE '' TO lcDyelot,lcPikTkt,lcOrgWare
STORE '' TO lcPikStat1,lcPikStat2,lcPikStat3,lcPikStat4,lcPikStat5,lcPikStat6,lcPikStat7,lcPikStat8
llModify  = .T.
STORE .F. TO llModiOrd,llModiDye,llModiWH,llModCurLin,llShowMes
STORE '' TO lcOrdTmp,lcInvertSt,lcSelAllSt,lcSelNonSt,lcAloSt,lcRelSt
lcSelAllSt = 'DISABLE'
lcSelNonSt = 'DISABLE'
lcAloSt    = 'DISABLE'
lcRelSt    = 'DISABLE'
lcInvertSt = 'DISABLE'
** lcInvertSt   Variable to hold the status of the Invert button



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
lcAlStyAl3 = ""
lcAlStyAl4 = ""
llFrstRun  = .T.
lcCodesOrd = ""  && Var. to hold the old codes order.

*B803668,1 MAB Define the While expression variable so that I can use it [Begin]
*B803668,1     when move to next record
lcWhilExpr = ""
*B803668,1 MAB Define the While expression variable so that I can use it [End  ]

STORE .F. TO llBrowse , llMultiWH , llDyelot

*-- Allow Force allocation option [Begin]
lcAlwForce = ''
llChkAprov = .F.

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

  *--Allow Force allocation option.
  lcAlwForce = ALLTRIM(laMainSetp[3,2])

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
  
  *E301077,15 Reham On 12/29/98   *** Begin ***
  =gfCloseFile('WAREHOUS')
  *E301077,15 Reham On 12/29/98   *** End   ***
  
  *-- Get Temp. names for the screen set.
  *lcAlStyAl0  = 'CW'+gfTempName() &&SYS(2015)
  *lcAlStyAl1  = 'CW'+gfTempName() &&SYS(2015)
  *lcAlStyAl2  = 'CW'+gfTempName() &&SYS(2015)
  lcAlStyAl0  = gfTempName()
  lcAlStyAl1  = gfTempName()
  lcAlStyAl2  = gfTempName()
  *C102318
  lcAlStyAl3  = gfTempName()
  lcAlStyAl4  = gfTempName()
  lcOrdTmp    = gfTempName()
ENDIF

*C102318
*CREATE CURSOR (lcOrdTmp) (Order C(6), Key C(100),cSel C(1))
CREATE CURSOR (lcOrdTmp) (Order C(6), Key C(48),store c(8),cSel C(1))
INDEX ON Order+key TAG (lcOrdtmp) 

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
*ON KEY LABEL ALT+B ACTIVATE WINDOW (lcAloBrwTl)

*-- Call the screen.
SELECT Style
*C102318
DO (gcScrDir+gcWinAppl+"\ALSADAL.SPX")

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
*! Developer : Adel Mohammed el Gazzar
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
return
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
*! Developer : Adel Mohammed el Gazzar
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
*! Developer : Adel Mohammed el Gazzar
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
    puPikWare = IIF(ASCAN(laWareHous,lcPikWare) > 0 , ASUBSCRIPT(laWareHous,ASCAN(laWareHous,lcPikWare),1) , 0)
    *B804033,1 AMM return setting of exact as it was.
    SET EXACT &lcSetEx
    *B804033,1 AMM end
    
    *-- Get the inventory totals.
    =lfSayTotal()
    
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
    lcViewStat = IIF(!EMPTY(laData[1])     , "ENABLE" , "DISABLE")
    lcAloStat  = IIF(!EMPTY(OrdLine.Style) , "ENABLE" , "DISABLE")
    lcEdtStat  = IIF(!EMPTY(OrdLine.Style) .AND. !EMPTY(OrdLine.PikTkt) , "ENABLE" , "DISABLE")

    *C200084,1 Call function to enable/disable to call FPMAIN Once [Begin]
    *lcPikStat  = IIF(!EMPTY(OrdLine.Style) .AND. (EMPTY(OrdLine.PikTkt) .OR. "*" $ OrdLine.PikTkt) , "ENABLE" , "DISABLE")
    =lfPickStat()
    *C200084,1 Call function to enable/disable to call FPMAIN Once [End]

    lcAloStat  = IIF(!EMPTY(OrdLine.Style) .AND. EMPTY(OrdLine.PikTkt) , "ENABLE" , "DISABLE")
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
*! Developer : Adel Mohammed el Gazzar
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
RETURN
IF VARREAD() = 'PUPIKWARE' AND !EMPTY(ORDLINE.PIKTKT)
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Adel Mohammed el Gazzar
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
    laData[1] = gfStyBrw("I" , laData[1] , "" , .F.)
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
  *C102318
  =lfvProcess(2)
  IF RECCOUNT(lcOrdTmp) <> 0
    SHOW GET pbSelect ENABLE
    SHOW GET pbSelAll ENABLE
    SHOW GET pbInvert ENABLE
  ENDIF
ENDIF
llBrowse = .F.
lcViewStat = IIF(!EMPTY(laData[1]) , "ENABLE" , "DISABLE")
SHOW GET puOrdWare  &lcViewStat
SHOW GET puPikWare  &lcViewStat
SELECT STYLE

*!*************************************************************
*! Name      : lfvOrdWare
*! Developer : Adel Mohammed el Gazzar
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
ENDIF

SELECT STYLE

*!*************************************************************
*! Name      : lfvPikWare
*! Developer : Adel Mohammed el Gazzar
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
  llModify = .T.
  lModCurLin = .T.
  llModiDye = IIF(llDyelot .AND. Style.cDye_Flg = 'Y' .AND. SEEK(PADR(laData[1],19)+lcPikWare+lcDyelot,'StyDye') , .T. , .F.)
  lcDyelot  = OrdLine.Dyelot
  *--If approve release, execute the release function.
  IF llModCurLin
    =lfModiLine(llModiDye , PADR(laData[1],19) , lcOrgWare , lcDyelot)
  ENDIF
ENDIF
SELECT STYLE

*!*************************************************************
*! Name      : lfSayTotal
*! Developer : Adel Mohammed el Gazzar
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
lcSeekExp = "PADR(laData[1],19)+lcPikWare+SPACE(10)"

IF SEEK(&lcSeekExp , lcSelFile)
  *-- If found style in the file, Get the inventory totals.
  lnTotStk   = &lcSelFile..TotStk
  lnTotAlo   = &lcSelFile..TotAlo
  *lnTotAvl   = IIF(llMultiWH , &lcSelFile..TotStk , &lcSelFile..TotStk+&lcSelFile..TotWip)
  *--call function to calculate the total available qty
  lnTotAvl   = lfTotAvlbl('9')
  lnTotWip   = &lcSelFile..TotWip
  lnTotUnalo = &lcSelFile..TotStk - &lcSelFile..TotAlo
  lnTotOrd   = &lcSelFile..TotOrd
ELSE
  STORE 0 TO lnTotStk , lnTotAlo , lnTotAvl , lnTotWip , lnTotUnalo  , lnTotOrd
ENDIF
*-- Refresh the totals.
=lfRefresh()

*!*************************************************************
*! Name      : lfwBrowOrd
*! Developer : Adel Mohammed el Gazzar
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

SELECT OrdLine
lnMarker = RECNO()
*C102318
lcAloBrFld  = 'cMarker=IIF(RECNO()=lnMarker ,">"," "):H=" ":R:1:W=.F. ,'+;
       'cSel=IIF(SEEK(Ordline.ORDER+Ordline.Style+DTOS(Ordline.Complete)+Ordline.cOrdType+Ordline.Order+Ordline.Store+STR(Ordline.LineNo,6),lcOrdTmp) AND !EMPTY(&lcOrdTmp..cSel) AND OrdLine.store = &lcOrdTmp..store ,">>"," "):H=" ":R:2:W=.F. ,'+;
       'OrdHdr.Order :H="Order#":8 , OrdHdr.Account :H="Acct." :7 ,'+;
       'OrdLine.qty1 :H="Ord.1 ":8 , OrdLine.qty2 :H="Ord.2 ":8 ,'+;
       'OrdLine.qty3 :H="Ord.3 ":8 , OrdLine.qty4 :H="Ord.4 ":8 ,'+;
       'OrdLine.qty5 :H="Ord.5 ":8 , OrdLine.qty6 :H="Ord.6 ":8 ,'+;
       'OrdLine.qty7 :H="Ord.7 ":8 , OrdLine.qty8 :H="Ord.8 ":8 ,'+;
       'OrdLine.Totqty :H="TotOrd ":8 ,'+;
       'OrdLine.Store :H="Store":14 ,OrdLine.cWareCode:H="Pick from":14,'+;
       'OrdLine.Dyelot:H="Dyelot":14, OrdLine.Complete :H="Complete":10 ,'+;
       'OrdHdr.Status :H="S":2 , '+;
       'OrdLine.PikTkt :H="PikTkt":7, OrdLine.Price :H="Price":10,'+;
       'OrdLine.TotPik :H="PikQty":8 ,'+;
       'lnDumy=(OrdLine.TotQty-OrdLine.TotPik):H="Unallo.Qty":10'
       
lcForExpr  = "Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = PADR(laData[1],19)"
lcWareExpr = [OrdHdr.cWareCode = lcOrdWare]
lcBForExpr = [OrdHdr.Status $ "OH" AND TotQty <> 0 AND cOrdType <> 'T']

IF ASCAN(laEvntTrig,PADR("ADJBROW",10)) <> 0
  =gfDoTriger('ALSTYAL',PADR("ADJBROW",10))
ENDIF
lcWhilExpr = lcBForExpr + [ AND ] + lcWareExpr
llLinFound = .F.
IF SEEK(PADR(laData[1],19) , "OrdLine")
  LOCATE REST WHILE &lcForExpr FOR &lcWhilExpr
  llLinFound = IIF(FOUND() , .T. , .F.)
ELSE
  llLinFound = .F.
ENDIF
*C102318
IF !EMPTY(laData[1])
  SELECT (lcOrdTmp)
  ZAP
  SELECT ORDLINE 
  SCAN WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = PADR(laData[1],19) ;
       FOR &lcWhilExpr
    lcKey = EVAL(KEY())    
    INSERT INTO (lcOrdTmp) (ORDER,KEY,store) VALUES (ORDLINE.ORDER,lcKey,ORDLINE.store)
  ENDSCAN     
ENDIF  
=SEEK(PADR(laData[1],19) , "OrdLine")
lnCurMark = IIF(llLinFound , RECNO("OrdLine") , lnMarker)
IF llFrstRun .OR. EMPTY(laData[1]) .OR. !llLinFound
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
  ELSE
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
  ENDIF
ENDIF

SHOW WINDOW (lcAloBrwTl) REFRESH
=lfwBrwRef()
SELECT STYLE

*!*************************************************************
*! Name      : lfwBrwRef
*! Developer : Adel Mohammed el Gazzar
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
*-- Change the status of the process buttons.
lcEdtStat = IIF(!EMPTY(OrdLine.PikTkt) .AND. !EMPTY(OrdLine.Style) , "ENABLE" , "DISABLE")
=lfPickStat()
lcAloStat = IIF( EMPTY(OrdLine.PikTkt) .AND. !EMPTY(OrdLine.Style) , "ENABLE" , "DISABLE")
lnElem     = ASCAN(laWareHous, OrdLine.cWareCode)
puPikWare  = IIF(lnElem = 0 , 0 , ASUBSCRIPT(laWareHous, lnElem, 1))
SHOW GET puPikWare
*-- Refresh the process buttons.
SHOW GET pbLink    &lcEdtStat
SHOW GET pbEdtLin  &lcEdtStat
SHOW GET pbReles   &lcEdtStat
IF llPikNow
  SHOW GET pbPick    &lcPikStat
ELSE
  SHOW GET pbPick     DISABLE
ENDIF
SHOW GET pbAloct   &lcAloStat
IF EMPTY(laData[1])
  RETURN
ENDIF
*C102318
llModify = .T.
lcPikTkt = OrdLine.PikTkt
IF "*" $ lcPikTkt
  lcOrgWare = lcPikWare
ELSE
  lcOrgWare = IIF(SEEK(PADR(lcPikTkt,6) , "PIKTKT") , PIKTKT.cWareCode , lcPikWare)
ENDIF
IF !USED('Pack_hdr')
  =gfOpenFile(gcDataDir+'Pack_hdr',gcDataDir+'Pack_hdr','SH')
ENDIF  
lModCurLin = .T.
*LOOOOOOOK
IF PADR(lcPikTkt,6) <> "******" AND SEEK(PADR(lcPikTkt,6) , "Pack_hdr") AND ;
   PADR(Pack_hdr.piktkt,6) = PADR(lcPikTkt,6)
  *** Picking ticket #: {PADR(lcPikTkt,6)} has a packing list. Cannot modify! ***
  *** < Ok > ***
  =gfModalGen("INM44060B00000" , "DIALOG" , PADR(lcPikTkt,6))
  llModCurLin = .F.
ENDIF
=gfCloseFile('Pack_hdr')
llModiDye = IIF(llDyelot .AND. Style.cDye_Flg = 'Y' .AND. SEEK(PADR(laData[1],19)+lcPikWare+lcDyelot,'StyDye') , .T. , .F.)
lcDyelot  = OrdLine.Dyelot
*--If approve release, execute the release function.
IF llModCurLin
  =lfModiLine(llModiDye , PADR(laData[1],19) , lcOrgWare , lcDyelot)
ENDIF
lcEval = Ordline.Style+DTOS(Ordline.Complete)+Ordline.cOrdType+Ordline.Order+Ordline.Store+STR(Ordline.LineNo,6)
=SEEK(Ordline.ORDER+lcEval,lcOrdTmp)
IF !EMPTY(&lcOrdTmp..cSel)
  SHOW GET pbSelect,1 ENABLE PROMPT '\<Unselect'
ELSE
  *-- If UnSelect
  SHOW GET pbSelect,1 ENABLE PROMPT 'Se\<lect' 
ENDIF

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Adel Mohammed el Gazzar
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
ENDIF

*!*************************************************************
*! Name      : lfActMain
*! Developer : Adel Mohammed el Gazzar
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
*! Developer : Adel Mohammed el Gazzar
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
*! Developer : Adel Mohammed el Gazzar
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
*C102318
IF WLAST() = lcAlStyAl3 AND !EMPTY(lcPikTkt)
  =lfvClsMod()
ENDIF

IF WONTOP() = lcAloBrwTl
  ON KEY LABEL CTRL+Q  lnDummy = 1
  ON KEY LABEL CTRL+W  lnDummy = 1
  ON KEY LABEL CTRL+M  DO lpTrap WITH lcALStyAl2 , 'pbEdtLin'  , IIF(lcEdtStat = "ENABLE" , .T. , .F.)
  ON KEY LABEL CTRL+E  DO lpTrap WITH lcALStyAl2 , 'pbReles'   , IIF(lcEdtStat = "ENABLE" , .T. , .F.)
  ON KEY LABEL CTRL+P  DO lpTrap WITH lcALStyAl2 , 'pbPick'    , IIF(lcPikStat = "ENABLE" , .T. , .F.)
  ON KEY LABEL CTRL+A  DO lpTrap WITH lcALStyAl2 , 'pbAloct'   , IIF(lcAloStat = "ENABLE" , .T. , .F.)

*  ON KEY LABEL TAB     DO lpTrap WITH lcALStyAl2 , 'pbPick'    , .F.
  ON KEY LABEL TAB     DO lpTrap WITH lcAlStyAl3 , 'laPik[1]'    , .F.
  
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
*! Developer : Adel Mohammed el Gazzar
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
*! Developer : Adel Mohammed el Gazzar
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
*! Developer : Adel Mohammed el Gazzar
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
*C102318
PRIVATE lnAlias,llRelease,llFatExt,llNotAct,llOrdSt,llSkpHold
lnAlias = SELECT()
SELECT (lcOrdTmp)
COUNT TO lnRec FOR !EMPTY(cSel)
STORE .F. TO llRelease,llFatExt,llNotAct,llOrdSt,llSkpHold,llHasPikNo,llAddNew,llForAll
STORE .F. TO llShowMes
*--If the user doesn't select any record, Process the record.
IF lnRec = 0
  SELECT (lnAlias)
  =lfvProces2(lnProcess)
ELSE
  *--If the user seleted many records.
  SELECT (lcOrdTmp)
  SCAN FOR !EMPTY(cSel)
    lcKey    = ALLTRIM(&lcOrdTmp..KEY)
    lcOrdKey = Eval(key())
    SELECT ORDLINE
    *--Get the order
    =SEEK(lcKey)
    *--Don't pick previously picked, or release previously released.
    IF (lnProcess = 1 AND (EMPTY(PIKTKT) OR "*" $ lcPikTkt)) OR (lnProcess = 4 AND EMPTY(PIKTKT)) OR lnProcess = 3
      =lfvProces2(lnProcess)
    ENDIF
    *--If any problem objects picking or releasing.
    IF llFatExt
      EXIT
    ENDIF
    SELECT (lcOrdTmp)
    =SEEK(lcOrdKey)
  ENDSCAN
ENDIF
*--Unselct all recods.
SELECT (lcOrdTmp)
REPLACE ALL cSel WITH ''
SHOW GET pbSelect,1 ENABLE PROMPT 'Se\<lect' 
*--Enable and disable buttons.
=lfSetBut()
SHOW WINDOW (lcAloBrwTl) REFRESH SAME  

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvProces2
*! Developer : Adel Mohammed el Gazzar
*! Date      : 07/22/2001
*! Purpose   : Allocate.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*C102318
FUNCTION lfvProces2
PARAMETERS lnProcess
*-- If the pick button was pressed & there is no quantities to allocate.
SELECT OrdLine

*-- If the process is Allocate or Pick then check the status of the customer if it was 
*-- non-active customer do not allow allocating or picking.
IF (lnProcess = 1 .OR. lnProcess = 4) AND Customer.Status <> 'A' AND !llNotAct
  llNotAct = .T.
  lcStatus = "One or more account with inactive status found. "+ IIF(lnProcess=1,'Cannot Pick.','Cannot Allocate.')
  =gfModalGen("QRM00000B00000",.F.,.F.,.F.,lcStatus)
ENDIF
*--If a nonactive account and the message appeard before, skip it.
IF (lnProcess = 1 .OR. lnProcess = 4) AND Customer.Status <> 'A' AND llNotAct
  RETURN
ENDIF
*--If user trying to pick or allocate an order with status on hold Get a message :
*--                "Order status on hold! continue?"
*--                       <Yes><No>
IF (lnProcess = 1 .OR. lnProcess = 4) AND Ordhdr.Status = 'H' AND !llOrdSt
  llOrdSt = .T.
  lcMes = "One or more on hold order found. Continue?"
  IF gfModalGen("QRM00000B32000",.F.,.F.,.F.,lcMes) = 2
    llSkpHold = .T.
  ELSE
    llSkpHold = .F.
  ENDI
ENDIF
IF (lnProcess = 1 .OR. lnProcess = 4) AND Ordhdr.Status = 'H' AND llSkpHold
  SELECT STYLE
  RETURN
ENDIF    
IF lnRec <= 1 AND (lnProcess = 1 .OR. lnProcess = 4) .AND. OrdLine.TotQty = 0
  SELECT STYLE
  RETURN
ENDIF

*** Are you sure you want to release this picking ticket? ***
*** <  Yes  > - <  No  > ***
IF lnProcess = 3  AND !llRelease
   *-- Release one or more than one piktkt.
   llRelease = .T.
   lcMes = IIF(ORDLINE.PIKTKT = "******","Are you sure you want to de-allocate?","Are you sure you want to release selected picking tickets?")
   IF gfModalGen("QRM00000B32000",.F.,.F.,.F.,lcMes) = 2
     llFatExt = .T.
     SELECT STYLE
     RETURN
   ENDIF
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
  llFatExt = .T.
ELSE
  DO CASE
    CASE lnProcess = 1 .OR. lnProcess = 4
      *-- If picking a previous allocated picking ticket, just assign
      *-- a picking ticket # & add it to the piktkt file.
      IF lnProcess = 1 .AND. "*" $ lcPikTkt
        =lfPikAlo()
        *--If the order has existing piktkts and the user doen't want to add new ones.
        IF llHasPikNo AND !llAddNew
          RETURN
        ENDIF
      ELSE
        *-- Call local function to allocate the current line.
        =lfAloLine(llModiDye , PADR(laData[1],19) , lcPikWare)
        IF llFatExt = .T.
          RETURN
        ENDIF
        *-- If succeed to pick automatically & the picking method was "Maulay".
        IF !EMPTY(OrdLine.PikTkt) .AND. lnPicMethd = 2
          =lfModiLine(llModiDye , PADR(laData[1],19) , lcPikWare , lcDyelot)
        ENDIF
      ENDIF
      *B603529,1 KHM 03/26/2000 (Begin) If allocating or picking then move
      *B603529,1                the cursor to the next record in the browse.

      IF !EMPTY(OrdLine.PikTkt)
        SELECT ORDLINE
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
      *-- Call local function to modify the current line.
      IF "*" $ lcPikTkt
        lcOrgWare = lcPikWare
      ELSE
        lcOrgWare = IIF(SEEK(PADR(lcPikTkt,6) , "PIKTKT") , PIKTKT.cWareCode , lcPikWare)
      ENDIF
      =gfOpenFile(gcDataDir+'Pack_hdr',gcDataDir+'Pack_hdr','SH')
      llModCurLin = .T.
      *-- Do not apply packing condition upon modify if it is allocated line only [Start
      IF PADR(lcPikTkt,6) <> "******" AND SEEK(PADR(lcPikTkt,6) , "Pack_hdr") AND ;
        PADR(Pack_hdr.piktkt,6) = PADR(lcPikTkt,6)
        *-- Do not apply packing condition upon modify if it is allocated line only [End
        *** Picking ticket #: {PADR(lcPikTkt,6)} has a packing list. Cannot modify! ***
        *** < Ok > ***
        =gfModalGen("INM44060B00000" , "DIALOG" , PADR(lcPikTkt,6))
        *-- Set the modify flag to false to stop release.
        llModCurLin = .F.
      ENDIF
      =gfCloseFile('Pack_hdr')
      *--If approve release, execute the release function.
      IF llModCurLin
        =lfModiLine(llModiDye , PADR(laData[1],19) , lcOrgWare , lcDyelot)
      ENDIF
    CASE lnProcess = 3
      *-- Call local function to release the current line.
      IF "*" $ lcPikTkt
        lcOrgWare = lcPikWare
      ELSE
        lcOrgWare = IIF(SEEK(PADR(lcPikTkt,6) , "PIKTKT") , PIKTKT.cWareCode , lcPikWare)
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
        =lfRelLine(llModiDye , PADR(laData[1],19) , lcOrgWare)
      *B602545,1 Reham On 02/22/99   *** Begin ***
      ENDIF
      *B602545,1 Reham On 02/22/99   *** End   ***
  ENDCASE
  
  *-- Unlock all the locked files.
  SET MULTILOCKS OFF
  
  *-- Get the inventory totals.
  =lfSayTotal()
  
  *-- Execute the browse when function to refresh the control buttons.
  =lfwBrwRef()
ENDIF
SELECT STYLE

*!*************************************************************
*! Name      : lfAloLine
*! Developer : Adel Mohammed el Gazzar
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
*-- Declare arrays and variabel to be used in checking 
*-- if we will increase order Qty to mach the available stock [Begin]
DIMENSION laIncrsOrd[8]   && logical array to check if we will increase order Qty

lnAllocate = 0         
*-- Open Style & Scale files to get the Sizes of this style
IF !USED("STYLE")
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
ENDIF
IF !USED("SCALE")
  =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
ENDIF  
STORE 0 TO laPikQty
*-- Call local function to edit dyelot.
IF llModiDye .AND. lfNoDye(PADR(lcStyle,19) , lcPikWare , @lcDyelot)
  *** No dyelots selected.  Cannot allocation. ***
  *** <  OK  > ***
  =gfModalGen("INM44015B00000" , "DIALOG")
  =SEEK(PADR(lcStyle,19)+lcPikWare+SPACE(10) , 'StyDye')
  lcDyelot = ""
  llFatExt = .T.
  RETURN
ELSE
  IF (!RLOCK('StyDye') .OR. !lfRecLock('StyDye' , 'Style Dyelot'))
    RETURN
  ENDIF
ENDIF
lcDyelot = IIF(lcDyelot='.F.' , '' , lcDyelot)
lcFrmFile  = "STYDYE"
llForce    = .F.

*-- Check the available stock for allocation.
IF (&lcFrmFile..TotStk - &lcFrmFile..TotAlo) <= 0
  *** No stock available for this line.  Force allocation? ***
  *** <  Yes  > - <  No  > ***
  lnPressed    = lfForceMsg("M44016")
  IF lnPressed = 2
    IF llModiDye
      =SEEK(PADR(lcStyle,19)+lcPikWare+SPACE( 10 ), 'StyDye' )
    ENDIF  
    RETURN
  ENDIF
  llForce = .T.
ENDIF

IF llForce
  SCATTER FIELDS OrdLine.Qty1, OrdLine.Qty2, OrdLine.Qty3, OrdLine.Qty4,;
                 OrdLine.Qty5, OrdLine.Qty6, OrdLine.Qty7, OrdLine.Qty8,;
                 OrdLine.TotQty TO laPikQty
ELSE
  IF SEEK(&lcFrmFile..Style,'STYLE') AND SEEK('S'+Style.Scale,"SCALE")
    DECLARE laIncrsOrd[Scale.cnt]
  ENDIF  
  FOR lnI = 1 To ALEN(laIncrsOrd,1)
   lcI = STR(lnI,1)
   laIncrsOrd[lnI] = &lcFrmFile..Stk&lcI - OrdLine.Qty&lcI >= 0
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
      lnAllocate = gfModalGen('TRM44104B44002', 'DIALOG',lcSize)
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
          laPikQty[lnI] = &lcFrmFile..Stk&lcI
          laPikQty[9]    = laPikQty[9] + laPikQty[lnI]
      ENDCASE  
    ELSE
      laPikQty[lnI] = OrdLine.Qty&lcI  
      laPikQty[9]    = laPikQty[9] + laPikQty[lnI]
    ENDIF 
  ENDFOR
  *-- IF total Pik > 0 , so this line will be allocate or pick
  llPicked = laPikQty[9] > 0
  IF !llPicked
     *** No avilable pieces to allocate. ***
     *** < Ok > ***
    =gfModalGen("INM44017B00000" , "DIALOG")
    RETURN
  ENDIF
ENDIF
lcHdrKey  = OrdHdr.cOrdType + OrdHdr.Order
lnDiffAmt = (laPikQty[9] - OrdLine.TotPik)*Ordline.Price
IF !lfChkAprov(lcHdrKey , lnDiffAmt)
  RETURN
ENDIF
*-- Assign a picking ticket no. for the current order line.
IF lnProcess = 1
  lcPikTkt = lfGetPkTkt(OrdLine.order , OrdHdr.cDivision , OrdLine.Store , lcPikWare)
  IF llFatExt
    RETURN
  ENDIF
ELSE
  lcPikTkt = "******"
ENDIF
*-- Add record for the current picking ticket in the picking ticket file.
SELECT PikTkt
IF !SEEK(lcPikTkT)
  APPEND BLANK
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
*--- While allocating check if the pick from
*--- warehouse is different than the order warehouse 
*--- then replace the ordline warehouse with the
*--- pick from warehouse
IF OrdHdr.cWareCode <> lcPikWare
  REPLACE OrdLine.cWareCode WITH lcPikWare
ENDIF
*-- end of lfAloLine.



*!*************************************************************
*! Name      : lfRelLine
*! Developer : Adel Mohammed el Gazzar
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

REPLACE OrdLine.cWareCode WITH OrdHdr.cWareCode,;
        OrdLine.lSubstSz  WITH .F.

=lfOnlyLine(lcPikTkt)
*-- end of lfRelLine.


*!*************************************************************
*! Name      : lfModiLine.                      
*! Developer : Adel Mohammed el Gazzar
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

lcCurAlias = ALIAS()
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
IF !EMPTY(lcCurAlias)
  SELECT (lcCurAlias)
ENDIF
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
  laAvl[lnCount] = lfTotAvlbl(lcCount)
  laOrd[lnCount] = OrdLine.Qty&lcCount
  laPik[lnCount] = OrdLine.Pik&lcCount
  laAvl[9] = laAvl[9] + laAvl[lnCount]
  laOrd[9] = laOrd[9] + laOrd[lnCount]
  laPik[9] = laPik[9] + laPik[lnCount]
  lcPikStat&lcCount = IIF(lnCount <= lnScaleCnt , "ENABLE" , "DISABLE")
  *C102318
  SHOW GET laAvl[lnCount]
  SHOW GET laOrd[lnCount]
  lcPikSt = lcPikStat&lcCount
  SHOW GET laPik[lnCount] &lcPikSt
ENDFOR
SHOW GET laPik[9] &lcPikSt
=lfrefresh()
*-- Clear the trapped keys.
ON KEY LABEL ALT+B 
*=lfClrTrap()
lcHdrKey   = OrdHdr.cOrdType + OrdHdr.Order
lnChkAprov = lfChkAprov(lcHdrKey , 0.00 , .T.)
DIMENSION laOldPik[ALEN(laPik,1)]
=ACOPY(laPik,laOldPik)
*-- Call screen to be able to modify the current line.
*DO (gcScrDir+gcWinAppl+"\ALMDLIN.SPX")
*-- Restore the trapped keys.
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcAloBrwTl)

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : Adel Mohammed el Gazzar
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
    IF lfNoDye(PADR(lcStyle,19) , lcOrdWare , @lcDyelot)
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
              laAvl[lnCount] = StyDye.Stk&lcCount - laPik[lnCount]
              laAvl[9] = StyDye.TotStk - laPik[9]
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
*! Developer : Adel Mohammed el Gazzar
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

*-- Force user to values greater than or equal zero.
IF laPik[lnPikNo] < 0
  *Message : 44081 ==> A negative value is not allowed.
  *Button  : 00000 ==> < Ok >
  =gfModalGen('INM44081B00000', 'DIALOG')
  laPik[lnPikNo] = lcOldValue
  _CUROBJ    = _CUROBJ
  RETURN
ENDIF
*-- Force user to values greater than or equal zero. [end  ]

lcPikNo   = ALLTRIM(STR(lnPikNo))
lcSelFile = 'StyDye'
*--ADEL

*-- If the allocated qty. > qty entered in the ordline file & > available stock.
IF laPik[lnPikNo] > OrdLine.Pik&lcPikNo .AND. ;
  (laPik[lnPikNo] - OrdLine.Pik&lcPikNo) > lfTotAvlbl(lcPikNo)
  *E301225,1 Allow Force allocation option [Begin]
  *** No available pieces could be allocated ***
  *** for this size, force allocation?       ***
  *** < Yes > - < No > ***
  lnPressed    = lfForceMsg("M44027",.T.)
  IF lnPressed = 1
    llModiOrd = .T.
    laPik[9]  = laPik[1] + laPik[2] + laPik[3] + laPik[4] + laPik[5] + laPik[6] + laPik[7] + laPik[8]
  ELSE
    *-- Get the order line qty.
    laPik[lnPikNo] = OrdLine.Pik&lcPikNo
  ENDIF
ELSE
  *-- If allocated qty > order line qty., warn the user.
  IF laPik[lnPikNo] > OrdLine.Qty&lcPikNo OR ;
     OrdLine.TotQty<laPik[1] + laPik[2] + laPik[3] + laPik[4] + laPik[5] + laPik[6] + laPik[7] + laPik[8]
    *C102318
    llOverPack = OrdLine.TotQty<laPik[1] + laPik[2] + laPik[3] + laPik[4] + laPik[5] + laPik[6] + laPik[7] + laPik[8]
    llSowMes = .F.
    IF !ORDlINE.lSubstSz
      FOR lnFrom = 1 TO 8
        lcFrom = STR(lnFrom,1)
        *--Don't recheck the same element.
        IF lnFrom <> lnPikNo AND;
           OrdLine.Qty&lcFrom-laPik[lnFrom] >= laPik[lnPikNo] - OrdLine.Qty&lcPikNo
           lcMes = "Quantity allocated is greater than ordered for this size."+;
                    "Do you want to .."
           IF gfModalGen("QRM00000B44013",.F.,.F.,.F.,lcMes) = 2
             REPLACE ORDLINE.lSubstSz WITH .T.
             llModiOrd = .F.  
           ELSE
             llModiOrd = .T.  
           ENDIF
           llSowMes = .T.
           EXIT
        ENDIF   
      ENDFOR
    ENDIF  
    IF (llOverPack AND !llSowMes) OR (!ORDlINE.lSubstSz AND !llModiOrd)
      *** Quantity allocated is greater than ordered!  Increase order? ***
      *** <  Yes  > - <  No  > ***
      IF gfModalGen("INM44002B00006" , "DIALOG") = 1
        llModiOrd = .T.
      ELSE
        laPik[lnPikNo] = OrdLine.Pik&lcPikNo
        RETURN
      ENDIF  
    ENDIF  
  ENDIF
  laPik[9]  = laPik[1] + laPik[2] + laPik[3] + laPik[4] + laPik[5] + laPik[6] + laPik[7] + laPik[8]
ENDIF
IF llChkAprov AND (laPik[lnPikNo] <> laOldPik[lnPikNo])
  =lfSetAprv()
ENDIF  
laAvl[lnPikNo] = MAX(StyDye.Stk&lcPikNo - laPik[lnPikNo],0)
laAvl[9] = laAvl[1]+laAvl[2]+laAvl[3]+laAvl[4]+laAvl[5]+laAvl[6]+laAvl[7]+laAvl[8]
SHOW GET laPik[lnPikNo]
SHOW GET laPik[9]
=lfvClsMod()
=lfSayTotal()
=lfRefresh('lcAlStyAl0')

*!*************************************************************
*! Name      : lfvClsMod                      
*! Developer : Adel Mohammed el Gazzar
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

*C102318
lcStyle = PADR(laData[1],19)

*** This line will be removed from this picking ticket.  Continue? *** 
*** <  Yes  > - <  No  > ***
IF laPik[9] <= 0 .AND. !EMPTY(ORDLINE.PIKTKT) AND gfModalGen("QRM44021B00006" , "DIALOG") = 2
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
  
  IF laOrd[9] <= 0 OR laPik[9] <= 0
    =lfOnlyLine(lcPikTkt)
  ENDIF
ENDIF

*CLEAR READ

*!*************************************************************
*! Name      : lfAloQty
*! Developer : Adel Mohammed el Gazzar
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
*! Developer : Adel Mohammed el Gazzar
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
  IF !SDYEBROW(PADR(lcStyle,19) , @lcDyelot, .T., lcOrdWare,.T.)
    llGoOut = .T.
  ENDIF
ENDIF 
RETURN llGoOut

*!*************************************************************
*! Name      : lfModiOrd
*! Developer : Adel Mohammed el Gazzar
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
*-- end of lfModiOrd.

*!*************************************************************
*! Name      : lfOrdQty
*! Developer : Adel Mohammed el Gazzar
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
*! Developer : Adel Mohammed el Gazzar
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
=gfOpenFile(gcDataDir+'PikLine',gcDataDir+'PikLine','SH')
*-- Save the record no. in the order line file.
SELECT OrdLine
lnCurRec   = RECNO()
lcCurOrd   = OrdLine.Order
SET ORDER TO TAG ORDLINE
*-- Check if there is any order line use the same picking ticket.
SCAN REST WHILE Order = lcCurOrd FOR PikTkt = lcPikTkt
  *-- Set the delete flag to false.
  llDelete = .F.
  EXIT
ENDSCAN
SET ORDER TO TAG ORDLINES
*-- Restore the record no.
IF lnCurRec <= RECCOUNT("OrdLine") .AND. lnCurRec > 0
  GOTO lnCurRec
ENDIF
IF llDelete .AND. !("*" $ lcPikTkt)
  *-- Change the piktkt status to be canceled.
  IF SEEK(lcPikTkt , "PikTkt")
    REPLACE PikTkt.Status WITH "X"
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
    GATHER FROM laReleased FIELDS Pik1,Pik2,Pik3,Pik4,Pik5,Pik6,Pik7,Pik8,TotPik,Picked,PikTkt,PikDate,Dyelot
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfSqlTherm
*! Developer : Adel Mohammed el Gazzar
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
*! Developer : Adel Mohammed el Gazzar
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

IF llHasPikNo AND !llAddNew
  RETURN
ENDIF

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

*!*************************************************************
*! Name      : lfRecLock
*! Developer : Adel Mohammed el Gazzar
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
PARAMETERS lcMessage,llOneSize
PRIVATE lcButton , lnAction , lcMsgParm , llForceMe
STORE '' TO lcButton , lcMsgParm
lnAction = 0

DO CASE 
  CASE !llOneSize
     IF lfForceOpt()
       IF !llShowMes
         llShowMes = .T.
         lcMes = "One or more order line has no stock available. Do you want to force allocation?"
         IF gfModalGen("QRM00000B32000",.F.,.F.,.F.,lcMes) = 1
           llForAll = .T.
         ELSE   
           llForAll = .F.
         ENDIF
       ENDIF  
     ELSE
       llForAll = .F.
     ENDIF
     lnAction = IIF(llForAll,1,2)
     RETURN lnAction
  CASE llOneSize
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
ENDCASE  
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

lnTotal = 0
*------------- this function is to calculate the availble qty depend on setting
*------------- in AL module  ( the availble = STQ - ALO ) or
*------------- (the availble qty = WIP + STQ - ALO )
*--lcSize  --> hold '9' if the function is called to get total availble qty
*--			   hold ('1'->'8') called to get availble by size
*--lnTotal --> hold the availble qty
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
  IF puPikWare # 0
    =SEEK(laData[1]+laWareHous[puPikWare,2]+SPACE(10),lcSelFile)
    lnWip = IIF(lcSize = '9',&lcSelFile..TotWIP,&lcSelFile..WIP&lcSize)
    IF BETWEEN(lnRecNo, 1, RECCOUNT(lcSelFile))
      GO  lnRecNo IN &lcSelFile
    ENDIF
    IF lcSize = '9'		   && the total qty
      lnTotal = lnWip + &lcSelFile..TotStk - &lcSelFile..TotALO
    ELSE					   && by size
      lnTotal = lnWip + &lcSelFile..Stk&lcSize - &lcSelFile..Alo&lcSize
    ENDIF
  ENDIF
ENDIF
RETURN(lnTotal)

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Adel Mohammed el Gazzar
*! Date      : 07/22/2001
*! Purpose   : Validate Select button.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed parameters : lcOrdNo
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*C102318
FUNCTION lfvSelect

PRIVATE lnAlias
lnAlias = SELECT()
IF SEEK(ORDLINE.ORDER,lcOrdTmp)
  IF !EMPTY(ORDLINE.STORE)
    SELECT (lcOrdTmp)
    LOCATE REST WHILE Order+key  = ORDLINE.ORDER FOR STORE = ORDLINE.STORE
  ENDIF  
  *-- If Select
  IF EMPTY(&lcOrdTmp..cSel)
    SHOW GET pbSelect,1 ENABLE PROMPT '\<Unselect'
    REPLACE &lcOrdTmp..cSel WITH 'S'
  ELSE
    *-- If UnSelect
    SHOW GET pbSelect,1 ENABLE PROMPT 'Se\<lect' 
    REPLACE &lcOrdTmp..cSel WITH ''
*            &lcOrdTmp..Key  WITH ''    
  ENDIF
  =lfSetBut()
  SHOW WINDOW (lcAloBrwTl) REFRESH SAME  
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Adel Mohammed el Gazzar
*! Date      : 07/22/2001
*! Purpose   : Validate Select All button.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed parameters : lcOrdNo
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*C102318
FUNCTION lfvSelAll

PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcOrdTmp)
REPLACE ALL cSel WITH 'S'
SHOW GET pbSelect,1 ENABLE PROMPT '\<Unselect'
=lfSetBut()
SHOW WINDOW (lcAloBrwTl) REFRESH SAME  

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvSelNon
*! Developer : Adel Mohammed el Gazzar
*! Date      : 07/22/2001
*! Purpose   : Validate Select Non button.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed parameters : lcOrdNo
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*C102318
FUNCTION lfvSelNon

PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcOrdTmp)
REPLACE ALL cSel WITH ''
SHOW GET pbSelect,1 ENABLE PROMPT '\<Select'
=lfSetBut()
SHOW WINDOW (lcAloBrwTl) REFRESH SAME  

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvInvert
*! Developer : Adel Mohammed el Gazzar
*! Date      : 07/22/2001
*! Purpose   : Validate Invert button.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*C102318
FUNCTION lfvInvert

PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcOrdTmp)
REPLACE ALL cSel WITH IIF(!EMPTY(cSel),'','S')
IF EMPTY(&lcOrdTmp..cSel)
  SHOW GET pbSelect,1 ENABLE PROMPT '\<Unselect'
ELSE
  SHOW GET pbSelect,1 ENABLE PROMPT 'Se\<lect' 
ENDIF
=lfSetBut()
SHOW WINDOW (lcAloBrwTl) REFRESH SAME  
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfSetBut
*! Developer : Adel Mohammed el Gazzar
*! Date      : 07/22/2001
*! Purpose   : Set buttons mode.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*C102318
FUNCTION lfSetBut

PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcOrdTmp)
LOCATE FOR EMPTY(cSel)
lcSelAll = IIF(FOUND(),'ENABLE','DISABLE')
LOCATE FOR !EMPTY(cSel)
lcSelNon = IIF(FOUND(),'ENABLE','DISABLE')
SHOW GET pbSelAll &lcSelAll
SHOW GET pbSelNon &lcSelNon
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGetPkTkt                      
*! Developer : Adel Mohammed el Gazzar
*! Date      : 07/22/2001
*! Purpose   : Get a picking ticket for the sended order no.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed parameters : lcOrdNo
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*! Example           :  =lfGetPkTkt(lcOrdNo , OrdHdr.Division)
*!*************************************************************
FUNCTION lfGetPkTkt

PARAMETERS lcOrdNo , lcDivision , lcStore , lcWareCode , lnAskType

IF TYPE('lnAskType') $ 'UL' 
  lnAskType = 0
ENDIF

PRIVATE lcOrdNo   , lcDivision , lcCurAlias , lcPikTkt , ;
        llOpnPkTk , lnOldTag   , lnRecPkTk  , laPikNo  , lcExact , llAskUser

llAskUser = (lnAskType = 0)
*-- Save the exact setting.
lcExact = SET("EXACT")

*-- Define array hold the piktkts no. , status , printed or not.
DECLARE laPikNo[1]

*-- Save the current alias.
lnCurAlias = SELECT()

*-- Initialize the needed variables.
STORE ""  TO laPikNo , lcPikTkt
STORE .F. TO llOpnPkTk
STORE 0   TO lnOldTag   , lnRecPkTk

SELECT PikTkt
lnOldTag = INT(VAL(SYS(21)))
SET ORDER TO TAG OrdPik
*-- Save the current record no. if the file was open.
lnRecPkTk = RECNO("PikTkt")

SELECT PikTkt
IF SEEK(lcOrdNo , "PikTkt")  AND !llAddNew
  LOCATE REST WHILE Order = lcOrdNo;
         FOR !(PikTkt.Status $ "CX") .AND. ;
         Store = lcStore .AND. cWareCode = lcWareCode        
  IF FOUND() AND !llHasPikNo
    llHasPikNo = .T.
    lcMes = "One or more order has existing picking ticket(s)." + ;
            "You can add a new picking ticket or manipulate it later. Do you want to add a new one ?"
    IF gfModalGen("QRM00000B32000",.F.,.F.,.F.,lcMes) = 2
      llAddNew = .F.
      llFatExt = .T.
    ELSE
      llAddNew = .T.
      lcPikTkT = gfSequence('PIKTKT', '', '', lcDivision)
    ENDI
  ELSE
    lcPikTkT = gfSequence('PIKTKT', '', '', lcDivision)
  ENDIF
ELSE
  lcPikTkT = gfSequence('PIKTKT', '', '', lcDivision)
ENDIF
SELECT PikTkt
SET ORDER TO lnOldTag
*-- File was opened in previous session, Set the opinter to th right record.
IF lnRecPkTk > 0 .AND. lnRecPkTk <= RECCOUNT("PikTkt")
  GOTO lnRecPkTk
ENDIF

*-- Restore the exact setting.
SET EXACT &lcExact
*-- Restore the old alias.
SELECT (lnCurAlias)
RETURN lcPikTkt

