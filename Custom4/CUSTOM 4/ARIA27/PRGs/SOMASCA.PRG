*:----------------------------------------------------------------------------------------------
*: Program file        : SMMASCA.PRG
*: Program description : Mass Order Cancellation - Order Shaving
*: For screen          : SMMASCA.SPR
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SM)
*: Developer Name      : Hossam El Etreby (HDM)
*: Tracking Job Number : E300970
*:----------------------------------------------------------------------------------------------
*: Calls               : SMMASCA.SPR
*:----------------------------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------------------------
*: Example             : DO SMMASCA
*:----------------------------------------------------------------------------------------------
*: Modification
*:B602179,1 AMM 11/25/1998 Erase temporary files when quitting the program
*:B602343,1 HDM 12/13/1998 Fixing Bug of Incorrect Header Status
*:E301077,6 HDM 12/20/1998 Open Needed Files Using gfOpenFile
*:E602374,1 HDM 12/20/1998 Update Cancelled amount as well
*:B602396,1 HDM 12/31/1998 Incorrect fabric seek
*:B602409,1 HDM 01/03/1999 Disable\enabel Buttons according to temp File State
*:B602425,1 HDM 01/06/1999 Write correct Po# Browse Opens
*:B602421,1 HDM 01/06/1999 Filter Orders And Contracts
*:B602414,1 HDM 01/06/1999 Incorrect Qty Header
*:B602423,1 HDM 01/07/1999 Incorrect CT Header Status
*:B602417,1 HDM 01/10/1999 We Shouldn't Include Pikked Order Lines In cancelation process 
*:B602475,1 HDM 01/26/1999 Check The CutPick File to avoid cancelling picked CT & PO
*:B602511,1 HDM 02/11/1999 Mark Session as completed if user selected not to releas the allocation
*:B602512,1 HDM 02/11/1999 Open cutting Ticket file with the needed Order
*:B602505,1 HDM 02/11/1999 Operato / Operand Not Found When CT Releasing allocation
*:B602534,1 HDM 02/16/1999 The second sales order line Cut Fileds was not updated
*:                         as we should use the order line# in seek Ecpresion
*:B602531,1 HDM 02/16/1999 Cancel whatever available according to user selection
*:B602559,1 HDM 02/19/1999 Remove the established relations before opening the option grid
*:B602573,1 HDM 02/23/1999 In order shaving we have to open CutTktH % L Files
*:B602560,1 HDM 02/23/1999 Scan Should be on line level IN CUTPICK File
*:B602597,1 HDM 02/28/1999 Allow user to edit shaved quintities
*:B602601,1 HDM 03/01/1999 Update WIP & WO Quantities in stydye & style files
*:B602604,1 HDM 03/01/1999 Incorrect Ord Total quantity
*:B602605,1 HDM 03/01/1999 Open the uncomplete session file using order
*:                         to allow gfsetup saving the correct information about it
*:B602527,1 HDM 03/07/1999 We have to Update the estimated cost Elements using style file costs
*:B602677,1 HDM 03/21/1999 Fix Bug of incorrect Total Quantity Shaving
*:B801961,1  HS  03/23/1999 Declare some new arrays that will be used in the
*:B801961,1                 filter expression that will be returned by the
*:B801961,1                 option grid in the case of "In list" operator.
*:B602676,1 HDM 03/24/1999  Also we have to RELEASE totcut in ordhdr file
*:B802166,1 HDM 04/21/1999  Fill the transaction type popup according to Installed modules
*:B602671,1 HDM 05/04/1999  When updating PO or CT if they are hold we have to update the header file as well
*:B602918,1 HDM 05/27/1999  We should not cancel the sales order line QTY if the total cancelled QTYs = 0
*:B802665,1 HDM 09/30/1999  1- We should take in consideration the shipped Qty when updating OrdHdr file
*:                             as it affects the open Qty
*:                          2- Default the Transaction type to the active module
*:B603194,1 HDM 10/12/1999  1- We Shouldn't update WIP in Style File
*:                          2- Validate the style range only if not empty
*:B603368,1 AMM 01/06/2000  Don't release allocation if the user has no access on the cut ticket screen
*:B803403,1 ABD 08/01/2000 Fix bug that wrong update for the boooked Qty when cancelling a bulk order.
*:B603785,1 ADEL 08/08/2000 1- When C/T or PO hide camcellation reason window.
*:B603785,1                 2- Make the trapping work
*:B603785,1                 3- Fix the bug of "Variable 'Icogprompt' not found".
*:B802430,1 ADEL 09/18/2000 Fix the bug of not enabling <Select All> buuton when pressing <Invert>.
*:B803671,1 AAN  09/19/2000 Don't open CutPick unless PO or MF or PS installed.
*:B803652,1 ADEL 08/20/2000 - We update OrdCanLn when :
*:B803652,1                   1- Deleting whole Prtially-shiped order(s).
*:B803652,1                   2- Deleting line by line after depletion date.
*:B803652,1                 - Update OrdCanLn with Style,Account,Store and Dyelot.
*:B604019,1 ADEL 11/16/2000 - Update OrdCanLn with Style Dyelot for PO and  CT also.
*:B803636,1 AAN 30/10/2000  Change the spelling of Cancelation to Cancellation in SO module.
*:C200149,1 ABD 02/20/2001  Add new triger FOR ENG10.
*:B604258,1 SSE 06/14/2001 Fix bug of wrong update of order header in case of bulk order.
*:B604004,1 SSE 06/17/2001 Fix bug of an error message when pressing CTRL+EDIT in Order Shaving.
*:B604500,1 SSE 06/24/2001 Updating the Price field in Order Cancellation file with Order line price.
*:B604592,1 AME 07/26/2001 Fix the bug of not saving cancel reson in case of delete the whole order.
*:B804386,1 SSE 09/18/2001 Fix bug of not updating the right warehouse in StyDye file.
*:B604107,1 AAN 09/26/2001 Fix bug of when we back from browse window too O.G. and back again to 
*:B604107,1 AAN			   browse window and press any button an error message will appear.
*:E301865,1 AMH 06/25/2002 Add Inter-location PO to the mass cancellation.
*:B606242,1 TMI 08/22/2002 fix the bug that when transaction type is changed the Trans#  is not cleared
*:B606049,1 RAE 08/20/2002 Modify the performance.
*:B606402,1 TMI 11/06/2002 Fix the bug that the cancelled qty in ordcanln is not the same as in ordhdr.CANCEL field
*:B606802,1 AMH 12/18/2002 Fix the bug of syntax error in mass po cancellation screen.
*:B606743,1 WAB 04/07/2003 WO and WIP figures in Cut and Sold using dyelots    
*:B607207,1 KHM 06/01/2003 Fix the problem of not displaying records to cancel when selecting
*:B607207,1                by style inspite of there are records to display because of fixing 
*:B607207,1                bug # 606049
*:B607117,1 KHM 06/01/2003 Change the transaction number (Order) to inlist instead of between.
*:B121328,1 NNA 01/28/2004 Fix bug of save the cancellation date in the posln file
*:B125052,1 NNA 12/06/2004 Fix bug that if you filtered for style or color or date only
*:B125052,1 NNA            you'll get 'There is no Record' even there are Po's have this info.
*:B125052,1 NNA            this bug occurs by cause of bug# 606049.
*:B128212,1 MMR 07/14/2005 Fix bug of no records if i include only this color in filter.
*:B128212,2 MMR 09/14/2005 Fix bug of not cancelling some PO numbers.
*:B129483,1 NNA 09/18/2005 Fix bug that when you're using mass order cancellation the cancelled date field not filled
*:----------------------------------------------------------------------------------------------

*-- Preparing Screen Variables
STORE '' TO lcMasCanCh1 , lcMasCanCh2 , lcExpr ,;
            lcOldPikTk  , lcOldStyle  , lcSession  , lcCurrObj  , lcTmpFil  ,;
            lcPrtCond   , lcStylePct  , lcMasCaCh0 , cMasCaCh1  , lcMasCaCh2,;
            lcOgOldExp   , lcPrevCanT  , lcActAlias , lcTmpCur   , lcBrowTtl ,;
            lcRpCrt , lcOldChld , lcClose,lcOldVal, lcOldBrTtl
            
STORE 0 TO lnSelRec , lnDelRec , lnUnCmRec , lnUnSelRec , lnShavePer

STORE 1 TO lnRpPrtFlg , lnOPrtFlag

STORE 'O' TO lcRpCanTyp  && Pre Definetion to be in safe-side

STORE .F. TO llCallScop , llDyelot , llMultiWH , llSekPak ,;
             llContinue , llCallUnCS , LLSEL
STORE .F. TO llOkCancel , llUpdPoCt
*--B602531,1 HDM Flag variables to check allocation messages status[Start]
STORE .F. TO llPickMsg , llAlocMsg , llAcordMsg
*--B602531,1 HDM Flag variables to check allocation messages status[end]
*B603368,1 AMM Initialize variable to get holds user privilige on the cut ticket
*llOGStart = .T.
STORE .T. TO llOGStart , llUsrCPrv , llUsrSPrv , llUsrPPrv
*B603368,1 AMM end
DECLARE laOgFxFlt[1,6] , laReason[1,2] , laCodes[1,10] , laReason1[1]
STORE '' TO lareason
*B801961,1 Add this line to declare some new arrays that will be used in the
*          filter expression that will be returned by the option grid in
*          the case of "In list" operator [Begin]
DIMENSION laHdFlt[1] , laFxFlt[1] , laVrFlt[1]
*B801961,1 Add this line to declare some new arrays that will be used [End]
*B803671,1 [Begin] Don't open CutPick unless PO or MF or PS installed.
llOpenCut = ('PO' $ gcCmpModules OR 'MF' $ gcCmpModules OR 'PS' $ gcCmpModules)
*B803671,1 [End]
lnSessNo = gnProgCopy   && Session #

laCodes[1,1] = 'CCANCRESON'
laCodes[1,2] = 'laReason'
laCodes[1,3] = 'puReason'
laCodes[1,4] = 'D'
laCodes[1,5] = .F.
laCodes[1,6] = .F.
laCodes[1,7] = ''
laCodes[1,8] = ''
laCodes[1,9] = ''
laCodes[1,10] = 'CCANCRESON'
*-- Open Data files Needed
*llNthing   = lfOpenFile()

lnBrRecNo = 1                     && Varible to hold the browse record number
lcBrowTtl = 'Sales Order'  && Varible to hold the browse title
lnBrRecNo = 1                  && Varible to hold the browse record number

DECLARE laFileStru[1] , laScopExpr[3,3] , laUsedFile[9,3] ,;
        laVarTSav[1] , laSavScop[1,7]
laVarTSav[1] = ''
*laVarTSav[2] = ''
laScopExpr = ''
laScopExpr[2,1] = {}
laScopExpr[2,2] = {}
laUsedFile = ''
*B803652,1 (Begin)  Declare array to get depletion date
DECLARE laSetups[1,2] 
laSetups[1,1]  = 'M_CANAFTER'       && Number of days need to calculate order completion date
=gfGetMemVar(@laSetups,gcAct_Comp)
STORE .F. TO llSndLnToC,llSndOrToC,llUpdOrdCa
*B803652,1 (End)


DIMENSION laPanelObj(2,3)    && Array to add new buttons to the control panel
laPanelObj[1,1] = 'pbScop'
laPanelObj[1,2] = gcBmpHome + 'SCOPE.BMP'
laPanelObj[1,3] = 'VALID lfvScope()'
laPanelObj[2,1] = 'pbRel'
laPanelObj[2,2] = gcBmpHome + 'RELEASE2.BMP'
laPanelObj[2,3] = 'VALID lfRelScr()'

lnSessNo = gnProgCopy

IF !gfSetup()
  RETURN
ENDIF

llNoShow = .F.         && Flag to make the screen call the PROCEDURE lpShow evry time it runs
*lcScFields = 'PIKTKT' && Varible to hold the screen fields
llFrstTime = .T.       && Flag to know if we are going to call lpShow for the first time

IF !WEXIST(gcBaseWind)
  lcMasCaCh0  = gfTempName()
  lcMasCaCh1  = gfTempName()
  lcMasCaCh2  = gfTempName()
  lcTmpFil    = gfTempName()


  *--HDM B602288,1 12/06/1998 Variable window name[start]
  DO CASE
    CASE gcAct_appl = 'MF'
      *lcWindTitle = 'Mass Cutting Ticket Cancelation'
      lcWindTitle = 'Mass Cutting Ticket Cancellation'
    CASE gcAct_appl = 'PO'
      *lcWindTitle = 'Mass Purchase Order Cancelation'
      lcWindTitle = 'Mass Purchase Order Cancellation'
    OTHERWISE
      *:B803636 AAN Change the spelling of Cancelation to Cancellation in SO module[Start].
      *lcWindTitle = 'Mass Order Cancelation'
      lcWindTitle = 'Mass Order Cancellation'
      *:B803636 AAN Change the spelling of Cancelation to Cancellation in SO module[End].
  ENDCASE
  *--HDM B602288,1 12/06/1998 Variable window name[end]
  *--Open The Uncomlete session file
  *--HDM B602605,1[start] Open the uncomplete session file using order
  *--              to allow gfsetup saving the correct information about it
  *= gfOpenFile(gcDataDir + 'UNCMSESS' ,'','SH')
  = gfOpenFile(gcDataDir + 'UNCMSESS' ,'TRANS','SH')
  *--HDM B602605,1[End]
  *-- Uncomplete session
  llContinue = lfFndUnCSe()
  lcSession = IIF(llContinue , lcSession , gfSequence('CSESSION'))

  IF !llContinue
    llCallScop = .T.
  ENDIF   && End of IF

  IF !USED('CODES')
    = gfOpenFile(gcDataDir + 'CODES' ,'','SH')
  ENDIF


  *--- First Show an empty Browser
   *llNthing   = lfOpenFile()
   llNthing   = lfCrtTmp()      && Create the Temp File Based on the ORDLINE File
   *B603368,1 AMM Get the previlige on the cut ticket screen
   llUsrCPrv = gfUserPriv('MF','MFCUTKT')
   llUsrSPrv = gfUserPriv('SO','SOORD')
   llUsrPPrv = gfUserPriv('PO','POSTY')
   *B603368,1 AMM end
ENDIF
IF TYPE('lcMasCaCh1')$'NU'
  lcMasCaCh1 = lcOldChld
ENDIF
*-- Preparing Option Grid Variables
lcStyTitle  = gfItemMask('HI')
lcStylePic  = "@! " + gfItemMask('PM')
lcStyMajor  = gfItemMask('HM')
lnMajorLen  = LEN(lcStylePic)

lcColorTlt  = ''
lcNonMajTlt = ''
lcNonMajPi  = ''
lcFree_Clr  = ''
lnNonMajSt  = 0
lnFreeLen   = 0
lnColorLen  = 0

lnMajSeg    = gfItemMask('SM')  && No. of major segments.

*-- Compute Free/Color Items in Style Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you didn't find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]

*--------- Run the screen
DO (gcScrDir + 'SOMASCA.SPX')

*B602179,1 AMM Erase temporary files when quitting the program
IF glQuitting
  IF USED(lcTmpFil)
    USE IN (lcTmpFil)
  ENDIF
  ERASE &gcWorkdir.&lcTmpFil..DBF  
  ERASE &gcWorkdir.&lcTmpFil..CDX  
  ERASE &gcWorkdir.&lcTmpFil..FPT  
ENDIF
*B602179,1 AMM 11/25/98 end

RELEASE WINDOW (lcBrowTtl)
POP KEY
*------------------------------------------------------------------
*!*************************************************************
*! Name        : lfWhenRep
*! Developer   : Hossam El Etreby (HDM)
*! Date        : 04/08/1998
*! Purpose     : Option Grid When Function
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfWhenRep

*--HDM B602288,1 12/06/1998 Variable window name[start]
  DO CASE
    CASE gcAct_appl = 'MF'
      *MODIFY WINDOW GWDGRID TITLE 'Mass Cutting Ticket Cancelation'
      MODIFY WINDOW GWDGRID TITLE 'Mass Cutting Ticket Cancellation'
    CASE gcAct_appl = 'PO'
      *MODIFY WINDOW GWDGRID TITLE 'Mass Purchase Order Cancelation'
      MODIFY WINDOW GWDGRID TITLE 'Mass Purchase Order Cancellation'
    OTHERWISE
      *:B803636 AAN Change the spelling of Cancelation to Cancellation in SO module[Start].
      *MODIFY WINDOW GWDGRID TITLE 'Mass Order Cancelation'
      MODIFY WINDOW GWDGRID TITLE 'Mass Order Cancellation'
      *:B803636 AAN Change the spelling of Cancelation to Cancellation in SO module[Start].
  ENDCASE
  
=lfvCrt()
*--HDM B602288,1 12/06/1998 Variable window name[end]


*!*************************************************************
*! Name      : lfvAccount
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for Validating Transaction #
*!*************************************************************
*! Called from : Transaction # [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvAccount
PRIVATE lcVar , lcObj , laTemp,lcBrowFields,llFromBrw

STORE .F. TO llFromBrw

lcVar = SYS(18)
lcObj = EVALUATE(SYS(18))
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
lcPrevAl = SELECT()
STORE '' TO lcBrowCond
DECLARE laTemp[1]
DO CASE
  CASE lcRpCanTyp = 'O' .OR. lcRpCanTyp = 'S'
    *-- B602421,1 HDM 01/06/1999 Filter Orders And Contracts [start]
    lcBrowCond = [For cOrdType = lcRpCrt]
    *-- B602421,1 HDM 01/06/1999 Filter Orders And Contracts [end]

    SELECT ORDHDR
    SET ORDER TO ORDHDR
    DIMENSION laTemp[1]
    IF !EMPTY(lcObj) AND !(SEEK(lcRpCrt + lcObj , 'ORDHDR'))
      *--HDM B602458,1 If ordhdr doesn't contain orders or contracts display message[start]
      IF SEEK(lcRpCrt , 'ORDHDR')
        lcBrFields = "Order :H = 'Order#':R ,lcSesDesc = gfCodDes(SEASON,'SEASON'):H = 'Season':31,cFacCode :H = 'Factor':R ,Complete :H = 'Complete':R ,OpenAmt :H = 'Open Amnt.' :R ,Approval :H = 'Aprvl-No'   :R ,ApprAmt :H = 'Approved'   :R"
        lNThing    = gfBrows(lcBrowCond,'ORDER','laTemp')
        llFromBrw = .T.
      ELSE
        IF lcRpCrt = 'O'
          lcMsg = 'No orders found'
          =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
        ELSE
          lcMsg = 'No contarcts found'
          =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
        ENDIF
      *--HDM B602458,1 If ordhdr doesn't contain orders or contracts display message[End]
      ENDIF
    ENDIF
    
  CASE lcRpCanTyp = 'P'

    *-- B602421,1 HDM 01/06/1999 Filter Orders And Contracts [start]
    lcBrowCond = [For cStyType = IIF(lcRpCrt='O','P','C')]
    *-- B602421,1 HDM 01/06/1999 Filter Orders And Contracts [end]
    *--mhm
    *lcBrowTtl = 'Purchase Order'
    *--mhm
    SELECT POSHDR
    SET ORDER TO POSHDR
    DIMENSION laTemp[1]
    *--HDM B602425,1[START]
    *IF !EMPTY(lcObj) AND !(SEEK(lcRpCrt + lcObj , 'POSHDR'))

    IF !EMPTY(lcObj) AND !(SEEK(IIF(lcRpCrt='O','P','C') + lcObj , 'POSHDR'))
      *--HDM B602458,1 If poshdr doesn't contain orders or contracts display message[Start]
      IF SEEK(IIF(lcRpCrt='O','P','C') , 'POSHDR')
        *--HDM B602425,1[END]
        lcBrFields = "PO        :R :H= 'PO #'    :8  ,"+;
                     "Vendor    :R :H= 'Vendor'  :11 , "+;
    	             "Complete  :R :H= 'Complete':8  ,"+;
  	                 "nStyOrder :R :H= 'Tot.Qty.':7  ,"+;
                     "POTotal   :R :H= 'Amount'  :10 ,"+;
                     "Receive   :R :H= 'Receive' :7  ,"+;
                     "Open      :R :H= 'Open'    :7"
        lNThing    = gfBrows(lcBrowCond,'PO','laTemp')
        llFromBrw = .T.
      ELSE
        IF lcRpCrt = 'O'
          lcMsg = 'No orders found'
          =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
        ELSE
          lcMsg = 'No contarcts found'
          =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
        ENDIF
      *--HDM B602458,1 If poshdr doesn't contain orders or contracts display message[End]
      
      ENDIF
    ENDIF
    
  *E301865,1 AMH Add Inter-location PO [Start]
  CASE lcRpCanTyp = 'N'
    lcBrowCond = [For cStyType = 'N']
    SELECT POSHDR
    SET ORDER TO POSHDR
    DIMENSION laTemp[1]
    IF !EMPTY(lcObj) AND !(SEEK('N' + lcObj , 'POSHDR'))
      IF SEEK('N' , 'POSHDR')
        lcBrFields = "PO        :R :H= 'PO #'    :8  ,"+;
    	             "Complete  :R :H= 'Complete':8  ,"+;
  	                 "nStyOrder :R :H= 'Tot.Qty.':7  ,"+;
                     "POTotal   :R :H= 'Amount'  :10 ,"+;
                     "Receive   :R :H= 'Receive' :7  ,"+;
                     "Open      :R :H= 'Open'    :7"
        lNThing    = gfBrows(lcBrowCond,'PO','laTemp')
        llFromBrw = .T.
      ELSE
        lcMsg = 'No orders found'
        =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
      ENDIF
    ENDIF
  *E301865,1 AMH [End]
  
  CASE lcRpCanTyp = 'C'
    *--mhm
    *lcBrowTtl = 'Cutting Ticket'
    *--mhm
    SELECT CUTTKTH
    SET ORDER TO CUTTKTH
    DIMENSION laTemp[1]
    IF !EMPTY(lcObj) AND !(SEEK(lcObj , 'CUTTKTH'))
    lcBrFields ="CUTTKT    :R :H='CUTTKT'   ,"+;
                "STYLE     :R :H='Style'    ,"+;
                "STATUS    :R :H='Status'   ,"+;
                "ENTERED   :R :H='Issue'    ,"+;
                "COMPLETE  :R :H='Complete' ,"+;
                "SEASON    :R :H='Season'   ,"+;
                "CDIVISION :R :H='Division' ,"+;
                "PCS_BUD   :R :H='Budget'   ,"+;
                "PCS_REC   :R :H='Received' ,"+;
                "PCS_DAM   :R :H='Damaged'  ,"+;
                "PCS_OPN   :R :H='Open' "

      lNThing    = gfBrows(lcBrowCond,'CUTTKT','laTemp')
      llFromBrw = .T.
    ENDIF
ENDCASE
*-- IF The user selected a record
IF llFromBrw 
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = laOldVal
  ENDIF
ENDIF

&lcVar = lcObj      && Update the field

SELECT (lcPrevAl)
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfOldVal
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Keeps the old value of the editor when cancel the browse
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfOldVal

laOldVal = EVALUATE(SYS(18))
lcOldVal = EVALUATE(SYS(18))

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfMajPic
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Get the major segment picture
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvStyle
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for Validating Style Code
*!*************************************************************
*! Called from : Style Field [Option Grid]
*!*************************************************************
*! Calls       : gfStyBrw()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()
IF EMPTY(EVAL(lcStyle))
  RETURN
ENDIF
lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE
*B603194,1 HDM [Start] If moving in & out in Ragne fields just seek
*                      Don't clear the object value

*IF LASTKEY() = 13 AND !MDOWN()
*B603194,1 HDM [End]
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
*B603194,1 HDM [Start] If moving in & out in Ragne fields just seek
*                      Don't clear the object value
*ELSE
*  &lcStyle = ''
*ENDIF
*B603194,1 HDM [End]

SET ORDER TO lcTag IN STYLE
*------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvNonMaj
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for validating non-major Segment
*!*************************************************************
*! Called from : Only this color [Option Grid]
*!*************************************************************
*! Calls       : gfStyBrw()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvNonMaj

lcStyle = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('N',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE
*-------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfNMajType
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Function For calculating major and non-major segments
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : lcFree_Clr IF Not Empty
*!*************************************************************

FUNCTION lfNMajType
PARAMETERS lcNMajType,lnMajSegs

*-- Loop Around Non Major elements.
FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)

  IF laMajSegs[lnI,1] = lcNMajType

    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
    lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
                     lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

    lcNonMajTl = IIF(EMPTY(lcNonMajTl),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                     lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)
*-- end of lfNMajType. 
*-------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvFabric
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for validating Fabric Code
*!*************************************************************
*! Called from : Only this color [Option Grid]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvFabric

lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.
IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC
IF LASTKEY() = 13 AND !MDOWN()
  *--HDM B602396,1 [START]
  IF SEEK(PADR(lcFab,7),'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
  *--HDM B602396,1 [END]
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC

IF llUseByMe
  USE IN FABRIC
ENDIF  

*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : Fill Criteria Field In Option Grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSortDumy()
*!*************************************************************
FUNCTION lfSortDumy

*E301865,1 AMH Move these lines to the transaction type validation function [Start]
*IF TYPE('llOGStart') = 'L' AND !EMPTY(VARREAD())
*  lcRpCanTyp = laTypeVal(EVAL(VARREAD()))
*ENDIF
*E301865,1 AMH [End]

DO CASE
  CASE lcRpCanTyp = 'O' .OR. lcRpCanTyp = 'S'
    llSuppress = .F.
    DIMENSION laSortDesc[2,1] , laSortVal[2,1]    

    laSortDesc[1] = 'Order'
    laSortDesc[2] = 'Contract'
    *laSortDesc[3] = 'Both'

    laSortVal[1] = 'O'
    laSortVal[2] = 'C'
    *laSortVal[3] = 'B'
    IF lcRpCanTyp = 'O'
      lcBrowTtl = 'Sales Order'
    ELSE
      lcBrowTtl = "Sales Order Shaving 'CTRL+ENTER To edit shaved Quantity'"
    ENDIF

  *E301865,1 AMH Consider case of inter location PO [Start]
  *CASE lcRpCanTyp = 'P'
  CASE lcRpCanTyp $ 'PN'
  *E301865,1 AMH [End]

    llSuppress = .F.
    *--MHM decrease array number by one
    *DIMENSION laSortDesc[3,1] , laSortVal[3,1]    
    DIMENSION laSortDesc[2,1] , laSortVal[2,1]    
    *--MHM
    laSortDesc[1] = 'Order'
    laSortDesc[2] = 'Contract'
    *laSortDesc[3] = 'Both'

    laSortVal[1] = 'O'
    laSortVal[2] = 'C'
    *laSortVal[3] = 'B'
    *--mhm
    *lcBrowTtl = 'Purchase Order'
    *--mhm
  CASE lcRpCanTyp = 'C'

    llSuppress = .F.
    llSuppress = .T.
    *--mgm
    *lcBrowTtl = 'Cutting Ticket Order'
    *--mhm
ENDCASE
*--The End
*----------------------------------------------------------------
*!*************************************************************
*! Name      : lfvCanTyp
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : Validation Function For Cancellation Type Pop Up
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCanTyp()
*!*************************************************************
FUNCTION lfvCanTyp

IF llOGStart

  *E301865,1 AMH Update the lcRpCanTyp variable [Start]
  IF TYPE('llOGStart') = 'L' AND !EMPTY(VARREAD())
    lcRpCanTyp = laTypeVal(EVAL(VARREAD()))
  ENDIF
  *E301865,1 AMH [End]
  
  llNThing = lfSortDumy()
  CLEAR READ            && Issue CLEAR READ Only in lcRpCanTyp Validation
  laOgFxFlt[1,6]="|"    &&*606242 TMI clear Trans# when TransType is changed
  =lfvCrt()
ENDIF

*!*************************************************************
*! Name      : lfvCust
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvCust
PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
*-- IF The user wants to Browse or the Account is not in the file
SET ORDER TO TAG CUSTOMER IN CUSTOMER
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*---------------------------------------------------------------------------
*                      Screen Functions
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfDispBrow
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Browses the TEMP File Records
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfDispBrow()
*!*************************************************************

FUNCTION lfDispBrow

*--mhm
*lnBrRecNo  = RECNO()
IF EOF()
  lnBrRecNo = RECCOUNT()
ELSE
  lnBrRecNo = RECNO()
ENDIF  
DO CASE
  CASE lcRpCanTyp = 'O' .OR. lcRpCanTyp = 'S'
    IF lcRpCanTyp = 'O'
      lcBrowTtl = 'Sales Order'
    ELSE
      lcBrowTtl = "Sales Order Shaving 'CTRL+ENTER To edit shaved Quantity'"
    ENDIF

  CASE lcRpCanTyp = 'P'
    lcBrowTtl = 'Purchase Order'
  
  *E301865,1 AMH Add Inter-location PO [Start]
  CASE lcRpCanTyp = 'N'
    lcBrowTtl = 'Inter-Location Purchase Order'
  *E301865,1 AMH [End]
  
  CASE lcRpCanTyp = 'C'
    lcBrowTtl = 'Cutting Ticket Order'
    
ENDCASE

*--mhm
DO CASE
  CASE lcRpCanType = 'O' .OR. lcRpCanType = 'S'
  *--HDM B602414,1 Incorrect Qty Header[Start]
  *  lcBrowStr = "lcMarker=IIF(RECNO() = lnBrRecNo,'>',' '):1:H=' ':W = .F., "+;
  *              "lcSelect=IIF(LLSEL , '»', ' '):R :H='»',"+;
  *              "CORDTYPE  :R :H= 'Order Type' :10 ,"+;
  *              "ORDER     :R :H= 'Order'      :9  ,"+;
  *              "ACCOUNT   :R :H= 'Account'    :10 ,"+;
  *              "CWARECODE :R :H= 'Warehouse'  :13 ,"+;
  *              "STORE     :R :H= 'Store'      :16 ,"+;
  *              "STYLE     :R :H= 'Style'      :16 ,"+;
  *              "QTY1      :R :H= 'Qty1'       :6  ,"+;
  *              "QTY2      :R :H= 'Qty2'       :6  ,"+;
  *              "QTY3      :R :H= 'Qty2'       :6  ,"+;
  *              "QTY4      :R :H= 'Qty2'       :6  ,"+;
  *              "QTY5      :R :H= 'Qty2'       :6  ,"+;
  *              "QTY6      :R :H= 'Qty2'       :6  ,"+;
  *              "QTY7      :R :H= 'Qty2'       :6  ,"+;
  *              "QTY8      :R :H= 'Qty2'       :6  ,"+;
  *              "TOTQTY    :R :H= 'TOTQTY'     :6  ,"+;
  *              "PRICE     :R :H= 'Price'      :11 "

    lcBrowStr = "lcMarker=IIF(RECNO() = lnBrRecNo,'>',' '):1:H=' ':W = .F., "+;
                "lcSelect=IIF(LLSEL , '»', ' '):R :H='»',"+;
                "CORDTYPE  :R :H= 'Order Type' :10 ,"+;
                "ORDER     :R :H= 'Order'      :9  ,"+;
                "ACCOUNT   :R :H= 'Account'    :10 ,"+;
                "CWARECODE :R :H= 'Warehouse'  :13 ,"+;
                "STORE     :R :H= 'Store'      :16 ,"+;
                "STYLE     :R :H= 'Style'      :16 ,"+;
                "QTY1      :R :H= 'Qty1'       :6  ,"+;
                "QTY2      :R :H= 'Qty2'       :6  ,"+;
                "QTY3      :R :H= 'Qty3'       :6  ,"+;
                "QTY4      :R :H= 'Qty4'       :6  ,"+;
                "QTY5      :R :H= 'Qty5'       :6  ,"+;
                "QTY6      :R :H= 'Qty6'       :6  ,"+;
                "QTY7      :R :H= 'Qty7'       :6  ,"+;
                "QTY8      :R :H= 'Qty8'       :6  ,"+;
                "TOTQTY    :R :H= 'TOTQTY'     :6  ,"+;
                "PRICE     :R :H= 'Price'      :11 "

  *--HDM B602414,1 Incorrect Qty Header[End]

  CASE lcRpCanType = 'P'
    lcBrowStr = "lcMarker=IIF(RECNO() = lnBrRecNo,'>',' '):1:H=' ':W = .F., "+;
                "lcSelect=IIF(LLSEL , '»', ' '):R :H='»',"+;
                "CSTYTYPE  :R :H= 'Order Type' :10 ,"+;
                "PO        :R :H= 'Order#'      :9  ,"+;
                "VENDOR    :R :H= 'Account'    :10 ,"+;
                "CWARECODE :R :H= 'Warehouse'  :13 ,"+;
                "STYLE     :R :H= 'Style'      :16 ,"+;
                "QTY1      :R :H= 'Qty1'       :6  ,"+;
                "QTY2      :R :H= 'Qty2'       :6  ,"+;
                "QTY3      :R :H= 'Qty2'       :6  ,"+;
                "QTY4      :R :H= 'Qty2'       :6  ,"+;
                "QTY5      :R :H= 'Qty2'       :6  ,"+;
                "QTY6      :R :H= 'Qty2'       :6  ,"+;
                "QTY7      :R :H= 'Qty2'       :6  ,"+;
                "QTY8      :R :H= 'Qty2'       :6  ,"+;
                "TOTQTY    :R :H= 'TOTQTY'     :6  ,"+;
                "PRICE     :R :H= 'Price'      :11 "

  *E301865,1 AMH Add Inter-location PO [Start]
  CASE lcRpCanType = 'N'
    lcBrowStr = "lcMarker=IIF(RECNO() = lnBrRecNo,'>',' '):1:H=' ':W = .F., "+;
                "lcSelect=IIF(LLSEL , '»', ' '):R :H='»',"+;
                "CSTYTYPE  :R :H= 'Order Type' :10 ,"+;
                "PO        :R :H= 'Order#'     :9  ,"+;
                "VENDOR    :R :H= 'Source'     :10 ,"+;
                "CWARECODE :R :H= 'Target'     :13 ,"+;
                "STYLE     :R :H= 'Style'      :16 ,"+;
                "QTY1      :R :H= 'Qty1'       :6  ,"+;
                "QTY2      :R :H= 'Qty2'       :6  ,"+;
                "QTY3      :R :H= 'Qty2'       :6  ,"+;
                "QTY4      :R :H= 'Qty2'       :6  ,"+;
                "QTY5      :R :H= 'Qty2'       :6  ,"+;
                "QTY6      :R :H= 'Qty2'       :6  ,"+;
                "QTY7      :R :H= 'Qty2'       :6  ,"+;
                "QTY8      :R :H= 'Qty2'       :6  ,"+;
                "TOTQTY    :R :H= 'TOTQTY'     :6  ,"+;
                "PRICE     :R :H= 'Price'      :11 "
  *E301865,1 AMH [End]
  
  CASE lcRpCanType = 'C'
      lcBrowStr = "lcMarker=IIF(RECNO() = lnBrRecNo,'>',' '):1:H=' ':W = .F., "+;
                "lcSelect=IIF(LLSEL , '»', ' '):R :H='»',"+;
                "CUTTKT    :R :H= 'Cut Ticket' :10 ,"+;
                "STYLE     :R :H= 'Style'      :16 ,"+;
                "QTY1      :R :H= 'Qty1'       :6  ,"+;
                "QTY2      :R :H= 'Qty2'       :6  ,"+;
                "QTY3      :R :H= 'Qty3'       :6  ,"+;
                "QTY4      :R :H= 'Qty4'       :6  ,"+;
                "QTY5      :R :H= 'Qty5'       :6  ,"+;
                "QTY6      :R :H= 'Qty6'       :6  ,"+;
                "QTY7      :R :H= 'Qty7'       :6  ,"+;
                "QTY8      :R :H= 'Qty8'       :6  ,"+;
                "TOTQTY    :R :H= 'TOTQTY'     :6   "
ENDCASE

SELECT (lcTmpFil)
GOTO  TOP
BROWSE FIELDS &lcBrowStr               ;
       WINDOW (lcMasCaCh1)             ;
       WHEN lfwBrows() .AND. lfvpbSel();
       VALID :F lfvBrows()             ;  
       IN WINDOW (gcBaseWind)          ;
       LOCK 0                          ;
       NOAPPEND                        ;
       NOCLEAR                         ;
       NODELETE                        ;
       NOWAIT                          ;
       NOEDIT                          ;
       NOMENU                          ;
       SAVE                            ;
       TITLE lcBrowTtl
*--HDM B602597,1 Allow editing shaved quantities[start]
ON KEY LABEL CTRL+ENTER DO lpvBrow
*--HDM B602597,1 Allow editing shaved quantities[end]
*B603785,1 (Begin) Trap Browse window.
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTtl)
*B603785,1 (End)
*----------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Browse Traping
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfBrowTrap()
*!*************************************************************

FUNCTION lfBrowTrap

RETURN .F.
*----------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Browse UNTraping
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfBrwUnTrp()
*!*************************************************************

FUNCTION lfBrwUnTrp
*----------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Browse UNTraping
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfwIbBrow()
*!*************************************************************

FUNCTION lfwIbBrow
*--------------------------------------------------------------------------
*!*************************************************************
*! Name      : lpShow
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Handle The special case for screen modes
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpShow
*!*************************************************************

PROCEDURE lpShow

SHOW GET pbEdt     DISABLE
SHOW GET pbBrws    DISABLE
SHOW GET pbcpPrint DISABLE
SHOW GET pbSlct    DISABLE
SHOW GET pbDlt     DISABLE

laCtrStat[7]  = "DISABLE"                && Edit button 
laCtrStat[8]  = "DISABLE"                && Delete button
laCtrStat[9]  = "DISABLE"                && Select button
laCtrStat[10] = "DISABLE"                && Browse button

SHOW MENU _mSysMenu

*IF Statment to check if we are going to call the option grid for the user
*IF these is the first time for these session of the program
IF llCallScop
  SHOW GET pbRel DISABLE
  llNthing   = lfvScope()
  IF lcRpCanTyp $'OS'
    SHOW GET puReason ENABLE
  ELSE
    SHOW GET puReason DISABLE
  ENDIF
  llCallScop = .F.
ENDIF  && End of IF

llFrstTime = .F.

*--B602409,1 HDM 01/03/1999 Disable\enabel Buttons according to temp File State[start]
IF USED(lcTmpFil)
  IF EOF(lcTmpFil)
    SHOW GET pbSelect DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbSelNon DISABLE
    SHOW GET pbInvert DISABLE
  ELSE
    SHOW GET pbSelect ENABLE
    SHOW GET pbSelAll ENABLE
    SHOW GET pbSelNon ENABLE
    SHOW GET pbInvert ENABLE
  ENDIF
ENDIF
*--B602409,1 HDM 01/03/1999 Disable\enabel Buttons according to temp File State[end]

lnBrRecNo  = RECNO(lcTmpFil)
IF TYPE('lcExpr') <> 'L'
  SHOW WINDOW (lcBrowTtl) REFRESH
ENDIF

*------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvScope
*! Developer : Hossam El Etreby
*! Date      : 09/13/1997
*! Purpose   : Valid function of push button Scope
*!*************************************************************
*! Called from : Control Panel , lpShow
*!*************************************************************
*! Calls       : gfOpGrid() , lfCrT() , lfDispBrow()
*!*************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
FUNCTION lfvScope

ON KEY LABEL CTRL+ENTER
*-- Get the current cancellation Type
lcPrevCanT = lcRpCanTyp
llSuppress = .F.
IF USED ('ORDHDR')
  SELECT ORDHDR
  IF !EMPTY(SET('RELATION'))
    SET RELATION TO
  ENDIF
ENDIF
IF USED ('ORDLINE')
  SELECT ORDLINE
  IF !EMPTY(SET('RELATION'))
    SET RELATION TO
  ENDIF
ENDIF
IF USED('POSHDR')
  SELECT POSHDR
  IF !EMPTY(SET('RELATION'))
    SET RELATION TO
  ENDIF
ENDIF

IF USED('POSLN')
  SELECT POSLN
  IF !EMPTY(SET('RELATION'))
    SET RELATION TO
  ENDIF
ENDIF
*--- HDM B602559,1 Remove the prev Relations [START]
*IF USED('CUTKTH')
*  SELECT CUTKTH
*  IF !EMPTY(SET('RELATION'))
*    SET RELATION TO
*  ENDIF
*ENDIF

*IF USED('CUTKTL')
*  SELECT CUTTKTL
*  IF !EMPTY(SET('RELATION'))
*    SET RELATION TO
*  ENDIF
*ENDIF

IF USED('CUTKTH')
  SELECT CUTTKTH
  IF !EMPTY(SET('RELATION'))
    SET RELATION TO
  ENDIF
ENDIF

IF USED('CUTTKTL')
  SELECT CUTTKTL
  IF !EMPTY(SET('RELATION'))
    SET RELATION TO
  ENDIF
ENDIF

*--- HDM B602559,1 Remove the prev Relations [END]

IF USED ('STYLE')
  SELECT STYLE
  IF !EMPTY(SET('RELATION'))
    SET RELATION TO
  ENDIF
ENDIF
DIMENSION laSortDesc[2,1] , laSortVal[2,1]
lcOldChld = lcMasCaCh1

*E301865,1 AMH Set the transaction type to default [Start]
lcRpCanTyp = 'O'
*E301865,1 AMH [End]

*B607117,1 KHM 06/01/2003 (Begin) Saving the ESC key and restore it after calling the option grid.
lcEscapeKy = ON("KEY","ESC")
lcExpr    = gfOpGrid('SOMASCAN' , .T.)
*:B128212,1 MMR 07/14/2005 Fix bug of no records if i include only this color in filter.[Start]
lnColorPos   = lfItmPos('SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)')

*B129483,1 NNA 09/18/2005 (Start) Fix Bug Related To B#128212 that Generate Error massage "Subscript out of bounds'
*IF !EMPTY(laFxFlt[lnColorPos,6])
IF lnColorPos>0 AND !EMPTY(laFxFlt[lnColorPos,6])
*B129483,1 NNA (End)

  DIMENSION laColor[1]
  = gfSubStr(laFxFlt[lnColorPos,6],@laColor,'|')         
  FOR I = 1 TO ALEN(laColor)
    IF laColor[I] $ (lcExpr)
      lcExpr = STRTRAN(lcExpr,laColor[I],ALLTRIM(laColor[I]))
    ENDIF
  ENDFOR
ENDIF
*:B128212,1 MMR [End]
ON KEY LABEL ESCAPE &lcEscapeKy
*B607117,1 KHM 06/01/2003 (End)

*B603785,1 (Begin) Toggling windows.
IF lcRpCanTyp $'OS'
  MODIFY WINDOW (lcMasCaCh1) AT 5.1,0.75 SIZE 14.4,109.75
ELSE
  MODIFY WINDOW (lcMasCaCh1) AT 0.5,0.75 SIZE 18.60,109.75
ENDIF  
*B603785,1 (End)
IF lcRpCanTyp $'OS'
  SHOW GET puReason ENABLE
ELSE
  SHOW GET puReason DISABLE
ENDIF

laVarTSav[1] = lcExpr

IF llCallScop
  lcOgOldExp = lcExpr      && Fill the old Expr With Expr.

  *khalid
  lcOldBrTtl =  lcBrowTtl

ENDIF

*IF The user Selected to cancel [Close] From the Option Grid

IF TYPE('lcExpr') = 'L'
  IF TYPE('lcOgOldExp') = 'U'
    lcOgOldExp = " "
  ENDIF
  lcExpr = lcOgOldExp             && Restore the old Expr.

  *khalid
  lcBrowTtl = lcOldBrTtl
  
  *B606802,1 AMH Save the correct variables [Start]
  laVarTSav[1] = lcExpr
  *B606802,1 AMH [End]
  

ELSE    && Else
  *-- Open all Needed Files as the option grid closed them
  llNthing  = lfOpenFile()
  *---
*-----------  Set relations according Cancellation Type and Criteria

DO CASE
  CASE lcRpCanTyp = 'O' .OR. lcRpCanTyp = 'S'
  *--  Sales Order Relations
    SELECT ORDLINE
    SET ORDER TO Style IN Style
    IF EMPTY(SET('RELATION'))
      SET RELATION TO Ordline.style INTO Style ADDITIVE
    ENDIF
    SELECT ORDHDR
    SET ORDER TO Ordhdr IN Ordhdr
    IF EMPTY(SET('RELATION'))
      SELECT ORDLINE
      SET RELATION TO IIF(lcRpCrt = 'O','O',;
                      IIF(lcRpCrt = 'C','C',ORDLINE.CORDTYPE)) +;
                      Ordline.order INTO Ordhdr ADDITIVE
    ENDIF
*--
  CASE lcRpCanTyp = 'C'

*--  Cut Ticket Relations
    SELECT CUTTKTL
    SET ORDER TO Style IN Style
    IF EMPTY(SET('RELATION'))
      SET RELATION TO style INTO Style ADDITIVE
    ENDIF
    SET ORDER TO Cuttkth IN Cuttkth
    SELECT Cuttkth
    IF EMPTY(SET('RELATION'))
      SELECT CUTTKTL
      SET RELATION TO Cuttktl.cuttkt INTO Cuttkth ADDITIVE
    ENDIF
    *B603785,1 (Begin)
    lcBrowTtl = 'Cutting Ticket'
    *B603785,1 (End)
*--
  CASE lcRpCanTyp = 'P'
*--  Purchase Order Relations
    SELECT POSLN
    SET ORDER TO Style IN Style
    IF EMPTY(SET('RELATION'))
      SET RELATION TO style INTO Style ADDITIVE
    ENDIF

    SET ORDER TO Poshdr IN Poshdr
    SELECT POSHDR
    IF EMPTY(SET('RELATION'))
      SELECT POSLN
      SET RELATION TO IIF(lcRpCrt="C","C",;
                      IIF(lcRpCrt="P","P",Posln.cstytype));
                      + Posln.po INTO Poshdr ADDITIVE
    ENDIF
    *B603785,1 (Begin) Correct the browse title.
    lcBrowTtl = 'Purchase Order'
    *B603785,1 (End)
  
  *E301865,1 AMH Add Inter-location PO [Start]
*--
  CASE lcRpCanTyp = 'N'
*--  Purchase Order Relations
    SELECT POSLN
    SET ORDER TO Style IN Style
    IF EMPTY(SET('RELATION'))
      SET RELATION TO style INTO Style ADDITIVE
    ENDIF

    SET ORDER TO Poshdr IN Poshdr
    SELECT POSHDR
    IF EMPTY(SET('RELATION'))
      SELECT POSLN
      SET RELATION TO 'N'+ Posln.po INTO Poshdr ADDITIVE
    ENDIF
    lcBrowTtl = 'Inter-Location Purchase Order'
  *E301865,1 AMH [End]
  
ENDCASE
  *--mhm2000
  *-- Check the cancellation type Changed
  *  IF lcPrevCanT <> lcRpCanTyp
  *    llNthing = lfRemTmp()        && Dlete the old Temp File
  *    llNthing = lfCrtTmp()        && Re-Create the Temp File
  *  ELSE
  *    SELECT (lcTmpFil)
  *   DELETE ALL
  *  ENDIF
  llNthing = lfRemTmp()        && Dlete the old Temp File
  llNthing = lfCrtTmp()        && Re-Create the Temp File
  *--mhm2000
  llNthing   = lfFillField()   && AnyWay Start filling the Tmp File

  SELECT (lcTmpFil)
  REPLACE ALL LLSEL WITH .F.

  IF RECNO() = 0
    *--Disable Buttons
    SHOW GET pbSelect DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbSelNon DISABLE
  ELSE
    *--Enable Buttons
    SHOW GET pbSelect ENABLE
    SHOW GET pbSelAll ENABLE
    SHOW GET pbInvert ENABLE
    SHOW GET pbSelNon DISABLE
  ENDIF
  lnBrRecNo = RECNO()
  llNthing   = lfDispBrow()    && Browse What U Got
  llCallScop = .F.             && Screen Already Initialized
  *IF The temp file [lcTmpFil] is not empty
  IF !EOF()
    laScrMode[2] = .T.
    SHOW GET pbSelect ENABLE
    SHOW GET pbSelAll ENABLE
    SHOW GET pbInvert ENABLE
    SHOW GET pbSelNon DISABLE
  ELSE    && Else
    laScrMode[1] = .T.
    SHOW GET pbSelect DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbSelNon DISABLE
  ENDIF    && End of IF
  SHOW GETS ONLY
  SHOW GET pbTop DISABLE
  SHOW GET pbPrvs DISABLE
  laCtrStat[1]  = "DISABLE"               && First button 
  laCtrStat[4]  = "DISABLE"               && Priveus button
  SHOW MENU _mSysMenu

  *khalid
  lcOldBrTtl =  lcBrowTtl
  lcOgOldExp =  lcExpr


ENDIF    && End of IF

*_CUROBJ = OBJNUM(IbBrow)
*--------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Function To Create the temp file according to Cancellation Type
*!*************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfCrtTmp
PRIVATE lcPrevAl
lcPrevAl = SELECT(0)

DO CASE      && Whitch table to get its structuer
  CASE lcRpCanTyp = 'O' .OR. lcRpCanTyp = 'S' && Order and Order Shaving
    IF !USED('ORDLINE')
      = gfOpenFile(gcDataDir + 'ORDLINE' ,'ORDLINE','SH')
    ENDIF
    SELECT ORDLINE
  
  *E301865,1 AMH Add Inter-location PO [Start]
  *CASE lcRpCanTyp = 'P'                       && Purchase Order
  CASE lcRpCanTyp $ 'PN'                       && Purchase Order
  *E301865,1 AMH [End]
  
    IF !USED('POSLN') .AND. 'PO' $ gcCmpModules
      = gfOpenFile(gcDataDir + 'POSLN' ,'POSLN','SH')
    ENDIF
    SELECT POSLN

  CASE lcRpCanTyp = 'C'                       && Cut Ticket Order
    IF !USED('CUTTKTL') .AND. 'MF' $ gcCmpModules
      = gfOpenFile(gcDataDir + 'CUTTKTL' ,'CUTTKTL','SH')
    ENDIF
    SELECT CUTTKTL

ENDCASE

= AFIELDS(laFileStru)

*-- Add Some Fields To The Structure
lnFileStru = ALEN(laFileStru,1)

DIMENSION laFileStru[lnFileStru+3,4]

laFileStru[lnFileStru+1,1] = 'LLSEL'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'NORDLNNO'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 9
laFileStru[lnFileStru+2,4] = 0

laFileStru[lnFileStru+3,1] = 'NPROCNO'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 2
laFileStru[lnFileStru+3,4] = 0

lcActAlias = SELECT(0)

CREATE TABLE &gcWorkDir.&lcTmpFil FROM ARRAY laFileStru
*B606049,1 RAE Modify the performance. [Start]
IF lcRpCanTyp = 'O'
  INDEX ON CORDTYPE+ORDER TAG 'ORDER' OF (gcWorkDir + lcTmpFil + ".CDX")
ENDIF
INDEX ON LLSEL DESCENDING TAG (lcTmpFil) OF (gcWorkDir + lcTmpFil + ".CDX")
*B606049,1 RAE [End]
*---------------------------------------------------------------------------
*!**************************************************************************
*! Name      : lfFillField
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Function To Fill The Temp Table according to user selection
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfFillField

SELECT (lcActAlias)

DO CASE
  CASE lcRpCanTyp = 'O'&& Sales Order
    *khalid
    lcOldExpVl = lcExpr
    *khalid
    IF lcRpCrt $'OC'
      *--HDM B602417,1[Start]
      *lcExpr = lcExpr + " AND CORDTYPE = '" + lcRpCrt + "'AND ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0"
      
      *B606049,1 RAE Modify the performance, use full index expr.[start]
      
      *B607207,1 KHM 06/01/2003 (Begin) Commenting all the lines fixed under bug 606049
      *B607207,1                because its completely wrong and uncomment the old line
      *B607207,1                of code for emergency issues.
      lcExpr = ALLTRIM(lcExpr) + " AND ORDLINE.CORDTYPE = '" + lcRpCrt + "'AND ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0 "
      *lcExpr = ALLTRIM(lcExpr) + " AND ORDLINE.CORDTYPE = '" + lcRpCrt + "'AND ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0 "
      *IF !(UPPER(lcExpr) == ".T.")
      *  lcExpr = ALLTRIM(STRTRAN(lcExpr,"ORDLINE.ORDER","ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)"))
      *  lnPosOrd = ATC('))),',LCEXPR)
	  *  lcFrmOrd = SUBSTR(lcExpr,lnPosOrd+5,6)
      *  lcToOrd  = SUBSTR(lcExpr,ATC(lcFrmOrd,lcExpr)+9,6)
      *  lcExpr = "CORDTYPE+ORDER+STR(LINENO,6) = '' "
      *ENDIF
      *B607207,1 KHM 06/01/2003 (End)
      
      *B606049,1 RAE [end]
      *--HDM B602417,1[End]
    ENDIF

    SELECT (lcActAlias)
    *B606049,1 RAE Modify the performance.[start]
    
    *B607207,1 KHM 06/01/2003 (Begin) Commenting all the lines fixed under bug 606049
    *B607207,1                because its completely wrong and uncomment the old line
    *B607207,1                of code for emergency issues.
    *GOTO TOP
    *SCAN FOR &lcExpr
    LOCATE
    SCAN FOR &lcExpr
    *LOCATE
    
    *IF !(UPPER(lcExpr) == ".T.")
    *  =SEEK(lcRpCrt + lcFrmOrd)
    *ELSE
    *  SET FILTER TO cordtype+order+STR(lineno,6) = "O"
    *  GO BOTTOM
    *  lcToOrd = ORDLINE.ORDER
    *  SET FILTER TO
    *  LOCATE
    *ENDIF
    *B607207,1 KHM 06/01/2003 (End)
    
    *B606049,1 RAE [Begin]

    *B607207,1 KHM 06/01/2003 (Begin) Commenting all the lines fixed under bug 606049
    *B607207,1                because its completely wrong and uncomment the old line
    *B607207,1                of code for emergency issues.
    *SCAN REST WHILE &lcExpr
    *  IF lcToOrd => ORDLINE.ORDER
    *    IF ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0
    *B607207,1 KHM 06/01/2003 (End)
    
    *B606049,1 RAE [end]
      
          SCATTER MEMVAR MEMO
          WAIT WINDOW 'Collecting Canceled quantities for order# '+ M.ORDER NOWAIT
          IF !EMPTY(ORDLINE.PIKTKT)
            M.QTY1   = M.QTY1   - ORDLINE.PIK1
            M.QTY2   = M.QTY2   - ORDLINE.PIK2
            M.QTY3   = M.QTY3   - ORDLINE.PIK3
            M.QTY4   = M.QTY4   - ORDLINE.PIK4
            M.QTY5   = M.QTY5   - ORDLINE.PIK5
            M.QTY6   = M.QTY6   - ORDLINE.PIK6
            M.QTY7   = M.QTY7   - ORDLINE.PIK7
            M.QTY8   = M.QTY8   - ORDLINE.PIK8
            M.TOTQTY = M.TOTQTY - ORDLINE.TOTPIK
          ENDIF
          IF M.TOTQTY > 0
            INSERT INTO (lcTmpFil) FROM MEMVAR
          ENDIF
          *B606049,1 RAE [start]
      *B607207,1 KHM 06/01/2003 (Begin) Commented out
      *  ENDIF
      *ELSE
      *  EXIT
      *ENDIF
      *B607207,1 KHM 06/01/2003 (End)

      *B606049,1 RAE [end]
    ENDSCAN
    *khalid
    lcExpr = lcOldExpVl  
    *khalid
  CASE lcRpCanTyp = 'S' && Order Shaving

      IF !(UPPER(lcExpr) == ".T.")
*       lcExpr = ALLTRIM(STRTRAN(lcExpr,"ORDLINE.ORDER","ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)"))
        lnPosOrd = ATC('))),',LCEXPR)
        lcFrmOrd = SUBSTR(lcExpr,lnPosOrd+5,6)
        lcToOrd  = SUBSTR(lcExpr,ATC(lcFrmOrd,lcExpr)+9,6)
        lcExpr = "CORDTYPE+ORDER+STR(LINENO,6) = '' "
      ENDIF

    IF !(UPPER(lcExpr) == ".T.")
      =SEEK(lcRpCrt + lcFrmOrd)
    ELSE
      SET FILTER TO cordtype+order+STR(lineno,6) = "O"
      GO BOTTOM
      lcToOrd = ORDLINE.ORDER
      SET FILTER TO
      LOCATE
    ENDIF    

    IF lcRpCrt $'OC'
      *--HDM B602417,1[Start]
      *lcExpr = lcExpr + " AND CORDTYPE = '" + lcRpCrt + "'AND ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0"
      *rania
      *lcExpr = ALLTRIM(lcExpr) + " AND ORDLINE.CORDTYPE = '" + lcRpCrt + "'AND ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0 "

      *B125052,1 NNA 12/06/2004 (Begin) Commenting all the lines fixed under bug 606049
      *B125052,1 NNA            because its completely wrong and uncomment the old line of code
      lcExpr = ALLTRIM(lcExpr) + " AND ORDLINE.CORDTYPE = '" + lcRpCrt + "'AND ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0 "
      *B125052,1 NNA (End)

      *--HDM B602417,1[End]
    ENDIF
    SELECT (lcActAlias)
    *B606049,1 RAE [start]
    *GO TOP
    LOCATE
   
    *B125052,1 NNA 12/06/2004 (Begin) Commenting all the lines fixed under bug 606049
    *B125052,1 NNA            because its completely wrong and uncomment the old line of code
    SCAN FOR &lcExpr
    *B606049,1 RAE [end]
    *SCAN REST WHILE &lcExpr
      *IF lcToOrd => ORDLINE.ORDER
        *IF ORDHDR.STATUS $'OH' .AND. ORDLINE.TOTQTY > 0
    *B125052,1 NNA (End)
    
          SCATTER MEMVAR MEMO
          WAIT WINDOW 'Collecting Shaved quantities for order# '+ M.ORDER NOWAIT
          IF !EMPTY(ORDLINE.PIKTKT)
            M.QTY1   = M.QTY1   - ORDLINE.PIK1
            M.QTY2   = M.QTY2   - ORDLINE.PIK2
            M.QTY3   = M.QTY3   - ORDLINE.PIK3
            M.QTY4   = M.QTY4   - ORDLINE.PIK4
            M.QTY5   = M.QTY5   - ORDLINE.PIK5
            M.QTY6   = M.QTY6   - ORDLINE.PIK6
            M.QTY7   = M.QTY7   - ORDLINE.PIK7
            M.QTY8   = M.QTY8   - ORDLINE.PIK8
            M.TOTQTY = M.TOTQTY - ORDLINE.TOTPIK
          ENDIF

          M.QTY1   = INT((M.QTY1 * lnShavePer/100))
          M.QTY2   = INT((M.QTY2 * lnShavePer/100))
          M.QTY3   = INT((M.QTY3 * lnShavePer/100))
          M.QTY4   = INT((M.QTY4 * lnShavePer/100))
          M.QTY5   = INT((M.QTY5 * lnShavePer/100))
          M.QTY6   = INT((M.QTY6 * lnShavePer/100))
          M.QTY7   = INT((M.QTY7 * lnShavePer/100))
          M.QTY8   = INT((M.QTY8 * lnShavePer/100))
          M.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
          IF M.TOTQTY > 0
            INSERT INTO (lcTmpFil) FROM MEMVAR
         ENDIF

      *B125052,1 NNA 12/06/2004 (Begin) Commenting all the lines fixed under bug 606049
      *B125052,1 NNA            because its completely wrong and uncomment the old line of code
       *ENDIF
     *ELSE
       *EXIT
     *ENDIF
      *B125052,1 NNA (End)
     
    ENDSCAN

  CASE lcRpCanTyp = 'P'                       && Purchase Order
    lcRpCrt = IIF(lcRpCrt='O','P','C')
    *khalid
    lcOldExpVl = lcExpr
    *khalid
    
    IF lcRpCrt $'PC'
      *RANIA
      *lcExpr = lcExpr + " AND CSTYTYPE = '"+ lcRpCrt + "'" + " .AND. POSHDR.STATUS $'OH' .AND. POSLN.TRANCD $'1' AND POSLN.TOTQTY > 0"
      
      *B121328,1 NNA 01/28/2004 (Begin) stop Fixes that Rania had made
      lcExpr = lcExpr + " AND CSTYTYPE = '"+ lcRpCrt + "'" + " .AND. POSHDR.STATUS $'OH' .AND. POSLN.TRANCD $'1' AND POSLN.TOTQTY > 0"
      *IF !(UPPER(lcExpr) == ".T.")
        *lcExpr = ALLTRIM(STRTRAN(lcExpr,"POSLN.PO","POSLN.CSTYTYPE+POSLN.PO+POSLN.STYLE+STR(POSLN.LINENO,6)+POSLN.TRANCD"))
        *lnPosOrd = ATC('))),',lcexpr)
        *lcFrmOrd = SUBSTR(lcexpr,lnPosOrd+5,6)
        *lcToOrd  = SUBSTR(lcexpr,ATC(lcFrmOrd,lcexpr)+9,6)
        *lcExpr = "CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = '' "
      *ENDIF
      *B121328,1 NNA (End)

    ENDIF

    *B125052,1 NNA 12/06/2004 (Begin) Commenting all the lines fixed under bug 606049
    *B125052,1 NNA            because its completely wrong and uncomment the old line of code
    *IF !(UPPER(lcExpr) == ".T.")
      *=SEEK(lcRpCrt + lcFrmOrd)
    *ELSE
      *SET FILTER TO CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = "P"
      *GO BOTTOM
      *lcToOrd = POSLN.PO
      *SET FILTER TO
      *LOCATE
    *ENDIF
    SCAN FOR &lcExpr
    *SCAN REST WHILE &lcExpr
      *IF lcToOrd => POSLN.PO
       *IF POSHDR.STATUS $'OH' .AND. POSLN.TRANCD $'1' .AND. POSLN.TOTQTY > 0
    *B125052,1 NNA (End)

          SCATTER MEMVAR MEMO
          WAIT WINDOW 'Collecting Canceled quantities for order# '+ M.PO NOWAIT
          *--Check Transaction Code[start]
          lnCurRec = RECNO()
          SCAN FOR PO = M.PO AND LINENO = M.LINENO
            IF TRANCD $'245'
              M.QTY1 = M.QTY1 - QTY1
              M.QTY2 = M.QTY2 - QTY2
              M.QTY3 = M.QTY3 - QTY3
              M.QTY4 = M.QTY4 - QTY4
              M.QTY5 = M.QTY5 - QTY5
              M.QTY6 = M.QTY6 - QTY6
              M.QTY7 = M.QTY7 - QTY7
              M.QTY8 = M.QTY8 - QTY8
              M.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
            ENDIF
          ENDSCAN
          GOTO (lnCurRec)
          *--Check Transaction Code[end]  
          INSERT INTO (lcTmpFil) FROM MEMVAR

      *B125052,1 NNA 12/06/2004 (Begin) Commenting all the lines fixed under bug 606049
      *B125052,1 NNA            because its completely wrong and uncomment the old line of code
        *ENDIF
      *ELSE
        *EXIT
      *ENDIF
      *B121328,1 NNA (End)

    ENDSCAN
    SELECT (lcActAlias)
  
    *khalid
    lcExpr = lcOldExpVl
    *khalid

  *E301865,1 AMH Add Inter-location PO [Start]

  CASE lcRpCanTyp = 'N'                       && Inter-Locarion Purchase Order

    *khalid
    lcOldExpVl = lcExpr 
    *khalid
  
    *rae
    *B125052,1 NNA 12/06/2004 (Begin) Commenting all the lines fixed under bug 606049
    *B125052,1 NNA            because its completely wrong and uncomment the old line of code

    lcExpr = lcExpr + " AND CSTYTYPE = '"+ lcRpCrt + "'" + " .AND. POSHDR.STATUS $'OH' .AND. POSLN.TRANCD $'1' AND POSLN.TOTQTY > 0"
    *IF !(UPPER(lcExpr) == ".T.")
      *lnPosOrd = ATC('))),',lcexpr)
      *lcFrmOrd = SUBSTR(lcexpr,lnPosOrd+5,6)
      *lcToOrd  = SUBSTR(lcexpr,ATC(lcFrmOrd,lcexpr)+9,6)
      *lcExpr = "CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = '' "
    *ENDIF

    *IF !(UPPER(lcExpr) == ".T.")
      *=SEEK(lcRpCrt + lcFrmOrd)
    *ELSE
      *SET FILTER TO CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = "P"
      *GO BOTTOM
      *lcToOrd = POSLN.PO
      *SET FILTER TO
      *LOCATE
    *ENDIF
    SCAN FOR &lcExpr
    *B125052,1 NNA (End)
    
    DECLARE laIssQty[9],laBalQty[8]
    PRIVATE llLoop

    *B125052,1 NNA 12/06/2004 (Begin) Commenting all the lines fixed under bug 606049
    *B125052,1 NNA            because its completely wrong and uncomment the old line of code
    *SCAN REST WHILE &lcExpr
      *IF lcToOrd => POSLN.PO
        *IF POSHDR.STATUS $'OH' .AND. POSLN.TRANCD $'1' .AND. POSLN.TOTQTY > 0          
    *B125052,1 NNA (End)

          SCATTER MEMVAR MEMO
          WAIT WINDOW 'Collecting Canceled quantities for order# '+ M.PO NOWAIT
          *--Check Transaction Code[start]
          lnCurRec = RECNO()
          STORE 0 TO laIssQty,laBalQty
          STORE .F. TO llLoop
          SCAN FOR PO = M.PO AND LINENO = M.LINENO
            IF TRANCD = '6'
              FOR lnI = 1 TO 8
                lcI = STR(lnI,1)
                laIssQty[lnI] = laIssQty[lnI] + EVALUATE('QTY'+lcI)
                laIssQty[9] = laIssQty[9] + EVALUATE('QTY'+lcI)
                laBalQty[lnI] = laBalQty[lnI] + EVALUATE('QTY'+lcI)
              ENDFOR
            ENDIF
            IF TRANCD $'245'
              FOR lnI = 1 TO 8
                lcI = STR(lnI,1)
                laBalQty[lnI] = laBalQty[lnI] - EVALUATE('QTY'+lcI)
              ENDFOR
            ENDIF
          ENDSCAN
          FOR lnI = 1 TO 8
            IF laBalQty[lnI] # 0
              llLoop = .T.
              EXIT
            ENDIF
          ENDFOR
          GOTO (lnCurRec)
          IF llLoop
            LOOP
          ENDIF
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            m.Qty&lcI. = MAX(EVALUATE('m.Qty'+lcI) - laIssQty[lnI],0)
          ENDFOR
          m.TotQty = MAX(m.TotQty - laIssQty[9],0)
          IF m.TotQty = 0
            LOOP
          ENDIF 
          *--Check Transaction Code[end]  
          INSERT INTO (lcTmpFil) FROM MEMVAR
    
      *B125052,1 NNA 12/06/2004 (Begin) Commenting all the lines fixed under bug 606049
      *B125052,1 NNA            because its completely wrong and uncomment the old line of code
      *ENDIF
      *ELSE
        *EXIT
      *ENDIF
      *B125052,1 NNA (End)
      
    ENDSCAN
    SELECT (lcActAlias)
  *E301865,1 AMH [End]
  
  *khalid
  lcExpr  = lcOldExpVl
  *khalid

  CASE lcRpCanTyp = 'C'

    *khalid
    lcOldExpVl = lcExpr
    *khalid

    *RANIA
    *B125052,1 NNA 12/06/2004 (Begin) Commenting all the lines fixed under bug 606049
    *B125052,1 NNA            because its completely wrong and uncomment the old line of code
    *IF !(UPPER(lcExpr) == ".T.")
      *lcExpr = ALLTRIM(STRTRAN(lcExpr,"CUTTKTL.CUTTKT","CUTTKTL.CUTTKT+CUTTKTL.STYLE+CUTTKTL.DYELOT+CUTTKTL.TRANCD"))
      *lnPosOrd = ATC('))),',lcexpr)
      *lcFrmOrd = SUBSTR(lcexpr,lnPosOrd+5,6)
      *lcToOrd  = SUBSTR(lcexpr,ATC(lcFrmOrd,lcexpr)+9,6)
      *lcExpr = "CUTTKT+STYLE+DYELOT+TRANCD = '' "
      lcExpr = lcExpr + " .AND. CUTTKTH.STATUS $'OH' .AND. TRANCD $'1' AND TOTQTY >0"
    *ENDIF
    *IF !(UPPER(lcExpr) == ".T.")
      *=SEEK(lcFrmOrd)
    *ELSE
      *GO BOTTOM
      *lcToOrd = CUTTKTL.CUTTKT
      *LOCATE
    *ENDIF
    SCAN FOR &lcExpr
    *SCAN REST WHILE &lcExpr
      *IF lcToOrd => CUTTKTL.CUTTKT
        *IF CUTTKTH.STATUS $'OH' .AND. TRANCD $'1' .AND. TOTQTY > 0
    *B125052,1 NNA (End)
        
          SCATTER MEMVAR MEMO
          WAIT WINDOW 'Collecting Canceled quantities for CT# '+ M.CUTTKT NOWAIT
          *--Check Transaction Code[start]
          lnCurRec = RECNO()
          *SCAN FOR CUTTKT = M.CUTTKT AND LINENO = M.LINENO
          SCAN FOR  cuttkt+style+dyelot+trancd = M.CUTTKT AND LINENO = M.LINENO
            IF TRANCD $'234'
              M.QTY1 = M.QTY1 - QTY1
              M.QTY2 = M.QTY2 - QTY2
              M.QTY3 = M.QTY3 - QTY3
              M.QTY4 = M.QTY4 - QTY4
              M.QTY5 = M.QTY5 - QTY5
              M.QTY6 = M.QTY6 - QTY6
              M.QTY7 = M.QTY7 - QTY7
              M.QTY8 = M.QTY8 - QTY8
              M.TOTQTY = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
            ENDIF
          ENDSCAN
          GOTO (lnCurRec)
          *--Check Transaction Dode[end]  
          *--HDM B602423,1[START]
          IF M.TOTQTY > 0
            INSERT INTO (lcTmpFil) FROM MEMVAR
          ENDIF
          *--HDM B602423,1[End]
          SELECT (lcActAlias)
          lnPrevTbl = SELECT(0)

     *B125052,1 NNA 12/06/2004 (Begin) Commenting all the lines fixed under bug 606049
     *B125052,1 NNA            because its completely wrong .
        *ENDIF
      *ELSE
        *EXIT
      *ENDIF
      *B125052,1 NNA (End)
      
    ENDSCAN
    *khalid
    lcExpr = lcOldExpVl
    *khalid

ENDCASE

SELECT (lcTmpFil)
IF EOF()
  WAIT WINDOW 'No Records Were Selected'
ENDIF
*-------------------------------------------------------------------------
*!**************************************************************************
*! Name      : lfOpenFile
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Function To Open the needed Files
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
*!Modi:
*!     E301077,6 HDM 12/20/1998  Open Needed Files Using gfOpenFile
*!***************************************************************************

FUNCTION lfOpenFile

*- E301077,6 HDM 12/20/1998  Open Needed Files Using gfOpenFile[start]
*--Purchse order files
*B602573,1 HDM Also open if Order Shaving[start]
*IF lcRpCanTyp $ 'PO'

*E301865,1 AMH Add Inter-location PO [Start]
*IF lcRpCanTyp $ 'POS'
IF lcRpCanTyp $ 'POSN'
*E301865,1 AMH [End]

*B602573,1 HDM Also open if Order Shaving[end]
  IF !USED('POSHDR') .AND. 'PO' $ gcCmpModules
    = gfOpenFile(gcDataDir + 'POSHDR' ,'POSHDR','SH')
  ENDIF

  IF !USED('POSLN') .AND. 'PO' $ gcCmpModules
    = gfOpenFile(gcDataDir + 'POSLN' ,'POSLN','SH')
  ENDIF

ENDIF
*--Purchse order files

*--Cutting Ticket files
IF lcRpCanTyp $ 'COS'
  *--HDM B602512,1 Open cutting Ticket file with the needed Order[Start]
  IF !USED('CUTTKTH') .AND. 'MF' $ gcCmpModules
    = gfOpenFile(gcDataDir + 'CUTTKTH' ,'CUTTKTH','SH')
  ENDIF

  IF !USED('CUTTKTL') .AND. 'MF' $ gcCmpModules
    = gfOpenFile(gcDataDir + 'CUTTKTL' ,'CUTTKTL','SH')
  ENDIF
ENDIF
*--Cutting Ticket files
*--HDM B602512,1 Open cutting Ticket file with the needed Order[End]

*--Sales Order Files
IF !USED('ORDHDR')
  = gfOpenFile(gcDataDir + 'ORDHDR' ,'','SH')
ENDIF
IF !USED('ORDLINE')
  = gfOpenFile(gcDataDir + 'ORDLINE' ,'ORDLINE','SH')
ENDIF
*--Sales Order Files

*--Style Files
IF !USED('STYLE')
  = gfOpenFile(gcDataDir + 'STYLE' ,'','SH')
ENDIF

IF !USED('STYDYE')
  = gfOpenFile(gcDataDir + 'STYDYE' ,'','SH')
ENDIF
*--Style Files


IF !USED('ORDCANLN')
  *B803403,1 ABD Open File wiht order . [Begin]
  *= gfOpenFile(gcDataDir + 'ORDCANLN' ,'','SH')
  = gfOpenFile(gcDataDir + 'ORDCANLN' ,'ORDCANLN','SH')
  *B803403,1 ABD [End]
ENDIF



*- E301077,6 HDM 12/20/1998  Open Needed Files Using gfOpenFile[end]

*---------------------------------------------------------------------------
*!**************************************************************************
*! Name      : lfRemTmp
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Function To Delete the tepm file
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfRemTmp

  IF USED(lcTmpFil)  && We are going to close the Temp Pick ticket & Erase it.
    SELECT(lcTmpFil)
    USE IN &lcTmpFil
  ENDIF   && End of IF

  ERASE &gcWorkdir.&lcTmpFil..DBF  && Erase the Temp file.
  ERASE &gcWorkdir.&lcTmpFil..CDX  && Erase the Temp index.

*------------------------------------------------------------
*!**************************************************************************
*! Name      : lfvSelect
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Valid Function for pbSelect
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfvSelect
*:B604107,1 AAN Select a Tmp. file[Begin]
SELECT(lcTmpFil)
*:B604107,1 AAN Select a Tmp. file[End]
REPLACE LLSEL WITH !LLSEL
=lfvpbSel()
lnSelRec   = IIF(llSel , lnSelRec + 1 , lnSelRec - 1)
lnUnSelRec = IIF(llSel , lnUnSelRec - 1 , lnUnSelRec + 1)
SHOW WINDOW (lcBrowTtl) REFRESH

*IF No records was selected
IF lnSelRec = 0
  SHOW GET pbSelNon DISABLE
  SHOW GET pbRel DISABLE
  SHOW GET pbSelAll ENABLE
ELSE    && Else
  SHOW GET pbSelNon ENABLE
  SHOW GET pbRel ENABLE
  
  *IF All the records was selected
  IF lnUnSelRec = 0
    SHOW GET pbSelAll DISABLE
  ELSE    && Else
    SHOW GET pbSelAll ENABLE
  ENDIF    && End of IF
ENDIF    && End of IF
*-- End
*!**************************************************************************
*! Name      : lfvSelAll
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Valid Function for pbSelall
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfvSelAll
*:B604107,1 AAN Select a Tmp. file[Begin]
SELECT(lcTmpFil)
*:B604107,1 AAN Select a Tmp. file[End]
IF !EOF()
  REPLACE ALL LLSEL WITH .T.
  lnSelRec   = RECCOUNT() - lnDelRec
  lnUnSelRec = 0
  GO lnBrRecNo
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
  SHOW WINDOW (lcBrowTtl) REFRESH
  SHOW GET pbRel ENABLE
  SHOW GET pbSelNon ENABLE
  SHOW GET pbSelAll DISABLE
ENDIF
*!**************************************************************************
*! Name      : lfvSelNon
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Valid Function for pbSelnon
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
FUNCTION lfvSelNon
*:B604107,1 AAN Select a Tmp. file[Begin]
SELECT(lcTmpFil)
*:B604107,1 AAN Select a Tmp. file[End]
IF !EOF()
  REPLACE ALL LLSEL WITH .F.
  lnSelRec   = 0
  lnUnSelRec = RECCOUNT() - lnDelRec
  GO lnBrRecNo
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'
  SHOW WINDOW (lcBrowTtl) REFRESH
  SHOW GET pbRel DISABLE
  SHOW GET pbSelNon DISABLE
  SHOW GET pbSelAll ENABLE
ENDIF
*-- End
*!**************************************************************************
*! Name      : lfvInvert
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Valid Function for pbInv
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfvInvert
*:B604107,1 AAN Select a Tmp. file[Begin]
SELECT(lcTmpFil)
*:B604107,1 AAN Select a Tmp. file[End]
REPLACE ALL LLSEL WITH !LLSEL
*B802430,1 (Begin) Remove the IF condition as it has no meaning because the above command will
*B802430,1         always set the pointer on EOF()
*IF!EOF()
*B802430,1 (End)
  GO lnBrRecNo
  =lfvpbSel()
  lnUnSelRec = lnSelRec
  lnSelRec   = RECCOUNT() - lnDelRec - lnSelRec
  SHOW WINDOW (lcBrowTtl) REFRESH

  *IF there is no selected records
  IF lnSelRec = 0
    SHOW GET pbRel DISABLE
    SHOW GET pbSelNon DISABLE
    SHOW GET pbSelAll ENABLE
  ELSE    && Else
    SHOW GET pbRel ENABLE
    SHOW GET pbSelNon ENABLE

    *IF All the records was selected
    IF lnUnSelRec = 0
      SHOW GET pbSelAll DISABLE
    ENDIF    && End of IF
  ENDIF    && End of IF
*B802430,1 (Begin)   Remark ENDIF
*ENDIF
*B802430,1 (End)
*--End
*!**************************************************************************
*! Name      : lfvpbSel
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Function for Switching 'Select And Unselect'
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfvpbSel

*IF The record is selected
IF LLSEL
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
ELSE    && Else
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'
ENDIF    && End of IF

RETURN .T.
*----------------------------------------------------------
*!**************************************************************************
*! Name      : lfRelScr
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Function for 
*!                  1- Cancelling the Selected Lines
*!                  2- Filling the ORDCANLN File with selected lines
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfRelScr
PRIVATE lnCounter , lcOrdNum ,lcPrevOrd , lnRecCount , llCancel
*--HDM B602475,1
*--AAN B803671,1 [Begin] Don't open CutPick unless PO or MF or PS installed.
*= gfOpenFile(gcDataDir + 'CutPick' ,'CutPick','SH')
IF llOpenCut
  = gfOpenFile(gcDataDir + 'CutPick' ,'CutPick','SH')
ENDIF
*--AAN B803671,1[End]

*--mhm
DO CASE
  CASE lcRpCanTyp = 'O' .OR. lcRpCanTyp = 'S'
    IF lcRpCanTyp = 'O'
      lcBrowTtl = 'Sales Order'
    ELSE
      lcBrowTtl = "Sales Order Shaving 'CTRL+ENTER To edit shaved Quantity'"
    ENDIF

  CASE lcRpCanTyp = 'P'
    lcBrowTtl = 'Purchase Order'
  
  *E301865,1 AMH Add Inter-location PO [Start]
  CASE lcRpCanTyp = 'N'
    lcBrowTtl = 'Inter-Location Purchase Order'
  *E301865,1 AMH [End]

  CASE lcRpCanTyp = 'C'
    lcBrowTtl = 'Cutting Ticket Order'
    
ENDCASE

*--mhm
DO CASE
  CASE lcRpCanTyp$'OS'
    lcMsg = 'Sales Orders'

  CASE lcRpCanTyp$'P'
    lcMsg = 'Purchase Orders'

  *E301865,1 AMH Add Inter-location PO [Start]
  CASE lcRpCanTyp$'N'
    lcMsg = 'Inter-Location Purchase Orders'
  *E301865,1 AMH [End]
  
  CASE lcRpCanTyp$'C'
    lcMsg = 'Cutting Tickets'

ENDCASE
*----
IF gfModalGen('QRM32057B00006','DIALOG',lcMsg) = 2
  RETURN
ENDIF
*----
=lfAdUnCmSR(.F. , 'pbRel')

STORE '' TO lcPrevOrd
STORE 0 TO lnCanc
SELECT ORDLINE

lnCounter = 0
lcOrdNum  = 0
lcOrdNum = ''

DO CASE             && Save From temp file to the cancellation file
  CASE lcRpCanTyp = 'O'   && Order cancellation
    
    *-->O R D E R  C A N C E L A T I O N<--
        
    SELECT (lcTmpFil)
    *B606049,1 RAE Modify the performance.[start]
    *GOTO TOP
    LOCATE
    *B606049,1 RAE [end]
    
    *--HDM B602493,1 Check SO Allocation Status[start]
    =lfChkAlloc('O')
    *IF !llOkCancel
    *  REPLACE ALL &lcTmpFil..LLSEL WITH .F.
    *  *--B602511,1 HDM 02/11/1999 Mark Session as completed[start]
    *  *--Refresh Browse Window
    *  SHOW WINDOW (lcBrowTtl) REFRESH
    *  *-- Mark Session as Completed
    *  =lfAdUnCmSR(.T. , '')
    *  llContinue = .F.
    *  lcCurrObj = ''
    *  *--B602511,1 HDM 02/11/1999 Mark Session as completed[End]
    *  RETURN
    *ENDIF
    *--HDM B602493,1 Check SO Allocation Status[end]
    SCAN FOR LLSEL = .T.
      *B606049,1 MAN Added Wait Window [start]
      WAIT WINDOW ORDER NOWAIT
      *B606049,1 [end]
      = SEEK(lcRpCrt + &lcTmpFil..ORDER +STR(&lcTmpFil..LINENO,6), 'ORDLINE')
      *-- B602343,1 HDM [start]
      IF lcOrdNum  <> ORDER
        lnCounter = 0
      ENDIF
      lcOrdNum  = ORDER
      IF lnCounter = 0
        lnRecNo   = RECNO()
        *--HDM B602918,1 [Start] Pre-Summision of canceled lines
        
        *B606049,1 RAE Modify the performance. [Start]
        *SUM &lcTmpFil..TOTQTY  TO lnTCan FOR ORDER = lcOrdNum AND LLSEL
        *MAN (1)
        SELECT COUNT(*),SUM(TotQty) ;
          FROM (lcTmpFil)           ;
         WHERE cOrdType+Order=lcRpCrt+lcOrdNum AND LLSEL;
          INTO ARRAY laTmpTot
        lnSelected = laTmpTot[1]
        lnTCan     = laTmpTot[2]
        *B606049,1 RAE [END]
        
        *--HDM B602918,1 [End]
        GOTO (lnRecNo)
        *B803652,1 (Begin) We will update OrdCanLn when:
        *B803652,1         1- Deleting whole Prtially-shiped order(s).
        *B803652,1         2- Deleting line by line under depletion date.
        *-Flag to Update OrdCanLn when removing line (or more but not all order)
        llSndLnToC = ORDHDR.STATUS <> 'B' AND (gdSysDate > ORDHDR.ENTERED + laSetups[1,2])
        *-Flag to Update OrdCanLn when removing the whole all order
        llSndOrToC = (ORDHDR.Open = lnTCan) AND ORDHDR.SHIP > 0
        *-Main flag
        llUpdOrdCa = llSndOrToC OR (llSndLnToC AND ORDHDR.Open <> lnTCan)
        *B803652,1 (End)
      ENDIF
      *--HDM B602918,1 [Start] Check the cancelled quantity < order Quantity
      IF lnTCan >= ORDHDR.OPEN AND ORDHDR.SHIP <= 0
        IF lnCounter = 0
          llCancel = .F.
        ENDIF
      ELSE
        IF lnCounter = 0
          llCancel = .T.
        ENDIF
      ENDIF
      *--HDM B602918,1 [End]
      lnCounter = lnCounter + 1
      
      lnRecNo   = RECNO()
      *B606049,1 RAE [start]
      *COUNT TO lnSelected for ORDER = lcOrdNum AND LLSEL = .T.
      *B606049,1 RAE [end]
      GOTO (lnRecNo)
      *-- B602343,1 HDM [end]
      *-- Always Get the lines count for the current order header
      lcPrevAl=SELECT()
      SELECT ORDLINE
      *B606049,1 RAE Modify the performance. [Start]
      *COUNT TO lnRecCount FOR ORDLINE.ORDER = lcOrdNum
      *B606049,1 RAE [End]
      SELECT (lcPrevAl)
      SELECT (lcTmpFil)
      
      IF &lcTmpFil..nProcNo <= 1  && Step '1'
        SCATTER MEMVAR MEMO
        *--AAN B803671,1[Begin] Don't use CutPick unless PO or MF or PS installed.
        IF llOpenCut
        *--AAN B803671,1[End]
          SELECT CUTPICK
          *--HDM B602493,1 Release SO Allocation[start]
          lcCutPkOrd = ORDER()
          SET ORDER TO TAG CUTORD
          IF SEEK('1' + M.ORDER ,'CutPick') .OR. SEEK('2' + M.ORDER ,'CutPick')
            = lfRelAllOc('O')
          ENDIF
          SELECT CUTPICK
          SET ORDER TO TAG (lcCutPkOrd)
        *--AAN B803671,1[Begin] End the previous if stat.
        ENDIF
        *--AAN B803671,1[End]

          *--HDM B602493,1 Release SO Allocation[start]
        
          *-- Collect Qty1 ==> Qty8 for each line For updating Header file
          lnCanc = lnCanc + M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
          SELECT (lcTmpFil)
          =RLOCK(lcTmpFil)
          REPLACE &lcTmpFil..nProcNo WITH 2
          UNLOCK IN &lcTmpFil
     ENDIF
      IF &lcTmpFil..nProcNo = 2  && Step '2'
        SELECT ORDHDR
        = SEEK(lcRpCrt + &lcTmpFil..ORDER)
        =RLOCK(lcTmpFil)
        *-- E602374,1 HDM 12/20/1998 Update Cancelled amount as well[start]
        *B803403,1 ABD Cheak if this order is bulk order then we will update the 
        *B803403,1 ABD Order header file as sales order program [Begin]
        
        *B604258,1 SSE Commented out. [Begin] 
        *IF Bulk = "Y"
        *  REPLACE OPEN      WITH 0                  ;
        *          OPENAMT   WITH 0                  ;
        *          CANCEL    WITH CANCEL             ;
        *          CANCELAMT WITH CANCELAMT          ;
        *          Book      WITH Ship + CANCEL      ;
        *          BookAmt	  WITH ShipAmt + CancelAmt;
        *  FOR ORDER = M.ORDER
        *ELSE
        *B604258,1 SSE Commented out. [End]

        *B803403,1 ABD [End]
        *B606049,1 MAN (3) [start]
        *  REPLACE OPEN      WITH (OPEN     - lnCanc)           ;
        *  CANCEL    WITH CANCEL    + (M.TotQty) ;
        *  CANCELAMT WITH CANCELAMT + (lnCanc * M.PRICE);
        *  OPENAMT   WITH OPENAMT   - (lnCanc * M.PRICE);
        *  FOR ORDER = M.ORDER 
          REPLACE OPEN      WITH (OPEN     - lnCanc)           ;
                  CANCEL    WITH CANCEL    + (M.TotQty)        ;
                  CANCELAMT WITH CANCELAMT + (lnCanc * M.PRICE);
                  OPENAMT   WITH OPENAMT   - (lnCanc * M.PRICE)
        *B606049,1 [end] 

        *B129483,1 NNA 09/18/2005 (Start) Fill the Cancelled date with system date
        REPLACE CANCELLED WITH gdSysDate                         
        *B129483,1 NNA (End)

        *B803403,1 ABD End for if statement [Begin]

        *B604258,1 SSE Commented out. [Begin] 
        *ENDIF
        *B604258,1 SSE Commented out. [End] 

        *B803403,1 ABD [End]
        *-- E602374,1 HDM 12/20/1998 Update Cancelled amount as well[end]
        UNLOCK
        lnCanc = 0
        lcPrevOrd = lcOrdNum
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        =RLOCK(lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 3
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
      IF &lcTmpFil..nProcNo = 3  && Step '3'
        *B803403,1 ABD Dont Update the ordercanln file upon this condition to work
        *B803403,1 ABD As the sales order program. [Begin]
        *B803652,1 (Begin) Update OrdCanLn under proper condition.
        *IF SEEK(lcRpCrt+&lcTmpFil..ORDER+STR(&lcTmpFil..lineno,6),'Ordline') ;
                .AND. ORDHDR.Ship > 0 .AND. ORDLINE.TOTQTY > 0
        IF SEEK(lcRpCrt+&lcTmpFil..ORDER+STR(&lcTmpFil..lineno,6),'Ordline') ;
                .AND. ORDLINE.TOTQTY > 0 .AND. llUpdOrdCa
        *B803652,1 (End)
          *B606402,1 TMI [START] Remove this seek condition , to allow adding a line to ordcanln added befor
          *IF !SEEK(lcRpCrt+&lcTmpFil..ORDER+STR(&lcTmpFil..lineno,6),'ORDCANLN')
            *B606402,1 TMI [End] 
            *B803403,1 ABD [End]
            SELECT  ORDCANLN
            APPEND  BLANK
            REPLACE CORDTYPE  WITH lcRpCrt                           ;
                    ORDER     WITH M.ORDER                           ;
                    LINENO    WITH M.LINENO                          ;
                    QTY1      WITH M.QTY1                            ;
                    QTY2      WITH M.QTY2                            ;
                    QTY3      WITH M.QTY3                            ;
                    QTY4      WITH M.QTY4                            ;
                    QTY5      WITH M.QTY5                            ;
                    QTY6      WITH M.QTY6                            ;
                    QTY7      WITH M.QTY7                            ;
                    QTY8      WITH M.QTY8                            ;
                    TOTQTY    WITH M.TOTQTY                          ;
                    CANCELLED WITH gdSysDate                         ;
                    CCANCRESON WITH lareason[pureason,2]
            *B803652,1 (Begin) Update OrdCanLn with Style,Account,Store and Dyelot.
            REPLACE STYLE    WITH m.Style,;
                    Account  WITH m.Account,;
                    Store    WITH m.Store,;
                    Dyelot   WITH m.Dyelot
            *B803652,1 (End)                    
            
            *B604500,1 Update Price field in Order Cancellation file. [Begin]
            REPLACE Price WITH m.Price
            *B604500,1 Update Price field in Order Cancellation file. [End]
            
            *B803403,1 ABD End For If statement. [Begin]
            *B606402,1 TMI [Start] Comment endif
          *ENDIF  
          *B606402,1 TMI [End] 
        ENDIF  
        *B803403,1 ABD [End]
        
        *--Update Style & StyDye Files (WIP & WO) Fields[Start]
        SELECT STYLE
        SET ORDER TO TAG STYLE
        SEEK &lcTmpFil..STYLE
        *--Start Updatin Style File
        REPLACE ORD1   WITH ORD1   - M.QTY1   ;
                ORD2   WITH ORD2   - M.QTY2   ;
                ORD3   WITH ORD3   - M.QTY3   ;
                ORD4   WITH ORD4   - M.QTY4   ;
                ORD5   WITH ORD5   - M.QTY5   ;
                ORD6   WITH ORD6   - M.QTY6   ;
                ORD7   WITH ORD7   - M.QTY7   ;
                ORD8   WITH ORD8   - M.QTY8   ;
                TOTORD WITH TOTORD - M.TOTQTY

        *--HDM B602601,1 Update WIP & WO Quantities in style file[start]
        *REPLACE WIP1   WITH WIP1   - M.QTY1   ;
                WIP2   WITH WIP2   - M.QTY2   ;
                WIP3   WITH WIP3   - M.QTY3   ;
                WIP4   WITH WIP4   - M.QTY4   ;
                WIP5   WITH WIP5   - M.QTY5   ;
                WIP6   WITH WIP6   - M.QTY6   ;
                WIP7   WITH WIP7   - M.QTY7   ;
                WIP8   WITH WIP8   - M.QTY8   ;
                TOTWIP WITH TOTWIP - M.TOTQTY ;
                NWO1   WITH NWO1   - M.QTY1   ;
                NWO2   WITH NWO2   - M.QTY2   ;
                NWO3   WITH NWO3   - M.QTY3   ;
                NWO4   WITH NWO4   - M.QTY4   ;
                NWO5   WITH NWO5   - M.QTY5   ;
                NWO6   WITH NWO6   - M.QTY6   ;
                NWO7   WITH NWO7   - M.QTY7   ;
                NWO8   WITH NWO8   - M.QTY8   ;
                NTOTWO WITH NTOTWO - M.TOTQTY
        *--HDM B602601,1 Update WIP & WO Quantities in style file[End]

        SELECT STYDYE
        SET ORDER TO TAG STYDYE

        *B804386,1 Update the right warehouse. [Begin]
        *SEEK &lcTmpFil..STYLE
        SEEK &lcTmpFil..Style + &lcTmpFil..cWareCode
        *B804386,1 Update the right warehouse. [End]
                
        *--Start Updating StyDye File
        REPLACE ORD1   WITH ORD1   - M.QTY1   ;
                ORD2   WITH ORD2   - M.QTY2   ;
                ORD3   WITH ORD3   - M.QTY3   ;
                ORD4   WITH ORD4   - M.QTY4   ;
                ORD5   WITH ORD5   - M.QTY5   ;
                ORD6   WITH ORD6   - M.QTY6   ;
                ORD7   WITH ORD7   - M.QTY7   ;
                ORD8   WITH ORD8   - M.QTY8   ;
                TOTORD WITH TOTORD - M.TOTQTY

        *--HDM B602601,1 Update WIP & WO Quantities in stydye file[start]
        *B603194,1 HDM [Start] We shouldn't update WIP in SO Cancellation
        *REPLACE WIP1   WITH WIP1   - M.QTY1   ;
                WIP2   WITH WIP2   - M.QTY2   ;
                WIP3   WITH WIP3   - M.QTY3   ;
                WIP4   WITH WIP4   - M.QTY4   ;
                WIP5   WITH WIP5   - M.QTY5   ;
                WIP6   WITH WIP6   - M.QTY6   ;
                WIP7   WITH WIP7   - M.QTY7   ;
                WIP8   WITH WIP8   - M.QTY8   ;
                TOTWIP WITH TOTWIP - M.TOTQTY ;
                NWO1   WITH NWO1   - M.QTY1   ;
                NWO2   WITH NWO2   - M.QTY2   ;
                NWO3   WITH NWO3   - M.QTY3   ;
                NWO4   WITH NWO4   - M.QTY4   ;
                NWO5   WITH NWO5   - M.QTY5   ;
                NWO6   WITH NWO6   - M.QTY6   ;
                NWO7   WITH NWO7   - M.QTY7   ;
                NWO8   WITH NWO8   - M.QTY8   ;
                NTOTWO WITH NTOTWO - M.TOTQTY
        *B603194,1 HDM [End]
        
        *--HDM B602601,1 Update WIP & WO Quantities in stydye file[End]

        *--Update Style & StyDye Files (WIP & WO) Fields[End]
        = gfAdd_Info()
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        =RLOCK(lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 4
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
      
      *-- Update the CANCELLED quantities

      IF &lcTmpFil..nProcNo = 4  && Step '4'
        *--HDM B602918,1 [Start] if Cancelled = Open Leave line quantities as it is
        *--HDM B602918,1      else Subtract cancelled quantities from it
        
        IF llCancel
          SELECT ORDLINE
          = SEEK(lcRpCrt + &lcTmpFil..ORDER +STR(&lcTmpFil..LINENO,6))
          *B606049,1 MAN [start]
          *REPLACE QTY1      WITH QTY1 - M.QTY1 ;
                  QTY2      WITH QTY2 - M.QTY2 ;
                  QTY3      WITH QTY3 - M.QTY3 ;
                  QTY4      WITH QTY4 - M.QTY4 ;
                  QTY5      WITH QTY5 - M.QTY5 ;
                  QTY6      WITH QTY6 - M.QTY6 ;
                  QTY7      WITH QTY7 - M.QTY7 ;
                  QTY8      WITH QTY8 - M.QTY8 ;
                  TOTQTY    WITH TOTQTY - M.TOTQTY ;
          FOR ORDER = M.ORDER .AND. LINENO = M.LINENO
          REPLACE QTY1      WITH QTY1 - M.QTY1 ;
                  QTY2      WITH QTY2 - M.QTY2 ;
                  QTY3      WITH QTY3 - M.QTY3 ;
                  QTY4      WITH QTY4 - M.QTY4 ;
                  QTY5      WITH QTY5 - M.QTY5 ;
                  QTY6      WITH QTY6 - M.QTY6 ;
                  QTY7      WITH QTY7 - M.QTY7 ;
                  QTY8      WITH QTY8 - M.QTY8 ;
                  TOTQTY    WITH TOTQTY - M.TOTQTY
          *B606049,1 MAN [end]
        ENDIF
        *--HDM B602918,1 [End]
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        =RLOCK(lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 5
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
      *-- B602343,1 HDM [start]
      
      IF &lcTmpFil..nProcNo = 5  && Step '5'
        *SELECT ORDLINE
        *LOCATE FOR ORDER = lcOrdNum .AND. TOTQTY > 0
        *IF FOUND()
        SELECT ORDHDR
        = SEEK(lcRpCrt + &lcTmpFil..ORDER)
        *-- HDM B802665,1 [Start] We should take in consideration the shipped Qty
        *--                       as it affects the open Qty
        *IF BOOK - CANCEL = 0
        IF BOOK - (CANCEL + Ship) = 0
        *-- HDM B802665,1 [End]
          REPLACE STATUS WITH IIF(SHIP > 0 , 'C' , 'X')
          *B604592,1 AME [Start] save cncel reason in ordhdr file.
          IF SHIP <= 0 
            REPLACE cCancReson WITH  lareason[pureason,2]
          ENDIF
          *B604592,1 AME [End]
         *C200149,1  ABD 02/20/2001 Add new triger. [Begin]
         IF STATUS = 'X' .AND. ASCAN(laEvntTrig , PADR('DLEALOSO',10)) <> 0
             llMasOrder = .T.
             =gfDoTriger('SOMASCA',PADR('DLEALOSO',10))
         ENDIF
         *C200149,1  ABD [End]
          
        ENDIF
        *ENDIF
        *-- B602343,1 HDM [end]
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        =RLOCK(lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 6
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
    ENDSCAN
    SELECT(lcTmpFil)
    DELETE ALL FOR LLSEL

  CASE lcRpCanTyp = 'S'  && Order Shaving
    *-->O R D E R  S H A V I N G<--
    SELECT (lcTmpFil)
    *B606049,1 RAE Modify the performance.[start]
    *GOTO TOP
    LOCATE
    *B606049,1 RAE [end]

    *--HDM B602493,1 Check SO Allocation Status[Start]
    =lfChkAlloc('S')
    *B602573,1 HDM no need for this check [start]
    *IF !llOkCancel
    *  REPLACE ALL &lcTmpFil..LLSEL WITH .F.
    *  *--B602511,1 HDM 02/11/1999 Mark Session as completed[start]*

    *  *--Refresh Browse Window
    *  SHOW WINDOW (lcBrowTtl) REFRESH
    *  *-- Mark Session as Completed
    *  =lfAdUnCmSR(.T. , '')
    *  llContinue = .F.
    *  lcCurrObj = ''

    *  *--B602511,1 HDM 02/11/1999 Mark Session as completed[End]
    *  RETURN
    *ENDIF
    *B602573,1 HDM no need for this check [end]
    *--HDM B602493,1 Check SO Allocation Status[End]
    SCAN FOR LLSEL = .T.
      =SEEK(lcRpCrt + &lcTmpFil..ORDER , 'ORDHDR')
      *-- B602343,1 HDM [start]
      IF lcOrdNum  <> ORDER
        lnCounter = 0
      ENDIF
      lcOrdNum  = ORDER
      lnCounter = lnCounter + 1
      lnRecNo   = RECNO()
      COUNT TO lnSelected for ORDER = lcOrdNum AND LLSEL = .T.
      GOTO (lnRecNo)
      *-- B602343,1 HDM [END]
      *-- Always Get the lines count for the current order header
      lcPrevAl=SELECT()
      SELECT ORDLINE
      *B606049,1 RAE Modify the performance. [Start]
      *COUNT TO lnRecCount FOR ORDLINE.ORDER = lcOrdNum
      *B606049,1 RAE [End]
      SELECT (lcPrevAl)
      lnCounter = lnCounter + 1
      SELECT (lcTmpFil)
      SCATTER MEMVAR MEMO
      
        *--HDM B602493,1 Release SO Allocation Status[Start]
      *--AAN B803671,1[Begin] Don't use CutPick unless PO or MF or PS installed.
      IF llOpenCut
      *--AAN B803671,1[End]
        SELECT CUTPICK
        lcCutPkOrd = ORDER()
        SET ORDER TO TAG CUTORD
        IF SEEK('1' + M.ORDER ,'CutPick') .OR. SEEK('2' + M.ORDER ,'CutPick')
          = lfRelAllOc('O')
        ENDIF
        SELECT CUTPICK
        SET ORDER TO TAG (lcCutPkOrd)
      *--AAN B803671,1[Begin] End the previous if stat.
      ENDIF
      *--AAN B803671,1[End]
        *--HDM B602493,1 Release SO Allocation Status[End]
      
     IF &lcTmpFil..nProcNo <= 1  && Step '1'

       *-- Collect Qty1 ==> Qty8 for each line For updating Header file
       lnCanc = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
       lnPrevSel=SELECT(0)
       SELECT (lcTmpFil)
       REPLACE &lcTmpFil..nProcNo WITH 2
       =RLOCK(lcTmpFil)
       UNLOCK IN &lcTmpFil
       SELECT (lnPrevSel)
     ENDIF
     IF &lcTmpFil..nProcNo = 2  && Step '2'
       *IF lnCounter = lnRecCount
         SELECT ORDHDR
         REPLACE OPEN      WITH (OPEN   - lnCanc) ;
                 CANCEL    WITH (CANCEL + lnCanc) ;
                 CANCELAMT WITH (CANCELAMT + (lnCanc * M.PRICE));
                 OPENAMT   WITH (OPENAMT   - (lnCanc * M.PRICE));
                 CANCELLED WITH gdSysDate                         ;
                 CCANCRESON WITH lareason[pureason,2]             ;
         FOR ORDER = M.ORDER
         lnCanc = 0
         lcPrevOrd = lcOrdNum
       *ENDIF
       lnPrevSel=SELECT(0)
       SELECT (lcTmpFil)
       =RLOCK(lcTmpFil)
       REPLACE &lcTmpFil..nProcNo WITH 3
       UNLOCK IN &lcTmpFil
       SELECT (lnPrevSel)
     ENDIF
     IF &lcTmpFil..nProcNo = 3  && Step '3'
       SELECT  ORDCANLN
       APPEND  BLANK
       REPLACE CORDTYPE   WITH lcRpCrt                                   ;
               ORDER      WITH M.ORDER                                   ;
               LINENO     WITH M.LINENO                                  ;
               QTY1       WITH M.QTY1                  ;
               QTY2       WITH M.QTY2                  ;
               QTY3       WITH M.QTY3                  ;
               QTY4       WITH M.QTY4                  ;
               QTY5       WITH M.QTY5                  ;
               QTY6       WITH M.QTY6                  ;
               QTY7       WITH M.QTY7                  ;
               QTY8       WITH M.QTY8                  ;
               TOTQTY     WITH (QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8) ;
               CANCELLED WITH gdSysDate                         ;
               CCANCRESON WITH lareason[pureason,2]

       *B604500,1 Update Price field in Order Cancellation file. [Begin]
       REPLACE Price WITH m.Price
       *B604500,1 Update Price field in Order Cancellation file. [End]

       = gfAdd_Info()
       lnPrevSel=SELECT(0)
       SELECT (lcTmpFil)
       =RLOCK(lcTmpFil)
       REPLACE &lcTmpFil..nProcNo WITH 4
       UNLOCK IN &lcTmpFil
       SELECT (lnPrevSel)
     ENDIF
     IF &lcTmpFil..nProcNo = 4  && Step '4'
        *-- Update the shaved quantities
        SELECT ORDLINE
        *-- HDM B602677,1 We should depend on m.totqty[Start]
        *REPLACE QTY1      WITH (QTY1 - M.QTY1);
                QTY2      WITH (QTY2 - M.QTY2);
                QTY3      WITH (QTY3 - M.QTY3);
                QTY4      WITH (QTY4 - M.QTY4);
                QTY5      WITH (QTY5 - M.QTY5);
                QTY6      WITH (QTY6 - M.QTY6);
                QTY7      WITH (QTY7 - M.QTY7);
                QTY8      WITH (QTY8 - M.QTY8);
                TOTQTY    WITH TOTQTY - lnCanc ;
        FOR ORDER = M.ORDER .AND. LINENO = M.LINENO
        REPLACE QTY1      WITH (QTY1 - M.QTY1);
                QTY2      WITH (QTY2 - M.QTY2);
                QTY3      WITH (QTY3 - M.QTY3);
                QTY4      WITH (QTY4 - M.QTY4);
                QTY5      WITH (QTY5 - M.QTY5);
                QTY6      WITH (QTY6 - M.QTY6);
                QTY7      WITH (QTY7 - M.QTY7);
                QTY8      WITH (QTY8 - M.QTY8);
                TOTQTY    WITH (TOTQTY - M.TOTQTY) ;
        FOR ORDER = M.ORDER .AND. LINENO = M.LINENO
        *-- HDM B602677,1 We should depend on m.totqty[End]
        *--Update Style & StyDye Files (ORD) Fields[Start]
        SELECT STYLE
        SET ORDER TO TAG STYLE
        SEEK &lcTmpFil..STYLE

        *--Start Updatin Style File
        
        REPLACE ORD1   WITH (ORD1  - M.QTY1);
                ORD2   WITH (ORD2  - M.QTY2);
                ORD3   WITH (ORD3  - M.QTY3);
                ORD4   WITH (ORD4  - M.QTY4);
                ORD5   WITH (ORD5  - M.QTY5);
                ORD6   WITH (ORD6  - M.QTY6);
                ORD7   WITH (ORD7  - M.QTY7);
                ORD8   WITH (ORD8  - M.QTY8);
                TOTORD WITH TOTORD - (ORD1 + ORD2 + ORD3 + ORD4 + ORD5 + ORD6 + ORD7 + ORD8)
        *--HDM B602601,1 Update WIP & WO Quantities in style file[start]
        
        *REPLACE WIP1   WITH WIP1   - M.QTY1   ;
                WIP2   WITH WIP2   - M.QTY2   ;
                WIP3   WITH WIP3   - M.QTY3   ;
                WIP4   WITH WIP4   - M.QTY4   ;
                WIP5   WITH WIP5   - M.QTY5   ;
                WIP6   WITH WIP6   - M.QTY6   ;
                WIP7   WITH WIP7   - M.QTY7   ;
                WIP8   WITH WIP8   - M.QTY8   ;
                TOTWIP WITH TOTWIP - M.TOTQTY ;
                NWO1   WITH NWO1   - M.QTY1   ;
                NWO2   WITH NWO2   - M.QTY2   ;
                NWO3   WITH NWO3   - M.QTY3   ;
                NWO4   WITH NWO4   - M.QTY4   ;
                NWO5   WITH NWO5   - M.QTY5   ;
                NWO6   WITH NWO6   - M.QTY6   ;
                NWO7   WITH NWO7   - M.QTY7   ;
                NWO8   WITH NWO8   - M.QTY8   ;
                NTOTWO WITH NTOTWO - M.TOTQTY
        
        *--HDM B602601,1 Update WIP & WO Quantities in style file[End]

        SELECT STYDYE
        SET ORDER TO TAG STYDYE

        *B804386,1 Update the right warehouse. [Begin]
        *SEEK &lcTmpFil..STYLE
        SEEK &lcTmpFil..Style + &lcTmpFil..cWareCode
        *B804386,1 Update the right warehouse. [End]

        *--Start Updating StyDye File
       *--HDM B602604,1 Incorrect Ord Total quantity[start] 
        * REPLACE ORD1   WITH (ORD1  - M.QTY1);
                ORD2   WITH (ORD2  - M.QTY2);
                ORD3   WITH (ORD3  - M.QTY3);
                ORD4   WITH (ORD4  - M.QTY4);
                ORD5   WITH (ORD5  - M.QTY5);
                ORD6   WITH (ORD6  - M.QTY6);
                ORD7   WITH (ORD7  - M.QTY7);
                ORD8   WITH (ORD8  - M.QTY8);
                TOTORD WITH TOTORD - (ORD1 - ORD2 - ORD3 - ORD4 - ORD5 - ORD6 - ORD7 - ORD8)

        REPLACE ORD1   WITH (ORD1  - M.QTY1);
                ORD2   WITH (ORD2  - M.QTY2);
                ORD3   WITH (ORD3  - M.QTY3);
                ORD4   WITH (ORD4  - M.QTY4);
                ORD5   WITH (ORD5  - M.QTY5);
                ORD6   WITH (ORD6  - M.QTY6);
                ORD7   WITH (ORD7  - M.QTY7);
                ORD8   WITH (ORD8  - M.QTY8);
                TOTORD WITH TOTORD - (ORD1 + ORD2 + ORD3 + ORD4 + ORD5 + ORD6 + ORD7 + ORD8)
        *--HDM B602604,1 Incorrect Ord Total quantity[end] 
        
        *--Update Style & StyDye Files (ORD) Fields[End]
        *--HDM B602601,1 Update WIP & WO Quantities in style file[start]
        *B603194,1 HDM [Start] We shouldn't update WIP in SO Cancellation
        *REPLACE WIP1   WITH WIP1   - M.QTY1   ;
                WIP2   WITH WIP2   - M.QTY2   ;
                WIP3   WITH WIP3   - M.QTY3   ;
                WIP4   WITH WIP4   - M.QTY4   ;
                WIP5   WITH WIP5   - M.QTY5   ;
                WIP6   WITH WIP6   - M.QTY6   ;
                WIP7   WITH WIP7   - M.QTY7   ;
                WIP8   WITH WIP8   - M.QTY8   ;
                TOTWIP WITH TOTWIP - M.TOTQTY ;
                NWO1   WITH NWO1   - M.QTY1   ;
                NWO2   WITH NWO2   - M.QTY2   ;
                NWO3   WITH NWO3   - M.QTY3   ;
                NWO4   WITH NWO4   - M.QTY4   ;
                NWO5   WITH NWO5   - M.QTY5   ;
                NWO6   WITH NWO6   - M.QTY6   ;
                NWO7   WITH NWO7   - M.QTY7   ;
                NWO8   WITH NWO8   - M.QTY8   ;
                NTOTWO WITH NTOTWO - M.TOTQTY
        *B603194,1 HDM [End]
        *--HDM B602601,1 Update WIP & WO Quantities in style file[End]

        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        =RLOCK(lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 5
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
     ENDIF
     *-- B602343,1 HDM [start]
     IF &lcTmpFil..nProcNo = 5  && Step '5'
       *SELECT ORDLINE
       *LOCATE FOR ORDER = lcOrdNum .AND. TOTQTY > 0
       *IF !FOUND()
       SELECT ORDHDR
       = SEEK(lcRpCrt + &lcTmpFil..ORDER)
       *-- HDM B802665,1 [Start] Consider the ship QTY
       *--                       as it affects open Qty 
       *IF BOOK - CANCEL = 0
       IF BOOK - (CANCEL + SHIP) = 0
       *-- HDM B802665,1 [End]
         REPLACE STATUS WITH IIF(SHIP > 0 , 'C' , 'X')
         *B604592,1 AME [Start] save cncel reson in ordhdr file.
         IF SHIP <= 0 
           REPLACE cCancReson WITH  lareason[pureason,2]
         ENDIF
         *B604592,1 AME [End]
       ENDIF
       *ENDIF
       lnPrevSel=SELECT(0)
       SELECT (lcTmpFil)
       =RLOCK(lcTmpFil)
       REPLACE &lcTmpFil..nProcNo WITH 6
       UNLOCK IN &lcTmpFil
       SELECT (lnPrevSel)
     ENDIF
     *-- B602343,1 HDM [End]
   ENDSCAN
   SELECT(lcTmpFil)
   DELETE ALL FOR LLSEL
   
  *--> P U S C H A S E  O R D E R <--
  
  *E301865,1 AMH Add Inter-location PO [Start]
  *CASE lcRpCanTyp = 'P'  &&Purchase order
  CASE lcRpCanTyp $ 'PN'  &&Purchase order or Inter-location PO
  *E301865,1 AMH [End]
  
    SELECT (lcTmpFil)
    *B606049,1 RAE Modify the performance.[start]
    *GOTO TOP
    LOCATE
    *B606049,1 RAE [end]

    lcOrdNum = ''
    *--B602531,1 HDM no need for these line any more [start]
    =lfChkAlloc('P')
    **--HDM B602476,1 Check Allocation Status[Start]
    *IF !llOkCancel
    *  *--B602511,1 HDM 02/11/1999 Mark Session as completed[start]
    *  REPLACE ALL &lcTmpFil..LLSEL WITH .F.
    *  llOkCancel = .T.
    *  *--Refresh Browse Window
      SHOW WINDOW (lcBrowTtl) REFRESH
    *  *-- Mark Session as Completed
    *  =lfAdUnCmSR(.T. , '')
    *  llContinue = .F.
    *  lcCurrObj = ''
    *  *--B602511,1 HDM 02/11/1999 Mark Session as completed[End]
    *  RETURN
    *ENDIF
    *--HDM B602476,1 Check Allocation Status[End]
    *--B602531,1 HDM no need for these line any more[end]
    
    SCAN FOR LLSEL = .T.
      =SEEK(lcRpCrt + &lcTmpFil..PO , 'POSHDR')
      *-- B602343,1 HDM [start]
      IF lcOrdNum  <> PO
        lnCounter = 0
      ENDIF

      lcOrdNum  = PO
      
      lnRecNo   = RECNO()
      COUNT TO lnSelected for PO = lcOrdNum AND LLSEL = .T.
      GOTO (lnRecNo)
      *-- B602343,1 HDM [End]
      *-- Always Get the lines count for the current order header
      lcPrevAl=SELECT(0)
      SELECT POSLN
      lnCounter = lnCounter + 1

      *B606049,1 RAE Modify the performance.[start]
      *GOTO TOP
      LOCATE
      *COUNT TO lnRecCount FOR PO = lcOrdNum AND TRANCD = '1' .AND. TOTQTY >0
      *B606049,1 RAE [end]
      
      *SELECT (lcPrevAl)
      SELECT (lcTmpFil)
      IF &lcTmpFil..nProcNo <= 1  && Step '1'
        SCATTER MEMVAR MEMO
  
        *--AAN B803671,1[Begin] Don't use CutPick unless PO or MF or PS installed.
        *IF SEEK('2' + M.PO +M.Style ,'CutPick')
        IF llOpenCut  AND SEEK('2' + M.PO +M.Style ,'CutPick')
        *--AAN B803671,1[End] 
        *--HDM B602476,1 Release the Allocation[Start]
          = lfRelAllOc('P')
        ENDIF
        *--HDM B602476,1 Release the Allocation[End]
  

        *-- Collect Qty1 ==> Qty8 for each line For updating Header file
        lnCanc = lnCanc + M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 2
        =RLOCK(lcTmpFil)
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
      IF &lcTmpFil..nProcNo = 2  && Step '2'
        *IF lnCounter = lnSelected
          SELECT POSHDR
          =RLOCK()
          REPLACE OPEN    WITH (OPEN   - lnCanc);
                  CANCEL  WITH (CANCEL + lnCanc);
                  NICOST1 WITH NICOST1 - (POSLN.NCOST1*lnCanc);
                  NICOST2 WITH NICOST2 - (POSLN.NCOST2*lnCanc);
                  NICOST3 WITH NICOST3 - (POSLN.NCOST3*lnCanc);
                  NICOST4 WITH NICOST4 - (POSLN.NCOST4*lnCanc);
                  NICOST5 WITH NICOST5 - (POSLN.NCOST5*lnCanc);
          FOR PO = M.PO
          UNLOCK
          lnCanc = 0
          lcPrevOrd = lcOrdNum
        *ENDIF
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 3
        =RLOCK(lcTmpFil)
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
      IF &lcTmpFil..nProcNo = 3  && Step '3'
        SELECT  ORDCANLN
        APPEND  BLANK
        REPLACE CORDTYPE  WITH lcRpCrt                           ;
                ORDER     WITH M.PO                              ;
                LINENO    WITH M.LINENO                          ;
                QTY1      WITH M.QTY1                            ;
                QTY2      WITH M.QTY2                            ;
                QTY3      WITH M.QTY3                            ;
                QTY4      WITH M.QTY4                            ;
                QTY5      WITH M.QTY5                            ;
                QTY6      WITH M.QTY6                            ;
                QTY7      WITH M.QTY7                            ;
                QTY8      WITH M.QTY8                            ;
                TOTQTY    WITH M.TOTQTY                          ;
                CANCELLED WITH gdSysDate
         *B604019,1 (Begin) Update OrdCanLn with Style and Dyelot in case of PO.
         REPLACE STYLE    WITH m.Style,;
                 Dyelot   WITH m.Dyelot
        *B604019,1 (End)                    

        *B604500,1 Update Price field in Order Cancellation file. [Begin]
        REPLACE Price WITH m.Price
        *B604500,1 Update Price field in Order Cancellation file. [End]
 
        = gfAdd_Info()
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 4
        =RLOCK(lcTmpFil)
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
      IF &lcTmpFil..nProcNo = 4  && Step '4'
        *-- Update the CANCELLED quantities
        SELECT POSLN
        *B606743,1 WAB (Start) - is not correct to substract the canceled qty from budjetqty
        *REPLACE QTY1      WITH QTY1   - M.QTY1 ;
        *        QTY2      WITH QTY2   - M.QTY2 ;
        *        QTY3      WITH QTY3   - M.QTY3 ;
        *        QTY4      WITH QTY4   - M.QTY4 ;
        *        QTY5      WITH QTY5   - M.QTY5 ;
        *        QTY6      WITH QTY6   - M.QTY6 ;
        *        QTY7      WITH QTY7   - M.QTY7 ;
        *        QTY8      WITH QTY8   - M.QTY8 ;
        *        TOTQTY    WITH TOTQTY - M.TOTQTY ;
        *FOR PO = M.PO .AND. LINENO = M.LINENO .AND. TRANCD = '1'
        *B606743,1 WAB (End)
        *--Update Style & StyDye Files (WIP & WO) Fields[Start]

        *E301865,1 AMH Update style file for PO only [Start]
        IF lcRpCanTyp = 'P'
        *E301865,1 AMH [End]
        
        SELECT STYLE
        SET ORDER TO TAG STYLE
        SEEK &lcTmpFil..STYLE
        *--Start Updatin Style File
        REPLACE WIP1   WITH WIP1   - M.QTY1   ;
                WIP2   WITH WIP2   - M.QTY2   ;
                WIP3   WITH WIP3   - M.QTY3   ;
                WIP4   WITH WIP4   - M.QTY4   ;
                WIP5   WITH WIP5   - M.QTY5   ;
                WIP6   WITH WIP6   - M.QTY6   ;
                WIP7   WITH WIP7   - M.QTY7   ;
                WIP8   WITH WIP8   - M.QTY8   ;
                TOTWIP WITH TOTWIP - M.TOTQTY ;
                NWO1   WITH NWO1   - M.QTY1   ;
                NWO2   WITH NWO2   - M.QTY2   ;
                NWO3   WITH NWO3   - M.QTY3   ;
                NWO4   WITH NWO4   - M.QTY4   ;
                NWO5   WITH NWO5   - M.QTY5   ;
                NWO6   WITH NWO6   - M.QTY6   ;
                NWO7   WITH NWO7   - M.QTY7   ;
                NWO8   WITH NWO8   - M.QTY8   ;
                NTOTWO WITH NTOTWO - M.TOTQTY

        *E301865,1 AMH Update style file for PO only [Start]
        ENDIF
        *E301865,1 AMH [End]
        
        SELECT STYDYE
        SET ORDER TO TAG STYDYE

        *B804386,1 Update the right warehouse. [Begin]
        *SEEK &lcTmpFil..STYLE
        SEEK &lcTmpFil..Style + &lcTmpFil..cWareCode
        *B804386,1 Update the right warehouse. [End]

        *--Start Updating StyDye File
        REPLACE WIP1   WITH WIP1   - M.QTY1   ;
                WIP2   WITH WIP2   - M.QTY2   ;
                WIP3   WITH WIP3   - M.QTY3   ;
                WIP4   WITH WIP4   - M.QTY4   ;
                WIP5   WITH WIP5   - M.QTY5   ;
                WIP6   WITH WIP6   - M.QTY6   ;
                WIP7   WITH WIP7   - M.QTY7   ;
                WIP8   WITH WIP8   - M.QTY8   ;
                TOTWIP WITH TOTWIP - M.TOTQTY ;
                NWO1   WITH NWO1   - M.QTY1   ;
                NWO2   WITH NWO2   - M.QTY2   ;
                NWO3   WITH NWO3   - M.QTY3   ;
                NWO4   WITH NWO4   - M.QTY4   ;
                NWO5   WITH NWO5   - M.QTY5   ;
                NWO6   WITH NWO6   - M.QTY6   ;
                NWO7   WITH NWO7   - M.QTY7   ;
                NWO8   WITH NWO8   - M.QTY8   ;
                NTOTWO WITH NTOTWO - M.TOTQTY

        *E301865,1 AMH Update the source warehouse in stydye file for Inter-location PO only [Start]
        IF lcRpCanTyp = 'N'
        SEEK EVALUATE(lcTmpFil+'.STYLE') + EVALUATE(lcTmpFil+'.VENDOR')

        *--Start Updating StyDye File
        REPLACE WIP1   WITH WIP1   + M.QTY1   ;
                WIP2   WITH WIP2   + M.QTY2   ;
                WIP3   WITH WIP3   + M.QTY3   ;
                WIP4   WITH WIP4   + M.QTY4   ;
                WIP5   WITH WIP5   + M.QTY5   ;
                WIP6   WITH WIP6   + M.QTY6   ;
                WIP7   WITH WIP7   + M.QTY7   ;
                WIP8   WITH WIP8   + M.QTY8   ;
                TOTWIP WITH TOTWIP + M.TOTQTY ;
                NWO1   WITH NWO1   + M.QTY1   ;
                NWO2   WITH NWO2   + M.QTY2   ;
                NWO3   WITH NWO3   + M.QTY3   ;
                NWO4   WITH NWO4   + M.QTY4   ;
                NWO5   WITH NWO5   + M.QTY5   ;
                NWO6   WITH NWO6   + M.QTY6   ;
                NWO7   WITH NWO7   + M.QTY7   ;
                NWO8   WITH NWO8   + M.QTY8   ;
                NTOTWO WITH NTOTWO + M.TOTQTY
        ENDIF
        *E301865,1 AMH [End]
        
        *--Update Style & StyDye Files (WIP & WO) Fields[End]

        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        =RLOCK(lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 5
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
      *-- B602343,1 HDM [start]

      IF &lcTmpFil..nProcNo = 5  && Step '5'
        *SELECT POSLN
        *LOCATE FOR PO = lcOrdNum .AND. TOTQTY > 0 AND TRANCD = '1'
        IF SEEK(lcRpCrt + &lcTmpFil..PO , 'POSHDR')
          SELECT POSHDR
          IF POSHDR.OPEN <= 0
            =RLOCK()
            REPLACE STATUS WITH IIF(RECEIVE > 0 , 'C' , 'X');
            FOR PO = M.PO
            UNLOCK
            *B606743,1 WAB (Start) - call function to ad cancel line(s)
            *B128212,2 MMR 09/14/2005 Fix bug of not cancelling some PO numbers.[Start]
            lnPrevSel2=SELECT(0)
            SELECT (lcTmpFil)
            lnRec=RECNO()
            SELECT (lnPrevSel2)
            *B128212,2 MMR.[End]
            =lfAdCnLine()
            *B128212,2 MMR 09/14/2005 Fix bug of not cancelling some PO numbers.[Start]
            lnPrevSel3=SELECT(0)
            SELECT (lcTmpFil)
            IF lnRec<>RECNO()
              GOTO lnREC
            ENDIF
            SELECT (lnPrevSel3)
            *B128212,2 MMR.[End]
            *B606743,1 WAB (End)

          ENDIF
        ENDIF
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 6
        =RLOCK(lcTmpFil)
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
      *-- B602343,1 HDM [End]
    ENDSCAN
    SELECT(lcTmpFil)
    DELETE ALL FOR LLSEL

  CASE lcRpCanTyp = 'C' && Cut ticket
    SELECT (lcTmpFil)
    *B606049,1 RAE Modify the performance.[start]
    *GOTO TOP
    LOCATE
    *B606049,1 RAE [end]

    lcOrdNum = ''
    *--HDM B602476,1 Check The Allocation Status[Start]
    =lfChkAlloc('C')
    *--B602531,1 HDM no need for these line any more[Start]
    *IF !llOkCancel
    *  *--B602511,1 HDM 02/11/1999 Mark Session as completed[start]
    *  REPLACE ALL &lcTmpFil..LLSEL WITH .F.
    *  llOkCancel = .T.
    *  *--Refresh Browse Window
      SHOW WINDOW (lcBrowTtl) REFRESH
    *  *-- Mark Session as Completed
    *  =lfAdUnCmSR(.T. , '')
    *  llContinue = .F.
    *  lcCurrObj = ''
    *  *--B602511,1 HDM 02/11/1999 Mark Session as completed[End]
    *  RETURN
    *ENDIF
    *--HDM B602476,1 Check The Allocation Status[End]
    *--B602531,1 HDM no need for these line any more[end]

    SCAN FOR LLSEL = .T.
      =SEEK(&lcTmpFil..CUTTKT , 'CUTTKTH')
      *-- B602343,1 HDM [start]
      IF lcOrdNum  <> CUTTKT
        lnCounter = 0
      ENDIF
      
      lcOrdNum  = CUTTKT
      lnCounter = lnCounter + 1
      lnRecNo   = RECNO()
      
      COUNT TO lnSelected for cuttkt = lcOrdNum AND LLSEL = .T.
      GOTO (lnRecNo)
      *-- B602343,1 HDM [end]
      *-- Always Get the lines count for the current order header
      lcPrevAl=SELECT()
      SELECT CUTTKTL
      *B606049,1 RAE Modify the performance. [Start]
      *COUNT TO lnRecCount FOR CUTTKTL.CUTTKT = lcOrdNum AND CUTTKTL.TRANCD = '1' .AND. CUTTKTL.TOTQTY > 0
      *B606049,1 RAE [End]
      
      SELECT (lcTmpFil)
      IF &lcTmpFil..nProcNo <= 1  && Step '1'
        SCATTER MEMVAR MEMO

        *--AAN B803671,1[Begin] Don't use CutPick unless PO or MF or PS installed.
        *IF SEEK('1' + M.CUTTKT +M.Style ,'CutPick')
        IF llOpenCut AND SEEK('1' + M.CUTTKT +M.Style ,'CutPick')
        *--AAN B803671,1[End]
          *--HDM B602476,1 Release the Allocation[Start]
          = lfRelAllOc('C')
        ENDIF
        *--HDM B602476,1 Release the Allocation[End]
        *-- Collect Qty1 ==> Qty8 for each line For updating Header file
        lnCanc = lnCanc + M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 2
        =RLOCK(lcTmpFil)
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF

      IF &lcTmpFil..nProcNo = 2  && Step '2'
        IF lnCounter = lnSelected
          SELECT CUTTKTH
          *--Update Both Open And Canceled Qty in CT Header File
          REPLACE PCS_OPN WITH (PCS_OPN - lnCanc)          ;
                  PCS_CAN WITH (PCS_CAN + lnCanc)          ;
          FOR CUTTKT = M.CUTTKT
          lnCanc = 0
          lcPrevOrd = lcOrdNum
        ENDIF
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 3
        =RLOCK(lcTmpFil)
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
      IF &lcTmpFil..nProcNo = 3  && Step '3'
        SELECT  ORDCANLN
        APPEND  BLANK
        REPLACE CORDTYPE   WITH 'T'                               ;
                ORDER      WITH M.CUTTKT                          ;
                LINENO     WITH M.LINENO                          ;
                QTY1       WITH M.QTY1                            ;
                QTY2       WITH M.QTY2                            ;
                QTY3       WITH M.QTY3                            ;
                QTY4       WITH M.QTY4                            ;
                QTY5       WITH M.QTY5                            ;
                QTY6       WITH M.QTY6                            ;
                QTY7       WITH M.QTY7                            ;
                QTY8       WITH M.QTY8                            ;
                TOTQTY     WITH M.TOTQTY                          ;
                CANCELLED  WITH gdSysDate
        *B604019,1 (Begin) Update OrdCanLn with Style and Dyelot in case of CT.
        REPLACE STYLE    WITH m.Style,;
                Dyelot   WITH m.Dyelot
        *B604019,1 (End)                    

        *B604500,1 Update Price field in Order Cancellation file. [Begin]
        *--- SSH Commentes out sinc that no PRICE field in CUTTKTL
        *REPLACE Price WITH m.Price
        *--- SSH
        *B604500,1 Update Price field in Order Cancellation file. [End]

        = gfAdd_Info()
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 4
        =RLOCK(lcTmpFil)
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
      
      IF &lcTmpFil..nProcNo = 4  && Step '4'
        *-- Update the CANCELLED quantities
 
        *B606743,1 WAB (Start) - is not correct to substract the canceled qty from budjetqty
        *SELECT CUTTKTL
        *REPLACE QTY1      WITH QTY1   - M.QTY1 ;
        *        QTY2      WITH QTY2   - M.QTY2 ;
        *        QTY3      WITH QTY3   - M.QTY3 ;
        *        QTY4      WITH QTY4   - M.QTY4 ;
        *        QTY5      WITH QTY5   - M.QTY5 ;
        *        QTY6      WITH QTY6   - M.QTY6 ;
        *        QTY7      WITH QTY7   - M.QTY7 ;
        *        QTY8      WITH QTY8   - M.QTY8 ;
        *        TOTQTY    WITH TOTQTY - M.TOTQTY ;
        *FOR CUTTKT = M.CUTTKT .AND. LINENO = M.LINENO AND TRANCD = '1'
        *  
        **--HDM B602527,1 Also Update the estimated cost Elements using style file costs[Start]
        *REPLACE NCOST1 WITH CUTTKTL.TOTQTY * STYLE.NMCOST1 ;
        *        NCOST2 WITH CUTTKTL.TOTQTY * STYLE.NMCOST2 ;
        *        NCOST3 WITH CUTTKTL.TOTQTY * STYLE.NMCOST3 ;
        *        NCOST4 WITH CUTTKTL.TOTQTY * STYLE.NMCOST4 ;
        *        NCOST5 WITH CUTTKTL.TOTQTY * STYLE.NMCOST5 ;
        *FOR CUTTKT = M.CUTTKT .AND. LINENO = M.LINENO AND TRANCD = '1'
        **--HDM B602527,1 Also Update the estimated cost Elements using style file costs[End]
        *B606743,1 WAB (End) 

        
        *--Update Style & StyDye Files (WIP & WO) Fields[Start]
        
        SELECT STYLE
        SET ORDER TO TAG STYLE
        SEEK &lcTmpFil..STYLE
        *--Start Updatin Style File
        REPLACE WIP1   WITH WIP1   - M.QTY1   ;
                WIP2   WITH WIP2   - M.QTY2   ;
                WIP3   WITH WIP3   - M.QTY3   ;
                WIP4   WITH WIP4   - M.QTY4   ;
                WIP5   WITH WIP5   - M.QTY5   ;
                WIP6   WITH WIP6   - M.QTY6   ;
                WIP7   WITH WIP7   - M.QTY7   ;
                WIP8   WITH WIP8   - M.QTY8   ;
                TOTWIP WITH TOTWIP - M.TOTQTY ;
                NWO1   WITH NWO1   - M.QTY1   ;
                NWO2   WITH NWO2   - M.QTY2   ;
                NWO3   WITH NWO3   - M.QTY3   ;
                NWO4   WITH NWO4   - M.QTY4   ;
                NWO5   WITH NWO5   - M.QTY5   ;
                NWO6   WITH NWO6   - M.QTY6   ;
                NWO7   WITH NWO7   - M.QTY7   ;
                NWO8   WITH NWO8   - M.QTY8   ;
                NTOTWO WITH NTOTWO - M.TOTQTY
        
        SELECT STYDYE
        SET ORDER TO TAG STYDYE

        *B804386,1 Update the right warehouse. [Begin]
        *SEEK &lcTmpFil..STYLE
        SEEK &lcTmpFil..Style + &lcTmpFil..cWareCode
        *B804386,1 Update the right warehouse. [End]

        *--Start Updating StyDye File
        REPLACE WIP1   WITH WIP1   - M.QTY1   ;
                WIP2   WITH WIP2   - M.QTY2   ;
                WIP3   WITH WIP3   - M.QTY3   ;
                WIP4   WITH WIP4   - M.QTY4   ;
                WIP5   WITH WIP5   - M.QTY5   ;
                WIP6   WITH WIP6   - M.QTY6   ;
                WIP7   WITH WIP7   - M.QTY7   ;
                WIP8   WITH WIP8   - M.QTY8   ;
                TOTWIP WITH TOTWIP - M.TOTQTY ;
                NWO1   WITH NWO1   - M.QTY1   ;
                NWO2   WITH NWO2   - M.QTY2   ;
                NWO3   WITH NWO3   - M.QTY3   ;
                NWO4   WITH NWO4   - M.QTY4   ;
                NWO5   WITH NWO5   - M.QTY5   ;
                NWO6   WITH NWO6   - M.QTY6   ;
                NWO7   WITH NWO7   - M.QTY7   ;
                NWO8   WITH NWO8   - M.QTY8   ;
                NTOTWO WITH NTOTWO - M.TOTQTY

        *--Update Style & StyDye Files (WIP & WO) Fields[End]
        
        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 5
        =RLOCK(lcTmpFil)
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
      *-- B602343,1 HDM [start]
      IF &lcTmpFil..nProcNo = 5  && Step '5'
        *--HDM B602423,1[START]
        SELECT CUTTKTL
        lnTOTQTY = 0
        SCAN FOR CUTTKT = M.CUTTKT
          IF TRANCD $'234'
            lnQTY1 = M.QTY1 - QTY1
            lnQTY2 = M.QTY2 - QTY2
            lnQTY3 = M.QTY3 - QTY3
            lnQTY4 = M.QTY4 - QTY4
            lnQTY5 = M.QTY5 - QTY5
            lnQTY6 = M.QTY6 - QTY6
            lnQTY7 = M.QTY7 - QTY7
            lnQTY8 = M.QTY8 - QTY8
            lnTOTQTY = lnQTY1 + lnQTY2 + lnQTY3 + lnQTY4 + lnQTY5 + lnQTY6 + lnQTY7 + lnQTY8
          ENDIF
        ENDSCAN
        *LOCATE FOR CUTTKT = lcOrdNum .AND. TOTQTY > 0
        *--HDM B602423,1[end]
        *IF lnTOTQTY <= 0
        
        =SEEK(lcOrdNum,'CUTTKTH')
        
        IF CUTTKTH.PCS_OPN <= 0
          SELECT CUTTKTH
          REPLACE STATUS WITH IIF(PCS_REC > 0 , 'C' , 'X');
          FOR CUTTKT = lcOrdNum
  
          *B606743,1 WAB (Start) - call function to ad cancel line(s)
          =lfAdCnLine()
          *B606743,1 WAB (End)

        ENDIF
        *--HDM B602527,1 Also Update the estimated cost Elements using style file costs[Start]
        SELECT CUTTKTH
        FOR lnLoop = 1 TO 5
          lcCostPos = ALLTRIM(STR(lnLoop))
          SUM NCOST&lcCostPos TO lnCost&lcCostPos FOR CUTTKT = M.CUTTKT
        ENDFOR
        REPLACE NEST_COST1 WITH (PCS_bud - PCS_Can) * lnCost1 ;
                NEST_COST2 WITH (PCS_bud - PCS_Can) * lnCost2 ;
                NEST_COST3 WITH (PCS_bud - PCS_Can) * lnCost3 ;
                NEST_COST4 WITH (PCS_bud - PCS_Can) * lnCost4 ;
                NEST_COST5 WITH (PCS_bud - PCS_Can) * lnCost5 ;
        FOR CUTTKT = M.CUTTKT
        *--HDM B602527,1 Also Update the estimated cost Elements using style file costs[End]

        lnPrevSel=SELECT(0)
        SELECT (lcTmpFil)
        REPLACE &lcTmpFil..nProcNo WITH 6
        =RLOCK(lcTmpFil)
        UNLOCK IN &lcTmpFil
        SELECT (lnPrevSel)
      ENDIF
    *-- B602343,1 HDM [End]
    
    ENDSCAN
    
    SELECT(lcTmpFil)
    DELETE ALL FOR LLSEL

  ENDCASE
  SELECT(lcTmpFil)
  *B606049,1 RAE Modify the performance.[start]
  *GOTO TOP
  LOCATE
  *B606049,1 RAE [end]

  SHOW GET pbRel DISABLE
  *--Refresh Browse Window
  SHOW WINDOW (lcBrowTtl) REFRESH
  *B603368,1 AMM REFRESH THE SELECT/UNSELECT BUTTON
  ACTIVATE WINDOW (lcBrowTtl)
  =lfvpbSel()
  *B603368,1 AMM END
  *-- Mark Session as Completed
  =lfAdUnCmSR(.T. , '')
  *-- Restore Flags
  STORE .F. TO llPickMsg , llAlocMsg , llAcordMsg
  STORE .F. TO llOkCancel , llUpdPoCt
  llContinue = .F.
  lcCurrObj = ''

*!**************************************************************************
*! Name      : lfwBrows
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : When Function The Browse
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfwBrows

lnBrRecNo  = RECNO(lcTmpFil)
SHOW WINDOW (lcBrowTtl) REFRESH
    
llEOF = .F.
llBOF = .F.

IF !EOF() .AND. !BOF()
  SKIP 1
  IF EOF()
    llEOF = .T.
    SKIP -2
    IF BOF()
      llBOF = .T.
    ELSE
      SKIP 1
    ENDIF
  ELSE
    SKIP -2
    IF BOF()
      llBOF = .T.
    ELSE
      SKIP 1
    ENDIF
  ENDIF

ELSE
  llEOF = .T.
  llBOF = .T.
ENDIF

laCtrStat[1]  = IIF(llBOF , 'DISABLE' , 'ENABLE')         && First button 
laCtrStat[4]  = laCtrStat[1]                              && Priveus button
laCtrStat[2]  = IIF(llEOF , 'DISABLE' , 'ENABLE')         && Last button
laCtrStat[3] = laCtrStat[2]                               && Next button
SHOW GET pbTop  &laCtrStat[1]
SHOW GET pbPrvs &laCtrStat[4]
SHOW GET pbBtm  &laCtrStat[2]
SHOW GET pbNxt  &laCtrStat[3]
SHOW MENU _mSysMenu

*!**************************************************************************
*! Name      : lfFndUnCSe
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Function To find uncomplete session
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfFndUnCSe

PRIVATE llReturn

llReturn = gfUnCompSession('SMMASCA' , lnSessNo , 'Mass Order Cancelation')

IF llReturn
  lcCurrObj  = ALLTRIM(UNCMSESS.cCurrObj) + '()'
  lcSession  = UNCMSESS.cSession
  lnUnCmRec  = RECNO('UNCMSESS')
  =lfPrepVar()
ENDIF

RETURN llReturn

*!**************************************************************************
*! Name      : lfPrepVar
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Function To prepare variables for uncomplete session
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfPrepVar

PRIVATE lnTotRec , lnCurnRec
SELECT (lcTmpFil)

lnSelRec   = 0
lnUnSelRec = 0
lnDelRec   = RECCOUNT()
lnTotRec   = RECCOUNT()          && Varible to hold the Total count to be done for the thermometer
lnCurnRec  = 0                   && Varible to hold the current count to be done for the thermometer

*SCAN Loop to scan the temp. Order lines file
SCAN
  lnCurnRec = lnCurnRec + 1
  lnDelRec  = lnDellRec - 1
  
  *IF the current record is selected
  IF llSel
    lnSelRec   = lnSelRec + 1
  ELSE
    lnUnSelRec = lnUnSelRec + 1
  ENDIF    && End of IF
  = gfThermo(lnTotRec , lnCurnRec , "Preparing data for incompleted session... ")
ENDSCAN    && End of SCAN Loop
*B606049,1 RAE [start]
*GO TOP
LOCATE
*B606049,1 RAE [end]
= gfThermo(lnTotRec , lnTotRec , "Preparing data for incompleted session... ")
*--------------------------------------------------
*!**************************************************************************
*! Name      : lfAdUnCmSR
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : Function Add uncomplete session
*!***************************************************************************
*! Passed Parameters : llComplete , lcCurObj
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfAdUnCmSR
PARAMETERS llComplete , lcCurObj

PRIVATE lnAlias


llComplete = IIF(TYPE('llComplete') <> 'L' , .F. , llComplete)
lcCurObj   = IIF(TYPE('lcCurObj') <> 'C' , '' , lcCurObj)
lnAlias = SELECT(0)

SELECT UnCmSess
IF lnUnCmRec = 0
  IF !SEEK('I')
    APPEND BLANK
  ENDIF
  lnUnCmRec = RECNO()
  =RLOCK()
ELSE
  GO lnUnCmRec
ENDIF

BLANK
REPLACE Status     WITH IIF(llComplete , 'C' , 'O')       ,;
        cUTranType WITH 'SMMASCA' ,;
        cUserId    WITH gcUser_id ,;
        cSession   WITH lcSession ,;
        cProgram   WITH 'SMMASCA' ,;
        cCurrScr   WITH 'SMMASCA' ,;
        cCurrObj   WITH lcCurObj  ,;
        dTranDate  WITH gdSysDate ,;
        cTranTime  WITH TIME()
IF !llComplete
    lcFiles = "lcTmpFil," + lcTmpFil +"," + lcTmpFil +";"
    =gfSavSess('SMMASCA' , lcFiles , @laVarTSav , lcSession)
ENDIF

UNLOCK
=RLOCK()

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfChkAlloc
*! Developer : Hossam El Etreby
*! Date      : 01/30/1999
*! Purpose   : Function to Check the allocation status
*!***************************************************************************
*! Passed Parameters : lfChkAlloc ==> Cancelation Type
*!***************************************************************************
*! Return      : Allocation Message Flag
*!***************************************************************************
*!This Function Due to B602475,B602476,602493
*!***************************************************************************
*! B602475,1 Check The CutPick File to Prevent cancelling picked CT/PO
*! B602476,1 Check The CutPick File to ask the user to Releas the Allocation or Not
*! B602493,1 Check In SO Cancelation Releas the allocation & Update the allocated SO/CT
*!***************************************************************************
FUNCTION lfChkAlloc

PARAMETER lcCancType
PRIVATE lnRespons
DO CASE
  CASE lcCancType = 'C'
    SELECT (lcTmpFil)
    SCAN FOR LLSEL = .T.
        *--AAN B803671,1[Begin] Don't use CutPick unless PO or MF or PS installed.
        *IF SEEK('1' + &lcTmpFil..CUTTKT + &lcTmpFil..Style ,'CutPick')
        IF llOpenCut AND SEEK('1' + &lcTmpFil..CUTTKT + &lcTmpFil..Style ,'CutPick')
        *--AAN B803671,1[End]
          *--Check The ordline PikTkt
          *--If There is PickTkt Display Message & Do nothing
          IF SEEK(lcRpCrt + CutPick.Order, 'ORDLINE')
            SELECT ORDLINE
            *B606049,1 RAE Modify the performance. [Start]            
            *LOCATE FOR cOrdType + Order + Style = 'O' + CutPick.Order + &lcTmpFil..Style
            LOCATE FOR cordtype+order+STR(lineno,6) = 'O' + CutPick.Order .AND. ;
                       style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = &lcTmpFil..Style
            *B606049,1 RAE [End]
            IF FOUND() .AND. !EMPTY(ORDLINE.PIKTKT)
              IF !llPickMsg
                lcMsg = 'There is an allocated SO, with a Pick Ticket, for these CT lines. '+;
                        'You have to release this Pick Ticket manually first'
                =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
              ENDIF
              *--MARK THE LINE AS NOT SELECTED
              SELECT(lcTmpFil)
              REPLACE LLSEL WITH .F.
              llPickMsg = .T.
              LOOP
            ELSE
              *B606049,1 RAE Modify the performance. [Start]
              *LOCATE FOR cOrdType + Order + Style = 'C' + CutPick.Order + &lcTmpFil..Style
              LOCATE FOR cordtype+order+STR(lineno,6) = 'C' + CutPick.Order .AND. ;
                         style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = &lcTmpFil..Style
              *B606049,1 RAE [End]
              IF FOUND() .AND. !EMPTY(ORDLINE.PIKTKT)
                IF !llPickMsg
                  lcMsg = 'There is an allocated SO, with a Pick Ticket, for these CT lines. '+;
                          'You have to release this Pick Ticket manually first'
                  =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
                ENDIF
                *--MARK THE LINE AS NOT SELECTED
                SELECT(lcTmpFil)
                REPLACE LLSEL WITH .F.
                llPickMsg = .T.
                LOOP
              ENDIF

            ENDIF
          ENDIF
          IF !llAlocMsg
            *--Check if we didn't display the allocation message
            lcMsg = 'These cutting tickets have allocated quantities from order(s),'+;
                    'canceling these lines will release the allocation, would you like to continue ?'
            lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
            IF lnRespons = 1
              llOkCancel = .T.
            ELSE
              llOkCancel = .F.
            ENDIF
            llAlocMsg = .T.
          ENDIF
          *--Replace the current Temp. record with the selected Y/N
          SELECT(lcTmpFil)
          REPLACE LLSEL WITH llOkCancel
        ENDIF
    ENDSCAN

  CASE lcCancType = 'P'
    SELECT (lcTmpFil)
    SCAN FOR LLSEL = .T.
        *--AAN B803671,1[Begin] Don't use CutPick unless PO or MF or PS installed.
        IF llOpenCut AND SEEK('2' + &lcTmpFil..PO + &lcTmpFil..Style ,'CutPick')
        *--AAN B803671,1[End]
          *--Check The ordline PikTkt
          *--If There is PickTkt Display Message & Do nothing
          IF SEEK('O' + CutPick.Order , 'ORDLINE') .OR. SEEK('C' + CutPick.Order , 'ORDLINE')
            SELECT ORDLINE
            *B606049,1 RAE Modify the performance. [Start]           
            *LOCATE FOR cOrdType + Order + Style = 'O' + CutPick.Order + &lcTmpFil..Style

            *khalid
            *LOCATE FOR cordtype+order+STR(lineno,6) = 'O' + CutPick.Order .AND.,;
                       style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = &lcTmpFil..Style
            LOCATE FOR cordtype+order+STR(lineno,6) = 'O' + CutPick.Order .AND. ;
                       style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = &lcTmpFil..Style

            *B606049,1 RAE [End]
            IF FOUND() .AND. !EMPTY(ORDLINE.PIKTKT)
              IF !llPickMsg
                lcMsg = 'There is an allocated SO, with a Pick Ticket, for these PO lines. '+;
                        'You have to release this Pick Ticket manually first'
                =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
              ENDIF
              *--MARK THE LINE AS NOT SELECTED
              SELECT(lcTmpFil)
              REPLACE LLSEL WITH .F.
              llPickMsg = .T.
              LOOP
            ELSE
              *B606049,1 RAE Modify the performance. [Start]
              *LOCATE FOR cOrdType + Order + Style = 'C' + CutPick.Order + &lcTmpFil..Style
              *ashraf
              *LOCATE FOR cordtype+order+STR(lineno,6) = 'C' + CutPick.Order .AND.,;
                         style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = &lcTmpFil..Style
              LOCATE FOR cordtype+order+STR(lineno,6) = 'C' + CutPick.Order .AND. ;
                         style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = &lcTmpFil..Style                         
              *B606049,1 RAE [End]
              IF FOUND() .AND. !EMPTY(ORDLINE.PIKTKT)
                IF !llPickMsg
  
                  lcMsg = 'There is an allocated SO, with a Pick Ticket, for these PO lines. '+;
                          'You have to release this Pick Ticket manually first'
                  =gfModalGen('TRM32059B34000','DIALOG',lcMsg)
        
                ENDIF
                *--MARK THE LINE AS NOT SELECTED
                SELECT(lcTmpFil)
                REPLACE LLSEL WITH .F.
                llPickMsg = .T.
                LOOP
              ENDIF
            ENDIF
          ENDIF
          IF !llAlocMsg
            *--Check if we didn't display the allocation message
            lcMsg = 'These Purchase Orders have allocated quantities from order(s),'+;
                    'canceling these lines will release the allocation, would you like to continue ?'
            lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
            IF lnRespons = 1
              llOkCancel = .T.
            ELSE
              llOkCancel = .F.
            ENDIF
            llAlocMsg = .T.
          ENDIF
          *--Replace the current Temp. record with the selected Y/N
          SELECT(lcTmpFil)
          REPLACE LLSEL WITH llOkCancel
        ENDIF
    ENDSCAN

  CASE lcCancType $ 'OS'
    SELECT (lcTmpFil)
    SCAN FOR LLSEL = .T.
      *--Check we didn't display messages before
      *--AAN B803671,1[Begin] Don't use CutPick unless PO or MF or PS installed.
      IF llOpenCut
      *--AAN B803671,1[End]
        SELECT CUTPICK
        lcCutPkOrd = ORDER()
        SET ORDER TO TAG CUTORD
        *--scan the cutpick file for any selected orders
        *B606049,1 RAE Modify the performance. [Start]
        *SCAN FOR ORDER  = &lcTmpFil..ORDER .AND. ALLTRIM(CUTPICK.CORDLINE) = ALLTRIM(STR(&lcTmpFil..LINENO))
        SCAN FOR trancd+order+cordline = '1' + &lcTmpFil..ORDER .OR.  ;
                 trancd+order+cordline = '2' + &lcTmpFil..ORDER .AND. ;
                 ALLTRIM(CUTPICK.CORDLINE) = ALLTRIM(STR(&lcTmpFil..LINENO))
        *B606049,1 RAE [End]
          *--If we didn't display the releas allocation message
          *--Check if the allocation is found
          IF !llAlocMsg
            *B603368,1 AMM Display the message due to the user privilige.
            *lcMsg = 'These Sales Order lines have allocated quantities,'+;
                      'canceling these lines will release the allocation, would you like to continue?'
            *lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
            IF llUsrCPrv
              lcMsg = 'These Sales Order lines have allocated quantities,'+;
                      'canceling these lines will release the allocation, would you like to continue?'
              lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
            ELSE
              lcMsg = 'These Sales Order lines have allocated quantities,'+;
                      'canceling these lines will release the allocation, but access to do this is denied, Cannot proceed.'
              =gfModalGen('TRM32059B00000','DIALOG',lcMsg)
              lnRespons = 2
            ENDIF
            *B603368,1 AMM end
            llAlocMsg    = .T.
            IF lnRespons = 1
              llOkCancel = .T.
            ELSE
              llOkCancel = .F.
            ENDIF
          ENDIF
          REPLACE &lcTmpFil..LLSEL WITH llOkCancel

          *--Check for CT/PO updates only if on Hold
          IF !llUpdPoCt .AND. !llAcordMsg AND llOkCancel
            IF CUTPICK.TRANCD = '1'
              IF SEEK(CUTPICK.CTKTNO,'CUTTKTH') .AND. CUTTKTH.STATUS = 'H'
                lcMsg = 'CT/PO is Hold, Would You like to update the generated CT/PO accordingly?'
                lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
                llAcordMsg = .T.
                IF lnRespons = 1
                  llUpdPoCt  = .T.
                ELSE
                  llUpdPoCt  = .F.
                ENDIF
              ENDIF
            ELSE
              IF (SEEK('P'+CUTPICK.CTKTNO,'POSHDR') .OR. SEEK('C'+CUTPICK.CTKTNO,'POSHDR')) .AND. POSHDR.STATUS = 'H'
                lcMsg = 'CT/PO is Hold, Would You like to update the generated CT/PO accordingly?'
                lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
                llAcordMsg = .T.
                IF lnRespons = 1
                  llUpdPoCt  = .T.
                ELSE
                  llUpdPoCt  = .F.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDSCAN
      *--AAN B803671,1[Begin] End the previous if stat.
      ENDIF
      *--AAN B803671,1[End]
  ENDSCAN
ENDCASE

*!**************************************************************************
*! Name      : lfRelAllOc
*! Developer : Hossam El Etreby
*! Date      : 01/30/1999
*! Purpose   : Function to Check the allocation status
*!***************************************************************************
*! Passed Parameters : lfChkAlloc ==> Cancelation Type
*!***************************************************************************
*! Return      : NONE
*!***************************************************************************
*!This Function Due to B602476,602493
*!***************************************************************************
*! B602476,1 Releas the Allocation in CT/PO
*! B602493,1 Releas the allocation & Update the allocated SO/CT/PO
*!***************************************************************************

FUNCTION lfRelAllOc

PARAMETER lcCanType
DO CASE
  CASE lcCanType = 'C'
      *--AAN B803671,1[Begin] Don't use CutPick unless PO or MF or PS installed.
      *--B602505,1 HDM 02/11/1999 Operato / Operand Not Found When CT Releasing allocation[start]
      *= SEEK('1' + CUTTKT + Style , 'CutPick')
      IF llOpenCut AND SEEK('1' + &lcTmpFil..CUTTKT + &lcTmpFil..Style , 'CutPick')
      *--AAN B803671,1[End]
      *--B602505,1 HDM 02/11/1999 Operato / Operand Not Found When CT Releasing allocation[End]
      IF !EMPTY(CutPick.ORDER)
        SELECT ORDLINE
      
        IF SEEK('O' + CutPick.Order + CutPick.CORDLINE) .OR. SEEK('C' + CutPick.Order + CutPick.CORDLINE)
          *--Zero out Cut1,.....TotCut Fields in OrdLine
          REPLACE CUT1   WITH CUT1   - CutPick.QTY1,;
                  CUT2   WITH CUT2   - CutPick.QTY2,;
                  CUT3   WITH CUT3   - CutPick.QTY3,;
                  CUT4   WITH CUT4   - CutPick.QTY4,;
                  CUT5   WITH CUT5   - CutPick.QTY5,;
                  CUT6   WITH CUT6   - CutPick.QTY6,;
                  CUT7   WITH CUT7   - CutPick.QTY7,;
                  CUT8   WITH CUT8   - CutPick.QTY8,;
                  TOTCUT WITH TOTCUT - CutPick.TOTQTY
        ENDIF
        SELECT ORDHDR
        SET ORDER TO Ordhdr
        IF SEEK('O' + CutPick.Order,'ORDHDR') OR SEEK('C' + CutPick.Order,'ORDHDR')
          REPLACE TOTCUT WITH (TOTCUT - CutPick.TOTQTY)
        ENDIF
      ENDIF
    
      *--Delete Allocated lines from CutPik File (Releas Allocation)
      SELECT CutPick
      BLANK
      DELETE
    *-- Releas the allocated QTYs From CutTktl file
    SELECT CutTktl
    lcCtTag = ORDER()
    SET ORDER TO TAG Cutlin
    
    IF SEEK(M.CUTTKT + M.style + STR(M.lineno,6) + '1')
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
    SET ORDER TO TAG &lcCtTag
    SELECT CUTTKTH
    SET ORDER TO Cuttkth
    IF SEEK(M.CUTTKT)
      REPLACE TOTORD WITH 0
    ENDIF
  ENDIF 
  CASE lcCanType = 'P'

    *--AAN B803671,1[Begin] Don't use CutPick unless PO or MF or PS installed.
    IF llOpenCut
    *--AAN B803671,1[End] 
      SELECT CutPick
      = SEEK('2' + &lcTmpFil..PO + &lcTmpFil..Style , 'CutPick')
      SCAN REST WHILE trancd + ctktno + style = '2' + &lcTmpFil..PO + &lcTmpFil..Style
      IF !EMPTY(CutPick.ORDER)
        SELECT ORDLINE
      
        IF SEEK('O' + CutPick.Order + CutPick.CORDLINE) .OR. SEEK('C' + CutPick.Order + CutPick.CORDLINE)
          *--Zero out Cut1,.....TotCut Fields in OrdLine
          REPLACE CUT1   WITH CUT1   - CutPick.QTY1,;
                  CUT2   WITH CUT2   - CutPick.QTY2,;
                  CUT3   WITH CUT3   - CutPick.QTY3,;
                  CUT4   WITH CUT4   - CutPick.QTY4,;
                  CUT5   WITH CUT5   - CutPick.QTY5,;
                  CUT6   WITH CUT6   - CutPick.QTY6,;
                  CUT7   WITH CUT7   - CutPick.QTY7,;
                  CUT8   WITH CUT8   - CutPick.QTY8,;
                  TOTCUT WITH TOTCUT - CutPick.TOTQTY
        ENDIF
        SELECT ORDHDR
        SET ORDER TO Ordhdr
        IF SEEK('O' + CutPick.Order,'ORDHDR') OR SEEK('C' + CutPick.Order,'ORDHDR')
          REPLACE TOTCUT WITH (TOTCUT - CutPick.TOTQTY)
        ENDIF
      ENDIF
    ENDSCAN
  *--AAN B803671,1[Begin] End the previous if stat.
  ENDIF
  *--AAN B803671,1[End]
    
    *-- Releas the allocated QTYs From POSLN file
    SELECT POSLN
    IF SEEK('P'+M.PO + M.style + STR(M.lineno,6) + '1') OR SEEK('C'+M.PO + M.style + STR(M.lineno,6) + '1')
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
    SELECT POSHDR
    SET ORDER TO POSHDR
    IF SEEK('P'+M.PO) OR SEEK('C'+M.PO)
      REPLACE TOTORD WITH 0
    ENDIF
    
    *--AAN B803671,1[Begin] Don't use CutPick unless PO or MF or PS installed.
    IF llOpenCut
    *--AAN B803671,1[End] 
      *--Delete Allocated lines from CutPik File (Releas Allocation)
      SELECT CutPick
      IF SEEK('2' + &lcTmpFil..PO + &lcTmpFil..Style , 'CutPick')
        DELETE REST WHILE trancd + ctktno + style = '2' + &lcTmpFil..PO + &lcTmpFil..Style
      ENDIF
    *--AAN B803671,1[Begin] End the previous if stat.
    ENDIF
    *--AAN B803671,1[End]
 
  CASE lcCanType $ 'SO'
 
    *--AAN B803671,1[Begin] Don't use CutPick unless PO or MF or PS installed.
    IF llOpenCut
    *--AAN B803671,1[End]
      SELECT CUTPICK
      lcCutPkOrd = ORDER()
      SET ORDER TO TAG CUTORD
      *B606049,1 RAE Modify the performance.[start]
      *GOTO TOP
      LOCATE
      *B606049,1 RAE [end]

      *--HDM B602560,1 Scan Should be on line level[start]
      *SCAN FOR ORDER = &lcTmpFil..ORDER
      *B606049,1 RAE Modify the performance. [start]      
      *SCAN FOR ORDER = &lcTmpFil..ORDER .AND. ALLTRIM(CUTPICK.CORDLINE) = ALLTRIM(STR(&lcTmpFil..LINENO))
      SCAN FOR trancd+order+cordline = '1'+ &lcTmpFil..ORDER + SUBSTR(STR(&lcTmpFil..LINENO),5,10) .OR. ;
               trancd+order+cordline = '2'+ &lcTmpFil..ORDER + SUBSTR(STR(&lcTmpFil..LINENO),5,10)
      *B606049,1 RAE [End]
        *--HDM B602560,1 Scan Should be on line level[End]
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
                  REPLACE QTY1   WITH QTY1   - CUTPICK.QTY1,;
                          QTY2   WITH QTY2   - CUTPICK.QTY2,;
                          QTY3   WITH QTY3   - CUTPICK.QTY3,;
                          QTY4   WITH QTY4   - CUTPICK.QTY4,;
                          QTY5   WITH QTY5   - CUTPICK.QTY5,;
                          QTY6   WITH QTY6   - CUTPICK.QTY6,;
                          QTY7   WITH QTY7   - CUTPICK.QTY7,;
                          QTY8   WITH QTY8   - CUTPICK.QTY8,;
                          TOTQTY WITH TOTQTY - CUTPICK.TOTQTY
                  SELECT STYLE
                  lcStyOdr = ORDER()
                  SET ORDER TO STYLE
                  IF SEEK(CUTTKTL.STYLE,'STYLE')
                    =RLOCK()
                    REPLACE WIP1   WITH WIP1   - CUTPICK.QTY1  ,;
                            WIP2   WITH WIP2   - CUTPICK.QTY2  ,;
                            WIP3   WITH WIP3   - CUTPICK.QTY3  ,;
                            WIP4   WITH WIP4   - CUTPICK.QTY4  ,;
                            WIP5   WITH WIP5   - CUTPICK.QTY5  ,;
                            WIP6   WITH WIP6   - CUTPICK.QTY6  ,;
                            WIP7   WITH WIP7   - CUTPICK.QTY7  ,;
                            WIP8   WITH WIP8   - CUTPICK.QTY8  ,;
                            TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,;
                            NWO1   WITH NWO1   - CUTPICK.QTY1  ,;
                            NWO2   WITH NWO2   - CUTPICK.QTY2  ,;
                            NWO3   WITH NWO3   - CUTPICK.QTY3  ,;
                            NWO4   WITH NWO4   - CUTPICK.QTY4  ,;
                            NWO5   WITH NWO5   - CUTPICK.QTY5  ,;
                            NWO6   WITH NWO6   - CUTPICK.QTY6  ,;
                            NWO7   WITH NWO7   - CUTPICK.QTY7  ,;
                            NWO8   WITH NWO8   - CUTPICK.QTY8  ,;
                            NTOTWO WITH NTOTWO - M.TOTQTY

                    UNLOCK
                    SET ORDER TO (lcStyOdr)
                  
                    SELECT STYDYE
                    lcDyOrd = ORDER()
                    SET ORDER TO STYDYE
                    IF SEEK(CUTTKTL.STYLE+CUTTKTL.CWARECODE+PADR(CUTTKTL.DYELOT,10),'STYDYE')
                      
                      =RLOCK()
                      REPLACE WIP1   WITH WIP1   - CUTPICK.QTY1  ,;
                              WIP2   WITH WIP2   - CUTPICK.QTY2  ,;
                              WIP3   WITH WIP3   - CUTPICK.QTY3  ,;
                              WIP4   WITH WIP4   - CUTPICK.QTY4  ,;
                              WIP5   WITH WIP5   - CUTPICK.QTY5  ,;
                              WIP6   WITH WIP6   - CUTPICK.QTY6  ,;
                              WIP7   WITH WIP7   - CUTPICK.QTY7  ,;
                              WIP8   WITH WIP8   - CUTPICK.QTY8  ,;
                              TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,;
                              NWO1   WITH NWO1   - CUTPICK.QTY1  ,;
                              NWO2   WITH NWO2   - CUTPICK.QTY2  ,;
                              NWO3   WITH NWO3   - CUTPICK.QTY3  ,;
                              NWO4   WITH NWO4   - CUTPICK.QTY4  ,;
                              NWO5   WITH NWO5   - CUTPICK.QTY5  ,;
                              NWO6   WITH NWO6   - CUTPICK.QTY6  ,;
                              NWO7   WITH NWO7   - CUTPICK.QTY7  ,;
                              NWO8   WITH NWO8   - CUTPICK.QTY8  ,;
                              NTOTWO WITH NTOTWO - M.TOTQTY
                      UNLOCK
                    ENDIF
                    SET ORDER TO (lcDyOrd)
                    SELECT CUTTKTL
                  ENDIF
                ENDIF
                SET ORDER TO TAG &lcOldCTOrd
                *--B602671,1 HDM [Start] Update cutting ticket header file

                SELECT CUTTKTH
                = SEEK (CUTPICK.CTKTNO)
                lnRestOpn = PCS_OPN - CUTPICK.TOTQTY
                =RLOCK()
                REPLACE PCS_OPN WITH IIF(PCS_OPN > 0 , PCS_OPN - CUTPICK.TOTQTY , PCS_OPN),;
                        TOTORD  WITH IIF(TOTORD > 0 , TOTORD - CUTPICK.TOTQTY , TOTORD),;
                        PCS_CAN WITH PCS_CAN + CUTPICK.TOTQTY
                
                REPLACE nEst_Cost1 WITH (CUTTKTL.nCost1 * lnRestOpn),;
                        nEst_Cost2 WITH (CUTTKTL.nCost2 * lnRestOpn),;
                        nEst_Cost3 WITH (CUTTKTL.nCost3 * lnRestOpn),;
                        nEst_Cost4 WITH (CUTTKTL.nCost4 * lnRestOpn),;
                        nEst_Cost5 WITH (CUTTKTL.nCost5 * lnRestOpn)
                
                IF PCS_OPN = 0
                  REPLACE STATUS WITH IIF(PCS_REC > 0 , 'C' , 'X')
                ENDIF
                UNLOCK
                *--B602671,1 HDM [End]
              ENDIF
            ENDIF
            *--Releas The Allocation
  
            *--B602676,1 HDM 05/03/1999[Start] We have to RELEASE totcut in ordhdr file
           SELECT ORDHDR
             =SEEK (lcRpCrt+M.ORDER)
            REPLACE TOTCUT WITH TOTCUT - CutPick.TotQty
            *--B602676,1 HDM [End]


            SELECT ORDLINE
            *-- HDM B602534,1 USE order line no when seeking the ordline file[Start]
            *= SEEK(lcRpCrt + CutPick.Order)
            = SEEK(lcRpCrt + CutPick.Order + CutPick.CORDLINE)
            *-- HDM B602534,1 USE order line no when seeking the ordline file[end]
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
          
            *-- Releas the allocated QTYs From CutTktl file
            SELECT CUTTKTL
            lcOldCTOrd = ORDER()
            SET ORDER TO TAG Cutlin
            IF SEEK (CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.cTktLineNo + '1')
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
              REPLACE QTY1  WITH QTY1   - CUTPICK.QTY1,;
                      QTY2  WITH QTY2   - CUTPICK.QTY2,;
                      QTY3  WITH QTY3   - CUTPICK.QTY3,;
                      QTY4  WITH QTY4   - CUTPICK.QTY4,;
                      QTY5  WITH QTY5   - CUTPICK.QTY5,;
                      QTY6  WITH QTY6   - CUTPICK.QTY6,;
                      QTY7  WITH QTY7   - CUTPICK.QTY7,;
                      QTY8  WITH QTY8   - CUTPICK.QTY8,;
                      TOTQTY WITH TOTQTY- CUTPICK.TOTQTY
                
                PRIVATE lcStyOdr ,lcDyOrd
                SELECT STYLE
                lcStyOdr = ORDER()
                SET ORDER TO STYLE
                IF SEEK(POSLN.STYLE,'STYLE')
                  SELECT STYLE
                  =RLOCK()
                  REPLACE WIP1   WITH WIP1   - CUTPICK.QTY1  ,;
                          WIP2   WITH WIP2   - CUTPICK.QTY2  ,;
                          WIP3   WITH WIP3   - CUTPICK.QTY3  ,;
                          WIP4   WITH WIP4   - CUTPICK.QTY4  ,;
                          WIP5   WITH WIP5   - CUTPICK.QTY5  ,;
                          WIP6   WITH WIP6   - CUTPICK.QTY6  ,;
                          WIP7   WITH WIP7   - CUTPICK.QTY7  ,;
                          WIP8   WITH WIP8   - CUTPICK.QTY8  ,;
                          TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,;
                          NWO1   WITH NWO1   - CUTPICK.QTY1  ,;
                          NWO2   WITH NWO2   - CUTPICK.QTY2  ,;
                          NWO3   WITH NWO3   - CUTPICK.QTY3  ,;
                          NWO4   WITH NWO4   - CUTPICK.QTY4  ,;
                          NWO5   WITH NWO5   - CUTPICK.QTY5  ,;
                          NWO6   WITH NWO6   - CUTPICK.QTY6  ,;
                          NWO7   WITH NWO7   - CUTPICK.QTY7  ,;
                          NWO8   WITH NWO8   - CUTPICK.QTY8  ,;
                          NTOTWO WITH NTOTWO - M.TOTQTY
                  UNLOCK
                  SET ORDER TO (lcStyOdr)
                  SELECT STYDYE
                  lcDyOrd = ORDER()
                  SET ORDER TO STYDYE

                  IF SEEK(POSLN.STYLE+POSLN.CWARECODE+PADR(POSLN.DYELOT,10),'STYDYE')
                    =RLOCK()
                    REPLACE WIP1   WITH WIP1   - CUTPICK.QTY1  ,;
                            WIP2   WITH WIP2   - CUTPICK.QTY2  ,;
                            WIP3   WITH WIP3   - CUTPICK.QTY3  ,;
                            WIP4   WITH WIP4   - CUTPICK.QTY4  ,;
                            WIP5   WITH WIP5   - CUTPICK.QTY5  ,;
                            WIP6   WITH WIP6   - CUTPICK.QTY6  ,;
                            WIP7   WITH WIP7   - CUTPICK.QTY7  ,;
                            WIP8   WITH WIP8   - CUTPICK.QTY8  ,;
                            TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,;
                            NWO1   WITH NWO1   - CUTPICK.QTY1  ,;
                            NWO2   WITH NWO2   - CUTPICK.QTY2  ,;
                            NWO3   WITH NWO3   - CUTPICK.QTY3  ,;
                            NWO4   WITH NWO4   - CUTPICK.QTY4  ,;
                            NWO5   WITH NWO5   - CUTPICK.QTY5  ,;
                            NWO6   WITH NWO6   - CUTPICK.QTY6  ,;
                            NWO7   WITH NWO7   - CUTPICK.QTY7  ,;
                            NWO8   WITH NWO8   - CUTPICK.QTY8  ,;
                            NTOTWO WITH NTOTWO - M.TOTQTY
                    UNLOCK
                  
                  ENDIF
                  SET ORDER TO (lcDyOrd)
                  SELECT POSLN
                ENDIF

              SELECT POSHDR
              *--B602671,1 HDM [Start] Update PURCHASE ORDER header file
              
              IF SEEK ('P' + CUTPICK.CTKTNO) OR SEEK(('C' + CUTPICK.CTKTNO))
              =RLOCK()
                REPLACE OPEN   WITH (OPEN - CUTPICK.TOTQTY),;
                        CANCEL WITH (CANCEL+ CUTPICK.TOTQTY)
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

              *--B602671,1 HDM [End]
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
        *--B602676,1 HDM 05/03/1999[Start] We have to RELEASE totcut in ordhdr file
        SELECT ORDHDR
        =SEEK (lcRpCrt+M.ORDER)
        REPLACE TOTCUT WITH TOTCUT - CutPick.TotQty
        *--B602676,1 HDM [End]
      
        SELECT ORDLINE
        *-- HDM B602534,1 USE order line no when seeking the ordline file[START]
        *= SEEK(lcRpCrt + CutPick.Order)
        = SEEK(lcRpCrt + CutPick.Order + CutPick.CORDLINE)
        *-- HDM B602534,1 USE order line no when seeking the ordline file[end]
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
  *--AAN B803671,1[Begin] End the previous if stat.
  ENDIF
  *--AAN B803671,1[Begin]
ENDCASE

*!**************************************************************************
FUNCTION lfvread
*!**************************************************************************
*!**************************************************************************
*! Name      : lpvBrow
*! Developer : Hossam El Etreby
*! Date      : 02/28/1999
*! Purpose   : Procedure to check and call shaved quantities modification screen
*!***************************************************************************
*! Passed Parameters : none
*!***************************************************************************
*! Return      : NONE
*!***************************************************************************
*!This PROCEDURE Due to B602597
*!***************************************************************************

PROCEDURE lpvBrow

IF lcRpCanTyp = 'S' .AND. &lcTmpFil..llSel
  = gfOpenFile(gcDataDir + 'Scale' ,'Scale','SH')
  SELECT(lcTmpFil)
  SCATTER MEMVAR MEMO
  lnQty1 = M.QTY1
  lnQty2 = M.QTY2
  lnQty3 = M.QTY3
  lnQty4 = M.QTY4
  lnQty5 = M.QTY5
  lnQty6 = M.QTY6
  lnQty7 = M.QTY7
  lnQty8 = M.QTY8
  
  SELECT SCALE
  =SEEK('S'+ &lcTmpFil..SCALE)
  DO (gcScrDir + 'SOSHAVE.SPX')
  = gfCloseFile('SCALE')
ENDIF


*!**************************************************************************
*! Name      : lfvShave
*! Developer : Hossam El Etreby
*! Date      : 02/28/1999
*! Purpose   : Valid Function for shaved quantity fields (shaved quantity screen)
*!***************************************************************************
*! Passed Parameters : Quantity delimeter
*!***************************************************************************
*! Return      : NONE
*!***************************************************************************
*!This Function Due to B602597
*!***************************************************************************

FUNCTION lfvShave

PARAMETER lcSZ

IF lnQty&lcSZ > lcOldVal
  WAIT WINDOW STR(M.Qty&lcSZ,4) +' is the maximum quantity to shave'
  lnQty&lcSZ = lcOldVal
ENDIF

*!**************************************************************************
*! Name      : lfvOk
*! Developer : Hossam El Etreby
*! Date      : 05/26/1999
*! Purpose   : Valid Function for Accept button (shaved quantity screen)
*!***************************************************************************
*! Passed Parameters : NONE
*!***************************************************************************
*! Return      : NONE
*!***************************************************************************
*!This Function Due to B602597
*!***************************************************************************

FUNCTION lfvOk

SELECT(lcTmpFil)
REPLACE QTY1 WITH lnQty1,;
        QTY2 WITH lnQty2,;
        QTY3 WITH lnQty3,;
        QTY4 WITH lnQty4,;
        QTY5 WITH lnQty5,;
        QTY6 WITH lnQty6,;
        QTY7 WITH lnQty7,;
        QTY8 WITH lnQty8,;
        TOTQTY WITH lnQty1 + lnQty2 + lnQty3 + lnQty4 + lnQty5 + lnQty6 + lnQty7 + lnQty8

CLEAR READ
SHOW WINDOW (lcBrowTtl) REFRESH

*!*****************************************************************
*! Function: lfvBrows
*!*****************************************************************
*! Author : Hesham
*!*****************************************************************
*! Purpose : Stop browse case activating another screen
*!*****************************************************************
*! DUE TO Bug# 602588
*!*****************************************************************

FUNCTION lfvBrows
IF WONTOP(lcBrowTtl)
  glFromBrow = .T.
ELSE
  = gfStopBrow()
  glFromBrow = .F.  
  IF !WVISIBLE(gcBaseWind)
    glQuitting = .T.
    CLEAR READ
    RETURN TO SMFRMCD.SPR
  ENDIF  
ENDIF  

*!*****************************************************************
*! Function: lfSortType
*!*****************************************************************
*! Author : Hossam El Etreby (HDM)
*!*****************************************************************
*! Purpose : Check for installed modules to fill transaction type POPUP
*!           in the option grid
*!*****************************************************************
*! DUE TO Bug# 802166
*!*****************************************************************

FUNCTION lfSortType
PRIVATE lnLastDim

DIMENSION laSortType[1,2] , laTypeVal[1,1]
*B603368,1 AMM Check the user access
*IF OCCURS('SO',gcComp_mdl) > 0
IF OCCURS('SO',gcComp_mdl) > 0 .AND. llUsrSPrv
*B603368,1 AMM  end
  laSortType[1,1] = 'Sales Order'
  laSortType[1,2] = 'SO'
  laTypeVal[1]  = 'O'
  DIMENSION laSortType[2,2] , laTypeVal[2,1]
  laSortType[2,1] = 'Order Shaving'
  laSortType[2,2] = 'SO'
  laTypeVal[2]  = 'S'

ENDIF

IF !EMPTY(laSortType[1])
  *B603368,1 AMM check user access 
  *IF OCCURS('PO',gcComp_mdl) > 0
  IF OCCURS('PO',gcComp_mdl) > 0 .AND. llUsrPPrv
  *B603368,1 AMM  end
    DIMENSION laSortType[ALEN(laSortType,1)+1,2] , laTypeVal[ALEN(laSortType,1)+1,2]
    lnLastDim = ALEN(laSortType,1)
    DIMENSION laSortType[ALEN(laSortType,1),2] , laTypeVal[ALEN(laSortType,1),2]
    laSortType[lnLastDim,1] = 'Purchase Order'
    laSortType[lnLastDim,2] = 'PO'
    laTypeVal[lnLastDim]  = 'P'
    
    *E301865,1 AMH Add Inter-location purchase order [Start]
    DIMENSION laSortType[lnLastDim+1,2] , laTypeVal[lnLastDim+1,2]
    laSortType[lnLastDim+1,1] = 'Inter-Location Purchase Order'
    laSortType[lnLastDim+1,2] = 'PO'
    laTypeVal[lnLastDim+1]  = 'N'
    *E301865,1 AMH [End]
    
  ENDIF
ELSE
  *B603368,1 AMM check user access
  *IF OCCURS('PO',gcComp_mdl) > 0
  IF OCCURS('PO',gcComp_mdl) > 0 .AND. llUsrPPrv
  *B603368,1 AMM  end
    laSortType[1,1] = 'Purchase Order'
    laSortType[1,2] = 'PO'
    laTypeVal[1]  = 'P'
    
    *E301865,1 AMH Add Inter-location purchase order [Start]
    DIMENSION laSortType[2,2] , laTypeVal[2,2]
    laSortType[2,1] = 'Inter-Location Purchase Order'
    laSortType[2,2] = 'PO'
    laTypeVal[2]  = 'N'
    *E301865,1 AMH [End]
    
  ENDIF
ENDIF


IF !EMPTY(laSortType[1])
  *B603368,1 AMM check user access  
  *IF OCCURS('MF',gcComp_mdl) > 0
  IF OCCURS('MF',gcComp_mdl) > 0 .AND. llUsrCPrv
  *B603368,1 AMM  end
    DIMENSION laSortType[ALEN(laSortType,1)+1,2] , laTypeVal[ALEN(laSortType,1)+1,2]
    lnLastDim = ALEN(laSortType,1)
    DIMENSION laSortType[ALEN(laSortType,1),2] , laTypeVal[ALEN(laSortType,1),2]
    laSortType[lnLastDim,1] = 'Cutting Ticket'
    laSortType[lnLastDim,2] = 'MF'
    laTypeVal[lnLastDim]  = 'C'
  ENDIF
ELSE
  *B603368,1 AMM check user access 
  *IF OCCURS('MF',gcComp_mdl) > 0
  IF OCCURS('MF',gcComp_mdl) > 0 .AND. llUsrCPrv
  *B603368,1 AMM  end
    laSortType[1,1] = 'Cutting Ticket'
    laSortType[1,2] = 'MF'
    laTypeVal[1]  = 'C'
  ENDIF
ENDIF
*-- B802665,1 HDM [START] Sort the array according to the active module

DIMENSION laDummy[ALEN(laSortType,1),2]
*B603368,1 AMM Get the actual number of colums and rows
*DIMENSION laDummyVal[ALEN(laTypeVal,1),2]
DIMENSION laDummyVal[ALEN(laTypeVal)]
*B603368,1 AMM end

lnActPos = ASUBSCRIPT(laSortType,ASCAN(laSortType,gcAct_appl),1)
lnActEl  = ASCAN(laSortType,gcAct_appl) -1
*-- Copy array elements starting from the active module element
= ACOPY(laSortType , laDummy    , lnActEl)
= ACOPY(laTypeVal  , laDummyVal , lnActPos)
*-- get the no. of copied rows to the dummy array
lnCopied = (ALEN(laSortType,1)+1) - lnActPos

*-- get the no. of remaining rows to be copied
lnRemain = ALEN(laSortType,1) - lnCopied

*-- Loop to copy the rest
FOR lnI = 1 TO lnRemain
  lnRowNum = lnI + lnCopied
  laDummy[lnRowNum,1]  = laSortType[lnActPos - lnI,1]
  laDummy[lnRowNum,2]  = laSortType[lnActPos - lnI,2]
  laDummyVal[lnRowNum] = laTypeVal[lnActPos - lnI]
ENDFOR

= ACOPY(laDummy , laSortType)
= ACOPY(laDummyVal , laTypeVal)
*B603785,1 (Begin) Call lfSortDumy after lcRpCanTyp had got the new default.
= lfSortDumy ()
*B603785,1 (End)
*-- B802665,1 HDM [END]

*!*****************************************************************
*! Function: lfvCrt
*!*****************************************************************
*! Author : Hossam El Etreby (HDM)
*!*****************************************************************
*! Purpose : NEW FUNCTION TO HANDLE THE lcRpCRT variable.
*!*****************************************************************
*-- B802665,1 HDM [START] 
FUNCTION lfvCrt

IF lcRpCanTyp = 'O' OR lcRpCanTyp = 'S'
  lcRpCrt = 'O'
ELSE
  IF lcRpCanTyp = 'P'
    lcRpCrt = 'O'
  *--MHM COMMENT IT HERE  
  *ELSE
  *--MHM
    *lcRpCrt = ''
  ENDIF
  
  *E301865,1 AMH Add Inter-location PO [Start]
  IF lcRpCanTyp = 'N'
    lcRpCrt = 'N'
  ENDIF
  *E301865,1 AMH [End]
  
ENDIF
*-- B802665,1 HDM [END]


*!*****************************************************************
*! Function: lfReadAct()
*!*****************************************************************
*! Author  : Adel Mohammed El Gazzar (ADEL)
*!*****************************************************************
*! Purpose : Read activate function
*!*****************************************************************
*! Date    : 08/08/2000 
*!*****************************************************************
*!B603785,1 
*!*****************************************************************
FUNCTION lfReadAct

ON KEY LABEL TAB
ON KEY LABEL BACKTAB


*!*****************************************************************
*! Function: lfClrTrap
*!*****************************************************************
*! Author  : Adel Mohammed El Gazzar (ADEL)
*!*****************************************************************
*! Purpose : Trap Browse window
*!*****************************************************************
*! Date    : 08/08/2000 
*!*****************************************************************
*!B603785,1 
*!*****************************************************************
FUNCTION lfClrTrap
IF WONTOP() = (lcBrowTtl)
  ON KEY LABEL TAB     DO lpFld4Tab WITH (lcMasCaCh2),pbSelect
  ON KEY LABEL BACKTAB DO lpBackTab
ENDIF

*!*****************************************************************
*! Function: lpFld4Tab
*!*****************************************************************
*! Author  : Adel Mohammed El Gazzar (ADEL)
*!*****************************************************************
*! Purpose : Trap Browse window
*!*****************************************************************
*! Date    : 08/08/2000 
*!*****************************************************************
*!B603785,1 
*!*****************************************************************
PROCEDURE lpFld4Tab
PARAMETERS lcWindToAct,lcObject

ACTIVATE WINDOW (lcWindToAct)
_CUROBJ=OBJNUM(lcObject)

*!*****************************************************************
*! Function: lpFld4Tab
*!*****************************************************************
*! Author  : Adel Mohammed El Gazzar (ADEL)
*!*****************************************************************
*! Purpose : Trap Browse window
*!*****************************************************************
*! Date    : 08/08/2000 
*!*****************************************************************
*!B603785,1 
*!*****************************************************************
PROCEDURE lpBackTab

lcWind = IIF(lcRpCanTyp $'OS',(lcMasCaCh0),"gwccontrl1")
lcObj  = IIF(lcRpCanTyp $'OS','puReason','pbCls')
ACTIVATE WINDOW (lcWind)
_CUROBJ=OBJNUM(lcObj)

*!*****************************************************************
*! Function: lfAdCnLine
*!*****************************************************************
*! Author  : Walid A. Wahab (WAB)
*!*****************************************************************
*! Purpose : add cancel qty for the compled po
*!*****************************************************************
*! Date    : 04/07/2003
*!*****************************************************************
*!B606743,1
*!*****************************************************************
FUNCTION lfAdCnLine

lcHDrFile = IIF(lcRpCanTyp='C','CutTktH','POSHDR')
lcFilToUSe= IIF(lcRpCanTyp='C','CutTktl','PosLn')

SELECT (lcTmpFil)
SCAN FOR TRANCD = '1'
  lcSeekExp  = IIF(lcRpCanTyp='C',CutTkt,lcRpCrt+PO)
  IF SEEK(lcSeekExp ,lcHDrFile) AND &lcHDrFile..STATUS = 'C'
    SCATTER MEMVAR MEMO
    m.TranCd = IIF(lcRpCanTyp='C','4','5')
    *B121328,1 NNA 01/28/2004 [Begin] Save The cancellation date
    m.date= gdSysDate
    *B121328,1 NNA [End]
    SELECT (lcFilToUSe)
    APPEND BLANK
    GATHER MEMVAR MEMO
    SELECT (lcTmpFil)
  ENDIF
ENDSCAN
**************************************************************
*:B128212,1 MMR 07/14/2005 Fix bug of no records if i include only this color in filter.[Start]
*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Mostafa Rawash (MMR)
*! Date      : 06/14/2005
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
*:B128212,1 MMR [End]

