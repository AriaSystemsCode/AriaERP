*:***************************************************************************
*: Program file  : POAND50.PRG
*: Program desc. : Get 3 new dates for the purchase order shipments.
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Order (PO)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Andrew Mark
*:***************************************************************************
*C101391,1 KHM 02/08/99
*:***************************************************************************
*:B101706,1 ADEL 12/07/1999 Add 2 new fields (Ship via and ship/advise #)
*:***************************************************************************

*--Open files
=gfOpenFile(gcDataDir+'SHPMTHDR','SHPMTHDR','SH')
=gfOpenFile(gcDataDir+'SHPDATES','SHPDATES','SH')
SELECT SHPMTHDR
SET RELATION TO ShipNo INTO SHPDATES

*B101706 (Begin) Open codes file and initialize needed var.
*--Open Codes file to get all avaliable ship via codes.
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH' )
*--Declare needed array and variables for function gfwCodePop.
DECLARE laCodes[1,10],laShipVia[1,2]
laCodes = ''
laShipVia = ''
laCodes[1,1] = 'SHIPVIA'
laCodes[1,2] = 'laShipVia'
laCodes[1,3] = 'lnShipVia'
laCodes[1,4] = ''
laCodes[1,5] = .F.
laCodes[1,6] = .F.
laCodes[1,10] = 'SHIPVIA'
*B101706 (End)

*---Initilaize variables
*--lcShipNo   && Holds the shipment no.
*--lcStatus   && Holds the Po's status.
*--ldEntered  && Holds the entered date.
*--lnCartons  && Holds the no. of cartons.
*--lcAirWayB  && Holds the air way field value.
*--ldEta      && Holds the E T A date.
*--ldCustDate && Holds the customs date.
*--ldTrckDate && Holds the trucker date.
*--ldWareDate && Holds the warehouse date.
*--lcScreMode && Holds the screen mode.
*--llBrowse   && Holds the Browse flag. 
*--lcBrowTtl  && Holds the browse title.
*--lcBrFields && Holds the fields names.
*--ldOldVal   && Holds the old value of ldCustDate,ldTrckDate,ldWareDate
*--llChanges  && Flag to see if the user did any changes.
*---Initilize var.
STORE ' ' TO lcShipNo,lcStatus,lcAirWayB
STORE 0   TO lnCartons
STORE {}  TO ldEta,ldEntered,ldCustDate,ldTrckDate,ldWareDate,ldOldVal
lcScreMode = 'S'
STORE .F. TO llBrowse,llChanges
lcBrowTtl  = "Shipments"
lcBrFields = "SHIPNO    :H= 'Shipment#'       ,"+;
             "STATUS    :H= 'Status'          ,"+;
             "ENTERED   :H= 'Entered'         ,"+;
             "CARTONS   :H= 'CTNS.   '        ,"+;
             "AIRWAYB   :H= 'AIRWAY - BILL #' ,"+;
             "ETA       :H= 'E.T.A'           "

ON KEY LABEL ESC DO lfOnEsc
*-- Call the screen
DO gcScrDir+'PO\POAND50.SPR'

*!*************************************************************
*! Name      : lfvShipNo
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/08/1999
*! Purpose   : Validate the shipno field.
*!*************************************************************
*! Example     : =lfvShipNo()
*!*************************************************************
FUNCTION lfvShipNo

*--Go to ibShipNo if it was pressed.
IF !llBrowse AND MDOWN()
  RETURN
ENDIF
*-- Quit if browse button was not pressed and the field was left empty.
IF !llBrowse AND EMPTY(lcShipNo)
  RETURN
ENDIF

SELECT SHPMTHDR
IF llBrowse OR ("?" $ lcShipNo) OR !SEEK(lcShipNo)
  IF RECCOUNT() =0
    = gfDialog('I',"No records to display." )
    _CUROBJ = OBJNUM(lcShipNo)
    RETURN
  ENDIF
  DECLARE laReturn[1] 
  STORE '' TO laReturn
  *--To speed the browse
  GO TOP
  =AriaBrow('', lcBrowTtl, gnBrFSRow1, gnBrFSCol1,;
            gnBrFSRow2, gnBrFSCol2,'','','ShipNo','laReturn')
  lcShipNo = laReturn(1)
  llBrowse = .F.
  *--Esc was pressed.          
  IF EMPTY(lcShipNo)
    _CUROBJ = OBJNUM(lcShipNo)
    RETURN
  ENDIF
ENDIF
*---Valid ship no. found in the file
*--Get the says fields.
lcStatus  = IIF(STATUS = 'O','OPEN',IIF(STATUS = 'C','COMPLT',IIF(STATUS = 'X','CANCLD','HOLD')))
ldEntered = ENTERED
lnCartons = CARTONS
lcAirWayB = AIRWAYB
ldEta     = ETA
*--See whether it exists in the SHPDATES file
IF !EOF('SHPDATES')
  ldCustDate  = SHPDATES.dCustDate
  ldTrckDate  = SHPDATES.dTrckDate
  ldWareDate  = SHPDATES.dWareDate
  *B101706 (Begin) Get the ship advise.
  lcShipAdv   = SHPDATES.cShipAdvse
  *B101706 (End)
  lcScreMode  = 'V'
ELSE  
  lcScreMode  = 'A'
ENDIF
*B101706 (Begin) Fill the ship via popup and Get the ship via for current ship no..
*--Fill the ship via popup.
=gfwCodePop(@laCodes,'SHIPVIA','L')
*-- Get the ship via for current ship no.
lnPos = ASCAN(laShipVia,SHPDATES.SHIPVIA)
IF lnPos >0
  *--Get the ship via row to be refreshed with SHOW GETS.
  lnShipVia  = ASUBSCRIPT(laShipVia,lnPos,1)
ENDIF
*B101706 (End)
SHOW GETS
*--Refresh says fields.
=lfRefresh()

*!*************************************************************
*! Name      : lfvEdit
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/08/1999
*! Purpose   : Validate the Edit button.
*!*************************************************************
*! Example     : =lfvEdit()
*!*************************************************************
FUNCTION lfvEdit

lcScreMode  = 'E'
SHOW GETS

*!*************************************************************
*! Name      : lfvDelete
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/08/1999
*! Purpose   : Validate the Delete button.
*!*************************************************************
*! Example     : =lfvDelete()
*!*************************************************************
FUNCTION lfvDelete
PRIVATE lcAlias

lcAlias = ALIAS()
IF gfDialog('I','Are you sure you want to delete shipment dates?','\<Yes;\<No' )=1 ;
   AND SEEK(lcShipNo,'SHPDATES')
   SELECT SHPDATES
   DELETE
   =lfClear()
   lcScreMode = 'S'
   SHOW GETS
ENDIF
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/08/1999
*! Purpose   : Validate the Select button.
*!*************************************************************
*! Example     : =lfvSelect()
*!*************************************************************
FUNCTION lfvSelect

*--Clear fields and get the Select mode
=lfClear()
lcScreMode = 'S'
SHOW GETS

*!*************************************************************
*! Name      : lfvSave
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/08/1999
*! Purpose   : Validate the Save button.
*!*************************************************************
*! Example     : =lfvSave()
*!*************************************************************
FUNCTION lfvSave

SELECT SHPDATES
llChanges = .F.
*--Insert a new record in case oa add mode.
IF lcScreMode = 'A'
  APPEN BLANK
ENDIF  
REPLACE  ShipNo    WITH lcShipNo  ,;
         dCustDate WITH ldCustDate,;
         dTrckDate WITH ldTrckDate,;
         dWareDate WITH ldWareDate
*B101706 (Begin) Update ship via and advise.
REPLACE SHPDATES.SHIPVIA   WITH ALLTRIM(laShipVia[lnShipVia,2]),;
        SHPDATES.CShipAdvse WITH lcShipAdv 
*B101706 (End)
*--Clear fields and get the Select mode
=lfClear()
lcScreMode = 'S'
SHOW GETS
	
*!*************************************************************
*! Name      : lfvClose
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/08/1999
*! Purpose   : Validate the Close button.
*!*************************************************************
*! Example     : =lfvClose()
*!*************************************************************
FUNCTION lfvClose

*--If we are in the View or Select modes,Terminate.
IF lcScreMode $ 'VS'
  CLEAR READ
  RETURN
ENDIF
*--If we are in the Add or Edit modes.
IF (!llChanges) OR (llChanges AND gfDialog('I','Are you sure? You will lose all changes.','\<Yes;\<No' )=1)
  IF lcScreMode = 'E'
    *--Edit mode.
    ldCustDate  = SHPDATES.dCustDate
    ldTrckDate  = SHPDATES.dTrckDate
    ldWareDate  = SHPDATES.dWareDate
    lcScreMode  = 'V'
  ELSE
    *--Add mode.
    =lfClear()
    lcScreMode = 'S'
  ENDIF   
  SHOW GETS
  *--Return it to .F. if it is .T. in case of losing changes.
  llChanges = .F.
ENDIF
*B101706 (Begin) If the user cancels get the default for ship no.
*-- Get the ship via for current ship no.
lnPos = ASCAN(laShipVia,SHPDATES.SHIPVIA)
IF lnPos >0
  *--Get the ship via row to be refreshed with SHOW GETS.
  lnShipVia  = ASUBSCRIPT(laShipVia,lnPos,1)
ENDIF
*B101706 (End)


*!*************************************************************
*! Name      : lfvDates
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/08/1999
*! Purpose   : To see if the user made any changes.
*!*************************************************************
*! PARAMETERS :
*!            : lcFieldVal : Holds the dates fields values. 
****************************************************************************
*! Example     : =lfvDates()
*!*************************************************************
FUNCTION lfvDates
PARAMETERS lcFieldVal

llChanges = IIF(!llChanges,(ldOldVal <> lcFieldval),llChanges)

*!*************************************************************
*! Name      : lfClear
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/08/1999
*! Purpose   : To clear variables.
*!*************************************************************
*! Example     : =lfClear()
*!*************************************************************
FUNCTION lfClear

*--Clear fields.
*B101706 (Begin) Clear ship advise also.
*STORE ' ' TO lcShipNo,lcStatus,lcAirWayB
STORE ' ' TO lcShipNo,lcStatus,lcAirWayB,lcShipAdv
*B101706 (End)
STORE 0   TO lnCartons
STORE {}  TO ldEta,ldEntered,ldCustDate,ldTrckDate,ldWareDate,ldOldVal


*!*************************************************************
*! Name      : lfShow
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/08/1999
*! Purpose   : Show screen's onjects.
*!*************************************************************
*! Example     : =lfShow()
*!*************************************************************
FUNCTION lfShow

*--Ship No. Field will be Enable in select mode only.
lcShipNoS = IIF(lcScreMode = 'S','ENABLE','DISABLE')
*--Customs,Trucker and Warehouse dates will be Enable in Add and Edit Modes only.
lcDatesS = IIF(lcScreMode $ 'AE','ENABLE','DISABLE')
*--Edit and Select and delete buttons will be Enable in View mode.
lcEditSelS =  IIF(lcScreMode = 'V','ENABLE','DISABLE')
*--Save button will be Enable in Add and Edit modes.
lcSaveS =  IIF(lcScreMode $ 'AE','ENABLE','DISABLE')
*--CLose button will always be Enable but its prompt will chnages.
lcClsPrmpt = IIF(lcScreMode $ 'AE','\<Cancel','\<Close')

SHOW GET lcShipNo   &lcShipNoS
SHOW GET ibShipNo   &lcShipNoS
SHOW GET ldCustDate &lcDatesS
SHOW GET ldTrckDate &lcDatesS
SHOW GET ldWareDate &lcDatesS
SHOW GET pbEdit     &lcEditSelS
SHOW GET pbDelete   &lcEditSelS
SHOW GET pbSelect   &lcEditSelS
SHOW GET pbSave     &lcSaveS
SHOW GET pbClose , 1 PROMPT lcClsPrmpt ENABLE
DO CASE
  CASE lcScreMode = 'S'
    _CUROBJ = OBJNUM(lcShipNo)
  CASE lcScreMode = 'E'
    _CUROBJ = OBJNUM(ldCustDate)
  CASE lcScreMode = 'V'
    _CUROBJ = OBJNUM(pbEdit)  
ENDCASE  
*B101706 (Begin) Enable when appending and editing otherwise Disable.
*--Just reinitialized the popup in case of select  or append modes.
IF lcScreMode $ 'SA'
  =gfwCodePop(@laCodes,'SHIPVIA','N')
ENDIF  
SHOW GET lnShipVia  &lcDatesS
SHOW GET lcShipAdv  &lcDatesS
*B101706 (End)


*!*************************************************************
*! Name      : lfvActBrow
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/08/1999
*! Purpose   : To Validate the invisible button to browse the styles.
*!*************************************************************
*! PARAMETERS :
*!            : lcFieldVal : Holds the the onject name. 
****************************************************************************
*! Example     : =lfvActBrow()
*!*************************************************************
FUNCTION lfvActBrow
PARAMETERS lcObjName

llBrowse = .T.
_CUROBJ = OBJNUM(&lcObjName)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfOnEsc
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/08/1999
*! Purpose   : To Trap the ESC button.
*!*************************************************************
*! Example     : =lfOnEsc()
*!*************************************************************
FUNCTION lfOnEsc

_CUROBJ=OBJNUM(pbClose)
KEYBOARD "{SPACEBAR}"

*!*************************************************************
*! Name      : lfvShipVia
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/07/1999
*! Purpose   : Validate shipvia POPUP.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvShipVia()
*! Refer to : (B101706)
*!*************************************************************
FUNCTION lfvShipVia

*--Has the user changed the shipvia?
llChanges = (SHPDATES.SHIPVIA <> ALLTRIM(laShipVia[lnShipVia,2]))

*--End of lfvShipVia()