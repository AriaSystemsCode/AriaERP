******************************************************************
* PROG : ROB4200.PRG FOR (Robyn Meredith)
* DATE : 08/17/98
* DESC : Assebler's declaration DB and FORM.
* AUTH : Adel Mohammed El Gazzar (ADEL)
* Refer to (101260)
****************************************************************************


*----- Variables info and intialization
*-- llBrowse   && Flag to browse the styles having decalrations of value.
*-- laSource   && Array var. to have all style POs. 
*-- laTarget   && Array var. to have all selected POs. 
*-- laData[1]  && Variable to hold the style no.
*-- laData[2]  && Variable to hold the invoice no.
*-- laData[3]  && Variable to hold the style description.
*-- laData[4]  && Variable to hold the unit price for the selected POs.
*-- laData[6]  && Variable to hold total qty for a po.
*-- lcScrMode  && Variable to hold the screen mode.
*-- lcScrFlds  && Variable to hold some names of fields.
*-- llGetPo    && Flag for not filling the source and target arrays for the same invoice more than once.
*-- lnRecCnt   && Variable to hold the records count in the source array.
*-- lcOldVal   && Variable to hold the oldvalue of the fields.
*-- lcOldUnit  && Variable to hold the old unit price.
*-- llChange   && Flag to know if the use made any changes.

EXTERNAL ARRAY laData 
STORE 0 TO lnRecCnt,lcOldUnit
DECLARE laSource[1],laTarget[1]
DIMENSION laData[7]
DIMENSION laHdFlt[1],laVrFlt[1],laFxFlt[1]

laDefProc = .T.
laScrMode[1] = .T.
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
*laCtrStat[6]  = "ENABLE"                && Print Button
IF !gfSetup()
  RETURN
ENDIF
DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
lcMjrPct  = gfItemMask('PM')
lnStyleWid = LEN(lcMjrPct)


IF !WEXIST(gcBaseWind)
  lcScFields = 'Style , cInvoice , Desc , nUnitVal , Cartons , nTotQty , InvDate' 

  SELECT AssmDech
  SET ORDER TO AssmDech
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK               

ENDIF
llBrowse = .F.
STORE ' ' TO laSource,laTarget,lcOldVal
llGetPo  = .T.
llChange = .F.
ON KEY LABEL ESC DO lfOnEsc
IF !USED('STYLE_A')
  USE gcDataDir+'STYLE' AGAIN ALIAS STYLE_A ORDER TAG cStyle IN 0
ENDIF  

*--Initialize fields variables
SELECT AssmDech
*SCATTER FIELDS &lcScrFlds MEMVAR BLANK
DO (gcScrDir+gcWinAppl+"\ICASDCL.SPX")
IF USED('STYLE_A')
  USE IN STYLE_A
ENDIF  


*********************************************************************************
*! Name      : lfvStyle
*! DATE      : 08/10/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate style code field.
*!*************************************************************
FUNCTION lfvStyle

*-- Go to the invoice field if laData[1] field was empty and ibSyle was not pressed.
IF !llBrowse AND EMPTY(laData[1])
  RETURN
ENDIF
SELECT Style 
SET RELATION TO
SELECT DECVALST
SET RELATION TO
*--Set a relation between the STYLE (parent) file and DECVALST (child) file
*--to browse styles having declarations of value (!EOF(DECVALST))
SELECT STYLE
SET RELATION TO cStyMajor INTO DECVALST
*--Set a relation between the DECVALST (parent) file and STYLE_A (child) file
*--to get the desc. for styles having declarations of value (!EOF(DECVALST))
SELECT DECVALST
SET RELATION TO cStyMajor INTO STYLE_A


*--  The steps will be as follows:
*--  1) Check if the style exists in the style file.
*--  2) Check if the style has a declaration of value.
*--  3) Check if the style has any assembler form.
*--  4) Check if the the existing of the invoice if laData[1] and laData[2] are !empty.

*--  1) Check if the style exists in the style file.
IF llBrowse OR ("?" $ laData[1]) OR (!SEEK(SUBSTR(laData[1],1,lnStyleWid),'STYLE'))
  *-- Browse styles having declarations of value.
  laData[1]=lfBrowStyl('STYLE',.F.)
  llBrowse = .F.
  *-- If no style was selected.
  IF EMPTY(laData[1])
    laData[1] = lcOldVal
    _CUROBJ = OBJNUM(laData[1])
    RETURN
  ENDIF
ENDIF

*--  Here laData[1] not empty and found in the style file.
*--  2) Check if the style has a declaration of value.
SET ORDER TO DECSTYLE IN DECVALST
IF !SEEK(SUBSTR(laData[1],1,lnstylewid),'DECVALST')
  ** Message : "The Style "+Style+"does not exist in the delaration of value file."
  **           '\<Browse;\<Reenter'
  lnChoice=gfModalGen("QRM42212B42014","ALERT","The style "+ALLTRIM(SUBSTR(laData[1],1,lnStyleWid))+"|"+" exist in the declaration of value file.")
  *--Browse
  IF lnChoice = 1
    laData[1]=lfBrowStyl('STYLE',.F.)
  ENDIF
  *-- If no style was selected.
  IF EMPTY(laData[1]) OR lnChoice = 2
    laData[1] = lcOldVal
    _CUROBJ = OBJNUM(laData[1])
    RETURN
  ENDIF
ENDIF

*--  Here the Style has a declaration of value.
*--  3) Check if the style has any assembler form.
DO WHILE .T.
  IF !SEEK(SUBSTR(laData[1],1,lnstylewid),'AssmDech')
    ** Message : "The Style"+style+"does not have any assembler form"
    **  '\<Add;\<Browse;\<Reenter')
    lnChoice=gfModalGen("QRM42212B42003","ALERT","The style "+ALLTRIM(SUBSTR(laData[1],1,lnStyleWid))+"|"+" have any assembler form.")
    DO CASE
      CASE lnChoice = 3
        *--Reenter
        _CUROBJ = OBJNUM(laData[1])
        RETURN
      CASE lnChoice = 1
        *--Add
        laData[3] = STYLE_A.DESC
        laData[7] = DATE()
        laScrMode = .F. 
        laScrMode[4] = .T.
        SHOW GETS
        RETURN
      OTHERWISE
        *--Browse
        laData[1] = lfBrowStyl('STYLE',.F.)
        *-- If no style was selected.        
        IF EMPTY(laData[1])
          _CUROBJ = OBJNUM(laData[1])
          RETURN
        ENDIF
        *-- See whether the selected style has a decalration form, get the message again.
        LOOP
     ENDCASE
  ENDIF
  *-- The style has a declaration form.
  EXIT
ENDDO 
*--  Here the Style exists in the assemler form file or will be added.
*--  4) Check the existing of the invoice if laData[1] and laData[2] are not empty.
IF !EMPTY(laData[1]) AND !EMPTY(laData[2])
  *---- Check the existing of the invoice.
  RETURN lfCheckInv()
ENDIF  
*********************************************************************************
*! Name      : lfvActBrow
*! DATE      : 08/17/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To Validate the invisible button to browse the styles.
*!*************************************************************
FUNCTION lfvActBrow
PARAMETERS lcObjName

llBrowse = .T.
_CUROBJ = OBJNUM(&lcObjName)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfvInvoice
*! DATE      : 08/17/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate invoice button.
*!*************************************************************
FUNCTION lfvInvoice

*--Not to check the invoice in case of adding a new style.
IF laScrMode[4] 
  RETURN
ENDIF  

IF !EMPTY(laData[1]) AND !EMPTY(laData[2])
  *-- Check the existing of the invoice.
  IF !lfCheckInv()
    laData[2] = lcOldVal
    _CUROBJ = OBJNUM(laData[2])
  ENDIF
ENDIF  

*!****************************************************************
*! Name      : lfvFields
*! DATE      : 08/17/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To know if the user changes something.
*!****************************************************************
FUNCTION lfvFields
PARAMETERS lnFldNo

IF (lnFldNo = 2 AND laData[5] <0) OR (lnFldNo = 3 AND laData[6] < 0)
  lcField  = IIF(lnFldNo=2,'Cartons No. ','Quantity ')
  ** Message : "Negative values are not allowed."
  =gfModalGen('INM42000B42000','DIALOG' )
  IF lnFldNo = 2
    _CUROBJ = OBJNUM(laData[5])  
  ELSE
    _CUROBJ = OBJNUM(laData[6])  
  ENDIF
  RETURN
ENDIF
IF laScrMode[3] AND !llChange
  llChange = (lcOldVal<>EVAL(VARREAD()))
ENDIF  

*!*************************************************************
*! Name      : lfvSelPos
*! DATE      : 08/17/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate selceted POs button.
*!*************************************************************
FUNCTION lfvSelPos

PARAMETERS llMover
*--Zero out the Unit price var. as we calculate it each time we select POs.
laData[4] = 0
*-- Fill both arrays if we press this button for the first time for this invoice.
IF llGetPo
  llGetPo = .F.
  DECLARE laSource[1],laTarget[1]
  STORE ' ' TO laSource,laTarget
  *-- Fill the source array in  modes ADD ,View and Edit .
  SELECT DISTINCT PO        									  ;
    FROM DECVALST  												  ;
   WHERE DECVALST.cSTYMajor+DECVALST.PO= substr(laData[1],1,12)   ;
    INTO ARRAY laSource
  lnRecCnt = _TALLY 
  IF laScrMode[2] OR laScrMode[3] 
    *-- Fill the TARGET array in View AND EDIT modes only.
    SELECT PO 														   ;
     FROM AssmDecl 													   ;
    WHERE AssmDecl.STYLE+AssmDecl.cINVOICE+AssmDecl.PO=laData[1]+laData[2];
     INTO ARRAY laTarget
  ENDIF
ENDIF

*-- Not to browse the mover if EDIT mode and no changes made for the POs.
IF !llMover
  RETURN
ENDIF  

*--Call the mover if the style has POs and let the user move and remove POs only
*--in the ADD and Edit modes only.
IF lnRecCnt <>0
  = gfMover(@laSource,@laTarget,"Selected POs",IIF(laScrMode[2],.F.,.T.))
ELSE
  ** Message "No POs having declarations of values found for this style."
  =gfModalGen("INM42041B42000","ALERT","POS having declarations of values "+"|"+" this style.")
  _CUROBJ = OBJNUM(pbSelPos)
  RETURN
ENDIF
*--Calculate the Unit price for the selected POs.
SELECT DECVALCM

FOR laTargetN = 1 to ALEN(laTarget,1)
  IF SEEK(laTarget(laTargetN))
    SUM REST Dutiable TO lnUnitVal WHILE Po+cStyMajor = laTarget(laTargetN)
    laData[4] = laData[4]+lnUnitVal
  ENDIF 
ENDFOR
*--Refresh the laData[4] field.
=lfRefresh()



****
FUNCTION lpShow
DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]  && Select mode. 
    lnAlias=SELECT()
    SELECT AssmDech
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK
    SHOW GET laData[2] ENABLE
    lcSelInvP = 'Selected P\<Os'
    SHOW GET pbSelPos , 1 PROMPT lcSelInvP DISABLE
  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]  && View mode.
    SCATTER FIELDS &lcScFields MEMO TO laData 
    lcSelInvP = 'Selected P\<Os'
    SHOW GET pbSelPos , 1 PROMPT lcSelInvP ENABLE
    
  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]  && Edit mode. 
    SHOW GET laData[2] DISABLE
    lcSelInvP = 'Select P\<Os'
    SHOW GET pbSelPos , 1 PROMPT lcSelInvP ENABLE
  ***--- A D D   M O D E ---***
  CASE laScrMode[4]  && Add mode. 
    SHOW GET laData[2] ENABLE
    lcSelInvP = 'Select P\<Os'
    SHOW GET pbSelPos , 1 PROMPT lcSelInvP ENABLE
ENDCASE


*********************************************************************************
*! Name      : lpDelScr
*! DATE      : 08/10/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate Delete button.
*!*************************************************************
PROCEDURE lpDelScr

PRIVATE lcAlias
lcAlias = ALIAS()
SELECT AssmDech
=SEEK(laData[1]+laData[2])
DELETE
IF SEEK(laData[1]+laData[2],'AssmDecl')
  SELECT AssmDecl
  DELETE WHILE STYLE+cINVOICE+PO = laData[1]+laData[2]
ENDIF  
SCATTER FIELDS &lcScFields MEMVAR BLANK
laScrMode = .F.
laScrMode[1] = .T.
SHOW GETS
SELECT (lcAlias)  

*********************************************************************************
*! Name      : lfvBrowse
*! DATE      : 08/10/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate Browse button.
*!*************************************************************
FUNCTION lfvBrowse

SELECT AssmDech
lcOldVal=lfBrowStyl('AssmDech',.T.)
IF EMPTY(lcOldVal)
  RETURN
ENDIF
laData[1] = lcOldVal
*--Get header info.
=lfGeStyHdr()
laScrMode = .F.
laScrMode[2] =.T.
SHOW GETS
*-- Turn the llGetPo to .T. to get the new style's POs wen pressing 'Select POs' button.
llGetPo = .T.


*********************************************************************************
*! Name      : lfvSave
*! DATE      : 08/10/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate Save button.
*!*************************************************************
PROCEDURE lpSavscr
IF EMPTY(laData[2])
  **Message "Invoice no. must be entered!"
  =gfModalGen("INM42080B42000","ALERT","Invoice no. must be entered!")
   _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF
IF EMPTY(laTarget)
  ** Message "No POs selected for the invoice"+InvNo
  =gfModalGen("INM42080B42000","ALERT","No POs selected for the invoice  "+laData[2])
  _CUROBJ = OBJNUM(pbSelPos)
  RETURN
ENDIF  
SELECT AssmDech
IF laScrMode[4] 
  APPEND BLANK
ENDIF
REPLACE Style    WITH laData[1] ,;
        cInvoice WITH laData[2] ,;
        Desc     WITH laData[3] ,;
        nUnitVal WITH laData[4] ,;
        Cartons  WITH laData[5] ,;
        nTotQty  WITH laData[6] ,;
        InvDate  WITH laData[7]
SELECT AssmDecl
*--If the mode is edit,delete all POs and save the new ones.
IF laScrMode[3] AND SEEK(laData[1]+laData[2])
   DELETE REST FOR Style+cInvoice+Po = laData[1]+laData[2]
ENDIF
FOR laTargetN = 1 to ALEN(laTarget,1)
  IF !SEEK(laData[1]+laData[2]+laTarget(laTargetN))
    APPEND BLANK
  ENDIF
  REPLACE Style    WITH laData[1]    ,;
          cInvoice WITH laData[2]  ,;
          PO       WITH laTarget(laTargetN)
ENDFOR
*-- Turn the flag to fill the arrays for the forthcoming style.
llGetPo = .T.  
*--Save the style no. in a temp var. to get it back after being in SELECT mode.
lcOldSty = laData[1]
 
SCATTER FIELDS &lcScFields MEMVAR BLANK
laData[1] = lcOldSty
STORE ' ' TO laSource,laTarget
laScrMode = .F.
laScrMode[1] = .T.
SHOW GETS
*--Turn the change flag to .f.
llChange = .F.


*!*************************************************************
*! Name      : lfGeStyHdr
*! DATE      : 08/17/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To get the style header info
*!*************************************************************
FUNCTION lfGeStyHdr

SELECT AssmDech
SCATTER FIELDS &lcScFields MEMVAR

*!*************************************************************
*! Name      : lfBrowStyl
*! DATE      : 08/17/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To browse the styles having declarations of values.
*!*************************************************************
*! PARAMETERS:
*!            lcAlias : Holds the alias from which we will browse fields.
*!            llBrAll : Check if we will browse all fields or only some. 
*!****************************************************************
FUNCTION lfBrowStyl
PARAMETERS lcAlias,llBrAll

PRIVATE lcOldAlias,lcBrowTtl,lcBrowCon,lcBrFields,lcRetVal
lcOldAlias = ALIAS()
*--Put the old style in a new var. lcStyle as laData[1] maskes trouble in ARIABROW()
lcStyle    = laData[1]
*---Vriables needed for ARIABROW()
*--lcBrowCon   && The 'FOR condition' VAR. for the BROWSE statement.
*--lcBrowTtl   && The browse title VAR. .
*--lcBrFields  && The VAR. holds fields nsmes.
IF UPPER(lcAlias)="STYLE"
  lcBrowCon  = "FOR !EOF('DECVALST')"
  lcBrowTtl  = "Styles having POs with declarations of value"
  lcBrFields = "cStyMajor  :H= 'Style'      ,"+;
                "desc      :H= 'Style desc.',"+;
                "Pattern   :H= 'Pattern'    ,"+;
                "cStyGroup :H= 'Group' ,"+;
                "Scale     :H= 'Scale' ,  "+;
                "cDivision :H= 'Division',"+;
                "Season    :H= 'Season'      "
ELSE
  lcBrowCon   = IIF(llBrAll,"","FOR Style = lcStyle")
  lcBrowTtl   = IIF(llBrAll,"All styles with assembler'S declarations ","Assembler'S declarations")    && Varible to hold the browse title
  IF llBrAll
    lcBrFields  = "Style     :H= 'Style#' , "+;
                  "desc      :H= 'Style desc.',"+;    
                  "cInvoice  :H= 'Invoice#' , "+;
                  "nUnitVal  :H= 'Unit price' , "+;
                  "Cartons   :H= 'Cartons' , "+;
                  "nTotQty   :H= 'Quantity' , "+;
                  "InvDate   :H= 'Invoice date'"

  ELSE
    lcBrFields  = "cInvoice  :H= 'Invoice#' , "+;
                  "nUnitVal  :H= 'Unit price' , "+;
                  "Cartons   :H= 'Cartons' , "+;
                  "nTotQty   :H= 'Quantity' , "+;
                  "InvDate   :H= 'Invoice date'"
                  
  ENDIF                
ENDIF
DECLARE laReturn[1] 
STORE '' TO laReturn
lcRetVal = IIF(UPPER(lcAlias)='STYLE','CSTYMAJOR','cInvoice')
SELECT (lcAlias)
*--To speed the browse
IF UPPER(lcAlias) = "STYLE" 
  GO TOP
ENDIF 

*=AriaBrow(lcBrowCon, lcBrowTtl, gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,'','',lcRetVal,'laReturn')
*  =gfBrows("FOR cStyType = IIF(lcRpForm = 'A' ,'', lcRpForm)",'PO','laTemp')
lcFile_Ttl = lcBrowTtl
=gfBrows(lcBrowCon,lcRetVal,'laReturn') 
SELECT (lcOldAlias)
RETURN laReturn[1]


*!*************************************************************
*! Name      : lfCheckInv
*! DATE      : 08/17/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To check the existing f the invoive for the style in ASSMDECH.
*!*************************************************************
FUNCTION lfCheckInv

PRIVATE lcAlias,lnChoice
lcAlias = ALIAS()
SELECT AssmDech

IF SEEK(laData[1]+laData[2])
  *--Get header info.
  =lfGeStyHdr()
  laScrMode = .F.
  laScrMode[2] = .T.
  SHOW GETS
  
  =lfvSelPOs(.F.)
ELSE
  ** Message : "Invoice "+Invoice#+" does not exist for this style."
  **            \<Add;\<Browse;\<Reenter
  lnChoice=IIF("?" $ laData[2],2,gfModalGen("QRM42212B42003","ALERT","Invoice "+ALLTRIM(laData[2])+"|"+" exist for this style.")) 
  DO CASE
    CASE lnChoice = 1
      *-- Add
      laData[3] = STYLE_A.DESC
      laData[7] = DATE()
      laScrMode = .F.
      laScrMode[4] = .T.
      SHOW GETS
      SHOW GET laData[2] DISABLE
    CASE lnChoice = 3
      *-- Reenter
      _CUROBJ = OBJNUM(laData[2])
    OTHERWISE
      *--Browse
      *--Browse if the style has invoices.
      IF SEEK(SUBSTR(laData[1],1,lnStyleWid))
        laData[2]=lfBrowStyl('AssmDech',.F.)
      ELSE
        ** Message : No invoices found for the style '+Invoice#
        =gfModalGen("INM42041B42000","ALERT","invoices found for the style "+"|"+ALLTRIM(laData[1]))
      ENDIF
      *--If this style does not have any invoices or no invoice was selected.
      IF EMPTY(laData[2]) OR EOF('AssmDech')
        _CUROBJ = OBJNUM(laData[2])
        RETURN 
      ENDIF
      *---Get header info.
      =lfGeStyHdr()
      laScrMode = .F.
      laScrMode[2] = .T.
      SHOW GETS
  ENDCASE  
ENDIF 
*!*************************************************************
*! Name      : lfvPrint
*! DATE      : 08/17/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate the Print button.
*!*************************************************************
FUNCTION lfvPrint

*--Hide the screen
HIDE WINDOW ROB4200
*--Define a new window to get the grid in.
DEFINE WINDOW lcGridWin FROM 00,00 SIZE 25,80 
MOVE WINDOW lcGridWin CENTER
ACTIVATE WINDOW lcGridWin
ON KEY LABEL ESC 
lnCurAli = SELECT(0)
*--Call the print progarm
DO ROB4250
ON KEY LABEL ESC DO lfOnEsc
RELEASE WINDOW lcGridWin
CLEAR
*--Recall the screen.
ACTIVATE WINDOW ROB4200
IF lcScrMode = 'S'
  _CUROBJ = OBJNUM(laData[1])
ENDIF
SELECT (lnCurAli)

*!*************************************************************
*! Name      : lfOnEsc
*! DATE      : 08/17/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To Trap the ESC button.
*!*************************************************************
FUNCTION lfOnEsc

KEYBOARD "{SPACEBAR}"



