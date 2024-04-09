*!*************************************************************************
*! Program file  : getobj.prg
*! Program desc. : program to add OLE objects to the objects file and link
*!                 it to different codes of different files Ex. Cutting Tik.
*! For Screen    : objectS
*!        System : AAS26
*!        Module : AAS26
*!     Developer : Hesham El-Sheltawi 
*!*************************************************************************
*! Calls       :
*!            PROCEDURE : lpTrapTab
*!*************************************************************************
*! Passed parameters  : lcobjectid
*!*************************************************************************
*! Modifications      :
*E300320,1 Reham On 12/26/95
*E300320,1 Change the screen design & layout & enhance the interface.
*E300320,5 Reham On 03/04/96
*E300320,5 1- Fix spelling mistakes in the messages.
*E300320,5 2- Fix the get fields colors.
*E300320,5 3- Make the preview of the pictures ISOMETRIC instead of STRETCH.
*E300320,5 4- Prevent the flecurring if open the zoom screen.
*E300320,5 5- Display the object description in the preview screen.
*E300320,5 6- If changing from linked to all, don't change the record 
*E300320,5    pointer if there is object displayed on the screen.
*E300320,5 7- If calling the OLE screen from the style inquire screen & 
*E300320,5    adding new object & the radio button value was linked,
*E300320,5    link the added object to the current style.
*E300619,1 Reham On 03/27/97
*E300619,1 Give the ability to link this program with the following :
*E300619,1 Customers - Picking Tickets - Materials - Vendors
*E300619,1 Sales Reps. - Return Authorization - Credit Memos
*E300637,1 WAM 06/03/1997 Use the new global variables names
*E301311,1 WAB 08/25/1999 add the following entities to object linking:
*E301311,1 WAB 1. Designs (G)  2. Name drops (N) 3. Material Manufacturing(U)
*E500418,1 AME Add the ability to preview differnt format of images
*C200388,1 ADEL 08/06/02 Avoid this message for Dare as we made a program for him to open .DOC,.XLS
*B608093,1 NNA 05/23/2007  Add to the OBJECTS file the audit trail fields like (CADD_USER,CADD_TIME,DADD_DATE....etc) 
*B608093,1 NNA             and Modify the objects link screen to update audit trail fields in the objects file when adding new object 
*!*************************************************************************
PARAMETERS lcObjType,lcObjectID

*E500418,4 AME [Start]
PUSH KEY
*E500418,4 AME [End]
*--variables definition part
lcOldAlias   = SELECT()          && current work area
llLink       = PARAMETERS() = 2  && to determine whether the program is stand alone or not

*E500418,1 AME [Start]
IF AT("ESPIA_I.FLL", SET("LIBRARY")) = 0
  SET LIBRA TO (gcDef_path+"\ESPIA_I.FLL") ADDITIVE

ENDIF
=imHIDEWIN(imGET("HWND","Espia for Windows"))
*E500418,1 AME [End]

*E300320,1 Reham On 01/09/96    *** Begin ***
*E300320,1 Change the button to be a check box.
*lcLinkPrompt = IIF(llLink,"\<Link","\<Link To...")  && Link push button prompt
*E300320,1 Reham On 01/09/96    *** End   ***

llinsObj     = .F.      && to controll the calling of the foxpro menu options for the general
lcWWindow    = ' Text'  && to define the prompt of the menu bars "Text or Object"

*QDD = 'T:\AASTST26\DATA1\' && data directory 
*QID = 'T:\AASTST26\DATA1\' && images directory
*qwd = 'T:\AASTST26\WORK\' 
*qsd = 'T:\AASTST26\screens\' 

*E300320,1 Reham On 12/31/95   *** Begin ***
*E300320,1 Define the variables hold the OLE bitmaps in the code.

*E300637,1 WAM 06/03/1997 Use the new global variables names
*lcAdLnBmp = (QBD + "OL_ADLNK.BMP")
*lcRmLnBmp = (QBD + "OL_RMLNK.BMP")
*lcDescBmp = (QBD + "DESCRIP.BMP")
*lcOkBmp   = (QBD + "OK.BMP")
*lcCanBmp  = (QBD + "CAN.BMP")
*lcZoomBmp = (QBD + "ZOOM.BMP")
*lcClsBmp  = (QBD + "CLS.BMP")
*lcExtBmp  = (QBD + "EXTKEY.BMP")
*lcNewBmp  = (QBD + "NEW.BMP")
*lcMdObBmp = (QBD + "OL_MDOBJ.BMP")
*lcRpObBmp = (QBD + "OL_RPOBJ.BMP")
*lcNextBmp = (QBD + "NEXT.BMP")
*lcBottBmp = (QBD + "BOTTOM.BMP")
*lcTopBmp  = (QBD + "TOP.BMP")
*lcPrvBmp  = (QBD + "PRIOR.BMP")
*lcFltrBmp = (QBD + "OL_FILTR.BMP")
*lcSelBmp  = (QBD + "SEL.BMP")
*lcDelBmp  = (QBD + "OL_DELET.BMP")
*lcLinkBmp = (QBD + "OL_LINK.BMP")

lcAdLnBmp = (gcBmpHome+ "OL_ADLNK.BMP")
lcRmLnBmp = (gcBmpHome+ "OL_RMLNK.BMP")
lcDescBmp = (gcBmpHome+ "DESCRIP.BMP")
lcOkBmp   = (gcBmpHome+ "OK.BMP")
lcCanBmp  = (gcBmpHome+ "CAN.BMP")
lcZoomBmp = (gcBmpHome+ "ZOOM.BMP")
lcClsBmp  = (gcBmpHome+ "CLS.BMP")
lcExtBmp  = (gcBmpHome+ "EXTKEY.BMP")
lcNewBmp  = (gcBmpHome+ "NEW.BMP")
lcMdObBmp = (gcBmpHome+ "OL_MDOBJ.BMP")
lcRpObBmp = (gcBmpHome+ "OL_RPOBJ.BMP")
lcNextBmp = (gcBmpHome+ "NEXT2.BMP")
lcBottBmp = (gcBmpHome+ "BOTTOM2.BMP")
lcTopBmp  = (gcBmpHome+ "TOP2.BMP")
lcPrvBmp  = (gcBmpHome+ "PRIOR2.BMP")
lcFltrBmp = (gcBmpHome+ "OL_FILTR.BMP")
lcSelBmp  = (gcBmpHome+ "SEL.BMP")
lcDelBmp  = (gcBmpHome+ "OL_DELET.BMP")
lcLinkBmp = (gcBmpHome+ "OL_LINK.BMP")
*E300637,1 WAM 06/03/1997 (End)

*E300320,1 Reham On 12/31/95   *** End   ***

*E300320,1 Reham On 12/31/95   *** Begin ***
*E300320,1 Define the radio button that detrmine the filter criteria.
*E300320,1 And define the check box link.
IF llLink
  rbLink     = 2
  lnOldLink  = 0
  cbLink     = 0
  lnOldCbLnk = 0 
ENDIF
*E300320,1 Reham On 12/31/95   *** End   ***

lcObjStat     = 'DISABLE' && hold the status of all the controll push button except close
lcCodeStat    = 'ENABLE'  && hold the status of the code field
lcObjCode     = ''        && Object ID variable
lcNotes       = ''
lcDescription = ''

lcObjToAct    = ''        && variable hold the short cut in the menu for the genenal field functions

*--IF the program is called as stand alone
IF !llLink
  *E300619,1 Reham On 03/27/97  ** Begin **
  *E300619,1 Increase the array dimension to link new objects.
  *DIMENSION laObjLinkF[6,6]  && Array hold the files that can link to the objects
  
  *E301311,1 WAB - increase array length by 3 TO add 3 Entities 
  *E301311,1 WAB - START
  *DIMENSION laObjLinkF[13,6]  && Array hold the files that can link to the objects
  
  DIMENSION laObjLinkF[16,6]  && Array hold the files that can link to the objects
  *E301311,1 WAB - END
  
  *E300619,1 Reham On 03/27/97  ** End   **
  *-- column 1 hold the prompt of the popup
  *-- column 2 hold the file name
  *-- column 3 hold the Tag name
  *-- column 4 hold the browse function name
  *-- column 5 hold the Link type "Ex. 'S' for style"
  *-- column 6 hold the parameters for the browse function.

  laObjLinkF[1,1] = "Cutting ticket"
  laObjLinkF[1,2] = "CUTTKTH"
  laObjLinkF[1,3] = "CUTTKTH"
  laObjLinkF[1,4] = "CUTBROW"
  laObjLinkF[1,5] = "C"
  laObjLinkF[1,6] = [{VALUE}]

  laObjLinkF[2,1] = "Sales Order"
  laObjLinkF[2,2] = "ORDHDR"
  laObjLinkF[2,3] = "ORDHDR"
  laObjLinkF[2,4] = "ORDBROWO"
  laObjLinkF[2,5] = "O"
  laObjLinkF[2,6] = [{VALUE}]    

  laObjLinkF[3,1] = "Style Purchase Order"
  laObjLinkF[3,2] = "POSHDR"
  laObjLinkF[3,3] = "POSHDR"
  laObjLinkF[3,4] = "POSBROW"
  laObjLinkF[3,5] = "P"
  laObjLinkF[3,6] = [{VALUE},'']
 
  laObjLinkF[4,1] = "Material Purchase Order"
  laObjLinkF[4,2] = "POFHDR"
  laObjLinkF[4,3] = "POFHDR"
  laObjLinkF[4,4] = "POFBROW"
  laObjLinkF[4,5] = "M"
  laObjLinkF[4,6] = [{VALUE},'']

  laObjLinkF[5,1] = "Invoice"
  laObjLinkF[5,2] = "INVHDR"
  laObjLinkF[5,3] = "INVHDR"
  laObjLinkF[5,4] = "gfInvBrow"
  laObjLinkF[5,5] = "I"
  laObjLinkF[5,6] = [{VALUE}]
  
  laObjLinkF[6,1] = "Style"
  laObjLinkF[6,2] = "STYLE"
  laObjLinkF[6,3] = "CSTYLE"
  laObjLinkF[6,4] = "STYBROW"
  laObjLinkF[6,5] = "S"
  laObjLinkF[6,6] = [{VALUE},'*','*']
  
  *E300619,1 Reham On 03/27/97  ** Begin **
  *E300619,1 Add new row for linking Customers.
  laObjLinkF[7,1] = "Customers"
  laObjLinkF[7,2] = "CUSTOMER"
  laObjLinkF[7,3] = "CUSTOMER"
  laObjLinkF[7,4] = "CUSBROWM"
  laObjLinkF[7,5] = "A"
  laObjLinkF[7,6] = [{VALUE}]
  
  *E300619,1 Add new row for linking Picking Tickets.
  laObjLinkF[8,1] = "Picking Tickets"
  laObjLinkF[8,2] = "PIKTKT"
  laObjLinkF[8,3] = "PIKTKT"
  laObjLinkF[8,4] = "gfPkTkBrow"
  laObjLinkF[8,5] = "T"
  laObjLinkF[8,6] = [{VALUE}]
  
  *E300619,1 Add new row for linking Materials.
  laObjLinkF[9,1] = "Materials"
  laObjLinkF[9,2] = "FABRIC"
  laObjLinkF[9,3] = "FABRIC"
  laObjLinkF[9,4] = "FABROW"
  laObjLinkF[9,5] = "F"
  laObjLinkF[9,6] = [{VALUE},'*','*']
  
  *E300619,1 Add new row for linking Vendors.
  laObjLinkF[10,1] = "Vendors"
  laObjLinkF[10,2] = "VENDOR"
  laObjLinkF[10,3] = "VENDOR"
  laObjLinkF[10,4] = "VENBROW"
  laObjLinkF[10,5] = "V"
  laObjLinkF[10,6] = [{VALUE}]
  
  *E300619,1 Add new row for linking Sales Reps.
  laObjLinkF[11,1] = "Sales Rep."
  laObjLinkF[11,2] = "SALESREP"
  laObjLinkF[11,3] = "SALESREP"
  laObjLinkF[11,4] = "REPCHK"
  laObjLinkF[11,5] = "L"
  laObjLinkF[11,6] = [{VALUE}]
  
  *E300619,1 Add new row for linking Return Authorization.
  laObjLinkF[12,1] = "Return Authorization"
  laObjLinkF[12,2] = "RETAUTH"
  laObjLinkF[12,3] = "RETAUTH"
  laObjLinkF[12,4] = "RABROW"
  laObjLinkF[12,5] = "R"
  laObjLinkF[12,6] = [{VALUE}]
  
  *E300619,1 Add new row for linking Credit Memos.
  laObjLinkF[13,1] = "Credit Memos"
  laObjLinkF[13,2] = "RETHDR"
  laObjLinkF[13,3] = "RETHDR"
  laObjLinkF[13,4] = "gfCrdtBrow"
  laObjLinkF[13,5] = "D"
  laObjLinkF[13,6] = [{VALUE}]
  *E300619,1 Reham On 03/27/97  ** End   **

  *E301311,1 WAB - add the following entities to object linking:
  *E301311,1 WAB - 1. Designs (G)  2. Name drops (N) 3. Material Manufacturing(U)  
  *E301311,1 WAB - START
  *--------------------- Design Screen-
  laObjLinkF[14,1] = "Design"
  laObjLinkF[14,2] = "ICDESIGN"
  laObjLinkF[14,3] = "ICDESIGN"
  laObjLinkF[14,4] = ""
  laObjLinkF[14,5] = "G"
  laObjLinkF[14,6] = [{VALUE}]
  *--------------------- Name Drop -
  laObjLinkF[15,1] = "Name Drop"
  laObjLinkF[15,2] = "ICNAMDRP"
  laObjLinkF[15,3] = "ICNAMDRPID"
  laObjLinkF[15,4] = ""
  laObjLinkF[15,5] = "N"
  laObjLinkF[15,6] = [{VALUE}]
  *--------------------- Material Manufacturing Order-
  laObjLinkF[16,1] = "Material Manufacturing Order"
  laObjLinkF[16,2] = "MMFGORDH"
  laObjLinkF[16,3] = "MMFGORDH"
  laObjLinkF[16,4] = ""
  laObjLinkF[16,5] = "U"
  laObjLinkF[16,6] = [{VALUE}]

  *E301311,1 WAB - END

  *-- column 1 hold the prompt of the popup
  *-- column 2 hold the file name
  *-- column 3 hold the Tag name
  *-- column 4 hold the browse function name
  *-- column 5 hold the Link type "Ex. 'S' for style"
  *-- column 6 hold the parameters for the browse function.

ENDIF

*lnRefresh = SET('REFRESH')
SET CONFIRM ON
*--open needed files
IF !USED('OBJLINK')
  *E300637,1 WAM 06/03/1997 Use the new global variables names
  *USE (Qdd+'OBJLINK') IN 0 ORDER TAG COBJLINK
  USE (gcDataDir+'OBJLINK') IN 0 ORDER TAG COBJLINK
  *E300637,1 WAM 06/03/1997 (End)
ELSE
  SELECT OBJLINK
  SET ORDER TO TAG COBJLINK
ENDIF
*E500418,1 AME [Start]
*E500418,4 AME [Start]
*cbPath = IIF(OBJLINK.LLNK,1,0)
*E500418,4 AME [End]
*E500418,1 AME [End]

*E300320,1 Reham On 12/31/95   *** Begin ***
*E300320,1 Set filter to the linked objects to the current style.
IF llLink
  SELECT OBJLINK
  SET FILTER TO
  SET FILTER TO OBJLINK.COBJLINK = lcObjectID
  LOCATE
ENDIF

*E300320,1 This flag to avoid refreshing the navigations buttons in the
*E300320,1 show function if not the first time go in the show procedure.
llFrsShow = .T.
*E300320,1 Reham On 12/31/95   *** End   ***

IF !USED('OBJECTS')
  *E300637,1 WAM 06/03/1997 Use the new global variables names
  *USE (QID+'OBJECTS') IN 0 ORDER TAG objectid
  USE (gcDataDir+'OBJECTS') IN 0 ORDER TAG objectid
  *E300637,1 WAM 06/03/1997 (End)
ELSE
  SELECT OBJECTS
  SET ORDER TO TAG objectid
ENDIF

*--CREATE TEMPRORY GENERAL
lcTempName = 'X'+RIGHT(SYS(3),7)
CREATE CURSOR (lcTempName) (GOBJECT G)
APPEND BLANK
SELECT OBJECTS
IF llLink
  SET FILTER TO SEEK(cObject_ID+lcObjType+PADR(lcObjectID,20),'OBJLINK')
  LOCATE
  IF FOUND()
    lcObjCode     = cObject_ID
    lcNotes       = OBJLINK.Mobj_Notes
    lcDescription = OBJLINK.cObj_Desc
    lcObjStat     = 'ENABLE'
    lcCodeStat    = 'DISABLE'
    *E300320,1 Reham On 12/31/95    *** Begin ***
    *E300320,1 Remark because the push button changed to be check box.
    *lcLinkPrompt  = "\<UnLink"
    cbLink = IIF(SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK'),1,0)
    *E300320,1 Reham On 12/31/95    *** End   ***
  ENDIF
  SET FILTER TO SEEK(cObject_ID+lcObjType+PADR(lcObjectID,20),'OBJLINK') OR cObject_ID=(lcObjCode)  
ENDIF  
*E500418,4 AME [Start]
cbPath = IIF(OBJECTS.LLNK,1,0)
*E500418,4 AME [End]

*--trap the needed keys
ON KEY LABEL ESC LL = .T.
ON KEY LABEL TAB DO lpTrapTab WITH 1
ON KEY LABEL BACKTAB DO lpTrapTab WITH 2
ON KEY LABEL Ctrl+J DO lfActOLEObj

*SET REFRESH TO 1
*--call the objects screen

*E300637,1 WAM 06/03/1997 Use the new global variables names
*DO (qsd+'objectS.SPR')
DO objectS.SPR
*E300637,1 WAM 06/03/1997 (End)

*--clear trapping for the keys
*SET REFRESH TO lnRefresh

*E300320,1 Reham On 12/31/95    *** Begin ***
SELECT OBJLINK
SET FILTER TO
*E300320,1 Reham On 12/31/95    *** End   ***
 
 *E500418,1 AME
 RELEASE LIBRARY (gcDef_path+"\ESPIA_I.FLL")
 *E500418,1 AME
SELECT OBJECTS
SET FILTER TO

ON KEY LABEL ESC
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL Ctrl+J
IF USED('OBJLINK')
  USE IN OBJLINK
ENDIF

IF USED('OBJECTS')
  USE IN OBJECTS
ENDIF

USE IN (lcTempName)
SELECT (lcOldAlias)
*E500418,4 AME [Start]
POP KEY 
*E500418,4 AME [End]
*!*************************************************************************
*! Name      : lfDispPict
*! Developer : Hesham El-Sheltawi
*! Date      : 09/06/95    
*! Purpose   : To display the objects in the window
*!*************************************************************************
*! Returns   : None 
*!*************************************************************************     
*
FUNCTION lfDispPict


IF lcObjStat = 'DISABLE'
  MODIFY GENERAL &lcTempName..GOBJECT NOWAIT WINDOW OBJDISP ;
  IN WINDOW OBJCNTNT NOMODIFY
ELSE
  MODIFY GENERAL GOBJECT NOWAIT WINDOW OBJDISP ;
  IN WINDOW OBJCNTNT
ENDIF  
*E500418,4 AME [Start]
IF EMPTY(IIF(lcObjStat = 'DISABLE',&lcTempName..GOBJECT,GOBJECT))
  RETURN .F.
ELSE
  RETURN .T.
ENDIF
*E500418,4 AME [End]

*!*************************************************************************
*! Name      : lfAddPad
*! Developer : Hesham El-Sheltawi
*! Date      : 09/06/95    
*! Purpose   : adding new pad to the menu "Picture" and define it's bars
*!*************************************************************************
*! Calls     :
*!            FUNCTION : lfDefineBars
*!            FUNCTION : lfDispPict
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfAddPad

DEFINE PAD _picture OF _msysmenu PROMPT "P\<icture"
DEFINE POPUP _pctpop

=lfDefineBars()
DEFINE BAR _MED_PSTLK OF  _PctPop PROMPT "  Paste \<Special..."

*E300320,1 Reham On 12/31/95    *** Begin ***
*E300320,1 Change the bar name to be replace instead of insert.
*DEFINE BAR _MED_INSOB OF _PctPop PROMPT  "  \<Insert object..." KEY Ctrl+Z,"Ctrl+Z   "
DEFINE BAR _MED_INSOB OF _PctPop PROMPT  "  \<Replace object..." KEY Ctrl+Z,"Ctrl+Z   "
*E300320,1 Reham On 12/31/95    *** End   ***

DEFINE BAR _MED_OBJ OF  _PctPop PROMPT   "  \<Object...       " KEY Ctrl+O,"Ctrl+O   "
*DEFINE BAR 1 OF _PCTPOP PROMPT          "  Activate Object   " KEY Ctrl+J,"Ctrl+J   " SKIP FOR lcObjStat='DISABLE'

ON PAD _picture OF _msysmenu ACTIVATE POPUP _pctpop
*ON SELECTION BAR 1 OF _PCTPOP ACTIVATE WINDOW ("OBJECTS.GOBJECT")
*E500418,4 AME [Start]
IF Objects.llnk
  =lfdraw()
ELSE
*E500418,4 AME [End]
  =lfDispPict()
  ACTIVATE WINDOW ("OBJECTCNT")
*E500418,4 AME [Start]
ENDIF
*E500418,4 AME [End]
_CUROBJ = OBJNUM(lcObjCode)

*!*************************************************************************
*! Name      : lfShowObj
*! Developer : Hesham El-Sheltawi
*! Date      : 09/11/95    
*! Purpose   : SHOW function to controll the enable
*!             and disable of the object push buttons and to change the 
*!             prompt of the Link push button if needed
*!*************************************************************************
*! Calls     :
*!            FUNCTION : lfDefineBars
*!*************************************************************************      
*! Passed parameters  : llreturn
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfShowObj

*E300320,1 Reham On 12/31/95    *** Begin ***
*E300320,1 Change the push button link to be check box.
*lcLinkProm = IIF(SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK'),'\<UnLink','\<Link')
*SHOW GET pbLink,1 PROMPT (lcLinkProm)
IF llLink
  *E300320,1 Reham On 02/11/96    *** Begin ***
  IF !EMPTY(lcObjCode)
    cbLink = IIF(SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK'),1,0)
    SHOW GET cbLink        ENABLE
  ELSE
    cbLink = 0
    SHOW GET cbLink        DISABLE
    SHOW GET lcNotes       DISABLE
    SHOW GET lcDescription DISABLE
  ENDIF
  *E300320,1 Reham On 02/11/96    *** End   ***
ENDIF
*E300320,1 Reham On 12/31/95    *** End   ***

*E300320,1 Reham On 12/31/95    *** Begin ***
*E300320,1 This condition will be excuted first time in the show function
*E300320,1 because it'll be the begining of the object file.
IF llFrsShow
  llFrsShow = .F.
  *E300320,1 Put the conditions in cases instead of the if condition.
  DO CASE
    CASE llLink AND !EMPTY(lcObjCode)
    *IF llLink AND !EMPTY(lcObjCode)
      *E300320,1 Disable the top & previous buttons because I'm on the first object.
      SHOW GET pbTop      DISABLE
      SHOW GET pbPrevious DISABLE
      SHOW GET pbNext     ENABLE
      SHOW GET pbBottom   ENABLE
    CASE !llLink .OR. EMPTY(lcObjCode)
      SHOW GET pbTop      DISABLE
      SHOW GET pbPrevious DISABLE
      SHOW GET pbNext     DISABLE
      SHOW GET pbBottom   DISABLE
  ENDCASE
*E300320,1 Reham On 02/11/96    *** Begin ***
*E300320,1 If not first time, call function to handle the enabling 
*E300320,1 & disabling of the navigations buttons.
ELSE
  =lfCntrPanl()
  *E300320,1 Reham On 02/11/96    *** End   ***
ENDIF
*E300320,1 Reham On 12/31/95    *** End   ***

*!*************************************************************************
*! Name      : lfActivate
*! Developer : Hesham El-Sheltawi
*! Date      : 09/06/95    
*! Purpose   : read activate and deactivate function to controll the enable
*!             and disable of the object push buttons and to change the 
*!             prompt of the menu bars
*!*************************************************************************
*! Calls     :
*!            FUNCTION : lfDefineBars
*!*************************************************************************      
*! Passed parameters  : llreturn
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfActivate
PARAMETERS llReturn


  lcWhichw = IIF(WONTOP() = 'OBJECTS.GOBJECT' , ' Object' , ' Text')
  IF lcWWindow <> lcWhichw
    lcWWindow = lcWhichw
    =lfDefineBars()
  ENDIF

IF EMPTY(GOBJECT) OR lcObjStat = 'DISABLE'
  IF lcObjStat = 'DISABLE'
    SHOW GET pbInsObj DISABLE
  ELSE
    SHOW GET pbInsObj ENABLE      
  ENDIF
  SHOW GET pbModObj DISABLE
  *E500418,1 AME [Start]
  *SHOW GET pbZoomObj DISABLE
  IF WVISIBLE('ZOOMPATH')
    HIDE WINDOW ZOOMPATH
  ENDIF
  *E500418,1 AME [End]   
  IF WVISIBLE('ZOOMOBJ')
    HIDE WINDOW ZOOMOBJ
  ENDIF
ELSE
  SHOW GET pbModObj  ENABLE
  *E500418,1 AME [Start]
  *SHOW GET pbZoomObj ENABLE
  *E500418,1 AME [End]
  SHOW GET pbInsObj  ENABLE
ENDIF
*E500418,1 AME [START]
*E500418,4 AME [START]
IF (CbPath = 1 .AND. !llnk) .OR. (CbPath <> 1 .AND. llnk)
  SHOW GET pbInsObj  DISABLE
ENDIF
*IF SEEK (cObject_ID,'OBJLINK') AND (!(OBJLINK.LLNK) .AND. EMPTY(GOBJECT)) OR ((OBJLINK.LLNK) .AND. EMPTY(CIMGPATH)) OR lcObjStat = 'DISABLE'
IF (!LLNK .AND. EMPTY(GOBJECT)) OR ( LLNK .AND. EMPTY(CIMGPATH)) OR lcObjStat = 'DISABLE'
*E500418,4 AME [END]
  SHOW GET pbZoomObj DISABLE
ELSE
  SHOW GET pbZoomObj ENABLE
ENDIF
SHOW GET cbPath
IF CbPath = 1
  SHOW GET pbModObj DISABLE
ENDIF
*E500418,1 AME [END]

RETURN llReturn

*!*************************************************************************
*! Name      : lfvInsObj
*! Developer : Hesham El-Sheltawi
*! Date      : 09/06/95    
*! Purpose   : valid function for the insert object push button
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvInsObj

llinsObj   = .T.
*lcObjToAct = ['{CTRL+Z}']
*_CUROBJ    = _CUROBJ + 1

_CUROBJ    = OBJNUM(PBNEW)

KEYBOARD "{ENTER}"

*!*************************************************************************
*! Name      : lfwObjIns
*! Developer : Hesham El-Sheltawi
*! Date      : 09/06/95    
*! Purpose   : When function for the insert object push button
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfwObjIns

*E300320,5 Reham On 03/05/96    *** Begin ***
CLEAR TYPEAHEAD
*E300320,5 Reham On 03/05/96    *** End   ***

IF llinsObj
  llinsObj = .F.
  KEYBOARD "{Ctrl+J}"
ENDIF
RETURN .F.

*!*************************************************************************
*! Name      : lfvObject
*! Developer : Hesham El-Sheltawi
*! Date      : 09/06/95    
*! Purpose   : valid function for the modify object push button
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvObject

IF !EMPTY(GOBJECT)
  llinsObj   = .T.
  lcObjToAct = ['{CTRL+O}']
  _CUROBJ    = _CUROBJ - 1
ENDIF

*!*************************************************************************
*! Name      : lfvZoomObj
*! Developer : Hesham El-Sheltawi
*! Date      : 09/07/95    
*! Purpose   : valid function for the Zoom object push button
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvZoomObj

*E300320,5 Reham On 03/04/96    *** Begin ***
*E300320,5 Change the way of calling the zoom screen to prevent
*E300320,5 the flecurring.
*SHOW GETS WINDOW ZOOMOBJ
*SHOW WINDOW ZOOMOBJ
*_CUROBJ = OBJNUM(INVDUM)
*E300320,5 Clear the trapped key befor calling the zoom screen.
PUSH KEY CLEAR
*E300320,5 Call the zoom screen.

*E300637,1 WAM 06/03/1997 Use the new global variables names  
*DO (qsd+'ZOOMOBJ.SPR')
*E500418,1 AME [Start]
IF OBJECTS.LLNK
  DO ZOOMPATH.SPR
  =lfdraw()
ELSE 
*E500418,1 AME [End]
  DO ZOOMOBJ.SPR
*E500418,1 AME [Start]
ENDIF
*E500418,1 AME [End]
*E300637,1 WAM 06/03/1997 (End)

*E300320,5 Pop the trapped key in the buffer again.
POP KEY
*E300320,5 Reham On 03/04/96    *** End   ***

*!*************************************************************************
*! Name      : lfwLinkFlt
*! Developer : Reham Alallamy
*! Date      : 12/31/95
*! Purpose   : When function for the radio button to control the OBJLINK 
*!           : file filter.
*!*************************************************************************
*! Calls     :
*!             FUNCTION : 
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfwLinkFlt

lnOldLink = rbLink

*!*************************************************************************
*! Name      : lfvLinkFlt
*! Developer : Reham Alallamy
*! Date      : 12/31/95
*! Purpose   : valid function for the radio button to control the OBJLINK 
*!           : file filter.
*!*************************************************************************
*! Calls     :
*!             FUNCTION : lfActivate
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvLinkFlt
PRIVATE lcSelect

IF lnOldLink <> rbLink
  *E300320,1 Save the old alias.
  lcSelect = SELECT()
  
  SELECT OBJLINK
  DO CASE
    *E300320,1 If the radio button value is "ALL", clear the  filter.
    CASE rbLink = 1
      SET FILTER TO
      *E300320,5 Reham On 03/04/96    *** Begin ***
      *E300320,5 If changing from linked to all, don't change the record 
      *E300320,5 pointer if there is object displayed on the screen.
      *LOCATE
      *GO TOP
      *E300320,5 Reham On 03/04/96    *** End   ***
      IF EMPTY(lcObjCode)
        lcObjStat = 'DISABLE'
        =lfDispPict()
      ENDIF
      SELECT OBJECTS
      SET FILTER TO
      *E300320,5 Reham On 03/04/96    *** Begin ***
      *E300320,5 If changing from linked to all, don't change the record 
      *E300320,5 pointer if there is object displayed on the screen.
      *LOCATE
      *GO TOP
      *E300320,5 Reham On 03/04/96    *** End   ***
    *E300320,1 If the radio button value is linked, set filter to 
    *E300320,1 the current style.
    CASE rbLink = 2
      SET FILTER TO
      SET FILTER TO OBJLINK.COBJLINK = lcObjectID
      LOCATE
      GO TOP
      SELECT OBJECTS
      SET FILTER TO SEEK(cObject_ID+lcObjType+PADR(lcObjectID,20),'OBJLINK')
      LOCATE
      GO TOP
  ENDCASE
  *E300320,1 Select the old alias again.
  SELECT (lcSelect)
  
  IF !EMPTY(lcObjCode)
    lcObjCode = COBJECT_ID
    IF llLink AND SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
      lcNotes       = OBJLINK.Mobj_Notes
      lcDescription = OBJLINK.cObj_Desc    
      *E300320,1 Reham On 01/02/96    *** Begin ***
      cbLink = IIF(SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK'),1,0)
      *E300320,1 Reham On 01/02/96    *** End   ***
    ELSE
      lcNotes       = Mobj_Notes
      lcDescription = cObj_Desc
    ENDIF  
    SHOW GETS
    =lfActivate()
  ENDIF
ENDIF

*!*************************************************************************
*! Name      : lfvActBrow
*! Developer : Reham Alallamy
*! Date      : 01/08/96
*! Purpose   : valid function for Object Code icon
*!*************************************************************************
*! Calls     :
*!            FUNCTION : lfvActBrow
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvActBrow

lcObjCode = "?"
_CUROBJ   = OBJNUM(lcObjCode)
KEYBOARD CHR(13)

*!*************************************************************************
*! Name      : lfvObjCode
*! Developer : Hesham El-Sheltawi
*! Date      : 09/06/95    
*! Purpose   : valid function for Object Code GetField
*!*************************************************************************
*! Calls     :
*!            FUNCTION : lfActivate
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvObjCode

IF !EMPTY(lcObjCode) AND LASTKEY() = 13
  lcObjCode = PADL(ALLTRIM(lcObjCode),10,'0') 
  SELECT OBJECTS
  IF !SEEK(lcObjCode)
    *E300320,1 Reham On 01/08/96    *** Begin ***
    *E300320,1 Change the button browse to be list.
    *lnOption = gfDialog('?','Object not found in the database.','\!\<Browse;\?\<Reenter')
    *E300320,1 Reham On 02/12/96    *** Begin ***
    *E300320,1 If the object code contain "?" no need for displaying
    *E300320,1 the dialog.
    IF "?" $ lcObjCode
      *E300320,1 Set default for <List> choice.
      lnOption = 1
    ELSE
    *E300320,1 Reham On 02/12/96    *** End   ***
      lnOption = gfDialog('?','Object not found in the database.','\!\<List;\?\<Reenter')
    ENDIF
    
    *E300320,1 Reham On 01/08/96    *** End   ***
    IF lnOption = 1
      private lcBrFields,lcFilter,laData
      DIMENSION laData[1]
      STORE '' to laData
      lcBrFields = [cobject_id:H="Object Code",cObj_desc:H="Description"]
      lcTemp     = gfTempName()
      *E300320,1 Reham On 12/31/95    *** Begin ***
      *E300320,1 If called from style inquire screen, & the link radio
      *E300320,1 button value is "Linked", select only the objects linked
      *E300320,1 to the current style.
      *E300320,1 Add the field "gObject" to the selected fields.
      IF llLink .AND. rbLink = 2
         *E300320,5 Reham On 03/05/96    *** Begin ***
         *E300320,5 Get the description from the object link file if linked.
         *SELECT cObject_ID,COBJ_DESC,gObject ;
               FROM (QID+'OBJECTS') ;
               WHERE SEEK(cObject_ID+lcObjType+PADR(lcObjectID,20),'OBJLINK') ;
                     .OR. cObject_ID=(lcObjCode) ;
               INTO CURSOR (lcTemp)
         *E300637,1 WAM 06/03/1997 Use the new global variables names
         *SELECT OBJLINK.cObject_ID,OBJLINK.COBJ_DESC,OBJECTS.gObject ;
            FROM (QID+'OBJECTS'), (QDD+'OBJLINK') ;
         WHERE Objlink.cobjlnktyp+Objlink.cobjlink = lcObjType+PADR(lcObjectID,20) .AND. ;
                      OBJLINK.cObject_ID = OBJECTS.cObject_ID ;
              INTO CURSOR (lcTemp)
         
         *E500418,1 AME 10/05/2001 [Start]  
         *E500418,1 AME 10/05/2001  Add the llnk and imgpath fields to temp file.
         *SELECT OBJLINK.cObject_ID,OBJLINK.COBJ_DESC,OBJECTS.gObject ;
            FROM (gcDataDir+'OBJECTS'), (gcDataDir+'OBJLINK') ;
         WHERE Objlink.cobjlnktyp+Objlink.cobjlink = lcObjType+PADR(lcObjectID,20) .AND. ;
                      OBJLINK.cObject_ID = OBJECTS.cObject_ID ;
              INTO CURSOR (lcTemp)

         
         SELECT OBJECTS.lLnk,OBJLINK.cObject_ID,OBJLINK.COBJ_DESC, ;
                OBJECTS.gObject,OBJECTS.cImgPath;
            FROM (gcDataDir+'OBJECTS'), (gcDataDir+'OBJLINK') ;
         WHERE Objlink.cobjlnktyp+Objlink.cobjlink = lcObjType+PADR(lcObjectID,20) .AND. ;
                      OBJLINK.cObject_ID = OBJECTS.cObject_ID ;
              INTO CURSOR (lcTemp)
         *E500418,1 AME 10/05/2001 [End]
         
         *E300637,1 WAM 06/03/1997 (End)
           
         *E300320,5 Reham On 03/05/96    *** End   ***
               
         *E300320,1 Reham On 12/31/95    *** End   ***
      ELSE
        *E300637,1 WAM 06/03/1997 Use the new global variables names
        *SELECT cObject_ID,COBJ_DESC,gObject ;
               FROM (QID+'OBJECTS');
               WHERE COBJECT_ID='';
               INTO CURSOR (lcTemp)
        *E500418,1 AME [Start]     
        *SELECT cObject_ID,COBJ_DESC,gObject ;
               FROM (gcDataDir+'OBJECTS');
               WHERE COBJECT_ID='';
               INTO CURSOR (lcTemp)
        *E500418,4 AME [Start]                                                 
        *SELECT cObject_ID,COBJ_DESC,gObject ,cImgPath;
               FROM (gcDataDir+'OBJECTS');
               WHERE COBJECT_ID='';
               INTO CURSOR (lcTemp)

        SELECT llnk,cObject_ID,COBJ_DESC,gObject ,cImgPath;
               FROM (gcDataDir+'OBJECTS');
               WHERE COBJECT_ID='';
               INTO CURSOR (lcTemp)
      *E500418,4 AME [End]                                
      *E500418,1 AME [End]    
                 
               
               
        *E300637,1 WAM 06/03/1997 (End)
      ENDIF
      *E300320,1 Reham On 12/31/95    *** Begin ***
      *E300320,1 Instead of calling the browse, preview the available 
      *E300320,1 objects through the preview screen.
      *=ARIABROW(.F.,"Pictures & Objects",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","","cobject_id","laData")
      GO TOP
      
      *E300320,1 Reham On 02/11/96    *** Begin ***
      IF EMPTY(&lcTemp..cObject_ID)
        =gfDialog('!','There are no objects to be displayed.','\!\?\<Ok')
        lcObjCode = ''
        _CUROBJ = _CUROBJ
      ELSE
      *E300320,1 Reham On 02/11/96    *** End   ***
        lsAllObj = &lcTemp..cObject_ID
        lcCurObj = ""
        *E300320,1 Clear the trapped key befor calling the preview screen.
        PUSH KEY CLEAR
        *E300320,1 Call the preview screen.
        *E300637,1 WAM 06/03/1997 Use the new global variables names
        *DO (qsd+'PRVIEW.SPR')
        DO PRVIEW.SPR
        *E300637,1 WAM 06/03/1997 (End)
        *E300320,1 Pop the trapped key in the buffer again.
        POP KEY
        *E300320,1 Reham On 12/31/95    *** End   ***
        
        USE IN (lcTemp)
        SELECT OBJECTS
        
        *E300320,1 Reham On 12/31/95    *** Begin ***
        *lcObjCode = laData[1]
        lcObjCode = lcCurObj
        *E300320,1 Reham On 12/31/95    *** End   ***
        
        IF !EMPTY(lcObjCode) AND SEEK (lcObjCode)
          IF llLink AND SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
            lcNotes       = OBJLINK.Mobj_Notes
            lcDescription = OBJLINK.cObj_Desc        
            *E300320,1 Reham On 01/02/96    *** Begin ***
            cbLink = IIF(SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK'),1,0)
            SHOW GET cbLink      ENABLE
            *E300320,1 Reham On 01/02/96    *** End   ***
          ELSE
            lcNotes       = Mobj_Notes
            lcDescription = cObj_Desc
          ENDIF  
          SHOW GET pbNewObj      ENABLE    
          SHOW GET lcObjCode     DISABLE
          *E300320,1 Reham On 12/31/95    *** Begin ***
          SHOW GET pbBrow        DISABLE
          *E300320,1 Reham On 12/31/95    *** End   ***
          SHOW GET lcNotes       ENABLE
          SHOW GET lcDescription ENABLE  
          lcObjStat = 'ENABLE'
          SHOW GETS ENABLE WINDOW OBJBUTTN
        
       *E500418,1 AME [Start] N
       *E500418,4 AME [Start]
       *=SEEK (cObject_ID,'OBJLINK')
       *IF OBJLINK.LLNK
       IF LLNK
       *E500418,4 AME [END]
        =lfDraw()
        ELSE
         =lfDispPict()
        ENDIF
        
          *=lfDispPict()
         *E500418,1 AME [End]N
          =lfActivate()
        
        ELSE
          lcObjCode = ''
          _CUROBJ = _CUROBJ
        ENDIF
      ENDIF
    ELSE
      lcObjCode = ''
      _CUROBJ = _CUROBJ
    ENDIF
  ELSE
    IF llLink AND SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
      lcNotes       = OBJLINK.Mobj_Notes
      lcDescription = OBJLINK.cObj_Desc        
      *E300320,1 Reham On 01/02/96    *** Begin ***
      cbLink = IIF(SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK'),1,0)
      SHOW GET cbLink      ENABLE
      *E300320,1 Reham On 01/02/96    *** End   ***
    ELSE
      lcNotes       = Mobj_Notes
      lcDescription = cObj_Desc
    ENDIF  
    SHOW GET pbNewObj      ENABLE
    SHOW GET lcObjCode     DISABLE
    *E300320,1 Reham On 12/31/95    *** Begin ***
    SHOW GET pbBrow        DISABLE
    *E300320,1 Reham On 12/31/95    *** End   ***
    SHOW GET lcNotes       ENABLE
    SHOW GET lcDescription ENABLE
    lcObjStat = 'ENABLE'
    SHOW GETS ENABLE WINDOW OBJBUTTN
    =lfDispPict()
    =lfActivate()
  ENDIF
ENDIF

*!*************************************************************************
*! Name      : lfwPrvObj
*! Developer : Reham Alallamy
*! Date      : 09/01/96
*! Purpose   : When function of the list object in the prview screen to 
*!           : display the current object for the current object ID in 
*!           : the list.
*!*************************************************************************
*! Calls     :
*!             FUNCTION : 
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfwPrvObj
*E500418,1 AME [Start]

=imSET("MENU",.F.)
=imCLOSE( ) 
*E500418,1 AME [Start] 
=SEEK (cObject_ID,'OBJLINK')
*E500418,4 AME [Start] 
*IF OBJLINK.llnk
IF &lcTemp..llnk
*E500418,4 AME [End] 
*E500418,1 AME [End] 
  *wh = imGet("HWND",'Obj')
  wh = imGet("HWND",'Selected Object')
  lcNew_wh = imCREATEF("Object",0,0,180,200,WH)
  *E500418,4 AME [start] 
  imgsize=imGET("ImgSize",&lcTemp..cImgPath)
  lnwidth=0 
  lnheight=0
  lnDepth=0
  =imSCANINTS(ImgSize, @lnwidth, @lnheight, @lnDepth)  
  *C200388,1 (Begin) Avoid this message for Dare as we made a program for him to open .DOC,.XLS
  IF ASCAN(laEvntTrig , PADR('CANDISP',10)) <> 0
    RETURN
  ENDIF
  *C200388,1 (End)
  IF !INLIST(lnDepth, 1,4,8,16,24)
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'File cannot be displayed because it’s current format is not supported.')
    RETURN
  ENDIF
  IF lnwidth =0 .OR. lnheight =0
    =imSET("SCROLL",.T.)
    =imImage(&lcTemp..cImgPath, 0,0)
    =imSET("MENU",.F.)
    RETURN
  ENDIF
  IF lnwidth < 200  .AND.  lnheight <180
    =imSET("FILL",.F.) 
    =imSET("SCROLL",.F.)
    =imImage(&lcTemp..cImgPath, 90-lnheight/2,100-lnwidth/2)
  ELSE
  *E500418,4 AME [End] 
    *=imSET("FILL",.T.) 
    =imSET("FILL",.F.) 
    IF lnwidth > 200  
      =imSET("SCROLLX",.T.) 
    ENDIF
    IF lnheight >180
      =imSET("SCROLLY",.T.) 
    ENDIF
    =imImage(&lcTemp..cImgPath, 0, 0)
  *E500418,4 AME [Start] 
  ENDIF
  =imSET("MENU",.F.)
  *E500418,4 AME [End] 
ELSE
*E500418,1 AME [End]
  ACTIVATE WINDOW PRVIEW_1
  *E300320,5 Reham On 03/04/96    *** Begin ***
  *E300320,5 Display the objects isometric instead of stretch.
  *@ 0.083,0.250 SAY &lcTemp..gObject SIZE 16.500,24.375 STRETCH
  @ 0.083,0.250 SAY &lcTemp..gObject SIZE 16.500,24.375 ISOMETRIC
  *E300320,5 Display the description in the preview screen.
  SHOW GET cObj_Desc
  *E300320,5 Reham On 03/04/96    *** Begin ***
*E500418,1 AME [Start]
ENDIF
*E500418,1 AME [End]
*!*************************************************************************
*! Name      : lfvDescription
*! Developer : Hesham El-Sheltawi
*! Date      : 09/07/95    
*! Purpose   : valid function for the Description get field
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvDesciption
PRIVATE lcSelect

lcSelect = SELECT()

IF llLink
  IF !SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
    REPLACE cObj_Desc WITH lcDescription
  ELSE
    *E300320,5 Reham On 03/05/96    *** Begin ***
    *E300320,5 Save the description in the master file also if the field was empty.
    REPLACE cObj_Desc WITH IIF(EMPTY(cObj_Desc) , lcDescription, cObj_Desc)
    *E300320,5 Reham On 03/05/96    *** End   ***
    REPLACE OBJLINK.cObj_Desc WITH lcDescription  
  ENDIF
ELSE
  REPLACE cObj_Desc WITH lcDescription
ENDIF  
SELECT (lcSelect)

*!*************************************************************************
*! Name      : lfvNotes
*! Developer : Hesham El-Sheltawi
*! Date      : 09/07/95    
*! Purpose   : valid function for the Notes Get Field
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvNotes
PRIVATE lcSelect

lcSelect = SELECT()
IF llLink
  IF !SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
    REPLACE Mobj_Notes WITH lcNotes
  ELSE
    *E300320,5 Reham On 03/05/96    *** Begin ***
    *E300320,5 Save the notes in the master file also if the field was empty.
    REPLACE Mobj_Notes WITH IIF(EMPTY(Mobj_Notes) , lcNotes, Mobj_Notes)
    *E300320,5 Reham On 03/05/96    *** End   ***
    REPLACE OBJLINK.Mobj_Notes WITH lcNotes
  ENDIF
ELSE  
  REPLACE Mobj_Notes WITH lcNotes
ENDIF  
SELECT (lcSelect)

*!*************************************************************************
*! Name      : lfvNewObj
*! Developer : Hesham El-Sheltawi
*! Date      : 09/11/95    
*! Purpose   : valid function for New push button
*!*************************************************************************
*! Calls     :
*!            FUNCTION : lfActivate
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvNewObj

SELECT OBJECTS
*E500418,1 AME [Start] 
IF cbPath=0
  SHOW WINDOW ("OBJCNTNT")
ENDIF
*E500418,1 AME [End] 

*E500418,4 AME [Start]
llCancl = .F.
IF CbPath = 1
  lcPic_File = ""
  IF lfPicGet()
    llCancl = .F.
  ELSE
    llCancl = .T.
  ENDIF
ENDIF
IF llCancl
  =lfdraw()
  RETURN
ENDIF
*IF !llinsObj 
IF !llinsObj  .AND.  !llCancl
*E500418,4 AME [End]
  lcObjCode = lfGetLastObj()
  *SET DELETE OFF
  *IF !SEEK(' ')
    APPEND BLANK
  *ELSE
  *  RECALL  
  *ENDIF  
  *SET DELETE ON
  REPLACE COBJECT_ID WITH lcObjCode
  
  *E300320,5 Reham On 03/04/96    *** Begin ***
  *E300320,5 If called from the style screen.
  IF llLink
    *E300320,5 If added new object & the radio button value was linked,
    *E300320,5 link the added object to the current style.
    IF rbLink = 2
      SELECT OBJLINK
      APPEND BLANK  
      *E500418,1 AME [Start]
      *REPLACE cobject_id WITH lcObjCode,;
              cObjLink   WITH PADR(lcObjectID,20),;
              cObjLnkTyp WITH lcObjType 
      *E500418,4 AME [Start]
      *REPLACE cobject_id WITH lcObjCode,;
              cObjLink   WITH PADR(lcObjectID,20),;
              cObjLnkTyp WITH lcObjType ,;
              lLnk       WITH IIF(CbPath = 1,.T.,.F.)
      REPLACE cobject_id WITH lcObjCode,;
              cObjLink   WITH PADR(lcObjectID,20),;
              cObjLnkTyp WITH lcObjType 
      *E500418,4 AME [END]
      *E500418,1 AME [End]
      SELECT OBJECTS
    ENDIF
  ENDIF
  *E300320,5 Reham On 03/04/96    *** End   ***
ENDIF

lcObjStat     = 'ENABLE'
lcNotes       = Mobj_Notes
lcDescription = cObj_Desc

*E300320,1 Reham On 01/02/96    *** Begin ***
IF llLink
  cbLink = IIF(SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK'),1,0)
  SHOW GET cbLink        ENABLE
ENDIF
*E300320,1 Reham On 01/02/96    *** End   ***

SHOW GET lcNotes       ENABLE
SHOW GET lcDescription ENABLE  
SHOW GET pbNewObj      ENABLE
SHOW GET lcObjCode     DISABLE
*E300320,1 Reham On 12/31/95    *** Begin ***
SHOW GET pbBrow        DISABLE
*E300320,1 Reham On 12/31/95    *** End   ***
SHOW GETS ENABLE WINDOW OBJBUTTN

*E500418,4 AME [Start] 
IF CbPath <> 1
  =lfDispPict()
  REPLACE llnk WITH .F.
  =lfActivate()
ENDIF


IF !llCancl
  IF CbPath = 1 
    REPLACE cImgPath WITH lcPic_File,;
            llnk WITH .T.
    llinsObj   = .F.          
  ELSE
    llinsObj   = .T.          
  ENDIF
ELSE
  IF CbPath = 1 
    =lfdraw()
  ELSE
    =lfActivate()
  ENDIF
ENDIF

*B608093,1 NNA 05/23/2007 (Begin) Get the Audit Trail Information
=gfAdd_Info('OBJECTS')
=SEEK(Cobject_id,'OBJLINK')
=gfAdd_Info('OBJLINK')
*B608093,1 NNA (End)

*=lfDispPict()
*=lfActivate()
*llinsObj   = .T.
*E500418,4 AME [End] 

lcObjToAct = ['{CTRL+Z}']
_CUROBJ    = _CUROBJ + 1

*E300320,5 Reham On 03/05/96    *** Begin ***
CLEAR TYPEAHEAD
*E300320,5 Reham On 03/05/96    *** End   ***

*KEYBOARD "{TAB}" &&+"{CTRL+Z}" CLEAR

*!*************************************************************************
*! Name      : lfvLinkCb
*! Developer : Hesham El-Sheltawi
*! Date      : 09/11/95    
*! Purpose   : valid function for the Link push button
*!*************************************************************************
*! Calls     : 
*!             FUNCTION : lfActivate
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvLinkCb

lcSelect = SELECT()
SELECT OBJLINK
IF llLink
  IF !SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
    SET DELETE OFF
    IF SEEK(' ')
      RECALL
    ELSE
      APPEND BLANK  
    ENDIF
    SET DELETE ON
    REPLACE cobject_id WITH lcObjCode,;
            cObjLink   WITH PADR(lcObjectID,20),;
            cObjLnkTyp WITH lcObjType 
  ELSE
    *E300320,1 Reham On 01/09/96    *** Begin ***
    *E300320,1 Give the user a warning message that he is going to unlink
    *E300320,1 the current object.
    lnOption =gfDialog('?','Are you sure you want to unlink the current object?','\!\<Yes;\?\<No')
    IF lnOption = 1
    *E300320,1 Reham On 01/09/96    *** End   ***
      REPLACE cobject_id WITH '',;
              cObjLink   WITH '',;
              cObjLnkTyp WITH '' 
      DELETE
      *E300320,1 Reham On 01/09/96    *** Begin ***
      GO TOP
      SELECT OBJECTS
      SET FILTER TO
      SET FILTER TO SEEK(cObject_ID+lcObjType+PADR(lcObjectID,20),'OBJLINK')
      LOCATE
      GO TOP
      SELECT (lcSelect)
      lcObjCode = COBJECT_ID
      IF llLink AND SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
        lcNotes       = OBJLINK.Mobj_Notes
        lcDescription = OBJLINK.cObj_Desc    
        cbLink = IIF(SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK'),1,0)
      ELSE
        lcNotes       = Mobj_Notes
        lcDescription = cObj_Desc  
      ENDIF
      
      SHOW GETS
      *E500418,4 AME [START]      
      =lfDraw()
      *E500418,4 AME [END]      
      =lfActivate()
      *E300320,1 Reham On 01/09/96    *** End   ***
    ENDIF
  ENDIF
*E300320,1 Reham On 02/12/96    *** Begin ***
*E300320,1 This code was moved to a new function to open the link to screen in case
*E300320,1 of the screen called from the menu.
*ELSE
*  lcTempCurs = 'X'+RIGHT(SYS(3),7)
*  SELECT *,'S' AS CSTATUS,RECNO() AS NRECNO FROM (Qdd+'OBJLINK') ;
*   WHERE cobject_id+cobjlnktyp+cobjlink = lcObjCode ;
*    INTO DBF (qwd+lcTempCurs)
*   INDEX ON cobject_id + cobjlnktyp + cobjlink TAG CLINKCODE
*  SET FILTER TO !(CSTATUS $ 'DR')
*  DO lflinkScreen WITH 'L'
*  USE IN (lcTempCurs)
*  ERASE (qwd+lcTempCurs+'.DBF')
*  ERASE (qwd+lcTempCurs+'.FPT')
*  ERASE (qwd+lcTempCurs+'.CDX')
*E300320,1 Reham On 02/12/96    *** End   ***
ENDIF
SELECT (lcSelect)
SHOW GETS

*!*************************************************************************
*! Name      : lfvLink
*! Developer : Reham Alallamy
*! Date      : 02/12/96
*! Purpose   : Function called from link to button to call the link screen.
*!           : 
*!*************************************************************************
*! Calls     :
*!             FUNCTION : 
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvLink

*E300320,1 This code with moved to create new function for
*E300320,1 calling the linking screen.
lcSelect = SELECT()
SELECT OBJLINK
IF !llLink
  lcTempCurs = 'X'+RIGHT(SYS(3),7)
  
  *E300637,1 WAM 06/03/1997 Use the new global variables names  
  *SELECT *,'S' AS CSTATUS,RECNO() AS NRECNO FROM (Qdd+'OBJLINK') ;
   WHERE cobject_id+cobjlnktyp+cobjlink = lcObjCode ;
    INTO DBF (qwd+lcTempCurs)
  *INDEX ON cobject_id + cobjlnktyp + cobjlink TAG CLINKCODE
  *SET FILTER TO !(CSTATUS $ 'DR')
  *DO lflinkScreen WITH 'L'
  *USE IN (lcTempCurs)
  *ERASE (qwd+lcTempCurs+'.DBF')
  *ERASE (qwd+lcTempCurs+'.FPT')
  *ERASE (qwd+lcTempCurs+'.CDX')

  SELECT *,'S' AS CSTATUS,RECNO() AS NRECNO FROM (gcDataDir+'OBJLINK') ;
   WHERE cobject_id+cobjlnktyp+cobjlink = lcObjCode ;
    INTO DBF (gcWorkDir+lcTempCurs)
  INDEX ON cobject_id + cobjlnktyp + cobjlink TAG CLINKCODE
  SET FILTER TO !(CSTATUS $ 'DR')
  DO lflinkScreen WITH 'L'
  USE IN (lcTempCurs)
  ERASE (gcWorkDir+lcTempCurs+'.DBF')
  ERASE (gcWorkDir+lcTempCurs+'.FPT')
  ERASE (gcWorkDir+lcTempCurs+'.CDX')
  *E300637,1 WAM 06/03/1997 (End)
ENDIF
SELECT (lcSelect)
SHOW GETS

*!*************************************************************************
*! Name      : lfvPrevious
*! Developer : Hesham El-Sheltawi
*! Date      : 09/06/95    
*! Purpose   : valid function for the previous push button
*!*************************************************************************
*! Calls     :
*!            FUNCTION : lfActivate
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvPrevious

IF !BOF()
  SKIP -1
  lcObjCode = COBJECT_ID  
  IF llLink AND SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
    lcNotes       = OBJLINK.Mobj_Notes
    lcDescription = OBJLINK.cObj_Desc    
  ELSE
    lcNotes       = Mobj_Notes
    lcDescription = cObj_Desc  
  ENDIF 
  *E500418,1 AME [START]
  IF lcObjStat='ENABLE'
    *E500418,4 AME [START]
    *cbPath = IIF( OBJLINK.LLNK,1,0)
    cbPath = IIF( LLNK,1,0)
    *E500418,4 AME [END]
  ENDIF
  *E500418,1 AME [End]
   *E500418,1 AME [Start] N
 
     =lfdraw()
 
  *E500418,1 AME [End]N
  SHOW GETS
  =lfActivate()  
ENDIF

*!*************************************************************************
*! Name      : lfvNext
*! Developer : Hesham El-Sheltawi
*! Date      : 09/06/95    
*! Purpose   : valid function for the next push button
*!*************************************************************************
*! Calls     :
*!            FUNCTION : lfActivate
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvNext

IF !EOF()
  SKIP 1
  lcObjCode = COBJECT_ID
  IF llLink AND SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
    lcNotes       = OBJLINK.Mobj_Notes
    lcDescription = OBJLINK.cObj_Desc
  ELSE
    lcNotes       = Mobj_Notes
    lcDescription = cObj_Desc 
  ENDIF
  *E500418,1 AME [START]
  IF lcObjStat='ENABLE'
    *E500418,4 AME [START]
    *cbPath = IIF( OBJLINK.LLNK,1,0)
    cbPath = IIF( LLNK,1,0)
    *E500418,4 AME [END]
  ENDIF
  *E500418,1 AME [End]
  SHOW GETS
  *E500418,1 AME [Start]N
 *=SEEK (cObject_ID,'OBJLINK')
 =lfdraw()
  
  *E500418,1 AME [End]N
  =lfActivate()
ENDIF

*!*************************************************************************
*! Name      : lfvTop
*! Developer : Reham Alallamy
*! Date      : 12/31/95
*! Purpose   : valid function for the top push button
*!*************************************************************************
*! Calls     :
*!            FUNCTION : lfActivate
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvTop

GO TOP

lcObjCode = COBJECT_ID
IF llLink AND SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
  lcNotes       = OBJLINK.Mobj_Notes
  lcDescription = OBJLINK.cObj_Desc
ELSE
  lcNotes       = Mobj_Notes
  lcDescription = cObj_Desc 
ENDIF
*E500418,1 AME [START]
IF lcObjStat='ENABLE'
    *E500418,4 AME [START]
    *cbPath = IIF( OBJLINK.LLNK,1,0)
    cbPath = IIF( LLNK,1,0)
    *E500418,4 AME [END]
ENDIF
*E500418,1 AME [Start]
=lfdraw()
*E500418,1 AME [End]
SHOW GETS
=lfActivate()

*!*************************************************************************
*! Name      : lfvBottom
*! Developer : Reham Alallamy
*! Date      : 12/31/95
*! Purpose   : valid function for the bottom push button
*!*************************************************************************
*! Calls     :
*!            FUNCTION : lfActivate
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvBottom

GO BOTTOM
lcObjCode = COBJECT_ID
IF llLink AND SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
  lcNotes       = OBJLINK.Mobj_Notes
  lcDescription = OBJLINK.cObj_Desc    
ELSE
  lcNotes       = Mobj_Notes
  lcDescription = cObj_Desc  
ENDIF  
*E500418,1 AME [START]
IF lcObjStat='ENABLE'
    *E500418,4 AME [START]
    *cbPath = IIF( OBJLINK.LLNK,1,0)
    cbPath = IIF( LLNK,1,0)
    *E500418,4 AME [END]
ENDIF

 *E500418,1 AME [Start] N

     =lfdraw()
 
  *E500418,1 AME [End]N
*E500418,1 AME [End]
SHOW GETS
=lfActivate()  

*!*************************************************************************
*! Name      : lfvSelect
*! Developer : Hesham El-Sheltawi
*! Date      : 09/11/95    
*! Purpose   : Valid Function for the select push button
*!*************************************************************************
*! Passed parameters  : 
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvSelect

lcObjStat = 'DISABLE'
STORE '' TO lcObjCode,lcNotes,lcDescription

*E300320,1 Reham On 01/02/96    *** Begin ***
IF llLink
  cbLink = 0
  SHOW GET cbLink DISABLE
ENDIF
*E300320,1 Reham On 01/02/96    *** End   ***


SHOW GET pbNewObj      ENABLE
SHOW GET lcObjCode     ENABLE
*E300320,1 Reham On 12/31/95    *** Begin ***
SHOW GET pbBrow        ENABLE
*E300320,1 Reham On 12/31/95    *** End   ***
SHOW GET lcNotes       DISABLE
SHOW GET lcDescription DISABLE  
SHOW GETS DISABLE WINDOW OBJBUTTN
SHOW GET pbClose ENABLE

*E300320,1 Reham On 01/09/96    *** Begin ***
SHOW GETS DISABLE WINDOW OBJPANEL
*E300320,1 Reham On 01/09/96    *** End   ***
*E500418,1 AME [Start]
IF !llnk 
*E500418,1 AME [End]
  =lfDispPict()
*E500418,1 AME [Start]
ELSE 
  SHOW WINDOW OBJCNTNT  
ENDIF
*E500418,1 AME [End]
=lfActivate()

_CUROBJ = OBJNUM(lcObjCode)

*!*************************************************************************
*! Name      : lfvDelete
*! Developer : Hesham El-Sheltawi
*! Date      : 09/11/95    
*! Purpose   : Valid Function for the Delete push button
*!*************************************************************************
*! Passed parameters  : 
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvDelete

lcSelect = SELECT()
SELECT OBJECTS
llDelete = .F.
IF SEEK(lcObjCode,'OBJLINK')
   llDelete = gfDialog('?','This object is linked to one or more data files. Are you sure you want to delete it?','\!\<Delete;\?\<Cancel')=1
ELSE
   llDelete = gfDialog('?','Are you sure you want to delete this object?','\!\<Delete;\?\<Cancel')=1
ENDIF
IF llDelete
  SELECT OBJLINK
  REPLACE COBJECT_ID WITH REPL(' ',10),;
          COBJLNKTYP with ' ',;
          COBJLINK   WITH REPL(' ',20)
  DELETE FOR COBJECT_ID = REPL(' ',31)
  SELECT OBJECTS
  REPLACE COBJECT_ID WITH ''
  DELETE
  lcObjStat = 'DISABLE'
  STORE '' TO lcObjCode,lcNotes,lcDescription
  SHOW GET pbNewObj      ENABLE
  SHOW GET lcObjCode     ENABLE
  *E300320,1 Reham On 12/31/95    *** Begin ***
  SHOW GET pbBrow        ENABLE
  *E300320,1 Reham On 12/31/95    *** End   ***
  SHOW GET lcNotes       DISABLE
  SHOW GET lcDescription DISABLE  
  SHOW GETS DISABLE WINDOW OBJBUTTN
  SHOW GET pbClose ENABLE
  =lfDispPict()
  =lfActivate()
  _CUROBJ = OBJNUM(lcObjCode)
ENDIF
SELECT (lcSelect)

*!*************************************************************************
*! Name      : lpTrapTab
*! Developer : Hesham El-Sheltawi
*! Date      : 09/06/95    
*! Purpose   : key trapping function to redirect the objects movements
*!*************************************************************************
*! Passed parameters  : lnswitch
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
PROCEDURE lpTrapTab
PARAMETERS lnSwitch

*-- lnswitch is even 1 or 2
*-- 1 for the pressing of the tab key
*-- 2 for the shift tab key

DO CASE
  *-- case tab key pressed
  CASE lnSwitch = 1
    DO CASE
      *-- if the object window is the window ontop
      CASE 'GOBJECT' $ WONTOP()
        *--activate the next window
        ACTIVATE WINDOW ("OBJECTCNT")

      *-- if the window on top is the window that hold the controll
      *-- buttons and the active object is the close push button  
      CASE WONTOP() = 'OBJBUTTN' AND VARREAD()='PBCLOSE'  
        *-- activate the object window
        IF lcObjStat = 'DISABLE'
          _CUROBJ = OBJNUM(lcObjCode)
        ELSE
          KEYBOARD "{Ctrl+J}"
        ENDIF  
      *-- if normal objects then let the fox controll itself  
      OTHERWISE
        KEYBOARD "{TAB}" PLAIN CLEAR
    ENDCASE
  *-- case the pressed key was shift tab  
  CASE lnSwitch = 2
    DO CASE
      *-- if the active window is the object window
      CASE 'GOBJECT' $ WONTOP()
        *-- go to the control keys window and make the current object
        *-- the close push button
        ACTIVATE WINDOW ("OBJBUTTN")
        _CUROBJ = OBJNUM(PBCLOSE)
      *-- if the window on top is the window that have the code
      *-- and description then activate the objects window  
      CASE WONTOP()  = 'OBJECTCNT' AND VARREAD()='LCOBJCODE'
        IF lcObjStat = 'ENABLE'
          KEYBOARD "{Ctrl+J}"
        ELSE
          _CUROBJ    = OBJNUM(PBCLOSE)
        ENDIF  
      CASE WONTOP()  = 'OBJECTCNT' AND VARREAD()='PBNEW' .AND. lcObjStat='ENABLE'
          KEYBOARD "{Ctrl+J}"
      *-- if normal case then let the fox controll itself  
      OTHERWISE
        KEYBOARD "{BACKTAB}" PLAIN CLEAR
    ENDCASE
ENDCASE

*!*************************************************************************
*! Name      : lfDefineBars
*! Developer : Hesham El-Sheltawi
*! Date      : 09/06/95    
*! Purpose   : function to define bars of the menu for the objects control
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfDefineBars

DEFINE BAR _MED_CUT   OF _PctPop PROMPT PADR("  \<Cut"+lcWWindow,18)   KEY Ctrl+X,"Ctrl+X   "
DEFINE BAR _MED_COPY  OF _PctPop PROMPT PADR("  Cop\<y"+lcWWindow,18)  KEY Ctrl+C,"Ctrl+C   "
DEFINE BAR _MED_PASTE OF _PctPop PROMPT PADR("  \<Paste"+lcWWindow,18) KEY Ctrl+V,"Ctrl+V   "
DEFINE BAR _MED_CLEAR OF _PctPop PROMPT PADR("  Cl\<ear"+lcWWindow,18)

*!*************************************************************************
*! Name      : lfObjMessage
*! Developer : Hesham El-Sheltawi
*! Date      : 09/12/95    
*! Purpose   : function to return messages of the objects
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfObjMessage

RETURN ''

*!*************************************************************************
*! Name      : lfActOLEObj
*! Developer : Hesham El-Sheltawi
*! Date      : 09/11/95    
*! Purpose   : function to activate one of the OLE foxpro functions
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfActOLEObj

*E500418,4 AME [Start]
IF !OBJECTS.llnk
*E500418,4 AME [End]

  ACTIVATE WINDOW ("OBJECTS.GOBJECT")

*E500418,4 AME [Start]
ENDIF
*E500418,4 AME [End]
*E300320,5 Reham On 03/05/96    *** Begin ***
CLEAR TYPEAHEAD
*E300320,5 Reham On 03/05/96    *** End   ***

IF !EMPTY(lcObjToAct)
  KEYBOARD &lcObjToAct
ENDIF

lcObjToAct = ''

*!*************************************************************************
*! Name      : lfGetLastObj
*! Developer : Hesham El-Sheltawi
*! Date      : 09/11/95    
*! Purpose   : function to get sequential object ID
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfGetLastObj

lcFilter = FILTER()
SET FILTER TO
GO BOTT
lcReturn = PADL(INT(VAL(COBJECT_ID))+1,10,'0')
SET FILTER TO &lcFilter

RETURN lcReturn

*!*************************************************************************
*! Name      : gfDialog1
*! Developer : Hesham El-Sheltawi
*! Date      : 09/11/95    
*! Purpose   : 
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION gfDialog1
PARAMETERS lcIcon,lcMessage,lcButtons

DEFINE WINDOW DDD FROM 1,1 TO 7,50 SYSTEM
MOVE WINDOW DDD CENTER
ACTI WINDOW DDD NOSHOW
@ 1,1 SAY lcMessage
@ 3,3 GET lnOption PICTURE "@*HT "+lcButtons ;
	  SIZE 1.769,9.333,0.667 ;
	  DEFAULT 1 

ACTIVATE WINDOW DDD	  
READ CYCLE
RELEASE WINDOW DDD
RETURN lnOption

*************** Valid Functions For The Link To Screen ***************


*!*************************************************************************
*! Name      : lflinkScreen
*! Developer : Hesham El-Sheltawi
*! Date      : 09/12/95    
*! Purpose   : function to call the LinkObj screen
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lflinkScreen
PARAMETERS lcScreenType
PRIVATE lcDescription ,lcNotes

lcExact = SET('EXACT')
SET EXACT ON
DO CASE
  CASE lcScreenType = 'L'
    lcFirstPrompt   = '\<Add Link'
    lcSecondPrompt  = '\<Remove Link'
    lcTitle = 'Link To'    
  CASE lcScreenType = 'F'  
    lcFirstPrompt   = '\<Add Filter'
    lcSecondPrompt  = '\<Remove Filter'      
    lcTitle = 'Filter On'        
ENDCASE
PUSH KEY CLEAR
llActDesc = .F.
*E300637,1 WAM 06/03/1997 Use the new global variables names
*DO (qsd+'linkObj.SPR')
DO linkObj.SPR
*E300637,1 WAM 06/03/1997 (End)
POP KEY
SET EXACT &lcExact

*!*************************************************************************
*! Name      : lfLinkShow
*! Developer : Hesham El-Sheltawi
*! Date      : 09/12/95    
*! Purpose   : SHOW function for the Linking screen
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfLinkShow

lnRecPos = IIF(RECCOUNT()<RECNO(),0,RECNO())
LOCATE
IF EOF() 
  SHOW GET PBSECOND DISABLE
  SHOW GET pbDesc   DISABLE
  IF WVISIBLE('OBJDESC')
    HIDE WINDOW OBJDESC
  ENDIF
ELSE  
  SHOW GET PBSECOND ENABLE
  SHOW GET pbDesc   ENABLE
ENDIF
IF lnRecPos > 0
  GO lnRecPos
ENDIF

*!*************************************************************************
*! Name      : lfwList
*! Developer : Hesham El-Sheltawi
*! Date      : 09/12/95    
*! Purpose   : WHEN FUNCTON FOR THE LIST OF THE LINKING
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfwList

lnArrPos   = ASCAN(laObjLinkF,cobjlnktyp)
PUFILEPOS  = ASUB(laObjLinkF,IIF(lnArrPos>0,lnArrPos,1),1)
lcLinkCode = COBJLINK
IF lcScreenType = 'L'
  lcNotes       = Mobj_Notes
  lcDescription = cObj_Desc  
ENDIF  

*SHOW GET PUFILEPOS
*SHOW GET lcLinkCode
SHOW GETS

*!*************************************************************************
*! Name      : lfvRemLink
*! Developer : Hesham El-Sheltawi
*! Date      : 09/12/95    
*! Purpose   : VALID FUNCTON FOR remove link push button
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvRemLink

IF lcScreenType = 'L'
  REPLACE CSTATUS WITH IIF(CSTATUS='S','D','R')
  SHOW GETS
  _CUROBJ = OBJNUM(LSLINKOBJ)
ELSE
ENDIF  

*!*************************************************************************
*! Name      : lfvAddLink
*! Developer : Hesham El-Sheltawi
*! Date      : 09/12/95    
*! Purpose   : VALID FUNCTON FOR Add link push button
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvAddLink

SHOW GET PUFILEPOS ENABLE
_CUROBJ = OBJNUM(PUFILEPOS)
KEYBOARD "{SPACEBAR}"

*!*************************************************************************
*! Name      : lfvFilePos
*! Developer : Hesham El-Sheltawi
*! Date      : 09/12/95    
*! Purpose   : VALID FUNCTON FOR files popup 
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     

FUNCTION lfvFilePos

IF EMPTY(pufilepos) OR LASTKEY()<>13
  _CUROBJ = OBJNUM(PBFIRST)
ELSE
  lcLinkCode = ''
  SHOW GET lcLinkCode ENABLE
  _CUROBJ = OBJNUM(lcLinkCode)
ENDIF
SHOW GET PUFILEPOS DISABLE

*!*************************************************************************
*! Name      : lfvLinkCode
*! Developer : Hesham El-Sheltawi
*! Date      : 09/13/95    
*! Purpose   : VALID FUNCTON FOR Link Code Get Field
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvLinkCode
PRIVATE lcSelect

IF PUFILEPOS <> 0 AND !EMPTY(lcLinkCode) AND LASTKEY() = 13
  lcSelect = SELECT()
  llUsedBy = .F.
  IF !USED(laObjLinkF[PUFILEPOS,2])
    SELECT 0
    *E300637,1 WAM 06/03/1997 Use the new global variables names
    *USE (qdd+laObjLinkF[PUFILEPOS,2]) ORDER TAG (laObjLinkF[PUFILEPOS,3])
    USE (gcDataDir+laObjLinkF[PUFILEPOS,2]) ORDER TAG (laObjLinkF[PUFILEPOS,3])
    *E300637,1 WAM 06/03/1997 (End)
    llUsedBy = .T.
  ELSE  
    SELECT (laObjLinkF[PUFILEPOS,2])
    lcOldTag = TAG()
    SET ORDER TO TAG (laObjLinkF[PUFILEPOS,3])
  ENDIF
  lnFieldLen = LEN(EVAL(SYS(14,VAL(SYS(21)))))
  lcLinkCode = IIF(lnFieldLen>LEN(ALLTRIM(lcLinkCode)),SUBSTR(lcLinkCode,1,lnFieldLen),lcLinkCode)
  IF RIGHT(ALLTRIM(lcLinkCode),1) ='?' OR !SEEK(PADR(lcLinkCode,lnFieldLen))
    IF !EMPTY(laObjLinkF[PUFILEPOS,4])
      lcParam = STRTRAN(laObjLinkF[PUFILEPOS,6],'{VALUE}','lcLinkCode')
      SET EXACT OFF
      DO (laObjLinkF[PUFILEPOS,4]) WITH &lcParam
      SET EXACT ON
    ELSE
      DIMENSION laData[1]
      =ARIABROW('',laObjLinkF[PUFILEPOS,1],gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"","",SYS(14,VAL(SYS(21))),"laData")
      lcLinkCode = laData[1]
    ENDIF
  ENDIF
  IF llUsedBy
    USE IN (laObjLinkF[PUFILEPOS,2])
  ELSE
    SET ORDER TO TAG (lcOldTag)   
  ENDIF
  SELECT (lcSelect)
  IF !EMPTY(lcLinkCode) AND !SEEK(lcObjCode+laObjLinkF[PUFILEPOS,5]+PADR(lcLinkCode,20))
    SET DELETE OFF
    IF SEEK(' ')
      RECALL
    ELSE
      APPEND BLANK  
    ENDIF
    SET DELETE ON
    REPLACE cobject_id WITH lcObjCode,;
            cObjLink   WITH PADR(lcLinkCode,20),;
            cObjLnkTyp WITH laObjLinkF[PUFILEPOS,5],;
            CSTATUS    WITH 'A'
    IF lcScreenType = 'L'              
         REPLACE cobj_desc WITH OBJECTS.cobj_desc,;
                 mobj_notes WITH OBJECTS.mobj_notes
    ENDIF
  ENDIF
  lnRecNo = IIF(RECCOUNT()<RECNO(),0,RECNO())
  COUNT ALL FOR  cobject_id+cObjLnkTyp+cObjLink<=lcObjCode+laObjLinkF[PUFILEPOS,5]+PADR(lcLinkCode,20);
             AND cStatus $ 'SA' TO LSLINKOBJ  
  IF lnRecNo <> 0           
    GO lnRecNo         
  ENDIF
  SHOW GET lcLinkCode DISABLE
  =lfwList()
  SHOW GETS
ENDIF

*!*************************************************************************
*! Name      : lfvOkLink
*! Developer : Hesham El-Sheltawi
*! Date      : 09/15/95    
*! Purpose   : VALID FUNCTON FOR Ok push button
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
FUNCTION lfvOkLink

IF lcScreenType = 'L'
  DIMENSION laData[5],laDumy[5]
  SCATTER FIELDS cobject_id,cobjlink,cobjlnktyp,cobj_desc,mobj_notes MEMO TO laDumy BLANK
  SET FILTER TO
  SCAN ALL FOR NOT (cStatus$'SR')
    SCATTER FIELDS cobject_id,cobjlink,cobjlnktyp,cobj_desc,mobj_notes MEMO TO laData 
    lnRecNo  = nrecno
    lCSTATUS = CSTATUS
    SELECT OBJLINK
    IF lCSTATUS  <> 'D'
      IF lnRecNo <> 0
        GO lnRecNo
      ELSE
        APPEND BLANK
      ENDIF
      GATHER FIELDS cobject_id,cobjlink,cobjlnktyp,cobj_desc,mobj_notes FROM laData MEMO
    ELSE
       GO lnRecNo
       GATHER FIELDS cobject_id,cobjlink,cobjlnktyp,cobj_desc,mobj_notes FROM laDUMY MEMO     
       DELETE  
    ENDIF   
  ENDSCAN
ELSE
ENDIF            

*!*************************************************************************
*! Name      : lfvLinkDesc
*! Developer : Hesham El-Sheltawi
*! Date      : 09/15/95    
*! Purpose   : VALID FUNCTON FOR Description get field
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvLinkDesc

REPLACE cobj_desc WITH lcDescription,;
        cstatus   WITH IIF(cSTATUS='S','M',cStatus)

*!*************************************************************************
*! Name      : lfvLinkNote
*! Developer : Hesham El-Sheltawi
*! Date      : 09/15/95    
*! Purpose   : VALID FUNCTON FOR Notes get field
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvLinkNote

REPLACE mobj_notes WITH lcNotes,;
        cstatus   WITH IIF(cSTATUS='S','M',cStatus)

*!*************************************************************************
*! Name      : lfActObjDes
*! Developer : Hesham El-Sheltawi
*! Date      : 09/15/95    
*! Purpose   : activate function for the description screen
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfActObjDes

IF lcScreenType = 'L'
  IF WOUTPUT() = 'OBJDESC' AND !llActDesc
    _CUROBJ = 1
  ENDIF
ENDIF

RETURN .T.

*!*************************************************************************
*! Name      : lfvActNotes
*! Developer : Hesham El-Sheltawi
*! Date      : 09/15/95    
*! Purpose   : valid function for the description push button 
*!             to activate the description screen 
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfvActNotes

llActDesc = .T.
_CUROBJ = OBJNUM(lcDescription)

*!*************************************************************************
*! Name      : lfCntrPanl
*! Developer : Reham Alallamy
*! Date      : 02/12/96
*! Purpose   : Function called from different places to handle the enabling
*!           : & disabling of the navigations button in the control pannel.
*!*************************************************************************
*! Calls     :
*!             FUNCTION : 
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfCntrPanl

*E300320,1 If the object code is not empty.
IF !EMPTY(lcObjCode)
  *E300320,1 Check the position of the current record
  lnOldRec = RECNO()
  SKIP 1
  llEof = EOF()
  SKIP -2
  llBof = BOF()
  
  *E300320,1 Restore the current record position.
  IF lnOldRec <=RECCOUNT()
    GO lnOldRec
  ENDIF
  
  DO CASE
    *E300320,1 Record is the only record in the file
    CASE llBof .AND. llEof  
      *E300320,1 Disable all the navigations buttons.
      SHOW GET pbTop      DISABLE
      SHOW GET pbBottom   DISABLE
      SHOW GET pbNext     DISABLE
      SHOW GET pbPrevious DISABLE
    *E300320,1 Top record.
    CASE llBof
      *E300320,1 Disable the top & previous buttons.
      SHOW GET pbTop      DISABLE
      SHOW GET pbPrevious DISABLE
      *E300320,1 Enable the bottom & next buttons.
      SHOW GET pbBottom   ENABLE
      SHOW GET pbNext     ENABLE
    *E300320,1 Last record.  
    CASE llEof
      *E300320,1 Enable the top & previous buttons.
      SHOW GET pbTop      ENABLE
      SHOW GET pbPrevious ENABLE
      *E300320,1 Disable the bottom & next buttons.
      SHOW GET pbBottom   DISABLE
      SHOW GET pbNext     DISABLE
    *E300320,1 Any other record.
    OTHERWISE
      *E300320,1 Enable all the navigations buttons.
      SHOW GET pbTop      ENABLE
      SHOW GET pbBottom   ENABLE
      SHOW GET pbNext     ENABLE
     SHOW GET pbPrevious ENABLE
  ENDCASE   
ELSE
  *E300320,1 If the object coe is empty.
  *E300320,1 Disable all the navigations buttons.
  SHOW GET pbTop      DISABLE
  SHOW GET pbBottom   DISABLE
  SHOW GET pbNext     DISABLE
  SHOW GET pbPrevious DISABLE
ENDIF

*!*************************************************************************
*! Name      : lfPicGet
*! Developer : Ahmed Mohamed Elanwer (AME)
*! Date      : 05/09/2001
*! Purpose   : Function called to get picture form file.
*!*************************************************************************
*! Calls     :
*!             FUNCTION : 
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*E500418,1
FUNCTION lfPicGet

=imSET("MENU",.F.)
=imCLOSE( ) 

*E500418,4 AME [START]
*lcPic_File = GETFILE('JPG;GIF;DIB;MAC;PIC;WPG;TIF;BMP;TGA;IMG', '') 
lcPic_File = GETFILE('JPG|GIF|DIB|MAC|PIC|WPG|TIF|BMP|TGA|IMG', '') 
IF EMPTY (lcPic_File)
  RETURN .F.
ENDIF
*E500418,4 AME [End]
HIDE WINDOW ("OBJCNTNT")
HIDE WINDOW ("OBJDISP")
IF WEXIST("OBJECTS.GOBJECT")
  HIDE WINDOW ("OBJECTS.GOBJECT")
ENDIF
*E500418,1 AME [Start] 
SHOW WINDOW ("OBJPATH")
*E500418,1 AME [End]

wh = imGet("HWND",'Preview')
lcNew_wh = imCREATEF("MyObjWin",0,0,168,238,WH) 
*E500418,4 AME [START]
imgsize=imGET("ImgSize",lcPic_File)
lnwidth=0 
lnheight=0
lnDepth=0
=imSCANINTS(ImgSize, @lnwidth, @lnheight, @lnDepth)  
IF !INLIST(lnDepth, 1,4,8,16,24)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Invalid or not supported file format. Please resave it in different format and try again')
  RETURN .F.
ENDIF
IF lnwidth =0 .OR. lnheight =0
  =imSET("SCROLL",.T.)
  =imImage(lcPic_File, 0,0)
  =imSET("MENU",.F.)
  RETURN .T.
ENDIF
IF lnwidth < 238  .AND.  lnheight <168
  =imSET("FILL",.F.) 
  =imSET("SCROLL",.F.)
  =imImage(lcPic_File, 84-lnheight/2,119-lnwidth/2)
ELSE
*E500418,4 AME [END]
  *=imSET("FILL",.T.) 
  =imSET("FILL",.F.)   
  IF lnwidth > 238  
    =imSET("SCROLLX",.T.) 
  ENDIF
  IF lnheight >168
    =imSET("SCROLLY",.T.) 
  ENDIF
  =imImage(lcPic_File, 0, 0)
*E500418,4 AME [start] 
ENDIF
=imSET("MENU",.F.)
RETURN .T.
*E500418,4 AME [End] 

*!*************************************************************************
*! Name      : lfVPath
*! Developer : Ahmed Mohamed Elanwer (AME)
*! Date      : 05/09/2001
*! Purpose   : Valid Function for Path Check box
*!*************************************************************************
*! Calls     :
*!             FUNCTION : 
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*E500418,1
FUNCTION lfVPath

llPath = (CbPath = 1)
IF (CbPath = 1 .AND. !llnk) .OR. (CbPath <> 1 .AND. llnk)
  SHOW GET pbInsObj DISABLE
ELSE
  IF lcObjStat = 'DISABLE'
    SHOW GET pbInsObj DISABLE
  ELSE
    SHOW GET pbInsObj ENABLE      
  ENDIF
ENDIF
IF CbPath = 1
  SHOW GET pbModObj DISABLE
ELSE
  *E500418,4 AME [Start]
  *IF cbLink = 1
  *IF !llnk
  IF !llnk .AND. !EMPTY(GObject) .AND. lcObjStat <> 'DISABLE'
  *E500418,4 AME [End]
    SHOW GET pbModObj ENABLE
  ENDIF
ENDIF
*IF rbLink = 1
*  SET FILTER TO IIF(CbPath = 1,OBJLINK.lLnk,!(OBJLINK.lLnk))
*ELSE
*  SET FILTER TO SEEK(cObject_ID+lcObjType+PADR(lcObjectID,20),'OBJLINK') .AND. IIF(CbPath = 1,OBJLINK.lLnk,!(OBJLINK.lLnk)) 
*ENDIF

*!*************************************************************************
*! Name      : lfWZoom
*! Developer : Ahmed Mohamed Elanwer (AME)
*! Date      : 05/09/2001
*! Purpose   : When  Function for Zoom Path Screen
*!*************************************************************************
*! Calls     :
*!             FUNCTION : 
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*E500418,1
FUNCTION lfWZoom

=imSET("MENU",.F.)
=imCLOSE( ) 
ACTIVATE WINDOW ("ZOOMPATH")
lcWhZoom = imGet("HWND",'Zoom')
lcIm_wh = imCREATEF("MyObjWin",0,0,389,579,lcWhZoom) 
*E500418,4 AME [Start]   
imgsize=imGET("ImgSize",OBJECTS.cImgPath)
lnwidth=0 
lnheight=0
lnDepth=0
=imSCANINTS(ImgSize, @lnwidth, @lnheight, @lnDepth)  
IF !INLIST(lnDepth, 1,4,8,16,24)
  RETURN
ENDIF
IF lnwidth =0 .OR. lnheight =0
  =imSET("SCROLL",.T.)
  =imImage(OBJECTS.cImgPath, 0,0)
  =imSET("MENU",.F.)
  RETURN 
ENDIF
IF lnwidth < 579  .AND.  lnheight <389
  =imSET("FILL",.F.) 
  =imSET("SCROLL",.F.)
  =imImage(OBJECTS.cImgPath, 194-lnheight/2,289-lnwidth/2)
ELSE
*E500418,4 AME [END]
  =imSET("FILL",.F.) 
  =imSET("STRETCH",.F.) 
  =imSET("SCROLL",.T.)
  =imSET("STRETCH",.F.) 
  =imImage(OBJECTS.cImgPath, 0, 0)
  =imSET("SCROLL",.F.)
*E500418,4 AME [START]
ENDIF
=imSET("MENU",.F.)
*E500418,4 AME [End] 



*!*************************************************************************
*! Name      : lfDraw
*! Developer : Ahmed Mohamed Elanwer (AME)
*! Date      : 05/09/2001
*! Purpose   : 
*!*************************************************************************
*! Calls     :
*!             FUNCTION : 
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*E500418,1
FUNCTION lfDraw
PARAMETER llStrch
=imSET("MENU",.F.)
=imCLOSE( ) 
=SEEK(lcObjCode+lcObjType+PADR(lcObjectID,20),'OBJLINK')
*E500418,4 AME [START]
*IF    OBJLINK.LLNK
IF    LLNK
*E500418,4 AME [END]
  HIDE WINDOW ("OBJCNTNT")
  HIDE WINDOW ("OBJDISP")
  IF WEXIST("OBJECTS.GOBJECT")
    HIDE WINDOW ("OBJECTS.GOBJECT")
  ENDIF
  *E500418,1 AME [Start] 
  SHOW WINDOW ("OBJPATH") REFRESH 
  *E500418,1 AME [End] 
  
  wh = imGet("HWND",'Preview')
  lcNew_wh = imCREATEF("Object",0,0,168,238,WH) 
  *E500418,4 AME [START]
  imgsize=imGET("ImgSize",OBJECTS.cImgPath)
  lnwidth=0 
  lnheight=0
  lnDepth=0
  =imSCANINTS(ImgSize, @lnwidth, @lnheight, @lnDepth)  
  *C200388,1 (Begin) Avoid this message for Dare as we made a program for him to open .DOC,.XLS
  IF ASCAN(laEvntTrig , PADR('CANDISP',10)) <> 0
    RETURN
  ENDIF
  *C200388,1 (End)
  IF !INLIST(lnDepth, 1,4,8,16,24)
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'File cannot be displayed because it’s current format is not supported.')
    RETURN
  ENDIF
  IF lnwidth =0 .OR. lnheight =0
    =imSET("SCROLL",.T.)
    =imImage(OBJECTS.cImgPath, 0,0)
    =imSET("MENU",.F.)
    RETURN 
  ENDIF
  IF lnwidth < 238  .AND.  lnheight <168
    =imSET("FILL",.F.) 
    =imSET("SCROLL",.F.) 
    =imImage(OBJECTS.cImgPath, 84-lnheight/2,119-lnwidth/2)
  ELSE
  *E500418,4 AME [END]
    *=imSET("FILL",.T.) 
    =imSET("FILL",.F.) 
    IF lnwidth > 238  
      =imSET("SCROLLX",.T.) 
    ENDIF
    IF lnheight >168
      =imSET("SCROLLY",.T.) 
    ENDIF
    =imImage(OBJECTS.cImgPath, 0, 0)
  *E500418,4 AME [START]
  ENDIF
  =imSET("MENU",.F.)
  *E500418,4 AME [END]
ELSE
  IF WEXIST("OBJECTS.GOBJECT")
    SHOW WINDOW ("OBJECTS.GOBJECT")
  ENDIF
  SHOW WINDOW ("OBJCNTNT")
  *E500418,1 AME [End]  
ENDIF
*!*************************************************************************
*! Name      : lfVPrview
*! Developer : Ahmed Mohamed Elanwer (AME)
*! Date      : 05/09/2001
*! Purpose   : 
*!*************************************************************************
*! Calls     :
*!             FUNCTION : 
*!*************************************************************************      
*! Returns   :  
*!*************************************************************************     
*E500418,1
FUNCTION lfVPrview
IF llnk
  CbPath = 1
ELSE
  CbPath = 0
ENDIF