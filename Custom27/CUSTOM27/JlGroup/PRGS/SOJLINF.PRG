*!*******************************************************************************
*! Program file  : SoJLInf.PRG
*! Program desc. : Custom Order Design link with Thread Information.
*! Screen        : SoJLInf.SPR
*! System        : Aria Apparel System
*! Module        : Sales Order (SO)
*! Developer     : Sameh Saiid Ezzat (SSE)
*! Date          : 05/03/2001
*! Reference     : C102231
*!*******************************************************************************
*: Passed Parameters : lcType     -> "SO" if from order screen.
*:                                -> "AD" if from PO screen.
*:                   : lcOrdNo    -> Current sales order #.
*:                   : lnOrdLinNo -> Current line no.
*:                   : lcStyle    -> Current style in the line.
*:                   : lcT_BomVar -> Temp file copied from BOMVAR.
*:                   : lnOrdFold  -> Folder number coming from Order screen. 
*:*******************************************************************************
*: Example             : 
*: DO (gcAppHome+'SOJLINF.FXP') WITH "SO" , laData[1] , m.LineNo , m.Style , laData[2] , m.Store , lcT_BomVar
*:*******************************************************************************
*: Modification :
*C102357,1 AMH 06/21/2001 Using this screen with custom adornment order screen for JL.
*B804365,1 SSE 08/23/2001 Add some modifications to the Design screen.
*B604696,1 SSE 09/27/2001 Fix bug of wrong in the serialization.
*B804439,1 SSE 10/02/2001 Fix bug of alowing user to edit ThreadCl code.
*B605059,1 SSE 10/25/2001 Fix bug of wrong serialization also variable lctmpThClr not found.
*B804498,1 SSE 10/29/2001 Fix bug of adding the color change in thread folder.
*B804542,1 AKA 12/03/2001 Fix bug of repeating thread color in thread folder.
*B605571,1 SSE 02/18/2002 Fix bug of Deleting all lines of thread color for same design.
*B605487,1 MHM 02/24/2002 Make location ediatble
*B606103,1 SSH Fix the bug of removeing the thread when changing the locations.
*B605989,4 AMH 06/16/2002 Rename the account field in ICDESIGN file to cdsgnacct.
*:*******************************************************************************
*

*C102357,1 AMH Add parameter for calling from PO [Start]
*PARAMETERS lcType , lcOrdNo , lnOrdLinNo , lcStyle , lcAccount , lcStore , lcT_BomVar , lnOrdFold
PARAMETERS lcType , lcOrdNo , lnOrdLinNo , lcStyle , lcAccount , lcStore , lcT_BomVar , lnOrdFold, llFromPo
*C102357,1 AMH [End]

PRIVATE laFoldWinds , lcFoldPrnt , lnActFolder , lcFolder , lcWFoldChng , lcFoldPush , ;
        lnFolderCen , lnFolderRen , ibFolder , lnFoldSize , lnLastFold , lcType , ;
        lcOrdNo , lnOrdLinNo , lcStyle , lcAccount , lcStore , lcT_BomVar , lcBrowBmp , ;
        lcNewPos , lcRemPos , lcClsPos , lcBrwDsgn , lcBrwColor , lcCurStat , llOpnDsCd , ;
        lnMark , llBrowse , lnStatus , lnDsgnLoc , lcDsgnCode , lcDsgnName , lnStitches , ;
        lcNameDrp1 , lcNameDrp2 , lcNameDrp3 , lcThredClr , lcClrDesc , lcClrNotes , ;
        llOpnCodes , lcOldTag , lcBrTtl1 , lcBrTtl2 , lcBrowseTl

*B804365,1 Add variable to display the number of thread colors. [Begin]
PRIVATE lnTmpRecNo
lnTmpRecNo = 0
*B804365,1 Add variable to display the number of thread colors. [End]

*B804498,1 Initialize the color change variable. [Begin]
PRIVATE lcColorChg
lcColorChg = '0'
*B804498,1 Initialize the color change variable. [End]

STORE "" TO lcDsgnCode , lcDsgnName , lcNameDrp1 , lcNameDrp2 , lcNameDrp3 , ;
            lcThredClr , lcClrDesc , lcClrNotes , lcBrowseTl
lnStitches = 0

DECLARE laStatus[2] , laDsgnLoc[1,2]

*-- For Design Status. 
laStatus[1] = 'Approved'
laStatus[2] = 'Pending'

*-- For Design Locations. 
laDsgnLoc[1,1] = 'N/A'
laDsgnLoc[1,2] = ''

*-- Save current alias.
lcCurAlias = ALIAS()

SELECT (lcT_BomVar)
LOCATE

*-- Define flag to know if open codes code file or not.
llOpnCodes = .F.
IF USED('Codes')
  SELECT Codes
  lcOldTag = TAG()
  SET ORDER TO TAG cCode_No
ELSE
  llOpnCodes = gfOpenFile(gcDataDir+'Codes',gcDataDir+'cCode_No','SH')
ENDIF

*-- Define flag to know if open design code file or not.
llOpnDsCd = .F.
IF USED("IcDesign")
  SET ORDER TO TAG AcctDsgn IN IcDesign
ELSE
  *-- Open the design file.
  =gfOpenFile(gcDataDir+'IcDesign',gcDataDir+'AcctDsgn','SH')
  llOpnDsCd  = .T.
ENDIF

*-- Open the thread color file.
*B804365,1 change index on Thread color file to be on Serial instead of Color. [Begin]
*llOpnThClr = gfOpenFile(gcDataDir+'ThreadCl',gcDataDir+'ThreadCl','SH')
llOpnThClr = gfOpenFile(gcDataDir+'ThreadCl',gcDataDir+'ThreadClS','SH')
*B804365,1 change index on Thread color file to be on Serial instead of Color. [End]

*-- Open Thread Color file only once to copy its stucture to the temp Thread color file.
SELECT ThreadCl
IF !USED(lcTmpThClr)
  COPY STRUCTURE TO (gcWorkDir+lcTmpThClr)
  =gfOpenFile(gcWorkDir+lcTmpThClr,'','EX')  

  *B804365,1 Change index to be on serial instead of Color. [Begin] 
  *INDEX ON cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos + cThreadCl TAG (lcTmpThClr)
  INDEX ON cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos + cSerial TAG (lcTmpThClr)
  *B804365,1 Change index to be on serial instead of Color. [End] 
ENDIF

*-- Array hold the folders info.
DECLARE laFoldWinds[2,2]
*-- Adjust & fill the folders array.
laFoldWinds[1,1] = "Embroidery"
laFoldWinds[1,2] = "SOJLINF1"
laFoldWinds[2,1] = "Threads"
laFoldWinds[2,2] = "SOJLINF2"
  
*-- Window parent name for the folder.
lcFoldPrnt  = "SOJLINF"

*-- Current active folder.
lnActFolder = 1
lcFolder    = gfTempName()

*-- Function to control shows after change the folder.
lcWFoldChng = '=lfActFolder()'

*-- Push button name for the next folder.
lcFoldPush  = 'pbFolders'

*-- Position of the folders on the base window.
lnFolderCen = 83.00
lnFolderRen = 2

lnMark = 0                                  && hold the browse record pointer.
llBrowse  = .F.                             && hold the browse variable flag.
lcNewPos  = gcBmpHome + "new1.bmp"          && hold the new button bitmap.
lcRemPos  = gcBmpHome + "rem1.bmp"          && hold the remove button bitmap.
lcClsPos  = gcBmpHome + "Cls.bmp"           && hold the close button bitmap. 
lcBrowBmp = gcBmpHome + "ExtKey.BMP"        && hold the external key bitmap. 
 
*-- Variable hold all the objects status.
lcCurStat = "DISABLE"

*-- Variable hold the browse title.
lcBrwDsgn  = "Designs"
lcBrwColor = "Threads"
lcBrTtl1 = 'Order Lines'
lcBrTtl2 = 'Thread Colors'

ON KEY LABEL ESC DO lfCloseScr
DO (gcScrDir+"SoJLInf.SPX")

*-- Restore lcT_BomVar Tag
SELECT (lcT_BomVar)
SET RELATION TO

*-- If Design code file in this program, close it.
IF llOpnDsCd
  =gfCloseFile("IcDesign")
ENDIF

*-- If Codes file in this program, close it
IF llOpnCodes
  USE IN Codes
ELSE
  SELECT Codes
  SET ORDER TO TAG (lcOldTag)
ENDIF

SELECT (lcCurAlias)             && Restore old alias.
*-- End of Program.

*!*******************************************************************************
*! Name      : lfScrShw
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Run when the screen is first showed.
*!*******************************************************************************
*! Example   : =lfScrShw()
*!*******************************************************************************
*
FUNCTION lfScrShw
=lfActFolder()
*-- End of lfScrShw.

*!*******************************************************************************
*! Name      : lfScrWhen
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Run when the screen is first showed.
*!*******************************************************************************
*! Example   : =lfScrWhen()
*!*******************************************************************************
*
FUNCTION lfScrWhen
IF lnOrdFold = 2 AND !(BOF(lcT_BomVar) OR EOF(lcT_BomVar))
  lnActFolder = 2
ELSE
  lnActFolder = 1
ENDIF
=lfChngFolder(lnActFolder)
*-- End of lfScrWhen.

*!*******************************************************************************
*! Name      : lfActFolder
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Run when switching between folders.
*!*******************************************************************************
*! Example   : =lfActFolder()
*!*******************************************************************************
*
FUNCTION lfActFolder
PRIVATE lcDesgnCod
DO CASE
  CASE lnActFolder = 1
    SHOW GETS WINDOW 'SOJLIN2B' DISABLE ONLY
    SHOW GETS WINDOW 'SOJLIN2C' DISABLE ONLY
    
    SELECT (lcT_BomVar)
    IF EOF(lcT_BomVar)
      =lfbrowDsgn()
    ELSE
      SELECT (lcT_BomVar)
      lcDesgnCod = cDsgnCode
      LOCATE
      =lfbrowDsgn()
      =SEEK(lcDesgnCod+laDsgnLoc[lnDsgnLoc,2])
    ENDIF
    =lfWhenBrow()
  CASE lnActFolder = 2    
    SHOW GETS WINDOW 'SOJLIN1B' DISABLE ONLY

    IF SEEK(lcAccount+lcStore+&lcT_BomVar..cDsgnCode,'IcDesign')
      lcDsgnName = IcDesign.cDsnName
      
      *B804498,1 Get the color change variable. [Begin]
      lcColorChg = IIF(EMPTY(IcDesign.cColorChg),"0",IcDesign.cColorChg)
      *B804498,1 Get the color change variable. [End]
      
    ENDIF  
    SELECT (lcTmpThClr)
    =lfRefresh('SOJLIN2a')
    =lfBrowClr()
    =lfWhClrBrw()
ENDCASE
*-- End of lfActFolder.

*!*******************************************************************************
*! Name      : lfBrowDsgn
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Browse the Design for Embroidery filer.
*!*******************************************************************************
*! Example   : =lfBrowDsgn()
*!*******************************************************************************
*
FUNCTION lfBrowDsgn
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT (lcT_BomVar)
lnMark = RECNO()

*-- Brow the IcDesign file.
*B605059,1 Modify LOOKUP function which gets the design name. [Begin]
*BROWSE FIELDS cMarker = IIF(RECNO()=lnMark,'>',' '):H=' ':R:1, ;
*       cDsgnCode :H='Design Code' :R:14, ; 
*       lcDsnNam=LOOKUP(IcDesign.cDsnName,cDsgnCode,IcDesign.cDsgnCode,'IcDesign') :H='Design Name':R:17, ;
*       lcStylePos = SUBSTR(msizes,1,50) :H='Design Location' :R:18, ;
*       nStitches :H='Stitches' :R:10, ;       
*       lcStatus = IIF(LOOKUP(IcDesign.Status,cDsgnCode,IcDesign.Status,'IcDesign')="A","Approved","Pending") :H='Status' :R:10, ;       
*       cNameDrp1 :H='Name Drop 1' :R:15, ;
*       cNameDrp2 :H='Name Drop 2' :R:15, ;
*       cNameDrp3 :H='Name Drop 3' :R:15 ;
*       WHEN lfWhenBrow() ;
*       VALID :F lfVldBrow();
*       NOEDIT            ;
*       LOCK 0            ;
*       NOMENU            ;
*       NOAPPEND          ;
*       NODELETE          ;
*       NOWAIT            ;
*       SAVE              ;
*	   NOCLEAR           ;
*       WINDOW SOJLIn1a   ;
*       IN WINDOW SOJLINF1 ;
*       TITLE lcBrwDsgn

*B605487,1 MHM 02/24/2002 refrish location in browse window [Start]
*BROWSE FIELDS cMarker = IIF(RECNO()=lnMark,'>',' '):H=' ':R:1, ;
       cDsgnCode :H='Design Code' :R:14, ; 
       lcDsnNam=LOOKUP(IcDesign.cDsnName,lcAccount+lcStore+cDsgnCode,IcDesign.cDsgnCode,'Acctdsgn') :H='Design Name':R:17, ;
       lcStylePos = SUBSTR(msizes,1,50) :H='Design Location' :R:18, ;
       nStitches :H='Stitches' :R:10, ;       
       lcStatus = IIF(LOOKUP(IcDesign.Status,lcAccount+lcStore+cDsgnCode,IcDesign.Status,'Acctdsgn')="A","Approved","Pending") :H='Status' :R:10, ;       
       cNameDrp1 :H='Name Drop 1' :R:15, ;
       cNameDrp2 :H='Name Drop 2' :R:15, ;
       cNameDrp3 :H='Name Drop 3' :R:15 ;
       WHEN lfWhenBrow() ;
       VALID :F lfVldBrow();
       NOEDIT            ;
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WINDOW SOJLIn1a   ;
       IN WINDOW SOJLINF1 ;
       TITLE lcBrwDsgn

*B605989,4 AMH Rename the status field in ICDESIGN file to cdsgstatus [Start]
*BROWSE FIELDS cMarker = IIF(RECNO()=lnMark,'>',' '):H=' ':R:1, ;
       cDsgnCode :H='Design Code' :R:14, ; 
       lcDsnNam=LOOKUP(IcDesign.cDsnName,lcAccount+lcStore+cDsgnCode,IcDesign.cDsgnCode,'Acctdsgn') :H='Design Name':R:17, ;
       cStylePos :H='Design Location' :R:18, ;
       nStitches :H='Stitches' :R:10, ;       
       lcStatus = IIF(LOOKUP(IcDesign.Status,lcAccount+lcStore+cDsgnCode,IcDesign.Status,'Acctdsgn')="A","Approved","Pending") :H='Status' :R:10, ;       
       cNameDrp1 :H='Name Drop 1' :R:15, ;
       cNameDrp2 :H='Name Drop 2' :R:15, ;
       cNameDrp3 :H='Name Drop 3' :R:15 ;
       WHEN lfWhenBrow() ;
       VALID :F lfVldBrow();
       NOEDIT            ;
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WINDOW SOJLIn1a   ;
       IN WINDOW SOJLINF1 ;
       TITLE lcBrwDsgn
BROWSE FIELDS cMarker = IIF(RECNO()=lnMark,'>',' '):H=' ':R:1, ;
       cDsgnCode :H='Design Code' :R:14, ; 
       lcDsnNam=LOOKUP(IcDesign.cDsnName,lcAccount+lcStore+cDsgnCode,IcDesign.cDsgnCode,'Acctdsgn') :H='Design Name':R:17, ;
       cStylePos :H='Design Location' :R:18, ;
       nStitches :H='Stitches' :R:10, ;       
       lcStatus = IIF(LOOKUP(IcDesign.cDsgStatus,lcAccount+lcStore+cDsgnCode,IcDesign.cDsgStatus,'Acctdsgn')="A","Approved","Pending") :H='Status' :R:10, ;       
       cNameDrp1 :H='Name Drop 1' :R:15, ;
       cNameDrp2 :H='Name Drop 2' :R:15, ;
       cNameDrp3 :H='Name Drop 3' :R:15 ;
       WHEN lfWhenBrow() ;
       VALID :F lfVldBrow();
       NOEDIT            ;
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WINDOW SOJLIn1a   ;
       IN WINDOW SOJLINF1 ;
       TITLE lcBrwDsgn
*B605989,4 AMH [End]

*B605487,1 MHM [End]

*B605059,1 Modify LOOKUP function which gets the design name. [End]       
       
SHOW WINDOW (lcBrwDsgn) REFRESH
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfBrowDsgn.

*!*******************************************************************************
*! Name      : lfWhenBrow
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : When for Design browse Embroidery folder.
*!*******************************************************************************
*! Example   : =lfWhenBrow()
*!*******************************************************************************
*
FUNCTION lfWhenBrow
PRIVATE lcAlias
lcAlias = ALIAS()

lnMark   = RECNO(lcT_BomVar)        && Set the browse pointer.     

*-- Flag to know that the active window is the browse.
glFromBrow = .T.

SELECT (lcT_BomVar)
IF BOF() OR EOF()
  lcCurStat = "DISABLE"
  STORE "" TO lcDsgnCode , lcDsgnName , lcNameDrp1 , lcNameDrp2 , lcNameDrp3
  
  *B804498,1 Initialize the color change variable. [Begin]
  lcColorChg = '0'
  *B804498,1 Initialize the color change variable. [End]
  
  DECLARE laDsgnLoc[1,2]  
  laDsgnLoc[1,1] = 'N/A'
  laDsgnLoc[1,2] = ''
  
  STORE 1 TO lnDsgnLoc , lnStatus
  STORE 0 TO lnStitches
ELSE
  lcCurStat = IIF(laScrMode[2] , "DISABLE" , "ENABLE")
  IF EMPTY(&lcT_BomVar..cDsgnCode)
    DECLARE laDsgnLoc[1,2]

    *-- For Design Locations. 
    laDsgnLoc[1,1] = 'N/A'
    laDsgnLoc[1,2] = ''
  ELSE
    IF SEEK(lcAccount+lcStore+&lcT_BomVar..cDsgnCode,'IcDesign')
      lcCurStat = "DISABLE"
    
      DECLARE laDsgnLoc[1,2]
      laDsgnLoc = ''
      IF SEEK("NCSTYLEPOS " + ALLTRIM(&lcT_BomVar..cStylePos),'Codes')
        laDsgnLoc[1,1] = Codes.cDiscrep
        laDsgnLoc[1,2] = ALLTRIM(&lcT_BomVar..cStylePos)       
      ENDIF  
      lnDsgnLoc = 1

      *B605989,4 AMH Rename the status field in ICDESIGN file to cdsgstatus [Start]
      *lnStatus = IIF(IcDesign.Status="A",1,2)
      lnStatus = IIF(IcDesign.cDsgStatus="A",1,2)
      *B605989,4 AMH [End]
      
      lcDsgnName = IcDesign.cDsnName
      
      *B804498,1 Get the color change variable. [Begin]
      lcColorChg = IIF(EMPTY(IcDesign.cColorChg),"0",IcDesign.cColorChg)
      *B804498,1 Get the color change variable. [End]
      
    ENDIF  
  ENDIF  
  lcDsgnCode = &lcT_BomVar..cDsgnCode
  lnStitches = &lcT_BomVar..nStitches
  lcNameDrp1 = &lcT_BomVar..cNameDrp1
  lcNameDrp2 = &lcT_BomVar..cNameDrp2
  lcNameDrp3 = &lcT_BomVar..cNameDrp3    
ENDIF
=lfRefresh()

*-- Refresh the objects on the screen.
SHOW GET ibBrowKey  &lcCurStat
SHOW GET lcDsgnCode &lcCurStat

IF laScrMode[2]
  SHOW GET lcNameDrp1 DISABLE
  SHOW GET lcNameDrp2 DISABLE
  SHOW GET lcNameDrp3 DISABLE
  SHOW GET lnStitches DISABLE
ELSE
  SHOW GET lcNameDrp1 ENABLE
  SHOW GET lcNameDrp2 ENABLE
  SHOW GET lcNameDrp3 ENABLE
  SHOW GET lnStitches ENABLE
ENDIF  

SHOW GET lcDsgnName DISABLE
SHOW GET lnStatus   DISABLE
SHOW GET lnDsgnLoc DISABLE

*B605487,1 MHM 02/24/2002 make location ediatble [Start]
*--- AddMode OR Edit mode
IF (laScrMode[4] .OR. laScrMode[3]) .AND. lcType =  "SO"
  =lfActPop()
ENDIF
*B605487,1 MHM  [End]

IF laScrMode[2]
  SHOW GET pbNewPos DISABLE
ELSE
  IF EOF(lcT_BomVar)
    SHOW GET pbRemPos DISABLE
  ELSE
    SHOW GET pbRemPos ENABLE
  ENDIF
    
  SHOW GET pbNewPos ENABLE
  _CUROBJ = OBJNUM(pbNewPos)
ENDIF

SHOW WINDOW (lcBrwDsgn) REFRESH SAME       && Refresh the browse.
SHOW WINDOW SoJlIn1b REFRESH SAME 
SHOW WINDOW SoJlInf3 REFRESH SAME 
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfWhenBrow.

*!*******************************************************************************
*! Name      : lfVldBrow
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid for Design browse Embroidery folder.
*!*******************************************************************************
*! Example   : =lfVldBrow()
*!*******************************************************************************
*
FUNCTION lfVldBrow

*-- If exist from browse window.
IF !WONTOP(lcBrwDsgn)
  *-- Set flag to say I'm coming from a browse.
  glFromBrow = .T.
  *-- Call global function to stop the browse.
  =gfStopBrow()
ENDIF
*-- End of lfVldBrow.

*!*******************************************************************************
*! Name      : lfActPos
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Activate function for the Design screen.
*!*******************************************************************************
*! Example   : =lfActPos()
*!*******************************************************************************
*
FUNCTION lfActPos

*-- Clear browse as you are coming out of a browse window.
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If none of the screen's browses is active then clear the trapped keys.
IF !INLIST(WONTOP() , lcBrwDsgn , lcBrwColor)
  *-- Clear all the trapped keys.
  ON KEY LABEL CTRL+Q
  ON KEY LABEL CTRL+W
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
ENDIF
RETURN .F.
*-- End of lfActPos.

*!*******************************************************************************
*! Name      : lfDActPos
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Activate function for the Design screen.
*!*******************************************************************************
*! Example   : =lfDActPos()
*!*******************************************************************************
*
FUNCTION lfDActPos

glFromBrow = INLIST(WONTOP() , lcBrwDsgn , lcBrwColor)

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL TAB        DO lpPosTab
  ON KEY LABEL BACKTAB    DO lpPosBTab
ENDIF
RETURN .F.
*-- End of lfDActpos.

*!*******************************************************************************
*! Name      : lpPosTab
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Trap of Tab key for the Design screen.
*!*******************************************************************************
*! Example   : DO lpPosTab
*!*******************************************************************************
*
PROCEDURE lpPosTab
ON KEY LABEL TAB
DO CASE
  CASE lnActFolder = 1
    ACTIVATE WINDOW SoJLIn1b
    _CUROBJ = OBJNUM(pbNewPos)    
  CASE lnActFolder = 2
    ACTIVATE WINDOW SoJLIn2c
    _CUROBJ = OBJNUM(pbAddClr)  
ENDCASE
ON KEY LABEL TAB DO lpPosTab
*-- End of lpPosTab.

*!*******************************************************************************
*! Name      : lpPosBTab
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Trap of backtab key for the Design screen.
*!*******************************************************************************
*! Example   : DO lpPosBTab
*!*******************************************************************************
*
PROCEDURE lpPosBTab
ON KEY LABEL BACKTAB

ACTIVATE WINDOW SoJLInf3
_CUROBJ = OBJNUM(pbClsPos)  

ON KEY LABEL BACKTAB DO lpPosBTab
*-- End of lpPosBTab.

*!*******************************************************************************
*! Name      : lfvNewPos
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid function for the new button.
*!*******************************************************************************
*! Example   : =lfvNewPos()
*!*******************************************************************************
*
FUNCTION lfvNewPos

*-- Add new line in the temp. R/A lines file.
SELECT (lcT_BomVar)
LOCATE FOR EMPTY(cDsgnCode)
IF FOUND()
  BLANK
ELSE
  APPEND BLANK
ENDIF

REPLACE cIdType  WITH lcType , cCost_Id WITH lcOrdNo , LineNo   WITH lnOrdLinNo

*-- Call global function to add the audit fields to the newly added record.
=gfAdd_Info(lcT_BomVar)

*-- Initialize screen variables.
DECLARE laDsgnLoc[1,2]
laDsgnLoc[1,1] = 'N/A'
laDsgnLoc[1,2] = ''

STORE "" TO lcDsgnCode , lcDsgnName , lcNameDrp1 , lcNameDrp2 , lcNameDrp3
STORE 1 TO lnDsgnLoc , lnStatus
STORE 0 TO lnStitches

*B804498,1 Get the color change variable. [Begin]
lcColorChg = '0'
*B804498,1 Get the color change variable. [End]

*-- Refresh the objects on the screen.
SHOW GET ibBrowKey  ENABLE
SHOW GET lcDsgnCode ENABLE
SHOW GET lnStitches ENABLE
SHOW GET lcDsgnName DISABLE
SHOW GET lnStatus   DISABLE
SHOW GET lnDsgnLoc  DISABLE
SHOW GET lcNameDrp1 ENABLE
SHOW GET lcNameDrp2 ENABLE
SHOW GET lcNameDrp3 ENABLE
SHOW GET pbRemPos   ENABLE

IF laScrMode[2]
  SHOW GET pbNewPos DISABLE
ELSE
  SHOW GET pbNewPos ENABLE
ENDIF
_CUROBJ = OBJNUM(lcDsgnCode)
=lfRefresh()
*-- End of lfvNewPos.

*!*******************************************************************************
*! Name      : lfvRemPos
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid function for the Remove button.
*!*******************************************************************************
*! Example   : =lfvRemPos()
*!*******************************************************************************
*
FUNCTION lfvRemPos

*-- Confirm Removing current line.
*-- Message < Are you sure you want to Remove this record ? >
*-- Buttons <                 < Yes > - < No >                >
IF gfModalGen("QRM00002B00006","ALERT","remove") = 1

  *-- Delete all Thread color records.
  SELECT (lcTmpThClr)
  
  *B605571,1 Delete all lines of thread color for the same design. [Begin]
  *DELETE ALL FOR cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos = ;
  *       &lcT_BomVar..cIdType + &lcT_BomVar..cCost_Id + STR(&lcT_BomVar..LineNo,6) + ;
  *       &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos
  DELETE ALL FOR cIdType + cCost_Id + cDsgnCode + cStylePos = ;
         &lcT_BomVar..cIdType + &lcT_BomVar..cCost_Id + ;
         &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos
  *B605571,1 Delete all lines of thread color for the same design. [End]

  *-- Delete the record & change the status according to the current.
  SELECT (lcT_BomVar)
  DELETE
  LOCATE
  
  *-- Call the browse when function.
  =lfWhenBrow()
ENDIF
*-- End of lfvRemPos.

*!*******************************************************************************
*! Name      : lfCloseScr
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Executes when closing the configure screen.
*!*******************************************************************************
*! Example   : =lfCloseScr()
*!*******************************************************************************
*
FUNCTION lfCloseScr

ACTIVATE WINDOW SojlInf3
CLEAR READ
*-- End of lfCloseScr.

*!*******************************************************************************
*! Name      : lfBrowClr
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/06/2001
*! Purpose   : Browse the threads color for specific style. 
*!*******************************************************************************
*! Example   : =lfBrowClr()
*!*******************************************************************************
*
FUNCTION lfBrowClr
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT (lcTmpThClr)
lnMark = RECNO()

*-- Brow the IcDesign file.

*B804365,1 Add the Serial column to the browse. [Begin]
*BROWSE FOR cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos = &lcT_BomVar..cIdType + ;
*           &lcT_BomVar..cCost_Id + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos ;
*       FIELDS cMarker = IIF(RECNO()=lnMark,'>',' '):H=' ':R:1, ;
*       cThreadCl :H='Thread Color' :R:16, ;         
*       lcClrDesc=IIF(SEEK('NCTHREADCL '+cThreadCl,'Codes'),Codes.cDiscRep,'') :H='Thread Color Description' :R:40 ,;
*       cComment1 :H='Notes' :R:40 ;
*       WHEN lfWhClrBrw() ;
*       VALID :F lfVldClrBr();
*       NOEDIT            ;
*       LOCK 0            ;
*       NOMENU            ;
*       NOAPPEND          ;
*       NODELETE          ;
*       NOWAIT            ;
*       SAVE              ;
*	   NOCLEAR           ;
*       WINDOW SOJLIn2b   ;
*       IN WINDOW SOJLINF2 ;
*       TITLE lcBrwColor
BROWSE FOR cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos = &lcT_BomVar..cIdType + ;
           &lcT_BomVar..cCost_Id + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos ;
       FIELDS cMarker = IIF(RECNO()=lnMark,'>',' '):H=' ':R:1, ;
       cSerial   :H='Serial' :R:6, ;
       cThreadCl :H='Thread Color' :R:16, ;         
       lcClrDesc=IIF(SEEK('NCTHREADCL '+cThreadCl,'Codes'),Codes.cDiscRep,'') :H='Thread Color Description' :R:40 ,;
       cComment1 :H='Notes' :R:40 ;
       WHEN lfWhClrBrw() ;
       VALID :F lfVldClrBr();
       NOEDIT            ;
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WINDOW SOJLIn2b   ;
       IN WINDOW SOJLINF2 ;
       TITLE lcBrwColor
*B804365,1 Add the Serial column to the browse. [End]       
       
SHOW WINDOW (lcBrwColor) REFRESH
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfBrowClr.

*!*******************************************************************************
*! Name      : lfWhClrBrw
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/06/2001
*! Purpose   : when function for browse Thread colors. 
*!*******************************************************************************
*! Example   : =lfWhClrBrw()
*!*******************************************************************************
*
FUNCTION lfWhClrBrw
PRIVATE lcAlias
lcAlias = ALIAS()

*-- Set the browse pointer.
lnMark   = RECNO(lcTmpThClr)

*-- Flag to know that the active window is the browse.
glFromBrow = .T.

SELECT (lcTmpThClr)
IF SEEK(lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos)
  
  *B804365,1 Get last record number. [Begin]
  SET ORDER TO TAG (lcTmpThClr) DESCENDING
  IF SEEK(lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos)
    lnTmpRecNo = VAL(&lcTmpThClr..cSerial)
  ENDIF
  SET ORDER TO TAG (lcTmpThClr) ASCENDING
  *B804365,1 Get last record number. [End]

  *B605059,1 Check if EOF() OR BOF(). [Begin]
  *GOTO lnMark
  IF BETWEEN( lnMark, 1, RECCOUNT()) 
    GOTO lnMark
  ENDIF  
  *B605059,1 Check if EOF() OR BOF(). [End]
  
  IF laScrMode[2]
    SHOW GET pbRemClr DISABLE
    SHOW GET pbRemALL DISABLE
  ELSE
    SHOW GET pbRemClr ENABLE
    SHOW GET pbRemALL ENABLE    
  ENDIF  
  
  lcThredClr = &lcTmpThClr..cThreadCl
  lcClrDesc  = IIF(SEEK('NCTHREADCL ' + lcThredClr,'Codes'),Codes.cDiscrep,'')
  lcClrNotes = &lcTmpThClr..cComment1
ELSE
  STORE '' TO lcThredClr , lcClrDesc , lcClrNotes
  
  *B804365,1 Reset Record number to 1 [Begin]
  lnTmpRecNo = 0
  *B804365,1 Reset Record number to 1 [End]

  SHOW GET pbRemClr DISABLE
  SHOW GET pbRemALL DISABLE  
ENDIF

IF EMPTY(lcThredClr)
  IF SEEK(lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos)
    GOTO lnMark
    
    *B804439,1 Disable in Edit mode. [Begin]
    IF laScrMode[2]
      SHOW GET ibClrKey DISABLE       
      SHOW GET lcThredClr DISABLE
    ELSE
      SHOW GET ibClrKey ENABLE       
      SHOW GET lcThredClr ENABLE
    ENDIF
    *B804439,1 Disable in Edit mode. [End]

    SHOW GET lcClrDesc ENABLE
    SHOW GET lcClrNotes ENABLE
  ELSE
    SHOW GET ibClrKey DISABLE    
    SHOW GET lcThredClr DISABLE
    SHOW GET lcClrDesc DISABLE
    SHOW GET lcClrNotes DISABLE
  ENDIF
ELSE
  *B804439,1 Always enable thread color code. [Begin]
  *SHOW GET ibClrKey DISABLE
  *SHOW GET lcThredClr DISABLE
  IF laScrMode[2]
    SHOW GET ibClrKey DISABLE       
    SHOW GET lcThredClr DISABLE
  ELSE
    SHOW GET ibClrKey ENABLE       
    SHOW GET lcThredClr ENABLE
  ENDIF
  *B804439,1 Always enable thread color code. [End]
  
  SHOW GET lcClrDesc DISABLE
  SHOW GET lcClrNotes ENABLE
ENDIF  

IF EOF(lcT_BomVar) OR EMPTY(lcStyle)
  SHOW GET pbAsPrevs DISABLE
  SHOW GET pbAddClr  DISABLE
ELSE
  IF EMPTY(&lcT_BomVar..cDsgnCode)
    SHOW GET pbAsPrevs DISABLE
    SHOW GET pbAddClr  DISABLE
  ELSE
    IF laScrMode[2]
      SHOW GET pbAsPrevs DISABLE
      SHOW GET pbAddClr  DISABLE
    ELSE
      *C102357,4 AMH Disable pbasprevs in case of comming from PO [Start]
      *SHOW GET pbAsPrevs ENABLE
      IF llFromPo
        SHOW GET pbAsPrevs DISABLE
      ELSE
        SHOW GET pbAsPrevs ENABLE
      ENDIF
      *C102357,4 AMH [End]
      SHOW GET pbAddClr  ENABLE
    ENDIF  
  ENDIF  
ENDIF

SHOW WINDOW (lcBrwColor) REFRESH SAME       && Refresh the browse.

IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfWhClrBrw.

*!*******************************************************************************
*! Name      : lfVldClrBr
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/06/2001
*! Purpose   : Valid function for browse Thread colors. 
*!*******************************************************************************
*! Example   : =lfVldClrBr()
*!*******************************************************************************
*
FUNCTION lfVldClrBr

*-- If exist from browse window.
IF !WONTOP(lcBrwColor)
  *-- Set flag to say I'm coming from a browse.
  glFromBrow = .T.
  *-- Call global function to stop the browse.
  =gfStopBrow()
ENDIF
*-- End of lfVldClrBr.

*!*******************************************************************************
*! Name      : lfvDsgnCod
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid the design code and browse from table icdesign.
*!*******************************************************************************
*! Example   : =lfvDsgnCod()
*!*******************************************************************************
*
FUNCTION lfvDsgnCod
PRIVATE lcBrFields , lcAlias , lcField , llAddStats
llAddStats = .F.        && Variable flag to check for adding Design Code line to (lcT_BomVar)

lcAlias = ALIAS()

DECLARE laTmpDat[1]
laTmpDat[1] = ' '

IF (!llBrowse AND EMPTY(lcDsgnCode)) OR MDOWN()
  RETURN .T.
ENDIF

SELECT IcDesign
*C102357,1 AMH if coming from Po [Start]
IF llFromPo
  lcOrder = SET('ORDER')
  SET ORDER TO TAG ICDESIGN
ENDIF
*C102357,1 AMH [End]
IF !llBrowse AND lcDsgnCode <> '?'
  *C102357,1 AMH if coming from Po [Start]
  *SEEK lcAccount + lcStore + lcDsgnCode 
  IF llFromPo
    SEEK lcDsgnCode 
  ELSE
    SEEK lcAccount + lcStore + lcDsgnCode 
  ENDIF
  *C102357,1 AMH [End]
  IF FOUND()
    *-- Message < 'Design Code ' + lcDsgnCode + ' has not been approved yet. Are you sure you want to proceed. ?' >
    *-- Buttons <                                        < Yes > - < No >                                         >
    
    *B605989,4 AMH Rename the status field in ICDESIGN file to cdsgstatus [Start]
    *IF IcDesign.Status = 'A' OR (IcDesign.Status = 'P' AND gfModalGen("QRM00000B00006","ALERT",'','','Design Code ' + ALLTRIM(lcDsgnCode) + ' has not been approved yet. Are you sure you want to proceed. ?') = 1)
    IF IcDesign.cDsgStatus = 'A' OR (IcDesign.cDsgStatus = 'P' AND gfModalGen("QRM00000B00006","ALERT",'','','Design Code ' + ALLTRIM(lcDsgnCode) + ' has not been approved yet. Are you sure you want to proceed. ?') = 1)
    *B605989,4 AMH [End]
    
      llAddStats = .T.
      SHOW GET lcDsgnCode DISABLE
      SHOW GET lnDsgnLoc ENABLE
    
      PRIVATE lnLoop , lcLoop , lnPosNum , lcKeyVal
      lnPosNum = 0
      FOR lnLoop = 6 TO 1 STEP -1
        lcLoop = ALLTRIM(STR(lnLoop,6))
        IF !EMPTY(IcDesign.cStylePos&lcLoop)
          lnPosNum = lnLoop
          EXIT
        ENDIF  
      ENDFOR
      
      IF lnPosNum > 0
        PRIVATE lnTotalPos 
        lnTotalPos = lnPosNum

        FOR lnLoop = 1 TO lnPosNum
          lcLoop = ALLTRIM(STR(lnLoop,6))
          IF SEEK(lcDsgnCode + ALLTRIM(IcDesign.cStylePos&lcLoop),(lcT_BomVar))
            lnTotalPos = lnTotalPos - 1
          ENDIF
        ENDFOR

        IF lnTotalPos > 0                
          DECLARE laDsgnLoc[lnTotalPos,2]
          laDsgnLoc = ''
          
          PRIVATE lnNumber
          lnNumber = 0
          FOR lnLoop = 1 TO lnPosNum
            lcLoop = ALLTRIM(STR(lnLoop,6))
            IF SEEK("NCSTYLEPOS " + ALLTRIM(IcDesign.cStylePos&lcLoop),'Codes') AND ;
               !SEEK(lcDsgnCode + ALLTRIM(IcDesign.cStylePos&lcLoop),(lcT_BomVar))
              lnNumber = lnNumber + 1 
              laDsgnLoc[lnNumber,1] = Codes.cDiscrep
              laDsgnLoc[lnNumber,2] = ALLTRIM(IcDesign.cStylePos&lcLoop)       
            ENDIF  
          ENDFOR      
          lnDsgnLoc = 1

          *B605989,4 AMH Rename the status field in ICDESIGN file to cdsgstatus [Start]
          *lnStatus = IIF(IcDesign.Status="A",1,2)
          lnStatus = IIF(IcDesign.cDsgStatus="A",1,2)
          *B605989,4 AMH [End]
          
          lcDsgnName = IcDesign.cDsnName
          lnStitches = IcDesign.nStitches
          lcDsgnCode = IcDesign.cDsgnCode
          lcNameDrp1 = IcDesign.cNameDrp1
          lcNameDrp2 = IcDesign.cNameDrp2
          lcNameDrp3 = IcDesign.cNameDrp3
          
          *B804498,1 Get the color change variable. [Begin]
          lcColorChg = IIF(EMPTY(IcDesign.cColorChg),"0",IcDesign.cColorChg)
          *B804498,1 Get the color change variable. [End]
          
          =lfRefresh('SoJLIn1b')
          SHOW GETS WINDOW 'SoJLIn1b' ONLY
        ELSE
          *-- Message < There are no more design location for Design Code XXXXXX>
          *-- Buttons <                         OK                         >
          =gfModalGen("QRM00000B00000","ALERT",'','','There are no more design location for Design Code ' + lcDsgnCode)
          
          llAddStats = .F.
          lcDsgnCode = ''
          SHOW GET lcDsgnCode ENABLE
          _CUROBJ = OBJNUM(lcDsgnCode)
          SHOW GET lnDsgnLoc DISABLE
        ENDIF
      ELSE
        *-- Message < There are no more design location for Design Code XXXXXX>
        *-- Buttons <                         OK                         >
        =gfModalGen("QRM00000B00000","ALERT",'','','There are no more design location for Design Code ' + lcDsgnCode)

        llAddStats = .F.
        lcDsgnCode = ''
        SHOW GET lcDsgnCode ENABLE
        _CUROBJ = OBJNUM(lcDsgnCode)
        SHOW GET lnDsgnLoc DISABLE 
      ENDIF  
    ELSE
      llAddStats = .F.
      lcDsgnCode = ''
      SHOW GET lcDsgnCode ENABLE
      _CUROBJ = OBJNUM(lcDsgnCode)
      SHOW GET lnDsgnLoc DISABLE 
    ENDIF  
  ELSE
    SHOW GET lcDsgnCode ENABLE
    SHOW GET lnDsgnLoc DISABLE
    llBrowse = .T.
  ENDIF
ENDIF
IF llBrowse OR lcDsgnCode = '?'
  *C102357,1 AMH conseder case of adornment order [Start]
  *IF SEEK(lcAccount+lcStore)
  IF SEEK(lcAccount+lcStore) .OR. llFromPo
  *C102357,1 AMH [End]
    lcField     = 'cDsgnCode'
    lcBrFields  = "cDsgnCode :12 :H='Design Code',"+;
                  "cDsnName :40 :H='Design Name',"+;
                  "Royalty   :12 :H='Royality',"+;
                  "nRoyltAmt :20 :H='Royality Amount',"+;
                  "nDsgnCst  :20 :H='Design Adornment Cost',"+;
                  "nDsgnPrc  :20 :H='Design Adornment Price',"+;
                  "cStockDsn :12 :H='Stock Design Code',"+;
                  "nStitches :20 :H='Number of Stitches',"+;
                  "cNameDrp1 :40 :H='Name Drop One',"+;
                  "cNameDrp2 :40 :H='Name Drop Two',"+;
                  "cNameDrp3 :40 :H='Name Drop Three'"
    *C102357,1 AMH if coming form Po [Start]
    *=gfBrows('FOR Account+Store+cDsgnCode = lcAccount+lcStore',lcField,"laTmpDat",'Design Code','','',.F.)
    IF llFromPo
      =gfBrows(' ',lcField,"laTmpDat",'Design Code','','',.F.)
    ELSE
      
      *B605989,4 AMH Rename the account field in ICDESIGN file to cdsgnacct [Start]
      *=gfBrows('FOR Account+Store+cDsgnCode = lcAccount+lcStore',lcField,"laTmpDat",'Design Code','','',.F.)
      =gfBrows('FOR cDsgnAcct+SkuStore+cDsgnCode = lcAccount+lcStore',lcField,"laTmpDat",'Design Code','','',.F.)
      *B605989,4 AMH [End]
      
    ENDIF
    *C102357,1 AMH [End]

    IF EMPTY(laTmpDat[1])
      lcDsgnCode = ''
      SHOW GET lcDsgnCode ENABLE
      _CUROBJ = OBJNUM(lcDsgnCode)
      SHOW GET lnDsgnLoc DISABLE
    ELSE
      *C102357,1 AMH if coming from Po [Start]
      *=SEEK(lcAccount + lcStore + laTmpDat[1],'IcDesign')
      IF llFromPo
        =SEEK(laTmpDat[1],'IcDesign')
      ELSE
        =SEEK(lcAccount + lcStore + laTmpDat[1],'IcDesign')
      ENDIF
      *C102357,1 AMH [End]
      *-- Message < 'Design Code ' + lcDsgnCode + ' has not been approved yet. Are you sure you want to proceed. ?' >
      *-- Buttons <                                        < Yes > - < No >                                         >
      
      *B605989,4 AMH Rename the status field in ICDESIGN file to cdsgstatus [Start]
      *IF IcDesign.Status = 'A' OR (IcDesign.Status = 'P' AND gfModalGen("QRM00000B00006","ALERT",'','','Design Code ' + ALLTRIM(lcDsgnCode) + ' has not been approved yet. Are you sure you want to proceed. ?') = 1)
      IF IcDesign.cDsgStatus = 'A' OR (IcDesign.cDsgStatus = 'P' AND gfModalGen("QRM00000B00006","ALERT",'','','Design Code ' + ALLTRIM(lcDsgnCode) + ' has not been approved yet. Are you sure you want to proceed. ?') = 1)
      *B605989,4 AMH [End]
      
        llAddStats = .T.
        
        lcDsgnCode = laTmpDat[1]
        SHOW GET lcDsgnCode DISABLE
        SHOW GET lnDsgnLoc ENABLE
        
        *C102357,1 AMH if coming from Po [Start]
        *IF SEEK(lcAccount+lcStore+lcDsgnCode,'IcDesign')
        IF IIF(llFromPo,SEEK(lcDsgnCode,'IcDesign'),SEEK(lcAccount+lcStore+lcDsgnCode,'IcDesign'))
        *C102357,1 AMH [End]

          PRIVATE lnLoop , lcLoop , lnPosNum , lcKeyVal
          lnPosNum = 0
          FOR lnLoop = 6 TO 1 STEP -1
            lcLoop = ALLTRIM(STR(lnLoop,6))
            IF !EMPTY(IcDesign.cStylePos&lcLoop)
              lnPosNum = lnLoop
              EXIT
            ENDIF  
          ENDFOR
      
          IF lnPosNum > 0
            PRIVATE lnTotalPos 
            lnTotalPos = lnPosNum

            FOR lnLoop = 1 TO lnPosNum
              lcLoop = ALLTRIM(STR(lnLoop,6))
              IF SEEK(lcDsgnCode + ALLTRIM(IcDesign.cStylePos&lcLoop),(lcT_BomVar))
                lnTotalPos = lnTotalPos - 1
              ENDIF
            ENDFOR

            IF lnTotalPos > 0                
              DECLARE laDsgnLoc[lnTotalPos,2]
              laDsgnLoc = ''
          
              PRIVATE lnNumber
              lnNumber = 0
              FOR lnLoop = 1 TO lnPosNum
                lcLoop = ALLTRIM(STR(lnLoop,6))
                IF SEEK("NCSTYLEPOS " + ALLTRIM(IcDesign.cStylePos&lcLoop),'Codes') AND ;
                  !SEEK(lcDsgnCode + ALLTRIM(IcDesign.cStylePos&lcLoop),(lcT_BomVar))
                  lnNumber = lnNumber + 1 
                  laDsgnLoc[lnNumber,1] = Codes.cDiscrep
                  laDsgnLoc[lnNumber,2] = ALLTRIM(IcDesign.cStylePos&lcLoop)       
                ENDIF  
              ENDFOR      
              lnDsgnLoc = 1

              *B605989,4 AMH Rename the status field in ICDESIGN file to cdsgstatus [Start]
              *lnStatus = IIF(IcDesign.Status="A",1,2)
              lnStatus = IIF(IcDesign.cDsgStatus="A",1,2)
              *B605989,4 AMH [End]
              
              lcDsgnName = IcDesign.cDsnName
              lnStitches = IcDesign.nStitches
              lcDsgnCode = IcDesign.cDsgnCode
              lcNameDrp1 = IcDesign.cNameDrp1
              lcNameDrp2 = IcDesign.cNameDrp2
              lcNameDrp3 = IcDesign.cNameDrp3

              *B804498,1 Get the color change variable. [Begin]
              lcColorChg = IIF(EMPTY(IcDesign.cColorChg),"0",IcDesign.cColorChg)
              *B804498,1 Get the color change variable. [End]
              
              =lfRefresh('SoJLIn1b')
              SHOW GETS WINDOW 'SoJLIn1b' ONLY
            ELSE
              *-- Message < There are no more design location for Design Code XXXXXX>
              *-- Buttons <                         OK                         >
              =gfModalGen("QRM00000B00000","ALERT",'','','There are no more design location for Design Code ' + lcDsgnCode)
            
              llAddStats = .F.
              lcDsgnCode = ''
              SHOW GET lcDsgnCode ENABLE
              _CUROBJ = OBJNUM(lcDsgnCode)
              SHOW GET lnDsgnLoc DISABLE
            ENDIF
          ELSE
            *-- Message < There are no more design location for Design Code XXXXXX>
            *-- Buttons <                         OK                         >
            =gfModalGen("QRM00000B00000","ALERT",'','','There are no more design location for Design Code ' + lcDsgnCode)

            llAddStats = .F.
            lcDsgnCode = ''
            SHOW GET lcDsgnCode ENABLE
            _CUROBJ = OBJNUM(lcDsgnCode)
            SHOW GET lnDsgnLoc DISABLE 
          ENDIF  

        ENDIF
      
      ELSE
        llAddStats = .F.
        lcDsgnCode = ''
        SHOW GET lcDsgnCode ENABLE
        _CUROBJ = OBJNUM(lcDsgnCode)
        SHOW GET lnDsgnLoc DISABLE 
      ENDIF  
    ENDIF  
  ELSE
    IF EMPTY(lcAccount)
      = gfModalGen("TRM00000B00000","DIALOG",'','','No Design Code found.')    
    ELSE
      = gfModalGen("TRM00000B00000","DIALOG",'','','No Design Code found for this Account , Store.')    
    ENDIF  
    llAddStats = .F.
    lcDsgnCode = ''
    SHOW GET lcDsgnCode ENABLE    
    _CUROBJ = OBJNUM(lcDsgnCode)
    SHOW GET lnDsgnLoc DISABLE
  ENDIF  
ENDIF
llBrowse = .F.

*C102357,1 AMH if coming from PO [Start]
*IF !EMPTY(lcDsgnCode) AND SEEK(lcAccount+lcStore+lcDsgnCode,'IcDesign') AND llAddStats
IF !EMPTY(lcDsgnCode) AND IIF(llFromPo,SEEK(lcDsgnCode,'IcDesign'),;
   SEEK(lcAccount+lcStore+lcDsgnCode,'IcDesign')) AND llAddStats
*C102357,1 AMH [End]
  SELECT (lcT_BomVar)
  LOCATE FOR EMPTY(cDsgnCode) 
  REPLACE cDsgnCode WITH lcDsgnCode , cNameDrp1 WITH lcNameDrp1 , ;
          cNameDrp2 WITH lcNameDrp2 , cNameDrp3 WITH lcNameDrp3 , ;
          cStylePos WITH laDsgnLoc[1,2] , mSizes WITH laDsgnLoc[1,1] , ;
          nStitches WITH IcDesign.nStitches
  LOCATE        
  SHOW WINDOW (lcBrwDsgn) REFRESH SAME
  =SEEK(lcDsgnCode+laDsgnLoc[1,2])
ENDIF

*C102357,1 AMH if coming from Po [Start]
IF llFromPo
  SELECT ICDESIGN
  SET ORDER TO &lcOrder.
ENDIF
*C102357,1 AMH [End]

IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfvDsgnCod.

*!*******************************************************************************
*! Name      : lfvThrdClr
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid for Thread color codes.
*!*******************************************************************************
*! Example   : =lfvThrdClr()
*!*******************************************************************************
*
FUNCTION lfvThrdClr
PRIVATE lcAlias , lnMessage , lcSerialNo
lcAlias = ALIAS()
lnMessage = 0
DECLARE laTmpDat[1]
laTmpDat[1] = ' '

IF (!llBrowse AND EMPTY(lcThredClr)) OR MDOWN()
  RETURN .T.
ENDIF
SELECT Codes

IF !llBrowse AND lcThredClr <> '?'
  IF SEEK("NCTHREADCL " + lcThredClr,"Codes")
    
    *B804365,1 Don't check existing thread colors. [Begin] 
    *IF SEEK(lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos + lcThredClr,(lcTmpThClr))
    *  = gfModalGen("TRM00000B00000","DIALOG",'','','Thread color code already exists.')    
    *  lcThredClr = ''
    *  SHOW GET lcThredClr ENABLE      
    *  _CUROBJ = OBJNUM(lcThredClr)
    *ELSE
    *B804365,1 Don't check existing thread colors. [End] 
          
      lcThredClr = Codes.cCode_No
      lcClrDesc = Codes.cDiscRep
      
      *B804439,1 Always enable thread color code. [Begin]
      *SHOW GET lcThredClr DISABLE
      IF laScrMode[2]
        SHOW GET ibClrKey DISABLE       
        SHOW GET lcThredClr DISABLE
      ELSE
        SHOW GET ibClrKey ENABLE       
        SHOW GET lcThredClr ENABLE
      ENDIF
      *B804439,1 Always enable thread color code. [End]
      
      SHOW GET lcClrDesc DISABLE
      =lfRefresh('SoJLIn2c')

    *B804365,1 Don't check existing thread colors. [Begin] 
    *ENDIF
    *B804365,1 Don't check existing thread colors. [End]
      
  ELSE
    lnMessage = gfModalGen("TRM00001B00001","DIALOG","Thread Color Code")
    DO CASE
      CASE lnMessage = 1
        llBrowse = .T.
      CASE lnMessage = 2
        SHOW GET lcThredClr ENABLE
        
        SELECT Codes
        IF !SEEK("DCTHREADCL ","Codes")
          APPEND BLANK
          =gfAdd_Info()                     && Add the add user,time and date
          =gfObj_Lock(.T.)                  && Lock the record after saving
          REPLACE cDefCode WITH "D" , cFld_Name WITH "CTHREADCL" , cCode_No WITH lcThredClr ,;
                  cDiscrep WITH lcClrDesc , lRltFields WITH .F. , cRltField WITH "N"             
          =gfObj_Lock(.F.)                  && Unlock the record after saving. 
        ENDIF         
        
        APPEND BLANK
        =gfAdd_Info('Codes')
        =gfObj_Lock(.T.)                  && Lock the record after saving
        REPLACE cDefCode WITH "N" , cFld_Name WITH "CTHREADCL" , cCode_No WITH lcThredClr ,;
                lRltFields WITH .F. , cRltField WITH "N"
        =gfObj_Lock(.F.)                  && Unlock the record after saving. 

        *B804439,1 Always enable thread color code. [Begin]
        *SHOW GET lcThredClr DISABLE
        IF laScrMode[2]
          SHOW GET ibClrKey DISABLE       
          SHOW GET lcThredClr DISABLE
        ELSE
          SHOW GET ibClrKey ENABLE       
          SHOW GET lcThredClr ENABLE
        ENDIF
        *B804439,1 Always enable thread color code. [End]
        
        SHOW GET lcClrDesc ENABLE
      CASE lnMessage = 3
         lcThredClr = ''
         SHOW GET lcThredClr ENABLE     
        _CUROBJ = OBJNUM(lcThredClr)        
    ENDCASE
  ENDIF
ENDIF
IF llBrowse OR lcThredClr = '?'
  IF SEEK("NCTHREADCL","Codes")
    lcField     = 'cCode_No'
    lcBrFields  = "cCode_No :20 :H='Thread Color Code',"+;
                  "cDiscRep :40 :H='Thread Color Description'"
    =gfBrows([FOR cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'NCTHREADCL'],lcField,"laTmpDat",'Thread Color Code','','',.F.)                  
    IF EMPTY(laTmpDat[1])
      lcThredClr = ''
      SHOW GET lcThredClr ENABLE      
      _CUROBJ = OBJNUM(lcThredClr)
    ELSE

      *B804365,1 Don't check existing thread colors. [Begin] 
      *IF SEEK(lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos + laTmpDat[1],(lcTmpThClr)) 
      *  = gfModalGen("TRM00000B00000","DIALOG",'','','Thread color code already exists.')
      *  lcThredClr = ''
      *  SHOW GET lcThredClr ENABLE      
      *  _CUROBJ = OBJNUM(lcThredClr)        
      *ELSE
      *B804365,1 Don't check existing thread colors. [End] 

        lcThredClr = laTmpDat[1]
        lcClrDesc = IIF(SEEK("NCTHREADCL "+lcThredClr),Codes.cDiscRep,'')
        
        *B804439,1 Always enable thread color code. [Begin]
        *SHOW GET lcThredClr DISABLE
        IF laScrMode[2]
          SHOW GET ibClrKey DISABLE       
          SHOW GET lcThredClr DISABLE
        ELSE
          SHOW GET ibClrKey ENABLE       
          SHOW GET lcThredClr ENABLE
        ENDIF
        *B804439,1 Always enable thread color code. [End]
        
        SHOW GET lcClrDesc DISABLE
      
        =lfRefresh('SoJLIn2c')
        *SHOW GETS WINDOW 'SoJLIn2c' ONLY

      *B804365,1 Don't check existing thread colors. [Begin] 
      *ENDIF  
      *B804365,1 Don't check existing thread colors. [End]
            
    ENDIF  
  ELSE
    = gfModalGen("TRM00000B00000","DIALOG",'','','No Thread Color Code found.')
    lcThredClr = ''
    _CUROBJ = OBJNUM(lcThredClr)
  ENDIF  
ENDIF
llBrowse = .F.

*B804365,1 Store Record number. [Begin] 
SELECT (lcTmpThClr)

*B804439,1 Get the current serial from Temp thread color. [Begin]
lcSerialNo = &lcTmpThClr..cSerial
*B804439,1 Get the current serial from Temp thread color. [End]

LOCATE FOR EMPTY(cThreadCl)
IF FOUND()
  lcSerialNo = &lcTmpThClr..cSerial

*B804439,1 No need for the ELSE. [Begin]
*ELSE
*  lcSerialNo = PADL(ALLTRIM(STR(1,4)),4,'0')
*B804439,1 No need for the ELSE. [End]

ENDIF  
*B804365,1 Store Record number. [End] 

*B804365,1 Store Record number. [Begin] 
*IF !EMPTY(lcThredClr) AND SEEK(lcType + lcOrdNo + STR(lnOrdLinNo,6)+&lcT_BomVar..cDsgnCode+&lcT_BomVar..cStylePos,(lcTmpThClr))
IF !EMPTY(lcThredClr) AND SEEK(lcType + lcOrdNo + STR(lnOrdLinNo,6)+&lcT_BomVar..cDsgnCode+&lcT_BomVar..cStylePos+lcSerialNo,(lcTmpThClr))
*B804365,1 Store Record number. [End] 

  SELECT (lcTmpThClr)
  REPLACE cThreadCl WITH lcThredClr , cComment1 WITH lcClrNotes  
  LOCATE
  PRIVATE lcThredKey
  
  *B804365,1 Modify the current key to seek on Recno instead of Thread color. [Begin]
  *lcThredKey = lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos + lcThredClr
  lcThredKey = lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos + lcSerialNo
  *B804365,1 Modify the current key to seek on Recno instead of Thread color. [End]

  SHOW WINDOW (lcBrwColor) REFRESH SAME
  =SEEK(lcThredKey)
ENDIF  
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfvThrdClr.

*!*******************************************************************************
*! Name      : lfvAsPrevs
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid Push button pbAsPrevs in Thread folder.
*!*******************************************************************************
*! Example   : =lfvAsPrevs()
*!*******************************************************************************
*
FUNCTION lfvAsPrevs
PRIVATE lcAlias , lnOrdHdrNo , lnOrdLnNum , lnThreadNo , lcOrdLnTag
lcAlias = ALIAS()

SELECT OrdHdr
lnOrdHdrNo = RECNO()

SELECT ThreadCl
lnThreadNo = RECNO()

IF !USED(lcTmpColor)
  COPY STRUCTURE TO (gcWorkDir+lcTmpColor)
  =gfOpenFile(gcWorkDir+lcTmpColor,'','EX')
  
  *B804365,1 Change index to be on Serial instead of Color. [Begin]
  *INDEX ON cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos + cThreadCl TAG (lcTmpColor)
  INDEX ON cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos + cSerial TAG (lcTmpColor)
  *B804365,1 Change index to be on Serial instead of Color. [End]
ENDIF  
SELECT (lcTmpColor)  
ZAP

SELECT OrdLine
lcOrdLnTag = TAG()
SET ORDER TO TAG OrdLines
lnOrdLnNum = RECNO()
SET RELATION TO OrdLine.cOrdType + OrdLine.Order INTO OrdHdr ADDITIVE 
SET RELATION TO lcType + OrdLine.Order + STR(OrdLine.LineNo,6) INTO (lcTmpColor) ADDITIVE

SELECT OrdLine
IF SEEK(lcStyle)
  SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcStyle
    IF SEEK(lcType+OrdLine.Order+STR(OrdLine.LineNo,6)+&lcT_BomVar..cDsgnCode+&lcT_BomVar..cStylePos,'ThreadCl')
      SELECT ThreadCl

      *B804365,1 Scan rest on Serial instead of Thread Color. [Begin]
      *SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)+cdsgncode+cstylepos+cthreadcl = ; 
      *                lcType+OrdLine.Order+STR(OrdLine.LineNo,6)+&lcT_BomVar..cDsgnCode+&lcT_BomVar..cStylePos
      SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)+cdsgncode+cstylepos+cSerial = ; 
                      lcType+OrdLine.Order+STR(OrdLine.LineNo,6)+&lcT_BomVar..cDsgnCode+&lcT_BomVar..cStylePos
      *B804365,1 Scan rest on Serial instead of Thread Color. [End]
      
        SCATTER MEMVAR MEMO
        INSERT INTO (lcTmpColor) FROM MEMVAR
      ENDSCAN    
    ENDIF
  ENDSCAN  
ENDIF  
LOCATE

PUSH KEY
ON KEY LABEL ESC CLEAR READ
DO (gcScrDir+"SoJLPrv.SPX")
POP KEY

SELECT OrdHdr
SET RELATION TO
IF BETWEEN(lnOrdHdrNo,1,RECCOUNT())
  GOTO lnOrdHdrNo
ENDIF  

SELECT OrdLine
SET RELATION TO
SET ORDER TO TAG (lcOrdLnTag)
IF BETWEEN(lnOrdLnNum,1,RECCOUNT())
  GOTO lnOrdLnNum
ENDIF  

SELECT ThreadCl
IF BETWEEN(lnThreadNo,1,RECCOUNT())
  GOTO lnThreadNo
ENDIF  

IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfvAsPrevs.

*!*******************************************************************************
*! Name      : lfvSelPrvs
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid for the push button pbSelect in Previous Orders screen.
*!*******************************************************************************
*! Example   : =lfvSelPrvs()
*!*******************************************************************************
*
FUNCTION lfvSelPrvs
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT (lcTmpColor)
IF SEEK(lcType+OrdLine.Order+STR(OrdLine.LineNo,6)+&lcT_BomVar..cDsgnCode+&lcT_BomVar..cStylePos)

  *B804365,1 change scan rest to be on Serial instead of Thread Color. [Begin]
  *SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)+cdsgncode+cstylepos+cthreadcl = ;
  *                lcType+OrdLine.Order+STR(OrdLine.LineNo,6)+&lcT_BomVar..cDsgnCode+&lcT_BomVar..cStylePos
  SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)+cdsgncode+cstylepos+cSerial = ;
                  lcType+OrdLine.Order+STR(OrdLine.LineNo,6)+&lcT_BomVar..cDsgnCode+&lcT_BomVar..cStylePos
  *B804365,1 change scan rest to be on Serial instead of Thread Color. [End]
    
    *B804365,1 No need for this seek since we add duplicate thread colors. [Begin]
    *IF !SEEK(lcType+lcOrdNo+STR(lnOrdLinNo,6)+cDsgnCode+cStylePos+cThreadCl,(lcTmpThClr))
    *B804365,1 No need for this seek since we add duplicate thread colors. [End]

      SCATTER MEMVAR MEMO
      m.cCost_ID = lcOrdNo
      m.LineNo   = lnOrdLinNo
      
      *B804365,1 Add Serial to file. [Begin]
      lnTmpRecNo = lnTmpRecNo + 1
      m.cSerial = PADL(ALLTRIM(STR(lnTmpRecNo,4)),4,'0')
      *B804365,1 Add Serial to file. [End]
      
      INSERT INTO (lcTmpThClr) FROM MEMVAR
    
    *B804365,1 No need for this seek since we add duplicate thread colors. [Begin]
    *ENDIF      
    *B804365,1 No need for this seek since we add duplicate thread colors. [End]
  ENDSCAN
ENDIF

CLEAR READ
=lfActFolder()
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfvSelPrvs.

*!*******************************************************************************
*! Name      : lfvRemClr
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid Push button PbRemClr in Thread folder.
*!*******************************************************************************
*! Example   : =lfvRemClr()
*!*******************************************************************************
*
FUNCTION lfvRemClr
*-- Confirm Removing current line.
*-- Message < Are you sure you want to Remove this record ? >
*-- Buttons <                 < Yes > - < No >                >
IF gfModalGen("QRM00002B00006","ALERT","remove") = 1

  *-- Delete the record & change the status according to the current.
  SELECT (lcTmpThClr)
  DELETE
  LOCATE
  
  *B605059,1 Call Procedure to re-serialize the temp thread color. [Begin]
  DO lpReSerial
  *B605059,1 Call Procedure to re-serialize the temp thread color. [End]

  *-- Call the browse when function.
  =lfWhClrBrw()
ENDIF
*-- End of lfvRemClr.

*!*******************************************************************************
*! Name      : lfvClrDesc
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid for the Color Description field.
*!*******************************************************************************
*! Example   : =lfvClrDesc()
*!*******************************************************************************
*
FUNCTION lfvClrDesc
PRIVATE lcAlias , lcThredKey , lcSerialNo
lcAlias = ALIAS()

IF SEEK("NCTHREADCL "+lcThredClr,'Codes')
  SELECT Codes
  =gfObj_Lock(.T.)                  && Lock the record after saving. 
  REPLACE cDiscrep WITH lcClrDesc
  =gfObj_Lock(.F.)                  && Unlock the record after saving.   
ENDIF

SELECT (lcTmpThClr)

*B804365,1 Store Record number. [Begin] 
lcSerialNo = &lcTmpThClr..cSerial
*B804365,1 Store Record number. [End] 

*B804365,1 Modify the current seek to be on RecNo instead of thread colors. [Begin]
*lcThredKey = lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos + lcThredClr
lcThredKey = lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos + lcSerialNo
*B804365,1 Modify the current seek to be on RecNo instead of thread colors. [Begin]

LOCATE        
SHOW WINDOW (lcBrwColor) REFRESH SAME
=SEEK(lcThredKey)
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfvClrDesc.

*!*******************************************************************************
*! Name      : lfvClrNote
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid for the Color Notes field.
*!*******************************************************************************
*! Example   : =lfvClrNote()
*!*******************************************************************************
*
FUNCTION lfvClrNote
PRIVATE lcAlias , lcThredKey , lcSerialNo
lcAlias = ALIAS()

SELECT (lcTmpThClr)
REPLACE cComment1 WITH lcClrNotes

*B804365,1 Store Record number. [Begin] 
lcSerialNo = &lcTmpThClr..cSerial
*B804365,1 Store Record number. [End] 

*B804365,1 Modify the current seek to be on RecNo instead of thread colors. [Begin]
*lcThredKey = lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos + lcThredClr
lcThredKey = lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos + lcSerialNo
*B804365,1 Modify the current seek to be on RecNo instead of thread colors. [Begin]

LOCATE        
SHOW WINDOW (lcBrwColor) REFRESH SAME
=SEEK(lcThredKey)
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfvClrNote.

*!*******************************************************************************
*! Name      : lfvDsgnLoc
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid for the Design Location Popup.
*!*******************************************************************************
*! Example   : =lfvDsgnLoc()
*!*******************************************************************************
*
FUNCTION lfvDsgnLoc

PRIVATE lcAlias , lcDesgnCod
lcAlias = ALIAS()

SELECT (lcT_BomVar)
REPLACE cStylePos WITH laDsgnLoc[lnDsgnLoc,2] , mSizes WITH laDsgnLoc[lnDsgnLoc,1]
lcDesgnCod = cDsgnCode
LOCATE
*B606103,1 SSH Fix the bug of removeing the thread when changing the locations.
SELECT (lcTmpThClr)
REPLACE ALL cStylePos WITH laDsgnLoc[lnDsgnLoc,2] ;
  FOR cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos + cSerial=;
      &lcT_BomVar..cIdType + &lcT_BomVar..cCost_Id;
 .AND. cDsgnCode = (&lcT_BomVar..cDsgnCode)
FLUSH
SELECT (lcT_BomVar)
*B606103,1 SSH Fix the bug of removeing the thread when changing the locations.
SHOW WINDOW (lcBrwDsgn) REFRESH SAME
=SEEK(lcDesgnCod+laDsgnLoc[lnDsgnLoc,2])
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfvDsgnLoc.

*!*******************************************************************************
*! Name      : lfvNameDrp
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid for the Design Location Popup.
*!*******************************************************************************
*! Example   : =lfvNameDrp("1")
*!*******************************************************************************
*
FUNCTION lfvNameDrp
PARAMETERS lcNumber
PRIVATE lcAlias , lcDesgnCod
lcAlias = ALIAS()

SELECT (lcT_BomVar)
REPLACE cNameDrp&lcNumber WITH lcNameDrp&lcNumber
lcDesgnCod = cDsgnCode
LOCATE        
SHOW WINDOW (lcBrwDsgn) REFRESH SAME
=SEEK(lcDesgnCod+laDsgnLoc[lnDsgnLoc,2])
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfvNameDrp.

*!*******************************************************************************
*! Name      : lfvStitch
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid for the Design Location Popup.
*!*******************************************************************************
*! Example   : =lfvStitch()
*!*******************************************************************************
*
FUNCTION lfvStitch
PRIVATE lcAlias , lcDesgnCod
lcAlias = ALIAS()

SELECT (lcT_BomVar)
REPLACE nStitches WITH lnStitches
lcDesgnCod = cDsgnCode
LOCATE        
SHOW WINDOW (lcBrwDsgn) REFRESH SAME
=SEEK(lcDesgnCod+laDsgnLoc[lnDsgnLoc,2])
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfvStitch.

*!*******************************************************************************
*! Name      : lfvAddClr
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid Push button PbAddClr in Thread folder.
*!*******************************************************************************
*! Example   : =lfvAddClr()
*!*******************************************************************************
*
FUNCTION lfvAddClr
PRIVATE lcAlias
lcAlias = ALIAS()

*-- Add new line in the Temp. Thread color file.
SELECT (lcTmpThClr)
LOCATE FOR EMPTY(cThreadCl)
IF FOUND()
  
  *B604696,1 Add a serial of 0001 if different design code. [Begin]
  IF !(&lcT_BomVar..cDsgnCode==cDsgnCode)
    lnTmpRecNo = lnTmpRecNo + 1  
  ENDIF
  *B604696,1 Add a serial of 0001 if different design code. [End]
  
  BLANK
ELSE
  lnTmpRecNo = lnTmpRecNo + 1
  APPEND BLANK  
ENDIF

*B605059,1 check for lcTmpRecNo. [Begin]
IF lnTmpRecNo = 0
  lnTmpRecNo = lnTmpRecNo + 1
ENDIF
*B605059,1 check for lcTmpRecNo. [End]

*-- Call global function to add the audit fields to the newly added record.
=gfAdd_Info(lcTmpThClr)

*B804365,1 Add record number to Temp thread color. [Begin]
*REPLACE cIDType  WITH lcType , cCost_ID WITH lcOrdNo , LineNo   WITH lnOrdLinNo ,;
*        cDsgnCode WITH &lcT_BomVar..cDsgnCode , cStylePos WITH &lcT_BomVar..cStylePos
REPLACE cIDType  WITH lcType , cCost_ID WITH lcOrdNo , LineNo   WITH lnOrdLinNo ,;
        cDsgnCode WITH &lcT_BomVar..cDsgnCode , cStylePos WITH &lcT_BomVar..cStylePos ,;
        cSerial WITH PADL(ALLTRIM(STR(lnTmpRecNo,4)),4,'0')
*B804365,1 Add record number to Temp thread color. [End]
        
*-- Initialize screen variables.
STORE "" TO lcThredClr , lcClrDesc , lcClrNotes
SHOW GET lcThredClr ENABLE
SHOW GET lcClrDesc DISABLE
_CUROBJ = OBJNUM(lcThredClr)

=lfWhClrBrw()                   && Refresh the detail folder.
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfvAddClr.

*!*******************************************************************************
*! Name      : lfvRemAll
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid Push button PbRemAll in Thread folder.
*!*******************************************************************************
*! Example   : =lfvRemAll()
*!*******************************************************************************
*
FUNCTION lfvRemAll
*-- Message < Are you sure you want to Remove all records ? >
*-- Buttons <                 < Yes > - < No >                >
IF gfModalGen("QRM00000B00006","ALERT",'','','Are you sure you want to remove all records. ?') = 1
  *-- Delete the record & change the status according to the current.
  SELECT (lcTmpThClr)
  
  *B605059,1 Move to beginning of file. [Begin]
  *DELETE ALL FOR cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos = ;
  *       &lcT_BomVar..cIdType + &lcT_BomVar..cCost_Id + STR(&lcT_BomVar..LineNo,6) + ;
  *       &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos  
  DELETE ALL FOR cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos = ;
         &lcT_BomVar..cIdType + &lcT_BomVar..cCost_Id + STR(&lcOrdLine..LineNo,6) + ;
         &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos  
  LOCATE
  *B605059,1 Move to beginning of file. [End]
    
  *B804365,1 Reset Record number to 1 [Begin]
  lnTmpRecNo = 0
  *B804365,1 Reset Record number to 1 [End]
    
  *-- Call the browse when function.
  =lfWhClrBrw()
ENDIF
*-- End of lfvRemAll.

*!*******************************************************************************
*! Name      : lfBrwOrdLn
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Browse OrdLine file from Previous Orders screen.
*!*******************************************************************************
*! Example   : =lfBrwOrdLn()
*!*******************************************************************************
*
FUNCTION lfBrwOrdLn
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT OrdLine
*lnMark = RECNO()

*-- Brow the IcDesign file.
BROWSE FOR Style + DTOS(Complete) + cOrdType + Order + Store + STR(LineNo,6) = lcStyle ;
       AND Account = lcAccount AND Store = lcStore ;
       AND &lcTmpColor..cDsgnCode + &lcTmpColor..cStylePos = &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos ;
       FIELDS Order :H='Order' :R:8, ; 
       Status =IIF(OrdHdr.Status="B","Bid",IIF(OrdHdr.Status="O","Open",;
               IIF(OrdHdr.Status="H",'Hold',IIF(OrdHdr.Status="C",'Completed',;
               IIF(OrdHdr.Status="X",'Cancelled',''))))) :H='Status' :R:10, ;       
       Account :H='Account' :R:9, ;
       Store :H='Store' :R:15 ;
       WHEN lfWhBrOrdL() ;
       VALID :F lfvBrOrdLn();
       NOEDIT            ;
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WINDOW SOJLPRV1    ;
       IN WINDOW SOJLPRV  ;
       TITLE lcBrTtl1
SHOW WINDOW (lcBrTtl1) REFRESH SAME

SELECT (lcTmpColor)

*-- Brow the IcDesign file.
BROWSE FOR cIDType + cCost_ID + STR(LineNo,6) + cDsgnCode + cStylePos + cThreadCl = lcType+OrdLine.Order+STR(OrdLine.LineNo,6) ;
       FIELDS cThreadCl :H='Thread Color' :R:14, ; 
       lcDesc =IIF(SEEK('NCTHREADCL '+cThreadCl,'Codes'),Codes.cDiscRep,'') :H='Thread Color Description' :R:30 ,;
       cComment1 :H='Notes' :R:30 ;
       WHEN lfWhBrThCl() ;
       VALID :F lfvBrThClr();
       NOEDIT            ;
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WINDOW SOJLPRV2   ;
       IN WINDOW SOJLPRV ;
       TITLE lcBrTtl2
SHOW WINDOW (lcBrTtl2) REFRESH SAME

IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfBrwOrdLn.

*!*******************************************************************************
*! Name      : lfWhBrOrdL
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : When for OrdLine Line Browse in Previous Orders screen.
*!*******************************************************************************
*! Example   : =lfWhBrOrdL()
*!*******************************************************************************
*
FUNCTION lfWhBrOrdL
PRIVATE lcAlias
lcAlias = ALIAS()
*lnMark   = RECNO('OrdLine')         && Set the browse pointer.

*-- Flag to know that the active window is the browse.
glFromBrow = .T.
SELECT OrdLine
SHOW WINDOW (lcBrTtl1) REFRESH SAME  && Refresh the browse.
SHOW WINDOW (lcBrTtl2) REFRESH SAME  && Refresh the browse.
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfWhBrOrdL.

*!*******************************************************************************
*! Name      : lfvBrOrdLn
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid for OrdLine Line Browse in Previous Orders screen.
*!*******************************************************************************
*! Example   : =lfvBrOrdLn()
*!*******************************************************************************
*
FUNCTION lfvBrOrdLn

*-- If exist from browse window.
IF !WONTOP(lcBrTtl1)
  *-- Set flag to say I'm coming from a browse.
  glFromBrow = .T.
  *-- Call global function to stop the browse.
  =gfStopBrow()
ENDIF
*-- End of lfvBrOrdLn.

*!*******************************************************************************
*! Name      : lfWhBrThCl
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : When for Thread Colors in Previous Orders screen.
*!*******************************************************************************
*! Example   : =lfWhBrThCl()
*!*******************************************************************************
*
FUNCTION lfWhBrThCl
*lnMark   = RECNO('ThreadCl')        && Set the browse pointer.     
*-- Flag to know that the active window is the browse.
glFromBrow = .T.
SHOW WINDOW (lcBrTtl2) REFRESH SAME   && Refresh the browse.
*-- End of lfWhBrThCl.

*!*******************************************************************************
*! Name      : lfvBrThClr
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Valid for Thread Colors in Previous Orders screen.
*!*******************************************************************************
*! Example   : =lfvBrThClr()
*!*******************************************************************************
*
FUNCTION lfvBrThClr

*-- If exist from browse window.
IF !WONTOP(lcBrTtl2)
  *-- Set flag to say I'm coming from a browse.
  glFromBrow = .T.
  *-- Call global function to stop the browse.
  =gfStopBrow()
ENDIF
*-- End of lfvBrThClr.

*!*******************************************************************************
*! Name      : lfActScr
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Activate function for the Previous Orders screen.
*!*******************************************************************************
*! Example   : =lfActScr()
*!*******************************************************************************
*
FUNCTION lfActScr
*-- Clear browse as you are coming out of a browse window.
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If none of the screen's browses is active then clear the trapped keys.
IF !INLIST(WONTOP() , lcBrTtl1 , lcBrTtl2)
  *-- Clear all the trapped keys.
  ON KEY LABEL CTRL+Q
  ON KEY LABEL CTRL+W
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
ENDIF
*-- End of lfActScr.

*!*******************************************************************************
*! Name      : lfDActScr
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Activate function for the Previous Orders screen.
*!*******************************************************************************
*! Example   : =lfDActScr()
*!*******************************************************************************
*
FUNCTION lfDActScr

glFromBrow = INLIST(WONTOP() , lcBrTtl1 , lcBrTtl2)

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL TAB        DO lpOrdTab
  ON KEY LABEL BACKTAB    DO lpOrdBTab
ENDIF
*-- End of lfDActScr.

*!*******************************************************************************
*! Name      : lpOrdTab
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Trap of Tab key for Previous Orders screen.
*!*******************************************************************************
*! Example   : DO lpOrdTab
*!*******************************************************************************
*
PROCEDURE lpOrdTab
ON KEY LABEL TAB

*-- Point to the current object from the browse if press tab from the browse.
ACTIVATE WINDOW SOJLPRV3
_CUROBJ = OBJNUM(pbSelect)
ON KEY LABEL TAB DO lpOrdTab
*-- End of lpOrdTab.

*!*******************************************************************************
*! Name      : lpOrdBTab
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Trap of backtab key for Previous Orders screen.
*!*******************************************************************************
*! Example   : DO lpOrdBTab
*!*******************************************************************************
*
PROCEDURE lpOrdBTab

ON KEY LABEL BACKTAB

*-- Point to the current object from the browse if press 
*-- Shift + tab from the browse.
ACTIVATE WINDOW SOJLPRV3
_CUROBJ = OBJNUM(pbSelect)
ON KEY LABEL BACKTAB DO lpOrdBTab
*-- End of lpOrdBTab.

*!*******************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Collect Data from BomVar & Thread file.
*!*******************************************************************************
*! Example   : DO lpCollData
*!*******************************************************************************
*
PROCEDURE lpCollData
PARAMETERS lcType , lcOrdNo , lnOrdLinNo , lcT_BomVar , lcBulkNo

PRIVATE llThreadCl , lcAlias
lcAlias = ALIAS()
*-- Open the thread color file.
llThreadCl = gfOpenFile(gcDataDir+'ThreadCl',gcDataDir+'ThreadCl','SH')

*-- Open Thread Color file only once to copy its stucture to the temp Thread color file.
IF !USED(lcTmpThClr)  
  COPY STRUCTURE TO (gcWorkDir+lcTmpThClr)
  =gfOpenFile(gcWorkDir+lcTmpThClr,'','EX')  

  *B804365,1 Change index to be on serial instead of Color. [Begin] 
  *INDEX ON cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos + cThreadCl TAG (lcTmpThClr)
  INDEX ON cIdType + cCost_Id + STR(LineNo,6) + cDsgnCode + cStylePos + cSerial TAG (lcTmpThClr)
  *B804365,1 Change index to be on serial instead of Color. [End] 
ENDIF

*B804542,1 AKA 12/03/2001 Fix bug of repeating thread color in thread folder.[Start]
SELECT (lcTmpThClr)
ZAP
*B804542,1 AKA 12/03/2001 Fix bug of repeating thread color in thread folder. [End]


*-- If Temp BomVar empty we have to fill it. 
SELECT (lcT_BomVar)
LOCATE
IF EOF()
  *-- Copy data of Thread color file if View mode or Edit mode or Add from Bulk
  IF laScrMode[2] OR laScrMode[3] OR (laScrMode[4] AND !EMPTY(lcBulkNo))
    PRIVATE lcBomVrOrd
    lcBomVrOrd = IIF(laScrMode[2] OR laScrMode[3],lcOrdNo,lcBulkNo)
    
    SELECT ThreadCl
    IF SEEK(lcType + lcBomVrOrd) AND !SEEK(lcType + lcBomVrOrd,(lcTmpThClr))
      
      *B804365,1 Change scan rest to be on Serial instead of Thread Color. [Begin]
      *SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)+cdsgncode+cstylepos+cthreadcl = lcType + lcBomVrOrd
      SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)+cdsgncode+cstylepos+cSerial = lcType + lcBomVrOrd
      *B804365,1 Change scan rest to be on Serial instead of Thread Color. [End]
      
        SCATTER MEMVAR MEMO                
        m.cCost_Id = lcOrdNo
        INSERT INTO (lcTmpThClr) FROM MEMVAR
      ENDSCAN
    ENDIF

    *B804365 Remove empty line if found. [Begin]
    SELECT (lcTmpThClr)
    LOCATE FOR EMPTY(cThreadCl)
    IF FOUND()
      DELETE
    ENDIF
    *B804365 Remove empty line if found. [End]
        
    PRIVATE lcBomVrTag
  
    SELECT BomVar
    lcBomVrTag = TAG()
    SET ORDER TO TAG cDsgnCode
    
    IF SEEK(lcType + lcBomVrOrd)
      SCAN WHILE cidtype + ccost_id + STR(lineno,6) + cdsgncode + cstylepos = lcType + lcBomVrOrd
        IF !SEEK(cDsgnCode + cStylePos,(lcT_BomVar)) 
          SCATTER MEMVAR MEMO
          m.cCost_Id = lcOrdNo
          IF SEEK(lctype + lcBomVrOrd + STR(LineNo,6) + cDsgnCode + cStylePos,'ThreadCl')
            m.LineNo   = ThreadCl.LineNo        
          ELSE
            m.LineNo   = 0
          ENDIF
          INSERT INTO (lcT_BomVar) FROM MEMVAR
        ENDIF  
      ENDSCAN
    ENDIF
    
    SELECT BomVar
    SET ORDER TO TAG (lcBomVrTag)
  ENDIF
ELSE
  *-- IF Temp BomVar is not empty so we need to check if there are lines with Zero line number
  *-- lines with zero line number means that they are entered in BomVar file while there are no style lines in OrdLine file.
  SELECT (lcT_BomVar)
  LOCATE FOR LineNo = 0
  IF FOUND()
    SCAN FOR LineNo = 0
      =gfObj_Lock(.T.)                  && Lock the record after saving
      REPLACE LineNo WITH lnOrdLinNo
      =gfObj_Lock(.F.)                  && UnLock the record after saving      
    ENDSCAN
  ENDIF
ENDIF  

IF llThreadCl
  =gfCloseFile("ThreadCl")
ENDIF
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lpCollData.

*!*******************************************************************************
*! Name      : lpCreatBom
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 05/02/2001
*! Purpose   : Create Temp BomVar file.
*!*******************************************************************************
*! Example   : DO lpCreatBom
*!*******************************************************************************
*
PROCEDURE lpCreatBom
PRIVATE laFileStru , lnFileStru , laIndex , lcAlias
lcAlias = ALIAS()

IF !USED('BomVar')
  =gfOpenFile(gcDataDir+'BomVar',gcDataDir+'BomVar','SH')
ENDIF
  
SELECT BomVar
=AFIELDS(laBomVarSt)
=gfCrtTmp(lcT_BomVar,@laBomVarSt,[cdsgncode+cstylepos],lcT_BomVar)

IF USED(lcT_BomVar)
  SELECT (lcT_BomVar)
  USE
  =gfOpenFile(gcWorkDir+lcT_BomVar,gcWorkDir+lcT_BomVar,'EX')
ENDIF  

IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF  
*-- End of lpCreatBom.

*!*******************************************************************************
*! Name      : lpReSerial
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 10/25/2001
*! Purpose   : Re-Serialize Temp thread color file.
*!*******************************************************************************
*! Example   : DO lpReSerial
*!*******************************************************************************
*B605059,1 Procedure to Re-Seralize the temp thread color.
PROCEDURE lpReSerial
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT (lcTmpThClr)
IF SEEK(lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos)
  lnTmpRecNo = 0
  SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)+cdsgncode+cstylepos+cSerial = ;
                  lcType + lcOrdNo + STR(lnOrdLinNo,6) + &lcT_BomVar..cDsgnCode + &lcT_BomVar..cStylePos
    IF !EMPTY(cThreadCl)
      lnTmpRecNo = lnTmpRecNo + 1    
      REPLACE cSerial WITH PADL(ALLTRIM(STR(lnTmpRecNo,4)),4,'0')
    ENDIF
  ENDSCAN
ENDIF  
LOCATE

SELECT (lcAlias)
*-- End of lpReSerial
*!*******************************************************************************
*! Name      : lfActPop
*! Developer : Mohamed Shokry (MHM) 
*! Date      : 02/19/2002
*! Purpose   : activiate popup of Design location
*!*******************************************************************************
*! Example   : =lfActPop()
*!*******************************************************************************
*B605487,1
FUNCTION lfActPop

PRIVATE lcAlias
lcAlias  = SELECT(0)
lnCurRec = RECNO()

SELECT IcDesign

IF llFromPo
  lcOrder = SET('ORDER')
  SET ORDER TO TAG ICDESIGN
ENDIF
IF llFromPo
  =SEEK(lcDsgnCode)
ELSE
  =SEEK(lcAccount + lcStore + lcDsgnCode)
ENDIF
IF !EMPTY(lcDsgnCode)
  IF FOUND()
    llAddStats = .T.
    PRIVATE lnLoop , lcLoop , lnPosNum , lcKeyVal
    lnPosNum = 0
    FOR lnLoop = 6 TO 1 STEP -1
      lcLoop = ALLTRIM(STR(lnLoop,6))
      IF !EMPTY(IcDesign.cStylePos&lcLoop)
        lnPosNum = lnLoop
        EXIT
      ENDIF  
    ENDFOR
    IF lnPosNum > 0
      PRIVATE lnTotalPos 
      lnTotalPos = lnPosNum
      IF lnTotalPos > 0                
        DECLARE laDsgnLoc[lnTotalPos,2]
        laDsgnLoc = ''
        PRIVATE lnNumber
        lnNumber = 0
        FOR lnLoop = 1 TO lnPosNum
          lcLoop = ALLTRIM(STR(lnLoop,6))
          IF SEEK("NCSTYLEPOS " + ALLTRIM(IcDesign.cStylePos&lcLoop),'Codes') 
            lnNumber = lnNumber + 1 
            laDsgnLoc[lnNumber,1] = Codes.cDiscrep
            laDsgnLoc[lnNumber,2] = ALLTRIM(IcDesign.cStylePos&lcLoop)       
          ENDIF  
        ENDFOR 
        SELECT (lcT_BomVar)
        IF BETWEEN( lnCurRec ,1,RECCOUNT())
          GOTO lnCurRec 
        ENDIF      
        lnScnPos = ASCAN(laDsgnLoc,ALLTRIM(&lcT_BomVar..cStylePos),1)
        lnDsgnLoc = IIF(lnScnPos > 0 ,CEILING(lnScnPos/2),1)
        =lfRefresh('SoJLIn1b')
        SHOW GETS WINDOW 'SoJLIn1b' ONLY
      ENDIF
    ENDIF  
    IF laScrMode[3] 
      SHOW GET lnDsgnLoc ENABLE
    ENDIF
  ENDIF  
ENDIF
SELECT (lcAlias)
IF BETWEEN( lnCurRec ,1,RECCOUNT())
  GOTO lnCurRec 
ENDIF