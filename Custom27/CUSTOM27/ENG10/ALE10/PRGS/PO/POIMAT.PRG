*:----------------------------------------------------------------
*: Program file        : POIMAT.PRG
*: Program description : Issue material screen
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (MF)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C200147
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Modifications       : 
*:----------------------------------------------------------------
*: C200198,1 KHM 06/25/2001 Add two screens when recalculating the issued
*: C200198,1                quantities to handle the multi-store and 
*: C200198,1                multi-prepack for one Po line and two Po lines
*: C200220,1 KHM 09/03/2001 Adding the recalculation button and allow
*: C200220,1                the user to edit the unit quantity field.
*: B605037,1 KHM 10/21/2001 Checking the type of the primary fabric
*: B605037,1                in the style cost sheet: Same as, Constant or Different.
*: B605207,1 KHM 12/19/2001 Changing the way of recalculating the PO line
*: B605207,1                quantities in the case of no pre-pack.
*: C200294,1 KHM 02/19/2002 Adding a new routine to recalculate the quantities
*: C200294,1                in case of no prepacks and multi-lines.
*: C200316,1 KHM 03/24/2002 Allow the program to update the GL entries.
*: B605687,1 KHM 04/15/2002 Issue the selected open lot instead of 
*: B605687,1                automatically issue from the first lot.
*: B606633,1 KHM 11/27/2002 Handle the case of having the fabric/color more than once
*: B606633,1                in the style cost sheet by simulating the bom file from bomline file.
*: C037959,1 MHM 09/20/2004 Custom Amend to Issue screen for multi primary fabric
*: B128788,1 TMI 08/18/2005 Fix the bug that if more than one Mat.PO is recieved in the same session only the first is shown
*: B131603,1 HBG 04/11/2006 Check if issue / Return date falls in locked period 
*: B608049,1 TMI 04/17/2007 Fix a problem that the recalculate screen freezes when editing used qty ( T20070315.0003  )
*:----------------------------------------------------------------------------------
*C037959,1 MHM 09/20/2004 add new parameter to call the program from other programs[Start]
PARAMETERS lcPoCode
*C037959,1 MHM [End]

*-- Initializing the necessary variables.
PRIVATE laCodName , laCodes
DECLARE laCodes[1,2] , laCodName[1,2] , laLiStarr[1,2], laScrMode[4]
STORE .F. TO laScrMode, llEdit, llFrmValid, llNoShow, llMissScr, llAutoScr,;
             llBrowse, llEditQty

STORE .T. TO llMainScr, laScrMode[1]
STORE SPACE(1) TO lcCTStatus, laCodes, laCodName, laLiStarr, lcHost,;
                  lcMiHost, lcAiHost, lcTmpQuery, lcOpenLots,lcOpen,;
                  lcAllTrans, lcMtBrowTt, lcMisBTit, lcAutoTit, lcTmpPoLn

*C200198,1 KHM 06/25/2001 (Begin) Adding the following variables to hold
*C200198,1                the values of major and non-major segments
STORE 0 TO lnColorStr, lnColorLen, lnMajorLen
*C200198,1 KHM 06/25/2001 (End)

*C200220,1 KHM 09/03/2001 (Begin) Initializing the following variables
*C200220,1                in order to hold the old unit quantity.
lnOldUntQt = 0
lcDet_ttl = 'PO'
*C200220,1 KHM 09/03/2001 (End)

*C200316,1 KHM 03/24/2002 (Begin) check if the system is linked to GL or not.
llGlLink   = ALLTRIM(gfGetMemVar('M_LINK_GL'))  = 'Y' 
lcGlDTemp = ''
*C200316,1 KHM 03/24/2002 (End)

*B131603,1 HBG 04/11/2006 Add variables to check if issue / Return date falls in locked period [Begin]
lcGLFYear  = SPACE(4)   && Variable hold Fiscal Year
lcGLPeriod = SPACE(2)   && Variable hold Period
STORE {}  TO ldOldDate
*B131603,1 HBG 04/11/2006 [End]

lnTotRet   = 0
lcClosBmp  = gcBmpHome + "CLS.bmp"

*C037959,1 MHM 09/20/2004 add new variables to handel return to diffrent warehouse[Start]
STORE 1 TO lnWareHous 
DIMENSION laWareHous[1,2]
STORE '' TO laWareHous
STORE '' TO lcWareCod
*C037959,1 MHM [End]

IF !gfSetup()
  RETURN
ENDIF

*-- Get color Segmant and major length.
*C200198,1 KHM 06/25/2001 (Begin) To get the non-major position and the
*C200198,1                length of the major segment.
=lfGetColor()
lnMajorLen = LEN(gfItemMask("PM"))
*C200198,1 KHM 06/25/2001 (End)

lcHost        = "PODetScr"
lcMiHost      = "POMIss1"
lcAiHost      = "POAISS"
laDefProc[9]  = .F.  && Disable the control panel save proc.  (lpSavScr)
laDefProc[7]  = .F.  && Disable the control panel delete proc.(lpDelScr)

IF !WEXIST(gcBaseWind)
  lcScFields = "PO"
  SCATTER FIELDS &lcScFields TO laData BLANK
  =gfOpenFile(gcDataDir+"POSHDR",gcDataDir+"POSHDR","SH")
  =gfOpenFile(gcDataDir+"SCALE",gcDataDir+"SCALE","SH")
  =gfOpenFile(gcDataDir+"Style",gcDataDir+"Style","SH")
  =gfOpenFile(gcDataDir+"FabDye",gcDataDir+"FabDye","SH")
  =gfOpenFile(gcDataDir+"BomCost",gcDataDir+"BomCost","SH")
  =gfOpenFile(gcDataDir+"Fabric",gcDataDir+"Fabric","SH")
  =gfOpenFile(gcDataDir+"BomLine",gcDataDir+"BomLine","SH")
  =gfOpenFile(gcDataDir+"CtktBom",gcDataDir+"CtktBom","SH")
  =gfOpenFile(gcDataDir+"mapoalo",gcDataDir+"mapoalo","SH")
  =gfOpenFile(gcDataDir+"cutpick",gcDataDir+"cutpick","SH")
  =gfOpenFile(gcDataDir+"pofhdr",gcDataDir+"pofhdr","SH")
  =gfOpenFile(gcDataDir+"POFLN",gcDataDir+"POFLN","SH")
  =gfOpenFile(gcDataDir+'ApVendor',gcDataDir+'VenCode','SH')
  =gfOpenFile(gcDataDir+'StyDye',gcDataDir+'StyDye','SH')
  =gfOpenFile(gcDataDir+'OrdHdr','OrdHdr','SH')
  =gfOpenFile(gcDataDir+'OrdLine','OrdLine','SH')
  =gfOpenFile(gcDataDir+"MatInvJl",gcDataDir+"MatInvJl","SH")

  *C200316,1 KHM 03/24/2002 (Begin) Open the Gldist file.
  =gfOpenFile(gcDataDir+'GlDIST',gcDataDir+'GlDISTAC','SH')
  *C200316,1 KHM 03/24/2002 (End)

  *B605037,1 KHM 10/21/2001 (Begin) Open the BOM file to check the type
  *B605037,1                of the primary fabric: Same as, Constant 
  *B605037,1                or Different.
  =gfOpenFile(gcDataDir+'BOM',gcDataDir+'BOMITEM','SH')
  *B605037,1 KHM 10/21/2001 (End)
  
  *C037959,1 MHM 09/20/2004 Open sales order cost sheet file[Start]
  =gfOpenFile(gcDataDir+'ORDBOM','ORDBOM','SH')
  *C037959,1 MHM [End]
  
  lcTmpQuery= gfTempName()
  lcOpenLots= gfTempName()
  lcOpen = gfTempName()
  lcAllTrans= gfTempName()
  lcMtBrowTt = "Cost Sheet Details"
  lcMisBTit  = "Open Lots"
  lcAutoTit  = "Open Lots"

  *C200316,1 KHM 03/24/2002 (Begin) Initialize the temporary file to hold GL entries
  lcGlDTemp   = gfTempName()
  *C200316,1 KHM 03/24/2002 (End)

  *B605037,1 KHM 10/21/2001 (Begin) Initialize the lcDet_ttl variable.
  lcDet_ttl = lcMtBrowTt
  *B605037,1 KHM 10/21/2001 (End)

  =lfCretTmp()
ENDIF

*C037959,1 MHM 09/20/2004 set order to original order[Start]
SET ORDER TO POSLN IN POSLN
*C037959,1 MHM 09/20/2004 [End]

*C200316,1 KHM 03/24/2002 (Begin) Copy the structure of the GlDist file
IF llGlLink AND !USED(lcGlDTemp)
  SELECT GLDIST
  COPY STRUCTURE TO (gcWorkDir+lcGlDTemp)
  =gfOpenFile(gcWorkDir+lcGlDTemp,'','EX')
ENDIF
*C200316,1 KHM 03/24/2002 (Eng)

*C037959,1 MHM 09/20/2004 add option BAR to call Po Cost Sheet[Start]
=lfActPad()
*C037959,1 MHM [End]

IF TYPE('lcPoCode') = 'C' 
  laData[1]  = lcPoCode
  =lfvData1()
ENDIF

DO (gcScrDir + gcWinAppl + '\POIMAT1.SPX')
POP KEY
RELEASE WINDOW (lcMtBrowTt)
RELEASE PAD _Option OF _MSYSMENU
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01

*!*************************************************************
*! Name      : lfUnTrap
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Activate screen function....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfUnTrap
PARAMETER lcFrmScr
* -- Clear Trap
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

DO CASE
  CASE lcFrmScr = "MA" .AND. ALLTRIM(WONTOP()) # ALLTRIM((lcMtBrowTt))
    ON KEY LABEL TAB
    ON KEY LABEL BACKTAB
    ON KEY LABEL CTRL+TAB
    ON KEY LABEL CTRL+ENTER
    ON KEY LABEL CTRL+HOME
    ON KEY LABEL CTRL+END
    ON KEY LABEL CTRL+W
  CASE lcFrmScr = "MI" .AND. ALLTRIM(WONTOP()) # ALLTRIM((lcMisBTit))
    ON KEY LABEL TAB
    ON KEY LABEL BACKTAB
    ON KEY LABEL CTRL+TAB
    ON KEY LABEL CTRL+ENTER
    ON KEY LABEL CTRL+HOME
    ON KEY LABEL CTRL+END
    ON KEY LABEL CTRL+W
  CASE lcFrmScr = "AI"
ENDCASE
*-- If TOP window is not one of the browses window                                        
*-- end of lfUnTrap.

*!*************************************************************
*! Name      : lfKey
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Screen ICINVLK When Function.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfKey()
*!*************************************************************
*!
FUNCTION lfKey
PARAMETER lcFrmScr
PUSH KEY

DO CASE
  CASE TYPE("lcFrmScr") <> "C"
    DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
    DEFINE BAR 100 OF P01PU01 PROMPT lcMtBrowTt KEY ALT+B
    ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcMtBrowTt)
  CASE TYPE("lcFrmScr") = "C" .AND. lcFrmScr = "M"
    DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
    DEFINE BAR 100 OF P01PU01 PROMPT lcMisBTit KEY ALT+V
    ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcMisBTit)
  CASE .F.
ENDCASE


*!*************************************************************
*! Name      : lfTrap
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trap function for Browse....
*!           : Deactivate screen function.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab,lpBackTab
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfTrap

*-- if TOP window is one of the browse window
IF ALLTRIM(WONTOP()) == ALLTRIM((lcMtBrowTt)) .OR. ALLTRIM(WONTOP()) == ALLTRIM((lcMisBTit))
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing
  ON KEY LABEL TAB           DO lpTab
  ON KEY LABEL CTRL+TAB      DO lpTab
  ON KEY LABEL BACKTAB       DO lpBackTab
ENDIF
*-- end of lfTrap.

*!*************************************************************
*! Name      : lpTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trapping TAB order for browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
PROCEDURE lpTab
PARAMETER lcFrmScr
ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB
*-- Go to next window
DO CASE
  CASE  llMainScr    && Main Screen
    IF ALLTRIM(WONTOP()) == ALLTRIM((lcMtBrowTt))
       ACTIVATE WINDOW MFIMAT3
       _CUROBJ = OBJNUM(pbAutIss)
    ELSE
       ACTIVATE WINDOW (lcMtBrowTt)
      _CUROBJ = OBJNUM(ibBBrow)
    ENDIF
  CASE  llMissScr  && Manual Issue Screen
    IF ALLTRIM(WONTOP()) == ALLTRIM((lcMisBTit))
       ACTIVATE WINDOW POMISS3
      _CUROBJ = OBJNUM(pbMIss)
    ELSE
       ACTIVATE WINDOW (lcMisBTit)
      _CUROBJ = OBJNUM(ibMBrow)
    ENDIF
  CASE  llAutoScr
ENDCASE
*-- end of lpTab.

*!*************************************************************
*! Name      : lpBackTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trapping BACKTAB order for browse window
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab
*!*************************************************************
PROCEDURE lpBackTab

ON KEY LABEL BACKTAB
DO CASE
  CASE llMainScr  && Main Screen
     IF ALLTRIM(WONTOP()) == ALLTRIM((lcMtBrowTt))
      *-- Go to previous window
      ACTIVATE WINDOW MFIMAT2		&&Activate popup windows
      _CUROBJ = OBJNUM(pbCls)
    ELSE
       ACTIVATE WINDOW (lcMtBrowTt)
      _CUROBJ = OBJNUM(ibBBrow) && Activate control panel window
    ENDIF  
  CASE llMissScr  && Manual Issue Screen
    IF ALLTRIM(WONTOP()) == ALLTRIM((lcMisBTit))
      *-- Go to previous window
      ACTIVATE WINDOW POMISS2		&&Activate popup windows
      _CUROBJ = OBJNUM(ldMiDat)
    ELSE
       ACTIVATE WINDOW (lcMisBTit)
      _CUROBJ = OBJNUM(ibMBrow) && Activate control panel window
    ENDIF  
  CASE llAutoScr  && Automatic Issue
    
ENDCASE

*-- end of lpBackTab.

*!*************************************************************
*! Name      : lfActsBrow
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Activate the browse.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfActsBrow()
*!*************************************************************
FUNCTION lfActsBrow

SELECT &lcTmpQuery
GO TOP

*C200220,1 KHM 09/03/2001 (Begin) Adding a valid function lfvUntQty()
*C200220,1                in order to allow the user to edit the unit field.
*lcBrfield1 = [cMark    :02  :H = ''         :R,] +;
             [Item     :10  :H = 'Item'     :R,] +;
             [cDesc    :20  :H = 'Desc'     :R,] +;
             [Color    :08  :H = 'Colour'   :R,] +;
             [UOM      :05  :H = 'UOM'      :R,] +;
             [Unit     :14  :H = 'Units'    :R,] +;
             [OnHnd    :14  :H = 'On Hand'  :R,] +;
             [REQ      :14  :H = 'Required' :R,] +;
             [Issue    :14  :H = 'Issued'    :R,]+;
             [Used     :14  :H = 'Used'     :R,]+;
             [OnOrder  :14  :H = 'On Order' :R]
*C037959,1 MHM 09/20/2004 reduce values for fields[Start]
*lcBrfield1 = [cMark    :02  :H = ''         :R,] +;
             [Item     :10  :H = 'Item'     :R,] +;
             [cDesc    :20  :H = 'Desc'     :R,] +;
             [Color    :08  :H = 'Colour'   :R,] +;
             [UOM      :05  :H = 'UOM'      :R,] +;
             [Unit     :14  :H = 'Units'    :V=lfvUntQty('A'),] +;
             [OnHnd    :14  :H = 'On Hand'  :R,] +;
             [REQ      :14  :H = 'Required' :R,] +;
             [Issue    :14  :H = 'Issued'    :R,]+;
             [Used     :14  :H = 'Used'     :R,]+;
             [OnOrder  :14  :H = 'On Order' :R]

lcBrfield1 = [cMark    :01  :H = ''         :R,] +;
             [Item     :09  :H = 'Item'     :R,] +;
             [cDesc    :20  :H = 'Desc'     :R,] +;
             [Color    :08  :H = 'Colour'   :R,] +;
             [UOM      :04  :H = 'UOM'      :R,] +;
             [Unit     :08  :H = 'Units'    :V=lfvUntQty('A'),] +;
             [OnHnd    :08  :H = 'On Hand'  :R,] +;
             [Used     :08  :H = 'Used'     :R,]+;
             [REQ      :08  :H = 'Required' :R,] +;
             [Issue    :08  :H = 'Issued'   :R,]+;
             [OnOrder  :08  :H = 'On Order' :R]
*C037959,1 MHM [End]
*C200220,1 KHM 09/03/2001 (End)

BROWSE FIELDS &lcBrField1;
       WHEN lfwBrows("MA");
       VALID :F lfvBrowse("MA") ;
       WINDOW MFIMATB IN WINDOW (gcBaseWind);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE ALLTRIM(lcMtBrowTt)

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : When Browse Temp. sequence file fn.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrows()
*!*************************************************************
FUNCTION lfwBrows
PARAMETER lcFrmScr

DO CASE
  CASE lcFrmScr = "MA"
    SELECT (lcTmpQuery)
    lnNewRec = RECNO()
    REPLACE ALL cMark WITH ' '
    GO lnNewRec
    REPLACE cMark WITH ">"
    SHOW WINDOW (lcMtBrowTt) REFRESH

    *C200220,1 KHM 09/03/2001 (Begin) Saving the old unit quantity field.
    lnOldUntQt = Unit
    *C200220,1 KHM 09/03/2001 (End)

  CASE lcFrmScr = "MI"
    SELECT (lcOpenLots)
    lnNewRec = RECNO()
    REPLACE ALL cMark WITH ' '
    GO lnNewRec
    REPLACE cMark WITH ">"
    *llEdit = .F.
    SHOW WINDOW (lcMisBTit) REFRESH
  CASE lcFrmScr = "AI"
    SELECT (lcOpenLots)
    lnNewRec = RECNO()
    REPLACE ALL cMark WITH ' '
    GO lnNewRec
    REPLACE cMark WITH ">"
    *llEdit = .F.
    lcPrompt = IIF(cSelect="�","\<Unselect","S\<elect")
    SHOW GET pbISelect,1 PROMPT lcPrompt
    SHOW WINDOW (lcAutIssTt) REFRESH
ENDCASE
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvBrowse
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Browse Temp. sequence file fn.
*!*************************************************************
*! Passed Parameters  : lcFrmScr
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowse()
*!*************************************************************
*!
FUNCTION lfvBrowse
PARAMETER lcFrmScr

DO CASE
  CASE lcFrmScr == "MA"
    IF ALLTRIM(WONTOP()) # ALLTRIM((lcMtBrowTt))
      glFromBrow = .T.
      = gfStopBrow()
    ENDIF
  CASE lcFrmScr == "MI"
    IF ALLTRIM(WONTOP()) # ALLTRIM((lcMisBTit))
      glFromBrow = .T.
      = gfStopBrow()
    ENDIF
  CASE lcFrmScr == "AI"
    IF ALLTRIM(WONTOP()) # ALLTRIM((lcAutIssTt))
      glFromBrow = .T.
      = gfStopBrow()
    ENDIF
ENDCASE
*-- end of lfvBrowse.

*!*************************************************************
*! Name      : lfCretTmp
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To create the temporary file
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCretTmp()
*!*************************************************************
FUNCTION lfCretTmp

*C200220,1 KHM 09/03/2001 (Begin) Adding a new field cBomType
*CREATE TABLE (gcWorkDir+lcTmpQuery);
             (cMark C(1) , Item C(07) , Color C(06),;
              UOM C(3)   , Unit N(7,3), OnHnd N(12,3) , Req N(12,3),;
              Issue N(12,3) , Used N(12,3) , OnOrder N(12,3),cDesc C(20))

*C037959,1 MHM 09/20/2004 Add new field[Start]
*Style      style line belongs to
*lPrimary   fabric Primary (Y/N)
*cCatgTyp   (Fabric/Trim)
*lFrmBom    come from BOMLINE /CTKTBOM 
*CREATE TABLE (gcWorkDir+lcTmpQuery);
             (cMark C(1) , Item C(07) , Color C(06),;
              UOM C(3)   , Unit N(7,3), OnHnd N(12,3) , Req N(12,3),;
              Issue N(12,3) , Used N(12,3) , OnOrder N(12,3),cDesc C(20),;
              cBomType C(1))
CREATE TABLE (gcWorkDir+lcTmpQuery);
             (cMark C(1) , Item C(07) , Color C(06),;
              UOM C(3)   , Unit N(7,3), OnHnd N(12,3) , Req N(12,3),;
              Issue N(12,3) , Used N(12,3) , OnOrder N(12,3),cDesc C(20),;
              cBomType C(1),Style C(19),lPrimary L,cCatgTyp C(1) , lFrmBom L(1))
*C037959,1 MHM 09/20/2004 [End]

*C200220,1 KHM 09/03/2001 (End)

=gfOpenFile(gcWorkDir+lcTmpQuery,"","SH")
SELECT (lcTmpQuery)

*C037959,1 MHM 09/20/2004 Add Catagry type to sort by Fabric and trim[Start]
*INDEX ON Item +Color  TAG (lcTmpQuery)
INDEX ON cCatgTyp + Item +Color  TAG (lcTmpQuery)
*C037959,1 MHM [End]

*!*************************************************************
*! Name      : lpEnter
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To Change the select button prompt.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpEnter()
*!*************************************************************
FUNCTION lpEnter
SELECT (lcTmpQuery)
lcPrompt = IIF(cSelect="�","\<Unselect","S\<elect")


*!*************************************************************
*! Name      : lpShow
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To control the tool bar buttons
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpShow()
*!*************************************************************
FUNCTION lpShow
SHOW GET pbEdt DISABLE
SHOW GET pbSav DISABLE
SHOW GET pbDlt DISABLE
=lfEnabDis("DISABLE","ENABLE")
DO CASE
  *--- Case Select Mode Screen
  CASE laScrMode[1]
    =lfEnabDis( "ENABLE","DISABLE")
    SELECT (lcTmpQuery)
    DELETE ALL
    =lfActsBrow()
    llFrmValid = .F.
  *--- Case View Mode Screen
  CASE laScrMode[2]
    =lfViewMod()
  *--- Case Edit Mode Screen
  CASE laScrMode[3]
  *--- Case Add New Mode Screen
  CASE laScrMode[4]
ENDCASE

*!*************************************************************
*! Name      : lfEnabDis
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To enable and disable the buttons according to the
*!             screen mode.
*!*************************************************************
*! Passed Parameters  : lcKeySta , lcButSta
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfEnabDis()
*!*************************************************************
FUNCTION lfEnabDis
PARAMETER lcKeySta , lcButSta

*--- Key Level Variable
SHOW  GET laData[1] &lcKeySta
SHOW  GET pbBrAcct &lcKeySta

*--- First Screen Button [START]
SHOW  GET pbAutIss &lcButSta
SHOW  GET pbManIss &lcButSta
SHOW  GET pbRet    &lcButSta

*C200220,1 KHM 09/03/2001 (Begin) Adding the recalculate button
SHOW  GET pbReCal  &lcButSta
*C200220,1 KHM 09/03/2001 (End)

SHOW  GET pbPoDet  &lcButSta
lcCTStatus = ""
*--- First Screen Button [END..]

*!*************************************************************
*! Name      : lfvData1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To validate the PO
*!             screen mode.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData1()
*!*************************************************************
FUNCTION lfvData1
PRIVATE lnOldAls

lnOldAls = SELECT(0)

IF llBrowse OR ( !EMPTY(laData[1]) AND !SEEK('P'+laData[1],'POSHDR') )
  lcPo=laData[1]
  SELECT POSHDR
  SET FILTER TO cStyType='P'
  DO POSBrow WITH lcPo,'','P'
  laData[1]=lcPo
  llBrowse = .F.
ENDIF 

IF !EMPTY(laData[1])
  IF !(POSHDR.Status $ "O||A")
    lcMsg = "Only Open PO is allowed. Cannot proceed."
    DO CASE
      CASE POSHDR.Status = "H"
        lcMsg = "P/O Status is On HOLD since a cost sheet has not yet been created.Cannot proceed."
      CASE POSHDR.Status = "X"
        lcMsg = "P/O Status is CANCELED. Cannot proceed."
      CASE POSHDR.Status = "S"
        lcMsg = "P/O Status is CLOSED. Cannot proceed."
      CASE POSHDR.Status = "C"
        lcMsg = "P/O Status is COMPLETED. Cannot proceed."
    ENDCASE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
    laData[1] = ''
    llFrmValid = .T.
    SHOW GET laData[1]
    laScrMode = .F.
    laScrMode[1] = .T.
    SHOW GETS
    SELECT(lnOldAls)
    RETURN
  ENDIF
  laScrMode = .F.
  laScrMode[2] = .T.
  SHOW GETS
ENDIF


*!*************************************************************
*! Name      : lfDataVld
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To collect the PO information
*!             screen mode.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDataVld()
*!*************************************************************
FUNCTION lfDataVld

=lfGetData()
IF POSHDR.Status = "O"
  lcCTStatus = "Open"
ELSE
  lcCTStatus = "Actualiz"
ENDIF
SHOW GET lcCTStatus
SELECT (lcTmpQuery)
GO TOP
IF !EOF()
  SHOW  GET pbPODet  ENABLE
ELSE
  SHOW  GET pbPODet  DISABLE
ENDIF

*!*************************************************************
*! Name      : lfGetData
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To get the fabric & trims for the selected PO
*!             screen mode.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetData()
*!*************************************************************
FUNCTION lfGetData
PRIVATE lnOldAls

lnOldAls = SELECT(0)

*C037959,1 MHM 09/20/2004 if tow diffrent style with the same scale[Start]
*-then collect data from BOMLINE else collect from CTKTBOM
llchckTru = .F.
IF SEEK("P"+laData[1],"POSLN") 
  lcCurrSty = SUBSTR(PosLn.Style,1,12)
  lcCurrScal = Posln.scale
  SELECT POSLN
  SCAN REST WHILE  cStyType+Po+Style+STR(lineno,6)+Trancd= "P"+laData[1]
    IF SUBSTR(PosLn.Style,1,12) <> lcCurrSty
      IF Posln.scale  = lcCurrScal  
        llchckTru = .T.
        EXIT
      ENDIF
    ENDIF
  ENDSCAN
ENDIF  
IF llchckTru 
  SELECT BOMLINE
  *--- cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
  =SEEK('I'+'1'+laData[1])
  SCAN REST WHILE cImTyp+cType+cTktNo+STR(lineno,6)+cBomTyp+Style+Sclr+Item+Iclr+mfgcode =;
                  'I'+'1'+laData[1];
             FOR   cCatgTyp $ "F|T"
    SCATTER MEMVAR MEMO
    =SEEK(PADL(m.Item,7)+m.IClr,'Fabric')    
    m.Desc = Fabric.Desc
    m.UOM  = Fabric.UOMUSE
    SELECT (lcTmpQuery)
    *--- cMark,Item,Color,UOM,Unit,OnHnd,Req,Issue,Used,OnOrder
    APPEND BLANK
    REPLACE Item    WITH m.item,;
            Color   WITH m.iClr,;
            cDesc   WITH m.Desc,;
            UOM     WITH m.UOM ,;
            Unit    WITH m.UnitQty,;
            OnHnd   WITH lfGetOnH(),;
            Req     WITH m.ItemQty,;
            Issue   WITH m.Issue_Qty,;
            Used    WITH m.Used_Qty,;
            OnOrder WITH lfGtOnOrd(),;
            lFrmBom WITH .T.

    REPLACE cBomType WITH m.cBomTyp
    REPLACE lPrimary WITH lfChkOrdF(m.item,m.iClr),;
            Style    WITH BOMLINE.Style,;
            cCatgTyp WITH m.cCatgTyp 
  ENDSCAN
ELSE
*C037959,1 MHM [End]

  SELECT CtktBom
  *--- cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
  =SEEK('I'+laData[1])
  SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot =;
                  'I'+laData[1];
            FOR   cCatgTyp $ "F|T"
    SCATTER MEMVAR MEMO
    SELECT (lcTmpQuery)
    *--- cMark,Item,Color,UOM,Unit,OnHnd,Req,Issue,Used,OnOrder
    APPEND BLANK
    REPLACE Item    WITH m.item,;
            Color   WITH m.iClr,;
            cDesc   WITH m.Desc,;
            UOM     WITH m.UOM ,;
            Unit    WITH m.UntQty,;
            OnHnd   WITH lfGetOnH(),;
            Req     WITH m.Req_Qty,;
            Issue   WITH m.Issue_Qty,;
            Used    WITH m.Used_Qty,;
            OnOrder WITH lfGtOnOrd()

    *C200220,1 KHM 09/03/2001 (Begin) Adding the replacement of m.typ in order
    *C200220,1                to be able to get the corresponding records 
    *C200220,1                from CTKTBOM and BOMLINE
    REPLACE cBomType WITH m.Typ
    *C200220,1 KHM 09/03/2001 (End)
 
    *C037959,1 MHM 09/20/2004 get primary , style and catagary type[Start]
    REPLACE lPrimary WITH lfChkOrdF(m.item,m.iClr),;
            Style    WITH lfGetSty(m.item,m.iClr),;
            cCatgTyp WITH m.cCatgTyp 
    *C037959,1 MHM [End]
  ENDSCAN
  
  
*C037959,1 MHM 09/20/2004 if tow diffrent style with the same scale[Start]
ENDIF
*C037959,1 MHM [End]

SELECT (lcTmpQuery)
GO TOP
IF EOF()
  = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              "No materials have been assigned to this PO.")
  SHOW GET pbEdt DISABLE
  SHOW GET pbDlt DISABLE
ENDIF
=lfActsBrow()
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfGetOnH
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To get the material on hand
*!             screen mode.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetOnH()
*!*************************************************************
FUNCTION lfGetOnH
PRIVATE lnOldAls , lcOrder , lcMatPo , lnTotOpn , lnTmpOpn

lcOrder  = SPACE(06)
lnOldAls = SELECT(0)
STORE 0 TO lnTotOpn, lnTmpOpn
SELECT cutpick
lcOrder = IIF(SEEK("2"+laData[1]),Order,SPACE(06))
IF lfGetOpn(PADL(m.Item,7),m.Iclr)
  SELECT mapoalo
  SET ORDER TO Maposo
  IF !EMPTY(lcOrder) .AND. SEEK(lcOrder+PADL(m.Item,7)+m.Iclr)

    SCAN REST WHILE order+fabric+color = lcOrder+PADL(m.Item,7)+m.Iclr
      lcMatPo  = PoMat
      lnTmpOpn = 0
      SELECT (lcOpenLots)
      SUM ALL nBalance TO lnTmpOpn FOR PofHdr.PoMat = lcMatPo
      lnTotOpn = lnTotOpn + lnTmpOpn
    ENDSCAN

    IF lnTotOpn <= 0
      SELECT (lcOpenLots)
      SUM ALL nBalance TO lnTotOpn
    ENDIF
  ELSE
    SELECT (lcOpenLots)
    SUM ALL nBalance TO lnTotOpn
  ENDIF
  IF USED("FABINV")
    USE IN FABINV
  ENDIF
ENDIF
SELECT(lnOldAls)
RETURN(lnTotOpn)


*!*************************************************************
*! Name      : lfViewMod
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To get the material on hand
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfViewMod()
*!*************************************************************
FUNCTION lfViewMod

SELECT (lcTmpQuery)
DELETE ALL
IF SEEK("P"+laData[1],"POSHDR") .AND. (POSHDR.Status $ "O||A")
  =lfDataVld()
ELSE
  IF !llFrmValid
    lcMsg = ""
    DO CASE
      CASE POSHDR.Status = "H"
        lcMsg = "P/O Status is On HOLD since a cost sheet has not yet been created.Cannot proceed."
      CASE POSHDR.Status = "X"
        lcMsg = "P/O Status is CANCELED. Cannot proceed."
      CASE POSHDR.Status = "S"
        lcMsg = "P/O Status is CLOSED. Cannot proceed."
      CASE POSHDR.Status = "C"
        lcMsg = "P/O Status is On COMPLETED. Cannot proceed."
    ENDCASE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
    llFrmValid = .F.
  ENDIF
  SHOW GET pbEdt DISABLE
  SHOW GET pbDlt DISABLE
ENDIF
=lfActsBrow()

*!*************************************************************
*! Name      : lfGetOpn
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To get the open lots
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetOpn()
*!*************************************************************
FUNCTION lfGetOpn
PARAMETER lcFabric , lcColor , lcFrmRet
PRIVATE llToRet

*--- cFabric,cColor,,cDyelot,cRSession
llToRet = .F.
lcMatInvJl  = gfTempName()  && Alias name for matinvjl
IF TYPE("lcFrmRet") = "C" .AND. lcFrmRet = "R"
  =lfMatOnHnd()  && Functino to get the open balance for each lot
  =lfCrMatTmp()  && Functino to create temp file.
  =lfBomCst()

  SELECT PofLn
  SET ORDER TO PofLnF
  
  SELECT (lcOpenLots)
  SCAN
    lcTempRes = cRsession
    lcTempIss = cISession
    SCATTER MEMVAR MEMO
  
    =SEEK(lcTempRes,lcOpen)
    SELECT (lcOpenLots)

   REPLACE  PO WITH lfRetMPo(PADL(&lcTmpQuery..Item,7),&lcTmpQuery..Color,lcTempRes),;
            nBalance WITH &lcOpen..nBalance
  ENDSCAN
  SELECT (lcOpenLots)
  DELETE ALL FOR nToIssue <=0

  *-- To calculate the on hand.
  =lfCalOnHnd()

ELSE
  *B128788,1  TMI [Start] Fix the bug that if more than one Mat.PO is recieved in the same session only the first is shown
  *SELECT SPACE(02) AS cMark , SPACE(02) AS cSelect ,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,nUnitCost,;
  *       cApInvNo,dTranDate,nRecCost,cTran,SUM(nReceived-nIssued) AS 'nBalance',;
  *       nUntCstBuy, 00000000000.000 AS nToIssue ,nReceived ,.T. AS lUpdate FROM MatInvJl ;
  *WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
  *       lcFabric+lcColor;
  *GROUP BY cFabric,cColor,cWareCode,cDyelot,cRSession ;
  *HAVING nBalance > 0 INTO DBF (gcWorkDir+lcOpenLots)

  *B131603,1 HBG 04/11/2006 (Begin) Clear fixing Of B#128788 that because system dealing with Receiving Session as a Lot
  *B131603,1 HBG            so if there are more one PO in the same Receiving session all of them are considered as one lot
  *B131603,1 HBG            and the first Po is displaying at the screen as a reference not as a lot number
  *SELECT SPACE(02) AS cMark , SPACE(02) AS cSelect ,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,nUnitCost,;
         cApInvNo,dTranDate,nRecCost,cTran,SUM(nReceived-nIssued) AS 'nBalance',;
         nUntCstBuy, 00000000000.000 AS nToIssue ,nReceived ,.T. AS lUpdate FROM MatInvJl ;
  WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
         lcFabric+lcColor;
  GROUP BY cFabric,cColor,cWareCode,cDyelot,cRSession,cTran ;
  HAVING nBalance > 0 INTO DBF (gcWorkDir+lcOpenLots)
  
  SELECT SPACE(02) AS cMark , SPACE(02) AS cSelect ,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,nUnitCost,;
         cApInvNo,dTranDate,nRecCost,cTran,SUM(nReceived-nIssued) AS 'nBalance',;
         nUntCstBuy, 00000000000.000 AS nToIssue ,nReceived ,.T. AS lUpdate FROM MatInvJl ;
  WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
         lcFabric+lcColor;
  GROUP BY cFabric,cColor,cWareCode,cDyelot,cRSession;
  HAVING nBalance > 0 INTO DBF (gcWorkDir+lcOpenLots)
  *B131603,1 (End)
  
  *B128788,1  TMI [End  ] 
ENDIF
SELECT (lcOpenLots)
GO TOP
IF !EOF()
  USE (gcDataDir+'MATINVJL') AGAIN ALIAS FABINV ORDER TAG MatInvJl IN 0
  SELECT (lcOpenLots)
  *B128788,1  TMI [Start] Add a key in the index for ctran
  *INDEX ON cWareCode+cDyelot TAG (lcOpenLots)
  INDEX ON cWareCode+cDyelot+cTran TAG (lcOpenLots)
  *B128788,1  TMI [End  ] 
  SET RELATION TO cFabric+cColor+cWareCode+cDyelot+cRSession INTO FABINV
  *B128788,1  TMI [Start] Let the relation be directly from lcOpenLots to pofhdr file
  *SELECT FABINV
  *SET RELATION TO 'P'+cTran INTO PofHdr
  SELECT &lcOpenLots
  SET RELATION TO 'P'+cTran INTO PofHdr ADDITIVE
  *B128788,1  TMI [End  ] 
  llToRet = .T.
ENDIF
SELECT PofLn
SET ORDER TO PofLn
RETURN llToRet

*!*************************************************************
*! Name      : lfGtOnOrd
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To get the on order for the open lots
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGtOnOrd()
*!*************************************************************
FUNCTION lfGtOnOrd
PRIVATE lnOldAls , lcScanExp , lnOnOrd

lnOldAls = SELECT(0)
SELECT cutpick
lcScanExp = ""
lnOnOrd   = 0

lcOrder = IIF(SEEK("2"+laData[1]),Order,SPACE(06))
SELECT mapoalo
SET ORDER TO Maposo
IF !EMPTY(lcOrder) .AND. SEEK(lcOrder+PADL(m.Item,7)+m.Iclr)
  SCAN REST WHILE order+fabric+color = lcOrder+PADL(m.Item,7)+m.Iclr
    lcScanExp = "P"+POMat+PADL(m.Item,7)+m.Iclr
    SELECT POFLN
    =SEEK(lcScanExp)
    SCAN REST WHILE cmattype+pomat+fabric+color+trancd = lcScanExp
      lnOnOrd   = lnOnOrd + IIF(trancd = "1",nFabTotQty,-nFabTotQty)
    ENDSCAN
  ENDSCAN
ENDIF
SELECT(lnOldAls)

*-- Seek in the fabric file to get the conversion factor.
=SEEK(PADL(m.Item,7)+m.IClr,'Fabric')
lnOnOrd = lnOnOrd* IIF(Fabric.Conv = 0 ,1,Fabric.Conv)

RETURN(MAX(lnOnOrd,0))

*!*************************************************************
*! Name      : lfvPODet
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : To display the material detail PO screen.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPODet()
*!*************************************************************
FUNCTION lfvPODet
PARAMETER lcFrom

PRIVATE lnOldAls , lcMatPo ,lcDetPo ,lcDetVen,ldDetComp,lnDetQty
lnOldAls = SELECT(0)
PUSH KEY
ON KEY
SELECT cutpick
lnDetQty  = 0
STORE '' TO lcDetVen, lcDetPo
*C200220,1 KHM 09/03/2001 (Begin) Initializing the following variable to hold
*C200220,1                the on hand qty for each material PO.
lnOnHndQty = 0
*C200220,1 KHM 09/03/2001 (End)
lcOrder = IIF(SEEK("2"+laData[1]),Order,SPACE(06))
SELECT mapoalo
SET ORDER TO Maposo

IF SEEK(lcOrder+PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..Color)

  *-- Seek in the fabric file to get the conversion factor.
  =SEEK(PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..Color,'Fabric')

  SCAN REST WHILE order+fabric+color = lcOrder+PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..Color
    lcDetPo = PoMat  
    SELECT POFLN
    *--- cmattype+pomat+fabric+color+trancd
    =SEEK("P"+lcDetPo+&lcTmpQuery..Item+&lcTmpQuery..Color)
    lcDetVen  = Vendor
    IF TYPE("lcFrom")<>"C"
      SCAN REST WHILE cmattype+pomat+fabric+color+trancd = "P"+lcDetPo+PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..Color
        lnDetQty  =  lnDetQty +  IIF(trancd ="1" ,nFabTotQty,-nFabTotQty)
      ENDSCAN
    ELSE

      IF SEEK(PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..Color,'MatInvJl')
        SELECT MatInvJl        
        LOCATE REST WHILE cFabric+cColor+cWarecode+cDyelot+cRSession+cISession=;
                          PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..Color;
                     FOR cTran = lcDetPo AND cTranType = '1'
        IF FOUND()
          lcRSession = cRSession
          *SUM REST (nReceived-nIssued) TO lnDetQty ;
              WHILE cFabric+cColor+cWarecode+cDyelot+cRSession+cISession=;
                    PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..Color;
              FOR cRSession = lcRSession
          *C200220,1 KHM 09/03/2001 (Begin) Adding the FOR condition.
          *SUM REST (nReceived-nIssued) TO lnDetQty ;
              WHILE cFabric+cColor+cWarecode+cDyelot+cRSession+cISession=;
                    PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..Color

          SUM REST (nReceived-nIssued) TO lnOnHndQty ;
              WHILE cFabric+cColor+cWarecode+cDyelot+cRSession+cISession=;
                    PADL(&lcTmpQuery..Item,7)+&lcTmpQuery..Color;
              FOR cTran = lcDetPo 
          lnDetQty = lnDetQty + lnOnHndQty          
          *C200220,1 KHM 09/03/2001 (End)
        ENDIF
      ENDIF  

    ENDIF
    IF TYPE("lcFrom")<>"C" .AND. lnDetQty > 0
      EXIT
    ENDIF
  ENDSCAN
  IF !EMPTY(lcDetPo) .AND. TYPE("lcFrom")<>"C" AND lnDetQty > 0
    SELECT POFHDR
    =SEEK("P"+lcDetPo)
    ldDetComp = complete
    lnDetQty  = lnDetQty * IIF(Fabric.Conv = 0 ,1,Fabric.Conv)

    DO (gcScrDir + gcWinAppl + '\Pomatdet.SPX')
    RETURN
  ELSE
    IF TYPE("lcFrom") <> "C" AND lnDetQty <= 0 
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
                 "No open PO's found for fabric/color: "+ALLTRIM(&lcTmpQuery..Item)+"/"+&lcTmpQuery..Color)
    ENDIF
  ENDIF
  POP KEY
ELSE
  POP KEY
  IF TYPE("lcFrom") <> "C" 
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
                 "No material's PO has been assigned to fabric/color: "+ALLTRIM(&lcTmpQuery..Item)+"/"+&lcTmpQuery..Color)
  ENDIF
ENDIF
SELECT(lnOldAls)

IF TYPE("lcFrom") = "C"
  RETURN(lnDetQty)
ENDIF

*!*************************************************************
*! Name      : lfvMIss1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/2001
*! Purpose   : To manually issue or retrun materials.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvMIss1()
*!*************************************************************
FUNCTION lfvMIss1
PARAMETER lcIssRet
PRIVATE lnOldAls , llCanDist , lnSeeIssu 
lnOldAls = SELECT(0)

*B131603,1 HBG 04/11/2006 (BEGIN) initialize new flag for first Validation only
*STORE .F. TO llCanDist, llDistDone
STORE .F. TO llCanDist, llDistDone,llChecked
*B131603,1 (END)

lnSeeIssu  = 0
 IF lfGetOpn(&lcTmpQuery..Item,&lcTmpQuery..Color,lcIssRet)
  lcOldVal = ""

  *C200198,1 KHM 06/25/2001 (Begin) Allowing the user to issue the remaining
  *C200198,1                of the material if any.
  *llCanDist = (&lcTmpQuery..Issue=0)
  llCanDist = (&lcTmpQuery..Issue=0 OR &lcTmpQuery..Issue < &lcTmpQuery..REQ)
  *C200198,1 KHM 06/25/2001 (End)

  STORE .T. TO llMissScr, llIssue
  STORE .F. TO llMainScr, llAutoScr
  ldMiDat    = gdSysDate
  lcMiItm    = &lcTmpQuery..Item
  lcMiClr    = &lcTmpQuery..Color

  *C200198,1 KHM 06/25/2001 (Begin) Calculate the minimum requirement after
  *C200198,1                deducting the issued.
  *lnMiReq    = &lcTmpQuery..REQ

  *B605687,1 KHM 04/15/2002 (Begin) We should reduce the used not the issued.
  *lnMiReq    = MAX(&lcTmpQuery..REQ - &lcTmpQuery..Issue,0)
  lnMiReq    = MAX(&lcTmpQuery..REQ - &lcTmpQuery..Used,0)
  *B605687,1 KHM 04/15/2002 (End)
  
  *B605687,1 KHM 04/15/2002 (Begin) Initialize lnMiToCal to hold the issued
  *B605687,1                quantity that will be distributed.
  lnMiToCal =lnMiReq
  *B605687,1 KHM 04/15/2002 (End)
  
  *C200198,1 KHM 06/25/2001 (End)

  SELECT CtktBom
  SET ORDER TO Ctktyp

  =SEEK("I"+laData[1]+PADR(lcMiItm,19)+lcMiClr)
  lcOperCode = cOprCode
  DECLARE laLots[1]
  =gfOpenFile(gcDataDir+"mfgoprdt",gcDataDir+"mfgoprdt","SH")
  SELECT DIST cLotNo FROM mfgoprdt ;
  WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot= "I"+laData[1]+lcOperCode ;
  AND TranCd='1'INTO ARRAY laLots
  SELECT CtktBom
  SET ORDER TO CtktBom
  llUpd = .F.
  IF lcIssRet = "I"
    lcCaptin= "\<Issue"
    lcIssTit = "Issue Date   :"
  ELSE
    lcCaptin= "\<Return"
    lcIssTit = "Returns Date :"
  ENDIF
  DO (gcScrDir + gcWinAppl + '\pomiss1.SPX')
  USE IN FABINV
  POP KEY
  RELEASE WINDOW (lcMisBTit)
  RELEASE PAD _Option OF _MSYSMENU
  RELEASE BAR 099 OF P01PU01
  RELEASE BAR 100 OF P01PU01
  llMissScr  = .F.
  llMainScr  = .T.
  llAutoScr  = .F.
ELSE
  IF lcIssRet = "I"
    =gfModalGen('TRM38094B00000','ALERT','fabric/color: '+SUBSTR(&lcTmpQuery..Item,1,7)+'/'+&lcTmpQuery..Color)
  ELSE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"No open PO's found to be returned. Cannot proceed.")
  ENDIF
ENDIF
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfMIssBrow
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To handle the issue/return browse
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfMIssBrow()
*!*************************************************************
FUNCTION lfMIssBrow
SELECT &lcOpenLots
GO TOP

IF lcIssRet = "I"
  lcBrfield2 = [cMark     :02  :H = ''         :R,] +;
               [cWareCode :15  :H = 'Warehouse':R,] +;
               [PofHdr.PoMat:10  :H = 'Lot No.':R,] +;
               [cRSession :10  :H = 'Session'  :R,] +;
               [nBalance  :14  :H = 'On Hand'  :R,] +;
               [nUnitCost :10  :H = 'Unit Cost':R,] +;
               [nToIssue  :14  :H = 'Issued'    :W=lfGetOld() :V=lfvMiQty(.T.)]

ELSE
  lcBrfield2 = [cMark    :02  :H = ''         :R,] +;
               [cWareCode:15  :H = 'Warehouse':R,] +;
               [Po       :10  :H = 'Lot No.':R,] +;
               [cRSession:10  :H = 'Session'  :R,] +;
               [nBalance :14  :H = 'On Hand'  :R,] +;
               [nUnitCost:10  :H = 'Unit Cost':R,] +;
               [nBalance :10  :H = 'On Hand':R,] +;
               [nToIssue :14  :H = 'Issued' :R,]+;
               [nReturn  :14  :H = 'Returned' :W=lfGetOld() :V=lfvRetQty()]

ENDIF
BROWSE FIELDS &lcBrField2;
       WHEN lfwBrows("MI");
       VALID :F lfvBrowse("MI") ;
       WINDOW POMISSB IN WINDOW (lcMiHost);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE ALLTRIM(lcMisBTit)


*!*************************************************************
*! Name      : lfvMiQty
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To check the issued and returned qty.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvMiQty()
*!*************************************************************
FUNCTION lfvMiQty
PARAMETERS llFrmManu
PRIVATE llDistPrim
llDistPrim = .F.
SELECT (lcOpenLots)

IF nToIssue < 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              "Issued quantity should not be negative.")
  REPLACE nToIssue WITH lcOldVal
  RETURN
ENDIF

IF nToIssue > nBalance
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              "Issued quantity cannot exceed open lot quantity. Cannot proceed.")
  REPLACE nToIssue WITH lcOldVal
ELSE  
  
  llUpd = (nToIssue <> lcOldVal)

  *C200198,1 KHM 06/25/2001 (Begin) Added
  llDistDone = (nToIssue <> 0)
  *C200198,1 KHM 06/25/2001 (End)

  lnSeeIssu = lnSeeIssu + (nToIssue - lcOldVal)
  
  *C200198,1 KHM 06/25/2001 (Begin) Commented the following line and move it
  *C200198,1                to validation of the close button.
  *IF llFrmManu
    *IF lfChkPrim() && Function to check if issueed F less than one style req.
    *  llStrt = .F.
    *  lldymmy =llDistPrim .AND. lfDisFOnS(lnSeeIssu,&lcTmpQuery..Unit,&lcTmpQuery..Item,&lcTmpQuery..Color)  && Function to Distri Fabric on Style.
    *  llDummy = llStrt .AND. lfIssMat(lcMiItm,lcMiClr,nToIssue,cWareCode,nUnitCost)
    *  SELECT (lcOpenLots)
    *  REPLACE lUpdate  WITH !llStrt
    *ENDIF
  *ENDIF
  *C200198,1 KHM 06/25/2001 (End)
ENDIF

*!*************************************************************
*! Name      : lfGetOld
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To save the old value.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetOld()
*!*************************************************************
FUNCTION lfGetOld
SELECT (lcOpenLots)
lcOldVal = nToIssue

*!*************************************************************
*! Name      : lfvMiss2
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To issue or return materials.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetOld()
*!*************************************************************
FUNCTION lfvMiss2
PARAMETER lcIssRet1

PRIVATE lnOldAls

lnOldAls = SELECT(0)
*llEdit = .T.
IF lcIssRet = "I"
  *---- IF no issue occure before then we are going to distribute the required
  *---- abone the issued.
  *-- IF (&lcTmpQuery..Issue=0)
  *-- Distribute only one time [Start]
  
  *B605687,1 KHM 04/15/2002 (Begin) Comment out the IF command and change
  *B605687,1                the parameter that will be passed to the 
  *B605687,1                function. Also recalcualted the quantity to be issued.  
  *IF llCanDist .AND. !llDistDone
  * *-- Distribute only one time [End  ]
  * =lfDistrib(lnMiReq)  && Function to distribute the required on the issued.
  *ENDIF
  SELECT (lcOpenLots)
  lnSavRec = RECNO()
  SUM ALL nToIssue TO lnOldIss 
  IF BETWEEN(lnSavRec,1,RECCOUNT())
    GOTO lnSavRec
  ENDIF
  lnMiToCal = MAX(lnMiReq - lnOldIss,0)
  =lfDistrib(lnMiToCal)   
  *B605687,1 KHM 04/15/2002 (End)
  
  KEYBOARD "{ALT+V}" CLEAR
ELSE
  =lfReturn()
  KEYBOARD "{ALT+V}" CLEAR
ENDIF
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfDistrib
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To distribute qty.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDistrib()
*!*************************************************************
FUNCTION lfDistrib
PARAMETER lnToDist

PRIVATE lnOldAls , lnTempIss
lnOldAls  = SELECT(0)
lnTempIss = lnToDist
SELECT (lcOpenLots)

*B605687,1 KHM 04/15/2002 (Begin) Comment out the SCAN.
*GO TOP
*SCAN
*  DO CASE
*    CASE lnTempIss = 0
*      EXIT
*    CASE lnTempIss > 0 .AND. lnTempIss <= nBalance
*      REPLACE nToIssue WITH lnTempIss
*      lnSeeIssu = lnSeeIssu + lnTempIss
*      *=lfIssMat(lcMiItm,lcMiClr,lnToDist,cWareCode,nUntCstBuy)
*      lnTempIss = IIF(lnTempIss - nToIssue <0,0,lnTempIss - nToIssue)
*    CASE lnTempIss > 0 .AND. lnTempIss > nBalance
*      REPLACE nToIssue WITH nBalance
*      lnSeeIssu = lnSeeIssu + nBalance
*      *=lfIssMat(lcMiItm,lcMiClr,nBalance,cWareCode,nUntCstBuy)
*      lnTempIss = IIF(lnTempIss - nBalance <0,0,lnTempIss - nBalance)
*  ENDCASE
*ENDSCAN
*llDistDone = .T.
lnSeeIssu = lnSeeIssu - nToIssue
REPLACE nToIssue WITH MIN(lnTempIss, nBalance)
lnSeeIssu = lnSeeIssu + nToIssue
*B605687,1 KHM 04/15/2002 (End)
llUpd      = .T.
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfIssMat
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To actually issue materials
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfIssMat()
*!*************************************************************
FUNCTION lfIssMat
PARAMETER lcItem , lcColor , ln2Issue , lcWareH , lnBuyIssCost ,lcfrom

PRIVATE laOtherPar , lcOldAls
lcOldAls = SELECT(0)
lcLotNo = ""
DIMENSION laOtherPar[2,2]
laOtherPar[1,1] = 'lcOperCode'
laOtherPar[1,2] = lcOperCode
laOtherPar[2,1] = 'lcLotNo'
laOtherPar[2,2] = lcLotNo

*C200316,1 KHM 03/24/2002 (Begin) Initialize the necessary variables to
*C200316,1                hold update the gl entries.
*DIME laGLDistAr[1,1]
*laGLDistAr = ''
IF llGlLink
  =SEEK(PADR(lcItem,7)+lcColor,'Fabric')
  =SEEK(PADR(lcItem,7)+lcColor+lcWareH,'FabDye')
  lcFabLinkCode = IIF(EMPTY(Fabric.Link_Code),'DEFDEF',Fabric.Link_Code)
  lcFabLinkCode = IIF(EMPTY(FabDye.Gl_Link),lcFabLinkCode,FabDye.Gl_Link)

  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcFabLinkCode
  laGLDistAr[2,1] = PosHdr.LINK_CODE
  laGLDistAr[1,2] = '015'
  laGLDistAr[2,2] = '013'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'MA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE laData[1]  TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE gdSysDate  TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE lcGlYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcGlDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
  STORE ''         TO laGLDistAr[1,10],laGLDistAr[2,10]
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF
*C200316,1 KHM 03/24/2002 (End)

IF TYPE("lcfrom") = "C"
  lcResSess = &lcOpen..cRSession
ELSE
  lcResSess = &lcOpenLots..cRSession
ENDIF
lcIssSess = gfsequence('GLSession')
SELECT Fabric
=SEEK(lcItem + lcColor)
=lfUpdIssFil()
=gfMatCrl('4',lcItem,lcColor,lcWareH,'',ldMiDat,ldMiDat,;
          laData[1],-1*ln2Issue,lnBuyIssCost,;
          '','',0,'','',@laGLDistAr,'',"I",laData[1],lcResSess,lcIssSess,;
          '','','',@laOtherPar)

*C200316,1 KHM 03/24/2002 (Begin) Update the GL entries
IF llGlLink
  SELECT (lcGlDTemp)
  REPLACE ALL GlSession WITH lcIssSess
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcGlDTemp)
  SELECT (lcGlDTemp)
  ZAP
ENDIF  
*C200316,1 KHM 03/24/2002 (End)

SELECT(lcOldAls)

*!*************************************************************
*! Name      : lfUpdIssFil
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To update the necessary files
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpdIssFil()
*!*************************************************************
FUNCTION lfUpdIssFil
PRIVATE lcBomType

SELECT BOMLINE
*--- cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
lcBomType = ""
IF SEEK("I"+"1"+laData[1])
  LOCATE REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                    "I"+"1"+laData[1];
              FOR item+iclr = PADR(lcItem,19)+lcColor
  IF FOUND()
    lcBomType = cBomTyp
  ENDIF

  *C037959,1 MHM 09/20/2004 to update new fields in BOMLINE[Start]
   IF &lcTmpQuery..lFrmBom
      LOCATE REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                      "I"+"1"+laData[1];
                FOR item+iclr = PADR(lcItem,19)+lcColor AND style= &lcTmpQuery..Style
     IF FOUND()
        REPLACE Used_Qty  WITH Used_Qty  + ln2Issue ,;
                Issue_Qty WITH Issue_Qty + MAX(ln2Issue,0)
     ENDIF
   ENDIF             
  *C037959,1 MHM [End]
ENDIF
SELECT CTktBom
SET ORDER TO Ctktbom
*--- cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
IF SEEK("I"+laData[1]+lcBomType+PADR(lcItem,19)+lcColor+SPACE(6))
  REPLACE Used_Qty  WITH Used_Qty  + ln2Issue ,;
          Issue_Qty WITH Issue_Qty + MAX(ln2Issue,0)
ENDIF
SELECT BomCost
SET ORDER TO TAG Bomcstkt
*--- cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession
SET DELETE OFF
IF !SEEK(lcBomType+"I"+laData[1]+PADR(lcItem,19)+lcColor+;
         SPACE(6)+lcWareH +SPACE(10)+lcResSess+lcIssSess)
  APPEND BLANK
ELSE
  IF DELETED()
    BLANK
    RECALL
  ENDIF
ENDIF
SET DELETE ON
REPLACE cTktNo    WITH laData[1]  ,;
        cWareCode WITH lcWareH ,;
        cDyelot   WITH ""   ,;
        Item      WITH lcItem,;
        IClr      WITH lcColor,;
        cBomType  WITH lcBomType,;
        cIMTyp    WITH "I" ,;
        MfgCode   WITH SPACE(6)   ,;
        nTotQty   WITH nTotQty+ln2Issue,;
        nTotCst   WITH nTotCst +ln2Issue*lnBuyIssCost,;
        dTranDate WITH ldMiDat,;
        cRSession WITH lcResSess ,;
        cISession WITH lcIssSess ,;
        cCostType WITH "F" ,;
        nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
        nUnitACst WITH nUnitCst  ,;
        nTotACst  WITH nTotCst   ,;
        cOprCode  WITH lcOperCode ,;
        cLotNo    WITH lcLotNo

IF nTOtQty <= 0
    BLANK
    DELETE
ENDIF
SELECT POSHDR
=RLOCK()
REPLACE nAct_Cost&lcBomType WITH nAct_Cost&lcBomType + ln2Issue*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
UNLOCK

*!*************************************************************
*! Name      : lfChkPrim
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To check if we are issuing from the primary fabric.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfChkPrim()
*!*************************************************************
FUNCTION lfChkPrim

PRIVATE lnOldAls , llToRet

lnOldAls = SELECT(0)
llToRet = .T.
SELECT CtktBom
SET ORDER TO Ctktyp
*--- cimtyp+cuttkt+item+iclr+mfgcode+dyelot
SET ORDER TO POSLN IN POSLN

*C037959,1 MHM 09/20/2004 check for ORDBOM file[Start]

*IF SEEK("I"+laData[1]+PADR(lcMiItm,19)+lcMiClr);
  .AND. SEEK("P"+laData[1],'POSLN');
  .AND. SEEK(POSLN.Style,'Style') ;
  .AND. PADL(Style.Fabric,7) = PADL(lcMiItm,7)

IF SEEK("I"+laData[1]+PADR(lcMiItm,19)+lcMiClr);
  .AND. SEEK("P"+laData[1],'POSLN');
  .AND. SEEK(POSLN.Style,'Style') 
*C037959,1 MHM [End]
  
  *C200220,1 KHM 09/03/2001 (Begin) The following code is commented out 
  *C200220,1                because we will not diplay the message of 
  *C200220,1                recalculating the quantities unless the user 
  *C200220,1                presses the Recalculate button.
  *IF lnSeeIssu = 0
  *  SELECT (lcOpenLots)
  *  lcOldVal = 0
  *  REPLACE nToIssue WITH lcOldVal
  *  llToRet = .F.
  *ELSE
  *  llDistPrim = (&lcTmpQuery..Issue = 0)
  *ENDIF
  
*C037959,1 MHM 09/20/2004 check for ORDBOM file[Start]
  *llToRet = .T.
  IF lfChkOrdF(lcMiItm,lcMiClr)
    llToRet = .T.
  ELSE
    llToRet = .F.
  ENDIF
*C037959,1 MHM [End]

ELSE  
  llToRet = .F.
*C200220,1 KHM 09/03/2001 (End)
ENDIF
SELECT(lnOldAls)
RETURN(llToRet)

*!*************************************************************
*! Name      : lfDisFOnS
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To distribute the PO line qty according to the issued
*!             qty from the primary fabric.
*!*************************************************************
*! Example   : = lfDisFOnS()
*!*************************************************************
*--comment this function to write it according to new concept of multi fabric
FUNCTION lfDisFOnS1
PARAMETER lnFabQty , lnStyReq, lcFabric, lcColor
PRIVATE lnOldAls , lcMsgAns

lnOldAls = SELECT(0)

*C200220,1 KHM 09/03/2001 (Begin) Adding the following code in order to
*C200220,1                get the style and check if it has been received 
*C200220,1                before then do not recalculate the quantities.

*B605037,1 KHM 10/21/2001 (Begin) Calling this function to check the type of
*B605037,1                the fabric in the style cost sheet.
lcColor = lfvStyClr(lcFabric, lcColor)
*B605037,1 KHM 10/21/2001 (End)

STORE "" TO lcStyCode1,lcStyCode2
lnNoOfLn = lfGetPoLn(lcColor)
IF !EMPTY(lcStyCode1) AND SEEK('P'+laData[1]+lcStyCode1,'PosLn')
  SELECT PosLn
  LOCATE REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = ;
                    'P'+laData[1]+lcStyCode1;
              FOR   TranCd = '2'
  IF FOUND()
    = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              'Style '+ ALLTRIM(lcStyCode1) + ' has received quantities . Cannot proceed.')
    RETURN (.F.)
  ENDIF                  
ENDIF
*-- In case we have the extended style
IF !EMPTY(lcStyCode2) AND SEEK('P'+laData[1]+lcStyCode2,'PosLn')
  SELECT PosLn
  LOCATE REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = ;
                    'P'+laData[1]+lcStyCode2;
              FOR   TranCd = '2'
  IF FOUND()
    = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              'Style '+ ALLTRIM(lcStyCode2) + ' has received quantities . Cannot proceed.')
    RETURN (.F.)
  ENDIF                  
ENDIF
IF lnSeeIssu = 0 
  RETURN .F.
ENDIF
*C200220,1 KHM 09/03/2001 (End)

IF lnSeeIssu <> lnMiReq
  
  *C200198,1 KHM 06/25/2001 (Begin) Changing the message to display
  *C200198,1                the issued Fabric/Color.
  *lcMsgAns = gfModalGen('INM00000B00006',.F.,.F.,.F.,;
               "Fabric issued is "+ALLT(STR(ABS(lnSeeIssu-lnMiReq)))+;
               IIF(lnSeeIssu-lnMiReq>0," more"," less")+;
               " than required. Recalculate quantities?")
  
  lcMsgAns = gfModalGen('INM00000B00006',.F.,.F.,.F.,;
               "Fabric/Colour:"+ALLTRIM(lcFabric)+"/"+ALLTRIM(lcColor)+" issued is "+ALLT(STR(ABS(lnSeeIssu-lnMiReq)))+;
               IIF(lnSeeIssu-lnMiReq>0," more"," less")+;
               " than required. Recalculate quantities?")
  *C200198,1 KHM 06/25/2001 (End)

*C200220,1 KHM 09/03/2001 (Begin) if the required qty = used qty then we
*C200220,1                will not display the message but we will allow 
*C200220,1                the user to recalculate the qty
ELSE
  lcMsgAns = 1
ENDIF
*C200220,1 KHM 09/03/2001 (End)
*-- If the user agrees to recalculate the Po lines quantities.
IF lcMsgAns = 1
  IF SEEK("P"+ladata[1],'PosHdr')
    *-- If there is only one line in the PO
  

    *C200198,1 KHM 06/25/2001 (Begin) Adding the following code to check
    *C200198,1                whether the PO has one or two lines
    *STORE "" TO lcStyCode1,lcStyCode2
    lnNoOfLn = lfGetPoLn(lcColor)
    IF lnNoOfLn = 1
    *C200198,1 KHM 06/25/2001 (End)
      
      *-- To distribute the qty for the first line
      =lfDistLin1(lcColor)
    ELSE
      =lfDistLin2(lcColor)
    ENDIF
  ENDIF
ENDIF 

*C200220,1 KHM 09/03/2001 (Begin) Commented out
*ENDIF
*C200220,1 KHM 09/03/2001 (End)

SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfvApply
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To amended the po line qty.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvApply()
*!*************************************************************
FUNCTION lfvApply
PARAMETERS lcType
PRIVATE lnAlias
lnAlias = SELECT()

IF lcType = '1'
  IF m.TotQty <> lnTotOld
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Recalculated quantities should "+;
                "be equal to "+ALLTRIM(STR(lnTotOld))+". Cannot proceed.")
    RETURN
  ENDIF
  CLEAR READ
  llStrt = .T.
  *-- Update the recalculated qty.
  =lfUpdPOLin('A')
  

  *C200220,1 KHM 09/03/2001 (Begin) allow the user to edit the unit quantity
  *C200220,1                and do the necessary updates.
  =lfvUntQty('B')
  *C200220,1 KHM 09/03/2001 (End)

  *C200198,1 KHM 06/25/2001 (Begin) Call lfUpdTemp to update the required
  *C200198,1                quantities in the outer browse after 
  *C200198,1                distributing the quantities.
  =lfUpdTemp()
  *C200198,1 KHM 06/25/2001 (End)

ELSE
  IF m.TotQty <> lnTotOld
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Recalculated quantities should "+;
                "be equal to "+ALLTRIM(STR(lnTotOld))+". Cannot proceed.")
    RETURN
  ENDIF
  
  IF lfAdjLines()
    SELECT (lcTmpPoLn)
    SCAN
      SCATTER MEMVAR
      =SEEK('P'+laData[1]+Style+STR(LineNo,6)+'1','PosLn')
      *-- Update the recalculated qty.    
      =lfUpdPOLin('A')
    ENDSCAN

    *C200220,1 KHM 09/03/2001 (Begin) allow the user to edit the unit quantity
    *C200220,1                and do the necessary updates.
    =lfvUntQty('B')
    *C200220,1 KHM 09/03/2001 (End)

    *C200198,1 KHM 06/25/2001 (Begin) Call lfUpdTemp to update the required
    *C200198,1                quantities in the outer browse after 
    *C200198,1                distributing the quantities.
    =lfUpdTemp()
    *C200198,1 KHM 06/25/2001 (End)
  ELSE
    RETURN  
  ENDIF  
ENDIF
CLEAR READ
SELECT(lnAlias)


*!*************************************************************
*! Name      : lfUpdTemp
*! Developer : Khalid Mohi El-Din
*! Date      : 06/25/2001
*! Purpose   : To update the required quantities after updating the
*!             recalculated quantities of the PO lines.
*!*************************************************************
*! Example   : = lfUpdTemp()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfUpdTemp
PRIVATE lnAlias, lnRecNo

SELECT (lcTmpQuery)
lnRecNo = RECNO()

SELECT CtktBom
*--- cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
=SEEK('I'+laData[1])
SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot =;
                'I'+laData[1];
          FOR   cCatgTyp $ "F|T"
  SCATTER MEMVAR MEMO
  SELECT (lcTmpQuery)
  *C037959,1 MHM 09/20/2004 change seek criteria [Start]
  *IF SEEK(PADL(m.item,7)+m.IClr)
  IF SEEK(m.cCatgTyp+PADL(m.item,7)+m.IClr)
  *C037959,1 MHM [END]
  
    REPLACE Req     WITH m.Req_Qty
  ENDIF
ENDSCAN

SELECT (lcTmpQuery)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF  

lnAlias = SELECT()
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvSQty
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To validate the size qty in order to not exceeded the req. qty.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSQty()
*!*************************************************************
FUNCTION lfvSQty
PARAMETER lcQtyNo, llScr

PRIVATE lnTempTot , lnCurTmp
lnCurTmp = 0
IF llScr
  FOR lnInde = 1 TO Scale.Cnt
    lcIndex = STR(lnInde,1)
    lnCurTmp = lnCurTmp + m.Qty&lcIndex
  ENDFOR   
ELSE
  FOR lnInde = 1 TO 12
    lcIndex = ALLTRIM(STR(lnInde))
    lnCurTmp = lnCurTmp + m.Qty&lcIndex
  ENDFOR   
ENDIF
IF m.Qty&lcQtyNo < 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Quantity should be greater than 0. Cannot proceed.")
   
   m.Qty&lcQtyNo = lnOldQty&lcQtyNo
  _CUROBJ = OBJNUM(m.Qty&lcQtyNo)
ENDIF
m.TotQty = 0
IF llScr
  FOR lnInde = 1 TO Scale.Cnt
    lcIndex = STR(lnInde,1)
    m.TotQty = m.TotQty + m.Qty&lcIndex
  ENDFOR   
ELSE
  FOR lnInde = 1 TO 12
    lcIndex = ALLTRIM(STR(lnInde))
    m.TotQty = m.TotQty + m.Qty&lcIndex
  ENDFOR   
ENDIF

IF m.TotQty = 0
  SHOW GET pbApply DISABLE
ELSE
  SHOW GET pbApply ENABLE
ENDIF
SHOW GET m.TotQty

*!*************************************************************
*! Name      : lfUpdIssu
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To update issued qty.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpdIssu()
*!*************************************************************
FUNCTION lfUpdIssu
PRIVATE llDistPrim
llDistPrim = .F.
*C200198,1 KHM 06/25/2001 (Begin) Checking if we are issuing and the issued
*C200198,1                quantity is > 0 and this is the first time to 
*C200198,1                issue the primary fabric and the issued quantity 
*C200198,1                is > or < than the required. Then display the 
*C200198,1                recalculation screen

*C200220,1 KHM 09/03/2001 (Begin) The following code is commented out because
*C200220,1 KHM 09/03/2001 will not recalculate the qty from this function.
*IF lcIssRet <> "R" AND lnSeeIssu > 0 AND lfChkPrim() && Function to check if issueed F less than one style req.
*  llStrt = .F.
*  lldymmy =llDistPrim .AND. lfDisFOnS(lnSeeIssu,&lcTmpQuery..Unit,&lcTmpQuery..Item,&lcTmpQuery..Color)  && Function to Distri Fabric on Style.
*ENDIF  
*C200220,1 KHM 09/03/2001 (End)

*C200198,1 KHM 06/25/2001 (End)

IF llUpd
   REPLACE &lcTmpQuery..Issue WITH &lcTmpQuery..Issue+lnSeeIssu,;
           &lcTmpQuery..Used  WITH &lcTmpQuery..Used+lnSeeIssu,;
           &lcTmpQuery..Onhnd WITH &lcTmpQuery..OnHnd - lnSeeIssu
ENDIF
IF lcIssRet = "R"
  =lfAplyRet()
  SELECT (lcTmpQuery)
  *REPLACE &lcTmpQuery..Used  WITH &lcTmpQuery..Used-lnTotRet
   REPLACE &lcTmpQuery..Used  WITH &lcTmpQuery..Used-lnTotRet,;
           &lcTmpQuery..Onhnd WITH &lcTmpQuery..OnHnd + lnTotRet
  lnTotRet = 0
ELSE
  SELECT (lcOpenLots)
  GO TOP
  *C200316,1 KHM 03/24/2002 (Begin) Check if there are quantity to issue then
  *C200316,1                check the GL period.
  STORE '' TO lcGlYear,lcGlPeriod
  LOCATE FOR nToIssue > 0

  *B131603,1 HBG 04/11/2006 (BEGIN) Remove checking for period that beccause it already checked in the start
  *IF !FOUND() OR (llGlLink AND !CHECKPRD(gdSysDate,'lcGlYear','lcGlPeriod','IA'))
  IF !FOUND() 
  *B131603,1 HBG (END)

    RETURN
  ENDIF
  LOCATE
  *C200316,1 KHM 03/24/2002 (End)

  SCAN FOR lUpdate .AND. nToIssue <> 0
    =lfIssMat(cFabric,cColor,nToIssue,cWareCode,nUnitCost)
  ENDSCAN

ENDIF
*-- [End  ] Local fn to update Issued & used quantitis

*!*************************************************************
*! Name      : lfmatOnHnd
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To get the material on hand that has been assigned to SO
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfmatOnHnd()
*!*************************************************************
FUNCTION lfmatOnHnd
PARAMETERS lcType

IF TYPE("lcType") <> "C"
  *SELECT cRSession,cISession,cTran,SUM(nReceived-nIssued) AS 'nBalance';
         ,nIssued AS nToIssue ,dTranDate, SPACE(6) AS cMatPo, nUnitCost FROM MatInvJl ;
  WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
         LEFT(lcFabric,7)+LEFT(lcColor,6);
  GROUP BY cFabric,cColor,cWareCode,cDyelot,cRSession,cISession ;
  HAVING nBalance > 0 INTO DBF (gcWorkDir+lcOpen)
  
  SELECT cRSession,cISession,cTran,SUM(nReceived-nIssued) AS 'nBalance';
         ,nIssued AS nToIssue ,dTranDate, SPACE(6) AS cMatPo, nUnitCost FROM MatInvJl ;
  WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
         LEFT(lcFabric,7)+LEFT(lcColor,6);
  GROUP BY cFabric,cColor,cWareCode,cDyelot,cISession ;
  HAVING nBalance > 0 INTO DBF (gcWorkDir+lcOpen)

ELSE
  SELECT cRSession,cISession,cTran,SUM(nReceived-nIssued) AS 'nBalance';
         ,nIssued AS nToIssue ,dTranDate, SPACE(6) AS cMatPo, nUnitCost, cWareCode FROM MatInvJl ;
  WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
         LEFT(lcFabric,7)+LEFT(lcColor,6);
  GROUP BY cFabric,cColor,cWareCode,cDyelot,cRSession ;
  HAVING nBalance > 0 INTO DBF (gcWorkDir+lcOpen)
ENDIF
SELECT (lcOpen)
INDEX ON cRSession+cISession+cTran TAG (lcOpen)
SCAN
  IF SEEK(PADL(lcfabric,7)+lccolor,'MatInvJl')
    SELECT MatInvJl
    LOCATE REST WHILE cFabric+cColor+cWarecode+cDyelot+cRSession+cISession=;
                      lcFabric+lcColor FOR cRSession = &lcopen..cRSession AND;
                      cTranType = '1'
    IF FOUND()
      SELECT (lcOpen)
      REPLACE cMatPo WITH MatInvJl.cTran
      
      IF TYPE("lcType") <> "C"
        REPLACE nbalance WITH nbalance - nToIssue
      ENDIF
    ENDIF
  ENDIF  
ENDSCAN

*!*************************************************************
*! Name      : lfCrMatTmp
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To create the temporary file
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCrMatTmp()
*!*************************************************************
FUNCTION lfCrMatTmp

SELECT SPACE(02) AS cMark ,SPACE(10) AS PO ,SPACE(02) AS cSelect ,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,nUnitCost,;
       cApInvNo,dTranDate,nRecCost,cTran,SUM(nReceived-nIssued) AS 'nBalance',;
       SPACE(20) AS Desc ,0000000000.00 AS nReq,0000000000.00 AS Issued,0000000000.00 AS Used,;
       nUntCstBuy, 0000000000.00 AS nToIssue ,SPACE(03) AS UOM ,nReceived,00000000.00 AS nReturn ,.T. AS lUpdate FROM MatInvJl;
WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
       "*************************";
GROUP BY cFabric,cColor,cWareCode,cDyelot,cRSession ;
HAVING nBalance > 0 INTO DBF (gcWorkDir+lcOpenLots)

*!*************************************************************
*! Name      : lfReturn
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : Valid function for the return button.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfReturn()
*!*************************************************************
FUNCTION lfReturn
PRIVATE lnOldAls

lnOldAls = SELECT(0)
SELECT (lcOpenLots)
SHOW WINDOW (lcMisBTit) REFRESH
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfvRetQty
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To validate the returned qty.
*!*************************************************************
*! Example   : = lfvRetQty()
*!*************************************************************
FUNCTION lfvRetQty

IF nReturn < 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              "Returned quantity should not be negative.")
  REPLACE nReturn WITH 0
  RETURN
ENDIF

IF nReturn > nToIssue
  =gfModalGen('TRM38057B00000','ALERT')
  REPLACE nReturn WITH 0
  SHOW WINDOW (lcMisBTit) REFRESH
ELSE
  lnTotRet   = lnTotRet   + nReturn
ENDIF

*!*************************************************************
*! Name      : lfAplyRet
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : Valid function to apply the returned qty.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAplyRet()
*!*************************************************************
FUNCTION lfAplyRet

SELECT (lcOpenLots)
*C200316,1 KHM 03/24/2002 (Begin) Check if there are quantity to issue then
*C200316,1                check the GL period.
STORE '' TO lcGlYear,lcGlPeriod
LOCATE FOR nReturn <> 0
IF !FOUND() OR (llGlLink AND !CHECKPRD(gdSysDate,'lcGlYear','lcGlPeriod','IA'))
  RETURN
ENDIF
LOCATE
*C200316,1 KHM 03/24/2002 (End)

SCAN FOR nReturn <> 0
  ln2Issue = -nReturn
  lcItem = &lcTmpQuery..Item
  lcColor= &lcTmpQuery..Color
  lcWareH= cWareCode
  lnBuyIssCost = nUnitCost
  lcResSess = &lcOpenLots..cRSession
  lcIssSess = &lcOpenLots..cISession
  lcLotNo = ""
  DIMENSION laOtherPar[2,2]
  laOtherPar[1,1] = 'lcOperCode'
  laOtherPar[1,2] = lcOperCode
  laOtherPar[2,1] = 'lcLotNo'
  laOtherPar[2,2] = lcLotNo

  *C200316,1 KHM 03/24/2002 (Begin) Initialize the necessary variables to upate
  *C200316,1                the gl entries.
  *DIME laGLDistAr[1,1]
  *laGLDistAr = ''
  IF llGlLink
    =SEEK(PADR(lcItem,7)+lcColor,'Fabric')
    =SEEK(PADR(lcItem,7)+lcColor+lcWareH,'FabDye')
    lcFabLinkCode = IIF(EMPTY(Fabric.Link_Code),'DEFDEF',Fabric.Link_Code)
    lcFabLinkCode = IIF(EMPTY(FabDye.Gl_Link),lcFabLinkCode,FabDye.Gl_Link)

    DECLARE laGLDistAr[2,13]
    laGLDistAr[1,1] = lcFabLinkCode
    laGLDistAr[2,1] = PosHdr.LINK_CODE
    laGLDistAr[1,2] = '015'
    laGLDistAr[2,2] = '013'
    laGLDistAr[1,3] = 1
    laGLDistAr[2,3] = -1
    STORE 'MA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
    STORE laData[1]  TO laGLDistAr[1,5],laGLDistAr[2,5]
    STORE gdSysDate  TO laGLDistAr[1,6],laGLDistAr[2,6]
    STORE lcGlYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
    STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
    STORE lcGlDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
    STORE ''         TO laGLDistAr[1,10],laGLDistAr[2,10]
  ELSE
    DIME laGLDistAr[1,1]
    laGLDistAr = ''  
  ENDIF
  *C200316,1 KHM 03/24/2002 (End)

  =lfUpdIssFil()
  =gfMatCrl('4',lcItem,lcColor,lcWareH,'',ldMiDat,ldMiDat,;
            laData[1],-1*ln2Issue,lnBuyIssCost,;
           '','',0,'','',@laGLDistAr,'',"I",laData[1],lcResSess,lcIssSess,;
           '','','',@laOtherPar)

  *C200316,1 KHM 03/24/2002 (Begin) Update the GL entries.
  IF llGlLink
    SELECT (lcGlDTemp)
    REPLACE ALL GlSession WITH lcIssSess
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lcGlDTemp)
    SELECT (lcGlDTemp)
    ZAP
  ENDIF
  *C200316,1 KHM 03/24/2002 (End)

ENDSCAN

*!*************************************************************
*! Name      : lfBomCst
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To get unit cost from bomcost
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfBomCst()
*!*************************************************************
FUNCTION lfBomCst
SELECT BOMCOST
SET ORDER TO Pobomcls
*--- cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession
=SEEK("I"+laData[1])
SCAN REST WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession=;
                "I"+laData[1];
          FOR   Item+Iclr = PADR(lcFabric,19)+PADR(lcColor,6)
  SCATTER MEMVAR MEMO
  INSERT INTO (lcOpenLots) FROM MEMVAR
  SELECT (lcOpenLots)
  REPLACE nToIssue  WITH m.nTotQty,;
          nUnitCost WITH m.nUnitCst
ENDSCAN


*---- Start Auto Isssue Functions
*!*************************************************************
*! Name      : lfvAuIss
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : Valid function for the automatic issue button
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvAuIss()
*!*************************************************************
FUNCTION lfvAuIss

*B131603,1 HBG 04/11/2006 (BEGIN) initialize new flag for first Validation only
STORE .F. TO llChecked
*B131603,1 HBG (END)

PUSH KEY
ON KEY
ON KEY LABEL ESC DO lpTrapESC
lcAutIssTt = "Temp File"
lcClose    = gcBmpHome + "CLOSE1.BMP"
lcPrompt   = "S\<elect"
lnSeeIssu  = 0
lcOldVal   = ""
ldMiDat    = gdSysDate
lcMiItm    = &lcTmpQuery..Item
lcMiClr    = &lcTmpQuery..Color
lnMiReq    = &lcTmpQuery..REQ
lcOk       = gcBmpHome + "OK.BMP"
lcCancel   = gcBmpHome + "CAN.BMP"
lcAutIssTt = 'Issue Cost Items'
ldIssDate  = gdSysDate
=lfCrMatTmp()  && Functino to create temp file.
IF lfGetMat()
  GO TOP
  DO (gcScrDir + gcWinAppl + '\POAUISS.SPX')
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              "There are no open PO's found. Cannot proceed.")

ENDIF
POP KEY
KEYBOARD "{ALT+B}" CLEAR

*!*************************************************************
*! Name      : lfAutBrows
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : Browse for the automatic issue screen
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAutBrows()
*!*************************************************************
FUNCTION lfAutBrows

PRIVATE lcBrowFild

SELECT (lcOpenLots)
lnIMarker = RECNO()

lcBrowFild = [cMark    :02  :H = ''        :R,] +;
             [cSelect  :02  :H = ''        :R,] +;
             [cFabric  :10  :H = 'Item'    :R,] +;
             [Desc     :20  :H = 'Desc.'   :R,] +;
             [cColor   :07  :H = 'Colour'  :R,] +;
             [UOM      :05  :H = 'UOM'     :R,] +;
             [nBalance :15  :H = 'On Hand' :R,] +;
             [nReq     :15  :H = 'Required':R,] +;
             [nToIssue :15  :H = 'Issue' :W = lfGetOld() :V=lfvMiQty(.F.),]+;
             [Used   :15  :H = 'Used'      :R,] +;
             [Issued   :15  :H = 'Issued'  :R]

BROWSE FIELDS &lcBrowFild ;
       WINDOW POAUISS2   ;
       WHEN lfwBrows("AI");
       VALID :F lfvBrowse("AI") ;
       IN WINDOW POAUISS ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       TITLE lcAutIssTt


*!*************************************************************
*! Name      : lfReadAct1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : Read activate for automatic issue screen
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfReadAct1()
*!*************************************************************
FUNCTION lfReadAct1

IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcAutIssTt)

*!*************************************************************
*! Name      : lfDMainLot
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : De-activate for automatic issue screen
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDMainLot()
*!*************************************************************
FUNCTION lfDMainLot

ON KEY LABEL CTRL+Q lnDummy = 1
ON KEY LABEL CTRL+W lnDummy = 1
ON KEY LABEL CTRL+HOME GO TOP
ON KEY LABEL CTRL+END  GO BOTTOM
IF WONTOP()=lcAutIssTt
    ON KEY LABEL TAB     DO lpTab1  WITH 'POAUISS3','pbISelect'
    ON KEY LABEL BACKTAB DO lpBTab WITH 'POAUISS1','ldIssDate'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfClearKey
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : Clear the keys
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

*!*************************************************************
*! Name      : lpTab1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To handle the tab button
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpTab1()
*!*************************************************************
PROCEDURE lpTab1
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL TAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lpBTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To handle the backtab button
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpBTab()
*!*************************************************************
PROCEDURE lpBTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL BACKTAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lfGetMat
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To get the open lots 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetMat()
*!*************************************************************
FUNCTION lfGetMat

PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT &lcTmpQuery
lcTmpKey = EVAL(Key())

*C037959,1 MHM 09/20/2004 dont include Fabric in case of automatic issue[Start]
*SCAN
SEEK('T')
SCAN REST WHILE cCatgTyp = "T"
*C037959,1 MHM [End]

  SCATTER MEMVAR MEMO
  SELECT (lcOpenLots)
  APPEND BLANK
  REPLACE cFabric    WITH m.Item,;
          cColor     WITH m.Color,;
          Desc       WITH IIF(SEEK(m.Item+m.Color,'Fabric'),Fabric.Desc,''),;
          nBalance   WITH lfvPODet(""),;
          UOM        WITH m.UOM,;
          nReq       WITH m.REQ,;
          Issued     WITH m.Issue,;
          Used       WITH m.Used

  IF nBalance=0
    BLANK
    DELETE
  ENDIF
ENDSCAN
=SEEK(lcTmpKey)
SELECT (lcOpenLots)
GO TOP
llToRet = !EOF()
SELECT(lnOldAls)
RETURN(llToRet )

*!*************************************************************
*! Name      : lfAdjIButt
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To handle the select/unselect buttons
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAdjIButt()
*!*************************************************************
FUNCTION lfAdjIButt
PARAMETER lcAction

PRIVATE lnOldAls

lnOldAls = SELECT(0)
SELECT (lcOpenLots)
lnNewRec = RECNO()
DO CASE
  CASE lcAction = "S"   && Select
    REPLACE cSelect WITH IIF(cSelect="�",' ',"�")
    lcPrompt = IIF(cSelect="�","\<Unselect","S\<elect")
    REPLACE nToIssue  WITH IIF(cSelect="�",nReq,0)
    *REPLACE nToIssue  WITH IIF(cSelect="�",nBalance,0)
  CASE lcAction = "A"   && Select All
    REPLACE ALL cSelect WITH "�"
    REPLACE ALL nToIssue  WITH nReq
    *REPLACE ALL nToIssue  WITH nBalance
    lcPrompt = "\<Unselect"
    GO TOP
  CASE lcAction = "N"   && Select None.
    REPLACE ALL cSelect WITH " "
    lcPrompt = "S\<elect"
    REPLACE ALL nToIssue  WITH 0
    GO TOP
  CASE lcAction = "V"   && Invert Selecttion.
    REPLACE ALL cSelect WITH IIF(cSelect="�",' ',"�")
    lcPrompt = IIF(cSelect="�","\<Unselect","S\<elect")
    REPLACE ALL nToIssue  WITH IIF(cSelect="�",nReq,0)
    *REPLACE ALL nToIssue  WITH IIF(cSelect="�",nBalance,0)
ENDCASE
GO lnNewRec
SHOW GET pbISelect,1 PROMPT lcPrompt
SHOW WINDOW (lcAutIssTt) REFRESH
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfvOkIssLt
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : Validate the OK button in the automatic issue screen
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvOkIssLt()
*!*************************************************************
FUNCTION lfvOkIssLt
PRIVATE lnOldAls , lnToIss, lnUnitCost, llDistPrim, lnMiReq
llDistPrim = .F.

lnOldAls = SELECT(0)
lnUnitCost = 0
SELECT CtktBom
SET ORDER TO Ctktyp
*--- cimtyp+cuttkt+item+iclr+mfgcode+dyelot
=SEEK("I"+laData[1]+PADR(lcMiItm,19)+lcMiClr)
lcOperCode = cOprCode
DECLARE laLots[1]
=gfOpenFile(gcDataDir+"mfgoprdt",gcDataDir+"mfgoprdt","SH")
SELECT DIST cLotNo FROM mfgoprdt ;
WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot= "I"+laData[1]+lcOperCode ;
AND TranCd='1'INTO ARRAY laLots

SELECT CtktBom
SET ORDER TO CtktBom
SELECT (lcOpenLots)

*C200316,1 KHM 03/24/2002 (Begin) Check any quantity to issue and check the
*C200316,1                fiscal year and period.
STORE '' TO lcGlYear,lcGlPeriod
LOCATE FOR cSelect="�" .AND. nToIssue<>0

*B131603,1 HBG 04/11/2006 (BEGIN) Remove checking for period that beccause it already checked in the start
*IF !FOUND() OR (llGlLink AND !CHECKPRD(gdSysDate,'lcGlYear','lcGlPeriod','IA'))
IF !FOUND() 
*B131603,1 HBG (END)

  RETURN
ENDIF
LOCATE
*C200316,1 KHM 03/24/2002 (End)

SCAN FOR cSelect="�" .AND. nToIssue<>0
  lcWare  = IIF(SEEK("P"+laData[1],'POSHDR'),POSHDR.cWareCode,"DEFWHE")
  lcFabric = PADL(cFabric,7)
  lcColor  = cColor
  *C037959,1 MHM 09/20/2004 dont include Fabric in case of automatic issue[Start]
  lcCatgTyp = "T"
  *C037959,1 MHM [End]
  
  lnAllIssu = nToIssue
  lcAllMPo = lfRetMAtPo(cFabric,cColor)
  
  =lfmatOnHnd("")
  SELECT (lcOpen)
  INDEX ON DTOS(dTranDate) TAG (lcOpen)
  llFound = .F.
  SCAN FOR cMatPo $ lcAllMPo
    SELECT MatInvJl
    SEEK lcFabric+lcColor
    LOCATE REST WHILE cFabric+cColor+cWarecode+cDyelot+cRSession+cISession=;
                      lcFabric+lcColor FOR cRSession = &lcopen..cRSession AND;
                      cTranType = '1' AND dTranDate = &lcOpen..dTranDate AND;
                      cTran = &lcOpen..cMatPo
    =SEEK(lcFabric+lcColor,'Fabric')
    *lnIssCost = nUnitCost
    lnMiReq = &lcOpenLots..nreq
    
    SELECT (lcOpen)
    IF lnAllIssu > 0
      llFound = .T.
      lnToIss = MIN(lnAllIssu,nBalance)
      lnAllIssu = lnAllIssu - lnToIss
      
      *C037959,1 MHM 09/20/2004 change seek criteria[Start]
      *=SEEK(lcFabric+lcColor,lcTmpQuery)     
      =SEEK(lcCatgTyp+ lcFabric+lcColor,lcTmpQuery)     
      *C037959,1 MHM 09/20/2004 [ENd]
      
       lnSeeIssu = lnToIss
       
       *C200220,1 KHM 09/03/2001 (Begin) Commented out because we have a 
       *C200220,1 KHM 09/03/2001 recalculation button that will do the work.
       *=lfChkPrimF(lcFabric, lcColor, lnToIss)
       *C200220,1 KHM 09/03/2001 (End)
       
      REPLACE &lcTmpQuery..Issue WITH &lcTmpQuery..Issue+lnToIss,;
              &lcTmpQuery..Used  WITH &lcTmpQuery..Used+lnToIss ,;
              &lcTmpQuery..OnHnd WITH &lcTmpQuery..OnHnd - lnToIss
     lnIssCost = &lcOpen..nUnitCost
     lcware    = &lcOpen..cwarecode         
     =lfIssMat(&lcOpenLots..cFabric,&lcOpenLots..cColor,lnToIss,lcWare,lnIssCost,"A")
     
    ENDIF
  ENDSCAN
  IF !llFound
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"No open PO's found to issue from.")
  ENDIF
ENDSCAN
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfvEditQty
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : Validate the sizes qty in the distribution screen
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvEditQty()
*!*************************************************************
FUNCTION lfvEditQty
PARAMETERS lcType

IF lcType = '1'
  FOR lnCount = 1 TO Scale.Cnt
    lcCount = STR(lnCount,1)
    SHOW GET m.Qty&lcCount ENABLE
  ENDFOR
ELSE
  SELECT (lcTmpPoLn)
  lnLine = 0
  SCAN
    lnLine = lnLine + 1
    =SEEK('S'+Scale,'Scale')
    FOR lnInde = 1 TO Scale.Cnt
      lcIndex = STR(lnInde,1)
      lnCount  = lnInde + IIF(lnLine = 1,0,8)
      lcCount  = ALLTRIM(STR(lnCount))
      SHOW GET m.Qty&lcCount ENABLE
    ENDFOR
  ENDSCAN
ENDIF
_CUROBJ = OBJNUM(m.Qty1)
SHOW GET pbEditQty DISABLE

*!*************************************************************
*! Name      : lpTrapESC
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : Validate Escape Key
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpTrapESC()
*!*************************************************************
PROCEDURE lpTrapESC


*!*************************************************************
*! Name      : lfRetMAtPo
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : Get the open lots 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRetMAtPo()
*!*************************************************************
FUNCTION lfRetMAtPo
PARAMETER lcFabric,lcColor
PRIVATE lnOldAls , lcToRet

lnOldAls = SELECT(0)
lcToRet = ""
SELECT cutpick
lcOrder = IIF(SEEK("2"+laData[1]),Order,SPACE(06))
SELECT mapoalo
SET ORDER TO Maposo
IF SEEK(lcOrder+lcFabric+lcColor)
  SCAN REST WHILE order+fabric+color = lcOrder+lcFabric+lcColor
    lcToRet = lcToRet+"|"+PoMat
  ENDSCAN
ENDIF
SELECT(lnOldAls)
RETURN(lcToRet)


*!*************************************************************
*! Name      : lfUpdPOLin
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To update the PO lines qty
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpdPOLin()
*!*************************************************************
FUNCTION lfUpdPOLin
PARAMETERS lcType

*-- Initialize the necessary variables.
lnOpnAmt = 0
lcSign   = '+'
lcOpSign = '-'

*-- Update the first PO line
*C200198,1 KHM 06/25/2001 (Begin) Calling the function with parameter
*=lfUpd1stLn()
=lfUpd1stLn(lcType)
*C200198,1 KHM 06/25/2001 (End)

*-- To recalculate costs in the PosHdr file.
=lfRecalcPO()

*-- Recalculate Vendor's amount
SELECT PosHdr
=SEEK('P'+laData[1])
SELECT ApVendor
=SEEK(PosHdr.Vendor)
=RLOCK()
REPLACE nVenOpnPO WITH nVenOpnPO +lnOpnAmt
UNLOCK
*--Call TraceKey global function.
=gfTraceKey('APVENDOR',APVENDOR.cvendcode,'M')

*!*************************************************************
*! Name      : lfUpd1stLn
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To update the PO lines qty
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpd1stLn()
*!*************************************************************
FUNCTION lfUpd1stLn
PARAMETERS lcLType

*-- Modify the PO BOM
=lfUpdBomCs()

*-- Update the WIP in the style file
SELECT STYLE
=SEEK(PosLn.Style)
= RLOCK()
REPLACE WIP1 WITH WIP1 &lcSign m.QTY1 &lcOpSign POSLN.QTY1,;
        WIP2 WITH WIP2 &lcSign m.QTY2 &lcOpSign POSLN.QTY2,;
        WIP3 WITH WIP3 &lcSign m.QTY3 &lcOpSign POSLN.QTY3,;
        WIP4 WITH WIP4 &lcSign m.QTY4 &lcOpSign POSLN.QTY4,;
        WIP5 WITH WIP5 &lcSign m.QTY5 &lcOpSign POSLN.QTY5,;
        WIP6 WITH WIP6 &lcSign m.QTY6 &lcOpSign POSLN.QTY6,;
        WIP7 WITH WIP7 &lcSign m.QTY7 &lcOpSign POSLN.QTY7,;
        WIP8 WITH WIP8 &lcSign m.QTY8 &lcOpSign POSLN.QTY8,;
        TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8

*-- Update the WO in the style file  
REPLACE nWO1 WITH nWO1 &lcSign m.QTY1 &lcOpSign POSLN.QTY1,;
        nWO2 WITH nWO2 &lcSign m.QTY2 &lcOpSign POSLN.QTY2,;
        nWO3 WITH nWO3 &lcSign m.QTY3 &lcOpSign POSLN.QTY3,;
        nWO4 WITH nWO4 &lcSign m.QTY4 &lcOpSign POSLN.QTY4,;
        nWO5 WITH nWO5 &lcSign m.QTY5 &lcOpSign POSLN.QTY5,;
        nWO6 WITH nWO6 &lcSign m.QTY6 &lcOpSign POSLN.QTY6,;
        nWO7 WITH nWO7 &lcSign m.QTY7 &lcOpSign POSLN.QTY7,;
        nWO8 WITH nWO8 &lcSign m.QTY8 &lcOpSign POSLN.QTY8,;
        nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
UNLOCK  
=gfTraceKey('STYLE',STYLE.Style,'M')

*-- Update the WIP in the stydye file        
SELECT STYDYE
IF SEEK(PosLn.STYLE+Posln.cWareCode+SPACE(10))
  =RLOCK() 
  REPLACE WIP1 WITH WIP1 &lcSign m.QTY1 &lcOpSign POSLN.QTY1,;
          WIP2 WITH WIP2 &lcSign m.QTY2 &lcOpSign POSLN.QTY2,;
          WIP3 WITH WIP3 &lcSign m.QTY3 &lcOpSign POSLN.QTY3,;
          WIP4 WITH WIP4 &lcSign m.QTY4 &lcOpSign POSLN.QTY4,;
          WIP5 WITH WIP5 &lcSign m.QTY5 &lcOpSign POSLN.QTY5,;
          WIP6 WITH WIP6 &lcSign m.QTY6 &lcOpSign POSLN.QTY6,;
          WIP7 WITH WIP7 &lcSign m.QTY7 &lcOpSign POSLN.QTY7,;
          WIP8 WITH WIP8 &lcSign m.QTY8 &lcOpSign POSLN.QTY8,;
          TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8

  *-- Update the WO in the style file  
  REPLACE nWO1 WITH nWO1 &lcSign m.QTY1 &lcOpSign POSLN.QTY1,;
          nWO2 WITH nWO2 &lcSign m.QTY2 &lcOpSign POSLN.QTY2,;
          nWO3 WITH nWO3 &lcSign m.QTY3 &lcOpSign POSLN.QTY3,;
          nWO4 WITH nWO4 &lcSign m.QTY4 &lcOpSign POSLN.QTY4,;
          nWO5 WITH nWO5 &lcSign m.QTY5 &lcOpSign POSLN.QTY5,;
          nWO6 WITH nWO6 &lcSign m.QTY6 &lcOpSign POSLN.QTY6,;
          nWO7 WITH nWO7 &lcSign m.QTY7 &lcOpSign POSLN.QTY7,;
          nWO8 WITH nWO8 &lcSign m.QTY8 &lcOpSign POSLN.QTY8,;
          nTotWO WITH nWO1+nWO2+nWO3+nWO4+nWO5+nWO6+nWO7+nWO8
  UNLOCK            
  *--Call TraceKey global function.
  =gfTraceKey('STYDYE',STYDYE.style+STYDYE.cwarecode+STYDYE.dyelot,'M')
ENDIF
lnTotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8

lnOpnAmt = lnOpnAmt &lcSign (lnTotQty*PosLn.nECost1) &lcOpSign (POSLN.TotQty * POSLN.neCost1)
  
*-- Update the cut pick quantities.
*C200198,1 KHM 06/25/2001 (Begin) Updating the Allocated quantities according
*C200198,1                the passed parameter in order to be able to handle 
*C200198,1                the case of one PO line, two PO line, and PO that 
*C200198,1                is generated from a multi-store order or 
*C200198,1                multi-prepack.
IF lcLType = 'A'
  SELECT CUTPICK   
  IF SEEK('2'+laData[1]+POsLn.Style)
    =SEEK('O'+CUTPICK.Order,'ORDHDR')
    =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')

    FOR lnSizeNo = 1 TO 8
      lcSz = STR(lnSizeNo,1)
      IF m.Qty&lcSz < CutPick.Qty&lcSz AND CutPick.Qty&lcSz <> 0    
        SELECT OrdHdr
        
        *C200220,1 KHM 09/03/2001 (Begin) We should reduce the cutpick qty
        *C200220,1                not the posln qty.
        *REPLACE TotCut WITH (TotCut - PosLn.Qty&lcSz) + m.Qty&lcSz
        REPLACE TotCut WITH (TotCut - CutPick.Qty&lcSz) + m.Qty&lcSz
        *C200220,1 KHM 09/03/2001 (End)
        
        SELECT OrdLine
        *C200220,1 KHM 09/03/2001 (Begin) We should reduce the cutpick qty
        *C200220,1                not the posln qty.
        *REPLACE TotCut   WITH (TotCut - PosLn.Qty&lcSz) + m.Qty&lcSz,;
              Cut&lcSz WITH (Cut&lcSz - PosLn.Qty&lcSz) + m.Qty&lcSz
        REPLACE Cut&lcSz WITH (Cut&lcSz - CutPick.Qty&lcSz) + m.Qty&lcSz,;
                TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8              
        *C200220,1 KHM 09/03/2001 (End)
  
        *C200220,1 KHM 09/03/2001 (Begin) Commented out
        *SELECT CutPick
        *REPLACE TotQty   WITH (TotQty - PosLn.Qty&lcSz) + m.Qty&lcSz,;
                Qty&lcSz WITH (Qty&lcSz - PosLn.Qty&lcSz) + m.Qty&lcSz        
        *C200220,1 KHM 09/03/2001 (End)

        SELECT PosLn
        *C200220,1 KHM 09/03/2001 (Begin) We should reduce the cutpick qty
        *C200220,1                not the posln qty.
        *REPLACE TotOrd   WITH (TotOrd - PosLn.Qty&lcSz) + m.Qty&lcSz,;
                Ord&lcSz WITH (Ord&lcSz - PosLn.Qty&lcSz) + m.Qty&lcSz
        REPLACE Ord&lcSz WITH (Ord&lcSz - CutPick.Qty&lcSz) + m.Qty&lcSz,;
                TotOrd   WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8                
        *C200220,1 KHM 09/03/2001 (End)

        SELECT CutPick
        *C200220,1 KHM 09/03/2001 (Begin) Reducing the old qty then adding
        *C200220,1                the new one.
        REPLACE Qty&lcSz WITH (Qty&lcSz - CutPick.Qty&lcSz) + m.Qty&lcSz,;
                TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
                
        *C200220,1 KHM 09/03/2001 (End)

      ENDIF
    ENDFOR
    SELECT OrdHdr
    =gfTraceKey('ORDHDR','O'+CUTPICK.Order,'M')
    SELECT OrdLine   
    =gfTraceKey('ORDLINE','O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'M')
  ENDIF
ELSE

  llNothing = IIF(lcLType = 'B', lfUpdCtPk('1'), lfUpdCtPk('2'))
*C200198,1 KHM 06/25/2001 (End)
ENDIF

*-- Update PosLn quantities.
SELECT POSLN
m.cStyGrade = Style.cStyGrade
FOR lnCounter = 1 TO 8
  lcCounter = STR(lnCounter,1)
  REPLACE Qty&lcCounter WITH m.Qty&lcCounter
ENDFOR
REPLACE TotQty    WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
        cStyGrade WITH m.cStyGrade

=gfAdd_Info('POSLN')
SELECT POSLN
=gfTraceKey('POSLN',cStyType+Po+cRsession+Shipno+Style+STR(Lineno,6)+Trancd,'M')

RETURN



*-- Function to recalculate the costs in header.
*!*************************************************************
*! Name      : lfRecalcPO
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : Function to recalculate the costs in header.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRecalcPO()
*!*************************************************************
FUNCTION lfRecalcPO

lnAlias = SELECT()

SELECT PosLn

SUM TotQty,;
    (TotQty*nECost1),(TotQty*nECost2),(TotQty*nECost3),(TotQty*nECost4),(TotQty*nECost5),;
    (TotQty*nCost1),(TotQty*nCost2),(TotQty*nCost3),(TotQty*nCost4),(TotQty*nCost5);    
TO ;
    lnStyOrd,;
    lnECost1,lnECost2,lnECost3,lnECost4,lnECost5,;
    lnFCost1,lnFCost2,lnFCost3,lnFCost4,lnFCost5;
WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = 'P'+laData[1]    


lnPoTotal =lnECost1+lnECost2+lnECost3+lnECost4+lnECost5

=SEEK('P'+laData[1])

SELECT PosHdr
=SEEK('P'+laData[1])

FOR lnCounter = 1 TO 5
  lcCounter = STR(lnCounter,1)
  REPLACE nICost&lcCounter WITH lnECost&lcCounter,;
          nFCost&lcCounter WITH lnFCost&lcCounter
ENDFOR
REPLACE PoTotal   WITH nICost1+nICost2+nICost3+nICost4+nICost5,;
        nStyOrder WITH lnStyOrd,;
        Open      WITH MAX((lnStyOrd - Receive - Damage - Cancel),0)

=gfAdd_Info('POSHDR')
=gfTraceKey('POSHDR',POSHDR.cstytype+POSHDR.po,'M')

SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name      : lfUpdBomCs
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To update cTktBom and BOMLINE
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpdBomCs()
*!*************************************************************
FUNCTION lfUpdBomCs
PRIVATE lnAlias,lcTkBomTag, lnTkBomRec, lcBomLnTag, lnBomLnRec, lcPassFile

lnAlias    = SELECT(0)
lcPassFile = gfTempName() 
llRetVal   = .T.

=gfOpenFile(gcDataDir+'BOM',gcDataDir+'BOM','SH')
=gfOpenFile(gcDataDir+'BOMLINE',gcDataDir+'BOMLINE','SH')
=gfOpenFile(gcDataDir+'MFGOprHd',gcDataDir+'MFGOprHd','SH')

SELECT cTktBom
lcTkBomTag = SET("ORDER")
lnTkBomRec = RECNO()
SET ORDER TO cTktBom
= SEEK("I"+laData[1]) 
lcLinkCode  = CTktBom.Link_Code
lcWareCode  = CTktBom.cWareCode

llNoThing   = SEEK("P"+laData[1], "POSHDR")
lcLastOpr   = POSHDR.cLastOpr
lcStyle     = PosLn.Style
lnPoLNo     = PosLn.LineNo
lnPrice     = PosLn.nCost1
lcCostDye   = PADR(PosLn.Dyelot,10)
DIMENSION laBomQty[9]
FOR lnCountr = 1 TO 8
  lcCountr = STR(lnCountr,1)
  laBomQty[lnCountr] = m.Qty&lcCountr
ENDFOR
laBomQty[9] = 0

FOR lnSize = 1 TO 8
    lcField = "POSLN.Qty" + STR(lnSize,1)
    laBomQty[lnSize] = laBomQty[lnSize] - &lcField
  laBomQty[9] = laBomQty[9] + laBomQty[lnSize]
ENDFOR

SELECT cTktBom
lcTkBomTag = SET("ORDER")
lnTkBomRec = RECNO()

SELECT BomLine
lcBomLnTag = SET("ORDER")
lnBomLnRec = RECNO()
SET ORDER TO BomLine

*B606633,1 KHM 11/27/2002 (Begin) Handle the case of having the fabric/color more than once
*B606633,1                in the style cost sheet by simulating the bom file from bomline file.
*llRetVal = gfSheetItem ("I", laData[1] , lcLinkCode, lcStyle    ,;
                        "" , lnPoLNo   , lcCostDye , POSHDR.cItemWare ,;
                        POSHDR.cMatWare, @laBomQty ,  "Bom" , "CtktBom"  ,;
                        "BomLine", "MFGOprHd", lcLastOpr , lnPrice    ,;
                        0, 0, 0, 0, 0,lcPassFile)
=lfModBom()
*B606633,1 KHM 11/27/2002 (End)


SELECT cTktBom
SET ORDER TO &lcTkBomTag
IF BETWEEN(lnTkBomRec,1,RECCOUNT())
  GOTO lnTkBomRec
ENDIF

SELECT BomLine
SET ORDER TO &lcBomLnTag
IF BETWEEN(lnTkBomRec,1,RECCOUNT())
  GOTO lnBomLnRec
ENDIF

SELECT (lnAlias)
RETURN (llRetVal)

*!*************************************************************
*! Name      : lfRetMPo
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To get the receiving session for a specific MPO
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRetMPo()
*!*************************************************************
FUNCTION lfRetMPo
PARAMETERS lcFab, lcClr,lcSession
PRIVATE lnAlias, lcRetVal

lnAlias  = SELECT()
lcRetVal = ''
IF SEEK(lcFab+lcClr,'PofLn')
   SELECT PofLn
   LOCATE REST WHILE fabric+color+cmattype+pomat+trancd= lcFab+lcClr;
               FOR cRSession = lcSession                   
   IF FOUND()
     lcRetVal = PoMat
     SELECT(lnAlias)
     RETURN lcRetVal
   ENDIF            
ENDIF
SELECT(lnAlias)
RETURN lcRetVal

*!*************************************************************
*! Name      : lpSavScr
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : valid function for the Save button
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpSavScr()
*!*************************************************************
FUNCTION lpSavScr

*!*************************************************************
*! Name      : lfCalOnHnd
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To calculate on hand for automatic issue screen.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCalOnHnd()
*!*************************************************************
FUNCTION lfCalOnHnd
PRIVATE lcPo, lcRSession, lcISession, lnBalance

SELECT (lcOpenLots)
INDEX ON Po+cRSession+cISession TAG (lcOpenLots)

LOCATE
DO WHILE !EOF()
  lcPo       = PO
  lcRSession = cRSession
  lcISession = cISession
  lnBalance  = nBalance
  
  SCAN REST WHILE Po+cRSession+cISession =   lcPo + lcRSession
    REPLACE nBalance WITH lnBalance - nToIssue
    lnBalance = lnBalance - nToIssue    
  ENDSCAN

ENDDO

*!*************************************************************
*! Name      : lfChkPrimF
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To check if we are issuing the primary fabric for 
*!             the 1st time and the issued qty is not equal to the required.
*!*************************************************************
*! Example   : = lfChkPrimF()
*!*************************************************************
FUNCTION lfChkPrimF
PARAMETERS lcFab, lcClr, lnToIssue

PRIVATE lnAlias, llToRet, lcOldOrd

lnAlias = SELECT()

llToRet = .T.
SELECT CtktBom
lcOldOrd = SET('ORDER')
SET ORDER TO Ctktyp
*--- cimtyp+cuttkt+item+iclr+mfgcode+dyelot
SET ORDER TO POSLN IN POSLN

IF SEEK("I"+laData[1]+PADR(lcFab,19)+lcClr);
  .AND. SEEK("P"+laData[1],'POSLN');
  .AND. SEEK(POSLN.Style,'Style') ;
  .AND. PADL(Style.Fabric,7) = PADL(lcFab,7)
  IF lnToIssue = 0
    SELECT (lcOpenLots)
    lcOldVal = 0
    REPLACE nToIssue WITH lcOldVal
    llToRet = .F.
    SELECT CtktBom
    SET ORDER TO &lcOldOrd
    SELECT(lnAlias)
    RETURN
  ELSE
    llDistPrim = (&lcTmpQuery..Issue = 0)
  ENDIF
ENDIF
*-- Function to Distri Fabric on Style.
IF llDistPrim 
  =lfDisFOnS(lnToIssue ,cTktBom.UntQty,PADR(lcFab,7),lcClr)
  SELECT (lcOpenLots)
  REPLACE lUpdate  WITH .T.
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfDistLin1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To recalculate the PO line if there is only one line in the PO
*!*************************************************************
*! Example   : = lfDistLin1()
*!*************************************************************
FUNCTION lfDistLin1
PARAMETERS lcFabClr
PRIVATE lnScalCnt, lnPrePack1, lnPrePack2, lnPrePack3, lnPrePack4,;
                   lnPrePack5, lnPrePack6, lnPrePack7, lnPrePack8, lnCount,;
                   lcStyleNo 
STORE 0 TO lnPrePack1, lnPrePack2, lnPrePack3, lnPrePack4,;
           lnPrePack5, lnPrePack6, lnPrePack7, lnPrePack8

*B605207,1 KHM 12/19/2001 (Begin) Initializing llPrePack to check if the
*B605207,1                scale has a pre-pack or no.
llPrePack = .F.
*B605207,1 KHM 12/19/2001 (End)

= SEEK("P"+ladata[1]+lcStyCode1,'POSLN')
= SEEK(POSLN.Style,'Style')
=SEEK("S"+Style.Scale,'SCALE')
lnScalCnt = Scale.Cnt

SET ORDER TO CutPick IN CutPick

IF SEEK('2'+laData[1]+lcStyCode1,'CutPick')
  SELECT CutPick

  *-- To check if there are more than one line in the cutpick
  COUNT REST TO lnCount WHILE TranCd+cTktNo+Style = '2'+laData[1]+lcStyCode1
  =SEEK('2'+laData[1]+lcStyCode1,'CutPick')
  
  IF lnCount = 1
    *-- To get the prepak code.
    =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')
    IF SEEK("P"+Style.Scale+OrdLine.PrePak,'SCALE')
      FOR lnPpCnt = 1 TO lnScalCnt
        lcPpCnt = STR(lnPpCnt,1)
        lnPrePack&lcPpCnt = Scale.Pp&lcPpCnt
      ENDFOR
      
      *B605207,1 KHM 12/19/2001 (Begin) .T. if the scale has a pre-pack.
      llPrePack = .T.
      *B605207,1 KHM 12/19/2001 (End)

    ELSE
      SELECT PosLn
      *-- To get the prepack as an estimation.
      =lfGetPrePk()

      *B605207,1 KHM 12/19/2001 (Begin) .F. if the scale has no pre-pack.
      llPrePack = .F.
      *B605207,1 KHM 12/19/2001 (End)

    ENDIF
  
    = SEEK("S"+Style.Scale,'SCALE')  
    *-- lnFabQty == Fabric Issue Qty
    *-- lnStyReq == Unit required from fabric 

    lnTempTot  = MAX((lnSeeIssu/lnStyReq),1)
    lnTempTot  = FLOOR(lnTempTot)

    FOR lnInde = 1 TO 8
      lcIndex = STR(lnInde,1)
      lcSz&lcIndex = SCALE.SZ&lcIndex
      lnOldQty&lcIndex = 0
      m.Qty&lcIndex= PosLn.Qty&lcIndex
    ENDFOR

    *B605207,1 KHM 12/19/2001 (Begin) Initializing lnTmpDist with the
    *B605207,1                quantity to be re-calculated.
    lnTmpDist = lnTempTot
    *B605207,1 KHM 12/19/2001 (End)
    
    lnTempTot  = lnTempTot - PosLn.TotQty
    lcOpration = IIF(lnTempTot > 0, '+','-')
    lnTempTot  = IIF(lnTempTot > 0, lnTempTot,(lnTempTot * -1))

    *B605207,1 KHM 12/19/2001 (Begin) Check if there is a pre-pack then
    *B605207,1                do not change anything, otherwise change 
    *B605207,1                the way of re-calculation. 
    IF llPrePack
    *B605207,1 KHM 12/19/2001 (End)
      DO WHILE lnTempTot > 0
        FOR lnInde = 1 TO Scale.Cnt
          lcIndex = STR(lnInde,1)
          lcSz&lcIndex = SCALE.SZ&lcIndex
        
          IF PosLn.Qty&lcIndex > 0 AND lnTempTot > 0
          
            *C200220,1 KHM 09/03/2001 (Begin) Checking if the m.qty is 0 then
            *C200220,1                do not reduce the qty in order to 
            *C200220,1                distribute the qty correctly
            IF m.Qty&lcIndex > 0
            *C200220,1 KHM 09/03/2001 (End)
          
              m.Qty&lcIndex = MAX(m.Qty&lcIndex &lcOpration MIN(lnPrePack&lcIndex,lnTempTot),0)
              lnOldQty&lcIndex = m.Qty&lcIndex
              lnTempTot = lnTempTot - lnPrePack&lcIndex
            *C200220,1 KHM 09/03/2001 (Begin) Added.
            ENDIF
            *C200220,1 KHM 09/03/2001 (End)
          ENDIF
        ENDFOR
      ENDDO
    *B605207,1 KHM 12/19/2001 (Bein) The way of no pre-pack
    ELSE
      =lfCalcPoQt(lnTmpDist,PosLn.TotQty,8)
    ENDIF
    *B605207,1 KHM 12/19/2001 (End)        
    m.TotQty = 0
    FOR lnInde = 1 TO Scale.Cnt
      lcIndex = STR(lnInde,1)
      m.TotQty = m.TotQty + m.Qty&lcIndex
      lnTotOld = m.TotQty
    ENDFOR
    lcStyDesc = Style.Desc1
    m.Style = Style.Style
    
    lcEscInt = ON('KEY','ESC')
    lcTabInt = ON('KEY','TAB')
    lcHldBtb = ON('KEY','BACKTAB')    
    lcAltB   = ON('KEY','ALT+B')
    ON KEY LABEL ESC
    ON KEY LABEL TAB
    ON KEY LABEL BACKTAB
    ON KEY LABEL ALT+B
    DO (gcScrDir + gcWinAppl + '\podissty.SPX')
    ON KEY LABEL ESC      &lcEscInt
    ON KEY LABEL TAB      &lcTabInt
    ON KEY LABEL BACKTAB  &lcHldBtb
    ON KEY LABEL ALT+B    &lcAltB
  ELSE
    *-- If there are more than one line in the cut pick
    *C200198,1 KHM 06/25/2001 (Begin) If the PO was generated from a 
    *C200198,1                multi-store or multi-prepack order.
    =lfDisMul1(lnStyReq)
    *C200198,1 KHM 06/25/2001 (End)
    
    RETURN
  ENDIF

ENDIF  
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfDistLin2
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To recalculate the PO line if there are 2 line2 in the PO
*!*************************************************************
*! Example   : = lfDistLin2()
*!*************************************************************
FUNCTION lfDistLin2
PARAMETERS lcStyClr
PRIVATE lnScalCnt, lnPrePack1, lnPrePack2, lnPrePack3, lnPrePack4,;
                   lnPrePack5, lnPrePack6, lnPrePack7, lnPrePack8,;
                   lnOldQty1 , lnOldQty2 , lnOldQty3 , lnOldQty4,;
                   lnOldQty5 , lnOldQty6 , lnOldQty7 , lnOldQty8,;
                   lnOldQty9 , lnOldQty10, lnOldQty11, lnOldQty12,;
                   lnOldQty13, lnOldQty14, lnOldQty15 , lnOldQty16,;
                   lnCount
*B605207,1 KHM 12/19/2001 (Begin) Initializing llPrePack to change the
*B605207,1                way of recalculating the PO qty in case of no pre-pack.
llPrePack = .F.
*B605207,1 KHM 12/19/2001 (End)

lcTmpPoLn = gfTempName()

STORE '' TO lcSz1 , lcSz2 , lcSz3, lcSz4, lcSz5, lcSz6, lcSz7, lcSz8, lcSz9,;
            lcSz10, lcSz11, lcSz12, lcSz13, lcSz14, lcSz15, lcSz16

IF SEEK('2'+laData[1],'CutPick')

  SELECT CutPick
  *-- To check if there are more than one line in the cutpick
  COUNT REST TO lnCount WHILE TranCd+cTktNo+Style = '2'+laData[1] ;
                        FOR SUBSTR(Style,lnColorStr,lnColorLen) = lcStyClr
  =SEEK('2'+laData[1],'CutPick')
  
  IF lnCount = 2

    FOR lnCnt = 1 TO 12
      lcCnt = ALLTRIM(STR(lnCnt))
      lnOldQty&lcCnt = 0
      m.Qty&lcCnt    = 0    
    ENDFOR
    m.TotQty = 0
    
    *-- Function to get the PO lines in a temporary file
    =lfvGetPoLn(lcStyClr)


    *-- lnFabQty == Fabric Issue Qty
    *-- lnStyReq == Unit required from fabric 
    lnTempTot  = MAX((lnSeeIssu/lnStyReq),1)
    lnTempTot  = FLOOR(lnTempTot)

    *B605207,1 KHM 12/19/2001 (Begin) Initializing lnTmpDist with the qty
    *B605207,1                to be re-calculated.  
    lnTmpDist = lnTempTot
    *B605207,1 KHM 12/19/2001 (End)

    SELECT (lcTmpPoLn)
    LOCATE
    SUM TotQty TO lnTotal
    lnTempTot  = lnTempTot - lnTotal
    lcOpration = IIF(lnTempTot > 0, '+','-')
    lnTempTot  = IIF(lnTempTot > 0, lnTempTot,(lnTempTot * -1))

    *B605207,1 KHM 12/19/2001 (Begin) Check if there is a pre-pack or no.
    SELECT (lcTmpPoLn)
    LOCATE
    llPrePack = SEEK('P'+Scale+cPrePack,'Scale')      
    *B605207,1 KHM 12/19/2001 (End)
  
    *B605207,1 KHM 12/19/2001 (Begin) Check if the is a pre-pack then do not
    *B605207,1                change the way of recalculating otherwise do 
    *B605207,1                the new way of calculation.
    IF llPrePack
    *B605207,1 KHM 12/19/2001 (End)
      DO WHILE lnTempTot > 0
        STORE 0 TO lnCount , lnLine

        SELECT (lcTmpPoLn)
        SCAN
          lnLine = lnLine + 1
          IF SEEK('P'+Scale+cPrePack,'Scale')
            FOR lnCntr1 = 1 TO 8
              lcCntr1 = STR(lnCntr1,1)
              *lnPrePck&lcCntr1 = IIF(Scale.Pp&lcCntr1 = 0,1,Scale.Pp&lcCntr1)
              lnPrePack&lcCntr1 = Scale.Pp&lcCntr1            
            ENDFOR
          ELSE
            *C200198,1 KHM 06/25/2001 (Begin) Getting the ratio.
            STORE 0 TO lnPrePack1, lnPrePack2, lnPrePack3, lnPrePack4,;
                       lnPrePack5, lnPrePack6, lnPrePack7, lnPrePack8
           =SEEK(Style,'Style')
           =SEEK('S'+Style.Scale,'Scale')
           lnScalCnt = Scale.Cnt
                     
           =lfGetPrePk()
           *C200198,1 KHM 06/25/2001 (End)
          ENDIF

          FOR lnInde = 1 TO 8
            lcIndex = STR(lnInde,1)
            lnCount  = lnInde + IIF(lnLine = 1,0,8)
            lcCount  = ALLTRIM(STR(lnCount))
            IF Qty&lcIndex > 0 AND lnTempTot > 0
            
              *C200220,1 KHM 09/03/2001 (Begin) Check if m.qty is 0 then do not
              *C200220,1                reduce lnTempTot in order to 
              *C200220,1                recalculate correctly.
              IF m.Qty&lcCount > 0
              *C200220,1 KHM 09/03/2001 (End)
            
                m.Qty&lcCount    = MAX(m.Qty&lcCount &lcOpration MIN(lnPrePack&lcIndex,lnTempTot),0)
                lnOldQty&lcCount = m.Qty&lcCount
                lnTempTot        = lnTempTot - lnPrePack&lcIndex

              *C200220,1 KHM 09/03/2001 (Begin) Added.
              ENDIF
              *C200220,1 KHM 09/03/2001 (End)
            ENDIF  
          ENDFOR
        ENDSCAN
      ENDDO
    *B605207,1 KHM 12/19/2001 (Begin) The new procedure to recalculate the qty
    *B605207,1                in the case of no pre-pack.
    ELSE
      =lfCalcPoQt(lnTmpDist,lnTotal,12)
    ENDIF
    *B605207,1 KHM 12/19/2001 (End)
      
    SELECT (lcTmpPoLn)
    LOCATE
    m.TotQty = 0
    =SEEK(Style,'Style')
    FOR lnCnt = 1 TO 12
      lcCnt    = ALLTRIM(STR(lnCnt))
      m.TotQty = m.TotQty + m.Qty&lcCnt
    ENDFOR
    lnTotOld  = m.TotQty
    lcStyDesc = Style.Desc1
    m.Style   = Style.Style

    lcEscInt = ON('KEY','ESC')
    lcTabInt = ON('KEY','TAB')
    lcHldBtb = ON('KEY','BACKTAB')    
    lcAltB   = ON('KEY','ALT+B')
    ON KEY LABEL ESC
    ON KEY LABEL TAB
    ON KEY LABEL BACKTAB
    ON KEY LABEL ALT+B
    DO (gcScrDir + gcWinAppl + '\podisst2.SPX')
    ON KEY LABEL ESC      &lcEscInt
    ON KEY LABEL TAB      &lcTabInt
    ON KEY LABEL BACKTAB  &lcHldBtb
    ON KEY LABEL ALT+B    &lcAltB

  ELSE
    *-- If there are more than one line in the cut pick
    *C200198,1 KHM 06/25/2001 (Begin) To handle the case of generating PO 
    *C200198,1                from multi-store or multi-prepack order and 
    *C200198,1                having two styles in the SO
    =lfDisMul2(lnStyReq)
    *C200198,1 KHM 06/25/2001 (End)
    RETURN

  ENDIF
ENDIF

SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfvGetPoLn
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To get the PO lines in a temporary file
*!*************************************************************
*! Example   : = lfvGetPoLn()
*!*************************************************************
FUNCTION lfvGetPoLn
PARAMETERS lcSClr
PRIVATE lnAlias, lnLineNo, lnFrom, lnTo

lnAlias  = SELECT()
STORE 0 TO lnFrom, lnTo, lnLineNo

SELECT PosLn
=AFIELDS(laFStru)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+1,4]
laFStru[lnFStru+1,1] = 'cPrePack'
laFStru[lnFStru+1,2] = 'C'
laFStru[lnFStru+1,3] = 1
laFStru[lnFStru+1,4] = 0

=gfCrtTmp(lcTmpPoLn,@laFStru,'cStyType+Po+Style+STR(LineNo,6)+TranCd',lcTmpPoLn)

= SEEK("P"+ladata[1],'POSLN')
SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = "P"+ladata[1] ;
          FOR SUBSTR(Style,lnColorStr,lnColorLen) = lcSClr
  lnLineNo = lnLineNo + 1
  lcLineNo = STR(lnLineNo,1)
  = SEEK(POSLN.Style,'Style')
  SCATTER MEMVAR MEMO
  
  =SEEK("2"+laData[1]+Style,'CutPick')
  =SEEK('O'+CutPick.Order+PADL(CutPick.cOrdLine,6,' '),'OrdLine')
  INSERT INTO (lcTmpPoLn) FROM MEMVAR
  SELECT (lcTmpPoLn)
  REPLACE cPrepack   WITH OrdLine.PrePak
  lnCount = 0
  = SEEK("S"+Style.Scale,'SCALE')
   FOR lnCntr = 1 TO 8
    lnCount   = lnCntr + IIF(lnLineNo = 1,0,8)
    lcCntr    = STR(lnCntr,1)
    lcCount   = ALLTRIM(STR(lnCount))
    lcSz&lcCount   = SCALE.SZ&lcCntr
    STORE PosLn.Qty&lcCntr TO lnOldQty&lcCount,m.Qty&lcCount

  ENDFOR
ENDSCAN
FOR lnCnt = 1 TO 12
  lcCnt = ALLTRIM(STR(lnCnt))
  lnOldQty&lcCnt = 0
  m.Qty&lcCnt    = 0    
ENDFOR
lnLine   = 0
SELECT (lcTmpPoLn)
SCAN
  lnLine = lnLine + 1
  FOR lnInde = 1 TO 8
    lcIndex       = STR(lnInde,1)
    lnCount       = lnInde + IIF(lnLine = 1,0,8)
    lcCount       = ALLTRIM(STR(lnCount))
    STORE Qty&lcIndex TO lnOldQty&lcCount,m.Qty&lcCount
  ENDFOR
ENDSCAN

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfAdjLines
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/25/2001
*! Purpose   : To adjust the qty in the temporary file in order to updating 
*!             the master files
*!*************************************************************
*! Example   : = lfAdjLines()
*!*************************************************************
FUNCTION lfAdjLines
PRIVATE lnLine, llRetVal

lnLine   = 0
SELECT (lcTmpPoLn)
SCAN
  lnLine = lnLine + 1
  FOR lnInde = 1 TO 8
    lcIndex       = STR(lnInde,1)
    lnCount       = lnInde + IIF(lnLine = 1,0,8)
    lcCount       = ALLTRIM(STR(lnCount))
    m.Qty&lcIndex = m.Qty&lcCount
  ENDFOR
  FOR lnCnt = 1 TO 8
    lcCnt   = STR(lnCnt,1)
    REPLACE Qty&lcCnt WITH m.Qty&lcCnt
  ENDFOR
  REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
ENDSCAN

llRetVal = .T.
lnLine   = 0
SELECT (lcTmpPoLn)
SCAN
  lnLine = lnLine + 1
  IF lnLine = 1 
    IF TotQty <= 0
     = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              "Recalculated quantities for style: "+ALLT(Style)+" should be greater "+;
              "than zero. Cannot proceed.")

      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8
      llRetVal = .F.
      EXIT
    ENDIF
  ELSE
    IF TotQty <= 0
     = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              "Recalculated quantities for style: "+ALLT(Style)+" should be greater "+;
              "than zero. Cannot proceed.")
      STORE 0 TO m.Qty9,m.Qty10,m.Qty11,m.Qty12,m.Qty13,m.Qty14,m.Qty15,m.Qty16
      llRetVal = .F.
      EXIT
    ENDIF
  ENDIF  
ENDSCAN
RETURN llRetVal

*!*************************************************************
*! Name      : lfGetPoLn
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : To check how many styles in the PO
*!*************************************************************
*! Example   : = lfGetPoLn()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfGetPoLn
PARAMETERS lcStyClr
PRIVATE lnAlias,lcStyCode, lnPoLns

lnAlias   = SELECT()
lnPoLns   = 0
= SEEK("P"+ladata[1],'POSLN')

SELECT PosLn
*--khalid
*SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+Trancd = "P"+ladata[1] ;
          FOR SUBSTR(Style,lnColorStr,lnColorLen) = lcStyClr
SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+Trancd = "P"+ladata[1] ;
          FOR SUBSTR(Style,lnColorStr,lnColorLen) = lcStyClr AND TranCd = '1'
*--khalid

    lnPoLns = lnPoLns + 1
    DO CASE
      CASE lnPoLns = 1
        lcStyCode1 = Style
      CASE lnPoLns = 2  
        lcStyCode2 = Style
    ENDCASE    
ENDSCAN

SELECT(lnAlias)
RETURN(lnPoLns)

*!*************************************************************
*! Name      : lfGetPrePk
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : To calculate the ratio (prepack) of the quantities if
*!             there was no prepack.
*!*************************************************************
*! Example   : = lfGetPrePk()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfGetPrePk
PRIVATE lnMinQty

FOR lnCntr = 1 TO lnScalCnt
  lcCntr   = STR(lnCntr,1)
  IF Qty&lcCntr > 0
    lnMinQty = Qty&lcCntr
    EXIT
  ENDIF  
ENDFOR

FOR lnCntr = 1 TO lnScalCnt
  lcCntr   = STR(lnCntr,1)
  IF Qty&lcCntr > 0
    lnMinQty = MIN(lnMinQty,Qty&lcCntr)
  ENDIF  
ENDFOR

FOR lnCntr = 1 TO lnScalCnt
  lcCntr   = STR(lnCntr,1)
  IF Qty&lcCntr > 0
    lnPrePack&lcCntr = ROUND(Qty&lcCntr/lnMinQty,0)
  ENDIF  
ENDFOR

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : To get the position and length of none major segment.
*!*************************************************************
*! Example   : = lfGetColor()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfGetColor

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnColorStr = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnColorLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR
*-- end of lfGetColor.


*---------------- Case of there are more than one line for a specific -----*
*----------------- style/Clr in the cutpick file  -------------------------*
*-- If there are more than one line in the cut pick -----------------------*
*!*************************************************************
*! Name      : lfDisMul1
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : To distribute the PO quantities in case of there was one PO
*!             and the PO was generated from multi-store or multi-prepack order
*!*************************************************************
*! Example   : = lfDisMul1()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfDisMul1
PARAMETERS lnUntQty
PRIVATE lnAlias, lcDistSty, lcDisClr, lnDiff, lcDisMul,;
        lcOldTab , lcOldBkTab, lcOldEsc, lcAltB
STORE '' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8

lnAlias    = SELECT()
lcDistSty  = LEFT(PosLn.Style,12)
lcDisClr   = SUBSTR(PosLn.Style,14,6)
*lnDiff     = lnMiReq - lnSeeIssu
lnDiff     = lnSeeIssu - lnMiReq 
lnCosting  = lnUntQty
lcDisMul   = "Distribute Line Quantities"
lcTmpCutPk = gfTempName()
lcTmpIndx  = gfTempName()
*-- To get the sizes description
FOR lnInde = 1 TO 8
  lcIndex = STR(lnInde,1)
  lcSz&lcIndex = SCALE.SZ&lcIndex
ENDFOR

*-- To get the cut pick records
=lfGetCutLn()

lcOldTab   = ON("KEY","TAB")
lcOldBkTab = ON("KEY","BACKTAB")
lcOldEsc   = ON("KEY","ESC")
lcAltB     = ON("KEY","ALT+B")
POP KEY
DO (gcScrDir + gcWinAppl + '\PODISMUL.SPX')

ON KEY LABEL TAB &lcOldTab
ON KEY LABEL BACKTAB &lcOldBkTab
ON KEY LABEL ESCAPE &lcOldEsc
ON KEY LABEL ALT+B &lcAltB
*PUSH KEY
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfDisBrow
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : To browse the lines
*!*************************************************************
*! Example   : = lfDisBrow()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfDisBrow

SELECT (lcTmpCutPk)
GO TOP

lcDistBrow = [lcStore = IIF(cTranType = 'A',cStore,''):9   :H = 'Store'   :R,]+;
             [nRequired :13  :H = 'Required':R,]+;
             [nUse      :13  :H = 'Use' :W=llEdit :V=lfvUseQty(cOrder,cStore,cOrdLine,cTranType),]+; 
             [cType     :9   :H = 'Type'     :R,]             

FOR lnCntSz = 1 TO 8
  DO CASE
    CASE lnCntSz = 1 AND !EMPTY(lcSz1)
      lcDistBrow = lcDistBrow +;
          [nQty1 :7:H = lcSz1 :P= "@Z999999" :W=llEditQty :V= nQty1 >= 0 AND lfvEUseQty(cOrder,cStore,cOrdLine,cTranType),]
    CASE lnCntSz = 2 AND !EMPTY(lcSz2)
      lcDistBrow = lcDistBrow +;
          [nQty2 :7:H = lcSz2 :P= "@Z999999" :W=llEditQty AND !EMPTY(lcSz2) :V= nQty2 >= 0 AND lfvEUseQty(cOrder,cStore,cOrdLine,cTranType),]
    CASE lnCntSz = 3 AND !EMPTY(lcSz3)
      lcDistBrow = lcDistBrow +;
          [nQty3 :7:H = lcSz3 :P= "@Z999999" :W=llEditQty AND !EMPTY(lcSz3) :V= nQty3 >= 0 AND lfvEUseQty(cOrder,cStore,cOrdLine,cTranType),]
    CASE lnCntSz = 4 AND !EMPTY(lcSz4)
      lcDistBrow = lcDistBrow +;
         [nQty4 :7:H = lcSz4 :P= "@Z999999" :W=llEditQty AND !EMPTY(lcSz4) :V= nQty4 >= 0 AND lfvEUseQty(cOrder,cStore,cOrdLine,cTranType),]
    CASE lnCntSz = 5 AND !EMPTY(lcSz5)
      lcDistBrow = lcDistBrow +;             
         [nQty5 :7:H = lcSz5 :P= "@Z999999" :W=llEditQty AND !EMPTY(lcSz5) :V= nQty5 >= 0 AND lfvEUseQty(cOrder,cStore,cOrdLine,cTranType),]
    CASE lnCntSz = 6 AND !EMPTY(lcSz6)
      lcDistBrow = lcDistBrow +;             
         [nQty6 :7:H = lcSz6 :P= "@Z999999" :W=llEditQty AND !EMPTY(lcSz6) :V= nQty6 >= 0 AND lfvEUseQty(cOrder,cStore,cOrdLine,cTranType),]
    CASE lnCntSz = 7 AND !EMPTY(lcSz7)
      lcDistBrow = lcDistBrow +;             
         [nQty7 :7:H = lcSz7 :P= "@Z999999" :W=llEditQty AND !EMPTY(lcSz7) :V= nQty7 >= 0 AND lfvEUseQty(cOrder,cStore,cOrdLine,cTranType),]
    CASE lnCntSz = 8 AND !EMPTY(lcSz8)
      lcDistBrow = lcDistBrow +;             
         [nQty8 :7:H = lcSz8 :P= "@Z999999" :W=llEditQty AND !EMPTY(lcSz8) :V= nQty8 >= 0 AND lfvEUseQty(cOrder,cStore,cOrdLine,cTranType),]
  ENDCASE
ENDFOR  
lcDistBrow = lcDistBrow +;             
             [lnTot = IIF(cTranType <> 'A',nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,'') :10:H = 'TotQty' :R]

BROWSE FIELDS &lcDistBrow;
       WINDOW PODISMU2 IN WINDOW PODISMUL;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       WHEN lfwDisMul('1');
       TITLE ALLTRIM(lcDisMul)

*!*************************************************************
*! Name      : lfwDisMul
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : When function for the browse
*!*************************************************************
*! Example   : = lfwDisMul()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfwDisMul
PARAMETERS lcType
llEdit = .F.
llEditQty = .F.

DO CASE
  CASE &lcTmpCutPk..cTranType = 'A'
    IF lcType = '1'
      SHOW GET pbEditUsag ENABLE
      SHOW GET pbEditQty  DISABLE
    ELSE
      SHOW GET pbEditMUsg ENABLE
      SHOW GET pbEditMQty  DISABLE
    ENDIF  
  CASE &lcTmpCutPk..cTranType = 'B'
    IF lcType = '1'  
      SHOW GET pbEditUsag DISABLE
      SHOW GET pbEditQty  ENABLE
    ELSE
      SHOW GET pbEditMUsg DISABLE
      SHOW GET pbEditMQty ENABLE 
    ENDIF
  OTHERWISE
    IF lcType = '1'  
      SHOW GET pbEditUsag DISABLE
      SHOW GET pbEditQty  DISABLE
    ELSE
      SHOW GET pbEditMUsg DISABLE
      SHOW GET pbEditMQty DISABLE
    ENDIF  
ENDCASE

*!*************************************************************
*! Name      : lfEditUsag
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : To allow the user to edit the use quantity
*!*************************************************************
*! Example   : = lfEditUsag()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfEditUsag
llEdit = .T.
KEYBOARD "{ALT+B}" 

*!*************************************************************
*! Name      : lfvEditMQt
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : To allow the user to edit the quantities
*!*************************************************************
*! Example   : = lfvEditMQt()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfvEditMQt
llEditQty = .T.
KEYBOARD "{ALT+B}" 

*!*************************************************************
*! Name      : lfReadAMul
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : Activate function in the screen
*!*************************************************************
*! Example   : = lfReadAMul()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfReadAMul
IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDisMul)

*!*************************************************************
*! Name      : lfDDisMul
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : DeActivate function in the screen
*!*************************************************************
*! Example   : = lfDDisMul()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfDDisMul
PARAMETERS lcType

ON KEY LABEL CTRL+Q lnDummy = 1
ON KEY LABEL CTRL+W lnDummy = 1
ON KEY LABEL CTRL+HOME GO TOP
ON KEY LABEL CTRL+END  GO BOTTOM
IF WONTOP() = lcDisMul
  IF lcType = '1'
    ON KEY LABEL TAB DO lpTab1  WITH 'PODISMU3','PbEditUsag'
  ELSE
    ON KEY LABEL TAB DO lpTab1  WITH 'PODISMUD','PbEditMUsg'
  ENDIF  
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfGetCutLn
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : To get the allocated lines
*!*************************************************************
*! Example   : = lfGetCutLn()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfGetCutLn
PRIVATE lnAlias

lnAlias = SELECT()
CREATE CURSOR (lcTmpCutPk);
              (cOrder C(6), cOrdLine C(6), cStore C(8), nRequired N(13,2),;
               nUse N(13,2),cType C(5),;
               nQty1 N(6), nQty2 N(6), nQty3 N(6), nQty4 N(6),;
               nQty5 N(6), nQty6 N(6), nQty7 N(6), nQty8 N(6),;
               cTrantype C(1),cTktLn C(6), cStyle C(19),cPrePack C(1))
INDEX ON cTranType TAG (lcTmpIndx) OF (lcTmpCutPk)
INDEX ON cOrder+cStore+cOrdLine+cTranType TAG (lcTmpCutPk) OF (lcTmpCutPk)

SELECT CutPick
=SEEK('2'+laData[1]+POsLn.Style)
SCAN REST WHILE TranCd+cTktNo+Style = '2'+laData[1]+POsLn.Style
  =SEEK('O'+CutPick.Order+PADL(CutPick.cOrdLine,6,' '),'OrdLine')
  =SEEK(CutPick.Style,'Style')  
  *-- Record for the store, required and use
  INSERT INTO (lcTmpCutPk) ;
              (cOrder,cOrdLine,cStore,nRequired,nUse,cTranType,cTktLn,cStyle) VALUES;
               (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,CutPick.TotQty*lnCosting,;
               CutPick.TotQty*lnCosting,'A',CutPick.cTktLineNo,CutPick.Style)


  *-- Record for the quantities
  INSERT INTO (lcTmpCutPk) ;
              (cOrder,cOrdLine,cStore,cTranType,cType,cTktLn,cStyle) VALUES;
              (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,'B','Qty',;
               CutPick.cTktLineNo,CutPick.Style)
  SELECT (lcTmpCutPk)
  FOR lnCntr = 1 TO 8
    lcCntr   = STR(lnCntr,1)
    REPLACE nQty&lcCntr WITH CutPick.Qty&lcCntr
  ENDFOR
  lnTempTot  = CutPick.TotQty
  STORE 0 TO lnUseQty1,lnUseQty2,lnUseQty3,lnUseQty4,lnUseQty5,lnUseQty6,lnUseQty7,lnUseQty8
  =lfCalcPoQt(lnTempTot,CutPick.TotQty,8,.T.)
  SELECT (lcTmpCutPk)
  FOR lnCntr = 1 TO 8
    lcCntr   = STR(lnCntr,1)
    REPLACE nQty&lcCntr WITH lnUseQty&lcCntr
  ENDFOR
  
  *-- Record for the prepack
  INSERT INTO (lcTmpCutPk) ;
              (cOrder,cOrdLine,cStore,cTranType,cType,cTktLn,cStyle) VALUES;
              (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,'C','Ratio',;
               CutPick.cTktLineNo,CutPick.Style)

  IF SEEK("P"+Style.Scale+OrdLine.PrePak,'SCALE')
    SELECT (lcTmpCutPk)
    FOR lnPpCnt = 1 TO lnScalCnt
      lcPpCnt = STR(lnPpCnt,1)
      REPLACE nQty&lcPpCnt WITH Scale.Pp&lcPpCnt
    ENDFOR
    REPLACE cPrePack WITH OrdLine.PrePak
  ELSE
    *C200294,1 KHM 02/19/2002 (Begin) Commented out.
    *STORE 0 TO lnPrePack1, lnPrePack2, lnPrePack3, lnPrePack4,;
    *           lnPrePack5, lnPrePack6, lnPrePack7, lnPrePack8
    *=SEEK(CutPick.Style,'Style')
    *=SEEK('S'+Style.Scale,'Scale')
    *lnScalCnt = Scale.Cnt                     
    *SELECT CutPick
    *=lfGetPrePk()
    *SELECT (lcTmpCutPk)
    *FOR lnPpCnt = 1 TO lnScalCnt
    *  lcPpCnt = STR(lnPpCnt,1)
    *  REPLACE nQty&lcPpCnt WITH lnPrePack&lcPpCnt
    *ENDFOR
    *C200294,1 KHM 02/19/2002 (End)
  ENDIF    
ENDSCAN

*!*************************************************************
*! Name      : lfvUseQty
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : To validate the use quantity for one PO line
*!*************************************************************
*! Example   : = lfvUseQty()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfvUseQty
PARAMETERS lcOrder,lcStore,lcOrdLine,lcType
PRIVATE lnUseQty,lnUsQty1,lnUsQty2,lnUsQty3,lnUsQty4,lnUsQty5,lnUsQty6,;
        lnUsQty7,lnUsQty8, lnPp1,lnPp2,lnPp3,lnPp4,lnPp5,lnPp6,lnPp7,lnPp8

*C200294,1 KHM 02/19/2002 (Begin) Initialize this variable to check whether
*C200294,1                a style has a prepack or not, in order to have 
*C200294,1                different routines.
llPrePack = .F.
*C200294,1 KHM 02/19/2002 (End)


lnUseQty = 0
SELECT (lcTmpCutPk)
SEEK lcOrder+lcStore+lcOrdLine+'B'
FOR lnCounter = 1 TO 8
  lcCounter = STR(lnCounter,1)
  lnUseQty&lcCounter = 0
  lnUseQty = lnUseQty + nQty&lcCounter
ENDFOR

SEEK lcOrder+lcStore+lcOrdLine+'A'

*C200220,1 KHM 09/03/2001 (Begin) Allow the user to edit zero in the usage
*IF nUse <= 0
*   =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
*             "Use quantity should be greater than 0. Cannot proceed.")
*  REPLACE nUse WITH (lnUseQty * lnCosting)
*  RETURN
*ENDIF
IF nUse = 0
  FOR lnCounter = 1 TO 8
    lcCounter = STR(lnCounter,1)
    lnUseQty&lcCounter = 0
  ENDFOR
  lnUseQty = 0
ENDIF
*C200220,1 KHM 09/03/2001 (End)

SEEK lcOrder+lcStore+lcOrdLine+'C'

*C200294,1 KHM 02/19/2002 (Begin) Check if there is a prepack or not.
llPrePack = !EMPTY(cPrePack)
*C200294,1 KHM 02/19/2002 (End)

FOR lnCounter = 1 TO 8
  lcCounter = STR(lnCounter,1)
  lnPp&lcCounter = nQty&lcCounter
ENDFOR

SEEK lcOrder+lcStore+lcOrdLine+'A'

*C200220,1 KHM 09/03/2001 (Begin) Adding the case of usage = 0
*lnTempTot  = MAX(nUse/lnCosting,1)
lnTempTot  = IIF(nUse = 0,0,MAX(nUse/lnCosting,1))
*C200220,1 KHM 09/03/2001 (End)

lcOpration = '+'
lnTempTot  = FLOOR(lnTempTot)

SEEK lcOrder+lcStore+lcOrdLine+'B'
=SEEK(cStyle,'Style')
=SEEK('S'+Style.Scale,'Scale')

*C200294,1 KHM 02/19/2002 (Begin) Adding the IF condition.
IF llPrePack
*C200294,1 KHM 02/19/2002 (End)
  DO WHILE lnTempTot > 0
    FOR lnInde = 1 TO Scale.Cnt
      lcIndex = STR(lnInde,1)
    
      *C200220,1 KHM 09/03/2001 (Begin) If the usage = 0 then do not check qty>0    
      *IF nQty&lcIndex > 0 AND lnTempTot > 0
      IF IIF(lnUseQty > 0,nQty&lcIndex > 0,.T.) AND lnTempTot > 0
      *C200220,1 KHM 09/03/2001 (End)
    
        lnUseQty&lcIndex = MAX(lnUseQty&lcIndex &lcOpration MIN(lnPp&lcIndex,lnTempTot),0)
        lnTempTot = lnTempTot - lnPp&lcIndex
      ENDIF
    ENDFOR
  ENDDO
*C200294,1 KHM 02/19/2002 (Begin) New routine to hand the case of no prepack
ELSE
  =lfCalcPoQt(lnTempTot,lnUseQty,8,.T.)
ENDIF
*C200294,1 KHM 02/19/2002 (End)

FOR lnInde = 1 TO 8
 lcIndex = STR(lnInde,1)
 REPLACE nQty&lcIndex WITH lnUseQty&lcIndex   
ENDFOR

SUM nUse TO lnCalDiff FOR cTranType = 'A'
lnDiff   = lnSeeIssu - lnCalDiff
SHOW GET lnDiff

SEEK lcOrder+lcStore+lcOrdLine+'A'

SHOW WINDOW (lcDisMul) REFRESH


*!*************************************************************
*! Name      : lfvEUseQty
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 06/25/2001
*! Purpose   : To validate the quantities for one PO line
*!*************************************************************
*! Example   : = lfvEUseQty()
*!*************************************************************
*C200198,1 KHM 06/25/2001 
*!*************************************************************
FUNCTION lfvEUseQty
PARAMETERS lcOrder, lcStore, lcOrdLine, lcTranType
PRIVATE lnTotQty

lnTotQty = 0
FOR lnCntr = 1 TO 8
  lcCntr = STR(lnCntr,1)
  lnTotQty = lnTotQty + nQty&lcCntr
ENDFOR

SEEK lcOrder+lcStore+lcOrdLine+'A'
SELECT (lcTmpCutPk)
REPLACE nUse WITH lnTotQty * lnCosting

SUM nUse TO lnCalDiff FOR cTranType = 'A'
lnDiff   = lnSeeIssu - lnCalDiff
SHOW GET lnDiff

SEEK lcOrder+lcStore+lcOrdLine+'B'
SHOW WINDOW (lcDisMul) REFRESH

*!*************************************************************
*! Name      : lfvMApply
*! Developer : Khalid Mohi El-Din
*! Date      : 06/25/2001
*! Purpose   : To Validate the apply button and make the necessary updates
*!*************************************************************
*! Example   : = lfvMApply()
*!*************************************************************
FUNCTION lfvMApply
PARAMETERS lcType

lnDiffernc = IIF(lnDiff < 0 , lnDiff * -1, lnDiff)
IF (lnDiff < 0 ) OR (lnDiff >= lnCosting)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              "The distributed quantities do not match the issued quantities. Cannot proceed.")
  RETURN
ENDIF

IF lcType = '1'
  STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
  SELECT (lcTmpCutPk)
  SET ORDER TO (lcTmpIndx)
  SEEK "B"
  SUM REST nQty1,nQty2,nQty3,nQty4,nQty5,nQty6,nQty7,nQty8 TO;      
      m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8;
      WHILE cTranType = "B"

  m.TotQty =  m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  CLEAR READ
  llStrt = .T.
  *-- Update the recalculated qty.
  =lfUpdPOLin('B')

  *C200220,1 KHM 09/03/2001 (Begin) allow the user to edit the unit quantity
  *C200220,1                and do the necessary updates.
  =lfvUntQty('B')
  *C200220,1 KHM 09/03/2001 (End)
  
  *-- To refresh the outer browse
  =lfUpdTemp()

ELSE

  IF lfAdjMLine()
    SELECT (lcTmpPoLn)
    SCAN
      SCATTER MEMVAR
      =SEEK('P'+laData[1]+Style+STR(LineNo,6)+'1','PosLn')
      *-- Update the recalculated qty.    
      =lfUpdPOLin('C')
    ENDSCAN
  
    *C200220,1 KHM 09/03/2001 (Begin) allow the user to edit the unit quantity
    *C200220,1                and do the necessary updates.
    =lfvUntQty('B')
    *C200220,1 KHM 09/03/2001 (End)

    *-- To refresh the outer browse
    =lfUpdTemp()
    CLEAR READ
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfUpdCtPk
*! Developer : Khalid Mohi El-Din
*! Date      : 06/25/2001
*! Purpose   : To Update the allocated qty according to the distributed one
*!             in the CutPick, OrdHdr, OrdLine and PosLn
*!*************************************************************
*! Example   : = lfUpdCtPk()
*!*************************************************************
FUNCTION lfUpdCtPk
PARAMETERS lcUType
PRIVATE lnAlias

lnAlias = SELECT()

SELECT CutPick
SET ORDER TO CutPkOrd

SELECT (lcTmpCutPk)
SET ORDER TO (lcTmpIndx)
SEEK "B"

IF lcUType = '1'
  
  SCAN REST WHILE cTranType = 'B'
    IF SEEK('2'+laData[1]+cTktLn+cOrder+cStyle+cOrdLine,'CutPick')
  
      *-- Updating the CutPick file.
      SELECT CutPick
      FOR lnCntr = 1 TO 8
        lcCntr = STR(lnCntr,1)
        REPLACE Qty&lcCntr WITH &lcTmpCutPk..nQty&lcCntr
      ENDFOR
      REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8   
     
      *C200220,1 KHM 09/03/2001 (Begin) Check if the totqty is 0 then delete
      IF TotQty = 0
        DELETE
      ENDIF 
      *C200220,1 KHM 09/03/2001 (End)
      
      *-- Updating the OrdLine file.
      IF SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')
        SELECT OrdLine
        FOR lnCntr = 1 TO 8
          lcCntr = STR(lnCntr,1)
          REPLACE Cut&lcCntr WITH &lcTmpCutPk..nQty&lcCntr
        ENDFOR
        REPLACE TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
      ENDIF
    ENDIF  
  ENDSCAN
  
  *-- Updating the order header file
  IF SEEK('O'+CUTPICK.Order,'ORDHDR')
    SELECT OrdHdr
    REPLACE TotCut WITH (TotCut - PosLn.TotQty) + m.TotQty
    =gfTraceKey('ORDHDR','O'+CUTPICK.Order,'M')
  ENDIF

  *-- Updating the po line file
  SELECT PosLn
  FOR lnCntr = 1 TO 8
    lcCntr = STR(lnCntr,1)
    REPLACE Ord&lcCntr WITH m.Qty&lcCntr
  ENDFOR
  REPLACE TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

  SELECT OrdLine
  =gfTraceKey('ORDLINE','O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'M')

ELSE

  SCAN REST WHILE cTranType = 'B'
    FOR lnOutLoop = 1 TO 2
      SELECT (lcTmpCutPk)
      lcCounter = STR(lnOutLoop,1)
      IF SEEK('2'+laData[1]+cTktLn&lcCounter+cOrder+cStyle&lcCounter+;
               cOrdLine&lcCounter,'CutPick')
  
        *-- Updating the CutPick file.
        SELECT CutPick
        FOR lnCntr = 1 TO 8
          lcCntr     = STR(lnCntr,1)
          lnCutPkCnt = lnCntr + IIF(lnOutLoop = 1,0,8)
          lcCutPkCnt = ALLTRIM(STR(lnCutPkCnt))
          REPLACE Qty&lcCntr WITH &lcTmpCutPk..nQty&lcCutPkCnt
        ENDFOR
        REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8   
        
        *C200220,1 KHM 09/03/2001 (Begin) Check if the TotQty = 0 then delete it.
        IF TotQty = 0
          DELETE
        ENDIF
        *C200220,1 KHM 09/03/2001 (End) 
            
        *-- Updating the OrdLine file.
        IF SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')
          SELECT OrdLine
          FOR lnCntr = 1 TO 8
            lcCntr = STR(lnCntr,1)
            lnCutPkCnt = lnCntr + IIF(lnOutLoop = 1,0,8)
            lcCutPkCnt = ALLTRIM(STR(lnCutPkCnt))
            REPLACE Cut&lcCntr WITH &lcTmpCutPk..nQty&lcCntr
          ENDFOR
          REPLACE TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
        ENDIF
      ENDIF  
    ENDFOR  
  ENDSCAN
  
  *-- Updating the order header file
  IF SEEK('O'+CUTPICK.Order,'ORDHDR')
    SELECT OrdHdr
    REPLACE TotCut WITH (TotCut - PosLn.TotQty) + m.TotQty
    =gfTraceKey('ORDHDR','O'+CUTPICK.Order,'M')
  ENDIF

  *-- Updating the po line file
  SELECT PosLn
  FOR lnCntr = 1 TO 8
    lcCntr = STR(lnCntr,1)
    REPLACE Ord&lcCntr WITH m.Qty&lcCntr
  ENDFOR
  REPLACE TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

  SELECT OrdLine
  =gfTraceKey('ORDLINE','O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'M')
ENDIF
SELECT(lnAlias )


*---------------- Case of multi-store or multi-prepack and there are two styles
*----------------- in the PO -------------------------*
*!*************************************************************
*! Name      : lfDisMul2
*! Developer : Khalid Mohi El-Din
*! Date      : 06/25/2001
*! Purpose   : To recalculate the quantities for multi-store, multi-prepack
*!             and having two styles in the PO
*!*************************************************************
*! Example   : = lfDisMul2()
*!*************************************************************
FUNCTION lfDisMul2
PARAMETERS lnUntQty
PRIVATE lnAlias, lcDistSty, lcDisClr, lnDiff, lcDisMul,;
        lcOldTab , lcOldBkTab, lcOldEsc, lcAltB

STORE '' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8,;
            lcSz9,lcSz10,lcSz11,lcSz12,lcSz13,lcSz14,lcSz15,lcSz16

lnAlias    = SELECT()
=SEEK('P'+laData[1]+lcStyCode1,'PosLn')
lcDistSty  = LEFT(lcStyCode1,12)
lcDisClr   = SUBSTR(PosLn.Style,14,6)
*lnDiff     = lnMiReq - lnSeeIssu
lnDiff     = lnSeeIssu - lnMiReq 
lnCosting  = lnUntQty
lcDisMul   = "Distribute Line Quantities"
lcTmpCutPk = gfTempName()
lcTmpIndx  = gfTempName()

*-- To get the sizes description
FOR lnOuter = 1 TO 2
  lcOuter = STR(lnOuter,1)
  =SEEK(lcStyCode&lcOuter,'Style')
  =SEEK('S'+Style.Scale,'Scale')
  
  FOR lnInde = 1 TO 8
    lcIndex = STR(lnInde,1)
    lnCount  = lnInde + IIF(lnOuter = 1,0,8)
    lcCount  = ALLTRIM(STR(lnCount))
    lcSz&lcCount = Scale.Sz&lcIndex
  ENDFOR  
ENDFOR

*-- To get the cut pick records
=lfGetCtLn2()

lcOldTab   = ON("KEY","TAB")
lcOldBkTab = ON("KEY","BACKTAB")
lcOldEsc   = ON("KEY","ESC")
lcAltB     = ON("KEY","ALT+B")
POP KEY
DO (gcScrDir + gcWinAppl + '\PODISMUA.SPX')

ON KEY LABEL TAB &lcOldTab
ON KEY LABEL BACKTAB &lcOldBkTab
ON KEY LABEL ESCAPE &lcOldEsc
ON KEY LABEL ALT+B &lcAltB
*PUSH KEY
SELECT(lnAlias)


*!*************************************************************
*! Name      : lfGetCtLn2
*! Developer : Khalid Mohi El-Din
*! Date      : 06/25/2001
*! Purpose   : To Collect the allocated lines for each PO line from CutPick
*!*************************************************************
*! Example   : = lfGetCtLn2()
*!*************************************************************
FUNCTION lfGetCtLn2
PRIVATE lnAlias

lnAlias = SELECT()
CREATE CURSOR (lcTmpCutPk);
              (cOrder C(6), cOrdLine1 C(6), cStore1 C(8), cStyle1 C(19),;
               cOrdLine2 C(6), cStore2 C(8),cStyle2 C(19),nRequired N(13,2),;
               nUse N(13,2),cType C(5), cPrepack1 C(1),cPrepack2 C(1),;
               nQty1 N(6), nQty2 N(6), nQty3 N(6), nQty4 N(6),;
               nQty5 N(6), nQty6 N(6), nQty7 N(6), nQty8 N(6),;
               nQty9 N(6), nQty10 N(6), nQty11 N(6), nQty12 N(6),;
               nQty13 N(6), nQty14 N(6), nQty15 N(6), nQty16 N(6),;
               cTrantype C(1),cTktLn1 C(6), cTktLn2 C(6))
INDEX ON cTranType TAG (lcTmpIndx) OF (lcTmpCutPk)
INDEX ON cOrder+cStore1+cPrePack1+cTranType TAG (lcTmpCutPk) OF (lcTmpCutPk)
*B608049,1 TMI [Start] add a new index with the CORDLINE1 field
INDEX ON CORDER+CSTORE1+CORDLINE1+CPREPACK1+CTRANTYPE TAG CORDLINE1 OF (LCTMPCUTPK)
*B608049,1 TMI [End  ] 

STORE SPACE(0) TO lcCutSty , lcCutStore , lcCutPpack

STORE 0 TO lnDummPPK

SELECT CutPick
=SEEK('2'+laData[1])
SCAN REST WHILE TranCd+cTktNo+Style = '2'+laData[1] ;
          FOR SUBSTR(Style,lnColorStr,lnColorLen) = lcDisClr

  =SEEK('O'+CutPick.Order+PADL(CutPick.cOrdLine,6,' '),'OrdLine')
  =SEEK(CutPick.Style,'Style')  
  =SEEK('S'+Style.Scale,'Scale')
  lnScalCnt = Scale.Cnt

  SELECT (lcTmpCutPk)
  *--mhm to handle style entered many times and assign dummy PrePack
  *IF !SEEK(CutPick.Order+OrdLine.Store+OrdLine.PrePak) 
  *B608049,1 TMI [Start] seek taking into considration CORDLINE1 field
  *IF !SEEK(CutPick.Order+OrdLine.Store+OrdLine.PrePak)  OR (EMPTY(OrdLine.Store) OR EMPTY(OrdLine.PrePak))
  IF !SEEK(CutPick.Order+OrdLine.Store+CUTPICK.CORDLINE+OrdLine.PrePak)  OR (EMPTY(OrdLine.Store) OR EMPTY(OrdLine.PrePak))
    *- initiate the variable M.CPREPACK1 with the first found prepack for the current scale
    =SEEK('P'+ORDLINE.SCALE,'SCALE')
    M.CPREPACK1 = SCALE.PREPAK
    *B608049,1 TMI [End  ]     
    
    IF EMPTY(OrdLine.PrePak)
      *B608049,1 TMI [Start] no need for this part
      *lnDummPPK =lnDummPPK +1
      *m.cPrepack1 =STR(lnDummPPK ,1)
      *B608049,1 TMI [End  ] 
    ELSE
      m.cPrepack1 = ORDLINE.PREPAK
    ENDIF

    *-- Record for the useage and required quantites
    *--mhm
    *INSERT INTO (lcTmpCutPk) ;
           (cOrder,cOrdLine1,cStore1,nRequired,nUse,cTranType,cTktLn1,;
           cStyle1,cPrepack1);
           VALUES;
           (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,CutPick.TotQty*lnCosting,;
           CutPick.TotQty*lnCosting,'A',CutPick.cTktLineNo,CutPick.Style,;
           ORDLINE.PREPAK)
    *-- Record for the quantities
    *INSERT INTO (lcTmpCutPk) ;
           (cOrder,cOrdLine1,cStore1,cTranType,cType,cTktLn1,cStyle1,;
            cPrepack1);
            VALUES;
           (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,'B','Qty',;
            CutPick.cTktLineNo,CutPick.Style,ORDLINE.PREPAK)
           
    INSERT INTO (lcTmpCutPk) ;
           (cOrder,cOrdLine1,cStore1,nRequired,nUse,cTranType,cTktLn1,;
           cStyle1,cPrepack1);
           VALUES;
           (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,CutPick.TotQty*lnCosting,;
           CutPick.TotQty*lnCosting,'A',CutPick.cTktLineNo,CutPick.Style,;
           m.cPrepack1)


    *-- Record for the quantities
    INSERT INTO (lcTmpCutPk) ;
           (cOrder,cOrdLine1,cStore1,cTranType,cType,cTktLn1,cStyle1,;
            cPrepack1);
            VALUES;
           (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,'B','Qty',;
            CutPick.cTktLineNo,CutPick.Style,m.cPrepack1)
    SELECT (lcTmpCutPk)
    FOR lnCntr = 1 TO 8
      lcCntr   = STR(lnCntr,1)
      REPLACE nQty&lcCntr WITH CutPick.Qty&lcCntr
    ENDFOR  
  
    *-- Record for the prepack
    *--mhm2004
    *INSERT INTO (lcTmpCutPk) ;
           (cOrder,cOrdLine1,cStore1,cTranType,cType,cTktLn1,cStyle1,;
            cPrePack1);
            VALUES;
           (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,'C','Ratio',;
            CutPick.cTktLineNo,CutPick.Style,ORDLINE.PREPAK)

    INSERT INTO (lcTmpCutPk) ;
           (cOrder,cOrdLine1,cStore1,cTranType,cType,cTktLn1,cStyle1,;
            cPrePack1);
            VALUES;
           (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,'C','Ratio',;
            CutPick.cTktLineNo,CutPick.Style,m.cPrepack1)

    *B608049,1 TMI [Start] locate the first prepack if no one recorded in ordline.prepak field or locate the correct prepack
    *IF SEEK("P"+Style.Scale+OrdLine.PrePak,'SCALE')
    IF SEEK("P"+Style.Scale+M.CPREPACK1,'SCALE')
      *B608049,1 TMI [End  ] 
      SELECT (lcTmpCutPk)
      FOR lnPpCnt = 1 TO lnScalCnt
        lcPpCnt = STR(lnPpCnt,1)
        REPLACE nQty&lcPpCnt WITH Scale.Pp&lcPpCnt
      ENDFOR
    
    *C200220,1 KHM 09/03/2001 (Begin) Recalculate the prepack qty.
    ELSE  
     *C200294,1 KHM 02/19/2002 (Begin) Commented out.
     *FOR lnCntr = 1 TO lnScalCnt
     * lcCntr = STR(lnCntr,1)
     * Qty&lcCntr = CutPick.Qty&lcCntr
     * lnPrePack&lcCntr = 0
     *ENDFOR  
     *=lfGetPrePk()

     * FOR lnCounter = 1 TO lnScalCnt
     *   lcCounter = STR(lnCounter,1)
     *   REPLACE nQty&lcCounter WITH lnPrePack&lcCounter
     * ENDFOR
     *C200294,1 KHM 02/19/2002 (End)
     *C200220,1 KHM 09/03/2001 (End)
     *B608049,1 TMI [Start] set the ratio to 1 if no prepacks found
     FOR LNPPCNT = 1 TO LNSCALCNT
        LCPPCNT = STR(LNPPCNT,1)
        REPLACE nQty&lcPpCnt WITH 1
     ENDFOR
     *B608049,1 TMI [End  ] 

    ENDIF
  ELSE
    =SEEK('S' + ORDLINE.SCALE , 'SCALE')
    SELECT (lcTmpCutPk)
    *B608049,1 TMI [Start] set the order to the new tag CORDLINE1
    SET ORDER TO (LCTMPCUTPK) IN (LCTMPCUTPK)
    *B608049,1 TMI [End  ] 

    *-- Record for the useage and required quantites
    SEEK CutPick.Order+OrdLine.Store+OrdLine.PrePak+'A'
    REPLACE nRequired WITH nRequired + (CutPick.TotQty*lnCosting),;
            nUse      WITH nUse      + (CutPick.TotQty*lnCosting)

    *-- Record rest of the sizes
    SEEK CutPick.Order+OrdLine.Store+OrdLine.PrePak+'B'
    FOR lnInde = 1 TO 8
      lcIndex = STR(lnInde,1)
      lnCount  = lnInde + 8
      lcCount  = ALLTRIM(STR(lnCount))
      REPLACE nQty&lcCount WITH CutPick.Qty&lcIndex
    ENDFOR
    REPLACE cOrdLine2 WITH CutPick.cOrdLine  ,;
            cStore2   WITH OrdLine.Store     ,;
            cTktLn2   WITH CutPick.cTktLineNo,;
            cStyle2   WITH CutPick.Style     ,;
            cPrePack2 WITH OrdLine.PrePak

    *-- Record for the ratio
    SEEK CutPick.Order+OrdLine.Store+OrdLine.PrePak+'C'
    IF SEEK("P"+Style.Scale+OrdLine.PrePak,'SCALE')
      SELECT (lcTmpCutPk)
      FOR lnPpCnt = 1 TO lnScalCnt
        lcPpCnt = STR(lnPpCnt,1)
        lnCount  = lnPpCnt + 8
        lcCount  = ALLTRIM(STR(lnCount))
        REPLACE nQty&lcCount WITH Scale.Pp&lcPpCnt
      ENDFOR
    *C200220,1 KHM 09/03/2001 (Begin) Recalculate prepack qty.
    ELSE  
     *C200294,1 KHM 02/19/2002 (Begin) Commented out
     *FOR lnCntr = 1 TO lnScalCnt
     * lcCntr = STR(lnCntr,1)
     * Qty&lcCntr = CutPick.Qty&lcCntr
     * lnPrePack&lcCntr = 0
     *ENDFOR  
     *=lfGetPrePk()
     * FOR lnCounter = 1 TO lnScalCnt
     *   lcCounter = STR(lnCounter,1)
     *   lnCount  = lnCounter + 8
     *   lcCount  = ALLTRIM(STR(lnCount))
     *   REPLACE nQty&lcCount WITH lnPrePack&lcCounter
     * ENDFOR
     *C200294,1 KHM 02/19/2002 (End)
      *C200220,1 KHM 09/03/2001 (End)
    ENDIF  
  ENDIF  
ENDSCAN

*!*************************************************************
*! Name      : lfDisBrow2
*! Developer : Khalid Mohi El-Din
*! Date      : 06/25/2001
*! Purpose   : To browse the allocated records.
*!*************************************************************
*! Example   : = lfDisBrow2()
*!*************************************************************
FUNCTION lfDisBrow2

SELECT (lcTmpCutPk)
GO TOP

*B608049,1 TMI [Start] Add a new parameter to the lfvMUseQty that passes the CORDLINE1 to locate the correct order line
*lcDistBrow = [lcStore = IIF(cTranType = 'A',ALLTRIM(cStore1),''):12 :H = 'Store'   :R,]+;
             [nRequired :13  :H = 'Required':R,]+;
             [nUse      :13  :H = 'Use' :W=llEdit :V=lfvMUseQty(cOrder,cStore1,cPrePack1,cTranType),]+; 
             [cType     :9   :H = 'Type'     :R,]             
lcDistBrow = [lcStore = IIF(cTranType = 'A',ALLTRIM(cStore1),''):12 :H = 'Store'   :R,]+;
             [nRequired :13  :H = 'Required':R,]+;
             [nUse      :13  :H = 'Use' :W=llEdit :V=lfvMUseQty(cOrder,cStore1,cPrePack1,cTranType,CORDLINE1),]+; 
             [cType     :9   :H = 'Type'     :R,]             
*B608049,1 TMI [End  ] 
FOR lnCntSz = 1 TO 12
  DO CASE
    CASE lnCntSz = 1 AND !EMPTY(lcSz1)
      lcDistBrow = lcDistBrow +;
         [nQty1 :7 :H=lcSz1 :W=llEditQty :V= nQty1 >= 0 AND lfvEdtMQty() :P= "@Z999999",]
    CASE lnCntSz = 2 AND !EMPTY(lcSz2)
      lcDistBrow = lcDistBrow +;
         [nQty2 :7 :H=lcSz2 :W=llEditQty :V= nQty2 >= 0 AND lfvEdtMQty() :P= "@Z999999",]
    CASE lnCntSz = 3 AND !EMPTY(lcSz3)
      lcDistBrow = lcDistBrow +;
         [nQty3 :7 :H=lcSz3 :W=llEditQty :V= nQty3 >= 0 AND lfvEdtMQty() :P= "@Z999999",]
    CASE lnCntSz = 4 AND !EMPTY(lcSz4)
      lcDistBrow = lcDistBrow +;
         [nQty4 :7 :H=lcSz4 :W=llEditQty :V= nQty4 >= 0 AND lfvEdtMQty() :P= "@Z999999",]
    CASE lnCntSz = 5 AND !EMPTY(lcSz5)
      lcDistBrow = lcDistBrow +;             
         [nQty5 :7 :H=lcSz5 :W=llEditQty :V=lfvEdtMQty() :P= "@Z999999",]
    CASE lnCntSz = 6 AND !EMPTY(lcSz6)
      lcDistBrow = lcDistBrow +;             
         [nQty6 :7 :H=lcSz6 :W=llEditQty :V=lfvEdtMQty() :P= "@Z999999",]
    CASE lnCntSz = 7 AND !EMPTY(lcSz7)
      lcDistBrow = lcDistBrow +;             
         [nQty7 :7 :H=lcSz7 :W=llEditQty :V=lfvEdtMQty() :P= "@Z999999",]    
    CASE lnCntSz = 8 AND !EMPTY(lcSz8)
      lcDistBrow = lcDistBrow +;             
         [nQty8 :7 :H=lcSz8 :W=llEditQty :V=lfvEdtMQty() :P= "@Z999999",]
    CASE lnCntSz = 9 AND !EMPTY(lcSz9)
      lcDistBrow = lcDistBrow +;
         [nQty9 :7 :H=lcSz9 :W=llEditQty AND !EMPTY(lcSz9) :V=lfvEdtMQty() :P= "@Z999999",]    
    CASE lnCntSz = 10 AND !EMPTY(lcSz10)
      lcDistBrow = lcDistBrow +;
         [nQty10:7 :H=lcSz10:W=llEditQty AND !EMPTY(lcSz10) :V=lfvEdtMQty() :P= "@Z999999",]    
    CASE lnCntSz = 11 AND !EMPTY(lcSz11)
      lcDistBrow = lcDistBrow +;
         [nQty11:7 :H=lcSz11:W=llEditQty AND !EMPTY(lcSz11) :V=lfvEdtMQty() :P= "@Z999999",]    
    CASE lnCntSz = 12 AND !EMPTY(lcSz12)
      lcDistBrow = lcDistBrow +;
         [nQty12:7 :H=lcSz12:W=llEditQty AND !EMPTY(lcSz12) :V=lfvEdtMQty() :P= "@Z999999",]    
  ENDCASE
ENDFOR  
lcDistBrow = lcDistBrow +;
             [lnTot = IIF(cTranType <> 'A',nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8+nQty9+nQty10+nQty11+nQty12,'') :14:H = 'TotQty' :R]

BROWSE FIELDS &lcDistBrow;
       WINDOW PODISMUC IN WINDOW PODISMUA;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       WHEN lfwDisMul('2');
       TITLE ALLTRIM(lcDisMul)


*!*************************************************************
*! Name      : lfEditMUsg
*! Developer : Khalid Mohi El-Din
*! Date      : 06/25/2001
*! Purpose   : To validate the Edit Usage Button
*!*************************************************************
*! Example   : = lfEditMUsg()
*!*************************************************************
FUNCTION lfEditMUsg
llEdit = .T.
KEYBOARD "{ALT+B}" 

*!*************************************************************
*! Name      : lfvMUseQty
*! Developer : Khalid Mohi El-Din
*! Date      : 06/25/2001
*! Purpose   : Valid function for the Edit Usage button.
*!*************************************************************
*! Example   : = lfvMUseQty()
*!*************************************************************
FUNCTION lfvMUseQty

PARAMETERS lcOrder,lcStore,lcPrPk,lcType,lcOrdLine1

PRIVATE lnUseQty,lnUsQty1,lnUsQty2,lnUsQty3,lnUsQty4,lnUsQty5,lnUsQty6,;
        lnUsQty7,lnUsQty8,;
        lnUsQty9,lnUsQty10,lnUsQty11,lnUsQty12,lnUsQty13,lnUsQty14,;
        lnUsQty15,lnUsQty16,;
        lnPp1,lnPp2,lnPp3,lnPp4,lnPp5,lnPp6,lnPp7,lnPp8,;
        lnPp9,lnPp10,lnPp11,lnPp12,lnPp13,lnPp14,lnPp15,lnPp16
        
*B605207,1 KHM 12/19/2001 (Begin) Initializing llPrePack to check if the
*B605207,1                scale has a pre-pack or no.
llPrePack = .F.
*B605207,1 KHM 12/19/2001 (End)

lnUseQty = 0
SELECT (lcTmpCutPk)
*B608049,1 TMI [Start] set the order to CORDLINE1
PRIVATE lcSvOrd
lcSvOrd = ORDER(lcTmpCutPk)
SET ORDER TO CORDLINE1
*B608049,1 TMI [End  ] 

*-- To get the quantities
*B608049,1 TMI [Start] seek with the lcOrdLine1 value
*SEEK lcOrder+lcStore+lcPrPk+'B'
SEEK lcOrder+lcStore+lcOrdLine1+lcPrPk+'B'
*B608049,1 TMI [End  ] 
FOR lnCounter = 1 TO 12
  lcCounter = ALLTRIM(STR(lnCounter,2))
  lnUseQty&lcCounter = nQty&lcCounter
  lnUseQty = lnUseQty + lnUseQty&lcCounter
ENDFOR

*-- To get the prepack for the first line.
*B608049,1 TMI [Start] seek with the lcOrdLine1 value
*SEEK lcOrder+lcStore+lcPrPk+'C'
SEEK lcOrder+lcStore+lcOrdLine1+lcPrPk+'C'
*B608049,1 TMI [End  ] 
=SEEK(lcStyCode1,'Style')
=SEEK('S'+Style.Scale,'Scale')
lnScalCnt = Scale.Cnt

*C200220,1 KHM 09/03/2001 (Begin) The following lines are commented out.
*IF !EMPTY(cPrepack1)
*  FOR lnCounter = 1 TO 8
*    lcCounter = STR(lnCounter,1)
*    lnPp&lcCounter = nQty&lcCounter
*  ENDFOR
*ELSE
*  STORE 0 TO lnPrePack1, lnPrePack2, lnPrePack3, lnPrePack4,;
*             lnPrePack5, lnPrePack6, lnPrePack7, lnPrePack8,;
*             Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
*  SEEK lcOrder+lcStore+lcPrPk+'B'
*  FOR lnCntr = 1 TO lnScalCnt
*    lcCntr = STR(lnCntr,1)
*    Qty&lcCntr = nQty&lcCntr
*  ENDFOR  
*  =lfGetPrePk()
*  FOR lnCounter = 1 TO 8
*    lcCounter = STR(lnCounter,1)
*    lnPp&lcCounter = lnPrePack&lcCounter
*  ENDFOR 
*ENDIF  
FOR lnCounter = 1 TO 8
  lcCounter = STR(lnCounter,1)
  lnPp&lcCounter = nQty&lcCounter
ENDFOR
*C200220,1 KHM 09/03/2001 (End)

*B605207,1 KHM 12/19/2001 (Begin) Check if there is a prepack or not.
llPrePack = !EMPTY(cPrePack1)
*B605207,1 KHM 12/19/2001 (End)

*-- To get the prepack for the second line.
*B608049,1 TMI [Start] seek with the lcOrdLine1 value
*SEEK lcOrder+lcStore+lcPrPk+'C'
SEEK lcOrder+lcStore+lcOrdLine1+lcPrPk+'C'
*B608049,1 TMI [End  ] 
=SEEK(lcStyCode2,'Style')
=SEEK('S'+Style.Scale,'Scale')
lnScalCnt = Scale.Cnt

*C200220,1 KHM 09/03/2001 (Begin) The following lines are commented out.
*IF !EMPTY(cPrepack2)
*  FOR lnCounter = 1 TO 8
*    lcCounter = STR(lnCounter,1)
*    lnCountrX = lnCounter + 8
*    lcCountrX = ALLTRIM(STR(lnCountrX))
*    lnPp&lcCountrX = nQty&lcCounter
*  ENDFOR
*ELSE
*  STORE 0 TO lnPrePack1, lnPrePack2, lnPrePack3, lnPrePack4,;
*             lnPrePack5, lnPrePack6, lnPrePack7, lnPrePack8,;
*             Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
*  SEEK lcOrder+lcStore+lcPrPk+'B'
*  FOR lnCntr = 1 TO lnScalCnt
*    lcCntr = STR(lnCntr,1)
*    lnCountr = lnCntr + 8
*    lnCountr = ALLTRIM(STR(lnCountr))
*    Qty&lcCntr = nQty&lnCountr
*  ENDFOR  
*  =lfGetPrePk()
*  FOR lnCounter = 1 TO 8
*    lcCounter = STR(lnCounter,1)
*    lnCountr  = lnCounter + 8
*    lnCountr  = ALLTRIM(STR(lnCountr))
*    lnPp&lnCountr = lnPrePack&lcCounter
*  ENDFOR  
*ENDIF  
FOR lnCounter = 1 TO 8
  lcCounter = STR(lnCounter,1)
  lnCountrX = lnCounter + 8
  lcCountrX = ALLTRIM(STR(lnCountrX))
  lnPp&lcCountrX = nQty&lcCounter
ENDFOR
*C200220,1 KHM 09/03/2001 (End)

*B608049,1 TMI [Start] seek with the lcOrdLine1 value
*SEEK lcOrder+lcStore+lcPrPk+'A'
SEEK lcOrder+lcStore+lcOrdLine1+lcPrPk+'A'
*B608049,1 TMI [End  ] 

*C200220,1 KHM 09/03/2001 (Begin) Adding the case of allowing zero usage.
IF nUse = 0
  FOR lnCounter = 1 TO 12
    lcCounter = ALLTRIM(STR(lnCounter,2))
    lnUseQty&lcCounter = 0
  ENDFOR
  lnUseQty = 0
ENDIF  
*C200220,1 KHM 09/03/2001 (End)

*C200220,1 KHM 09/03/2001 (Begin) Taking care of zero usage case.
*lnTempTot  = MAX(nUse/lnCosting,1)
lnTempTot  = IIF(nUse = 0, 0 , MAX(nUse/lnCosting,1))
*C200220,1 KHM 09/03/2001 (End)

*B605207,1 KHM 12/19/2001 (Begin) Added
lnTmpDist  = lnTempTot 
*B605207,1 KHM 12/19/2001 (End)

lcOpration = '+'
lnTempTot  = FLOOR(lnTempTot)

lnTempTot  = lnTempTot - lnUseQty 
lcOpration = IIF(lnTempTot > 0, '+','-')
lnTempTot  = IIF(lnTempTot > 0, lnTempTot,(lnTempTot * -1))

*B608049,1 TMI [Start] seek with the lcOrdLine1 value
*SEEK lcOrder+lcStore+lcPrPk+'B'
SEEK lcOrder+lcStore+lcOrdLine1+lcPrPk+'B'
*B608049,1 TMI [End  ] 

*B605207,1 KHM 12/19/2001 (Begin) Adding the IF command.
IF llPrePack
*B605207,1 KHM 12/19/2001 (End)
  DO WHILE lnTempTot > 0
    *C200220,1 KHM 09/03/2001 (Begin)
    *FOR lnInde = 1 TO 16
    FOR lnInde = 1 TO 12
    *C200220,1 KHM 09/03/2001 (End)
  
      lcIndex = ALLTRIM(STR(lnInde,2))

      *C200220,1 KHM 09/03/2001 (Begin) Added
      *IF nQty&lcIndex > 0 AND lnTempTot > 0
      IF IIF(lnUseQty > 0, nQty&lcIndex > 0, .T.) AND lnTempTot > 0
      *C200220,1 KHM 09/03/2001 (End)
    
        lnUseQty&lcIndex = MAX(lnUseQty&lcIndex &lcOpration MIN(lnPp&lcIndex,lnTempTot),0)
        lnTempTot = lnTempTot - lnPp&lcIndex
      ENDIF
    ENDFOR
  ENDDO
*B605207,1 KHM 12/19/2001 (Begin) A new routine for the case of no prepack.
ELSE
  =lfCalcPoQt(lnTmpDist,lnUseQty,12,.T.)
ENDIF
*B605207,1 KHM 12/19/2001 (End)
FOR lnInde = 1 TO 12
 lcIndex = ALLTRIM(STR(lnInde,2))
 REPLACE nQty&lcIndex WITH lnUseQty&lcIndex   
ENDFOR

SUM nUse TO lnCalDiff FOR cTranType = 'A'
lnDiff   = lnSeeIssu - lnCalDiff
SHOW GET lnDiff

*B608049,1 TMI [Start] seek with the lcOrdLine1 value
*SEEK lcOrder+lcStore+lcPrPk+'A'
SEEK lcOrder+lcStore+lcOrdLine1+lcPrPk+'A'

SET ORDER TO &lcSvOrd IN &lcTmpCutPk
*B608049,1 TMI [End  ] 

SHOW WINDOW (lcDisMul) REFRESH

*-- end of lfvMUseQty.

*!*************************************************************
*! Name      : lfvEdtMuQt
*! Developer : Khalid Mohi El-Din
*! Date      : 06/25/2001
*! Purpose   : Valid function for the Edit Quantities button
*!*************************************************************
*! Example   : = lfvEdtMuQt()
*!*************************************************************
FUNCTION lfvEdtMuQt
llEditQty = .T.
KEYBOARD "{ALT+B}" 

*!*************************************************************
*! Name      : lfvEdtMQty
*! Developer : Khalid Mohi El-Din
*! Date      : 06/25/2001
*! Purpose   : Valid function for editing the quantities.
*!*************************************************************
*! Example   : = lfvEdtMQty()
*!*************************************************************
FUNCTION lfvEdtMQty
PRIVATE lnTotQty,lcOrder,lcStore,lcPrPk,lcType,lnRecNo

lcOrder = cOrder
lcStore = cStore1
lcPrPk  = cPrePack1
lnTotQty = 0
FOR lnCntr = 1 TO 12
  lcCntr = ALLTRIM(STR(lnCntr,2))
  lnTotQty = lnTotQty + nQty&lcCntr
ENDFOR
SEEK lcOrder+lcStore+lcPrPk+'A'
SELECT (lcTmpCutPk)
REPLACE nUse WITH lnTotQty * lnCosting

SUM nUse TO lnCalDiff FOR cTranType = 'A'
lnDiff   = lnSeeIssu - lnCalDiff
SHOW GET lnDiff

SEEK lcOrder+lcStore+lcPrPk+'B'
SHOW WINDOW (lcDisMul) REFRESH

*!*************************************************************
*! Name      : lfAdjMLine
*! Developer : Khalid Mohi El-Din
*! Date      : 06/25/2001
*! Purpose   : To adjust the lines when having two styles in the PO
*!*************************************************************
*! Example   : = lfAdjMLine()
*!*************************************************************
FUNCTION lfAdjMLine
PRIVATE lnAlias, llRetVal, lnLine

lnAlias = SELECT()

SELECT PosLn
=AFIELDS(laFStru)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+1,4]
laFStru[lnFStru+1,1] = 'cPrePack'
laFStru[lnFStru+1,2] = 'C'
laFStru[lnFStru+1,3] = 1
laFStru[lnFStru+1,4] = 0

=gfCrtTmp(lcTmpPoLn,@laFStru,'cStyType+Po+Style+STR(LineNo,6)+TranCd',lcTmpPoLn)

SELECT (lcTmpCutPk)
SET ORDER TO (lcTmpIndx)
SEEK "B"
SCAN REST WHILE cTranType = "B"
  FOR lnOutCnt = 1 TO 2
    lcCnt = STR(lnOutCnt,1)
    SELECT (lcTmpPoLn)
    IF !SEEK('P'+laData[1]+lcStyCode&lcCnt)
      SELECT PosLn
      =SEEK('P'+laData[1]+lcStyCode&lcCnt)
      SCATTER MEMVAR MEMO
      SELECT (lcTmpPoLn)
      INSERT INTO (lcTmpPoLn) FROM MEMVAR
      FOR lnCounter = 1 TO 8
        lcCounter = STR(lnCounter,1)
        REPLACE Qty&lcCounter WITH 0  
      ENDFOR
      REPLACE TotQty WITH 0
    ENDIF
    
    FOR lnCntr = 1 TO 8
      lcCntr    = STR(lnCntr,1)
      lnCount   = lnCntr + IIF(lnOutCnt = 1,0,8)
      lcCount   = ALLTRIM(STR(lnCount))
      REPLACE Qty&lcCntr WITH Qty&lcCntr + &lcTmpCutPk..nQty&lcCount,;
              TotQty     WITH TotQty + Qty&lcCntr
    ENDFOR
    REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    
  ENDFOR  
ENDSCAN

llRetVal = .T.
lnLine   = 0
SELECT (lcTmpPoLn)
SCAN
  lnLine = lnLine + 1
  IF lnLine = 1 
    IF TotQty <= 0
     = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              "Recalculated quantities for the first style should be greater "+;
              "than zero. Cannot proceed.")

      llRetVal = .F.
      EXIT
    ENDIF
  ELSE
    IF TotQty <= 0
     = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
              "Recalculated quantities for the second style should be greater "+;
              "than zero. Cannot proceed.")
      llRetVal = .F.
      EXIT
    ENDIF
  ENDIF  
ENDSCAN
SELECT(lnAlias)
RETURN llRetVal


*!*************************************************************
*! Name      : lfReCalQty
*! Developer : Khalid Mohi El-Din
*! Date      : 09/03/2001
*! Purpose   : Valid function of the recalculation button. The purpose
*!             of this function is to check if it is a primary fabric then 
*!             it will check if the issued qty is less or more than the 
*!             required qty then a message will be displayed informing the 
*!             user that the issued qty is less or more than the required 
*!             qty. Do you want to recalculate the qty. If yes then the
*!             recalculation screen will be displayed. 
*!             IF THE ISSUED QTY = REQUIRED QTY THEN THE MESSAGE WILL NOT BE 
*!             DISPLAYED AND WE WILL DISPLAY THE RECALCULATION SCREEN ONLY.
*!*************************************************************
*! Example   : = lfReCalQty()
*!*************************************************************
*C200220,1 KHM 09/03/2001 
*!*************************************************************
*C037959,1 MHM 09/20/2004 comment this function to rewrite according to new concept

*FUNCTION lfReCalQty
FUNCTION lfRClQty111
PRIVATE llDistPrim, lnAlias

llDistPrim = .F.
lnAlias    = SELECT()

lnMiReq    = &lcTmpQuery..REQ
lnSeeIssu  = &lcTmpQuery..Used
lcMiItm    = &lcTmpQuery..Item
lcMiClr    = &lcTmpQuery..Color

*-- Function to Distri Fabric on Style.
IF lfChkPrim()
  llStrt = .F.
  =lfDisFOnS(lnSeeIssu,&lcTmpQuery..Unit,&lcTmpQuery..Item,&lcTmpQuery..Color)
ENDIF  

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvUntQty
*! Developer : Khalid Mohi El-Din
*! Date      : 09/03/2001
*! Purpose   : To allow the user to edit the unit quantity and do 
*!             the necessary updates.
*!*************************************************************
*! Example   : = lfvUntQty()
*!*************************************************************
*C200220,1 KHM 09/03/2001 
*!*************************************************************
FUNCTION lfvUntQty
PARAMETERS lcType

PRIVATE lnAlias, lnOldEstCs, lnNewEstCs, lnUntQty, lnReqQty, lnUntCost
SELECT &lcTmpQuery

STORE 0 TO lnOldEstCs, lnNewEstCs, lnUntQty, lnReqQty, lnUntCost
*IF lnOldUntQt = Unit
IF lcType = 'A' AND lnOldUntQt = Unit

  RETURN
ENDIF 
SCATTER MEMVAR

*-- Changing the tag of the posln file
SET ORDER TO TAG PosLn IN PosLn


*C037959,1 MHM 09/20/2004 add check for unit qty[Start]
IF lcType = 'A' 
  IF SEEK('I1'+laData[1],'BomLine')
    SELECT BomLine
    LOCATE REST WHILE ;
    cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode =;
    'I'+'1'+laData[1] ;
    FOR cbomtyp+item+iclr+mfgcode = ;
    m.cBomType+PADR(&lcTmpQuery..Item,19)+&lcTmpQuery..Color+SPACE(6)
    IF FOUND()
      SCAN REST WHILE ;
        cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode =;
        'I'+'1'+laData[1] ;
        FOR cbomtyp+item+iclr+mfgcode = ;
        m.cBomType+PADR(&lcTmpQuery..Item,19)+&lcTmpQuery..Color+SPACE(6)
        *-- Get the old estimated cost
        lnReqQty   = lnReqQty   + (StyQty*&lcTmpQuery..Unit) 
      ENDSCAN
    ENDIF  
  ENDIF

  IF lnReqQty > &lcTmpQuery..issue
    lcMsgAns = gfModalGen('INM00000B00006',.F.,.F.,.F.,;
                "You have Issued " + ALLT(STR(ABS(lnReqQty - &lcTmpQuery..issue))) +" "+&lcTmpQuery..Uom+" more than required. Do you wish to return this quantity?")
                
    IF lcMsgAns = 1
      *--first choose warehouse
      =lfvWarHus()
      DO (gcScrDir + gcWinAppl + '\powarhos.SPX')      
      =lfRtrnQty(lnReqQty - &lcTmpQuery..issue)
    ENDIF            
  ELSE
    IF lnReqQty < &lcTmpQuery..issue
      lcMsgAns = gfModalGen('INM00000B00006',.F.,.F.,.F.,;
                 "You have Issued "+ALLT(STR(ABS(&lcTmpQuery..issue - lnReqQty)))+" "+&lcTmpQuery..Uom+" less than required. Do you wish to issue this quantity?")
      IF lcMsgAns = 1
        =lfvMIss1("I")
      ENDIF           
    ENDIF             
  ENDIF

ENDIF
lnReqQty = 0
*C037959,1 MHM 09/20/2004 [End]

*-- Updating the BomLine file, getting the old estimated cost and calculating
*-- the required qty and new estimated cost.
IF SEEK('I1'+laData[1],'BomLine')
  SELECT BomLine
  LOCATE REST WHILE ;
  cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode =;
  'I'+'1'+laData[1] ;
  FOR cbomtyp+item+iclr+mfgcode = ;
  m.cBomType+PADR(&lcTmpQuery..Item,19)+&lcTmpQuery..Color+SPACE(6)
  IF FOUND()
    SCAN REST WHILE ;
      cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode =;
      'I'+'1'+laData[1] ;
      FOR cbomtyp+item+iclr+mfgcode = ;
      m.cBomType+PADR(&lcTmpQuery..Item,19)+&lcTmpQuery..Color+SPACE(6)
  
      *-- Get the old estimated cost
      lnOldEstCs = lnOldEstCs + (UnitCost*StyQty*UnitQty)
      REPLACE UnitQty  WITH &lcTmpQuery..Unit ,;
              ItemQty  WITH StyQty*UnitQty ,;
              ItemAmt  WITH StyQty*UnitQty*UnitCost ;

      lnNewEstCs = lnNewEstCs + (UnitCost*ItemQty)
      lnReqQty   = lnReqQty   + (StyQty*UnitQty) 
    ENDSCAN

    *-- Updating the cTktBom file
    lnUntCost = lnNewEstCs/lnReqQty
    IF SEEK('I'+laData[1]+m.cBomType+PADR(&lcTmpQuery..Item,19)+&lcTmpQuery..Color+SPACE(6),'cTktBom')
      SELECT cTktBom
      REPLACE UntCost  WITH lnUntCost         ,;
              UntQty   WITH &lcTmpQuery..Unit ,;
              Req_Qty  WITH lnReqQty          ,;
              Est_Cost WITH lnNewEstCs
    ENDIF
    *-- Updating the estimated cost in the PosHdr file
    IF SEEK('P'+laData[1],'PosHdr')
      SELECT PosHdr
      REPLACE ('nICost'+m.cBomType) WITH EVAL('nICost'+m.cBomType) - lnOldEstCs + lnNewEstCs,;
              ('NfCost'+m.cBomType) WITH EVAL('NfCost'+m.cBomType) - lnOldEstCs + lnNewEstCs
    ENDIF
  ENDIF
  *-- To update estimated cost in the posln file
  =lfUpdEstCst()
ENDIF
SELECT (lcTmpQuery)
lnOldUntQt = Unit
REPLACE Req WITH lnReqQty


*!*************************************************************
*! Name      : lfUpdEstCst
*! Developer : Khalid Mohi El-Din
*! Date      : 09/03/2001
*! Purpose   : To update the estimated costs in the POSLN file
*!*************************************************************
*! Example   : = lfUpdEstCst()
*!*************************************************************
*! C200220,1 KHM 09/03/2001 
*!*************************************************************
FUNCTION lfUpdEstCst
PRIVATE lnAlias
lnAlias = SELECT(0)

=SEEK('P'+laData[1],'PosHdr')
STORE '/' TO lcPExSign,lcDExSign,lcPUntSin,lcDUntSin
lcPExSign = gfGetExSin(@lcPUntSin,POSHDR.cPriceCur)
lcDExSign = gfGetExSin(@lcDUntSin,POSHDR.cDutyCur)

IF SEEK('I'+'1'+laData[1],'BOMLINE')

  IF SEEK('P'+laData[1],'PosLn')
    SELECT PosLn
    REPLACE REST nCost1  WITH 0,nCost2  WITH 0,nCost3  WITH 0,nCost4  WITH 0,;
                 nCost5  WITH 0,nECost1 WITH 0,nECost2 WITH 0,nECost3 WITH 0,;
                 nECost4 WITH 0,nECost5 WITH 0;
     WHILE cStytype+po+style+STR(lineno,6)+trancd+STR(RECNO(),7) = 'P'+laData[1]
  ENDIF  

  SELECT BOMLINE
  lcBomLnKey = EVALUATE(Key())
  DO WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode=;
           'I'+'1'+laData[1] FOR !lVoid
    SCATTER MEMVAR
    lnECost    = 0
    lnTransQty  = 0
    lnBomLLine = LineNo
    lcBomLType = cBomTyp
    lcBomStyle = Style
    lcBomSClr  = SClr
    lcBomItem  = Item
    lcBomIClr  = IClr
    lcBomMCode = MfgCode
    lcBomCatgT = cCatgTyp
    lcBomLnKey = EVALUATE(Key())

    SELECT PosLn
    =SEEK('P'+m.CTktNo+m.Style+STR(m.LineNo,6))
    SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = 'P'+m.CTktNo+m.Style+STR(m.LineNo,6)
      lnEstCost  = 0
      SELECT BOMLINE
      SEEK lcBomLnKey
      SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+;
                      Item+IClr+MfgCode=;
                      'I'+'1'+laData[1]+STR(lnBomLLine,6)+lcBomLType+;
                      lcBomStyle+lcBomSClr
         lcBomSizes = IIF(EMPTY(cSizes),'12345678',cSizes)
       
         lnTransQty  = 0
         FOR lnCntr = 1 TO 8
           lcCntr   = STR(lnCntr,1)
           IF lcCntr $ lcBomSizes
             lnTransQty = lnTransQty + PosLn.Qty&lcCntr
           ENDIF
         ENDFOR       
         lnEstCost  = lnEstCost + ((UnitQty * UnitCost) * lnTransQty)
      ENDSCAN

      lnEstCost = lnEstCost / PosLn.TotQty
      DO CASE
        CASE lcBomCatgT = 'P'
          lnECost = (lnEstCost &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit)
        CASE !INLIST(lcBomCatgT ,'S','F','T')
          lnECost = (lnEstCost &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
        OTHERWISE
          lnECost  = lnEstCost
      ENDCASE   
      SELECT PosLn
       REPLACE ('nCost'+lcBomLType) WITH lnEstCost ,;
               ('nECost'+m.cBomTyp) WITH lnECost
    ENDSCAN
    SELECT BomLine
  ENDDO
  SEEK lcBomLnKey
ENDIF
SELECT(lnAlias)
*--end of lfUpdEstCst.

*!*************************************************************
*! Name      : lfvStyClr
*! Developer : Khalid Mohi El-Din
*! Date      : 09/03/2001
*! Purpose   : To check the fabric in the style cost sheet.
*!*************************************************************
*! Example   : = lfvStyClr()
*!*************************************************************
*! *B605037,1 KHM 10/21/2001 
*!*************************************************************
FUNCTION lfvStyClr
PARAMETERS lcItm, lcIClr
PRIVATE lnAlias, lcBomOrd
lnAlias = SELECT()
lcRetClr = ''

*C037959,1 MHM 09/20/2004 comment the following lines and make check from OrdBom[Start]
*lcBomOrd = ORDER('BOM')
*SET ORDER TO BOMITEM IN BOM
*IF SEEK("P"+ladata[1],'PosLn')
*  *-- typ+item+iclr+citmmajor+citmmask
*  IF SEEK('2'+PADR(lcItm,19),'Bom')
*    SELECT Bom
*    LOCATE REST WHILE Typ+Item+IClr+cItmMajor+cItmMask = '2'+PADR(lcItm,19);
*                FOR cItmMajor = SUBSTR(PosLn.Style,1,12)
*    IF FOUND()
*      DO CASE
*        CASE SUBSTR(Bom.cItmMask,14,6) = "******" AND ICLR = "******"
*          lcRetClr = lcIClr
*        CASE SUBSTR(Bom.cItmMask,14,6) = "******" AND ICLR <> "******"   
*          lcRetClr = lcIClr
*        CASE SUBSTR(Bom.cItmMask,14,6) <> "******" AND ICLR <> "******" 
*          =SEEK('2'+PADR(lcItm,19)+lcIClr)
*          LOCATE REST WHILE Typ+Item+IClr+cItmMajor+cItmMask = ;
*                            '2'+PADR(lcItm,19)+lcIClr;
*                FOR cItmMajor = SUBSTR(PosLn.Style,1,12)
*          IF FOUND()
*            lcRetClr  = SUBSTR(Bom.cItmMask,14,6)
*          ENDIF
*      ENDCASE    
*      SELECT Bom
*    LOCATE REST WHILE Typ+Item+IClr+cItmMajor+cItmMask = '2'+PADR(lcItm,19);
*                FOR cItmMajor = SUBSTR(PosLn.Style,1,12)
*        
*      lcRetClr = IIF(SUBSTR(Bom.cItmMask,14,6) = "******" ,lcIClr,SUBSTR(Bom.cItmMask,14,6))
*    ENDIF  
*  ENDIF
*ENDIF
*SELECT BOM
*SET ORDER TO lcBomOrd 
SELECT cutpick
lcOrder = IIF(SEEK("2"+laData[1]),Order,SPACE(06))

lcBomOrd = ORDER('ORDBOM')
SET ORDER TO ORDBOM IN ORDBOM
IF SEEK("P"+ladata[1],'PosLn')
  IF SEEK(lcOrder+"     0"+SUBSTR(PosLn.Style,1,12),'ORDBOM')
    SELECT ORDBOM
    LOCATE REST WHILE order+STR(lineno,6)+style+sclr+item+iclr+mfgcode= lcOrder+'0'+SUBSTR(PosLn.Style,1,12);
                FOR ALLTRIM(Item)= ALLTRIM(lcItm) AND OrdBom.cItmMask =PosLn.Style
  
    lcRetClr = IIF(SUBSTR(OrdBom.cItmMask,14,6) = "******" ,lcIClr,SUBSTR(OrdBom.cItmMask,14,6))
  ENDIF
  
ENDIF
*C037959,1 MHM [End]

SELECT(lnAlias)
RETURN lcRetClr

*!*************************************************************
*! Name      : lfCalcPoQt
*! Developer : Khalid Mohi El-Din
*! Date      : 12/19/2001
*! Purpose   : To re-calculate the PO qty in the case of no pre-pack.
*!*************************************************************
*! Example   : = lfCalcPoQt()
*!*************************************************************
*!*************************************************************
* B605207,1 KHM 12/19/2001 
*!*************************************************************
FUNCTION lfCalcPoQt
*B605207,1 KHM 12/19/2001 (Begin) Adding a new parameter llStatus to 
*B605207,1 KHM 12/19/2001 distinguish between one and mutli lines cases.
*PARAMETERS lnDistQty, lnOrigQty, lnIterate
PARAMETERS lnDistQty, lnOrigQty, lnIterate, llStatus
*B605207,1 KHM 12/19/2001 (End)

PRIVATE lnPercnt1,lnPercnt2,lnPercnt3,lnPercnt4,lnPercnt5,lnPercnt6,;
        lnPercnt7,lnPercnt8,lcQtyCnt,lnDistVal,;
        lnOrgQty1,lnOrgQty2,lnOrgQty3,lnOrgQty4,lnOrgQty5,lnOrgQty6,lnOrgQty7,;
        lnOrgQty8,lnOrgQty9,lnOrgQty10,lnOrgQty11,lnOrgQty12
        
STORE 0 TO lnPercnt1,lnPercnt2,lnPercnt3,lnPercnt4,lnPercnt5,lnPercnt6,;
           lnPercnt7,lnPercnt8,;
           lnOrgQty1,lnOrgQty2,lnOrgQty3,lnOrgQty4,lnOrgQty5,lnOrgQty6,;
           lnOrgQty7,lnOrgQty8,lnOrgQty9,lnOrgQty10,lnOrgQty11,lnOrgQty12
           

lnDistVal = lnDistQty
DO WHILE lnDistVal > 0
  FOR lnQtyCnt = 1 TO lnIterate
    lcQtyCnt = ALLTRIM(STR(lnQtyCnt))
    *B605207,1 KHM 12/19/2001 (Begin) Add the case of multi-prepacks
    *IF m.Qty&lcQtyCnt > 0
    IF IIF(llStatus,nQty&lcQtyCnt,m.Qty&lcQtyCnt) > 0
    *B605207,1 KHM 12/19/2001 (End)
      IF lnDistVal <= 0
        EXIT
      ENDIF
      *B605207,1 KHM 12/19/2001 (Begin) Add the case of multi-prepack
      *lnPercnt&lcQtyCnt = (m.Qty&lcQtyCnt/lnOrigQty)*100
      lnPercnt&lcQtyCnt = IIF(llStatus,(nQty&lcQtyCnt/lnOrigQty),;
                          (m.Qty&lcQtyCnt/lnOrigQty))*100
      *B605207,1 KHM 12/19/2001 (End)
      lnOrgQty&lcQtyCnt  = lnOrgQty&lcQtyCnt + MIN(ROUND((lnDistQty*lnPercnt&lcQtyCnt)/100,0),lnDistVal)
      lnDistVal = lnDistVal - lnOrgQty&lcQtyCnt
    ENDIF
  ENDFOR
ENDDO

*B605207,1 KHM 12/19/2001 (Begin) Add the case of multi-prepack
IF llStatus
  FOR lnCntr = 1 TO lnIterate
    lcCntr = ALLTRIM(STR(lnCntr))
    lnUseQty&lcCntr = lnOrgQty&lcCntr
  ENDFOR
ELSE
*B605207,1 KHM 12/19/2001 (End)
  FOR lnCntr = 1 TO lnIterate
    lcCntr = ALLTRIM(STR(lnCntr))
    m.Qty&lcCntr    = lnOrgQty&lcCntr
    lnOldQty&lcCntr = m.Qty&lcCntr  
  ENDFOR
ENDIF

*!*************************************************************
*! Name      : lfModBom
*! Developer : Khalid Mohi El-Din
*! Date      : 11/27/2002
*! Purpose   : To simulate the BOM from Bomline file.
*!*************************************************************
*! Example   : = lfModBom()
*!*************************************************************
*! B606633,1 KHM 11/27/2002
*!*************************************************************
FUNCTION lfModBom

PRIVATE lcTmpBomLn, lcMjrMsk, lcSizes, lnI
lcMjrMsk  = gfItemMask("PM")
lcTmpBomLn = gfTempName()
SELECT BOM
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpBomLn) FROM ARRAY laFileStru
INDEX ON citmmajor+typ+citmmask+mfgcode+item+iclr TAG (lcTmpBomLn)
SELECT BomLine
SEEK "I1"+laData[1]+STR(lnPoLNo,6)
SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+SClr+Item+IClr+MfgCode =;
                "I1"+laData[1]+STR(lnPoLNo,6) ;
            FOR Style = lcStyle
  llTrim_Inv = .F.
  lcRecDesc  = ""
  lcUom      = ''
  lcMarker   = ''
  llBaseOnSz = .F.
  IF SEEK("I"+laData[1]+cBomtyp+Item+IClr+MfgCode+dyelot,'CTktBom')
    llTrim_Inv = cTktBom.Trim_Invt
    lcRecDesc  = cTktBom.Desc
    lcUom      = cTktBom.Uom
    lcMarker   = cTktBom.cMarker
  ENDIF
  IF SEEK(SUBSTR(lcStyle,1,LEN(lcMjrMsk)),'Bom')
    SELECT Bom
    LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=SUBSTR(lcStyle,1,LEN(lcMjrMsk));
                FOR lBasOnSiz
    llBaseOnSz = FOUND()
  ENDIF
  lcSizes = ''
  IF LEN(BOMLINE.CSIZES) > 1
    FOR lnI = 1 TO LEN(BOMLINE.CSIZES)-1
      lcSizes = lcSizes + SUBSTR(BOMLINE.CSIZES,lnI,1) + ','
    ENDFOR
    lcSizes = lcSizes + SUBSTR(BOMLINE.CSIZES,LEN(BOMLINE.CSIZES),1)
  ELSE
    lcSizes = BOMLINE.CSIZES
  ENDIF
  IF !EMPTY(lcSizes)
    lcSizes = POSLN.SCALE + '~' + lcSizes
  ENDIF
  lcCrsRef = ''
  IF LEN(BOMLINE.CCOMPSIZES) > 0
    FOR lnI = 1 TO LEN(BOMLINE.CCOMPSIZES)
      =SEEK(BOMLINE.ITEM,'STYLE')
      lcCrsRef = lcCrsRef+POSLN.SCALE+','+SUBSTR(BOMLINE.CCOMPSIZES,lnI,1)+'~'+STYLE.SCALE+','+;
                 SUBSTR(BOMLINE.CSIZES,lnI,1)+CHR(13)+CHR(10)
    ENDFOR
  ENDIF
  SELECT (lcTmpBomLn)
  APPEND BLANK
  REPLACE cItmMajor  WITH SUBSTR(lcStyle,1,LEN(lcMjrMsk)) ,;
          Typ        WITH BomLine.cBomtyp  ,;
          cItmMask   WITH lcStyle          ,;
          cOprCode   WITH BomLine.cOprCode ,;
          MfgCode    WITH BomLine.MfgCode  ,;
          Item       WITH BomLine.Item     ,;
          IClr       WITH BomLine.IClr     ,;
          Trim_Invt  WITH llTrim_Inv       ,;
          Desc       WITH lcRecDesc        ,;
          lBasOnSiz  WITH llBaseOnSz
  REPLACE MSIZES     WITH lcSizes          ,;
          MSZCROSREF WITH lcCrsRef         ,;
          UOM        WITH lcUom            ,;
          UNTCOST    WITH BOMLINE.UNITCOST ,;
          NPERCENT   WITH BOMLINE.NPERCENT ,;
          NESTBOMQTY WITH BOMLINE.UNITQTY  ,;
          NBOMWASTGE WITH 0                ,;
          NBOMTOTQTY WITH NESTBOMQTY       ,;
          TOTCOST    WITH NESTBOMQTY*UNTCOST,;
          CCOSTSTAT  WITH BOMLINE.CCOSTSTAT
  REPLACE CCATGTYP   WITH BOMLINE.CCATGTYP ,;
          CMARKER    WITH lcMarker
ENDSCAN

llRetVal = gfSheetItem ("I", laData[1] , lcLinkCode, lcStyle    ,;
                        "" , lnPoLNo   , lcCostDye , POSHDR.cItemWare ,;
                        POSHDR.cMatWare, @laBomQty , lcTmpBomLn , "CtktBom"  ,;
                        "BomLine", "MFGOprHd", lcLastOpr , lnPrice    ,;
                        0, 0, 0, 0, 0,lcPassFile)
USE IN (lcTmpBomLn)
ERASE (gcWorkDir+lcTmpBomLn+'.DBF')
ERASE (gcWorkDir+lcTmpBomLn+'.CDX')
ERASE (gcWorkDir+lcTmpBomLn+'.FPT')
*-- end of lfModBom.
*!*************************************************************
*! Name      : lfChkOrdF
*! Developer : Mohamed Shokry (MHM)
*! Date      : 07/27/2004
*! Purpose   : To check prim fabric in new concept (sales order cost sheet).
*!*************************************************************
*! Example   : = lfChkOrdF()
*!*************************************************************
*! C037959,1 MHM 07/27/2004
*!*************************************************************
FUNCTION lfChkOrdF
PARAMETER lcItem , lcCurrClr
PRIVATE llChkFab ,lnOldAls
llChkFab = .F.
lnOldAls = SELECT(0)

SELECT cutpick
lcOrder = IIF(SEEK("2"+laData[1]),Order,SPACE(06))
SELECT ORDBOM
IF SEEK(lcOrder)
  SCAN REST WHILE order+STR(lineno,6)+style+sclr+item+iclr+mfgcode = lcOrder
    IF PADL(Item,7) = PADL(lcItem,7) AND lprimary
      IF iclr= "******" OR iclr = lcCurrClr
        llChkFab = lprimary
        EXIT
      ENDIF  
    ENDIF
  ENDSCAN
ENDIF

SELECT(lnOldAls)

RETURN llChkFab
*!*************************************************************
*! Name      : lfGetSty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 07/27/2004
*! Purpose   : To get style for fabric
*!*************************************************************
*! Example   : = lfChkOrdF()
*!*************************************************************
*! C037959,1 MHM 07/27/2004
*!*************************************************************
FUNCTION lfGetSty
PARAMETER lcItem , lcCurrClr

PRIVATE lcCurSty ,lnOldAls
lcCurSty = ''
lnOldAls = SELECT(0)

SELECT cutpick
lcOrder = IIF(SEEK("2"+laData[1]),Order,SPACE(06))
SELECT ORDBOM
IF SEEK(lcOrder)
  SCAN REST WHILE order+STR(lineno,6)+style+sclr+item+iclr+mfgcode = lcOrder
    IF PADL(Item,7) = PADL(lcItem,7) AND (cCatgTyp = "F")
      IF iclr = "******" OR iclr = lcCurrClr
        IF iclr= "******"
          lcCurSty= SUBSTR(OrdBom.Style,1,12) +'-'+SUBSTR(OrdBom.cItmMajor,lnColorStr,lnColorLen) 
        ELSE
          lcCurSty= OrdBom.cItmMask
        ENDIF 
        EXIT
      ENDIF  
    ENDIF
  ENDSCAN
ENDIF

SELECT(lnOldAls)

RETURN lcCurSty

*!*************************************************************
*! Name      : lfReCalQty
*! Developer : Mohamed Shokry - (MHM)
*! Date      : 09/25/2004
*! Purpose   : Valid function of the recalculation button. The purpose
*!             of this function is to check if it is a (Multi)primary fabric then 
*!             it will check if the issued qty is less or more than the 
*!             required qty then a message will be displayed informing the 
*!             user that the issued qty is less or more than the required 
*!             qty. Do you want to recalculate the qty. If yes then the
*!             recalculation screen will be displayed. 
*!             IF THE ISSUED QTY = REQUIRED QTY THEN THE MESSAGE WILL NOT BE 
*!             DISPLAYED AND WE WILL DISPLAY THE RECALCULATION SCREEN ONLY.
*!*************************************************************
*! Example   : = lfReCalQty()
*!*************************************************************
*C037959,1 MHM 09/20/2004 rewrite according to new concept[Start]
*!*************************************************************
*--
FUNCTION lfReCalQty
PRIVATE llDistPrim, lnAlias,lnCurrRec,lcCurrFab
STORE '' TO lcCurrFab
llDistPrim = .F.
lnOldAls = SELECT(0)
lnCurrRec  = RECNO(lcTmpQuery) 
lnMiReq    = 0
lnSeeIssu  = 0
lcMiItm    = &lcTmpQuery..Item
lcMiClr    = &lcTmpQuery..Color
lcStyle    = &lcTmpQuery..Style
lnNoOfFb  = 0
lnStyReq  = 0

IF &lcTmpQuery..lPrimary
  lnNoOfLn = lfGetPoLn(lcMiClr)
  SELECT (lcTmpQuery)
  SCAN FOR lPrimary
    IF &lcTmpQuery..Style = lcStyle
      lnNoOfFb = lnNoOfFb +1
      lnMiReq    = lnMiReq + &lcTmpQuery..REQ
      lnSeeIssu  = lnSeeIssu + &lcTmpQuery..Used
      lnStyReq  = lnStyReq +&lcTmpQuery..Unit    
    ENDIF
  ENDSCAN
  GOTO lnCurrRec
  IF lnSeeIssu = 0
    RETURN .F.
  ENDIF
  *-- Function to Distri Fabric on Style.
  llStrt = .F.

  SELECT (lcTmpQuery)
  SCAN FOR lPrimary
    IF !EMPTY(&lcTmpQuery..Style) AND SEEK('P'+laData[1]+&lcTmpQuery..Style,'PosLn')
      SELECT PosLn
      LOCATE REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = ;
                        'P'+laData[1]+&lcTmpQuery..STyle;
                  FOR   TranCd = '2'
      IF FOUND()
        = gfModalGen('INM00000B00000',.F.,.F.,.F.,;
                  'Style '+ ALLTRIM(&lcTmpQuery..STyle) + ' has received quantities . Cannot proceed.')
        RETURN (.F.)
      ENDIF                  
    ENDIF
  ENDSCAN

  SELECT (lcTmpQuery)
  IF lnSeeIssu <> lnMiReq
    lcMsgAns = gfModalGen('INM00000B00006',.F.,.F.,.F.,;
                 "Issued quantities is "+ALLT(STR(ABS(lnSeeIssu-lnMiReq)))+;
                 IIF(lnSeeIssu-lnMiReq>0," more"," less")+;
                 " than required. Recalculate quantities?")
  ELSE
    lcMsgAns = 1
  ENDIF

  *-- If the user agrees to recalculate the Po lines quantities.
  IF lcMsgAns = 1
    IF SEEK("P"+ladata[1],'PosHdr')
      *-- If there is only one line in the PO
  
      IF lnNoOfLn = 1 
        *-- To distribute the qty for the first line
        GOTO lnCurrRec
        lcStyCode1 = &lcTmpQuery..Style
        =lfDistLin1(lcMiClr)
      ELSE
        IF lnNoOfFb = 1
          GOTO lnCurrRec
          lcStyCode1 = &lcTmpQuery..Style
          lcStyCode2 = &lcTmpQuery..Style
          lnStyReq   = &lcTmpQuery..Unit
          =lfGetPoLn(lcMiClr)
          =lfDistLin2(lcMiClr)
        ELSE
          GOTO lnCurrRec
          lnStyReq   = &lcTmpQuery..Unit
          =lfDistLin3(lcMiClr)
        ENDIF 
      ENDIF
    ENDIF
  ENDIF 

ENDIF
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfActPad
*! Developer : Mohamed Shokry - (MHM)
*! Date      : 09/25/2004
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActPad()
*!*************************************************************
*C037959,1 MHM 09/20/2004 [Start]
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT "Po Cost Sheet  " SKIP FOR Empty(laData[1])
ON SELECTION POPUP _OPTIONPOP DO lpPoCstSht

*!*************************************************************
*! Name      : lpvActBar
*! Developer : Mohamed Shokry - (MHM)
*! Date      : 09/25/2004
*! Purpose   : Call Po Cost Sheet Program
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*C037959,1 MHM 09/20/2004 rewrite according to new concept[Start]
FUNCTION lpPoCstSht
PRIVATE llOpnBom
*-- Call the Po cost sheet program.

llNoShow    = .F.
llCallShow  = .T.
llShow      = .F.

lcParameter = "'" + laData[1] + "'"+",.F."
DO gpDoProg WITH "AWRPOCSSH", .F., "PO", lcParameter


*!*************************************************************
*! Name      : lfvWarHus
*! Developer : Mohamed Shokry - (MHM)
*! Date      : 09/25/2004
*! Purpose   : Validate WareHouse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*C037959,1 MHM 09/20/2004 
FUNCTION lfvWarHus

=gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
SELECT cDesc,cWareCode FROM WAREHOUS INTO ARRAY laWareHous
=gfCloseFile('WAREHOUS')

SELECT BOMCOST
SET ORDER TO Pobomcls
*--- cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession
=SEEK("I"+laData[1])
SCAN REST WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession=;
                "I"+laData[1];
          FOR   PADL(Item,7)+Iclr = &lcTmpQuery..Item+&lcTmpQuery..Color
  lcWareCod = cWareCode        
  EXIT
ENDSCAN          

lnWareHouse= ASCAN(laWareHous,lcWareCod)
lnWareHous= IIF(lnWareHous=0,1,ASUBSCRIPT(laWareHous,lnWareHous,1))
*!*************************************************************
*! Name      : lfRtrnQty
*! Developer : Mohamed Shokry - (MHM)
*! Date      : 09/25/2004
*! Purpose   : Validate return QTY
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*C037959,1 MHM 09/20/2004 

FUNCTION lfRtrnQty
PARAMETER lnTotRet
lcIssRet = "R"
PRIVATE lnOldAls , llCanDist , lnSeeIssu 
lnOldAls = SELECT(0)
STORE .F. TO llCanDist, llDistDone

IF lnWareHous <> 0 
  lcWareCode = laWareHous[lnWareHous,2]
ENDIF

lnSeeIssu  = 0
IF lfGetOpn(&lcTmpQuery..Item,&lcTmpQuery..Color,lcIssRet)
  lcOldVal = ""

  llCanDist = (&lcTmpQuery..Issue=0 OR &lcTmpQuery..Issue < &lcTmpQuery..REQ)

  STORE .T. TO llIssue
  ldMiDat    = gdSysDate
  lcMiItm    = &lcTmpQuery..Item
  lcMiClr    = &lcTmpQuery..Color

  lnMiReq    = MAX(&lcTmpQuery..REQ - &lcTmpQuery..Used,0)
  lnMiToCal =lnMiReq

  SELECT CtktBom
  SET ORDER TO Ctktyp

  =SEEK("I"+laData[1]+PADR(lcMiItm,19)+lcMiClr)
  lcOperCode = cOprCode
  DECLARE laLots[1]
  =gfOpenFile(gcDataDir+"mfgoprdt",gcDataDir+"mfgoprdt","SH")
  SELECT DIST cLotNo FROM mfgoprdt ;
  WHERE cIMTYp+cTktNo+cOprCode+cLotNo+Item+Color+TranCd+cDyelot= "I"+laData[1]+lcOperCode ;
  AND TranCd='1'INTO ARRAY laLots
  SELECT CtktBom
  SET ORDER TO CtktBom

  REPLACE  &lcOpenLots..nReturn  WITH lnTotRet     
  REPLACE  &lcOpenLots..cWareCode  WITH lcWareCode
  IF !SEEK(lcMiItm+lcMiClr+lcWareCode,'FABDYE')
    DO gpAdFabWar WITH lcMiItm, lcMiClr,SPACE(10), lcWareCode
  ENDIF
  
  =lfAplyRet()
  SELECT (lcTmpQuery)
   REPLACE &lcTmpQuery..Used  WITH &lcTmpQuery..Used-lnTotRet,;
           &lcTmpQuery..Onhnd WITH &lcTmpQuery..OnHnd + lnTotRet

  IF USED("FABINV")
    USE IN FABINV
  ENDIF
ENDIF
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfDistLin3
*! Developer : Mohamed Shokry - (MHM)
*! Date      : 09/25/2004
*! Purpose   : To recalculate the PO line if there are 2 line2 in the PO
*!*************************************************************
*! Example   : = lfDistLin3()
*!*************************************************************
*C037959,1 MHM 09/20/2004 rewrite according to new concept[Start]
FUNCTION lfDistLin3
PARAMETERS lcStyClr
PRIVATE lnScalCnt, lnPrePack1, lnPrePack2, lnPrePack3, lnPrePack4,;
                   lnPrePack5, lnPrePack6, lnPrePack7, lnPrePack8,;
                   lnOldQty1 , lnOldQty2 , lnOldQty3 , lnOldQty4,;
                   lnOldQty5 , lnOldQty6 , lnOldQty7 , lnOldQty8,;
                   lnOldQty9 , lnOldQty10, lnOldQty11, lnOldQty12,;
                   lnOldQty13, lnOldQty14, lnOldQty15 , lnOldQty16,;
                   lnCount
llPrePack = .F.

lcTmpPoLn = gfTempName()

PRIVATE lnAlias, lcDistSty, lcDisClr, lnDiff, lcDisMul,;
        lcOldTab , lcOldBkTab, lcOldEsc, lcAltB

STORE '' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8,;
            lcSz9,lcSz10,lcSz11,lcSz12,lcSz13,lcSz14,lcSz15,lcSz16

lnAlias    = SELECT()
=SEEK('P'+laData[1]+&lcTmpQuery..Style,'PosLn')
lcDistSty  = LEFT(&lcTmpQuery..Style,12)
lcDisClr   = SUBSTR(PosLn.Style,14,6)
lnDiff     = lnSeeIssu - lnMiReq 
lnCosting  = &lcTmpQuery..Unit
lcDisMul   = "Distribute Line Quantities"
lcTmpCutPk = gfTempName()
lcTmpIndx  = gfTempName()
lcStyCode1 = &lcTmpQuery..Style
lcStyCode2 = &lcTmpQuery..Style
*-- To get the sizes description
FOR lnOuter = 1 TO 2
  lcOuter = STR(lnOuter,1)
  =SEEK(lcStyCode&lcOuter,'Style')
  =SEEK('S'+Style.Scale,'Scale')
  
  FOR lnInde = 1 TO 8
    lcIndex = STR(lnInde,1)
    lnCount  = lnInde + IIF(lnOuter = 1,0,8)
    lcCount  = ALLTRIM(STR(lnCount))
    lcSz&lcCount = Scale.Sz&lcIndex
  ENDFOR  
ENDFOR

*-- To get the cut pick records
CREATE CURSOR (lcTmpCutPk);
              (cOrder C(6), cOrdLine1 C(6), cStore1 C(8), cStyle1 C(19),;
               cOrdLine2 C(6), cStore2 C(8),cStyle2 C(19),nRequired N(13,2),;
               nUse N(13,2),cType C(5), cPrepack1 C(1),cPrepack2 C(1),;
               nQty1 N(6), nQty2 N(6), nQty3 N(6), nQty4 N(6),;
               nQty5 N(6), nQty6 N(6), nQty7 N(6), nQty8 N(6),;
               nQty9 N(6), nQty10 N(6), nQty11 N(6), nQty12 N(6),;
               nQty13 N(6), nQty14 N(6), nQty15 N(6), nQty16 N(6),;
               cTrantype C(1),cTktLn1 C(6), cTktLn2 C(6),Item C(07) , Color C(06))
INDEX ON cTranType TAG (lcTmpIndx) OF (lcTmpCutPk)
INDEX ON Item+Color+cOrder+cStore1+cPrePack1+cTranType TAG (lcTmpCutPk) OF (lcTmpCutPk)

STORE SPACE(0) TO lcCutSty , lcCutStore , lcCutPpack
SELECT CutPick
=SEEK('2'+laData[1])
SCAN REST WHILE TranCd+cTktNo+Style = '2'+laData[1] ;
          FOR SUBSTR(Style,lnColorStr,lnColorLen) = lcDisClr

  =SEEK('O'+CutPick.Order+PADL(CutPick.cOrdLine,6,' '),'OrdLine')
  =SEEK(CutPick.Style,'Style')  
  =SEEK('S'+Style.Scale,'Scale')
  lnScalCnt = Scale.Cnt
  SELECT (lcTmpQuery)

  SCAN FOR lPrimary AND &lcTmpQuery..Style = CutPick.Style
    lnCosting  = &lcTmpQuery..Unit

    SELECT (lcTmpCutPk)
    *IF !SEEK(CutPick.Order+OrdLine.Store+OrdLine.PrePak)
    IF !SEEK(&lcTmpQuery..Item +&lcTmpQuery..Color+CutPick.Order+OrdLine.Store+OrdLine.PrePak)
    
      *-- Record for the useage and required quantites
      INSERT INTO (lcTmpCutPk) ;
             (cOrder,cOrdLine1,cStore1,nRequired,nUse,cTranType,cTktLn1,;
             cStyle1,cPrepack1,Item,Color);
             VALUES;
             (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,CutPick.TotQty*lnCosting,;
             CutPick.TotQty*lnCosting,'A',CutPick.cTktLineNo,CutPick.Style,;
             ORDLINE.PREPAK,&lcTmpQuery..Item ,&lcTmpQuery..Color)


      *-- Record for the quantities
      INSERT INTO (lcTmpCutPk) ;
              (cOrder,cOrdLine1,cStore1,cTranType,cType,cTktLn1,cStyle1,;
              cPrepack1,Item,Color);
              VALUES;
             (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,'B','Qty',;
              CutPick.cTktLineNo,CutPick.Style,ORDLINE.PREPAK,&lcTmpQuery..Item ,&lcTmpQuery..Color)
      SELECT (lcTmpCutPk)
      FOR lnCntr = 1 TO 8
        lcCntr   = STR(lnCntr,1)
        REPLACE nQty&lcCntr WITH CutPick.Qty&lcCntr
      ENDFOR  
  
      *-- Record for the prepack
      INSERT INTO (lcTmpCutPk) ;
             (cOrder,cOrdLine1,cStore1,cTranType,cType,cTktLn1,cStyle1,;
              cPrePack1,Item,Color);
              VALUES;
             (CutPick.Order,CutPick.cOrdLine,OrdLine.Store,'C','Ratio',;
              CutPick.cTktLineNo,CutPick.Style,ORDLINE.PREPAK,&lcTmpQuery..Item ,&lcTmpQuery..Color)

      IF SEEK("P"+Style.Scale+OrdLine.PrePak,'SCALE')
        SELECT (lcTmpCutPk)
        FOR lnPpCnt = 1 TO lnScalCnt
          lcPpCnt = STR(lnPpCnt,1)
          REPLACE nQty&lcPpCnt WITH Scale.Pp&lcPpCnt
        ENDFOR
      ENDIF
    ELSE
      =SEEK('S' + ORDLINE.SCALE , 'SCALE')
      SELECT (lcTmpCutPk)
 
      *-- Record for the useage and required quantites
      SEEK CutPick.Order+OrdLine.Store+OrdLine.PrePak+'A'
      REPLACE nRequired WITH nRequired + (CutPick.TotQty*lnCosting),;
              nUse      WITH nUse      + (CutPick.TotQty*lnCosting)
 
      *-- Record rest of the sizes
      SEEK CutPick.Order+OrdLine.Store+OrdLine.PrePak+'B'
      FOR lnInde = 1 TO 8
        lcIndex = STR(lnInde,1)
        lnCount  = lnInde + 8
        lcCount  = ALLTRIM(STR(lnCount))
        REPLACE nQty&lcCount WITH CutPick.Qty&lcIndex
      ENDFOR
      REPLACE cOrdLine2 WITH CutPick.cOrdLine  ,;
              cStore2   WITH OrdLine.Store     ,;
              cTktLn2   WITH CutPick.cTktLineNo,;
              cStyle2   WITH CutPick.Style     ,;
              cPrePack2 WITH OrdLine.PrePak

      *-- Record for the ratio
      SEEK CutPick.Order+OrdLine.Store+OrdLine.PrePak+'C'
      IF SEEK("P"+Style.Scale+OrdLine.PrePak,'SCALE')
        SELECT (lcTmpCutPk)
        FOR lnPpCnt = 1 TO lnScalCnt
          lcPpCnt = STR(lnPpCnt,1)
          lnCount  = lnPpCnt + 8
          lcCount  = ALLTRIM(STR(lnCount))
          REPLACE nQty&lcCount WITH Scale.Pp&lcPpCnt
        ENDFOR
      ENDIF  
    ENDIF  
  ENDSCAN 
ENDSCAN

lcOldTab   = ON("KEY","TAB")
lcOldBkTab = ON("KEY","BACKTAB")
lcOldEsc   = ON("KEY","ESC")
lcAltB     = ON("KEY","ALT+B")
POP KEY
DO (gcScrDir + gcWinAppl + '\PODISMUA.SPX')

ON KEY LABEL TAB &lcOldTab
ON KEY LABEL BACKTAB &lcOldBkTab
ON KEY LABEL ESCAPE &lcOldEsc
ON KEY LABEL ALT+B &lcAltB
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfOldVal
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/11/2006
*! Purpose   : Valid function for Issue / Return Date
*!*************************************************************
*! Example   : = lfvIssDat()
*!*************************************************************
*B131603
FUNCTION lfOldVal
ldOldDate = EVAL(SYS(18))

*!*************************************************************
*! Name      : lfvIsRtDat
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/11/2006
*! Purpose   : Valid function for Issue / Return Date
*!*************************************************************
*! Example   : = lfvIsRtDat()
*!*************************************************************
*B131603
FUNCTION lfvIsRtDat
IF ldMiDat <> ldOldDate OR !llChecked
  IF llGlLink AND !CHECKPRD(ldMiDat,'lcGlYear','lcGlPeriod','IA')
    *B608049,1 TMI [Start] do not change to the old value if not invalid date
    *ldMiDat = ldOldDate
    *B608049,1 TMI [End  ] 

    _CUROBJ = OBJNUM(ldMiDat)

    *B608049,1 TMI [Start] disable the issue button if invalid date
    SHOW GET pbMIss DISABLE
    *B608049,1 TMI [End  ] 
    RETURN
  ENDIF  
  llChecked = .T.
  *B608049,1 TMI [Start] enable issue button if valid date
  SHOW GET pbMIss ENABLE
  *B608049,1 TMI [End  ] 
ENDIF

*!*************************************************************
*! Name      : lfvIssDate
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/11/2006
*! Purpose   : Valid function for Issue Date
*!*************************************************************
*! Example   : = lfvIssDate()
*!*************************************************************
*B131603
FUNCTION lfvIssDate
IF ldIssDate <> ldOldDate OR !llChecked
  IF llGlLink AND !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
    ldIssDate = ldOldDate
    _CUROBJ = OBJNUM(ldIssDate)
    RETURN
  ENDIF  
  llChecked = .T.
ENDIF








*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
FUNCTION Checkprd
PARAMETERS ldDate,lcFYear,lcPeriod,lcTranTyp,llHideMsg

PRIVATE lcDType,lcAddMes1,lcAddMes2,lcSysDir,lcGlVers,lcGlComp, ;
        lcDate,llContinue,lcErrorM1,lcErrorM2, lnAlias
        
lnAlias = SELECT()
STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO

=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',gcAct_Comp)
lcSysDir   = ALLTRIM(M_SYS_DIR)
lcGlVers   = ALLTRIM(M_GL_VERS)
lcGlComp   = ALLTRIM(M_GL_CO)
STORE SPACE(1) TO lcDType,lcAddMes1,lcAddMes2

lcDate = DTOC(ldDate)      && Transaction date as a string used in messages
IF lcGlVers = 'S'            &&   <<<... SBT 2.5 ... >>>
  *B602317,1 Open SBT system company file with another name
  *=gfOpenFile(lcSysDir+'SYCCOMP',lcSysDir+'COMPID','SH')
  *=SEEK(lcGlComp,'SYCCOMP')

  USE lcSysDir+'SYCCOMP' ORDER TAG 'COMPID' IN 0 AGAIN ALIAS 'SBTCOMP'
  =SEEK(lcGlComp,'SBTCOMP')
  *B602317,1 (End)
  
  =gfOpenFile(lcSysDir+'SYCHFIS',lcSysDir+'COMPID1','SH')
  =gfOpenFile(lcSysDir+'SYCDFIS',lcSysDir+'COMPID1','SH')

  llContinue = .T.
  IF SEEK(lcGlComp)
    LOCATE REST FOR BETWEEN(ldDate,Bdate,Edate) ;
                WHILE (ldDate >= Bdate) .AND. (CompId = lcGlComp)
  ENDIF
  IF !FOUND()                && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = SUBSTR(Yearprd,1,4)      && Transaction date year
    &lcPeriod = SUBSTR(Yearprd,5,2)      && Transaction date period     
  ENDIF  
  IF llContinue .AND. Permlck         && Permanently locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a permanently locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue .AND. Plocked         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue              && So far so good
    IF Pclosed               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')  && Transaction is neither 
                                  && 'Void invoice' nor 'void return'.
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
        =gfDialog( 'I',lcErrorM1+lcErrorM2)
      ELSE  
        llContinue = .F.
      ENDIF
    ELSE    && Period not closed. Check if it is a future period
      *B602317,1 Open SBT system company file with another name
      *IF Yearprd <>  SYCCOMP.CURYR+SYCCOMP.CURPRD .AND. !(lcTranTyp $ 'VI2VR2')
      IF Yearprd <>  SBTCOMP.CURYR+SBTCOMP.CURPRD .AND. !(lcTranTyp $ 'VI2VR2')
      *B602317,1 (End)

        llDummy   =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
        =gfDialog( 'I',lcErrorM1+lcErrorM2)
      ENDIF
    ENDIF    
  ENDIF  
  *B602317,1 Open SBT system company file with another name
  USE IN SBTCOMP
  *B602317,1 (End)

ELSE
  =gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'CCOMP_ID','SH')
  =SEEK(gcPrnt_Cmp,'SYCCOMP')
  IF 'GL' $ SYCCOMP.mModlset
    *=gfOpenFile(ALLTRIM(SYCCOMP.CCOM_DDIR)+'GLSETUP','','SH')
    *E301098,1 Hesham (Start)
    *USE (ALLTRIM(SYCCOMP.CCOM_DDIR)+'GLSETUP') SHARED AGAIN ALIAS TGLSETUP IN 0
    USE (gfGetDataDir(ALLTRIM(SYCCOMP.CCOM_DDIR))+'GLSETUP') SHARED AGAIN ALIAS TGLSETUP IN 0
    *E301098,1 Hesham (End)
    lDSETBBDAT=TGLSETUP.DSETBBDAT
    *-- Variable that hold the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    llAllPBB = TGLSETUP.LSETALBBE
    *-- Variable that showes the Allow posting before beginning balance (End)
    USE IN TGLSETUP 
  ELSE  
    lDSETBBDAT={}
    *-- Variable that showes the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    *-- .T. is assigend as default because we need not to check
    *-- if the GL module not installed or not linked
    llAllPBB = .T.
    *-- Variable that hold the Allow posting before beginning balance (End)
  ENDIF  
  *E300692,5 Use FISHD, FSPRD instead of SYCFISHD, SYCFSPRD
  *=gfOpenFile(gcSysHome+'SYCFISHD',gcSysHome+'COMPFYEAR','SH')
  *=gfOpenFile(gcSysHome+'SYCFSPRD',gcSysHome+'COMFYRPRDI','SH')
  =gfOpenFile(gcDataDir+'FISHD',gcDataDir+'COMPFYEAR','SH')
  =gfOpenFile(gcDataDir+'FSPRD',gcDataDir+'COMFYRPRDI','SH')
  *E300692,5 end
  llContinue = .T.
  *E300789,1 Hesham (Start)
  *IF SEEK(lcGlComp)
  LOCATE
  IF FOUND()
  *E300789,1 Hesham (End)
    *E300789,1 Remove cComp_ID
    *LOCATE REST FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) ;
                WHILE (ldDate >= Dfsppbgdt) .AND. (CComp_Id = lcGlComp) 
    LOCATE REST FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) ;
                WHILE (ldDate >= Dfsppbgdt)                 
    *E300789,1 end
  ENDIF
  IF !FOUND()                  && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = Cfisfyear      && Transaction date year
    &lcPeriod = Cfspprdid      && Transaction date period     
  ENDIF  
  IF llHideMsg
    SELECT (lnAlias)
    RETURN(llContinue)
  ENDIF
  *** Check if transaction date falls in a history period.
  IF llContinue .AND. Cfisfyear < STR(VAL(SYCCOMP.CCURR_YER)-1)
    llContinue = .F.
    lcErrorM1 = ' belongs to a history fiscal year.'
    lcErrorM2 = ''
  ENDIF 
  IF llContinue         
    *** Check if the transaction date before the begining balance
    *** date, and if the user is allowed to post before the begining
    *** balance date

    *-- Check if the system is linked to GL And Allow posting before beginning Balance (Start)
    *-- AAMER 11/12/98
    *IF !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
    IF lcGlVers='A' AND !llAllPBB AND !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
    *-- Check if the system is linked to GL And Allow posting before beginning Balance (End) 
      llContinue = .F.
      lcErrorM1 = ' falls before the begining balance date.'
      lcErrorM2 = ' No posting allowed before the begining balance date. '
    ENDIF  
  ENDIF  
  IF llContinue .AND. Lfsplocks         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue 
    IF Lfspclsds               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
        *E300420,1 Message : 00274
        *E300420,1 
        *E300420,1 Button : 00000 
        *E300420,1 Ok
        
        *B802236 (Start)
        IF lcTranTyp # 'VI1'
        *B802236 (End)
          =gfModalGen('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
        *B802236 (Start)
        ENDIF
        *B802236 (End)
          
        *=gfDialog( 'I',lcErrorM1+lcErrorM2)
      ELSE  

        *B802236 (Start)
        IF lcTranTyp # 'VI2'
        *B802236 (End)
          llContinue = .F.
        *B802236 (Start)
        ENDIF
        *B802236 (End)
          
      ENDIF
    ELSE      && Period not closed. Check if it is a future period.
      IF Cfisfyear+Cfspprdid <> SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD .AND. !(lcTranTyp $ 'VI2VR2')
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
        *E300420,1 Message : 00274
        *E300420,1 
        *E300420,1 Button : 00000 
        *E300420,1 Ok
        =gfModalGen('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
        *=gfDialog( 'I',lcErrorM1+lcErrorM2)
      ENDIF
    ENDIF    
  ENDIF  
ENDIF
IF !llContinue             && There is an error.
  IF lcTranTyp $ 'VI2VR2'       && Transaction is either 'Void invoice'
                                && or 'Void return'
    lcErrorM1  = ' not in the current period. '
    lcErrorM2 = ''
  ENDIF
  llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
  lcErrorM1= lcDType + lcDate + lcErrorM1
  *E300420,1 Message : 00274
  *E300420,1 
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  
  *B802236 (Start)
  IF lcTranTyp # 'VI2'
  *B802236 (End)
    =gfModalGen('INM00274B00000','ALERT',lcErrorM1+lcErrorM2+lcAddMes1+lcAddMes2)
  *B802236 (Start)
  ENDIF
  *B802236 (End)

  *=gfDialog( 'I',lcErrorM1+lcErrorM2+lcAddMes1+lcAddMes2)
  SELECT (lnAlias)
  RETURN(.F.)
ENDIF
SELECT (lnAlias)
RETURN(.T.)
*****
* End of Function CheckPrd
