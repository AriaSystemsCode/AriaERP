*:***************************************************************************
*: Program file  : iclocn
*: Program desc. : Location Maintenance
*: For screen    : iclocn,iclocns
*:        System : Aria Advantage Series.
*:        Module : Inventory Control (IC) / Material (MA).
*:     Developer : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:     Procedures : lpTab,lpBackTab,lpShow,lpDelScr,lpSavscr
*:     Functions  : gfGetMemVar,gfSetup,gfTempName,gfStopBrow,gfCPClose     ,
*:                : gfModalGen,lfCollSite                                   ,
*:                : lfActBrow,lfVBrow,lfwBrow,lfvWareHs,lfvStates,lfvStyInv ,
*:                : lfvMatInv,lfvBin,lfTrap,lfUnTrap,lfvNew,lfvRemove       ,
*:                : lfRefresh,lfRemvLine,lfInStyFab
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO iclocn
*:***************************************************************************
*: Note that : If we use single warehouse and we find this warehouse
*:           : in warehouse file we must enter screen in view mode. 
*:***************************************************************************
*: Modification :
*E300889,1 TAK 06/23/1998 Added to control the location editing if the
*E300889,1                system was setup to Point of Sale.
*E300889,4 MAB 07/21/1998 Added to Display N/A in sites popup if select mode
*E300889,4                or View mode and user does not select sites before.
*E300872,7 MAB 04/30/1998  Adjust Program for Communication process.
*E300872,7  Code for fill sites popup with company sites. [begin]
*E300872,7  llSite : .T. if user install communication module and machine
*E300872,7         : System type is Either Point of Sell OR back office.
*B602066,1 AMM 08/05/1998  Fix the bug of 'File FABDYE not found'
*B602171,1 MAB 11/17/1998  Fix Several bugs in screen. 
*E301486,1 AMH 11/19/2000  Change the check of comuncation module from CM to be EC. 
*B604104,1 BWA 10/30/2001 Fix the bug of the seek is not working in the state screen.
*E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char
*C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*N123070,1 NNA 07/14/2004 Add new field [3PL Provider] to the ware house Scareen and the field
*N123070,1 NNA            name in the table is [cThrdplPr] 
*E131400,1 NNA 04/12/2006 Add defaule location option to the location screen.
*C132754,1 TMI 08/03/2006 Add a trigger to update the CREPLENISH field for GPS00
*B607919,1 TMI 03/01/2007 Fix a bug record out of range when adding a new Wh.  ( Ticket # T20061220.0001 )
*:***************************************************************************
*:
*E300889,1  Initialize variable holds the system type setup.
*E301077,21 MAB 01/05/1999 Open and close files.
*
EXTERNAL ARRAY laData , laDefProc 
DECLARE laScrMode[4],laSetups[3,2]
*E131400,1 NNA 04/12/2006 Add defaule location option to the location screen.[Start]
llDefCode = .F.
*E131400,1 NNA [End]
*E301486,1 AMH 11/19/2000 Declare array to hold the cSiteId code information [Start]
DIMENSION laCodInfo [1,10]
laCodInfo = SPACE(0)
laCodInfo[1,01] = "CSITEID"
laCodInfo[1,02] = "laSites"
laCodInfo[1,03] = "lnSite"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.            
laCodInfo[1,06] = .F.            
laCodInfo[1,07] = "WAREHOUS"
laCodInfo[1,08] = "WAREHOUS"
laCodInfo[1,09] = "LADATA[1]"
laCodInfo[1,10] = "cSiteId"
*E301486,1 AMH [End]

*E301077,21 Flags to open and close files.
*-- llOpenCode : .T. if this program open codes  file.
*-- llOpenSite : .T. if this program open Sites  file.
*-- llLocLines : .T. if this program open Bins   file.
*-- llOpFabDye : .T. if this program open Fabdye file.
*-- llMatInst  : .T. if Material module is installed.
*-- llSite     : System have sites.

*STORE .F. TO llOpenCode,llOpenSite,llLocLines,llOpFabDye,llMatInst
STORE .F. TO llOpenCode,llMatInst

*llSite = ('CM' $ gcComp_Mdl) AND (gfGetMemVar('M_SYSTYPE') != 'M')

*E301077,21 Comment out the following block and Transfer it after [Begin]
*E301077,21 gfsetup function calling.
*IF llSite
  *E300889,4 Define popup array and initializing it. [begin]
  *DIMENSION laSites[1,2]
  *laSites[1,1] = 'N/A'
  *lnSite = 1
  *E300889,4 Define popup array and initializing it. [end]
  
  *lnFrAlias = SELECT()
  *IF !USED('sycsites')
    *E301077,21 Rise flag detect file opening.
    * = gfOpenFile(gcSysHome+'sycsites',gcSysHome+'cSiteId','SH')
    *llOpenSite = gfOpenFile(gcSysHome+'sycsites',gcSysHome+'cSiteId','SH')
  *ENDIF

  *E300889,1 Select site array depends on system type setup.
  *E300889,1 Select site array depends on system type setup.
  *E300889,4 Comment Next condition which maked by [E300889,1]
  *E300889,4 lcSitCond = IIF(lcSysType = 'P',"cSiteId=gcCurSite",".T.")
  
  *E300889,4 Collect array transfered to lpShow [Begin]
  *-- Collect data for array laSites
  *E300889,4 SELECT cSiteDesc , cSiteId ;
   *FROM sycsites             ;
   *WHERE &lcSitCond          ;
   *ORDER BY cSiteId          ;
   *INTO ARRAY laSites

  *E300889,4 SELECT (lnFrAlias)

  *-- If there is no collection from last SELECT SQL, there is no sites in file. 
  *-- thus you must return
  *E300889,4IF _TALLY = 0
    *-- No Sites found, Cannot Proceed.
    *--          <  OK  >
    *E300889,4 = gfModalGen("TRM42141B42000","DIALOG")
    *E300889,4 RETURN
  *E300889,4 ENDIF
  *E300889,4 Collect array transfered to lpShow [Begin]

  *E300889,4 Add Next block instead of previous one to check [begin]
  *E300889,4 for existing of sites in sites file. 
  *IF !SEEK(IIF(lcSysType = 'P',gcCurSite,''),'sycsites')
    *-- No Sites found, Cannot Proceed.
    *--          <  OK  >
    *= gfModalGen("TRM42141B42000","DIALOG")
    *RETURN
  *ENDIF
  *SELECT (lnFrAlias)
  *E300889,4 Add Next block instead of previous one to check [end]

*ENDIF
*E300872,7  Code for fill sites popup with company sites. [end]
*E301077,21 Comment out the following block and Transfer it after [End  ]


*E301077,21 Comment out the following block and Transfer it after [Begin]
*E301077,21 gfsetup function calling.
*B602066,1 AMM  Begin
*IF 'MA' $ gcComp_Mdl
*  llOpFabDye = gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYEW','SH')
*  llMatInst  = .T.
*ENDIF
*B602066,1 AMM  end
*E301077,21 Comment out the following block and Transfer it after [End  ]

*-- llSelctAdd : When .T. it means that enter screen in Select mode,
*--            :                   else enter screen in View   mode.
llSelctAdd = .T.

*-- Screen variables declaration in multilocation screen .
STORE ' ' TO lcIcLocN0,lcIcLocN1,lcIcLocN2,lcIcLocN3

*-- lcBwTitle  : Browse title in multi location case.
*-- lcWLocline : Name for temporary location file.
*-- lcUsedFile : Hold name of current used file corressponding to screen mode.
*-- lcBrowExpr : Hold Expression for location browse.
*-- lcMarker   : Hold first portion on any browse fields (cMarker > )
STORE ' ' TO lcBwTitle,lcWLocline,lcUsedFile, lcBrowExpr,lcMarker

*-- Screen variables declaration in no location screen .
STORE ' ' TO lcIcLocS0,lcIcLocS1

*-- lcAddHed1  : Hold Multi address format for current warehouse.
*-- lcOldValue : Hold Object old value
STORE ' ' TO lcAddHed1,lcOldValue

*-- lnMarker   : Hold recno of lcWareline file.
STORE 1   TO lnMarker

STORE .F. TO glFromBrow,llBrowse,llAlowNew,llNoShow

*E301077,21 No need to this variables (Clean Up code Check existance in laFileName array)
*lcLocOrd   = ''     && Variable have order of WhsLoc
*lnLocRec   = 0      && Number of record in WhsLoc file


IF !gfSetup()
  RETURN
ENDIF

*E30077,21 Define array hold Variables that come by gfGetMemVar, [Begin
*E30077,21 to avoiding open setups file for every call.
*-- llMultiWH  : When .T. it means that Multi WareHouses case.
*-- llMultiLoc : When .F. it means that no locations case.
*-- lcSysType  : Hold system type
laSetups[1,1]  = 'M_WAREHOUS'
laSetups[2,1]  = 'M_WARELOC'
laSetups[3,1]  = 'M_SYSTYPE'
=gfGetMemVar(@laSetups,gcAct_Comp)

*llMultiWH  = ALLTRIM(UPPER(gfGetMemVar('M_WAREHOUS')))  = 'Y'
*llMultiLoc = ALLTRIM(UPPER(gfGetMemVar('M_WARELOC')) )  = 'Y'
*lcSysType  = gfGetMemVar('M_SYSTYPE')
llMultiWH  = UPPER(ALLTRIM(laSetups[1,2]))  = 'Y'
llMultiLoc = UPPER(ALLTRIM(laSetups[2,2]))  = 'Y'
lcSysType  = laSetups[3,2]

*E301486,1 AMH 11/19/2000  Change the check of comuncation module from CM to be EC [Start]
*llSite     = ('CM' $ gcComp_Mdl) AND (lcSysType != 'M')
llSite     = ('NC' $ gcComp_Mdl) AND !(lcSysType $ 'MP')
IF !USED('CODES')
  = gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
ENDIF
*E301486,1 AMH [End]

*E30077,21 Define array hold Variables that come by gfGetMemVar, [End..

*-- llDoBrowFn : This flag is to prevent flashing done by twice doing of
*--            : lfactbrow (1St from .spr and 2Nd from lpshow), and after 
*--            : doing lpshow for the first time it becames .T. at all. 
STORE !llMultiWH TO llDoBrowFn


*E301077,21 This block of code instead of its equavelent befor gfSetUp [Begin
IF llSite
  lnFrAlias = SELECT(0)
  *E300889,4 Define popup array and initializing it. [begin]
  DIMENSION laSites[1,2]
  *E301486,1 AMH 11/19/2000 Fill laSites and lnSite from codes file [Start]
  *laSites[1,1] = 'N/A'
  *lnSite = 1
  = gfwCodePop ( @laCodInfo, "CSITEID" ,"N" )
  *E301486,1 AMH [End]
  *E300889,4 Define popup array and initializing it. [end]
  
  *E301486,1 AMH 11/19/2000 Next lines not needed [Start]
  *IF !USED('sycsites')
  *  *E301077,21 Rise flag detect file opening.
  *  * = gfOpenFile(gcSysHome+'sycsites',gcSysHome+'cSiteId','SH')
  *  = gfOpenFile(gcSysHome+'sycsites',gcSysHome+'cSiteId','SH')
  *ENDIF
  *
  **E300889,4 for existing of sites in sites file. 
  *IF !SEEK(IIF(lcSysType = 'P',gcCurSite,''),'sycsites')
  *  *-- No Sites found, Cannot Proceed.
  *  *--          <  OK  >
  *  = gfModalGen("TRM42141B42000","DIALOG")
  *  RETURN
  *ENDIF
  *E301486,1 AMH [End]

  SELECT (lnFrAlias)

ENDIF

IF 'MA' $ gcComp_Mdl
  = gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYEW','SH')
  llMatInst  = .T.
ENDIF
*E301077,21 This block of code instead of its equavelent befor gfSetUp [End


*************----1-----*----2-----*----3-----*----4-----*----5-----****
*N123070,1 NNA 07/14/2004 (Begin) Add the new Field to the Array
*lcScFields ='cWareCode ,cDesc ,    cAddress1  ,cAddress2 ,cAddress3 ,'+;
            'cAddress4 ,cAddress5, cAddress6,  cPhone,    cFax,      '+;
            'lStyInv,   lMatInv  , cCont_Code, uPs   ,    lFlag , cSiteId ' 
            
*E131400,1 NNA 04/12/2006 Add defaule location option to the location screen.[Start]
*lcScFields ='cWareCode , cDesc    , cAddress1 , cAddress2 , cAddress3 ,'+;
            'cAddress4 , cAddress5, cAddress6 , cPhone    , cFax      ,'+;
            'lStyInv   , lMatInv  , cCont_Code, uPs       , lFlag     ,'+;
            'cSiteId   , cThrdplPr' 
lcScFields ='cWareCode , cDesc    , cAddress1 , cAddress2 , cAddress3 ,'+;
            'cAddress4 , cAddress5, cAddress6 , cPhone    , cFax      ,'+;
            'lStyInv   , lMatInv  , cCont_Code, uPs       , lFlag     ,'+;
            'cSiteId   , cThrdplPr,ldefware' 
*E131400,1 NNA [End]            
*N123070,1 NNA (End)

***********************************************************************

DECLARE laKeyField[1,4]
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'WAREHOUS'
laKeyField[1,4] = 1

laDefProc[9]  = .F.  && To call lpSavScr from control panel global save Proc.
laDefProc[7]  = .F.  && To call lpDelScr from control panel global delete proc. 

IF !WEXIST(gcBaseWind)
  SELECT WareHous
  GO TOP
  llSelctAdd = llMultiWH OR ;
   (RECCOUNT('WareHous') = 0 OR BOF('WareHous') OR EOF('WareHous'))

  SELECT WareHous		&&Master file in this screen. 
  *-- if you enter screen in Select mode.
  IF llSelctAdd
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  ELSE		&& you enter screen in View mode.
    SCATTER FIELDS &lcScFields MEMO TO laData
    *-- In single WareHouse the modes is either View or Edit thus initially we
    *-- adjust the screen mode to be in View mode.
    laScrMode    = .F.
    laScrMode[2] = .T.

    *E131400,1 NNA 04/12/2006 (BEGIN) if there is only one location then this will be the default location
     REPLACE ldefware  WITH .T.
     llDefCode = .T.
    *E131400,1 NNA (END)

  ENDIF  

  *--Program screen windows [begin]
  IF llMultiLoc
    lcIcLocN0    = gfTempName()
    lcIcLocN1    = gfTempName()
    lcIcLocN2    = gfTempName()
    lcIcLocN3    = gfTempName()
    lcWLocline   = gfTempName()               && Temporary location file name.

    *B602171 Fix Header's browse. 
    lcBwTitle    = "Location Bins"     && Location Browse title
  ELSE
    lcIcLocS0    = gfTempName()
    lcIcLocS1    = gfTempName()
  ENDIF  
  *--Program screen windows [end]

  lcAddHed1  = PROPER(LOOKUP(SYCINT.CPARt1LAB,ALLTRIM(gcContCode),sycint.ccont_code,'CCONTCODE'))

ENDIF

SELECT (lcBaseFile)
PUSH KEY
IF llMultiLoc
  
  *E301077,21 Only open file [Begin
  *-- Open WhsLoc File under condition of system support multi Bins.[Begin
  *IF !USED('WhsLoc')
  *  llLocLines = gfOpenFile(gcDataDir+'WhsLoc',gcDataDir+'WhsLoc','SH')
  *ELSE
  *  llLocLines = .F.
  *  lcLocOrd = ORDER('WhsLoc')
  *  lnLocRec = RECNO('WhsLoc')
  *  SELECT WhsLoc
  *  SET ORDER TO TAG WhsLoc    
  *ENDIF

  IF !USED('WhsLoc')
    = gfOpenFile(gcDataDir+'WhsLoc',gcDataDir+'WhsLoc','SH')
  ENDIF
  *-- Open WhsLoc File under condition of system support multi Bins.[End..
  *E301077,21 Only open file [End..

  *--Activate Browse window
  *-- Define a menu bar with a short-cut key for browse window 
  *-- in the program just to be able to activate the browse window
  *-- using its menu bar short-cut key.
  DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
  DEFINE BAR 100 OF P01PU01 PROMPT lcBwTitle KEY ALT+B
  ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBwTitle)

  *C037816,1 MHM 04/06/2004 Define New Option Bar for entring Bin Location[Start]
  IF ASCAN(laEvntTrig,PADR("DEFNMNU",10)) <> 0
    =gfDoTriger("ICLOCN",PADR("DEFNMNU",10))
  ENDIF
  *C037816,1 MHM [End]
  
  DO (gcScrDir+'\ICLOCN.SPR')
ELSE
  *C037816,1 MHM 04/06/2004 Define New Option Bar for entring Bin Location[Start]
  IF ASCAN(laEvntTrig,PADR("DEFNMNU",10)) <> 0
    =gfDoTriger("ICLOCN",PADR("DEFNMNU",10))
  ENDIF
  *C037816,1 MHM [End]
  DO (gcScrDir+'\ICLOCNS.SPR')
ENDIF  

POP KEY

IF llMultiLoc
  RELEASE WINDOW (lcBwTitle)
  RELEASE BAR 099 OF P01PU01
  RELEASE BAR 100 OF P01PU01
ENDIF  

*E301077,21 02/26/99 Again comment out this block of code [Begin]
*E301077,21 02/26/99 because gfCleanUp close files in laFileName array
*E301077,21 Close codes file if you open it your self.
*IF llOpenCode
*  =gfCloseFile('CODES')
*ENDIF

*E301077,21 Close Sites file if you open it your self.
*IF llOpenSite
*  =gfCloseFile('sycsites')
*ENDIF

*E301077,21 Close WHSLOC file if you open it your self.
*IF llLocLines
  *USE IN WhsLoc
*  =gfCloseFile('WHSLOC')
*ELSE
*  SELECT WhsLoc
*  SET ORDER TO &lcLocOrd
*  IF lnLocRec > 0 AND lnLocRec <= RECCOUNT('WhsLoc')
*    GO lnLocRec
*  ENDIF
*ENDIF

*B602066,1 AMM Closed the file if opened by me.
*E301077,21 Close FabDye file if you open it your self.
*IF llOpFabDye
  *USE IN FABDYE
*  =gfCloseFile('FABDYE')
*ENDIF
*B602066,1 AMM  end
*E301077,21 02/26/99 Again comment out this block of code [End  ]

*--Normal exit.
IF glQuitting AND llMultiLoc
  *--erase temporary files [begin]
  IF FILE(gcWorkDir+lcWLocline+'.DBF')
    USE IN (lcWLocline)
    ERASE (gcWorkDir+lcWLocline+'.DBF')
    ERASE (gcWorkDir+lcWLocline+'.CDX')
  ENDIF
ENDIF
*-End of program code...

*--------------------------------------------------------------
*-- Browse functions and trapping section. 
*-- This section contains all functions and procedures used in 
*-- browse Bins. 
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lfActBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/10/98
*! Purpose   : Location Browse function. 
*!           : Multi Location only.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfwBrow,lfvBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfActBrow()
*!*************************************************************
FUNCTION lfActBrow
IF !EMPTY(lcBwTitle) AND WEXIST(ALLTRIM(lcBwTitle)) 
  RELEASE WINDOW (lcBwTitle)
ENDIF

lcUsedFile = IIF((laScrMode[3] OR laScrMode[4]),lcWLocline,'WhsLoc')

lcBrowExpr = "cWareCode+cLocation+Style+Color = laData[1]" + ;
  IIF(laScrMode[1] OR laScrMode[2]," AND EMPTY(Style)"," AND cStatus != 'D'")

lcMarker   = "cMarker = IIF(RECNO()=lnMarker ,'>',' '):H=' ':01:R :W =.F.,"
lcBrowflds = lcMarker + ;
             "cLocation :H= ' ':27" + ;
             " :W=(laScrMode[3] OR laScrMode[4]) AND EMPTY(cLocation)" + ;
             " :V=lfvBin()"

SELECT (lcUsedFile)
LOCATE FOR &lcBrowExpr

lnMarker = RECNO()		&&Save record pointer to draw > shape.

BROWSE FIELDS &lcBrowflds  ;
       WHEN lfwBrow()      ;
       FOR &lcBrowExpr     ;
                  NOAPPEND ;
                  NOCLEAR  ;
                  NODELETE ;
                  NOMENU   ;
                  NOWAIT   ;
                  SAVE     ;
         TITLE lcBwTitle   ;
       VALID :F lfvBrow()  ;         
       WINDOW (lcIcLocN2)  ;
    IN WINDOW (gcBaseWind)
*-- end of lfActBrow.

*!*************************************************************
*! Name      : lfVBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/10/98
*! Purpose   : Valid function for Browse Locations in Multi-location case.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVBrow()
*!*************************************************************
FUNCTION lfVBrow
IF WONTOP() # (lcBwTitle)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfVBrow

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/10/98
*! Purpose   : When function for Browse Locations in Multi-location case.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : None.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrow()
*!*************************************************************
FUNCTION lfwBrow
lnMarker    = RECNO()
llNewStat   = laScrMode[1] OR laScrMode[2]
lcNewStat   = IIF(llNewStat,"DISABLE","ENABLE")
lcRemvStat  = IIF(llNewStat OR RECCOUNT(lcWLocline) = 0        OR  ;
              BOF(lcWLocline) OR EOF(lcWLocline),"DISABLE","ENABLE")

SHOW GET pbNew    &lcNewStat
SHOW GET pbRemove &lcRemvStat
SHOW WINDOW (lcBwTitle) REFRESH
*-- end of lfwBrow.

*!*************************************************************
*! Name      : lfTrap
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/10/98
*! Purpose   : Trap function for Browse (Multi Locations)
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
*-- if TOP window is the browse window                                        
IF llMultiLoc AND ALLTRIM(WONTOP()) = ALLTRIM(lcBwTitle)
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing
  ON KEY LABEL TAB        DO lpTab
  ON KEY LABEL BACKTAB    DO lpBackTab
ENDIF
*-- end of lfTrap.

*!*************************************************************
*! Name      : lfUnTrap
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/10/98
*! Purpose   : UnTrap function for Browse (Multi Locations)
*!           : Activate screen function.
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
* -- Clear Trap
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- if TOP window is not the browse window                                        
IF llMultiLoc AND (ALLTRIM(WONTOP()) # ALLTRIM(lcBwTitle))
  ON KEY LABEL CTRL+ENTER
  ON KEY LABEL CTRL+HOME
  ON KEY LABEL CTRL+END
  ON KEY LABEL CTRL+W
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
ENDIF
*-- end of lfUnTrap.

*!*************************************************************
*! Name      : lpTab
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/10/98
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
ON KEY LABEL TAB

*-- Go to next window
IF laScrMode[1] OR laScrMode[2]
  ACTIVATE WINDOW gwcContrl1		&& Activate control panel window
  _CUROBJ = OBJNUM(pbTop)
ELSE
  ACTIVATE WINDOW (lcIcLocN3)		&& Activate Location panel window
  _CUROBJ = OBJNUM(pbNew)
ENDIF
*-- end of lpTab.

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/10/98
*! Purpose   : Trapping BACKTAB order for browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
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

*-- Go to previous window
DO CASE
  CASE laScrMode[1]
    ACTIVATE WINDOW (lcIcLocN0)		&& Activate Key window
    _CUROBJ = OBJNUM(ladata[1])
  CASE laScrMode[2]
    ACTIVATE WINDOW gwcContrl1		&& Activate control panel window
    _CUROBJ = OBJNUM(pbCls)
  OTHERWISE
    ACTIVATE WINDOW (lcIcLocN1)		&& Activate Edit region window
    _CUROBJ = OBJNUM(ladata[12])
ENDCASE
*-- end of lpBackTab.


*----------------------------------------------------------
*-- Control screen section
*-- This section contains all functions and procedures used in 
*-- control showing in screen .
*----------------------------------------------------------
*!*************************************************************
*! Name      : lpShow
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/10/98
*! Purpose   : Handling the screen modes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfActBrow,lfRefresh
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************
PROCEDURE lpShow
*-- Remove previous data from Temp. Location file.
IF (laScrMode[1] OR laScrMode[2]) AND llMultiLoc AND llDoBrowFn AND FILE(gcWorkDir+lcWLocline+'.DBF') AND ;
   USED(lcWLocline) AND RECCOUNT(lcWLocline) != 0 
  lnAlias = SELECT(0)

  *-- Delete any existing data in the same ssession
  SELECT (lcWLocline)
  ZAP
  SELECT (lnAlias)
ENDIF  

*-- Create temporary file in Edit and Add modes. [begin]
IF (laScrMode[3] OR laScrMode[4]) AND llMultiLoc AND ;
   (!USED(lcWLocline) OR (RECCOUNT(lcWLocline) = 0))
  SELECT *,"S" AS cStatus             ;
  FROM WhsLoc                         ;
  WHERE cWareCode + cLocation +       ;
        Style + Color = laData[1] AND ;
        EMPTY(Style)                  ;
  INTO DBF (gcWorkDir+lcWLocline)
  INDEX ON cWareCode+cLocation+Style+Color TAG lcWLocline
ENDIF
*-- Create temporary file in Edit and Add modes. [end]

*-- single warehouse, no need to control panel <Select> and <Browse> buttons.
*-- also no need to navigation buttons.
IF !llMultiWH
  *-- Disable select and browse buttons in control panel [begin]
  laCtrStat[9]  = "DISABLE"                && Select button
  laCtrStat[10] = "DISABLE"                && Browse button

  SHOW GET pbbrws DISABLE
  SHOW GET PBSLCT DISABLE
  *-- Disable select and browse buttons in control panel [end]

  *-- Disable navigator buttons in control panel [begin]
  laCtrStat[1]  = "DISABLE"
  laCtrStat[2]  = "DISABLE"
  laCtrStat[3]  = "DISABLE" 
  laCtrStat[4]  = "DISABLE" 

  SHOW GET pbTOP  DISABLE
  SHOW GET PBNEXT DISABLE
  SHOW GET PBPRVS DISABLE
  SHOW GET PBBTM  DISABLE
  *-- Disable navigator buttons in control panel [end]
ENDIF

*-- Select & Add modes
IF llSelctAdd
  DO CASE
    CASE laScrMode[1]         && ----- < Select Mode > -----
      laData[13]    = gcContcode 

      *B602171  Default Country Description.
      laData[8]  = LOOKUP(SYCINT.CCONT_DESC,IIF(EMPTY(laData[13]),ALLTRIM(gcContCode),laData[13]),sycint.ccont_code,'CCONTCODE')
      
      *E300872,7 Give initial value to popup [begin]
      IF llSite
        *E300889,4 Add next two lines.
        DIMENSION laSites[1,2]
        *E301486,1 AMH 11/19/2000 Fill laSites and lnSite from codes file [Start]
        *laSites[1,1] = 'N/A'
        = gfwCodePop ( @laCodInfo, "CSITEID" ,"N" )
        *E301486,1 AMH [End]
        *E300889,4
        
        lnSite = 1
      ENDIF  

      *E300872,7 Give initial value to popup [endif]

    CASE laScrMode[4]         && ----- < Add    Mode > -----

      *E300889,4 If system have sites . [begin]
      *E300889,4 add next block
      IF llSite
        = lfCollSite()    && Collect data for array laSites
        lnSite = 1
      ENDIF
      *E300889,4 If system have sites . [end]

      IF gcAct_Appl = "IC"
        ladata[11] = .T.
        SHOW GET ladata[11]
      ELSE
        ladata[12] = .T.
        SHOW GET ladata[12]
      ENDIF
  ENDCASE
ENDIF   

DO CASE
  CASE laScrMode[2]        && ----- < View Mode > -----
    SELECT WareHous
    *E131400,1 NNA 04/12/2006 Add defaule location option to the location screen.[Start]
    IF ldefware
      llDefCode = .T.
    ELSE
      llDefCode = .F.
    ENDIF  
    *E131400,1 NNA [End]
    SCATTER FIELDS &lcScFields MEMO TO laData
    lcObjWind = IIF(llMultiLoc,lcIcLocN1,lcIcLocS1)  && Name of objects window.
    = lfRefresh(lcObjWind)
    
    IF llMultiLoc
      _CUROBJ = OBJNUM(ibTrap)
    ENDIF  

      *E300872,7 Change the site to its approparate value [begin]
      IF llSite
        *E300889,4 lnSite = INT(ASCAN(laSites,ladata[16]) / 2)

        *E300889,4 add next block instead of prvious line [begin]
        = lfCollSite()    && Collect data for array laSites
        lnHaveSite = ASCAN(laSites,ladata[16])
        IF lnHaveSite = 0
          DIMENSION laSites[1,2]
          *E301486,1 AMH 11/19/2000 Fill laSites and lnSite from codes file [Start]
          *laSites[1,1] = 'N/A'
          *lnSite = 1
          = gfwCodePop ( @laCodInfo, "CSITEID" ,"N" )
          *E301486,1 AMH [End]
        ELSE
          lnSite = IIF(lcSysType = 'P',1,ASUBSCRIPT(laSites,lnHaveSite,1))
        ENDIF
        *E300889,4 add next block instead of prvious line [end]

      ENDIF  
      *E300872,7 Change the site to its approparate value [end]

  CASE laScrMode[3]        && ----- < Edit Mode > -----
    _CUROBJ = OBJNUM(ladata[2])

    *E300889,4 add next block  [begin]
    *E300889,4 if system support sites and does have site for this location
    IF llSite AND laSites[1,1] = 'N/A'
      = lfCollSite()    && Collect data for array laSites
      lnSite = 1
    ENDIF
    *E300889,4 add next block  [end]
  
    *E131400,1 NNA 04/12/2006 (BEGIN) if there is only one location then this will be the default location
    IF !llSelctAdd
      SHOW GET llDefCode DISABLE
    ENDIF
    *E131400,1 NNA (END)


ENDCASE

lcAddHed1  = PROPER(LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[13]),ALLTRIM(gcContCode),laData[13]),sycint.ccont_code,'CCONTCODE'))
SHOW GET lcAddHed1

*-- If Multi Bins.
IF llMultiLoc

  *-- if it is not the first time you enter the screen.
  IF llDoBrowFn
    = lfActBrow()
  ELSE
    llDoBrowFn = !llDoBrowFn
  ENDIF  
  = lfWBrow()
ENDIF  
*-- end of lpShow.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/11/98
*! Purpose   : Delete Procedure.
*!           : Multi warehouse only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfInStyFab
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpDelScr
*!*************************************************************
PROCEDURE lpDelScr
PRIVATE lcAllocate
lcAllocate = ''

*-- if you find any style or fabric allocated to this warehouse.
IF lfInStyFab()
  *-- The following message is 
  *-- "One or more XXX is allocated to Location XXX, Cannot delete."
  *--                             < Ok > 
  = gfModalGen('TRM00284B42000','Dialog',;
               lcAllocate + "|" + ALLTRIM(ladata[1]))  
ELSE  && no style or fabric was allocated to this warehouse
  lnUsing = SELECT(0) 
  *-- Multi Location case
  IF llMultiLoc
    *-- delete Warehouse location line(S) .
    SELECT WhsLoc
    BLANK FOR (cWareCode+cLocation+Style+Color = ladata[1])
    DELETE FOR EMPTY(cWareCode+cLocation+Style+Color)
    
  ENDIF

  *-- delete scale from scale header file.
  SELECT WareHous
  BLANK
  DELETE
  SELECT(lnUsing)

  *-- if single warehouse and you delete it , [begin]
  *-- you Switch screen to select mode.
  IF !llMultiWH
    llSelctAdd  = .T.  && Serve when you select new warehouse and go to add mode.
  ENDIF  
  laScrMode = .F.
  laScrMode[1] = .T.


  SHOW GETS
  *-- if single warehouse and you delete it , [end]
ENDIF
*-- end of lpDelScr.

*!*************************************************************
*! Name      : lfInStyFab
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/02/98
*! Purpose   : Find location in stydye or fabdye files.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : .T. if find location else .F.
*!*************************************************************
*! Example   : = lfInStyFab()
*!*************************************************************
FUNCTION lfInStyFab
IF SEEK(ladata[1],'StyDye')
  lcAllocate = 'Style(s)'
ENDIF
*B602066,1 AMM Check if material module installed before seeking on FABDYE
*IF EMPTY(lcAllocate) AND SEEK(ladata[1],'FabDye')
IF llMatInst  .AND. EMPTY(lcAllocate) .AND. SEEK(ladata[1],'FabDye')
*B602066,1 AMM end
  lcAllocate = 'Fabric(s)'
ENDIF
RETURN (!EMPTY(lcAllocate))


*!*************************************************************
*! Name      : lpSavscr
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/12/98
*! Purpose   : Saving data from temporary file to master files
*!           : Multi Locations only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpSavscr
*!*************************************************************
PROCEDURE lpSavScr

*E131400,1 NNA 04/12/2006 Add defaule location option to the location screen.[Start]
PRIVATE lcOldAlis ,lnRecno 
lcOldAlis=ALIAS()
    SELECT WAREHOUS
    lnRecno=RECNO()
LOCATE FOR ldefware
IF !FOUND() AND llDefCode = .F.
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Can not save. A default location should be assigned')
    llCSave = .F.
    KEYBOARD "{ALT+B}"
    SELECT (lcOldAlis)
    *B607919,1 TMI [Start] 
    *IF RECNO()<>lnRecno
    IF BETWEEN(lnRecno,1,RECCOUNT())
      *B607919,1 TMI [End  ] 
      GOTO lnRecno
    ENDIF  
    RETURN
ENDIF  
SELECT (lcOldAlis)
*B607919,1 TMI [Start] 
*IF RECNO()<>lnRecno
IF BETWEEN(lnRecno,1,RECCOUNT())
  *B607919,1 TMI [End  ] 
  GOTO lnRecno
ENDIF  
*E131400,1 NNA [End]
*E300889,1 (Start) Check if no other location for the same site found.
IF laScrMode[4] AND llSite
  IF lcSysType = 'P' OR (lcSysType = 'B' AND laSites[lnSite,2] <> gcCurSite)
    SELECT WAREHOUS
    LOCATE FOR cSiteId = laSites[lnSite,2] && gcCurSite
    IF FOUND()
      *-This site allready has location {cWareCode},
      *-Please remove this location first before adding a new 
      *-location, since the one site must has only one location.
      *-                 <OK>
      = gfModalGen("TRM42143B42000","DIALOG",WAREHOUS.cWareCode)
      RETURN 
    ENDIF
  ENDIF
ENDIF
*E300889,1 (End).


*E300872,7  Fill site with approparate site. [begin]
*E301486,1 AMH Fill laData[16] from laSites in case of system type point of sale [Start]
IF llSite
  laData[16] = laSites[lnSite,2]
ELSE
  IF lcSysType = 'P'
    laData[16] = IIF(EMPTY(laData[16]),PADR(gfGetMemVar('M_CURRSITE'),6),laData[16])
  ENDIF
ENDIF
*E301486,1 AMH [End]
*E300872,7  Fill site with approparate site. [end]

PRIVATE lnAlias
lnAlias = SELECT(0)

IF llMultiLoc
  SELECT (lcWLocline)
  DELETE FOR EMPTY(cLocation)
ENDIF  

*-- If you are in add mode.
IF laScrMode[4]
  *-- Save data to master warehouse file.
  *-- Note : INSERT SQL is the faster way to add new line.

  *N123070,1 NNA 07/14/2004 (End) Save the new Field in the Table
  *INSERT INTO ('WareHous') (&lcScFields) ; 
    Values(ladata[1],ladata[2],ladata[3],ladata[4],ladata[5]  ,;
           ladata[6],ladata[7],ladata[8],ladata[9],ladata[10] ,;
           ladata[11],ladata[12],ladata[13],ladata[14],ladata[15],ladata[16])
    *E131400,1 NNA 04/12/2006 (Begin) Update default location field in the Warehous file
    *INSERT INTO ('WareHous') (&lcScFields) ; 
    Values(ladata[1],ladata[2],ladata[3],ladata[4],ladata[5]  ,;
           ladata[6],ladata[7],ladata[8],ladata[9],ladata[10] ,;
           ladata[11],ladata[12],ladata[13],ladata[14],ladata[15],ladata[16],ladata[17])
  INSERT INTO ('WareHous') (&lcScFields) ; 
    Values(ladata[1],ladata[2],ladata[3],ladata[4],ladata[5]  ,;
           ladata[6],ladata[7],ladata[8],ladata[9],ladata[10] ,;
           ladata[11],ladata[12],ladata[13],ladata[14],ladata[15],ladata[16],ladata[17],ladata[18])
  *E131400,1 NNA (End)
  *N123070,1 NNA (End)
  
  *E300872,7 Update Trace File for warehous file, add record [begin]
  *E300872,7 Full index expr. : cWareCode(6).  
  = gfTraceKey('WAREHOUS',PADR(ladata[1],6),'A')
  *E300872,7 Update Trace File for warehous file, add record [end]

  *-- if you have multi locations.
  IF llMultiLoc
    SELECT WhsLoc
    APPEND FROM (gcWorkDir+lcWLocline) FIELDS cWareCode,cLocation

    *C037816,1 MHM 04/06/2004 Save Bin location custom field for DL[Start]
    IF ASCAN(laEvntTrig,PADR("DLSAVCS",10)) <> 0
      =gfDoTriger("ICLOCN",PADR("DLSAVCS",10))
    ENDIF
    *C037816,1 MHM [End]

    *E300872,7 Update Trace File for WhsLoc file, add record [begin]
    *E300872,7 Full index expr. : CWARECODE(6)+CLOCATION(10)+STYLE(19)+COLOR(6).
    *E300872,7 I pass the warecode only to trace all records.
    = gfTraceKey('WHSLOC',PADR(ladata[1],6),'A')
    *E300872,7 Update Trace File for WhsLoc file, add record [end]

  ENDIF  
  *-- Go to view mode if you are in single warehouse company. [begin]
  *-- if we are single warehouse case we go to add mode only if we does not 
  *-- have data in warehouse file otherwise we go to edit mode
  *-- and for so after we save first record we must switch mode to
  *-- view mode instead of select mode because we sure that we have 
  *-- one warehouse in the file.
  llSelctAdd  = llMultiWH
  IF !llMultiWH
    laScrMode      = .F.
    laScrMode[3]   = .T.  && The system reconize that we are in edit mode not add mode.
  ENDIF  
  *-- Go to view mode if you are in single warehouse company. [end]

ELSE   && Edit mode.
  *-- Save data in master warehouse file.
  SELECT WareHous

  *E131400,1 NNA 04/12/2006 (BEGIN) Get the correct Record
  =SEEK(laData[1])
  *E131400,1 NNA (End)

  GATHER FIELDS &lcScFields MEMO FROM laData

  *E300872,7 Update Trace File for warehous file, edit record [begin]
  *E300872,7 Full index expr. : cWareCode(6).  
  = gfTraceKey('WAREHOUS',PADR(ladata[1],6),'M')
  *E300872,7 Update Trace File for warehous file, edit record [end]

  *-- if you have multi locations.
  IF llMultiLoc
    
    SELECT (lcWLocline)
    SCAN FOR cWareCode+cLocation+Style+Color = ladata[1] AND ;
             INLIST(cStatus,'A','D')    && There is no modify location.
      *-- If you add new location.
      IF cStatus = 'A'  
        INSERT INTO ('WhsLoc')                     ;
                (cWareCode,cLocation             ) ; 
          Values(ladata[1],&lcWLocline..cLocation)

        *E300872,7 Update Trace File for WhsLoc file, add record [begin]
        *E300872,7 Full index expr. : CWARECODE(6)+CLOCATION(10)+STYLE(19)+COLOR(6).
        = gfTraceKey('WHSLOC',PADR(ladata[1],6) + ;
                              PADR(&lcWLocline..cLocation,10) + ;
                              PADR(WHSLOC.STYLE,19) + ;
                              PADR(WHSLOC.COLOR,6),'A')
        *E300872,7 Update Trace File for WhsLoc file, add record [end]
        
      ELSE  && If you delete existing location.
        *-- WHSLOC INDEX EXPR CWARECODE(6) + CLOCATION(10) + STYLE(19)+COLOR(6)
        SELECT WhsLoc
        = SEEK(ladata[1] + &lcWLocline..cLocation,'WhsLoc')

        *E300872,7 Update Trace File for WhsLoc file, delete record [begin]
        *E300872,7 Full index expr. : CWARECODE(6)+CLOCATION(10)+STYLE(19)+COLOR(6).
        = gfTraceKey('WHSLOC',PADR(WhsLoc.cWareCode,6) + ;
                              PADR(WhsLoc.cLocation,10) + ;
                              PADR(WHSLOC.STYLE,19) + ;
                              PADR(WHSLOC.COLOR,6),'D')
        *E300872,7 Update Trace File for WhsLoc file, delete record [end]

        BLANK
        DELETE                 
      ENDIF
    ENDSCAN           
      *C037816,1 MHM 04/06/2004 Save Bin location custom field for DL[Start]
      IF ASCAN(laEvntTrig,PADR("DLSAVCS",10)) <> 0
        =gfDoTriger("ICLOCN",PADR("DLSAVCS",10))
      ENDIF
      *C037816,1 MHM [End]
  ENDIF
ENDIF
*E301486,1 AMH 11/19/2000  Send Location information to other sites
IF 'NC' $ gcComp_Mdl AND lcSysType <> 'M' AND Warehous.cSiteId = gcCurSite
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD'   ,gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
  
  *------------------------------------------------------------
  *E301557,1 Saving a record for the current saved location in the EDITrans Table (Start)
  *SELECT EDIACPRT
  *SCAN FOR lInterComp AND SEEK(cPartCode+'816','EDIPD')
  *  SELECT EdiTrans
  *  IF !SEEK('816'+PADR(PADL(EdiPd.nTranSeq,6,'0'),20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
  *    INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,LINTERCOMP) VALUES ;
  *              ('816',PADL(EdiPd.nTranSeq,6,'0'),EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
  *  ENDIF
  *  REPLACE cStatus WITH 'N'
  *  =gfAdd_Info('EDITRANS')
  *ENDSCAN
  
  SELECT EDIAcPrt
  SCAN
    IF lInterComp AND SEEK(EDIAcPrt.cPartCode+'816','EDIPD')
      SELECT EdiTrans
      *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
      *IF !SEEK('816'+PADR('W'+laData[1],20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
      IF !SEEK('816'+PADR('W'+laData[1],40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
      *E037853,1 [End]
        INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,lInterComp) ;
        VALUES                 ('816','W'+laData[1],EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
      ENDIF  
      REPLACE cStatus WITH 'N'
      =gfAdd_Info('EDITRANS')
    ENDIF
  ENDSCAN
  *E301557,1 (End)
  *------------------------------------------------------------
  
ENDIF
*E301486,1 (End)

SELECT (lnAlias)  
*-- end of lpSavscr.


*----------------------------------------------------------
*-- Validation section
*-- This section contains all Valid functions and procedures help it.
*----------------------------------------------------------
*!*************************************************************
*! Name      : lfvWareHs
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/10/98
*! Purpose   : This function is to valid Warehouse code in WareHous File.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvWareHs()
*!*************************************************************
FUNCTION lfvWareHs
IF MDOWN() AND ( LASTKEY() # 13 )
  RETURN
ENDIF  

PRIVATE llToBrow,llAddMode
STORE .F. TO llToBrow,llAddMode

lnAlias = SELECT(0)
SELECT WareHous

*-- If come from BitMap button or the key is not empty.
IF llBrowse OR !EMPTY(laData[1]) 
  IF llBrowse OR ATC('?',laData[1]) != 0
    llBrowse  = .F. 		                && Disable flag to its intial state   
    llToBrow = .T.
  ELSE
    IF !SEEK(laData[1])
      *-- This message is "Warehouse XX not found in data file "
      *--       <  Add  > < Browse > < Reenter >
      lnSelect = gfModalGen('QRM00001B42003','Dialog',"Location "+": "+laData[1])  
      DO CASE
        CASE lnSelect = 1		&&Add
          llAddMode = .T.
        CASE lnSelect = 2 		&&Browse
          llBrowse  = .F. 		                && Disable flag to its intial state   
          llToBrow = .T.
        CASE lnSelect = 3		&&Reenter
          laData[1] = ''
      ENDCASE
    ENDIF
  ENDIF  
ELSE  && you did not enter any thing to validate.
  RETURN
ENDIF							        && Ending seek condition

IF llToBrow
  DIMENSION laTemp[6]
  STORE '' TO laTemp
  *-- Browse warehous file.
  =ariabrow('','Locations', gnbrfsrow1, gnbrfscol1,;
            gnbrfsrow2, gnbrfscol2, '','',;
            "cWareCode ,cDesc , uPs,cCont_Code",'laTemp')

  IF !EMPTY(laTemp[1])
    =ACOPY(laTemp,laData) 
  ELSE
    laData[1] = ''
  ENDIF       
ENDIF

IF !EMPTY(laData[1])
  laScrMode    = .F.
  *-- IF add mode
  IF llAddMode
    laScrMode[4] = .T.
  ELSE      && ELSE it is view mode
    laScrMode[2] = .T.
  ENDIF  
  SHOW GETS
  
ELSE     && ELSE If ESCAPE pressed wait in your position
  _CUROBJ = _CUROBJ
ENDIF
SELECT(lnAlias)
*-- End of lfvWareHs.

*!*************************************************************
*! Name      : lfvStates 
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/09/98
*! Purpose   : Validate the state code
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStates()
*!*************************************************************
FUNCTION lfvStates
PRIVATE lcFile_Ttl, lcBrfields, lcStaCode

lcStaCode = PROPER(LOOKUP(SYCINT.CPARt4LAB,IIF(EMPTY(laData[13]),ALLTRIM(gcContCode),ladata[13]),sycint.ccont_code,'CCONTCODE'))

*-- if you select country code for USA or for CANADA.
IF ALLTRIM(ladata[13])='USA' OR ALLTRIM(ladata[13])='CANADA'

  *E301077,21 Open codes file first time you go to this function.
  IF !llOpenCode AND !USED('CODES')
    llOpenCode = gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
  ENDIF

  *-- you did not found state code in codes file.
  IF !EMPTY(laData[6])  
    SET ORDER TO CODES IN CODES

    *E301077,72 Seek use 'N' instead of Comp_Id [Begin]
    *IF !SEEK(gcAct_Comp+PADR(laData[6],6)+'N'+'STATE','CODES')
    IF !SEEK('N'+PADR(laData[6],6)+'N'+'STATE','CODES')
    *E301077,72 Seek use 'N' instead of Comp_Id [End  ]
    
       lnCurAlias = SELECT(0)
       *-- browse all country codes [begin]
       SELECT CODES
       DECLARE laCodeFld[2]
       lcFile_Ttl = ALLTRIM(lcStaCode) + ' Codes'
       lcBrfields = 'cCode_No :H=ALLTRIM(lcStaCode)+" Code",cDiscrep:H="Description"'
       
      *E301077,72 Browse condition use 'N' instead of Comp_Id [Begin]
       *IF gfBrows('FOR ccomp_id+cfld_name+ccode_no+cdiscrep = ;
       *  gcAct_Comp+"STATE" AND ;
       *  cRltField="N"','cCode_No,cDiscrep','laCodeFld')

       *B604104,1 BWA 10/30/2001 Fix the bug of the seek is not working in the state screen.[START]
       *IF gfBrows('FOR cDefCode+cfld_name+ccode_no+cdiscrep = ;
         "NSTATE" AND cRltField="N"','cCode_No','laCodeFld')

       IF gfBrows(' "N" FOR cDefCode+cfld_name+ccode_no+cdiscrep = ;
         "NSTATE" AND cRltField="N"','cCode_No','laCodeFld')
       *B604104,1 BWA 10/30/2001.[END]

      *E301077,72 Browse condition use 'N' instead of Comp_Id [End  ]

         laData[6] = laCodeFld[1]
       ENDIF
       *-- browse all country codes [end]
       SELECT (lnCurAlias)
    ENDIF
  ENDIF    
ENDIF
*-- end of lfvStates.

*!*************************************************************
*! Name      : lfvStyInv
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/11/98
*! Purpose   : Validate the Finished Goods inventory check box.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStyInv()
*!*************************************************************
FUNCTION lfvStyInv
IF laScrMode[3] AND lcOldValue AND !ladata[11] AND SEEK(ladata[1],'StyDye')
  *-- The following message is 
  *-- "One or more Style(s) is allocated to Location XXX, Cannot Update."
  *--                             < Ok > 
  = gfModalGen('TRM42140B42000','Dialog',;
               'Style(s)' + "|" + ALLTRIM(ladata[1]))  
  ladata[11] = .T.
  SHOW GET ladata[11]
ENDIF
*-- if you uncheck the two check boxes.
IF !ladata[11] AND !ladata[12]
  *-- The following message is 
  *-- "The location must be finish goods inventory or material inventory or both."
  *--                           < Ok > 
  = gfModalGen('TRM00286B42000','Dialog')  
  ladata[11] = .T.
  SHOW GET ladata[11]
  _CUROBJ = _CUROBJ
ENDIF
*-- end of lfvStyInv.

*!*************************************************************
*! Name      : lfvMatInv
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/11/98
*! Purpose   : Validate the Material inventory check box.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvMatInv()
*!*************************************************************
FUNCTION lfvMatInv
IF laScrMode[3] AND lcOldValue AND !ladata[12] AND SEEK(ladata[1],'FabDye')
  *-- The following message is 
  *-- "One or more Fabric(s) is allocated to Location XXX, Cannot Update."
  *--                             < Ok > 
  = gfModalGen('TRM42140B42000','Dialog',;
               'Fabric(s)' + "|" + ALLTRIM(ladata[1]))  
  ladata[12] = .T.
  SHOW GET ladata[12]
ENDIF

*-- if you uncheck the two check boxes.
IF !ladata[11] AND !ladata[12]
  *-- The following message is 
  *-- "The location must be finish goods inventory or material inventory or both."
  *--                           < Ok > 
  = gfModalGen('TRM00286B42000','Dialog')  
  ladata[12] = .T.
  SHOW GET ladata[12]
  _CUROBJ = _CUROBJ
ENDIF
*-- end of lfvMatInv.

*!*************************************************************
*! Name      : lfvNew
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/11/98
*! Purpose   : This is a valid function for <New> button.
*!           : if there is no empty records it add one.  
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen,lfWBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNew()
*!*************************************************************
FUNCTION lfvNew
*-- if you did not find any empty location, thus you add new line.
IF !SEEK(ladata[1]+'          ',lcWLocline)
  *-- Add New line to Temporary location file.
  SELECT (lcWLocline)
  APPEND BLANK
  REPLACE cWareCode  WITH ladata[1] ;
          cStatus    WITH "A"
  *C132754,1 TMI [Start] Add a trigger to update the CREPLENISH field for GPS00
  IF ASCAN(laEvntTrig,PADR("UPRPLNSH",10)) <> 0
    =gfDoTriger("ICLOCN",PADR("UPRPLNSH",10))
  ENDIF
  *C132754,1 TMI [End  ]           
  = lfWBrow()
ENDIF
KEYBOARD '{ALT+B}' CLEAR PLAIN

*_CUROBJ = OBJNUM(ibTrap)
*-- end of lfvNew.

*!*************************************************************
*! Name      : lfvBin
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/11/98
*! Purpose   : valid function for location Field in browse.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBin()
*!*************************************************************
FUNCTION lfvBin
*-- return if you don't enter any thing.
IF EMPTY(&lcWLocline..cLocation)
  RETURN
ENDIF

lcLocId = &lcWLocline..cLocation
REPLACE &lcWLocline..cLocation WITH '          '

*-- if you find This bin before. 
IF SEEK(ladata[1]+lcLocId,lcWLocline)
  *-- The following message is 
  *-- "Bin XXX already exists."
  *--         < Ok > 
  = gfModalGen('TRM00285B42000','Dialog',ALLTRIM(lcLocId))  
  = SEEK(ladata[1]+'          ',lcWLocline)
ELSE		&& you didn't find this Bin before.
  = SEEK(ladata[1]+'          ',lcWLocline)
  REPLACE &lcWLocline..cLocation WITH lcLocId

  ACTIVATE WINDOW (lcIcLocN3)
  _CUROBJ = OBJNUM(ibTrap)

  ON KEY LABEL TAB      DO lpTab		&& Refersh trapping
  ON KEY LABEL BACKTAB  DO lpBackTab	&& Refersh trapping
ENDIF
*-- end of lfvBin.

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/11/98
*! Purpose   : valid function for <Remove> button.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen,lfRemvLine,lfInStyFab
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRemove()
*!*************************************************************
FUNCTION lfvRemove
PRIVATE lcAllocate
lcAllocate = ''

*-- if edit mode and you find any style or fabric allocated to this warehouse.
IF !EMPTY(&lcWLocline..cLocation) AND laScrMode[3] AND lfInStyFab()
  lnSelected = SELECT(0)
  SELECT WhsLoc
  LOCATE FOR cWareCode+cLocation+Style+Color =    ;
             ladata[1]+&lcWLocline..cLocation AND ;
             !EMPTY(Style)
  IF FOUND()
    *-- The following message is 
    *-- "One or more XXX is allocated to Location XXX Bin XXX, Cannot delete."
    *--                             < Ok > 
    = gfModalGen('TRM00284B42000','Dialog',;
                  lcAllocate + "|" + ALLTRIM(ladata[1]) + " Bin " +;
                  ALLTRIM(&lcWLocline..cLocation))  
  ELSE
    = lfRemvLine()
  ENDIF
  SELECT (lnSelected)
ELSE
  = lfRemvLine()
ENDIF
_CUROBJ = OBJNUM(ibTrap)
*-- end of lfvRemove.

*!*************************************************************
*! Name      : lfRemvLine
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/11/98
*! Purpose   : Remove current line in temporary warehouse location file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen,lfRemvLine,lfWBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRemove()
*!*************************************************************
FUNCTION lfRemvLine
*-- 'Are you sure you want to delete Bin XXX.'
*--               < Yes >   < No >
IF gfModalGen('QRM00287B42002','Dialog',ALLTRIM(&lcWLocline..cLocation)) = 1
  lnSelected = SELECT(0)
  SELECT (lcWLocline)
  IF cStatus = "A"
    DELETE
  ELSE
    REPLACE cStatus WITH "D"
  ENDIF
  SKIP
  = lfWBrow()
  SELECT (lnSelected)
ENDIF  
*-- end of lfRemvLine.

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Mohamed Badran (MAB)
*! Date      : 04/30/98
*! Purpose   : To save the old value of an object
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfOldValue()
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
*-- end of lfOldValue

*!*************************************************************
*! Name      : lfCollSite
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/21/98
*! Purpose   : Collect Sites in add and edit modes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCollSite()
*!*************************************************************
*E300889,4
*
FUNCTION lfCollSite
*E301486,1 AMH 11/19/2000 Fill laSites and lnSite from codes file [Start]
*PRIVATE lcSiteId
*lcSiteId = IIF(EMPTY(ladata[16]),gcCurSite,ladata[16])
*-- Collect data for array laSites
*SELECT cSiteDesc , cSiteId ;
 FROM sycsites             ;
 WHERE IIF(lcSysType = 'P', cSiteId = lcSiteId , .T.) ;
 ORDER BY cSiteId          ;
 INTO ARRAY laSites
= gfwCodePop ( @laCodInfo, "CSITEID" ,"L" )
*E301486,1 AMH [End]
*-- end of lfCollSite.

*!*************************************************************
*! Name      : lfv3PlProv              *N123070,1 
*! Developer : NADER NABIL (NNA)
*! Date      : 07/14/2004
*! Purpose   : This function is to valid 3PL Provider code in WareHous File.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfv3PlProv()
*!*************************************************************
FUNCTION lfv3PlProv
IF MDOWN() AND ( LASTKEY() # 13 )
  RETURN
ENDIF  

PRIVATE llToBrow,lnAlias ,lnRecNo
STORE .F. TO llToBrow

lnAlias = SELECT(0)
SELECT WareHous
lnRecNo = RECNO()
LOCATE
*-- If come from BitMap button or the key is not empty.
IF llBrowse OR !EMPTY(laData[17]) 
  IF llBrowse OR ATC('?',laData[17]) != 0
    llBrowse  = .F. 		                && Disable flag to its intial state   
    llToBrow = .T.
  ELSE
    IF !SEEK(ALLTRIM(laData[17]))
      llBrowse  = .F. 		                && Disable flag to its intial state   
      llToBrow = .T.
    ENDIF
  ENDIF  
ELSE  && you did not enter any thing to validate.
  RETURN
ENDIF							        && Ending seek condition
IF llToBrow
  DIMENSION laThPrTmp[6]
  STORE '' TO laTemp
  *-- Browse warehous file.
  =AriaBrow('','Locations', gnbrfsrow1, gnbrfscol1,;
            gnbrfsrow2, gnbrfscol2, '','',;
            "cWareCode ,cDesc , uPs,cCont_Code",'laThPrTmp')

  IF !EMPTY(laThPrTmp[1])
    laData[17] = laThPrTmp[1]
  ELSE
    laData[17] = lcOldValue
    _CUROBJ = _CUROBJ
  ENDIF       
ENDIF

IF !EMPTY(laData[17])
  SHOW GET laData[17]
ENDIF
*-- return to the Record that hold the Base Location
IF !laScrMode[4]
  *B607919,1 TMI [Start] 
  IF BETWEEN(lnRecno,1,RECCOUNT('WAREHOUS'))
    *B607919,1 TMI [End  ] 
    GOTO lnRecNo IN WAREHOUS
    *B607919,1 TMI [Start] 
  ENDIF
  *B607919,1 TMI [End  ] 
ENDIF
SELECT(lnAlias)
*-- End of lfv3PlProv.

*!*************************************************************
*! Name      : lfvDefa              *E131400,1 
*! Developer : NADER NABIL (NNA)
*! Date      : 04/12/2006
*! Purpose   : valid default location in WareHous File.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDefa()
*!*************************************************************
FUNCTION lfvDefa
PRIVATE lcOldAlis ,lnRecno ,lcOldWareHous
lcOldAlis=ALIAS()
SELECT WAREHOUS
lnRecno=RECNO()
lcOldWareHous = cwarecode
LOCATE FOR ldefware
IF FOUND()
  IF llDefCode
    IF gfModalGen('QRM00000B00006','ALERT','','',cwarecode+'is your default location. Do you want to change it?') = 2
      ladata[18] = 'N'
      llDefCode = .F.
      SHOW GET llDefCode
    ELSE
      Replace all ldefware with .F.
      ladata[18] = .T.
    ENDIF
  ELSE
    IF lcOldWareHous = cwarecode
      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'A default location should be assigned')
      SELECT (lcOldAlis)
      *B607919,1 TMI [Start] 
      *IF RECNO()<>lnRecno
      IF BETWEEN(lnRecno,1,RECCOUNT())
        *B607919,1 TMI [End  ] 
        GOTO lnRecno
      ENDIF  
      llDefCode = .T.
      SHOW GET llDefCode
      RETURN
     ENDIF
    ladata[18] = .F.
  ENDIF
ELSE
  =SEEK(laData[1])
  IF llDefCode
      ladata[18] = .T.
  ELSE
    ladata[18] = .F.
  ENDIF
ENDIF   
SELECT (lcOldAlis)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
*-- End of lfvDefa.
