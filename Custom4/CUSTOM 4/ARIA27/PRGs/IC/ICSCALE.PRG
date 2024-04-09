*:***************************************************************************
*: Program file  : icscale
*: Program desc. : Scale Maintenance
*: For screen    : icscale,icscauto,icscsing
*:        System : Aria Advantage Series.
*:        Module : Inventory Control (IC).
*:     Developer : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:     Procedures : lpTab,lpTab2,lpTab3,lpBackTab,lpBackTab2,lpBackTab3     ,
*:                : lpShow,lpZooming,lpSavscr,lpDontSave,lpAddSz,lpReplace  ,
*:                : lpAddPp,lpRepPps,lpEdCode,lpDelScr,lpChanges,lpValidSz  ,
*:                : lpChangWin,lpAutEsc,lpWizTab,lpShWizTab,lpNextStat,lpActFold.
*:     Functions  : gfGetMemVar,gfSetup,gfTempName,gfStopBrow,gfModalGen    ,
*:                : lfVarBrow1,lfActBrow1,lfActBrow2,lfActBrow3,lfBrow3Fds  ,
*:                : lfVBrow1,lfVBrow2,lfVBrow3,lfwBrow1,lfwBrow2,lfwBrow3   ,
*:                : lfOldPp,lfvPpQty,lfTrap,lfUnTrap,lfActFolder,lfAddState ,
*:                : lfPrepBtn,lfCoding,lfCalCnt,lfEqCode,lfCalVal,lfvScale  ,
*:                : lfvDims,lfActMore,lfvPosDim1,lfvMore,lfvScaleS,lfvSclDes,
*:                : lfvNew,lfvRemove,lfSearchSy,lfvGetDim,lfvSize,lfDelSz   ,
*:                : lfInStyle,lfCapPosSz,lfvNRF,lfvNewP,lfPpLnAdd,lfPpLnBlnk,
*:                : lfvRemoveP,lfvPrePak,lfvAuto,lfvNext,lfvBack,lfAutoBrow ,
*:                : lfWizTrap,lfWizUnTp,lfWizard,lfIsValid,lfvMove          ,
*:                : lfvMoveAll,lfvRemvWiz,lfvRemAll,lfvCancel,lfvFinish        ,
*:                : lfCalValue,lfRefExtD,lfRefSinD,lfClsData,lfMoreStat     ,
*:                : lfMoreEnab,lfCapPosBw,lfoldvalue,lfAddLineP,lfCalTotPp  ,
*:                : lfBlankEnd,lfCollectP,lfCollectS,lfRepSizes,lfRefresh.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO icscale
*:***************************************************************************
*: Note that : i.   Any logical expression evaluate its value from left to right
*:           :      and if the left part is satisfied it finished the expresion
*:           :      regardless the value of right part.  
*:  examples :      1- (A AND B) if A is .F. the condition is .F. without completing it.
*:           :      2- (A OR  B) if A is .T. the condition is .T. without completing it.
*:           : ii.  In this program we have 5 screen modes not the normal 4
*:           :      because I add new mode called SWITCH mode, making it using 
*:           :      llSwMode flag, and this mode is like edit mode in every thing
*:           :      except in Saving procedure it is like add mode, and I use this 
*:           :      technique in extended scale only to have wide gaps in some cases
*:           :      cases descriped in the declaration of llSwMode. 
*:           : iii. In single size scale , and deal with prepaks, we don't deal
*:           :      deal with parent prepak file. 
*:***************************************************************************
*E300872,6 MAB 04/30/1998  Adjust Program for Communication process.
*E301209,1 MAB 04/25/99 Add new setup parameters ask use for width of 
*                       extended size scale (Either 1 or 2 Characters).
*B603489,1 AMH 08/08/2000  Fixing 'End of file encountered' when 
*B603489,1                 Enabling the More Button.
*B603152,1 AMH 08/30/2000  Fixing Size sescription doesn't changed
*B604123,1 KAM 18/02/2001  enabling more button if use not fill all sizes
*B802855,1 KAM 15/02/2001 Update all styles with new set sizes[start]
*B604430,1 WAB 05/30/2001 fixing bug Error "variable not found" when adding a new Scale  
*B606943,1 KHM 02/16/2003 fix the bug of deleting a scale the is included in a style
*B606943,1                in case of extended size scale.
*B037350,1 MHM 02/09/2004 Adding a new size to a size scale not creating style records
*C200597,1 TMI 08/19/2004 Call a customized tracekey function for Bong Wha
*B038431,1 MHM 9/13/2004 Fix the bug of wrong message in saving EX size scale
*:***************************************************************************

EXTERNAL ARRAY laData , laDefProc 
DECLARE lafoldwinds[2,2],laScrMode[4]

*-- Screen variables declaration [begin].
STORE ' ' TO lcWinCh1,lcWinC01,lcWinC12		&&Scale  folder single scale
STORE ' ' TO lcWinCh2,lcWinC22,lcWinC24		&&Prepak folder single scale
*-- Screen variables declaration [end].

STORE ' ' TO lcfolder,lcfoldprnt
STORE ' ' TO lcScaline			&& Hold name for temporary scale file.
STORE ' ' TO lcPrParent			&& Hold name for temporary parent prepak file.
STORE ' ' TO lcPrChild			&& Hold name for temporary child prepak file.	
STORE ' ' TO lcBrowTi3	        && Child prepak browse title
STORE ' ' TO lcScaleKey         && Extended code key.

*-- Push buttons variables.
STORE 'DISABLE'  TO lcNews,lcNew2,lcRemoves,lcRemove2,lcAutos,lcMore

*-- Coditional variables its value corressponding to case(single or extended)
*-- lcLocInCh  : Hold locate expression 
*-- lcSeekPrep : Hold seek function 
*-- lcSeekCond : Hold seek condition
STORE ' ' TO lcLocInCh,lcSeekPrep,lcSeekCond

*-- lcUsedFile : Hold name of current used file corressponding to screen mode.
*-- lcBrowExpr : Hold Expression for scale browse.
*-- lcInBrowSz : Hold Titles used in both browses and edit region for sizes.
*-- lcInBrowFd : Hold Title used in browse in extended scale 2 and 3 dimensions.
*-- lcMarker   : Hold first portion on any browse fields (cMarker > )
*-- lcBrflds   : Hold fields for used in browsing prepaks because it is to long for the
STORE ' ' TO lcUsedFile, lcBrowExpr,lcInBrowSz,lcInBrowFd,lcMarker,lcBrflds
STORE ' ' TO lcOldValue

*-- lnMarker   : Hold recno of lcScaline file.
*-- lnMarker2  : Hold recno of lcPrParent file.
*-- lnMarker3  : Hold recno of lcPrChild file.
*-- lnSize     : Refrence to change edit region for sizes title
*--            : (i.e:) When it's 1 title from Size1 ------> Size8
*--            :        When it's 2 title from Size9 ------> Size17
*--            : and so on.
STORE 1   TO lnActFolder,lnMarker,lnMarker2,lnMarker3,lnSize

*-- Hold old prepak quantity [begin]
*-- and this values used to validate the edited values is new or not 
*-- and thus we can use it to calculate total prepak and prevent doing 
*-- its code when it was not change.
STORE 0   TO lnOldPp1,lnOldPp2,lnOldPp3,lnOldPp4,;
             lnOldPp5,lnOldPp6,lnOldPp7,lnOldPp8
*-- Hold old prepak quantity [end]

STORE .F. TO glFromBrow,llBrowse,llNoShow,llAlowNew

*-- llShift    : Flag when it rised means that removing scale line was come
*--            : from deleting the size not from pushing <Remove> button.
*-- llActBrow1 : This flag is to prevent flashing done by twice doing of
*--            : lfactbrow1 (1St from .spr and 2Nd from lpshow), and after 
*--            : doing lpshow for the first time it becames .T. at all. 
*-- llNewPpLn  : This flag is rised when we add new line to scale file and 
*--            : find prepaks for current scale.
STORE .F. TO llShift,llActBrow1,llNewPpLn

*-- llSwMode   : When it's .T. means that in lpSavScr deal with edit mode like add mode
*--            : and this is very useful to create new codes with wide gaps.
*--            : and this flag is rised in extended scale edit mode
*--            : in the following cases:
*--            : 1- No records in lcScaline file.
*--            : 2- Delete all records from lcScaline file.
*--            : 3- Change dimensions.
*--            : 4- Using <Auto> button that changing structure of the file.
STORE .F. TO llSwMode

*-- llValidPre : When it's .T. you can TAB between the two prepak browses
*--            : thus we easely unrise it to make the browses editing 
*--            : simulate screen objects (1St browse is the key and 2Nd is 
*--            : the rest objects )
STORE .T. TO llValidPre

*-- Extended scale variables.
*-- laDim1     : Array hold values of the 3rd dimension popup.
*-- laPostion  : Array hold values of Sz1 to assign lnSize variable 
*--            : with its postion.
DIMENSION laDim1[1],laPostion[1]

*-- Screen variables declaration [begin].
STORE ' ' TO lcWinCh0		                &&Extended scale Header screen 
STORE ' ' TO lcWinC11,lcWinC13,lcWinC14	&&Extended scale folder 
STORE ' ' TO lcWinC21,lcWinC23			&&Extended scale Prepak folder
*-- Screen variables declaration [end].

*-- lcTempCnt : Cursor table have the 1st size for only current combination
*--           : and refreshed every time we change the combination.
*-- lcTempPos : Cursor table have the sizes and NRFS for only current 
*--           : combination to serve in evaluate the kind of shifting in lfDelSz().
STORE ' ' TO lcTempCnt,lcTempPos

*-- llSameDim  : Flag when it's .T. it means that you does not change any
*--            : thing in the screen.
*-- llNwAutoSt : Flag control the new and auto buttons if we change dimensions.
*-- llMoreClck : if user click More Buton.

STORE .F. TO llSameDim,llNwAutoSt
*,llMoreClck

*-- Single scale variables
STORE ' ' TO lcScale		&&Scale field in single scale 
*-- lcScRec    : Variable to adjust record pointer if we change screen mode 
*--            : in single scale (View TO Edit and vice versa)
STORE ' ' TO lcScRec

IF !gfSetup()
  RETURN
ENDIF

llExtSize  = gfGetMemVar('M_USEEXSSC')		&&When it is .T. it means that we are at extended size scales. 




lcBrowTi1  = 'Scales'		&& Scale Browse title

*************----1-----*----2-----*----3-----*----4-----*----5-----****
lcScFields ='cExtScale ,cScaleDes ,nNoOFDim  ,cDim1Desc ,cDim2Desc   ,'+;
            'cDim3Desc' 
***********************************************************************

DECLARE laKeyField[1,4]
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'CEXTSCALE'
laKeyField[1,4] = 1

*--Initialize the Folders array [begin]
lnFolderCEnd = 102.50
lnFolderREnd =   2.00
lnNoFld      =   2
lcwfoldchng  = '=lfActFolder()'  && function to control shows after change the folder
lcfoldpush   = 'pbFolders'       && push button name for the next folder
*--Initialize the Folders array [end]

laDefProc[9]  = .F.      && Disable the control panel global save proc. and (do lpSavScr)

IF llExtSize
  *B802855,1 KAM 15/02/2001 call function to get the scale position[start]
  STORE 0 TO lnExtScPos
  =lfGetSclPo()
  *B802855,1 KAM [end]
  *E301209,1 Define variable hold extended size scale picture [Begin]
  lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
  lcScalePic = REPLICATE('X',lnScaleLen)     && Extended size scale ID picture.
  lnExtndLen = 3 - lnScaleLen                && Extended Length.
  lcExtndPic = REPLICATE('X',lnExtndLen)     && Extended picture.
  lcExKeySta = 'DISABLE'                     && Extended Code Object State (Enable/Disable)
  *E301209,1 Define variable hold extended size scale picture [End  ]
     


  lcBrowTi2  = 'Prepacks' && Parent prepak Browse title(extended scale only)
  laDefProc[7]  = .F.    && Disable the control panel global delete proc. and (do lpDelScr)
  *-- llNewComb  : Flag used to fill lcTempCnt cursor in every new combination.
  STORE .T. TO llNewComb

ELSE  && Single size scale

  *E301209,1 length of scale is one character is single size company. [Begin]
  lnScaleLen = 1
  *E301209,1 length of scale is one character is single size company. [End  ]

  *-- IF we are in single scales and the header file is empty we must have 
  *-- a dummy record in it.
  IF RECCOUNT('ScaleHd') = 0
    SELECT ScaleHd
    APPEND BLANK

    *E300872,6 Update Trace File for SCALEHD file, add record [begin]
    *E300872,6 Full index expr. : cExtScale(1).  
    = gfTraceKey('SCALEHD',SPACE(1),'A')
    *E300872,6 Update Trace File for SCALEHD file, add record [end]

  ENDIF  

ENDIF  

IF !WEXIST(gcBaseWind)
  *-- llAnother  : Flag when it rised means that user come from another screen
  llAnother = .F.     && First time you open this session.
  IF llExtSize
    lcBrowTi3  = 'Prepack Details'
  ELSE
    lcBrowTi3  = "Prepacks for scale : " + scale.scale
  ENDIF  
  
  STORE ' ' TO lcInBrowSz,lcInBrowFd

  SELECT ScaleHd		&&Master file in this screen. 
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK

  *--Program screen windows [begin]
  lcWinCh1    = gfTempName()
  lcWinC01    = gfTempName()
  lcWinC12    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinC22    = gfTempName()
  lcWinC24    = gfTempName()
  lcfolder    = gfTempName()        && Folder Window Name
  *--Program screen windows [end]

  *--Tables naming  [begin]
  lcScaline   = gfTempName()
  lcPrParent  = gfTempName()
  lcPrChild   = gfTempName()
  *--Tables naming  [end]

  *--Assign values to folder array and variables [begin]
  lcfoldprnt  = gcBaseWind          && window parent name for the folder
  lnActFolder = 1                   && active folder
  lafoldwinds[1,1] = 'Scale'
  lafoldwinds[1,2] = lcWinCh1
  lafoldwinds[2,1] = 'Prepacks'
  lafoldwinds[2,2] = lcWinCh2
  *--Assign values to folder array and variables [end]

  *-- Create temporary prepak files (lcPrParent and lcPrChild) [begin]
  SELECT                ;
    Prepak,ppTot,Scale  ;
  FROM SCALE            ;
  WHERE .F.             ;
  INTO TABLE (gcWorkDir+lcPrParent)

  IF llExtSize

    *E301209,1 Index now is On Prepak only and we filter to cStatus <> 'D' [Begin]
    *lcLocInCh = "LEFT(Scale,1)+Prepak+cDim1+cDim2 = ;
    *             ladata[1]+&lcPrParent..Prepak AND cStatus != 'D'"
    *INDEX ON LEFT(Scale,1)+Prepak TAG ParentTag

    lcLocInCh = "Prepak+cDim1+cDim2 = &lcPrParent..Prepak"
    INDEX ON Prepak TAG ParentTag
    *E301209,1 Index now is On Prepak only and we filter to cStatus <> 'D' [End  ]
    
  ELSE

    STORE SCALE.SCALE TO lcScale

    *E301209,1 Filter to cStatus <> 'D' [Begin]
    *lcLocInCh = "Scale+Prepak+cDim1+cDim2 = ;
    *             lcScale AND cStatus != 'D'"
    lcLocInCh = "Scale+Prepak+cDim1+cDim2 = lcScale"
    *E301209,1 Filter to cStatus <> 'D' [End  ]

    INDEX ON Scale+Prepak TAG ParentTag

  ENDIF 
   
  SELECT Prepak,Scale,        ;
    cDim1,cDim2,PpTot,        ;  
    pP1,pP2,pP3,pP4,          ;
    pP5,pP6,pP7,pP8,          ;
    Sz1,Sz2,Sz3,Sz4,          ;
    Sz5,Sz6,Sz7,Sz8,          ;
    9999999999 AS nScRec,     ; 
    "S" AS cStatus            ;
  FROM SCALE                  ;
  WHERE .F.                   ;
  INTO TABLE (gcWorkDir+lcPrChild)
  IF llExtSize
    INDEX ON nScRec TAG nScRec

    *E301209,1 Index is on full Scale [Begin]
    *INDEX ON LEFT(Scale,1)+Prepak+cDim1+cDim2 TAG ChildTag
    INDEX ON Scale+Prepak+cDim1+cDim2 TAG ChildTag
    *E301209,1 Index is on full Scale [End  ]

  ELSE
    INDEX ON Scale+Prepak+cDim1+cDim2 TAG ChildTag
  ENDIF 
  *-- Create temporary prepak files (lcPrParent and lcPrChild) [end]
  
  *E301209,1 Index on cStatus <> 'D' while file is empty is faster than browse on cond [Begin]
  SET FILTER TO cStatus <> 'D'
  *E301209,1 Index on cStatus <> 'D' while file is empty is faster than browse on cond [End  ]
  
  IF llExtSize

    *E301209,1 Seek prepak condition on child file was changed [Begin]
    *lcSeekPrep = "laData[1]"
    *E301209,1 Seek prepak condition on child file was changed [End  ]

    lcSeekPrep = "laData[1]+lcScaleKey"
    lcSeekCond = "SEEK(&lcSeekPrep+&lcPrParent..Prepak,lcPrParent)"

    laDim1    = ' ' 
    laPostion = ' '

    *--Program screen windows.
    lcWinCh0    = gfTempName()
    lcWinC11    = gfTempName()
    lcWinC13    = gfTempName()
    lcWinC14    = gfTempName()
    lcWinC21    = gfTempName()
    lcWinC23    = gfTempName()
    *--Program screen windows.

    lcTempCnt = gfTempName()
    lcTempPos = gfTempName()
  ELSE
    lcSeekPrep = "lcScale"
    lcSeekCond = "SEEK(&lcSeekPrep+&lcPrChild..Prepak,lcPrChild)"
    lcScRec = "S"

    *-- In single scales the modes is either View or Edit thus initially we
    *-- adjust the screen mode to be in View mode.
    laScrMode    = .F.
    laScrMode[2] = .T.
  ENDIF
ELSE
  llAnother = .T.       && You come from another session.
ENDIF


SELECT (lcBaseFile)
PUSH KEY

*--Activate Browses windows  [Begin]
*-- Define a menu bar with a short-cut key for each browse window 
*-- in the program just to be able to activate the browse window
*-- using the its menu bar short-cut key.
lcPrompt3 = IIF(llExtSize,lcBrowTi3,"Prepacks")
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcBrowTi1 KEY ALT+B SKIP FOR (lnActFolder#1)
DEFINE BAR 101 OF P01PU01 PROMPT lcPrompt3 KEY ALT+D SKIP FOR (lnActFolder#2)
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTi1)
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcBrowTi3)
IF llExtSize
  DEFINE BAR 102 OF P01PU01 PROMPT lcBrowTi2 KEY ALT+P SKIP FOR (lnActFolder#2)
  ON SELECTION BAR 102 OF P01PU01 ACTIVATE WINDOW (lcBrowTi2)
ENDIF  
*--Activate Browses windows  [End]

*-- if extended size scales do icscale screen program
IF llExtSize
  DO (gcScrDir+gcWinAppl+'\ICSCALE.SPR')
ELSE  && but if it's single do icscsing screen program
  DO (gcScrDir+gcWinAppl+'\ICSCSING.SPR')
ENDIF  

POP KEY

*-- Release defined browses windows [Begin]
RELEASE WINDOW (lcBrowTi1)
RELEASE WINDOW (lcBrowTi3)

*-- Release the menu bars.
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01
RELEASE BAR 101 OF P01PU01

IF llExtSize
  RELEASE WINDOW (lcBrowTi2)
  RELEASE BAR 102 OF P01PU01
ENDIF  
*-- Release defined browses windows [End]

*--Normal exit.
IF glQuitting

  IF FILE(gcWorkDir+lcScaline+'.DBF')
    USE IN (lcScaline)
    ERASE (gcWorkDir+lcScaline+'.DBF')
    ERASE (gcWorkDir+lcScaline+'.CDX')
  ENDIF  

  IF FILE(gcWorkDir+lcPrParent+'.DBF') AND USED(lcPrParent)
    USE IN (lcPrParent)
    ERASE (gcWorkDir+lcPrParent+'.DBF')
    ERASE (gcWorkDir+lcPrParent+'.CDX')
  ENDIF  

  IF FILE(gcWorkDir+lcPrChild+'.DBF') AND USED(lcPrChild)
    USE IN (lcPrChild)
    ERASE (gcWorkDir+lcPrChild+'.DBF')
    ERASE (gcWorkDir+lcPrChild+'.CDX')
  ENDIF  



ENDIF
*-End of program code...

*--------------------------------------------------------------
*-- Browse functions and trapping section. 
*-- This section contains all functions and procedures used in 
*-- browses either scales or prepaks. 
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lfVarBrow1
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : Prepair Variables used in scale browse.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : None.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVarBrow1()
*!*************************************************************
*! Note      : I separate it for some case we want changes 
*!           : to affect refresh browse only.
*!*************************************************************
FUNCTION lfVarBrow1
IF llExtSize

  *lcInBrowSz = PROPER(ALLTRIM(IIF(laData[3] = 3 ,;
                      ALLTRIM(laData[6]),;
                      IIF(laData[3] = 2 ,;
                      ALLTRIM(laData[5]),;
                      ALLTRIM(laData[4])))))
  lcInBrowSz = ALLTRIM(IIF(laData[3] = 3 ,;
               ALLTRIM(laData[6]),;
                      IIF(laData[3] = 2 ,;
                      ALLTRIM(laData[5]),;
                      ALLTRIM(laData[4]))))


  *lcInBrowFd = PROPER(ALLTRIM(IIF(laData[3] = 3 ,;
                      ALLTRIM(laData[5]),;
                      IIF(laData[3] = 2 ,;
                      ALLTRIM(laData[4]),;
                      " " ))))
  lcInBrowFd = ALLTRIM(IIF(laData[3] = 3 ,;
               ALLTRIM(laData[5]),;
                      IIF(laData[3] = 2 ,;
                      ALLTRIM(laData[4]),;
                      " " )))

  lcBrflds = "SZ1   :H= lcInBrowSz  :10:R," +;
             "SZ2   :H= lcInBrowSz  :10:R," +;
             "SZ3   :H= lcInBrowSz  :10:R," +;
             "SZ4   :H= lcInBrowSz  :10:R," +;
             "SZ5   :H= lcInBrowSz  :10:R," +;
             "SZ6   :H= lcInBrowSz  :10:R," +;
             "SZ7   :H= lcInBrowSz  :10:R," +;
             "SZ8   :H= lcInBrowSz  :10:R "

ELSE
  lcInBrowSz = "Size"
  lcInBrowFd = "Scale"
  lcBrflds = "SZ1   :H= lcInBrowSz+'1' :10:R," +;
             "SZ2   :H= lcInBrowSz+'2' :10:R," +;
             "SZ3   :H= lcInBrowSz+'3' :10:R," +;
             "SZ4   :H= lcInBrowSz+'4' :10:R," +;
             "SZ5   :H= lcInBrowSz+'5' :10:R," +;
             "SZ6   :H= lcInBrowSz+'6' :10:R," +;
             "SZ7   :H= lcInBrowSz+'7' :10:R," +;
             "SZ8   :H= lcInBrowSz+'8' :10:R "

ENDIF                  
*-- end of lfVarBrow1.

*!*************************************************************
*! Name      : lfActBrow1
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : Scales Browse function in both ext. and single cases... 
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfVarBrow1,lfRefExtD,lfActBrow3,
*!                        : lfwBrow1,lfvBrow1
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfActBrow1()
*!*************************************************************
FUNCTION lfActBrow1
IF !EMPTY(lcBrowTi1) AND WEXIST(lcBrowTi1) 
  RELEASE WINDOW (lcBrowTi1)
ENDIF

lcUsedFile = IIF((laScrMode[3] OR laScrMode[4]),lcScaline,'Scale')

*-- Because the following is to long to fit in one line we discret it.
lcBrowExpr = IIF(laScrMode[3] OR laScrMode[4],".T.",                ;
             IIF(laScrMode[1],".F.",                                ;
             IIF(llExtSize,"(Scale = laData[1]) AND (Type = 'S')",  ;
                           "Type = 'S'")))
lcBrowExpr = IIF(laData[3] = 3,lcBrowExpr +" AND cDim1 = laDim1[lnPosDim1]",;
                 lcBrowExpr)

*E301209,1 No need to add status expression because we filter on it [Begin]
*lcBrowExpr = IIF(laScrMode[3],lcBrowExpr +[ AND (cStatus != "D")],;
*                 lcBrowExpr)
*E301209,1 No need to add status expression because we filter on it [End  ]

*E301209,1 Add extended code field to browse [Begin]
*lcMarker = "cMarker = IIF(RECNO()=lnMarker ,'>',' '):H=' ':01:R :W =.F.,"
lcMarker = "cMarker = IIF(RECNO()=lnMarker ,'>',' '):H=' ':01:R :W =.F.," +;
           IIF(llExtSize,"cExtCode=RIGHT(Scale,lnExtndLen) :H='Ext. Scale' :R ,","")
*E301209,1 Add extended code field to browse [Begin]

= lfVarBrow1()		&&Call function that prepair variables used in browse. 

SELECT (lcUsedFile)
*-- Extended scale 
IF llExtSize
  IF laScrMode[2]		&& View mode
    *-- Adjust record pointer
    LOCATE FOR (Scale = laData[1]) AND (Type = 'S') AND ;
               (IIF(laData[3] = 3 ,cDim1 = laDim1[lnPosDim1],.T.))
    =lfRefExtD('')		&& Refresh gets with new record data.
  ENDIF  

  *-- Fields in browse function according to spinner value.
  DO CASE
    CASE laData[3] = 2	&&2 Dimension
      lcBrfld1 = lcMarker + "CDIM1 :H= lcInBrowFd:9:R," +lcBrflds
    CASE laData[3] = 3	&&3 Dimension
      lcBrfld1 = lcMarker + "CDIM2 :H= lcInBrowFd:9:R," +lcBrflds
    OTHERWISE			&&1 Dimension or you are at select mode(ladata[3]=0)
      lcBrfld1 = lcMarker + lcBrflds
  ENDCASE

  *-- Assigning window name for browse because if we are 1 or 2 dimension 
  *-- the browse window is zoomed in else it zoomed out,
  *-- we make this zooming technique with two different windows,
  *-- one for big window (lcWinC01) and the other is for
  *-- small window (lcWinC11) and the zooming case is crrossponding to 
  *-- value of the spinner(ladata[3]).
  lcBWiName = IIF((ladata[3] <= 2),lcWinC01,lcWinC11) 

ELSE		&&Single size scale
  lcBrfld1 = lcMarker + "Scale :H= lcInBrowFd:8:R," + ;
             "cScl_Desc :H= 'Description' : 15:R," + lcBrflds

  lcBWiName = lcWinC01
ENDIF
lnMarker = RECNO()		&&Save record pointer to draw > shape.

BROWSE FIELDS &lcBrfld1    ;
       WHEN lfwBrow1()     ;
       FOR &lcBrowExpr     ;
                  NOEDIT   ;
                  NOAPPEND ;
                  NOCLEAR  ;
                  NODELETE ;
                  NOMENU   ;
                  NOWAIT   ;
                  SAVE     ;
       FREEZE cMarker      ;
  	   TITLE lcBrowTi1     ;
       VALID :F lfvBrow1() ;         
	   WINDOW (lcBWiName)  ;
	   IN WINDOW (lcWinCh1)

*-- if single size scale we must adjust record pointer when switch from 
*-- screen mode to another.
IF !llExtSize
  IF laScrMode[2]
    = SEEK(lcScRec,'SCALE')
    IF EOF()
      lcScRec = "S"
      = SEEK(lcScRec,'SCALE')
    ENDIF
  ELSE  
    = SEEK(SUBSTR(lcScRec,2,LEN(lcScRec)),lcScaline)
    IF EOF()
      lcScRec = "S"
      GO TOP IN (lcScaline)
    ENDIF
  ENDIF
ENDIF
*-- end of lfActBrow1.

*!*************************************************************
*! Name      : lfVBrow1
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : Valid function for Browse scales in both ext. and single cases.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow,lfActBrow3
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVBrow1()
*!*************************************************************
FUNCTION lfVBrow1
IF WONTOP() # (lcBrowTi1)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

lnScAlias = SELECT(0)
IF llExtSize
  llNewComb = .T.		&& initiate combinations.
  IF USED(lcTempCnt)
    USE IN (lcTempCnt)
  ENDIF  
*ELSE		&&Single size scale
  *= lfActBrow3()	&& Refresh Child prepak browse when switch folders.
ENDIF
SELECT (lnScAlias)
*-- end of lfVBrow1


*!*************************************************************
*! Name      : lfwBrow1
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : When function for Browse scales in both ext. and single cases.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfCapPosBw,lfwBrow1,lfRefresh,lfRefExtD,lfRefSinD
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrow1()
*!*************************************************************
FUNCTION lfwBrow1
SELECT (lcUsedFile)
lnMarker = RECNO()
IF llExtSize
  *-- Know the postion of record and get its size to control <More> button
  *-- state and refresh sizes titles for sizes edit region.
  *-- Note that there are more techniques eaiser than the one used, 
  *-- for example : 1- add a field hold recno for every combination to temp. scale file.
  *--             : 2- using the same technique but SELECT SQL direct 
  *--             :    in array instead of using a file before it.[begin]
  IF !laScrMode[1] AND                         ;
     ((laData[3] != 3) OR ((laData[3] = 3) AND ;
     !EMPTY(laDim1[1]))) AND !EOF(lcUsedFile)
    IF llNewComb
      = lfCapPosBw()		&& Function that calculate position
      llNewComb = .F.
    ENDIF  
  
    SELECT (lcTempCnt)
    COUNT TO lnAccom FOR cDim1+cDim2 == &lcUsedFile..cDim1+&lcUsedFile..cDim2
    
    *-- We make recurrsion at this point to avoid errors
    *-- occur when lnAccom = 0.
    IF lnAccom = 0
      llNewComb = .T.
      =lfwBrow1()		&&Recurrsion call
    ENDIF
    DIMENSION laPostion[lnAccom]
    lnI = 1
    GO TOP
    
    *E301209,1 Add Scale at start of scan condition [Begin]
    *SCAN FOR &lcTempCnt..cDim1 + &lcTempCnt..cDim2 == ;
    *         &lcUsedFile..cDim1+ &lcUsedFile..cDim2
    *B603152,1 AMH 08/30/2000  Fixing Size sescription doesn't changed [Start]
    *SCAN FOR &lcTempCnt..Scale + &lcTempCnt..cDim1 + &lcTempCnt..cDim2 == ;
    *         &lcUsedFile..Scale + &lcUsedFile..cDim1+ &lcUsedFile..cDim2
    SCAN FOR &lcTempCnt..cDim1 + &lcTempCnt..cDim2 == ;
             &lcUsedFile..cDim1+ &lcUsedFile..cDim2
    *B603152,1 AMH 08/30/2000  Fixing Size sescription doesn't changed [End  ]
    *E301209,1 Add Scale at start of scan condition [End  ]

      laPostion[lnI] = &lcTempCnt..SZ1
      lnI = lnI + 1
    ENDSCAN
    SELECT (lcUsedFile)
    lnCnt = ASCAN(laPostion,&lcUsedFile..SZ1)
    lnSize = lnCnt * 8 - 7
    = lfRefresh(lcWinC12)		&&Refresh edit sizes titles.
    
    *E301209,1 Refresh extended key object [Begin]
    SHOW GET lcScaleKey 
    *E301209,1 Refresh extended key object [end  ]
    
    SHOW GET lcGetDim1 
    SHOW GET lcGetDim2

  ENDIF  
  *-- Know the postion of record [end]
  
  IF laScrMode[2]
    = lfRefExtD('DISABLE')	&& Refresh gets and disable its objects.
  ENDIF

  IF laScrMode[3] OR laScrMode[4]
    IF ladata[3] = 3 AND EMPTY(laDim1[1])
      = lfRefExtD('')	&& Refresh gets and disable its key.
    ELSE
      = lfRefExtD('ENABLE',.T.,.T.)	&& Refresh gets and disable its key.
    ENDIF
  ENDIF
  = lfMoreEnab()
ELSE		&& Single size scale
  = lfRefresh(lcWinC12)
  = lfRefSinD()
ENDIF
SHOW WINDOW (lcBrowTi1) REFRESH
*-- end of lfwBrow1.


*!*************************************************************
*! Name      : lfActBrow2
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/10/98
*! Purpose   : Parent prepak Browse function (only Extended size scale)
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfwBrow2,lfvBrow2
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfActBrow2()
*!*************************************************************
FUNCTION lfActBrow2
IF !EMPTY(lcBrowTi2) AND WEXIST(lcBrowTi2) 
  RELEASE WINDOW (lcBrowTi2)
ENDIF

lcBrfld2 =  "cMarker = IIF(RECNO()=lnMarker2 ,'>',' '):H=' ':01:R:W=.F.," ;
         +  "Prepak :P='@! X' :H='ID' :6 :W=((laScrMode[3] OR laScrMode[4]) AND EMPTY(Prepak))       ;
             :V=lfvPrePak()," +  "pPTot :H='Quantity' :18:W=.F."

SELECT (lcPrParent)
GO TOP
lnMarker2 = RECNO()

BROWSE FIELDS &lcBrfld2    ;
       WHEN lfwBrow2()     ;
                  NOAPPEND ;
                  NOCLEAR  ;
                  NODELETE ;
                  NOMENU   ;
                  NOWAIT   ;
                  SAVE     ;
  	   TITLE lcBrowTi2     ;
       VALID :F lfvBrow2() ;         
	   WINDOW (lcWinC21)   ;
	   IN WINDOW (lcWinCh2)

*-- end of lfActBrow2.

*!*************************************************************
*! Name      : lfVBrow2
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/10/98
*! Purpose   : Valid function for Parent prepak Browse (only Extended size scales)
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVBrow2()
*!*************************************************************
FUNCTION lfVBrow2
IF WONTOP() # (lcBrowTi2)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfVBrow2

*!*************************************************************
*! Name      : lfwBrow2
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/10/98
*! Purpose   : When function for Parent prepak Browse (only Extended size scales)
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab2,lpBackTab2
*!             Functions  : lfwBrow3
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrow2()
*!*************************************************************
FUNCTION lfwBrow2
*-- Adjust record pointer for Child prepak browse 
SELECT (lcPrChild)
LOCATE FOR &lcLocInCh
= lfwBrow3()

SELECT (lcPrParent)
lnMarker2  = RECNO()

llValidPre = !EMPTY(&lcPrParent..Prepak) && You cannot move from a key if it's .F.  

*-- Restore trapping [begin]
ON KEY LABEL TAB        DO lpTab2
ON KEY LABEL BACKTAB    DO lpBackTab2
*-- Restore trapping [end]
SHOW WINDOW (lcBrowTi2) REFRESH
*-- end of lfwBrow2.

*!*************************************************************
*! Name      : lfBrow3Fds
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/10/98
*! Purpose   : Prepair fields used in prepak detail (Child) browse.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfOldPp,lfvPpQty
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfBrow3Fds()
*!*************************************************************
*! Note      : 1- I separate it because it is too long to the compiler
*!           :    and there is two cases (one for extended and the other 
*!           :    is for single) size scale.
*!           : 2- When and validation functions is inside the browse fields.
*!*************************************************************
FUNCTION lfBrow3Fds
*-- if extended size scale 
IF llExtSize
  lcBrfld31 = "Pp1 :H= lcInBrowSz :7;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz1) AND lfOldPp('1') ;
                   :V=lfvPpQty('1'), " +;
              "Pp2 :H= lcInBrowSz :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz2) AND lfOldPp('2') ;
                   :V=lfvPpQty('2'), " +;
              "Pp3 :H= lcInBrowSz :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz3) AND lfOldPp('3') ;
                   :V=lfvPpQty('3'), " +;
              "Pp4 :H= lcInBrowSz :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz4) AND lfOldPp('4') ;
                   :V=lfvPpQty('4'), "

  lcBrfld32 = "Pp5 :H= lcInBrowSz :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz5) AND lfOldPp('5') ;
                   :V=lfvPpQty('5'), " +;
              "Pp6 :H= lcInBrowSz :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz6) AND lfOldPp('6') ;
                   :V=lfvPpQty('6'), " +;
              "Pp7 :H= lcInBrowSz :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz7) AND lfOldPp('7') ;
                   :V=lfvPpQty('7'), " +;
              "Pp8 :H= lcInBrowSz :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz8) AND lfOldPp('8') ;
                   :V=lfvPpQty('8') "

ELSE		&& Single size scale
  lcBrfld31 = "Pp1 :H= &lcUsedFile..Sz1 :7;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz1) AND lfOldPp('1') ;
                   :V=lfvPpQty('1'), " +;
              "Pp2 :H= &lcUsedFile..Sz2 :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz2) AND lfOldPp('2') ;
                   :V=lfvPpQty('2'), " +;
              "Pp3 :H= &lcUsedFile..Sz3 :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz3) AND lfOldPp('3') ;
                   :V=lfvPpQty('3'), " +;
              "Pp4 :H= &lcUsedFile..Sz4 :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz4) AND lfOldPp('4') ;
                   :V=lfvPpQty('4'), "

  lcBrfld32 = "Pp5 :H= &lcUsedFile..Sz5 :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz5) AND lfOldPp('5') ;
                   :V=lfvPpQty('5'), " +;
              "Pp6 :H= &lcUsedFile..Sz6 :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz6) AND lfOldPp('6') ;
                   :V=lfvPpQty('6'), " +;
              "Pp7 :H= &lcUsedFile..Sz7 :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz7) AND lfOldPp('7') ;
                   :V=lfvPpQty('7'), " +;
              "Pp8 :H= &lcUsedFile..Sz8 :7 ;
                   :W=(laScrMode[3] OR laScrMode[4]) AND ;
                      !EMPTY(Prepak) AND !EMPTY(Sz8) AND lfOldPp('8') ;
                   :V=lfvPpQty('8') "
ENDIF
*-- end of lfBrow3Fds.

*!*************************************************************
*! Name      : lfActBrow3
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/10/98
*! Purpose   : Child prepak Browse function (Extended and Single) size scale
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfwBrow2,lfvBrow2
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfActBrow3()
*!*************************************************************
FUNCTION lfActBrow3
IF !EMPTY(lcBrowTi3) AND WEXIST(lcBrowTi3) 
  RELEASE WINDOW (lcBrowTi3)
ENDIF
STORE "" TO lcBrfld31,lcBrfld32		&& Variables that hold browse fields.

lcMarker  = "cMarker = IIF(RECNO()=lnMarker3 ,'>',' '):H=' ':01:R :W =.F., "

= lfBrow3Fds()	&&Call function that prepair fields used in browse. 

*-- Extended size scale
IF llExtSize
  lcBrfld3 = lcMarker + lcBrfld31 + lcBrfld32
ELSE 		&&Single size scale 
  *-- Note that : validation of prepak is same as parent browse in extended 
  *-- size scale but it is in one browse window not in Parent and Child. 
  lcAddFlds = "Prepak :P='@! X' :H='ID' :6 :W=((laScrMode[3] OR laScrMode[4]) AND EMPTY(Prepak)) ;
              :V=lfvPrePak()," +  "pPTot :H='Quantity' :15:W=.F. ,"
  lcBrfld3  = lcMarker + lcAddFlds + lcBrfld31 + lcBrfld32
ENDIF

SELECT (lcPrChild)
LOCATE FOR &lcLocInCh
lnMarker3 = RECNO()
*-- Single size scale 
IF !llExtSize
  *-- Adjust browse title to hold the corressponding scale.
  lcBrowTi3  = "Prepacks for scale : " + IIF(laScrMode[2],scale.scale,lcScale)
ENDIF  

BROWSE FIELDS &lcBrfld3    ;
       FOR  &lcLocInCh     ;
       WHEN lfwBrow3()     ;
                  NOAPPEND ;
                  NOCLEAR  ;
                  NODELETE ;
                  NOMENU   ;
                  NOWAIT   ;
                  SAVE     ;
  	   TITLE lcBrowTi3     ;
       VALID :F lfvBrow3() ;         
	   WINDOW (lcWinC24)   ;
	   IN WINDOW (lcWinCh2)

*-- end of lfActBrow3.

*!*************************************************************
*! Name      : lfVBrow3
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/10/98
*! Purpose   : Valid function for Child prepak Browse (Extended and Single) size scales.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVBrow3()
*!*************************************************************
FUNCTION lfVBrow3
IF WONTOP() # (lcBrowTi3)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfVBrow3

*!*************************************************************
*! Name      : lfwBrow3
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/10/98
*! Purpose   : When function for Child prepak Browse (Extended and Single) size scales.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfRefresh,lfPrepBtn
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrow2()
*!*************************************************************
FUNCTION lfwBrow3
SELECT (lcPrChild)
lnMarker3 = RECNO()
SHOW WINDOW (lcBrowTi3) REFRESH

IF llExtSize
  = lfRefresh(lcWinC23)
  IF ladata[3] >= 2
    = lfRefresh(lcWinC22)
  ENDIF
ENDIF  

= lfPrepBtn()		&& Control enable and disable buttons.
*-- end of lfwBrow3.

*!*************************************************************
*! Name      : lfTrap
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : Trap function for all Browses (in both Extended and single) size scale
*!           : Deactivate screen function.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab,lpBackTab,lpTab2,lpBackTab2
*!                        : lpTab3,lpBackTab3
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfTrap
*-- lcWinsCond : Hold Condition for TOP windows to distinguish between 
*--            : single and extended size scales.
lcWinsCond = IIF(!llExtSize,"INLIST(ALLTRIM(WONTOP()),ALLTRIM(lcBrowTi1)  ,;
                                                      ALLTRIM(lcBrowTi3))",;
              "INLIST(ALLTRIM(WONTOP()),ALLTRIM(lcBrowTi1),;
                                        ALLTRIM(lcBrowTi2),;
                                        ALLTRIM(lcBrowTi3))")

*-- if TOP window is one of the browses window                                        
IF &lcWinsCond
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing
ENDIF

*-- cases on TOP window 
DO CASE
  *-- Case TOP window is scale browse window 
  CASE ALLTRIM(WONTOP())  = ALLTRIM(lcBrowTi1)
    ON KEY LABEL TAB           DO lpTab
    ON KEY LABEL CTRL+TAB      DO lpTab
    ON KEY LABEL BACKTAB       DO lpBackTab
  *-- Case Extended size scale and TOP window is Parent prepak browse window  
  CASE llExtSize AND (ALLTRIM(WONTOP())  = ALLTRIM(lcBrowTi2))
    ON KEY LABEL TAB           DO lpTab2
    ON KEY LABEL CTRL+TAB      DO lpTab2
    ON KEY LABEL BACKTAB       DO lpBackTab2
  *-- Case TOP window is Child prepak browse window  
  CASE ALLTRIM(WONTOP())  = ALLTRIM(lcBrowTi3)
    ON KEY LABEL TAB           DO lpTab3
    ON KEY LABEL CTRL+TAB      DO lpTab3
    ON KEY LABEL BACKTAB       DO lpBackTab3
ENDCASE
*-- end of lfTrap.

*!*************************************************************
*! Name      : lfUnTrap
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : UnTrap function for all Browses (in both Extended and single) size scale
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

*-- lcWinsCond : Hold Condition for TOP windows to distinguish between 
*--            : single and extended size scales.
lcWinsCond = IIF(!llExtSize,"!INLIST(ALLTRIM(WONTOP()),ALLTRIM(lcBrowTi1)  ,;
                                                      ALLTRIM(lcBrowTi3))",;
              "!INLIST(ALLTRIM(WONTOP()),ALLTRIM(lcBrowTi1),;
                                        ALLTRIM(lcBrowTi2),;
                                        ALLTRIM(lcBrowTi3))")
*-- if TOP window is not one of the browses window                                        
IF &lcWinsCond
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL CTRL+TAB
  ON KEY LABEL CTRL+ENTER
  ON KEY LABEL CTRL+HOME
  ON KEY LABEL CTRL+END
  ON KEY LABEL CTRL+W
ENDIF
*-- end of lfUnTrap.

*!*************************************************************
*! Name      : lpTab
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : Trapping TAB order for scale browse window
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
ON KEY LABEL CTRL+TAB

*-- Go to next window
IF laScrMode[1] OR laScrMode[2]
  ACTIVATE WINDOW gwcContrl1		&& Activate control panel window
  _CUROBJ = OBJNUM(pbTop)
ELSE
  IF llExtSize
    ACTIVATE WINDOW (lcWinC13)		&& Activate scale panel window
    _CUROBJ = OBJNUM(pbNew)
  ELSE		&& Single size scale
    ACTIVATE WINDOW (lcWinC12)		&& Activate Edit scale window 
    _CUROBJ = OBJNUM(lcScale)
  ENDIF  
ENDIF
*-- end of lpTab.

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : Trapping BACKTAB order for scale browse window
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
*-- if Extended size scale and 3 dimension case
IF llExtSize AND ladata[3] = 3
  ACTIVATE WINDOW (lcWinC14)		&&Activate popup window 
  _CUROBJ = OBJNUM(lnPosDim1)
ELSE
  ACTIVATE WINDOW (lcfolder)		&&Activate folder window
  _CUROBJ = OBJNUM(ibFolder[1])
ENDIF
*-- end of lpBackTab.

*!*************************************************************
*! Name      : lpTab2
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : Trapping TAB order for prepak parent browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab2
*!*************************************************************
PROCEDURE lpTab2
ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB

*-- Go to next window
*-- if the current prepak code is valid
IF llValidPre 
  ACTIVATE WINDOW (lcWinC22)		&& Activate prepak panel window	
  _CUROBJ = OBJNUM(ibTrap3)
ENDIF  
*-- end of lpTab2.

*!*************************************************************
*! Name      : lpBackTab2
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : Trapping BACKTAB order for parent prepak browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab2
*!*************************************************************
PROCEDURE lpBackTab2
ON KEY LABEL BACKTAB

*-- Go to previous window
IF llValidPre 
  ACTIVATE WINDOW (lcfolder)		&&Activate folder window
  _CUROBJ = OBJNUM(ibFolder[2])
ENDIF  
*-- end of lpBackTab2.

*!*************************************************************
*! Name      : lpTab3
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : Trapping TAB order for child prepak browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab3
*!*************************************************************
PROCEDURE lpTab3
ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB

*-- Go to next window
ACTIVATE WINDOW (lcWinC22)		&& Activate control panel window
_CUROBJ = OBJNUM(pbNew2)
*-- end of lpTab3.

*!*************************************************************
*! Name      : lpBackTab3
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : Trapping BACKTAB order for child prepak browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab3
*!*************************************************************
PROCEDURE lpBackTab3
ON KEY LABEL BACKTAB

*-- Go to previous window
IF llExtSize
  ACTIVATE WINDOW (lcWinC22)		&&Activate Parent prepak browse window
  _CUROBJ = OBJNUM(ibTrap2)
ELSE
  ACTIVATE WINDOW (lcfolder)		&&Activate folder window
  _CUROBJ = OBJNUM(ibFolder[2])
ENDIF  
*-- end of lpBackTab3.

*----------------------------------------------------------
*-- Control screen section
*-- This section contains all functions and procedures used in 
*-- control showing in screen either in scales or prepaks. 
*----------------------------------------------------------
*!*************************************************************
*! Name      : lpShow
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : Handling the screen modes.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpZooming,lpActFold
*!             Functions  : lfActBrow1,lfActBrow2,lfActBrow3,lfWBrow3
*!                        : lfClsData,lfRefresh,lfMoreStat,lfCollectP
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************
PROCEDURE lpShow
*-- if you come from another session.
IF llAnother
  DO lpActFold
  llAnother = .F.
  RETURN
ENDIF  

*-- if you come from the same session and prepak child file have data.
IF RECCOUNT(lcPrChild) != 0) 
  lnAlias = SELECT(0)
  *-- Delete any existing data in the same ssession
  IF llExtSize
    SELECT (lcPrParent)
    ZAP
    IF laScrMode[1] OR laScrMode[4]
      = lfActBrow2()  	&& Refresh Parent prepak browse
    ENDIF
  ENDIF
    
  SELECT (lcPrChild)
  ZAP
  IF laScrMode[1] OR laScrMode[4]
    = lfActBrow3()		&& Refresh Child prepak browse
  ENDIF
  SELECT (lnAlias)
ENDIF

*-- Both Select and Add modes is in extended sizes only
IF llExtSize
  DO CASE
    CASE laScrMode[1]		&& <---- SELECT MODE ---->
      DO lpZooming		&& Zooming the screen to its initial state.
      =lfClsData()		&& Clear data on the screen and disable objects.

      *-- if we find the temp. scale file we must clear data from it if any.
      IF FILE(gcWorkDir+lcScaline+'.DBF') AND USED(lcScaline) AND ;
         RECCOUNT(lcScaline) != 0 
        lcAlias = SELECT(0)
        *-- Delete any existing data in the same ssession
        SELECT (lcScaline)
        ZAP
        SELECT (lcAlias)
      ENDIF  
      *-- Redeclaration of variables and arrays.
      DIMENSION laDim1[1]
      laDim1 = '     ' 
      STORE ' ' TO lcInBrowSz,lcInBrowFd

      SELECT ScaleHd
      SCATTER FIELDS &lcScFields MEMO TO laData BLANK
      lnSize = 1
      = lfRefresh(lcWinC12)
      *-- Disable all buttons [begin]
      SHOW GET pbNew     DISABLE 
      SHOW GET pbRemove  DISABLE 
      
      *E301209,1 Temporary comment out the following until take descision about <Auto> button
      *SHOW GET pbAuto    DISABLE 

      SHOW GET lcGetDim1 DISABLE
      SHOW GET lcGetDim2 DISABLE

      SHOW GET pbNew2    DISABLE 
      SHOW GET pbRemove2 DISABLE 
      
      STORE 'DISABLE' TO lcNews,lcRemoves,lcNew2,lcRemove2,lcAutos
      *-- Disable all buttons [end]
    CASE laScrMode[4]		&& <---- ADD    MODE ---->
      laData[3] = 1			&& It is the initial state for add mode.
      *--Create temp scale file.
      SELECT Scale
      SELECT *   ;
      FROM Scale ;
      WHERE .F.  ;
      INTO DBF (gcWorkDir+lcScaline)
      
      *E301209,1 Add Scale at start of Index [Begin]
      *INDEX ON cDim1+cDim2 TAG (lcScaline)
      INDEX ON Scale+cDim1+cDim2 TAG (lcScaline)
      *E301209,1 Add Scale at start of Index [End  ]
      
 ENDCASE

ELSE			&& Single size scale
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

  *-- Disable delete,select and browse buttons in control panel [begin]
  laCtrStat[8]  = "DISABLE"                && Delete button
  laCtrStat[9]  = "DISABLE"                && Select button
  laCtrStat[10] = "DISABLE"                && Browse button

  SHOW GET pbbrws DISABLE
  SHOW GET PBSLCT DISABLE
  SHOW GET PBDLT  DISABLE
  *-- Disable delete,select and browse buttons in control panel [end]
ENDIF

DO CASE
  CASE laScrMode[2]		&& <---- VIEW   MODE ---->
    STORE .T. TO llValidPre		&& In view mode all prepak codes is valid.
      
    *-- Clear data if found from temp. Scale file .
    IF FILE(gcWorkDir+lcScaline+'.DBF') AND USED(lcScaline) AND ;
       RECCOUNT(lcScaline) != 0
      lnAlias = SELECT(0)
      IF llExtSize
        STORE .F. TO llSwMode		&& Restore its initiall value.
      ELSE		&& Single size scale 
        SELECT (lcScaline)
        lcScRec = "S"+&lcScaline..Scale
      ENDIF
      *-- Delete any existing data in the same ssession
      SELECT (lcScaline)
      ZAP
      SELECT (lnAlias)
    ENDIF  

    *-- Renew ladata values.
    SELECT ScaleHd
    SCATTER FIELDS &lcScFields MEMO TO laData

    *E301209,1 Adjust ladata[1] which point to scale code.
    laData[1] = PADR(laData[1],lnScaleLen)
    
    *E301209,1 Adjust Seek in master file condition [Begin]
    *lcMastSeek = IIF(llExtSize,"P"+&lcSeekPrep,"P")  && Hold seek condition.
    lcMastSeek = IIF(llExtSize,"P"+laData[1],"P")     && Hold seek condition.
    *E301209,1 Adjust Seek in master file condition [End  ]

    *-- if you find prepak data collect it in temporary prepak files.
    IF SEEK(lcMastSeek,'SCALE')
      = lfCollectP()		&& Collect prepak data.
    ELSE		&& You did not find prepak data
      IF llExtSize
        = lfActBrow2()
      ENDIF  
    ENDIF

    IF llExtSize
      *-- if your data is 3 dimension case, fill popup array and sort it.  
      IF laData[3] = 3
        SELECT DISTINCT cDim1   ;
        FROM SCALE              ; 
        WHERE SCALE = laData[1] ; 
        INTO ARRAY laDim1
        = ASORT(laDim1)
      ELSE  && 1 or 2 dimensions
        *-- if popup array have any data, clear it.
        IF !EMPTY(laDim1[1])
          DIMENSION laDim1[1]
          laDim1 = ''
        ENDIF
      ENDIF  

      DO lpZooming

      *E301209,1 Temporary comment out the following until take descision about <Auto> button
      *SHOW GET pbAuto DISABLE 

      STORE 'DISABLE' TO lcAutos
    ELSE		&& Single size scale
      *-- Refresh record pointer to be the same as, pointer of edit mode 
      *-- if you come from it and this scale is found or first record if 
      *-- this record not found due to using <Cancel> button in control panel
      *-- in the previous edit mode. 
      = SEEK(lcScRec,'SCALE')
      IF EOF()
        lcScRec = "S"
        = SEEK(lcScRec,'SCALE')
      ENDIF
    ENDIF
    = lfActBrow3()		&&Refresh prepak folder detail browse
    = lfWBrow3()		&&Enable and disable prepak folder buttons.
    
    lnSize = 1
    = lfRefresh(lcWinC12)

    SHOW GET lcGetDim1
    SHOW GET lcGetDim2

    SHOW GET pbNew     DISABLE 
    SHOW GET pbRemove  DISABLE 
    STORE 'DISABLE' TO lcNews,lcRemoves

    *-- When Enter view mode you must be in browse window.
    IF lnActFolder = 1
      _CUROBJ = OBJNUM(ibTrap1)
    ELSE
      lcTrap2 = IIF(llExtSize,"ibTrap2","ibTrap3")	&& Hold inv. button name due to size scale type
      _CUROBJ = OBJNUM(&lcTrap2)
    ENDIF  
  CASE laScrMode[3]		&& <---- EDIT   MODE ---->

    *E301209,1 Adjust ladata[1] with proper length.
    laData[1] = PADR(laData[1],lnScaleLen)
    
    *-- lcColect  : hold expr. for data collection in both ext. or single size scale.
    lcColect = IIF(llExtSize,'Scale = laData[1] AND Type  = "S"';
                            ,'Type  = "S"')
    IF !llExtSize
      lcScRec = "S"+SCALE.Scale
    ENDIF  

    *--Create temp files.
    SELECT Scale
    SELECT *,"S" AS cStatus ;
    FROM Scale              ;
    WHERE &lcColect         ;
    INTO DBF (gcWorkDir+lcScaline)

    *E301209,1 Filter on not deleted records.
    SET FILTER TO cStatus <> 'D'

    *-- if the temp. scale file does not have data and you are 
    *-- in extended size scale you must rise llSwMode flag
    *-- to refresh your generated codes and have wide gaps 
    *-- when you create new sizes.
    llSwMode = (RECCOUNT(lcScaline) = 0) AND llExtSize

    *E301209,1 Adjust Seek in master file condition [Begin]
    *MAB HERE
    *lcMastSeek = IIF(llExtSize,"P"+&lcSeekPrep,"P")  && Hold seek condition.
    lcMastSeek = IIF(llExtSize,"P"+laData[1],"P")     && Hold seek condition.
    *E301209,1 Adjust Seek in master file condition [Begin]

    IF SEEK(lcMastSeek,'SCALE')
      = lfCollectP()		&& Collect prepak data.
    ELSE		            && You did not find prepak data
      IF llExtSize
        = lfActBrow2()
      ENDIF  
    ENDIF  

    SELECT (lcScaline)
    IF llExtSize
      
      INDEX ON Scale+cDim1+cDim2 TAG (lcScaline)
      
      *-- if you are in scale folder and your case is either 
      *-- (have data in temp. scale file and spinner value is greater than 1)
      *-- or you did not have data in temp. scale file, enable new button.
      IF (lnActFolder = 1) AND !((RECCOUNT() > 0) AND (laData[3] = 1)) 
        SHOW GET pbNew ENABLE 
        STORE 'ENABLE' TO lcNews
      ENDIF  
      *-- if you are in scale folder and the spinner value is greater than 1.
      IF (lnActFolder = 1) AND (ladata[3] > 1)

        *E301209,1 Temporary comment out the following until take descision about <Auto> button
        *SHOW GET pbAuto ENABLE
        STORE 'ENABLE' TO lcAutos
      ENDIF  

    ELSE		&& Single size scale

      INDEX ON SCALE+CSTATUS TAG (lcScaline)
            
      *-- Refresh record pointer to be the same as, pointer of view mode 
      = SEEK(lcScRec,'SCALE')
      IF EOF('SCALE')
        lcScRec = "S"
        = SEEK(lcScRec,'SCALE')
      ENDIF
      
      *-- Refresh record pointer to be the same as, pointer of view mode 
      = SEEK(SUBSTR(lcScRec,2,LEN(lcScRec)),lcScaline)
      IF EOF(lcScaline)
        lcScRec = "S"
        GO TOP IN (lcScaline)
      ENDIF

      IF lnActFolder = 1
        SHOW GET pbNew ENABLE 
        STORE 'ENABLE' TO lcNews
      ENDIF  
    
    ENDIF
    = lfActBrow3()		&& Refresh Child prepak browse
    = lfWBrow3()		&&Enable and disable prepak folder buttons.

    IF lnActFolder = 1
      *-- <Remove> button state is according to the state of temp. scale
      *-- file, if it has no data <Remove> must be disable.
      lcRemvStat = IIF(RECCOUNT(lcScaline) = 0             ;
                       OR EOF(lcScaline) OR BOF(lcScaline),;
                       "DISABLE","ENABLE")
      SHOW GET pbRemove &lcRemvStat
      STORE lcRemvStat TO lcRemoves
      _CUROBJ = OBJNUM(ibTrap1)
    ELSE  
      lcTrap2 = IIF(llExtSize,"ibTrap2","ibTrap3")	&& Hold inv. button name due to size scale type
      _CUROBJ = OBJNUM(&lcTrap2)
    ENDIF  
ENDCASE

*-- if scale folder and extended size scale
IF (lnActFolder = 1) AND llExtSize 
  = lfMoreStat()		&&Enable and disable <More> button due to our case.
ENDIF  

DO lpActFold

*-- if it is not the first time you enter the screen.
IF llActBrow1 
  = lfActBrow1()
ELSE
  llActBrow1 = !llActBrow1
ENDIF  
*-- End of lpShow 

*!*************************************************************
*! Name      : lpZooming
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/30/97
*! Purpose   : Switch screen shape corressponding to number of dimensions.
*!           : Extended sizes only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpZooming
*!*************************************************************
PROCEDURE lpZooming
*-- if 1 dimension
IF (ladata[3] <= 2)
  SHOW GETS WINDOW (lcWinC14) DISABLE ONLY
  HIDE WINDOW (lcWinC14) SAME
ELSE	&& 2 or 3 dimensions
  SHOW WINDOW (lcWinC14) SAME
  lnPosDim1 = 1
  IF lnActFolder = 1  
    SHOW GET lnPosDim1 ENABLE
  ENDIF  
  llNewComb = .T.		&& Initiate combinations.
  IF !EMPTY(lcTempCnt) AND USED(lcTempCnt)
    USE IN (lcTempCnt)		&& erase cusror file
  ENDIF
  DIMENSION laPostion[1]  
  laPostion = ""
ENDIF
*-- end of lpZooming.

*!*************************************************************
*! Name      : lpActFold
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/02/98
*! Purpose   : Disable deactive folder objects.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpActFold
*!*************************************************************
PROCEDURE lpActFold
*-- if scale folder
IF lnActFolder = 1
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY
ELSE		&& prepak folder
  *-- if select or view mode
  IF laScrMode[1] OR laScrMode[2]
    SHOW GET ibTrap1 DISABLE
  ELSE		&& Edit or Add modes
    IF llExtSize
      SHOW GETS WINDOW (lcWinC13) DISABLE ONLY
    ENDIF      
    SHOW GETS WINDOW (lcWinC12) DISABLE ONLY
  ENDIF  
ENDIF  
*-- end of lpActFold.

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : Validating the active folder
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfMoreStat,lfRefExtD,lfRefSinD,lfAddState
*!                        : lfWBrow3
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfActFolder()
*!*************************************************************
FUNCTION lfActFolder
*-- if you are in scale folder
IF lnActFolder = 1
  SHOW GETS WINDOW (lcWinC22) DISABLE ONLY	&& Prepak panel
 
  SHOW WINDOW (lcWinCh1) TOP
  SHOW GET ibTrap1 ENABLE 

  IF llExtSize
    = lfMoreStat()
    IF (ladata[3] = 3)
      SHOW GET lnPosDim1 ENABLE
    ENDIF  
  ENDIF  

  DO CASE
    CASE laScrMode[2]
      IF llExtSize
        = lfRefExtD('DISABLE')	&& Disable edit sizes region
      ELSE
        =lfRefSinD()			&& Disable edit sizes region
      ENDIF  
    CASE laScrMode[3] OR laScrMode[4]
      = lfAddState()  && Enable/Disable edit sizes region and scale panel buttons.
  ENDCASE  
  IF laScrMode[2] OR laScrMode[3]
    _CUROBJ = OBJNUM(ibTrap1)		&& To be in scale browse
  ENDIF  
  SHOW WINDOW (lcBrowTi1) REFRESH	&& Refresh scale browse
ELSE	&& You are in prepak folder
  *-- if extended size Scale the following window is sizes Edit region
  *-- else it is scale panel buttons.
  SHOW WINDOW (lcWinCh2) TOP
  SHOW GETS WINDOW (lcWinC12) DISABLE ONLY
  IF llExtSize
    SHOW GETS WINDOW (lcWinC13) DISABLE ONLY	&& Scale panel buttons.
    SHOW WINDOW (lcBrowTi2) REFRESH   && Refresh parent prepak browse 
  ENDIF  
  SHOW GET ibTrap2 ENABLE 
  SHOW GET ibTrap3 ENABLE 
  = lfActBrow3()	&& Refresh Child prepak browse when switch folders.
  = lfWBrow3()		&&Enable and disable prepak folder buttons.
  *SHOW WINDOW (lcBrowTi3) REFRESH	&& Refresh child prepak browse
ENDIF
*-- end of lfActFolder.

*!*************************************************************
*! Name      : lfAddState
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : Enable/Disable scale folder objects in Edit or Add modes
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfRefExtD,lfRefSinD
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAddState()
*!*************************************************************
FUNCTION lfAddState
llRemvStat = RECCOUNT(lcScaline) = 0 OR ;
             EOF(lcScaline)          OR ;
             BOF(lcScaline)

lcRemvStat = IIF(llRemvStat,"DISABLE","ENABLE")
        
SHOW GET pbRemove &lcRemvStat
STORE lcRemvStat TO lcRemoves

IF llExtSize
  IF llRemvStat
    = lfRefExtD('DISABLE')
  ELSE
    = lfRefExtD('ENABLE',.F.,.T.)
  ENDIF  
  *-- if no data in temp. scale file and we are in 1 dimension case
  *-- or we are in 2 or 3 dimension case and the previous condition
  *-- need all dimension titles to be filled.
  IF ((llRemvStat AND (laData[3] = 1)) OR (laData[3] > 1)) AND ;
     (ladata[3] = 1 AND !EMPTY(ladata[4])) OR ;
     (ladata[3] = 2 AND (!EMPTY(ladata[4]) AND !EMPTY(ladata[5]))) OR ;
     (ladata[3] = 3 AND (!EMPTY(ladata[4]) AND !EMPTY(ladata[5])   AND;
                         !EMPTY(ladata[6])))
    SHOW GET pbNew ENABLE 
    STORE 'ENABLE' TO lcNews
  ENDIF  
  *-- If we are in 2 or 3 dimension case and all dimension titles
  *-- filled with data.
  IF (ladata[3] > 1) AND ;
     (ladata[3] = 2 AND (!EMPTY(ladata[4]) AND !EMPTY(ladata[5]))) OR ;
     (ladata[3] = 3 AND (!EMPTY(ladata[4]) AND !EMPTY(ladata[5])   AND;
                         !EMPTY(ladata[6])))

    *E301209,1 Temporary comment out the following until take descision about <Auto> button
    *SHOW GET pbAuto ENABLE
    STORE 'ENABLE' TO lcAutos
  ENDIF
ELSE
  =lfRefSinD()
  SHOW GET pbNew ENABLE 
  STORE 'ENABLE' TO lcNews
ENDIF  
*-- end of lfAddState.

*!*************************************************************
*! Name      : lfPrepBtn
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : Enable/Disable prepak buttons.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPrepBtn()
*!*************************************************************
FUNCTION lfPrepBtn
lcNew2St   = IIF(laScrMode[1] OR laScrMode[2] OR (lnActFolder = 1),'DISABLE',;
             IIF((RECCOUNT(lcScaline) = 0) OR EOF(lcScaline) OR ;
                  BOF(lcScaline),'DISABLE','ENABLE'))

lcDealFile = IIF(llExtSize,lcPrParent,lcPrChild)
lcRemov2St = IIF(laScrMode[1] OR laScrMode[2] OR (lnActFolder = 1),'DISABLE',;
             IIF((RECCOUNT(lcDealFile) = 0) OR EOF(lcDealFile) OR;
                  BOF(lcDealFile),'DISABLE','ENABLE'))

SHOW GET pbNew2 &lcNew2St
SHOW GET pbRemove2 &lcRemov2St

STORE lcNew2St   TO lcNew2
STORE lcRemov2St TO lcRemove2
*-- end of lfPrepBtn.

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/05/98
*! Purpose   : Saving data from temporary files to master files
*!*************************************************************
*! Calls     : 
*!             Procedures : lpDontSave,lpAddSz,lpAddPp,lpEdCode
*!                        : lpReplace,lpRepPps
*!             Functions  : lfWBrow1,lfCoding,lfCalCnt
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpSavscr
*!*************************************************************
PROCEDURE lpSavScr

PRIVATE lnAlias,llMaster
lnAlias = SELECT(0)

*-- Prepair to save new data [begin]
*-----------------------------------------------------------------------
*-- if empty of temp. scale file and modes is normal

*E301209,1 Filter on deleted records do the same function [Begin]
*IF RECCOUNT(lcScaline) = 0 AND !llSwMode
SELECT (lcScaline)
GO TOP
IF EOF(lcScaline) AND !llSwMode
*E301209,1 Filter on deleted records do the same function [End  ]

  *-- The following message is 
  *-- "Scale details not found. Cannot proceed.."
  *--                  < Ok >
  = gfModalGen('TRM42109B42000','Dialog')  
  DO lpDontSave		&& Don't save data and return to screen.
  RETURN
ELSE	&& File has data or you are in switch mode
  llModify = .F.		&& If this flag is .T. we need to modify its line.
  *-- if you have existing data in temporary file
  IF RECCOUNT(lcScaline) != 0
    SELECT (lcScaline)
    *-- if you found empty size1 this means that its line is empty. 
    SCAN FOR EMPTY(&lcScaline..Sz1)
      IF llExtSize
        *-- the following message is
        *-- "Scale XXX has an empty line, proceed will skip this line."
        *--                   < Proceed >   < Modify >
        llModify = (gfModalGen('QRM42112B42008','Dialog',laData[1]) = 2)
      ELSE		&& Single size scale
        *-- the following message is
        *-- "New scale cannot be empty, proceed will skip it."
        *--                   < Proceed >   < Modify >
        llModify = (gfModalGen('QRM42113B42008','Dialog') = 2)
      ENDIF
  
      *-- if you found empty line and choose to modify it.
      IF llModify
        EXIT		&&exit scan loop and check for same condition.
      ELSE	&&if you found empty line and choose to skip it.
        DELETE
      ENDIF  
    ENDSCAN
  ENDIF  
   
  *-- if you found empty line and choose to modify it.
  IF llModify
    llCSave = .F.	&& to cancel saving
    *-- Adjust the file to the required line to modify it. [begin]
    IF llExtSize
      llNewComb = .T.
      IF !EMPTY(lcTempCnt) AND USED(lcTempCnt)
       USE IN (lcTempCnt)
      ENDIF
      IF ladata[3] = 3
        lnPosDim1 = ASCAN(laDim1,CDim1)
        SHOW GET lnPosDim1
      ENDIF
      _CUROBJ = OBJNUM(lcGetDim1)
    ELSE	&&Single size scale
      _CUROBJ = OBJNUM(lcScale)
    ENDIF
    =lfWBrow1()
    *-- Adjust the file to the required line to modify it. [end]
    RETURN
  ENDIF
  
  *-- if add mode or edit mode and some some changes make it like add mode 
  *-- in saving, for more description see description of llSwMode in program header. 
  IF laScrMode[4] OR llSwMode
    *-- if our case is switch mode, we must delete old data from master file
    *-- before continuing the procedure.
    IF llSwMode
      SELECT SCALE
      
      *MAB E300872,6 BLANK FOR Type+Scale+Prepak = "S"+laData[1]
      *MAB E300872,6 BLANK FOR Type+Scale+Prepak = "P"+laData[1]

      *E300872,6 Update Trace File for SCALE file, delete records [begin]
      *E300872,6 Full index expr. : Type(1)+Scale(3)+Prepak(1).  
      
      SCAN FOR (Type+Scale+Prepak = "S"+laData[1])
        = gfTraceKey('SCALE',"S"+SCALE.SCALE+SCALE.PREPAK,'D')
        *C200597,1  TMI [Start]  Call a customized tracekey function for Bong Wha
        IF ASCAN(laEvntTrig , PADR('ICSCALE_SD',10)) <> 0
          =gfDoTriger('ICSCALE',PADR('ICSCALE_SD',10))
        ENDIF
        *C200597,1  TMI [End  ] 
        BLANK
        DELETE
      ENDSCAN
      
      SCAN FOR (Type+Scale+Prepak = "P"+laData[1])
        = gfTraceKey('SCALE',"P"+SCALE.SCALE+SCALE.PREPAK,'D')
        *C200597,1  TMI [Start]  Call a customized tracekey function for Bong Wha
        IF ASCAN(laEvntTrig , PADR('ICSCALE_PD',10)) <> 0
          =gfDoTriger('ICSCALE',PADR('ICSCALE_PD',10))
        ENDIF
        *C200597,1  TMI [End  ] 
        BLANK
        DELETE
      ENDSCAN

      *E300872,6 Update Trace File for SCALE file, delete record [end]

      *MAB E300872,6 DELETE FOR EMPTY(Type+Scale+Prepak)
    ENDIF 
    *-- Calculate number of codes [begin]
    SELECT (lcScaline)
    COUNT TO lnCodes 
    *-- Calculate number of codes [end]
    *-- if no lines in temp. line and we are in add mode only
    *-- and this condition is because in edit mode we can delete all
    *-- lines for the current scale, and the scale still exist.   
    IF lnCodes = 0 AND !llSwMode
      *-- The following message is 
      *-- "Scale details not found. Cannot proceed.."
      *--                  < Ok >
      = gfModalGen('TRM42109B42000','Dialog')  
      DO lpDontSave
      RETURN
    ENDIF
  
  ELSE		&& else if you are in Edit mode

    *E301209,1 Release filter to start saving [Begin]
    SELECT (lcScaline)
    SET FILTER TO
    SELECT (lcPrChild)
    SET FILTER TO
    *E301209,1 Release filter to start saving [End  ]

    *-- lnAddPp : Number of added prepak lines
    *-- lnDelPp : Number of deleted prepak lines
    *-- lnModPp : Number of modified prepak lines
    STORE 0 TO lnAddPp,lnDelPp,lnModPp	&& Initial values
    *-- if you have existing lines in temp. prepak file.
    IF RECCOUNT(lcPrChild) > 0 
      SELECT (lcPrChild)
      DELETE FOR EMPTY(&lcPrChild..Prepak)
      COUNT TO lnAddPp FOR &lcPrChild..cStatus = "A"
      COUNT TO lnDelPp FOR &lcPrChild..cStatus = "D"
      COUNT TO lnModPp FOR &lcPrChild..cStatus = "M"
    ENDIF
    *-- lnAdd   : Number of added scale lines
    *-- lnDel   : Number of deleted scale lines
    *-- lnMod   : Number of modified scale lines
    *-- lnCodes : Number of codes after saving
    SELECT (lcScaline)
    COUNT TO lnAdd FOR &lcScaline..cStatus = "A"
    COUNT TO lnDel FOR &lcScaline..cStatus = "D"
    COUNT TO lnMod FOR &lcScaline..cStatus = "M"
    
    lnCodes = IIF(llExtSize,ScaleHd.nNOfCodes + lnAdd - lnDel,0)
  ENDIF
  
  GO TOP
  *-- if number of lines is greater than maximum number of codes, return.
  IF lnCodes > (36 * 36)
    *-- the following message is
    *-- "No. of items exceedes maxiumum number of codes, Cannot proceed.."
    *--                   < Ok >
    = gfModalGen('TRM42117B42000','Dialog')  
    DO lpDontSave
    RETURN
  ENDIF
ENDIF
*-----------------------------------------------------------------------
*-- Prepair to save new data [end]

*-- Saving new data [begin]
*-----------------------------------------------------------------------
IF laScrMode[4] OR llSwMode
  *-- Coding Extended size scale lines [begin]
  IF lnCodes > 0
    
    *E301209,1 Comment out the following lines because there is no generated codes now [Begin]
    *-- Note that : Function lfCoding fill array laCodes
    *--           : with approparate codes.
    *MAB DIMENSION laCodes[lnCodes]		&& Array that contain codes
    *-- 1 Dimension coding
    *MAB IF laData[3] = 1
      *-- Call coding function
      *MAB = lfCoding('laCodes',lnCodes) 
    *MAB ELSE	&& 2 or 3 Dimension coding
      *-- But all distinct combinations in array laDistrib 
      *-- to pass it to coding function
      *MAB SELECT DISTINCT cDim1+cDim2  ;
      *MAB FROM (lcScaline)             ; 
      *MAB INTO ARRAY laDistrib

      *-- Calculate largest combination length [begin]
      *-- to pass it to coding function
      *MAB SELECT (lcScaline)
      *MAB FOR lnI = 1 TO ALEN(laDistrib)
        *MAB COUNT TO lnX FOR cDim1+cDim2 == laDistrib[lnI]
        *MAB laDistrib[lnI] = lnX
        *MAB lnMax = IIF(lnI = 1,lnX,MAX(lnMax,lnX))
      *MAB ENDFOR
      *-- Calculate largest combination length [end]
      *MAB GO TOP
      *-- Call coding function
      *MAB = lfCoding('laCodes',lnCodes,'laDistrib',lnMax)
    *MAB ENDIF   
    *E301209,1 Comment out the following lines because there is no generated codes now [End  ]
    *-- Saving scales and N.R.F. Codes

    *MAB lnI   = 1  && E301209,1 No need to a counter

    SELECT (lcScaline)
    *-- Scan all temporary scale file records.
    SCAN
      lnCnt = lfCalCnt()	&& Number of sizes in scanned line.
      *-- lcScl_Desc : Hold Scale description
      lcScl_Desc = IIF(laData[3] = 1,"",            ;
                   IIF(laData[3] = 2,cDim1,         ;
                                     cDim1+"-"+cDim2))
      DO lpAddSz	&& Adding sizes and NRFs to master file.
      *mab lnI = lnI + 1     && E301209,1 No need to a counter
    ENDSCAN

    *-- if you find prepak lines in temporary prepak file,
    *-- add this lines to master file.

    *E301209,1 Change seek condition to check eof [Begin]
    *IF SEEK(ladata[1]+&lcPrParent..Prepak,lcPrParent)
    GO TOP IN (lcPrParent)
    IF !EOF(lcPrParent)
    *E301209,1 Change seek condition to check eof [End  ]

      SELECT (lcPrParent)
      *-- Scan all temporary parent prepak file records.
      SCAN
        *lnI   = 1     && E301209,1 No need to a counter
        SELECT (lcPrChild)
        *-- Scan all temporary child prepak file records.
      
        *E301209,1 Adjust scan condition to meet new index [Begin]
        *SCAN FOR ALLTRIM(Scale)+Prepak = ladata[1]+&lcPrParent..Prepak
        SCAN FOR Prepak = &lcPrParent..Prepak
        *E301209,1 Adjust scan condition to meet new index [End  ]

          DO lpAddPp         && Adding prepaks to master file.
          *lnI = lnI + 1     && E301209,1 No need to a counter
        ENDSCAN
      
      ENDSCAN  
    ENDIF
  ENDIF
  *-- Coding Extended size scale lines [end]

ELSE	&& 	-- < EDIT MODE > -- 

  *-- Create codes for scale lines in extendes size scale case.
  *E301209,1 Comment out the following block because now scale code is enterd by user [Begin]
  *MAB IF lnAdd != 0 AND llExtSize
    *-- llRetFrom : When it is .T. it means that return from saving procedure
    *--           : because of you reach maximum line numbers.
    *-- llSame    : When it is .T. it means that the previous line is in
    *--           : the same combination of the current line. 
    *-- llStatus  : When it is .T. it means that the previous line status
    *--           : not equal current line status (i.e: Not "A")
    *-- lnEqStCode: Hold Start position value of the gap
    *-- lnEqEdCode: Hold end   position value of the gap
    *--           : Example : The following astric lines is the gap width
    *--           : ******************   <-- lnEqStCode
    *--           : ******************
    *--           : ******************
    *--           : ******************
    *--           : ******************
    *--           : ******************
    *--           : ******************
    *--           : ******************    <-- lnEqEdCode
    *MAB HERE
    *MAB llRetFrom  = .F.
    *MAB llSame     = .F. 
    *MAB llStatus   = .T.
    *MAB lnEqStCode = 1
    *MAB lnI        = 1 
    *-- Coding Edit Mode lines.
    *-- Note that : In this case we replace our codes in temp. Scale File.
    *MAB DO lpEdCode
    *MAB IF llRetFrom
    *MAB   DO lpDontSave
    *MAB   RETURN
    *MAB ENDIF
  *MAB ENDIF
  *E301209,1 Comment out the following block because now scale code is enterd by user [End  ]
    
  *-- Deal with modified or deleted lines in temp. scale file.
  IF (lnDel + lnMod) != 0
    *-- Deleting deleted records from master and temp file
    SELECT (lcScaline)
    GO TOP
    SCAN FOR INLIST(cStatus,"M","D")
      SELECT SCALE
      = SEEK("S"+&lcScaline..Scale)

      *-- Modify scale records in master scale file
      IF &lcScaline..cStatus = "M"
        lnCnt = lfCalCnt()
        DO lpReplace
      ELSE	&& Delete scale records from master scale file
        
        *E300872,6 Update Trace File for SCALE file, delete record [begin]
        *E300872,6 Full index expr. : Type(1)+Scale(3)+Prepak(1).  
        = gfTraceKey('SCALE',"S"+PADR(laData[1],3)+SCALE.PREPAK,'D')
        *E300872,6 Update Trace File for SCALE file, delete record [end]
        *C200597,1  TMI [Start]  Call a customized tracekey function for Bong Wha
        IF ASCAN(laEvntTrig , PADR('ICSCALE2SD',10)) <> 0
          =gfDoTriger('ICSCALE',PADR('ICSCALE2SD',10))
        ENDIF
        *C200597,1  TMI [End  ] 

        BLANK
        DELETE
        IF SEEK("P"+&lcScaline..Scale)

          *MAB E300872,6 BLANK FOR TYPE+SCALE+PREPAK = "P"+&lcScaline..Scale

          *E300872,6 Update Trace File for SCALE file, delete records [begin]
          *E300872,6 Full index expr. : Type(1)+Scale(3)+Prepak(1).  
          SCAN REST FOR (TYPE+SCALE+PREPAK = "P"+&lcScaline..Scale)
            = gfTraceKey('SCALE',"P"+SCALE.SCALE+SCALE.PREPAK,'D')
            *C200597,1  TMI [Start]  Call a customized tracekey function for Bong Wha
            IF ASCAN(laEvntTrig , PADR('ICSCALE_PD',10)) <> 0
              =gfDoTriger('ICSCALE',PADR('ICSCALE_PD',10))
            ENDIF
            *C200597,1  TMI [End  ] 
            BLANK
            DELETE
          ENDSCAN
          
          *E300872,6 Update Trace File for SCALE file, delete record [end]

          *MAB E300872,6 DELETE FOR EMPTY(TYPE+SCALE+PREPAK)
        ENDIF
        SELECT (lcScaline)
      ENDIF  
    ENDSCAN
  ENDIF

  *-- Add new record(S)
  IF lnAdd != 0  
    *-- Add scale lines to scale file in edit mode.
    SCAN FOR cStatus = "A"
      lnCnt = lfCalCnt()
      IF llExtSize
        lcScl_Desc = IIF(laData[3] = 1,"",            ;
                     IIF(laData[3] = 2,cDim1,         ;
                                       cDim1+"-"+cDim2))
      ENDIF
      DO lpAddSz 
    ENDSCAN
  ENDIF

  *-- If there is new prepak lines.
  IF (lnAddPp+lnDelPp + lnModPp) != 0
    SELECT (lcPrChild)
    *-- Modify prepak records in master scale file
    IF lnModPp > 0
      SCAN FOR &lcPrChild..cStatus = "M"
        SELECT SCALE
        = SEEK("P"+&lcPrChild..Scale+&lcPrChild..Prepak,'SCALE')
        DO lpRepPps	  && Replace existing lines.
      ENDSCAN
    ENDIF
    *-- Delete prepak records from master scale file
    IF lnDelPp > 0
      SCAN FOR &lcPrChild..cStatus = "D"
        SELECT SCALE
        = SEEK("P"+&lcPrChild..Scale+&lcPrChild..Prepak,'SCALE')

        *E300872,6 Update Trace File for SCALE file, delete record [begin]
        *E300872,6 Full index expr. : Type(1)+Scale(3)+Prepak(1).  
        = gfTraceKey('SCALE',"P"+&lcPrChild..Scale+&lcPrChild..Prepak,'D')
        *E300872,6 Update Trace File for SCALE file, delete record [end]
        *C200597,1  TMI [Start]  Call a customized tracekey function for Bong Wha
        IF ASCAN(laEvntTrig , PADR('ICSCALE2PD',10)) <> 0
          =gfDoTriger('ICSCALE',PADR('ICSCALE2PD',10))
        ENDIF
        *C200597,1  TMI [End  ] 

        BLANK
        DELETE
      ENDSCAN
    ENDIF
    *-- Add prepak lines to master scale file
    IF lnAddPp > 0
      SCAN FOR &lcPrChild..cStatus = "A"
        SELECT SCALE
        *-- If extended size scale you must find the scale line
        *-- equavelent to our prepak line to save its code in prepak line.
        IF llExtSize
          *-- if length of lcPrChild..Scale = 3 this means that 
          *-- it's new prepak for already existing scale otherwise

         *E301209,1 Seek scale in prepak file instead of check for its length [Begin]
          *MAB IF (LEN(ALLTRIM(&lcPrChild..Scale)) = 3)
          = SEEK("S"+ALLTRIM(&lcPrChild..Scale),'SCALE')
         *E301209,1 Seek scale in prepak file instead of check for its length [End  ]

         *E301209,1 Also no need to else condition just figure this prepak master scale[Begin]
          *MAB ELSE	       && it's new prepak for new scale

            *MAB LOCATE FOR SCALE.cDim1+SCALE.cDim2+SCALE.Sz1 == ;
                       &lcPrChild..cDim1+&lcPrChild..cDim2+&lcPrChild..Sz1
          *MAB ENDIF
         *E301209,1 Also no need to else condition just figure this prepak master scale[End  ]

        ENDIF
        DO lpAddPp
      ENDSCAN
    ENDIF
    SELECT SCALE
  ENDIF
  GO TOP IN (lcScaline)
ENDIF
*-----------------------------------------------------------------------
*-- Saving new data [end]

*-- Saving data in master scalehd file.
SELECT ScaleHd
*-- if extended size scale and you are in add mode.
IF laScrMode[4]
  APPEND BLANK

  *E300872,6 Update Trace File for SCALEHD file, add record [begin]
  *E300872,6 Full index expr. : cExtScale(1).  
  = gfTraceKey('SCALEHD',ladata[1],'A')
  *E300872,6 Update Trace File for SCALEHD file, add record [end]
ELSE
  *E300872,6 Update Trace File for SCALEHD file, add record [begin]
  *E300872,6 Full index expr. : cExtScale(1).  
  = gfTraceKey('SCALEHD',ladata[1],'M')
  *E300872,6 Update Trace File for SCALEHD file, add record [end]
ENDIF  
GATHER FIELDS &lcScFields MEMO FROM laData
REPLACE nNOfCodes WITH lnCodes		&& New Number of codes.
= gfAdd_Info('SCALEHD')
*B802855,1 KAM 15/02/2001 [start]
*-- check for extend size scale company and in edit mode
IF llExtSize AND laScrMode[3] 
*--call function to check if new set of size found or not     
  IF lfInlcscal() 
    *--display message for use confirmation  
    lnChoice= gfModalGen('QRM42220B42002','Dialog') 
    *-- lnchoice=1 means the user accept to update styles 
    IF lnChoice=1
      *--call function to automatic update
      =lfStyUpdt()
    ENDIF  
  ENDIF  
ENDIF  
*B802855,1 KAM [end]
SELECT (lnAlias)
*-- end of lpSavScr .

*!*************************************************************
*! Name      : lpDontSave
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/05/98
*! Purpose   : Suspend saving until user adjust some thing not valid. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpDontSave
*!*************************************************************
PROCEDURE lpDontSave
llCSave = .F.
_CUROBJ = OBJNUM(ibTrap1)
SELECT (lnAlias)
*-- end of lpDontSave.

*!*************************************************************
*! Name      : lfCoding
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/20/97
*! Purpose   : This Function is to calculate codes and gaps 
*!           : Extended sizes, Add mode only.  
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcCodeArr , lnItems , lcLoopArr , lnMaxiumum
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCoding()
*!*************************************************************
*! Notes     : 1- Case 1 dimension pass the following parameters
*!           :    - Name of array that hold codes
*!           :    - Number of lines (codes) <--- length of array 
*!           : 2- Case 2 or 3 dimension pass the following parameters
*!           :    - Name of array that hold codes
*!           :    - Number of lines (codes) <--- length of array 
*!           :    - Name of array that hold different combinations (loop array)
*!           :    - highest number of lines in loop array
*!*************************************************************
FUNCTION lfCoding
PARAMETERS lcCodeArr , lnItems , lcLoopArr , lnMaxiumum

PRIVATE lnLoop
lnLoop = 0

*-- Checks to know that passed parameters type is valid. [begin]

*-- Type of code array must be a character.
IF TYPE('lcCodeArr') != 'C'
  RETURN
ENDIF  
*-- No. OF Codes must be an integer greater than Zero.
IF (TYPE('lnItems') != 'N') OR ((TYPE('lnItems') = 'N') AND (lnItems <= 0))
  RETURN
ENDIF  
*-- Type of loop array must be a character or ommited.
IF !(TYPE('lcLoopArr') $ 'LC')
  RETURN
ENDIF  
*-- Checks to know that passed parameters type is valid. [end]

*-- looding variables used in calculate Codes.
DO CASE
  *-- Case 1 Dimension1
  CASE TYPE('lcLoopArr') $ 'UL'
    lnLoop = lnItems
    lnNoOfGaps = 1   	&& No Of Gaps
    lnGapWidth = 0	    && Gap width, and must be 0 because we have no gaps. 
  OTHERWISE
    *-- We have 2 big gaps, one at top and the other at bottom
    *-- and the width of this two gaps is twice the width of a normal gap
    *-- we make this two big gaps because of the propability of enter new
    *-- combination less or greater than the found in file is greater than
    *-- other propabilities. 
    lnNoOfGaps = ALEN(&lcLoopArr) + 3 
    lnGapWidth = INT((36 * 36)/lnNoOfGaps) - lnMaxiumum		
    *-- the following is to avoiding overflow comes when number of
    *-- lines equal or slitely equal maximum number of codes.
    lnGapWidth = IIF(lnGapWidth < 0 , 0 , lnGapWidth) 
ENDCASE

*-- Calculate Codes
lnCalCode = 1 + lnGapWidth  		&&Initiall value to calculate code
lnStep    = 1

FOR lnI = 1 TO IIF(lnNoOfGaps = 1 , 1 , (lnNoOfGaps - 3))

  *-- First time lnCalCode equal one and in every combination you loop
  *--  the outer loop increasing it by gap width  .
  lnCalCode = lnCalCode + lnGapWidth
  
  *-- if lcLoopArr is found it means that your case is either 
  *-- 2 or 3 dimensions, thus you must loop the inner loop  
  *-- until finishing combinations, before loop in the outer loop .
  IF TYPE('lcLoopArr') = 'C'
    lnLoop = &lcLoopArr[lnI]
  ENDIF  
 
  *-- if 1 dimension lnLoop = 1 and this means that no loops
  *-- thus there is no gaps
  FOR lnJ = 1 TO lnLoop

    lnPosInFir = lnCalCode - 36 * INT(lnCalCode/36)
    IF lnPosInFir = 0
      lnPosInFir = 36
    ENDIF  
    *-- Using CHR() and STR() functions is the easy way to generate 
    *-- sequntial codes in the range
    *-- (00,01,02,.......,98,99,.....AA,AB,..........,ZY,ZZ) 
    &lcCodeArr[lnStep] = IIF(CEILING(lnCalCode/36) <= 10 ,;
                             ALLTRIM(STR(CEILING(lnCalCode/36)-1))  ,;
                             CHR(54+CEILING(lnCalCode/36)))         +;
                         IIF(lnPosInFir <= 10                       ,;
                             ALLTRIM(STR(lnPosInFir-1))             ,;
                             CHR(54+lnPosInFir))                              
    lnStep = lnStep + 1
    *-- Every time you loop in inner loop lnCalCode increase by one
    *-- because we have sequential codes in the same combination.
    lnCalCode = lnCalCode + 1

  ENDFOR

ENDFOR
* end of lfCoding.

*!*************************************************************
*! Name      : lfCalCnt
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/05/98
*! Purpose   : This Function calculate number of sizes in the line
*!           : Both extended and single size scale, add and edit modes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Numeric [Number of sizes]
*!*************************************************************
*! Example   : = lfCalCnt()
*!*************************************************************
FUNCTION lfCalCnt
RETURN (  IIF(!EMPTY(&lcScaline..Sz8),8,;
          IIF(!EMPTY(&lcScaline..Sz7),7,;
          IIF(!EMPTY(&lcScaline..Sz6),6,;
          IIF(!EMPTY(&lcScaline..Sz5),5,;
          IIF(!EMPTY(&lcScaline..Sz4),4,;
          IIF(!EMPTY(&lcScaline..Sz3),3,;
          IIF(!EMPTY(&lcScaline..Sz2),2, 1))))))) )
*-- end of lfCalCnt.

*!*************************************************************
*! Name      : lpAddSz
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/05/98
*! Purpose   : This Function add new line to master file for every 
*!           : new scale code.
*!           : Both extended and single size scale, add and edit modes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpAddSz
*!*************************************************************
PROCEDURE lpAddSz
*
*E301209,1 In all cases replace scale with temporary scale file value [Begin]
*INSERT INTO ("SCALE")                            ;
*         (Type,Cnt,                              ;
*         Scale,                                  ;
*         cDim1,cDim2,                            ;
*         cScl_Desc,                              ; 
*         Sz1,Sz2,                                ;
*         Sz3,Sz4,                                ;
*        Sz5,Sz6,                                ;
*        Sz7,Sz8 )                               ;
*VALUES ("S" ,lnCnt,                             ;
*        IIF(laScrMode[4] OR llSwMode,laData[1]+ ;
*        laCodes[lnI], &lcScaline..Scale),       ;
*        &lcScaline..cDim1,&lcScaline..cDim2,    ;
*        IIF(llExtSize,lcScl_Desc,               ;
*        &lcScaline..cScl_Desc) ,                ;
*        &lcScaline..Sz1,&lcScaline..Sz2,        ;
*        &lcScaline..Sz3,&lcScaline..Sz4,        ;
*        &lcScaline..Sz5,&lcScaline..Sz6,        ;
*        &lcScaline..Sz7,&lcScaline..Sz8)     

INSERT INTO ("SCALE")                            ;
         (Type,Cnt,                              ;
         Scale,                                  ;
         cDim1,cDim2,                            ;
         cScl_Desc,                              ; 
         Sz1,Sz2,                                ;
         Sz3,Sz4,                                ;
         Sz5,Sz6,                                ;
         Sz7,Sz8 )                               ;
 VALUES ("S" ,lnCnt,                             ;
         &lcScaline..Scale,  					 ;
         &lcScaline..cDim1,&lcScaline..cDim2,    ;
         IIF(llExtSize,lcScl_Desc,               ;
         &lcScaline..cScl_Desc) ,                ;
         &lcScaline..Sz1,&lcScaline..Sz2,        ;
         &lcScaline..Sz3,&lcScaline..Sz4,        ;
         &lcScaline..Sz5,&lcScaline..Sz6,        ;
         &lcScaline..Sz7,&lcScaline..Sz8)     
*E301209,1 In all cases replace scale with temporary scale file value [End  ]

SELECT SCALE
REPLACE cNrfCode1 WITH &lcScaline..cNrfCode1 ,;
        cNrfCode2 WITH &lcScaline..cNrfCode2 ,;  
        cNrfCode3 WITH &lcScaline..cNrfCode3 ,;  
        cNrfCode4 WITH &lcScaline..cNrfCode4 ,;  
        cNrfCode5 WITH &lcScaline..cNrfCode5 ,;  
        cNrfCode6 WITH &lcScaline..cNrfCode6 ,;  
        cNrfCode7 WITH &lcScaline..cNrfCode7 ,;  
        cNrfCode8 WITH &lcScaline..cNrfCode8 

*E301209,1 Add scale audit fields
= gfAdd_Info('SCALE')

*E300872,6 Update Trace File for SCALE file, add record [begin]
*E300872,6 Full index expr. : Type(1)+Scale(3)+Prepak(1).  
= gfTraceKey('SCALE',SCALE.TYPE+SCALE.Scale+SCALE.Prepak,'A')
*C200597,1  TMI [Start]  Call a customized tracekey function for Bong Wha
IF ASCAN(laEvntTrig , PADR('ICSCALE_A',10)) <> 0
  =gfDoTriger('ICSCALE',PADR('ICSCALE_A',10))
ENDIF
*C200597,1  TMI [End  ] 
*E300872,6 Update Trace File for SCALE file, add record [end]
*-- end of lpAddSz. 

*!*************************************************************
*! Name      : lpReplace
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/05/98
*! Purpose   : This Function edit existing line in master file 
*!           : for changes made to temporary scale lines.
*!           : Both extended and single size scale, edit mode.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpReplace
*!*************************************************************
PROCEDURE lpReplace
=RLOCK()
REPLACE Scale.Cnt WITH lnCnt                 ,;
        Scale.Sz1 WITH &lcScaline..Sz1       ,;
        Scale.Sz2 WITH &lcScaline..Sz2       ,;
        Scale.Sz3 WITH &lcScaline..Sz3       ,;
        Scale.Sz4 WITH &lcScaline..Sz4       ,;
        Scale.Sz5 WITH &lcScaline..Sz5       ,;
        Scale.Sz6 WITH &lcScaline..Sz6       ,;
        Scale.Sz7 WITH &lcScaline..Sz7       ,;
        Scale.Sz8 WITH &lcScaline..Sz8

REPLACE cNrfCode1 WITH &lcScaline..cNrfCode1 ,;
        cNrfCode2 WITH &lcScaline..cNrfCode2 ,;  
        cNrfCode3 WITH &lcScaline..cNrfCode3 ,;  
        cNrfCode4 WITH &lcScaline..cNrfCode4 ,;  
        cNrfCode5 WITH &lcScaline..cNrfCode5 ,;  
        cNrfCode6 WITH &lcScaline..cNrfCode6 ,;  
        cNrfCode7 WITH &lcScaline..cNrfCode7 ,;  
        cNrfCode8 WITH &lcScaline..cNrfCode8 
IF !llExtSize
  REPLACE cScl_Desc WITH lcScl_Desc
ENDIF
= gfAdd_Info('SCALE')
UNLOCK

*E300872,6 Update Trace File for SCALE file, modify record [begin]
*E300872,6 Full index expr. : Type(1)+Scale(3)+Prepak(1).  
= gfTraceKey('SCALE',SCALE.TYPE+SCALE.Scale+SCALE.Prepak,'M')
*C200597,1  TMI [Start]  Call a customized tracekey function for Bong Wha
IF ASCAN(laEvntTrig , PADR('ICSCALE_M',10)) <> 0
  =gfDoTriger('ICSCALE',PADR('ICSCALE_M',10))
ENDIF
*C200597,1  TMI [End  ] 
*E300872,6 Update Trace File for SCALE file, modify record [end]

*-- end of lpReplace.

*!*************************************************************
*! Name      : lpEdCode
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : This procedure is to scan around new records to 
*!           : generate new codes in the available gaps.
*!           : Extended size scale, edit mode.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpEdCode
*!*************************************************************
PROCEDURE lpEdCode
SELECT (lcScaline)
SCAN FOR cStatus != "D"
  IF (cStatus = "A")
    lnThRec = RECNO(lcScaline)
    *-- If it is the first add line llStatus must be true [begin]
    *-- thus skip the following lines to avoid go to BOF(),
    *-- and llSame flag does not matter in this case. 
    IF (lnI != 1)
      lcScaled = cDim1 + cDim2
      lcSt = cStatus
      SKIP -1
      llSame     = ((cDim1 + cDim2) = lcScaled)	&&Described in lpSavScr
      llStatus   = (cStatus != lcSt)			&&Described in lpSavScr
      *-- if it's different combination or 
      *-- the same combination but not the same status as the previous line
      *-- recalculate number equavelent to start code.
      IF !llSame OR (llSame AND llStatus)		
        lnEqStCode = lfEqCode(&lcScaline..Scale) &&Calculate number equavelent to start code.
      ENDIF
      SKIP
    ENDIF
    *-- If it is the first add line llStatus must be true [end]

    IF llStatus
      LOCATE REST FOR cStatus != "D"
      DO WHILE .T.
        IF EOF()
          lnEqEdCode = 36 * 36
          EXIT		&&Exit do while loop.
        ENDIF
        lcCalcs = scale
        IF cStatus = "A" AND EMPTY(scale)
            CONTINUE
        ELSE
          lnEqEdCode = lfEqCode(lcCalcs)  &&Calculate number equavelent to end code.
          EXIT
        ENDIF
      ENDDO  
      GO lnThRec IN (lcScaline)
    ENDIF

    *-- Calculate number equavelent to generated code.
    *-- if it's new combination it's in middle of the gap to have new gaps.
    lnGenCode = IIF(llSame,IIF(llStatus,lnEqStCode+1,lnGenCode+1)   ,;
                           lnEqStCode+INT((lnEqEdCode - lnEqStCode)/2))

    IF (lnEqEdCode = lnGenCode)
      *-- The following message is 
      *-- You have reach maximum code in scale XXX, Cannot proceed..
      *--                 < Ok >
      = gfModalGen('TRM42118B42000','Dialog',ladata[1])  
      llRetFrom = .T.  
      EXIT		&&Exit scan loop.
    ENDIF  
        
    *-- lnRightNum : Hold number equavelent to right number of to character codes.
    lnRightNum = lnGenCode - 36 * INT(lnGenCode/36)
    IF lnRightNum = 0
      lnRightNum = 36
    ENDIF  

    *-- Calculate code .
    lcGenCode = IIF(INT(lnGenCode/36) <= 10                 ,;
                    ALLTRIM(STR(INT(lnGenCode/36)-1))      ,;
                    CHR(54+INT(lnGenCode/36)))               +;
                IIF(lnRightNum <= 10                       ,;
                    ALLTRIM(STR(lnRightNum-1))             ,;
                    CHR(54+lnRightNum))                              
        
    *-- Replace code in temporary scale file.
    REPLACE Scale WITH laData[1] + lcGenCode
  ENDIF  
  lnI = lnI + 1 
ENDSCAN
*-- end of lpEdCode.

*!*************************************************************
*! Name      : lfEqCode
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/25/98
*! Purpose   : Masking code and discrete it to its two characters
*!           : then returns the value of corressponding two numbers.
*!           : Extended size scale, edit mode.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfCalVal
*!*************************************************************
*! Passed Parameters  : lcPassedCd (Three character code)
*!*************************************************************
*! Returns            : Numirec (Number eqavelent to calculate code)
*!*************************************************************
*! Example   : = lfEdCode()
*!*************************************************************
FUNCTION lfEqCode
PARAMETERS lcPassedCd

lcPassedCd = RIGHT(lcPassedCd,2)
lcRight    = RIGHT(lcPassedCd,1)
lcLeft     = LEFT (lcPassedCd,1)
lnRight = lfCalVal(lcRight)
lnLeft  = lfCalVal(lcLeft )
RETURN(lnLeft * 36 + lnRight)
*-- end of lfEqCode.

*!*************************************************************
*! Name      : lfCalVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/25/98
*! Purpose   : This Function is to calculate equavilant value for 
*!           : coressponding code.
*!           : Extended size scale, edit mode.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfCalVal
*!*************************************************************
*! Passed Parameters  : lcPassedCd (Three character code)
*!*************************************************************
*! Returns            : Numirec (Number eqavelent to calculate code)
*!*************************************************************
*! Example   : = lfCalVal()
*!*************************************************************
FUNCTION lfCalVal
PARAMETERS lcCalc
RETURN (IIF(BETWEEN(lcCalc,"0","9"),VAL(lcCalc)+1,ASC(lcCalc)-54))
*-- end of lfCalVal.

*!*************************************************************
*! Name      : lpAddPp
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/05/98
*! Purpose   : This Function add new line to master file
*!           : for every new prepak code.
*!           : Both extended and single size scale, edit mode.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpAddPp
*!*************************************************************
PROCEDURE lpAddPp
IF llExtSize
  *-- calculate total prepak for every line.
  lnTotalPp = &lcPrChild..Pp1+&lcPrChild..Pp2+&lcPrChild..Pp3 +;
              &lcPrChild..Pp4+&lcPrChild..Pp5+&lcPrChild..Pp6 +;
              &lcPrChild..Pp7+&lcPrChild..Pp8
  IF laScrMode[3]
    lcThiScale = SCALE.Scale
  ENDIF
ELSE	&& Single size scale
  lnTotalPp  = &lcPrChild..PpTot
  lcThiScale = &lcPrChild..Scale
ENDIF  

*-- Add new line to master scale file.

*E301209,1 In all cases replace scale with temporary scale file value [Begin]
*INSERT INTO ("SCALE")         ;
*        (Type,Scale,Prepak   ,;
*         cDim1,cDim2         ,;
*         Pp1,Pp2             ,;
*         Pp3,Pp4             ,;
*         Pp5,Pp6             ,;
*         Pp7,Pp8             ,;
*         PpTot               );
* VALUES ("P" ,                                     ;
*         IIF(laScrMode[4] OR llSwMode,laData[1]+   ;
*         laCodes[lnI],lcThiScale),                 ;
*         &lcPrChild..Prepak                       ,;
*         &lcPrChild..cDim1,&lcPrChild..cDim2      ,;
*         &lcPrChild..Pp1,&lcPrChild..Pp2          ,;
*         &lcPrChild..Pp3,&lcPrChild..Pp4          ,;
*         &lcPrChild..Pp5,&lcPrChild..Pp6          ,;
*         &lcPrChild..Pp7,&lcPrChild..Pp8          ,;
*         lnTotalPp                                )

INSERT INTO ("SCALE")         ;
        (Type,Scale,Prepak   ,;
         cDim1,cDim2         ,;
         Pp1,Pp2             ,;
         Pp3,Pp4             ,;
         Pp5,Pp6             ,;
         Pp7,Pp8             ,;
         PpTot               );
 VALUES ("P" ,                                     ;
         &lcPrChild..Scale                        ,;
         &lcPrChild..Prepak                       ,;
         &lcPrChild..cDim1,&lcPrChild..cDim2      ,;
         &lcPrChild..Pp1,&lcPrChild..Pp2          ,;
         &lcPrChild..Pp3,&lcPrChild..Pp4          ,;
         &lcPrChild..Pp5,&lcPrChild..Pp6          ,;
         &lcPrChild..Pp7,&lcPrChild..Pp8          ,;
         lnTotalPp                                )
*E301209,1 In all cases replace scale with temporary scale file value [End  ]

*E301209,1 Add scale audit fields.
= gfAdd_Info('SCALE')

*E300872,6 Update Trace File for SCALE file, add record [begin]
*E300872,6 Full index expr. : Type(1)+Scale(3)+Prepak(1).  
= gfTraceKey('SCALE',SCALE.TYPE+SCALE.Scale+SCALE.Prepak,'A')
*C200597,1  TMI [Start]  Call a customized tracekey function for Bong Wha
IF ASCAN(laEvntTrig , PADR('ICSCALE_A',10)) <> 0
  =gfDoTriger('ICSCALE',PADR('ICSCALE_A',10))
ENDIF
*C200597,1  TMI [End  ] 
*E300872,6 Update Trace File for SCALE file, add record [end]

*-- end of lpAddPp.

*!*************************************************************
*! Name      : lpRepPps
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/05/98
*! Purpose   : This Function edit existing line in master file
*!           : for changes made to temporary prepak lines.
*!           : Both extended and single size scale, edit mode.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpRepPps
*!*************************************************************
PROCEDURE lpRepPps
lnTotalPp = &lcPrChild..Pp1+&lcPrChild..Pp2+&lcPrChild..Pp3 +;
            &lcPrChild..Pp4+&lcPrChild..Pp5+&lcPrChild..Pp6 +;
            &lcPrChild..Pp7+&lcPrChild..Pp8
=RLOCK()
REPLACE Scale.Pp1   WITH &lcPrChild..Pp1 ,;
        Scale.Pp2   WITH &lcPrChild..Pp2 ,;
        Scale.Pp3   WITH &lcPrChild..Pp3 ,;
        Scale.Pp4   WITH &lcPrChild..Pp4 ,;
        Scale.Pp5   WITH &lcPrChild..Pp5 ,;
        Scale.Pp6   WITH &lcPrChild..Pp6 ,;
        Scale.Pp7   WITH &lcPrChild..Pp7 ,;
        Scale.Pp8   WITH &lcPrChild..Pp8 ,;
        Scale.PpTot WITH lnTotalPp

= gfAdd_Info('SCALE')
UNLOCK
*-- end of lpRepPps.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/05/98
*! Purpose   : Delete Procedure.
*!           : Extended size scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfInStyle
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpDelScr
*!*************************************************************
PROCEDURE lpDelScr
*-- If this scale does not allocated to any style, delete it. 
IF !lfInStyle()
  lnUsing = SELECT(0) 

  *-- delete scale line form scale file.
  SELECT SCALE


  *E300872,6 Update Trace File for SCALE file, delete records [begin]
  *E300872,6 Full index expr. : Type(1)+Scale(3)+Prepak(1).  
  *BLANK FOR (TYPE+SCALE+PREPAK = "S"+SCALEHD.cExtScale) OR (TYPE+SCALE+PREPAK = "P"+SCALEHD.cExtScale)
  *DELETE FOR EMPTY(TYPE+SCALE+PREPAK)
   
  *E301209,1 Scan proper length [Begin]
  *SCAN FOR (TYPE+SCALE+PREPAK = "S"+SCALEHD.cExtScale)  OR ;
  *         (TYPE+SCALE+PREPAK = "P"+SCALEHD.cExtScale)
  SCAN FOR (TYPE+SCALE+PREPAK = "S"+PADR(SCALEHD.cExtScale,lnScaleLen))  OR ;
           (TYPE+SCALE+PREPAK = "P"+PADR(SCALEHD.cExtScale,lnScaleLen))
  *E301209,1 Scan proper length [End  ]

    = gfTraceKey('SCALE',SCALE.TYPE+SCALE.SCALE+SCALE.PREPAK,'D')
    *C200597,1  TMI [Start]  Call a customized tracekey function for Bong Wha
    IF ASCAN(laEvntTrig , PADR('ICSCALE_D',10)) <> 0
      =gfDoTriger('ICSCALE',PADR('ICSCALE_D',10))
    ENDIF
    *C200597,1  TMI [End  ] 
    BLANK
    DELETE           
  ENDSCAN
  *E300872,6 Update Trace File for SCALE file, delete records [end]
   


  *-- delete scale from scale header file.
  SELECT SCALEHD

  *E300872,6 Update Trace File for SCALEHD file, delete record [begin]
  *E300872,6 Full index expr. : cExtScale(1).  
  = gfTraceKey('SCALEHD',SCALEHD.cExtScale,'D')
  *E300872,6 Update Trace File for SCALEHD file, delete record [end]

  BLANK
  DELETE

  SELECT(lnUsing)

  *-- Switch screen mode to select mode.
  laScrMode = .F.
  laScrMode[1] = .T.
  SHOW GETS
ENDIF  
*-- end of lpDelScr.

*----------------------------------------------------------
*-- Validation section
*-- This section contains all Valid functions and procedures help it.
*----------------------------------------------------------
*!*************************************************************
*! Name      : lfvScale
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/25/97
*! Purpose   : This function is to valid extended size scale in ScaleHd File.
*!           : Extended size scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ariabrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvScale()
*!*************************************************************
FUNCTION lfvScale
IF MDOWN()
  RETURN
ENDIF  

PRIVATE llToBrow,llAddMode
STORE .F. TO llToBrow,llAddMode

*E301209,1 Check no. of enterd characters [Begin]
IF !EMPTY(laData[1]) AND LEN(ALLTRIM(laData[1])) < lnScaleLen AND !('?' $ laData[1])

  *Message : 42172, Scale ID should not be less than 2 characters.
  *Button  : 42015, <Browse>  <Reenter>
  IF gfModalGen('TRM42172B42015','ALERT','Scale ID') = 2
    _CUROBJ  = _CUROBJ
    RETURN
  ENDIF
  llBrowse = .T.    
ENDIF
*E301209,1 Check no. of enterd characters [End  ]

lnAlias = SELECT(0)
SELECT ScaleHd

*-- If come from BitMap button or the key is not empty.
IF llBrowse OR !EMPTY(laData[1]) 
  IF llBrowse OR ATC('?',laData[1]) != 0
    llBrowse  = .F. 		                && Disable flag to its intial state   
    llToBrow = .T.
  ELSE
    IF !SEEK(laData[1])
      *-- This message is "Scale XX not found in data file "
      *--       <  Add  > < Browse > < Reenter >
      lnSelect = gfModalGen('QRM00001B42003','Dialog',"Scale "+": "+laData[1])  
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
  *-- Browse scale file.
  =ariabrow('','Scales', gnbrfsrow1, gnbrfscol1,;
            gnbrfsrow2, gnbrfscol2, '','',;
            'cExtScale,cScaleDes,nNoofDim,cDim1Desc,cDim2Desc,cDim3Desc', ;
            'laTemp')
  IF !EMPTY(laTemp[1])
    =ACOPY(laTemp,laData) 
  ELSE
    laData[1] = ''
  ENDIF       
ENDIF

IF !EMPTY(laData[1])

  *E301209,1 Adjust laData[1]
  laData[1] = PADR(laData[1],lnScaleLen)

  laScrMode    = .F.
  IF llAddMode  && IF ADD MODE
    laScrMode[4] = .T.
  ELSE  && ELSE it is VIEW MODE
    laScrMode[2] = .T.
  ENDIF  
  SHOW GETS
ELSE  &&ELSE If ESCAPE pressed wait in your position
  _CUROBJ = _CUROBJ
ENDIF
SELECT(lnAlias)
*-- End of lfvScale

*!*************************************************************
*! Name      : lfvDims
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/25/97
*! Purpose   : This function is to valid multiple objects according to 
*!           : passed parameter, like this
*!           : lnCalls = 1 --> Validate Spinner       (ladata[3])
*!           : lnCalls = 2 --> Validate 1st dimension (ladata[4])
*!           : lnCalls = 3 --> Validate 2nd dimension (ladata[5])
*!           : lnCalls = 4 --> Validate 3rd dimension (ladata[6])
*!           : Extended size scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfInStyle
*!*************************************************************
*! Passed Parameters  : lnCalls, llFrmAuto
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDims()
*!*************************************************************
FUNCTION lfvDims
PARAMETERS lnCalls, llFrmAuto
*-- save current values and variables.
lcData     = SYS(18)
lcContent  = EVALUATE(SYS(18))

IF lnCalls = 0
  IF !BETWEEN(laData[3],1,3)
    *-- The following message is 
    *-- 'Valid range between 1 and 3 .'
    *--                  < Ok >
    = gfModalGen('TRM42119B42000','Dialog')  
    laData[3] = lcOldValue
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF

  IF laData[3] = lcOldValue
    llSameDim = .T.	&& Avoid screen flashing.
    RETURN
  ELSE
    *-- If the temp. scale file has data.
    IF RECCOUNT(lcScaline) != 0
      *-- check If you are in edit mode
      *-- and this scale allocated to any style return,otherwise
      *-- ask the user the message shared with the 
      *-- following condition which is 
      *-- 'You will change structure, thus you will lose your data,Are you sure.'
      *--                  < Yes >   < No >
      *-- and if he press < No > return
      IF (laScrMode[3] AND lfInStyle()) OR ;
          gfModalGen('QRM42105B42002','Dialog') = 2
        laData[3] = lcOldValue
        SHOW GET laData[3]
        RETURN 
      ELSE  && User press < Yes >
        lnSele = SELECT(0)
        *-- Clear data from scale file.
        SELECT (lcScaline)
        ZAP

        *-- Clear data from prepak files
        IF RECCOUNT(lcPrParent) > 0
          SELECT (lcPrParent)
          ZAP
          = lfActBrow2()
            
          SELECT (lcPrChild)
          ZAP
          = lfActBrow3()
        ENDIF  
        = lfwBrow3() 

        SELECT (lnSele) 
        llSwMode = laScrMode[3]	&& Change mode to Switch mode if you are in edit mode.	
      ENDIF
    ENDIF  
      
    =lfClsData()				&& Clear data.
    DO lpChanges				&& Make change to screen shape
    SHOW GET pbRemove  DISABLE 	&& Scale  folder <Remove> button initially disable.
    STORE 'DISABLE' TO lcRemoves

    *-- Enable/Disable <Auto> button [begin]
    llAutoSt = ((laData[3] = 2) AND !EMPTY(laData[4])  ;
                                AND !EMPTY(laData[5])) ;
            OR ((laData[3] = 3) AND !EMPTY(laData[4])  ;
                                AND !EMPTY(laData[5])  ;
                                AND !EMPTY(laData[6]))
    lcAutoEnDs = IIF(lnActFolder = 1 AND ladata[3] > 1 AND llAutoSt,;
                    "ENABLE","DISABLE") 

    *E301209,1 Temporary comment out the following until take descision about <Auto> button
    *SHOW GET pbAuto &lcAutoEnDs
    STORE lcAutoEnDs TO lcAutos
    *-- Enable/Disable <Auto> button [end]
 
    *-- Enable/Disable <New> button [begin]
    llNewSt = ((laData[3] = 1) AND !EMPTY(laData[4])) OR llAutoSt
    lcNewEnDs  = IIF(lnActFolder = 1 AND llNewSt,;
                    "ENABLE","DISABLE")
    SHOW GET pbNew &lcNewEnDs
    STORE lcNewEnDs TO lcNews     
    *-- Enable/Disable <New> button [end]

    llSameDim = .F.		&& No. of dimensions was changed.
  ENDIF

ELSE  && Validate 1st or 2nd or 3rd dimension
  *-- if you press mouse or the old value not empty and 
  *-- equal to current value and number of dimensions not change
  IF (!EMPTY(lcOldValue) AND (lcContent == lcOldValue) AND llSameDim) OR;
      MDOWN()
      IF EMPTY(lcContent)
        &lcData = lcOldValue
        SHOW GET &lcData 
      ENDIF  
    RETURN
  ENDIF  

  *-- You cannot clear data from any dimension title, only you can change 
  *-- its value.
  IF EMPTY(lcContent)
    &lcData = lcOldValue
    SHOW GET &lcData 
  ELSE		&& Title value was changed
    *-- llNwAutoSt : Flag used to enable and disable <New> and <Auto> buttons.
    llNwAutoSt = ((laData[3] = 1) AND !EMPTY(laData[4])) ;
              OR ((laData[3] = 2) AND !EMPTY(laData[4])  ;
                                  AND !EMPTY(laData[5])) ;
              OR ((laData[3] = 3) AND !EMPTY(laData[4])  ;
                                  AND !EMPTY(laData[5])  ;
                                  AND !EMPTY(laData[6]))
  ENDIF

  *-- if you don't come from wizard screen 
  *-- (Note: Wizard screen does not change No. of dimensions.)
  IF !llFrmAuto
    = lfRefresh(lcWinC12)

    SHOW GET lcGetDim1
    SHOW GET lcGetDim2

    IF ladata[3] = 3
      = lfRefresh(lcWinC14)
    ENDIF  
    = lfMoreStat()
    = lfVarBrow1()
    SHOW WINDOW (lcBrowTi1) REFRESH  &&Refresh title of scale browse
    SHOW WINDOW (lcBrowTi3) REFRESH  &&Refresh title of child prepak browse
  ENDIF
ENDIF

*-- If you have to change buttons state and not come from wizard screen.
IF llNwAutoSt AND !llFrmAuto
  llNwAutoSt = !llNwAutoSt		&& UnRise this flag
  IF lnActFolder = 1
    SHOW GET pbNew  ENABLE
    STORE 'ENABLE' TO lcNews
    lcAutoEnDs = IIF(ladata[3] > 1,"ENABLE","DISABLE") 

    *E301209,1 Temporary comment out the following until take descision about <Auto> button
    *SHOW GET pbAuto &lcAutoEnDs
    STORE lcAutoEnDs TO lcAutos
  ENDIF  
ENDIF
*-- end of lfvDims.

*!*************************************************************
*! Name      : lpChanges
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/25/97
*! Purpose   : This procedure is switch screen corressponding 
*!           : to changing number of dimensions.
*!           : Extended size scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpZooming
*!             Functions  : lfRefresh,lfMoreStat,lfActBrow1
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpChanges
*!*************************************************************
PROCEDURE lpChanges
DO CASE
  CASE laData[3] = 1
    STORE '' TO  laData[5],laData[6]
  CASE laData[3] = 2
    STORE '' TO  laData[6]
  CASE laData[3] = 3
    = lfRefresh(lcWinC14)
ENDCASE
= lfRefresh(lcWinCh0)
= lfRefresh(lcWinC12)

SHOW GET lcGetDim1
SHOW GET lcGetDim2

= lfMoreStat()

SHOW GET laData[4]
SHOW GET laData[5]
SHOW GET laData[6]

DO lpZooming
= lfActBrow1() 
*-- end of lpChanges.

*!*************************************************************
*! Name      : lfvPosDim1
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/27/97
*! Purpose   : Validation of popup holding data for first dimension.
*!           : Extended size scale, 3 dimension only .
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfwBrow1
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPosDim1()
*!*************************************************************
FUNCTION lfvPosDim1
*-- Refresh record pointer in scale browse when change 
*-- first dimension value (pick value from popup)
lcLocExp = IIF((laScrMode[3] OR laScrMode[4]),'&lcScaline..cDim1 = laDim1[lnPosDim1]',;
                             '(SCALE.cDim1 = laDim1[lnPosDim1]) AND  ;
                              (SCALE.SCALE = laData[1]) AND          ;
                              (SCALE.TYPE = "S")')
SELECT (lcUsedFile)
LOCATE FOR &lcLocExp
lnMarker = RECNO()
= lfwBrow1()
*-- end of lfvPosDim1.

*!*************************************************************
*! Name      : lfvNew
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/29/97
*! Purpose   : This is a valid function for <New> button, in scale folder,
*!           : and if there is no empty records it add one.  
*!           : Both extended and single sizes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSearchSy,lfWBrow1,lfAddSclLn
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNew()
*!*************************************************************
FUNCTION lfvNew
*-- Extended size scale and edit mode and find this scale 
*-- allocated to one or more style and answer the message with < No >
*-- thus you don't add size.
IF llExtSize AND laScrMode[3] AND lfSearchSy()
  RETURN
ENDIF

*-- check if it is an empty file.
IF RECCOUNT(lcScaline) > 0 AND !EOF(lcScaline) AND !EOF(lcScaline)
  SELECT (lcScaline)
  LOCATE FOR EMPTY(Sz1)
  *-- If you find empty record, fill it.
  IF FOUND()
    IF llExtSize
      llNewComb = .T.
      IF !EMPTY(lcTempCnt) AND USED(lcTempCnt)
       USE IN (lcTempCnt)
      ENDIF
      IF ladata[3] = 3
        lnPosDim1 = ASCAN(laDim1,CDim1)
        SHOW GET lnPosDim1
      ENDIF

      _CUROBJ = OBJNUM(lcGetDim1)

    ELSE	&& Single size scale
      _CUROBJ = OBJNUM(lcScale)
    ENDIF
    =lfWBrow1()
    RETURN
  ENDIF
ENDIF
llAddExt=.T.
=lfAddSclLn()
*-- end of lfvNew. _CUROBJ

*!*************************************************************
*! Name      : lfSearchSy
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/02/97
*! Purpose   : This function is check for the scale in style file 
*!           : in case of add new line, if found it give a message,
*!           : and if we press no it return .T. .  
*!           : Extended Size scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : llMakeRet (.T. OR .F.)
*!*************************************************************
*! Example   : = lfSearchSy()
*!*************************************************************
FUNCTION lfSearchSy
llMakeRet = .F.  
lnUsing = SELECT(0)
SELECT STYLE
LOCATE FOR STYLE.SCALE = &lcSeekPrep
*-- Message shared with the following condition is 
*-- 'This scale is assigned to one or more style, Add new.'
*--                  < Yes >   < No >
IF FOUND() AND ;
   (gfModalGen('QRM42125B42002','Dialog') = 2)
  llMakeRet = .T.  
ENDIF  
SELECT (lnUsing)
RETURN (llMakeRet)
*-- end of lfSearchSy.

*!*************************************************************
*! Name      : lfvGetDim
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/25/97
*! Purpose   : This function is to valid First and Second dimension values
*!           : according to passed parameter, like this
*!           : lnDimNum = 1 --> Validate 1st dimension (lcGetDim1)
*!           : lnDimNum = 2 --> Validate 2nd dimension (lcGetDim2)
*!           : Extended size scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpCheckDim
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnDimNum
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvGetDim()
*!*************************************************************
*! Note      : This two objects is the edit sizes region key. 
*!*************************************************************
FUNCTION lfvGetDim
PARAMETER lnDimNum
IF EMPTY(EVALUATE(SYS(18)))
  IF (ladata[3] = 3) AND (lnDimNum = 1)
    lcGetDim1 = lcOldValue
    SHOW GET lcGetDim1
  ENDIF
  RETURN
ENDIF  
*-- Note : At this point your you fill dimension description with data

*-- Valid lcGetDim1
IF lnDimNum = 1
  *-- Your case is 3 dimension case (You have popup in your screen)
  IF laData[3] = 3
    *-- Replace data to file and disable key button.
    REPLACE &lcScaline..cDim1 WITH lcGetDim1
    SHOW GET lcGetDim1 DISABLE
    
    *-- Fill POPUP with new data. [begin]
    *-- If it's first time you add data to array.
    IF EMPTY(laDim1[1])
      laDim1[1] = lcGetDim1
    ELSE		&&You already have data in popup array.
      *-- if first dimension not found in popup array.
      IF (ASCAN(laDim1,lcGetDim1) = 0)
        DIMENSION laDim1[ALEN(laDim1)+1]
        lnPosDim1 = ALEN(laDim1)
        IF laScrMode[3]
          REPLACE &lcScaline..cStatus WITH "A"
        ENDIF 
      ENDIF
      laDim1[lnPosDim1] = lcGetDim1
      = ASORT(laDim1)
      lnPosDim1 = ASCAN(laDim1,lcGetDim1)
    ENDIF
    *-- Fill POPUP with new data. [end]
    
    SHOW GET lnPosDim1 

  ELSE	&&Your case 2 dimensions. (lcGetDim2 not found in screen)

    llExitFn = .F. 
    DO lpCheckDim	&& Check for found before and replace field not found.
    IF llExitFn
      RETURN
    ENDIF
  ENDIF

ELSE		&& Valid lcGetDim2

  llExitFn = .F. 
  DO lpCheckDim  && Check for found before and replace field not found.
  IF llExitFn
    RETURN
  ENDIF
  _CUROBJ = OBJNUM(lcGetSz1)
  
ENDIF  
SHOW WINDOW (lcBrowTi1) REFRESH
*-- end of lfvGetDim.

*!*************************************************************
*! Name      : lpCheckDim
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/05/98
*! Purpose   : Check for foundation of dimension description in file before
*!           : and replace its field if it not found.
*!           : Extended size scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpCheckDim
*!*************************************************************
PROCEDURE lpCheckDim
lcGetDim   = SYS(18)
lcGetDimVl = EVALUATE(SYS(18))

lnPointer = RECNO()
lcLockCond = IIF(laScrMode[3],[cStatus != "D"],[.T.])
LOCATE FOR cDim1+cDim2 = lcGetDim1+lcGetDim2 AND &lcLockCond
*-- If you found this set of sizes key before.
IF FOUND()
  lcThisMess = ladata[1] + " : " + ALLTRIM(ladata[4]) +          ;
      IIF(lnDimNum = 1," = " + ALLTRIM(lcGetDim1),               ;
      " \ " + ALLTRIM(ladata[5]) + " = " + ALLTRIM(lcGetDim1) +  ;
      " \ " + ALLTRIM(lcGetDim2))
               
  *-- The following message is 
  *-- "Scale XXX is already exists."
  *--          < Ok > 
  = gfModalGen('TRM42103B42000','Dialog',lcThisMess)  
  &lcGetDim = lcOldValue	&& Restore old value
  SHOW GET &lcGetDim
  _CUROBJ = _CUROBJ
  GO lnPointer
  llExitFn = .T.
ELSE	&& You did not find this dimension before.
  GO lnPointer
  lcRepDimFd = IIF(lnDimNum = 1,"cDim1","cDim2")
  REPLACE (lcRepDimFd) WITH &lcGetDim
  SHOW GET &lcGetDim DISABLE
ENDIF
*-- end of lpCheckDim.

*!*************************************************************
*! Name      : lfvSize
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/25/97
*! Purpose   : Validate 8 Sizes in both Add and Remove,
*!           : also it calls functions to shift sizes and remove it .
*!           : Both Extended and single size scale .
*!*************************************************************
*! Calls     : 
*!             Procedures : lpValidSz,
*!             Functions  : lfInStyle,lfCalTotPp,lfDelSz,lfvRemove
*!                        : lfBlankEnd
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSize()
*!*************************************************************
FUNCTION lfvSize
IF (EVALUATE(SYS(18)) = lcOldValue)
  RETURN
ENDIF  

lnChild  = RECNO(lcPrChild)
*-- llPrepak : When it's .T. it means that there is prepaks for this scale
*--          : in this case we rise a flag to avoid multiple checking for prepaks. 

*E301209,1 Change parent condition [Begin]
*llPrepak = &lcSeekCond
llPrepak = IIF(llExtSize,!EOF(lcPrParent),&lcSeekCond)
*E301209,1 Change parent condition [End  ]

lcCurVar = ALLTRIM(SYS(18))		&& Current variable name.
lcSuffix    = RIGHT(lcCurVar,1) && Suffix after variable between ['1' and '8']

*-- Declare variables used in remove size validation [Begin] 
*-- Hold Size data.
lcCurFld    = "&lcScaline..Sz"+lcSuffix  && Current Size Field {Scale file}. 

*-- Hold NRF data
lcCurNRFv  = "lcGetNRF"+lcSuffix				&&Current NRF variable. 
lcCurNRFd  = "&lcScaline..cNrfCode"+lcSuffix	&&Current NRF Field. 

*-- Hold PREPAK data
lcCurPpSz   = "&lcPrChild..Sz"+lcSuffix    &&Current Size Field {prepak file}. 
lcCurPrep   = "&lcPrChild..Pp"+lcSuffix    &&Current Prepak Field . 

IF VAL(lcSuffix) < 8
  lcNextSfx   = ALLTRIM(STR(VAL(lcSuffix)+1))     && Suffix after next variable between ['2' and '8']
  lcNextFld   = "&lcScaline..Sz"+lcNextSfx        && Next size field {Scale file}
  lcNextNRFd  = "&lcScaline..cNrfCode"+lcNextSfx  && Next NRF field
  lcNextPpSz  = "&lcPrChild..Sz"+lcNextSfx        && Next size field {prepak file} 				
  lcNextPrep  = "&lcPrChild..Pp"+lcNextSfx        && Next prepak field {prepak file}
ENDIF  
*-- Declare variables used in remove size validation [End.] 

*-- if you delete contents of field, and    [begin]
*-- it's between Sz2 and Sz8 or it's Sz1 but the next field have data
*-- in brife you can say that it's a shift case.
IF EMPTY(EVALUATE(SYS(18))) AND ;
   ((VAL(lcSuffix) > 1)     OR  ;
   (VAL(lcSuffix) = 1)      AND ;
   !EMPTY(EVALUATE(lcNextFld))))

  lcScalMess = lfMessTitl()

  *-- check If you are in edit mode
  *-- and this scale allocated to any style return,otherwise
  *-- ask the user the message shared with the 
  *-- following condition which is 
  *-- 'Are you sure you want to delete Scale XXX.'
  *--               < Yes >   < No >
  *-- and if he press < No > return
  IF (laScrMode[3] AND lfInStyle()) OR ;
     (gfModalGen('QRM42126B42002','Dialog',lcScalMess) = 2)     
    &lcCurVar = lcOldValue
    SHOW GET &lcCurVar
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF

  *-- if you have prepak data you must collect it.
  IF llPrepak
    = lfCalTotPp()
    GO lnChild IN (lcPrChild)	&& Restore Child prepak record pointer. 
  ENDIF  

ENDIF
*-- if you delete contents of field, and    [end]

*-- Global Variables declaration for this function [Begin]
lnSelected = SELECT(0)
lcThisFld = "" 
llFindOne = .F.
*-- Global Variables declaration for this function [End.]

*-- if current field is empty
*-- Delete data from size 
IF EMPTY(EVALUATE(SYS(18)))
  *-- Complicated shift case [you are in size8 or in any size and the next size not empty]
  IF (lcSuffix = "8") OR ((lcSuffix != "8") AND !EMPTY(EVALUATE(lcNextFld)))
    =lfDelSz()		&& Delete this size and shift other sizes.
    RETURN
  ELSE   && Next Field is empty.
    *-- IF you are at Sz1, its the same case for remove.  
    IF lcSuffix = "1"
      *-- llShift : Rise this flag to avoid go to remove message and to 
      *--         : and to be in the approparate object.
      llShift = .T.
      = lfvRemove()	&& Call valid function for remove line.
      RETURN
    ELSE  && You are not at size 8, we only empty current field .
      *-- 1- empty current NRF field.
      REPLACE (lcCurNRFd) WITH EVALUATE(SYS(18))
      STORE EVALUATE(SYS(18)) TO &lcCurNRFv
      SHOW GET &lcCurNRFv
        
      *-- 2- empty current prepak field(S).
      IF llPrepak
        lnCurAlias = SELECT(0)
        SELECT (lcPrChild)
        *-- Note : We use nScRec to hold record number for corressponding
        *--      : scale line.
        REPLACE ALL &lcCurPpSz WITH EVALUATE(SYS(18)) ,;
                    (lcCurPrep) WITH 0                 ;
                FOR &lcPrChild..nScRec = RECNO(lcScaline)
        SELECT (lnCurAlias)        
        = lfBlankEnd(RECNO(lcScaline))	&& Blank Replaced line(S).
      ENDIF  
      _CUROBJ = _CUROBJ - 1
    ENDIF
  ENDIF
ELSE		&& Validate added sizes.
  IF llExtSize
    lnPointer = RECNO(lcScaline)
    SELECT (lcScaline)
    SCAN FOR  cDim1+cDim2 = lcGetDim1 + lcGetDim2 AND  ;
              IIF(laScrMode[4],.T.,cStatus != "D"       )                

      DO lpValidSz 	&& If current is found before llFindOne becames .T.
      IF llFindOne
        EXIT
      ENDIF  
    ENDSCAN
    GO lnPointer
  ELSE		&& Single size scale
    DO lpValidSz
  ENDIF
  IF llFindOne
    RETURN	&& Return to avoid completing function.
  ENDIF

  *-- if you there is prepak line(S).
  IF llPrepak
    SELECT (lcPrChild)
    lnPpRec = RECNO(lcPrChild)
  
    *-- if child prepak file Dimension description is empty fill it.
    *-- and replace its size with lcGetDim(S).
    SCAN FOR nScRec = RECNO(lcScaline)
      *-- recplace current prepak field, and its status.
      REPLACE &lcCurPpSz WITH EVALUATE(SYS(18)) ;
              &lcPrChild..cStatus WITH ;
          IIF(&lcPrChild..cStatus = "A","A","M")
      IF llExtSize AND EMPTY(&lcPrChild..cDim1) AND EMPTY(&lcPrChild..cDim2)
        REPLACE &lcPrChild..cDim1 WITH lcGetDim1 ,;
                &lcPrChild..cDim2 WITH lcGetDim2
      ENDIF
      *-- You add this size now, thus its prepaks is 0.
      IF EMPTY(lcOldValue)
        REPLACE &lcCurPrep  WITH 0 
      ENDIF          
    ENDSCAN
    GO lnPpRec
  ENDIF  

ENDIF			&& end of EMPTY(EVALUATE(SYS(18))) IF .

SELECT (lcScaline)
*-- Replace Current Size Field with its new Value
REPLACE (lcCurFld) WITH EVALUATE(SYS(18))

IF laScrMode[3]
  REPLACE &lcScaline..cStatus WITH IIF(&lcScaline..cStatus != "A","M","A")
ENDIF

*-- Refresh pointer
*E301209,1 Add if block for single scale compatability [Begin]
IF !llExtSize
  = &lcSeekCond
ENDIF  
*E301209,1 Add if block for single scale compatability [End  ]

*B604123,1 KAM 18/02/2001 call function from within lfvsize function 
*B604123,1 KAM            to enable more button [start]
= lfMoreEnab()
*B604123,1 KAM [end]

SHOW WINDOW (lcBrowTi1) REFRESH
SELECT (lnSelected)
*-- end of lfvSize.

*!*************************************************************
*! Name      : lfMessTitl
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/25/97
*! Purpose   : Prepair message sent to gfModalGen
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : llAddSize
*!*************************************************************
*! Returns            : Character (Message To send to gfModalGen)
*!*************************************************************
*! Example   : = lfMessTitl()
*!*************************************************************
FUNCTION lfMessTitl
PARAMETERS llAddSize

lcDim1Desc = ALLTRIM(ladata[4])
lcDim2Desc = ALLTRIM(ladata[5])
lcDim3Desc = ALLTRIM(ladata[6])

lcDim1 = IIF(llExtSize,ALLTRIM(lcGetDim1),"")
lcDim2 = IIF(llExtSize,ALLTRIM(lcGetDim2),"")
lcDim3 = IIF(llAddSize,ALLTRIM(EVALUATE(SYS(18))),ALLTRIM(lcOldValue))

lcScalDesc = IIF(llExtSize,ladata[1],lcScale + " Size ")
lcSizeDesc = IIF(!llExtSize ,lcDim3,                                   ;
               IIF(ladata[3] = 1,lcDim1Desc + " = " + lcDim3 ,           ;
               IIF(ladata[3] = 2,lcDim1Desc + " \ " + lcDim2Desc + " = " + ;
               lcDim1 + " \ " + lcDim3,                                    ;
               lcDim1Desc + " \ " + lcDim2Desc + " \ " + lcDim3Desc + " = " +;
               lcDim1 + " \ " + lcDim2 + " \ " + lcDim3 )))

RETURN (lcScalDesc + " : " + lcSizeDesc)
*-- end of lfMessTitl.

*!*************************************************************
*! Name      : lpValidSz
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/25/97
*! Purpose   : Check for Existing of size in same combination, 
*!           : and if found give us a message and rise a flag and return.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpValidSz
*!*************************************************************
PROCEDURE lpValidSz
FOR lnI = 1 TO 8
  lcFld   = "&lcScaline..Sz" + ALLTRIM(STR(lnI))
  IF ALLTRIM(&lcFld) == ALLTRIM(EVALUATE(SYS(18)))   
    lcScalMess = lfMessTitl(.T.)

    *-- The following message is 
    *-- "Scale XXX is already exists."
    *--          < Ok > 
    = gfModalGen('TRM42103B42000','Dialog',lcScalMess)  
    &lcCurVar = lcOldValue
    SHOW GET &lcCurVar
    _CUROBJ = _CUROBJ
    llFindOne = .T.
    EXIT
  ENDIF
ENDFOR  
*-- end of lpValidSz.

*!*************************************************************
*! Name      : lfDelSz
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/04/97
*! Purpose   : Shift Sizes.
*!           : Both Extended and single size scale .
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfCapPosSz,lfBlankEnd
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDelSz()
*!*************************************************************
FUNCTION lfDelSz
llChangObj = .F.	&& Control object number.
*-- Last line (i.e: no shift from another line ) [begin]
*-- and this case occur when either single sizes 
*-- or extended but we are in the last line or it's the only scale line.
*-- Note : lfCapPosSz Function return .T. if it's last line.
IF !llExtSize OR lfCapPosSz()
  *-- Shift from the same line and size between (Sz1 and Sz7)
  IF RIGHT(lcCurVar,1) != "8"
    FOR lnI = VAL(RIGHT(lcCurVar,1)) TO 7
      *-- Hold sizes.
      lcUseVar   = "lcGetSz"+ALLTRIM(STR(lnI))
      lcPressFld = ALLTRIM("&lcScaline..Sz")+ALLTRIM(STR(lnI))
      lcNextFld  = ALLTRIM("&lcScaline..Sz")+ALLTRIM(STR(lnI+1))
      *-- Shift sizes.
      REPLACE (lcPressFld) WITH EVALUATE(lcNextFld)
      &lcUseVar = EVALUATE(lcNextFld)
      SHOW GET &lcUseVar

      *-- Hold NRF codes
      lcUseNRFv   = "lcGetNRF"+ALLTRIM(STR(lnI))
      lcPressNRF = ALLTRIM("&lcScaline..cNrfCode")+ALLTRIM(STR(lnI))
      lcNextNRF  = ALLTRIM("&lcScaline..cNrfCode")+ALLTRIM(STR(lnI+1))
      *-- Shift NRFs.
      REPLACE (lcPressNRF) WITH EVALUATE(lcNextNRF)
      &lcUseNRFv = EVALUATE(lcNextNRF)
      SHOW GET &lcUseNRFv
      
      IF llPrepak
        *-- Hold Prepak values
        SELECT (lcPrChild)
        lcPressSz = ALLTRIM("&lcPrChild..Sz")+ALLTRIM(STR(lnI))
        lcNextSz  = ALLTRIM("&lcPrChild..Sz")+ALLTRIM(STR(lnI+1))
        lcPressPre = ALLTRIM("&lcPrChild..Pp")+ALLTRIM(STR(lnI))
        lcNextPre  = ALLTRIM("&lcPrChild..Pp")+ALLTRIM(STR(lnI+1))

        *-- Shift prepaks.
        REPLACE ALL (lcPressSz)  WITH EVALUATE(lcNextSz) ,;
                    (lcPressPre) WITH EVALUATE(lcNextPre) ;
                FOR &lcPrChild..nScRec = RECNO(lcScaline)

        SELECT (lcScaline)
      ENDIF  
    ENDFOR

    IF llPrepak
      SELECT (lcPrChild)
      *-- Clear last prepak to finish shifting
      REPLACE ALL &lcPrChild..Sz8  WITH " " ,;
                  &lcPrChild..Pp8 WITH 0     ;
              FOR &lcPrChild..nScRec = RECNO(lcScaline)
      SELECT (lcScaline)

      = lfBlankEnd(RECNO(lcScaline))
      SELECT (lcScaline)
    ENDIF
  ELSE	&& Shift from the same line and size is Sz8.
    llChangObj = .T.
  ENDIF  

  *-- Change status in edit mode
  IF laScrMode[3]
    REPLACE &lcScaline..cStatus WITH IIF(&lcScaline..cStatus = "A","A","M")
  ENDIF  

  *-- Clear last size to finish shifting
  REPLACE &lcScaline..Sz8 WITH " "
  lcGetSz8 = " "
  SHOW GET lcGetSz8

  *-- Clear last NRF to finish shifting
  REPLACE &lcScaline..cNrfCode8 WITH " "
  lcGetNRF8 = " "
  SHOW GET lcGetNRF8

ELSE		&&Shift from another line(S) i.e: Not last line
  *-- In this shift case I do two techniques for shift 
  *-- 1st Tech. Descriped in shifting prepaks and prepak sizes,
  *-- and also you can use it for shifting scale sizes.
  *-- 2nd Tech. Descriped in shifting scale sizes.
  
  *-- 1st Tech. : Shift prepaks and prepak sizes [begin]
  IF llPrepak
    lnDelRow = RECNO(lcTempPos)
    lnDelCol = VAL(RIGHT(lcCurVar,1))
    
    *-- laParent : Array hold all prepak codes for this scale.
    SELECT Prepak ;
    FROM (lcPrParent)      ;
    INTO ARRAY laParent

    FOR lnI = 1 TO ALEN(laParent)

      *-- laPrepak : Array hold all prepak Qty.  for selected prepak.
      *-- laPreSz  : Array hold all prepak sizes for selected prepak.
      SELECT Pp1,Pp2,Pp3,Pp4,             ;
             Pp5,Pp6,Pp7,Pp8              ;
      FROM (lcPrChild)                    ;
      WHERE Prepak+cDim1+cDim2 =          ;
        laParent[lnI]+lcGetDim1+lcGetDim2 ;
        AND cStatus != "D"                ;
      INTO ARRAY laPrepak

      SELECT Sz1,Sz2,Sz3,Sz4,             ;
             Sz5,Sz6,Sz7,Sz8              ;
      FROM (lcPrChild)                    ;
      WHERE Prepak+cDim1+cDim2 =          ;
        laParent[lnI]+lcGetDim1+lcGetDim2 ;
        AND cStatus != "D"                ;
      INTO ARRAY laPreSz

      *-- Transfer data to one dimension arrays. [begin]
      DIMENSION laTempPre[ALEN(laPrepak)],laTempSz[ALEN(laPreSz)]
      = ACOPY (laPrepak,laTempPre)
      = ACOPY (laPreSz,laTempSz)
      *-- Transfer data to one dimension arrays. [end]

      *-- Delete Sizes from array , shift all sizes and prepaks [begin]
      *-- then replace last items in arrays and retransfer to original arrays.
      = ADEL(laTempPre,AELEMENT(laPrepak,lnDelRow,lnDelCol))
      = ADEL(laTempSz,AELEMENT(laPreSz,lnDelRow,lnDelCol))
      laTempPre[ALEN(laTempPre)] = 0
      laTempSz[ALEN(laTempSz)]   = " "

      = ACOPY (laTempPre,laPrepak)
      = ACOPY (laTempSz,laPreSz)
      *-- Delete Sizes from array , shift all sizes and prepaks [end]

      SELECT (lcPrChild)
      lnChRecord = RECNO(lcPrChild)
      lnK = 1
      *-- Save new data (after shift) to prepak file
      SCAN FOR Prepak+cDim1+cDim2 = laParent[lnI]+lcGetDim1+lcGetDim2 ;
           AND cStatus != "D" 
        FOR lnJ = 1 TO 8
          lcPpFld = "Pp"+ALLTRIM(STR(lnJ)) 
          lcPpSz  = "Sz"+ALLTRIM(STR(lnJ))
          REPLACE (lcPpFld) WITH laPrepak[lnK,lnJ] ,;
                  (lcPpSz)  WITH laPreSz[lnK,lnJ]
        ENDFOR
        lnK = lnK + 1
      ENDSCAN 
      GO lnChRecord
    ENDFOR

    SELECT (lcScaline)

    llDelet = .F.		&& Flag is it's .T. we delete prepak line.
  ENDIF  
  *-- 1st Tech. : Shift prepaks and prepak sizes [end]

  *-- 2nd Tech. : Shift scale sizes [begin]
  lnRows = RECCOUNT(lcTempPos) - RECNO(lcTempPos) + 1
  DIMENSION laSizes[lnRows,8]		&& Array hold scale sizes
  laSizes  = " "

  DIMENSION laNRFs[lnRows,8]		&& Array hold NRFs
  laNRFs  = " "

  lnSpaces = VAL(RIGHT(lcCurVar,1))	&& Calculate when we start

  SELECT (lcTempPos)
  lnMark = RECNO(lcTempPos)  
  *-- Fill Sizes and NRFs arrays. [begin]
  FOR lnI = 1 TO lnRows
    FOR lnJ = 1 TO 8
      IF (lnI != 1) OR (lnJ > lnSpaces)
        lcUsedFld  = ALLTRIM("&lcTempPos..Sz")+ALLTRIM(STR(lnJ))
        laSizes[lnI,lnJ] = EVALUATE(lcUsedFld)
        
        lcUsedNRFd = ALLTRIM("&lcTempPos..cNrfCode")+ALLTRIM(STR(lnJ))
        laNRFs[lnI,lnJ] = EVALUATE(lcUsedNRFd)

      ENDIF
    ENDFOR
    SKIP
  ENDFOR
  *-- Fill Sizes and NRFs arrays. [end]
  GO lnMark
  
  lnAllItems = lnRows * 8 - 1		&& Number of items

  *-- Create new values which will substitutes in file. [shift arrays begin]
  FOR lnI = 1 TO lnAllItems
    laSizes[lnI] = laSizes[lnI + 1]
    laNRFs[lnI] = laNRFs[lnI + 1]
  ENDFOR
  STORE " " TO laSizes[lnAllItems+1],laNRFs[lnAllItems+1]  && Clear last item.
  *-- Create new values which will substitutes in file. [shift arrays end]

  llAddSpace = .F.		&& When it's .T. we clear sizes and NRFs in file. 
  SELECT (lcScaline)
  lnMark = RECNO(lcScaline)  
  lnI = 1
  
  *-- This is a complicated shift case
  *-- we shift sizes from another line(s).
  *-- by replacing new data from arrays.
  SCAN REST FOR cDim1+cDim2 = lcGetDim1+lcGetDim2
    FOR lnJ = 1 TO 8
      IF !EMPTY(laSizes[lnI,lnJ]) OR llAddSpace 
        IF ! llAddSpace
          llAddSpace = .T.
        ENDIF
        lcUsedVr   = "lcGetSz"+ALLTRIM(STR(lnJ))
        lcUsedFld = ALLTRIM("&lcScaline..Sz")+ALLTRIM(STR(lnJ))
        
        lcUsedNRv   = "lcGetNRF"+ALLTRIM(STR(lnJ))
        lcUsedNRf   = ALLTRIM("&lcScaline..cNrfCode")+ALLTRIM(STR(lnJ))
        
        *-- First size is empty (i.e: delete line)  
        IF (lnJ = 1) AND EMPTY(laSizes[lnI,lnJ])
          *-- if you find prepak lines delete it.
          IF llPrepak
            SELECT (lcPrChild)
            SCAN FOR &lcPrChild..nScRec = RECNO(lcScaline)
              DELETE
            ENDSCAN
            LOCATE FOR &lcLocInCh
            lnMarker3 = RECNO(lcPrChild)
            llDelet = .T.
            SELECT (lcScaline)
          ENDIF

          *-- delete scale sizes.
          IF laScrMode[3] AND (cStatus != "A")
            REPLACE cStatus WITH "D"
          ELSE
            DELETE 
          ENDIF  
          EXIT		&& Exit for loop.
        ENDIF
        
        REPLACE (lcUsedFld) WITH laSizes[lnI,lnJ] ,;
                (lcUsedNRf) WITH laNRFs[lnI,lnJ]

        *-- if current is the line from which we shift
        IF lnI = 1
          &lcUsedVr = laSizes[lnI,lnJ]
          SHOW GET &lcUsedVr
          
          &lcUsedNRv = laNRFs[lnI,lnJ]
          SHOW GET &lcUsedNRv
        ENDIF  

      ENDIF  && end of !EMPTY(laSizes[lnI,lnJ]) OR llAddSpace 
    ENDFOR
    lnI = lnI + 1
    IF laScrMode[3] AND (cStatus = "S")
      REPLACE cStatus WITH "M"
    ENDIF  
    IF llPrepak
      lnBlanked = RECNO(lcScaline)		&& Record number of blanked lines.
    ENDIF  
  ENDSCAN

  IF llPrepak AND !llDelet
    = lfBlankEnd(lnBlanked)
  ENDIF
  GO lnMark
  *-- 2nd Tech. : Shift scale sizes [end]
ENDIF && if Last line (i.e: no shift from another line ) [end]

_CUROBJ = IIF(llChangObj,_CUROBJ - 1,_CUROBJ)	&& Adjust current object. 
SHOW WINDOW (lcBrowTi1) REFRESH

IF llExtSize
  USE IN (lcTempPos)
ENDIF  
*-- end of lfDelSz.

*!*************************************************************
*! Name      : lfCapPosSz
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/25/97
*! Purpose   : Know the position of shifted line,and returns .T. 
*!           : if it's last line or it's the only line, else it returns .F.
*!           : Also it create a file used in the two shifting techniques.
*!           : Extended scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Logical
*!*************************************************************
*! Example   : = lfCapPosSz()
*!*************************************************************
FUNCTION lfCapPosSz
PRIVATE lnSelcting,lnRecN

lnSelcting = SELECT(0)
lnRecN     = RECNO()

SELECT cDim1,cDim2,     ;
       SZ1,             ;
       SZ2,             ;
       SZ3,             ;
       SZ4,             ;
       SZ5,             ;
       SZ6,             ;
       SZ7,             ;
       SZ8,             ;
       cNrfCode1,       ;
       cNrfCode2,       ;
       cNrfCode3,       ;
       cNrfCode4,       ;
       cNrfCode5,       ;
       cNrfCode6,       ;
       cNrfCode7,       ;
       cNrfCode8        ;
FROM (lcScaline)        ;
WHERE cDim1+cDim2 =     ;
  lcGetDim1+lcGetDim2   ;
INTO CURSOR (lcTempPos)

lcThisFld  = ALLTRIM("&lcTempPos..Sz"+RIGHT(lcCurVar,1))
LOCATE FOR EVALUATE(lcThisFld) = lcOldValue
  
SELECT (lnSelcting)
GO lnRecN
RETURN (RECNO(lcTempPos) = RECCOUNT(lcTempPos))
*-- end of lfCapPosSz.

*!*************************************************************
*! Name      : lfActMore
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/20/97
*! Purpose   : Called from validation of lcGetSz8 to affect more state
*!           : Extended scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfActMore()
*!*************************************************************
FUNCTION lfActMore
*B604123,1 KAM 18/02/2001 if first size is not empty call function for enable more button[start]
*IF !EMPTY(EVALUATE(SYS(18)))
IF !EMPTY(lcGetSz1)
*B604123,1 KAM [end]
  = lfMoreEnab()
ELSE
  SHOW GET pbMore DISABLE
ENDIF
*-- end of lfActMore.

*!*************************************************************
*! Name      : lfvMore
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/20/97
*! Purpose   : Validation when press <More> button,which Check for more 
*!           : state and change it corressponding to current case.
*!           : Extended scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvMore()
*!*************************************************************
FUNCTION lfvMore
*-- if edit mode and this scale if found in style file
IF laScrMode[3] AND lfSearchSy()
  RETURN
ENDIF

SELECT (lcScaline)
APPEND BLANK			&&Add line to temporary scale file.
*llMoreClck = .T.

*-- if you find prepak line(S), add new line(S).
*IF &lcSeekCond
*  = lfAddLineP()
*  SELECT (lcScaline)
*ENDIF  

IF laScrMode[3]
  REPLACE &lcScaline..cStatus   WITH "A"
ENDIF  

lnMarker = RECNO()

REPLACE &lcScaline..cDim1 WITH lcGetDim1 ;
        &lcScaline..cDim2 WITH lcGetDim2

*-- lnCnt : Control showing of size titles in screen.
COUNT TO lnCnt FOR ALLTRIM(&lcScaline..cDim1) + ALLTRIM(&lcScaline..cDim2) == ;
                   ALLTRIM(lcGetDim1)         + ALLTRIM(lcGetDim2) 

GO lnMarker

lnSize = lnCnt * 8 - 7

= lfRefresh(lcWinC12)

= lfRefExtD('ENABLE',.T.,.T.)

*E301209,1 Point to scale code in all cases [Begin]
*_CUROBJ = OBJNUM(lcGetSz1)
_CUROBJ = OBJNUM(lcScaleKey)
*E301209,1 Point to scale code in all cases [End  ]

SHOW WINDOW (lcBrowTi1) REFRESH
SHOW GET pbMore   DISABLE
*-- end of lfvMore.

*!*************************************************************
*! Name      : lfvNRF
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : Validate NRF codes
*!           : Single and Extended size scale.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNRF()
*!*************************************************************
FUNCTION lfvNRF
IF EMPTY(EVALUATE(SYS(18)))
  RETURN
ENDIF

lcCurVar  = ALLTRIM(SYS(18))
lcCurFld  = ALLTRIM("&lcScaline..cNrfCode"+RIGHT(lcCurVar,1))

REPLACE (lcCurFld) WITH EVALUATE(SYS(18))
SHOW GET &lcCurVar

IF (lcOldValue != EVALUATE(SYS(18))) AND laScrMode[3]
  REPLACE &lcScaline..cStatus WITH IIF(&lcScaline..cStatus != "A","M","A")
ENDIF

*-- Control TAB sequence.
*IF VAL(RIGHT(lcCurVar,1)) < 8
  *lcSzChk = "lcGetSz"+ALLTRIM(STR(VAL(RIGHT(lcCurVar,1))+1))
  *IF EMPTY(&lcSzChk)
    *_CUROBJ = OBJNUM(pbSav)
  *ENDIF
*ELSE
  *_CUROBJ = OBJNUM(pbSav)
*ENDIF  
*-- end of lfvNRF.

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : valid function for <Remove> button, scale folder, and also 
*!           : we call it if we delete last size in the line. 
*!           : Both extended and single sizes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfInStyle,lfActBrow2,lfActBrow3,lfActBrow1
*!                        : lfClsData,lfRefresh,lfMoreEnab 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRemove()
*!*************************************************************
FUNCTION lfvRemove
*-- If you press <Remove> button and this scale does not allocated
*-- to any style.
IF !llShift AND laScrMode[3] AND lfInStyle()
  RETURN  
ENDIF

lcRemovMes = IIF(!llExtSize,"Scale " + lcScale ,;
             "This " + ALLTRIM(ladata[4]) + " " + ;
             IIF(ladata[3] = 3,"\ " + ALLTRIM(ladata[5]) + " = " + ;
             ALLTRIM(lcGetDim1)+ " \ " + ALLTRIM(lcGetDim2),;
             IIF(ladata[3]= 2 ,"= ","")+ ALLTRIM(lcGetDim1))+" set of sizes")

*-- The following message is 
*-- "Are you sure you want to delete XXX ."
*--         < Yes > < NO >
IF gfModalGen('QRM42124B42002','Dialog',lcRemovMes) = 2
  IF llShift
    &lcCurVar = lcOldValue
    SHOW GET &lcCurVar
    llShift = .F.
  ENDIF  
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

llCUpdate  = .T.

lnAlias = SELECT(0)
SELECT (lcScaline)
*-- lnCntDim1 : Number of lines for current combination
*-- lnCntAll  : Number of all lines in the file.
IF llExtSize
  lnRecT = RECNO()          
  lcExpr2 = IIF(laScrMode[3],[&lcScaline..cStatus != "D"],".T.")
  COUNT TO lnCntDim1 FOR (cDim1 = lcGetDim1) AND &lcExpr2
  COUNT TO lnCntAll FOR &lcExpr2
  GO lnRecT 
ELSE
  STORE 1 TO lnCntDim1,lnCntAll
ENDIF

*-- if you find prepak lines. [begin]

*E301209,1 Adjust condition to support changes to extended case.
*IF &lcSeekCond
IF (llExtSize AND !EOF(lcPrParent)) OR (!llExtSize AND &lcSeekCond)
  SELECT (lcPrParent)
  *-- lcScalPrep : Save current record data to seek for it later.

  *E301209,1 Adjust condition to support changes to extended case. [Begin]
  *lcScalPrep = ALLTRIM(&lcPrParent..Scale)+&lcPrParent..Prepak
  lcScalPrep = IIF(llExtSize,'',ALLTRIM(&lcPrParent..Scale)) + &lcPrParent..Prepak
  *E301209,1 Adjust condition to support changes to extended case. [End  ]
  
  SELECT (lcPrChild)
  SCAN FOR &lcPrChild..nScRec = RECNO(lcScaline)
    
    SELECT (lcPrParent)

    *E301209,1 Adjust condition to support changes to extended case. [Begin]
    *= SEEK (&lcSeekPrep+&lcPrChild..Prepak)
    = SEEK (IIF(llExtSize,'',&lcSeekPrep) + &lcPrChild..Prepak)
    *E301209,1 Adjust condition to support changes to extended case. [End  ]
  
    *-- calculate new total prepak value.
    FOR lnI = 1 TO 8
      lcPpField = "&lcPrChild..Pp"+ALLTRIM(STR(lnI))
      REPLACE &lcPrParent..pPTot WITH ;
              &lcPrParent..pPTot - &lcPpField
    ENDFOR
    SELECT (lcPrChild)
    DELETE
  ENDSCAN
  
  SELECT (lcPrParent)

  = SEEK(lcScalPrep,lcPrParent)

  SELECT (lcPrChild)
  LOCATE FOR &lcLocInCh

  lnMarker3 = RECNO(lcPrChild)
  SELECT (lcScaline)

  *-- if you remove all lines and it's extended size scale
  *-- clear temp. prepak files data. 
  IF lnCntAll = 1 AND llExtSize
    IF llExtSize 
      SELECT (lcPrParent)
      ZAP
      = lfActBrow2()
    ENDIF
    SELECT (lcPrChild)
    ZAP
    = lfActBrow3()
  ENDIF

ENDIF
*-- if you find prepak lines. [end]

*-- if you remove all lines and it's extended size scale
*-- clear temp. scale files data, and refresh screen.
IF lnCntAll = 1 AND llExtSize
  SELECT (lcScaline)
  ZAP
  = lfActBrow1()
  SHOW GET pbRemove DISABLE
  STORE 'DISABLE' TO lcRemoves
  = lfClsData()
  IF laData[3] = 1
    SHOW GET pbNew ENABLE
    STORE 'ENABLE' TO lcNews
  ENDIF  
  IF laData[3] = 3
    DIMENSION laDim1[1]
    laDim1 = ''
    lnPosDim1 = 1
    SHOW GET lnPosDim1
  ENDIF
  IF ladata[3] >= 2
    = lfRefresh(lcWinC22)
  ENDIF
  _CUROBJ = OBJNUM(pbNew) 
  llSwMode = laScrMode[3]
ELSE	&& file still have another data or Single size scale.
  IF laScrMode[3] AND &lcScaline..cStatus != "A"
    REPLACE &lcScaline..cStatus WITH "D"
  ELSE
    DELETE
  ENDIF
  SKIP -1

  *-- Refresh new record pointer
  IF laScrMode[4]
    IF BOF()
      GO TOP IN (lcScaline)
    ENDIF  
  ELSE
    IF BOF() OR &lcScaline..cStatus = "D"
      LOCATE FOR &lcScaline..cStatus != "D"
    ENDIF
  ENDIF
  
  lnMarker = RECNO()

  SELECT (lnAlias)
  *-- i.e: User remove all lines in current combination
  IF lnCntDim1 <= 1
    IF llExtSize
      DO CASE
        CASE laData[3] = 1		&& 1 dimension
          SHOW GET pbNew ENABLE
          STORE 'ENABLE' TO lcNews
          = lfClsData()
          _CUROBJ = OBJNUM(pbNew) 
        CASE laData[3] = 2		&& 2 dimension
          IF EOF(lcScaline) 
            = lfClsData()
            _CUROBJ = OBJNUM(pbNew) 
          ELSE
            _CUROBJ = OBJNUM(ibTrap1)
          ENDIF  
        CASE laData[3] = 3		&& 3 dimension
          IF EOF(lcScaline) 
            = lfClsData()
            lnPosDim1 = 1
            SHOW GET lnPosDim1
            _CUROBJ = OBJNUM(pbNew) 
          ELSE
            lnTemPos1 = lnPosDim1
            = ADEL(laDim1,lnTemPos1)   
            DIMENSION laDim1[ALEN(laDim1) - 1]
            lnPosDim1 = IIF(lnPosDim1 = 1, 1 , lnPosDim1 - 1)
            SHOW GET lnPosDim1
            _CUROBJ = OBJNUM(lnPosDim1)
            KEYBOARD CHR(13) PLAIN CLEAR
            _CUROBJ = OBJNUM(ibTrap1)
          ENDIF
      ENDCASE
    ELSE		&& Single size scale
      GO TOP  
      IF EOF(lcScaline) OR BOF(lcScaline)
        = lfClsData()
        _CUROBJ = OBJNUM(pbNew) 
      ELSE
        _CUROBJ = OBJNUM(ibTrap1)
      ENDIF
    ENDIF
  ELSE		&& User does not remove all lines in current combination
    IF llShift
      llShift = !llShift
      _CUROBJ = OBJNUM(lcGetSz8)
    ELSE
      _CUROBJ = OBJNUM(ibTrap1)
    ENDIF  
 
    *-- Refresh screen.
    lnSize = lnSize - 8
    lnSize = IIF(lnSize >= 1 , lnSize , 1)
    = lfRefresh(lcWinC12)
    = lfRefExtD('')
    = lfMoreEnab()
  ENDIF
  SHOW WINDOW (lcBrowTi1) REFRESH
ENDIF
llShift = .F.
*-- end of lfvRemove.

*!*************************************************************
*! Name      : lfvScaleS
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/97
*! Purpose   : Validate single size scale,in scale file
*!           : Single sizes only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen,lfActBrow3,lfRefSinD
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvScaleS()
*!*************************************************************
FUNCTION lfvScaleS
IF MDOWN()
  RETURN
ENDIF  
PRIVATE lnNum
lnNum = RECNO(lcScaline)
lnAlias = SELECT(0)

SELECT (lcScaline)
*-- Note that : Index is on SCALE+CSTATUS
IF !EMPTY(lcScale) 
  *-- if you find this scale before.
  llFindScal = IIF(SEEK(lcScale+"S"),.T.,;
               IIF(SEEK(lcScale+"M"),.T.,;
               IIF(SEEK(lcScale+"A"),.T., .F.)))
  GO lnNum
  *-- If you find this scale before.
  IF llFindScal
    *-- The following message is 
    *-- "Scale XXX is already exists."
    *--         < Ok >
    = gfModalGen('TRM42103B42000','Dialog',lcScale)  
    lcScale = lcOldValue
    _CUROBJ = _CUROBJ
    RETURN
  ELSE		&& This scale not found before.
    *SELECT (lcScaline)
    *GO lnNum
    SHOW GET lcScale DISABLE
    REPLACE &lcScaline..Scale WITH EVALUATE(SYS(18))
    = lfActBrow3()
    SHOW WINDOW (lcBrowTi1) REFRESH
    =lfRefSinD()
    _CUROBJ = _CUROBJ + 1
  ENDIF		&& Ending seek condition
ENDIF							        
SELECT(lnAlias)
*-- End of lfvScaleS

*!*************************************************************
*! Name      : lfvSclDes
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/97
*! Purpose   : Validate Single size scale description in scale file.
*!           : Single sizes only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSclDes()
*!*************************************************************
FUNCTION lfvSclDes
IF (lcOldValue != EVALUATE(SYS(18)))
  REPLACE &lcScaline..cScl_Desc WITH EVALUATE(SYS(18))
  REPLACE &lcScaline..cStatus WITH IIF(&lcScaline..cStatus != "A","M","A")
  SHOW WINDOW (lcBrowTi1) REFRESH
ENDIF
*-- End of lfvSclDes

*!*************************************************************
*! Name      : lfvNewP
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/07/98
*! Purpose   : This is a valid function for <New> button, in prepak folder,
*!           : and if there is no empty records it add one.  
*!           : Both extended and single sizes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfPpLnAdd,lfPpLnBlnk,lfWBrow3
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNewP()
*!*************************************************************
FUNCTION lfvNewP
IF llExtSize
  *-- if you did not find any empty prepak, thus you add new line(S).

  *E301209,1 Comment out if...endif because No need to this check.
  *IF !SEEK(&lcSeekPrep+' ',lcPrParent)
    *-- Note : you add lines for all combinations in temp. scale file. [begin]
    lcScanCond = IIF(laScrMode[4],'.T.','&lcScaline..cStatus != "D"')
    lnSaved = RECNO(lcScaline) 

    *E301209,1 Arrange lines by scale [Begin]
    *SELECT DISTINCT cDim1+cDim2 ;
    FROM (lcScaline)            ; 
    WHERE &lcScanCond           ;
    INTO ARRAY laCombine

    SELECT DISTINCT SCALE+cDim1+cDim2 ;
    FROM (lcScaline)            ; 
    WHERE &lcScanCond           ;
    ORDER BY Scale              ;
    INTO ARRAY laCombine
    *E301209,1 Arrange lines by scale [End  ]

    FOR lnCombine = 1 TO ALEN(laCombine)
      SELECT (lcScaline)
      
      *E301209,1 Add scale to scan condition [Begin]
      *SCAN FOR (cDim1+cDim2 = laCombine[lnCombine]) AND &lcScanCond
      SCAN FOR (Scale+cDim1+cDim2 = laCombine[lnCombine]) AND &lcScanCond
      *E301209,1 Add scale to scan condition [End  ]

        *E301209,1 Pass scale to function which add prepaks [Begin]
        *= lfPpLnAdd()		&& Add new line to temp. child prepak file.
        = lfPpLnAdd(Scale)	&& Add new line to temp. child prepak file.
        *E301209,1 Pass scale to function which add prepaks [End  ]

      ENDSCAN
      SELECT (lcPrChild)
      *-- if empty of last size blank the ends of this line.
      IF EMPTY(&lcPrChild..Sz8)
        = lfPpLnBlnk()
      ENDIF  
    ENDFOR

    SELECT (lcScaline)
    GO lnSaved
    *-- Note : you add lines for all combinations in temp. scale file. [end]

    *-- add line to temp. parent prepak file.
    SELECT (lcPrParent)
    APPEND BLANK

    *E301209,1 Add new prepak has ladata[1] scale [Begin]
    *REPLACE Scale   WITH &lcSeekPrep
    REPLACE Scale   WITH laData[1]
    *E301209,1 Add new prepak has ladata[1] scale [End  ]

    SHOW WINDOW (lcBrowTi2) REFRESH
    SHOW WINDOW (lcBrowTi3) REFRESH
  
  *E301209,1 Comment out if...endif because No need to this check.
  *ENDIF

ELSE		&& Single size scale
  IF !SEEK(&lcSeekPrep+' ',lcPrChild) OR !EMPTY(&lcPrChild..Prepak)
    SELECT (lcPrChild)
    *= lfPpLnAdd()
    = lfPpLnAdd(&lcSeekPrep)

    IF EMPTY(&lcPrChild..Sz8)
      = lfPpLnBlnk()
    ENDIF  
    SHOW WINDOW (lcBrowTi3) REFRESH
  ENDIF
ENDIF

llValidPre = .F.		&& it is .F. until user fill a valid PrepakID
=lfWBrow3()		&&Enable and disable prepak folder buttons.
lcTrap2 = IIF(llExtSize,"ibTrap2","ibTrap3")
_CUROBJ = OBJNUM(&lcTrap2)
*-- end of lfvNewP.

*!*************************************************************
*! Name      : lfPpLnAdd
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/07/98
*! Purpose   : Add line to child prepak file
*!           : Both extended and single sizes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPpLnAdd()
*!*************************************************************
FUNCTION lfPpLnAdd
*E301209,1 Add new parameter which has scale code then replace scale code with it in child 
*          Prepak file.
PARAMETERS lcRepcScal

INSERT INTO (lcPrChild)    ;
        (Scale,            ;
         cDim1,            ;
         cDim2,            ;
         Sz1,              ;
         Sz2,              ;
         Sz3,              ;
         Sz4,              ;
         Sz5,              ;
         Sz6,              ;
         Sz7,              ;
         Sz8,              ;
         Pp1,              ;
         Pp2,              ;
         Pp3,              ;
         Pp4,              ;
         Pp5,              ;
         Pp6,              ;
         Pp7,              ;
         Pp8,              ;
         nScRec,           ;
         cStatus           );
 VALUES (lcRepcScal,       ;
         &lcScaline..cDim1,;
         &lcScaline..cDim2,;
         &lcScaline..Sz1,  ;
         &lcScaline..Sz2,  ;
         &lcScaline..Sz3,  ;
         &lcScaline..Sz4,  ;
         &lcScaline..Sz5,  ;
         &lcScaline..Sz6,  ;
         &lcScaline..Sz7,  ;
         &lcScaline..Sz8,  ;
         0,0,0,0,0,0,0,0,  ;
         RECNO(lcScaline), ;
         "A"               )     
*-- end of lfPpLnAdd.

*!*************************************************************
*! Name      : lfPpLnBlnk
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/25/98
*! Purpose   : Blank end of added line to child prepak file.
*!           : Both extended and single sizes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPpLnBlnk()
*!*************************************************************
FUNCTION lfPpLnBlnk
SELECT (lcPrChild)
SCATTER FIELDS Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8  TO laTSzs
SCATTER FIELDS cDim1,cDim2,nScRec,cStatus,Scale TO laFlds
               
BLANK
llRepAtAll = .F.
FOR lnSzs = 7 TO 1 STEP -1
  IF llRepAtAll OR !EMPTY(laTSzs[lnSzs])
    lcPps = "&lcPrChild..Pp"+ALLTRIM(STR(lnSzs))
    REPLACE (lcPps) WITH 0
    llRepAtAll = .T.
  ENDIF
ENDFOR
GATHER FROM laTSzs FIELDS Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8
GATHER FROM laFlds FIELDS cDim1,cDim2,nScRec,cStatus,Scale
*-- end of lfPpLnBlnk.

*!*************************************************************
*! Name      : lfvRemoveP
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/29/98
*! Purpose   : valid function for <Remove> button, prepak folder
*!           : Both extended and single sizes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfWBrow2,lfWBrow3
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRemoveP()
*!*************************************************************
FUNCTION lfvRemoveP

lcPreMes = IIF(llExtSize,&lcPrParent..Prepak,&lcPrChild..Prepak)

*-- The following message is 
*-- "Are you sure you want to delete prepack XXX ."
*--               < Yes >   < No > 
IF gfModalGen('QRM42127B42002','Dialog',lcPreMes) = 1
  SELECT (lcPrChild)
  IF llExtSize
    DELETE FOR &lcPrChild..Prepak = &lcPrParent..Prepak ;
           AND &lcPrChild..cStatus = "A"
    IF laScrMode[3]
      REPLACE ALL &lcPrChild..cStatus WITH "D" ;
              FOR &lcPrChild..Prepak = &lcPrParent..Prepak
    ENDIF
    SELECT (lcPrParent)
    DELETE
    SKIP -1
    =lfWBrow2()
  ELSE		&& Single size scale.
    IF &lcPrChild..cStatus = "A"
      DELETE
    ELSE
      REPLACE &lcPrChild..cStatus WITH "D"
    ENDIF

    IF &lcPrChild..cStatus = "D"
      lcPreScale = &lcPrChild..Scale
      SELECT (lcPrChild)
      LOCATE FOR SCALE+PREPAK = lcPreScale AND cstatus != "D"
    ENDIF

  ENDIF  
  
  = lfWBrow3()		&&Enable and disable prepak folder buttons.

  lcTrap2 = IIF(llExtSize,"ibTrap2","ibTrap3")
  _CUROBJ = OBJNUM(&lcTrap2)
  llCUpdate  = .T.
ENDIF
*-- end of lfvRemoveP.

*!*************************************************************
*! Name      : lfvPrePak
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/29/98
*! Purpose   : validate prepak codes.
*!           : Both extended and single sizes.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab2,lpBackTab2
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPrePak()
*!*************************************************************
FUNCTION lfvPrePak
lcDealFile   = IIF(llExtSize,lcPrParent,lcPrChild)	&& Prepak used file

*-- return if you don't enter any thing.
IF EMPTY(&lcDealFile..Prepak)
  RETURN
ENDIF

lcPrePakId = UPPER(&lcDealFile..Prepak)
REPLACE &lcDealFile..Prepak WITH ' '

llFindPpak = .F.

*E301209,1 Adjust seek condition [begin]
*IF laScrMode[4] AND SEEK(&lcSeekPrep+lcPrePakId,lcDealFile)
IF laScrMode[4] AND SEEK(IIF(llExtSize,'',&lcSeekPrep)+lcPrePakId,lcDealFile)
*E301209,1 Adjust seek condition [End  ]
  llFindPpak = .T.

ELSE

  *E301209,1 Adjust locate condition [begin]
  *lcPrepakCd = IIF(llExtSize,"LEFT(SCALE,1) ","SCALE ") + ;
  *             "+ PREPAK = &lcSeekPrep + lcPrePakId"    + ;
  *             IIF(llExtSize,""," AND cStatus != 'D'")
  lcPrepakCd = IIF(llExtSize,"Prepak = lcPrePakId","Scale+Prepak = &lcSeekPrep+lcPrePakId AND cStatus != 'D'")
  *E301209,1 Adjust locate condition [End  ]

  SELECT (lcDealFile)
  LOCATE FOR &lcPrepakCd
  llFindPpak = FOUND()
ENDIF  

*-- if you find prepak code before. 
IF llFindPpak
  *-- The following message is 
  *-- "Prepack XXX is already exists."
  *--         < Ok > 
  = gfModalGen('TRM42123B42000','Dialog',lcPrePakId)  

  *E301209,1 Adjust Seek condition [begin]
  *= SEEK(&lcSeekPrep+' ',lcDealFile)
  = SEEK(IIF(llExtSize,'',&lcSeekPrep)+' ',lcDealFile)
  *E301209,1 Adjust Seek condition [End  ]

ELSE		&& you didn't find this prepakID before.

  *E301209,1 Adjust Seek condition [begin]
  *= SEEK(&lcSeekPrep+' ',lcDealFile)
  = SEEK(IIF(llExtSize,'',&lcSeekPrep)+' ',lcDealFile)
  *E301209,1 Adjust Seek condition [End  ]

  REPLACE &lcDealFile..Scale  WITH &lcSeekPrep ,;
          &lcDealFile..Prepak WITH lcPrePakId

  llValidPre = .T.	&& you can TAB from this browse field.

  *-- if extended size scale you must replace all empty child records with prepakID.
  IF llExtSize
    SELECT (lcPrChild)

    *E301209,1 Adjust Replace to replace prepak only [begin]
    *REPLACE ALL &lcPrChild..Scale  WITH &lcSeekPrep ,;
    *            &lcPrChild..Prepak WITH lcPrePakId ;
    *        FOR EMPTY(&lcPrChild..Prepak)
    REPLACE ALL &lcPrChild..Prepak WITH lcPrePakId ;
            FOR EMPTY(&lcPrChild..Prepak)
    *E301209,1 Adjust Replace to replace prepak only [end  ]

    LOCATE FOR &lcLocInCh
    ACTIVATE WINDOW (lcWinC22)
    _CUROBJ = OBJNUM(ibTrap3)

    ON KEY LABEL TAB        DO lpTab2		&& Refersh trapping
    ON KEY LABEL BACKTAB    DO lpBackTab2	&& Refersh trapping
  ENDIF  

  = gfUpDate()    

ENDIF
*-- end of lfvPrePak.

*!*************************************************************
*! Name      : lfOldPp
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/10/98
*! Purpose   : To save the old prepak quantity in child browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcRefrence
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfOldPp()
*!*************************************************************
FUNCTION lfOldPp
PARAMETERS lcRefrence

lcOldField = "Pp"+lcRefrence
lcOldVar   = "lnOldPp"+lcRefrence
STORE &lcOldField TO &lcOldVar 
*-- end of lfOldPp.

*!*************************************************************
*! Name      : lfvPpQty
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/10/98
*! Purpose   : To valid prepak quantities in the following cases
*!           : 1- Extended sizes and Parent browse
*!           : 2- Single   sizes and Child  browse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcRefrence
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPpQty()
*!*************************************************************
FUNCTION lfvPpQty
PARAMETERS lcRefrence

*-- lcDealFile : Variable hold name of prepak used file
lcDealFile   = IIF(llExtSize,lcPrParent,lcPrChild)

lcCurField = "Pp"+lcRefrence
lcOldVar   = "lnOldPp"+lcRefrence
*-- if quantity of current prepak field is changed.
IF &lcCurField != &lcOldVar
  *-- Change total prepak quantity due to changes to current prepak quantity
  REPLACE &lcDealFile..pPTot WITH ;
          &lcDealFile..pPTot + &lcCurField - &lcOldVar

  *-- if you are in edit mode,
  *-- Note that : The following condition is not important because if 
  *--           : cStatus != "A" we must in edit mode.
  *--           : but it is for more readability.
  IF laScrMode[3]
    REPLACE &lcPrChild..cStatus WITH ;
            IIF(&lcPrChild..cStatus != "A","M","A")
  ENDIF

  *-- Extended size scale.
  IF llExtSize
    SHOW WINDOW (lcBrowTi2) REFRESH SAME
  ENDIF  
ENDIF
*-- end of lfvPpQty.


*--------------------------------------------------------------
*-- WIZARD code section.
*-- This section contains all functions and procedures used in 
*-- Wizard modal screens.
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lfvAuto
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/10/97
*! Purpose   : Validate <Auto> button, activate modal screens 
*!           : to fill the file automatically,also prepair for wizard , 
*!           : and have the code for saving and canceling.
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfInStyle,lfRefresh,lfRefExtD,lfVarBrow1
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvAuto()
*!*************************************************************
FUNCTION lfvAuto
*-- llRefScBrw : When it's .T. means that Zap old files 
*--            : before fill new data if user select <Finish> in last step.
llRefScBrw = .F.
*-- If this scale have data,check If you are in edit mode
*-- and this scale allocated to any style return,otherwise
*-- ask the user the message shared with the 
*-- following condition which is 
*-- 'You will change structure, thus you will lose your data,Are you sure.'
*--                  < Yes >   < No >
*-- and if he press < No > return
IF RECCOUNT(lcScaline) != 0         AND        ;
   ((laScrMode[3] AND lfInStyle()) OR          ;
   gfModalGen('QRM42105B42002','Dialog') = 2)
   RETURN 
ELSE
  llRefScBrw = .T.
ENDIF

PUSH KEY
ON KEY

lcFirst   = gfTempName()		&& Name of file hold data for 1st dimension.
lcSecond  = gfTempName()		&& Name of file hold data for 2nd dimension.
lcThird   = gfTempName()		&& Name of file hold data for 3rd dimension.

DIMENSION laSource[1],laTarget[1]		&& arrays used in last step(Mover)
STORE SPACE(0) TO laSource,laTarget

lcWizard   = ""
lcWizFld   = ""

*-- Save screen data [begin]
lnTempNo   = ladata[3]		&& Spinner variable
lcBroWizTl = "Dimension Values"
lcFirstDm  = ladata[4]
lcSecondDm = ladata[5]
lcThirdDm  = ladata[6]
*-- Save screen data [end]

llFinished = .F.	&& if it's .T. you transfer automatic data to files.
lcInTitle = IIF(ladata[3] = 2 , "/4 - ","/5 - ")  && used in modal screen title.
lcWizTitl1 = IIF(ladata[3] = 2 , "Step 1/4 - ","Step 1/5 - ") +;
             "Specify Dimensions"

CREATE TABLE (gcWorkDir+lcFirst)  (Size C(5))
APPEND BLANK

CREATE TABLE (gcWorkDir+lcSecond) (Size C(5))
APPEND BLANK

IF ladata[3] > 2
  CREATE TABLE (gcWorkDir+lcThird) (Size C(5))
  APPEND BLANK
ENDIF  

ON KEY LABEL ALT+R ACTIVATE WINDOW (lcBroWizTl)

DO (gcScrDir+gcWinAppl+'\ICScAuto.SPR')

ON KEY

USE IN (lcFirst)
USE IN (lcSecond)
ERASE (gcWorkDir+lcFirst+".DBF")
ERASE (gcWorkDir+lcSecond+".DBF")
IF ladata[3] > 2
  USE IN (lcThird)
  ERASE (gcWorkDir+lcThird+".DBF")
ENDIF  

POP KEY

*-- Saving automatic data. [begin]
IF llFinished
  llFinished = .F.
  llSwMode = laScrMode[3]
  
  lnSize = 1
  = lfRefresh(lcWinC12)

  = lfRefExtD('ENABLE',.T.,.T.)

  SHOW GET ladata[4]
  SHOW GET ladata[5]
  *-- if your case is 3 dimension
  IF ladata[3] = 3
    SHOW GET ladata[6]
    = lfRefresh(lcWinC14)

    *-- Fill popup array with approparate data.
    SELECT DISTINCT cDim1   ;
    FROM (lcScaline)        ; 
    INTO ARRAY laDim1
    = ASORT(laDim1)

    lnPosDim1 = 1
    SHOW GET lnPosDim1
  ENDIF

  SHOW GET pbRemove ENABLE
  STORE 'ENABLE' TO lcRemoves
  SELECT (lcScaline)
  GO TOP
  lnMarker = RECNO()
  = lfVarBrow1()
  SHOW WINDOW (lcBrowTi1) REFRESH

  _CUROBJ = OBJNUM(ibTrap1)
ENDIF
*-- Saving automatic data. [end]
*-- end of lfvAuto.

*!*************************************************************
*! Name      : lfvNext
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/12/97
*! Purpose   : <Next> button Validation
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpNextStat
*!             Functions  : lfAutoBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNext()
*!*************************************************************
FUNCTION lfvNext

*-- lnActWndow : Numiric variable used to know in which screen we in.

*-- If 2 dimension and you are at 3rd screen you must skip 4th screen.
IF (lnActWndow = 3) AND (ladata[3] = 2)
  lnActWndow = lnActWndow + 1
ENDIF

lnActWndow = lnActWndow + 1
lcWizTitle = "Step "+ALLTRIM(STR(lnActWndow))+lcInTitle

*-- lcWinName : Hold name of window we use to calculate its coordnates.
*--           : when we are in parent this variable is name of top window,
*--           : else if we are at browse window it's the name of its parent window. 
lcWinName = IIF(EMPTY(WPARENT(WONTOP())),WONTOP(),WPARENT(WONTOP()))
*-- The next two variable assigned for later modifications in code.
lnX = WLCOL(lcWinName)	&& Row coordnate.
lnY = WLROW(lcWinName)	&& Coloumn coordnate.

DO CASE
  CASE lnActWndow = 2
    SHOW GETS WINDOW Auto1 DISABLE
    HIDE WINDOW Auto1
    
    SHOW GETS WINDOW Auto3 DISABLE
    HIDE WINDOW Auto3
    
    lcWizTitle = lcWizTitle + "First Dimension Values"
    MODIFY WINDOW Auto2 TITLE lcWizTitle

*    MOVE WINDOW Auto2 TO lnY, lnX	&& Later modifications in code.
    MOVE WINDOW Auto2 CENTER
    ACTIVATE WINDOW Auto2 TOP
    SHOW GETS WINDOW WizCnt ENABLE	&& Wizard control window
    = lfAutoBrow(lcFirst, ladata[4])  && Call Wizard Browse Fn.
    SHOW GET pbFinish2 DISABLE  && Enable in last step only.
    DO lpNextStat     
  CASE lnActWndow = 3
    lcWizTitle = lcWizTitle + "Second Dimension Values"

    MODIFY WINDOW Auto2 TITLE lcWizTitle
    = lfAutoBrow(lcSecond, ladata[5])
    DO lpNextStat     
  CASE lnActWndow = 4
    lcWizTitle = lcWizTitle + "Third Dimension Values"

    MODIFY WINDOW Auto2 TITLE lcWizTitle
    = lfAutoBrow(lcThird, ladata[6])
    DO lpNextStat     
  CASE lnActWndow = 5
    SHOW GETS WINDOW Auto1 DISABLE
    HIDE WINDOW Auto1
    
    SHOW GETS WINDOW WizCnt DISABLE		&& Wizard control window
    HIDE WINDOW Auto2

    *-- Collect mover data, [begin].
    *-- all of this data is separated by "\" and fill
    *-- target list array.
    lnTargetLen = 0
    SELECT (lcFirst)
    SCAN FOR !EMPTY(Size)         && Scan in 1st dimension file.
      lcTarget1 = ALLTRIM(Size)
        SELECT (lcSecond)
        SCAN FOR !EMPTY(Size)     && Scan in 2nd dimension file.	
          lcTarget2 = lcTarget1 + " \ " + ALLTRIM(Size)
          IF ladata[3] = 2
            lnTargetLen = lnTargetLen + 1
            DIMENSION laTarget[lnTargetLen]
            laTarget[lnTargetLen] = lcTarget2
          ELSE
            SELECT (lcThird)
            SCAN FOR !EMPTY(Size)  && Scan in 3rd dimension file.
              lcTarget3 = lcTarget2 + " \ " + ALLTRIM(Size)
              lnTargetLen = lnTargetLen + 1
              DIMENSION laTarget[lnTargetLen]
              laTarget[lnTargetLen] = lcTarget3
            ENDSCAN		&& End scan in 3rd dimension file
          ENDIF
        ENDSCAN			&& Scan in 2nd dimension file.
    ENDSCAN				&& Scan in 1st dimension file.
    
    =ASORT(laTarget)
    
    DIMENSION laSource[1]
    STORE "" TO laSource

    lcWizTitle = IIF(ladata[3] = 2 , "Step 4" , "Step 5")+lcInTitle

    MODIFY WINDOW Auto3 TITLE lcWizTitle + "Select combinations"
*    MOVE WINDOW Auto3 TO lnY, lnX	 &&later modifications in code.
    MOVE WINDOW Auto3 CENTER
    ACTIVATE WINDOW Auto3 TOP
    SHOW GETS WINDOW Auto3 ENABLE
    SHOW GET pbNext3  DISABLE	&& Must be disable in last step
    SHOW GET pbMove   DISABLE
    SHOW GET pbMovAll DISABLE
ENDCASE
*-- end of lfvNext.


*!*************************************************************
*! Name      : lfvBack
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/12/97
*! Purpose   : <Back> button Validation
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpChangWin,lpNextStat
*!             Functions  : lfAutoBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBack()
*!*************************************************************
FUNCTION lfvBack

IF (lnActWndow = 5) AND (ladata[3] = 2)
  lnActWndow = lnActWndow - 1
ENDIF

lnActWndow = lnActWndow - 1
lcWizTitle = "Step "+ALLTRIM(STR(lnActWndow))+lcInTitle

lcWinName = IIF(EMPTY(WPARENT(WONTOP())),WONTOP(),WPARENT(WONTOP()))
lnX = WLCOL(lcWinName)
lnY = WLROW(lcWinName)

DO CASE
  CASE lnActWndow = 4
    lcWizTitle = lcWizTitle + "Third Dimension Values"

    MODIFY WINDOW Auto2 TITLE lcWizTitle
    
    *-- Control window2 states, and we separate this code avoiding
    *-- writing twice,
    *-- 1 - for this case.
    *-- 2 - if we skip this case and go to  lnActWndow = 3 case. [2 dimension] 
    DO lpChangWin
    = lfAutoBrow(lcThird, ladata[6])
    DO lpNextStat     
  CASE lnActWndow = 3
    lcWizTitle = lcWizTitle + "Second Dimension Values"

    MODIFY WINDOW Auto2 TITLE lcWizTitle

    IF ladata[3] = 2
      DO lpChangWin
    ENDIF  
    = lfAutoBrow(lcSecond, ladata[5])
    DO lpNextStat     
  CASE lnActWndow = 2
    lcWizTitle = lcWizTitle + "First Dimension Values"
    MODIFY WINDOW Auto2 TITLE lcWizTitle
    = lfAutoBrow(lcFirst, ladata[4])
    DO lpNextStat     
  CASE lnActWndow = 1
    SHOW GETS WINDOW WizCnt DISABLE  && Wizard control window 
    HIDE WINDOW Auto2

    SHOW GETS WINDOW Auto3 DISABLE
    HIDE WINDOW Auto3
    
*    MOVE WINDOW Auto1 TO lnY, lnX	&&later modifications in code.
    MOVE WINDOW Auto1 CENTER
    ACTIVATE WINDOW Auto1 TOP  
    SHOW GETS WINDOW Auto1 ENABLE
    SHOW GET pbBack1   DISABLE	&& Must be disable in first step
    SHOW GET pbFinish1 DISABLE
    SHOW GET lnTempNo  DISABLE
ENDCASE
*-- end of lfvBack.

*!*************************************************************
*! Name      : lpChangWin
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/12/97
*! Purpose   : To distinguished between (2 and 3) dimensions,
*!           : when user press <Back> button from last screen in wizard.
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpChangWin
*!*************************************************************
PROCEDURE lpChangWin

SHOW GETS WINDOW Auto1 DISABLE
HIDE WINDOW Auto1

SHOW GETS WINDOW Auto3 DISABLE
HIDE WINDOW Auto3

*MOVE WINDOW Auto2 TO lnY, lnX
MOVE WINDOW Auto2 CENTER
ACTIVATE WINDOW Auto2 TOP
SHOW GETS WINDOW WizCnt EnABLE		&& Wizard control window
SHOW GET pbFinish2 DISABLE
*-- end of lpChangWin.

*!*************************************************************
*! Name      : lpNextStat
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/01/98
*! Purpose   : Enable/Disable <Next> button.
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpNextStat
*!*************************************************************
PROCEDURE lpNextStat
COUNT FOR !EMPTY(Size) TO lnSzRecs
lcNextSt = IIF(lnSzRecs = 0 ,'DISABLE','ENABLE')
SHOW GET pbNext2 &lcNextSt
*-- end of lpNextStat.

*!*************************************************************
*! Name      : lfAutoBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/12/97
*! Purpose   : Wizard browse function
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfWizard,lfIsValid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAutoBrow()
*!*************************************************************
FUNCTION lfAutoBrow
PARAMETERS lcFile, lcName

IF !EMPTY(lcBroWizTl) AND WEXIST(lcBroWizTl)
  RELEASE WINDOW (lcBroWizTl)
ENDIF  
lcBroWizTl = "Dimension " + ALLTRIM(lcName) + " ..."

lcBrFdName = ALLTRIM(lcName)+"s"
lcWizFld   = "Size :H= '" + lcBrFdName +"':10:W=lfWizard():V=lfIsValid():F"

SELECT (lcFile)
GO TOP
BROWSE FIELDS &lcWizFld    ;
                  NOAPPEND ;
                  NODELETE ;
                  NOMENU   ;
                  SAVE     ;
                  NOCLEAR  ;
                  NOWAIT   ;
       TITLE (lcBroWizTl)  ;
       WINDOW AutoBrow     ;
       IN WINDOW Auto2
*-- end of lfAutoBrow.

*!*************************************************************
*! Name      : lfWizTrap
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/12/97
*! Purpose   : Wizard Trap function, For wizard browse
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpWizTab,lpShWizTab,lpAutEsc
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWizTrap()
*!*************************************************************
FUNCTION lfWizTrap
IF WONTOP() = (lcBroWizTl)
  ON KEY LABEL TAB        DO lpWizTab
  ON KEY LABEL BACKTAB    DO lpShWizTab
  ON KEY LABEL ESC        DO lpAutEsc
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+W     lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
ENDIF
RETURN .F.
*-- end of lfWizTrap.

*!*************************************************************
*! Name      : lfWizUnTp
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/12/97
*! Purpose   : Wizard UnTrap function, For wizard browse
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWizUnTp()
*!*************************************************************
FUNCTION lfWizUnTp
IF WONTOP() # (lcBroWizTl)
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ESC
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL Ctrl+W
ENDIF
*-- end of lfWizUnTp.

*!*************************************************************
*! Name      : lpAutEsc
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/12/97
*! Purpose   : Control ESCAPE key in wizard
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpAutEsc
*!*************************************************************
PROCEDURE lpAutEsc

ON KEY LABEL ESC
ACTIVATE WINDOW WizCnt  
_CUROBJ = OBJNUM(pbCancel2)
KEYBOARD "{ENTER}" clear plain
ON KEY LABEL ESC DO lpAutEsc		&& Refresh ESC key procedure.
*-- end of lpAutEsc.

*!*************************************************************
*! Name      : lpWizTab
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/12/97
*! Purpose   : Control TAB key in wizard
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpWizTab
*!*************************************************************
PROCEDURE lpWizTab
ON KEY LABEL TAB
DO CASE
  CASE WONTOP() = "AUTO1" OR WONTOP() = "AUTO3"
    _CUROBJ = _CUROBJ + 1
  CASE WONTOP() = lcBroWizTl
    ACTIVATE WINDOW WizCnt  
    _CUROBJ = OBJNUM(pbBack2)
ENDCASE
ON KEY LABEL TAB DO lpWizTab	&& Refresh TAB key procedure.
*-- end of lpWizTab.

*!*************************************************************
*! Name      : lpShWizTab
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/12/97
*! Purpose   : Control BACKTAB key in wizard
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShWizTab
*!*************************************************************
PROCEDURE lpShWizTab
ON KEY LABEL BACKTAB
DO CASE
  CASE WONTOP() = "AUTO1" OR WONTOP() = "AUTO3"
    _CUROBJ = _CUROBJ - 1

  CASE WONTOP() = lcBroWizTl
    ACTIVATE WINDOW WizCnt
    _CUROBJ = OBJNUM(pbCancel2)
ENDCASE
ON KEY LABEL BACKTAB DO lpShWizTab		&& Refresh BACKTAB key procedure.
*-- end of lpShWizTab.

*!*************************************************************
*! Name      : lfWizard
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/12/97
*! Purpose   : When function for wizard browse field (Save value of size)
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWizard()
*!*************************************************************
FUNCTION lfWizard
lcWizard = Size
*-- end of lfWizard.

*!*************************************************************
*! Name      : lfIsValid
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/12/97
*! Purpose   : Valid function for wizard browse field.
*!           : Check for existing of code edited in wizard browse.  
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfIsValid()
*!*************************************************************
FUNCTION lfIsValid
IF LASTKEY() = 27
  RETURN .F.
ELSE
  IF Size = lcWizard
    RETURN
  ENDIF
  lnMyPos = RECNO()
  lcSize  = Size
  LOCATE FOR (Size = lcSize) AND RECNO() != lnMyPos
  GOTO lnMyPos
  IF !EMPTY(Size)
    IF FOUND()
      = gfModalGen('TRM42128B42000','Dialog',ALLTRIM(lcSize))  
      REPLACE Size WITH "     "
      SHOW WINDOW (lcBroWizTl) REFRESH SAME
      RETURN
    ELSE
      *-- add new empty line.
      LOCATE FOR EMPTY(Size)
      IF !FOUND()
        APPEND BLANK
        GOTO BOTTOM
      ENDIF
    ENDIF
  ELSE
    IF FOUND()
      DELETE NEXT 1
      *-- To refresh editable browse
      ACTIVATE WINDOW WizCnt
      _CUROBJ = OBJNUM(ibWizTrap)
    ENDIF
  ENDIF
  lnNewPos = RECNO()
  COUNT TO lnRecs FOR !EMPTY(Size)
  GO lnNewPos
  lcNextStat = IIF(lnRecs > 0 , "ENABLE", "DISABLE")
  SHOW GET pbNext2 &lcNextStat
  KEYBOARD '{DNARROW}' CLEAR PLAIN
  SHOW WINDOW (lcBroWizTl) REFRESH SAME
ENDIF
*-- end of lfIsValid.

*!*************************************************************
*! Name      : lfvMove
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/15/97
*! Purpose   : Validate Mover <Move> button in last wizard screen.
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvMove()
*!*************************************************************
FUNCTION lfvMove

*-- if you remove first item and source list go to zero item(out of list)
IF lnSource = 0
  lnSource = 1
  SHOW GET lnSource
ENDIF
    
*-- if you remove last item and source list go to over all items(out of list)
IF lnSource > ALEN(laSource)
  lnSource = ALEN(laSource)
  SHOW GET lnSource
ENDIF

lnCount = ALEN(laTarget,1)		&& Target array length
*-- if latarget already have data
IF !(lnCount = 1 AND EMPTY(laTarget[1]))
  DIMENSION laTarget [lnCount+1]
ENDIF	
laTarget[ALEN(laTarget)] = laSource[lnSource]	&& Transfer item.

= ASORT(laTarget)				&& Arrange target array.
= ADEL(laSource, lnSource)		&& Delete item from source array.
*-- if source array have one item only, and this item is empty.
IF ALEN(laSource) = 1
  laSource[1] = SPACE(0)
ELSE 		 	&& Source array have many items.
  DIMENSION laSource[ALEN(laSource)-1]		&& Delete last item.
ENDIF  

*-- Disable/Enable Source list and its relative buttons. [begin]
lcStat = IIF(ALEN(laSource)=1 AND EMPTY(laSource[1]), "DISABLE", "ENABLE")
SHOW GET lnSource &lcStat
SHOW GET pbMove   &lcStat
SHOW GET pbMovAll &lcStat
*-- Disable/Enable Source list and its relative buttons. [end]

*-- Disable/Enable Target list and its relative buttons. [begin]
lcStat = IIF(ALEN(laTarget)=1 AND EMPTY(laTarget[1]), "DISABLE", "ENABLE")
SHOW GET lnTarget  &lcStat
SHOW GET pbRemove  &lcStat
SHOW GET pbRemAll  &lcStat
SHOW GET pbFinish3 &lcStat
*-- Disable/Enable Target list and its relative buttons. [end]
*-- end of lfvMove.

*!*************************************************************
*! Name      : lfvMoveAll
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/15/97
*! Purpose   : Validate Mover <Move All> button in last wizard screen.
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvMoveAll()
*!*************************************************************
FUNCTION lfvMoveAll

lnStart = ALEN(laTarget) 
*-- if target array have one item only, and this item is empty.
IF lnStart = 1 AND EMPTY(laTarget[1])
  DIMENSION laTarget[ALEN(laSource)]
ELSE		&& target array have many items.
  lnStart = lnStart + 1
  *-- Redimension target array, to be the same length as source array. 
  DIMENSION laTarget[ALEN(laTarget)+ALEN(laSource)]
ENDIF	

*-- Transfer all data from source array to target array [begin]
*-- then clear contents of source array.
= ACOPY(laSource,laTarget,1,-1,lnStart)
= ASORT(laTarget)
DIMENSION laSource[1]
laSource = SPACE(0)
*-- Transfer all data from source array to target array [end]

*-- Control buttons in the screen. [begin]
SHOW GET pbMove    DISABLE		&& Must be disable, source array is empty.
SHOW GET pbMovAll  DISABLE		&& Must be disable, source array is empty.
SHOW GET pbRemove  ENABLE		&& Must be enable, target array is full.
SHOW GET pbRemAll  ENABLE		&& Must be enable, target array is full.
SHOW GET lnTarget  ENABLE		&& Must be enable, target array is full.
SHOW GET lnSource  DISABLE		&& Must be disable, source array is empty.
SHOW GET pbFinish3 ENABLE
*-- Control buttons in the screen. [end]
*-- end of lfvMoveAll.

*!*************************************************************
*! Name      : lfvRemvWiz
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/15/97
*! Purpose   : Validate Mover <Remove> button in last wizard screen.
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRemvWiz()
*!*************************************************************
*! Note      : See documentation for lfvMove
*!           : <Finish> button initially enable when we enter last screen
*!           : because we initially select all items, but after press 
*!           : <Remove All> button it must be disable, because you cannot
*!           : save empty data.
*!*************************************************************
FUNCTION lfvRemvWiz

IF lnTarget = 0
  lnTarget = 1
  SHOW GET lnTarget
ENDIF

IF lnTarget > ALEN(laTarget)
  lnTarget = ALEN(laTarget)
  SHOW GET lnTarget
ENDIF

lnCount = ALEN(laSource,1)
IF !(lnCount = 1 AND EMPTY(laSource[1]))
  DIMENSION laSource [lnCount+1]
ENDIF	
laSource [ALEN(laSource)] = laTarget[lnTarget]

= ASORT(laSource)
= ADEL(laTarget, lnTarget)
IF ALEN(laTarget) = 1
  laTarget[1] = SPACE(0)
ELSE  
  DIMENSION laTarget[ALEN(laTarget)-1]
ENDIF  

lcStat = IIF(ALEN(laSource)=1 AND EMPTY(laSource[1]), "DISABLE", "ENABLE")
SHOW GET lnSource &lcStat
SHOW GET pbMove   &lcStat
SHOW GET pbMovAll &lcStat

lcStat = IIF(ALEN(laTarget)=1 AND EMPTY(laTarget[1]), "DISABLE", "ENABLE")
SHOW GET lnTarget  &lcStat
SHOW GET pbRemove  &lcStat
SHOW GET pbRemAll  &lcStat
SHOW GET pbFinish3 &lcStat		&& Must be disable in this case.
*-- end of lfvRemvWiz.

*!*************************************************************
*! Name      : lfvRemAll
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/15/97
*! Purpose   : Validate Mover <Remove All> button in last wizard screen.
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRemAll()
*!*************************************************************
*! Note      : See documentation for lfvMoveAll.
*!*************************************************************
FUNCTION lfvRemAll

lnStart = ALEN(laSource) 
IF lnStart = 1 AND EMPTY(laSource)
  DIMENSION laSource[ALEN(laTarget)]
ELSE
  lnStart = lnStart + 1
  DIMENSION laSource[ALEN(laSource)+ALEN(laTarget)]
ENDIF	

= ACOPY(laTarget,laSource,1,-1,lnStart)
= ASORT(laSource)
DIMENSION laTarget[1]
laTarget[1] = SPACE(0)

SHOW GET pbMove    ENABLE
SHOW GET pbMovAll  ENABLE
SHOW GET pbRemove  DISABLE
SHOW GET pbRemAll  DISABLE
SHOW GET lnTarget  DISABLE
SHOW GET lnSource  ENABLE
SHOW GET pbFinish3 DISABLE
*-- end of lfvRemAll.

*!*************************************************************
*! Name      : lfvCancel
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/18/97
*! Purpose   : Validate Wizard <Cancel> button in all wizard screens.
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCancel()
*!*************************************************************
FUNCTION lfvCancel
*-- Restore old data and don't change any thing.
ladata[4]  = lcFirstDm
ladata[5]  = lcSecondDm
ladata[6]  = lcThirdDm
*-- end of lfvCancel.

*!*************************************************************
*! Name      : lfvFinish
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/18/97
*! Purpose   : Validate Wizard <Finish> button, which is active only,
*!           : in last wizard screen.
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfActBrow1,lfActBrow2,lfActBrow3
*!                        : lfCalValue
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvFinish()
*!*************************************************************
FUNCTION lfvFinish
*-- if you must clear files before fill new data. [begin]
IF llRefScBrw
  llRefScBrw = .F.
  lnSele = SELECT(0)
  SELECT (lcScaline)
  ZAP
  = lfActBrow1()

  *E301209,1 Adjust Seek condition [begin]
  *-- if you find prepak line(S). [begin]
  *IF SEEK(ladata[1]+&lcPrParent..Prepak,lcPrParent)
  IF SEEK(ladata[1],lcPrParent)
  *E301209,1 Adjust Seek condition [end  ]

    SELECT (lcPrParent)
    ZAP

    IF llExtSize
      = lfActBrow2()
    ENDIF  

    SELECT (lcPrChild)
    ZAP
    = lfActBrow3()
  ENDIF  
  *-- if you find prepak line(S). [end]

  SELECT (lnSele) 
ENDIF
*-- if you must clear files before fill new data. [end]

*-- llNOTSame : When it's .F. means that it's the same combination.
llNOTSame = .F.
lnArrPos = 0		&& Refrence to array.
lcSize = "Sz"

PRIVATE lcOldFir,lcOldSec,lcOldThd   && Old values of dimension descriptions.
STORE "" TO lcFir,lcSec,lcThd        && Dimension descriptions values.

SELECT (lcScaline)
*-- Select distinct combinations from target array data. [begin]
DO WHILE lnArrPos <= ALEN(laTarget)   && Loop around target array.			
  *-- if it's the same combination.
  IF !llNOTSame
    lnArrPos = lnArrPos + 1
    IF lnArrPos > ALEN(laTarget)
      EXIT
    ENDIF
    = lfCalValue(laTarget[lnArrPos])   && Calculate line data.
    lcOldFir = lcFir
    lcOldSec = lcSec
    lcOldThd = lcThd
  ELSE  && another combination.
    llNOTSame = .F.
  ENDIF

  *-- Every time you loop outer do loop, you add new line. 
  APPEND BLANK
  REPLACE cdim1 WITH lcFir ,;
          cdim2 WITH lcSec ,;
          Sz1   WITH lcThd

  *-- inner loop to add sizes values.
  FOR lnI = 2 TO 8
    lnArrPos = lnArrPos + 1
    IF lnArrPos > ALEN(laTarget)
      EXIT
    ENDIF
    = lfCalValue(laTarget[lnArrPos])
    *-- If it's the same combination.
    IF ((ladata[3] = 2) AND (lcOldFir = lcFir)) OR ;
       ((ladata[3] = 3) AND (lcOldFir = lcFir) AND (lcOldSec = lcSec))
      REPLACE (lcSize+ALLTRIM(STR(lnI))) WITH lcThd    
    ELSE		&& another combination.
      lcOldFir = lcFir
      lcOldSec = lcSec
      lcOldThd = lcThd
      llNOTSame = .T.
      EXIT	&& Exit for loop because the combination was finished.
    ENDIF   
  ENDFOR        
ENDDO
*-- Select distinct combinations from target array data. [end]
llFinished = .T.	&& rise this flag to switch screen and refresh new data.
*-- end of lfvFinish.

*!*************************************************************
*! Name      : lfCalValue
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : Calculate values of wizard dimensions and ,
*!           : to distinct between every new lines combinations.
*!           : it masks the passed parameter to restore dinmensions from it.
*!           : Extended sizes (2 or 3 dimensions) only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcValue
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCalValue()
*!*************************************************************
FUNCTION lfCalValue
PARAMETERS lcValue

lnX     = ATC("\",lcValue)   && First mask position.	
lcFir   = ALLTRIM(SUBSTR(lcValue , 1 , lnX - 1))
lcValue = SUBSTR(lcValue , lnX + 1 , LEN(lcValue) - lnX)

IF ladata[3] = 2
  lcSec   = ""
  lcThd = ALLTRIM(lcValue)
ELSE
  lnX     = ATC("\",lcValue)  && Second mask position.
  lcSec   = ALLTRIM(SUBSTR(lcValue , 1 , lnX-1))
  lcThd = ALLTRIM(SUBSTR(lcValue , lnX + 1, LEN(lcValue) - lnX))
ENDIF
*-- end of lfCalValue.


*----------------------------------------------------------
*-- The following is the General Functions and Procedures section
*-- This section contains all functions and procedures frequently called 
*-- from more than one function in the program code. 
*----------------------------------------------------------
*!*************************************************************
*! Name      : lfRefExtD
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/16/97
*! Purpose   : Disable/Enable Sizes edit region objects
*!           : Extended size Scales only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcObjState,llFldsOnly,llKeyDis
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRefExtD()
*!*************************************************************
*! Note      : lcObjState -> Hold "DISABLE","ENABLE", OR ""
*!           : llFldsOnly -> If it's .T., don't change normal objects cursor.
*!           : llKeyDis   -> If it's .T., enable and disable sizes edit region keys.
*!*************************************************************
FUNCTION lfRefExtD 
PARAMETERS lcObjState,llFldsOnly,llKeyDis

IF lnActFolder = 1
  FOR lnI = 1 TO 8
    *-- Sizes state
    lcField = "&lcUsedFile..Sz"      + ALLTRIM(STR(lnI))
    lcVar   = "lcGetSz" + ALLTRIM(STR(lnI))
    STORE &lcField TO &lcVar 
    SHOW GET &lcVar &lcObjState

    *-- NRFs state
    lcNRFFd = "&lcUsedFile..cNrfCode"      + ALLTRIM(STR(lnI))
    lcNRF   = "lcGetNRF"+ ALLTRIM(STR(lnI))
    STORE &lcNRFFd TO &lcNRF 
    SHOW GET &lcNRF &lcObjState
  ENDFOR  

  *E301209,1 Evaluate extended code object [begin]
  lcScaleKey = RIGHT(&lcUsedFile..Scale,lnExtndLen)            && Extended Code Object.
  *E301209,1 Evaluate extended code object [End  ]

  lcGetDim1 = &lcUsedFile..cDim1                               && First  dimension value
  lcGetDim2 = &lcUsedFile..cDim2                               && Second dimension value

  IF llKeyDis

    lcObjStat1 = IIF((laData[3] >= 2) AND EMPTY(lcGetDim1),"ENABLE","DISABLE")
    lcObjStat2 = IIF((laData[3] = 3 ) AND EMPTY(lcGetDim2),"ENABLE","DISABLE")

    *E301209,1 Evaluate extended code object status [begin]
    lcObjStat3 = IIF(EMPTY(lcScaleKey),'ENABLE','DISABLE')
    *E301209,1 Evaluate extended code object status [end  ]

    SHOW GET lcGetDim1 &lcObjStat1
    SHOW GET lcGetDim2 &lcObjStat2
    SHOW GET lcScaleKey &lcObjStat3          && Extended Code Object.
  
  ELSE
    
    SHOW GET lcGetDim1 &lcObjState
    SHOW GET lcGetDim2 &lcObjState
    SHOW GET lcScaleKey &lcObjState          && Extended Code Object.
  
  ENDIF

  IF (lcObjState = 'ENABLE') AND !llFldsOnly AND EMPTY(lcScaleKey)
    
    *E301209,1 Current object must be extended scale object [begin]
    *IF laData[3] = 1
    *  _CUROBJ = OBJNUM(lcGetSz1)
    *ELSE
    *  _CUROBJ = OBJNUM(lcGetDim1)
    *ENDIF

    lcExKeySta = lcObjState                  && Extended Code Object State (Enable/Disable)
    SHOW GET lcScaleKey &lcExKeySta          && Extended Code Object.
    _CUROBJ = OBJNUM(lcScaleKey)
    *E301209,1 Current object must be extended scale object [end  ]

  ENDIF
  
ENDIF  
*-- end of lfRefExtD.

*!*************************************************************
*! Name      : lfRefSinD
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/16/97
*! Purpose   : Disable/Enable Sizes edit region objects
*!           : Single size Scales only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : llEmpty
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRefSinD()
*!*************************************************************
*! Note      : llEmpty -> If it's .T., take object states as is it. 
*!*************************************************************
FUNCTION lfRefSinD 
PARAMETERS llEmpty

IF lnActFolder = 1
  lcScale    = &lcUsedFile..Scale
  lcFirObj = IIF (laScrMode[2],"DISABLE",;
             IIF (EMPTY(lcScale),"ENABLE","DISABLE"))
  SHOW GET lcScale &lcFirObj	&& Key state

  IF llEmpty
    lcObjState = " "
  ELSE
    lcObjState = IIF (laScrMode[2],"DISABLE",;
                 IIF (EMPTY(lcScale),"DISABLE","ENABLE"))
  ENDIF

  lcscl_desc = &lcUsedFile..cscl_desc
  SHOW GET lcscl_desc &lcObjState

  FOR lnI = 1 TO 8
    *-- Sizes state
    lcField = "&lcUsedFile..Sz"      + ALLTRIM(STR(lnI))
    lcVar   = "lcGetSz" + ALLTRIM(STR(lnI))
    STORE &lcField TO &lcVar 
    SHOW GET &lcVar &lcObjState

    *-- NRFs state
    lcNRFFd = "&lcUsedFile..cNrfCode"      + ALLTRIM(STR(lnI))
    lcNRF   = "lcGetNRF"+ ALLTRIM(STR(lnI))
    STORE &lcNRFFd TO &lcNRF 
    SHOW GET &lcNRF &lcObjState
  ENDFOR  
ENDIF  
*-- end of lfRefSinD.

*!*************************************************************
*! Name      : lfClsData
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/16/97
*! Purpose   : Clear Screen data,  refresh it, and disable objects.
*!           : Both Extended and Single size Scales.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfClsData()
*!*************************************************************
FUNCTION lfClsData
FOR lnI = 1 TO 8
  *-- Clear sizes and disable it.
  lcVar   = "lcGetSz" + ALLTRIM(STR(lnI))
  STORE " " TO &lcVar 
  SHOW GET &lcVar DISABLE

  *-- Clear NRFs and disable it.
  lcNRF   = "lcGetNRF"+ ALLTRIM(STR(lnI)) 
  STORE " " TO &lcNRF
  SHOW GET &lcNRF DISABLE
ENDFOR  

IF llExtSize

  *E301209,1 Clear extended code also [begin]
  *STORE " " TO lcGetDim1,lcGetDim2
  STORE " " TO lcScaleKey,lcGetDim1,lcGetDim2

  SHOW GET lcGetDim1 DISABLE
  SHOW GET lcGetDim2 DISABLE

  *-- Add this code
  lcExKeySta = 'DISABLE'                  && Extended Code Object State (Enable/Disable)
  SHOW GET lcScaleKey &lcExKeySta

  *E301209,1 Clear extended code also [end  ]

  IF !EMPTY(laDim1[1])
    DIMENSION laDim1[1]
    laDim1 = ' '
    SHOW GET lnPosDim1
  ENDIF  
ELSE	&& Single size scale.
  lcscl_desc = " "
  lcScale    = " "
  SHOW GET lcscl_desc DISABLE
  SHOW GET lcscale    DISABLE
ENDIF
*-- end of lfClsData

*!*************************************************************
*! Name      : lfMoreStat
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/28/97
*! Purpose   : Enable/Disable <More> button.
*!           : Extended size Scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfMoreStat()
*!*************************************************************
FUNCTION lfMoreStat
*-- More button is disable if Sz8 is empty.
lcEnStat = IIF((laScrMode[3] OR laScrMode[4]) AND (!EMPTY(lcGetSz8)) ,;
           "ENABLE" , "DISABLE" )
SHOW GET pbMore  &lcEnStat
*-- end of lfMoreStat.

*!*************************************************************
*! Name      : lfCapPosBw
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/28/97
*! Purpose   : Capture position of record due to moving from one record 
*!           : to another in scale browse.
*!           : Extended size Scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCapPosBw()
*!*************************************************************
FUNCTION lfCapPosBw
PRIVATE lnSelcting,lnRecN

IF !laScrMode[1]
  lnSelcting = SELECT(0)
  lnRecN     = RECNO()
    
  lcWhere = IIF(laScrMode[2],'Scale = laData[1] AND Type = "S"', ;
            IIF(laScrMode[3],[ cStatus != "D" ],".T."))

  *E301209,1 Add Order by scale and get scale field [begin]
  *SELECT cDim1,cDim2,SZ1  ;
  *FROM (lcUsedFile)       ;
  *WHERE &lcWhere          ;
  *ORDER BY cDim1 , cDim2  ;
  *INTO CURSOR (lcTempCnt)

  SELECT Scale,cDim1,cDim2,SZ1  ;
  FROM (lcUsedFile)       ;
  WHERE &lcWhere          ;
  ORDER BY Scale , cDim1 , cDim2  ;
  INTO CURSOR (lcTempCnt)
  *E301209,1 Add Order by scale and get scale field [end  ]
  
   
  SELECT (lnSelcting)
  GO lnRecN
ENDIF  
*-- end of lfCapPosBw.

*!*************************************************************
*! Name      : lfMoreEnab
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/28/97
*! Purpose   : Enable/Disable <More> button due to moving from one record 
*!           : to another in scale browse.
*!           : Extended size Scale only.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfMoreEnab()
*!*************************************************************
FUNCTION lfMoreEnab
llEnable = .F.

*B603489,1 AMH 08/08/2000  Fixing Enabling the More Button [Start]
PRIVATE lcCurAlias
lcCurAlias = SELECT()
SELECT (lcUsedFile)
*B603489,1 AMH 08/08/2000  Fixing Enabling the More Button [End  ]
*B604123,1 KAM 18/02/2001 if first size not empty we enable more button[start]
*IF !EMPTY(&lcUsedFile..Sz8) AND (laScrMode[3] OR laScrMode[4])
IF !EMPTY(&lcUsedFile..Sz1) AND (laScrMode[3] OR laScrMode[4])
*B604123,1 KAM [end]
  SKIP
  *B604430,1 WAB (START) lcGetDim1 ,lcGetDim2 this e variable intialiad only in case of extended size scale.
  *IF EOF(lcUsedFile) OR ;
     (&lcUsedFile..cDim1+&lcUsedFile..cDim2 != lcGetDim1+lcGetDim2) OR ;
     IIF(laScrMode[3],(&lcUsedFile..cStatus = "D"),.F.)
  IF EOF(lcUsedFile) OR ;
     (llExtSize .AND. &lcUsedFile..cDim1+&lcUsedFile..cDim2 != lcGetDim1+lcGetDim2) OR ;
     IIF(laScrMode[3],(&lcUsedFile..cStatus = "D"),.F.)
    *B604430,1 WAB (END) 
    llEnable = .T.
  ENDIF
  IF !llEnable AND laData[3] >= 2
    IF ALLTRIM(&lcUsedFile..cDim1) + ALLTRIM(&lcUsedFile..cDim2) != ;
       ALLTRIM(lcGetDim1)          + ALLTRIM(lcGetDim2)
      llEnable = .T.
    ENDIF
  ENDIF
  SKIP -1
ENDIF

lcMoreSt = IIF(llEnable,"ENABLE","DISABLE")
SHOW GET pbMore &lcMoreSt

*B603489,1 AMH 08/08/2000  Fixing Enabling the More Button [Start]
SELECT (lcCurAlias)
*B603489,1 AMH 08/08/2000  Fixing Enabling the More Button [End  ]
*-- end of lfMoreEnab.

*!*************************************************************
*! Name      : lfInStyle
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/97
*! Purpose   : Check the existance of scale in style file and 
*!           : if scale is found it returns .T. .
*!           : Both Extended and Single size Scale.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfInStyle()
*!*************************************************************
FUNCTION lfInStyle
lnUsing = SELECT(0)
SELECT STYLE

*B606943,1 KHM 02/16/2003 (Begin) Set the order to the style tag in order to be able to check
*B606943,1 KHM 02/16/2003 the scale of all style's colors.
PRIVATE lcOldOrd
lcOldOrd = ORDER()
SET ORDER TO TAG Style
*B606943,1 KHM 02/16/2003 (End)

LOCATE FOR STYLE.SCALE = &lcSeekPrep
llInStyle = FOUND()

*B606943,1 KHM 02/16/2003 (Begin) Restore the old order.
SET ORDER TO &lcOldOrd
*B606943,1 KHM 02/16/2003 (End)

IF llInStyle
  *-- The following message is 
  *-- "This scale is used by one or more style, Cannot proceed.."
  *--                         < Ok > 
  = gfModalGen('TRM42121B42000','Dialog')  
ENDIF  
SELECT (lnUsing)
RETURN (llInStyle)
*-- end of lfInStyle.

*!*************************************************************
*! Name      : lfoldvalue
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/15/97
*! Purpose   : Get lcOldValue to compare with current value
*!           : Both Extended and Single size Scale.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfoldvalue()
*!*************************************************************
FUNCTION lfoldvalue
lcOldValue = EVALUATE(SYS(18))
*-- end of lfoldvalue.

*!*************************************************************
*! Name      : lfAddLineP
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/15/97
*! Purpose   : Add line(S) to prepak details corressponding to
*!           : another in scale details.
*!           : Both Extended and Single size Scale, Edit mode.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAddLineP()
*!*************************************************************
FUNCTION lfAddLineP
lcScalPrep = &lcPrParent..Prepak
SELECT (lcPrParent)
SCAN

  *E301209,1 Replace scale code with its value direct from temp. scale file [begin]
  *INSERT INTO (lcPrChild)       ;
           (Scale               ,;
            prepak              ,;
            nScRec              ,;
            cDim1               ,;
            cDim2               ,;
            cStatus             );
    VALUES (&lcSeekPrep                 ,;
            &lcPrParent..prepak         ,;
            RECNO(lcScaline)            ,;
            IIF(llNewPpLn,'',lcGetDim1) ,;
            IIF(llNewPpLn,'',lcGetDim2) ,;
            "A"                         )     

  INSERT INTO (lcPrChild)        ;
           (Scale               ,;
            prepak              ,;
            nScRec              ,;
            cDim1               ,;
            cDim2               ,;
            cStatus             );
    VALUES (&lcScaline..Scale           ,;
            &lcPrParent..prepak         ,;
            RECNO(lcScaline)            ,;
            IIF(llNewPpLn,'',lcGetDim1) ,;
            IIF(llNewPpLn,'',lcGetDim2) ,;
            "A"                         )     
  *E301209,1 Replace scale code with its value direct from temp. scale file [end  ]

ENDSCAN

= SEEK(lcScalPrep,lcPrParent)

llNewPpLn = .F.
*-- end of lfAddLineP.

*!*************************************************************
*! Name      : lfCalTotPp
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/15/97
*! Purpose   : Calculates total prepaks for every prepakID, 
*!           : after we delete corressopnding size, and it used in delete
*!           : scale functions.
*!           : Both Extended and Single size Scale.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCalTotPp()
*!*************************************************************
FUNCTION lfCalTotPp
IF llExtSize
  SELECT (lcPrParent)

  *E301209,1 Save scale prepak value [begin]
  *lcScalPrep = ALLTRIM(&lcPrParent..Scale)+&lcPrParent..Prepak
  lcScalPrep  = &lcPrParent..Prepak
  *E301209,1 Save scale prepak value [end  ]

ENDIF  

SELECT (lcPrChild)
SCAN FOR &lcPrChild..nScRec = RECNO(lcScaline)
  IF laScrMode[3]
    REPLACE &lcPrChild..cStatus WITH IIF(&lcPrChild..cStatus != "A","M","A")
  ENDIF

  *-- if extended size scales totals is in Parent prepak file
  IF llExtSize
  
    *E301209,1 Change seek command [begin]
    *= SEEK(&lcSeekPrep+&lcPrChild..Prepak,lcPrParent)
    = SEEK(&lcPrChild..Prepak,lcPrParent)
    *E301209,1 Change seek command [end  ]

    REPLACE &lcPrParent..pPTot   WITH &lcPrParent..pPTot - &lcCurPrep
  ELSE  && Single size scale, totals is in child prepak file 
    REPLACE &lcPrChild..pPTot    WITH &lcPrChild..pPTot  - &lcCurPrep
  ENDIF  

ENDSCAN

*-- Restore record pointer.
IF llExtSize
  SELECT (lcPrParent)
  = SEEK(lcScalPrep,lcPrParent)
ENDIF
  
SELECT (lcScaline)
*-- end of lfCalTotPp.

*!*************************************************************
*! Name      : lfBlankEnd
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/15/97
*! Purpose   : Blanks zero prepak quantity in end of line,
*!           : used in delete scale functions.
*!           : Both Extended and Single size Scale.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnScanedRec (Record number of current scale record.)
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfBlankEnd()
*!*************************************************************
FUNCTION lfBlankEnd
PARAMETERS lnScanedRec
lnThAlias = SELECT(0)
SELECT (lcPrChild)
SCAN FOR &lcPrChild..nScRec = lnScanedRec
  llFill = .F.
  *-- Save data.
  SCATTER FIELDS Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8,;
                 Pp1,Pp2,Pp3,Pp4,Pp5,Pp6,Pp7,Pp8,;
                 cDim1,cDim2,nScRec,cStatus,Prepak,Scale,PpTot TO laChildTmp
  BLANK		&& Blank line.
  FOR lnChildTmp = 8 TO 1 STEP -1
    lcPps = "&lcPrChild..Pp"+ALLTRIM(STR(lnChildTmp))
    IF !EMPTY(laChildTmp[lnChildTmp]) OR llFill
      REPLACE (lcPps) WITH laChildTmp[lnChildTmp+8]
      llFill = IIF(!llFill,!llFill,llFill)
    ENDIF
  ENDFOR
  
  *-- Restore data with out replacing zeros.
  REPLACE Sz1     WITH laChildTmp[1]  ,;
          Sz2     WITH laChildTmp[2]  ,;  
          Sz3     WITH laChildTmp[3]  ,;  
          Sz4     WITH laChildTmp[4]  ,;  
          Sz5     WITH laChildTmp[5]  ,;  
          Sz6     WITH laChildTmp[6]  ,;  
          Sz7     WITH laChildTmp[7]  ,;  
          Sz8     WITH laChildTmp[8]  ,;  
          cDim1   WITH laChildTmp[17] ,;
          cDim2   WITH laChildTmp[18] ,;
          nScRec  WITH laChildTmp[19] ,;
          cStatus WITH laChildTmp[20] ,;
          Prepak  WITH laChildTmp[21] ,;
          Scale   WITH laChildTmp[22] ,;
          PpTot   WITH laChildTmp[23]

ENDSCAN

GO lnChild IN (lcPrChild)
SELECT (lnThAlias)
*-- end of lfBlankEnd.

*!*************************************************************
*! Name      : lfCollectP
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/15/97
*! Purpose   : Collect prepak data, and it used in lpshow procedure.
*!           : Both Extended and Single size Scale, View and Edit modes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfCollectS,lfActBrow2,lfActBrow3
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCollectP()
*!*************************************************************
FUNCTION lfCollectP
PrevAlias = SELECT(0)

*-- if extended sizes scale, fill parent prepak file.
IF llExtSize

  *E301209,1 Change collect condition [begin]
  *SELECT DISTINCT Prepak     ;
  *FROM SCALE                 ;
  *WHERE Scale = &lcSeekPrep  ;
  *  AND Type  = "P"          ;
  *INTO CURSOR tempPar

  SELECT DISTINCT Prepak     ;
  FROM SCALE                 ;
  WHERE Scale = laData[1]    ;
    AND Type  = "P"          ;
  INTO CURSOR tempPar
  *E301209,1 Change collect condition [end  ]
    
  SCAN 
    INSERT INTO (lcPrParent)                ;
                (Scale,prepak)              ;
         VALUES (&lcSeekPrep,tempPar.prepak)     
  ENDSCAN
  USE IN tempPar
ENDIF

*-- lcColCond : Condition to fill child prepak file.
lcColCond = IIF(llExtSize,'Type+Scale+Prepak = "P"+ladata[1]',;
                          [Type+Scale+Prepak = "P"])
   
*E301209,1 Change collectection order [begin]
*-- Fill child prepak file.
*SELECT Prepak,Scale,        ;
*  cDim1,cDim2,PpTot,        ;  
*  pP1,pP2,pP3,pP4,          ;
*  pP5,pP6,pP7,pP8,          ;
*  Sz1,Sz2,Sz3,Sz4,          ;
*  Sz5,Sz6,Sz7,Sz8,          ;
*  9999999999 AS nScRec,     ; 
*  "S" AS cStatus            ;
*FROM SCALE                  ;
*WHERE &lcColCond            ;
*ORDER BY prepak ,           ;
*         cDim1 , cDim2      ;
*INTO TABLE (gcWorkDir+lcPrChild)

SELECT Prepak,Scale,        ;
  cDim1,cDim2,PpTot,        ;  
  pP1,pP2,pP3,pP4,          ;
  pP5,pP6,pP7,pP8,          ;
  Sz1,Sz2,Sz3,Sz4,          ;
  Sz5,Sz6,Sz7,Sz8,          ;
  9999999999 AS nScRec,     ; 
  "S" AS cStatus            ;
FROM SCALE                  ;
WHERE &lcColCond            ;
ORDER BY Scale,prepak ,     ;
         cDim1 , cDim2      ;
INTO TABLE (gcWorkDir+lcPrChild)
*E301209,1 Change collectection order [end  ]

IF llExtSize
  INDEX ON nScRec TAG nScRec

  *E301209,1 Change index expression [begin]
  *INDEX ON LEFT(Scale,1)+Prepak+cDim1+cDim2 TAG ChildTag
  INDEX ON Scale+Prepak+cDim1+cDim2 TAG ChildTag
  *E301209,1 Change index expression [end  ]  
  
ELSE  && Single size scale.
  INDEX ON Scale+Prepak+cDim1+cDim2 TAG ChildTag
ENDIF 

*-- Fill Sizes data to used it in showing in prepak folder.
*-- also calculate Total quantity and save record numbers which
*-- later use in mainpulating with prepak lines when dealing with 
*-- corressponding scale lines.
IF llExtSize
  SELECT (lcPrParent)
  SCAN
    STORE 0 TO lnTotalPp,lnPpRecNo

    SELECT (lcPrChild)
    SCAN FOR &lcPrChild..Prepak = &lcPrParent..Prepak
      lnTotalPp = lnTotalPp + &lcPrChild..PpTot
      lnPpRecNo = lnPpRecNo + 1
      = lfCollectS()	&& Used to collect sizes coressponding to prepaks.
    ENDSCAN
    SELECT (lcPrParent)
    REPLACE &lcPrParent..PpTot WITH lnTotalPp
  ENDSCAN

  *E301209,1 go top because no status = 'D' lines.
  *= &lcSeekCond
  GO TOP
  
  = lfActBrow2()
ELSE	&& Single size scale.
  STORE 0 TO lnPpRecNo  && we don't need record number in single size scale.
  SELECT (lcPrChild)
  SCAN
    IF laScrMode[3]
      lnSelect = SELECT(0)
      SELECT (lcScaline)
      LOCATE FOR cdim1+cdim2 = &lcPrChild..cdim1+&lcPrChild..cdim2 AND ;
                 scale = &lcPrChild..Scale
      lnPpRecNo = RECNO(lcScaline)
      SELECT (lnSelect)
    ENDIF
    = lfCollectS()
  ENDSCAN
  = &lcSeekCond
ENDIF

= lfActBrow3()
SELECT (PrevAlias)
*-- end of lfCollectP.

*!*************************************************************
*! Name      : lfCollectS
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/15/97
*! Purpose   : Collect corressponding sizes for prepak data.
*!           : Both Extended and Single size Scale, View and Edit modes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfRepSizes
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCollectS()
*!*************************************************************
FUNCTION lfCollectS
= SEEK("S"+&lcPrChild..Scale,'SCALE')

*-- if size8 is empty.
IF EMPTY(Scale.Sz8)
  *-- Blank data when we collect it. [begin]
  SCATTER FIELDS Pp2,Pp3,Pp4,Pp5,Pp6,Pp7,Pp8 TO laTempPps
  SCATTER FIELDS Scale,Prepak,cDim1,cDim2,cStatus,Pp1,PpTot TO laTmpChild
  BLANK
  GATHER FROM laTmpChild FIELDS Scale,Prepak,cDim1,cDim2,cStatus,Pp1,PpTot

  = lfRepSizes()    && Replace prepak sizes.

  FOR lnEnds = 2 TO 8
    lcSzFdEn   = "&lcPrChild..Sz" + ALLTRIM(STR(lnEnds))
    lcPrepFdEn = "&lcPrChild..Pp" + ALLTRIM(STR(lnEnds))
    IF !EMPTY(&lcSzFdEn)
      REPLACE (lcPrepFdEn) WITH laTempPps[lnEnds-1]
    ELSE
      EXIT  
    ENDIF
  ENDFOR
  *-- Blank data when we collect it. [end]
ELSE
  = lfRepSizes()
ENDIF
*-- end of lfCollectS.

*!*************************************************************
*! Name      : lfCollectS
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/15/97
*! Purpose   : Replace sizes in child prepak file from corressponding
*!           : scale file.
*!           : Both Extended and Single size Scale, View and Edit modes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCollectS()
*!*************************************************************
FUNCTION lfRepSizes
REPLACE &lcPrChild..nScRec WITH lnPpRecNo ,;
        &lcPrChild..Sz1 WITH Scale.Sz1    ,;
        &lcPrChild..Sz2 WITH Scale.Sz2    ,;
        &lcPrChild..Sz3 WITH Scale.Sz3    ,;
        &lcPrChild..Sz4 WITH Scale.Sz4    ,;
        &lcPrChild..Sz5 WITH Scale.Sz5    ,;
        &lcPrChild..Sz6 WITH Scale.Sz6    ,;
        &lcPrChild..Sz7 WITH Scale.Sz7    ,;
        &lcPrChild..Sz8 WITH Scale.Sz8
*-- end of lfRepSizes.

*!*************************************************************
*! Name      : lfAddSclLn
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/15/97
*! Purpose   : Add new scale line
*!           : Both Extended and Single size Scale, View and Edit modes.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfAddSclLn()
*!*************************************************************
*
FUNCTION lfAddSclLn
SELECT (lcScaline)
APPEND BLANK		&& Add new line to temporary scale file

*-- if Edit Mode
IF laScrMode[3]
  REPLACE &lcScaline..cStatus   WITH "A"
ENDIF  
lnMarker = RECNO(lcScaline)

IF llExtSize
  = lfRefExtD('ENABLE')

  SHOW GET pbMore   DISABLE		&&Disable <More> because we are at size1.

  *-- if 3 dimension assign current popup value to dimension 1 description.
  IF laData[3] = 3
    lcGetDim1 = laDim1[lnPosDim1]
    REPLACE &lcScaline..cDim1 WITH lcGetDim1
  ENDIF
  *-- if 1 dimension disable <New> button, because we use <More> button
  *-- to add new lines.
  IF laData[3] = 1
    SHOW GET pbNew  DISABLE
    STORE 'DISABLE' TO lcNews
  ENDIF
ELSE
  =lfRefSinD(.T.)	&& Enable edit region objects.
  _CUROBJ = OBJNUM(lcScale)
ENDIF

*-- Change title of sizes to start with 1.
IF lnSize != 1
  lnSize = 1
  = lfRefresh(lcWinC12)

  SHOW GET lcGetDim1
  SHOW GET lcGetDim2

ENDIF  
SHOW GET pbRemove ENABLE		&&You can remove the added line or others.
STORE 'ENABLE' TO lcRemoves
SHOW WINDOW (lcBrowTi1) REFRESH
*-- end of lfAddSclLn.

*!*************************************************************
*! Name      : lfvExtndCd
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/19/99
*! Purpose   : Extended scale code validation
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvExtndCd()
*!*************************************************************
*
FUNCTION lfvExtndCd
IF EMPTY(lcScaleKey) OR LEN(ALLTRIM(lcScaleKey)) < lnExtndLen
  IF !EMPTY(lcScaleKey)
    *Message : 42172, Scale ID should not be less than 2 characters.
    *Button  : 00000, <Ok>
    =gfModalGen('TRM42172B00000','ALERT','Extended Scale')
     _CUROBJ  = _CUROBJ
  ENDIF
  RETURN
ENDIF  

*-- Validation section [Begin]
SELECT (lcScaline)
PRIVATE lcCurrRec , llUpdTemp , llContVld

llUpdTemp = .F.
llContVld = .T.

lcCurrRec = SCALE+CDIM1+CDIM2

*IF llMoreClck
*  LOCATE FOR Scale  > lcScaleKey
*  lcNxtGrpCd = RIGHT(SCALE,2)
*  SKIP -1
*  lcLstGrpCd = RIGHT(SCALE,2)
*  IF (lcScaleKey <= lcLstGrpCd) OR (!EMPTY(lcNxtGrpCd) AND (lcScaleKey >= lcNxtGrpCd))
*    WAIT WINDOW 'Scale code must be in Range between ' + lcLstGrpCd + ' And ' + lcNxtGrpCd
*    _CUROBJ = _CUROBJ
*    llContVld = .F.
*  ENDIF
*ENDIF

IF llContVld
  IF SEEK(laData[1] + lcScaleKey)

    *-- The following message is 
    *-- "Scale XXX is already exists."
    *--          < Ok > 
    = gfModalGen('TRM42103B42000','Dialog',laData[1] + lcScaleKey)  
    lcScaleKey = ''
    _CUROBJ = _CUROBJ
  
  ELSE

    llUpdTemp = .T.
    lcExKeySta = 'DISABLE'                && Extended Code Object State (Enable/Disable)
    SHOW GET lcScaleKey &lcExKeySta          && Extended Code Object.
    _CUROBJ = _CUROBJ + 1                 && Go to next object

  ENDIF
  = SEEK(lcCurrRec)
  IF llUpdTemp

    llMoreClck = .F.
    REPLACE SCALE WITH laData[1] + lcScaleKey

    *-- You find prepaks for this scale Add new line(S)
    *-- to temporary prepak file(S)
    IF !EOF(lcPrParent)
      *-- llNewPpLn : Flag serve the lfAddLineP() assigning values
      *--           : to cDim1 and cDim2 
      llNewPpLn = .T.
      = lfAddLineP()
      SELECT (lcScaline)
    ENDIF  && Endif You find prepaks for this scale

    SHOW WINDOW (lcBrowTi1) REFRESH
  
  ENDIF

ELSE

  = SEEK(lcCurrRec)

ENDIF  

*-- Validation section [End  ]
*-- end of lfvExtndCd.
*!*************************************************************
*! Name      : lfStyUpdt
*! Developer : Abd Elrahim Koura -KAM
*! Date      : 14/02/2001
*! Purpose   : UpDate style file for new extended size scale
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfStyUpdt()
*!*************************************************************
*!B802855
*!*************************************************************
FUNCTION lfStyUpdt
PRIVATE lcOldArea,lcTmpFiel,lnRecNo,lcKeyScale

lcOldArea=SELECT()
lnRecNo  =RECNO()
lcTmpFile=gfTempName()
*--get the first scale from scale file
SELECT SCALE
IF !SEEK('S'+laData[1])
   WAIT 'not found ' WINDOW NOWAIT 
ENDIF

*--lcSearchKey contain first scale
lcSearchKey=SCALE.Scale
*--collect all stles with first scale from style file
SELECT * FROM STYLE WHERE STYLE.SCALE=lcSearchKey INTO CURSOR (lcTmpFile)

*B037350,1 MHM 02/09/2004 Adding a new size to a size scale not creating style records[Start]
IF EOF(lcTmpFile)
  SELECT SCALE
  IF !EOF()
    SKIP
  ENDIF
  lcSearchKey=SCALE.Scale
  SELECT * FROM STYLE WHERE STYLE.SCALE=lcSearchKey INTO CURSOR (lcTmpFile)
ENDIF
*B037350,1 MHM 02/09/2004 [End]

*--navigate through collected file 
SELECT (lcScaline)
SCAN FOR EMPTY(TYPE)
  lcKeyScale=SCALE
  SELECT(lcTmpFile)
  SCAN
    SCATTER MEMVAR 
    STORE 0 TO m.nMCost1,m.nMcost2,m.nMcost3,m.nMcost4,m.nMcost5,;
    m.nICost1,m.nIcost2,m.nIcost3,m.nIcost4,m.nIcost5,;
    m.nPrCost2,m.nPrCost3,m.nPrCost4,m.nPrCost5,;
    m.Ord1,m.Ord2,m.Ord3,m.Ord4,m.Ord5,m.Ord6,m.Ord7,m.Ord8,m.TotOrd,;
    m.WIP1,m.WIP2,m.WIP3,m.WiP4,m.WIP5,m.WIP6,m.WIP7,m.WIP8,m.TotWIp,;
    m.Stk1,m.Stk2,m.Stk3,m.Stk4,m.Stk5,m.Stk6,m.Stk7,m.Stk8,m.TotStk,;
    m.Alo1,m.Alo2,m.Alo3,m.Alo4,m.Alo5,m.Alo6,m.Alo7,m.Alo8,m.TotAlo,;
    m.Shp1,m.Shp2,m.Shp3,m.Shp4,m.Shp5,m.Shp6,m.Shp7,m.Shp8,m.TotShp,;
    m.Ret1,m.Ret2,m.Ret3,m.Ret4,m.Ret5,m.Ret6,m.Ret7,m.Ret8,m.TotRet,;
    m.Plan1,m.Plan2,m.Plan3,m.Plan4,m.Plan5,m.Plan6,m.Plan7,m.Plan8,m.TotPlan
    *--update m.style with new size
    m.Style=SUBSTR(m.Style,1,lnExtScPos-1)+lcKeyScale
    m.Scale=lcKeyScale
    *--insert new style into style file and stydye file
    INSERT INTO STYLE FROM MEMVAR 
    DO gpAdStyWar WITH m.Style,SPACE(10),m.cdefware    
   ENDSCAN 
ENDSCAN
SELECT(lcOldArea)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF 
RETURN
*--end function lfStyUpdt
*!*************************************************************
*! Name      : lfInlcscal
*! Developer : Abd Elrahim Koura -KAM
*! Date      : 14/02/2001
*! Purpose   : check for new exetended size scale
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfInlcscal()
*!*************************************************************
*!B802855
*!*************************************************************
FUNCTION lfInlcscal 
PRIVATE lcOldArea,lnRecNo,llRetVal
lcOldArea=SELECT()
lnRecNo  =RECNO()
SELECT (lcScaline)
*-- check for scale with type empty which means new size add and not removed
LOCATE FOR EMPTY(TYPE)
llRetVal=FOUND()
SELECT(lcOldArea)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF 
*B038431,1 MHM 9/13/2004 check if there is styles assigned to this scale or not [Start]
lcTmpfil=gfTempName()
*--get the first scale from scale file
SELECT SCALE
=SEEK('S'+laData[1])

*--lcSearchKey contain first scale
lcSearchScl=SCALE.Scale

SELECT * FROM STYLE WHERE STYLE.SCALE=lcSearchScl INTO CURSOR (lcTmpFil)

IF EOF(lcTmpFil)
  llRetVal = .F.
ENDIF
*B038431,1 MHM [End]

RETURN llRetVal
*--end function lfInlcscal

*!*************************************************************
*! Name      : lfGetSclPo
*! Developer : Abd Elrahim Koura -KAM
*! Date      : 14/02/2001
*! Purpose   : get scale position
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetSclPo()
*!*************************************************************
*!*B802855
*!*************************************************************
FUNCTION lfGetSclPo
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnExtScPos = IIF(lnExtScPos=0,laItemSeg[lnCount,4],lnExtScPos)
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*--end function lfGetSclPo


