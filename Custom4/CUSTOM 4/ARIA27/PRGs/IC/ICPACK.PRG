*:************************************************************
*: Program file  : icPack
*: Program desc. : Create Pack_id for group of styles on 
*:                 specific account
*: For screen    : icPack
*:        System : Aria Advantage Series.
*:        Module : Material (IC).
*:     Developer : Ahmed Amer (AHM)
*:************************************************************
*: Calls : 
*:         Procedures : lpShow,lpSavscr
*:         Functions  : gfSetup,gfOpenFile,lfvData_1,lfvData_2,
*:                    : gfCodes,gfSeekRec,lfWHBrow,lfWBrow,
*:                      lfTrap,lfClrTrap  
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Example : DO icPack
*:************************************************************
*: Modifications :
*: B603823,1 08/21/2000  AMH Update SKU templete File if it is Empty
*: B803692,4 09/24/2000 Ramy Update the Desc. field when saving the screen
*: B803703,1 10/02/2000  ABD Fix bug that you must enter all index key
*: B803703,1                  "P"+Account code+Pack ID when search.
*: C102082,1 [SSH] 02/04/2001 Call the program with parameter
*: B605562,1 ASH 02/17/2002 Fix the bug of not delete the pack from spck_lin.
*: B605894,1 ASH 04/30/2002 The style browse always at BOF even you entered the first chr of the style.
*: B606590,1 ASH 01/01/2003 Fix the bug of saving pack without lines.
*: B606909,1 ABD 06/15/2003 Fix bug that the pack id can not be re-enter in ic module   
*: B123063,1 MHM 13/07/2004 Fix Bug of 'Alias Not Found' in case type the style 
*: B123063,1 MHM            without color and then press on browse button.
*: C123656,1 NNA 08/15/2004 show a screen to input a predicted Pack Total For NikNak
*:************************************************************
*: C102082,1 02/04/2001 Call the program with parameter
PARAMETER lcpAcct , lcpPack
*: C102082,1 [END]

*-- laData      Array that hold the values of the fields that we use from base file
*-- laScrMode   Array that hold the screen modes
*-- laKeyField  Array that hold the name of the fields that represent screen keys
*-- laCodInfo   Array that hold information about the fields that use code file
*-- laDiv       Array that hold the divisions codes
*-- laSea       Array that hold the seasons codes
*-- lnPTotQt    Array from one element to use in SQL select to hold the sum ;
                of totqty from 'spck_lin' and it is treated as numeric variable

DIMENSION laData[1],laScrMode[4],laKeyField[2,4],laCodInfo[2,10],;
          laDiv[1],laSea[1],lnPTotQt[1]

*-- lcIcPack1   Variable that hold the name of screen 'icpack1'
*-- lcIcPack2   Variable that hold the name of screen 'icpack2'
*-- lcIcPack3   Variable that hold the name of screen 'icpack3' 
*-- lcIcPack4   Variable that hold the name of screen 'icpack4'
*-- lcScFields  Variable that hold the name of the used fields from basefile
*-- lcStyle     Variable that hold the style code
*-- lcStyDesc   Variable that hold the desc of the style
*-- lcLinFil    Variable that hold the name of the temp file that is used for 'spck_lin' file
*-- lcStyPic    Variable that hold the style picture(format)
*-- lcStyTtl    Variable that hold the style title

STORE SPACE(0) TO laData,laKeyField,laCodInfo,laDiv,laSea,;
                  lcIcPack1, lcIcPack2, lcIcPack3, lcIcPack4,lcScFields,;
                  lcStyle,lcStyDesc,lcLinFil,lcStyPic,lcStyTtl

*-- lnStyleWid  Variable that hold the width of the style
*-- lnBrRecNo   Variable that hold the no of the current record of the browse screen

STORE 0 TO lnPTotQt,lnStyleWid,lnBrRecNo

*-- llBrowse    Variable that used to show if any browse button is clicked
*-- llNoShow    Variable that prevent or enable the program go into    ;
                lpshow once the program is executed;
                (.F. go in lpshow,.T. don't)
*-- llWFirst    Variable that showes if we enter the screen for the first time
*--             or we back to it from another session
*-- llTemplOpn  Variable that showes if SkuTmpl file is opened by this program 
*--             or not

STORE .F. TO llBrowse,llNoShow,llWFirst

*B605562,1 ASH 02/17/2002 (Begin) Use the local delete function instead of the global delete one.
laDefproc[7] = .F.                 && This is to prevent the global delete
*B605562,1 ASH 02/17/2002 (End)
laDefproc[9] = .F.                 && This is to prevent the global save
lcBrTtl    = 'Pack Details'        && This is to hold the title of the browse screen
 
laKeyField[1,1] = "laData[1]"
laKeyField[1,2] =.F.
laKeyField[1,3] = "Spck_Hdr"
laKeyField[1,4] = 1

laKeyField[2,1] = "laData[2]"
laKeyField[2,2] = .T.
laKeyField[2,3] = "Spck_Hdr"
laKeyField[2,4] = 2

IF !gfSetup()
  RETURN
ENDIF  

*-- This scattering is used to give empty value for sizes label and values 
*-- at starting the program
SELECT Spck_Lin
SCATTER FIELDS Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty MEMVAR BLANK
SELECT Scale
SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR BLANK

IF !WEXIST(gcBaseWind)
  
  *B603823,1 08/21/2000  AMH Update SKU templete File if it is Empty [Start]
  lnAlias = SELECT()
  llTemplOpn =gfOpenFile(gcDataDir+'SkuTmpl',gcDataDir+'SkuTmpl','SH')
  LOCATE
  IF EOF() .OR. !SEEK('P'+'DEF')
    APPEND BLANK
    REPLACE TYPE     WITH 'P'                    ,;
            CSKUCODE WITH 'DEF'                  ,;
            DESC     WITH 'Default pack        ' ,;
            FIELD1   WITH 'PACK ID   '           ,;
            LEN1     WITH 16                     ,;
            LEN2     WITH 0                      ,;
            LEN3     WITH 0                      ,;
            LEN4     WITH 0
  ENDIF
  IF llTemplOpn
    =gfCloseFile('SkuTmpl')
    llTemplOpn = .F.
  ENDIF
  SELECT (lnAlias)
  *B603823,1 08/21/2000  AMH Update SKU templete File if it is Empty [End  ]

  lcStyPic   = gfItemMask("PI")
  lcStyTtl   = gfItemMask("HI")
  lnStyleWid = LEN(lcStyPic)  
  
  lcIcPack1  = gfTempName()
  lcIcPack2  = gfTempName()
  lcIcPack3  = gfTempName()
  lcIcPack4  = gfTempName()
  
  lcLinFil   = gfTempName()         && Hold the temp file of warehouse.
  
  lcScFields = "Account, Pack_ID, Desc, cDivision, Season"
  SELECT Spck_Hdr
  SCATTER FIELDS &lcScFields TO laData BLANK

  SELECT Spck_Lin
  = AFIELDS(laFileStru)  
  lnFileStru = ALEN(laFileStru,1)
  CREATE TABLE (gcWorkDir + lcLinFil) FROM ARRAY laFileStru
  INDEX ON Type+Account+Pack_ID+Style TAG StyPack ADDITIVE
  llWFirst = .T.
ELSE  
  llWFirst = .F.
ENDIF

laCodInfo[1,01] = "CDIVISION"               && Field Name
laCodInfo[1,02] = "laDiv"                   && Array Name
laCodInfo[1,03] = "lnDiv"                   && Popup Name
laCodInfo[1,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
laCodInfo[1,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[1,06] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[1,07] = "Spck_Hdr"                && Alternative File (For default val.)
laCodInfo[1,08] = "Spck_Hdr"                && Use this index for the Alternative file.
laCodInfo[1,09] = "'P'+laData[1]+laData[2]" && Seek this expretion.
laCodInfo[1,10] = "cDivision"               && Alternative Field Name

laCodInfo[2,01] = "SEASON"                  && Field Name
laCodInfo[2,02] = "laSea"                   && Array Name
laCodInfo[2,03] = "lnSea"                   && Popup Name
laCodInfo[2,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
laCodInfo[2,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[2,06] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[2,07] = "Spck_Hdr"                && Alternative File (For default val.)
laCodInfo[2,08] = "Spck_Hdr"                && Use this index for the Alternative file.
laCodInfo[2,09] = "'P'+laData[1]+laData[2]" && Seek this expretion.
laCodInfo[2,10] = "Season"                  && Alternative Field Name

SELECT Spck_Lin
SET FILTER TO Type = 'P'
SET ORDER TO Spck_Lin
SELECT Spck_Hdr
SET FILTER TO Type = 'P'
SET RELATION TO Type+Account+pack_id INTO Spck_lin

PUSH KEY                                     && To save the the current on key label
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl) && To activate the browse screen when pressing ALT+B
DO (gcScrDir + gcWinAppl + '\ICPACK.SPR')    && calling the screen Mainvlo
RELEASE WINDOW (lcBrTtl)
POP KEY                                      && To Restore the previous assignments for on key label

SELECT Spck_Lin
SET FILTER TO 

SELECT Spck_Hdr
SET FILTER TO 
SET RELATION TO

SELECT Style
SET RELATION TO

*-- If we realy quitting the screen 
IF glQuitting                 
  IF USED(lcLinFil)
    USE IN (lcLinFil)
  ENDIF
  ERASE (gcWorkDir+lcLinFil +".DBF")
  ERASE (gcWorkDir+lcLinFil +".CDX")
ENDIF  

*!*************************************************************
*! Name      : lfPackBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse the pack_id data
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfwBrow,lfvBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPackBrow()
*!*************************************************************

FUNCTION lfPackBrow

IF WEXIST(lcBrTtl)
  RELEASE WINDOW (lcBrTtl)
ENDIF

lnCurAlias = SELECT(0)
IF laScrMode[2]
  SELECT SPck_Hdr
  GOTO RECNO()
  lcFileName = "Spck_Lin"
ELSE
  lcFileName = lcLinFil
  SELECT (lcFileName)
  GOTO TOP
ENDIF  
SELECT (lcFileName)
lnBrRecNo = RECNO()

*-- The treating of lcForExp as it comes because in view mode we browse
*-- from the master file'spck_lin' which hold the data of all pack_id
*-- but in other modes we browse from tempfile 'lcLinFil' which hold only
*-- the data for this pack_id
lcForExp = IIF(laScrMode[2],;
           "Type+Account+Pack_ID+Style = 'P'+laData[1]+laData[2]+SPACE(0)",;
           "Type+Account+Pack_ID+Style = 'P'+SPACE(0) +SPACE(0) +SPACE(0)")

BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecNo,'>',' '):1:R:H=' ':W=.F.,;
              Style:R:H=lcStyTtl, Qty1 , Qty2 , Qty3 , Qty4 , ;
              Qty5 , Qty6:R , Qty7:R , Qty8:R , TotQty:R , ;
              Style.TotWip:R:H="Tot. WIP"    ,;
              Style.TotStk:R:H="Tot. Stock"  ,;
              Style.TotOrd:R:H="Tot. Orders",;
              Style.Wip1:R:H="WIP1",Style.Wip2:R:H="WIP2",;
              Style.Wip3:R:H="WIP3",Style.Wip4:R:H="WIP4",;
              Style.Wip5:R:H="WIP5",Style.Wip6:R:H="WIP6",;
              Style.Wip7:R:H="WIP7",Style.Wip8:R:H="WIP8",;
              Style.Stk1:R:H="STK1",Style.Stk2:R:H="STK2",;
              Style.Stk3:R:H="STK3",Style.Stk4:R:H="STK4",;
              Style.Stk5:R:H="STK5",Style.Stk6:R:H="STK6",;
              Style.Stk7:R:H="STK7",Style.Stk8:R:H="STK8",;                                          
              Style.Ord1:R:H="ORD1",Style.Ord2:R:H="ORD2",;
              Style.Ord3:R:H="ORD3",Style.Ord4:R:H="ORD4",;
              Style.Ord5:R:H="ORD5",Style.Ord6:R:H="ORD6",;
              Style.Ord7:R:H="ORD7",Style.Ord8:R:H="ORD8" ;
         FOR &lcForExp ;
         SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
         TITLE(lcBrTtl) WHEN lfwBrow() VALID :F lfVBrow()    ;         
         WINDOW (lcIcPack2) IN WINDOW (gcBaseWind)         
= lfwBrow()

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : to make the sizes and quantities fields match what in the file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrow()
*!*************************************************************

FUNCTION lfwBrow

lcFilToUse = IIF(laScrMode[2], "Spck_Lin", lcLinFil)

SELECT Scale
SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR BLANK

SELECT (lcFilToUse)
lnBrRecNo  = RECNO()
SCATTER FIELDS Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty MEMVAR BLANK

IF EOF() OR BOF() OR DELETED()
  SHOW GET pbRem DISABLE
  lcStyStat = "DISABLE"
  lcPopstat = IIF(laScrMode[4],"ENABLE","DISABLE")
ELSE

  lcPopstat = "DISABLE"
  SCATTER FIELDS Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty MEMVAR
  lcRemStat = IIF((laScrMode[3] OR laScrMode[4]),"ENABLE","DISABLE")

  SHOW GET pbRem &lcRemStat

  IF laScrMode[4] OR laScrMode[3]
    lcStyStat = IIF(EMPTY(m.Style),"ENABLE","DISABLE")
  ELSE
    lcStyStat = "DISABLE"
  ENDIF    

  IF !EMPTY(m.Style) AND SEEK(m.Style,"Style")
    IF SEEK ('S'+Style.Scale,"Scale")
      SELECT SCALE
      SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR
    ENDIF
  ENDIF
ENDIF
SHOW GET m.Style   &lcStyStat
SHOW GET pbStyBrow &lcStyStat
SHOW GET lnDiv     &lcPopStat
SHOW GET lnSea     &lcPopStat
SHOW GET m.Qty1
SHOW GET m.Qty2
SHOW GET m.Qty3
SHOW GET m.Qty4
SHOW GET m.Qty5
SHOW GET m.Qty6
SHOW GET m.Qty7
SHOW GET m.Qty8

SHOW WINDOW (lcBrTtl) REFRESH SAME

=lfRefresh()

*!*************************************************************
*! Name      : lfvNew
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : add new record in tempfile
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvNew()
*!*************************************************************

FUNCTION lfvNew
PRIVATE lnAlias

lnAlias = SELECT(0)

SELECT (lcLinFil)
IF !SEEK('P'+laData[1]+laData[2]+SPACE(19))
  APPEND BLANK
  REPLACE Type    WITH 'P'      ,;
          Account WITH laData[1],;
          Pack_ID WITH laData[2]
ENDIF
= lfwBrow()
_CUROBJ = OBJNUM(m.Style)

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate the style and add it in tempfile
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvStyle()
*!*************************************************************

FUNCTION lfvStyle

*B123063,1 MHM 13/07/2004 Added check that you should press enter or tab to validate[Start].
IF llBrowse OR LASTKEY()=13 OR LASTKEY()=9
  *--Ok start validate.
ELSE
  RETURN
ENDIF
*B123063,1 MHM 13/07/2004  [End].

IF llBrowse .OR. !EMPTY(m.Style)
  lnAlias = SELECT(0)
  SELECT (lcLinFil)
  IF llBrowse OR !SEEK(m.Style,"Style")
    llBrowse = .F.
    *B605894,1 ASH 04/30/2002 (Begin) The style browse always at BOF even you entered the first chr of the style.
    *m.Style  = gfStyBrw('I',"","",.F.)
    m.Style  = gfStyBrw('I',m.Style,"",.F.)
    *B605894,1 ASH 04/30/2002 (End)
  ENDIF

  IF !EMPTY(m.Style)
    IF Style.cDivision # laData[4]
      IF laCodInfo[1,4] = "L"
        lcDivision = laDiv[lnDiv,1]
      ELSE
        lcDivision = laDiv[1,1]
      ENDIF
      lcMessStr = m.Style+"|"+"division"+"|"+lcDivision
    ENDIF
    IF Style.Season   # laData[5]
      IF laCodInfo[2,4] = "L"
        lcSeason = laSea[lnSea,1]
      ELSE
        lcSeason = laSea[1,1]
      ENDIF
      lcMessStr = m.Style+"|"+"season"+"|"+lcSeason
    ENDIF
    IF Style.cDivision # laData[4] OR Style.Season   # laData[5]
      *-- Conflict! Style X restricted to (cdivision/season) Y
      *-- <OK>
      llNoThing = gfModalGen("INM42096B42001","Dialog",lcMessStr)
      m.Style   = SPACE(19)
      _CUROBJ   = _CUROBJ
    ELSE
      IF !EMPTY(m.Style) AND SEEK('P'+laData[1]+laData[2]+m.Style,lcLinFil)
        *-- "Style already entered! cannot add a second entry.."
        *-- <Ok>
        llNoThing = gfModalGen("INM42093B42001","Dialog",m.Style)  
        m.Style   = SPACE(19)
        _CUROBJ   = _CUROBJ
      ELSE
        =SEEK('P'+laData[1]+laData[2]+SPACE(19), lcLinFil)
        REPLACE Style WITH m.Style 
        = lfwBrow()
      ENDIF
    ENDIF
    SELECT (lnAlias)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvQuantities
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To replace the quantities filed in tempfile with
*!             quantities value
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvQuantities()
*!*************************************************************

FUNCTION lfvQuantities
PRIVATE lnAlias, lcFldName, lnOldVal, lcVarName, lnNewVal

lnAlias   = SELECT(0)
lcFldName = VARREAD()
lnOldVal  = &lcLinFil..&lcFldName
lcVarName = "m." + lcFldName
lnNewVal  = &lcVarName

IF lnNewVal < 0
  &lcVarName = 0
  *-- can not accept negative values
  = gfModalGen("INM42000B42001","Dialog") 
  _CUROBJ = _CUROBJ 
ELSE
  SELECT(lcLinFil) 
  m.TotQty  = m.TotQty - lnOldVal + lnNewVal

  *C123656,1 NNA 08/15/2004 (BEGIN) Check if the lnPTotQt has the predicated Qty. or not for NikNak
  IF ASCAN(laEvntTrig,PADR('CHKTOT',10))<>0 AND gfGetMemVar('M_ChkTotal',gcAct_Comp)  
    =gfDoTriger(lcProgName,PADR('CHKTOT',10))
  ELSE
  *C123656,1 NNA (End)

    lnPTotQt  = lnPTotQt - &lcLinFil..TotQty + m.TotQty

  *C123656,1 (Begin)    
  ENDIF
  *C123656,1 (End)  
  
  REPLACE &lcFldName WITH &lcVarName ,;
          TotQty     WITH m.TotQty

  SHOW WINDOW (lcBrTtl) REFRESH SAME
  =lfRefresh()
  
  SELECT(lnAlias)
ENDIF

*!*************************************************************
*! Name      : lpShow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Handling the screen mode
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfWHBrow,lfwBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************

PROCEDURE lpShow
*: C102082,1 02/04/2001 Add conditoin in case if call with paramete

IF !laScrMode[2] .AND. TYPE("lcpAcct") = "C" .AND. TYPE("lcpPack") = "C"
  laData[1] = lcpAcct
  laData[2] = lcpPack
  KEYBOARD "{ENTER}"
  lcDummy   = lfvData_2()
ENDIF
*: C102082,1 [END]
PRIVATE lnCurAlias,lcFilToUse
lcFilToUse = IIF(laScrMode[2], "Spck_Lin", lcLinFil)

lnCurAlias = SELECT(0)

SELECT Spck_Hdr
SET ORDER TO Spck_Hdr
SET RELATION TO Type+Account+pack_id INTO &lcFilToUse
SELECT(lcFilToUse)
SET RELATION TO Style INTO Style
SELECT Style
SET ORDER TO Style
SET RELATION TO 'S'+Scale INTO Scale

lcStyStat  = "DISABLE"
DO CASE
  *-- Select mode
  CASE laScrMode[1]
    = gfwCodePop(@laCodInfo , "CDIVISION" , "N")
    SHOW GET lnDiv
    = gfwCodePop(@laCodInfo , "SEASON" , "N")
    SHOW GET lnSea
    STORE 0 TO lnPTotQt
    SELECT(lcLinFil)
    SCATTER FIELDS Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty MEMVAR BLANK
    SELECT Scale
    SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR BLANK

    SELECT Spck_Hdr
    SCATTER FIELDS &lcScFields TO laData BLANK
    _CUROBJ = OBJNUM(laData[1])
    STORE SPACE(0) TO lcStyle
    SELECT (lcLinFil)
    ZAP
    
  *-- View mode
  
  CASE laScrMode[2]
    SELECT Spck_Hdr
    SCATTER FIELDS &lcScFields TO laData
    
    *-- Fill codes popups with its real values.
    = gfwCodePop(@laCodInfo, "CDIVISION", "T")
    SHOW GET lnDiv
    = gfwCodePop(@laCodInfo , "SEASON" , "T")
    SHOW GET lnSea

    SELECT SUM(TotQty) FROM Spck_Lin INTO ARRAY lnPTotQt ;
      WHERE Type+Account+Pack_ID+Style LIKE 'P'+laData[1]+laData[2]+'%'

  *-- Edit mode
  CASE laScrMode[3]
    *-- create the file only if you are enter the screen for first
    *-- time, if you coming from another session in edit mode
    *-- don't create the file from the beginning
    IF llWFirst
      SELECT * FROM SPck_Lin;
          WHERE Type + Account   + Pack_ID   + Style;
          LIKE  'P'  + laData[1] + laData[2] + '%';
          INTO DBF (gcWorkDir + lcLinFil)
        INDEX ON Type+Account+Pack_ID+Style TAG StyPack ADDITIVE
      _CUROBJ = OBJNUM(laData[3])      
    ENDIF
    

  *-- Add mode
  CASE laScrMode[4]
    lcPopMode = IIF(EMPTY(laData[4]),'D',"V,"+laData[4])
    = gfwCodePop(@laCodInfo, "CDIVISION" , lcPopMode)
    laData[4] = laDiv[lnDiv,2]
    lcPopMode = IIF(EMPTY(laData[5]),'D',"V,"+laData[5])
    = gfwCodePop(@laCodInfo , "SEASON"   , lcPopMode)
    laData[5] = laSea[lnSea,2]
    SHOW GET lnDiv
    SHOW GET lnSea
    
ENDCASE

= lfPackBrow() AND lfRefresh()

SHOW GET m.Style   &lcStyStat
SHOW GET pbStyBrow &lcStyStat

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key (Account).
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_1()
*!*************************************************************

FUNCTION lfvData_1

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

laData[1] = PADR(ALLTRIM(laData[1]), FSIZE('Account', 'Spck_Hdr'))

XACCOUNT = laData[1]
IF llBrowse OR (!EMPTY(laData[1]) AND !SEEK('M'+XACCOUNT,'Customer'))
  llBrowse = .F.
  DO CUSBROWM WITH XACCOUNT
  laData[1] = XACCOUNT
  IF EMPTY(laData[1])
    _CUROBJ = _CUROBJ
  ENDIF  
ENDIF  
=lfRefresh()

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvData_2
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key (pack id).
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_2()
*!*************************************************************

FUNCTION lfvData_2

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
laData[1] = IIF(EMPTY(laData[1]),SPACE( 0),laData[1])
laData[2] = IIF(EMPTY(laData[2]),SPACE(16),laData[2])

lnLstKey = LASTKEY()
*: C102082,1 02/04/2001 Add conditoin in case if call with paramete
*IF (lnLstKey = 13 AND !MDOWN())
IF (lnLstKey = 13 AND !MDOWN()) .OR. (TYPE("lcpPack")="C" .AND. !EMPTY(lcpPack))
  IF (TYPE("lcpPack")="C" .AND. !EMPTY(lcpPack))
    lcpPack = .F.
  ENDIF
*: C102082,1 [END]
  *-- Account is equaled by '*****' if it is left empty
  *-- to careate generic pack ID for all accounts
  laData[1] = IIF(EMPTY(laData[1]),'*****',laData[1])
  IF !SEEK('P'+laData[1]+laData[2],'Spck_Hdr') OR llBrowse
    IF !llBrowse AND !EMPTY(laData[2]) AND laData[2] # PADR('?',16)
      IF laData[1] = "*****"
        *-- THIS generic PACK_ID does not exist
        *-- < Add > < Browse > < Reenter >
        lnABR = gfModalGen("QRM42091B42003","Dialog")  
      ELSE
        *-- THIS PACK ID IS NOT FOUND DO YOU WANT TO BROWSE OR ADD
        *-- < Add > < Browse > < Reenter >
        lnABR = gfModalGen("QRM42090B42003","Dialog",'Pack ID'+'|'+laData[1])
      ENDIF
      DO CASE
        CASE lnABR = 1            &&Add
          SHOW GET laData[1]
          *-- Would you like to copy information from another Pack ID
          *-- <Yes>, <No>
          IF gfModalGen("QRM42005B42002","Dialog","Pack ID") = 1 
            = lfPackCopy()
          ELSE
            IF lfPackScr() = 1    && OK
              laScrMode    = .F.
              IF !SEEK('P'+laData[1]+laData[2],'Spck_Hdr')
                laScrMode[4] = .T.        
                _CUROBJ = OBJNUM(laData[3])
              ELSE
                laScrMode[2] = .T.
              ENDIF
              SHOW GETS
            ELSE                  && Cancel
              _CUROBJ = OBJNUM(laData[2])
            ENDIF
          ENDIF
        CASE lnABR = 2            &&Browse
          llBrowse = .F.
          = lfPackIDBrow()
        CASE lnABR = 3            &&Reenter
          STORE SPACE(0) TO laData[2]
          _CUROBJ = OBJNUM(laData[2])
      ENDCASE
    ELSE
      
      llBrowse  = .F.
      llNoThing = lfPackIDBrow()
    ENDIF
  ELSE
    laScrMode    = .F.
    laScrmOde[2] = .T.  
    SHOW GETS    
  ENDIF
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPackCopy
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : use if the user wishes to copy data for new pack from 
*!             an existing one.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPackCopy()
*!*************************************************************

FUNCTION lfPackCopy

PRIVATE llBrowse
llBrowse = .F.
lcPackID = SPACE(0)
lcWName    = "ICPCopy"
lcWSPTitl  = "Copy Information From Pack_ID"

PUSH KEY                                      && To save the the current on key label
ON KEY
DO (gcScrDir + gcWinAppl + '\ICSPCOPY.SPR')   && calling the screen ICSPCOPY
POP KEY                                       && To Restore the previous assignments for on key label

IF !EMPTY(lcPackID)
    SELECT * FROM SPck_Lin;
      WHERE Type + Account   + Pack_ID   + Style;
      LIKE  'P'  + laData[1] + lcPackID + '%';
      INTO DBF (gcWorkDir + lcLinFil)
  SELECT(lcLinFil)
  GO TOP
  REPLACE Pack_ID WITH laData[2] ALL
  INDEX ON Type+Account+Pack_ID+Style TAG StyPack ADDITIVE
  =SEEK('P'+laData[1]+lcPackID,"Spck_Hdr")  
  laData[3] = Spck_Hdr.Desc
  laData[4] = Spck_Hdr.cDivision
  laData[5] = Spck_Hdr.Season
  laScrMode    = .F.
  laScrMode[4] = .T.
  SHOW GETS

  *C123656,1 NNA 08/15/2004 (BEGIN) Store a Flag to know that this Pack ic copied for customer NikNak
  IF ASCAN(laEvntTrig,PADR('PCOPY',10))<>0 AND gfGetMemVar('M_ChkTotal',gcAct_Comp)  
    =gfDoTriger(lcProgName,PADR('PCOPY',10))
  ENDIF
  *C123656,1 NNA (End)

ELSE
  _CUROBJ = OBJNUM(laData[2])
ENDIF

*!*************************************************************
*! Name      : lfvPackID
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate the selected pack id to copy data from it
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPackID()
*!*************************************************************

FUNCTION lfvPackID

IF llBrowse OR (!EMPTY(lcPackID) AND !SEEK('P'+laData[1]+lcPackID,'Spck_Hdr'))
  llBrowse = .F.  
  = lfPackIDBrow(.T.)
ENDIF


*!*************************************************************
*! Name      : lfPackIDBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : browse all pack id to select from it
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : llFromCopy
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPackIDBrow()
*!*************************************************************

FUNCTION lfPackIDBrow
PARAMETERS llFromCopy


*-- llFromCopy Parameter that showes if the function is called from
*--            copy screen or from base screen
*--            because if it is called from copy screen we needent to
*--            change mode

PRIVATE lcBrFields ,lcBrTitle,lcKey,lnCurAlias,laRetData
PRIVATE laBrow
DIMENSION laBrow[5]
STORE SPACE(0) TO laBrow
lnCurAlias = SELECT(0)

lcBrTitle  = 'Available Packs'
lcBrFields = "Account   :R :H='Account' :16," +;
             "Pack_ID   :R :H='Pack_ID' :16," +;
             "Desc      :R :H='Desc'    :30," +;
             "cDivision :R :H='Devision':8,"  +;
             "Season    :R :H='Season'  :8"


IF !SEEK('P'+laData[1],'Spck_Hdr')
  *--There are no records to browse.
  *-- OK
  = gfModalGen("INM42097B42001","Dialog")  
  _CUROBJ = OBJNUM(laData[2])
ELSE
  *B803703,1 ABD Fix bug that you must enter all index key
  *B803703,1     "P"+Account code+Pack ID when search.
  *lcKey = "FOR Type+Account = 'P' + laData[1]"
   lcKey = "'P' + laData[1]"
  *B803703,1 ABD [End]
 
  SELECT Spck_Hdr
  =gfBrows(lcKey,"Account,Pack_ID,Desc,cDivision,Season",;
           "laBrow",lcBrTitle)

  IF !EMPTY(laBrow)
    IF llFromCopy
      lcPackID = laBrow[2]
    ELSE      
      = ACOPY(laBrow,laData)
      laScrMode    = .F.
      laScrMode[2] = .T.
      SHOW GETS
    ENDIF
  ELSE
    STORE SPACE(0) TO laData[2]
    laData[1] = IIF(laData[1]='*****',SPACE(0),laData[1])
    SHOW GET laData[1]
    _CUROBJ = OBJNUM(laData[2])
  ENDIF
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPackScr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To formalize the PACK ID templete.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPackScr()
*!*************************************************************

FUNCTION lfPackScr

PRIVATE lnChoice,lnCurAlias

lnChoice = 0

llTemplOpn =gfOpenFile(gcDataDir+'SkuTmpl',gcDataDir+'SkuTmpl','SH')

*-- The next statements to have the pak ID templete form
*-- and the picture of the templete fileds
IF SEEK('P'+Customer.Skutmpl,'SkuTmpl') OR SEEK('P'+'DEF','SkuTmpl')
  lcSPTtl = IIF(!EMPTY(SkuTmpl.Field1),ALLTRIM(SkuTmpl.Field1)+;
            IIF(!EMPTY(SkuTmpl.Field2),"-"+ALLTRIM(SkuTmpl.Field2)+;
            IIF(!EMPTY(SkuTmpl.Field3),"-"+ALLTRIM(SkuTmpl.Field3),SPACE(0)),;
            IIF(!EMPTY(SkuTmpl.Field3),"-"+ALLTRIM(SkuTmpl.Field3),SPACE(0))),;
            IIF(!EMPTY(SkuTmpl.Field2),ALLTRIM(SkuTmpl.Field2)+;
            IIF(!EMPTY(SkuTmpl.Field3),"-"+ALLTRIM(SkuTmpl.Field3),SPACE(0)),;
                ALLTRIM(SkuTmpl.Field3) ) ) + ":"
  lCSPPic = SPACE(0)
  lCSPPic = IIF(!EMPTY(SkuTmpl.Len1),PADL(lCSPPic,SkuTmpl.Len1,'!')+;    
            IIF(!EMPTY(SkuTmpl.Len2),"-"+PADL(lCSPPic,SkuTmpl.Len2,'!')+;
            IIF(!EMPTY(SkuTmpl.Len3),"-"+PADL(lCSPPic,SkuTmpl.Len3,'!'),SPACE(0)),;
            IIF(!EMPTY(SkuTmpl.Len3),"-"+PADL(lCSPPic,SkuTmpl.Len3,'!'),SPACE(0))),;
            IIF(!EMPTY(SkuTmpl.Len2),PADL(lCSPPic,SkuTmpl.Len2,'!')+;
            IIF(!EMPTY(SkuTmpl.Field3),"-"+PADL(lCSPPic,SkuTmpl.Len3,'X'),SPACE(0)),;
                PADL(lCSPPic,SkuTmpl.Len3,'!') ) )
  lnCodeTtl  = LEN(lcSPPic)
  lcSPCode   = SPACE(lnCodeTtl)
  lcSPCode   = laData[2]
  lnPackWid  = LEN(ALLTRIM(lcSPTtl))
  lcWName    = "SPTmp"
  lcWSPTitl  = "Pack_ID"
  PUSH KEY                                     && To save the the current on key label
  ON KEY
  DO (gcScrDir + gcWinAppl + '\ICSPTMP.SPR')   && calling the screen ICSPTMP
  POP KEY                                      && To Restore the previous assignments for on key label

  *C123656,1 NNA 08/15/2004 (BEGIN) Display the Total Qty. Screen for NIKNAK
  IF ASCAN(laEvntTrig,PADR('DISPSCR',10))<>0 .AND. gfGetMemVar('M_ChkTotal',gcAct_Comp) 
    =gfDoTriger(lcProgName,PADR('DISPSCR',10))
  ENDIF
  *C123656,1 NNA (End)
 
  IF lnChoice = 1
    lnSepCount = OCCURS('-',lcSPPic)
    laData[2] = SUBSTR(lcSPCode,1,lnCodeTtl-lnSepCount)
    IF !EMPTY(laData[2])
      laScrMode    = .F.
      laScrMode[4] = .T.                       && Enable Add Mode
      SHOW GETS
    ELSE
      _CUROBJ = _CUROBJ
    ENDIF
  ELSE
    STORE SPACE(0) TO laData[2]
    _CUROBJ = _CUROBJ
  ENDIF
ELSE
  *--There is no pack templete.
  *-- OK
  = gfModalGen("INM42101B42001","Dialog",'pack')  
  _CUROBJ = OBJNUM(laData[1])
ENDIF

IF llTemplOpn
  =gfCloseFile('SkuTmpl')
  llTemplOpn = .F.
ENDIF

RETURN lnChoice

*!*************************************************************
*! Name      : lfvRem
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : deleteing record from tempfile
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : lnBttn 
*!*************************************************************
*! Returns            : llRetVal
*!*************************************************************
*! Example   : = lfvRem()
*!*************************************************************

FUNCTION lfvRem

lnAlias = SELECT(0)

*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>
IF gfModalGen("QRM42029B42002","Dialog") = 1
  SELECT(lcLinFil)
  DELETE
  GOTO TOP
  llNoThing = lfwBrow()
  SELECT (lnAlias)
ENDIF

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To make local save.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpSavscr
*!*************************************************************

PROCEDURE lpSavscr
PRIVATE lnCurAlias,lcCurNdx,llReturn

lnCurAlias = SELECT(0)
lcDelStat = SET("DELETED")

SELECT(lcLinFil)
*B606590,1 ASH 01/01/2003 (Begin) Fix the bug of saving pack without lines.
llContinue = .F.
SCAN
  IF TotQty <> 0
    llContinue = .T.
  ENDIF
ENDSCAN
*B606590,1 ASH 01/01/2003 (End)
GOTO TOP
*B606590,1 ASH 01/01/2003 (Begin) Fix the bug of saving pack without lines.
*IF EOF() OR BOF() OR DELETED() 
IF EOF() OR BOF() OR DELETED() OR !llContinue
*B606590,1 ASH 01/01/2003 (End)
  llReturn = .F.
  *-- No Transactions made, cannot proceed
  *-- Ok
  = gfModalGen("INM42095B42001","Dialog")
  _CUROBJ = OBJNUM(m.Style)
ELSE

  *C123656,1 NNA 08/15/2004 (BEGIN) Ckeck if the entered Qty. = The predicted Total or not for NIKNAK
  IF laScrMode[4]  .AND. ASCAN(laEvntTrig,PADR('PACKSAV',10))<>0 .AND. gfGetMemVar('M_ChkTotal',gcAct_Comp)  .AND. ;
    !gfDoTriger(lcProgName,PADR('PACKSAV',10))
    STORE .F. TO llCSave
    RETURN .F.
  ENDIF
  *C123656,1 NNA (End)

  IF SEEK('P'+laData[1]+laData[2],'Spck_Hdr')
    SELECT Spck_Hdr
    REPLACE Desc      WITH laData[3],;
            cDivision WITH laData[4],;
            Season    WITH laData[5]
  ELSE
    *--B803692,4 RAMY [START]
    *INSERT INTO Spck_Hdr (Type,Account,Pack_ID,cDivision,Season,dAdd_Date,;
                          cAdd_Time,cAdd_User) ;
                VALUES   ('P' ,laData[1],laData[2],laData[4],laData[5],;
                           gdSysDate,gfGettime(),gcUser_Id)
    INSERT INTO Spck_Hdr (Type,Account,Pack_ID,cDivision,Season,dAdd_Date,;
                          cAdd_Time,cAdd_User,Desc) ;
                VALUES   ('P' ,laData[1],laData[2],laData[4],laData[5],;
                             gdSysDate,gfGettime(),gcUser_Id,laData[3])
    *--B803692,4 RAMY [END]
  ENDIF

  SET DELETED OFF
  SELECT(lcLinFil)
  SCAN
    llReturn = .T.
    IF !DELETED() AND EMPTY(TotQty)
      *-- Style XXXX has zero Total Quantity, Transaction(s)
      *-- with zero Total Quantity will be ignored.
      *-- <PROCEED> , <MODIFY>
      lnAmtCh = gfModalGen("QRM42094B42008",m.Style)
      IF lnAmtCh = 1
        LOOP
      ELSE
        llReturn = .F.
        = lfwBrow()
        _CUROBJ = OBJNUM(m.Qty1)
        EXIT
      ENDIF
    ENDIF
  ENDSCAN
  
  IF llReturn    
    SCAN
      IF !DELETED() AND !EMPTY(TotQty)
        SCATTER MEMVAR
        IF !SEEK('P'+Account+Pack_ID+Style,'Spck_Lin')
          SCATTER MEMVAR
          SELECT Spck_Lin
          APPEND BLANK
          GATHER MEMVAR
          REPLACE dAdd_Date  WITH gdSysDate  ,;
                  cAdd_Time  WITH gfGettime(),;
                  cAdd_User  WITH gcUser_Id
        ELSE
          SELECT Spck_Lin
          GATHER MEMVAR
        ENDIF
      ELSE
        IF DELETED() AND SEEK(&lcLinFil..Type+&lcLinFil..Account+&lcLinFil..Pack_ID+&lcLinFil..Style,"Spck_Lin")
          SELECT Spck_Lin
          IF DELETED('Spck_Lin')
            LOCATE REST FOR Type+Accuont+Pack_ID+Style = ;
                   &lcLinFil..Type+&lcLinFil..Account+&lcLinFil..Pack_ID+&lcLinFil..Style AND !DELETED()
          ENDIF
          BLANK
          DELETE
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

SELECT(lnCurAlias)

SET DELETED &lcDelStat

llCSave = llReturn

*!*************************************************************
*! Name      : lfTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO Assign functions to some keys to not affect the browse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfBrTab,lfBrBack
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrap()
*!*************************************************************

FUNCTION lfTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcBrTtl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrTab
  ON KEY LABEL BACKTAB DO lfBrBack
ENDIF

*!*************************************************************
*! Name      : lfClrTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Clearing the previous trapping
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfClrTrap()
*!*************************************************************

FUNCTION lfClrTrap

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

IF glFromBrow
  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lfBrTab
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the Tab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrTab()
*!*************************************************************

FUNCTION lfBrTab

ON KEY LABEL TAB
ACTIVATE WINDOW (lcIcpack3) 
IF EMPTY(m.Style)
  _CUROBJ = OBJNUM(m.Style)
ELSE  
  _CUROBJ = OBJNUM(m.Qty1)
ENDIF

*!*************************************************************
*! Name      : lfBrBack
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrBack()
*!*************************************************************

FUNCTION lfBrBack

ON KEY LABEL BACKTAB

ACTIVATE WINDOW (lcIcPack1)
_CUROBJ = OBJNUM(laData[2])

*!*************************************************************
*! Name      : lfvBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() function
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrow()
*!*************************************************************

FUNCTION lfVBrow
IF WONTOP() # (lcBrTtl)
  =gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfPackScrAct
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the keys in screen ICPACKSCR
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfPackScrAct()
*!*************************************************************

FUNCTION lfPackScrAct

*-- This function called in activate snippet for screen packScr
*-- which use to copy data from another pack id

ON KEY LABEL ESCAPE DO lfPackScrEsc

*!*************************************************************
*! Name      : lfPackScrEsc
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the Esacpe Key in screen IcpackScr
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfPackScrEsc()
*!*************************************************************

FUNCTION lfPackScrEsc

ON KEY LABEL ESCAPE

_CUROBJ = OBJNUM(pbCancel)
KEYBOARD "{ENTER}" CLEAR PLAIN

*!*************************************************************
*! Name      : lfvDiv
*! Developer : Ahmed Amer(AHM)
*! Date      : 05/05/1997
*! Purpose   : Valid function of the division popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDiv()
*!*************************************************************

FUNCTION lfvDiv

*-- Validate the header division.
laData[4] = laDiv[lnDiv,2]

*!*************************************************************
*! Name      : lfvSea
*! Developer : Ahmed Amer(AHM)
*! Date      : 05/05/1997
*! Purpose   : Valid function of the season popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSea()
*!*************************************************************

FUNCTION lfvSea

*-- Validate the header season.
laData[5] = laSea[lnSea,2]

*!*************************************************************
*: B605562,1 ASH 02/17/2002 Use the local delete to fix the bug of not delete the pack from spck_lin.
*!*************************************************************
*! Name      : lpDelscr
*! Developer : Ashraf Sherif (AHM)
*! Date      : 02/17/2002
*! Purpose   : To make local delete.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpDelscr
*!*************************************************************

PROCEDURE lpDelscr

*B606909,1 ABD - Blank the Fields before delete that if u delete without 
*B606909,1 ABD - blank , it will cause a problem if the user add the same
*B606909,1 ABD - Pack id again, the program will not save the Pack id at line level. [Begin]
*SELECT SPCK_LIN
*IF SEEK('P'+laData[1]+laData[2])
*  DELETE REST WHILE Type+Account+Pack_id+Style = 'P'+laData[1]+laData[2]
*ENDIF
PRIVATE lnPrvAlias
lnPrvAlias = SELECT(0)
IF SEEK('P'+laData[1]+laData[2],'SPCK_LIN')
  SELECT SPCK_LIN
  SCAN WHILE SEEK('P'+laData[1]+laData[2])
    BLANK
    DELETE
  ENDSCAN
ENDIF
SELECT(lnPrvAlias)
*B606909,1 ABD - [End]

SELECT SPCK_HDR
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
GATHER FROM laData FIELDS &lcScFields MEMO 
DELETE

laScrMode    = .F.
laScrMode[1] = .T.

