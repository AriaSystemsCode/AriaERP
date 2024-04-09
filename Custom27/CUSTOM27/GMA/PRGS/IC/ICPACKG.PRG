*:*******************************************************************************************************************************************
*: Program file  : ICPACKG       C#102561
*: Program desc. : Create Pack_id for group of styles on 
*:                 specific account
*: For screen    : ICPACKGM
*:        System : Aria Advantage Series.
*:        Module : INVENTORY CONTROL (IC).
*:     Developer : Mohamed Shokry (AHM)
*:*******************************************************************************************************************************************
*: Calls : 
*:         Procedures : lpShow,lpSavscr
*:         Functions  : gfSetup,gfOpenFile,lfvData_1,lfvData_2,
*:                    : gfCodes,gfSeekRec,lfWHBrow,lfWBrow,
*:                      lfTrap,lfClrTrap  
*:*******************************************************************************************************************************************
*: Passed Parameters  : None
*:*******************************************************************************************************************************************
*: Example : DO icPackg
*:*******************************************************************************************************************************************
*: Modifications :
*:B606395,1 HBG 08/21/2002 Fix Bugs in Pack screen:
*:                           - when we delete an existing pack id, the program doesn't blank
*:                             out the records in spck_lin file
*:                           - if the "Sell.Price" is zero and we click "Re-Calc" the
*:                             program check "Price Per Piece" automatically
*:                           - calculate the sell price based on style prices in case 
*:                             of sell price = 0
*:B606607,1 HBG 11/03/2002 Fix bugs in Pack screen :
*:                           - Create account pack from generic pack using same pack_id, color,
*:                             size if account pack exists system will replace it. 
*:                           - If zero is assigned to sell price => don’t assign zero for price
*:                             of component styles.
*:C200479,1 HBG 30/12/2002 Allow creating versions of range Packs 
*:B607252,1 HBG 11/02/2004 Fix bug of
*:B607252,1                  - Editing anothor version of the pack
*:B607252,1                  - Creating new version of the pack if the version is 10 or more
*:B607301,1 SSE 05/27/2003 Fix bug of not allowing the user to remove a style in a pack assigned to Invoice.
*:C200590,1 SSE 08/07/2003 Enhancement to browse only Obsolete Colors.
*:B121235,1 TMI 01/14/2004 Fix the bug that when saving without click "ReCalc" button it calculates style prices wrong
*:B123475,1 MHM 08/05/2004 Fix the bug that Disable New and Remove butt. in case of pack has open orders
*:B127280,1 MHM 04/07/2005 FIx Bug of Custom Pack Maintenance Doesn't update version correctly.
*:*******************************************************************************************************************************************

PARAMETER lcpAcct , lcpPack , lcpColor , lcpSize

*--DECLARATION OF ARRAYS PART
*-- laData      Array that hold the values of the fields that we use from base file
*-- laScrMode   Array that hold the screen modes
*-- laKeyField  Array that hold the name of the fields that represent screen keys
*-- laCodInfo   Array that hold information about the fields that use code file
*-- laDiv       Array that hold the divisions codes
*-- laSea       Array that hold the seasons codes
*-- laVer       Array that hold the version No.
*-- laSize      Array that hold Sizes
DIMENSION laData[1],laScrMode[4],laKeyField[2,4],laCodInfo[2,10],;
          laDiv[1],laSea[1],lnPTotQt[1],laVer[1],laSize[1]

*--DECLARATION OF BUTTONS PART
*--pnNewVer     Variable that hold the name of new version
*--pncalclt     Variable that hold the name of recalculate price
*--pbNew        Variable that hold the name of new Button
*--pbRem        Variable that hold the name of Remove Button

*--DECLARATION OF VARIABLES PART
*-- lcIcPack1   Variable that hold the name of screen 'icpackG1'
*-- lcIcPack2   Variable that hold the name of screen 'icpackG2'
*-- lcIcPack3   Variable that hold the name of screen 'icpackG3' 
*-- lcIcPack4   Variable that hold the name of screen 'icpackG4'
*-- lcScFields  Variable that hold the name of the used fields from basefile
*-- lcStyle     Variable that hold the style code
*-- lcStyDesc   Variable that hold the desc of the style
*-- lcLinFil    Variable that hold the name of the temp file that is used for 'spck_lin' file
*-- lcStyPic    Variable that hold the style picture(format)
*-- lcStyTtl    Variable that hold the style title
*-- lcOldVal    Variable that hold the Old value
*-- lcTempColr  Variable that hold the Color temp file
*-- lcTempSize  Variable that hold the Size temp file
*-- lcScale     Variable that hold the Scale
*-- lnPTotQt    Variable hold the sum of totqty 
*-- lcUPCNo     Variable hold the UPC #
*-- lnStyleWid  Variable that hold the width of the style
*-- lnBrRecNo   Variable that hold the no of the current record of the browse screen
*-- llBrowse    Variable that used to show if any browse button is clicked
*-- llNoShow    Variable that prevent or enable the program go into    ;
                lpshow once the program is executed;
                (.F. go in lpshow,.T. don't)
*-- llWFirst    Variable that showes if we enter the screen for the first time
*--             or we back to it from another session
*-- llTemplOpn  Variable that showes if SkuTmpl file is opened by this program 
*--             or not
*--llPackCopy    Variable to heck if come from copy or ordinary
llPackCopy = .F.

*--llReCal    variable to chack recaculate
llReCal = .F.
*B606607,1 HBG 11/03/2002 Flag to chech if we edit in old pack or create new one [Begin]
*--llEdit     Variavle to chech if we edit in old pack or create new one 
llEdit  = .F.
*B606607,1 [End]
lcOldVal = ''   

STORE SPACE(0) TO laData,laKeyField,laCodInfo,laDiv,laSea,;
                  lcIcPack1, lcIcPack2, lcIcPack3, lcIcPack4,lcScFields,;
                  lcStyle,lcStyDesc,lcLinFil,lcStyPic,lcStyTtl,laVer,lcTempColr,;
                  lcScale,lcTempSize,laSize,lcUPCNo

STORE 0 TO lnPTotQt,lnStyleWid,lnBrRecNo , lnCalcPr ,lnSellPr

STORE .F. TO llBrowse,llNoShow,llWFirst

*--To make NEW button Editable
LLALOWNEW = .T.

**--Use the local delete function instead of the global delete one.
laDefproc[7] = .F.                 && This is to prevent the global delete
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

*--Open STYLEUPC
=gfOpenFile(gcDataDir+'STYLEUPC','GMAPKUPC','SH')
=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINES','SH')
=gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINES','SH')
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')

*-- This scattering is used to give empty value for sizes label and values 
*-- at starting the program
SELECT Spck_Lin
SCATTER FIELDS Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty MEMVAR BLANK
SELECT Scale
SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR BLANK

IF !WEXIST(gcBaseWind)
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

  lcStyPic   = gfItemMask("PI")
  lcStyTtl   = gfItemMask("HI")
  lnStyleWid = LEN(lcStyPic)  
  
  lcIcPack1  = gfTempName()        && Hold the temp file of Screen1.
  lcIcPack2  = gfTempName()        && Hold the temp file of Screen2.
  lcIcPack3  = gfTempName()        && Hold the temp file of Screen3.
  lcIcPack4  = gfTempName()        && Hold the temp file of Screen4.
  
  lcLinFil   = gfTempName()         && Hold the temp file of warehouse.
  lcTempColr = gfTempName()         && Hold the temp file of Colors.
  lcTempSize = gfTempName()         &&Hold the temp file of size.
              
  lcScFields = "Account, Pack_ID   , Desc     ,cDivision  ,Season  ,"+;
               "lRange , NPKSLPRICE ,cPkVersion, cPkColor  ,cPckSize,"+;
               "nPckClPr, nPckQty, lPckRecal,LPCKPRPIEC,LQTYPCS"

  
  *---------   ----1---,----2------,-----3----,-----4-----,---5----"

  *--LADATA[1] ------------> Account
  *--LADATA[2] ------------> Pack_ID
  *--LADATA[3] ------------> Desc     
  *--LADATA[4] ------------> cDivision  
  *--LADATA[5] ------------> Season  
  *--LADATA[6] ------------> lRange           && Range
  *--LADATA[7] ------------> NPKSLPRICE       && pack price  
  *--LADATA[8] ------------> cPkVersion       && pack version
  *--LADATA[9] ------------> cPkColor         && pack color
  *--LADATA[10] ------------> cPckSize        && pack size
  *--LADATA[11] ------------> nPckClPr        && calculate pack price
  *--LADATA[12] ------------> nPckQty         && pack Qty
  *--LADATA[13] ------------> lPckRecal       && cheack Pack recalculated
  *--LADATA[14] ------------> LPCKPRPIEC       && cheack price per piece
  
  SELECT Spck_Hdr
  SCATTER FIELDS &lcScFields TO laData BLANK

  *-by defalte price per piece is checked
  *C200479,1 HBG 30/12/2002 Change Pric/PC check box to be popup. [Begin]
  *laData[14] = .F.
  lnPrPeic = 1
  *C200479,1 [End]
  laData[15] = .T.
  *--create temp lines file
  SELECT Spck_Lin
  = AFIELDS(laFileStru)  
  lnFileStru = ALEN(laFileStru,1)
  CREATE TABLE (gcWorkDir + lcLinFil) FROM ARRAY laFileStru

  INDEX ON Type+Account+Pack_ID+Style TAG StyPack ADDITIVE
  INDEX ON Type+Account+Pack_ID+cPkColor+cPckSize+CPKVERSION+Style TAG StyPack ADDITIVE

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
laCodInfo[1,08] = "Spck_HdrVr"                && Use this index for the Alternative file.
laCodInfo[1,09] = "'P'+laData[1]+laData[2]+laData[9]+laData[10]" && Seek this expretion.
laCodInfo[1,10] = "cDivision"               && Alternative Field Name

laCodInfo[2,01] = "SEASON"                  && Field Name
laCodInfo[2,02] = "laSea"                   && Array Name
laCodInfo[2,03] = "lnSea"                   && Popup Name
laCodInfo[2,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
laCodInfo[2,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[2,06] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[2,07] = "Spck_Hdr"                && Alternative File (For default val.)
laCodInfo[2,08] = "Spck_HdrVr"                && Use this index for the Alternative file.
laCodInfo[2,09] = "'P'+laData[1]+laData[2]+laData[9]+laData[10]"    && Seek this expretion.
laCodInfo[2,10] = "Season"                  && Alternative Field Name

*--Relation part
SELECT Spck_Lin
SET FILTER TO Type = 'P'
SET ORDER TO Spck_Lin
SELECT Spck_Hdr
SET FILTER TO Type = 'P'
*--Set relation
SET RELATION TO Type+Account+pack_id INTO Spck_lin

SET ORDER TO SPCK_HDRVR IN SPCK_HDR
SET ORDER TO SPCK_LINVR IN SPCK_LIN

SELECT Spck_Hdr  
*--Break standard relation 
SET RELATION OFF INTO Spck_lin                                       
*--Establish a new relation
SET RELATION TO Type+Account+pack_id+cPkColor+cPckSize+CPKVERSION INTO Spck_lin,; 
                Account+Pack_Id+'   '+cPkColor+cPckSize INTO STYLEUPC  ADDITIVE

PUSH KEY                                       && To save the the current on key label
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)   && To activate the browse screen when pressing ALT+B
DO (gcScrDir + gcWinAppl + '\ICPACKGM.SPR')    && calling the screen Mainvlo
RELEASE WINDOW (lcBrTtl)
POP KEY                                        && To Restore the previous assignments for on key label

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

  IF USED(lcTempColr)
    USE IN (lcTempColr)
  ENDIF
  ERASE (gcWorkDir+lcTempColr +".DBF")

  IF USED(lcTempSize)
    USE IN (lcTempSize)
  ENDIF
  ERASE (gcWorkDir+lcTempSize +".DBF")
  
ENDIF  

*!*************************************************************
*! Name      : lfPackBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
           "Type+Account+Pack_ID+cPkColor+cPckSize+CPKVERSION+Style = 'P'+laData[1]+laData[2]+laData[9]+laData[10]+laData[8]",;
           "Type+Account+Pack_ID+cPkColor+cPckSize+CPKVERSION+Style = 'P' ")

*BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecNo,'>',' '):1:R:H=' ':W=.F.,;
              Style:R:H=lcStyTtl, Qty1 , Qty2 , Qty3 , Qty4 , ;
              Qty5 , Qty6:R , Qty7:R , Qty8:R , TotQty:R , ;
              cPkColor  :R:H="Color" , LCPCKSIZE=lfGetSize(CPCKSIZE) :R:H= "Size" , ;
              npck_price :R:H="Unit Price"    ,;
              lnAmount=npck_price*TotQty :R:H="Amount"  ;
         FOR &lcForExp ;
         SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
         TITLE(lcBrTtl) WHEN lfwBrow() VALID :F lfVBrow()    ;         
         WINDOW (lcIcPack2) IN WINDOW (gcBaseWind)         

BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecNo,'>',' '):1:R:H=' ':W=.F.,;
              Style:R:H=lcStyTtl, Qty1 , Qty2 , Qty3 , Qty4 , ;
              Qty5 , Qty6:R , Qty7:R , Qty8:R , TotQty:R , ;
              npck_price :R:H="Unit Price"    ,;
              lnAmount=npck_price*TotQty :R:H="Amount"  ;
         FOR &lcForExp ;
         SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
         TITLE(lcBrTtl) WHEN lfwBrow() VALID :F lfVBrow()    ;         
         WINDOW (lcIcPack2) IN WINDOW (gcBaseWind)         

= lfwBrow()

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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

*--style price 
lnStyPrice = nPck_Price

IF EOF() OR BOF() OR DELETED()
  SHOW GET pbRem DISABLE
  SHOW GET pnCalclt DISABLE
  lcStyStat = "DISABLE"
  lcPopstat = IIF(laScrMode[4],"ENABLE","DISABLE")
  
ELSE

  lcPopstat = "DISABLE"
  SCATTER FIELDS Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty MEMVAR
  lcRemStat = IIF((laScrMode[3] OR laScrMode[4]),"ENABLE","DISABLE")

  SHOW GET pbRem &lcRemStat
  SHOW GET pnCalclt &lcRemStat

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
SHOW GET m.Style    &lcStyStat
SHOW GET pbStyBrow  &lcStyStat
SHOW GET lnDiv      &lcPopStat
SHOW GET lnSea      &lcPopStat
SHOW GET lnStyPrice 
SHOW GET m.Qty1
SHOW GET m.Qty2
SHOW GET m.Qty3
SHOW GET m.Qty4
SHOW GET m.Qty5
SHOW GET m.Qty6
SHOW GET m.Qty7
SHOW GET m.Qty8

*B123475,1 MHM 08/05/2004 [Start] Disable 2 buttons in case of there is open orders
IF laScrMode[3] AND !lfChkPack()
  SHOW GET pbNew  DISABLE
  SHOW GET pbRem  DISABLE
ENDIF
*B123475,1 MHM 08/05/2004 [End]

SHOW WINDOW (lcBrTtl) REFRESH SAME

=lfRefresh()

*!*************************************************************
*! Name      : lfvNew
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
IF !SEEK('P'+laData[1]+laData[2]++laData[9]+laData[10]+ALLTRIM(STR(lnVer,4))+SPACE(19))
  APPEND BLANK
  REPLACE Type     WITH 'P'      ,;
          Account  WITH laData[1],;
          Pack_ID  WITH laData[2],;
          cPkColor WITH laData[9],;
          cPckSize WITH ladata[10]
  REPLACE CPKVERSION WITH ALLTRIM(STR(lnVer,4))

ENDIF
= lfwBrow()
_CUROBJ = OBJNUM(m.Style)
lnStyPrice = 0
SHOW GET lnStyPrice
SHOW GET pnCalclt ENABLE

*--hide key
STORE '' TO laSize
SHOW GET laData[1] DISABLE
SHOW GET laData[2] DISABLE
SHOW GET laData[3] DISABLE
SHOW GET laData[9] DISABLE
SHOW GET lcScale   DISABLE
SHOW GET lnSize    DISABLE
SHOW GET pbPckBrow DISABLE
SHOW GET pbSizBrow DISABLE
SHOW GET pbClrBrow DISABLE

PRIVATE lnCrAlias
lnCrAlias = SELECT()

IF laData[6] 
  IF !laData[14]
    SHOW GET lnPTotQt ENABLE
    FOR lnCnt = 1 TO SCALE.CNT
      lcCount = STR(lnCnt,1)
      SHOW GET m.Qty&lcCount DISABLE   
    ENDFOR
  ELSE
    lnPTotQt = 1
    laData[12] = lnPTotQt
    SHOW GET lnPTotQt DISABLE
  ENDIF
  m.TotQty = 0
  FOR lnCnt = 1 TO SCALE.CNT
    lcCount = STR(lnCnt,1)
    m.Qty&lcCount = 1
    m.TotQty = m.TotQty + 1
    SELECT(lcLinFil) 
    REPLACE Qty&lcCount WITH 1 ,;
            TotQty      WITH m.TotQty
  ENDFOR
  lnCalcPr    = lnCalcPr - TotQty * npck_price + m.TotQty * lnStyPrice 
  ladata[11]  = lnCalcPr
  
ELSE
  FOR lnCnt = 1 TO SCALE.CNT
    lcCount = STR(lnCnt,1)
    SHOW GET m.Qty&lcCount ENABLE 
  ENDFOR
  SHOW GET lnPTotQt DISABLE
ENDIF  
SELECT (lnCrAlias)


SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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

IF llBrowse .OR. !EMPTY(m.Style)
  lnAlias = SELECT(0)
  SELECT (lcLinFil)
  IF llBrowse OR !SEEK(m.Style,"Style")
    llBrowse = .F.
    SELECT STYLE
    SET FILTER TO STATUS <>"X"
    m.Style  = gfStyBrw('I',"","",.F.)
    SELECT (lcLinFil)
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
      IF !EMPTY(m.Style) AND SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10]+IIF(lnVer>10,PADR(STR(lnVer,2),4,' '),PADR(STR(lnVer,1),4,' '))+m.Style,lcLinFil)

        *-- "Style already entered! cannot add a second entry.."
        *-- <Ok>
        llNoThing = gfModalGen("INM42093B42001","Dialog",m.Style)  
        m.Style   = SPACE(19)
        _CUROBJ   = _CUROBJ
      ELSE

        =SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10]+ALLTRIM(STR(lnVer,4))+SPACE(19), lcLinFil)
        REPLACE Style WITH m.Style 
        = lfwBrow()
      ENDIF
    ENDIF
    SELECT (lnAlias)
  ENDIF
ENDIF

IF !EMPTY(m.Style)
  lnStyPrice = STYLE.PriceA
  SHOW GET lnStyPrice ENABLE
ENDIF  

*!*************************************************************
*! Name      : lfvQuantities
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
  *--to update total QTY
  m.TotQty    = m.TotQty - lnOldVal + lnNewVal
  lnPTotQt    = lnPTotQt - &lcLinFil..TotQty + m.TotQty
  laData[12]  = lnPTotQt
  SHOW GET lnPTotQt
  *--to update total Calculated price
  lnCalcPr    = lnCalcPr - TotQty * npck_price + m.TotQty * lnStyPrice 
  ladata[11]  = lnCalcPr
  REPLACE &lcFldName WITH &lcVarName ,;
          TotQty     WITH m.TotQty
  SHOW WINDOW (lcBrTtl) REFRESH SAME
  =lfRefresh()
  SELECT(lnAlias)
ENDIF

*!*************************************************************
*! Name      : lpShow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
*-- Add conditoin in case if call with paramete

IF !laScrMode[2] .AND. TYPE("lcpAcct") = "C" .AND. TYPE("lcpPack") = "C"
  laData[1] = lcpAcct
  laData[2] = lcpPack
  laData[9] = lcpColor
  laData[10] = lcpSize
  lnVer = 1
  KEYBOARD "{ENTER}"
  lcDummy   = lfvData_2()
ENDIF

PRIVATE lnCurAlias,lcFilToUse
lcFilToUse = IIF(laScrMode[2], "Spck_Lin", lcLinFil)
lnCurAlias = SELECT(0)
SELECT Spck_Hdr
SET ORDER TO Spck_HdrVr

IF 'SPCK_LIN'$UPPER(lcFilToUse)
  SET RELATION TO Type+Account+pack_id+cPkColor+cPckSize  INTO &lcFilToUse
ENDIF
  
SELECT Spck_Hdr
SET ORDER TO Spck_HdrVr
IF 'SPCK_LIN'$UPPER(lcFilToUse)
  SET RELATION TO Type + Account + pack_id + cPkColor+cPckSize + ALLTRIM(CPKVERSION) INTO SPCK_LIN,;
                  Account+Pack_Id+'   '+cPkColor+cPckSize INTO STYLEUPC
ELSE
  SET RELATION TO Account+Pack_Id+'   '+cPkColor+cPckSize INTO STYLEUPC
ENDIF                    
                  
*--Disable new version command
SHOW GET pnNewVer DISABLE

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
    STORE SPACE(0) TO lcStyle ,lcUPCNo, lcScale , laSize
    STORE 0 TO lnSellPr,lnCalcPr
    DIME laSize[1]
    SELECT (lcLinFil)
    ZAP
    llPackCopy = .F.    
    lnVer = 1
    PRIVATE lnCrAlias
    lnCrAlias = SELECT()

    PRIVATE lnCrAlias
    lnCrAlias = SELECT()
    
    IF laData[6] 
      IF !laData[14]
        SHOW GET lnPTotQt ENABLE
        FOR lnCnt = 1 TO SCALE.CNT
          lcCount = STR(lnCnt,1)
          SHOW GET m.Qty&lcCount DISABLE   
        ENDFOR
      ELSE
        lnPTotQt = 1
        laData[12] = lnPTotQt
        SHOW GET lnPTotQt DISABLE
      ENDIF
      m.TotQty = 0
      FOR lnCnt = 1 TO SCALE.CNT
        lcCount = STR(lnCnt,1)
        m.Qty&lcCount = 1
        m.TotQty = m.TotQty + 1
        SELECT(lcLinFil) 
        REPLACE Qty&lcCount WITH 1 ,;
                TotQty      WITH m.TotQty
      ENDFOR
      lnCalcPr    = lnCalcPr - TotQty * &lcLinFil..npck_price + m.TotQty * lnStyPrice 
      ladata[11]  = lnCalcPr
    ELSE
      FOR lnCnt = 1 TO SCALE.CNT
        lcCount = STR(lnCnt,1)
        SHOW GET m.Qty&lcCount ENABLE 
      ENDFOR
      SHOW GET lnPTotQt DISABLE
   ENDIF  
   SELECT (lnCrAlias)


  *-- View mode
  CASE laScrMode[2]
    SELECT Spck_Hdr
    SCATTER FIELDS &lcScFields TO laData
    *C200479,1 HBG 30/12/2002 Change Pric/PC check box to be popup. [Begin]
    lnPrPeic = IIF(laData[14],2,3)
    *C200479,1 [End]

    lnSellPr = laData[7] 
    lnCalcPr = laData[11]
    *-- Fill codes popups with its real values.
    = gfwCodePop(@laCodInfo, "CDIVISION", "T")
    SHOW GET lnDiv
    = gfwCodePop(@laCodInfo , "SEASON" , "T")
    SHOW GET lnSea
    
    lnPTotQt = 0
    SELECT Spck_Lin
    SEEK 'P'+laData[1]+laData[2]+laData[9]+laData[10]+laData[8]
    SCAN REST WHILE Type+Account+Pack_ID+cPkColor+cPckSize+CPKVERSION+Style = ;
                    'P'+laData[1]+laData[2]+laData[9]+laData[10]+laData[8]
      lnPTotQt = lnPTotQt + TotQty
    ENDSCAN

    IF laData[6] 
      lnPTotQt = Spck_Hdr.npckqty
    ENDIF  
    *--
    SELECT Spck_Hdr   

    *--to get upc#
    SELECT ALLTRIM(CPKVERSION) ;
    FROM Spck_HDR ;
    WHERE (Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion='P'+laData[1]+laData[2]+laData[9]+laData[10]);
    INTO ARRAY laVer; 
    ORDER BY CPKVERSION 
    
    *B127280,1 MHM 04/07/2005 resort array to sort by Integer not string[Start]
    *lnVer = ASCAN(laVer,ALLTRIM(laData[8]))
    FOR lnArrCont = 1 to ALEN(laVer)
      laVer[lnArrCont] = VAL(laVer[lnArrCont])
    ENDFOR 
    =ASORT(laVer)
    FOR lnArrCont = 1 to ALEN(laVer)
      laVer[lnArrCont] = ALLTRIM(STR(laVer[lnArrCont],4))
    ENDFOR 
    lnVer = ASCAN(laVer,ALLTRIM(laData[8]))
    *B127280,1 MHM 04/07/2005 [End]

    SHOW GET lnVer ENABLE
    
    *-- we add space to fitch Index
    IF SEEK(laData[1]+laData[2]+"   "+ladata[9]+ladata[10],'STYLEUPC') OR ;
       SEEK("*****"+laData[2]+"   "+ladata[9]+ladata[10],'STYLEUPC')    
      lcUPCNo =STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3 
    ELSE
       STORE SPACE(0) TO lcUPCNo
    ENDIF  
    *--SCALE

      lcScale = LEFT(laData[10],1)
    IF !EMPTY(lcScale)
      SELECT Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8,Cnt ;
      FROM SCALE ;
      WHERE (Type+Scale+PrePak='S'+lcScale);
      INTO ARRAY laSize
      *--TO REFRISH PopUp

      lnSizLen=laSize[9]
      DIME laSize[lnSizLen]     
      lnSize = VAL(SUBSTR(laData[10],2))
      SELECT Spck_Hdr   
    ENDIF  
    llPackCopy = .F.    
  *-- Edit mode
  CASE laScrMode[3]
    
    *B606607,1 HBG 11/03/2002 Flag to chech if we edit in old pack or create new one [Begin]
    llEdit = .T.
    *B606607,1 [End]
    *-- create the file only if you are enter the screen for first
    *-- time, if you coming from another session in edit mode
    *-- don't create the file from the beginning
    IF llWFirst
      *B607252,1 HBG 11/02/2004 Fix bug of editing anothor version of the pack [Begin]
      *SELECT * FROM SPck_Lin;
      *  WHERE Type + Account   + Pack_ID  + cPkColor + cPckSize +cpkversion+ Style;
      *  LIKE  'P'  + laData[1] + laData[2] + laData[9] + laData[10] +ALLTRIM(STR(lnVer,4)) +'%';
      *  INTO DBF (gcWorkDir + lcLinFil)
      SELECT * FROM SPck_Lin;
        WHERE Type + Account   + Pack_ID  + cPkColor + cPckSize +cpkversion+ Style;
        LIKE  'P'  + laData[1] + laData[2] + laData[9] + laData[10] + laData[8] +'%';
        INTO DBF (gcWorkDir + lcLinFil)
      *B607252,1 [END]
       INDEX ON Type+Account+Pack_ID+Style TAG StyPack ADDITIVE
       INDEX ON Type+Account+Pack_ID+cPkColor+cPckSize+CPKVERSION+Style TAG StyPack ADDITIVE
       
      _CUROBJ = OBJNUM(laData[6])      
    ENDIF
    
    IF laData[1]<>'*****'
      SHOW GET pnNewVer ENABLE
    ENDIF

    SHOW GET laData[2] DISABLE
    SHOW GET laData[3] DISABLE
    SHOW GET laData[9] DISABLE
    SHOW GET lcScale   DISABLE
    SHOW GET lnSize    DISABLE
    SHOW GET lnVer     DISABLE
    SHOW GET pbSizBrow DISABLE
    SHOW GET pbClrBrow DISABLE
    llPackCopy = .F.    
    PRIVATE lnCrAlias
    lnCrAlias = SELECT()
    
    IF laData[6] 
      IF !laData[14]
        SHOW GET lnPTotQt ENABLE
        FOR lnCnt = 1 TO SCALE.CNT
          lcCount = STR(lnCnt,1)
          SHOW GET m.Qty&lcCount DISABLE   
        ENDFOR
      ELSE
        lnPTotQt = 1
        laData[12] = lnPTotQt
        SHOW GET lnPTotQt DISABLE
      ENDIF
      m.TotQty = 0
      FOR lnCnt = 1 TO SCALE.CNT
        lcCount = STR(lnCnt,1)
        m.Qty&lcCount = 1
        m.TotQty = m.TotQty + 1
        SELECT(lcLinFil) 
        REPLACE Qty&lcCount WITH 1 ,;
                TotQty      WITH m.TotQty
      ENDFOR
      lnCalcPr    = lnCalcPr - TotQty * &lcLinFil..npck_price + m.TotQty * lnStyPrice 
      ladata[11]  = lnCalcPr
    ELSE
      FOR lnCnt = 1 TO SCALE.CNT
        lcCount = STR(lnCnt,1)
        SHOW GET m.Qty&lcCount ENABLE 
      ENDFOR
      SHOW GET lnPTotQt DISABLE
   ENDIF  
   *B123475,1 MHM 08/05/2004 [End]   
   IF !lfChkPack()
      FOR lnCnt = 1 TO SCALE.CNT
        lcCount = STR(lnCnt,1)
        SHOW GET m.Qty&lcCount DISABLE   
      ENDFOR
   ENDIF
   *B123475,1 MHM 08/05/2004 [End]   

   =lfvContShw()
   
   SELECT (lnCrAlias)
   
    
  *-- Add mode
  CASE laScrMode[4]
    *B606607,1 HBG 11/03/2002 Flag to chech if we edit in old pack or create new one [Begin]
    llEdit = .F.
    *B606607,1 [End]
    lcPopMode = IIF(EMPTY(laData[4]),'D',"V,"+laData[4])
    = gfwCodePop(@laCodInfo, "CDIVISION" , lcPopMode)
    laData[4] = laDiv[lnDiv,2]
    lcPopMode = IIF(EMPTY(laData[5]),'D',"V,"+laData[5])
    = gfwCodePop(@laCodInfo , "SEASON"   , lcPopMode)
    laData[5] = laSea[lnSea,2]
    SHOW GET lnDiv
    SHOW GET lnSea

    *--by defualte price per piece is true
    IF lnVer >1
      SHOW GET laData[1] DISABLE
      SHOW GET laData[2] DISABLE
      SHOW GET pbPckBrow DISABLE
      SHOW GET pbFabBrow DISABLE

      SHOW GET pbClrBrow DISABLE
      SHOW GET laData[9] DISABLE
      SHOW GET pbSizBrow DISABLE
      SHOW GET lcScale   DISABLE
      SHOW GET lnSize    DISABLE
      SHOW GET pnNewVer  DISABLE
      
    ELSE
      SHOW GET laData[1] ENABLE
      SHOW GET laData[2] ENABLE
      SHOW GET pbPckBrow ENABLE
      SHOW GET pbFabBrow ENABLE
      SHOW GET lnSize    DISABLE
      IF !llPackCopy
        *C200479,1 HBG 30/12/2002 Change Pric/PC check box to be popup. [Begin]
        *laData[14] = .F.
        lnPrPeic = 1
        *C200479,1 [End]
        laData[9] = "ASSORT"
        lnVer = 1
        laVer[1] = lnVer
        STORE '' TO lcScale
        STORE 0 TO lnSize
        STORE .F. TO laSize
        *C200479,1 HBG 30/12/2002 Have the qty/pc checked by default when a pack is created. [Begin]
        *C200479,1 HBG 30/12/2002 Change Pric/PC check box to be popup. [Begin]
        *STORE .T. TO laData[14],laData[15]
        *SHOW GET laData[14]
        STORE .T. TO laData[15]
        lnPrPeic = 1
        *C200479,1 [End]
        SHOW GET laData[15]
        *C200479,1 [End]
      ENDIF  
    ENDIF  
    IF !(llPackCopy)
      IF USED(lcLinFil)
        SELECT (lcLinFil)
        ZAP
      ENDIF
      STORE 0 TO lnPTotQt,lnCalcPr,lnSellPr
    ENDIF  
    SHOW GET lnVer
    _CUROBJ = OBJNUM(laData[1])    

    PRIVATE lnCrAlias
    lnCrAlias = SELECT()
    
    IF laData[6] 
      IF !laData[14]
        SHOW GET lnPTotQt ENABLE
        FOR lnCnt = 1 TO SCALE.CNT
          lcCount = STR(lnCnt,1)
          SHOW GET m.Qty&lcCount DISABLE   
        ENDFOR
      ELSE
        lnPTotQt = 1
        laData[12] = lnPTotQt
        SHOW GET lnPTotQt DISABLE
      ENDIF
      m.TotQty = 0
      FOR lnCnt = 1 TO SCALE.CNT
        lcCount = STR(lnCnt,1)
        m.Qty&lcCount = 1
        m.TotQty = m.TotQty + 1
        SELECT(lcLinFil) 
        REPLACE Qty&lcCount WITH 1 ,;
                TotQty      WITH m.TotQty
      ENDFOR
      lnCalcPr    = lnCalcPr - TotQty * &lcLinFil..npck_price + m.TotQty * lnStyPrice 
      ladata[11]  = lnCalcPr
      
    ELSE
      FOR lnCnt = 1 TO SCALE.CNT
        lcCount = STR(lnCnt,1)
        SHOW GET m.Qty&lcCount ENABLE 
      ENDFOR
      SHOW GET lnPTotQt DISABLE
    ENDIF  
    

    SHOW GET lnPTotQt    ENABLE 
    *C200479,1 HBG 30/12/2002 Change Pric/PC check box to be popup. [Begin]
    *SHOW GET laData[14]  ENABLE 
    SHOW GET lnPrPeic  ENABLE 
    *C200479,1 [End]
    SHOW GET laData[6]   ENABLE 
    SHOW GET lnStyPrice  ENABLE 
    FOR lnCnt = 1 TO SCALE.CNT
      lcCount = STR(lnCnt,1)
      SHOW GET m.Qty&lcCount ENABLE 
    ENDFOR
    
    SELECT (lnCrAlias)
ENDCASE

= lfPackBrow() AND lfRefresh()

DIME laVer[ALEN(laVer)]
SHOW GET m.Style    &lcStyStat
SHOW GET pbStyBrow  &lcStyStat
SHOW GET lcUPCNo
*--to disable user defined field 
SHOW GET PBUSRFIELD DISABLE

*B123475,1 MHM 08/05/2004 [Start] Disable 2 buttons in case of there is open orders
IF laScrMode[3] AND !lfChkPack()
  SHOW GET pbNew  DISABLE
  SHOW GET pbRem  DISABLE
ENDIF
*B123475,1 MHM 08/05/2004 [End]

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
IF  (lnLstKey = 13 AND !MDOWN() AND EMPTY(laData[2])) AND !llBrowse
  RETURN
ENDIF

IF (EMPTY(laData[2]) OR (laData[2] = PADR('?',16))) AND laScrMode[4]
  = gfModalGen("INM00000B42001","Dialog",.F.,.F.,"Please enter valid Pack ID ")
  _CUROBJ = OBJNUM(laData[2])
  laData[2] = SPACE(16)
  RETURN
ENDIF

laData[1] = IIF(EMPTY(laData[1]),'*****',laData[1])

IF laScrMode[4]
  *-- Would you like to copy information from another Pack ID
  *-- <Yes>, <No>
  IF gfModalGen("QRM42005B42002","Dialog","Pack ID") = 1 
    = lfPackCopy()
  ELSE
    IF lfPackScr() = 1    && OK
      IF !SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10],'Spck_Hdr')
        _CUROBJ = OBJNUM(laData[3])
      ENDIF
    ELSE                  && Cancel
    _CUROBJ = OBJNUM(laData[2])
    ENDIF
  ENDIF
ELSE
  IF (lnLstKey = 13 AND !MDOWN()) .OR. (TYPE("lcpPack")="C" .AND. !EMPTY(lcpPack))
    IF (TYPE("lcpPack")="C" .AND. !EMPTY(lcpPack))
      lcpPack = .F.
    ENDIF
    *-- Account is equaled by '*****' if it is left empty
    *-- to careate generic pack ID for all accounts
    laData[1] = IIF(EMPTY(laData[1]),'*****',laData[1])
    IF !SEEK('P'+laData[1]+laData[2],'Spck_Hdr') OR llBrowse
      IF !llBrowse AND !EMPTY(laData[2]) AND laData[2] # PADR('?',16)
        IF laData[1] = "*****"
          *-- THIS generic PACK_ID does not exist
          *-- < Browse > < Reenter >
          lnABR = gfModalGen("QRM42091B42014","Dialog")  
        ELSE
          *-- THIS PACK ID IS NOT FOUND DO YOU WANT TO BROWSE OR ADD
          *--< Browse > < Reenter >
        
          lnABR = gfModalGen("QRM42090B42014","Dialog",'Pack ID '+'|'+laData[1])
        ENDIF

        DO CASE
          CASE lnABR = 1            &&Browse
            llBrowse = .F.
            = lfPackIDBrow()

          CASE lnABR = 2            &&Reenter
            STORE SPACE(0) TO laData[2]
            _CUROBJ = OBJNUM(laData[2])
        ENDCASE
      ELSE
        llBrowse  = .F.
        llNoThing = lfPackIDBrow()
      ENDIF
    ELSE
      SHOW GET laData[9] ENABLE
      SHOW GET pbClrBrow ENABLE
      IF !EMPTY(lcpAcct) AND !EMPTY(lcpColor)
        =SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10],'Spck_Hdr')
        laScrMode    = .F.
        laScrmOde[2] = .T.  
        SHOW GETS    
      ENDIF  
    ENDIF
  ENDIF
ENDIF
SELECT ALLTRIM(CPKVERSION) ;
FROM Spck_HDR ;
WHERE (Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion='P'+laData[1]+laData[2]+laData[9]+laData[10]);
INTO ARRAY laVer; 
ORDER BY CPKVERSION 
*B127280,1 MHM 04/07/2005 resort array to sort by Integer not string[Start]
IF !laScrMode[4] 
  FOR lnArrCont = 1 to ALEN(laVer)
    laVer[lnArrCont] = VAL(laVer[lnArrCont])
  ENDFOR 
  =ASORT(laVer)
  FOR lnArrCont = 1 to ALEN(laVer)
    laVer[lnArrCont] = ALLTRIM(STR(laVer[lnArrCont],4))
  ENDFOR 
ENDIF  
*B127280,1 MHM 04/07/2005 [End]

IF laScrMode[4] 
  STORE '' TO laVer
  laVer[1]='1'
  SHOW GET lnVer DISABLE
  lnVer = 1
ENDIF


SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPackCopy
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
llPackCopy = .T.
llBrowse = .F.
lcPackID = SPACE(0)
lcPackID2 = SPACE(0)

lcWName    = "ICPCopy"
lcWSPTitl  = "Copy Information From Pack_ID"

PUSH KEY                                      && To save the the current on key label
ON KEY
PRIVATE lcVerNo,lcAccnt,lcCopColr,lcCopSiz
lcVerNo   = ''  && Holds Version No. selected
lcAccnt   = ''  && Holds Account Selected
lcCopColr = ''  && Holds Color selected
lcCopSiz  = ''  && Holds Sizes Selected
DO (gcScrDir + gcWinAppl + '\ICSPCOPY.SPR')   && calling the screen ICSPCOPY
POP KEY                                       && To Restore the previous assignments for on key label
IF EMPTY(lcPackID)
  lcPackID2 = SPACE(0)
ENDIF

IF !EMPTY(lcPackID2)
  SELECT * FROM SPck_Lin;
    WHERE Type + Account   + Pack_ID  + cPkColor + cPckSize +CPKVERSION + Style;
    = 'P'  + lcAccnt + lcPackID2 +lcCopColr+lcCopSiz+ lcVerNo ; 
    INTO DBF (gcWorkDir + lcLinFil)
              
  INDEX ON Type+Account+Pack_ID+cPckSize+cPkColor+CPKVERSION+Style TAG StyPack ADDITIVE
  =SEEK('P'+laData[1]+lcPackID2,"Spck_Hdr")  
  SELECT(lcLinFil)
  GOTO TOP
  REPLACE ALL ACCOUNT    WITH laData[1],;
              CPKVERSION WITH ALLTRIM(STR(lnVer,4)) 
  GOTO TOp
  
  INDEX ON Type+Account+Pack_ID+cPkColor+cPckSize+CPKVERSION+Style TAG StyPack ADDITIVE
  =SEEK('P'+lcAccnt+lcPackID2+lcCopColr+lcCopSiz+lcVerNo,"Spck_Hdr")

  laData[3]  = Spck_Hdr.Desc
  laData[4]  = Spck_Hdr.cDivision
  laData[5]  = Spck_Hdr.Season
  laData[6]  = Spck_Hdr.LRANGE
  laData[7]  = Spck_Hdr.NPKSLPRICE
  laData[9]  = Spck_Hdr.cPkColor         && pack color
  laData[10] = Spck_Hdr.cPckSize         && pack size
  laData[11] = Spck_Hdr.nPckClPr         && calculate pack price
  laData[12] = Spck_Hdr.nPckQty          && pack Qty
  laData[14] = Spck_Hdr.LPCKPRPIEC       && pack Qty
  *C200479,1 HBG 30/12/2002 Change Pric/PC check box to be popup. [Begin]
  lnPrPeic = IIF(laData[14],2,3)
  *C200479,1 [End]
  laData[15] = Spck_Hdr.LQTYPCS          && Qty Per Pack
  lnSellPr   = Spck_Hdr.nPkSlPrice
  lnCalcPr   = Spck_Hdr.nPckClPr
  lnPTotQt   = Spck_Hdr.nPckQty

  SHOW GETS

  IF !EMPTY(laData[10])
    lcScale    = LEFT(laData[10],1)
    lnSize     = VAL(RIGHT(laData[10],2))
    SELECT Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 ;
           FROM SCALE;
           WHERE (Type+Scale ='S'+lcScale) ;
           INTO ARRAY laSize
   ENDIF        

  SELECT(lcLinFil)
  GOTO TOP
  REPLACE ALL Pack_ID  WITH laData[2] ,;
              cPkColor WITH laData[9] ,;
              cPckSize WITH laData[10]
  GOTO TOP
  *--TO REFRISH PopUp
  IF !EMPTY(laData[10])
    lnCnt = 0
    FOR lnCount = 1 TO 8
      IF !EMPTY(laSize[lnCount])
        lnCnt =lnCnt +1
      ELSE
        EXIT  
      ENDIF  
    ENDFOR
    DIME laSize[lnCnt]     
    lnSize = VAL(RIGHT(laData[10],1))
  ENDIF

  
  SHOW GET laData[1] DISABLE
  SHOW GET laData[2] DISABLE
  SHOW GET laData[3] DISABLE
  SHOW GET lnDiv     ENABLE
  SHOW GET lnSea     ENABLE
  
  SHOW GET laData[6]
  SHOW GET laData[7]
  SHOW GET laData[9] 
  SHOW GET laData[11]
  SHOW GET laData[12]
  *C200479,1 HBG 30/12/2002 Change Pric/PC check box to be popup. [Begin]
  *SHOW GET laData[14]
  SHOW GET lnPrPeic 
  *C200479,1 [End]
  SHOW GET lcScale 
  SHOW GET lnSize 
  SHOW GET pbPckBrow 
  SHOW GET pbSizBrow 
  SHOW GET pbClrBrow 
  _CUROBJ = OBJNUM(laData[9])
ELSE
  _CUROBJ = OBJNUM(laData[2])
ENDIF

*!*************************************************************
*! Name      : lfvPackID
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
ELSE
  IF SEEK('P'+laData[1]+lcPackID2,'Spck_Hdr')    
    lcAccnt  = SPCK_HDR.ACCOUNT
    lcVerNo  = SPCK_HDR.CPKVERSION
    lcCopColr= SPCK_HDR.cPkColor
    lcCopSiz = SPCK_HDR.cPckSize
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfPackIDBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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

lnCurAlias = SELECT(0)

lcBrTitle  = 'Available Packs'

lcBrFields = "Account           :R :H='Account'      :16," +;
             "Pack_ID           :R :H='Pack_ID'      :16," +;
             "CPKCOLOR          :R :H='Color'        :6,"  +;
             "LCPCKSIZE=lfGetSize(CPCKSIZE) :R :H='Size'      :6,"  +;
             "Desc              :R :H='Desc'         :30," +;
             "CPKVERSION        :R :H='Version'      :7,"  +;
             "cDivision         :R :H='Devision'     :8,"  +;
             "Season            :R :H='Season'       :8,"  +;
             "NPKSLPRICE        :R :H='Selling Price':13," +;
             "STYLEUPC.CUPCNUM1 :R :H='UPC'          :7,"  +;  
             "STYLEUPC.CUPCNUM2 :R :H=''             :6,"  +;  
             "STYLEUPC.CUPCNUM3 :R :H=''             :2"

IF !SEEK('P'+IIF(llFromCopy OR laData[1]='*****','',laData[1]),'Spck_Hdr')
  *--There are no records to browse.
  *-- OK
  = gfModalGen("INM42097B42001","Dialog")  
  _CUROBJ = OBJNUM(laData[2])
ELSE
  lcKey = "'P'" 
  IF !llFromCopy
    lcKey = lcKey + IIF(laData[1]='*****','',' + laData[1]')
  ENDIF
  
  SELECT Spck_Hdr
  PRIVATE laBrow
  DIMENSION laBrow[10]
  laBrow = ''
  =gfBrows(lcKey,"Account, Pack_ID   , Desc     ,cDivision  ,Season,;
                  LRANGE , NPKSLPRICE, CPKVERSION, CPKCOLOR , CPCKSIZE" ;  
          ,"laBrow",lcBrTitle)

  IF !EMPTY(laBrow)
    IF llFromCopy
      lcPackID  = ALLTRIM(laBrow[2]) +ALLTRIM(laBrow[9]) +lfGetSize(ALLTRIM(laBrow[10]))
      lcPackID2 =laBrow[2]
      lcAccnt   = laBrow[1]
      lcVerNo   = laBrow[8]
      lcCopColr = laBrow[9]
      lcCopSiz  = laBrow[10]
      
    ELSE      
      = ACOPY(laBrow,laData)
      laScrMode    = .F.
      laScrMode[2] = .T.
      SHOW GETS
    ENDIF
    IF SEEK(laBrow[1]+laBrow[2]+"   "+laBrow[9]+laBrow[10],'STYLEUPC') OR ;
       SEEK("*****"+laBrow[2]+"   "+laBrow[9]+laBrow[10],'STYLEUPC')    
      lcUPCNo =STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3
      SHOW GET lcUPCNo
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
 
  IF lnChoice = 1
    lnSepCount = OCCURS('-',lcSPPic)
    laData[2] = SUBSTR(lcSPCode,1,lnCodeTtl-lnSepCount)
    IF !EMPTY(laData[2])
      *laScrMode    = .F.
      *laScrMode[4] = .T.                       && Enable Add Mode
      *lnVer = STR(1,4)
      *SHOW GETS
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
PRIVATE llFound
STORE .F. TO llFound 
lnAlias = SELECT(0)

*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>

IF gfModalGen("QRM42029B42002","Dialog") = 1
  
  *B607301,1 SSE Commented out. [Begin]  
  *SELECT(lcLinFil)  
  **--we check if this pack has order or invoice
  *IF SEEK(&lcLinFil..STYLE,'INVLINE')
  *  SELECT INVLINE
  *  SCAN REST WHILE style+invoice+STR(lineno,6) = &lcLinFil..STYLE
  *    IF Pack_ID+cPkColor+cPckSize+CPKVERSION = LADATA[2] +LADATA[9]+LADATA[10] + LADATA[8] 
  *      llFound = .T.
  *    ENDIF
  *  ENDSCAN
  *  IF llFound 
  *    *-message this pack belong to order,can not remove 
  *    *--OK
  *    =gfModalGen("INM00000B42001","Dialog",.F.,.F.,"this pack belong to inovice,can not remove ")
  *    RETURN
  *  ENDIF
  *ENDIF

  *IF SEEK(&lcLinFil..STYLE,'ORDLINE')
  *  SELECT ORDLINE
  *  SCAN REST WHILE style+invoice+STR(lineno,6) = &lcLinFil..STYLE
  *    IF Pack_ID+cPkColor+cPckSize+CPKVERSION = LADATA[2] +LADATA[9]+LADATA[10] +LADATA[8] 
  *      llFound = .T.
  *    ENDIF
  *  ENDSCAN
  *  IF llFound 
  *     *-message this pack belong to order,can not remove 
  *     *--OK
  *    =gfModalGen("INM00000B42001","Dialog",.F.,.F.,"this pack belong to order,can not remove ")
  *    RETURN
  *  ENDIF
  *ENDIF
  *B607301,1 SSE Commented out. [Begin]

  SELECT(lcLinFil)
  lnPTotQt = lnPTotQt - TotQty
  lnCalcPr  = lnCalcPr - TotQty * npck_price  
  DELETE
  GOTO TOP
  llNoThing = lfwBrow()
  SELECT (lnAlias)
ENDIF

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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

*C200479,1 HBG 30/12/2002 Change Pric/PC check box to be popup. [Begin]
IF lnPrPeic = 1
  = gfModalGen("INM00000B00000","Dialog",.F.,.F.,;
  'No price type selected. Cannot save. Please select either Price pre piece or Price per pack.')
  llCSave = .F.
  RETURN 
ENDIF
*C200479,1 [End]

*--if not recalculate then calculate it
IF !llReCal
  =lfvReCal(.T.)
ENDIF  
IF !llReCal
  RETURN
ENDIF

SELECT(lcLinFil)
GOTO TOP
IF EOF() OR BOF() OR DELETED()
  llReturn = .F.
  *-- No Transactions made, cannot proceed
  *-- Ok
  = gfModalGen("INM42095B42001","Dialog")
  _CUROBJ = OBJNUM(m.Style)
ELSE
  *B606607,1 HBG 11/03/2002 If create new pack, and this pack found befor prevent saving [Begin]
  IF !llEdit AND SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10]+ALLTRIM(STR(lnVer,4)),'Spck_Hdr')
    = gfModalGen("INM00000B42001","Dialog",.F.,.F.,'This Pack is already exist. Cannot save.')
    llCSave = .F.
    RETURN
  ENDIF
  *B606607,1 [End]
  IF SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10]+ALLTRIM(STR(lnVer,4)),'Spck_Hdr')
    SELECT Spck_Hdr
    REPLACE Desc      WITH laData[3],;
            cDivision WITH laData[4],;
            Season    WITH laData[5]
    *--to save version in user defined field         
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPKVERSION'),1)
    laUsrField[lnFieldPos,6] = ALLTRIM(STR(lnVer,4))

    *--to save Range in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'LRANGE'),1)
    laUsrField[lnFieldPos,6] = laData[6]
            
    *--to save Pack Price in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'NPKSLPRICE'),1)
    laUsrField[lnFieldPos,6] = laData[7]

    *--to save Pack Color in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPKCOLOR'),1)
    laUsrField[lnFieldPos,6] = laData[9]

    *--to save Size in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPCKSIZE'),1)
    laUsrField[lnFieldPos,6] = laData[10]

    *--to save pack calculated price in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'NPCKCLPR'),1)
    laUsrField[lnFieldPos,6] = laData[11]

    *--to save Pack QTY in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'NPCKQTY'),1)
    laUsrField[lnFieldPos,6] = laData[12]

    *--to save Price Per Piece in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'LPCKPRPIEC'),1)
    laUsrField[lnFieldPos,6] = laData[14]

    *--to save Qty Per Piece in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'LQTYPCS'),1)
    laUsrField[lnFieldPos,6] = laData[15]


  ELSE
    INSERT INTO Spck_Hdr (Type,Account,Pack_ID,CPKVERSION,cDivision,Season,dAdd_Date,;
                          cAdd_Time,cAdd_User,Desc) ;
                VALUES   ('P' ,laData[1],laData[2],ALLTRIM(STR(lnVer,4)),laData[4],laData[5],gdSysDate,;
                           gfGettime(),gcUser_Id,laData[3])
                           
    *--to save version in user defined field         
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPKVERSION'),1)
    laUsrField[lnFieldPos,6] = ALLTRIM(STR(lnVer,4))

    *--to save Range in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'LRANGE'),1)
    laUsrField[lnFieldPos,6] = laData[6]
            
    *--to save Pack Price in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'NPKSLPRICE'),1)
    laUsrField[lnFieldPos,6] = laData[7]

    *--to save Pack Color in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPKCOLOR'),1)
    laUsrField[lnFieldPos,6] = laData[9]

    *--to save Size in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPCKSIZE'),1)
    laUsrField[lnFieldPos,6] = laData[10]

    *--to save pack calculated price in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'NPCKCLPR'),1)
    laUsrField[lnFieldPos,6] = laData[11]

    *--to save Pack QTY in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'NPCKQTY'),1)
    laUsrField[lnFieldPos,6] = laData[12]

    *--to save Price Per Piece in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'LPCKRECAL'),1)
    laUsrField[lnFieldPos,6] = laData[13]

    *--to save Price Per Piece in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'LPCKPRPIEC'),1)
    laUsrField[lnFieldPos,6] = laData[14]


    *--to save Qty Per Piece in user defined field        
    lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'LQTYPCS'),1)
    laUsrField[lnFieldPos,6] = laData[15]


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
        lcSetDel = SET('DELETE')
        SET DELETE ON
        IF !SEEK('P'+Account+Pack_ID+cPkColor+cPckSize+CPKVERSION+Style,'Spck_Lin')
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
        SET DELETE &lcSetDel 
      ELSE
        IF DELETED() AND SEEK(&lcLinFil..Type+&lcLinFil..Account+&lcLinFil..Pack_ID+&lcLinFil..cPkColor+&lcLinFil..cPckSize+&lcLinFil..CPKVERSION+&lcLinFil..Style,"Spck_Lin")
        
          SELECT Spck_Lin
          IF DELETED('Spck_Lin')
            LOCATE REST FOR Type+Accuont+Pack_ID+cPkColor+cPckSizeCPKVERSION+Style = ;
                   &lcLinFil..Type+&lcLinFil..Account+&lcLinFil..Pack_ID+&lcLinFil..cPkColor+&lcLinFil..cPkColor+&lcLinFil..CPKVERSION+&lcLinFil..Style AND !DELETED()
          ENDIF
          BLANK
          DELETE
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  
  IF  llReturn
    *--No Selling price entered
    IF laData[7]=0
      = gfModalGen("INM00000B42001","Dialog",.F.,.F.,"The Pack ID is saved with no selling price.")
    ENDIF
    *--No upc entered
    IF !(SEEK(laData[1]+laData[2]+'   '+laData[9]+laData[10],'STYLEUPC') OR ;
       SEEK("*****"+laData[2]+'   '+laData[9]+laData[10],'STYLEUPC'))AND (laData[1] <> "*****")
      = gfModalGen("INM00000B42001","Dialog",.F.,.F.,"This Pack ID does not have a UPC generated for it.")
    ENDIF
    IF SEEK("*****"+laData[2]+'   '+laData[9]+laData[10],'STYLEUPC'))AND (laData[1] <> "*****") AND !EMPTY(lcUPCNo)
      SELECT STYLEUPC
      SCATTER MEMVAR MEMO
      m.Account = laData[1]
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDIF
    
  ENDIF
ENDIF

SELECT(lnCurAlias)

SET DELETED &lcDelStat

llReCal = .F.

llCSave = llReturn

*!*************************************************************
*! Name      : lfTrap
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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

*-- This function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcBrTtl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrTab
  ON KEY LABEL BACKTAB DO lfBrBack
ENDIF
*!*************************************************************
*! Name      : lfClrTrap
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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

*-- This function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

IF glFromBrow
  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lfBrTab
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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

ACTIVATE WINDOW (lcIcPack1)
IF laScrMode[1]
  _CUROBJ=OBJNUM(pbSlct)
ELSE
  IF laScrMode[2]
    _CUROBJ=OBJNUM(pbSlct)
  ELSE
    _CUROBJ=OBJNUM(pbSav)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfBrBack
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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

ACTIVATE WINDOW (lcIcPack4)

*!*************************************************************
*! Name      : lfvBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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

*:**************************************************************************
*:* Name        : lfNewVer
*!* Developer   : Mohamed Shokry (MHM)
*!* Date        : 03/18/02
*:* Purpose     :  Validate new version
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfNewVer()
*:***************************************************************************
*
FUNCTION lfNewVer
=lfNEWVERSN()

*:**************************************************************************
*:* Name        : lfvOldVal
*!* Developer   : Mohamed Shokry (MHM)
*!* Date        : 03/18/02
*:* Purpose     : Hold Old value of a control
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvOldVal()
*:***************************************************************************
FUNCTION lfvOldVal
lcOldVal = EVAL(SYS(18))
*-- end of lfvOldVal.

*!*************************************************************
*! Name      : lpDelscr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
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
PRIVATE lnRecNo,lcOldOrd,llFound

STORE .F. TO llFound 

*--we check If this pack has  generic UPC
SELECT SPCK_HDR
IF Account="*****" AND SEEK(Account+Pack_Id+'   '+cPkColor+cPckSize,'STYLEUPC')
   *-message this pack has SKU,can not remove  
   *--OK
  =gfModalGen("INM00000B42001","Dialog",.F.,.F.,"This pack has generic UPC,can not remove ")
  =gfObj_Lock(.F.)
  RETURN
ENDIF

*--we check If this pack has SKU 
SELECT SPCK_HDR
lcFilt = SET('FILTER')
SET FILTER TO
lcPackSek = Account+PADR(pack_id,19)+cPkcolor+cpcksize
lcOldOrd = SET('ORDER')
lnRecNo = RECNO()
SET ORDER TO TAG Spck_hdrst
IF SEEK('S' + lcPackSek)
   *-message this pack has SKU,can not remove  
   *--OK
  =gfModalGen("INM00000B42001","Dialog",.F.,.F.,"This pack has SKU,can not remove ")
  SET ORDER TO &lcOldOrd 
  GOTO lnRecNo 
  SET FILTER TO &lcFilt
  =gfObj_Lock(.F.)
  RETURN
ENDIF

SET ORDER TO &lcOldOrd 
GOTO lnRecNo 
SET FILTER TO &lcFilt

*--we check if this pack has order or invoice
lcStr = ""
llFree  = lfChkPack()
IF !llFree  
  *-message this pack belong to order,can not remove 
  *--OK
  =gfModalGen("INM00000B42001","Dialog",.F.,.F.,"This pack belong to active " + lcStr + ",can not remove.")
  SELECT SPCK_HDR
  =gfObj_Lock(.F.)
  RETURN
ENDIF

SELECT SPCK_LIN
*B606395,1 HBG 08/21/2002 Cancel the Filter to fix Bug of when we delete an existing 
*B606395,1                pack id, the  program doesn't blank out the records in spck_lin file [Begin]
lcFilt = SET('FILTER')
SET FILTER TO
*B606395,1 [End]

IF SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10]+laData[8])
  *B606395,1 HBG 08/21/2002 Fix Bug of when we delete an existing pack id, the 
  *B606395,1                program doesn't blank out the records in spck_lin file [Begin]
  *DELETE REST WHILE Type+Account+Pack_id+cPkColor+cPckSize+cPkVersion+Style = 'P'+laData[1]+laData[2]+laData[9]+laData[10]+laData[8]
  BLANK  FOR Type+Account+Pack_id+cPkColor+cPckSize+cPkVersion+Style = 'P'+laData[1]+laData[2]+laData[9]+laData[10]+laData[8] 
  DELETE FOR EMPTY(Pack_id)
  *B606395,1 [End]
ENDIF
*B606395,1 HBG 08/21/2002 Set the filter to the previous one [Begin]
SET FILTER TO &lcFilt
*B606395,1 [End]
SELECT SPCK_HDR
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
GATHER FROM laData FIELDS &lcScFields MEMO 
DELETE

laScrMode    = .F.
laScrMode[1] = .T.
SHOW GETS

*:**************************************************************************
*:* Name        : lfNewVersn
*!* Developer   : Mohamed Shokry (MHM)
*!* Date        : 03/18/02
*:* Purpose     : Add a new version number for the current pack id for certain account
*:***************************************************************************
*:* Called from : Pack Generation screen
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfNewVersn()
*:***************************************************************************
FUNCTION lfNewVersn
PRIVATE lcVer
*C200479,1 HBG 30/12/2002 Allow creating versions of range Packs [Begin]
*IF laData[6]
*  =gfModalGen("INM00000B42001","Dialog",.F.,.F.,"This pack is a range one you can not make versions")
*  RETURN
*ENDIF
*C200479,1 [End]

*--Get current pack values
SELECT SPCK_HDR
SCATTER MEMV MEMO
lcVer = M.CPKVERSION

*--Disable Lock State
REPLACE LLOK_STAT WITH .F.
*B607252,1 HBG 11/02/2004 Fix bug of creating new version of the pack if the version is 10 or more[Begin]
=SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10])
lnVersion  = 0
*B607252,1 [End]
*--Get a new version number 
SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION = 'P'+laData[1]+laData[2]+laData[9]+laData[10]
  *B607252,1 HBG 11/02/2004 Fix bug of creating new version of the pack if the version is 10 or more[Begin]
  *M.CPKVERSION  = SPCK_HDR.CPKVERSION 
  lnVersion  = lnVersion + 1
  *B607252,1 [End]
ENDSCAN
*B607252,1 HBG 11/02/2004 Fix bug of creating new version of the pack if the version is 10 or more[Begin]
*M.CPKVERSION = STR(VAL(M.CPKVERSION)+1,4)
lnVersion  = lnVersion + 1
M.CPKVERSION = ALLTRIM(STR(lnVersion))
*B607252,1 [End]

*--Update version in laData

*--Update version in laUsrFields
lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPKVERSION'),1)
laUsrFields[lnFieldPos,6] = M.CPKVERSION

SELECT * FROM SPck_Lin;
  WHERE Type + Account   + Pack_ID   + cPkColor +cPckSize   + ALLTRIM(CPKVERSION) + Style;
  =     'P'  + laData[1] + laData[2] +laData[9] +laData[10] + ALLTRIM(lcVer) ;
  INTO DBF (gcWorkDir + lcLinFil)  
SELECT(lcLinFil)
INDEX ON Type+Account+Pack_ID+cPkColor+cPckSize+CPKVERSION+Style TAG StyPack ADDITIVE

*--Update Version and lock state in detail file
GO TOP
REPLACE ALL CPKVERSION WITH ALLTRIM(M.CPKVERSION) ,;
            LLOK_STAT  WITH .F. 

=SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10]+laData[8],'SPCK_HDR')

*--Turn on Add mode
laScrMode    = .F.  
laScrMode[4] = .T.  
DIME laVer[ALEN(laVer)+1]
laVer[ALEN(laVer)] = M.CPKVERSION
lnVer = VAL(M.CPKVERSION)
*--to copy version from the original one
llPackCopy = .T.

laData[8] = laVer[lnVer]

SHOW GETS

*C200479,1 HBG 30/12/2002 Allow creating versions of range Packs [Begin]
*SHOW GET laData[6] DISABLE
SHOW GET laData[6] ENABLE
*C200479 [End]
SHOW GET lnVer     DISABLE

SHOW GET lnPTotQt    ENABLE   
SHOW GET lnSellPr    ENABLE   
SHOW GET pncalclt    ENABLE   
*C200479,1 HBG 30/12/2002 Change Pric/PC check box to be popup. [Begin]
*SHOW GET laData[14]  ENABLE   
SHOW GET lnPrPeic  ENABLE   
*C200479,1 [End]
SHOW GET lnStyPrice  ENABLE   
FOR lnCnt = 1 TO SCALE.CNT
  lcCount = STR(lnCnt,1)
  SHOW GET m.Qty&lcCount ENABLE   
ENDFOR

*--Ask to undo updates
llCUpDate = .T.

_CUROBJ = OBJNUM(laData[3])

*-- end of lfNewVersn.

*:**************************************************************************
*:* Name        : lfVerCount
*!* Developer   : Mohamed Shokry (MHM)
*!* Date        : 03/18/02
*:* Purpose     : browse a screen if there are more than one version of the entered pack id
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfVerCount()
*:***************************************************************************
FUNCTION lfVerCount
PRIVATE lcOldAlias,lcAlias
lcAlias = ''
lcOldAlias = SELECT()
laScrMode    = .F.   
lnVersions = 0    && lnVersions--> Holds number of versions of this pack id for this account

SELECT SPCK_HDR
=gfOpenFile(gcDataDir+'SPCK_HDR','SPCK_HDRVr','SH',@lcAlias,.T.)
SELECT (lcAlias)
=SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10],(lcAlias))
SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+cPkColor+cPckSize+CPKVERSION = 'P'+laData[1]+laData[2]+laData[9]+laData[10]
  lnVersions = lnVersions + 1  
ENDSCAN
USE IN (lcAlias)

IF lnVersions > 1
  PRIVATE lnBrowVer
  lnBrowVer = gfModalGen("INM00000B42015","Dialog",.F.,.F.,;
                  'The entered Pack ID has more than one version for the same account, Please select one.')
  *'There are more than one version of the selected pack id for the same account'
  IF lnBrowVer=1   && BROWSE
    PRIVATE laBrowVer,lcBrTitle
    DIME laBrowVer[12]
    lcBrTitle  = 'Available Packs'            
    lcBrFields = "Account           :R :H='Account'      :16," +;
                 "Pack_ID           :R :H='Pack_ID'      :16," +;
                 "Desc              :R :H='Desc'         :30," +;
                 "CPKVERSION        :R :H='Version'      :7,"  +;
                 "cPkColor          :R :H='Color'      :7,"  +;
                 "LCPCKSIZE=lfGetSize(CPCKSIZE) :R :H='Size'    :6,"  +;
                 "cDivision         :R :H='Devision'     :8,"  +;
                 "Season            :R :H='Season'       :8,"  +;
                 "NPKSLPRICE        :R :H='Selling Price':13," +;
                 "STYLEUPC.CUPCNUM1 :R :H='UPC'          :7,"  +;  
                 "STYLEUPC.CUPCNUM2 :R :H=''             :6,"  +;  
                 "STYLEUPC.CUPCNUM3 :R :H=''             :2"            

    SELECT SPCK_HDR
    
    lcKey = "'P'+laData[1]+laData[2]+laData[9]+laData[10]"
    =gfBrows(lcKey,"Account, Pack_ID   , Desc     ,cDivision  ,Season,;
              LRANGE , NPKSLPRICE, CPKVERSION, CPKCOLOR,CPCKSIZE" ;  
              ,"laBrowVer",lcBrTitle)
    IF !EMPTY(laBrowVer)
      SELECT ALLTRIM(CPKVERSION) ;
      FROM Spck_Hdr ;
      WHERE (Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion='P'+laData[1]+laData[2]+laData[9]+laData[10]);
      INTO ARRAY laVer; 
      ORDER BY CPKVERSION 
      DIME laVer[ALEN(laVer)]
      lnVer = ASCAN(laVer,ALLTRIM(laBrowVer[8]))

      =SEEK('P'+laBrowVer[1]+laBrowVer[2]+laBrowVer[9]+laBrowVer[10]+laBrowVer[8],'SPCK_HDR')
      laScrmOde[2] = .T.               && Turn to view mode
    ELSE
      laScrmOde[1] = .T.               && Turn to SELECT mode
    ENDIF
  ELSE              && REENTER
    laScrmOde[1] = .T.                 && Turn to select mode
  ENDIF                              
ELSE  && ONLY ONE VERSION
  laScrmOde[2] = .T.                   && Turn to view mode
ENDIF
laData[8] = laVer[lnVer]

SELECT (lcOldAlias)
*-- end of lfVerCount.

*:**************************************************************************
*:* Name        : lfvColor
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : Valid function for Color
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvColor()
*:***************************************************************************
FUNCTION lfvColor
PRIVATE lcBrFields ,lcBrTitle,lnCurAlias

lnCurAlias = SELECT(0)

lnLstKey = LASTKEY()
IF (lnLstKey = 13 AND !MDOWN()) 

*OR (SUBSTR(ALLTRIM(laData[9]),1,6) <> CODES.cCode_No) 
  IF !SEEK("N"+"COLOR     "+ALLTRIM(laData[9]),'CODES') ;
     OR llBrowse
   
    llBrowse = .F.

    lcBrTitle  = 'Color'
    lcBrFields = "cCode_No          :R :H='Color'      :10," +;
                 "cDiscrep          :R :H='Desc'       :30" 
     
     SELECT cCode_No,cDiscrep ;
            FROM CODES;
            WHERE (Codes.cdefcode = 'N') AND;
            (Codes.cFld_Name ='COLOR     ')AND(Codes.cRltField='N');
            INTO DBF (gcWorkDir+lcTempColr) ;
            ORDER BY cCode_No
    
    SELECT(lcTempColr)

    *C200590,1 Enhancement to browse only Obsolete Colors. [Begin]
    SCAN
      IF SEEK('N'+'COLOR     '+cCode_no+SPACE(30)+'LLOBSOLETE','Codes') AND ALLTRIM(Codes.cRltd_Vlu) = "T" 
        DELETE
      ENDIF 
    ENDSCAN
    *C200590,1 Enhancement to browse only Obsolete Colors. [End]

    PRIVATE laBrow
    DIMENSION laBrow[1]
    laBrow = ''

    INDEX ON cCode_No TAG lcTempColr ADDITIVE

    =ARIABROW("",lcBrTitle,.F.,.F.,.F.,.F.,.F.,.F.,"cCode_No","laBrow",.F.,.F.,.F.)    
    *=gfBrows("","cCode_No" ,"laBrow",lcBrTitle)
                  
    IF !EMPTY(laBrow)
        laData[9] = laBrow[1]
    ELSE
      STORE SPACE(0) TO laData[9]
      _CUROBJ = OBJNUM(laData[9])
    ENDIF

  ENDIF  

  IF laScrMode[1] OR laScrMode[2]
    IF !SEEK('P'+laData[1]+laData[2]+laData[9],'Spck_Hdr') 
      IF laData[1] = "*****"
        *-- THIS generic PACK_ID does not exist
        *-- < Browse > < Reenter >
        lnABR = gfModalGen("QRM42091B42014","Dialog")  
      ELSE
        *-- THIS PACK ID IS NOT FOUND DO YOU WANT TO BROWSE OR ADD
        *-- < Browse > < Reenter >
        lnABR = gfModalGen("QRM42090B42014","Dialog",'Pack ID and Color'+'|'+laData[1])
      ENDIF
 
      DO CASE
        CASE lnABR = 1            &&Add
          *SHOW GET laData[1]
          *laScrMode    = .F.
          *laScrMode[4] = .T.        
          *SHOW GETS

        CASE lnABR = 2            &&Reenter
          STORE SPACE(0) TO laData[9]
          _CUROBJ = OBJNUM(laData[9])
      ENDCASE

    ELSE
      SHOW GET lcScale ENABLE
      SHOW GET pbSizBrow ENABLE
    ENDIF
  ENDIF  
ENDIF
*--Update Color if changed
SELECT(lcLinFil)
LOCATE
REPLACE ALL cPkColor  WITH laData[9] 

SELECT(lnCurAlias)

*:**************************************************************************
*:* Name        : lfvScale
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : Valid function for Scale
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvScale()
*:***************************************************************************
FUNCTION lfvScale

PRIVATE lcBrFields ,lcBrTitle,lnCurAlias

lnCurAlias = SELECT(0)
IF MDOWN() 
  RETURN
ENDIF

IF laScrMode[4] AND EMPTY(lcScale) 
  IF SEEK('P'+laData[1]+laData[2]+laData[9],'Spck_Hdr') 
    =gfModalGen("INM00000B42001","Dialog",.F.,.F.,"This pack add before ,please choose another name")
    SELECT(lnCurAlias)
    _CUROBJ = OBJNUM(lcScale)
    RETURN
  ENDIF
ENDIF


IF !SEEK("S"+ALLTRIM(lcScale),'SCALE') OR llBrowse 

  llBrowse = .F.

  lcBrTitle  = 'Scale'
  lcBrFields = "Scale             :R :H='Scale'      :3," +;
               "cscl_desc         :R :H='Desc'       :10," +;
               "Sz1               :R :H='Size1'      :5," +;
               "Sz2               :R :H='Size2'      :5," +;
               "Sz3               :R :H='Size3'      :5," +;
               "Sz4               :R :H='Size4'      :5," +;
               "Sz5               :R :H='Size5'      :5," +;
               "Sz6               :R :H='Size6'      :5," +;
               "Sz7               :R :H='Size7'      :5," +;
               "Sz8               :R :H='Size8'      :5" 
               
   SELECT Scale,Cnt,cScl_Desc,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 ;
          FROM SCALE;
          WHERE (Type = 'S') ;
          INTO DBF (gcWorkDir+lcTempSize) ;
          ORDER BY Scale

  SELECT(lcTempSize)
  PRIVATE laBrow
  DIMENSION laBrow[1]
  laBrow = ''
  =gfBrows(.F.,"SCALE" ,"laBrow",lcBrTitle)

  IF !EMPTY(laBrow)
      lcScale = laBrow[1]
  ELSE
    STORE SPACE(0) TO lcScale
    _CUROBJ = OBJNUM(lcScale)
  ENDIF

  SELECT Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 ;
  FROM (lcTempSize) ;
  WHERE (Scale=ALLTRIM(lcScale));
  INTO ARRAY laSize

  *--TO REFRISH PopUp
  lnCnt = 0
  FOR lnCount = 1 TO 8
    IF !EMPTY(laSize[lnCount])
      lnCnt =lnCnt +1
    ELSE
      EXIT  
    ENDIF  
  ENDFOR
  DIME laSize[lnCnt]     
ELSE
  SELECT Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 ;
         FROM SCALE;
         WHERE (Type+Scale ='S'+ALLTRIM(lcScale)) ;
         INTO ARRAY laSize

  *--TO REFRISH PopUp
  lnCnt = 0
  FOR lnCount = 1 TO 8
    IF !EMPTY(laSize[lnCount])
      lnCnt =lnCnt +1
    ELSE
      EXIT  
    ENDIF  
  ENDFOR
  DIME laSize[lnCnt]     
ENDIF  
lnSize = 1
IF !EMPTY(lcScale)
  IF !llPackCopy
    laData[10] = ALLTRIM(lcScale)+"01"
  ELSE
    lnSize = VAL(RIGHT(laData[10],1))
  ENDIF  
  SHOW GET lnSize ENABLE
ELSE
  laData[10] = "   "
  STORE '' TO laSize
  SHOW GET laData[1] DISABLE
  SHOW GET laData[2] DISABLE
  SHOW GET laData[3] DISABLE
  SHOW GET laData[9] DISABLE
  SHOW GET lcScale   DISABLE
  SHOW GET lnSize    DISABLE
  SHOW GET pbPckBrow DISABLE
  SHOW GET pbSizBrow DISABLE
  SHOW GET pbClrBrow DISABLE

ENDIF  
IF laScrMode[1] OR laScrMode[2]
  IF !SEEK('P'+laData[1]+laData[2]+laData[9]+LEFT(lcScale,1),'Spck_Hdr') 
    IF laData[1] = "*****"
      *-- THIS generic PACK_ID does not exist
      *-- < Browse > < Reenter >
      lnABR = gfModalGen("QRM42091B42014","Dialog")  
    ELSE
      *-- THIS PACK ID IS NOT FOUND DO YOU WANT TO BROWSE OR ADD
      *-- < Browse > < Reenter >
      lnABR = gfModalGen("QRM42090B42014","Dialog",'Pack ID, Color and Scale'+'|'+laData[1])
    ENDIF
 
    DO CASE
      CASE lnABR = 1            &&Add
        *SHOW GET laData[1]
        *laScrMode    = .F.
        *laScrMode[4] = .T.        
        *SHOW GETS

      CASE lnABR = 2            &&Reenter
        STORE SPACE(0) TO lcScale
        _CUROBJ = OBJNUM(lcScale)
    ENDCASE
  ELSE
    SHOW GET lnSize ENABLE
  ENDIF
ENDIF  
*--Update Scale if changed
SELECT(lcLinFil)
LOCATE
REPLACE ALL cPckSize WITH laData[10]

SELECT(lnCurAlias)

*:**************************************************************************
*:* Name        : lfvSize
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : Valid function for Size
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSize()
*:***************************************************************************
FUNCTION lfvSize

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

laData[10] = ALLTRIM(lcScale) +"0"+STR(ASCAN(laSize,laSize[lnSize]),1)

IF laScrMode[4] AND !EMPTY(lcScale) 
  IF SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10],'Spck_Hdr') 
    =gfModalGen("INM00000B42001","Dialog",.F.,.F.,"This pack add before ,please choose another name")
    SELECT(lnCurAlias)
    _CUROBJ = OBJNUM(lcScale)
    RETURN
  ENDIF
ENDIF


*-- we add space to fitch Index
IF SEEK(laData[1]+laData[2]+"   "+laData[9]+ladata[10],'STYLEUPC')
  lcUPCNo =STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3
  SHOW GET lcUPCNo
ENDIF  
SHOW GET lcUPCNo
IF laScrMode[1] OR laScrMode[2]
  IF !SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10],'Spck_Hdr') 
    IF laData[1] = "*****"
      *-- THIS generic PACK_ID does not exist
      *--  < OK >
      = gfModalGen("QRM42091B42000","Dialog")  
    ELSE
      *-- THIS PACK ID IS NOT FOUND DO YOU WANT TO BROWSE OR ADD
      *-- < OK >
      = gfModalGen("QRM42090B42000","Dialog",'Pack ID, Color, Scale and Size'+'|'+laData[1])
    ENDIF
 
  ELSE
    laScrMode    = .F.
    laScrmOde[2] = .T.  
    =lfVERCOUNT()
    SHOW GETS    

    *- to get pack version
    SELECT ALLTRIM(CPKVERSION) ;
    FROM Spck_Hdr ;
    WHERE (Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion='P'+laData[1]+laData[2]+laData[9]+laData[10]);
    INTO ARRAY laVer; 
    ORDER BY CPKVERSION 

    *B127280,1 MHM 04/07/2005 resort array to sort by Integer not string[Start]
    FOR lnArrCont = 1 to ALEN(laVer)
      laVer[lnArrCont] = VAL(laVer[lnArrCont])
    ENDFOR 
    =ASORT(laVer)
    FOR lnArrCont = 1 to ALEN(laVer)
      laVer[lnArrCont] = ALLTRIM(STR(laVer[lnArrCont],4))
    ENDFOR 
    *B127280,1 MHM 04/07/2005 [End]

  ENDIF
ENDIF
*-- we add space to fitch Index
IF SEEK(laData[1]+laData[2]+"   "+laData[9]+ladata[10],'STYLEUPC')
  lcUPCNo =STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3
ENDIF  
SHOW GET lcUPCNo
SHOW GET pnCalclt DISABLE
SHOW GET laData[1] DISABLE
SHOW GET laData[2] DISABLE
SHOW GET laData[3] DISABLE
SHOW GET laData[9] DISABLE
SHOW GET lcScale DISABLE
SHOW GET lnSize DISABLE
SHOW GET pbPckBrow DISABLE
SHOW GET pbSizBrow DISABLE
SHOW GET pbClrBrow DISABLE


SELECT(lcLinFil)
LOCATE
REPLACE ALL cPckSize WITH laData[10]
SELECT(lnCurAlias)

*:**************************************************************************
*:* Name        : lfvStyPric
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : Valid function for Style price
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvStyPric()
*:***************************************************************************
FUNCTION lfvStyPric

PRIVATE lnAlias,lnOldVal

lnAlias   = SELECT(0)

lnOldVal  = &lcLinFil..npck_price

IF lnStyPrice < 0
  lnStyPrice = 0
  *-- can not accept negative values
  = gfModalGen("INM42000B42001","Dialog") 
  _CUROBJ = _CUROBJ 
ELSE

  SELECT(lcLinFil) 
  lnCalcPr  = lnCalcPr - TotQty * npck_price + m.TotQty * lnStyPrice 

  REPLACE npck_price WITH lnStyPrice 

  SHOW WINDOW (lcBrTtl) REFRESH SAME
  =lfRefresh()
  SELECT(lnAlias)

  PRIVATE lnCrAlias
  lnCrAlias = SELECT()
    
    IF laData[6] 
      IF !laData[14]
        SHOW GET lnPTotQt ENABLE
        FOR lnCnt = 1 TO SCALE.CNT
          lcCount = STR(lnCnt,1)
          SHOW GET m.Qty&lcCount DISABLE   
        ENDFOR
      ELSE
        lnPTotQt = 1
        laData[12] = lnPTotQt
        SHOW GET lnPTotQt DISABLE
      ENDIF
      m.TotQty = 0
      FOR lnCnt = 1 TO SCALE.CNT
        lcCount = STR(lnCnt,1)
        m.Qty&lcCount = 1
        m.TotQty = m.TotQty + 1
        SELECT(lcLinFil) 
        REPLACE Qty&lcCount WITH 1 ,;
                TotQty      WITH m.TotQty
      ENDFOR
      lnCalcPr    = lnCalcPr - TotQty * &lcLinFil..npck_price + m.TotQty * lnStyPrice 
      ladata[11]  = lnCalcPr
    ELSE
      FOR lnCnt = 1 TO SCALE.CNT
        lcCount = STR(lnCnt,1)
        SHOW GET m.Qty&lcCount ENABLE 
      ENDFOR
   ENDIF  
   *B123475,1 MHM 08/05/2004 [End]   
   IF laScrMode[3] AND !lfChkPack()
      FOR lnCnt = 1 TO SCALE.CNT
        lcCount = STR(lnCnt,1)
        SHOW GET m.Qty&lcCount DISABLE   
      ENDFOR
   ENDIF
   *B123475,1 MHM 08/05/2004 [End]   
   
  SELECT (lnCrAlias)
  = lfwBrow()
ENDIF

*--End of lfvStyPric
*:**************************************************************************
*:* Name        : lfvReCal
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : Valid function for recalculate prices
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvReCal()
*:***************************************************************************
FUNCTION lfvReCal
PARAMETER llFromSav
PRIVATE lnAlias ,lnCurrRec

*B606395,1 HBG 08/21/2002 If sell price <> 0 AND Calculated price <> 0 Don't Recalculate again[Begin]
*B121235,1  TMI [Start] Ask this question only for Price Per Pack
IF EMPTY(laData[14])
  *B121235,1  TMI [End  ] 
  IF lnSellPr <> 0 AND lnSellPr = lnCalcPr  
    llReCal = .T.
    RETURN
  ENDIF
  *B121235,1  TMI [Start] return if no type is selected 
  IF TYPE('laData[14]')='C'
    RETURN
  ENDIF
ENDIF   
*B121235,1  TMI [End  ]   
*B606395,1 [End]

*--if not from save get it from 
IF !llFromSav AND (lnSellPr = 0)
  *B606395,1 HBG 08/21/2002 Don't check "Price Per Piece" automatically if the "Sell.Price" 
  *B606395,1                is zero and we click "Re-Calc" [BEGIN]
  *laData[14] = .T.
  *B606395,1 [End]
  SELECT(lcLinFil) 
  lnCurrRec = RECNO()
  SCAN
    *B606395,1 HBG 08/21/2002 calculate the sell price based on style prices in case
    *B606395,1                of sell price = 0 [Begin]
    *REPLACE npck_price WITH lnSellPr
    lnSellPr = lnSellPr + (nPck_price * TotQty)
    *B606395,1 [End]
  ENDSCAN
  ladata[11] = 0
  *B606395,1 HBG 08/21/2002 Don't set Calculated price to 0 in case 
  *B606395,1                of sell price was 0 [Begin]
  *lnCalcPr  = 0
  *B606395,1 [End]
  IF BETWEEN(lnCurrRec ,1,RECCOUNT()) 
    GOTO lnCurrRec
  ENDIF
  *B606395,1 HBG 08/21/2002 Refresh Sell price field [Begin]
  *SHOW GET laData[14]
  SHOW GET lnSellPr   
  *B606395,1 [End]
  *B121235,1  TMI [Start] Update "NPKSLPRICE","nPckClPr" 
  lnCalcPr = lnSellPr
  SHOW GET lnCalcPr
  laData[7] = lnSellPr
  ladata[11] = lnCalcPr
  *B121235,1  TMI [End  ] 
  = lfwBrow()
  llReCal = .F.
  RETURN
ENDIF

lnAlias   = SELECT(0)
*--update calculated flag
laData[13] = .T.
llReCal = .T.
IF laData[14] 
  SELECT(lcLinFil) 
  lnCurrRec = RECNO()
  lnCalcPr  = 0
  SCAN
    REPLACE npck_price WITH lnSellPr
    lnCalcPr  = lnCalcPr + TotQty * lnSellPr
  ENDSCAN
  ladata[11] = lnCalcPr
  IF BETWEEN(lnCurrRec ,1,RECCOUNT()) 
  	GOTO lnCurrRec
  ENDIF
  = lfwBrow()
ELSE
  SELECT(lcLinFil) 
  lnCurrRec = RECNO()
  IF llFromSav .AND. lnSellPr <> lnCalcPr AND lnSellPr <> 0
    lcMessag = "Do you want assign Calc. price " +STR(lnCalcPr) +" to Sell price" 
    IF gfModalGen("QRM00000B42002","Dialog",.F.,.F.,lcMessag) = 1
      lnSellPr = lnCalcPr
      laData[7] = lnSellPr
      *B121235,1  TMI [Start] Update "nPckClPr" 
      laData[11] = lnCalcPr
      *B121235,1  TMI [End  ] 
      SHOW GET lnSellPr
      RETURN
    ENDIF
  ENDIF
  lnCalcPr  = 0
  lnTotPrice = 0
  lnCountRec = 0
  *B121235,1  TMI [Start] Sum of all pecies in "lnTotQty"
  lnTotQty = 0
  *B121235,1  TMI [End  ] 
  SCAN
    lnCountRec = lnCountRec +1
    lnTotPrice = lnTotPrice + TotQty * npck_price
    *B121235,1  TMI [Start] Get totqty of all styles in the pack
    lnTotQty = lnTotQty + TotQty
    *B121235,1  TMI [End  ] 
  ENDSCAN
  lnCalcPr  = 0

  IF lnSellPr = 0 AND lnTotPrice <>0
    lcMessag = "Do you want assign Calc. price " +STR(lnTotPrice) +" to Sell price" 
    IF gfModalGen("QRM00000B42002","Dialog",.F.,.F.,lcMessag) = 1
      lnSellPr = lnTotPrice
      laData[7] = lnSellPr
      *B121235,1  TMI [Start] Update "nPckClPr" 
      laData[11] = lnTotPrice
      *B121235,1  TMI [End  ] 
      SHOW GET lnSellPr
      RETURN
    *B606607,1 HBG 11/03/2002 If zero is assigned to sell price => don’t assign zero for price
    *                         of component styles. [Begin]
    *ELSE
      *IF gfModalGen("QRM00000B42002","Dialog",.F.,.F.,"Do you want assign Zero to Sell price")=1    
      *  lnSellPr = 0 
      *  lnCurrRec = RECNO()
      *  SCAN
      *    REPLACE npck_price WITH lnSellPr
      *  ENDSCAN
      *  ladata[11] = 0
      *  lnCalcPr  = 0
      *  IF BETWEEN(lnCurrRec ,1,RECCOUNT()) 
      *     GOTO lnCurrRec
      *  ENDIF
      *  laData[14] = .T.
      *  SHOW GET laData[14]
      *  RETURN
      *ELSE
      *  llReCal = .F.
      *  RETURN
      *                         of component styles. [Begin]
      *ENDIF
      *B606607,1 [End]
    ENDIF
  ENDIF
  
  *B606395,1 HBG 08/21/2002 Don't check "Price Per Piece" automatically if the "Sell.Price" 
  *B606395,1                is zero and we click "Re-Calc" [BEGIN]
  *IF  lnSellPr = 0 AND lnTotPrice  = 0
  *  laData[14] = .T.
  *  SHOW GET laData[14]
  *ENDIF
  *B606395,1 [End]
  
  IF !llFromSav AND lnTotPrice = 0
    lnTotPrice = lnPTotQt
  ENDIF
  
  lnCalPrSty  = 0
  SCAN
    *B121235,1  TMI [Start] No need to these variables
    *lnCalPrSty = (TotQty * IIF(npck_price <> 0, npck_price, 1)) / ;
    *             IIF(lnTotPrice = 0, lnCountRec, lnTotPrice)
    *lnCalPrSty = ROUND(lnCalPrSty, 2)
    *lnStyPer = lnCalPrSty * lnSellPr
    *lnCalcPr  = lnCalcPr + lnStyPer
    *B121235,1  TMI [End  ] 
    IF TotQty <> 0  
      *B121235,1  TMI [Start] Update peice price
      *REPLACE npck_price WITH ROUND(lnStyPer / TotQty, 2)
      REPLACE nPck_price WITH ROUND(lnSellPr/lnTotQty,2)
      lnCalcPr = lnCalcPr + nPck_price*TOTQTY
      *B121235,1  TMI [End  ] 
    ENDIF  
  ENDSCAN
  ladata[11] = lnCalcPr
  *B121235,1  TMI [Start] Update "NPKSLPRICE"
  laData[7] = lnSellPr
  *B121235,1  TMI [End  ] 
  
  IF BETWEEN(lnCurrRec ,1,RECCOUNT()) 
  	GOTO lnCurrRec
  ENDIF
  =lfwBrow()
ENDIF
IF !llFromSav
  llReCal = .F.
ENDIF
*--Undo Update
llCUpDate = .T.


SELECT(lnAlias)
*- End of lfvReCal
*:**************************************************************************
*:* Name        : lfvSelPric
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : Valid function for Style price
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSelPric()
*:***************************************************************************
FUNCTION lfvSelPric

ladata[7]  = lnSellPr
*--End of lfvSelPric
*:**************************************************************************
*:* Name        : lfvVersin
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : Valid function for vesion
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvVersin()
*:***************************************************************************
FUNCTION lfvVersin
PRIVATE lnAlias

lnAlias   = SELECT(0)
IF !EMPTY(laData[2])
  =SEEK('P'+laData[1]+laData[2]+laData[9]+laData[10]+ALLTRIM(laVer[lnVer]),'SPCK_HDR')
  laData[8] = ALLTRIM(laVer[lnVer])
ENDIF  

SHOW GETS
SELECT(lnAlias)

*--end of lfvVersin

*:**************************************************************************
*:* Name        : lfGetSize
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : get Size 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetSize()
*:***************************************************************************
FUNCTION lfGetSize
PARAMETER lcPackSize
PRIVATE lcNombr

IF !EMPTY(lcPackSize)
  =SEEK('S'+LEFT(lcPackSize,1),'SCALE')
  lcNombr = RIGHT(lcPackSize,1)
  lcSize=EVAL('SCALE.SZ'+lcNombr)
ELSE
  lcSize ='*****'
ENDIF  

RETURN lcSize 


*:**************************************************************************
*:* Name        : lfChkRang
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : Check range
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfChkRang()
*:***************************************************************************
FUNCTION lfChkRang
PRIVATE llRange
STORE .F. TO llRange

SELECT(lcLinFil)
LOCATE
SCAN
  IF llRange
    EXIT
  ENDIF
  IF TOTQTY>1
    llRange = .T.
  ENDIF
ENDSCAN
RETURN llRange

*:**************************************************************************
*:* Name        : lfVTotQty
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : Validate Total QTY
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfVTotQty()
*:***************************************************************************
FUNCTION lfVTotQty

laData[12]  = lnPTotQt

*:**************************************************************************
*:* Name        : lfvRange
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : Validate range
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvRange()
*:***************************************************************************
FUNCTION lfvRange
PRIVATE lnAlias
lnAlias = SELECT()

IF laData[6] 
  IF !laData[14]
    SHOW GET lnPTotQt ENABLE
    FOR lnCnt = 1 TO SCALE.CNT
      lcCount = STR(lnCnt,1)
      SHOW GET m.Qty&lcCount DISABLE   
    ENDFOR
  ELSE
    lnPTotQt = 1
    laData[12] = lnPTotQt
    SHOW GET lnPTotQt DISABLE
  ENDIF
  SELECT(lcLinFil) 
  SCAN 
    m.TotQty = 0
    FOR lnCnt = 1 TO SCALE.CNT
      lcCount = STR(lnCnt,1)
      m.Qty&lcCount = 1
      m.TotQty = m.TotQty + 1
      SELECT(lcLinFil) 
      REPLACE Qty&lcCount WITH 1 ,;
              TotQty      WITH m.TotQty
    ENDFOR
  ENDSCAN  
  lnCalcPr    = lnCalcPr - TotQty * &lcLinFil..npck_price + m.TotQty * lnStyPrice 
  ladata[11]  = lnCalcPr
ELSE
  FOR lnCnt = 1 TO SCALE.CNT
    lcCount = STR(lnCnt,1)
    SHOW GET m.Qty&lcCount ENABLE 
  ENDFOR
  SELECT(lcLinFil) 
  lnPTotQt = 0
  SCAN 
      lnPTotQt = SCALE.CNT +lnPTotQt
  ENDSCAN  
  SHOW GET lnPTotQt DISABLE
ENDIF  

*B123475,1 MHM 08/05/2004 [End]   
IF laScrMode[3] AND !lfChkPack()
   FOR lnCnt = 1 TO SCALE.CNT
     lcCount = STR(lnCnt,1)
     SHOW GET m.Qty&lcCount DISABLE   
   ENDFOR
ENDIF
*B123475,1 MHM 08/05/2004 [End]   
ACTIVATE WINDOW (lcBrTtl)
SELECT (lnAlias)


*:**************************************************************************
*:* Name        : lfvPrPric
*:* Developer   : Mohame Shokry(MHM)
*:* Date        : 03/20/2002
*:* Purpose     : Validate price per pice
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvPrPric()
*:***************************************************************************
FUNCTION lfvPrPric
PRIVATE lnAlias
lnAlias = SELECT()

*C200479,1 HBG 30/12/2002 Change Pric/PC check box to be popup. [Begin]
DO CASE
  CASE lnPrPeic = 2
    laData[14] = .T.
  CASE lnPrPeic = 3
    laData[14] = .F.
  Case lnPrPeic = 1
    laData[14] = ""
ENDCASE
*C200479,1 [End]

IF laData[6] 
  IF !laData[14]
    SHOW GET lnPTotQt ENABLE
    FOR lnCnt = 1 TO SCALE.CNT
      lcCount = STR(lnCnt,1)
      SHOW GET m.Qty&lcCount DISABLE   
    ENDFOR
  ELSE
    lnPTotQt = 1
    laData[12] = lnPTotQt
    SHOW GET lnPTotQt DISABLE
  ENDIF
  m.TotQty = 0
  FOR lnCnt = 1 TO SCALE.CNT
    lcCount = STR(lnCnt,1)
    m.Qty&lcCount = 1
    m.TotQty = m.TotQty + 1
    SELECT(lcLinFil) 
    REPLACE Qty&lcCount WITH 1 ,;
            TotQty      WITH m.TotQty
  ENDFOR
  lnCalcPr    = lnCalcPr - TotQty * &lcLinFil..npck_price + m.TotQty * lnStyPrice 
  ladata[11]  = lnCalcPr
  
ELSE
  FOR lnCnt = 1 TO SCALE.CNT
    lcCount = STR(lnCnt,1)
    SHOW GET m.Qty&lcCount ENABLE 
  ENDFOR
ENDIF  
*B123475,1 MHM 08/05/2004 [End]   
IF laScrMode[3] AND !lfChkPack()
   FOR lnCnt = 1 TO SCALE.CNT
     lcCount = STR(lnCnt,1)
     SHOW GET m.Qty&lcCount DISABLE   
   ENDFOR
ENDIF
*B123475,1 MHM 08/05/2004 [End]   

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvContShw
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
*! Purpose   : Control showing for prices and qty.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : = lfvContShw()
*!*************************************************************
*!
FUNCTION lfvContShw
PRIVATE llFound
STORE .F. TO llFound 
lnAlias = SELECT(0)
*:B123475,1 MHM 08/05/2004 if GENERAIC pack then return without check[Start]
IF EMPTY(laData[1]) OR (laData[1] = '*****')
  RETURN 
ENDIF
*:B123475,1 MHM [End]

SELECT(lcLinFil)
*--we check if this pack has order or invoice
IF SEEK(&lcLinFil..STYLE,'INVLINE')
  SELECT INVLINE
  SCAN REST WHILE style+invoice+STR(lineno,6) = &lcLinFil..STYLE
    IF Pack_ID+cPkColor+cPckSize+CPKVERSION = LADATA[2] +LADATA[9]+LADATA[10] +LADATA[8] 
      llFound = .T.
    ENDIF
  ENDSCAN
  SELECT(lcLinFil)
  IF llFound 
    =SEEK(STYLE,'STYLE')
    =SEEK("S"+Style.Scale,'SCALE')
    SHOW GET lnPTotQt    DISABLE
    SHOW GET pncalclt    DISABLE
    *C200479,1 HBG 30/12/2002 Allow the user to edit range/price per piece fields on the 
    *C200479,1                pack header even if the pack is ordered/invoiced [Begin]
    *SHOW GET laData[14]  DISABLE
    *SHOW GET laData[6]   DISABLE
    *C200479,1 [End]
    SHOW GET lnStyPrice  DISABLE
    FOR lnCnt = 1 TO SCALE.CNT
      lcCount = STR(lnCnt,1)
      SHOW GET m.Qty&lcCount DISABLE   
    ENDFOR
  ENDIF
ENDIF

IF SEEK(&lcLinFil..STYLE,'ORDLINE')
  SELECT ORDLINE
  SCAN REST WHILE style+invoice+STR(lineno,6) = &lcLinFil..STYLE
    IF Pack_ID+cPkColor+cPckSize+CPKVERSION = LADATA[2] +LADATA[9]+LADATA[10] +LADATA[8] 
      llFound = .T.
    ENDIF
  ENDSCAN
  SELECT(lcLinFil)
  IF llFound 
    =SEEK(STYLE,'STYLE')
    =SEEK("S"+Style.Scale,'SCALE')
    SHOW GET lnPTotQt    DISABLE
    *SHOW GET lnSellPr    DISABLE
    SHOW GET pncalclt    DISABLE
    *C200479,1 HBG 30/12/2002 Allow the user to edit range/price per piece fields on the 
    *C200479,1                pack header even if the pack is ordered/invoiced [Begin]
    *SHOW GET laData[14]  DISABLE
    *SHOW GET laData[6]   DISABLE
    *C200479,1 [End]

    SHOW GET lnStyPrice  DISABLE
    FOR lnCnt = 1 TO SCALE.CNT
      lcCount = STR(lnCnt,1)
      SHOW GET m.Qty&lcCount DISABLE   
    ENDFOR
  ENDIF
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfChkPack
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/18/02
*! Purpose   : check if this pack has order or invoice
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : = lfChkPack()
*!*************************************************************
*!
FUNCTION lfChkPack

*:B123475,1 MHM 08/05/2004 if GENERAIC pack then return without check[Start]
IF EMPTY(laData[1]) OR (laData[1] = '*****')
  RETURN 
ENDIF
*:B123475,1 MHM [End]

SELECT SPCK_LIN
SCAN REST WHILE Type+Account+Pack_id+cpkcolor+cPckSize+cPkversion+Style ='P'+laData[1]+laData[2]+laData[9]+laData[10]+laData[8]
  IF SEEK(SPCK_LIN.STYLE,'INVLINE')
    SELECT INVLINE
    SCAN REST WHILE style+invoice+STR(lineno,6) = SPCK_LIN.STYLE
      IF Pack_ID+cPkColor+cPckSize+CPKVERSION = LADATA[2] +LADATA[9]+LADATA[10] +LADATA[8] AND;
         SEEK(INVLINE.Invoice,'INVHDR') AND INVHDR.Status <> 'V'
        lcStr  = 'Invoice'
        RETURN .F.
      ENDIF
    ENDSCAN
  ENDIF
  
  IF SEEK(SPCK_LIN.STYLE,'ORDLINE')
    SELECT ORDLINE
    SCAN REST WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6) = SPCK_LIN.STYLE
      IF Pack_ID+cPkColor+cPckSize+CPKVERSION = LADATA[2] +LADATA[9]+LADATA[10] +LADATA[8] AND;
         SEEK('O'+ORDLINE.Order,'ORDHDR') AND ORDHDR.Status <> 'X'
        IF laData[1] <>  OrdLine.cPckAcc  
          LOOP
        ENDIF
        lcStr  = 'Order'
        RETURN .F.
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
