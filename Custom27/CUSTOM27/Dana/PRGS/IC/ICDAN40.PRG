*:**************************************************************************
*! Name : ICDAN40.PRG    C101778   Done for (Dana Undies)
*: Program desc. : Custom conversion entry screen.
*:         System: Aria Apparel system
*:      Developer: RAMY MABROUK
*:           Date: 03/07/2000
*:**************************************************************************
*: Calls : gfOpenFile()
*:         gfwCodePop()
*:         gfModalGen()
*:**************************************************************************
*! MODIFICATIONS:
*:**************************************************************************
*-- Variable Declaration
DECLARE laData[4], laGrp[1], laKeyField[1,4]
STORE '' TO lcScFields 
lnGrp      = 0   && Group popup

DIMEN laCodInfo[1,10]
laCodInfo[1,01] = "CSTYGROUP"
laCodInfo[1,02] = "laGrp"
laCodInfo[1,03] = "lnGrp"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.            
laCodInfo[1,06] = .F.            
laCodInfo[1,07] = "STYLE"     
laCodInfo[1,08] = "STYLE"     
laCodInfo[1,09] = "lcMajor+lcNMKey"
laCodInfo[1,10] = "cStyGroup"     

= gfwCodePop ( @laCodInfo, "CSTYGROUP","N" )

*-- Force to local Save procedure  (lpSavScr)
*laDefProc[9]  = .F.

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'GROUP'
laKeyField[1,4] = 1

*-- Opening files and calling gfSetup()
IF !gfSetup()
  RETURN
ENDIF

lcScFields = "cStyGroup, nConv1, nConv2, nConv3"
=gfReInUsrF(lcScFields, @laUsrFields)


*-- If calling the screen for the first time
IF !WEXIST(gcBaseWind)
  
  *-- Temporary file creation
  *-- Window definition
  
  *-- Blank laData elements
  SELECT (lcBaseFile)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ENDIF

DO (gcScrDir + gcWinAppl + '\ICDAN40.SPX')

*:****************************************************************************
*! Name      : lpShow
*! Developer : RAMY MABROUK (RAM)
*! Date      : 03/07/2000
*! Purpose   : Show Screen objects when Mode Changes.
*:****************************************************************************
*! Calls     : 
*!****************************************************************************
*! Passed Parameters : None.
*:****************************************************************************
*! Return    : None
*:****************************************************************************
*! Example   : = lpShow()
*:****************************************************************************

PROCEDURE lpShow
DO CASE
  *-- S E L E C T   M O D E
  CASE laSCrMode[1]

    lnGrp = 0

  *-- V I E W   M O D E
  CASE laSCrMode[2]

    *-- Get corresponding lnGrp
    lnElement = ASCAN(laGrp , laData[1])
    lnGrp     = IIF(lnElement > 0, ASUBSCRIPT(laGrp , lnElement , 1) , 0)
    SHOW GET lnGrp

  *-- E D I T   M O D E   
  CASE laSCrMode[3]

  *-- A D D   M O D E 
  CASE laSCrMode[4]
   
ENDCASE  

*:****************************************************************************
*! Name      : lfvGroup
*! Developer : RAMY MABROUK (RAM)
*! Date      : 03/07/2000
*! Purpose   : The group pop up valid function
*:****************************************************************************
*! Calls       : 
*!****************************************************************************
*! Passed Parameters : None.
*:****************************************************************************
*! Return      : None
*:****************************************************************************
*! Example     : = lfvGroup()
*:****************************************************************************
FUNCTION lfvGroup
IF type('laGrp[lnGrp,2]') <> 'U'
  laData[1] = laGrp[lnGrp,2]
  IF !gfSeekRec()
    lnGrp   = 0
    SHOW GET lnGrp
  ENDIF
ENDIF