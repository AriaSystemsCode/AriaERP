*:***************************************************************************
*: Program file  : ICOBJPT.PRG 
*: Program desc. : Customized Object Link Directories Screen for Dare
*: System        : Aria Advantage Series.
*: Module        : IC
*: Developer     : Adel Mohhamed El Gazzar (ADEL)
*: Date          : 08/14/02
*: Refer to      : C(200388)
*:***************************************************************************
*: B606735,1 TMI 12/08/2002 Get office path from C:\OFICPATH.MEM
*:***************************************************************************

lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
lcCan      = gcBmpHome + "Can.bmp"
lcOk       = gcBmpHome + "Ok.bmp"
lcWindTitl = "Object Link Directories Screen for Dare"
gcBaseWind = 'AWRICOBJPT'
STORE "" TO lcPath1,lcPath2,lcPath3,lcPath4,lcPath5,lcPHead1,lcPHead2,lcPHead3,lcPHead4,lcPHead5,lcPOffice
*-- Check the memo file existance.
llFoundP = FILE('DARMAIN.MEM')
IF llFoundP
  RESTORE FROM DARMAIN.MEM ADDITIVE
  *B606735,1 TMI [Start] Get office path from C:\OFICPATH.MEM
  lcPOffice = ' ' 
  IF FILE('C:\OFICPATH.MEM')
    RESTORE FROM C:\OFICPATH.MEM ADDITIVE
  ENDIF
  *B606735,1 TMI [End  ] 
ENDIF

IF !gfSetup()
  RETURN
ENDIF

DO (gcScrDir+gcWinAppl+"\ICOBJPT.SPX")

FUNCTION lfvOk

SAVE ALL LIKE lcP* TO DARMAIN.MEM
*B606735,1 TMI [Start] save office path to C:\OFICPATH.MEM
SAVE ALL LIKE lcPOffice* TO C:\OFICPATH.MEM
*B606735,1 TMI [End  ] 
CLEAR READ

FUNCTION lfvGetDir
PARAMETERS lcPathNo

IF lcPathNo <> '0'
  lcPath&lcPathNo = GETDIR()
  =lfRefresh(gcBaseWind)
ELSE
  lcPOffice = GETDIR()
  =lfRefresh(gcBaseWind)
ENDIF  






