*:***************************************************************************
*: Program file  : ICOBJNK.PRG 
*: Program desc. : Customized Object Link Directories Screen for NikNak
*: System        : Aria Advantage Series.
*: Module        : IC
*: Developer     : NADER NABIL (NNA)
*: Date          : 09/19/2004
*: Refer to      : CP#123614
*:***************************************************************************
*: Modifications :
*:***************************************************************************

lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
lcCan      = gcBmpHome + "Can.bmp"
lcOk       = gcBmpHome + "Ok.bmp"
lcWindTitl = "Object Link Directories Screen for Nik Nak Clothing"
gcBaseWind = 'AWRICOBJNK'
STORE "" TO lcPath1,lcPath2,lcPath3,lcPath4,lcPath5,lcPHead1,lcPHead2,lcPHead3,lcPHead4,lcPHead5,lcPOffice
*-- Check the memo file existance.
llFoundP = FILE('NAKMAIN.MEM')
IF llFoundP
  RESTORE FROM NAKMAIN.MEM ADDITIVE
  *-- Get office path from C:\OFICPATH.MEM
  lcPOffice = ' ' 
  IF FILE('C:\OFICPATH.MEM')
    RESTORE FROM C:\OFICPATH.MEM ADDITIVE
  ENDIF
ENDIF
IF !gfSetup()
  RETURN
ENDIF
DO (gcScrDir+gcWinAppl+"\ICOBJNK.SPX")

*-- END OF the Program.

*!**************************************************************************
*! Name      : lfvOk
*! Developer : NADER NABIL (NNA)
*! Date      : 09/19/2004
*! Purpose   : Valid function for [OK] Button
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C123614
FUNCTION lfvOk

SAVE ALL LIKE lcP* TO NAKMAIN.MEM
*--Save office path to C:\OFICPATH.MEM
SAVE ALL LIKE lcPOffice* TO C:\OFICPATH.MEM
CLEAR READ
*-- END OF FUNCTION lfvOk.

*!**************************************************************************
*! Name      : lfvGetDir
*! Developer : NADER NABIL (NNA)
*! Date      : 09/19/2004
*! Purpose   : Valid function for the Paths Address
*!**************************************************************************
*! Passed Parameters : lcPathNo
*!**************************************************************************
*! Called From : the Screen
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C123614
FUNCTION lfvGetDir
PARAMETERS lcPathNo

IF lcPathNo <> '0'
  lcPath&lcPathNo = GETDIR()
  =lfRefresh(gcBaseWind)
ELSE
  lcPOffice = GETDIR()
  =lfRefresh(gcBaseWind)
ENDIF  

*-- END OF FUNCTION lfvGetDir.
