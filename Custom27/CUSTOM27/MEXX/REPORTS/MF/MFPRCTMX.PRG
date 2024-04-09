*:***************************************************************************
*: Program file  : MFPRCTMX (200095)
*: Program desc. : Print CUTTING TICKETS for Consolidated Casuals
*: For Report    : MFPRCTMX.FRX,MFPRCSMX.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Developer     : RAMY MABROUK
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfItemMask()
*:                 gfCodDes()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:Modifications:
*:***************************************************************************

*-- If we will print cost sheet
IF llrPrtCs
  lnCurAls = SELECT(0)
  SELECT (lcCostF)
  INDEX ON CUTTKT+Typ+cCatgTyp+IClr+Item TAG (lcCostF)
  SELECT (lnCurAls)
ENDIF && end of (IF llrPrtCs)

*!*************************************************************
*! Name      : lfGetDiv
*! Developer : RAMY MABROUK
*! Date      : 10/25/1999
*! Purpose   : Get the Division descreption
*!*************************************************************
*! Called from : MFPRCTMX.FRX
*!*************************************************************
*! Calls       : gfCodDes()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetDiv()
*!*************************************************************

FUNCTION lfGetDiv
PARAMETER lcDiv
lcDiv = gfCodDes(STYLE.CDIVISION, 'CDIVISION')



*!*************************************************************
*! Name      : lfGetStyTitl
*! Developer : RAMY MABROUK
*! Date      : 10/25/1999
*! Purpose   : Get the Major Header
*!*************************************************************
*! Called from : MFPRCTMX.FRX
*!*************************************************************
*! Calls       : gfItemMask()
*!*************************************************************
*! Passed Parameters : lcStyTitl
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetStyTitl()
*!*************************************************************

FUNCTION lfGetStyTitl
PARAMETER lcStyTitl

lcStyTitl = gfItemMask('HM')



*!*************************************************************
*! Name      : lfGetClr
*! Developer : RAMY MABROUK
*! Date      : 10/25/1999
*! Purpose   : Get the Color code + Color Desc.
*!*************************************************************
*! Called from : MFPRCTMX.FRX
*!*************************************************************
*! Calls       : gfItemMask() , gfCodDes()
*!*************************************************************
*! Passed Parameters : lcStyClr
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetClr()
*!*************************************************************

FUNCTION lfGetClr
PARAMETER lcStyClr

lcStyClr = SUBSTR(STYLE.Style, LEN(gfItemMask('PM'))+2 , 4 )
lcClr    = gfCodDes(ALLTRIM(lcStyClr) , 'COLOR')
lcStyClr = lcStyClr + '-' + lcClr



*!*************************************************************
*! Name      : lfPrnAgain
*! Developer : RAMY MABROUK
*! Date      : 10/25/1999
*! Purpose   : Get the Color code + Color Desc.
*!*************************************************************
*! Called from : MFPRCTMX.FRX
*!*************************************************************
*! Calls       : gfItemMask() , gfCodDes()
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrnAgain()
*!*************************************************************

FUNCTION lfPrnAgan

PARAMETER lcDummy

llPrnAgan = .F.
lcDummy = ''
