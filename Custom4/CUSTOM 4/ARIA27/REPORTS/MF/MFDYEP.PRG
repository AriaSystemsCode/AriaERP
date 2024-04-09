*:****************************************************************************
*: Program file        : MFDYEP
*: Program desc.       : Dye Order Form
*: For Report          : MFDYEPA.FRX
*: System              : Aria Advantage Series VER. 2.7
*: Module              : Manufucturing (MF)
*: Developer           : RAMY MABROUK (RAMY)
*: Tracking Job Number : C200098,1
*: Date                : October/99
*:****************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : gfApVnBrow()
*:                 gfCodDes()
*:                 gfRltFld()
*:                 gfGetAdr()
*:                 gfDispRe
*:                 gfBrows()
*:****************************************************************************
*: Called From         : System Menu (MF --> Output --> Dye Orders )
*:****************************************************************************
*: Passed Parameters  : None
*:****************************************************************************
*: Example : DO MFSTYDYE
*:****************************************************************************
*: Modification        :
*:B803095,1 SSH  19/03/00 Print NotePad
*:B603866,1 Ramy 31/08/2000 Add option in the option grid to print the notepad Yes\No
*:B605399,1 KHM  01/21/2001 Adding a function to set the cursor at the top of
*:B605399,1                 the file in the option grid.
*:****************************************************************************
*-- Variable diclartion
DECLARE laVenAdr[5] && Array that holds the vendor address

*-- Creating temp. file that will hold the collected data
SELECT POSLN
SET RELATION TO Style INTO STYLE ADDITIVE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru , 1)
*:B#803095,1 SSH 19/03/00 Print NotePad
*DIMENSION laFileStru[lnFileStru+1,4]
DIMENSION laFileStru[lnFileStru+2,4]
*:B#803095,1 SSH (END)
laFileStru[lnFileStru+1 , 1] = 'nStyWeight'
laFileStru[lnFileStru+1 , 2] = 'N'
laFileStru[lnFileStru+1 , 3] = 5
laFileStru[lnFileStru+1 , 4] = 2
*:B#803095,1 SSH 19/03/00 Print NotePad
laFileStru[lnFileStru+2 , 1] = 'PrtNot'
laFileStru[lnFileStru+2 , 2] = 'L'
laFileStru[lnFileStru+2 , 3] = 1
laFileStru[lnFileStru+2 , 4] = 1
*:B#803095,1 SSH (END)
CREATE TABLE (gcWorkDir+lcRpTmp) FROM ARRAY laFileStru

*-B603866 Ramy [start]
IF lcRpPrnNot = "Y"
  llPrnNot = .T.
ELSE
  llPrnNot = .F.
ENDIF
*-B603866 Ramy [end]
SELECT POSHDR
*-- Loop for inserting data in the temp file
SCAN FOR &lcRpExp
  SELECT POSLN
  IF SEEK(POSHDR.cStyType + POSHDR.PO)

    SCAN WHILE PO = POSHDR.PO  .AND. TRANCD = '1' && Looking for the PO# in POSLN file
      SCATTER MEMVAR MEMO
      m.nStyWeight = STYLE.nStyWeight
      *:B#803095,1 SSH 19/03/00 Print NotePad
      m.PrtNot = .F.
      *:B#803095,1 SSH (END)
      INSERT INTO (lcRpTmp) FROM MEMVAR
    ENDSCAN  && SCAN FOR POSHDR.PO = POSLN.PO
    *:B#803095,1 SSH 19/03/00 Print NotePad
    SELECT (lcRpTmp)
    REPLACE PrtNot WITH .T.
    *:B#803095,1 SSH (END)
  ENDIF   && IF SEEK(POSHDR.cStyType......
ENDSCAN  && SCAN FOR CSTYTYPE='D' .AND. POSLN.PO .AND. &lcRpExp
SELECT POSLN
SET RELATION TO

*--- Set some relation.
SELECT (lcRpTmp)
SET RELATION TO cStyType + PO INTO POSHDR
SET RELATION TO 'S' + Scale INTO SCALE      ADDITIVE
SET RELATION TO Vendor      INTO APVENDOR   ADDITIVE
SET RELATION TO Style       INTO STYLE      ADDITIVE
*:B#803095,1 SSH 19/03/00 Print NotePad
*-B603866 Ramy [start]
IF llPrnNot
*-B603866 Ramy [end]
  SET RELATION TO 'P'+ PO   INTO notepad      ADDITIVE
  GOTO TOP
*-B603866 Ramy [start]
ENDIF
*-B603866 Ramy [end]
*:B#803095,1 SSH (END)
*-- Call the FRX
DO gfDispRe WITH EVAL('lcForm')

*-- Close tmp file
SELECT (lcRpTmp)
SET RELATION TO
USE
IF FILE(gcWorkDir+lcRpTmp)
  ERASE (gcWorkDir + lcRpTmp + '.DBF')
  ERASE (gcWorkDir + lcRpTmp + '.CDX')
  ERASE (gcWorkDir + lcRpTmp + '.FPT')
ENDIF

*!****************************************************************************
*! Name      : lfwOldVal
*! Developer : RAMY MABROUK (RAMY)
*! Date      : 10/17/1999
*! Purpose   : To get the old value of the field
*!****************************************************************************
*! Calls       : None.
*!****************************************************************************
*! Passed Parameters : None.
*!****************************************************************************
*! Return      : None
*!****************************************************************************
*! Example     : = lfwOldVal()
*!****************************************************************************

FUNCTION lfwOldVal

*-- Save the Old Value
lcOldVal = EVALUATE(SYS(18))


*!****************************************************************************
*! Name      : lfvVend
*! Developer : RAMY MABROUK (RAMY)
*! Date      : 10/17/1999
*! Purpose   : Valid function of the Vendor field.
*!****************************************************************************
*! Calls       : gfApVnBrow().
*!****************************************************************************
*! Passed Parameters : None.
*!****************************************************************************
*! Return      : None
*!****************************************************************************
*! Example     : = lfvVend()
*!****************************************************************************

FUNCTION lfvVend

PRIVATE lcVar, lcObj
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
SELECT APVENDOR
SET ORDER TO TAG VenCode

*-- IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))

  * Browse the vendors file
  =gfApVnBrow(@lcObj)

  * Check if we well update the field or not
  IF !EMPTY(lcObj)
    &lcVar = lcObj      && Update the field
  ELSE    && Else of IF !EMPTY(lcObj)
    &lcVar = lcOldVal
  ENDIF    && End of IF !EMPTY(lcObj)
ENDIF    && End of IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj...
SELECT APVENDOR
SET ORDER TO

*--End of FUNCTION lfvVend

*!****************************************************************************
*! Name      : lfGetClr
*! Developer : RAMY MABROUK (RAMY)
*! Date      : 10/17/1999
*! Purpose   : To get the Color code + the color description
*!****************************************************************************
*! Calls       : gfCodDes()
*!****************************************************************************
*! Passed Parameters : None.
*!****************************************************************************
*! Return      : lcStyClr
*!****************************************************************************
*! Example     : = lfGetClr()
*!****************************************************************************

FUNCTION lfGetClr

lcClr = IIF(EMPTY(POSHDR.Color), 'ALL' , gfCodDes(POSHDR.Color , 'COLOR'))
lcStyClr = POSHDR.Color + '-' + lcClr

RETURN lcStyClr

*--End of FUNCTION lfGetClr

*!****************************************************************************
*! Name      : lfVenAdr
*! Developer : RAMY MABROUK (RAMY)
*! Date      : 10/17/1999
*! Purpose   : Get the vendor addresses
*!****************************************************************************
*! Calls       : gfCodDes()
*!****************************************************************************
*! Passed Parameters : None.
*!****************************************************************************
*! Return      : Array holds the vendor address
*!****************************************************************************
*! Example     : = lfVenAdr()
*!****************************************************************************

FUNCTION lfVenAdr
*--- Get the vendor addr.
*WAIT WINDOW VENDOR + '+' + &lcRpTmp..Vendor+ ' BEFOR'
IF SEEK(&lcRpTmp..Vendor,'APVENDOR')
*WAIT WINDOW VENDOR + '+' + &lcRpTmp..Vendor+ ' AFTER'
laVenAdr[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
laVenAdr[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
laVenAdr[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
laVenAdr[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
laVenAdr[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
ENDIF

*--End of FUNCTION lfVenAdr


*!****************************************************************************
*! Name      : lfRelClr
*! Developer : RAMY MABROUK (RAMY)
*! Date      : 10/17/1999
*! Purpose   : To get the related color field value
*!****************************************************************************
*! Calls       : gfRltFld()
*!****************************************************************************
*! Passed Parameters : None.
*!****************************************************************************
*! Return      : lcRelClr
*!****************************************************************************
*! Example     : = lfRelClr()
*!****************************************************************************

FUNCTION lfRelClr

*lcClr = IIF(EMPTY(POSHDR.Color), 'ALL' , gfCodDes(POSHDR.Color , 'COLOR'))

lcRelClr = ''

DECLARE laClrRel[1,2]
laClrRel[1,1] = 'CLRLNAME'
laClrRel[1,2] = 'lcRelClr'
= gfRltFld(POSHDR.Color , @laClrRel,'COLOR')

RETURN lcRelClr

*--End of FUNCTION lfRelClr

*!****************************************************************************
*! Name      : lfSetDye
*! Developer : Khalid Mohi El-din
*! Date      : 01/21/2001
*! Purpose   : To set the cursor at the top of the browse.
*!****************************************************************************
*! Example     : = lfSetDye()
*!****************************************************************************
*! B605399,1 KHM 01/21/2001 Adding this function in the SYREPUVR.
*!****************************************************************************
FUNCTION lfSetDye
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT POSHDR
    llClear = .F.
    GOTO TOP
ENDCASE
