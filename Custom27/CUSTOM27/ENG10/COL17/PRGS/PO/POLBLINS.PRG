*:***************************************************************************
*: Program file  : POLBLINS.PRG
*: Program desc. : LABELLING INSTRUCTIONS FOR COLLAGE LTD.
*: Date          : 03/18/2002
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Order - (PO)
*: Developer     : Ahmed Maher (AMH)
*: Tracking Job Number: C200305
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POLBLINS
*:***************************************************************************
*: Modifications :
*:***************************************************************************

*-- Initializing the necessary variables.
lcWinLbl1 = gfTempName()
lcWinLbl2 = gfTempName()
lcDown_Up = 'D'
lnCurObj = 0

IF laScrMode[2]
  STORE 'DISABLE' TO lcDIS_EN
ELSE
  STORE 'ENABLE' TO lcDIS_EN
ENDIF

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpLblIn)
SCATTER MEMO MEMVAR
SELECT (lnAlias)

lcHldEsc = ON('KEY','ESC')
ON KEY LABEL ESC DO lfvCancel

DO (gcScrDir+gcWinAppl+"\POLBLINS.SPX")

ON KEY LABEL ESC     &lcHldEsc

*!*************************************************************
*! Name      : lfvOk
*: Date      : 03/18/2002
*: Developer : Ahmed Maher (AMH)
*! Purpose   : Valid function of the Ok button.
*!*************************************************************
*! Example            : =lfvOk()
*!*************************************************************
*
FUNCTION lfvOk

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpLblIn)
GATHER MEMO MEMVAR
SELECT (lnAlias)
=lfvCancel()
*-- end of lfvOk.

*!*************************************************************
*! Name      : lfvCancel
*: Date      : 03/18/2002
*: Developer : Ahmed Maher (AMH)
*! Purpose   : Valid function of the Cancel button.
*!*************************************************************
*! Example            : =lfvCancel()
*!*************************************************************
*
FUNCTION lfvCancel

CLEAR READ
*-- end of lfvCancel.

*!*************************************************************
*! Name      : lfwShowLbl
*: Date      : 03/18/2002
*: Developer : Ahmed Maher (AMH)
*! Purpose   : When function of the screen.
*!*************************************************************
*! Example            : =lfwShowLbl()
*!*************************************************************
*
FUNCTION lfwShowLbl

SHOW WINDOW (lcWinLbl1) TOP
SHOW GETS WINDOW (lcWinLbl2) DISABLE ONLY
HIDE WINDOW (lcWinLbl2)
*-- end of lfwShowLbl.

*!*************************************************************
*! Name      : lfvDown_Up
*: Date      : 03/18/2002
*: Developer : Ahmed Maher (AMH)
*! Purpose   : Valid function of the Move Down/Up button.
*!*************************************************************
*! Example            : =lfvDown_Up()
*!*************************************************************
*
FUNCTION lfvDown_Up

IF lcDown_Up = 'D'
  IF !laScrMode[2]
    SHOW GETS WINDOW (lcWinLbl2) ENABLE ONLY
  ENDIF
  SHOW WINDOW (lcWinLbl2) TOP
  SHOW GETS WINDOW (lcWinLbl1) DISABLE ONLY
  HIDE WINDOW (lcWinLbl1)
  SHOW GET pbScrl,1 PROMPT lcCSUp
  lcDown_Up = 'U'
ELSE
  IF !laScrMode[2]
    SHOW GETS WINDOW (lcWinLbl1) ENABLE ONLY
  ENDIF
  SHOW WINDOW (lcWinLbl1) TOP
  SHOW GETS WINDOW (lcWinLbl2) DISABLE ONLY
  HIDE WINDOW (lcWinLbl2)
  SHOW GET pbScrl,1 PROMPT lcCSDn
  lcDown_Up = 'D'
ENDIF
DO CASE
  CASE lnCurObj > 0 .AND. lnCurObj < 9
    _CUROBJ = 17
  CASE lnCurObj > 8 .AND. lnCurObj < 17
    _CUROBJ = lnCurObj + 8
  CASE lnCurObj > 16 .AND. lnCurObj < 25
    _CUROBJ = lnCurObj - 8
  CASE lnCurObj > 24
    _CUROBJ = 1
ENDCASE
*-- end of lfvDown_Up.

*!*************************************************************
*! Name      : lfvObj
*: Date      : 03/18/2002
*: Developer : Ahmed Maher (AMH)
*! Purpose   : Valid function of all object fields.
*!*************************************************************
*! Example            : =lfvObj()
*!*************************************************************
*
FUNCTION lfvObj

lnCurObj = _CUROBJ
*-- end of lfvObj.