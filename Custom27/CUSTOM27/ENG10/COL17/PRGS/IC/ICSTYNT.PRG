*:***************************************************************************
*: Program file  : ICSTYNT
*: Program desc. : Style coded notes for Collage
*: For screen    : ICSTYNT.SCX
*:        System : Aria Advantage Series.
*:        Module : Inventory Control (IC).
*:        Date   : 03/26/2002
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO ICSTYNT
*:***************************************************************************
*: Due to C200307,1
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*

EXTERNAL ARRAY laData , laKeyField , laDefProc

laDefProc[7]  = .F.               && Disable the control panel delete proc.(lpDelScr)
DECLARE laKeyField [1,4]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'STYLENOT'
laKeyField[1,4] = 1

llNoShow   = .F.           && Flag to make the screen call the PROCEDURE lpShow evry time it run
glFromBrow = .T.
llBrowse   = .F.

IF !gfSetup()
  RETURN
ENDIF
lcBaseFile = 'STYLENOT'
DECLARE laField_N[1]
laField_N[1] = 'CODE'
DECLARE laField_H[1]
laField_H[1] = 'Style coded note code'

IF !WEXIST(gcBaseWind)
  *-- Screen fields variable, to be used to build the laData array.
  lcScFields = "CODE,CNOTE"
  lcBrFields = lcScFields
  SCATTER FIELDS &lcScFields. TO laData BLANK
ENDIF

DO (gcScrDir+gcWinAppl+"\ICSTYNT.SPX")

*:----------------------------------------------------------------
*: Name       : lfvCode
*: Developer  : AHMED MAHER (AMH)
*: Date       : 03/26/2002
*: Purpose    : Code object valid function.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvCode()
*:----------------------------------------------------------------
FUNCTION lfvCode
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT STYLENOT
IF llBrowse
  laData[1] = '?'
  llBrowse  = .F.
ENDIF
IF !EMPTY(laData[1])
  = gfSeekRec()
  llCUpDate = .T.
ENDIF
SELECT(lnAlias)
*-- end of lfvCode.

*!**************************************************************************
*! Name      : lpShow
*! Developer : AMH (AHMED MAHER)
*! Date      : 03/27/2002
*! Purpose   : procedure to refresh screen objects.
*!**************************************************************************
*
PROCEDURE lpShow
 
EXTERNAL ARRAY laScrMode

DO CASE
  CASE laScrMode[1] .OR. laScrMode[2] && SELECT Mode and VIEW Mode
    IF laScrMode[2]
      laData[2] = STYLENOT.CNOTE
    ENDIF
    SHOW GET laData[2] DISABLE
  CASE laScrMode[3] .OR. laScrMode[4] && EDIT   Mode and ADD  Mode
    SHOW GET laData[2] ENABLE
ENDCASE

*-- end of lpShow.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/10/2002
*! Purpose   : Delete style coded note
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lpDelScr()
*!*************************************************************

PROCEDURE lpDelScr

IF SEEK(laData[1],'STYCDNOT')
  =gfModalGen("INM000000B00000","DIALOG",'','',"The code : "+ALLTRIM(laData[1])+;
              " is assigned to style : "+ALLTRIM(STYCDNOT.CSTYMAJOR)+". Cannot delete.")
  RETURN .F.
ENDIF
*-- end of lpDelScr.