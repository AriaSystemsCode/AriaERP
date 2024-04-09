*:***************************************************************************
*: Program file  : ICSTYCDN.PRG
*: Program desc. : ASSIGN CODED NOTES FOR COLLAGE LTD.
*: Date          : 03/27/2002
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC).
*: Developer     : Ahmed Maher (AMH)
*: Tracking Job Number: C200307
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICSTYCDN
*:***************************************************************************
*: Modifications :
*:***************************************************************************

*-- Initializing the necessary variables.
lcWinLbl1  = gfTempName()
lcWinLbl2  = gfTempName()
lcNoteBrow = 'Style Coded Notes'

IF laScrMode[2]
  STORE 'DISABLE' TO lcDIS_EN
  STORE 'DISABLE' TO lcDIS_ENREM
ELSE
  STORE 'ENABLE' TO lcDIS_EN
  STORE 'ENABLE' TO lcDIS_ENREM
ENDIF

PRIVATE lnAlias,llRecFound
llRecFound = .F.
lnAlias = SELECT(0)
= gfOpenFile(gcDataDir+'STYLENOT',gcDataDir+'STYLENOT','SH')
SELECT (lcTmpStyNt)
SCAN
  = SEEK(CODE,'STYLENOT')
  REPLACE CNOTE WITH STYLENOT.CNOTE
  IF CFLAG # 'D'
    llRecFound = .T.
  ENDIF
ENDSCAN
LOCATE
IF !laScrMode[2]
  IF !llRecFound
    STORE 'DISABLE' TO lcDIS_ENREM
  ELSE
    STORE 'ENABLE' TO lcDIS_ENREM
  ENDIF
ENDIF
SELECT (lnAlias)

lcHldEsc = ON('KEY','ESC')
ON KEY LABEL ESC DO lfvClose

DO (gcScrDir+gcWinAppl+"\ICSTYCDN.SPX")

ON KEY LABEL ESC     &lcHldEsc
USE IN STYLENOT

*!*************************************************************
*! Name      : lfvClose
*: Date      : 03/27/2002
*: Developer : Ahmed Maher (AMH)
*! Purpose   : Valid function of the Close button.
*!*************************************************************
*! Example            : =lfvClose()
*!*************************************************************
*
FUNCTION lfvClose

glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
ACTI WINDOW (lcWinLbl2) TOP
CLEAR READ
*-- end of lfvClose.

*:----------------------------------------------------------------
*: Name       : lfvCode
*: Developer  : AHMED MAHER (AMH)
*: Date       : 03/28/2002
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
DIMENSION laTempData[1]
STORE '' TO laTempData
lcOldBrwFl = lcBrFields

lcFile_Ttl = 'Style coded notes'

lcBrFields = "CODE  :H='Code',"+;
             "CNOTE :H='Notes'"

IF llBrowse OR (!EMPTY(lcCode) AND !SEEK(lcCode,'STYLENOT'))
  llBrowse  = .F.
  SELECT STYLENOT
  =AriaBrow([],lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CODE','laTempData')
  lcCode = laTempData[1]
ENDIF
lcBrFields = lcOldBrwFl
IF !EMPTY(lcCode)
  IF SEEK(PADR(lcMajor,19)+lcCode,lcTmpStyNt)
    IF EVALUATE(lcTmpStyNt+'.CFLAG') # 'D'
      =gfModalGen("INM000000B00000","DIALOG",'','',"The code : "+ALLTRIM(lcCode)+" already exist.")
      lcCode = SPACE(10)
      _CUROBJ = _CUROBJ
    ELSE
      SELECT (lcTmpStyNt)
      REPLACE CFLAG WITH 'A'
      lcCode = SPACE(10)
      SHOW GET lcCode DISABLE
      SHOW GET ibCode DISABLE
      SHOW GET pbRem  ENABLE
      =lfwBrows()
      llCUpDate = .T.
    ENDIF
  ELSE
    IF SEEK(lcCode,'STYLENOT')
      SELECT (lcTmpStyNt)
      APPEND BLANK
      REPLACE CODE      WITH lccode,;
              CNOTE     WITH STYLENOT.CNOTE,;
              CSTYMAJOR WITH lcMajor,;
              CFLAG     WITH 'A'
      lcCode = SPACE(10)
      SHOW GET lcCode DISABLE
      SHOW GET ibCode DISABLE
      SHOW GET pbRem  ENABLE
      =lfwBrows()
      llCUpDate = .T.
    ENDIF
  ENDIF
ENDIF
SELECT(lnAlias)
*-- end of lfvCode.

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : AMH (AHMED MAHER)
*! Date      : 03/28/2002
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcNoteBrow) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B DO lfDispBrow
ENDIF    && End of IF
*-- end of lfBrwUnTrp.

*!*************************************************************
*! Name      : lfTraps
*! Developer : AMH (AHMED MAHER)
*! Date      : 03/28/2002
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfTraps
PARAMETERS lcTrap
*-- Tab Case
ACTI WINDOW (lcWinLbl2) TOP
IF lcTrap = "TAB"
  _CUROBJ = OBJNUM(lcCode)
ELSE  && Backtab Case.
  _CUROBJ = OBJNUM(pbClose)
ENDIF
*-- end of lfTraps.

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : AMH (AHMED MAHER)
*! Date      : 03/28/2002
*! Purpose   : Function to trap the keys for the Browse , 
*!             and save the changes if the current record was edited
*!*************************************************************
*
FUNCTION lfBrowTrap
*-- IF The window on top is the Browse
IF WONTOP(lcNoteBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH "TAB"
  ON KEY LABEL BACKTAB DO lfTraps WITH "BACKTAB"
  ON KEY LABEL ALT+B 

ENDIF    && End of IF
*-- end of lfBrowTrap.

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 03/28/2002
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispBrow

SELECT (lcTmpStyNt)
lcBrowFild = "CODE  :H = 'Code' :15 :R,"+;
             "CNOTE :H = 'Note' :95 :R"
BROWSE FIELDS &lcBrowFild;
       WINDOW (lcWinLbl1);
       WHEN lfwBrows()   ;
       IN WINDOW ICSTYCDN;
       FOR CFLAG # 'D'   ;
       LOCK 0            ;
       NOAPPEND          ;
       NOCLEAR           ;
       NODELETE          ;
       NOWAIT            ;
       NOMENU            ;
       SAVE              ;
       TITLE lcNoteBrow
*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AMH (AHMED MAHER)
*! Date      : 03/28/2002
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwBrows

lcCode = CODE
SHOW GET lcCode
SHOW WINDOW (lcNoteBrow) REFRESH SAME
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvNew
*! Developer : AMH (AHMED MAHER)
*! Date      : 03/31/2002
*! Purpose   : Valid function of new botton
*!*************************************************************
*
FUNCTION lfvNew

lcCode = SPACE(0)
SHOW GET lcCode ENABLE
SHOW GET ibCode ENABLE
_CUROBJ = OBJNUM(lcCode)
*-- end of lfvNew.

*!*************************************************************
*! Name      : lfvRem
*! Developer : AMH (AHMED MAHER)
*! Date      : 03/31/2002
*! Purpose   : Valid function of Remove botton
*!*************************************************************
*
FUNCTION lfvRem

IF gfModalGen("INM00000B42002","DIALOG",'','',"Are you sure you want to remove this record?") = 1
  PRIVATE lnAlias,lnRecCnt
  lnAlias = SELECT(0)
  SELECT (lcTmpStyNt)
  REPLACE CFLAG WITH 'D'
  COUNT FOR CFLAG # 'D' TO lnRecCnt
  LOCATE FOR CFLAG # 'D'
  IF lnRecCnt = 0
    SHOW GET pbRem DISABLE
  ENDIF
  llCUpDate = .T.
  =lfwBrows()
  SELECT (lnAlias)
ENDIF
*-- end of lfvRem.