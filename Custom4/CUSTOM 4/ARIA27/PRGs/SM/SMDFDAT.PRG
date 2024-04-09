*:************************************************************************
*:
*: Procedure file: SMDFDAT.PRG 
*:                 (Default data)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
*: Last modified :
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*:************************************************************************

EXTERNAL ARRAY laStruc,laLogphfld 
DECLARE laOldStrc[1,4],laNewStrc[1,4]

*** If there is data in the memo field, we have to restore ***
*** it before building the strucure of the file.
  
*** Make the strucure of this file in an array. ***
DECLARE laOldStrc[ALEN(laStruc,1),4]
SELECT SYDFIELD
FOR lnCount = 1 TO ALEN(laStruc,1)
  IF SEEK(laStruc[lnCount,1])
    laOldStrc[lnCount,1] = cFld_name
    laOldStrc[lnCount,2] = cData_typ
    laOldStrc[lnCount,3] = nFld_wdth
    laOldStrc[lnCount,4] = nFld_dec
  ENDIF
ENDFOR
    
*** Create temp. file with the old strucure. ***
CREATE DBF (gcWorkDir+lcOldDfTmp) FROM ARRAY laOldStrc
    
*** Put the data in a temp. text file. ***
COPY MEMO sydFiles.mFldfdata TO (gcWorkDir+lcMemoTxt+".TXT")

*** Append the old temp file with the old data. ***
SELECT (lcOldDfTmp)
APPEND FROM (gcWorkDir+lcMemoTxt+".TXT") TYPE SDF

*** Get the new srucure in a new temp. file. ***
*** Arrange the array hold the fields of this file ***
=gfPopArang("puField",@laLogPhFld)
*** Get the logical names of the struc. ***
DECLARE laPhFld [ALEN(laLogPhFld,1),1]
FOR lnCount = 1 TO ALEN(laLogPhFld,1)
  laPhFld [lnCount,1] = ALLTRIM(SUBSTR(laLogPhFld[lnCount,1],1,10))
ENDFOR

*** Create new array hold the file strucure ***
DECLARE laNewStrc[ALEN(laPhFld,1),4]
SELECT SYDFIELD
FOR lnCount = 1 TO ALEN(laPhFld,1)
  IF SEEK(laPhFld[lnCount,1])
    laNewStrc[lnCount,1] = cFld_name
    laNewStrc[lnCount,2] = cData_typ
    laNewStrc[lnCount,3] = nFld_wdth
    laNewStrc[lnCount,4] = nFld_dec
  ENDIF
ENDFOR

*** Create the new temp file that hold the new strucure ***
CREATE TABLE (gcWorkDir+lcNewDfTmp) FROM ARRAY laNewStrc
    
*** Put the old data in the new strucure.
SELECT (lcNewDfTmp)
APPEND FROM (gcWorkDir+lcOldDfTmp+".DBF")

lcTitlBrow = "Default data for this file"

=lfPushKeys()

*E300683,4 Call *.SPR from screens directory
* DO SMDFDAT.SPR 
DO (gcScrDir + gcWinAppl + '\SMDFDAT.SPR')
*E300683,4 end   
=lfKeyClean()

*!**************************************************************************
*!
*!      FUNCTION lfDispBrow
*!
*!**************************************************************************
*
FUNCTION lfDispBrow
PARAMETERS lcWait

lcWait = IIF(TYPE('lcWait')='C','','NOWAIT')

BROWSE  NOAPPEND ;
        NODELETE ;
        SAVE     ;
        NOMENU   ;
        NOCLEAR  ;
        LOCK 0   ;
        TITLE lcTitlBrow ;
        WINDOW SMDFDA1 IN WINDOW SMDFDA1;
        &lcWait

*!**************************************************************************
*!
*!      FUNCTION lfvNew
*!
*!**************************************************************************
*
FUNCTION lfvNew

APPEND BLANK
=lfDispBrow('WAIT')

*!**************************************************************************
*!
*!      FUNCTION lfvRem
*!
*!**************************************************************************
*
FUNCTION lfvRem

DELETE
=lfDispBrow('WAIT')

*!**************************************************************************
*!
*!      FUNCTION lfvOk
*!
*!**************************************************************************
*
FUNCTION lfvOk

*!**************************************************************************
*!
*!      FUNCTION lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel

*!**************************************************************************
*!
*!      Function: lfPushKeys
*!
*!**************************************************************************
*
FUNCTION lfPushKeys

PUSH KEY
ON KEY LABEL ESC        DO lfDataTrap
ON KEY LABEL TAB        DO lfDataTrap
ON KEY LABEL BACKTAB    DO lfDataTrap
ON KEY LABEL CTRL+W     lnDumi = 1
ON KEY LABEL CTRL+Q     lnDumi = 1
ON KEY LABEL CTRL+END   lnDumi = 1

*!**************************************************************************
*!
*!      Function: lfDataTrap
*!
*!**************************************************************************
*
*** The default data browse trap function. ***

FUNCTION lfDataTrap

DO CASE
  CASE LASTKEY() = 27              && ESC
     ACTIVATE WINDOW SMDFDA2
    _CUROBJ = OBJNUM(pbCancel)
    KEYBOARD "{ENTER}"
  CASE LASTKEY() = 9               && TAB
    IF WONTOP() = lcTitlBrow
      ACTIVATE WINDOW SMDFDA2
      _CUROBJ = OBJNUM(pbNew)
    ELSE
      IF _CUROBJ = OBJNUM(pbCancel)
        ACTIVATE WINDOW (lcTitlBrow)
      ELSE
        _CUROBJ = _CUROBJ + 1
      ENDIF
    ENDIF
  CASE LASTKEY() = 15              && SHIFT+TAB
    IF WONTOP() = lcTitlBrow
      ACTIVATE WINDOW SMDFDA2
      _CUROBJ = OBJNUM(pbCancel)
    ELSE
      IF _CUROBJ = OBJNUM(pbNew)
        ACTIVATE WINDOW (lcTitlBrow)
      ELSE
        _CUROBJ = _CUROBJ - 1
      ENDIF
    ENDIF
ENDCASE

*!**************************************************************************
*!
*!      Function: lfKeyClean
*!
*!**************************************************************************
*
*** The cleaning function for all the trapped keys. ***

FUNCTION lfKeyClean

ON KEY LABEL ESC
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+END
