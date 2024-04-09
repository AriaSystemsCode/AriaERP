*:***************************************************************************
*: Program file  : ARLETER
*: Program desc. : Customer / Vendor Letters
*: For screen    : ARLETER.SCX
*:        System : Aria Advantage Series.
*:        Module : Accounts Receiviable/Payable (AR/AP)
*:        Date   : 10/30/1999
*:     Developer : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Parameters    : Detect calling Module ("C" for customer AR module
*:               :                        "V" for vendor   AP module)
*:***************************************************************************
*: Example : DO ARLETER -- E301119,1 Customer/Vendor Letters
*:***************************************************************************
*: Modifications :
*:B603658,1 MHM 06/27/2000 Change set as default a confirmation message .
*:***************************************************************************
PARAMETERS lcFromMod
EXTERNAL ARRAY laData,laKeyField
DECLARE laKeyField [2,4]
 
laKeyField[1,1] = 'laData[6]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'LETTERTO'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[1]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'LETTERTO'
laKeyField[2,4] = 2

lnLetrType = 0
STORE .F. TO llAsDefa , llHaveDefa , llBrowse
lcHaveDefa = ''  && Have defa Id + Description.

*-- This is to prevent the global save (i.e. Do lpSavScr)
laDefProc[9] = .F.                 
 
IF !gfSetup()    
  RETURN
ENDIF  

SELECT LETTERS
SET FILTER TO cLeterTo = lcFromMod

IF !WEXIST(gcBaseWind)
  lcScFields = "cletterid,cletshdes,mletrbody,cletertype,casdefault,cleterto,cletermsg"
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laData[6] = lcFromMod
ENDIF
 
DO (gcScrDir+"ARLETER.SPX")
*-- end of program code.
 
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
PRIVATE lnI
laData[6]  = lcFromMod
llHaveDefa = .F.
DO CASE
  CASE laScrMode[1]
    SELECT LETTERS
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK
    SET ORDER TO Cletterid IN LETTERS
    lnLetrType = 0
    llAsDefa   = .F.
    
  CASE laScrMode[2]
    
    SELECT LETTERS
    SCATTER FIELDS &lcScFields MEMO TO laData
    lnI = 0
    FOR lnI = 1 TO ALEN(laLetrType,1)
      IF laData[4] = LEFT(laLetrType[lnI],1)
        lnLetrType = lnI
        EXIT
      ENDIF
    ENDFOR
    llAsDefa = (laData[5]="D")
  
  CASE laScrMode[3] OR laScrMode[4]
    
    IF laScrMode[4]
      laData[4]  = LEFT(laLetrType[1],1)
    ENDIF
    
    SELECT LETTERS
    lcOldOrder = ORDER()
    lcOldRec   = EVALUATE(KEY())
    SET ORDER TO Leterdefa

    llHaveDefa = SEEK("D"+laData[6]+laData[4])  && This type has a default value.
    lcHaveDefa = IIF(llHaveDefa, cletterid + " - " + cletshdes,"")
    IF laScrMode[4]
      lnLetrType = 1
      llAsDefa   = !llHaveDefa
      laData[5] = IIF(llAsDefa,"D"," ")
    ENDIF  

    SET ORDER TO &lcOldOrder IN LETTERS
    =SEEK(lcOldRec,"LETTERS")
ENDCASE
*-- end of lpShow.

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1
IF !MDOWN()
  PRIVATE lnCurrAls
  lnCurrAls = SELECT(0)
  laData[6] = lcFromMod

  *-- If come from BitMap button or the key is found
  IF llBrowse OR ("?" $ laData[1])
    llBrowse  = .F. 		                && Disable flag to its intial state   
  
    laData[1] = SPACE(10)
    ACTIVATE WINDOW gwcContrl1		&& Activate control panel window
    _CUROBJ = OBJNUM(pbBrws)
    KEYBOARD CHR(13) CLEAR PLAIN

  ENDIF							        && Ending seek condition

  = !EMPTY(laData[1]) AND gfSeekRec()

  SELECT (lnCurrAls)
ENDIF  
*-- end of lfvData_1.

*!**************************************************************************
*!
*!      Function: lfvLetrTyp
*!
*!**************************************************************************
*
FUNCTION lfvLetrTyp
IF laData[4] != LEFT(laLetrType[lnLetrType],1)
  laData[4]  = LEFT(laLetrType[lnLetrType],1)

  SELECT LETTERS
  lcOldOrder = ORDER()
  lcOldRec   = EVALUATE(KEY())
  SET ORDER TO Leterdefa

  llHaveDefa = SEEK("D"+laData[6]+laData[4])  && This type has a default value.

*:B603658,1 MHM 06/27/2000 Change set as default a confirmation message  [Begin]
  lcHaveDefa = IIF(llHaveDefa, cletterid + " - " + cletshdes,"")
*  lcHaveDefa = IIF(llHaveDefa, laData[1] + " - " + laData[1],"")
*:B603658,1 MHM 06/27/2000 [End]

  SET ORDER TO &lcOldOrder IN LETTERS
  =SEEK(lcOldRec,"LETTERS")

  =lfvAsDefa('ANY')
ENDIF
*-- end of lfvLetrTyp.

*!**************************************************************************
*!
*!      Function: lfvAsDefa
*!
*!**************************************************************************
*
FUNCTION lfvAsDefa
PARAMETERS lcNormDefa
IF !EMPTY(lcNormDefa)
  llAsDefa = !llHaveDefa
  SHOW GET llAsDefa
ENDIF
IF llAsDefa AND llHaveDefa
  *Message : 40166 => XXX Letter YYY was set as default, would want to set active letter as default?
  *Button  : <Yes> <No>
  
  *:B603658,1 MHM 06/27/2000 Change set as default a confirmation message [Begin]
  *--IF gfModalGen('QRM40166B40000','Dialog',laLetrType[lnLetrType]+"|("+ALLTRIM(lcHaveDefa)+")") = 2
  IF gfModalGen('QRM40166B40000','Dialog',"("+ALLTRIM(lcHaveDefa)+")|") = 2
  *:B603658,1 MHM 06/27/2000 [End]
  
    llAsDefa = .F.
    SHOW GET llAsDefa
  ENDIF
ENDIF
laData[5] = IIF(llAsDefa,"D"," ")
*-- end of lfvAsDefa.

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr
SELECT LETTERS
lcOldOrder = ORDER()
lcOldRec   = EVALUATE(KEY())
SET ORDER TO Leterdefa

*-- Cancel old Default type because current on is the default one.
IF llAsDefa AND llHaveDefa
  *-- Old default for this type.
  SET ORDER TO Leterdefa IN LETTERS
  IF SEEK("D"+laData[6]+laData[4],"LETTERS")
    SELECT LETTERS
    =RLOCK()
    REPLACE cAsDefault WITH " "
    UNLOCK
  ENDIF
ENDIF
SET ORDER TO LetterTo IN LETTERS

SELECT LETTERS
*-- if it was a new record.
IF !SEEK(laData[6]+laData[1],"LETTERS")
  APPEND BLANK
ENDIF
=RLOCK()
REPLACE cletterid  WITH laData[1],;
        cletshdes  WITH laData[2],;
        mletrbody  WITH laData[3],;
        cletertype WITH laData[4],;
        casdefault WITH laData[5],;
        cleterto   WITH laData[6],;
        cletermsg  WITH laData[7]
UNLOCK

SET ORDER TO &lcOldOrder IN LETTERS
=SEEK(lcOldRec,"LETTERS")
*-- end of lpSavScr.
