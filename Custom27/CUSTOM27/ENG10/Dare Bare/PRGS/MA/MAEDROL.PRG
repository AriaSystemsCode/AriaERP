*:****************************************************************
*: Program file  : MAFBSTK.PRG
*: Program desc. : Facility to edit material's rolls
*: System        : Aria Apparel System (A27).
*: Module        : Material (MA)
*: Developer     : Khalid Mohi El-Din Mohamed
*: Date          : 02/18/2003
*:****************************************************************
*:C#200473,1
*:****************************************************************
*: Modifications      :
*:B120271,1 ABD 11/30/2003 Fix bug that if we have 2 rolls and modify one 
*:B120271,1 ABD            and leave the other one, while saving a 
*:B120271,1 ABD            message appear saying rolls # XX (the second roll id) 
*:B120271,1 ABD            already exist in the roll file cannot proceed.
*:****************************************************************


EXTERNAL ARRAY laDefProc,laCtrStat,laScrMode
laDefProc[9]  = .F.               && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.               && Disable the control panel close proc. (lpClsScr)


STORE ''  TO lcWareHous, lcFabric, lcColor, lcBatch, lcPieces, lcLoc,;
             lcFBatch, lcFPieces,  lcFLoc, lcRoll1

STORE .F. TO llBrowse, llAddRec, llEditRec, llDeleRec
lcTmpRolls = gfTempName()
lcTmpRoll2 = gfTempName()
lcRoll1    = gfTempName()

lcBrTit    = "Rolls"
IF !gfSetup()
  RETURN
ENDIF

*-- To Create the temporary file.
=lfCreatTmp()

IF !USED(lcRoll1)
   =gfOpenFile(gcDataDir+'ROLLS','ROLLS','SH',@lcRoll1,.T.)
ENDIF


PUSH KEY
DO (gcScrDir + gcWinAppl + '\MAEDROLL.SPX')
POP KEY

*!**************************************************************************
*! Name      : lpShow
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/18/2003
*! Purpose   : To control the control the tool bar
*!**************************************************************************
*! Example   : DO lpShow
*!**************************************************************************
PROCEDURE lpShow

DO CASE
  *-- S E L E C T   M O D E -----------------------------------------------
  CASE laScrMode[1]  && Select mode. 
    SHOW GET pbBrws     DISABLE
    SHOW GET pbEdt      DISABLE
    SHOW GET PbWareH    ENABLE
    SHOW GET lcWareHous ENABLE
    SHOW GET pbSav      DISABLE
    SHOW GET pbSlct     DISABLE
    SHOW GET pbDlt      DISABLE

    STORE ''  TO lcFabric, lcColor, lcBatch, lcPieces, lcLoc,;
                 lcFBatch, lcFPieces,  lcFLoc
    IF USED(lcTmpRoll2) AND RECCOUNT(lcTmpRoll2) > 0
      SELECT (lcTmpRoll2)
      ZAP
    ENDIF

    IF USED (lcTmpRolls) AND RECCOUNT(lcTmpRolls) > 0
      SELECT (lcTmpRolls)
      ZAP
    ENDIF
     
     SHOW GET pbFilter   DISABLE
     SHOW GET PbFabric   DISABLE
     SHOW GET lcFabric   DISABLE
     SHOW GET PbColor    DISABLE
     SHOW GET lcColor    DISABLE
     SHOW GETS WINDOW MAEDROL3 DISABLE ONLY
     =lfBrows()
    _CUROBJ = OBJNUM(lcWareHous)    

  *-- V I E W   M O D E -------------------------------------------------
  CASE laScrMode[2]  && View mode.
    SHOW GET pbEdt  ENABLE
    SHOW GET pbSlct   ENABLE
    SHOW GET pbDlt    DISABLE
    SHOW GET pbFilter ENABLE

    SHOW GET ibTabs ENABLE
    _CUROBJ = OBJNUM(ibTabs)

        
  *-- E D I T   M O D E -------------------------------------------------
  CASE laScrMode[3]  && Edit mode. 
    llcUpdate = .T.
    SHOW GET PbWareH    DISABLE
    SHOW GET lcWareHous DISABLE
    SHOW GET PbFabric   DISABLE
    SHOW GET lcFabric   DISABLE
    SHOW GET PbColor    DISABLE
    SHOW GET lcColor    DISABLE

    SHOW GET pbFilter  DISABLE
    SHOW GET lcFBatch  DISABLE
    SHOW GET lcFPieces DISABLE
    SHOW GET lcFLoc    DISABLE
    SHOW GET pbSlct   DISABLE

    _CUROBJ = OBJNUM(lcFabric)

    
ENDCASE

*!**************************************************************************
*! Name      : lfBrows
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/18/2003
*! Purpose   : To browse the fields
*!**************************************************************************
*! Example   : =lfBrows()
*!**************************************************************************
FUNCTION lfBrows
PRIVATE lnAlias

lnAlias = SELECT()

SELECT (lcTmpRolls)
GOTO TOP
BROWSE FIELDS ;
     lcDummy = ALLTRIM(cBatch)+'-'+ALLTRIM(cPieces)+'-'+ALLTRIM(cLoc):30:H= 'Roll Id' :R,;
     nQtyBal  :H   = 'Quantity':R;
     WINDOW    MAEDROL2  ;
     IN WINDOW (gcBaseWind);
     WHEN lfwBrow()    ;
     NOMENU            ;         
     NOAPPEND          ;
     NODELETE          ;         
     NOWAIT            ;
     SAVE              ;
     NOCLEAR           ;
     NOEDIT            ;
     LOCK 0            ;
     TITLE lcBrTit

SHOW WINDOW (lcBrTit) REFRESH SAME
SELECT (lnAlias)
	
*!**************************************************************************
*! Name      : lfwBrow
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/18/2003
*! Purpose   : To update the objects 
*!**************************************************************************
*! Example   : =lfwBrow()
*!**************************************************************************
FUNCTION lfwBrow

SELECT (lcTmpRolls)
lcBatch  = cBatch
lcPieces = cPieces
lcLoc    = cLoc

SHOW GET lcBatch
SHOW GET lcPieces
SHOW GET lcLoc


*!**************************************************************************
*! Name      : lfCreatTmp
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/18/2003
*! Purpose   : To create the temporary file.
*!**************************************************************************
*! Example   : =lfCreatTmp()
*!**************************************************************************
FUNCTION lfCreatTmp


CREATE CURSOR (lcTmpRoll2) (cBatch C(8), cPieces C(6), cLoc C(4),cOldRollId C(20),;
                            nQtyBal N(12,3), cStatus C(1))

*B120271,1 ABD - Add new field as a falge to know which recored was modified. [Begin]
*CREATE CURSOR (lcTmpRolls) (cBatch C(8), cPieces C(6), cLoc C(4),cOldRollId C(20),;
*                            nQtyBal N(12,3), cStatus C(1))
CREATE CURSOR (lcTmpRolls) (cBatch C(8), cPieces C(6), cLoc C(4),cOldRollId C(20),;
                            nQtyBal N(12,3), cStatus C(1) , cFlage C(1))
*B120271,1 ABD - [End]

*!**************************************************************************
*! Name      : lfvWareH
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/18/2003
*! Purpose   : To validate the selected ware house.
*!**************************************************************************
*! Example   : =lfvWareH()
*!**************************************************************************
FUNCTION lfvWareH

IF llBrowse OR (!EMPTY(lcWareHous) AND !SEEK(lcWareHous,'WAREHOUS'))
 lcWareHous = gfBrowWare( .F.,.F.,.F.,.F.,.F.,'M' )
 llBrowse = .F.
ENDIF

IF !EMPTY(lcWareHous)
  SHOW GET PbFabric ENABLE
  SHOW GET lcFabric ENABLE
  SHOW GET PbColor  ENABLE
  SHOW GET lcColor  ENABLE
ENDIF

*!**************************************************************************
*! Name      : lfvFabric
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/18/2003
*! Purpose   : To validate the selected Fabric
*!**************************************************************************
*! Example   : =lfvFabric()
*!**************************************************************************
FUNCTION lfvFabric

IF llBrowse OR (!EMPTY(lcFabric) AND !SEEK(lcFabric, 'Fabric'))
  DO FaBrow WITH lcFabric,'*'
  llBrowse = .F.
ENDIF

*!**************************************************************************
*! Name      : lfvColor
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/18/2003
*! Purpose   : To validate the selected Color
*!**************************************************************************
*! Example   : =lfvColor()
*!**************************************************************************
FUNCTION lfvColor

IF llBrowse OR (!EMPTY(lcColor) AND !SEEK(lcFabric+lcColor,'Fabric'))
  lcColor  = IIF(EMPTY(lcFabric),PADR(lcColor,6),CHR(240))
  DO FaBrow WITH lcFabric,lcColor
  llBrowse = .F.
  IF EMPTY(lcColor)
    _CUROBJ = OBJNUM(lcColor)
    RETURN
  ENDIF
ENDIF

IF !EMPTY(lcWareHous) AND !EMPTY(lcFabric) AND !EMPTY(lcColor)
  SHOW GET PbWareH    DISABLE
  SHOW GET lcWareHous DISABLE
  SHOW GET PbFabric   DISABLE
  SHOW GET lcFabric   DISABLE
  SHOW GET PbColor    DISABLE
  SHOW GET lcColor    DISABLE
  =lfvGetInfo()
ENDIF

*!*************************************************************
*! Name      : lfvGetInfo
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : The READ Activate function of NAP800
*!*************************************************************
*! Example            :  lfvGetInfo()
*!*************************************************************
FUNCTION lfvGetInfo

SELECT Rolls
IF SEEK(lcFabric+lcColor+lcWareHous)
  SCAN REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRsession = ;
                  lcFabric+lcColor+lcWareHous FOR TranCd = '1' AND nQtyBal > 0

    lcBatch  = SUBSTR(cRollId,1,AT("-",crollid,1)-1)
    lcPieces = SUBSTR(cRollId,AT("-",cRollId,1)+1)
    lcPieces = SUBSTR(lcPieces,1,AT("-",lcPieces,1)-1)
    lcLoc    = SUBSTR(cRollId,AT("-",cRollId,2)+1)

    INSERT INTO (lcTmpRolls) (cBatch, cPieces, cLoc, cOldRollId, nQtyBal, cStatus) ;
                     VALUES  (lcBatch, lcPieces, lcLoc, Rolls.crollid, Rolls.nQtyBal, "S")

  ENDSCAN                
                  
ENDIF

SELECT (lcTmpRolls)
LOCATE
IF EOF()
  =gfModalGen("QRM00000B00000",.F.,.F.,.F.,"There are no rolls for faric/colour "+;
             ALLTRIM(lcFabric)+"/"+ALLTRIM(lcColor)+" in warehouse "+lcWareHous+".")

  SHOW GET PbWareH    ENABLE
  SHOW GET lcWareHous ENABLE
  STORE "" TO lcFabric, lcColor
  SHOW GET PbFabric   DISABLE
  SHOW GET lcFabric   DISABLE
  SHOW GET PbColor    DISABLE
  SHOW GET lcColor    DISABLE
  _CUROBJ = OBJNUM(lcWareHous)
ELSE
  laScrMode    = .F.
  laScrMode[2] = .T.
  DO lpShow
ENDIF

*!*************************************************************
*! Name      : lpSavScr
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : To save the changed rolls.
*!*************************************************************
*! Example            :  lpSavScr()
*!*************************************************************
PROCEDURE lpSavScr

STORE .F. TO llCSave, llShow, llReject

SELECT (lcTmpRolls)

*B120271,1 ABD - Scan from modified records only. [Begin]
*SCAN
SCAN FOR cFlage = 'M'
  *B120271,1 ABD - [End]
  
  IF SEEK(lcFabric+lcColor+lcWareHous+SPACE(10)+cOldRollId, 'Rolls')
    IF ALLTRIM(cBatch)+'-'+ALLTRIM(cPieces)+'-'+ALLTRIM(cLoc) <> ALLTRIM(cOldRollId)
      lcNewRoll = ALLTRIM(&lcTmpRolls..cBatch)+'-'+ALLTRIM(&lcTmpRolls..cPieces)+;
                  '-'+ALLTRIM(&lcTmpRolls..cLoc) ;
      
      IF SEEK(lcNewRoll,lcRoll1)
        llReject = .T.
        =gfModalGen("QRM00000B00000",.F.,.F.,.F.,"Roll ID #: "+ALLTRIM(lcNewRoll)+;
                     " is already exist in the roll file. Cannot proceed")
        EXIT             
      ENDIF
    ENDIF  
  ENDIF
ENDSCAN
WAIT CLEAR

IF llReject
  RETURN
ENDIF

SELECT (lcTmpRolls)

*B120271,1 ABD - Scan from modified records only. [Begin]
*SCAN
SCAN FOR cFlage = 'M'
 *B120271,1 ABD - [End]

  WAIT WINDOW "Changing roll id #: " + cOldRollId NOWAIT
  IF SEEK(lcFabric+lcColor+lcWareHous+SPACE(10)+cOldRollId, 'Rolls')
    IF ALLTRIM(cBatch)+'-'+ALLTRIM(cPieces)+'-'+ALLTRIM(cLoc) <> ALLTRIM(cOldRollId)
      SELECT Rolls
      REPLACE cRollId WITH ALLTRIM(&lcTmpRolls..cBatch)+'-'+ALLTRIM(&lcTmpRolls..cPieces)+;
                                '-'+ALLTRIM(&lcTmpRolls..cLoc) ;
              FOR cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRSession =;
                    lcFabric+lcColor+lcWareHous+SPACE(10)+&lcTmpRolls..cOldRollId
                           
    ENDIF  
  ENDIF
ENDSCAN
WAIT CLEAR

IF llReject
  RETURN
ENDIF

*-- To Create the temporary file.
=lfCreatTmp()
STORE ''  TO lcFabric, lcColor, lcBatch, lcPieces, lcLoc,;
             lcFBatch, lcFPieces,  lcFLoc

SHOW GET PbWareH    ENABLE
SHOW GET lcWareHous ENABLE
SHOW GET PbFabric   DISABLE
SHOW GET lcFabric   DISABLE
SHOW GET PbColor    DISABLE
SHOW GET lcColor    DISABLE
SHOW GETS WINDOW MAEDROL3 DISABLE ONLY
laScrMode    = .F.
laScrMode[1] = .T.
DO lpShow
=lfBrows()

*!*************************************************************
*! Name      : lpClsScr
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : TO validate the cancel button
*!*************************************************************
*! Example            :  lpClsScr()
*!*************************************************************
PROCEDURE lpClsScr


*!*************************************************************
*! Name      : lfUpdFld
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : TO validate batch, pieces and loc fields
*!*************************************************************
*! Example            :  lfUpdFld()
*!*************************************************************
FUNCTION lfUpdFld
PARAMETERS lcField, lcValue
SELECT (lcTmpRolls)

*B120271,1 ABD - Update the flag field in case the update the field. [Begin]
IF EMPTY(cFlage) .AND. &lcField # &lcValue
  REPLACE cFlage WITH 'M'
ENDIF
*B120271,1 ABD - [End]

REPLACE &lcField WITH &lcValue
SHOW WINDOW (lcBrTit) REFRESH SAME


*!*************************************************************
*! Name      : lfvFilter
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : The valid function for the filter button
*!*************************************************************
*! Example            :  lfvFilter()
*!*************************************************************
FUNCTION lfvFilter

SHOW GET lcFBatch  ENABLE 
SHOW GET lcFPieces ENABLE 
SHOW GET lcFLoc    ENABLE 
_CUROBJ = OBJNUM(lcFBatch)


*!*************************************************************
*! Name      : lfvFBatch
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : TO filter the selected records according to info. 
*!             in the batch
*!*************************************************************
*! Example            :  lfvFBatch()
*!*************************************************************
FUNCTION lfvFldFltr
PARAMETERS lcValue, lcField

SELECT (lcTmpRolls)
IF !EMPTY(&lcValue)
  DELETE FOR !(&lcValue $ &lcField)
  SHOW WINDOW (lcBrTit) REFRESH SAME
ENDIF  

*!*************************************************************
*! Name      : lfReadAct
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : The READ Activate function of MAEDROLL
*!*************************************************************
*! Example            :  lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 


*!*************************************************************
*! Name      : lfvDeact
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : The READ Deactivate function of screen MAEDROLL
*!*************************************************************
*! Example            :  lfvDeact()
*!*************************************************************
FUNCTION lfvDeact

IF WONTOP() = lcBrTit
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  IF laScrMode[1]
    ON KEY LABEL TAB DO lpTrapKey WITH 'MAEDROL1', 'lcWareHous'
    ON KEY LABEL BACKTAB DO lpTrapKey WITH 'MAEDROL1', 'lcWareHous'
  ELSE
    ON KEY LABEL TAB DO lpTrapKey WITH 'MAEDROL3', 'lcBatch'
    ON KEY LABEL BACKTAB DO lpTrapKey WITH 'MAEDROL3', 'lcBatch'
  ENDIF
ENDIF

RETURN .F.

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : To handle the Trapping of keys
*!*************************************************************
*! Example            :  lpTrapKey()
*!*************************************************************
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
