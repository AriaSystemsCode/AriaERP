*:***************************************************************************
*: Program file  : MFPRCTFP.PRG
*: Program desc. : Print Cutting Tickets for Fresh Produce
*: For Report    : MFPRCTFP.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Developer     : ABDOU ELGENDI        - (ABD)
*: Reference     : C101931,1
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : lfGetNotes , lfLastRec , lfGetExp , lfCutExp , lfItmPos
*:               : lfvMop_ApV , lfUsrVldFn.
*:***************************************************************************
*: Passed Parameters  : None.
*:***************************************************************************
*:Modifications:
*C200137,1 TAK 10/10/2000
*C102542,1 KHM 01/28/2002 Adding the issued field and USED header to FRX.
*:***************************************************************************
*:
SELECT (lcMainF)
SET RELATION TO
IF USED (lcMainF2)
  USE IN (lcMainF2)
  ERASE(gcWorkDir+lcMainF2+'.DBF')
  ERASE(gcWorkDir+lcMainF2+'.CDX')
ENDIF
COPY TO (gcWorkDir+lcMainF2)
SELECT 0
USE (gcWorkDir+lcMainF2) EXCL
SELECT (lcMainF2)
INDEX ON CUTTKT+cWareCode+Style+Dyelot+NoteFlag TAG (lcMainF2)
*-- open file with anther Alias name.
 =gfOpenFile(gcDataDir+'CTKTBOM','','SH',@lcCtktBom,.T.)
**C200137,1
 =gfOpenFile(gcDataDir+'BOMCOST','Bomcstkt','SH')
 
SELECT (lcMainF)
GOTO TOP
*- delete all recored to void the problem in set skip to.

DELETE ALL FOR NoteFlag $ 'ST' 

SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'M'+cuttkt INTO Ctktbom  ADDITIVE
SET RELATION TO 'M'+cuttkt INTO MFGOPRHD ADDITIVE
SET RELATION TO CutTkt     INTO CUTTKTH  ADDITIVE  
SET SKIP TO Ctktbom


*-- End Of Option Program.
*!*************************************************************
*! Name      : lfGetNotes
*! Developer : ABDOU ELGENDI        - (ABD)
*! Date      : 08/24/2000
*! Purpose   : Get notes for style and cutting ticket.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : FRX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes
PARAMETERS lcParm
PRIVATE lnAlias
lnAlias = 0

lnAlias = SELECT (0)
IF lnLastRec = &lcMainF..LineNo
  STORE "" To lcNotes
  SELECT (lcMainF2)
  =SEEK(&lcMainF..CUTTKT)
  SCAN REST WHILE  CUTTKT+cWareCode+Style+Dyelot+NoteFlag = ;
          &lcMainF..CUTTKT
    DO CASE
      CASE NoteFlag ='S'
**C200137,1
*       lcNotes = 'Style Notes :'+CHR(13) + ALLTRIM(Notes) +CHR(13)+ REPLICATE("_",112)
        lcNotes = 'Style Notes :'+CHR(13) + ALLTRIM(Notes) +CHR(13)+ REPLICATE("_",86)
      CASE NoteFlag ='T'
        IF EMPTY(lcNotes)
          lcNotes = 'Cutting Ticket Notes :' +CHR(13) + ALLTRIM(Notes)
        ELSE
          lcNotes = lcNotes + CHR(13) + 'Cutting Ticket Notes :'+CHR(13)+ ALLTRIM(Notes)
        ENDIF
    ENDCASE    
  ENDSCAN
  *- Function to get the last line in the cost sheet.
    =lfLastRecd()
ENDIF
SELECT (lnAlias)
RETURN lcParm
*- End Of lfGetNotes.
*!*************************************************************
*! Name      : lfLastRec
*! Developer : ABDOU ELGENDI        - (ABD)
*! Date      : 08/24/2000
*! Purpose   : Get the last recored.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : FRX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfLastRec()
*!*************************************************************
*
FUNCTION lfLastRec
PARAMETERS lcParm
PRIVATE lcThAlias , lcCurrOrd , lcToSeekVl

lcThAlias = ALIAS()           && Save Current Alias.

SELECT (lcMainf2)
lcCurrOrd  = ORDER()
SET ORDER TO (lcMainf2) DESCENDING
=SEEK(&lcMainF..CUTTKT)
lnLastRec = &lcMainF2..LineNo  && Evaluate record Number of last record in detail lines.
SET ORDER TO (lcMainf2) ASCENDING

IF Cuttkth.status  $ 'OASC'
  lnLastRecd = lnLastRec
ENDIF  

SELECT (lcThAlias)             && Restore Alias.
RETURN lcParm
*- End Of lfLastRec.
*!*************************************************************
*! Name      : lfGetExp
*! Developer : ABDOU ELGENDI        - (ABD)
*! Date      : 08/24/2000
*! Purpose   : Get the Exp.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : FRX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetExp()
*!*************************************************************
*
FUNCTION lfGetExp
PARAMETERS lcParm

*- Remove the APVENDOR.CVENDCODE From lcRpExp .
IF "APVENDOR.CVENDCODE" $ lcRpExp
  = lfCutExp("APVENDOR.CVENDCODE")
ENDIF

*- Remove the MFGOPRHD.COPRCODE From lcRpExp .
IF "MFGOPRHD.COPRCODE"  $ lcRpExp
  =lfCutExp ("MFGOPRHD.COPRCODE")
ENDIF

RETURN lcParm
*- End Of lfGetExp
*!*************************************************************
*! Name      : lfCutExp
*! Developer : ABDOU ELGENDI        - (ABD)
*! Date      : 08/24/2000
*! Purpose   : Get the last recored.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : FRX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCutExp()
*!*************************************************************
*
FUNCTION lfCutExp
PARAMETERS lcParmtr
PRIVATE lcVrExp , lcBrowPos , lnVrPos
STORE 0 TO lnVrPos
lcVrExp = ""
lnVrPos   = lfItmPos(lcParmtr)

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1] , laBrTmpFlt[1]
=ACOPY(laOGFxFlt,laTempExpr)
=ACOPY(laBrFldFlt,laBrTmpFlt)

DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
STORE "" To laOGFxFlt , laBrFldFlt
=ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnVrPos,1),7)
  
*-- Convert numeric position to string
lcBrowPos = ALLTRIM(STR(lnVrPos))

FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
  IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
    =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
    EXIT
  ENDIF
ENDFOR

laBrFldFlt[1,5] = "laOGFxFlt:1"
  
*-- Generate InList expression for transaction Number.
lcVrExp = gfGenFlt('laOGFxFlt',.T.,.T.)

*-- Get the Exp. in Global variable to use later when collect Data.

IF "APVENDOR.CVENDCODE" $ lcVrExp 
  lcVenExp  = lcVrExp 
ELSE
  lcMopExp  = lcVrExp 
ENDIF
*- Replace the Inlist in Poshdr.po withe .T.
lcRpExp = STRTRAN(lcRpExp,lcVrExp,".T.")
  
*-- Restore original laOGFxFlt , laBrFldFlt
DIMENSION laOGFxFlt[1] , laBrFldFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)    && restore Original array
=ACOPY(laBrTmpFlt,laBrFldFlt)

*- End Of lfCutExp.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/22/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
*!*************************************************************
*! Name      : lfvMop_ApV
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 08/22/2000
*! Purpose   : Cheak for vendor and M.opration when collect data.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfvMop_ApV()
*!*************************************************************
*
FUNCTION lfvMop_ApV
PARAMETERS lcParm
PRIVATE lnAlias
lcParm = .F.

lnAlias = SELECT(0)

SELECT MFGOPRHD
=SEEK("M"+CUTTKTH.CutTkt)
SCAN REST WHILE cimtyp+ctktno+coprcode = "M"+CUTTKTH.CutTkt
  *-- Cheak if found the m. opration that user select in this cutting ticket 
  *-- Return and take this cutting ticket recored.
  IF !EMPTY (lcMopExp) 
    IF &lcMopExp
      lcParm = .T.
      EXIT    
    ENDIF
  ENDIF
  *-- Cheak if found the vendor that user select in this cutting ticket 
  *-- Return and take this cutting ticket recored.
  IF !EMPTY (lcVenExp)
    lcVenEXP = STRTRAN(lcVenEXP,"APVENDOR.CVENDCODE","MFGOPRHD.CCONTCODE")  
    IF &lcVenExp
      lcParm = .T.
      EXIT
    ENDIF
  ENDIF
ENDSCAN

SELECT (lnAlias)
RETURN  lcParm
*-- End OF lfvMop_ApV.
*!*************************************************************
*! Name      : lfLastRecd
*! Developer : ABDOU ELGENDI        - (ABD)
*! Date      : 08/24/2000
*! Purpose   : Get the last recored to print the style notes.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : FRX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfLastRecd()
*!*************************************************************
*
FUNCTION lfLastRecd
PRIVATE lcThAlias , lcCurrOrd , lcToSeekVl
lcThAlias = ALIAS()           && Save Current Alias.

IF Cuttkth.status  $ 'OASC'
  SELECT (lcCtktBom)
  SET ORDER TO TAG CTKTBOM DESCENDING
  =SEEK('M'+&lcMainF..CUTTKT,lcCtktBom)
  lnLastRecd = IIF(EOF(lcCtktBom) , 0 , RECNO(lcCtktBom)) && Evaluate record Number of last record in detail lines.
  SET ORDER TO TAG CTKTBOM ASCENDING
ELSE
  lnLastRecd = lnLastRec  
ENDIF

SELECT (lcThAlias)             && Restore Alias.

*- End Of lfLastRecd.
*!*************************************************************
