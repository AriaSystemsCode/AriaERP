*:****************************************************************
*: Program file  : MAFABDEL.PRG
*: Program desc. : Fabric Order Delivery Schedule Report.
*: System        : Aria Apparel System (A27).
*: Module        : Material (MA)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200439,1
*: Date          : 12/11/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfCreatemp , lfwOGWhen  , 
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200439,1
*:****************************************************************
*: Modification:
*: B606888,4 ASH 02/27/2003 Fix the bug of not sorting by date.
*:****************************************************************
*- lcMainF     : - Temp Detail line.
*-- ldFromDate :- Printed date at the FRX (From Date)
*-- ldToDate   :- Printed date at the FRX (To Date)

STORE {} TO ldFromDate , ldToDate

*-- Get the Position.
lnDatePos = lfItmPos("POFLN.DARCOMPLE")
IF lnDatePos # 0
  ldFromDate  = Left(laOgFxFlt[lnDatePos,6],10)
  ldToDate    = Substr(laOgFxFlt[lnDatePos,6],12,10)
ENDIF

*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
 
  *-- Create Temp Files.
  = lfCreatemp ()

  *-- Collecting Code...
  = lfCollect ()
  
ENDIF
*-- End if for user change filter .

SELECT (lcMainF)
SET ORDER TO (lcMainF)
SET RELATION TO Fabric+Color                INTO Fabric   ADDITIVE

lcCENTURY = SET('CENTURY')
SET CENTURY ON
DO gfDispRe WITH EVAL('lcRpName')
SET CENTURY &lcCENTURY

SELECT (lcMainF)
SET RELATION TO 

*-- End OF Code.
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/11/2002
*: Purpose   : function Collect data into temp file.
*:*************************************************************
*: Called from : The Option Grid
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect()
*:*************************************************************
*:
FUNCTION lfCollect
PRIVATE lnAlias

lnAlias = SELECT (0)
lcWorkFile = 'POFLN'

SELECT (lcWorkFile)
SET RELATION TO cmattype+pomat INTO POFHDR   ADDITIVE
SET RELATION TO Vendor                      INTO apvendor ADDITIVE
SET RELATION TO Fabric+Color                INTO Fabric   ADDITIVE

IF lnDatePos > 0
  lnBet = ATC("BETWEEN(DTOS(POFLN.DARCOMPLE" , lcrpexp)
  IF lnBet > 0
    lcPoflnExp = SUBSTR(lcRpexp,lnBet,ATC("})))" , LCRPEXP , 1) + 3)
    lcRpExp = STRTRAN(lcRpExp,lcPoflnExp," .T. ") 
  ELSE
  lcPoflnExp = ".T."
  ENDIF
ENDIF

*-- fabric+color+cmattype+pomat+trancd
SCAN FOR &lcRpExp .AND. POFHDR.STATUS = 'O' .AND. POFHDR.CMATTYPE = 'P'
  SCATTER MEMVAR MEMO
  IF SEEK(M.fabric+M.color+M.cmattype+M.pomat,lcMainF)
    SELECT (lcMainF)
    DO CASE
      CASE M.trancd = '1' .AND. &lcPoflnExp
        REPLACE nFabTotQty With nFabTotQty + M.nFabTotQty
      OTHERWISE
        REPLACE nFabTotQty With nFabTotQty - M.nFabTotQty
    ENDCASE
    SELECT(lcWorkFile)
  ELSE
    IF (M.trancd = '1' .AND. &lcPoflnExp)
      INSERT INTO (lcMainF) FROM MEMVAR
    ENDIF  
  ENDIF  

ENDSCAN

SELECT (lcWorkFile)
SET RELATION TO


SELECT (lcMainF)
DELE ALL FOR nFabTotQty <= 0
SELECt(lnAlias)


*-- End OF lfCollect.
*:***************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/11/2002
*: Purpose   : Create the temp files.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfCreatemp()
*:****************************************************************
*:
FUNCTION lfCreatemp
PRIVATE lnAlias

lnAlias = SELECT(0)
*-- Create the temporary file
SELECT POFLN
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

CREATE TABLE (gcWorkDir+lcMainF) FROM ARRAY laFileStru
*B606888,4 ASH 02/27/2003 (Begin) Fix the bug of not sorting by date.
*INDEX ON Vendor+DTOC(DARCOMPLE) TAG (lcMainF)
INDEX ON Vendor+DTOS(DARCOMPLE) TAG (lcMainF)
*B606888,4 ASH 02/27/2003 (End)
INDEX ON fabric+color+cmattype+pomat TAG 'lcMainF'
SELECT(lnAlias)

*-- End Of lfCreatemp.
*:*************************************************************
*: Name      : lfSRVFab
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/11/2002
*: Purpose   : control browsing primary fabric and validate 
*:           : selecting it in inlist function.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : gfModalGen
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None
*:*************************************************************
*: Example   : =lfSRVFab()
*:*************************************************************
*: Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*:*************************************************************
*:
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    *llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE

*-- end of lfSRVFab.
*:*************************************************************
*: Name      : lfFabSum
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/11/2002
*: Purpose   : sum a specific field for the current fabric in fabric file
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid,fabric browse calculated fields.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : Calculated field value.
*:*************************************************************
*: Example   : =lfFabSum()
*:*************************************************************
*:
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)

*-- end of lfFabSum.
*:*************************************************************
*: Name      : lfSetVenRp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/11/2002
*: Purpose   : Go top in Vendor file.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfSetVenRp()
*:*************************************************************
*:
FUNCTION lfSetVenRp
PARAMETERS OpGrdParm
PRIVATE lnAlias

lnAlias = SELECT (0)

DO CASE
  CASE OpGrdParm = 'S'
   SELECT APVENDOR
   LOCATE
ENDCASE

SELECT(lnAlias)
RETURN

*-- End of lfSetVenRp.
*:*************************************************************
*: Name      : lfItmPos
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/11/2002
*: Purpose   : Evaluate fixed filter position within array.
*:*************************************************************
*: Called from : This Program.
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfItmPos()
*:*************************************************************
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
*:*************************************************************

