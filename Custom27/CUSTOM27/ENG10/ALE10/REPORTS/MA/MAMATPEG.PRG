*:****************************************************************************************
*: Program file  : MAMATPEG.PRG
*: Program desc. : Material Purchase Order [For Cust ENG10]
*: System        : Aria Apparel System [A27].
*: Module        : Material Module  [MA]
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200148,1
*: Date          : 02/01/2001
*:****************************************************************************************
*: Calls 
*:               : FUNCTIONS  :
*:               ------------------------------------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************
*:C#200148,1          : 
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
*:
*-- Declaration variables.

STORE 0 TO lnAlias , lnlineno
STORE '' TO lcTempOrds

*-- END Declaration variables.
lnAlias = SELECT(0)
= gfOpenFile(gcDataDir+'MAPOALO',gcDataDir+'MAPOALO','SH')

*-- Create temp File.
= lfCrTmpFls ()



*-- Select the PO liens from the poflin file
SELECT POFLN
SCAN FOR Trancd = '1'
  STORE 0 To lnlineno
  
  *-- get the ass. SO to the selected PO# . 
  IF SEEK(POMAT+FABRIC+COLOR,'MAPOALO') 
    SELECT MAPOALO
    SCATTER MEMVAR MEMO
    SCAN FOR POMAT+FABRIC+COLOR+ORDER = POFLN.POMAT+POFLN.FABRIC+POFLN.COLOR
     lcTempOrds = lcTempOrds + IIF(lnlineno > 0 ,"|" + ORDER, ORDER)
     lnlineno = lnlineno + 1
    ENDSCAN
    *-- Save the variable to the filed.
    SELECT (lcDetTmp)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE mOrdrsPo WITH lcTempOrds ,;
            ORDER WITH '' 
    
    lcTempOrds = ''
    SELECT POFLN
  ENDIF
ENDSCAN

SELECT POFLN 
GOTO TOP
SET RELATION TO POMAT+FABRIC+COLOR INTO (lcDetTmp) ADDITIVE
SELECT (lnAlias)
*-- End Of Coding.
*:*************************************************************
*: Name        : lfCrTmpFls
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 02/01/2001
*: Purpose     : Create the temp file.
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example     : =lfCrTmpFls()
*:*************************************************************
*
FUNCTION lfCrTmpFls
PRIVATE lnAlias
lnAlias = 0 

lnAlias = SELECT(0)
SELECT MAPOALO 

=AFIELDS(laMlnTmp)
lnNewFld = ALEN(laMlnTmp,1)

DIMENSION laMlnTmp[lnNewFld + 1 , 4]

laMlnTmp[lnNewFld + 1,1] = 'mOrdrSpo'
laMlnTmp[lnNewFld + 1,2] = 'M'
laMlnTmp[lnNewFld + 1,3] = 10
laMlnTmp[lnNewFld + 1,4] = 0


CREATE DBF (gcWorkDir+lcDetTmp) FROM ARRAY laMlnTmp
=gfOpenFile(gcWorkDir+lcDetTmp,'','EX')
INDEX ON POMAT+FABRIC+COLOR TAG lcDetTmp
SELECT(lnAlias)

*-- End Of lfCrTmpFls

*:*************************************************************

