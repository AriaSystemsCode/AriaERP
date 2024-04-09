*:**************************************************************************
*: Program file  : MAFIXFAB
*: Program desc. : Fix program to fill the new added field (cClr_Ref) in Fabric form Old version.
*: System        : Aria Advantage Series.
*: Module        : Materials (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 09/05/2001
*: Reference     : ????????
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*
PARAMETERS lcDataDir

PRIVATE lcDataDir , llEmptyMA , lnLength , lcOldVrDir
lcDataDir = ""
lcDataDir = GETDIR('','Select 2.7 data directory')

IF EMPTY(lcDataDir)
  RETURN
ENDIF

llEmptyMA = .F.
llEmptyMA = FILE(lcDataDir+'FABRIC.DBF')

lnLength = LEN(lcDataDir)
IF !SUBSTR (lcDataDir,lnLength)='\'
  lcDataDir=lcDataDir+'\'
ENDIF
  
*--Open Fabric
IF USED('Fabric')
  USE IN Fabric
ENDIF  
USE (lcDataDir+'Fabric') IN 0 ORDER TAG Fabric

*--Update Sales Orders
WAIT WINDOW 'Start updating....' NOWAIT
IF llEmptyMA
  DO lpUpdFab
ENDIF

*--Close files
IF USED('Fabric')
  USE IN Fabric
ENDIF
IF USED('Item')
  USE IN Item
ENDIF
WAIT CLEAR
*--End of SOCANFIX

*!**************************************************************************
*! Name      : lpUpdFab
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 06/24/2001
*! Purpose   : Fill the new added field (cClr_Ref) in Fabric form Old version.
*!**************************************************************************
*! Example   : DO lpUpdFab
*!**************************************************************************
*
PROCEDURE lpUpdFab

lcOldVrDir = ""
lcOldVrDir = GETDIR('','Select 2.0 data directory')

IF EMPTY(lcOldVrDir) 
  WAIT WINDOW "Operation is cancelled." TIMEOUT 3
  RETURN
ENDIF
lnLength = LEN(lcOldVrDir)
IF !SUBSTR (lcOldVrDir,lnLength)='\'
  lcOldVrDir=lcOldVrDir+'\'
ENDIF

IF !FILE(lcOldVrDir+'Item.DBF')
  WAIT WINDOW "Wrong path for Aria old version." TIMEOUT 3
  RETURN
ENDIF

*--Open Item
IF USED('Item')
  USE IN Item
ENDIF  

USE (lcOldVrDir+'Item') IN 0 ORDER TAG Item

SELECT Fabric
SCAN   
  DO CASE
    CASE SUBSTR(Fabric,1,1) = "L"
      lcKey2Sek = "L"
    CASE SUBSTR(Fabric,1,1) = "T"
      lcKey2Sek = "T"
    OTHERWISE
      lcKey2Sek = "F"
  ENDCASE
  lcKey2Sek = lcKey2Sek + substr(CITEM,1,3)+ substr(CITEM,5,7)+ substr(CITEM,13)+SUBSTR(Color,1,3)
  *--- type+season+style_num+patt_num+color
  IF SEEK(lcKey2Sek,'ITEM')
    WAIT WINDOW 'Updating fabric : ' + Fabric NOWAIT
    REPLACE cOldDesc WITH Item.Desc
  ELSE
    WAIT WINDOW 'ITEM ' + LCKEY2SEK + ' WAS NOT FOUND IN THE OLD SYSTEM. OPERATION WILL BE TERMINATED.'
    EXIT
  ENDIF
ENDSCAN
*--End of lpUpdFab
