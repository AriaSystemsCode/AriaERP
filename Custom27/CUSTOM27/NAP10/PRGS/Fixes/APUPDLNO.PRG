******************************************************************************************
*! Name      : APUpdLNo
*! Developer : Amin Khodary 
*! Date      : 11/05/2001
*! Purpose   : Update new fields cApdGlAct and cApdLinNo in APInvTkt file with the proper values
*!             get from APInvDtl file
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   :  None
*!*************************************************************

PRIVATE lcPath ,  lcDataDir ,llUsApDtl , llUsApTkt , llExitApDtl ,llExitApTkt

lcPath = SET('PATH')

STORE .T. TO llUsApDtl , llUsApTkt 
STORE .F. TO llExitApDtl ,llExitApTkt

lcDataDir = GETDIR('','Select data directory')

IF TYPE('lcDataDir') # 'C' OR EMPTY(lcDataDir)
  WAIT WIND "Invalid data directory. Conversion not compelted"
  lcDataDir = ""
  RETURN 
ENDIF

llExitApDtl = FILE(lcDataDir+'APVINVDT.DBF') 
llExitApTkt = FILE(lcDataDir+'APINVTKT.DBF')

IF !llExitApDtl OR !llExitApTkt 
  WAIT WIND "AP files not existed in the selected directory! Conversion not completed"
  RETURN 
ENDIF

lnLen=LEN(lcDataDir)
IF !SUBSTR (lcDataDir,lnLen)='\'
  lcDataDir=lcDataDir+'\'
ENDIF  

USE lcDataDir+'APVINVDT'  SHARE IN 0   AGAIN ALIAS lcApvInvDt
SELECT lcApvInvDt
SET ORDER TO TAG Orgvinv

USE lcDataDir+'APINVTKT'  SHARE IN 0   AGAIN ALIAS lcAPInvTkt

SELECT lcAPInvTkt
SCAN FOR EMPTY(cApdLinNo) OR  EMPTY(cApdGlAct)
   lcKey = lcAPInvTkt.cVendCode+lcAPInvTkt.cApInvNo+ lcAPInvTkt.cApvilNo
   IF SEEK(lcKey ,'lcAPVINVDT')   
     REPLACE lcAPInvTkt.cApdGlAct WITH lcAPVINVDT.cApdGlAct  ,;
             lcAPInvTkt.cApdLinNo WITH lcAPVINVDT.cApViLno
   ENDIF
ENDSCAN 

WAIT WIND "AP files has been updated "  NOWAIT 

SELECT lcAPVINVDT 
USE

SELECT lcAPInvTkt
USE

SET PATH TO &lcPath 

RETURN

