*:***************************************************************************
* Application : Aria27
* Module      : SM
* Date        : 10/03/2006
* Purpose     : This program is to remove duplicated fields from sydflfld 
* Developer   : Tarek Mohammed Ibrahim
*:***************************************************************************
CLOSE DATA
lcDir = GETDIR('')
IF EMPTY(lcDir)  
  RETURN
ENDIF  
SET DEFA TO (lcDir)
IF !FILE(lcDir+'SYDFLFLD.DBF')
  WAIT WINDOW NOWAIT 'WRONG FOLDER'
  RETURN
ENDIF

USE (LCDIR+'SYDFLFLD') ORDER CFLFLD
_TALLY = 1

*- get duplicated lines from the sydflfld system file
SELECT CFILE_NAM, CFLD_NAME , COUNT(*) AS CNT ;
   FROM SYDFLFLD ;
   WHERE !DELETED('SYDFLFLD') ;
   GROUP BY CFILE_NAM, CFLD_NAME ;
   HAVING CNT > 1 ;
   INTO CURSOR DUPS

IF _TALLY = 0
  WAIT WINDOW NOWAIT 'NO DOUPLICATES FOUND'
  RETURN
ENDIF
  
*- Loop throug duplicated fields and delete extra lines  
SELECT DUPS
SET RELATION TO CFILE_NAM+CFLD_NAME INTO SYDFLFLD
SCAN
  SELECT SYDFLFLD
  SKIP
  DELETE REST WHILE CFILE_NAM+CFLD_NAME = DUPS.CFILE_NAM+DUPS.CFLD_NAME
ENDSCAN

WAIT WINDOW NOWAIT 'DUPLICATED REMOVED'