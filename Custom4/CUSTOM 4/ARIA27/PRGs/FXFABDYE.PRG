*:**************************************************************************
*: Program     : FxFabDye
*: Date        : 12/11/2001
*: Description : Fix FabDye file.
*: Developer   : Sameh Saiid Ezzat (SSE)
*: Refer to    : B606997,1
*:**************************************************************************
*
PRIVATE lcDataDir
lcDataDir = GETDIR('','Select Company Data.')
IF EMPTY(lcDataDir) OR !FILE(lcDataDir+'FabDye.DBF')
  WAIT WINDOW 'Wrong company data directory!' TIMEOUT 3
  RETURN
ENDIF

PRIVATE llFabDye , laFabDye

*-- Open needed files.
IF !USED('FabDye')
  USE (lcDataDir+'FabDye.DBF') IN 0 SHARE ORDER TAG FabDye
  llFabDye = .T.
ENDIF

SELECT FabDye
=AFIELDS(laFabDye)
CREATE TABLE (lcDataDir+'TmpFbDye') FROM ARRAY laFabDye
INDEX ON Fabric + Color + cWareCode + Dyelot TAG TmpFbDye

SELECT FabDye
SCAN
  WAIT WINDOW 'Fixing duplicate records, please wait.' NOWAIT
  SCATTER MEMVAR MEMO
  SELECT TmpFbDye
  IF SEEK(FabDye.Fabric + FabDye.Color + FabDye.cWareCode + FabDye.Dyelot)
    REPLACE OnHand  WITH OnHand  + FabDye.OnHand  ,;
            Usage   WITH Usage   + FabDye.Usage   ,;
            OnOrder WITH OnOrder + FabDye.OnOrder ,;
            OnRet   WITH OnRet   + FabDye.OnRet   ,;
            nMatWip WITH nMatWip + FabDye.nMatWip
  ELSE
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF
ENDSCAN
WAIT CLEAR

SELECT FabDye
DELETE ALL
APPEND FROM (lcDataDir + 'TmpFbDye')

USE IN TmpFbDye
ERASE (lcDataDir + 'TMPFBDYE.DBF')
ERASE (lcDataDir + 'TMPFBDYE.CDX')

IF llFabDye
  USE IN FabDye
ENDIF
*-- End of FxFabDye.