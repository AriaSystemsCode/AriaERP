*************************************************
*B606959,1 ASH 02/16/2003 
*************************************************
LCFILE=GETFILE('DBF','Select PikTkt file')
IF RIGHT(LCFILE,10) <> 'PIKTKT.DBF'
  WAIT WINDOW 'Please select PIKTKT file'
  RETURN
ENDIF
USE (LCFILE) AGAIN IN 0
LOCATE
LCPT = ''
SCAN
  WAIT WINDOW PIKTKT NOWAIT
  IF PIKTKT = '******'
    LOOP
  ENDIF
  IF LCPT = PIKTKT
    REPLACE CLOK_USER WITH 'ASH'
  ENDIF
  LCPT = PIKTKT
ENDSCAN
LOCATE
DELETE ALL FOR CLOK_USER = 'ASH'
WAIT WINDOW 'PIKTKT file has been fixed'
USE