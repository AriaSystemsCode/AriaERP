LCDATA=GETDIR('','Select data directory')
IF EMPTY(LCDATA)
  WAIT WINDOW ' NO DATA DIRECRORY WAS SELECTED!'
  RETURN
ENDIF  

CLOSE DATA

*--Open needed files.
USE (LCDATA+'ordline.dbf') SHARED IN 0
USE (LCDATA+'ordhdr.dbf')  SHARED IN 0
SELECT 0
USE (LCDATA+'posln.dbf') SHARED 
SET ORDER TO TAG Poslnsh
SELECT 0
USE (LCDATA+'cutpick.dbf') SHARED
SET ORDER TO TAG Cutord
SELECT 0
USE (LCDATA+'Piktkt.dbf') SHARED
SET ORDER TO TAG Piktkt




*-- 1 ) FIX THE WRONG FLAG OF THE PICKED ORDER LINES.
SELECT ORDLINE 
REPLACE ALL Picked WITH .T. FOR !EMPTY(PikTkt) AND !Picked

            

*-- 2 ) RELEASE THE PICK TICKETS THAT INCLUDED IN COMPLETE OR CANCELLED ORDER.

*-- Collect all order lines that has piktkts.
SELECT ordline.*,ordhdr.status,Piktkt.Status AS PIKSTATUS,Piktkt.cpicktype ;
    FROM ordline,ordhdr,piktkt ;
   WHERE !EMPTY(ordline.piktkt) and ordline.piktkt#'*' and ordhdr.order=ordline.order and piktkt.piktkt=ordline.piktkt ;
    INTO CURSOR TmpOrdLn

SELECT ORDLINE
SET ORDER TO TAG ORDLINE
*--Set filter to see only complete or canceled order line and which has an
*--open or pulled or hold pick tickets.
SELECT TmpOrdLn
SET FILTER TO status $ 'CX' and PIKSTATUS $ 'OHP'

SCAN 
  WAIT WINDOW Order NOWAIT

  *--Complete or cancel this pick tickets.
  IF SEEK(Piktkt,'PIKTKT')
    REPLACE PIKTKT.Status WITH TmpOrdLn.Status
  ENDIF

  *--Clear releated fields to this complete or canceled piktkt in ordline file.
  IF SEEK(cordtype+order+STR(lineno,6),'ORDLINE')
    SELECT ORDLINE
    =RLOCK()
    REPLACE PIK1 WITH 0,PIK2 WITH 0,PIK3 WITH 0,PIK4 WITH 0,PIK5 WITH 0,PIK6 WITH 0,PIK7 WITH 0,PIK8 WITH 0,;
            TOTPIK WITH 0,PICKED WITH .F.,PIKDATE WITH {},PIKTKT WITH ' ' 
    UNLOCK
    SELECT TmpOrdLn
  ENDIF

ENDSCAN



*-3) If the piktkt was cancelled and the piktkt field still in order line.
SELECT ORDLINE
SCAN
  WAIT WINDOW Order NOWAIT

  *--If no link with adornment order, then fine , skip this order line.
  *--All bank links should be skipped.
  IF !SEEK('2'+ORDLINE.Order+STR(ORDLINE.LineNo,6),'CUTPICK')
    LOOP
  ENDIF

  *--If the relation with adornment order was found.
  *--then need to check if it was correct.
  
  *--If no piktkt asigned OR it not generated yet.
  *--Then this link should not be exist.
  IF EMPTY(ORDLINE.Piktkt) OR ORDLINE.Piktkt='******'
    *--Remove the link.
    SELECT CUTPICK
    DELETE
  ELSE

  *--If there is a piktkt exist.  
    IF !SEEK(ORDLINE.Piktkt+'A'+CUTPICK.cTktNo+CUTPICK.style+CUTPICK.CTKTLINENO+'1','POSLN')
      *--No adornmet line found for this piktkt.
      *STOP
      WAIT WINDOW 'Wrong Data :'+ ORDLINE.Piktkt
 *<<You should record this piktkts and release them and generate new one.>>


    ELSE
     *--both order line and adornment line are there, then fine.
     
    ENDIF

  ENDIF

ENDSCAN

CLOS DATA

