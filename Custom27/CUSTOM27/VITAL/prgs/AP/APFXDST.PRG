*B606939,1 ASH 03/24/2003 Fix program to fix APDIST file.
** The problem was that some ap invoices has post date in APINVHDR differ than what is in APDIST.
** To fix this, I changed the date of APDIST using the same mechanism used if you changed the date from ap invoice screen.
** By voiding the orginal record and add new records with the new date.

lnRespns = gfModalGen('QRM00000B00006','DIALOG',.F.,.F.,;
                      'This program will update APDIST file, Are you sure to continue?')
IF lnRespns # 1
  RETURN
ENDIF

lcTempSave  = gfTempName()
IF !USED('APDIST')
  =gfOpenFile(gcDataDir+'APDIST','Invvend','SH')
ENDIF
IF !USED('apinvhdr')
  =gfOpenFile(gcDataDir+'apinvhdr','Invvend','SH')
ENDIF

SELECT APDIST
=AFIELDS(laFileStru)
** Creat temp  file from the array.
CREATE TABLE &gcWorkDir.&lcTempSave FROM ARRAY laFileStru
lcSequence = gfSequence('CAPSESSNO')

SELECT apinvhdr
scan 
  WAIT WINDOW cinvno+cvendcode NOWAIT
  IF SEEK(cinvno+cvendcode,'APDIST')
    SELECT APDIST
    SCAN while cinvno+cvendcode+capdtrtyp = apinvhdr.cinvno+apinvhdr.cvendcode
      IF capdstat = 'V' OR dapdtrdat = APINVHDR.dpostdate OR capdtrtyp <> 'I'
        LOOP
      ENDIF
      SCATTER MEMVAR MEMO
      REPLACE capdstat WITH 'V'
      SELECT (lcTempSave)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE NAPDAMNT  WITH NAPDAMNT * -1      ,;
              CAPSESSNO WITH lcSequence         ,;
              capdstat  WITH 'V'
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE dapdtrdat WITH APINVHDR.dpostdate ,;
              CAPSESSNO WITH lcSequence         ,;
              lapdpost  WITH .F. 
    ENDSCAN      
  ENDIF
ENDSCAN

SELECT (lcTempSave)
GO TOP
IF EOF()
  WAIT CLEAR
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'No changes done to APDIST.')
ELSE
  WAIT WINDOW 'Updating APDIST file' NOWAIT
  SCAN
    SCATTER MEMVAR 
    INSERT INTO APDIST FROM MEMVAR 
  ENDSCAN
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'APDIST updated successfully.')
ENDIF
SELECT APDIST
USE
SELECT apinvhdr
USE
WAIT CLEAR