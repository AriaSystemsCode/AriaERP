*RemoveDuplicationsFromSydflfld

SET DELETED ON
SET EXCLUSIVE OFF

** lcPath = '..\..\SYSFILES\SYDFLFLD.DBF' : TO BE CORRECTED BY SABER

IF EMPTY(lcPath )
  RETURN
ENDIF
IF JUSTFNAME(lcPath ) <> 'SYDFLFLD.DBF'
  RETURN
ENDIF

USE (lcPath )
SET ORDER TO CFLFLD   && CFILE_NAM+CFLD_NAME

=lfGetDupl()
DO WHILE !EOF('SYDFLFLD_DUPL')
  SELECT SYDFLFLD_DUPL
  SCAN
    IF SEEK(CFILE_NAM+CFLD_NAME,'sydflfld')
      SELECT sydflfld
      DELETE
    ENDIF 
  ENDSCAN
  =lfGetDupl()
ENDDO 

************************************************************
*! Name      : lfGetDupl
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/01/2013
*! Purpose   : get duplicated lines
************************************************************
FUNCTION lfGetDupl
SELECT CFILE_NAM,CFLD_NAME,COUNT(*) AS CN FROM SYDFLFLD GROUP BY CFILE_NAM,CFLD_NAME HAVING CN>1 INTO CURSOR SYDFLFLD_DUPL

*- End of lfGetDupl.
