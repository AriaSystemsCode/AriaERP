LPARAMETERS lcA27Sys
USE ADDBS(lcA27Sys)+'SYCCOMP.DBF' SHAR ALIAS 'SYCCOMP_A' IN 0
SELECT SYCCOMP_A
lcOldComp = oAriaApplication.ActiveCompanyId
SCAN FOR lRunfroma4
  WAIT WINDOW 'Updating GLDIST table of company:'+SYCCOMP_A.ccomp_id NOWAIT
  SELECT SYCCOMP_A
  IF EMPTY(ALLTRIM(CCONSERVER)) OR EMPTY(ALLTRIM(CCONDBNAME))
    LOOP
  ENDIF
  oAriaApplication.activecompanyid = SYCCOMP_A.ccomp_id
  oAriaApplication.GetCompanyInformation(SYCCOMP_A.ccomp_id)
  =gfOpenTable('GLDIST','GLDISTNO','SH','SQLGLDST')
  USE ADDBS(ALLTRIM(syccomp.ccom_ddir))+'GLDIST.dbf' sHaR ORDER GLDISTNO IN 0 ALIAS 'FOXGLDST'
  SELECT FOXGLDST
  LOCATE
  lnCntRec = 0
  SCAN FOR !DELETED()
    *IF !gfSeek(FOXAUD.CAPOBJNAM+FOXAUD.KEY+FOXAUD.CAUDTRALID,'SQLAUD')
      SCATTER MEMO memVA
      SELECT  'SQLGLDST'
      APPEND BLANK
      GATHER MEMO MEMVAR
      =gfReplace('')
      lnCntRec = lnCntRec + 1
      IF lnCntRec > 100000
        SELECT 'SQLGLDST'
        =gfTableUpdate()        
        lnCntRec = 0
      ENDIF
    *ENDIF
  ENDSCAN
  SELECT   'SQLGLDST'
  =gfTableUpdate()
  gfCloseTable('SQLGLDST')
  USE IN 'FOXGLDST'
  
****
  WAIT WINDOW 'Updating STYINVJL table of company:'+SYCCOMP_A.ccomp_id NOWAIT
  =gfOpenTable('STYINVJL','STYINVJL','SH','SQLSTYIN')
  USE ADDBS(ALLTRIM(syccomp.ccom_ddir))+'STYINVJL.dbf' sHaR ORDER STYINVJL IN 0 ALIAS 'FOXSTYIN'
  SELECT FOXSTYIN
  LOCATE
  lnCntRec = 0
  SCAN FOR !DELETED()
    *IF !gfSeek(FOXAUD.CAPOBJNAM+FOXAUD.KEY+FOXAUD.CAUDTRALID,'SQLAUD')
      SCATTER MEMO memVA
      SELECT  'SQLSTYIN'
      APPEND BLANK
      GATHER MEMO MEMVAR
      =gfReplace('')
      IF lnCntRec > 100000
        SELECT 'SQLGLDST'
        =gfTableUpdate()        
        lnCntRec = 0
      ENDIF
    *ENDIF
  ENDSCAN
  SELECT   'SQLSTYIN'
  =gfTableUpdate()
  gfCloseTable('SQLSTYIN')
  USE IN 'FOXSTYIN'
  ****
  
ENDSCAN
oAriaApplication.activecompanyid = lcOldComp
oAriaApplication.GetCompanyInformation(lcOldComp)
USE IN 'SYCCOMP_A'
