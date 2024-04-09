LPARAMETERS lcA27Sys
USE ADDBS(lcA27Sys)+'SYCCOMP.DBF' SHAR ALIAS 'SYCCOMP_A' IN 0
SELECT SYCCOMP_A
lcOldComp = oAriaApplication.ActiveCompanyId
SCAN FOR lRunfroma4
  WAIT WINDOW 'Updating Audtrail table of company:'+SYCCOMP_A.ccomp_id NOWAIT 
  SELECT SYCCOMP_A
*MT
    IF EMPTY(ALLTRIM(CCONSERVER)) OR EMPTY(ALLTRIM(CCONDBNAME))
      LOOP
    ENDIF
    *MT  
  oAriaApplication.activecompanyid = SYCCOMP_A.ccomp_id
  oAriaApplication.GetCompanyInformation(SYCCOMP_A.ccomp_id)
  =gfOpenTable('AUDTRAIL','AUDTRAIL','SH','SQLAUD')
  USE ADDBS(ALLTRIM(syccomp.ccom_ddir))+'AUDTRAIL.dbf' sHaR ORDER AUDTRAIL IN 0 ALIAS 'FOXAUD'
  SELECT FOXAUD
  SCAN
    IF !gfSeek(FOXAUD.CAPOBJNAM+FOXAUD.KEY+FOXAUD.CAUDTRALID,'SQLAUD')
      SCATTER MEMO memVA
      SELECT  'SQLAUD'
      APPEND BLANK
      GATHER MEMO MEMVAR 
      =gfReplace('')
    ENDIF
  ENDSCAN
  SELECT   'SQLAUD'
  =gfTableUpdate()
  gfCloseTable('SQLAUD')
  USE IN 'FOXAUD'
ENDSCAN 
oAriaApplication.activecompanyid = lcOldComp 
oAriaApplication.GetCompanyInformation(lcOldComp)
USE IN 'SYCCOMP_A'