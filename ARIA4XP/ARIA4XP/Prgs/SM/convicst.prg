LPARAMETERS lcA27Sys
*B610062,1 HIA 

USE ADDBS(lcA27Sys)+'SYCCOMP.DBF' SHAR ALIAS 'SYCCOMP_A' IN 0
SELECT SYCCOMP_A
lcOldComp = oAriaApplication.ActiveCompanyId
SCAN FOR lRunfroma4
  WAIT WINDOW 'Updating IC Style History table of company:'+SYCCOMP_A.ccomp_id NOWAIT
  SELECT SYCCOMP_A
  *MT
  IF EMPTY(ALLTRIM(CCONSERVER)) OR EMPTY(ALLTRIM(CCONDBNAME))
    LOOP
  ENDIF
  *MT
  oAriaApplication.activecompanyid = SYCCOMP_A.ccomp_id
  oAriaApplication.GetCompanyInformation(SYCCOMP_A.ccomp_id)
  =gfOpenTable('ICSTYHST','STYHST','SH','SQLSTY')
  USE ADDBS(ALLTRIM(syccomp.ccom_ddir))+'ICSTYHST.dbf' SHAR ORDER STYHST IN 0 ALIAS 'FOXSTY'
  SELECT FOXSTY
  SCAN
    IF !gfSeek(FOXSTY.STYLE+FOXSTY.CFISFYEAR,'SQLSTY')
      SCATTER MEMO MEMVA
      SELECT  'SQLSTY'
      APPEND BLANK
      GATHER MEMO MEMVAR
      =gfReplace('')
    ENDIF
  ENDSCAN
  SELECT   'SQLSTY'
  =gfTableUpdate()
  gfCloseTable('SQLSTY')
  USE IN 'FOXSTY'
ENDSCAN
oAriaApplication.activecompanyid = lcOldComp
oAriaApplication.GetCompanyInformation(lcOldComp)
USE IN 'SYCCOMP_A'
