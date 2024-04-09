*E302944,1 MMT 07/25/2011 Save Carriers account info. per account in customer screen[T20101207.0006]
LPARAMETERS lcA27Sys
USE ADDBS(lcA27Sys)+'SYCCOMP.DBF' SHAR ALIAS 'SYCCOMP_A' IN 0 
SELECT SYCCOMP_A
lcOldComp = oAriaApplication.ActiveCompanyId
SCAN FOR lRunfroma4
  WAIT WINDOW 'Updating ACCOUNT_CARRIERS_T table of company:'+SYCCOMP_A.ccomp_id NOWAIT 
  SELECT SYCCOMP_A
  IF EMPTY(ALLTRIM(CCONSERVER)) OR EMPTY(ALLTRIM(CCONDBNAME))
    LOOP
  ENDIF
  oAriaApplication.activecompanyid = SYCCOMP_A.ccomp_id
  oAriaApplication.GetCompanyInformation(SYCCOMP_A.ccomp_id)
  lnEmailForm = oAriaApplication.RemoteCompanyData.execute("Select * From ACCOUNT_CARRIERS_T ",'',;
                "ACCOUNT_CARRIERS_T","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))  
  IF lnEmailForm < 0
  *Create  
  lnEmailForm = oAriaApplication.RemoteCompanyData.execute("CREATE TABLE [ACCOUNT_CARRIERS_T]([ACCOUNT] [char](5) NULL,	[STORE] [char](8) NULL,"+;
	"[CARRIER_ID] [char](20) NULL,"+;
	"[Overwritten] [bit] NULL,"+;
	"[CARRIER_ACCOUNT] [char](20) NULL,"+;
	"[ACCOUNT_CARRIERST_KEY] [uniqueidentifier])",'',;
                "ACCOUNT_CARRIERS_T","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))  
   IF lnEmailForm > 0
      lnEmailForm = oAriaApplication.RemoteCompanyData.execute("ALTER TABLE [ACCOUNT_CARRIERS_T] ADD  DEFAULT (newid()) FOR [ACCOUNT_CARRIERST_KEY]",'',;
                 "ACCOUNT_CARRIERS_T","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))  
    ENDIF
  ENDIF         
ENDSCAN 
oAriaApplication.activecompanyid = lcOldComp 
oAriaApplication.GetCompanyInformation(lcOldComp)
USE IN 'SYCCOMP_A'