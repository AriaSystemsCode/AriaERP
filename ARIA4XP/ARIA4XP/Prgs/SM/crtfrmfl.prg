*maRIAM
LPARAMETERS lcA27Sys
USE ADDBS(lcA27Sys)+'SYCCOMP.DBF' SHAR ALIAS 'SYCCOMP_A' IN 0 
SELECT SYCCOMP_A
lcOldComp = oAriaApplication.ActiveCompanyId
SCAN FOR lRunfroma4
  WAIT WINDOW 'Updating forms table of company:'+SYCCOMP_A.ccomp_id NOWAIT 
  SELECT SYCCOMP_A
  *MT
    IF EMPTY(ALLTRIM(CCONSERVER)) OR EMPTY(ALLTRIM(CCONDBNAME))
      LOOP
    ENDIF
    *MT
  oAriaApplication.activecompanyid = SYCCOMP_A.ccomp_id
  oAriaApplication.GetCompanyInformation(SYCCOMP_A.ccomp_id)
  lnEmailForm = oAriaApplication.RemoteCompanyData.execute("Select * From EMAIL_FORMS ",'',;
                "EMAIL_FORMS","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))  
  IF lnEmailForm < 0
  *Create  
  lnEmailForm = oAriaApplication.RemoteCompanyData.execute("CREATE TABLE [EMAIL_FORMS]([PARTNER_TYPES] [char](10) NULL,	[PARTNER_ID] [char](20) NULL,"+;
	"[STORE_ID] [char](8) NULL,"+;
	"[EMAIL_ADDRESSES] [text] NULL,"+;
	"[Overwritten] [bit] NULL,"+;
	"[ID] [char](20) NULL,"+;
	"[EMAIL_FORMS_KEY] [uniqueidentifier] NULL"+;
	") ON [PRIMARY] TEXTIMAGE_ON [PRIMARY] ",'',;
                "EMAIL_FORMS","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))  
   IF lnEmailForm > 0
      lnEmailForm = oAriaApplication.RemoteCompanyData.execute("ALTER TABLE [EMAIL_FORMS] ADD  DEFAULT (newid()) FOR [EMAIL_FORMS_KEY]",'',;
                 "EMAIL_FORMS","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))  
    ENDIF
  ENDIF         
ENDSCAN 
oAriaApplication.activecompanyid = lcOldComp 
oAriaApplication.GetCompanyInformation(lcOldComp)
USE IN 'SYCCOMP_A'