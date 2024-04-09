LCA27SYSFILESPATH = GETDIR("","Select Aria27 Sysfiles Folder")
IF EMPTY(LCA27SYSFILESPATH) OR (!EMPTY(LCA27SYSFILESPATH) AND !DIRECTORY(LCA27SYSFILESPATH))
  MESSAGEBOX('Selected Directory is Incorrect')
  RETURN .F.
ENDIF

*!*	lcA4SqlDicPath = GETDIR("","Select Aria4 SQL Dictionary Folder")
*!*	IF EMPTY(lcA4SqlDicPath) OR (!EMPTY(lcA4SqlDicPath) AND !DIRECTORY(lcA4SqlDicPath))
*!*	  MESSAGEBOX('Selected Directory is Incorrect')
*!*	  RETURN .F.
*!*	ENDIF


IF FILE(ADDBS(LCA27SYSFILESPATH)+'SYCCOMP.DBF')
  USE ADDBS(LCA27SYSFILESPATH)+'SYCCOMP.DBF' IN 0
ELSE
  MESSAGEBOX('Selected Directory Does not Contain SYccomp Table')
  RETURN .F.
ENDIF

SELECT SYCCOMP
SCAN

  LCSTYLEDATAPATH = ALLTRIM(SYCCOMP.CCOM_DDIR)    &&GETDIR("","Select Aria27 Dbfs directory")
  LCSTYLEDATAPATH = ADDBS(LCSTYLEDATAPATH)
  IF !DIRECTORY(LCSTYLEDATAPATH)
    MESSAGEBOX('Could Not update company '+SYCCOMP.CCOMP_ID)
    LOOP
  ENDIF
  WAIT WINDOW "Updating Aria27 data Files of Company "+SYCCOMP.CCOMP_ID NOWAIT
  *USE ADDBS(lcA27SysfilesPath)+"\SYDFLFLD.DBF" SHARED IN 0
  USE ADDBS(LCSTYLEDATAPATH )+"\Contact.DBF" SHARED IN 0
  USE ADDBS(LCSTYLEDATAPATH )+"\Employee.DBF" SHARED IN 0
  SELECT EMPLOYEE
  SCAN
    SCATTER MEMVAR MEMO
    SELECT CONTACT
    APPEND BLANK
    REPLACE CONTACT.CCONT_ID   WITH m.ACCOUNT  ,;
      CONTACT.STORE      WITH m.STORE    ,;
      CONTACT.CCNTCTCODE WITH m.EMPLOYEE ,;
      CONTACT.CONTACT    WITH m.EMPNAME  ,;
      CONTACT.CADDRESS1 WITH m.EMPADD1   ,;
      CONTACT.CADDRESS2 WITH m.EMPADD2   ,;
      CONTACT.CADDRESS3 WITH m.EMPADD3   ,;
      CONTACT.CADDRESS4 WITH m.EMPADD4   ,;
      CONTACT.CADDRESS5 WITH m.EMPADD4   ,;
      CONTACT.CADDRESS6 WITH m.EMPADD5   ,;
      CONTACT.PAYROLLNO WITH m.PAYROLLNO ,;
      CONTACT.UCODE     WITH m.UCODE     ,;
      CONTACT.DSTART    WITH m.DSTART    ,;
      CONTACT.DEND      WITH m.DEND      ,;
      CONTACT.SITE_NO   WITH m.SITE_NO   ,;
      CONTACT.NPERIOD   WITH m.NPERIOD   ,;
      CONTACT.CCONTTYPE WITH "C"
    Replace CONTACT.cadd_user WITH m.cadd_user  ,;
      CONTACT.cadd_time       WITH m.cadd_time  ,;
      CONTACT.dadd_date       WITH m.dadd_date  ,;
      CONTACT.cedit_user      WITH m.cedit_user ,;
      CONTACT.cedit_time      WITH m.cedit_time ,;
      CONTACT.dedit_date      WITH m.dedit_date ,;
      CONTACT.clok_user       WITH m.clok_user  ,;
      CONTACT.clok_time       WITH m.clok_time  ,;
      CONTACT.dlok_date       WITH m.dlok_date  ,;
      CONTACT.cowner          WITH m.cowner 

  ENDSCAN
  USE IN CONTACT
  USE IN EMPLOYEE
  SELECT SYCCOMP
ENDSCAN

