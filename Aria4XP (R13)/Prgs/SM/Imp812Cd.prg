*E612202,1 MMT 08/10/2020 Import VICS adjustment Reason ad handling code from XML [Send 812 Transcation]
Lparameters lcA27Sys
Set Step On
oAriaApplication.User_ID = 'Admin'
oAriaApplication.ref4.User_ID = 'Admin'
oAriaApplication.ref5.User_ID = 'Admin'
IF !FILE(oAriaApplication.ApplicationHome+"\SM\812Codes.XML")
  RETURN .F.
ENDIF
=XMLTOCURSOR(oAriaApplication.ApplicationHome+"\SM\812Codes.XML","ImpoCode",512)
IF !USED("ImpoCode") OR (USED("ImpoCode") AND RECCOUNT("ImpoCode") = 0)
  RETURN .F.
ENDIF

Use Addbs(lcA27Sys)+'SYCCOMP.DBF' Shar Alias 'SYCCOMP_A' In 0
Select SYCCOMP_A
lcOldComp = oAriaApplication.ActiveCompanyId
Scan For lRunfroma4 AND ('EB' $ SYCCOMP_A.mcomp_mdl OR 'EB' $ SYCCOMP_A.mmodlset)
  Wait Window 'Updating Codes table of company:'+SYCCOMP_A.ccomp_id Nowait
  Select SYCCOMP_A
  If Empty(Alltrim(CCONSERVER)) Or Empty(Alltrim(CCONDBNAME))
    Loop
  Endif
  oAriaApplication.ref4.ActiveCompanyId = SYCCOMP_A.ccomp_id
  oAriaApplication.GetCompanyInformation(SYCCOMP_A.ccomp_id)
  =gfOpenTable('CODES','CCODE_NO','SH')
  SELECT "ImpoCode"
  SCAN
    IF !gfSeek('N'+PADR(IIF(ImpoCode.Type ='R','CVICSADJCD','CADJHNDLNG'),10,' ')+ImpoCode.Code,'CODES')
      INSERT INTO CODES (CDEFCODE,CFLD_NAME,CCODE_NO,CDISCREP,crltfield) VALUES ;
      ('N',IIF(ImpoCode.Type ='R','CVICSADJCD','CADJHNDLNG'),ImpoCode.Code,ImpoCode.DESC,'N')
    ENDIF
  ENDSCAN
  SELECT CODES
  =gfTableUpdate()
  If USED('CODES')
    =gfCloseTable('CODES')
  ENDIF 
ENDSCAN
oAriaApplication.ActiveCompanyId = lcOldComp
oAriaApplication.GetCompanyInformation(lcOldComp)
Use In 'SYCCOMP_A'
