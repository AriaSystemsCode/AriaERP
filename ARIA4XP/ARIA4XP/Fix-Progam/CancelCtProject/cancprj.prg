* EXE to update the Status of the projects of cancelled Cutting tickets to be cancelled[T20111207.0046]
_SCreen.Visible = .F.
SET DELETED ON 
SET CPDIALOG OFF 
lcAria27SysFlsPath= GETDIR("", "Select Aria27 Sysfiles Directory")
  
IF EMPTY(lcAria27SysFlsPath) OR !DIRECTORY(lcAria27SysFlsPath) OR !FILE(ADDBS(lcAria27SysFlsPath)+"SYCCOMP.DBF")
  MESSAGEBOX("Invalid Aria27 Sysfiles Directory")
  RETURN .F.
ENDIF
lcAria27SysFlsPath = ADDBS(lcAria27SysFlsPath)
USE (lcAria27SysFlsPath +'SYCCOMP.DBF') SHARED IN 0
SELECT SYCCOMP
SCAN FOR lrunfroma4=.T.
  lcconnstr = lpcreateconnstr(ccondriver, cconserver, ccondbname, cconuserid, cconpaswrd)
  lnhandle = SQLSTRINGCONNECT(lcconnstr)
  IF lnhandle> 0
    lnCancCt= SQLEXEC(lnhandle,"Select POSHDR.PO from POSHDR INNER JOIN PMPRJHD ON pmprjhd.cprj_typ = 'C' and  PMPRJHD.CPRJ_ID = POSHDR.PO   Where POSHDR.Status ='X' and POSHDR.cStyType ='U' and POSHDR.CbusDocu = 'P' and pmprjhd.cprj_stts <> 'X'",'TmpCtHdr')
    IF lnCancCt > 0 
      SELECT TmpCtHdr
      SCAN
        lnPrjCt= SQLEXEC(lnhandle,"UPDATE PMPRJHD SET cprj_stts='X'  Where cprj_stts<> 'X' and pmprjhd.cprj_typ = 'C' and PMPRJHD.CPRJ_ID ='"+TmpCtHdr.PO+"'",'TmpPRJHdr')
      ENDSCAN 
      USE IN TmpCtHdr
    ENDIF
  ENDIF
  lnhandle = SQLDISCONNECT(lnhandle)
ENDSCAN
USE IN SYCCOMP
MESSAGEBOX('Updating Projects status is completed successfully')
FUNCTION lpCreateConnStr
 LPARAMETERS ccondriver, cconserver, ccondbname, cconuserid, cconpaswrd
 LOCAL lcconnstr
 lcconnstr = ""
 DO CASE
    CASE ccondriver='SQL'
       lcconnstr = "Driver={SQL Server};server="+ALLTRIM(cconserver)+";DATABASE="+ALLTRIM(ccondbname)+";uid="+ALLTRIM(cconuserid)+";pwd="+ALLTRIM(cconpaswrd)
    CASE ccondriver='FOX'
       lcconnstr = "Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB="+ALLTRIM(ccondbname)+";SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;"
 ENDCASE
 RETURN lcconnstr
ENDFUNC
