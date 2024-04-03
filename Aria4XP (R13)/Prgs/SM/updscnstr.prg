LPARAMETERS lcA4SysFiles
SET STEP ON

LOCAL lcReServer
lcReServer = ALLTRIM(SUBSTR(SYS(0), 1, AT('#', SYS(0))-1))
loActivator = CREATEOBJECT("Aria.Utilities.RemoteCall.AriaActivator")
loA5ariaEnvironment = loActivator.GetRemoteObject("Aria.Environment.AriaEnviromentVariables" , lcReServer, 1500)
= SQLSETPROP(0, "DispLogin", 3)
lcSysFlConnect = loA5ariaEnvironment.Aria50SystemFilesConnectionStringODBC
lnconnecthandle = SQLSTRINGCONNECT(lcsysflconnect)

lnsqlget = SQLEXEC(lnconnecthandle, "Select COUNT(*) AS ClCnt From Clients", "CLIENTSCNT")
IF lnsqlget > 0
  SELECT CLIENTSCNT
  LOCATE
  IF CLIENTSCNT.ClCnt < 3
    lnsqlget = SQLEXEC(lnconnecthandle, "Select 0 as lSelect,* From Clients  Order by CClientID", "CLIENTS")
    IF lnsqlget > 0
      SELECT CLIENTS
      SCAN
        WAIT WINDOW "Updating Client " + ALLTRIM(CLIENTS.CCLIENTNAME) TIMEOUT 2
        
        * Add Correct SydField Records  
        =lfAddA40Fields(ALLTRIM(CLIENTS.ARIA40SYS), ALLTRIM(CLIENTS.CCLIENTID))
        
        * Remove Duplications From Sydflfld
        =lfRemoveSydFlFldDupl(ALLTRIM(CLIENTS.ARIA40SYS), ALLTRIM(CLIENTS.CCLIENTID))
        
        * Fix SydIndex Unique Indexes
        =lfFixIndexs(ALLTRIM(CLIENTS.ARIA40SYS), ALLTRIM(CLIENTS.CCLIENTID))
        
        * Delete definition of BTCH_HDR and BTCH_DET views
        * Rename SCAN_BATCH_HEADER_T and SCAN_BATCH_DETAILS_T Tables         
        =lfDropScanIndexs(ALLTRIM(CLIENTS.ARIA40SYS), ALLTRIM(CLIENTS.CCLIENTID))
        
        * Update File Structure for 
        =lfUpdateScanStructure(ALLTRIM(CLIENTS.ARIA40SYS), ALLTRIM(CLIENTS.CCLIENTID))
        
        *- Append records from renamed SCAN_BATCH_HEADER_T and SCAN_BATCH_DETAILS_T tables to the new created tables
        =lfAppendScanRecords(ALLTRIM(CLIENTS.ARIA40SYS), ALLTRIM(CLIENTS.CCLIENTID))
        
      ENDSCAN
      WAIT WINDOW "Update Completed Successfully" TIMEOUT 2
    ENDIF
  ENDIF  
ENDIF



*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*-                          lfDropScanIndexs
*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
FUNCTION lfDropScanIndexs
LPARAMETERS lcSysFiles, lcClientID

IF !USED('SydFiles')
  USE ADDBS(lcSysFiles) + 'SydFiles' IN 0 SHARED
ENDIF
SELECT SydFiles
DELETE FOR INLIST(CFILE_NAM, 'BTCH_HDR', 'BTCH_DET')
USE IN SydFiles

IF !USED('SydFlFld')
  USE ADDBS(lcSysFiles) + 'SydFlFld' IN 0 SHARED
ENDIF
SELECT SydFlFld
DELETE FOR INLIST(CFILE_NAM, 'BTCH_HDR', 'BTCH_DET')
USE IN SydFlFld

IF !USED('SydIndex')
  USE ADDBS(lcSysFiles) + 'SydIndex' IN 0 SHARED
ENDIF
SELECT SydIndex
DELETE FOR INLIST(CFILE_NAM, 'BTCH_HDR', 'BTCH_DET')
USE IN SydIndex


IF !USED('SycComp')
  USE ADDBS(lcSysFiles) + 'SycComp' IN 0 SHARED
ENDIF

SELECT SycComp
SCAN FOR !DELETED()
  lcServer = ALLTRIM(SycComp.cConServer)
  lcDBName = ALLTRIM(SycComp.cConDBName)
  lcUser   = ALLTRIM(SycComp.cConUserID)
  lcPass   = ALLTRIM(SycComp.cConPaswrd)
  lcCompConnStr = "Driver={SQL Server};server=" + lcServer + ";DATABASE=" + lcDBName + ";UID=" + lcUser + ";PWD=" + lcPass
  lnConHndle    = SQLSTRINGCONNECT(lcCompConnStr)
  IF lnConHndle > 0
    lnRslt0 = SQLEXEC(lnConHndle, "SELECT COUNT(*) AS TblCnt FROM sys.objects where name IN ('SCAN_BATCH_DETAILS_T', 'SCAN_BATCH_HEADER_T')", "Result")
    IF lnRslt0 > 0
      SELECT RESULT
      LOCATE
      IF Result.TblCnt >= 2
        lnRslt1 = SQLEXEC(lnConHndle, "ALTER TABLE SCAN_BATCH_DETAILS_T DROP CONSTRAINT PK_SCAN_BATCH_DETAILS_T", "Result")
        lnRslt2 = SQLEXEC(lnConHndle, "ALTER TABLE SCAN_BATCH_DETAILS_T DROP CONSTRAINT FK_SCAN_BATCH_DETAILS_T_SCAN_BATCH_HEADER_T", "Result")
        lnRslt3 = SQLEXEC(lnConHndle, "ALTER TABLE SCAN_BATCH_HEADER_T DROP CONSTRAINT PK_SCAN_BATCH_HEADER_T", "Result")
        lnRslt4 = SQLEXEC(lnConHndle, "SP_RENAME SCAN_BATCH_DETAILS_T, xxSCAN_BATCH_DETAILS_T", "Result")
        lnRslt5 = SQLEXEC(lnConHndle, "SP_RENAME SCAN_BATCH_HEADER_T, xxSCAN_BATCH_HEADER_T", "Result")
        
        IF !(lnRslt4 > 0 AND lnRslt5 > 0)
          WAIT WINDOW "Couldn't Update Client " + lcClientID + " Database " + ALLTRIM(SycComp.cConDBName)
        ENDIF
      ENDIF
    ELSE
      WAIT WINDOW "Couldn't Update Client " + lcClientID + " Database " + ALLTRIM(SycComp.cConDBName)
    ENDIF    
  ELSE
    WAIT WINDOW "Error in Connecting to Client " + lcClientID + " Database " + ALLTRIM(SycComp.cConDBName)
  ENDIF
ENDSCAN

USE IN SycComp

ENDFUNC
*- End of lfDropScanIndexs


*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*-                          lfUpdateScanStructure
*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
FUNCTION lfUpdateScanStructure
LPARAMETERS lcA40SysFiles, lcClientID


USE SHARED ADDBS(lcA40SysFiles)+'SydFiles.dbf' IN 0 ORDER CFILE_NAM
USE SHARED ADDBS(lcA40SysFiles)+'SydFlFld.dbf' IN 0 ORDER CFILE_NAM
USE SHARED ADDBS(lcA40SysFiles)+'SydField.dbf' IN 0 ORDER CFLD_NAME
USE SHARED ADDBS(lcA40SysFiles)+'SydIndex.dbf' IN 0 ORDER CFILE

SydFiles = 'SydFiles'
SydFlFld = 'SydFlFld'
SydField = 'SydField'
SydIndex = 'SydIndex'

USE SHARED ADDBS(lcA40SysFiles)+'SycComp.dbf' ALIAS 'SycComp_A' IN 0
SELECT 'SycComp_A'
LOCATE
objRemoteDataAccess = CREATEOBJECT("remotedataaccess")
nDataSessionID = SET("Datasession")
SCAN FOR lRunFroma4=.T.
  lcCompID = cComp_ID
  SELECT 'SycComp_A' 
  IF EMPTY(ALLTRIM(CCONSERVER)) OR EMPTY(ALLTRIM(CCONDBNAME))
    LOOP
  ENDIF
    
  lcConnStr = oAriaApplication.mGenConstr()
  lnHandle = SQLSTRINGCONNECT(lcConnStr)
  IF lnHandle < 0
    LOOP 
  ENDIF
  SELECT SydFiles
  LOCATE
  SCAN FOR INLIST(cFile_Nam, 'SCAN_BATCH_HEADER_T', 'SCAN_BATCH_DETAILS_T')
    lcTable = cFile_Nam
    lcFile_Ttl = cFile_Ttl
    WAIT WINDOW TIMEOUT 1 "Update "+lcTable+"-"+ALLTRIM(lcFile_Ttl)+" In company "+lccompid+", Please Wait."
    
    SET KEY TO PADR(ALLTRIM(lcTable), oAriaApplication.FileW) IN (SydFlFld)
    objRemoteDataAccess.mCreateTable(lcCompID, lcTable, lcFile_Ttl, SydFlFld, sydfield, SydIndex, ndatasessionid, lnHandle, .F., lcconnstr)
    
    SET KEY TO PADR(ALLTRIM(lcTable), oAriaApplication.FileW) IN (SydIndex)
    objRemoteDataAccess.mCreateIndex(lcCompID, lcTable, lcFile_Ttl, SydIndex, ndatasessionid, lnhandle, lcconnstr)
    SELECT (SydIndex)
    SET KEY TO 
    SELECT (SydFlFld)
    SET KEY TO 
  ENDSCAN
  SELECT 'syccomp_A' 
ENDSCAN
RELEASE objremotedataaccess
USE IN 'syccomp_A' 

USE IN SydFiles
USE IN SydFlFld
USE IN SydField
USE IN SydIndex

ENDFUNC
*- End of lfUpdateScanStructure


*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*-                          lfAppendScanRecords
*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
FUNCTION lfAppendScanRecords
LPARAMETERS lcSysFiles, lcClientID

IF !USED('SycComp')
  USE ADDBS(lcSysFiles) + 'SycComp' IN 0 SHARED
ENDIF

SELECT SycComp
SCAN FOR !DELETED()
  lcServer = ALLTRIM(SycComp.cConServer)
  lcDBName = ALLTRIM(SycComp.cConDBName)
  lcUser   = ALLTRIM(SycComp.cConUserID)
  lcPass   = ALLTRIM(SycComp.cConPaswrd)
  lcCompConnStr = "Driver={SQL Server};server=" + lcServer + ";DATABASE=" + lcDBName + ";UID=" + lcUser + ";PWD=" + lcPass
  lnConHndle    = SQLSTRINGCONNECT(lcCompConnStr)
  IF lnConHndle > 0
    lnRslt1 = SQLEXEC(lnConHndle, "INSERT INTO SCAN_BATCH_HEADER_T (SCAN_BATCH_HEADER_KEY,BATCH,DESCRIPTION,DATE,[USER],STATUS,VENDOR"+;
                                  ",ACCOUNT,TRANSACTION_TYPE,TRANSACTION_NO,DADD_DATE,CADD_TIME,CADD_USER,CADD_VER,DEDIT_DATE,CEDIT_TIME"+;
                                  ",CEDIT_USER,CEDT_VER,DLOK_DATE,CLOK_TIME,CLOK_USER,LLOK_STAT,REC_NO) SELECT SCAN_BATCH_HEADER_KEY,"+;
                                  "LEFT(BATCH,6),DESCRIPTION,DATE,[USER],LEFT(STATUS,1),VENDOR,LEFT(ACCOUNT,5),TRANSACTION_TYPE,TRANSACTION_NO"+;
                                  ",DADD_DATE,CADD_TIME,CADD_USER,CADD_VER,DEDIT_DATE,CEDIT_TIME,CEDIT_USER,CEDT_VER,DLOK_DATE,LEFT(CLOK_TIME,8),"+;
                                  "CLOK_USER,LLOK_STAT,REC_NO FROM XXSCAN_BATCH_HEADER_T", "Result")
    lnRslt2 = SQLEXEC(lnConHndle, "INSERT INTO SCAN_BATCH_DETAILS_T (SCAN_BATCH_DETAILS_KEY,SCAN_BATCH_HEADER_KEY,LINE_NO,ITEM_NUMBER,"+;
                                  "CARTON_NUMBER,PACK_NUMBER,CSTYMAJOR,STYLE,SCALE,SIZE,QUANTITY,STATUS,REJECTION_REASON,REC_no)"+;                                  
                                  "SELECT SCAN_BATCH_DETAILS_KEY,SCAN_BATCH_HEADER_KEY,LINE_NO,ITEM_NUMBER,LEFT(CARTON_NUMBER,6),LEFT(PACK_NUMBER,6),CSTYMAJOR"+;
                                  ",STYLE,SCALE,LEFT(SIZE,2),QUANTITY,LEFT(STATUS,1),REJECTION_REASON,REC_no FROM xxSCAN_BATCH_DETAILS_T", "Result")
    IF !(lnRslt1 > 0 AND lnRslt2 > 0 )
      WAIT WINDOW "Couldn't Update Client " + lcClientID + " Database " + ALLTRIM(SycComp.cConDBName)
    ENDIF
  ELSE
    WAIT WINDOW "Error in Connecting to Client " + lcClientID + " Database " + ALLTRIM(SycComp.cConDBName)
  ENDIF
ENDSCAN

USE IN SycComp

ENDFUNC
*- End of lfAppendScanRecords



*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*-                          lfFixIndexs
*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
FUNCTION lfFixIndexs
LPARAMETERS lcSysFiles, lcClientID


IF !USED('R_SydIndex')
  USE lcSysFiles + 'R_SydIndex' IN 0 SHARED ALIAS R_SydIndex ORDER CFILE_NAM   && CFILE_NAM+CFILE_TAG
ELSE
  SELECT R_SydIndex
  SET ORDER TO CFILE_NAM   && CFILE_NAM+CFILE_TAG
ENDIF


LOCAL lcFileName, lcTagNam, lcIndx_Exp
lcFileName = ''
lcTagNam   = ''
lcIndx_Exp = ''

IF USED('SydIndex')
  USE IN SydIndex
ENDIF

=lfBackUpFoxTable(lcSysFiles, 'SydIndex')

USE ADDBS(lcSysFiles) + 'SydIndex' IN 0 SHARED ALIAS SydIndex ORDER CFILE_NAM   && CFILE_NAM+CFILE_TAG

SELECT R_SydIndex
LOCATE
SCAN 
  SCATTER MEMO MEMVAR
  SELECT SydIndex
  IF SEEK(R_SydIndex.CFILE_NAM+R_SydIndex.CFILE_TAG)
    REPLACE lUnique WITH R_SydIndex.lUnique
  ELSE
    APPEND BLANK
    GATHER MEMO MEMVAR
  ENDIF
ENDSCAN

SELECT R_SydIndex
LOCATE
SCAN 
  lcFileName = ALLTRIM(cFile_Nam)
  lcTagNam   = ALLTRIM(cFile_Tag)
  lcIndx_Exp = ALLTRIM(cIndx_Exp)
  
  =lfBackUpFoxTable(lcSysFiles, lcFileName)
  
  USE ADDBS(lcSysFiles) + lcFileName IN 0 EXCLUSIVE ALIAS (lcFileName)
  
  SELECT (lcFileName)
  IF !R_SydIndex.lUnique    
    TRY
      DELETE TAG (lcTagNam)
    CATCH
    ENDTRY
    INDEX ON &lcIndx_Exp. TAG (lcTagNam)
  ELSE
    INDEX ON &lcIndx_Exp. TAG (lcTagNam) UNIQUE
  ENDIF
  
  USE IN (lcFileName)
ENDSCAN

ENDFUNC
*- End of lfFixIndexs



*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*-                          lfAddA40Fields
*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
FUNCTION lfAddA40Fields
LPARAMETERS lcSysFiles, lcClientID

** Add A40 Fields Definitions to sydfield
SET DELETED ON
SET EXCLUSIVE OFF

lcPath = lcSysFiles

IF EMPTY(lcPath )
  RETURN
ENDIF
IF JUSTFNAME(lcPath ) <> 'SYDFIELD.DBF'
  RETURN
ENDIF

USE lcPath + 'R_SYDFIELD'
SET FILTER TO CVER = 'A40'
LOCATE

SELECT 0
USE (lcPath ) ORDER CFLD_NAME
SET FILTER TO CVER = 'A40'
LOCATE

SELECT R_SYDFIELD
SCAN
  IF !SEEK(CFLD_NAME,'SYDFIELD')
    SCATTER MEMVAR MEMO
    INSERT INTO SYDFIELD FROM MEMVAR
    WAIT WINDOW NOWAIT CFLD_NAME
  ENDIF 
ENDSCAN
WAIT CLEAR

ENDFUNC 
*- End of lfAddA40Fields


*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*-                          lfRemoveSydFlFldDupl
*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
FUNCTION lfRemoveSydFlFldDupl
LPARAMETERS lcSysFiles, lcClientID

SET DELETED ON
SET EXCLUSIVE OFF

lcPath = lcSysFiles

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

ENDFUNC 
*- End of lfRemoveSydFlFldDupl



************************************************************
*! Name      : lfGetDupl
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/01/2013
*! Purpose   : get duplicated lines
************************************************************
FUNCTION lfGetDupl
SELECT CFILE_NAM,CFLD_NAME,COUNT(*) AS CN FROM SYDFLFLD GROUP BY CFILE_NAM,CFLD_NAME HAVING CN>1 INTO CURSOR SYDFLFLD_DUPL

*- End of lfGetDupl.


*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*-                          lfBackUpFoxTable
*-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
FUNCTION lfBackUpFoxTable
LPARAMETERS lcFilePath, lcFileName

IF FILE(ADDBS(lcFilePath) + lcFileName + '.CDX')
  COPY FILE ADDBS(lcFilePath) + lcFileName + '.CDX' TO ADDBS(lcFilePath) + 'BKUP_' + lcFileName + '.CDX'
ENDIF 

IF FILE(ADDBS(lcFilePath) + lcFileName + '.DBF')
  COPY FILE ADDBS(lcFilePath) + lcFileName + '.DBF' TO ADDBS(lcFilePath) + 'BKUP_' + lcFileName + '.DBF'
ENDIF 

IF FILE(ADDBS(lcFilePath) + lcFileName + '.FPT')
  COPY FILE ADDBS(lcFilePath) + lcFileName + '.FPT' TO ADDBS(lcFilePath) + 'BKUP_' + lcFileName + '.FPT'
ENDIF 

ENDFUNC
*- End of lfBackUpFoxTable
