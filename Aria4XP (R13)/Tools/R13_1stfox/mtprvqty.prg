*-- MTPRVQTY : calculates MATINVJL.nMPrvSQty field
*-- Author   : Ahmed Amer
PARAMETERS lcDataDir
DIMENSION laFileName[1,2]

llErr   = .F.
lcError = ON('ERROR')

IF TYPE('lcDataDir') # 'C' OR EMPTY(lcDataDir)
  ON ERROR llErr = .T.
  CLOSE ALL
  llExtCall = .T.
  lcDataDir = GETDIR('','Select data directory')
ELSE
  llExtCall = .F.
ENDIF  

IF !EMPTY(lcDataDir)
  IF !FILE(lcDataDir+'MatInvJl.DBF')
    WAIT "The selected directory has not MatInvJl File." WINDOW
    RETURN
  ENDIF
ELSE
  RETURN
ENDIF  

IF llExtCall
  lcSysDir = SUBSTR(lcDataDir,1,AT("DBFS",lcdatadir)-1)+"SYSFILES\"
  USE (lcSysDir+"SyuStatc.DBF") IN 0 EXCLUSIVE
ENDIF

IF llErr
  WAIT "This program needs an exclusive use. One or more users are logged in." WINDOW
  ON ERROR &lcError
  RETURN
ENDIF

IF llExtCall
  USE (lcDataDir+'MatInvJl.DBF') IN 0 ORDER MatInvJl EXCLUSIVE
ELSE
  lcMatJour = gfTempName()
  =gfOpenFile(lcDataDir+'MatInvJl',lcDataDir+'MatInvJl','SH')
  SELECT MatInvJl
  PRIVATE laFileStru,laIndex,lnIndxNo
  STORE 0 TO lnIndxNo
  
  llNoThing = AFIELDS(laFileStru)
  IF ASCAN(laFileStru,'NMPRVSQTY') = 0
    lnI = ALEN(laFileStru,1)+1
    DIMENSION laFileStru[lnI,4]
    laFileStru[lnI,1] = 'nMPrvSQty'
    laFileStru[lnI,2] = 'N'
    laFileStru[lnI,3] = 12
    laFileStru[lnI,4] = 3
  ENDIF

  llMyOpn = gfSysOpen(gcSysHome+'SydIndex')
  SELECT SydIndex
  lcTag = ORDER()
  SET ORDER TO cFile
  IF SEEK('MATINVJL')
    SCAN REST WHILE cfile_nam = "MATINVJL"
      lnIndxNo = lnIndxNo + 1
      DIMENSION laIndx[lnIndxNo,2]
      laIndx[lnIndxNo,1] = cIndx_Exp
      laIndx[lnIndxNo,2] = cFile_Tag
    ENDSCAN
  ENDIF
  SET ORDER TO (lcTag)
  IF llMyOpn
    USE IN SydIndex
  ENDIF

  =gfCrtTmp(lcMatJour,@laFileStru,@laIndx)
  SELECT (lcMatJour)
  SET ORDER TO MatInvJl
  APPEND FROM (lcDataDir+'MatInvJl.DBF')
  USE IN  MatInvJl
ENDIF

IF llExtCall
  SELECT MatInvJl
ELSE
  SELECT (lcMatJour)
ENDIF
  
lnBal = 0
lcFabric   = cFabric
lcColor    = cColor
lcWareCode = cWareCode
lcDyelot   = cDyelot

SCAN WHILE cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = '' AND !llErr
  IF lcFabric+lcColor+lcWareCode+lcDyelot # cFabric+cColor+cWareCode+cDyelot
    lnBal = 0
    lcFabric   = cFabric
    lcColor    = cColor
    lcWareCode = cWareCode
    lcDyelot   = cDyelot
  ENDIF
  REPLACE nMPrvSQty WITH lnBal
  lnBal = lnBal + (nReceived - nIssued)
ENDSCAN

IF llErr
  WAIT "Field nMPrvSQty does not exist in file MatInvJl." WINDOW
ELSE  
  IF llExtCall
    WAIT "Field nMPrvSQty has been updated successfully." WINDOW
  ELSE
    ERASE  (lcDataDir+'MatInvJl.DBF')
    ERASE  (lcDataDir+'MatInvJl.CDX')
    SELECT (lcMatJour)
    COPY TO (lcDataDir+"MatInvJl.DBF") WITH CDX
    USE IN (lcMatJour)
    ERASE   (gcWorkDir+lcMatJour+'.DBF')
    ERASE   (gcWorkDir+lcMatJour+'.CDX')
  ENDIF
ENDIF

IF llExtCall
  CLOSE ALL
  ON ERROR &lcError
ENDIF