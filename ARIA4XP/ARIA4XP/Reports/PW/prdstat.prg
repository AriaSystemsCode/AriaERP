*:****************************************************************
*: Program file  : PRDSTAT
*: Program desc. : Production Status Report.
*: System        : Aria Apparel System - Version 40.
*: Module        : (PW)
*: Developer     : SABER A.Razek -  (SAB)
*: Date          : 06/18/2012
*: Tracking#     : E303196
*:****************************************************************
*:Modifications  :
*:****************************************************************

STORE '' TO lcOrdTkts

IF !USED('STYLE')
  gfOpenTable('STYLE', 'STYLE', 'SH', 'STYLE')
ENDIF
IF !USED('POSHDR')
  gfOpenTable('POSHDR', 'POSHDR', 'SH', 'POSHDR')
ENDIF
IF !USED('PWTRKDT')
  gfOpenTable('PWTRKDT', 'PWTRKDT', 'SH', 'PWTRKDT')
ENDIF
IF !USED('PWBUNDL')
  gfOpenTable('PWBUNDL', 'PWBUNDL', 'SH', 'PWBUNDL')
ENDIF
IF !USED('PEPERSON')
  gfOpenTable('PEPERSON', 'PEPERSON', 'SH', 'PEPERSON')
ENDIF
IF !USED('PEDEPART')
  gfOpenTable('PEDEPART', 'PEDEPART', 'SH', 'PEDEPART')
ENDIF
IF !USED('CUTPICK')
  gfOpenTable('CUTPICK', 'CUTPICK', 'SH', 'CUTPICK')
ENDIF
IF !USED('MFGOPRDT')
  gfOpenTable('MFGOPRDT', 'MFGOPRITME', 'SH', 'MFGOPRDT')
ENDIF
IF !USED('ORDHDR')
  gfOpenTable('ORDHDR', 'ORDHDR', 'SH', 'ORDHDR')
ENDIF

SELECT POSHDR
=gfSeek('')
SELECT PWTRKDT
=gfSeek('')
SELECT PWBUNDL
=gfSeek('')
SELECT PEPERSON
=gfSeek('')
SELECT PEDEPART
=gfSeek('')
SELECT CUTPICK
=gfSeek('')
SELECT MFGOPRDT
=gfSeek('')

=lfCreateTemp()
lcRpExp = lfChangeExp()

IF !lfCollectData()
  RETURN 
ENDIF

=lfGetForm()
=lfAdjustCRSettings()
USE IN (lcRpTmpNam)
DO gfdispre WITH EVALUATE('lcRpForm')


*:***************************************************************************************************************************
*														 END of Code.
*:***************************************************************************************************************************


*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Saber A.Razek (SAB)
*: Date      : 06/18/2012
*: Purpose   : Report When
*:*************************************************************
FUNCTION lfwRepWhen

IF !USED('MFGOPRDT')
  gfOpenTable('MFGOPRDT', 'MFGOPRITME', 'SH', 'MFGOPRDT')
ENDIF

SELECT MFGOPRDT
DIMENSION laFileStruct[1,6]

laFileStruct[1  ,1] = 'clotno'
laFileStruct[1  ,2] = 'C'
laFileStruct[1  ,3] = 2
laFileStruct[1  ,4] = 0

=gfCrtTmp(lcTmpLots,@laFileStruct,"CLOTNO",lcTmplots,.T.)

LOCAL lcLotNo
lcLotNo = ''

SELECT MFGOPRDT
IF gfseek('')
  SELECT MFGOPRDT
  SCAN
    IF ALLTRIM(CLOTNO) == ALLTRIM(lcLotNo)
      LOOP
    ENDIF
    
    SCATTER MEMO MEMVAR
    INSERT INTO (lcTmpLots) FROM MEMVAR
    lcLotNo = CLOTNO
  ENDSCAN 
ENDIF


ENDFUNC

*:*************************************************************
*: Name      : lfGetForm
*: Developer : Saber A.Razek (SAB)
*: Date      : 06/18/2012
*: Purpose   : Function to get form name and sorting
*:*************************************************************
PROCEDURE lfGetForm

DO CASE 
  CASE lcRpSort1 = 'O' AND lcRpSort2 = 'B'
    lcRpForm = 'PrdOrdBn'
    SELECT (lcRpTmpNam)
    SET ORDER TO OrdBundle  && ORDER+CUTTKT+CLOTNO+SIZEDESC+CBUNDLE
  CASE lcRpSort1 = 'O' AND lcRpSort2 = 'D'
    lcRpForm = 'PrdOrdOp'
    SELECT (lcRpTmpNam)
    SET ORDER TO OrdOper    && ORDER+CUTTKT+CLOTNO+SIZEDESC+COPRCODE
  CASE lcRpSort1 = 'C' AND lcRpSort2 = 'B'
    lcRpForm = 'PrdOrdBn'
    SELECT (lcRpTmpNam)
    SET ORDER TO TktBundle  && CUTTKT+CLOTNO+SIZEDESC+CBUNDLE
  CASE lcRpSort1 = 'C' AND lcRpSort2 = 'D'
    lcRpForm = 'PrdOrdOp'
    SELECT (lcRpTmpNam)
    SET ORDER TO TktOpr     && CUTTKT+CLOTNO+SIZEDESC+COPRCODE
  CASE lcRpSort1 = 'E'    
    lcRpForm = 'ProdEmp'
    SELECT (lcRpTmpNam)
    SET ORDER TO Employee   && CPERSON_ID+CLOTNO+CBUNDLE
ENDCASE

ENDPROC

*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Saber A.Razek (SAB)
*! Date      : 06/18/2011
*! Purpose   : Procedure to create temp.file 
*!*************************************************************
PROCEDURE lfCreateTemp

SELECT PWTRKDT
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru, 1)
DIMENSION laFileStru[lnFileStru+39, 18]
lnIndx = lnFileStru

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'ORDER'
laFileStru[lnIndx, 2] = 'C'
laFileStru[lnIndx, 3] = 6
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'ACCOUNT'
laFileStru[lnIndx, 2] = 'C'
laFileStru[lnIndx, 3] = 5
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'ORDCOMPD'
laFileStru[lnIndx, 2] = 'D'
laFileStru[lnIndx, 3] = 8
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'ISSUDATE'
laFileStru[lnIndx, 2] = 'D'
laFileStru[lnIndx, 3] = 8
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'COMPDATE'
laFileStru[lnIndx, 2] = 'D'
laFileStru[lnIndx, 3] = 8
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'STYLDESC'
laFileStru[lnIndx, 2] = 'C'
laFileStru[lnIndx, 3] = 60
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'SIZEDESC'
laFileStru[lnIndx, 2] = 'C'
laFileStru[lnIndx, 3] = 6
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'ISSUEQTY'
laFileStru[lnIndx, 2] = 'N'
laFileStru[lnIndx, 3] = 6
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'OPERDESC'
laFileStru[lnIndx, 2] = 'C'
laFileStru[lnIndx, 3] = 30
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'EMPNAME'
laFileStru[lnIndx, 2] = 'C'
laFileStru[lnIndx, 3] = 30
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'EmpDepID'
laFileStru[lnIndx, 2] = 'C'
laFileStru[lnIndx, 3] = 6
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'EmpDeptNam'
laFileStru[lnIndx, 2] = 'C'
laFileStru[lnIndx, 3] = 30
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'BNDQTY'
laFileStru[lnIndx, 2] = 'N'
laFileStru[lnIndx, 3] = 6
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'RJCQTY'
laFileStru[lnIndx, 2] = 'N'
laFileStru[lnIndx, 3] = 6
laFileStru[lnIndx, 4] = 0

lnIndx = lnIndx + 1
laFileStru[lnIndx, 1] = 'ACTQTY'
laFileStru[lnIndx, 2] = 'N'
laFileStru[lnIndx, 3] = 6
laFileStru[lnIndx, 4] = 0

FOR i = 1 TO 8
  lnIndx = lnIndx + 1
  lcI = ALLTRIM(STR(i))
  laFileStru[lnIndx, 1] = 'NUMOFBNDL' + lcI
  laFileStru[lnIndx, 2] = 'N'
  laFileStru[lnIndx, 3] = 6
  laFileStru[lnIndx, 4] = 0
ENDFOR
FOR i = 1 TO 8
  lnIndx = lnIndx + 1
  lcI = ALLTRIM(STR(i))
  laFileStru[lnIndx, 1] = 'ISSUQTY' + lcI
  laFileStru[lnIndx, 2] = 'N'
  laFileStru[lnIndx, 3] = 6
  laFileStru[lnIndx, 4] = 0
ENDFOR
FOR i = 1 TO 8
  lnIndx = lnIndx + 1
  lcI = ALLTRIM(STR(i))
  laFileStru[lnIndx, 1] = 'RJCTQTY' + lcI
  laFileStru[lnIndx, 2] = 'N'
  laFileStru[lnIndx, 3] = 6
  laFileStru[lnIndx, 4] = 0
ENDFOR

FOR lnLoop = 1 TO 39
  STORE '' TO laFileStru[lnFileStru+lnLoop,  7], laFileStru[lnFileStru+lnLoop,  8], ;
              laFileStru[lnFileStru+lnLoop,  9], laFileStru[lnFileStru+lnLoop, 10], ;
              laFileStru[lnFileStru+lnLoop, 11], laFileStru[lnFileStru+lnLoop, 12], ;
              laFileStru[lnFileStru+lnLoop, 13], laFileStru[lnFileStru+lnLoop, 14], ;
              laFileStru[lnFileStru+lnLoop, 15], laFileStru[lnFileStru+lnLoop, 16]
  STORE 0 TO  laFileStru[lnFileStru+lnLoop, 17], laFileStru[lnFileStru+lnLoop, 18]
ENDFOR
=gfCrtTmp(lcRpTmpNam, @laFileStru)
SELECT (lcRpTmpNam)
INDEX ON ORDER+CUTTKT+CLOTNO+SIZEDESC+CBUNDLE  TAG 'OrdBundle'
INDEX ON ORDER+CUTTKT+CLOTNO+SIZEDESC+COPRCODE TAG 'ORDOPER'
INDEX ON CUTTKT+CLOTNO+SIZEDESC+CBUNDLE        TAG 'TKTBUNDLE'
INDEX ON CUTTKT+CLOTNO+SIZEDESC+COPRCODE       TAG 'TKTOPR'
INDEX ON CPERSON_ID+CLOTNO+CBUNDLE             TAG 'EMPLOYEE'

ENDPROC


*!*************************************************************
*! Name       : lfCollectData
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Procedure to make replenishment table have 14 size per line
*! Parameters : 
*!*************************************************************
PROCEDURE lfCollectData

*- Fill PW Tracking Details Table
SELECT PWTRKDT
SET RELATION TO
SET RELATION TO 'PU'+PWTRKDT.CUTTKT INTO POSHDR
SET RELATION TO PWTRKDT.STYLE INTO STYLE ADDITIVE
SET RELATION TO PWTRKDT.CPERSON_ID INTO PEPERSON ADDITIVE
SET RELATION TO PWTRKDT.CUTTKT+PWTRKDT.CBUNDLE INTO PWBUNDL ADDITIVE
SET RELATION TO 'M'+PWTRKDT.CUTTKT+PWTRKDT.MFGCODE+PWTRKDT.CLOTNO+'10001'+PWTRKDT.STYLE INTO MFGOPRDT ADDITIVE
SELECT PEPERSON
SET RELATION TO PEPERSON.CDEPTID INTO PEDEPART  && ADDITIVE

lcKey    = ''
lcTktKey = ''

SELECT PWTRKDT
SCAN FOR &lcRpExp.  
  FOR indx = 1 TO 8
    lcIndx = ALLTRIM(STR(indx))
    m.NUMOFBNDL&lcIndx. = IIF(lcTktKey <> PWTRKDT.CUTTKT, lfGetNumOfBndls(PWTRKDT.CUTTKT, lcIndx), 0)
    m.ISSUQTY&lcIndx.   = IIF(lcTktKey <> PWTRKDT.CUTTKT, lfGetIssuedQty(PWTRKDT.CUTTKT, lcIndx), 0)
    m.RJCTQTY&lcIndx.   = IIF(lcTktKey <> PWTRKDT.CUTTKT, lfGetRejectedQty(PWTRKDT.CUTTKT, lcIndx), 0)
  ENDFOR  
  lcTktKey = PWTRKDT.CUTTKT
  
  IF lcKey <> PWTRKDT.CUTTKT+PWTRKDT.MFGCODE+PWTRKDT.COPRCODE+PWTRKDT.CPERSON_ID+PWTRKDT.CBUNDLE
    SCATTER MEMVAR MEMO
    SELECT (lcRpTmpNam)
    APPEND BLANK
    IF gfSeek('1'+PWTRKDT.CUTTKT, 'CUTPICK')
      m.ORDER    = CUTPICK.Order
      IF gfSeek('O'+CUTPICK.Order, 'ORDHDR')
        m.ACCOUNT  = ORDHDR.Account
        m.ORDCOMPD = ORDHDR.Complete
      ENDIF
    ELSE
      m.ORDER    = ''
      m.ACCOUNT  = ''
      m.ORDCOMPD = {}
    ENDIF    
    m.ISSUDATE   = POSHDR.Entered
    m.COMPDATE   = POSHDR.Complete
    m.STYLDESC   = STYLE.Desc1
    m.SIZEDESC   = PWBUNDL.LotSize
    m.ISSUEQTY   = EVALUATE('MFGOPRDT.nLotQty'+SUBSTR(PWBUNDL.LotSize, 1, 1))
    m.OPERDESC   = gfCodDes(PWTRKDT.MFGCode, 'MFGCODE')
    m.EMPNAME    = PEPERSON.cName
    m.EmpDepID   = PEPERSON.cDeptID
    m.EmpDeptNam = PEDEPART.cDeptName
    GATHER MEMVAR MEMO
  ENDIF  
  lcKey = PWTRKDT.CUTTKT+PWTRKDT.MFGCODE+PWTRKDT.COPRCODE+PWTRKDT.CPERSON_ID+PWTRKDT.CBUNDLE
  
  DO CASE
    CASE PWTRKDT.Type = 'I'
      REPLACE &lcRpTmpNam..BNDQTY WITH PWTRKDT.nTotQty
    CASE PWTRKDT.Type = 'Q'
      REPLACE &lcRpTmpNam..RJCQTY WITH PWTRKDT.nTotQty
    CASE PWTRKDT.Type = 'C'
      REPLACE &lcRpTmpNam..ACTQTY WITH PWTRKDT.nTotQty
  ENDCASE    
ENDSCAN

ENDPROC


*!*************************************************************
*! Name       : lfGetNumOfBndl
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Function to Get Number of Bundles per size
*!*************************************************************
FUNCTION lfGetNumOfBndl
LPARAMETERS lcBndlNum

LOCAL lnNumOfBnld, lnAlias
lnAlias = SELECT()

IF !USED('PWBUNDL_A')
  gfOpenTable('PWBUNDL', 'PWBUNDL', 'SH', 'PWBUNDL_A')
ENDIF
SELECT PWBUNDL_A
=gfSeek('')

SELECT COUNT(CBUNDLE) FROM PWBUNDL_A WHERE CUTTKT = PWTRKDT.CutTkt AND CLOTNO = PWTRKDT.cLotNo AND ITEM = PWTRKDT.Style AND LOTSIZE Like lcBndlNum+'%' INTO ARRAY Result
lnNumOfBnld = Result[1]

SELECT (lnAlias)
RETURN lnNumOfBnld

ENDFUNC


*!*************************************************************
*! Name       : lfChangeExp
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Function to Change Filter Expression
*!*************************************************************
FUNCTION lfChangeExp
LOCAL lcNewExp, lcOrdExp, lnAlias
IF OCCURS('ORDER', UPPER(lcRpExp)) < 1
  lcNewExp = lcRpExp
  RETURN lcNewExp
ENDIF 

lnAlias = SELECT(0)
lcOrdExp = ''

lnOccur = OCCURS(' AND', lcRpExp) + 1
IF lnOccur >= 1
  lcNewExp = ''
  FOR lnCount = 1 TO lnOccur
    lnStart  = IIF(lnCount = 1, 1, ATC(' AND', lcRpExp, lnCount-1) + 5)
    lnEnd    = IIF(lnCount = lnOccur, LEN(lcRpExp), ATC(' AND', lcRpExp, lnCount))
    lnLength = lnEnd - lnStart + IIF(lnCount = lnOccur, 1, 0)
    lcTake = SUBSTR(lcRpExp, lnStart, lnLength)
    IF ATC('ORDER', lcTake) > 0
      lcOrdExp = IIF(EMPTY(lcOrdExp), lcTake, lcOrdExp + ' AND ' + lcTake)      
    ELSE
      lcNewExp = IIF(EMPTY(lcNewExp), lcTake, lcNewExp + ' AND ' + lcTake)
    ENDIF    
  ENDFOR
ELSE
  lcNewExp = lcRpExp
ENDIF

IF !EMPTY(lcOrdExp)
  DIMENSION laStruArr[1, 4]
  laStruArr[1  ,1] = 'cTktNo'
  laStruArr[1  ,2] = 'C'
  laStruArr[1  ,3] = 6
  laStruArr[1  ,4] = 0
  
  lcOrdTkts = gfTempName()
  CREATE CURSOR (lcOrdTkts) FROM ARRAY laStruArr
  INDEX ON CTKTNO TAG (lcOrdTkts)
  
  SELECT CUTPICK
  SCAN FOR &lcOrdExp. AND TRANCD = '1'
    SELECT (lcOrdTkts)
    APPEND BLANK
    REPLACE cTktNo WITH CUTPICK.cTktNo
  ENDSCAN  
  lcNewExp = lcNewExp + IIF(EMPTY(lcNewExp), "", " AND ") + "SEEK(CUTTKT, '"+ lcOrdTkts +"')"
ENDIF

SELECT (lnAlias)
RETURN lcNewExp

ENDFUNC


*!*************************************************************
*! Name       : lfGetNumOfBndls
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Function to Get Number of bundles per size
*!*************************************************************
FUNCTION lfGetNumOfBndls
LPARAMETERS lcTktNum, lcSize
LOCAL lnAlias
lnAlias = SELECT()

SELECT COUNT(DISTINCT cBundle) FROM PWTRKDT WHERE CutTkt = lcTktNum AND nQty&lcSize. > 0 INTO ARRAY Result

SELECT(lnAlias)
RETURN IIF(ISNULL(Result[1]), 0, Result[1])

ENDFUNC


*!*************************************************************
*! Name       : lfGetIssuedQty
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Function to Get Issued quatity per size
*!*************************************************************
FUNCTION lfGetIssuedQty
LPARAMETERS lcTktNum, lcSize
LOCAL lnAlias
lnAlias = SELECT()

SELECT SUM(nQty&lcSize.) FROM (SELECT DISTINCT Cuttkt, cBundle, nQty&lcSize. FROM PWTRKDT WHERE CutTkt = lcTktNum AND Type = 'I') AS A INTO ARRAY Result

SELECT(lnAlias)
RETURN IIF(ISNULL(Result[1]), 0, Result[1])

ENDFUNC


*!*************************************************************
*! Name       : lfGetRejectedQty
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Function to Get Rejected quatity per size
*!*************************************************************
FUNCTION lfGetRejectedQty
LPARAMETERS lcTktNum, lcSize
LOCAL lnAlias
lnAlias = SELECT()

SELECT SUM(nQty&lcSize.) FROM PWTRKDT WHERE CutTkt = lcTktNum AND Type = 'Q' INTO ARRAY Result

SELECT(lnAlias)
RETURN IIF(ISNULL(Result[1]), 0, Result[1])

ENDFUNC


***************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Saber Saber(SAB)
*! Date      : 09/29/2011
*! Purpose   : To set the report data files and parameters
*!*************************************************************
PROCEDURE lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[6, 2]
 
loOGScroll.cCROrientation ='P'
loOGScroll.lcOGLastForm   = lcRpForm
loOGScroll.lcLogoPath     = ""
  
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcRpTmpNam + ".DBF"
*CompanyName,UserName,ReportName,Layout,SortBy,OpTitle,SysDate
LOCAL lnI 
lnI  = 0
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'ReportName'
loOgScroll.laCRParams[lnI ,2] = "Production Status Report"
   
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'Layout'
loOgScroll.laCRParams[lnI, 2] = lcRpFormat
  
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'SortBy'
loOgScroll.laCRParams[lnI, 2] = lcRpSort1
  
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'OpTitle'
loOgScroll.laCRParams[lnI, 2] = ""
  
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'SysDate'
loOgScroll.laCRParams[lnI, 2] = oAriaApplication.SYSTEMDATE 

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'OrderBy'
loOgScroll.laCRParams[lnI, 2] = lcRpSort2

ENDPROC