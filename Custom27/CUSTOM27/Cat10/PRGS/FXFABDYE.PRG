SET DATE TO AMER

SELECT 0
USE fabric.dbf SHARED
SET ORDER TO TAG Fabric 

SELECT 0
USE fabdye.dbf EXCL
SET ORDER TO TAG Fabdye 
ZAP
SCATTER MEMVAR MEMO BLANK

lcOldFile=getfile('dbf','select 2.6 fabric','Select')
SELECT 0
USE (lcOldFile) AGAIN ALIAS FAB26 Shared 
SET ORDER TO TAG FABRIC

SELECT 0
USE MATINVJL excl
SET ORDER TO TAG MATINVJL
ZAP

SELECT 0
USE CODES SHARED
SET ORDER TO TAG Ccode_no

SELECT 0
USE bomcost.dbf SHARED
SET ORDER TO TAG Bomcost 
=AFIELDS(laFileStru)
CREATE CURSOR "lcTmpCst" FROM ARRAY laFileStru
INDEX ON item+iclr+cwarecode+DTOS(DTRANDATE)+cisession TAG "lcTmpCst"

***-get the default adjustment reason from codes fiel

lcGVr_AdjR = ''
lcGVr_ARRf = ''
SELECT CODES
****cdefcode+cfld_name
IF SEEK('D'+'CADJREASON')
  lcGVr_AdjR = CODES.cCode_No
  IF SEEK('N'+'CADJREASON')
    SCAN REST WHILE cDefCode = 'N' AND  cFld_Name = 'CADJREASON' FOR  cCode_No = lcGVr_AdjR
      IF cRltd_nam = 'GLACCOUNT' 
        lcGVr_ARRf = cRltd_Vlu
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

SELECT FABRIC
LOCATE
SCAN
  WAIT WINDOW Fabric.Fabric+Fabric.Color NOWAIT 
  IF SEEK(FABRIC.FABRIC+FABRIC.COLOR,'FAB26')
    REPLACE Fabric.OnHand WITH MAX(FAB26.OnHand,0)
  ENDIF
  m.Fabric     = Fabric.Fabric
  m.Color      = Fabric.Color
  m.Dyelot     = ''
  m.OnHand     = Fabric.OnHand
  m.Usage      = Fabric.Usage
  m.OnOrder    = Fabric.OnOrder
  m.OnRet      = Fabric.OnRet
  m.nMatWip    = Fabric.nMatWip
  m.cWareCode  = 'DEFWHE'
  m.Gl_Link    = Fabric.Link_code
  m.nFAve_Cost = Fabric.nFAve_cost
  m.nStkVal    = Fabric.nStkVal
  m.nAveCstBuy = Fabric.nAveCstBuy
  INSERT INTO FABDYE FROM MEMVAR

*  DO CASE
*    CASE SUBSTR(FABRIC.Fabric,1,1) = "L"
*      lcKey2Sek = "L"
*    CASE SUBSTR(FABRIC.Fabric,1,1) = "T"
*      lcKey2Sek = "T"
*    OTHERWISE
*      lcKey2Sek = "F"
*  ENDCASE
*  lcKey2Sek = lcKey2Sek + SUBSTR(CITEM,1,3)+ SUBSTR(CITEM,5,7)+ SUBSTR(CITEM,13)+SUBSTR(Color,1,3)
*  *--- type+season+style_num+patt_num+color
*  IF SEEK(lcKey2Sek,'ITEM')
   IF OnHand >  0
    SELECT MATINVJL
    APPEND BLANK
    REPLACE CTRN_SEQ   WITH '000000',;
            CFABRIC    WITH FABRIC.Fabric,;
            CCOLOR     WITH FABRIC.Color,;
            CWARECODE  WITH 'DEFWHE',;
            CDYELOT    WITH '',;
            DTRANDATE  WITH CTOD('09/04/2001'),;
            CTRANTYPE  WITH '2',;
            CTRAN      WITH '000000',;
            CRSESSION  WITH IIF(FABRIC.OnHand<0,'','000000'),;
            CISESSION  WITH IIF(FABRIC.OnHand<0,'000000',''),;
            NUNITCOST  WITH FABRIC.nfAve_Cost,;
            NRECEIVED  WITH IIF(FABRIC.OnHand<0,0,FABRIC.OnHand),;
            NISSUED    WITH IIF(FABRIC.OnHand<0,FABRIC.OnHand,0),;
            NUNTCSTBUY WITH FABRIC.nfAve_Cost,;
            REFERENCE  WITH 'Starting Balance',;
            CADJREASON WITH lcGVr_AdjR,;
            CGLMATADJ  WITH lcGVr_ARRf,;
            DPOSTDATE  WITH CTOD('09/04/2001'),;
            NSTKVAL    WITH (FABRIC.nfAve_Cost*FABRIC.OnHand)
  ENDIF
  IF SEEK(PADR(FABRIC.FABRIC,19)+FABRIC.COLOR,'BOMCOST')
    SELECT BOMCOST
    SCAN REST WHILE PADR(ITEM,7)+iClr = FABRIC.FABRIC+FABRIC.COLOR
      IF DTRANDATE  >  CTOD('09/04/2001')
        SCATTER MEMVAR MEMO
        SELECT lcTmpCst
        APPEND BLANK
        GATHER MEMVAR MEMO
        SELECT BOMCOST
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN

SELECT FABRIC
SCAN
  WAIT WINDOW FABRIC.FABRIC+FABRIC.COLOR NOWAIT
  IF SEEK(PADR(Fabric.Fabric,19)+Fabric.Color,'lcTmpCst')
    lnOldQty = Fabric.Onhand
    SELECT lcTmpCst
    SCAN REST WHILE ITEM+iClr = PADR(FABRIC.FABRIC,19)+FABRIC.COLOR
      SELECT MATINVJL
      APPEND BLANK
      REPLACE CTRN_SEQ   WITH lcTmpCst.cisession,;
              CFABRIC    WITH FABRIC.Fabric,;
              CCOLOR     WITH FABRIC.Color,;
              CWARECODE  WITH 'DEFWHE',;
              CDYELOT    WITH '',;
              DTRANDATE  WITH lcTmpCst.DTRANDATE,;
              CTRANTYPE  WITH '4',;
              CIMTYP     WITH lcTmpCst.CIMTYP,;
              CTRAN      WITH lcTmpCst.CTKTNO,;
              CRSESSION  WITH lcTmpCst.cRSession,;
              CISESSION  WITH lcTmpCst.cISession,;
              NUNITCOST  WITH lcTmpCst.nUnitCst,;
              NISSUED    WITH lcTmpCst.nTotQty,;
              NUNTCSTBUY WITH FABRIC.nfAve_Cost,;
              CtktNo     WITH lcTmpCst.CTKTNO,;
              cOprCode   WITH lcTmpCst.cOprCode,;
              cLotNo     WITH lcTmpCst.cLotNo,;
              DPOSTDATE  WITH lcTmpCst.DTRANDATE,;
              NSTKVAL    WITH (lcTmpCst.nUnitCst*lcTmpCst.nTotQty) -1 ,;
              nMprvsQty  WITH lnOldQty
      lnOldQty = lnOldQty - lcTmpCst.nTotQty
    ENDSCAN
    IF SEEK(Fabric.Fabric+Fabric.Color,'FABDYE')
      SELECT FABDYE
      REPLACE OnHand WITH lnOldQty
    ENDIF
    SELECT FABRIC
    REPLACE OnHand WITH lnOldQty
  ENDIF
ENDSCAN

