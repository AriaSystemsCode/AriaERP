*:***************************************************************************
*: Program file  : POSTYPF
*: Program desc. : Print PO, Contract and Return PO
*: For Report    : POSTYPOF.FRX (C130884)
*: System        : Aria Advantage Series ARIA4XP  
*: Module        : Purchase Order (PO) 
*: Developer     : Mariam Mazhar (MMT)
*:***************************************************************************
STORE SPACE(0) TO lcCompMail 
llPoDisp = .F.
*--check if there is any cancelled qty
lcType =IIF(INLIST(lcRpForm,'P','R'),'P', lcRpForm )
lcTable ="POSLN,POSHDR"
lcSelFld = "  POSLN.*  "
lcSelCond ="  POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='5'  AND  POSLN.CSTYTYPE ='"+lcType+"'   "
STORE SPACE(0) TO lcTermsCond
IF !EMPTY(loogscroll.lcRpSqlExp)
  lcSelCond =" POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND  POSLN.CSTYTYPE ='"+lcType+"' AND POSLN.TRANCD ='5'  AND    " + loogscroll.lcrpsqlexp
  lcTable ="POSLN,POSHDR"
ELSE 
  lnPos = ASCAN(loOGScroll.laogFxflt,'POSHDR.PO')
  IF lnPos <> 0 
    lnPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnPos,1)
    lcOrders = IIF(EMPTY(loOGScroll.laogFxflt[lnPos,6]),'',loOGScroll.laogFxflt[lnPos,6])
    llFirst = .T.
    lcSelOrd =""
    IF !EMPTY(lcOrders)
      SELECT &lcOrders 
      LOCATE 
      IF !EOF()
        lcCurName = lcOrders 
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
          IF (RECCOUNT() > 0) 
            lcSQLOrder = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO')
            lcSelCond ="POSLN.CBUSDOCU ='"+lcRpForm+"'  AND  POSLN.CSTYTYPE ='"+lcType+"'  AND POSLN.TRANCD ='5'  AND  POSLN.PO = "+lcSQLOrder+'.PO'
            lcTable = 'POSLN INNER JOIN '+lcSQLOrder +' ON posln.po ='+lcSQLOrder+'.PO'
          ENDIF 
        ENDIF 
      ELSE
        lcSelCond ="  POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='5'  AND  POSLN.CSTYTYPE ='"+lcType+"'   "
        lcTable ="POSLN,POSHDR"
      ENDIF  
    ENDIF   
  ELSE 
    lcSelCond =" POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='5'   AND  POSLN.CSTYTYPE ='"+lcType+"'    "
    lcTable ="POSLN,POSHDR"
  ENDIF 
ENDIF 

lcSqlStatment   = "SELECT  " + lcSelFld + "  FROM " + lcTable + IIF(TYPE('lcSelCond') = 'C' AND !EMPTY(lcSelCond)," WHERE " + lcSelCond ,"")
lcPosln = loogscroll.gfTempName()


lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcPosln ,'POSLN',oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  SELECT (lcPOSLN)
  LOCATE
  IF !EOF()
    SCAN
      IF SEEK(CBUSDOCU+CSTYTYPE+PO+CWARECODE+ACCOUNT+STORE+CINVTYPE+STYLE+TRANCD,'POSLN')
        SELECT POSLN 
        REPLACE QTY1 WITH QTY1 - &lcPOSLN..QTY1,;
                QTY2 WITH QTY2 - &lcPOSLN..QTY2,;
                QTY3 WITH QTY3 - &lcPOSLN..QTY3,;
                QTY4 WITH QTY4 - &lcPOSLN..QTY4,;
                QTY5 WITH QTY5 - &lcPOSLN..QTY5,;
                QTY6 WITH QTY6 - &lcPOSLN..QTY6,;
                QTY7 WITH QTY7 - &lcPOSLN..QTY7,;
                QTY8 WITH QTY8 - &lcPOSLN..QTY8,
                TOTQTY WITH TOTQTY - &lcPOSLN..TOTQTY
         SELECT (lcPOSLN)
       ENDIF           
    ENDSCAN 
  ENDIF 
ENDIF 

= gfOpenFile(oAriaApplication.DataDir+'ORDHDR',oAriaApplication.DataDir+'ORDHDR','SH')
= gfOpenFile(oAriaApplication.DataDir+'NOTEPAD',oAriaApplication.DataDir+'NOTEPAD','SH','NOTEPAD2')

loogScroll.cCROrientation = 'L'
lcSqlCommand="SELECT SYCCOMP.CCOM_NAME,SYCCOMP.CCOM_PHON,cCom_Fax,caddress1,caddress2,caddress3,caddress4,caddress5,caddress6,ccont_code "
lcSqlCommand=lcSqlCommand+"  FROM SYCCOMP WHERE CCOMP_ID ='"+OARIAAPPLICATION.ACTIVECOMPANYID+"'  "
LOCAL lnResult
lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlCommand,"",lcCompInfo,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
IF lnResult = 1
  SELECT &lcCompInfo
  lcCompName = cCom_Name
  lcCompPhon = cCom_Phon              && Variable to hold the Company Phone
  lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
  lcCompFax  = TRANSFORM(cCom_Fax,'@R '+lcPhonPict)               && Variable to hold the Company Fax
  lcCompMail = gfGetMemVar('M_COMMAIL ',oAriaApplication.ActiveCompanyID)&& Variable to hold the Company Mail 

  * Get the company addresses
  laCompAdd[1]    = gfGetAdr(lcCompInfo , '' , '' , '' , 1)
  laCompAdd[2]    = gfGetAdr(lcCompInfo , '' , '' , '' , 2)
  laCompAdd[3]    = gfGetAdr(lcCompInfo , '' , '' , '' , 3)
  laCompAdd[4]    = gfGetAdr(lcCompInfo , '' , '' , '' , 4)
  laCompAdd[5]    = caddress5
  laCompAdd[6]    = TRANSFORM(lcCompPhon ,'@R '+lcPhonPict)

  * Pack the addresses array
  DO lfShiftArr WITH laCompAdd
ENDIF 

lcCutpickFlds = "CUTPICK.CTKTLINENO,CUTPICK.CORDLINE,CUTPICK.CTKTNO,CUTPICK.[ORDER],CUTPICK.style"
lcCutFiles  = "CUTPICK(INDEX = CUTPICK)"
lcCutCond = "Trancd = '1'"
STORE SPACE(0) TO lcCostItm, lcHead1, lcHead2,lcLotNo,lcStyMaj,lcPattrn,lcBOMTit
lcType =IIF(INLIST(lcRpForm,'P','R'),'P', lcRpForm )
lcMGroup  = 'CUTTKT'
lcInGroup = 'CUTTKT+TYP'
lcBOMTit  = 'Purchase Order#'
lcSelFld  = " CTKTBOM.ITEM,CTKTBOM.CUTTKT,TYP,CTKTBOM.CINVTYPE,CIMTYP,cCatgTyp,CTKTBOM.UNTQTY,CTKTBOM.UNTCOST,CTKTBOM.DYELOT"
lcSelFld = lcSelFld + ",[DESC],CTKTBOM.MFGCODE,CTKTBOM.ISSUE_QTY,CTKTBOM.REQ_QTY ,CTKTBOM.cadd_user,CTKTBOM.cedit_user,"
lcSelFld = lcSelFld + "UOM.CUOM_V AS UOM"
lcTable  = 'UOM,CTKTBOM,poshdr'
lcSelCond =" CIMTYP ='I' AND cCatgTyp IN ('F','T','S') AND  UOM.CUOMCODE = CTKTBOM.CUOMCODE AND  CTKTBOM.CUTTKT = POSHDR.PO AND POSHDR.CBUSDOCU ='"+lcRpForm+"'  AND  POSHDR.CSTYTYPE ='"+lcType+"'"
lnPos = ASCAN(loOGScroll.laogFxflt,'POSHDR.PO')
IF lnPos <> 0 
  lnPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnPos,1)
  lcOrders = IIF(EMPTY(loOGScroll.laogFxflt[lnPos,6]),'',loOGScroll.laogFxflt[lnPos,6])
  llFirst = .T.
  lcSelOrd =""
  IF !EMPTY(lcOrders)
    SELECT &lcOrders 
    LOCATE 
    IF !EOF()
      lcCurName = lcOrders 
      IF !EMPTY(lcCurName)
        SELECT &lcCurName    
        IF (RECCOUNT() > 0) 
          lcSQLOrder = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO')
          lcSelCond =" CIMTYP ='I'  AND  CUTTKT ="+ lcSQLOrder +".PO  AND cCatgTyp IN ('F','T','S') AND  UOM.CUOMCODE = CTKTBOM.CUOMCODE AND CTKTBOM.CUTTKT = POSHDR.PO AND POSHDR.CBUSDOCU ='"+lcRpForm+"'  AND  POSHDR.CSTYTYPE ='"+lcType+"'"
          lcTable = 'POSHDR,UOM,CTKTBOM INNER JOIN '+lcSQLOrder +' ON CTKTBOM.CUTTKT ='+lcSQLOrder+'.PO'
          lcCutFiles  = lcCutFiles + " INNER JOIN " + lcSQLOrder +" On Cutpick.ctktno = " + lcSQLOrder + ".po"
        ENDIF 
      ENDIF 
    ENDIF 
  ENDIF   
ENDIF 
IF lfopensql(lcSelFld ,lcTable,'CTKTBOM',lcSelCond)
  SELECT CTKTBOM
  SET ORDER TO TAG CTKTBOM
ENDIF 


lcCutCond = "Trancd = '2'"
IF lfopensql(lcCutpickFlds  ,lcCutFiles  ,'CUTPICK',lcCutCond)
  SELECT CUTPICK
  SET ORDER TO TAG CUTPICK
ENDIF 

SELECT POSLN
IF llExtendedSizeScale 
 DIMENSION laFileStru[1,18]
 SELECT PosLn
 =AFIELDS(laFileStru)
 lnFileStru = ALEN(laFileStru,1)
 lcOldFileStru = lnFileStru
 DIMENSION laFileStru[lnFileStru + 53,18]

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY9'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY10'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY11'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY12'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY13'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY14'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY15'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY16'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY17'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY18'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY19'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY20'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0

  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ1'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ2'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ3'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ4'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ5'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ6'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ7'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ8'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ9'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ10'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ11'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ12'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ13'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ14'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ15'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ16'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ17'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ18'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ19'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ20'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri1'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri2'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri3'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri4'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri5'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri6'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2
  
    lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri7'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri8'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri9'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri10'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri11'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri12'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri13'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri14'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri15'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri16'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri17'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri18'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri19'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'Pri20'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 2


  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'llEqualNot'
  laFileStru[lnFileStru,2] = 'L'
  laFileStru[lnFileStru,3] = 1
  laFileStru[lnFileStru,4] = 0

  
  FOR lnLoop = 1  TO  53
    STORE ' ' TO  laFileStru[lcOldFileStru +lnLoop,7],laFileStru[lcOldFileStru+lnLoop,8],;
                laFileStru[lcOldFileStru +lnLoop,9],laFileStru[lcOldFileStru+lnLoop,10],;
                laFileStru[lcOldFileStru+lnLoop,11],laFileStru[lcOldFileStru+lnLoop,12],;
                laFileStru[lcOldFileStru +lnLoop,13],laFileStru[lcOldFileStru+lnLoop,14],;
                laFileStru[lcOldFileStru +lnLoop,15],laFileStru[lcOldFileStru+lnLoop,16]
    STORE 0 TO    laFileStru[lcOldFileStru+lnLoop,17] ,laFileStru[lcOldFileStru+lnLoop,18]

  ENDFOR


  =gfCrtTmp(lcTmpPolns,@laFileStru,'po+cwarecode+SUBSTR(STYLE,1,16)',lcTmpPolns)

  SELECT(lcTmpPolns)
  INDEX on CBUSDOCU+CSTYTYPE+PO+CWARECODE+ACCOUNT+STORE+CINVTYPE+STYLE+TRANCD TAG 'POsLines'
  SET ORDER TO (lcTmpPolns)
  DECLARE laSizes[24,2]
  STORE " " TO lcLastSty,lcLastWar,lcLastAcc,lcLastStr
  lnExtWidth = gfGetMemVar('M_EXTWIDTH')
  STORE 0 TO lnCost1
  SELECT posln 
  SCAN 
    IF SEEK(PO+CWARECODE+SUBSTR(STYLE,1,16),lcTmpPolns)
      SELECT (lcTmpPolns)
      lnScalePos = ASCAN(laSizes,POSLN.SCALE)
      lnScalePos = IIF(lnScalePos<>0,ASUBSCRIPT(laSizes,lnScalePos,1),0)
      IF lnScalePos <> 0
        FOR lnI = lnScalePos TO MIN(lnScalePos+SCALE.CNT-1,20)
          lcI = ALLTRIM(STR(lnI,2))
          lcJ = STR(lnI-lnScalePos+1,1)
          REPLACE QTY&lcI. WITH POSLN.QTY&lcJ.
          REPLACE TOTQTY   WITH TOTQTY+POSLN.QTY&lcJ.
          REPLACE Pri&lcI. WITH POSLN.gros_price * (1 - (POSLN.disc_pcnt/ 100))
        ENDFOR
      ENDIF
    ELSE
      SCATTER MEMVAR MEMO
      =lfGetSizes()
      INSERT INTO (lcTmpPolns) FROM MEMVAR
      SELECT (lcTmpPolns)
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        REPLACE QTY&lcI. WITH 0
      ENDFOR
      REPLACE TOTQTY WITH 0
      lnScalePos = ASCAN(laSizes,POSLN.SCALE)
      lnScalePos = IIF(lnScalePos<>0,ASUBSCRIPT(laSizes,lnScalePos,1),0)
      IF lnScalePos <> 0
        FOR lnI = lnScalePos TO MIN(lnScalePos+SCALE.CNT-1,20)
          lcI = ALLTRIM(STR(lnI,2))
          lcJ = STR(lnI-lnScalePos+1,1)
          REPLACE QTY&lcI. WITH POSLN.QTY&lcJ.
          REPLACE TOTQTY   WITH TOTQTY+POSLN.QTY&lcJ.
          REPLACE Pri&lcI. WITH POSLN.gros_price * (1 - (POSLN.disc_pcnt/ 100))
        ENDFOR
      ENDIF
      FOR lnI = 1 TO 20
        lcI = ALLTRIM(STR(lnI,2))
        REPLACE SZ&lcI. WITH laSizes[lnI,1]
      ENDFOR
      SELECT POSLN
    ENDIF
  ENDSCAN
ENDIF  

IF !llExtendedSizeScale 
  lcSkipExpr =[POSLN]
  IF LLPRNTBOTH
    LCSKIPEXPR = [POSLN,&LCNOTELNS]
  ENDIF
  SELECT POSLN
  SET RELATION OFF INTO &LCNOTELNS
  SET RELATION TO PO + Style INTO CUTPICK ADDITIVE 
  SELECT POSHDR 
  SET SKIP TO 
  SET RELATION OFF INTO POSLN  
  SET RELATION TO lcRpForm + IIF(lcRpForm $ 'PR','P',lcRpForm)+POSHDR.PO INTO POSLN  ADDITIVE
  SET RELATION TO 'P'+ POSHDR.PO INTO NOTEPAD ADDITIVE 
  SET SKIP TO POSLN
*!*    SELECT NOTEPAD 
*!*    LOCATE 
*!*   
ELSE
  lcSkipExpr = [&lcTmpPolns]
  SELECT (lcTmpPolns)
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
  SET RELATION TO 'S'+ SUBSTR(style,1,lnMajSize) INTO Objlink ADDITIVE
  SET ORDER TO TAG 'POSLINES'
  *SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  SET RELATION TO PO + Style INTO CUTPICK ADDITIVE 
  SELECT POSHDR 
  SET SKIP TO 
  SET RELATION OFF INTO POSLN 
  SET RELATION TO lcRpForm+IIF(lcRpForm $ 'PR','P',lcRpForm)+POSHDR.PO INTO (lcTmpPolns) ADDITIVE
  SET RELATION TO 'P'+ POSHDR.PO INTO NOTEPAD ADDITIVE 
  SET SKIP TO &lcSkipExpr
  SELECT (lcTmpPolns)
  SCAN
    llNotEqual = .F.
    lcOldPri = Pri1
    FOR lnI = 2 TO 20
      lcI = ALLTRIM(STR(lnI,2))
      IF Pri&lcI. <> 0 AND lcOldPri <> Pri&lcI.
        llNotEqual = .T.
        EXIT 
      ENDIF   
    ENDFOR
    REPLACE llEqualNot WITH llNotEqual 
  ENDSCAN 
ENDIF 

lnCounRec = 0
SELECT POSHDR
lnPosTrm = ASCAN(loOGScroll.laogFxflt,'lnNotePadTemp')
IF lnPosTrm  <> 0 

  lnPosTrm  = ASUBSCRIPT(loOGScroll.laogFxflt,lnPosTrm ,1)
  lcTemp = IIF(EMPTY(loOGScroll.laogFxflt[lnPosTrm ,6]),'',loOGScroll.laogFxflt[lnPosTrm ,6])
  IF !EMPTY(lcTemp) AND USED(lcTemp) AND RECCOUNT(lcTemp) > 0
    SELECT POSHDR
    COUNT FOR SEEK(UPPER(POSHDR.CTRMC),lcTemp) TO lnCounRec 
    SET FILTER TO SEEK(UPPER(POSHDR.CTRMC),lcTemp)
    
  ENDIF 
ENDIF 

SELECT POSHDR  
LOCATE
IF lnCounRec > 0 
  WAIT WINDOW 'SELECTED ' + ALLTRIM(STR(lnCounRec)) +  ' RECORDS FOR REPORT' TIMEOUT 1
ELSE
  IF RECCOUNT('POSHDR')> 0
    WAIT WINDOW 'SELECTED ' + ALLTRIM(STR(RECCOUNT('POSHDR'))) +  ' RECORDS FOR REPORT' TIMEOUT 1
  ENDIF 
ENDIF   
DO gfDispRe WITH EVAL('lcFormName')
*!*************************************************************
*! Name      : lfGetSizes
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/21/2002
*! Purpose   : Get the sizes of the current style
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetSizes()
*!*************************************************************
*
FUNCTION lfGetSizes

PRIVATE lnSizeCnt, lnAlias, lnRecNo, lcDim1
lnSizeCnt = 0
laSizes = SPACE(0)
lnAlias = SELECT(0)
SELECT SCALE
lnRecNo = RECNO()
IF SEEK('S'+POSLN.SCALE)
  lcDim1 = CDIM1
  =SEEK('S'+SUBSTR(POSLN.SCALE,1,lnExtWidth))
  LOCATE REST WHILE Type+Scale+Prepak = 'S'+SUBSTR(POSLN.SCALE,1,lnExtWidth) FOR CDIM1 = lcDim1
  SCAN REST WHILE Type+Scale+Prepak = 'S'+SUBSTR(POSLN.SCALE,1,lnExtWidth) FOR CDIM1 = lcDim1
    FOR lnI = lnSizeCnt + 1 TO MIN(lnSizeCnt + CNT,20)
      lcI = STR(lnI-lnSizeCnt,1)
      laSizes[lnI,1] = EVALUATE('SZ'+lcI)
      laSizes[lnI,2] = SCALE
    ENDFOR
    IF lnSizeCnt + CNT >= 20
      EXIT
    ENDIF
    lnSizeCnt = MIN(lnSizeCnt + CNT,20)
  ENDSCAN
ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
SELECT (lnAlias)
*-- end of lfGetSizes.

*!*************************************************************
*! Name        : lfGetLL
*! Developer   : Mariam Mazhar (MMT)
*! Date        : 06/30/2004
*! Purpose     : Function to get record number of last line in the PO
*!*************************************************************
*! Calls       :
*!              Procedures : None
*!              Functions  : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetLL()
*!*************************************************************
FUNCTION lfGetLLL

PRIVATE lnAlias, lnRecNo
lnAlias = SELECT(0)
SELECT POSLN
lnRecNo = RECNO()
lcOldExp = CBUSDOCU+CSTYTYPE+PO
SET ORDER TO TAG POSLNW DESCENDING
LOCATE FOR CBUSDOCU+CSTYTYPE+PO+CWARECODE+ACCOUNT+STORE+STYLE+TRANCD = lcOldExp AND Trancd='1'
lnLstLn = RECNO()


SET ORDER TO TAG POSLNW ASCENDING
SELECT (lnAlias)

* Refresh the relation between POSHDR and POSLN
IF !EOF('POSHDR')
  GOTO RECNO('POSHDR') IN POSHDR
ENDIF

IF BETWEEN(lnRecNo,1,RECCOUNT('POSLN'))
  GOTO lnRecNo IN POSLN
ENDIF

IF !EOF('NOTEPAD')
  GOTO RECNO('NOTEPAD') IN NOTEPAD
ENDIF


IF llPrntBoth
  GOTO RECNO(lcNoteLns) IN &lcNoteLns
ENDIF 
SELECT (lnAlias)
RETURN ''

FUNCTION lfgetnot


lcTermsCond = ''
lcoldalias = SELECT(0)
SELECT notepad
LOCATE FOR TYPE + KEY = 'T'+PADR(UPPER(ALLTRIM(POSHDR.CTRMC)),40)
IF FOUND()
  SELECT(lcoldalias )
  lcTermsCond = ALLTRIM(NOTEPAD.MNOTES)
ELSE
  SELECT(lcoldalias)
  lcTermsCond =  ''
ENDIF 

