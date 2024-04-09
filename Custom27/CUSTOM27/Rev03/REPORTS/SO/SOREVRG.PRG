*:***************************************************************************
*: Program file  : SOREVRG.PRG 
*: Program desc. : Daily Order Register (for Revue)
*: For Report    : (SOREVRG.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Adel Mohmmed El Gazzar (ADEL)
*: Date          : 02/12/2002
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: ..C102504,1
*:***************************************************************************
*...

*===============NOTES=====================
* This report is to be run from 2 places :
* 1 Normal Aria27 OG
* 2 As an EXE fron Windows Schedule
*=========================================

IF !FILE('DEFCOMP.MEM')
  *--Initialize needed variables
  STORE '' TO laRpCmpCod,lcRpFrom,lcRpTo
  *--Get all companies information.
  DO lpGetComp
ENDIF

*--If run from outside Aria
IF TYPE('gcAct_Comp') = 'U'
  DO lpOutAria
ELSE
  DO lpInAria
ENDIF

DO lpClosFil

*!**************************************************************************
*! Name      : lpInAria
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : Run report from Inside Aria.
*!**************************************************************************
*! Example   : DO lpInAria
*!**************************************************************************
*..
PROCEDURE lpInAria

*--If the user changes companies, recollect data.
llOGFltCh = llOGFltCh OR (lcRpCmpExp <> lcOldComp) OR (lcOldComp <> lcRpCmpExp)
lcOldComp = lcRpCmpExp
*--Prepaire filter.
lcRpExp = lcRpExp + " AND cordtype+order = 'O' AND INLIST(STATUS,'O','H')"
*--Save default companies.
lcStyTitle = gfItemMask('HI')
llOGFltCh = .T.
IF llOGFltCh
  llOGFltCh = .F.
  IF USED(lcTempLine)
    IF RECCOUNT(lcTempLine) >0
      SELECT (lcTempLine)
      ZAP
    ENDIF
  ELSE
    *--Create the temp file.
    DO lpCrtTemp
  ENDIF
  IF !lfColData()
    *---Text 'NO RECORDS SELECTED FOR REPORT!'
    = gfModalGen('TRM00052B00000','DIALOG' )
    RETURN
  ENDIF
ENDIF
SELECT (lcTempLine)
LOCATE
IF EOF()
  *---Text 'NO RECORDS SELECTED FOR REPORT!'
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
lcOGPlatForm='WINDOWS '
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ENTERED'),1)
IF !EMPTY(laOGFxFlt[lnDatePos,6])
  lnSep    = ATC('|',laOGFxFlt[lnDatePos,6])
  lcRpFrom = IIF(lnSep>1,SUBSTR(laOGFxFlt[lnDatePos,6],1,lnSep-1),'')
  lcRpTo   = SUBSTR(laOGFxFlt[lnDatePos,6],lnSep+1)
ENDIF
IF llRpDefCm
  SAVE ALL LIKE l?Rp* TO "DEFCOMP.MEM"
ENDIF
*adel
DELETE FOR Total = 0
DO gfDispRe WITH EVAL('lcRpName')


*!*************************************************************
*! Name      : lfvCompany
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : - Call Companies mover function 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover,lfCmpExpr
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvCompany()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************

FUNCTION lfvCompany
= gfMover(@laRpSorCmp,@laRpTarCmp,'Select Company',.T.,'')  && call mover function.
= lfCmpExpr()
*-- end of lfvCompany.

*!**************************************************************************
*! Name      : lfwRepWhen
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen

*-- Define companies array that be used in company mover
IF EMPTY(laRpCmpCod)
  DECLARE laRpCmpCod[1,3]
  STORE '' TO lcRpCmpExp
  *-- Collect all companies
  SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mModlSet ;
    FROM SYCCOMP                            ;
    INTO ARRAY laRpCmpCod                   ;
    ORDER BY 1
  DECLARE laRpSorCmp[ALEN(laRpCmpCod,1),1],laRpTarCmp[ALEN(laRpCmpCod,1),1]
  FOR lnI = 1 TO ALEN(laRpCmpCod,1)
    STORE laRpCmpCod[lnI,1] TO laRpSorCmp[lnI,1],laRpTarCmp[lnI,1]
  ENDFOR
  =lfCmpExpr()
  *--
  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ENTERED'),1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(DATE())+'|'+DTOC(DATE())
  ENDIF
ELSE
  IF llNComp .AND. ALEN(laRpTarCmp,1) = 1
    llNComp = .F.
    CLEAR READ
  ENDIF
ENDIF
*-- End of lfwRepWhen.


*!**************************************************************************
*! Name      : lfwOldVal
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : To save the old value.
*!**************************************************************************
*! Example   : = lfwOldVal()
*!**************************************************************************
*
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))
*-- End of lfwOldVal.

*!***************************************************************************
*! Name      : lfCmpExpr
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : - Evaluate Company expression.
*!***************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!***************************************************************************
*! Called from : lfvCategory(),lfvCompany(),lfwRepWhen()
*!***************************************************************************
*! Passed Parameters  : ....
*!***************************************************************************
*! Returns            : ....
*!***************************************************************************
*! Example   : = lfCmpExpr()
*!***************************************************************************
*E301294,1  AKA 08/23/99  
*!***************************************************************************

FUNCTION lfCmpExpr
PRIVATE laTarget

IF EMPTY(laRpTarCmp)
  = ACOPY(laRpSorCmp,laTarget)
ELSE
  = ACOPY(laRpTarCmp,laTarget)
ENDIF
= ASORT(laTarget)
lcOldComp = lcRpCmpExp
lcRpCmpExp = ''
FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpCmpExp = IIF(EMPTY(lcRpCmpExp),PADR(laTarget[lnI],2),;
                    lcRpCmpExp + ','+PADR(laTarget[lnI],2))
ENDFOR

IF LEN(lcRpCmpExp) > 2
  llNComp = .T.
ELSE
  llNComp = .F.
ENDIF
CLEAR READ
*-- end of lfCmpExpr.

*!**************************************************************************
*! Name      : lpCrtTemp
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : Create temp file.
*!**************************************************************************
*! Example   : DO lpCrtTemp
*!**************************************************************************
*..
PROCEDURE lpCrtTemp

IF TYPE('gcWorkDir') = 'U'
  gcWorkDir = FULLPATH("")+"WORK\"
ENDIF

CREATE TABLE (gcWorkDir+lcTempLine);
             (CompId C(02), Rep1    C(03),CompNm  C(30),Account C(05),  Name C(30), Order C(06),  PO C(15),;
              Season C(06), Style   C(19),  Qty  N(04), Price N(6.2), Total N(8.2),;
              Terms  C(30), MarkUp  N(5,2), SlsRepN C(24), LineNo N(6))
INDEX ON CompId+Rep1+Account+Order+Style+PO+STR(lineno,6) TAG (lcTempLine)

*!**************************************************************************
*! Name      : lfColData
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : Collect data.
*!**************************************************************************
*! Example   : =lfColData()
*!**************************************************************************
*..
FUNCTION lfColData

FOR lnAllComp = 1 TO ALEN(laRpCmpCod,1)
  DIMENSION laFileName [1,2]      
  laFileName = ''
  lcComp_ID  = PADR(laRpCmpCod[lnAllComp,1],2)
  IF lcComp_ID $ lcRpCmpExp
    lcFilePath = ALLTRIM(LOWER(laRpCmpCod[lnAllComp,2]))
    lcCompName = ALLTRIM(SUBSTR(laRpCmpCod[lnAllComp,1],6))
    *--Open Company Files
    DO lpOpenFile
    *--Get records
    SELECT ORDHDR
    LOCATE FOR &lcRpExp
    IF EOF()
      LOOP
    ENDIF
    SCAN FOR &lcRpExp AND SEEK (CORDTYPE+ORDER,'ORDLINE')
      SELECT ORDLINE
      SCAN REST WHILE CORDTYPE+order+STR(lineno,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
        DO lpInsert
      ENDSCAN
    ENDSCAN
  ENDIF
ENDFOR
RETURN RECCOUNT(lcTempLine) >0

*!**************************************************************************
*! Name      : lpOpenFile
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : Open compnay files.
*!**************************************************************************
*! Example   : DO lpCrtTemp
*!**************************************************************************
*..
PROCEDURE lpOpenFile

IF USED('ORDHDR')
  USE IN ORDHDR
ENDIF  
IF USED('ORDLINE')
  USE IN ORDLINE
ENDIF  
IF USED('Style')
  USE IN Style
ENDIF
IF USED('SalesRep')
  USE IN SalesRep
ENDIF
IF USED('CUSTOMER')
  USE IN CUSTOMER
ENDIF
IF TYPE('gcAct_Comp') = 'U'
  USE lcFilePath+'ORDHDR'   ORDER TAG ORDHDR   IN 0
  USE lcFilePath+'Style'    ORDER TAG Style    IN 0
  USE lcFilePath+'SalesRep' ORDER TAG SalesRep IN 0
  USE lcFilePath+'ORDLINE'  ORDER TAG ORDLINE  IN 0
  USE lcFilePath+'CUSTOMER' ORDER TAG CUSTOMER IN 0
ELSE
  =gfOpenFile(lcFilePath+'ORDHDR',lcFilePath+'ORDHDR','SH')  
  =gfOpenFile(lcFilePath+'Style',lcFilePath+'Style','SH')  
  =gfOpenFile(lcFilePath+'SalesRep',lcFilePath+'SalesRep','SH')  
  =gfOpenFile(lcFilePath+'ORDLINE',lcFilePath+'ORDLINE','SH')  
  =gfOpenFile(lcFilePath+'CUSTOMER',lcFilePath+'CUSTOMER','SH')  
ENDIF  

*!**************************************************************************
*! Name      : lpInsert
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : Update Records.
*!**************************************************************************
*! Example   : DO lpInsert
*!**************************************************************************
*..
PROCEDURE lpInsert

IF !SEEK(lcComp_ID+Account+Order+Style+PO+STR(lineno,6),lcTempLine)
  lcAccName  = IIF(SEEK('M'+ORDLINE.ACCOUNT,'CUSTOMER'),CUSTOMER.STNAME,'')
  lcTermData = gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE')
  lcSlsName  = IIF(SEEK(ordhdr.Rep1,'SalesRep'),salesrep.NAME,'')
  =SEEK(ORDLINE.STYLE,'STYLE')
  lnMarkUp   = IIF(Price = 0,0,((ROUND(Price,2)-ROUND(style.Totcost,2))/ROUND(Price,2))*100)
  INSERT INTO (lcTempLine) (CompId,Rep1,CompNm,Account,Name,Order,PO,Season,Style,;
                            Qty,Price,Total,Terms,MarkUp,SlsRepN,LineNo);
                    VALUES (lcComp_ID,ordhdr.Rep1,lcCompName,ORDLINE.ACCOUNT,lcAccName,Ordline.Order,;
                            Ordline.CustPo,OrdLine.Season,OrdLine.Style,OrdLine.TotQty,OrdLine.Price,;
                            OrdLine.TotQty*OrdLine.Price,lcTermData,lnMarkUp,lcSlsName,OrdLine.LineNo)
ENDIF


*!**************************************************************************
*! Name      : lpOutAria
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : Run report from outside Aria.
*!**************************************************************************
*! Example   : DO lpOutAria
*!**************************************************************************
*..
PROCEDURE lpOutAria

*-- Get all companies information in case of running the program from outsode Aria.
gcSysHome = FULLPATH("")+"SYSFILES\"
gcWorkDir = FULLPATH("")+"WORK\"

RESTORE FROM DEFCOMP.MEM
*--If the Admin save companies whithout entering Entered date.
IF CTOD(lcRpTo) = {}
  lcCent = ON('CENT')
  SET CENTURY ON
  lcRpExp = "BETWEEN(DTOS(ORDHDR.ENTERED),ALLTRIM(DTOS({  "+DTOC(DATE())+"  })),ALLTRIM(DTOS({  "+DTOC(DATE())+"  }))) AND CORDTYPE = 'O' AND INLIST(STATUS,'O','H')"
  IF lcCent =  'OFF'
    SET CENTURY OFF
  ENDIF
ELSE
  *--Update date with system date.
  lcCent = ON('CENT')
  SET CENTURY ON
  lcRpExp = STRTRAN(lcRpExp,lcRpFrom,DTOC(DATE()))
  lcRpExp = STRTRAN(lcRpExp,lcRpTo,DTOC(DATE()))
  IF lcCent =  'OFF'
    SET CENTURY OFF
  ENDIF
ENDIF
lcStyTitle = "Style       -Color "
lcTempLine = 'TMPDATA'
*--Create the temp file.
DO lpCrtTemp
IF !lfColData()
  RETURN
ENDIF
SELECT (lcTempLine)
LOCATE
lcRpName = 'SOREVRG'
lcCons = SET('CONS')
SET CONS OFF
gcSysHome = FULLPATH("")+"SYSFILES\"
IF USED('sycinst')
  USE IN sycinst
ENDIF
USE gcSysHome + 'sycinst' IN 0
gcRepHome = ALLTRIM(sycinst.cInswinRD)
gcRepHome = FULLPATH("") + SUBSTR(gcRepHome,RAT("\",gcRepHome,2)+1)
USE IN sycinst
SET DEVICE TO PRINT
gcUser_Id  = "ADMN      "
gdSysDate  = DATE()
lcRpFrom   = DTOC(DATE())
lcRpTo     = DTOC(DATE())
lcOGRprtNam = gcRepHome +  'SO\SOREVRG'
*ADEL
DELETE FOR Total = 0
REPORT FORM (lcOGRprtNam) TO PRINTER NOCONSOLE NOEJECT
SET CONS &lcCons

*!**************************************************************************
*! Name      : lpGetComp
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : Get all companies information in case of running the program
*!           : from outsode Aria.
*!**************************************************************************
*! Example   : DO lpGetComp
*!**************************************************************************
*..
PROCEDURE lpGetComp

IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF
USE gcSysHome+'SYCCOMP' IN 0
DECLARE laRpCmpCod[1,3]
STORE '' TO lcRpCmpExp
*-- Collect all companies
SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mModlSet ;
  FROM SYCCOMP                            ;
  INTO ARRAY laRpCmpCod                   ;
  ORDER BY 1
DECLARE laRpSorCmp[ALEN(laRpCmpCod,1),1],laRpTarCmp[ALEN(laRpCmpCod,1),1]
FOR lnI = 1 TO ALEN(laRpCmpCod,1)
  STORE laRpCmpCod[lnI,1] TO laRpSorCmp[lnI,1],laRpTarCmp[lnI,1]
ENDFOR
=lfCmpExpr()
USE IN SYCCOMP
lcRpFrom   = DTOC(DATE())
lcRpTo     = DTOC(DATE())
lcRpExp = "BETWEEN(DTOS(ORDHDR.ENTERED),ALLTRIM(DTOS({  "+DTOC(DATE())+"  })),ALLTRIM(DTOS({  "+DTOC(DATE())+"  }))) AND CORDTYPE = 'O' AND INLIST(STATUS,'O','H')"
SAVE ALL LIKE l?Rp* TO "DEFCOMP.MEM"


*!**************************************************************************
*! Name      : gfCodDes
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : Get codes desc if reun from outside Aria,
*!**************************************************************************
*! Example   : DO lpGetComp
*!**************************************************************************
*..
FUNCTION gfCodDes
PARAMETERS lcCodeVal, lcFldName , llChkEdit
PRIVATE lcSavOrder,lcReturnVal,lcSavSelct

llRetArray = (TYPE("lcCodeVal[1,1]") = "C")
IF !llRetArray AND (TYPE("lcCodeVal") <> "C" OR TYPE("lcFldName") <> "C")
  RETURN ''
ENDIF

IF !llRetArray
  lcCodeVal   = PADR(UPPER(lcCodeVal),6)
  lcFldName   = PADR(UPPER(lcFldName),10)
ENDIF  
lcReturnVal = ""
lcSavSelct  = ALIAS()      && Variable to save the currently selected file.

llUseCodes = .F.
IF !USED("CODES")
  USE (lcFilePath+"Codes") IN 0
  llUseCodes = .T.
ENDIF

SELECT CODES               && Select CODES file
lcSavOrder = SYS(22)       && Save the file order
SET ORDER TO TAG CODES     && Change the order
*-- if pass array of codes.
IF llRetArray
  PRIVATE lnArrLen , lnCodeLen
  lnCodeLen = 6
  lnArrLen  = 0
  FOR lnArrLen = 1 TO ALEN(lcCodeVal,1)
    lcCodeVal[lnArrLen,1] = PADR(UPPER(lcCodeVal[lnArrLen,1]),6)
    lcCodeVal[lnArrLen,2] = PADR(UPPER(lcCodeVal[lnArrLen,2]),10)
    IF EMPTY(lcCodeVal[lnArrLen,1]) .OR. lcCodeVal[lnArrLen,1] = "*"
      lcCurFlt = FILTER()
      lnFltRec = IIF(EOF() .OR. BOF(), 0, RECNO())
      SET FILTER TO
      IF !SEEK(SPACE(1)+LEFT(lcCodeVal[lnArrLen,1],1))
        APPEND BLANK
        REPLACE cFld_Name  WITH IIF(lcCodeVal[lnArrLen,1] = '*','ALL','N/A') ;
                cCode_No   WITH IIF(lcCodeVal[lnArrLen,1] = '*','*','')      ;
                cDiscrep   WITH IIF(lcCodeVal[lnArrLen,1] = '*','All','N/A') ;
                cRltField  WITH 'N'
      ENDIF  
      lcCodeVal[lnArrLen,3] = CODES.cDiscrep
      SET FILTER TO &lcCurFlt.
      IF BETWEEN(lnFltRec,1,RECCOUNT())
        GO lnFltRec
      ENDIF    
    ELSE
      IF SEEK('N' + lcCodeVal[lnArrLen,1] + "N" + lcCodeVal[lnArrLen,2])  
        lcCodeVal[lnArrLen,3] = CODES.cDiscrep
      ELSE
        lcCodeVal[lnArrLen,3] = ''       && In case of this code record is deleted
      ENDIF  
    ENDIF
    IF !EMPTY(lcCodeVal[lnArrLen,3]) AND gfIsEdtble(ALLTRIM(lcCodeVal[lnArrLen,2]) , @lnCodeLen)
       lcCodeVal[lnArrLen,3] = PADR(lcCodeVal[lnArrLen,1],lnCodeLen) + '-' + lcCodeVal[lnArrLen,3]
    ENDIF
  ENDFOR
ELSE && Pass one code only
  IF EMPTY(lcCodeVal) .OR. lcCodeVal = "*"
    lcCurFlt = FILTER()
    lnFltRec = IIF(EOF() .OR. BOF(), 0, RECNO())
    SET FILTER TO
    IF !SEEK(SPACE(1)+LEFT(lcCodeVal,1))
      APPEND BLANK
      REPLACE cFld_Name  WITH IIF(lcCodeVal = '*','ALL','N/A') ;
              cCode_No   WITH IIF(lcCodeVal = '*','*','')      ;
              cDiscrep   WITH IIF(lcCodeVal = '*','All','N/A') ;
              cRltField  WITH 'N'
    ENDIF  
    lcReturnVal = CODES.cDiscrep
    SET FILTER TO &lcCurFlt.
    IF BETWEEN(lnFltRec,1,RECCOUNT())
      GO lnFltRec
    ENDIF    
  ELSE
    IF SEEK('N' + lcCodeVal + "N" + lcFldName)  
      lcReturnVal = CODES.cDiscrep
    ELSE
      lcReturnVal = ''       && In case of this code record is deleted
    ENDIF  
  ENDIF  
  PRIVATE lnCodeLen
  lnCodeLen = 6
  IF llChkEdit AND !EMPTY(lcReturnVal) AND gfIsEdtble(ALLTRIM(lcFldName) , @lnCodeLen)
    lcReturnVal = PADR(lcCodeVal,lnCodeLen) + '-' + lcReturnVal
  ENDIF
ENDIF

SET ORDER TO &lcSavOrder
IF llUseCodes
  USE IN Codes
ENDIF
SELECT IIF(EMPTY(lcSavSelct),0,lcSavSelct)
RETURN lcReturnVal
*-- end of gfCodDes.

*!**************************************************************************
*! Name      : lpClosFil
*: Developer : Adel Mohmmed El Gazzar (ADEL)
*: Date      : 02/12/2002
*! Purpose   : Close all files.
*!**************************************************************************
*! Example   : DO lpClosFil
*!**************************************************************************
*..
PROCEDURE lpClosFil

IF TYPE('gcAct_Comp') = 'U'
  USE IN ORDHDR
  USE IN Style
  USE IN SalesRep
  USE IN ORDLINE
  USE IN CUSTOMER
  USE IN TMPDATA
  gcWorkDir = FULLPATH("")+"WORK\"
  ERASE gcWorkDir+'TMPDATA.DBF'
  ERASE gcWorkDir+TMPDATA.CDX'
ELSE
  USE IN (lcTempLine)
  ERASE gcWorkDir+lcTempLine + '.DBF'
  ERASE gcWorkDir+lcTempLine + '.CDX'
ENDIF  
