*!**************************************************************************
*! Name      : BRAMAIN.PRG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/05/02
*! Purpose   : Bratex Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C102684,1
*!**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.



*!**************************************************************************
*! Name      : lfInitVar
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/05/02
*! Purpose   : Initialize needed variables
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102684
FUNCTION lfInitVar

IF USED('VendVar')
  USE IN VendVar
ENDIF
CREATE CURSOR VendVar (nVndOnOrd N(13,3))
APPEND BLANK
SCATTER MEMVAR
*--End of lfInitVar



*!**************************************************************************
*! Name      : lfGetOrd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/05/02
*! Purpose   : Get On Order qty for the Vendor iTem\color
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102684
FUNCTION lfGetOrd

CLEAR READ

*!**************************************************************************
*! Name      : lfCALCORD
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/05/02
*! Purpose   : Calculate On Order qty for the Vendor iTem\color
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102684
FUNCTION lfCALCORD

PRIVATE lnAlias
lnAlias = SELECT()
SELECT POFLN
lcOldOrd = ORDER()
SET ORDER TO Poflnf
=SEEK(laData[1]+IIF(llAllColrs,'',laData[2]))
SUM REST WHILE fabric+color+cmattype+pomat+trancd = laData[1]+IIF(llAllColrs,'',laData[2]) IIF(POFLN.cMatType = 'P',IIF(TRANCD='1',nFabTOTQTY,-nFabTOTQTY),-nFabTOTQTY) TO lnVndOnOrd FOR (POFLN.cMatType = 'P' OR POFLN.cMatType = 'R') ;
     AND VENDOR = lcVenCode AND IIF(lcWare <> 'All',CWARECODE = lcWare,.T.)
*--Create this cursor to hold lnVndOnOrd instead of intializing it in the standard MAMATRL
IF USED('VendVar')
  SELECT VendVar
  REPLACE VendVar.nVndOnOrd WITH lnVndOnOrd
ELSE
  CREATE CURSOR VendVar (nVndOnOrd N(13,3))
  APPEND BLANK
  REPLACE VendVar.nVndOnOrd WITH lnVndOnOrd  
ENDIF

SELECT POFLN
SET ORDER TO (lcOldOrd)
SELECT (lnAlias)
=lfRefresh()

*!**************************************************************************
*! Name      : lfFalseIt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/05/02
*! Purpose   : False llVendQury in select mode.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102684
FUNCTION lfFalseIt

llVendQury = .F.


*!**************************************************************************
*! Name      : lfTrueIt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/05/02
*! Purpose   : True llVendQury in select mode.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102684
FUNCTION lfTrueIt

llVendQury = .T.

*!**************************************************************************
*! Name      : lfAddBar
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/05/02
*! Purpose   : Add a custom bar 'Vendor On Order'.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102684
FUNCTION lfAddBar

*B606663,1 AMH Define this bar in the correct position [Start]
*DEFINE BAR 3 OF _OPTIONPOP PROMPT 'Vendor On Order' SKIP FOR !llVendQury OR !laScrMode[2] ;
                                                         OR (EMPTY(laData[2]) OR llAllColrs)
DEFINE BAR lnBarNo+1 OF _OPTIONPOP PROMPT 'Vendor On Order' SKIP FOR !llVendQury OR !laScrMode[2] ;
                                                            OR (EMPTY(laData[2]) OR llAllColrs)
*B606663,1 AMH [End]

*!**************************************************************************
*! Name      : lfRUNSCR
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/05/02
*! Purpose   : Show screen 'Vendor On Order'.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102684
FUNCTION lfRUNSCR

llVendOk  = .F.

IF USED('VendVar')
  lnVndOnOrd = VendVar.nVndOnOrd
ELSE
  lnVndOnOrd = 0
ENDIF

DO (gcScrDir+gcWinAppl+'\mamatvn.SPX')

IF !llVendOk
  PRIVATE lnAlias,lcBrFields,lcPO,laPO,llWasSel,lcType
  lnAlias    = SELECT()
  lcPO       = SPACE(06)
  lcType     = SPACE(01)
  SELECT POFLN.cMatType ,POFLN.POMAT,POFLN.nCost1 as nPrice,;
         SUM(IIF(TRANCD='1',nFabTOTQTY,0.000)) AS nORDERED,;
         SUM(IIF(TRANCD='2',nFabTOTQTY,0.000)) AS nRECEIVED,;
         SUM(IIF(TRANCD='3',nFabTOTQTY,0.000)) AS nDAMAGED,;
         SUM(IIF(TRANCD='4',nFabTOTQTY,0.000)) AS nCANCELLED ;
    FROM POFLN;
    INTO DBF (gcWorkDir+lcCursName);
    WHERE (POFLN.cMatType = 'P' OR POFLN.cMatType = 'R') ;
    AND POFLN.FABRIC+POFLN.COLOR = SUBSTR(laData[1],1,7)+SUBSTR(laData[2],1,6);  
    AND POFLN.vendor = lcVenCode;
    GROUP BY POFLN.POMAT

  WAIT CLEAR
  IF _TALLY > 0
    lcFile_Ttl  = 'Item PO (s) / Returns'
    lcBrFields  = [dummi=IIF(cMatType = 'P','Pur.Ord','Return') :H='Trans. Type',]+;
                  [POMAT:8:H='P/O #',]+;
                  [POFHDR.STATUS :2:H='S',]+;
                  [POFHDR.cWareCode:11:H='Location',]+;                  
                  [POFHDR.VENDOR:11:H='Vendor',]+;
                  [POFHDR.COMPLETE:8:H='Complete',]+;
                  IIF(llCostPrv, [nPRICE:H='Price':P='99999.999',],[])+;
                  [nORDERED:H='Ordered':P='999999.999',]+;
                  [nRECEIVED:H='Received':P='999999.999',]+;
                  [nOPEN=IIF( (POFHDR.STATUS='X').OR.]+;
                  [ ((nORDERED-nRECEIVED-nDAMAGED-nCANCELLED)<0) ,]+;
                  [ 0, nORDERED-nRECEIVED-nDAMAGED-nCANCELLED)]+;
                  [:P='999999.999':H='Open']
    DECLARE laPO[2]
    SELECT (lcCursName)
    SET RELA TO cMatType+POMAT INTO POFHDR ADDI
    IF lcWare = 'All'  
    ELSE
      SET FILTER TO POFHDR.CWareCode= PADR(lcWare,6) 
    ENDIF
    GO TOP
    IF !EOF()
      llWasSel=AriaBrow('', lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,;
                gnBrHSRow2, gnBrHSCol2,"","","POMAT,cMatType","laPO")
      IF llWasSel
        lcPO   = laPO[1]
        lcCall = laPO[2]
      ENDIF

      IF !EMPTY(lcPO)
        lcParam  = "'"+lcCall+"','"+lcPO + "'"
        DO gpDoProg WITH 'AWRMAPRCAM',.F.,'MA',lcParam
      ENDIF
    ELSE
      =gfModalGen('QRM36030B36000','ALERT', 'purchase orders|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))  
    ENDIF
    SELECT (lcCursName)
    SET FILTER TO
    SET RELA TO
  ELSE
    =gfModalGen('QRM36030B36000','ALERT', 'purchase orders|'+ALLTRIM(laData[1]) + '/'+ALLTRIM(laData[2]))
  ENDIF
  SELECT (lnAlias)
ENDIF  

*!**************************************************************************
*! Name      : lfMatAcct
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/09/2002
*! Purpose   : Add customer field to the generate M.PO. screen in MAMATRQ
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102689
FUNCTION lfMatAcct

lcFields = lcFields + ",cMatAcct :P='@! XXXXX' :H='Customer' "+;
                      ":V=gfDoTriger('MAMATRQ',PADR('VMATACCT',10))"

*!**************************************************************************
*! Name      : lfvMatAcct
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/09/2002
*! Purpose   : Validate the customer field in the generate M.PO. screen of MAMATRQ
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102689
FUNCTION lfvMatAcct

PRIVATE llOpenCust,lcMatAcct,llObjRet
STORE .F. TO llOpenCust
IF !USED('CUSTOMER')
  = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
  llOpenCust = .T.
ENDIF
SELECT (lcPOTmpHD)
IF '?' $ CMATACCT .OR. (!EMPTY(CMATACCT) .AND. !SEEK('M' + CMATACCT , 'CUSTOMER'))
  lcMatAcct = CMATACCT
  llObjRet  = CusBrowM(@lcMatAcct , '' , 'M')
  SELECT (lcPOTmpHD)
  lcMatAcct = IIF(llObjRet , lcMatAcct , '')
  REPLACE CMATACCT WITH lcMatAcct
ENDIF
IF llOpenCust
  USE IN CUSTOMER
ENDIF

*!**************************************************************************
*! Name      : lfvCstAcct
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/12/2002
*! Purpose   : Validate the customer field in the PO Revision screen of MAPRCAM
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102685
FUNCTION lfvCstAcct
PARAMETERS lcRetVal

lcRetVal = .T.
PRIVATE llOpenCust,lcMatAcct,llObjRet,lnAlias
STORE .F. TO llOpenCust
lcVar = SYS(18)
IF TYPE('LAUSRFIELD') = "U"
  RETURN
ENDIF
lnCustPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CMATACCT'),1)
IF !(lcVar == "LAOGFXFLT(" + ALLTRIM(STR(lnCustPos)) + ",6)")
  RETURN
ENDIF
lcCustAcct = EVALUATE(lcVar)
lnAlias = SELECT(0)
IF !USED('CUSTOMER')
  = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
  llOpenCust = .T.
ENDIF
IF !EMPTY(lcCustAcct) .AND. !SEEK('M' + lcCustAcct, 'CUSTOMER')
  lcMatAcct  = lcCustAcct
  llObjRet   = CusBrowM(@lcMatAcct , '' , 'M')
  lcMatAcct  = IIF(llObjRet , lcMatAcct , '')
  lcCustAcct = lcMatAcct
  laUsrFields[lnCustPos,6] = lcCustAcct
  laOGFxFlt[lnCustPos,6] = lcCustAcct
ENDIF
IF llOpenCust
  USE IN CUSTOMER
ENDIF
SELECT (lnAlias)
RETURN lcRetVal

*:*************************************************************
*: Name      : lfPRNTRUN
*: Developer : AHMED MAHER
*: Date      : 09/19/2002
*: Purpose   : To do not display the printer screen
*:*************************************************************
*: Example   : = lfPRNTRUN()
*:*************************************************************
*! Due to C102699,1
*:*************************************************************
FUNCTION lfPRNTRUN

IF TYPE("lcDummy") = "C" .AND. lcDummy = "MFSIMTEX"
  gcDevice = 'PRINTER'
  RETURN .T.
ELSE
  RETURN .F.
ENDIF
