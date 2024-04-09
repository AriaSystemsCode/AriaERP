*:***************************************************************************
*: Program file  : ARGRSPNR
*: Program desc. : Gross profit for NOR01
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : Account receivable (AR)
*: Developer     : Mostafa Rawash (MMR)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARGROSSP
*:***************************************************************************
*-- lcSort    variable that showes the report sort
*-- ldRPPLDate Variable that hold From date value
*-- ldRPPHDate Variable that hold to   date value

lnMajorLen = LEN(gfItemMask("PM"))
lcSort    = lcRPSortBy
ldLDate   = ldRPPLDate
ldHDate   = ldRPPHDate

STORE SPACE(0) TO XFILTER,lDtFrm,lDtTo
lDtFrm=CTOD(LEFT(LAOGFXFLT(1,20),10))
lDtTo=CTOD(RIGHT(LAOGFXFLT(1,20),10))
DO lpFilesRela

*--initilize filters on file.
*--Creat a temp file.
INVLTEMP = gfTEMPNAME()

DO lpCreateTmp
WAIT WINDOW 'Building the invoices records...' NOWAIT
*-- Build the invoice records.
SELECT INVLINE
SET ORDER TO Invline
SCAN  FOR &lcrpexp
  WAIT WINDOW 'Collecting invoice # : '+Invoice NOWAIT
  IF  INVHDR.Status = 'V'
    LOOP
  ENDIF
  STORE 0 to lnTotCAmnt,lntotRAmnt,lnTotRet,lnRetQty,lnRetCst
  SELECT (INVLTEMP)
  IF lcSort='A' 
    LOCATE FOR ACCOUNT=INVLINE.ACCOUNT
      IF NOT FOUND()   
        SELECT CREDIT
        SET ORDER TO CREDIT
        IF  EMPTY(ldLDate) AND EMPTY(ldHDate)
          =SEEK (INVLINE.ACCOUNT,'CREDIT')
          SCAN REST WHILE account+tran+DTOS(trandate)= INVLINE.ACCOUNT   
            lnTotCAmnt = lnTotCAmnt + Amount
          ENDSCAN
        ELSE
          =SEEK (INVLINE.ACCOUNT,'CREDIT')
          SCAN REST WHILE account+tran+DTOS(trandate)= INVLINE.ACCOUNT FOR BETWEEN(TRANDATE,ldLDate,ldHDate)  
            lnTotCAmnt = lnTotCAmnt + Amount
          ENDSCAN
        ENDIF
       SELECT RETLINE
      IF  EMPTY(LDTFRM) AND EMPTY(lDtTo)
       =SEEK (INVLINE.ACCOUNT,'RETLINE')
      SCAN REST WHILE account+invoice+style+crmemo = INVLINE.ACCOUNT 
         lnRetQty   = lnRetQty   + Totqty 
         lntotRAmnt = lntotRAmnt + Amount
         lnRetCst   = lnRetCst   + Cost
         lnTotRet   = lnTotRet   +(Totqty * Cost)   
       ENDSCAN
      ELSE
       =SEEK (INVLINE.ACCOUNT,'RETLINE')
      SCAN REST WHILE account+invoice+style+crmemo = INVLINE.ACCOUNT FOR BETWEEN(CRDATE,lDtFrm,lDtTo)
         lnRetQty   = lnRetQty   + Totqty 
         lntotRAmnt = lntotRAmnt + Amount
         lnRetCst   = lnRetCst   + Cost
         lnTotRet   = lnTotRet   +(Totqty * Cost)   
       ENDSCAN
      ENDIF 
     SELECT (INVLTEMP)  
     ENDIF
   ELSE 
     LOCATE FOR STYLE=STYLE.CSTYMAJOR
     IF NOT FOUND()
      SELECT RETLINE
      SET ORDER TO RETLINES
     IF  EMPTY(LDTFRM) AND EMPTY(lDtTo)
      =SEEK( SUBSTR(STYLE.STYLE , 1 , lnMajorLen ) , 'RETLINE')
      SCAN REST WHILE STYLE+CRMEMO = SUBSTR(STYLE.STYLE , 1 , lnMajorLen ) 
         lnRetQty   = lnRetQty   + Totqty 
         lntotRAmnt = lntotRAmnt + Amount
         lnRetCst   = lnRetCst   + Cost
         lnTotRet   = lnTotRet   +(Totqty * Cost)   
       ENDSCAN
      ELSE
      =SEEK( SUBSTR(STYLE.STYLE , 1 , lnMajorLen ) , 'RETLINE')
      SCAN REST WHILE STYLE+CRMEMO = SUBSTR(STYLE.STYLE , 1 , lnMajorLen ) FOR BETWEEN(CRDATE,lDtFrm,lDtTo) 
         lnRetQty   = lnRetQty   + Totqty 
         lntotRAmnt = lntotRAmnt + Amount
         lnRetCst   = lnRetCst   + Cost
         lnTotRet   = lnTotRet   +(Totqty * Cost)   
       ENDSCAN
      ENDIF 
       SELECT (INVLTEMP)
     ENDIF
   ENDIF
   lcDivision = gfCodDes(INVHDR.CDIVISION , 'CDIVISION')  
   APPEND BLANK
   REPLACE INVOICE    WITH INVLINE.INVOICE ,;
          ACCOUNT    WITH INVLINE.ACCOUNT ,;
          STYLE      WITH STYLE.CSTYMAJOR ,;
          STYNME     WITH INVLINE.STYLE   ,; 
          LINENO     WITH INVLINE.LINENO  ,;
          PRICE      WITH INVLINE.PRICE   ,;
          TOTQTY     WITH INVLINE.TOTQTY  ,;
          COST       WITH INVLINE.COST    ,;
          DATE      WITH INVLINE.INVDATE  ,;             
          NAME      WITH CUSTOMER.BTNAME  ,;
          DESC      with invline.DESC1    ,;
          DISCPCNT  WITH INVHDR.DISCPCNT  ,;
          TRDE_DISC WITH INVHDR.TRDE_DISC ,;
          CDIVISION WITH INVHDR.CDIVISION ,; 
          CDIVDISC  WITH lcDivision       ,;
          NCREDIT   WITH lnTotCAmnt       ,;
          NRETQTY   WITH -lnRetQty        ,; 
          NRETURN   WITH -lntotRAmnt      ,;
          NRETCST   WITH -lnRetCst        ,;
          NTOTRET   WITH lnTotRet          
ENDSCAN

SELE &INVLTEMP 
IF RECCOUNT() = 0
  WAIT CLEAR
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*--Clear relations
SELE INVLINE
SET RELA TO
*-- Indexing file.
Z = RECCOUNT()
WAIT WINDOW 'Sorting '+STR(Z,6)+' records for the report ' NOWAIT
lcInvDate=strtran(LAOGFXFLT(1,20),"|","-")
SELE &INVLTEMP 
 IF lcSort='A'
   INDEX ON CDIVISION+Account+Style+Invoice+STR(RECNO(),7) TAG &INVLTEMP
   lcFormName="ARGPNBYA"
 ELSE  
   INDEX ON CDIVISION+Style+Invoice+STR(RECNO(),7) TAG &INVLTEMP  
   lcFormName="ARGPNBYS"  
ENDIF
DO gfDispRe WITH EVAL('lcFormName')
RETURN


DO ENDREPORT
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lpFilesRela
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/11/2005
*! Purpose   : Create files relation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpFilesRela
*!*************************************************************

PROCEDURE lpFilesRela

SELECT INVLINE
SET RELATION TO INVOICE INTO INVHDR,'M'+ACCOUNT INTO CUSTOMER, Account into credit
SET RELATION TO STYLE INTO STYLE ADDITIVE

*!*************************************************************
*! Name      : lpCreateTmp
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/11/2005
*! Purpose   : Craete temp. file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCreateTmp
*!*************************************************************

PROCEDURE lpCreateTmp

CREATE TABLE &gcWorkDir.&INVLTEMP ;
  ( INVOICE C(6) , DATE D    , ACCOUNT C(5) , NAME C(30),STYLE C(19),STYNME C(19),DESC C(60),;
   LINENO N(6) , PRICE N(12,2)  , TOTQTY N(8)  , COST N(10,2) ,;
   DISCPCNT N(6,2) , TRDE_DISC N(5,2) , CDIVISION C(6) , CDIVDISC C(30),;
   NCREDIT N(14,2) ,NRETQTY N(7), NRETURN N(14,2),NRETCST N(10,2), NTOTRET N(15,2))

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/11/2005
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************

FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/11/2005
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************

FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'

      lnClrPo    = laMajSeg[lnI,4]

      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
  
      lcNonMajPi = laMajSeg[lnI,3]

      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
  
      EXIT
  
    ELSE
      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*!*************************************************************
*! Name      : lfMajPic
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/11/2005
*! Purpose   : Get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic


*!*************************************************************
*! Name      : lfchkRet
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/11/2005
*! Purpose   : Check existence of RM module
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfchkRet()
*!*************************************************************

FUNCTION lfchkRet

RETURN ('RM' $ gcCMpModules)

*MMR
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/16/2005
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************

FUNCTION lfvDateRng

PRIVATE ldFrom,ldTo

ldFrom = ldRPPLDate
ldTo   = ldRPPHDate

lcTitle = 'Credit Date'

DO DateRng.Spx

ldRPPLDate = ldFrom
ldRPPHDate = ldTo
*-- end of lfvDateRng.

*!*************************************************************
*! Name      : lfvPbOk
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/16/2005
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPbOk()
*!*************************************************************

FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  ldRPPLDate = ldFrom
  ldRPPHDate = ldTo
  CLEAR READ
ENDIF


*!*************************************************************
*! Name      : lfRepShow
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/11/2005
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B603115,1 
*
FUNCTION lfRepShow

laOGFxFlt[2,6]= gcBaseCurr
*laOGObjCnt[11] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")




*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/11/2005
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen
RETURN
SET ORDER TO CODES    IN CODES
SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO SALESREP IN SALESREP
SET ORDER TO STYLE    IN STYLE
SET ORDER TO INVHDR   IN INVHDR
SET ORDER TO INVLINES IN INVLINE

IF llRetModu
  SET ORDER TO RETHDRA  IN RETHDR
  SET ORDER TO RETLINES IN RETLINE
ENDIF

IF ALLTRIM(gfGetMemVar('M_LINK_GL')) <>  'Y'
  SHOW GET pbAdvanced DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/11/2005
*! Purpose   : Validate function for the Account.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvAccount()
*!*************************************************************

FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.
*!*************************************************************
*! Name      : lfStySum
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/11/2005
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfVldSrt
*! Developer : Mostafa Rawash (MMR)
*! Date      : 05/11/2005
*! Purpose   : Refresh Option Grid
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfVldSrt()
*!*************************************************************
FUNCTION lfVldSrt

Clear Read
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/05/2002
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
PRIVATE lcAliasCst

lcAliasCst = SELECT(0)
SELECT Customer
SET ORDER TO Customer
GO TOP
SELECT(lcAliasCst)
