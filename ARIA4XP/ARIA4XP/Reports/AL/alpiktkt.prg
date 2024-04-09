*:***************************************************************************
*: Program file  : ALPIKTKT
*: Program desc. : MOMENTRENDS ALLOCATION PIKTICKT LOG
*! Date          : 12/27/04
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar - (MMT) Due to issue:037542
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPIKTKT
*:***************************************************************************
*: Modifications:
*: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [T20080603.0001]
*: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[T20110106.0004]
*: E303148,1 HIA 05/14/2012 Export Customer name to Excel[T20120425.0006]
*: B610679,1 TMI 02/18/2014 Modify report program to apply 'has packing list' filtering code in case of exporting to excel  [T20140209.0008] 
*:***************************************************************************
*--Section of Variables

lcTime = TIME()         && Variable to hold the Time
loOgScroll.cCRorientation = 'P'
*To get the date in a variables
lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
IF lnPosDate > 0 
  lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
  LDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnPosDate,6],1,10)))
  HDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnPosDate,6],12,20)))
ENDIF   

IF loOGScroll.llOGFltCh
  WAIT WINDOW "Collecting Data......." NOWAIT 
  IF USED(lcTempOrdHdr)
    USE IN (lcTempOrdHdr)
    SELECT CORDTYPE,ACCOUNT,ORDER,cCurrCode,Nexrate,NcurrUnit,SHIPVIA FROM &lcTempOrdhd WHERE .F. into CURSOR &lcTempOrdHdr READWRITE 
    =lfMakeIndex(lcTempOrdHdr)
  ENDIF   
  IF USED(lcPickTkTmp)
    SELECT &lcTempPikTkt..*  FROM  &lcTempPikTkt WHERE .F. INTO CURSOR  &lcPickTkTmp READWRITE 
    =lfMakeIndex(lcPickTkTmp)
  ENDIF 
  IF USED(lcTempCustomer)  
    SELECT TYPE,ACCOUNT,STORE,BTNAME FROM &lcTempCust WHERE .F. into CURSOR &lcTempCustomer READWRITE 
    =lfMakeIndex(lcTempCustomer)
  ENDIF 
  DO lpCreaTemp  && Create Temp Cursor
  DO lpColect    && Collect data
ENDIF

SELECT (lcPickTmp)   &&Temp. File Hold The Records That Satisfy Requiments 
SET ORDER TO TAG &lcPickTmp

llEndReprt = .F.
GO BOTTOM
REPLACE &lcPickTmp..lEndRep WITH .T.

SELECT(lcPickTmp)
SET RELATION TO 'O' + Order INTO &lcTempOrdHdr ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO &lcTempCustomer ADDITIVE
LOCATE   

lcCurrExpr = IIF(llMultCurr AND lcRpCurr="F",[EVALUATE(lcTempOrdHdr+'.CCURRCODE')],[oAriaApplication.BaseCurrency ])
lcCurrCode = EVALUATE(lcCurrExpr)
llCurrChg  = .F.
lcTime1 = TIME()         && Variable to hold the Time

*!*	IF RECCOUNT(lcPickTmp)> 0
*!*	  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPickTmp))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcTime,lcTime1),6,2)) + ' Seconds...' TIMEOUT 2
*!*	ENDIF 
SELECT(lcPickTmp)
SET FILTER TO 

llHasPack_List = .F.
IF lcRPPCKLST  <> 'B'
  llHasPack_List  = IIF(lcRPPCKLST = 'Y',.T.,.F.)
  SELECT(lcPickTmp)
  IF llHasPack_List
    SET FILTER TO loPack_Hdr.Seek(&lcPickTmp..piktkt)
  ELSE
    SET FILTER TO !loPack_Hdr.Seek(&lcPickTmp..piktkt)
  ENDIF   
ENDIF 

SELECT(lcPickTmp)
*B610679,1 TMI 02/18/2014 17:19 [Start] get the filter to use in the select statement that exports to Excel 
lcSvFlt = UPPER(FILTER())
*B610679,1 TMI 02/18/2014 17:19 [End  ] 
LOCATE 
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF 
*: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[Start]
IF (oAriaApplication.gcDevice = "FILE" .AND. loOGScroll.cTextRepType = "EXCEL")
  lcOrderBExp = IIF(llMultCurr AND (lcRpCurr = "F") ,IIF(LCRPSORT='P',"cCurrCode , PIKTKT","cCurrCode , SHIPVIA,PIKTKT"),;
  							IIF(LCRPSORT='P',"PIKTKT","SHIPVIA,PIKTKT"))
   *: E303148,1 HIA 05/14/2012 Export Customer name to Excel[T20120425.0006][Begin]
	*SELECT *, 0000000 as TotQty, 00000000000.00 as Amount FROM (lcPickTmp) INTO CURSOR 'TmpExcl' READWRITE ;
	*ORDER BY &lcOrderBExp.

	SELECT *, 0000000 as TotQty, 00000000000.00 as Amount, SPACE(30) as BtName FROM (lcPickTmp) INTO CURSOR 'TmpExcl' READWRITE ;
	ORDER BY &lcOrderBExp.
	
	*: E303148,1 HIA 05/14/2012 Export Customer name to Excel[T20120425.0006] [End]
	SET RELATION TO IIF(llMultCurr AND (lcRpCurr = "F") ,IIF(LCRPSORT='P',cCurrCode + PIKTKT,cCurrCode + SHIPVIA+PIKTKT),;
									IIF(LCRPSORT='P',PIKTKT,SHIPVIA+PIKTKT)) INTO (lcPickTmp)
  SELECT 'TmpExcl'
  *B610679,1 TMI 02/18/2014 17:20 [Start] select the filtered lines only if the filter is applied
  IF !EMPTY(lcSvFlt)
    lcSvFlt = STRTRAN(lcSvFlt,UPPER(lcPickTmp),'TmpExcl')
    SET FILTER TO EVALUATE(lcSvFlt)
  ENDIF 
  *B610679,1 TMI 02/18/2014 17:20 [end ]    
  LOCATE 
  SCAN
    STORE 0 TO lnTotQty,lnTotAmnt 
    lfSumPik()
    REPLACE TOTQTY WITH lnTotQty,Amount WITH lnTotAmnt IN  'TmpExcl'
	*: E303148,1 HIA 05/14/2012 Export Customer name to Excel[T20120425.0006] [Begin]
    Replace BtName WITH &lcTempCustomer..BtName
 	*: E303148,1 HIA 05/14/2012 Export Customer name to Excel[T20120425.0006] [End]
  ENDSCAN
  
  SET RELATION OFF INTO (lcPickTmp)
ENDIF
STORE 0 TO lnTotQty,lnTotAmnt
*: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[End]
DO gfDispRe WITH EVALUATE('lcRpName') 

*-- Clear relation
SELECT (lcPickTmp)
SET RELATION TO


*!**************************************************************************
*! Name      : lfSeTOrdr
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from        : Option Grid 
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example            : =fSeTOrdr()
*!**************************************************************************
*! Note               : symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSeTOrdr
PARAMETERS OpGrdParm
SELECT ORDHDR
DO CASE
  CASE OpGrdParm = 'S'    
    lcRelation = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcRelation INTO CUSTOMER 
    GO TOP
  CASE OpGrdParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER
ENDCASE

*!**************************************************************************
*! Name      : lfSumPik
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : To return the Total Amount and Total Quantity
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from        : ALPKTMTD.FRX
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example            : lfSumPik()
*!**************************************************************************
*! Note               : 
*!**************************************************************************
FUNCTION lfSumPik

PRIVATE lcExpList,lnCurAlias,lnPrice
lnCurAlias = SELECT(0)
lcExpList = "TotPik,TotPik*Price"
*--
IF &lcPickTmp..Status $ 'CX'

  loDBFPikLine.SEEK(&lcPickTmp..PikTkt)
  SELECT(lcTempPikLine)
  SUM &lcExpList REST WHILE PIKTKT+ORDER+STR(LINENO,6) = &lcPickTmp..PikTkt ;
      TO lnTotQty,lnTotAmnt 
    
ELSE

  loDBFOrdline.SEEK('O'+&lcPickTmp..Order+&lcPickTmp..Store)
  SELECT(lcTempOrdline)
  SUM &lcExpList REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = ;
                            'O'+&lcPickTmp..Order+&lcPickTmp..Store ;
                      FOR PikTkt = &lcPickTmp..PikTkt TO lnTotQty,lnTotAmnt 
ENDIF

IF llMultCurr AND (lcRpCurr <> "F") AND;
   (lnTotAmnt <> 0) AND (EVALUATE(LCTEMPORDHDR+'.CCURRCODE') <> oAriaApplication.BaseCurrency)
  lnTotAmnt = gfAmntDisp(lnTotAmnt,lcRpCurr,ldRpExDate,;
                         lcRpTmpNam,.F.,lcTempOrdHdr)
ENDIF

IF !llCurrChg
  llCurrChg = !(EVALUATE(lcCurrExpr) == lcCurrCode)
  lcCurrCode = EVALUATE(lcCurrExpr)
ENDIF  

llEndReprt =  &lcPickTmp..lEndRep 
SELECT (lnCurAlias)
RETURN 0

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
FUNCTION lfwRepWhen

= lfvInvoice()
*--Ordline File
loDBFOrdline = CreateObject("RemoteTable","Ordline","ORDLINST",lcTempOrdline,SET("DATASESSION"))

*--PikLine File
loDBFPikLine = CreateObject("RemoteTable","PikLine","PikLine",lcTempPikLine,SET("DATASESSION"))

*--Ordhdr File
loDBFOrdhdr   = CreateObject("RemoteTable","Ordhdr","Ordhdr",lcTempOrdhd,SET("DATASESSION"))
  SELECT CORDTYPE,ACCOUNT,ORDER,cCurrCode,Nexrate,NcurrUnit,SHIPVIA FROM &lcTempOrdhd WHERE .F. into CURSOR &lcTempOrdHdr READWRITE 
  =lfMakeIndex(lcTempOrdHdr)

*--PikTkt file 
loDBFPikTkt  = CreateObject("RemoteTable","PikTkt","PikTkt",lcTempPikTkt,SET("DATASESSION"))
sELECT &lcTempPikTkt..*  FROM  &lcTempPikTkt WHERE .F. INTO CURSOR  &lcPickTkTmp READWRITE 
 =lfMakeIndex(lcPickTkTmp)
*--Customer file 
loDBFCust     = CreateObject("RemoteTable","Customer","Customer",lcTempCust,SET("DATASESSION"))
  SELECT TYPE,ACCOUNT,STORE,BTNAME FROM &lcTempCust WHERE .F. into CURSOR &lcTempCustomer READWRITE 
  =lfMakeIndex(lcTempCustomer)
  
IF TYPE('loPack_Hdr') <> 'O'
  loPack_Hdr = CreateObject("RemoteTable","Pack_hdr","Pack_hdr",'Pack_hdr',SET("DATASESSION"))
ENDIF  
  
*!*************************************************************
*! Name      : lfvInvoice
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
FUNCTION lfvInvoice

IF lcRPInv = 'Y'
  llRPRelPT = .F.
ENDIF  
lnRelPTPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LLRPRELPT'),1)
laOGObjCnt[lnRelPTPo] = lcRPInv $ 'BN'
= lfOGShowGet('LLRPRELPT')

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
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
SELECT Customer
SET ORDER TO Customer
GO TOP
*-- End of lfsrAcc.

*!*************************************************************
*! Name      : lfsrPkt
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrPkt()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol is Reset
*!*************************************************************
FUNCTION lfsrPkt
PARAMETERS lcParm
SELECT PIKTKT
LOCATE 
*-- End of lfsrPkt.
*!*************************************************************
*! Name      : lfFillVars
*: Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
FUNCTION lfFillVars

IF llMultCurr
  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF

*-- End Of lfFillVars.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.  &&You erase temporary file.
*-- Close temp. opended files, if it used.

IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign
ENDIF

*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfGtCurDes
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Function Get the Description ofthe Currency
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGtCurDes()
*!*************************************************************
FUNCTION lfGtCurDes

IF lcRpCurr <> "F"
  RETURN oAriaApplication.BaseCurrency      
ELSE
  IF llMultCurr and SEEK(&lcTempOrdHdr..cCurrCode,'SYCCURR')
    RETURN SYCCURR.cCurrDesc
  ENDIF
ENDIF  

RETURN ""
*-- end of lfGtCurDes.

*!**************************************************************************
*! Name      : lpCreaTemp
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Procedure to create Temp. File 
*!**************************************************************************
*! Example   : DO lpCreaTemp
*!**************************************************************************
PROCEDURE lpCreaTemp

*-- check If File is created or not
IF USED(lcPickTmp) AND RECCOUNT(lcPickTmp) > 0
  USE IN (lcPickTmp)
ENDIF
*-- Create File
IF !USED(lcPickTmp)
  IF TYPE("laTempStru[1,1]") $ "UL" 
    DIMENSION laTempStru[1,18]
    SELECT(lcTempPikTkt)
    =AFIELDS(laTempStru)
    =lfAddField("laTempStru","cCurrCode","C",3,0)
    =lfAddField("laTempStru","lEndRep","L",1,0)
    =lfAddField("laTempStru","SHIPVIA","C",15,0)
  ENDIF
  
*!*	  IF llMultCurr AND (lcRpCurr = "F") 
*!*	    =gfCrtTmp(lcPickTmp,@laTempStru,"cCurrCode + PIKTKT",lcPickTmp,.T.)
*!*	  ELSE 
*!*	    =gfCrtTmp(lcPickTmp,@laTempStru,"PIKTKT",lcPickTmp,.T.)
*!*	  ENDIF 
  IF llMultCurr AND (lcRpCurr = "F") 
   *: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[Start]
    * =gfCrtTmp(lcPickTmp,@laTempStru,IIF(LCRPSORT='P',"cCurrCode + PIKTKT","cCurrCode + SHIPVIA"),lcPickTmp,.T.)
    =gfCrtTmp(lcPickTmp,@laTempStru,IIF(LCRPSORT='P',"cCurrCode + PIKTKT","cCurrCode + SHIPVIA+ PIKTKT"),lcPickTmp,.T.)
    *: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[END]
  ELSE 
    *: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[Start]
    *=gfCrtTmp(lcPickTmp,@laTempStru,IIF(LCRPSORT='P',"PIKTKT","SHIPVIA"),lcPickTmp,.T.)
    =gfCrtTmp(lcPickTmp,@laTempStru,IIF(LCRPSORT='P',"PIKTKT","SHIPVIA+PIKTKT"),lcPickTmp,.T.)
    *: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[ENd]
  ENDIF 

  SELECT(lcPickTmp)
  ZAP
 
ENDIF
*-- End of lpCreaTemp.

*!**************************************************************************
*! Name      : lpColect
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04 
*! Purpose   : Procedure TO Colecte Data
*!**************************************************************************
*! Example   : Do lpColect()
*!**************************************************************************

PROCEDURE lpColect


*-- Check If user select a piktkt no. or not
lnPosPikTkt = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.PIKTKT")
IF lnPosPikTkt > 0 
  lnPosPikTkt = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikTkt,1)
  lcPikTktSel =IIF(!EMPTY(laOgFxFlt[lnPosPikTkt,6]),laOgFxFlt[lnPosPikTkt,6],'')
  IF !EMPTY(lcPikTktSel)
    SELECT(lcPikTktSel)
    LOCATE
    IF !EOF()
      SCAN 
  	    loDBFPikTkt.Seek(&lcPikTktSel..PIKTKT)
  	    SELECT(lcTempPikTkt)
  	    SCATTER MEMO MEMVAR 
  	    INSERT INTO (lcPickTkTmp) FROM MEMVAR 
      ENDSCAN     
      lcSelFlds  = "&lcPickTkTmp..*"
      lcSelFiles = lcPickTkTmp 
      lcSeleCond = ""
      *--Check if user select order no.
      lnPosOrder = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ORDER")
  	  IF lnPosOrder > 0 
	      lnPosOrder = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosOrder,1)
	      lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnPosOrder,6]),laOgFxFlt[lnPosOrder,6],'')
        IF !EMPTY(lcOrderSel)
          SELECT(lcOrderSel)
          LOCATE
          IF !EOF()
            lcSelFiles = lcSelFiles + "," + lcOrderSel
            lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ") + lcPickTkTmp + ".ORDER = " + lcOrderSel + ".ORDER"
	        ENDIF 
    	  ENDIF 
    	ENDIF   
      *--Check if user select warecode 
      lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
      IF lnPosWare > 0 
        lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
        lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
        IF !EMPTY(lcWareSel)
          SELECT(lcWareSel)
          LOCATE
          IF !EOF()
            lcSelFiles = lcSelFiles + "," + lcWareSel
            lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ") + lcPickTkTmp + ".CWARECODE = " + lcWareSel + ".CWARECODE"
          ENDIF 
        ENDIF 
      ENDIF
      *--Check if user select accounts 
      lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
      IF lnPosAcc > 0 
        lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
        lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosAcc,6]),laOgFxFlt[lnPosAcc,6],'')
        IF !EMPTY(lcAccSel)
          SELECT(lcAccSel)
          LOCATE
          IF !EOF()
            lcSelFiles = lcSelFiles + "," + lcAccSel
            lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ") + lcPickTkTmp + ".ACCOUNT = " + lcAccSel + ".ACCOUNT"
          ENDIF 
        ENDIF 
      ENDIF   
      *--Check Date
      lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
      IF lnPosDate > 0 
        lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
        SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
        EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
        
        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
        IF SUBSTR(laOgFxFlt[lnPosDate,6],1,1) = "|"
          SDATE = DTOC({})
          EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],2,11)
        ENDIF 
        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]

        
        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
        *IF !EMPTY(EDATE) AND !EMPTY(SDATE)
        IF (!EMPTY(EDATE) AND !EMPTY(SDATE)) OR (!EMPTY(EDATE) AND EMPTY(SDATE))
        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]
        
          lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ")+ "BETWEEN(&lcPickTkTmp..DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
        ENDIF 
      ENDIF
      SELECT(lcPickTkTmp)
      LOCATE 
      IF !EMPTY(lcSeleCond) 
        SELECT &lcSelFlds FROM &lcSelFiles WHERE &lcSeleCond INTO CURSOR &lcFnlPikTKT
      ELSE 
        SELECT &lcSelFlds FROM &lcSelFiles INTO CURSOR &lcFnlPikTKT
      ENDIF 
      SELECT(lcFnlPikTKT)
      =lfGetOrdhdrFile()
      RETURN 
  	ENDIF 
  ENDIF  
ENDIF 	
*--check if user select order no.
lnPosOrder = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ORDER")
IF lnPosOrder > 0 
  lnPosOrder = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosOrder,1)
  lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnPosOrder,6]),laOgFxFlt[lnPosOrder,6],'')
  IF !EMPTY(lcOrderSel)
    SELECT(lcOrderSel)
    LOCATE
    IF !EOF()
      loDBFPikTkt.SetOrder("ORDPIK")
      SCAN 
        loDBFPikTkt.seek(&lcOrderSel..ORDER)
        SELECT(lcTempPikTkt)
        SCAN REST WHILE order+piktkt = &lcOrderSel..ORDER
          SCATTER MEMO MEMVAR 
          INSERT INTO(lcPickTkTmp) FROM MEMVAR 
        ENDSCAN 
      ENDSCAN 
      loDBFPikTkt.SetOrder("PIKTKT")
      lcSelFiles = lcPickTkTmp
      lcSeleCond = ""
      lcSelFlds  = "&lcPickTkTmp..*"
      *--Check if user select warecode 
      lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
      IF lnPosWare > 0 
        lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
        lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
        IF !EMPTY(lcWareSel)
          SELECT(lcWareSel)
          LOCATE
          IF !EOF()
            lcSelFiles = lcSelFiles + "," + lcWareSel
            lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ") + lcPickTkTmp + ".CWARECODE = " + lcWareSel + ".CWARECODE"
          ENDIF 
        ENDIF 
      ENDIF
      *--Check if user select accounts 
      lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
      IF lnPosAcc > 0 
        lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
        lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosAcc,6]),laOgFxFlt[lnPosAcc,6],'')
        IF !EMPTY(lcAccSel)
          SELECT(lcAccSel)
          LOCATE
          IF !EOF()
            lcSelFiles = lcSelFiles + "," + lcAccSel
            lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ") + lcPickTkTmp + ".ACCOUNT = " + lcAccSel + ".ACCOUNT"
          ENDIF 
        ENDIF 
      ENDIF   
      *--Check Date
      lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
      IF lnPosDate > 0 
        lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
        SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
        EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
        
        
        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
        IF SUBSTR(laOgFxFlt[lnPosDate,6],1,1) = "|"
          SDATE = DTOC({})
          EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],2,11)
        ENDIF 
        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]

        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
        *IF !EMPTY(EDATE) AND !EMPTY(SDATE)
        IF (!EMPTY(EDATE) AND !EMPTY(SDATE)) OR (!EMPTY(EDATE) AND EMPTY(SDATE))        
        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]
        
          lcSeleCond = lcSeleCond  + IIF(EMPTY(lcSeleCond),""," AND ")+ "BETWEEN(&lcPickTkTmp..DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
        ENDIF 
      ENDIF  
      SELECT(lcPickTkTmp)
      LOCATE 
      IF !EMPTY(lcSeleCond) 
        SELECT &lcSelFlds FROM &lcSelFiles WHERE &lcSeleCond INTO CURSOR &lcFnlPikTKT
      ELSE 
        SELECT &lcSelFlds FROM &lcSelFiles INTO CURSOR &lcFnlPikTKT
      ENDIF         
      SELECT(lcFnlPikTKT)
      =lfGetOrdhdrFile()
      RETURN 
    ENDIF 
  ENDIF 
ENDIF   
*--if user select account,warehous,date
IF loDBFPikTkt.llnative
  lcSelectFile = lcTempPikTkt
  lcSelectCond = ""
  lcSelFields = lcTempPikTkt+".*"
  lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
  IF lnPosWare > 0 
    lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
    lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
    IF !EMPTY(lcWareSel)
      SELECT(lcWareSel)
      LOCATE
      IF !EOF()
        lcSelectFile = lcSelectFile + "," + lcWareSel
        lcSelectCond = lcSelectCond + IIF(EMPTY(lcSelectCond),""," AND ") + lcTempPikTkt + ".CWARECODE = " + lcWareSel + ".CWARECODE"
      ENDIF 
    ENDIF 
  ENDIF
  *--Check if user select accounts 
  lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
  IF lnPosAcc > 0 
    lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
    lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosAcc,6]),laOgFxFlt[lnPosAcc,6],'')
    IF !EMPTY(lcAccSel)
      SELECT(lcAccSel)
      LOCATE
      IF !EOF()
        lcSelectFile = lcSelectFile + "," + lcAccSel
        lcSelectCond = lcSelectCond + IIF(EMPTY(lcSelectCond ),""," AND ") + lcTempPikTkt + ".ACCOUNT = " + lcAccSel + ".ACCOUNT"
      ENDIF 
    ENDIF 
  ENDIF   
  *--Check Date
  lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
  IF lnPosDate > 0 
    lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
    SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
    EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
    
    *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
	IF SUBSTR(laOgFxFlt[lnPosDate,6],1,1) = "|"
	  SDATE = DTOC({})
	  EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],2,11)
	ENDIF 
	*: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]

   
    **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
    *IF !EMPTY(EDATE) AND !EMPTY(SDATE)
     IF (!EMPTY(EDATE) AND !EMPTY(SDATE)) OR (!EMPTY(EDATE) AND EMPTY(SDATE))    
    **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]
       lcSelectCond = lcSelectCond + IIF(EMPTY(lcSelectCond ),""," AND ")+ "BETWEEN(&lcTempPikTkt..DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
    ENDIF 
  ENDIF  
*!*	  SELECT(lcPickTkTmp)
*!*	  LOCATE 
  IF !EMPTY(lcSelectCond)
    SELECT &lcSelFields FROM &lcSelectFile WHERE  &lcSelectCond  INTO CURSOR &lcFnlPikTKT
  ELSE
    SELECT &lcSelFields FROM &lcSelectFile  INTO CURSOR &lcFnlPikTKT
  ENDIF   
  SELECT(lcFnlPikTKT)
  =lfGetOrdhdrFile()
  RETURN 
ELSE 
  lcSelectFile = PikTkt
  lcSelectCond = ""
  lcSelFields = "PikTkt.*"
  lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
  IF lnPosWare > 0 
    lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
    lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
    IF !EMPTY(lcWareSel)
      SELECT(lcWareSel)
      LOCATE
      IF !EOF()
        lcCurName = lcWareSel
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
          IF (RECCOUNT() > 0) 
            lcSQLWare = loOgScroll.gfSQLTempName('','Cwarecode C(10)',lcCurName,'Cwarecode')
            lcSelectFile = lcSelectFile + "," + lcSQLWare
            lcSelectCond =IIF(EMPTY(lcSelectCond),""," AND ") + "PikTkt.CWARECODE = " + lcSQLWare + ".CWARECODE"
          ENDIF 
        ENDIF     
      ENDIF 
    ENDIF 
  ENDIF
  *--Check if user select accounts 
  lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
  IF lnPosAcc > 0 
    lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
    lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosAcc,6]),laOgFxFlt[lnPosAcc,6],'')
    IF !EMPTY(lcAccSel)
      SELECT(lcAccSel)
      LOCATE
      IF !EOF()
        lcCurName = lcAccSel
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
           IF (RECCOUNT() > 0) 
             lcSQLAcc = loOgScroll.gfSQLTempName('','Account C(5)',lcCurName,'Account')
             lcSelectFile = lcSelectFile + "," + lcSQLAcc
             lcSelectCond = IIF(EMPTY(lcSelectCond ),""," AND ") + "PikTkt.ACCOUNT = " + lcSQLAcc + ".ACCOUNT"
           ENDIF 
         ENDIF   
      ENDIF 
    ENDIF 
  ENDIF   
  *--Check Date
  lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
  IF lnPosDate > 0 
    lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
    SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
    EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
    
    **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
    IF SUBSTR(laOgFxFlt[lnPosDate,6],1,1) = "|"
      SDATE = DTOC({})
      EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],2,11)
    ENDIF 
    **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]

    **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
    *IF !EMPTY(EDATE) AND !EMPTY(SDATE)
    IF (!EMPTY(EDATE) AND !EMPTY(SDATE)) OR (!EMPTY(EDATE) AND EMPTY(SDATE))
    **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]
    
       lcSelectCond = IIF(EMPTY(lcSelectCond ),""," AND ")+ " PikTkt..DATE BETWEEN "+CTOD(SDATE)+" AND "+CTOD(EDATE)
    ENDIF 
  ENDIF  
  =lfOpenSql(lcSelFields ,lcSelectFile  ,lcFnlPikTKT,lcSelectCond)
  SELECT(lcFnlPikTKT)
  =lfGetOrdhdrFile()
  RETURN 
ENDIF 
*------------------------
*---------------------------------
****************************************************************
*!*  PRIVATE  lcPikTktSel,lcOrderSel,lcAccSel,lcWareSel
*!*  lcWherePik = "Piktkt.Piktkt # '******' .AND. !EMPTY(Piktkt.Piktkt)"
*!*  lcTablePik =  "piktkt"
*!*  *--Getting the file which Contains the selected picking tickets 
*!*  lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
*!*  IF lnPosDate > 0 
*!*    lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
*!*    SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
*!*    EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
*!*    IF !EMPTY(EDATE) AND !EMPTY(SDATE)
*!*      lcWherePik = lcWherePik+IIF(!EMPTY(lcWherePik),' AND  ' ,"" )+"BETWEEN(PIKTKT.DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
*!*    ENDIF 
*!*  ENDIF   
*!*  lnPosPikTkt = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.PIKTKT")
*!*  IF lnPosPikTkt > 0 
*!*    lnPosPikTkt = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikTkt,1)
*!*    lcPikTktSel =IIF(!EMPTY(laOgFxFlt[lnPosPikTkt,6]),laOgFxFlt[lnPosPikTkt,6],'')
*!*    IF !EMPTY(lcPikTktSel)
*!*      SELECT(lcPikTktSel)
*!*      LOCATE
*!*      IF !EOF()
*!*  	  COPY TO oariaapplication.workdir+lcTempPiktk +".dbf"
*!*        lcTablePik = lcTablePik  +",'"+oariaapplication.workdir+lcTempPiktk +".dbf'"
*!*  	  lcWherePik = lcWherePik+IIF(!EMPTY(lcWherePik),' AND  ' ,"" )+ "PIKTKT.PIKTKT = "+lcTempPiktk +".PIKTKT"
*!*      ENDIF 
*!*    ENDIF 
*!*  ENDIF   
*!*  *--Getting the file which Contains the selected orders
*!*  lnPosOrder = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ORDER")
*!*  IF lnPosOrder > 0 
*!*    lnPosOrder = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosOrder,1)
*!*    lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnPosOrder,6]),laOgFxFlt[lnPosOrder,6],'')
*!*    IF !EMPTY(lcOrderSel)
*!*      SELECT(lcOrderSel)
*!*      LOCATE
*!*      IF !EOF()

*!*  	  COPY TO oariaapplication.workdir+lcTempOrder +".dbf"
*!*        lcTablePik = lcTablePik  +",'"+oariaapplication.workdir+lcTempOrder +".dbf'"
*!*  	  lcWherePik = lcWherePik +IIF(!EMPTY(lcWherePik),' AND  ' ,"" )+ "PIKTKT.ORDER = "+lcTempOrder +".ORDER"
*!*      ENDIF 
*!*    ENDIF 
*!*  ENDIF   
*!*  *--Getting the file which Contains the selected warehouses
*!*  lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
*!*  IF lnPosWare > 0 
*!*    lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
*!*    lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
*!*    IF !EMPTY(lcWareSel)
*!*      SELECT(lcWareSel)
*!*      LOCATE
*!*      IF !EOF()

*!*  	  COPY TO oariaapplication.workdir+lcTempWare+".dbf"
*!*        lcTablePik = lcTablePik  +",'"+oariaapplication.workdir+lcTempWare+".dbf'"
*!*  	  lcWherePik = lcWherePik +IIF(!EMPTY(lcWherePik),' AND  ' ,"" )+ "PIKTKT.cwarecode = "+lcTempWare +".cwarecode"
*!*      ENDIF 
*!*      ENDIF 
*!*  ENDIF   
*!*  *--Getting the file which Contains the selected accounts
*!*  lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
*!*  IF lnPosAcc > 0 
*!*    lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
*!*    lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosAcc,6]),laOgFxFlt[lnPosAcc,6],'')
*!*    IF !EMPTY(lcAccSel)
*!*      SELECT(lcAccSel)
*!*      LOCATE
*!*      IF !EOF()
*!*        
*!*  	  COPY TO oariaapplication.workdir+lcTempacc+".dbf"
*!*        lcTablePik = lcTablePik  +",'"+oariaapplication.workdir+lcTempacc+".dbf'"
*!*  	  lcWherePik = lcWherePik + IIF(!EMPTY(lcWherePik),' AND  ' ,"" )+ "PIKTKT.account = "+lcTempacc +".account"
*!*      ENDIF 
*!*    ENDIF 
*!*  ENDIF   

*!*  lcSlctFldSPik = "piktkt.*"
*!*  =lfOpenFox(lcSlctFldSPik,lcTablePik ,lcPickTkTmp,lcWherePik)
*!*  *lcTempTktPick = loOGScroll.gfTempName()
*!*  SELECT(lcPickTkTmp)
*!*  COPY TO oariaapplication.workdir+lcTempTktPick+".dbf"
*!*  lcSeleOrdfld = "Ordhdr.ACCOUNT,ORDHDR.ORDER,ORDHDR.CORDTYPE,ORDHDR.cCurrCode"
*!*  lcSelordcon = "ordhdr.cordtype+ordhdr.order ='O'+"+lcTempTktPick+".ORDER "
*!*  lcSelTable = "ordhdr,'"+oariaapplication.workdir+lcTempTktPick+".dbf'"
*!*  =lfOpenFox(lcSeleOrdfld ,lcSelTable ,lcTempOrdHdr,lcSelordcon )

*-- end of lpColect.

*!**************************************************************************
*! Name      : lfAddField
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Add fields to the the array of file structer
*!**************************************************************************
*! Example   : =lfAddField()
*!**************************************************************************
*!E301439,1 

FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec
lnFldPos  = ALEN(&lcStruArry,1) + 1
DIMENSION &lcStruArry[lnFldPos , 18]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec
STORE ' ' TO  &lcStruArry[lnFldPos,7],&lcStruArry[lnFldPos,8],;
              &lcStruArry[lnFldPos,9],&lcStruArry[lnFldPos,10],;
              &lcStruArry[lnFldPos,11],&lcStruArry[lnFldPos,12],;
              &lcStruArry[lnFldPos,13],&lcStruArry[lnFldPos,14],;
              &lcStruArry[lnFldPos,15],&lcStruArry[lnFldPos,16]
STORE 0 TO    &lcStruArry[lnFldPos,17] ,&lcStruArry[lnFldPos,18]

*-- end of lfAddField.


*!*************************************************************
*! Name      : lfvRepCurr
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : 
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRepCurr()
*!*************************************************************
FUNCTION lfvRepCurr

llOGFltCh = .T.
=gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfOpenFox
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to open FOX tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenFox
LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial

LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")


LOCAL lnConNum
lnConNum = 0
lnConnectionHandlar = loOGScroll.oRDA.SqlRun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.cAriaNativeDataFilesConStr,3,;
                                      'SAVE',SET("DATASESSION"),.F.,@lnConNum)
IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)
  =CURSORSETPROP("Buffering",5,lcCursor)
 * loOGScroll.oRDA.oConnectionsClass.Close(lnConNum)
ELSE
  =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
*  loOGScroll.oRDA.oConnectionsClass.Close(lnConNum)
  RETURN .F.
ENDIF

*-- end of lfOpenSql.
*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
  DO CASE

   *--temp. Customer File
   CASE UPPER(lcTable) = lcTempCustomer
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'TYPE+ACCOUNT+STORE'
     laIndex[1,2] = lcTempCustomer

   *--temp. ordhdr file
   CASE UPPER(lcTable) =  lcTempOrdHdr
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'CORDTYPE+ORDER'
     laIndex[1,2] = lcTempOrdHdr

   *--temp. ordline file
   CASE UPPER(lcTable) =  lcTempOrdLine   
      DIMENSION laIndex[1,2]

        laIndex[1,1] = 'CORDTYPE+ORDER+STORE'
        laIndex[1,2] = lcTempOrdLine
 
   *-- temp. piktkt file lcPickTkTmp
   CASE UPPER(lcTable) = lcPickTkTmp
      DIMENSION laIndex[4,2]
      laIndex[1,1] = 'PIKTKT'
      laIndex[1,2] = lcPickTkTmp
      laIndex[2,1] = 'ORDER'
      laIndex[2,2] = lcIndOrd
      laIndex[3,1] = 'CWARECODE'
      laIndex[3,2] = lcIndWare
      laIndex[4,1] = 'ACCOUNT'
      laIndex[4,2] = lcIndAcc
 
   *--temp. pikline file
   CASE UPPER(lcTable) =  lcTempPikLin
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'piktkt'
      laIndex[1,2] = lcTempPikLin


ENDCASE
*!**************************************************************************
*! Name      : lfCollTime
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!**************************************************************************
*! Returns            : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- END OF lfCollTime.
*!*************************************************************
*! Name      : lfMakeIndex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/03/05
*! Purpose   : function to make index on a temp. file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMakeIndex
PARAMETERS lcTempName
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcCursor = lcTempName
lnBuffering = CURSORGETPROP("Buffering",lcCursor)
 =CURSORSETPROP("Buffering",3,lcCursor)
*-- To initialize the indecis that will be created for each file
=lfCrtindex(lcCursor)
SELECT (lcCursor)
FOR lnI = 1 TO ALEN(laIndex,1)
  lcIndex = laIndex[lnI,1]
  lcTag   = laIndex[lnI,2]
  INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
ENDFOR
lcTag = laIndex[1,2]
SET ORDER TO TAG (lcTag)

*!*************************************************************
*! Name      : lfGetOrdhdrFile
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/03/05
*! Purpose   : function to get Orhdr Data
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetOrdhdrFile

lcScanExp = "IIF(lcRpInv = 'B',IIF(llRPRelPT,.T.,&lcFnlPikTKT..Status $ 'OCHP'),"
lcScanExp = lcScanExp +" IIF(lcRpInv = 'Y' ,  &lcFnlPikTKT..Status$'C',"
lcScanExp = lcScanExp +"IIF(llRPRelPT,&lcFnlPikTKT..Status $ 'OHPX',&lcFnlPikTKT..Status $ 'OHP' ))) =  .T.  AND"
lcScanExp = lcScanExp +" IIF(lcRpPrint='B',.T.,"
lcScanExp = lcScanExp +"IIF(lcRpPrint='Y' , &lcFnlPikTKT..PrtFlag= 'P',&lcFnlPikTKT..PrtFlag<>'P')) =  .T."

SELECT(lcFnlPikTKT)
LOCATE
SCAN
*loDBFOrdhdr   = CreateObject("RemoteTable","Ordhdr","Ordhdr",lcTempOrdhd,SET("DATASESSION"))
  loDBFOrdhdr.Seek("O"+&lcFnlPikTKT..ORDER)
  SELECT(lcTempOrdhd)
  SCATTER FIELDS ACCOUNT,ORDER,CORDTYPE,cCurrCode,Nexrate,NcurrUnit,SHIPVIA MEMO MEMVAR 
  INSERT INTO (lcTempOrdHdr) FROM MEMVAR 
ENDSCAN 
*--get Customer file 
SELECT(lcFnlPikTKT)
LOCATE
SCAN
  IF EMPTY(&lcFnlPikTKT..store)
    loDBFCust.Seek("M"+&lcFnlPikTKT..account+&lcFnlPikTKT..Store)
  ELSE 
    loDBFCust.Seek("S"+&lcFnlPikTKT..account+&lcFnlPikTKT..Store)
  ENDIF 
  SELECT(lcTempCust)
  SCATTER FIELDS TYPE,ACCOUNT,STORE,BTNAME MEMO MEMVAR 
  INSERT INTO (lcTempCustomer) FROM MEMVAR 
ENDSCAN 

SELECT(lcFnlPikTKT)
SET RELATION TO 'O' + Order INTO &lcTempOrdHdr ADDITIVE

SELECT(lcFnlPikTKT)
SCAN FOR &lcScanExp AND Piktkt # '******' .AND. !EMPTY(Piktkt)
  SCATTER MEMVAR MEMO
  m.cCurrCode = &lcTempOrdHdr..cCurrCode
  M.SHIPVIA=   SUBSTR(gfCodDes(EVAL(LCTEMPORDHDR+'.SHIPVIA'),'SHIPVIA'),1,20)
  INSERT INTO (lcPickTmp) FROM MEMVAR
ENDSCAN
SET RELATION TO  
*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/08/2004
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenSql

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.
