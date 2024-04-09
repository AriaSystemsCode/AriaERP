*:**************************************************************************
*: Program file  : ARPINVRV
*: Program desc. : Custom invoice form based on Form H For REVUE
*: System        : Aria Advantage Series.4XP
*: Module        : Accounts Receivable (AR)
*: Developer     : Mariam Mazhar (MMT)
*: Date          : 06/30/2008
*: Reference     : C201020
*:**************************************************************************
lcRpPrSt = IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt)
IF EMPTY(lcRpFac1) AND EMPTY(lcRpFac2) AND EMPTY(lcRpFac3)
  IF FILE(oAriaApplication.DataDir+lcFacStNam+'.MEM')
    RESTORE FROM oAriaApplication.DataDir+lcFacStNam+'.MEM' ADDITIVE
  ENDIF
ENDIF

DIMENSION laScalArr[19,4]

laScalArr[1,1] = 'S1'
laScalArr[1,2] = 'C'
laScalArr[1,3] = 3
laScalArr[1,4] = 0

laScalArr[2,1] = 'S2'
laScalArr[2,2] = 'C'
laScalArr[2,3] = 3
laScalArr[2,4] = 0


laScalArr[3,1] = 'INVOICE'
laScalArr[3,2] = 'C'
laScalArr[3,3] = 6
laScalArr[3,4] = 0


lnCount = 4
FOR lnI = 1 TO 2
 FOR lnJ = 1 To 8
  laScalArr[lnCount ,1] = 'SZ'+STR(lnI,1)+STR(lnJ,1)
  laScalArr[lnCount ,2] = 'C'
  laScalArr[lnCount ,3] = 5
  laScalArr[lnCount ,4] = 0
  lnCount = lnCount + 1
 ENDFOR  
ENDFOR 



=gfCrtTmp(lcTempScale ,@laScalArr,"INVOICE",lcTempScale,.F.)




llAltShp=.F.
XTAX      = IIF(gfGetMemVar("M_TAX",oAriaApplication.ActiveCompanyID)='Y', .T. , .F.)  && llTax 
XTAX_DESC = gfGetMemVar('M_TAX_DESC',oAriaApplication.ActiveCompanyID)&&lcTaxDesc 
XTAX_RATE = gfGetMemVar('M_TAX_RATE',oAriaApplication.ActiveCompanyID)
XTAX_METH = gfGetMemVar('M_TAX_METH',oAriaApplication.ActiveCompanyID)&&lcTaxMeth 
lcTaxRefr = gfGetMemVar('M_TAX_REFE',oAriaApplication.ActiveCompanyID) &&lcTaxRefDs
llIsCanada = IIF(UPPER(ALLTRIM(oAriaApplication.DefaultCountry)) = 'CANADA', .T., .F.)


STORE ''TO xStName,xStAddr1,xStAddr2,xStAddr3,;
           xBtName,xBtAddr1,xBtAddr2,xBtAddr3
STORE ''   TO PTerms,PShipVia,lcFACTACCT
lntotqty = 0
lnAmount = 0

lcDist = ' '
llNote = llRpInvNot

SELECT INVHDR
STORE TRIM(lcCompName)   TO HLINE1
STORE TRIM(laCompAdd[1]) TO HLINE2
STORE TRIM(laCompAdd[2]) TO HLINE3
STORE TRIM(laCompAdd[3]) TO HLINE4
STORE laCompAdd[6] 		 TO HLINE5
IF EMPTY(HLINE3)
  STORE HLINE4 TO HLINE3
  STORE HLINE5 TO HLINE4
  STORE ''     TO HLINE5
ENDIF

lcZone = ''
DECLARE laZone[1,2]
laZone[1,1]     = 'CUPS'
laZone[1,2]     = 'lcZone'


SELECT INVHDR
DIMENSION laInvhdStr[1,18]
lnHLen = AFIELDS(laInvhdStr)
DIMENSION laInvhdStr[lnHLen +1,18]
laInvhdStr[lnHLen +1 ,1] = 'MTotals'
laInvhdStr[lnHLen +1 ,2] = 'M'
laInvhdStr[lnHLen +1 ,3] = 10
laInvhdStr[lnHLen +1 ,4] = 0

STORE '' TO laInvhdStr[lnHLen +1,7],laInvhdStr[lnHLen +1,8],laInvhdStr[lnHLen +1,9],;
    laInvhdStr[lnHLen +1,10],laInvhdStr[lnHLen +1,11],laInvhdStr[lnHLen +1,12],;
    laInvhdStr[lnHLen +1,13],laInvhdStr[lnHLen +1,14],laInvhdStr[lnHLen +1,15],;
    laInvhdStr[lnHLen +1,16]
    
STORE 0 TO  laInvhdStr[lnHLen +1,17],laInvhdStr[lnHLen +1,18]


=gfCrtTmp(lcTmpInvhdr  ,@laInvhdStr,"INVOICE",lcTmpInvhdr  ,.F.)

SELECT INVLINE
AFIELDS(laInvLStr)
=gfCrtTmp(lcTmpInvLine ,@laInvLStr,"INVOICE+STR(LINENO,6)",lcTmpInvLine ,.F.)

SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  =gfModalGen('TRM00052B40011','ALERT')
  lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
SELECT INVHDR
LOCATE 
SCAN FOR &lcASExp
  SCATTER MEMO MEMVAR 
  SELECT (lcTmpInvhdr)  
  APPEND BLANK 
  GATHER MEMO MEMVAR 
  REPLACE MTotals WITH  "'TOTAL - M E R C H A N D I S E'+SPACE(8)+STR(XTOTPIECES,6,0)+SPACE(11)+STR(XSUTOTAL,10,2)+CHR(13)"+;
          "+IIF(Freight + Insur + Cod<>0,'TOTAL - F R E I G H T'+space(33)+STR(Freight + Insur + Cod,10,2)+CHR(13),'')"+;  
          "+CHR(13)+IIF(Discount <> 0,'TOTAL - D I S C O U N T'+SPACE(10)+STR(DiscPcnt,6,2)+ ' %'+SPACE(12)+STR(discount,11,2)+CHR(13),'')"+;
          "+IIF(llTAx  ,IIF(!EMPTY(xTax_Desc),PADR(xTax_Desc ,20),SPACE(20))+SPACE(10)+IIF(!EMPTY(LCTAXREFR),PADR(LCTAXREFR,15),SPACE(15))+STR(TAX_RATE,5,2)+'%'+SPACE(3)+STR(TAX_AMT,10,2)+CHR(13),'')"+;
          "+IIF(llTAx and lliscanada,'P S T  T A X'+SPACE(33)+STR(NPSTRATE,5,2)+'%'+SPACE(3)+STR(NPSTAMT,10,2)+CHR(13),'')"+;
          "+IIF(llTAx and lliscanada ,'H S T  T A X'+SPACE(33)+STR(NHSTRATE,5,2)+'%'+SPACE(3)+STR(NHSTAMT,10,2)+CHR(13),'')+"+;
          "IIF(lltaxmetha ,IIF(!EMPTY(xTax_Desc ),PADR(xTax_Desc ,20),SPACE(20))+SPACE(10)+IIF(!EMPTY(LCTAXREFR),PADR(LCTAXREFR,15),SPACE(15))+STR(TAX_RATE,5,2)+'%'+SPACE(3)+STR(TAX_AMT,10,2)+CHR(13),'')"+;
          "+IIF(lltaxmetha and lliscanada ,'P S T  T A X'+SPACE(33)+STR(NPSTRATE,5,2)+'%'+SPACE(3)+STR(NPSTAMT,10,2)+CHR(13),'')"+;
          "+IIF(lltaxmetha AND lliscanada ,'H S T  T A X'+SPACE(33)+STR(NHSTRATE,5,2)+'%'+SPACE(3)+STR(NHSTAMT,10,2),'')"
          
  SELECT InvLine
  IF SEEK(m.Invoice)
    SELECT InvLine
    SCAN REST WHILE Invoice=m.Invoice
      SCATTER MEMO MEMVAR 
      SELECT (lcTmpInvLine)
      APPEND BLANK 
      GATHER MEMO MEMVAR 
    ENDSCAN 
  ENDIF 
ENDSCAN 

SELECT (lcTmpInvhdr)
LOCATE 
SET RELATION TO Invoice INTO INVHDR ADDITIVE 
SET RELATION TO Invoice INTO (lcTmpInvLine) ADDITIVE 
SET RELATION TO Invoice INTO (lcTempScale)ADDITIVE 
SET SKIP TO (lcTmpInvLine)


gfDispRe()        
lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
*:**************************************************************************
*! Name      : lfFacMsg
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/30/2008
*! Purpose   : Function to get Factor Statement from the User
*!             [Validation function for the Push button Factor Statement]
*:**************************************************************************
*! Called from : Option Grid    [Factor Statement option]
*:**************************************************************************
*! Calls       : gfOptMsg()
*:**************************************************************************
*
FUNCTION lfFacMsg
PARAMETER llDummy
llDummy = .T.
PRIVATE laFacMsg
DECLARE laFacMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laFacMsg[1,1] = 'lcRpFac1'        && 1st. line Variable
laFacMsg[2,1] = 'lcRpFac2'        && 2nd. line Variable
laFacMsg[3,1] = 'lcRpFac3'        && 2nd. line Variable

laFacMsg[1,2] = 75                && Line length

IF EMPTY(lcRpFac1) AND EMPTY(lcRpFac2) AND EMPTY(lcRpFac3)

  IF FILE(oAriaApplication.DataDir+lcFacStNam+'.MEM')
    RESTORE FROM oAriaApplication.DataDir+lcFacStNam+'.MEM' ADDITIVE
  ENDIF

  =gfOptMsg('laFacMsg','Factor Statement')

  SET MEMOWIDTH TO 75              && the length of the memo field.
  SAVE TO oAriaApplication.DataDir+lcFacStNam+'.MEM' ALL LIKE lcRpFac*

ELSE
  =gfOptMsg('laFacMsg','Factor Statement')
ENDIF
RETURN llDummy
*-- End of lfFacMsg

*!*************************************************************
*! Name      : lpGetHdr
*: Developer : Marim Mazahr
*: Date      : 06/30/2008
*!*************************************************************
*! Synopsis : Get information for invoice heder.
*!*************************************************************
PROCEDURE lpGetHdr

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'

xNote1 = IIF(Note1<>'*',Note1, '')
xNote2 = IIF(Note2<>'*',Note2, '')
xPhone = IIF(!EMPTY(Phone),Phone,'')

lntotqty = 0
lnAmount = 0 
*--Get the bill to and ship to address.
SELECT CUSTOMER
lcFactAcct = IIF(SEEK('M'+&lcTmpInvhdr..ACCOUNT),FactAcct,'')
SEEK IIF(&lcTmpInvhdr..STORE= SPACE(8),'M'+&lcTmpInvhdr..ACCOUNT,'S'+&lcTmpInvhdr..ACCOUNT+&lcTmpInvhdr..STORE)
      
=lfSolShp()

XBTNAME  = lcSolTName
XBTADDR1 = laSoldTo[1]
XBTADDR2 = laSoldTo[2]
XBTADDR3 =TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
IF LEN(TRIM(laSoldTo[2])) =0
  XBTADDR2 = laSoldTo[3]
  XBTADDR3 = ''
ENDIF

XSTNAME  = lcShpTName
XSTADDR1 = laShipTo[1]
XSTADDR2 = laShipTo[2]
XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
IF LEN(TRIM(laShipTo[2])) =0
  XSTADDR2 = laShipTo[3]
  XSTADDR3 = ''
ENDIF

*--Get the description about the codes.

PTERMS     = gfCodDes(&lcTmpInvhdr..CTERMCODE,'CTERMCODE')
PSHIPVIA   = gfCodDes(&lcTmpInvhdr..SHIPVIA,'SHIPVIA')
= gfRltFld(&lcTmpInvhdr..SHIPVIA , @laZone , 'SHIPVIA')
PSHIPVIA = IIF(TRIM(PSHIPVIA) = "N/A" , "" ,TRIM(PSHIPVIA)) &&&+XZN

lfGetScale()

SELECT (lcTmpInvhdr)
RETURN

*!*************************************************************
*! Name      : lfSolShp
*! Developer : Mariam Mazhar
*! Date      : 06/30/2008
*! Purpose   : Function to Get the Sold to & Ship to Address
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfSolShp
PARAMETER lcSolShp
PRIVATE lcAlias , lcInvHdr

STORE '' TO lcShpTName

lcInvHdr = SELECT(0)

SELECT INVHDR


=SEEK(&lcTmpInvhdr..Invoice,'INVHDR','INVHDR')


DECLARE laSoldTo[5,1] , laShipTo[5,1] , laFactor[5,1]

laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
laFactor = ''


IF !EMPTY(INVHDR.CFACCODE)
  =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

lcSolTName = CUSTOMER.BTName
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

=SEEK('O' + Invhdr.order,'ORDHDR','ORDHDR')

IF OrdHdr.Alt_ShpTo
  lcShpTName  = OrdHdr.STName  
  laShipTo[1] = OrdHdr.cAddress1
  laShipTo[2] = OrdHdr.cAddress2
  laShipTo[3] = OrdHdr.cAddress3
  laShipTo[4] = OrdHdr.cAddress4
  laShipTo[5] = OrdHdr.cAddress5
ELSE 
  lnCUSRec = 0
  lcAlias = SELECT(0)
  SELECT CUSTOMER
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
  ENDIF

  lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))
  
  laShipTo[1] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF BETWEEN(lnCusRec , 1 , RECCOUNT('CUSTOMER'))
    GOTO lnCusRec IN CUSTOMER
  ENDIF  
  SELECT(lcAlias)
ENDIF

=lfAdrShift('laShipTo')

SELECT(lcTmpInvhdr)
RETURN ''

*-- End of lfSolShp.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mariam MAzhar
*! Date      : 06/30/2008
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPINVOL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 6
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

FUNCTION lfGetScale

=SEEK(&lcTmpInvhdr..Invoice,'Invline')
  SELECT INVLINE
  lnSclCount = 1
SCAN REST WHILE INVOICE+STR(LINENO,6) = &lcTmpInvhdr..Invoice
  lntotqty = lntotqty + TOTQTY
  lnAmount = lnAmount +TOTQTY * Price
  IF lnSclCount >  2
    LOOP 
  ENDIF 
  IF SEEK(&lcTmpInvhdr..INVOICE,lcTempScale)  
  llFound = .F.
  FOR lnI = 1 TO 2
    lcI = STR(lnI,1)
    IF &lcTempScale..S&lcI = INVLINE.Scale
      llFound = .T.
      EXIT 
    ENDIF 
  ENDFOR 
  IF !llFound 
    IF SEEK('S'+INVLINE.Scale,'Scale')
      IF !SEEK(&lcTmpInvhdr..INVOICE,lcTempScale)  
        SELECT(lcTempScale)
        APPEND BLANK 
        lcSclCount = STR(1,1)
        REPLACE S1  WITH INVLINE.Scale,;
  		          SZ11  WITH Scale.SZ1,;
  		          SZ12  WITH Scale.SZ2,;
  		          SZ13  WITH Scale.SZ3,;
  		          SZ14  WITH Scale.SZ4,;
  		          SZ15  WITH Scale.SZ5,;
  		          SZ16  WITH Scale.SZ6,;
  		          SZ17  WITH Scale.SZ7,;
  		          SZ18  WITH Scale.SZ8,;
  		          invoice WITH &lcTmpInvhdr..invoice
      ELSE
        SELECT(lcTempScale)
        lcSclCount = STR(lnSclCount ,1)
        REPLACE S&lcSclCount  WITH INVLINE.Scale,;
  		          Sz&lcSclCount.1  WITH Scale.SZ1,;
  		          SZ&lcSclCount.2  WITH Scale.SZ2,;
  		          SZ&lcSclCount.3  WITH Scale.SZ3,;
  		          SZ&lcSclCount.4  WITH Scale.SZ4,;
  		          SZ&lcSclCount.5  WITH Scale.SZ5,;
  		          SZ&lcSclCount.6  WITH Scale.SZ6,;
  		          SZ&lcSclCount.7  WITH Scale.SZ7,;
  		          SZ&lcSclCount.8  WITH Scale.SZ8
      ENDIF 
      lnSclCount = lnSclCount + 1
    ENDIF 
  ELSE
    LOOP   
  ENDIF 
ELSE
  =SEEK('S'+INVLINE.Scale,'Scale')
  SELECT(lcTempScale)
  APPEND BLANK 
  lcSclCount = STR(1,1)
  REPLACE S1  WITH INVLINE.Scale,;
      SZ11  WITH Scale.SZ1,;
      SZ12  WITH Scale.SZ2,;
      SZ13  WITH Scale.SZ3,;
      SZ14  WITH Scale.SZ4,;
      SZ15  WITH Scale.SZ5,;
      SZ16  WITH Scale.SZ6,;
      SZ17  WITH Scale.SZ7,;
      SZ18  WITH Scale.SZ8,;
      invoice WITH &lcTmpInvhdr..invoice
  lnSclCount = lnSclCount + 1          
ENDIF   
ENDSCAN 

*!***************************************************************
*! Name      : lpPrtSku.
*! Develpmer : Mariam Mazhar[MMT]
*! Date      : 06/30/2008
*! Purpose   : Get SKU
*!***************************************************************
PROCEDURE lpPrtSku
IF !SEEK('S'+&lcTmpInvLine..Account+&lcTmpInvLine..Style,'Spck_Lin')
  RETURN ''
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  =SEEK('S'+&lcTmpInvLine..Scale,'Scale')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 

  DIME laSku[8]
  laSku = SPACE(16)
   SCAN WHILE Type+Account+Style = 'S'+&lcTmpInvLine..Account+&lcTmpInvLine..Style AND lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku(lnX)=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN

  lnI = 1
  = SEEK('S'+&lcTmpInvLine..Account+&lcTmpInvLine..Style,'Spck_Lin')

  DO WHILE Type+Account+Style = 'S'+&lcTmpInvLine..Account+&lcTmpInvLine..Style AND lnI < 9
    lcStrToPrn = 'SKU N#' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = ;
             'S'+&lcTmpInvLine..Account+&lcTmpInvLine..Style AND !EOF()

      lcI = STR(lnI,1)
      lcStrToPrn = lcStrToPrn + Scale.Sz&lcI+':'+laSku(lnI) + ' '
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
	RETURN lcStrToPrn
  ENDDO  
ELSE
  RETURN  PADR(Sku,15) + 'CUSTOMER SKU #'
ENDIF
RETURN
*!***************************************************************
*! Name      : lfchecklast
*! Develpmer : Mariam Mazhar[MMT]
*! Date      : 06/30/2008
*! Purpose   : Get last style Line NO.
*!***************************************************************
FUNCTION lfchecklast
lcAlias = ALIAS()
lcInvoice = &lcTmpInvhdr..invoice
SELECT (lcTmpInvLine) 

lnRecNo = RECNO()
lnLineNo = 0
SCAN FOR invoice  = lcInvoice 
  lnLineNo = lineno 
ENDSCAN
GO RECORD lnRecNo

SELECT (lcAlias)
RETURN lnLineNo
*!***************************************************************
*! Name      : lfGetTotal
*! Develpmer : Mariam Mazhar[MMT]
*! Date      : 06/30/2008
*! Purpose   : Get Line Totals
*!***************************************************************
FUNCTION lfGetTotal
PARAMETERS lnLineNo
IF lnLineNo = 51
  IF XTAX AND XTAX_METH = 'M' AND INVHDR.TAX_AMT # 0
    IF !EMPTY(lcTaxRefr)
      RETURN lcTaxRefr
    ELSE
      XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
      RETURN SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'+SPACE(7)+STR(INVHDR.TAX_AMT,10,2)
    ENDIF 
  ELSE
    WKAMT = FREIGHT + INSUR + COD
    IF WKAMT <> 0
      RETURN 'TOTAL - FREIGHT' + SPACE(4)+STR(WKAMT,10,2)
    ELSE
      IF DISCOUNT<>0
        RETURN 'DISCOUNT' + SPACE(11)+STR(DISCOUNT,10,2)
      ENDIF   
    ENDIF   
  ENDIF 
ELSE
  IF lnLineNo = 52
    IF XTAX AND XTAX_METH = 'M' AND INVHDR.TAX_AMT # 0
      IF !EMPTY(lcTaxRefr)
        XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
        RETURN SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'+SPACE(7)+STR(INVHDR.TAX_AMT,10,2)
      ELSE
        WKAMT = FREIGHT + INSUR + COD
        IF WKAMT <> 0
          RETURN 'TOTAL - FREIGHT' + SPACE(4)+STR(WKAMT,10,2)
        ELSE
          IF DISCOUNT<>0
            RETURN 'DISCOUNT' + SPACE(11)+STR(DISCOUNT,10,2)
          ENDIF   
        ENDIF   
      ENDIF 
    ELSE
      WKAMT = FREIGHT + INSUR + COD
      IF WKAMT <> 0 
        IF  DISCOUNT<>0 
          RETURN 'DISCOUNT' + SPACE(11)+STR(DISCOUNT,10,2)
        ENDIF
      ENDIF 
 	ENDIF   
  ELSE
    IF lnLineNo = 53
      IF XTAX AND XTAX_METH = 'M' AND INVHDR.TAX_AMT # 0
        IF !EMPTY(lcTaxRefr)
          WKAMT = FREIGHT + INSUR + COD
   		  IF WKAMT <> 0
		    RETURN 'TOTAL - FREIGHT' + SPACE(4)+STR(WKAMT,10,2)
		  ELSE
		    IF DISCOUNT<>0
        	  RETURN 'DISCOUNT' + SPACE(11)+STR(DISCOUNT,10,2)
		    ENDIF   
		  ENDIF   
		ELSE
          WKAMT = FREIGHT + INSUR + COD
   		  IF WKAMT <> 0
   		    IF DISCOUNT<>0
   		      RETURN 'DISCOUNT' + SPACE(11)+STR(DISCOUNT,10,2)
   		    ENDIF 
   		  ENDIF 
        ENDIF 
      ELSE
        IF XTAX AND XTAX_METH = 'A' AND INVHDR.TAX_AMT # 0  
          RETURN lcTaxRefr
        ENDIF
      ENDIF 
    ELSE
      IF lnLineNo = 54
        IF XTAX AND XTAX_METH = 'M' AND INVHDR.TAX_AMT # 0
          IF !EMPTY(lcTaxRefr)
            WKAMT = FREIGHT + INSUR + COD
     		  IF WKAMT <> 0
     		    IF DISCOUNT<>0
     		      RETURN 'DISCOUNT' + SPACE(11)+STR(DISCOUNT,10,2)
     		    ENDIF 
			  ENDIF 
          ENDIF 
        ELSE
          IF XTAX AND XTAX_METH = 'A' AND INVHDR.TAX_AMT # 0
            XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
            RETURN SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'+SPACE(7)+STR(INVHDR.TAX_AMT  ,10,2)
          ELSE
            IF xTAx AND llIsCanada 
              lcStrRate = ALLTRIM(STR(InvHdr.nPstRate,5,2))     
              RETURN 'PST TAX    :' + lcStrRate + ' %'+SPACE(1)+STR(InvHdr.nPstAmt,10,2)
            ENDIF   
          ENDIF 
        ENDIF 
      ELSE
        IF lnLineNo = 55
          IF !( XTAX AND XTAX_METH = 'A' AND INVHDR.TAX_AMT # 0) AND xTAx AND llIsCanada 
            IF InvHdr.nHSTAmt <> 0
              RETURN 'HST TAX    :' + STR(InvHdr.nHSTRate,5,2)+' %'+STR(InvHdr.nHSTAmt,10,2)
            ENDIF 
          ELSE
            IF (XTAX AND XTAX_METH = 'A' AND INVHDR.TAX_AMT # 0) AND xTAx AND llIsCanada 
              lcStrRate = ALLTRIM(STR(InvHdr.nPstRate,5,2))     
              RETURN 'PST TAX    :' + lcStrRate + ' %'+SPACE(1)+STR(InvHdr.nPstAmt,10,2)
            ENDIF 
          ENDIF 
        ELSE
          IF lnLineNo = 56
            IF (XTAX AND XTAX_METH = 'A' AND INVHDR.TAX_AMT # 0) AND xTAx AND llIsCanada AND  InvHdr.nHSTAmt <> 0
              RETURN 'HST TAX    :' + STR(InvHdr.nHSTRate,5,2)+' %'+STR(InvHdr.nHSTAmt,10,2)
            ENDIF 
          ENDIF 
        ENDIF 
      ENDIF 
    ENDIF 
  ENDIF 
ENDIF   
RETURN ''