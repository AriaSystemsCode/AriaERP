*:***************************************************************************
*: Program file  : ARPINVK
*: Program desc. : Custom Invoice Form Report
*: For Report    : ....
*: System        : Aria 4XP
*: Module        : Account Receivable (AR)
*: Developer     : ALAA ABDELWAHED  (ALA)
*: Date          : 06/26/2008
*:Entry No.      : 000613 
*:***************************************************************************
*:Modifications
*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[T20080505.0011] 
*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[T20090413.0015]
*:***************************************************************************

lcRpPrSt = IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt)



*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[Start] 
lcTmpInvhdr  = gfTempName()
SELECT INVHDR
DIMENSION laInvhdStr[1,18]
AFIELDS(laInvhdStr)
=gfCrtTmp(lcTmpInvhdr  ,@laInvhdStr,"INVOICE",lcTmpInvhdr  ,.F.)
*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[End] 


*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[Start]
lcTmpInvLine  = gfTempName()
SELECT INVLINE
DIMENSION laInvLStr[1,18]
AFIELDS(laInvLStr)
=gfCrtTmp(lcTmpInvLine    ,@laInvLStr,"INVOICE+STR(LINENO,6)+STYLE",lcTmpInvLine  ,.F.)
*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[End]

*lcRpPrSt = IIF(oAriaApplication.ProcessID = 'ARPINV',"",IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt))

STORE TRIM(laCompAdd[1])                  TO HLINE2
STORE TRIM(laCompAdd[2])                  TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4] TO HLINE4
STORE TRANSFORM(lcCompPhon , '@R ' + lcPhonPict) TO HLINE5

IF EMPTY(HLINE3)
   STORE HLINE4 TO HLINE3
   STORE HLINE5 TO HLINE4
   STORE ''     TO HLINE5
ENDIF


XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
lcTempScale = loogscroll.gfTempName()
DIMENSION laScalArr[56,4]

laScalArr[1,1] = 'S1'
laScalArr[1,2] = 'C'
laScalArr[1,3] = 3
laScalArr[1,4] = 0

laScalArr[2,1] = 'S2'
laScalArr[2,2] = 'C'
laScalArr[2,3] = 3
laScalArr[2,4] = 0

laScalArr[3,1] = 'S3'
laScalArr[3,2] = 'C'
laScalArr[3,3] = 3
laScalArr[3,4] = 0

laScalArr[4,1] = 'S4'
laScalArr[4,2] = 'C'
laScalArr[4,3] = 3
laScalArr[4,4] = 0

laScalArr[5,1] = 'S5'
laScalArr[5,2] = 'C'
laScalArr[5,3] = 3
laScalArr[5,4] = 0

laScalArr[6,1] = 'S6'
laScalArr[6,2] = 'C'
laScalArr[6,3] = 3
laScalArr[6,4] = 0

laScalArr[7,1] = 'INVOICE'
laScalArr[7,2] = 'C'
laScalArr[7,3] = 6
laScalArr[7,4] = 0

laScalArr[8,1] = 'MTotals'
laScalArr[8,2] = 'M'
laScalArr[8,3] = 10
laScalArr[8,4] = 0

lnCount = 9
FOR lnI = 1 TO 6
 FOR lnJ = 1 To 8
  laScalArr[lnCount ,1] = 'SZ'+STR(lnI,1)+STR(lnJ,1)
  laScalArr[lnCount ,2] = 'C'
  laScalArr[lnCount ,3] = 5
  laScalArr[lnCount ,4] = 0
  lnCount = lnCount + 1
 ENDFOR  
ENDFOR 



=gfCrtTmp(lcTempScale ,@laScalArr,"INVOICE",lcTempScale,.F.)
llIsCanada = IIF(UPPER(ALLTRIM(oAriaApplication.DefaultCountry ))='CANADA',.T.,.F.)
llTAx  = (xTax .AND. xTax_Meth='M')
lltaxmetha = (xTax .AND. xTax_Meth='A')



*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[Start] 
*!*	SELECT InvHdr
*!*	SCAN FOR &lcRpExp
*!*	  SELECT(lcTempScale)
*!*	  APPEND BLANK 
*!*	  REPLACE INVOICE WITH InvHdr.INVOICE,;
*!*	          MTotals with  "'TOTAL - M E R C H A N D I S E'"+;
*!*	          "+SPACE(13)+STR(lnTOTQty,6,0)+SPACE(10)+STR(lnAmount,10,2)+CHR(13)"+;
*!*	          "+IIF(InvHdr.Discount <> 0,'TOTAL - D I S C O U N T'+SPACE(34)+STR(InvHdr.discount,11,2)+CHR(13),'')"+;
*!*	          "+IIF(llTAx  ,IIF(!EMPTY(xTax_Desc),PADR(xTax_Desc ,20),SPACE(20))+SPACE(1)+IIF(!EMPTY(LCTAXREFDS),PADR(LCTAXREFDS,31),SPACE(31))+STR(InvHdr.TAX_RATE,5,2)+'%'+STR(InvHdr.TAX_AMT,10,2)+CHR(13),'')"+;
*!*	          "+IIF(llTAx and InvHdr.nPSTAmt <> 0 AND lliscanada,'P S T  T A X'+SPACE(40)+STR(InvHdr.NPSTRATE,5,2)+'%'+STR(InvHdr.NPSTAMT,10,2)+CHR(13),'')"+;
*!*	          "+IIF(llTAx and lliscanada AND InvHdr.NHSTAMT <> 0 ,'H S T  T A X'+SPACE(40)+STR(InvHdr.NHSTRATE,5,2)+'%'+STR(InvHdr.NHSTAMT,10,2)+CHR(13),'')"+;
*!*	          "+IIF(InvHdr.Freight + InvHdr.Insur + InvHdr.Cod<>0,'TOTAL - F R E I G H T'+space(37)+STR(InvHdr.Freight + InvHdr.Insur + InvHdr.Cod,10,2)+CHR(13),'')"+;
*!*	          "+IIF(lltaxmetha ,IIF(!EMPTY(xTax_Desc),PADR(xTax_Desc ,20),SPACE(20))+SPACE(1)+IIF(!EMPTY(LCTAXREFDS),PADR(LCTAXREFR,31),SPACE(31))+STR(InvHdr.TAX_RATE,5,2)+'%'+STR(InvHdr.TAX_AMT,10,2)+CHR(13),'')"+;
*!*	          "+IIF(lltaxmetha and InvHdr.nPSTAmt <> 0 AND lliscanada ,'P S T  T A X'+SPACE(40)+STR(InvHdr.NPSTRATE,5,2)+'%'+STR(InvHdr.NPSTAMT,10,2)+CHR(13),'')"+;
*!*	          "+IIF(lltaxmetha AND InvHdr.NHSTAMT <> 0 AND lliscanada ,'H S T  T A X'+SPACE(40)+STR(InvHdr.NHSTRATE,5,2)+'%'+STR(InvHdr.NHSTAMT,10,2),'')"


*!*	  =SEEK(InvHdr.Invoice,'Invline')
*!*	  SELECT INVLINE
*!*	  lnSclCount = 1
*!*	  SCAN REST WHILE INVOICE+STR(LINENO,6) = InvHdr.Invoice
*!*	    IF lnSclCount > 6 
*!*	      LOOP 
*!*	    ENDIF 
*!*	    IF SEEK(INVHDR.INVOICE,lcTempScale)  
*!*	      llFound = .F.
*!*	      FOR lnI = 1 TO 6
*!*	        lcI = STR(lnI,1)
*!*	        IF &lcTempScale..S&lcI = INVLINE.Scale
*!*	          llFound = .T.
*!*	          EXIT 
*!*	        ENDIF 
*!*	      ENDFOR 
*!*	      IF !llFound 
*!*	        IF SEEK('S'+INVLINE.Scale,'Scale')
*!*	          IF !SEEK(INVHDR.INVOICE,lcTempScale)  
*!*	            SELECT(lcTempScale)
*!*	            APPEND BLANK 
*!*	            lcSclCount = STR(1,1)
*!*	            REPLACE S1  WITH INVLINE.Scale,;
*!*	  		          SZ11  WITH Scale.SZ1,;
*!*	  		          SZ12  WITH Scale.SZ2,;
*!*	  		          SZ13  WITH Scale.SZ3,;
*!*	  		          SZ14  WITH Scale.SZ4,;
*!*	  		          SZ15  WITH Scale.SZ5,;
*!*	  		          SZ16  WITH Scale.SZ6,;
*!*	  		          SZ17  WITH Scale.SZ7,;
*!*	  		          SZ18  WITH Scale.SZ8,;
*!*	  		          invoice WITH INVhdr.invoice
*!*	          ELSE
*!*	            SELECT(lcTempScale)
*!*	            lcSclCount = STR(lnSclCount ,1)
*!*	            REPLACE S&lcSclCount  WITH INVLINE.Scale,;
*!*	  		          Sz&lcSclCount.1  WITH Scale.SZ1,;
*!*	  		          SZ&lcSclCount.2  WITH Scale.SZ2,;
*!*	  		          SZ&lcSclCount.3  WITH Scale.SZ3,;
*!*	  		          SZ&lcSclCount.4  WITH Scale.SZ4,;
*!*	  		          SZ&lcSclCount.5  WITH Scale.SZ5,;
*!*	  		          SZ&lcSclCount.6  WITH Scale.SZ6,;
*!*	  		          SZ&lcSclCount.7  WITH Scale.SZ7,;
*!*	  		          SZ&lcSclCount.8  WITH Scale.SZ8
*!*	          ENDIF 
*!*	          lnSclCount = lnSclCount + 1
*!*	        ENDIF 
*!*	      ELSE
*!*	        LOOP   
*!*	      ENDIF 
*!*	    ELSE
*!*	      =SEEK('S'+INVLINE.Scale,'Scale')
*!*	      SELECT(lcTempScale)
*!*	      APPEND BLANK 
*!*	      lcSclCount = STR(1,1)
*!*	      REPLACE S1  WITH INVLINE.Scale,;
*!*	          SZ11  WITH Scale.SZ1,;
*!*	          SZ12  WITH Scale.SZ2,;
*!*	          SZ13  WITH Scale.SZ3,;
*!*	          SZ14  WITH Scale.SZ4,;
*!*	          SZ15  WITH Scale.SZ5,;
*!*	          SZ16  WITH Scale.SZ6,;
*!*	          SZ17  WITH Scale.SZ7,;
*!*	          SZ18  WITH Scale.SZ8,;
*!*	          invoice WITH INVhdr.invoice
*!*	      lnSclCount = lnSclCount + 1          
*!*	    ENDIF   
*!*	  ENDSCAN 
*!*	ENDSCAN 
*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[End] 

SELECT InvHdr
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
STORE 0 TO lnLenth , lnClrLen , lnClrPos
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*!*	llPrintCont=.t.
*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[Start] 
*SELECT InvHdr
SELECT InvHdr
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
SELECT INVHDR
LOCATE 
SCAN FOR &lcASExp
  SCATTER MEMO MEMVAR 
  SELECT (lcTmpInvhdr)  
  APPEND BLANK 
  GATHER MEMO MEMVAR 
  lfGetScales(m.invoice)
  *B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[Start]
  SELECT Invline
  =SEEK(Invhdr.Invoice)
  lnCounter = 0
  SCAN REST WHILE INVOICE+STR(LINENO,6)   = Invhdr.Invoice
    SCATTER MEMO MEMVAR 
    m.llOK_STat = .T.
    lnCounter = lnCounter + 1
    IF !SEEK(m.INVOICE+STR(m.LINENO,6)+m.STYLE,lcTmpInvLine)
      INSERT INTO (lcTmpInvLine) FROM MEMVAR 
    ENDIF   
    IF MOD(lnCounter,11) = 0
      lnRecNm = RECNO('Invline')
      SELECT invline 
      SKIP 
      IF EOF() OR Invoice <> invhdr.Invoice
        IF BETWEEN(lnRecNm ,1,RECCOUNT())
          GO RECORD lnRecNm  IN Invline 
        ENDIF 
        SELECT(lcTmpInvLine)
        REPLACE llOK_STat WITH  .F.
        APPEND BLANK 
        REPLACE Invoice WITH Invhdr.Invoice,;
      		  LINENO WITH 	m.Lineno,;
      		  Style WITH 'XXXXXXXXXXXXXXXXXXX',;
      		  llOK_STat WITH  .T.
      		  
        lnCounter = 0
     ELSE
       IF BETWEEN(lnRecNm ,1,RECCOUNT())
         GO RECORD lnRecNm  IN Invline 
       ENDIF   
     ENDIF   
    ENDIF 
  ENDSCAN 
  *B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[End]
ENDSCAN 

*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[Start]
SELECT (lcTmpInvLine) 
SET RELATION TO INVOICE+STR(LINENO,6) INTO Invline ADDITIVE 
*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[End]

SELECT (lcTmpInvhdr)  

*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[Start]
*SET RELATION TO invoice INTO INVLINE  ADDITIVE 
SET RELATION TO invoice INTO (lcTmpInvLine)  ADDITIVE 
*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[End]

*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[End] 

SET RELATION TO invoice INTO (lcTempScale) ADDITIVE 

*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[Start]
*SET SKIP TO INVLINE 
SET SKIP TO (lcTmpInvLine)
*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[End]

*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[Start] 
*SELECT InvHdr
SELECT (lcTmpInvhdr)
*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[End] 

LOCATE 


*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[Start] 
*DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
DO gfDispRe WITH EVAL('lcFormName')
*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[End] 

lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
RETURN



*!*************************************************************
*! Name       : lfCheck4Notes
*!LPARAMETERS : lcInvP
*: Author     : Alaa AbdelWahed 
*: DATE       : 06/24/2008
*! Purpose    : CHECK the end of the notes
*!*************************************************************
FUNCTION lfCheck4Notes
LPARAMETERS lcInvP

PRIVATE llFound
llFound=SEEK("C"+lcInvP,'Notepad','Notepad')

RETURN(llFound)

*FUNCTION lftemp
* =lfSolSpAdr()

*!*************************************************************
*! Name      : lfchecklast
*: Author   : Mariam Mazher 
*: DATE     : 12/24/2007
*! Purpose   : CHECK the end of the grp
*!*************************************************************
FUNCTION lfchecklast
lcAlias = ALIAS()

*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[Start] 
*lcInvoice = INVHDR.invoice
lcInvoice = EVALUATE(lcTmpInvhdr+'.invoice')
*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[End] 

*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[Start]
*SELECT invline 
SELECT (lcTmpInvLine)
*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[End]

lnRecNo = RECNO()
lnLineNo = 0

*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[Start] 
*SCAN FOR invoice  = lcInvoice 

*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[Start]
*=SEEK(lcInvoice,'invline','invline')
*SCAN REST WHILE INVOICE+STR(LINENO,6) = lcInvoice 
=SEEK(lcInvoice,lcTmpInvLine)
SCAN REST WHILE INVOICE+STR(LINENO,6)+STYLE = lcInvoice 
*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[End]

*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[End] 

  lnLineNo = lineno 
ENDSCAN

*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[Start] 
IF BETWEEN(lnRecNo,1,RECCOUNT())
*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[End] 

  GO RECORD lnRecNo
  
*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[Start] 
ENDIF 
*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[End] 
SELECT (lcAlias)
RETURN lnLineNo
 

*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[Start] 
*!*************************************************************
*! Name      : lfGetScales
*: Author    : Mariam Mazhar 
*: DATE      : 07/13/2008
*! Purpose   : Get Scales
*!*************************************************************
FUNCTION  lfGetScales
PARAMETERS lcInvoice

IF !SEEK(lcInvoice,lcTempScale)
  SELECT(lcTempScale)
  APPEND BLANK 
  REPLACE INVOICE WITH lcInvoice,;
          MTotals with  "'TOTAL - M E R C H A N D I S E'"+;
          "+SPACE(13)+STR(lnTOTQty,6,0)+SPACE(10)+STR(lnAmount,10,2)+CHR(13)"+;
          "+IIF(EVALUATE(lcTmpInvhdr+'.Discount') <> 0,'TOTAL - D I S C O U N T'+SPACE(34)+STR(EVALUATE(lcTmpInvhdr+'.discount'),11,2)+CHR(13),'')"+;
          "+IIF(llTAx  ,IIF(!EMPTY(xTax_Desc),PADR(xTax_Desc ,20),SPACE(20))+SPACE(1)+IIF(!EMPTY(LCTAXREFDS),PADR(LCTAXREFDS,31),SPACE(31))+STR(EVALUATE(lcTmpInvhdr+'.TAX_RATE'),5,2)+'%'+STR(EVALUATE(lcTmpInvhdr+'.TAX_AMT'),10,2)+CHR(13),'')"+;
          "+IIF(llTAx and EVALUATE(lcTmpInvhdr+'.nPSTAmt') <> 0 AND lliscanada,'P S T  T A X'+SPACE(40)+STR(EVALUATE(lcTmpInvhdr+'.NPSTRATE'),5,2)+'%'+STR(EVALUATE(lcTmpInvhdr+'.NPSTAMT'),10,2)+CHR(13),'')"+;
          "+IIF(llTAx and lliscanada AND EVALUATE(lcTmpInvhdr+'.NHSTAMT') <> 0 ,'H S T  T A X'+SPACE(40)+STR(EVALUATE(lcTmpInvhdr+'.NHSTRATE'),5,2)+'%'+STR(EVALUATE(lcTmpInvhdr+'.NHSTAMT'),10,2)+CHR(13),'')"+;
          "+IIF(EVALUATE(lcTmpInvhdr+'.Freight') + EVALUATE(lcTmpInvhdr+'.Insur') + EVALUATE(lcTmpInvhdr+'.Cod')<>0,'TOTAL - F R E I G H T'+space(37)+STR(EVALUATE(lcTmpInvhdr+'.Freight') + EVALUATE(lcTmpInvhdr+'.Insur') +"+;
          "EVALUATE(lcTmpInvhdr+'.Cod'),10,2)+CHR(13),'')"+;
          "+IIF(lltaxmetha ,IIF(!EMPTY(xTax_Desc),PADR(xTax_Desc ,20),SPACE(20))+SPACE(1)+IIF(!EMPTY(LCTAXREFDS),PADR(LCTAXREFR,31),SPACE(31))+STR(EVALUATE(lcTmpInvhdr+'.TAX_RATE'),5,2)+'%'+STR(EVALUATE(lcTmpInvhdr+'.TAX_AMT'),10,2)+CHR(13),'')"+;
          "+IIF(lltaxmetha and EVALUATE(lcTmpInvhdr+'.nPSTAmt') <> 0 AND lliscanada ,'P S T  T A X'+SPACE(40)+STR(EVALUATE(lcTmpInvhdr+'.NPSTRATE'),5,2)+'%'+STR(EVALUATE(lcTmpInvhdr+'.NPSTAMT'),10,2)+CHR(13),'')"+;
          "+IIF(lltaxmetha AND EVALUATE(lcTmpInvhdr+'.NHSTAMT') <> 0 AND lliscanada ,'H S T  T A X'+SPACE(40)+STR(EVALUATE(lcTmpInvhdr+'.NHSTRATE'),5,2)+'%'+STR(EVALUATE(lcTmpInvhdr+'.NHSTAMT'),10,2),'')"


  =SEEK(lcInvoice,'Invline')
  SELECT INVLINE
  lnSclCount = 1
  SCAN REST WHILE INVOICE+STR(LINENO,6) = lcInvoice
    IF lnSclCount > 6 
      LOOP 
    ENDIF 
    IF SEEK(lcInvoice,lcTempScale)  
      llFound = .F.
      FOR lnI = 1 TO 6
        lcI = STR(lnI,1)
        IF &lcTempScale..S&lcI = INVLINE.Scale
          llFound = .T.
          EXIT 
        ENDIF 
      ENDFOR 
      IF !llFound 
        IF SEEK('S'+INVLINE.Scale,'Scale')
          IF !SEEK(lcInvoice,lcTempScale)  
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
                invoice WITH lcInvoice
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
          invoice WITH lcInvoice
      lnSclCount = lnSclCount + 1          
    ENDIF   
  ENDSCAN 
   =SEEK(lcInvoice,'Invline')
ENDIF 
RETURN .T.

*!*************************************************************
*! Name      : lfSolSpAddr
*: Author    : Mariam Mazhar 
*: DATE      : 07/13/2008
*! Purpose   : Get Address
*!*************************************************************
FUNCTION lfSolSpAddr

PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec
lnInvHdRec = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))
lnInvLnRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
lnPakLnRec = IIF(EOF('SPCK_LIN') , 0 , RECNO('SPCK_LIN'))

IF USED(lcTmpDbt)
  lnTmpDbt = IIF(EOF(lcTmpDbt) , 0 , RECNO(lcTmpDbt))
  lnARINSTMD = IIF(EOF('ARINSTMD') , 0 , RECNO('ARINSTMD'))
ELSE
  lnTmpDbt   = 0
  lnARINSTMD = 0
ENDIF
lnLineRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
lnHrRc    = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))

*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[Start]
SELECT Invline
=SEEK(INVHDR.INVOICE)
*B608931,1 MMT 07/12/2009 Fix bugs of extra empty Page in Invoice Form K[End]

COUNT TO lnLines WHILE INVLINE.INVOICE = INVHDR.INVOICE
IF lnInvLnRec > 0
  GO (lnLineRec) IN INVLINE
ENDIF
IF lnHrRc > 0
  GO (lnHrRc) IN INVHDR
ENDIF

=SEEK(&lcTmpInvhdr..invoice,'INVHDR','INVHDR')
DECLARE laFactor[5,1]
STORE '' TO laFactor,lcFacName   


*-- Fill laFactor with factor address
IF !EMPTY(&lcTmpInvhdr..CFACCODE)
  =SEEK(&lcTmpInvhdr..CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

llEndGroup = .F.
=gfRltFld(&lcTmpInvhdr..cDivision , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(&lcTmpInvhdr..ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(&lcTmpInvhdr..cTermCode , 'CTERMCODE')

lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

SELECT (lcTmpInvhdr)
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GOTO RECNO()
ENDIF
SELECT CUSTOMER

IF ORDHDR.Alt_ShpTo
  lcShpTName  = ORDHDR.STName
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
ELSE    && Else

  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr) AND !ORDHDR.lStrDirct
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF

  lcShpTName  = IIF(&lcTmpInvhdr..STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

  laShipTo[1] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF lnCUSRec <> 0
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
ENDIF    && End of IF

=lfAdrShift('laShipTo')

  SELECT INVHDR

IF lnTmpDbt <> 0
  GO lnTmpDbt IN (lcTmpDbt)
ENDIF
IF lnARINSTMD <> 0
  GO lnARINSTMD IN ARINSTMD
ENDIF

*-- Restore the old record pointer in INVLINE
IF lnInvLnRec = 0
  GO BOTTOM IN INVLINE
  IF !EOF('INVLINE')
    SKIP IN INVLINE
  ENDIF
ELSE
  GO lnInvLnRec IN INVLINE
ENDIF

*-- Restore the old record pointer in SPCK_LIN
IF lnPakLnRec = 0
  GO BOTTOM IN SPCK_LIN
  IF !EOF('SPCK_LIN')
    SKIP IN SPCK_LIN
  ENDIF
ELSE
  GO lnPakLnRec IN SPCK_LIN
ENDIF
RETURN ''
*B608611,1 MMT 07/13/2008 Enhance performance of invoice Form K[End] 