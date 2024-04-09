*:***********************************************************************************************************************
*: Procedure file: ARPINVER.PRG
*: System		 : ARIA4XP (Cutsom Invoice Form For ERI02)
*: Module		 : AR (c200827) [T20070418.0009] 
*: Author		 : Mariam Mazher 
*: DATE			 : 08/14/2007
*:***********************************************************************************************************************
llTax      = IIF(gfGetMemVar("M_TAX",oAriaapplication.ActiveCompanyID)='Y', .T. , .F.)
lcTax_Desc = gfGetMemVar('M_TAX_DESC',oAriaapplication.ActiveCompanyID)
lcTax_Meth = gfGetMemVar('M_TAX_METH',oAriaapplication.ActiveCompanyID)
lcTaxRefr  = gfGetMemVar('M_TAX_REFE',oAriaapplication.ActiveCompanyID)
lcDUNS     = gfGetMemVar('XDUNS') 

RELEASE ALL LIKE M_*

DIMENSION laItemSeg[1]
STORE 0 TO lcClrLen,lnClrPos
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lcClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

lcDist = ' '

llGrpPrint =.F.

llNote = llRpInvNot
SELECT INVHDR

MSG1 = lcRpMsg1 && = 'lcRpMsg1'        && 1st. line Variable
MSG2 = lcRpMsg2
MSG3 = lcRpMsg3

WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT

lcRpPrSt =IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt)

SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

lcWorkfile  = loogscroll.gfTempName()

=lfCrtTempFile()

lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)

 
m.XPHONE    = TRANSFORM(lcCompPhon , '@R ' + lcPhonPict)


SCAN FOR &lcASExp 
  STORE '' TO m.Invoice ,m.Account ,m.Piktkt  ,m.APPROVAL,m.MNOTES   ,m.XFNAME ,;
  m.XFADDR3 ,m.XFADDR2,m.XFADDR1 ,m.XNOTE2,m.XNOTE1 ,m.PSHIPVIA, m.REP2, m.REP1, m.PTERMS,;
  m.ORDER,m.DEPT,m.XSTORE, m.CUSTPO,m.XBTNAME ,m.XBTADDR1, m.XBTADDR2, m.XBTADDR3,;
   m.XSTNAME,m.XSTADDR1, m.XSTADDR2 , m.XSTADDR3 ,m.cFACCODE,m.MTotals
  
  STORE 0 TO m.TOTALCHG,m.CARTONS ,m.WEIGHT , m.TAX_AMT ,;
              m.TAX_RATE, m.nFRINCOD, m.nHSTRate, m.nHSTAmt ,m.nPSTAmt ,m.nPSTRate,m.LINENUM
              
  STORE {} to m.Shipdate, m.invDate 
  
  m.Invoice = invoice
  m.Account = Account
  m.DISCOUNT= DISCOUNT
  m.Piktkt  = piktkt
  m.invDate = invDate
  m.Shipdate= Shipdate 
  m.TOTALCHG= TOTALCHG
  m.WEIGHT  = WEIGHT
  m.CARTONS = CARTONS
  m.APPROVAL= APPROVAL
  m.TAX_AMT = TAX_AMT
  m.TAX_RATE= TAX_RATE
  m.nFRINCOD = FREIGHT + INSUR + COD
  m.nHSTRate = nHSTRate
  m.nHSTAmt =nHSTAmt
  m.nPSTAmt =nPSTAmt
  m.nPSTRate=nPSTRate
  m.MTotals = "'TOTAL - M E R C H A N D I S E'+SPACE(10)+STR(sumpieces)+SPACE(9)+STR(sumamount,10,2)"+;
  			  "+CHR(13)+IIF(Discount <> 0,'TOTAL - D I S C O U N T'+SPACE(34)+STR(discount,11,2)+CHR(13),'')"+;
  			  "+IIF(lltaxable,IIF(!EMPTY(LCTAX_DESC),PADR(LCTAX_DESC,20),SPACE(20))+SPACE(1)+IIF(!EMPTY(LCTAXREFR),PADR(LCTAXREFR,30),SPACE(30))+SPACE(1)+STR(TAX_RATE,5,2)+'%'+SPACE(1)+STR(TAX_AMT,10,2)+CHR(13),'')"+;
  			  "+IIF(lltaxable and NPSTAMT <> 0,'P S T   T A X'+SPACE(38)+STR(NPSTRATE,5,2)+'%'+SPACE(1)+STR(NPSTAMT,10,2)+CHR(13),'')"+;
  			  "+IIF(lltaxable and lliscanada AND NHSTAMT <> 0,'H S T  T A X'+SPACE(39)+STR(NHSTRATE,5,2)+'%'+SPACE(1)+STR(NHSTAMT,10,2)+CHR(13),'')"+;
  			  "+IIF(nFRINCOD<>0,'TOTAL - F R E I G H T'+space(37)+STR(nFRINCOD,10,2)+CHR(13),'')+"+;
  			  "IIF(lltaxmetha,IIF(!EMPTY(LCTAX_DESC),PADR(LCTAX_DESC,20),SPACE(20))+SPACE(1)+IIF(!EMPTY(LCTAXREFR),PADR(LCTAXREFR,30),SPACE(30))+STR(TAX_RATE,5,2)+'%'+SPACE(1)+STR(TAX_AMT,10,2)+CHR(13),'')"+;
  			  "+IIF(lltaxmetha and NPSTAMT <> 0,'P S T   T A X'+SPACE(38)+STR(NPSTRATE,5,2)+'%'+SPACE(1)+STR(NPSTAMT,10,2)+CHR(13),'')"+;
  			  "+IIF(lltaxmetha AND lliscanada AND NHSTAMT <> 0,'H S T  T A X'+SPACE(39)+STR(NHSTRATE,5,2)+'%'+SPACE(1)+STR(NHSTAMT,10,2),'')"
  			  

  				
  

  IF llRpInvNot AND SEEK('C' + INVHDR.Invoice , 'NOTEPAD')
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75
    m.MNOTES   = NOTEPAD.MNOTES
  ENDIF
  
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    =SEEK(INVHDR.CFACCODE,'SYCFACT')
    m.XFNAME = SYCFACT.cfaccomp
    m.XFADDR3 = LEFT(TRIM(SYCFACT.CADDRESS3)+' '+SYCFACT.CADDRESS4+' '+SYCFACT.CADDRESS5,30)
    m.XFADDR2 = SYCFACT.CADDRESS2
    m.XFADDR1 = SYCFACT.CADDRESS1
    m.cFACCODE = INVHDR.CFACCODE
  ENDIF
  
  m.XNOTE2 = IIF(Invhdr.note2 <> '*',Invhdr.note2,'')
  m.XNOTE1 = IIF(Invhdr.note1<> '*',Invhdr.note1,'')
  m.PSHIPVIA = LEFT(gfCodDes(INVHDR->SHIPVIA,'SHIPVIA'),14)
  m.REP2     = Invhdr.rep2
  m.REP1     = Invhdr.rep1 
  m.PTERMS  = LEFT(gfCodDes(INVHDR->CTERMCODE,'CTERMCODE'),15)
  m.ORDER   = Invhdr.ORDER
  m.DEPT    = Invhdr.DEPT
  m.XSTORE  = Invhdr.Store
  m.CUSTPO  = LEFT(Invhdr.CUSTPO,12)
  
  lfSolSpAdr()

  m.XBTNAME = lcSolTName
  m.XBTADDR1 = laSoldTo[1]
  m.XBTADDR2 = laSoldTo[2]
  m.XBTADDR3 = TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
  
  IF LEN(TRIM(laSoldTo[2])) =0
    m.XBTADDR2 = laSoldTo[3]
    m.XBTADDR3 = ''
  ENDIF

  m.XSTNAME = lcShpTName
  m.XSTADDR1 = laShipTo[1]
  m.XSTADDR2 = laShipTo[2]
  m.XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
  IF LEN(TRIM(laShipTo[2])) =0
    m.XSTADDR2 = laShipTo[3]
    m.XSTADDR3 = ''
  ENDIF
  
  SELECT Invline 
  =SEEK(Invhdr.Invoice,'INVLINE')
  SCAN REST WHILE INVOICE+STR(LINENO,6) = Invhdr.Invoice FOR totqty <> 0
  
    STORE '' TO m.Style,m.STYLEDESC,m.SKU 
    STORE 0 TO m.PRICE ,m.TOTQTY
    
    m.LINENUM = m.LINENUM +1
    
    m.Style = Style
    =SEEK(style,'Style','Style')
    m.STYLEDESC = Style.DEsc
    m.PRICE = invline.PRICE
    m.TOTQTY = invline.TOTQTY

    IF llRpPrtSku AND SEEK('S'+INVLINE.ACCOUNT+INVLINE.STYLE,"SPCK_HDR",'SKU_STYLE') OR  ;
           SEEK('P'+INVLINE.ACCOUNT+INVLINE.STYLE,"SPCK_HDR",'SKU_STYLE')
      m.SKU = SPCK_HDR.Pack_id
    ENDIF  
    SELECT  (lcWorkfile)
    APPEND BLANK
    GATHER MEMO MEMVAR
  ENDSCAN 
ENDSCAN

SELECT  (lcWorkfile)
lcTempUpdate = loogscroll.gfTempName()
SELECT distinct Invoice from (lcWorkfile) INTO CURSOR (lcTempUpdate)
SELECT  (lcWorkfile)
gfDispRe()
IF UPPER(OariaApplication.gcDevice ) ='PRINTER'
  PRIVATE lnOldAls,lcInvNo
  lnOldAls = SELECT(0)
  SELECT (lcTempUpdate)
  SCAN
    lcInvNo = Invoice
    SELECT InvHdr
    LOCATE FOR invoice=lcInvNo
    IF FOUND()
      REPLACE PrtFlag WITH "P"
    ENDIF
  ENDSCAN
  USE IN (lcTempUpdate)
  SELECT(lnOldAls)
ENDIF



*!*************************************************************
*! Name      : lfCrtTempFile
*: Author	 : Mariam Mazher 
*: DATE		 : 08/14/2007
*! Purpose   : create temp. files
*!*************************************************************
FUNCTION lfCrtTempFile
DIMENSION laFileStruct[49,4]


laFileStruct[1,1] =  'Invoice'
laFileStruct[1,2] =  'C'
laFileStruct[1,3] =  6
laFileStruct[1,4] = 0

laFileStruct[2,1] =  'INVDATE'
laFileStruct[2,2] =  'D'
laFileStruct[2,3] =  8
laFileStruct[2,4] = 0

laFileStruct[3,1] =  'XBTNAME'
laFileStruct[3,2] =  'C'
laFileStruct[3,3] =  30
laFileStruct[3,4] = 0

laFileStruct[4,1] =  'XSTNAME'
laFileStruct[4,2] =  'C'
laFileStruct[4,3] =  30
laFileStruct[4,4] = 0


laFileStruct[5,1] =  'XBTADDR1'
laFileStruct[5,2] =  'C'
laFileStruct[5,3] =  30
laFileStruct[5,4] = 0

laFileStruct[6,1] =  'XSTADDR1'
laFileStruct[6,2] =  'C'
laFileStruct[6,3] =  30
laFileStruct[6,4] = 0

laFileStruct[7,1] =  'XBTADDR2'
laFileStruct[7,2] =  'C'
laFileStruct[7,3] =  30
laFileStruct[7,4] = 0

laFileStruct[8,1] =  'XSTADDR2'
laFileStruct[8,2] =  'C'
laFileStruct[8,3] =  30
laFileStruct[8,4] = 0

laFileStruct[9,1] =  'XBTADDR3'
laFileStruct[9,2] =  'C'
laFileStruct[9,3] =  30
laFileStruct[9,4] = 0

laFileStruct[10,1] =  'XSTADDR3'
laFileStruct[10,2] =  'C'
laFileStruct[10,3] =  30
laFileStruct[10,4] = 0


laFileStruct[11,1] =  'ACCOUNT'
laFileStruct[11,2] =  'C'
laFileStruct[11,3] =  5
laFileStruct[11,4] = 0

laFileStruct[12,1] =  'CUSTPO'
laFileStruct[12,2] =  'C'
laFileStruct[12,3] =  12
laFileStruct[12,4] = 0

laFileStruct[13,1] =  'XSTORE'
laFileStruct[13,2] =  'C'
laFileStruct[13,3] =  8
laFileStruct[13,4] = 0


laFileStruct[14,1] =  'DEPT'
laFileStruct[14,2] =  'C'
laFileStruct[14,3] =  5
laFileStruct[14,4] = 0

laFileStruct[15,1] =  'ORDER'
laFileStruct[15,2] =  'C'
laFileStruct[15,3] =  6
laFileStruct[15,4] = 0

laFileStruct[16,1] =  'PTERMS'
laFileStruct[16,2] =  'C'
laFileStruct[16,3] =  15
laFileStruct[16,4] = 0

laFileStruct[17,1] =  'REP1'
laFileStruct[17,2] =  'C'
laFileStruct[17,3] =  3
laFileStruct[17,4] = 0

laFileStruct[18,1] =  'REP2'
laFileStruct[18,2] =  'C'
laFileStruct[18,3] =  3
laFileStruct[18,4] = 0

laFileStruct[19,1] =  'PSHIPVIA'
laFileStruct[19,2] =  'C'
laFileStruct[19,3] =  14
laFileStruct[19,4] = 0

laFileStruct[20,1] =  'XNOTE1'
laFileStruct[20,2] =  'C'
laFileStruct[20,3] =  30
laFileStruct[20,4] = 0

laFileStruct[21,1] =  'XNOTE2'
laFileStruct[21,2] =  'C'
laFileStruct[21,3] =  30
laFileStruct[21,4] = 0

laFileStruct[22,1] =  'PIKTKT'
laFileStruct[22,2] =  'C'
laFileStruct[22,3] =  6
laFileStruct[22,4] = 0

laFileStruct[23,1] =  'SHIPDATE'
laFileStruct[23,2] =  'D'
laFileStruct[23,3] =  8
laFileStruct[23,4] = 0

laFileStruct[24,1] =  'XFADDR1'
laFileStruct[24,2] =  'C'
laFileStruct[24,3] =  30
laFileStruct[24,4] = 0

laFileStruct[25,1] =  'XFADDR2'
laFileStruct[25,2] =  'C'
laFileStruct[25,3] =  30
laFileStruct[25,4] = 0

laFileStruct[26,1] =  'XFADDR3'
laFileStruct[26,2] =  'C'
laFileStruct[26,3] =  100
laFileStruct[26,4] = 0

laFileStruct[27,1] =  'XFNAME'
laFileStruct[27,2] =  'C'
laFileStruct[27,3] =  30
laFileStruct[27,4] = 0

laFileStruct[28,1] =  'STYLE'
laFileStruct[28,2] =  'C'
laFileStruct[28,3] =  19
laFileStruct[28,4] = 0


laFileStruct[29,1] =  'STYLEDESC'
laFileStruct[29,2] =  'C'
laFileStruct[29,3] =  20
laFileStruct[29,4] = 0

laFileStruct[30,1] =  'TOTQTY'
laFileStruct[30,2] =  'N'
laFileStruct[30,3] =  7
laFileStruct[30,4] = 0


laFileStruct[32,1] =  'PRICE'
laFileStruct[32,2] =  'N'
laFileStruct[32,3] =  12
laFileStruct[32,4] = 2

laFileStruct[33,1] =  'SKU'
laFileStruct[33,2] =  'C'
laFileStruct[33,3] =  16
laFileStruct[33,4] = 0

laFileStruct[34,1] =  'MNOTES'
laFileStruct[34,2] =  'M'
laFileStruct[34,3] =  10
laFileStruct[34,4] = 0

laFileStruct[35,1] =  'DISCOUNT'
laFileStruct[35,2] =  'N'
laFileStruct[35,3] =  13
laFileStruct[35,4] = 2


laFileStruct[36,1] =  'nPSTRate'
laFileStruct[36,2] =  'N'
laFileStruct[36,3] =  6
laFileStruct[36,4] = 2

laFileStruct[37,1] =  'nPSTAmt'
laFileStruct[37,2] =  'N'
laFileStruct[37,3] =  13
laFileStruct[37,4] = 2

laFileStruct[38,1] =  'nHSTAmt'
laFileStruct[38,2] =  'N'
laFileStruct[38,3] =  13
laFileStruct[38,4] = 2

laFileStruct[39,1] =  'nHSTRate'
laFileStruct[39,2] =  'N'
laFileStruct[39,3] =  6
laFileStruct[39,4] = 2

laFileStruct[40,1] =  'nFRINCOD'
laFileStruct[40,2] =  'N'
laFileStruct[40,3] =  13
laFileStruct[40,4] = 2


laFileStruct[41,1] =  'TAX_RATE'
laFileStruct[41,2] =  'N'
laFileStruct[41,3] =  6
laFileStruct[41,4] = 3

laFileStruct[42,1] =  'TAX_AMT'
laFileStruct[42,2] =  'N'
laFileStruct[42,3] =  13
laFileStruct[42,4] = 2

laFileStruct[43,1] =  'APPROVAL'
laFileStruct[43,2] =  'C'
laFileStruct[43,3] =  10
laFileStruct[43,4] = 0

laFileStruct[44,1] =  'CARTONS'
laFileStruct[44,2] =  'N'
laFileStruct[44,3] =  5
laFileStruct[44,4] = 0

laFileStruct[45,1] =  'WEIGHT'
laFileStruct[45,2] =  'N'
laFileStruct[45,3] =  8
laFileStruct[45,4] = 2

laFileStruct[31,1] =  'TOTALCHG'
laFileStruct[31,2] =  'N'
laFileStruct[31,3] =  14
laFileStruct[31,4] = 2

laFileStruct[46,1] =  'cFACCODE'
laFileStruct[46,2] =  'C'
laFileStruct[46,3] =  6
laFileStruct[46,4] = 0


laFileStruct[48,1] =  'XPHONE'
laFileStruct[48,2] =  'C'
laFileStruct[48,3] =  20
laFileStruct[48,4] = 0

laFileStruct[47,1] =  'LINENUM'
laFileStruct[47,2] =  'N'
laFileStruct[47,3] =  10
laFileStruct[47,4] = 0

laFileStruct[49,1] =  'MTotals'
laFileStruct[49,2] =  'M'
laFileStruct[49,3] =  10
laFileStruct[49,4] = 0



=gfCrtTmp(lcWorkfile ,@laFileStruct,,"INVOICE",lcWorkfile)

*!*************************************************************
*! Name      : lfEndingGrp
*: Author	 : Mariam Mazher 
*: DATE		 : 08/14/2007
*! Purpose   : CHECK the end of the grp
*!*************************************************************
FUNCTION lfEndingGrp

lnRecNo = RECNO()
lnCount = 0
lcInvoice = invoice
SCAN FOR invoice  = lcInvoice 
  lnCount = lnCount + 1 
ENDSCAN
GO RECORD lnRecNo
RETURN lnCount
*!*************************************************************
*! Name      : lfUpdGrp
*: Author	 : Mariam Mazher 
*: DATE		 : 08/14/2007
*! Purpose   : update  the group  flag
*!*************************************************************
FUNCTION lfUpdGrp
llGrpPrint =.T.
RETURN .T.

*!*************************************************************
*! Name      : lfSetGrp 
*: Author	 : Mariam Mazher 
*: DATE		 : 08/14/2007
*! Purpose   : Reset the group flag
*!*************************************************************
FUNCTION lfSetGrp 
llGrpPrint =.F.
RETURN .T.