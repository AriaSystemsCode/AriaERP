****************************************************************************
*: Program file  : ALPKLSFR.PRG --- ALPKLSFR.FRX
*: Program desc. : Print Packing List for French room
*: System        : Aria Apparel System (A27).
*: Module        : Sales Order Allocation  (AL)
*: Developer     : Ahmed Abdel Naby- (AAN) Due to C#101975
*: Date          : 10/10/2000
*:**************************************************************************
*: Calls : 
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*:C#101975
*:**************************************************************************
*-- Variables Declaration

DECLARE  laBtAddr[3,1] , laStAddr[3,1] , laCodDesc[5,3] , laCompany[5,1] , lawarhusad[4]
STORE "" TO laBtAddr , laStAddr , lacompany

= (TYPE("lnFreeLen") = "C") AND lfEvalSegs()

IF !llMultWare
  lcCompany=gccom_name
  =gfGetAdr('SycComp' , '' , '' , '' , @laCompany, 1)
  lcAddr1 = laCompany[1]
  lcAddr2 = laCompany[2]
  lcAddr3 = laCompany[3]+laCompany[4]+laCompany[5]
ENDIF

lcTmphdr = gfTempName() 
lcTmpPack = gfTempName() 
lcTmpLin = gfTempName()
lcPackLin = gfTempName()
lcTmpTotal = gfTempName() 

SELECT PACK_HDR  
COPY STRUCTURE TO (lcTmpPack)  

=gfOpenFile(lcTmpPack,'','SH')  
INDEX ON PACK_NO TAG &lcTmpPack  


SELECT * ;         
FROM PACK_HDR;     
WHERE &lcRpExp;   
INTO TABLE (lcTmpPack)  


CREATE TABLE (lcTmpLin) (PACK_NO C(6),FROM_CRT N(4),TO_CRT N(4),NO_CART N(4),STYLE C(19),;
                         COLOR C(6),QTY1 N(6),QTY2 N(6),QTY3 N(6),;
                         QTY4 N(6),QTY5 N(6),QTY6 N(6),QTY7 N(6) ,;
                         QTY8 N(6),TOTQTY N(7),WEIGHT N(8,2))

INDEX ON PACK_NO+STR(FROM_CRT,4)+STR(TO_CRT,4)+STYLE TAG &lcTmpLin
INDEX ON pack_no+STR(no_cart,4)+style TAG 'PACKCRTN' OF lcTmplin+'.CDX' ADDITIVE

CREATE TABLE (lcTmpTotal) (PACK_NO C(6) ,FROM_CRT N(4), TO_CRT N(4),;
                           TOTAL N(7),WEIGHT N(8,2)) 

INDEX ON PACK_NO+STR(FROM_CRT,4)+STR(TO_CRT,4) TAG &lcTmpTotal

SELECT *;
FROM PACK_LIN ;
INTO TABLE (lcPackLin)
INDEX ON PACK_NO + STR(FROM_CRT,4) TAG &lcPackLin

SELECT &lcTmpPack
SCAN 
  IF SEEK(&lcTmpPack..Pack_No,lcPackLin)
    =lfBldLn()  
  ENDIF
ENDSCAN


SELECT STYLE
SET RELATION TO "S" + SCALE INTO SCALE ADDITIVE

SELECT (lcTmpLin)
SET ORDER TO (lcTmpLin)
LOCATE

SET RELATION TO STYLE INTO STYLE ,;
                PACK_NO+STR(FROM_CRT,4)+STR(TO_CRT,4) INTO &lcTmpTotal ,;
                PACK_NO INTO PACK_HDR ADDITIVE


DO gfDispRe WITH EVALUATE('lcRpName')
*-- end of main Program.

*!**************************************************************************
*! Name      : lfBldLn
*! Developer : Ahmed Abdel Naby- (AAN)
*! Date      : 10/10/2000
*! Purpose   : Build the packing list lines.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfBldLn()
*!**************************************************************************
FUNCTION lfBldLn

SET ORDER TO PackStyle IN Pack_lin
SET ORDER TO TAG PACKCRTN IN (lcTmpLin)

SELECT &lcTmpPack

SCAN
  SELECT Pack_lin
  lnStyles = 0
  =SEEK(&lcTmpPack..Pack_No)
  DO WHILE pack_no+STR(no_cart,4)+style = &lcTmpPack..Pack_No
    lnCartonNO = no_cart
    llSameCarton = .F.
    IF lfSameCartn(&lcTmpPack..Pack_No,lnCartonNO,lnStyles)
      llSameCarton  = .T.
    ELSE  
      lnStyles = 0
    ENDIF
    SCAN REST WHILE pack_no+STR(no_cart,4)+style = &lcTmpPack..Pack_No+STR(lnCartonNO,4)
      SCATTER MEMVAR MEMO
      IF llSameCarton
        SELECT (lcTmpLin)
		APPEND BLANK
        REPLACE Pack_No WITH &lcPackLin..Pack_No,;
                To_Crt  WITH lnCartonNO  ,;
                No_Cart WITH lnCartonNO ,;
                Style WITH &lcPackLin..Style,;
                Qty1    WITH Qty1 + m.Qty1 ,;
                Qty2    WITH Qty2 + m.Qty2 ,;
                Qty3    WITH Qty3 + m.Qty3 ,;
                Qty4    WITH Qty4 + m.Qty4 ,;
                Qty5    WITH Qty5 + m.Qty5 ,;
                Qty6    WITH Qty6 + m.Qty6 ,;
                Qty7    WITH Qty7 + m.Qty7 ,;
                Qty8    WITH Qty8 + m.Qty8 ,;
                TotQty  WITH TotQty + m.TotQty ,;
                Weight  WITH Weight + m.Weight
      ELSE  
        STORE m.No_Cart TO m.From_Crt,To_Crt,lnFromCrtn
        INSERT INTO (lcTmpLin) FROM MEMVAR
        lnStyles = lnStyles + 1
      ENDIF
      SELECT (lcTmpTotal)
      IF !SEEK(&lcTmpLin..PACK_NO+STR(&lcTmpLin..FROM_CRT,4)+STR(&lcTmpLin..TO_CRT,4))
         APPEND BLANK
         REPLACE PACK_NO WITH &lcTmpLin..PACK_NO,;
                 FROM_CRT WITH &lcTmpLin..FROM_CRT,;
                 TO_CRT WITH &lcTmpLin..TO_CRT,;
                 TOTAL WITH &lcTmpLin..TOTQTY,;
                 WEIGHT WITH &lcTmpLin..WEIGHT
      ELSE
         REPLACE TOTAL WITH TOTAL + &lcTmpLin..TOTQTY,;
                 WEIGHT WITH WEIGHT + &lcTmpLin..WEIGHT 
      ENDIF
    ENDSCAN
  ENDDO
ENDSCAN
*-- end of 

*!**************************************************************************
*! Name      : lfGetAdd
*! Developer : Ahmed Abdel Naby- (AAN)
*! Date      : 10/10/2000
*! Purpose   : 
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfGetAdd()
*!**************************************************************************
FUNCTION lfGetAdd
PARAMETERS lnLineNo
*--
PRIVATE lnActAlias
lnActAlias = SELECT(0)

STORE SPACE(30) TO lcStname,lcStAddr1,lcStAddr2,lcStAddr3

=SEEK(PACK_HDR.Order,"ORDHDR")
IF ORDHDR.Alt_Shpto
  lcStname  = ORDHDR.StName
  lcStAddr1 = ORDHDR.caddress1
  lcStAddr2 = IIF(EMPTY(ORDHDR.caddress2),ALLTRIM(ORDHDR.caddress3)+", "+;
                  ALLTRIM(ORDHDR.caddress4)+" "+ALLTRIM(ORDHDR.caddress5),ALLTRIM(ORDHDR.caddress2))

  lcStAddr3 = IIF(!EMPTY(ORDHDR.caddress2),ALLTRIM(ORDHDR.caddress3)+", "+;
                  ALLTRIM(ORDHDR.caddress4)+" "+ALLTRIM(ORDHDR.caddress5),"")
ELSE

  IF SEEK(IIF(EMPTY(PACK_HDR.Store),"M","S")+PACK_HDR.Account+PACK_HDR.Store,;
            "CUSTOMER") 
     IF !EMPTY(CUSTOMER.Dist_Ctr)
       =SEEK("S"+CUSTOMER.Account+CUSTOMER.Dist_Ctr,"CUSTOMER")
     ENDIF
    lcStname  = IIF( EMPTY(Customer.DBA) , Customer.STNAME , Customer.DBA)
    = gfGetAdr('CUSTOMER' , '' , '' , '' , @laStAddr)
    lcStAddr1 = laStAddr[1]
    lcStAddr2 = laStAddr[2]
    lcStAddr3 = laStAddr[3]

    IF LEN(TRIM(lcSTADDR2)) =0
      lcSTADDR2 = lcSTADDR3
      lcSTADDR3 = ''
    ENDIF
  ENDIF 
ENDIF
SELECT (lnActAlias)

DO CASE
  CASE lnLineNo = 1
    RETURN lcStname  
  CASE lnLineNo = 2 
    RETURN lcStAddr1
  CASE lnLineNo = 3 
    RETURN lcStAddr2
  OTHERWISE
    RETURN lcStAddr3
ENDCASE
*--

*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 10/10/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs
*PARAMETER lcReturn
STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF
*-- end of lfEvalSegs.

*!**************************************************************************
*! Name      : lfSameCartn
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 10/10/2000
*! Purpose   : Check if the previous carton has the same information
*!**************************************************************************
*! Passed Parameters : lcPackNo    : Packinf list# 
*!                     lnCartnNo   : Carton #
*!                     lnSameLines : Previous carton total lines number
*!**************************************************************************
*! Return      : .T. If the previous carton has the same information
*!**************************************************************************
*! Example     : =lfSameCartn('000001',3,20)
*!**************************************************************************
FUNCTION lfSameCartn
PARAMETERS lcPackNo,lnCartnNo,lnSameLines
PRIVATE llSameCrtn,lnCrtnLines

llSameCrtn  = .T.
lnCrtnLines = 0
SELECT Pack_lin
SCAN REST WHILE pack_no+STR(no_cart,4)+style = lcPackNo+STR(lnCartnNo,4)
  =SEEK(lcPackNo+STR(lnCartnNo-1,4)+style,lcTmpLin)
  lnCrtnRange = &lcTmpLin..To_Crt - &lcTmpLin..From_Crt+1
  IF EOF(lcTmpLin) OR ;
     Qty1*lnCrtnRange  <> &lcTmpLin..Qty1 OR Qty2*lnCrtnRange <>  &lcTmpLin..Qty2 OR ;
     Qty3*lnCrtnRange  <> &lcTmpLin..Qty3 OR Qty4*lnCrtnRange <>  &lcTmpLin..Qty4 OR ;
     Qty5*lnCrtnRange  <> &lcTmpLin..Qty5 OR Qty6*lnCrtnRange <>  &lcTmpLin..Qty6 OR ;
     Qty7*lnCrtnRange  <> &lcTmpLin..Qty7 OR Qty8*lnCrtnRange <>  &lcTmpLin..Qty8 OR ;
     Weight*lnCrtnRange <> &lcTmpLin..Weight
    llSameCrtn = .F.
    EXIT
  ENDIF
  lnCrtnLines = lnCrtnLines + 1
ENDSCAN
llSameCrtn = llSameCrtn AND (lnCrtnLines=lnSameLines)
=SEEK(lcPackNo+STR(lnCartnNo,4))
RETURN(llSameCrtn)


