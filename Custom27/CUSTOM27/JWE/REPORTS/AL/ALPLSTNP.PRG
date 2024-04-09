*:************************************************************
*: Program file  : ALPLSTNP
*: Program desc. : Print Packing List for NAP  C200211
*:        System : Aria Advantage Series.
*:        Module : Allocation (AL).
*:     Developer : HEND GHANEM (HBG)
*:          Date : 08/01/2001   
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Example : DO ALPLSTNP
*:************************************************************


IF lcRpExp = '.T.' 
  =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,'No records selected for report.')
  RETURN
ENDIF


lcRpExp = STRTRAN(lcRpExp,'CUSTOMER','PACK_HDR')
lcRpExp = STRTRAN(lcRpExp,'ORDHDR','PACK_HDR')

lcMjrPct  = gfItemMask('PM')
lnMajorLn = LEN(lcMjrPct)
lCnMJPct  = gfItemMask('PN')
lnNMjrLn  = LEN(lCnMJPct)

IF !llWare
  SELECT SYCCOMP
  SEEK gcAct_Comp
  lcCompany = cCom_Name
  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)

  lcAddr1= laCompAdd[1]
  lcAddr2= IIF(!EMPTY(laCompAdd[2]),laCompAdd[2],ALLTRIM(laCompAdd[3])+", "+;
               ALLTRIM(laCompAdd[4]) +" "+ ALLTRIM(laCompAdd[5]))
  lcAddr3=IIF(!EMPTY(laCompAdd[2]),ALLTRIM(laCompAdd[3])+", "+;
              ALLTRIM(laCompAdd[4]) +" "+ ALLTRIM(laCompAdd[5]),"")
ENDIF

*-- if user change filter expression.(We Collect the Data of the report again)
IF llOGFltCh
  *-- Create Tables needed for this report
  =lfCreatTab()
  *-- Collect data
  =lfCollect()
ENDIF

SELECT &lcTmpLin
GO TOP
SET RELATION TO STYLE+'-'+COLOR INTO STYLE ,;
                PACK_NO+STR(FROM,4)+STR(TO,4) INTO &lcTmpTotal ,;
                PACK_NO INTO PACK_HDR ADDITIVE

SELECT STYLE 
SET RELATION TO "S" + SCALE INTO SCALE ADDITIVE

SELECT &lcTmpLin
SET ORDER TO lcTmpLin
GO TOP

DO gfDispRe WITH EVALUATE('lcRpName') 

SELECT STYLE 
SET RELATION TO 

SELECT &lcTmpLin
SET RELATION TO

*!*************************************************************
*! Name      : lfCreatTab
*! Developer : HEND GHANEM (HBG)
*! Date      : 07/29/2001
*! Purpose   : Create the needed files
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCreatTab


CREATE TABLE (gcWorkDir+lcTmpLin) (PACK_NO C(6),FROM N(4),TO N(4),STYLE C(12),;
                                   COLOR C(6),QTY1 N(5),QTY2 N(5),QTY3 N(5),;
                                   QTY4 N(5),QTY5 N(5),QTY6 N(5),QTY7 N(5),;
                                   QTY8 N(5),TOTQTY N(6),WEIGHT N(8,2))

INDEX ON PACK_NO+STR(FROM,4)+STR(TO,4)+STYLE+COLOR TAG lcTmpLin ADDITIVE

CREATE TABLE (gcWorkDir+lcTmpTotal) (PACK_NO C(6) ,FROM N(4), TO N(4),;
                                     TOTAL N(6),WEIGHT N(8,2)) 

INDEX ON PACK_NO+STR(FROM,4)+STR(TO,4) TAG &lcTmpTotal

SELECT PACK_LIN
=AFIELDS(laTempStru)
CREATE CURSOR (lcTmpPack) FROM ARRAY laTempStru          
ZAP
INDEX ON PACK_NO + STR(FROM_CRT,4) TAG &lcTmpPack
INDEX ON PACK_NO + STYLE + STR(FROM_CRT,4) TAG &lcTmpPkSt

*!*************************************************************
*! Name      : lfCollect
*! Developer : HEND GHANEM (HBG)
*! Date      : 07/29/2001
*! Purpose   : Collect the Data of the report
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCollect


SELECT *;
FROM PACK_LIN ;
INTO TABLE (gcWorkDir+lcPackLin)

INDEX ON Pack_No+STR(line_no,6)+Style+cPackColor TAG &lcPackLin

SELECT (lcTmpPack)
SET ORDER TO lcTmpPkSt


SELECT PACK_HDR
SCAN FOR &lcRpExp
  SELECT &lcPackLin
  
  IF SEEK(PACK_HDR.Pack_No,lcPackLin) 
    lcStyle   = Style
    lnCart_No = No_Cart
    lcFrom    = STR(No_Cart,4)
    SCAN WHILE Pack_No+STR(Line_No,3)+Style =  PACK_HDR.Pack_No
      IF SEEK(Pack_No+Style+lcFrom,lcTmpPack) AND lcStyle = Style  AND No_Cart = lnCart_No + 1
        REPLACE &lcTmpPack..To_Crt  WITH &lcPackLin..No_Cart,;
                &lcTmpPack..No_Cart WITH lnNo_Cart
        lnCart_No = No_Cart 
        lnNo_Cart = lnNo_Cart + 1
      ELSE
        SCATTER MEMVAR MEMO
        SELECT (lcTmpPack)
        APPEND BLANK
        GATHER MEMVAR MEMO
        lnNo_Cart = 1
        REPLACE &lcTmpPack..From_Crt WITH &lcPackLin..No_Cart,;
                &lcTmpPack..To_Crt   WITH &lcPackLin..No_Cart,;
                &lcTmpPack..No_Cart  WITH lnNo_Cart
                                
        lcStyle   = &lcPackLin..Style
        lnCart_No = &lcPackLin..No_Cart   
        lnNo_Cart = lnNo_Cart + 1
        lcFrom    = STR(&lcPackLin..No_Cart,4)
      ENDIF
      SELECT &lcPackLin
    ENDSCAN 
  ENDIF  
  SELECT PACK_HDR 
ENDSCAN

SELECT (lcTmpPack)
SET ORDER TO lcTmpPack

SELECT PACK_HDR
SCAN FOR &lcRpExp
  DIMENSION laRange(1,2)
  STORE 0 TO laRange
  STORE 1 TO lnRange
  IF SEEK(PACK_HDR.Pack_No,lcTmpPack)
    =lfBldLn()  
  ENDIF
ENDSCAN

*!**************************************************************************
*! Name      : lfBldLn
*! Developer : Sherif Attala Ishak
*! Date      : 03/18/98
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

SELECT (lcTmpPack)
SET ORDER TO lcTmpPack

laRange(lnRange,1) = &lcTmpPack..From_Crt
lnTo = &lcTmpPack..To_Crt
      

DO WHILE .T.
  SCAN WHILE Pack_No = PACK_HDR.Pack_No  
    IF (From_Crt <= lnTo)  AND (From_Crt - 1 >= laRange(lnRange,1))
      laRange(lnRange,2) = From_Crt - 1
      EXIT
    ENDIF                    
    lnTo = IIF(lnTo <= &lcTmpPack..To_Crt,IIF(lnTo >= laRange(lnRange,1) ,lnTO, &lcTmpPack..To_Crt),IIF(&lcTmpPack..To_Crt >= laRange(lnRange,1),&lcTmpPack..To_Crt,lnTO))
  ENDSCAN
  laRange(lnRange,2) = IIF(laRange(lnRange,2) = 0,IIF(lnTo < laRange(lnRange,1),PACK_HDR.Tot_Cart,lnTo),laRange(lnRange,2))
  IF laRange(lnRange,2) = PACK_HDR.Tot_Cart
    EXIT
  ENDIF
  =SEEK(PACK_HDR.Pack_No,lcTmpPack)
  lnTo = &lcTmpPack..To_Crt
  lnRange = lnRange + 1
  DIMENSION laRange(lnRange,2)
  laRange(lnRange,1) = laRange(lnRange -1 ,2) + 1
  laRange(lnRange,2) = 0 
ENDDO

FOR lnCount = 1 TO lnRange
  STORE 0 to lnTotal  
  =SEEK(PACK_HDR.Pack_No,lcTmpPack)
  SELECT &lcTmpPack
  SCAN WHILE Pack_No = PACK_HDR.Pack_No  
     IF BETWEEN(laRange(lnCount,1),From_Crt,To_Crt) OR ;
        BETWEEN(laRange(lnCount,2),From_Crt,To_Crt)  
       SELECT &lcTmpLin
       APPEND BLANK
       REPLACE Pack_No WITH &lcTmpPack..Pack_No,;
               From    WITH laRange(lnCount,1),;
               To      WITH laRange(lnCount,2),;                 
               Style   WITH SUBSTR(EVAL(lcTmpPack+'.Style'),1,lnMajorln),;
               Color   WITH SUBSTR(EVAL(lcTmpPack+'.Style'),lnMajorln+2,lnNmjrln),;
               Qty1    WITH &lcTmpPack..Qty1,;
               Qty2    WITH &lcTmpPack..Qty2,;
               Qty3    WITH &lcTmpPack..Qty3,;
               Qty4    WITH &lcTmpPack..Qty4,;
               Qty5    WITH &lcTmpPack..Qty5,;
               Qty6    WITH &lcTmpPack..Qty6,;
               Qty7    WITH &lcTmpPack..Qty7,;
               Qty8    WITH &lcTmpPack..Qty8,;             
               TOTQTY  WITH &lcTmpPack..Totqty,;
               Weight  WITH &lcTmpPack..Weight                

      SELECT &lcTmpTotal
      IF !SEEK(&lcTmpLin..PACK_NO+STR(&lcTmpLin..FROM,4)+STR(&lcTmpLin..TO,4))
         APPEND BLANK
         REPLACE PACK_NO WITH &lcTmpLin..PACK_NO,;
                 FROM    WITH &lcTmpLin..FROM,;
                 TO      WITH &lcTmpLin..TO,;
                 TOTAL   WITH &lcTmpLin..TOTQTY,;
                 WEIGHT  WITH &lcTmpLin..WEIGHT
      ELSE
         REPLACE TOTAL  WITH TOTAL  + &lcTmpLin..TOTQTY,;
                 WEIGHT WITH WEIGHT + &lcTmpLin..WEIGHT 
      ENDIF
    ENDIF
    SELECT &lcTmpPack
  ENDSCAN
ENDFOR


*!*************************************************************
*! Name      : lfvAccount
*! Developer : HEND GHANEM (HBG)
*! Date      : 07/29/2001
*! Purpose   : Valid function of the Account
*!*************************************************************
*! Called from : Option grid [Account Get field]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvAccount

lcObjName = SYS(18)      
lcObjVal = EVALUATE(SYS(18))
laOldVal = EVALUATE(SYS(18))  

*--IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF


*!*************************************************************
*! Name      : lfvOrder
*! Developer : HEND GHANEM (HBG)
*! Date      : 07/29/2001
*! Purpose   : Valid function of the Order number
*!*************************************************************
*! Called from : Option grid [Order number Get field]
*!*************************************************************
*! Calls       : OrdBrowO()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOrder

PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)     
lcObjVal = EVALUATE(SYS(18))
lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))
laOldVal = EVALUATE(SYS(18))  
*--IF The user want to Browse or if the Order number he entered is not in
*--the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('O' + lcObjVal , 'ORDHDR'))
  llBrowse = .T.
  llObjRet = OrdBrowO(@lcObjVal , .F. , 'O')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal

*!*************************************************************
*! Name      : lfSRPack  
*! Developer : HEND GHANEM (HBG)
*! Date      : 07/29/2001
*! Purpose   : 
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
*! Calls       : lfSRPack()
 *!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfSRPack  
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty


*--Get the selected Customer
lnPos = ASCAN(laOGFxFlt,'CUSTOMER')
IF lnPos > 0
  lnPos = ASUBSCRIPT(laOGFxFlt,lnPos,1)   
ENDIF

lcAccount = laOGFxFlt[lnPos,6]

*--Get the selected Order
lnPos = ASCAN(laOGFxFlt,'ORDHDR')
IF lnPos > 0
  lnPos = ASUBSCRIPT(laOGFxFlt,lnPos,1)   
ENDIF

lcFOrd = PADL(laOGFxFlt[lnPos,6],6)
lcTOrd = PADR(laOGFxFlt[lnPos,6],6)

lcFilter = IIF(!EMPTY(lcAccount),"PACK_HDR.Account = lcAccount","") +;
           IIF(!EMPTY(lcAccount) AND (!EMPTY(lcFOrd) OR !EMPTY(lcTOrd))," .AND. ","") +;
           IIF(!EMPTY(lcFOrd) OR !EMPTY(lcTOrd),"BETWEEN(PACK_HDR.Order,lcFOrd,lcTOrd)","")

DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT PACK_HDR
    SET FILTER TO &lcFilter
    LOCATE

  CASE lcParm = 'R'  && Reset code
    SELECT PACK_HDR
    SET FILTER TO
  OTHERWISE      && Valid code
ENDCASE

*!*************************************************************
*! Name      : lfClearRep
*! Developer : HEND GHANEM (HBG)
*! Date      : 07/29/2001
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

llClearFn = .T.
*-- Close temp. opended files, if it used.

IF USED(gcWorkDir+lcTmpLin)
  USE IN (gcWorkDir+lcTmpLin)

  IF FILE(gcWorkDir+lcTmpLin+'.DBF') 
    ERASE(gcWorkDir+lcTmpLin+'.DBF')
  ENDIF

  IF FILE(gcWorkDir+lcTmpLin+'.CDX') 
    ERASE(gcWorkDir+lcTmpLin+'.CDX')
  ENDIF

  IF FILE(gcWorkDir+lcTmpLin+'.FPT') 
    ERASE(gcWorkDir+lcTmpLin+'.FPT')
  ENDIF
ENDIF

IF USED(gcWorkDir+lcTmpTotal)
  USE IN (gcWorkDir+lcTmpTotal)

  IF FILE(gcWorkDir+lcTmpTotal+'.DBF') 
    ERASE(gcWorkDir+lcTmpTotal+'.DBF')
  ENDIF

  IF FILE(gcWorkDir+lcTmpTotal+'.CDX') 
    ERASE(gcWorkDir+lcTmpTotal+'.CDX')
  ENDIF

  IF FILE(gcWorkDir+lcTmpTotal+'.FPT') 
    ERASE(gcWorkDir+lcTmpTotal+'.FPT')
  ENDIF
ENDIF

IF USED(gcWorkDir+lcPackLin)
  USE IN (gcWorkDir+lcPackLin)

  IF FILE(gcWorkDir+lcPackLin+'.DBF') 
    ERASE(gcWorkDir+lcPackLin+'.DBF')
  ENDIF

  IF FILE(gcWorkDir+lcPackLin+'.CDX') 
    ERASE(gcWorkDir+lcPackLin+'.CDX')
  ENDIF

  IF FILE(gcWorkDir+lcPackLin+'.FPT') 
    ERASE(gcWorkDir+lcPackLin+'.FPT')
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ALPKTKT.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *--IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *--IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfGetAdd
*! Developer : HEND GHANEM (HBG)
*! Date      : 07/29/2001
*! Purpose   : Function to Get the Addresses
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetAdd
PARAMETERS lnLineNo

STORE SPACE(30) TO lcStname,lcStAddr1,lcStAddr2,lcStAddr3

=SEEK('O'+PACK_HDR.Order,"ORDHDR")
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
     lcStname  = CUSTOMER.StName
     lcStAddr1 = CUSTOMER.caddress1
     lcStAddr2 = IIF(EMPTY(CUSTOMER.caddress2),ALLTRIM(CUSTOMER.caddress3)+", "+;
                    ALLTRIM(CUSTOMER.caddress4)+" "+ALLTRIM(CUSTOMER.caddress5),ALLTRIM(CUSTOMER.caddress2))
     lcStAddr3 = IIF(!EMPTY(CUSTOMER.caddress2),ALLTRIM(CUSTOMER.caddress3)+", "+;
                    ALLTRIM(CUSTOMER.caddress4)+" "+ALLTRIM(CUSTOMER.caddress5),"")
  ENDIF 
ENDIF

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