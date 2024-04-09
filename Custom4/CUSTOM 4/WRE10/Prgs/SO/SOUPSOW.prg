*:***************************************************************************
*: Program file  : SOUPSOW.prg
*: Program desc. : Custom Program to Update Location Code on SO For Carole Wren
*: System        : Aria Advantage Series.(Aria4XP)
*: Module        : Sales Order (SO)
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 03/24/2011
*! Entry No.     : C201320.122,c201321.EXE- [T20101213.0031]
*!***************************************************************************
*: Modifications:
*********************************************************************************************
lcOldVal   = ""
DECLARE laScopExpr[1]
STORE "" TO laScopExpr
lfCallOG()

FUNCTION lfCallOG
lcExpr = gfOpGrid('SOUPSOW' , .T.)&&,.F.,.F.,.T.,.T.)
IF lcExpr <> ".F."
  llOrderNo = .F.
  lcOrderSel = ''
  lnPosOrder = ASCAN(laScopExpr,"ORDHDR.ORDER")
  IF lnPosOrder> 0 
    lnPosOrder= ASUBSCRIPT(laScopExpr,lnPosOrder,1)
    lcOrderSel=IIF(!EMPTY(laScopExpr[lnPosOrder,6]),laScopExpr[lnPosOrder,6],'')
    IF !EMPTY(lcOrderSel) AND USED(lcOrderSel)
      SELECT(lcOrderSel)
      LOCATE
      IF !EOF()
        llOrderNo= .T.
      ENDIF
    ENDIF  
  ENDIF 

  llAcctNo = .F.
  lcAccSel = ''
  lnPosAcc= ASCAN(laScopExpr,"CUSTOMER.ACCOUNT")
  IF lnPosAcc> 0 
    lnPosAcc= ASUBSCRIPT(laScopExpr,lnPosAcc,1)
    lcAccSel=IIF(!EMPTY(laScopExpr[lnPosAcc,6]),laScopExpr[lnPosAcc,6],'')
    IF !EMPTY(lcAccSel) AND USED(lcAccSel)
      SELECT(lcAccSel)
      LOCATE
      IF !EOF()
        llAcctNo = .T.
      ENDIF
    ENDIF  
  ENDIF 

  
  lcSelWare = ''
  lnPosWare = ASCAN(laScopExpr,"WAREHOUS.CWARECODE")
  IF lnPosWare> 0 
    lnPosWare= ASUBSCRIPT(laScopExpr,lnPosWare,1)
    lcSelWare =IIF(!EMPTY(laScopExpr[lnPosWare,6]),laScopExpr[lnPosWare,6],'')
  ENDIF 

  llEntDate = .F.
  ldSPDate = {}
  ldEPDate = {}
  lnPosEntDate = ASCAN(laScopExpr,"ORDHDR.ENTERED")
  IF lnPosEntDate> 0 
    lnPosEntDate=  ASUBSCRIPT(laScopExpr,lnPosEntDate,1)
    lcEntDate =laScopExpr[lnPosEntDate,6]
    IF !EMPTY(lcEntDate)
      ldSPDate = IIF(EMPTY(SUBSTR(laScopExpr[lnPosEntDate,6],1,10)),CTOD(""),CTOD(SUBSTR(laScopExpr[lnPosEntDate,6],1,10)))
      ldEPDate = IIF(EMPTY(SUBSTR(laScopExpr[lnPosEntDate,6],12,21)),CTOD(""),CTOD(SUBSTR(laScopExpr[lnPosEntDate,6],12,21)))
      llEntDate = .T.
    ENDIF  
  ENDIF
  =gfOpenTable('Ordhdr','Ordhdr','SH','Ordhdr_A')
  =gfOpenTable('OrdLine','OrdLine','SH','OrdLine_A')
  =gfOpenTable('STYDYE','STYDYE','SH','STYDYE_A')
  llOrderupdated = .F.
  
  IF !llOrderNo  AND !llEntDate AND !llAcctNo
    IF gfModalGen('QRM00000B32000','F','ALERT',' ','Do you want to update all Open and Hold Orders?') = 2 
      lfCallOG()
      RETURN .F.
    ENDIF
  ENDIF
  
  DO CASE 
  CASE  llOrderNo && if Order is Selected
    SELECT ORDHDR_A
    =gfSetOrder('ORDHDR')   
    SELECT(lcOrderSel)
    
    SCAN FOR gfSeek('O'+&lcOrderSel..Order,'Ordhdr_A') AND Ordhdr_A.Status $ 'OH' AND Ordhdr_A.cWareCode <> lcSelWare  AND ;
         IIF(llAcctNo ,SEEK(Ordhdr_A.Account,lcAccSel),.T.) AND IIF(llEntDate,BETWEEN(Ordhdr_A.ENTERED,ldSPDate,ldEPDate),.T.) 
      WAIT WINDOW 'Updating Order#:'+&lcOrderSel..Order NOWAIT 
      llOrderupdated = .T.
      lfChngLoc(&lcOrderSel..Order)
    ENDSCAN
  CASE llAcctNo 
    SELECT Ordhdr_A
    =gfSetOrder('ORDACCT')   && ACCOUNT+CORDTYPE+ORDER)
    SELECT (lcAccSel)
    SCAN
      SELECT Ordhdr_A
      =gfSeek(&lcAccSel..Account+'O')
      SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = &lcAccSel..Account+'O' FOR Ordhdr_A.Status $ 'OH' AND Ordhdr_A.cWareCode <> lcSelWare AND IIF(llEntDate,BETWEEN(Ordhdr_A.ENTERED,ldSPDate,ldEPDate),.T.)
        WAIT WINDOW 'Updating Order#:'+Ordhdr_A.Order NOWAIT 
        llOrderupdated = .T.
        lfChngLoc(Ordhdr_A.Order)
      ENDSCAN 
    ENDSCAN 
   OTHERWISE 
     SELECT Ordhdr_A
     =gfSetOrder('ORDHDR')   
     =gfSeek('O')
     SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR Ordhdr_A.Status $ 'OH' AND Ordhdr_A.cWareCode <> lcSelWare AND IIF(llEntDate,BETWEEN(Ordhdr_A.ENTERED,ldSPDate,ldEPDate),.T.)
       WAIT WINDOW 'Updating Order#:'+Ordhdr_A.Order NOWAIT 
        llOrderupdated = .T.
       lfChngLoc(Ordhdr_A.Order)
     ENDSCAN 
  ENDCASE 
  IF llOrderupdated
    SELECT 'STYDYE_A'
    =gfTableUpdate()
    SELECT ORDHDR_A
    =gfTableUpdate()
    SELECT OrdLine_A
    =gfTableUpdate()
    lcMessage = 'Location is updated successfully'
    = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
  ELSE
    = gfModalGen('INM32118B00000','F','ALERT')
    lfCallOG()
  ENDIF  
ENDIF


 *!*************************************************************
*! Name          : lfvOrdWare
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 03/24/2011
*! Purpose       : Valid function of the Order Warehouse
*!*************************************************************
*! Called from : Option grid [Order Warehouse Get field]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOrdWare

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Warehouse he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , lcPkWOldVl , lcObjVal)
  lcPkWOldVl = lcObjVal
  &lcObjName. = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwOldVal
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/24/2011
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Get fields and some of the Option grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwOldVal

lcOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value

 *!*************************************************************
*! Name          : lfSRORDER 
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 03/24/2011
*! Purpose       : Set/ReSet function of the Order 
*!*************************************************************
FUNCTION lfSRORDER 
LPARAMETERS lcStat
IF lcStat = 'S'
  SELECT ORDHDR
  lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
  SET ORDER TO Customer IN Customer
  SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
  GO TOP
ELSE
  SELECT ORDHDR
  SET RELATION OFF INTO CUSTOMER && To customer file.
ENDIF
 *!*************************************************************
*! Name          : lfCreatExp
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 03/24/2011
*! Purpose       : Function to Copy fixed filter array 
*!*************************************************************
FUNCTION lfCreatExp

lcSelWare = ''
lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"WAREHOUS.CWARECODE")
IF lnPosWare> 0 
  lnPosWare= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
  lcSelWare =IIF(!EMPTY(loOgScroll.laOgFxFlt[lnPosWare,6]),loOgScroll.laOgFxFlt[lnPosWare,6],'')
ENDIF 
IF EMPTY(lcSelWare)
  =gfModalGen("TRM42150B00000","DIALOG")
  RETURN .F.
ENDIF


=ACOPY(loOGScroll.laOGFxFlt , laScopExpr)
ENDFUNC 
 *!*************************************************************
*! Name          : lfChngLoc
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 03/24/2011
*! Purpose       : Function to Change location
*!*************************************************************
FUNCTION lfChngLoc
LPARAMETERS lcOrderNum
 =gfSeek('O'+lcOrderNum,'OrdLine_A')
 SELECT OrdLine_A
 SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrderNum
   lcOldLoc = OrdLine_A.cWarecode
   SELECT 'STYDYE_A'
   =gfSeek(OrdLine_A.STYLE+OrdLine_A.CWARECODE+SPACE(10))
   REPLACE ord1 WITH ord1 - OrdLine_A.Qty1,;
           ord2 WITH ord2 - OrdLine_A.Qty2,;   
           ord3 WITH ord3 - OrdLine_A.Qty3,;   
           ord4 WITH ord4 - OrdLine_A.Qty4,;   
           ord5 WITH ord5 - OrdLine_A.Qty5,;   
           ord6 WITH ord6 - OrdLine_A.Qty6,;   
           ord7 WITH ord7 - OrdLine_A.Qty7,;   
           ord8 WITH ord8 - OrdLine_A.Qty8,;   
           totord WITH totord - OrdLine_A.totQty
    =gfAdd_Info('STYDYE_A')                                          
    =gfReplace('')       
    IF gfSeek(OrdLine_A.STYLE+lcSelWare+SPACE(10))
      REPLACE ord1 WITH ord1 + OrdLine_A.Qty1,;
              ord2 WITH ord2 + OrdLine_A.Qty2,;   
              ord3 WITH ord3 + OrdLine_A.Qty3,;   
              ord4 WITH ord4 + OrdLine_A.Qty4,;   
              ord5 WITH ord5 + OrdLine_A.Qty5,;   
              ord6 WITH ord6 + OrdLine_A.Qty6,;   
              ord7 WITH ord7 + OrdLine_A.Qty7,;   
              ord8 WITH ord8 + OrdLine_A.Qty8,;   
              totord WITH totord + OrdLine_A.totQty    
      =gfAdd_Info('STYDYE_A')                                             
      =gfReplace('')                     
    ELSE
      DO gpAdStyWar WITH OrdLine_A.STYLE,SPACE(10),lcSelWare
      SELECT  'STYDYE_A'
      IF gfSeek(OrdLine_A.STYLE+lcSelWare+SPACE(10))
        REPLACE ord1 WITH ord1 + OrdLine_A.Qty1,;
                ord2 WITH ord2 + OrdLine_A.Qty2,;   
                ord3 WITH ord3 + OrdLine_A.Qty3,;   
                ord4 WITH ord4 + OrdLine_A.Qty4,;   
                ord5 WITH ord5 + OrdLine_A.Qty5,;   
                ord6 WITH ord6 + OrdLine_A.Qty6,;   
                ord7 WITH ord7 + OrdLine_A.Qty7,;   
                ord8 WITH ord8 + OrdLine_A.Qty8,;   
                totord WITH totord + OrdLine_A.totQty    
        =gfAdd_Info('STYDYE_A')                               
        =gfReplace('')                     
      ENDIF
    ENDIF   
    SELECT OrdLine_A           
    REPLACE cWareCode WITH lcSelWare
    =gfAdd_Info('OrdLine_A')                                   
     =gfReplace('')                     
 ENDSCAN 
gfSeek('O'+lcOrderNum,'Ordhdr_A','ORDHDR') 
SELECT Ordhdr_A
REPLACE cWareCode WITH lcSelWare
=gfAdd_Info('Ordhdr_A')            
=gfReplace('')                     
 