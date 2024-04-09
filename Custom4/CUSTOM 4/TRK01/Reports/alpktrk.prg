*:***************************************************************************
*: Program file  : ALPIKTKT
*: Program desc. : Export Pitkt to CSV File For TRK01
*! Date          : 02/18/09
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar - (MMT) 
*: Ticket        : [T20081027.0027]
*: Entry         : C201107 Aria27, C201108 Aria4
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO 
*:***************************************************************************
*: Modifications:
*B609077,1 MMT 11/09/2009 Fix bug of export not picked sizes[T20081027.0027]
*B609077,2 MMT 01/26/2010 Fill columns AE,AD,AR,AQ Based on Customer Country[T20081027.0027]
*:***************************************************************************
*--Section of Variables
*To get the date in a variables
 
IF EMPTY(lcRpPath) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File Name")
  RETURN 
ENDIF 

lcPth = JUSTPATH(lcRpPath)
IF !DIRECTORY(lcPth)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File Name")
  RETURN 
ENDIF 

lcRpPath = FORCEEXT(lcRpPath,'CSV')


lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
IF lnPosDate > 0 
  lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
  LDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnPosDate,6],1,10)))
  HDATE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnPosDate,6],12,20)))
ENDIF   

IF loOGScroll.llOGFltCh
  WAIT WINDOW "Collecting Data......." NOWAIT 
  DO lpCreaTemp  && Create Temp Cursor
  DO lpColect    && Collect data
ENDIF 

SELECT (lcPickTmp)   &&Temp. File Hold The Records That Satisfy Requiments 
SET ORDER TO TAG &lcPickTmp

SELECT(lcPickTmp)
LOCATE   


SELECT(lcPickTmp)
SET FILTER TO 
SELECT(lcPickTmp)
LOCATE 
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF 


lfExportToFile()


*-- Clear relation
SELECT (lcPickTmp)



*!**************************************************************************
*! Name      : lfSeTOrdr
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 02/18/09
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


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 02/18/09
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
FUNCTION lfwRepWhen

= lfvInvoice()
*--Ordline File
=gfOpenTable("Ordline","ORDLINST")

*--PikLine File
=gfOpenTable("PikLine","PikLine")

*--Ordhdr File
=gfOpenTable("Ordhdr","Ordhdr")

*--PikTkt file 
=gfOpenTable("PikTkt","PikTkt")

*--Customer file 
=gfOpenTable("Customer","Customer")
  
* Pack_Hdr File
=gfOpenTable("Pack_hdr","Pack_hdr")

*Scale File
=gfOpenTable("Scale","Scale")

*WareHous table
=gfOpenTable("WareHous","WareHous")

*Pikline Table 
=gfOpenTable("PIKLINE","PIKLINE")

*Style File
=gfOpenTable("STYLE","STYLE")
*!*************************************************************
*! Name      : lfvInvoice
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 02/18/09
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
*! Date      : 02/18/09
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
*! Date      : 02/18/09
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
*! Name      : lfClearRep
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 02/18/09
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
*!**************************************************************************
*! Name      : lpCreaTemp
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 02/18/09
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
    SELECT PIKTKT
    =AFIELDS(laTempStru)
    =lfAddField("laTempStru","cCurrCode","C",3,0)
    =lfAddField("laTempStru","lEndRep","L",1,0)
    =lfAddField("laTempStru","SHIPVIA","C",30,0)
    =lfAddField("laTempStru","TOTAMT","N",13,3)
    =lfAddField("laTempStru","QTYTOT","N",9,0)
  ENDIF
  
    =gfCrtTmp(lcPickTmp,@laTempStru,IIF(LCRPSORT='P',"PIKTKT","SHIPVIA"),lcPickTmp,.T.)

  SELECT(lcPickTmp)
  ZAP
ENDIF
*-- End of lpCreaTemp.

*!**************************************************************************
*! Name      : lpColect
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 02/18/09
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
      *--Check if user select order no.
      llOrderSelected = .F.
      lnPosOrder = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ORDER")
  	  IF lnPosOrder > 0 
	     lnPosOrder = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosOrder,1)
	     lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnPosOrder,6]),laOgFxFlt[lnPosOrder,6],'')
         IF !EMPTY(lcOrderSel)
           SELECT(lcOrderSel)
           LOCATE
           IF !EOF()
		     llOrderSelected = .T.
    	   ENDIF 
     	ENDIF   
      ENDIF 	
      *--Check if user select warecode 
      llWareHouseSelected = .F.
      lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
      IF lnPosWare > 0 
        lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
        lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
        IF !EMPTY(lcWareSel)
          SELECT(lcWareSel)
          LOCATE
          IF !EOF()
		    llWareHouseSelected = .T.           
          ENDIF 
        ENDIF 
      ENDIF
      
      *--Check if user select accounts 
      llAccountSelected = .F.
      lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
      IF lnPosAcc > 0 
        lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
        lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosAcc,6]),laOgFxFlt[lnPosAcc,6],'')
        IF !EMPTY(lcAccSel)
          SELECT(lcAccSel)
          LOCATE
          IF !EOF()
            llAccountSelected = .T.
          ENDIF 
        ENDIF 
      ENDIF   
      *--Check Date
      llDateSelected = .F.
      lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
      IF lnPosDate > 0 
        lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
        SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
        EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
        
        IF SUBSTR(laOgFxFlt[lnPosDate,6],1,1) = "|"
          SDATE = DTOC({})
          EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],2,11)
        ENDIF 
        IF (!EMPTY(EDATE) AND !EMPTY(SDATE)) OR (!EMPTY(EDATE) AND EMPTY(SDATE))
	      llDateSelected = .T.         
        ENDIF 
      ENDIF
      
      SELECT(lcPikTktSel)
      LOCATE 
      SCAN 
        =gfSeek(&lcPikTktSel..PIKTKT,'PIKTKT')
        IF llDateSelected AND !BETWEEN(PIKTKT.DATE,CTOD(SDATE),CTOD(EDATE))
          LOOP 
        ENDIF 
        
        IF llAccountSelected  AND !SEEK(PIKTKT.Account,lcAccSel)
          LOOP 
        ENDIF 
                
        IF llWareHouseSelected AND !SEEK(PIKTKT.CWARECODE,lcWareSel)
          LOOP 
        ENDIF 
        
        IF llOrderSelected AND !SEEK(PIKTKT.ORDER,lcOrderSel)
          LOOP 
        ENDIF 
     	=lfGetOrdhdrFile()   
      ENDSCAN   
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
      llWareHouseSelected = .F.
      lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
      IF lnPosWare > 0 
        lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
        lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
        IF !EMPTY(lcWareSel)
          SELECT(lcWareSel)
          LOCATE
          IF !EOF()
		    llWareHouseSelected = .T.           
          ENDIF 
        ENDIF 
      ENDIF
      
      *--Check if user select accounts 
      llAccountSelected = .F.
      lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
      IF lnPosAcc > 0 
        lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
        lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosAcc,6]),laOgFxFlt[lnPosAcc,6],'')
        IF !EMPTY(lcAccSel)
          SELECT(lcAccSel)
          LOCATE
          IF !EOF()
            llAccountSelected = .T.
          ENDIF 
        ENDIF 
      ENDIF   
      *--Check Date
      llDateSelected = .F.
      lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
      IF lnPosDate > 0 
        lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
        SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
        EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
        
        IF SUBSTR(laOgFxFlt[lnPosDate,6],1,1) = "|"
          SDATE = DTOC({})
          EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],2,11)
        ENDIF 
        IF (!EMPTY(EDATE) AND !EMPTY(SDATE)) OR (!EMPTY(EDATE) AND EMPTY(SDATE))
	      llDateSelected = .T.         
        ENDIF 
      ENDIF
    
    
      SELECT PikTkt
      gfSetOrder("ORDPIK")
      SELECT(lcOrderSel)
      SCAN 
        SELECT PikTkt
        =gfseek(&lcOrderSel..ORDER)
        SCAN REST WHILE order+piktkt = &lcOrderSel..ORDER
          IF llDateSelected AND !BETWEEN(PIKTKT.DATE,CTOD(SDATE),CTOD(EDATE))
            LOOP 
          ENDIF 
          
          IF llAccountSelected  AND !SEEK(PIKTKT.Account,lcAccSel)
            LOOP 
          ENDIF 
                  
          IF llWareHouseSelected AND !SEEK(PIKTKT.CWARECODE,lcWareSel)
            LOOP 
          ENDIF 
          =lfGetOrdhdrFile() 
        ENDSCAN 
      ENDSCAN 
      RETURN 
    ENDIF 
  ENDIF 
ENDIF   
*--if user select account,warehous,date
llWareHouseSelected = .F.
lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
IF lnPosWare > 0 
  lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
  lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
  IF !EMPTY(lcWareSel)
    SELECT(lcWareSel)
    LOCATE
    IF !EOF()
  llWareHouseSelected = .T.           
    ENDIF 
  ENDIF 
ENDIF

*--Check if user select accounts 
llAccountSelected = .F.
lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
IF lnPosAcc > 0 
  lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
  lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosAcc,6]),laOgFxFlt[lnPosAcc,6],'')
  IF !EMPTY(lcAccSel)
    SELECT(lcAccSel)
    LOCATE
    IF !EOF()
      llAccountSelected = .T.
    ENDIF 
  ENDIF 
ENDIF   
*--Check Date
llDateSelected = .F.
lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
IF lnPosDate > 0 
  lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
  SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
  EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
  
  IF SUBSTR(laOgFxFlt[lnPosDate,6],1,1) = "|"
    SDATE = DTOC({})
    EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],2,11)
  ENDIF 
  IF (!EMPTY(EDATE) AND !EMPTY(SDATE)) OR (!EMPTY(EDATE) AND EMPTY(SDATE))
  llDateSelected = .T.         
  ENDIF 
ENDIF
    
SELECT  PikTkt
=gfSeek('')
SCAN 
  IF llDateSelected AND !BETWEEN(PIKTKT.DATE,CTOD(SDATE),CTOD(EDATE))
    LOOP 
  ENDIF 
          
  IF llAccountSelected  AND !SEEK(PIKTKT.Account,lcAccSel)
    LOOP 
  ENDIF 
                  
  IF llWareHouseSelected AND !SEEK(PIKTKT.CWARECODE,lcWareSel)
    LOOP 
  ENDIF 
  =lfGetOrdhdrFile()
ENDSCAN   
RETURN 
*-- end of lpColect.

*!**************************************************************************
*! Name      : lfAddField
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 02/18/09
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
*! Name      : lfGetOrdhdrFile
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/18/09
*! Purpose   : function to get Orhdr Data
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetOrdhdrFile

lcScanExp = "IIF(lcRpInv = 'B',IIF(llRPRelPT,.T.,PikTKT.Status $ 'OCHP'),"
lcScanExp = lcScanExp +" IIF(lcRpInv = 'Y' ,  PikTKT.Status$'C',"
lcScanExp = lcScanExp +"IIF(llRPRelPT,PikTKT.Status $ 'OHPX',PikTKT.Status $ 'OHP' ))) =  .T.  AND"
lcScanExp = lcScanExp +" IIF(lcRpPrint='B',.T.,"
lcScanExp = lcScanExp +"IIF(lcRpPrint='Y' , PikTKT.PrtFlag= 'P',PikTKT.PrtFlag<>'P')) =  .T."

=gfSeek("O"+PikTKT.ORDER,'Ordhdr')

llHasPack_List = .F.
IF lcRPPCKLST  <> 'B'
  llHasPack_List  = IIF(lcRPPCKLST = 'Y',.T.,.F.)
ENDIF 

SELECT PIKTKT
IF &lcScanExp AND Piktkt # '******' AND IIF(llHasPack_List,gfSeek(piktkt,'PAck_hdr'),.T.)
  SCATTER MEMVAR MEMO
  m.cCurrCode = OrdHdr.cCurrCode
  M.SHIPVIA=   gfCodDes(ORDHDR.SHIPVIA,'SHIPVIA')
  m.TOTAMT = 0
  m.QTYTOT = 0
  IF PIKTKT.Status <> 'C'
    SELECT Ordline
    =gfSeek('O'+Ordhdr.Order)
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6)='O'+Ordhdr.Order for Piktkt = Piktkt.Piktkt
      m.TOTAMT = m.TOTAMT +  Ordline.TotPik * Ordline.Price
      m.QTYTOT = m.QTYTOT + Ordline.TotPik
    ENDSCAN 
  ELSE
    SELECT PIKLINE
    =gfSeek(Piktkt.Piktkt)
    SCAN REST WHILE PIKTKT+ORDER+STR(LINENO,6) = Piktkt.Piktkt
      m.TOTAMT = m.TOTAMT +  PIKLINE.TotPik * PIKLINE.Price
      m.QTYTOT = m.QTYTOT + PIKLINE.TotPik
    ENDSCAN 
  ENDIF
  INSERT INTO (lcPickTmp) FROM MEMVAR
ENDIF 

*!**************************************************************************
*! Name      : lfvPath
*! Developer : Mariam Mazhar (MMT)
*! Date      : 02/18/2009
*! Purpose   : Get File name and path
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfvPath()
*!**************************************************************************
*
FUNCTION lfvPath
IF ALLTRIM(lcRpPath)  = "?"
 lcRpPath = GETFILE('CSV','','Create')
ENDIF

*!**************************************************************************
*! Name      : lfExportToFile
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 02/18/09
*! Purpose   : Export to CSV File
*!**************************************************************************
*! Example   : =lfExportToFile()
*!**************************************************************************
FUNCTION lfExportToFile
lnFHandle = FCREATE(lcRpPath)

IF !(lnFHandle > 0 )
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Could not create CSV File")
  RETURN 
ENDIF 

*Put Header Record
lcHeaderVar = "PickTicketNumber,Warehouse,OrderDate,StartDate,CancelDate,ShipDate,"
lcHeaderVar = lcHeaderVar + "PaymentTerms,Certified Funds,Department,Store,ShipVia,ShipViaService,"
lcHeaderVar = lcHeaderVar + "ShipViaAccountNumber,BillingOption,DeclaredValuePercentage,SpecialInstructions,"
lcHeaderVar = lcHeaderVar + "CustomerPONumber,AuthorizationNumber,OrderNumber,TakenBy,"
lcHeaderVar = lcHeaderVar + "BillToCode,BillToFirstName,BillToLastName,BillToCompanyName',"
lcHeaderVar = lcHeaderVar + "BillToAddress1,BillToAddress2,BillToCity,BillToState,BillToZip,"
lcHeaderVar = lcHeaderVar + "BillToCountry,BillToNon_US_Region,BillToPhone,BillToEmail,ShipToCode,"
lcHeaderVar = lcHeaderVar + "ShipToFirstName,ShipToLastName,ShipToCompanyName,ShipToAddress1,ShipToAddress2,"
lcHeaderVar = lcHeaderVar + "ShipToCity,ShipToState,ShipToZip,ShipToCountry,ShipToNon_US_Region,ShipToPhone,"
lcHeaderVar = lcHeaderVar + "ShipToEmail,LineItemsTotalAmount,LineItemsTotalQuantity,LineItemSeason,LineItemStyle,"
lcHeaderVar = lcHeaderVar + "LineItemDescription,LineItemColor,LineItemSize,LineItemUPC,LineItemUnitPrice,"
lcHeaderVar = lcHeaderVar + "LineItemQuantity"

FPUTS(lnFHandle ,lcHeaderVar)

SELECT (lcPickTmp)
SCAN
  lcWareCode = CWARECODE
  lcOrder = Order
  lcPiktkt = Piktkt
  =gfSeek(lcWareCode ,'WareHous')
  =gfSeek('O'+lcOrder,'Ordhdr')
  =GFSEEK(IIF(!EMPTY(&lcPickTmp..Store),'S'+Ordhdr.account+&lcPickTmp..Store,'M'+Ordhdr.account),'Customer')
  lcTermsDesc = gfCodDes(ordhdr.ctermcode,'CTERMCODE ')
  lcUPSType = ''
  lcUPSBill = ''
  DIMENSION laShipRel[2,2]  
  laShipRel[1,1] = "CUPS      "
  laShipRel[1,2] = "lcUPSType"
  
  laShipRel[2,1] = "CUPSBILL  "
  laShipRel[2,2] = "lcUPSBill"
        *-- Fill the related GL information from the codes file.
  llNoThing = gfRltFld(ordhdr.Shipvia, @laShipRel, "SHIPVIA   ")

  IF &lcPickTmp..Status <> 'C'
    SELECT Ordline
    =gfSeek('O'+lcOrder)
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder FOR Piktkt = lcPiktkt
      lcLineToAdd = ""+PIKTKt+","+Warehous.Cdesc+","+DTOC(Ordhdr.Entered)+","+DTOC(Ordline.Start)+","
      lcLineToAdd = lcLineToAdd +DTOC(Ordline.Complete)+",,"+lcTermsDesc+",,"+Ordhdr.dept+","+Ordline.Store+"," 
      lcLineToAdd = lcLineToAdd +&lcPickTmp..ShipVia+","+lcUPSType+",,"+lcUPSBill+",,"+ORdhdr.Note1+","
      lcLineToAdd = lcLineToAdd +ORDLINE.CUSTPO+",,"+ORDLINE.ORDER+",,,,,"+Customer.btName+","
      lcLineToAdd = lcLineToAdd + Customer.Caddress12+"," + Customer.Caddress22+","
      lcLineToAdd = lcLineToAdd + Customer.Caddress32+"," + Customer.Caddress42+","
*B609077,2 MMT 01/26/2010 Fill columns AE,AD,AR,AQ Based on Customer Country[Start]
*!*	      lcLineToAdd = lcLineToAdd + Customer.Caddress52+"," + Customer.Caddress62+","
*!*	      lcLineToAdd = lcLineToAdd + Customer.Caddress62+",,,,,,"
      lcLineToAdd = lcLineToAdd + Customer.Caddress52+"," + IIF(ALLTRIM(Customer.Caddress62)= 'USA',Customer.Caddress62,'')+","
      lcLineToAdd = lcLineToAdd + IIF(ALLTRIM(Customer.Caddress62)= 'USA','',Customer.Caddress62)+",,,,,,"
*B609077,2 MMT 01/26/2010 Fill columns AE,AD,AR,AQ Based on Customer Country[End]
      
     
      lcLineToAdd = lcLineToAdd + Customer.Stname+","
      
      IF ordhdr.alt_shpto
        lcLineToAdd = lcLineToAdd + ORDHdR.Caddress1+","
        lcLineToAdd = lcLineToAdd + ORDHdR.Caddress2+","
        lcLineToAdd = lcLineToAdd + ORDHdR.Caddress3+","
        lcLineToAdd = lcLineToAdd + ORDHdR.Caddress4+","
        lcLineToAdd = lcLineToAdd + ORDHdR.Caddress5+","
		*B609077,2 MMT 01/26/2010 Fill columns AE,AD,AR,AQ Based on Customer Country[Start]
        *lcLineToAdd = lcLineToAdd + ","
        *lcLineToAdd = lcLineToAdd + ",,,"+STR(&lcPickTmp..TOTAMT ,13,3)+","        
        lcLineToAdd = lcLineToAdd +IIF(ALLTRIM(Customer.Caddress6)='USA',Customer.Caddress6,'') +","
        lcLineToAdd = lcLineToAdd +IIF(ALLTRIM(Customer.Caddress6)='USA','',Customer.Caddress6) +",,,"+STR(&lcPickTmp..TOTAMT ,13,3)+","
        *B609077,2 MMT 01/26/2010 Fill columns AE,AD,AR,AQ Based on Customer Country[End]
      ELSE
        lcDistCntr = CUSTOMER.Dist_Ctr
        IF !EMPTY(lcDistCntr) AND !(ORDHDR.lStrDirct)
          GFSEEK('S' + Ordhdr.account + lcDistCntr,'CUSTOMER')
        ENDIF
        lcLineToAdd = lcLineToAdd + Customer.Caddress1+","
        lcLineToAdd = lcLineToAdd + Customer.Caddress2+","
        lcLineToAdd = lcLineToAdd + Customer.Caddress3+","
        lcLineToAdd = lcLineToAdd + Customer.Caddress4+","
        lcLineToAdd = lcLineToAdd + Customer.Caddress5+","
        *B609077,2 MMT 01/26/2010 Fill columns AE,AD,AR,AQ Based on Customer Country[Start]
        *lcLineToAdd = lcLineToAdd + Customer.Caddress6+","
        *lcLineToAdd = lcLineToAdd + Customer.Caddress6+",,,"+STR(&lcPickTmp..TOTAMT ,13,3)+","
        lcLineToAdd = lcLineToAdd + IIF(ALLTRIM(Customer.Caddress6)='USA',Customer.Caddress6,'') +","
        lcLineToAdd = lcLineToAdd + IIF(ALLTRIM(Customer.Caddress6)='USA','',Customer.Caddress6)+",,,"+STR(&lcPickTmp..TOTAMT ,13,3)+","
		*B609077,2 MMT 01/26/2010 Fill columns AE,AD,AR,AQ Based on Customer Country[End]
      ENDIF   
      lcLineToAdd = lcLineToAdd + STR(&lcPickTmp..QTYTOT,9)+","
      lcLineToAdd = lcLineToAdd + gfCodDes(ORdline.Season,'SEASON    ')+","
      lcLineToAdd = lcLineToAdd + SUBSTR(ORdline.Style,1,12)+","
      lcLineToAdd = lcLineToAdd + ORdline.Desc1+","
      lcLineToAdd = lcLineToAdd + RIGHT(ORdline.Style,6)+","  
      =gfSeek(ORdline.Style,'STYLE')
      =gfSeek('S'+Style.Scale,'SCaLE')
      lcSharedData = lcLineToAdd 
      FOR lnT = 1 TO Scale.Cnt
        lcT = STR(lnT ,1)
        *B609077,1 MMT 11/09/2009 Fix bug of export not picked sizes[Start]
        IF ORdline.Pik&lcT. = 0
          LOOP 
        ENDIF 
        *B609077,1 MMT 11/09/2009 Fix bug of export not picked sizes[End]
        lcLineToAdd = lcLineToAdd + Scale.Sz&lcT. +",,"
        lcLineToAdd = lcLineToAdd + STR(ORdline.Price,12,2)+","
        lcLineToAdd = lcLineToAdd + STR(ORdline.Pik&lcT.,5)+""
         =FPUTS(lnFHandle ,lcLineToAdd)
         lcLineToAdd = lcSharedData  
      ENDFOR 
      
    ENDSCAN   
  ELSE
    SELECT Pikline
    =gfSeek(lcPiktkt)
    SCAN REST WHILE PIKTKT+ORDER+STR(LINENO,6) = lcPiktkt 

      lcLineToAdd = ""+PIKTKT+","+Warehous.Cdesc+","+DTOC(Ordhdr.Entered)+","+DTOC(Pikline.Start)+","
      lcLineToAdd = lcLineToAdd +DTOC(Pikline.Complete)+",,"+lcTermsDesc+",,"+Ordhdr.dept+","+Pikline.Store+"," 
      lcLineToAdd = lcLineToAdd +&lcPickTmp..ShipVia+","+lcUPSType+",,"+lcUPSBill+",,"+ORdhdr.Note1+","
      lcLineToAdd = lcLineToAdd +Pikline.CUSTPO+",,"+Pikline.ORDER+",,,,,"+Customer.btName+","
      lcLineToAdd = lcLineToAdd + Customer.Caddress12+"," + Customer.Caddress22+","
      lcLineToAdd = lcLineToAdd + Customer.Caddress32+"," + Customer.Caddress42+","
      lcLineToAdd = lcLineToAdd + Customer.Caddress52+"," + Customer.Caddress62+","
      lcLineToAdd = lcLineToAdd + Customer.Caddress62+",,,,,,"
      lcLineToAdd = lcLineToAdd + Customer.Stname+","
      
      IF ordhdr.alt_shpto
      
        lcLineToAdd = lcLineToAdd + ORDHdR.Caddress1+","
        lcLineToAdd = lcLineToAdd + ORDHdR.Caddress2+","
        lcLineToAdd = lcLineToAdd + ORDHdR.Caddress3+","
        lcLineToAdd = lcLineToAdd + ORDHdR.Caddress4+","
        lcLineToAdd = lcLineToAdd + ORDHdR.Caddress5+","
        lcLineToAdd = lcLineToAdd + ","
        lcLineToAdd = lcLineToAdd + ",,,"+STR(&lcPickTmp..TOTAMT ,13,3)+","
      
      ELSE
      
        lcDistCntr = CUSTOMER.Dist_Ctr
        IF !EMPTY(lcDistCntr) AND !(ORDHDR.lStrDirct)
          GFSEEK('S' + Ordhdr.account + lcDistCntr,'CUSTOMER')
        ENDIF
        
        lcLineToAdd = lcLineToAdd + Customer.Caddress1+","
        lcLineToAdd = lcLineToAdd + Customer.Caddress2+","
        lcLineToAdd = lcLineToAdd + Customer.Caddress3+","
        lcLineToAdd = lcLineToAdd + Customer.Caddress4+","
        lcLineToAdd = lcLineToAdd + Customer.Caddress5+","
        lcLineToAdd = lcLineToAdd + Customer.Caddress6+","
        lcLineToAdd = lcLineToAdd + Customer.Caddress6+",,,"+STR(&lcPickTmp..TOTAMT ,13,3)+","
      ENDIF   
      lcLineToAdd = lcLineToAdd + STR(&lcPickTmp..QTYTOT,9)+","
      lcLineToAdd = lcLineToAdd + gfCodDes(Pikline.Season,'SEASON    ')+","
      lcLineToAdd = lcLineToAdd + SUBSTR(Pikline.Style,1,12)+","
      lcLineToAdd = lcLineToAdd + Pikline.Desc1+","
      lcLineToAdd = lcLineToAdd + RIGHT(Pikline.Style,6)+","  
      =gfSeek(Pikline.Style,'STYLE')
      =gfSeek('S'+Style.Scale,'SCaLE')
      lcSharedData = lcLineToAdd 
      FOR lnT = 1 TO Scale.Cnt
        lcT = STR(lnT ,1)
        *B609077,1 MMT 11/09/2009 Fix bug of export not picked sizes[Start]
        IF Pikline.Pik&lcT. = 0
          LOOP 
        ENDIF 
        *B609077,1 MMT 11/09/2009 Fix bug of export not picked sizes[End]
        lcLineToAdd = lcLineToAdd + Scale.Sz&lcT. +",,"
        lcLineToAdd = lcLineToAdd + STR(Pikline.Price,12,2)+","
        lcLineToAdd = lcLineToAdd + STR(Pikline.Pik&lcT.,5)+""
        FPUTS(lnFHandle ,lcLineToAdd)
        lcLineToAdd = lcSharedData  
      ENDFOR 
    ENDSCAN 
  ENDIF   
ENDSCAN 
FCLOSE(lnFHandle)

IF FILE(lcRpPath)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File "+ALLTRIM(lcRpPath)+" has been exported successfully")
  RETURN 
ENDIF 
