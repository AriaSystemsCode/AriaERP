*:***************************************************************************
*: Program file : SJB100.PRG
*: DESC : PRINT SHIPPING LABELS FROM ORDERS, PIKTKTS OR INVOICES (For SJB)
*: Module: Aria Apparel Series.
*: DATE : 07/30/2001
*: Developer: Ahmed Abdel Naby (AAN)
*: Reference: 102290 
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:         PROCEDURE  : 
*:************************************************************************

PRIVATE lcWareCode,HDRL1,HDRL2,HDRL3,HDRL4,HDRL5
STORE "" TO lcWareCode,HDRL1,HDRL2,HDRL3,HDRL4,HDRL5
DECLARE laCompAdd[5]
STORE "" TO laCompAdd

IF llData = .F.
  DECLARE xWk[5],xPo[5],xStyle[5],xPcs[5]
  FOR lcCnt=1 TO 5
    STORE "" TO xWk,xPo,xStyle,xPcs
  ENDFOR
ENDIF


*--piktkt
llUsePIK = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'PIKTKT.PIKTKT'),1)
IF lnPosition > 0
  lcPIKlFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUsePIK  = IIF(!EMPTY(lcPIKlFile) .AND. USED(lcPIKlFile) .AND. RECCOUNT(lcPIKlFile)>0,.T.,.F.)
ENDIF
IF llUsePIK  
  SELECT(lcPIKlFile)
  LOCATE 
  IF EOF()
    llUsePIK  = .F.
  ENDIF 
ENDIF 

ldtDateS = {}
ldDateE = {}

*-- To get the Piktkt date.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(LOOGSCROLL.laOGFXFlt,'PIKTKT.DATE'),1)
IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGFXFlt[lnPosition,6])
 ldtDateS  = IIF(EMPTY(SUBSTR(laOGFXFlt[lnPosition,6],1,10)),DTOC(CTOD("")),SUBSTR(laOGFXFlt[lnPosition,6],1,10))
 ldDateE   = IIF(EMPTY(SUBSTR(laOGFXFlt[lnPosition,6],12,21)),DTOC(CTOD("")),SUBSTR(laOGFXFlt[lnPosition,6],12,21))
ENDIF


SELECT PIKTKT
*SET RELATION TO "O"+ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER

llMultiWH = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',oAriaApplication.ActiveCompanyID)) = 'Y', .T. , .F.)

=lfCretTemp()



IF llUsePIK  
  SELECT(lcPIKlFile)
  SCAN 
    IF loDBFPiktkt.Seek(&lcPIKlFile..Piktkt) AND IIF(!EMPTY(ldtDateS) AND !EMPTY(ldDateE),BETWEEN(PIKTKT.DATE,CTOD(ldtDateS),CTOD(ldDateE)),.T.)
	  SELECT PIKTKT
      =lfGetWareh()
      m.Desc = HDRL1
      m.Add1 = HDRL2
      m.Add2 = HDRL3
      m.Add3 = HDRL4
      m.Phone = HDRL5
      m.Piktkt = PikTkt
      m.Account = Account
      m.Store = Store
      m.Order = Order
      m.Labels = lnLabelNum
      m.Date = Date
      m.Flag = Flag
      m.PrtFlag = PrtFlag
    IF loDBFCustomer.Seek(IIF(STORE=SPACE(8),'M'+ACCOUNT, 'S'+ACCOUNT+STORE),'Customer') 
      m.Cust_CAdd1  = Customer.caddress1
      m.Cust_CAdd2  = Customer.caddress2
	  m.Cust_CAdd3  = Customer.caddress3
      m.Cust_CAdd4  = Customer.caddress4
      m.Cust_CAdd5  = Customer.caddress5
      m.Cust_Dba    = Customer.dba
      m.Cust_Stname = Customer.stname
    ELSE
      STORE '' TO m.Cust_CAdd1 ,m.Cust_CAdd2,m.Cust_CAdd3,m.Cust_CAdd4,m.Cust_CAdd5,m.Cust_Dba,m.Cust_Stname
    ENDIF 
      INSERT INTO (lcTmplbl) FROM MEMVAR
    ENDIF 
  ENDSCAN 
ELSE
  
  loDBFPiktkt.Seek('')
  SELECT PIKTKT
  SCAN FOR IIF(!EMPTY(ldtDateS) AND !EMPTY(ldDateE),BETWEEN(PIKTKT.DATE,CTOD(ldtDateS),CTOD(ldDateE)),.T.)
    SELECT PIKTKT
    =lfGetWareh()
    m.Desc = HDRL1
    m.Add1 = HDRL2
    m.Add2 = HDRL3
    m.Add3 = HDRL4
    m.Phone = HDRL5
    m.Piktkt = PikTkt
    m.Account = Account
    m.Store = Store
    m.Order = Order
    m.Labels = lnLabelNum
    m.Date = Date
    m.Flag = Flag
    m.PrtFlag = PrtFlag
    IF loDBFCustomer.Seek(IIF(STORE=SPACE(8),'M'+ACCOUNT, 'S'+ACCOUNT+STORE),'Customer') 
      m.Cust_CAdd1  = Customer.caddress1
      m.Cust_CAdd2  = Customer.caddress2
	  m.Cust_CAdd3  = Customer.caddress3
      m.Cust_CAdd4  = Customer.caddress4
      m.Cust_CAdd5  = Customer.caddress5
      m.Cust_Dba    = Customer.dba
      m.Cust_Stname = Customer.stname
    ELSE
      STORE '' TO m.Cust_CAdd1 ,m.Cust_CAdd2,m.Cust_CAdd3,m.Cust_CAdd4,m.Cust_CAdd5,m.Cust_Dba,m.Cust_Stname
    ENDIF 
    INSERT INTO (lcTmplbl) FROM MEMVAR
  ENDSCAN 
ENDIF 

*!*	LOCATE FOR &lcRpExp
SELECT(lcTmplbl)
LOCATE 
IF EOF() 
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF 
  
*!*	ELSE
*!*	  SCAN FOR &lcRpExp
*!*	    =lfGetWareh()
*!*	    m.Desc = HDRL1
*!*	    m.Add1 = HDRL2
*!*	    m.Add2 = HDRL3
*!*	    m.Add3 = HDRL4
*!*	    m.Phone = HDRL5
*!*	    m.Piktkt = PikTkt
*!*	    m.Account = Account
*!*	    m.Store = Store
*!*	    m.Order = Order
*!*	    m.Labels = lnLabelNum
*!*	    m.Date = Date
*!*	    m.Flag = Flag
*!*	    m.PrtFlag = PrtFlag
*!*	    INSERT INTO (lcTmplbl) FROM MEMVAR
*!*	  ENDSCAN
*!*	ENDIF
**************************************************************************
* TEMP FILE HAS BEEN SELECTED , PREPARE TO PRINT THE LABELS
**************************************************************************
SELECT &lcTmplbl
SET INDEX TO 
*SET RELATION TO "O"+ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER

*llMultiWH = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)) = 'Y', .T. , .F.)

LINEUP = .T.
R_WIDTH = 'N'

lcChoice = 1
SELECT &lcTmplbl
*--------------------------
* PRINT LABEL LINEUP
*--------------------------
DO WHILE LINEUP
  SELECT &lcTmplbl
  LOCATE
  XACCOUNT = ACCOUNT
  XSTORE   = STORE
  SELECT &lcTmplbl
  *Message 'Would you like to test the lineup ? Y/N/Q => ','YNQ'
  lcChoice = gfModalGen('TRM32093B34015','DIALOG' )
  IF lcChoice = 3
    EXIT
  ENDIF
  IF lcChoice = 2
    LINEUP=.F.
    EXIT
  ENDIF
  loDBFORdhdr.Seek("O"+ORDER,'ORDHDR')
  XDIVI_DATA = gfCodDes(ORDHDR.CDivision , 'CDIVISION ')
  HDRL1 = IIF(!EMPTY(XDIVI_DATA), XDIVI_DATA, HDRL1)

  SELECT &lcTmplbl
  =lfGetWareh()
  SET CONSOLE OFF
  =gfDispRe(lcRpName,'LABELS # 0',.F.,'L',.T.)
  SET CONSOLE ON
ENDDO

*--------------------------
* PRINT LABEL SETS
*--------------------------
SELECT &lcTmplbl
LOCATE

DO WHILE INKEY() <> 32   AND lcChoice <> 3

  SELECT &lcTmplbl
  IF EOF()
    EXIT
  ENDIF
  IF !loDBFORdhdr.Seek("O"+ORDER,'ORDHDR')
    SELECT &lcTmplbl
    SKIP
    LOOP
  ENDIF

  SELECT &lcTmplbl
  XACCOUNT = ACCOUNT
  XSTORE   = STORE
  XLBLCNT  = LABELS
  XPIKTKT  = PIKTKT
  XORDER   = ORDHDR.ORDER

  SET DEVICE TO PRINTER
  PRIVATE lnLabl
  lnLabl = XLBLCNT
  IF OARIAAPPLICATION.GCDEVICE == 'SCREEN'
    XLBLCNT = 1
  ENDIF
  SELECT &lcTmplbl
  DO WHILE XLBLCNT > 0
    SET CONSOLE OFF
    =gfDispRe(lcRpName,'LABELS # 0',.F.,'L',.T.)
    GO BOTTOM
    SET CONSOLE ON
    XLBLCNT = XLBLCNT-1
  ENDDO
  XLBLCNT = lnLabl
  SELECT &lcTmplbl
  SKIP
ENDDO

SET DEVICE TO SCREEN
RETURN

*:***************************************************************************
*: Name        : lfCretTemp
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date         : 07/30/2001
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfCretTemp()
*:***************************************************************************
*
FUNCTION lfCretTemp
DIMENSION laFStru[21,4]
*-- Create the structure of lcTmplbl file
laFStru[1,1] = 'PikTkt'
laFStru[1,2] = 'C'
laFStru[1,3] = 6
laFStru[1,4] = 0
laFStru[2,1] = 'Account'
laFStru[2,2] = 'C'
laFStru[2,3] = 5
laFStru[2,4] = 0
laFStru[3,1] = 'Store'
laFStru[3,2] = 'C'
laFStru[3,3] = 8
laFStru[3,4] = 0
laFStru[4,1] = 'Order'
laFStru[4,2] = 'C'
laFStru[4,3] = 6
laFStru[4,4] = 0
laFStru[5,1] = 'Labels'
laFStru[5,2] = 'N'
laFStru[5,3] = 4
laFStru[5,4] = 0
laFStru[6,1] = 'Date'
laFStru[6,2] = 'D'
laFStru[6,3] = 10
laFStru[6,4] = 0
laFStru[7,1] = 'Flag'
laFStru[7,2] = 'C'
laFStru[7,3] = 1
laFStru[7,4] = 0
laFStru[8,1] = 'PrtFlag'
laFStru[8,2] = 'C'
laFStru[8,3] = 1
laFStru[8,4] = 0
laFStru[9,1] = 'Form_No'
laFStru[9,2] = 'N'
laFStru[9,3] = 4
laFStru[9,4] = 0
*********
laFStru[10,1] = 'Desc'
laFStru[10,2] = 'C'
laFStru[10,3] = 35
laFStru[10,4] = 0
laFStru[11,1] = 'Add1'
laFStru[11,2] = 'C'
laFStru[11,3] = 30
laFStru[11,4] = 0
laFStru[12,1] = 'Add2'
laFStru[12,2] = 'C'
laFStru[12,3] = 30
laFStru[12,4] = 0
laFStru[13,1] = 'Add3'
laFStru[13,2] = 'C'
laFStru[13,3] = 92
laFStru[13,4] = 0
laFStru[14,1] = 'Phone'
laFStru[14,2] = 'C'
laFStru[14,3] = 50
laFStru[14,4] = 0

laFStru[15,1] = 'Cust_CAdd1'
laFStru[15,2] = 'C'
laFStru[15,3] = 30
laFStru[15,4] = 0

laFStru[16,1] = 'Cust_CAdd2'
laFStru[16,2] = 'C'
laFStru[16,3] = 30
laFStru[16,4] = 0

laFStru[17,1] = 'Cust_CAdd3'
laFStru[17,2] = 'C'
laFStru[17,3] = 30
laFStru[17,4] = 0

laFStru[18,1] = 'Cust_CAdd4'
laFStru[18,2] = 'C'
laFStru[18,3] = 30
laFStru[18,4] = 0

laFStru[19,1] = 'Cust_CAdd5'
laFStru[19,2] = 'C'
laFStru[19,3] = 30
laFStru[19,4] = 0

laFStru[20,1] = 'Cust_Dba'
laFStru[20,2] = 'C'
laFStru[20,3] = 30
laFStru[20,4] = 0

laFStru[21,1] = 'Cust_Stname'
laFStru[21,2] = 'C'
laFStru[21,3] = 30
laFStru[21,4] = 0

*********
 = gfCrtTmp(lcTmplbl,@laFStru,"PIKTKT+ALLTRIM(STR(FORM_NO))" ,lcTmplbl,.T.)
*!*	CREATE DBF (gcWorkDir+lcTmplbl) FROM ARRAY laFStru
*!*	=gfOpenFile(gcWorkDir+'&lcTmplbl','','EX')

*!*	INDEX ON PIKTKT+ALLTRIM(STR(FORM_NO)) TAG (lcTmplbl)
*!*	SET ORDER TO TAG (lcTmplbl)

*:*************************************************************
*: Name      : lfGetWareh
*: DATE      : 07/30/2001
*: Developer : Ahmed Abdel Naby (AAN)
*: Purpose   : To get the warehouse information.
*:*************************************************************
*: Example   :  lfGetWareh()
*:*************************************************************
FUNCTION lfGetWareh
PRIVATE lcAlias
lcAlias = SELECT(0)
*IF llMultiWH AND SEEK(&lcTmplbl..PIKTKT,'PIKTKT') AND SEEK (PIKTKT.cWareCode,'WAREHOUS')
IF llMultiWH AND loDBFWarehous.SEEK(PIKTKT.cWareCode,'WAREHOUS')
  HDRL1    = ALLTRIM(WareHous.cDesc)
  HDRL2   = ALLTRIM(WareHous.cAddress1)
  HDRL3   = ALLTRIM(WareHous.cAddress2)
  HDRL4    = ALLTRIM(WareHous.cAddress3) + ' ' + ALLTRIM(WareHous.cAddress4) + ' ' + ALLTRIM(WareHous.cAddress5)
  HDRL5 = "TEL#: " +ALLTRIM(WareHous.cphone)+SPACE(2)+"FAX#: "+ALLTRIM(WareHous.cfax)
ELSE
  SELECT SYCCOMP
  SEEK oAriaApplication.ActiveCompanyID
  lcCompName = cCom_Name
  lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
  lcCompFax = cCom_Fax
  * Get the company addresses
  laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)

  HDRL1=cCom_Name
  HDRL2=laCompAdd[1]
  HDRL3=laCompAdd[2]
  HDRL4=TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5]
  HDRL5 = "Tel#: " +ALLTRIM(lcCompPhon)+SPACE(2)+"Fax#: "+ALLTRIM(lcCompFax)
ENDIF
IF LEN(TRIM(HDRL3))=0
  HDRL3=HDRL4
  HDRL4=HDRL5
  HDRL5=' '
ENDIF
SELECT (lcAlias)
*!*************************************************************
*! Name      : lfvOk1
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 07/30/2001
*! Purpose   : Function to Close a default data screen
*!*************************************************************
*! Example   : lfvOk1()
*!*************************************************************
FUNCTION lfvOk1
FOR lcCNt=1 TO 5
  IF TYPE('xWk[lcCnt]')<>"U" OR TYPE('xPo[lcCnt]')<>"U" OR TYPE('xStyle[lcCnt]')<>"U" OR TYPE('xPcs[lcCnt]')<>"U"
    llData = .T.
  ENDIF
ENDFOR
CLEAR READ
*-- End of lfvOk.
*!*************************************************************
*: Name      : lfvScreen
*: Developer : Ahmed Abdel Naby (AAN)
*: Date      : 07/30/2001
*: Purpose   : Function to open a default data screen
*!*************************************************************
*! Example   : lfvScreen()
*!*************************************************************
FUNCTION lfvScreen

IF llData = .F.
  DECLARE xWk[5],xPo[5],xStyle[5],xPcs[5]
  STORE "" TO xWk,xPo,xStyle,xPcs
ENDIF
*DO gcScrDir+"SO\SOSJB100.SPR"
DIMENSION laDefaArray[20]
DO FORM oAriaApplication.ReportHome+"SO\SOSJB100.SCX"  WITH xWk,xPo,xStyle,xPcs

*-- End of lfvScreen.

*!*************************************************************
*: Name      : lfvLblNo
*: Developer : Ahmed Abdel Naby (AAN)
*: Date      : 07/30/2001
*: Purpose   : Function to open a default data screen
*!*************************************************************
*! Example   : lfvLblNo()
*!*************************************************************
FUNCTION lfvLblNo
IF lnLabelNum <1
  lnLabelNum = 1
ENDIF
*-- End of lfvLblNo.

*!*************************************************************
*: Name      : lfDefaValue
*: Developer : MAriam Mazhar(MMT)
*: Date      : 08/16/2006
*: Purpose   : Function to save default values in array
*!*************************************************************
FUNCTION lfDefaValue
PARAMETERS loFormSet 

xWk[1]  = loFormSet.ariaform1.Ariatextbox1.Value 
xWk[2]  = loFormSet.ariaform1.Ariatextbox4.Value 
xWk[3]  = loFormSet.ariaform1.Ariatextbox2.Value 
xWk[4]  = loFormSet.ariaform1.Ariatextbox3.Value 
xWk[5]  = loFormSet.ariaform1.Ariatextbox17.Value 
xPo[1]  = loFormSet.ariaform1.Ariatextbox6.Value 
xPo[2]  = loFormSet.ariaform1.Ariatextbox5.Value 
xPo[3]  = loFormSet.ariaform1.Ariatextbox7.Value 
xPo[4]  = loFormSet.ariaform1.Ariatextbox8.Value 
xPo[5]  = loFormSet.ariaform1.Ariatextbox18.Value 
xStyle[1] = loFormSet.ariaform1.Ariatextbox10.Value 
xStyle[2] = loFormSet.ariaform1.Ariatextbox9.Value 
xStyle[3] = loFormSet.ariaform1.Ariatextbox11.Value 
xStyle[4] = loFormSet.ariaform1.Ariatextbox12.Value 
xStyle[5] = loFormSet.ariaform1.Ariatextbox19.Value 
xPcs[1] = loFormSet.ariaform1.Ariatextbox16.Value 
xPcs[2] = loFormSet.ariaform1.Ariatextbox15.Value 
xPcs[3] = loFormSet.ariaform1.Ariatextbox14.Value 
xPcs[4] = loFormSet.ariaform1.Ariatextbox13.Value 
xPcs[5] = loFormSet.ariaform1.Ariatextbox20.Value 

FOR lcCNt=1 TO 5
  IF TYPE('xWk[lcCnt]')<>"U" OR TYPE('xPo[lcCnt]')<>"U" OR TYPE('xStyle[lcCnt]')<>"U" OR TYPE('xPcs[lcCnt]')<>"U"
    llData = .T.
  ENDIF
ENDFOR

*!*************************************************************
*: Name      : lfwOGWhen
*: Developer : MAriam Mazhar(MMT)
*: Date      : 08/21/2006
*: Purpose   : Function when
*!*************************************************************
FUNCTION lfwOGWhen
loDBFPiktkt    = CreateObject("RemoteTable","Piktkt","Piktkt",'Piktkt',SET("DATASESSION"))&&,"",.T.)
loDBFCustomer  = CreateObject("RemoteTable",'Customer','Customer','Customer',SET("DATASESSION"))
loDBFORdhdr    = CreateObject("RemoteTable","ORdhdr","ORdhdr",'ORdhdr',SET("DATASESSION"))
loDBFWarehous  = CreateObject("RemoteTable","Warehous","Warehous",'Warehous',SET("DATASESSION"))