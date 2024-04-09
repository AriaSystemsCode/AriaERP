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

SELECT PIKTKT
SET RELATION TO "O"+ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER
llMultiWH = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)) = 'Y', .T. , .F.)
=lfCretTemp()

SELECT PIKTKT
LOCATE FOR &lcRpExp

IF EOF() 
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ELSE
  SCAN FOR &lcRpExp
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
    INSERT INTO (lcTmplbl) FROM MEMVAR
  ENDSCAN
ENDIF
**************************************************************************
* TEMP FILE HAS BEEN SELECTED , PREPARE TO PRINT THE LABELS
**************************************************************************
SELECT &lcTmplbl
SET INDEX TO
SET RELATION TO "O"+ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER

*llMultiWH = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)) = 'Y', .T. , .F.)

LINEUP = .T.
R_WIDTH = 'N'


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
  XDIVI_DATA = gfCodDes(ORDHDR.CDivision , 'CDIVISION ')
  HDRL1 = IIF(!EMPTY(XDIVI_DATA), XDIVI_DATA, HDRL1)

  SELECT &lcTmplbl
  =lfGetWareh()
  SET CONSOLE OFF
  =gfDispRe(lcRpName,'LABELS # 0',.F.,'L')
  SET CONSOLE ON
ENDDO

*--------------------------
* PRINT LABEL SETS
*--------------------------
SELECT &lcTmplbl
LOCATE

DO WHILE INKEY() <> 32  

  SELECT &lcTmplbl
  IF EOF()
    EXIT
  ENDIF

  SELECT ORDHDR
  IF EOF()
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

  *=lfGetWareh()

  SET DEVICE TO PRINTER
  PRIVATE lnLabl
  lnLabl = XLBLCNT
  IF gcDevice == 'SCREEN'
    XLBLCNT = 1
  ENDIF
  SELECT &lcTmplbl
  DO WHILE XLBLCNT > 0
    SET CONSOLE OFF
    =gfDispRe(lcRpName,'LABELS # 0',.F.,'L')
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
*: Date        : 07/30/2001
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
DIMENSION laFStru[14,4]
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
*********
CREATE DBF (gcWorkDir+lcTmplbl) FROM ARRAY laFStru
=gfOpenFile(gcWorkDir+'&lcTmplbl','','EX')

INDEX ON PIKTKT+ALLTRIM(STR(FORM_NO)) TAG (lcTmplbl)
SET ORDER TO TAG (lcTmplbl)

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
IF llMultiWH AND SEEK (PIKTKT.cWareCode,'WAREHOUS')
  HDRL1    = ALLTRIM(WareHous.cDesc)
  HDRL2   = ALLTRIM(WareHous.cAddress1)
  HDRL3   = ALLTRIM(WareHous.cAddress2)
  HDRL4    = ALLTRIM(WareHous.cAddress3) + ' ' + ALLTRIM(WareHous.cAddress4) + ' ' + ALLTRIM(WareHous.cAddress5)
  HDRL5 = "TEL#: " +ALLTRIM(WareHous.cphone)+SPACE(2)+"FAX#: "+ALLTRIM(WareHous.cfax)
ELSE
  SELECT SYCCOMP
  SEEK gcAct_Comp
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
DO gcRepHome+"SO\SOSJB100.SPR"
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
