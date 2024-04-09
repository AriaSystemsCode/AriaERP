*:***************************************************************************
*: Program file : SJB300.PRG
*: DESC : PRINT SHIPPING LABELS FROM ORDERS, PIKTKTS OR INVOICES (For SJB)
*: DATE : 08/05/2001
*: Developer: Ahmed Abdel Naby (AAN)
*: Reference: 102291
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:         PROCEDURE  : 
*:************************************************************************

PRIVATE HDRL1,HDRL2,HDRL3,HDRL4,HDRL5,llFlag,llftot,xtotqty,lnbox,lnCunt
STORE "" TO HDRL1,HDRL2,HDRL3,HDRL4,HDRL5
DECLARE laCompAdd[5]
STORE "" TO laCompAdd
STORE .T. TO llFlag,llftot
STORE 0 TO xtotqty,lnbox,lnCunt
lcTemp = gfTempName()
lcTmp = gfTempName()
SELECT PikTkt
SET RELATION TO "O"+ORDER INTO ORDHDR, IIF(EMPTY(STORE) ,;
'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER,;
"O"+ORDER + STORE INTO ORDLINE

=lfCretTemp()
SELECT PIKTKT
LOCATE FOR &lcRpExp

IF EOF() 
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ELSE
  SCAN FOR &lcRpExp
    m.Piktkt = PikTkt
    m.Account = Account
    m.Store = Store
    m.Order = Order    
    m.Date = Date
    m.Flag = Flag
    m.PrtFlag = PrtFlag
    lcPickTkt= PikTkt
    m.Labels = lfLabel()
    INSERT INTO (lcTmplbl) FROM MEMVAR
  ENDSCAN
ENDIF

**************************************************************************
* TEMP FILE HAS BEEN SELECTED , PREPARE TO PRINT THE LABELS
**************************************************************************
SELECT &lcTmplbl
SET INDEX TO
SET RELATION TO "O"+ORDER INTO ORDHDR,;
     IIF(STORE=SPACE(8) ,'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER ,;
     "O"+ORDER + STORE INTO ORDLINE

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

  LNNUM    = 0  
  SELECT &lcTmplbl

  XPIKTKT  = PIKTKT
  lcPickTkt= PikTkt
    
  XORDER   = ORDHDR.ORDER

  IF gcDevice <> 'S'
    *Message 'Would you like to test the lineup ? Y/N/Q => ','YNQ'
    lcChoice = gfModalGen('TRM32093B34015','DIALOG' )
    IF lcChoice = 3
      EXIT
    ENDIF

    IF lcChoice = 2
      LINEUP=.F.
      EXIT
    ENDIF
  ELSE  
    LINEUP=.F.
    EXIT
  ENDIF
  XDIVI_DATA = gfCodDes(ORDHDR.CDivision , 'CDIVISION ')
  HDRL1 = IIF(!EMPTY(XDIVI_DATA), XDIVI_DATA, HDRL1)

  SELECT &lcTmplbl
  SET CONSOLE OFF
  =gfDispRe(lcRpName,'LABELS # 0',.F.,'L')
  SET CONSOLE ON
ENDDO

*--------------------------
* PRINT LABEL SETS
*--------------------------
SELECT &lcTmplbl
LOCATE

WAIT WINDOW 'Labels printing - <Space Bar> to abort' NOWAIT

SELECT &lcTmplbl
lcPickTkt= PikTkt

SET DEVICE TO PRINT

XDIVI_DATA = gfCodDes(ORDHDR.CDivision , 'CDIVISION ')
HDRL1 = IIF(!EMPTY(XDIVI_DATA), XDIVI_DATA, HDRL1)

SELECT &lcTmplbl
COPY STRUCTURE TO (gcWorkDir+lcMainF) WITH CDX
USE (gcWorkDir+lcMainF) IN 0 ORDER 1
SCAN FOR !EOF('ORDHDR') .AND. LABELS # 0
  *-- Print Required number of labels
  FOR lnc=1 TO &lcTmplbl..LABELS
    SCATTER MEMVAR
    m.nLblsNo = lnc
    INSERT INTO (lcMainF) FROM MEMVAR
  ENDFOR
ENDSCAN
SET RELATION TO
SELECT (lcMainF)
SET RELATION TO 'O'+ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
                'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER

=gfDispRe(lcRpName,'LABELS # 0',.F.,'L')

IF USED(lcMainF)
  USE IN (lcMainF)
ENDIF

SET DEVICE TO SCREEN
RETURN

*:************************************************************************
*: Program file  : lfLABEL
*: Program desc. : Calculate no of labels for eash pick slip
*: Developer     : Ahmed Abdel Naby (AAN)
*:************************************************************************
FUNCTION LFLABEL
STORE 0 TO Lnlabel
STORE 0 TO Lncount
SELECT Ordline
lOrdSty = Order+Store
lcOldOrder = SET("ORDER")
SET ORDER TO OrdLinSt
*SELECT Ordline.order ,PikTkt,store, style,sum(totpik) AS totqty FROM ordline;
WHERE  cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+ lordsty; 
      AND totpik <> 0 ;
      AND PikTkt = lcPickTkt;
INTO DBF (gcWorkDir+lcTmp)
*INDEX ON style TAG (lcTmp)
SELECT Ordline.order ,PikTkt,store, style,totpik AS totqty FROM ordline;
WHERE  cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+ lordsty; 
      AND totpik <> 0 ;
      AND PikTkt = lcPickTkt;
INTO DBF (gcWorkDir+lcTmp)
INDEX ON style TAG (lcTmp)
SELECT (lcTemp)
APPEND FROM (gcWorkDir+lcTmp)
SELECT OrdLine
SET ORDER TO &lcOldORder

SELECT (lcTemp)
LOCATE
SCAN WHILE !EOF() FOR Order + PikTkt = OrdLine.Order + OrdLine.PikTkt
  lnlabel = CEILING(TotQty / 24)
  lncount = lnCount+lnLabel
ENDSCAN  

SELECT (lcTemp)
LOCATE

LOCATE
lnCunt = lnCunt + 1
RETURN lncount

*:************************************************************************
*: Program file  : lfstyle
*: Program desc. : Print  dept,po#,style#,pcs
*: Developer     : Ahmed Abdel Naby (AAN)
*:************************************************************************
FUNCTION lfStyle
PRIVATE lcAlias , lcReturn
lcAlias = ALIAS()

SELECT (lcTemp)
=SEEK(&lcMainF..Order + &lcMainF..PikTkt + '',(lcTemp))
REPLACE lOk WITH 'T'
IF llFlag            
  lnbox   = CEILING(TotQty/24)    
  IF TotQty <= 24
    xTotQty = TotQty
    llfTot  = .F.
  ELSE
    llfTot = .T.
    xTotQty = 24 
  ENDIF
ENDIF

xStyle  = SUBSTR(Style,1,7)
xColor  = SUBSTR(Style,14,3)

xCustpo = LEFT(IIF(OrdHdr.Multipo,Ordline.Custpo,OrdHdr.Custpo),10)
lnBox = lnBox - 1    
IF lnBox < 1
  IF llfTot 
    xTotQty = TotQty % 24
    IF xTotQty = 0
      xTotQty = 24 
    ENDIF    
  ENDIF
  IF !EOF()
    SKIP
  ENDIF
  llFlag = .T.
ELSE 
  llFlag = .F.
ENDIF

lcReturn = OrdHdr.Dept+'  '+xCustpo +'  '+xStyle+'  '+xColor+' '+PADR(STR(xTotQty,6),6)
SELECT (lcAlias)
RETURN lcReturn

*:***************************************************************************
*: Name        : lfCretTemp
*: Developer   : Ahmed Abdel Naby (AAN)
*: Purpose     : Create Temp. files
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfCretTemp()
*:***************************************************************************
*
FUNCTION lfCretTemp
DIMENSION laFStru[12,4]
DIMENSION laFStrc[6,4]
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
laFStru[10,1] = 'nFrom'
laFStru[10,2] = 'N'
laFStru[10,3] = 4
laFStru[10,4] = 0
laFStru[11,1] = 'nTotal'
laFStru[11,2] = 'N'
laFStru[11,3] = 4
laFStru[11,4] = 0
laFStru[12,1] = 'nLblsNo'
laFStru[12,2] = 'N'
laFStru[12,3] = 4
laFStru[12,4] = 0

CREATE DBF (gcWorkDir+lcTmplbl) FROM ARRAY laFStru
=gfOpenFile(gcWorkDir+'&lcTmplbl','','EX')

INDEX ON PIKTKT TAG (lcTmplbl)
SET ORDER TO TAG (lcTmplbl)

laFStrc[1,1] = 'Order'
laFStrc[1,2] = 'C'
laFStrc[1,3] = 6
laFStrc[1,4] = 0
laFStrc[2,1] = 'Store'
laFStrc[2,2] = 'C'
laFStrc[2,3] = 8
laFStrc[2,4] = 0
laFStrc[3,1] = 'Style'
laFStrc[3,2] = 'C'
laFStrc[3,3] = 19
laFStrc[3,4] = 0
laFStrc[4,1] = 'TotQty'
laFStrc[4,2] = 'N'
laFStrc[4,3] = 6
laFStrc[4,4] = 0
laFStrc[5,1] = 'PikTkt'
laFStrc[5,2] = 'C'
laFStrc[5,3] = 6
laFStrc[5,4] = 0
laFStrc[6,1] = 'lOk'
laFStrc[6,2] = 'C'
laFStrc[6,3] = 1
laFStrc[6,4] = 0

CREATE DBF (gcWorkDir+lcTemp) FROM ARRAY laFStrc
INDEX ON Order + PikTkt + lOk TAG (lcTemp)
=gfOpenFile(gcWorkDir+'&lcTemp',gcWorkDir+'&lcTemp','EX')

