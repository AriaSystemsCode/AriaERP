*:****************************************************************
*: Program file  : ALPKTKPP.PRG [Custom Pick Ticket for Appres For 
*: Program desc. : Generate Inventory Declaration file.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Sales Order Allocation  (AL)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 07/17/2003
*: Tracking Job Number : C#102841,1
*:****************************************************************
*: Calls         : 
*:               : FUNCTIONS  :  
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#102841,1.
*:****************************************************************
*:Modifications  :
*:C129311,1 EIH 09/18/2005 Add Style Price in Picking Ticket Form 
*:****************************************************************
*:

lcMainTemp = gfTempName()
lcRpTmpBmp = gfTempName()
lcDuns     = gfGetMemVar('XDUNS')

*-- Function To create Temp File to print BMp at the Form.
= lfCreatBmp ()
*-- Function To create Temp File to print from
=lfCreatTmp()
*-- Function To Collect Data
=lfCollect()

DIMENSION laScalAr[12] , laScalAr2[12]
STORE SPACE(0) TO laScalAr , laScalAr2

*-- Select the new temp file to print from
SELECT (lcMainTemp)
SET ORDER TO TAG (lcTmpOrdH) IN &lcTmpOrdH

SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE

*--IF We are to Print Order Lines Note Pad
IF llRpOrdLNt
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
ENDIF

SET RELATION TO Style INTO STYLE ADDITIVE
SELECT PIKTKT
SET RELATION TO cWareCode INTO WAREHOUS
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SELECT (lcMainTemp)
SET ORDER TO (lcMainTemp)

LOCATE

*-- End OF Code.
*:**************************************************************************
*: Name      : lfCollect
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/17/2003
*: Purpose   : Collect Data to Print
*:**************************************************************************
*: Calls     : 
*:**************************************************************************
*: Parameters: None
*:**************************************************************************
*: Returns   :  None.
*:**************************************************************************
*: Example   :  =lfCollect()
*:**************************************************************************
*:
FUNCTION lfCollect

PRIVATE lcAliasSc , lcKeyOrdrS , lcAlias2T
lcMajorPic  = gfItemMask('PM')
lnMajorLen  = LEN(lcMajorPic)

SELECT (lcTmpOrdL)
LOCATE

SCAN
  IF SEEK(PikTkt + Order + cGrupDetal + SUBSTR(STYLE,1,lnMajorLen+7), lcMainTemp) 

    lcAlias2T = SELECT(0)
    SELECT SCALE
    lcKyOrdr2T = EVAL(KEY()) 
    lnQty = 0
    IF SEEK("S" + LEFT(&lcTmpOrdL..SCALE,1) , 'SCALE')
      SCAN FOR TYPE + SCALE + PREPAK = "S" + LEFT(&lcTmpOrdL..SCALE,1)
        IF SCALE.SCALE = &lcTmpOrdL..SCALE
          lnQty = lnQty
          EXIT
        ELSE
          lnQty = lnQty + Scale.Cnt
        ENDIF
      ENDSCAN
    ENDIF
   =SEEK(lcKyOrdr2T)
   SELECT(lcAlias2T)
    =SEEK('S'+Scale,'SCALE') 
    FOR lnI = 1 TO Scale.Cnt
      lcI = STR(lnI,1)
      lnQty = lnQty + 1
      IF lnQty > 11
        EXIT
      ENDIF
      lcQty = STR(lnQty,LEN(ALLTRIM(STR(lnQty))))
      REPLACE &lcMainTemp..Pik&lcQty WITH &lcMainTemp..Pik&lcQty + PIK&lcI
    ENDFOR  
    REPLACE &lcMainTemp..TotPik WITH &lcMainTemp..TotPik + TotPIk,;
            &lcMainTemp..BO     WITH &lcMainTemp..BO + (TotQTY - TotPIk)
  ELSE
    STORE 0 TO m.Pik1,m.Pik2,m.Pik3,m.Pik4,m.Pik5,m.Pik6,m.Pik7,m.Pik8,m.Pik9,m.Pik10,m.Pik11
    SCATTER MEMVAR MEMO
    m.Account   = OrdHdr.Account
    m.Rep1      = OrdHdr.Rep1
    m.Rep2      = OrdHdr.Rep2
    
    llInvoice   = SEEK(m.Order,'INVLINE') AND SEEK(INVLINE.Invoice,'INVHDR')
    m.ShipDate  = IIF(llInvoice,InvHdr.ShipDate,{})
    m.OrdDate   = OrdHdr.Start
    m.cDivision = OrdHdr.cdivision
    m.cTermCode = OrdHdr.ctermcode 
    m.Dept      = OrdHdr.Dept
    m.ShipVia   = IIF(ORDHDR.ShipVia ='*',CUSTOMER.ShipVia,ORDHDR.ShipVia)
    m.Date      = Piktkt.Date
    m.Approval  = OrdHdr.Approval
    m.Desc      = Style.Desc1
    m.Style = SUBSTR(STYLE,1,lnMajorLen)
    m.COLOR = SUBSTR(STYLE,lnMajorLen+2,6)
    m.StyClr = SUBSTR(STYLE,1,lnMajorLen+7)
    =SEEK('S'+Scale,'SCALE')
    m.Sacle = PADL(SCALE.Scale,2)
    
    *C129311,1 EIH 09/18/2005 Set the variable LSHWPRICE with its related field in customer file [Begin]
    m.LSHWPRICE = CUSTOMER.LSHWPRICE
    *C129311,1 EIH 09/18/2005 [End]
    
    STORE SPACE(0) TO m.Sz1,m.Sz2,m.Sz3,m.Sz4,m.Sz5,m.Sz6,m.Sz7,m.Sz8,m.Sz9,m.Sz10,m.Sz11


    lnQty = Scale.Cnt
    m.BO = TotQTY - TotPIk
    lcAliasSc = SELECT(0)
    SELECT SCALE
    lcKeyOrdrS = EVAL(KEY()) 
    =SEEK("S" + LEFT(M.SCALE,1))
    lnL = 0
    STORE 0 TO lnPosPik , lnContSc , lnPosScl
    STORE .T. TO llEnterAp
    SCAN FOR TYPE + SCALE + PREPAK = "S" + LEFT(M.SCALE,1)
      IF llEnterAp
        IF SCALE.SCALE = M.SCALE
          lnPosPik = lnPosPik + 1
          lnContSc = lnContSc + Scale.Cnt
          lnPosScl = Scale.Cnt
          llEnterAp = .F.
        ELSE
          lnPosPik = lnPosPik + 1
          lnContSc = lnContSc + Scale.Cnt
        ENDIF
      ENDIF

      FOR I = 1 TO 8
        lcSize  = 'SZ' + ALLTRIM(STR(I))
        IF !EMPTY(SCALE.&lcSize)
          lnL = lnL + 1
          lcMSiz = 'SZ' + ALLTRIM(STR(lnL))
          m.&lcMSiz = SCALE.&lcSize
        ENDIF
      ENDFOR
    ENDSCAN
    IF lnPosPik > 1
      FOR lnI = 1 TO lnPosScl
        lnLoop = ALLTRIM(STR(lnI))
        lnTot  = lnI + (lnContSc - lnPosScl)
        ln2nd = ALLTRIM(STR(lnTot))
        STORE M.PIK&lnLoop TO M.PIK&ln2nd
      ENDFOR

      FOR lnE = 1 TO (lnContSc - lnPosScl)
        lnEmpty = ALLTRIM(STR(lnE))
        STORE 0 TO M.PIK&lnEmpty
      ENDFOR
      
    ENDIF

    =SEEK(lcKeyOrdrS)
    SELECT(lcAliasSc)
    INSERT INTO (lcMainTemp) FROM MEMVAR
  ENDIF
ENDSCAN
*-- End OF lfCollect.
*:**************************************************************************
*: Name      : lfCreatTmp
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/17/2003
*: Purpose   : Create Temp File
*:**************************************************************************
*: Calls     : 
*:**************************************************************************
*: Parameters: None
*:**************************************************************************
*: Returns   :  None.
*:**************************************************************************
*: Example   :  =lfCreatTmp()
*:**************************************************************************
*:
FUNCTION lfCreatTmp


=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
=gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'Invlineo','SH')

lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PikTkt'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Order'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'LineNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Account'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Store'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Style'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 12
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Color'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Desc'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 60
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'ShipVia'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'ShipDate'
laFileStru[lnI,2] = 'D'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdDate'
laFileStru[lnI,2] = 'D'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Date'
laFileStru[lnI,2] = 'D'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cDivision'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cTermCode'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CustPo'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 15
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Rep1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Rep2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Dept'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Approval'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik9'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik10'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik11'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotPik'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'BO'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Sz1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Sz2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Sz3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Sz4'
laFileStru[lnI,2] = 'c'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Sz5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Sz6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Sz7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Sz8'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Sz9'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Sz10'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Sz11'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Scale'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 2
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cGrupDetal'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'StyClr'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 15
laFileStru[lnI,4] = 0

*C129311,1 EIH 09/18/2005 Add the price and LSHWPRICE fields in lcMainTemp file [Begin]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'price'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 12
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'LSHWPRICE'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

*C129311,1 EIH 09/18/2005 [End]

CREATE CURSOR (lcMainTemp) FROM ARRAY laFileStru
ZAP
INDEX ON PikTkt + Order + cGrupDetal + StyClr  TAG (lcMainTemp)

*-- End Of lfCreatTmp
*:*************************************************************
*: Name      : lfPrt2ndS
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/17/2003
*: Purpose   : Function to print the 2nd scale.
*:*************************************************************
*: Called from : ARPINVAP.FRX
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : =lfPrtLNotes()
*:*************************************************************
*:
FUNCTION lfPrt2ndS
PARAMETER lcReturn
PRIVATE lcAlias , lcKeyAp

lcReturn = .T.
lcAlias = SELECT(0)
STORE SPACE(0) TO laScalAr , laScalAr2

*-- Call this function to get the address.
*= lfSolSpAdr ()
SELECT(lcAlias)

SELECT(lcMainTemp)
lcKeyAp = EVAL(KEY())
lcPktktAr = &lcMainTemp..PIKTKT

lcScaleAr = LEFT(&lcMainTemp..SCALE,1)

=SEEK(lcPktktAr)
lnLoop2 = 0

SCAN REST WHILE PIKTKT = lcPktktAr
  IF !(LEFT(&lcMainTemp..SCALE,1) $ lcScaleAr)
    lcScaleAr = lcScaleAr + LEFT(&lcMainTemp..SCALE,1)

    lnLoop2 = lnLoop2 + 1
    IF lnLoop2 = 1
      laScalAr[1] = LEFT(EVAL(lcMainTemp+'.SCALE'),1)
    ELSE
      laScalAr2[1] = LEFT(EVAL(lcMainTemp+'.SCALE'),1)
    ENDIF

    FOR I = 1 TO 11
      lcSize  = 'SZ' + ALLTRIM(STR(I))
      IF lnLoop2 = 1
        laScalAr[I+1] = &lcMainTemp..&lcSize
      ELSE
        laScalAr2[I+1] = &lcMainTemp..&lcSize
      ENDIF
    ENDFOR
    IF lnLoop2 = 2
      EXIT
    ENDIF
  ENDIF
ENDSCAN

=SEEK(lcKeyAp)
SELECT(lcAlias)
RETURN ""

*--End of lfPrt2ndS.
*:*************************************************************
*: Name      : lfSolTSpAd
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/17/2003
*: Purpose   : Function to Get the Sold to Address & Ship to Address
*:             & the Description of the Ship Via , Season ,
*:             Special Instructions , Terms
*:*************************************************************
*: Called from : ALPKTKTA.FRX
*:*************************************************************
*: Calls       : gfRltFld() , gfCodDes() , gfGetAdr() , lfAdrShift()
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : ''
*:*************************************************************
*
FUNCTION lfSolTSpAd
PARAMETER lcReturn
PRIVATE lcDistCntr , lnPrvAlias

lnPrvAlias = SELECT (0)

llEndGroup = .F.
=gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')

SELECT CUSTOMER
SEEK IIF(EMPTY(PIKTKT.STORE),'M','S')+ PIKTKT.Account + PIKTKT.STORE 
lcShipVia = gfCodDes(IIF(CUSTOMER.nBrkWeight <> 0 .AND.;
                         &lcTmpOrdH..nWeight > CUSTOMER.nBrkWeight ,;
                         CUSTOMER.cAltShpvia ,IIF(ORDHDR.ShipVia ='*',CUSTOMER.ShipVia,ORDHDR.ShipVia)), 'SHIPVIA')

lcSeason = gfCodDes(ORDHDR.Season , 'SEASON')
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST')
lcTerms = gfCodDes(ORDHDR.cTermCode , 'CTERMCODE')

SELECT CUSTOMER
SEEK IIF(EMPTY(PIKTKT.Store) , 'M' , 'S') + PIKTKT.Account + PIKTKT.Store

lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')

*-- Sold To Phone#

laSoldTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2'))+ ' Phone# '+ TRANSFORM(Customer.Phone1 , lcPhonPict)
=lfAdrShift('laSoldTo')

IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
  
ELSE    && Else
  
  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  
  *-- If there is a distribution center
  IF !EMPTY(lcDistCntr)
    SEEK 'S' + PIKTKT.Account + lcDistCntr
  ENDIF
  
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(Customer.Phone1 , lcPhonPict)

ENDIF    && End of IF

=lfAdrShift('laShipTo')
lcReturn = .T.

SELECT (lnPrvAlias)

RETURN ''

*-- End OF lfSolTSpAd
*:*************************************************************
*: Name      : lfCreatBmp
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/17/2003
*: Purpose   : Create cursor to save bmps files.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : lfCreatBmp()
*:*************************************************************
*
FUNCTION lfCreatBmp


CREATE CURSOR (lcRpTmpBmp) (gBmp1 G,gBmp2 G)
SELECT (lcRpTmpBmp)
APPEND BLANK
APPEND GENERAL gBmp1 FROM (gcBmpHome + "001.BMP")
APPEND GENERAL gBmp2 FROM (gcBmpHome + "002.BMP")


*--end of lfCreatBmp.
*:*************************************************************
