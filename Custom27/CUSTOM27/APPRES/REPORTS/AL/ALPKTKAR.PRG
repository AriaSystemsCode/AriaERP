*:**************************************************************************
*: Procedure file: ALPKTKAR.PRG   Custom Pik Ticket for Appres For C#102406
*:         System: ARIA 2.7
*:         Module: Allocation
*:         Author: HEND GHANEM (HBG)
*:      Copyright (c) 
*:  Last modified: 10/09/2001
*:**************************************************************************
*: Modifications :
***************************************************************************
*: B605100,1 BWA 11/07/2001 Fix the bug of missing fields and alignment.[FRX]
*: B605100,4 BWA 11/11/2001 Add new scale size line.
*: B605139,1 BWA 11/19/2001 Fix the bug of the total pik field accumulates the values.[FIX IN FRX]
*: B605172,1 BWA 11/28/2001 Fix the bugs in the printing of the scale size in the header.
*: B605178,1 BWA 12/02/2001 Fix the bug of the picked quantities are printed under wrong sizes.
*: B605216,1 BWA 12/09/2001 Fix the bug of the picked quantities are printed under wrong sizes.
***************************************************************************


*-- Function To create Temp File to print from
=lfCreatTmp()
*-- Function To Collect Data
=lfCollect()

*B605100,4 BWA 11/07/2001 Add new scale size line.[START]
DIMENSION laScalAr[12] , laScalAr2[12]
STORE SPACE(0) TO laScalAr , laScalAr2
*B605100,4 BWA 11/07/2001.[END]

*-- Select the new temp file to print from
SELECT (lcTempFile)
SET ORDER TO TAG (lcTmpOrdH) IN &lcTmpOrdH

SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE

*--IF We are to Print Order Lines Note Pad
IF llRpOrdLNt
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
ENDIF

SET RELATION TO Style INTO STYLE ADDITIVE
*SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SELECT PIKTKT
SET RELATION TO cWareCode INTO WAREHOUS
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SELECT (lcTempFile)
SET ORDER TO (lcTempFile)
LOCATE

*!**************************************************************************
*! Name      : lfCollect
*! Developer : HEND GHANEM (HBG)
*! Date      : 09/10/2001
*! Purpose   : Collect Data to Print
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfCollect()
*!**************************************************************************
FUNCTION lfCollect

PRIVATE lcAliasSc , lcKeyOrdrS , lcAlias2T
lcMajorPic  = gfItemMask('PM')
lnMajorLen  = LEN(lcMajorPic)

SELECT (lcTmpOrdL)
LOCATE

SCAN
  IF SEEK(PikTkt + Order + cGrupDetal + SUBSTR(STYLE,1,lnMajorLen+7), lcTempFile) 

    *B605216,1 BWA 12/09/2001 Fix the bug of the picked quantities are printed under wrong sizes.[START]
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
   *B605216,1 BWA 12/09/2001.[END]

    =SEEK('S'+Scale,'SCALE') 
    FOR lnI = 1 TO Scale.Cnt
      lcI = STR(lnI,1)
      lnQty = lnQty + 1
      IF lnQty > 11
        EXIT
      ENDIF
      lcQty = STR(lnQty,LEN(ALLTRIM(STR(lnQty))))

      *B605172,1 BWA 11/28/2001 Fix the bugs in the printing of the scale size in the header.[START]
      *REPLACE &lcTempFile..Pik&lcQty WITH PIK&lcI,;
               &lcTempFile..Sz&lcQty  WITH SCALE.Sz&lcI

      *B605216,1 BWA 12/09/2001 Fix the bug of the picked quantities are printed under wrong sizes.[START]
      *REPLACE &lcTempFile..Pik&lcQty WITH PIK&lcI
      REPLACE &lcTempFile..Pik&lcQty WITH &lcTempFile..Pik&lcQty + PIK&lcI
      *B605216,1 BWA 12/09/2001.[END]

      *B605172,1 BWA 11/28/2001.[END]

    ENDFOR  
    REPLACE &lcTempFile..TotPik WITH &lcTempFile..TotPik + TotPIk,;
            &lcTempFile..BO     WITH &lcTempFile..BO + (TotQTY - TotPIk)
  ELSE
    *B605172,1 BWA 11/28/2001 Fix the bugs in the printing of the scale size in the header.[START]
    STORE 0 TO m.Pik1,m.Pik2,m.Pik3,m.Pik4,m.Pik5,m.Pik6,m.Pik7,m.Pik8,m.Pik9,m.Pik10,m.Pik11
    *B605172,1 BWA 11/28/2001.[END]
    
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

    *B605100,4 BWA 11/07/2001 Add new scale size line.[START]
    STORE SPACE(0) TO m.Sz1,m.Sz2,m.Sz3,m.Sz4,m.Sz5,m.Sz6,m.Sz7,m.Sz8,m.Sz9,m.Sz10,m.Sz11
    *B605100,4 BWA 11/07/2001.[END]


    lnQty = Scale.Cnt
    *B605172,1 BWA 11/28/2001 Fix the bugs in the printing of the scale size in the header.[START]
    *FOR lnI = 1 TO Scale.Cnt
    *  lcI = STR(lnI,1)
    *  m.Sz&lcI = SCALE.Sz&lcI
    *ENDFOR
    *B605172,1 BWA 11/28/2001.[END]

    m.BO = TotQTY - TotPIk

    *B605172,1 BWA 11/28/2001 Fix the bugs in the printing of the scale size in the header.[START]
    lcAliasSc = SELECT(0)
    SELECT SCALE
    lcKeyOrdrS = EVAL(KEY()) 
    =SEEK("S" + LEFT(M.SCALE,1))
    lnL = 0

    *B605178,1 BWA 12/02/2001 Fix the bug of the picked quantities are printed under wrong sizes.[START]
    STORE 0 TO lnPosPik , lnContSc , lnPosScl
    STORE .T. TO llEnterAp
    *B605178,1 BWA 12/02/2001.[END]

    SCAN FOR TYPE + SCALE + PREPAK = "S" + LEFT(M.SCALE,1)
      *B605178,1 BWA 12/02/2001 Fix the bug of the picked quantities are printed under wrong sizes.[START]
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
      *B605178,1 BWA 12/02/2001.[END]

      FOR I = 1 TO 8
        lcSize  = 'SZ' + ALLTRIM(STR(I))
        IF !EMPTY(SCALE.&lcSize)
          lnL = lnL + 1
          lcMSiz = 'SZ' + ALLTRIM(STR(lnL))
          m.&lcMSiz = SCALE.&lcSize
        ENDIF
      ENDFOR
    ENDSCAN

    *B605178,1 BWA 12/02/2001 Fix the bug of the picked quantities are printed under wrong sizes.[START]
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
    *B605178,1 BWA 12/02/2001.[END]

    =SEEK(lcKeyOrdrS)
    SELECT(lcAliasSc)
    *B605172,1 BWA 11/28/2001.[END]

    INSERT INTO (lcTempFile) FROM MEMVAR
  ENDIF
ENDSCAN

*!**************************************************************************
*! Name      : lfCreatTmp
*! Developer : HEND GHANEM (HBG)
*! Date      : 09/10/2001
*! Purpose   : Create Temp File
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfCreatTmp()
*!**************************************************************************
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

CREATE CURSOR (lcTempFile) FROM ARRAY laFileStru
ZAP
INDEX ON PikTkt + Order + cGrupDetal + StyClr  TAG (lcTempFile)

*B605100,4 BWA 11/07/2001 Add new scale size line.[START]
*!*************************************************************
*! Name      : lfPrt2ndS
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 11/11/2001
*! Purpose   : Function to print the 2nd scale.
*!*************************************************************
*! Called from : ARPINVAP.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrtLNotes()
*!*************************************************************
FUNCTION lfPrt2ndS
PARAMETER lcReturn
PRIVATE lcAlias , lcKeyAp

lcReturn = .T.
lcAlias = SELECT(0)
STORE SPACE(0) TO laScalAr , laScalAr2

SELECT(lcTempFile)
lcKeyAp = EVAL(KEY())
lcPktktAr = &lcTempFile..PIKTKT

lcScaleAr = LEFT(&lcTempFile..SCALE,1)

=SEEK(lcPktktAr)
lnLoop2 = 0

SCAN REST WHILE PIKTKT = lcPktktAr

  *B605172,1 BWA 11/28/2001 Fix the bugs in the printing of the scale size in the header.[START]
  *IF lcScaleAr # &lcTempFile..SCALE
  *  laScalAr[1] = LEFT(EVAL(lcTempFile+'.SCALE'),1)
  *  FOR I = 1 TO 11
  *    lcSize  = 'SZ' + ALLTRIM(STR(I))
  *    laScalAr[I+1] = &lcTempFile..&lcSize
  *  ENDFOR
  *  EXIT
  *ENDIF

  IF !(LEFT(&lcTempFile..SCALE,1) $ lcScaleAr)
    lcScaleAr = lcScaleAr + LEFT(&lcTempFile..SCALE,1)

    lnLoop2 = lnLoop2 + 1
    IF lnLoop2 = 1
      laScalAr[1] = LEFT(EVAL(lcTempFile+'.SCALE'),1)
    ELSE
      laScalAr2[1] = LEFT(EVAL(lcTempFile+'.SCALE'),1)
    ENDIF

    FOR I = 1 TO 11
      lcSize  = 'SZ' + ALLTRIM(STR(I))
      IF lnLoop2 = 1
        laScalAr[I+1] = &lcTempFile..&lcSize
      ELSE
        laScalAr2[I+1] = &lcTempFile..&lcSize
      ENDIF
    ENDFOR
    IF lnLoop2 = 2
      EXIT
    ENDIF
  ENDIF
  *B605172,1 BWA 11/28/2001.[END]

ENDSCAN

=SEEK(lcKeyAp)
SELECT(lcAlias)
RETURN ""

*--End of lfPrt2ndS.
*B605100,4 BWA 11/07/2001.[END]