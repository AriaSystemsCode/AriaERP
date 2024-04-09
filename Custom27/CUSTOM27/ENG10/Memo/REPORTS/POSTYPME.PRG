*:***************************************************************************
*: Program file  : POSTYPME
*: Program desc. : Print PO for Memo Fashoins
*: For Report    : POSTYPME.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Developer     : AHMED MAHER (AMH)
*: Reference     : C102185,1
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: C200237,1 AMH Amendments to job 200185.
*:***************************************************************************
* Modifications :
*C200237,1 AMH Define variable and array to get the NRF code [Start]
*C037265,1 TMI 12/09/2003 Amendments in 200185, Purchase order form for MEMO
*C037867,1 TMI 03/04/2004 Add BTName instead of Account, Add two new fields Cprice,Cquota in the frx
*C037867,1                Show the description of the delivery terms
*:***************************************************************************
PRIVATE lcRltColor
lcRltColor = SPACE(0)
DECLARE laColor[1,2]
laColor[1,1] = 'CNRFCODE'
laColor[1,2] = 'lcRltColor'
*C200237,1 AMH [End]

PRIVATE lnI, lnColorStr, lnColorLen
STORE 0 TO lnColorStr, lnColorLen
*C037265,1  TMI [Start] Define variables
PUBLIC lcScaleFl
PRIVATE lnSclLen,lnSclWid,lcPosLnRel
STORE '' TO lcScaleFl,lcPosLnRel 
STORE 0 TO lnSclLen,lnSclWid
lcPoLnFl = gfTempName()
*C037265,1  TMI [End  ] 
SELECT PosHdr
SET SKIP TO
SELECT PosLn
SET SKIP TO
SET ORDER TO PosLn

*-- Open Cutpick & OrdHdr.
IF !USED('CUTPICK')
  =gfOpenFile(gcDataDir+'CUTPICK','CUTPICK','SH')
ENDIF
IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
ENDIF

*C037265,3  TMI [Start] *-- Get Color Segment.
=lfGetColor()
*C037265,3  TMI [End  ] 

*-- Creating the temporary file
lcTmpPolns = gfTempName()
SELECT PosLn
=AFIELDS(laFStru)
*C037265,3  TMI [Start] Remove scale portion from style #
laFStru[ASCAN(laFStru,'STYLE')+2] = 19 - lnSclLen
*C037265,3  TMI [End  ] 

*C037867,1  TMI [Start] set relation to the customer file 
lcSvCstOrd = ORDER('CUSTOMER')
SET ORDER TO CUSTOMER IN CUSTOMER
SELECT ORDHDR
SET RELATION TO 'M'+ORDHDR.ACCOUNT INTO CUSTOMER
LOCATE
*C037867,1  TMI [End  ] 

*C200237,1 AMH Add new field to hold the NRF code & remove not need fields [Start]
*DIMENSION laFStru[ALEN(laFStru,1)+17,4]
DIMENSION laFStru[ALEN(laFStru,1)+13,4]
*C200237,1 AMH [End]

laFStru[ALEN(laFStru,1),1] = 'cNotePad'
laFStru[ALEN(laFStru,1),2] = 'M'
laFStru[ALEN(laFStru,1),3] = 0
laFStru[ALEN(laFStru,1),4] = 0

laFStru[ALEN(laFStru,1)-1,1] = 'nPoTotQty'
laFStru[ALEN(laFStru,1)-1,2] = 'N'
laFStru[ALEN(laFStru,1)-1,3] = 8
laFStru[ALEN(laFStru,1)-1,4] = 0

laFStru[ALEN(laFStru,1)-2,1] = 'cCustPo'
laFStru[ALEN(laFStru,1)-2,2] = 'C'
laFStru[ALEN(laFStru,1)-2,3] = 15
laFStru[ALEN(laFStru,1)-2,4] = 0

laFStru[ALEN(laFStru,1)-3,1] = 'cCompos'
laFStru[ALEN(laFStru,1)-3,2] = 'C'
*C037265,1  TMI [Start] Increase the "cCompos" field width to 100
*laFStru[ALEN(laFStru,1)-3,3] = 63
laFStru[ALEN(laFStru,1)-3,3] = 100
*C037265,1  TMI [End  ] 
laFStru[ALEN(laFStru,1)-3,4] = 0

*C200237,1 AMH remove not need fields [Start]
*laFStru[ALEN(laFStru,1)-4,1] = 'cBackneck'
*laFStru[ALEN(laFStru,1)-4,2] = 'C'
*laFStru[ALEN(laFStru,1)-4,3] = 23
*laFStru[ALEN(laFStru,1)-4,4] = 0

*laFStru[ALEN(laFStru,1)-5,1] = 'cHangTag'
*laFStru[ALEN(laFStru,1)-5,2] = 'C'
*laFStru[ALEN(laFStru,1)-5,3] = 22
*laFStru[ALEN(laFStru,1)-5,4] = 0

*laFStru[ALEN(laFStru,1)-6,1] = 'cCare'
*laFStru[ALEN(laFStru,1)-6,2] = 'C'
*laFStru[ALEN(laFStru,1)-6,3] = 23
*laFStru[ALEN(laFStru,1)-6,4] = 0

*laFStru[ALEN(laFStru,1)-7,1] = 'cBarcode'
*laFStru[ALEN(laFStru,1)-7,2] = 'C'
*laFStru[ALEN(laFStru,1)-7,3] = 24
*laFStru[ALEN(laFStru,1)-7,4] = 0

*laFStru[ALEN(laFStru,1)-8,1] = 'cApproval'
*laFStru[ALEN(laFStru,1)-8,2] = 'C'
*laFStru[ALEN(laFStru,1)-8,3] = 23
*laFStru[ALEN(laFStru,1)-8,4] = 0

*laFStru[ALEN(laFStru,1)-9,1] = 'cPhoto'
*laFStru[ALEN(laFStru,1)-9,2] = 'C'
*laFStru[ALEN(laFStru,1)-9,3] = 4
*laFStru[ALEN(laFStru,1)-9,4] = 0

*laFStru[ALEN(laFStru,1)-10,1] = 'cPcs'
*laFStru[ALEN(laFStru,1)-10,2] = 'C'
*laFStru[ALEN(laFStru,1)-10,3] = 4
*laFStru[ALEN(laFStru,1)-10,4] = 0

*laFStru[ALEN(laFStru,1)-11,1] = 'cSize'
*laFStru[ALEN(laFStru,1)-11,2] = 'C'
*laFStru[ALEN(laFStru,1)-11,3] = 6
*laFStru[ALEN(laFStru,1)-11,4] = 0

*laFStru[ALEN(laFStru,1)-12,1] = 'cPrePro'
*laFStru[ALEN(laFStru,1)-12,2] = 'C'
*laFStru[ALEN(laFStru,1)-12,3] = 25
*laFStru[ALEN(laFStru,1)-12,4] = 0

*laFStru[ALEN(laFStru,1)-13,1] = 'cType'
*laFStru[ALEN(laFStru,1)-13,2] = 'C'
*laFStru[ALEN(laFStru,1)-13,3] = 1
*laFStru[ALEN(laFStru,1)-13,4] = 0

*laFStru[ALEN(laFStru,1)-14,1] = 'cObject_id'
*laFStru[ALEN(laFStru,1)-14,2] = 'C'
*laFStru[ALEN(laFStru,1)-14,3] = 10
*laFStru[ALEN(laFStru,1)-14,4] = 0

*laFStru[ALEN(laFStru,1)-15,1] = 'cColor'
*laFStru[ALEN(laFStru,1)-15,2] = 'C'
*laFStru[ALEN(laFStru,1)-15,3] = 6
*laFStru[ALEN(laFStru,1)-15,4] = 0

*laFStru[ALEN(laFStru,1)-16,1] = 'cClrDesc'
*laFStru[ALEN(laFStru,1)-16,2] = 'C'
*laFStru[ALEN(laFStru,1)-16,3] = 30
*laFStru[ALEN(laFStru,1)-16,4] = 0

laFStru[ALEN(laFStru,1)-4,1] = 'cApproval'
laFStru[ALEN(laFStru,1)-4,2] = 'C'
laFStru[ALEN(laFStru,1)-4,3] = 15
laFStru[ALEN(laFStru,1)-4,4] = 0

*C037265,1  TMI [Start] increase 'cPhoto' width be 11 
*laFStru[ALEN(laFStru,1)-5,1] = 'cPhoto'
*laFStru[ALEN(laFStru,1)-5,2] = 'C'
*laFStru[ALEN(laFStru,1)-5,3] = 5
*laFStru[ALEN(laFStru,1)-5,4] = 0
laFStru[ALEN(laFStru,1)-5,1] = 'cPhoto'
laFStru[ALEN(laFStru,1)-5,2] = 'C'
laFStru[ALEN(laFStru,1)-5,3] = 11
laFStru[ALEN(laFStru,1)-5,4] = 0
*C037265,1  TMI [End  ] 
laFStru[ALEN(laFStru,1)-6,1] = 'cSize'
laFStru[ALEN(laFStru,1)-6,2] = 'C'
laFStru[ALEN(laFStru,1)-6,3] = 10
laFStru[ALEN(laFStru,1)-6,4] = 0

laFStru[ALEN(laFStru,1)-7,1] = 'cPrePro'
laFStru[ALEN(laFStru,1)-7,2] = 'C'
laFStru[ALEN(laFStru,1)-7,3] = 15
laFStru[ALEN(laFStru,1)-7,4] = 0

laFStru[ALEN(laFStru,1)-8,1] = 'cType'
laFStru[ALEN(laFStru,1)-8,2] = 'C'
laFStru[ALEN(laFStru,1)-8,3] = 1
laFStru[ALEN(laFStru,1)-8,4] = 0

laFStru[ALEN(laFStru,1)-9,1] = 'cObject_id'
laFStru[ALEN(laFStru,1)-9,2] = 'C'
laFStru[ALEN(laFStru,1)-9,3] = 10
laFStru[ALEN(laFStru,1)-9,4] = 0

laFStru[ALEN(laFStru,1)-10,1] = 'cColor'
laFStru[ALEN(laFStru,1)-10,2] = 'C'
laFStru[ALEN(laFStru,1)-10,3] = 6
laFStru[ALEN(laFStru,1)-10,4] = 0

laFStru[ALEN(laFStru,1)-11,1] = 'cClrDesc'
laFStru[ALEN(laFStru,1)-11,2] = 'C'
laFStru[ALEN(laFStru,1)-11,3] = 30
laFStru[ALEN(laFStru,1)-11,4] = 0

laFStru[ALEN(laFStru,1)-12,1] = 'cNRFCode'
laFStru[ALEN(laFStru,1)-12,2] = 'C'
laFStru[ALEN(laFStru,1)-12,3] = 5
laFStru[ALEN(laFStru,1)-12,4] = 0
*C200237,1 AMH [End]

*C037265,1  TMI [Start] Add the custom field "cCustNo" and qty's 9-16 fields
lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'cCustNo'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 15
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'QTY9'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'QTY10'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'QTY11'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'QTY12'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'QTY13'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'QTY14'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'QTY15'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'QTY16'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0
*C037265,1  TMI [End  ] 

*C037867,1  TMI [Start] Add the "BTNAME    " field
lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'BTNAME'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 30
laFStru[lnNewLen,4] = 0
*C037867,1  TMI [End  ] 

=gfCrtTmp(lcTmpPolns,@laFStru,'cstytype+po+cType+style+STR(lineno,6)+trancd',lcTmpPolns)


*-- Save current setting of memowidth.
lnMemoWdth = SET('MEMOWIDTH')
*C037265,3  TMI [Start] Set memowidth to the maximum width allowed so that use the whole page width 
*SET MEMOWIDTH TO 80
SET MEMOWIDTH TO 170
*C037265,3  TMI [End  ] 

*-- Declare Array to hold the total Qty.
*C037265,3  TMI [Start] Declare total qty array to hold 16 size 
*DECLARE laTotQty[8]
DECLARE laTotQty[16]
*C037265,3  TMI [End  ] 

*C037265,3  TMI [Start] Comment this line , copy it befor creating the temp file "lcTmpPoLns"
*-- Get Color Segment.
*=lfGetColor()
*C037265,3  TMI [End  ] 

*C037265,1  TMI [Start]  Check if this extended scale company
llExtSS = gfGetMemVar('M_USEEXSSC')
IF llExtSS
  lnSclWid = gfGetMemVar('M_EXTWIDTH')
ENDIF
lcScaleFl = gfTempName()
= lfScaleFl()     && Create scale file with 16 field scale width
= lfPoLnFl()      && Creates Temp PosLn file with 16 field scale width
*C037265,1  TMI [End  ] 

*-- Collecting Data.
SELECT PosHdr
SCAN FOR &lcRpExp
  WAIT WINDOW "PO# : " + PosHdr.PO NOWAIT
  IF SEEK(cStyType+Po,'PosLn')
    *-- Get Header information.
    *C037265,1  TMI [Start] use the created temp file not the posln itself
    *SELECT PosLn
    SELECT &lcPoLnFl
    *C037265,1  TMI [End  ] 
    lnPoTotQty = 0
    *C037265,1  TMI [Start] sum qty's for 16 sizes scale
    *SUM Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
        REST WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cStyType+PosHdr.PO ;
        FOR TranCd = '1' ;
        TO lnPoTotQty
    SUM Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8+Qty9+Qty10+Qty11+Qty12+Qty13+Qty14+Qty15+Qty16 ;
        REST WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cStyType+PosHdr.PO ;
        FOR TranCd = '1' ;
        TO lnPoTotQty
    *C037265,1  TMI [End  ] 
    IF SEEK('2'+POSHDR.PO,'CUTPICK') .AND. SEEK('O'+CUTPICK.ORDER,'ORDHDR')
      lcCustPo = ORDHDR.CUSTPO
    ELSE
      lcCustPo = ''
    ENDIF
    SEEK POSHDR.cStyType+POSHDR.Po
    *C037265,1  TMI [Start] get "cCompos" field from CODES file
    *IF SEEK('F'+SUBSTR(style,1,lnMajSize),'NOTEPAD')
    *  lcCompos = SUBSTR(MLINE(NOTEPAD.MNOTES,1),13)
    *ELSE
    *  lcCompos = ''
    *ENDIF
    
    *C037265,3  TMI [Start] Only Print the Cdiscrep from the codes file as per customer
    *lcCompos = IIF( ALLTRIM(STYLE.CCOMP1) = 'N/A' .OR. EMPTY(STYLE.CCOMP1) , '' ,  ALLTRIM(PADR('CCOMP1 '+ ALLTRIM(STYLE.CCOMP1) + ' ' + ALLTRIM(gfCodDes(STYLE.CCOMP1,'CCOMP1')) , 20)) + ' ' ) + ;
               IIF( ALLTRIM(STYLE.CCOMP2) = 'N/A' .OR. EMPTY(STYLE.CCOMP2) , '' ,  ALLTRIM(PADR('CCOMP2 '+ ALLTRIM(STYLE.CCOMP2) + ' ' + ALLTRIM(gfCodDes(STYLE.CCOMP2,'CCOMP2')) , 20)) + ' ' ) + ;
               IIF( ALLTRIM(STYLE.CCOMP3) = 'N/A' .OR. EMPTY(STYLE.CCOMP3) , '' ,  ALLTRIM(PADR('CCOMP3 '+ ALLTRIM(STYLE.CCOMP3) + ' ' + ALLTRIM(gfCodDes(STYLE.CCOMP3,'CCOMP3')) , 20)) + ' ' ) + ;
               IIF( ALLTRIM(STYLE.CCOMP4) = 'N/A' .OR. EMPTY(STYLE.CCOMP4) , '' ,  ALLTRIM(PADR('CCOMP4 '+ ALLTRIM(STYLE.CCOMP4) + ' ' + ALLTRIM(gfCodDes(STYLE.CCOMP4,'CCOMP4')) , 20)) + ' ' ) + ;
               IIF( ALLTRIM(STYLE.CCOMP5) = 'N/A' .OR. EMPTY(STYLE.CCOMP5) , '' ,  ALLTRIM(PADR('CCOMP5 '+ ALLTRIM(STYLE.CCOMP5) + ' ' + ALLTRIM(gfCodDes(STYLE.CCOMP5,'CCOMP5')) , 20)) + ' ' )    
    *C037867,3  TMI [Start] if cComp2,...,cComp5 is empty do not put the commas
    *lcCompos = IIF( ALLTRIM(STYLE.CCOMP1) = 'N/A' .OR. EMPTY(STYLE.CCOMP1) , '' ,  ALLTRIM(PADR( ALLTRIM(gfCodDes(STYLE.CCOMP1,'CCOMP1')) , 20)) + ', ' ) + ;
               IIF( ALLTRIM(STYLE.CCOMP2) = 'N/A' .OR. EMPTY(STYLE.CCOMP2) , '' ,  ALLTRIM(PADR( ALLTRIM(gfCodDes(STYLE.CCOMP2,'CCOMP2')) , 20)) + ', ' ) + ;
               IIF( ALLTRIM(STYLE.CCOMP3) = 'N/A' .OR. EMPTY(STYLE.CCOMP3) , '' ,  ALLTRIM(PADR( ALLTRIM(gfCodDes(STYLE.CCOMP3,'CCOMP3')) , 20)) + ', ' ) + ;
               IIF( ALLTRIM(STYLE.CCOMP4) = 'N/A' .OR. EMPTY(STYLE.CCOMP4) , '' ,  ALLTRIM(PADR( ALLTRIM(gfCodDes(STYLE.CCOMP4,'CCOMP4')) , 20)) + ', ' ) + ;
               IIF( ALLTRIM(STYLE.CCOMP5) = 'N/A' .OR. EMPTY(STYLE.CCOMP5) , '' ,  ALLTRIM(PADR( ALLTRIM(gfCodDes(STYLE.CCOMP5,'CCOMP5')) , 20))       )    
    
    DIMENSION laCmp[5,2]    
    laCmp[1,1] = ALLTRIM(STYLE.CCOMP1) = 'N/A' .OR. EMPTY(STYLE.CCOMP1)
    laCmp[2,1] = ALLTRIM(STYLE.CCOMP2) = 'N/A' .OR. EMPTY(STYLE.CCOMP2)
    laCmp[3,1] = ALLTRIM(STYLE.CCOMP3) = 'N/A' .OR. EMPTY(STYLE.CCOMP3)
    laCmp[4,1] = ALLTRIM(STYLE.CCOMP4) = 'N/A' .OR. EMPTY(STYLE.CCOMP4)
    laCmp[5,1] = ALLTRIM(STYLE.CCOMP5) = 'N/A' .OR. EMPTY(STYLE.CCOMP5)

    laCmp[1,2] = ALLTRIM(PADR( ALLTRIM(gfCodDes(STYLE.CCOMP1,'CCOMP1')) , 20))    
    laCmp[2,2] = ALLTRIM(PADR( ALLTRIM(gfCodDes(STYLE.CCOMP2,'CCOMP2')) , 20))    
    laCmp[3,2] = ALLTRIM(PADR( ALLTRIM(gfCodDes(STYLE.CCOMP3,'CCOMP3')) , 20))    
    laCmp[4,2] = ALLTRIM(PADR( ALLTRIM(gfCodDes(STYLE.CCOMP4,'CCOMP4')) , 20))    
    laCmp[5,2] = ALLTRIM(PADR( ALLTRIM(gfCodDes(STYLE.CCOMP5,'CCOMP5')) , 20))    
    
    lcCompos = ''
    FOR lnCmp = 1 TO 5
      IF !laCmp[lnCmp,1]
        lcCompos = lcCompos + laCmp[lnCmp,2] + ', '
      ENDIF
    ENDFOR
    
    IF RIGHT(lcCompos,1) = ', '
      lcCompos = SUBSTR(lcCompos,1,LEN(lcCompos)-1)
    ENDIF
    *C037867,3  TMI [End  ]                
    lcCompos = ALLTRIM(lcCompos)
    lcCompos = IIF(RIGHT(lcCompos,1)=',' , SUBSTR(lcCompos,1,LEN(lcCompos)-1) , lcCompos )
    *C037265,3  TMI [End  ]
    *C037265,1  TMI [End  ] 
    *-- Add Footer # 1.
    *C037265,1  TMI [Start] use the created temp file "lcPoLnFl" not the Posln file itself
    *SELECT POSLN
    SELECT &lcPoLnFl
    *C037265,1  TMI [End  ] 
    SEEK POSHDR.cStyType+POSHDR.Po
    SCATTER MEMVAR MEMO    
    *C037265,1  TMI [Start] Update scale field
    IF lnSclWid>0
      M.SCALE = SUBSTR(M.SCALE,1,lnSclWid)
    ENDIF
    *C037265,1  TMI [End  ] 
    INSERT INTO (lcTmpPolns) FROM MEMVAR
    SELECT (lcTmpPolns)
    REPLACE cType    WITH 'F'
    *C200237,1 AMH Get this data from OrdHdr file [Start]
    *IF SEEK('2'+POSHDR.PO,'CUTPICK') .AND. SEEK('B'+CUTPICK.ORDER,'NOTEPAD')
      *REPLACE cBackneck WITH SUBSTR(MLINE(NOTEPAD.MNOTES,2),10,23);
              cHangTag  WITH SUBSTR(MLINE(NOTEPAD.MNOTES,3),11,22);
              cCare     WITH SUBSTR(MLINE(NOTEPAD.MNOTES,4),10,23);
              cBarcode  WITH SUBSTR(MLINE(NOTEPAD.MNOTES,5),9,24);
              cApproval WITH SUBSTR(MLINE(NOTEPAD.MNOTES,8),10,23);
              cPhoto    WITH SUBSTR(MLINE(NOTEPAD.MNOTES,9),7,4);
              cPcs      WITH SUBSTR(MLINE(NOTEPAD.MNOTES,10),7,4);
              cSize     WITH SUBSTR(MLINE(NOTEPAD.MNOTES,11),6,6);
              cPrePro   WITH SUBSTR(MLINE(NOTEPAD.MNOTES,12),16,25)
    *ENDIF            
    IF SEEK('2'+POSHDR.PO,'CUTPICK') .AND. SEEK('O'+CUTPICK.ORDER,'ORDHDR')
      *C037265,1  TMI [Start] Update code values from CODES file
      *REPLACE cApproval WITH ORDHDR.CAPR;
              cPhoto    WITH ORDHDR.CPHO;
              cSize     WITH ORDHDR.CSIZ;
              cPrePro   WITH ORDHDR.CPPROD
      REPLACE cApproval WITH PADR(ALLTRIM(gfCodDes(ORDHDR.CAPR  ,'CAPR'  )),15);
              cPhoto    WITH PADR(ALLTRIM(gfCodDes(ORDHDR.CPHO  ,'CPHO'  )),11);
              cSize     WITH PADR(ALLTRIM(gfCodDes(ORDHDR.CSIZ  ,'CSIZ'  )),10);
              cPrePro   WITH PADR(ALLTRIM(gfCodDes(ORDHDR.CPPROD,'CPPROD')),15);
              ACCOUNT   WITH ORDHDR.ACCOUNT ;
              cCustNo   WITH STYLE.cCustNo
      *C037265,1  TMI [End  ]               
      *C037867,1  TMI [Start] Update the customer name field
      REPLACE BTNAME    WITH CUSTOMER.BTNAME    
      *C037867,1  TMI [End  ] 
    ENDIF
    *C200237,1 AMH [End]


    REPLACE nPoTotQty WITH lnPoTotQty;
            cCustPo WITH lcCustPo;
            cCompos WITH lcCompos
    *-- Add Po Notes.
    IF llRpPrtPn .AND. SEEK('P' + POSHDR.PO , 'NOTEPAD')    
      APPEND BLANK
      REPLACE cStyType WITH PosHdr.cStyType;
              Po       WITH PosHdr.PO;
              cType    WITH 'N'
      lnI = 0
      *C037265,1  TMI [Start] notes can be 10 lines
      *FOR lnI = 1 TO MIN(8,MEMLINES(NotePad.mNotes))
      FOR lnI = 1 TO MIN(10,MEMLINES(NotePad.mNotes))
        *C037265,1  TMI [End  ] 
        REPLACE cNotePad WITH cNotePad + IIF(lnI=1,'',CHR(13)) + MLINE(NotePad.mNotes,lnI)
      ENDFOR
      REPLACE nPoTotQty WITH lnPoTotQty;
              cCustPo WITH lcCustPo;
              cCompos WITH lcCompos
      *C037265,1  TMI [Start] Update account field      
      REPLACE ACCOUNT WITH ORDHDR.ACCOUNT ;
              STYLE   WITH STYLE.STYLE ;
              cCustNo WITH STYLE.cCustNo
      *C037265,1  TMI [End  ]         
      *C037867,1  TMI [Start] Update the customer name field
      REPLACE BTNAME    WITH CUSTOMER.BTNAME    
      *C037867,1  TMI [End  ] 
    ENDIF
    *-- Add Footer # 2.
    APPEND BLANK
    REPLACE cStyType WITH PosHdr.cStyType;
            Po       WITH PosHdr.PO;
            cType    WITH 'O'
    REPLACE nPoTotQty WITH lnPoTotQty;
            cCustPo WITH lcCustPo;
            cCompos WITH lcCompos
    *C037265,1  TMI [Start] Update account field
    REPLACE ACCOUNT WITH ORDHDR.ACCOUNT ;
            STYLE   WITH STYLE.STYLE ;
            cCustNo WITH STYLE.cCustNo
    *C037265,1  TMI [End  ]         
    *C037867,1  TMI [Start] Update the customer name field
    REPLACE BTNAME    WITH CUSTOMER.BTNAME    
    *C037867,1  TMI [End  ] 
   
    *-- Add Objects.
    *C037265,1  TMI [Start]  use the created temp file "lcPoLnFl" not the Posln file itself
    *IF SEEK('S'+SUBSTR(Posln.style,1,lnMajSize),'Objlink')
    IF SEEK('S'+SUBSTR(&lcPoLnFl..style,1,lnMajSize),'Objlink')
    *C037265,1  TMI [End  ]     
      SELECT OBJLINK
      *C037265,1  TMI [Start]  use the created temp file "lcPoLnFl" not the Posln file itself
      *SCAN REST WHILE cObjLnkTyp+cObjLink = 'S'+SUBSTR(Posln.style,1,lnMajSize)
      SCAN REST WHILE cObjLnkTyp+cObjLink = 'S'+SUBSTR(&lcPoLnFl..style,1,lnMajSize)
        *C037265,1  TMI [End  ] 
        SELECT (lcTmpPolns)
        APPEND BLANK
        REPLACE cStyType WITH PosHdr.cStyType;
                Po       WITH PosHdr.PO;
                cType    WITH 'Z';
                cObject_id WITH OBJLINK.COBJECT_ID
        *C037265,3  TMI [Start] 
        REPLACE STYLE     WITH SUBSTR(&lcPoLnFl..style,1,lnMajSize) ;
                nPoTotQty WITH lnPoTotQty ;
                ACCOUNT   WITH ORDHDR.ACCOUNT ;
                cCustPo   WITH lcCustPo ;
                cCustNo   WITH STYLE.cCustNo 
        *C037265,3  TMI [End  ] 
        *C037867,1  TMI [Start] Update the customer name field
        REPLACE BTNAME    WITH CUSTOMER.BTNAME    
        *C037867,1  TMI [End  ] 
      ENDSCAN
    ENDIF
    *-- Add Detail lines.
    *C037265,1  TMI [Start]  use the created temp file "lcPoLnFl" not the Posln file itself
    *SELECT PosLn    
    SELECT &lcPoLnFl
    *C037265,1  TMI [End  ] 
    STORE 0 TO laTotQty
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
                    PosHdr.cStyType+PosHdr.PO FOR TranCd = '1' AND TotQty <> 0
      SCATTER MEMVAR MEMO
      *C037265,1  TMI [Start] Update scale field
      IF lnSclWid>0
        M.SCALE = SUBSTR(M.SCALE,1,lnSclWid)
      ENDIF
      *C037265,1  TMI [End  ] 
      INSERT INTO (lcTmpPolns) FROM MEMVAR
      lnI = 0
      *C037265,3  TMI [Start] Get the sum of 16 colors
      *FOR lnI = 1 TO 8
      *  laTotQty[lnI] = laTotQty[lnI] + EVALUATE('m.Qty'+STR(lnI,1))
      FOR lnI = 1 TO 16
        laTotQty[lnI] = laTotQty[lnI] + EVALUATE('m.Qty'+LTRIM(STR(lnI)))
        *C037265,3  TMI [End  ] 
      ENDFOR
      SELECT (lcTmpPolns)
      REPLACE cType  WITH 'D';
              cColor WITH SUBSTR(STYLE,lnColorStr,lnColorLen);
              cClrDesc WITH gfCodDes(cColor, 'COLOR')
      *C200237,1 AMH fill the cNRFCode field with NRF Code [Start]
      = gfRltFld(cColor, @laColor , 'COLOR     ')
      REPLACE cNRFCode WITH lcRltColor
      *C200237,1 AMH [End]
      REPLACE nPoTotQty WITH lnPoTotQty;
              cCustPo WITH lcCustPo;
              cCompos WITH lcCompos
      *C037265,1  TMI [Start] Update account field
      REPLACE ACCOUNT WITH ORDHDR.ACCOUNT ;
              cCustNo WITH STYLE.cCustNo
      *C037265,1  TMI [End  ]         
      *C037867,1  TMI [Start] Update the customer name field
      REPLACE BTNAME    WITH CUSTOMER.BTNAME    
      *C037867,1  TMI [End  ] 
    ENDSCAN
    *-- Add Total line.
    SELECT (lcTmpPolns)
    *C200237,1 AMH Not need to add total line [Start]
    *C200237,1 AMH only replace the ctype of the last record with 'E'
    *APPEND BLANK
    *C037265,3  TMI [Start] Use the type 'E' for totals , and get the     
    APPEND BLANK
    FOR lnI = 1 TO 16
      lcI = LTRIM(STR(lnI))
      REPLACE QTY&lcI. WITH laTotQty[lnI]      ;
              TOTQTY WITH TOTQTY+laTotQty[lnI]      
    ENDFOR
    REPLACE REFERENCE WITH 'TOTALS:'
    *C037265,3  TMI [End  ] 
    REPLACE cStyType WITH PosHdr.cStyType;
            Po       WITH PosHdr.PO;
            cType    WITH 'E'
    *lnI = 0
    *FOR lnI = 1 TO 8
      *lcI = STR(lnI,1)
      *REPLACE QTY&lcI. WITH laTotQty[lnI]
    *ENDFOR
    *C200237,1 AMH [End]
    REPLACE nPoTotQty WITH lnPoTotQty;
            cCustPo WITH lcCustPo;
            cCompos WITH lcCompos
  ENDIF
ENDSCAN

*-- Restore setting of memowidth.
SET MEMOWIDTH TO EVALUATE('lnMemoWdth')

IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF

IF USED('ORDHDR')
  USE IN ORDHDR
ENDIF

SELECT PosLn
SET RELATION TO
SELECT PosHdr
SET RELATION TO

*C037265,1  TMI [Start] close the temp created file "lcPoLnFl"
USE IN &lcPoLnFl
ERASE (gcWorkDir+lcPoLnFl+".DBF")
ERASE (gcWorkDir+lcPoLnFl+".CDX")
ERASE (gcWorkDir+lcPoLnFl+".FPT")
*C037265,1  TMI [End  ] 

SELECT (lcTmpPolns)
LOCATE
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
*C037265,1  TMI [Start] Set the relation to "lcScaleFl" instead
*SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO 'S' + Scale INTO &lcScaleFl ADDITIVE
*C037265,1  TMI [End  ] 
SET RELATION TO cObject_id INTO Objects ADDITIVE

SELECT PosHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE

llOpenComp = .F.
IF !USED('SYCCOMP')
  =gfOpenFile(gcSysHome+'SYCCOMP','','SH')
  llOpenComp = .T.
ENDIF
SELECT SYCCOMP
SEEK gcAct_Comp
*C037265,1  TMI [Start] Use default values assigned to the array "laCompAdd"
*DECLARE laCompAdd[2,1]
*laCompAdd = ''                     && Array to hold the Company address
*C037265,1  TMI [End  ] 
lcCompName = cCom_Name             && Variable to hold the Company Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
lcCompFax = cCom_Fax               && Variable to hold the Company Fax
*C037265,1  TMI [Start] Use default values assigned to the array "laCompAdd"
*laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
*laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
*C037265,1  TMI [End  ] 
lcCompFax = TRANSFORM(lcCompFax , lcPhonPict) && Fax No. Pic
lcXphone = TRANSFORM(lcCompPhon , lcPhonPict) && variable hold the phone format to use it
                                              && in the forms befor shifting the array.
*C037867,1  TMI [Start] Restore order in customer file
SET ORDER TO &lcSvCstOrd IN CUSTOMER
*C037867,1  TMI [End  ] 
IF llOpenComp
  USE IN SYCCOMP
ENDIF
SELECT (lcTmpPolns)
lcRpExp = ".T."

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 04/29/2001
*! Purpose   : Get the color information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetColor()
*!*************************************************************
*
FUNCTION lfGetColor

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)

FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnColorStr = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnColorLen = LEN(laStySeg[lnCnt , 3])
  ENDIF  
  *C037265,1  TMI [Start] Get Scale data
  IF laStySeg[lnCnt , 1] = "S"
    *-- Var. hold the color segment lenght in the style code strucure.
    lnSclLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
  *C037265,1  TMI [End  ] 
ENDFOR

*:**************************************************************************
*:* Name        : lfComps
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/07/2003
*:* Purpose     : Return the concatenation of the 5 custom fields for composition
*:***************************************************************************
*:* Called from : postypme.frx
*:***************************************************************************
FUNCTION lfComps
PRIVATE lcCompos


RETURN lcCompos

*-- end of lfComps.

*:**************************************************************************
*:* Name        : lfScaleFl                                       *C037265,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/14/2003
*:* Purpose     : Creates a temp file for scales, with the same alias of scale
*                 to be used in the relation in the report but with 16 size in 
*                 each scale
*:***************************************************************************
FUNCTION lfScaleFl
PRIVATE laScale,lnCnt,lnI
lcScaleFl = gfTempName()
CREATE TABLE (gcWorkDir+lcScaleFl) (SZ1 C(5),SZ2 C(5),SZ3 C(5),SZ4 C(5),;
                                    SZ5 C(5),SZ6 C(5),SZ7 C(5),SZ8 C(5),;
                                    SZ9 C(5),SZ10 C(5),SZ11 C(5),SZ12 C(5),;
                                    SZ13 C(5),SZ14 C(5),SZ15 C(5),SZ16 C(5),;
                                    TYPE C(1),SCALE C(3),NOTE M)
INDEX ON TYPE+SCALE TAG 'SCALE'
                                
SELECT SCALE
SET ORDER TO SCALE
GO TOP
=SEEK('S','SCALE')
DO WHILE !EOF()
  DIMENSION laScale[19]
  laScale = ''
  laScale[17] = 'S'
  laScale[18] = SUBSTR(SCALE.SCALE,1,IIF(lnSclWid>0,lnSclWid,3))
  lnCnt = 1
  SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+laScale[18]
    IF lnCnt <= 16
      laScale[19] = laScale[19]  + SCALE.SCALE + ' ' + STR(SCALE.CNT,1) + CHR(13)
    ENDIF
    FOR lnI = 1 TO SCALE.CNT
      IF lnCnt <= 16
        IF !EMPTY(EVAL('SCALE.SZ'+STR(lnI,1)))
          laScale[lnCnt] = EVAL('SCALE.SZ'+STR(lnI,1))
          lnCnt = lnCnt + 1
        ENDIF
      ELSE
        EXIT  && exit the loop
      ENDIF
    ENDFOR
  ENDSCAN
  IF !SEEK('S'+laScale[2],lcScaleFl)
    INSERT INTO &lcScaleFl FROM ARRAY laScale
  ENDIF
ENDDO
*-- end of lfScaleFl.

*:**************************************************************************
*:* Name        : lfPoLnFl                                        *C037265,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/14/2003
*:* Purpose     : Create a temp posln file with 12 field scales
*:***************************************************************************
FUNCTION lfPoLnFl
PRIVATE laStru,lnLen,lcKey
IF !'POSLN' $ DBF('POSLN')
  USE IN POSLN
ENDIF  

SELECT POSLN
=AFIELDS(laStru)
lcKey = KEY()

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'QTY9'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 6
laStru[lnLen,4] = 0

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'QTY10'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 6
laStru[lnLen,4] = 0

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'QTY11'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 6
laStru[lnLen,4] = 0

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'QTY12'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 6
laStru[lnLen,4] = 0

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'QTY13'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 6
laStru[lnLen,4] = 0

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'QTY14'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 6
laStru[lnLen,4] = 0

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'QTY15'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 6
laStru[lnLen,4] = 0

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'QTY16'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 6
laStru[lnLen,4] = 0

CREATE TABLE (gcWorkDir+lcPoLnFl) FROM ARRAY laStru
INDEX ON &lcKey TAG 'POSLN'

SELECT PosHdr
SCAN FOR &lcRpExp
  SELECT POSLN
  SEEK POSHDR.cStyType+POSHDR.Po
  DO WHILE POSLN.cstytype+POSLN.po = PosHdr.cStyType+PosHdr.PO AND ;
           TranCd = '1' AND TotQty <> 0
    SCATTER MEMVAR MEMO
    lcSty = SUBSTR(POSLN.STYLE,1,19-lnSclLen)
    STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,;
               M.QTY7,M.QTY8,M.QTY9,M.QTY10,M.QTY11,M.QTY12,;
               M.QTY13,M.QTY14,M.QTY15,M.QTY16
    =SEEK('S'+SUBSTR(SCALE.SCALE,1,lnSclWid),lcScaleFl)
    PRIVATE laSclArr     
    DIME laSclArr[16,4]
    laSclArr = ' '
    =lfSclArr() && Populate the array laScrArr
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cStyType+PosHdr.PO+lcSty ;
              FOR TranCd = '1' AND TotQty <> 0
      *-Get qty's 
      lnSclPos = ASCAN(laSclArr,SCALE.SCALE)
      IF lnSclPos>0
        FOR lnI = 1 TO laSclArr[lnSclPos+3]-laSclArr[lnSclPos+2]+1
          lcI = STR(lnI,1)
          lcCnt = LTRIM(STR(laSclArr[lnSclPos+2]+lnI-1))
          M.QTY&lcCnt = POSLN.QTY&lcI
        ENDFOR
      ENDIF
      *-
    ENDSCAN
    
    M.TOTQTY = M.QTY1+M.QTY2+M.QTY3+M.QTY4+M.QTY5+M.QTY6+;
               M.QTY7+M.QTY8+M.QTY9+M.QTY10+M.QTY11+M.QTY12+;
               M.QTY13+M.QTY14+M.QTY15+M.QTY16
    M.STYLE = SUBSTR(M.STYLE,1,19-lnSclLen)
    IF !SEEK(PosHdr.cStyType+PosHdr.PO+lcSty,lcPoLnFl)
      INSERT INTO (lcPoLnFl) FROM MEMVAR   
    ENDIF
  ENDDO
ENDSCAN

USE IN &lcPoLnFl
SELECT PosLn
lcPosLnRel = SET('RELATION')
USE (gcWorkDir+lcPoLnFl) IN 0 ORDER TAG 'POSLN' 
SET RELATION TO &lcPosLnRel
SET RELATION OFF INTO STYLE
SET RELATION TO SUBSTR(STYLE,1,19-lnSclLen) INTO STYLE
GO TOP

SELECT POSHDR
SET RELATION TO IIF(lcrpform="A",Poshdr.cstytype,lcrpform)+po INTO &lcPoLnFl ADDITIVE
GO TOP

*-- end of lfPoLnFl.

*:**************************************************************************
*:* Name        : lfSclArr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/21/2003
*:* Purpose     : Populate the array laSclArr from the field &lcSclFl..NOTE 
*:***************************************************************************
*:* Called from : lfPoLnFl
*:***************************************************************************
FUNCTION lfSclArr
PRIVATE lnI,lnCnt
lnCnt = 1
FOR lnI = 1 TO MEMLINES(&lcScaleFl..NOTE)
  IF !EMPTY(MLINE(&lcScaleFl..NOTE,lnI))
    laSclArr[lnI,1] = SUBSTR( MLINE(&lcScaleFl..NOTE,lnI) , 1 , 3 )
    laSclArr[lnI,2] = VAL(SUBSTR( MLINE(&lcScaleFl..NOTE,lnI) , 5 , 1 ))  && SCALE.CNT
    laSclArr[lnI,3] = lnCnt
    laSclArr[lnI,4] = lnCnt + VAL(SUBSTR( MLINE(&lcScaleFl..NOTE,lnI) , 5 , 1 )) - 1
    IF laSclArr[lnI,4] >= 16
      laSclArr[lnI,4] = 16
      EXIT
    ENDIF
    lnCnt = lnCnt + VAL(SUBSTR( MLINE(&lcScaleFl..NOTE,lnI) , 5 , 1 ))
  ENDIF
ENDFOR
*-- end of lfSclArr.