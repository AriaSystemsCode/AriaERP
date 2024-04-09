*:***************************************************************************
*: Program file  : POSTYPNK
*: Program desc. : Print PO for "NIK NAK CLOTHING LTD"
*: For Report    : POSTYPNK.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Developer     : TMI - TAREK MOHAMMED IBRAHIM 
*: Reference     : *C123615,1  
*: Date          : 10/31/2004
*:***************************************************************************
PRIVATE lnI,lcI, lnColorStr, lnColorLen , lcPctFl

PRIVATE lnSclLen,lnSclWid,lcPosLnRel,lnPos,lnExt
STORE '' TO lcPosLnRel 
STORE 0 TO lnSclLen,lnSclWid

*IF llOGFltCh
  *-- Get Color Data.
  STORE 0 TO lnColorStr, lnColorLen
  =lfGetColor()
  
  *- Restore variables used in Object Auto Searsh, 
  *- we will use from them lcPath1,...,lcPath5 and LCPEXT1,...,LCPEXT5
  llFoundP = FILE('NAKMAIN.MEM')
  IF llFoundP
    RESTORE FROM NAKMAIN.MEM ADDITIVE
  ENDIF
  DIMENSION laPctPath[2],lcPctTyp[2],laLnkClass[2]
  STORE '' TO laPctPath,lcPctTyp
  lcPctTyp[1] = 'BMP'
  lcPctTyp[2] = 'XLS'
  laLnkClass[1] = 'Paint.Picture'
  laLnkClass[2] = 'ExcelWorksheet'
  
  FOR lnI = 1 TO 5
    lcI = STR(lnI,1)
    laPctPath[1] = IIF(lcPExt&lcI = 'BMP' , lcPath&lcI , laPctPath[1] )
    laPctPath[2] = IIF(lcPExt&lcI = 'XLS' , lcPath&lcI , laPctPath[2] )
  ENDFOR

  SELECT PosHdr
  SET SKIP TO
  SELECT PosLn
  SET SKIP TO
  SET ORDER TO PosLn
  
  *-- Open some needed files.
  =lfOpnFls()
  
  *- Creating temp files
  =lfTmpFl()
  
  *- set relation to the customer file 
  lcSvCstOrd = ORDER('CUSTOMER')
  SET ORDER TO CUSTOMER IN CUSTOMER
  SELECT ORDHDR
  SET RELATION TO 'M'+ORDHDR.ACCOUNT INTO CUSTOMER
  LOCATE
  
  *-- Save current setting of memowidth.
  lnMemoWdth = SET('MEMOWIDTH')
  SET MEMOWIDTH TO 170
  
  llExtSS = gfGetMemVar('M_USEEXSSC')
  IF llExtSS
    lnSclWid = gfGetMemVar('M_EXTWIDTH')
  ENDIF
  =lfScaleFl()     && Create scale file with 13 field scale width
  =lfPoLnFl()      && Creates Temp PosLn file with 13 field scale width
  
  *-- Collecting Data.
  SELECT PosHdr
  SCAN FOR &lcRpExp
    
    =SEEK(&lcPoLnFl..CSTYTYPE+&lcPoLnFl..PO,'POSHDR')
    
    WAIT WINDOW "PO# : " + PosHdr.PO NOWAIT
    IF SEEK(cStyType+Po,'PosLn')
      
      *-- Get Header information.
      SELECT &lcPoLnFl
      lnPoTotQty = 0
      SUM Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8+Qty9+Qty10+Qty11+Qty12+Qty13 ;
          REST WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cStyType+PosHdr.PO ;
          FOR TranCd = '1' ;
          TO lnPoTotQty
  
      lcCustPo = ''
      IF SEEK('2'+POSHDR.PO,'CUTPICK') .AND. SEEK('O'+CUTPICK.ORDER,'ORDHDR')
        lcCustPo = ORDHDR.CUSTPO
      ENDIF
  
      SEEK POSHDR.cStyType+POSHDR.Po
      
      *-- Add Footer # 1.
      *use the created temp file "lcPoLnFl" not the Posln file itself
      SELECT &lcPoLnFl
      SEEK POSHDR.cStyType+POSHDR.Po
      SCATTER MEMVAR MEMO    
      *- Update scale field
      IF lnSclWid>0
        M.SCALE = SUBSTR(M.SCALE,1,lnSclWid)
      ENDIF
      =SEEK(POSHDR.CADD_USER ,'SYUUSER')
      m.EMAIL = SYUUSER.EMAIL
      m.cCustPo = lcCustPo
      INSERT INTO (lcTmpPolns) FROM MEMVAR

      *- Update notepad 
      =SEEK('P' + POSHDR.PO , 'NOTEPAD')    
      m.cNotePad = NotePad.mNotes

      *- (BackNick- and other fields ) If there are multiple styles on the PO just pick up from the first style
      SELECT (lcTmpPolns)
      REPLACE cType      WITH 'F' ;
              cNotePad   WITH m.cNotePad ;
              CBN        WITH STYLE.CBN ;
              CBN2       WITH STYLE.CBN2 ;
              CHT        WITH STYLE.CHT ;
              CCOMC1     WITH STYLE.CCOMC1 ;
              CCOMC2     WITH STYLE.CCOMC2 ;
              CSIZETAB   WITH STYLE.CSIZETAB ;
              CFLAGLABEL WITH STYLE.CFLAGLABEL ;
              HT2        WITH STYLE.HT2 ;
              cCustNo    WITH STYLE.cCustNo ;      
              nPoTotQty  WITH lnPoTotQty

      *-- Add Footer # 2.
      APPEND BLANK
      REPLACE cStyType  WITH PosHdr.cStyType;
              Po        WITH PosHdr.PO;
              cType     WITH 'O' ;
              nPoTotQty WITH lnPoTotQty ;
              STYLE     WITH STYLE.STYLE ;
              cCustNo   WITH STYLE.cCustNo ;
              cCustPo   WITH lcCustPo
     
      *-- Add Objects.
      IF TYPE('llRpPct')='U' .OR. llRpPct
      
      PRIVATE laStys
      DIMENSION laStys[1]
      laStys = ' '
      lcPO = POSHDR.PO
      SELECT DIST SUBSTR(&lcPoLnFl..style,1,lnMajSize) ;
             FROM &lcPoLnFl ;
             WHERE PO = lcPO ;
             INTO ARRAY laStys
            
      FOR lnK = 1 TO ALEN(laStys)
        IF !EMPTY(laStys[lnK])
        
          IF SEEK('S'+PADR(laStys[lnK],20),'Objlink')
            SELECT OBJLINK
            SCAN REST WHILE COBJLNKTYP+COBJLINK = 'S'+PADR(laStys[lnK],20)
            
              *- Append bmp file , and xls file
              =SEEK(ObjLink.COBJECT_ID,'OBJECTS')
              lcPctFl = ALLTRIM(OBJECTS.CIMGPATH)
              lnExt = ASCAN(lcPctTyp, RIGHT(ALLTRIM(lcPctFl),3) )
              IF lnExt>0 .AND. !EMPTY(laPctPath[lnExt]) .AND. FILE(lcPctFl)
                =SEEK(laStys[lnK],'STYLE')
  
                SELECT (lcTmpPolns)
  
                APPEND BLANK
  
                REPLACE cStyType   WITH PosHdr.cStyType;
                        Po         WITH PosHdr.PO;
                        cType      WITH 'Z';
                        CIMGPATH   WITH lcPctFl ;
                        STYLE      WITH laStys[lnK] ;
                        nPoTotQty  WITH lnPoTotQty ;
                        cCustNo    WITH STYLE.cCustNo ;
                        cCustPo    WITH lcCustPo ;
                        OBJTYP     WITH lcPctTyp[lnExt]
                        
                lcPictClas = IIF(lcPctTyp[lnExt] = 'BMP' , '' , 'CLASS (laLnkClass[lnExt])' )
                APPEND GENERAL gObject FROM (lcPctFl) LINK &lcPictClas
  
              ENDIF
            ENDSCAN
          ENDIF
          
        ENDIF
      ENDFOR
      
      WAIT CLEAR
      DIME laStyList[1]
      laStyList = ' '
      
      ENDIF
  
      *-- Add Detail lines.
      *-  use the created temp file "lcPoLnFl" not the Posln file itself
      *SELECT PosLn    
      SELECT &lcPoLnFl
      SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
                      PosHdr.cStyType+PosHdr.PO FOR TranCd = '1' AND TotQty <> 0
        SCATTER MEMVAR MEMO
        
*        WAIT WINDOW NOWAIT 'Updating lines for style ' + &lcPoLnFl..STYLE
  
        *- Get order line notepad for the style
        m.SLSNOTEPAD = ''
        lcStyClr = SUBSTR(&lcPoLnFl..STYLE,1,19-lnSclLen)
        IF SEEK('2'+POSHDR.PO,'CUTPICK')
          =SEEK('2'+POSHDR.PO+STR(&lcPoLnFl..LINENO,6),'CUTPICK')      
          IF SEEK('O'+CUTPICK.ORDER+CUTPICK.CORDLINE,'ORDLINE')
            m.SLSNOTEPAD = MLINE(ORDLINE.NOTE_MEM,1)
          ENDIF
        ELSE
          *if there is no SO assigned to the PO i.e.
          *no cutpick record can it pick up this data starting from line 12 on the
          *PO notepad, assuming a line per colour in the order in which the colours
          *are listed on the PO
          lnPos = lfClrNo()
          IF lnPos > 0
            m.SLSNOTEPAD = MLINE(m.cNotePad,12+lnPos-1)
          ENDIF
        ENDIF
  
        *- Update scale field
        IF lnSclWid>0
          M.SCALE = SUBSTR(M.SCALE,1,lnSclWid)
        ENDIF
  
        m.cCustPo = lcCustPo
        INSERT INTO (lcTmpPolns) FROM MEMVAR
  
        SELECT (lcTmpPolns)
        REPLACE cType     WITH 'D' ;
                cColor    WITH SUBSTR(STYLE,lnColorStr,lnColorLen) ;
                cClrDesc  WITH gfCodDes(cColor, 'COLOR') ;
                nPoTotQty WITH lnPoTotQty ;
                cCustNo   WITH STYLE.cCustNo 
      ENDSCAN
    ENDIF
  ENDSCAN
  
  *-Get Cartons Qtys based on ratios
  
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
  
  *- close the temp created file "lcPoLnFl"
  USE IN &lcPoLnFl
  ERASE (gcWorkDir+lcPoLnFl+".DBF")
  ERASE (gcWorkDir+lcPoLnFl+".CDX")
  ERASE (gcWorkDir+lcPoLnFl+".FPT")
  
  SELECT PosHdr
  SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
  SET RELATION TO CADD_USER INTO SYUUSER ADDITIVE
  
  llOpenComp = .F.
  IF !USED('SYCCOMP')
    =gfOpenFile(gcSysHome+'SYCCOMP','','SH')
    llOpenComp = .T.
  ENDIF
  SELECT SYCCOMP
  SEEK gcAct_Comp
  *- Use default values assigned to the array "laCompAdd"
  *DECLARE laCompAdd[2,1]
  *laCompAdd = ''                    && Array to hold the Company address
  lcCompName = cCom_Name             && Variable to hold the Company Name
  lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
  lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
  lcCompFax = cCom_Fax               && Variable to hold the Company Fax
  
  *- remove Phone line from laCompAdd array
  FOR lnIx = 1 TO ALEN(laCompAdd)
    laCompAdd[lnIx] = IIF('Phone#' $ laCompAdd[lnIx] , '' , laCompAdd[lnIx] )
  ENDFOR
  
  lcCompFax = TRANSFORM(lcCompFax , lcPhonPict) && Fax No. Pic
  lcXphone = TRANSFORM(lcCompPhon , lcPhonPict) && variable hold the phone format to use it
                                                && in the forms befor shifting the array.
  *- Restore order in customer file
  SET ORDER TO &lcSvCstOrd IN CUSTOMER
  IF llOpenComp
    USE IN SYCCOMP
  ENDIF
  
*ENDIF

SELECT (lcTmpPolns)
SET RELATION TO cStyType+PO INTO PosHdr
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO &lcScaleFl ADDITIVE
SET RELATION TO 'F' + PADR(SUBSTR(STYLE,1,lnMajSize),20) INTO NOTEPAD ADDITIVE
LOCATE

SET CENTURY ON 
lcRpExp = '.T.'

*:**************************************************************************
*:* Name        : lfClrNo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/08/2004
*:* Purpose     : Get color # in list of colors of a style
*:***************************************************************************
*C123615,1
FUNCTION lfClrNo

* Use this array to count the style/color position in the list of style/colors in the po to 
* pick the appropriate po note line for the style/color
IF SUBSTR(laStyList[1],1,lnMajSize) <> SUBSTR(&lcPoLnFl..STYLE,1,lnMajSize)
  PRIVATE lcMajSty
  DIME laStyList[1]
  laStyList = ' '
  lcMajSty = SUBSTR(&lcPoLnFl..STYLE,1,lnMajSize)
  SELECT SUBSTR(STYLE,1,19-lnSclLen) AS STY;
    FROM &lcPoLnFl ;
    WHERE SUBSTR(STYLE,1,19-lnSclLen) = lcMajSty ;
    ORDER BY STY ;
    INTO ARRAY laStyList
ENDIF
RETURN ASCAN(laStyList,lcStyClr)
*-- end of lfClrNo.

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
*C123615,1
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
  *- Get Scale data
  IF laStySeg[lnCnt , 1] = "S"
    *-- Var. hold the color segment lenght in the style code strucure.
    lnSclLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR

*:**************************************************************************
*:* Name        : lfScaleFl                                       *C123615,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/02/2004
*:* Purpose     : Creates a temp file for scales, with the same alias of scale
*                 to be used in the relation in the report but with 13 size in 
*                 each scale
*:***************************************************************************
*C123615,1
FUNCTION lfScaleFl
PRIVATE laScale,lnCnt,lnI
CREATE TABLE (gcWorkDir+lcScaleFl) (SZ1  C(5),SZ2  C(5),SZ3  C(5),SZ4 C(5),SZ5  C(5),;
                                    SZ6  C(5),SZ7  C(5),SZ8  C(5),SZ9 C(5),SZ10 C(5),;
                                    SZ11 C(5),SZ12 C(5),SZ13 C(5),;
                                    TYPE C(1),SCALE C(3),NOTE M)
INDEX ON TYPE+SCALE TAG 'SCALE'
                                
SELECT SCALE
SET ORDER TO SCALE
GO TOP
=SEEK('S','SCALE')
DO WHILE !EOF()
  DIMENSION laScale[19]
  laScale = ''
  laScale[14] = 'S'
  laScale[15] = SUBSTR(SCALE.SCALE,1,IIF(lnSclWid>0,lnSclWid,3))
  lnCnt = 1
  SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+laScale[15]
    IF lnCnt <= 13
      laScale[16] = laScale[16]  + SCALE.SCALE + ' ' + STR(SCALE.CNT,1) + CHR(13)
    ENDIF
    FOR lnI = 1 TO SCALE.CNT
      IF lnCnt <= 13
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
*:* Name        : lfPoLnFl                                        *C123615,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/02/2004
*:* Purpose     : Create a temp posln file with 13 field scales
*:***************************************************************************
*C123615,1
FUNCTION lfPoLnFl
PRIVATE laFStru,lnLen,lcKey,lnNewLen
IF !'POSLN' $ DBF('POSLN')
  USE IN POSLN
ENDIF  

SELECT POSLN
=AFIELDS(laFStru)
lcKey = KEY()

FOR lnI = 9 TO 13
  lnNewLen = ALEN(laFStru,1) + 1
  DIMENSION laFStru[lnNewLen,4]
  laFStru[lnNewLen,1] = 'QTY'+ALLTRIM(STR(lnI))
  laFStru[lnNewLen,2] = 'N'
  laFStru[lnNewLen,3] = 6
  laFStru[lnNewLen,4] = 0
ENDFOR

FOR lnI = 1 TO 13
  lnNewLen = ALEN(laFStru,1) + 1
  DIMENSION laFStru[lnNewLen,4]
  laFStru[lnNewLen,1] = 'PQTY'+ALLTRIM(STR(lnI))
  laFStru[lnNewLen,2] = 'N'
  laFStru[lnNewLen,3] = 6
  laFStru[lnNewLen,4] = 0
ENDFOR  

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'PTOTQTY'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'CTNQTY'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

IF ASCAN(laFStru,'PACK_ID')=0
  lnNewLen = ALEN(laFStru,1) + 1
  DIMENSION laFStru[lnNewLen,4]
  laFStru[lnNewLen,1] = 'PACK_ID'
  laFStru[lnNewLen,2] = 'C'
  laFStru[lnNewLen,3] = 16
  laFStru[lnNewLen,4] = 0
ENDIF  

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'nPoTotQty'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 8
laFStru[lnNewLen,4] = 0

CREATE TABLE (gcWorkDir+lcPoLnFl) FROM ARRAY laFStru
INDEX ON &lcKey TAG 'POSLN'

SELECT PosHdr
SCAN FOR &lcRpExp
  SELECT POSLN
  SEEK POSHDR.cStyType+POSHDR.Po
  
  DO WHILE POSLN.cstytype+POSLN.po = PosHdr.cStyType+PosHdr.PO AND ;
           TranCd = '1' AND TotQty <> 0
    SCATTER MEMVAR MEMO

    lcSty = SUBSTR(POSLN.STYLE,1,19-lnSclLen)
    STORE 0 TO M.QTY1 ,M.QTY2 ,M.QTY3 ,M.QTY4 ,M.QTY5 ,M.QTY6,M.QTY7,M.QTY8,M.QTY9,;
               M.QTY10,M.QTY11,M.QTY12,M.QTY13,M.TOTQYT,;
               M.PQTY1 ,M.PQTY2 ,M.PQTY3 ,M.PQTY4 ,M.PQTY5 ,M.PQTY6,M.PQTY7,M.PQTY8,M.PQTY9,;
               M.PQTY10,M.PQTY11,M.PQTY12,M.PQTY13,M.PTOTQTY
    =SEEK('S'+SUBSTR(SCALE.SCALE,1,lnSclWid),lcScaleFl)
    PRIVATE laSclArr     
    DIME laSclArr[13,4]
    laSclArr = ' '
    =lfSclArr() && Populate the array laScrArr
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cStyType+PosHdr.PO+lcSty ;
              FOR TranCd = '1' AND TotQty <> 0
      m.PACK_ID = ' '
      IF SEEK('2'+POSLN.PO+STR(POSLN.LINENO,6),'CUTPICK')
        =SEEK('O'+CUTPICK.ORDER+CUTPICK.CORDLINE,'ORDLINE')
        m.PACK_ID = ORDLINE.PACK_ID
        IF !SEEK('P'+ORDLINE.ACCOUNT+ORDLINE.PACK_ID+ORDLINE.STYLE,'SPCK_LIN')
          =SEEK('P*****'+ORDLINE.PACK_ID+ORDLINE.STYLE,'SPCK_LIN')
        ENDIF
      ENDIF
      
      *-Get qty's 
      lnSclPos = ASCAN(laSclArr,SCALE.SCALE)
      IF lnSclPos>0
        FOR lnI = 1 TO laSclArr[lnSclPos+3]-laSclArr[lnSclPos+2]+1
          lcI = STR(lnI,1)
          lcCnt = LTRIM(STR(laSclArr[lnSclPos+2]+lnI-1))
          M.QTY&lcCnt = POSLN.QTY&lcI
          
          IF !EMPTY(m.PACK_ID) 
            M.PQTY&lcCnt = SPCK_LIN.QTY&lcI
          ENDIF
          
        ENDFOR
      ENDIF

    ENDSCAN
    
    =IIF(EMPTY(m.PACK_ID),lfGetRatio(),'')
     
    M.TOTQTY  = M.QTY1+M.QTY2+M.QTY3+M.QTY4+M.QTY5+M.QTY6+;
                M.QTY7+M.QTY8+M.QTY9+M.QTY10+M.QTY11+M.QTY12+M.QTY13
    M.PTOTQTY = M.PQTY1+M.PQTY2+M.PQTY3+M.PQTY4+M.PQTY5+M.PQTY6+;
                M.PQTY7+M.PQTY8+M.PQTY9+M.PQTY10+M.PQTY11+M.PQTY12+M.PQTY13
             
    M.STYLE   = SUBSTR(M.STYLE,1,19-lnSclLen)

    IF !SEEK(PosHdr.cStyType+PosHdr.PO+lcSty,lcPoLnFl)
      INSERT INTO (lcPoLnFl) FROM MEMVAR   
    ENDIF

  ENDDO
ENDSCAN

*-If no pack , let the style be the actual pack
SELECT &lcPoLnFl
REPLACE PACK_ID WITH STYLE FOR EMPTY(PACK_ID)

PRIVATE laPoCtnQty,lnArrLen
DIMENSION laPoCtnQty[1,3]
lnArrLen = 0
lcPO      = ' '
lcPack_ID = ' '

SELECT (lcPoLnFl)
lcOrder = ORDER()
INDEX ON PO+PACK_ID TAG PACK_ID
GO TOP
DO WHILE !EOF(lcPoLnFl)
  IF lcPO <> &lcPoLnFl..PO
    lcPO     = &lcPoLnFl..PO
    lnArrLen = lnArrLen + 1
    DIMENSION laPoCtnQty[lnArrLen,3]
    laPoCtnQty[lnArrLen,1] = &lcPoLnFl..PO
    STORE 0 TO laPoCtnQty[lnArrLen,2],laPoCtnQty[lnArrLen,3]
  ENDIF
  laPoCtnQty[lnArrLen,2] = laPoCtnQty[lnArrLen,2] + TOTQTY/PTOTQTY
  lcPack_id = &lcPoLnFl..PACK_ID
  SCAN REST WHILE PO+PACK_ID = lcPO+lcPack_id
    laPoCtnQty[lnArrLen,3] = laPoCtnQty[lnArrLen,3] + TOTQTY
  ENDSCAN
ENDDO

*- Update CtnQty field 
GO TOP
SCAN
  lnPo = ASCAN(laPoCtnQty,&lcPoLnFl..PO)
  REPLACE CTNQTY    WITH laPoCtnQty[lnPo+1] ;
          nPototqty WITH laPoCtnQty[lnPo+2]
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
*:* Name        : lfSclArr                                          *C123615
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/02/2004
*:* Purpose     : Populate the array laSclArr from the field &lcSclFl..NOTE 
*:***************************************************************************
*:* Called from : lfPoLnFl
*:***************************************************************************
*C123615,1
FUNCTION lfSclArr
PRIVATE lnI,lnCnt
lnCnt = 1
FOR lnI = 1 TO MEMLINES(&lcScaleFl..NOTE)
  IF !EMPTY(MLINE(&lcScaleFl..NOTE,lnI))
    laSclArr[lnI,1] = SUBSTR( MLINE(&lcScaleFl..NOTE,lnI) , 1 , 3 )
    laSclArr[lnI,2] = VAL(SUBSTR( MLINE(&lcScaleFl..NOTE,lnI) , 5 , 1 ))  && SCALE.CNT
    laSclArr[lnI,3] = lnCnt
    laSclArr[lnI,4] = lnCnt + VAL(SUBSTR( MLINE(&lcScaleFl..NOTE,lnI) , 5 , 1 )) - 1
    IF laSclArr[lnI,4] >= 13
      laSclArr[lnI,4] = 13
      EXIT
    ENDIF
    lnCnt = lnCnt + VAL(SUBSTR( MLINE(&lcScaleFl..NOTE,lnI) , 5 , 1 ))
  ENDIF
ENDFOR
*-- end of lfSclArr.

*:**************************************************************************
*:* Name        : lfTmpFl 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/02/2004
*:* Purpose     : *-- Creating the temporary file
*:***************************************************************************
*C123615,1
FUNCTION lfTmpFl 
PRIVATE lnNewLen,laFStru

SELECT PosLn
=AFIELDS(laFStru)

*- Remove scale portion from style #
laFStru[ASCAN(laFStru,'STYLE')+2] = 19 - lnSclLen

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'cNotePad'
laFStru[lnNewLen,2] = 'M'
laFStru[lnNewLen,3] = 0
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'nPoTotQty'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 8
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'cCustPo'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 15
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'cCompos'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 100
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'cType'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 1
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'CIMGPATH'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 50
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'cColor'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'cClrDesc'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 30
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'cCustNo'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 15
laFStru[lnNewLen,4] = 0

FOR lnI = 9 TO 13
  lnNewLen = ALEN(laFStru,1) + 1
  DIMENSION laFStru[lnNewLen,4]
  laFStru[lnNewLen,1] = 'QTY'+ALLTRIM(STR(lnI))
  laFStru[lnNewLen,2] = 'N'
  laFStru[lnNewLen,3] = 6
  laFStru[lnNewLen,4] = 0
ENDFOR

FOR lnI = 1 TO 13
  lnNewLen = ALEN(laFStru,1) + 1
  DIMENSION laFStru[lnNewLen,4]
  laFStru[lnNewLen,1] = 'PQTY'+ALLTRIM(STR(lnI))
  laFStru[lnNewLen,2] = 'N'
  laFStru[lnNewLen,3] = 6
  laFStru[lnNewLen,4] = 0
ENDFOR  

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'PTOTQTY'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'CTNQTY'
laFStru[lnNewLen,2] = 'N'
laFStru[lnNewLen,3] = 6
laFStru[lnNewLen,4] = 0

IF ASCAN(laFStru,'PACK_ID')=0
  lnNewLen = ALEN(laFStru,1) + 1
  DIMENSION laFStru[lnNewLen,4]
  laFStru[lnNewLen,1] = 'PACK_ID'
  laFStru[lnNewLen,2] = 'C'
  laFStru[lnNewLen,3] = 16
  laFStru[lnNewLen,4] = 0
ENDIF  

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'CQTYCTN'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 30
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'EMAIL'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 50
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'gObject'
laFStru[lnNewLen,2] = 'G'
laFStru[lnNewLen,3] = 10
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'CBN'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 30
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'CBN2'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 30
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'CHT'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 30
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'CCOMC1'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 30
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'CCOMC2'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 30
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'CSIZETAB'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 30
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'CFLAGLABEL'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 30
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'HT2'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 30
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'SLSNOTEPAD'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 100
laFStru[lnNewLen,4] = 0

lnNewLen = ALEN(laFStru,1) + 1
DIMENSION laFStru[lnNewLen,4]
laFStru[lnNewLen,1] = 'OBJTYP'
laFStru[lnNewLen,2] = 'C'
laFStru[lnNewLen,3] = 3
laFStru[lnNewLen,4] = 0

=gfCrtTmp(lcTmpPolns,@laFStru,'cstytype+po+cType+style+STR(lineno,6)+trancd+OBJTYP',lcTmpPolns)
*-- end of lfTmpFl .

*:**************************************************************************
*:* Name        : lfOpnFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/02/2004
*:* Purpose     : *-- Open some needed files.
*:***************************************************************************
*C123615,1
FUNCTION lfOpnFls
PRIVATE lnSlct
lnSlct = SELECT()

IF !USED('SYUUSER')
  =gfOpenFile(gcSysHome+'SYUUSER','CUSER_ID','SH')
ENDIF
IF !USED('CUTPICK')
  =gfOpenFile(gcDataDir+'CUTPICK','CUTPKORD','SH')
ENDIF
IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
ENDIF
IF !USED('ORDLINE')
  =gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
ENDIF
IF !USED('SPCK_LIN')
  =gfOpenFile(gcDataDir+'SPCK_LIN','SPCK_LIN','SH')
ENDIF

SELECT ORDLINE
SET ORDER TO ORDLINE
SELECT CUTPICK
SET ORDER TO CUTPKORD

SELECT(lnSlct)
*-- end of lfOpnFls.

*:**************************************************************************
*:* Name        : lfGetRatio
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/07/2004
*:* Purpose     : Get ratios of a set of numbers ( here m.qty1,...,m.qty13)
*:***************************************************************************
*C123615,1
FUNCTION lfGetRatio
PRIVATE lnMinQ,lnI
*- Get Minimum qty for this color
lnMinQ = 0
FOR lnI = 1 TO 13
  lcI = ALLTRIM(STR(lnI))
  IF M.QTY&lcI>0
    lnMinQ = IIF(lnMinQ=0,M.QTY&lcI,MIN(lnMinQ,M.QTY&lcI))
  ENDIF
ENDFOR        

*- Get carton ratio based on the minimum qty
FOR lnM = lnMinQ TO 1 STEP -1
  IF MOD(lnMinQ,lnM) > 0 && if not divisible , do not use
    LOOP
  ENDIF

  lnRem = 0
  FOR lnI = 1 TO 13
    lcI = ALLTRIM(STR(lnI))
    lnRem = lnRem + MOD(M.QTY&lcI,lnM)
  ENDFOR
  IF lnRem = 0
    EXIT
  ENDIF
ENDFOR
lnMxCmnDiv = lnM  && This is the maximum common divisor
FOR lnI = 1 TO 13
  lcI = ALLTRIM(STR(lnI))
  M.PQTY&lcI = M.QTY&lcI/lnMxCmnDiv
ENDFOR

*-- end of lfGetRatio.
