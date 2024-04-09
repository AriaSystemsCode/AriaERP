
*:***************************************************************************
*: Program file  : probrcdbf.PRG   
*: Program desc. : CUSTOM Bar code preview For Blum & Fink  
*: For Report    : probrcdbf.FRX
*: System        : Aria Advantage Series 4XP.
*: Module        : Style purchaes order (po)
*: Developer     : Mostafa Eid(MOS)
*: Date          : 04/07/2008
*: FIX NUMBER    : C200997
******************************************************************************

*-- DECLARATION PART 
 lcTmpLbl = gfTempName()
*- Create the temp file needed to collect data
=lfCrtTmp()

*---body part
*--collect data
=lfCollData()

SELECT (lcTmpLbl)

*--functions part

*!*************************************************************
*! Name      : lfPrintLbl
*! Developer : Mostafa Eid
*! Date      : 05/09/2008
*! Purpose   : To print labels
*!*************************************************************

FUNCTION lfCollData 

STORE 0 TO lnClrLen,lnClrStPos

DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen   = LEN(laItemSeg[lnCount,3])
    lnClrStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR  

IF !USED('StyleUpc')
  =gfOpenTable(oAriaApplication.DataDir+'StyleUpc','STYLEUPC','SH')
ENDIF

SELECT (lcTmpBars)
LOCATE

SCAN

SCATTER MEMO MEMVAR 
  
  SELECT (lcTmpLbl) 
  *--add all neded records
  
  =SEEK(&lcTmpBars..cCode,'StyleUpc')

  *--first bulk 
  APPEND BLANK
  REPLACE cStyle  WITH  &lcTmpBars..cCode  ;
          Ccolor  WITH  gfCodDes(SUBSTR(&lcTmpBars..cCode,lnClrStPos,lnClrLen),'COLOR')  ;
          cSize   WITH  &lcTmpBars..SizeDesc ;
          cFabNam WITH  Style.cFabNam ;
          cFurOrg WITH  style.cFurOrg ;
          cUPC    WITH  StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3 
          
  =OLEBarCode(cUPC)  

  =SEEK(&lcTmpBars..cCode1,'StyleUpc')
  
  *--second bulk 
  APPEND BLANK
  REPLACE cStyle  WITH  &lcTmpBars..cCode1 ;
          Ccolor  WITH  gfCodDes(SUBSTR(&lcTmpBars..cCode1,lnClrStPos,lnClrLen),'COLOR')  ;
          cSize   WITH  &lcTmpBars..SizeDesc1;
          cFabNam WITH  Style.cFabNam ;
          cFurOrg WITH  style.cFurOrg ; 
          cUPC    WITH  StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3 
  =OLEBarCode(cUPC)
  
  =SEEK(&lcTmpBars..cCode2,'StyleUpc')
  
  *--third bulk 
  APPEND BLANK
  REPLACE cStyle  WITH  &lcTmpBars..cCode2 ;
          Ccolor  WITH  gfCodDes(SUBSTR(&lcTmpBars..cCode2,lnClrStPos,lnClrLen),'COLOR')  ;
          cSize   WITH  &lcTmpBars..SizeDesc2 ;
          cFabNam WITH  Style.cFabNam ;
          cFurOrg WITH  style.cFurOrg ;
          cUPC    WITH  StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3 
  =OLEBarCode(cUPC)
  
ENDSCAN

*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : MOS - MOSTAFA EID
*! Date      : 05/08/2008
*! Purpose   : < Create temp file as astructure of lcTmpLbl table >
*!*************************************************************
FUNCTION lfCrtTmp

PRIVATE laFileStru

laFileStru = ''
DIMENSION laFileStru[7,18]
laFileStru[1,1] = 'cStyle'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 12
laFileStru[1,4] = 0

laFileStru[2,1] = 'cColor'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 20
laFileStru[2,4] = 0

laFileStru[3,1] = 'cSize'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 5
laFileStru[3,4] = 0

laFileStru[4,1] = 'cUPC'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 13
laFileStru[4,4] = 0

laFileStru[5,1] = 'cFabNam'
laFileStru[5,2] = 'C'
laFileStru[5,3] = 60
laFileStru[5,4] = 0

laFileStru[6,1] = 'cFurOrg'
laFileStru[6,2] = 'C'
laFileStru[6,3] = 20
laFileStru[6,4] = 0

laFileStru[7,1] = 'upcBar'
laFileStru[7,2] = 'G'
laFileStru[7,3] = 10
laFileStru[7,4] = 0

lnCount = 0
FOR lnCount = 1 TO 7
  STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
              laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
              laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
              laFileStru[lnCount,16]
  STORE 0 TO  laFileStru[lnCount,17],laFileStru[lnCount,18]
ENDFOR

=GfCrtTmp(lcTmpLbl,@laFileStru)


************************************************************************
** OLEBarCode 
** Create the bitmap used to fill the general field upcBar 
************************************************************************
PROCEDURE OLEBarCode 
PARAMETERS  tcCUPC

IF EMPTY(tcCUPC)
  RETURN
ENDIF

lcF_name = UPPER('UPCBAR')
oForm = loOgScroll.Parent
WITH oForm
  IF TYPE('oForm.'+ lcF_name) <> 'O'
    .ADDOBJECT(lcF_name,"OLEBoundControl")
  ENDIF
  .&lcF_name..CONTROLSOURCE = lcF_name
  .&lcF_name..WIDTH         = 150
  .&lcF_name..HEIGHT        = 150
   APPEND GENERAL &lcF_name. CLASS ("IDAuto.BarCode")
  .&lcF_name..REFRESH
  .&lcF_name..OBJECT.DataToEncode = tcCUPC && EVALUATE(SYCUPCDT.F_VALUE) The CUPC #
  .&lcF_name..OBJECT.Barheight    = 1
  .&lcF_name..OBJECT.SymbologyID  = 6
  .&lcF_name..OBJECT.showtext     = 2
  .&lcF_name..OBJECT.NarrowBarWidth = 0.03
  *adding Orientation to the barcode
  * Values are 0,90,180
  .&lcF_name..OBJECT.Orientation = 0
  .&lcF_name..OBJECT.TopMarginCm = 0
  
 .&lcF_name..OBJECT.LeftMarginCm = 0.3
  .&lcF_name..OBJECT.BACKCOLOR = RGB(255,255,255)
  .&lcF_name..OBJECT.FORECOLOR = RGB(0,0,0)
    
ENDWITH
