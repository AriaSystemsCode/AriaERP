*:***************************************************************************
*: Program file  : MFOPMGNG.PRG
*: Program desc. : Custom Operation Managment form for ANG10
*: TRACK NO      : C201289(Aria4),C201287(Aria27)
*: System        : Aria4XP
*: Module        : MF
*: Developer     : Mariam Mazhar(MMT)
*:***************************************************************************
* Modifications:
* B610296,1 HIA 04/04/2013 Aria XP -T20130220.0014 - Printing issue
*:***************************************************************************
IF !loOgScroll.llOGFltCh
  RETURN
ENDIF
SELECT(lcMainF)
INDEX ON cTktNo+cOprCode+cTarget+cLotNo+SUBSTR(ITEM,1,lnMajSize) TAG 'ItemMajor' ADDITIVE UNIQUE

SELECT(lcMainF)
SET ORDER TO (lcMainF)
SELECT DISTINCT cTktNo+cOprCode+cTarget+cLotNo+SUBSTR(ITEM,1,lnMajSize) AS Disctex FROM;
  (lcMainF) INTO CURSOR 'TotalCurs'

SELECT(lcMainF)
SELECT 'TotalCurs'
SCAN
  lnTotQty = 0
  lnQty1 = 0
  lnQty2 = 0
  lnQty3 = 0
  lnQty4 = 0
  lnQty5 = 0
  lnQty6 = 0
  lnQty7 = 0
  lnQty8 = 0
  lcKeyValue = TotalCurs.Disctex
  SELECT (lcMainF)
  SCAN FOR cTktNo+cOprCode+cTarget+cLotNo+SUBSTR(ITEM,1,lnMajSize)  =  lcKeyValue
    lnTotQty = lnTotQty + nlottotqty
    lnQty1 = lnQty1 + nlotqty1
    lnQty2 = lnQty3 + nlotqty2
    lnQty3 = lnQty3 + nlotqty3
    lnQty4 = lnQty4 + nlotqty4
    lnQty5 = lnQty5 + nlotqty5
    lnQty6 = lnQty6 + nlotqty6
    lnQty7 = lnQty7 + nlotqty7
    lnQty8 = lnQty8 + nlotqty8
  ENDSCAN
  LOCATE
  LOCATE FOR cTktNo+cOprCode+cTarget+cLotNo+SUBSTR(ITEM,1,lnMajSize)  =  lcKeyValue
  IF FOUND()
    REPLACE nlottotqty WITH lnTotQty,;
      nlotqty1   WITH lnQty1 ,;
      nlotqty2   WITH lnQty2 ,;
      nlotqty3   WITH lnQty3 ,;
      nlotqty4   WITH lnQty4 ,;
      nlotqty5   WITH lnQty5 ,;
      nlotqty6   WITH lnQty6 ,;
      nlotqty7   WITH lnQty7 ,;
      nlotqty8   WITH lnQty8 IN (lcMainF)
  ENDIF
ENDSCAN
SELECT(lcMainF)
SET ORDER TO 'ItemMajor'
LOCATE






IF llRpSOAll
  DECLARE laItemSeg[1]
  PRIVATE lnCount ,lnClrLen ,lnClrPos ,lcClrSpr
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='C'
      lnClrLen = LEN(laItemSeg[lnCount,3])
      lnClrPos = laItemSeg[lnCount,4]
      lcClrSpr = ALLT(laItemSeg[lnCount,6])
      EXIT
    ENDIF
  ENDFOR

  IF !USED('Ordline')
    =gfOpenTable('Ordline','Ordline','SH')
  ENDIF

  IF !USED('CUTPICK')
    =gfOpenTable('CUTPICK','CUTPICK','SH')
  ENDIF

  IF !USED('ORDHDR')
    =gfOpenTable('ORDHDR','ORDHDR','SH')
  ENDIF

  IF llRpSONot
    DIMENSION laSoAllocarr[19,4]
  ELSE
    DIMENSION laSoAllocarr[18,4]
  ENDIF
  laSoAllocarr[1,1] = 'Order'
  laSoAllocarr[1,2] = 'C'
  laSoAllocarr[1,3] = 6
  laSoAllocarr[1,4] = 0

  laSoAllocarr[2,1] = 'Account'
  laSoAllocarr[2,2] = 'C'
  laSoAllocarr[2,3] = 5
  laSoAllocarr[2,4] = 0

  laSoAllocarr[3,1] = 'Complete'
  laSoAllocarr[3,2] = 'D'
  laSoAllocarr[3,3] = 8
  laSoAllocarr[3,4] = 0

  laSoAllocarr[4,1] = 'Qty1'
  laSoAllocarr[4,2] = 'N'
  laSoAllocarr[4,3] = 11
  laSoAllocarr[4,4] = 0

  laSoAllocarr[5,1] = 'Qty2'
  laSoAllocarr[5,2] = 'N'
  laSoAllocarr[5,3] = 11
  laSoAllocarr[5,4] = 0

  laSoAllocarr[6,1] = 'Qty3'
  laSoAllocarr[6,2] = 'N'
  laSoAllocarr[6,3] = 11
  laSoAllocarr[6,4] = 0

  laSoAllocarr[7,1] = 'Qty4'
  laSoAllocarr[7,2] = 'N'
  laSoAllocarr[7,3] = 11
  laSoAllocarr[7,4] = 0

  laSoAllocarr[8,1] = 'Qty5'
  laSoAllocarr[8,2] = 'N'
  laSoAllocarr[8,3] = 11
  laSoAllocarr[8,4] = 0

  laSoAllocarr[9,1] = 'Qty6'
  laSoAllocarr[9,2] = 'N'
  laSoAllocarr[9,3] = 11
  laSoAllocarr[9,4] = 0

  laSoAllocarr[10,1] = 'Qty7'
  laSoAllocarr[10,2] = 'N'
  laSoAllocarr[10,3] = 11
  laSoAllocarr[10,4] = 0

  laSoAllocarr[11,1] = 'Qty8'
  laSoAllocarr[11,2] = 'N'
  laSoAllocarr[11,3] = 11
  laSoAllocarr[11,4] = 0

  laSoAllocarr[12,1] = 'TotQty'
  laSoAllocarr[12,2] = 'N'
  laSoAllocarr[12,3] = 12
  laSoAllocarr[12,4] = 0

  laSoAllocarr[13,1] = 'Color'
  laSoAllocarr[13,2] = 'C'
  laSoAllocarr[13,3] = 6
  laSoAllocarr[13,4] = 0

  laSoAllocarr[14,1] = 'cStyMajor'
  laSoAllocarr[14,2] = 'C'
  laSoAllocarr[14,3] = lnMajSize
  laSoAllocarr[14,4] = 0

  laSoAllocarr[15,1] = 'cTktNo'
  laSoAllocarr[15,2] = 'C'
  laSoAllocarr[15,3] = 6
  laSoAllocarr[15,4] = 0

  laSoAllocarr[16,1] = 'cTarget'
  laSoAllocarr[16,2] = 'C'
  laSoAllocarr[16,3] = 6
  laSoAllocarr[16,4] = 0

  laSoAllocarr[17,1] = 'COPRCODE'
  laSoAllocarr[17,2] = 'C'
  laSoAllocarr[17,3] = 6
  laSoAllocarr[17,4] = 0

  laSoAllocarr[18,1] = 'cLotNo'
  laSoAllocarr[18,2] = 'C'
  laSoAllocarr[18,3] = 2
  laSoAllocarr[18,4] = 0

  IF llRpSONot
    laSoAllocarr[19,1] = 'note_mem'
    laSoAllocarr[19,2] = 'M'
    laSoAllocarr[19,3] = 10
    laSoAllocarr[19,4] = 0
  ENDIF

  = gfCrtTmp(lcMainNG,@laSoAllocarr,"cTktNo+cOprCode+cTarget+cLotNo+cStyMajor+Color+Order" ,lcMainNG,.F.)

  SELECT (lcMainF)
  SCAN
    SCATTER MEMO MEMVAR
    m.cStyMajor = SUBSTR(&lcMainF..ITEM,1,lnMajSize)
    m.cTktNo    = &lcMainF..cTktNo
    SELECT CUTPICK
    lcTrancd = IIF(oariaapplication.activemoduleid = 'PO','2','1')
    =gfSqlRun("Select * FROM CUTPICK WHERE ctktNo = '"+&lcMainF..ctktNo+"' AND TRANCD ='"+lcTrancd+"' and Style like '"+SUBSTR(&lcMainF..ITEM,1,lnMajSize)+"%'",'CUTPICK')
    lnTotQty = 0
    lnQty1 = 0
    lnQty2 = 0
    lnQty3 = 0
    lnQty4 = 0
    lnQty5 = 0
    lnQty6 = 0
    lnQty7 = 0
    lnQty8 = 0
    SCAN
      SCATTER MEMO MEMVAR
      =gfSeek('O'+m.Order,'ORDHDR','ORDHDR')
      m.account = Ordhdr.Account
      m.Complete = Ordhdr.COMPLETE
      m.Color = SUBSTR(m.Style,lnClrPos,lnClrLen)
      IF llRpSONot
        =gfSeek('O'+m.ORDER+m.CORDLINE,'Ordline')
        m.note_mem = Ordline.note_mem
      ENDIF
      IF !SEEK(m.cTktNo+m.cOprCode+m.cTarget+m.cLotNo+m.cStyMajor+m.Color+m.Order,lcMainNG)
        INSERT INTO (lcMainNG) FROM MEMVAR
        * B610296,1 HIA 04/04/2013 Aria XP -T20130220.0014 - Printing issue [Start]
      ELSE
        REPLACE &lcMainNG..Qty1   WITH m.qty1
        REPLACE &lcMainNG..Qty2   WITH m.qty2
        REPLACE &lcMainNG..Qty3   WITH m.qty3
        REPLACE &lcMainNG..Qty4   WITH m.qty4
        REPLACE &lcMainNG..Qty5   WITH m.qty5
        REPLACE &lcMainNG..Qty6   WITH m.qty6
        REPLACE &lcMainNG..Qty7   WITH m.qty7
        REPLACE &lcMainNG..Qty8   WITH m.qty8
        REPLACE &lcMainNG..totqty WITH m.totqty

        * B610296,1 HIA 04/04/2013 Aria XP -T20130220.0014 - Printing issue [End]
      ENDIF
    ENDSCAN
  ENDSCAN
  SELECT(lcMainF)
  SET RELATION TO CTKTNO+cOprCode+cTarget+cLotNo+SUBSTR(ITEM,1,lnMajSize) INTO (lcMainNG) ADDITIVE
  SET SKIP TO  (lcMainNG)
ENDIF
SELECT(lcMainF)
SET ORDER TO 'ItemMajor'
LOCATE

*!*************************************************************
*! Name      : lfgetStatus
*: Developer : Mariam Mazhar (MMT)
*: Date      : 11/10/2010
*! Purpose   : get Cut tkt Status
*!*************************************************************
FUNCTION lfgetStatus
lcCTStyle = EVALUATE(lcMainF+'.ITEM')
lcCStatus = IIF(oariaapplication.activemoduleid = 'MF', CUTTKTH.STATUS,IIF(oariaapplication.activemoduleid  ='PO',POSHDR.STATUS,IIF(oariaapplication.activemoduleid ='MA',MMFGORDH.STATUS,'')))
RETURN  IIF(lcCStatus = 'O','Open',IIF(lcCStatus = 'C','Complete',IIF(lcCStatus = 'A','Actualized','Closed')))

*!*************************************************************
*! Name      : lfImgFile
*: Developer : Mariam Mazhar (MMT)
*: Date      : 11/10/2010
*! Purpose   : Adjust Variable to seek for image in obejct file
*!*************************************************************
FUNCTION lfImgFile
IF !EMPTY(EVALUATE(lcMainF+'.ITEM'))
  lcCTStyle = EVALUATE(lcMainF+'.ITEM')
ENDIF
RETURN .T.
