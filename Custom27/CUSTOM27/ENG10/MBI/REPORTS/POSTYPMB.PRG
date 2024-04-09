*:***************************************************************************
*: Program file  : POSTYPMB.PRG
*: Program desc. : CUSTOMIZED PURCHASE ORDER FOR MBI.
*: Date          : 02/20/2000
*: System        : Aria Advantage Series.
*: Module        : STYLE PURCHASE ORDER (PO)
*: Developer     : (AMH) Ahmed Maher
*: Tracking Job Number: C200163
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POSTYPMB
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*B604332,1 KHM 03/29/2001 Printing the second dimension of the scale and 
*B604332,1                make some modifications in the FRX,
*:***************************************************************************
*--Section break the relation and skip to.
SELECT PosHdr
SET SKIP TO

*-- Creating the temporary file and its index.
SELECT PosLn
=AFIELDS(laFStru)
DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cBtName'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 30
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cCustPo'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 15
laFStru[ALEN(laFStru,1),4] = 0

*B604332,1 KHM 03/29/2001 (Begin) increasing the width of the cSize.
*DIMENSION laFStru[ALEN(laFStru,1)+1,4]
*laFStru[ALEN(laFStru,1),1] = 'cSize'
*laFStru[ALEN(laFStru,1),2] = 'C'
*laFStru[ALEN(laFStru,1),3] = 5
*laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cSize'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 6
laFStru[ALEN(laFStru,1),4] = 0
*B604332,1 KHM 03/29/2001 (End)

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cPack_id'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 16
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cCurrSymb'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 3
laFStru[ALEN(laFStru,1),4] = 0

=gfCrtTmp(lcTmpPolns,@laFStru,'cstytype+PO+cWareCode+account+store+LEFT(STYLE,lnMajSize)',lcTmpPolns)
= gfOpenFile(gcDataDir+'CUTPICK','CUTPICK','SH')
= gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
= gfOpenFile(gcDataDir+'SPCK_LIN','SPCKLINS','SH')
llSycCurr = .F.
IF !USED('SYCCURR')
  llSycCurr = gfOpenFile(gcDataDir+'SycCurr','SycCurr','SH')
ENDIF
*--Section of collecting the data.
SELECT PosHdr
SCAN FOR &lcRpExp
  lcCurrSymb = ''
  IF SEEK(cStyType+Po,'PosLn')
    =SEEK(PosHdr.cPriceCur,'SycCurr')
    lcCurrSymb = SycCurr.cCurrSmbl
    STORE SPACE(0) TO lcCustName, lcCustPo
    SELECT PosLn
    IF SEEK('2'+PO,'CUTPICK')
      IF SEEK('O'+CUTPICK.ORDER,'ORDHDR')
        lcCustPo = ORDHDR.CUSTPO
        IF OrdHdr.Multi = 'Y'
          =SEEK('S'+OrdHdr.Account,'Customer')
        ELSE
          =SEEK('M'+OrdHdr.Account,'Customer')
        ENDIF
        lcCustName = CUSTOMER.BTNAME
      ENDIF
    ENDIF
    SCAN REST WHILE cstytype+PO+cWareCode+Account+Store+Style+TranCd = ;
                    PosHdr.cStyType+PosHdr.PO 
      SCATTER MEMVAR MEMO
      IF SEEK('S'+SCALE,'SCALE')
        FOR lnI = 1 TO SCALE.CNT
          lcI = STR(lnI,1)
          IF EVALUATE('m.Qty'+lcI) > 0
            INSERT INTO (lcTmpPolns) FROM MEMVAR
            *B604332,1 KHM 03/29/2001 (Begin) Adding the second dimension
            *B604332,1 KHM 03/29/2001 of the scale.
            *REPLACE (lcTmpPolns+'.CSIZE') WITH EVALUATE('SCALE.SZ'+lcI),;
                    (lcTmpPolns+'.QTY1')  WITH EVALUATE('m.Qty'+lcI),;
                    (lcTmpPolns+'.CBTNAME') WITH lcCustName,;
                    (lcTmpPolns+'.CCUSTPO') WITH lcCustPo
            REPLACE (lcTmpPolns+'.CSIZE') WITH ALLTRIM(SCALE.cDim1)+ALLTRIM(EVALUATE('SCALE.SZ'+lcI)),;
                    (lcTmpPolns+'.QTY1')  WITH EVALUATE('m.Qty'+lcI),;
                    (lcTmpPolns+'.CBTNAME') WITH lcCustName,;
                    (lcTmpPolns+'.CCUSTPO') WITH lcCustPo
            *B604332,1 KHM 03/29/2001 (End)
            
            IF llrpfc AND lcRpPrice = 'C'       
              REPLACE (lcTmpPolns+'.cCurrSymb') WITH lcCurrSymb
            ENDIF  
                    
            IF SEEK('S'+ORDHDR.ACCOUNT+STYLE,'SPCK_LIN')
              SELECT SPCK_LIN
              SCAN REST WHILE Type+Account+Style+Pack_id = 'S'+ORDHDR.ACCOUNT+POSLN.STYLE;
                   FOR EVALUATE('QTY'+lcI) <> 0
                REPLACE (lcTmpPolns+'.CPACK_ID') WITH PACK_ID
              ENDSCAN
              SELECT POSLN
            ENDIF
          ENDIF
        ENDFOR
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN

SELECT PosLn
SET RELATION TO
SELECT PosHdr
SET RELATION TO

SELECT (lcTmpPolns)
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

*SELECT objlink
*SET RELATION off INTO Objects

*SELECT &TMPObjlink
*SET RELATION TO

SELECT PosHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
SET RELATION TO 'P'+PO INTO NOTEPAD ADDITIVE

IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF
IF USED('ORDHDR')
  USE IN ORDHDR
ENDIF
IF USED('SPCK_LIN')
  USE IN SPCK_LIN
ENDIF
IF USED('SYCCURR') AND llSycCurr
  USE IN SycCurr
ENDIF

SELECT (lcTmpPolns)
GOTO TOP
lcRpExp = '.T.'
*!*************************************************************
*! Name      : lfFirstTim
*! Developer : 
*! Date      : 
*! Purpose   : Function to initial the variabls in the first time.
*!*************************************************************
*! Called from : SYREPUVR.DBF
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFirstTim()
*!*************************************************************
FUNCTION lfFirstTim
PARAMETER lcReturn

CREATE CURSOR (lcLogBmp) (gPic G)
APPEND BLANK
APPEND GENERAL gPIC FROM ( gcBmpHome + 'LOGOMB.BMP')

RETURN ''
*-- End of lfFirstTim.
