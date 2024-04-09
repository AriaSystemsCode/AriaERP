*!****************************************************************************
*! Program file        : ALPKLSBR.PRG   
*! Program desc.       : Customized Packing List Form For BROADWAY
*! System              : Aria Advantage Series VER. 2.7
*! Module              : Sales Oredr Allocation - (AL)
*! Developer           : NADER NABIL ABD ELMONEM (NNA)
*! Tracking Job Number : C127839
*! Date                : 08/25/2005
*!****************************************************************************
*! Calls :  Functions  : lfCrtTemp - lfGetData - lfGetSblCO - lfvGtSclSp - lfFacCode
*!                     : lfAdrShift - lfGetSizes - lfTotPages - lfGetCode
*!****************************************************************************
*! Called From         : System Menu (AL --> Output --> Picking Ticket)
*!****************************************************************************
*! Passed Parameters   : None
*!****************************************************************************
*! Example             : DO ALPKLSBR
*!****************************************************************************
*! Modification        :
*!****************************************************************************
PRIVATE lcCrtnTmp,llUsedSpck,lcOldOrder
llUsedSpck = .F.
lcOldOrder =''
lcCrtnTmp = gfTempName()
IF !USED('STYLEUPC')
  =gfOpenFile(gcDataDir+'STYLEUPC',gcDataDir+'STYLEUPC','SH')
ENDIF
IF !USED('SPCK_HDR')
  =gfOpenFile(gcDataDir+'SPCK_HDR',gcDataDir+'SKU_STYLE','SH')
ELSE
  llUsedSpck =.T.
  lcOldOrder = ORDER('SPCK_HDR')
  SET ORDER TO TAG SKU_STYLE INTO SPCK_HDR
ENDIF
=lfCrtTemp()
=lfCollect()
=lfSetRela()
SELECT(lcPackTmp)
DO gfDispRe WITH EVAL('lcFormName')
llALPakLst =.F.
IF USED('STYLEUPC')
  USE IN STYLEUPC
ENDIF
IF USED(lcPrntTmp)
  USE IN (lcPrntTmp)
  ERASE (gcWorkDir+lcPrntTmp+".DBF")
  ERASE (gcWorkDir+lcPrntTmp+".CDX")
ENDIF
IF USED(lcTotalTmp)
  USE IN (lcTotalTmp)
  ERASE (gcWorkDir+lcTotalTmp+".DBF")
  ERASE (gcWorkDir+lcTotalTmp+".CDX")
ENDIF
IF USED('SPCK_HDR')
  IF llUsedSpck
    SET ORDER TO &lcOldOrder INTO SPCK_HDR
  ELSE
    USE IN SPCK_HDR
  ENDIF
ENDIF
*--END OF THE REPORT
*!*************************************************************************
*!* Name        : lfCrtTemp
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 08/28/2005
*!* Purpose     : Create temp file to collectiong data
*!***************************************************************************
*!* Called from : ALPKSBR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCrtTemp()
*!***************************************************************************
FUNCTION lfCrtTemp
SELECT(lcPaklnTmp)
DIMENSION laFileStru[1,4]
lnFldLen = AFIELDS(laFileStru)
DIMENSION laFileStru[lnFldLen + 9 , 4]
laFileStru[lnFldLen + 1 , 1] = 'S1'
laFileStru[lnFldLen + 1 , 2] = 'N'
laFileStru[lnFldLen + 1 , 3] = 6
laFileStru[lnFldLen + 1 , 4] = 0

laFileStru[lnFldLen + 2 , 1] = 'S2'
laFileStru[lnFldLen + 2 , 2] = 'N'
laFileStru[lnFldLen + 2 , 3] = 6
laFileStru[lnFldLen + 2 , 4] = 0

laFileStru[lnFldLen + 3 , 1] = 'S3'
laFileStru[lnFldLen + 3 , 2] = 'N'
laFileStru[lnFldLen + 3 , 3] = 6
laFileStru[lnFldLen + 3 , 4] = 0

laFileStru[lnFldLen + 4 , 1] = 'S4'
laFileStru[lnFldLen + 4 , 2] = 'N'
laFileStru[lnFldLen + 4 , 3] = 6
laFileStru[lnFldLen + 4 , 4] = 0

laFileStru[lnFldLen + 5 , 1] = 'S5'
laFileStru[lnFldLen + 5 , 2] = 'N'
laFileStru[lnFldLen + 5 , 3] = 6
laFileStru[lnFldLen + 5 , 4] = 0

laFileStru[lnFldLen + 6 , 1] = 'S6'
laFileStru[lnFldLen + 6 , 2] = 'N'
laFileStru[lnFldLen + 6 , 3] = 6
laFileStru[lnFldLen + 6 , 4] = 0

laFileStru[lnFldLen + 7 , 1] = 'S7'
laFileStru[lnFldLen + 7 , 2] = 'N'
laFileStru[lnFldLen + 7 , 3] = 6
laFileStru[lnFldLen + 7 , 4] = 0

laFileStru[lnFldLen + 8 , 1] = 'S8'
laFileStru[lnFldLen + 8 , 2] = 'N'
laFileStru[lnFldLen + 8 , 3] = 6
laFileStru[lnFldLen + 8 , 4] = 0

laFileStru[lnFldLen + 9 , 1] = 'ACCOUNT'
laFileStru[lnFldLen + 9 , 2] = 'C'
laFileStru[lnFldLen + 9 , 3] = 5
laFileStru[lnFldLen + 9 , 4] = 0

CREATE CURSOR (lcCrtnTmp) FROM ARRAY laFileStru
INDEX ON PACK_NO+STYLE TAG (lcCrtnTmp) OF (lcCrtnTmp)

CREATE TABLE (gcWorkDir + lcPrntTmp)(PACK_NO C(6),SHOPKOITEM C(8),STYLE C(8),DESC1 C(60),SIZ_NO C(1),;
       SHOPKOMFG C(10),COLOR C(6),SCALE C(3),QTY1 N(5),QTY2 N(5),QTY3 N(5),QTY4 N(5),QTY5 N(5),QTY6 N(5),QTY7 N(5),;
       QTY8 N(5),TOT_P_CRTN N(5),TOT_CRTNS N(5),UPC C(14),TOT_UNITS N(7))
INDEX ON PACK_NO + STYLE + SIZ_NO TAG (lcPrntTmp) OF (lcPrntTmp)

*-- End of Function lfCrtTemp.
*!*************************************************************************
*!* Name        : lfCollect
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 08/28/2005
*!* Purpose     : Collecting Data to print it on the Report
*!***************************************************************************
*!* Called from : ALPKLSBR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCollect()
*!***************************************************************************
FUNCTION lfCollect
PRIVATE lnOldAlias,lnTotCrtns,lcStyUpc
STORE 0 TO lnOldAlias,lnTotCrtns
lnOldAlias = SELECT(0)
SELECT(lcPakLnTmp)
SCAN
  SCATTER MEMVAR MEMO
  m.Account = &lcPackTmp..Account
  INSERT INTO (lcCrtnTmp) FROM MEMVAR
ENDSCAN
SELECT (lcCrtnTmp)
GO TOP
replace S1 with IIF(QTY1>0,1,0) ;
        S2 with IIF(QTY2>0,1,0) ;
        S3 with IIF(QTY3>0,1,0) ;
        S4 with IIF(QTY4>0,1,0) ;
        S5 with IIF(QTY5>0,1,0) ;
        S6 with IIF(QTY6>0,1,0) ;
        S7 with IIF(QTY7>0,1,0) ;
        S8 with IIF(QTY8>0,1,0) ALL

TOTAL ON PACK_NO+STYLE+STR(nOrdLineNO,6) TO (gcWorkDir+lcTotalTmp)
USE (gcWorkDir+lcTotalTmp) IN 0 
SELECT(lcTotalTmp)
SCAN
  SELECT (lcPrntTmp)
  FOR I = 1 TO 8
    LCI = ALLTRIM(STR(I))
    lnTotCrtns = IIF(&lcTotalTmp..QTY&LCI>0 AND &lcTotalTmp..S&LCI>0,(&lcTotalTmp..QTY&LCI/&lcTotalTmp..S&LCI),0)
    IF !SEEK(&lcTotalTmp..PACK_NO + &lcTotalTmp..STYLE + LCI) AND &lcTotalTmp..QTY&LCI>0
      APPEND BLANK
      REPLACE  PACK_NO 	      WITH  &lcTotalTmp..PACK_NO                                      ,;
               STYLE          WITH  IIF(SEEK(&lcTotalTmp..STYLE,'STYLE'),STYLE.CSTYMAJOR,SUBSTR(&lcTotalTmp..STYLE,1,8)) ,;
               DESC1          WITH  IIF(SEEK(&lcTotalTmp..STYLE,'STYLE'),STYLE.DESC1,'')      ,;
               SHOPKOMFG      WITH  IIF(SEEK(&lcTotalTmp..STYLE,'STYLE'),STYLE.CSHOPKOMFG,'') ,;
               SIZ_NO         WITH  LCI                                                       ,;
               COLOR          WITH  SUBSTR(&lcTotalTmp..STYLE,lnClrStart,lnClrLen)	          ,;
               SCALE          WITH  IIF(SEEK(&lcTotalTmp..STYLE,'STYLE'),STYLE.SCALE,'')      ,;
               QTY&LCI        WITH  &lcTotalTmp..QTY&LCI                                      ,;
               TOT_P_CRTN     WITH  lnTotCrtns                                                ,;
               TOT_CRTNS      WITH  &lcTotalTmp..S&LCI                                        ,;
               TOT_UNITS      WITH  (QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8)
      IF SEEK(&lcTotalTmp..STYLE + LCI,'STYLEUPC')
        SELECT STYLEUPC
        lcStyUpc = STYLEUPC.cUpcNum1+STYLEUPC.cUpcNum2+ALLTRIM(STYLEUPC.cUpcNum3)
      ELSE
        lcStyUpc = ''
      ENDIF
      SELECT (lcPrntTmp)
      REPLACE UPC           WITH lcStyUpc
      IF SEEK('S'+&lcTotalTmp..Account+&lcTotalTmp..STYLE,'SPCK_LIN')
        SELECT SPCK_LIN
        SCAN REST WHILE Type+Account+Style+Pack_id = 'S'+&lcTotalTmp..Account+&lcTotalTmp..STYLE FOR SPCK_LIN.QTY&LCI >0
          SELECT (lcPrntTmp)
          REPLACE SHOPKOITEM  WITH SUBSTR(SPCK_LIN.PACK_ID,1,8)
        ENDSCAN
        SELECT (lcPrntTmp)
      ENDIF
    ENDIF
  ENDFOR
ENDSCAN
SELECT(lnOldAlias)

*-- End of Function lfCollect.
*!*************************************************************************
*!* Name        : lfCompAdr
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 08/28/2005
*!* Purpose     : used to shift the lashipto array
*!***************************************************************************
*!* Called from : ALPKLSBR.PRG
*!***************************************************************************
*!* Parameters  : lcArrayNam : Array hold The address.
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAdrShift()
*!***************************************************************************
FUNCTION lfCompAdr
PARAMETERS lcDummy
lnOldAlias = ALIAS()
laCompAdd = ''           && Array to hold the Company address
*-- Get company Address [begin].
SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name             && Company Name.
lcCompPhon = cCom_Phon             && Company Phone.
lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.
= gfGetAdr('SYCCOMP' , '' , '' , '' , @laCompAdd)
DIMENSION laCompAdd[6,1]
laCompAdd[6,1] = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)
= lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
SELECT(lnOldAlias)
RETURN ''
*-- End Of lfCompAdr
*!*************************************************************************
*!* Name        : lfAdrShift
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 08/28/2005
*!* Purpose     : used to shift the lashipto array
*!***************************************************************************
*!* Called from : ALPKLSBR.PRG
*!***************************************************************************
*!* Parameters  : lcArrayNam : Array hold The address.
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAdrShift()
*!***************************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam
*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  *--IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *--IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End Of lfAdrShift
*!*************************************************************************
*!* Name        : lfSetRela
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 08/28/2005
*!* Purpose     : Set relation between files
*!***************************************************************************
*!* Called from : ALPKLSBR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSetRela()
*!***************************************************************************
FUNCTION lfSetRela
SELECT INVHDR 
SET RELATION TO INVOICE INTO INVLINE
SELECT PACK_HDR
SET RELATION TO PACK_NO INTO (lcPrntTmp) 
SELECT STYLE
SET RELATION TO  "S" + scale  INTO Scale 
SELECT(lcPackTmp)
SET RELATION TO
SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO CUSTOMER ,;
                PACK_NO               INTO PACK_HDR   ,; 
                INVOICE               INTO INVHDR     ,; 
                PikTkt				  INTO PIKTKT	  ,;
                "O" + ORDER           INTO ORDHDR     ,;
                "O" + ORDER + STORE   INTO ORDLINE    ,;
                "B" + ORDER           INTO NOTEPAD               

lcSkipExpr = IIF(EVAL(lcPackTmp+'.nRprtTyp')=1,"INVLINE",IIF(EVAL(lcPackTmp+'.nRprtTyp')=2,lcPrntTmp,"ORDLINE"))
SET SKIP TO &lcSkipExpr
IF EVAL(lcPackTmp+'.nRprtTyp')=1
  SELECT INVLINE
  SET RELATION OFF INTO STYLE
  SET RELATION OFF INTO SPCK_HDR
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SET RELATION TO 'S'+ ACCOUNT+ STYLE INTO SPCK_HDR ADDITIVE
ENDIF
IF EVAL(lcPackTmp+'.nRprtTyp')=2
  SELECT (lcPrntTmp)
  SET RELATION OFF INTO STYLE
  SET RELATION TO STYLE INTO STYLE ADDITIVE  
ENDIF
IF EVAL(lcPackTmp+'.nRprtTyp')=3
  SELECT ORDLINE
  SET RELATION OFF INTO STYLE
  SET RELATION OFF INTO SPCK_HDR
  SET RELATION TO STYLE INTO STYLE ADDITIVE  
  SET RELATION TO 'S'+ ACCOUNT+ STYLE INTO SPCK_HDR ADDITIVE
ENDIF
*-- End of Function lfSetRela.