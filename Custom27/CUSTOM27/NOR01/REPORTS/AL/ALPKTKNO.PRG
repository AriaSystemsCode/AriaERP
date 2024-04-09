*:***************************************************************************
*: Program file  : ALPKTKNO.PRG
*: Program desc. : CUSTOMIZED SO ALLOCATION PICKING TICKET Form FOR NORTH BAY
*: Date          : 26/05/2004
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C122907
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKNO
*:***************************************************************************
*: Modifications :
*: B123482,1 NNA 07/07/2004 Fix some things as follow :
*: B123482,1 NNA            1 - Add the Zip Code to the Label Part of the form (FRX)
*: B123482,1 NNA            2 - Add the customer name to the Sold To Field (FRX)
*: B123482,1 NNA            3 - Leave a line after the style major changes (FRX)
*: B123482,1 NNA            4 - Let the program print in a regular way in case the scale has one dimension scale 
*: B123482,1 NNA                else Reverse the way (PRG)
*: B123880,1 ASH 08/04/2004 Fix the bug of printed the ordered qty instead of the picked.
*: B124826,1 NNA 10/31/2004 Print the Order NotePad
*: C126785,1 EIH 04/30/2005 Custom Sort by Regions and State
*: E128455,1 EIH 06/19/2005 Make Custom Prg# 126785 work on the customer 'W149A' "WALMART" only.
*: B129061,1 EIH 08/02/2005 Delete Custom Prg# 126785 and Enhancment# E128455.
*: B131211,1 MMR 02/09/2006 Fix the bug of printed the ordered qty instead of the picked.
***************************************************************************
STORE '' TO lcOldScale , lcLvl , lnLvl
IF !USED(lcSpckHdr)
  = gfOpenFile(gcDataDir + 'SPCK_HDR' , gcDataDir + 'SKU_STYLE', 'SH', @lcSpckHdr, .T.)
ENDIF

IF !USED(lcWhsLoc)
  = gfOpenFile(gcDataDir + 'WHSLOC' , gcDataDir + 'WHSLOCST', 'SH', @lcWhsLoc, .T.)
ENDIF

*B123482,1 NNA 07/07/2004 (Begin) Open the ScaleHD File To Check if the scale one or two Dim.
IF !USED('SCALEHD')
  = gfOpenFile(gcDataDir + 'SCALEHD' , gcDataDir + 'EXTSCALE', 'SH')
ENDIF

=lfChngScPO() && Change The Scale Sizes Position From Vertical to Horizontal

STORE '' TO lcOldScale
USE IN (lcTmpLvls)
USE IN (lcTmpOrdL)
USE (gcWorkDir+(lcTmpLvls))  ALIAS (lcTmpOrdL)
SET ORDER TO TAG (lcTmpOrdL)
*--Set the relation to the new file.
SELECT (lcTmpOrdL)
LOCATE
IF EOF(lcTmpOrdL)
  llNoRec = .T.
  llAlpktk = .F.  
  *-- Message <No records to display.>
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*B129061,1 EIH 08/02/2005 Delete Custom Prg# 126785 and Enhancment# E128455 [Begin].
*C126785,1 EIH 04/30/2005 Custom Sort by Regions and State we add 2 fields cregion and corderno and new index.[Begin]
*SET ORDER TO TAG lcRegIDX
*C126785,1 EIH 04/30/2005 [End]
*B129061,1 EIH 08/02/2005 [End].
LOCATE
SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO "S" + ORDHDR.ACCOUNT + LEFT(Style,lnLenthM1) + "-" + SUBSTR(Style,lnClrPosM1,lnClrLnM1) INTO (lcSpckHdr) ADDITIVE
SET RELATION TO LEFT(Style,lnLenthM1) + "-" + SUBSTR(Style,lnClrPosM1,lnClrLnM1) INTO (lcWhsLoc) ADDITIVE
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE

*--IF We are to Print Order Lines Note Pad
IF llRpOrdLNt
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
ENDIF    && End of IF

SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
DO gfDispRe WITH EVAL('lcFormName')
SET DEVICE TO SCREEN
llAlpktk = .F.

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfInvPad
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/05/2004
*! Purpose   : Function to return the first 2 line of the notepad.
*!*************************************************************
*! Called from : ALPKTKNO.FRX
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfInvPad
PARAMETERS lcDumdy

STORE SPACE(0) TO lcInvPad1 , lcInvPad2 , lcInvPad3 , lcInvPad4
IF SEEK('A' + CUSTOMER.ACCOUNT , 'NOTEPAD') .AND. ;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes))    .AND. ;
       LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' 

  lnOldMemW = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 35
  FOR lnContCd = 1 TO 4
    lcNum = ALLTRIM(STR(lncontcd))
    lcInvPad&lcNum = LEFT(MLINE(NOTEPAD.MNOTES,lnContCd),35)
  ENDFOR
  SET MEMOWIDTH TO lnOldMemW
ENDIF

RETURN ""
*--End of lfInvPad.
*!*************************************************************
*! Name      : lfWareHAdd
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/05/2004
*! Purpose   : Function to Get the Sold to & Ship to Address
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfWareHAdd
PARAMETER lcSolShp
PRIVATE lcAlias , lcInvHdr

lcAlias = ALIAS()
SELECT WAREHOUS
=SEEK(ORDLINE.CWARECODE)

IF EMPTY(lcDivLName)
  lcCompName = WAREHOUS.CWARECODE
ENDIF

DECLARE laCompAdd[5]
laCompAdd = ''           && Array to hold the Warehouse address.

laCompAdd[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
=lfAdrShift('laCompAdd')


SELECT(lcAlias)
RETURN ''

*--End of lfWareHAdd.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 26/05/2004
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address.
*!*************************************************************
*! Called from : lfWareHAdd()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 5
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

FOR lnCount = 1 TO 5
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*-- End of lfAdrShift.

*!*************************************************************
*! Name      : lfChkScl
*! Developer : NADER NABIL (NNA)
*! Date      : 06/15/2004
*! Purpose   : Function if the scale changed to print it or not.
*!*************************************************************
*! Called from : ALPKTKNO.FRX
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfChkScl
PARAMETERS lcDumdy
IF Scale <> lcFrxScale
  lcFrxScale = SCALE
  = lfGetLvls(Scale)
  llPrtScale =  .T.
ELSE
  llPrtScale = .F.
ENDIF
*-- End of lfChkScl.

*!*************************************************************
*! Name      : lfGetShpTo
*! Developer : NADER NABIL (NNA)
*! Date      : 06/15/2004
*! Purpose   : Function to get the shipto addresses.
*!*************************************************************
*! Called from : ALPKTKNO.FRX
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetShpTo
PARAMETERS lcDumdy
PRIVATE Dist_str , lnCustRec , lcOldAlias
lcOldAlias = ALIAS()
lnCustRec  = RECNO('CUSTOMER')

IF ORDHDR.ALT_SHPTO = .T.
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE
  SELECT CUSTOMER
  IF !EMPTY(PIKTKT.STORE)
    = SEEK('S' + PIKTKT.ACCOUNT + PIKTKT.STORE)
    IF !EMPTY(DIST_CTR)
      Dist_str = CUSTOMER.Dist_ctr
      = SEEK('S' + PIKTKT.ACCOUNT + Dist_str)
    ENDIF
  ELSE
    = SEEK('M' + PIKTKT.ACCOUNT)
  ENDIF
    STORE '' TO laShipTo
    lcShpTName  = STName
    laShipTo[1] = cAddress1
    laShipTo[2] = cAddress2
    laShipTo[3] = cAddress3
    laShipTo[4] = cAddress4
    laShipTo[5] = cAddress5
    
ENDIF

GOTO lnCustRec IN CUSTOMER
SELECT &lcOldAlias

*-- End of lfGetShpTo.

*!*************************************************************
*! Name      : lfGetTqty
*! Developer : NADER NABIL (NNA)
*! Date      : 06/15/2004
*! Purpose   : Function to get The total qty for every PikTkt. 
*!*************************************************************
*! Called from : ALPKTKNO.FRX
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetTqty
PARAMETERS lcDumdy
PRIVATE lnTmpRec , lcOldAlias , lcPikTkt

STORE 0 TO lnTotQty

lcOldAlias = ALIAS()
lnTmpRec   = RECNO(lcTmpOrdL)
lcPikTkt   = PIKTKT.Piktkt
IF llEndGroup
  SUM TOTQTY FOR EVAL(lcTmpOrdL + '.Piktkt') = lcPikTkt  TO lnTotQty
  GOTO lnTmpRec IN &lcTmpOrdL
  SELECT &lcOldAlias
  RETURN lnTotQty
ELSE
  RETURN 
ENDIF

*-- End of lfGetTqty.

*!*************************************************************
*! Name      : lfChngScPO
*! Developer : NADER NABIL (NNA)
*! Date      : 06/15/2004
*! Purpose   : Function to Change the Position of the Scale Sizes from Vertical To Horizontal
*!*************************************************************
*! Called from : ALPKTKNO.Prg
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfChngScPO
PRIVATE lcSiz , lcLvl, lnLvl ,lnTotQty
STORE 0 TO lnTotQty , lnRecNo
DECLARE laScaleLvl[12]

*B129061,1 EIH 08/02/2005 Delete Custom Prg# 126785 and Enhancment# E128455 [Begin].
*C126785,1 EIH 04/30/2005 Custom Sort by Regions and State we add 2 fields cregion and corderno and new index.[Begin]
*CREATE DBF (gcWorkDir+(lcTmpLvls)) ;
        (Order C(6),LineNo N(6),Store C(8),CustPo C(15),STyle C(19),Group C(1),Price N(12,2),;
        Scale C(1),Size C(5),Lvl1 N(6),Lvl2 N(6),Lvl3 N(6),Lvl4 N(6),Lvl5 N(6),Lvl6 N(6),;
        Lvl7 N(6),Lvl8 N(6),Lvl9 N(6),Lvl10 N(6),Lvl11 N(6),Lvl12 N(6),Totqty N(7),Piktkt C(6),;
        Pikdate D(8),Dyelot C(10),Pack_id C(16),CgrupDetal C(1),CurrSmbl C(3),CurrCode C(3))
        
*CREATE DBF (gcWorkDir+(lcTmpLvls)) ;
        (Order C(6),LineNo N(6),Store C(8),CustPo C(15),STyle C(19),Group C(1),Price N(12,2),;
        Scale C(1),Size C(5),Lvl1 N(6),Lvl2 N(6),Lvl3 N(6),Lvl4 N(6),Lvl5 N(6),Lvl6 N(6),;
        Lvl7 N(6),Lvl8 N(6),Lvl9 N(6),Lvl10 N(6),Lvl11 N(6),Lvl12 N(6),Totqty N(7),Piktkt C(6),;
        Pikdate D(8),Dyelot C(10),Pack_id C(16),CgrupDetal C(1),CurrSmbl C(3),CurrCode C(3),;
        CREGION C(20),NREGORDER N(3),COrderNo C(2) ,DONTDELETE L(1) ) 

        

*INDEX ON STR(NREGORDER)+COrderNo+Piktkt+SUBSTR(Style,1,LEN(STYLE)-2)+Scale+Size TAG lcRegIDX
CREATE DBF (gcWorkDir+(lcTmpLvls)) ;
        (Order C(6),LineNo N(6),Store C(8),CustPo C(15),STyle C(19),Group C(1),Price N(12,2),;
        Scale C(1),Size C(5),Lvl1 N(6),Lvl2 N(6),Lvl3 N(6),Lvl4 N(6),Lvl5 N(6),Lvl6 N(6),;
        Lvl7 N(6),Lvl8 N(6),Lvl9 N(6),Lvl10 N(6),Lvl11 N(6),Lvl12 N(6),Totqty N(7),Piktkt C(6),;
        Pikdate D(8),Dyelot C(10),Pack_id C(16),CgrupDetal C(1),CurrSmbl C(3),CurrCode C(3))
        
*C126785,1 EIH 04/30/2005 [End]
*B129061,1 EIH 08/02/2005 [End].


INDEX ON PikTkt TAG (lcTmpOrdU) UNIQUE			&& this index that the master prg. (Alpktk) use
INDEX ON Piktkt+SUBSTR(Style,1,LEN(STYLE)-2)+Scale+Size TAG (lcTmpOrdL)

SELECT (lcTmpOrdL)
SCAN FOR CgrupDetal = 'D'
  
  *B123482,1 NNA 07/07/2004 (Begin) if Scale has more one Dim. We Reverse the way that the size Prints
  IF SEEK(SUBSTR(EVAL(lcTmpOrdL+'.Scale'),1,1),'SCALEHD') .AND. SCALEHD.NnoofDim > 1
  *B123482,1 NNA (End)
  
    IF !EMPTY(SCALE)        && because there is a Record in the Temp File for order notepad (cGrupDetal = 'H')
      lnRecNo = RECNO('SCALE')
    ENDIF
    =lfGetLvls(Scale)
    GOTO lnRecNo IN SCALE   
    lnLvl = ASCAN(laScaleLvl,SUBSTR(Scale,2,2)) 	&& Get The Position of the level in the array
    lcLvl = STR(lnLvl , IIF(lnLvl > 9 , 2 , 1))
    IF lnLvl <> 0
      FOR I = 1 TO 8
        lcSiz = STR(I,1)
        SELECT (lcTmpLvls)
        IF SEEK(EVAL(lcTmpOrdL+'.Piktkt') + SUBSTR(EVAL(lcTmpOrdL+'.STyle'),1,LEN(STYLE)-2) +;
                SUBSTR(EVAL(lcTmpOrdL+'.Scale'),1,1) + ALLTRIM(SCALE.Sz&lcSiz))
          *B131211,1 MMR 02/09/2006 Fix the bug of printed the ordered qty instead of the picked.
          *REPLACE Lvl&lcLvl WITH Lvl&lcLvl + EVAL(lcTmpOrdL+'.Qty&lcSiz')
          REPLACE Lvl&lcLvl WITH Lvl&lcLvl + EVAL(lcTmpOrdL+'.Pik&lcSiz')
          *B131211,1 MMR.[End]
        ELSE
          *B131211,1 MMR 02/09/2006 Fix the bug of printed the ordered qty instead of the picked.
          *IF EVAL(lcTmpOrdL+'.QTY&lcSiz') > 0
          IF EVAL(lcTmpOrdL+'.Pik&lcSiz') > 0
          *B131211,1 MMR.[End]
            APPEND BLANK
            *B131211,1 MMR 02/09/2006 Fix the bug of printed the ordered qty instead of the picked.[Start]
            *REPLACE Order      WITH EVAL(lcTmpOrdL+'.Order')                ,;
                    LineNo     WITH EVAL(lcTmpOrdL+'.LineNo')               ,;
                    Store      WITH EVAL(lcTmpOrdL+'.Store')                ,;
                    CustPo     WITH EVAL(lcTmpOrdL+'.CustPo')               ,;
                    STyle      WITH EVAL(lcTmpOrdL+'.STyle')                ,;
                    Group      WITH EVAL(lcTmpOrdL+'.Group')                ,;
                    Price      WITH EVAL(lcTmpOrdL+'.Price')                ,;
                    Scale      WITH SUBSTR(EVAL(lcTmpOrdL+'.Scale'),1,1)    ,;
                    Size       WITH ALLTRIM(Scale.Sz&lcSiz)                 ,;
                    Lvl&lcLvl  WITH Lvl&lcLvl + EVAL(lcTmpOrdL+'.Qty&lcSiz'),;
                    PikTkt     WITH EVAL(lcTmpOrdL+'.PikTkt')               ,;                
                    Dyelot     WITH EVAL(lcTmpOrdL+'.Dyelot')               ,;                
                    Pack_ID    WITH EVAL(lcTmpOrdL+'.Pack_ID')              ,;
                    CgrupDetal WITH EVAL(lcTmpOrdL+'.CgrupDetal')           ,;                              
                    CurrSmbl   WITH EVAL(lcTmpOrdL+'.CurrSmbl')             ,;                              
                    CurrCode   WITH EVAL(lcTmpOrdL+'.CurrCode')
            REPLACE Order      WITH EVAL(lcTmpOrdL+'.Order')                ,;
                    LineNo     WITH EVAL(lcTmpOrdL+'.LineNo')               ,;
                    Store      WITH EVAL(lcTmpOrdL+'.Store')                ,;
                    CustPo     WITH EVAL(lcTmpOrdL+'.CustPo')               ,;
                    STyle      WITH EVAL(lcTmpOrdL+'.STyle')                ,;
                    Group      WITH EVAL(lcTmpOrdL+'.Group')                ,;
                    Price      WITH EVAL(lcTmpOrdL+'.Price')                ,;
                    Scale      WITH SUBSTR(EVAL(lcTmpOrdL+'.Scale'),1,1)    ,;
                    Size       WITH ALLTRIM(Scale.Sz&lcSiz)                 ,;
                    Lvl&lcLvl  WITH Lvl&lcLvl + EVAL(lcTmpOrdL+'.Pik&lcSiz'),;
                    PikTkt     WITH EVAL(lcTmpOrdL+'.PikTkt')               ,;                
                    Dyelot     WITH EVAL(lcTmpOrdL+'.Dyelot')               ,;                
                    Pack_ID    WITH EVAL(lcTmpOrdL+'.Pack_ID')              ,;
                    CgrupDetal WITH EVAL(lcTmpOrdL+'.CgrupDetal')           ,;                              
                    CurrSmbl   WITH EVAL(lcTmpOrdL+'.CurrSmbl')             ,;                              
                    CurrCode   WITH EVAL(lcTmpOrdL+'.CurrCode')
            *B131211,1 MMR.[End]         
          ENDIF                
        ENDIF
        SELECT (lcTmpOrdL)                           
      ENDFOR
    ENDIF

  *B123482,1 NNA 07/07/2004 (Begin) Else if the scale has one Dim. Only We Print with the Regular Way
  ELSE
    SELECT (lcTmpOrdL)                           
    SCATTER MEMVAR MEMO
    SELECT (lcTmpLvls)
      APPEND BLANK
      *B123880,1 ASH 08/04/2004 (Begin) Fix the bug of printed the ordered qty instead of the picked.
      *REPLACE Order      WITH EVAL(lcTmpOrdL+'.Order')                ,;
              LineNo     WITH EVAL(lcTmpOrdL+'.LineNo')               ,;
              Store      WITH EVAL(lcTmpOrdL+'.Store')                ,;
              CustPo     WITH EVAL(lcTmpOrdL+'.CustPo')               ,;
              STyle      WITH EVAL(lcTmpOrdL+'.STyle')                ,;
              Group      WITH EVAL(lcTmpOrdL+'.Group')                ,;
              Price      WITH EVAL(lcTmpOrdL+'.Price')                ,;
              Scale      WITH SUBSTR(EVAL(lcTmpOrdL+'.Scale'),1,1)    ,;
              Lvl1       WITH EVAL(lcTmpOrdL+'.Qty1')                 ,;
              Lvl2       WITH EVAL(lcTmpOrdL+'.Qty2')                 ,;              
              Lvl3       WITH EVAL(lcTmpOrdL+'.Qty3')                 ,;              
              Lvl4       WITH EVAL(lcTmpOrdL+'.Qty4')                 ,;              
              Lvl5       WITH EVAL(lcTmpOrdL+'.Qty5')                 ,;
              Lvl6       WITH EVAL(lcTmpOrdL+'.Qty6')                 ,;
              Lvl7       WITH EVAL(lcTmpOrdL+'.Qty7')                 ,;                                          
              Lvl8       WITH EVAL(lcTmpOrdL+'.Qty8')                 ,;              
              Lvl9       WITH 0                                       ,;              
              Lvl10      WITH 0                                       ,;              
              Lvl11      WITH 0                                       ,;              
              Lvl12      WITH 0                                       ,;                                                                      
              PikTkt     WITH EVAL(lcTmpOrdL+'.PikTkt')               ,;                
              Dyelot     WITH EVAL(lcTmpOrdL+'.Dyelot')               ,;                
              Pack_ID    WITH EVAL(lcTmpOrdL+'.Pack_ID')              ,;
              CgrupDetal WITH EVAL(lcTmpOrdL+'.CgrupDetal')           ,;                              
              CurrSmbl   WITH EVAL(lcTmpOrdL+'.CurrSmbl')             ,;                              
              CurrCode   WITH EVAL(lcTmpOrdL+'.CurrCode')
      REPLACE Order      WITH EVAL(lcTmpOrdL+'.Order')                ,;
              LineNo     WITH EVAL(lcTmpOrdL+'.LineNo')               ,;
              Store      WITH EVAL(lcTmpOrdL+'.Store')                ,;
              CustPo     WITH EVAL(lcTmpOrdL+'.CustPo')               ,;
              STyle      WITH EVAL(lcTmpOrdL+'.STyle')                ,;
              Group      WITH EVAL(lcTmpOrdL+'.Group')                ,;
              Price      WITH EVAL(lcTmpOrdL+'.Price')                ,;
              Scale      WITH SUBSTR(EVAL(lcTmpOrdL+'.Scale'),1,1)    ,;
              Lvl1       WITH EVAL(lcTmpOrdL+'.Pik1')                 ,;
              Lvl2       WITH EVAL(lcTmpOrdL+'.Pik2')                 ,;              
              Lvl3       WITH EVAL(lcTmpOrdL+'.Pik3')                 ,;              
              Lvl4       WITH EVAL(lcTmpOrdL+'.Pik4')                 ,;              
              Lvl5       WITH EVAL(lcTmpOrdL+'.Pik5')                 ,;
              Lvl6       WITH EVAL(lcTmpOrdL+'.Pik6')                 ,;
              Lvl7       WITH EVAL(lcTmpOrdL+'.Pik7')                 ,;                                          
              Lvl8       WITH EVAL(lcTmpOrdL+'.Pik8')                 ,;              
              Lvl9       WITH 0                                       ,;              
              Lvl10      WITH 0                                       ,;              
              Lvl11      WITH 0                                       ,;              
              Lvl12      WITH 0                                       ,;                                                                      
              PikTkt     WITH EVAL(lcTmpOrdL+'.PikTkt')               ,;                
              Dyelot     WITH EVAL(lcTmpOrdL+'.Dyelot')               ,;                
              Pack_ID    WITH EVAL(lcTmpOrdL+'.Pack_ID')              ,;
              CgrupDetal WITH EVAL(lcTmpOrdL+'.CgrupDetal')           ,;                              
              CurrSmbl   WITH EVAL(lcTmpOrdL+'.CurrSmbl')             ,;                              
              CurrCode   WITH EVAL(lcTmpOrdL+'.CurrCode')              
      *B123880,1 ASH 08/04/2004 (End)
  ENDIF  
  *B123482,1 NNA (End)

ENDSCAN

*B129061,1 EIH 08/02/2005 Delete Custom Prg# 126785 and Enhancment# E128455 [Begin].
*C126785,1 EIH 04/30/2005 Custom Sort by Regions and State we add 2 fields cregion and corderno [Begin]
*IF !USED('piktkreg')
*  = gfOpenFile(gcDataDir + 'piktkreg' , gcDataDir + 'State', 'SH', 'piktkreg', .T.)
*ENDIF
*SELECT (lcTmpLvls)
*SET ORDER TO 
*LOCATE
*SCAN
*  STORE '' TO lcOrdNo ,  lcStore   ,  lcAccount , lcRegion , lcState   ,lcOredrNo
*  SELECT PIKTKT
*  = SEEK(&lcTmpLvls..PIKTKT)
*  lcOrdNo   = &lcTmpLvls..Order
*  lcStore   = &lcTmpLvls..Store
*  lcAccount = PIKTKT.Account
*  *E128455,1 EIH 06/19/2005 Make Custom# 126785 work on the customer 'W149A'only [Begin].
*  IF lcAccount = 'W149A'
*  *E128455,1 [End].
*    SELECT ORDHDR
*    = SEEK(lcOrdNo)
*    IF ORDHDR.ALT_SHPTO = .T.      && case of alternate ship to
*      SELECT ORDHDR
*      lcRegion  = 'NotFound' 
*      lcState   = 'Not'
*    ELSE                           && case of customer
*      SELECT CUSTOMER
*      IF !EMPTY(lcStore)
*        = SEEK('S' + lcAccount +lcStore )
*        IF !EMPTY(DIST_CTR)
*          Dist_str = CUSTOMER.Dist_ctr
*          = SEEK('S' + lcAccount + Dist_str)
*        ENDIF
*      ELSE
*        = SEEK( 'M' + lcAccount )
*      ENDIF
*      lcState   = CUSTOMER.Caddress4                            && State = CUSTOMER.Caddress4                  
*      SELECT (lcTmpLvls)
*    ENDIF
*    SELECT ('piktkreg')
*    LOCATE
*    =SEEK(ALLTRIM(lcState))  
*    IF !EOF()
*      lcRegion  = PIKTKReg.CRegion
*      lcOredrNo = PIKTKReg.COrderNo
*      SELECT (lcTmpLvls)
*      REPLACE  CREGION     WITH         lcRegion   ,;
*               CORDERNO    WITH         lcOredrNo 
*    ENDIF
*  *E128455,1 EIH 06/19/2005 Make Custom# 126785 work on the customer 'W149A'only [Begin].
*  ENDIF
*  *E128455,1 [End].
*ENDSCAN
*SET ORDER TO TAG (lcTmpOrdL)
*SELECT piktkreg
*USE
*SELECT (lcTmpLvls)
*LOCATE
*STORE 1 TO lnCounter
*IF !EMPTY(laRpTRegn)
*  SELECT (lcTmpLvls)
*  * SET REGION ORDER
*  FOR lnCounter = 1 TO ALEN(laRpTRegn) STEP 1
*    REPLACE FOR CREGION = laRpTRegn[lnCounter]  NREGORDER  WITH lnCounter , ;
*                                                DONTDELETE WITH   .T.
*  ENDFOR
*  *E128455,1 EIH 06/19/2005 Make Custom# 126785 work on the customer 'W149A'only [Begin].
*  *DELETE FOR DONTDELETE <> .T.
*  DELETE FOR DONTDELETE <> .T.  AND !EMPTY(CREGION)
*  *E128455,1 [End].
*ENDIF

*C126785,1 EIH 04/30/2005 [End]
*B129061,1 EIH 08/02/2005 [End].

*B129061,1 EIH 08/02/2005 Delete Custom Prg# 126785 and Enhancment# E128455 [Begin].
SELECT (lcTmpLvls)
LOCATE
*B129061,1 EIH 08/02/2005 [End].
SCAN
  FOR I = 1 TO 12
    lcLvl = STR(I,IIF( I > 9,2,1))
    lnTotQty = lnTotQty + Lvl&lcLvl
  ENDFOR
  REPLACE TOTQTY WITH lnTotQty
  STORE 0 TO lnTotQty
ENDSCAN

*-- End of lfChngScPO.
*!*************************************************************
*! Name      : lfGetLvls
*! Developer : NADER NABIL (NNA)
*! Date      : 06/15/2004
*! Purpose   : Function to Get The Scale levels i.e(E30,E32,E34...etc)
*!*************************************************************
*! Called from : ALPKTKNO.Prg
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : lCScale = Scale's Chr.
*!*************************************************************
*! Example : = lfGetLvls(Scale)
*!*************************************************************
*! Return      : laScaleLvl filled with the Scale's levels if the 
*!               Scale has more one Dim. or Return With Scale Sizes
*!               if Scale has one Dim.
*!*************************************************************
FUNCTION lfGetLvls
PARAMETERS lCScale
PRIVATE lnArrPos , lnAlias
STORE 0 TO lnArrPos , lnOldAias
lnAlias = SELECT(0)
IF SUBSTR(lcScale,1,1) == lcOldScale
  RETURN
ELSE
  *B123482,1 NNA (Begin) 07/07/2004 Check if scale has one or more Dim.
  *-- if Scale has more one Dim. We Reverse the way that the size Prints
  IF SEEK(SUBSTR(lcScale,1,1),'SCALEHD') .AND. SCALEHD.NnoofDim > 1
  *B123482,1 NNA (End)
  
    *-- Get all the Scale Levels in an array
    lcOldScale = SUBSTR(lcScale,1,1)
    STORE '' TO laScaleLvl			&& Reset the array
    SELECT SCALE
    =SEEK('S'+SUBSTR(lcScale,1,1))    && Seek i.e ('S' + 'E')
    SCAN REST WHILE TYPE + SCALE = 'S'+SUBSTR(lcScale,1,1)
      lnArrPos = lnArrPos + 1
      IF lnArrPos <= 12               && because the Max. Levels that Printed in the form = 12
        laScaleLvl[lnArrPos] = SUBSTR(SCALE.Scale,2,2)
      ENDIF
    ENDSCAN
  *B123482,1 NNA 07/07/2004 (Begin) Else if the scale has one Dim. Only We Print with the Regular Way
  ELSE
    *-- Get all the Scale Sizes in an array
    lcOldScale = SUBSTR(lcScale,1,1)    
    STORE '' TO laScaleLvl			&& Reset the array
    SELECT SCALE
    =SEEK('S'+lcScale)    
    FOR I = 1 TO 12
      lcSiz = STR(I , IIF(I > 9 , 2 , 1))
      IF I <=8
        laScaleLvl[I] = SCALE.SZ&lcSiz
      ELSE
        laScaleLvl[I] = ' '
      ENDIF
    ENDFOR
  ENDIF
  *B123482,1 NNA (End) 

ENDIF
SELECT (lnAlias)
*--End of lfGetLvls.

*!*************************************************************
*! Name      : lfOrdNote
*! Developer : NADER NABIL (NNA)
*! Date      : 10/31/2004
*! Purpose   : Function to return Order notepad.
*!*************************************************************
*! Called from : ALPKTKNO.FRX
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*!B124826,1
FUNCTION lfOrdNote
PARAMETERS lcDumdy

STORE SPACE(0) TO lcOrdNote1 , lcOrdNote2 , lcOrdNote3 , lcOrdNote4
IF SEEK('B' + ORDHDR.ORDER,'NOTEPAD') .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. ;
       LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' 

  lnOldMemW = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 41
  FOR lnContCd = 1 TO 4
    lcNum = ALLTRIM(STR(lncontcd))
    lcOrdNote&lcNum = LEFT(MLINE(NOTEPAD.MNOTES,lnContCd),35)
  ENDFOR
  SET MEMOWIDTH TO lnOldMemW
ENDIF

RETURN ""
*--End of lfInvPad.


*B129061,1 EIH 08/02/2005 Delete Custom Prg# 126785 and Enhancment# E128455 [Begin].
*C126785,1 EIH 04/30/2005 Custom Sort by Regions and State we add 2 fields cregion and corderno [Begin]
*!*************************************************************
*! Name      : lfvRegion
*! Developer : Ehab Ismail (EIH)
*! Date      : 30/04/2005
*! Purpose   : Valid Function to Region showing mover
*!           : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvRegion()
*!*************************************************************
*FUNCTION lfvRegion
*PARAMETER lcParam
**--The Region.

*IF TYPE('laRpSRegn') == 'L'
*  DIMENSION laRpSRegn[1]
*ENDIF
*IF TYPE('laRpTRegn') == 'L'
*  DIMENSION laRpTRegn[1,1]
*  =lfFillReg()
*ENDIF
*=gfMover(@laRpSRegn,@laRpTRegn,'Region',.T.,'')
*lcParam = .T.
*RETURN lcParam 
**-- End of lfvRegion .
*!*************************************************************
*! Name      : lfFillReg
*! Developer : Ehab Ismail (EIH)
*! Date      : 30/04/2005
*! Purpose   : Function to fill Region
*!           : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillReg()
*!*************************************************************
*FUNCTION lfFillReg
*lcAlias = ALIAS()
*IF !USED('piktkreg')
*  = gfOpenFile(gcDataDir + 'piktkreg' , gcDataDir + 'State', 'SH', 'piktkreg', .T.)
*ENDIF

*SELECT piktkreg
*GOTO TOP
*SELECT DISTINCT cRegion FROM piktkreg INTO ARRAY laRpSRegn
*USE
*SELECT(lcAlias)
*-- End of lfFillReg.
*C126785,1 EIH 04/30/2005 [End]
*B129061,1 EIH 08/02/2005 [End].