*:****************************************************************************************
*: Program file  : Alpktkgm.Prg
*: Program desc. : Picking Ticket Form for GMA
*: System        : Aria Apparel System (A27).
*: Module        : Sales Order Allocation (AL).
*: Developer     : Ahmed Abdel Naby - (AAN) Due to C#102261,1
*: Date          : 05/28/2001
*:****************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************
*:
*: B604746,1 IAA 08/06/2001 fixed some bugs reported by Alaa
*: B604746,4 SSH 08/06/2001 Increase the qty field lenght.
*: B604854,1 TMI 08/30/2001 Fix some problems in the PT for GMA.
*: C200331,1 HBG 05/12/2002 Accommodate for the changes done to the pack and its components
*:****************************************************************************************

*-- Declaration variables.
*-- lcTmpOrdL :- this file come from the piktkt proram after filteration.
DECLARE laSoldTo[5,1] , laShipTo[5,1]
PRIVATE lcNToRet
lcNToRet = ''
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
PRIVATE  lnAlias,laFileStru,lnFileStru,lcOrdLTmp,lnOrdPos,lnPicPak,lcOrdNo,lnLoop,lnSzNo,lncnt,lcPack,lcPack1,lcAccount,lcSty
STORE 0  TO lnAlias,lnFileStru,lnOrdPos,lnPicPak,lnLoop,lnSzNo,lncnt
STORE '' TO lcTempPktk,lcPack,lcPack1,lcAccount,lcSty
DIMENSION laFileStru[1,4]
STORE '' TO laFileStru , lcOrdLTmp
lnAlias = SELECT (0)
=gfOpenFile(gcDataDir+'StyleUPC',gcDataDir+'StyleUPC','SH')
=gfOpenFile(gcDataDir+'InvHdr',gcDataDir+'InvHdra','SH')
=gfOpenFile(gcDataDir+'InvLine',gcDataDir+'InvLines','SH')
=gfOpenFile(gcDataDir+'ConsInvh',gcDataDir+'ConsInvh','SH')
=gfOpenFile(gcDataDir+'ConsInvl',gcDataDir+'ConsInvl','SH')
SELECT (lnAlias)
*-- End Declaration variables.

SELECT (lcTmpOrdL)
lcOrdLTmp = gfTempName()
*-- call function to create temp file.
= lfCrTmpFls ()

*-- New function to collect the data upon the new selection critria
*-- In the optiongrid.

= lfNewColet ()

*-- Return the old relation.
SELECT (lcTmpOrdL)
GO TOP

RETURN
*-- End Of Form code.

*-- Function Section.
*:****************************************************************
*: Name        : lfNewColet
*: Developer   : Ahmed Abdel Naby - (AAN)
*: Date        : 05/28/2001
*: Purpose     : Function to collect data on the new criteria
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfNewColet()
*:****************************************************************
*
FUNCTION lfNewColet
SELECT (lcTmpOrdL)
SET FILTER TO
IF !EMPTY(lcTempStyle) .AND. USED(lcTempStyle) .AND. RECCOUNT(lcTempStyle) > 0
  SET FILTER TO SEEK(&lcTmpOrdL..Order,lcTempStyle) .AND. PikTkt.Status<>'X'
ELSE
  SET FILTER TO PikTkt.Status<>'X'
ENDIF
GO TOP

DO WHILE !EOF()
  SELECT (lcTmpOrdL)
  lcTempPktk = PikTkt
  lcOrdNo    = Order
  lcStore    = Store
  *C200331,1 HBG 05/12/2002 Get the full Pack ID [Begin]
  *lcPack     = OrdLine.Pack_id
  lcPack     = OrdLine.Pack_id + OrdLine.cPkColor + OrdLine.cPckSize + OrdLine.cPkVersion
  *C200331,1 [End]
  lcPack1    = lcPack
  lnLoop     = 0
  =lfAccChk()
  SELECT (lcTmpOrdL)
  =SEEK(lcTempPktk)
  SCAN REST WHILE  PikTkt + Order + cGrupDetal + STR(Lineno,6) = lcTempPktk
    lcSty      = OrdLine.Style
    IF cGrupDetal <>"H"
      *C200331,1 HBG 05/12/2002 Get the full Pack ID [Begin]
      *IF lcPack1 <> OrdLine.Pack_Id
        *lcPack1 = OrdLine.Pack_Id
      IF lcPack1 <> OrdLine.Pack_id + OrdLine.cPkColor + OrdLine.cPckSize + OrdLine.cPkVersion
        lcPack1 = OrdLine.Pack_id + OrdLine.cPkColor + OrdLine.cPckSize + OrdLine.cPkVersion
        *C200331,1 [End]
        lnLoop = 0
        REPLACE &lcOrdLTmp..lFinal WITH .T.
        =lfAccChk()
      ENDIF

      =SEEK('S'+OrdLine.Scale,'Scale')
      lnSzNo = Scale.Cnt
      FOR lncnt = 1 to lnSzNo
        *B604854,1 TMI [Start] Move this line to the below if statement.
        *lnLoop = lnLoop + 1
        *B604854,1 TMI [End  ]        
        SCATTER MEMVAR MEMO
        *B604854,1 TMI [Start] if the qty of this size is 0 , don't add a line to the new tmp file
        IF EVAL('M.PIK'+STR(lncnt,1)) > 0
          lnLoop = lnLoop + 1
        *B604854,1 TMI [End  ]        
          INSERT INTO (lcOrdLTmp) FROM MEMVAR
          =SEEK('P'+OrdLine.Scale+OrdLine.PrePak,'Scale')
          *--- Seek SPack Line Get total Qty for Pack
          lnPicPak = Scale.PpTot
          IF lcAccount <>'*****'
            *C200331,1 HBG 05/12/2002 Seek by the full Pack ID [Begin]
            *=SEEK('P'+OrdHdr.Account+OrdLine.Pack_id+OrdLine.Style,'Spck_Lin')
            =SEEK('P'+OrdHdr.Account+OrdLine.Pack_id+OrdLine.cPkColor+;
                      OrdLine.cPckSize + OrdLine.cPkVersion+OrdLine.Style,'Spck_Lin')
            *C200331,1 [End]
          ELSE
            *C200331,1 HBG 05/12/2002 Seek by the full Pack ID [Begin]
            *=SEEK('P'+'*****'+OrdLine.Pack_id+OrdLine.Style,'Spck_Lin')
            =SEEK('P'+'*****'+OrdLine.Pack_id+OrdLine.cPkColor+;
                              OrdLine.cPckSize + OrdLine.cPkVersion+OrdLine.Style,'Spck_Lin')
            *C200331,1 [End]
          ENDIF
          =lfRplfld(lncnt)
        *B604854,1 TMI [Start] End of the above IF statement
        ENDIF
        *B604854,1 TMI [End  ]
      ENDFOR
    ENDIF
  ENDSCAN
  SELECT (lcOrdLTmp)
  APPEND BLANK
  REPLACE cType WITH '2',;
          PikTkt WITH lcTempPktk
  =lfPrtNot()  
  SELECT (lcTmpOrdL)
ENDDO

*B604854,1 TMI [Start] Display the units of color size if no pack , 
*B604854,1             and the total units of the Pack in the pack case.
SELECT (lcOrdLTmp)
lcNewSty=''
llFinal = .T.
SCAN 
  IF lSku AND lcNewSty#STYLE
    REPLACE nPikUnits WITH TOTPIK
  ELSE
    IF llFinal
      REPLACE nPikUnits WITH nUnits
    ENDIF
  ENDIF
  lcNewSty = STYLE
  llFinal = IIF(CTYPE='2',.T.,lFinal)  
ENDSCAN
*B604854,1 TMI [End  ]
lcTmpOrdL = lcOrdLTmp
*-- End Of lfNewColet.

*:****************************************************************
*: Name        : lfCrTmpFls
*: Developer   : Ahmed Abdel Naby - (AAN)
*: Date        : 05/28/2001
*: Purpose     : Function to Create temp files.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfCrTmpFls ()
*:****************************************************************
*
FUNCTION lfCrTmpFls

PRIVATE lnOldAls
lnOldAls = SELECT(0)

*-- Create the Temp file that we will collect the data on it.
SELECT (lcTmpOrdL)
= AFIELDS (laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru + 30, 4]

laFileStru[lnFileStru+1,1] = 'dStrtDat'
laFileStru[lnFileStru+1,2] = 'D'
laFileStru[lnFileStru+1,3] = 8
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'dCancDat'
laFileStru[lnFileStru+2,2] = 'D'
laFileStru[lnFileStru+2,3] = 8
laFileStru[lnFileStru+2,4] = 0

laFileStru[lnFileStru+3,1] = 'dCompDat'
laFileStru[lnFileStru+3,2] = 'D'
laFileStru[lnFileStru+3,3] = 8
laFileStru[lnFileStru+3,4] = 0

laFileStru[lnFileStru+4,1] = 'cBulkNo'
laFileStru[lnFileStru+4,2] = 'C'
laFileStru[lnFileStru+4,3] = 6
laFileStru[lnFileStru+4,4] = 0

laFileStru[lnFileStru+5,1] = 'cDept'
laFileStru[lnFileStru+5,2] = 'C'
laFileStru[lnFileStru+5,3] = 5
laFileStru[lnFileStru+5,4] = 0

laFileStru[lnFileStru+6,1] = 'cOrdNote1'
laFileStru[lnFileStru+6,2] = 'C'
laFileStru[lnFileStru+6,3] = 30
laFileStru[lnFileStru+6,4] = 0

laFileStru[lnFileStru+7,1] = 'cTerms'
laFileStru[lnFileStru+7,2] = 'C'
laFileStru[lnFileStru+7,3] = 10
laFileStru[lnFileStru+7,4] = 0

laFileStru[lnFileStru+8,1] = 'cSeason'
laFileStru[lnFileStru+8,2] = 'C'
laFileStru[lnFileStru+8,3] = 11
laFileStru[lnFileStru+8,4] = 0

laFileStru[lnFileStru+9,1] = 'cShipVia'
laFileStru[lnFileStru+9,2] = 'C'
laFileStru[lnFileStru+9,3] = 30
laFileStru[lnFileStru+9,4] = 0

laFileStru[lnFileStru+10,1] = 'cPackSku'
laFileStru[lnFileStru+10,2] = 'C'
*B604854,1 TMI [Start] The 'cPackSku' field must be 16 character
*laFileStru[lnFileStru+10,3] = 6
*C200331,1 HBG 05/12/2002 Increase the width of the field to get the full Pack ID [Begin]
*laFileStru[lnFileStru+10,3] = 16
laFileStru[lnFileStru+10,3] = 32
*C200331,1 [End]
*B604854,1 TMI [End  ]
laFileStru[lnFileStru+10,4] = 0

laFileStru[lnFileStru+11,1] = 'nPicPak'
laFileStru[lnFileStru+11,2] = 'N'
laFileStru[lnFileStru+11,3] = 20
laFileStru[lnFileStru+11,4] = 0

laFileStru[lnFileStru+12,1] = 'nUnits'
laFileStru[lnFileStru+12,2] = 'N'
laFileStru[lnFileStru+12,3] = 6
laFileStru[lnFileStru+12,4] = 0

laFileStru[lnFileStru+13,1] = 'cStyDesc'
laFileStru[lnFileStru+13,2] = 'C'
laFileStru[lnFileStru+13,3] = 30
laFileStru[lnFileStru+13,4] = 0

laFileStru[lnFileStru+14,1] = 'cPcsPk'
laFileStru[lnFileStru+14,2] = 'N'
laFileStru[lnFileStru+14,3] = 7
laFileStru[lnFileStru+14,4] = 0

laFileStru[lnFileStru+15,1] = 'cQty'
laFileStru[lnFileStru+15,2] = 'C'
laFileStru[lnFileStru+15,3] = 8
laFileStru[lnFileStru+15,4] = 0

laFileStru[lnFileStru+16,1] = 'cSize'
laFileStru[lnFileStru+16,2] = 'C'
laFileStru[lnFileStru+16,3] = 6
laFileStru[lnFileStru+16,4] = 0

laFileStru[lnFileStru+17,1] = 'cUnitShp'
laFileStru[lnFileStru+17,2] = 'N'
laFileStru[lnFileStru+17,3] = 6
laFileStru[lnFileStru+17,4] = 0

laFileStru[lnFileStru+18,1] = 'cStyUpc'
laFileStru[lnFileStru+18,2] = 'C'
laFileStru[lnFileStru+18,3] = 13
laFileStru[lnFileStru+18,4] = 0

laFileStru[lnFileStru+19,1] = 'cUpc'
laFileStru[lnFileStru+19,2] = 'C'
laFileStru[lnFileStru+19,3] = 13
laFileStru[lnFileStru+19,4] = 0

laFileStru[lnFileStru+20,1] = 'cDunn'
laFileStru[lnFileStru+20,2] = 'C'
laFileStru[lnFileStru+20,3] = 11
laFileStru[lnFileStru+20,4] = 0

laFileStru[lnFileStru+21,1] = 'cType'
laFileStru[lnFileStru+21,2] = 'C'
laFileStru[lnFileStru+21,3] = 1
laFileStru[lnFileStru+21,4] = 0

laFileStru[lnFileStru+22,1] = 'cNote'
laFileStru[lnFileStru+22,2] = 'M'
laFileStru[lnFileStru+22,3] = 10
laFileStru[lnFileStru+22,4] = 0

laFileStru[lnFileStru+23,1] = 'nLine'
laFileStru[lnFileStru+23,2] = 'N'
laFileStru[lnFileStru+23,3] = 3
laFileStru[lnFileStru+23,4] = 0

laFileStru[lnFileStru+24,1] = 'cAccount'
laFileStru[lnFileStru+24,2] = 'C'
laFileStru[lnFileStru+24,3] = 5
laFileStru[lnFileStru+24,4] = 0

laFileStru[lnFileStru+25,1] = 'lSku'
laFileStru[lnFileStru+25,2] = 'L'
laFileStru[lnFileStru+25,3] = 1
laFileStru[lnFileStru+25,4] = 0

laFileStru[lnFileStru+26,1] = 'lFinal'
laFileStru[lnFileStru+26,2] = 'L'
laFileStru[lnFileStru+26,3] = 1
laFileStru[lnFileStru+26,4] = 0

laFileStru[lnFileStru+27,1] = 'npacks'
laFileStru[lnFileStru+27,2] = 'C'
laFileStru[lnFileStru+27,3] = 4
laFileStru[lnFileStru+27,4] = 0
*: B604746,1 IAA 08/06/2001 [begin] added size qty, note 2 custpo

*: B604746,4 SSH 08/06/2001 [begin] Increase the qty field lenght
*laFileStru[lnFileStru+28,1] = 'nSizeQty'
*laFileStru[lnFileStru+28,2] = 'N'
*laFileStru[lnFileStru+28,3] = 3
*laFileStru[lnFileStru+28,4] = 0

laFileStru[lnFileStru+28,1] = 'nSizeQty'
laFileStru[lnFileStru+28,2] = 'N'
laFileStru[lnFileStru+28,3] = 6
laFileStru[lnFileStru+28,4] = 0
*: B604746,4 SSH 08/06/2001 [END].

laFileStru[lnFileStru + 29, 1] = 'cOrdNote2'
laFileStru[lnFileStru + 29, 2] = 'C'
laFileStru[lnFileStru + 29, 3] = 30
laFileStru[lnFileStru + 29, 4] = 0

laFileStru[lnFileStru + 30, 1] = 'cCustPO'
laFileStru[lnFileStru + 30, 2] = 'C'
laFileStru[lnFileStru + 30, 3] = 15
laFileStru[lnFileStru + 30, 4] = 0

*: B604746,1 IAA 08/06/2001 [end]

*B604854,1 TMI [Start] Add a new field to hold the Number of Picked units for this style
nLen = ALEN(laFileStru,1)
DIMENSION laFileStru[nLen+1, 4]
laFileStru[nLen+1,1] = 'nPikUnits'
laFileStru[nLen+1,2] = 'N'
laFileStru[nLen+1,3] = 6
laFileStru[nLen+1,4] = 0
*B604854,1 TMI [End  ]

CREATE TABLE (gcWorkDir+lcOrdLTmp) FROM ARRAY laFileStru
INDEX ON PikTkt TAG (lcTmpOrdU)

SELECT (lnOldAls)

*-- End OF lfCrTmpFls.

*:****************************************************************
*: Name        : lfRplfld
*: Developer   : Ahmed Abdel Naby - (AAN)
*: Date        : 05/28/2001
*: Purpose     : Function to replace the fields in temp. files.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : lnCurSz
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfRplfld()
*:****************************************************************
*
*: B604746,1 IAA 08/06/2001 add parameter to get the ordered qtys
FUNCTION lfRplfld
PARAMETER  lnCurSz

PRIVATE lcCurQty, lnSzQty
SELECT ORDLINE
lcCurQty = 'QTY' + ALLTRIM(STR(lnCurSz))
lnSzQty = &lcCurQty
SELECT (lcOrdLTmp)
=lpPrtSku()

IF m.cGrupDetal<>'H'
  =lfTotUnt()
  =lfPcsPak()
  =lfShiped()
ENDIF
SELECT (lcOrdLTmp)
*:B604746,1 IAA 08/06/2001 [begin] add complete date, note1, note2, custPO to temp file
*B604854,1 TMI [Start] Correct the seek index in case of PackUpc
*REPLACE dStrtDat  WITH ORDHDR.Start      ,;
		cAccount  WITH ORDHDR.Account,;
		cCustPO   WITH ORDHDR.custPO,;
        dCancDat  WITH ORDHDR.Complete   ,;
        dCompDat  WITH IIF(CUSTOMER.PRIORITY = '1', ORDHDR.Complete, {})   ,;
        cBulkNo   WITH ORDHDR.cFromOrder ,;
        cDept     WITH ORDHDR.Dept       ,;
        cOrdNote1 WITH LEFT(ORDHDR.Note1,30), ; 
        cOrdNote2 WITH LEFT(ORDHDR.Note2,30),;  
        cSeason   WITH gfCodDes(ORDHDR.Season , 'SEASON'),;
        cTerms    WITH gfCodDes(ORDHDR.ctermcode,'cTermCode'),;
        cStyDesc  WITH Style.Desc,;
        cPcSpk    WITH IIF(lcAccount<>'*****',IIF(SEEK('P'+OrdHdr.Account+OrdLine.Pack_id+OrdLine.Style,'Spck_Lin'),EVAL("Spck_Lin.Qty"+ALLT(STR(lncnt))),0),IIF(SEEK('P'+'*****'+OrdLine.Pack_id+OrdLine.Style,'Spck_Lin'),EVAL("Spck_Lin.Qty"+ALLT(STR(lncnt))),0)),;
        nSizeQty  WITH lnSzQty,;
        cDunn     WITH Customer.Duns,;
        cStyUpc   WITH IIF(SEEK(OrdLine.Style+ALLTRIM(STR(lncnt)),'StyleUpc'),StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,''),;
        cUpc      WITH IIF(SEEK(OrdLine.Pack_Id,'StyleUpc'),StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,''),;
        cSize     WITH IIF(lnSzNo = 1,'',ALLTRIM(EVAL("Scale.Sz"+ALLT(STR(lncnt))))),;
        cQty      WITH STR((nunits/nPicPak)*cPcSpk),;
        cType     WITH '1',;
        nLine     WITH lnLoop,;
        cShipvia  WITH IIF(ORDHDR.ShipVia ='*',gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'),gfCodDes(ORDHDR.ShipVia , 'SHIPVIA')),;
        npacks    WITH ALLTRIM(STR(IIF((nunits/npicpak)-INT(nunits/npicpak)>0,INT((nunits/npicpak))+1,nunits/npicpak)))

STORE '' TO lcPackUPC,lcStyleUPC
=lfGetUPC()
*C200331,1 HBG 05/12/2002 Get the full Pack ID [Begin]
*REPLACE dStrtDat  WITH ORDHDR.Start      ,;
		cAccount  WITH ORDHDR.Account,;
		cCustPO   WITH ORDHDR.custPO,;
        dCancDat  WITH ORDHDR.Complete   ,;
        dCompDat  WITH IIF(CUSTOMER.PRIORITY = '1', ORDHDR.Complete, {})   ,;
        cBulkNo   WITH ORDHDR.cFromOrder ,;
        cDept     WITH ORDHDR.Dept       ,;
        cOrdNote1 WITH LEFT(ORDHDR.Note1,30), ; 
        cOrdNote2 WITH LEFT(ORDHDR.Note2,30),;  
        cSeason   WITH gfCodDes(ORDHDR.Season , 'SEASON'),;
        cTerms    WITH gfCodDes(ORDHDR.ctermcode,'cTermCode'),;
        cStyDesc  WITH Style.Desc,;
        cPcSpk    WITH IIF(lcAccount<>'*****',IIF(SEEK('P'+OrdHdr.Account+OrdLine.Pack_id+OrdLine.Style,'Spck_Lin'),EVAL("Spck_Lin.Qty"+ALLT(STR(lncnt))),0),IIF(SEEK('P'+'*****'+OrdLine.Pack_id+OrdLine.Style,'Spck_Lin'),EVAL("Spck_Lin.Qty"+ALLT(STR(lncnt))),0)),;
        nSizeQty  WITH lnSzQty,;
        cDunn     WITH Customer.Duns,;
        cStyUpc   WITH lcStyleUpc,;
        cUpc      WITH lcPackUpc,;
        cSize     WITH IIF(lnSzNo = 1,'',ALLTRIM(EVAL("Scale.Sz"+ALLT(STR(lncnt))))),;
        cQty      WITH STR((nunits/nPicPak)*cPcSpk),;
        cType     WITH '1',;
        nLine     WITH lnLoop,;
        cShipvia  WITH IIF(ORDHDR.ShipVia ='*',gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'),gfCodDes(ORDHDR.ShipVia , 'SHIPVIA')),;
        npacks    WITH ALLTRIM(STR(IIF((nunits/npicpak)-INT(nunits/npicpak)>0,INT((nunits/npicpak))+1,nunits/npicpak)))
REPLACE dStrtDat  WITH ORDHDR.Start      ,;
		cAccount  WITH ORDHDR.Account,;
		cCustPO   WITH ORDHDR.custPO,;
        dCancDat  WITH ORDHDR.Complete   ,;
        dCompDat  WITH IIF(CUSTOMER.PRIORITY = '1', ORDHDR.Complete, {})   ,;
        cBulkNo   WITH ORDHDR.cFromOrder ,;
        cDept     WITH ORDHDR.Dept       ,;
        cOrdNote1 WITH LEFT(ORDHDR.Note1,30), ; 
        cOrdNote2 WITH LEFT(ORDHDR.Note2,30),;  
        cSeason   WITH gfCodDes(ORDHDR.Season , 'SEASON'),;
        cTerms    WITH gfCodDes(ORDHDR.ctermcode,'cTermCode'),;
        cStyDesc  WITH Style.Desc,;
        cPcSpk    WITH IIF(lcAccount<>'*****',IIF(SEEK('P'+OrdHdr.Account+OrdLine.Pack_id+;
                       OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion+OrdLine.Style,;
                       'Spck_Lin'),EVAL("Spck_Lin.Qty"+ALLT(STR(lncnt))),0),;
                       IIF(SEEK('P'+'*****'+OrdLine.Pack_id+;
                       OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion+OrdLine.Style,;
                       'Spck_Lin'),EVAL("Spck_Lin.Qty"+ALLT(STR(lncnt))),0)),;
        nSizeQty  WITH lnSzQty,;
        cDunn     WITH Customer.Duns,;
        cStyUpc   WITH lcStyleUpc,;
        cUpc      WITH lcPackUpc,;
        cSize     WITH IIF(lnSzNo = 1,'',ALLTRIM(EVAL("Scale.Sz"+ALLT(STR(lncnt))))),;
        cQty      WITH STR((nunits/nPicPak)*cPcSpk),;
        cType     WITH '1',;
        nLine     WITH lnLoop,;
        cShipvia  WITH IIF(ORDHDR.ShipVia ='*',gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'),gfCodDes(ORDHDR.ShipVia , 'SHIPVIA')),;
        npacks    WITH ALLTRIM(STR(IIF((nunits/npicpak)-INT(nunits/npicpak)>0,INT((nunits/npicpak))+1,nunits/npicpak)))
*C200331,1 [End]
*B604854,1 TMI [End  ]
*:B604746,1 IAA 08/06/2001 [end]
SELECT (lcTmpOrdL)
*-- End OF lfRplfld.
*!***************************************************************
*! Name      : lpPrtSku.
*: Developer : Ahmed Abdel Naby - (AAN)
*: Date      : 05/28/2001
*!***************************************************************
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************
*B604854,1 TMI [Start] Comment this function to re-write its contents.
*FUNCTION lpPrtSku
FUNCTION XlpPrtSku
*B604854,1 TMI [End  ]

PRIVATE lcAlas
lcAlas = ''
lcAlas = SELECT(0)
SELECT Spck_Lin
SET ORDER TO SpckLins
IF EMPTY(Sku)
  lnI = 1
  = SEEK('S'+Style.Scale,'Scale')
  = SEEK('M'+ORDHDR.Account,'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    *C200331,1 HBG 05/12/2002 Increase the width to get the full Pack ID [Begin]
    *lnDime1 = 8  &&Default
    *lnDime2 = 8  &&Default
    lnDime1 = 29  &&Default
    lnDime2 = 29  &&Default
    *C200331,1[End]
  ENDIF
*:B604746,1 IAA 08/06/2001 [begin] print 'N/A' IF SINGLE STYLE
  IF EMPTY(lcPack)
    SELECT (lcOrdLTmp)
    REPLACE cPackSku WITH 'N/A'
  ELSE
  
  *:B604746,1 IAA 08/06/2001 [end]
    =SEEK('S'+ ORDHDR.Account + &lcTmpOrdL..Style,'Spck_Lin')
    *aan[end]
    IF FOUND()
      SELECT (lcOrdLTmp)
      *C200331,1 HBG 05/12/2002 Get the full Pack ID [Begin]
      *REPLACE cPackSku WITH SUBSTR(Spck_Lin.Pack_Id,1,lnDime1) &&ORDLINE
      REPLACE cPackSku WITH SUBSTR(Spck_Lin.Pack_Id+'-'+Spck_Lin.cPkColor+'-'+;
                                   lfGetGmSz(Spck_Lin.cPckSize)+'-'+Spck_Lin.cPkVersion,1,lnDime1) &&ORDLINE
      *C200331,1 [End]
      REPLACE lSku WITH .T.
    ELSE
      IF lcAccount <>'*****'
        =SEEK('P'+OrdHdr.Account+&lcTmpOrdL..Style,'Spck_Lin')
      ELSE
        =SEEK('P'+'*****'+&lcTmpOrdL..Style,'Spck_Lin')
      ENDIF
      *=SEEK('P'+ ORDHDR.Account + &lcTmpOrdL..Style,'Spck_Lin')      
      IF FOUND()
        SELECT (lcOrdLTmp)
        *C200331,1 HBG 05/12/2002 Get the full Pack ID [Begin]
        *REPLACE cPackSku WITH Spck_Lin.Pack_Id &&ORDLINE
        REPLACE cPackSku WITH Spck_Lin.Pack_Id+'-'+Spck_Lin.cPkColor+'-'+;
                              lfGetGmSz(Spck_Lin.cPckSize)+'-'+Spck_Lin.cPkVersion &&ORDLINE
        
        *C200331,1 [End]
        REPLACE lSku WITH .F.
      ENDIF
    ENDIF
  ENDIF
ELSE
  SELECT (lcOrdLTmp)
  REPLACE lSku WITH .T.
ENDIF

SELECT Spck_Lin
*C200331,1 HBG 05/12/2002 Set order to the new index [Begin]
*SET ORDER TO Spck_Lin
SET ORDER TO Spck_Linvr
*C200331,1 [End]
SELECT (lcAlas)

*:****************************************************************
*: Name        : lfPrtNot
*: Developer   : Ahmed Abdel Naby - (AAN)
*: Date        : 05/28/2001
*: Purpose     : Function to Print the notepad.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfPrtNot()
*:****************************************************************
*
FUNCTION lfPrtNot
PRIVATE lcAlias

lcAlias = ''
lcAlias = SELECT(0)
SELECT NotePad
GO TOP
IF SEEK('B' + lcOrdNo,'NotePad')
  SELECT (lcOrdLTmp)
  REPLACE cNote WITH NotePad.MNOTES
ENDIF  
SELECT (lcAlias)

*!**************************************************************************
*! Name      : lfGetAddr
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 08/06/2001
*! Purpose   : get the address of the customer
*!**************************************************************************
*! Example   : = lfGetAddr()
*!**************************************************************************
*
FUNCTION lfGetAddr
PARAMETER llDummy
lcCurrAlis = ALIAS()
laSoldTo = ''
laShipTo = ''
IF &lcTmpOrdL..cAccount <>'*****'
  =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) , 'M'+ &lcTmpOrdL..cAccount,'S' + &lcTmpOrdL..cAccount + &lcTmpOrdL..Store),'Customer')
ELSE
  =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) , 'M'+ OrdHdr.Account,'S' + OrdHdr.Account + &lcTmpOrdL..Store),'Customer')
ENDIF  
lcSolTName = CUSTOMER.BTName

IF Customer.Type = 'S' AND Customer.billto = 'M'
  GO TOP IN 'Customer'
  IF &lcTmpOrdL..cAccount <>'*****'
    = SEEK('M'+ &lcTmpOrdL..cAccount ,'Customer')
  ELSE
    = SEEK('M'+ OrdHdr.Account ,'Customer')
  ENDIF
  = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , '2')
ELSE
  = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , '2')
ENDIF  


IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else
  IF &lcTmpOrdL..cAccount <>'*****'
    =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) ,'M'+ &lcTmpOrdL..cAccount,'S' + &lcTmpOrdL..cAccount + &lcTmpOrdL..Store) ,'Customer')
  ELSE
    =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) ,'M'+ OrdHdr.Account,'S' + OrdHdr.Account + &lcTmpOrdL..Store) ,'Customer')  
  ENDIF
  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  *--If there is a distribution center
  IF !EMPTY(lcDistCntr)
    IF &lcTmpOrdL..cAccount <>'*****'
      =SEEK('S' + &lcTmpOrdL..cAccount + lcDistCntr)
    ELSE
      =SEEK('S' + OrdHdr.cAccount + lcDistCntr)
    ENDIF
  ELSE
    IF &lcTmpOrdL..cAccount <>'*****'
      =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) , 'M'+ &lcTmpOrdL..cAccount,'S' + &lcTmpOrdL..cAccount + &lcTmpOrdL..Store))
    ELSE
      =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) , 'M'+ OrdHdr.Account,'S' + OrdHdr.Account + &lcTmpOrdL..Store))
    ENDIF
  ENDIF
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  *--- Get Ship To Phone#
  laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(Customer.Phone1 , lcPhonPict)
ENDIF    && End of IF
SELECT (lcCurrAlis)
RETURN ''
*-- End of lfGetAddr

*:****************************************************************
*: Name        : lfTotUnt
*: Developer   : Ahmed Abdel Naby - (AAN)
*: Date        : 05/28/2001
*: Purpose     : Function to Collect the total units from Ordline.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfTotUnt()
*:****************************************************************
*
FUNCTION lfTotUnt
PRIVATE lcUntAls,lnRec,lcOrd
lcUntAls = SELECT(0)
SELECT OrdLine
lnRec = RECNO()
=SEEK('O'+lcOrdNo,'Ordline')
SELECT ORDLINE
SET RELATION TO
SET RELATION TO lcTempPktk+ Ordline.order+STR(Ordline.lineno,6) INTO Pikline ADDITIVE
*C200331,1 HBG 05/12/2002 Use the full Pack ID [Begin]
*SCAN REST WHILE cOrdType + Order + STR(Lineno,6) = 'O' + lcOrdNo ;
          FOR Pack_Id = lcPack1 .AND. Store = lcStore
SCAN REST WHILE cOrdType + Order + STR(Lineno,6) = 'O' + lcOrdNo ;
          FOR Pack_Id+cPkColor+cPckSize+cPkVersion = lcPack1 .AND. Store = lcStore          
*C200331,1 [End]      
  *=SEEK(lcTempPktk+OrdLine.Order+"D"+STR(OrdLine.Lineno,6),lcTmpOrdL)
  IF EOF("Pikline") .AND. OrdHdr.Status <> "C"
    REPLACE &lcOrdLTmp..nUnits WITH &lcOrdLTmp..nUnits+OrdLine.TotPik
  ELSE
    IF Pikline.PikTkt == lcTempPktk
      REPLACE &lcOrdLTmp..nUnits WITH &lcOrdLTmp..nUnits + Pikline.TotPik
    ENDIF
  ENDIF
ENDSCAN
GOTO lnRec
SELECT (lcUntAls)

*:****************************************************************
*: Name        : lfPcsPak
*: Developer   : Ahmed Abdel Naby - (AAN)
*: Date        : 05/28/2001
*: Purpose     : Function to Collect the no. of pcs per pack.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfPcsPak()
*:****************************************************************
*
FUNCTION lfPcsPak
PRIVATE lcUntAls,lnRec,lcOrd
lcUntAls = SELECT(0)
SELECT Spck_Lin
IF lcAccount<>'*****'
  *C200331,1 HBG 05/12/2002 Use the full Pack ID [Begin]
  *=SEEK('P'+OrdHdr.Account+OrdLine.Pack_id)
  *SCAN REST WHILE Type+Account+Pack_Id+Style = 'P'+OrdHdr.Account+OrdLine.Pack_id
  =SEEK('P'+OrdHdr.Account+OrdLine.Pack_Id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion)
  SCAN REST WHILE Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion+Style =;
                  'P'+OrdHdr.Account+OrdLine.Pack_Id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
    REPLACE &lcOrdLTmp..nPicPak WITH &lcOrdLTmp..nPicPak+Spck_Lin.TotQty
  ENDSCAN
  *C200331,1 [End]
ELSE
  *C200331,1 HBG 05/12/2002 Use the full Pack ID [Begin]
  *=SEEK('P'+'*****'+OrdLine.Pack_id)
  *SCAN REST WHILE Type+Account+Pack_Id+Style = 'P'+'*****'+OrdLine.Pack_id
  =SEEK('P'+'*****'+OrdLine.Pack_Id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion)
  SCAN REST WHILE Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion+Style =;
                  'P'+'*****'+OrdLine.Pack_Id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
    REPLACE &lcOrdLTmp..nPicPak WITH &lcOrdLTmp..nPicPak+Spck_Lin.TotQty
  ENDSCAN
  *C200331,1 [End]
ENDIF
SELECT (lcUntAls)

*!**************************************************************************
*! Name      : lfRest
*: Developer : Ahmed Abdel Naby - (AAN)
*: Date      : 05/28/2001
*! Purpose   : Rest the Total Qty
*!**************************************************************************
*! Example   : = lfRest()
*!**************************************************************************
*
FUNCTION lfRest
PARAMETER llDummy
llEndGroup = .F.

*!**************************************************************************
*! Name      : lfAccChk
*: Developer : Ahmed Abdel Naby - (AAN)
*: Date      : 05/28/2001
*! Purpose   : Rest the Total Qty
*!**************************************************************************
*! Example   : = lfAccChk()
*!**************************************************************************
*
FUNCTION lfAccChk
PRIVATE lcChkAls
lcChkAls = SELECT(0)
SELECT Spck_Lin
IF SEEK('P'+ ORDHDR.Account + lcPack1,'Spck_Lin')
  lcAccount=""
ELSE
  IF SEEK('P'+ '*****' + lcPack1,'Spck_Lin')
    lcAccount='*****'
  ENDIF
ENDIF

SELECT (lcChkAls)

*!**************************************************************************
*! Name      : lfShiped
*: Developer : Ahmed Abdel Naby - (AAN)
*: Date      : 05/28/2001
*! Purpose   : Calculate the shipped Qty
*!**************************************************************************
*! Example   : = lfShiped()
*!**************************************************************************
*
FUNCTION lfShiped
PRIVATE lcShpAls,lcInvNo
lcShpAls = SELECT(0)
SELECT InvHdr
GO TOP
*=SEEK(lcAccount)
=SEEK(OrdHdr.Account)
*LOCATE REST WHILE Account+Invoice = lcAccount FOR PikTkt = lcTempPktk
LOCATE REST WHILE Account+Invoice = OrdHdr.Account FOR PikTkt = lcTempPktk
IF FOUND()
  lcInvNo = Invoice
  SELECT InvLine
  GO TOP
  =SEEK(lcSty+lcInvNo)
  *--- style+invoice+STR(lineno,6)
  *C200331,1 HBG 05/12/2002 Use the full Pack ID [Begin]
  *LOCATE REST WHILE style+invoice+STR(lineno,6) = lcSty+lcInvNo;
              FOR Store+Order = lcStore+lcOrdNo .AND. Pack_Id = lcPack1
  LOCATE REST WHILE style+invoice+STR(lineno,6) = lcSty+lcInvNo;
              FOR Store+Order = lcStore+lcOrdNo .AND.;
              Pack_Id+cPkColor+cPckSize+cPkVersion = lcPack1              
  *C200331,1 [End]           
  IF FOUND()
    REPLACE &lcOrdLTmp..cUnitShp  WITH EVAL("Qty"+ALLT(STR(lncnt)))
  ENDIF
ELSE
  SELECT ConsInvH
  GO TOP
  LOCATE FOR PikTkt = lcTempPktk
  IF FOUND()
    lcInvNo = Invoice
    SELECT ConsInvL
    GO TOP
    IF SEEK(lcInvNo+lcstore+lcordno+lcSty)  
     *C200331,1 HBG 05/12/2002 Use the full Pack ID [Begin]
     *LOCATE REST WHILE invoice+store+order+style+STR(lineno,6)= lcInvNo+lcstore+lcordno+lcSty;
                  FOR Pack_Id = lcPack1
     LOCATE REST WHILE invoice+store+order+style+STR(lineno,6)= lcInvNo+lcstore+lcordno+lcSty;
                  FOR Pack_Id+cPkColor+cPckSize+cPkVersion = lcPack1                  
     *C200331,1 [End]          
      REPLACE &lcOrdLTmp..cUnitShp  WITH EVAL("Qty"+ALLT(STR(lncnt)))
    ENDIF
  ENDIF
ENDIF
SELECT (lcShpAls)

*!**************************************************************************
*! Name      : lfRetNPad
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 28/05/2001
*! Purpose   : Return Notepad
*!**************************************************************************
*! Example   : = lfRetNPad()
*!**************************************************************************
*
FUNCTION lfnote
PARAMETER lcDummy
IF &lcTmpOrdL..cType=='2' AND llRpOrdNot
  IF !EMPTY(ALLTRIM(&lcTmpOrdL..cNote))
    FOR ln=1 TO MEMLINES(&lcTmpOrdL..cNote)
      IF !EMPTY(mline(&lcTmpOrdL..cNote,ln))
        lcNToRet = lcNToRet+IIF(!EMPTY(lcNToRet),CHR(13),'')+ALLTRIM(mline(&lcTmpOrdL..cNote,ln))
      ENDIF
    ENDFOR
  ENDIF
ENDIF
lcNToRet = ALLTRIM(lcNToRet)


*B604854,1 TMI [Start] rewrite the function lpPrtSku
*:**************************************************************************
*:* Name        : lpPrtSku
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/03/2001
*:* Purpose     : Prints Pack#/sku#
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpPrtSku()
*:***************************************************************************
FUNCTION lpPrtSku
PRIVATE lcAlas,lnI,lcPackSku
lcAlas = SELECT(0)
lcPackSku = ''

SELECT Spck_Lin
SET ORDER TO SpckLins   &&  Key  is : TYPE+ACCOUNT+STYLE+PACK_ID

=SEEK('S'+Style.Scale,'Scale')
=SEEK('M'+ORDHDR.Account,'Customer')

*-- If there is  a pack id,then prints it


IF !EMPTY(lcPack1)
  *--search the pack id in the spck_lin file for current account , 
  *--   if not found search for the generic pack with the same pack id
  *C200331,1 HBG 05/12/2002 Set order to the new index and seek by the full Pack ID [Begin] [Begin]
  *IF SEEK('P'+ ORDHDR.Account + &lcTmpOrdL..Style+lcPack1,'Spck_Lin') OR ;
     SEEK('P'+'*****'         + &lcTmpOrdL..Style+lcPack1,'Spck_Lin')
  SET ORDER TO Spck_Linvr IN Spck_Lin
  IF SEEK('P'+ ORDHDR.Account + lcPack1 + &lcTmpOrdL..Style,'Spck_Lin') OR ;
     SEEK('P'+'*****'         + lcPack1 + &lcTmpOrdL..Style,'Spck_Lin')     
    SELECT (lcOrdLTmp)
    *REPLACE cPackSku WITH Spck_Lin.Pack_Id
    REPLACE cPackSku WITH Spck_Lin.Pack_Id+'-'+Spck_Lin.cPkColor+'-'+;
                          lfGetGmSz(Spck_Lin.cPckSize)+'-'+Spck_Lin.cPkVersion
    *C200331,1 [End]
  ENDIF
ELSE
  *C200331,1 HBG 05/12/2002 Use the original index [Begin]
  SET ORDER TO Spcklins IN Spck_Lin
  *C200331,1 [End]
  *-- Otherwise prints the SKU 
  IF SEEK('S'+ORDHDR.Account+&lcTmpOrdL..Style,'Spck_lin')
    IF !EMPTY(PrePak)  && This is a prepack
      *C200331,1 HBG 05/12/2002 get the full Pack ID [Begin]
      *lcPackSku = Spck_lin.Pack_id
      lcPackSku = Spck_lin.Pack_Id+'-'+Spck_lin.cPkColor+'-'+;
                  lfGetGmSz(Spck_Lin.cPckSize)+'-'+Spck_lin.cPkVersion
      *C200331,1 [End]
    ELSE
      *-- Go to the right size in the spck_lin file to get the sku id for this size
      FOR lnI = 1 TO Scale.CNT
        IF EVAL('Spck_lin.Qty'+STR(lnCurSz,1)) = 0
          SKIP IN Spck_lin
        ELSE
          *C200331,1 HBG 05/12/2002 get the full Pack ID [Begin]
          *lcPackSku = Spck_lin.Pack_id
          lcPackSku = Spck_lin.Pack_Id+'-'+Spck_lin.cPkColor+'-'+;
                      lfGetGmSz(Spck_Lin.cPckSize)+'-'+Spck_lin.cPkVersion
          *C200331,1 [End]
          EXIT
        ENDIF
      NEXT
    ENDIF
    SELECT (lcOrdLTmp)
    *--Prints the sku
    REPLACE cPackSku WITH lcPackSku
  ENDIF
  
  *--Mark this line as an sku line
  SELECT (lcOrdLTmp)
  REPLACE lSku     WITH .T. 
ENDIF

SELECT Spck_Lin
*C200331,1 HBG 05/12/2002 Use the new index [Begin]
*SET ORDER TO Spck_Lin  && key is : TYPE+ACCOUNT+PACK_ID+STYLE
SET ORDER TO Spck_Linvr && key is : TYPE+ACCOUNT+PACK_ID+STYLE
*C200331,1 [End]
SELECT (lcAlas)
*-- end of lpPrtSku.
*B604854,1 TMI [End  ]


*B604854,1 TMI [Start] Get UPC from styleUPC file for either pack or a style
*:**************************************************************************
*:* Name        : lfGetUPC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/03/2001
*:* Purpose     : Get UPC from styleUPC file for either pack or a style
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetUPC()
*:***************************************************************************
FUNCTION lfGetUPC
PRIVATE lnSlct,lcSpcLnOrd
lnSlct = SELECT()

SELECT STYLEUPC
lcStUpcOrd = ORDER('STYLEUPC')

*-- Get the Style UPC
SET ORDER TO STYLEUPC IN STYLEUPC
IF SEEK(OrdLine.Style+STR(lncnt,1),'STYLEUPC')
  lcStyleUPC = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
ENDIF  

*--If this is a pack
IF !EMPTY(OrdLine.Pack_id)        
  SET ORDER TO PACKUPC IN STYLEUPC
  *--If this style is in an account pack , get it otherwise get the generic pack
  IF SEEK(ORDLINE.ACCOUNT+ORDLINE.PACK_ID,'STYLEUPC') OR SEEK('*****'+ORDLINE.PACK_ID,'STYLEUPC')
    lcPackUPC = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
  ENDIF
ENDIF
SET ORDER TO &lcStUpcOrd IN STYLEUPC

SELECT (lnSlct)
*--End of lfGetUPC.
*B604854,1 TMI [End  ]

*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : HEnd Ghanem (HBG)
*! Date      : 12/10/2001
*! Purpose   : Function To Size discreption
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfGetGmSz()
*!*************************************************************
*!C#200331
FUNCTION lfGetGmSz
PARAMETER lcPackSize

IF !EMPTY(lcPackSize) AND SEEK('S'+LEFT(lcPackSize,1),'SCALE')
  lcSz = RIGHT(lcPackSize,1)
  lcSize = SCALE.Sz&lcSz
ELSE
  lcSize = '*****'    
ENDIF  

RETURN lcSize 


