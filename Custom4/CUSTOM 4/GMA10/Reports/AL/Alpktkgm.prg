*:****************************************************************************************
*: Program file  : Alpktkgm.Prg
*: Program desc. : Picking Ticket Form for GMA
*: System        : Aria Apparel System (A4).
*: Module        : Sales Order Allocation (AL).
*: Developer     : Mariam Mazhar Tawfik - (MMT) Due to C#038677
*: Date          : 11/01/2004
*:****************************************************************************************
*: Passed Parameters  : None
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

IF TYPE('loStyleUPC') <> 'O'  
  loStyleUPC = CreateObject("RemoteTable","StyleUPC","StyleUpc",lcTempStyleUpc,SET("DATASESSION")) 
ENDIF 
IF TYPE('loInvHdr') <> 'O'  
  loInvHdr = CreateObject("RemoteTable","InvHdr","InvHdra",lcTempInvHdr,SET("DATASESSION")) 
ENDIF 
IF TYPE('loInvLine') <> 'O'  
  loInvLine = CreateObject("RemoteTable","InvLine","InvLines",lcTempInvLine,SET("DATASESSION")) 
ENDIF 
IF TYPE('loConsInvh') <> 'O'  
  loConsInvh = CreateObject("RemoteTable","ConsInvh","ConsInvh",lcTempConsInvh,SET("DATASESSION")) 
ENDIF 
IF TYPE('loConsInvl') <> 'O'  
  loConsInvl = CreateObject("RemoteTable","ConsInvl","ConsInvl",lcTempConsInvl,SET("DATASESSION")) 
ENDIF 

SELECT (lnAlias)
*-- End Declaration variables.
SELECT (lcTmpOrdL)

lcOrdLTmp = loOgScroll.gfTempName()
*-- call function to create temp file.
= lfCrTmpFls ()

*-- New function to collect the data upon the new selection critria
*-- In the optiongrid.

= lfNewColet ()

*-- Return the old relation.
SELECT (lcTmpOrdL)
LOCATE 
loogScroll.cCROrientation = 'L'

RETURN
*-- End Of Form code.

*-- Function Section.
*:****************************************************************
*: Name        : lfNewColet
*: Developer   : Mariam Mazhar Tawfik - (MMT)
*: Date        : 11/01/2004
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

lnPosition = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'ORDHDR.ORDER'),1)
IF lnPosition > 0
 lcTempOrder = loOgScroll.laOGFxFlt[lnPosition,6]
 IF !EMPTY(lcTempOrder)
   SELECT(lcTempOrder)
   INDEX ON Order TAG (lcTempOrder)
 ENDIF   
ENDIF 
SELECT (lcTmpOrdL)
SET FILTER TO
IF !EMPTY(lcTempOrder) .AND. USED(lcTempOrder) .AND. RECCOUNT(lcTempOrder) > 0
  SET FILTER TO SEEK(&lcTmpOrdL..Order,lcTempOrder) .AND. &lcPiktktTemp..Status <> 'X'
ELSE
  SET FILTER TO &lcPiktktTemp..Status<>'X'
ENDIF

LOCATE 

DO WHILE !EOF()
  SELECT (lcTmpOrdL)
  lcTempPktk = PikTkt
  lcOrdNo    = Order
  lcStore    = Store
  lcPack     = &lcOrdLnTmp..Pack_id + &lcOrdLnTmp..cPkColor + &lcOrdLnTmp..cPckSize + &lcOrdLnTmp..cPkVersion
  lcPack1    = lcPack
  lnLoop     = 0
  =lfAccChk()
  SELECT (lcTmpOrdL)
  =SEEK(lcTempPktk)
  SCAN REST WHILE  PikTkt + Order + cGrupDetal + STR(Lineno,6) = lcTempPktk
    lcSty      = &lcOrdLnTmp..Style
    IF cGrupDetal <>"H"
      IF lcPack1 <> &lcOrdLnTmp..Pack_id + &lcOrdLnTmp..cPkColor + &lcOrdLnTmp..cPckSize + &lcOrdLnTmp..cPkVersion
        lcPack1 = &lcOrdLnTmp..Pack_id + &lcOrdLnTmp..cPkColor + &lcOrdLnTmp..cPckSize + &lcOrdLnTmp..cPkVersion
        lnLoop = 0
        REPLACE &lcOrdLTmp..lFinal WITH .T.
        =lfAccChk()
      ENDIF

      =SEEK('S'+&lcOrdLnTmp..Scale,lcScalefile)
      lnSzNo = &lcScalefile..Cnt
      FOR lncnt = 1 to lnSzNo
        SCATTER MEMVAR MEMO
        IF EVAL('M.PIK'+STR(lncnt,1)) > 0
          lnLoop = lnLoop + 1
          INSERT INTO (lcOrdLTmp) FROM MEMVAR
          =SEEK('P'+&lcOrdLnTmp..Scale+&lcOrdLnTmp..PrePak,lcScaleFile)
          *--- Seek SPack Line Get total Qty for Pack
          lnPicPak = &lcScaleFile..PpTot
          IF lcAccount <>'*****'
            IF llUse_config
              =loSpck_lin.SEEK('P'+&lcOrdHdr..Account+&lcOrdLnTmp..Pack_id+&lcOrdLnTmp..cPkColor+;
                      &lcOrdLnTmp..cPckSize + &lcOrdLnTmp..cPkVersion+&lcOrdLnTmp..Style+&lcOrdLnTmp..Dyelot)
           ELSE
             =loSpck_lin.SEEK('P'+&lcOrdHdr..Account+&lcOrdLnTmp..Pack_id+&lcOrdLnTmp..cPkColor+;
                       &lcOrdLnTmp..cPckSize +&lcOrdLnTmp..cPkVersion+&lcOrdLnTmp..Style)
             
           ENDIF                       
          ELSE
            IF llUse_config
              =loSpck_lin.SEEK('P'+'*****'+&lcOrdLnTmp..Pack_id+&lcOrdLnTmp..cPkColor+;
                              OrdLine.cPckSize + OrdLine.cPkVersion+&lcOrdLnTmp..Style+&lcOrdLnTmp..Dyelot)
            ELSE
              =loSpck_lin.SEEK('P'+'*****'+&lcOrdLnTmp..Pack_id+&lcOrdLnTmp..cPkColor+;
                           &lcOrdLnTmp..cPckSize + &lcOrdLnTmp..cPkVersion+&lcOrdLnTmp..Style)

            ENDIF                               
          ENDIF
          =lfRplfld(lncnt)
        ENDIF
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

*Display the units of color size if no pack and the total units of the Pack in the pack case.
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
lcTmpOrdL = lcOrdLTmp
*-- End Of lfNewColet.

*:****************************************************************
*: Name        : lfCrTmpFls
*: Developer   : Mariam Mazhar Tawfik - (MMT)
*: Date        : 11/01/2004
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
DIMENSION laFileStru[lnFileStru + 30, 18]

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
laFileStru[lnFileStru+7,3] = 20
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
laFileStru[lnFileStru+10,3] = 32
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

laFileStru[lnFileStru+28,1] = 'nSizeQty'
laFileStru[lnFileStru+28,2] = 'N'
laFileStru[lnFileStru+28,3] = 6
laFileStru[lnFileStru+28,4] = 0

laFileStru[lnFileStru + 29, 1] = 'cOrdNote2'
laFileStru[lnFileStru + 29, 2] = 'C'
laFileStru[lnFileStru + 29, 3] = 30
laFileStru[lnFileStru + 29, 4] = 0

laFileStru[lnFileStru + 30, 1] = 'cCustPO'
laFileStru[lnFileStru + 30, 2] = 'C'
laFileStru[lnFileStru + 30, 3] = 15
laFileStru[lnFileStru + 30, 4] = 0

nLen = ALEN(laFileStru,1)
DIMENSION laFileStru[nLen+1, 18]
laFileStru[nLen+1,1] = 'nPikUnits'
laFileStru[nLen+1,2] = 'N'
laFileStru[nLen+1,3] = 6
laFileStru[nLen+1,4] = 0

FOR lnLoop = 1 to 31
  STORE ' ' TO  laFileStru[lnFileStru+lnLoop,7],laFileStru[lnFileStru+lnLoop,8],;
                laFileStru[lnFileStru+lnLoop,9],laFileStru[lnFileStru+lnLoop,10],;
                 laFileStru[lnFileStru+lnLoop,11],laFileStru[lnFileStru+lnLoop,12],;
                laFileStru[lnFileStru+lnLoop,13],laFileStru[lnFileStru+lnLoop,14],;
              laFileStru[lnFileStru+lnLoop,15],laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO    laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]

ENDFOR

=gfCrtTmp(lcOrdLTmp,@laFileStru,"PikTkt",lcTmpOrdU,.T.)

SELECT (lnOldAls)

*-- End OF lfCrTmpFls.

*:****************************************************************
*: Name        : lfRplfld
*: Developer   : Mariam Mazhar Tawfik - (MMT)
*: Date        : 11/01/2004
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
FUNCTION lfRplfld
PARAMETER  lnCurSz

PRIVATE lcCurQty, lnSzQty
SELECT(lcOrdLnTmp)
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

STORE '' TO lcPackUPC,lcStyleUPC
=lfGetUPC()
REPLACE dStrtDat  WITH &lcORDHDR..Start      ,;
        cAccount  WITH &lcORDHDR..Account,;
        cCustPO   WITH &lcORDHDR..custPO,;
        dCancDat  WITH &lcORDHDR..Complete   ,;
        dCompDat  WITH IIF(&lcCUSTOMER..PRIORITY = '1', &lcORDHDR..Complete, {})   ,;
        cBulkNo   WITH &lcORDHDR..cFromOrder ,;
        cDept     WITH &lcORDHDR..Dept       ,;
        cOrdNote1 WITH LEFT(&lcORDHDR..Note1,30), ; 
        cOrdNote2 WITH LEFT(&lcORDHDR..Note2,30),;  
        cSeason   WITH gfCodDes(&lcORDHDR..Season , 'SEASON'),;
        cTerms    WITH gfCodDes(&lcORDHDR..ctermcode,'cTermCode'),;
        cStyDesc  WITH &lcStylefile..Desc,;
        cPcSpk    WITH IIF(lcAccount<>'*****',IIF(loSpck_lin.SEEK('P'+&lcORDHDR..Account+&lcOrdLnTmp..Pack_id+;
                       &lcOrdLnTMP..cPkColor+&lcOrdLnTMP..cPckSize+&lcOrdLnTMP..cPkVersion+&lcOrdLnTMP..Style),EVAL("&lcTempSpck_Lin..Qty"+ALLT(STR(lncnt))),0),;
                       IIF(loSpck_lin.SEEK('P'+'*****'+&lcOrdLnTMP..Pack_id+;
                       &lcOrdLnTMP..cPkColor+&lcOrdLnTMP..cPckSize+&lcOrdLnTMP..cPkVersion+&lcOrdLnTMP..Style),EVAL("&lctempSpck_Lin..Qty"+ALLT(STR(lncnt))),0)),;
        nSizeQty  WITH lnSzQty,;
        cDunn     WITH &lcCustomer..Duns,;
        cStyUpc   WITH lcStyleUpc,;
        cUpc      WITH lcPackUpc,;
        cSize     WITH IIF(lnSzNo = 1,'',ALLTRIM(EVAL("&lcScaleFile..Sz"+ALLT(STR(lncnt))))),;
        cQty      WITH STR((nunits/nPicPak)*cPcSpk),;
        cType     WITH '1',;
        nLine     WITH lnLoop,;
        cShipvia  WITH IIF(&lcORDHDR..ShipVia ='*',gfCodDes(&lcCUSTOMER..ShipVia , 'SHIPVIA'),gfCodDes(&lcORDHDR..ShipVia , 'SHIPVIA')),;
        npacks    WITH ALLTRIM(STR(IIF((nunits/npicpak)-INT(nunits/npicpak)>0,INT((nunits/npicpak))+1,nunits/npicpak)))

IF lluse_config
  replace  cPcSpk    WITH IIF(lcAccount<>'*****',IIF(loSpck_Lin.SEEK('P'+&lcOrdHdr..Account+&lcOrdLnTmp..Pack_id+;
                       &lcOrdLnTmp..cPkColor+&lcOrdLnTmp..cPckSize+&lcOrdLnTmp..cPkVersion+&lcOrdLnTmp..Style+&lcOrdLnTmp..Dyelot),EVAL("&lcTempSpck_Lin..Qty"+ALLT(STR(lncnt))),0),;
                       IIF(loSpck_lin.SEEK('P'+'*****'+&lcOrdLnTmp..Pack_id+;
                       &lcOrdLnTmp..cPkColor+&lcOrdLnTmp..cPckSize+&lcOrdLnTmp..cPkVersion+&lcOrdLnTmp..Style+&lcOrdLnTmp..Dyelot),EVAL("&lcTempSpck_Lin..Qty"+ALLT(STR(lncnt))),0))

ENDIF 
SELECT (lcTmpOrdL)
*-- End OF lfRplfld.
*:****************************************************************
*: Name        : lfPrtNot
*: Developer   : Mariam Mazhar Tawfik - (MMT)
*: Date        : 11/01/2004
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
SELECT(lcNotePad)
LOCATE 
IF SEEK('B' + lcOrdNo,lcNotePad)
  SELECT (lcOrdLTmp)
  REPLACE cNote WITH &lcNotePad..MNOTES
ENDIF  
SELECT (lcAlias)

*!**************************************************************************
*! Name      : lfGetAddr
*! Developer : Mariam Mazhar Tawfik - (MMT)
*! Date      : 11/01/2004
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
  =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) , 'M'+ &lcTmpOrdL..cAccount,'S' + &lcTmpOrdL..cAccount + &lcTmpOrdL..Store),lcCustomer)
ELSE
  =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) , 'M'+ &lcOrdHdr..Account,'S' + &lcOrdHdr..Account + &lcTmpOrdL..Store),lcCustomer)
ENDIF  
lcSolTName = &lcCUSTOMER..BTName

IF &lcCustomer..Type = 'S' AND &lcCustomer..billto = 'M'
  GO TOP IN &lcCustomer
  IF &lcTmpOrdL..cAccount <>'*****'
    = SEEK('M'+ &lcTmpOrdL..cAccount ,lcCustomer)
  ELSE
    = SEEK('M'+ &lcOrdHdr..Account ,lcCustomer)
  ENDIF
  = gfGetAdr(lcCustomer, '' , '' , '' , @laSoldTo , '2')
ELSE
  = gfGetAdr(lcCustomer, '' , '' , '' , @laSoldTo , '2')
ENDIF  


IF &lcOrdHdr..Alt_ShpTo
  SELECT(lcORDHDR)
  lcShpTName  = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else
  IF &lcTmpOrdL..cAccount <>'*****'
    =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) ,'M'+ &lcTmpOrdL..cAccount,'S' + &lcTmpOrdL..cAccount + &lcTmpOrdL..Store) ,lcCustomer)
  ELSE
    =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) ,'M'+ &lcOrdHdr..Account,'S' + &lcOrdHdr..Account + &lcTmpOrdL..Store) ,lcCustomer)  
  ENDIF
  SELECT(lcCUSTOMER)
  lcDistCntr = &lcCUSTOMER..Dist_Ctr
  *--If there is a distribution center
  IF !EMPTY(lcDistCntr)
    IF &lcTmpOrdL..cAccount <>'*****'
      =SEEK('S' + &lcTmpOrdL..cAccount + lcDistCntr)
    ELSE
      =SEEK('S' + &lcOrdHdr..cAccount + lcDistCntr)
    ENDIF
  ELSE
    IF &lcTmpOrdL..cAccount <>'*****'
      =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) , 'M'+ &lcTmpOrdL..cAccount,'S' + &lcTmpOrdL..cAccount + &lcTmpOrdL..Store))
    ELSE
      =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) , 'M'+ &lcOrdHdr..Account,'S' + &lcOrdHdr..Account + &lcTmpOrdL..Store))
    ENDIF
  ENDIF
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 4)
  *--- Get Ship To Phone#
  laShipTo[5] = ALLTRIM(gfGetAdr(lcCUSTOMER , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(&lcCustomer..Phone1 , '@R '+lcPhonPict)
  *--MMT,Fixing the problem of phone # format
 
ENDIF    && End of IF
SELECT (lcCurrAlis)
RETURN ''
*-- End of lfGetAddr

*:****************************************************************
*: Name        : lfTotUnt
*: Developer   : Mariam Mazhar Tawfik - (MMT)
*: Date        : 11/01/2004
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
SELECT(lcOrdLnTmp)
lnRec = RECNO()
=SEEK('O'+lcOrdNo,lcOrdLnTmp)
=loPikLine.SEEK(lcTempPktk+ &lcOrdLnTmp..order+STR(&lcOrdLnTmp..lineno,6))
SELECT(lcOrdLnTmp)
SET RELATION TO
*--SET RELATION TO lcTempPktk+ &lcOrdLnTmp..order+STR(&lcOrdLnTmp..lineno,6) INTO Pikline ADDITIVE
SCAN REST WHILE cOrdType + Order + STR(Lineno,6) = 'O' + lcOrdNo ;
          FOR Pack_Id+cPkColor+cPckSize+cPkVersion = lcPack1 .AND. Store = lcStore          
  IF EOF(lcTempPikline) .AND. &lcOrdHdr..Status <> "C"
    REPLACE &lcOrdLTmp..nUnits WITH &lcOrdLTmp..nUnits+&lcOrdLnTmp..TotPik
  ELSE
    IF &lcTempPikline..PikTkt == lcTempPktk
      REPLACE &lcOrdLTmp..nUnits WITH &lcOrdLTmp..nUnits + &lcTempPikline..TotPik
    ENDIF
  ENDIF
ENDSCAN
GOTO lnRec
SELECT (lcUntAls)

*:****************************************************************
*: Name        : lfPcsPak
*: Developer   : Mariam Mazhar Tawfik - (MMT)
*: Date        : 11/01/2004
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
*SELECT Spck_Lin
loSpck_Lin.SetOrder('Spck_Linvr')
*--SET ORDER TO Spck_Linvr
IF lcAccount<>'*****'
  =loSpck_Lin.SEEK('P'+&lcOrdHdr..Account+&lcOrdLnTmp..Pack_Id+&lcOrdLnTmp..cPkColor+&lcOrdLnTmp..cPckSize+&lcOrdLnTmp..cPkVersion)
  SELECT(lcTempSpck_lin)
  SCAN REST WHILE Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion+Style =;
                  'P'+&lcOrdHdr..Account+&lcOrdLnTmp..Pack_Id+&lcOrdLnTmp..cPkColor+&lcOrdLnTmp..cPckSize+&lcOrdLnTmp..cPkVersion
    REPLACE &lcOrdLTmp..nPicPak WITH &lcOrdLTmp..nPicPak+&lcTempSpck_Lin..TotQty
  ENDSCAN

ELSE
  =loSpck_Lin.SEEK('P'+'*****'+&lcOrdLnTmp..Pack_Id+&lcOrdLnTmp..cPkColor+&lcOrdLnTmp..cPckSize+&lcOrdLnTmp..cPkVersion)
  SELECT(lcTempSpck_lin)
  SCAN REST WHILE Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion+Style =;
                  'P'+'*****'+&lcOrdLnTmp..Pack_Id+&lcOrdLnTmp..cPkColor+&lcOrdLnTmp..cPckSize+&lcOrdLnTmp..cPkVersion
    REPLACE &lcOrdLTmp..nPicPak WITH &lcOrdLTmp..nPicPak+&lcTempSpck_Lin..TotQty
  ENDSCAN

ENDIF
SELECT (lcUntAls)

*!**************************************************************************
*! Name      : lfRest
*: Developer : Mariam Mazhar Tawfik (MMT)
*: Date      : 11/01/2004
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
*: Developer : Mariam Mazhar Tawfik (MMT)
*: Date      : 11/01/2004
*! Purpose   : Rest the Total Qty
*!**************************************************************************
*! Example   : = lfAccChk()
*!**************************************************************************
*
FUNCTION lfAccChk
PRIVATE lcChkAls
lcChkAls = SELECT(0)
*SELECT Spck_Lin
loSpck_Lin.SetOrder('Spck_Linvr')
*SET ORDER TO Spck_Linvr
IF loSpck_Lin.SEEK('P'+ &lcORDHDR..Account + lcPack1)
  lcAccount=""
ELSE
  IF loSpck_Lin.SEEK('P'+ '*****' + lcPack1)
    lcAccount='*****'
  ENDIF
ENDIF

SELECT (lcChkAls)

*!**************************************************************************
*! Name      : lfShiped
*: Developer : Mariam Mazhar Tawfik (MMT)
*: Date      : 11/01/2004
*! Purpose   : Calculate the shipped Qty
*!**************************************************************************
*! Example   : = lfShiped()
*!**************************************************************************
*
FUNCTION lfShiped
PRIVATE lcShpAls,lcInvNo
lcShpAls = SELECT(0)
=loInvHdr.SEEK(&lcOrdHdr..Account)
SELECT(lcTempInvHdr)
LOCATE REST WHILE Account+Invoice = &lcOrdHdr..Account FOR PikTkt = lcTempPktk
IF FOUND()
  lcInvNo = Invoice
  =loInvLine.SEEK(lcSty+lcInvNo)
  SELECT(lcTempInvLine)
  LOCATE REST WHILE style+invoice+STR(lineno,6) = lcSty+lcInvNo;
              FOR Store+Order = lcStore+lcOrdNo .AND.;
              Pack_Id+cPkColor+cPckSize+cPkVersion = lcPack1              
  IF FOUND()
    REPLACE &lcOrdLTmp..cUnitShp  WITH EVAL("Qty"+ALLT(STR(lncnt)))
  ENDIF
ELSE
  loConsInvH.SetOrder('PikTkt')
  loConsInvH.Seek(lcTempPktk)
  SELECT(lcTempConsInvH)
  LOCATE FOR PikTkt = lcTempPktk
  IF FOUND()
    lcInvNo = Invoice
    IF  loConsInvL.Seek(lcInvNo+lcstore+lcordno+lcSty)
    SELECT(lcTempConsInvL)
*!*      SELECT ConsInvL
*!*      GO TOP
*    IF SEEK(lcInvNo+lcstore+lcordno+lcSty)  
     LOCATE REST WHILE invoice+store+order+style+STR(lineno,6)= lcInvNo+lcstore+lcordno+lcSty;
                  FOR Pack_Id+cPkColor+cPckSize+cPkVersion = lcPack1                  
      REPLACE &lcOrdLTmp..cUnitShp  WITH EVAL("Qty"+ALLT(STR(lncnt)))
    ENDIF
  ENDIF
ENDIF
SELECT (lcShpAls)

*!**************************************************************************
*! Name      : lfRetNPad
*: Developer : Mariam Mazhar Tawfik (MMT)
*: Date      : 11/01/2004
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

*:**************************************************************************
*:* Name        : lpPrtSku
*:* Developer   : Mariam Mazhar Tawfik (MMT)
*:* Date        : 11/01/2004
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

*SELECT Spck_Lin
*SET ORDER TO SpckLins   &&  Key  is : TYPE+ACCOUNT+STYLE+PACK_ID
loSpck_lin.SetOrder('SPKLNSTCN')
*SET ORDER TO SPKLNSTCN   && TYPE+ACCOUNT+STYLE+DYELOT+PACK_ID 


=SEEK('S'+&lcStyleFile..Scale,lcScaleFile)
=SEEK('M'+&lcORDHDR..Account,lcCustomer)

*-- If there is  a pack id,then prints it


IF !EMPTY(lcPack1)
  loSpck_lin.SetOrder('Spck_Linvr')
  IF loSpck_lin.SEEK('P'+ &lcORDHDR..Account + lcPack1 + &lcTmpOrdL..Style) OR ;
     loSpck_lin.SEEK('P'+'*****'         + lcPack1 + &lcTmpOrdL..Style)     
      SELECT (lcOrdLTmp)
      REPLACE cPackSku WITH &lcTempSpck_Lin..Pack_Id+'-'+&lcTempSpck_Lin..cPkColor+'-'+;
                            lfGetGmSz(&lcTempSpck_Lin..cPckSize)+'-'+&lcTempSpck_Lin..cPkVersion
  ENDIF
ELSE
  loSpck_lin.SetOrder('SPKLNSTCN')
*  SET ORDER TO SPKLNSTCN IN Spck_Lin  && TYPE+ACCOUNT+STYLE+DYELOT+PACK_ID 
  *-- Otherwise prints the SKU 
  IF lluse_config
    IF loSpck_lin.SEEK('S'+&lcORDHDR..Account+&lcTmpOrdL..Style+&lcTmpOrdL..Dyelot)
      SELECT(lcTempSpck_Lin)
      IF !EMPTY(PrePak)  && This is a prepack
        lcPackSku = &lcTempSpck_lin..Pack_Id+'-'+&lcTempSpck_lin..cPkColor+'-'+;
                    lfGetGmSz(&lcTempSpck_Lin..cPckSize)+'-'+&lcTempSpck_lin..cPkVersion
      ELSE
        *-- Go to the right size in the spck_lin file to get the sku id for this size
        FOR lnI = 1 TO &lcScaleFile..CNT
          IF EVAL('&lcTempSpck_lin..Qty'+STR(lnCurSz,1)) = 0
            SKIP IN &lcTempSpck_lin
          ELSE
            lcPackSku = &lcTempSpck_lin..Pack_Id+'-'+&lcTempSpck_lin..cPkColor+'-'+;
                        lfGetGmSz(&lcTempSpck_Lin..cPckSize)+'-'+&lcTempSpck_lin..cPkVersion
            EXIT
          ENDIF
        NEXT
      ENDIF
      SELECT (lcOrdLTmp)
      *--Prints the sku
      REPLACE cPackSku WITH lcPackSku
       ENDIF
  ELSE
    IF loSpck_Lin.SEEK('S'+&lcORDHDR..Account+&lcTmpOrdL..Style)
      SELECT(lcTempSpck_Lin)
      IF !EMPTY(PrePak)  && This is a prepack
        lcPackSku = &lcTempSpck_lin..Pack_Id+'-'+&lcTempSpck_lin..cPkColor+'-'+;
                  lfGetGmSz(&lcTempSpck_Lin..cPckSize)+'-'+&lcTempSpck_lin..cPkVersion
      ELSE
        *-- Go to the right size in the spck_lin file to get the sku id for this size
        FOR lnI = 1 TO &lcScaleFile..CNT
          IF EVAL('&lcTempSpck_lin..Qty'+STR(lnCurSz,1)) = 0
            SKIP IN &lcTempSpck_lin
          ELSE
            lcPackSku = &lcTempSpck_lin..Pack_Id+'-'+&lcTempSpck_lin..cPkColor+'-'+;
                      lfGetGmSz(&lcTempSpck_Lin..cPckSize)+'-'+&lcTempSpck_lin..cPkVersion
         
            EXIT
          ENDIF
          NEXT
        ENDIF
        SELECT (lcOrdLTmp)
        *--Prints the sku
        REPLACE cPackSku WITH lcPackSku
      ENDIF
    ENDIF      
    *--Mark this line as an sku line
    SELECT (lcOrdLTmp)
    REPLACE lSku     WITH .T. 
  ENDIF

loSpck_Lin.SetOrder('Spck_Linvr')
*--SET ORDER TO Spck_Linvr && key is : TYPE+ACCOUNT+PACK_ID+STYLE
SELECT (lcAlas)
*-- end of lpPrtSku.
*:**************************************************************************
*:* Name        : lfGetUPC
*: Developer : Mariam Mazhar Tawfik (MMT)
*: Date      : 11/01/2004
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


*-- Get the Style UPC
loStyleUPC.SetOrder('STYLEUPC')
IF loStyleUPC.SEEK(&lcOrdLnTmp..Style+STR(lncnt,1))
  lcStyleUPC = &lcTempStyleUpc..cUpcNum1+&lcTempStyleUpc..cUpcNum2+&lcTempStyleUpc..cUpcNum3
ENDIF  

*--If this is a pack
IF !EMPTY(&lcOrdLnTmp..Pack_id)        
  loStyleUPC.SetOrder('PACKUPC')
  *--If this style is in an account pack , get it otherwise get the generic pack
  IF loStyleUPC.SEEK(&lcOrdLnTmp..ACCOUNT+&lcOrdLnTmp..PACK_ID) OR loStyleUPC.SEEK('*****'+&lcORDLNTmp..PACK_ID)
    lcPackUPC = &lcTempStyleUpc..cUpcNum1 + &lcTempStyleUpc..cUpcNum2 + &lcTempStyleUpc..cUpcNum3
  ENDIF
ENDIF

SELECT (lnSlct)
*--End of lfGetUPC.

*!*************************************************************
*! Name      : lfGetGmSz
*: Developer : Mariam Mazhar Tawfik (MMT)
*: Date      : 11/01/2004
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

IF !EMPTY(lcPackSize) AND SEEK('S'+LEFT(lcPackSize,1),lcScaleFile)
  lcSz = RIGHT(lcPackSize,1)
  lcSize = &lcScaleFile..Sz&lcSz
ELSE
  lcSize = '*****'    
ENDIF  

RETURN lcSize 


