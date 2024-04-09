*!**************************************************************************
*! Name      : FPMAIN.PRG
*! Developer : TAK 
*! Date      : 06/23/1999
*! Purpose   : Fresh Produce Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*! Modifications : 
*B603603,1 TAK 05/02/00 Cheange the logic of updating the alo & ord qty. 
*C101929,1 SSH 06/09/00  Generate separate sequance no for adornment order.
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars


lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue



*!***************************************************************
*! Name      : lfINI_Var1                              *C200076,1
*! Developer : TAK
*! Date      : 06/23/1999
*! Purpose   : Initialize Customer file Custom fields defaults.
*!***************************************************************
*! Program   : 'ARCUST'  -> Customer screen.
*! Event     : 'INI_VAR' -> Initialize variables (custom fields).
*!***************************************************************
*! Returns   : None.
*!***************************************************************
FUNCTION lfINI_Var1

*laUsrFields[1,6] = gfgetmemvar('M_CLIR')
*laUsrFields[2,6] = gfgetmemvar('M_ASEWET')
*laUsrFields[3,6] = gfgetmemvar('M_MAAP')

lnUserItem = ASCAN(laUsrFields,"LITEMRATIO")
IF lnUserItem > 0
  lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
  laUsrFields[lnUserItem,6] = gfgetmemvar('M_CLIR')
ENDIF  
lnUserItem = ASCAN(laUsrFields,"LALLSHPEXT")
IF lnUserItem > 0
  lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
  laUsrFields[lnUserItem,6] = gfgetmemvar('M_ASEWET')
ENDIF  
lnUserItem = ASCAN(laUsrFields,"NMINALOPER")
IF lnUserItem > 0
  lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
  laUsrFields[lnUserItem,6] = gfgetmemvar('M_MAAP')
ENDIF  
RETURN


*!***************************************************************
*! Name      : lfINI_Var2                            *C200076,1
*! Developer : TAK
*! Date      : 06/23/1999
*! Purpose   : Initialize Style file Custom fields defaults.
*!***************************************************************
*! Program   : 'ICSTYLE' -> Customer screen.
*! Event     : 'INI_VAR' -> Initialize variables (custom fields).
*!***************************************************************
*! Returns   : None.
*!***************************************************************
FUNCTION lfINI_Var2

*--Style file Custom fields defaults.
*laUsrFields[1,6] = gfgetmemvar('M_CLIR')
*laUsrFields[2,6] = 0.00

lnUserItem = ASCAN(laUsrFields,"LITEMRATIO")
IF lnUserItem > 0
  lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
  laUsrFields[lnUserItem,6] = gfgetmemvar('M_CLIR')
  IF EMPTY(laUsrFields[lnUserItem,6])
    laUsrFields[lnUserItem,6] = .F.
  ENDIF
ENDIF  
lnUserItem = ASCAN(laUsrFields,"NAVGADNCST")
IF lnUserItem > 0
  lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
  laUsrFields[lnUserItem,6] = 0.00
ENDIF  
RETURN


*!***************************************************************
*! Name      : lfINI_Var3                            *C200081,1
*! Developer : Reham Al-Allamy
*! Date      : 06/27/1999
*! Purpose   : Get Custom fields values in the sles order program.
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'INI_VAR' -> Get Values (custom fields).
*!***************************************************************
*! Returns   : None.
*!***************************************************************
*
FUNCTION lfINI_Var3
PARAMETERS lcCust
PRIVATE lcCust

*-- If there is customer ID was entered.
IF !EMPTY(laData[2])
  *-- Search in the customer file to get the default values custom fields.
  IF SEEK("M"+laData[2] , "CUSTOMER")

    *-- Allow ship early with extended terms.
    *laUsrFields[1,6] = CUSTOMER.lAllShpExt
    lnUserItem = ASCAN(laUsrFields,"LALLSHPEXT")
    IF lnUserItem > 0
      lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
      laUsrFields[lnUserItem,6] = CUSTOMER.lAllShpExt
    ENDIF  

    *-- Check Line item ratio.
    *laUsrFields[2,6] = CUSTOMER.lItemRatio
    lnUserItem = ASCAN(laUsrFields,"LITEMRATIO")
    IF lnUserItem > 0
      lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
      laUsrFields[lnUserItem,6] = CUSTOMER.lItemRatio
    ENDIF  

    *-- Mininmum Allowable Allocation.
    *laUsrFields[3,6] = CUSTOMER.nMinAloPer
    lnUserItem = ASCAN(laUsrFields,"NMINALOPER")
    IF lnUserItem > 0
      lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
      laUsrFields[lnUserItem,6] = CUSTOMER.nMinAloPer
    ENDIF  

    *-- Allow Backorder.
    *laUsrFields[5,6] = IIF(CUSTOMER.cbackord $ "AI" , .T. , .F.)
    lnUserItem = ASCAN(laUsrFields,"LALWBKORD")
    IF lnUserItem > 0
      lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
      laUsrFields[lnUserItem,6] = IIF(CUSTOMER.cbackord $ "AI" , .T. , .F.)
    ENDIF  

  ENDIF

  *-- Allocated Percentage.
  *laUsrFields[4,6] = 0.00
  lnUserItem = ASCAN(laUsrFields,"NNETALOQTY")
  IF lnUserItem > 0
    lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
    laUsrFields[lnUserItem,6] = 0.00
  ENDIF  

ENDIF


*!***************************************************************
*! Name      : lfALO_PNT1                            *C200081,1
*! Developer : Reham Al-Allamy
*! Date      : 06/29/1999
*! Purpose   : Calculate the allocated % "Custom field" in sales 
*!           : order program.
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'ALO_PNT' -> Calculate allocated Value (custom fields).
*!***************************************************************
*! Returns   : None.
*!***************************************************************
*
FUNCTION lfALO_PNT1
PRIVATE lnCount , lnCurCnt , lcCrAlias , lnCrRecNo , lnTotPik

*-- Variable hold the total Piked Qty.
lnTotPik    = 0
*-- Variable hold the count of the allocated amount in the user fields array.
lnCurCnt    = 0

*-- Loop in the user fields array to get the row of the allocated amount
FOR lnCount = 1 TO ALEN(laUsrFields,1)
  IF UPPER(ALLTRIM(laUsrFields[lnCount,1])) = "NNETALOQTY"
    lnCurCnt = lnCount
  ENDIF
ENDFOR

*-- If the returned array row not equal zero (means find the allocated amount row)
IF !(lnCurCnt = 0)
  *-- If add mode, means that the allocated qty. still zero, so it's percentage will be zero.
  IF laScrMode[4] .OR. EMPTY(laData[1])
    laogfxflt[lnCurCnt,6] = 0
  ELSE
    *-- Save current alias.
    lcCrAlias = ALIAS()
    *-- Save current record pointer in the current alias.
    lnCrRecNo = RECNO()
    *-- Select order lines file.
    SELECT OrdLine
    *-- Calculate the total pikked qty. for the current order.
    *SUM TotPik For cOrdType+Order+STR(LineNo,6) = "O"+laData[1] .AND. PikTkt <> "******" TO lnTotPik
    SEEK "O"+laData[1] 
    SUM REST TotQty,TotPik WHILE cOrdType+Order+STR(LineNo,6) = "O"+laData[1] TO lnTotOpn,lnTotPik
    *-- If the open qty. not equal zero, calculate the percentage.
    IF lnTotOpn <> 0
      *-- Calculate the allocated percentage & save it in its row element.
      laogfxflt[lnCurCnt,6] = ROUND((lnTotPik / lnTotOpn) * 100 , 2)
    ELSE
      laogfxflt[lnCurCnt,6] = 0
    ENDIF
  ENDIF
ENDIF

*-- Change the when variable to make this field for view only.
lcWhen = " WHEN .F."
*-- Change the diplay picture variable. 
lcPict = "999.99 %"
*-- Return with the current object picture.
RETURN "999.99 %"

*!***************************************************************
*! Name      : lfALO_RES                            *C200081,1
*! Developer : Reham Al-Allamy
*! Date      : 07/01/1999
*! Purpose   : Function to be executed upon saving to restore the 
*!           : value of the allocation amount field "nNetAloQty"
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'ALO_RES' -> Function to restore the allocation 
*!           :              amount upon saving.
*!***************************************************************
*! Returns   : None.
*!***************************************************************
*
FUNCTION lfALO_RES

PRIVATE lnCount , lnCurCnt

*-- Variable hold the count of the allocated amount in the user fields array.
lnCurCnt    = 0

*-- Loop in the user fields array to get the row of the allocated amount
FOR lnCount = 1 TO ALEN(laUsrFields,1)
  IF UPPER(ALLTRIM(laUsrFields[lnCount,1])) = "NNETALOQTY"
    lnCurCnt = lnCount
  ENDIF
ENDFOR

*-- If the returned array row not equal zero (means find the allocated amount row)
IF !(lnCurCnt = 0)
  *-- Default the value of the allocated amount in the user fields array 
  *-- to its value from the order header file.
  laUsrField[lnCurCnt,6] = IIF(laScrMode[4] .OR. EMPTY(laData[1]) , 0 , OrdHdr.nNetAloQty)
ENDIF

*!***************************************************************
*! Name      : lfWHN_STR                            *C200081,1
*! Developer : Reham Al-Allamy
*! Date      : 06/29/1999
*! Purpose   : When function for start date.
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'WHN_STR' -> When function for start date.
*!***************************************************************
*! Returns   : None.
*!***************************************************************
*
FUNCTION lfWHN_STR

*!***************************************************************
*! Name      : lfVLD_STR                            *C200081,1
*! Developer : Reham Al-Allamy
*! Date      : 06/29/1999
*! Purpose   : Valid function for start date.
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'VLD_STR' -> Valid function for start date.
*!***************************************************************
*! Returns   : None.
*!***************************************************************
*
FUNCTION lfVLD_STR

*!***************************************************************
*! Name      : lfWHN_CMP                            *C200081,1
*! Developer : Reham Al-Allamy
*! Date      : 06/29/1999
*! Purpose   : When function for complete date.
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'WHN_CMP' -> When function for complete date.
*!***************************************************************
*! Returns   : None.
*!***************************************************************
*
FUNCTION lfWHN_CMP

*!***************************************************************
*! Name      : lfVLD_CMP                            *C200081,1
*! Developer : Reham Al-Allamy
*! Date      : 06/29/1999
*! Purpose   : Valid function for complete date.
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'VLD_CMP' -> Valid function for complete date.
*!***************************************************************
*! Returns   : None.
*!***************************************************************
*
FUNCTION lfVLD_CMP

*!***************************************************************
*! Name      : lfORD_SAV                            *C200081,1
*! Developer : Reham Al-Allamy
*! Date      : 06/29/1999
*! Purpose   : Function to be executed upon saving
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'ORD_SAV' -> Function to be executed upon saving.
*!***************************************************************
*! Returns   : None.
*!***************************************************************
*
FUNCTION lfORD_SAV

*!***************************************************************
*! Name      : lfSTY_SAV                             *C200076,1
*! Developer : TAK
*! Date      : 06/30/1999
*! Purpose   : Save Custom fields in Style file.
*!***************************************************************
*! Program   : 'ICSTYLE' -> Customer screen.
*! Event     : 'STY_SAV' -> Save (custom fields).
*!                          Save in all colors if there. 
*!***************************************************************
*! Returns   : None.
*!***************************************************************
FUNCTION lfSTY_SAV   

lnUsrItm1 = ASCAN(laUsrFields,"LITEMRATIO")
IF lnUsrItm1 > 0
  lnUsrItm1 = ASUBSCRIPT(laUsrFields ,lnUsrItm1 , 1)  
ENDIF  
lnUsrItm2 = ASCAN(laUsrFields,"NAVGADNCST")
IF lnUsrItm2 > 0
  lnUsrItm2 = ASUBSCRIPT(laUsrFields ,lnUsrItm2 , 1)  
ENDIF  

SELECT STYLE
SEEK lcMajor+lcNMKey
SCAN WHILE Style=lcMajor+lcNMKey
  =RLOCK()
  REPLACE lItemRatio WITH laUsrFields[lnUsrItm1,6],;
          nAvgAdnCst WITH laUsrFields[lnUsrItm2,6]
  UNLOCK
ENDSCAN
RETURN


*!*********************************************************************
*! Name      : lfCHG_OPN,lfCHG_HLD,lfAPR_OPN,lfAPR_HLD
*! Developer : TAK
*! Date      : 11/23/1999
*! Purpose   : 
*!  To Control order status changes from Hold to Open and the oposite.
*!*********************************************************************
FUNCTION lfCHG_OPN
=lfChngOSts('O')
FUNCTION lfCHG_HLD
=lfChngOSts('H')
FUNCTION lfAPR_OPN
=lfChngOSts('O')
FUNCTION lfAPR_HLD
=lfChngOSts('H')

*******************
FUNCTION lfChngOSts
PARA lcNewOStats
PRIVATE lcAlias
lcAlias=SELECT()
=gfOpenFile(gcDataDir+'PikTkt','PikTkt','SH')
=gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')

SELECT ORDLINE
lnOLRec = IIF(!EOF(),RECNO(),0)
=SEEK(ORDHDR.cOrdType+ORDHDR.Order)
SCAN REST WHILE cOrdType+Order = ORDHDR.cOrdType+ORDHDR.Order ;
            FOR !EMPTY(Piktkt) AND Piktkt<>'*'
  IF SEEK(ORDLINE.PikTkt,'PIKTKT') AND PIKTKT.Status = IIF(lcNewOStats='O','H','O')
    REPLACE PIKTKT.Status WITH lcNewOStats
  ENDIF  
ENDSCAN
IF lnOLRec<>0
  GOTO lnOLRec
ENDIF
SELECT (lcAlias)
RETURN

*!***************************************************************
*! Name      : LFCANL_ORD
*! Developer : TAK
*! Date      : 11/30/1999
*! Purpose   : Not allow to cancel order if piktkt exist.
*!***************************************************************
*! Program   : 'SOORD'  -> Order cancel.
*! Event     : 'TRMTE_CANL'
*!***************************************************************
*B603318,1
FUNCTION LFCANL_ORD
*--Some order lines have been picked, You should release the pick ticket(s) first, Unable to cancel the order.
= gfModalGen('INM44092B00000','ALERT')
SELECT ORDHDR
=gfObj_lock(.F.)
RETURN


*!***************************************************************
*! Name      : LFCHKADOPK
*! Developer : TAK
*! Date      : 12/03/1999
*! Purpose   : Not allow to release piktkt from piktkt screen.
*!***************************************************************
*! Program   : 'SOORD'  -> Piktkt delete.
*! Event     : 'NOCHAGMSG '
*!***************************************************************
*B603318,1
FUNCTION LFCHKADOPK
IF llRelPik
  *--Only Blank pick ticket can be released, the Adornment pickets should be released throw the Release Pick Tickets screen.
  = gfModalGen('TRM44094B00000','ALERT')
  SELECT PIKTKT
  =gfObj_lock(.F.)
ELSE
  *--Only Blank pick ticket can be modified, the Adornment pickets should be modified or pulled throw the Issue/Receive Adornment order process.
  =gfModalGen('TRM44093B00000','ALERT')
ENDIF
RETURN



*!***************************************************************
*! Name      : lfSpcilSet                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/07/1999
*! Purpose   : Allocation Special Settings
*!***************************************************************
*! Program   : 'ALAUTAL'  -> Automatic Allocation screen.
*! Event     : 'SPC_SETS' -> Special Setting evant
*!***************************************************************
*! Returns   : False
*!***************************************************************
FUNCTION lfSpcilSet
PARAMETERS lcSetParm

DO CASE

  *-- Initial settings
  CASE lcSetParm = "INI"
    lnPanArrLn = 3
    =lfStorStru()

  *-- Default Select grid settings.
  CASE (lcSetParm = "SLCWHEN") AND (lnOGSeting = 1)
    PRIVATE lnDyeVisWn , ldHighDate
    lnDyeVisWn = gfGetMemVar("M_DOVW")
    ldHighDate = gdSysDate + lnDyeVisWn
    lnStartPos = lfItmPos('ORDHDR.START')
    IF lnStartPos > 0
      laOGFxFlt[lnStartPos,6] = "  /  /    |" + DTOC(ldHighDate)
    ENDIF

  *-- Create Temporary Stores File.
  CASE lcSetParm = "CRTTMP"
    =lfCrtTmpSt()

  *-- When Quit Program.
  CASE lcSetParm = "QUITPRG"
    =lfFPQuit()

ENDCASE
RETURN .F.
*-- end of lfSpcilSet.

*!***************************************************************
*! Name      : lfStorStru                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/07/1999
*! Purpose   : Fill store structure into array.
*!***************************************************************
*!
FUNCTION lfStorStru
PRIVATE lnFldsCnt , lnFldPos , lcFldName
lnFldsCnt = 0

DIMENSION laStorStru[11,4]

laStorStru[1,1] = "Order"
laStorStru[1,2] = "C"
laStorStru[1,3] = 6
laStorStru[1,4] = 0

laStorStru[2,1] = "Style"
laStorStru[2,2] = "C"
laStorStru[2,3] = 19
laStorStru[2,4] = 0

laStorStru[3,1] = "mAddedKey"
laStorStru[3,2] = "M"
laStorStru[3,3] = 10
laStorStru[3,4] = 0
  
FOR lnFldsCnt = 4 TO 11
  
  IF BETWEEN(lnFldsCnt,4,11)
    lnFldPos  = lnFldsCnt - 3
    lcFldName = "nTotStyRq"
  ENDIF
  
  laStorStru[lnFldsCnt,1] = lcFldName + STR(lnFldPos,1)
  laStorStru[lnFldsCnt,2] = "N"
  laStorStru[lnFldsCnt,3] = 6
  laStorStru[lnFldsCnt,4] = 0
  
ENDFOR
*-- end of lfStorStru.

*!***************************************************************
*! Name      : lfCrtTmpSt                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/07/1999
*! Purpose   : Create temporary store file
*!***************************************************************
*!
FUNCTION lfCrtTmpSt
PRIVATE lnCurrAlis
lnCurrAlis = SELECT(0)
IF USED(lcTmpStore)
  USE IN (lcTmpStore)
ENDIF  
DIMENSION laStrIndex[1,2]
laStrIndex[1,1]  = 'Order+Style'
laStrIndex[1,2]  = lcTmpStore
=gfCrtTmp(lcTmpStore , @laStorStru , @laStrIndex)
SELECT (lnCurrAlis)
*-- end of lfCrtTmpSt.

*!***************************************************************
*! Name      : lfFPQuit                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/07/1999
*! Purpose   : Quit Automatic Allocation Program
*!***************************************************************
*!
FUNCTION lfFPQuit
PRIVATE lnCurrAls
lnCurrAls = SELECT(0)
IF USED(lcTmpStore)  && We are going to close the Temp Order line & Erase it.
  USE IN (lcTmpStore)
ENDIF
IF FILE(gcWorkdir + lcTmpStore + '.DBF')
  ERASE &gcWorkdir.&lcTmpStore..DBF          && Erase the Temp file.
ENDIF
IF FILE(gcWorkdir + lcTmpStore + '.CDX')
  ERASE &gcWorkdir.&lcTmpStore..CDX          && Erase the Temp index.
ENDIF
IF FILE(gcWorkdir + lcTmpStore + '.FPT')
  ERASE &gcWorkdir.&lcTmpStore..FPT          && Erase the Temp memo.
ENDIF
SELECT (lnCurrAls)
*-- end of lfFPQuit.

*!***************************************************************
*! Name      : lfStorMang                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/07/1999
*! Purpose   : Manage Dealing with with stores (Distribute Allocation)
*!***************************************************************
*! Program   : 'ALAUTAL'    -> Automatic Allocation screen.
*! Event     : 'AUALOSTORE' -> Allocate with distributed quantity logic
*!***************************************************************
*! Returns   : Value indicate insert line or not.
*!***************************************************************
FUNCTION lfStorMang
PARAMETERS lcSetParm
PRIVATE lnCurAlias , lnI , laOrdReq
lnCurAlias = SELECT(0)
DIMENSION  laOrdReq[8]
STORE 0 TO lnI, laOrdReq

*-- Evaluate we can include this line, and increment store fields.
IF (lcSetParm = "EVALLINE")

  *-- no picked lines (Only empty or allocated)
  IF Ordline.Picked AND !('******' $ Ordline.PikTkt)
    RETURN .F.
  ENDIF
    
  *-- if it was multi store order.
  IF OrdHdr.Multi = "Y"
    =lfFilStore()
  ENDIF  

ENDIF

*-- if it is multi store and we want to delete this line decrement 
*-- store fields.
IF (lcSetParm = "DELESTORE") AND (OrdHdr.Multi = "Y")
  =lfDecStore()
ENDIF

SELECT (lnCurAlias)
RETURN .T.
*-- end of lfStorMang.

*!***************************************************************
*! Name      : lfFilStore                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/07/1999
*! Purpose   : Fill stores temp. file
*!***************************************************************
*!
FUNCTION lfFilStore
PARAMETERS llSkipDefn

IF !llSkipDefn
  PRIVATE lcThisOrd , lcThisStyl
  lcThisOrd  = Ordline.ORDER
  lcThisStyl = Ordline.STYLE
  =lfFilOrdRq()
ENDIF  

*-- if you found this Order + style before.
IF SEEK(lcThisOrd + lcThisStyl , lcTmpStore)
  SELECT (lcTmpStore)
  *-- Note that Put Max because laOrdReq may have -Ve values in 
  *--           case of Manual allocation.
  REPLACE nTotStyRq1 WITH MAX(nTotStyRq1 + laOrdReq[1],0) ,;  
          nTotStyRq2 WITH MAX(nTotStyRq2 + laOrdReq[2],0) ,;  
          nTotStyRq3 WITH MAX(nTotStyRq3 + laOrdReq[3],0) ,;  
          nTotStyRq4 WITH MAX(nTotStyRq4 + laOrdReq[4],0) ,;  
          nTotStyRq5 WITH MAX(nTotStyRq5 + laOrdReq[5],0) ,;  
          nTotStyRq6 WITH MAX(nTotStyRq6 + laOrdReq[6],0) ,;  
          nTotStyRq7 WITH MAX(nTotStyRq7 + laOrdReq[7],0) ,;  
          nTotStyRq8 WITH MAX(nTotStyRq8 + laOrdReq[8],0)
ELSE
  INSERT INTO (lcTmpStore)                                  ;
           (Order, Style,                                   ;
            nTotStyRq1, nTotStyRq2, nTotStyRq3, nTotStyRq4 ,;
            nTotStyRq5, nTotStyRq6, nTotStyRq7, nTotStyRq8 );
    VALUES (lcThisOrd, lcThisStyl,                          ;
            laOrdReq[1],laOrdReq[2],laOrdReq[3],laOrdReq[4],;
            laOrdReq[5],laOrdReq[6],laOrdReq[7],laOrdReq[8])

ENDIF
*-- end of lfFilStore.

*!***************************************************************
*! Name      : lfDecStore                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/07/1999
*! Purpose   : Decrement stores from temp. file
*!***************************************************************
*!
FUNCTION lfDecStore
PARAMETERS llSkipDefn

IF !llSkipDefn
  PRIVATE lcThisOrd , lcThisStyl
  lcThisOrd  = &lcTmpOrdLn..ORDER
  lcThisStyl = &lcTmpOrdLn..STYLE
  =lfFilOrdRq()
ENDIF  

IF SEEK(lcThisOrd + lcThisStyl , lcTmpStore)
  SELECT (lcTmpStore)
  REPLACE nTotStyRq1 WITH MAX(nTotStyRq1 - laOrdReq[1],0) ,;  
          nTotStyRq2 WITH MAX(nTotStyRq2 - laOrdReq[2],0) ,;  
          nTotStyRq3 WITH MAX(nTotStyRq3 - laOrdReq[3],0) ,;  
          nTotStyRq4 WITH MAX(nTotStyRq4 - laOrdReq[4],0) ,;  
          nTotStyRq5 WITH MAX(nTotStyRq5 - laOrdReq[5],0) ,;  
          nTotStyRq6 WITH MAX(nTotStyRq6 - laOrdReq[6],0) ,;  
          nTotStyRq7 WITH MAX(nTotStyRq7 - laOrdReq[7],0) ,;  
          nTotStyRq8 WITH MAX(nTotStyRq8 - laOrdReq[8],0)

  *-- Add Decremented lines Key to Memo Field. [Begin]
  IF llSkipDefn
    lcReplacIt = ""
    IF !EMPTY(mAddedKey)
      lcReplacIt = lcReplacIt + "|"
    ENDIF
    SELECT (lcTmpOrdLn)
    lcReplacIt = lcReplacIt + EVALUATE(lcIndexExp)

    SELECT (lcTmpStore)
    REPLACE mAddedKey WITH mAddedKey + lcReplacIt
  ENDIF  
  *-- Add Decremented lines Key to Memo Field. [End  ]
  
ENDIF          
*-- end of lfDecStore.

*!***************************************************************
*! Name      : lfFilOrdRq                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/08/1999
*! Purpose   : Fill Required quantity array
*!***************************************************************
*!
FUNCTION lfFilOrdRq
PRIVATE lcOrgQty , lcOrdQty
IF llExclude
  SELECT (lcTmpOrdLn)
  IF (lcOldSlct $ 'KP')
    lcOrgQty = IIF(lcRpScpMod$"KP","CUTPICK.Qty","Qty")
  ELSE
    lcOrgQty = "Qty"
  ENDIF  
ELSE
  SELECT ORDLINE
  lcOrgQty = IIF(lcRpScpMod$"KP","CUTPICK.Qty","Qty")
ENDIF

FOR lnI = 1 TO 8
  lcOrdQty      = lcOrgQty + STR(lnI,1)
  laOrdReq[lnI] = MAX(&lcOrdQty,0)
ENDFOR
*-- end of lfFilOrdRq.

*!***************************************************************
*! Name      : lfAloRatio                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/13/1999
*! Purpose   : Temporary Allocation Process
*!***************************************************************
*! Program   : 'ALAUTAL'  -> Automatic Allocation screen.
*! Event     : 'ALLOCATE' -> Evaluate Allocate Ratio.
*!***************************************************************
*! Returns   : False
*!***************************************************************
FUNCTION lfAloRatio
PRIVATE lnCurrAls , lcThisOrd , lcThisSty , laOrdReq, lnI, lcI
lnCurrAls = SELECT(0)

SELECT (lcTmpOrdLn)
IF SEEK(Order + Style , lcTmpStore)
  lcThisOrd  = Order
  lcThisStyl = Style
  DIMENSION laOrdReq[8]
  STORE 0 TO lnI , laOrdReq , laAllocPer
  
  SELECT (lcTmpStore)
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    laOrdReq[lnI]   = MAX(IIF(lcRpScpMod$"KP",&lcTmpOrdLn..Cut&lcI,;
                                              &lcTmpOrdLn..Qty&lcI),0) 
    laAllocPer[lnI] = IIF(nTotStyRq&lcI=0,0,laOrdReq[lnI]/nTotStyRq&lcI)
  ENDFOR
  
  =lfDecStore(.T.)
ENDIF

SELECT (lnCurrAls)
*-- end of lfAloRatio.

*!***************************************************************
*! Name      : lfAloToMst                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/14/1999
*! Purpose   : Allocate to master file
*!***************************************************************
*! Program   : 'ALAUTAL'  -> Automatic Allocation screen.
*! Event     : 'MASTALOC' -> Allocate to master ordhdr file.
*!***************************************************************
FUNCTION lfAloToMst
*-- if it is FP Step
IF nProcNo = lnProcVal
  lnProcVal = lnProcVal + 1
  =SEEK("O" + Order , "OrdHdr")
  SELECT ORDHDR
  = RLOCK()
  REPLACE nNetAloQty WITH nNetAloQty + &lcTmpOrdLn..Tot_PoAlo
  =gfAdd_Info("ORDHDR")
  UNLOCK
  SELECT (lcTmpOrdLn)
  =RLOCK()
  REPLACE nProcNo WITH lnProcVal
  UNLOCK
ENDIF
*-- end of lfAloToMst.

*!***************************************************************
*! Name      : lfRelFrMst                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/14/1999
*! Purpose   : Release from master file
*!***************************************************************
*! Program   : 'ALAUTAL'  -> Automatic Allocation screen.
*! Event     : 'MASTRELE' -> Release from master ordhdr file.
*!***************************************************************
FUNCTION lfRelFrMst
PRIVATE lnCurrAlis
lnCurrAlis = SELECT (0)

=SEEK("O" + Order , "OrdHdr")
SELECT ORDHDR
= RLOCK()
REPLACE nNetAloQty WITH MAX(nNetAloQty - &lcTmpOrdLn..TotPik,0)
=gfAdd_Info("ORDHDR")
UNLOCK
SELECT (lnCurrAlis)
*-- end of lfRelFrMst.

*!***************************************************************
*! Name      : lfUnCmpFP1                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/14/1999
*! Purpose   : Allocation uncomplete session function.
*!***************************************************************
*! Program   : 'ALAUTAL'    -> Automatic Allocation screen.
*! Event     : 'UNCOMPLETE' -> Release from master ordhdr file.
*!***************************************************************
*!
FUNCTION lfUnCmpFP1
lcFiles = lcFiles + "lcTmpStore,"  + lcTmpStore+","  + ORDER(lcTmpStore)+";"
*-- end of lfUnCmpFP1.

*!***************************************************************
*! Name      : lfAlAutSel                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/14/1999
*! Purpose   : Keep track of select / Unselect Record
*!***************************************************************
*! Program   : 'ALAUTAL'    -> Automatic Allocation screen.
*! Event     : 'ALLOCSELCT' -> Automatic allocation select/Unselect Record(s).
*!***************************************************************
*!
FUNCTION lfAlAutSel
PARAMETERS llRestAgan
*-- if it is multi store order.
IF llRestAgan OR SEEK(Order + Style , lcTmpStore)
  PRIVATE lcThisOrd , lcThisStyl , laOrdReq
  lcThisOrd  = ORDER
  lcThisStyl = STYLE
  DIMENSION laOrdReq[8]
  laOrdReq = 0

  =lfEvalQty()  && Calculate Required Quantities Size by Size.

  IF llSel
    =lfFilStore(.T.)
  ELSE
    =lfDecStore(.T.)
  ENDIF

ENDIF
*-- end of lfAlAutSel.

*!***************************************************************
*! Name      : lfCancelns                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/14/1999
*! Purpose   : Cancel Ratio for not selected lines
*!***************************************************************
*! Program   : 'ALAUTAL'    -> Automatic Allocation screen.
*! Event     : 'ALLOCANCEL' -> Automatic allocation cancel record(s).
*!***************************************************************
*!
FUNCTION lfCancelns
SELECT (lcTmpOrdLn)
SCAN FOR !llSel
  *-- if it is multi store order.
  IF SEEK(Order + Style , lcTmpStore)
    =lfAlAutSel(.T.)
  ENDIF  
ENDSCAN
*-- end of lfCancelns.

*!***************************************************************
*! Name      : lfManuAloc                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/14/1999
*! Purpose   : Issue manual allocation from within automatic allocation screen.
*!***************************************************************
*! Program   : 'ALAUTAL'  -> Automatic Allocation screen.
*! Event     : 'MANUAL'   -> Automatic allocation cancel record(s).
*!***************************************************************
*!
FUNCTION lfManuAloc

SELECT (lcTmpOrdLn)
=SEEK("O" + Order , "OrdHdr")
SELECT ORDHDR
= RLOCK()
REPLACE nNetAloQty WITH nNetAloQty + &lcTmpOrdLn..Tot_PoAlo
=gfAdd_Info("ORDHDR")
UNLOCK
SELECT (lcTmpOrdLn)
*-- end of lfManuAloc.

*!***************************************************************
*! Name      : lfAddAgain                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/14/1999
*! Purpose   : Cancel Ratio for not selected lines
*!***************************************************************
*! Program   : 'ALAUTAL'    -> Automatic Allocation screen.
*! Event     : 'ADJRATIO'   -> Automatic allocation Adjust Ratio.
*!***************************************************************
*!
FUNCTION lfAddAgain
PRIVATE laOrdReq , lcThisOrd , lcThisStyl

SELECT (lcTmpOrdLn)
SCAN FOR SEEK(Order + Style , lcTmpStore) AND ;
         EVALUATE(lcIndexExp) $ &lcTmpStore..mAddedKey

  *-- if it is selected record add it again.
  IF llSel
    lcThisOrd  = ORDER
    lcThisStyl = STYLE
    DIMENSION laOrdReq[8]
    laOrdReq = 0
    =lfEvalQty()         && Evaluate Quantity to add Size by Size.
    =lfFilStore(.T.)     && Update Sizes to activate new Ratio.
  ENDIF  
  =lfDelKey()            && Remove key from memo field.
  
ENDSCAN

GO lnBrRecNo
*-- end of lfAddAgain.

*!***************************************************************
*! Name      : lfAdjLine                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/14/1999
*! Purpose   : Cancel Ratio for current line because no avaliability.
*!***************************************************************
*! Program   : 'ALAUTAL'   -> Automatic Allocation screen.
*! Event     : 'ADJLINE'   -> Automatic allocation Adjust Line Ratio.
*!***************************************************************
*!
FUNCTION lfAdjLine
PRIVATE laOrdReq , lcThisOrd , lcThisStyl

SELECT (lcTmpOrdLn)
IF SEEK(Order + Style , lcTmpStore) AND ;
         EVALUATE(lcIndexExp) $ &lcTmpStore..mAddedKey
  lcThisOrd  = ORDER
  lcThisStyl = STYLE
  DIMENSION laOrdReq[8]
  laOrdReq = 0
  =lfEvalQty()         && Evaluate Quantity to add Size by Size.
  =lfFilStore(.T.)     && Update Sizes to activate new Ratio.
  =lfDelKey()          && Remove key from memo field.
ENDIF
*-- end of lfAddAgain.

*!***************************************************************
*! Name      : lfEvalQty                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/13/1999
*! Purpose   : Evaluate required quantity.
*!***************************************************************
*!
FUNCTION lfEvalQty
PRIVATE lnJ
lnJ = 0

FOR lnJ = 1 TO 8
  lcI = STR(lnJ,1)
  laOrdReq[lnJ] = IIF(lcRpScpMod$"KP",Cut&lcI,Qty&lcI)
ENDFOR
*-- end of lfEvalQty.

*!***************************************************************
*! Name      : lfDelKey                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/13/1999
*! Purpose   : Remove master key from memo field.
*!***************************************************************
*!
FUNCTION lfDelKey
PRIVATE lcNewKey , lcDelKey
SELECT (lcTmpOrdLn)
lcDelKey = EVALUATE(lcIndexExp)
SELECT (lcTmpStore)
lcNewKey = STRTRAN(mAddedKey,lcDelKey,"")  && Remove key
lcNewKey = STRTRAN(lcNewKey,"||","|")      && Remove double pipes
IF ALLTRIM(lcNewKey) == "|"
  lcNewKey = ''                            && Empty Key.
ENDIF
REPLACE mAddedKey WITH lcNewKey            && Update action.
*-- end of lfDelKey.


*!***************************************************************
*! Name      : lfPIKTYPE                              E#301290,1
*! Developer : WAM
*! Date      : 07/21/1999
*! Purpose   : 
*!***************************************************************
*! Program   : 'MFADORD'  -> 
*! Event     : 'PICKTYPE' -> 
*!***************************************************************
*! Returns   : None.
*!***************************************************************
FUNCTION lfPIKTYPE

RETURN (PIKTKT.cPickType='A')


*!***************************************************************
*! Name      : lfStyAlCnd                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/22/1999
*! Purpose   : Change standard Condition.
*!***************************************************************
*! Program   : 'ALSTYAL'    -> Allocation by Style screen.
*! Event     : 'ADJBROW'    -> Adjust Browse Conditions.
*!***************************************************************
*!
FUNCTION lfStyAlCnd
lcBForExpr = lcBForExpr + [ AND ] + ;
             [(EMPTY(OrdLine.PikTkt) OR (OrdLine.PikTkt=="******"))]
*-- end of lfStyAlCnd.

*!***************************************************************
*! Name      : lfStyPikSt                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/22/1999
*! Purpose   : Disable Pick Button.
*!***************************************************************
*! Program   : 'ALSTYAL'    -> Allocation by Style screen.
*! Event     : 'STYPKSTAT ' -> Disable Pick Button.
*!***************************************************************
*!
FUNCTION lfStyPikSt
lcPikStat = "DISABLE"
llPickOpt = .F.
*-- end of lfStyPikSt.

*!***************************************************************
*! Name      : lfHdrNtQty                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/22/1999
*! Purpose   : Update ordhdr net allocated quantity .
*!***************************************************************
*! Program   : 'ALSTYAL'    -> Allocation by Style screen.
*! Event     : 'UPDTORDHDR' -> Adjust Browse Conditions.
*!***************************************************************
*!
FUNCTION lfHdrNtQty
PRIVATE lnUpdtWith , lnCurrAls
lnUpdtWith = IIF(llStatus,laPikd[9],0) - OrdLine.TotPik
lnCurrAls = SELECT(0)
SELECT OrdHdr
=SEEK("O"+OrdLine.Order)

= RLOCK()
REPLACE nNetAloQty WITH nNetAloQty + lnUpdtWith
=gfAdd_Info("ORDHDR")
UNLOCK

SELECT (lnCurrAls)
*-- end of lfHdrNtQty.

*!***************************************************************
*! Name      : lfSavSorts                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/22/1999
*! Purpose   : Save Sort levels.
*!***************************************************************
*! Program   : 'ALAUTAL'    -> Allocation by Style screen.
*! Event     : 'SAVESORT'   -> Adjust Browse Conditions.
*!***************************************************************
*!
FUNCTION lfSavSorts
SAVE ALL LIKE laSortAry TO (gcDataDir+"SORTLEVL.MEM")
*-- end of lfSavSorts.

*************** Sales order allocation Functions ****************
*************** MAB Notes : [Begin
*** In all of the next work I try to do the same functionality 
*** of automatic allocation.
*************** MAB Notes : [End..

*!***************************************************************
*! Name      : lfNoPkOptn                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/22/1999
*! Purpose   : Supress generate pick ticket option from Order allocation screen.
*!***************************************************************
*! Program   : 'ALORDAL'    -> Allocation by Sales order screen.
*! Event     : 'NOPICKNOW'  -> Do not make generate pick ticket option.
*!***************************************************************
*!
FUNCTION lfNoPkOptn
llGenBar = .F.
lcFiles  = lcFiles +;
          [ + 'lcTmpStore,' + lcTmpStore + ',' + ORDER(lcTmpStore) + ';']
*-- end of lfNoPkOptn.

*!***************************************************************
*! Name      : lfCrtStore                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/22/1999
*! Purpose   : Create Temporary Store File used in distributing.
*!***************************************************************
*! Program   : 'ALORDAL'    -> Allocation by Sales order screen.
*! Event     : 'CRTSTORE'   -> Create Store File.
*!***************************************************************
*!
FUNCTION lfCrtStore
IF !FILE(gcWorkdir + lcTmpStore + '.DBF')
  =lfSpcilSet("INI")
  =lfSpcilSet("CRTTMP")
ENDIF  
*-- end of lfCrtStore.

*!***************************************************************
*! Name      : lfClsStore                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/22/1999
*! Purpose   : Close Temp. Store Function
*!***************************************************************
*! Program   : 'ALORDAL'    -> Allocation by Sales order screen.
*! Event     : 'CLSSTORE'   -> Close Store File.
*!***************************************************************
*!
FUNCTION lfClsStore
=lfSpcilSet("QUITPRG")
*-- end of lfCrtStore.

*!***************************************************************
*! Name      : lfAdToStor                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/22/1999
*! Purpose   : Fill Store file with approparate values.
*!***************************************************************
*! Program   : 'ALORDAL'    -> Allocation by Sales order screen.
*! Event     : 'FILLSTORE'  -> Fill Store File.
*!***************************************************************
*!
FUNCTION lfAdToStor
PRIVATE lnCurAlias , lcLineOrd , lcLineRec
lnCurAlias = SELECT (0)
IF USED(lcTmpStore) AND RECCOUNT(lcTmpStore) > 0
  SELECT (lcTmpStore)
  DELETE ALL
ENDIF
IF OrdHdr.Multi = "Y"
  SELECT ORDLINE
  lcLineOrd = ORDER()
  lcLineRec = EVALUATE(KEY())
  SET ORDER TO ORDLINE

  llExclude  = .F.
  lcRpScpMod = ""
  DIMENSION  laOrdReq[8]
  
  SCAN FOR cOrdType + Order + STR(lineNo,6) = "O" + OrdHdr.Order
    STORE 0 TO lnI, laOrdReq
    =lfFilStore()
  ENDSCAN  
  SET ORDER TO &lcLineOrd
  =SEEK(lcLineRec)
ENDIF  
SELECT (lnCurAlias)
*-- end of lfAdToStor.

*!***************************************************************
*! Name      : lfStkRatio                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/25/1999
*! Purpose   : Evaluate Allocation Ratio in Multi store Order.
*!***************************************************************
*! Program   : 'ALORDAL'    -> Allocation by Sales order screen.
*! Event     : 'BYORDPRCNT' -> Fill Store File.
*!***************************************************************
*!
FUNCTION lfStkRatio
IF SEEK(m.Order + m.Style , lcTmpStore)

  PRIVATE laPrevQty , lnCurAls , lcTmpOrdLn
  DIMENSION laPrevQty[8]
  laPrevQty = 0

  lnCurAls = SELECT(0)
  SELECT (lc_TmpOrdL)
  lcTmpOrdLn = EVALUATE(KEY())
  SUM Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO ARRAY laPrevQty ;
    FOR (cOrdType+Order+STR(lineno,6) = "O" + m.Order) AND ;
        (Style = m.Style) AND (TotPik > 0)
  
  =SEEK(lcTmpOrdLn)    
  SELECT (lnCurAls)
  
  laAllocPer[1] = IIF(m.Qty1=0,0,;
                  m.Qty1/(&lcTmpStore..nTotStyRq1-laPrevQty[1]))
  laAllocPer[2] = IIF(m.Qty2=0,0,;
                  m.Qty2/(&lcTmpStore..nTotStyRq2-laPrevQty[2]))
  laAllocPer[3] = IIF(m.Qty3=0,0,;
                  m.Qty3/(&lcTmpStore..nTotStyRq3-laPrevQty[3]))
  laAllocPer[4] = IIF(m.Qty4=0,0,;
                  m.Qty4/(&lcTmpStore..nTotStyRq4-laPrevQty[4]))
  laAllocPer[5] = IIF(m.Qty5=0,0,;
                  m.Qty5/(&lcTmpStore..nTotStyRq5-laPrevQty[5]))
  laAllocPer[6] = IIF(m.Qty6=0,0,;
                  m.Qty6/(&lcTmpStore..nTotStyRq6-laPrevQty[6]))
  laAllocPer[7] = IIF(m.Qty7=0,0,;
                  m.Qty7/(&lcTmpStore..nTotStyRq7-laPrevQty[7]))
  laAllocPer[8] = IIF(m.Qty8=0,0,;
                  m.Qty8/(&lcTmpStore..nTotStyRq8-laPrevQty[8]))
ENDIF
*-- end of lfStkRatio.

*!***************************************************************
*! Name      : lfByOrdHdr                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/25/1999
*! Purpose   : Update Order Header File .
*!***************************************************************
*! Program   : 'ALORDAL'  -> Allocation by Sales order screen.
*! Event     : 'UPDTHDR'  -> Fill Store File.
*!***************************************************************
*!
FUNCTION lfByOrdHdr
*B603318,1
SELECT ORDLINE
lnOrLRec = IIF(!EOF(),RECNO(),0)
LOCATE FOR &lcNdxExp = laData[9] + laData[1] &lcBrowExp AND !EMPTY(Piktkt) AND Piktkt#"*"
llOrdPktk = FOUND()
IF lnOrLRec # 0
  GOTO lnOrLRec
ENDIF  
SELECT ORDHDR
IF llOrdPktk    &&piktkt found.
  *--This order has picked quantity assigned to pick ticket(s), use automatic allocation to allocate the rest.
  =gfModalGen('TRM44091B00000', 'DIALOG')
  =gfCPClose()
ENDIF  
*-- end of lfByOrdHdr.


*!***************************************************************
*! Name      : lfChkBkOrd                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/25/1999
*! Purpose   : Check back Order flag in order Header File .
*!***************************************************************
*! Program   : 'ARIINV'      -> Sales Order Invoice.
*! Event     : 'CHKBKORDER'  -> Check allow back order flag.
*!***************************************************************
*!
FUNCTION lfChkBkOrd
lcChkBkOrd = IIF(OrdHdr.lAlwBkOrd,"I","N")
*-- end of lfChkBkOrd.

*!***************************************************************
*! Name      : lfDueDatIs                             C200084,1
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/25/1999
*! Purpose   : Calculate New Due date if it is earlier Date .
*!***************************************************************
*! Program   : 'ARIINV'      -> Sales Order Invoice.
*! Event     : 'WHATISDUED'  -> Check allow back order flag.
*!***************************************************************
*!
FUNCTION lfDueDatIs
IF ORDHDR.lAllShpExt AND (ldDefInvDate < OrdHdr.Start)
  lnTDaysDue = lnTDaysDue + OrdHdr.Start - ldDefInvDate
ENDIF
*-- end of lfDueDatIs.
*!***************************************************************
*! Name      : lfSchedul
*! Developer : WAM (Walid Abou El-Magd)
*! Date      : 10/12/1999
*! Purpose   : SEEK in ICADSCH for spacefic design
*!***************************************************************
*! Program   : 'MFADORD'     -> Adornment Order Planning Tool.
*! Event     : 'SCHEDUL'     -> 
*!***************************************************************
*!
FUNCTION lfSchedul
IF !USED('ICADSCH')
  =gfOpenFile(gcDataDir+'ICADSCH','DESNDATE','SH')
ENDIF
lcSchedul=IIF(SEEK(CDSGNCODE,'ICADSCH'),'Y','N')
RETURN lcSchedul


*!***************************************************************
*! Name      : LFADCOLCT
*! Developer : TAK
*! Date      : 10/19/1999
*! Purpose   : Colect adornment order information.
*!***************************************************************
*! Program   : 'MFADORD'     -> Adornment Order Planning Tool.
*! Event     : 'FPCOLECT'     -> 
*!***************************************************************
FUNCTION LFADCOLCT
PRIVATE llFirst
llFirst=.T.

ldFromDate = ldStrtDate
ldToDate   = ldLOSSDate

SELECT (lcTmpGenPk)
IF RECCOUNT() > 0
  ZAP
ENDIF

=lfBtnDatOk()

GO TOP IN (lcTmpGenPk)
IF EOF(lcTmpGenPk)
  llAcepSel = .F.
  RETURN .F.
ENDIF

SELECT (lcTmpGenPk)
SET RELATION TO
SET RELATION TO 'O'+ Order   INTO Ordhdr   ADDITIVE
SET RELATION TO 'M'+ Account INTO Customer ADDITIVE
SET RELATION TO 'SO'+ Order+STR(Lineno,6) INTO Bomvar ADDITIVE

=gfOpenFile(gcDataDir+'ICADSCH','DESNDATE','SH')
  
SELECT (lcTmpBom)
SET ORDER TO (lcTmpBom)

SELECT (lcTmpGenPk)
SCAN
  *-- Save the smallest start date order number , we will use it to 
  *-- get information in saving
  IF llFirst
    ldStartDate = Ordhdr.Start
    lcHdrOrder  = Ordhdr.Order 
    llFirst     = .F.
  ENDIF
  IF Ordhdr.Start < ldStartDate
    ldStartDate = Ordhdr.Start
    lcHdrOrder  = Ordhdr.Order 
  ENDIF
 
  SCATTER MEMVAR MEMO
  m.STNAME = CUSTOMER.STNAME
  m.llSel  = .F.
  *--Update tmp order.
  INSERT INTO (lcTmpOrder) FROM MEMVAR 


  SELECT BOMVAR
  =SEEK( 'SO'+&lcTmpGenPk..Order+STR(&lcTmpGenPk..LineNo,6) )
  SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)=;
                  'SO'+&lcTmpGenPk..ORDER+STR(&lcTmpGenPk..lineno,6)
    SCATTER MEMVAR MEMO
    m.llSel      = .F.
    m.TOTPIK     = &lcTmpGenPk..TotPik
    m.Gros_price = &lcTmpGenPk..Gros_price  
    INSERT INTO (lcTmpBom) FROM MEMVAR

    =SEEK(BOMVAR.cDsgnCode,'ICDESIGN') 
        
    SELECT (lcTmpDsgn)
    IF SEEK(BOMVAR.cDsgnCode,(lcTmpDsgn))
      *-- Accomulate
      REPLACE TOTQTY WITH TOTQTY + &lcTmpGenPk..TOTPIK ,;
              TOTVAL WITH TOTVAL + (&lcTmpGenPk..TOTPIK * &lcTmpGenPk..Gros_price)
      IF Ordhdr.Start < DSTRT
        REPLACE DSTRT WITH Ordhdr.Start
      ENDIF
      IF Ordhdr.Complete < DEND
        REPLACE DEND WITH Ordhdr.Complete
      ENDIF
      *--Selected by default.
      IF TotQty > ICDESIGN.Nvenadmin
        REPLACE llSEL WITH .T.
        lnSelRec = lnSelRec + 1
      ENDIF

    ELSE
      *-- Add
      DIME laTemp[10]
      laTemp[1]  = BOMVAR.CDSGNCODE
      laTemp[2]  = ICDESIGN.CDSGNNAME
      laTemp[3]  = &lcTmpGenPk..TOTPIK
      laTemp[4]  = (&lcTmpGenPk..TOTPIK * &lcTmpGenPk..Gros_price )
      laTemp[5]  = Ordhdr.Start
      laTemp[6]  = Ordhdr.Complete
      laTemp[7]  = IIF(laTemp[3] > ICDESIGN.Nvenadmin,.T.,.F.)
      laTemp[8]  = laTemp[7]
      laTemp[9]  = ICDESIGN.Cdsgnlink
      laTemp[10] = IIF(SEEK(BOMVAR.CDSGNCODE,'ICADSCH'),'Y','N')

      SELECT (lcTmpDsgn)
      INSERT INTO (lcTmpDsgn) FROM ARRAY laTemp
      laTemp=' '
    ENDIF

    SELECT BOMVAR
  ENDSCAN  &&-- ENDSCAN of bomvar
  
  SELECT (lcTmpGenPk)
ENDSCAN &&-- ENDSCAN of TmpGenPk

SELECT (lcTmpDsgn)
GO TOP
IF EOF() OR BOF()
  llAcepSel = .F.
  RETURN(.F.)
ELSE  
  SCAN FOR llSEL  &&-- scan lcTmpDsgn for selected by default only .
    =lfDsgnToBom(.T.)
  ENDSCAN
  GO TOP
ENDIF
SELECT (lcTmpGenPk)
SET RELATION TO
llAcepSel = .T.
RETURN(.T.)




*!*************************************************************
*! Name      : lfBtnDatOk
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Start date <Ok> Button Pressed Valid Function
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfBtnDatOk


*-- Start Business Rules ...........................
*----------------------- ...........................
*-- if user change date bounds, then apply business Rules.
*-- lnAlocPcs  : No. of Allocated Pieces.
*-- lnNetOpen  : No. of actually open pieces.
PRIVATE lnAlocPcs , lnNetOpen
STORE 0 TO lnAlocPcs , lnNetOpen

*--Save relations.
STORE '' TO lcOrdLnRel,lcStyRela,lcCPIKTag
=lfSetRelat()  && Set Relation between files.

SELECT ORDHDR
SEEK "O"
SCAN REST WHILE cOrdType + Order = "O" ;
            FOR BETWEEN(Start,ldFromDate,ldToDate) AND Status = 'O'

  IF SEEK("O"+Order, "OrdLine")
    *--Read open and alocated qty.
    STORE 0 TO lnAlocPcs , lnNetOpen
    =lfNetOpnAl(@lnAlocPcs , @lnNetOpen)

    WAIT WINDOW "Processing Order # " + Order NOWAIT
    =lfBusiness()
  ENDIF  
ENDSCAN
WAIT CLEAR

=lfReSetRel()  && ReSet Relation between files.
SELECT (lcTmpGenPk)
SET ORDER TO (lcTmpGenPk)
GO TOP
*-- end of lfBtnDatOk.


*!*************************************************************
*! Name      : lfSetRelat
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Set Relation between files.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfSetRelat

SELECT CUTPICK
lcCPIKTag = SET('ORDER')
SET ORDER TO TAG Cutord

SELECT STYLE
lcStyRela = SET('RELATION')
SET RELATION TO "S" + scale INTO Scale

SELECT ORDLINE
lcOrdLnRel = SET('RELATION')
SET RELATION TO Style INTO Style ,;
                "SO" + Order + STR(LineNo,6) INTO Bomvar
*-- end of lfSetRelat.

*!*************************************************************
*! Name      : lfReSetRel
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : ReSet Relation between files.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfReSetRel

SELECT CUTPICK
SET ORDER TO &lcCPIKTag

SELECT STYLE
SET RELATION TO &lcStyRela

SELECT ORDLINE
SET RELATION TO &lcOrdLnRel
*-- end of lfReSetRel.

*!*************************************************************
*! Name      : lfNetOpnAl
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Compute Open for allocation Quantity.
*!*************************************************************
*! Return    : Net Open Quantity.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfNetOpnAl
PARAMETERS lnNetAlo , lnNetQty
PRIVATE lnNetAlo , lnNetQty , lcFltExp
STORE 0 TO lnNetAlo , lnNetQty 

lcFltExp = IIF(lcPassType="B",'','!') + [EOF("BOMVAR")]
SELECT ORDLINE
SCAN REST WHILE cOrdType+Order+STR(LineNo,6)="O"+ORDHDR.Order ;
            FOR &lcFltExp 
  IF !EMPTY(PikTkt)
    lnNetAlo = lnNetAlo + TotPik
  ENDIF
  lnNetQty = lnNetQty + TotQty
ENDSCAN
SELECT ORDHDR
*-- end of lfNetOpnAl.


**********************************************************************
************************ Business Rules Section **********************
**********************************************************************

*!*************************************************************
*! Name      : lfBusiness
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Apply Business Rules Function.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfBusiness

************************* FIRST ORDER RULES ******************************
*-- Rule # 1 (ThreShold OK Percentage)
*-------------------------------------

llPassRule = .F.
IF lfThrShold("O")

  *-- Line Item Ratio Rules...
  *-- Rule # 2 (Company line item ratio)
  *-- Rule # 3 (It is not back order and Order header line item 
  *--           ratio was set)
  IF lfCmpRatio() AND !lfBackOrd() AND lfOrdRatio()
    ***************************** LINE RULES *********************
    =lfApplyLns()
  ELSE
    llPassRule = .T.
  ENDIF

  ************************* SECOND ORDER RULES *****************
  *-- Rule # 5 (Threshold again to the new quantity)
  =SEEK(Order , lcTmpGenPk)
  IF lfThrShold("O")
    IF llPassRule
      =lfAddLines()
    ENDIF
  ELSE  && Does not meet threshold ok again
    IF !(llPassRule OR EOF(lcTmpGenPk))
      SELECT (lcTmpGenPk)
      =SEEK(OrdHdr.Order)
      DELETE REST WHILE Order = OrdHdr.Order
    ENDIF  
  ENDIF

ENDIF  
*-- end of lfBusiness.


*!*************************************************************
*! Name      : lfAddLines
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Add order lines.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfAddLines

= SEEK("O"+Order,"ORDLINE")
SELECT ORDLINE
SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = "O"+OrdHdr.Order ;
     FOR !EMPTY(PikTkt) AND PikTkt = '*'

  SCATTER MEMVAR MEMO
  m.cPickType = IIF(EOF("BOMVAR"),"B","A")
  IF m.cPickType <> lcPassType
    LOOP
  ENDIF
  IF lcPassType = 'A' AND SEEK('2'+ORDLINE.Order+STR(ORDLINE.Lineno,6),'CUTPICK')
    LOOP
  ENDIF


  m.lSelRec = IIF(lnAlocPcs = lnNetOpen,.T.,.F.)
    
  INSERT INTO (lcTmpGenPk) FROM MEMVAR

ENDSCAN
*-- end of lfAddLines.

*!*************************************************************
*! Name      : lfApplyLns
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Rules on lines
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfApplyLns

= SEEK("O"+Order,"ORDLINE")
SELECT ORDLINE
SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = "O"+OrdHdr.Order ;
     FOR !EMPTY(PikTkt) AND PikTkt = '*'

  SCATTER MEMVAR MEMO
  m.cPickType = IIF(EOF("BOMVAR"),"B","A")
  IF m.cPickType <> lcPassType
    LOOP
  ENDIF
  IF lcPassType = 'A' AND SEEK('2'+ORDLINE.Order+STR(ORDLINE.Lineno,6),'CUTPICK')
    LOOP
  ENDIF


  m.lSelRec = IIF(lnAlocPcs = lnNetOpen,.T.,.F.)

  INSERT INTO (lcTmpGenPk) FROM MEMVAR
    
  *-- Rule # 4,1 (Style line item ratio)
  *-- Rule # 4,2 (All Sizes was filled (ordered))
  *-- Rule # 4,3 (AT least one size has 40%)

  IF lfStyRatio() AND lfAllFill() AND lfOneHas40()

    *-- Rule # 4,4 (Total 40s% / Size count >= 60 %)
    *-- Rule # 4,5 (Consecutive 3 Sizes)
    IF !(lf60SzCnt() AND lfConSecTv())
      SELECT (lcTmpGenPk)
      *C200111,1 Start.
      lnAlocPcs = lnAlocPcs - TotPik
      REPLACE Flag WITH 'R'
      DELETE
      *C200111,1 End.
    ENDIF
  ENDIF

ENDSCAN    
*-- end of lfApplyLns.



*!*************************************************************
*! Name      : lfThrShold
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Compute Order ThreShold OK percentage
*!*************************************************************
*! Parameters: Min. Allow level (Company or Order)
*! Return    : .T. or .F.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfThrShold
PARAMETERS lcChkLevel
PRIVATE llRetVal , lnChKQty
lnChKQty = IIF(lcChkLevel="C",lnCmpMnAlo,OrdHdr.nMinAloPer)
llRetVal = lnNetOpen <> 0 AND ((lnAlocPcs/lnNetOpen)*100 >= lnChKQty)
RETURN llRetVal
*-- end of lfThrShold.

*!*************************************************************
*! Name      : lfCmpRatio
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Evaluate Company Line item ratio.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfCmpRatio

RETURN llCmpRatio
*-- end of lfCmpRatio.

*!*************************************************************
*! Name      : lfBackOrd
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Detect if it was back order .
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfBackOrd
PRIVATE llRetVal
llRetVal = ((Status = "O") AND (Ship > 0))
RETURN llRetVal
*-- end of lfBackOrd.

*!*************************************************************
*! Name      : lfOrdRatio
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check order header line item ratio flag.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfOrdRatio

RETURN lItemRatio
*-- end of lfOrdRatio.

*!*************************************************************
*! Name      : lfStyRatio
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check Style line item ratio.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfStyRatio

RETURN Style.lItemRatio
*-- end of lfStyRatio.

*!*************************************************************
*! Name      : lfAllFill
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check if all sizes have allocated quantity.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfAllFill
PRIVATE lcPickType
lcPickType = IIF(EOF("BOMVAR"),"B","A")

=SEEK(Order + Store + lcPickType + STR(LineNo,6) , lcTmpGenPk)
PRIVATE llRetVal , lnI , lcI
llRetVal=.T.
FOR lnI = 1 TO Scale.Cnt
  lcI = STR(lnI,1)
  *C200111,1 Start.
  *llRetVal = (Pik&lcI > 0)
  llRetVal = (Qty&lcI > 0)
  *C200111,1 End.
  IF llRetVal
    =lfUpdatTmp()
  ELSE
    EXIT
  ENDIF
ENDFOR
RETURN llRetVal
*-- end of lfAllFill.

*!*************************************************************
*! Name      : lfUpdatTmp
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Update Temporary file new fields.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfUpdatTmp
PRIVATE llOneHas40,lnTot40,lcConSecTv
llOneHas40 = &lcTmpGenPk..lOneHas40
lnTot40    = &lcTmpGenPk..nTot40
lcConSecTv = &lcTmpGenPk..mConSecTv3

IF Pik&lcI/Qty&lcI >= 0.40
  llOneHas40 = .T.
  lnTot40    = lnTot40 + 1
  lcConSecTv = lcConSecTv + "|"
ELSE
  lcConSecTv = lcConSecTv + " "
ENDIF  
SELECT (lcTmpGenPk)
REPLACE lOneHas40  WITH llOneHas40 ,;
        nTot40     WITH lnTot40    ,;
        mConSecTv3 WITH lcConSecTv
SELECT Ordline
*-- end of lfUpdatTmp.

*!*************************************************************
*! Name      : lfOneHas40
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check if at least one size has 40%.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfOneHas40

RETURN &lcTmpGenPk..lOneHas40
*-- end of lfOneHas40.

*!*************************************************************
*! Name      : lf60SzCnt
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Number of Sizes have 40% greater than or equal 60% 
*!           : relative to all sizes.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lf60SzCnt
PRIVATE llRetVal
llRetVal = (&lcTmpGenPk..nTot40/Scale.Cnt) >= 0.60
RETURN llRetVal
*-- end of lf60SzCnt.

*!*************************************************************
*! Name      : lfConSecTv
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check if have 40% sizes in 3 consecuative order.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!Called from 'LFADCOLCT' function 
*!*************************************************************
FUNCTION lfConSecTv
PRIVATE llRetVal
IF Scale.Cnt <= 5
  *llRetVal = ("|||" $ &lcTmpGenPk..mConSecTv3)
  DO CASE
    CASE Scale.Cnt=5 OR Scale.Cnt=4
      llRetVal = ("|||" $ &lcTmpGenPk..mConSecTv3)
    CASE Scale.Cnt=3 OR Scale.Cnt=2
      llRetVal = ("||" $ &lcTmpGenPk..mConSecTv3)
    CASE Scale.Cnt=1
      llRetVal = ("|" $ &lcTmpGenPk..mConSecTv3)
  ENDCASE  
ELSE
  llRetVal = .T.
ENDIF
RETURN llRetVal
*-- end of lfConSecTv.



******************
* End collecting
******************





**********************************************************************
******************** Generate pick tickets Section *******************
**********************************************************************

*!***************************************************************
*! Name      : lfGENPIK
*! Developer : TAK
*! Date      : 10/19/1999
*! Purpose   : Generate adornment piktkt.
*!***************************************************************
*! Program   : 'MFADORD'     -> Adornment Order Planning Tool.
*! Event     : 'FPGENPIK'    -> 
*!***************************************************************
FUNCTION lfGENPIK

GO TOP IN (lcTmpGenPk)
IF EOF(lcTmpGenPk)
  WAIT WINDOW 'No Pick Tickets was generated!'
  RETURN
ENDIF

*--Update selected from screen into lcTmpGenPk file.
SELECT (lcTmpGenPk)
LCTMPTAG = ORDER()
SET ORDER TO TAG (lcTmpGenPk)
REPLACE ALL lSelRec WITH .F.
SELECT (lcTmpOrder)
SCAN FOR llSEL
  IF SEEK(Order+Store+'A'+STR(LineNo,6),lcTmpGenPk)
    SELECT (lcTmpGenPk)
    REPLACE lSelRec WITH IIF(LEXIST,.F.,.T.)
    SELECT (lcTmpOrder)
    REPLACE LExist  WITH &lcTmpGenPk..LExist
  ENDIF
ENDSCAN
SELECT (lcTmpGenPk)
SET ORDER TO &LCTMPTAG


 
*-- lcMyPickNo : Current Pick ticket No.  
PRIVATE lcOldTag,lcPkTkKey,llOpnPkTk,lnBlanks,;
        lnNmDpOnly,lnDsgnOnly,lnDsgnDrop,llNamDrop,llDesign,lcThrmMsg
STORE "" TO lcOldTag , lcPkTkKey, lcMyPickNo
    
*-- Open Pick ticket File if not opened. [Begin]
IF USED(PIKTKT)
  llOpnPkTk = .F.
  lcOldTag = ORDER("PIKTKT")
  SET ORDER TO OrdPik IN PIKTKT
  lcPkTkKey = PIKTKT.Order + PIKTKT.PikTkt
ELSE
  llOpnPkTk = gfOpenFile(gcDataDir+"PIKTKT","ORDPIK",'SH')
ENDIF
*-- Open Pick ticket File if not opened. [End  ]


USE (gcWorkDir+lcTmpGenPk) IN 0 AGAIN ALIAS GenPikTkt ORDER TAG lcTmpGenPk
SELECT GenPikTkt
SET RELATION TO "SO" + Order + STR(LineNo,6) INTO Bomvar
SET ORDER TO TAG lcTmpGenPk
GO TOP

SELECT (lcTmpGenPk)
SET ORDER TO TAG (lcTmpGenPk)
lcPrvOrder = '*Ordr*'
GO TOP
SCAN

  IF lcPrvOrder = Order
    LOOP
  ELSE
    lcPrvOrder = Order
  ENDIF      

  = SEEK("O"+Order , "OrdHdr")
  STORE 0 TO lnBlanks,lnNmDpOnly,lnDsgnOnly,lnDsgnDrop
  llOrdLFnd = .F. 
    
  SELECT GenPikTkt
  GO TOP
  SCAN FOR Order = &lcTmpGenPk..Order AND lSelRec
    llOrdLFnd = .T.       
    IF SEEK(Order+Store+cPickType,lcTmpPkTk)
      lcMyPickNo = &lcTmpPkTk..PikTkt
    ELSE
      *B603603,1 Start.
      *lcMyPickNo = lfGenPkNum(Order,Store,PADR(laware[lnTware],6),cPickType,OrdHdr.cDivision)
      lcMyPickNo = lfGenPkNum(Order,Store,OrdHdr.cWareCode,cPickType,OrdHdr.cDivision)
      *B603603,1 End.
    ENDIF  

    *--Update releated files.
    =lfGenPikTk(lcMyPickNo)

    IF SEEK('O'+GenPikTkt.Order+STR(GenPikTkt.Lineno,6),'ORDLINE')
      SELECT ORDLINE
      = RLOCK()
      REPLACE PikTkt  WITH lcMyPickNo,;
              Picked  WITH .T.,;
              PikDate WITH gdSysDate
      UNLOCK
    ENDIF    && End of IF

    *Update tmp Order in MFADORD.
    IF SEEK('O'+GenPikTkt.Order+STR(GenPikTkt.Lineno,6),lcTmpOrder)
      SELECT (lcTmpOrder)
      REPLACE PikTkt WITH lcMyPickNo
    ENDIF

    SELECT GenPikTkt
    *-- Caling Thermometer. [Begin]
    lcThrmMsg = "Generating Adornment Picking Ticket # "+lcMyPickNo+" , Order # " +Order+IIF(!EMPTY(STORE)," , Store : "+Store,"")

    WAIT WINDOW lcThrmMsg NOWAIT
    *-- Caling Thermometer. [End  ]
      
    IF lfAlowBack()
      SELECT GenPikTkt
      IF cPickType = "B"
        lnBlanks = lnBlanks + (TotQty - TotPik)
      ELSE
        SELECT BOMVAR
        STORE .F. TO llNamDrop,llDesign
        SCAN REST FOR cidtype+ccost_id+STR(lineno,6) =;
                      "SO" + GenPikTkt.Order + STR(GenPikTkt.LineNo,6)
          llNamDrop = llNamDrop OR !EMPTY(cNDrpID)
          llDesign  = llDesign  OR !EMPTY(cDsgnCode)
        ENDSCAN

        DO CASE
          *-- Name Drop Only.
          CASE llNamDrop AND !llDesign
            lnNmDpOnly = lnNmDpOnly + (GenPikTkt.TotQty - GenPikTkt.TotPik)
            
          *-- Design Only.
          CASE llDesign AND !llNamDrop
            lnDsgnOnly = lnDsgnOnly + (GenPikTkt.TotQty - GenPikTkt.TotPik)
            
          *-- Design and Name Drop.
          CASE llDesign AND llNamDrop
            lnDsgnDrop = lnDsgnDrop + (GenPikTkt.TotQty - GenPikTkt.TotPik)
            
        ENDCASE

      ENDIF
    ENDIF  
      
  ENDSCAN

  *-- Last Step (if Allow Back Order flag)
  *-- and Non of numeric values achive the minimum value then 
  *-- Reset Allow Back Order flag.
  IF llOrdLFnd AND lfAlowBack() AND ;
                        (lnBlanks   < lnBlankBk ) AND ;
                        (lnDsgnOnly < lnDesignBk) AND ;
                        (lnNmDpOnly < lnNmDropBk) AND ;
                        (lnDsgnDrop < lnDesDrpBk)
    SELECT ORDHDR
    REPLACE lAlwBkOrd WITH .F.
  ENDIF

  SELECT (lcTmpGenPk)
ENDSCAN
WAIT CLEAR

*C200111,1 Start, Added.
*-First release alocation for not valid item ration lines.
*=lfRelLIRAL()
*C200111,1 End.

USE IN GenPikTkt

IF !llOpnPkTk
  SELECT PIKTKT
  =SEEK(lcPkTkKey)
  SET ORDER TO &lcOldTag
ENDIF
  
*-- if no records in temporary file.
IF USED(lcTmpGenPk)
  SELECT (lcTmpGenPk)
  ZAP
ENDIF

*B603111,1 Clear Piktkt temp. file for new session [Begin]
IF USED(lcTmpPkTk)
  SELECT (lcTmpPkTk)
  ZAP
ENDIF

RETURN


*!*************************************************************
*! Name      : lfGenPkNum
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Generate new pick ticket number or add to old one.
*!*************************************************************
*! Return    : Pick Ticket Number.
*!*************************************************************
*!
FUNCTION lfGenPkNum
*-- lcMyOrder  : Order     #.
*-- lcMyStore  : Store     #.
*-- lcMyLoc    : Location  #.
*-- lcMyPikTyp : Pick Type #.
*-- lcMyDiv    : Division  #.
PARAMETERS lcMyOrder , lcMyStore , lcMyLoc , lcMyPikTyp , lcMyDiv

PRIVATE lnCurAlias,lcPikTkt,laPikNo,llNewPick,lcWhereExpr
        
lnCurAlias = SELECT(0)

*-- Define array hold the piktkts no. , status , printed or not.
DECLARE laPikNo[1]

*-- Initialize the needed variables.
STORE ""  TO laPikNo , lcPikTkt , lcOldTag , lcPkTkKey , lcLocExpr
llNewPick = .F. && Create new pick ticket

*-- Select pick file.
SELECT PIKTKT
SET ORDER TO OrdPik  && Restore Tag setting. 

*-- if you find pick tickets for this order.
IF SEEK(lcMyOrder) 
  lcWhereExpr = [Order=lcMyOrder AND !(Status $ "CX") AND (Store = lcMyStore) AND (cWareCode = lcMyLoc) AND (cPickType = lcMyPikTyp)]
  SELECT DISTINCT PikTkt FROM PikTkt   ;
    WHERE &lcWhereExpr ORDER BY PikTkt ;
    INTO ARRAY laPikNo

  IF EMPTY(laPikNo[1]) OR laType[lnType,2]='G'
    llNewPick = .T.
  ELSE

    IF ALEN(laPikNo,1) = 1
    
      *** There is Picking ticket no. : {laPikNo[1]}.     ***
      *** Do you wish to assign a new picking ticket no.? ***
      *** < Add > - < New > ***
      IF gfModalGen("TRM44011B44000","DIALOG" , laPikNo[1]) = 1
        =SEEK(lcMyOrder + laPikNo[1])
      ELSE  
        llNewPick = .T.
      ENDIF
    
    ELSE  && More than one Pick Ticket.
      *-- Loop Picktkt array to add required values.
      *FOR lnI = 1 TO ALEN(laPikNo,1)
      *  =SEEK(lcMyOrder + laPikNo[lnI])
      *  lcAddToArr = SPACE(1) + IIF(PrtFlag="P","Yes","No ") +;
      *               SPACE(5) + IIF(status $ "O ","Open   ",;
      *                          IIF(Status="H","On Hold",SPACE(7))) +;
      *               SPACE(1) + DTOC(Date)
      *  laPikNo[lnI] = laPikNo[lnI] + lcAddToArr
      *ENDFOR
      *lsPikLst = 1
      *DO ALPIKLST.SPR

      *** There is Picking ticket no. : {laPikNo[X]}.     ***
      *** Do you wish to assign a new picking ticket no.? ***
      *** < Add > - < New > ***
      IF gfModalGen("TRM44011B44000","DIALOG" , laPikNo[ALEN(laPikNo,1)]) = 1
        =SEEK(lcMyOrder + laPikNo[ALEN(laPikNo,1)])
      ELSE  
        llNewPick = .T.
      ENDIF
   
    ENDIF
  ENDIF

ELSE
  llNewPick = .T.
ENDIF

*-- If create new piktkt.
IF llNewPick
  lcPikTkT = gfSequence('PIKTKT', '', '', lcMyDiv)

ELSE  && Add to Old PikTkt
  lcPikTkT = PikTkt
ENDIF
SELECT (lnCurAlias)
RETURN lcPikTkT
*-- end of lfGenPkNum.


*!*************************************************************
*! Name      : lfGenPikTk
*! Developer : (MAB) Mohamed Atia Badran
*! Date      : 07/21/1999
*! Purpose   : Function to update the needed files to Generate a Pick
*!             ticket for the current record of the temp. Order lines file
*!*************************************************************
*! Passed Parameters : Pick ticket number
*!*************************************************************
*
FUNCTION lfGenPikTk
PARAMETERS lcPickParm

SELECT PIKTKT
*IF There is a no record for this Pick ticket number in the PIKTKT file

IF !SEEK(GenPikTkt.Order + lcPickParm)
  APPEND BLANK
  = RLOCK()
*B603603,1 Start.
*  REPLACE Account   WITH GenPikTkt.Account   ,;
          Store     WITH GenPikTkt.Store     ,;
          Order     WITH GenPikTkt.Order     ,;
          cPickType WITH GenPikTkt.cPickType ,;
          PikTkt    WITH lcPickParm          ,;
          Date      WITH gdSysDate           ,;
          cWareCode WITH PADR(laware[lnTware],6),;
          CustPo    WITH IIF(ORDHDR.MultiPo , GenPikTkt.CustPo , ORDHDR.CustPo) ,;
          Status    WITH 'O'
  REPLACE Account   WITH GenPikTkt.Account   ,;
          Store     WITH GenPikTkt.Store     ,;
          Order     WITH GenPikTkt.Order     ,;
          cPickType WITH GenPikTkt.cPickType ,;
          PikTkt    WITH lcPickParm          ,;
          Date      WITH gdSysDate           ,;
          cWareCode WITH ORDHDR.cWareCode    ,;
          CustPo    WITH IIF(ORDHDR.MultiPo , GenPikTkt.CustPo , ORDHDR.CustPo) ,;
          Status    WITH 'O'
*B603603,1 End.

  =gfAdd_Info()
  UNLOCK
  
  *-- Audit Trial.
  *IF ASCAN(laEvntTrig,PADR("GEN_PIK",10)) <> 0
  *  =gfDoTriger('ALGENPK',PADR("GEN_PIK",10))
  *ENDIF

ENDIF    && End of IF

SELECT PIKTKT
*B603111,1 Add new Piktkt to temp. session file. [Begin]
IF !SEEK(Order+Store+cPickType,lcTmpPkTk)
  m.Order = Order
  m.Store = Store
  m.cPickType = cPickType
  m.PikTkt = PikTkt
  INSERT INTO (lcTmpPkTk) FROM MEMVAR
ENDIF
*B603111,1 Add new Piktkt to temp. session file. [End  ]

SELECT GenPikTkt
*-- end of lfGenPikTk.

*!*************************************************************
*! Name      : lfAlowBack
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check allow back order flag.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!
FUNCTION lfAlowBack

RETURN OrdHdr.lAlwBkOrd
*-- end of lfAlowBack.



*!*************************************************************
*! Name      : LFSETBRFLD
*! Developer : TAK
*! Date      : 10/25/99
*! Purpose   : Set Browse fields for the custom Rcv/Issu screen.
*!*************************************************************
FUNCTION LFSETBRFLD

lcBrTpNme  = 'Ad.PO #'
IF lcPtype = 'E'
  lcBlFields = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
               "ShipNo :R :H='Piktkt #':10,"+;
               "lcOrdBr = PADR(Reference,6) :R :H='Order #':10,"+;
               IIF(llMfCall,"Cuttkt","Po")+" :R :H=lcBrTpNme :10,"+;
               "Style     :R :H=lcStyHdr :25,"+;
               IIF(llWareHous ,"lcWHTtl = IIF(lcPType$'NA',Vendor,cWareCode) :H=IIF(lcPType$'NA','Source','Location'):10,","")+;
               IIF(lcPType$'NA',"cWareCode :H='Target':10,","")+;
               "nTotQty = MAX(Qty1,0)+MAX(Qty2,0)+MAX(Qty3,0)+MAX(Qty4,0)+;
                          MAX(Qty5,0)+MAX(Qty6,0)+MAX(Qty7,0)+MAX(Qty8,0) :R :H='Original':8,"+;
               "TotStk :R :H='Stock'   :8,"+;
               "TotDam :R :H='Other'   :8,"+;
               "TotCan :R :H='Cancel'  :8,"+;
               "nTotBal = MAX(MAX(Qty1,0)+MAX(Qty2,0)+MAX(Qty3,0)+MAX(Qty4,0)+;
                          MAX(Qty5,0)+MAX(Qty6,0)+MAX(Qty7,0)+MAX(Qty8,0) - ;
                          TotStk - TotDam - TotCan,0) :R :H='Balance' :8"
ELSE
  lcBlFields = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
               "ShipNo :R :H='Piktkt #':9,"+;
               "lcOrdBr = PADR(Reference,6) :R :H='Order #':9,"+;
               IIF(llMfCall,"Cuttkt","Po")+" :R :H=lcBrTpNme :10,"+;
               "Style     :R :H=lcStyHdr :24,"+;
               IIF(llWareHous ,"lcWHTtl = IIF(lcPType$'NA',Vendor,cWareCode) :H=IIF(lcPType$'NA','Source','Location'):10,","")+;
               IIF(lcPType$'NA',"cWareCode :H='Target':10,","")+;
               "nTotQty = MAX(Qty1,0)+MAX(Qty2,0)+MAX(Qty3,0)+MAX(Qty4,0)+;
                          MAX(Qty5,0)+MAX(Qty6,0)+MAX(Qty7,0)+MAX(Qty8,0) :R :H='Original':8,"+;
               "TotStk :R :H='Stock'   :7,"+;
               "nTotBal = MAX(MAX(Qty1,0)+MAX(Qty2,0)+MAX(Qty3,0)+MAX(Qty4,0)+;
                          MAX(Qty5,0)+MAX(Qty6,0)+MAX(Qty7,0)+MAX(Qty8,0) - ;
                          TotStk - TotDam - TotCan,0) :R :H='Bal.To Cancel' :14"
ENDIF
RETURN



*!*************************************************************
*! Name      : lfSAVADO
*! Developer : TAK
*! Date      : 10/27/99
*! Purpose   : Save adornment order , process -> 'UPDADORN'.
*!*************************************************************
FUNCTION lfSAVADO

SELECT CUTPICK
SET ORDER TO TAG Cutord

*-- Generate a new sequence number if you are sure that you will update.
IF laType[lnType,2]='G' OR EMPTY(lcAdorNO)
   *C101929,1 SSH 06/09/00  [Start Generate separate sequance no for adornment order.
   *lcAdorNO=gfSequence('PO')
   lcAdorNO=gfSequence('POAD')
   *C101929,1 SSH 06/09/00  END]
ENDIF


SELECT (lcTmpOrder)
SCAN FOR llSel          &&-- (lcTmpOrder)

  *--Make a pointers.
  IF !SEEK('O'+Order+STR(LineNo,6),'ORDLINE') OR ORDLINE.Piktkt='*'
    LOOP
  ENDIF

  IF SEEK('2'+ORDLINE.Order+STR(ORDLINE.Lineno,6),'CUTPICK')
    LOOP
  ENDIF

  lnLineNo=POSHDR.LastLine+1

  *-- 1) Update BOMVAR ************************************************
  lnNcost2=0
  IF laType[lnType,2]='G'  &&-- new line not exist in this order befor .
    SELECT (lcTmpBom)
    =SEEK("SO"+&lcTmpOrder..order+STR(&lcTmpOrder..lineno,6))
    SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)="SO"+&lcTmpOrder..order+STR(&lcTmpOrder..lineno,6);
                FOR llSel
      lnNcost2 = lnNcost2 + TotCost
      SCATTER MEMVAR MEMO
      m.Cidtype  = 'AD'
      m.Ccost_id = lcAdorNO
      m.Lineno   = lnLineNo
      INSERT INTO BOMVAR FROM MEMVAR
    ENDSCAN

  ELSE
    SELECT (lcTmpBom)
    SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)="SO"+&lcTmpOrder..order+STR(&lcTmpOrder..lineno,6) FOR  LLSEL
      =SEEK("SO"+&lcTmpOrder..order+STR(&lcTmpOrder..lineno,6),'BOMVAR')
      SELECT BOMVAR
      SCAN WHILE cidtype+ccost_id+STR(lineno,6)="SO"+&lcTmpOrder..order+STR(&lcTmpOrder..lineno,6)
        lcCdsgncode = CDSGNCODE+CNDRPID
        lnRecNo = RECNO() 
        =SEEK('AD'+lcAdorNO+PADL(ALLT(CUTPICK.CTKTLINENO),6),'BOMVAR')
        LOCATE REST WHILE cidtype+ccost_id+STR(lineno,6)='AD'+lcAdorNO+PADL(ALLT(CUTPICK.CTKTLINENO),6) ;
               FOR CDSGNCODE+CNDRPID = lcCdsgncode
        llFoundRec = FOUND()
        GOTO lnRecNo
        lnNcost2=lnNcost2+TOTCOST
        IF llFoundRec
          LOOP
        ENDIF    
        SCATTER MEMVAR MEMO
        m.Cidtype  = 'AD'
        m.Ccost_id = lcAdorNO
        m.Lineno   = lnLineNo
        INSERT INTO BOMVAR FROM MEMVAR
        GOTO lnRecNo
      ENDSCAN
    ENDSCAN
  ENDIF 

  SELECT (lcTmpOrder)
  REPLACE &lcTmpOrder..Ncost2 WITH lnNcost2
  *-- ************************************************


  SELECT (lcTmpOrder)  
  *-- 2) Update STYDYE ************************************************    
  =lfUStyWIP()

  =SEEK(Style,'STYLE')
  =SEEK(Style+ORDLINE.cWareCode+SPACE(10),'STYDYE')  


  *-- 3) Update POSLN ************************************************
  *--Create a new line.
  SELECT POSLN  
  APPEND BLANK
  REPLACE CSTYTYPE  WITH 'A'                     ,;
          PO        WITH lcAdorNO                ,;
          TRANCD    WITH '1'                     ,;
          LineNo    WITH lnLineNo                ,;
          VENDOR    WITH PADR(laware[lnSware],6) ,;
          STYLE     WITH &lcTmpOrder..STYLE      ,;
          Reference WITH &lcTmpOrder..Order+STR(&lcTmpOrder..LineNo,6),;
          SCALE     WITH &lcTmpOrder..SCALE      ,;
          PREPAK    WITH &lcTmpOrder..PREPAK     ,;
          PPQTY     WITH &lcTmpOrder..PPQTY
  FOR lnX= 1 TO 8
    Z=STR(lnX,1)
    REPLACE QTY&Z   WITH &lcTmpOrder..PIK&Z      ,;
            ORD&Z   WITH &lcTmpOrder..PIK&Z
  ENDFOR  
  REPLACE TOTQTY    WITH &lcTmpOrder..TOTPIK     ,;
          TOTORD    WITH &lcTmpOrder..TOTPIK     ,;
          SHIPNO    WITH &lcTmpOrder..PIKTKT     ,;  
          NCOST1    WITH STYDYE.AVE_COST         ,;
          NCOST2    WITH &lcTmpOrder..NCOST2     ,;
          STORE     WITH ORDHDR.STORE            ,;
          CWARECODE WITH PADR(laware[lnTware],6) ,;
          NECOST1   WITH STYDYE.AVE_COST         ,;
          NECOST2   WITH &lcTmpOrder..NCOST2
  =gfAdd_Info('POSLN')



  *-- 4) Update ORDLINE ************************************************   
  SELECT ORDLINE
  =RLOCK() 
  *B603603,1 Start, Comment out.
  *REPLACE cWareCode WITH PADR(laware[lnTware],6)
  *B603603,1 End.

  FOR lnX=1 TO 8
    Z=STR(lnX,1)
    REPLACE ORDLINE.CUT&Z WITH ORDLINE.CUT&Z+&lcTmpOrder..PIK&Z
  ENDFOR 
  REPLACE ORDLINE.TOTCUT WITH ORDLINE.TOTCUT+&lcTmpOrder..TOTPIK
  UNLOCK
  SELECT ORDHDR
  REPLACE ORDHDR.TOTCUT  WITH ORDHDR .TOTCUT+&lcTmpOrder..TOTPIK


  *-- 5) Update CUTPICK ************************************************   
  SELECT CUTPICK
  APPEND BLANK
  REPLACE CTKTNO WITH lcAdorNO ,;
          TRANCD WITH '2'      ,;
          ORDER  WITH &lcTmpOrder..ORDER      ,; 
          CORDLINE WITH STR(&lcTmpOrder..LINENO,6)      ,; 
          CTKTLINENO WITH STR(lnLineNo,6) ,;
          STYLE WITH &lcTmpOrder..STYLE      
  FOR lnX=1 TO 8
    Z=STR(lnX,1)
    REPLACE QTY&Z WITH &lcTmpOrder..PIK&Z
  ENDFOR
  REPLACE TOTQTY WITH &lcTmpOrder..TOTPIK
  =gfAdd_Info('CUTPICK')

    

  *-- 6) Update POSHDR ************************************************   
  SELECT POSHDR
  =RLOCK()
  REPLACE PO        WITH lcAdorNO ,;
          NSTYORDER WITH NSTYORDER+&lcTmpOrder..TOTPIK ,;
          OPEN      WITH OPEN     +&lcTmpOrder..TOTPIK ,; 
          LastLine  WITH lnLineNo
  REPLACE NICOST1  WITH NICOST1+(POSLN.NCOST1*&lcTmpOrder..TOTPIK) ,;
          NFCOST1  WITH NICOST1,;  
          NICOST2  WITH NICOST2+(POSLN.NCOST2*&lcTmpOrder..TOTPIK) ,;
          NFCOST2  WITH NICOST2,;
          POTOTAL  WITH NICOST1+NICOST2
  UNLOCK

  SELECT (lcTmpOrder)  


  *--Update Audit Trail.
  =SEEK('O'+Order+STR(Lineno,6),'ORDLINE')

  IF lcTrgAudit # ORDER+PIKTKT 
    lcTrgAudit=ORDER+PIKTKT
    *-- Audit Trial.
    IF ASCAN(laEvntTrig,PADR("ADRN_ORD",10)) <> 0
      =gfDoTriger('MFADORD',PADR("ADRN_ORD",10))
    ENDIF
  ENDIF
  
  SELECT (lcTmpOrder)
ENDSCAN

*-----------------------    Main Loop [end..]    ----------------------


*-- XXX is saved as XXX
SELECT (lcTmpOrder)
SELECT DISTINCT PikTkt FROM (lcTmpOrder) WHERE llSEL ORDER BY PikTkt INTO ARRAY laPikNo
lcPkFrm = laPikNo[1]
lcPkTO  = laPikNo[ALEN(laPikNo,1)]
=gfModalGen('INM42085B42000','DIALOG',;
      'This adornment order'+'|'+lcAdorNO+' with adornment pick ticket'+IIF(lcPkFrm=ALLT(lcPkTO),': ','(s): ')+lcPkFrm+IIF(lcPkFrm=ALLT(lcPkTO),'',' to-> '+lcPkTO) )


SELECT (lcTmpOrder)        
ZAP
SET RELATION OFF INTO (lcTmpBom)
SET RELATION OFF INTO Piktkt 
SELECT (lcTmpBom)
ZAP
SELECT (lcTmpDsgn) 
ZAP
RETURN




*!*************************************************************
*! Name      : LFUPDIAD
*! Developer : TAK
*! Date      : 10/25/99
*! Purpose   : Issue Adornment order custom process.
*!*************************************************************
FUNCTION LFUPDIAD


= gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
= gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')

SELECT STYDYE
SET ORDER TO TAG Stydye
SELECT CUTPICK
SET ORDER TO TAG Cutpkord
SELECT POSLN
SET ORDER TO TAG Posln   &&*cstytype+po+style+STR(lineno,6)+trancd.

SELECT (lcTmpLine)
SELECT DIST PO,SHIPNO,PADR(REFERENCE,6) AS ORDER FROM (lcTmpLine) INTO DBF (gcWorkDir+'POAudtTl.dbf')
*--If pulled at zero.
DIME laZeroArry[18]
laZeroArry = 0
SELECT POAudtTl
SCAN
  SELECT POSLN
  SEEK 'A'+POAudtTl.PO
  SCAN WHILE cStyType+Po = 'A'+POAudtTl.PO FOR TranCd = '1' AND ShipNo=POAudtTl.ShipNo
    lcTKey = cStyType+Po+Style+STR(LineNo,6)
    SCATTER MEMVAR MEMO
    SELECT (lcTmpLine)
    LOCATE FOR cStyType+Po+Style+STR(LineNo,6)+TranCd=lcTKey+'2'
    *--If no issued was done.
    IF !FOUND()
      *--Release this line.
      APPEND BLANK
      GATHER MEMVAR
      REPLACE TranCd WITH '2'
      GATHER FROM laZeroArry FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
      REPLACE clotno WITH 'X'
    ENDIF
  ENDSCAN
ENDSCAN


SELECT (lcTmpLine)
GO TOP
SCAN FOR TranCd = '2'
  *--Update Ordered qty.
  FOR lnI =1 TO 8
    lcI = STR(lnI,1)
    REPLACE Ord&lcI WITH Qty&lcI
  ENDFOR
  REPLACE TotOrd WITH TotQty
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO laNewQArr

  * 1- update Posln
  =SEEK('A'+Po+Style+STR(Lineno,6)+'1','POSLN')
  lcSrcWarCd = POSLN.cWareCode
  SELECT POSLN
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laOLDQArr
  lnOldTotQy = TotQty
  =RLOCK()
  GATHER FROM laNewQArr FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
  UNLOCK

  * 2- Update Poshdr
  SELECT POSHDR
  =SEEK('A'+&lcTmpLine..Po)
  =RLOCK()
  REPLACE nStyOrder WITH nStyOrder - (lnOldTotQy - POSLN.TotQty ),;
          Open      WITH Open      - (lnOldTotQy - POSLN.TotQty ),;
          nICost1   WITH nICost1   - ((lnOldTotQy - POSLN.TotQty )* POSLN.nECost1),;
          nICost2   WITH nICost2   - ((lnOldTotQy - POSLN.TotQty )* POSLN.nECost2),;
          nFCost1   WITH nICost1 ,;
          nFCost2   WITH nICost2
  UNLOCK

  * 3- Update CutPick
  SELECT CUTPICK
  =SEEK('2'+&lcTmpLine..Po+STR(&lcTmpLine..LineNo,6),'CUTPICK')
  =RLOCK()
  GATHER FROM laNewQArr FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
  IF TotQty=0
    DELETE
  ENDIF
  UNLOCK

    
  * 4- Update Order files.
  SELECT ORDHDR
  =SEEK('O'+CUTPICK.Order,'ORDHDR')
  =RLOCK()
  REPLACE TotCut WITH TotCut - ( lnOldTotQy - &lcTmpLine..TotQty)
  UNLOCK
  SELECT ORDLINE
  =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')    
  =RLOCK()
  REPLACE TotCut WITH TotCut - ( lnOldTotQy - &lcTmpLine..TotQty),;
          TotPik WITH TotPik - ( lnOldTotQy - &lcTmpLine..TotQty)
  FOR lnSizeNo = 1 TO 8
    lcSz = STR(lnSizeNo,1)
    REPLACE Cut&lcSz WITH Cut&lcSz - (laOLDQArr[lnSizeNo] - laNewQArr[lnSizeNo]),;
            Pik&lcSz WITH Pik&lcSz - (laOLDQArr[lnSizeNo] - laNewQArr[lnSizeNo])
  ENDFOR
  IF TotPik=0
    REPLACE PIKTKT  WITH ' ',;
            PIKDATE WITH {},;
            PICKED  WITH .F.
  ENDIF
  UNLOCK
  

  *--Update Piktkt.
  SELECT PikTkt
  IF SEEK(POSLN.ShipNo)
    *B603603,1 Update by target warehouse.
    =RLOCK()
    REPLACE Status    WITH 'P',;
            cWareCode WITH lcSrcWarCd
    UNLOCK 
    *B603603,1 End.
  ENDIF

  * 6- Update style.
  =SEEK(POSLN.Style,'STYLE')
  SELECT STYLE
  =RLOCK()
  FOR lnSizeNo = 1 TO 8
    lcSz = STR(lnSizeNo,1)
    REPLACE WIP&lcSz WITH WIP&lcSz - (laOLDQArr[lnSizeNo] - laNewQArr[lnSizeNo]),;
            Alo&lcSz WITH Alo&lcSz - (laOLDQArr[lnSizeNo] - laNewQArr[lnSizeNo])
  ENDFOR
  REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8,;
          TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
  UNLOCK 


  SELECT STYDYE
  =SEEK(POSLN.Style+lcSrcWarCd+SPACE(10))
  =RLOCK()
  FOR lnSizeNo = 1 TO 8
    lcSz = STR(lnSizeNo,1)
    REPLACE WIP&lcSz WITH WIP&lcSz - (laOLDQArr[lnSizeNo] - laNewQArr[lnSizeNo])
  ENDFOR
  REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
  UNLOCK 


  *B603603,1 Start.
  * 7- Update styDye.
  IF ORDLINE.cWareCode = ORDHDR.cWareCode
    SELECT ORDLINE
    =RLOCK()
    REPLACE cWareCode WITH lcSrcWarCd
    UNLOCK
  
    SELECT STYDYE
    =SEEK(POSLN.Style+ORDLINE.cWareCode+SPACE(10))
    =RLOCK()
    FOR lnSizeNo = 1 TO 8
      lcSz = STR(lnSizeNo,1)
      REPLACE Alo&lcSz WITH Alo&lcSz + laNewQArr[lnSizeNo],;
              Ord&lcSz WITH Ord&lcSz + laNewQArr[lnSizeNo]
    ENDFOR
    REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8,;
            TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
    UNLOCK 

    SELECT STYDYE
    =SEEK(POSLN.Style+ORDHDR.cWareCode+SPACE(10))
    =RLOCK()
    FOR lnSizeNo = 1 TO 8
      lcSz = STR(lnSizeNo,1)
      REPLACE Alo&lcSz WITH Alo&lcSz - laOLDQArr[lnSizeNo],;
              Ord&lcSz WITH Ord&lcSz - laNewQArr[lnSizeNo]
    ENDFOR
    REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8,;
            TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
    UNLOCK 
  ENDIF
  *B603603,1 End.

  SELECT (lcTmpLine)
ENDSCAN


*--Update Audit Trail.
SELECT POAudtTl
SCAN
  = gfDoTriger('MFRCVAP',PADR("PK_PULLD",10))
ENDSCAN
USE

SELECT (lcTmpLine)
DELETE ALL FOR clotno = 'X'

SELECT (lcTmpLine)
RETURN



*!*************************************************************
*! Name      : LFPREADRRV
*! Developer : TAK
*! Date      : 11/01/99
*! Purpose   : Receiving Adornment order should Cancel the rest
*!             of Open qty on the Ador Line and update order Pik qty.
*! process   : 'UPDRCEVAD'.
*!*************************************************************
*B603493,1 KHM 03/01/2000 
*!*************************************************************
FUNCTION LFPREADRRV

*--Cancel the rest of Open qty on the Adornment line after receiving.

= gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
= gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
SELECT STYDYE
SET ORDER TO TAG Stydye
SELECT CUTPICK
SET ORDER TO TAG Cutpkord

SELECT (lcTmpLine)
SELECT DIST PO,SHIPNO,PADR(REFERENCE,6) AS ORDER,clotno FROM (lcTmpLine) INTO DBF (gcWorkDir+'POAudtTl.dbf')

SELECT (lcTmpLine)
SET ORDER TO TAG POSLN
USE (gcWorkDir+lcTmpLine) IN 0 AGAIN ALIAS PTMPLINE ORDER TAG POSLN
SELECT (lcTmpLine)
GO TOP
SCAN FOR TranCd = '1'
  lcTKey = cStyType+Po+Style+STR(LineNo,6)  
  DIME laUQty[9]
  laUQty = 0
  SELECT PTMPLINE
  =SEEK(lcTKey)
  SCAN WHILE cStyType+Po+Style+STR(LineNo,6) =lcTKey FOR TranCd<>'5'
    SCATTER MEMVAR
    FOR lnI=1 TO 8
      lcI = STR(lnI,1)
      laUQty[lnI] = laUQty[lnI] + IIF(TranCd='1',Qty&lcI,-Qty&lcI)
    ENDFOR
    laUQty[9] = laUQty[9] + IIF(TranCd='1',TotQty,-TotQty)
  ENDSCAN
  *--If there is something to cancel.
  IF laUQty[9]<>0
    SELECT (lcTmpLine)
    IF !SEEK(lcTKey+'5',lcTmpLine)
      APPEND BLANK
      GATHER MEMVAR
      REPLACE TranCd WITH '5'
    ENDIF
    FOR lnI=1 TO 8
      lcI = STR(lnI,1)
      *B603493,1 KHM 03/01/2000 (Begin) Commenting the IF condition
      *IF laUQty[lnI]<>0
        REPLACE Qty&lcI WITH laUQty[lnI]
      *ENDIF
      *B603493,1 KHM 03/01/2000 (End)
    ENDFOR
    REPLACE TotQty WITH laUQty[9]
    *--Put the pointer back.
    =SEEK(lcTKey)
    LOCATE REST WHILE cStyType+Po+Style+STR(LineNo,6)=lcTKey FOR TranCd='1'
  ENDIF
ENDSCAN
USE IN PTMPLINE


SELECT (lcTmpLine)
SCAN FOR TRANCD='5'
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laQtyCanl

  * 1- Update CutPick
  SELECT CUTPICK
  =SEEK('2'+&lcTmpLine..Po+STR(&lcTmpLine..LineNo,6),'CUTPICK')
  =RLOCK()
  FOR lnI=1 TO 8
    lcZ=STR(lnI,1)
    REPLACE Qty&lcZ WITH MAX(Qty&lcZ-laQtyCanl[lnI],0)
  ENDFOR
  REPLACE TotQty WITH MAX(TotQty-laQtyCanl[9],0)
  IF TotQty=0
    DELETE
  ENDIF  
  UNLOCK

  * 2- Update Order files.
  SELECT ORDHDR
  =SEEK('O'+CUTPICK.Order,'ORDHDR')
  =RLOCK()
  REPLACE TotCut WITH MAX(TotCut-laQtyCanl[9],0)
  UNLOCK
  SELECT ORDLINE
  =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')    
  =RLOCK()
  REPLACE TotCut WITH MAX(TotCut - laQtyCanl[9],0),;
          TotPik WITH MAX(TotPik - laQtyCanl[9],0)
  FOR lnI = 1 TO 8
    lcZ = STR(lnI,1)
    REPLACE Cut&lcZ WITH MAX(Cut&lcZ-laQtyCanl[lnI],0),;
            Pik&lcZ WITH MAX(Pik&lcZ-laQtyCanl[lnI],0)
  ENDFOR
  IF TotPik=0
    REPLACE PIKTKT  WITH ' ',;
            PIKDATE WITH {},;
            PICKED  WITH .F.
  ENDIF
  UNLOCK


  * 3- Update style.
  =SEEK(&lcTmpLine..Style,'STYLE')
  SELECT STYLE
  =RLOCK()
  FOR lnX = 1 TO 8
    lcSz = STR(lnX,1)
    REPLACE Alo&lcSz WITH Alo&lcSz - laQtyCanl[lnX]
  ENDFOR
  REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
  UNLOCK 


  * 7- Update styDye.
  SELECT STYDYE
  =SEEK(&lcTmpLine..Style+PADR(&lcTmpLine..cWareCode,6)+SPACE(10))
  =RLOCK()
  FOR lnX = 1 TO 8
    lcSz = STR(lnX,1)
    REPLACE Alo&lcSz WITH Alo&lcSz - laQtyCanl[lnX]
  ENDFOR
  REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
  UNLOCK 

  *--Put the difference back to scource on order.
  IF ORDHDR.cWareCode <> &lcTmpLine..cWareCode
    SELECT STYDYE
    =SEEK(&lcTmpLine..Style+ORDLINE.cWareCode+SPACE(10),'STYDYE')
    =RLOCK()
    FOR lnX= 1 TO 8
      Z=STR(lnX,1)
      REPLACE ORD&Z WITH ORD&Z - laQtyCanl[lnX]
    ENDFOR
    REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
    UNLOCK
    =SEEK(&lcTmpLine..Style+ORDHDR.cWareCode+SPACE(10),'STYDYE')
    =RLOCK()
    FOR lnX= 1 TO 8
      Z=STR(lnX,1)
      REPLACE ORD&Z WITH ORD&Z + laQtyCanl[lnX]
    ENDFOR
    REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
    UNLOCK
  ENDIF

  *--Update audit trail as partial.
  SELECT POAudtTl
  LOCATE FOR PO=&lcTmpLine..PO AND ShipNo=&lcTmpLine..ShipNo
  IF FOUND() AND EMPTY(clotno)
    REPLACE clotno WITH 'P'
  ENDIF  
  SELECT (lcTmpLine)
  
ENDSCAN

*--Update Audit Trail.
*B603237,1 Added to update an audit trail.
SELECT POAudtTl
SCAN
  IF !EMPTY(clotno)
    =gfDoTriger('MFRCVAR',PADR("RECV_ADOP",10))
  ELSE
    =gfDoTriger('MFRCVAR',PADR("RECV_ADOC",10))
  ENDIF  
ENDSCAN
USE

SELECT (lcTmpLine)
RETURN



*!***************************************************************************
*! Name      : lfUStyWIP                                                  
*! Developer : Walid Abou El-Magd                                            
*! Date      : 07/01/1999                                                   
*! Purpose   :                                                               
*!***************************************************************************
*! Calls     : None                                                         
*!***************************************************************************
*! Passed Parameters  :  None                                               
*!***************************************************************************
*! Returns            :  None                                               
*!***************************************************************************
*! Example            :                                                     
*!***************************************************************************
FUNCTION lfUStyWIP
PRIVATE lcAlias

lcAlias=SELECT()
IF SEEK(&lcTmpOrder..STYLE,'STYLE')
  SELECT STYLE
  FOR lnX= 1 TO 8
    Z=STR(lnX,1)
    REPLACE WIP&Z WITH WIP&Z + &lcTmpOrder..PIK&Z
  ENDFOR
  REPLACE TOTWIP WITH TOTWIP + &lcTmpOrder..TOTPIK
ENDIF


IF !SEEK(&lcTmpOrder..STYLE+PADR(laware[lnSware],6)+SPACE(10),'STYDYE')
  WAIT "Assigning Style"+ALLTRIM(&lcTmpOrder..STYLE)+" to warehouse "+PADR(laware[lnSware],6) WINDOW NOWAIT
  DO gpAdStyWar WITH &lcTmpOrder..STYLE,SPACE(10),PADR(laware[lnSware],6)
  =SEEK(&lcTmpOrder..STYLE+PADR(laware[lnSware],6)+SPACE(10),'STYDYE')
  WAIT CLEAR
ENDIF


IF !SEEK(&lcTmpOrder..STYLE+PADR(laware[lnTware],6)+SPACE(10),'STYDYE')
  WAIT "Assigning Style"+ALLTRIM(&lcTmpOrder..STYLE)+" to warehouse "+PADR(laware[lnTware],6) WINDOW NOWAIT
  DO gpAdStyWar WITH &lcTmpOrder..STYLE,SPACE(10),PADR(laware[lnTware],6)
  =SEEK(&lcTmpOrder..STYLE+PADR(laware[lnTware],6)+SPACE(10),'STYDYE')
  WAIT CLEAR
ENDIF

FOR lnX= 1 TO 8
  Z=STR(lnX,1)
  REPLACE STYDYE.WIP&Z WITH STYDYE.WIP&Z + &lcTmpOrder..PIK&Z
ENDFOR
REPLACE STYDYE.TOTWIP WITH STYDYE.TOTWIP + &lcTmpOrder..TOTPIK

*B603603,1 Start, Comment out.
*IF ORDLINE.CWARECODE <> PADR(laware[lnTware],6)
*  SELECT STYDYE
*  =SEEK(&lcTmpOrder..STYLE+ORDLINE.CWARECODE+SPACE(10),'STYDYE')
*  =RLOCK()
*  FOR lnX= 1 TO 8
*    Z=STR(lnX,1)
*    REPLACE ALO&Z WITH ALO&Z - &lcTmpOrder..PIK&Z,;
*            ORD&Z WITH ORD&Z - &lcTmpOrder..PIK&Z
*  ENDFOR
*  REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8,;
*          TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
*  UNLOCK
*  =SEEK(&lcTmpOrder..STYLE+PADR(laware[lnTware],6)+SPACE(10),'STYDYE')  
*  =RLOCK()
*  FOR lnX= 1 TO 8
*    Z=STR(lnX,1)
*    REPLACE ALO&Z WITH ALO&Z + &lcTmpOrder..PIK&Z,;
*            ORD&Z WITH ORD&Z + &lcTmpOrder..PIK&Z
*  ENDFOR
*  REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8,;
*          TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
*  UNLOCK
*ENDIF
*B603603,1 End.
SELECT (lcAlias)


*!***************************************************************************
*! Name      : lfAdPoBrFl
*! Developer : TAK
*! Date      : 11/28/1999
*! Purpose   : Adornment P/O browse fields.                                                              
*!***************************************************************************
FUNCTION lfAdPoBrFl

=gfOpenFile(gcDataDir+'PIKTKT' ,'PIKTKT','SH')
IF llQtyPSize
  lcBrD1fld= [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
             IIF(laScrMode[1],[],[lcAdorSO = IIF(SEEK(ShipNo,'PIKTKT'),PIKTKT.Order,'') :H='Order' :R,])+;
             [ShipNo  :H='Piktkt' :R,]+;
             [Style :30 :H=lcStyHdr :R,]+;
             [Qty1  :H='Size1':R,]+;
             [Qty2  :H='Size2':R,]+;
             [Qty3  :H='Size3':R,]+;
             [Qty4  :H='Size4':R,]+;
             [Qty5  :H='Size5':R,]+;
             [Qty6  :H='Size6':R,]+;
             [Qty7  :H='Size7':R,]+;
             [Qty8  :H='Size8':R,]+;
             [TotQty  :H='Qty'   :R :P='9999999']
ELSE
  lcBrD1fld= [cMarker  = IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
             IIF(laScrMode[1],[],[lcAdorSO = IIF(SEEK(ShipNo,'PIKTKT'),PIKTKT.Order,'') :H='Order' :R,])+;
             [ShipNo  :H='Piktkt' :R,]+;
             [Style   :30 :H=lcStyHdr :R,]+;
             [TotQty  :H='Qty'   :R :P='9999999']
ENDIF
IF llCostPrv
  lcBrD1fld=lcBrD1fld+[,nCost1  :H='Net '+lcPfrnsmbl :R:P='9999999.999',]+;
                      [lnAmount=(nCost1*TotQty) :H='Amount' :P='999999999.999']
ENDIF
lcBrD1fld=lcBrD1fld+[,cWareCode :H='Ship To']



*!***************************************************************************
*! Name      : LFUPDPIKLN
*! Developer : TAK
*! Date      : 11/28/1999
*! Purpose   : Invoice Sales Order updates.
*B603318,1
*!***************************************************************************
FUNCTION LFUPDPIKLN
PRIVATE lcAlias
lcAlias=SELECT()
llOpByMe1=gfOpenFile(gcDataDir+'CUTPICK','Cutord' ,'SH')
IF SEEK('2'+ORDLINE.Order+STR(ORDLINE.lineNo,6),'CUTPICK')
  llOpByMe2=gfOpenFile(gcWorkDir+lcTmpPikLn,'','SH')
  SELECT CUTPICK
  lcAdrPo   = cTktNo
  lnAPOline = INT(VAL(cTktLineNo))
  =RLOCK()
  DELETE
  UNLOCK
  SELECT (lcTmpPikLn)
  REPLACE ALL PO WITH lcAdrPo, NPOLINENO WITH lnAPOline
  IF llOpByMe2
    USE IN (lcTmpPikLn)
  ENDIF  
ENDIF
IF llOpByMe1
  USE IN CUTPICK
ENDIF  
SELECT (lcAlias)
RETURN



*!***************************************************************************
*! Name      : LFURELADPT
*! Developer : TAK
*! Date      : 12/05/1999
*! Purpose   : Prepere or update the Release adornment Pick Ticket.
*B603318,1   : Process : UPDRELEASE
*!***************************************************************************
FUNCTION LFURELADPT

IF llPrepere
  =gfOpenFile(gcDataDir+'CUTPICK','Cutord' ,'SH')
  =gfOpenFile(gcDataDir+'POSHDR' ,'POSHDR' ,'SH')
  =gfOpenFile(gcDataDir+'POSLN'  ,'POSLN'  ,'SH')

  IF llLinkToGl
    STORE '' TO lcGLFYear,lcGLPeriod
    lcGlDist = gfTempName()
    = gfOpenFile(gcDataDir+'GLDIST','GlDistAc','SH')
    SELECT GLDist
    =AFIELDS(laFStru)
    =gfCrtTmp(lcGlDist,@laFStru)
  ENDIF
  lcGlSession = IIF(EMPTY(lcGlSession),gfsequence('GLSESSION'),lcGlSession)

ELSE
  *--Update and close the gl file.
  IF llLinkToGl
    SELECT (lcGlDist)
    REPLACE ALL GlSession with lcGlSession
    USE
    SELECT GlDist
    APPEND FROM (gcWorkDir+lcGlDist)
    ERASE (gcWorkDir+lcGLDist+'.DBF')
    ERASE (gcWorkDir+lcGLDist+'.CDX')
  ENDIF
ENDIF
RETURN


*!***************************************************************************
*! Name      : LFRELADPKT
*! Developer : TAK
*! Date      : 12/05/1999
*! Purpose   : Release adornment Pick Ticket.
*B603318,1   : Process : RELEASE_PK
*!***************************************************************************
FUNCTION LFRELADPKT

*-1) Update Order Header.
SELECT ORDHDR
=SEEK('O'+ORDLINE.Order)
REPLACE ORDHDR.TotCut WITH ORDHDR.TotCut - ORDLINE.TotCut


*-2) Update Order Line.
SELECT ORDLINE
=RLOCK()
FOR lnSizeNo = 1 TO 8
  lcSz = STR(lnSizeNo,1)
  REPLACE ORDLINE.Cut&lcSz WITH 0
ENDFOR
REPLACE ORDLINE.TotCut WITH 0
UNLOCK

*- Update Adornment order.
SELECT CUTPICK
IF SEEK('2'+ORDLINE.Order+STR(ORDLINE.lineNo,6),'CUTPICK')
  llHdrOk=SEEK('A'+CUTPICK.cTktNo,'POSHDR')

  lcAdPoKey = 'A'+CUTPICK.cTktNo+CUTPICK.Style+CUTPICK.cTktLineNo
  =RLOCK()
  DELETE
  UNLOCK
  

  SELECT POSLN
  **********************************************************
  IF SEEK(lcAdPoKey+'2','POSLN')   &&if receiving was done.
    *--Leave the line as it is.
    =lfCngStQty(.T.)

  ELSE

  **********************************************************
  *--1) Updates of original budget po line.
    IF SEEK(lcAdPoKey+'1','POSLN')

      =lfCngStQty(.F.)

      ***********************************    
      SELECT POSHDR
      IF llHdrOk
        =RLOCK()
        REPLACE NSTYORDER WITH MAX(NSTYORDER - POSLN.TotQty, 0) ,;
                OPEN      WITH MAX(OPEN   - POSLN.TotQty, 0) ,;
                TotOrd    WITH MAX(TotOrd - POSLN.TotOrd, 0) ,;
                NICOST1   WITH MAX(NICOST1-(POSLN.NCOST1*POSLN.TotQty), 0) ,;
                NFCOST1   WITH NICOST1,;  
                NICOST2   WITH MAX(NICOST2-(POSLN.NCOST2*POSLN.TotQty), 0) ,;
                NFCOST2   WITH NICOST2,;
                POTOTAL   WITH NICOST1+NICOST2
        UNLOCK
      ENDIF
      *--Delete this line. 
      SELECT POSLN
      =RLOCK()
      DELETE
      UNLOCK
    ENDIF
  **********************************************************
  *--2) Updates of Issued po line.
  **********************************************************
    IF SEEK(lcAdPoKey+'6','POSLN')
      IF POSLN.TotQty<>0   && If not received yet.
        IF llLinkToGl
          *--G/L Array difinition and initialization.
          DECLARE laGLDistAr[2,13]
          laGLDistAr[1,1] = IIF(SEEK(Style+PADR(Vendor,6)+SPACE(10),'STYDYE') AND ;
                                !EMPTY(STYDYE.Gl_link),STYDYE.Gl_link,'DEFDEF')
          laGLDistAr[2,1] = IIF(!EMPTY(POSHDR.Link_Code),POSHDR.Link_Code,'DEFDEF')
          laGLDistAr[1,2] = '006'
          laGLDistAr[2,2] = '013'
          laGLDistAr[1,3] = -1
          laGLDistAr[2,3] = 1
          STORE 'IA' TO laGLDistAr[1,4],laGLDistAr[2,4]
          STORE POSLN.PO   TO laGLDistAr[1,5],laGLDistAr[2,5]
          STORE POSLN.Date TO laGLDistAr[1,6],laGLDistAr[2,6]
          STORE gdSysDate  TO laGLDistAr[1,6],laGLDistAr[2,6]
          STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
          STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
          STORE lcGlDist   TO laGLDistAr[1,9],laGLDistAr[2,9]
          laGLDistAr[2,10] = ''
        ELSE
          DIME laGLDistAr[1,1]
          laGLDistAr = ''
        ENDIF
    
        SELECT POSLN
        DECLARE laAdjust[9]
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laAdjust
        =gfStyCrl('1',Style,PADR(Vendor,6),Dyelot,gdSysDate,PO,@laAdjust,nCost1,;
                   'Reverse issued P/O',lcGlSession,'',0,'','',@laGLDistAr)
      ENDIF
      *--Delete this line. 
      SELECT POSLN
      =RLOCK()
      DELETE
      UNLOCK
      **********************************************************  
    ENDIF
  ENDIF

  SELECT POSHDR
  IF llHdrOk
    =RLOCK()
    REPLACE STATUS  WITH IIF(Open=0 ,IIF(Receive<>0,'C','X'),Status)
    UNLOCK
  ENDIF

ENDIF
RETURN

***************************************************************************
FUNCTION lfCngStQty
*******************
PARA llAldyRcvd

*--Switch the ordered qty.
IF ORDHDR.CWARECODE <> ORDLINE.CWARECODE
  SELECT STYDYE
  =SEEK(ORDLINE.STYLE+ORDLINE.CWARECODE+SPACE(10),'STYDYE')
  =RLOCK()
  FOR lnX= 1 TO 8
    Z=STR(lnX,1)
    REPLACE ORD&Z WITH MAX(ORD&Z - IIF(llAldyRcvd,POSLN.Qty&Z,POSLN.ORD&Z), 0)
  ENDFOR
  REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
  UNLOCK

  =SEEK(ORDLINE.STYLE+ORDHDR.CWARECODE+SPACE(10),'STYDYE')  
  =RLOCK()
  FOR lnX= 1 TO 8
    Z=STR(lnX,1)
    REPLACE ORD&Z WITH ORD&Z + IIF(llAldyRcvd,POSLN.Qty&Z,POSLN.ORD&Z)
  ENDFOR
  REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
  UNLOCK

  SELECT ORDLINE
  =RLOCK()
  REPLACE ORDLINE.cWareCode WITH ORDHDR.cWareCode
  UNLOCK
ENDIF

*--If reveived dont update WIP in style & stydye.
IF llAldyRcvd
  RETURN
ENDIF

IF SEEK(POSLN.Style,'STYLE')
  SELECT STYLE
  =RLOCK()
  FOR lnX= 1 TO 8
    Z=STR(lnX,1)
    REPLACE WIP&Z WITH MAX(WIP&Z - m.Pik&Z, 0)
  ENDFOR
  REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
  UNLOCK
ENDIF

IF SEEK(POSLN.STYLE+POSLN.CWARECODE+SPACE(10),'STYDYE')
  SELECT STYDYE
  =RLOCK()
  FOR lnX= 1 TO 8
    Z=STR(lnX,1)
    REPLACE STYDYE.WIP&Z WITH MAX(WIP&Z - m.Pik&Z, 0)
  ENDFOR
  REPLACE TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8
  UNLOCK
ENDIF 
RETURN


*!***************************************************************
*! Name      : lfNoReceiv
*! Developer : Kahlid Mohi El-Din
*! Date      : 12/06/1999
*! Purpose   : To display a message when there is no received line for
*!             the invoiced piktkt
*!***************************************************************
*! Returns   : .F. if no received record found in the piktkt
*!***************************************************************
FUNCTION lfNoReceiv
PRIVATE lnAlias,llReturn

lnAlias   = SELECT(0)
llPosLn   = gfOpenFile(gcDataDir+'PosLn','PosLnSH' ,'SH')
llCutPick = gfOpenFile(gcDataDir+'CutPick','Cutord' ,'SH')
llReturn  = .T.

IF SEEK('2'+m.Order+STR(m.LineNo,6),'CUTPICK')
  IF !SEEK(m.PikTkt+'A'+CutPick.cTktNo+CUTPICK.Style+CutPick.cTKtLineNo+'2','POSLN')
    IF m.PikTkt <> lcHoldTkt
      *--Adornment PikTkt# '+m.PikTkt+' has not been Received. Cannot be invoiced.
      = gfModalGen('INM40164B00000','ALERT',m.PikTkt)
      lcHoldTkt = m.PikTkt
    ENDIF
    llReturn = .F.
  ELSE
    RETURN(.T.)  
  ENDIF 
  
  IF !SEEK(m.PikTkt+'A'+CutPick.cTktNo+CUTPICK.Style+CutPick.cTKtLineNo+'5','POSLN')
   IF m.PikTkt <> lcHoldTkt
     *--Adornment PikTkt# '+m.PikTkt+' has not been Received. Cannot be invoiced.
     = gfModalGen('INM40164B00000','ALERT',m.PikTkt)
     lcHoldTkt = m.PikTkt
   ENDIF  
   llReturn = .F.
  ELSE
   llReturn = .T.
  ENDIF  
ENDIF
SELECT(lnAlias)
RETURN llReturn



*!***************************************************************************
*! Name      : LFURELADPT
*! Developer : TAK
*! Date      : 01/02/2000
*! Purpose   : If back order retained set original warehouse back in Ordline.
*! Process   : 'SAV_INV   '
*!***************************************************************************
FUNCTION LFOWHSETBK
* 
IF ORDHDR.Status $ 'OH' AND ORDHDR.Open <> 0
  PRIVATE lnAlias
  lnAlias   = SELECT(0)
  
  SELECT ORDLINE
  =SEEK(ORDHDR.cOrdType+ORDHDR.Order)
  SCAN REST WHILE cOrdType+Order=ORDHDR.cOrdType+ORDHDR.Order ;
        FOR TotQty <> 0 AND TotPik = 0 AND cWareCode <> ORDHDR.cWareCode

    SELECT STYDYE
    IF SEEK(ORDLINE.Style+ORDLINE.cWareCode+SPACE(10))
      =RLOCK()
      FOR lnX= 1 TO 8
        Z=STR(lnX,1)
        REPLACE ORD&Z WITH ORD&Z - ORDLINE.QTY&Z
      ENDFOR
      REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
    ENDIF
    IF SEEK(ORDLINE.Style+ORDHDR.cWareCode+SPACE(10))
      =RLOCK()
      FOR lnX= 1 TO 8
        Z=STR(lnX,1)
        REPLACE ORD&Z WITH ORD&Z + ORDLINE.QTY&Z
      ENDFOR
      REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
    ENDIF

    SELECT ORDLINE
    =RLOCK()
    REPLACE cWareCode WITH ORDHDR.cWareCode
    UNLOCK

  ENDSCAN
 
  SELECT(lnAlias)
ENDIF




*!*************************************************************
*! Name      : lfRelLIRAL()
*! Developer : TAK
*! Date      : 02/05/00
*! Purpose   : Release allocation from order line if line item
*!             ration check is faild.
*!*************************************************************
*C200111,1.
FUNCTION lfRelLIRAL

SELECT GenPikTkt
lcDSEt = SET('DELETE')
SET DELETE OFF
GO TOP
SCAN FOR DELETED() AND Flag='R' AND Piktkt='*'
  WAIT WINDOW 'Release allocation for failed item ration order lines :'+Order+'/'+STR(LineNo , 6) NOWAIT
  = SEEK('O' + Order,'ORDHDR')
  IF SEEK('O' + Order + STR(LineNo , 6) , 'ORDLINE')
    SELECT STYLE
    IF SEEK(ORDLINE.Style) 
      = RLOCK()
      REPLACE Alo1   WITH Alo1 - ORDLINE.Pik1,;
              Alo2   WITH Alo2 - ORDLINE.Pik2,;
              Alo3   WITH Alo3 - ORDLINE.Pik3,;          
              Alo4   WITH Alo4 - ORDLINE.Pik4,;          
              Alo5   WITH Alo5 - ORDLINE.Pik5,;          
              Alo6   WITH Alo6 - ORDLINE.Pik6,;
              Alo7   WITH Alo7 - ORDLINE.Pik7,;          
              Alo8   WITH Alo8 - ORDLINE.Pik8,;
              TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
      UNLOCK
    ENDIF
    SELECT STYDYE
    IF SEEK(ORDLINE.Style+ORDLINE.cWareCode)
      = RLOCK()
      REPLACE Alo1   WITH Alo1 - ORDLINE.Pik1,;
              Alo2   WITH Alo2 - ORDLINE.Pik2,;
              Alo3   WITH Alo3 - ORDLINE.Pik3,;          
              Alo4   WITH Alo4 - ORDLINE.Pik4,;          
              Alo5   WITH Alo5 - ORDLINE.Pik5,;          
              Alo6   WITH Alo6 - ORDLINE.Pik6,;
              Alo7   WITH Alo7 - ORDLINE.Pik7,;          
              Alo8   WITH Alo8 - ORDLINE.Pik8,;
              TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
      UNLOCK
    ENDIF
    SELECT ORDLINE
    = RLOCK()
    REPLACE cWareCode WITH ORDHDR.cWareCode,;
            PikTkt  WITH SPACE(6),;
            Picked  WITH .F.,;
            PikDate WITH {},;
            Pik1    WITH 0,;
            Pik2    WITH 0,;
            Pik3    WITH 0,;
            Pik4    WITH 0,;
            Pik5    WITH 0,;
            Pik6    WITH 0,;
            Pik7    WITH 0,;
            Pik8    WITH 0,;                                                                                    
            TotPik  WITH 0
    UNLOCK
  ENDIF
  SELECT ORDHDR
  = RLOCK()
  REPLACE nNetAloQty WITH MAX(nNetAloQty - GenPikTkt.TotPik,0)
  UNLOCK
  *--
ENDSCAN
WAIT CLEAR
SELECT GenPikTkt
SET DELETED &lcDSEt
RETURN

