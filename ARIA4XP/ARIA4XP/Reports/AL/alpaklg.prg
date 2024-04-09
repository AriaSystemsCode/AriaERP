*:***************************************************************************
*: Program file  : ALPAKLG.PRG
*: Program desc. : Packing List Log Report
*: For Report    : ALPAKLG.FRX
*: System        : Aria Advantage Series.(Aria4xp)
*: Module        : Sales Order Allocation (AL)
*: Developer     : Mariam Mazhar (MMT)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: This Program is due to N037541 ...
*E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[T20101014.0041]
*E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[T20110125.0008]
*:***************************************************************************
*-- Variables Declaration
lcSPTime = TIME()         && Variable to hold the Start print Time
loogScroll.cCROrientation = 'P'
lnCurrAmt = 0
*-- if Filter was changed

IF llOGFltCh
  DO lpCreaTemp  && Create Temp Cursor

  IF USED(lcPackPckHdr)
    SELECT(lcPackPckHdr)
    USE IN (lcPackPckHdr) 
  ENDIF 
  *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[Start]
  *  SELECT PACK_NO,ACCOUNT,CWARECODE,STORE,ORDER,SHIP_DATE,TOT_CART,TOT_PCS,TOT_WGHT  FROM  &lcTempPack_Hdr WHERE .F.INTO CURSOR &lcPackPckHdr READWRITE 
  SELECT PACK_NO,ACCOUNT,CWARECODE,STORE,ORDER,SHIP_DATE,TOT_CART,TOT_PCS,TOT_WGHT,bill_ladg  FROM  &lcTempPack_Hdr WHERE .F.INTO CURSOR &lcPackPckHdr READWRITE   
  *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[End]
  =lfMakeIndex(lcPackPckHdr)


  IF lcTempOrdhd = lcTempordhdr
    *--Ordhdr file 
    lcTempordhdr =  loogscroll.gftempname()
  ELSE 
    IF USED(lcTempordhdr)
      SELECT(lcTempordhdr)
      USE IN (lcTempordhdr)
    ENDIF   
  ENDIF 
    SELECT order,multipo,cordtype,custpo,cCurrCode,Nexrate,NcurrUnit FROM &lcTempOrdhd WHERE .F. INTO CURSOR &lcTempordhdr READWRITE 
    =lfMakeIndex(lcTempordhdr)
 
  IF lcTempOrdLine = lcTempordLn 
    *--Ordline file 
    lcTempordLn =  loogscroll.gftempname()
  ELSE
    IF USED(lcTempordLn)
      SELECT(lcTempordLn)
      USE IN (lcTempordLn) 
    ENDIF   
  ENDIF 
    SELECT Cordtype,custpo,price,totqty,order,lineno FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcTempordLn READWRITE 
    =lfMakeIndex(lcTempordLn)
  
  IF lcTempCust = lcTempCustomer
    lcTempCustomer =  loOgScroll.gftempname()
    *--Customer file 
  ELSE 
    IF USED(lcTempCustomer)
      SELECT(lcTempCustomer)
      USE IN (lcTempCustomer)
    ENDIF   
  ENDIF 
    SELECT Type,Account,Dist_ctr,btName,store FROM  &lcTempCust WHERE .F. INTO CURSOR &lcTempCustomer READWRITE  
    =lfMakeIndex(lcTempCustomer)

  *-- data collecting 
  DO lpColect    && Collect data
  
ENDIF

*-- Asking if no records (Display message) otherwise print report [Begin.]
IF RECCOUNT(lcPackTmp) = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

*-- llLastPage : Detect we are to print last page (Used within .FRX)
*-- llNotRep : Detect we are to print Not Repeated Pk#,Order,... (Used within .FRX)
PRIVATE llLastPage
llLastPage = .F.
llNotRep = .F.
*--To get the date in a variables (Date Sides is between)
lnSHdate = ASCAN(laOgFxFlt,'PACK_HDR.SHIP_DATE')    &&Vriable to get ship_date
IF lnSHdate  = 0
  STORE {} TO ldLDate,ldUDate
ELSE
  lnSHdate  = ASUBSCRIPT(laOgFxFlt,lnSHdate ,1)
  ldLDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnSHdate ,6],1,10)))   && Variable to hold the Lower Date
  ldUDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnSHdate ,6],12,20)))  && Variable to hold the Upper Date
ENDIF 

*-- Relation Section
SELECT (lcPackTmp)
  SET ORDER TO TAG &lcPackTmp
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
*!*	  SET RELATION TO "O" + ORDER +  STR(nordlineno,6) INTO &lcTempordLn,;
*!*	                  "O" + ORDER INTO &lcTempordhdr,;
*!*	                  IIF(EMPTY(Store),'M','S') + Account + Store INTO &lcTempCustomer ,;
*!*	                  Pack_No INTO (lcRevTemp) ADDITIVE
  SET RELATION TO "O" + ORDER +  STR(nordlineno,6) INTO &lcTempordLn,;
                  "O" + ORDER INTO &lcTempordhdr,;
                  IIF(EMPTY(Store),'M','S') + Account + Store INTO &lcTempCustomer ,;
                  IIF(lcRpSort='P',PACK_NO,bill_ladg+ STYLE + Pack_No ) INTO (lcRevTemp) ADDITIVE
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]

*--lcTime1 = TIME()
*!*  IF RECCOUNT(lcPackTmp)> 0
*!*    WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPackTmp))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcSPTime,lcTime1),6,2)) + ' Seconds...' TIMEOUT 2
*!*    MESSAGEBOX('Selected ' + ALLTRIM(STR(RECCOUNT(lcPackTmp))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcSPTime,lcTime1),6,2)) + ' Seconds...')
*!*  ENDIF 

*!--End of Relations Section
*!-- Printing Section
SELECT (lcPackTmp)
LOCATE 
DO gfDispRe WITH EVALUATE('lcRpName')

SELECT (lcPackTmp)
SET RELATION TO 
*!-- End of Printing Section


*!**************************************************************************
*! Name      : lpCreaTemp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/2005
*! Purpose   : Procedure to create Temp. File 
*!**************************************************************************
*! Example   : DO lpCreaTemp
*!**************************************************************************
*!
PROCEDURE lpCreaTemp

*-- check If File is created or not
IF USED(lcPackTmp) AND RECCOUNT(lcPackTmp) > 0
  USE IN (lcPackTmp)
  USE IN (lcRevTemp)
ENDIF
*E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start] 
*lcSeleFldPkLn = "&lcTempPack_lin..*,&lcTempPack_Hdr..ACCOUNT,&lcTempPack_Hdr..STORE,&lcTempPack_Hdr..ORDER,&lcTempPack_Hdr..SHIP_DATE,&lcTempPack_Hdr..TOT_CART,&lcTempPack_Hdr..TOT_PCS,&lcTempPack_Hdr..TOT_WGHT"
lcSeleFldPkLn = "&lcTempPack_lin..*,&lcTempPack_Hdr..ACCOUNT,&lcTempPack_Hdr..STORE,&lcTempPack_Hdr..ORDER,&lcTempPack_Hdr..SHIP_DATE,&lcTempPack_Hdr..TOT_CART,&lcTempPack_Hdr..TOT_PCS,&lcTempPack_Hdr..TOT_WGHT,&lcTempPack_Hdr..bill_ladg"
*E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
lcSelCondPkLn = " &lcTempPack_lin..pack_no+STR(&lcTempPack_lin..no_cart,4)+&lcTempPack_lin..style = &lcTempPack_Hdr..pack_no AND .F. "
lcSelTablePkLn = lcTempPack_lin+","+lcTempPack_Hdr

*-- Create File
IF !USED(lcPackTmp)
  IF TYPE("laTempStru[1,1]") $ "UL" 
    DIMENSION laTempStru[1,18]
    SELECT  &lcSeleFldPkLn FROM &lcSelTablePkLn WHERE &lcSelCondPkLn INTO CURSOR &lcTempPikLin READWRITE 

    =lfMakeIndex(lcTempPikLin)

    SELECT(lcTempPikLin)
    =AFIELDS(laTempStru)
    =lfAddField("laTempStru","cCurrCode","C",3,0)
    
  ENDIF

IF llMultCurr AND (lcRpCurr = "F") 
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
  *=gfCrtTmp(lcPackTmp,@laTempStru,"cCurrCode + Pack_No",lcPackTmp,.T.)  
  =gfCrtTmp(lcPackTmp,@laTempStru,IIF(lcRpSort='P',"cCurrCode + Pack_No","cCurrCode +bill_ladg+ STYLE + Pack_No"),lcPackTmp,.T.)
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
  SELECT(lcPackTmp)
  ZAP
  *--Index on Pack Number On reverse Temp.  To get Last Reccount  
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
  IF lcRpSort='P'
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
    INDEX ON Pack_No TAG (lcRevTemp)  DESCENDING
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
  ELSE
    INDEX ON bill_ladg+ STYLE + Pack_No TAG (lcRevTemp)  DESCENDING
  ENDIF
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
ELSE
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
  *=gfCrtTmp(lcPackTmp,@laTempStru," Pack_No",lcPackTmp,.T.)
  =gfCrtTmp(lcPackTmp,@laTempStru,IIF(lcRpSort='P'," Pack_No","bill_ladg+ STYLE + Pack_No"),lcPackTmp,.T.)
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
  SELECT(lcPackTmp)
  ZAP
  *--Index on Pack Number On reverse Temp.  To get Last Reccount  
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
  IF lcRpSort='P'
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
  INDEX ON  Pack_No TAG (lcRevTemp) DESCENDING
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
  ELSE
      INDEX ON bill_ladg+ STYLE + Pack_No TAG (lcRevTemp)  DESCENDING
  ENDIF
  *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
  *-- Index On Temp File  
ENDIF

  DO lpRevOpen
ENDIF


*-- End of lpCreaTemp.

*!**************************************************************************
*! Name      : lpRevOpen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : Function  To Get Reverse Of File In Another Alias
*!**************************************************************************
*! Example   : =lpRevOpen
*!**************************************************************************
*!
FUNCTION lpRevOpen
PRIVATE lcFullPath , lcRevTable , lnActAlias

*-- Save Setting
lnActAlias = SELECT(0)
lcFullPath = SET("FULLPATH")

*-- Get Cursor path [Begin]
SET FULLPATH ON
lcRevTable = FULLPATH(DBF(lcPackTmp))
*-- Get Cursor path [End  ]

*-- Use the table again in reverse order [Begin]
USE (lcRevTable) IN 0 AGAIN ALIAS (lcRevTemp) ORDER (lcRevTemp)
*-- Use the table again in reverse order [End  ]

*-- Restore Setting
SET FULLPATH &lcFullPath
SELECT (lnActAlias)
*-- end of lpRevOpen.

*!**************************************************************************
*! Name      : lpColect
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : Procedure TO Colecte Data
*!**************************************************************************
*! Example   : Do lpColect()
*!**************************************************************************
*!
PROCEDURE lpColect
*--Check if user select Pack_no
lnPosPakNo = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.PACK_NO")
IF lnPosPakNo > 0 
  lnPosPakNo = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPakNo,1)
  lcPakNoFile =IIF(!EMPTY(laOgFxFlt[lnPosPakNo,6]),laOgFxFlt[lnPosPakNo,6],'')
  *B999999,1 mariam 07/11/2005 fix bug of error if no packing list exist[Start]
  *IF !EMPTY(lcPakNoFile) 
  IF !EMPTY(lcPakNoFile) AND USED(lcPakNoFile)
  *B999999,1 mariam 07/11/2005 fix bug of error if no packing list exist[End]
    SELECT(lcPakNoFile)
    LOCATE
    IF !EOF()
      SCAN
        loDBFPack_hdr.Seek(&lcPakNoFile..PACK_NO)
        SELECT(lcTempPack_Hdr)
        SCATTER MEMO MEMVAR 
        INSERT INTO (lcPackPckHdr) FROM MEMVAR 
      ENDSCAN 
      lcSelCond = ""
      lcSelFiles = lcPackPckHdr
      *--Check account 
      lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.ACCOUNT")
      IF lnPosAcc > 0 
        lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
        lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosAcc,6]),laOgFxFlt[lnPosAcc,6],'')
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
        *IF !EMPTY(lcAccSel)
        IF !EMPTY(lcAccSel) AND USED(lcAccSel)
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[End]
          SELECT(lcAccSel) 
          LOCATE
          IF !EOF()
            lcSelCond = lcSelCond+IIF(EMPTY(lcSelCond),""," AND ")+lcPackPckHdr+".Account = "+lcAccSel+".ACCOUNT"
            lcSelFiles =lcSelFiles + IIF(!EMPTY(lcSelFiles),",","") + lcAccSel
          ENDIF
        ENDIF
      ENDIF
      
      *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[Start]
      lnPosBol = ASCAN(loOgScroll.laOgFXFlt,"BOL_HDR.BOL_NO")
  	IF lnPosBol  > 0 
        lnPosBol = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosBol ,1)
        lcBolSel =IIF(!EMPTY(laOgFxFlt[lnPosBol ,6]),laOgFxFlt[lnPosBol ,6],'')
        IF !EMPTY(lcBolSel) AND USED(lcBolSel)
          SELECT(lcBolSel) 
          LOCATE
          IF !EOF()
            lcSelCond = lcSelCond+IIF(EMPTY(lcSelCond),""," AND ")+lcPackPckHdr+".bill_ladg= "+lcBolSel +".BOL_NO"
            lcSelFiles =lcSelFiles + IIF(!EMPTY(lcSelFiles),",","") + lcBolSel 
          ENDIF
        ENDIF
      ENDIF      
	  *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[End]
      
      
      *--Check order 
      lnPosOrder = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.ORDER")
      IF lnPosOrder > 0 
        lnPosOrder = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosOrder,1)
        lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnPosOrder,6]),laOgFxFlt[lnPosOrder,6],'')
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
        *IF !EMPTY(lcOrderSel) 
        IF !EMPTY(lcOrderSel) AND USED(lcOrderSel)
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[End]
          SELECT(lcOrderSel)
          LOCATE
          IF !EOF()
            lcSelCond = lcSelCond+IIF(EMPTY(lcSelCond),""," AND ")+lcPackPckHdr+".order = "+lcOrderSel+".Order"
            lcSelFiles =lcSelFiles + IIF(!EMPTY(lcSelFiles),",","")+ lcOrderSel
          ENDIF
        ENDIF
      ENDIF
      *-- Check the warecode selection 
      lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.CWARECODE")
      IF lnPosWare > 0 
        lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
        lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
        *IF !EMPTY(lcWareSel)
        IF !EMPTY(lcWareSel) AND USED(lcWareSel)
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[End]
          SELECT(lcWareSel)
          LOCATE
          IF !EOF()
           lcSelCond = lcSelCond+IIF(EMPTY(lcSelCond),""," AND ")+lcPackPckHdr+".CWARECODE= "+lcWareSel +".CWARECODE"
           lcSelFiles =lcSelFiles + IIF(!EMPTY(lcSelFiles),",","")+ lcWareSel 
          ENDIF 
        ENDIF 
      ENDIF   
      *--CHECK SHIP DATE
      lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.SHIP_DATE")
      IF lnPosDate > 0 
        lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
        SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
        EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
        IF !EMPTY(EDATE) AND !EMPTY(SDATE)
          lcSelCond = lcSelCond + IIF(EMPTY(lcSelCond),""," AND ")+"BETWEEN("+lcPackPckHdr+".SHIP_DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
        ENDIF 
      ENDIF   
      SELECT(lcPackPckHdr)
      LOCATE 
      IF !EMPTY(lcSelCond)
        SELECT &lcPackPckHdr..* FROM &lcSelFiles WHERE  &lcSelCond  INTO CURSOR &lcFnlPack_hdr
      ELSE 
        SELECT &lcPackPckHdr..* FROM &lcSelFiles   INTO CURSOR &lcFnlPack_hdr
      ENDIF 
      SELECT(lcFnlPack_hdr)
      LOCATE 
      SCAN 
        loDBFPack_lin.SEEK(&lcFnlPack_hdr..PACK_NO)
        SELECT(lcTempPack_lin)
        SCAN REST WHILE PACK_NO+STR(LINE_NO,6)+STYLE+CPACKCOLOR = &lcFnlPack_hdr..PACK_NO
          SCATTER MEMO MEMVAR        
          m.ACCOUNT   = &lcFnlPack_hdr..ACCOUNT
          m.STORE     = &lcFnlPack_hdr..STORE
          m.ORDER     = &lcFnlPack_hdr..ORDER
          m.SHIP_DATE = &lcFnlPack_hdr..SHIP_DATE
          m.TOT_CART  = &lcFnlPack_hdr..TOT_CART 
          m.TOT_PCS   = &lcFnlPack_hdr..TOT_PCS 
          m.TOT_WGHT  = &lcFnlPack_hdr..TOT_WGHT
          *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
          m.bill_ladg= &lcFnlPack_hdr..bill_ladg
          *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
          INSERT INTO (lcTempPikLin) FROM MEMVAR
        ENDSCAN 
      ENDSCAN 
      =lfGetOrdData()
      RETURN 
    ENDIF 
  ENDIF         
ENDIF
*-- if user doesnot select a pack_no
*--Check account 
lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.ACCOUNT")
IF lnPosAcc > 0 
  lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
  lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosAcc,6]),laOgFxFlt[lnPosAcc,6],'')
  *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
  *IF !EMPTY(lcAccSel)
  IF !EMPTY(lcAccSel) AND USED(lcAccSel)
  *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[End]
    SELECT(lcAccSel)
    LOCATE
    IF !EOF()
      loDBFPack_hdr.SETORDER('ACCPACK')
      SCAN
        loDBFPack_hdr.Seek(&lcAccSel..ACCOUNT)
        SELECT(lcTempPack_Hdr)
        SCAN REST WHILE ACCOUNT+PACK_NO  = &lcAccSel..ACCOUNT
          SCATTER MEMO MEMVAR 
          INSERT INTO (lcPackPckHdr) FROM MEMVAR 
        ENDSCAN   
      ENDSCAN 
      loDBFPack_hdr.SETORDER('Pack_hdr')
      lcSelCond = ""
      lcSelFiles = lcPackPckHdr
      *--Check order 
      lnPosOrder = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.ORDER")
      IF lnPosOrder > 0 
        lnPosOrder = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosOrder,1)
        lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnPosOrder,6]),laOgFxFlt[lnPosOrder,6],'')
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
        *IF !EMPTY(lcOrderSel)
        IF !EMPTY(lcOrderSel) AND USED(lcOrderSel)
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[End]
          SELECT(lcOrderSel)
          LOCATE
          IF !EOF()
            lcSelCond = lcSelCond+IIF(EMPTY(lcSelCond),""," AND ")+lcPackPckHdr+".order = "+lcOrderSel+".Order"
            lcSelFiles =lcSelFiles + IIF(!EMPTY(lcSelFiles),",","")+ lcOrderSel
          ENDIF
        ENDIF
      ENDIF
      
      
	  *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[Start]
      lnPosBol = ASCAN(loOgScroll.laOgFXFlt,"BOL_HDR.BOL_NO")
  	IF lnPosBol  > 0 
        lnPosBol = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosBol ,1)
        lcBolSel =IIF(!EMPTY(laOgFxFlt[lnPosBol ,6]),laOgFxFlt[lnPosBol ,6],'')
        IF !EMPTY(lcBolSel) AND USED(lcBolSel)
          SELECT(lcBolSel) 
          LOCATE
          IF !EOF()
            lcSelCond = lcSelCond+IIF(EMPTY(lcSelCond),""," AND ")+lcPackPckHdr+".bill_ladg= "+lcBolSel +".BOL_NO"
            lcSelFiles =lcSelFiles + IIF(!EMPTY(lcSelFiles),",","") + lcBolSel 
          ENDIF
        ENDIF
      ENDIF      
      *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[End]
      
      *-- Check the warecode selection 
      lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.CWARECODE")
      IF lnPosWare > 0 
        lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
        lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
        *IF !EMPTY(lcWareSel)
        IF !EMPTY(lcWareSel) AND USED(lcWareSel)
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
          SELECT(lcWareSel)
          LOCATE
          IF !EOF()
           lcSelCond = lcSelCond+IIF(EMPTY(lcSelCond),""," AND ")+lcPackPckHdr+".CWARECODE= "+lcWareSel +".CWARECODE"
           lcSelFiles =lcSelFiles + IIF(!EMPTY(lcSelFiles),",","")+ lcWareSel 
          ENDIF 
        ENDIF 
      ENDIF   
      *--CHECK SHIP DATE
      lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.SHIP_DATE")
      IF lnPosDate > 0 
        lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
        SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
        EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
        IF !EMPTY(EDATE) AND !EMPTY(SDATE)
          lcSelCond = lcSelCond + IIF(EMPTY(lcSelCond),""," AND ")+"BETWEEN("+lcPackPckHdr+".SHIP_DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
        ENDIF 
      ENDIF   
      SELECT(lcPackPckHdr)
      LOCATE 
      IF !EMPTY(lcSelCond)
        SELECT &lcPackPckHdr..* FROM &lcSelFiles WHERE  &lcSelCond  INTO CURSOR &lcFnlPack_hdr
      ELSE 
        SELECT &lcPackPckHdr..* FROM &lcSelFiles   INTO CURSOR &lcFnlPack_hdr
      ENDIF 
      SELECT(lcFnlPack_hdr)
      LOCATE 
      SCAN 
        loDBFPack_lin.SEEK(&lcFnlPack_hdr..PACK_NO)
        SELECT(lcTempPack_lin)
        SCAN REST WHILE PACK_NO+STR(LINE_NO,6)+STYLE+CPACKCOLOR = &lcFnlPack_hdr..PACK_NO
          SCATTER MEMO MEMVAR        
          m.ACCOUNT   = &lcFnlPack_hdr..ACCOUNT
          m.STORE     = &lcFnlPack_hdr..STORE
          m.ORDER     = &lcFnlPack_hdr..ORDER
          m.SHIP_DATE = &lcFnlPack_hdr..SHIP_DATE
          m.TOT_CART  = &lcFnlPack_hdr..TOT_CART 
          m.TOT_PCS   = &lcFnlPack_hdr..TOT_PCS 
          m.TOT_WGHT  = &lcFnlPack_hdr..TOT_WGHT
          *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
          m.bill_ladg= &lcFnlPack_hdr..bill_ladg
          *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
          
          INSERT INTO (lcTempPikLin) FROM MEMVAR 
        ENDSCAN 
      ENDSCAN 
      =lfGetOrdData()
      RETURN 
    ENDIF
  ENDIF
ENDIF
*-- if user doesnot select a neither pack_no nor account
*--Check order
lnPosOrder = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.ORDER")
IF lnPosOrder > 0 
  lnPosOrder = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosOrder,1)
  lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnPosOrder,6]),laOgFxFlt[lnPosOrder,6],'')
  *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
  *IF !EMPTY(lcOrderSel)
  IF !EMPTY(lcOrderSel) AND USED(lcOrderSel)
  *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
    SELECT(lcOrderSel)
    LOCATE
    IF !EOF()
      loDBFPack_hdr.SETORDER('ORDERPCK')    
      SCAN
        loDBFPack_hdr.Seek(&lcOrderSel..ORDER)
        SELECT(lcTempPack_Hdr)
        SCAN REST WHILE ORDER+STORE+PACK_NO = &lcOrderSel..ORDER 
          SCATTER MEMO MEMVAR 
          INSERT INTO (lcPackPckHdr) FROM MEMVAR 
        ENDSCAN 
      ENDSCAN 
      loDBFPack_hdr.SETORDER('PACK_HDR')    
      lcSelCond = ""
      lcSelFiles = lcPackPckHdr
      *-- Check the warecode selection 
      lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.CWARECODE")
      IF lnPosWare > 0 
        lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
        lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
        *IF !EMPTY(lcWareSel)
        IF !EMPTY(lcWareSel) AND USED(lcWareSel)
        *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[End]
          SELECT(lcWareSel)
          LOCATE
          IF !EOF()
           lcSelCond = lcSelCond+IIF(EMPTY(lcSelCond),""," AND ")+lcPackPckHdr+".CWARECODE= "+lcWareSel +".CWARECODE"
           lcSelFiles =lcSelFiles + IIF(!EMPTY(lcSelFiles),",","")+ lcWareSel 
          ENDIF 
        ENDIF 
      ENDIF 
      
      *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[Start]
      lnPosBol = ASCAN(loOgScroll.laOgFXFlt,"BOL_HDR.BOL_NO")
  	IF lnPosBol  > 0 
        lnPosBol = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosBol ,1)
        lcBolSel =IIF(!EMPTY(laOgFxFlt[lnPosBol ,6]),laOgFxFlt[lnPosBol ,6],'')
        IF !EMPTY(lcBolSel) AND USED(lcBolSel)
          SELECT(lcBolSel) 
          LOCATE
          IF !EOF()
            lcSelCond = lcSelCond+IIF(EMPTY(lcSelCond),""," AND ")+lcPackPckHdr+".bill_ladg= "+lcBolSel +".BOL_NO"
            lcSelFiles =lcSelFiles + IIF(!EMPTY(lcSelFiles),",","") + lcBolSel 
          ENDIF
        ENDIF
      ENDIF      
	  *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[End]
        
      *--CHECK SHIP DATE
      lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.SHIP_DATE")
      IF lnPosDate > 0 
        lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
        SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
        EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
        IF !EMPTY(EDATE) AND !EMPTY(SDATE)
          lcSelCond = lcSelCond + IIF(EMPTY(lcSelCond),""," AND ")+"BETWEEN("+lcPackPckHdr+".SHIP_DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
        ENDIF 
      ENDIF   
      SELECT(lcPackPckHdr)
      LOCATE 
      IF !EMPTY(lcSelCond)
        SELECT &lcPackPckHdr..* FROM &lcSelFiles WHERE  &lcSelCond  INTO CURSOR &lcFnlPack_hdr
      ELSE 
        SELECT &lcPackPckHdr..* FROM &lcSelFiles   INTO CURSOR &lcFnlPack_hdr
      ENDIF 
      SELECT(lcFnlPack_hdr)
      LOCATE 
      SCAN 
        loDBFPack_lin.SEEK(&lcFnlPack_hdr..PACK_NO)
        SELECT(lcTempPack_lin)
        SCAN REST WHILE PACK_NO+STR(LINE_NO,6)+STYLE+CPACKCOLOR = &lcFnlPack_hdr..PACK_NO
          SCATTER MEMO MEMVAR        
          m.ACCOUNT   = &lcFnlPack_hdr..ACCOUNT
          m.STORE     = &lcFnlPack_hdr..STORE
          m.ORDER     = &lcFnlPack_hdr..ORDER
          m.SHIP_DATE = &lcFnlPack_hdr..SHIP_DATE
          m.TOT_CART  = &lcFnlPack_hdr..TOT_CART 
          m.TOT_PCS   = &lcFnlPack_hdr..TOT_PCS 
          m.TOT_WGHT  = &lcFnlPack_hdr..TOT_WGHT
          *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
          m.bill_ladg= &lcFnlPack_hdr..bill_ladg
          *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
          
          INSERT INTO (lcTempPikLin) FROM MEMVAR
        ENDSCAN 
      ENDSCAN 
      =lfGetOrdData()
      RETURN 
    ENDIF
   ENDIF 
ENDIF

*-- if user did not select any data or even select only the location filter
IF loDBFPack_hdr.llNative AND loDBFPack_lin.llNative
*------------------------------------------------------------------------------------------------
  lcSelCondPkLn = " "
  lcSeleFldPkLn = "PACK_LIN.*,PACK_HDR.ACCOUNT,PACK_HDR.STORE,PACK_HDR.ORDER,PACK_HDR.SHIP_DATE,PACK_HDR.TOT_CART,PACK_HDR.TOT_PCS,PACK_HDR.TOT_WGHT"
  *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[Start]
  *lcSelTablePkLn = "PACK_HDR,PACK_LIN"
  lcSelTablePkLn = "PACK_HDR"
  *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[end]
  lcPack_hdrcond = ""
  lcPack_File = "Pack_hdr"
  *--
  lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.CWARECODE")
  IF lnPosWare > 0 
    lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
    lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
    *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
    *IF !EMPTY(lcWareSel)
    IF !EMPTY(lcWareSel) AND USED(lcWareSel)
    *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[End]
      SELECT(lcWareSel)
      LOCATE
      IF !EOF()
        lcSelCondPkLn = lcSelCondPkLn +IIF(EMPTY(lcSelCondPkLn),""," AND ")+"PACK_HDR.CWARECODE= "+lcWareSel +".CWARECODE"
        lcSelTablePkLn =lcSelTablePkLn + IIF(!EMPTY(lcSelTablePkLn),",","")+ lcWareSel
      ENDIF 
    ENDIF 
  ENDIF   
  *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[Start]
  lnPosBol = ASCAN(loOgScroll.laOgFXFlt,"BOL_HDR.BOL_NO")
  IF lnPosBol  > 0 
    lnPosBol = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosBol ,1)
    lcBolSel =IIF(!EMPTY(laOgFxFlt[lnPosBol ,6]),laOgFxFlt[lnPosBol ,6],'')
    IF !EMPTY(lcBolSel) AND USED(lcBolSel)
      SELECT(lcBolSel) 
      LOCATE
      IF !EOF()
        lcSelCondPkLn = lcSelCondPkLn +IIF(EMPTY(lcSelCondPkLn),""," AND ")+"PACK_HDR.bill_ladg= "+lcBolSel +".BOL_NO"
        lcSelTablePkLn =lcSelTablePkLn + IIF(!EMPTY(lcSelTablePkLn ),",","") + lcBolSel 
      ENDIF
    ENDIF
  ENDIF      
  *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[End]
   *--CHECK SHIP DATE
   lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.SHIP_DATE")
   IF lnPosDate > 0 
     lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
     SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
     EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
     IF !EMPTY(EDATE) AND !EMPTY(SDATE)
        lcSelCondPkLn = lcSelCondPkLn + IIF(EMPTY(lcSelCondPkLn)," "," AND ")+" BETWEEN(PacK_hdr.SHIP_DATE,CTOD(SDATE),CTOD(EDATE))"
     ENDIF 
   ENDIF   
   IF !EMPTY(lcSelCondPkLn)
     *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
     *lcPackHdrFlds="PACK_HDR.PACK_NO,PACK_HDR.ACCOUNT,PACK_HDR.STORE,PACK_HDR.ORDER,PACK_HDR.SHIP_DATE,PACK_HDR.TOT_CART,PACK_HDR.TOT_PCS,PACK_HDR.TOT_WGHT"
     lcPackHdrFlds="PACK_HDR.PACK_NO,PACK_HDR.ACCOUNT,PACK_HDR.STORE,PACK_HDR.ORDER,PACK_HDR.SHIP_DATE,PACK_HDR.TOT_CART,PACK_HDR.TOT_PCS,PACK_HDR.TOT_WGHT,PACK_HDR.bill_ladg"
     *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
     *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[Start]
*     lcPackHdrFils="Pack_hdr"
     lcPackHdrFils=lcSelTablePkLn 
	 *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[End]
*     lcPackHdrLine = loogscroll.gfTempName()
     SELECT &lcPackHdrFlds FROM &lcPackHdrFils WHERE &lcSelCondPkLn INTO CURSOR &lcFnlPack_hdr
     SELECT(lcFnlPack_hdr)
     LOCATE 
     SCAN 
       IF loDBFPack_lin.SEEK(&lcFnlPack_hdr..PACK_NO)
         SELECT(lcTempPack_lin)
         SCAN REST WHILE PACK_NO+STR(NO_CART,4)+PACK_ID+CPKCOLOR+CPCKSIZE+STYLE = &lcFnlPack_hdr..PACK_NO
*         SCAN REST WHILE PACK_NO+STR(LINE_NO,6)+STYLE+CPACKCOLOR = &lcFnlPack_hdr..PACK_NO
           SCATTER MEMO MEMVAR        
           m.ACCOUNT   = &lcFnlPack_hdr..ACCOUNT
           m.STORE     = &lcFnlPack_hdr..STORE
           m.ORDER     = &lcFnlPack_hdr..ORDER
           m.SHIP_DATE = &lcFnlPack_hdr..SHIP_DATE
           m.TOT_CART  = &lcFnlPack_hdr..TOT_CART 
           m.TOT_PCS   = &lcFnlPack_hdr..TOT_PCS 
           m.TOT_WGHT  = &lcFnlPack_hdr..TOT_WGHT
           *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
           m.bill_ladg= &lcFnlPack_hdr..bill_ladg
           *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
           
           INSERT INTO (lcTempPikLin) FROM MEMVAR 
         ENDSCAN   
        ENDIF 
     ENDSCAN 
*!*       lcSelCondPkLn = "PACK_LIN.pack_no+STR(PACK_LIN.no_cart,4)+PACK_LIN.style = PACK_HDR.pack_no and "+lcSelCondPkLn
*!*       SELECT &lcSeleFldPkLn FROM &lcSelTablePkLn WHERE &lcSelCondPkLn INTO CURSOR &lcFnlPack_hdr 
     SELECT(lcTempPikLin)
     LOCATE 
     SET RELATION TO  "O" + ORDER INTO &lcTempOrdhd
     SCAN 
       SCATTER MEMO MEMVAR
       m.cCurrCode = &lcTempOrdhd..cCurrCode
       INSERT INTO (lcPackTmp) FROM MEMVAR
     ENDSCAN 
     
   ELSE 
     SELECT(lcTempPack_Hdr)
     LOCATE 
     SET RELATION TO  "O" + ORDER INTO &lcTempOrdhd
     SCAN 
       *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
       *SCATTER FIELDS ACCOUNT,STORE,ORDER,SHIP_DATE,TOT_CART,TOT_PCS,TOT_WGHT MEMO MEMVAR        
       SCATTER FIELDS ACCOUNT,STORE,ORDER,SHIP_DATE,TOT_CART,TOT_PCS,TOT_WGHT,bill_ladg   MEMO MEMVAR 
       *E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]
       IF SEEK(pack_no,lcTempPack_lin)
         SELECT(lcTempPack_lin)
         SCAN REST WHILE PACK_NO+STR(LINE_NO,6)+STYLE+CPACKCOLOR = &lcTempPack_Hdr..pack_no
           SCATTER MEMVAR MEMO
           m.cCurrCode = &lcTempOrdhd..cCurrCode
           INSERT INTO (lcPackTmp) FROM MEMVAR
         ENDSCAN
       ENDIF
     ENDSCAN
   ENDIF      
   lcTempOrdLn = lcTempOrdLine 
   lcTempCustomer =lcTempCust
   lcTempOrdHdr =lcTempOrdHd 
   SET RELATION TO  
ELSE 
  lcSelCondPkLn = "PACK_LIN.pack_no+STR(PACK_LIN.no_cart,4)+PACK_LIN.style = PACK_HDR.pack_no"
  lcSeleFldPkLn = "PACK_LIN.*,PACK_HDR.ACCOUNT,PACK_HDR.STORE,PACK_HDR.ORDER,PACK_HDR.SHIP_DATE,PACK_HDR.TOT_CART,PACK_HDR.TOT_PCS,PACK_HDR.TOT_WGHT"
  lcSelTablePkLn = "PACK_HDR,PACK_LIN"
  lcPack_hdrcond = ""
  lcPack_File = "Pack_hdr"
  lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.CWARECODE")
  IF lnPosWare > 0 
    lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
    lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
    *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[Start]
    *IF !EMPTY(lcWareSel)
    IF !EMPTY(lcWareSel) AND USED(lcWareSel)
    *B999999,1 mmt 07/11/2005 fix bug of error if no data in browse[End]
      SELECT(lcWareSel)
      LOCATE
      IF !EOF()
        lcCurName = lcWareSel
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
           IF (RECCOUNT() > 0) 
             lcSQLWare = loOgScroll.gfSQLTempName('','Cwarecode C(10)',lcCurName,'Cwarecode')
             lcSelCondPkLn = lcSelCondPkLn +IIF(EMPTY(lcSelCondPkLn),""," AND ")+"PACK_HDR.CWARECODE= "+lcSQLWare +".CWARECODE"
             lcPack_File = "Pack_hdr"+","+lcSQLWare
             lcPack_hdrcond = lcPack_hdrcond + IIF(EMPTY(lcPack_hdrcond),""," AND ")+"PACK_HDR.CWARECODE= "+lcSQLWare +".CWARECODE"
             lcSelTablePkLn =lcSelTablePkLn + IIF(!EMPTY(lcSelFiles),",","")+ lcSQLWare 
           ENDIF 
         ENDIF 
       ENDIF 
     ENDIF 
   ENDIF   
   
   *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[Start]
   lnPosBol = ASCAN(loOgScroll.laOgFXFlt,"BOL_HDR.BOL_NO")
   IF lnPosBol  > 0 
     lnPosBol = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosBol ,1)
     lcBolSel =IIF(!EMPTY(laOgFxFlt[lnPosBol ,6]),laOgFxFlt[lnPosBol ,6],'')
    IF !EMPTY(lcBolSel) AND USED(lcBolSel)
      SELECT(lcBolSel) 
      LOCATE
      IF !EOF()
        lcCurName = lcBolSel 
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
           IF (RECCOUNT() > 0) 
             lcSQLWare = loOgScroll.gfSQLTempName('','BOL_NO C(6)',lcCurName,'BOL_NO')
             lcSelCondPkLn = lcSelCondPkLn +IIF(EMPTY(lcSelCondPkLn),""," AND ")+"PACK_HDR.bill_ladg= "+lcSQLWare +".BOL_NO "
             lcPack_File = "Pack_hdr"+","+lcSQLWare
             lcPack_hdrcond = lcPack_hdrcond + IIF(EMPTY(lcPack_hdrcond),""," AND ")+"PACK_HDR.bill_ladg= "+lcSQLWare +".BOL_NO "
             lcSelTablePkLn =lcSelTablePkLn + IIF(!EMPTY(lcSelFiles),",","")+ lcSQLWare 
           ENDIF 
         ENDIF 
      ENDIF
    ENDIF
  ENDIF      
  *E302793,1 MMT 11/07/2010 add BOL filter to Packing list log report[End]      
  
   *--CHECK SHIP DATE
   lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.SHIP_DATE")
   IF lnPosDate > 0 
     lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
     SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
     EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
     IF !EMPTY(EDATE) AND !EMPTY(SDATE)
        lcSelCondPkLn = lcSelCondPkLn + IIF(EMPTY(lcSelCondPkLn),""," AND  PacK_hdr.SHIP_DATE BETWEEN "+CTOD(SDATE)+" and "+CTOD(EDATE))
        lcPack_hdrcond  = lcPack_hdrcond  +IIF(EMPTY(lcPack_hdrcond),""," AND  PacK_hdr.SHIP_DATE BETWEEN "+CTOD(SDATE)+" and "+CTOD(EDATE))
     ENDIF 
   ENDIF   

   =lfOpenSql(lcSeleFldPkLn ,lcSelTablePkLn ,lcTempPikLin,lcSelCondPkLn)
*  SELECT &lcSeleFldPkLn FROM &lcSelTablePkLn WHERE &lcSelCondPkLn INTO CURSOR lcTempPikLin

  lcSelFldOrd = "Ordhdr.order,Ordhdr.multipo,Ordhdr.cordtype,Ordhdr.custpo,Ordhdr.cCurrCode,Nexrate,NcurrUnit"
  lcSelTable = "ordhdr"+","+lcPack_File
  lcWhrCondOrdhd = lcPack_hdrcond +IIF(!EMPTY(lcPack_hdrcond),"AND ","")+"ordhdr.cordtype+ordhdr.order ='O' +pack_hdr.order"

  =lfOpenSql(lcSelFldOrd ,lcSelTable ,lcTempordhdr,lcWhrCondOrdhd )
*   SELECT &lcSelFldOrd  FROM  &lcSelTable  WHERE &lcWhrCondOrdhd INTO &lcTempordhdr

  lcSelFldOrdLn = "Ordline.Cordtype,Ordline.custpo,Ordline.price,Ordline.totqty,Ordline.order,Ordline.lineno"
  lcSelTableLn = "Ordline,PACK_LIN" +","+lcPack_File
  lcWhrCondOrdLn =lcSelCondPkLn+IIF(EMPTY(lcSelCondPkLn),""," AND ")+" Ordline.cordtype+Ordline.order+STR(Ordline.lineno,6) ='O' +PACK_HDR.order+STR(PACK_LIN.nordlineno,6)"
  =lfOpenSql(lcSelFldOrdLn ,lcSelTableLn ,lcTempordLn,lcWhrCondOrdLn )
*   SELECT &lcSelFldOrdLn FROM  &lcSelTableLn WHERE &lcWhrCondOrdLn INTO &lcTempordLn
   
  lcSelFldcust= "CUSTOMER.Type,CUSTOMER.Account,CUSTOMER.Dist_ctr,CUSTOMER.btName,CUSTOMER.store"
  lcSelTableCust = "CUSTOMER"+","+lcPack_File
  lcWhrCondCust = lcPack_hdrcond +IIF(EMPTY(lcPack_hdrcond),""," AND ")+"CUSTOMER.type+CUSTOMER.account+CUSTOMER.store =IIF(EMPTY("+lcTemppk+".STORE),'M','S')+PACK_HDR.ACCOUNT+PACK_HDR.STORE"

  =lfOpenSql(lcSelFldcust ,lcSelTableCust ,lcTempCustomer,lcWhrCondCust)
*  SELECT &lcSelFldcust FROM  &lcSelTableCust WHERE &lcWhrCondCust INTO &lcTempCustomer
  SELECT(lcTempPikLin)
  LOCATE 
  SET RELATION TO  "O" + ORDER INTO &lcTempordhdr
  SCAN 
    SCATTER MEMVAR MEMO
    m.cCurrCode = &lcTempordhdr..cCurrCode
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN
  SET RELATION TO  
ENDIF 
*-- end of lpColect.

*!**************************************************************************
*! Name      : lfLastPage
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : Fuction to Get Last Page (Called from .FRX summary band)
*!**************************************************************************
*! Example   : =lfLastPage
*!**************************************************************************
*!
FUNCTION lfLastPage
llLastPage = .T.
RETURN ""
*-- end of lfLastPage.

*!*************************************************************************
*! Name      : lfClearRep
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Called from : Option Grid
*!*************************************************************************
*! Example   : =lfClearRep()
*!*************************************************************************
*
FUNCTION lfClearRep
llOGFltCh = .T.
*-- Delete temporary  REP. file.
IF USED(lcPackTmp)
 USE IN (lcPackTmp)
 USE IN (lcRevTemp)
ENDIF
=lfSpcClear()

IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

   IF llOpenCurr AND USED("SYCCURR")
     USE IN SYCCURR
   ENDIF

  
ENDIF
*-- end of lfClearRep.
*!**************************************************************************
*! Name      : lfNotRep
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : Fuction to Get Not repeated pk# ,order,...
*!**************************************************************************
*! Example   : =lfNotRep
*!**************************************************************************
*!
FUNCTION lfNotRep

llNotRep = .T.
RETURN ""
*-- end of lfNotRep.
*!**************************************************************************
*! Name      : lfNNotRep
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : Fuction to Get Next Not repeated pk# ,order,...
*!**************************************************************************
*! Example   : =lfNotRep
*!**************************************************************************
*!
FUNCTION lfNNotRep

llNotRep = .F.
RETURN ""
*-- end of lfNotRep.

*!*************************************************************
*! Name      : lfFillVars
*: Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
FUNCTION lfFillVars

IF llMultCurr
  
  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(oAriaApplication.SysPath +'SYCCURR',oAriaApplication.SysPath +'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF

*-- End Of lfFillVars.



*!**************************************************************************
*! Name      : lfAddField
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : Add fields to the the array of file structer
*!**************************************************************************
*! Example   : =lfAddField()
*!**************************************************************************
*!
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec
lnFldPos  = ALEN(&lcStruArry,1) + 1
DIMENSION &lcStruArry[lnFldPos , 18]
&lcStruArry[lnFldPos , 1]  = lcFldName
&lcStruArry[lnFldPos , 2]  = lcFldType
&lcStruArry[lnFldPos , 3]  = lnFldLen
&lcStruArry[lnFldPos , 4]  = lnFldDec
STORE ' ' TO  &lcStruArry[lnFldPos,7],&lcStruArry[lnFldPos,8],;
              &lcStruArry[lnFldPos,9],&lcStruArry[lnFldPos,10],;
              &lcStruArry[lnFldPos,11],&lcStruArry[lnFldPos,12],;
              &lcStruArry[lnFldPos,13],&lcStruArry[lnFldPos,14],;
              &lcStruArry[lnFldPos,15],&lcStruArry[lnFldPos,16]
STORE 0 TO    &lcStruArry[lnFldPos,17] ,&lcStruArry[lnFldPos,18]

*-- end of lfAddField.


*!*************************************************************
*! Name      : lfGtCurDes
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : Function Get the Description ofthe Currency
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGtCurDes()
*!*************************************************************
FUNCTION lfGtCurDes
IF .F.
  IF SEEK(cCurrCode,'SYCCURR')
    RETURN SYCCURR.cCurrDesc
  ENDIF
  RETURN ""
ENDIF
*-- end of lfGtCurDes.
*!*************************************************************
*! Name      : lfvFormat
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : Function Get the Description ofthe Currency
*!*************************************************************
FUNCTION lfvFormat
IF (llMultCurr AND (lcRpCurr = 'F') AND lcRpFormat <> 'S')
  lcRepGrp = [&lcTempOrdHdr..cCurrCode]
ELSE
  lcRepGrp = [""]
ENDIF
*-- end of lfvFormat.

*!*************************************************************
*! Name      : lfvRepCurr
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : 
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRepCurr()
*!*************************************************************
FUNCTION lfvRepCurr

llOGFltCh = .T.
=gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm

SELECT Customer
SET ORDER TO Customer
LOCATE
*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfSpcClear
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/05
*! Purpose   : Close a special case tables
*!*************************************************************
*! Called from : Any Picket Ticket form
*!*************************************************************
*
FUNCTION lfSpcClear
IF TYPE("laSpcFiles[1]") $ "UL"
  RETURN
ENDIF

IF !EMPTY(laSpcFiles[1])
  PRIVATE lnI
  lnI = 0
  FOR lnI = 1 TO ALEN(laSpcFiles,1)
    IF USED(laSpcFiles[lnI])
      USE IN (laSpcFiles[lnI])
    ENDIF
  ENDFOR
ENDIF
*-- end of lfSpcClear.
*!**************************************************************************
*! Name      : lfCollTime
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 01/03/05
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!**************************************************************************
*! Returns            : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- END OF lfCollTime.
*!*************************************************************
*! Name      : lfOpenFox
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/03/05
*! Purpose   : function to open FOX tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenFox

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

IF TYPE('loSqlConnection') <> 'O'
  loSqlConnection = CREATEOBJECT('remotedataaccess')
ENDIF 
lnConnectionHandlar = loSqlConnection.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.cAriaNativeDataFilesConStr,3,;
                                      'SAVE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  *lnResult = loSqlConnection.mCloneStandardCursor (lcCursor, ;
                       SET("DATASESSION"))

  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)
  =CURSORSETPROP("Buffering",5,lcCursor)
ELSE
  =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.
*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/03/05
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
  DO CASE

   *--temp. Customer File
   CASE UPPER(lcTable) = lcTempCustomer
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'TYPE+ACCOUNT+STORE'
     laIndex[1,2] = lcTempCustomer

   *--temp. ordhdr file
   CASE UPPER(lcTable) =  lcTempOrdHdr
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'CORDTYPE+ORDER'
     laIndex[1,2] = lcTempOrdHdr

   *--temp. ordline file
   CASE UPPER(lcTable) = lcTempordLn  
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'cOrdType+Order+STR(lineno,6)'
      laIndex[1,2] = lcTempordLn

   *--temp. pack_lin file
   CASE UPPER(lcTable) =  lcTempPikLin
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'pack_no+STR(line_no,6)+style+cpackcolor'
      laIndex[1,2] = lcTempPikLin

  CASE UPPER(lcTable) =  lcPackPckHdr
      DIMENSION laIndex[4,2]
      laIndex[1,1] = 'PACK_NO'
      laIndex[1,2] = lcPackPckHdr
     
      laIndex[2,1] = 'ORDER'
      laIndex[2,2] = lcIndOrder
      
      laIndex[3,1] = 'ACCOUNT'
      laIndex[3,2] = lcIndAccount
    
      laIndex[4,1] = 'CWARECODE'
      laIndex[4,2] = lcIndWare

   
ENDCASE
*!*************************************************************
*! Name      : lfGetOrdData
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/03/05
*! Purpose   : function to get the order header and line date
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetOrdData

*-- Get the ORDHDR file
SELECT(lcFnlPack_hdr)
LOCATE 
SCAN 
  loDBFOrdhdr.SEEK('O'+&lcFnlPack_hdr..ORDER)
  SELECT(lcTempOrdhd)
  SCATTER MEMO MEMVAR 
  SELECT(lcTempordhdr)
  APPEND BLANK 
  GATHER MEMO MEMVAR 
ENDSCAN 
*--get the customer file 
SELECT(lcFnlPack_hdr)
LOCATE 
SCAN 
  IF EMPTY(&lcFnlPack_hdr..Store)
    loDBFCust.SEEK('M'+&lcFnlPack_hdr..ACCOUNT+&lcFnlPack_hdr..STORE)
  ELSE
    loDBFCust.SEEK('S'+&lcFnlPack_hdr..ACCOUNT+&lcFnlPack_hdr..STORE)
  ENDIF 
  SELECT(lcTempCust)
  SCATTER MEMO MEMVAR 
  SELECT(lcTempCustomer)
  APPEND BLANK 
  GATHER MEMO MEMVAR 
ENDSCAN 

SELECT(lcTempPikLin)
LOCATE 
SCAN 
*lcTempOrdLine,
  loDBFOrdLine.SEEK('O'+&lcTempPikLin..ORDER+STR(&lcTempPikLin..NORDLINENO,6 ))
  SELECT(lcTempOrdLine)
  SCATTER MEMO MEMVAR 
  SELECT(lcTempordLn)
  APPEND BLAN
  GATHER MEMO MEMVAR   
ENDSCAN 

SELECT(lcTempPikLin)
LOCATE 
SET RELATION TO  "O" + ORDER INTO &lcTempordhdr
SCAN 
  SCATTER MEMVAR MEMO
  m.cCurrCode = &lcTempordhdr..cCurrCode
  m.SHIP_DATE = IIF(&lcTempPikLin..SHIP_DATE = CTOD('12/30/1899'),{//},&lcTempPikLin..SHIP_DATE)
  INSERT INTO (lcPackTmp) FROM MEMVAR
ENDSCAN
SET RELATION TO  

*!*************************************************************
*! Name      : lfMakeIndex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/03/05
*! Purpose   : function to make index on a temp. file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMakeIndex
PARAMETERS lcTempName
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcCursor = lcTempName
lnBuffering = CURSORGETPROP("Buffering",lcCursor)
 =CURSORSETPROP("Buffering",3,lcCursor)
*-- To initialize the indecis that will be created for each file
=lfCrtindex(lcCursor)
SELECT (lcCursor)
FOR lnI = 1 TO ALEN(laIndex,1)
  lcIndex = laIndex[lnI,1]
  lcTag   = laIndex[lnI,2]
  INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
ENDFOR
lcTag = laIndex[1,2]
SET ORDER TO TAG (lcTag)
*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/08/2004
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenSql

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.
*!*************************************************************
*! Name      : lfRepWhen
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/23/2005
*! Purpose   : report When function
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfRepWhen

*--Pack_hdr file 
loDBFPack_hdr = CreateObject("RemoteTable","Pack_hdr","Pack_hdr",lcTempPack_Hdr,SET("DATASESSION"))

*--Pack_lin file 
loDBFPack_lin = CreateObject("RemoteTable","Pack_lin","Pack_lin",lcTempPack_lin,SET("DATASESSION"))  

*--Ordhdr file 
loDBFOrdhdr   = CreateObject("RemoteTable","Ordhdr","Ordhdr",lcTempOrdhd,SET("DATASESSION"))

*--Ordline file 
loDBFOrdLine  = CreateObject("RemoteTable","OrdLine","OrdLine",lcTempOrdLine,SET("DATASESSION"))

*--Customer file 
loDBFCust     = CreateObject("RemoteTable","Customer","Customer",lcTempCust,SET("DATASESSION"))


*E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[Start]
*!*************************************************************
*! Name      : lfGetBolCrt
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/31/2011
*! Purpose   : function to get total Cartons/Bol
*!*************************************************************
FUNCTION lfGetBolCrt
LPARAMETERS lcBolNum,lcCurr
IF lcRpSort<> 'B'
  RETURN 0
ENDIF
SELECT (lcPackTmp)
lnTotCrtn = 0
lnOldRec = RECNO()
DIMENSION laPackArr[1]
STORE '' TO laPackArr
IF llMultCurr AND (lcRpCurr = "F")
  =SEEK(lcCurr+lcBolNum)
  SCAN REST WHILE cCurrCode +bill_ladg+ STYLE + Pack_No = lcCurr+lcBolNum
    IF ASCAN(laPackArr,&lcPackTmp..Pack_No) = 0
      lnTotCrtn = lnTotCrtn + &lcPackTmp..Tot_CART 
      IF !EMPTY(laPackArr[1])
        DIMENSION laPackArr[ALEN(laPackArr,1)+1]
        laPackArr[ALEN(laPackArr,1)] = &lcPackTmp..Pack_No
      ELSE  
        laPackArr[1] = &lcPackTmp..Pack_No
      ENDIF  
    ENDIF
  ENDSCAN   
ELSE
  =SEEK(lcBolNum)
  SCAN REST WHILE bill_ladg+ STYLE + Pack_No = lcBolNum
    IF ASCAN(laPackArr,&lcPackTmp..Pack_No) = 0
      lnTotCrtn = lnTotCrtn + &lcPackTmp..Tot_CART 
      IF !EMPTY(laPackArr[1])
        DIMENSION laPackArr[ALEN(laPackArr,1)+1]
        laPackArr[ALEN(laPackArr,1)] = &lcPackTmp..Pack_No
      ELSE  
        laPackArr[1] = &lcPackTmp..Pack_No
      ENDIF  
    ENDIF
  ENDSCAN       
ENDIF
SELECT (lcPackTmp)
IF BETWEEN(lnOldRec ,1,RECCOUNT())
  GO RECORD lnOldRec 
ENDIF
RETURN lnTotCrtn
*!*************************************************************
*! Name      : lfGetCurCrt
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/31/2011
*! Purpose   : function to get total Cartons/Currecny
*!*************************************************************
FUNCTION lfGetCurCrt
LPARAMETERS lcCurr
IF lcRpSort<> 'B' 
  RETURN 0
ENDIF
SELECT (lcPackTmp)
lnTotCrtn = 0
lnOldRec = RECNO()
DIMENSION laPackArr[1]
STORE '' TO laPackArr
IF llMultCurr AND (lcRpCurr = "F")
  =SEEK(lcCurr)
  SCAN REST WHILE cCurrCode +bill_ladg+ STYLE + Pack_No = lcCurr
    IF ASCAN(laPackArr,&lcPackTmp..Pack_No) = 0
      lnTotCrtn = lnTotCrtn + &lcPackTmp..Tot_CART 
      IF !EMPTY(laPackArr[1])
        DIMENSION laPackArr[ALEN(laPackArr,1)+1]
        laPackArr[ALEN(laPackArr,1)] = &lcPackTmp..Pack_No
      ELSE  
        laPackArr[1] = &lcPackTmp..Pack_No
      ENDIF  
    ENDIF
  ENDSCAN   
ELSE
  SCAN 
    IF ASCAN(laPackArr,&lcPackTmp..Pack_No) = 0
      lnTotCrtn = lnTotCrtn + &lcPackTmp..Tot_CART 
      IF !EMPTY(laPackArr[1])
        DIMENSION laPackArr[ALEN(laPackArr,1)+1]
        laPackArr[ALEN(laPackArr,1)] = &lcPackTmp..Pack_No
      ELSE  
        laPackArr[1] = &lcPackTmp..Pack_No
      ENDIF  
    ENDIF
  ENDSCAN       
ENDIF
SELECT (lcPackTmp)
IF BETWEEN(lnOldRec ,1,RECCOUNT())
  GO RECORD lnOldRec 
ENDIF
RETURN lnTotCrtn
*!*************************************************************
*! Name      : lfGettotCrt
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/31/2011
*! Purpose   : function to get total Cartons
*!*************************************************************
FUNCTION lfGettotCrt
IF lcRpSort<> 'B' 
  RETURN 0
ENDIF
SELECT (lcPackTmp)
lnTotCrtn = 0
lnOldRec = RECNO()
DIMENSION laPackArr[1]
STORE '' TO laPackArr
SCAN 
  IF ASCAN(laPackArr,&lcPackTmp..Pack_No) = 0
    lnTotCrtn = lnTotCrtn + &lcPackTmp..Tot_CART 
    IF !EMPTY(laPackArr[1])
      DIMENSION laPackArr[ALEN(laPackArr,1)+1]
      laPackArr[ALEN(laPackArr,1)] = &lcPackTmp..Pack_No
    ELSE  
      laPackArr[1] = &lcPackTmp..Pack_No
    ENDIF  
  ENDIF
ENDSCAN       
SELECT (lcPackTmp)
IF BETWEEN(lnOldRec ,1,RECCOUNT())
  GO RECORD lnOldRec 
ENDIF
RETURN lnTotCrtn
*E302884,1 MMT 03/31/2011 Add Sort by Option to Sort by BOL[End]