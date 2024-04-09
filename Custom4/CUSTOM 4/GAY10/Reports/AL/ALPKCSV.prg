*!*****************************************************************************************
*! Name      : ALPKCSV.PRG
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 02/16/2005 
*! Purpose   : Export Picking Ticket to CSV
*! Entry no. : C201507- {T20120523.0025}
*!*****************************************************************************************
*! Modification
*!*****************************************************************************************
IF EMPTY(lcRpPath) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File Name or Path")
  RETURN .F.
ENDIF 
IF !EMPTY(lcRpPath) 
  lcDir = JUSTPATH(lcRpPath)
  IF !DIRECTORY(lcDir)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File Path")
    RETURN .F.
  ENDIF   
ENDIF 
SAVE TO oariaApplication.DataDir+'ExportPkTk'+'.MEM' ALL LIKE lcRpPath*


PRIVATE lnCount,lnOldpLen
llPrinter=.F.
lcCurChr = ALLTRIM(SET('CURRENCY' , 1))        && Variable to hold the currency symbol
llCurLeft = (SET('CURRENCY') = 'LEFT')         && Flag to know if the currency symbol is left
lcSolTName = ''        && Variable to hold the Sold to name
lcShpTName = ''        && Variable to hold the Ship to name
lcShipVia = ''         && Variable to hold the Ship Via Description
lcSeason = ''          && Variable to hold the Season Description
lcSpcInst = ''         && Variable to hold the Special Instructions Description
lcTerms = ''           && Variable to hold the Terms Description
lcDivLName = ''        && Variable to hold the Division long name
llAlpktk = .T.            && llAlpktk it will be a commen variable.

*--THE COLOR LENGTH
STORE 0 TO lnClrLnM1 , lnClrPosM1
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnM1  = LEN(laItemSeg[lnCount,3])
    lnClrPosM1 = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
  DECLARE laCompAdd[5,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]

  laCompAdd = ''          && Array to hold the Company address
  laSoldTo = ''           && Array to hold the Sold To address
  laShipTo = ''           && Array to hold the Ship To address
  laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'

  lcTime = TIME()          && Variable to hold the Time

  lcSqlCommand="SELECT SYCCOMP.CCOM_NAME,SYCCOMP.CCOM_PHON,cCom_Fax,caddress1,caddress2,caddress3,caddress4,caddress5,caddress6,ccont_code "
  lcSqlCommand=lcSqlCommand+"  FROM SYCCOMP WHERE CCOMP_ID ='"+oAriaApplication.ActiveCompanyID+"'  "
  LOCAL lnResult
  lnResult  = oAriaApplication.RemoteSystemData.Execute(lcSqlCommand,"",lcCompInfo,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  IF lnResult = 1
    SELECT &lcCompInfo
    lcCompFax       = cCom_Fax
    lcCompName      = cCom_Name
    lcCompPhon      = cCom_Phon              && Variable to hold the Company Phone
    lcPhonPict      = gfPhoneTem()          && Variable to hold the Company Phone Format
    laCompAdd[1]    = gfGetAdr(lcCompInfo , '' , '' , '' , 1)
    laCompAdd[2]    = gfGetAdr(lcCompInfo , '' , '' , '' , 2)
    laCompAdd[3]    = gfGetAdr(lcCompInfo , '' , '' , '' , 3)
    laCompAdd[4]    = gfGetAdr(lcCompInfo , '' , '' , '' , 4)
    laCompAdd[5]    = gfGetAdr(lcCompInfo, '' , '' , '' , 5)
    lcCompFax = TRANSFORM(lcCompFax , '@R '+lcPhonPict)  && Fax No. Pic
    lcXphone  = TRANSFORM(lcCompPhon , '@R '+lcPhonPict) && variable hold the phone format to use it
    = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  ENDIF 


IF llOGFltCh
  IF USED(lcOrdhdr)
    USE IN (lcOrdhdr)
    SELECT * FROM &lcTempOrdHdr WHERE .F. INTO CURSOR &lcOrdhdr READWRITE 
    =lfMakeIndex(lcOrdhdr)
  ENDIF 
  IF USED(lcNotePad)
     USE IN (lcNotePad)
     SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE 
    =lfMakeIndex(lcNotePad)
  ENDIF   
  IF USED(lcOrdLnTmp)
    USE IN (lcOrdLnTmp)
    SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE 
    =lfMakeIndex(lcOrdLnTmp)
  ENDIF 
  IF USED(lcPikTemp)
    USE IN (lcPikTemp)
    SELECT * FROM &lcTempPikTkt WHERE .F. INTO CURSOR &lcPikTemp READWRITE 
    =lfMakeIndex(lcPikTemp)
  ENDIF 
  STORE 0 TO lnLenthM1
  lnLenthM1 = LEN(gfItemMask('PM'))

  =lfGTmpOrdL()

  SELECT (lcTmpOrdL)
  SET ORDER TO TAG (lcTmpOrdL)
  SET ORDER TO TAG (lcTmpOrdH) IN &lcTmpOrdH
  SET RELATION TO Order + PikTkt INTO &lcTmpOrdH

  SET RELATION TO PikTkt INTO &lcPiktktTemp ADDITIVE
  SET RELATION TO 'O' + Order INTO &lcOrdHdr ADDITIVE


  SET RELATION TO Style INTO &lcStyleFile ADDITIVE
  SET RELATION TO 'S' + Scale INTO &lcScaleFile ADDITIVE
  SELECT(lcPiktktTemp)
  SET RELATION TO cWareCode INTO &lcWareHous
  SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                      'S' + Account + Store) INTO &lcCustomer ADDITIVE


ENDIF 
SELECT (lcTmpOrdL)
LOCATE FOR !EMPTY(Style)
IF EOF()
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN 
ELSE
  lnOutFile = FCREATE(lcRpPath,0)
  IF lnOutFile < 0
    =gfModalGen('TRM00000B00000','ALERT','','','Could not create file. Cannot proceed.')
    RETURN .F.
  ENDIF
  lfExportCSV()
  =FCLOSE(lnOutFile)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File "+ALLTRIM(lcRpPath)+" has been exported successfully")
ENDIF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/05/2012
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*
FUNCTION lfwRepWhen
*PRIVATE lnWareHElm , lnVrFltElm , llRefresh
loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdPrint.enabled = .F.

IF EMPTY(lcRpPath) OR EMPTY(lcRpPath)
  IF FILE(oariaApplication.DataDir+'ExportPkTk'+'.MEM')
    RESTORE FROM oariaApplication.DataDir+'ExportPkTk'+'.MEM' ADDITIVE
  ENDIF
ENDIF


*--PikLine file
IF TYPE('loPikLine') <> 'O'
  loPikLine= CreateObject("RemoteTable","PikLine","PikLine",lcTempPikLine,SET("DATASESSION")) 
ENDIF 
*--Piktkt file
IF TYPE('loPikTkt') <> 'O'
   loPikTkt   = CreateObject("RemoteTable","PikTkt","PikTkt",lcTempPikTkt,SET("DATASESSION"))
   SELECT * FROM &lcTempPikTkt WHERE .F. INTO CURSOR &lcPikTemp READWRITE 
   =lfMakeIndex(lcPikTemp)
ENDIF

*--Customer file
IF TYPE('loCustomer') <> 'O'
  loCustomer = CreateObject("RemoteTable","Customer","Customer",lcTempCustomer,SET("DATASESSION"))  
  SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
  =lfMakeIndex(lcCustomer)
ENDIF   

*--Notepad file
IF TYPE('loNotePad') <> 'O'
  loNotePad  = CreateObject("RemoteTable","NotePad","NotePad",lcTempNotePad,SET("DATASESSION")) 
  SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE 
  =lfMakeIndex(lcNotePad)
ENDIF  

*--Style file
IF TYPE('loStyle') <> 'O' 
  loStyle    = CreateObject("RemoteTable","Style","Style",lcTempStyle,SET("DATASESSION")) 
  SELECT * FROM &lcTempStyle WHERE .F. INTO CURSOR &lcStyleFile READWRITE 
  =lfMakeIndex(lcStyleFile)
ENDIF

*--Scale file
IF TYPE('loScale') <> 'O'
  loScale    = CreateObject("RemoteTable","Scale","Scale",lcTempScale,SET("DATASESSION")) 
  SELECT * FROM &lcTempScale WHERE .F. INTO CURSOR &lcScaleFile READWRITE 
  =lfMakeIndex(lcScaleFile)
ENDIF 
  
*--Ordhdr file  
IF TYPE('loOrdHdr') <> 'O'
  loOrdHdr   = CreateObject("RemoteTable","OrdHdr","OrdHdr",lcTempOrdHdr,SET("DATASESSION")) 
  SELECT * FROM &lcTempOrdHdr WHERE .F. INTO CURSOR &lcOrdhdr READWRITE 
  =lfMakeIndex(lcOrdhdr)
ENDIF

*--Ordline file 
IF TYPE('loOrdLine') <> 'O'  
  loOrdLine  = CreateObject("RemoteTable","OrdLine","ORDLINST",lcTempOrdLine,SET("DATASESSION")) 
  SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE 
  =lfMakeIndex(lcOrdLnTmp)
ENDIF   

*--WareHous  file
IF TYPE('loWareHous') <> 'O'  
  loWareHous  = CreateObject("RemoteTable","WareHous","WareHous",lcTempWareHous,SET("DATASESSION")) 
  SELECT * FROM &lcTempWareHous WHERE .F. INTO CURSOR &lcWareHous READWRITE 
  =lfMakeIndex(lcWareHous)
ENDIF   

*--Spck_lin file
IF TYPE('loSpck_Lin') <> 'O'  
  loSpck_Lin = CreateObject("RemoteTable","Spck_Lin","SPKLNSTCN",lcTempSpck_Lin,SET("DATASESSION")) 
ENDIF   

DIMENSION laTmpOrdLS[1,4]

SELECT(lcTempOrdLine)
DIMENSION laTmpOrdLS[1,18]
lnFldLen = AFIELDS(laTmpOrdLS)
*C200803,1 TMI [Start] 
*DIMENSION laTmpOrdLS[lnFldLen + 3 , 18]
*! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
DIMENSION laTmpOrdLS[lnFldLen + 5 , 18]

laTmpOrdLS[lnFldLen + 1 , 1] = 'cGrupDetal'
laTmpOrdLS[lnFldLen + 1 , 2] = 'C'
laTmpOrdLS[lnFldLen + 1 , 3] = 1
laTmpOrdLS[lnFldLen + 1 , 4] = 0

laTmpOrdLS[lnFldLen + 2 , 1] = 'CurrSmbl'
laTmpOrdLS[lnFldLen + 2 , 2] = 'C'
laTmpOrdLS[lnFldLen + 2 , 3] = 3
laTmpOrdLS[lnFldLen + 2 , 4] = 0

laTmpOrdLS[lnFldLen + 3 , 1] = 'CurrCode'
laTmpOrdLS[lnFldLen + 3 , 2] = 'C'
laTmpOrdLS[lnFldLen + 3 , 3] = 3
laTmpOrdLS[lnFldLen + 3 , 4] = 0

*C200803,1 TMI [Start] 
laTmpOrdLS[lnFldLen + 4 , 1] = 'LLASTPAGE'
laTmpOrdLS[lnFldLen + 4 , 2] = 'L'
laTmpOrdLS[lnFldLen + 4 , 3] = 1
laTmpOrdLS[lnFldLen + 4 , 4] = 0

laTmpOrdLS[lnFldLen + 5 , 1] = 'PAGENO'
laTmpOrdLS[lnFldLen + 5 , 2] = 'N'
laTmpOrdLS[lnFldLen + 5 , 3] = 4
laTmpOrdLS[lnFldLen + 5 , 4] = 0
FOR lnLoop = 1 TO ALEN(laTmpOrdLS,1)-lnFldLen 
  STORE ' ' TO  laTmpOrdLS[lnFldLen +lnLoop,7],laTmpOrdLS[lnFldLen +lnLoop,8],;
                laTmpOrdLS[lnFldLen +lnLoop,9],laTmpOrdLS[lnFldLen +lnLoop,10],;
                laTmpOrdLS[lnFldLen +lnLoop,11],laTmpOrdLS[lnFldLen +lnLoop,12],;
                laTmpOrdLS[lnFldLen +lnLoop,13],laTmpOrdLS[lnFldLen +lnLoop,14],;
                laTmpOrdLS[lnFldLen +lnLoop,15],laTmpOrdLS[lnFldLen +lnLoop,16]
  STORE 0 TO    laTmpOrdLS[lnFldLen +lnLoop,17] ,laTmpOrdLS[lnFldLen +lnLoop,18]

ENDFOR

lnOrdSize = ALEN(laTmpOrdLS,1)
DIMENSION laTmpOrdLS[lnOrdSize +16,18]
laTmpOrdLS[lnOrdSize + 1, 1] = 'STNAME'
laTmpOrdLS[lnOrdSize + 1, 2] = 'C'
laTmpOrdLS[lnOrdSize + 1, 3] = 30
laTmpOrdLS[lnOrdSize + 1, 4] = 0

laTmpOrdLS[lnOrdSize + 2, 1] = 'CADDRESS1'
laTmpOrdLS[lnOrdSize + 2, 2] = 'C'
laTmpOrdLS[lnOrdSize + 2, 3] = 30
laTmpOrdLS[lnOrdSize + 2, 4] = 0

laTmpOrdLS[lnOrdSize + 3, 1] = 'CADDRESS2'
laTmpOrdLS[lnOrdSize + 3, 2] = 'C'
laTmpOrdLS[lnOrdSize + 3, 3] = 30
laTmpOrdLS[lnOrdSize + 3, 4] = 0

laTmpOrdLS[lnOrdSize + 4, 1] = 'CADDRESS3'
laTmpOrdLS[lnOrdSize + 4, 2] = 'C'
laTmpOrdLS[lnOrdSize + 4, 3] = 30
laTmpOrdLS[lnOrdSize + 4, 4] = 0

laTmpOrdLS[lnOrdSize + 5, 1] = 'CADDRESS4'
laTmpOrdLS[lnOrdSize + 5, 2] = 'C'
laTmpOrdLS[lnOrdSize + 5, 3] = 30
laTmpOrdLS[lnOrdSize + 5, 4] = 0

laTmpOrdLS[lnOrdSize + 6, 1] = 'CADDRESS5'
laTmpOrdLS[lnOrdSize + 6, 2] = 'C'
laTmpOrdLS[lnOrdSize + 6, 3] = 30
laTmpOrdLS[lnOrdSize + 6, 4] = 0

laTmpOrdLS[lnOrdSize + 7, 1] = 'CADDRESS6'
laTmpOrdLS[lnOrdSize + 7, 2] = 'C'
laTmpOrdLS[lnOrdSize + 7, 3] = 30
laTmpOrdLS[lnOrdSize + 7, 4] = 0
laTmpOrdLS[lnOrdSize + 8, 1] = 'ShipVia'
laTmpOrdLS[lnOrdSize + 8, 2] = 'C'
laTmpOrdLS[lnOrdSize + 8, 3] = 30
laTmpOrdLS[lnOrdSize + 8, 4] = 0
lnCntSizes = 1
FOR lnCntSz = 9 TO 16
  laTmpOrdLS[lnOrdSize + lnCntSz, 1] = 'Size'+STR(lnCntSizes,1)
  laTmpOrdLS[lnOrdSize + lnCntSz, 2] = 'C'
  laTmpOrdLS[lnOrdSize + lnCntSz, 3] = 5
  laTmpOrdLS[lnOrdSize + lnCntSz, 4] = 0
  lnCntSizes = lnCntSizes + 1
ENDFOR 
FOR lnLoop = 1 TO 16
  STORE ' ' TO  laTmpOrdLS[lnOrdSize +lnLoop,7],laTmpOrdLS[lnOrdSize +lnLoop,8],;
                laTmpOrdLS[lnOrdSize +lnLoop,9],laTmpOrdLS[lnOrdSize +lnLoop,10],;
                laTmpOrdLS[lnOrdSize +lnLoop,11],laTmpOrdLS[lnOrdSize +lnLoop,12],;
                laTmpOrdLS[lnOrdSize +lnLoop,13],laTmpOrdLS[lnOrdSize +lnLoop,14],;
                laTmpOrdLS[lnOrdSize +lnLoop,15],laTmpOrdLS[lnOrdSize +lnLoop,16]
  STORE 0 TO    laTmpOrdLS[lnOrdSize +lnLoop,17] ,laTmpOrdLS[lnOrdSize +lnLoop,18]
ENDFOR

 
=gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'cCurrCode','SH')

DIMENSION laTmpOrdHS[7,18]

=ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'ORDER') , 4 , 1)
=ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'PIKTKT') , 4 , 19)

laTmpOrdHS[3 , 1] = 'NWEIGHT'
laTmpOrdHS[3 , 2] = 'N'
laTmpOrdHS[3 , 3] = 10
laTmpOrdHS[3 , 4] = 2

laTmpOrdHS[4 , 1] = 'STATUS'
laTmpOrdHS[4 , 2] = 'C'
laTmpOrdHS[4 , 3] = 1
laTmpOrdHS[4 , 4] = 0

laTmpOrdHS[5 , 1] = 'CUSTPO'
laTmpOrdHS[5 , 2] = 'C'
laTmpOrdHS[5 , 3] = 15
laTmpOrdHS[5 , 4] = 0

laTmpOrdHS[6 , 1] = 'Account'
laTmpOrdHS[6 , 2] = 'C'
laTmpOrdHS[6 , 3] = 5
laTmpOrdHS[6 , 4] = 0

laTmpOrdHS[7 , 1] = 'Store'
laTmpOrdHS[7 , 2] = 'C'
laTmpOrdHS[7 , 3] = 8
laTmpOrdHS[7 , 4] = 0

FOR  n = 1 TO 7
  STORE ' ' TO  laTmpOrdHS[ n ,7] , laTmpOrdHS[ n ,8],;
                laTmpOrdHS[ n ,9] ,laTmpOrdHS[ n ,10],;
                laTmpOrdHS[ n ,11],laTmpOrdHS[ n ,12],;
                laTmpOrdHS[ n ,13],laTmpOrdHS[ n ,14],;
                laTmpOrdHS[ n ,15],laTmpOrdHS[ n ,16]
  STORE 0 TO    laTmpOrdHS[ n ,17],laTmpOrdHS[ n ,18]

ENDFOR 


=lfBrowStat()
*-- end of lfwRepWhen.


*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/05/2012
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*
FUNCTION lfwOldVal
laOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfGTmpOrdL
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/05/2012
*! Purpose   : Function to colect the neede data in a temp. Order lines file
*!*************************************************************
*! Called from : This program.
*!*************************************************************
*
FUNCTION lfGTmpOrdL
PRIVATE lcForExp , lnTotRec , lnCurRec , lnRest , lnSavRec , lcOrder,;
       lcCDXName, lcFullPath

IF USED(lcTmpOrdL)
  USE IN (lcTmpOrdL)
ENDIF    && End of IF
=gfCrtTmp(lcTmpOrdL,@laTmpOrdLS,"PikTkt + Order + cGrupDetal + STR(LineNo , 6)",lcTmpOrdL,.T.)
SELECT(lcTmpOrdL)

*IF The Temp. Order Header file is Opened in one of the work areas
IF USED(lcTmpOrdH)
  USE IN (lcTmpOrdH)
ENDIF    && End of IF
=gfCrtTmp(lcTmpOrdH,@laTmpOrdHS,"Order + PikTkt",lcTmpOrdH,.T.)    
SELECT (lcTmpOrdH)
SET ORDER TO

*- collecting data 
lnPosPikTkt = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.PIKTKT")
IF lnPosPikTkt > 0 
  lnPosPikTkt = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikTkt,1)
  lcPikTkteSel =IIF(!EMPTY(laOgFxFlt[lnPosPikTkt,6]),laOgFxFlt[lnPosPikTkt,6],'')
  IF !EMPTY(lcPikTkteSel) AND USED(lcPikTkteSel)
    SELECT(lcPikTkteSel)
    LOCATE
    IF !EOF()
      SCAN
        loPikTkt.Seek(&lcPikTkteSel..piktkt)
        SELECT(lcTempPikTkt)
        SCATTER MEMO MEMVAR 
        INSERT INTO (lcPikTemp) FROM MEMVAR
      ENDSCAN 
      SELECT(lcPikTemp)
      LOCATE 
      lcSelFileds = lcPikTemp + ".*"
      lcSeleCond  = " "
      lcSeleFiles = lcPikTemp
      *--Status Check
      lcStatusValue = ""
      lnPosPikStatus  = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.STATUS")
      IF lnPosPikStatus  > 0 
        lnPosPikStatus  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikStatus ,1)
        lcStatusValue = IIF(!EMPTY(laOgFxFlt[lnPosPikStatus,6]),laOgFxFlt[lnPosPikStatus,6],'C|X|H|O|P')
        lcStatusValue = "INLIST("+lcPikTemp+".Status,'" + STRTRAN(lcStatusValue,"|","','") +"')"
      ENDIF 
      lcSeleCond  = lcSeleCond + IIF(!EMPTY(lcStatusValue), IIF(EMPTY(lcSeleCond),""," AND ") + lcStatusValue,"")
      *--Date Check
      lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
      IF lnPosDate > 0 
        lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
        SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
        EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
        IF !EMPTY(EDATE) AND !EMPTY(SDATE)
          lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ")+ "BETWEEN(&lcPikTemp..DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
        ENDIF 
      ENDIF
      *--Check user selected accounts
      lnPosPikAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
      IF lnPosPikAcc > 0 
        lnPosPikAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikAcc,1)
        lcPikTktAccSel =IIF(!EMPTY(laOgFxFlt[lnPosPikAcc,6]),laOgFxFlt[lnPosPikAcc,6],'')
        IF !EMPTY(lcPikTktAccSel) AND USED(lcPikTktAccSel)
          SELECT(lcPikTktAccSel)
          LOCATE
          IF !EOF()
            lcSeleCond  = lcSeleCond  + IIF(!EMPTY(lcSeleCond)," AND ","")+lcPikTemp + ".ACCOUNT = " + lcPikTktAccSel + ".ACCOUNT"
            lcSeleFiles = lcSeleFiles + IIF(!EMPTY(lcSeleFiles),",","") +  lcPikTktAccSel
          ENDIF  
        ENDIF 
      ENDIF   
      *--Check user selected locations
      lnPosPikWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
      IF lnPosPikWare > 0 
        lnPosPikWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikWare,1)
        lcPikTktWareSel =IIF(!EMPTY(laOgFxFlt[lnPosPikWare,6]),laOgFxFlt[lnPosPikWare,6],'')
        IF !EMPTY(lcPikTktWareSel) AND USED(lcPikTktWareSel)
          SELECT(lcPikTktWareSel)
          LOCATE
          IF !EOF()
            lcSeleCond  = lcSeleCond +  IIF(!EMPTY(lcSeleCond)," AND ","")+lcPikTemp + ".CWARECODE = " + lcPikTktWareSel + ".CWARECODE"
            lcSeleFiles = lcSeleFiles + IIF(!EMPTY(lcSeleFiles),",","")+lcPikTktWareSel
          ENDIF  
        ENDIF 
      ENDIF   
      IF !EMPTY(lcSeleCond)
        SELECT &lcSelFileds FROM &lcSeleFiles  WHERE &lcSeleCond INTO CURSOR &lcPiktktTemp READWRITE 
        =lfMakeIndex(lcPiktktTemp)
      ELSE
        SELECT &lcSelFileds FROM &lcSeleFiles INTO CURSOR &lcPiktktTemp READWRITE 
        =lfMakeIndex(lcPiktktTemp)
      ENDIF 

      SELECT(lcPiktktTemp)
      LOCATE 
      IF !EOF()
        SCAN
          loOrdhdr.Seek('O'+&lcPiktktTemp..Order) 
          SELECT(lcTempOrdHdr)
          SCATTER MEMO MEMVAR 
          INSERT INTO(lcOrdhdr) FROM MEMVAR 
        ENDSCAN 
      ENDIF 
      *--check the order status on hold or not 
      =lfGetData()
      RETURN 
    ENDIF 
  ENDIF 
ENDIF 
*-- In CASE of user does not select picking Ticket no. OR SELECT any other data 
IF loPiktkt.llNative && case files still fox
  lcSelFileds = "Piktkt.*"
  lcSeleCond  = "!EMPTY(Piktkt.PikTkt) AND Piktkt.PikTkt <> '******'"
  lcSeleFiles = "Piktkt"
  *--Status Check
  lcStatusValue = ""
  lnPosPikStatus  = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.STATUS")
  IF lnPosPikStatus  > 0 
    lnPosPikStatus  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikStatus ,1)
    lcStatusValue = IIF(!EMPTY(laOgFxFlt[lnPosPikStatus,6]),laOgFxFlt[lnPosPikStatus,6],'C|X|H|O|P')
    lcStatusValue = "INLIST(Piktkt.Status,'" + STRTRAN(lcStatusValue,"|","','") +"')"
  ENDIF 
  lcSeleCond  = lcSeleCond + IIF(!EMPTY(lcStatusValue),  IIF(EMPTY(lcSeleCond),""," AND ")+  lcStatusValue,"")
  *--Date Check
  lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
  IF lnPosDate > 0 
    lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
    SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
    EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
    IF !EMPTY(EDATE) AND !EMPTY(SDATE)
      lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ")+ "BETWEEN(Piktkt.DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
    ENDIF 
  ENDIF
  *--Check user selected accounts
  lnPosPikAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
  IF lnPosPikAcc > 0 
    lnPosPikAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikAcc,1)
    lcPikTktAccSel =IIF(!EMPTY(laOgFxFlt[lnPosPikAcc,6]),laOgFxFlt[lnPosPikAcc,6],'')
    IF !EMPTY(lcPikTktAccSel) AND USED(lcPikTktAccSel)
      SELECT(lcPikTktAccSel)
      LOCATE
      IF !EOF()
        lcSeleCond  = lcSeleCond  + IIF(!EMPTY(lcSeleCond)," AND ","")+"Piktkt.ACCOUNT = " + lcPikTktAccSel + ".ACCOUNT"
        lcSeleFiles = lcSeleFiles + IIF(!EMPTY(lcSeleFiles),",","") +  lcPikTktAccSel
      ENDIF  
    ENDIF 
  ENDIF   
  *--Check user selected locations
  lnPosPikWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
  IF lnPosPikWare > 0 
    lnPosPikWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikWare,1)
    lcPikTktWareSel =IIF(!EMPTY(laOgFxFlt[lnPosPikWare,6]),laOgFxFlt[lnPosPikWare,6],'')
    IF !EMPTY(lcPikTktWareSel) AND USED(lcPikTktWareSel)
      SELECT(lcPikTktWareSel)
      LOCATE
      IF !EOF()
        lcSeleCond  = lcSeleCond +  IIF(!EMPTY(lcSeleCond)," AND ","")+"Piktkt.CWARECODE = " + lcPikTktWareSel + ".CWARECODE"
        lcSeleFiles = lcSeleFiles + IIF(!EMPTY(lcSeleFiles),",","")+lcPikTktWareSel
      ENDIF  
    ENDIF 
  ENDIF   
  IF !EMPTY(lcSeleCond)
     SELECT &lcSelFileds FROM &lcSeleFiles  WHERE &lcSeleCond INTO CURSOR &lcPiktktTemp READWRITE 
     =lfMakeIndex(lcPiktktTemp)
  ELSE
     SELECT &lcSelFileds FROM &lcSeleFiles INTO CURSOR &lcPiktktTemp READWRITE 
     =lfMakeIndex(lcPiktktTemp)
  ENDIF 
  SELECT(lcPiktktTemp)
  LOCATE 
  IF !EOF()
    SCAN
      loOrdhdr.Seek('O'+&lcPiktktTemp..Order) 
      SELECT(lcTempOrdHdr)
      SCATTER MEMO MEMVAR 
      INSERT INTO(lcOrdhdr) FROM MEMVAR 
    ENDSCAN 
  ENDIF 
  =lfGetData()
  RETURN 
ELSE && files converted to SQL

  lcSelFileds = "Piktkt.*"
  lcSeleCond  = "!EMPTY(Piktkt.PikTkt) AND Piktkt.PikTkt <> '******'"
  lcSeleFiles = "Piktkt"
  *--Status Check
  lcStatusValue = ""
  lnPosPikStatus  = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.STATUS")
  IF lnPosPikStatus  > 0 
    lnPosPikStatus  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikStatus ,1)
    lcStatusValue = IIF(!EMPTY(laOgFxFlt[lnPosPikStatus,6]),laOgFxFlt[lnPosPikStatus,6],'C|X|H|O|P')
    lcStatusValue = "Piktkt.Status IN('" + STRTRAN(lcStatusValue,"|","','") +"')"
  ENDIF 
  lcSeleCond  = lcSeleCond + IIF(!EMPTY(lcStatusValue), IIF(EMPTY(lcSeleCond),""," AND ")+  lcStatusValue,"")
  *--Date Check
  lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
  IF lnPosDate > 0 
    lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
    SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
    EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
    IF !EMPTY(EDATE) AND !EMPTY(SDATE)
      lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ")+ "Piktkt.DATE BETWEEN "+CTOD(SDATE)+" AND " + CTOD(EDATE)
    ENDIF 
  ENDIF
  *--Check user selected accounts
  lnPosPikAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
  IF lnPosPikAcc > 0 
    lnPosPikAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikAcc,1)
    lcPikTktAccSel =IIF(!EMPTY(laOgFxFlt[lnPosPikAcc,6]),laOgFxFlt[lnPosPikAcc,6],'')
    IF !EMPTY(lcPikTktAccSel) AND USED(lcPikTktAccSel)
      SELECT(lcPikTktAccSel)
      LOCATE
      IF !EOF()
        lcCurName = lcPikTktAccSel
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
          IF (RECCOUNT() > 0) 
            lcSQLAcc = loOgScroll.gfSQLTempName('','Account C(5)',lcCurName,'Account')
            lcSeleFiles = lcSeleFiles + "," + lcSQLAcc
            lcSeleCond = IIF(EMPTY(lcSeleCond ),""," AND ") + "PikTkt.ACCOUNT = " + lcSQLAcc + ".ACCOUNT"
          ENDIF 
        ENDIF    
      ENDIF 
    ENDIF 
  ENDIF 
  lnPosPikWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
  IF lnPosPikWare > 0 
    lnPosPikWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikWare,1)
    lcPikTktWareSel =IIF(!EMPTY(laOgFxFlt[lnPosPikWare,6]),laOgFxFlt[lnPosPikWare,6],'')
    IF !EMPTY(lcPikTktWareSel) AND USED(lcPikTktWareSel)
      SELECT(lcPikTktWareSel)
      LOCATE
      IF !EOF()
        lcCurName = lcPikTktWareSel 
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
          IF (RECCOUNT() > 0) 
            lcSQLWare = loOgScroll.gfSQLTempName('','Cwarecode C(10)',lcCurName,'Cwarecode')
            lcSeleFiles = lcSeleFiles + "," + lcSQLWare
            lcSeleCond  =IIF(EMPTY(lcSeleCond),""," AND ") + "PikTkt.CWARECODE = " + lcSQLWare + ".CWARECODE"
          ENDIF 
        ENDIF     
      ENDIF  
    ENDIF 
  ENDIF   

  =lfOpenSql(lcSelFileds ,lcSeleFiles  , lcPiktktTemp ,lcSeleCond )  
  SELECT(lcPiktktTemp)
  LOCATE 
  IF !EOF()
    SCAN
      loOrdhdr.Seek('O'+&lcPiktktTemp..Order) 
      SELECT(lcTempOrdHdr)
      SCATTER MEMO MEMVAR 
      INSERT INTO(lcOrdhdr) FROM MEMVAR 
    ENDSCAN 
  ENDIF 
  =lfGetData()
  RETURN 
ENDIF 
*-- end of lfGTmpOrdL.

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/05/2012
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Season ,
*!             Special Instructions , Terms
*!*************************************************************
*! Called from : ALPKTKTA.FRX (Inside the FRX)
*!*************************************************************
*
FUNCTION lfSolSpAdr
PRIVATE lcDistCntr

llEndGroup = .F.
=gfRltFld(EVALUATE(lcORDHDR+'.cDivision') , @laDivLName , 'CDIVISION')

SELECT(lcCUSTOMER)
SEEK IIF(EMPTY(&lcPiktktTemp..STORE),'M','S')+ &lcPiktktTemp..Account + &lcPiktktTemp..STORE

lcShipVia = gfCodDes(IIF(&lcCUSTOMER..nBrkWeight <> 0 .AND.;
                         &lcTmpOrdH..nWeight > &lcCUSTOMER..nBrkWeight  and !EMPTY(&lcCUSTOMER..cAltShpvia),;
                         &lcCUSTOMER..cAltShpvia ,IIF(&lcORDHDR..ShipVia ='*',&lcCUSTOMER..ShipVia,&lcORDHDR..ShipVia)), 'SHIPVIA')                         


lcSeason = gfCodDes(&lcORDHDR..Season , 'SEASON')
lcSpcInst = gfCodDes(&lcORDHDR..SpcInst , 'SPCINST')
lcTerms = gfCodDes(&lcORDHDR..cTermCode , 'CTERMCODE')

SELECT(lcCUSTOMER)
SEEK IIF(EMPTY(&lcPiktktTemp..Store) , 'M' , 'S') + &lcPiktktTemp..Account + &lcPiktktTemp..Store

lcSolTName = BTName
lcShpTName = IIF(&lcORDHDR..Alt_ShpTo , &lcORDHDR..STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 4 , '2')
laSoldTo[5] = ALLTRIM(gfGetAdr(lcCUSTOMER , '' , '' , '' , 5 , '2'))+ ' Phone# '+ TRANSFORM(&lcCustomer..Phone1 , '@R '+lcPhonPict)
=lfAdrShift('laSoldTo')

IF &lcORDHDR..Alt_ShpTo

  SELECT (lcORDHDR)
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5

ELSE    && Else
  SELECT(lcCUSTOMER)
  lcDistCntr = &lcCUSTOMER..Dist_Ctr
  *--If there is a distribution center
  *N000592,1 HBG 02/27/2007 Print Store Address or DC Address depnding on the Flag of Dircet To Store in ORDHDR [Begin]
  *IF !EMPTY(lcDistCntr)
  IF !EMPTY(lcDistCntr) AND !(&lcORDHDR..lStrDirct)
  *N000592,1 HBG [End]
    SEEK 'S' + &lcPiktktTemp..Account + lcDistCntr
  ENDIF

  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 4)
  laShipTo[5] = ALLTRIM(gfGetAdr(lcCUSTOMER , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(&lcCustomer..Phone1 ,'@R '+lcPhonPict)
ENDIF    && End of IF

=lfAdrShift('laShipTo')

SELECT (lcTmpOrdL)
RETURN ''
*-- end of lfSolSpAdr.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/05/2012
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : This program , lfSolSpAdr()
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5

  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])

    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5

  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.

*!*************************************************************
*! Name      : lfPktktSet
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/05/2012
*! Purpose   : Function to set and release the relations needed
*!             for the Pick ticket # field [For the In range]
*!*************************************************************
*! Called from : Picking ticket field [Option Grid]
*!*************************************************************
*!
FUNCTION lfPktktSet
PARAMETERS lcParm
IF lcParm = 'S'
  SELECT PIKTKT
  SET RELATION TO 'O' + Order INTO ORDHDR
  SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                      'S' + Account + Store) INTO CUSTOMER ADDITIVE
ELSE
  SELECT PIKTKT
  SET RELATION TO
ENDIF
*-- end of lfPktktSet.

*!*************************************************************
*! Name      : lfBrowStat
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/05/2012
*! Purpose   : Calling from the when function to fill lcText.
*!*************************************************************
*! Called from : This program
*!*************************************************************
*!
FUNCTION lfBrowStat

STORE SPACE(0) TO lcText1 , lcText
lnDataPos = ASCAN(loOGScroll.laOGFXFlt,'PIKTKT.STATUS')
IF lnDataPos > 0
    lnDataPos  = ASUBSCRIPT(loOGScroll.laOGFXFlt,lnDataPos,1)
    lcText1 = ALLTRIM(loOGScroll.laOGFXFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFxFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFxFlt[lnDataPos,6] + '"' )
ENDIF
IF EMPTY(lcText1)
  lcText = "FOR PIKTKT # '******' "
ELSE
  lcText  = "FOR " + lcText1  + " .AND. PIKTKT # '******' "
ENDIF
RETURN lcText
*--End of lfBrowStat

*!*************************************************************
*! Name      : lfvstate
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/05/2012
*! Purpose   : Called from PIKTKT.STATUS field to match the user selection.
*!*************************************************************
*! Called from : This program
*!*************************************************************
*!
FUNCTION lfvstate
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'V'  && Set code

   STORE SPACE(0) TO lcText1 , lcText
   lnDataPos = ASCAN(loOGScroll.laOGFxFlt,'PIKTKT.STATUS')
   IF lnDataPos > 0
    lnDataPos  = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDataPos,1)
    lcText1 = ALLTRIM(loOGScroll.laOGFxFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFxFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgfFxFlt[lnDataPos,6] + '"' )
ENDIF
IF EMPTY(lcText1)
  lcText = "FOR PIKTKT # '******' "
ELSE
  lcText  = "FOR " + lcText1  + " .AND. PIKTKT # '******' "
ENDIF
ENDCASE 
*lcText  = lcText1  + " .AND. PIKTKT # '******' "
*--End of lfvstate.
*!*************************************************************
*! Name      : lfSrvPik 
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/05/2012
*! Purpose   : Calling from the when function to fill lcText.
*!*************************************************************
*! Called from : This program
*!*************************************************************
*!
FUNCTION lfSrvPik  
PARAMETERS lcParm
PRIVATE lcAlias,lcSeleStatus
lnAlias = SELECT()
SELECT PIKTKT
SET FILTER TO
lnDataPos = ASCAN(loOGScroll.laOGFXFlt,'PIKTKT.STATUS')
IF lnDataPos > 0
  lnDataPos  = ASUBSCRIPT(loOGScroll.laOGFXFlt,lnDataPos,1)
  lcSeleStatus =IIF(EMPTY(laOgFxFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFxFlt[lnDataPos,6] + '"' )
ENDIF 
DO CASE
  CASE lcParm = 'S'  && Set code
    STORE SPACE(0) TO lcText1 , lcText
    lnDataPos = ASCAN(loOGScroll.laOGFXFlt,'PIKTKT.STATUS')
    IF lnDataPos > 0
    lnDataPos  = ASUBSCRIPT(loOGScroll.laOGFXFlt,lnDataPos,1)
    lcText1 = ALLTRIM(loOGScroll.laOGFXFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFxFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFxFlt[lnDataPos,6] + '"' )
    lcSeleStatus =IIF(EMPTY(laOgFxFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFxFlt[lnDataPos,6] + '"' )
  ENDIF 
  IF EMPTY(lcText1)
     lcText = " PIKTKT # '******' "
 *    lcText = "FOR PIKTKT # '******' "
  ELSE
*    lcText  = "FOR " + lcText1  + " .AND. PIKTKT # '******' "
    lcText  =  lcText1  + " .AND. PIKTKT # '******' "
  ENDIF
  SELECT PIKTKT
  SET FILTER TO &lcText
*  RETURN lcText
  CASE lcParm = 'R'
    STORE SPACE(0) TO  lcTempPik
    lnPosition = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'PIKTKT.PIKTKT'),1)
    IF lnPosition > 0
      lcTempPik = loOgScroll.laOGFxFlt[lnPosition,6]
      IF !EMPTY(lcTempPik) AND USED(lcTempPik)
        SELECT(lcTempPik)
        SCAN
          lcPikTk = &lcTempPik..PikTkt
          IF SEEK(&lcTempPik..piktkt,'piktkt')
            IF !(piktkt.status $ lcSeleStatus)
              DELETE FROM &lcTempPik WHERE  &lcTempPik..PikTkt = lcPikTk
            ENDIF 
          ENDIF 
        ENDSCAN 
      ENDIF 
    ENDIF 
ENDCASE
SELECT(lnAlias)  

*!*************************************************************
*! Name      : lfMakeIndex
*: Developer : Mariam Mazhar (MMT)
*! Date      : 08/05/2012
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
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*! Date      : 08/05/2012
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
  DO CASE

   CASE UPPER(lcTable) =  lcPikTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'piktkt'
      laIndex[1,2] = lcPikTemp

   *--temp. Customer File
   CASE UPPER(lcTable) = lcCustomer
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'TYPE+ACCOUNT+STORE'
     laIndex[1,2] = lcCustomer

   *--temp. ordhdr file
   CASE UPPER(lcTable) =  lcOrdHdr
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'CORDTYPE+ORDER'
     laIndex[1,2] = lcOrdHdr

   *--temp. ordline file
   CASE UPPER(lcTable) =  lcOrdLnTmp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'CORDTYPE+ORDER+STR(LINENO,6)'
      laIndex[1,2] = lcOrdLnTmp

   *--temp. pikline file
   CASE UPPER(lcTable) =  lcNotePad
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'type+key'
      laIndex[1,2] = lcNotePad

   CASE UPPER(lcTable) =  lcStyleFile
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'Style'
      laIndex[1,2] = lcStyleFile

   CASE UPPER(lcTable) =  lcScalefile
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'type+scale+prepak'
      laIndex[1,2] = lcScalefile

   CASE UPPER(lcTable) =  lcWareHous
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'cWareCode'
      laIndex[1,2] = lcWareHous
   *--lcPikTemp
   CASE UPPER(lcTable) =  lcPikTemp
      DIMENSION laIndex[2,2]
      laIndex[1,1] = 'ACCOUNT'
      laIndex[1,2] = lcPikTemp
      laIndex[1,1] = 'CWARECODE'
      laIndex[1,2] = lcPikTempInd

   *--lcPiktktTemp
   CASE UPPER(lcTable) =  lcPiktktTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'PikTkt'
      laIndex[1,2] = lcPiktktTemp

ENDCASE
*!*************************************************************
*! Name      : lfGetData
*: Developer : Mariam Mazhar (MMT)
*! Date      : 08/05/2012
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetData

SELECT(lcPiktktTemp)
SET ORDER TO
SET RELATION TO 'O' + Order INTO &lcORDHDR


WAIT 'Selecting picking tickets...' WINDOW NOWAIT

SELECT(lcPiktktTemp)
LOCATE 
SCAN
  INSERT INTO (lcTmpOrdH) (Order , PikTkt , Status , Account,Store) VALUES;
                            (&lcPiktktTemp..Order ,&lcPiktktTemp..PikTkt ,&lcPiktktTemp..Status ,&lcPiktktTemp..Account,&lcPiktktTemp..Store)
ENDSCAN                           

SELECT(lcPiktktTemp)
SET ORDER TO tag &lcPiktktTemp
SET RELATION TO

SELECT(lcTempOrdLine)
SET RELATION TO PikTkt INTO &lcPiktktTemp


SELECT (lcTmpOrdH)
LOCATE  
SET ORDER TO TAG (lcTmpOrdH)
SET RELATION TO 'O'+Order INTO &lcORDHDR ADDITIVE
lnTotRec = RECCOUNT()      && Varible to hold the Total count to be done [For the thermometer]
lnCurRec = 0               && Varible to hold the current count to be done [For the thermometer]

SELECT (lcTmpOrdH)
LOCATE  

SCAN FOR Status <> 'X'
 
  lnCurRec = lnCurRec + 1
  lnSavRec = RECNO()
  *IF There is one or more records for this Order in the ORDLINE file
  IF loOrdLine.seek('O' + &lcTmpOrdH..Order)
*  IF SEEK('O' + &lcTmpOrdH..Order , 'ORDLINE')
    SELECT (lcTmpOrdH)
    lcPikTkt = piktkt
    M.cGrupDetal = 'D'
    lcOrder = Order
    SELECT(lcTempOrdLine)
*-- Scan Loop to scan the ORDLINE file FOR The current Order and FOR the Option Grid condition
    SCAN REST;
        WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+lcOrder FOR  &lcTempOrdLine..piktkt = lcPikTkt
      SELECT(lcTmpOrdH)
      SEEK &lcTempOrdLine..Order + &lcTempOrdLine..PikTkt
      =loStyle.SEEK(&lcTempOrdLine..Style)
      SELECT (lcTmpOrdH)
      REPLACE nWeight WITH nWeight + (&lcTempStyle..nStyWeight * &lcTempOrdLine..totpik)

      IF(SEEK("O"+&lcTempOrdLine..ORDER,lcOrdHdr) AND &lcOrdHdr..Multipo)
        REPLACE Custpo WITH IIF(Custpo <> &lcTempOrdLine..Custpo,IIF(EMPTY(Custpo),&lcTempOrdLine..Custpo,;
        "Multi PO"),&lcTempOrdLine..Custpo)
      ELSE
        REPLACE Custpo WITH &lcOrdHdr..Custpo
      ENDIF

      SELECT(lcTempOrdLine)
      SCATTER MEMVAR MEMO

      =SEEK(&lcOrdHdr..cCurrCode,'SYCCURR')
      m.CurrCode = &lcOrdHdr..cCurrCode
      m.CurrSmbl = SYCCURR.ccurrsmbl
      

      IF &lcORDHDR..Alt_ShpTo
        m.StName    = &lcORDHDR..STName
        m.cAddress1 = &lcORDHDR..caddress1 
        m.cAddress2 = &lcORDHDR..caddress2 
        m.cAddress3 = &lcORDHDR..caddress3 
        m.cAddress4 = &lcORDHDR..caddress4 
        m.cAddress5 = &lcORDHDR..caddress5
        m.cAddress6 = '' 
      ELSE
        loCustomer.Seek(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store),'Customer')
        lcDistCenter = &lcTempCustomer..Dist_Ctr
        IF !EMPTY(lcDistCenter) AND !(&lcORDHDR..lStrDirct)
		  loCustomer.Seek('S' + m.Account + lcDistCenter,'Customer')
		ENDIF
        m.StName    = IIF(EMPTY(&lcTempCustomer..DBA) , &lcTempCustomer..STName, &lcTempCustomer..DBA)
        m.cAddress1 = &lcTempCustomer..caddress1 
        m.cAddress2 = &lcTempCustomer..caddress2 
        m.cAddress3 = &lcTempCustomer..caddress3 
        m.cAddress4 = &lcTempCustomer..caddress4 
        m.cAddress5 = &lcTempCustomer..caddress5
        m.cAddress6 = &lcTempCustomer..caddress6
      ENDIF
	  m.ShipVia = gfCodDes(&lcORDHDR..SHIPVIA, 'SHIPVIA')
 	  =loScale.SEEK('S'+&lcTempStyle..Scale)
 	  FOR lnCntSz = 1 TO 8
 	    lcCntSz = STR(lnCntSz,1)
 	    m.Size&lcCntSz. = &lcTempScale..Sz&lcCntSz.
 	  ENDFOR 

      INSERT INTO (lcTmpOrdL)  FROM MEMVAR
    ENDSCAN    && End of SCAN Loop

    SCATTER MEMVAR MEMO BLANK
    SELECT (lcTmpOrdH)
    GO lnSavRec
    M.Order = Order

    LOCATE REST FOR Order + PikTkt > M.Order
    
    GO RECORD lnSavRec
**      SKIP -1
  ENDIF    && End of IF
ENDSCAN    && End of SCAN Loop

SELECT (lcTmpOrdH)

*SCAN Loop to scan the Temp. Order Header file FOR Status = 'X' [Released]
SCAN FOR Status $ 'XC'

  lnCurRec = lnCurRec + 1

  *IF There is one or more records for this Order in the PIKLINE file
  IF loPikLine.SEEK(&lcTmpOrdH..PikTkt + &lcTmpOrdH..Order)
    M.cGrupDetal = 'D'
    SELECT(lcTempPikLine)
    SCAN REST;
        WHILE PikTkt + Order = &lcTmpOrdH..PikTkt + &lcTmpOrdH..Order

      SELECT (lcTmpOrdH)
      =loStyle.Seek(&lcTempPikLine..Style)
      SELECT (lcTmpOrdH)
      REPLACE nWeight WITH nWeight + (&lcTempStyle..nStyWeight * &lcTempPikLine..totpik)

      SELECT(lcTempPikLine)
      SCATTER MEMVAR MEMO
      =SEEK(&lcOrdHdr..cCurrCode,'SYCCURR')
      m.CurrCode = &lcOrdHdr..cCurrCode
      m.CurrSmbl = SYCCURR.ccurrsmbl

      IF &lcORDHDR..Alt_ShpTo
        m.StName    = &lcORDHDR..STName
        m.cAddress1 = &lcORDHDR..caddress1 
        m.cAddress2 = &lcORDHDR..caddress2 
        m.cAddress3 = &lcORDHDR..caddress3 
        m.cAddress4 = &lcORDHDR..caddress4 
        m.cAddress5 = &lcORDHDR..caddress5
        m.cAddress6 = '' 
      ELSE
        loCustomer.Seek(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store),'Customer')
        lcDistCenter = &lcTempCustomer..Dist_Ctr
        IF !EMPTY(lcDistCenter) AND !(&lcORDHDR..lStrDirct)
		  loCustomer.Seek('S' + m.Account + lcDistCenter,'Customer')
		ENDIF
        m.StName    = IIF(EMPTY(&lcTempCustomer..DBA) , &lcTempCustomer..STName, &lcTempCustomer..DBA)
        m.cAddress1 = &lcTempCustomer..caddress1 
        m.cAddress2 = &lcTempCustomer..caddress2 
        m.cAddress3 = &lcTempCustomer..caddress3 
        m.cAddress4 = &lcTempCustomer..caddress4 
        m.cAddress5 = &lcTempCustomer..caddress5
        m.cAddress6 = &lcTempCustomer..caddress6
      ENDIF
      m.ShipVia = gfCodDes(&lcORDHDR..SHIPVIA, 'SHIPVIA')
      =loScale.SEEK('S'+&lcTempStyle..Scale)
      FOR lnCntSz = 1 TO 8
       lcCntSz = STR(lnCntSz,1)
       m.Size&lcCntSz. = &lcTempScale..Sz&lcCntSz.
      ENDFOR 

      INSERT INTO (lcTmpOrdL)  FROM MEMVAR
    ENDSCAN    && End of SCAN Loop

    SCATTER MEMVAR MEMO BLANK
    SELECT (lcTmpOrdH)

    loCustomer.Seek(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store),'Customer')
    lcDistCenter = &lcTempCustomer..Dist_Ctr
    IF !EMPTY(lcDistCenter) AND !(&lcORDHDR..lStrDirct)
	  loCustomer.Seek('S' + m.Account + lcDistCenter,'Customer')
	ENDIF
    m.StName    = IIF(EMPTY(&lcTempCustomer..DBA) , &lcTempCustomer..STName, &lcTempCustomer..DBA)
    m.cAddress1 = &lcTempCustomer..caddress1 
    m.cAddress2 = &lcTempCustomer..caddress2 
    m.cAddress3 = &lcTempCustomer..caddress3 
    m.cAddress4 = &lcTempCustomer..caddress4 
    m.cAddress5 = &lcTempCustomer..caddress5
    m.cAddress6 = &lcTempCustomer..caddress6

  	  m.ShipVia = gfCodDes(&lcORDHDR..SHIPVIA, 'SHIPVIA')
      =loScale.SEEK('S'+&lcTempStyle..Scale)
      FOR lnCntSz = 1 TO 8
       lcCntSz = STR(lnCntSz,1)
       m.Size&lcCntSz. = &lcTempScale..Sz&lcCntSz.
      ENDFOR 
      INSERT INTO (lcTmpOrdL)  FROM MEMVAR
   ENDIF   
ENDSCAN    && End of SCAN Loop
SELECT (lcTmpOrdH)
SET RELATION TO
= lfGetTempFiles()

*!*************************************************************
*! Name      : lfGetTempFiles
*: Developer : Mariam Mazhar (MMT)
*! Date      : 08/05/2012
*! Purpose   : function to get the temp files of master files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfGetTempFiles
PRIVATE lcType 
*--NotePad File
SELECT (lcTmpOrdH)
LOCATE 
SCAN 
  loNotePad.SEEK('B'+order)
  SELECT(lcTempNotePad)
  SCATTER MEMO MEMVAR 
  INSERT INTO (lcNotePad) FROM MEMVAR 
ENDSCAN 
*--Ordline file
SELECT (lcTmpOrdH)
LOCATE 
SCAN 
  loOrdLine.Seek('O'+ &lcTmpOrdH..order + &lcTmpOrdH..Store)
  SELECT(lcTempOrdLine)
   SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+ &lcTmpOrdH..order + &lcTmpOrdH..Store
    SCATTER MEMVAR MEMO
    INSERT INTO (lcOrdLnTmp) FROM MEMVAR
  ENDSCAN 
ENDSCAN 

*--Customer file
SELECT (lcPiktktTemp)
LOCATE 
SCAN 
  IF EMPTY(Store)
    lcType = 'M'
    loCustomer.Seek('M'+ Account + Store)
  ELSE
    lcType = 'S'
    loCustomer.Seek('S'+ Account + Store)
  ENDIF 
  
  IF !SEEK(lcType + &lcPiktktTemp..Account + &lcPiktktTemp..Store,lcCustomer)
    SELECT(lcTempCustomer)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcCustomer) FROM MEMVAR 
    IF !EMPTY(m.Dist_ctr) AND  !SEEK(lcType + m.Dist_ctr,lcCustomer)
      IF loCustomer.Seek('S'+ &lcPiktktTemp..Account + m.Dist_ctr)
        SELECT(lcTempCustomer)
        SCATTER MEMO MEMVAR 
        INSERT INTO (lcCustomer) FROM MEMVAR 
      ENDIF 
    ENDIF 

  ENDIF   
  
ENDSCAN 

*--Style File
SELECT(lcTmpOrdL)
LOCATE 
SCAN 
  loStyle.Seek(Style)
  SELECT(lcStyleFile)
  IF !SEEK(&lcTempStyle..Style,lcStyleFile)
    SELECT(lcTempStyle)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcStyleFile) FROM MEMVAR 
  ENDIF 
ENDSCAN 

*--Scale File
SELECT(lcStyleFile)
LOCATE 
SCAN 
  loScale.Seek('S'+ Scale)
  IF !SEEK('S'+ &lcTempScale..Scale,lcScalefile)
    SELECT(lcTempScale)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcScaleFile) FROM MEMVAR 
  ENDIF   
ENDSCAN 

*--WAREHOUS FILE
SELECT(lcPiktktTemp)
LOCATE 
SCAN 
  loWareHous.Seek(&lcPiktktTemp..cWareCode)
  IF !SEEK(&lcTempWareHous..cWareCode,lcWareHous)
    SELECT(lcTempWareHous)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcWareHous) FROM MEMVAR 
  ENDIF   
ENDSCAN 
*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*! Date      : 08/05/2012
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


*!**************************************************************************
*! Name      : lfvPath
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/05/2012
*! Purpose   : Validate Path/File Name
*!************************************************************************** 
FUNCTION lfvPath
IF !EMPTY(lcRpPath) AND '?' $ lcRpPath  
  lcRpPath = GETFILE('CSV','Select the Output File Location and Name')
ENDIF 
RETURN .T.
*!**************************************************************************
*! Name      : lfvPath
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/05/2012
*! Purpose   : Export to csv file
*!************************************************************************** 
FUNCTION lfExportCSV
IF !USED('STYLEUPC')
  =gfOpenTable('STYLEUPC','STYLEUPC')
ENDIF 
= FPUTS( lnOutFile ,"Sales Order#,Customer Code,Location Code,Line Number,Store Number,Customer PO #,Style-color-scl,Description 1,Start Date,Complete Date,"+;
					"Picked Qty,UPC,Size-1,Size-2,Picking Ticket #,Picking Date,Ship to Name,Ship to Address 1,Ship to Address 2,Ship to Address 3,"+;
					"Ship to Address 4,Ship to Address 5,Ship to Address 6,Ship Via")
lcFileLine = ""
SELECT (lcTmpOrdL)
LOCATE
SCAN FOR !EMPTY(Style)
  FOR lnCnt = 1 TO &lcScaleFile..cnt
    lccCnt = STR(lnCnt,1)
    IF Pik&lccCnt. > 0
      =gfSeek(&lcTmpOrdL..Style+PADR(lccCnt,2),'STYLEUPC','STYLEUPC')
      lcFileLine = ""+Order+","+Account+","+cWarecode+","+STR(LineNo,6)+","+Store+","+Custpo+","+Style+","+Desc1+","+DTOC(Start)+","+DTOC(Complete)+","+;
			       STR(Pik&lccCnt.,5)+","+StyleUPC.CUPCNUM1+StyleUPC.CUPCNUM2+StyleUPC.CUPCNUM3+","+&lcScaleFile..cDim1+","+&lcScaleFile..Sz&lccCnt.+","+PIKTKT+","+DTOC(pikdate)+","+;
			       Stname+","+caddress1+","+caddress2+","+caddress3+","+caddress4+","+caddress5+","+caddress6+","+ShipVia+""
      = FPUTS( lnOutFile ,lcFileLine)
    ENDIF 
  ENDFOR
ENDSCAN 




