************************************************************************
*: Program file  : ARPINVRZ.PRG (C# 101361)
*: Program desc. : PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" - 8 SIZE SCALES
*:               : (Cust.: ROBYN MEREDITH)
*:               : Convert INV810z from 2.6 to 2.7
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls : FUNCTIONS  : gfGetZone,lfGetSzScl,lfPrnHdr,
*:                      lfAdrShift,lfGetColor,gfGetMemVar,
*:                      gfItemMask,gfPhoneTem,gfGetAdr,
*:                      gfModalGen,gfTempName,gfOpenFile
*:                      gfRltFld,gfCodDes
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*B605581,1 SSE 03/20/2002 Fix bug of removing the "@R" prior to the phone #.
lcNonMajTl = ''
lcNonMajPi = ''
STORE 0 TO lnFreeLen , lnColorLen , lnMajSeg,lnNonMajSt
lnMajLen = LEN(gfItemMask('PM'))
= lfGetColor()
llNote_Loop = .F.
llPrnSku    = .F.
llM_Tax     = (gfGetMemVar('M_TAX') = 'Y')
lcUpsFrom   = gfGetMemVar('XUPSFROM')
DECLARE laCompAdd[6,1] , laSoldTo[5,1] , laShipTo[5,1] , lacUps[1,2],;
laDivLName[1,2]
lcCups  = SPACE(10)
lcDivLName = ''        && Variable to hold the Division long name
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
lacUps[1,1] = 'CUPS      '
lacUps[1,2] = 'lcCups'
llTAX_DESC = gfGetMemVar('M_TAX_DESC')
lcTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr  = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
RELE ALL LIKE M_*
SELECT SYCCOMP
SEEK gcAct_Comp
*:C101361,1 SSH  Variable to hold the Company Name
lcCompName = cCom_Name             
*:C101361,1 SSH  Variable to hold the Company Phone
lcCompPhon = cCom_Phon
*:C101361,1 SSH Variable to hold the Company Phone Format
lcPhonPict = gfPhoneTem()
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6] = TRANSFORM(lcCompPhon , lcPhonPict)
=lfAdrShift('laCompAdd')
llNewDoc = .T.
lnMaxRow = 47
STORE TRIM(laCompAdd[1])   TO HLINE2
STORE TRIM(laCompAdd[2])   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4
STORE lcCompPhon           TO HLINE5
STORE '' TO lcRemtnc1, lcRemtnc2, lcRemtnc3 ,lcRemtnc4
lnNotLine = 1
INVHTEMP = gfTempName()
SELECT INVHDR
COPY ALL FOR &lcRpExp TO &gcWorkDir.&INVHTEMP
= gfOpenFile('&gcWorkDir.&INVHTEMP',' ','EX')
SELECT (INVHTEMP)
GOTO TOP
IF EOF()
  *:C101333,1 SSH  Text "No Record Selected to display."
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
llLineUp  = gfModalGen('QRM40145B40012','DIALOG' ) = 1
llPrnSku  = gfModalGen('QRM40142B40000','DIALOG' ) = 1
*:C101361,1 SSH  MAIN LOOP
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
XMODE  = 'M'
DO WHILE INKEY() <> 32
  IF XMODE  = 'M' .OR. XMODE = 'Y'
    SELECT (INVHTEMP)
    IF FLAG = 'N'
      SKIP
      LOOP
    ENDIF
    IF EOF()
      EXIT
    ENDIF
    lcInvoice = INVOICE
  ENDIF
  IF llNewDoc
    lnPrtErr = 0
    STORE 0.00 TO lnPieces, lnSubTotal
    SELECT INVHDR
    IF !SEEK(lcInvoice)
      SELECT (INVHTEMP)
      SKIP
      LOOP
    ENDIF
    llNewDoc  = .F.
    lcOrder   = ORDER
    lcPikTkt  = PIKTKT
    lcAccount = ACCOUNT
    lcStore   = STORE
    =gfRltFld(INVHDR.cDivision , @laDivLName , 'CDIVISION')

    SELECT CUSTOMER
    = SEEK('M'+lcAccount)
    llSeek = !EMPTY(SkuTmpl) AND SEEK('S'+SkuTmpl,'SkuTmpl')
    = SEEK (IIF(lcStore= SPACE(8),'M'+lcAccount,'S'+lcAccount+lcStore))
    lcSolTName  = CUSTOMER.BTName
    laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
    laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
    laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
    laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
    laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
    = lfAdrShift('laSoldTo')
    SELECT ORDHDR
    = SEEK('O'+lcOrder)
    IF ORDHDR.Alt_ShpTo
      lcShpTName  = ORDHDR.STName
      laShipTo[1] = ORDHDR.cAddress1
      laShipTo[2] = ORDHDR.cAddress2
      laShipTo[3] = ORDHDR.cAddress3
      laShipTo[4] = ORDHDR.cAddress4
      laShipTo[5] = ORDHDR.cAddress5
    ELSE
      IF INVHDR.CONSOL = 'Y'       
        SELECT CONSINVH
        = SEEK(lcInvoice)
        SELECT CUSTOMER
        = SEEK('S'+lcAccount+CONSINVH->STORE)
      ENDIF
      SELECT CUSTOMER
      lcShpTName  = IIF( EMPTY(DBA) , CUSTOMER.STName , DBA)
      laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
      laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
      laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
      laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
      laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
    ENDIF
    =lfAdrShift('laShipTo')
    SELECT INVLINE
    =SEEK(lcInvoice)
    IF EOF()
      lnPrtErr = 2
    ENDIF
*:C101361,1 SSH  GET THE DESCRIPTION ABOUT THE CODES
    PTERMS   = gfCodDes(INVHDR->CTERMCODE,'CTERMCODE')
    PSHIPVIA = gfCodDes(INVHDR->SHIPVIA,'SHIPVIA')
    = gfRltFld(INVHDR.SHIPVIA , @lacUps , 'SHIPVIA')
    
    DO CASE
      CASE 'G' $ lcCUPS
        
        *B605581,1 No need to call gfGetZone. [Begin]
        *XZN = gfGetZone(lccUps,lcUpsFrom,SUBSTR(CUSTOMER->STZIP,1,3))
        XZN = laCups[1,2]
        *B605581,1 No need to call gfGetZone. [End]
            
        XZN = IIF(!EMPTY(XZN),'('+XZN+')',XZN)        
      CASE '2' $ lcCUPS
        XZN  = '(12)'
      CASE 'N' $ lcCUPS
        XZN  = '(22)'
      OTHERWISE
       XZN  = ''
    ENDCASE    
    PSHIPVIA = TRIM(PSHIPVIA)+XZN
    PSPCINST = gfCodDes(INVHDR->SPCINST,'SPCINST')
    PDIVISION = IIF(!EMPTY(lcDivLName), gfCodDes(INVHDR->CDIVISION,'CDIVISION') , '')

    lcCurLoop = '1'
    STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
    IF !EMPTY(INVHDR.cFacCode)
      lcRemtnc1 ='*PAYABLE TO HELLER FINANCIAL,INC., *'
      lcRemtnc2 ='*P.O. BOX 7247-8292 PHILADELPHIA,PA*'
      lcRemtnc3 ='*19170-8292 TO WHOM THIS INVOICE IS*'
      lcRemtnc4 ='*ASSIGNED. REMITTANCE IS TO BE MADE*'
      lcRemtnc5 ='*ONLY TO THEM.                     *'
    ELSE  
      DO CASE 
        CASE gcAct_Comp = "01" OR gcAct_Comp = "03"
          lcRemtnc1 ='*PAYABLE TO : '+ SUBSTR(qCompany,1,21)+'*'
          lcRemtnc2 ='*Robyn Meredith Inc                *'
          lcRemtnc3 ='*P.O. BOX 7780-4769                *'
          lcRemtnc4 ='*PHILADELPHIA,PA 19182-4769        *'
        CASE gcAct_Comp ="07"
          lcRemtnc1 ='*PAYABLE TO : ' + SUBSTR(qCompany,1,22)+'*'
          lcRemtnc2 ='*HEIRLOOMS CLOTHING, Inc.           *'
          lcRemtnc3 ='*P.O. BOX 7780-4767                 *'
          lcRemtnc4 ='*PHILADELPHIA,PA 19182-4767         *'
        OTHERWISE
          lcRemtnc1 = '*'+PADR(qCompany,34,' ')+'*'         &&&30
          lcRemtnc2 = '*'+PADR(laCompAdd[1],34,' ')+'*'          &&&30
          lcRemtnc3 = '*'+PADR(laCompAdd[2],34,' ')+'*'          &&&30
          lcRemtnc4 = '*'+PADR(laCompAdd[3],34,' ')+'*'          &&15 3 10
      ENDCASE
    ENDIF
  ENDIF
*:C101361,1 SSH END llNewDoc
  IF lnPrtErr >0
    IF XMODE   = 'M' .OR. XMODE = 'Y'
      SELE (INVHTEMP)
      SKIP
      llNewDoc = .T.
      LOOP
    ELSE
      ? CHR(7)
      = gfModalGen('QRM40144B00000','DIALOG' )
      RETURN
    ENDIF
  ENDIF
  =lfPrnHdr()
  IF !llNote_Loop
    lcScalLn1 = SPACE(1)
    lcScalLn2 = SPACE(1)
    lcScalLn3 = SPACE(1)
    llNoThing = lfGetSzScl()
    @ 22, 20 SAY lcScalLn1
    @ 23, 20 SAY lcScalLn2
    @ 24, 20 SAY lcScalLn3
  ENDIF
  SELECT INVLINE
  lcStore = STORE
  ROW    = 27
  DO WHILE lcCurLoop = '1' .AND. !llNote_Loop           
     SELECT INVLINE
     IF EOF() .OR. INVOICE <> lcInvoice .OR. ROW >= lnMaxRow
        EXIT
     ENDIF
     IF TOTQTY = 0
        SKIP
        LOOP
     ENDIF
     KEY = INVLINE->STYLE
     SELECT INVLINE
     @ ROW,00 SAY SUBSTR(STYLE,1,lnMajLen)
     @ ROW,13 SAY SUBSTR(STYLE,lnNonMajSt,lnColorLen)
     @ ROW,20 SAY Scale
     = SEEK('S'+Scale,'Scale')
     FOR lnCnt = 1 TO 8
       Z = STR(lnCnt,1)
       @ ROW, 17+(5*lnCnt) SAY QTY&Z.  PICTURE '@Z 9999'
     ENDFOR                  
     @ ROW,62 SAY TOTQTY PICTURE '999999'
     lnLineTot   = PRICE * TOTQTY
     lnPieces    = lnPieces + TOTQTY
     lnSubTotal  = lnSubTotal+lnLineTot
     @ ROW,69  SAY PRICE     PICTURE '999.99'      
     @ ROW,75  SAY lnLineTot  PICTURE '999999.99'
     ROW = ROW + 1      
     llFlag = .T.
     llMainSku  = .T.
     llSkuSzRow = .T.
     IF SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
       lnSel = SELECT()
       SELECT Spck_Lin
       IF llPrnSku
         DIMENSION laSku[8,2]
         laSku = CHR(255)
         lnCount = 0
         lnQtyn  = 1
         SCAN REST WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style
           FOR lnQtyn=1 TO 8
             IF EVAL('Spck_Lin.Qty'+ALLTRIM(STR(lnQtyn))) = 1
               lnCount = lnCount + 1
               laSku(lnCount,1) = STR(lnQtyn)
               laSku(lnCount,2) = IIF(!EMPTY(Spck_Lin.Sku),Spck_Lin.Sku,Spck_Lin.Pack_Id)
               EXIT
             ENDIF
           ENDFOR
         ENDSCAN
         = ASORT(laSku,1)
         FOR lnQtyn = 1 TO lnCount
           IF EVAL('InvLine.Qty'+ALLTRIM(laSku[lnQtyn,1])) <> 0
             IF ROW >= lnMaxRow
               ENDPAGE = IIF(INVLINE->INVOICE = lcInvoice ,'1','0')
               IF ENDPAGE = '1'
                 @ ROW+1,12 SAY 'C O N T I N U E D ...'
                 @ 51,55 SAY 'MERCHANDISE'
                 @ 51,75 SAY '******'
                 ROW = ROW+1
                 @ 52,07 SAY INVHDR->NOTE1
                 @ 53,07 SAY INVHDR->NOTE2
                 @ 58,18 SAY INVHDR->CARTONS  PICTURE '@Z 999'
                 @ 58,62 SAY '******'
                 @ 58,75 SAY '******'
               ENDIF
               =lfPrnHdr()
                @ 22, 20 SAY lcScalLn1
                @ 23, 20 SAY lcScalLn2
                @ 24, 20 SAY lcScalLn3
                ROW    = 27
               SELECT Spck_Lin
             ENDIF
           ENDIF
             IF llMainSku
               @ ROW,0 SAY 'SKU#: '+IIF((UPPER(SkuTmpl.cSkuCode)='JCP' OR UPPER(SkuTmpl.cSkuCode)='MER') AND llSeek,LEFT(ALLTRIM(laSku[lnQtyn,2]),;
               SkuTmpl.Len1)+' '+SUBSTR(ALLTRIM(laSku[lnQtyn,2]),SkuTmpl.Len1+1,SkuTmpl.Len2),;
               IIF(llSeek,LEFT(laSku[lnQtyn,2],SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3),;
               LEFT (laSku[lnQtyn,2],8)))
               llMainSku = .F.
             ENDIF
             IF SEEK(KEY,'Style') AND llFlag
               @ ROW , 30 SAY Style.Desc
               llFlag = .F.
             ENDIF
             IF llSkuSzRow
               ROW = ROW + 1
               COL = 30
               llSkuSzRow = .F.
             ENDIF
             lnStrWidth = LEN(ALLTRIM(EVAL('Scale.Sz'+ALLTRIM(laSku[lnQtyn,1])))+':')
             lnMaxWidth = lnStrWidth + IIF(llSeek,SkuTmpl.Len4,8)
             IF ROW = 47
               ENDPAGE = IIF(INVLINE->INVOICE = lcInvoice ,'1','0')
               IF ENDPAGE = '1'
                 @ ROW+1,12 SAY 'C O N T I N U E D ...'
                 @ 51,55 SAY 'MERCHANDISE'
                 @ 51,75 SAY '******'
                 ROW=ROW+1
                 @ 52,07 SAY INVHDR->NOTE1
                 @ 53,07 SAY INVHDR->NOTE2
                 @ 58,18 SAY INVHDR->CARTONS  PICTURE '@Z 999'
                 @ 58,62 SAY '******'
                 @ 58,75 SAY '******'
               ENDIF
               =lfPrnHdr()
                @ 22, 20 SAY lcScalLn1
                @ 23, 20 SAY lcScalLn2
                @ 24, 20 SAY lcScalLn3
                ROW    = 27
               SELECT Spck_Lin
             ENDIF
             IF  COL + lnMaxWidth < 80
               @ ROW,COL SAY ALLTRIM(EVAL('Scale.SZ'+ALLTRIM(laSku[lnQtyn,1])))+":"+;
                         ALLTRIM(SUBSTR(laSku[lnQtyn,2],IIF(llSeek,SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3+1,9),IIF(llSeek,SkuTmpl.Len4,8)))
                COL = COL + lnStrWidth + LEN(ALLTRIM(SUBSTR(laSku[lnQtyn,2],IIF(llSeek,SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3+1,9),IIF(llSeek,SkuTmpl.Len4,8))))+ 1
             ELSE
               ROW = ROW +  1
               IF ROW >= lnMaxRow
                 ENDPAGE = IIF(INVLINE->INVOICE = lcInvoice ,'1','0')
                 IF ENDPAGE = '1'
                   @ ROW+1,12 SAY 'C O N T I N U E D ...'
                   @ 51,55 SAY 'MERCHANDISE'
                   @ 51,75 SAY '******'
                   ROW=ROW+1
                   @ 52,07 SAY INVHDR->NOTE1
                   @ 53,07 SAY INVHDR->NOTE2
                   @ 58,18 SAY INVHDR->CARTONS  PICTURE '@Z 999'
                   @ 58,62 SAY '******'
                   @ 58,75 SAY '******'
                 ENDIF
                 =lfPrnHdr()
                 @ 22, 20 SAY lcScalLn1
                 @ 23, 20 SAY lcScalLn2
                 @ 24, 20 SAY lcScalLn3
                 ROW    = 27
                 SELECT Spck_Lin
               ENDIF
               COL = 30
                @ ROW,COL SAY ALLTRIM(EVAL('Scale.SZ'+ALLTRIM(laSku[lnQtyn,1])))+":"+;
                          ALLTRIM(SUBSTR(laSku[lnQtyn,2],IIF(llSeek,SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3+1,9),IIF(llSeek,SkuTmpl.Len4,8)))
                COL = COL + lnStrWidth + LEN(ALLTRIM(SUBSTR(laSku[lnQtyn,2],IIF(llSeek,SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3+1,9),IIF(llSeek,SkuTmpl.Len4,8)))) + 1
             ENDIF
         ENDFOR
         ROW = ROW + 1
       ELSE
         IF ROW >= lnMaxRow
           =lfPrnHdr()
           SELECT Spck_Lin
         ENDIF          
         @ ROW,0 SAY 'SKU#: '+IIF((UPPER(SkuTmpl.cSkuCode)='JCP' OR UPPER(SkuTmpl.cSkuCode)='MER') AND llSeek,LEFT(ALLTRIM(Spck_Lin.Pack_Id),;
                     SkuTmpl.Len1)+' '+SUBSTR(ALLTRIM(Spck_Lin.Pack_Id),SkuTmpl.Len1+1,SkuTmpl.Len2),;
                     IIF(llSeek,LEFT(Spck_Lin.Pack_Id,SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3),;
                     LEFT (Spck_Lin.Pack_Id,8)))
         IF SEEK(KEY,'Style') AND llFlag
            @ ROW , 30 SAY Style.Desc
            llFlag = .F.
          ENDIF          
         ROW = ROW + 1
       ENDIF
       SELECT (lnSel)       
     ELSE
       IF SEEK(KEY,'Style')
         IF ROW >= lnMaxRow
           =lfPrnHdr()
           SELECT Spck_Lin
         ENDIF
         @ ROW , 0 SAY Style.Desc
         ROW = ROW + 1
       ENDIF
     ENDIF
     SELE INVLINE
     IF llLineUp
        EXIT
     ENDIF
     SELECT INVLINE
     SKIP
  ENDDO
  SET DEVICE TO PRINT
  IF llLineUp .AND. !llNote_Loop
     EJECT
     lnChoice = gfModalGen('QRM40143B40012','DIALOG' )
     DO CASE
        CASE lnChoice = 3
          RETURN
         CASE lnChoice = 2
          llLineUp =.F.
     ENDCASE
     SET DEVICE TO PRINT
     STORE 0.00 TO lnPieces, lnSubTotal
     LOOP
  ENDIF
  ENDPAGE = IIF(INVLINE->INVOICE = lcInvoice ,'1','0')
  IF ENDPAGE = '1' .AND. !llNote_Loop
     @ ROW+1,12 SAY 'C O N T I N U E D ...'
     SELECT INVHDR
     @ 51,55 SAY 'MERCHANDISE'
     @ 51,75 SAY '******'
     ROW=ROW+1
     @ 52,07 SAY INVHDR->NOTE1
     @ 53,07 SAY INVHDR->NOTE2
     @ 58,18 SAY INVHDR->CARTONS  PICTURE '@Z 999'
     @ 58,62 SAY '******'
     @ 58,75 SAY '******'
     LOOP
  ENDIF
  SELECT NOTEPAD
  lnOldMemW = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  IF TYPE + KEY <> 'C' + lcInvoice
    = SEEK('C' + lcInvoice)
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  ENDIF
  IF TYPE + KEY = 'C' + lcInvoice
    @ ROW,02 SAY '* -- N O T E S -- *' 
    ROW = ROW + 1 
    DO WHILE lnNotLine <= lnMemLins
      IF ROW >= lnMaxRow
        llNote_Loop = .T.
        EXIT
      ELSE
        llNote_Loop = .F.
        @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
        ROW = ROW + 1
      ENDIF
      lnNotLine = lnNotLine + 1
    ENDDO
    IF !llNote_Loop
      @ ROW,02 SAY '* -- END OF NOTES -- *'
      lnNotLine = 1
      ROW = ROW + 1 
    ELSE
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      SELECT INVHDR
      @ 51,55 SAY 'MERCHANDISE'
      @ 51,75 SAY '******'
      ROW = ROW + 1
      @ 52,07 SAY INVHDR->NOTE1
      @ 53,07 SAY INVHDR->NOTE2
      @ 58,18 SAY INVHDR->CARTONS PICTURE '@Z 99999'
      @ 58,62 SAY '******'
      @ 58,75 SAY '******'
      LOOP
    ENDIF
  ENDIF
  SET MEMOWIDTH TO lnOldMemW      
  SELECT INVHDR
  @ 51,55 SAY 'MERCHANDISE'
  @ 51,74 SAY lnSubTotal    PICTURE '9999999.99'  
  ROW=52
  @ 52,07 SAY INVHDR->NOTE1   
  llNotePrnt = .F.  
  IF llM_Tax .AND. lcTAX_METH = 'M'      
    IF !EMPTY(lcTaxRefr)
      @ Row,49 SAY lcTaxRefr
      Row = Row + 1
    ENDIF        
  ENDIF  
  IF ROW = 53
    @ ROW,07 SAY INVHDR->NOTE2
    llNotePrnt = .T.
  ENDIF           
  IF llM_Tax .AND. lcTAX_METH = 'M'      
    XSTRING_RATE = ALLTRIM(STR (INVHDR->TAX_RATE,5,2))
    @ ROW ,52 SAY SUBSTR(llTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
    @ ROW ,74 SAY INVHDR->TAX_AMT   PICT '9999999.99'
    ROW = ROW + 1
  ENDIF     
  IF !llNotePrnt AND ROW=53
    @ ROW, 07 SAY INVHDR->NOTE2
    llNotePrnt = .T.   
  ENDIF
  lnWKAmt = FREIGHT + INSUR + COD
  IF lnWKAmt <> 0
     @ ROW,55 SAY 'TOTAL - FREIGHT'
     @ ROW,74 SAY lnWKAmt       PICTURE '9999999.99'  
     ROW=ROW+1
  ENDIF
   IF !llNotePrnt AND ROW = 53
    @ ROW,07 SAY INVHDR->NOTE2
    llNotePrnt =.T.
  ENDIF
  IF DISCOUNT<>0
     @ ROW,55 SAY 'DISCOUNT'
     @ ROW,73 SAY DISCOUNT    PICTURE  '99999999.99'   
     ROW = ROW + 1
  ENDIF
  IF !llNotePrnt
    @ 53,07 SAY INVHDR->NOTE2
    llNotePrnt =.T.
  ENDIF
  IF llM_Tax .AND. lcTAX_METH = 'A'
    @ 54,49 SAY lcTaxRefr                  
    XSTRING_RATE = ALLTRIM(STR (INVHDR->TAX_RATE,5,2))
    @ 55 ,52 SAY SUBSTR(llTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
    @ 55 ,74 SAY INVHDR->TAX_AMT   PICT '9999999.99'
  ENDIF  
  @ 58,6 SAY INVHDR->CARTONS    PICTURE '@Z 99999'
  @ 58,62 SAY lnPieces          PICTURE '9999999'
  @ 58,74 SAY INVHDR->TOTALCHG  PICTURE '9999999.99'
  IF XMODE = 'N'
     EXIT
  ENDIF
  SELECT INVHDR
  =SEEK(lcInvoice)
  REPLACE PRTFLAG WITH 'P'
  SELECT (INVHTEMP)
  SKIP
  llNewDoc = .T.
ENDDO
DO ENDREPORT

*B605581,1 SSE Commented out. [Begin]
*IF USED('SysZones')
*  USE IN SysZones
*ENDIF
*B605581,1 SSE Commented out. [End]

RETURN

*!*************************************************************
*! Name      : gfGetZone.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 05/11/1998
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : lcUpsType,lcUpsFrom,lcToZip
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =gfGetZone(lcUpsType,lcUpsFrom,lcToZip)
*!************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip
PRIVATE lnOldWrk

lnOldWrk = SELECT()
= gfOpenFile(gcSysHome+'syszones',gcSysHome+'Frtzones','SH')
SELECT (lnOldWrk)
RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,'syszones'),'',syszones.ZONE)

*!*************************************************************
*! Name      : lfGetSzScl.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 05/11/1998
*! Purpose   : Get the first three size scale used in the invoice 
*!             to be printed on the invoice header.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfGetSzScl()
*!************************************************************
FUNCTION lfGetSzScl
PRIVATE lnAlias, lcInvExp, lnScalNo

lnAlias   = SELECT()
lcInvExp  = InvLine.Invoice + STR(InvLine.LineNo,6)
lcStyExp  = Style.style
lnScalNo  = 1
lcScalStr = ''
SELECT InvLine
IF SEEK(lcInvoice)
  SCAN WHILE Invoice = lcInvoice AND lnScalNo <= 3
    IF SEEK (InvLine.Style, "Style") AND ;
       SEEK ("S"+Style.Scale, "Scale")
       lcVariable  = "lcScalLn" + ALLTRIM(STR(lnScalNo))
       &lcVariable = Style.Scale + SPACE(1)                 +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz1),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz2),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz3),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz4),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz5),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz6),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz7),1,4),5) +;
                     PADL(SUBSTR(ALLTRIM(Scale.Sz8),1,4),5)
       IF Style.Scale $ lcScalStr
         &lcVariable = SPACE(1)
       ELSE
         lcScalStr = lcScalStr + Scale
         lnScalNo  = lnScalNo + 1
       ENDIF
    ENDIF
  ENDSCAN
ENDIF
= SEEK (lcInvExp, "InvLine")
= SEEK (lcStyExp, "Style"  )
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfPrnHdr.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 05/11/1998
*! Purpose   : Print the Header of the page.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfPrnHdr()
*!************************************************************
FUNCTION lfPrnHdr

SELECT INVHDR
IF lcPrnComp='Y'
   HLINE1 = IIF(!EMPTY(lcDivLName), gfCodDes(INVHDR->CDIVISION,'CDIVISION') , lcCompName)
   @ 01,01 SAY HLINE1
   SELECT INVHDR
ENDIF
@ 01,60 SAY lcInvoice
@ 01,71 SAY INVDATE        
IF lcPrnComp='Y'
  @ 02,01 SAY HLINE2
  @ 03,01 SAY HLINE3
ENDIF
@ 03,71 SAY SUBSTR(PDIVISION, 1, 14)
IF lcPrnComp = 'Y'
  IF EMPTY(HLINE5)
    
    *B605581,1 Remove the "@R" prior to the phone #. [Begin]
    *@ 04,01 SAY SUBSTR(HLINE4,1,30) PICTURE "@R "+lcPhonPict SIZE 1,16
    @ 04,01 SAY SUBSTR(HLINE4,1,30) PICTURE lcPhonPict SIZE 1,16
    *B605581,1 Remove the "@R" prior to the phone #. [End]
    
  ELSE
    @ 04,01 SAY SUBSTR(HLINE4,1,30)
  ENDIF
ENDIF
@ 04,30 SAY 'ATTENTION :'
IF lcPrnComp='Y'
  
  *B605581,1 Remove the "@R" prior to the phone #. [Begin]
  *@ 05,01 SAY SUBSTR(HLINE5,1,23)  PICTURE "@R "+lcPhonPict SIZE 1,16
  @ 05,01 SAY SUBSTR(HLINE5,1,23)  PICTURE lcPhonPict SIZE 1,16
  *B605581,1 Remove the "@R" prior to the phone #. [End]
  
ENDIF
@ 05,19 SAY IIF(gcAct_Comp = "07",REPLICATE('*',37),REPLICATE('*',36))
@ 05,71 SAY ORDER
@ 06,19 SAY lcRemtnc1
@ 07,19 SAY lcRemtnc2 
@ 07,71 SAY lcPikTkt        
@ 08,19 SAY lcRemtnc3 
@ 09,19 SAY lcRemtnc4
@ 09,71 SAY APPROVAL
IF !EMPTY(INVHDR.cFacCode)
  @ 10,19 SAY lcRemtnc5
  @ 11,19 SAY REPLICATE('*',36)
ELSE
  @ 10,19 SAY IIF(gcAct_Comp = "07",REPLICATE('*',37),REPLICATE('*',36)) 
ENDIF  
@ 13,08 SAY lcSolTName
@ 13,54 SAY lcShpTName
@ 14,08 SAY laSoldTo[1]
@ 14,54 SAY laShipTo[1]
@ 15,08 SAY laSoldTo[2]
@ 15,54 SAY laShipTo[2]
@ 16,08 SAY laSoldTo[3]+' '+laSoldTo[4]+' '+laSoldTo[5]
@ 16,54 SAY laShipTo[3]+' '+laShipTo[4]+' '+laShipTo[5]
@ 21,02 SAY ACCOUNT
@ 21,12 SAY CUSTPO
@ 21,23 SAY STORE
@ 21,33 SAY DEPT                         
@ 21,42 SAY SUBSTR(PTERMS,1,15)
@ 21,64 SAY REP1                         
@ 21,68 SAY REP2  
@ 21,72 SAY SUBSTR(PSHIPVIA,1,13)
ROW = 23

*!*************************************************************
*! Name      : lfAdrShift.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 05/11/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfAdrShift(Array Name)
*!************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 6
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
    EMPTY(&lcArrayNam.[lnCount])
    = ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfGetColor.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 05/11/1998
*! Purpose   : Function to get color start & end positions.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfGetColor()
*!************************************************************
FUNCTION lfGetColor

lcNonMajTl = ''
lcNonMajPi = ''
*:C101361,1 SSH  No. of major segments.
lnMajSeg    = gfItemMask('SM')
*:C101361,1 SSH  Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*:C101361,1 SSH  Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
*:C101361,1 SSH This item hold seg. start position.
    lnNonMajSt = laMajSegs[lnI,4]
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
*:C101361,1 SSH  If you Find Color Type.
  IF laMajSegs[lnI,1] = 'C'
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen