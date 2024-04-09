*!********************************************************************
*: Program file  : DOR200.PRG
*: Program desc. : A customized pick ticket form for Dorby frocks.
*:         System: ARIA APPAREL SERIES
*:         Module: 
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*!********************************************************************
*: Example            : DO ALDOR200
*!********************************************************************
*! Refer to (C101554)
*!********************************************************************
*:Modifications  :
*:B603509,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.
*:C101868,1   AMH 06/20/2000 Add Option to Print Size Scale
*!********************************************************************

*--Check the date
IF lcRpSelect <> 'P'
  lnDatePos = ASCAN(laOGFxFlt,"PIKTKT.DATE")
  IF lnDatePos > 0
    lnDatePos   = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
    lnPPos      = AT("|",laOGFxFlt[lnDatePos,6])
    ldFrstDate  = IIF(lnPPos <>1,CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10)),{})
    ldSndDate   = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],lnPPos+1,10))
    IF ldSndDate = {}
      *-- Message : Date can not be blank!
      =gfModalGen('TRM44078B40011','ALERT')
     RETURN
    ENDIF  
  ENDIF
ENDIF  
*--Restore the global flags.

*C101868,1   AMH 06/20/2000 (Start)
IF TYPE("llWareHous") = "C"
  *llWareHous  = (gfGetMemVar('M_WareHouse') = 'Y')
  *llStyle12   = (gfGetMemVar('M_Style12') ='Y')
  *llColor6    = (gfGetMemVar('M_Color6') ='Y')    
  DIMENSION laSetups[3,2]
  laSetups[1,1] = 'M_WareHouse'
  laSetups[2,1] = 'M_Style12'
  laSetups[3,1] = 'M_Color6'
  =gfGetMemVar(@laSetups,gcAct_Comp)
  llWareHous  = (laSetups[1,2] = 'Y')
  llStyle12   = (laSetups[2,2] = 'Y')
  llColor6    = (laSetups[3,2] = 'Y')    
ENDIF
*C101868,1   AMH 06/20/2000 (End)

*-- Get the major and nonmajor titles and lengths.
lcMajTitle = ALLTRIM(gfItemMask('HM'))
lnMajorLen = LEN(ALLTRIM(gfItemMask('PM')))
lcNonMajTl = ''
lcNonMajPi = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR
DIMENSION laCompAdd[6]
laCompAdd = ' '
STORE ' ' TO HLINE1,HLINE2,HLINE3,HLINE4,HLINE5
*--Open SYCCOMP file
=gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'Ccomp_id','SH')
SEEK gcAct_Comp
STORE TRIM(cCom_Name) TO HLINE1
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
=lfAdrShift('laCompAdd')
STORE TRIM(laCompAdd[1]) TO HLINE2
STORE TRIM(laCompAdd[2]) TO HLINE3
STORE TRIM(laCompAdd[3]) TO HLINE4
STORE TRIM(cCom_Phon)    TO HLINE5
*--Get the phone format.
lcPhnFrmt= gfPhoneTem()
IF llWareHous
   =gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
ENDIF
SELE PIKTKT
SET RELATION TO 'O'+ORDER INTO ORDHDR, IIF(STORE=SPACE(8),'M'+ACCOUNT,'S'+ACCOUNT+STORE);
 INTO CUSTOMER
lcPikTemp  = gfTempName()
XACCOUNT = SPACE(5)
DIMENSION laAddress[1,1]
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6,laAddress
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6
STORE '' TO lcTermData,lcShipVia,lcSpcInst,lcSeason,lcDivLname
DECLARE laDivLName[1,2]
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
CNT      = 0
STORE SPACE(6) TO LPIKTKT, HPIKTKT
R_WIDTH  = 'N'  && NARROW PRINTOUT
*-- Get the records
SELECT PIKTKT
LOCATE ALL FOR &lcRpExp
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
*-- Collect data
WAIT WINDOW 'Selecting picking tickets. Please wait...' NOWAIT
COPY REST TO (gcWorkDir + lcPIKTEMP) FOR &lcRpExp
=gfOpenFile(gcWorkDir+'&lcPIKTEMP','','EX')
IF OPENED('PIKTKT')
  SELE PIKTKT
  USE
ENDIF
SELE (lcPIKTEMP)
SET RELATION TO 'O'+ORDER INTO ORDHDR, IIF(STORE=SPACE(8),'M'+ACCOUNT,'S'+ACCOUNT+STORE);
 INTO CUSTOMER
GOTO TOP
IF EOF()
   RETURN
ENDIF
STORE 0   TO FORMSCT
STORE .F. TO SAMEDOC
STORE ' ' TO HTERMS,HSHIPVIA,HSPCINST
FLD='PIK'
*----------------------
* SELECT THE DEVICE (PRINTER)
*----------------------
*-- Call the main procedure.
DO ALO820B
*-- End report
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN
*-- end of main report code.

*!*************************************************************
*! Name      : PROCEDURE lpHeader
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/27/1999
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Synopsis : To print the header before each page.  
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
PROCEDURE ALO820B

XDYELOT_S = (gfGetMemVar('M_DYELOT') = 'Y')
llNotes   = (gfGetMemVar('M_LN_NOTE') = 'Y') 
= gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
= gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'PIKTKT','SH')
lcLinTemp = gfTempName()
ROW     = 0
lcStore = ' '
*-- Variable to get next store or not.
llCont  = .F.       
lcCstPO  = ' '
*-- Variable to hold string of line1.
lcStrln1 = ' '   &&B800063,1 Variable to hold string of line1.
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
 A='-------------------------------------------------------------------------------'
 B='  SHIPVIA       | SEASON |SPCL INSTRUCTIONS | STORE#   | DEPT# | PURCH ORDER  |'
 C='       G '+lcMajTitle+'        '+lcNonMajTl+'      DESCRIPTION                  PRICE      AMOUNT'
 D='   _______ ___ ____________________ ___  ___  ___  ___  ___  ___  ___  ___ ____'
 E='|  BILL OF LADING | # CARTONS | WEIGHT| PICKED BY | PACKED BY | SHIPPED VIA   |'
 G='|  MERCHANDISE    |  FREIGHT  | INSUR | OTHER CHGS| TERMS     | COMMENTS:     |'
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*   G STYLE   CLR     DESCRIPTION              DYELOT #         PRICE      AMOUNT
*12 X 1234567 123 12345678901234567890                        1234.99  123,456.99
*     SIZE: 12345 12345 12345 12345 12345 12345 12345 12345
*     ORDR: 12345 12345 12345 12345 12345 12345 12345 12345
*     ALLO: 12345 12345 12345 12345 12345 12345 12345 12345
*     PICK: 12345 12345 12345 12345 12345 12345 12345 12345

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
NEWDOC = .T.
LORDER = &lcPIKTEMP->ORDER
HORDER = &lcPIKTEMP->ORDER
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
XTIME = TIME()
DO WHILE INKEY() <>32
   WAIT WINDOW 'PRINT PICK TICKETS - <Space Bar> TO ABORT' NOWAIT
   SELECT (lcPIKTEMP)
   IF EOF()
      EXIT
   ENDIF
   IF FLAG = 'N'
      SKIP
      LOOP
   ENDIF
   *----------------------------
   * GET ORDERHDR & LINE ITEMS
   *----------------------------
   IF NEWDOC
      SELECT (lcPIKTEMP)
      XPIKTKT  = PIKTKT
      XORDER   = ORDER
      XDATE    = DATE
      XSTORE   = STORE
      LORDER   = IIF(XORDER<LORDER, XORDER, LORDER)
      HORDER   = IIF(XORDER>HORDER, XORDER, HORDER)
      IF llWareHous .AND. SEEK(&lcPIKTEMP->cWareCode,'WareHous')
        lcWDesc = 'WARHSE: '+ WareHous.cWareCode+ ' '+SUBSTR(WareHous.cDesc,1,18)
      ELSE
        lcWDesc = SPACE(1)
      ENDIF
      SELECT ORDHDR
      XMULTI   = MULTI
      XACCOUNT = ACCOUNT
      SELECT CUSTOMER
      STORE 0.00 TO XORDTOT, XVALUE , SVALUE
      SELECT ORDLINE
      ******
      ** We added the STORE# to use the new created index (ORDLINST) which
      ** consists of ORDER+STORE+STYLE+COLOR+STR(LINENO,6) in order to make
      ** the search faster because one PIKTKT can't exceed one STORE. This
      ** will work both form single or multi store orders. In single store
      ** order, either the store will be empty or have something and all
      ** the lines will have the same store. So this change will not improve
      ** anything on the single store orders but it will on big multistore
      ** orders. And in most cases the single store orders are not that big
      ** anyway.
      ******
      SEEK 'O'+XORDER+XSTORE                && ARH 12/07/93 Added STORE
      IF ORDHDR->MultiPO       &&TAK 08/22/94
        lcCustPO=CustPO
      ENDIF 
      IF PIKTKT <> XPIKTKT
         LOCATE REST FOR PIKTKT=XPIKTKT;
                     WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+XORDER+XSTORE
      ENDIF
      IF ORDER+STORE <> XORDER+XSTORE        && ARH 12/07/93 Added STORE
         SELECT &lcPIKTEMP
         SKIP
         LOOP
      ENDIF
      IF OPENED(lcLinTemp)
        SELECT &lcLinTemp
        USE
      ENDIF
      SELECT ORDLINE
      COPY REST TO gcWorkDir+'&lcLinTemp' FOR PIKTKT=XPIKTKT;
                                WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+ XORDER+XSTORE
      =gfOpenFile(gcWorkDir+'&lcLinTemp','','EX')
      INDEX ON ORDER+STR(LINENO,6) TAG &lcLinTemp        && ARH 09/09/93
      GO TOP
      *--Get the BT address
      STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
      lcBtName  = CUSTOMER.BTNAME
      =gfGetAdr('Customer','','','',1,'2')
      FOR lnCount = 1 TO ALEN(laAddress,1)
        lcCount = STR(laAddress[lnCount,1],1)
        
        *:C101868,1   AMH 06/20/2000 Delete Spaces (Start)
        *lcBtAdd&lcCount = lcBtAdd&lcCount + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
            SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
        lcBtAdd&lcCount = lcBtAdd&lcCount + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
            ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
        *:C101868,1   AMH 06/20/2000 Delete Spaces (End)
        
      ENDFOR
      *------------------------------------
      * GET DESCRIPTIONS FOR CODED FIELDS
      *------------------------------------
      SELECT CODES
      *--Terms
      lcTermData = gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE')
      *--ShipVia
      lcShipVia = gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA')
      *--Division desc.
      lcDivison = SUBSTR(gfCodDes(OrdHdr.CDIVISION , 'CDIVISION'),1,13)
      *--Special instruction
      lcSpcInst = gfCodDes(OrdHdr.SPCINST , 'SPCINST')
      *---Season
      lcSeason = gfCodDes(OrdHdr.SEASON , 'SEASON')
      *-- Division long name
      =gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
      **** added 2/12/92 BY FA *****
      HLINE1 = IIF(!EMPTY(lcDivLname),TRIM(lcDivLname),HLINE1)
      STORE "" TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4
      lnPieces = 0
   ENDIF
   *--------------------------------------------
   * SHIP-TO ADDRESS FOR THIS STORE
   *--------------------------------------------
   *-- Initialize the alt address if ther is any. 04/07/94 MFM.
   SELE OrdHdr
   SEEK 'O'+XOrder
   *--Variable to hold the distribution 
   lcDistCntr = ""
   STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6
   IF OrdHdr.Alt_ShpTo
     lcStName = ALLT(STName)
     lcStAdd1 = cAddress1
     lcStAdd2 = cAddress2
     lcStAdd3 = SUBSTR(cAddress3,1,15)+ ',' +;
                SUBSTR(cAddress4,1,3)+ ',' + SUBSTR(cAddress5,1,10)
   ELSE
     IF !EMPTY(CUSTOMER.Dist_Ctr)
       lcDistCntr = CUSTOMER.Dist_Ctr
       =SEEK("S"+CUSTOMER.Account+CUSTOMER.Dist_Ctr,"CUSTOMER")
     ENDIF
     lcStName  = IIF(EMPTY(Customer.DBA),ALLT(Customer.STNAME),ALLT(Customer.DBA))
     =gfGetAdr('CUSTOMER','','','',1,'')
     *--Get the Ship To adddess except the country.    
     FOR lnCount = 1 TO ALEN(laAddress,1)
       lcCount = STR(laAddress[lnCount,1],1)
       
       *:C101868,1   AMH 06/20/2000 Delete Spaces (Start)
       *lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
       SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
       lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
       ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
       *:C101868,1   AMH 06/20/2000 Delete Spaces (Start)
       
     ENDFOR  
   ENDIF
   =lfPrntHdr()

   SELECT (lcLinTemp)
   NEWDOC     = .T.
   XSTORE     = STORE
   XTOTQTY    = 0
   
   *:C101868,1   AMH 06/20/2000 Add Variable to Save Old Scale (Start)
   lcOldScale = SPACE(3)
   *:C101868,1   AMH 06/20/2000 Add Variable to Save Old Scale (End)

   DO WHILE !EOF()
      DO CASE
         CASE EOF()
           NEWDOC = .T.
           EXIT
         CASE STORE # XSTORE
           NEWDOC = .F.
           EXIT
         CASE TOTPIK<=0
           SKIP
           LOOP
         CASE ROW>=47
           NEWDOC=.F.
           EXIT
      ENDCASE
      *-- TEST PAGE OVERFLOW
      KEY = STYLE
      SELECT STYLE
      SEEK KEY
      XSTYDESC =  DESC
      SELECT (lcLinTemp)
      @ ROW,00 SAY LINENO 
      @ ROW,07 SAY GROUP
      @ ROW,09 SAY STYLE
      @ ROW,29 SAY SUBSTR(XSTYDESC,1,19)
      IF XDYELOT_S
        @ ROW,49 SAY DyeLot
      ENDIF

      IF llRpStyPr
         XAMOUNT = PRICE*TOTPIK
         @ ROW,60 SAY PRICE        PICTURE '9999.99'
         @ ROW,69 SAY XAMOUNT      PICTURE '999,999.99'
      ENDIF
      ROW = ROW+1

      *:C101868,1   AMH 06/20/2000 Check if Print Size Scale or Not (Start)
      IF llRpSizePr  && If Print Size Scale Yes
        *:C101868,1   AMH 06/20/2000 Print Scale only if shanged (Start)
        IF lcOldScale <> SCALE   && Print Scale in First time Only
          XSCALE = GETSCALE(SCALE,"|")
          @ ROW,12 SAY STRTRAN(XSCALE,"|"," ")
          ROW =ROW+1
          lcOldScale = SCALE
        ENDIF
        *:C101868,1   AMH 06/20/2000 Print Scale only if shange (End)
        @ ROW,05 SAY 'ORDR:'
        *:C101868,1   AMH 06/20/2000 Print Qty of existing Size only (Start)
        *@ ROW,12 SAY QTY1     PICTURE '99999'
        *@ ROW,18 SAY QTY2     PICTURE '99999'
        *@ ROW,24 SAY QTY3     PICTURE '99999'
        *@ ROW,30 SAY QTY4     PICTURE '99999'
        *@ ROW,36 SAY QTY5     PICTURE '99999'
        *@ ROW,42 SAY QTY6     PICTURE '99999'
        *@ ROW,48 SAY QTY7     PICTURE '99999'
        *@ ROW,54 SAY QTY8     PICTURE '99999'

        *SIZE1 SIZE2 SIZE3 SIZE4 SIZE5 SIZE6 SIZE7 SIZE8
        PRIVATE laScales
        DIMENSION laScales[1]
        =gfSubStr(XSCALE,@laScales,"|")
        IF !EMPTY(laScales[1])
          lnSzCol = 12
          FOR lnSize = 1 TO ALEN(laScales,1)
            lcSize = STR(lnSize,1)
            @ ROW,lnSzCol SAY QTY&lcSize PICTURE '99999'
            lnSzCol = lnSzCol + LEN(laScales[lnSize]) + 1
          ENDFOR
        ENDIF
        *:C101868,1   AMH 06/20/2000 Print Qty of existing Size only (End)
        
        @ ROW,61 SAY TOTQTY   PICTURE '999999'
        *-- CUM ORDER TOTALS
        XTOTQTY  = XTOTQTY+ TOTQTY
        XVALUE   = XVALUE + TOTPIK * PRICE
        PRTPIK=.F.
        PRTPIK = IIF(PIK1 # QTY1, .T., PRTPIK)
        PRTPIK = IIF(PIK2 # QTY2, .T., PRTPIK)
        PRTPIK = IIF(PIK3 # QTY3, .T., PRTPIK)
        PRTPIK = IIF(PIK4 # QTY4, .T., PRTPIK)
        PRTPIK = IIF(PIK5 # QTY5, .T., PRTPIK)
        PRTPIK = IIF(PIK6 # QTY6, .T., PRTPIK)
        PRTPIK = IIF(PIK7 # QTY7, .T., PRTPIK)
        PRTPIK = IIF(PIK8 # QTY8, .T., PRTPIK)
        IF PRTPIK
           ROW = ROW+1
           @ ROW,05 SAY 'ALLO:'
           @ ROW,12 SAY PIK1     PICTURE '99999'
           @ ROW,18 SAY PIK2     PICTURE '99999'
           @ ROW,24 SAY PIK3     PICTURE '99999'
           @ ROW,30 SAY PIK4     PICTURE '99999'
           @ ROW,36 SAY PIK5     PICTURE '99999'
           @ ROW,42 SAY PIK6     PICTURE '99999'
           @ ROW,48 SAY PIK7     PICTURE '99999'
           @ ROW,54 SAY PIK8     PICTURE '99999'
           @ ROW,61 SAY TOTPIK   PICTURE '999999'
        ENDIF
        lnPieces = lnPieces + TOTPIK
        ROW =ROW+1
        @ ROW,05 SAY 'PICK:'
        @ ROW,12 SAY '_____'
        @ ROW,18 SAY '_____'
        @ ROW,24 SAY '_____'
        @ ROW,30 SAY '_____'
        @ ROW,36 SAY '_____'
        @ ROW,42 SAY '_____'
        @ ROW,48 SAY '_____'
        @ ROW,54 SAY '_____'
        @ ROW,62 SAY '_____'
        ROW=ROW+2
      ELSE           && If Print Size Scale No
        IF lcOldScale <> SCALE   && Print Scale in First time Only
          XSCALE = GETSCALE(SCALE,"|")
          lcOldScale = SCALE
        ENDIF
        IF SEEK('S' + &lcLinTemp..ACCOUNT + &lcLinTemp..Style, 'Spck_Hdr' )
          SELECT Spck_Hdr
          *PRIVATE laSku
          *DIMENSION laSku[1]
          *SCAN WHILE type + account + style = 'S' + &lcLinTemp..ACCOUNT + &lcLinTemp..Style
          *  FOR lnCount = 1 to 8
          *    lcCount = STR(lnCount,1)
          *    IF QTY&lcCount = 1
          *      IF ALEN(laSku,1) < lnCount
          *        DIMENSION laSku[lnCount]
          *      ENDIF
          *      laSku[lnCount] = Pack_ID
          *      EXIT
          *    ENDIF
          *  ENDFOR
          *ENDSCAN
          *PRIVATE laScales
          *DIMENSION laScales[1]
          *=gfSubStr(XSCALE,@laScales,"|")
          *lnSkCol = 1
          *FOR lnSku = 1 TO ALEN(laSku,1)
          *  @ ROW,lnSkCol SAY SUBSTR(laScales[lnSku],1,4) + ":" + SUBSTR(laSku[lnSku],1,14)
          *  IF lnSku = 4
          *    lnSkCol = 1
          *    ROW = ROW + 1
          *  ELSE
          *    lnSkCol = lnSkCol + LEN(laSku[lnSku]) + 1
          *  ENDIF
          *ENDFOR
          @ ROW,9 SAY Pack_ID
          SELECT (lcLinTemp)
        ENDIF
        @ ROW,61 SAY TOTPIK   PICTURE '999999'
        ROW = ROW + 1
        XVALUE   = XVALUE + TOTPIK * PRICE
      ENDIF
      *:C101868,1   AMH 06/20/2000 Check if Print Size Scale or Not (End)
      
      IF llNotes = .T.
        IF !EMPTY(NOTE_MEM)
          llFstNotL = .T.
          FOR X = 1 TO MEMLINES(NOTE_MEM)
            IF SUBSTR(MLINE(NOTE_MEM,X),1,1) <> '*'
              IF ROW>=47
                IF lnPieces >0
                  ROW = ROW + 1
                  @ 51,30 SAY "TOTAL PIECES TO PICK ............"  
                  @ 51,61 SAY lnPieces      PICTURE '999999'
                  ROW = ROW + 1
                ENDIF
                @ 57,28 SAY '** CONTINUED NEXT PAGE **'
                @ 58,00 SAY A
                DO lpHeader
                SELECT(lcLinTemp)
              ENDIF
              IF llFstNotL
                @ ROW , 05 SAY 'NOTES:'
                llFstNotL = .F.
              ENDIF
              @ ROW,12 SAY MLINE(NOTE_MEM,X)
              ROW = ROW + 1      
            ENDIF  
          ENDFOR   
        ENDIF  
      ENDIF
      *-- GET NEXT LINE ITEM
      SELECT (lcLinTemp)
      SKIP
   ENDDO
   *------------------------ END PRINT LINE LOOP ----------------------
   IF lnPieces >0
      ROW = ROW + 1
      @ ROW,30 SAY "TOTAL PIECES TO PICK ............"  &&  51->ROW
      @ ROW,61 SAY lnPieces      PICTURE '999999'
      ROW = ROW + 1
   ENDIF

   *-- Updated notepad to work with memo field.
   *------------------------
   * [NOTEPAD] COMMENT LINES
   *------------------------
   IF llRpOrdNot .AND. NEWDOC
     SELECT NOTEPAD
     IF SEEK('B'+XORDER)
       lnMemoWdth = SET('MEMOWIDTH')
       SET MEMOWIDTH TO 75
       lnMemoLnNo = 0      
       lnMemolins = 0
       ROW = ROW +1
       @ ROW,02 SAY '*-- N O T E P A D --*'
       ROW = ROW +1
       lnMemolins = MEMLINES(mNotes)
       IF lnMemolins > 0
         DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
           IF ROW > 47
             ROW = ROW + 1
             @ ROW,2 SAY 'CONTINUED NEXT PAGE ...'
             * Print header in the new page (Begin).
             llCont  = .T.
             SELECT ORDHDR
             =lfPrntHdr()
             SELECT NotePad
           ENDIF
           lnMemoLnNo = lnMemoLnNo + 1
           @ ROW,02 SAY MLINE(mNotes,lnMemoLnNo)
           ROW = ROW + 1
         ENDDO
       ENDIF
       @ ROW,02 SAY '*-- END OF NOTEPAD --*'
       lnMemoLnNo = 0      
       lnMemolins = 0
       SET MEMOWIDTH TO (lnMemoWdth)
     ENDIF
   ENDIF
   SELECT (lcLinTemp)
   IF XSTORE = STORE .AND. .NOT. NEWDOC
     @ 57,28 SAY '** CONTINUED NEXT PAGE **'
     @ 58,00 SAY A
   ELSE
     @ 52,01 SAY ORDHDR.NOTE1
     @ 52,41 SAY ORDHDR.NOTE2
     @ 53,00 SAY A
     @ 54,00 SAY E
     *-- concatinate variables with vertical lines
     F="|                 |           |       |           |           |"+;
        PADR(SUBSTR(lcShipVia,1,10),15)+"|"
     @ 55,00 SAY F
     @ 56,00 SAY A
     @ 57,00 SAY G
     *-- concatinate variables with vertical lines
     *-- Don't display the Merchandise if print prices is No
     IF llRpStyPr
       H="|  $"+PADL(XVALUE,12)+"  | $         | $     | $         |"+;
          PADR(SUBSTR(lcTermData,1,10),11)+"|               |"
     ELSE
       H="|  $              | $         | $     | $         |"+;
          PADR(SUBSTR(lcTermData,1,10),11)+"|               |"
     ENDIF
     @ 58,00 SAY H
     XVALUE   = 0.00
     @ 59,00 SAY A
   ENDIF
   *--Prevent an empty page after each pick ticket if we left the optional message empty.
   IF LEN(ALLTRIM(lcRpMsg1+lcRpMsg2+lcRpMsg3)) > 0
     @ 60,00 SAY lcRpMsg1
     @ 61,00 SAY lcRpMsg2
     @ 62,00 SAY lcRpMsg3
   ENDIF
   IF !NEWDOC
      LOOP
   ENDIF
   *-- Not to display "*" in front of the PIKTKT if it was printed once
   *B603509,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.[START]   
   *SELE PIKTKT
   *SEEK &lcPIKTEMP->PIKTKT
   *REPL NEXT 1 PRTFLAG WITH 'P'
   *B603509,1 BWA 03/08/2000 [END]
   
   SELECT (lcPIKTEMP)
   SKIP
ENDDO
RETURN

*!*************************************************************
*! Name      : PROCEDURE lpHeader
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/27/1999
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�        
*! Synopsis : To print the header before each page.  
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�         
*! Called from : 
*!          ALO20B.PRG
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Calls : 
*!         Procedures : None.
*!         Functions  : None.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Modifications : None.
*!*************************************************************
PROCEDURE lpHeader
PRIVATE lcAlias

lcAlias = SELECT()
SELECT ORDHDR
* LINE 1
@ 00,01 SAY 'R E L E A S E    F O R M'
@ 00,35 SAY HLINE1
@ 00,70 SAY DATE()
@ 01,35 SAY HLINE2
@ 02,01 SAY 'TICKET: '+XPIKTKT
@ 02,16 SAY XDATE
@ 02,35 SAY HLINE3
@ 02,70 SAY XTIME
@ 03,01 SAY 'ORDER : '+ORDER
@ 03,16 SAY SUBSTR(DTOC(START),1,5)
@ 03,24 SAY SUBSTR(DTOC(COMPLETE),1,5)
@ 03,35 SAY HLINE4
@ 04,01 SAY IIF(!EMPTY(APPROVAL),'APPRVL: '+APPROVAL,'')
@ 04,35 SAY HLINE5  PICTURE lcPhnFrmt SIZE 1,16
@ 06,04 SAY '.....SOLD TO .....'
@ 06,46 SAY '.....SHIP TO .....'
@ 07,04 SAY ACCOUNT
@ 07,10 SAY PHONE  PICTURE lcPhnFrmt SIZE 1,16
IF EMPTY(XSTORE).AND. XSTORE<>'*'
  @ 07,46 SAY 'STORE#: ' + IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
ENDIF
@ 08,04 SAY lcBtName
@ 08,46 SAY lcStName
@ 09,04 SAY lcBtAdd1
@ 09,46 SAY lcStAdd1
@ 10,04 SAY lcBtAdd2
@ 10,46 SAY lcStAdd2
@ 11,04 SAY lcBtAdd3
@ 11,46 SAY lcStAdd3
@ 13,00 SAY A
@ 14,00 SAY B
@ 15,00 SAY A
SELECT ORDHDR
@ 16,01 SAY lcShipVia
*-- Not to overwrite the next printed var.
@ 16,18 SAY SUBSTR(lcSeason,1,7)
@ 16,26 SAY lcSpcInst
@ 16,47 SAY &lcLinTemp..STORE
@ 16,58 SAY DEPT
@ 16,66 SAY IIF(MultiPO,lcCustPO,CUSTPO)
@ 17,00 SAY A
@ 18,00 SAY C
@ 19,00 SAY A
ROW=20
SELECT (lcAlias)

RETURN
*--------------------------------
*    END ALO820B.PRG
*--------------------------------

*!*************************************************************
*! Name      : lfPrntHdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/27/1999
*! Purpose   : Print report header.
*!*************************************************************
FUNCTION lfPrntHdr
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT ORDHDR
* LINE 1
@ 00,01 SAY 'R E L E A S E    F O R M'
@ 00,35 SAY HLINE1
@ 00,70 SAY DATE()
@ 01,01 SAY 'TICKET: '+XPIKTKT
@ 01,16 SAY XDATE
@ 01,35 SAY HLINE2
@ 02,01 SAY 'ORDER : '+ORDER
@ 02,16 SAY SUBSTR(DTOC(START),1,5)
@ 02,24 SAY SUBSTR(DTOC(COMPLETE),1,5)
@ 02,35 SAY HLINE3
@ 02,70 SAY XTIME
*-- Print Approval code if it's not empty.
@ 03,01 SAY IIF(!EMPTY(APPROVAL),'APPRVL: '+APPROVAL,'')
@ 03,35 SAY HLINE4
IF llWareHous .AND. !EMPTY(lcWDesc)
  @ 04,01 SAY lcWDesc
ENDIF
*-- Display phone with new format.
@ 04,35 SAY HLINE5  PICTURE lcPhnFrmt SIZE 1,16
@ 06,04 SAY '.....SOLD TO .....'
@ 06,46 SAY '.....SHIP TO .....'
@ 07,04 SAY ACCOUNT
*-- Display phone with new format.
@ 07,10 SAY PHONE  PICTURE lcPhnFrmt SIZE 1,16
IF !EMPTY(XSTORE) .AND. XSTORE<>'*'
  @ 07,46 SAY 'STORE#: ' + IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
ENDIF
@ 08,04 SAY lcBtName
@ 08,46 SAY lcStName
@ 09,04 SAY lcBtAdd1
@ 09,46 SAY lcStAdd1
@ 10,04 SAY lcBtAdd2
@ 10,46 SAY lcStAdd2
@ 11,04 SAY lcBtAdd3
@ 11,46 SAY lcStAdd3
@ 13,00 SAY A
@ 14,00 SAY B
@ 15,00 SAY A
*-- If store changed get new store (Begin).
IF llCont
  llCont = .F.
ELSE
  lcStore = &lcLinTemp->STORE
ENDIF  
SELECT ORDHDR
*-- concatinate variables with vertical lines (Begin)
lcCstPO  = IIF(MultiPO,lcCustPO,CUSTPO)
lcStrln1 = PADR(lcShipVia,16)+"|"+PADR(SUBSTR(lcSeason,1,7),8)+"|"+;
           PADR(lcSpcInst,18)+"|"+PADR(lcStore,10)+"|"+;
           PADR(DEPT,7)+"|"+PADR(lcCstPO,14)+"|"
@ 16,00 SAY lcStrln1
@ 17,00 SAY A
@ 18,00 SAY C
@ 19,00 SAY A
ROW=20
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfRepWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/27/1999
*! Purpose   : When function for the option grid
*!*************************************************************
*! Called from : Option grid when function
*!*************************************************************
FUNCTION lfRepWhen

*-- Initilize the date range with system date.
lnDatePos = ASCAN(laOGFxFlt,"PIKTKT.DATE")
IF lnDatePos > 0 AND lcRpSelect <> 'P'
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF  
ENDIF


*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/27/1999
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvSelAct
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/27/1999
*! Purpose   : Valid the 'Select by' COMBO BOX in the OG
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
FUNCTION lfvSelAct

CLEAR READ
*!*************************************************************
*! Name      : lfvcustom
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/27/1999
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvcustom

PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvPikTkt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/27/1999
*! Purpose   : Validation function for the Picking ticket field
*!*************************************************************
*! Called from : Picking ticket field [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPikTkt

PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'PIKTKT'))

  SELECT PIKTKT
  SET RELATION TO 'O' + Order INTO ORDHDR
  SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                      'S' + Account + Store) INTO CUSTOMER ADDITIVE
  
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "PIKTKT    :R :H= 'Pick tikit' ,"+;
               "PRINTED = IIF(PRTFLAG = 'P' , 'Yes' , 'No') :R :H= 'Printed' ,"+;
               "ORDER     :R :H= 'Order' ,"+;
               "STORE     :R :H= 'Store' ,"+;
               "ORDHDR.COMPLETE :R :H= 'Complete' ,"+;
               "DATE      :R :H= 'Pick Date' ,"+;
               "ACCOUNT   :R :H= 'Account' ,"+;
               "CUSTOMER.STNAME :R :H= 'Name' ,"+;
               "STATUS    :R :H= 'S' ,"+;
               "LABELS    :R :H= 'Labels' ,"+;
               "CWARECODE :R :H= 'WH. Code' ,"+;
               "CUSTPO    :R :H= 'Customer PO' "
  
  lcFile_Ttl = "Pick tickets"

  =gfBrows('FOR PIKTKT <> "******" ','PIKTKT','laTemp')
    
  SET RELATION TO
  
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = lcOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF

&lcVar = lcObj      && Update the field


****************************************************************************
* FUNC: lfvWareHouse
* DESC: To valid the warehouse code.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* Date: 06/27/1999
* Refer To : (C)
****************************************************************************
FUNCTION lfvWareHouse

lcWareHo = VARREAD()
lcTag = ORDER('WAREHOUS')
SET ORDER TO WAREHOUS IN WAREHOUS
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHo.,'WAREHOUS') 
    &lcWareHo = WAREHOUS.cWareCode
  ELSE
    &lcWareHo = gfBroWWare(.T.)
  ENDIF
ELSE
  &lcWareHo = ''
ENDIF
SET ORDER TO WAREHOUS IN WAREHOUS

*!*************************************************************
* Name  : lfAdrShift
* AUTH  : Adel Mohammed El Gazzar (ADEL)
* Date  : 06/27/1999
* Desc  : Function to Shift the Address array if there is any
*             empty lines in the address
*!*************************************************************
*! Called from : ALPKTKT.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
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

*!*************************************************************
*! Name      : lfvOptMsg
*! Programer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/27/1999
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOptMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length
=gfOptMsg('laOptMsg')