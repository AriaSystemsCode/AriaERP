**********************************************************************************
*! Program   : SOIMPCS.PRG
*! Developer : Sara Osama
*! Date      : 09/18/2016
*! Purpose   : Import Sales Order from CSV screen for ERI02
*! Entry#    : C201866 [P20160610.0003]
**********************************************************************************
*! Modifications:
*B611222,1 MMT 10/27/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161025.0008]
*C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045]
*B611253,1 MMT 1/23/2017 Issue#5: Import SO from CSV program rounds style price[P20161109.0001]
*B611273,MHM 26/02/2017 Modify the custom SO importing program for ERI02 to update BULK field with value N [T20170209.0019] 
**********************************************************************************
DO FORM (oAriaApplication.ClientScreenHome+"SO\SOIMPCSV.scx")
************************************************************
*! Name      : lfGetFile
*! Developer : Sara Osama
*! Date      : 09/18/2016
*! Purpose   : Get File
************************************************************
FUNCTION lfGetFile
PARAMETERS loFormSet
lcFile = GETFILE('CSV','Browse a .CSV file:','Import',0,'Select File to be Imported')
loFormSet.AriaForm1.txtFileName.Value = lcFile 

************************************************************
*! Name      : lfProceed
*! Developer : Sara Osama
*! Date      : 09/18/2016
*! Purpose   : Proceed after file selection
************************************************************
FUNCTION lfProceed
PARAMETERS loFormSet
IF EMPTY(ALLTRIM(loFormSet.AriaForm1.txtFileName.Value)) OR (!EMPTY(ALLTRIM(loFormSet.AriaForm1.txtFileName.Value)) AND !FILE(ALLTRIM(loFormSet.AriaForm1.txtFileName.Value)))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid file')
  RETURN .F.
ENDIF

IF UPPER(JUSTEXT(ALLTRIM(loFormSet.AriaForm1.txtFileName.Value))) <> 'CSV'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid file format')
  RETURN .F.
ENDIF

CREATE CURSOR TMPSTR (mStrRep M(10))

lcSOCSVTmp = gfTempName()
* Create Cursor
*C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][Start]
*!*	CREATE CURSOR (lcSOCSVTmp) ;
*!*	 (order C(6), season C(6), cdivision C(6), cwarecode C(6), ;
*!*	  account C(5), store C(8) , dept C(5), cordercat C(6), ; 
*!*	  priority C(3), custpo C(15), note1 C(30), note2 C(30), ;
*!*	  entered D(8), start D(8), complete D(8), caddress1 C(30), ;
*!*	  caddress2 C(30), caddress3 C(30), caddress4 C(30), caddress5 C(30), ;
*!*	  shipvia C(6), disc N(5), ctermcode C(6), lineno N(6), style C(19), ;
*!*	  qty1 N(6), price N(12))
*B611253,1 MMT 1/23/2017 Issue#5: Import SO from CSV program rounds style price[P20161109.0001][Start]
*!*	CREATE CURSOR (lcSOCSVTmp) ;
*!*	 (order C(6), season C(6), cdivision C(6), cwarecode C(6), ;
*!*	  account C(5), store C(8) , dept C(5), cordercat C(6), ; 
*!*	  priority C(3), CCARTRACK C(30), custpo C(15), note1 C(30), note2 C(30), ;
*!*	  entered D(8), start D(8), complete D(8), StName C(30),  caddress1 C(30), ;
*!*	  caddress2 C(30), caddress3 C(30), caddress4 C(30), caddress5 C(30), ;
*!*	  shipvia C(6), disc N(5), ctermcode C(6), lineno N(6), style C(19), ;
*!*	  qty1 N(6), price N(12), FRGHTAMT N(13,2), TAXAMOUNT N(13,2))
CREATE CURSOR (lcSOCSVTmp) ;
 (order C(6), season C(6), cdivision C(6), cwarecode C(6), ;
  account C(5), store C(8) , dept C(5), cordercat C(6), ; 
  priority C(3), CCARTRACK C(30), custpo C(15), note1 C(30), note2 C(30), ;
  entered D(8), start D(8), complete D(8), StName C(30),  caddress1 C(30), ;
  caddress2 C(30), caddress3 C(30), caddress4 C(30), caddress5 C(30), ;
  shipvia C(6), disc N(5), ctermcode C(6), lineno N(6), style C(19), ;
  qty1 N(6), price N(12,2), FRGHTAMT N(13,2), TAXAMOUNT N(13,2))
*B611253,1 MMT 1/23/2017 Issue#5: Import SO from CSV program rounds style price[P20161109.0001][End]  
*C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][End]
* create Index on the cursor
SELECT(lcSOCSVTmp)
INDEX ON Order + Account + store   TAG(lcSOCSVTmp)
* Append from CSV
APPEND FROM (loFormSet.AriaForm1.txtFileName.Value) TYPE CSV
*B611222,1 MMT 10/27/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161025.0008][Start]
SELECT(lcSOCSVTmp)
DELETE FOR EMPTY(ALLTRIM(ORDER))
*B611222,1 MMT 10/27/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161025.0008][End]
*-----Validations-------*
SELECT DISTINCT ORDER,Account FROM (lcSOCSVTmp) WHERE !DELETED() INTO CURSOR 'AccTmp'
IF !USED('CUSTOMER')
  =gfOpenTable("Customer","Customer")
ENDIF 
SELECT 'AccTmp'
LOCATE
SCAN
  IF !gfSeek('M'+ AccTmp.Account,'Customer','Customer')
   SELECT TMPSTR 
   LOCATE
   IF EOF()
     APPEND BLANK
   ENDIF
   REPLACE mStrRep WITH mStrRep +"Order#:"+ALLTRIM(AccTmp.ORDER)+", Account: "+ AccTmp.Account+" is not found in Customer file"+CHR(13)+CHR(10)
  ENDIF
ENDSCAN

SELECT Distinct ORDER,Account,Store FROM (lcSOCSVTmp) WHERE !DELETED() AND !EMPTY(ALLTRIM(Store)) INTO CURSOR 'StoreTmp'
SELECT 'StoreTmp'	
LOCATE 
SCAN
  IF !gfSeek('S'+ StoreTmp.Account+StoreTmp.Store,'Customer','Customer')
   SELECT TMPSTR 
   LOCATE
   IF EOF()
     APPEND BLANK
   ENDIF
   REPLACE mStrRep WITH mStrRep +"Order#:"+ALLTRIM(StoreTmp.ORDER)+", Account: "+ StoreTmp.Account+", Store: "+StoreTmp.Store+" is not found in Customer file"+CHR(13)+CHR(10)
  ENDIF
ENDSCAN

SELECT DISTINCT Order,ShipVia FROM (lcSOCSVTmp) WHERE !DELETED()  AND !EMPTY(ShipVia) INTO CURSOR 'ShipViaTmp'
SELECT ShipViaTmp
LOCATE
SCAN
  lcCodSh = lfGeCodeNO(ShipViaTmp.ShipVia,'SHIPVIA')  
  IF EMPTY(ALLTRIM(lcCodSh))
    SELECT TMPSTR 
    LOCATE
    IF EOF()
      APPEND BLANK
    ENDIF
    REPLACE mStrRep WITH mStrRep +"Order#:"+ALLTRIM(ShipViaTmp.ORDER)+", ShipVia: "+ ALLTRIM(ShipViaTmp.ShipVia)+" is not found in Codes file"+CHR(13)+CHR(10)
  ENDIF
ENDSCAN

SELECT DIStinct Order,Season FROM (lcSOCSVTmp) WHERE !DELETED() AND !EMPTY(Season) INTO CURSOR 'SeasonTmp'
SELECT 'SeasonTmp'
LOCATE
IF !EOF()
  SELECT 'SeasonTmp'
  SCAN
   lcCodSh = lfGeCodeNO(SeasonTmp.Season ,'SEASON')  
   IF EMPTY(ALLTRIM(lcCodSh))
    *IF !gfSeek('N'+padr('SEASON',10)+ALLTRIM(SeasonTmp.Season) ,'Codes','CCODE_NO')
      SELECT TMPSTR 
      LOCATE
      IF EOF()
        APPEND BLANK
      ENDIF
      REPLACE mStrRep WITH mStrRep +"Order#:"+ALLTRIM(SeasonTmp.ORDER)+", Season: "+ ALLTRIM(SeasonTmp.Season)+" is not found in Codes file"+CHR(13)+CHR(10)
    ENDIF
  ENDSCAN
ENDIF
SELECT DIStinct Order,CDIVISION FROM (lcSOCSVTmp) WHERE !DELETED() AND !EMPTY(CDIVISION) INTO CURSOR 'CDIVSIONTmp'
SELECT 'CDIVSIONTmp'
LOCATE
IF !EOF()
  SELECT 'CDIVSIONTmp'
  SCAN
    lcCodSh = lfGeCodeNO(CDIVSIONTmp.CDIVISION ,'CDIVISION')  
    IF EMPTY(ALLTRIM(lcCodSh))
    *IF !gfSeek('N'+padr('CDIVISION',10)+ALLTRIM(CDIVSIONTmp.CDIVISION) ,'Codes','CCODE_NO')
      SELECT TMPSTR 
      LOCATE
      IF EOF()
        APPEND BLANK
      ENDIF
      REPLACE mStrRep WITH mStrRep +"Order#:"+ALLTRIM(CDIVSIONTmp.ORDER)+", Division: "+ ALLTRIM(CDIVSIONTmp.CDIVISION)+" is not found in Codes file"+CHR(13)+CHR(10)
    ENDIF
  ENDSCAN
ENDIF


SELECT DISTINCT Order,Cordercat FROM (lcSOCSVTmp) WHERE !DELETED()  AND !EMPTY(Cordercat) INTO CURSOR 'CordercatTmp'
SELECT CordercatTmp
LOCATE
SCAN
  lcCodSh = lfGeCodeNO(CordercatTmp.Cordercat,'CORDERCAT')  
  IF EMPTY(ALLTRIM(lcCodSh))
    SELECT TMPSTR 
    LOCATE
    IF EOF()
      APPEND BLANK
    ENDIF
    REPLACE mStrRep WITH mStrRep +"Order#:"+ALLTRIM(CordercatTmp.ORDER)+", Order Category: "+ ALLTRIM(CordercatTmp.Cordercat)+" is not found in Codes file"+CHR(13)+CHR(10)
  ENDIF
ENDSCAN

SELECT DISTINCT Order,ctermcode FROM (lcSOCSVTmp) WHERE !DELETED()  AND !EMPTY(ctermcode ) INTO CURSOR 'ctermcodeTmp'
SELECT ctermcodeTmp
LOCATE
SCAN
  lcCodSh = lfGeCodeNO(ctermcodeTmp.ctermcode ,'CTERMCODE')  
  IF EMPTY(ALLTRIM(lcCodSh))
    SELECT TMPSTR 
    LOCATE
    IF EOF()
      APPEND BLANK
    ENDIF
    REPLACE mStrRep WITH mStrRep +"Order#:"+ALLTRIM(ctermcodeTmp.ORDER)+", Term Code: "+ ALLTRIM(ctermcodeTmp.ctermcode)+" is not found in Codes file"+CHR(13)+CHR(10)
  ENDIF
ENDSCAN

SELECT DISTINCT Order,Cwarecode FROM (lcSOCSVTmp) WHERE !DELETED() INTO CURSOR 'CwarecodeTmp'
IF !USED('WAREHOUS')
  =gfOpenTable("Warehous","Warehous")
ENDIF 
SELECT 'CwarecodeTmp'
LOCATE
SCAN
  IF !gfSeek(CwarecodeTmp.Cwarecode,'Warehous','Warehous')
   SELECT TMPSTR 
   LOCATE
   IF EOF()
     APPEND BLANK
   ENDIF
   REPLACE mStrRep WITH mStrRep +"Order#:"+ALLTRIM(CwarecodeTmp.ORDER)+", Warehouse: "+ CwarecodeTmp.cwarecode +" is not found in Warehouse file"+CHR(13)+CHR(10)
  ENDIF
ENDSCAN

SELECT DISTINCT Order, style FROM (lcSOCSVTmp) WHERE !DELETED() INTO CURSOR 'StyleTmp'
IF !USED('STYLE')
  =gfOpenTable("style","style")
ENDIF 
SELECT 'StyleTmp'
LOCATE
SCAN
  IF !gfSeek(StyleTmp.style,'style','style')
   SELECT TMPSTR 
   LOCATE
   IF EOF()
     APPEND BLANK
   ENDIF
   REPLACE mStrRep WITH mStrRep +"Order#:"+ALLTRIM(StyleTmp.Order)+", Style: "+ StyleTmp.style +" is not found in Style file"+CHR(13)+CHR(10)
  ENDIF
ENDSCAN

SELECT DISTINCT Order,Dept,account FROM (lcSOCSVTmp) WHERE !DELETED() AND !EMPTY(Dept) INTO CURSOR 'DeptTmp'
IF !USED('CUSTDEPT')
  =gfOpenTable("CUSTDEPT","CUSTDEPT")
ENDIF 
SELECT 'DeptTmp'
LOCATE
SCAN FOR !EMPTY(DeptTmp.Dept)
  IF !gfSeek(DeptTmp.account+DeptTmp.Dept,'CUSTDEPT','CUSTDEPT')
   SELECT TMPSTR 
   LOCATE
   IF EOF()
     APPEND BLANK
   ENDIF
   REPLACE mStrRep WITH mStrRep +"Order#:"+ALLTRIM(DeptTmp.ORDER)+", Department: "+ DeptTmp.Dept+" is not found in Department file"+CHR(13)+CHR(10)
  ENDIF
ENDSCAN
*------ Call form ------------
SELECT TMPSTR
LOCATE
IF !EOF()
  DO FORM (oAriaApplication.ClientScreenHome+"SO\soimpre.scx")
  RETURN .F.
ENDIF
*-------------------------------------
STORE 0 TO lnClrLen ,lnClrPos ,lnSizeLen ,lnSizePos ,lnMajLen
STORE '' TO lcClrSpr
DIMENSION laItemSeg[1]
=gfItemMask(@laItemSeg)
For lnCount = 1 To Alen(laItemSeg,1)
  Do Case
  Case laItemSeg[lnCount,1]='F'
    lnMajLen = Len(laItemSeg[lnCount,3])

  Case laItemSeg[lnCount,1]='C'
    lnClrLen = Len(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = Allt(laItemSeg[lnCount,6])
  Case laItemSeg[lnCount,1]='S'
    lnSizeLen = Len(laItemSeg[lnCount,3])
    lnSizePos = laItemSeg[lnCount,4]
  Endcase
Endfor
LCORDLNTMP = gftempName()
lcOrdHdrTmp= gfTempName()
lcT_BomVar= gfTempName()
DIMENSION laOrders[1]
laOrders = ''
IF !useD('BomVar')
  =gfOpenTable(oAriaApplication.DataDir+'BomVar',oAriaApplication.DataDir+'BomVar','SH')
ENDIF
IF !USED('Scale')
  =gfOpenTable('Scale','Scale')
ENDIF
IF !used('STYDYE')
  =gfOpenTable(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
ENDIF

IF !USED("ORDLINE")
  =gfOpenTable("ORDLINE","ORDLINE")
ENDIF

IF !USED("ORDHDR")
  =gfOpenTable("ORDHDR","ORDHDR")
ENDIF

lfCreateOrdTmp()
lfCreateOrders()


IF !EMPTY(laOrders[1])
  IF ALEN(laOrders,1)>1
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Orders from order#'+laOrders[1]+' to order#'+laOrders[ALEN(laOrders,1)]+' are created.')
  ELSE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order#'+laOrders[1]+' is created.')
  ENDIF
ENDIF

************************************************************
*! Name      : lfGeCodeNO
*! Developer : Sara Osama
*! Date      : 09/18/2016
*! Purpose   : Get Code No. from code description
************************************************************
FUNCTION lfGeCodeNO
LPARAMETERS lcCodeNum,lcCodeName
IF !USED('Codes')
  =gfOpenTable("Codes",'cCode_NO')
ENDIF
lcCodeNo = ''
SELECT Codes
=gfSetOrder('cCode_NO')
if gfSeek("N"+PADR(lcCodeName,10)+allt(lcCodeNum))
  lcCodeNo = Codes.CCODE_NO
ENDIF
RETURN (lcCodeNo)


************************************************************
*! Name      : lfCreateOrders
*! Developer : Sara Osama
*! Date      : 09/18/2016
*! Purpose   : Create SO function
************************************************************
FUNCTION lfCreateOrders
LOFORMSET =  CREATEOBJECT('Custom')
LOFORMSET.Addproperty('lcdeposittemp','')
LOFORMSET.Addproperty('laEvntTrig[1]','')
LOFORMSET.Addproperty('ActiveMode','A')
LOFORMSET.laEvntTrig[1] =PADR('SOIMPRO',10)
LOFORMSET.lcdeposittemp = gfTempName()
laOrders[1]=''
SELECT DISTINCT ORDER FROM (lcSOCSVTmp) WHERE !DELETED() INTO CURSOR 'TmpOrder'
SELECT 'TmpOrder'
LOCATE
SCAN
**************
  =SEEK(TmpOrder.Order ,lcSOCSVTmp)
  *C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][Start]
*!*	  STORE '' TO m.cclass,M.Ccurrcode ,m.Cdivision,m.Cordtype,m.ctermcode ,m.CustPO ,;
*!*	         m.Order,m.CwareCode,m.GL_Sales,m.LINK_Code, m.Multi,m.Note1,;
*!*	         m.priority,m.Season,m.shipvia,m.spcinst ,m.Status ,m.StName ,lcOrder
*!*	  STORE 0 TO m.comm1,m.comm2,m.Nexrate,m.Disc,m.Appramt
  STORE '' TO m.cclass,M.Ccurrcode ,m.Cdivision,m.Cordtype,m.ctermcode ,m.CustPO ,;
         m.Order,m.CwareCode,m.GL_Sales,m.LINK_Code, m.Multi,m.Note1,;
         m.priority,m.Season,m.shipvia,m.spcinst ,m.Status ,m.StName ,lcOrder ,m.CCARTRACK
  store .F. TO m.alt_shpto ,m.lhasnotes,m.MultiPO,m.lfromweb
  STORE '' TO m.Bulk,m.creorder,m.Rep1,m.Rep2,m.Buyer,m.Phone
    *B611273,MHM 26/02/2017 Modify the custom SO importing program for ERI02 to update BULK field with value N [T20170209.0019][Begin]                
     m.BULK = "N"
     *B611273,MHM 26/02/2017 Modify the custom SO importing program for ERI02 to update BULK field with value N [T20170209.0019][End]                
	STORE 0 TO m.comm1,m.comm2,m.Nexrate,m.Disc,m.Appramt ,m.FRGHTAMT ,m.TAXAMOUNT
  STORE 0 TO  m.Book,m.BookAmt,lnLastLNo,m.lastline,m.nExRate,m.NcurrUnit,m.OPenAmt,m.OPen, m.TotAmnt 
  *C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][End]                
  SELECT (lcSOCSVTmp)
  SCATTER MEMO MEMVAR 
  m.Order = ''
  =gfSeek('M'+m.Account,'Customer','Customer')
  M.Ccurrcode = Customer.ccurrcode 
  IF M.Ccurrcode <> oAriaApplication.BaseCurrency
    lnUnit = 0
    m.Nexrate  = gfChkRate('lnUnit' , M.Ccurrcode , m.entered  , .F.) 
    m.NcurrUnit = lnUnit
  ELSE
    m.NcurrUnit = 1
    m.Nexrate  = 1
  ENDIF 
      
   m.lfromweb = .F.
   m.Rep1 = Customer.SalesRep
   m.comm1 = Customer.Comm
   m.comm2 = Customer.Comm2
   m.Rep2 = Customer.Rep2
   m.Buyer = customer.buyer
   m.Phone = Customer.Phone1
   m.Appramt = 0
   
   SELECT DISTInCT SEASON FROM (lcSOCSVTmp) WHERE Order = TmpOrder.Order AND Account = m.Account INTO CURSOR 'TmpSeason'
   IF RECCOUNT('TmpSeason') > 1
     m.Season = '*'
   ELSE
     SELECT TmpSeason
     LOCATE
     m.Season = TmpSeason.Season
   ENDIF
   
   SELECT DISTInCT cDivision FROM (lcSOCSVTmp) WHERE Order = TmpOrder.Order AND Account = m.Account INTO CURSOR 'TmpcDivision'
   IF RECCOUNT('TmpcDivision') > 0
     SELECT TmpcDivision
     LOCATE
     m.cDivision = TmpcDivision.cDivision
   ENDIF
   
   SELECT(lcSOCSVTmp)
   COUNT FOR Order = TmpOrder.Order  AND Account = m.Account  TO lnLastLNo 
      
   SELECT(lcSOCSVTmp)
   SUM Qty1,;
       Qty1*Price ;
       FOR Order = TmpOrder.Order  AND Account = m.Account TO M.qty1,m.TotAmnt 
      
*   SELECT (lcHeaderTmp)      
   m.MultiPO  = .F.
   m.Cordtype = 'O'
   m.Order = '' &&gfSequence('ORDER','','',m.cDivision)
   *m.CwareCode = WAREHOUS.CwareCode
   = gfSeek('M'+m.Account,'CUSTOMER','CUSTOMER')
   IF EMPTY(ALLTRIM(m.ctermcode))
     m.ctermcode = Customer.ctermcode 
   ENDIF  
   m.GL_Sales = Customer.cslsgllink
   m.LINK_Code = Customer.LINK_Code
   m.Multi = 'N'
   SELECT DISTInCT STORE FROM (lcSOCSVTmp) WHERE Order = TmpOrder.Order AND Account = m.Account AND !EMPTY(Store) INTO CURSOR 'TmpcStore'
   IF RECCOUNT('TmpcStore') > 1
     m.Multi = 'Y'
   ENDIF
   IF EMPTY(m.Priority)
     m.Priority = Customer.priority 
   ENDIF  
   IF EMPTY(ALLTRIM(m.shipvia))
     m.shipvia = Customer.shipvia 
   ENDif  
   IF EMPTY(ALLTRIM(m.spcinst))
     m.spcinst = Customer.spcinst 
   ENDIF   
   m.Status = 'O'
   =gfSeek(IIF(!EMPTY(m.Store),"S","M")+m.Account+IIF(!EMPTY(m.Store),m.Store,""),'CUSTOMER','CUSTOMER')
   *C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][Start]          
     *!*m.StName = IIF(!EMPTY(m.Store),Customer.StName,"")
	 m.StName = IIF(EMPTY(m.StName) AND !EMPTY(m.Store),Customer.StName,m.StName)
	 IF Customer.ntaxrate > 0 AND m.TAXAMOUNT> 0
	 lnChoice  =gfModalGen("INM00000B44009","Dialog","","","Customer "+m.Account+" has tax rate "+ALLTRIM(STR(Customer.ntaxrate,6,3))+;
	                                                       " and the imported order has tax amount "+ALLTRIM(STR(m.TAXAMOUNT,13,2))+;
	                                                       ", would you like to use imported order tax amount?")
        DO CASE
        CASE lnChoice = 1
        *Yes
        CASE lnChoice = 2
        *No 
        m.TAXAMOUNT= 0
        ENDCASE
	 ENDIF
	 IF !EMPTY(m.Store) AND !EMPTY(m.StName)
   	   IF m.StName <> Customer.StName 
	 	  *Warning Message
	 	  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Store '+ALLTRIM(m.Store)+' has Ship to name in system: '+ ALLTRIM(Customer.StName) +', and the imported order ship name is '+ALLTRIM(m.StName)+'.')
	 	ENDIF
	 ENDIF 
   *C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][End]          
   m.alt_shpto = .F.
   IF !EMPTY(ALLTRIM(m.caddress1))
     m.alt_shpto = .T.
   ENDIF  
   
   m.lhasnotes = .F. 
   
   m.Book = m.Qty1 
   m.BookAmt = m.TotAmnt 
   m.lastline  = lnLastLNo 
   m.Flag = 'N'
   m.OPEN = m.Qty1 
   m.OPENAMT =  m.TotAmnt 
   INSERT INTO (lcOrdHdrTmp) FROM MEMVAR
   lcOrder = m.Order
      *Inserting Lines in Ordline File
   SELECT(lcSOCSVTmp) 
   lnLine = 1
   SCAN FOR ORDER = TmpOrder.Order AND Account = m.Account 
     STORE 0 TO m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8
     STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8
     STORE 0 TO m.comm1,m.comm2
     SCATTER MEMO MEMVAR 
     m.note_mem = ''
     m.Gros_price = m.price  
     m.Account = &lcSOCSVTmp..Account
     *m.Order   = lcOrder         
     *m.CwareCode = WAREHOUS.CwareCode
     *m.store  = &lcLineTmpOrd..store
     m.Cordtype = 'O'
     *m.CustPo = m.Worder
     =gfSeek(&lcSOCSVTmp..Style,'Style','Style')
     *m.Gros_price = M.Price
     m.Cost = Style.TOTCOST
     m.Desc1 = Style.Desc1
     m.Flag = 'N'
     m.Gl_Cost = Style.Link_Code
     m.GL_Sales = Customer.cslsgllink + Style.CSLSGLLINK
     m.Scale = Style.Scale
     m.Season = Style.Season
     m.Book1 = m.Qty1 
     m.Book2 = m.Qty2
     m.Book3 = m.Qty3
     m.Book4 = m.Qty4
     m.Book5 = m.Qty5
     m.Book6 = m.Qty6 
     m.Book7 = m.Qty7
     m.Book8 = m.Qty8
     m.totbook =  m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
     m.totQty =  m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
     *m.lineno = lnLine 
     lnLine = lnLine + 1
     INSERT INTO (lcOrdLnTmp) FROM MEMVAR
   ENDSCAN
   llOrdSaved = .F.
   SET ORDER TO oRDLINE IN (lcOrdLnTmp)
   DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') ;
                  WITH .F., 'A', lcOrdHdrTmp,lcOrdLnTmp,.F.,.f.,lcT_BomVar,loFormSet

   IF llOrdSaved
     IF !EMPTY(laOrders[1])
       DIMENSION laOrders[ALEN(laOrders,1)+1]
       laOrders[ALEN(laOrders,1)] = ORDHDR.ORDER
     ELSE
       laOrders[1] = ORDHDR.ORDER
     ENDIF
   ENDIF               
   
   SELECT (lcOrdLnTmp)
   DELETE ALL
   PACK

   SELECT (lcOrdHdrTmp)
   DELETE ALL
   PACK
 ENDSCAN  
 lfSavefiles()
************************************************************
*! Name      : lfCreateOrdTmp
*! Developer : Sara Osama
*! Date      : 09/18/2016
*! Purpose   : Create Temp. Order cursors
************************************************************
FUNCTION lfCreateOrdTmp
SELECT OrdLine
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
llTrade =  gfGetMemVar('M_TRDDISCL')
IF llTrade 
  IF lnSizeLen  = 0
    DIMENSION laFileStru[lnFileStru+4,18]
  ELSE
    DIMENSION laFileStru[lnFileStru+5,18]
  ENDIF  
ELSE
  IF lnSizeLen  = 0
    DIMENSION laFileStru[lnFileStru+3,18]
  else
    DIMENSION laFileStru[lnFileStru+4,18]
  ENDIF
ENDIF

    laFileStru[lnFileStru+1,1] = 'lContract'
    laFileStru[lnFileStru+1,2] = 'L'
    laFileStru[lnFileStru+1,3] = 1
    laFileStru[lnFileStru+1,4] = 0
    laFileStru[lnFileStru+2,1] = 'cMajor'
    laFileStru[lnFileStru+2,2] = 'C'
    laFileStru[lnFileStru+2,3] = lnMajLen
    laFileStru[lnFileStru+2,4] = 0
    laFileStru[lnFileStru+3,1] = 'cNonMajor'
    laFileStru[lnFileStru+3,2] = 'C'
    laFileStru[lnFileStru+3,3] = lnClrLen
    laFileStru[lnFileStru+3,4] = 0
    IF lnSizeLen  <> 0
      laFileStru[lnFileStru+4,1] = 'cMjrScale'
      laFileStru[lnFileStru+4,2] = 'C'
      laFileStru[lnFileStru+4,3] =  lnSizeLen 
      laFileStru[lnFileStru+4,4] = 0
    ENDIF  
IF llTrade 
  IF lnSizeLen  = 0
    laFileStru[lnFileStru+4,1] = 'TRD_price'
    laFileStru[lnFileStru+4,2] = 'N'
    laFileStru[lnFileStru+4,3] =  12
    laFileStru[lnFileStru+4,4] = 2

  ELSE
  
  laFileStru[lnFileStru+5,1] = 'TRD_price'
  laFileStru[lnFileStru+5,2] = 'N'
  laFileStru[lnFileStru+5,3] =  12
  laFileStru[lnFileStru+5,4] = 2
  ENDIF
ENDIF
FOR lnCount = 1 TO IIF(lnSizeLen  = 0,IIF(llTrade ,4,3),IIF(llTrade ,5,4))
  STORE '' TO laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
              laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
              laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
              laFileStru[lnFileStru+lnCount,16]
  STORE 0 TO  laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
ENDFOR
DECLARE laIndex[4,2]
laIndex[1,1] = 'cOrdType+ORDER+STORE+STYLE+DYELOT+STR(LINENO,6)'
laIndex[1,2] = 'ORDLINST'
laIndex[2,1] = 'cOrdType+ORDER+STYLE+STORE+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINES'
laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
laIndex[3,2] = 'ORDLINE'
laIndex[4,1] = 'Order+Store+STYLE+Dyelot+STR(LineNo,6)'
laIndex[4,2] = 'CONFIGLIN'

=gfCrtTmp(lcOrdLnTmp,@laFileStru,@laIndex)
SET ORDER TO TAG 'ORDLINE' IN (lcOrdLnTmp)

SELECT ORDHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru,18]
DECLARE laIndex[1,2]
laIndex[1,1] = 'cordtype+order'
laIndex[1,2] = lcOrdHdrTmp
=gfCrtTmp(lcOrdHdrTmp,@laFileStru,@laIndex)


SELECT BomVar
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]

    
laFileStru[lnFileStru+1,1] = 'nRecno'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'cStatus'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
FOR lnLoop = 1 to  2
   STORE ' ' TO  laFileStru[lnFileStru+lnLoop,7],laFileStru[lnFileStru+lnLoop,8],;
            laFileStru[lnFileStru+lnLoop,9],laFileStru[lnFileStru+lnLoop,10],;
            laFileStru[lnFileStru+lnLoop,11],laFileStru[lnFileStru+lnLoop,12],;
            laFileStru[lnFileStru+lnLoop,13],laFileStru[lnFileStru+lnLoop,14],;
            laFileStru[lnFileStru+lnLoop,15],laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO    laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR
IF USED(lcT_BomVar)
  ZAP IN (lcT_BomVar)
ELSE
 =gfCrtTmp(lcT_BomVar,@laFileStru,[cIdType+cCost_Id+STR(LineNo,6)],lcT_BomVar)
ENDIF

*!*************************************************************
*! Name      : lfSavefiles
*! Developer : Sara Osama
*! Date      : 09/18/2016
*! Purpose   : Function to save 
*!*************************************************************
FUNCTION lfSavefiles
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
   RETURN .F.
ENDIF
lnUpdated = 0
lnAryLen = ALEN(oAriaApplication.laRemoteTable)
FOR lnCounter=1 TO lnAryLen
  IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == SET("Datasession" )
    IF !oAriaApplication.laRemoteTable[lnCounter].TableUpdate(lcTranCode)
      lnUpdated=lnCounter
      exit
    ENDIF
  ENDIF
NEXT
IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  MESSAGEBOX('Saving Process is Rolled Back')
  ThisFormSet.Undo()
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF
************************************************************
*! Name      : lfcustmsg
*! Developer : Sara Osama
*! Date      : 09/18/2016
*! Purpose   : Custom function to check if order is created or not
************************************************************
FUNCTION lfcustmsg
llOrdSaved = .T.
