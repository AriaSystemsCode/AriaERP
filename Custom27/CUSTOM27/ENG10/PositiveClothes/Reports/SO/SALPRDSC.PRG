*!***************************************************************************
*! Program file        : SALPRDSC.PRG   
*! Program desc.       : Sales/Production Schedule  Report  [TIP10]
*! For Report          : SALPRDSC.FRX
*! System              : Aria Advantage Series VER. 2.7
*! Module              : Sales Oredr - (SO)
*! Developer           : NADER NABILC (NNA)
*! Tracking Job Number : C200907 [T20071109.0001]
*! Date                : 01/07/2008
*!****************************************************************************
*! Calls :  Functions  : lfvDateRng , lfOpnFiles , lfCrtTemp , lfGetProd , lfGetSales
*!                     : lfVToDate
*!****************************************************************************
*! Called From         : System Menu (SO --> Output --> Sales/Production Schedule Report)
*!****************************************************************************
*! Passed Parameters   : None
*!****************************************************************************
*! Example             : DO SALPRDSC
*!****************************************************************************
*! Modification        :
*!
*!****************************************************************************
*--Open the needed files
PRIVATE lcSetCen , lcOrder
*-->Hold the current status for set century
lcSetCen = SET('CENTURY')
lnMajorLen = LEN(gfItemMask("PM"))
SET CENTURY ON
*-->Check if user leave the To Date Blank or not
IF EMPTY(ldRPToD)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'The (To) date can not be blank , Please input a valid to date')
  =lfvDateRng()
  RETURN .F.
ENDIF
*-->Open the needed files for the report.
= lfOpnFiles(.T.,"ORDBOM,POCUT,INVHDR,INVLINE,CONSINVH,CONSINVL,SCALE,CTKTBOM,NOTEPAD,CODES",;
                 "ORDBOM,POCUT,INVHDR,INVLINEO,CONSINVH,CINVLINE,SCALE,CTKTBOM,NOTEPAD,CCODE_NO")
SET ORDER TO TAG Cutord IN CUTPICK
lcOrder = ORDER('STYLE')

*-->Create the needed Temp files.
=lfCrtTemp()

*-->Set Relationships.
= lfSetRela()

*-->Collecting Data depending on Report format.
IF lcRpFormat='S'
  =lfGetSales()
  SELECT (lcTmpSales)
ELSE
  =lfGetProd()
  SELECT (lcTmpProd)
ENDIF
DO gfDispRe WITH EVAL('lcRpForm')    

*--Close the opened files.
= lfOpnFiles(.F.,"ORDBOM,POCUT,INVHDR,INVLINE,CONSINVH,CONSINVL,CTKTBOM,NOTEPAD")

SET CENTURY &lcSetCen

IF USED('TmpSlct')
  USE IN TmpSlct
  ERASE (gcWorkDir + 'TmpSlct.DBF')
ENDIF
RETURN
*!****************************************************************************
*! Name      : lfvDateRng
*! Developer : Nader Nabil (NNA)
*! Date      : 01/07/2008
*! Purpose   : Validate sales order date range
*!****************************************************************************
*! Called from : Option Grid
*!****************************************************************************
*! Passed Parameters : None
*!****************************************************************************
*! Return      : None
*!****************************************************************************
*! Example     : = lfvDateRng()
*!****************************************************************************
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo
IF EMPTY(ldRPFromD) .AND. EMPTY(ldRPToD)
  STORE {} TO ldFrom,ldTo
ELSE && not empty the vars that hold the dates From and To
  ldFrom = ldRPFromD
  ldTo = ldRPToD
ENDIF
lcTitle = 'Sales Order Complete Date'
DO DateRng.Spx
ldRPFromD = ldFrom
ldRPToD = ldTo
*--> End of function lfvDateRng.
*!****************************************************************************
*! Name      : lfCrtTemp
*! Developer : Nader Nabil (NNA)
*! Date      : 01/07/2008
*! Purpose   : Create Temp Files
*!****************************************************************************
*! Called from : salprdsc.prg
*!****************************************************************************
*! Passed Parameters : None
*!****************************************************************************
*! Return      : None
*!****************************************************************************
*! Example     : = lfCrtTemp()
*!****************************************************************************
FUNCTION lfCrtTemp
IF lcRpFormat='S'
  CREATE TABLE (gcWorkDir+lcTmpSales)(Ref C(6),Status C(1),Account C(8),Store C(8),Style C(12),Vendor C(8),;
      Desc C(25),Complete D(10),Entered D(10),Cancelled D(10),INVDATE D(10),SORT_DATE C(10),INVOICED L,;
      PosDel C(6),SalesQty N(8),Qty_Wrk N(7),Qty_Cut N(7),TOTQTY N(7),CMT_Price N(15,7),;
      Sales_Price N(12,2),Disc N(6,2),Cost_Price N(13,5),TotCostPr N(13,3),Expt_Desp N(7),;
      ExDesp_DEL N(7),Tot_SalVal N(12,2),Gross_Marg N(4),Net_Marg N(4),TotDis_Val N(10,2),;
      Sect_Flag C(7),Sect_Hdr C(20),Two_Sec C(2))
  DO CASE
    CASE lcRpSortBy='O'
      INDEX ON (SECT_FLAG+SORT_DATE+REF) TAG (lcTmpSales) OF (lcTmpSales)
    CASE lcRpSortBy='C'
      INDEX ON (SECT_FLAG+SORT_DATE+ACCOUNT) TAG (lcTmpSales) OF (lcTmpSales)
    CASE lcRpSortBy='F'
      INDEX ON (SECT_FLAG+SORT_DATE+VENDOR) TAG (lcTmpSales) OF (lcTmpSales)
    CASE lcRpSortBy='S'
      INDEX ON (SECT_FLAG+SORT_DATE+STYLE) TAG (lcTmpSales) OF (lcTmpSales)
    OTHERWISE    
      INDEX ON (SECT_FLAG+SORT_DATE+DTOS(complete)) TAG (lcTmpSales) OF (lcTmpSales)
  ENDCASE

ELSE
  CREATE TABLE (gcWorkDir+lcTmpProd)(Ref C(6),Account C(8),Store C(8),Style C(12),Vendor C(8),;
      Desc C(25),Complete D(10),Entered D(10),Cancelled D(10),INVDATE D(10),SORT_DATE C(10),INVOICED L,;
      PosDel C(6),SalesQty N(8),TOTQTY N(7),Expt_Desp N(7),ExDesp_DEL N(7),New_Rpt C(3),Qty_Wrk N(7),Qty_Cut N(7),;
      Fact_Delv C(7),CtktNo C(6),Issue_Qty C(3),Ctech C(3),Comments C(30),Sect_Flag C(7),Sect_Hdr C(20),Two_Sec C(2))
  DO CASE
    CASE lcRpSortBy='O'
      INDEX ON (SECT_FLAG+SORT_DATE+REF) TAG (lcTmpProd) OF (lcTmpProd)
    CASE lcRpSortBy='C'
      INDEX ON (SECT_FLAG+SORT_DATE+ACCOUNT) TAG (lcTmpProd) OF (lcTmpProd)
    CASE lcRpSortBy='F'
      INDEX ON (SECT_FLAG+SORT_DATE+VENDOR) TAG (lcTmpProd) OF (lcTmpProd)
    CASE lcRpSortBy='S'
      INDEX ON (SECT_FLAG+SORT_DATE+STYLE) TAG (lcTmpProd) OF (lcTmpProd)
    OTHERWISE    
      INDEX ON (SECT_FLAG+SORT_DATE+DTOS(complete)) TAG (lcTmpProd) OF (lcTmpProd)
  ENDCASE
ENDIF
*-->End of Function lfCrtTemp.
*!****************************************************************************
*! Name      : lfGetSales
*! Developer : Nader Nabil (NNA)
*! Date      : 01/07/2008
*! Purpose   : Collecting data for the Sales Schedule report
*!****************************************************************************
*! Called from : salprdsc.prg
*!****************************************************************************
*! Passed Parameters : None
*!****************************************************************************
*! Return      : None
*!****************************************************************************
*! Example     : = lfGetSales()
*!****************************************************************************
FUNCTION lfGetSales
PRIVATE RECCOUNT , CPICK_TOTQ , lcAvlSizes , lnMonth , lnYear , lnSclCnt ,lnBomScCnt ,Rpt_Grp ,;
        Rpt_Sc_hdr , ORD_STYLE , ORD_ORDER
DIMENSION laOrdBomSz[1]
STORE 0 TO lnMonth , lnYear , lnSclCnt ,lnBomScCnt , ORD_ORDER
STORE '' TO Rpt_Grp , Rpt_Sc_hdr , ORD_STYLE 
SELECT ORDHDR
=SEEK('O')
SCAN REST WHILE CORDTYPE+ORDER ='O' FOR INLIST(STATUS,'O','H','X','C')
  WAIT WINDOW 'Checking Order : ' + ORDHDR.ORDER NOWAIT
  *--> Initialize the variable that hold the cutpick records that related to the sales order to zero.
  STORE 0 TO RECCOUNT,CPICK_TOTQ
  SELECT ORDLINE
  SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O'+ORDHDR.ORDER 
    IF IIF(!EMPTY(ldRPFromD),BETWEEN(ORDLINE.COMPLETE,ldRPFromD,ldRPToD) , ORDLINE.COMPLETE <=ldRPToD) OR;
       (ORDHDR.STATUS='X' AND BETWEEN(ORDHDR.CANCELLED,ldRPToD-7,ldRPToD)) OR ;
       (!INLIST(ORDHDR.STATUS,'X','C') AND BETWEEN(ORDHDR.ENTERED,ldRPToD-7,ldRPToD))
      ORD_STYLE = ORDLINE.STYLE
      ORD_ORDER = ORDLINE.ORDER
      *-->Reject cancelled orders and cancelled date not in the past 7 days
      IF ORDHDR.STATUS='X' AND !BETWEEN(ORDHDR.CANCELLED,DATE()-7,DATE()) 
        LOOP
      ENDIF
      *-->Include open/hold sales orders lines with a complete date in an earlier month in the current month.  
      lnMonth    = IIF(MONTH(ORDLINE.COMPLETE)<=MONTH(DATE()) AND EMPTY(ldRPFromD) ,MONTH(DATE()),MONTH(ORDLINE.COMPLETE))
      lnYear     = YEAR(ORDLINE.COMPLETE)
      Rpt_Grp    = IIF(ORDLINE.COMPLETE<=DATE() AND EMPTY(ldRPFromD),STR(YEAR(DATE()),4)+PADL(ALLTRIM(STR(MONTH(DATE()))),2,'0'),;
                      STR(YEAR(ORDLINE.COMPLETE),4)+PADL(ALLTRIM(STR(MONTH(ORDLINE.COMPLETE))),2,'0'))
      Rpt_Sc_hdr = IIF(ORDHDR.STATUS='X','CANCELLED THIS WEEK' ,;
                      'SALES '+IIF(ORDLINE.COMPLETE<=DATE() AND EMPTY(ldRPFromD),SUBSTR(CMONTH(Date()),1,3)+' '+STR(YEAR(Date()),4),;                    
                         SUBSTR(CMONTH(ORDLINE.COMPLETE),1,3)+' '+STR(YEAR(ORDLINE.COMPLETE),4)))
      SELECT (lcTmpSales)
      APPEND BLANK
      REPLACE REF         WITH ORDLINE.ORDER              ,;
              Status      WITH ORDHDR.Status              ,;
              ACCOUNT     WITH ORDHDR.CDIVISION           ,;
              STYLE       WITH SUBSTR(ORDLINE.STYLE,1,12) ,;
              Desc        WITH STYLE.DESC+SUBSTR(ORDLINE.STYLE,13,19),;
              PosDel      WITH PADL(DAY(ORDLINE.COMPLETE),2,'0')+'-'+SUBSTR(CMONTH(ORDLINE.COMPLETE),1,3),;
              Sales_Price WITH ROUND(ORDLINE.PRICE/ORDHDR.NEXRATE,2),;
              DISC        WITH lfCalcDisc(ROUND(ORDLINE.PRICE/ORDHDR.NEXRATE,2)),;
              Complete    WITH ORDLINE.COMPLETE           ,;
              Entered     WITH ORDHDR.Entered             ,;
              Cancelled   WITH ORDHDR.Cancelled           ,;
              SORT_DATE   WITH Rpt_Grp                    ,;
              TotQty      WITH ORDLINE.TOTQTY             ,;
              Store       WITH ORDLINE.STORE              ,;
              SECT_FLAG   WITH IIF(ORDHDR.STATUS='X','2','1'+Rpt_Grp),;
              SECT_HDR    WITH Rpt_Sc_hdr                 
              
      *--> get the Sales Qty. = ORDLINE.TOTQty
      IF SEEK(ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'ORDCANLN') 
        REPLACE SalesQty WITH ORDLINE.TOTQty
      ELSE
        REPLACE SalesQty WITH ORDLINE.TOTQTY
      ENDIF
      *-->Get Qty in Work by checking for PO related to the sales order.
      IF SEEK('2'+ ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'CUTPICK') 
        SELECT COUNT(CUTPICK.STYLE) from CUTPICK WHERE CUTPICK.STYLE=ORD_STYLE AND CUTPICK.ORDER=ORD_ORDER INTO ARRAY CNT_RECS
        SELECT CUTPICK
        SCAN REST WHILE TRANCD+ORDER+CORDLINE = '2'+ ORDLINE.ORDER+STR(ORDLINE.LINENO,6) ;
          FOR CUTPICK.STYLE=ORDLINE.STYLE
          IF SEEK ('P'+CUTPICK.CTKTNO,'POSHDR') 
            DIMENSION laPoQTY[1] , laOrdQty[1]
            STORE 0 TO laPoQTY[1],laOrdQty[1]
            lcPo      = POSHDR.PO
            SELECT SUM(POSLN.TOTQTY) FROM POSLN WHERE POSLN.PO+POSLN.TRANCD+POSLN.STYLE =lcPo +'2'+ORD_STYLE ;
                  INTO ARRAY laPoQTY              
            *--> Point 2.12.1 and 2.12.2
            IF SEEK(POSHDR.CSTYTYPE+POSHDR.PO+CUTPICK.STYLE+CUTPICK.CTKTLINENO+'1','POSLN') 
              IF INLIST(POSHDR.STATUS,'O','H') 
                SELECT (lcTmpSales)
                IF CNT_RECS[1]=1
                  SELECT (lcTmpSales)
                  REPLACE Qty_Wrk WITH POSLN.TOTQTY-laPoQty[1] , VENDOR WITH POSHDR.VENDOR
                ELSE
                  lnstyQty = ORDLINE.TOTQTY
                  SELECT SUM(TOTQTY) FROM ORDLINE WHERE ORDLINE.STYLE=ORD_STYLE AND ORDLINE.ORDER = ORD_ORDER  INTO ARRAY laOrdQty
                  SELECT (lcTmpSales)
                  IF laOrdQty[1]>lnstyQty 
                    IF lnstyQty>0 
                      REPLACE Qty_Wrk WITH MAX(ROUND((POSLN.TOTQTY-laPoQty[1])*(lnstyQty/laOrdQty[1]),0),0) , VENDOR WITH POSHDR.VENDOR
                    ENDIF
                  ELSE
                    REPLACE Qty_Wrk WITH MAX((POSLN.TOTQTY-laPoQty[1]),0) , VENDOR WITH POSHDR.VENDOR
                  ENDIF
                ENDIF
              ENDIF    
            ENDIF
            *--> Point [2.12.3] , [2.12.4] , [2.13.1] and [2.13.2]
            IF INLIST(POSHDR.STATUS,'A','C') 
              IF SEEK(CUTPICK.CTKTNO+CUTPICK.STYLE+CUTPICK.CTKTLINENO,'POCUT')
                SELECT (lcTmpSales)
                IF CNT_RECS[1]=1 
                  REPLACE Qty_Wrk WITH IIF(POSHDR.STATUS='A' ,MAX(POCUT.NTOTWO-laPoQty[1],0) ,0),;
                          Qty_Cut WITH MAX(POCUT.TOTCUT-laPoQty[1],0)
                ELSE
                  DIMENSION laTOTQTY[1]
                  laTOTQTY[1]=0
                  POCUT_PO  = POCUT.PO
                  POCUT_LNO = STR(POCUT.LINENO,6)
                  *SELECT SUM(CUTPICK.TOTQTY) FROM CUTPICK WHERE CUTPICK.CTKTNO=POCUT_PO AND CUTPICK.CTKTLINENO=POCUT_LNO;
                  *      INTO ARRAY laTOTQTY
                  SELECT (lcTmpSales)
                  REPLACE Qty_Wrk WITH IIF(POSHDR.STATUS='A' ,IIF(POCUT.NTOTWO>0 ,MAX(ROUND((ORDLINE.TOTQTY/POCUT.NTOTWO)*(POCUT.NTOTWO-laPoQty[1]),0),0),0) ,0),;
                          Qty_Cut WITH IIF(POCUT.NTOTWO>0 ,MAX(ROUND((ORDLINE.TOTQTY/POCUT.NTOTWO)*(POCUT.TOTCUT-laPoQty[1]),0),0),0)
                ENDIF
              ENDIF    
            ENDIF
          ENDIF
        ENDSCAN       
      ENDIF
      *-->Get Expt Desp. units (point 2.25)
      SELECT (lcTmpSales)
      REPLACE Expt_Desp WITH IIF(!EMPTY(Qty_Cut),Qty_Cut,;
                             IIF(!EMPTY(Qty_Wrk),Qty_Wrk,;
                             IIF(!EMPTY(SalesQty),SalesQty,0)))
      *--> Get the CMT Price , Cost Price and Total Cost Price
      IF SEEK(ORDLINE.ORDER,'ORDBOM')
        *-->change order of the style table to seek for OrdBom style
        SELECT STYLE
        SET ORDER TO TAG CSTYLE INTO STYLE
        SELECT ORDBOM
        SCAN REST WHILE ORDER+STR(LINENO,6)+STYLE+SCLR+ITEM+ICLR+MFGCODE=ORDLINE.ORDER ;
             FOR ALLTRIM(STYLE)=ALLTRIM(SUBSTR(ORDLINE.STYLE,1,lnMajorLen))
          *-->Get Style's sizes count from the Scale table
          IF SEEK(STYLE ,'STYLE')
            IF SEEK('S'+STYLE.Scale,'SCALE')
               lnSclCnt=SCALE.Cnt
            ENDIF
          ENDIF
          SELECT ORDBOM
          IF TYP='1' THEN
            SELECT (lcTmpSales)
            REPLACE CMT_PRICE WITH ORDBOM.UNTCOST
          ENDIF
          *-->Get the Cost Price and Total Cost Price
          SELECT ORDBOM
          lnBomScCnt=lnSclCnt
          FOR lnMln=1 TO MEMLINES(MSizes)
            IF OCCURS(STYLE.Scale,MLINE(MSizes,lnMln)) <> 0
              lnBomScCnt=OCCURS(',',MLINE(MSizes,lnMln))+1
              EXIT
            ENDIF
          ENDFOR
          *-->Get Cost Price (point 2.23)
          *-->First for Msizes includes all the sizes
          IF SUBSTR(CITMMASK,14,19)="*****"  OR (SUBSTR(CITMMASK,14,19)=SUBSTR(ORDLINE.STYLE,14,19) AND ;
            lnBomScCnt=lnSclCnt) THEN
            SELECT (lcTmpSales)
            REPLACE Cost_Price WITH Cost_Price+ROUND((ORDBOM.UNTCOST*ORDBOM.NBOMTOTQTY),5)

          *-->Second for Msizes includes Some sizes even one size or more
          ELSE
            IF SUBSTR(CITMMASK,14,19)<>"*****" AND lnBomScCnt<lnSclCnt THEN
              lcOrdbomSz = ALLTRIM(SUBSTR(MLINE(ORDBOM.mSizes,1),AT('~',MLINE(ORDBOM.mSizes,1))+1))
              lnOrdSzQT=0
              IF lnBomScCnt>1 THEN
                =GFSUBSTR(lcOrdbomSz,@laOrdBomSz,',')
                FOR I=1 TO ALEN(laOrdBomSz,1)
                  SELECT SUM(ORDLINE.QTY&laOrdBomSz[I]) FROM ORDLINE WHERE ORDLINE.ORDER=ORD_ORDER AND;
                         ORDLINE.STYLE=ORD_STYLE ;
                         INTO ARRAY laTOTQTY
                  lnOrdSzQT=lnOrdSzQT+laTOTQTY[1]
                ENDFOR
                 SELECT SUM(ORDLINE.TOTQTY) FROM ORDLINE WHERE ORDLINE.ORDER=ORD_ORDER AND;
                        ORDLINE.STYLE=ORD_STYLE ;
                        INTO ARRAY laTOTQTY
                SELECT (lcTmpSales)
                REPLACE Cost_Price WITH Cost_Price+IIF(laTOTQTY[1]>0,;
                      ROUND(lnOrdSzQT*(ORDBOM.UNTCOST * ORDBOM.nBomTotQty)/laTOTQTY[1],5),0)
              ELSE
              SELECT SUM(ORDLINE.TOTQTY),SUM(ORDLINE.QTY&lcOrdbomSz) FROM ORDLINE WHERE ORDLINE.ORDER=ORD_ORDER AND;
                     ORDLINE.STYLE=ORD_STYLE ;
                     INTO ARRAY laTOTQTY
                SELECT (lcTmpSales)
                REPLACE Cost_Price WITH Cost_Price+IIF(laTOTQTY[1]>0,;
                   ROUND(laTOTQTY[2]*(ORDBOM.UNTCOST * ORDBOM.nBomTotQty)/laTOTQTY[1],5),0)
              ENDIF
            ENDIF
          ENDIF
        ENDSCAN
        SELECT STYLE
        SET ORDER TO TAG &lcOrder INTO STYLE
      ENDIF
      *-->Get the Total Disc Value (Point 2.29)
      SELECT (lcTmpSales)
      REPLACE &lcTmpSales..TotDis_Val WITH &lcTmpSales..Expt_Desp*&lcTmpSales..Disc
      *-->Get Total sales Value (Point 2.26)
      SELECT (lcTmpSales)
      IF ORDHDR.STATUS<>'X' 
        REPLACE &lcTmpSales..Tot_SalVal WITH &lcTmpSales..Expt_Desp*&lcTmpSales..Sales_Price
        IF BETWEEN(ORDHDR.ENTERED,DATE()-7,DATE()) 
          REPLACE  &lcTmpSales..TWO_SEC    WITH '3'
        ENDIF
      ENDIF
      *--> Get deleivers this Week and Month
      MY_COUNTER = 0
      DO CASE
         CASE SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'INVLINE') AND INVHDR.STATUS<>'X' 
           SELECT INVLINE
           *-->Scan for ordline.lineno that if the line has multi shipped 
           SCAN REST WHILE ORDER+STR(LINENO,6) = ORDLINE.ORDER+STR(ORDLINE.LINENO,6) 
             IF INVHDR.STATUS='V'
               LOOP
             ENDIF
             MY_COUNTER = MY_COUNTER + 1
             IF (MONTH(INVLINE.INVDATE)=MONTH(DATE()) AND YEAR(INVLINE.INVDATE)=YEAR(DATE())) OR;
                BETWEEN(INVLINE.INVDATE,DATE(),DATE()+7) 
                IF MY_COUNTER <=1
                  SELECT (lcTmpSales)
                  REPLACE ExDesp_DEL WITH INVLINE.TOTQTY ,;
                          INVDATE    WITH INVLINE.INVDATE,;                  
                          TWO_SEC    WITH IIF(BETWEEN(INVLINE.INVDATE,DATE()-7,DATE()),'4','5')
                ELSE
                  SELECT (lcTmpSales)
                  SCATTER MEMVAR MEMO
                  APPEND BLANK
                  GATHER MEMVAR MEMO  
                  *-->I put '51' and '41' for the duplicate lines in invline to remove them after that from sales section.
                  REPLACE ExDesp_DEL WITH INVLINE.TOTQTY ,;
                          INVDATE    WITH INVLINE.INVDATE,;
                          TWO_SEC    WITH IIF(BETWEEN(INVLINE.INVDATE,DATE()-7,DATE()),'41','51')
                ENDIF
             ENDIF
           ENDSCAN
         CASE SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'CONSINVL') AND CONSINVH.STATUS<>'X' 
           SELECT CONSINVL
           *-->Scan for ordline.lineno that if the line has multi shipped 
           SCAN REST WHILE ORDER+STR(LINENO,6) = ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
             IF CONSINVH.STATUS='V'
               LOOP
             ENDIF
             MY_COUNTER = MY_COUNTER + 1
             IF (MONTH(INVLINE.INVDATE)=MONTH(DATE()) AND YEAR(INVLINE.INVDATE)=YEAR(DATE())) OR;
               BETWEEN(INVDATE,DATE(),DATE()+7) 
                IF MY_COUNTER <=1
                   SELECT (lcTmpSales)
                   REPLACE ExDesp_DEL WITH ExDesp_DEL+CONSINVL.TOTQTY ,;
                           INVDATE    WITH CONSINVL.INVDATE,;
                           TWO_SEC    WITH IIF(BETWEEN(CONSINVL.INVDATE,DATE()-7,DATE()),'4','5')
                ELSE
                  SELECT (lcTmpSales)
                  SCATTER MEMVAR MEMO
                  APPEND BLANK
                  GATHER MEMVAR MEMO  
                  *-->I put '51' and '41' for the duplicate lines in invline to remove them after that from sales section.
                  REPLACE ExDesp_DEL WITH CONSINVL.TOTQTY ,;
                          INVDATE    WITH CONSINVL.INVDATE,;                  
                          TWO_SEC    WITH IIF(BETWEEN(CONSINVL.INVDATE,DATE()-7,DATE()),'41','51')
                ENDIF
             ENDIF
           ENDSCAN
         OTHERWISE
           *-->(DATE()-DOW(DATE())+1) To get the first day of the current week
           *-->(DATE()+7-DOW(DATE())) To get the last day of the current week
           IF !EMPTY(ORDLINE.TOTPIK) AND (MONTH(ORDLINE.PIKDATE)=MONTH(DATE()) AND YEAR(ORDLINE.PIKDATE)=YEAR(DATE())) OR;
             BETWEEN(ORDLINE.PIKDATE, DATE()-DOW(DATE())+1 , DATE()+7-DOW(DATE()))
             SELECT (lcTmpSales)
             REPLACE ExDesp_DEL WITH ORDLINE.TOTPIK ,;
                     TWO_SEC    WITH IIF(BETWEEN(ORDLINE.PIKDATE,DATE()-7,DATE()),'4','5')
           ENDIF
      ENDCASE
    ENDIF
  ENDSCAN
ENDSCAN

*--> This section to duplicate records that must be show in two sections ,
*--> Get the Gross Margin (Point 2.27) and 
*--> Get the Nett Margin (Point 2.28)    
SELECT (lcTmpSales) 
REPLACE ALL TotCostPr  WITH (ROUND(Cost_Price,2) * Expt_Desp),;
            Gross_Marg WITH IIF(Tot_SalVal>0,ROUND((((Tot_SalVal-TotCostPr)/Tot_SalVal)*100),0),0),;
            Net_Marg   WITH IIF(TotDis_Val>0,ROUND((((TotDis_Val - TotCostPr)/TotDis_Val)*100),0),0)  ;
     FOR STATUS<>'X'  

SELECT (lcTmpSales) 
LOCATE 
SCAN FOR !EMPTY(TWO_SEC)
  REC_NO = RECNO()
  IF !(TWO_SEC == SUBSTR(ALLTRIM(SECT_FLAG),1,1))
    SCATTER MEMVAR MEMO
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE SECT_FLAG WITH TWO_SEC ,;
          SECT_HDR  WITH IIF(INLIST(TWO_SEC,'4','41') , 'DELIVERIES THIS WEEK' , IIF(TWO_SEC='3','SALES THIS WEEK',;
                           'DELIVERIES '+SUBSTR(CMONTH(INVDATE),1,3)+' '+STR(YEAR(INVDATE),4)))
    REPLACE TWO_SEC   WITH '' &&Empty the filed to not scan it again
  ENDIF
  GO RECORD REC_NO
  IF (!INLIST(ALLTRIM(TWO_SEC),'3','4') AND SalesQty<=0) OR (INLIST(ALLTRIM(TWO_SEC),'41','51'))
    DELETE
  ENDIF
ENDSCAN
DELETE ALL FOR SUBSTR(ALLTRIM(EVAL(lcTmpSales+'.Sect_Flag')),1,1)='1' AND TOTQTY=0
REPLACE ALL Sect_Flag  WITH IIF(Sect_Flag='41' , '4','5') FOR INLIST(SECT_FLAG , '41','51')
REPLACE ALL SalesQty   WITH SalesQty + ExDesp_DEL ,;
            Expt_Desp  WITH IIF(!EMPTY(Qty_Cut),Qty_Cut,IIF(!EMPTY(Qty_Wrk),Qty_Wrk,IIF(!EMPTY(SalesQty),SalesQty,0))),;
            TotDis_Val WITH ExDesp_DEL*Disc ,;
            Tot_SalVal WITH ExDesp_DEL*Sales_Price ,;
            TotCostPr  WITH (ROUND(Cost_Price,2) * ExDesp_DEL),;
            Gross_Marg WITH IIF(Tot_SalVal>0,ROUND((((Tot_SalVal-TotCostPr)/Tot_SalVal)*100),0),0),;
            Net_Marg   WITH IIF(TotDis_Val>0,ROUND((((TotDis_Val - TotCostPr)/TotDis_Val)*100),0),0) ;
    FOR INLIST(ALLTRIM(SECT_FLAG) , '4','5')
*-->End of Function lfGetSales.
*!****************************************************************************
*! Name      : lfGetProd
*! Developer : Nader Nabil (NNA)
*! Date      : 01/07/2008
*! Purpose   : Collecting data for the Production Schedule report
*!****************************************************************************
*! Called from : salprdsc.prg
*!****************************************************************************
*! Passed Parameters : None
*!****************************************************************************
*! Return      : None
*!****************************************************************************
*! Example     : = lfGetProd()
*!****************************************************************************
FUNCTION lfGetProd
PRIVATE RECCOUNT , CPICK_TOTQ , lcAvlSizes , lnMonth , lnYear , lnSclCnt ,lnBomScCnt ,Rpt_Grp ,;
        Rpt_Sc_hdr , ORD_STYLE , ORD_ORDER , C_CTECH
DIMENSION laOrdBomSz[1]
STORE 0 TO lnMonth , lnYear , lnSclCnt ,lnBomScCnt , ORD_ORDER
STORE '' TO Rpt_Grp , Rpt_Sc_hdr , ORD_STYLE , C_CTECH

SELECT ORDHDR
=SEEK('O')
SCAN REST WHILE CORDTYPE+ORDER ='O' FOR INLIST(STATUS,'O','H','X','C')
  WAIT WINDOW 'Checking Order : ' + ORDHDR.ORDER NOWAIT
  *--> Initialize the variable that hold the cutpick records that related to the sales order to zero.
  STORE 0 TO RECCOUNT,CPICK_TOTQ
  SELECT ORDLINE
  SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O'+ORDHDR.ORDER
    IF IIF(!EMPTY(ldRPFromD),BETWEEN(ORDLINE.COMPLETE,ldRPFromD,ldRPToD) , ORDLINE.COMPLETE <=ldRPToD) OR;
       (ORDHDR.STATUS='X' AND BETWEEN(ORDHDR.CANCELLED,ldRPToD-7,ldRPToD)) OR ;
       (!INLIST(ORDHDR.STATUS,'X','C') AND BETWEEN(ORDHDR.ENTERED,ldRPToD-7,ldRPToD))
      ORD_STYLE = ORDLINE.STYLE
      ORD_ORDER = ORDLINE.ORDER
      *-->Reject cancelled orders and cancelled date not in the past 7 days
      IF ORDHDR.STATUS='X' AND !BETWEEN(ORDHDR.CANCELLED,DATE()-7,DATE()) 
        LOOP
      ENDIF
      *-->Include open/hold sales orders lines with a complete date in an earlier month in the current month.  
      lnMonth    = IIF(MONTH(ORDLINE.COMPLETE)<=MONTH(DATE()) AND EMPTY(ldRPFromD) ,MONTH(DATE()),MONTH(ORDLINE.COMPLETE))
      lnYear     = YEAR(ORDLINE.COMPLETE)
      Rpt_Grp    = IIF(ORDLINE.COMPLETE<=DATE() AND EMPTY(ldRPFromD),STR(YEAR(DATE()),4)+PADL(ALLTRIM(STR(MONTH(DATE()))),2,'0'),;
                      STR(YEAR(ORDLINE.COMPLETE),4)+PADL(ALLTRIM(STR(MONTH(ORDLINE.COMPLETE))),2,'0'))
      Rpt_Sc_hdr = IIF(ORDHDR.STATUS='X','CANCELLED THIS WEEK' ,;
                      'SALES '+IIF(ORDLINE.COMPLETE<=DATE() AND EMPTY(ldRPFromD),SUBSTR(CMONTH(Date()),1,3)+' '+STR(YEAR(Date()),4),;                    
                         SUBSTR(CMONTH(ORDLINE.COMPLETE),1,3)+' '+STR(YEAR(ORDLINE.COMPLETE),4)))
      IF SEEK('N'+'CTECH     '+STYLE.CTECH,'CODES')
        C_CTECH = SUBSTR(CODES.CDISCREP,1,3)
      ENDIF
      SELECT (lcTmpProd)
      APPEND BLANK
      REPLACE REF         WITH ORDLINE.ORDER              ,;
              ACCOUNT     WITH ORDHDR.CDIVISION           ,;
              STYLE       WITH SUBSTR(ORDLINE.STYLE,1,12) ,;
              Desc        WITH STYLE.DESC+SUBSTR(ORDLINE.STYLE,13,19),;
              PosDel      WITH PADL(DAY(ORDLINE.COMPLETE),2,'0')+'-'+SUBSTR(CMONTH(ORDLINE.COMPLETE),1,3),;
              Complete    WITH ORDLINE.COMPLETE           ,;
              Entered     WITH ORDHDR.Entered             ,;
              New_Rpt     WITH IIF(ORDHDR.CReorder='N','New','Rpt'),;
              Cancelled   WITH ORDHDR.Cancelled           ,;
              SORT_DATE   WITH Rpt_Grp                    ,;
              TotQty      WITH ORDLINE.TOTQTY             ,;              
              Comments    WITH SUBSTR(MLINE(NOTEPAD.MNOTES,1),1,30),;
              Ctech       WITH C_CTECH                    ,;
              Store       WITH ORDLINE.STORE              ,;
              SECT_FLAG   WITH IIF(ORDHDR.STATUS='X','2','1'+Rpt_Grp),;
              SECT_HDR    WITH Rpt_Sc_hdr

      *--> get the Sales Qty. = ORDLINE.TotQty
      IF SEEK(ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'ORDCANLN') 
        REPLACE SalesQty WITH ORDLINE.TOTQTY
      ELSE
        REPLACE SalesQty WITH ORDLINE.TOTQTY
      ENDIF
      *-->Get Qty in Work by checking for PO related to the sales order.
      IF SEEK('2'+ ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'CUTPICK') 
        SELECT COUNT(CUTPICK.STYLE) from CUTPICK WHERE CUTPICK.STYLE=ORD_STYLE AND CUTPICK.ORDER=ORD_ORDER INTO ARRAY CNT_RECS
        SELECT CUTPICK
        SCAN REST WHILE TRANCD+ORDER+CORDLINE = '2'+ ORDLINE.ORDER+STR(ORDLINE.LINENO,6) ;
          FOR CUTPICK.STYLE=ORDLINE.STYLE
          IF SEEK ('P'+CUTPICK.CTKTNO,'POSHDR') 
            DIMENSION laPoQTY[1] , laOrdQty[1]
            STORE 0 TO laPoQTY[1],laOrdQty[1]
            lcPo      = POSHDR.PO
            SELECT SUM(POSLN.TOTQTY) FROM POSLN WHERE POSLN.PO+POSLN.TRANCD+POSLN.STYLE =lcPo +'2'+ORD_STYLE ;
                  INTO ARRAY laPoQTY              
            *-->Get Dkt. No And Vendor
            SELECT (lcTmpProd)        
            REPLACE CTKTNO    WITH CUTPICK.CTKTNO ,;
                    VENDOR    WITH POSHDR.VENDOR  ,;
                    Fact_Delv WITH PADL(DAY(POSHDR.COMPLETE),2,'0')+'-'+SUBSTR(CMONTH(POSHDR.COMPLETE),1,3)
            *--> Get I/W (Issue_qty)
            IF POSHDR.STATUS<>'H' AND SEEK('I'+POSHDR.PO+'2','CTKTBOM')
              SELECT CTKTBOM
              First_Rec = 1 && counter to get only the first record of ctktbom table with ccatgtyp='f'
              SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = 'I'+POSHDR.PO+'2' FOR CCATGTYP='F'
                IF CTKTBOM.ISSUE_QTY>0 AND FIRST_REC<=1
                  FIRST_REC=FIRST_REC+1
                  SELECT (lcTmpProd)                          
                  *REPLACE ISSUE_QTY WITH CTKTBOM.ISSUE_QTY
                  REPLACE ISSUE_QTY WITH 'I/W'
                ENDIF
              ENDSCAN            
            ENDIF
            IF SEEK(POSHDR.CSTYTYPE+POSHDR.PO+CUTPICK.STYLE+CUTPICK.CTKTLINENO+'1','POSLN') 
              IF INLIST(POSHDR.STATUS,'O','H') 
                SELECT (lcTmpProd)
                IF CNT_RECS[1]=1 
                  REPLACE Qty_Wrk WITH MAX(POSLN.TOTQTY-laPoQty[1],0) , VENDOR WITH POSHDR.VENDOR
                ELSE
                  lnstyQty = ORDLINE.TOTQTY
                  SELECT SUM(TOTQTY) FROM ORDLINE WHERE ORDLINE.STYLE=ORD_STYLE AND ORDLINE.ORDER = ORD_ORDER  INTO ARRAY laOrdQty
                  SELECT (lcTmpProd)
                  IF laOrdQty[1]>lnstyQty 
                    IF lnstyQty>0 
                      REPLACE Qty_Wrk WITH MAX(ROUND((POSLN.TOTQTY-laPoQty[1])*(lnstyQty/laOrdQty[1]),0),0) , VENDOR WITH POSHDR.VENDOR
                    ENDIF
                  ELSE
                    REPLACE Qty_Wrk WITH MAX((POSLN.TOTQTY-laPoQty[1]),0) , VENDOR WITH POSHDR.VENDOR
                  ENDIF
                ENDIF
              ENDIF    
            ENDIF
            IF INLIST(POSHDR.STATUS,'A','C') 
              IF SEEK(CUTPICK.CTKTNO+CUTPICK.STYLE+CUTPICK.CTKTLINENO,'POCUT')
                SELECT (lcTmpProd)
                IF CNT_RECS[1]=1 
                  REPLACE Qty_Wrk WITH IIF(POSHDR.STATUS='A' ,MAX(POCUT.NTOTWO-laPoQty[1],0) ,0),;
                          Qty_Cut WITH MAX(POCUT.NTOTWO-laPoQty[1],0)
                ELSE
                  DIMENSION laTOTQTY[1]
                  laTOTQTY[1] =0
                  POCUT_PO    = POCUT.PO
                  POCUT_LNO   = STR(POCUT.LINENO,6)
                  SELECT SUM(CUTPICK.TOTQTY) FROM CUTPICK WHERE CUTPICK.CTKTNO=POCUT_PO AND CUTPICK.CTKTLINENO=POCUT_LNO;
                        INTO ARRAY laTOTQTY
                  SELECT (lcTmpProd)
                  REPLACE Qty_Wrk WITH IIF(POSHDR.STATUS='A' ,CUTPICK.TOTQTY ,0),;
                          Qty_Cut WITH IIF(laTOTQTY[1]>0 ,MAX(ROUND((CUTPICK.TOTQTY/laTOTQTY[1])*(POCUT.NTOTWO-laPoQty[1]),0),0),0)
                ENDIF
              ENDIF    
            ENDIF
          ENDIF
        ENDSCAN       
      *--> IF there is no PO related to the Sales Order
      ELSE
        SELECT (lcTmpProd)    
        REPLACE VENDOR WITH STYLE.VENDOR
      ENDIF
      *--> Get the CMT Price , Cost Price and Total Cost Price
      IF SEEK(ORDLINE.ORDER,'ORDBOM')
        *-->change order of the style table to seek for OrdBom style
        SELECT STYLE
        SET ORDER TO TAG CSTYLE INTO STYLE
        SELECT ORDBOM
        SCAN REST WHILE ORDER+STR(LINENO,6)+STYLE+SCLR+ITEM+ICLR+MFGCODE=ORDLINE.ORDER ;
             FOR ALLTRIM(STYLE)=ALLTRIM(SUBSTR(ORDLINE.STYLE,1,lnMajorLen))

          *-->Get Style's sizes count from the Scale table
          IF SEEK(STYLE ,'STYLE')
            IF SEEK('S'+STYLE.Scale,'SCALE')
               lnSclCnt=SCALE.Cnt
            ENDIF
          ENDIF
        ENDSCAN
        SELECT STYLE
        SET ORDER TO TAG &lcOrder INTO STYLE
      ENDIF
      *--> Get deleivers this Week and Month
      MY_COUNTER = 0
      DO CASE
         CASE SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'INVLINE') AND INVHDR.STATUS<>'X' 
           SELECT INVLINE
           *-->Scan for ordline.lineno that if the line has multi shipped 
           SCAN REST WHILE ORDER+STR(LINENO,6) = ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
             IF INVHDR.STATUS='V'
               LOOP
             ENDIF
             MY_COUNTER = MY_COUNTER + 1
             IF (MONTH(INVLINE.INVDATE)=MONTH(DATE()) AND YEAR(INVLINE.INVDATE)=YEAR(DATE())) OR;
                BETWEEN(INVLINE.INVDATE,DATE(),DATE()+7) 
                IF MY_COUNTER <=1
                  SELECT (lcTmpProd)
                  REPLACE ExDesp_DEL WITH INVLINE.TOTQTY ,;
                          INVDATE    WITH INVLINE.INVDATE,;                  
                          TWO_SEC    WITH IIF(BETWEEN(INVLINE.INVDATE,DATE()-7,DATE()),'4','5')
                ELSE
                  SELECT (lcTmpProd)
                  SCATTER MEMVAR MEMO
                  APPEND BLANK
                  GATHER MEMVAR MEMO  
                  REPLACE ExDesp_DEL WITH INVLINE.TOTQTY ,;
                          INVDATE    WITH INVLINE.INVDATE,;                  
                          TWO_SEC    WITH IIF(BETWEEN(INVLINE.INVDATE,DATE()-7,DATE()),'41','51')
                ENDIF
             ENDIF
           ENDSCAN
         CASE SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'CONSINVL') AND CONSINVH.STATUS<>'X' 
           SELECT CONSINVL
           *-->Scan for ordline.lineno that if the line has multi shipped 
           SCAN REST WHILE ORDER+STR(LINENO,6) = ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
             IF CONSINVH.STATUS='V'
               LOOP
             ENDIF
             MY_COUNTER = MY_COUNTER + 1
             IF (MONTH(INVLINE.INVDATE)=MONTH(DATE()) AND YEAR(INVLINE.INVDATE)=YEAR(DATE())) OR;
               BETWEEN(INVDATE,DATE(),DATE()+7) 
               IF MY_COUNTER <=1
                 SELECT (lcTmpProd)
                 REPLACE ExDesp_DEL WITH INVLINE.TOTQTY ,;
                         INVDATE    WITH CONSINVL.INVDATE,;                  
                         TWO_SEC    WITH IIF(BETWEEN(INVLINE.INVDATE,DATE()-7,DATE()),'4','5')
               ELSE
                 SELECT (lcTmpProd)
                 SCATTER MEMVAR MEMO
                 APPEND BLANK
                 GATHER MEMVAR MEMO  
                  *-->I put '51' and '41' for the duplicate lines in invline to remove them after that from sales section.
                 REPLACE ExDesp_DEL WITH INVLINE.TOTQTY ,;
                         INVDATE    WITH CONSINVL.INVDATE,;                                  
                         TWO_SEC    WITH IIF(BETWEEN(INVLINE.INVDATE,DATE()-7,DATE()),'41','51')
               ENDIF
             ENDIF
           ENDSCAN
         OTHERWISE
           *-->(DATE()-DOW(DATE())+1) To get the first day of the current week
           *-->(DATE()+7-DOW(DATE())) To get the last day of the current week
           IF !EMPTY(ORDLINE.TOTPIK) AND (MONTH(ORDLINE.PIKDATE)=MONTH(DATE()) AND YEAR(ORDLINE.PIKDATE)=YEAR(DATE())) OR;
             BETWEEN(ORDLINE.PIKDATE, DATE()-DOW(DATE())+1 , DATE()+7-DOW(DATE()))
             SELECT (lcTmpProd)
             REPLACE ExDesp_DEL WITH ORDLINE.TOTPIK ,;
                     TWO_SEC    WITH IIF(BETWEEN(ORDLINE.PIKDATE,DATE()-7,DATE()),'4','5')
           ENDIF
      ENDCASE
    ENDIF
  ENDSCAN
ENDSCAN
*-->This section to duplicate records that must be show in two sections i.e (
SELECT (lcTmpProd) 
LOCATE 
SCAN FOR !EMPTY(TWO_SEC)
  REC_NO = RECNO()
  IF !(TWO_SEC == SUBSTR(ALLTRIM(SECT_FLAG),1,1))
    SCATTER MEMVAR MEMO
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE SECT_FLAG WITH TWO_SEC ,;
          SECT_HDR  WITH IIF(INLIST(TWO_SEC,'4','41') , 'DELIVERIES THIS WEEK' , 'DELIVERIES '+SUBSTR(CMONTH(INVDATE),1,3)+' '+STR(YEAR(INVDATE),4))
    REPLACE TWO_SEC   WITH '' &&Empty the filed to not scan it again
  ENDIF
  GO RECORD REC_NO
  IF (!INLIST(ALLTRIM(TWO_SEC),'3','4') AND SalesQty<=0) OR (INLIST(ALLTRIM(TWO_SEC),'41','51'))
    DELETE
  ENDIF
ENDSCAN
DELETE ALL FOR SUBSTR(ALLTRIM(EVAL(lcTmpProd+'.Sect_Flag')),1,1)='1' AND TOTQTY=0
REPLACE ALL Sect_Flag WITH IIF(Sect_Flag='41' , '4','5') FOR INLIST(SECT_FLAG , '41','51')
REPLACE ALL SalesQty   WITH SalesQty + ExDesp_DEL ,;
            Expt_Desp  WITH IIF(!EMPTY(Qty_Cut),Qty_Cut,IIF(!EMPTY(Qty_Wrk),Qty_Wrk,IIF(!EMPTY(SalesQty),SalesQty,0))) ;
    FOR INLIST(ALLTRIM(SECT_FLAG) , '4','5')

*-->End of Function lfGetProd.
*!****************************************************************************
*! Name      : lfVpbOk
*! Developer : Nader Nabil (NNA)
*! Date      : 01/07/2008
*! Purpose   : Collecting data for the Production Schedule report
*!****************************************************************************
*! Called from : salprdsc.prg
*!****************************************************************************
*! Passed Parameters : None
*!****************************************************************************
*! Return      : None
*!****************************************************************************
*! Example     : = lfVpbOk()
*!****************************************************************************
FUNCTION lfVpbOk
IF EMPTY(ldTo)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'you can not leave the To date blank')
  SHOW GET pbRun DISABLE
  SHOW GET pbOGpreview DISABLE
  _CUROBJ=OBJNUM(ldTo)
  RETURN .F.
ELSE
  CLEAR READ
  RETURN .T.
ENDIF
*-->End of Function lfVpbOk.

*!****************************************************************************
*! Name      : lfSetRela
*! Developer : Nader Nabil (NNA)
*! Date      : 01/07/2008
*! Purpose   : Set relationship between tables 
*!****************************************************************************
*! Called from : salprdsc.prg
*!****************************************************************************
*! Passed Parameters : None
*!****************************************************************************
*! Return      : None
*!****************************************************************************
*! Example     : = lfSetRela()
*!****************************************************************************
FUNCTION lfSetRela
SELECT ORDHDR
SET RELATION TO Ordhdr.cordtype+ Ordhdr.order INTO ORDLINE ADDITIVE
SET RELATION TO 'B'+ORDHDR.ORDER INTO NOTEPAD ADDITIVE

SELECT ORDLINE
SET RELATION TO ORDLINE.Style INTO STYLE ADDITIVE
SET RELATION TO '2'+ ORDLINE.ORDER+STR(ORDLINE.LINENO,6) INTO CUTPICK ADDITIVE
SET RELATION TO ORDLINE.ORDER+STR(ORDLINE.LINENO,6) INTO CONSINVL ADDITIVE
SET RELATION TO ORDLINE.ORDER+STR(ORDLINE.LINENO,6) INTO INVLINE ADDITIVE
SET RELATION TO IIF(EMPTY(ORDLINE.STORE) , 'M' + ACCOUNT ,'S' + ACCOUNT + STORE) INTO CUSTOMER ADDITIVE

SELECT CUTPICK
SET RELATION TO 'P'+ CUTPICK.CTKTNO INTO POSHDR ADDITIVE
SET RELATION TO CUTPICK.CTKTNO+CUTPICK.STYLE+CUTPICK.CTKTLINENO INTO POCUT ADDITIVE

SELECT POSHDR
SET RELATION TO POSHDR.CSTYTYPE+ POSHDR.PO INTO POSLN ADDITIVE

SELECT INVLINE
SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE

SELECT CONSINVL
SET RELATION TO CONSINVL.INVOICE + CONSINVL.STORE + CONSINVL.ORDER INTO CONSINVH ADDITIVE
*-->End of Function lfSetRela.
*!***************************************************************************
*!* Name        : lfCalcDisc
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/06/2005
*!* Purpose     : Calculate the payment Terms Discount
*!***************************************************************************
*!* Called from : SalPrdSc.PRG
*!***************************************************************************
*!* Parameters  : lnSalPrice
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCalcDisc()
*!***************************************************************************
FUNCTION lfCalcDisc
PARAMETERS lnSalPrice
PRIVATE lnRetNum
STORE 0 TO lnRetNum
DECLARE laDisc1[1,2],laDDate1[1,2]
lnOldAlias = SELECT(0)
lnDisc1  = 0
lnDisc2  = 0
laDisc1[1,1]  = 'NTERDISCR'
laDisc1[1,2]  = 'lnDisc1'
=gfRltFld(ORDHDR.CTERMCODE,@laDisc1,'CTERMCODE')
lnRetNum = lnSalPrice - (lnSalPrice*lnDisc1)/100 
lnRetNum = lnRetNum -(lnRetNum*ORDHDR.DISC)/100
SELECT(lnOldAlias)
RETURN ROUND(lnRetNum,2)
*-- End of Function lfCalcDisc.
*!***************************************************************************
*!* Name        : lfChngForm
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/06/2005
*!* Purpose     : Calculate the payment Terms Discount
*!***************************************************************************
*!* Called from : Option Grid
*!***************************************************************************
*!* Parameters  : 
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChngForm()
*!***************************************************************************
FUNCTION lfChngForm
*-->Change the Report Frx based on the user selection for Report format.
lcRpForm=IIF(lcRpFormat='S' , 'SOSALESCH','SOPRODSCH')
*-- End of Function lfChngForm.
*!****************************************************************************
*!* Name        : lfOpnFiles
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 01/07/2008
*!* Purpose     : Open the needed files
*!****************************************************************************
*!* Called from : SALPRDSC.PRG
*!****************************************************************************
*!* Parameters  : llOpen     --> True for open files , False for closing files
*!*             : lcFilesExp --> Files that need to open
*!*             : lcTages    --> Tags for the opened files.
*!****************************************************************************
*!* Return      : None
*!****************************************************************************
*!* Example     : = lfOpnFiles()
*!****************************************************************************
FUNCTION lfOpnFiles
PARAMETERS llOpen,lcFilesExp,lcTages
DIMENSION laFiles[1],laTages[1]

=GFSUBSTR(lcFilesExp,@laFiles,',')
IF llOpen
  =GFSUBSTR(lcTages,@laTages,',')
  FOR I=1 TO ALEN(laFiles,1)
    IF USED(laFiles[I])
       USE IN laFiles[I]
    ENDIF
    =gfOpenFile(gcDataDir+laFiles[I],laTages[I],'SH')
  ENDFOR
ELSE
  FOR I=1 TO ALEN(laFiles,1)
    IF USED(laFiles[I])
      USE IN laFiles[I]
    ENDIF
  ENDFOR
ENDIF
*-- End of Function lfOpnFiles.
