**************************************************************************
*
*   FP Greige Goods Shortage Report
*
**************************************************************************

*-- Gets the value of the dates from the report criteria 
IF ASCAN(laOgFxFlt,'ORDHDR.START') = 0
  =gfModalGen("TRM00250B00000","DIALOG","Last Order StartShip Date") 
  RETURN
ELSE
  lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'ORDHDR.START'),1)
  lDSDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
  lDEDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
  IF EMPTY(lDSDate) .OR. EMPTY(lDEDate)
    =gfModalGen("TRM00250B00000","DIALOG","Last Order StartShip Date")   
    RETURN
  ENDIF
ENDIF

*-- Check For Order Status ( must be not empty )
IF ASCAN(laOgFxFlt,'ORDHDR.STATUS') = 0
  =gfModalGen('TRM00250B00000','DIALOG', 'ORDER STATUS') 
  RETURN
ELSE
  lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'ORDHDR.STATUS'),1)
  lCSTATUS = laOgFxFlt[lnInd,6]
  IF EMPTY(lCSTATUS) 
    =gfModalGen('TRM00250B00000','DIALOG','ORDER STATUS') 
    RETURN
  ENDIF
ENDIF

*-Call Function to set Relation 
=lfSetRel()

lnClrPos = 0				&& for color postion
lnStrtPos = ''				&& color start postion
lnFldLngth= 0				&& color length
DECLARE laOpenCt[8]			&& CUTTkt Open Qty

*- call function to get color postion
=lfGetPos()


**************************************************************************

*No need for this.
***CLOSE DATABASES
***SET TALK OFF
***CLEAR
***SET SAFETY OFF
* ---------------------------------------

* --- The following variables are put in for testing purposes only
* --- They need to be replaced by Selection Grid values
* --- and standard Aria Table references

***m.cAriaRoot = "N:\ARIA27"
m.cAriaRoot    = gcDef_Path
***m.cCompany  = "01"
m.cCompany = gcAct_Comp

*m.cHomeDir = m.cAriaRoot + "\ROE"
*m.cDefDir = m.cAriaRoot + "\ROE"

***m.cAriaDbfs = m.cAriaRoot + "\DBFS\" + m.cCompany
m.cAriaDbfs = gcDataDir

*SET DEFAULT TO &cDefDir
*SET PATH TO &cDefDir; &cAriaDbfs

***m.work_dir = m.cAriaRoot + "\WORK\"
m.work_dir = gcWorkDir
***m.scrn_dir = m.cAriaRoot + "\SCREENS\"
m.scrn_dir = gcScrDir
***m.prg_dir = m.cAriaRoot + "\PRGS\"
m.prg_dir = gcAppHome
***m.rpt_dir = m.cAriaRoot + "\REPORTS\"
m.rpt_dir = gcRepHome


*m.cOrdSort1 = "DTOC(START)"
*m.cOrdSort2 = "DTOC(COMPLETE)"
*m.cOrdSort3 = "PRIORITY"
*m.cOrdSort4 = "ORDER"
*m.cOrdSort = m.cOrdSort1 ;
   + "+" + m.cOrdSort2 ;
   + "+" + m.cOrdSort3 ;
   + "+" + m.cOrdSort4

m.cOrdSort = ''
llOrdChk = .F.
*- create the index expresion
FOR lnI = 1 TO 4
  IF !EMPTY(laSortAry[lnI,2])
    m.cOrdSort = ALLTRIM(m.cOrdSort + laSortAry[lnI,2]+'+')
  ENDIF
  IF laSortAry[lnI,2] = [ORDER+STORE+GROUP]
    llOrdChk = .T.			
  ENDIF
ENDFOR
IF !llOrdChk 
  m.cOrdSort = ALLTRIM(m.cOrdSort + 'ORDER+STORE+GROUP')
ELSE
  m.cOrdSort = SUBSTR(m.cOrdSort,1,LEN(m.cOrdSort)-1)
ENDIF


*---
***m.dbf = m.cAriaDbfs + "\style.dbf"
***USE &dbf IN 0
***m.dbf = m.cAriaDbfs + "\bomline.dbf"
***USE &dbf IN 0
=gfOpenFile(gcDataDir+'STYLE','','SH')
=gfOpenFile(gcDataDir+'BOMLINE','','SH')


* --- Style Selection Variables from 'Grid' ---
WAIT WINDOW " Building List Of Styles To Include In This Report... " NOWAIT
*SELECT DISTINCT cStyMajor;
   FROM DBF('style');
   WHERE !("-ZZZ" $ STYLE);
   AND !EMPTY(cStyMajor);
   INTO CURSOR styls

m.cstylist  = ""
lcSelStylst = IIF(!USED(laOgFxFlt[1,6]),"",laOgFxFlt[1,6])
IF !EMPTY(lcSelStylst)
  SELECT (lcSelStylst)
  SCAN
     m.cStyList = m.cStyList + ALLTRIM(cStyMajor) + ','
  ENDSCAN
ENDIF
*USE
*RELEASE styls
WAIT CLEAR

m.cseaslist = ""
m.cdivision = ""
m.cseaslist = laOgVrFlt[1,6]
m.cdivision = laOgVrFlt[2,6]


* --- Order Selection Variables from 'Grid' ---
*m.cStatList = "O,H"
m.cStatList = laOgFxFlt[4,6]

*m.dFStart = {01/01/1999}
*m.dLStart = DATE() + (6 * 7)
*m.dLStart = chk_century(m.dLStart)
m.dFStart = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
m.dLStart = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))



*m.ok_2_go = .F.
*m.rpt_grid = m.scrn_dir + "mfgrgrpt.spr"
*DO &rpt_grid

* ----- End of Test Variables Setup ------


*IF m.ok_2_go	&& IF Variables Input OK

   * --- Create Style/cStyMajor <-> Greige Style Reference Table ---
   *SET DEFAULT TO &cAriaDbfs
   WAIT WINDOW " Building Style to Greige Reference Table... " NOWAIT
   SELECT DISTINCT;
      bom.citmmajor AS STYLE,;
      bom.citmmajor AS cstymajor,;
      bom.item AS greige;
      FROM bom;
      WHERE !EMPTY(bom.item);
      AND bom.ccatgtyp = "S";
      ORDER BY 1;
      NOCONSOLE;
      INTO TABLE (m.work_dir + "stybom")

   * --- Build stybom Indices ---
   SELECT stybom
   INDEX ON STYLE TAG STYLE
   INDEX ON cstymajor TAG cstymajor UNIQUE
   INDEX ON greige TAG greige UNIQUE

   m.cGrgList = ""
   SELECT stybom
   SCAN
      m.cGrgList = m.cGrgList + ALLTRIM(greige) + ","
   ENDSCAN

   * --- Get Style Inventory ---
   * --- Get Greige Inventory ---
   WAIT WINDOW " Gathering Greige Goods Inventory Data... " NOWAIT
   SELECT STYLE,;
      LEFT(STYLE, 12) AS greige,;
      stk1,;
      stk2,;
      stk3,;
      stk4,;
      stk5,;
      stk6,;
      stk7,;
      stk8,;
      wip1,;
      wip2,;
      wip3,;
      wip4,;
      wip5,;
      wip6,;
      wip7,;
      wip8;
      FROM STYLE;
      WHERE ALLTRIM(style.style) $ m.cgrglist;
      NOCONSOLE;
      GROUP BY STYLE;
      ORDER BY STYLE;
      INTO TABLE (m.work_dir + "availgrg")

   SELECT availgrg
   INDEX ON greige TAG greige

   * --- Get True Open Dye WIP From Open Dye PO's ---
   *m.src1 = cAriaDbfs + "\posln.dbf"
   *USE &src1 IN 0 ALIAS posln
   =gfOpenFile(gcDataDir+'POSLN','','SH')
   *m.src2 = cAriaDbfs + "\poshdr.dbf"
   *USE &src2 IN 0 ALIAS poshdr
   =gfOpenFile(gcDataDir+'POSHDR','','SH')
  
   SELECT posln.style, ;
      poshdr.status, ;
      SUM(IIF(trancd = '1', qty1, -qty1)) AS wip1, ;
      SUM(IIF(trancd = '1', qty2, -qty2)) AS wip2, ;
      SUM(IIF(trancd = '1', qty3, -qty3)) AS wip3, ;
      SUM(IIF(trancd = '1', qty4, -qty4)) AS wip4, ;
      SUM(IIF(trancd = '1', qty5, -qty5)) AS wip5, ;
      SUM(IIF(trancd = '1', qty6, -qty6)) AS wip6, ;
      SUM(IIF(trancd = '1', qty7, -qty7)) AS wip7, ;
      SUM(IIF(trancd = '1', qty8, -qty8)) AS wip8 ;
      FROM posln, poshdr ;
      WHERE poshdr.po = posln.po ;
      AND poshdr.status = "O" ;
      AND posln.cstytype="D" ;
      GROUP BY STYLE;
      INTO TABLE (m.work_dir + "t_wip")

   SELECT t_wip
   INDEX ON STYLE TAG STYLE

   * --- Add in other grid selection criteria as needed ---
   WAIT WINDOW " Gathering Dyed Goods Inventory Data... " NOWAIT
 
   SELECT STYLE,;
      PADR(LEFT(STYLE,12), 19) AS cStyMajor,;
      SPACE(19) AS greige,;
      SUM(stk1 - alo1) AS dyeavail1,;
      SUM(stk2 - alo2) AS dyeavail2,;
      SUM(stk3 - alo3) AS dyeavail3,;
      SUM(stk4 - alo4) AS dyeavail4,;
      SUM(stk5 - alo5) AS dyeavail5,;
      SUM(stk6 - alo6) AS dyeavail6,;
      SUM(stk7 - alo7) AS dyeavail7,;
      SUM(stk8 - alo8) AS dyeavail8;
      FROM STYLE;
      WHERE IIF(!EMPTY(m.cstylist),ALLTRIM(style.cstymajor) $ m.cstylist, .T.);
      AND IIF(!EMPTY(m.cseaslist), style.season $ m.cseaslist, .T.);
      AND IIF(!EMPTY(m.cdivision), style.cdivision $ m.cdivision, .T.);
      NOCONSOLE;
      GROUP BY STYLE;
      ORDER BY STYLE;
      INTO TABLE (m.work_dir + "availinv")
 


   SELECT availinv
   SET RELATION TO STYLE INTO t_wip
   REPLACE ALL dyeavail1 WITH dyeavail1 + t_wip.wip1,;
      dyeavail2 WITH dyeavail2 + t_wip.wip2,;
      dyeavail3 WITH dyeavail3 + t_wip.wip3,;
      dyeavail4 WITH dyeavail4 + t_wip.wip4,;
      dyeavail5 WITH dyeavail5 + t_wip.wip5,;
      dyeavail6 WITH dyeavail6 + t_wip.wip6,;
      dyeavail7 WITH dyeavail7 + t_wip.wip7,;
      dyeavail8 WITH dyeavail8 + t_wip.wip8
   SET RELATION TO

   SELECT t_wip
   USE
   *SELECT posln
   *USE
   *SELECT poshdr
   *USE

   * --- Fill In Greige ---
   SELECT stybom
   SET ORDER TO cStyMajor

   SELECT availinv
   SET RELATION TO cStyMajor INTO stybom
   REPLACE ALL greige WITH stybom.greige

   SELECT availinv
   INDEX ON STYLE TAG STYLE

   * --- Now Get Order Values ---
   * --- Add in other grid selection criteria as needed ---
   *m.dbf = m.cAriaDbfs + "\ordhdr.dbf"
   *USE &dbf IN 0
   =gfOpenFile(gcDataDir+'ORDHDR','','SH')
   *m.dbf = m.cAriaDbfs + "\ordline.dbf"
   *USE &dbf IN 0
   =gfOpenFile(gcDataDir+'ORDLINE','','SH')


   WAIT WINDOW " Gathering Style Shortage Data By Order... " NOWAIT
   SELECT ordline.order,;
      ordline.style,;
      PADR(LEFT(ordline.style, 12), 19) AS cstymajor,;
      ordhdr.start,;
      ordhdr.complete,;
      ordhdr.priority,;
      ordhdr.entered,;
      INT(VAL('1')) AS RANK,;
      SUM(qty1 - pik1) AS os1,;
      SUM(qty2 - pik2) AS os2,;
      SUM(qty3 - pik3) AS os3,;
      SUM(qty4 - pik4) AS os4,;
      SUM(qty5 - pik5) AS os5,;
      SUM(qty6 - pik6) AS os6,;
      SUM(qty7 - pik7) AS os7,;
      SUM(qty8 - pik8) AS os8;
      FROM ordline, ordhdr;
      WHERE ordhdr.order = ordline.order;
      AND IIF(!EMPTY(m.dFStart), ordhdr.start >= m.dFStart, .T.);
      AND IIF(!EMPTY(m.dLStart), ordhdr.start <= m.dLStart, .T.);
      AND ordhdr.status $ m.cStatList ;
      NOCONSOLE;
      GROUP BY ordline.order, ordline.style;
      ORDER BY ordline.order, ordline.style;
      INTO TABLE (m.work_dir + "ordshort")

   * --- Rank Orders ---
   WAIT WINDOW " Ranking The Orders... " NOWAIT
   SELECT ordshort
   INDEX ON (m.cOrdSort) TAG KEY
   m.nOneThird = INT(RECCOUNT() / 3)
   i = 0
   SCAN
      i = i + 1
      DO CASE
         CASE i <= m.nOneThird
            m.Rank = 1

         CASE BETWEEN(i, m.nOneThird + 1, 2 * m.nOneThird)
            m.Rank = 2

         CASE i > 2 * m.nOneThird
            m.Rank = 3
      ENDCASE

      REPLACE RANK WITH m.Rank
   ENDSCAN

   * --- Total Rank Quantities At Style Level ---
   WAIT WINDOW " Gathering Style Shortage Data By Rank... " NOWAIT
   SELECT STYLE,;
      SUM(IIF(RANK = 1, os1, 0)) AS r1qty1,;
      SUM(IIF(RANK = 2, os1, 0)) AS r2qty1,;
      SUM(IIF(RANK = 3, os1, 0)) AS r3qty1,;
      SUM(IIF(RANK = 1, os2, 0)) AS r1qty2,;
      SUM(IIF(RANK = 2, os2, 0)) AS r2qty2,;
      SUM(IIF(RANK = 3, os2, 0)) AS r3qty2,;
      SUM(IIF(RANK = 1, os3, 0)) AS r1qty3,;
      SUM(IIF(RANK = 2, os3, 0)) AS r2qty3,;
      SUM(IIF(RANK = 3, os3, 0)) AS r3qty3,;
      SUM(IIF(RANK = 1, os4, 0)) AS r1qty4,;
      SUM(IIF(RANK = 2, os4, 0)) AS r2qty4,;
      SUM(IIF(RANK = 3, os4, 0)) AS r3qty4,;
      SUM(IIF(RANK = 1, os5, 0)) AS r1qty5,;
      SUM(IIF(RANK = 2, os5, 0)) AS r2qty5,;
      SUM(IIF(RANK = 3, os5, 0)) AS r3qty5,;
      SUM(IIF(RANK = 1, os6, 0)) AS r1qty6,;
      SUM(IIF(RANK = 2, os6, 0)) AS r2qty6,;
      SUM(IIF(RANK = 3, os6, 0)) AS r3qty6,;
      SUM(IIF(RANK = 1, os7, 0)) AS r1qty7,;
      SUM(IIF(RANK = 2, os7, 0)) AS r2qty7,;
      SUM(IIF(RANK = 3, os7, 0)) AS r3qty7,;
      SUM(IIF(RANK = 1, os8, 0)) AS r1qty8,;
      SUM(IIF(RANK = 2, os8, 0)) AS r2qty8,;
      SUM(IIF(RANK = 3, os8, 0)) AS r3qty8;
      FROM ordshort;
      NOCONSOLE;
      GROUP BY STYLE;
      INTO TABLE (m.work_dir + "StyShrt1")

   * --- Total By Style ---
   WAIT WINDOW " Grouping Style Shortage Data By Rank... " NOWAIT
   SELECT STYLE,;
      PADR(LEFT(STYLE,12),19) AS cStyMajor,;
      r1qty1 AS nsr1_1,;
      (r1qty1 + r2qty1) AS nsr1r2_1,;
      (r1qty1 + r2qty1 + r3qty1) AS nsr1r2r3_1,;
      r1qty2 AS nsr1_2,;
      (r1qty2 + r2qty2) AS nsr1r2_2,;
      (r1qty2 + r2qty2 + r3qty2) AS nsr1r2r3_2,;
      r1qty3 AS nsr1_3,;
      (r1qty3 + r2qty3) AS nsr1r2_3,;
      (r1qty3 + r2qty3 + r3qty3) AS nsr1r2r3_3,;
      r1qty4 AS nsr1_4,;
      (r1qty4 + r2qty4) AS nsr1r2_4,;
      (r1qty4 + r2qty4 + r3qty4) AS nsr1r2r3_4,;
      r1qty5 AS nsr1_5,;
      (r1qty5 + r2qty5) AS nsr1r2_5,;
      (r1qty5 + r2qty5 + r3qty5) AS nsr1r2r3_5,;
      r1qty6 AS nsr1_6,;
      (r1qty6 + r2qty6) AS nsr1r2_6,;
      (r1qty6 + r2qty6 + r3qty6) AS nsr1r2r3_6,;
      r1qty7 AS nsr1_7,;
      (r1qty7 + r2qty7) AS nsr1r2_7,;
      (r1qty7 + r2qty7 + r3qty7) AS nsr1r2r3_7,;
      r1qty8 AS nsr1_8,;
      (r1qty8 + r2qty8) AS nsr1r2_8,;
      (r1qty8 + r2qty8 + r3qty8) AS nsr1r2r3_8;
      FROM StyShrt1;
      NOCONSOLE;
      GROUP BY STYLE;
      INTO TABLE (m.work_dir + "StyShrt2")

   * --- Subtract Color Specific Dye Available ---
   SELECT availinv
   SET ORDER TO STYLE

   WAIT WINDOW " Subtracting Dye Available Quantities From Style Shortages... " NOWAIT
   SELECT styshrt2
   SET RELATION TO STYLE INTO availinv
   REPLACE ALL nsr1_1 WITH MAX(0, nsr1_1 - availinv.dyeavail1),;
      nsr1r2_1 WITH MAX(0, nsr1r2_1 - availinv.dyeavail1),;
      nsr1r2r3_1 WITH MAX(0, nsr1r2r3_1 - availinv.dyeavail1),;
      nsr1_2 WITH MAX(0, nsr1_2 - availinv.dyeavail2),;
      nsr1r2_2 WITH MAX(0, nsr1r2_2 - availinv.dyeavail2),;
      nsr1r2r3_2 WITH MAX(0, nsr1r2r3_2 - availinv.dyeavail2),;
      nsr1_3 WITH MAX(0, nsr1_3 - availinv.dyeavail3),;
      nsr1r2_3 WITH MAX(0, nsr1r2_3 - availinv.dyeavail3),;
      nsr1r2r3_3 WITH MAX(0, nsr1r2r3_3 - availinv.dyeavail3),;
      nsr1_4 WITH MAX(0, nsr1_4 - availinv.dyeavail4),;
      nsr1r2_4 WITH MAX(0, nsr1r2_4 - availinv.dyeavail4),;
      nsr1r2r3_4 WITH MAX(0, nsr1r2r3_4 - availinv.dyeavail4),;
      nsr1_5 WITH MAX(0, nsr1_5 - availinv.dyeavail5),;
      nsr1r2_5 WITH MAX(0, nsr1r2_5 - availinv.dyeavail5),;
      nsr1r2r3_5 WITH MAX(0, nsr1r2r3_5 - availinv.dyeavail5),;
      nsr1_6 WITH MAX(0, nsr1_6 - availinv.dyeavail6),;
      nsr1r2_6 WITH MAX(0, nsr1r2_6 - availinv.dyeavail6),;
      nsr1r2r3_6 WITH MAX(0, nsr1r2r3_6 - availinv.dyeavail6),;
      nsr1_7 WITH MAX(0, nsr1_7 - availinv.dyeavail7),;
      nsr1r2_7 WITH MAX(0, nsr1r2_7 - availinv.dyeavail7),;
      nsr1r2r3_7 WITH MAX(0, nsr1r2r3_7 - availinv.dyeavail7),;
      nsr1_8 WITH MAX(0, nsr1_8 - availinv.dyeavail8),;
      nsr1r2_8 WITH MAX(0, nsr1r2_8 - availinv.dyeavail8),;
      nsr1r2r3_8 WITH MAX(0, nsr1r2r3_8 - availinv.dyeavail8)

   * --- Sum By cStyMajor ---
   WAIT WINDOW " Re-totalling Shortages Data By cStyMajor & Rank... " NOWAIT
   SELECT cStyMajor,;
      SPACE(19) AS greige,;
      SUM(nsr1_1) AS nsr1_1,;
      SUM(nsr1r2_1) AS nsr1r2_1,;
      SUM(nsr1r2r3_1) AS nsr1r2r3_1,;
      SUM(nsr1_2) AS nsr1_2,;
      SUM(nsr1r2_2) AS nsr1r2_2,;
      SUM(nsr1r2r3_2) AS nsr1r2r3_2,;
      SUM(nsr1_3) AS nsr1_3,;
      SUM(nsr1r2_3) AS nsr1r2_3,;
      SUM(nsr1r2r3_3) AS nsr1r2r3_3,;
      SUM(nsr1_4) AS nsr1_4,;
      SUM(nsr1r2_4) AS nsr1r2_4,;
      SUM(nsr1r2r3_4) AS nsr1r2r3_4,;
      SUM(nsr1_5) AS nsr1_5,;
      SUM(nsr1r2_5) AS nsr1r2_5,;
      SUM(nsr1r2r3_5) AS nsr1r2r3_5,;
      SUM(nsr1_6) AS nsr1_6,;
      SUM(nsr1r2_6) AS nsr1r2_6,;
      SUM(nsr1r2r3_6) AS nsr1r2r3_6,;
      SUM(nsr1_7) AS nsr1_7,;
      SUM(nsr1r2_7) AS nsr1r2_7,;
      SUM(nsr1r2r3_7) AS nsr1r2r3_7,;
      SUM(nsr1_8) AS nsr1_8,;
      SUM(nsr1r2_8) AS nsr1r2_8,;
      SUM(nsr1r2r3_8) AS nsr1r2r3_8;
      FROM styshrt2;
      NOCONSOLE;
      GROUP BY cStyMajor;
      INTO TABLE (m.work_dir + "StyShrt3")


   SELECT stybom
   SET ORDER TO cStyMajor

   SELECT styshrt3
   SET RELATION TO cStyMajor INTO stybom
   REPLACE ALL greige WITH stybom.greige

   * --- Sum Order Data By Greige ---
   WAIT WINDOW " Totalling Again By Greige & Rank... " NOWAIT
   SELECT greige,;
      SUM(nsr1_1) AS nsr1_1,;
      SUM(nsr1r2_1) AS nsr1r2_1,;
      SUM(nsr1r2r3_1) AS nsr1r2r3_1,;
      SUM(nsr1_2) AS nsr1_2,;
      SUM(nsr1r2_2) AS nsr1r2_2,;
      SUM(nsr1r2r3_2) AS nsr1r2r3_2,;
      SUM(nsr1_3) AS nsr1_3,;
      SUM(nsr1r2_3) AS nsr1r2_3,;
      SUM(nsr1r2r3_3) AS nsr1r2r3_3,;
      SUM(nsr1_4) AS nsr1_4,;
      SUM(nsr1r2_4) AS nsr1r2_4,;
      SUM(nsr1r2r3_4) AS nsr1r2r3_4,;
      SUM(nsr1_5) AS nsr1_5,;
      SUM(nsr1r2_5) AS nsr1r2_5,;
      SUM(nsr1r2r3_5) AS nsr1r2r3_5,;
      SUM(nsr1_6) AS nsr1_6,;
      SUM(nsr1r2_6) AS nsr1r2_6,;
      SUM(nsr1r2r3_6) AS nsr1r2r3_6,;
      SUM(nsr1_7) AS nsr1_7,;
      SUM(nsr1r2_7) AS nsr1r2_7,;
      SUM(nsr1r2r3_7) AS nsr1r2r3_7,;
      SUM(nsr1_8) AS nsr1_8,;
      SUM(nsr1r2_8) AS nsr1r2_8,;
      SUM(nsr1r2r3_8) AS nsr1r2r3_8;
      FROM styshrt3;
      NOCONSOLE;
      GROUP BY greige;
      INTO TABLE (m.work_dir + "grgshort")

   * --- Pull Numbers From Inventory & Orders Together ---
   WAIT WINDOW " Creating Report Table... " NOWAIT
   m.rpt_dbf = m.work_dir + "rpt_dbf.dbf"
   CREATE TABLE &rpt_dbf (greige C(12),;
      SIZE C(3),;
      grgstk N(5,0),;
      nsr1   N(5,0),;
      remr1  N(5,0),;
      nsr1r2 N(5,0),;
      remr1r2 N(5,0),;
      nsr1r2r3 N(5,0),;
      remr1r2r3 N(5,0),;
      grgwip N(5,0);
      )

   SELECT grgshort
   INDEX ON  greige TAG greige

   *m.dbf = m.cAriaDbfs + "\scale.dbf"
   *USE &dbf IN 0 ORDER scale
   =gfOpenFile(gcDataDir+'SCALE','SCALE','SH')

   SELECT STYLE
   SET ORDER TO STYLE
   SET RELATION TO "S" + scale INTO scale

   SELECT availgrg
   SET ORDER TO greige
   SET RELATION TO greige INTO grgshort ADDITIVE
   SET RELATION TO STYLE INTO STYLE ADDITIVE
   SET FILTER TO !EMPTY(grgshort.greige)
   SCAN
      m.greige = availgrg.greige
      m.cnt = scale.cnt

      FOR i = 1 TO m.cnt
         m.cSz = STR(i,1)

         m.fld = "scale.sz" + m.cSz
         m.size = EVAL(m.fld)

         m.fld = "availgrg.stk" + m.cSz
         m.grgstk = EVAL(m.fld)

         m.fld = "grgshort.nsr1_" + m.cSz
         m.nsr1 = EVAL(m.fld)
         m.remr1 = m.grgstk - m.nsr1

         m.fld = "grgshort.nsr1r2_" + m.cSz
         m.nsr1r2 = EVAL(m.fld)
         m.remr1r2 = m.grgstk - m.nsr1r2

         m.fld = "grgshort.nsr1r2r3_" + m.cSz
         m.nsr1r2r3 = EVAL(m.fld)
         m.remr1r2r3 = m.grgstk - m.nsr1r2r3

         m.fld = "availgrg.wip" + m.cSz
         m.grgwip = EVAL(fld)

         SELECT rpt_dbf
         APPEND BLANK
         GATHER MEMVAR
      ENDFOR

      SELECT availgrg
   ENDSCAN

   * --- Print Out Report ---
   SELECT rpt_dbf

   *REPORT FORM (m.rpt_dir + "mfgrgrpt") NOCONSOLE TO PRINT	&& Output to appropriate Form here
   *- display the frx file
   lcOgPlatForm = 'WINDOW'
   DO CASE 
     CASE lcFrmTyp = 'S'			&& by style
       DO gfDispRe WITH 'mfgrgrpt'
     CASE lcFrmTyp = 'C'			&& by Contractor
       *???? PUT THE NAME OF BY CONTRACTOR FRX NAME.
       DO gfDispRe WITH 'mfgrgrpt'
  OTHERWISE					    &&  both(style,contractor)
    DO gfDispRe WITH 'mfgrgrpt'
    *???? PUT THE NAME OF BY CONTRACTOR FRX NAME.
    DO gfDispRe WITH 'mfgrgrpt'
ENDCASE  


*ENDIF	&& IF m.ok_2_go

RETURN

*--- Not needed part...
* ------------------------------------
CLOSE DATABASES

m.cHomeDrive = "N:"
m.cHomeDir = m.cHomeDrive + "\ARIA27\ROE"
m.cHoldDir = m.cHomeDir + "\RECEIVED"
m.cTempDir = SYS(2023)
SET DEFAULT TO &cHomeDir

*m.cAriaDbfs = m.cHomeDrive + SYS(2003)
m.cAriaDbfs = m.cHomeDrive + "\ARIA27\ROE"
m.loc = AT("ROE", m.cAriaDbfs)
m.cAriaDbfs = LEFT(m.cAriaDbfs, m.loc -1 )
m.cAriaSys = m.cAriaDbfs + "sysfiles"
m.cAriaDbfs = m.cAriaDbfs + "dbfs\01"
m.cAriaDir = m.cAriaDbfs

m.cPath = m.cHomeDir + ";" + m.cTempDir + ";" + m.cAriaDbfs + ";" + m.cHoldDir
SET PATH TO &cPath

* --- Erase Temporary Tables ---
CLOSE DATABASES
=DelTmpDbf('stybom')
=DelTmpDbf('availgrg')
=DelTmpDbf('t_wip')
=DelTmpDbf('availinv')
=DelTmpDbf('ordshort')
=DelTmpDbf('styshrt1')
=DelTmpDbf('styshrt2')
=DelTmpDbf('styshrt3')
=DelTmpDbf('grgshort')
=DelTmpDbf('rpt_dbf')

CLOSE DATABASES
CLEAR
RETURN .T.

* ---------------------------------
*  "Clean House"
*
FUNCTION DelTmpDbf
PARAMETER m.cAlias
PRIVATE m.dbf, m.cdx, m.fpt

IF USED(m.cAlias)
   SELECT &cAlias
   USE
ENDIF

m.dbf = m.work_dir + m.cAlias + ".dbf"
ERASE &dbf

m.cdx = m.work_dir + m.cAlias + ".cdx"
ERASE &cdx

m.fpt = m.work_dir + m.cAlias + ".fpt"
ERASE &fpt

RETURN .T.


* ---------------------------
*  If Necessary, Adjust For Century
*
*  Example Use:  m.dEntered = chk_century(m.dEntered)
*

FUNCTION chk_century
PARAMETER m.dChkDate
PRIVATE m.mo, m.day, m.yr, m.left, m.right
PRIVATE m.cChkDate

m.cChkDate = DTOC(m.dChkDate)

* --- Handle If Only 2 Digits Were Entered For Year ---
IF SUBSTR(m.cChkDate,7,2) $ "00,99" ;
      OR INLIST(YEAR(m.dChkDate),1919,1920) ;
      OR BETWEEN(SUBSTR(m.cChkDate,7,2),"01","09")

   m.left = LEFT(m.cChkDate,6)
   m.yr = YEAR(m.dChkDate)
   m.yr = PADL(ALLTRIM(STR(m.yr)), 4, "0")

   DO CASE
      CASE LEFT(m.yr,2) $ "19,00"
         m.yr = "2000"

      CASE LEFT(m.yr,2) = "99"
         m.yr = "1999"

      CASE BETWEEN(SUBSTR(m.cChkDate,7,2),"01","09")
         m.yr = SUBSTR(m.cChkDate,7,2)
         m.yr = "20" + m.yr
   ENDCASE

   m.cChkDate = m.left + m.yr
   m.dChkDate = CTOD(m.cChkDate)
ENDIF

* --- Check Full 4 Digit Date ---
IF YEAR(m.dChkDate) < 1990
   m.yr = YEAR(m.dChkDate)
   m.yr = m.yr + 100
   m.yr = STR(m.yr, 4)
   m.left = LEFT(DTOC(m.dChkDate), 6)
   m.dChkDate = CTOD(m.left + m.yr)
ENDIF
RETURN m.dChkDate










*!**************************************************************************
*! Name      : lfSeTOrdr
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/13/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid && Report Greige goods shortage report.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =fSeTOrdr()
*!**************************************************************************
*! Note      : symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSeTOrdr 
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
    SELECT ORDHDR
    lcRelation = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcRelation INTO CUSTOMER && To customer file.
    GO TOP
  CASE OpGrdParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
ENDCASE
*!**************************************************************************
*! Name      : lfSetFab 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/13/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid  && Report Greige goods shortage report.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetFab()
*!**************************************************************************
*! Note      :  symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSetFab  
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'  && Set code
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE OpGrdParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
ENDCASE
*!**************************************************************************
*! Name      : lfFabSum 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/13/1999
*! Purpose   : sum a specific field for the current fabric in fabric file
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : Calculated field value.
*!**************************************************************************
*! Example   : =lfFabSum()
*!**************************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')
  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
RETURN INT(lnTotcomp)
*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : WAb - WALID A. WAHAB
*! Date      : 07/13/1999
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid  && report Greige goods shortage report.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty  
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE   
   GO TOP
  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE
*!**************************************************************************
*! Name      : lfGrgWhen 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/13/1999
*! Purpose   : set Last Order Start Ship Date = today()+dye 
*!             visibility window
*!**************************************************************************
*! Calls     : gfGetmemVar()
*!**************************************************************************
*! Called from :Option Grid && Report Greige goods shortage report.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfGrgWhen()
*!**************************************************************************
FUNCTION lfGrgWhen 
PRIVATE lnarrPos,lnCount,lcStr
llSetRel = .F.
lcFrmTyp = 'B'
*-Store default date to start date in option gride
lnarrpos = ASCAN(laOgFxFlt,'ORDHDR.START') 
IF lnarrpos > 0
  lnarrpos = ASUBSCRIPT(laOGFxFlt,lnarrpos,1)
  *-get today and today + dye visibility  from company master file
  lcStr = ALLTRIM(DTOC(gdSysDate)) + '|' +;
          ALLTRIM(DTOC(gdSysDate+gfGetmemVar('M_DOVW','gcAct_Comp')))     
  IF EMPTY(laOGFxFlt[lnarrpos,6])
    laOGFxFlt[lnarrpos,6] = lcStr
  ENDIF
ENDIF

*-Store default Status (Open / Hold) in option gride
lnarrpos = ASCAN(laOgFxFlt,'ORDHDR.STATUS') 
IF lnarrpos > 0
  lnarrpos = ASUBSCRIPT(laOGFxFlt,lnarrpos,1)
  IF EMPTY(laOGFxFlt[lnarrpos,6])
    laOGFxFlt[lnarrpos,6] = 'O|H'
  ENDIF
ENDIF

*-- Form the structure of the temporary Table of the report
DIMENSION  laFlds[73,4],laNewFlds[23,4],laCutTkt[23]
STORE 0 TO laFlds,laNewFlds,laCutTkt

laFlds[1,1] = 'STYLE'
laFlds[1,2] = 'C'
laFlds[1,3] = 19
FOR lnCount = 1 TO 8
  laFlds[1+lnCount,1] = 'nTotGrg'+ALLTRIM(STR(lnCount))
  laFlds[1+lnCount,2] = 'N'
  laFlds[1+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
  laFlds[9+lnCount,1] = 'nNsRFrst'+ALLTRIM(STR(lnCount))
  laFlds[9+lnCount,2] = 'N'
  laFlds[9+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
  laFlds[17+lnCount,1] = 'nRGrgFrst'+ALLTRIM(STR(lnCount))
  laFlds[17+lnCount,2] = 'N'
  laFlds[17+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
  laFlds[25+lnCount,1] = 'nNsRScnd'+ALLTRIM(STR(lnCount))
  laFlds[25+lnCount,2] = 'N'
  laFlds[25+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
  laFlds[33+lnCount,1] = 'nRGrgScnd'+ALLTRIM(STR(lnCount))
  laFlds[33+lnCount,2] = 'N'
  laFlds[33+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
  laFlds[41+lnCount,1] = 'nNsRThrd'+ALLTRIM(STR(lnCount))
  laFlds[41+lnCount,2] = 'N'
  laFlds[41+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
  laFlds[49+lnCount,1] = 'nRGrgThrd'+ALLTRIM(STR(lnCount))
  laFlds[49+lnCount,2] = 'N'
  laFlds[49+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
  laFlds[57+lnCount,1] = 'nWip'+ALLTRIM(STR(lnCount))
  laFlds[57+lnCount,2] = 'N'
  laFlds[57+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
  laFlds[65+lnCount,1] = 'nTotal'+ALLTRIM(STR(lnCount))
  laFlds[65+lnCount,2] = 'N'
  laFlds[65+lnCount,3] = 10
ENDFOR

laNewFlds[1,1] = 'STYLE'
laNewFlds[1,2] = 'C'
laNewFlds[1,3] = 19

laNewFlds[2,1] = 'cContCode'
laNewFlds[2,2] = 'C'
laNewFlds[2,3] = 6

laNewFlds[3,1] = 'CUTNOST'
laNewFlds[3,2] = 'C'
laNewFlds[3,3] = 6

FOR lnCount = 1 TO 8
  laNewFlds[3+lnCount,1] = 'QtySt'+ALLTRIM(STR(lnCount))
  laNewFlds[3+lnCount,2] = 'N'
  laNewFlds[3+lnCount,3] = 8
ENDFOR

laNewFlds[12,1] = 'CutNoEnd'
laNewFlds[12,2] = 'C'
laNewFlds[12,3] = 6

FOR lnCount = 1 TO 8
  laNewFlds[12+lnCount,1] = 'QtyEnd'+ALLTRIM(STR(lnCount))
  laNewFlds[12+lnCount,2] = 'N'
  laNewFlds[12+lnCount,3] = 8
ENDFOR

laNewFlds[21,1] = 'DateSt'
laNewFlds[21,2] = 'D'
laNewFlds[21,3] = 8

laNewFlds[22,1] = 'nTotal1'
laNewFlds[22,2] = 'N'
laNewFlds[22,3] = 8

laNewFlds[23,1] = 'nTotal2'
laNewFlds[23,2] = 'N'
laNewFlds[23,3] = 8

IF laSortFlag		&& resored  default sorts levels
  FOR lnI = 1 To 4
    lcI = STR(lnI,1)
    IF ASCAN(laOgObjType,'lnRpSort'+lcI) # 0
      lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'lnRpSort'+lcI),1)
      laOGObjCnt[lnPos] = .F.
      = lfOGShowGet('lnRpSort'+lcI)
    ENDIF
  ENDFOR
ENDIF

*!**************************************************************************
*! Name      : lfGetPos  
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/14/1999
*! Purpose   : Get Starting Position & lenth Of Color
*!**************************************************************************
*! Calls     : ()
*!**************************************************************************
*! Called from : Option Grid && Report Greige goods shortage report.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfGetPos()
*!**************************************************************************
FUNCTION lfGetPos
PRIVATE lnClrPos
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major. 
 DIMENSION laMajSeg[1,1]
 = gfItemMask(@laMajSeg)
 lnClrPos = int(ASCAN(laMajSeg,'C')/7+0.9)
 lnStrtPos = laMajSeg[lnClrPos,4] 
 lnFldLngth= LEN(laMajSeg[lnClrPos,3])
*!**************************************************************************
*! Name      : lfSetRel
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/15/1999
*! Purpose   : Set Relation Betwwen Tables
*!**************************************************************************
*! Calls     :
*!**************************************************************************
*! Called from : icGrgSh.prg
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetRel()
*!**************************************************************************
FUNCTION lfSetRel

  SELECT Mfgoprdt
  SET ORDER TO TAG Mfgoprdt 

  SELECT CUTTKTH
  SET ORDER TO CUTTKTH

  SELECT CUTTKTL
  SET ORDER TO CUTTKTLS
  SET RELATION TO Cuttktl.cuttkt INTO Cuttkth ADDITIVE
*  SET RELATION TO 'M'+ Cuttktl.cuttkt INTO Mfgoprdt ADDITIVE  
  
  SELECT FABRIC
  SET ORDER TO TAG cFabric 

  SELECT POSHDR
  SET ORDER TO TAG POSHDR
  
  SELECT POSLN
  SET ORDER TO TAG Poslns
  SET RELATION TO 'D'+PO INTO POSHDR

  SELECT STYLE
  SET ORDER TO TAG Style
  SET RELATION TO STYLE.FABRIC INTO FABRIC 	ADDITIVE
  SET RELATION TO style+'D' INTO Posln ADDITIVE
  SET RELATION TO Style.style INTO Cuttktl ADDITIVE
    
  SELECT CUSTOMER
  SET ORDER TO TAG CUSTOMER

  SELECT ORDHDR
  SET ORDER TO TAG OrdHdr

  SELECT ORDLINE
  SET ORDER TO TAG ORDLINE
  SET RELATION TO STYLE 		INTO STYLE 		ADDITIVE
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER 	ADDITIVE
  SET RELATION TO 'O'+ORDER 	INTO ORDHDR 	ADDITIVE
  GO TOP
  
*!**************************************************************************
*! Name      : lfUpdTemp
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/21/1999
*! Purpose   : update temp file for contractor AND The cut tickets sequence 
*!             ordered with the earliest completion date
*!**************************************************************************
*! Calls     :
*!**************************************************************************
*! Called from : icGrgSh.prg
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfUpdTemp()
*!**************************************************************************
FUNCTION lfUpdTemp
PRIVATE lcAlias
IF !SEEK(ALLTRIM(laCuttkt[1]+laCuttkt[2]),lcContrc) 	&& NEW RECORD
  laCutTkt[3] = lcOldCtkt
  FOR lnI = 1 TO 8
    laCutTkt[lnI+3] = laOpenCt[lnI]
  ENDFOR
  laCutTkt[22] = lnTotal 
  INSERT INTO (lcContrc) FROM ARRAY laCutTkt
ELSE
  lcAlias = SELECT()
  SELECT &lcContrc
  IF EMPTY(CutNoEnd)		&& the cuttckt no 2 is empty
    IF laCutTkt[21] > DateSt	
      *- The cut tickets sequence ordered with the earliest completion date
      REPLACE CutNoEnd  WITH  CutNoSt
      REPLACE CutNoSt   WITH  lcOldCtkt
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        REPLACE QtyEnd&lcI WITH QtySt&lcI
        REPLACE QtySt&lnI  WITH laOpenCt[lnI]        
      ENDFOR
      REPLACE nTotal2 WITH  nTotal1
      REPLACE nTotal1 WITH  lnTotal
    ELSE
      *- save  cut ticket data as sequence # 2
      REPLACE CutNoEnd  WITH lcOldCtkt
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        REPLACE QtyEnd&lcI  WITH laOpenCt[lnI]        
      ENDFOR
      REPLACE nTotal2 WITH  lnTotal      
    ENDIF    
  ENDIF
  SELECT (lcAlias)
ENDIF
*!**************************************************************************
*! Name      : lfDeclAry
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/27/1999
*! Purpose   : intialise the variables from option grid
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : from option grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfDeclAry()
*!**************************************************************************
FUNCTION lfDeclAry

DIMENSION laSortAry[4,2]
laSortAry[1,1] = 1
laSortAry[1,2] = [DTOS(COMPLETE)]
laSortAry[2,1] = 2
laSortAry[2,2] = [ORDHDR.PRIORITY]
laSortAry[3,1] = 3
laSortAry[3,2] = [DTOS(START)]
laSortAry[4,1] = 4
laSortAry[4,2] = [ORDER+STORE+GROUP]

DECLARE laIndexExp[6] 
laIndexExp[1] = 'DTOS(COMPLETE)'
laIndexExp[2] = 'ORDHDR.PRIORITY'
laIndexExp[3] = 'DTOS(START)'
laIndexExp[4] = 'ORDER+STORE+GROUP'
laIndexExp[5] = 'ACCOUNT'
laIndexExp[6] = ''

*- restore arrys from mem file (default sorts)
IF FILE(gcDataDir+'SORTLEVL.MEM')
  laSortFlag = .T.
  RESTORE FROM (gcDataDir+'SORTLEVL.MEM') additive
  FOR lnI = 1 To 4
    IF laSortAry[lnI,2] = [PRIORITY]
      laSortAry[lnI,2] = [ORDHDR.PRIORITY]
    ENDIF
    IF laSortAry[lnI,2] = [ORDER]
      laSortAry[lnI,2] = [ORDER+STORE+GROUP]
    ENDIF
  ENDFOR
ENDIF
*!**************************************************************************
*! Name      : lfvSortBy
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/27/1999
*! Purpose   : All Sort by validations
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : from option grid
*!**************************************************************************
*! Passed Parameters  : Sort Number (1,2,3, Or 4)
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfvSortBy(x)
*!**************************************************************************
FUNCTION lfvSortBy
PARAMETERS lnSortItem
PRIVATE lcObjName , lnObjVal , lcObjGet , llOldValue , lnI , lcItmObj
llOldValue = .F.
lcObjGet  = SYS(18)
lcObjName = "lnRpSort" + STR(lnSortItem,1)
lnObjVal  = EVALUATE(lcObjName)
lnI = 0
IF lnObjVal = 6
  FOR lnI = lnSortItem + 1 TO 4
    lcItmObj = "lnRpSort" + STR(lnI,1)
    IF EVALUATE(lcItmObj) <> 6
      llOldValue = .T.
      EXIT 
    ENDIF
  ENDFOR
ELSE
  IF lnSortItem > 2
    FOR lnI = lnSortItem-1 TO 2 STEP -1
      lcItmObj = "lnRpSort" + STR(lnI,1)
      IF EVALUATE(lcItmObj) = 6
        llOldValue = .T.
        EXIT 
      ENDIF
    ENDFOR
  ENDIF
ENDIF
llOldValue = IIF(llOldValue,llOldValue,;
                (lnObjVal<> 6) AND (ASCAN(laSortAry,lnObjVal) > 0))
IF llOldValue
  *-- Restore old values.
  STORE laOldVal TO &lcObjName , &lcObjGet 
  SHOW GET &lcObjGet
ELSE
  *-- Sort By Arrays make Sort Index.
  laSortAry[lnSortItem,1] = lnObjVal
  laSortAry[lnSortItem,2] = laIndexExp[lnObjVal]
ENDIF
*!**************************************************************************
*! Name      : lfwOldVal
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/27/1999
*! Purpose   : When function to get the Old value
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : from option grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfwOldVal()
*!**************************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*!**************************************************************************
*! Name      : lfDataDef
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/27/1999
*! Purpose   : intialise value to arry holding sort levels
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : from option grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfwOldVal()
*!**************************************************************************
FUNCTION lfDataDef
parameter lnFldSort
PRIVATE lnVarDef

IF laSortFlag		&& if restored  default sort levels
  do case 
    case lnFldSort = 1
      lnVarDef = laSortAry[1,1] 
    case lnFldSort = 2
      lnVarDef = laSortAry[2,1]
    case lnFldSort = 3
      lnVarDef = laSortAry[3,1]
    case lnFldSort = 4
      lnVarDef = laSortAry[4,1]
  ENDCASE
ELSE			&& if NOT restored  default sort levels
  do case 
    case lnFldSort = 1
      lnVarDef = 1
    case lnFldSort = 2
      lnVarDef = 2
    case lnFldSort = 3
      lnVarDef = 3
    case lnFldSort = 4
      lnVarDef = 4
  ENDCASE
ENDIF
RETURN(lnVarDef)







*********
*COMMENTS
*********
* Check the following in the code, search on ????? in the front of the code.
* 3- Add the name of FRX for Contractor , since you didn't sent it to me.
* 4- Some optinal grid element you didn't use , why?
* Like ->Primary Fabric, Grige Colors, Style groups, Style pattern.
*You can use it as :
*1- Primary Fabric -> same concept as styles selection ,
*     STYLE.Fabric exist in temp file laOgFxFlt[2,6].
*2- Grige Colors   -> SUBSTR(Style,lnStrtPos,lnFldLngth) $ laOgFxFlt[6,6]
*3- Style groups   -> STYLE.cStyGroup $ laOgVrFlt[3,6]
*4- Pattern        -> STYLE.Pattern = ALLT(laOgVrFlt[4,6])
* Or tell me which tabels you want to filter on , and I will do it for you.
*******