*:************************************************************************
*: Program file  : MFDYDMD.Prg    Cust. Prog. # 200079
*: Program desc. : Dye demand report
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MF
*: Developer     : WAB- WALID A. WAHAB
*: Date          : 06/10/1999
*:************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : lfGetPos  lfDydWhen lfSetSTY lfFabSum lfSetFab lfSeTOrdr
*:                 lfSeTOrdr
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO MFDYDMD
*:************************************************************************
*: Modifications:
*:B803073,1 KHM 02/28/2000 Adding a new option (warehouse) to the option
*:B803073,1                grid and modify the code according to the new
*:B803073,1                option
*B803073,1 KHM 02/28/2000 (Begin) Checking if no warehouse was selected
*B803073,1                then do not run the report. Initializing a new
*B803073,1                array laWareHDsc to hold the description of
*B803073,1                the selected warehouses.
*:          JRB 03/03/2000 Review & modify
*:************************************************************************


IF EMPTY(laRpTarget)
   =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'You should select a warehouse.')
   RETURN
ENDIF

DIMENSION laWareHDsc[ALEN(laRpTarget,1)]
STORE '' TO laWareHDsc,lcAllWHs

FOR lnInd = 1 TO ALEN(laRpTarget,1)
   =SEEK(PADR(laRpTarget[lnInd],6),'WareHous')
   laWareHDsc[lnInd] = WareHous.cDesc
   lcAllWHs = lcAllWHs + PADR(laRpTarget[lnInd],6) + ' | '
ENDFOR

lcAllWHs = IIF(ALLTRIM(lcAllWHs) = '|','',lcAllWHs)
*B803073,1 KHM 02/28/2000 (End)

*-- Gets the value of the dates from the report criteria
IF ASCAN(laOgFxFlt,'ORDHDR.START') = 0
   =gfModalGen('TRM00250B00000','DIALOG', 'Last Order StartShip Date')
   RETURN
ELSE
   lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'ORDHDR.START'),1)
   lDSDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
   lDEDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
   IF EMPTY(lDSDate)
      =gfModalGen('TRM00250B00000','DIALOG','Last Order StartShip Date')
      RETURN
   ENDIF
ENDIF

*-CHECK THE ORDER STATUS ( MUST BO NOT EMPTY )
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

*- CALL fUNCTION TO SET THE RELATIONS
=lfSetRel()
lnStrtPos = 0				&& start color postion
lnFldLngth= 0				&& color length

*- Call function to get the color position
=lfGetPos()

*------ Collecting data  [BEGIN] --------
WAIT WINDOW " Preparing data... " NOWAIT

*B803073,1 KHM 02/28/2000 (Begin) Adding the following variable to hold the
*B803073,1                conditions that will be added to filter the OrdLine.
lcScndExp = "!DELETED('OrdLine') ;
   AND TotQty > 0 ;
   AND OrdHdr.cWareCode $ lcAllWHs"
*B803073,1 KHM 02/28/2000 (End)

SELECT ORDLINE
*B803073,1 KHM 02/28/2000 (Begin) Adding an additional filter to the OrdLine.
*LOCATE FOR &lcRpExp
LOCATE FOR &lcRpExp AND &lcScndExp
*B803073,1 KHM 02/28/2000 (End)

IF EOF()
   = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display'
   RETURN
ENDIF


lcOrdTmp   = gfTEMPNAME()  && TO HOLD RECORDS From Ordline B4 INDEX
lcOrdTmp1  = gfTEMPNAME()  && TO HOLD RECORDS From Ordline after INDEX
lcOrdCrs   = gfTEMPNAME()  && TO HOLD RECORDS For Layout

CREATE DBF (gcWorkDir+lcOrdCrs) FROM ARRAY laFlds
=gfOpenFile(gcWorkDir+lcOrdCrs,'','SH')
INDEX ON STYLE TAG &lcOrdCrs

* ----  Get OS Qty Data from Orders ---
SELECT ORDLINE
*B803073,1 KHM 02/28/2000 (Begin) Adding a new filter lcScndExp
*COPY TO (gcWorkDir+lcOrdTmp) FOR &lcRpExp
COPY TO (gcWorkDir+lcOrdTmp) FOR &lcRpExp AND &lcScndExp
*B803073,1 KHM 02/28/2000 (End)

=gfOpenFile(gcWorkDir+lcOrdTmp,'','SH')

*-Sort the temp file
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
lcKeyNam = ''
llOrdChk = .F.
*- create the index expresion
FOR lnI = 1 TO 4
   IF !EMPTY(laSortAry[lnI,2])
      lckeyNam = ALLTRIM(lcKeyNam + laSortAry[lnI,2]+'+')
   ENDIF
   IF laSortAry[lnI,2] = [ORDER+STORE+GROUP]
      llOrdChk = .T.
   ENDIF
ENDFOR

IF !llOrdChk
   lckeyNam = ALLTRIM(lckeyNam + 'ORDER+STORE+GROUP')
ELSE
   lckeyNam = SUBSTR(lcKeyNam,1,LEN(lcKeyNam)-1)
ENDIF

INDEX ON  &lcKeyNam TAG &lcOrdTmp

*-copy to file after sorting
COPY TO (gcWorkDir+lcOrdTmp1)

=gfOpenFile(gcWorkDir+lcOrdTmp1,'','SH')

* --- JRB 03/03/2000 Mod. - For Better Accuracy Use Ordline Alo Qty (Begin)
lcAloQty   = gfTEMPNAME()    && TO Hold Alo Qty
llChkdQty  = IIF(FILE(gcDataDir + "do_rebal.txt"),.T.,.F.)

IF llChkdQty
   WAIT WINDOW " Re-Calculating Sales Order Allocated Quantities... " NOWAIT

   SELECT ordline.style,;
      ordline.cwarecode,;
      SUM(pik1) AS alo1,;
      SUM(pik2) AS alo2,;
      SUM(pik3) AS alo3,;
      SUM(pik4) AS alo4,;
      SUM(pik5) AS alo5,;
      SUM(pik6) AS alo6,;
      SUM(pik7) AS alo7,;
      SUM(pik8) AS alo8;
      FROM ordline, ordhdr;
      WHERE ordhdr.order = ordline.order;
      AND ordhdr.status $ "O,H";
      AND !DELETED('ORDLINE');
      AND ordline.totqty > 0;
      AND ordline.cWareCode $ lcAllWHs;
      NOCONSOLE;
      GROUP BY 1, 2;
      ORDER BY 1, 2;
      INTO TABLE (gcWorkDir+lcAloQty)

   SELECT &lcAloQty
   INDEX ON STYLE + cwarecode TAG AloQty

   WAIT CLEAR
ENDIF
* --- JRB 03/03/2000 Mod. (End)

SELECT &lcOrdTmp1

*- divide  the records into 3 rank
lnRecNo   = RECCOUNT()
lnRnkMax  = INT(lnRecNo / 3)
lnRnkRmnd = lnRnkMax +(lnRecNo - lnRnkMax * 3 )
REPLACE FLAG WITH '1' FOR RECNO() <= lnRnkRmnd
REPLACE FLAG WITH '2' FOR RECNO() > lnRnkRmnd .AND. RECNO() <= lnRnkRmnd+lnRnkMax
REPLACE FLAG WITH '3' FOR RECNO() > lnRnkRmnd+lnRnkMax
INDEX ON  STYLE      TAG &lcOrdTmp

SELECT &lcOrdTmp1

DO WHILE !EOF()
   WAIT WINDOW STYLE NOWAIT

   IF !SEEK(STYLE,lcOrdCrs)
      m.Style = STYLE
      *- store 0 TO memvar
      STORE 0 TO m.nOsQty1, m.nOsQty2, m.nOsQty3, m.nOsQty4,;
         m.nOsQty5, m.nOsQty6, m.nOsQty7, m.nOsQty8,;
         m.nWipDye1, m.nWipDye2, m.nWipDye3, m.nWipDye4,;
         m.nWipDye5, m.nWipDye6, m.nWipDye7, m.nWipDye8,;
         m.nAvlInv1, m.nAvlInv2, m.nAvlInv3, m.nAvlInv4,;
         m.nAvlInv5, m.nAvlInv6, m.nAvlInv7, m.nAvlInv8,;
         m.nQShFrst1, m.nQShFrst2, m.nQShFrst3, m.nQShFrst4,;
         m.nQShFrst5, m.nQShFrst6, m.nQShFrst7, m.nQShFrst8,;
         m.nQShScnd1, m.nQShScnd2, m.nQShScnd3, m.nQShScnd4,;
         m.nQShScnd5, m.nQShScnd6, m.nQShScnd7, m.nQShScnd8,;
         m.nQShThrd1, m.nQShThrd2, m.nQShThrd3, m.nQShThrd4,;
         m.nQShThrd5, m.nQShThrd6, m.nQShThrd7, m.nQShThrd8,;
         m.nTotal1, m.nTotal2, m.nTotal3, m.nTotal4,;
         m.nTotal5, m.nTotal6

      *- get Outstanding qty & open order qty for rank #1,#2,#3
      SCAN WHILE STYLE = m.Style
         *B803073,1 KHM 02/28/2000 (Begin) Seeking in the OrdHdr file in order
         *B803073,1                to get the OrdHdr.lalwBkOrd
         =SEEK('O'+ORDER,'OrdHdr')
         *B803073,1 KHM 02/28/2000 (End)
         FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            *B803073,1 KHM 02/28/2000 (Begin) Adding a condition to check
            *B803073,1                if !ordhdr.lalwbkord and !EMPTY(PikTkt)
            *B803073,1                do not accumolate.
            *m.nOsQty&lcI = m.nOsQty&lcI + (Qty&lcI - Pik&lcI)
            m.nOsQty&lcI = m.nOsQty&lcI +;
               IIF(Ordhdr.lAlwBkOrd OR VAL(PikTkt)=0, (Qty&lcI - Pik&lcI), 0)
            *B803073,1 KHM 02/28/2000 (End)

            DO CASE
               CASE FLAG = '1'				&& rank #1
                  *B803073,1 KHM 02/28/2000 (Begin) Adding a condition to check
                  *B803073,1                if !ordhdr.lalwbkord and !EMPTY(PikTkt)
                  *B803073,1                do not accumolate.
                  *m.nQShFrst&lcI = m.nQShFrst&lcI + (Qty&lcI - Pik&lcI)
                  m.nQShFrst&lcI = m.nQShFrst&lcI +;
                     IIF(Ordhdr.lAlwBkOrd OR VAL(PikTkt)=0, (Qty&lcI - Pik&lcI), 0)
                  *B803073,1 KHM 02/28/2000 (End)

               CASE FLAG = '2'				&& rank #2
                  *B803073,1 KHM 02/28/2000 (Begin) Adding a condition to check
                  *B803073,1                if !ordhdr.lalwbkord and !EMPTY(PikTkt)
                  *B803073,1                do not accumolate.
                  *m.nQShScnd&lcI = m.nQShScnd&lcI + (Qty&lcI - Pik&lcI)
                  m.nQShScnd&lcI = m.nQShScnd&lcI +;
                     IIF(Ordhdr.lAlwBkOrd OR VAL(PikTkt)=0, (Qty&lcI - Pik&lcI), 0)
                  *B803073,1 KHM 02/28/2000 (End)

               CASE FLAG = '3'				&& rank #3
                  *B803073,1 KHM 02/28/2000 (Begin) Adding a condition to check
                  *B803073,1                if !ordhdr.lalwbkord and !EMPTY(PikTkt)
                  *B803073,1                do not accumolate.
                  *m.nQShThrd&lcI = m.nQShThrd&lcI + (Qty&lcI - Pik&lcI)
                  m.nQShThrd&lcI = m.nQShThrd&lcI +;
                     IIF(Ordhdr.lAlwBkOrd OR VAL(PikTkt)=0, (Qty&lcI - Pik&lcI), 0)
                  *B803073,1 KHM 02/28/2000 (End)
            ENDCASE
         ENDFOR
      ENDSCAN

      *- get Dye Wip
      =SEEK(m.STYLE,'STYLE')
      SELECT Posln
      *B803073,1 KHM 02/28/2000 (Begin) Removing the checking of the date
      *B803073,1                and adding the checking of the warehouse where
      *B803073,1                the posln warehouse should fall wihtin the
      *B803073,1                seleted ones.
      *SCAN  WHILE STYLE = m.Style FOR POSHDR.Status $ 'OH' ;
      AND BETWEEN(POSHDR.Complete,lDSDate,lDEDate)
   *SCAN WHILE STYLE+cStyType+PO+STR(LINENO,6)+TranCd = m.Style+'D' ;
       FOR POSHDR.Status $ 'OH' AND cWareCode $ lcAllWHs AND BETWEEN(POSHDR.Complete,lDSDate,lDEDate)

     * --- JRB 03/03/2000  Mod. - Remove Due Date condition (Begin)
     SCAN WHILE STYLE+cStyType+PO+STR(LINENO,6)+TranCd = m.Style+'D' ;
            FOR POSHDR.Status $ 'OH' AND cWareCode $ lcAllWHs
     * --- JRB 03/03/2000  Mod. - Remove Due Date condition (End)

     *B803073,1 KHM 02/28/2000 (End)
       FOR lnI = 1 TO 8
         lcI = STR(lnI,1)
         *-get Dye WIP  ( qty with trancd = 1 - qty with trancd = x )
         m.nWipDye&lcI = m.nWipDye&lcI + IIF(TranCD='1',Qty&lcI,-Qty&lcI)
       ENDFOR
     ENDSCAN

     SELECT &lcOrdTmp1
     *B803073,1 KHM 02/28/2000 (Begin) Adding the following code in order
     *B803073,1                to calculate the available inventory for the
     *B803073,1                seleted warehouses from the stydye instead
     *B803073,1                of the style file.
     
     lcStyDye = IIF(llChkdQty,lcAloQty,'STYDYE')
     
     FOR lnArryCnt = 1 TO ALEN(laRpTarget,1)
       IF SEEK(m.STYLE+PADR(laRpTarget[lnArryCnt],6),'STYDYE')
         IF llChkdQty
           =SEEK(m.STYLE+PADR(laRpTarget[lnArryCnt],6),lcAloQty)
         ENDIF
         FOR lnCntr = 1 TO 8
           lcI = STR(lnCntr,1)
           *-Available Inventory
           m.nAvlInv&lcI = m.nAvlInv&lcI + ( STYDYE.Stk&lcI - &lcStyDye..Alo&lcI)
         ENDFOR
       ENDIF
     ENDFOR
     *B803073,1 KHM 02/28/2000 (End)

     * --- Determine OS Qty Per Rank ---
     *B803073,1 TAK 03/07/2000 (Begin) Changing the code due to last discustion with Rick.
     FOR lnI = 1 TO 8
       lcI = STR(lnI,1)

       m.nTotal1 = m.nTotal1 +m.nOsQty&lcI
       m.ntotal2 = m.ntotal2 +m.nWipDye&lcI
       
       lnQScnd&lcI    = m.nQShScnd&lcI
       m.nQShScnd&lcI = m.nQShScnd&lcI + m.nQShFrst&lcI
       m.nQShThrd&lcI = m.nQShThrd&lcI + lnQScnd&lcI + m.nQShFrst&lcI

       *-Available Inventory
       m.nAvlInv&lcI = m.nAvlInv&lcI + m.nWipDye&lcI
       m.ntotal3 = m.ntotal3 + m.nAvlInv&lcI

       *-Quantity shortage for rank # 1
       m.nQShFrst&lcI = m.nAvlInv&lcI - m.nQShFrst&lcI
       m.nTotal4 = m.nTotal4 + m.nQShFrst&lcI

       *-Quantity shortage for rank #1 + #2
       m.nQShScnd&lcI = m.nAvlInv&lcI - m.nQShScnd&lcI
       m.nTotal5 = m.nTotal5 + m.nQShScnd&lcI

       *-Quantity shortage for rank #1 + #2 + #3
       m.nQShThrd&lcI = m.nAvlInv&lcI - m.nQShThrd&lcI
       m.nTotal6 = m.nTotal6 + m.nQShThrd&lcI

     ENDFOR
     *B803073,1 TAK 03/07/2000 (End)

     INSERT INTO (lcOrdCrs) FROM MEMVAR
   ELSE
     SKIP
   ENDIF
ENDDO


SET RELATION TO
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE
SELECT &lcOrdCrs
SET RELATION TO STYLE INTO STYLE ADDITIVE
GO TOP

*------ Collecting data  [End ] --------

*- display the frx file
DO gfDispRe WITH EVAL('lcRpForm')

*- close temp. files
IF USED(lcOrdTmp)
   USE IN (lcOrdTmp)
ENDIF

IF USED(lcOrdTmp1)
   USE IN (lcOrdTmp1)
ENDIF

IF USED(lcOrdCrs)
   USE IN (lcOrdCrs)
ENDIF

IF USED(lcAloQty)
   SELECT &lcAloQty
   USE
ENDIF

*- Delete Temp. Files
ERASE(gcWorkDir+lcOrdTmp+'.DBF')
ERASE(gcWorkDir+lcOrdTmp+'.CDX')
ERASE(gcWorkDir+lcOrdTmp+'.FPT')
ERASE(gcWorkDir+lcOrdTmp1+'.DBF')
ERASE(gcWorkDir+lcOrdTmp1+'.CDX')
ERASE(gcWorkDir+lcOrdTmp1+'.CDX')
ERASE(gcWorkDir+lcOrdCrs+'.DBF')
ERASE(gcWorkDir+lcOrdCrs+'.CDX')
ERASE(gcWorkDir+lcAloQty+'.DBF')
ERASE(gcWorkDir+lcAloQty+'.CDX')

RETURN .T.


*!**************************************************************************
*! Name      : lfSeTOrdr
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/09/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     :
*!**************************************************************************
*! Called from : Option Grid && Report DYE DEMOND REPORT
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
*! Date      : 06/09/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     :
*!**************************************************************************
*! Called from : Option Grid  && Report DYE DEMOND REPORT
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
*! Date      : 06/10/1999
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
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/10/1999
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
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
*! Name      : lfDydWhen
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/10/1999
*! Purpose   : set Last Order Start Ship Date = today()+dye
*!             visibility window
*!**************************************************************************
*! Calls     : gfGetmemVar()
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfDydWhen()
*!**************************************************************************
FUNCTION lfDydWhen
PRIVATE lnarrPos,lnCount,lcStr

llSetRel = .F.
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
DIMENSION  laFlds[55,4]
STORE 0 TO laFlds

laFlds[1,1] = 'STYLE'
laFlds[1,2] = 'C'
laFlds[1,3] = 19
FOR lnCount = 1 TO 8
   laFlds[1+lnCount,1] = 'nOsQty'+ALLTRIM(STR(lnCount))
   laFlds[1+lnCount,2] = 'N'
   laFlds[1+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
   laFlds[9+lnCount,1] = 'nWipDye'+ALLTRIM(STR(lnCount))
   laFlds[9+lnCount,2] = 'N'
   laFlds[9+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
   laFlds[17+lnCount,1] = 'nAvlInv'+ALLTRIM(STR(lnCount))
   laFlds[17+lnCount,2] = 'N'
   laFlds[17+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
   laFlds[25+lnCount,1] = 'nQShFrst'+ALLTRIM(STR(lnCount))
   laFlds[25+lnCount,2] = 'N'
   laFlds[25+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
   laFlds[33+lnCount,1] = 'nQShScnd'+ALLTRIM(STR(lnCount))
   laFlds[33+lnCount,2] = 'N'
   laFlds[33+lnCount,3] = 8
ENDFOR
FOR lnCount = 1 TO 8
   laFlds[41+lnCount,1] = 'nQShThrd'+ALLTRIM(STR(lnCount))
   laFlds[41+lnCount,2] = 'N'
   laFlds[41+lnCount,3] = 8
ENDFOR

FOR lnCount = 1 TO 6
   laFlds[49+lnCount,1] = 'nTotal'+ALLTRIM(STR(lnCount))
   laFlds[49+lnCount,2] = 'N'
   laFlds[49+lnCount,3] = 10
ENDFOR

IF laSortFlag
   FOR lnI = 1 TO 4
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
*! Date      : 06/10/1999
*! Purpose   : Get Starting Position & lenth Of Color
*!**************************************************************************
*! Calls     : ()
*!**************************************************************************
*! Called from : Option Grid
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
lnClrPos = INT(ASCAN(laMajSeg,'C')/7+0.9)
lnStrtPos = laMajSeg[lnClrPos,4]
lnFldLngth= LEN(laMajSeg[lnClrPos,3])

*!**************************************************************************
*! Name      : lfSetRel
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/03/1999
*! Purpose   : Set Relation Betwwen Tables
*!**************************************************************************
*! Calls     :
*!**************************************************************************
*! Called from : icDydMd.prg
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetRel()
*!**************************************************************************
FUNCTION lfSetRel

*IF !llSetRel
SELECT FABRIC
SET ORDER TO TAG cFabric

SELECT POSHDR
SET ORDER TO TAG POSHDR

SELECT POSLN
SET ORDER TO TAG Poslns
SET RELATION TO 'D'+PO INTO POSHDR

SELECT STYLE
SET ORDER TO TAG STYLE
SET RELATION TO STYLE.FABRIC INTO FABRIC 	ADDITIVE
SET RELATION TO STYLE+'D' INTO Posln ADDITIVE

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

llSetRel = .T.
*ENDIF

*!**************************************************************************
*! Name      : lfDeclAry
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/26/1999
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
   RESTORE FROM (gcDataDir+'SORTLEVL.MEM') ADDITIVE
   FOR lnI = 1 TO 4
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
*! Date      : 07/26/1999
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
*! Date      : 07/26/1999
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
*! Date      : 07/26/1999
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
PARAMETER lnFldSort
PRIVATE lnVarDef

IF laSortFlag		&& if restored  default sort levels
   DO CASE
      CASE lnFldSort = 1
         lnVarDef = laSortAry[1,1]
      CASE lnFldSort = 2
         lnVarDef = laSortAry[2,1]
      CASE lnFldSort = 3
         lnVarDef = laSortAry[3,1]
      CASE lnFldSort = 4
         lnVarDef = laSortAry[4,1]
   ENDCASE
ELSE			&& if NOT restored  default sort levels
   DO CASE
      CASE lnFldSort = 1
         lnVarDef = 1
      CASE lnFldSort = 2
         lnVarDef = 2
      CASE lnFldSort = 3
         lnVarDef = 3
      CASE lnFldSort = 4
         lnVarDef = 4
   ENDCASE
ENDIF
RETURN(lnVarDef)



*!*************************************************************
*! Name      : lfFillLoc
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/28/2000
*! Purpose   : Vaildate function to fill location.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillLoc()
*!*************************************************************
*B803073,1 KHM 02/28/2000 Added to be called from O.G
*!*************************************************************
FUNCTION lfFillLoc

DIME laRpSource[1,1]
DIME laRpTarget[1,1]
STORE '' TO laRpSource,laRpTarget
SELECT WareHous
SELECT DISTINCT cWareCode FROM WareHous INTO ARRAY laRpSource

*!*************************************************************
*! Name      : lfvLoc
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/30/1999
*! Purpose   : Function to valid location mover.
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*! Example   : = lfvLoc()
*!*************************************************************
*B803073,1 KHM 02/28/2000 Added to be called from O.G
*!*************************************************************
FUNCTION lfvLoc

= gfMover(@laRpSource,@laRpTarget,'Inventory Location',.T.,'')
