*:************************************************************************
*: Program file  : MFGRGSH.Prg 		Cust. Prog. # 200086,200087
*: Program desc. : Greige goods shortage report
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MF
*: Developer     : WAB- WALID A. WAHAB
*: Date          : 07/12/1999
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetPos  lfGrgWhen  lfSetSTY lfFabSum lfSetFab lfSeTOrdr 
*:                 lfSeTOrdr     
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO MFGrgSh
*:************************************************************************
*: Modifications:
*:B603211,3 WAB 10/17/1999 to colect greige good go to style cost sheet of 
*:B603211,3 WAB            that style and pick up greige good from the style component.  
*C200137,1 TAK 10/10/2000 
*:************************************************************************

*B603540,1 KHM 03/30/2000 (Begin) Checking if there is no warehouse selected
*B603540,1                then do not proceed, otherwise assign the selected
*B603540,1                warehouse to lcAllWhs to be included in the 
*B603540,1                criteria and laWareHDsc to display the description 
*B603540,1                of the warehouses in the FRX
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
*B603540,1 KHM 03/30/2000 (End)

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

*------ Collecting data  [BEGIN] --------
WAIT WINDOW "Preparing data" NOWAIT

SELECT ORDLINE

*B603540,1 KHM 03/30/2000 (Begin) Adding the checking of not deleted lines,
*B603540,1                totqty > 0 and the order's warehouse in the 
*B603540,1                selected warehouses.
*LOCATE FOR &lcRpExp
LOCATE FOR &lcRpExp AND !DELETED('OrdLine') AND TotQty > 0;
                    AND OrdHdr.cWareCode $ lcAllWHs
*B603540,1 KHM 03/30/2000 (End)

IF EOF()
  = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display' 
  RETURN
ENDIF

lcOrdTmp   = gfTEMPNAME()  && TO HOLD RECORDS From Ordline B4 INDEX
lcOrdTmp1  = gfTEMPNAME()  && TO HOLD RECORDS From Ordline after INDEX
lcOrdCrs   = gfTEMPNAME()  && TO HOLD RECORDS FOr Layout
lcContrc   = gfTEMPNAME()  && TO HOLD RECORDS FOr cut tiket records
*B603211,3 WAB - START
lcGrgTmp   = gfTEMPNAME()  &&TO HOLD RECORDS from the style component.
*B603211,3 WAB -END

CREATE DBF (gcWorkDir+lcOrdCrs) FROM ARRAY laFlds
=gfOpenFile(gcWorkDir+lcOrdCrs,'','SH')
INDEX ON STYLE TAG &lcOrdCrs


IF lcFrmTyp $ 'BC'			&& user choose reports format (Both or by Contractor)
  CREATE DBF (gcWorkDir+lcContrc) FROM ARRAY laNewFlds
  =gfOpenFile(gcWorkDir+lcContrc,'','SH')
  INDEX ON STYLE+cContCode TAG &lcContrc
ENDIF

SELECT ORDLINE
*B603540,1 KHM 03/30/2000 (Begin) Adding the checking of not deleted lines,
*B603540,1                totqty > 0 and the order's warehouse in the 
*B603540,1                selected warehouses.
*COPY TO (gcWorkDir+lcOrdTmp) FOR &lcRpExp
COPY TO (gcWorkDir+lcOrdTmp) FOR &lcRpExp AND !DELETED('OrdLine');
                             AND TotQty > 0 AND OrdHdr.cWareCode $ lcAllWHs
*B603540,1 KHM 03/30/2000 (End)

=gfOpenFile(gcWorkDir+lcOrdTmp,'','SH')

*-Sort the temp file
SET RELATION TO 'O'+ORDER 	INTO ORDHDR 	ADDITIVE
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

*- copy to temp file after sorting 
COPY TO (gcWorkDir+lcOrdTmp1)
=gfOpenFile(gcWorkDir+lcOrdTmp1,'','SH')

*B603540,1 KHM 03/30/2000 (Begin) Added
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
*B603540,1 KHM 03/30/2000 (End)

*C200137,1
SELECT &lcOrdTmp1

*- devide  the records into 3 ranck
lnRecNo   = RECCOUNT()
lnRnkMax  = INT(lnRecNo / 3)
lnRnkRmnd = lnRnkMax +(lnRecNo - lnRnkMax * 3 )
REPLACE FLAG WITH '1' FOR RECNO() <= lnRnkRmnd
REPLACE FLAG WITH '2' FOR RECNO() > lnRnkRmnd .AND. RECNO() <= lnRnkRmnd+lnRnkMax
REPLACE FLAG WITH '3' FOR RECNO() > lnRnkRmnd+lnRnkMax

INDEX ON  style TAG &lcOrdTmp

DO WHILE !EOF()
  WAIT WINDOW  SUBSTR(style,lnStrtPos,lnFldLngth) NOWAIT
  IF !SEEK(STYLE,lcOrdCrs)
    m.Style = STYLE 
    *- store 0 TO memvar     
    STORE 0 TO m.nTotGrg1,m.nTotGrg2,m.nTotGrg3,m.nTotGrg4,m.nTotGrg5,m.nTotGrg6,;
               m.nTotGrg7,m.nTotGrg8,;
               m.nNsRFrst1,m.nNsRFrst2,m.nNsRFrst3,m.nNsRFrst4,m.nNsRFrst5,;
               m.nNsRFrst6,m.nNsRFrst7,m.nNsRFrst8,;
               m.nRGrgFrst1,m.nRGrgFrst2,m.nRGrgFrst3,m.nRGrgFrst4,m.nRGrgFrst5,;
               m.nRGrgFrst6,m.nRGrgFrst7,m.nRGrgFrst8,;
               m.nNsRScnd1,m.nNsRScnd2,m.nNsRScnd3,m.nNsRScnd4,m.nNsRScnd5,;
               m.nNsRScnd6,m.nNsRScnd7,m.nNsRScnd8,;
               m.nRGrgScnd1,m.nRGrgScnd2,m.nRGrgScnd3,m.nRGrgScnd4,m.nRGrgScnd5,;
               m.nRGrgScnd6,m.nRGrgScnd7,m.nRGrgScnd8,;
               m.nNsRThrd1,m.nNsRThrd2,m.nNsRThrd3,m.nNsRThrd4,m.nNsRThrd5,;
               m.nNsRThrd6,m.nNsRThrd7,m.nNsRThrd8,;
               m.nRGrgThrd1,m.nRGrgThrd2,m.nRGrgThrd3,m.nRGrgThrd4,m.nRGrgThrd5,;
               m.nRGrgThrd6,m.nRGrgThrd7,m.nRGrgThrd8,;
               lnWipDye1,lnWipDye2,lnWipDye3,lnWipDye4,lnWipDye5,lnWipDye6,;
               lnWipDye7,lnWipDye8,;
               m.nWip1,m.nWip2,m.nWip3,m.nWip4,m.nWip5,m.nWip6,m.nWip7,m.nWip8,;
               m.nTotal1,m.nTotal2,m.nTotal3,m.nTotal4,m.nTotal5,m.nTotal6,;
               m.nTotal7,m.nTotal8

    *B603540,1 KHM 03/30/2000 (Begin) Adding the following variables to hold
    *B603540,1                the available qty for the selected warehouses.
    STORE 0 TO lnAvlInv1, lnAvlInv2, lnAvlInv3, lnAvlInv4,;
               lnAvlInv5, lnAvlInv6, lnAvlInv7, lnAvlInv8
    *B603540,1 KHM 03/30/2000 (End)

	*- get open order qty 
    SCAN WHILE style = m.Style

      *B603540,1 KHM 03/30/2000 (Begin) Seeking in the OrdHdr file in order
      *B603540,1                to get the OrdHdr.lalwBkOrd.
      =SEEK('O'+ORDER,'OrdHdr')
      *B603540,1 KHM 03/30/2000 (End)

      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        DO CASE
          CASE FLAG = '1'		&& ranckt #1
            m.nNsRFrst&lcI = m.nNsRFrst&lcI + ;
            IIF(Ordhdr.lAlwBkOrd OR VAL(PikTkt)=0, (Qty&lcI - Pik&lcI), 0)
                       
          CASE FLAG = '2' 		&& ranckt #2
            m.nNsRScnd&lcI = m.nNsRScnd&lcI + ;
            IIF(Ordhdr.lAlwBkOrd OR VAL(PikTkt)=0, (Qty&lcI - Pik&lcI), 0)
            
          CASE FLAG = '3'  		&& ranckt #3
            m.nNsRThrd&lcI = m.nNsRThrd&lcI + ;
            IIF(Ordhdr.lAlwBkOrd OR VAL(PikTkt)=0, (Qty&lcI - Pik&lcI), 0)
            *B603540,1 KHM 03/30/2000 (End)
            
        ENDCASE
      ENDFOR
    ENDSCAN

    *- get Dye WIP
    =SEEK(m.STYLE,'STYLE')
    SELECT Posln
    
    *B603540,1 KHM 03/30/2000 (Begin) Changing the SCAN command by
    *B603540,1                adding the check for Dye order and removing 
    *B603540,1                the check of the date.
    *SCAN  WHILE STYLE = m.Style FOR POSHDR.Status $ 'OH' AND ;
                                     BETWEEN(POSHDR.Complete,lDSDate,lDEDate)
    SCAN  WHILE Style+cStyType+Po+STR(LineNo,6)+TranCd = m.Style+'D';
          FOR POSHDR.Status $ 'OH' AND cWareCode $ lcAllWHs 
    *B603540,1 KHM 03/30/2000 (End)
    
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        *-get Dye WIP  ( qty with trancd = 1 - qty with trancd = x )
        lnWipDye&lcI = lnWipDye&lcI + IIF(TranCD='1',Qty&lcI,-Qty&lcI)        
      ENDFOR
    ENDSCAN
    SELECT &lcOrdTmp1
    
    *B603540,1 KHM 03/30/2000 (Begin) Adding the following code to get the
    *B603540,1                available qty for the selected warehouses.
    lcStyDye = IIF(llChkdQty,lcAloQty,'STYDYE')
    
    FOR lnArryCnt = 1 TO ALEN(laRpTarget,1)
      IF SEEK(m.STYLE+PADR(laRpTarget[lnArryCnt],6),'STYDYE')
        IF llChkdQty
          =SEEK(m.STYLE+PADR(laRpTarget[lnArryCnt],6),lcAloQty)
        ENDIF
        FOR lnCntr = 1 TO 8
          lcI = STR(lnCntr,1)
          *-Available Inventory
          lnAvlInv&lcI = lnAvlInv&lcI + ( STYDYE.Stk&lcI - &lcStyDye..Alo&lcI)
        ENDFOR
      ENDIF
    ENDFOR
    *B603540,1 KHM 03/30/2000 (End)

    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      *- Total Greige Goods     
      m.nTotGrg&lcI = m.nTotGrg&lcI + lnWipDye&lcI

      lnQScnd&lcI    = m.nNsRScnd&lcI
      m.nNsRScnd&lcI = m.nNsRScnd&lcI + m.nNsRFrst&lcI
      m.nNsRThrd&lcI = m.nNsRThrd&lcI + lnQScnd&lcI + m.nNsRFrst&lcI
      
      *-- Available Inventory
      lnAvlInv&lcI = lnAvlInv&lcI + lnWipDye&lcI
      
      *- Net shortage for ranck#1
      m.nNsRFrst&lcI =  lnAvlInv&lcI - m.nNsRFrst&lcI
      
      *- Net shortage for ranck#1+ranck#2
      m.nNsRScnd&lcI =  lnAvlInv&lcI - m.nNsRScnd&lcI
      
      *- Net shortage for ranck#1+ranck#+ranck#3
      m.nNsRThrd&lcI = lnAvlInv&lcI - m.nNsRThrd&lcI
    ENDFOR
    INSERT INTO (lcOrdCrs) FROM MEMVAR
  ELSE
    SKIP
  ENDIF
ENDDO

 
SET RELATION TO
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE
SELECT (lcOrdCrs)
SET RELATION TO STYLE INTO STYLE ADDITIVE
go top

*B603211,3 WAB - Call function to colect records from the style component.
*B603211,3 WAB - START
=lfGrgSh()
SELECT (lcGrgTmp)
SET RELATION TO STYLE INTO STYLE ADDITIVE
*B603211,3 WAB - END

*------ Collecting data  [End ] --------

*- display the frx file
lcOgPlatForm = 'WINDOW'
DO CASE 
  CASE lcFrmTyp = 'S'			&& by style
    DO gfDispRe WITH 'MFGRGSH'
  CASE lcFrmTyp = 'C'			&& by Contractor
    SELECT &lcContrc
    *B603211,3 WAB - the layout file is lcGrgTmp
    *B603211,3 WAB - START
    *SET RELATION TO STYLE INTO &lcOrdCrs ADDITIVE
    SET RELATION TO STYLE INTO (lcGrgTmp) ADDITIVE
    GO TOP
    DO gfDispRe WITH 'MFGRCNT'
  OTHERWISE					    &&  both(style,contractor)
    DO gfDispRe WITH 'MFGRGSH'
    SELECT &lcContrc
    *B603211,3 WAB - the layout file is lcGrgTmp
    *B603211,3 WAB - START
    *SET RELATION TO STYLE INTO &lcOrdCrs ADDITIVE
    SET RELATION TO STYLE INTO (lcGrgTmp) ADDITIVE
    GO TOP
    DO gfDispRe WITH 'MFGRCNT'
ENDCASE  

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

IF USED(lcContrc)
  USE IN (lcContrc)
ENDIF


*- Delete temp. files
ERASE(gcWorkDir+lcOrdTmp+'.DBF')
ERASE(gcWorkDir+lcOrdTmp+'.CDX')
ERASE(gcWorkDir+lcOrdTmp+'.FPT')
ERASE(gcWorkDir+lcOrdTmp1+'.DBF')
ERASE(gcWorkDir+lcOrdTmp1+'.CDX')
ERASE(gcWorkDir+lcOrdTmp1+'.CDX')
ERASE(gcWorkDir+lcOrdCrs+'.DBF')
ERASE(gcWorkDir+lcOrdCrs+'.CDX')

IF lcFrmTyp $ 'BC'			&& (Both or by Contractor)
  ERASE(gcWorkDir+lcContrc+'.DBF')
  ERASE(gcWorkDir+lcContrc+'.CDX')
ENDIF


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

*B603211,3 WAB - START

*!**************************************************************************
*! Name      : lfGrgSh
*! Developer : Khalid Mohi El-Din
*! Date      : 04/20/2000
*! Purpose   : update temp file for collect greige goods
*!**************************************************************************
*! Example   : =lfGrgSh()
*!**************************************************************************
FUNCTION lfGrgSh

* --- Change All Styles To cStyMajors ---
SELECT (lcOrdCrs)
=AFIELDS(laFileStru)
Create Table (gcWorkDir+lcGrgTmp) FROM ARRAY laFileStru
INDEX ON STYLE TAG (lcGrgTmp)

SELECT (lcOrdCrs)
REPLACE ALL style WITH SUBSTR(style,1,lnStrtPos-2)

* --- Total on cStyMajors ---
lcgrgtot = gfTempName()
SELECT style,;
       SUM(nNsRFrst1) AS nNsRFrst1, SUM(nNsRFrst2) AS nNsRFrst2,;
       SUM(nNsRFrst3) AS nNsRFrst3, SUM(nNsRFrst4) AS nNsRFrst4,;
       SUM(nNsRFrst5) AS nNsRFrst5, SUM(nNsRFrst6) AS nNsRFrst6,;
       SUM(nNsRFrst7) AS nNsRFrst7, SUM(nNsRFrst8) AS nNsRFrst8,;
       SUM(nNsRScnd1) AS nNsRScnd1, SUM(nNsRScnd2) AS nNsRScnd2,;
       SUM(nNsRScnd3) AS nNsRScnd3, SUM(nNsRScnd4) AS nNsRScnd4,;
       SUM(nNsRScnd5) AS nNsRScnd5, SUM(nNsRScnd6) AS nNsRScnd6,;
       SUM(nNsRScnd7) AS nNsRScnd7, SUM(nNsRScnd8) AS nNsRScnd8,;
       SUM(nNsRThrd1) AS nNsRThrd1, SUM(nNsRThrd2) AS nNsRThrd2,;
       SUM(nNsRThrd3) AS nNsRThrd3, SUM(nNsRThrd4) AS nNsRThrd4,;
       SUM(nNsRThrd5) AS nNsRThrd5, SUM(nNsRThrd6) AS nNsRThrd6,;
       SUM(nNsRThrd7) AS nNsRThrd7, SUM(nNsRThrd8) AS nNsRThrd8;
    FROM (lcOrdCrs);
    GROUP BY 1;
    NOCONSOLE;
    INTO DBF(gcWorkDir+lcgrgtot)



SELECT (lcOrdCrs)
* --- Remove Previous Records ---
DELETE ALL
* --- Put In New Records ---
APPEND FROM (gcWorkDir+lcGrgTot)

*-- Close this file
SELECT &lcGrgTot
USE

IF !USED('BOM')
   USE BOM IN 0
ENDIF

SELECT bom
SET ORDER TO bom

* --- Replace All cStyMajors With BOM Style Component (Greige Style) ---
SELECT (lcOrdCrs)
SET RELATION TO style + "4" INTO bom
REPLACE ALL style WITH bom.item

* --- Total On Greige Style ---
SELECT style,;
       SUM(nNsRFrst1) AS nNsRFrst1,SUM(nNsRFrst2) AS nNsRFrst2,;
       SUM(nNsRFrst3) AS nNsRFrst3,SUM(nNsRFrst4) AS nNsRFrst4,;
       SUM(nNsRFrst5) AS nNsRFrst5,SUM(nNsRFrst6) AS nNsRFrst6,;
       SUM(nNsRFrst7) AS nNsRFrst7,SUM(nNsRFrst8) AS nNsRFrst8,;
       SUM(nNsRScnd1) AS nNsRScnd1,SUM(nNsRScnd2) AS nNsRScnd2,;
       SUM(nNsRScnd3) AS nNsRScnd3,SUM(nNsRScnd4) AS nNsRScnd4,;
       SUM(nNsRScnd5) AS nNsRScnd5,SUM(nNsRScnd6) AS nNsRScnd6,;
       SUM(nNsRScnd7) AS nNsRScnd7,SUM(nNsRScnd8) AS nNsRScnd8,;
       SUM(nNsRThrd1) AS nNsRThrd1,SUM(nNsRThrd2) AS nNsRThrd2,;
       SUM(nNsRThrd3) AS nNsRThrd3,SUM(nNsRThrd4) AS nNsRThrd4,;
       SUM(nNsRThrd5) AS nNsRThrd5,SUM(nNsRThrd6) AS nNsRThrd6,;
       SUM(nNsRThrd7) AS nNsRThrd7,SUM(nNsRThrd8) AS nNsRThrd8;
    FROM (lcOrdCrs);
    GROUP BY 1;
    NOCONSOLE;
    INTO DBF(gcWorkDir+lcgrgtot)

SELECT (lcOrdCrs)
* --- Remove Previous Records ---
DELETE ALL
* --- Put In New Records ---
APPEND FROM (gcWorkDir+lcGrgTot)
*--- Close The File   
SELECT &lcGrgTot
USE

*--  Now add in data for Greige Style Stk 
IF !USED('Style')
   USE style IN 0
ENDIF

SELECT style
SET ORDER TO style

SELECT (lcOrdCrs)
*SET RELATION TO style INTO style
*REPLACE ALL ntotgrg1 WITH Style.stk1,;
            ntotgrg2 WITH Style.stk2,;
            ntotgrg3 WITH Style.stk3,;
            ntotgrg4 WITH Style.stk4,;
            ntotgrg5 WITH Style.stk5,;
            ntotgrg6 WITH Style.stk6,;
            ntotgrg7 WITH Style.stk7,;
            ntotgrg8 WITH Style.stk8

lcStyDye = IIF(llChkdQty,lcAloQty,'STYDYE')
SCAN
  lcStyle = SUBSTR(Style,1,12)
  STORE 0 TO lnAlo1,lnAlo2,lnAlo3,lnAlo4,lnAlo5,lnAlo6,lnAlo7,lnAlo8,;
             lnStk1,lnStk2,lnStk3,lnStk4,lnStk5,lnStk6,lnStk7,lnStk8
  FOR lnArryCnt = 1 TO ALEN(laRpTarget,1)
    SELECT StyDye
    SEEK lcStyle 
    SCAN REST WHILE Style+cWareCode+Dyelot = lcStyle;
              FOR cWareCode=PADR(laRpTarget[lnArryCnt],6)
      FOR lnCnt = 1 TO 8
        lcCnt = STR(lnCnt,1)
        lnStk&lcCnt = lnStk&lcCnt + Stk&lcCnt 
      ENDFOR        
    ENDSCAN          
    IF llChkdQty
      SELECT (lcStyDye)
    ELSE
      SELECT StyDye
    ENDIF  
    SEEK lcStyle
    SCAN WHILE Style+cWareCode = lcStyle;
         FOR cWareCode=PADR(laRpTarget[lnArryCnt],6)
      FOR lnCnt = 1 TO 8
        lcCnt = STR(lnCnt,1)
        lnAlo&lcCnt = lnAlo&lcCnt + Alo&lcCnt 
      ENDFOR
    ENDSCAN  
  ENDFOR
  SELECT (lcOrdCrs)
  REPLACE ntotgrg1 WITH lnStk1 - lnAlo1, ntotgrg2 WITH lnStk2 - lnAlo2,;
          ntotgrg3 WITH lnStk3 - lnAlo3, ntotgrg4 WITH lnStk4 - lnAlo4,;
          ntotgrg5 WITH lnStk5 - lnAlo5, ntotgrg6 WITH lnStk6 - lnAlo6,;
          ntotgrg7 WITH lnStk7 - lnAlo7, ntotgrg8 WITH lnStk8 - lnAlo8
  
ENDSCAN
    
    
    FOR lnArryCnt = 1 TO ALEN(laRpTarget,1)
      IF SEEK(m.STYLE+PADR(laRpTarget[lnArryCnt],6),'STYDYE')
        IF llChkdQty
          =SEEK(m.STYLE+PADR(laRpTarget[lnArryCnt],6),lcAloQty)
        ENDIF
        FOR lnCntr = 1 TO 8
          lcI = STR(lnCntr,1)
          *-Available Inventory
          lnAvlInv&lcI = lnAvlInv&lcI + ( STYDYE.Stk&lcI - &lcStyDye..Alo&lcI)
        ENDFOR
      ENDIF
    ENDFOR

SET RELATION TO
REPLACE ALL nRgrgFrst1 WITH nTotGrg1 - nNSRFrst1,nRgrgFrst2 WITH nTotGrg2 - nNSRFrst2,;
            nRgrgFrst3 WITH nTotGrg3 - nNSRFrst3,nRgrgFrst4 WITH nTotGrg4 - nNSRFrst4,;
            nRgrgFrst5 WITH nTotGrg5 - nNSRFrst5,nRgrgFrst6 WITH nTotGrg6 - nNSRFrst6,;
            nRgrgFrst7 WITH nTotGrg7 - nNSRFrst7,nRgrgFrst8 WITH nTotGrg8 - nNSRFrst8,;
            nRgrgScnd1 WITH nTotGrg1 - nNSRScnd1,nRgrgScnd2 WITH nTotGrg2 - nNSRScnd2,;
            nRgrgScnd3 WITH nTotGrg3 - nNSRScnd3,nRgrgScnd4 WITH nTotGrg4 - nNSRScnd4,;
            nRgrgScnd5 WITH nTotGrg5 - nNSRScnd5,nRgrgScnd6 WITH nTotGrg6 - nNSRScnd6,;
            nRgrgScnd7 WITH nTotGrg7 - nNSRScnd7,nRgrgScnd8 WITH nTotGrg8 - nNSRScnd8,;
            nRgrgThrd1 WITH nTotGrg1 - nNSRThrd1,nRgrgThrd2 WITH nTotGrg2 - nNSRThrd2,;
            nRgrgThrd3 WITH nTotGrg3 - nNSRThrd3,nRgrgThrd4 WITH nTotGrg4 - nNSRThrd4,;
            nRgrgThrd5 WITH nTotGrg5 - nNSRThrd5,nRgrgThrd6 WITH nTotGrg6 - nNSRThrd6,;
            nRgrgThrd7 WITH nTotGrg7 - nNSRThrd7,nRgrgThrd8 WITH nTotGrg8 - nNSRThrd8
SELECT (lcGrgTmp)
APPEND FROM (gcWorkDir+lcOrdCrs)
=lfGrgTot()

*!**************************************************************************
*! Name      : lfGrgSh
*! Developer : WAB - WALID A. WAHAB
*! Date      : 10/19/1999
*! Purpose   : update temp file for collect greige goods
*!**************************************************************************
*! Calls     :
*!**************************************************************************
*! Called from : icGrgSh.prg
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfGrgSh()
*!**************************************************************************
FUNCTION lfGrgSh1
SELECT (lcOrdCrs)
=AFIELDS(laFileStru)
Create Table (gcWorkDir+lcGrgTmp) FROM ARRAY laFileStru
INDEX ON STYLE TAG (lcGrgTmp)
SELECT (lcOrdCrs)
GO top

SCAN 
  SCATTER MEMVAR
  *-- check if the style have an style components in style cost sheet file
  *B603540,1 KHM 03/30/2000 (Begin) Getting the stkn qty from the stydye
  FOR lnArryCnt = 1 TO ALEN(laRpTarget,1)
    IF SEEK(STYLE+PADR(laRpTarget[lnArryCnt],6),'STYDYE')
      FOR lnCntr = 1 TO 8
        lcI = STR(lnCntr,1)
        *- Stock quantities in the selected warehouses.
        m.nTotGrg&lcI = m.nTotGrg&lcI + STYDYE.Stk&lcI        
      ENDFOR
    ENDIF
  ENDFOR
  *B603540,1 KHM 03/30/2000 (End)
  
  IF SEEK(SUBSTR(style,1,lnStrtPos-2),'BOM')
    SELECT BOM
    SCAN REST FOR  cItmMajor = SUBSTR(style,1,lnStrtPos-2) 
      *-- get the item name
      IF (cItmMask = m.Style OR SUBSTR(CitmMask,lnStrtPos,1) = '*' ) ;
             AND cCatgTyp = 'S'
        m.Style=IIF(SUBSTR(Item,lnStrtPos,1)='*',;
          SUBSTR(Item,1,lnStrtPos-1)+SUBSTR(style,lnStrtPos,20-lnStrtPos),Item)
        IF SEEK(m.Style,lcGrgTmp)
          SELECT (lcGrgTmp)
          FOR lnCount = 1 TO 8      
            lcCount = ALLTRIM(STR(lnCount))
            REPLACE &lcGrgTmp..nNsRFrst&lcCount  WITH ;
                    &lcGrgTmp..nNsRFrst&lcCount  + &lcOrdCrs..nNsRFrst&lcCount ,;
                    &lcGrgTmp..nNsRScnd&lcCount  WITH ;
                    &lcGrgTmp..nNsRScnd&lcCount  + &lcOrdCrs..nNsRScnd&lcCount ,;
                    &lcGrgTmp..nNsRThrd&lcCount  WITH ;
                    &lcGrgTmp..nNsRThrd&lcCount  + &lcOrdCrs..nNsRThrd&lcCount ,;
                    &lcGrgTmp..nTotal&lcCount    WITH ;
                    &lcGrgTmp..nTotal&lcCount    + &lcOrdCrs..nTotal&lcCount 
            *B603540,1 KHM 03/30/2000 (Begin) Added
            REPLACE &lcGrgTmp..nTotGrg&lcCount WITH  ;
                    &lcGrgTmp..nTotGrg&lcCount + m.nTotGrg&lcCount
            *B603540,1 KHM 03/30/2000 (End)        
          ENDFOR
        ELSE
          INSERT INTO (lcGrgTmp) FROM MEMVAR
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
=lfGrgTot()  
*!**************************************************************************
*! Name      : lfGrgTot
*! Developer : WAB - WALID A. WAHAB
*! Date      : 10/20/1999
*! Purpose   : update temp file with the unalocate qty and the Wip qty
*!**************************************************************************
*! Calls     :
*!**************************************************************************
*! Called from : icGrgSh.prg
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfGrgTot()
*!**************************************************************************
FUNCTION lfGrgTot
*- get WIP from Cut Ticket lines
SELECT (lcGrgTmp)
GO TOP
SCAN 
  =SEEK(&lcGrgTmp..STYLE,'STYLE')
  SELECT CUTTKTL 
  IF lcFrmTyp $ 'BC'			&& user choose reports format (Both or by Contractor)
    lcOldCtkt = CutTkt
    STORE 0 TO laOpenCt,lnTotal,laCutTkt
    laCutTkt[1]  = m.style
  ENDIF
  STORE 0 TO lnWip1,lnWip2,lnWip3,lnWip4,lnWip5,lnWip6,lnWip7,lnWip8

  *B603540,1 KHM 03/30/2000 (Begin) Changing the scan by removing the
  *B603540,1                checking of the date.
  *SCAN WHILE STYLE = &lcGrgTmp..Style FOR CUTTKTH.Status $ 'OH' AND ;
                  BETWEEN(CUTTKTH.Complete,lDSDate,lDEDate)  
  SCAN WHILE Style+CutTkt+TranCd = &lcGrgTmp..Style FOR CUTTKTH.Status $ 'OH'
  *B603540,1 KHM 03/30/2000 (End)
  
    IF lcFrmTyp $ 'BC'			&& user choose reports format (Both or by Contractor)
      laCuttkt[21] = CUTTKTH.Complete
      IF Cuttkt <> lcOldCtkt
        FOR lnI = 1 TO 8
          lnTotal = lntotal + laOpenCt[lnI] 
        ENDFOR 
        *- get the contractor code 
        SELECT MFGOPRDT
        =SEEK('M'+lcOldCtkt)
        SCAN WHILE ALLTRIM(CimTyp+CtktNo) = ALLTRIM('M'+lcOldCtkt)
          laCuttkt[2] = cContCode
          *-call function for update temp file
          =lfUpdTemp()
        ENDSCAN
        SELECT CUTTKTL  
        STORE 0 TO laOpenCt,lnTotal
        lcOldCtkt = Cuttkt
      ENDIF
    ENDIF
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      IF lcFrmTyp $ 'BC'		
         *-get open Qty for Cuttkt ( qty with trancd = 1 - qty with trancd = x )
         laOpenCt[lnI] = laOpenCt[lnI] + IIF(TranCD='1',Qty&lcI,-Qty&lcI)
      ENDIF
      *- get WIP  for Style ( qty with trancd = 1 - qty with trancd = x )
      lnWip&lcI    = lnWip&lcI + IIF(TranCD='1',Qty&lcI,-Qty&lcI)
    ENDFOR
  ENDSCAN  

 
  *-last record didn 't checked in last loop
  IF lcFrmTyp $ 'BC'			&& user choose reports format (Both or by Contractor)
    FOR lnI = 1 TO 8
      lnTotal = lntotal + laOpenCt[lnI] 
    ENDFOR 
    *-get the contractor code 
    SELECT MFGOPRDT
    =SEEK('M'+lcOldCtkt)
    SCAN WHILE ALLTRIM(CimTyp+CtktNo) = ALLTRIM('M'+lcOldCtkt)
      laCuttkt[2] = cContCode
      *-call function for update temp file
      =lfUpdTemp()
    ENDSCAN
  ENDIF
  =SEEK(&lcGrgTmp..STYLE,'STYLE')

  SELECT (lcGrgTmp)
  SCATTER MEMVAR MEMO
  *C200137,1
  *STORE 0 TO m.nTotal1,m.nTotal3,m.nTotal5,m.nTotal7,m.nTotal8
  STORE 0 TO m.nTotal1,m.nTotal2,m.nTotal3,m.nTotal4,m.nTotal5,m.nTotal6,m.nTotal7,m.nTotal8
  FOR lnCount = 1 TO 8      
    lcCount = ALLTRIM(STR(lnCount))
    *-Total Greige good
    
    m.nWip&lcCount     = lnWip&lcCount
    *- Rem. griege goods for ranck#1
    m.nRGrgFrst&lcCount =  m.nNsRFrst&lcCount - m.nTotGrg&lcCount
    *- Rem. griege goods for ranck#2
    m.nRGrgScnd&lcCount =  m.nNsRScnd&lcCount - m.nTotGrg&lcCount
    *- Rem. griege goods for ranck#3
    m.nRGrgThrd&lcCount =  m.nNsRThrd&lcCount - m.nTotGrg&lcCount      

    m.nTotal1  = m.nTotal1 + m.nTotGrg&lcCount
    m.nTotal2  = m.nTotal2 + m.nNsRFrst&lcCount
    m.nTotal3  = m.nTotal3 + m.nRGrgFrst&lcCount
    m.nTotal4  = m.nTotal4 + m.nNsRScnd&lcCount
    m.nTotal5  = m.nTotal5 + m.nRGrgScnd&lcCount 
    m.nTotal6  = m.nTotal6 + m.nNsRThrd&lcCount
    m.nTotal7  = m.nTotal7 + m.nRGrgThrd&lcCount
    m.nTotal8  = m.nTotal8 + m.nWip&lcCount            

  ENDFOR
  GATHER MEMVAR MEMO
ENDSCAN
 

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
*B603540,1 KHM 03/30/2000 To fill the locations 
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
*B603540,1 KHM 03/30/2000 To display the mover.
*!*************************************************************
FUNCTION lfvLoc

= gfMover(@laRpSource,@laRpTarget,'Inventory Location',.T.,'')


