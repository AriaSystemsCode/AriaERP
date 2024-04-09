*:************************************************************************
*: Program file  : POBRCDDL.PRG    C037855
*: Program desc. : Print Bar Codes for receiving P/Os. For David Luke
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Purchase Orders (PO).
*:      Developer: Mohamed Shokry (MHM)
*:           Date: 03/07/2004
*:************************************************************************
*: For Laser Printer Only.
*:************************************************************************
* Modifications
*C131050,1 TMI 06/13/2006 Amend custom bar code label program 037855                  
*:***************************************************************************


lcHeader = IIF(lcPoTyp = "M" , "CutTktH" , "PosHdr")
lcDetail = IIF(lcPoTyp = "M" , "CutTktL" , "PosLn")
*C131050,1  TMI [Start] case if sales order type is selected
lcOrdCnLn = gfTempName()  && get temp name for order cancelled

IF lcPoTyp = "O"
  STORE SPACE(6) TO lcFstOrd,lcLstOrd
  lcCurNam = ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFXFlt,'ORDHDR.ORDER') ,1),6])
  lcCnCrit = ".T."
  IF !EMPTY(lcCurNam) AND USED(lcCurNam) .AND. RECCOUNT(lcCurNam) > 0
    SELECT &lcCurNam
    GO TOP
    IF !EOF(lcCurNam)
      lcFstOrd = &lcCurNam..ORDER
      GO BOTTOM 
      lcLstOrd = &lcCurNam..ORDER
    ENDIF
    lcCnCrit = ""
    GO TOP
    SCAN
      lcCnCrit = lcCnCrit + ORDER + "|"
    ENDSCAN
    lcCnCrit = "ORDER $ '" + lcCnCrit +"'"
  ENDIF
  *- Get summation per line for all cancelled lines for the selected orders  
  SELECT CORDTYPE+ORDER+STR(LINENO,6) AS KEY , ;
         SUM(QTY1) AS QTY1, SUM(QTY2) AS QTY2, SUM(QTY3) AS QTY3, SUM(QTY4) AS QTY4, ;
         SUM(QTY5) AS QTY5, SUM(QTY6) AS QTY6, SUM(QTY7) AS QTY7, SUM(QTY8) AS QTY8  ;
    FROM ORDCANLN ;
    WHERE &lcCnCrit ;
    GROUP BY KEY ;
    INTO TABLE (gcWorkDir+lcOrdCnLn)
  INDEX ON KEY TAG &lcOrdCnLn 
ELSE
  *C131050,1  TMI [End  ] 

  IF lcPoTyp = "M"
    lcCurNam = ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFXFlt,'CUTTKTL.CUTTKT') ,1),6])
  ELSE
    IF lcPoTyp <> "S"
      *--Read the P/Os cursor name.
      lcCurNam = ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFXFlt,'POSLN.PO') ,1),6])
    ELSE
      lcCurNam =''
    ENDIF  
  ENDIF
  
  STORE SPACE(6) TO lcFstPo , lcLstPo 
  IF lcRpExp <> ".T." AND !EMPTY(lcCurNam) AND USED(lcCurNam)
    GO TOP IN (lcCurNam)
    IF !EOF(lcCurNam)
      lcFstPo = IIF(lcPoTyp = 'M',&lcCurNam..CUTTKT,&lcCurNam..PO)
      GO BOTTOM IN (lcCurNam)
      lcLstPo = IIF(lcPoTyp = 'M',&lcCurNam..CUTTKT,&lcCurNam..PO)
    ENDIF
  ENDIF
  IF lcPoTyp <> "S"
    IF lcRpBasdOn = 'R'
      lcRpExp = lcRpExp + " .AND. TranCd = '2'"
    ELSE
      lcRpExp = lcRpExp + " .AND. TranCd = '1'"
    ENDIF
  ENDIF

  *C131050,1  TMI [Start] close the above if statement
ENDIF  
*C131050,1  TMI [End  ] 

*--Read structure separators used.
I=1
SELECT ICISTRU
SCAN FOR cItemRecTy='U' AND !EMPTY(cISegSepr)
  DIME laSep[I]
  laSep[I]=cISegSepr
  I=I+1
ENDSCAN 

*--Read the Style length.
lcStyPct = gfItemMask('PI')
lnStyLen = LEN(lcStyPct)

*--Fonts librorary installed.
llLibInst = .T.

llPrntFrm = .T.

*--Create bar codes temp file.
lcTmpBars = gfTempName()
  *--Tabel for 3 labels across.

*--Create bar codes temp file.
  *--Tabel for 3 labels across.
*C131050,1  TMI [Start] Add price field to the temp file
*CREATE DBF (gcWorkDir+lcTmpBars) ;
*    (cStyMaj1 C(19),cStyDesc1 C(60), CColrDsc1 C(30),SclDim1 C(5) , SclSz1 C(5) , cStyBarCd1 C(13) , Po1 C(6) ,;
*     cStyMaj2 C(19),cStyDesc2 C(60), CColrDsc2 C(30),SclDim2 C(5) , SclSz2 C(5) , cStyBarCd2 C(13) , Po2 C(6) ,;
*     cStyMaj3 C(19),cStyDesc3 C(60), CColrDsc3 C(30),SclDim3 C(5) , SclSz3 C(5) , cStyBarCd3 C(13) , Po3 C(6) ,;
*     cStyMaj4 C(19),cStyDesc4 C(60), CColrDsc4 C(30),SclDim4 C(5) , SclSz4 C(5) , cStyBarCd4 C(13) , Po4 C(6))
  CREATE DBF (gcWorkDir+lcTmpBars) ;
    (cStyMaj1 C(19),cStyDesc1 C(60), CColrDsc1 C(30),SclDim1 C(5) , SclSz1 C(5) , cStyBarCd1 C(13) , Po1 C(6) , Price1 N(12,2),;
     cStyMaj2 C(19),cStyDesc2 C(60), CColrDsc2 C(30),SclDim2 C(5) , SclSz2 C(5) , cStyBarCd2 C(13) , Po2 C(6) , Price2 N(12,2),;
     cStyMaj3 C(19),cStyDesc3 C(60), CColrDsc3 C(30),SclDim3 C(5) , SclSz3 C(5) , cStyBarCd3 C(13) , Po3 C(6) , Price3 N(12,2),;
     cStyMaj4 C(19),cStyDesc4 C(60), CColrDsc4 C(30),SclDim4 C(5) , SclSz4 C(5) , cStyBarCd4 C(13) , Po4 C(6) , Price4 N(12,2) )
*C131050,1  TMI [End  ] 

*C131050,1  TMI [Start] case if sales order type is selected
IF lcPoTyp = "O"
  SELECT ORDLINE
  SET RELATION TO CORDTYPE+ORDER INTO ORDHDR
  SET RELATION TO STYLE          INTO STYLE  ADDITIVE
  SET RELATION TO CORDTYPE+ORDER+STR(LINENO,6) INTO &lcOrdCnLn ADDITIVE 
  GO TOP
  IF !EMPTY(lcFstOrd) AND !EMPTY(lcLstOrd)
    SEEK 'O'+lcFstOrd
    lcWlExpr = "ORDER<=lcLstOrd"
  ELSE
    lcWlExpr = ".T."
  ENDIF
ELSE
  *C131050,1  TMI [End  ] 

  IF lcPoTyp <> "S"
    SELECT (lcDetail)
    IF lcPoTyp = "M"
      SELECT CutTktL
      SET RELATION TO CUTTkt INTO CutTktH
      SET RELATION TO STYLE INTO STYLE ADDITIVE
    ELSE
      SELECT PosLn
      SET RELATION TO cStyType+PO INTO PosHdr 
      SET RELATION TO STYLE INTO STYLE ADDITIVE
    ENDIF
  
    SELECT (lcDetail)
    GO TOP
    IF !EMPTY(lcFstPo) AND !EMPTY(lcLstPo)
      IF lcPoTyp = "M"
        SEEK lcFstPo
        lcWlExpr = "CUTTKT<=lcLstPo"
      ELSE
        SEEK lcPoTyp + lcFstPo
        lcWlExpr = "cStyType = lcPoTyp AND Po<=lcLstPo"
      ENDIF
    ELSE
      IF lcPoTyp = "M"
        lcWlExpr = ".T."
      ELSE
        SEEK lcPoTyp
        lcWlExpr = "cStyType = lcPoTyp"
      ENDIF
    ENDIF
  ELSE
    SELECT STYLE
    lcWlExpr = ".T."
    LOCATE
  ENDIF
  *C131050,1  TMI [Start] close the above if statement
ENDIF  
*C131050,1  TMI [End  ]     

lnLbCount = 1  

SCAN REST WHILE &lcWlExpr FOR &lcRpExp
  WAIT WINDOW IIF(lcPoTyp="M",'P/O#:'+ CUTTKT,IIF(lcPoTyp="S",'','P/O#:'+Po))+'  Style:'+Style NOWAIT
  lcStyMaj    = LEFT(STYLE.cdivision,2) + ALLTRIM(IIF(lcPoTyp="M" , PADR(CUTTKTL.Style,lnMajLen) , PADR(STYLE.Style,lnMajLen)))
  lcStyDesc  = Style.Desc1
  lcColrDsc   = gfCodDes(SUBSTR(Style.Style,lnClrPo,lnColorLen),'Color')
  =SEEK('S'+STYLE.SCALE,'SCALE')

  FOR I=1 TO SCALE.Cnt
    lcSz = STR(I,1)
    *C131050,1  TMI [Start] check case if sales order type is selected
    *IF EMPTY(Posln.Qty&lcSz )  AND lcPoTyp <> "S"    
    IF IIF(lcPoTyp = 'O', EMPTY(ORDLINE.BOOK&lcSz - &lcOrdCnLn..QTY&lcSz ) , lcPoTyp <> "S" .AND. EMPTY(&lcDetail..Qty&lcSz ) )
      *C131050,1  TMI [End  ] 
      LOOP
    ENDIF
    IF lnNoOfLabl > 0

      IF SEEK(LEFT(SCALE.SCALE,2),'SCALEHD')
        lcSclDim   = SCALEHD.cDim1Desc 
        lcSclSz    = SCALE.SZ&lcSz 
      ELSE
        lcSclDim = ''
        lcSclSz  = ''
      ENDIF  
      IF SEEK(STYLE.Style + lcSz,'STYLEUPC')
        lcStyBarCd = STYLEUPC.cupcnum1+STYLEUPC.cupcnum2+STYLEUPC.cupcnum3
      ELSE
        lcStyBarCd = ''
      ENDIF  
      
      *C131050,1  TMI [Start] case if the sales order type is selected
      IF lcPoTyp = "O"
        IF ORDLINE.BOOK&lcSz - &lcOrdCnLn..QTY&lcSz > 0
          lnNoOfLabl   = CEILING((ORDLINE.BOOK&lcSz - &lcOrdCnLn..QTY&lcSz) * (lnPerc/100))
        ENDIF
      ELSE
        *C131050,1  TMI [End  ] 
      
        *--mhm2000
        IF lcPoTyp <> "S"
          IF &lcDetail..Qty&lcSz > 0
            lnNoOfLabl   = IIF(lcPoTyp="M" , CEILING(CUTTKTL.Qty&lcSz * (lnPerc/100)) , CEILING(POSLN.Qty&lcSz * (lnPerc/100)))
          ENDIF
        ENDIF    
        *--mhm2000
      
        *C131050,1  TMI [Start] close the above if statement
      ENDIF
      *C131050,1  TMI [End  ] 
      
      SELECT (lcTmpBars)
      FOR lnQtyCount = 1 TO lnNoOfLabl
          IF lnLBCount =1
            APPEND BLANK
          ENDIF
          DO CASE
            CASE lnlbCount =1
              REPLACE cStyMaj1     WITH lcStyMaj,;
                      cStyDesc1    WITH lcStyDesc,;
                      CColrDsc1    WITH lcColrDsc,;
                      SclDim1      WITH lcSclDim,;
                      SclSz1       WITH lcSclSz,;
                      cStyBarCd1   WITH lcStyBarCd
              *C131050,1  TMI [Start] Do not update PO1,...,4 fields if Sales order type is selected.
              IF lcPoTyp = "O"
                IF llRpPrPric                  
                  REPLACE Price1 WITH IIF(lcRpPrTyp='O',ORDLINE.PRICE,; 
                                      IIF(lcRpPrTyp='R',STYLE.NSUGRETPRI , STYLE.PRICE&lcRpPrTyp ))
                ENDIF
              ELSE
                *C131050,1  TMI [End  ]
                IF lcPoTyp <> "S"
                  REPLACE Po1        WITH Posln.Po
                ENDIF
                *C131050,1  TMI [Start]
              ENDIF
              *C131050,1  TMI [End  ]
              
            CASE lnlbCount =2
              REPLACE cStyMaj2     WITH lcStyMaj,;
                      cStyDesc2    WITH lcStyDesc,; 
                      CColrDsc2    WITH lcColrDsc,;
                      SclDim2      WITH lcSclDim,;
                      SclSz2       WITH lcSclSz,;
                      cStyBarCd2   WITH lcStyBarCd
              *C131050,1  TMI [Start] Do not update PO1,...,4 fields if Sales order type is selected.
              IF lcPoTyp = "O"
                IF llRpPrPric
                  REPLACE Price2 WITH IIF(lcRpPrTyp='O',ORDLINE.PRICE,; 
                                      IIF(lcRpPrTyp='R',STYLE.NSUGRETPRI , STYLE.PRICE&lcRpPrTyp ))
                ENDIF
              ELSE
                *C131050,1  TMI [End  ] 
                IF lcPoTyp <> "S"
                  REPLACE Po2        WITH Posln.Po
                ENDIF
                *C131050,1  TMI [Start] 
              ENDIF
              *C131050,1  TMI [End  ] 

            CASE lnlbCount =3          
              REPLACE cStyMaj3     WITH lcStyMaj,;
                      cStyDesc3    WITH lcStyDesc,; 
                      CColrDsc3    WITH lcColrDsc,;
                      SclDim3      WITH lcSclDim,;
                      SclSz3       WITH lcSclSz,;
                      cStyBarCd3   WITH lcStyBarCd
                      
              *C131050,1  TMI [Start] Do not update PO1,...,4 fields if Sales order type is selected.
              IF lcPoTyp = "O"
                IF llRpPrPric
                  REPLACE Price3 WITH IIF(lcRpPrTyp='O',ORDLINE.PRICE,; 
                                      IIF(lcRpPrTyp='R',STYLE.NSUGRETPRI , STYLE.PRICE&lcRpPrTyp ))
                ENDIF
              ELSE
                *C131050,1  TMI [End  ] 
                IF lcPoTyp <> "S"
                  REPLACE Po3        WITH Posln.Po
                ENDIF
                *C131050,1  TMI [Start] 
              ENDIF
              *C131050,1  TMI [End  ] 

            CASE lnlbCount =4          
              REPLACE cStyMaj4     WITH lcStyMaj,;
                      cStyDesc4    WITH lcStyDesc,; 
                      CColrDsc4    WITH lcColrDsc,;
                      SclDim4      WITH lcSclDim,;
                      SclSz4       WITH lcSclSz,;
                      cStyBarCd4   WITH lcStyBarCd
              *C131050,1  TMI [Start] Do not update PO1,...,4 fields if Sales order type is selected.
              IF lcPoTyp = "O"
                IF llRpPrPric
                  REPLACE Price4 WITH IIF(lcRpPrTyp='O',ORDLINE.PRICE,; 
                                      IIF(lcRpPrTyp='R',STYLE.NSUGRETPRI , STYLE.PRICE&lcRpPrTyp ))
                ENDIF
              ELSE
                *C131050,1  TMI [End  ] 
                IF lcPoTyp <> "S"
                  REPLACE Po4        WITH Posln.Po
                ENDIF
                *C131050,1  TMI [Start] 
              ENDIF
              *C131050,1  TMI [End  ] 
                      
          ENDCASE
          lnlbCount = lnlbCount  + 1
          IF lnlbCount = 5
            lnlbCount = 1
          ENDIF
      ENDFOR
    ENDIF
  ENDFOR  
ENDSCAN
WAIT CLEAR

SELECT (lcTmpBars)
SCAN
  =EAN13(cStyBarCd1)
ENDSCAN

GO TOP
IF EOF()
  *--No receiving lines for the selected purchase orders . [Ok]
  IF lcRpBasdOn = 'R'
    =gfModalGen('TRM34006B34000','DIALOG','the selected'+'|'+IIF(lcPoTyp='M', 'cutting tickets' , IIF(lcPoTyp='P','purchase orders','inter company purchase orders')))
  ELSE
    =gfModalGen('TRM00052B00000','DIALOG') 
  ENDIF
ELSE
  DO gfDispRe WITH EVAL('lcRpForm')  
ENDIF
USE IN (lcTmpBars)
ERASE (gcWorkDir+lcTmpBars+'.DBF')
ERASE (gcWorkDir+lcTmpBars+'.CDX')

*C131050,1  TMI [Start] close the file lcOrdCnLn 
IF USED(lcOrdCnLn)
  USE IN &lcOrdCnLn
ENDIF
ERASE (gcWorkDir+lcOrdCnLn+'.DBF')
ERASE (gcWorkDir+lcOrdCnLn+'.CDX')
*C131050,1  TMI [End  ] 

IF lcPoTyp = "M"
  SELECT CutTktL
  SET RELATION TO
ELSE
  SELECT PosLn
  SET RELATION TO
ENDIF

SELECT (lcHeader)

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Timour A. K.
*! Date      : 05/25/98
*! Purpose   : Optional Grid When Function.
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

IF lcPoTyp = "M"
  SELECT CUTTKTH
  SET ORDER TO TAG CUTTKTH
  SELECT CUTTKTL
  SET ORDER TO TAG CUTTKTL
ELSE
  SELECT POSHDR
  SET ORDER TO TAG POSHDR
  SELECT POSLN
  SET ORDER TO TAG POSLN
ENDIF

RETURN

*!*************************************************************
*! Name      : lfvTranNo
*! Developer : Reham Al-Allamy
*! Date      : 11/16/1999
*! Purpose   : Valid function of the transaction #
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*!          Calls: None
*!*************************************************************
*! Example   : =lfvTranNo()
*!*************************************************************
*
Function lfvTranNo
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

IF lcPoTyp = "M"
  *-- Browse the cutting tickets from the cutting ticket file.
  IF !EMPTY(lcObj) AND !SEEK(lcObj , 'CUTTKTH')))
    SELECT CUTTKTH
    DIMENSION laTemp[1]
    laTemp = ''
    lcBrFields = "Cuttkt :R:H='CutTkt#':8,Style :R:20, "+;
                 "Status :R:H='S':2 , Entered :R:12 , "+;
                 "Complete :R:12 , Pcs_bud :R:H='Tot.Qty':9 ,"+;
                 "Pcs_rec :R:H='Receive':9 , Pcs_opn :R:H='Open':9"
    
    lcFile_Ttl = "Cutting Tickets"
    lcBrowCond = [FOR Pcs_rec>0]
    = gfBrows(lcBrowCond , 'CutTkt' , 'laTemp')
    
    IF !EMPTY(laTemp[1])
      lcObj = laTemp[1]
    ELSE
      lcObj = ""
    ENDIF
  ENDIF
  &lcVar = lcObj
ELSE
  *-- Browse the purchase orders from the purchase orders file.
  IF !EMPTY(lcObj) AND !SEEK(lcObj , 'POSHDR')))
    SELECT POSHDR
    DIMENSION laTemp[1]
    laTemp = ''
    lcBrFields = "PO :R :H='P/O #':8,Status :R :H='S':2, "+;
               "Vendor :R :H=IIF(lcPoTyp='N','Source Loc.','Vendor'):12 , "+;
               "Entered :R :H='Entered':12,Complete :R :H='Complete':12 , "+;
               "nStyOrder :R :H='Tot.Qty.':9,POTotal :R :H='Amount':12 , "+;
               "Receive :R :H='Receive':9,Open :R :H='Open':9"
    
    lcFile_Ttl = "Purchse orders"
    lcBrowCond = [lcPoTyp FOR Receive>0]
    = gfBrows(lcBrowCond,'PO','laTemp')
      
    IF !EMPTY(laTemp[1])
      lcObj = laTemp[1]
    ELSE
      lcObj = ""
    ENDIF
  ENDIF
  &lcVar = lcObj
ENDIF

*!*************************************************************
*! Name      : lfvTranTyp
*! Developer : Reham Al-Allamy
*! Date      : 11/17/1999
*! Purpose   : Valid function of the transaction type
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*!          Calls: None
*!*************************************************************
*! Example   : =lfvTranTyp()
*!*************************************************************
*
FUNCTION lfvTranTyp

CLEAR READ

*!*************************************************************
*! Name      : lfvPRecBdg
*! Developer : Khalid Mohi El-Din M.
*! Date      : 02/05/2001
*! Purpose   : Valid function of the print based on option
*!*************************************************************
*E301545,1 KHM 02/05/2001 
*!*************************************************************

FUNCTION lfvPRecBdg
CLEAR READ
*!*************************************************************
*! Name      : lfsrStyle
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/29/2002
*! Purpose   : Set and Rest functions for style filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrStyle()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfsrStyle
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  GO TOP IN STYLE

ELSE  && Reset code

  SELECT STYLE
  SET ORDER TO TAG STYLE

ENDIF

*--End of lfsrStyle.

*!**************************************************************************
*! Name      : lfNonMaj
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : To get the style nonmajor segment structure
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfItemMask()
*!**************************************************************************
*! Example     : = lfNonMaj()
*!**************************************************************************
*
FUNCTION lfNonMaj
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'  
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT  
    ELSE      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.      
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- End of lfNonMaj.

*************************************************mhmhmhmhmhmhmhmmh
*-----------------------------------------------------------
* Conversion VB --> VFP : Emile MAITREJEAN www.emsystems.be
*-----------------------------------------------------------
* Entrée : lcChaine
* 			
* Retour : * w_ean13 qui, affichée avec la police EAN13.TTF, donne le code barre
*          * une chaine vide si paramètre fourni incorrect
FUNCTION EAN13
PARAMETER lcChaine
ean13 = ""
* Vérifier qu'il y a 12 caractères
lcChaine = LEFT(lcChaine,12)
IF LEN(lcChaine) = 12 THEN
	* Et que ce sont bien des chiffres
	FOR lni = 1 TO 12
		IF ASC(SUBSTR(lcChaine, lni, 1)) < 48 OR ASC(SUBSTR(lcChaine, lni, 1)) > 57 THEN
			lni = 0
			EXIT
		ENDIF
	ENDFOR
	IF lni = 13 THEN
		* Calcul de la clé de contrôle
		lnCheckSum = 0
		FOR lni = 2 TO 12 STEP 2
			lnCheckSum = lnCheckSum + VAL(SUBSTR(lcChaine, lni, 1))
		ENDFOR
		lnCheckSum = lnCheckSum * 3
		FOR lni = 1 TO 11 STEP 2
			lnCheckSum = lnCheckSum + VAL(SUBSTR(lcChaine, lni, 1))
		ENDFOR
		lnM = (VAL(SUBSTR(STR(lnCheckSum,3,0),1,2))+1)*10
		lnDigit = lnM-lnCheckSum
		lcChaine = ALLTRIM(lcChaine)+STR(lnDigit,1,0)
		* Le premier chiffre est pris tel quel.
		lcCodeBarre = SUBSTR(lcChaine, 1, 1)
		*
		lnFirst = VAL(SUBSTR(lcChaine, 1, 1))
		FOR ix_chiffre = 2 TO 7
			tableA = .F.
			DO CASE
				CASE ix_chiffre = 2
					tableA = .T.
				CASE ix_chiffre = 3
					IF lnFirst >=  0 AND lnFirst <= 3
						tableA = .T.
					ENDIF
				CASE ix_chiffre = 4
					IF lnFirst =  0 OR lnFirst = 4 OR lnFirst =  7 OR lnFirst =  8
						tableA = .T.
					ENDIF
				CASE ix_chiffre = 5
					IF lnFirst = 0 OR lnFirst =  1 OR lnFirst =  4 OR lnFirst =  5 OR lnFirst =  9
						tableA = .T.
					ENDIF
				CASE ix_chiffre = 6
					IF lnFirst = 0 OR lnFirst =  2 OR lnFirst =  5 OR lnFirst =  6 OR lnFirst =  7
						tableA  = .T.
					ENDIF
				CASE ix_chiffre = 7
					IF lnFirst = 0 OR lnFirst =  3 OR lnFirst =  6 OR lnFirst =  8 OR lnFirst =  9
						tableA = .T.
					ENDIF
			ENDCASE
			IF tableA THEN
				lcCodeBarre = lcCodeBarre+CHR(65 + VAL(SUBSTR(lcChaine, ix_chiffre, 1)))
			ELSE
				lcCodeBarre = lcCodeBarre+CHR(75 + VAL(SUBSTR(lcChaine, ix_chiffre, 1)))
			ENDIF
		ENDFOR
		*
		lcCodeBarre = lcCodeBarre+"*"   && Ajout séparateur central
		FOR lni = 8 TO 13
			lcCodeBarre = lcCodeBarre+CHR(97 + VAL(SUBSTR(lcChaine, lni, 1)))
		ENDFOR
		lcCodeBarre = lcCodeBarre+"+"   && Ajout de la marque de fin
		ean13 = lcCodeBarre
	ENDIF
ENDIF
w_ean13 = ean13
RETURN  w_ean13


*!**************************************************************************
*! Name      : lfSROrder
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 19/06/2000
*! Purpose   : Go top in the ORDHDR IN RANGE
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
*! Example   : =lfSROrder()
*!**************************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    GO TOP
  CASE lcParm = 'R'
    SELECT ORDHDR
ENDCASE
*-- end of lfSROrder.

*:**************************************************************************
*:* Name        : lfvPrnPric
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/13/2006
*:* Purpose     : valid fn for print price
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*C131050,1  
FUNCTION lfvPrnPric
CLEAR READ
*-- end of lfvPrnPric.