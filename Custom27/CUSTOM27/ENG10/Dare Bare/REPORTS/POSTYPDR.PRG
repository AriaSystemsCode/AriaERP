*:***************************************************************************
*: Program file  : POSTYPDR
*: Program desc. : Print PO for Dare to Bare Ltd
*: For Report    : POSTYPDR.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Date          : 10/23/2002
*: Developer     : AHMED MAHER (AMH)
*: Reference     : C200421,1
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfGetAdr()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:Modifications:
*:B606891,1 AMH 01/19/2003 Fix the bug of duplicate rolls qty.
*:C200503,1 KHM 03/17/2003 Adding the note pad of the primary fabric
*:B037176,1 ABD 10/09/2003 Problem occures when user print the form after user open the 
*:B037176,1 ABD            PO and the PO cost sheet from the Generate PO from sales order screen.
*:C037438,1 TMI 02/15/2004 change the PO to treate the case of 10-Sz Scale styles
*:B124597,1 NNA 11/04/2004 Fix bug that the fabric is printed twice in both form of the po that if
*:B124597,1 NNA            you have a style with 2 colours and both colour have the the same fabric
*:B124597,1 NNA            and colour
*:B608248,1 NNA 09/01/2007 (T20070418.0014) Fix Bug that if there are two styles on the Po and 2 colours for each style. 
*:B608248,1 NNA            The fabric details dosen't print , it must prints a keep a record for every fabric/colour Per style
*:***************************************************************************

SELECT PosHdr
SET SKIP TO
SELECT PosLn
SET SKIP TO
SET ORDER TO PosLn
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SCALE
SET RELATION OFF INTO OBJLINK

*-- Creating the temporary file
IF USED(lcTmpPoLns)
  USE IN (lcTmpPoLns)
  USE (gcWorkDir+lcTmpPoLns) IN 0 ORDER TAG (lcTmpPoLns) EXCLUSIVE
  SELECT (lcTmpPoLns)
  ZAP
ELSE
  SELECT PosLn
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  
  *B606891,1 AMH Add new field to save the style for which rolls calculated [Start]
  *DIMENSION laFileStru[lnFileStru+24,4]
  DIMENSION laFileStru[lnFileStru+25,4]
  *B606891,1 AMH [End]

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CCUSTNAME'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 30
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CORDER'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CUSTPO'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 15
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CDEPTDESC'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 30
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CFABDESC'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 20
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CFABPAT'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 10
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CFABWIDTH'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CFABCONT'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 60
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CCOLORDESC'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 30
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CLINENO'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 19
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CTYPE'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 1
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CITEM'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 7
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CICLR'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CITEMDESC'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 30
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CITEMDESC1'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 20
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NUNTQTY'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 7
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'MPONOTE'
  laFileStru[lnFileStru,2] = 'M'
  laFileStru[lnFileStru,3] = 10
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CLOCATION'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 10
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CSUPCOL'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 20
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NREQQTY'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 12
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NTOTQTY'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 11
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CROLLID'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 20
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'LFIRST'
  laFileStru[lnFileStru,2] = 'L'
  laFileStru[lnFileStru,3] = 1
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CNEWGROUP'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 1
  laFileStru[lnFileStru,4] = 0
  
  *B606891,1 AMH Add new field to save the style for which rolls calculated [Start]
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'CSTYROLL'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 19
  laFileStru[lnFileStru,4] = 0
  *B606891,1 AMH [End]
  
  *C037438,1  TMI [Start] Add new fields used for the case style with 10 colors ;
  *                       ( two styles with same color one 8-Sz Scale , other 2-Sz Scale )
  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'QTY9'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0

  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'QTY10'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0

  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'Sz1'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'Sz2'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'Sz3'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'Sz4'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'Sz5'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'Sz6'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'Sz7'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'Sz8'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'Sz9'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = ALEN(laFileStru,1) + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'Sz10'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0
  *C037438,1  TMI [End  ] 

  =gfCrtTmp(lcTmpPolns,@laFileStru,'po+ctype+style+citem+cIClr+cWareCode+cRollId',lcTmpPolns)
ENDIF

*--- Opend needed files.
PRIVATE llOpenFab,llOpenCut,llOpenOrdH,llOpenOrdL,llOpenCtkt,llOpenBom,llOpenCust,llOpenDept,;
        llOpenWLoc,llOpenCost,llOpenRoll,llOpenBmLn
STORE .F. TO llOpenFab,llOpenCut,llOpenOrdH,llOpenOrdL,llOpenCtkt,llOpenBom,llOpenCust,;
             llOpenDept,llOpenWLoc,llOpenCost,llOpenRoll,llOpenBmLn
IF !USED('FABRIC')
  =gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
  llOpenFab = .T.
ENDIF
IF !USED('CUTPICK')
  =gfOpenFile(gcDataDir+'CUTPICK','CUTPKORD','SH')
  llOpenCut = .T.
ENDIF
IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
  llOpenOrdH = .T.
ENDIF
IF !USED('ORDLINE')
  =gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
  llOpenOrdL = .T.
ENDIF
IF !USED('CTKTBOM')
  =gfOpenFile(gcDataDir+'CTKTBOM','CTKTBOM','SH')
  llOpenCtkt = .T.
ENDIF
IF !USED('BOM')
  =gfOpenFile(gcDataDir+'BOM','BOMITEM','SH')
  llOpenBom = .T.
  *B037176,1 ABD - [Begin]
ELSE
    SELECT BOM
    SET ORDER TO BOMITEM
    *B037176,1 ABD - [End]
ENDIF
IF !USED('CUSTOMER')
  =gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
  llOpenCust = .T.
ENDIF
IF !USED('CUSTDEPT')
  =gfOpenFile(gcDataDir+'CUSTDEPT','CUSTDEPT','SH')
  llOpenDept = .T.
ENDIF
IF !USED('WHSLOC')
  =gfOpenFile(gcDataDir+'WHSLOC','WHSLOCST','SH')
  llOpenWLoc = .T.
ENDIF
SET ORDER TO WHSLOCST DESCENDING IN WHSLOC
IF !USED('BOMCOST')
  =gfOpenFile(gcDataDir+'BOMCOST','BOMCSTKT','SH')
  llOpenCost = .T.
ENDIF
IF !USED('ROLLS')
  =gfOpenFile(gcDataDir+'ROLLS','ROLLITEM','SH')
  llOpenRoll = .T.
ENDIF
IF !USED('BOMLINE')
  =gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')
  llOpenBmLn = .T.
ENDIF

*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
=lfGetColor()

SELECT PosHdr
SCAN FOR &lcRpExp
  IF SEEK(cStyType+Po,'PosLn')
    
    *-- Add the PO lines (type = 1)
    SELECT PosLn
    lnLastCost = POSLN.NCOST1
    lcLastSty  = POSLN.STYLE
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
                    PosHdr.cStyType+PosHdr.PO FOR TranCd = '1' AND TotQty <>0
      *C037438,1  TMI [Start] Update the variable 'lcLastSty' if the PO has two styles
      lcLastSty  = POSLN.STYLE
      *C037438,1  TMI [End  ] 
      lnLastLine = POSLN.LINENO
      IF SEEK(PO+'1'+STYLE,lcTmpPolns)
        SELECT (lcTmpPolns)
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          REPLACE ('QTY'+lcI) WITH EVALUATE('POSLN.QTY'+lcI)
          REPLACE TOTQTY      WITH TOTQTY+EVALUATE('POSLN.QTY'+lcI)
        ENDFOR
      ELSE
        SCATTER MEMVAR MEMO
        INSERT INTO (lcTmpPolns) FROM MEMVAR
        SELECT (lcTmpPolns)
        =lfReplace()
        REPLACE CTYPE      WITH '1',;
                CCOLORDESC WITH gfCodDes(SUBSTR(m.Style,lnClrSrt,lnClrEnd),'COLOR'),;
                CLINENO    WITH ORDLINE.ITEM_NO
      ENDIF
      *-- Add the Fabric section (type = 2)
      IF SEEK('I1'+POSHDR.PO+STR(POSLN.LINENO,6)+'2'+POSLN.STYLE+SPACE(6),'BOMLINE')
        SELECT BOMLINE
        SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                        'I1'+POSHDR.PO+STR(POSLN.LINENO,6)+'2'+POSLN.STYLE+SPACE(6)
          *C037438,1  TMI [Start] Locate pointer to the correct record in style file
          =SEEK(BOMLINE.STYLE,'STYLE')
          *C037438,1  TMI [End  ] 
          IF SEEK('2'+ITEM+ICLR+STYLE.CSTYMAJOR+STYLE,'BOM')
            SELECT BOM
            STORE 0 TO lnBomQty,lnMaxQty,lnCount
            SCAN REST WHILE typ+item+iclr+citmmajor+citmmask =;
                            '2'+BOMLINE.ITEM+BOMLINE.ICLR+STYLE.CSTYMAJOR+BOMLINE.STYLE
              lnCount = lnCount + 1
              IF NBOMTOTQTY > lnMaxQty
                lnMaxQty = NBOMTOTQTY
              ENDIF
              lnBomQty = lnBomQty + NBOMTOTQTY
              SELECT (lcTmpPolns)
                        
              *B124597,1 NNA 11/04/2004 (Begin) Check if POSHDR.PO if Found with Type '2'
              STORE 0 TO lnLineNo
              IF !SEEK(POSHDR.PO+'2',lcTmpPolns)
                APPEND BLANK
                llFound = .T.	&& to not Append Blank Twice
              ELSE
                llFound = .F.
                *-- Scan for PO + cType For the same Fabric and color
                SCAN REST WHILE &lcTmpPoLns..PO+CTYPE = POSHDR.PO+'2' FOR &lcTmpPoLns..CITEM =;
                          PADR(BOMLINE.ITEM,7) AND &lcTmpPoLns..CICLR = BOMLINE.ICLR
                  llFound = .T.	
                  *-- Hold the Record NO. to go to it after exiting from the Scan because at the end
                  *-- of Scan Pointer is moving to the next record
                  lnLineNo = RECNO()	
                ENDSCAN
                  IF BETWEEN(lnLineNo,1,RECCOUNT(lcTmpPolns))
                    GOTO lnLineNo IN &lcTmpPolns
                  ENDIF
              ENDIF
              IF !llFound 	&& if there is no record has the same values [Append Blank] Else Replace on it
              *B124597,1 NNA (End)

                APPEND BLANK
              *B124597,1 NNA (Begin)
              ENDIF
              *B124597,1 NNA (End)

              =lfReplace()
              =SEEK(PADR(BOM.ITEM,7)+BOM.ICLR,'FABRIC')
              REPLACE PO         WITH POSHDR.PO,;
                      CSTYTYPE   WITH POSHDR.CSTYTYPE,;
                      STYLE      WITH lcLastSty,;
                      NCOST1     WITH lnLastCost,;
                      CTYPE      WITH '2',;
                      CITEM      WITH BOM.ITEM,;
                      CICLR      WITH BOM.ICLR,;
                      CITEMDESC  WITH BOM.CBOMDESC,;
                      CITEMDESC1 WITH FABRIC.DESC,;
                      CCOLORDESC WITH gfCodDes(BOM.ICLR,'COLOR'),;
                      NUNTQTY    WITH BOM.NBOMTOTQTY
            *C037438,3  TMI [Start] Update the "VENDOR" field            
            REPLACE  VENDOR WITH FABRIC.VENDOR
            *C037438,3  TMI [End  ]                       
            ENDSCAN
            IF lnBomQty <> BOMLINE.UNITQTY .AND.;
               SEEK(POSHDR.PO+'2'+lcLastSty+PADR(BOMLINE.ITEM,7)+BOMLINE.ICLR,lcTmpPolns)
              SELECT (lcTmpPolns)
              LOCATE REST WHILE PO+CTYPE+STYLE+CITEM+CICLR =;
                                POSHDR.PO+'2'+lcLastSty+PADR(BOMLINE.ITEM,7)+BOMLINE.ICLR;
                            FOR NUNTQTY >= lnMaxQty
              IF FOUND()
                REPLACE NUNTQTY WITH NUNTQTY + BOMLINE.UNITQTY - lnBomQty
              ENDIF
            ENDIF
          ELSE
            SELECT (lcTmpPolns)
              
              *B124597,1 NNA 11/04/2004 (Begin) Check if POSHDR.PO if Found with Type '2'
              STORE 0 TO lnLineNo
              IF !SEEK(POSHDR.PO+'2',lcTmpPolns)
                APPEND BLANK
                llFound = .T.		&& to not Append Blank Twice
              ELSE
                llFound = .F.
                *-- Scan for PO + cType For the same Fabric and color
                SCAN REST WHILE &lcTmpPoLns..PO+CTYPE = POSHDR.PO+'2' FOR &lcTmpPoLns..CITEM =;
                          PADR(BOMLINE.ITEM,7) AND &lcTmpPoLns..CICLR = BOMLINE.ICLR
                  llFound = .T.
                  *-- Hold the Record NO. to go to it after exiting from the Scan because at the end
                  *-- of Scan Pointer is moving to the next record
                  lnLineNo = RECNO()
                ENDSCAN
                  IF BETWEEN(lnLineNo,1,RECCOUNT(lcTmpPolns))
                    GOTO lnLineNo IN &lcTmpPolns
                  ENDIF
              ENDIF
              IF !llFound
              *B124597,1 NNA (End)

                APPEND BLANK
              *B124597,1 NNA (Begin)
              ENDIF
              *B124597,1 NNA (End)

            =lfReplace()
            =SEEK(PADR(BOMLINE.ITEM,7)+BOMLINE.ICLR,'FABRIC')
            =SEEK('I'+POSHDR.PO+BOMLINE.CBOMTYP+BOMLINE.ITEM+BOMLINE.ICLR,'CTKTBOM')
            REPLACE PO         WITH POSHDR.PO,;
                    CSTYTYPE   WITH POSHDR.CSTYTYPE,;
                    STYLE      WITH lcLastSty,;
                    NCOST1     WITH lnLastCost,;
                    CTYPE      WITH '2',;
                    CITEM      WITH BOMLINE.ITEM,;
                    CICLR      WITH BOMLINE.ICLR,;
                    CITEMDESC  WITH CTKTBOM.DESC,;
                    CITEMDESC1 WITH FABRIC.DESC,;
                    CCOLORDESC WITH gfCodDes(BOMLINE.ICLR,'COLOR'),;
                    NUNTQTY    WITH BOMLINE.UNITQTY
            *C037438,3  TMI [Start] Update the "VENDOR" field            
            REPLACE  VENDOR WITH FABRIC.VENDOR
            *C037438,3  TMI [End  ]                       
          ENDIF
          *-- Add the Piece Material Requirements section (type = 5)
          IF FABRIC.LTRKROLLS
            =lfAddType5()
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN
    
    *-- Add the Special instructions section (type = 3)
    
    *C200503,1 KHM 03/17/2003 (Begin) Commented out in order to add the printing of fabric notepad
    *IF llRpPrtPn .AND. SEEK('P'+POSHDR.PO,'NOTEPAD')
    *  SELECT (lcTmpPolns)
    *  APPEND BLANK
    *  =lfReplace()
    *  REPLACE PO         WITH POSHDR.PO,;
              CSTYTYPE   WITH POSHDR.CSTYTYPE,;
              STYLE      WITH lcLastSty,;
              NCOST1     WITH lnLastCost,;
              CTYPE      WITH '3',;
              MPONOTE    WITH NotePad.mNotes,;
              CNEWGROUP  WITH '3'
    *ENDIF
    IF llRpPrtPn 
      IF SEEK('P'+POSHDR.PO,'NOTEPAD')
        SELECT (lcTmpPolns)
        APPEND BLANK
        =lfReplace()
        REPLACE PO         WITH POSHDR.PO,;
                CSTYTYPE   WITH POSHDR.CSTYTYPE,;
                STYLE      WITH lcLastSty,;
                NCOST1     WITH lnLastCost,;
                CTYPE      WITH '3',;
                MPONOTE    WITH NotePad.mNotes,;
                CNEWGROUP  WITH '3'
      
        IF SEEK('G'+Style.Fabric,'NOTEPAD')
          REPLACE mPoNote WITH mPoNote+CHR(13)+CHR(13)+NotePad.mNotes
        ENDIF
      ELSE
        IF SEEK('G'+Style.Fabric,'NOTEPAD')
          SELECT (lcTmpPolns)
          APPEND BLANK
          =lfReplace()
          REPLACE PO         WITH POSHDR.PO,;
                  CSTYTYPE   WITH POSHDR.CSTYTYPE,;
                  STYLE      WITH lcLastSty,;
                  NCOST1     WITH lnLastCost,;
                  CTYPE      WITH '3',;
                  MPONOTE    WITH NotePad.mNotes,;
                  CNEWGROUP  WITH '3'
        ENDIF          
      ENDIF
    ENDIF
    *C200503,1 KHM 03/17/2003 (End)

    *-- Add the Trim section (type = 4)
    IF SEEK('I'+POSHDR.PO+'3','CTKTBOM')
      SELECT CTKTBOM
      SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = 'I'+POSHDR.PO+'3'
        IF SEEK('3I'+POSHDR.PO+ITEM+ICLR+MFGCODE,'BOMCOST')
          SELECT BOMCOST
          SCAN REST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          '3I'+POSHDR.PO+CTKTBOM.ITEM+CTKTBOM.ICLR+CTKTBOM.MFGCODE
            IF SEEK(POSHDR.PO+'4'+lcLastSty+ITEM+ICLR+CWARECODE,lcTmpPolns)
              SELECT (lcTmpPolns)
              REPLACE NTOTQTY WITH NTOTQTY + BOMCOST.NTOTQTY
            ELSE
              SELECT (lcTmpPolns)
              APPEND BLANK
              =lfReplace()
              =SEEK(PADR(CTKTBOM.ITEM,7)+CTKTBOM.ICLR,'FABRIC')
              =SEEK(CTKTBOM.ITEM+CTKTBOM.ICLR+BOMCOST.CWARECODE,'WHSLOC')
              REPLACE PO         WITH POSHDR.PO,;
                      CSTYTYPE   WITH POSHDR.CSTYTYPE,;
                      STYLE      WITH lcLastSty,;
                      NCOST1     WITH lnLastCost,;
                      CTYPE      WITH '4',;
                      CITEM      WITH CTKTBOM.ITEM,;
                      CICLR      WITH CTKTBOM.ICLR,;
                      CITEMDESC  WITH FABRIC.PATTERN,;
                      CITEMDESC1 WITH FABRIC.DESC,;
                      CLOCATION  WITH WHSLOC.CLOCATION,;
                      CCOLORDESC WITH gfCodDes(CTKTBOM.ICLR,'COLOR'),;
                      CSUPCOL    WITH FABRIC.CSUPCOL,;
                      CWARECODE  WITH BOMCOST.CWARECODE,;
                      NUNTQTY    WITH CTKTBOM.UNTQTY,;
                      NREQQTY    WITH CTKTBOM.REQ_QTY,;
                      NTOTQTY    WITH BOMCOST.NTOTQTY,;
                      CNEWGROUP  WITH '4'
            ENDIF
          ENDSCAN
        ELSE
          SELECT (lcTmpPolns)
          APPEND BLANK
          =lfReplace()
          =SEEK(PADR(CTKTBOM.ITEM,7)+CTKTBOM.ICLR,'FABRIC')
          =SEEK(CTKTBOM.ITEM+CTKTBOM.ICLR,'WHSLOC')
          REPLACE PO         WITH POSHDR.PO,;
                  CSTYTYPE   WITH POSHDR.CSTYTYPE,;
                  STYLE      WITH lcLastSty,;
                  NCOST1     WITH lnLastCost,;
                  CTYPE      WITH '4',;
                  CITEM      WITH CTKTBOM.ITEM,;
                  CICLR      WITH CTKTBOM.ICLR,;
                  CITEMDESC  WITH FABRIC.PATTERN,;
                  CITEMDESC1 WITH FABRIC.DESC,;
                  CLOCATION  WITH WHSLOC.CLOCATION,;
                  CCOLORDESC WITH gfCodDes(CTKTBOM.ICLR,'COLOR'),;
                  CSUPCOL    WITH FABRIC.CSUPCOL,;
                  NUNTQTY    WITH CTKTBOM.UNTQTY,;
                  NREQQTY    WITH CTKTBOM.REQ_QTY,;
                  CNEWGROUP  WITH '4'
        ENDIF
        *-- Add the Piece Material Requirements section (type = 5)
        IF FABRIC.LTRKROLLS
          =lfAddType5()
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
ENDSCAN

IF llOpenFab
  USE IN FABRIC
ENDIF
IF llOpenCut
  USE IN CUTPICK
ENDIF
IF llOpenOrdH
  USE IN ORDHDR
ENDIF
IF llOpenOrdL
  USE IN ORDLINE
ENDIF
IF llOpenCtkt
  USE IN CTKTBOM
ENDIF
IF llOpenBom
  USE IN BOM
ENDIF
IF llOpenCust
  USE IN CUSTOMER
ENDIF
IF llOpenDept
  USE IN CUSTDEPT
ENDIF
IF llOpenWLoc
  USE IN WHSLOC
ENDIF
IF llOpenCost
  USE IN BOMCOST
ENDIF
IF llOpenRoll
  USE IN ROLLS
ENDIF
IF llOpenBmLn
  USE IN BOMLINE
ENDIF

SELECT PosLn
SET RELATION TO
SELECT PosHdr
SET RELATION TO

SELECT (lcTmpPolns)
GOTO TOP
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE

SELECT PosHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE

SELECT (lcTmpPolns)
lcRpExp = ".T."
*C037438,1  TMI [Start] Check if this PO has a style with 9-10 Sz-Scale, if so copy the second line to the first one
=lf10SzScl()
*C037438,1  TMI [End  ] 

*-- End Of Option Program.

*!*************************************************************
*! Name      : lfReplace
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/23/2002
*! Purpose   : Fill the header fields
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfReplace()
*!*************************************************************
*
FUNCTION lfReplace

=SEEK('2'+POSHDR.PO+STR(lnLastLine,6),'CUTPICK')
=SEEK('O'+CUTPICK.ORDER,'ORDHDR')
IF EMPTY(ORDHDR.STORE)
  =SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER')
ELSE
  =SEEK('S'+ORDHDR.ACCOUNT+ORDHDR.STORE,'CUSTOMER')
ENDIF
=SEEK(ORDHDR.ACCOUNT+ORDHDR.DEPT,'CUSTDEPT')
=SEEK('O'+CUTPICK.ORDER+CUTPICK.CORDLINE,'ORDLINE')
=SEEK(lcLastSty,'STYLE')
=SEEK(STYLE.FABRIC,'FABRIC')
REPLACE CCUSTNAME WITH CUSTOMER.BTNAME,;
        CORDER    WITH CUTPICK.ORDER,;
        CUSTPO    WITH ORDHDR.CUSTPO,;
        CDEPTDESC WITH CUSTDEPT.CDEPTDESC,;
        CFABDESC  WITH FABRIC.DESC,;
        CFABPAT   WITH FABRIC.PATTERN,;
        CFABWIDTH WITH FABRIC.WIDTH,;
        CFABCONT  WITH FABRIC.CONTENT
*-- end of lfReplace.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/23/2002
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
    EXIT
  ENDIF
ENDFOR
RETURN

*!*************************************************************
*! Name      : lfAddType5
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/27/2002
*! Purpose   : Add the Piece Material Requirements section (type = 5)
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfAddType5()
*!*************************************************************
FUNCTION lfAddType5

IF SEEK(PADR(BOMLINE.ITEM,7)+BOMLINE.ICLR,'ROLLS')
  SELECT ROLLS
  LOCATE REST WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession=;
                    PADR(BOMLINE.ITEM,7)+BOMLINE.ICLR FOR CTKTNO = POSHDR.PO
  IF FOUND()
    SCAN REST WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession=;
                    PADR(BOMLINE.ITEM,7)+BOMLINE.ICLR FOR CTKTNO = POSHDR.PO
      IF SEEK(POSHDR.PO+'5'+lcLastSty+CROLLITEM+COLOR+CWARECODE+CROLLID,lcTmpPolns)
        SELECT (lcTmpPolns)
        
        *B606891,1 AMH Acumelate rolls qty only if the same style [Start]
        *REPLACE NREQQTY WITH NREQQTY + ROLLS.NQTY
        REPLACE NREQQTY WITH NREQQTY + IIF(CSTYROLL==BOMLINE.STYLE,ROLLS.NQTY,0)
        *B606891,1 AMH [End]
        
      ELSE
        SELECT (lcTmpPolns)
        llFirst = !SEEK(POSHDR.PO+'5'+lcLastSty+ROLLS.CROLLITEM+ROLLS.COLOR+ROLLS.CWARECODE)

        *B124597,1 NNA 11/04/2004 (Begin) Check if POSHDR.PO if Found with Type '5'
        STORE 0 TO lnLineNo
        IF !SEEK(POSHDR.PO+'5',lcTmpPolns)
          APPEND BLANK
          llFound = .T.		&& to not Append Blank Twice
        ELSE
          llFound = .F.
          *-- Scan for PO + cType For the same Fabric,color and RollID
          SCAN REST WHILE &lcTmpPoLns..PO+CTYPE = POSHDR.PO+'5' FOR &lcTmpPoLns..CITEM =;
                          PADR(BOMLINE.ITEM,7) AND &lcTmpPoLns..CICLR = BOMLINE.ICLR AND;
                          &lcTmpPoLns..cRollID = ROLLS.cRollID
            llFound = .T.
            *-- Hold the Record NO. to go to it after exiting from the Scan because at the end
            *-- of Scan Pointer is moving to the next record
            lnLineNo = RECNO()
          ENDSCAN
          IF BETWEEN(lnLineNo,1,RECCOUNT(lcTmpPolns))
            GOTO lnLineNo IN &lcTmpPolns
          ENDIF
        ENDIF
        IF !llFound
        *B124597,1 NNA (End)
      
          APPEND BLANK
     
        *B124597,1 NNA (Begin)
        ENDIF
        *B124597,1 NNA (End)


        =lfReplace()
        =SEEK(PADR(BOMLINE.ITEM,7)+BOMLINE.ICLR,'FABRIC')
        REPLACE PO         WITH POSHDR.PO,;
                CSTYTYPE   WITH POSHDR.CSTYTYPE,;
                STYLE      WITH lcLastSty,;
                NCOST1     WITH lnLastCost,;
                CTYPE      WITH '5',;
                CITEM      WITH BOMLINE.ITEM,;
                CICLR      WITH BOMLINE.ICLR,;
                CITEMDESC1 WITH FABRIC.DESC,;
                CCOLORDESC WITH gfCodDes(BOMLINE.ICLR,'COLOR'),;
                CWARECODE  WITH ROLLS.CWARECODE,;
                CRSESSION  WITH ROLLS.CRSESSION,;
                CROLLID    WITH ROLLS.CROLLID,;
                NREQQTY    WITH ROLLS.NQTY,;
                LFIRST     WITH llFirst,;
                CNEWGROUP  WITH '5'
        
        *B606891,1 AMH Save the style for which rolls is calculated [Start]
        REPLACE CSTYROLL   WITH BOMLINE.STYLE
        *B606891,1 AMH [End]
        
      ENDIF
      IF NREQQTY = 0
        DELETE
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
*:**************************************************************************
*:* Name        : lf10SzScl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 15/02/2004
*:* Purpose     : Check if this PO has a style with 9-10 Sz-Scale, if so copy 
*:*             : the second line to the first one
*:***************************************************************************
*C037438,1  
FUNCTION lf10SzScl
PRIVATE lcPO
lnSlct = SELECT()

SELECT (lcTmpPolns)
LOCATE
IF EOF()
  SELECT (lnSlct)
  RETURN
ENDIF  

lcTmpCur = gfTempName()

*-Get po & color
SELECT DIST PO,SUBSTR(STYLE,lnClrSrt,lnClrEnd) AS CLR ;
      FROM (lcTmpPolns) ;
      WHERE CTYPE = '1' ;
      INTO TABLE (gcWorkDir+lcTmpCur)
INDEX ON PO+CLR TAG &lcTmpCur

IF RECCOUNT(lcTmpCur) = 0
  USE IN &lcTmpCur
  ERASE (gcWorkDir+lcTmpCur+'.DBF')
  ERASE (gcWorkDir+lcTmpCur+'.CDX')
  SELECT (lnSlct)
  RETURN
ENDIF


SELECT (lcTmpCur)
SCAN
  SELECT (lcTmpPolns)
  LOCATE  
  
  LOCATE FOR &lcTmpCur..PO+&lcTmpCur..CLR = &lcTmpPolns..PO+SUBSTR(&lcTmpPolns..STYLE,lnClrSrt,lnClrEnd) ;
           AND CTYPE = '1'
  IF FOUND()    
    *- Update the size-description fields - used instead Scale.Sz... fields
    REPLACE SZ1 WITH SCALE.SZ1 ;
            SZ2 WITH SCALE.SZ2 ;
            SZ3 WITH SCALE.SZ3 ;
            SZ4 WITH SCALE.SZ4 ;
            SZ5 WITH SCALE.SZ5 ;
            SZ6 WITH SCALE.SZ6 ;
            SZ7 WITH SCALE.SZ7 ;
            SZ8 WITH SCALE.SZ8  
    
    lnRecno = RECNO(lcTmpPolns)
    CONTINUE
    
    IF FOUND()
      M.SZ9   = SCALE.SZ1
      M.SZ10  = SCALE.SZ2
      M.QTY9  = QTY1
      M.QTY10 = QTY2    
      
      DELETE

      GOTO (lnRecno)
      REPLACE SZ9    WITH M.SZ9   ;
              SZ10   WITH M.SZ10  ;
              QTY9   WITH M.QTY9  ;
              QTY10  WITH M.QTY10 ;
              TOTQTY WITH TOTQTY+QTY9+QTY10

      *- Delete Fabrics with type '2' that cited more than once
      DIMENSION laItem[1,2]
      laItem[1] = ''
      lcPO = &lcTmpCur..PO
      SELECT DIST CITEM,CICLR,NUNTQTY FROM (lcTmpPolns) ;
        WHERE po+ctype+style+citem+cIClr+cWareCode+cRollId = lcPO+'2' ;
             .AND. !EMPTY(CITEM) ;          
        INTO ARRAY laItem        

      *B608248,1 NNA 09/01/2007 (Begin) Count the fabric/Colour Per Po to can remove the first Occurance of item-clr if there
      *B608248,1 NNA             is a repeating
      lcTmpFaCnt = gfTempName()
      SELECT PO,(CITEM+CICLR) as FABRIC,COUNT(CITEM+CICLR) AS CNT;
           FROM (lcTmpPolns) ;
           WHERE CTYPE = '2' ;
           GROUP BY PO,FABRIC;
           INTO TABLE (gcWorkDir+lcTmpFaCnt)
      INDEX ON PO+FABRIC TAG lcTmpFaCnt
      SELECT (lcTmpPolns)
      *B608248,1 NNA (End)
  
      *- Locate for 1st occurance of item-clr and delete it
      *ash1
      IF !empty(laItem)
        FOR lnK = 1 TO ALEN(laItem,1)
          =SEEK(&lcTmpCur..PO+'2',lcTmpPolns)
          LOCATE REST WHILE po+ctype+style+citem+cIClr+cWareCode+cRollId = &lcTmpCur..PO+'2' ;
                      FOR CITEM+CICLR = laItem[lnK,1]+laItem[lnK,2] .AND. NUNTQTY = laItem[lnK,3]

          *B608248,1 NNA 09/01/2007 (Begin) Delete Record if it repeated more than one time only
          *IF FOUND()
          IF FOUND() AND SEEK(&lcTmpCur..PO+&lcTmpPolns..CITEM+&lcTmpPolns..CICLR,lcTmpFaCnt) ;
                     AND &lcTmpFaCnt..CNT>1
          *B608248,1 NNA (End)

            DELETE

          ENDIF
        ENDFOR
      ENDIF
      *C037438,4  TMI [Start] Delete Fabrics with type '5' that cited more than once based on RollID
      DIMENSION laItem[1,4]
      laItem = ''
      SELECT CITEM,CICLR,CROLLID,COUNT(*) FROM (lcTmpPolns) ;
        WHERE po+ctype+style+citem+cIClr+cWareCode+cRollId = lcPO+'5' ;
             .AND. !EMPTY(CITEM) ;          
        GROUP BY CITEM,CICLR,CROLLID ;
        INTO ARRAY laItem        
      *- Locate for 1st occurance of item-clr and delete it
      *ash1
      IF !empty(laItem)
        FOR lnK = 1 TO ALEN(laItem,1)
          IF laItem[lnK,4] > 1
            =SEEK(&lcTmpCur..PO+'5',lcTmpPolns)
            LOCATE REST WHILE po+ctype+style+citem+cIClr+cWareCode+cRollId = &lcTmpCur..PO+'5' ;
                        FOR CITEM+CICLR+CROLLID = laItem[lnK,1]+laItem[lnK,2]+laItem[lnK,3]
            IF FOUND()
              DELETE
            ENDIF
          ENDIF
        ENDFOR
      ENDIF
      *C037438,4  TMI [End  ] 
      
    ENDIF         
    
  ENDIF
ENDSCAN

*C037438,3  TMI [Start] Update the style field to be one style for each PO
SELECT (lcTmpPoLns)
LOCATE
DO WHILE !EOF()
  lcPO = &lcTmpPoLns..PO
  lcStyMaj = SUBSTR(&lcTmpPoLns..STYLE,1,lnClrSrt-1) 
  SCAN REST WHILE po+ctype+style+citem+cIClr+cWareCode+cRollId = lcPO
    REPLACE STYLE WITH lcStyMaj+SUBSTR(STYLE,lnClrSrt,lnClrEnd)
  ENDSCAN
ENDDO
*C037438,3  TMI [End  ] 
USE IN &lcTmpCur
ERASE (gcWorkDir+lcTmpCur+'.DBF')
ERASE (gcWorkDir+lcTmpCur+'.CDX')

SELECT (lcTmpPoLns)
LOCATE

SELECT (lnSlct)

*-- end of lf10SzScl.