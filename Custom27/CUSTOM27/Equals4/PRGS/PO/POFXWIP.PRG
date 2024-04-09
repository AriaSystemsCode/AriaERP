*:****************************************************************
*: Program file  : POFIXWIP
*: Program desc. : FIX WIP
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Style Purchase Order [PO]
*: Developer     : ABDOU ELGENDI -  [ABD]
*: Date          : 09/14/2003
*: Tracking Job Number : B607373,1 
*:****************************************************************
*: Calls         : FUNCTIONS  : 
*:               -----------------------------------------------
*:               : PROCEDURE  : ..
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:B607373,1
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:

DIMENSION laOpenFile[06,3]
STORE .F. TO llOpenFils

IF gfModalGen("TRM00000B00006","DIALOG",.F.,.F.,;
              'Are you sure you want to run the fix now?.') = 2
  RETURN
ENDIF 

IF !gfSetup()
  RETURN
ENDIF

lcCTKTBOM = gfTempName()

*-- Function to open needed files.
= lfOpenFls ()

*-- Function to Start fix the data.
= lfFixWip()

*-- Close opened files.
= lfClosFls()

= gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,;
              'Process completed successful.')

glQuitting = .T.  && This is modal screen, this rise quitting flag.



*-- End OF Code.
*:*************************************************************
*: Name      : lfOpenFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to open needed files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfOpenFls ()
*:*************************************************************
*:
FUNCTION lfOpenFls
PRIVATE lnPrvAlias 

lnPrvAlias = SELECT(0)

*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .
*-- Aria 27 Tables.

llOpenFils = .T.
laOpenFile[1,1] = 'Posln'
laOpenFile[1,2] = 'Posln'

laOpenFile[2,1] = 'poshdr'
laOpenFile[2,2] = 'poshdr'

laOpenFile[3,1] = 'CTKTBOM'
laOpenFile[3,2] = 'CTKTBOM'

laOpenFile[4,1] = 'BOMLINE'
laOpenFile[4,2] = 'BOMLINE'

laOpenFile[5,1] = 'BOM'
laOpenFile[5,2] = 'BOM'

laOpenFile[6,1] = 'STYLE'
laOpenFile[6,2] = 'STYLE'


FOR I = 1 To ALEN(laOpenFile,1)
  IF !USED(laOpenFile[I,1])
    laOpenFile[I,3] =gfOpenFile(gcDataDir+laOpenFile[I,1],laOpenFile[I,2], "SH")
  ENDIF
ENDFOR

*SELECT CTKTBOM
*=AFIELDS(laFileStru)
*=gfCrtTmp(lcCTKTBOM,@laFileStru,'')

SELECT(lnPrvAlias)

*-- End of lfOpenFls.
*:*************************************************************
*: Name      : lfClosFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to close opened files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfClosFls ()
*:*************************************************************
*:
FUNCTION lfClosFls

IF llOpenFils
  FOR I = 1 To ALEN(laOpenFile,1)
    IF USED(laOpenFile[I,1]) .AND.  laOpenFile[I,3]
      = gfCloseFile(laOpenFile[I,1])
    ENDIF
  ENDFOR
ENDIF

IF USED(lcCTKTBOM)
  USE IN (lcCTKTBOM)
ENDIF
ERASE (gcWorkDir+lcCTKTBOM+".DBF")
ERASE (gcWorkDir+lcCTKTBOM+".CDX")


*-- End of lfClosFls
*:*************************************************************
*: Name      : lfFixWip
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function Fix the wrong data.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfFixWip ()
*:*************************************************************
*:
FUNCTION lfFixWip
PRIVATE lcOldSteDl

*-- Save the old seting for the delete to scan on the file without change 
*-- the pointer in case I will delete the record.

SELECT POSHDR
LOCATE
SCAN
  STORE 0 TO lnPoOrdr , lnRcvPO
  SCATTER MEMVAR MEMO 
  WAIT WINDOW 'Update PO# :  ' + M.Po +'   Please Wait....' NOWAIT
  DO CASE
    *-- case the open POs
    CASE Status = 'O'
     IF SEEK (M.cstytype+M.po,'POSLN')
       *-- Check If the order is open and received all the Qty.
       *-- this order should be complete.
       IF lfchkStatus ()  && in Case return True,so This order is open and Check the Cost Sheet
         = lfCostShet ()
       ENDIF
     ELSE
       *-- Complet this PO for refrence.
       IF POSHDR.Status # 'C'
         REPLACE STATUS WITH 'C'   ,;
                 cOwner WITH 'ARIA'
       ENDIF
     ENDIF
     
    *-- case the Hold POs
    CASE Status = 'H'
    
    *-- case the Complete POs
    CASE Status = 'C'
    
  ENDCASE
ENDSCAN


*-- End OF lfFixWip
*:*************************************************************
FUNCTION lfchkStatus 
PRIVATE llComplete , laOrdQty , laRcvQty, laDamQty , laCanQty, laOpenQty ;
        lnOrdQty, lnRcvQty , lnDamQty , lnCanQty , lnPrvAlias


lnPrvAlias = SELECT(0)
llComplete = .F.
DIME laOrdQty[8] , laRcvQty[8] , laDamQty[8] , laCanQty[8], laOpenQty[8]
STORE 0 TO lnOrdQty, lnRcvQty , lnDamQty , lnCanQty ,;
            laOrdQty , laRcvQty , laDamQty , laCanQty, laOpenQty , lnOpenQty ,;
            lnlancost1 , lnlancost2 , lnlancost3,lnlancost4,lnlancost5

SELECT POSLN
=SEEK (M.cstytype+M.po)
DO WHILE cstytype+po+style+STR(lineno,6)+trancd = M.cstytype+M.po
  
  lcLineNo = STR(lineno,6)
  lcStyle  = Style
  *-- Scan For Every line Every time.
  SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = M.cstytype+M.po+lcStyle+lcLineNo

  WAIT WINDOW 'Collecting data for style # :' + POSLN.style +'   Please Wait....' NOWAIT  
  
    *-- Get the Line Qty to update the POSHDR.
    DO CASE
      *-- Order Qty.
      CASE trancd = '1'
        lnOrdQty = lnOrdQty + (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)

        *-- if still faluse that means this order still open.
        
          FOR lnStart = 1 to 8
            lcStart = STR(lnStart,1)
            laOrdQty[lnStart] = laOrdQty[lnStart] + Qty&lcStart
          ENDFOR
        
        *-- Received Qty.
      CASE trancd = '2'
        lnQty = Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        lnRcvQty = lnRcvQty + lnQty

        lnlancost1 = lnlancost1 + (lnQty * nlan_Cst1)
        lnlancost2 = lnlancost2 + (lnQty * nlan_Cst2)
        lnlancost3 = lnlancost3 + (lnQty * nlan_Cst3)
        lnlancost4 = lnlancost4 + (lnQty * nlan_Cst4)
        lnlancost5 = lnlancost5 + (lnQty * nlan_Cst5)
        

        *-- if still faluse that means this order still open.
        
          FOR lnStart = 1 to 8
            lcStart = STR(lnStart,1)
            laRcvQty[lnStart] = laRcvQty[lnStart] + Qty&lcStart
          ENDFOR
        
        
      *- PO Shipment :- we will ignore it.  
      CASE trancd = '3'
    
      *-- Secound + Damage
      CASE trancd = '4'
        lnDamQty = lnDamQty + (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)

        *-- if still faluse that means this order still open.
        
          FOR lnStart = 1 to 8
            lcStart = STR(lnStart,1)
            laDamQty[lnStart] = laDamQty[lnStart] + Qty&lcStart
          ENDFOR
      
      *-- Cancel Qty
      CASE trancd = '5'

        lnCanQty = lnCanQty + (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)
        *-- if still faluse that means this order still open.
        
          FOR lnStart = 1 to 8
            lcStart = STR(lnStart,1)
           laCanQty[lnStart] = laCanQty[lnStart] + Qty&lcStart
          ENDFOR
        
    ENDCASE
  ENDSCAN
  
  *-- Check for the Order.
  IF !llComplete
    FOR lnStart = 1 to 8
      lcStart = STR(lnStart,1)
      IF laOrdQty[lnStart] > laRcvQty[lnStart]  + laDamQty[lnStart] + laCanQty[lnStart]
        llComplete = .T.
        EXIT
      ENDIF  
    ENDFOR
  ENDIF

ENDDO

FOR lnStart = 1 to 8
  lcStart   = STR(lnStart,1)
  lnOpenQty = lnOpenQty + MAX(laOrdQty[lnStart]-(laRcvQty[lnStart]  + laDamQty[lnStart] + laCanQty[lnStart]),0)
ENDFOR

SELECT POSHDR
REPLACE  nstyorder  WITH lnOrdQty  ,;
         receive    WITH lnRcvQty  ,;
         Cancel     WITH lnCanQty  ,;
         Damage     WITH lnDamQty  ,;
         Open       WITH lnOpenQty ,;
         nlan_cost1 WITH lnlancost1,;
         nlan_cost2 WITH lnlancost2,;
         nlan_cost3 WITH lnlancost3,;
         nlan_cost4 WITH lnlancost4,;
         nlan_cost5 WITH lnlancost5,;
         ntot_Cost  WITH nlan_cost1 + nlan_cost2 + nlan_cost3 + nlan_cost4 + nlan_cost5
         
         
IF POSHDR.Status # 'C' .AND. !llComplete
  REPLACE STATUS WITH 'C'   ,;
          cOwner WITH 'ARIA'
ENDIF

SELECT(lnPrvAlias)

RETURN .T.

*-- End OF lfchkStatus 
*:*************************************************************
*-- should Check for Every line in case 1 or 2.
FUNCTION lfCostShet
PRIVATE lnPrvAlias , lnUntCost
DIME laStyls[5] 
STORE '' to laStyls 
lnUntCost  = 0
lnPrvAlias = SELECT(0)


*-- Check first if we have cost sheet r not.
IF !SEEK('I'+M.Po,'CTKTBOM')
  FOR lnStart = 1 TO 5
    lcStart   = STR(lnStart,1)
    lnUntCost = lfGetCost(lcStart)
    IF !(lnUntCost = 0)
      = SEEK(laStyls[lnStart],'STYLE') .AND. SEEK(Style.cstymajor+lcStart,'BOM')
      SELECT CTKTBOM
      APPEN BLANK
      REPLACE Cuttkt    WITH M.po        ,;
              Typ       WITH Bom.Typ     ,;
              cimTyp    WITH 'I'         ,;
              cCatgTyp  WITH Bom.cCatgTyp,;
              Item      WITH Bom.Item    ,;
              mfgcode   WITH Bom.mfgcode ,;
              trim_invt WITH .F.         ,;
              Desc      WITH Bom.Desc    ,;
              UOM       WITH 'EAC'       ,;
              UntCost   WITH lnUntCost   ,;
              untQty    WITH 1           ,;
              Pieces    WITH POSHDR.nstyorder,;
              Req_Qty   WITH POSHDR.nstyorder,;
              Est_Cost  WITH UntCost * Pieces,;
              link_code WITH 'DEFDEF'        ,;
              Date      WITH POSHDR.Entered  ,;
              lVoid     WITH .F.             ,;
              cOwner    WITH 'ARIA'
      =gfAdd_Info('CTKTBOM')
    ENDIF
  ENDFOR
ENDIF

SELECT POSLN
=SEEK (M.cstytype+M.po)
SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = M.cstytype+M.po ;
  FOR Trancd $ '12'

  llAddRec = .F.
  *-- Update the BomLine File.
  *-- cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
  IF SEEK(POSLN.STYLE,'STYLE') .AND. SEEK(style.cstymajor,'BOM')

    IF SEEK('I'+ Bom.Typ+M.po,'BOMLINE')
      SELECT BOMLINE
      LOCATE REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode = ;
      'I'+ Bom.Typ+M.po FOR STR(lineno,6) = STR(POSLN.lineno,6) .AND. cType = Posln.Trancd
      IF !FOUND()
        llAddRec = .T.
      ENDIF
    ELSE
      llAddRec = .T.
    ENDIF
    
    IF llAddRec
      SELECT BOM
      SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = style.cstymajor FOR ;
        citmmask = STYLE.Style
      
        SELECT BOMLINE
        APPEN BLANK
        REPLACE cImtyp    WITH 'I'    ,;
                ctktno    WITH M.po   ,;
                LineNo    WITH POSLN.LineNo,;
                Style     WITH POSLN.Style ,;
                mfgcode   WITH Bom.mfgcode ,;
                UnitQty   WITH Bom.nBomTotQty,;
                UnitCost  WITH Bom.UntCost,;
                cType     WITH POSLN.Trancd   ,;
                cCatgtyp  WITH 'P'        ,;
                crsession WITH '000000'   ,;
                lVoid     WITH .F.        ,;
                cSizes    WITH '12345678' ,;
                cStygrade WITH Style.cStygrade
            =gfAdd_Info('BOMLINE')
        SELECT BOM          
      ENDSCAN     
    ENDIF
    
  ENDIF
ENDSCAN

SELECT(lnPrvAlias)
*-- END OF lfCostShet
*:*************************************************************
FUNCTION lfGetCost
PARAMETER lcSendType
PRIVATE lnPrvAls , lnTotQtCst , lnTotQty
lnPrvAls = SELECT(0)

SELECT BOM
STORE 0 TO lnTotQtCst , lnTotQty

SELECT POSLN
=SEEK (M.cstytype+M.po)
SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = M.cstytype+M.po ;
  For Trancd = '1'
  *-- citmmajor+typ+citmmask+mfgcode+item+iclr
  IF SEEK(POSLN.STYLE,'STYLE') .AND. SEEK(style.cstymajor+lcSendType,'Bom')
    IF EMPTY(laStyls[EVal(lcSendType)])
      laStyls[EVal(lcSendType)] = style.Style
    ENDIF

    lnTotQtCst =  lnTotQtCst + (TotQty * Bom.UntCost)
    lnTotQty   =  lnTotQty  + TotQty
  ENDIF
ENDSCAN

IF lnTotQty = 0
  lnReturn = 0
ELSE
  lnReturn = lnTotQtCst / lnTotQty
ENDIF

SELECT(lnPrvAls)
RETURN lnReturn
*-- End OF lfGetCost
*:*************************************************************