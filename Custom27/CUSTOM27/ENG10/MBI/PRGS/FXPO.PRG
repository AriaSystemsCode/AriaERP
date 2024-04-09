*- Fix program to add the missing records in POSHDR
*- Issue 120850
*- Done by Ashraf Sherif on 12/23/2003
*- 
close all
use NotePad order 1
sele 0
use style order 1
sele 0
use ApVendor order 1
sele 0
use poshdr order 1
sele 0
use posln order 1
scan
  wait wind po nowait
  if !seek(cstytype+po,'poshdr')
    =seek(posln.style,'Style')
    =seek(vendor,'ApVendor')
    llNote = seek('P'+PO,'NotePad')
    select poshdr
    append blank
    replace cstytype   WITH posln.cstytype    ,;
            po         with posln.po          ,;
            vendor     with posln.Vendor      ,;
            status     with 'C'               ,;
            cDivision  with Style.cDivision   ,;
            cPurCode   With Style.Cpurcode    ,;
            Entered    with posln.dAdd_date   ,;
            Complete   with posln.dAdd_date   ,;
            Available  with posln.dAdd_date   ,;
            cTermCode  with IIF(EMPTY(ApVendor.cTermCode),'000006',ApVendor.cTermCode),;
            ShipVia    with '000027'          ,;
            ShpName    with 'MAIN WAREHOUSE'  ,;
            cWareCode  with 'MAIN'            ,;
            Link_code  with 'DEFDEF'          ,;
            cPriceCur  with 'GBP'             ,;
            cDutyCur   with 'GBP'             ,;
            nPriceRat  with 1                 ,;
            nDutyRat   with 1                 ,;
            cItemWare  with 'MAIN'            ,;
            cMatWare   with 'MAIN'            ,;
            nCurrUnit  with 1                 ,;
            ndCurUnit  with 1                 ,;
            lHasNotes  with llNote            ,;
            cAdd_User  with POSLN.cAdd_User   ,;
            cAdd_time  with POSLN.cAdd_time   ,;
            dAdd_date  with POSLN.dAdd_date   ,;
            cOwner     with 'ARIA'            
  endif
endscan

select poshdr
scan for cowner = 'ARIA'
  wait wind po nowait
  =seek(cstytype+po,'posln')
  select posln
  lnLastLn = 0
  lnOrd    = 0
  lnRecv   = 0
  lnDamg   = 0
  lnCncl   = 0
  lnSOrd   = 0
  scan while cstytype+po = poshdr.cstytype+poshdr.po
    DO Case 
      Case Trancd = '1'
        lnLastLn = lnLastLn + 1
        lnSOrd   = lnSOrd   + TotQty
        lnOrd    = lnOrd    + TotOrd
      Case Trancd = '2'        
        lnRecv   = lnRecv   + TotQty
      Case Trancd = '4'
        lnDamg   = lnDamg   + TotQty
      Case Trancd = '5'        
        lnCncl   = lnCncl   + TotQty
    ENDCASE
  endscan
  select poshdr
  replace lastline  with lnLastLn ,;
          nStyOrder with lnSOrd   ,;
          TotOrd    with lnOrd    ,;
          Receive   with lnRecv   ,;
          Damage    with lnDamg   ,;
          Cancel    with lnCncl   ,;
          Open      With MAX(lnSOrd - lnRecv - lnDamg - lnCncl,0)
  IF Open <> 0
    REPLACE Status WITH 'O'
  ENDIF
  IF EMPTY(cDivision)
    REPLACE cDivision WITH 'ARA'
  ENDIF
  IF EMPTY(cPurCode)
    REPLACE cPurCode WITH '000060'
  ENDIF
  
endscan
close all
wait window 'Done' nowait


