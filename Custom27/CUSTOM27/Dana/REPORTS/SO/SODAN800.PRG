*!********************************************************************
*: Program file  : SoDan800.prg (Refere to C101782) -Conversion of Dan800.prg in A26-
*: Program desc. : Customized open order report for Dana Udnies Inc.
*: Date          : 03/02/2000
*: For screen    : None.
*:         System: Aria Apparel Series (A27)
*:      Developer: Sameh Al-Desouki (SAM)
*!********************************************************************
*! Calls  : - 
*!         Functions : gfTempName , gfOpenFile
*!********************************************************************
*! Modifications : -
*!********************************************************************
*-- Note :
*--      The needed files for this report are opened by OPTION GRID ,they are :
*--              1- Customer file with customer index ( Type + Account + Store )
*--              2- OrdHdr file with OrdAcct index  (Account + cOrdType + Order) 
*--              3- OrdLine file with OrdLine index (cOrdType + Order + STR(Lineno,6))

*-- Initialize variables needed
PRIVATE lcOldCurr,lnX,lnStyPos,lnStyLen,laAllSegs,lcDumSty
DIMENSION laAllSegs[1]
STORE SPACE(0) TO lcOldCurr,lnX,lnStyPos,lnStyLen
STORE '' TO laAllSegs,lcDumSty

*-- Create Lines temp. file 
lcTmpLine = gfTempName()
CREATE DBF (gcWorkDir + lcTmpLine) (Account C(5),Order C(6),Entered D(8),CustPo C(15),;
            Start D(8),MultiPo L(1),Complete D(8),Multi C(1),Store C(8),Style C(19),;
            nQty N(9,0),nQtyPrice N(15,2),cStore C(8),cCustPo C(15),PikTkt C(6),STName C(30)) 

INDEX ON Account + Order + Style TAG &lcTmpLine
*-- Get company name
lcCompName = gcCom_Name

*-- Get major start pos. and len.
=gfItemMask(@laAllSegs)
FOR lnX=1 TO ALEN(laAllSegs,1)
  IF laAllSegs[lnX,1]='F' 
    lnStyPos = laAllSegs[lnX,4] 
    lnStyLen = LEN(laAllSegs[lnX,3])
  ENDIF
ENDFOR


*-- Use the temp. file generated bo Option Grid to scan for selected customers

*-- Note: (Why I use three sections to insert)
*--      If you select some customers from option grid and you preview them ,
*--      The temp. file laOgFxFlt[1,6] is generated and have the selected records
*--      Then if you deselect them and preview ,laOgFxFlt[1,6] still have the temp. name
*--      and the file is still used but it's empty of records .
*--      So I use those three sections to insert new records 

IF !EMPTY(laOgFxFlt[1,6]) AND USED(laOgFxFlt[1,6])
  *-- scan the customer temp. file 
  SELECT (laOgFxFlt[1,6])
  GO TOP
  IF !EOF()
    SCAN
      SELECT OrdHdr
      IF SEEK(EVALUATE(laOgFxFlt[1,6]+'.Account') + 'O')
        SCAN REST WHILE Account + cOrdType = &laOgFxFlt[1,6]..Account + 'O' ;
                    FOR (Status = 'O' OR Status = 'H')
          SELECT OrdLine
          =SEEK(OrdHdr.cOrdType + OrdHdr.Order)
          SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O' + OrdHdr.Order FOR  OrdLine.TotQty <> 0
            *-- Insert into temporary file
            WAIT 'Collecting data for customer '+ordhdr.account+', Order #'+ORDHDR.ORDER  WINDOW NOWAIT
            *-- Check if this major is the last saved one
            lcDumSty = SUBSTR(OrdLine.Style,lnStyPos,lnStyLen)  
            IF !SEEK( Account + Order + lcDumSty , lcTmpLine )
              *-- Add new record if not the same major
              INSERT INTO (lcTmpLine) (Account,Order,Entered,CustPo,Start,MultiPo,Complete,;
                                       Multi,Store,Style,nQty,nQtyPrice,cStore,cCustPo,PikTkt) ;
                                       VALUES (OrdHdr.Account,OrdHdr.Order,;
                                       OrdHdr.Entered,OrdHdr.CustPo,OrdHdr.Start,OrdHdr.MultiPo,;
                                       OrdHdr.Complete,OrdHdr.Multi,OrdHdr.Store,;
                                       lcDumSty,OrdLine.TotQty,;
                                       OrdLine.Price * OrdLine.TotQty,;
                                       OrdLine.Store,OrdLine.CustPo,OrdLine.PikTkt)
            ELSE
              *-- Add total quantities if the same major found
              REPLACE &lcTmpLine..nQty      WITH &lcTmpLine..nQty + OrdLine.TotQty  ;
                      &lcTmpLine..nQtyPrice WITH &lcTmpLine..nQtyPrice + OrdLine.Price * OrdLine.TotQty
            ENDIF *-- End of check if this major is the last saved one
          ENDSCAN   
        ENDSCAN
      ENDIF
    ENDSCAN
  ELSE
    *-- get all the orders of all accounts.
    SELECT OrdHdr
    *-- Change default order to optimize search expression
    SET ORDER TO TAG ORDHDR && cOrdType + Order
    =SEEK('O')
    SCAN REST WHILE cOrdType ='O' FOR (Status = 'O' OR Status = 'H')
      SELECT OrdLine
      =SEEK(OrdHdr.cOrdType + OrdHdr.Order)
      SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O' + OrdHdr.Order FOR  OrdLine.TotQty <> 0
        *-- Insert into temporary file
        WAIT 'Collecting data for customer '+ordhdr.account+', Order #'+ORDHDR.ORDER  WINDOW NOWAIT
        *-- Check if this major is the last saved one
        lcDumSty = SUBSTR(OrdLine.Style,lnStyPos,lnStyLen)  
        IF !SEEK( Account + Order + lcDumSty , lcTmpLine )
          *-- Add new record if not the same major
          INSERT INTO (lcTmpLine) (Account,Order,Entered,CustPo,Start,MultiPo,Complete,;
                                   Multi,Store,Style,nQty,nQtyPrice,cStore,cCustPo,PikTkt) ;
                                   VALUES (OrdHdr.Account,OrdHdr.Order,;
                                   OrdHdr.Entered,OrdHdr.CustPo,OrdHdr.Start,OrdHdr.MultiPo,;
                                   OrdHdr.Complete,OrdHdr.Multi,OrdHdr.Store,;
                                   lcDumSty,OrdLine.TotQty,;
                                   OrdLine.Price * OrdLine.TotQty,;
                                   OrdLine.Store,OrdLine.CustPo,OrdLine.PikTkt)
        ELSE
          *-- Add total quantities if the same major found
          REPLACE &lcTmpLine..nQty      WITH &lcTmpLine..nQty + OrdLine.TotQty  ;
                  &lcTmpLine..nQtyPrice WITH &lcTmpLine..nQtyPrice + OrdLine.Price * OrdLine.TotQty
        ENDIF *-- End of check if this major is the last saved one
      ENDSCAN
    ENDSCAN
    *-- Reset order of OrdHdr 
    SET ORDER TO TAG ORDACCT IN 'OrdHdr' && Account + cOrdType + Order IN 'OrdHdr'
  ENDIF  
ELSE
*-- if the user first choose to preview without selecting customers  
  *-- get all the orders of all accounts.
  SELECT OrdHdr
  *-- Change default order to optimize search expression
  SET ORDER TO TAG ORDHDR  && cOrdType + Order
  =SEEK('O')
  SCAN REST WHILE cOrdType ='O' FOR (Status = 'O' OR Status = 'H')
    SELECT OrdLine
    =SEEK(OrdHdr.cOrdType + OrdHdr.Order)
    SCAN REST WHILE cOrdType+Order = 'O' + OrdHdr.Order FOR  OrdLine.TotQty # 0
      *-- Insert into temporary file
      WAIT 'Collecting data for customer '+ordhdr.account+', Order #'+ORDHDR.ORDER  WINDOW NOWAIT
      *-- Check if this major is the last saved one
      lcDumSty = SUBSTR(OrdLine.Style,lnStyPos,lnStyLen)  
      IF !SEEK( Account + Order + lcDumSty , lcTmpLine )
        *-- Add new record if not the same major
        INSERT INTO (lcTmpLine) (Account,Order,Entered,CustPo,Start,MultiPo,Complete,;
                                 Multi,Store,Style,nQty,nQtyPrice,cStore,cCustPo,PikTkt) ;
                                 VALUES (OrdHdr.Account,OrdHdr.Order,;
                                 OrdHdr.Entered,OrdHdr.CustPo,OrdHdr.Start,OrdHdr.MultiPo,;
                                 OrdHdr.Complete,OrdHdr.Multi,OrdHdr.Store,;
                                 lcDumSty,OrdLine.TotQty,;
                                 OrdLine.Price * OrdLine.TotQty,;
                                 OrdLine.Store,OrdLine.CustPo,OrdLine.PikTkt)
      ELSE
        *-- Add total quantities if the same major found
        REPLACE &lcTmpLine..nQty      WITH &lcTmpLine..nQty + OrdLine.TotQty  ;
                &lcTmpLine..nQtyPrice WITH &lcTmpLine..nQtyPrice + OrdLine.Price * OrdLine.TotQty
      ENDIF *-- End of check if this major is the last saved one
    ENDSCAN
  ENDSCAN
  *-- Reset order of OrdHdr 
  SET ORDER TO TAG ORDACCT IN 'OrdHdr' && Account + cOrdType + Order IN 'OrdHdr'
ENDIF
WAIT CLEAR

*-- check if there are records in line temp. file
SELECT (lcTmpLine)
IF EOF() AND RECCOUNT() <= 0
  = gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

SET RELATION TO 'M'+Account INTO Customer

*-- change to dollars for that company
lcOldCurr = SET("CURRENCY")
SET CURRENCY TO "$"

*-- display report
DO gfDispRe WITH EVALUATE('lcFormName')

*-- reset currency
SET CURRENCY TO (lcOldCurr)


*--- End of Program