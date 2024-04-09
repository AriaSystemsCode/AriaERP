*:****************************************************************************************
*: Program file  : Alpktkcd.Prg
*: Program desc. : Picking Ticket (Cathy Danial) 
*: System        : Aria Apparel System (A27).
*: Module        : Sales Order Allocation (AL).
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102332,1
*: Date          : 05/14/2001
*:****************************************************************************************
*: Calls 
*:               : FUNCTIONS  : lfNewColet , lfUpdatBlk , lfCrTmpFls , lfUpStores
*:               :            : lfInsrtRec, lfBulkQty, lfGetOrd
*:               ----------------------------------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
*: C#102332,4 IAA
*: Note :- We will Not Print the NotePad or line Note in this Form.
*: B606156,1 BWA 06/24/2002 Add the No of stores in the header of the Piktkt.
*: B606277,1 ABD 07/16/2002 Problem with the pick ticket format.
*: B606431,1 ABD 09/02/2002 Add group by DC. in the option grid and default to 'Yes'
*:****************************************************************************************
*:
*-- Declaration variables.
*-- lcTmpOrdL :- this file come from the piktkt program after filteration.
*-- lcTmpBulk :- Bulk File to hold the total for all orders on this style.
*-- lcStyQty  :- Index on the file that came from the program after 
*--           :- filteration on style + qty1+....+qty8+store
*-- cBulkQty  :- Index on the transaction file that we will print the
*--           :- data from it , index on ALLTRIM(cBulkQty)+  STYLE + ALLTRIM(CgroupNo)+Shipvia
*-- lcTmpOrdU :- this index use in the printing in the main program.
*-- lcOrdLTmp1:- this index help us to update the header recored .
*-- lcOrderBy:-  use this index to set the order of the final table

PRIVATE  lnAlias       , laFileStru , lnFileStru , lcOrdLTmp , lnOrdPos, lcOrderBy
STORE 0  TO lnAlias    , lnFileStru , lnOrdPos
DIMENSION laFileStru[1,4]
STORE '' TO laFileStru , lcOrdLTmp, lcOrderBy
*-- End Declaration variables.

lnAlias = SELECT (0)

*-- Remove all relation.
SELECT (lcTmpOrdL)
lcOrdLTmp = gfTempName()
lcBulkQty = gfTempName()
lcOrdIndx = gfTempName()
lcOrderBy = gfTempName()

*B606277,1 ABD - Create needed new files. [Begin]
lcOrdLTmp2 = gfTempName()
lcMainF    = gfTempName()
IF llRpLayOut = 'B'
  lcTmpStyle = gfTempName()
  lcTmpStors = gfTempName()
  lcTempOrds = gfTempName()
  lcTmpOrdSt = gfTempName()
  
  CREATE TABLE (gcWorkDir+lcTmpStors) (Store C(8) , mStyles M(10))
  INDEX ON STORE TAG (lcTmpStors) ADDITIVE

  CREATE TABLE (gcWorkDir+lcTmpStyle) (Style C(19) , cBulkQty C(50) , nStyleNo N(5))
  INDEX ON Style + cBulkQty TAG (lcTmpStyle) ADDITIVE
ENDIF
*B606277,1 ABD - [End]

* lcTmpGrp = gfTempName()
*-- New index on temp file that hold the select data.
lcStyQty = gfTempName()

lcOrdLTmp1 = gfTempName()
*-- call function to create temp file.

= lfCrTmpFls ()
*-- Return the old relation.
*-- New function to collect the data upon the new selection critria
*-- In the optiongrid.

SELECT (lcTmpOrdL)
*-- C#102332,4 IAA
IF llRpLayOut = 'B'
*-- BULK
  lcFormName = "alpktkcd"
  = lfNewColet ()
  SELECT (lcTmpOrdL)
  *B606277,1 ABD - create new index on store and style order. [Begin]
  *INDEX ON SUBSTR(style,1,3) + shipvia  TAG (lcOrderBy) ADDITIVE  
  INDEX ON Dist_ctr+Store+cStyOrder+SUBSTR(style,1,3) + shipvia  TAG (lcOrderBy) ADDITIVE  
  *B606277,1 ABD - [End]
ELSE
  *-- multi Order

  *B606156,1 BWA 07/03/2002 Comment this line because they one form only.[START]
  *lcFormName = "alpktkc2"
  *B606156,1 BWA 07/03/2002.[END]

  *-- Flag True if we select more than one order.
  llWorkOrdr = !EMPTY(lcTempStyle) AND USED(lcTempStyle) AND RECCOUNT(lcTempStyle) > 0
  IF llWorkOrdr
    SELECT (lcOrdLTmp)
    INDEX ON cType + ALLTRIM(order) + ALLTRIM(store) + SUBSTR(style,1,3) TAG (lcOrderBy) ADDITIVE
    = lfGetOrd ()
    SELECT (lcTmpOrdL)
    set ORDER to TAG (lcOrderBy)
  ELSE
    SELECT (lcTmpOrdL)
    INDEX ON ALLTRIM(order) + ALLTRIM(store) + SUBSTR(style,1,3) TAG (lcOrderBy) ADDITIVE
  ENDIF
  = lfBulkQty ()
ENDIF
*-- C#102332,4 IAA

*-- Remove all relation & Return it to old relation.
SET RELATION TO 

*B606156,1 BWA 06/24/2002 Add the No of stores in the header of the Piktkt.[START]
*B606277,1 ABD - remark the next few lines. [Begin]
*IF llRpLayOut = 'B'
*  SELECT (lcTmpOrdL)
*  DIMENSION laSearch[1]
*  STORE SPACE(0) TO laSearch
*  SCAN
*    laSearch[1] = &lcTmpOrdL..Mpiktkt
*    REPLACE NoofStor WITH OCCURS("," ,  laSearch[1]) + 1
*  ENDSCAN
*  LOCATE
*ENDIF
*B606277,1 ABD - [End]
*B606156,1 BWA 06/24/2002.[END]

SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE

*B606156,1 BWA 07/03/2002 Add the relation to the ordline table.[START]
SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
*B606156,1 BWA 07/03/2002.[END]

IF llRpLayOut = 'B'
  SET RELATION TO Style+ALLTRIM(cBulkQty)+Shipvia INTO (lcTmpBulk) ADDITIVE
ENDIF
SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
LOCATE

*--- SSH
* lcFormName = IIF(llRpLayOut="B","alpktkc2","alpktkcd")
*B606277,1 ABD - Need to Change the form to the needed form. [Begin]
IF llRpLayOut = "B"
  lcFormName = 'ALPKTKCD'
  = gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
  = lfRepPltFr(lcFormName)
ELSE
  lcFormName = 'ALPKTKC2'
  = gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
  = lfRepPltFr(lcFormName)
ENDIF
SELECT (lcTmpOrdL)
*B606277,1 ABD - [End]
*--- SSH

RETURN
*-- End Of Form code.
*-- Function Section.
*:****************************************************************
*: Name        : lfNewColet
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 05/14/2001
*: Purpose     : Function to collect data on the new criteria
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfNewColet()
*:****************************************************************
*
FUNCTION lfNewColet
PRIVATE lcOldOrder , lcStyle , lcSeekExp

DIMENSION laStores[1]
STORE '' TO lcStyle , lcOldOrder , lcSeekExp , laStores
lnAvrgPrce = 0
SELECT (lcOrdLTmp)
lcOldOrder = Order()
SET ORDER TO lcBulkQty

*-- Flage True if we select more than one order.
llWorkOrdr = !EMPTY(lcTempStyle) AND USED(lcTempStyle) AND RECCOUNT(lcTempStyle) > 0
*-- Get the order

SELECT (lcTmpOrdL)
lcTmpOrder = Order()
Set order to lcStyQty


LOCATE
*-- Variable hold the group Number.
lnGroupNo = 0
lcGroupNo = ''

*B606277,1 ABD - Create new index for needed files. [Begin]
IF llRpLayOut = 'B' 
  SELECT (lcOrdLTmp)
  lnFileStru = AFIELDS(laFileStru)
  DIMENSION laFileStru[lnFileStru,4]
  
  DIMENSION laTags[2,2]
  laTags[1,1] = 'Style+Store+cBulkQty'
  laTags[1,2] = lcTempOrds
  laTags[2,1] = 'Store+Style'
  laTags[2,2] = 'lcTempOrds'
  =gfCrtTmp(lcTempOrds ,@laFileStru,@laTags)

  DIMENSION laTags[2,2]
  laTags[1,1] = 'Style+Store+cStyOrder+ALLTRIM(cBulkQty)'
  laTags[1,2] =  lcMainF
  laTags[2,1] = 'Store+cStyOrder+SUBSTR(style,1,3) + shipvia'
  laTags[2,2] = 'lcMainF'
  =gfCrtTmp(lcMainF,@laFileStru,@laTags)
  SELECT (lcTmpOrdL)
ENDIF
*B606277,1 ABD - [End]

DO WHILE !EOF()
  lcStyle   = Style
  SCAN WHILE  Style = lcStyle
    SCATTER MEMVAR MEMO
    IF !llWorkOrdr .OR. SEEK(m.Order,lcTempStyle)
      
      lcSeekExp = ''
      *-- Cheak for the same Qty.
      FOR I = 1 To 8
        Z = STR(I,1)
        lcSeekExp = lcSeekExp + ALLTRIM(STR(Qty&Z,8))
      ENDFOR

      *B606277,1 ABD - Fill the needed files for style and store , those 
      *B606277,1 ABD - files will use later on. [Begin]
      IF llRpLayOut = 'B' 
        m.cBulkQty = lcSeekExp
        INSERT INTO (lcTempOrds) FROM MEMVAR
        IF !SEEK(m.Store,lcTmpStors)
          INSERT INTO (lcTmpStors) FROM MEMVAR
        ENDIF
      
        IF !SEEK(m.Style+lcSeekExp,lcTmpStyle)
          m.nstyleNo = RECCOUNT(lcTmpStyle) + 1
          m.cBulkQty = lcSeekExp
          INSERT INTO (lcTmpStyle) FROM MEMVAR
        ENDIF
      ENDIF  
      *B606277,1 ABD - [End]
      

      lcShipVia = IIF(CUSTOMER.nBrkWeight <> 0 .AND.;
                      &lcTmpOrdH..nWeight > CUSTOMER.nBrkWeight   ,;
                      CUSTOMER.cAltShpvia ,IIF(ORDHDR.ShipVia ='*',;
                      CUSTOMER.ShipVia,ORDHDR.ShipVia))
     
      *-- If we have the same Qty before that from sales order.
      IF SEEK(lcSeekExp+M.Style,lcOrdLTmp)
        *-- If the pik. qty for every size equal the match record.
        llMatchBlk = .T.
        FOR Y = 1 To 8 
          *-- Cheak for every size.
          Q = ALLTRIM(STR(Y,1))
          IF EVAL(lcOrdLTmp+'.nBulk&Q') <> M.Qty&Q
            llMatchBlk = .F.
            EXIT
          ENDIF 
        ENDFOR
        *-- if all size is match the selected recored.
        IF llMatchBlk .AND. SEEK(lcSeekExp+EVAL(lcTmpOrdL+'.Style')+lcGroupNo+lcShipVia,lcOrdLTmp)
          *-- Update the header file.
          SELECT (lcOrdLTmp)
          FOR W = 1 To 8 
            V = ALLTRIM(STR(W,1))
            REPLACE Pik&V   WITH Pik&V + M.Pik&V ,;
                    Qty&V   WITH Qty&V + M.Qty&V ,;
                    nBulk&V WITH M.Qty&V
          ENDFOR
          *-- Update the Total .
          REPLACE TotPik     WITH TotPik + M.TotPik                   ,;
                  TotQty     WITH TotQty + M.TotQty                  ,;
                  llDif_Prce WITH IIF(llDif_Prce,.T.,Price # M.Price),;
                  nTotBulk   WITH M.TotQty
          
          *-- Update the array Add the Element in this array.
          IF ASCAN(laStores,EVAL(lctmpOrdl+'.Store')) = 0

            STORE 0 To lnLenArray 
            lnLenArray = ALEN(laStores)
            DIMENSION laStores[lnLenArray + 1]
            laStores[lnLenArray + 1] = EVAL(lcTmpOrdL+'.Store')
            SELECT (lcTmpGrp)
            Replace Mstores With mStores + ALLTRIM(EVAL(lcTmpOrdL+'.Store'))

          ENDIF
          *-- Function to add new element in the file & in the array.
          *-- Add the new store
          = lfUpStores ()
          *-- Update header file & Bulk File.
          = lfUpdatBlk (lcShipVia)
        ELSE
          *-- Insert new recored
          M.cBulkQty = lcSeekExp
          INSERT INTO (lcOrdLTmp) FROM MEMVAR
          SELECT (lcOrdLTmp)      

          *-- Update the line file.
          = lfInsrtRec ()
          *-- Update the Header recored.
          REPLACE Mstore WITH Mstore+ IIF(Empty(Mstore),'',',') + ALLTRIM(EVAL(lcTmpGrp+'.Mstores'))
          IF ATC(EVAL(lcTmpOrdL+'.Piktkt'),Eval(lcOrdLTmp+'.MPiktkt')) = 0
            REPLACE MPiktkt   WITH MPiktkt + IIF(Empty(MPiktkt),'',',') + EVAL(lcTmpOrdL+'.Piktkt') + '-' +     ALLTRIM(EVAL(lcTmpOrdL+'.Store'))
          ENDIF
              
          *-- Update the memo Order field,
          IF ATC(OrdLine.Order,Eval(lcOrdLTmp+'.Morder')) = 0
            *-- Update memo order fields.
            REPLACE MOrder WITH MOrder + IIF(Empty(MOrder),'',',') + OrdLine.Order
          ENDIF

         *-- Update the memo account field
         IF ATC(OrdLine.Account,Eval(lcOrdLTmp+'.MAccount')) = 0
           *-- Update memo order fields.
           REPLACE Maccount WITH MAccount + IIF(Empty(MAccount),'',',') + ALLTRIM(OrdLine.Account)
         ENDIF

          
          *-- Update header file & Bulk File.
          = lfUpdatBlk (lcShipVia)
        ENDIF  
        SELECT (lcTmpOrdL)
      ELSE
        *-- Insert new recored
        *-- Add New Group.
        SELECT (lcTmpGrp)
        IF EOF()
          lnGroupNo = lnGroupNo +1
          lcGroupNo = ALLTRIM(STR(lnGroupNo))

          *-- Update the array that hold the stores.
          DIMENSION laStores[1] 
          laStores = ''
*--- SSH
          FOR lnSInde = 1 TO ALEN(laStores,1)
            laStores [lnSInde] = ""
          ENDFOR
          laStores [1] = EVAL(lcTmpOrdL+'.Store')
          SELECT (lcTmpGrp)
          APPEND BLANK
          Replace Mstores With ALLTRIM(EVAL(lcTmpOrdL+'.Store')),;
                  CgroupNo  WITH lcGroupNo
        ELSE
          LOCATE
          llEndLoop = .F.
          SCAN WHILE !llEndLoop .OR. EOF()
            IF ALEN(laStores) = OCCURS(',',Mstores) + 1
              FOR I = 1 To OCCURS(',',Mstores) + 1
              *-- If found this store in the memo field.
                IF laStores [I] $ Mstores
                  lcGroupNo = ALLTRIM(CgroupNo)
                  llEndLoop = .F.
                ELSE
                  llEndLoop = .T.
                  EXIT
                ENDIF
              ENDFOR
             ELSE
               llEndLoop = .T. 
            ENDIF
          ENDSCAN
         
         *-- If this flag is true we will addd new recored with new 
         *-- Group for new stores.
          IF llEndLoop
*--- SSH
            DIMENSION laStores[1] 
            laStores = ''
            FOR lnSInde = 1 TO ALEN(laStores,1)
              laStores [lnSInde] = ""
            ENDFOR
            lnGroupNo = lnGroupNo +1
            lcGroupNo = ALLTRIM(STR(lnGroupNo))
            *-- Update the array that hold the stores.
            DIMENSION laStores[lnGroupNo] 
            laStores [lnGroupNo] = EVAL(lcTmpOrdL+'.Store')
            *-- Update the group file.
            SELECT (lcTmpGrp)
            APPEND BLANK
            Replace Mstores With ALLTRIM(EVAL(lcTmpOrdL+'.Store')),;
                    CgroupNo  WITH lcGroupNo
          ENDIF
        ENDIF
        *--- Update the line file.
        M.cBulkQty = lcSeekExp
        INSERT INTO (lcOrdLTmp) FROM MEMVAR
        SELECT (lcOrdLTmp)
        
        *-- Update the line file.
        = lfInsrtRec ()
         
        *-- Update the header recored that hold all stores in the memo 
        *-- Field that we will print first.
        = lfUpStores()
        
       SELECT (lcTmpOrdL)
       *-- Update header file & Bulk File.
        = lfUpdatBlk (lcShipVia)
      ENDIF
    ENDIF
    SELECT (lcTmpOrdL)
  ENDSCAN
ENDDO


*B606277,1 ABD - call function to rearange the record. [Begin]
IF llRpLayOut = 'B'
  *-- Function to rearrange the stores - styles.
  = lfReArgRec ()
ENDIF
*B606277,1 ABD - [End]


SELECT (lcTmpOrdL)
SET ORDER TO &lcTmpOrder

SELECT (lcOrdLTmp)
SET ORDER TO &lcOldOrder

*B606156,1 BWA 07/03/2002 Function to break the old relations.[START]
=lfBrakRel()
*B606156,1 BWA 07/03/2002.[END]
lcTmpOrdL = lcOrdLTmp

*-- End Of lfNewColet.
*:****************************************************************
*: Name        : lfUpdatBlk
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 02/11/2001
*: Purpose     : Function to Update the header file.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfUpdatBlk ()
*:****************************************************************
*
FUNCTION lfUpdatBlk
PARAMETER lcSendShip
PRIVATE lnAlias , lcShipVia
lcShipVia = ''
lnAlias   = 0
lnAlias  = SELECT (0)  
lcShipVia = lcSendShip


*-- Update the bulk file 
SELECT (lcTmpBulk)
IF SEEK(Ordline.style+lcSeekExp+lcShipVia,lcTmpBulk)
  SELECT (lcTmpBulk)
  *-- Update sizes.
  FOR I = 1 To 8
    Z = ALLTRIM(STR(I,1))
    REPLACE  nBulk&Z WITH nBulk&Z + M.Qty&Z &&,;
*             nPik&Z  WITH nPik&Z + M.Pik&Z
  ENDFOR
  *-- Update the Total.
  REPLACE nTotBulk  WITH nTotBulk + M.TotQty
  
ELSE
  SELECT (lcTmpBulk)
  APPEND BLANK
  *-- Update sizes.
  FOR I = 1 To 8 
    Z = ALLTRIM(STR(I,1))
    REPLACE  nBulk&Z WITH M.Qty&Z &&,;
*             nPik&Z  WITH M.PIK&Z
  ENDFOR
  *-- Update the Total.
  REPLACE nTotBulk  WITH M.TotQty ,;
          Style     WITH M.Style  ,;
          cBulkQty  WITH lcSeekExp,;
          Shipvia   WITH lcShipVia 
ENDIF

SELECT (lnAlias)
*-- End OF lfUpdatBlk.
*:****************************************************************
*: Name        : lfCrTmpFls
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 02/11/2001
*: Purpose     : Function to Create temp files.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfCrTmpFls ()
*:****************************************************************
*
FUNCTION lfCrTmpFls

PRIVATE lnOldAls
lnOldAls = SELECT(0)

*-- Create the Temp file that we will collect the data on it.
SELECT (lcTmpOrdL)
*--We will add new index on style + Sizes.
INDEX ON STYLE + STR(Qty1,6) + STR(Qty2,6) + STR(Qty3,6) + STR(Qty2,6);
               + STR(Qty4,6) + STR(Qty5,6) + STR(Qty6,6) + STORE TAG (lcStyQty);
                 OF (gcWorkDir+lcStyQty+'.CDX')

= AFIELDS (laFileStru)
lnFileStru = ALEN(laFileStru,1)

*B606156,1 BWA 06/24/2002 Redimension the array to add new field for the no of stores.[START]
*DIMENSION laFileStru[lnFileStru + 19, 4]
*B606277,1 ABD - redimension the array to add new field. [Begin]
*DIMENSION laFileStru[lnFileStru + 20, 4]

*B606431,1 ABD - ReDIMENSION the array To add new field 'Dist_ctr' [Begin]
*DIMENSION laFileStru[lnFileStru + 21, 4]
DIMENSION laFileStru[lnFileStru + 22, 4]
*B606431,1 ABD - [End]

*B606277,1 ABD - [End]

*B606156,1 BWA 06/24/2002.[END]

laFileStru[lnFileStru+1,1] = 'cBulkQty'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 50
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'nBulk1'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 10
laFileStru[lnFileStru+2,4] = 0

laFileStru[lnFileStru+3,1] = 'nBulk2'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 10
laFileStru[lnFileStru+3,4] = 0

laFileStru[lnFileStru+4,1] = 'nBulk3'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 10
laFileStru[lnFileStru+4,4] = 0

laFileStru[lnFileStru+5,1] = 'nBulk4'
laFileStru[lnFileStru+5,2] = 'N'
laFileStru[lnFileStru+5,3] = 10
laFileStru[lnFileStru+5,4] = 0

laFileStru[lnFileStru+6,1] = 'nBulk5'
laFileStru[lnFileStru+6,2] = 'N'
laFileStru[lnFileStru+6,3] = 10
laFileStru[lnFileStru+6,4] = 0

laFileStru[lnFileStru+7,1] = 'nBulk6'
laFileStru[lnFileStru+7,2] = 'N'
laFileStru[lnFileStru+7,3] = 10
laFileStru[lnFileStru+7,4] = 0

laFileStru[lnFileStru+8,1] = 'nBulk7'
laFileStru[lnFileStru+8,2] = 'N'
laFileStru[lnFileStru+8,3] = 10
laFileStru[lnFileStru+8,4] = 0

laFileStru[lnFileStru+9,1] = 'nBulk8'
laFileStru[lnFileStru+9,2] = 'N'
laFileStru[lnFileStru+9,3] = 10
laFileStru[lnFileStru+9,4] = 0

laFileStru[lnFileStru+10,1] = 'nTotBulk'
laFileStru[lnFileStru+10,2] = 'N'
laFileStru[lnFileStru+10,3] = 10
laFileStru[lnFileStru+10,4] = 0

*-- For Average price
laFileStru[lnFileStru+11,1] = 'llDif_Prce'
laFileStru[lnFileStru+11,2] = 'L'
laFileStru[lnFileStru+11,3] = 1
laFileStru[lnFileStru+11,4] = 0

*-- For season
laFileStru[lnFileStru+12,1] = 'Season'
laFileStru[lnFileStru+12,2] = 'C'
laFileStru[lnFileStru+12,3] = 6
laFileStru[lnFileStru+12,4] = 0

*-- For ship Via
laFileStru[lnFileStru+13,1] = 'Shipvia'
laFileStru[lnFileStru+13,2] = 'C'
laFileStru[lnFileStru+13,3] = 6
laFileStru[lnFileStru+13,4] = 0

laFileStru[lnFileStru+14,1] = 'MPiktkt'
laFileStru[lnFileStru+14,2] = 'M'
laFileStru[lnFileStru+14,3] = 10
laFileStru[lnFileStru+14,4] = 0


laFileStru[lnFileStru+15,1] = 'MStore'
laFileStru[lnFileStru+15,2] = 'M'
laFileStru[lnFileStru+15,3] = 10
laFileStru[lnFileStru+15,4] = 0

laFileStru[lnFileStru+16,1] = 'MOrder'
laFileStru[lnFileStru+16,2] = 'M'
laFileStru[lnFileStru+16,3] = 10
laFileStru[lnFileStru+16,4] = 0

laFileStru[lnFileStru+17,1] = 'MAccount'
laFileStru[lnFileStru+17,2] = 'M'
laFileStru[lnFileStru+17,3] = 10
laFileStru[lnFileStru+17,4] = 0

laFileStru[lnFileStru+18,1] = 'CgroupNo'
laFileStru[lnFileStru+18,2] = 'C'
laFileStru[lnFileStru+18,3] = 4
laFileStru[lnFileStru+18,4] = 0

laFileStru[lnFileStru+19,1] = 'cType'
laFileStru[lnFileStru+19,2] = 'C'
laFileStru[lnFileStru+19,3] = 1
laFileStru[lnFileStru+19,4] = 0

*B606156,1 BWA 06/24/2002 Add the new field for the No of stores.[START]
*-- For the No of stores to print in the header.
laFileStru[lnFileStru+20,1] = 'NoofStor'
laFileStru[lnFileStru+20,2] = 'N'
laFileStru[lnFileStru+20,3] = 5
laFileStru[lnFileStru+20,4] = 0
*B606156,1 BWA 06/24/2002.[END]

*B606277,1 ABD - add new field. [Begin]
laFileStru[lnFileStru+21,1] = 'cStyOrder'
laFileStru[lnFileStru+21,2] = 'C'
laFileStru[lnFileStru+21,3] = 5
laFileStru[lnFileStru+21,4] = 0
*B606277,1 ABD - [End]

*B606431,1 ABD - Add new field 'Dist_ctr' [Begin]
laFileStru[lnFileStru+22,1] = 'Dist_ctr'
laFileStru[lnFileStru+22,2] = 'C'
laFileStru[lnFileStru+22,3] = 8
laFileStru[lnFileStru+22,4] = 0
*B606431,1 ABD - [End]


CREATE TABLE (gcWorkDir+lcOrdLTmp) FROM ARRAY laFileStru
INDEX ON ALLTRIM(cBulkQty)+  STYLE + ALLTRIM(CgroupNo)+Shipvia  TAG (lcBulkQty);
        OF (gcWorkDir+lcBulkQty+'.CDX')

*-- This index use when print the form on printer only.
INDEX ON PikTkt TAG (lcTmpOrdU) UNIQUE ADDITIVE
*-- If we will Print Pick Ticket By Style number.
INDEX ON ALLTRIM(CgroupNo)+ShipVia TAG (lcOrdLTmp1) ADDITIVE
INDEX ON Style + ALLTRIM(CgroupNo) TAG (lcOrdLTmp) ADDITIVE

*B606277,1 ABD - create new index on ALLTRIM(cBulkQty)+STYLE. [Begin]
INDEX ON ALLTRIM(cBulkQty)+STYLE + Store TAG (lcOrdLTmp2) ADDITIVE
*B606277,1 ABD - [End]

IF USED(lcTmpBulk)
  SELECT (lcTmpBulk)
  ZAP
ELSE
  *-- new table to hold the bulk order.
  DIMENSION laFileStru[12, 4]

  laFileStru[1,1] = 'cBulkQty'
  laFileStru[1,2] = 'C'
  laFileStru[1,3] = 50
  laFileStru[1,4] = 0

  laFileStru[2,1] = 'nBulk1'
  laFileStru[2,2] = 'N'
  laFileStru[2,3] = 10
  laFileStru[2,4] = 0
 
  laFileStru[3,1] = 'nBulk2'
  laFileStru[3,2] = 'N'
  laFileStru[3,3] = 10
  laFileStru[3,4] = 0

  laFileStru[4,1] = 'nBulk3'
  laFileStru[4,2] = 'N'
  laFileStru[4,3] = 10
  laFileStru[4,4] = 0

  laFileStru[5,1] = 'nBulk4'
  laFileStru[5,2] = 'N'
  laFileStru[5,3] = 10
  laFileStru[5,4] = 0

  laFileStru[6,1] = 'nBulk5'
  laFileStru[6,2] = 'N'
  laFileStru[6,3] = 10
  laFileStru[6,4] = 0

  laFileStru[7,1] = 'nBulk6'
  laFileStru[7,2] = 'N'
  laFileStru[7,3] = 10
  laFileStru[7,4] = 0

  laFileStru[8,1] = 'nBulk7'
  laFileStru[8,2] = 'N'
  laFileStru[8,3] = 10
  laFileStru[8,4] = 0

  laFileStru[9,1] = 'nBulk8'
  laFileStru[9,2] = 'N'
  laFileStru[9,3] = 10
  laFileStru[9,4] = 0

  laFileStru[10,1] = 'nTotBulk'
  laFileStru[10,2] = 'N'
  laFileStru[10,3] = 10
  laFileStru[10,4] = 0
 
  laFileStru[11,1] = 'Style'
  laFileStru[11,2] = 'C'
  laFileStru[11,3] = 19
  laFileStru[11,4] = 0

  laFileStru[12,1] = 'Shipvia'
  laFileStru[12,2] = 'C'
  laFileStru[12,3] = 6
  laFileStru[12,4] = 0
  CREATE TABLE (gcWorkDir+lcTmpBulk) FROM ARRAY laFileStru
ENDIF

INDEX ON Style+ALLTRIM(cBulkQty)+Shipvia TAG (lcTmpBulk) OF (gcWorkDir+lcTmpBulk+'.CDX')

*-- new table to hold the bulk order.
DIMENSION laFileStru[2, 4]

laFileStru[1,1] = 'Mstores'
laFileStru[1,2] = 'M'
laFileStru[1,3] = 10
laFileStru[1,4] = 0

laFileStru[2,1] = 'CgroupNo'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 3
laFileStru[2,4] = 0

IF USED(lcTmpGrp)
  SELECT (lcTmpGrp)
  ZAP
ELSE
  CREATE TABLE (gcWorkDir+lcTmpGrp) FROM ARRAY laFileStru
ENDIF
INDEX ON ALLTRIM(CgroupNo) TAG (lcTmpGrp) OF (gcWorkDir+lcTmpGrp+'.CDX')

SELECT (lnOldAls)

*-- End OF lfCrTmpFls.
*:****************************************************************
*: Name        : lfUpStores
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 02/11/2001
*: Purpose     : Function update stores with new element.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfUpStores ()
*:****************************************************************
*
FUNCTION lfUpStores
PRIVATE lnAlias , lnLenArray , lnOlrREcNo ,lcTpOrder
STORE 0  To lnAlias , lnOlrREcNo
STORE '' To lcTpOrder

lnAlias = SELECT (0)

*-- Update the header recored.
SELECT (lcOrdLTmp)
lnOlrREcNo = RECNO()
lcTpOrder = ORDER()
SET ORDER TO lcOrdLTmp1
IF SEEK(lcGroupNo+lcShipVia)
  *-- Update the Header recored.
  IF ATC(EVAL(lcTmpOrdL+'.Store'),Eval(lcOrdLTmp+'.MStore')) = 0
    REPLACE Mstore WITH Mstore+ IIF(Empty(Mstore),'',',') + ALLTRIM(EVAL(lcTmpGrp+'.Mstores'))
  ENDIF
  IF ATC(EVAL(lcTmpOrdL+'.Piktkt'),Eval(lcOrdLTmp+'.MPiktkt')) = 0
    REPLACE MPiktkt   WITH MPiktkt + IIF(Empty(MPiktkt),'',',') + EVAL(lcTmpOrdL+'.Piktkt') + '-' + ALLTRIM(EVAL(lcTmpOrdL+'.Store'))
  ENDIF
              
  *-- Update the memo Order field,
  IF ATC(OrdLine.Order,Eval(lcOrdLTmp+'.Morder')) = 0
    *-- Update memo order fields.
    REPLACE MOrder WITH MOrder + IIF(Empty(MOrder),'',',') + OrdLine.Order
   ENDIF
  
  *-- Update the memo account field
  IF ATC(OrdLine.Account,Eval(lcOrdLTmp+'.MAccount')) = 0
    *-- Update memo order fields.
    REPLACE Maccount WITH MAccount + IIF(Empty(MAccount),'',',') + ALLTRIM(OrdLine.Account)
  ENDIF
ENDIF

SELECT (lcOrdLTmp) 
SET ORDER TO &lcTpOrder
GOTO lnOlrREcNo

SELECT (lnAlias)
*-- End Of lfUpStores
*:****************************************************************
*: Name        : lfInsrtRec
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 02/11/2001
*: Purpose     : Function update stores with new element.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfInsrtRec ()
*:****************************************************************
*
FUNCTION lfInsrtRec
*-- If we didn't define this param it will pe false
*-- else it will come true
PRIVATE W
W = 0


FOR W = 1 To 8 
  V = ALLTRIM(STR(W,1))
  REPLACE  nBulk&V WITH M.Qty&V
ENDFOR
      
*-- Update the Total lines.
REPLACE nTotBulk  WITH M.TotQty ,;
        Season    WITH ORDHDR.Season  ,;
        Shipvia   WITH lcShipVia      ,;
        CgroupNo  WITH lcGroupNo

*-- End OF lfInsrtRec
*:****************************************************************
*==========
FUNCTION lfGetOrd
*--C#102332,4 IAA
SELECT (lcTmpOrdL)
INDEX ON ORDER TAG  (lcOrdIndx) ADDITIVE

  SCAN
     IF SEEK(&lcTmpOrdL..Order, lcTempStyle)
       SCATTER MEMVAR MEMO
       SELECT (lcOrdLTmp)
       APPEND BLANK       
       GATHER MEMVAR MEMO
       REPLACE cType with '1'
     ENDIF
  ENDSCAN
  
SELECT (lcTmpOrdL)
USE IN (lcTmpOrdL)

lcTmpOrdL = lcOrdLTmp

*-- End of lfGetOrd

FUNCTION lfBulkQty
*--C#102332,4 IAA
PRIVATE lcBulkTb
*-- sums the qty per style and appends them to the original table
lcBulkTb = gcWorkDir + '\'  + gfTempName()
SELECT 'zzzzzz' AS order, ;
       style,                 ;
       scale,                 ;
       SUM(qty1) AS qty1,     ;
       SUM(qty2) AS qty2,     ;
       SUM(qty3) AS qty3,     ;
       SUM(qty4) AS qty4,     ;
       SUM(qty5) AS qty5,     ;
       SUM(qty6) AS qty6,     ;
       SUM(qty7) AS qty7,     ;
       SUM(qty8) AS qty8,     ;
       SUM(pik1) AS pik1,     ;
       SUM(pik2) AS pik2,     ;
       SUM(pik3) AS pik3,     ;
       SUM(pik4) AS pik4,     ;
       SUM(pik5) AS pik5,     ;
       SUM(pik6) AS pik6,     ;
       SUM(pik7) AS pik7,     ;
       SUM(pik8) AS pik8,     ;
       SUM(totPik) AS totPik, ;
       SUM(totQty) AS totQty, ;
       '2' AS cType           ;
FROM   (lcTmpOrdL) ;
GROUP BY style ;
INTO TABLE (lcBulkTb)
SELECT (lcTmpOrdL)
APPEND FROM (lcBulkTb)

*--End of lfBulkQty
*B606156,1 BWA 07/03/2002 Function to break the old relations.[START]
*!*************************************************************
*! Name      : lfBrakRel
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/01/2002
*! Purpose   : Break the relation of the temp file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBrakRel()
*!*************************************************************
FUNCTION lfBrakRel
PRIVATE lcAlasCd

lcAlasCd = SELECT()
SELECT (lcTmpOrdL)
SET RELATION TO
SELECT(lcAlasCd)

*--End of LfBrakRel.
*B606156,1 BWA 07/03/2002.[END]
*:****************************************************************
*: Name        : lfInsrtRec
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 02/11/2001
*: Purpose     : Function update stores with new element.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfInsrtRec ()
*:****************************************************************
*B606277,1 ABD - [Begin]
FUNCTION lfReArgRec
PRIVATE lnAlias , lcStyle , lcStore , llFirstTim , lcStyleStr, lnSvMainRc;
        lcOldOrder
STORE '' TO lcStyle    , lcStore ,lcStyleStr 
STORE 0  TO lnstylordr , lnSvMainRc

llFirstTim = .T.
lnAlias = SELECT (0)
SELECT (lcTempords)

SELECT (lcTmpStors)
SCAN
  lcStore = Store
  SELECT (lcTmpStyle)
  SCAN
    lcStyle = style
    lcBulkQty = ALLTRIM(cBulkQty)
    SELECT (lcTmpStors)
    IF SEEK(lcStyle+lcStore+lcBulkQty,lcTempords)
      REPLACE mStyles WITH mStyles + 'X'
    ELSE
      REPLACE mStyles WITH mStyles + 'A'
    ENDIF
    SELECT (lcTmpStyle)
  ENDSCAN
ENDSCAN

*-- This SQL will show the memo files as a field in a table , this will help
*-- us to know the semilar lines store.
SELECT * ,LEFT(mStyles,255) as a,PADR(substr(mStyles,256,254),254) as b,;
       PADR(substr(mStyles,256,254),254) as c,PADR(substr(mStyles,256,254),254) as d,;
       PADR(substr(mStyles,256,254),254) as e,PADR(substr(mStyles,256,254),254) as f,;
       PADR(substr(mStyles,256,254),254) as g,PADR(substr(mStyles,256,254),254) as h,;
       PADR(substr(mStyles,256,254),254) as i,PADR(substr(mStyles,256,254),254) as j;
FROM  (lcTmpStors)                     ;
ORDER by a,b,c,d,e,f,g,h,i,j,store ASC;
INTO  DBF (gcWorkDir+lcTmpOrdSt)

SELECT(lcOrdLTmp)
lcOldOrder = ORDER()
SET ORDER TO (lcOrdLTmp2)

STORE '' TO lcLstStyls , lcStore
SELECT (lcTmpOrdSt)
SCAN
  llFirstTim = .T.
  lcStyleStr = ALLTRIM(A+B+C+D+E+F+G+H+I+J)
  IF OCCURS("X" ,  lcStyleStr)>=1
    FOR I = 1 To OCCURS("X" ,  lcStyleStr) + 1
      lnPosStyle = ATC('X',lcStyleStr)
      IF lnPosStyle = 0
        EXIT
      ELSE
        SELECT(lcTmpStyle)
        IF BETWEEN(lnPosStyle,1,RECCOUNT())
          GOTO lnPosStyle
          lcFrstStyl = Style
          lcBullkQty  = ALLTRIM(cBulkQty)
          SELECT(lcOrdLTmp)
          =SEEK(lcBullkQty +lcFrstStyl)
          SCATTER MEMVAR MEMO
          STORE '' TO m.MPiktkt , m.MStore , m.MOrder , m.MAccount
          *-- If enter for first time.
          IF llFirstTim
            llFirstTim = .F.
            SELECT (lcTempOrds)
            IF SEEK(lcFrstStyl+&lcTmpOrdSt..Store)
            
              *B606431,1 ABD - get the DC for the Current Customer store.  [Begin]
              llSameDC = .T.
              IF llRpGrpDC
                = SEEK('S'+ORDLINE.Account+&lcMainf..Store,'CUSTOMER')
                lcStorDC1 = CUSTOMER.Dist_ctr
                = SEEK('S'+ORDLINE.Account+&lcTmpOrdSt..Store,'CUSTOMER')
                lcStorDC2 = CUSTOMER.Dist_ctr
                llSameDC = (lcStorDC1 = lcStorDC2)
              ENDIF
              *B606431,1 ABD - [End]
              
              *- Get memo fields from the main record.
              *B606431,1 ABD - Remark the next line and add lsamedc for the condation. [Begin]
              *IF lcLstStyls = lcStyleStr
              IF lcLstStyls = lcStyleStr .AND. llSameDC
                *B606431,1 ABD - [End]
                
                SELECT (lcMainF)
                GOTO lnSvMainRc
                SCATTER Fields Mstore,MPiktkt,MOrder,Maccount MEMO MEMVAR
                lcStore = &lcMainF..Store
              ELSE
                lcStore = &lcTmpOrdSt..Store
                lnstylordr = lnstylordr + 1   
              ENDIF


              m.Mstore   = m.Mstore+ IIF(Empty(m.Mstore),'',',') + ALLTRIM(EVAL(lcTempOrds+'.store'))
              IF ATC(EVAL(lcTempOrds+'.Piktkt'),m.MPiktkt) = 0
                m.MPiktkt  = m.MPiktkt + IIF(Empty(m.MPiktkt),'',',') + EVAL(lcTempOrds+'.Piktkt') + '-' ;
                + ALLTRIM(EVAL(lcTempOrds+'.Store'))
              ENDIF
                  
              *-- Update the memo Order field,
              IF ATC(EVAL(lcTempOrds+'.Order'),m.Morder) = 0
                *-- Update memo order fields.
                m.MOrder =  m.MOrder + IIF(Empty(m.MOrder),'',',') + ALLTRIM(&lcTempOrds..Order)
              ENDIF
              
              *-- Update the memo account field
              IF SEEK('O'+Order+STR(LineNo,6),'Ordline') .AND. ATC(ORDLINE.Account,m.MAccount) = 0
                *-- Update memo order fields.
                m.Maccount = m.MAccount + IIF(Empty(m.MAccount),'',',') + ALLTRIM(Ordline.Account)
              ENDIF
              
            ENDIF
            lcPiktStor  =  m.MPiktkt
            M.NoofStor = OCCURS("," ,  lcPiktStor) + 1
            m.cStyOrder = STR(lnstylordr,5)
              
            SELECT (lcMainF)
            IF lcLstStyls = lcStyleStr
              GOTO lnSvMainRc
              
              *B606431,1 ABD - Add check for the same DC. [Begin]
              IF llSameDC
                *B606431,1 ABD - [End]
                REPLACE Mstore    WITH M.Mstore   ,;
                        MPiktkt   WITH M.MPiktkt  ,;
                        MOrder    WITH M.MOrder   ,;
                        Maccount  WITH M.Maccount ,;
                        NoofStor  WITH M.NoofStor ,;
                        cStyOrder WITH M.cStyOrder,;
                        Store     WITH lcStore
               *B606431,1 ABD - Else For If Statment. [Begin]
              ELSE
                lcLstStyls = lcStyleStr
                m.store = lcStore
                APPEND BLANK
                GATHER MEMVAR MEMO
                lnSvMainRc = RECNO()
              ENDIF       
              *B606431,1 ABD - [End]
            ELSE
              lcLstStyls = lcStyleStr
              m.store = lcStore
              APPEND BLANK
              GATHER MEMVAR MEMO
              lnSvMainRc = RECNO()
            ENDIF
          ELSE
            *-- Check if this record exist before that.
            m.cStyOrder = STR(lnstylordr,5)
            *B606431,1 ABD - Add check for the same DC . [Begin]
            *IF !SEEK(M.Style+lcStore+M.cStyOrder+ALLTRIM(m.cBulkQty),lcMainF)
            IF !SEEK(M.Style+lcStore+M.cStyOrder+ALLTRIM(m.cBulkQty),lcMainF) .OR. !llSameDC
              *B606431,1 ABD - [End]
              m.store = lcStore
              INSERT INTO (lcMainF) FROM MEMVAR
            ENDIF  
          ENDIF
          
          SELECT (lcTempOrds)
        ENDIF
        *-- this line will remove the X from the current record.
        lcStyleStr = STRTRAN(lcStyleStr,'X','A',1,1)
      ENDIF
    ENDFOR
  ENDIF
ENDSCAN

*-- Delete all line to enter new lines.
SELECT (lcOrdLTmp)
DELETE ALL
SET ORDER TO &lcOldOrder


STORE '' TO lcmStores,lcmPikt,lcMorder,lcMAccount,lcNoStors,lcSeasons
SELECT (lcMainf)
SET ORDER TO lcMainF
*-- Add all lines on main file to the old custom files.
SCAN 
  SCATTER MEMVAR MEMO
  *-- Add the same stores to the header in case different Season.
  IF !EMPTY(m.Mstore)
    lcmStores = M.Mstore
    lcmPikt   = M.MPiktkt
    lcMorder  = M.MOrder
    lcMAccount= M.MAccount
    lcNoStors = M.NoofStor
    lcSeasons  = Left(M.Style,3)
  ELSE
    IF lcSeasons # Left(M.Style,3) 
      M.Mstore   = lcmStores
      M.MPiktkt  = lcmPikt
      M.MOrder   = lcMorder
      M.MAccount = lcMAccount
      M.NoofStor = lcNoStors
      lcSeasons  = Left(M.Style,3)
    ENDIF
  ENDIF
  *-ABD1
  IF llRpGrpDC .AND. !EMPTY(STORE)
    = SEEK('O'+Order+STR(LineNo,6),'Ordline')
    = SEEK('S'+Ordline.account+m.Store,'CUSTOMER')
    M.Dist_ctr = CUSTOMER.Dist_ctr
  ENDIF
  *-ABD1
  INSERT INTO (lcOrdLTmp) FROM MEMVAR
ENDSCAN

IF USED(lcTmpStyle)
  USE IN (lcTmpStyle)
  ERASE (gcWorkDir+lcTmpStyle+'.DBF')
  ERASE (gcWorkDir+lcTmpStyle+'.CDX')
ENDIF  


IF USED(lcTmpStors)
  USE IN (lcTmpStors)
  ERASE (gcWorkDir+lcTmpStors+'.DBF')
  ERASE (gcWorkDir+lcTmpStors+'.CDX')
ENDIF  

IF USED(lcTempOrds)
  USE IN (lcTempOrds)
  ERASE (gcWorkDir+lcTempOrds+'.DBF')
  ERASE (gcWorkDir+lcTempOrds+'.CDX')
ENDIF  

IF USED(lcTmpOrdSt)
  USE IN (lcTmpOrdSt)
  ERASE (gcWorkDir+lcTmpOrdSt+'.DBF')
  ERASE (gcWorkDir+lcTmpOrdSt+'.CDX')
ENDIF  

IF USED(lcMainF)
  USE IN (lcMainF)
  ERASE (gcWorkDir+lcMainF+'.DBF')
  ERASE (gcWorkDir+lcMainF+'.CDX')
ENDIF  

SELECT(lnAlias)

*-- End OF lfReArgRec
*B606277,1 ABD - [End]
*:****************************************************************
