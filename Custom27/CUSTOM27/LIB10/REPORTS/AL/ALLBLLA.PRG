*:***************************************************************************
*: Program file  : ALLBLLA.PRG
*: Program desc. : CUSTOM LABEL FOR LIBERTY APPAREL.
*! Date          : 10/27/2004
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALLBLLA
*:***************************************************************************
*: This Report Program is due to C123708 ...
*:***************************************************************************
*: Modification:
*:***************************************************************************

STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl
DECLARE laFrmAdr[3,1] , laToAdrs[3,1] , laSz[8,1]
lcSolTNam = ''           && Variable to hold the Sold to name
lcShpTNam = ''           && Variable to hold the Ship to name
laFrmAdr  = ''           && Array to hold the From address
laToAdrs  = ''           && Array to hold the To address
laSz      = ''           && Array to hold the size scale from scale file.





=lfChkStrct()  && Get the length of the style and color.
=lfCreatTmp()  && Create work cursor.
=lfRelatons()  && Create the used relation.

*--bas
*-- Get the Variables its Initial value
STORE SPACE(0) TO lcCustpo
=lfvCrATVar()
*--bas

=lfCollData()  && Collect the data.

SELECT (lcPikLins)
SET ORDER TO TAG PikIndex
LOCATE

*EIH 05/08/2005 Fix bug that the user want to print lables of the selected Piktkt number of 
*               times [Begin]. 
*Create another cursor to store the required number of times .
lcPikLins2 = gfTempName()
CREATE CURSOR (lcPikLins2) (PiktktNo C(6)  , Custpo C(15)   , DpartFld C(5)  , StylFld C(lnStyLnGl)    ,;
                           ShipVFld C(20) , PageNo N(5)    , TotQty N(5)    , ClrFld C(lnClrLnGl)     ,;
                           AdrsFrm1 C(30) , AdrsFrm2 C(30) , AdrsFrm3 C(30) , AddrsTO1 C(30)          ,;
                           AddrsTO2 C(30) , AddrsTO3 C(30) , Qty1 N(6)      , Qty2 N(6)   , Qty3 N(6) ,;
                           Qty4 N(6)      , Qty5 N(6)      , Qty6 N(6)      , Qty7 N(6)   , Qty8 N(6) ,;
                           Siz1 C(5)      , Siz2 C(5)      , Siz3 C(5)      , Siz4 C(5)   , Siz5 C(5) ,;
                           Siz6 C(5)      , Siz7 C(5)      , Siz8 C(5)      , SkuNo C(16) , VPrp N(6) ,;
                           PagIndx N(5)   , SolTNam C(30)  , ShpTNam C(30) ,PagIndx2 N(5) ) 

INDEX ON PiktktNo + ALLTRIM(STR(PagIndx)+STR(PagIndx2)) TAG PikIndex2 of (gcWorkDir + lcPikLins2)
SELECT (lcPikLins)
LOCATE
SCAN 
  REPLACE PagIndx2 WITH 1
  SCATTER MEMVAR MEMO
  IF SEEK(M.PiktktNo, lclblTbl )
    FOR I = 2 TO &lclblTbl..LABELS
      M.PagIndx2 = I
      SELECT (lcPikLins2)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDFOR
  ENDIF
  SELECT (lcPikLins)
ENDSCAN
SELECT (lcPikLins2)
SCAN
  SCATTER MEMVAR MEMO
  INSERT INTO (lcPikLins) FROM MEMVAR
  SELECT (lcPikLins2)
ENDSCAN
SELECT (lcPikLins)
COUNT TO lnCountRec
REPLACE ALL PageNo WITH lnCountRec

LOCATE
*EIH 05/08/2005 [End]. 

DO gfDispRe WITH EVAL('lcFormName')
WAIT CLEAR
                 *--Section for the functions of the style color filters --*
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/27/2004
*! Purpose   : Validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*--End of lfvStyle. 
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/27/2004
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*--End of lfMajTtGet.
*!*************************************************************
*! Name      : lfNonMaj
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/27/2004
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

*--Compute Free/Color Items in Style Structure.
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

*--End of lfNonMaj.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/27/2004
*! Purpose   : Get major seg. picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*--End of lfMajPic.
*!**************************************************************************
*! Name      : lfSetSTY
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/27/2004
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
   GO TOP IN STYLE
  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE

*--End of lfSetSTY.
                 *--Section for the functions of the style color filters --*

*!**************************************************************************
*! Name      : lfSetDept
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 10/27/2004
*! Purpose   : Go top in the Dept# IN RANGE
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
*! Example   : =lfSetDept()
*!**************************************************************************
FUNCTION lfSetDept
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   GO TOP IN ORDHDR
ENDCASE

*--End of lfSetDept.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/27/2004
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/27/2004
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  
  *--IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array => FOR lnCount = 1 TO 5
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *--IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*--End of lfAdrShift.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/27/2004
*! Purpose   : Create Temp. file that hold the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : Report code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp
PRIVATE lcAlias

*--PagIndx : I used this field only to print each label in 1 page not to sort the piktkt records.

lcAlias = ALIAS()





*EIH 05/08/2005 Fix bug that the user want to print lables of the selected Piktkt number of 
*               times [Begin]. 
*ADD PagIndx2 TO INDEX FOR REPEATED PRINT
*CREATE CURSOR (lcPikLins) (PiktktNo C(6)  , Custpo C(15)   , DpartFld C(5)  , StylFld C(lnStyLnGl)    ,;
                           ShipVFld C(20) , PageNo N(5)    , TotQty N(5)    , ClrFld C(lnClrLnGl)     ,;
                           AdrsFrm1 C(30) , AdrsFrm2 C(30) , AdrsFrm3 C(30) , AddrsTO1 C(30)          ,;
                           AddrsTO2 C(30) , AddrsTO3 C(30) , Qty1 N(6)      , Qty2 N(6)   , Qty3 N(6) ,;
                           Qty4 N(6)      , Qty5 N(6)      , Qty6 N(6)      , Qty7 N(6)   , Qty8 N(6) ,;
                           Siz1 C(5)      , Siz2 C(5)      , Siz3 C(5)      , Siz4 C(5)   , Siz5 C(5) ,;
                           Siz6 C(5)      , Siz7 C(5)      , Siz8 C(5)      , SkuNo C(16) , VPrp N(6) ,;
                           PagIndx N(5)   , SolTNam C(30)  , ShpTNam C(30))


CREATE CURSOR (lcPikLins) (PiktktNo C(6)  , Custpo C(15)   , DpartFld C(5)  , StylFld C(lnStyLnGl)    ,;
                           ShipVFld C(20) , PageNo N(5)    , TotQty N(5)    , ClrFld C(lnClrLnGl)     ,;
                           AdrsFrm1 C(30) , AdrsFrm2 C(30) , AdrsFrm3 C(30) , AddrsTO1 C(30)          ,;
                           AddrsTO2 C(30) , AddrsTO3 C(30) , Qty1 N(6)      , Qty2 N(6)   , Qty3 N(6) ,;
                           Qty4 N(6)      , Qty5 N(6)      , Qty6 N(6)      , Qty7 N(6)   , Qty8 N(6) ,;
                           Siz1 C(5)      , Siz2 C(5)      , Siz3 C(5)      , Siz4 C(5)   , Siz5 C(5) ,;
                           Siz6 C(5)      , Siz7 C(5)      , Siz8 C(5)      , SkuNo C(16) , VPrp N(6) ,;
                           PagIndx N(5)   , SolTNam C(30)  , ShpTNam C(30),PagIndx2 N(5))                           

*ADD PagIndx2 to saving the index on report
*INDEX ON PiktktNo + ALLTRIM(STR(PagIndx)) TAG PikIndex of (gcWorkDir + lcPikLins)
INDEX ON PiktktNo + ALLTRIM(STR(PagIndx)+STR(PagIndx2)) TAG PikIndex of (gcWorkDir + lcPikLins)

*EIH 05/08/2005 [End]. 

INDEX ON PiktktNo TAG PiktktNo of (gcWorkDir + lcPikLins)


SELECT(lcAlias)
*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfRelatons
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/27/2004
*! Purpose   : Function to create the used relation.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfRelatons()
*!*************************************************************
FUNCTION lfRelatons

SELECT PACK_LIN
SET RELATION TO PACK_LIN.PACK_NO                       INTO PACK_HDR ADDITIVE
SET RELATION TO "S" +  ORDLINE.ACCOUNT + ORDLINE.STYLE INTO SPCK_HDR ADDITIVE
SET RELATION TO STYLE                                  INTO STYLE ADDITIVE

SELECT STYLE
SET RELATION TO "S" + SCALE                            INTO SCALE ADDITIVE

SELECT PACK_HDR
SET RELATION TO "O" + PACK_HDR.ORDER                   INTO ORDHDR ADDITIVE
SET RELATION TO PIKTKT                                 INTO PIKTKT ADDITIVE
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                        'S' + Account + Store) INTO CUSTOMER ADDITIVE

SELECT ORDHDR
SET RELATION TO "O" + ORDHDR.ORDER                     INTO ORDLINE ADDITIVE


*--End of lfRelatons.
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/27/2004
*! Purpose   : Function to collect the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************
FUNCTION lfCollData

PRIVATE lnCrmntNo , lcShipVia , lcPikAdr , lcScalDiff
STORE SPACE(0) TO lcScalDiff , lcPikAdr
lnCrmntNo = 0
lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp) + " AND !EMPTY(PACK_HDR.PIKTKT) AND !('****' $ PACK_HDR.PIKTKT)"

SELECT PACK_LIN
SCAN FOR &lcRpExp
  
  WAIT WINDOW 'Selecting Records For The Report ...' + PACK_HDR.PIKTKT + " Line No" + ALLTRIM(STR(PACK_LIN.LINE_NO)) NOWAIT
  lcShipVia = ''                                    && Variable to hold the Ship Via Description.
  lcShipVia = ALLTRIM(gfCodDes(PACK_HDR.ShipVia , 'SHIPVIA'))
  lnCrmntNo = lnCrmntNo + 1
  
  *-- Collect the scale values in case the scale is changed.
  IF lcScalDiff # SCALE.SCALE
    laSz     = ''
    FOR I = 1 TO 8
      lcSize  = ALLTRIM(STR(I))
      laSz[I] = ALLTRIM(SCALE.SZ&lcSize)
    ENDFOR
  ENDIF
  lcScalDiff = SCALE.SCALE

  *-- Collect the address values in case the piktkt is changed.
  IF lcPikAdr # PACK_HDR.PIKTKT
    =lfFrmToAdr()
  ENDIF
  lcPikAdr = PACK_HDR.PIKTKT

  SELECT (lcPikLins)
  APPEND BLANK
  *EIH 05/08/2005 Fix bug that Replace valid SKUNO [Begin]. 
  *REPLACE PiktktNo WITH PIKTKT.PIKTKT                                   ,;
          Custpo   WITH ALLTRIM(PIKTKT.CUSTPO)                          ,;
          DpartFld WITH ORDHDR.DEPT                                     ,;
          ShipVFld WITH lcShipVia                                       ,;
          SkuNo    WITH SPCK_HDR.PACK_ID                                ,;
          PagIndx  WITH lnCrmntNo                                       ,;
          StylFld  WITH SUBSTR(PACK_LIN.STYLE , lnStyPosGl , lnStyLnGl) ,;
          ClrFld   WITH SUBSTR(PACK_LIN.STYLE , lnClrPosGl , lnClrLnGl) ,;
          VPrp     WITH PACK_LIN.TOTQTY
   REPLACE PiktktNo WITH PIKTKT.PIKTKT                                   ,;
          Custpo   WITH ALLTRIM(PIKTKT.CUSTPO)                          ,;
          DpartFld WITH ORDHDR.DEPT                                     ,;
          ShipVFld WITH lcShipVia                                       ,;
          SkuNo    WITH IIF(SEEK('S'+ORDHDR.ACCOUNT+PACK_LIN.STYLE,'SPCK_HDR'),SPCK_HDR.PACK_ID,''),;
          PagIndx  WITH lnCrmntNo                                       ,;
          StylFld  WITH SUBSTR(PACK_LIN.STYLE , lnStyPosGl , lnStyLnGl) ,;
          ClrFld   WITH SUBSTR(PACK_LIN.STYLE , lnClrPosGl , lnClrLnGl) ,;
          VPrp     WITH PACK_LIN.TOTQTY

  *EIH 05/08/2005 [END]. 
  REPLACE Qty1   WITH PACK_LIN.Qty1   ,;
          Qty2   WITH PACK_LIN.Qty2   ,;
          Qty3   WITH PACK_LIN.Qty3   ,;
          Qty4   WITH PACK_LIN.Qty4   ,;
          Qty5   WITH PACK_LIN.Qty5   ,;
          Qty6   WITH PACK_LIN.Qty6   ,;
          Qty7   WITH PACK_LIN.Qty7   ,;
          Qty8   WITH PACK_LIN.Qty8   ,;
          TotQty WITH PACK_LIN.TotQty

  REPLACE Siz1 WITH laSz[1] ,;
          Siz2 WITH laSz[2] ,;
          Siz3 WITH laSz[3] ,; 
          Siz4 WITH laSz[4] ,;
          Siz5 WITH laSz[5] ,;
          Siz6 WITH laSz[6] ,;
          Siz7 WITH laSz[7] ,;
          Siz8 WITH laSz[8]

  REPLACE AdrsFrm1 WITH laFrmAdr[1] ,;
          AdrsFrm2 WITH laFrmAdr[2] ,;
          AdrsFrm3 WITH laFrmAdr[3] ,;
          AddrsTO1 WITH laToAdrs[1] ,;
          AddrsTO2 WITH laToAdrs[2] ,;
          AddrsTO3 WITH laToAdrs[3] ,;
          SolTNam  WITH lcSolTNam   ,;
          ShpTNam  WITH lcShpTNam
ENDSCAN
SELECT (lcPikLins)
COUNT TO lnCountRec
REPLACE ALL PageNo WITH lnCountRec

*--End of lfCollData.
*!*************************************************************
*! Name      : lfFrmToAdr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/27/2004
*! Purpose   : Function to collect the data for the Address.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfFrmToAdr()
*!*************************************************************
FUNCTION lfFrmToAdr
PRIVATE lcAlias , lcKey , lcDistCntr

DECLARE laFrmAdr[3,1] , laToAdrs[3,1]
laFrmAdr  = ''           && Array to hold the From address
laToAdrs  = ''           && Array to hold the To address
lcSolTNam = ''           && Variable to hold the Sold to name
lcShpTNam = ''           && Variable to hold the Ship to name

lcAlias = SELECT(0)
SELECT CUSTOMER
lcKey = EVAL(KEY())

SEEK IIF(EMPTY(PIKTKT.STORE),'M','S')+ PIKTKT.Account + PIKTKT.STORE 
lcSolTNam   = CUSTOMER.BTName
laFrmAdr[1] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2'))
laFrmAdr[2] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2'))
laFrmAdr[3] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')) + ;
              ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')) + ;
              ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2'))

=lfAdrShift('laFrmAdr')

IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
  lcShpTNam   = ORDHDR.STName
  laToAdrs[1] = ALLTRIM(cAddress1)
  laToAdrs[2] = ALLTRIM(cAddress2)
  laToAdrs[3] = ALLTRIM(cAddress3) + ALLTRIM(cAddress4) + ALLTRIM(cAddress5)

ELSE

  SELECT CUSTOMER
  *--If there is a distribution center.
  IF !EMPTY(CUSTOMER.Dist_Ctr)
    =SEEK('S' + PIKTKT.Account + CUSTOMER.Dist_Ctr , 'CUSTOMER')
  ENDIF

  lcShpTNam   = IIF(EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA)
  laToAdrs[1] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laToAdrs[2] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laToAdrs[3] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 3)) +;
                ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 4)) +;
                ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5))
ENDIF
=lfAdrShift('laToAdrs')

=SEEK(lcKey)
SELECT(lcAlias)
*--End of lfFrmToAdr.
*!*************************************************************
*! Name      : lfFillAll
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 7/06/2005
*! Purpose   : To fill the custpo from the ORDHDR file.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfFillAll()
*!*************************************************************
FUNCTION lfFillAll

DIMENSION laRpSCstpo[1,1],laRpTCstpo[1,1]
STORE '' TO laRpSCstpo,laRpTCstpo

*--The Custpo.
SELECT ORDHDR
SELECT DISTINCT Custpo FROM ORDHDR WHERE !EMPTY(Custpo) INTO ARRAY laRpSCstpo

*-- End of lfFillAll.
*!*************************************************************
*! Name      : lfvCstpo
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 7/06/2005
*! Purpose   : Function to call the mover that show the custpo.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvCstpo()
*!*************************************************************
FUNCTION lfvCstpo

=gfMover(@laRpSCstpo,@laRpTCstpo,'Cust P.O#',.T.,'')

*--End of lfvCstpo.
*!*************************************************************
*! Name      : lfvCrATVar
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 7/06/2005
*! Purpose   : Function to add the custpo variable to the LcRpExp.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvCrATVar

*--The Custpo.
FOR lnInd = 1 TO ALEN(laRpTCstpo)
  lcCustpo = lcCustpo + PADR(laRpTCstpo[lnInd],15) + ' | '
ENDFOR
lcCustpo = IIF(ALLTRIM(lcCustpo) = '|','',lcCustpo)

IF !EMPTY(lcCustpo)
  lcRpExp = lcRpExp + ' AND ALLTRIM(ORDHDR.CUSTPO) $ lcCustpo'
ENDIF
  
*-- End of lfvCrATVar.




*EIH 05/08/2005 Fix bug that the user want to print lables of the selected Piktkt number of 
*               times [Begin]. 
*!*************************************************************
*! Name      : lfPTLBLLA
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 08/05/2005
*! Purpose   : control browsing (PikTkt) and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPTLBLLA()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfPTLBLLA 
PARAMETERS lcParm
PRIVATE lnCurrAls 
lnCurrAls = SELECT(0)  && Save Active Alias
DO CASE   
  CASE lcParm = 'V'  && Reset code
    lcPkTik  = PIKTKT.PIKTKT
    lnLabels = 1  
    IF !cbdspdlg   
      PUSH KEY
      ON KEY
      *-- Call the screen to edit the number of labels
      DO (GCREPHOME+'AL\ALLBLLA.SPR') WITH ;
         'Pick Ticket ' + PIKTKT.PIKTKT
    ENDIF
    SELECT (lclblTbl)
    *-- Update table with the new number of labels entered by user.
    IF SEEK(lcPkTik)
      REPLACE LABELS WITH lnLabels
    ELSE
      APPEND BLANK
      REPLACE PIKTKT WITH lcPkTik    ,;
              LABELS WITH lnLabels
    ENDIF
    SELECT (lnCurrAls)
    =SEEK(lcPkTik)      
ENDCASE
SELECT (lnCurrAls)
POP KEY    
*--------END OF lfPTLBLLA()
*!*************************************************************
*! Name      : lfvCancel
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 08/05/2005
*! Purpose   : Valid function of the Cancel button of the edit 
*!             number of labels screen
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : ALLBLLA.SPR
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCancel()
*!*************************************************************
FUNCTION lfvCanLoc
=SEEK(PIKTKT.PIKTKT)
*---------END OF lfvCancel()
*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 08/05/2005
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfTempName()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen
lclblTbl  = gfTempName()
CREATE TABLE (gcWorkDir+lclblTbl) (PIKTKT C(6),LABELS N(4))
INDEX ON PIKTKT TAG (lclblTbl)

*---------END OF lfwOGWhen()

*EIH 05/08/2005 [End]. 