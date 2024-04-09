*:***************************************************************************
*: Program file  : ARPINVIW.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR Henry Segal (Hen02)
*: Date          : 03/04/2010
*: System        : Aria4xp.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Mariam Mazhar (MMT)
*: Track no      : C201222(A4) C201221(A27) - [T20100222.0017] 
*:***************************************************************************
*Modifications:
*B609443,1 MMT 11/10/2010 Fix bugs reported by customer in report layout[T20101019.0010]
*B609469,1 MMT 11/28/2010 Fix bug of cannot print report from the invoice screen[T20101027.0016]
*B611166,1 MMT 07/13/2016 Custom Invoice form HS does not export invoice detail to XLS or XML[T20160622.0012]
*:***************************************************************************
 
*B609469,1 MMT 11/28/2010 Fix bug of cannot print report from the invoice screen[Start]
*lcRpPrSt = IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt)
lcRpPrSt = IIF(oAriaApplication.ProcessID = 'ARPINV',IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt),"")
*B609469,1 MMT 11/28/2010 Fix bug of cannot print report from the invoice screen[End]

*B609443,1 MMT 11/10/2010 Fix bugs reported by customer in report layout[Start]
LnLstSty = 0
DECLARE laItemSeg[1]

*B609469,1 MMT 11/28/2010 Fix bug of cannot print report from the invoice screen[Start]
*PRIVATE lnCount , lnSclLen ,lnSclPos ,lcSclSpr
PRIVATE lnCount , lnSclLen ,lcSclSpr
STORE 0 TO lnSclPos 
*B609469,1 MMT 11/28/2010 Fix bug of cannot print report from the invoice screen[End]

=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
 IF laItemSeg[lnCount,1]='S'
   lnSclLen = LEN(laItemSeg[lnCount,3])
   lnSclPos = laItemSeg[lnCount,4]
   lcSclSpr = ALLT(laItemSeg[lnCount,6])
   EXIT
 ENDIF
ENDFOR
*B609443,1 MMT 11/10/2010 Fix bugs reported by customer in report layout[End]

SELECT INVHDR
LOCATE 
LOCATE FOR &lcRpExp
IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  *B609469,1 MMT 11/28/2010 Fix bug of cannot print report from the invoice screen[Start]
  llarpinv  = .f.
  *B609469,1 MMT 11/28/2010 Fix bug of cannot print report from the invoice screen[End]
  RETURN
ENDIF

lcPrintPart = ' '
*-- Loop to make Status expression.
*-- Now we didn`t canceld or deselect
IF !EMPTY(laRpTParts[1])
  FOR lnI = 1 TO ALEN(laRpTParts,1)
    lcPrintPart = lcPrintPart+ IIF(laRpTParts[lnI] = 'INVOICE','A',;
      IIF(laRpTParts[lnI] = 'PACKING LIST','B',;
      IIF(laRpTParts[lnI] = 'OFFICE COPY','C',IIF(laRpTParts[lnI] = 'SALES COPY','D',''))))
  ENDFOR  && end Loop to make Status expression.

ENDIF && End of ' IF !EMPTY(laRpTarget[1]) '

lcPrintPart = IIF(EMPTY(lcPrintPart),lcPrintPart ,ALLTRIM(lcPrintPart))


SELECT Invhdr 
DIMENSION laStructArr[1,18]

lnLength =AFIELDS(laStructArr)
DIMENSION laStructArr[lnLength +1,18]

laStructArr[lnLength +1,1]= 'cPType'
laStructArr[lnLength +1,2]= 'C'
laStructArr[lnLength +1,3]=  1
laStructArr[lnLength +1,4]= 0


FOR lncnt=7 TO 16
    STORE SPACE(0) TO laStructArr[lnLength +1,lnCnt]
ENDFOR  
STORE 0 TO laStructArr[lnLength +1,17],laStructArr[lnLength +1,18]

=gfCrtTmp(lcInvhFile ,@laStructArr,'INVOICE+cPType',lcInvhFile,.T.)

SELECT INVHDR
lcrelation = SET("Relation" )
lcSkip = SET("Skip")
SET RELATION TO
SET SKIP TO 
SCAN FOR  &lcRpExp
  SCATTER MEMO MEMVAR 
  IF !EMPTY(lcPrintPart)
    
    IF 'A' $ lcPrintPart
      m.CPTYPE = 'A'
      SELECT (lcInvhFile) 
      APPEND BLANK 
      GATHER MEMO MEMVAR
    ENDIF 
    
    IF 'B' $ lcPrintPart
      m.CPTYPE = 'B'
      SELECT (lcInvhFile) 
      APPEND BLANK 
      GATHER MEMO MEMVAR
    ENDIF 
    
    IF 'C' $ lcPrintPart
      m.CPTYPE = 'C'
      SELECT (lcInvhFile) 
      APPEND BLANK 
      GATHER MEMO MEMVAR
    ENDIF 
    
    IF 'D' $ lcPrintPart
      m.CPTYPE = 'D'
      SELECT (lcInvhFile) 
      APPEND BLANK 
      GATHER MEMO MEMVAR
    ENDIF 

  ELSE
    m.CPTYPE = 'X'
    SELECT (lcInvhFile) 
    APPEND BLANK 
    GATHER MEMO MEMVAR
  ENDIF   
ENDSCAN

lcrelation = STRTRAN(UPPER(lcrelation),'INVHDR.',"")

SELECT (lcInvhFile) 
SET RELATION TO &lcrelation
SET SKIP TO &lcSkip

SET RELATION TO invoice INTO invhdr ADDITIVE
*B609443,1 MMT 11/10/2010 Fix bugs reported by customer in report layout[Start]
LOCATE 
*B609443,1 MMT 11/10/2010 Fix bugs reported by customer in report layout[End]
*B611166,1 MMT 07/13/2016 Custom Invoice form HS does not export invoice detail to XLS or XML[T20160622.0012][Start]
IF "EXCEL" $ loogScroll.cTextRepType OR "XML" $ loogScroll.cTextRepType 
  lcExpTemp = gfTempName()
  DIMENSION laFileStru[1,1]
  =AFIELDS(laFileStru)
     
  lnFileStru = ALEN(laFileStru , 1)    && array rows
  DIMENSION laFileStru[lnFileStru + 6 , ALEN(laFileStru , 2)]
  I = 1

  laFileStru[lnFileStru + 1 , 1]  = 'Vendor'
  laFileStru[lnFileStru + 1 , 2]  = 'C'
  laFileStru[lnFileStru + 1 , 3]  = 15

  laFileStru[lnFileStru + 2 , 1]  = 'Terms'
  laFileStru[lnFileStru + 2 , 2]  = 'C'
  laFileStru[lnFileStru + 2 , 3]  = 30

  laFileStru[lnFileStru + 3 , 1]  = 'Shipvia_Ds'
  laFileStru[lnFileStru + 3 , 2]  = 'C'
  laFileStru[lnFileStru + 3 , 3]  = 30

  laFileStru[lnFileStru + 4 , 1]  = 'STYLE'
  laFileStru[lnFileStru + 4 , 2]  = 'C'
  laFileStru[lnFileStru + 4 , 3]  = 19   

  laFileStru[lnFileStru + 5 , 1]  = 'ALTSTYLE'
  laFileStru[lnFileStru + 5 , 2]  = 'C'
  laFileStru[lnFileStru + 5 , 3]  = 19      

  laFileStru[lnFileStru + 6 , 1]  = 'SCALE'
  laFileStru[lnFileStru + 6 , 2]  = 'C'
  laFileStru[lnFileStru + 6 , 3]  = 3      

  FOR I = 1 TO 6
    laFileStru[lnFileStru + I , 4]  = 0
    laFileStru[lnFileStru + I , 5]  = .F.
    laFileStru[lnFileStru + I , 6]  = .F.
    laFileStru[lnFileStru + I , 7]  = ""
    laFileStru[lnFileStru + I , 8]  = ""
    laFileStru[lnFileStru + I , 9]  = ""
    laFileStru[lnFileStru + I , 10] = ""
    laFileStru[lnFileStru + I , 11] = ""
    laFileStru[lnFileStru + I , 12] = ""
    laFileStru[lnFileStru + I , 13] = ""
    laFileStru[lnFileStru + I , 14] = ""
    laFileStru[lnFileStru + I , 15] = ""
    laFileStru[lnFileStru + I , 16] = ""
    laFileStru[lnFileStru + I , 17] = .F.
    laFileStru[lnFileStru + I , 18] = .F.
  ENDFOR

  lnFileStru = ALEN(laFileStru , 1)   && array rows
  DIMENSION laFileStru[lnFileStru + 8 , ALEN(laFileStru , 2)]
  I = 1
  FOR I = 1 TO 8
    laFileStru[lnFileStru + I , 1]  = 'SIZE'+ALLTRIM(STR(I))
    laFileStru[lnFileStru + I , 2]  = 'C'
    laFileStru[lnFileStru + I , 3]  = 5
    laFileStru[lnFileStru + I , 4]  = 0
    laFileStru[lnFileStru + I , 5]  = .F.
    laFileStru[lnFileStru + I , 6]  = .F.
    laFileStru[lnFileStru + I , 7]  = ""
    laFileStru[lnFileStru + I , 8]  = ""
    laFileStru[lnFileStru + I , 9]  = ""
    laFileStru[lnFileStru + I , 10] = ""
    laFileStru[lnFileStru + I , 11] = ""
    laFileStru[lnFileStru + I , 12] = ""
    laFileStru[lnFileStru + I , 13] = ""
    laFileStru[lnFileStru + I , 14] = ""
    laFileStru[lnFileStru + I , 15] = ""
    laFileStru[lnFileStru + I , 16] = ""
    laFileStru[lnFileStru + I , 17] = .F.
    laFileStru[lnFileStru + I , 18] = .F.
  ENDFOR

  lnFileStru = ALEN(laFileStru , 1)   && array rows
  DIMENSION laFileStru[lnFileStru + 8 , ALEN(laFileStru , 2)]
  I = 1
  FOR I = 1 TO 8
    laFileStru[lnFileStru + I , 1]  = 'QTY'+ALLTRIM(STR(I))
    laFileStru[lnFileStru + I , 2]  = 'N'
    laFileStru[lnFileStru + I , 3]  = 6
    laFileStru[lnFileStru + I , 4]  = 0
    laFileStru[lnFileStru + I , 5]  = .F.
    laFileStru[lnFileStru + I , 6]  = .F.
    laFileStru[lnFileStru + I , 7]  = ""
    laFileStru[lnFileStru + I , 8]  = ""
    laFileStru[lnFileStru + I , 9]  = ""
    laFileStru[lnFileStru + I , 10] = ""
    laFileStru[lnFileStru + I , 11] = ""
    laFileStru[lnFileStru + I , 12] = ""
    laFileStru[lnFileStru + I , 13] = ""
    laFileStru[lnFileStru + I , 14] = ""
    laFileStru[lnFileStru + I , 15] = ""
    laFileStru[lnFileStru + I , 16] = ""
    laFileStru[lnFileStru + I , 17] = .F.
    laFileStru[lnFileStru + I , 18] = .F.
  ENDFOR

  lnFileStru = ALEN(laFileStru , 1)   && array rows
  DIMENSION laFileStru[lnFileStru + 1 , ALEN(laFileStru , 2)]
  lnFileStru = lnFileStru + 1

  laFileStru[lnFileStru , 1]  = 'TotQTY'
  laFileStru[lnFileStru , 2]  = 'N'
  laFileStru[lnFileStru , 3]  = 6   
  laFileStru[lnFileStru , 4]  = 0
  laFileStru[lnFileStru , 5]  = .F.
  laFileStru[lnFileStru , 6]  = .F.
  laFileStru[lnFileStru , 7]  = ""
  laFileStru[lnFileStru , 8]  = ""
  laFileStru[lnFileStru , 9]  = ""
  laFileStru[lnFileStru , 10] = ""
  laFileStru[lnFileStru , 11] = ""
  laFileStru[lnFileStru , 12] = ""
  laFileStru[lnFileStru , 13] = ""
  laFileStru[lnFileStru , 14] = ""
  laFileStru[lnFileStru , 15] = ""
  laFileStru[lnFileStru , 16] = ""
  laFileStru[lnFileStru , 17] = .F.
  laFileStru[lnFileStru , 18] = .F.     

  lnFileStru = ALEN(laFileStru , 1)   && array rows
  DIMENSION laFileStru[lnFileStru + 8 , ALEN(laFileStru , 2)]
  I = 1
  FOR I = 1 TO 8
    laFileStru[lnFileStru + I , 1]  = 'SKU'+ALLTRIM(STR(I))
    laFileStru[lnFileStru + I , 2]  = 'C'
    laFileStru[lnFileStru + I , 3]  = 16
    laFileStru[lnFileStru + I , 4]  = 0
    laFileStru[lnFileStru + I , 4]  = 0
    laFileStru[lnFileStru + I , 5]  = .F.
    laFileStru[lnFileStru + I , 6]  = .F.
    laFileStru[lnFileStru + I , 7]  = ""
    laFileStru[lnFileStru + I , 8]  = ""
    laFileStru[lnFileStru + I , 9]  = ""
    laFileStru[lnFileStru + I , 10] = ""
    laFileStru[lnFileStru + I , 11] = ""
    laFileStru[lnFileStru + I , 12] = ""
    laFileStru[lnFileStru + I , 13] = ""
    laFileStru[lnFileStru + I , 14] = ""
    laFileStru[lnFileStru + I , 15] = ""
    laFileStru[lnFileStru + I , 16] = ""
    laFileStru[lnFileStru + I , 17] = .F.
    laFileStru[lnFileStru + I , 18] = .F.
  ENDFOR

  CREATE CURSOR (lcExpTemp) FROM ARRAY laFileStru
  SELECT (lcExpTemp)
  INDEX ON INVOICE+STYLE TAG INVSTYLE
  SET ORDER TO

  SELECT (lcInvhFile)
  SET RELATION TO 
  lcOldInv = ""
  LOCATE
  lcCptype = cptype
  SCAN FOR cptype = lcCptype 
    lcInv = Invoice
    SCATTER MEMO MEMVAR
    SELECT INVLINE
    =SEEK(lcInv)
    SCAN REST WHILE Invoice+STR(LINENO,6) = lcInv
      INSERT INTO (lcExpTemp) FROM MEMVAR
      SELECT (lcExpTemp)
      REPLACE TERMS WITH lcTerms
      REPLACE VENDOR WITH IIF(!EMPTY(Ordhdr.INT_VEND) ,Ordhdr.INT_VEND, CUSTOMER.CCUSVEND)
      REPLACE SHIPVIA_DS WITH lcShipVia
      
      SELECT INVLINE
      SCATTER FIELDS ALTSTYLE,STYLE,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TotQty MEMVAR
      SELECT (lcExpTemp)
      GATHER MEMVAR
      
      SELECT SPCK_LIN
      IF !EMPTY(PACK_ID)
        lnI = 1
        lcSkuTmpl=IIF(!EMPTY(CUSTOMER.SkuTmpl),CUSTOMER.SkuTmpl,'DEF')
        IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
          lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
          lnDime2 = SkuTmpl.Len4
        ELSE
          lnDime1 = 8  &&Default
          lnDime2 = 8  &&Default
        ENDIF

        IF llExtSize
          =SEEK(TYPE+Account+SUBSTR(STYLE,1,LEN(ALLTRIM(STYLE))-4),'SPCK_HDR')
        ELSE
          =SEEK(TYPE+Account+STYLE,'SPCK_HDR')
        ENDIF

        lnDime1 = MIN(lnDime1,LEN(ALLTRIM(SPCK_HDR.SKU)))

        SCAN WHILE TYPE+Account+STYLE = 'S'+INVLINE.Account+INVLINE.STYLE .AND. lnI < 9
          FOR lnX=1 TO 8
            Z=STR(lnX,1)
            IF QTY&Z > 0
              SELECT (lcExpTemp)
              REPLACE sku&Z WITH SUBSTR(SPCK_LIN.PACK_ID,lnDime1+1,lnDime2)
              EXIT
            ENDIF
          ENDFOR
          lnI = lnI + 1
        ENDSCAN
      ENDIF
      SELECT (lcExpTemp)
      REPLACE SCALE with SCALE.SCALE 
      FOR I=1 TO 8
        lcI = STR(I,1)
        REPLACE SIZE&lcI WITH SCALE.SZ&lcI
      ENDFOR 
    ENDSCAN
  ENDSCAN
  SELECT (lcExpTemp)
  SET ORDER TO INVSTYLE
  lcRpExp = ".T."
ENDIF
*B611166,1 MMT 07/13/2016 Custom Invoice form HS does not export invoice detail to XLS or XML[T20160622.0012][End]
DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
llarpinv  = .f.
RETURN 

*!*************************************************************
*! Name      : lfCrtMover
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/04/2010
*! Purpose   : Function to create mover for allowance ,
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfCrtMover()
*!*************************************************************
FUNCTION lfCrtMover
PARAMETER lcParam


= lfogmover(@laRpSParts,@laRpTParts,'Parts',.T.,'')
IF EMPTY(laRpTParts[1])
  =gfModalGen('INM00000B00000','','','','You have to select at least one copy to be printed.')    
  DIMENSION laRpTParts[2]
  laRpTParts[1] = 'INVOICE'
  laRpTParts[2] = 'PACKING LIST'
 * laRpTParts[3] = 'OFFICE COPY'
ENDIF 

lcPrintPart = ' '
*-- Loop to make Status expression.
*-- Now we didn`t canceld or deselect
IF !EMPTY(laRpTParts[1])
  FOR lnI = 1 TO ALEN(laRpTParts,1)
    lcPrintPart = lcPrintPart+ IIF(laRpTParts[lnI] = 'INVOICE','A',;
      IIF(laRpTParts[lnI] = 'PACKING LIST','B',;
      IIF(laRpTParts[lnI] = 'OFFICE COPY','C',IIF(laRpTParts[lnI] = 'SALES COPY','D',''))))
  ENDFOR  && end Loop to make Status expression.

ENDIF && End of ' IF !EMPTY(laRpTarget[1]) '

lcPrintPart = IIF(EMPTY(lcPrintPart),lcPrintPart ,ALLTRIM(lcPrintPart))


*!*************************************************************
*! Name      : lfFillAll
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/04/2010
*! Purpose   : Function to fill Allowance
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************

FUNCTION lfFillAll
PARAMETER lcParam
DIMENSION laRpSParts[1,1],laRpTParts[1,1]

STORE '' TO laRpSParts,laRpTParts

DIMENSION laRpSParts[4]


laRpSParts[1] = 'INVOICE'
laRpSParts[2] = 'PACKING LIST'
laRpSParts[3] = 'OFFICE COPY'
laRpSParts[4] = 'SALES COPY'

DIMENSION laRpTParts[2]
laRpTParts[1] = 'INVOICE'
laRpTParts[2] = 'PACKING LIST'
*laRpTParts[3] = 'OFFICE COPY'


lcPrintPart = ' '
*-- Loop to make Status expression.
*-- Now we didn`t canceld or deselect
IF !EMPTY(laRpTParts[1])
  FOR lnI = 1 TO ALEN(laRpTParts,1)
    lcPrintPart = lcPrintPart+ IIF(laRpTParts[lnI] = 'INVOICE','A',;
      IIF(laRpTParts[lnI] = 'PACKING LIST','B',;
      IIF(laRpTParts[lnI] = 'OFFICE COPY','C',IIF(laRpTParts[lnI] = 'SALES COPY','D',''))))
  ENDFOR  && end Loop to make Status expression.

ENDIF && End of ' IF !EMPTY(laRpTarget[1]) '

lcPrintPart = IIF(EMPTY(lcPrintPart),lcPrintPart ,ALLTRIM(lcPrintPart))
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 03/04/2010
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTParts)
    FOR lnTarget = 1 TO ALEN(laRpTParts,1)
      lcStatusStr = lcStatusStr + ", " + laRpTParts[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 

*!*************************************************************
*! Name      : lfSSpAdr
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 03/04/2010
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Terms
*!*************************************************************
*! Called from : ARPINVA.FRX
*!*************************************************************
*! Calls       : gfRltFld() , gfCodDes() , gfGetAdr() , lfAdrShift()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfSSpAdr
PARAMETERS lcnothing


PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec
lnInvHdRec = IIF(EOF(lcInvhFile) , 0 , RECNO(lcInvhFile))
lnInvLnRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
lnPakLnRec = IIF(EOF('SPCK_LIN') , 0 , RECNO('SPCK_LIN'))

IF USED(lcTmpDbt)
  lnTmpDbt = IIF(EOF(lcTmpDbt) , 0 , RECNO(lcTmpDbt))
  lnARINSTMD = IIF(EOF('ARINSTMD') , 0 , RECNO('ARINSTMD'))
ELSE
  lnTmpDbt   = 0
  lnARINSTMD = 0
ENDIF
lnLineRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
lnHrRc    = IIF(EOF(lcInvhFile) , 0 , RECNO(lcInvhFile))

COUNT TO lnLines WHILE INVLINE.INVOICE = EVALUATE(lcInvhFile+'.INVOICE')
IF lnInvLnRec > 0
  GO (lnLineRec) IN INVLINE
ENDIF
IF lnHrRc > 0
  GO (lnHrRc) IN (lcInvhFile)
ENDIF

*-- Fill laFactor with factor address
IF !EMPTY(EVALUATE(lcInvhFile+'.CFACCODE'))
  =SEEK(EVALUATE(lcInvhFile+'.CFACCODE'),'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

llEndGroup = .F.
=gfRltFld(EVALUATE(lcInvhFile+'.cDivision') , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(EVALUATE(lcInvhFile+'.ShipVia') , 'SHIPVIA')
lcTerms = gfCodDes(EVALUATE(lcInvhFile+'.cTermCode') , 'CTERMCODE')

lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

*IF ORDHDR.Alt_ShpTo is .T.
SELECT (lcInvhFile)
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GOTO RECNO()
ENDIF
SELECT CUSTOMER

IF ORDHDR.Alt_ShpTo
  lcShpTName  = ORDHDR.STName
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
ELSE    && Else

  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr) AND !ORDHDR.lStrDirct
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF

  lcShpTName  = IIF(EVALUATE(lcInvhFile+'.STORE') = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

  laShipTo[1] = IIF(EVALUATE(lcInvhFile+'.STORE')= "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(EVALUATE(lcInvhFile+'.STORE')= "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(EVALUATE(lcInvhFile+'.STORE')= "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(EVALUATE(lcInvhFile+'.STORE') = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(EVALUATE(lcInvhFile+'.STORE') = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF lnCUSRec <> 0
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
ENDIF    && End of IF

=lfAdrShift('laShipTo')

  SELECT (lcInvhFile)

IF lnTmpDbt <> 0
  GO lnTmpDbt IN (lcTmpDbt)
ENDIF
IF lnARINSTMD <> 0
  GO lnARINSTMD IN ARINSTMD
ENDIF

*-- Restore the old record pointer in INVLINE
IF lnInvLnRec = 0
  GO BOTTOM IN INVLINE
  IF !EOF('INVLINE')
    SKIP IN INVLINE
  ENDIF
ELSE
  GO lnInvLnRec IN INVLINE
ENDIF

*-- Restore the old record pointer in SPCK_LIN
IF lnPakLnRec = 0
  GO BOTTOM IN SPCK_LIN
  IF !EOF('SPCK_LIN')
    SKIP IN SPCK_LIN
  ENDIF
ELSE
  GO lnPakLnRec IN SPCK_LIN
ENDIF
RETURN ''


*B609443,1 MMT 11/10/2010 Fix bugs reported by customer in report layout[Start]
*!*************************************************************
*! Name      : lfGetLastLine
*: Developer : MAriam Mazhar (MMT)
*: Date      : 11/10/2010
*! Purpose   : get last line in invoice
*!*************************************************************
FUNCTION lfGetLastLine
PRIVATE lcThAlias,lnThRec,lcThStore
lcThAlias = ALIAS()           && Save Current Alias.
SELECT InvLine
lnThRec = RECNO()    && Save Current record #.
=SEEK(&lcInvhFile..Invoice)
SCAN REST WHILE INVOICE+STR(LINENO,6) =  &lcInvhFile..Invoice
  LnLstSty = LINENO
ENDSCAN 

IF BETWEEN(lnThRec,1,RECCOUNT('InvLine'))
  GO lnThRec IN InvLine   && Restore Record #
ELSE
  GO TOP IN InvLine   && Restore Record #
ENDIF
SELECT (lcThAlias)            && Restore Alias.
RETURN ''
*B609443,1 MMT 11/10/2010 Fix bugs reported by customer in report layout[end]