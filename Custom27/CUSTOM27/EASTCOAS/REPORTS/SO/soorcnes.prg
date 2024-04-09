*:***************************************************************************
*: Program file  : SoorcnEs
*: Program desc. : Custemized Order Confirmation for EAS (East Coast Molders, Inc.)
*: For Report    : SOORCNES.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Aldesouki (SAM)
*: Date          : 01/27/2000 
*:***************************************************************************
*: Calls : Procedures : ....
*:         Functions  : gfItemMask , gfOpenFile , lfFillScal , lfUsrVldFn , lfGetSku
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SoorcnEs
*:***************************************************************************
*: Due to C101737,1 SAM 01/20/2000 Custemized Order Confirmation for EAS 
*:***************************************************************************
*: Modifications :
*B803040,1 SSH 02/14/2000 Print the sku.
*:***************************************************************************
*:
*-- lnAlias   : Save active alias
*-- laAllSegs : Segments array.
*-- laScales  : Array that hold the following 
*--           : Col1 => Size scale description
*--           : Col2 => Order Quantities
*--           : Col3 => SKU/PACK Description.
*-- lnStrtPos : Variable hold scale start postion in full style mask. 
PRIVATE lnX , lnAlias , laAllSegs
*B803040,1 SSH 02/14/2000 Print the sku.
*DIMENSION laScales[8,3] , laAllSegs[1,1]
DIMENSION laScales[9,3] , laAllSegs[1,1]
*B803040,1 SSH (End)
*-- initialize variables for .FRX
lnStrtPos = 0
STORE "" TO laScales , laAllSegs

lnAlias = SELECT(0)  && Store Alias.

*-- Fill segments info. in laAllSegs array.
=gfItemMask(@laAllSegs)

*-- Loop to get start position of scale
FOR lnX=1 TO ALEN(laAllSegs,1)
  *-- if segment is scale
  IF laAllSegs[lnX,1]='S' 
    *-- start pos. -1 - separator's len.
    lnStrtPos = laAllSegs[lnX,4] -1 -LEN(laAllSegs[lnX,6])
  ENDIF
ENDFOR   

*-- [START] open files needed for sku/pack with temprary name declared in SYREPUVR.DBF, 
*-- assuring that thes files will be automatically closed when exit report.
IF USED(lcPackFile)
  SET ORDER TO SpckLins IN (lcPackFile)
ELSE  
  =gfOpenFile(gcDataDir+'Spck_Lin',gcDataDir+'SpckLins','SH',lcPackFile)
ENDIF

IF USED(lcTmplFile)
  SET ORDER TO SkuTmpl IN (lcTmplFile)
ELSE
  =gfOpenFile(gcDataDir+'SkuTmpl',gcDataDir+'SkuTmpl','SH',lcTmplFile)
ENDIF
*-- [END  ] open files needed for sku/pack with temprary name declared in SYREPUVR.DBF, 
  
*-- Restore saved alias.
SELECT (lnAlias)
*-- end of optional code.

*!***********************************************************************
*! Name      : lfFillScal
*! Developer : Sameh Aldesouki (SAM) 
*! Date      : 01/20/2000
*! Purpose   : 1- Shift zero quantities to the end of 8 scales.
*!           : 2- Fill SKU/PACK Description.
*!***********************************************************************
*! Called from : soorcnes.FRX [Dummy Variable lcEvalDumy]
*!***********************************************************************
*! Passed Parameters  : Dummy parameter assure return value.
*!***********************************************************************
*! Example   : =lfFillScal()
*!***********************************************************************
*
FUNCTION lfFillScal
PARAMETERS lcDummyPar
lcDummyPar = ''

*-- initialize SKU/Pack array.
DIMENSION laSku[8]
STORE "" TO laScales ,laSku

=lfGetSku()  &&get sku/pack

*-- lnRef : Incremented only if order quantity greater than zero.
STORE 1 TO lnIndex , lnRef
*-- loop to fill Scale array which described before.
FOR lnIndex=1 TO 8
  lcI=STR(lnIndex,1)
  *-- if non zero quantity.
  IF &lcTempOrd..QTY&lcI <> 0 
    laScales[lnRef,1] = SCALE.SZ&lcI  && Size
    laScales[lnRef,2] = TRANSFORM(STR(&lcTempOrd..QTY&lcI,6),"999999") && Quantity
    laScales[lnRef,3] = laSku[lnIndex]   && sku/pack
    lnRef = lnRef + 1
  ENDIF  && end if non zero quantity.
ENDFOR  && end loop to fill Scale array which described before.
*-- End of lfFillScal.

*!**************************************************************************
*! Func. Name: lfGetSku
*! Developer : Sameh Aldesouki
*! Date      : 01/19/2000
*! Purpose   : get sku/pack for sizes
*!**************************************************************************
*! Example   :  =lfGetSku()
*!**************************************************************************
*
FUNCTION lfGetSku 
PRIVATE lnPrevAl,lnSkuNo,lcSkuNo,lnArrElmnt,lcSize
STORE ' ' TO lcStrToPrn
lnArrElmnt = 1 
lcStyle = IIF(!EMPTY(&lcTempOrd..ALTSTYLE),&lcTempOrd..ALTSTYLE,&lcTempOrd..Style)

*-- if you find SKU.
IF SEEK('S'+&lcTempOrd..Account+lcStyle,lcPackFile)

  lnPrevAl = SELECT(0)
  SELECT (lcPackFile)
  *-- if you have pack id.
  IF !EMPTY(PACK_ID)
    lnSkuNo = 1
    =SEEK(&lcTempOrd..Account,"CUSTOMER")
    lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
    *-- if you have pre-defined dimensions for this SKU/Pack
    IF SEEK('S'+lcSkuTmpl,lcTmplFile)
      *B803040,1 SSH 02/14/2000 Print the sku.
      *lnDime1 = &lcTmplFile..Len1+&lcTmplFile..Len2+&lcTmplFile..Len3
      llPakHdr = gfOpenFile(gcDataDir+'Spck_Hdr' ,'Sku_style','SH')
      SELECT Spck_Hdr
      IF SEEK('S'+&lcTempOrd..Account+SUBSTR(lcStyle,1,LEN(lcStyle)-4))
        *Ren
        *laScales[9,3] = SUBSTR(Spck_Hdr.Pack_Id,1,4)
        laScales[9,3] = Spck_Hdr.Pack_Id
        *Ren end
        lnDime1       = LEN(ALLTRIM(Spck_Hdr.Pack_Id))
      ELSE
        lnDime1   = 8
        laScales[9,3] = ''
      ENDIF
      USE IN IIF(llPakHdr,'Spck_Hdr',0)
      SELECT (lcPackFile)
      *B803040,1 SSH (End)
      lnDime2 = &lcTmplFile..Len4
    ELSE  && else it is default Values [8 sizes]
      lnDime1 = 8  
      lnDime2 = 8  
    ENDIF  && end if you have pre-defined dimensions for this SKU/Pack
    *-- Scan SKU lines for active account,style.
    *-- The aim is to change 8 sizes from vertical (Max. 8 records) to be horizonital
    *-- (i.e. in array columns to be printed in one line)
    *-- Example :
    *--	Scale Desc.	SIZE1		SIZE2		SIZE3		SIZE4
    *-- Order Qty.	   10	   	 12	   	 15	    	7
    *-- Sku/Pack		SKU11		SKU12		SKU13		SKU14	
    SCAN REST WHILE Type+Account+Style = 'S'+&lcTempOrd..Account+lcStyle .AND. lnSkuNo < 9
      FOR lnArrElmnt=1 TO 8
        lcSize=STR(lnArrElmnt,1)
        IF QTY&lcSize > 0
          *Ren
          *laSku[lnArrElmnt]=SUBSTR(Pack_Id,lnDime1 + 1,lnDime2)
          laSku[lnArrElmnt]=ALLTRIM(Pack_Id)
          *Ren end
          EXIT
        ENDIF
      ENDFOR
      lnSkuNo = lnSkuNo + 1
    ENDSCAN  && END Scan SKU lines for active account,style.
  ENDIF  && end if you have pack id.
  SELECT (lnPrevAl)
  *B803040,1 SSH 02/14/2000 Print the sku.
ELSE
  laScales[9,3] = ''
  FOR lnArrElmnt=1 TO 8
    lcSize=STR(lnArrElmnt,1)
    laSku[lnArrElmnt]=''
  ENDFOR
  *B803040,1 SSH (End)
ENDIF  && end if you find SKU.
*-- End of lfGetSku. 
