*:***************************************************************************
*: Program file  : POSTYPVJ
*: Program desc. : Custom PO Form for Vijay      
*: For Report    : POSTYPVJ.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO2
*: Date          : 08/15/2004
*: Developer     : TMI - TAREK MOHAMMED IBRAHIM
*: Entry #       : C # 038291,1
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfGetAdr()
*:***************************************************************************
*: Passed Parameters  : None
*C038291,5 TMI 10/27/2004 Add two custom fields STYLE.CWGHT and POSHDR.CDOZPRICE , and make some other modifications in the FRX
*B607898,1 TMI 12/21/2006 Fix a problem that the report does not run at some PCs ( ticket # T20060919.0001 )
*:***************************************************************************
PRIVATE lnI,lnK,laOpFls,laScl,lnCnt,lcStyClr

*-Get Address and save it in the array laCompAdd
=lfGetAdd()

SELECT PosHdr
SET SKIP TO
SELECT PosLn
SET SKIP TO
SET ORDER TO PosLn
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SCALE
SET RELATION OFF INTO OBJLINK

*- open Needed files
=lfOpnFls()

*-- Creating the temporary file
=lfCrTmpFls()

*--- Opend needed files.
DIMENSION laOpFls[1]
laOpFls[1] = ''

=lfOpnFl('POSPEC'  ,'POSPECPO')
=lfOpnFl('POINS'   ,'POINS'   )
=lfOpnFl('STYSPEC' ,'STYSPEC' )

*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
=lfGetColor()

*- Get extended width 
lnExWid = gfGetMemvar('M_EXTWIDTH')

*- Collecting data
=lfCollect()

*- Close files opened for this report
*FOR lnI = 1 TO ALEN(laOpFls)
*  IF !EMPTY(laOpFls[lnI]) AND USED(laOpFls[lnI])
*    USE IN (laOpFls[lnI])
*  ENDIF
*ENDFOR

*- Reset relations
SELECT PosLn
SET RELATION TO
SELECT PosHdr
SET RELATION TO

SELECT (lcTmpPolns)
GOTO TOP
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE
SET RELATION TO PO INTO POINS ADDITIVE
SET RELATION TO '2'+PO INTO CUTPICK ADDITIVE

*C038291,3  TMI [Start] Add the phrase "ADDITIVE" to preserve other relations , and refresh the relation
*SET RELATION TO 'S'+SUBSTR(STYLE,1,lnMajSize) INTO OBJLINK 
SET RELATION TO 'S'+SUBSTR(STYLE,1,lnMajSize) INTO OBJLINK ADDITIVE
GO TOP
*C038291,3  TMI [End  ] 

SELECT PosHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE

SELECT (lcTmpPolns)
lcRpExp = ".T."

*-- End Of Option Program.

*:**************************************************************************
*:* Name        : lfGetAdd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/16/2004
*:* Purpose     : *-Get Address and save it in the array laCompAdd
*:***************************************************************************
FUNCTION lfGetAdd
SELECT CADDRESS1,CADDRESS3,CADDRESS5,CADDRESS6,CCOM_PHON,CCOM_FAX ;
    FROM (gcSysHome+'SYCCOMP') ;
    WHERE CCOMP_ID = gcACT_Comp ;
    INTO ARRAY laCompAdd
laCompAdd[2] = LEFT(ALLTRIM(laCompAdd[2]),15)
laCompAdd[3] = LEFT(ALLTRIM(laCompAdd[3]),15)
*-- end of lfGetAdd.
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


*:**************************************************************************
*:* Name        : lfOpnFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/15/2004
*:* Purpose     : Open needed files
*:***************************************************************************
FUNCTION lfOpnFl
PARAMETERS lcFile,lcTag
PRIVATE lnLen
IF !USED(lcFile)  
  =gfOpenFile(gcDataDir+lcFile,lcTag,'SH')
  lnLen = ALEN(laOpFls) + 1
  DIMENSION laOpFls[lnLen,2]
  laOpFls[lnLen] = 'POSPEC'
ENDIF

*-- end of lfOpnFl.


*:**************************************************************************
*:* Name        : lfCrTmpFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/09/2004
*:* Purpose     : Create the temp file needed to collect data.
*:***************************************************************************
FUNCTION lfCrTmpFls

IF USED(lcTmpPoLns)
  USE IN (lcTmpPoLns)
  USE (gcWorkDir+lcTmpPoLns) IN 0 ORDER TAG (lcTmpPoLns) EXCLUSIVE
  SELECT (lcTmpPoLns)
  ZAP
ELSE
  SELECT PosLn
  =AFIELDS(laStru)
  lnI = ALEN(laStru,1)
  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CPAGE'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 1
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'TYPE'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 1
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'SUPPLIER'
  laStru[lnI,2] = 'M'
  laStru[lnI,3] = 10
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CUSTPO'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 15
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'NOTEPAD'
  laStru[lnI,2] = 'M'
  laStru[lnI,3] = 10
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CPRI'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 50
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'NSTYORDER'
  laStru[lnI,2] = 'N'
  laStru[lnI,3] = 7
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'COMPLETE'
  laStru[lnI,2] = 'D'
  laStru[lnI,3] = 8
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'NSTYWEIGHT'
  laStru[lnI,2] = 'N'
  laStru[lnI,3] = 5
  laStru[lnI,4] = 2
  
  *C038291,5  TMI [Start] Use 'CWGHT' instead of 'NSTYWEIGHT'
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CWGHT'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 20
  laStru[lnI,4] = 0
  *C038291,5  TMI [End  ] 
  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'GAUGE'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'FABCOMP'
  laStru[lnI,2] = 'M'
  laStru[lnI,3] = 10
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CCFAB'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'MSZDESC'
  laStru[lnI,2] = 'M'
  laStru[lnI,3] = 10
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CLR'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'LINE'
  laStru[lnI,2] = 'N'
  laStru[lnI,3] = 2
  laStru[lnI,4] = 0
  *C038291,1  TMI [End  ] 
  
  FOR lnK = 9 TO 12
    lnI = lnI + 1
    DIMENSION laStru[lnI,4]
    laStru[lnI,1] = 'QTY'+ALLTRIM(STR(lnK))
    laStru[lnI,2] = 'N'
    laStru[lnI,3] = 5
    laStru[lnI,4] = 0
  ENDFOR
  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CDESC'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 40
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CTOLERANCE'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 5
  laStru[lnI,4] = 0
  
  FOR lnK = 1 TO 12
    lnI = lnI + 1
    DIMENSION laStru[lnI,4]
    laStru[lnI,1] = 'CSZ'+ALLTRIM(STR(lnK))
    laStru[lnI,2] = 'C'
    laStru[lnI,3] = 5
    laStru[lnI,4] = 0
  ENDFOR
  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'WASHING'
  laStru[lnI,2] = 'M'
  laStru[lnI,3] = 10
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'LABTEST'
  laStru[lnI,2] = 'M'
  laStru[lnI,3] = 10
  laStru[lnI,4] = 0

  *-- Add user definable fields that added to style file
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CBACKNECK '
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CWASHCARE '
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CSIZELABEL'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CSWINGTAG '
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CADDTAG   '
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CBARCODES '
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CKIMBALL  '
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CBUTTONS  '
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CZIPS     '
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'GWSYMBOL'  && A washing symbol picture
  laStru[lnI,2] = 'G'
  laStru[lnI,3] = 10 
  laStru[lnI,4] = 0
  
  *B607898,1 TMI [Start] add CWSYMBOL field to the header file
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CWSYMBOL'  && A washing symbol
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 6 
  laStru[lnI,4] = 0
  *B607898,1 TMI [End  ] 

  *- Project fields
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'DEST_FNSH1'
  laStru[lnI,2] = 'D'
  laStru[lnI,3] = 8
  laStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'DEST_FNSHP'
  laStru[lnI,2] = 'D'
  laStru[lnI,3] = 8
  laStru[lnI,4] = 0

  =gfCrtTmp(lcTmpPolns,@laStru,'PO+cPage+TYPE',lcTmpPolns)

ENDIF
*-- end of lfCrTmpFls.

*:**************************************************************************
*:* Name        : lfCollect
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/30/2004
*:* Purpose     : Collecting data
*:***************************************************************************
function lfCollect
PRIVATE lcRefrence,lnDeciml

SELECT PosHdr
SCAN FOR &lcRpExp
  IF SEEK(cStyType+Po,'PosLn')
  
    WAIT WINDOW NOWAIT 'Collecting data for order :' + POSHDR.PO
  
    lcRefrence = POSLN.REFERENCE 

    *- Get supplier Data
    SELECT APVENDOR
    SET ORDER TO TAG VenCode 
    =SEEK(POSHDR.VENDOR,'APVENDOR')
    M.SUPPLIER = IIF(!EMPTY(APVENDOR.CVENCOMP) ,APVENDOR.CVENCOMP +CHR(13),'')+;
                 IIF(!EMPTY(APVENDOR.CADDRESS1),APVENDOR.CADDRESS1+CHR(13),'')+;
                 IIF(!EMPTY(APVENDOR.CADDRESS2),APVENDOR.CADDRESS2+CHR(13),'')+;
                 IIF(!EMPTY(APVENDOR.CADDRESS3),APVENDOR.CADDRESS3+CHR(13),'')+;
                 IIF(!EMPTY(APVENDOR.CADDRESS4),APVENDOR.CADDRESS4+CHR(13),'')+;
                 IIF(!EMPTY(APVENDOR.CADDRESS5),APVENDOR.CADDRESS5+CHR(13),'')+;
                 IIF(!EMPTY(APVENDOR.CADDRESS6),APVENDOR.CADDRESS6+CHR(13),'')    
    
    M.PO = POSHDR.PO
    
    *- Get scale descriptions from scale file
    lcScl = SUBSTR(POSLN.SCALE,1,lnExWid)
    *C038291,3  TMI [Start] Put a filter on type wher selecting scale array ,and not deleted
    *SELECT CNT,SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,SCALE ;
    *  FROM SCALE ;
    *  WHERE SCALE.SCALE = lcScl ;
    *  INTO ARRAY laScl
    SELECT CNT,SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,SCALE ;
      FROM SCALE ;
      WHERE SCALE.SCALE = lcScl AND TYPE = 'S' AND !DELETED() ;
      INTO ARRAY laScl
    *C038291,3  TMI [End  ] 
    lnSclCnt = 1    

    *- Put scale descriptions into one memo field to be printed into one line in the report
    M.MSZDESC = ''
    FOR lnS = 1 TO ALEN(laScl,1)
      FOR lnCnt = 1 TO laScl[lnS,1]
        M.MSZDESC = M.MSZDESC + laScl[lnS,lnCnt+1] + CHR(13)
      ENDFOR
    ENDFOR   

    *- Get colors and save them in an array
    DIMENSION laClrs[1]
    laClrs = ' '
    *-Get po & colors    
    SELECT DISTINCT SUBSTR(STYLE,lnClrSrt,lnClrEnd) ;
      FROM POSLN ;
      WHERE PO = M.PO ;
      INTO ARRAY laClrs
    =ASORT(laClrs)    

    *- Get notepad
    *C038291,3  TMI [Start] initialize variable
    M.NOTEPAD = ''
    *C038291,3  TMI [End  ] 
    IF SEEK('F'+PADR(SUBSTR(POSLN.STYLE,1,lnMajSize),20),'NOTEPAD')
      M.NOTEPAD = SPACE(25)+NOTEPAD.MNOTES
    ENDIF

    *- Get currancy symbol
    =SEEK(POSHDR.CPRICECUR,'SYCCURR')
    lnDeciml = IIF(MOD(POSLN.NCOST1*100,1)=0,2,3)
    *C038291,5  TMI [Start] Use a new definable field CDOZPRICE
    *M.CPRI = ALLTRIM(SYCCURR.CCURRSMBL)+ALLTRIM(STR(POSLN.NCOST1,13,lnDeciml))+'  '+gfCodDes(POSHDR.CTERMS,'CTERMS')
    M.CPRI = ALLTRIM(SYCCURR.CCURRSMBL)+;
             IIF(POSHDR.CDOZPRICE>0 , ALLTRIM(STR(POSHDR.CDOZPRICE,8,2)) + ' DZ',;
                                      ALLTRIM(STR(POSLN.NCOST1,13,lnDeciml))+' PC')+;
                             '  '+IIF(!EMPTY(POSHDR.CTERMS),gfCodDes(POSHDR.CTERMS,'CTERMS'),'')
    *C038291,5  TMI [End  ] 

    =SEEK(POSLN.STYLE,'STYLE')
    *C038291,5  TMI [Start] Use user defined field "CWGHT" instead of STYLE.NSTYWEIGHT
    *M.NSTYWEIGHT = STYLE.NSTYWEIGHT
    M.CWGHT  = ALLTRIM(STYLE.CWGHT)
    *C038291,5  TMI [End  ] 
    M.GAUGE = gfCodDes(STYLE.CCGAUGE,'CCGAUGE')
    M.CCFAB = gfCodDes(STYLE.CCFAB,'CCFAB')

    *- Get instructions
    *C038291,3  TMI [Start] initialize instructions
    SELECT POINS
    SCATTER MEMVAR MEMO BLANK
    *C038291,3  TMI [End  ] 
    IF SEEK(POSHDR.PO,'POINS')
      SELECT POINS
      SCATTER MEMVAR MEMO    
    ENDIF

    SELECT POSLN
    SCATTER MEMVAR MEMO
    M.REFERENCE = lcRefrence 
    M.NSTYORDER = POSHDR.NSTYORDER
    M.COMPLETE = POSHDR.COMPLETE
    
    *C038291,4  TMI [Start] Get custpo , provided that the PO is generated from One sales order, 
    *                       if it is generated from more than one , 1st CustPo is returned
    M.CUSTPO = IIF(SEEK('2'+POSHDR.PO,'CUTPICK') .AND. SEEK('O'+CUTPICK.ORDER,'ORDHDR') ,;
                                                                          ORDHDR.CUSTPO , '' )
    *C038291,4  TMI [End  ] 

    M.CPAGE = '1'
    *- Fill qty fields QTY1,2,...,12 for each color
    lcSty = SUBSTR(POSLN.STYLE,1,lnClrSrt-1)
    FOR lnClr = 1 TO MIN(10,ALEN(laClrs))
      lcStyClr = lcSty+laClrs[lnClr]
      =SEEK('P'+POSHDR.PO+lcStyClr,'POSLN')
      STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8,M.QTY9,M.QTY10,M.QTY11,M.QTY12,M.TOTQTY
      STORE 0 TO M.PQTY1,M.PQTY2,M.PQTY3,M.PQTY4,M.PQTY5,M.PQTY6,M.PQTY7,M.PQTY8,M.PQTY9,M.PQTY10,M.PQTY11,M.PQTY12
      *C038291,3  TMI [Start] check po's with trancd = '1'
      *SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = 'P'+POSHDR.PO+lcStyClr
      SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = 'P'+POSHDR.PO+lcStyClr ;
                FOR TRANCD = '1'
        *C038291,3  TMI [End  ] 
        
        lnSzPos = 0  && Get scale position in scale array laScl 
                     && ( this is useful if one line within this scale is deleted from the PO )
        FOR lnS = 1 TO ALEN(laScl,1)
          IF laScl[lnS,10] = POSLN.SCALE
            EXIT
          ELSE
            lnSzPos = lnSzPos + laScl[lnS,1]
          ENDIF
        ENDFOR
        
        *- Check if this line is generated from a sales order using a pack
        llPck = .F.
        *C038291,4  TMI [Start] comment this line and get custpo befor the FOR loop
        *M.CUSTPO = ''
        *C038291,4  TMI [End  ] 
        IF SEEK('2'+POSLN.PO+STR(POSLN.LINENO,6),'CUTPICK')
          =SEEK('O'+CUTPICK.ORDER+CUTPICK.CORDLINE,'ORDLINE')
          *C038291,4  TMI [Start] comment these lines and get custpo befor the for loop
          *=SEEK('O'+CUTPICK.ORDER,'ORDHDR')
          *M.CUSTPO = ORDHDR.CUSTPO
          *C038291,4  TMI [End  ] 
          llPck = !EMPTY(ORDLINE.PACK_ID)
          IF !SEEK('P'+ORDLINE.ACCOUNT+ORDLINE.PACK_ID+ORDLINE.STYLE,'SPCK_LIN')
            =SEEK('P*****'+ORDLINE.PACK_ID+ORDLINE.STYLE,'SPCK_LIN')
          ENDIF
        ENDIF

        = SEEK('S'+POSLN.SCALE,'SCALE')
        FOR lnI = 1 TO SCALE.CNT
          lcI = STR(lnI,1)
          lcS = ALLTRIM(STR(lnI+lnSzPos))
          *C038291,3  TMI [Start] Accumulate also for size quantities , to take the case that same style color size is entered twice
          *M.QTY&lcS = POSLN.QTY&lcI
          M.QTY&lcS = M.QTY&lcS + POSLN.QTY&lcI
          *C038291,3  TMI [End  ] 
          M.TOTQTY = M.TOTQTY + POSLN.QTY&lcI

          IF llPck
            M.PQTY&lcS = SPCK_LIN.QTY&lcI
          ENDIF

        ENDFOR
      ENDSCAN
      M.CLR = gfCodDes(laClrs[lnClr],'COLOR')

      M.TYPE = 'A'
      IF M.TOTQTY <> 0 && If no qty's added for this color , do not add any lines and skip to next color
        M.LINE = lnClr
        INSERT INTO &LCTMPPOLNS FROM MEMVAR
        
        =IIF(!llPck,lfGetRatio(),'')
        
        *- Update M.qty1,... variables to be displaied in the report
        M.TOTQTY = 0
        FOR lnI = 1 TO 12
          lcI = ALLTRIM(STR(lnI))
          M.QTY&lcI = M.PQTY&lcI
          M.TOTQTY = M.TOTQTY + M.PQTY&lcI
        ENDFOR
        
        M.TYPE = 'B'
        INSERT INTO &LCTMPPOLNS FROM MEMVAR
      ENDIF
    ENDFOR
    
    *-If there is less than 10 colors add empty lines up to 10
    M.CLR = ' '    
    STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8,M.QTY9,M.QTY10,M.QTY11,M.QTY12,M.TOTQTY
    FOR lnK = lnClr TO 10
      M.LINE = lnK
      M.TYPE = 'A'
      INSERT INTO &LCTMPPOLNS FROM MEMVAR
      M.TYPE = 'B'
      INSERT INTO &LCTMPPOLNS FROM MEMVAR
    ENDFOR
    
    STORE {} TO M.DEST_FNSH1,M.DEST_FNSHP
    SELECT PMPRJDT
    IF SEEK('P'+POSHDR.PO,'PMPRJDT')
      SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID = 'P'+POSHDR.PO
        M.DEST_FNSH1 = IIF(COPRT_ID = '1STSZ' , PMPRJDT.DEST_FNSH , M.DEST_FNSH1 )
        M.DEST_FNSHP = IIF(COPRT_ID = 'PRODS' , PMPRJDT.DEST_FNSH , M.DEST_FNSHP )
      ENDSCAN
    ENDIF
    
    *- Add a line used only to show down part of page 1
    M.TYPE = 'C'
    INSERT INTO &LCTMPPOLNS FROM MEMVAR
    
    *- Initialize measurments variables
    STORE ' ' TO M.MDESC,M.TOLERANCE,M.MSZ1,M.MSZ2,M.MSZ3,M.MSZ4,M.MSZ5,M.MSZ6,M.MSZ7,M.MSZ8,M.MSZ9,M.MSZ10,M.MSZ11,M.MSZ12

    *- Get measurments
    =SEEK('P'+POSHDR.PO,'POSLN')
    =lfGetMsrmt()      

    *- Fill the data of page 2
    M.CPAGE = '2'
    M.TYPE = 'A'
    SELECT &lcTmpPoLns
    FOR lnLn = 1 TO 29 && MEMLINES(M.MDESC)
      M.CDESC = MLINE(M.MDESC,lnLn)
      M.CTOLERANCE = MLINE(M.TOLERANCE,lnLn)
      FOR lnLnsz = 1 TO 12
        lcLn = ALLTRIM(STR(lnLn))
        lcLnsz = ALLTRIM(STR(lnLnsz))
        M.CSZ&lcLnsz = MLINE(M.MSZ&lcLnsz , lnLn )
      ENDFOR
      INSERT INTO &lcTmpPoLns FROM MEMVAR         
    ENDFOR

    M.TYPE = 'B'    
    INSERT INTO &lcTmpPoLns FROM MEMVAR         
    SELECT &lcTmpPoLns 
    REPLACE CBACKNECK  WITH gfCodDes(STYLE.CBACKNECK ,'CBACKNECK ') ;
            CWASHCARE  WITH gfCodDes(STYLE.CWASHCARE ,'CWASHCARE ') ;
            CSIZELABEL WITH gfCodDes(STYLE.CSIZELABEL,'CSIZELABEL') ;
            CSWINGTAG  WITH gfCodDes(STYLE.CSWINGTAG ,'CSWINGTAG ') ;
            CADDTAG    WITH gfCodDes(STYLE.CADDTAG   ,'CADDTAG   ') ;
            CBARCODES  WITH gfCodDes(STYLE.CBARCODES ,'CBARCODES ') ;
            CKIMBALL   WITH gfCodDes(STYLE.CKIMBALL  ,'CKIMBALL  ') ;
            CBUTTONS   WITH gfCodDes(STYLE.CBUTTONS  ,'CBUTTONS  ') ;
            CZIPS      WITH gfCodDes(STYLE.CZIPS     ,'CZIPS     ') 
            
    *- Get washing symbol if any
    IF !EMPTY(STYLE.CWSYMBOL)
      lcDir = ALLTRIM(gfGetMemVar('M_WSHSYMB'))
      *B607898,1 TMI [Start] 
      IF !EMPTY(lcDir)
        *B607898,1 TMI [End  ] 
  
        *C038291,4  TMI [Start] Add backslash
        lcDir = lcDir + IIF(RIGHT(lcDir,1)<>'\','\','')
        *C038291,4  TMI [End  ] 
        *B607898,1 TMI [Start] create a new file that stores the washing symbols if not found and populate it
        *                           and discard the old way to show washing symbol
        *  lcSymbol = lcDir+ALLTRIM(STYLE.CWSYMBOL)+'.JPG'
        *  lcBmpFl = gcWorkDir+gfTempName()+'.BMP'      
        *  IF FILE(lcSymbol)
        *    COPY FILE (lcSymbol) TO (lcBmpFl)
        *    SELECT &lcTmpPoLns
        *    APPEND GENERAL GWSYMBOL FROM (lcBmpFl) LINK
        *    ERASE (lcBmpFl)
        *  ENDIF      
        =lfShWSymbl()
        *B607898,1 TMI [End  ] 
      
        *B607898,1 TMI [Start] 
      ENDIF
      *B607898,1 TMI [End  ] 
    ENDIF
    
  ENDIF
ENDSCAN
WAIT CLEAR

*-- end of lfCollect.

*:**************************************************************************
*:* Name        : lfOpnFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/09/2004
*:* Purpose     : Open needed files
*:***************************************************************************
FUNCTION lfOpnFls
IF !USED('CUTPICK')
  =gfOpenFile(gcDataDir+'CUTPICK','CUTPKORD','SH')
ENDIF
IF !USED('ORDLINE')
  =gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
ENDIF
IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
ENDIF
IF !USED('SPCK_LIN')
  =gfOpenFile(gcDataDir+'SPCK_LIN','SPCK_LIN','SH')
ENDIF
IF !USED('PMPRJDT')
  =gfOpenFile(gcDataDir+'PMPRJDT','PMPRJDT','SH')
ENDIF

*-- end of lfOpnFls.


*:**************************************************************************
*:* Name        : lfGetRatio
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/09/2004
*:* Purpose     : Get ratios of a set of numbers ( here m.qty1,...,m.qty12)
*:***************************************************************************
FUNCTION lfGetRatio
PRIVATE lnMinQ,lnI
*- Get Minimum qty for this color
lnMinQ = 0
FOR lnI = 1 TO 12
  lcI = ALLTRIM(STR(lnI))
  IF M.QTY&lcI>0
    lnMinQ = IIF(lnMinQ=0,M.QTY&lcI,MIN(lnMinQ,M.QTY&lcI))
  ENDIF
ENDFOR        

*- Get carton ratio based on the minimum qty
FOR lnM = lnMinQ TO 1 STEP -1
  IF MOD(lnMinQ,lnM) > 0 && if not divisible , do not use
    LOOP
  ENDIF

  lnRem = 0
  FOR lnI = 1 TO 12
    lcI = ALLTRIM(STR(lnI))
    lnRem = lnRem + MOD(M.QTY&lcI,lnM)
  ENDFOR
  IF lnRem = 0
    EXIT
  ENDIF
ENDFOR
lnMinQ = lnM  && This is the maximum common divisor
FOR lnI = 1 TO 12
  lcI = ALLTRIM(STR(lnI))
  M.PQTY&lcI = M.QTY&lcI/lnMinQ
ENDFOR

*-- end of lfGetRatio.

*:**************************************************************************
*:* Name        : lfGetMsrmt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/16/2004
*:* Purpose     : Initialize MDESC field if no data is saved for it in POSPEC or STYSPEC
*:***************************************************************************
FUNCTION lfGetMsrmt
PRIVATE lcCR
lcCR = CHR(13)
SET ORDER TO POSPECPO IN POSPEC
IF SEEK(POSHDR.PO,'POSPEC')
  SELECT POSPEC
  SCATTER MEMVAR MEMO
ELSE
  IF SEEK(SUBSTR(POSLN.STYLE,1,lnMajSize),'STYSPEC')
    SELECT STYSPEC
    SCATTER MEMVAR MEMO
  ELSE        
    M.MDESC = ''
    M.MDESC = M.MDESC + 'Chest  2.5cms below armhole             ' + lcCR
    M.MDESC = M.MDESC + 'Length  @ Side Neck Point               ' + lcCR
    M.MDESC = M.MDESC + 'Waist (38ms from SNP)                   ' + lcCR
    M.MDESC = M.MDESC + 'Sleeve overarm  including cuff          ' + lcCR
    M.MDESC = M.MDESC + 'Sleeve underarm including cuff          ' + lcCR
    M.MDESC = M.MDESC + 'Shoulder  (point to point)              ' + lcCR
    M.MDESC = M.MDESC + 'Shoulder drop F.I.L                     ' + lcCR
    M.MDESC = M.MDESC + 'Armhole (measured straight)             ' + lcCR
    M.MDESC = M.MDESC + 'Neck width inside/seam to seam          ' + lcCR
    M.MDESC = M.MDESC + 'Front Neck drop  F.I.L                  ' + lcCR
    M.MDESC = M.MDESC + 'Back Neck drop F.I.L                    ' + lcCR
    M.MDESC = M.MDESC + 'Neckband                                ' + lcCR
    M.MDESC = M.MDESC + 'Cuffs width (relaxed)                   ' + lcCR
    M.MDESC = M.MDESC + 'Cuffs depth (relaxed)                   ' + lcCR
    M.MDESC = M.MDESC + 'Welt /hem width  (relaxed)              ' + lcCR
    M.MDESC = M.MDESC + 'Welt /hem depth                         ' + lcCR
    M.MDESC = M.MDESC + 'Pocket depth                            ' + lcCR
    M.MDESC = M.MDESC + 'Pocket width                            ' + lcCR
    M.MDESC = M.MDESC + 'Neck to pocket                          ' + lcCR
    M.MDESC = M.MDESC + 'C.F. to pocket                          ' + lcCR
    M.MDESC = M.MDESC + 'Placket length                          ' + lcCR
    M.MDESC = M.MDESC + 'Placket width                           ' + lcCR
    M.MDESC = M.MDESC + 'Bicep at armhole                        ' + lcCR
    M.MDESC = M.MDESC + 'X-Front @ centre armhole 15cms from SNP ' + lcCR
    M.MDESC = M.MDESC + 'X-Back @ centre armhole 14cms from SNP  ' + lcCR
    M.MDESC = M.MDESC + 'Collar point                            ' + lcCR
    M.MDESC = M.MDESC + 'Hem curve                               ' + lcCR
    M.MDESC = M.MDESC + '                                        ' + lcCR
    M.MDESC = M.MDESC + '                                        ' 
    
    M.TOLERANCE = '1  ' + lcCR +;
                  '1  ' + lcCR +;
                  '1  ' + lcCR +;
                  '1  ' + lcCR +;
                  '1  ' + lcCR +;
                  '1  ' + lcCR +;
                  '0.5' + lcCR +;  
                  '0.5' + lcCR +;  
                  '0.5' + lcCR +;  
                  '0.5' + lcCR +;  
                  '0.5' + lcCR +;  
                  '0.5' + lcCR +;       
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '1  ' + lcCR +;    
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '0.5' + lcCR +;      
                  '   ' + lcCR +;
                  '   '
                  
  ENDIF
ENDIF

*-- end of lfGetMsrmt.



*:**************************************************************************
*:* Name        : lfCrWSymbl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/21/2006
*:* Purpose     : Create the wash symbol file if not found
*:***************************************************************************
*B607898,1 TMI 
FUNCTION lfCrWSymbl
PRIVATE lnSlct
lnSlct = SELECT()

*- Call this if file not found before
IF !FILE(gcDataDir+'WSYMBOL.DBF')
  lcWSymbol = gfTempName()
  CREATE TABLE (gcDataDir+'WSYMBOL') (CWSYMBOL C(6),GWSYMBOL G)
  INDEX ON CWSYMBOL TAG WSYMBOL
  SELECT DISTINCT CWSYMBOL FROM STYLE WHERE !EMPTY(CWSYMBOL) INTO TABLE  (gcWorkDir+lcWSymbol)
  SELECT WSYMBOL
  APPEND FROM (gcWorkDir+lcWSymbol)
  
  SCAN
    =lfApGen()  
  ENDSCAN
  
  *- use the file to open it in shared mode
  USE IN WSYMBOL
ENDIF

SELECT (lnSlct)
*-- end of lfCrWSymbl.

*:**************************************************************************
*:* Name        : lfShWSymbl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/20/2006
*:* Purpose     : collect washing symbols
*:***************************************************************************
*B607898,1 TMI
FUNCTION lfShWSymbl
PRIVATE lnSlct,lcWSymbol,lcJPG
lnSlct = SELECT()
lcJPG  = ''

*- Create the file, set the relation
=lfCrWSymbl()

*- open the symbols file
IF !USED('WSYMBOL')
  USE (gcDataDir+'WSYMBOL') IN 0 ORDER WSYMBOL    
ENDIF

*- set the relation to style file
SELECT &lcTmpPoLns
IF !'WSYMBOL' $ SET('RELATION')
  SET RELATION TO CWSYMBOL INTO WSYMBOL ADDITIVE
ENDIF

*- Update the cwsymbol field in the temp header file 
SELECT &lcTmpPoLns
* changed as per the note added to the ticket  T20060919.0001 dated 02/01/2007 by Tarek Ibrahim
*REPLACE CWSYMBOL WITH STYLE.CWSYMBOL
lnRecno = RECNO()
lcPo = PO
=SEEK(lcPO)
REPLACE CWSYMBOL WITH STYLE.CWSYMBOL ;
        REST WHILE PO+cPage+TYPE = lcPO
GOTO (lnRecno)        
* end

IF !SEEK(STYLE.CWSYMBOL,'WSYMBOL')
  SELECT WSYMBOL
  APPEND BLANK
  REPLACE CWSYMBOL WITH STYLE.CWSYMBOL
  lcJPG = lfApGen()
ELSE
  IF EMPTY(WSYMBOL.GWSYMBOL)
    lcJPG = lfApGen()
  ENDIF
ENDIF

IF !EMPTY(lcJPG)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The file "&lcJPG" must be saved in a Bitmap format.')
ENDIF

SELECT (lnSlct)
*-- end of lfShWSymbl.

*:**************************************************************************
*:* Name        : lfApGen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/20/2006
*:* Purpose     : Append general
*:***************************************************************************
*B607898,1 TMI
FUNCTION lfApGen
PRIVATE lcSymbol,lcBmpFl,lcSvErr,llError
lcSvErr = ON('ERROR')
llError = .F.
lcSymbol = lcDir+ALLTRIM(WSYMBOL.CWSYMBOL)+'.BMP'
IF FILE(lcSymbol)
  SELECT WSYMBOL
  * commented as per the note added to the ticket  T20060919.0001 dated 02/01/2007 by Tarek Ibrahim
  *ON ERROR llError = .T.
  *APPEND GENERAL GWSYMBOL FROM (lcSymbol)
  *ON ERROR &lcSvErr
ENDIF
RETURN IIF(llError,lcSymbol,'')
*-- end of lfApGen.
