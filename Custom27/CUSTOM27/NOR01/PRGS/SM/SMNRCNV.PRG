*:**************************************************************************
*: Program file  : SMNRCNV.PRG
*: Program desc. : Custom program to Convert the data from AS400 to Aria for NOR01
*: Date          : 03/18/2004
*: System        : Aria Advantage Series.
*: Module        : SM2
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Purpose       : Convert the data from AS400 to Aria.
*: Entry #       : C # 122068,1
*:**************************************************************************
*--Calling the option grid


DIMENSION laRpSource[3],laRpTarget[1]
laRpSource[1] = 'Codes'
laRpSource[2] = 'Customer'
laRpSource[3] = 'Style'

laRpTarget[1] = ''

STORE '' TO lcRpSlcted, lcRpCust, lcRpStyle,lcRpStyClr,lcRpStyUpc, lcRpCodes
STORE '' TO lcClrSpr, lcSclSpr 
*C122068,5  TMI [Start] 
lcRpClrDir = ''
*C122068,5  TMI [End  ] 
*C122068,6  TMI [Start] Define variables for Customer notes , cross refrence file and GLN id file
STORE ' ' TO lcRpCucmpd,lcRpCrosref,lcRpGlnid
*C122068,6  TMI [End  ] 

*- Restore variables from temp file saved with the current user and active company
lcSvMem = gcWorkDir+STRTRAN(PADR(gcUSER_ID,6),' ','_') + gcACT_COMP + '.MEM'
IF FILE(lcSvMem)
  RESTORE FROM (lcSvMem) ADDITIVE  
ENDIF  
*- Save variables so if they overwritten by OG
lcVar1 = lcRpSlcted
lcVar2 = lcRpCust
lcVar3 = lcRpStyle
lcVar4 = lcRpStyClr
lcVar5 = lcRpStyUpc
lcVar6 = lcRpCodes

*C122068,6  TMI [Start] 
lcVar7 = lcRpCucmpd
lcVar8 = lcRpCrosref
lcVar9 = lcRpGlnid
lcVar10 = lcRpClrDir
*C122068,6  TMI [End  ] 

lcUpdRec = 'N'     && Records to Update        
llShwAlMsg = .T.   && Show All info. Messages  

DIMENSION laTmpFl[1]  && An array that holds temp file names to erase them after end the program
laTmpFl[1] = ''
STORE 0 TO lnTmpCnt

*-Open needed files for convert
IF NOT lfOpnFl()
  RETURN
ENDIF  

*- Check if this valid co.
IF !lfValidCo()
  RETURN
ENDIF  

*-Call Option Grid
lcExpr = gfOpGrid('SMNRCNV' , .T.)

SAVE TO (lcSvMem) ALL LIKE l?Rp*

IF EMPTY(lcExpr)
  RETURN
ENDIF

*C122068,3  TMI [Start] Set the correct order for all files opened
=lfSetOrder()
*C122068,3  TMI [End  ] 

*-Convert codes and scales data
=IIF('Codes'    $ lcRpSlcted .AND. !EMPTY(lcRpCodes) , lfCodes() , '' )

*-Convert customrs data
=IIF('Customer' $ lcRpSlcted .AND. !EMPTY(lcRpCust) , lfCust()  , '' )

*-Convert Styles data
=IIF('Style'    $ lcRpSlcted .AND. !EMPTY(lcRpStyle)  .AND. ;
                                   !EMPTY(lcRpStyClr) .AND. ;
                                   !EMPTY(lcRpStyUpc) ,     ;
                                 lfStyles() , '' )

IF NOT llShwAlMsg
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Data converted successfully.')
ENDIF

*- Erase not needed temp files
=lfErase()	

*:**************************************************************************
*:* Name        : lfValidCo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : *- Check if this valid co.
*:***************************************************************************
FUNCTION lfValidCo
*- I need this company to be extended style with style code structure  8 - 6 - 3
*- and the scale has dimension 1
DO CASE   
  CASE !gfGetMemvar('M_USEEXSSC')
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
    'This is not an extended size scale company.')
    RETURN .F.
    
  *C122068,6  TMI [Start] let it must be 2 characters for dimensions
  *CASE gfGetMemvar('M_EXTWIDTH') <> 1
  *  =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
  *  'Extended Size Scale Width must be "One Character".')
  CASE gfGetMemvar('M_EXTWIDTH') <> 2
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
    'Extended Size Scale Width must be "Two Character".')
    *C122068,6  TMI [End  ] 
    RETURN .F.
    
  CASE !lfStyStru()
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
    'Extended Size Scale Structure must be (Style=8, Color=6, Scale=3).')
    RETURN .F.

  CASE gfGetMemvar('M_WAREHOUS') = 'N'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
    'The company must Use multiple locations.')
    RETURN .F.

  CASE ! 'IC' $ gcCmpModul
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'IC module is not installed.')
    RETURN .F.
  
  CASE ! 'PO' $ gcCmpModul
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'PO module is not installed.')
    RETURN .F.
  
  CASE gfGetMemvar('M_LIMPCOST') = .T.
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,;
    'Use detailed costing option in PO module setup must be set to "No".')
    RETURN .F.
  
ENDCASE

     
*-- end of lfValidCo.

*:**************************************************************************
*:* Name        : lfStyStru
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : Get style structure information
*:***************************************************************************
FUNCTION lfStyStru
PRIVATE lnSlct, laItemSeg, lnCount
lnSlct = Select()
DECLARE laItemSeg[1]

*- If the item structure is not set yet
IF !SEEK('U','ICISTRU')
  RETURN .F.
ENDIF  

=gfItemMask(@laItemSeg)

FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
    CASE laItemSeg[lnCount,1]='F'
      IF LEN(laItemSeg[lnCount,3]) <> 8
        RETURN .F.
      ENDIF
      lcClrSpr = ALLTRIM(laItemSeg[lnCount,6])
      
    CASE laItemSeg[lnCount,1]='C'
      IF LEN(laItemSeg[lnCount,3]) <> 6
        RETURN .F.
      ENDIF
      lcSclSpr = ALLTRIM(laItemSeg[lnCount,6])
      
    CASE laItemSeg[lnCount,1]='S'
      IF LEN(laItemSeg[lnCount,3]) <> 3
        RETURN .F.
      ENDIF
    
  ENDCASE
ENDFOR

SELECT (lnSlct)
*-- end of lfStyStru.

*:**************************************************************************
*:* Name        : lfvSlctfl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/18/2004
*:* Purpose     : Select Excel file
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvSlctfl
=gfMover(@laRpSource,@laRpTarget,"Select file to update Aria with.",.T.,'',.T.)
lcRpSlcted = ''
FOR lnI = 1 TO ALEN(laRpTarget)  
  IF !EMPTY(laRpTarget[lnI]) 
    lcRpSlcted = lcRpSlcted + '|' + laRpTarget[lnI]
  ENDIF
ENDFOR
CLEAR READ
*-- end of lfvSlctfl.

*:**************************************************************************
*:* Name        : lfwRepWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/22/2004
*:* Purpose     : OG When funcion 
*:***************************************************************************
FUNCTION lfwRepWhen

*lcRpSlcted = lcVar1
lcRpCust   = lcVar2
lcRpStyle  = lcVar3
lcRpStyClr = lcVar4
lcRpStyUpc = lcVar5
lcRpCodes  = lcVar6

*C122068,6  TMI [Start] restore additional variables
lcRpCucmpd = lcVar7 
lcRpCrosref = lcVar8 
lcRpGlnid = lcVar9 
lcRpClrDir = lcVar10
*C122068,6  TMI [End  ] 

*- Disable save options button
SHOW GET pbOGSave  DISABLE
SHOW GET pbOGReset DISABLE
*-- end of lfwRepWhen.

*:**************************************************************************
*:* Name        : lfvGtFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/18/2004
*:* Purpose     : Select Excel file to update Aria with 
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvGtFl
PARAMETERS lcVarNm,lcExt
IF !EMPTY(&lcVarNm) .AND. !FILE(&lcVarNm)
  &lcVarNm = GETFILE(lcExt)
ENDIF  
*-- end of lfvGtFl.

*:**************************************************************************
*:* Name        : lfOpnFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/18/2004
*:* Purpose     : Open needed files
*:***************************************************************************
FUNCTION lfOpnFl
PRIVATE llRet
llRet = .T.
=IIF(llRet,lfOpn('CUSTOMER','CUSTOMER'),'')
=IIF(llRet,lfOpn('ARCUSHST','ACTHST'),'')
=IIF(llRet,lfOpn('CODES','CCODE_NO'),'')
=IIF(llRet,lfOpn('STYLE','STYLE'),'')
=IIF(llRet,lfOpn('STYDYE','STYDYE'),'')
=IIF(llRet,lfOpn('SCALE','SCALE'),'')
=IIF(llRet,lfOpn('SCALEHD','EXTSCALE'),'')
=IIF(llRet,lfOpn('WAREHOUS','WAREHOUS'),'')
=IIF(llRet,lfOpn('STYLEUPC','STYLEUPC'),'')
=IIF(llRet,lfOpn('BOM','BOM'),'')
=IIF(llRet,lfOpn('ICSTYHST','STYHST'),'')
=IIF(llRet,lfOpn('ICSEGVAL','SEGVAL'),'')
=IIF(llRet,lfOpn('icistru','SEGNO'),'')
*C122068,3  TMI [Start] Open setups,SYCFACT files
=IIF(llRet,lfOpn('SETUPS','VARNAME'),'')
=IIF(llRet,lfOpn('SYCFACT','CFACCODE',.T.),'')
*C122068,3  TMI [End  ] 
*C122068,6  TMI [Start] Open notepad file
=IIF(llRet,lfOpn('NOTEPAD','NOTEPAD'),'')
*C122068,6  TMI [End  ] 
RETURN llRet
*-- end of lfOpnFl.

*:**************************************************************************
*:* Name        : lfOpn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : Open files
*:***************************************************************************
FUNCTION lfOpn
PARAMETERS lcFl,lcOrder,llSysfile
lcDir = IIF(llSysfile,gcSysHome,gcDataDir)
IF FILE(lcDir+lcFl+'.DBF')
  *C122068,6  TMI [Start] Open files exclusivly
  *=gfOpenFile(lcDir+lcFl,lcOrder,'SH')
  PRIVATE llOpen
  llOpen=gfOpenFile(lcDir+lcFl,lcOrder,'EX')
  *C122068,6  TMI [End  ] 
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'File '+lcDir+lcFl+'.DBF does not exist.')
  llRet = .F.
ENDIF  

*-- end of lfOpn.

*:**************************************************************************
*:* Name        : lfSetOrder
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/31/2004
*:* Purpose     :  Set the correct order for all files opened
*:***************************************************************************
*C122068,3
FUNCTION lfSetOrder
SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO ACTHST IN ARCUSHST
SET ORDER TO CCODE_NO IN CODES
SET ORDER TO STYLE IN STYLE
SET ORDER TO STYDYE IN STYDYE
SET ORDER TO SCALE IN SCALE
SET ORDER TO EXTSCALE IN SCALEHD
SET ORDER TO WAREHOUS IN WAREHOUS
SET ORDER TO STYLEUPC IN STYLEUPC
SET ORDER TO BOM IN BOM
SET ORDER TO STYHST IN ICSTYHST
SET ORDER TO SEGVAL IN ICSEGVAL
SET ORDER TO SEGNO IN icistru
*-- end of lfSetOrder.

*:**************************************************************************
*:* Name        : lfCodes
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/21/2004
*:* Purpose     : Get Codes 
*:***************************************************************************
FUNCTION lfCodes

PRIVATE lnI,laFileSt

*C122068,9  TMI [Start] Do not use this variable 'lcUpdRec'
lcUpdRec = 'x'
*C122068,9  TMI [End  ] 

*- Create "codestypes" that contains main types found in both AS400 and Aria
lnI = 0 

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ELMTYPE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 2
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CFLD_NAME'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ELMDESC'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 30
laFileSt[lnI,4] = 0

lcTmpTypFl = lfTmpFlNm()
CREATE TABLE (gcWorkDir+lcTmpTypFl) FROM ARRAY laFileSt
INDEX ON ELMTYPE TAG &lcTmpTypFl

*-Fill lcTmpTypFl file with related codes
INSERT INTO &lcTmpTypFl VALUES ('BA',	'BANK'        ,'Banks')
INSERT INTO &lcTmpTypFl VALUES ('BO',	'CPURCODE'    ,'Purchasing Group')
INSERT INTO &lcTmpTypFl VALUES ('CB',	'TRANCODE'    ,'Debit Adj Reasons')
INSERT INTO &lcTmpTypFl VALUES ('CF',	'COLOR'       ,'Colors')
INSERT INTO &lcTmpTypFl VALUES ('CO',	'COLOR'       ,'Colors')
INSERT INTO &lcTmpTypFl VALUES ('CR',	'CADJREASON'  ,'Inventory Adjust Reson')
INSERT INTO &lcTmpTypFl VALUES ('CT',	'CLASS'       ,'Customer Class.')
INSERT INTO &lcTmpTypFl VALUES ('CX',	'CCANCRESON'  ,'Order Cancellation Reason')
INSERT INTO &lcTmpTypFl VALUES ('FA',	'CADJREASON'  ,'Inventory Adjust Reson')
INSERT INTO &lcTmpTypFl VALUES ('FG',	'ITEM_TYPE'   ,'Fabric Type')
INSERT INTO &lcTmpTypFl VALUES ('HR',	'CCANCRESON'  ,'Order Decline Reason')
INSERT INTO &lcTmpTypFl VALUES ('OT',	'CORDERCAT'   ,'Order Category')
INSERT INTO &lcTmpTypFl VALUES ('PG',	'CSTYGROUP'   ,'Style Groups')
INSERT INTO &lcTmpTypFl VALUES ('Q2',	'SPCINST'     ,'Special Instructions')
INSERT INTO &lcTmpTypFl VALUES ('Q3',	'CDIVISION'   ,'Divisions')
*C122068,3  TMI [Start] Add divisions
INSERT INTO &lcTmpTypFl VALUES ('DV',	'CDIVISION'   ,'Divisions')
*C122068,3  TMI [End  ] 
INSERT INTO &lcTmpTypFl VALUES ('RG',	'REGION'      ,'Regions')
INSERT INTO &lcTmpTypFl VALUES ('SE',	'SEASON'      ,'Seasons')
INSERT INTO &lcTmpTypFl VALUES ('SI',	'SPCINST'     ,'Special Instructions')
INSERT INTO &lcTmpTypFl VALUES ('ST',	'STATE'       ,'States')
INSERT INTO &lcTmpTypFl VALUES ('SV',	'SHIPVIA'     ,'Ship via')
INSERT INTO &lcTmpTypFl VALUES ('SW',	'SHIPVIA'     ,'Ship via')
INSERT INTO &lcTmpTypFl VALUES ('TN',	'CTERMCODE'   ,'Terms')
*C122068,6  TMI [Start] 2)Terms Code - Replace the Element type TN with TM, details in the CNTLP file 
INSERT INTO &lcTmpTypFl VALUES ('TM',	'CTERMCODE'   ,'Terms')
*C122068,6  TMI [End  ] 

*- Create "codesfile" that contains the actual codes exsits in AS400 systme
lnI = 0 

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'TYPE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 2
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CCODE_NO'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 6
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CDISCREP'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 60
laFileSt[lnI,4] = 0

lcTmpCodFl = lfTmpFlNm()
CREATE TABLE (gcWorkDir+lcTmpCodFl) FROM ARRAY laFileSt
INDEX ON TYPE+CCODE_NO TAG lcTmpCodFl

*- Fill codesfile file
*C122068,3  TMI [Start] Check that Codes file is correct based on 3rd field in the temp file
*IF !lfAppend(lcRpCodes,'XLS','Element Type',1,'Codes')
IF !lfAppend(lcRpCodes,'XLS','Element Description',3,'Codes')
  *C122068,3  TMI [End  ] 
  RETURN
ENDIF  

*-Start updating Aria codes file
SET ORDER TO CCODE_NO IN CODES

*C122068,9  TMI [Start] Zap codes file befor updating
SELECT CODES
ZAP
*C122068,9  TMI [End  ] 

SELECT &lcTmpTypFl
GO TOP
SCAN
  IF SEEK(&lcTmpTypFl..ELMTYPE,lcTmpCodFl)    
    WAIT WINDOW NOWAIT 'Updating the code :'+ &lcTmpTypFl..ELMDESC
    SELECT &lcTmpCodFl
    SCAN REST WHILE TYPE+CCODE_NO = &lcTmpTypFl..ELMTYPE ;
              FOR !EMPTY(&lcTmpCodFl..CCODE_NO) .AND. ;
                  !'*' $ &lcTmpCodFl..CCODE_NO
      llNew = .F.
      SELECT CODES
      *C122068,3  TMI [Start] Add default code
      IF !SEEK('D'+&lcTmpTypFl..CFLD_NAME,'CODES')
        APPEND BLANK
        REPLACE CDEFCODE   WITH 'D' ;
                CFLD_NAME  WITH &lcTmpTypFl..CFLD_NAME ;
                CCODE_NO   WITH &lcTmpCodFl..CCODE_NO ;
                CDISCREP   WITH &lcTmpCodFl..CDISCREP ;
                CRLTFIELD  WITH 'N' 
        =lfAdUsrInf()
      ENDIF
      *C122068,3  TMI [End  ] 

      IF !SEEK('N'+&lcTmpTypFl..CFLD_NAME+&lcTmpCodFl..CCODE_NO,'CODES')
        APPEND BLANK
        llNew = .T.
      ENDIF
      IF lcUpdRec = 'A' .OR. llNew
        REPLACE CDEFCODE   WITH 'N' ;
                CFLD_NAME  WITH &lcTmpTypFl..CFLD_NAME ;
                CCODE_NO   WITH &lcTmpCodFl..CCODE_NO ;
                CDISCREP   WITH &lcTmpCodFl..CDISCREP ;
                CRLTFIELD  WITH 'N' 
        =lfAdUsrInf()
      ENDIF 
      
      *C122068,3  TMI [Start] Update SYCFACT file with factors
      IF &lcTmpTypFl..CFLD_NAME = 'BANK      ' .AND. !SEEK(&lcTmpCodFl..CCODE_NO,'SYCFACT')
        SELECT SYCFACT
        APPEND BLANK
        REPLACE CFACCODE  WITH &lcTmpCodFl..CCODE_NO ;
                CFACCOMP  WITH &lcTmpCodFl..CDISCREP ;
                CADDRESS6 WITH 'USA'
        =lfAdUsrInf()
      ENDIF
      *C122068,3  TMI [End  ]       
    ENDSCAN
    
  ENDIF  
ENDSCAN

*C122068,6  TMI [Start] Add Milberg as a Bank
IF !SEEK('NBANK      MLBERG','CODES')
  SELECT CODES
  APPEND BLANK
  REPLACE CDEFCODE   WITH 'N' ;
          CFLD_NAME  WITH 'BANK' ;
          CCODE_NO   WITH 'MLBERG' ;
          CDISCREP   WITH 'Milberg' ;
          CRLTFIELD  WITH 'N' 
  =lfAdUsrInf()
ENDIF  
IF !SEEK('MLBERG','SYCFACT')
  SELECT SYCFACT
  APPEND BLANK
  REPLACE CFACCODE  WITH 'MLBERG' ;
          CFACCOMP  WITH 'Milberg' ;
          CADDRESS6 WITH 'USA'
  =lfAdUsrInf()
ENDIF
*C122068,6  TMI [End  ] 

*-Get warehouses
*-'WH',	**********	,'Warehouses'
SELECT &lcTmpCodFl
IF SEEK('WH',lcTmpCodFl)
  SCAN REST WHILE TYPE+CCODE_NO = 'WH' ;
            FOR !EMPTY(&lcTmpCodFl..CCODE_NO) .AND. ;
                !'*' $ &lcTmpCodFl..CCODE_NO
    WAIT WINDOW NOWAIT 'Updating WareHous file with : ' + &lcTmpCodFl..CCODE_NO + ' ' + &lcTmpCodFl..CDISCREP
    llNew = .F.
    SELECT WAREHOUS    
    IF !SEEK(&lcTmpCodFl..CCODE_NO,'WAREHOUS')
      APPEND BLANK
      llNew = .T.
    ENDIF
    IF lcUpdRec = 'A' .OR. llNew    
      REPLACE CWARECODE  WITH &lcTmpCodFl..CCODE_NO ;
              CDESC      WITH &lcTmpCodFl..CDISCREP ;
              LSTYINV    WITH .T. ;
              CADDRESS6  WITH 'USA' ;
              CCONT_CODE WITH 'USA'
      =lfAdUsrInf()
    ENDIF
  ENDSCAN
ENDIF

*C122068,6  TMI [Start] ask to delete scale file befor processing
IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Delete Scales befor processing?') = 1
  SELECT SCALE
  ZAP
  SELECT SCALEHD
  ZAP
ENDIF
*C122068,6  TMI [End  ] 

*-Get Scales
*-'SC',	**********	,'Scales'
SELECT &lcTmpCodFl
IF SEEK('SC',lcTmpCodFl)
  SCAN REST WHILE TYPE+CCODE_NO = 'SC' ;
            FOR !EMPTY(&lcTmpCodFl..CCODE_NO) .AND. ;
                !'*' $ &lcTmpCodFl..CCODE_NO
    llNew = .F.
    SELECT SCALEHD
    IF !SEEK(ALLTRIM(&lcTmpCodFl..CCODE_NO),'SCALEHD')
      APPEND BLANK
      llNew = .T.
    ENDIF
    IF lcUpdRec = 'A' .OR. llNew
      WAIT WINDOW NOWAIT 'Updating SCALE file with scale : ' + PADR(&lcTmpCodFl..CCODE_NO,1)
      *C122068,6  TMI [Start] let width of scale be two characters
      *REPLACE CEXTSCALE WITH PADR(ALLTRIM(&lcTmpCodFl..CCODE_NO),1) ;
              NNOOFDIM  WITH 1 ;
              CDIM1DESC WITH ALLTRIM(&lcTmpCodFl..CCODE_NO) 
      REPLACE CEXTSCALE WITH PADR(ALLTRIM(&lcTmpCodFl..CCODE_NO),2,'0') ;
              NNOOFDIM  WITH 1 ;
              CDIM1DESC WITH ALLTRIM(&lcTmpCodFl..CCODE_NO) 
      *C122068,6  TMI [End  ] 
      =lfAdUsrInf()
    ENDIF
    lcSizes = ALLTRIM(&lcTmpCodFl..CDISCREP)
    lcSizes = STRTRAN(lcSizes,CHR(13),'')
    FOR lnSpace = 5 TO 1 STEP -1
      lcSizes = STRTRAN(lcSizes,SPACE(lnSpace),',')
    ENDFOR
    SELECT SCALEHD
    REPLACE CSCALEDES WITH lcSizes

    lcSizes = lcSizes+','
    lnSzs = OCCURS(',',lcSizes)
    FOR lnK = 1 TO lnSzs
      lcSz = SUBSTR(lcSizes,1,AT(',',lcSizes)-1)
      lcSizes = SUBSTR(lcSizes,AT(',',lcSizes)+1)
      *C122068,6  TMI [Start] let scale be two characters width
      *lcScNo = '0'+STR(CEILING(lnK/8),1)
      lcScNo = STR(CEILING(lnK/8),1)
      *C122068,6  TMI [End  ] 
      
      llNew = .F.
      SELECT SCALE
      *C122068,6  TMI [Start] let scale be two characters width
      *IF !SEEK('S'+PADR(&lcTmpCodFl..CCODE_NO,1)+lcScNo,'SCALE')
      IF !SEEK('S'+PADR(ALLTRIM(&lcTmpCodFl..CCODE_NO),2,'0')+lcScNo,'SCALE')
        *C122068,6  TMI [End  ] 
        APPEND BLANK
        *C122068,6  TMI [Start] scale will be two characters width
        *REPLACE TYPE   WITH 'S' ;
                SCALE  WITH PADR(&lcTmpCodFl..CCODE_NO,1)+lcScNo 
        REPLACE TYPE   WITH 'S' ;
                SCALE  WITH PADR(ALLTRIM(&lcTmpCodFl..CCODE_NO),2,'0')+lcScNo 
        =lfAdUsrInf()
        *C122068,6  TMI [End  ] 
        llNew = .T.
      ENDIF
      *C122068,10 TMI [Start] Comment this line
      *IF lcUpdRec = 'A' .OR. llNew
      *C122068,10 TMI [End  ] 
      lcK = IIF(MOD(lnK,8)=0,'8',STR(MOD(lnK,8),1))
      REPLACE SZ&lcK WITH lcSz ;
              CNT    WITH VAL(lcK)
      *C122068,10 TMI [Start] Comment this line
      *ENDIF          
      *C122068,10 TMI [End  ] 
    ENDFOR  
    *-Clear rest of field in scale file if they were previously filled      
    IF lcUpdRec = 'A' .OR. llNew
      IF MOD(lnSzs,8) > 0
        FOR lnRest = MOD(lnSzs,8)+1 TO 8
          lcRest = STR(lnRest,1)
          REPLACE SZ&lcRest WITH ''
        ENDFOR
      ENDIF
    ENDIF
    
  ENDSCAN
ENDIF
WAIT CLEAR
IF llShwAlMsg
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Data converted to Codes file successfully.')
ENDIF
*-- end of lfCodes.

*:**************************************************************************
*:* Name        : lfCust
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/18/2004
*:* Purpose     : Collect data for customer file
*:***************************************************************************
FUNCTION lfCust

PRIVATE lnI,laFileSt

*- Create the temp file to append to it the customer excel file
lnI = 0 
lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ACCOUNT'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 5
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'STORE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 8
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ACCTYPE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 1
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'STNAME'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 30
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CADDRESS1'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 30
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CADDRESS2'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 30
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CADDRESS3'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 30
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CADDRESS4'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 30
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CADDRESS5'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 30
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ALPHALOKUP'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 30
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'AlphaCrsRf'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'PHONE1'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'SHIPVIA'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 6
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'REGION'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 6
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CTERMCODE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 6
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'BillToRf'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 5
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'FactorFlag'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 1
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ShipZone'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CUSTTYPE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 6
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'DtAcctStrt'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CDShipTo'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'Custxref'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'AllocPr'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ARType'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'LegalName'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'MxNoOrdAcc'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'DIST_CTR'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 8
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'CADRS3RD'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 30
laFileSt[lnI,4] = 0

lcTmpFl = lfTmpFlNm()
CREATE TABLE (gcWorkDir+lcTmpFl) FROM ARRAY laFileSt
INDEX ON ACCOUNT+ACCTYPE TAG &lcTmpFl

*C122068,3  TMI [Start] Check that Customers file is correct based on 5th field in the temp file
*IF !lfAppend(lcRpCust,'XLS','Sold To',1,'Customers')
IF !lfAppend(lcRpCust,'XLS','Address 1st. Line',5,'Customers')
  *C122068,3  TMI [End  ] 
  RETURN
ENDIF  


*C122068,6  TMI [Start] Get the customer notes from the file CUCMPD.xls
*- Create temp file to get customer notes
lnI = 0 
lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ACCOUNT'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 5
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'STORE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 8
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ACCTYPE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 2
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ENTRDATE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 10
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'NOTELN1'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 80
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'NOTELN2'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 80
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'NOTELN3'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 80
laFileSt[lnI,4] = 0

IF !EMPTY(lcRpCucmpd)
  lcCucmpd = lfTmpFlNm()
  CREATE TABLE (gcWorkDir+lcCucmpd) FROM ARRAY laFileSt
  INDEX ON ACCOUNT TAG &lcCucmpd
  IF !lfAppend(lcRpCucmpd,'XLS','Comment text - Line 1',5,'Customer Notes')
    RETURN
  ENDIF
ENDIF  

*- Remove notes saved in NOTEPAD file
SELECT NOTEPAD
IF SEEK('A')
  DELETE REST WHILE TYPE+KEY = 'A'
ENDIF

*- Create temp file for CROSS REFRENCE AND GLNID
lnI = 0 
lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ACCOUNT'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 5
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'STORE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 8
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'GLNID'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 13
laFileSt[lnI,4] = 0

lcGlnfl = lfTmpFlNm()
CREATE TABLE (gcWorkDir+lcGlnfl) FROM ARRAY laFileSt
INDEX ON ACCOUNT+STORE TAG &lcGlnfl
IF !EMPTY(lcRpCrosref)
  IF !lfAppend(lcRpCrosref,'XLS','Cust. Ship-to',3,'Customers Cross Ref.')
    RETURN
  ENDIF
ENDIF
IF !EMPTY(lcRpGlnid)  
  IF !lfAppend(lcRpGlnid,'XLS','Cust. Ship-to',3,'Wall-Mart GLN ID')
    RETURN
  ENDIF
ENDIF  

*C122068,6  TMI [End  ]

*- Update the Customer file
*Note the following types
*Type 1 - means bill to (master account)
*Type 2 - Ship To
*Type 3 - Is the billing address to which the invoice should be sent, WHEN
*the bill to reference field, for a type 2 customer has been filled in.
*Type 6 - is a distribution center

*SET ORDER TO CCODE_NO IN CODES

*C122068,6  TMI [Start] Ask to delete styles data befor process
IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Delete Customer file befor processing?') = 1
  SELECT CUSTOMER
  ZAP
  SELECT ARCUSHST
  ZAP
  SELECT NOTEPAD
  ZAP
*C122068,6  TMI [End  ] 

ENDIF
*C122068,6  TMI [End  ] 


SELECT &lcTmpFl
LOCATE FOR ACCTYPE = '1'  && Get first main account

DO WHILE !EOF()
  SELECT CUSTOMER
  SCATTER MEMVAR BLANK

  SELECT &lcTmpFl
  SCATTER MEMVAR
  
  M.BTNAME      = &lcTmpFl..STNAME
  M.TYPE        = "M"  
  M.BILLTO      = "M"
  M.STATUS      = "A"
  M.LINK_CODE   = "DEFDEF"
  M.CONSOL      = "N"
  *C122068,3  TMI [Start] Add customer priority
  *M.PRIORITY    = "5  "
  M.PRIORITY  = ALLTRIM(&lcTmpFl..AllocPr)
  *C122068,3  TMI [End  ] 
  M.CINSUR      = "Y"
  M.PRICELVL    = "A"
  M.PRNT_STATM  = "Y"
  M.CTAXRULE    = " 1"  
  M.CCONT_CODE  = "USA   "  
  M.CADDRESS6   = "USA                 "  
  M.CADDRESS12  = M.CADDRESS1
  M.CADDRESS22  = M.CADDRESS2
  M.CADDRESS32  = M.CADDRESS3
  M.CADDRESS42  = M.CADDRESS4
  M.CADDRESS52  = M.CADDRESS5
  M.CADDRESS62  = M.CADDRESS6
  M.CCONT_COD2  = M.CCONT_CODE  
  M.CBACKORD    = "A"  
  M.CCURRCODE   = "USD"
  M.CSLSGLLINK  = "DEF"
  *C122068,3  TMI [Start] Updating customer class,Factored customers
  M.CLASS = PADR(ALLTRIM(&lcTmpFl..CUSTTYPE),6)
  IF &lcTmpFl..FactorFlag = 'F'
    *C122068,6  TMI [Start] Bank code is Milberg not CIT Bank
    *M.CFACCODE = 'CITI  '
    M.CFACCODE = 'MLBERG'
    *C122068,6  TMI [End  ] 
  ENDIF
  *C122068,3  TMI [End  ] 
  
  =lfUpdcodes('CLASS     ','M.CLASS')
  =lfUpdcodes('CTERMCODE ','M.CTERMCODE')
  =lfUpdcodes('CDIVISION ','M.CDIVISION')
  =lfUpdcodes('REGION    ','M.REGION')
  =lfUpdcodes('SHIPVIA   ','M.SHIPVIA')
  =lfUpdcodes('SPCINST   ','M.SPCINST')
  
  *C122068,6  TMI [Start] Select customer alias
  SELECT CUSTOMER
  *C122068,6  TMI [End  ] 
  IF !SEEK('M'+M.ACCOUNT,'CUSTOMER')
    INSERT INTO CUSTOMER FROM MEMVAR
  ELSE
    GATHER MEMVAR 
  ENDIF
  
  IF !SEEK(M.ACCOUNT+gcCurrYear,'ARCUSHST')
    SELECT ARCUSHST
    FOR lnFsYear = -1 TO 1
      APPEND BLANK
      REPLACE ACCOUNT   WITH M.ACCOUNT ;
              CFISFYEAR WITH STR(VAL(gcCurrYear)+lnFsYear,4)
      =lfAdUsrInf()
    ENDFOR 
  ENDIF
  
  lcAccount = M.ACCOUNT
  M.TYPE = 'S'

  SELECT &lcTmpFl
  SKIP
  IF ACCTYPE $ '2|6'
    SCAN REST WHILE ACCOUNT+TYPE = lcAccount ;
              FOR ACCTYPE $ '2|6'
      SCATTER MEMVAR 
      WAIT WINDOW NOWAIT 'Updating Account : '+ M.BTNAME+' / '+M.STORE
      *C122068,6  TMI [Start] Select customer alias
      SELECT CUSTOMER
      *C122068,6  TMI [End  ] 
      IF !SEEK('S'+M.ACCOUNT+M.STORE,'CUSTOMER')
        INSERT INTO CUSTOMER FROM MEMVAR
      ELSE
        GATHER MEMVAR
      ENDIF
      
      *C122068,6  TMI [Start] Get store ID from the cross refrence file
      IF SEEK(M.ACCOUNT+M.STORE,lcGlnfl)
        IF LEN(ALLTRIM(&lcGlnfl..GLNID)) > 8
          REPLACE CSTOREGLN  WITH &lcGlnfl..GLNID
        ELSE
          REPLACE STORE WITH ALLTRIM(&lcGlnfl..GLNID)
        ENDIF
      ENDIF
      *C122068,6  TMI [End  ] 

    ENDSCAN
  ENDIF

  IF LASTKEY() = 27
    IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you need to exit.') = 1
      EXIT
    ENDIF
  ENDIF
  
  *C122068,6  TMI [Start] Add notes to note file
  IF SEEK(M.ACCOUNT,lcCucmpd)
    IF !EMPTY(&lcCucmpd..NOTELN1)
      SELECT NOTEPAD
      IF !SEEK('A'+PADR(M.ACCOUNT,20),'NOTEPAD')
        APPEND BLANK      
        REPLACE TYPE  WITH 'A' ;
                KEY   WITH M.ACCOUNT ;
                CDESC WITH 'Notes For Account Number : ' + M.ACCOUNT 
      ENDIF    
      REPLACE MNOTES     WITH &lcCucmpd..ENTRDATE + ' - ' + &lcCucmpd..STORE + CHR(13) + ;
                              ALLTRIM(&lcCucmpd..NOTELN1)  + CHR(13) + ;
                              IIF(!EMPTY(&lcCucmpd..NOTELN2) , ALLTRIM(&lcCucmpd..NOTELN2)  + CHR(13) , '' ) + ;
                              IIF(!EMPTY(&lcCucmpd..NOTELN3) , ALLTRIM(&lcCucmpd..NOTELN3)  + CHR(13) , '' ) ;
                              ADDITIVE
    ENDIF
  ENDIF
  *C122068,6  TMI [End  ] 

ENDDO
WAIT CLEAR
IF llShwAlMsg
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Data converted to customer file successfully.')
ENDIF
*-- end of lfCust.

*:**************************************************************************
*:* Name        : lfStyles
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/22/2004
*:* Purpose     : Get styles
*:***************************************************************************
FUNCTION lfStyles

PRIVATE lnI,laFileSt,lcTmpFl

*C122068,9  TMI [Start] Only add new records , do not update 
lcUpdRec = 'x'
*C122068,9  TMI [End  ] 

*C122068,5  TMI [Start] 
IF EMPTY(lcRpClrDir)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No colors files folder selected.')
  RETURN
ENDIF
IF EMPTY(lcRpStyClr)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No colors files initial entered.')
  RETURN
ENDIF
*C122068,5  TMI [End  ] 

*- Create temp file to get styles
lcArrLn = 78
DIMENSION laFileSt[lcArrLn,4]
FOR lnI =  1 TO lcArrLn
  laFileSt[lnI,1] = 'FLD'+ALLT(STR(lnI,2))
  laFileSt[lnI,2] = 'C'
  laFileSt[lnI,3] = 30
  laFileSt[lnI,4] = 0
ENDFOR

lcTmpSty = lfTmpFlNm()
CREATE TABLE (gcWorkDir+lcTmpSty) FROM ARRAY laFileSt
*C122068,8  TMI [Start] comment this line, recreate it after updating the season with season #
*INDEX ON FLD2 TAG &lcTmpSty
*C122068,8  TMI [End  ] 

IF !lfAppend(lcRpStyle,'XLS','Season Code',1,'Styles')
  RETURN
ENDIF  

*C122068,8  TMI [Start] 
* FLD27 is not used in the xls file , so use it to replace it with the number of the season in the 
* array of seasons + style code, and then create a unique index with the field fld27 , and then when 
* retriving the data from of a style you get the correct season for the style
DIMENSION laSeasOrd[1] &&Get seasons order , take always first season 
=lfStr2Arr(',F,6,E,5,D,4,C,3,B,2,A,S,R,Z,1,',@laSeasOrd,',')
SELECT &lcTmpSty
GO TOP
*-There are also styles with width more than 8 characters, replace them with the correct styles
DIMENSION laCrctSty[7,2]
laCrctSty[1,1] =  '10/01/7743'
laCrctSty[1,2] =  'OCT7743'
laCrctSty[2,1] =  '10/01/7710'
laCrctSty[2,2] =  'OCT7710'
laCrctSty[3,1] =  '10/01/7714'
laCrctSty[3,2] =  'OCT7714'
laCrctSty[4,1] =  '10/01/7713'
laCrctSty[4,2] =  'OCT7713'
laCrctSty[5,1] =  '09/01/7748'
laCrctSty[5,2] =  'SEP7748'
laCrctSty[6,1] =  '09/01/7741'                   
laCrctSty[6,2] =  'SEP7741'
laCrctSty[7,1] =  '09/01/7747'
laCrctSty[7,2] =  'SEP7747'

WAIT WINDOW NOWAIT 'Indexing Style file .. '
SCAN
  lnCrctSty = ASCAN(laCrctSty,ALLTRIM(FLD2))  
  IF lnCrctSty > 0
    IF ASUBSCRIPT(laCrctSty , lnCrctSty , 2 ) = 1
      REPLACE FLD2 WITH laCrctSty[lnCrctSty+1]
    ENDIF
  ENDIF
  REPLACE FLD27 WITH STR(ASCAN(laSeasOrd,ALLTRIM(FLD1)),2) + FLD2 
ENDSCAN
INDEX ON FLD27 TAG &lcTmpSty UNIQUE
*C122068,8  TMI [End  ] 

*C122068,9  TMI [Start] We need to take only the first season , according to the array laSeasOrd, they are sorted well
*                       to deal with only the first we need to create an index with uniqe key on stymajor, so copy first 
*                       to a new temp file and then reindex
*                       This will also solve the problem of that a style has more than one scale
WAIT WINDOW NOWAIT 'Pick only first season .. '
SELECT &lcTmpSty
lcTmpNm = 'Z'+SUBSTR(lcTmpSty,2)
COPY TO (gcWorkDir+lcTmpNm)
USE IN &lcTmpSty
USE (gcWorkDir+lcTmpNm) ALIAS &lcTmpSty EXCL 
INDEX ON FLD2 TAG &lcTmpSty UNIQUE
*C122068,9  TMI [End  ] 

*- Create temp file to get colors from it
lnI = 0 

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'SEASON'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 6
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'STYLE'
laFileSt[lnI,2] = 'C'
*C122068,8  TMI [Start] 
*laFileSt[lnI,3] = 8
laFileSt[lnI,3] = 19
*C122068,8  TMI [End  ] 
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'COLOR'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 6
laFileSt[lnI,4] = 0

*C122068,6  TMI [Start] Dimension field
lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'DIM'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 2
laFileSt[lnI,4] = 0
*C122068,6  TMI [End  ] 

*C122068,8  TMI [Start] Add more field for the original season
lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ORGSEAS'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 6
laFileSt[lnI,4] = 0
*C122068,8  TMI [End  ] 

lcTmpClr = lfTmpFlNm()
CREATE TABLE (gcWorkDir+lcTmpClr) FROM ARRAY laFileSt
*C122068,6  TMI [Start] Add dimension field and add season to the index
*INDEX ON STYLE+COLOR TAG &lcTmpClr
DIMENSION laSeasOrd[1] &&Get seasons order , take always first season 
=lfStr2Arr(',F,6,E,5,D,4,C,3,B,2,A,S,R,Z,1,',@laSeasOrd,',')
SELECT &lcTmpClr
*C122068,8  TMI [Start] Comment This line 
*REPLACE SEASON WITH STR(ASCAN(laSeasOrd,ALLTRIM(SEASON),2) ALL
*C122068,8  TMI [End  ] 
INDEX ON STYLE+COLOR+SEASON+DIM TAG &lcTmpClr FOR SEASON<>' 0'
*C122068,6  TMI [End  ] 

*C122068,5  TMI [Start] Get all colors files  in the selected colors folder
*IF !lfAppend(lcRpStyClr,'XLS','Color',3,'Styles Colors')
DIME laXlsClr[1,5]
=ADIR(laXlsClr,lcRpClrDir+ALLTRIM(lcRpStyClr)+'*.XLS')
IF !EMPTY(laXlsClr[1])
  FOR lnXlClr = 1 TO ALEN(laXlsClr,1)
    IF !lfAppend(lcRpClrDir+laXlsClr[lnXlClr,1],'XLS','Color',3,'Styles Colors')
      *C122068,5  TMI [End  ] 
      RETURN
    ENDIF  
    *C122068,5  TMI [Start] Close the loop that appends colors files
  ENDFOR  
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No colors files exist.')
  RETURN
ENDIF  
*C122068,5  TMI [End  ]   


*C122068,8  TMI [Start] Update styles from laCrctSty array
SELECT &lcTmpClr
lcSvClrOrd = ORDER()
SET ORDER TO 
WAIT WINDOW NOWAIT 'Indexing colors file ..'
GO TOP
SCAN 
  lnCrctSty = ASCAN(laCrctSty,ALLTRIM(&lcTmpClr..STYLE))
  IF lnCrctSty > 0
    REPLACE &lcTmpClr..STYLE WITH laCrctSty[lnCrctSty+1]
  ENDIF
  
  *- Save original season value
  REPLACE ORGSEAS WITH SEASON ;
          SEASON  WITH STR(ASCAN(laSeasOrd,ALLTRIM(SEASON)),2)
ENDSCAN  
SET ORDER TO &lcSvClrOrd IN &lcTmpClr
*C122068,8  TMI [End  ] 

*C122068,6  TMI [Start] Collect dimensions groups that we can build different scales using these groups
*lcTmpDim = lfDimGrps()
SELECT SCALE
lcSvOrd = ORDER('SCALE')
INDEX ON TYPE + PADR(SCALE,1) + CDIM1 + SZ1+SZ2+SZ3+SZ4+SZ5+SZ6+SZ7+SZ8 TO (gcWorkDir+'SCLDIM')
*C122068,6  TMI [End  ] 

*:**************************************
*- Create temp file to get UPC's from it
*:**************************************
lnI = 0 

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'UPC'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 12
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'SEASON'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 6
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'STYLE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 8
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'COLOR'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 6
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'DIMEN'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 5
laFileSt[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'SIZE'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 5
laFileSt[lnI,4] = 0

*C122068,8  TMI [Start] Add more field for Original season
lnI = lnI + 1
DIMENSION laFileSt[lnI,4]
laFileSt[lnI,1] = 'ORGSEAS'
laFileSt[lnI,2] = 'C'
laFileSt[lnI,3] = 6
laFileSt[lnI,4] = 0
*C122068,8  TMI [End  ] 

lcTmpUpc = lfTmpFlNm()
CREATE TABLE (gcWorkDir+lcTmpUpc) FROM ARRAY laFileSt
*C122068,6  TMI [Start] include season in the index
*INDEX ON STYLE+COLOR+SIZE TAG &lcTmpUpc
*C122068,8  TMI [Start] Comment this line
*REPLACE SEASON WITH STR(ASCAN(laSeasOrd,ALLTRIM(SEASON)),2)
*C122068,8  TMI [End  ] 
INDEX ON STYLE+COLOR+SIZE+DIMEN+SEASON TAG &lcTmpUpc FOR SEASON <> ' 0'
*C122068,6  TMI [End  ] 
IF !lfAppend(lcRpStyUpc,'DELIMITED','Style UPC #',1,'Styles UPCs')
  RETURN
ENDIF

*C122068,8  TMI [Start] Save original season in ORGSEAS field
WAIT WINDOW NOWAIT 'Updating Season in temp UPC file .. '
REPLACE ORGSEAS WITH SEASON ;
        SEASON  WITH STR(ASCAN(laSeasOrd,ALLTRIM(SEASON)),2) ALL
*C122068,8  TMI [End  ] 

*C122068,6  TMI [Start] Ask to delete styles data befor process
IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Delete Styles befor processing?') = 1
  SELECT STYLE
  ZAP
  SELECT STYDYE
  ZAP
  SELECT STYLEUPC
  ZAP
  SELECT BOM
  ZAP
  SELECT ICSTYHST
  ZAP
  SELECT ICSEGVAL
  ZAP
ENDIF
*C122068,6  TMI [End  ]

*C122068,4  TMI [Start] Create a log file to detect rejected styles
CREATE TABLE (gcWorkDir+'CNVLOG') (TEXT C(100))
INSERT INTO CNVLOG VALUES ('This file is saved in ' + gcWorkDir+'CNVLOG.TXT')
INSERT INTO CNVLOG VALUES ('The following styles are REJECTED')
INSERT INTO CNVLOG VALUES ('Serial STYLE#            XLS LINE#       REJECTION REASON')
INSERT INTO CNVLOG VALUES ('---------------------------------------------------------')
*C122068,4  TMI [End  ] 

*-Start collecting Data into style file 
*C122068,3  TMI [Start] Save UCC manufactring ID to setups file
SELECT &lcTmpSty
LOCATE FOR !EMPTY(VAL(&lcTmpSty..FLD72))
IF FOUND()
  SELECT SETUPS
  IF !SEEK('XMANUFID','SETUPS')
    APPEND BLANK
    REPLACE CAPP_ID    WITH CHR(255)   ;
            CFLD_NAME  WITH 'XMANUFID' ;
            CDATA_TYP  WITH 'C' ;
            CFLD_HEAD  WITH 'Company UPS Account' ;
            NFLD_WDTH  WITH 7   ;
            CDEFA_TYP  WITH 'V' ;
            MDATA_DEF  WITH ALLTRIM(&lcTmpSty..FLD72) ;
            NVARPOS    WITH 13  ;
            COBJ_TYPE  WITH 'A' ;
            LCANTRANSF WITH .T. ;
            CUPGRDLVL  WITH 'S'
  ENDIF
ENDIF  
*C122068,3  TMI [End  ]

SELECT &lcTmpSty
LOCATE
*C122068,4  TMI [Start] Add rejected styles to a log file with rejection reason 
*SCAN FOR !EMPTY(PADR(&lcTmpSty..FLD2,8))
lnAccepted = 0
lnRejected = 0

*C122068,8  TMI [Start] Get lnMajorLen
lnMajorLen = LEN(gfItemMask("PM"))
*C122068,8  TMI [End  ] 

SCAN 
  IF EMPTY(PADR(&lcTmpSty..FLD2,8))
    lnRejected = lnRejected + 1    
    INSERT INTO CNVLOG VALUES ;
      (STR(lnRejected,5)+' '+PADR(&lcTmpSty..FLD2,19)+ STR(RECNO(lcTmpSty),5) + '      No STYLE')
    LOOP
  ENDIF 
  *C122068,4  TMI [End  ] 

  *C122068,8  TMI [Start] Reject style if major lenght is more than lnMajorLen
  IF LEN(ALLTRIM(&lcTmpSty..FLD2)) > lnMajorLen
    lnRejected = lnRejected + 1    
    INSERT INTO CNVLOG VALUES ;
      (STR(lnRejected,5)+' '+PADR(&lcTmpSty..FLD2,19)+ STR(RECNO(lcTmpSty),5) + '      Style lenght is more than'+STR(lnMajorLen,3))
    LOOP
  ENDIF 
  *C122068,8  TMI [End  ] 
  *-Get colors for style &lcTmpSty..FLD2
  SELECT &lcTmpClr
  DIMENSION laClr[1]
  laClr[1] = '      '
  lnClr = 0
  *C122068,6  TMI [Start] Get this style colors dimensions into Array laCurDim
  DIMENSION laCurDim[1]
  laCurDim = ''
  lnCurDim = 0  
  *C122068,6  TMI [End  ] 
  IF SEEK(PADR(&lcTmpSty..FLD2,8),lcTmpClr)
    *C122068,6  TMI [Start] Case style has dimension
    IF PADR(&lcTmpSty..FLD25,1) = 'Y'
      *C122068,8  TMI [Start] 
      *DO WHILE !EOF(lcTmpClr) .AND. PADR(&lcTmpSty..FLD2,8) = &lcTmpClr..STYLE
      DO WHILE !EOF(lcTmpClr) .AND. PADR(&lcTmpSty..FLD2,lnMajorLen) = PADR(&lcTmpClr..STYLE,lnMajorLen)
        IF ALLTRIM(&lcTmpSty..FLD1) = ALLTRIM(&lcTmpClr..ORGSEAS)
          *C122068,8  TMI [End  ] 
          lcColr = &lcTmpClr..COLOR
          lcSeas = &lcTmpClr..SEASON
          lcCurDim = '|'
          *C122068,8  TMI [Start] lcTmpClr.style field width is changed
          *SCAN REST WHILE STYLE+COLOR+SEASON+DIM = PADR(&lcTmpSty..FLD2,8)+lcColr+lcSeas ;
                    FOR !EMPTY(&lcTmpClr..COLOR)
          SCAN REST WHILE STYLE+COLOR+SEASON+DIM = PADR(&lcTmpSty..FLD2,19)+lcColr+lcSeas ;
                    FOR !EMPTY(&lcTmpClr..COLOR)
            *C122068,8  TMI [End  ] 
            IF ASCAN(laClr,&lcTmpClr..COLOR) = 0
              lnClr = lnClr + 1      
              DIMENSION laClr[lnClr]
              laClr[lnClr] = &lcTmpClr..COLOR
            ENDIF
            IF !EMPTY(&lcTmpClr..DIM) .AND. !&lcTmpClr..DIM $ lcCurDim
              lcCurDim = lcCurDim + &lcTmpClr..DIM + '|'
            ENDIF
          ENDSCAN
          lcCurDim = PADR(lcCurDim,30)
          IF ASCAN(laCurDim , lcCurDim ) = 0
            lnCurDim = lnCurDim + 1
            DIMENSION laCurDim[lnCurDim]
            laCurDim[lnCurDim] = lcCurDim
          ENDIF
        *C122068,8  TMI [Start] 
        ELSE        
          SKIP IN &lcTmpClr
        ENDIF
        *C122068,8  TMI [End  ] 
      ENDDO    
    ELSE
      *SCAN REST WHILE STYLE+COLOR = PADR(&lcTmpSty..FLD2,8) ;
              FOR !EMPTY(&lcTmpClr..COLOR)
      SCAN REST WHILE STYLE+COLOR+SEASON+DIM = PADR(&lcTmpSty..FLD2,8) ;
                FOR !EMPTY(&lcTmpClr..COLOR)               
        *C122068,6  TMI [End  ] 
  
        *C122068,8  TMI [Start] Check season
        IF ALLTRIM(&lcTmpSty..FLD1) = ALLTRIM(&lcTmpClr..ORGSEAS)
          *C122068,8  TMI [End  ] 
          IF ASCAN(laClr,&lcTmpClr..COLOR) = 0
            lnClr = lnClr + 1      
            DIMENSION laClr[lnClr]
            laClr[lnClr] = &lcTmpClr..COLOR
          ENDIF
          *C122068,8  TMI [Start] 
        ENDIF
        *C122068,8  TMI [End  ] 
        
      ENDSCAN
      *C122068,6  TMI [Start] Close the above If
    ENDIF
    *C122068,6  TMI [End  ] 
  ENDIF
  
  *- If style has no colors in color file , skip it
  IF EMPTY(laClr[1])
    *C122068,4  TMI [Start] Add a line to log file and count rejected lines
    lnRejected = lnRejected + 1
    INSERT INTO CNVLOG VALUES ;
       (STR(lnRejected,5)+' '+PADR(&lcTmpSty..FLD2,19)+ STR(RECNO(lcTmpSty),5) + '      No COLOR FOR THE STYLE SEASON')
    *C122068,4  TMI [End  ] 
    LOOP
  ENDIF
  
  *-Get scale
  DIMENSION laScale[1]
  laScale[1] = ''
  *C122068,6  TMI [Start] let scale width be 2 characters
  *lcScl = PADR(ALLTRIM(&lcTmpSty..FLD6),1)
  lcScl = PADR(ALLTRIM(&lcTmpSty..FLD6),2,'0')
  *C122068,6  TMI [End  ] 
  SELECT SCALE,SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8 ;
      FROM SCALE WHERE TYPE+SCALE = 'S'+lcScl AND !DELETED() ;
      INTO ARRAY laScale
  
  *- If the scale of this file is not found then skip it
  IF EMPTY(laScale[1])
    *C122068,4  TMI [Start] Add a line to log file and count rejected lines
    lnRejected = lnRejected + 1
    INSERT INTO CNVLOG VALUES ;
      (STR(lnRejected,5)+' '+PADR(&lcTmpSty..FLD2,19)+ STR(RECNO(lcTmpSty),5) + '      No SCALE')
    *C122068,4  TMI [End  ] 
    LOOP
  ENDIF

  *C122068,7  TMI [Start] 
  llHasDim = .F.
  *C122068,7  TMI [End  ] 
  *C122068,6  TMI [Start] Update the scale file with the current dimension relating it to this style scale 
  IF PADR(&lcTmpSty..FLD25,1) = 'Y' .AND. !EMPTY(laCurDim[1])
    SELECT SCALE
    SET ORDER TO TAG SCLDIM
    GO TOP
    
    lcSclDim = ' '  && refers to the dimensioned scale
    DIMENSION laChr[1]
    *- Fill the array laChr with an appropriate set of characters to build scale from
    =lfStr2Arr(',1,2,3,4,5,6,7,8,9,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,Z,Y,X,',@laChr,',')
    
    *C122068,6  TMI [Start] Get scalehd descreption field
    lcScHdDesc = ''                         &&laCurDim[1]

    SELECT MAX(ASC(SUBSTR(SCALE,2,1))) ;
           WHERE SCALE = SUBSTR(lcScl,1,1) ;
           FROM SCALE ;
         INTO ARRAY laMxScl
    lnI = 1
    DO WHILE .T.      
      lcNwSclChr = CHR(laMxScl[1]+lnI)
      IF lcNwSclChr $ '123456789ABCDEFGHIJKLMNOPQRSTUVWZYX'        
        EXIT
      ELSE
        lnI = lnI + 1  
      ENDIF
    ENDDO    
    *C122068,6  TMI [End  ] 
    
    lnS = 0
    DIMENSION laDim[1]
    =lfStr2Arr(laCurDim[1],@laDim,'|')
    lcDmSz = ''
    FOR lnDm = 1 TO ALEN(laDim)
      lcDmSz = lcDmSz + PADR(laDim[lnDm],5)
    ENDFOR
    lcDmSz = PADR(lcDmSz,40)  && We have 8 sizes each with 5 chr desc. name ( field scale.sz1 , ... , scale.sz8 ) 
    
    FOR lnScl = 1 TO ALEN(laScale,1)
                
      FOR lnS1 = 1 TO 8    
        
        IF !EMPTY(laScale[lnScl,lnS1+1])
    
          IF !SEEK('S' + PADR(lcScl,1) + PADR(laScale[lnScl,lnS1+1],5) + lcDmSz , 'SCALE' )
            
            lnS = lnS + 1
            lcSclDim = PADR(lcScl,1) + lcNwSclChr + laChr[lnS]
            APPEND BLANK
            REPLACE TYPE      WITH 'S' ;
                    SCALE     WITH lcSclDim ;
                    CSCL_DESC WITH laScale[lnScl,lnS1+1] ;
                    CDIM1     WITH laScale[lnScl,lnS1+1] ;                 
                    CNT       WITH ALEN(laDim) 
            =lfAdUsrInf()                  
            FOR lnS2 = 1 TO ALEN(laDim)
              lcS2 = STR(lnS2,1)
              REPLACE SZ&lcS2 WITH laDim[lnS2]
            ENDFOR
            *C122068,10 TMI [Start] Get scalehd descreption field
            lcScHdDesc = lcScHdDesc+ALLTRIM(laScale[lnScl,lnS1+1]) + ','
            *C122068,10 TMI [End  ] 
          ENDIF
        ENDIF
      
      ENDFOR
      
    ENDFOR
    
    *C122068,6  TMI [Start] Add a record to SCALEHD file
    IF lnS>0 .AND. !SEEK(lcSclDim,'SCALEHD')
      SELECT SCALEHD
      APPEND BLANK
      REPLACE CEXTSCALE  WITH lcSclDim ;
              NNOOFDIM   WITH 2 ;
              CDIM1DESC  WITH 'SIZE' ;
              CDIM2DESC  WITH 'DIM' ;
              NNOFCODES  WITH lnS
      *C122068,10 TMI [Start] Get scalehd descreption field
      lcScHdDesc = LEFT(lcScHdDesc,LEN(lcScHdDesc)-1)   && Remove the rightmost comma
      lcScHdDesc = lcScHdDesc+' ('+laCurDim[1]+')'      && Add dimension list
      *C122068,10 TMI [End  ] 
      REPLACE CSCALEDES WITH lcScHdDesc
      =lfAdUsrInf()                  
      *C122068,7  TMI [Start] 
    ENDIF

    *C122068,10 TMI [Start] If Scale exists Befor 
    IF lnS = 0 .AND. ;
        SEEK('S' + PADR(lcScl,1) + PADR(laScale[1,2],5) + lcDmSz , 'SCALE' )
      lnS = 1
      lcSclDim = SCALE.SCALE
    ENDIF      
    *C122068,10 TMI [End  ] 

    IF lnS > 0
      *C122068,7  TMI [End  ]     
      *- Refill the laScale array
      SELECT SCALE,SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,CDIM1 ;
          FROM SCALE WHERE TYPE+SCALE = 'S'+PADR(lcSclDim,2) AND !DELETED() ;
          INTO ARRAY laScale  
      SET ORDER TO &lcSvOrd IN SCALE
    ENDIF 
    *C122068,7  TMI [Start] llHasDim show if style has dimension
    llHasDim = !EMPTY(lnS)
    *C122068,6  TMI [End  ] 

  ENDIF
  
  *C122068,4  TMI [Start] Count Accepted lines
  lnAccepted = lnAccepted + 1
  *C122068,4  TMI [End  ] 
        
  FOR lnClr  = 1 TO ALEN(laClr)
    FOR lnScl = 1 TO ALEN(laScale,1)
      llNew = .F.
      SELECT STYLE
      *-Update style major
      lcSty = PADR( PADR(&lcTmpSty..FLD2,8)+lcClrSpr+laClr[lnClr]+lcSclSpr+laScale[lnScl,1] , 19 )
      IF !SEEK(lcSty,'STYLE')
        APPEND BLANK
        llNew = .T.
      ENDIF
      IF lcUpdRec = 'A' .OR. llNew
        *C122068,8  TMI [Start] 
        *WAIT WINDOW NOWAIT 'Updating master style file with : ' + lcSty 
        WAIT WINDOW NOWAIT 'Updating master style file with : ' + lcSty + ' for Season:'+ PADR(&lcTmpSty..FLD1,6)
        *C122068,8  TMI [End  ] 
        REPLACE STYLE      WITH lcSty ;
                CSTYMAJOR  WITH PADR(&lcTmpSty..FLD2,8) ;
                DESC       WITH PADR(&lcTmpSty..FLD5,20) ;
                SCALE      WITH laScale[lnScl,1] ;
                PRICEA     WITH VAL(&lcTmpSty..FLD8) ;
                CDEFWARE   WITH PADR(&lcTmpSty..FLD23,6) ;
                SEASON     WITH PADR(&lcTmpSty..FLD1,6) ;
                CDIVISION  WITH PADR(&lcTmpSty..FLD3,6) ;
                NICOST1    WITH VAL(&lcTmpSty..FLD11) ;
                NMCOST1    WITH NICOST1    ;
                TOTCOST    WITH NICOST1    ;
                AVE_COST   WITH NICOST1    ;
                GROS_PRICE WITH NICOST1    ;
                MARKA      WITH IIF(PRICEA>0,100 - (NICOST1/PRICEA)*100,0) ;
                LDETCOST   WITH .F. ;
                LINVSTY    WITH .T. ;
                STATUS     WITH 'A' ;
                CDYE_FLG   WITH 'N' ;
                DESC1      WITH gfCodDes(laClr[lnClr],'COLOR') ;
                CSTYGRADE  WITH '1' ;
                LINK_CODE  WITH 'DEFDEF' ;
                CSLSGLLINK WITH 'DEF' ;
                COMMISSION WITH .T. ;
                CPRICECUR  WITH 'USD' ;
                CDUTYCUR   WITH 'USD' ;
                NTAXBREAK  WITH 1 
        *C122068,3  TMI [Start] Add style group,Update the long description
        REPLACE CSTYGROUP  WITH PADR(ALLTRIM(&lcTmpSty..FLD19),6) ;
                DESC1      WITH ALLTRIM(DESC)+'  '+gfCodDes(laClr[lnClr],'COLOR')                
        *- Get default group if it is empty
        IF EMPTY(STYLE.CSTYGROUP)
          =SEEK('DCSTYGROUP','CODES')
          REPLACE CSTYGROUP WITH CODES.CCODE_NO  
        ENDIF
        
        *C122068,3  TMI [End  ]        
        =lfAdUsrInf()
        
        SELECT ICSTYHST
        IF !SEEK(lcSty + gcCurrYear,'ICSTYHST')
          FOR lnFsYear = -1 TO 1
            APPEND BLANK
            REPLACE Style     WITH lcSty ;
                    cFisFYear WITH STR(VAL(gcCurrYear)+lnFsYear,4)
            =lfAdUsrInf()
          ENDFOR
        ENDIF
        
        SELECT STYDYE  
        && key : STYLE+CWARECODE+DYELOT
        IF !SEEK(lcSty + PADR(&lcTmpSty..FLD23,6) , 'STYDYE' )
          APPEND BLANK
          REPLACE STYLE     WITH lcSty ;
                  CWARECODE WITH PADR(&lcTmpSty..FLD23,6) ;
                  GL_LINK   WITH 'DEFDEF' ;
                  AVE_COST  WITH STYLE.AVE_COST
          =lfAdUsrInf()
        ENDIF
        
        SELECT ICSEGVAL
        IF !SEEK('1'+lcSty,'ICSEGVAL')
          APPEND BLANK
          REPLACE CISEGNO   WITH '1' ;
                  CISEGVAL  WITH lcSty ;
                  CISGVALSD WITH STYLE.DESC ;
                  CISGVALLD WITH STYLE.DESC1
          =lfAdUsrInf()
        ENDIF
        
      ENDIF 
      
      *:***************************************************************************      
      *: Get UPC's
      *:***************************************************************************
      
      IF !EMPTY(laScale[1])
        *C122068,7  TMI [Start] Ask if this style has dimensions
        *-* In the case of that style has dimension , aria store the size in the field CDIM1 ,
        *-* and the fields Sz1,...,Sz8 stores the dimensions for this Size
        *-* Also the current key is "STYLE+COLOR+SIZE+DIMEN+SEASON" , so insert the variable 
        *-* in the seeked value and this will be appropriate for the two cases
        
        lcDimSz = ''        
        IF llHasDim
          lcDimSz = laScale[lnScl,10]
        ENDIF        
        *C122068,7  TMI [End  ] 
        FOR lnSz = 1 TO 8
          IF !EMPTY(laScale[lnScl,lnSz+1])
            *C122068,7  TMI [Start] 
            *IF SEEK(PADR(&lcTmpSty..FLD2,8) + laClr[lnClr] + laScale[lnScl,lnSz+1],lcTmpUpc)
            IF SEEK(PADR(&lcTmpSty..FLD2,8) + laClr[lnClr] + lcDimSz + laScale[lnScl,lnSz+1],lcTmpUpc)              
              *C122068,7  TMI [End  ] 
              *C122068,8  TMI [Start] Check season
              IF ALLTRIM(&lcTmpSty..FLD1) = ALLTRIM(&lcTmpUpc..ORGSEAS)
                *C122068,8  TMI [End  ]
                llNew = .F.
                SELECT STYLEUPC
                IF !SEEK(lcSty+STR(lnSz,1),'STYLEUPC')
                  APPEND BLANK
                  llNew = .T.
                ENDIF
                IF lcUpdRec = 'A' .OR. llNew
                  REPLACE STYLE    WITH lcSty ;
                          SIZE     WITH STR(lnSz,1) ;
                          CUPCNUM1 WITH SUBSTR(&lcTmpUpc..UPC,1,6) ;
                          CUPCNUM2 WITH SUBSTR(&lcTmpUpc..UPC,7,5) ;
                          CUPCNUM3 WITH SUBSTR(&lcTmpUpc..UPC,12,1)
                  =lfAdUsrInf()            
                ENDIF
                *C122068,3  TMI [Start] Update Style.LUPCYN  field
                =SEEK(STYLEUPC.STYLE,'STYLE')
                SELECT STYLE
                REPLACE LUPCYN    WITH .T.
                *C122068,3  TMI [End  ] 
              ENDIF
              *C122068,8  TMI [Start] 
            ENDIF
            *C122068,8  TMI [End  ] 
          ENDIF
        ENDFOR       
      ENDIF
       
      *-Add a line to BOM file 
      &&CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR
      llNew = .F.
      SELECT BOM
      IF !SEEK(STYLE.CSTYMAJOR+'1'+STYLE.STYLE+'*1    ','BOM')
        APPEND BLANK
        llNew = .T.
      ENDIF
      IF lcUpdRec = 'A' .OR. llNew
        REPLACE CITMMAJOR  WITH STYLE.CSTYMAJOR ;
                TYP        WITH '1' ;
                CITMMASK   WITH STYLE.STYLE ;
                MFGCODE    WITH '*1' ;               
                UNTCOST    WITH STYLE.NICOST1 ;
                TOTCOST    WITH STYLE.NICOST1 ;
                NBOMTOTQTY WITH 1 ;
                CCATGTYP   WITH 'P'
        =lfAdUsrInf()
      ENDIF        
    ENDFOR
    
    *-add color to codes file if it is not there
    =lfUpdcodes('COLOR     ','laClr[lnClr]')    

  ENDFOR
  
  *C122068,3  TMI [Start] Update division in Codes file
  SELECT STYLE
  SCATTER MEMVAR
  =lfUpdcodes('SEASON    ','M.SEASON')  
  =lfUpdcodes('CDIVISION ','M.CDIVISION')  
  =lfUpdcodes('CSTYGROUP ','M.CSTYGROUP')  
  *C122068,3  TMI [End  ] 

  *- Add WAREHOUS if it is not exists
  IF !SEEK(PADR(ALLTRIM(&lcTmpSty..FLD23),6),'WAREHOUS')
    SELECT WAREHOUS    
    APPEND BLANK
    REPLACE CWARECODE  WITH PADR(ALLTRIM(&lcTmpSty..FLD23),6) ;
            CDESC      WITH PADR(ALLTRIM(&lcTmpSty..FLD23),6) + '-' + 'PLEASE CHECK' ;
            LSTYINV    WITH .T. ;
            CADDRESS6  WITH 'USA' ;
            CCONT_CODE WITH 'USA'
    =lfAdUsrInf()
  ENDIF
  
  *C122068,3  TMI [Start] 
  *-add season to codes files if it is not there
  *=lfUpdcodes('SEASON    ','STYLE.SEASON')  
  *C122068,3  TMI [End  ] 
  
  IF LASTKEY() = 27
    IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you need to exit.') = 1
      EXIT
    ENDIF
  ENDIF
    
ENDSCAN
WAIT CLEAR

IF llShwAlMsg
  *C122068,4  TMI [Start] Show accepted and rejected no. of styles
  *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Data converted to style file successfully.')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Style converted successfully,' + ;
                                            STR(lnAccepted,5) + ' lines Accepted,' +;
                                            STR(lnRejected,5) + ' lines Rejected.')
  *C122068,4  TMI [End  ] 
ENDIF  
*C122068,6  TMI [Start] Add accepted and rejected qty's to log file.
INSERT INTO CNVLOG VALUES ( STR(lnAccepted,5) + ' lines Accepted.' )
INSERT INTO CNVLOG VALUES ( STR(lnRejected,5) + ' lines Rejected.' )
*C122068,6  TMI [End  ] 

*C122068,4  TMI [Start] Save data to log text file
SELECT CNVLOG
GO TOP
COPY TO (gcWorkDir+'CNVLOG.TXT') SDF
MODIFY FILE (gcWorkDir+'CNVLOG.TXT') NOEDIT

*C122068,4  TMI [End  ] 

*-- end of lfStyles.

*:**************************************************************************
*:* Name        : lfAppend
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/21/2004
*:* Purpose     : Append from excel files
*:***************************************************************************
FUNCTION lfAppend
PARAMETERS lcFile,lcAppTyp,lcString,lnFldNo,lcType
PRIVATE lcSvErr,lnErr,llRet
lcSvErr = ON('ERROR')
llRet = .T.

WAIT WINDOW NOWAIT 'Appendign data from file : ' + ALLTRIM(lcFile)
lnErr = .F.
ON ERROR lnErr = .T.
APPEND FROM &lcFile &lcAppTyp
ON ERROR &lcSvErr

*-Check if this is a correct Codes/Style/Customer file , by comparing the data in the first record with a known string that must be there
*C122068,3  TMI [Start] Check if there are records befor going to 1st record
IF RECCOUNT() > 0
  *C122068,3  TMI [End  ] 
  GOTO 1
  *C122068,3  TMI [Start] Close if Statment
ELSE
  WAIT CLEAR
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No records has been appended from the file:'+ lcFile + '.')
  llRet = .F.
  RETURN llRet   
ENDIF  
*C122068,3  TMI [End  ]   

IF !lcString $ EVAL(FIELD(lnFldNo)) 
  
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcFile+ ' is Not a Correct &lcType file')
  llRet = .F.
  
ELSE  

  &&  Remove the first record that contains fields names from the excel file
  GOTO 1
  DELETE   
  GO TOP

  IF lnErr
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'An error occured,can not append from the file :'+lcRpCust)
    llRet = .F.
  ELSE  
    IF RECCOUNT()=0
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No records found.')
      llRet = .F.
    ENDIF
  ENDIF
ENDIF

WAIT CLEAR

*-if Error occured , remove the temp created file
*IF !llRet 
*  lcDbf = DBF()
*  USE
*  ERASE (lcDbf)
*  ERASE (STRTRAN(lcDbf,'.DBF','.CDX'))
*ENDIF

RETURN llRet 
*-- end of lfAppend.


*:**************************************************************************
*:* Name        : lfUpdcodes
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/21/2004
*:* Purpose     : Update codes 
*:***************************************************************************
FUNCTION lfUpdcodes
PARAMETERS lcCode,lcCodVr
PRIVATE lnSlct
lnSlct = SELECT()

SELECT CODES
IF !EMPTY(&lcCodVr)
  && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
  llNew = .F.
  IF !SEEK('N'+PADR(lcCode,10)+PADR(&lcCodVr,6),'CODES')
    APPEND BLANK
    *C122068,3  TMI [Start] Add code only once
    *llNew = .T.
    REPLACE CDEFCODE   WITH 'N' ;
            CFLD_NAME  WITH lcCode ;
            CCODE_NO   WITH &lcCodVr ;
            CDISCREP   WITH &lcCodVr + '- PLEASE CHECK' ;
            CRLTFIELD  WITH 'N' 
    =lfAdUsrInf()
    *C122068,3  TMI [End  ] 
  ENDIF
  *C122068,3  TMI [Start] If Code is added befor do not update
  *-* IF lcUpdRec = 'A' .OR. llNew
  *-*   REPLACE CDEFCODE   WITH 'N' ;
  *-*           CFLD_NAME  WITH lcCode ;
  *-*           CCODE_NO   WITH &lcCodVr ;
  *-*           CDISCREP   WITH &lcCodVr + '- PLEASE CHECK' ;
  *-*           CRLTFIELD  WITH 'N' 
  *-*   =lfAdUsrInf()
  *-* ENDIF            
  *C122068,3  TMI [End  ] 
  
  *C122068,3  TMI [Start] Add default code
  llNew = .F.
  IF !SEEK('D'+PADR(lcCode,10),'CODES')
    APPEND BLANK
    *C122068,3  TMI [Start] Add code only once
    *llNew = .T.
    REPLACE CDEFCODE   WITH 'D' ;
            CFLD_NAME  WITH lcCode ;
            CCODE_NO   WITH &lcCodVr ;
            CDISCREP   WITH &lcCodVr + '- PLEASE CHECK' ;
            CRLTFIELD  WITH 'N' 
    =lfAdUsrInf()
    *C122068,3  TMI [End  ] 
  ENDIF
  *C122068,3  TMI [Start] 
  *-* IF lcUpdRec = 'A' .OR. llNew
  *-*   REPLACE CDEFCODE   WITH 'D' ;
  *-*           CFLD_NAME  WITH lcCode ;
  *-*           CCODE_NO   WITH &lcCodVr ;
  *-*           CDISCREP   WITH &lcCodVr + '- PLEASE CHECK' ;
  *-*           CRLTFIELD  WITH 'N' 
  *-*   =lfAdUsrInf()
  *-* ENDIF            
  *C122068,3  TMI [End  ] 
ELSE

  IF SEEK('D'+PADR(lcCode,6),'CODES')
    &lcCodVr = CODES.CCODE_NO
  ENDIF

ENDIF  

SELECT (lnSlct)
*-- end of lfUpdcodes.

*:**************************************************************************
*:* Name        : lfAdUsrInf
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : Add user information 
*:***************************************************************************
FUNCTION lfAdUsrInf
REPLACE CADD_USER  WITH gcUser_id ;
        DADD_DATE  WITH gdSysDate ;
        CADD_TIME  WITH TIME() ;
        CADD_VER   WITH 'A27'

*-- end of lfAdUsrInf.
*:**************************************************************************
*:* Name        : lfErase
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : *- Erase not needed temp files
*:***************************************************************************
FUNCTION lfErase
FOR lnTmpCnt = 1 TO ALEN(laTmpFl)
  IF !EMPTY(laTmpFl[lnTmpCnt])
    IF USED(laTmpFl[lnTmpCnt])
      USE IN (laTmpFl[lnTmpCnt])
    ENDIF
    ERASE (gcWorkDir+laTmpFl[lnTmpCnt]+'.DBF')
    ERASE (gcWorkDir+laTmpFl[lnTmpCnt]+'.CDX'))
    ERASE (gcWorkDir+laTmpFl[lnTmpCnt]+'.FPT'))
  ENDIF
ENDFOR
*-- end of lfErase.

*:**************************************************************************
*:* Name        : lfTmpFlNm
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/23/2004
*:* Purpose     : Create a temp file name and add it to temp names array 
*:***************************************************************************
FUNCTION lfTmpFlNm
lnTmpCnt = lnTmpCnt + 1
DIMENSION laTmpFl[lnTmpCnt]
laTmpFl[lnTmpCnt] = gfTempName()
RETURN laTmpFl[lnTmpCnt]
*-- end of lfTmpFlNm.


* notes 
* Delete divisons 01,02 from codes file , with type Q3


*:**************************************************************************
*:* Name        : lfGetDir
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/05/2004
*:* Purpose     : Get color files dir
*:***************************************************************************
FUNCTION lfGetDir
IF EMPTY(lcRpClrDir)
  lcRpClrDir =  GETDIR()
ENDIF  
*-- end of lfGetDir.

*:**************************************************************************
*:* Name        : lfDimGrps
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/15/2004
*:* Purpose     : Collect dimensions groups that we can build different scales using these groups
*:***************************************************************************
FUNCTION lfDimGrps
PRIVATE lnSlct,lcStyle,lcColor,lcDim,lnK
lnSlct = SELECT()

lcTempFl = gfTempName()

CREATE CURSOR &lcTempFl (CDIM C(30))
INDEX ON CDIM TAG &lcTempFl

lnK = 0
SELECT &lcTmpClr
GO TOP
DO WHILE !EOF()
  *C122068,8  TMI [Start] 
  *lcStyle = &lcTmpClr..STYLE
  lcStyle = PADR(&lcTmpClr..STYLE,lnMajorLen)
  *C122068,8  TMI [End  ] 
  lcColor = &lcTmpClr..COLOR
  lcDim   = &lcTmpClr..DIM
  lcDim = '|'
  SCAN REST WHILE STYLE+COLOR+DIM = lcStyle+lcColor
    IF !EMPTY(&lcTmpClr..DIM) .AND. !&lcTmpClr..DIM $ lcDim
      lcDim = lcDim + &lcTmpClr..DIM + '|'
    ENDIF
  ENDSCAN
  lcDim = PADR(lcDim,30)
  IF !SEEK(lcDim,lcTempFl)
    INSER INTO &lcTempFl VALUES (lcDim)
  ENDIF
ENDDO  

SELECT (lnSlct)
RETURN lcTempFl
*-- end of lfDimGrps.

*:**************************************************************************
*:* Name        : lfStr2Arr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/15/2004
*:* Purpose     : Convert a string to array 
*:***************************************************************************
FUNCTION lfStr2Arr
PARAMETERS lcStr,laArr,lcSepr
PRIVATE lnK
FOR lnK = 1 TO OCCURS(lcSepr,lcStr) - 1
  DIMENSION laArr[lnK]
  laArr[lnK] = SUBSTR(lcStr,AT(lcSepr,lcStr,1)+1,AT(lcSepr,lcStr,2)-2)
  lcStr = SUBSTR(lcStr,AT(lcSepr,lcStr,2))
ENDFOR

*-- end of lfStr2Arr.







*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
*:***************************************************************************
return

IF !USED('DIM')
  USE C:\WORK\DIM ORDER 1
ENDIF
  
lcTmpClr = 'DIM'
lcTempFl = 'ALLCOLOR'

CREATE CURSOR &lcTempFl (CDIM C(30))
INDEX ON CDIM TAG &lcTempFl

SELECT &lcTmpClr
SET ORDER TO 1

GO TOP
DO WHILE !EOF()
  *C122068,8  TMI [Start] 
  *lcStyle = &lcTmpClr..STYLE
  lcStyle = PADR(&lcTmpClr..STYLE,lnMajorLen)
  *C122068,8  TMI [End  ] 
  lcColor = &lcTmpClr..COLOR
  lcDim   = &lcTmpClr..DIM
  lcDim = '|'
  SCAN REST WHILE STYLE+COLOR+DIM = lcStyle
    IF !EMPTY(&lcTmpClr..DIM) .AND. !&lcTmpClr..DIM $ lcDim
      lcDim = lcDim + &lcTmpClr..DIM + '|'
    ENDIF
  ENDSCAN
  lcDim = PADR(lcDim,30)
  IF !SEEK(lcDim,lcTempFl)
    INSER INTO &lcTempFl VALUES (lcDim)
  ENDIF
ENDDO  


lcTmpClr = 'DIM'
lcTempFl = 'EACHCOLOR'

CREATE CURSOR &lcTempFl (CDIM C(30))
INDEX ON CDIM TAG &lcTempFl

SELECT &lcTmpClr
GO TOP
DO WHILE !EOF()
  *C122068,8  TMI [Start] 
  *lcStyle = &lcTmpClr..STYLE
  lcStyle = PADR(&lcTmpClr..STYLE,lnMajorLen)
  *C122068,8  TMI [End  ] 
  lcColor = &lcTmpClr..COLOR
  lcDim   = &lcTmpClr..DIM
  lcDim = '|'
  SCAN REST WHILE STYLE+COLOR+DIM = lcStyle+lcColor 
    IF !EMPTY(&lcTmpClr..DIM) .AND. !&lcTmpClr..DIM $ lcDim
      lcDim = lcDim + &lcTmpClr..DIM + '|'
    ENDIF
  ENDSCAN
  lcDim = PADR(lcDim,30)
  IF !SEEK(lcDim,lcTempFl)
    INSER INTO &lcTempFl VALUES (lcDim)
  ENDIF
ENDDO  
