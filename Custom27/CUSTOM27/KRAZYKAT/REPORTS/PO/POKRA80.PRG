*:************************************************************************
*: Program file  : PoKra80
*: Program desc. : Cutting card report (for Krazy Kat) 
*:                 Report to list the PO lines and sales order lines .
*:                 Converted From Aria26 (Kra800 )
*: System : Aria Advantage Series                  
*: Module : Style Purchase Order
*: Date   : 1/16/2000
*: Developer: Nader Anis Mitry
*: C100736 Refer to.
*:************************************************************************
*: Calls : 
*:         Functions  : gfOpenFile()
*:                    : gfTempName()
*:                    : lfPrnRep()
*:                    : lfGNum()  
*:                    : lfPrtNot() 
*:                    : lfPrt()
*:                    : lfCalcDate()
*:                    : lfPrnPrePk()
*:                    : lfPrnHdr() 
*:                    : lfOpnTosel() 
*:                    : lfShipped() 
*:                    : lfvDel()
*:                    : lfvCom()
*:                    : lfvPbOk()
*:                    : lfvDateRng() 
*:                    : lfCrtSetup()
*:                    : lfvStyle()  
*:                    : lfvFabric()
*:                    : lfvdir() 
*:                    : lfwOldDate()
*:         Procedures : lpPrtHdr
*:************************************************************************
*: Modifications :
*:************************************************************************

IF !USED('SYCCOMP')
    USE (gcSysHome+'SYCCOMP') ORDER TAG Ccomp_id IN 0 SHARED
  ENDIF
  *E301077,72 Open Company file [End  ]

  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK gcAct_Comp
  lcCompName = cCom_Name             && Company Name.

IF USED('SYCCOMP')
  USE IN Syccomp
ENDIF  


lnArrInd = 0
llNewPag = .T.
lcIScale = SPACE(01)
lnMajMask=LEN(gfItemMask('PM'))
lnMinMask=LEN(gfItemMask('PN'))

lcMcompDir=ALLTRIM(lcMcompDir)
IF EMPTY(lcMcompDir)
  RETURN
ENDIF  
= gfOpenFile(lcMcompDir+"ITPOSHIP",lcMcompDir+"Style","SH")
= gfOpenFile(lcMCompDir+"ITHBILLT",lcMcompDir+"HBILLNUM","SH")

*-- Copy selected records from style file to a temporary file.

WAIT WINDOW 'Validating selection criteria ...' NOWAIT

SELECT Style

IF USED(lcStyTmp)
  USE IN (lcStyTmp)
ENDIF
 

COPY TO (gcWorkDir+lcStyTmp) FOR &lcRpExp
lnNumRec=_Tally
** Msg *** There are No Records to Display ****
IF lnNumRec=0 
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ELSE
  =gfOpenFile(gcWorkDir+lcStyTmp) 
  SELECT &lcStyTmp
  INDEX ON STYLE TAG &lcStyTmp
ENDIF


DO CASE
  CASE EMPTY(ldRpDFro) AND EMPTY(ldRpDt) 
    lcFilter = "!EMPTY(ITPOSHIP.STYLE)"
  CASE EMPTY(ldRpDFro) 
    lcFilter = "ITHBILLT.dDelvDATE <= ldRpDt "
  CASE EMPTY(ldRpDt)
    lcFilter = "ITHBILLT.dDelvDATE >= ldRpDfro "
  OTHERWISE
    lcFilter = "ITHBILLT.dDelvDATE BETWEEN ldRpDfro AND ldRpDt"
ENDCASE

DO CASE
  CASE EMPTY(ldRpcfrom) AND EMPTY(ldRpCto) 
    lcFilter1 = "!EMPTY(ORDLINE.STYLE)"
  CASE EMPTY(ldRpcfrom) 
    lcFilter1 = "ORDLINE.COMPLETE <= ldRpCto "
  CASE EMPTY(ldRpCto)
    lcFilter1 = "ORDLINE.COMPLETE >= ldRpcfrom "
  OTHERWISE
    lcFilter1 = "ORDLINE.COMPLETE BETWEEN ldRpcfrom AND ldRpCto"
ENDCASE


SELECT ITPOSHIP.PO,ITPOSHIP.STYLE,SUM(ITPOSHIP.TOTQTY) AS TOTQTY,;
       ITHBILLT.CVENDOR,ITHBILLT.DDELVDATE,POSHDR.AVAILABLE,;
       POSHDR.CONTACT, POSHDR.CFOB,POSHDR.SHIPVIA,;
       POSHDR.ENTERED , .F. AS PRINTED,&lcStyTmp..Scale;
       FROM ITPOSHIP,&lcStyTmp ,POSHDR,ITHBILLT;
       WHERE ITPOSHIP.STYLE =  &lcStyTmp..STYLE;
       AND &lcFilter ;
       AND ITHBILLT.CSHIPNUM = ITPOSHIP.CSHIPNUM;
       AND POSHDR.PO = ITPOSHIP.PO;
       AND POSHDR.STATUS <> "X" ;
       INTO TABLE (gcWorkDir+lcPosLTmp);
       GROUP BY ITPOSHIP.STYLE,ITPOSHIP.PO  
 
INDEX ON Style    TAG &lcPosLTmp1
INDEX ON Style+PO TAG &lcPosLTmp

GO TOP

*-- Selecting sales order lines 

SELECT ORDLINE.ORDER,ORDLINE.LINENO,ORDLINE.STYLE ,ORDLINE.ACCOUNT ,;
       ORDLINE.START, ORDLINE.COMPLETE,SUM(ORDLINE.TOTQTY) AS TOTQTY,;
       ORDHDR.CUSTPO, ORDHDR.NOTE1 ,.F. AS PRINTED,OrdLine.Scale ;
       FROM ORDLINE , &lcStyTmp ,ORDHDR;
       WHERE ORDLINE.STYLE+DTOS(ORDLINE.COMPLETE)+ORDLINE.ORDER+;
       ORDLINE.STORE+STR(ORDLINE.LINENO,6) = &lcStyTmp..STYLE ;
       AND &lcFilter1;
       AND ORDHDR.ORDER = ORDLINE.ORDER;
       AND  ORDHDR.STATUS <> "X" ;
       INTO TABLE (gcWorkDir+lcOrdLTmp);
       GROUP BY ORDLINE.STYLE,ORDLINE.ORDER


INDEX ON SUBSTR(STYLE,1,lnMajMask)+DTOS(COMPLETE)+ORDER TAG &lcOrdLTmp

GO TOP

** There are No Records to Display ****

IF EOF(lcPosLTmp) AND EOF(lcOrdLTmp)
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF


SELECT DISTINCT SUBSTR(&lcStyTmp..STYLE,1,lnMajMask) as Style,&lcStyTmp..PrePak;
FROM &lcStyTmp ;
INTO TABLE (gcWorkDir+lcStyTmp1)  
  
XTITLE1 = 'Completion dates between '+DTOC(ldRpcFrom)+' AND '+DTOC(ldRpCtO)
XTITLE2 = 'Delivery dates between '+ DTOC(ldRpDFro)+ ' AND '+DTOC(ldRpDt) 
R_TITLE = 'CUSTOMIZED CUTTING CARD REPORT'
R_WIDTH ='W'

STORE 0  TO PAGENO,laTotQty,laSubTot,lnTotQty,laOTS,laTotOpQty

ROW     = 99
lcSty   = SPACE(19)
laColor = SPACE(6)
lnMaxRow= 59

CLEAR TYPEAHEAD
SET DEVICE TO SCREEN
WAIT WINDOW 'REPORT PRINTING - PLEASE WAIT' NOWAIT
SET DEVICE TO PRINT

SELECT(lcStyTmp1)
SET RELATION TO STYLE INTO &lcPosLTmp , STYLE INTO &lcOrdLTmp ADDITIVE

SELECT(lcOrdLTmp)
SET RELATION TO ORDER +STR(LINENO,6) INTO INVLINE ADDITIVE
 *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
SELECT(lcStyTmp1)

SCAN
  SET DEVICE TO PRINT
  lcSty = &lcStyTmp1..STYLE
  llPrtNot = .F.

  lcHeader  = "VND P/O   VIA  BUYER        ORDER DT DLVRY DT SOLD FOR          SPCL NOTES"
  lcLine    = "--- ---   ---  -----        -------- -------- --------          ----------"

  =lfPrnRep(lcPosLTmp,"PO")
  
  llPrtNot = .F.   
  
  lnGNum = lfGNum(lcSty,lcOrdLTmp)
  =lfShipped()
  lcHeader = "SOLD TO              P.O. #     P.O. DT  DLVRY DT   NOTE"
  lcLine   = "-------------------- ---------- -------- --------   --------------------"
 
  =lfPrnRep(lcOrdLTmp,"SO")
ENDSCAN
WAIT CLEAR
DO ENDREPORT

*!**************************************************************************
*! Name      : lfPrnRep
*! Developer : Sherif Attala Ishak
*! Date      : 12/23/1996
*! Purpose   : Print  PO lines & sales order lines .
*!**************************************************************************
*! Calls :
*!           : Functions  : lfGNum(),lfPrtNot(),lfPrt()
*!           : Procedures : lpPrtHdr 
*!**************************************************************************
*! Passed Parameters  : lcFile,lcTyp
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfPrnRep(lcOrdLTmp,"SO")
*!**************************************************************************
FUNCTION lfPrnRep
PARAMETERS lcFile,lcTyp

IF EOF(lcFile)
  
  IF lcFile = "&lcPosLTmp"
    llNewPag = .F.
  ENDIF
 
  RETURN
ENDIF
 
lnGNum = lfGNum(lcSty,lcFile)
lnGroup = 0


lcStyleNam = SPACE(19)

lnRecNo = 1


FOR lnCount = 1 TO lnGNum
  lcOrder  = SPACE(6)
  llFirst  = .T.
  laTotQty = 0
  laTotOpQty = 0
  laSubTot = 0

  
  laOTS    = 0
  
  lnElement = (lnCount-1) * 7 + 1
  GO RECNO() IN &lcStyTmp1
  SELECT(lcFile)

   
  SCAN WHILE  STYLE =&lcStyTmp1..STYLE FOR !PRINTED 
    
    IF ROW >= lnMaxRow OR SUBSTR(STYLE,1,lnMajMask) <> SUBSTR(lcStyleNam,1,lnMajMask)
      
     
     
    
      PAGENO = PAGENO+1
      DO lpPrtHdr WITH 'POKRA80',lcRPoptitl,XTITLE1,XTITLE2,R_WIDTH
      ROW = IIF(EMPTY(lcRpOpTitl),5,6)
      
      
      
      lcStyleNam = STYLE
     
     
    ENDIF
    IF !llPrtNot
      ROW = ROW + 1
        
      
      @ ROW,0 SAY "STYLE -> " + SUBSTR(ALLTRIM(lcSty),1,10) +" - "      
      
      @ ROW,22 SAY "Scale " + IIF(SEEK("S"+&lcFile..Scale,"SCALE"),"["+SCALE+"]:","")
      
      lnCol = 0
      FOR lnCounter = 1 TO 8
        lcCounter = STR(lnCounter,1)
      
         @ ROW,35 + lnCol SAY SCALE.Sz&lcCounter
        lnCol = lnCol + 7
      ENDFOR
      ROW = ROW + 1
      
      IF SEEK('P'+&lcFile..Scale+&lcStyTmp1..PrePak,'Scale')
        =lfPrnPrePk()
      ENDIF
      
       
      =lfPrtNot()
    ENDIF
    *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

    IF lnGroup <> lnCount
      
       
      
      Row = Row + 1
      =lfPrnHdr()
      
      
      @ ROW,0 SAY lcHeader
      =lfPrt("laColor",.T.,.F.)
      @ ROW,0 SAY lcLine
      =lfPrt("laColor",.F.,.T.)
      
      
       
      
      Row = Row + 1
      =lfPrnHdr()
      
      
      lnGroup = lnCount
      lcOrder = SPACE(6)
    ENDIF
   
    lnColor=SUBSTR(style,lnMajMask+2,lnMinMask)
    lnElmNum = ASCAN(laColor,lnColor,lnElement,7)
    IF lnElmNum <> 0
      lnElmNum1 = lnElmNum -lnElement + 1
      REPLACE PRINTED WITH .T.
      IF lcTyp = "PO" 
        IF lcOrder <> PO
          lcOrder = PO
          ROW =ROW +1  
          @ ROW,0 SAY SUBSTR(CVENDOR,1,3)
          @ ROW,4 SAY SUBSTR(PO,1,5)
          
          
          
          @ ROW,10 SAY SHIPVIA
          @ ROW,15 SAY SUBSTR(CONTACT,1,10)
          
          
          

          

          @ ROW,28 SAY ENTERED
                              
          @ ROW,37 SAY dDelvDATE
          @ ROW,46 SAY Available
          
          @ ROW,64 SAY SUBSTR(CFOB,1,13)
          
          
        ENDIF
        
        @ ROW,78+((lnElmNum1-1)* 8) SAY TOTQTY PICTURE '9999999'        
        
       
        
        
      ELSE
        *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
        IF llFirst
          llFirst = .F.
          ldComDat=lfCalcDate(COMPLETE)
        ENDIF
        IF COMPLETE > ldComDat
          ROW =ROW +1
          =lfPrt("laSubTot",.F.,.T.)
          ROW = ROW + 1
          
          @ ROW,9 SAY "Total UpTo " + DTOC(ldComDat) + " Delivery" 
          =lfPrt("laSubTot",.T.,.T.)
          ldComDat=lfCalcDate(COMPLETE)
          laSubTot = 0
          ROW = ROW +1
        ENDIF
        
        lnAlias1    = SELECT()
        lcTmpOrdr1 = ORDER
        lcTmpStyl1 = STYLE
        
        SELECT INVLINE
        lnRecNO2 = IIF(EOF(),0,RECNO())
        
        SUM TOTQTY TO lnInvQty FOR INVLINE.ORDER  +INVLINE.STYLE  = ;
                       lcTmpOrdr1 + lcTmpStyl1 
        IF lnRecNo2 <> 0
          GOTO lnRecNo2
        ENDIF
        SELECT(lnAlias1)
        lnTotQty = (lnInvQty + TotQty)
        
        IF lcOrder <> ORDER
          lcOrder = ORDER
          ROW =ROW +1  
           
          @ ROW,0 SAY IIF(SEEK("M"+ACCOUNT,"CUSTOMER"),SUBSTR(CUSTOMER.BTNAME,1,20),'')
          @ ROW,21 SAY SUBSTR(CUSTPO,1,10)
          @ ROW,32 SAY START
          @ ROW,41 SAY COMPLETE
          @ ROW,52 SAY SUBSTR(ALLTRIM(NOTE1),1,20)
        ENDIF

        
        
        @ ROW,78+((lnElmNum1-1)* 8) SAY TotQty PICTURE '9999999'
      

       
        laSubTot(lnElmNum) = laSubTot(lnElmNum) + TotQty
        
        lnRecNo = RECNO()
      ENDIF
      laTotQty(lnElmNum) = laTotQty(lnElmNum) + IIF(lcTyp = "SO",lnTotQty,TOTQTY)
      
      laTotOpQty(lnElmNum) = laTotOpQty(lnElmNum) +TOTQTY
     
    ELSE
      LOOP
    ENDIF
  ENDSCAN
  *-- Print group footer
  ROW = ROW + 1
  IF lcTyp = "SO"
    GO lnRecNo
    ldComDat=lfCalcDate(COMPLETE)
    =lfPrt("laSubTot",.F.,.T.)
    ROW = ROW + 1
    @ ROW,9 SAY "Total UpTo " + DTOC(ldComDat) + " Delivery" 
    =lfPrt("laSubTot",.T.,.T.)
    ROW = ROW + 1
  ENDIF
  =lfPrt(" ",.F.,.T.)
  ROW = ROW + 1
  @ ROW,9 SAY IIF(lcTyp = "PO","Orders Placed Total","Sales Orders total")
  =lfPrt("laTotQty",.T.,.T.)
  
  
  =lfOpnToSel()
 
  ROW = ROW + 2
ENDFOR


*!**************************************************************************
*! Name      : lpPrtHdr
*! Developer : Sherif Attala Ishak
*! Date      : 12/22/1996
*! Purpose   : PRINTS THE HEADER ON THE REPORTS
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Passed Parameters  : XPROG,XRPTNAME,XRPTNAME1,XTYPE
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : DO lpPrtHdr WITH 'KRA800',XTITLE,XTITLE1,R_WIDTH
*!**************************************************************************
PROCEDURE lpPrtHdr
PARAMETER XPROG,XRPTNAME,XRPTNAME1,XRPTNAME2,XTYPE

PRIVATE ALL LIKE X*
XRPTNAME = TRIM(XRPTNAME)
XRPTNAME1 = TRIM(XRPTNAME1)
R_TITLE  = TRIM(R_TITLE)

DO CASE
 CASE XTYPE = 'W'                            && WIDE REPORT
  X1 = ((130 - (LEN(TRIM(lcCompName))))/2)
  X2 = ((130 - (LEN( R_TITLE  )))/2)
  X3 = ((130 - (LEN( XRPTNAME )))/2)
  X4 = ((130 - (LEN( XRPTNAME1 )))/2)
  X5 = ((130 - (LEN( XRPTNAME2 )))/2)
  @ 01,000 SAY XPROG
  @ 01,X1  SAY lcCompName
  @ 01,120 SAY DATE()
  @ 01,129 SAY '~'   
  @ 02,000 SAY TIME()
  @ 02,X2  SAY R_TITLE 
  @ 02,120 SAY 'PAGE#'
  @ 02,126 SAY STR(PAGENO,4)
  lnRow = 3
  IF !EMPTY(XRPTNAME)
    @ lnRow,X3  SAY XRPTNAME
    lnRow = lnRow+1
  ENDIF
  @ lnRow,X4  SAY XRPTNAME1
  lnRow = lnRow+1
  @ lnRow,X5 SAY XRPTNAME2
  lnRow = lnRow+1
  @ lnRow,00 SAY REPLICATE('*',132)

 CASE XTYPE = 'XW'                            && XTRA WIDE REPORT
  X1 = ((203 - (LEN(TRIM(lcCompName))))/2)

  X2 = ((203 - (LEN( R_TITLE  )))/2)
  X3 = ((203 - (LEN( XRPTNAME )))/2)

  @ 01,000 SAY XPROG
  @ 01,X1  SAY lcCompName
  @ 01,190 SAY DATE()
  @ 01,199 SAY '~'   &&TAK 06/05/94
  @ 02,000 SAY TIME()
  @ 02,X2  SAY R_TITLE 
  @ 02,190 SAY 'PAGE#'
  @ 02,196 SAY STR(PAGENO,4)
  @ 03,X3  SAY XRPTNAME
  @ 04,00 SAY REPLICATE('*',225)

 CASE XTYPE='N'                                      && NARROW REPORT
  X1 = ((80 - (LEN(TRIM(lcCompName))))/2)
  X2 = ((80 - (LEN( R_TITLE  )))/2)
  X3 = ((80 - (LEN( XRPTNAME )))/2)

  @ 01,000 SAY XPROG
  @ 01,X1  SAY lcCompName
  @ 01,70 SAY DATE()
  @ 01,79 SAY '~'   &&TAK 06/05/94
  @ 02,000 SAY TIME()
  @ 02,X2  SAY R_TITLE
  @ 02,70 SAY 'PAGE#'
  @ 02,76 SAY STR(PAGENO,4)
  @ 03,X3 SAY XRPTNAME
  @ 04,00 SAY REPLICATE('*',80)
ENDCASE

RETURN

*!**************************************************************************
*! Name      : lfGNum
*! Developer : Sherif Attala Ishak
*! Date      : 12/22/1996
*! Purpose   : Calculate the number of color groups for eash style.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Passed Parameters  : lcStyle
*!**************************************************************************
*! Returns            : lnGrpNum
*!**************************************************************************
*! Example            : lfGNum(&lcStyTmp..STYLE)
*!**************************************************************************
FUNCTION lfGNum
PARAMETERS lcStyle,lcFile


SELECT DISTINCT &lcStyTmp..STYLE  ;
FROM &lcFile,&lcStyTmp;
WHERE &lcStyTmp..STYLE  = ;
     &lcFile..STYLE  ;
 AND SUBSTR(&lcStyTmp..STYLE,1,lnMajMask) = LCSTYLE;
INTO CURSOR (lcCountTmp)


lnCount = _TALLY
lnGrpNum = CEILING(lnCount/7)
IF lnCount > 0
  lnArr = lnGrpNum * 7
  DIMENSION laColor(lnArr)
  DIMENSION laTotQty(lnArr)
  DIMENSION laTotOpQty(lnArr)
  DIMENSION laSubTot(lnArr)
  DIMENSION laOTS(lnArr)
ENDIF

STORE 0 TO laTotQty,laTotOpQty

STORE 0 TO laSubTot
STORE SPACE(6) TO laColor
I=0
SCAN
  I=I+1
   
  lcColor =SUBSTR(style,lnMajMask+2,lnMinMask)
  store lcColor to lacolor[i]
ENDSCAN

lnArrInd = I

RETURN lnGrpNum

*!**************************************************************************
*! Name      : lfPrt
*! Developer : Sherif Attala Ishak
*! Date      : 12/22/1996
*! Purpose   : Print a group of color or total qty.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Passed Parameters  : lcArray,llPrint1,llPrint2
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfPrt("lcColor",.T.,.T.)
*!**************************************************************************
FUNCTION lfPrt
PARAMETERS lcArray,llPrint1,llPrint2

IF llPrint1
  lnCol =0
  FOR I = lnElement TO (lnElement+6)

    
   
   
     @ ROW,IIF(lcArray="laColor",79+lnCol,78+lnCol) SAY IIF(lcTyp="SO".AND.lcArray = "laTotQty",laTotOpQty(I),&lcArray(I));
                         PICTURE IIF(lcArray = "laColor",'@Z','@Z 9999999')

   

  
    
    lnCol = lnCol + 8 
  ENDFOR
  ROW = ROW + 1
ENDIF
IF llPrint2
  lnCol =0
  FOR I = lnElement TO (lnElement+6)
    IF EMPTY(laColor(I))
      LOOP  
    ENDIF

    
    IF lcArray <> "laOTS"
       

        @ ROW,IIF(lcArray="laColor",79+lnCol,78+lnCol) SAY ;
            IIF(lcTyp = "SO" AND (lcArray="laTotQty" OR lcArray=" "),;
            '======',"------")+IIF(lcArray = "laColor","",;
            IIF(lcTyp="SO" AND (lcArray="laTotQty" OR lcArray=" "),"=","-"))

    ELSE
      
      @ Row,78+lnCol SAY "*******"      
      

    ENDIF        
   
    
    lnCol = lnCol + 8
  ENDFOR
ENDIF
*!**************************************************************************
*! Name      : lfPrtNot
*! Developer : Sherif Attala Ishak
*! Date      : 12/22/1996
*! Purpose   : Print style notepad.
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfPrtNot()
*!**************************************************************************
FUNCTION lfPrtNot
lcAlias = Alias()
lnNotLine = 1
SELECT NOTEPAD
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 72

SEEK 'F' + SUBSTR(lcSty,1,lnMajMask)
lnMemLins = MEMLINES(NOTEPAD.MNOTES)
IF TYPE + KEY = 'F' + SUBSTR(lcSty,1,lnMajMask)
  DO WHILE lnNotLine <= lnMemLins

   
    =lfPrnHdr()
   
    
    IF !EMPTY(MLINE(MNOTES,lnNotLine))
      
      @ ROW,20 SAY MLINE(MNOTES,lnNotLine)
     
      ROW = ROW + 1
    ENDIF
    lnNotLine = lnNotLine + 1
  ENDDO
ENDIF
SET MEMOWIDTH TO lnOldMemW
llPrtNot = .T.
SELECT &lcAlias

*!**************************************************************************
*! Name      : lfCalcDate
*! Developer : Sherif Attala Ishak
*! Date      : 12/22/1996
*! Purpose   :Return the date of middle or end of the month .
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Passed Parameters  : ldCdate
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfCalcDate(date())
*!**************************************************************************
FUNCTION lfCalcDate
PARAMETERS ldCdate
IF DAY(ldCdate) <= 15
  RETURN (ldCdate + 15 - DAY(ldCdate))
ENDIF 
lnMonth=IIF(MONTH(ldCdate)+1 = 13,'01',ALLTRIM(STR(MONTH(ldCdate)+1)))
ldDate = CTOD(lnMonth+'/'+'1'+'/'+SUBSTR(ALLTRIM(STR(YEAR(ldCdate)+ IIF(lnMonth = "01",1,0))),3,2))-1
RETURN ldDate

*!**************************************************************************
*! Name      : lfPrnPrePk
*! Developer : Khalid Mohi El-Din
*! Date      : 03/08/1998
*! Purpose   :To print the PrePacks of the style's scale
*!**************************************************************************
*! Example            : lfPrnPrePk()
*!**************************************************************************
*C101126,1 KHM 03/09/98 Added
*!**************************************************************************
FUNCTION lfPrnPrePk
PRIVATE lnAlias,lnCol,lcScale

lnAlias   = SELECT(0)

SELECT Scale

*-- To print the header when the row exceeds the Max row.
=lfPrnHdr()


lnCol     = 35



@ Row,22 SAY "Prepak" +"["+PrePak+"]:"



FOR lnCounter = 1 TO 8
  lcCounter = STR(lnCounter,1)
  IF Pp&lcCounter > 0
    
  
     @ Row,lnCol+2 SAY Pp&lcCounter
    

   
    lnCol = lnCol + 7
   
  ENDIF
ENDFOR

Row = Row + 1

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfPrnHdr
*! Developer : Khalid Mohi El-Din
*! Date      : 03/08/1998
*! Purpose   :To print the header when the row exceeds the max row.
*!**************************************************************************
*! Example            : lfPrnHdr()
*!**************************************************************************
*C101126,1 KHM 03/09/98 Added
*!**************************************************************************
FUNCTION lfPrnHdr

IF Row >= lnMaxRow
  PAGENO = PAGENO+1
  DO lpPrtHdr WITH 'POKRA80',lcRpOpTitl,XTITLE1,XTITLE2,R_WIDTH
  Row = IIF(EMPTY(lcRpOpTitl),5,6)
  Row = Row + 1
ENDIF

*!**************************************************************************
*! Name      : lfOpnToSel
*! Developer : Khalid Mohi El-Din
*! Date      : 03/08/1998
*! Purpose   :To print the Open To Sell quantities.
*!**************************************************************************
*! Example            : lfOpnToSel()
*!**************************************************************************
*C101126,1 KHM 03/09/98 Added
*!**************************************************************************
FUNCTION lfOpnToSel
PRIVATE lnAlias

lnAlias = SELECT(0)
IF lcTyp = "SO"
  lnPoQty = 0
  Row = Row + 1
  =lfPrnHdr()
  =lfPrt("laOTS",.F.,.T.)
  Row = Row + 1
  =lfPrnHdr()
  @ Row,9 SAY "Open To Sell" 
  SELECT (lcPosLTmp)
  lnPoRecNo = RECNO()
  SET ORDER TO &lcPosLTmp1
  FOR lnArrayElm = lnElement TO (lnElement+6)
    SELECT (lcPosLTmp)
    
    = SEEK (lcStyleNam)
   
   SUM REST TotQty WHILE Style = lcStyleNam TO lnPoQty
   
    laOTS[lnArrayElm] = lnPoQty - laTotQty[lnArrayElm]
    

  ENDFOR
  SET ORDER TO &lcPosLTmp
  IF BETWEEN(lnPoRecNo,1,RECCOUNT(lcPosLTmp))
     GO lnPoRecNo IN (lcPosLTmp)
   ENDIF  
   
  =lfPrt("laOTS",.T.,.T.)
ENDIF
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfShipped
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 06/24/1998
*! Purpose   : To print the Invoice
*!**************************************************************************
*! Example   : lfShipped()
*!**************************************************************************
FUNCTION lfShipped
PRIVATE lnAlias, lcTmpInv, lcTmpOrd,lnRecNo1,lnMRecNo,lnCounter,lnTotQty1

lnAlias  = SELECT()
lnMRecNo = RECNO()
lnIRecNo = RECNO('InvLine')
lnDim    = 1
llMulti  = .F.
llContin = .F.
DIMENSION laInvTot(lnDim,lnArrInd+6)
SELECT INVLINE
lcInvTemp = gfTempName()
SELECT (lcOrdLTmp)
SELECT DISTINCT &lcOrdLTmp..ORDER,&lcOrdLTmp..COMPLETE;
FROM &lcOrdLTmp;
WHERE STYLE+DTOS(COMPLETE)+ORDER= SUBSTR(lcSty,1,lnMajMask);
   INTO CURSOR (lcInvTemp)
IF _TALLY = 0
  RETURN
ENDIF

SCAN
  lcTmpOrd  = ORDER
  ldComDate = COMPLETE
  SELECT INVLINE
  GOTO TOP
  IF SEEK(lcTmpOrd)
    
    llContin = .T.
    lcTmpInv  = INVLINE.Invoice
    lcIScale  = INVLINE.SCALE
    lcTmpAcc  = INVLINE.ACCOUNT
    ldInvDate = INVLINE.InvDate
    lnRecNo1 = RECNO()
    LOCATE REST FOR INVOICE <> lcTmpInv WHILE ORDER = lcTmpOrd
    llMulti = FOUND()
    GOTO lnRecNo1
    DIMENSION laInvTot(lnDim,lnArrInd+6)
    laInvTot(lnDim,lnArrInd+1) = IIF(SEEK("M"+lcTmpAcc,"CUSTOMER"),CUSTOMER.BTNAME,'')
    laInvTot(lnDim,lnArrInd+2) = ldComDate
    laInvTot(lnDim,lnArrInd+3) = lcTmpInv
    laInvTot(lnDim,lnArrInd+4) = ldInvDate
    laInvTot(lnDim,lnArrInd+5) = lcTmpOrd
    laInvTot(lnDim,lnArrInd+6) = llMulti
    FOR lnIndex = 1 TO lnArrInd
      SUM TotQty TO laInvTot(lnDim,lnIndex) FOR;
       Order = lcTmpOrd .AND.;
       STYLE  = lcSty +'-'+laColor(lnIndex)
    ENDFOR
       *STYLE  = SUBSTR(lcSty,1,12) laColor(lnIndex)
       
    
   lnDim = lnDim + 1
  ENDIF
ENDSCAN
IF !llContin
  RETURN
ENDIF
IF llNewPag
  PAGENO = PAGENO+1
ENDIF  


DO lpPrtHdr WITH 'POKRA80',lcRpOpTitl,XTITLE1,XTITLE2,R_WIDTH
ROW = IIF(EMPTY(lcRpOpTitl),5,6)
ROW = ROW + 1
@ ROW,0 SAY "STYLE -> " + SUBSTR(ALLTRIM(lcSty),1,10) +" - "
@ ROW,22 SAY "Scale " + IIF(SEEK("S"+lcIScale,"SCALE"),"["+lcIScale+"]:","")
lnCol = 0
FOR lnCounter = 1 TO 8
  lcCounter = STR(lnCounter,1)
  @ ROW,35 + lnCol SAY SCALE.Sz&lcCounter
  lnCol = lnCol + 7
ENDFOR
ROW = ROW + 1
IF SEEK('P'+lcIScale+&lcStyTmp1..PrePak,'Scale')
  =lfPrnPrePk()
ENDIF
=lfPrtNot()

lnMaxDim = lnDim - 1
DIMENSION laClrTot(lnArrInd)
STORE 0 TO laClrTot
lnDim  = 1
lnColr = 1
FOR lnDim = 1 TO lnArrInd
  FOR lnColr = 1  TO lnMaxDim

    laClrTot(lnDim) = laClrTot(lnDim) + laInvTot(lnColr,lnDim)
  ENDFOR
ENDFOR
ROW = ROW + 2
*---Start printing
lnTemp  =  7
lnStInd =  1
lnDim   =  1
llPrint = .T.
DO WHILE .T.
  IF llPrint
    @ ROW , 00 SAY "     FULL CUSTOMER NAME           INVOICE #    INVOICE DATE    ORDER #"  
    lnPos = 79
    FOR lnIndex = lnStInd  TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
      @ ROW , lnPos SAY laColor(lnIndex)
      lnPos = lnPos + 8
    ENDFOR
    ROW = ROW + 1    
    @ ROW , 00 SAY "------------------------------    ---------    ------------    -------"
    lnPos = 79
    FOR lnIndex = lnStInd TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
      @ ROW ,lnPos SAY "------"
      lnPos = lnPos + 8
    ENDFOR
  ENDIF
*---Start printing data
  ROW = ROW + 1
  @ ROW , 00 SAY laInvTot(lnDim,lnArrInd + 1)
  IF laInvTot(lnDim,lnArrInd + 6)
    @ ROW , 48 SAY laInvTot(lnDim,lnArrInd + 2)
  ELSE
    @ ROW , 35 SAY laInvTot(lnDim,lnArrInd + 3)
    @ ROW , 48 SAY laInvTot(lnDim,lnArrInd + 4)
  ENDIF
  @ ROW , 64 SAY laInvTot(lnDim,lnArrInd + 5)
*---End printing data
  lnPos = 79
  FOR lnIndex = lnStInd TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
    @ ROW ,lnPos+1 SAY IIF(laInvTot(lnDim,lnIndex) > 0 ,laInvTot(lnDim,lnIndex) ,'') PICTURE '99999'
    lnPos = lnPos + 8
  ENDFOR
  llPrint = .F.
  IF lnDim >= lnMaxDim
    ROW = ROW + 1
    lnPos = 79
    FOR lnIndex = lnStInd TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
      @ ROW ,lnPos SAY "------"
      lnPos = lnPos + 8
    ENDFOR
    ROW = ROW + 1
    @ ROW ,09 SAY "Invoices Total"
    lnPos = 79
    
    FOR lnIndex = lnStInd TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
      @ ROW ,lnPos+1 SAY IIF(laClrTot(lnIndex)>0,laClrTot(lnIndex),'') PICTURE '99999'
      lnPos = lnPos + 8
    ENDFOR
    ROW = ROW + 1
    lnPos = 79
    FOR lnIndex = lnStInd TO IIF(lnArrInd - lnStInd >= 7 ,lnTemp,lnArrInd)
      @ ROW ,lnPos SAY "------"
      lnPos = lnPos + 8
    ENDFOR
    ROW = ROW + 2
    lnDim   = 1
    lnStInd = lnTemp + 1
    lnTemp = lnTemp + 7
    llPrint = .T.
    ROW = ROW + 1
  ELSE
    lnDim   = lnDim + 1
  ENDIF
  IF lnStInd > lnArrInd
    EXIT
  ENDIF
  =lfPrnHdr()
ENDDO
RETURN
SELECT(lnAlias)
GOTO lnIRecNo IN InvLine
GOTO lnMRecNo
  


*!**************************************************************************
*! Name      : lfvDel
*! Developer : Nader Anis Mitry
*! Date      : 1/16/2000
*! Purpose   : To display the po deliver date in the date range screen
*!**************************************************************************
*! Calls :
*!           : Functions  : lfvDaterng()
*!**************************************************************************   
*! Called From : Valid Clause for lnDumDel in the Option Grid      
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : 
*!**************************************************************************    
FUNCTION lfvDel
  lnCurObj=1
  ldRpFrom=ldRpdfro
  ldrpto=ldrpdt
  =lfvDateRng()
  
*-- End of lfvDel.

*!**************************************************************************
*! Name      : lfvcom
*! Developer : Nader Anis Mitry
*! Date      : 1/16/2000
*! Purpose   : To display the completion date in the date range screen
*!**************************************************************************
*! Calls :
*!           : Functions  : lfvDaterng()
*!      
*!**************************************************************************   
*! Called From : Valid Clause for lnDumCom in the Option Grid
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : 
*!**************************************************************************    
FUNCTION lfvCom
  lnCurObj=2
  ldRpFrom=ldRpcfrom
  ldrpto=ldrpcto
  =lfvDateRng()
  *-- End of lfvCom.
*!**************************************************************************
*! Name      : lfvpbOk
*! Developer : Nader Anis Mitry
*! Date      : 1/16/2000
*! Purpose   : to validate the date entered by the user
*!**************************************************************************
*! Calls :
*!           : Functions  : gfModalgen()
*!      
*!**************************************************************************   
*! Called From : Valid Clause for the Ok push button in the date range screen
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : 
*!**************************************************************************    

FUNCTION lfvpbOk

IF ldRpFrom > ldRpTo
  *-- Text : Date RANGE FROM must be less than or equal TO Date range to  
  =gfModalGen('TRM00272B00000','DIALOG','Date "'+"|"+'" must be less than or equal'+"|"+'Date " range to " ') 
  _CUROBJ = OBJNUM(ldRpFrom)
ELSE
  DO CASE
    CASE lnCurObj=2
     ldRpcTo=ldRpTo
     ldRpcFrom=ldRpFrom
    CASE lnCurObj=1
     ldRpDT=ldRpTo
     ldRpDFro= ldRpFrom
  ENDCASE
  
  CLEAR READ
ENDIF
*-- End of lfvpbOk.
*!**************************************************************************
*! Name      : lfvDateRng
*! Developer : Nader Anis Mitry
*! Date      : 1/16/2000
*! Purpose   : To display the date range screen
*!**************************************************************************
*! Calls :
*!            Screen: ickra700.spx
*!      
*!**************************************************************************   
*! Called From : 
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : 
*!**************************************************************************    

FUNCTION lfvDateRng
lcTitle = 'Date range'

DO (gcRepHome + 'IC\ickra700.SPX')    && Run The Date Range screen
*-- End of lfvDateRng.

*!**************************************************************************
*! Name      : lfCrtSetup
*! Developer : Nader Anis Mitry
*! Date      : 01/16/2000
*! Purpose   : To display the setup screen for Krazy Kat
*!**************************************************************************
*! Calls :
*!           : Screens  : MFKRA800.SPX
*!      
*!**************************************************************************   
*! Called From : option grid
*!**************************************************************************
*! Example            : =lfCrtSetup()
*!**************************************************************************    

FUNCTION lfCrtSetup

IF !FILE(gcDataDir+'KRASETUP.MEM')
  DIMENSION laComp[1,2]
  STORE '' TO laComp , lcMcompdir , lcDir
  DO (gcrephome+"MF\MFKRA800.SPX")  && Run The Linked Directry and Selected Company.
ENDIF

IF FILE(gcDataDir+'KRASETUP.MEM')
  RESTORE FROM (gcDataDir+'KRASETUP.MEM') ADDITIVE
ELSE
  llOgTrmnat = .T.
  CLEAR READ
ENDIF 
 
*-- End of lfCrtSetup.

*!**************************************************************************
*! Name      : lfVAccpOk
*! Developer : Nader Anis Mitry
*! Date      : 01/16/2000
*! Purpose   : Validate push button accept in ICKRASETUP Screen
*!**************************************************************************
*! Example   : = lfVAccpOk()
*!**************************************************************************
*
FUNCTION lfVAccpOk
IF !EMPTY(lcDir)
  lcMcompdir =  laComp[PbComp,2]
  lcMcompdir = ALLTRIM(lcMcompdir)
  IF FILE(lcMcompdir+'ITPOSHIP.DBF') .AND. FILE(lcMcompdir+'ITHBILLT.DBF')
     CLEAR READ
     SAVE ALL LIKE lcMcompdir TO (gcDataDir+'KRASETUP.MEM')
  ELSE
    *-- TEXT : Company files can not be Found
    =gfModalGen('TRM00113B00000','DIALOG','found') 
    SET DEVICE TO SCREEN
    laComp = ''
    SHOW GET pbComp
    SHOW GET PbComp DISABLE
    SHOW GET PbOk   DISABLE
    RETURN
  ENDIF
ENDIF
*-- End of lfVAccpOk.

*!**************************************************************************
*! Name      : lfvStyle
*! Developer : Nader Anis Mitry
*! Date      : 1/16/2000
*! Purpose   : to display the Style browse screen 
*!**************************************************************************
*! Calls :
*!           : functions  : gfStyBrw()
*!      
*!**************************************************************************   
*! Called From : option grid
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : 
*!**************************************************************************    

 
FUNCTION lfvStyle
lcStyle = VARREAD()

lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
    *&lcStyle = STYLE.STYLE
  ELSE
    &lcStyle = gfStyBrw("M","","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF
SET ORDER TO lcTag IN STYLE
*--End of lfvStyle.

*!**************************************************************************
*! Name      : lfvFabric
*! Developer : Nader Anis Mitry
*! Date      : 1/16/2000
*! Purpose   : to display the fabric browse screen 
*!**************************************************************************
*! Calls :
*!           : functions  : FaBrow()
*!      
*!**************************************************************************   
*! Called From : option grid
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : 
*!**************************************************************************    



FUNCTION lfvFabric

lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF  
*-- End of lfvFabric.

*!**************************************************************************
*! Name      : lfvDir
*! Developer : Nader Anis Mitry
*! Date      : 1/16/2000
*! Purpose   : to display the select dir dialog box
*!**************************************************************************
*! Calls :
*!           : functions  : gfOpenFile()
*!      
*!**************************************************************************   
*! Called From : MFKRA800.SPX (KRAZY KAT SETUP SCREEN)
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : 
*!**************************************************************************    
FUNCTION lfVDir

lnCount = 0
lcDir = GETDIR()
lcDir = SUBSTR(lcDir,1,AT('\',lcDir,2))
SHOW GET lcDir
IF EMPTY(lcDir)
  RETURN
ENDIF
IF FILE(lcDir+'SYSFILES\SYCCOMP.DBF')
  USE IN (IIF(USED(lcAliasN),lcAliasN,0))
  *=gfOpenFile(lcDir+'SYSFILES\SYCCOMP',lcDir+'SYSFILES\Ccomp_id','SH',lcAliasN)
  =gfOpenFile(lcDir+'SYSFILES\SYCCOMP',lcDir+'SYSFILES\Ccomp_id','SH',@lcAliasN,.T.)
  SELECT (lcAliasN)
  COUNT TO lcCompNo FOR !EMPTY(CCOMP_id)
  DIMENSION laComp[lcCompNo,2]
  laComp = ''
  SCAN
    lnCount = lnCount + 1
    laComp[lnCount,1] = cComp_Id + '  ' + ALLTRIM(cCom_Name)
    laComp[lnCount,2] = ccom_ddir
  ENDSCAN  
  SHOW GET PbComp
ENDIF
IF lnCount > 0
  SHOW GET PbComp ENABLE
  SHOW GET PbOk   ENABLE
ENDIF
*-- END OF lfVDir.