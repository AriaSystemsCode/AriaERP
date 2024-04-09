*:************************************************************************
*: Program file  : SOFORCST.PRG
*: Program desc. : FORECASTING REPORT
*: System        : Aria Apparel Series. Ver 2.7
*: Module        : SO
*: Date          : 05/23/2002
*: Developer     : RANIA ABDEL RAZIK (RAE)
*: Refer to      : E301894
*:************************************************************************
*: Calls : 
*:         Functions  : lfGetWeek()
*:                    : lfvWeek()
*:                    : lfGetDtWk()
*:                    : lfGetDem()
*:                    : lfGetForc()			
*:                    : lfStyComp()
*:                    : lfGetHand()
*:                    : lfGetWip()
*:                    : lfGetAvail()
*:                    : lfPrnRep()	
*:                    : lfGetLine()	
*:                    : lfNewpag()	
*:                    : lfGetNeg()
*:         Procedures : RPT_HDR
*:                    : ENDREPORT
*:************************************************************************
*:Passed Parameters : None
*:************************************************************************
*:Example           : Do SOFORCST
*:************************************************************************
*:Modifications ..
*:B606822,4 ASH 12/26/2002 Fix 'Alias 'CUTTKTH' not found in case MF not installed.
*:B129506,1 MMR 09/18/2002 Fix Bug of not exiting the loop in case the current year changed.
*:************************************************************************

*-- Begin Of SOFORCST.PRG Program. --*

*-- Define Variables.
DIMENSION lcMTyp(1)   , lcITyp(1)
DIMENSION laForc(8)   , laDem(8)      , laHand(8)     , laTrans(8)   , laAvail(8)   , laSComp(8)        
*E301894,3 
*DIMENSION laForcWk(10), laDemWk(10)   , laHandWk(10)  , laTransWk(10), laAvailWk(10)
DIMENSION laForcWk(RECCOUNT('STYLE')) , laDemWk(RECCOUNT('STYLE'))   ,;
          laHandWk(RECCOUNT('STYLE')) , laTransWk(RECCOUNT('STYLE')) ,;
          laAvailWk(RECCOUNT('STYLE'))
*E301894,3          
DIMENSION laDate(9,2) , laWekYer(10,2), laSetupsM(5,2), laSetupsI(5,2)

STORE .F. TO llZeroAll ,llPrint
STORE {}  TO ldFOgDate, ldLOgDate     , laDate
STORE " " TO lcMTyp   , lcITyp        , lcCutCond     , lcPoCond      , lcSty1
STORE 0   TO lnITyp   , lnMTyp        , laForc        , laDem         , laHand       , laTrans   ,;
             laAvail  , laSComp       , laWekYer      ,lnTForc        , lnTDem       , lnTHand   ,;
             lnTTrans , lnTAvail      , lnNumWeek     , lnCurWeek     , lnWeek       , lnYear

*-- To get the length of the major segment.
lcMjrPct   = gfItemMask('PM')
lnMajWidth = LEN(lcMjrPct)
lnSpcReq = 19 - lnMajWidth

*-- Add filters.
lcRpExp = IIF(EMPTY(lcRpExp), ".T." , lcRpExp)
lcRpExp = lcRpExp + IIF(lcRpStatus<>' ',' .AND. Style.Status $ lcRpStatus','')
lcRpExp = lcRpExp + IIF(lcRpDomImp<>'B',IIF(lcRpDomImp='D',' .AND. Style.Make = .T.',' .AND. Style.Make = .F.'),'')

*-- Open Needed Files.
=gfOpenFile ('&gcDataDir.FORCAST','&gcDataDir.FORCAST','SH')

*-- Loop To Determine The Type Of Style Comp. For Manufactured And Imported Style.
laSetupsM[1,1] = 'M_CmType1'
laSetupsM[2,1] = 'M_CmType2'
laSetupsM[3,1] = 'M_CmType3'
laSetupsM[4,1] = 'M_CmType4'
laSetupsM[5,1] = 'M_CmType5'

laSetupsI[1,1] = 'M_CiType1'
laSetupsI[2,1] = 'M_CiType2'
laSetupsI[3,1] = 'M_CiType3'
laSetupsI[4,1] = 'M_CiType4'
laSetupsI[5,1] = 'M_CiType5'

=gfGetMemVar(@laSetupsM,gcAct_Comp)
=gfGetMemVar(@laSetupsI,gcAct_Comp)

FOR lnElmn = 1 TO 5
  lcElmn = STR(lnElmn,1)
  IF laSetupsM[lnElmn,2] = "S"
    lnMTyp = lnMTyp + 1
    DIMENSION lcMTyp(lnMTyp)
	lcMTyp(lnMTyp) = lcElmn     
  ENDIF
  IF laSetupsI[lnElmn,2] = "S"
    lnITyp = lnITyp + 1
    DIMENSION lcITyp(lnITyp)
    lcITyp(lnITyp) = lcElmn         
  ENDIF
ENDFOR

*-- Call lfGetWeek() Function.
=lfGetWeek(gdSysDate)


*-- Call lfGetDtWk() Function.
=lfGetDtWk(lnRpWeekNo)


*-- Create Temporary File To Hold Collecting Data.
CREATE TABLE (gcWorkDir+lcRpTmp) (WEEK N(2),STYLE C(19),;
                                  FORCAST1 N(6),FORCAST2 N(6),FORCAST3 N(6),;
                                  FORCAST4 N(6),FORCAST5 N(6),FORCAST6 N(6),; 
                                  FORCAST7 N(6),FORCAST8 N(6),TOTFORC  N(6),;
                                  DEMAND1  N(6),DEMAND2  N(6),DEMAND3  N(6),;
                                  DEMAND4  N(6),DEMAND5  N(6),DEMAND6  N(6),;
                                  DEMAND7  N(6),DEMAND8  N(6),TOTDEM   N(6),;
                                  ONHAND1  N(6),ONHAND2  N(6),ONHAND3  N(6),;
                                  ONHAND4  N(6),ONHAND5  N(6),ONHAND6  N(6),;
                                  ONHAND7  N(6),ONHAND8  N(6),TOTHAND  N(6),;
                                  TRANS1   N(6),TRANS2   N(6),TRANS3   N(6),;
                                  TRANS4   N(6),TRANS5   N(6),TRANS6   N(6),;
                                  TRANS7   N(6),TRANS8   N(6),TOTTRANS N(6),;
                                  AVAIL1   N(6),AVAIL2   N(6),AVAIL3   N(6),;
                                  AVAIL4   N(6),AVAIL5   N(6),AVAIL6   N(6),;
                                  AVAIL7   N(6),AVAIL8   N(6),TOTAV    N(6))
INDEX ON Style TAG &lcRpTmp
*-- SET Relations.
SELECT ORDLINE
SET RELATION TO ORDLINE.cordtype+ORDLINE.ORDER INTO ORDHDR ADDITIVE
*B606822,4 ASH 12/26/2002 (Begin) Open POSLN in case of related modules installed.
IF OCCURS('PO',gcCmpModules)<>0 OR OCCURS('MF',gcCmpModules)<>0 OR OCCURS('PS',gcCmpModules)<>0 OR OCCURS('RS',gcCmpModules)<>0
*B606822,4 ASH 12/26/2002 (End) 
  SELECT POSLN
  SET RELATION TO Posln.cstytype+ Posln.po INTO POSHDR ADDITIVE
ENDIF
*B606822,4 ASH 12/26/2002 (Begin) Open CUTTKTL file in case of MF module installed.
IF OCCURS('MF',gcCmpModules)<>0
*B606822,4 ASH 12/26/2002 (End)
  SELECT CUTTKTL
  SET RELATION TO CUTTKTL.Cuttkt INTO CUTTKTH ADDITIVE
ENDIF
*-- Collecting Data in Temporary File.
SELECT STYLE
SCAN For &lcRpExp 
  STORE 0 TO lnNoWeek
  WAIT WINDOW 'Collecting data for ' + lcStyTit +': ' + Style NOWAIT
  FOR lnCount = 1 TO 10
    =lfGetDem(lnCount,Style) 
    =lfGetForc(lnCount,Style) 
    =lfGetHand(lnCount) 
    =lfGetWip(lnCount,Style)
    =lfGetAvail(lnCount,Style)

    INSERT INTO (lcRpTmp) ;
                (WEEK, STYLE, FORCAST1, FORCAST2, FORCAST3, FORCAST4, FORCAST5             ,;
                 FORCAST6,FORCAST7,FORCAST8,TOTFORC,DEMAND1,DEMAND2,DEMAND3                ,;
                 DEMAND4 ,DEMAND5 ,DEMAND6 ,DEMAND7,DEMAND8,TOTDEM ,ONHAND1                ,;
                 ONHAND2 ,ONHAND3 ,ONHAND4 ,ONHAND5,ONHAND6,ONHAND7,ONHAND8,TOTHAND        ,;
                 TRANS1  ,TRANS2  ,TRANS3  ,TRANS4 ,TRANS5 ,TRANS6 ,TRANS7 ,TRANS8,TOTTRANS,;
                 AVAIL1,AVAIL2,AVAIL3,AVAIL4,AVAIL5,AVAIL6,AVAIL7,AVAIL8,TOTAV)             ;
    VALUES (laWekYer(lnCount,1),STYLE.Style,laForc(1),laForc(2),laForc(3)                  ,;
                 laForc(4) ,laForc(5) ,laForc(6) ,laForc(7) ,laForc(8) ,lnTForc            ,;
                 laDem(1)  ,laDem(2)  ,laDem(3)  ,laDem(4)  ,laDem(5)  ,laDem(6),laDem(7)  ,;
                 laDem(8)  ,lnTDem    ,laHand(1) ,laHand(2) ,laHand(3) ,laHand(4)          ,;
                 laHand(5) ,laHand(6) ,laHand(7) ,laHand(8) ,lnTHand   ,laTrans(1)         ,;
                 laTrans(2),laTrans(3),laTrans(4),laTrans(5),laTrans(6)                    ,;
                 laTrans(7),laTrans(8),lnTTrans  ,laAvail(1),laAvail(2),laAvail(3)         ,;
		      	 laAvail(4),laAvail(5),laAvail(6),laAvail(7),laAvail(8),lnTavail)          
  ENDFOR
ENDSCAN
WAIT CLEAR

*------------------------------------
* [REPORT] LOOP
*------------------------------------
XREPORT = 'SOFORCST'
R_TITLE = 'FORECASTING REPORT'
R_WIDTH ='W'


STORE 0  TO PAGENO
lnRow     = 99
lnMaxRow  = 59


*-- If Print Total.
IF lcRpByST = "T"
  DIMENSION laSizes(1)
  STORE .F. TO laSizes
  lnSizes = 1
ENDIF

*-- Initialize Printer & Call Function To Print.
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
SELECT STYLE
GO TOP
SCAN FOR &lcRpExp
  =SEEK("S"+STYLE.Scale,"SCALE")
  =SEEK(Style.Style,lcRpTmp)
  IF lcRpByST = "S"
    DIMENSION laSizes(SCALE.Cnt)
    lnSizes = SCALE.Cnt
  ENDIF  
  STORE .F. TO laSizes
  SELECT &lcRpTmp
  IF lcRpNegAll = "N"
    =lfGetNeg()
  ENDIF
  =lfPrnRep(lnSizes)
ENDSCAN  

*-- Display Message IF There are no Records to display.
SELECT (lcRpTmp)
GOTO TOP
IF llPrint
  WAIT CLEAR
  DO ENDREPORT
ELSE
  *-- Display Message IF There are no Records to display.
  ***There are no records to display***
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
ENDIF

*-- End Of Report & Delete Temp Files.
SET DEVICE TO SCREEN
IF USED(lcRpTmp)
  USE IN (lcRpTmp)
  ERASE(gcWorkDir+lcRpTmp+'.DBF')
  ERASE(lcRpTmp+'.CDX')
ENDIF
RETURN

*-- End Of SOFORCST.PRG Program. --*

*!**************************************************************************
*! Name      : lfGetWeek
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Get the week# of the current date and calculate 
*!             the No. of weeks in the current OGIO year
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : gdSysDate
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfGetWeek(gdSysDate)
*!**************************************************************************

FUNCTION lfGetWeek
PARAMETERS ldCurDate

lnYear = YEAR(ldCurDate)
DO WHILE .T.
  ldFDate=CTOD('01/01/'+STR(lnYear,4))   && Get the date of the first day in the year
  ldLDate=GOMONTH(ldFDate,12) - 1        && Get the date of the last day in the year

  ldFOgDate  = ldFDate-dow(ldFDate)+1   && Get the date of the first day in the OGIO year
  ldLOgDate  = ldLDate-dow(ldLDate)     && Get the date of the last day in the OGIO year

  DO CASE
    CASE BETWEEN(ldCurDate,ldFOgDate,ldLOgDate)
      EXIT
    CASE ldCurDate <  ldFOgDate
      lnYear = YEAR(ldCurDate) - 1
      *B129506,1 MMR 09/18/2002 Fix Bug of not exiting the loop in case the current year changed.
      ldFDate=CTOD('01/01/'+STR(lnYear,4))   &&Get the date of the first day in the year
      ldLDate=GOMONTH(ldFDate,12) - 1        &&Get the date of the last day in the year
      ldFOgDate  = ldFDate-dow(ldFDate)+1   &&Get the date of the first day in the OGIO year
      ldLOgDate  = ldLDate-dow(ldLDate)     &&Get the date of the last day in the OGIO year
      EXIT
      *B129506,1 MMR.[End]
    CASE ldCurDate > ldLOgDate
      lnYear = YEAR(ldCurDate) + 1
      *B129506,1 MMR 09/18/2002 Fix Bug of not exiting the loop in case the current year changed.
      ldFDate=CTOD('01/01/'+STR(lnYear,4))   &&Get the date of the first day in the year
      ldLDate=GOMONTH(ldFDate,12) - 1        &&Get the date of the last day in the year
      ldFOgDate  = ldFDate-dow(ldFDate)+1   &&Get the date of the first day in the OGIO year
      ldLOgDate  = ldLDate-dow(ldLDate)     &&Get the date of the last day in the OGIO year
      EXIT
      *B129506,1 MMR.[End]
  ENDCASE
ENDDO
lnNumWeek = (ldLOgDate - ldFOgDate + 1)/7   && Get the number of weeks of the current OGIO year
lnCurWeek  = FLOOR((ldCurDate - ldFOgDate)/7) + 1  &&Get the current week #
RETURN lnCurWeek

*-- End Of lfGetWeek.

*!**************************************************************************
*! Name      : lfvWeek
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Function to validate the week # to prevent the user from enter
*!           : week # less than the current week.
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : lnRpWeekNo
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfvWeek(lnRpWeekNo)
*!**************************************************************************

FUNCTION lfvWeek
PARAMETERS lnWeek

lnYear = YEAR(gdSysDate)
DO WHILE .T.
  ldFDate=CTOD('01/01/'+STR(lnYear,4))   &&Get the date of the first day in the year
  ldLDate=GOMONTH(ldFDate,12) - 1        &&Get the date of the last day in the year

  ldFOgDate  = ldFDate-dow(ldFDate)+1   &&Get the date of the first day in the OGIO year
  ldLOgDate  = ldLDate-dow(ldLDate)     &&Get the date of the last day in the OGIO year

  DO CASE
    CASE BETWEEN(gdSysDate,ldFOgDate,ldLOgDate)
      EXIT
    CASE gdSysDate <  ldFOgDate
      lnYear = YEAR(gdSysDate) - 1
      *B129506,1 MMR 09/18/2002 Fix Bug of not exiting the loop in case the current year changed.
      ldFDate=CTOD('01/01/'+STR(lnYear,4))   &&Get the date of the first day in the year
      ldLDate=GOMONTH(ldFDate,12) - 1        &&Get the date of the last day in the year
      ldFOgDate  = ldFDate-dow(ldFDate)+1   &&Get the date of the first day in the OGIO year
      ldLOgDate  = ldLDate-dow(ldLDate)     &&Get the date of the last day in the OGIO year
      EXIT
      *B129506,1 MMR.[End]
    CASE gdSysDate > ldLOgDate
      lnYear = YEAR(gdSysDate) + 1
      *B129506,1 MMR 09/18/2002 Fix Bug of not exiting the loop in case the current year changed.
      ldFDate=CTOD('01/01/'+STR(lnYear,4))   &&Get the date of the first day in the year
      ldLDate=GOMONTH(ldFDate,12) - 1        &&Get the date of the last day in the year
      ldFOgDate  = ldFDate-dow(ldFDate)+1   &&Get the date of the first day in the OGIO year
      ldLOgDate  = ldLDate-dow(ldLDate)     &&Get the date of the last day in the OGIO year
      EXIT
      *B129506,1 MMR.[End]
  ENDCASE
ENDDO
lnNumWeek = (ldLOgDate - ldFOgDate + 1)/7   &&Get the number of weeks of the current OGIO year
lnCurWeek  = FLOOR((gdSysDate - ldFOgDate)/7) + 1  &&Get the current week #

IF !BETWEEN(lnWeek,lnCurWeek,lnNumWeek)
  =gfModalGen(.f.,.f.,.f.,.f.,"Week# should be between week # "+ STR(lnCurWeek,2) +" and week # " + STR(lnNumWeek,2))  
  RETURN .F.
ENDIF

*-- End Of lfvWeek.
*!**************************************************************************
*! Name      : lfGetDtWk
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Get the dates for 8 consective weeks from the starting week
*!           : and the last week also get the number of weeks
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : lnRpWeekNo
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : lfGetDtWk(lnRpWeekNo)
*!**************************************************************************

FUNCTION lfGetDtWk
PARAMETERS lnWeek
PRIVATE lnCount 

FOR lnCount = 1 TO 9
  laDate(lnCount,1) = ldFOgDate + ((lnWeek-1)*7)
  IF lnCount = 1
    IF laDate(lnCount,1) < gdSysDate
      laDate(lnCount,1) = gdSysDate
    ENDIF
  ENDIF   
  laDate(lnCount,2) = ldFOgDate + ((lnWeek * 7) - 1)
  laWekYer(lnCount+1,1) = lnWeek
  laWekYer(lnCount+1,2) = lnYear
  lnWeek = lnWeek  + 1
  IF lnWeek > lnNumWeek
    lnWeek = lfGetWeek(laDate(lnCount,2) + 1)
  ENDIF
ENDFOR

*-- End Of lfGetDtWk.
*!**************************************************************************
*! Name      : lfGetDem
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Get the qty demanded for eash week/style/color/size
*!           : Demands = Total sales order + total (required - issued)
*!           : (for required - issued > 0 ) for the style as a style comp.
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : lnCount,Style
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetDem(lnCount,Style) 
*!**************************************************************************

FUNCTION lfGetDem
PARAMETERS lnWk, lcStyle 

lcAlias = ALIAS()
STORE 0 TO laDem , lnTdem
DO CASE
  CASE lnWk = 1                 &&Case for column previous
    lcOrdCond = "ORDLINE.Start < laDate(1,1)"  
    *B606822,4 ASH 12/26/2002 (Begin) Check if MF module installed.
    IF OCCURS('MF',gcCmpModules)<>0
    *B606822,4 ASH 12/26/2002 (End)
      lcCutCond = "CUTTKTH.Complete < laDate(1,1)"  
    ENDIF
    *B606822,4 ASH 12/26/2002 (Begin) Open POSLN in case of related modules installed.
    IF OCCURS('PO',gcCmpModules)<>0 OR OCCURS('MF',gcCmpModules)<>0 OR OCCURS('PS',gcCmpModules)<>0 OR OCCURS('RS',gcCmpModules)<>0
    *B606822,4 ASH 12/26/2002 (End)
      lcPoCond  = "POSHDR.Available < laDate(1,1)"  
    ENDIF
  CASE lnWk = 10                &&Case for the last week
    lcOrdCond = "ORDLINE.Start > laDate(9,1) "
    *B606822,4 ASH 12/26/2002 (Begin) Check if MF module installed.
    IF OCCURS('MF',gcCmpModules)<>0
    *B606822,4 ASH 12/26/2002 (End)
      lcCutCond = "CUTTKTH.Complete > laDate(9,1)"
    ENDIF
    *B606822,4 ASH 12/26/2002 (Begin) Open POSLN in case of related modules installed.
    IF OCCURS('PO',gcCmpModules)<>0 OR OCCURS('MF',gcCmpModules)<>0 OR OCCURS('PS',gcCmpModules)<>0 OR OCCURS('RS',gcCmpModules)<>0
    *B606822,4 ASH 12/26/2002 (End)
      lcPoCond  = "POSHDR.Available > laDate(9,1)"
    ENDIF
  OTHERWISE
    lnNoWeek = lnNoWeek + 1
    lcOrdCond = "BETWEEN(ORDLINE.Start,laDate(lnNoWeek,1),laDate(lnNoWeek,2))"
    *B606822,4 ASH 12/26/2002 (Begin) Check if MF module installed.
    IF OCCURS('MF',gcCmpModules)<>0
    *B606822,4 ASH 12/26/2002 (End)
      lcCutCond = "BETWEEN(CUTTKTH.Complete,laDate(lnNoWeek,1),laDate(lnNoWeek,2))"
    ENDIF
    IF OCCURS('PO',gcCmpModules)<>0
      lcPoCond  = "BETWEEN(POSHDR.Available,laDate(lnNoWeek,1),laDate(lnNoWeek,2))"
    ENDIF
ENDCASE  
IF SEEK(lcStyle,"ORDLINE")
  SELECT ORDLINE
  SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8 ;
      WHILE STYLE = lcStyle ;
      FOR ORDHDR.Status $ "OH" AND &lcOrdCond TO ARRAY laDem
ENDIF

IF !EMPTY(lcMTyp) .OR. !EMPTY(lcITyp)
  =lfStyComp()
ENDIF
FOR lnNum = 1 TO 8
  lnTdem = lnTdem + laDem(lnNum)
ENDFOR
SELECT (lcAlias)

*-- End Of lfGetDem.

*!**************************************************************************
*! Name      : lfGetForc
*! Developer : Rania Abdel Razik (RAE) 
*! Date      : 05/23/2002
*! Purpose   : Get the forecasted qty for eash week/style/color/size
*!           : Forecasted qty = forecast qty from the foercast file - demand  
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : lnCount,Style
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetForc(lnCount,Style) 
*!**************************************************************************

FUNCTION lfGetForc
PARAMETERS lnWk, lcStyle
STORE 0 TO laForc, lnTforc

SELECT FORCAST
DO CASE
  CASE lnWk = 1                 &&Case for column previous
    RETURN
  CASE lnWk = 10                &&Last column 
    IF SEEK(lcStyle)
      SUM nForQTY1,nForQTY2,nForQTY3,nForQTY4,nForQTY5,nForQTY6,nForQTY7,nForQTY8 ;
          WHILE style+STR(nyear,4)+STR(nweek,2)=;
                lcStyle ;
          FOR IIF(lnYear = nYear ,nWeek > laWekYer(9,1),.T.) AND nYear >= laWekYer(9,2);
          TO ARRAY laForc
    ENDIF    
    FOR lnElmn1 = 1 TO 8
      lcElmn1 = STR(lnElmn1,1)
      laForc(lnElmn1) = laForc(lnElmn1) - laDem(lnElmn1)
    ENDFOR 
  OTHERWISE
    = SEEK(lcStyle + STR(laWekYer(lnWk,2),4) + STR(laWekYer(lnWk,1),2))
    FOR lnElmn1 = 1 TO 8
      lcElmn1 = STR(lnElmn1,1)
      laForc(lnElmn1) = nForQTY&lcElmn1  - laDem(lnElmn1)
    ENDFOR 
ENDCASE
FOR lnNum = 1 TO 8
  lnTforc = lnTforc + laForc(lnNum)
ENDFOR

SELECT STYLE
*-- End Of lfGetForc.

*!**************************************************************************
*! Name      : lfStyComp
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Get the total required - issued for the style as a style 
*!           : component in another style
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfStyComp()
*!**************************************************************************

FUNCTION lfStyComp

STORE SPACE(12) TO lcSty 
STORE SPACE(6) TO lcPo
DIMENSION laPo(1),laSty(1)
STORE " " TO laSty
STORE 1 TO lnElm,lnElm1 
*B606822,4 ASH 12/26/2002 (Begin) Check if MF module installed.
IF OCCURS('MF',gcCmpModules)<>0
*B606822,4 ASH 12/26/2002 (End)
  SELECT CUTTKTL
  SET RELATION TO 
  SELECT CUTTKTH
  SET ORDER TO TAG CUTTKTHS
ENDIF  
SELECT BOM

*-- Search for the selected style as a style Component in another manifactured style
IF !EMPTY(lcMTyp)
  FOR lnNumTyp = 1 to ALEN(lcMTyp)
    DIMENSION laSty(1)
    STORE " " TO laSty
    STORE 1 TO lnElm 
    IF SEEK(lcMTyp(lnNumTyp)+LEFT(lcStyle,lnMajWidth))  &&lcMTyp is a type of style component for manifatured style
      SCAN WHILE Typ + LEFT(Item,lnMajWidth) = lcMTyp(lnNumTyp) + LEFT(lcStyle,lnMajWidth)
        IF ASCAN(laSty,Style.style) != 0
          LOOP
        ENDIF
        lcSty = Style.Style
        laSty(lnElm) = Style.Style
        lnElm = lnElm + 1
        DIMENSION laSty(lnElm)
        *B606822,4 ASH 12/26/2002 (Begin) Check if MF module installed.
        IF OCCURS('MF',gcCmpModules)<>0
        *B606822,4 ASH 12/26/2002 (End)
          IF SEEK(LEFT(lcSty,lnMajWidth),"CUTTKTH")
            SELECT CUTTKTH
            SCAN WHILE Style + Cuttkt = LEFT(lcSty,lnMajWidth)+SPACE(lnSpcReq) FOR Status $ "OA" ;
                 AND &lcCutCond     
              IF SEEK("M"+Cuttkt+lcMTyp(lnNumTyp),"CTKTBOM")
                STORE 0 TO laSComp
                lcCuttkt = Cuttkt
                SELECT CTKTBOM
                SUM (Req_qty1 - Iss_qty1) , (Req_qty2 - Iss_qty2),;
                    (Req_qty3 - Iss_qty3) , (Req_qty4 - Iss_qty4),;
                    (Req_qty5 - Iss_qty5) , (Req_qty6 - Iss_qty6),;
                    (Req_qty7 - Iss_qty7) , (Req_qty8 - Iss_qty8); 
                WHILE CIMTYP+CUTTKT +Typ = "M" + lcCuttkt + lcMTyp(lnNumTyp);
                FOR LEFT(Item,lnMajWidth) = LEFT(lcStyle,lnMajWidth);
                TO ARRAY laSComp
                FOR lnCnt = 1 TO 8
                  laDem(lnCnt) = laDem(lnCnt) + IIF(laSComp(lnCnt) > 0 , laSComp(lnCnt) , 0 )
                ENDFOR
              ENDIF
            ENDSCAN
          ENDIF 
        ENDIF
        SELECT BOM
      ENDSCAN
    ENDIF
  ENDFOR
ENDIF
SELECT BOM
*-- Search for the selected style as a style component in another imported style
IF !EMPTY(lcITyp)
  FOR lnNumTyp = 1 to ALEN(lcITyp)
    DIMENSION laSty(1)
    STORE " " TO laSty
    STORE 1 TO lnElm , lnElm1
    DIMENSION laPo(1)
    STORE " " TO laPo
    IF SEEK(lcITyp(lnNumTyp)+LEFT(lcStyle,lnMajWidth))  &&lcITyp is a type of style component for imported style
      SCAN WHILE Typ + LEFT(Item,lnMajWidth) = lcMTyp(lnNumTyp) + LEFT(lcStyle,lnMajWidth)
        IF ASCAN(laSty,Style) != 0
          LOOP
        ENDIF
        lcSty = Style.Style
        laSty(lnElm) = Style.Style
        lnElm = lnElm + 1
        DIMENSION laSty(lnElm)
        IF OCCURS('PO',gcCmpModules)<>0
          IF SEEK(lcSty,"POSLN")
            SELECT POSLN
            SCAN WHILE Style + Po= lcSty FOR POSHDR.Status = "O";
                       AND &lcPoCond.     
              IF ASCAN(laPo,Po) != 0
                LOOP
              ENDIF
              laPo(lnElm1) = Po
              lnElm1 = lnElm1 + 1
              DIMENSION laPo(lnElm1)
              IF SEEK("I"+Po+lcITyp(lnNumTyp),"CTKTBOM")
                STORE 0 TO laSComp
                lcCuttkt = Po
                SELECT CTKTBOM
                SUM (Req_qty1 - Iss_qty1) , (Req_qty2 - Iss_qty2),;
                    (Req_qty3 - Iss_qty3) , (Req_qty4 - Iss_qty4),;
                    (Req_qty5 - Iss_qty5) , (Req_qty6 - Iss_qty6),;
                    (Req_qty7 - Iss_qty7) , (Req_qty8 - Iss_qty8); 
                WHILE CIMTYP+CUTTKT +Typ = "I" + lcCuttkt + lcITyp(lnNumTyp);
                FOR LEFT(Item,lnMajWidth) = LEFT(lcStyle,lnMajWidth);
                TO ARRAY laSComp
                FOR lnCnt = 1 TO 8
                  laDem(lnCnt) = laDem(lnCnt) + IIF(laSComp(lnCnt) > 0 , laSComp(lnCnt) , 0 )
                ENDFOR
              ENDIF
            ENDSCAN
          ENDIF 
        ENDIF
        SELECT BOM
      ENDSCAN
    ENDIF
  ENDFOR
ENDIF
*-- End Of lfStyComp.

*!**************************************************************************
*! Name      : lfGetHand
*! Developer : Rania Abdel Razik (RAE) 
*! Date      : 05/23/2002
*! Purpose   : Get the On hand qty for eash week/style/color/size
*!           : On Hand = available qty of the previous column
*!           : for Previous column the on hand will be the current stock
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : lnCount
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetHand(lnCount) 
*!**************************************************************************

FUNCTION lfGetHand
PARAMETERS lnWk

STORE 0 TO lnTHand
IF lnWk = 1                 &&Case for column previous
  SELECT STYLE 
  FOR lnEmn2 = 1 TO 8 
    lcElmn2 = STR(lnEmn2,1)  
    laHand(lnEmn2) = Stk&lcElmn2
  ENDFOR
ELSE
  FOR lnEmn2 = 1 TO 8 
    laHand(lnEmn2) = laAvail(lnEmn2)
  ENDFOR
ENDIF
FOR lnNum = 1 TO 8
  lnTHand = lnTHand + laHand(lnNum)
ENDFOR
*-- End Of lfGetHand.

*!**************************************************************************
*! Name      : lfGetWip
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Get the WIP for eash week/style/color
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : lnCount,Style
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetWip(lnCount,Style) 
*!**************************************************************************

FUNCTION lfGetWip
PARAMETERS lnWk , lcStyle

STORE 0 TO laTrans,lnTTrans
IF STYLE.Make
  *B606822,4 ASH 12/26/2002 (Begin) Check if MF module installed.
  IF OCCURS('MF',gcCmpModules)<>0
  *B606822,4 ASH 12/26/2002 (End)
    SELECT CUTTKTH
    SET ORDER TO TAG CUTTKTH
    SELECT CUTTKTL
    *B606822,4 ASH 12/26/2002 (Begin) Remove the relation because it was setted before.
    *SET RELATION TO CUTTKTL.Cuttkt INTO CUTTKTH ADDITIVE
    *B606822,4 ASH 12/26/2002 (End)
    IF SEEK(lcStyle)
      SCAN WHILE STYLE+CUTTKT+TRANCD+STR(RECNO(),7)=;
                 lcStyle FOR CUTTKTH.Status $ "OHA"; 
                 AND &lcCutCond      
        IF Trancd = "1"  
          FOR lnQty = 1 TO 8       
            lcQty = STR(lnQty,1)
            laTrans(lnQty) = laTrans(lnQty) + Qty&lcQty        
          ENDFOR
        ELSE
          FOR lnQty = 1 TO 8       
            lcQty = STR(lnQty,1)
            laTrans(lnQty) = laTrans(lnQty) - Qty&lcQty        
          ENDFOR
        ENDIF 
      ENDSCAN
    ENDIF
  ENDIF
ELSE  
  *B606822,4 ASH 12/26/2002 (Begin) Open POSLN in case of related modules installed.
  IF OCCURS('PO',gcCmpModules)<>0 OR OCCURS('MF',gcCmpModules)<>0 OR OCCURS('PS',gcCmpModules)<>0 OR OCCURS('RS',gcCmpModules)<>0
  *B606822,4 ASH 12/26/2002 (End)
    SELECT POSLN
    IF SEEK(lcStyle)
      SCAN WHILE STYLE+PO+STR(LINENO,6)+TRANCD+STR(RECNO(),7)=;
                 lcStyle FOR POSHDR.Status $ "OH" ;
                 AND &lcPoCond      
        IF Trancd = "1"  
          FOR lnQty = 1 TO 8       
            lcQty = STR(lnQty,1)
            laTrans(lnQty) = laTrans(lnQty) + Qty&lcQty        
          ENDFOR
        ELSE
          FOR lnQty = 1 TO 8       
            lcQty = STR(lnQty,1)
            laTrans(lnQty) = laTrans(lnQty) - Qty&lcQty        
          ENDFOR
        ENDIF 
      ENDSCAN
    ENDIF
  ENDIF
ENDIF
FOR lnNum = 1 TO 8
  lnTTrans = lnTTrans + laTrans(lnNum)
ENDFOR
SELECT STYLE

*-- End Of lfGetWip.

*!**************************************************************************
*! Name      : lfGetAvail
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Get the available qty for eash week/style/color
*!           : Available = (In transit + On hand) - 
*!           : (Demand + calculated forecasted qty if greater than 0)
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : lnCount,Style
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetAvail(lnCount,Style)
*!**************************************************************************

FUNCTION lfGetAvail
PARAMETERS lnWk , lcStyle

STORE 0 TO laAvail,lnTAvail
FOR lnNum = 1 TO 8       
  laAvail(lnNum) = (laTrans(lnNum) + laHand(lnNum)) - ;
                   (laDem(lnNum) + IIF(laForc(lnNum) > 0 ,laForc(lnNum),0))
  lnTAvail = lnTAvail + laAvail(lnNum)
ENDFOR

*-- End Of lfGetAvail.
*!**************************************************************************
*! Name      : lfPrnRep
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Function to print the report 
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : lnSize
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfPrnRep(lnSizes)
*!**************************************************************************

FUNCTION lfPrnRep
PARAMETERS lnSize
                                   
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*Style: 123456789123                            Size : 12345

SELECT &lcRpTmp
FOR lnCount = 1 TO lnSize
  IF lcRpNegAll = "N"
    IF !laSizes(lnCount)
      LOOP 
    ENDIF
  ENDIF
  =lfNewpag()
  llZeroAll = .T.
  =lfGetLine(lnCount)
  IF llZeroAll
    LOOP
  ENDIF
  llPrint = .T.  
  lnRow = lnRow + 1 
  WAIT WINDOW 'Printing ' + lcStyTit +': ' + Style NOWAIT
  @ lnRow,0 SAY "Style: " + Style 
  IF lcRpByST = "S"
    lcCount = STR(lnCount,1)
    @ lnRow,48 SAY "Size: " + SCALE.Sz&lcCount   
  ENDIF
  lnRow = lnRow + 1 
  =lfNewpag()
  @ lnRow,0 SAY "Style description: "  + STYLE.Desc
  lnRow = lnRow + 1 
  @ lnRow,00 SAY REPLICATE('-',132)
  lnRow = lnRow + 1 
  =lfNewpag()
  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
  *Week n#  Previous  /     11    /     12    /     13    /     14    /     15    /     16    /     17    /     18    /     19+    
  *Date                 dd/dd/dd    dd/dd/dd    dd/dd/dd    dd/dd/dd     dd/dd/dd    dd/dd/dd    dd/dd/dd    dd/dd/dd    dd/dd/dd
  *------------------------------------------------------------------------------------------------------------------------------------
  *Forecast   123456      123456      123456      123456      123456       123456      123456      123456      123456      123456
  *Demands
  @ lnRow,0 SAY "Week n#"
  @ lnRow,10 SAY "Previous"
  lnPos = 26
  @ lnRow,26 SAY STR(laWekYer(2,1),2)+SPACE(4)+"/" +SPACE(5) +;
                 STR(laWekYer(3,1),2)+SPACE(4)+"/" +SPACE(5) +;
                 STR(laWekYer(4,1),2)+SPACE(4)+"/" +SPACE(5) +;
                 STR(laWekYer(5,1),2)+SPACE(4)+"/" +SPACE(5) +;
                 STR(laWekYer(6,1),2)+SPACE(4)+"/" +SPACE(5) +;
                 STR(laWekYer(7,1),2)+SPACE(4)+"/" +SPACE(5) +;
                 STR(laWekYer(8,1),2)+SPACE(4)+"/" +SPACE(5) +;
                 STR(laWekYer(9,1),2)+SPACE(4)+"/" +SPACE(5) +;
                 STR(laWekYer(10,1),2)+"+"
  
  lnRow = lnRow + 1 
  =lfNewpag()
  @ lnRow ,0 SAY "Date"
  lnPos = 22
  FOR N = 1 TO 9
    @ lnRow,lnPos SAY laDate(N,1)
    lnPos = lnPos +12
  ENDFOR
  lnRow = lnRow + 1  
  =lfNewpag()
  @ lnRow,00 SAY REPLICATE('-',132)
  lnRow = lnRow + 1  
  
  @ lnRow,0 SAY "Forecast"
  lnPos = 12
  FOR lnEmnt = 1 TO 10
    @ lnRow,lnPos  SAY laForcWk(lnEmnt) PICTURE "999999"
    lnPos = lnPos + 12
  ENDFOR
  lnRow = lnRow + 1  
  =lfNewpag()
  
  @ lnRow,0 SAY "Demand"
  lnPos = 12
  FOR lnEmnt = 1 TO 10
    @ lnRow,lnPos SAY laDemWk(lnEmnt) PICTURE "999999"
    lnPos = lnPos +12
  ENDFOR
  lnRow = lnRow + 1  
  =lfNewpag()
  
  @ lnRow,0 SAY "Onhand"
  lnPos = 12
  FOR lnEmnt = 1 TO 10
    @ lnRow,lnPos SAY laHandWk(lnEmnt) PICTURE "999999"
    lnPos = lnPos +12
  ENDFOR
  lnRow = lnRow + 1  
  =lfNewpag()
  
  @ lnRow,0 SAY "In-transit"
  lnPos = 12
  FOR lnEmnt = 1 TO 10
    @ lnRow,lnPos SAY laTransWk(lnEmnt) PICTURE "999999"
    lnPos = lnPos +12
  ENDFOR
  lnRow = lnRow + 1  
  =lfNewpag()
  
  @ lnRow,0 SAY "Available"
  lnPos = 12
  FOR lnEmnt = 1 TO 10
    @ lnRow,lnPos SAY laAvailWk(lnEmnt) PICTURE "999999"
    lnPos = lnPos +12
  ENDFOR
  lnRow = lnRow + 1  
  @ lnRow,00 SAY REPLICATE('-',132)
  lnRow = lnRow + 1  
ENDFOR

*-- End Of lfPrnRep.

*!**************************************************************************
*! Name      : lfGetLine
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Function to get line to print
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : lnCount
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetLine(lnCount)
*!**************************************************************************

FUNCTION lfGetLine
PARAMETERS lnCnt

lnRecNo = RECNO()
lcCnt = STR(lnCnt,1)
STORE 0 TO lnCont
SCAN WHILE Style = STYLE.Style
  lnCont = lnCont + 1
  laForcWk(lnCont)  = IIF(lcRpByST = "S",FORCAST&lcCnt,TOTFORC)
  laDemWk(lnCont)   = IIF(lcRpByST = "S",DEMAND&lcCnt,TOTDEM)
  laHandWk(lnCont)  = IIF(lcRpByST = "S",ONHAND&lcCnt,TOTHAND)
  laTransWk(lnCont) = IIF(lcRpByST = "S",TRANS&lcCnt,TOTTRANS)
  laAvailWk(lnCont) = IIF(lcRpByST = "S",AVAIL&lcCnt,TOTAV)
  IF llZeroAll AND (laForcWk(lnCont) != 0 OR laDemWk(lnCont) != 0 OR ;
    laHandWk(lnCont) != 0 OR laTransWk(lnCont) != 0 OR laAvailWk(lnCont) != 0 )
    llZeroAll = .F.
  ENDIF
ENDSCAN
GO lnRecNo

*-- End Of lfGetLine.

*!**************************************************************************
*! Name      : lfNewpag
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Function to determine if we print the new page or not
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfNewpag()
*!**************************************************************************

FUNCTION lfNewpag

IF lnRow >= lnMaxRow
  PAGENO = PAGENO+1
  DO RPT_HDR WITH XREPORT,lcRpOpTlt,R_WIDTH
  lnRow = 5
ENDIF
*-- End Of lfNewpag.

*!**************************************************************************
*! Name      : lfGetNeg
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002 
*! Purpose   : Check it the user select to print negative available only 
*!           : in order to print the style/color/size  which has negative 
*!           : available in in one of the first 9 columns.
*!**************************************************************************
*! Calls :
*!**************************************************************************
*! Passed Parameters  : 
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetNeg()
*!**************************************************************************

FUNCTION lfGetNeg

llFound = .F.
FOR lnNeg = 1 TO SCALE.Cnt
  lnWekCount = 0
  SCAN WHILE Style = STYLE.Style AND lnWekCount < 9
    lnWekCount = lnWekCount + 1
    lcNeg = STR(lnNeg,1)
    IF AVAIL&lcNeg < 0
      IF lcRpByST = "S"
        laSizes(lnNeg) = .T.   
      ELSE
        laSizes(1) = .T.
        llFound = .T.
      ENDIF  
      EXIT
    ENDIF
  ENDSCAN
  IF llFound
    EXIT
  ENDIF
  =SEEK(STYLE.Style)
ENDFOR
=SEEK(STYLE.Style)

*-- End Of lfGetNeg.
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************

FUNCTION lfSRVSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE

*-- End Of lfsrvSty.

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
*
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty

DO CASE
  CASE lcParm = 'S'  && Set code
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR Fabric = Fabric.Fabric AND ;
             !MAKE AND lDetCost AND STATUS = 'A'
    llHaveSty = FOUND()
    IF !llHaveSty
      = gfModalGen("INM34122B36000","Dialog",ALLTRIM(Fabric.Fabric))
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE

*-- End Of lfSRVFab.
*!*************************************************************
*! Name      : lfStySum
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
*
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec

lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)

*-- End Of lfStySum.
*!*************************************************************
*! Name      : lfEvalSeg
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : To evaluate the non-major segment.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSeg()
*!*************************************************************
*
FUNCTION lfEvalSeg
PRIVATE lnClrPos
DIMENSION laMajSeg[1,1]

= gfItemMask(@laMajSeg)
lnClrPos   = INT(ASCAN(laMajSeg,'C')/7+0.9)
lnStartPos = laMajSeg[lnClrPos,4]
lnNonMajLn = LEN(laMajSeg[lnClrPos,3])

*-- End Of lfEvalSeg.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

*E301894,3 change the customer status to the style status.
*DECLARE laRpSource[4],laRpTarget[1]
DECLARE laRpSource[3],laRpTarget[1]
*E301894,3
lnStatus = lcRpStatus
STORE 'Active'    TO laRpSource[1] 
STORE ''          TO laRpTarget[1]
STORE 'On Hold'   TO laRpSource[2] &&,laRpTarget[2]
*E301894,3
*STORE 'Potential' TO laRpSource[3]
*STORE 'Cancelled'  TO laRpSource[4]

STORE 'Cancelled' TO laRpSource[3]
*E301894,3
lcRpStatus = 'AH'

*- End OF lfwRepWhen.
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/23/2002
*! Purpose   : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOStatus()
*!*************************************************************
*
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  *E301894,3 change the customer status to the style status.
  *lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Active','A',;
                            IIF(laRpTarget[lnI] = 'Hold','H',;
                            IIF(laRpTarget[lnI] = 'Potential','P',;
                            IIF(laRpTarget[lnI] = 'Canceled','X',''))))
  
  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Active','A',;
                            IIF(laRpTarget[lnI] = 'On Hold','H',;
                            IIF(laRpTarget[lnI] = 'Cancelled','X','')))
  *E301894,3
ENDFOR  && end Loop to make Status expression.

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- End of lfvOStatus.
*!*************************************************************