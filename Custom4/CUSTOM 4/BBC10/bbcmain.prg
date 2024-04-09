*!**************************************************************************
*! Name      : BBCMAIN.PRG
*! Developer : Hesham Elmasry
*! Date      : 06/16/2009
*! Purpose   : Custom triggers for BBC customer 
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Modifications:
*! C201974,1 MMT 03/23/2017 Add Triggers to Packing list screens for Packing information[T20161021.0004]
*! B611916,1 MMT 06/27/2021 Enhance Manual Packing list at BBC[T20210329.0003]
*! C202429,1 MMT 09/09/2021 Add triggers to be called from Key off screen to update the EBREMITT and EBPAYMT[P20210827.0001]
*! B611924,1 MMT 02/15/2022 fix the issue of duplicated adjustment record while keyoff[T20220207.0002]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name       : lfFilFlds
*! Developer  : Hesham Elmasry (HES)
*! Date       : 06/15/2009
*! Purpose    : Fill the Two fields dOrgCmDate and dModCmDate with the POSHDR.Complete
*! Type       : Custom
*!**************************************************************************
FUNCTION lfFilFlds

SELECT(loFormSet.ariaForm1.mainworkorder.cPosHdr)
Replace dOrgCmDate with Complete ;
        dModCmDate with Complete 

* End of lfFilFlds

*!**************************************************************************
*! Name       : lfSvCurCmD
*! Developer  : Hesham Elmasry (HES)
*! Date       : 06/15/2009
*! Purpose    : Save the current COMPLETE date for the PO # in a formset property
*! Type       : Custom
*!**************************************************************************
FUNCTION lfSvCurCmD

IF TYPE('loFormSet.ldCurCmDat') = 'U' 
  loFormSet.AddProperty('ldCurCmDat',{})
ENDIF 

SELECT(loFormSet.ariaForm1.mainworkorder.cPosHdr)
loFormSet.ldCurCmDat = Complete

* End of lfSvCurCmD

*!**************************************************************************
*! Name       : lfVldCmDat
*! Developer  : Hesham Elmasry (HES)
*! Date       : 06/15/2009
*! Purpose    : Check if the Complete date modified or not to update the dModCmDate field
*! Type       : Custom
*!**************************************************************************
FUNCTION lfVldCmDat

LOCAL ldOldCmDate, ldNewCmDate

ldOldCmDate = loFormSet.ldCurCmDat
ldNewCmDate = loFormSet.AriaForm1.PgfPOStyle.page1.cntheaderFolder.DtPickerComplete.Text1.Value 

IF ldOldCmDate <> ldNewCmDate 
  SELECT(loFormSet.ariaForm1.mainworkorder.cPosHdr)
  REPLACE dModCmDate WITH ldOldCmDate
ENDIF 
* End of lfVldCmDat
*! C201974,1 MMT 03/23/2017 Add Triggers to Packing list screens for Packing information[T20161021.0004][Start]
*!**************************************************************************
*! Name       : lfADPKINFOP
*! Developer  : Mariam Mazhar - MMT
*! Date       : 03/23/2017
*! Purpose    : Add Option for Packing info. in Auto. PL 
*! Type       : Custom
*!**************************************************************************
FUNCTION lfADPKINFOP

lnBarNo = CNTBAR('_OPTIONPOP') + 1
lcHostFormName = '[' + loFormSet.cHostFormName + ']'
*&&>>> - make sure that these are called only in Add mode
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "Packing Information" ;
       SKIP FOR gfFormIsActive(&lcHostFormName) .AND. _screen.ActiveForm.Parent.ActiveMode <> "A"
*! B611916,1 MMT 06/27/2021 Enhance Manual Packing list at BBC[T20210329.0003][Start]
*ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfDispPackScr IN BBCMAIN.fxp
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfDispPackScr 
*! B611916,1 MMT 06/27/2021 Enhance Manual Packing list at BBC[T20210329.0003][End]
*!**************************************************************************
*! Name       : lfDispPackScr 
*! Developer  : Mariam Mazhar - MMT
*! Date       : 03/23/2017
*! Purpose    : Displaying Packing info.
*! Type       : Custom
*!**************************************************************************
FUNCTION lfDispPackScr 

loMainFormSet = _screen.ActiveForm.Parent
lnDataSession = SET("Datasession")
lcPakLin   = loMainFormSet.AriaForm1.oformClass.lcPakLin
IF !USED(lcPakLin)
  SET DATASESSION TO loMainFormSet.DataSessionId
ENDIF
SELECT (lcPakLin)
lcSetKeyPack = SET("Key") 
SET KEY TO
IF TYPE("&lcPakLin..CSTYLEPO") = 'U'
  SELECT (lcPakLin)
  lcRelations = SET("Relation") 
  lcIndex = ORDER()
  lnRecNum = RECNO()
  loMainFormSet.AriaForm1.Ariapageframe1.AriaPage3.grdPckDetail.RecordSource = ""
  ALTER Table (oAriaApplication.workdir+lcPakLin) ADD COLUMN NCASEPACK N(9)
  ALTER Table (oAriaApplication.workdir+lcPakLin) ADD COLUMN CSTYLEPO C(30)
  ALTER Table (oAriaApplication.workdir+lcPakLin) ADD COLUMN CPACKBY C(1)
  SELECT (lcPakLin)
  IF !EMPTY(lcRelations)
    SET RELATION TO &lcRelations.
  ENDIF
  IF !EMPTY(lcIndex)
    SET ORDER TO &lcIndex.
  ENDIF  
  IF BETWEEN(lnRecNum,1,RECCOUNT())
    GO RECORD lnRecNum  
  ENDIF
  WITH loMainFormSet.AriaForm1.Ariapageframe1.AriaPage3.grdPckDetail
	  .RecordSource = lcPakLin
	  .column1.ControlSource  = lcPakLin+ '.From_Crt' 
	  .column30.ControlSource  = lcPakLin+ '.To_Crt' 
	  .column2.ControlSource  = lcPakLin+ '.Style' 
	  .Column2.header1.Caption = loMainFormSet.AriaForm1.ariapageframe1.ariapage3.keyItem.lcimjrheader 
	  .column3.ControlSource  = lcPakLin+ '.Qty1' 
	  .column4.ControlSource  = lcPakLin+ '.Qty2' 
	  .column5.ControlSource  = lcPakLin+ '.Qty3' 
	  .column6.ControlSource  = lcPakLin+ '.Qty4' 
	  .column7.ControlSource  = lcPakLin+ '.Qty5' 
	  .column8.ControlSource  = lcPakLin+ '.Qty6' 
	  .column9.ControlSource  = lcPakLin+ '.Qty7' 
	  .column10.ControlSource  = lcPakLin+ '.Qty8' 
	  .column11.ControlSource  = lcPakLin+ '.TotQty' 
	  .column12.ControlSource  = lcPakLin+ '.OrgOrd1' 
	  .column13.ControlSource  = lcPakLin+ '.OrgOrd2' 
	  .column14.ControlSource  = lcPakLin+ '.OrgOrd3' 
	  .column15.ControlSource  = lcPakLin+ '.OrgOrd4' 
	  .column16.ControlSource  = lcPakLin+ '.OrgOrd5' 
	  .column17.ControlSource  = lcPakLin+ '.OrgOrd6' 
	  .column18.ControlSource  = lcPakLin+ '.OrgOrd7' 
	  .column19.ControlSource  = lcPakLin+ '.OrgOrd8' 
	  .column20.ControlSource  = lcPakLin+ '.TotOrgOrd' 
	  .column21.ControlSource  = lcPakLin+ '.Pik1' 
	  .column22.ControlSource  = lcPakLin+ '.Pik2' 
	  .column23.ControlSource  = lcPakLin+ '.Pik3' 
	  .column24.ControlSource  = lcPakLin+ '.Pik4' 
	  .column25.ControlSource  = lcPakLin+ '.Pik5' 
	  .column26.ControlSource  = lcPakLin+ '.Pik6' 
	  .column27.ControlSource  = lcPakLin+ '.Pik7' 
	  .column28.ControlSource  = lcPakLin+ '.Pik8' 
	  .column29.ControlSource  = lcPakLin+ '.TotPik'
	  .SetAll('READONLY',.T.,'Column')
  ENDWITH
ENDIF

IF TYPE('loMainFormSet.lcStyTmp') ='U' 
  loMainFormSet.AddProperty("lcStyTmp",gfTempName())
  CREATE CURSOR (loMainFormSet.lcStyTmp) (Style C(19),lSelect L,lUsePrePk L)
  SELECT (loMainFormSet.lcStyTmp) 
  INDEX ON STYLE TAG (loMainFormSet.lcStyTmp)
ELSE
  SELECT (loMainFormSet.lcStyTmp) 
  ZAP
ENDIF
IF TYPE('loMainFormSet.lcDetTmp') ='U' 
  loMainFormSet.AddProperty("lcDetTmp",gfTempName())
  CREATE CURSOR (loMainFormSet.lcDetTmp) (lSelect L,Style C(19),Pack_No C(6),NCASEPACK N(9),CSTYLEPO C(30),CPACKBY C(1))
  SELECT (loMainFormSet.lcDetTmp)
  INDEX ON Style+Pack_No TAG (loMainFormSet.lcDetTmp)
ELSE
  SELECT (loMainFormSet.lcDetTmp)
  SET KEY TO 
  ZAP
ENDIF
SELECT (lcPakLin)
lnRecNum = RECNO()
LOCATE
SCAN
 IF !SEEK(&lcPakLin..STYLE,loMainFormSet.lcStyTmp)
   INSERT INTO (loMainFormSet.lcStyTmp) VALUES (&lcPakLin..STYLE,.F.,!EMPTY(ALLTRIM(&lcPakLin..prepak)))
 ELSE
   REPLACE lUsePrePk WITH (lUsePrePk OR !EMPTY(ALLTRIM(&lcPakLin..prepak))) IN (loMainFormSet.lcStyTmp)
 ENDIF 
 IF !SEEK(&lcPakLin..STYLE+&lcPakLin..PACK_NO,loMainFormSet.lcDetTmp)
   INSERT INTO (loMainFormSet.lcDetTmp) VALUES (.T.,&lcPakLin..STYLE,&lcPakLin..PACK_NO,&lcPakLin..NCASEPACK,&lcPakLin..CSTYLEPO,&lcPakLin..CPACKBY)
 ENDIF 
ENDSCAN
SELECT (loMainFormSet.lcStyTmp)
LOCATE
*=GFCALLFORM("arcdiv",'',"'"+lcAccount+"','"+loFormSet.activemode+"'")
=GFCALLFORM("ALPCKINF",'AL',".F.,'"+loMainFormSet.lcStyTmp+"','"+loMainFormSet.lcDetTmp+"','"+lcPakLin+"'")

SELECT (lcPakLin)
IF !EMPTY(lcSetKeyPack)
  SET KEY TO (lcSetKeyPack)
ENDIF  
IF BETWEEN(lnRecNum,1,RECCOUNT())
  GO RECORD lnRecNum  
ENDIF
SET DATASESSION TO &lnDataSession.
*!**************************************************************************
*! Name       : lfADPKINFOPM
*! Developer  : Mariam Mazhar - MMT
*! Date       : 03/23/2017
*! Purpose    : Add Packing info. option to Manual PL screen
*! Type       : Custom
*!**************************************************************************
FUNCTION lfADPKINFOPM
lnBarNo = CNTBAR('_OPTPOP') + 1
lcHostFormName = '[' + loFormSet.cHostFormName + ']'
*&&>>> - make sure that these are called only in Add mode
DEFINE BAR lnBarNo OF _OPTPOP PROMPT "Packing Information" ;
       SKIP FOR gfFormIsActive(&lcHostFormName) .AND. _SCREEN.ACTIVEFORM.PARENT.ActiveMode='S'
*! B611916,1 MMT 06/27/2021 Enhance Manual Packing list at BBC[T20210329.0003][Start]       
*ON SELECTION BAR lnBarNo OF _OPTPOP DO lfDispPackScrM IN BBCMAIN.fxp
ON SELECTION BAR lnBarNo OF _OPTPOP DO lfDispPackScrM 
*! B611916,1 MMT 06/27/2021 Enhance Manual Packing list at BBC[T20210329.0003][End]
*!**************************************************************************
*! Name       : lfDispPackScrM
*! Developer  : Mariam Mazhar - MMT
*! Date       : 03/23/2017
*! Purpose    : Display Packing info. screen from Manual PL screen
*! Type       : Custom
*!**************************************************************************
FUNCTION lfDispPackScrM
#INCLUDE D:\ARIA4XP\PRGS\ALPLIST.h
loMainFormSet = _screen.ActiveForm.Parent
lnDataSession = SET("Datasession")
lcPakLin   = loMainFormSet.lcPckLin
IF !USED(lcPakLin)
  SET DATASESSION TO loMainFormSet.DataSessionId
ENDIF
SELECT(lcPakLin)
lcSetKeyPack = SET("Key") 
lcSetFilter = SET("Filter") 
SET KEY TO
SET FILTER TO 
IF TYPE("&lcPakLin..CSTYLEPO") = 'U'
  SELECT (lcPakLin)
  lcRelations = SET("Relation") 
  lcIndex = ORDER()
  lnRecNum = RECNO()
  IF loMainFormSet.llShoPckSu
    loMainFormSet.AriaForm1.pgfPacking.Detail.grdDetail.RecordSource = ""
  ENDIF  
  ALTER Table (oAriaApplication.workdir+lcPakLin) ADD COLUMN NCASEPACK N(9)
  ALTER Table (oAriaApplication.workdir+lcPakLin) ADD COLUMN CSTYLEPO C(30)
  ALTER Table (oAriaApplication.workdir+lcPakLin) ADD COLUMN CPACKBY C(1)
  SELECT (lcPakLin)
  IF !EMPTY(lcRelations)
    SET RELATION TO &lcRelations.
  ENDIF
  IF !EMPTY(lcIndex)
    SET ORDER TO &lcIndex.
  ENDIF  
  IF BETWEEN(lnRecNum,1,RECCOUNT())
    GO RECORD lnRecNum  
  ENDIF
  IF loMainFormSet.llShoPckSu
    WITH loMainFormSet.AriaForm1.pgfPacking.Detail.grdDetail
      .RECORDSOURCE = loMainFormSet.lcPckLin
      *-- Select
      .Column1.Header1.CAPTION = ""
      .Column1.CONTROLSOURCE  = loMainFormSet.lcPckLin +'.Selected'
      *-- Pack-Id
      .Column2.CONTROLSOURCE  = loMainFormSet.lcPckLin + '.cPackId'
      .Column2.VISIBLE = IIF(loMainFormSet.llUsePack, .T., .F.)
      *-- Style
      .Column3.CONTROLSOURCE  = loMainFormSet.lcPckLin + '.Style'
      .Column3.Header1.CAPTION = loMainFormSet.lcStyTtl
      .Column3.VISIBLE = .T.
      *-- Configuration
      .Column4.Header1.CAPTION = IIF(loMainFormSet.llUseConfg,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ManulPL_CptConfig,loMainFormSet.GetHeaderText("LANG_ManulPL_CptConfig",loMainFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ManulPL_CptDyelot,loMainFormSet.GetHeaderText("LANG_ManulPL_CptDyelot",loMainFormSet.HeaderAlias)))
      .Column4.CONTROLSOURCE   = loMainFormSet.lcPckLin + '.Dyelot'
      .Column4.VISIBLE      = IIF(loMainFormSet.llUseConfg OR loMainFormSet.llDyelot , .T., .F.)
      *-- Size
      .Column5.CONTROLSOURCE  = loMainFormSet.lcPckLin +'.cSizeCod'
      *-- O.Qty
      .Column6.CONTROLSOURCE = loMainFormSet.lcPckLin +'.OQty'
      .Column6.Header1.ALIGNMENT = 1     && Right

      *-- Qty.\Ctn
      .Column7.CONTROLSOURCE = loMainFormSet.lcPckLin +'.CtnQty'
      .Column7.Header1.ALIGNMENT = 1     && Right

      *-- Wgh.\Unt
      .Column8.CONTROLSOURCE = loMainFormSet.lcPckLin +'.StyWgh'
      .Column8.Header1.ALIGNMENT = 1     && Right

      *-- PackQty
      .Column9.CONTROLSOURCE = loMainFormSet.lcPckLin +'.PQty'
      .Column9.Header1.ALIGNMENT = 1     && Right

      *-- PackWght
      .Column10.CONTROLSOURCE = loMainFormSet.lcPckLin +'.PWgh'
      .Column10.Header1.ALIGNMENT = 1     && Right
      .Column11.VISIBLE = .T.
      .Column11.CONTROLSOURCE  = loMainFormSet.lcPckLin +'.SKUNUM'
      .Column11.COLUMNORDER = 11
    ENDWITH
  ENDIF  
ENDIF

IF TYPE('loMainFormSet.lcStyTmp') ='U' 
  loMainFormSet.AddProperty("lcStyTmp",gfTempName())
  CREATE CURSOR (loMainFormSet.lcStyTmp) (Style C(19),lSelect L,lUsePrePk L)
  SELECT (loMainFormSet.lcStyTmp) 
  INDEX ON STYLE TAG (loMainFormSet.lcStyTmp)
ELSE
  SELECT (loMainFormSet.lcStyTmp) 
  ZAP
ENDIF

IF TYPE('loMainFormSet.lcDetTmp') ='U' 
  loMainFormSet.AddProperty("lcDetTmp",gfTempName())
  CREATE CURSOR (loMainFormSet.lcDetTmp) (lSelect L,Style C(19),NCASEPACK N(9),CSTYLEPO C(30),CPACKBY C(1))
  SELECT (loMainFormSet.lcDetTmp)
  INDEX ON Style TAG (loMainFormSet.lcDetTmp)
ELSE
  SELECT (loMainFormSet.lcDetTmp)
  SET KEY TO 
  ZAP
ENDIF
SELECT (lcPakLin)
lnRecNum = RECNO()
LOCATE
SCAN
 IF !SEEK(&lcPakLin..STYLE,loMainFormSet.lcStyTmp)
   INSERT INTO (loMainFormSet.lcStyTmp) VALUES (&lcPakLin..STYLE,.F.,!EMPTY(ALLTRIM(&lcPakLin..prepak)))
 ELSE
   REPLACE lUsePrePk WITH (lUsePrePk OR !EMPTY(ALLTRIM(&lcPakLin..prepak))) IN (loMainFormSet.lcStyTmp)
 ENDIF 
 IF !SEEK(&lcPakLin..STYLE,loMainFormSet.lcDetTmp)
   INSERT INTO (loMainFormSet.lcDetTmp) VALUES (.T.,&lcPakLin..STYLE,&lcPakLin..NCASEPACK,&lcPakLin..CSTYLEPO,&lcPakLin..CPACKBY)
 ENDIF 
ENDSCAN
SELECT (loMainFormSet.lcStyTmp)
LOCATE
=GFCALLFORM("ALPCKINF",'AL',".T.,'"+loMainFormSet.lcStyTmp+"','"+loMainFormSet.lcDetTmp+"','"+lcPakLin+"','"+loMainFormSet.ActiveMode+"'")
SELECT (lcPakLin)
IF !EMPTY(lcSetKeyPack)
  SET KEY TO (lcSetKeyPack)
ENDIF
IF !EMPTY(lcSetFilter)
  SET FILTER TO &lcSetFilter.
ENDIF
IF BETWEEN(lnRecNum,1,RECCOUNT())
  GO RECORD lnRecNum  
ENDIF
SET DATASESSION TO &lnDataSession.
*!**************************************************************************
*! Name       : lfADDPCKFLD
*! Developer  : Mariam Mazhar - MMT
*! Date       : 03/23/2017
*! Purpose    : add Packing info. field  to  temp  file in Manual PL screen
*! Type       : Custom
*!**************************************************************************
FUNCTION lfADDPCKFLD
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'NCASEPACK'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 9
laFileStru[lnFileStru+1,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'CPACKBY'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'CSTYLEPO '
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 30
laFileStru[lnFileStru+1,4] = 0

*!**************************************************************************
*! Name       : lfUPDPCKFLD
*! Developer  : Mariam Mazhar - MMT
*! Date       : 03/23/2017
*! Purpose    : Update Packing info. fields  in  temp  file in Manual PL screen
*! Type       : Custom
*!**************************************************************************
FUNCTION lfUPDPCKFLD

REPLACE NCASEPACK WITH m.NCASEPACK,;
        CSTYLEPO  WITH m.CSTYLEPO,;
        CPACKBY   WITH m.CPACKBY IN (lcTmPckLin)
*!**************************************************************************
*! Name       : lfUPDPKLIN         
*! Developer  : Mariam Mazhar - MMT
*! Date       : 03/23/2017
*! Purpose    : Update Packing info. fields  in  Pack_lin  file in Manual PL screen
*! Type       : Custom
*!**************************************************************************
FUNCTION lfUPDPKLIN         
=SEEK(&lcCtnDtl..STYLE+&lcCtnDtl..Dyelot+STR(&lcCtnDtl..nOrdLineNo,6),lcPckLin,lcPckLin)
SELECT Pack_Lin
=gfREPLACE("CPACKBY   WITH &lcPckLin..CPACKBY   ,;
                                  CSTYLEPO   WITH &lcPckLin..CSTYLEPO,;
                                  NCASEPACK  WITH &lcPckLin..NCASEPACK")

*!**************************************************************************
*! Name       : lfUPDPKLIN         
*! Developer  : Mariam Mazhar - MMT
*! Date       : 03/23/2017
*! Purpose    : Read  Packing info. fields  from   Pack_lin  file in Manual PL screen
*! Type       : Custom
*!**************************************************************************
FUNCTION lfGETPCKFLD
*! B611916,1 MMT 06/27/2021 Enhance Manual Packing list at BBC[T20210329.0003][Start]       
*!*	REPLACE NCASEPACK WITH PACK_LIN.NCASEPACK,;
*!*	        CSTYLEPO  WITH PACK_LIN.CSTYLEPO,;
*!*	        CPACKBY   WITH PACK_LIN.CPACKBY IN (loFormSet.lcPckLin)                                  
lnSelecAl = ALIAS()
SELECT (loFormSet.lcPckLin)
lcPckLinTmp = loFormSet.lcPckLin
lnRecNoPkLin = RECNO()
lcOrderOldPk = ORDER()
SELECT Pack_Lin
loFormSet.Pack_Lin.SetOrder('PackStyle')
SET ORDER TO (loFormSet.lcPakIndxSt) IN (loFormSet.lcPckLin)
IF loFormSet.Pack_Lin.SEEK(lcPckNo)
  SCAN REST WHILE Pack_no+STR(No_Cart,4)+STYLE+Dyelot = lcPckNo
    IF llFrmPikLn
      SELECT PikLine
      loFormSet.PikLine.SEEK(lcPkTktNo+lcOrderNo+STR(Pack_Lin.nOrdLineNo,6))
    ELSE
      SELECT "OrdLine"
      loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+Pack_Lin.STYLE+STR(Pack_Lin.nOrdLineNo,6))
    ENDIF
    loFormSet.SCALE.SEEK('S'+IIF(llFrmPikLn,PikLine.SCALE,OrdLine.SCALE))
    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      IF Pack_Lin.Qty&lcI = 0
        LOOP
      ENDIF
      =SEEK(Pack_Lin.Pack_ID+Pack_Lin.cPkColor+Pack_Lin.cPckSize+Pack_Lin.cPkVersion+Pack_Lin.STYLE+Pack_Lin.Dyelot+;
          STR(Pack_Lin.nOrdLineNo,6)+lcI,loFormSet.lcPckLin)

      REPLACE NCASEPACK WITH PACK_LIN.NCASEPACK,;
              CSTYLEPO  WITH PACK_LIN.CSTYLEPO,;
              CPACKBY   WITH PACK_LIN.CPACKBY IN (loFormSet.lcPckLin)                                  
    ENDFOR
  ENDSCAN
ENDIF
SELECT (lcPckLinTmp)
IF !EMPTY(lcOrderOldPk)
  SET ORDER to (lcOrderOldPk)
ENDIF  
IF BETWEEN(lnRecNoPkLin ,1,RECCOUNT(loFormSet.lcPckLin))
  GO RECORD lnRecNoPkLin IN (loFormSet.lcPckLin)
ENDIF
SELECT(lnSelecAl)
*! B611916,1 MMT 06/27/2021 Enhance Manual Packing list at BBC[T20210329.0003][End]       

*! C202429,1 MMT 09/09/2021 Add triggers to be called from Key off screen to update the EBREMITT and EBPAYMT[P20210827.0001][Start]
*!**************************************************************************
*! Name       : lfCHKSMRINV
*! Developer  : Mariam Mazhar - MMT
*! Date       : 09/09/2021
*! Purpose    : Check the smart invoices of the selected remittances
*! Type       : Custom
*!**************************************************************************
FUNCTION lfCHKSMRINV
SET STEP ON 

*XXX
IF !USED('EBREMITT_B')
  =gfOpenTable('EBREMITT','EBREMITT','SH','EBREMITT_B')
ENDIF
*!*  SELECT(loBranchFormSet.lctempremit)
*!*  SCAN FOR llSel
*!*    SELECT(loBranchFormSet.lctempDebit)
*!*    SCAN FOR Reference = EVALUATE(loBranchFormSet.lctempremit +'.Reference')
*!*      
*!*    ENDSCAN 
*!*  ENDSCAN
SELECT(loBranchFormSet.lctempremit)
SELECT SUM(EBREMITT_B.NAPAMNT) As 'NAPAMNT',EBREMITT_B.DESC FROM 'EBREMITT_B' ;
      WHERE   EBREMITT_B.Account = loBranchFormSet.lokeyoffformset.cAccount AND; 
      EBREMITT_B.Reference in (Select Reference from (loBranchFormSet.lctempremit) where LLSEL AND !DELETED()) ;
      and !DELETED();
      GROUP BY EBREMITT_B.DESC INTO ARRAY laNewAccAmt
IF _tally > 0
  CREATE CURSOR 'AccSum' (Account C(5), Amount N(12,2), Desc C(30),TranType C(1),Group C(1))
  SELECT AccSum
  INDEX on Account+DESC TAG 'AccSum'
  FOR lnAccAm =1 TO ALEN(laNewAccAmt,1)
    INSERT INTO 'AccSum' (Account, Amount , Desc,TranType ,Group) VALUES ;
        (loBranchFormSet.lokeyoffformset.cAccount,laNewAccAmt[lnAccAm ,1],laNewAccAmt[lnAccAm ,2],IIF(laNewAccAmt[lnAccAm ,1]>0,'D','C'),'A')
  ENDFOR
  SELECT 'AccSum'
  LOCATE
  =gfCallForm('ARACCSUM','AR','.F.')  
ENDIF   

*XXX
IF !USED('SMART_INVOICE_HEADER')
  =gfOpenTable('SMART_INVOICE_HEADER','SMINVHDR','SH')
ENDIF
*XXX
llDSAccount = .F.
IF gfSeek(loBranchFormSet.lokeyoffformset.cAccount,'SMART_INVOICE_HEADER','SMINVACS')
  llDSAccount = .T.
ENDIF
*XXX
CREATE CURSOR 'TempInvoice' (INVOICE C(6),Amount N(14,2),OrgAmnt N(14,2))
SELECT 'TempInvoice'
INDEX on INVOICE TAG 'TmpInv'

SELECT(loBranchFormSet.lctempremit)
SCAN FOR llSel
  * Select the Cash receipt related to the Remittance
  SELECT tmpcredit
  LOCATE FOR Reference  =eVal(loBranchFormSet.lctempremit+'.Reference') AND TranType ='4'
  IF FOUND()
    =SEEK(tmpcredit.account+tmpcredit.tran+DTOS(tmpcredit.trandate),'Credit')
    loBranchFormSet.loKeyOffFormSet.AriaForm1.oArkeyOff.mvalidselect('CREDIT')
  ENDIF
  llShowMsg = .T.
  SELECT(loBranchFormSet.lctempDebit)
  SCAN FOR Reference = EVALUATE(loBranchFormSet.lctempremit +'.Reference')
    
    IF llDSAccount AND gfSeek(EVALUATE(loBranchFormSet.lctempDebit+'.CRDRNO'),'SMART_INVOICE_HEADER','SMINVHDR') AND ;
       !EMPTY(SMART_INVOICE_HEADER.INVOICE) AND  SMART_INVOICE_HEADER.Status <> 'C' AND gfSeek('1'+SMART_INVOICE_HEADER.INVOICE,'DEBIT_REM','DRTRAN') 
      IF !EMPTY(SMART_INVOICE_HEADER.INVOICE) AND  SMART_INVOICE_HEADER.Status <> 'C' AND gfSeek('1'+SMART_INVOICE_HEADER.INVOICE,'DEBIT_REM','DRTRAN') 
        IF !SEEK(SMART_INVOICE_HEADER.INVOICE,'TempInvoice','TmpInv')
          INSERT INTO 'TempInvoice' (INVOICE,Amount,OrgAmnt) VALUES (SMART_INVOICE_HEADER.INVOICE,EVALUATE(loBranchFormSet.lctempDebit+'.appramt'),DEBIT_REM.Amount)
        ELSE
          REPLACE Amount WITH Amount+ EVALUATE(loBranchFormSet.lctempDebit+'.appramt') IN  'TempInvoice' 
        ENDIF
      ELSE
        *XXXX
        IF llShowMsg 
          = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"One or more dropship invoices have not been invoiced and the system will create a debit adjustment with the corresponding amount.") 
          llShowMsg = .F.  
        ENDIF  
        SELECT (loBranchFormSet.lokeyoffformset.cDCAdjTmp)
        LOCATE FOR EMPTY(TranType)
        *-- If there is no empty recors then add a new one.
        IF !FOUND()
           APPEND BLANK
           *! B611924,1 MMT 02/15/2022 fix the issue of duplicated adjustment record while keyoff[T20220207.0002][Start]
           lcTranNo = 'T'+SUBSTR(SYS(3),4,5)
           DO WHILE (SEEK(loBranchFormSet.lokeyoffformset.cAccount+;
                        IIF(lcTranTypeToUse='C','7',IIF(lcTranTypeToUse='D','2',IIF(lcTranTypeToUse='B','3','6')))+;
                        lcTranNo,loBranchFormSet.lokeyoffformset.cDCAllTmp,loBranchFormSet.lokeyoffformset.cDCAllTmp))
             lcTranNo = 'T'+SUBSTR(SYS(3),4,5)
           ENDDO
           *! B611924,1 MMT 02/15/2022 fix the issue of duplicated adjustment record while keyoff[T20220207.0002][End]
           REPLACE Account   WITH loBranchFormSet.lokeyoffformset.cAccount   ,;
                    Tran      WITH lcTranNo  ,;
                    TranDate  WITH loBranchFormSet.lokeyoffformset.AriaForm1.dtpKeyOffDate.Value ,;
                    Amount    WITH 0                      ,;
                    cFacCode  WITH loBranchFormSet.lokeyoffformset.cCustFactor ,;
                    cCurrCode WITH loBranchFormSet.lokeyoffformset.cCurcode   ,;
                    nExRate   WITH loBranchFormSet.lokeyoffformset.AriaForm1.txtCurrencyRate.Value ,;
                    nCurrUnit WITH loBranchFormSet.lokeyoffformset.nCurrUnit  ,;
                    cTranDesc WITH 'N/A'                  ,;
                    cYear     WITH loBranchFormSet.lokeyoffformset.cGlFyear   ,;
                    cPrd      WITH loBranchFormSet.lokeyoffformset.cGlPeriod 
            *XX
            loBranchFormSet.lokeyoffformset.mEnableDebitCredit(.T.)
            *XX        
                REPLACE TranType   WITH IIF(EVALUATE(loBranchFormSet.lctempDebit+'.appramt') < 0,'7','2'),;
                cFacCode   WITH '' ,;
                Reference  WITH EVALUATE(loBranchFormSet.lctempDebit+'.CRDRNO') ,;
                Amount WITH IIF(TranType="2", EVALUATE(loBranchFormSet.lctempDebit+'.appramt')   ,EVALUATE(loBranchFormSet.lctempDebit+'.appramt')),;
                Deb_Adj   WITH IIF(TranType = "2", "Y", Deb_Adj) ,;
                dPostDate WITH loBranchFormSet.lokeyoffformset.AriaForm1.dtpKeyOffDate.Value ,;
                cTranDesc WITH  IIF(TranType="2",'Debit Adj.','Credit Adj.'),;
                cReason   WITH IIF(TRANTYPE $ '23',lcReasonDescDebit,lcReasonDescCredit),;
                Desc      WITH IIF(TRANTYPE $ '23',lcReasonDescDebit,lcReasonDescCredit),;
                TranCode WITH  IIF(TRANTYPE $ '76',lcCredAdjReason ,lcDebitAdjReason),;
                cBnkCode  WITH IIF(TRANTYPE $ '32',lcDebitBankCode,lcCreditBankCode)  ,;
                cChkAcct  WITH IIF(TRANTYPE $ '32',lcDebitcChkAcc ,lcCreditcChkAcc )   ,;
                cAdjAcct  WITH IIF(TRANTYPE $ '32',lcDebitcGLAccnt,lcCreditcGLAccnt )
   


                
            =loBranchFormSet.loKeyOffFormSet.AriaForm1.OArkeyoff.mAddInAll(loBranchFormSet.loKeyOffFormSet.cDCAdjTmp)        
            lnAdjType = IIF(TRANTYPE = '2',1,2)
            llNoThing = loBranchFormSet.loKeyOffFormSet.AriaForm1.OArkeyoff.mUpdatbalance(SUBSTR("PN",lnAdjType,1),IIF(TRANTYPE = '2',-1,1)*ABS(Amount), +1)
            
        ENDIF
        *XXXX  
      ENDIF
    ELSE 
      IF EVALUATE(loBranchFormSet.lctempDebit+'.appramt') < 0
        SELECT tmpcredit
        LOCATE FOR TRAN =ALLTRIM(EVALUATE(loBranchFormSet.lctempDebit+'.CRDRNO'))
        IF FOUND()
          =SEEK(tmpcredit.account+tmpcredit.tran+DTOS(tmpcredit.trandate),'Credit')
          loBranchFormSet.loKeyOffFormSet.AriaForm1.oArkeyOff.mvalidselect('CREDIT')
          SELECT (loBranchFormSet.lctempDebit)
          LOOP 
        ENDIF
      ELSE
        SELECT tmpDebit
        LOCATE FOR TRAN =ALLTRIM(EVALUATE(loBranchFormSet.lctempDebit+'.CRDRNO'))
        IF FOUND()
          IF tmpDebit.TranType ='1'
            IF !SEEK(tmpDebit.Tran,'TempInvoice','TmpInv')
              INSERT INTO 'TempInvoice' (INVOICE,Amount,OrgAmnt) VALUES (tmpDebit.Tran,EVALUATE(loBranchFormSet.lctempDebit+'.appramt'),tmpDebit.Amount)
            ELSE
              REPLACE Amount WITH Amount+ EVALUATE(loBranchFormSet.lctempDebit+'.appramt') IN  'TempInvoice' 
            ENDIF
            SELECT (loBranchFormSet.lctempDebit)
            LOOP 
          ELSE
            =SEEK(tmpDebit.account+tmpDebit.tran+DTOS(tmpDebit.trandate),'DEBIT')
            loBranchFormSet.loKeyOffFormSet.AriaForm1.oArkeyOff.mvalidselect('DEBIT')
            SELECT (loBranchFormSet.lctempDebit)
            LOOP 
          ENDIF  
        ENDIF
        
      ENDIF 
    
      lcTranTypeToUse = ''
      =SEEK(loBranchFormSet.lokeyoffformset.cAccount+EVALUATE(loBranchFormSet.lctempDebit+'.DESC'),'AccSum','AccSum')
      lcTranTypeToUse = AccSum.TranType
      
      IF lcTranTypeToUse $ 'DC'
        SELECT (loBranchFormSet.lokeyoffformset.cDCAdjTmp)
      ELSE
        SELECT (loBranchFormSet.lokeyoffformset.cDCOnATmp)
      ENDIF
      LOCATE FOR EMPTY(TranType)
      *-- If there is no empty recors then add a new one.
      IF !FOUND()
         APPEND BLANK
         *! B611924,1 MMT 02/15/2022 fix the issue of duplicated adjustment record while keyoff[T20220207.0002][Start]
         lcTranNo = 'T'+SUBSTR(SYS(3),4,5)
         DO WHILE (SEEK(loBranchFormSet.lokeyoffformset.cAccount+;
                        IIF(lcTranTypeToUse='C','7',IIF(lcTranTypeToUse='D','2',IIF(lcTranTypeToUse='B','3','6')))+;
                        lcTranNo,loBranchFormSet.lokeyoffformset.cDCAllTmp,loBranchFormSet.lokeyoffformset.cDCAllTmp))
           lcTranNo = 'T'+SUBSTR(SYS(3),4,5)
         ENDDO
         *! B611924,1 MMT 02/15/2022 fix the issue of duplicated adjustment record while keyoff[T20220207.0002][End]
         REPLACE Account   WITH loBranchFormSet.lokeyoffformset.cAccount   ,;
                  Tran      WITH lcTranNo ,;
                  TranDate  WITH loBranchFormSet.lokeyoffformset.AriaForm1.dtpKeyOffDate.Value ,;
                  Amount    WITH 0                      ,;
                  cFacCode  WITH loBranchFormSet.lokeyoffformset.cCustFactor ,;
                  cCurrCode WITH loBranchFormSet.lokeyoffformset.cCurcode   ,;
                  nExRate   WITH loBranchFormSet.lokeyoffformset.AriaForm1.txtCurrencyRate.Value ,;
                  nCurrUnit WITH loBranchFormSet.lokeyoffformset.nCurrUnit  ,;
                  cTranDesc WITH 'N/A'                  
            IF TRANTYPE $ '27'        
              REPLACE cYear     WITH loBranchFormSet.lokeyoffformset.cGlFyear   ,;
                      cPrd      WITH loBranchFormSet.lokeyoffformset.cGlPeriod 
              
             ENDIF
            *XX        
     
              *XX   
*!*                REPLACE TranType   WITH IIF(EVALUATE(loBranchFormSet.lctempDebit+'.appramt') < 0,'7','2'),;
*!*                cFacCode   WITH '' ,;
*!*                Reference  WITH EVALUATE(loBranchFormSet.lctempDebit+'.Reference') ,;
*!*                Amount WITH IIF(TranType="2", EVALUATE(loBranchFormSet.lctempDebit+'.appramt')   ,EVALUATE(loBranchFormSet.lctempDebit+'.appramt')),;
*!*                Deb_Adj   WITH IIF(TranType = "2", "Y", Deb_Adj) ,;
*!*                dPostDate WITH loBranchFormSet.lokeyoffformset.AriaForm1.dtpKeyOffDate.Value ,;
*!*                cTranDesc WITH  IIF(TranType="2",'Debit Adj.','Credit Adj.'),;
*!*                cReason   WITH IIF(TRANTYPE = '2',lcReasonDescDebit,lcReasonDescCredit),;
*!*                Desc      WITH IIF(TRANTYPE = '2',lcReasonDescDebit,lcReasonDescCredit),;
*!*                TranCode WITH  IIF(TRANTYPE = '7',lcCredAdjReason ,lcDebitAdjReason),;
*!*                cBnkCode  WITH IIF(TRANTYPE = '2',lcDebitBankCode,lcCreditBankCode)  ,;
*!*                cChkAcct  WITH IIF(TRANTYPE = '2',lcDebitcChkAcc ,lcCreditcChkAcc )   ,;
*!*                cAdjAcct  WITH IIF(TRANTYPE = '2',lcDebitcGLAccnt,lcCreditcGLAccnt )
 
              REPLACE TranType   WITH IIF(lcTranTypeToUse='C','7',IIF(lcTranTypeToUse='D','2',IIF(lcTranTypeToUse='B','3','6'))),;
              cFacCode   WITH '' ,;
              Reference  WITH EVALUATE(loBranchFormSet.lctempDebit+'.CRDRNO') ,;
              Amount    WITH IIF(TranType="2", EVALUATE(loBranchFormSet.lctempDebit+'.appramt')   ,EVALUATE(loBranchFormSet.lctempDebit+'.appramt')),;
              Deb_Adj   WITH IIF(TranType = "2", "Y", Deb_Adj) ,;
              dPostDate WITH loBranchFormSet.lokeyoffformset.AriaForm1.dtpKeyOffDate.Value ,;
              cTranDesc WITH  IIF(TranType="2",'Debit Adj.','Credit Adj.'),;
              cReason   WITH IIF(TRANTYPE $ '23',lcReasonDescDebit,lcReasonDescCredit),;
              Desc      WITH IIF(TRANTYPE $ '23',lcReasonDescDebit,lcReasonDescCredit),;
              cBnkCode  WITH IIF(TRANTYPE = '2',lcDebitBankCode,lcCreditBankCode)  ,;
              cChkAcct  WITH IIF(TRANTYPE = '2',lcDebitcChkAcc ,lcCreditcChkAcc )   ,;
              cAdjAcct  WITH IIF(TRANTYPE = '2',lcDebitcGLAccnt,lcCreditcGLAccnt )
              
              REPLACE Amount    WITH IIF(TranType="3", ABS(EVALUATE(loBranchFormSet.lctempDebit+'.appramt')),;
                                     IIF(TranType="6",-1 * ABS(EVALUATE(loBranchFormSet.lctempDebit+'.appramt')),;
                                     EVALUATE(loBranchFormSet.lctempDebit+'.appramt')))
              
            IF TRANTYPE $ '23'
              REPLACE TRANCODE WITH lcDebitAdjReason
            ELSE   
              REPLACE  TRANCODE WITH  lcCredAdjReason 
            ENDIF       
          IF TRANTYPE $ '27'    
            =loBranchFormSet.loKeyOffFormSet.AriaForm1.OArkeyoff.mAddInAll(loBranchFormSet.loKeyOffFormSet.cDCAdjTmp)        
            lnAdjType = IIF(TRANTYPE = '2',1,2)
            llNoThing = loBranchFormSet.loKeyOffFormSet.AriaForm1.OArkeyoff.mUpdatbalance(SUBSTR("PN",lnAdjType,1),IIF(TRANTYPE = '2',-1,1)*ABS(Amount), +1)
          ELSE
            REPLACE ChgBk_Date WITH IIF(TranType="3", loBranchFormSet.lokeyoffformset.AriaForm1.dtpKeyOffDate.Value , ChgBk_Date) ,;
                    Credt_Date WITH IIF(TranType="6", loBranchFormSet.lokeyoffformset.AriaForm1.dtpKeyOffDate.Value , Credt_Date) ,;
                    cBnkCode  WITH "",;
                    cChkAcct  WITH "",;
                    cAdjAcct  WITH '',;
                    cTranDesc  WITH IIF(TRANTYPE ='3','Debit On Acc.','Credit On Acc.'),;
                    dPostDate  WITH {},;
                    cFacCode  WITH loBranchFormSet.lokeyoffformset.cCustFactor IN (loBranchFormSet.lokeyoffformset.cDCOnATmp)
            
            llNoThing = loBranchFormSet.loKeyOffFormSet.AriaForm1.OArkeyoff.mUpdAccntBal(+1, Amount)      
          ENDIF 
          *XXX
            IF lcTranTypeToUse $ 'DC'
              loBranchFormSet.lokeyoffformset.mEnableDebitCredit(.T.)
               *XX
            ELSE 
               *XX         
               loBranchFormSet.lokeyoffformset.mRefreshOnAccount()
               *XX
            ENDIF

      ENDIF
    ENDIF      
  ENDSCAN 
ENDSCAN   
lcFacCode  = ''
IF SEEK('M'+loBranchFormSet.lokeyoffformset.cAccount,'CUSTOMER','CUSTOMER')  
  lcFacCode  = CUSTOMER.cFacCode
ENDIF

SELECT  'TempInvoice'
LOCATE 
SCAN 
  SELECT tmpdebit
  LOCATE FOR Tran  =TempInvoice.Invoice AND TranType ='1'
  IF FOUND()
    =SEEK(tmpdebit.account+tmpdebit.tran+tmpdebit.cinstalno+DTOS(tmpdebit.trandate),'Debit')
    lcKeyExp = Debit.Account+Debit.TranType+Debit.Tran+Debit.cInstalNo
    loBranchFormSet.loKeyOffFormSet.AriaForm1.oArkeyOff.mvalidselect('DEBIT')
    IF TempInvoice.Amount < TempInvoice.OrgAmnt 
      lnOldAccDiffr = loBranchFormSet.loKeyOffFormSet.AriaForm1.oArkeyOff.nAccDiffr
      loBranchFormSet.loKeyOffFormSet.AriaForm1.oArkeyOff.nAccDiffr = TempInvoice.OrgAmnt - TempInvoice.Amount 
      =loBranchFormSet.loKeyOffFormSet.AriaForm1.oArkeyOff.mAddOnAccount(lcFacCode)
      =loBranchFormSet.loKeyOffFormSet.AriaForm1.oArkeyOff.mAddOpenBalance(lcKeyExp)
      loBranchFormSet.loKeyOffFormSet.AriaForm1.oArkeyOff.nAccDiffr = lnOldAccDiffr 
    ENDIF  
  ENDIF
ENDSCAN 
*!**************************************************************************
*! Name       : LFUPDSMRTINV
*! Developer  : Mariam Mazhar - MMT
*! Date       : 09/09/2021
*! Purpose    : update the smart invoices of the selected remittances
*! Type       : Custom
*!**************************************************************************
FUNCTION LFUPDSMRTINV
IF !EMPTY(loFormSet.lctempremit) AND used (loFormSet.lctempremit)
SELECT(loFormSet.lctempremit)
LOCATE 
SCAN FOR llSel
  SELECT EBREMITT
  =gfSeek(loFormSet.cAccount+EVALUATE(loFormSet.lctempremit+'.Reference'))
  SCAN REST WHILE  ACCOUNT+REFERENCE+STR(LINENO,6) = loFormSet.cAccount+EVALUATE(loFormSet.lctempremit+'.Reference')
    SELECT SMART_INVOICE_HEADER
    IF gfSeek(EBREMITT.CRDRNO)
      gfReplace("STATUS with 'C'")  
      gfReplace("PAYDATE with loFormSet.AriaForm1.dtpKeyOffDate.Value")
    ENDIF
  ENDSCAN
ENDSCAN 
ENDIF
IF USED('SMART_INVOICE_HEADER')
  SELECT SMART_INVOICE_HEADER
  =gfTableUpdate()
ENDIF
*! C202429,1 MMT 09/09/2021 Add triggers to be called from Key off screen to update the EBREMITT and EBPAYMT[P20210827.0001][End]