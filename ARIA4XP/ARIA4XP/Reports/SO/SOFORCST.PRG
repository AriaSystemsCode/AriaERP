*:************************************************************************
*: Program file  : SOFORCST.PRG
*: Program desc. : FORECASTING REPORT
*: System        : Aria A4xp
*: Module        : SO
*: Date          : 01/18/2007
*: Developer     : AYMAN MAHMOUD AMED (AYM)
*: Ticket No     : T20061226.0037
*! Track no      : N000581
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
*:************************************************************************
*:Passed Parameters : None
*:************************************************************************
*:Example           : Do SOFORCST
*:************************************************************************
*:Modifications ..
*:************************************************************************

IF llOgFltCh
llDonprnt=.F.
llAllStyles=.t.

*-- Define Variables.
DIMENSION lcMTyp(1)   , lcITyp(1)
DIMENSION laForc(8)   , laDem(8)      , laHand(8)     , laTrans(8)   , laAvail(8)   , laSComp(8)        


DIMENSION laDate(9,2) , laWekYer(10,2), laSetupsM(7,2), laSetupsI(7,2)

STORE .F. TO llZeroAll ,llPrint
STORE {}  TO ldFOgDate, ldLOgDate     , laDate
STORE " " TO lcMTyp   , lcITyp        , lcCutCond     , lcPoCond      , lcSty1       , lcCutCondS , lcPoCondS
STORE 0   TO lnITyp   , lnMTyp        , laForc        , laDem         , laHand       , laTrans   ,;
             laAvail  , laSComp       , laWekYer      ,lnTForc        , lnTDem       , lnTHand   ,;
             lnTTrans , lnTAvail      , lnNumWeek     , lnCurWeek     , lnWeek       , lnYear

*-- To get the length of the major segment.
lcMjrPct   = gfItemMask('PM')
lnMajWidth = LEN(lcMjrPct)
lnSpcReq = 19 - lnMajWidth

*-- Open Needed Files.
=gfOpenTable('FORCAST','FORCAST','SH','FORCAST')
=gfOpenTable('STYLE','STYLE','SH','STYLE')
=gfOpenTable('SCALE','SCALE','SH','SCALE')
=gfOpenTable('ORDLINE','ORDLINES','SH','ORDLINE')
=gfOpenTable('ORDHDR','ORDHDR','SH','ORDHDR')


IF OCCURS('PO',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('MF',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('PS',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('RS',oAriaApplication.CompanyInstalledModules)<>0
  =gfOpenTable('POSLN','POSLNS','SH','POSLN')
  =gfOpenTable('POSHDR','POSHDR','SH','POSHDR')
  =gfOpenTable('POSHDR','POSHDRS','SH','POSHDRS')
  =gfOpenTable('BOM','MBOMITEM','SH','BOM')
  =gfOpenTable('CTKTBOM','CTKTBOM','SH','CTKTBOM')

ENDIF

*-- Loop To Determine The Type Of Style Comp. For Manufactured And Imported Style.
laSetupsM[1,1] = 'M_CmType1'
laSetupsM[2,1] = 'M_CmType2'
laSetupsM[3,1] = 'M_CmType3'
laSetupsM[4,1] = 'M_CmType4'
laSetupsM[5,1] = 'M_CmType5'
laSetupsM[6,1] = 'M_CmType6'
laSetupsM[7,1] = 'M_CmType7'


laSetupsI[1,1] = 'M_CiType1'
laSetupsI[2,1] = 'M_CiType2'
laSetupsI[3,1] = 'M_CiType3'
laSetupsI[4,1] = 'M_CiType4'
laSetupsI[5,1] = 'M_CiType5'
laSetupsI[6,1] = 'M_CiType6'
laSetupsI[7,1] = 'M_CiType7'

=gfGetMemVar(@laSetupsM,oAriaApplication.ActiveModuleID)
=gfGetMemVar(@laSetupsI,oAriaApplication.ActiveModuleID)

FOR lnElmn = 1 TO 7
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

lcRpTmp=loOgScroll.gfTempName()
*-- Create Temporary File To Hold Collecting Data.
CREATE TABLE (oAriaApplication.WorkDir +lcRpTmp) (SC C(1),SCALE    C(6),STYLE C(19),DESC  C(30),;
                                  FORCAST1 N(10),FORCAST2 N(10),FORCAST3 N(10),;
                                  FORCAST4 N(10),FORCAST5 N(10),FORCAST6 N(10),; 
                                  FORCAST7 N(10),FORCAST8 N(10),FORCAST9 N(10),FORCAST10 N(10),;
                                  DEMAND1  N(10),DEMAND2  N(10),DEMAND3  N(10),;
                                  DEMAND4  N(10),DEMAND5  N(10),DEMAND6  N(10),;
                                  DEMAND7  N(10),DEMAND8  N(10),DEMAND9  N(10),DEMAND10  N(10),;
                                  ONHAND1  N(10),ONHAND2  N(10),ONHAND3  N(10),;
                                  ONHAND4  N(10),ONHAND5  N(10),ONHAND6  N(10),;
                                  ONHAND7  N(10),ONHAND8  N(10),ONHAND9  N(10),ONHAND10  N(10),;
                                  TRANS1   N(10),TRANS2   N(10),TRANS3   N(10),;
                                  TRANS4   N(10),TRANS5   N(10),TRANS6   N(10),;
                                  TRANS7   N(10),TRANS8   N(10),TRANS9   N(10),TRANS10   N(10),;
                                  AVAIL1   N(10),AVAIL2   N(10),AVAIL3   N(10),;
                                  AVAIL4   N(10),AVAIL5   N(10),AVAIL6   N(10),;
                                  AVAIL7   N(10),AVAIL8   N(10),AVAIL9   N(10),AVAIL10   N(10),Neg N(10))  
INDEX ON Style+SCALE TAG &lcRpTmp

*Creting filter exp   .. begin
lcseek=''
lcCurFltr= lfCheckFilter(1, 'STYLE.CSTYMAJOR')
llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
IF llCurFltr   
  SELECT (lcCurFltr)
  INDEX ON CSTYMAJOR TAG (lcCurFltr)
  lcseek=lcseek+" AND SEEK(STYLE.CSTYMAJOR ,'"+lcCurFltr+"')"
  llAllStyles=.f.

ELSE
  IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
    USE IN (lcCurFltr)
  ENDIF
  lcCurFltr= ''
ENDIF
 
 
lcCurFltr= lfCheckFilter(1, 'STYLE.FABRIC')
llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
IF llCurFltr   
  SELECT (lcCurFltr)
  INDEX ON CSTYMAJOR TAG (lcCurFltr)
  lcseek=lcseek+" AND SEEK(STYLE.FABRIC ,'"+lcCurFltr+"')"
  llAllStyles=.f.

ELSE
  IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
    USE IN (lcCurFltr)
  ENDIF
  lcCurFltr= ''
ENDIF
 
 
* Check if there is a filter on Style CDIVISION
lcCurName = lfCheckFilter(1, 'STYLE.CDIVISION')  
lcDiv   = loOgScroll.gfTempName()
llDiv   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcDiv ,"CDivision")
IF llDiv   
  SELECT (lcDiv)
  INDEX on CDivision TAG (lcDiv)
  lcseek=lcseek+" AND SEEK(STYLE.CDIVISION,'"+lcDiv+"')"
  llAllStyles=.f.

ENDIF

* Check if there is a filter on Style SEASON
lcCurName = lfCheckFilter(1, 'STYLE.SEASON')   
lcSea  = loOgScroll.gfTempName()
llSea   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSea  ,"SEASON") 
IF llSea   
  SELECT (lcSea  )
  INDEX on SEASON TAG (lcSea  )
    lcseek=lcseek+" AND SEEK(STYLE.SEASON,'"+lcSea+"')"
    llAllStyles=.f.

ENDIF

* Check if there is a filter on Style Group
lcCurName = lfCheckFilter(1, 'STYLE.CSTYGROUP')  
lcGrp  = loOgScroll.gfTempName()
llGrp   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcGrp  ,"Group") 
IF llGrp   
  SELECT (lcGrp  )
  INDEX on Group TAG (lcGrp  )
  lcseek=lcseek+" AND SEEK(STYLE.CSTYGROUP,'"+lcGrp+"')"
  llAllStyles=.f.
ENDIF


* Check if there is a filter on Style COLOR
lcCurName = lfCheckFilter(1, 'SUBSTR(STYLE.STYLE,lnStartPos,lnNonMajLn)')   
lcCol  = loOgScroll.gfTempName()
llCol   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcCol  ,"Color") 
IF llCol   
  SELECT (lcCol  )
  INDEX on Color TAG (lcCol  )
  lcseek=lcseek+" AND SEEK(SUBSTR(STYLE.STYLE,lnStartPos,lnNonMajLn),'"+lcCol  +"')"
  llAllStyles=.f.
ENDIF
  
* PATTERN Filter
lcPattern= lfCheckFilter(1, 'STYLE.PATTERN')
llPattern=IIF(EMPTY(lcPattern),.f.,.t.)
IF llPattern
  lcSeek=lcSeek +" AND STYLE.PATTERN='"+lcPattern+"'"
  llAllStyles=.f.
ENDIF

lcSeek= lcSeek+ IIF(lcRpStatus<>' ',' .AND. Style.Status $ lcRpStatus','')
=GFSEEK('P','POSHDR','POSHDR')
lcStyMaj=''
IF llAllStyles
    llAllStyles=.T.
	=GFSEEK("0001","POSLN")
	=GFSEEK("0001","POSHDRS")
	SELECT BOM 
	=GFSETORDER("MULTIBOM")
	=GFSEEK("0001"+"BOM")
	=GFSETORDER("MBOMITEM")
	SELECT CTKTBOM
	=GFSETORDER("CTKTITEM")
	=GFSEEK("0001")
	=GFSETORDER("CTKTBOM")
ENDIF
=GFSEEK('S','SCALE')

lcseek=" .T. "+lcseek

SELECT STYLE
GFSETORDER('STYLE')
=gfseek('')

SCAN REST WHILE style='' For &lcSeek
  IF !llAllStyles AND lcStyMaj<>style.CSTYMAJOR
    =GFSEEK("0001"+LEFT(STYLE.STYLE,lnMajWidth),"POSLN")
    =GFSEEK("0001"+LEFT(STYLE.STYLE,lnMajWidth),"POSHDRS")
    SELECT BOM 
    =GFSETORDER("MULTIBOM")
    =GFSEEK("0001"+LEFT(STYLE.STYLE,lnMajWidth),"BOM")
    =GFSETORDER("MBOMITEM")
    SELECT CTKTBOM
    =GFSETORDER("CTKTITEM")
    =GFSEEK("0001"+LEFT(STYLE.STYLE,lnMajWidth))
    =GFSETORDER("CTKTBOM")
    lcStyMaj=style.CSTYMAJOR
  ELSE
    lcStyMaj=style.CSTYMAJOR
  ENDIF   

  SELECT STYLE
  =SEEK('S'+STYLE.SCALE,'SCALE')
  STORE 0 TO lnNoWeek
  WAIT WINDOW 'Collecting data for ' + lcStyTit +': ' + Style NOWAIT
  FOR LN_CONT = 1 TO 10
    =lfGetDem(LN_CONT,Style) 
    =lfGetForc(LN_CONT,Style) 
    =lfGetHand(LN_CONT) 
    =lfGetWip(LN_CONT,Style)
    =lfGetAvail(LN_CONT,Style)
     lcCont=ALLTRIM(STR(LN_CONT))
    FOR lnScale=1 TO scale.cnt
    lcSz=ALLTRIM(STR(lnScale))
      IF LN_CONT=1
		    INSERT INTO (lcRpTmp) ;
		                (SC,SCALE     ,STYLE ,DESC     ,FORCAST1,DEMAND1   ,ONHAND1  ,TRANS1   ,AVAIL1,Neg ) ;
		    VALUES (lcSz,SCALE.sz&lcSz ,STYLE.Style,STYLE.DESC ,laForc(lnScale)  , laDem(lnScale)    ,laHand(lnScale)    ,laTrans(lnScale)   ,laAvail(lnScale),IIF(laAvail(lnScale)<0,1,0))            
      ELSE
        =SEEK(style+SCALE.sz&lcSz,lcRpTmp)
      	REPLACE   	&lcRpTmp..FORCAST&lcCont   WITH 	laForc(lnScale),;
      	     	      &lcRpTmp..DEMAND&lcCont  WITH 	laDem(lnScale),;  
      	     	      &lcRpTmp..ONHAND&lcCont  WITH 	laHand(lnScale),;  	 
      	     	      &lcRpTmp..TRANS&lcCont   WITH 	laTrans(lnScale),;  	 
      	     	      &lcRpTmp..AVAIL&lcCont   WITH 	laAvail(lnScale),;
      	     	      &lcRpTmp..NEG            WITH     IIF(laAvail(lnScale)<0,1,&lcRpTmp..NEG)
      ENDIF 
		ENDFOR 	
  ENDFOR
ENDSCAN
WAIT CLEAR

SELECT STYLE
GFSETORDER('CSTYLE')

SELECT (lcRpTmp)
IF !RECCOUNT()>0
  llDonprnt=.T.
  *-- Message : There are no records to display...!
  *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF 
 
IF  lcRpNegAll ='N'
  SUM(NEG) TO lnNegat
  IF lnNegat=0
	  llDonprnt=.T.
	  *-- Message : There are no records to display...!
	  *--                < Ok > 
	    =gfModalGen('TRM00052B40011','ALERT')
	  RETURN
  ENDIF 
ENDIF 
=lfAdjustCRSettings()
IF USED(lcRpTmp)
    USE IN (lcRpTmp)
ENDIF
=gfDispRe()
ELSE
  IF llDonprnt
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    =gfDispRe()
  ENDIF  
ENDIF  &&FILTER CHANGE

*-- End Of SOFORCST.PRG Program. --*


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
*! Passed Parameters  : LN_CONT,Style
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetDem(LN_CONT,Style) 
*!**************************************************************************

FUNCTION lfGetDem
PARAMETERS lnWk, lcStyle 

lcAlias = ALIAS()
STORE 0 TO laDem , lnTdem

DO CASE
  CASE lnWk = 1                 &&Case for column previous
    lcOrdCond = "ORDLINE.Start < laDate(1,1)"  
    IF OCCURS('MF',oAriaApplication.CompanyInstalledModules)<>0
      lcCutCond = "POSHDR.Complete < laDate(1,1)"  
      lcCutCond = "POSHDRS.Complete < laDate(1,1)"  

    ENDIF
    
    IF OCCURS('PO',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('MF',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('PS',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('RS',oAriaApplication.CompanyInstalledModules)<>0
      lcPoCond  = "POSHDR.Available < laDate(1,1)" 
      lcPoCondS  = "POSHDR.Available < laDate(1,1)"  

    ENDIF
  CASE lnWk = 10                &&Case for the last week
    lcOrdCond = "ORDLINE.Start > laDate(9,1) "
    IF OCCURS('MF',oAriaApplication.CompanyInstalledModules)<>0
      lcCutCond = "POSHDR.Complete > laDate(9,1)"
      lcCutCondS = "POSHDR.Complete > laDate(9,1)"

    ENDIF
    IF OCCURS('PO',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('MF',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('PS',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('RS',oAriaApplication.CompanyInstalledModules)<>0
      lcPoCond  = "POSHDR.Available > laDate(9,1)"
      lcPoCondS  = "POSHDR.Available > laDate(9,1)"

    ENDIF
  OTHERWISE
    lnNoWeek = lnNoWeek + 1
    lcOrdCond = "BETWEEN(ORDLINE.Start,laDate(lnNoWeek,1),laDate(lnNoWeek,2))"
    IF OCCURS('MF',oAriaApplication.CompanyInstalledModules)<>0
      lcCutCond = "BETWEEN(POSHDR.Complete,laDate(lnNoWeek,1),laDate(lnNoWeek,2))"
      lcCutCondS = "BETWEEN(POSHDR.Complete,laDate(lnNoWeek,1),laDate(lnNoWeek,2))"

    ENDIF
    IF OCCURS('PO',oAriaApplication.CompanyInstalledModules)<>0
      lcPoCond  = "BETWEEN(POSHDR.Available,laDate(lnNoWeek,1),laDate(lnNoWeek,2))"
      lcPoCondS  = "BETWEEN(POSHDR.Available,laDate(lnNoWeek,1),laDate(lnNoWeek,2))"

    ENDIF
ENDCASE  

IF GFSEEK(lcStyle,"ORDLINE")
  SELECT ORDLINE
  SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8 ;
      WHILE STYLE = lcStyle ;
      FOR GFSEEK(ORDLINE.cordtype+ORDLINE.ORDER,'ORDHDR') AND ORDHDR.Status $ "OH" AND &lcOrdCond TO ARRAY laDem
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
*! Passed Parameters  : LN_CONT,Style
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetForc(LN_CONT,Style) 
*!**************************************************************************

FUNCTION lfGetForc
PARAMETERS lnWk, lcStyle
STORE 0 TO laForc, lnTforc

SELECT FORCAST
DO CASE
  CASE lnWk = 1                 &&Case for column previous
    RETURN
  CASE lnWk = 10                &&Last column 
    IF GFSEEK(lcStyle)
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
    = GFSEEK(lcStyle + STR(laWekYer(lnWk,2),4) + STR(laWekYer(lnWk,1),2))
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
*-- Search for the selected style as a style Component in another manifactured style
IF !EMPTY(lcMTyp)  && company configration allow style component
  SELECT BOM
  FOR lnNumTyp = 1 to ALEN(lcMTyp)
    DIMENSION laSty(1)
    STORE " " TO laSty
    STORE 1 TO lnElm 
    IF SEEK(lcMTyp(lnNumTyp)+SPACE(23)+"0001"+LEFT(lcStyle,lnMajWidth))  &&lcMTyp is a type of style component for manifatured style
      SCAN REST  WHILE typ+cinvtypc+item+cinvtype+citmmajor+ccstshtyp+ccstsht_id+citmmask = lcMTyp(lnNumTyp)+SPACE(23)+"0001"+LEFT(lcStyle,lnMajWidth)
        IF ASCAN(laSty,Style.style) != 0
          LOOP
        ENDIF
        lcSty = Style.Style
        laSty(lnElm) = Style.Style
        lnElm = lnElm + 1
        DIMENSION laSty(lnElm)
        IF OCCURS('MF',oAriaApplication.CompanyInstalledModules)<>0
          IF SEEK("0001"+LEFT(lcSty,lnMajWidth),"POSHDRS")
            SELECT POSHDRS
            SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO ="0001"+ LEFT(lcSty,lnMajWidth)+SPACE(lnSpcReq)+'PU' FOR Status $ "OA" ;
                 AND &lcCutCondS     
              IF SEEK("M"+PO+lcMTyp(lnNumTyp),"CTKTBOM")
                STORE 0 TO laSComp
                lcCuttkt = PO 
                SELECT CTKTBOM
                SUM (Req_qty1 - Iss_qty1) , (Req_qty2 - Iss_qty2),;
                    (Req_qty3 - Iss_qty3) , (Req_qty4 - Iss_qty4),;
                    (Req_qty5 - Iss_qty5) , (Req_qty6 - Iss_qty6),;
                    (Req_qty7 - Iss_qty7) , (Req_qty8 - Iss_qty8); 
                REST WHILE CIMTYP+CUTTKT +Typ+CINVTYPE+ITEM+MFGCODE+DYELOT = "M" + lcCuttkt + lcMTyp(lnNumTyp);
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

*-- Search for the selected style as a style component in another imported style
IF !EMPTY(lcITyp)
  SELECT BOM
  FOR lnNumTyp = 1 to ALEN(lcITyp)
    DIMENSION laSty(1)
    STORE " " TO laSty
    STORE 1 TO lnElm , lnElm1
    DIMENSION laPo(1)
    STORE " " TO laPo
    IF SEEK(lcITyp(lnNumTyp)+SPACE(23)+"0001"+LEFT(lcStyle,lnMajWidth))  &&lcITyp is a type of style component for imported style
      SCAN REST  WHILE typ+cinvtypc+item+cinvtype+citmmajor+ccstshtyp+ccstsht_id+citmmask = lcITyp(lnNumTyp)+SPACE(23)+"0001"+LEFT(lcStyle,lnMajWidth)
        IF ASCAN(laSty,style.Style) != 0
          LOOP
        ENDIF
        lcSty = Style.Style
        laSty(lnElm) = Style.Style
        lnElm = lnElm + 1
        DIMENSION laSty(lnElm)
        IF OCCURS('PO',oAriaApplication.CompanyInstalledModules)<>0
          IF SEEK("0001"+lcSty,"POSLN")
            SELECT POSLN
            SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD= "0001"+lcSty+'PP' FOR SEEK('PP'+PO,'POSHDR') AND POSHDR.Status = "O";
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
                REST WHILE CIMTYP+CUTTKT +Typ+CINVTYPE+ITEM+MFGCODE+DEYELOT= "I" + lcCuttkt + lcITyp(lnNumTyp);
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
*! Passed Parameters  : LN_CONT
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetHand(LN_CONT) 
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
*! Passed Parameters  : LN_CONT,Style
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetWip(LN_CONT,Style) 
*!**************************************************************************

FUNCTION lfGetWip
PARAMETERS lnWk , lcStyle

STORE 0 TO laTrans,lnTTrans
   
IF STYLE.Make
  IF OCCURS('MF',oAriaApplication.CompanyInstalledModules)<>0
    SELECT POSLN
    IF SEEK("0001"+lcStyle)
      SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD= ;
                 "0001"+lcStyle FOR SEEK('PU'+PO,'POSHDR')AND POSHDR.Status $ "OHA"; 
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
  IF OCCURS('PO',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('MF',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('PS',oAriaApplication.CompanyInstalledModules)<>0 OR OCCURS('RS',oAriaApplication.CompanyInstalledModules)<>0
    SELECT POSLN
    IF SEEK("0001"+lcStyle) 
      SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD= ;
                 "0001"+lcStyle FOR SEEK('PP'+PO,'POSHDR')AND POSHDR.Status $ "OH" ;
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
*! Passed Parameters  : LN_CONT,Style
*!**************************************************************************
*! Returns            : 
*!**************************************************************************
*! Example            : =lfGetAvail(LN_CONT,Style)
*!**************************************************************************

FUNCTION lfGetAvail
PARAMETERS lnWk , lcStyle

STORE 0 TO laAvail,lnTAvail
FOR lnNum = 1 TO 8       
  laAvail(lnNum) = (laTrans(lnNum) + laHand(lnNum)) - ;
                   (laDem(lnNum) + IIF(laForc(lnNum) > 0 ,laForc(lnNum),0))
  lnTAvail = lnTAvail + laAvail(lnNum)
ENDFOR


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

DECLARE laRpSource[3],laRpTarget[1]
lnStatus = lcRpStatus
STORE 'Active'    TO laRpSource[1] 
STORE ''          TO laRpTarget[1]
STORE 'On Hold'   TO laRpSource[2] &&,laRpTarget[2]

STORE 'Cancelled' TO laRpSource[3]
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

= LfOGMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  
  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Active','A',;
                            IIF(laRpTarget[lnI] = 'On Hold','H',;
                            IIF(laRpTarget[lnI] = 'Cancelled','X','')))
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
      ldFDate=CTOD('01/01/'+STR(lnYear,4))   &&Get the date of the first day in the year
      ldLDate=GOMONTH(ldFDate,12) - 1        &&Get the date of the last day in the year
      ldFOgDate  = ldFDate-dow(ldFDate)+1   &&Get the date of the first day in the OGIO year
      ldLOgDate  = ldLDate-dow(ldLDate)     &&Get the date of the last day in the OGIO year
      EXIT
    CASE ldCurDate > ldLOgDate
      lnYear = YEAR(ldCurDate) + 1

      ldFDate=CTOD('01/01/'+STR(lnYear,4))   &&Get the date of the first day in the year
      ldLDate=GOMONTH(ldFDate,12) - 1        &&Get the date of the last day in the year
      ldFOgDate  = ldFDate-dow(ldFDate)+1   &&Get the date of the first day in the OGIO year
      ldLOgDate  = ldLDate-dow(ldLDate)     &&Get the date of the last day in the OGIO year
      EXIT
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
      ldFDate=CTOD('01/01/'+STR(lnYear,4))   &&Get the date of the first day in the year
      ldLDate=GOMONTH(ldFDate,12) - 1        &&Get the date of the last day in the year
      ldFOgDate  = ldFDate-dow(ldFDate)+1   &&Get the date of the first day in the OGIO year
      ldLOgDate  = ldLDate-dow(ldLDate)     &&Get the date of the last day in the OGIO year
      EXIT
    CASE gdSysDate > ldLOgDate
      lnYear = YEAR(gdSysDate) + 1
      ldFDate=CTOD('01/01/'+STR(lnYear,4))   &&Get the date of the first day in the year
      ldLDate=GOMONTH(ldFDate,12) - 1        &&Get the date of the last day in the year
      ldFOgDate  = ldFDate-dow(ldFDate)+1   &&Get the date of the first day in the OGIO year
      ldLOgDate  = ldLDate-dow(ldLDate)     &&Get the date of the last day in the OGIO year
      EXIT

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
PRIVATE LN_CONT 

FOR LN_CONT = 1 TO 9
  laDate(LN_CONT,1) = ldFOgDate + ((lnWeek-1)*7)
  IF LN_CONT = 1
    IF laDate(LN_CONT,1) < gdSysDate
      laDate(LN_CONT,1) = gdSysDate
    ENDIF
  ENDIF   
  laDate(LN_CONT,2) = ldFOgDate + ((lnWeek * 7) - 1)
  laWekYer(LN_CONT+1,1) = lnWeek
  laWekYer(LN_CONT+1,2) = lnYear
  lnWeek = lnWeek  + 1
  IF lnWeek > lnNumWeek
    lnWeek = lfGetWeek(laDate(LN_CONT,2) + 1)
  ENDIF
ENDFOR


*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[23,2]

loOgScroll.lcOGLastForm ='SOFORCST'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcRpTmp+ ".DBF"
  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= 'FORECASTING REPORT'

loOgScroll.laCRParams[2,1] = 'LCBYSIZE'
loOgScroll.laCRParams[2,2]= IIF(lcRpBySt='S','Y','N')

loOgScroll.laCRParams[3,1] = 'LCNEG'
loOgScroll.laCRParams[3,2]= IIF(lcRpNegAll ='N','Y','N')

loOgScroll.laCRParams[4,1] = 'OPTITLE'
loOgScroll.laCRParams[4,2] = lcRpOpTlt 

FOR lnI=2 TO 10
  lcweek='WEEK'+ALLTRIM(STR(lnI))
  loOgScroll.laCRParams[lnI+3,1] = lcweek
  loOgScroll.laCRParams[lnI+3,2]= IIF(lnI=2,"           " ,"/           ")+STR(laWekYer(lnI,1),2)+IIF(lnI=10,"+" ,"    ")
ENDFOR 

FOR lnD=2 TO 10
  LCdate='DATE'+ALLTRIM(STR(lnD))
  loOgScroll.laCRParams[lnD+12,1] = LCdate
  loOgScroll.laCRParams[lnD+12,2]= ALLTRIM(DTOC(laDate(lnD-1,1)))
ENDFOR 

loOgScroll.laCRParams[23,1] = 'LAYOUT'
loOgScroll.laCRParams[23,2] = IIF(lcRpBySt='S','By Size','Total')


*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn


*!*************************************************************
*! Name      : lfStr2Curs 
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs 
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString

