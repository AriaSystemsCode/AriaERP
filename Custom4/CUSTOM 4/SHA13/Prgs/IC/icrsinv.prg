*!*:**********************************************************************
*: Program file       : ICRSINV.PRG
*: Program description: Custom Export Inventory to Excel (Repspark)
*: Module             : Inventory Control (IC)
*: Developer          : Mostafa Abou Shady (MAA)
*: Tracking Job Number: C201897
*: Date               : 11/23/2016
*:**********************************************************************
*:Modifications:
*:B611248,1 MMT 01/12/2017 Issue#1:Custom RepSpark Export program hangs on SAAS[P20160826.0001]
*:B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001]
*:B611264,1 MMT 02/09/2017 Custom RepSpark Inventory Export does not print in Transit[T20170207.0011]
*:**********************************************************************
=gfopgrid('ICRSINV',.T.)&&,.F.,.F.,.T.,.T.)

*!*************************************************************
*! Name      : lfExportFile
*! Developer : Mostafa Abou Shady (MAA)
*! Date      : 11/23/2016
*! Purpose   : Export File function
*!*************************************************************
FUNCTION lfExportFile
IF EMPTY(lcRpFile) OR !DIRECTORY(JUSTPATH(lcRpFile))
  =gfModalGen('INM00000B00000','','','',"Invalid file path")    
  RETURN .F.
ENDIF
 
IF !USED('Style')
  =gfOpenTable('Style','Style')
ENDIF

IF !USED('Scale')
  =gfOpenTable('Scale','Scale')
ENDIF

IF !USED ('POSHDR')
  =gfOpenTable('POSHDR', 'POSHDR')
ENDIF 

IF !USED ('POSLN')
  =gfOpenTable('POSLN', 'POSLNS')
ENDIF 

*:B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][Start]
IF !USED ('SHPMTHDR')
  =gfOpenTable('SHPMTHDR', 'SHPMTHDR')
ENDIF 
*:B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][End]


PRIVATE lcSqlTempStatmentS,lcStyleFlds,lcTempStyle,lcTempSeason,lcTempDivision,lcTempColor  
STORE .F. TO llSqlErro,lcTempStyle,llStyle,llSeason,llDivision,lcTempSeason,lcTempDivision,llColor 


*STYLE.CSTYMAJOR
lnPOS = ASCAN(loOgScroll.laOGFxFlt,'STYLE.CSTYMAJOR')
IF lnPOS > 0
  lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPOS,1)
  lcTempStyle = loOgScroll.laOGFxFlt[lnPOS,6]
  IF !EMPTY(lcTempStyle) AND USED(lcTempStyle)
    SELECT(lcTempStyle)
    LOCATE 
    IF !EOF()
      llStyle = .T.
    ENDIF
  ENDIF
ENDIF


*SEASON
llUseSeason  = .F.
lnSeaPos = ASCAN(loOgScroll.laOGFxFlt,"STYLE.SEASON")
IF lnSeaPos > 0
  lnSeaPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnSeaPos,1)
  lcSeaSel =IIF(!EMPTY(loOgScroll.laOGFxFlt[lnSeaPos,6]),loOgScroll.laOGFxFlt[lnSeaPos,6],'')
  IF !EMPTY(lcSeaSel)
    lcSeaFile = loOGScroll.gfTempName()
    llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
  ENDIF
ENDIF

*DIVISION
llUseDiv  = .F.
lnDivPos = ASCAN(loOgScroll.laOGFxFlt,"STYLE.CDIVISION")
IF lnDivPos > 0
  lnDivPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnDivPos,1)
  lcDivSel =IIF(!EMPTY(loOgScroll.laOGFxFlt[lnDivPos,6]),loOgScroll.laOGFxFlt[lnDivPos,6],'')
  IF !EMPTY(lcDivSel)
    lcDivFile = loOGScroll.gfTempName()
    llUseDiv = IIF(LEN(lcDivSel)>0,.T.,.F.) AND lfConvertToCursor(lcDivSel,'CDIVISION',lcDivFile)
  ENDIF
ENDIF

*COLOR
llUseColr = .F.
lnClr = ASCAN(loOgScroll.laOGFxFlt,'LcRepColor')
IF lnClr > 0
  lnClr = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnClr,1)
  lcColrSel =IIF(!EMPTY(loOgScroll.laOGFxFlt[lnClr ,6]),loOgScroll.laOGFxFlt[lnClr ,6],'')
  IF !EMPTY(lcColrSel)
    lcColrFile = loOGScroll.gfTempName()
    llUseColr = IIF(LEN(lcColrSel)>0,.T.,.F.) AND lfConvertToCursor(lcColrSel,'CSTYCLR',lcColrFile )
  ENDIF
ENDIF 

STORE 0 TO lnClrLnGl ,lnClrPosGL  
lnMajLen = LEN(gfItemMask("PM"))
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
CREATE CURSOR 'StyList' (ProductNumber C(lnMajLen), ColorCode C(6), GenderCode C(15),SizeCode C(5),AvailableQuantity N(8),AvailableDate D(8), SeasonCode C(6), DivisionCode C(6))
SELECT style

Do Case
Case llStyle
	Select (lcTempStyle)
	Scan
		lcTempStyleNo=Substr(&lcTempStyle..KeyExp,1,lnMajLen)
		Select Style
		If gfSeek(lcTempStyleNo,'Style')
			Select Style
			Scan Rest While Style = lcTempStyleNo For Iif(llUseColr , Seek(Substr(Style,lnClrPosGL,  lnClrLnGl),lcColrFile),.T.) And ;
					IIF(llUseDiv,Seek(Style.cDivision, lcDivFile), .T.) And Iif (llUseSeason, Seek(Style.Season,lcSeaFile), .T.)
				WAIT WINDOW 'Collecting data for Style: '+ Style.Style NOWAIT 
				Scatter Memvar Memo
				m.ProductNumber = lcTempStyleNo
				m.ColorCode = Substr(Style.Style,lnClrPosGL,  lnClrLnGl)
				m.SeasonCode = Style.Season
				m.DivisionCode = Style.cDivision
				m.AvailableDate = oAriaApplication.SystemDate
				= lfGetStyPO(Style.Style)
				If gfSeek('S'+Style.Scale,'Scale')
					For lnD = 1 To Scale.Cnt
					  lcD = Str(lnD,1)
                      m.AvailableDate = oAriaApplication.SystemDate
                      m.SizeCode = Scale.Sz&lcD.
                      m.AvailableQuantity = 0
  				      If (Style.Stk&lcD. - Style.Ord&lcD.) <> 0 Or (Style.Stk&lcD. - Style.Ord&lcD. = 0 And lcRpZero='Y')
						m.AvailableQuantity = Style.Stk&lcD.  - Style.Ord&lcD.
						*B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][Start]
						m.AvailableDate = oAriaApplication.SystemDate
						*B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][End]
						Insert Into StyList From Memvar
		              ENDIF     
					  If Used('StyPO')
						Select 'StyPO'
						Locate
						Scan
						  If StyPO.Date = oAriaApplication.SystemDate AND StyList.ProductNumber = Substr(Style.Style,1,lnMajLen) AND StyList.ColorCode = Substr(Style.Style,lnClrPosGL,  lnClrLnGl) AND StyList.SizeCode = Scale.Sz&lcD.
							Replace StyList.AvailableQuantity With  StyList.AvailableQuantity + StyPO.Qty&lcD.
							m.AvailableQuantity = StyList.AvailableQuantity
						  Else
							m.AvailableQuantity = m.AvailableQuantity +  StyPO.Qty&lcD.
							m.AvailableDate = StyPO.Date
							Insert Into StyList From Memvar
						  Endif
						Endscan
				      Endif
					Endfor
				  Endif
				  Select Style
  			    Endscan
  		      Endif
        	Endscan
  Otherwise
	Select Style
	Scan For  Iif(llUseColr , Seek(Substr(Style,lnClrPosGL,  lnClrLnGl),lcColrFile),.T.) And ;
			IIF(llUseDiv,Seek(Style.cDivision, lcDivFile), .T.) And Iif (llUseSeason, Seek(Style.Season,lcSeaFile), .T.)
		lcTempStyleNo=Substr(Style.Style,1,lnMajLen)
		WAIT WINDOW 'Collecting data for Style: '+ Style.Style NOWAIT 
		Scatter Memvar Memo
		m.ProductNumber = lcTempStyleNo
		m.ColorCode = Substr(Style.Style,lnClrPosGL,  lnClrLnGl)
		m.SeasonCode = Style.Season
		m.DivisionCode = Style.cDivision
		m.AvailableDate = oAriaApplication.SystemDate
		= lfGetStyPO(Style.Style)
		If gfSeek('S'+Style.Scale,'Scale')
			For lnD = 1 To Scale.Cnt
				lcD = Str(lnD,1)
				m.SizeCode = Scale.Sz&lcD.
				m.AvailableQuantity = 0
				*B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][Start]
				m.AvailableDate = oAriaApplication.SystemDate
				*B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][End]

				If (Style.Stk&lcD. - Style.Ord&lcD.) <> 0 Or (Style.Stk&lcD. - Style.Ord&lcD. = 0 And lcRpZero='Y')
					m.AvailableQuantity = Style.Stk&lcD.  - Style.Ord&lcD.
					*B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][Start]
					m.AvailableDate = oAriaApplication.SystemDate
					*B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][End]
					Insert Into StyList From Memvar
                ENDIF     
					If Used('StyPO')
						Select 'StyPO'
						Locate
						Scan
							If StyPO.Date = oAriaApplication.SystemDate AND StyList.ProductNumber = Substr(Style.Style,1,lnMajLen) AND StyList.ColorCode = Substr(Style.Style,lnClrPosGL,  lnClrLnGl) AND StyList.SizeCode = Scale.Sz&lcD.
								Replace StyList.AvailableQuantity With  StyList.AvailableQuantity + StyPO.Qty&lcD.
								m.AvailableQuantity = StyList.AvailableQuantity
							Else
								m.AvailableQuantity = m.AvailableQuantity +  StyPO.Qty&lcD.
								m.AvailableDate = StyPO.Date
								Insert Into StyList From Memvar
							Endif
						Endscan
					Endif
				

			Endfor
		Endif
		Select Style
	Endscan
Endcase
SELECT StyList 
LOCATE 
IF EOF()
   =gfModalGen('TRM00000B00000','','','',"There is No records to export")    
   RETURN .F.
ENDIF

Local lcFile
SELECT StyList
lcFile = lcRpFile
*:B611248,1 MMT 01/12/2017 Issue#1:Custom RepSpark Export program hangs on SAAS[P20160826.0001][Start]
*!*	COPY TO &lcFile TYPE XL5

*!*	=gfModalGen('INM00000B00000','','','',"File "+alLt(lcRpFile)+" has been created successfully.")    
*!*	DECLARE INTEGER ShellExecute IN shell32.DLL ;
*!*	    INTEGER hndWin, STRING cAction,  STRING cFileName

*!*	ShellExecute(0 ,'open' ,lcFile )
lcTmpFile = oAriaApplication.workDir+gfTempName()+".XLS" 
COPY TO &lcTmpFile TYPE XL5

*:B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][Start]
lcXTmpFile = FORCEEXT(lcTmpFile ,".XLSX")
oExcelSheet = CREATEOBJECT('Excel.Application') 
loFileExcel =oExcelSheet.Workbooks.Open (lcTmpFile)
loSheet = loFileExcel.Sheets [1]
loSheet.Range ("A1")= "ProductNumber"
loSheet.Range ("B1")= "ColorCode"
loSheet.Range ("C1")= "GenderCode"
loSheet.Range ("D1")= "SizeCode"
loSheet.Range ("E1")= "AvailableQuantity"
loSheet.Range ("F1")= "AvailableDate"
loSheet.Range ("G1")= "SeasonCode"
loSheet.Range ("H1")= "DivisionCode"
loFileExcel.SaveAs(lcXTmpFile,51)
loFileExcel.Close (.t.)
oExcelSheet = NULL
loFileExcel = NULL
lcTmpFile  = lcXTmpFile
*:B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][End]

IF FILE(lcTmpFile)
  COPY FILE (lcTmpFile) TO (lcFile)
  =gfModalGen('INM00000B00000','','','',"File "+alLt(lcRpFile)+" has been created successfully.")    
ENDIF
*:B611248,1 MMT 01/12/2017 Issue#1:Custom RepSpark Export program hangs on SAAS[P20160826.0001][End]

ENDFUNC 
*!*************************************************************
*! Name      : lfvHFile
*! Developer : Mostafa Abou Shady (MAA)
*! Date      : 11/23/2016
*! Purpose   : Validate File Path
*!*************************************************************
FUNCTION lfvHFile
IF '?' $ lcRpFile
  *:B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][Start]
  *lcRpFile = GETFILE('XLS','Select File Path/Name','Create')
  lcRpFile = GETFILE('XLSX','Select File Path/Name','Create')
  *:B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][End]
ENDIF

IF !DIRECTORY(JUSTPATH(lcRpFile))
  =gfModalGen('INM00000B00000','','','',"Invalid file path")    


ENDIF 
ENDFUNC 
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : Mostafa Abou Shady (MAA)
*: Date      : 11/23/2016
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************

FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName

DO CASE

CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

  CASE ALLTRIM(lcFieldName) = 'CDIVISION'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0

  CASE ALLTRIM(lcFieldName) = 'CSTYCLR'
   laTempacstru[1,2]='C'
   laTempacstru[1,3]= 6
   laTempacstru[1,4]= 0
ENDCASE
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO
  IF lnEnd = 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH lcValuesToConvert
  ENDIF
ENDIF
RETURN .T.
*!*************************************************************
*! Name      : lfGetStyPO
*: Developer : Mostafa Abou Shady (MAA)
*: Date      : 11/23/2016
*! Purpose   : Get Style PO List
*!*************************************************************
FUNCTION lfGetStyPO
LPARAMETERS lcStyle
IF USED('StyPO')
  USE IN 'StyPO'
ENDIF
CREATE CURSOR 'StyPO' (date D(8),Qty1 N(8),Qty2 N(8),Qty3 N(8),Qty4 N(8),Qty5 N(8),Qty6 N(8),Qty7 N(8),Qty8 N(8))
SELECT 'StyPO'
INDEX ON DTOS(date) TAG 'StyPO'
IF !USED('POSLN_Qty')
  =gfOpenTable('POSLN','POSLN','SH','POSLN_Qty')
ENDIF
SELECT POSLN
IF gfSeek('0001'+lcStyle+'PP','POSLN')
  SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = '0001'+lcStyle+'PP' FOR Trancd ='1'    
  	=gfSeek('PP'+POSLN.PO,'POSHDR','POSHDR')
  	*:B611264,1 MMT 02/09/2017 Custom RepSpark Inventory Export does not print in Transit[T20170207.0011][Start]
  	*IF POSHDR.STATUS $ 'CXS' OR POSHDR.Available < oAriaApplication.SystemDate
  	IF POSHDR.STATUS $ 'CXS'
  	*:B611264,1 MMT 02/09/2017 Custom RepSpark Inventory Export does not print in Transit[T20170207.0011][End]
  	  LOOP
  	ENDIF
  	=gfSeek('PP'+POSLN.PO+'0001'+POSLN.Style+STR(POSLN.LINENO,6),'POSLN_Qty')
  	SELECT POSLN_Qty
  	SUM IIF(TRANCD = '1',1,-1)* Qty1,;
  	    IIF(TRANCD = '1',1,-1)* Qty2,;
  	    IIF(TRANCD = '1',1,-1)* Qty3,;
  	    IIF(TRANCD = '1',1,-1)* Qty4,;
  	    IIF(TRANCD = '1',1,-1)* Qty5,;
  	    IIF(TRANCD = '1',1,-1)* Qty6,;
  	    IIF(TRANCD = '1',1,-1)* Qty7,;
  	    IIF(TRANCD = '1',1,-1)* Qty8 TO Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8 REST WHILE ;
  	    CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD = 'PP'+POSLN.PO+'0001'+POSLN.Style+STR(POSLN.LINENO,6) FOR Trancd <> '3'                                                    
  	 *B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][Start]
  	 *IF !SEEK(DTOS(POSHDR.Available), 'StyPO', 'StyPO')   
  	 lcDateAvl = DTOS(POSHDR.Available)
  	 ldDataAvl = POSHDR.Available
  	 IF gfSeek('PP'+POSLN.PO+'0001'+POSLN.Style+STR(POSLN.LINENO,6)+'3','POSLN_Qty')
  	   IF gfSeek('PP'+POSLN_Qty.SHIPNO,'SHPMTHDR','SHPMTHDR')
  	     lcDateAvl = DTOS(SHPMTHDR.ETA)
  	     ldDataAvl = SHPMTHDR.ETA
  	   ENDIF
  	 ENDIF
  	 *:B611264,1 MMT 02/09/2017 Custom RepSpark Inventory Export does not print in Transit[T20170207.0011][Start]
  	 IF ldDataAvl  < oAriaApplication.SystemDate
  	   LOOP 
  	 ENDIF
  	 *:B611264,1 MMT 02/09/2017 Custom RepSpark Inventory Export does not print in Transit[T20170207.0011][End]
  	 IF !SEEK(lcDateAvl , 'StyPO', 'StyPO') 
  	 *B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][End]
       SELECT  'StyPO'
       APPEND BLANK  
       *B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][Start]      
       *REPLACE Date WITH POSHDR.Available
       REPLACE Date WITH ldDataAvl
       *B611251,1 MMT 01/22/2017 Custom export inventory for RepSpark issues 2,3,4,5[P20160826.0001][End]
  	 ENDIF
  	 SELECT  'StyPO'
 	 REPLACE Qty1 WITH Qty1+Q1,;
		 	 Qty2 WITH Qty2+Q2,;
		 	 Qty3 WITH Qty3+Q3,;
		 	 Qty4 WITH Qty4+Q4,;
		 	 Qty5 WITH Qty5+Q5,;
		 	 Qty6 WITH Qty6+Q6,;
		 	 Qty7 WITH Qty7+Q7,;
		 	 Qty8 WITH Qty8+Q8
	 SELECT POSLN
  ENDSCAN
ENDIF

*:B611248,1 MMT 01/12/2017 Issue#1:Custom RepSpark Export program hangs on SAAS[P20160826.0001][Start]
*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/12/2017
*! Purpose   : OG When function
*!*************************************************************
FUNCTION lfwRepWhen
*:B611248,1 MMT 01/12/2017 Issue#1:Custom RepSpark Export program hangs on SAAS[P20160826.0001][End]