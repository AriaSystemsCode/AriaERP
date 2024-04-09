*:***************************************************************************
*: Program file  : POSTYPBC.PRG
*: Program desc. : ADD UPCs AND RETAIL PRICE FOR PO REPORT.
*: Date          : 02/20/2017
*: System        : Aria 4 XP
*: Module        : STYLE PO REPORT(PO)
*: Developer     : SARA OSAMA (SARA.O)
*: Tracking Job #: C201948
*:***************************************************************************
lc11 = ALIAS()
IF (Type("POSLN.UPC01") = 'U')
	Select *, Cast(" " As C(14) ) As UPC01, Cast(" " As C(14) ) As UPC02, Cast(" " As C(14) ) As UPC03,;
		CAST(" " As C(14) ) As UPC04, Cast(" " As C(14) ) As UPC05, Cast(" " As C(14) ) As UPC06,;
		CAST(" " As C(14) ) As UPC07, Cast(" " As C(14) ) As UPC08, Cast(" " As N(12,2) ) As RtlPrice;
		FROM POSLN Into Cursor POSLN Readwrite
	Index On cbusdocu+cstytype+po+cwarecode+account+Store+cinvtype+Style+trancd  Tag POSLNW
	SELECT POSLN
		SET RELATION TO POSLN.STYLE INTO STYLE ADDITIVE
		SET RELATION TO 'S' + POSLN.Scale INTO SCALE ADDITIVE
		SET RELATION TO 'S'+SUBSTR(Posln.style,1,lnMajSize) INTO Objlink ADDITIVE
		SET ORDER TO TAG POSLNW
	SELECT POSHDR
		SET RELATION TO lcRpForm+IIF(lcRpForm $ 'PR','P',lcRpForm)+POSHDR.PO INTO POSLN ADDITIVE
  		SET SKIP TO POSLN
  	IF llPrntBoth
	  SELECT POSLN
	  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
	  lcSkipExpr = [POSLN,&lcNoteLns]
	ENDIF
ENDIF
IF (llRpSize) And (llRpStlRtlPrc Or llRpStlUPC)
	Select POSLN
	Set Order To Tag POSLNW
	If !Used('styleupc')
		=gfOpenTable("styleupc","styleupc")
	Endif
	If !Used('scale')
		=gfOpenTable("scale","scale")
	Endif
	If !Used('style')
		=gfOpenTable("style","style")
	ENDIF
	lfGetStUPCRtlPrc()	
ENDIF
SELECT poshdr
*!*	SET RELATION TO POSLNW INTO POSLN
LOCATE  
************************************************************
*! Name      : lfGetStUPCRtlPrc
*! Developer : Sara Osama (SARA.O)
*! Date      : 02/20/2017
*! Purpose   : Get style UPCs and Retail Price
************************************************************
Function lfGetStUPCRtlPrc
Select 'Posln'
Locate
Scan Rest For !Empty(POSLN.Style)
	Select 'style'
	Locate
	If (llRpStlRtlPrc) And gfSeek (POSLN.Style,'style','style')
		Select 'Posln'
		Replace POSLN.RtlPrice With Style.Nsugretpri
	Endif
	If (llRpStlUPC)
		Select 'scale'
		Locate
		If gfSeek ('S'+POSLN.Scale,'scale','scale')
			For I = 1 To Scale.Cnt
				Select 'STYLEUPC'
				Locate
				If gfSeek (POSLN.Style+Alltrim(Str(I)),'STYLEUPC','STYLEUPC')
					Select 'Posln'
					UPCno = Alltrim(Str(I))
					Replace POSLN.UPC0&UPCno  With STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3
				Endif
			Endfor
		Endif
	Endif
ENDSCAN
_screen.Visible = .T.
SET STEP ON 