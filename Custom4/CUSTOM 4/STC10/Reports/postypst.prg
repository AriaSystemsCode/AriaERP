*:***************************************************************************
*: Program file  : POSTYPST.PRG
*: Program desc. : CUSTOM PO REPORT FOR STC.
*: Date          : 02/20/2017
*: System        : Aria 4 XP
*: Module        : STYLE PO REPORT(PO)
*: Developer     : SARA OSAMA (SARA.O)
*: Tracking Job #: C202018
*:***************************************************************************
*! B611327,1 Sara.o 06/04/2017 Issue 2: Size among scales [T20170524.0014]
*! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012]
*! B611364,2 MMT 07/27/2017 Fix issue of not printing trim color[T20170718.0012]
*! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003]
*! T-ERP-20240202.0003,1 MMT 02/18/2024 - MELWOOD PARTNERS INC - CHANGE COMPANY NAME ON PO
*:***************************************************************************
lcOldAlias = ALIAS() 
*! T-ERP-20240202.0003,1 MMT 02/18/2024 - MELWOOD PARTNERS INC - CHANGE COMPANY NAME ON PO[Start]
IF oAriaApplication.ActiveCompanyId='02'
  lcCompName ='MELWOOD PARTNERS INC'
ENDIF
*! T-ERP-20240202.0003,1 MMT 02/18/2024 - MELWOOD PARTNERS INC - CHANGE COMPANY NAME ON PO[End]
SELECT POSHDR
set skip to 
oGetMemVar     = CREATEOBJECT("GetMemVar")
PRIVATE llExtSzScl
llExtSzScl = oGetMemVar.DO("M_USEEXSSC",OAriaApplication.ActiveCompanyId)
Declare laItemSeg[1]
oGetItemMask = Createobject("GetItemMask")
llColor = .F.
Store 0 To lnClrLen,lnClrPos
=oGetItemMask.Do(@laItemSeg)
For lnCount = 1 To Alen(laItemSeg,1)
  If laItemSeg[lnCount,1]='C'
    llColor = .T.
    lnClrLen = Len(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
  Endif
  IF llExtSzScl AND laItemSeg[lnCount,1] = 'S'
    lnScaleLn  = LEN(laItemSeg[lnCount,3])
    lnScalePos = laItemSeg[lnCount,4]
  ENDIF
Endfor 
Release oGetItemMask
* Open Tables
If !Used('proflist')
  =gfOpenFile(oAriaApplication.DataDir+'proflist',oAriaApplication.DataDir+'PROFILE','SH')
Endif
If !Used('profvalu')
  =gfOpenFile(oAriaApplication.DataDir+'profvalu',oAriaApplication.DataDir+'PROFILE','SH')
Endif
If !Used('Styleupc')
  =gfOpenFile(oAriaApplication.DataDir+'Styleupc',oAriaApplication.DataDir+'PACKUPC','SH')
Endif
If !Used('cutpick')
  =gfOpentable('cutpick','CUTPICK','SH')
Endif

IF (TYPE("POSLN.UPC01")= 'U')
	SELECT *,CAST (' ' As memo) As cNoteHdr	FROM POShdr Into Cursor POShdr Readwrite
		
	Index on CBUSDOCU+CSTYTYPE+PO tag POSHDR
*! B611327,1 Sara.o 05/30/2017 Issue 2: Size among scales [Begin]
*!*		SELECT *, '              ' As UPC01, '              ' As UPC02, '              ' As UPC03,;
*!*			'              ' As UPC04, '              ' As UPC05, '              ' As UPC06, ;
*!*			'              ' As UPC07, '              ' As UPC08, '                   ' As STYMAJOR, ;
*!*			CAST (' ' As Char(60)) As STYDESC, CAST (' ' As Char(60)) As FABRIC, CAST(' ' As Char(30))  As cBdColor, ;
*!*			'      ' As cBdClrId, '            ' As cTrmClor,'      ' As cTrmClorId, CAST (' ' As Char(60)) As cStkrLg,;
*!*			CAST (' ' As Char(60)) As cMnLbl,CAST (' ' As Char(60)) As cLogoPs, CAST (' ' As Char(60)) As cHngPs,;
*!*			'   ' As LUPC, '   ' As LTRKLBL, CAST(0 As numeric(13, 3)) As TotPrc ,;
*!*			0 As nCount,CAST (' ' As Char(250)) As cSzUPCs,CAST (' ' As Char(250)) As ColStyMj ,CAST (' ' As Char(250)) As ColStyImg1;
*!*			,CAST (' ' As Char(250)) As ColStyImg2,CAST (' ' As Char(250)) As ColStyImg3,CAST (' ' As Char(250)) As ColStyImg4;
*!*			FROM POSLN Into Cursor POSLN Readwrite
    *! B611364,2 MMT 07/27/2017 Fix issue of not printing trim color[T20170718.0012][Start]
*!*		SELECT *, '              ' As UPC01, '              ' As UPC02, '              ' As UPC03,;
*!*			'              ' As UPC04, '              ' As UPC05, '              ' As UPC06, ;
*!*			'              ' As UPC07, '              ' As UPC08, '                   ' As STYMAJOR, ;
*!*			CAST (' ' As Char(60)) As STYDESC, CAST (' ' As Char(60)) As FABRIC, CAST(' ' As Char(30))  As cBdColor, ;
*!*			'      ' As cBdClrId, '              ' As cTrmClor,'      ' As cTrmClorId, CAST (' ' As Char(60)) As cStkrLg,;
*!*			CAST (' ' As Char(60)) As cMnLbl,CAST (' ' As Char(60)) As cLogoPs, CAST (' ' As Char(60)) As cHngPs,;
*!*			'   ' As LUPC, '   ' As LTRKLBL, CAST(0 As numeric(13, 3)) As TotPrc ,;
*!*			CAST (' ' As Char(250)) As cSzUPCs,CAST (' ' As Char(250)) As ColStyMj ,CAST (' ' As Char(250)) As ColStyImg1;
*!*			,CAST (' ' As Char(250)) As ColStyImg2,CAST (' ' As Char(250)) As ColStyImg3,CAST (' ' As Char(250)) As ColStyImg4,SPACE(32) as Sizes,SPACE(32) as qtys;
*!*			FROM POSLN Into Cursor POSLN Readwrite
*! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][Start]
*!*		SELECT *, '              ' As UPC01, '              ' As UPC02, '              ' As UPC03,;
*!*			'              ' As UPC04, '              ' As UPC05, '              ' As UPC06, ;
*!*			'              ' As UPC07, '              ' As UPC08, '                   ' As STYMAJOR, ;
*!*			CAST (' ' As Char(60)) As STYDESC, CAST (' ' As Char(60)) As FABRIC, CAST(' ' As Char(30))  As cBdColor, ;
*!*			'      ' As cBdClrId, SPACE(30) As cTrmClor,SPACE(30) As cTrmClorId, CAST (' ' As Char(60)) As cStkrLg,;
*!*			CAST (' ' As Char(60)) As cMnLbl,CAST (' ' As Char(60)) As cLogoPs, CAST (' ' As Char(60)) As cHngPs,;
*!*			'   ' As LUPC, '   ' As LTRKLBL, CAST(0 As numeric(13, 3)) As TotPrc ,;
*!*			CAST (' ' As Char(250)) As cSzUPCs,CAST (' ' As Char(250)) As ColStyMj ,CAST (' ' As Char(250)) As ColStyImg1;
*!*			,CAST (' ' As Char(250)) As ColStyImg2,CAST (' ' As Char(250)) As ColStyImg3,CAST (' ' As Char(250)) As ColStyImg4,SPACE(32) as Sizes,SPACE(32) as qtys;
*!*			FROM POSLN Into Cursor POSLN Readwrite
	SELECT *, '              ' As UPC01, '              ' As UPC02, '              ' As UPC03,;
		'              ' As UPC04, '              ' As UPC05, '              ' As UPC06, ;
		'              ' As UPC07, '              ' As UPC08, '                   ' As STYMAJOR, ;
		CAST (' ' As Char(60)) As STYDESC, CAST (' ' As Char(60)) As FABRIC, CAST(' ' As Char(30))  As cBdColor, ;
		'      ' As cBdClrId, SPACE(30) As cTrmClor,SPACE(30) As cTrmClorId, CAST (' ' As Char(60)) As cStkrLg,;
		CAST (' ' As Char(60)) As cMnLbl,CAST (' ' As Char(60)) As cLogoPs, CAST (' ' As Char(60)) As cHngPs,CAST (' ' As Char(60)) As cPkMth,;
		'   ' As LUPC, '   ' As LTRKLBL, CAST(0 As numeric(13, 3)) As TotPrc ,;
		CAST (' ' As Char(250)) As cSzUPCs,CAST (' ' As Char(250)) As ColStyMj ,CAST (' ' As Char(250)) As ColStyImg1;
		,CAST (' ' As Char(250)) As ColStyImg2,CAST (' ' As Char(250)) As ColStyImg3,CAST (' ' As Char(250)) As ColStyImg4,SPACE(32) as Sizes,SPACE(32) as qtys;
		FROM POSLN Into Cursor POSLN Readwrite
		
*! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][End]
   *! B611364,2 MMT 07/27/2017 Fix issue of not printing trim color[T20170718.0012][End]	
*! B611327,1 Sara.o 05/30/2017 Issue 2: Size among scales [End]	 	 
	Index On cbusdocu+cstytype+po+cwarecode+account+Store+cinvtype+Style+trancd  Tag POSLNW
	
	SELECT POSLN
		SET RELATION TO POSLN.STYLE INTO STYLE ADDITIVE
		SET RELATION TO 'S' + POSLN.Scale INTO SCALE ADDITIVE
		SET RELATION TO 'S'+SUBSTR(Posln.style,1,lnMajSize) INTO Objlink ADDITIVE
		SET ORDER TO TAG POSLNW
	SELECT POSHDR 
	
 	SET RELATION TO lcRpForm+IIF(lcRpForm $ 'PR','P',lcRpForm)+POSHDR.PO INTO POSLN ADDITIVE
 		
	IF llPrntBoth
	  SELECT POSLN
	  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
	  lcSkipExpr = [POSLN,&lcNoteLns]
	ENDIF
ENDIF

SELECT POSHDR 
SET SKIP TO POSLN

Store '' To lcFbrCd, lcwghtCd,lcTrmClorCd ,lcTrmClorIdCd , lclblCd, lcLblPosCd, lcLogoCd, lcLgPsCd, lcStckCd , lcHntgCd ,lcHntgPsCd
*! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][START]
STORE '' TO lcPackMthCD
*! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][End]
=lfGetPrfCd()

*! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][Start]
SELECT POSHDR 
LOCATE
SCAN
*! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][End]
Select POSLN
LOCATE
*! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][Start]
*SCAN
SCAN FOR PO = POSHDR.PO

  *! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][Start]
  lnNonZeroSz = 0
  FOR x= 1 TO 8
    y = STR(x,1)
    IF POSLN.Qty&Y. > 0
      lnNonZeroSz = lnNonZeroSz + 1  
    ENDIF
  ENDFOR 
  IF lnNonZeroSz > 0 &&AND lnNonZeroSz < 8
    lcSizeStr = ''
    lcQTYStr = ''
    lnSizeSpace = FLOOR(32/lnNonZeroSz)
    FOR x= 1 TO 8
      y = STR(x,1)
      IF POSLN.Qty&Y. > 0
        
        IF !EMPTY(lcSizeStr)
          lcSizeStr = lcSizeStr  + PADL(ALLTRIM(Scale.Sz&Y.),lnSizeSpace)
          lcQTYStr = lcQTYStr + PADL(ALLTRIM(STR(POSLN.QTY&Y.,5,0)),lnSizeSpace)
        ELSE
          lcSizeStr = lcSizeStr  + PADL(ALLTRIM(Scale.Sz&Y.),MAX(LEN(ALLTRIM(Scale.Sz&Y.)),LEN(ALLTRIM(STR(POSLN.QTY&Y.,5,0)))))
          lcQTYStr = lcQTYStr + PADL(ALLTRIM(STR(POSLN.QTY&Y.,5,0)),MAX(LEN(ALLTRIM(Scale.Sz&Y.)),LEN(ALLTRIM(STR(POSLN.QTY&Y.,5,0)))))
        ENDIF  
      ENDIF
    ENDFOR 
    REPLACE Sizes WITH RTRIM(lcSizeStr)
    REPLACE qtys WITH RTRIM(lcQTYStr)
  ENDIF
  *! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][End]
 
*! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][End]
  Store 0 To TotPrc
  *! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][Start]
  *Store '' To lcPrfVlFb , lcPrfWg ,cTrmClor ,cTrmClorId ,lcPrfLbl ,lcLblPos ,lcPrfLgo ,lcPrfLgPs ,lcPrfStck ,lcPrfHtg ,lcPrfHtgPs
  Store '' To lcPrfVlFb , lcPrfWg ,lcTrmClor ,lcTrmClorId ,lcPrfLbl ,lcLblPos ,lcPrfLgo ,lcPrfLgPs ,lcPrfStck ,lcPrfHtg ,lcPrfHtgPs
  *! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][START]
  STORE '' TO lcPackMeth
  *! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][End]
  *! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][End]
  Store " " To lcPrfVlFb ,lcPrfWg ,lcPrfLbl ,lcLblPos , lcPrfLgo ,lcPrfLgPs ,lcPrfStck, lcPrfHtg ,lcPrfHtgPs

  Replace TotPrc With  Round(lfGetLineCost() * POSLN.totqty,2)
  Replace LUPC With Iif(POSHDR.LLPOUPC,'YES','NO')
  Replace LTRKLBL With Iif(POSHDR.LLPOTRKNO, 'YES','NO')

  If !Empty(lcFbrCd) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lcFbrCd   ,'PROFVALU','PROFILE')
    lcPrfVlFb = Alltrim(PROFVALU.Cpro_value)
  Endif
  If !Empty(lcwghtCd) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lcwghtCd ,'PROFVALU','PROFILE')
    lcPrfWg = Alltrim(PROFVALU.Cpro_value)
  Endif
  If !Empty(lcTrmClorCd ) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lcTrmClorCd ,'PROFVALU','PROFILE')
    *! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][Start]
    *cTrmClor = Alltrim(PROFVALU.Cpro_value)
    lcTrmClor = Alltrim(PROFVALU.Cpro_value)
    *! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][End]
  Endif
  If !Empty(lcTrmClorIdCd ) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lcTrmClorIdCd ,'PROFVALU','PROFILE')
    *! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][Start]
    *cTrmClorId = Alltrim(PROFVALU.Cpro_value)
    lcTrmClorId = Alltrim(PROFVALU.Cpro_value)
    *! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][End]
  Endif
  If !Empty(lclblCd ) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lclblCd ,'PROFVALU','PROFILE')
    lcPrfLbl = Alltrim(PROFVALU.Cpro_value)
  Endif
  If !Empty(lcLblPosCd ) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lcLblPosCd ,'PROFVALU','PROFILE')
    lcLblPos = Alltrim(PROFVALU.Cpro_value)
  Endif
  If !Empty(lcLogoCd ) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lcLogoCd ,'PROFVALU','PROFILE')
    lcPrfLgo = Alltrim(PROFVALU.Cpro_value)
  Endif
  If !Empty(lcLgPsCd ) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lcLgPsCd ,'PROFVALU','PROFILE')
    lcPrfLgPs = Alltrim(PROFVALU.Cpro_value)
  Endif
  If !Empty(lcStckCd ) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lcStckCd ,'PROFVALU','PROFILE')
    lcPrfStck = Alltrim(PROFVALU.Cpro_value)
  Endif
  If !Empty(lcHntgCd ) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lcHntgCd ,'PROFVALU','PROFILE')
    lcPrfHtg = Alltrim(PROFVALU.Cpro_value)
  Endif
  If !Empty(lcHntgPsCd  ) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lcHntgPsCd  ,'PROFVALU','PROFILE')
    lcPrfHtgPs = Alltrim(PROFVALU.Cpro_value)
  Endif
  
  *! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][START]
  If !Empty(lcPackMthCD ) And Seek(Padr('POPP'+POSHDR.PO+Padl(Alltrim(Str(POSLN.Lineno)),6,' '),132,' ') + lcPackMthCD,'PROFVALU','PROFILE')
     lcPackMeth =  Alltrim(PROFVALU.Cpro_value)
  Endif
  *! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][End]
  
  
  Replace FABRIC   With lcPrfVlFb +Iif(Empty(lcPrfVlFb ) Or Empty(lcPrfWg ),'',', ')+ lcPrfWg
  *! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][Start]
*!*	  Replace cTrmClor With Alltrim(cTrmClor)
*!*	  Replace cTrmClorId With	Alltrim(cTrmClorId)
  Replace cTrmClor With Alltrim(lcTrmClor)
  Replace cTrmClorId With	Alltrim(lcTrmClorId)
  *! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][End]
  Replace cMnLbl With lcPrfLbl +Iif(Empty(lcPrfLbl ) Or Empty(lcLblPos ),'',', ')+ lcLblPos
  Replace cLogoPs With lcPrfLgo +Iif(Empty(lcPrfLgo ) Or Empty(lcPrfLgPs ),'',', ')+ lcPrfLgPs
  Replace cStkrLg With lcPrfStck +Iif(Empty(lcPrfStck ) Or Empty(lcPrfLgPs ),'',', ')+ lcPrfLgPs
  Replace cHngPs With lcPrfHtg +Iif(Empty(lcPrfHtg ) Or Empty(lcPrfHtgPs ),'',', ')+ lcPrfHtgPs
  Replace cBdClrId With Iif(llColor,Substr(Style,lnClrPos,lnClrLen),'')
  Replace cBdColor With  gfCodDes(cBdClrId  , 'COLOR')
  *! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][START]
  REPLACE cPkMth WITH ALLTRIM(lcPackMeth)
  *! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][End]
  If Seek (Style , 'style','style')
    Replace STYMAJOR With Style.cstymajor
    Replace STYDESC With  Style.Desc
  ENDIF
  *! B611327,1 Sara.o 05/30/2017 Issue 2: Size among scales [Begin]
  *lnCntqt = 0
  *! B611327,1 Sara.o 05/30/2017 Issue 2: Size among scales [End]
  If (POSHDR.LLPOUPC)
    For I = 1 To Scale.Cnt
      UPCno = Alltrim(Str(I))
      If POSLN.qty&UPCno  > 0
      *! B611327,1 Sara.o 05/30/2017 Issue 2: Size among scales [Begin]
        *lnCntqt = lnCntqt + 1
      *! B611327,1 Sara.o 05/30/2017 Issue 2: Size among scales [End]
        Select 'STYLEUPC'
        Locate
        If gfSeek (POSLN.Style+Alltrim(Str(I)),'STYLEUPC','STYLEUPC')
          Select 'PosLn'
          Replace POSLN.UPC0&UPCno  With STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3
        Endif
      Endif
    ENDFOR
    
  Endif
  ****************get Img Path *******Start********
  Replace posln.ColStyImg1 With lfGtStyImg(posln.style) IN posln 
  ****************get Img Path ********End*********
  SET STEP ON 
  *! B611327,1 Sara.o 05/30/2017 Issue 2: Size among scales [Begin]
  *REPLACE POSLN.nCount WITH lnCntqt IN posln 
  *! B611327,1 Sara.o 05/30/2017 Issue 2: Size among scales [End]
  REPLACE POSLN.cSzUPCs WITH 	iif(!empty(posln.upc01),scale.sz1+" :"+posln.upc01+",","")+;
  								iif(!empty(posln.upc02),scale.sz2+" :"+posln.upc02+",","")+;
  								iif(!empty(posln.upc03),scale.sz3+" :"+posln.upc03+",","")+;
  								iif(!empty(posln.upc04),scale.sz4+" :"+posln.upc04+",","")+;
  								iif(!empty(posln.upc05),scale.sz5+" :"+posln.upc05+",","")+;
  								iif(!empty(posln.upc06),scale.sz6+" :"+posln.upc06+",","")+;
  		 						iif(!empty(posln.upc07),scale.sz7+" :"+posln.upc07+",","")+;
  								iif(!empty(posln.upc08),scale.sz8+" :"+posln.upc08,"") IN posln 
  IF empty(ALLTRIM(posln.pack_id))
  *! B611327,1 Sara.o 06/04/2017 Issue 2: Size among scales [Begin]
  	*REPLACE POSLN.ColStyMj WITH posln.STYMAJOR IN posln 
  	REPLACE POSLN.ColStyMj WITH posln.STYMAJOR + scale.scale IN posln 
  *! B611327,1 Sara.o 06/04/2017 Issue 2: Size among scales [End]
  ELSE
  	lcStMjImg = lfGtPkMjrImg (posln.pack_id)
  *! B611327,1 Sara.o 06/04/2017 Issue 2: Size among scales [Begin] 
  	*REPLACE POSLN.ColStyMj WITH lcStMjImg IN posln 
  	REPLACE POSLN.ColStyMj WITH lcStMjImg + scale.scale   IN posln 
  *! B611327,1 Sara.o 06/04/2017 Issue 2: Size among scales [End]
  ENDIF 
ENDSCAN
*! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][Start]
ENDSCAN
*! B611364,1 MMT 07/24/2017 Fix issue of not printing trim color[T20170718.0012][End]
lfGtPoNotes()
If Seek(POSHDR.PO,'cutpick','cutpick')
  POSHDR.LCPOCUST = POSHDR.account
Endif
Select (lcOldAlias)
Set Step On

*!*************************************************************
*! Name      : lfGetPrfCd
*! Developer : Sara Osama
*! Date      : 05/03/2017
*! Purpose   : Get Codes of profile style
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetPrfCd()
*!*************************************************************
Function lfGetPrfCd

Select CODES
lcOrder = Order()
Set Order To IDRLTFNAME   && CDEFCODE+CRLTFIELD+CFLD_NAME

If Seek('N'+'N' + Padr('CPRO_CODE',10,' '))

  Scan Rest While CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N' + Padr('CPRO_CODE',10,' ')

    lcFbrCd = Iif(Alltrim(Upper(cdiscrep)) == 'FABRICATION',cCode_no, lcFbrCd)
    lcwghtCd = Iif(Alltrim(Upper(cdiscrep)) == 'WEIGHT',cCode_no, lcwghtCd)
    lcTrmClorCd = Iif(Alltrim(Upper(cdiscrep)) == 'TRIM COLOR',cCode_no, lcTrmClorCd )
    lcTrmClorIdCd = Iif(Alltrim(Upper(cdiscrep)) == 'COLOR',cCode_no, lcTrmClorIdCd )
    lclblCd = Iif(Alltrim(Upper(cdiscrep)) == 'MAIN LABEL',cCode_no,lclblCd)
    lcLblPosCd = Iif(Alltrim(Upper(cdiscrep)) == 'MAIN LABEL POSITION',cCode_no,lcLblPosCd)
    lcLogoCd =Iif(Alltrim(Upper(cdiscrep)) == 'LOGO',cCode_no,lcLogoCd)
    lcLgPsCd = Iif(Alltrim(Upper(cdiscrep)) == 'LOGO POSITION',cCode_no,lcLgPsCd)
    lcStckCd = Iif(Alltrim(Upper(cdiscrep)) == 'STICKER',cCode_no,lcStckCd )
    lcHntgCd = Iif(Alltrim(Upper(cdiscrep)) == 'HANGTAG',cCode_no,lcHntgCd )
    lcHntgPsCd = Iif(Alltrim(Upper(cdiscrep)) == 'HANGTAG POSITION',cCode_no,lcHntgPsCd  )
    *! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][START]
    lcPackMthCD = Iif(Alltrim(Upper(cdiscrep)) == 'PACKING METHOD',cCode_no,lcPackMthCD)
    *! C202425,1 MMT 06/15/2021 Display 'Packing Method' from Profiles[T20210521.0003][End]
  Endscan
Endif

If !Empty(lcOrder)
  Set Order To &lcOrder. In CODES
ENDIF
*!*************************************************************
*! Name      : lfGtStyImg
*! Developer : Sara Osama
*! Date      : 05/11/2017
*! Purpose   : Get Styles Image path
*!*************************************************************
*! Passed Parameters : Style
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGtStyImg()
*!*************************************************************
FUNCTION lfGtStyImg
PARAMETERS style

STORE "" TO cImgSrc ,lcStyle
Select objlink
lcStyle = Iif(llExtSzScl ,Substr(Style,1,lnscalepos-2),style)
If Seek('DS'+ lcStyle,'objlink','OBJDEFA')
   Select Objects
   If Seek(objlink.cobject_id)
     cImgSrc = Mimgpath
   Endif
ENDIF
RETURN cImgSrc 
*!*************************************************************
*! Name      : lfGtPkMjrImg
*! Developer : Sara Osama
*! Date      : 05/11/2017
*! Purpose   : Get All Styles major and images per pack  
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Passed Parameters : Pack_Id
*!*************************************************************
*! Return      : String of style majors and images concatenated
*!*************************************************************
*! Example     : = lfGtPkMjrImg()
*!*************************************************************
FUNCTION lfGtPkMjrImg 
PARAMETERS Pack_id

If !Used('SPCK_lin')
  =gfOpenFile(oAriaApplication.DataDir+'SPCK_lin',oAriaApplication.DataDir+'SPCK_LIN','SH')
ENDIF
If !Used('SPCK_HDR')
  =gfOpenFile(oAriaApplication.DataDir+'SPCK_HDR',oAriaApplication.DataDir+'SPCK_HDRTP','SH')
ENDIF
cOldAlias = ALIAS()
SELECT SPCK_HDR
LOCATE 
SEEK("P"+pack_id)
SELECT SPCK_LIN
LOCATE 
SEEK("P"+SPCK_HDR.account+Pack_id)
STORE "" TO lcStyMjrs, lcStyImgs 
lcI = 0
SCAN FOR  TYPE = "P" AND ACCOUNT = SPCK_HDR.account AND PACK_ID = Pack_id
	lcStyMjrs = IIF(!EMPTY(lcStyMjrs),lcStyMjrs +",","")+style+","
	lcStyImgs = lfGtStyImg(style)
	lcI = lcI + 1
	if lcI<5
	  lcic = alltrim(str(lci))
      REPLACE POSLN.ColStyImg&lcic  WITH lcStyImgs  IN posln 
    endif 
ENDSCAN 
SELECT (cOldAlias)
RETURN lcStyMjrs 

*!*************************************************************
*! Name        : lfGtPoNotes
*! Developer   : Sara Osama 
*! Date        : 05/15/2017
*! Purpose     : Function to fill the apropriate Note data for report Notes.
*!             : (NotePad) .
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGtPoNotes()
*!*************************************************************
FUNCTION lfGtPoNotes

cOldAls = ALIAS ()
select POSHDR
LOCATE
SCAN 
	lcNoteHdr = ''
	if(SEEK('P' + POSHDR.PO , 'NOTEPAD'))
		lcNoteHdr = ALLTRIM(Notepad.MNotes)
		replace poshdr.cNoteHdr with lcNoteHdr in poshdr 
	endif
ENDSCAN 
select (cOldAls)
RETURN ''


