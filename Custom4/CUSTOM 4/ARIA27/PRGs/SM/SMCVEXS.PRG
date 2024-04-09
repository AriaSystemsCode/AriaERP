*:***************************************************************
*: Name      : SMCvExS.prg					  	   *E500257,1
*: Developer : Timour Abdalla Khalil (TAK).
*: Date      : 03/15/99
*: Purpose   : Extended size scale conversion.
*:***************************************************************
*: Called from : CNVMAIN.PRG    if SYCNVSCL.DBF exist.
*:***************************************************************
*B802108,1 TAK 03/31/99 added to check the !empty styles and also update STYLEUPC. 
*B804514,1 ADEL 11/11/01 Fix the bug of not replacing the scale code in the style field
*B804514,1               in OrdlIne,InvLine ...etc.
*:***************************************************************
LCMOD_ID='Extended Size Scale Conversion'
SHOW GET lcMod_Id

*--Setup the conversion error handler.
lcErr1=ON('ERROR')
ON ERROR llDumy = lfUpdError('E.S.S.C.-> File :'+ALIAS()+'('+MESSAGE()+')')


*--Open system conversion of ext size scale.
=gfOpenFile(gcSysHome+'SYCNVSCL','CExtScale','SH')   && Custom Extended scale library.

STORE ' ' TO lcD1Thrm,lcD1ThPrc,lcD2Thrm,lcD2ThPrc,lcD3Thrm,lcD3ThPrc,lcmfledesc
lnMax3   = 4 
lnCurNm3 = 1
SHOW GET lcMfleDesc
SHOW GET lcD1Thrm
SHOW GET lcD1ThPrc
SHOW GET lcD2Thrm
SHOW GET lcD2ThPrc
SHOW GET lcD3Thrm
SHOW GET lcD3ThPrc


*--1) Update SCALE,SCALEHD,ICISTRU,SETUPS...........
=lfUSclInfo()


*--2) Color key change..............................
=lfUClrInfo()

*lcTmpSMap = gfTempName()


*--3) Style key change and create a styles..........
=lfUStyInfo()


*--4) Key Change....................................
=lfKeyChang()


IF USED('SYCNVSCL')
  USE IN SYCNVSCL
ENDIF
ON ERROR &lcErr1
RETURN




*:*************************************************************
*: Name      : lfUSclInfo
*: Developer : Timour A. K.
*: Date      : 03/15/99
*: Purpose   : Convert scales.
*:*************************************************************
*: Called From  : SMCvExS
*:*************************************************************
FUNCTION lfUSclInfo

*-Update 4 files.
STORE ' ' TO lcD1Thrm,lcD1ThPrc,lcD2Thrm,lcD2ThPrc
STORE 1   TO lnCurNm1,lnCurNm2

lcD1Thrm = 'Updating Scales...'
SHOW GET lcD1Thrm
SHOW GET lcD1ThPrc
*--
SHOW GET lcD2Thrm
SHOW GET lcD2ThPrc
lnMax2 = 4


*--Scale header update.
IF USED('SCALEHD')
  USE IN SCALEHD
  llOpend=.F.
ELSE
  llOpend =.T.
ENDIF
=gfOpenFile(gcDataDir+'SCALEHD','','EX')
=lfESSThrm()
SELECT SCALEHD
ZAP
SELECT SYCNVSCL
SCAN
  SELECT SCALEHD
  LOCATE FOR CEXTSCALE = SUBSTR(SYCNVSCL.SCALE,1,1)
  IF !FOUND()
    APPEND BLANK
    REPLACE CEXTSCALE WITH SUBSTR(SYCNVSCL.SCALE,1,1),;
            CSCALEDES WITH SYCNVSCL.CSCALEDES,;
            NNOOFDIM  WITH IIF(EMPTY(SYCNVSCL.CDIM2),1,2),;
            CDIM1DESC WITH SYCNVSCL.CDIM1DESC,;
            CDIM2DESC WITH SYCNVSCL.CDIM2DESC,;
            CDIM3DESC WITH '' ,;
            nNofcodes WITH 1
  ELSE
    REPLACE nNofcodes WITH nNofcodes+1
  ENDIF
ENDSCAN

IF llOpend
  USE IN SCALEHD
ENDIF


*--Scale file.  
llOpend = gfOpenFile(gcDataDir+'SCALE','','SH')
=lfESSThrm()
SELECT SCALE
*TTTT
DELETE ALL FOR !SEEK(ALLT(SCALE),'SYCNVSCL')

GO TOP
SCAN
  SELECT SYCNVSCL
  =SEEK(ALLT(SCALE.Scale))
  lnCntt = IIF(EMPTY(SZ2),1,IIF(EMPTY(SZ3),2,IIF(EMPTY(SZ4),3,IIF(EMPTY(SZ5),4,IIF(EMPTY(SZ6),5,IIF(EMPTY(SZ7),6,IIF(EMPTY(SZ8),7,8)))))))
  SELECT SCALE
  =RLOCK()
  REPLACE Scale     WITH SYCNVSCL.SCALE,;
          cScl_Desc WITH SYCNVSCL.cScl_Desc,;
          Cnt       WITH lnCntt,;
          cDim1     WITH SYCNVSCL.cDim1,;
          cDim2     WITH SYCNVSCL.cDim2,;
          SZ1       WITH SYCNVSCL.SZ1,;
          SZ2       WITH SYCNVSCL.SZ2,;  
          SZ3       WITH SYCNVSCL.SZ3,;    
          SZ4       WITH SYCNVSCL.SZ4,;  
          SZ5       WITH SYCNVSCL.SZ5,;  
          SZ6       WITH SYCNVSCL.SZ6,;  
          SZ7       WITH SYCNVSCL.SZ7,;  
          SZ8       WITH SYCNVSCL.SZ8
  UNLOCK
  SELECT SCALE
ENDSCAN

*--Create record for special scale divided scale 'Q'
SELECT SYCNVSCL
IF SEEK('Q')
  SKIP
ENDIF
IF !EOF() AND CEXTSCALE = 'Q'  
  lnCntt = IIF(EMPTY(SZ2),1,IIF(EMPTY(SZ3),2,IIF(EMPTY(SZ4),3,IIF(EMPTY(SZ5),4,IIF(EMPTY(SZ6),5,IIF(EMPTY(SZ7),6,IIF(EMPTY(SZ8),7,8)))))))
  SELECT SCALE
  APPEND BLANK
  REPLACE Type      WITH 'S',;
          Scale     WITH SYCNVSCL.SCALE,;
          cScl_Desc WITH SYCNVSCL.cScl_Desc,;
          Cnt       WITH lnCntt,;
          cDim1     WITH SYCNVSCL.cDim1,;
          cDim2     WITH SYCNVSCL.cDim2,;
          SZ1       WITH SYCNVSCL.SZ1,;
          SZ2       WITH SYCNVSCL.SZ2,;  
          SZ3       WITH SYCNVSCL.SZ3,;    
          SZ4       WITH SYCNVSCL.SZ4,;  
          SZ5       WITH SYCNVSCL.SZ5,;  
          SZ6       WITH SYCNVSCL.SZ6,;  
          SZ7       WITH SYCNVSCL.SZ7,;  
          SZ8       WITH SYCNVSCL.SZ8
ENDIF

IF llOpend
  USE IN SCALE
ENDIF


*--Style Structure File.
IF USED('ICISTRU')
  USE IN ICISTRU
  llOpend=.F.
ELSE
  llOpend =.T.
ENDIF
=gfOpenFile(gcDataDir+'ICISTRU','','EX')
=lfESSThrm()
SELECT ICISTRU
DELETE FOR CITEMRECTY = 'U'
PACK

APPEND BLANK
REPLACE cItemRecTy WITH 'U',;
        cIsegno    WITH '1',; 
        nISegSize  WITH 8,;
        cISegLDes  WITH 'Style  ',;
        cIsegSDes  WITH 'Style  ',;
        cISegSepr  WITH '-',;
        cISegHead  WITH 'Style   -Color /Scl',;
        cISegType  WITH 'F',;
        lSegEndMaj WITH .T.,;
        cAdd_user  WITH 'CONVERSION'
APPEND BLANK
REPLACE cItemRecTy WITH 'U',;
        cIsegno    WITH '2',; 
        nISegSize  WITH 6,;
        cISegLDes  WITH 'Color  ',;
        cIsegSDes  WITH 'Color  ',;
        cISegSepr  WITH '/',;
        cISegHead  WITH '',;
        cISegType  WITH 'C',;
        lSegEndMaj WITH .F.,;
        cAdd_user  WITH 'CONVERSION'
APPEND BLANK
REPLACE cItemRecTy WITH 'U',;
        cIsegno    WITH '3',; 
        nISegSize  WITH 3,;
        cISegLDes  WITH 'Scale  ',;
        cIsegSDes  WITH 'Scl  ',;
        cISegSepr  WITH '',;
        cISegHead  WITH '',;
        cISegType  WITH 'S',;
        lSegEndMaj WITH .F.,;
        cAdd_user  WITH 'CONVERSION'
IF llOpend
  USE IN ICISTRU
ENDIF


*--Setup update.
llOpend=gfOpenFile(gcDataDir+'SETUPS'  ,'Modvar','SH')
=lfESSThrm()
SELECT SETUPS
IF SEEK('IC'+'M_USEEXSSC')
  =RLOCK() 
  REPLACE mData_Def WITH ".T."
  UNLOCK
ENDIF
IF llOpend
  USE IN SETUPS
ENDIF

=lfThrmo(lnCurNm3,lnMax3,'3')
RETURN




*:*************************************************************
*: Name      : lfUClrInfo
*: Developer : Timour A. K.
*: Date      : 03/15/99
*: Purpose   : Convert colors.
*:*************************************************************
*: Called From  : SMCvExS
*:*************************************************************
FUNCTION lfUClrInfo

*--Start Color Key Change.
STORE ' ' TO lcD1Thrm,lcD1ThPrc,lcD2Thrm,lcD2ThPrc
STORE 1   TO lnCurNm1,lnCurNm2

lcD1Thrm = 'Updating Colors...'
SHOW GET lcD1Thrm
SHOW GET lcD1ThPrc

*--
lcMFleDesc = 'CODES '
SHOW GET lcMFleDesc
SHOW GET lcD2Thrm
SHOW GET lcD2ThPrc

llCodsOpn = !USED('CODES')

IF !llCodsOpn
  USE IN CODES
ENDIF
=gfOpenFile(gcDataDir+'CODES','Ccode_no','EX')
SELECT CODES
SEEK 'N'+'COLOR '
DELETE REST WHILE CDEFCODE+CFLD_NAME='N'+'COLOR '
SEEK 'D'+'COLOR '
DELETE REST WHILE CDEFCODE+CFLD_NAME='D'+'COLOR '
PACK


*--Update the codes file with the new colors after deleting the old (the one with scale).
*--Open 2.6 code file.
IF USED('ACODE')
  USE IN ACODE
ENDIF  
SELECT 0
USE (lcA26Data+'CODE') SHARED ALIAS 'ACODE'
COUNT ALL TO lnMax2 FOR Code = 'C'
LOCATE FOR Code = 'C' AND !EMPTY(SUBSTR(ACODE.Code,2,5))

*--Create default color.
SELECT CODES
APPEND BLANK
REPLACE CDEFCODE  WITH 'D',;
        CFLD_NAME WITH 'COLOR     ',;
        CCODE_NO  WITH SUBSTR(ACODE.Code,2,5),;
        CDISCREP  WITH IIF(EMPTY(ACODE.cData),SUBSTR(ACODE.Code,2,5),ACODE.cData),;
        CRLTFIELD WITH 'N'

SELECT ACODE
SCAN FOR Code = 'C'
  IF !SEEK('N'+'COLOR     '+SUBSTR(CODE,2,5),'CODES')
    SELECT CODES
    *--Create color main record.
    APPEND BLANK
    REPLACE CDEFCODE  WITH 'N',;
            CFLD_NAME WITH 'COLOR     ',;
            CCODE_NO  WITH SUBSTR(ACODE.Code,2,5),;
            CDISCREP  WITH IIF(EMPTY(ACODE.cData),SUBSTR(ACODE.Code,2,5),ACODE.cData),;
            CRLTFIELD WITH 'N'
    *--Create color releted fields record 'CLRLNAME'.
    APPEND BLANK
    REPLACE CDEFCODE  WITH 'N',;
            CFLD_NAME WITH 'COLOR     ',;
            CCODE_NO  WITH SUBSTR(ACODE.Code,2,5),;
            CRLTFIELD WITH 'Y',;
            CRLTD_NAM WITH 'CLRLNAME',;
            CRLTD_TYP WITH 'C',;
            CRLTD_VLU WITH ACODE.CLRLNAME
    *--Create color releted fields record 'CNRFCODE'.
    APPEND BLANK
    REPLACE CDEFCODE  WITH 'N',;
            CFLD_NAME WITH 'COLOR     ',;
            CCODE_NO  WITH SUBSTR(ACODE.Code,2,5),;
            CRLTFIELD WITH 'Y',;
            CRLTD_NAM WITH 'CNRFCODE',;
            CRLTD_TYP WITH 'C',;
            CRLTD_VLU WITH ACODE.CNRFCODE
  ENDIF

  *--Thermometers.
  =lfThrmo(lnCurNm2,lnMax2,'2')
ENDSCAN
*--Close codes file if opend.
IF llCodsOpn AND USED('CODES')
  USE IN CODES
ENDIF
IF USED('ACODE')
  USE IN ACODE
ENDIF  
=lfThrmo(lnCurNm3,lnMax3,'3')
RETURN


*:*************************************************************
*: Name      : lfUStyInfo
*: Developer : Timour A. K.
*: Date      : 03/15/99
*: Purpose   : Convert styles.
*:*************************************************************
*: Called From  : SMCvExS
*:*************************************************************
FUNCTION lfUStyInfo

STORE ' ' TO lcD1Thrm,lcD1ThPrc,lcD2Thrm,lcD2ThPrc
STORE 1   TO lnCurNm1,lnCurNm2

lcD1Thrm = 'Updating Styles...'
SHOW GET lcD1Thrm
SHOW GET lcD1ThPrc

*--
lcMFleDesc = 'STYLE '
SHOW GET lcMFleDesc
SHOW GET lcD2Thrm
SHOW GET lcD2ThPrc


*--Create style tmp file.
lcTmpStyl = gfTempName()

llStyOpns=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')

*--Check if no doplication on style.
*SELECT STYLE
*SET FILTER TO LEN(ALLT(CSTYMAJOR))>8
*GO TOP
*IF !EOF()
*  COPY ALL TO (gcWorkDir+lcTmpSMap)
*  =gfOpenFile(gcWorkDir+lcTmpSMap,'','EX')
*  SELECT (lcTmpSMap)
*  INDEX ON CSTYMAJOR TAG (lcTmpSMap) UNIQUE
*  SET ORDER TO TAG (lcTmpSMap)

*  REPLACE ALL DESC WITH SUBSTR(STYLE,1,8)
*  GO TOP
*  lcStMjr = Desc
*  lnI = 1
*  SCAN
*    lcZ = ALLT(STR(lnI,3))
*    IF Desc <> lcStMjr
*      lcStMjr = Desc
*      lnI = 1
*    ENDIF
*    REPLACE STYLE WITH PADR(STYLE,8-LEN(lcZ))+lcZ
*    lnI = lnI + 1
*  ENDSCAN
*  llLongSty = .T.
*ENDIF

SELECT STYLE
*SET FILTER TO
COPY STRU TO (gcWorkDir+lcTmpStyl)
=gfOpenFile(gcWorkDir+lcTmpStyl,'','EX')
SELECT (lcTmpStyl)
INDEX ON STYLE TAG (lcTmpStyl)


*--Update segment value file.
llCodsOpn = !USED('ICSEGVAL')
IF !llCodsOpn
  USE IN ICSEGVAL
ENDIF
=gfOpenFile(gcDataDir+'ICSEGVAL','Segval','EX')
SELECT ICSEGVAL
ZAP
*--Create a styles record for the scales that does not exist in the main extended scale.
llScleOpn = gfOpenFile(gcDataDir+'SCALE','SCALE','SH')


SELECT STYLE
lnCurNm2=1
lnMax2  =RECCOUNT()
SET ORDER TO 
SCAN

  =RLOCK()
  REPLACE Style     WITH lfvSty(Style),;
          cStyMajor WITH PADR(Style,8),;
          Scale     WITH SUBSTR(Style,17,3),;
          Desc1     WITH ALLT(Desc1) +' - '+IIF(SEEK('S'+SCALE,'SCALE'),ALLT(SCALE.CSCL_DESC),'')
  UNLOCK
  *tttt
  IF 'SKIPPED' $ Style
    REPLACE Scale WITH SPACE(3)
    *--Thermometers.
    =lfThrmo(lnCurNm2,lnMax2,'2')
    LOOP
  ENDIF
  
  SELECT ICSEGVAL
  IF !SEEK('1'+STYLE.CSTYMAJOR)
    APPEND BLANK
    REPLACE CISEGNO   WITH '1',;
            CISEGVAL  WITH STYLE.CSTYMAJOR,;
            CISGVALSD WITH STYLE.Desc,;
            CISGVALLD WITH STYLE.Desc1
  ENDIF

  *--Thermometers.
  =lfThrmo(lnCurNm2,lnMax2,'2')
  
ENDSCAN
*tttt
SELECT STYLE
DELETE ALL FOR EMPTY(Scale)

lnCurNm2=1
SELECT STYLE
SET ORDER TO TAG Style
SELECT 0
USE (gcDataDir+'STYLE') AGAIN ALIAS 'StyScan' SHARED
SET ORDER TO TAG Style
SCAN
  SCATTER MEMVAR
  lcStyClr  = SUBSTR(style,1,16)
  lcExScale = SUBSTR(Scale,1,1)
  lcOldSlDs = IIF(SEEK('S'+SCALE,'SCALE'),ALLT(SCALE.CSCL_DESC),'')

  SELECT SCALE
  =SEEK('S'+lcExScale)
  SCAN WHILE Type+Scale = 'S'+lcExScale
    lcNScale = Scale
    IF !SEEK(lcStyClr+lcNScale,'STYLE') AND !SEEK(lcStyClr+lcNScale,lcTmpStyl)
      SELECT (lcTmpStyl)
      APPEND BLANK
      GATHER MEMVAR
      REPLACE STYLE  WITH lcStyClr+lcNScale,;
              SCALE  WITH lcNScale,;
              DESC1  WITH STRTRAN(ALLT(DESC1),lcOldSlDs,'')+ALLT(SCALE.CSCL_DESC)

      *--Empty the quantity.
      REPLACE NSTKVAL  WITH 0,;
              ORD1     WITH 0,;
              ORD2     WITH 0,;
              ORD3     WITH 0,;
              ORD4     WITH 0,;
              ORD5     WITH 0,;
              ORD6     WITH 0,;
              ORD7     WITH 0,;
              ORD8     WITH 0,;
              TOTORD   WITH 0,;
              WIP1     WITH 0,;
              WIP2     WITH 0,;
              WIP3     WITH 0,;
              WIP4     WITH 0,;
              WIP5     WITH 0,;
              WIP6     WITH 0,;
              WIP7     WITH 0,;
              WIP8     WITH 0,;
              TOTWIP   WITH 0
      REPLACE STK1     WITH 0,;
              STK2     WITH 0,;
              STK3     WITH 0,;
              STK4     WITH 0,;
              STK5     WITH 0,;
              STK6     WITH 0,;
              STK7     WITH 0,;
              STK8     WITH 0,;
              TOTSTK   WITH 0,;
              ALO1     WITH 0,;
              ALO2     WITH 0,;
              ALO3     WITH 0,;
              ALO4     WITH 0,;
              ALO5     WITH 0,;
              ALO6     WITH 0,;
              ALO7     WITH 0,;
              ALO8     WITH 0,;
              TOTALO   WITH 0
      REPLACE SHP1     WITH 0,;
              SHP2     WITH 0,;
              SHP3     WITH 0,;
              SHP4     WITH 0,;
              SHP5     WITH 0,;
              SHP6     WITH 0,;
              SHP7     WITH 0,;
              SHP8     WITH 0,;
              TOTSHP   WITH 0,;
              RET1     WITH 0,;
              RET2     WITH 0,;
              RET3     WITH 0,;
              RET4     WITH 0,;
              RET5     WITH 0,;
              RET6     WITH 0,;
              RET7     WITH 0,;
              RET8     WITH 0,;
              TOTRET   WITH 0,;
              DMGQTY   WITH 0
      REPLACE RA1      WITH 0,;
              RA2      WITH 0,;
              RA3      WITH 0,;
              RA4      WITH 0,;
              RA5      WITH 0,;
              RA6      WITH 0,;
              RA7      WITH 0,;
              RA8      WITH 0,;
              TOTRA    WITH 0,;
              INTRANS1 WITH 0,;
              INTRANS2 WITH 0,;
              INTRANS3 WITH 0,;
              INTRANS4 WITH 0,;
              INTRANS5 WITH 0,;
              INTRANS6 WITH 0,;
              INTRANS7 WITH 0,;
              INTRANS8 WITH 0,;
              TOTINTRN WITH 0,;
              NWO1     WITH 0,;
              NWO2     WITH 0,;
              NWO3     WITH 0,;
              NWO4     WITH 0,;
              NWO5     WITH 0,;
              NWO6     WITH 0,;
              NWO7     WITH 0,;
              NWO8     WITH 0,;
              NTOTWO   WITH 0
      IF MAKE
        REPLACE NMCOST1  WITH StyScan.nMCost1,;
                NMCOST2  WITH StyScan.nMCost2,;
                NMCOST3  WITH StyScan.nMCost3,;
                NMCOST4  WITH StyScan.nMCost4,;
                NMCOST5  WITH StyScan.nMCost5,;
                TOTCOST  WITH NMCOST1+NMCOST2+NMCOST3+NMCOST4+NMCOST5,;
                AVE_COST WITH TOTCOST
      ELSE
        REPLACE NICOST1  WITH StyScan.nICost1,;
                NICOST2  WITH StyScan.nICost2,;
                NICOST3  WITH StyScan.nICost3,;
                NICOST4  WITH StyScan.nICost4,;
                NICOST5  WITH StyScan.nICost5,;
                NPRCOST2 WITH IIF(NICOST1<>0,(NICOST2/NICOST1)*100,0),;
                NPRCOST3 WITH IIF(NICOST1<>0,(NICOST3/NICOST1)*100,0),;
                NPRCOST4 WITH IIF(NICOST1<>0,(NICOST4/NICOST1)*100,0),;
                NPRCOST5 WITH IIF(NICOST1<>0,(NICOST5/NICOST1)*100,0),;
                TOTCOST  WITH NICOST1+NICOST2+NICOST3+NICOST4+NICOST5,;
                AVE_COST WITH TOTCOST
      ENDIF
    ENDIF
  ENDSCAN

  *--Thermometers.
  =lfThrmo(lnCurNm2,lnMax2,'2')
ENDSCAN

USE IN StyScan

IF llScleOpn AND USED('SCALE')
  USE IN SCALE
ENDIF  

*--Append to style.
SELECT (lcTmpStyl)
USE

SELECT STYLE
APPEND FROM (gcWorkDir+lcTmpStyl)

IF llStyOpns AND USED('STYLE')
  USE IN STYLE
ENDIF

ERASE (gcWorkDir+lcTmpStyl+'.DBF')
ERASE (gcWorkDir+lcTmpStyl+'.CDX')

*--Close Segment value file if opend.
IF llCodsOpn AND USED('ICSEGVAL')
  USE IN ICSEGVAL
ENDIF
=lfThrmo(lnCurNm3,lnMax3,'3')
RETURN



*:*************************************************************
*: Name      : lfKeyChang
*: Developer : Timour A. K.
*: Date      : 03/15/99
*: Purpose   : Key Change.
*:*************************************************************
*: Called From  : SMCvExS
*:*************************************************************
FUNCTION lfKeyChang


lcD1Thrm = 'Updating modules files...'
lcD1ThPrc=''
SHOW GET lcD1Thrm
SHOW GET lcD1ThPrc

lnCurNm2=1
lnMax2  =37

=lfKeyCg('APINVTKT')
=lfKeyCg('APVINVDT')
=lfKeyCg('BOM')
=lfKeyCg('BOMCOST')
=lfKeyCg('BOMLINE')
=lfKeyCg('CONSINVL')
=lfKeyCg('CTKTBOM')
=lfKeyCg('CTKTRCVL')
=lfKeyCg('CUTPICK')
=lfKeyCg('CUTTKTH')
=lfKeyCg('CUTTKTL')
=lfKeyCg('DYE_REL')
=lfKeyCg('FABDYE')
=lfKeyCg('FABRIC')
=lfKeyCg('FINVTADJ')
=lfKeyCg('ICSTYHST')
=lfKeyCg('INVLINE')
=lfKeyCg('INVTADJ')
=lfKeyCg('MATINVJL')
=lfKeyCg('MFGOPRDT')
=lfKeyCg('MMFGORDD')
=lfKeyCg('MFGOPRHD')
=lfKeyCg('ORDLINE')
=lfKeyCg('PACK_LIN')
=lfKeyCg('POFLN')
=lfKeyCg('POSLN')
=lfKeyCg('RALINE')
=lfKeyCg('RETLINE')
=lfKeyCg('SPCK_HDR')
=lfKeyCg('SPCK_LIN')
=lfKeyCg('STYDYE')
=lfKeyCg('STYINVJL')
=lfKeyCg('STYLEUPC')
=lfKeyCg('STYPRICE')
=lfKeyCg('TMPINVT')
=lfKeyCg('WHSLOC')
=lfKeyCg('WIPADJ')

=lfThrmo(lnCurNm3,lnMax3,'3')
RETURN


*:*************************************************************
*: Name      : lfvSty
*: Developer : Timour A. K.
*: Date      : 03/15/99
*: Purpose   : Get new Style code.
*:*************************************************************
*: Called From  : lfKyCgSt
*:*************************************************************
FUNCTION lfvSty
PARA lcPStyle,llSpCatg

*--Special Case for Item field in BOM and BOMLINE.
*--Special Case for AltStyle field in ORDLINE,INVLINE,CONSINVL.
IF llSpCatg
 *B802108,1 added to check the !empty styles. 
 IF (ALIAS() $ 'BOM,BOMLINE' AND cCatgTyp<>'S') OR ;
    (ALIAS() $ 'ORDLINE,INVLINE,CONSINVL' AND EMPTY(lcPStyle)) OR ;
    (ALIAS() = 'SPCK_HDR' AND EMPTY(lcPStyle)) OR ;
    (ALIAS() = 'WHSLOC' AND EMPTY(lcPStyle))
    RETURN lcPStyle
 ENDIF
ENDIF

*IF llLongSty AND LEN(ALLT(PADR(lcPStyle,12))) > 8 AND SEEK(PADR(lcPStyle,12),lcTmpSMap)
*  lcStyleMjr = PADR(&lcTmpSMap..Style,8)
*ELSE

lcStyleMjr = SUBSTR(lcPStyle,1,8)

*ENDIF

lcStyColor   = SUBSTR(lcPStyle,14,5)+' '

STORE '' TO lcStyScale,lcAddPatt
IF SEEK(SUBSTR(lcPStyle,19,1),'SYCNVSCL')
  *TTTT
  *B804514,1 (Begin) As per TAK and NAD this customer (Wearwolf) had 2 recods with 'Q' in  
  *                  scale file and he no longer has them, he has just one record now so 
  *                  remark the following condition as : 
  *                  line  =SEEK(SUBSTR('Q','SYCNVSCL')) gave an error causing the program return.
  *IF SUBSTR(lcPStyle,19,1) = 'Q'
  *  lnCrWAls = SELECT()
  *  SELECT SYCNVSCL
  *  SKIP
  *  IF CEXTSCALE<>'Q'
  *    =SEEK(SUBSTR('Q','SYCNVSCL'))
  *  ENDIF
  *  SELECT(lnCrWAls)
  *ENDIF
  *B804514,1 (End)
  lcStyScale = SYCNVSCL.Scale
  lcAddPatt  = ALLT(SYCNVSCL.Pattern)
  IF !EMPTY(lcAddPatt)
    lcStyleMjr = PADR(ALLT(PADR(lcStyleMjr,8-LEN(lcAddPatt) ))+lcAddPatt,8)
  ENDIF
ELSE
  IF lcStyColor='*****' OR lcStyColor='******'
    lcStyColor ='******'
    lcStyScale ='***'
  ELSE
    =lfUpdError('File/Style:'+ALIAS()+'/'+lcPStyle+' (2.6 Scale:"'+SUBSTR(lcPStyle,19,1)+'" not found in 2.7, (Skipped).)')
    *tttt
    lcStyleMjr = 'SKIPPED '
  ENDIF
ENDIF

*--Return style code.    *lcRStyle = '12345678'+'-'+'12345'+' /'+'123'
lcRStyle  = lcStyleMjr+'-'+lcStyColor+'/'+lcStyScale
RETURN lcRStyle





***************
FUNCTION lfvCnt
lnTotRec = RECCOUNT()
lcMFleDesc = ALIAS()+'  '+ALLT(STR( (lnTotRec-RECNO()),9 ) )+ ' Records Remaining...'
SHOW GET lcMFleDesc
RETURN .T.

******************
FUNCTION lfESSThrm
lcMFleDesc = ALIAS()
SHOW GET lcMFleDesc
*--Thermometers.
=lfThrmo(lnCurNm2,lnMax2,'2')
RETURN



************************************************************
FUNCTION lfKeyCg
PARA lcKCFile

IF !FILE(gcDataDir+lcKCFile+'.DBF')
  RETURN
ENDIF
llOpnded=gfOpenFile(gcDataDir+lcKCFile,'','SH')

=lfESSThrm()

DO CASE
  CASE lcKCFile $ 'WIPADJ,WHSLOC'
     REPLACE ALL Style WITH lfvSty(Style,.T.),Color WITH PADR(Color,5) FOR lfvCnt()

  CASE lcKCFile<>'INVTADJ' AND lcKCFile $ 'FABDYE,FABRIC,FINVTADJ,DYE_REL,TMPINVT,MMFGORDD,APINVTKT,APVINVDT,POFLN' 
     REPLACE ALL Color WITH PADR(Color,5) FOR lfvCnt()

  CASE lcKCFile = 'MATINVJL'
     REPLACE ALL cColor WITH PADR(cColor,5) FOR lfvCnt()

  CASE lcKCFile == 'BOMCOST' OR lcKCFile = 'CTKTBOM'
    IF lcKCFile='BOMCOST'
      SET FILTER TO cCostType='S'
    ELSE
      SET FILTER TO cCatgTyp='S'
    ENDIF
    REPLACE ALL Item WITH lfvSty(Item),IClr WITH PADR(IClr,5)+IIF(IClr='*','*','') FOR lfvCnt()

  CASE lcKCFile == 'BOM'
    REPLACE ALL CITMMASK WITH lfvSty(cItmMask),CITMMAJOR WITH PADR(CITMMASK,8),;
                Item     WITH lfvSty(Item,.T.),IClr WITH PADR(IClr,5)+IIF(IClr='*','*','') ;
            FOR lfvCnt()

  CASE lcKCFile == 'BOMLINE'
    REPLACE ALL STYLE WITH lfvSty(STYLE),SCLR WITH PADR(SCLR,5)+IIF(SCLR='*','*',''),;
                Item  WITH lfvSty(Item,.T.) ,IClr WITH PADR(IClr,5)+IIF(IClr='*','*','') ;
            FOR lfvCnt()
    DELETE ALL FOR ('SKIPPED' $ Style )

  CASE lcKCFile $ 'ORDLINE,INVLINE,CONSINVL'
    REPLACE ALL STYLE WITH lfvSty(STYLE),ALTSTYLE WITH lfvSty(ALTSTYLE,.T.),SCALE WITH SUBSTR(Style,17,3)
    DELETE ALL FOR ('SKIPPED' $ Style )

  CASE lcKCFile $ 'TMPINVT,POSLN' 
    REPLACE ALL STYLE WITH lfvSty(STYLE),SCALE WITH SUBSTR(Style,17,3)
    DELETE ALL FOR ('SKIPPED' $ Style )
    
  CASE lcKCFile = 'MFGOPRDT'
    REPLACE ALL Item WITH lfvSty(Item),Color WITH PADR(Color,5) FOR lfvCnt()
    
  CASE lcKCFile $ 'STYDYE,INVTADJ,STYLEUPC,STYPRICE,ICSTYHST,CUTTKTL,STYINVJL,RALINE,RETLINE,PACK_LIN,SPCK_LIN,CTKTRCVL,CUTPICK'
     REPLACE ALL Style WITH lfvSty(Style) FOR lfvCnt()
    DELETE ALL FOR ('SKIPPED' $ Style )

  CASE lcKCFile = 'SPCK_HDR'
     REPLACE ALL Style WITH lfvSty(Style,.T.) FOR lfvCnt()
     
  CASE lcKCFile = 'CUTTKTH'
    REPLACE ALL STYLE WITH PADR(Style,8) FOR lfvCnt()

  CASE lcKCFile = 'MFGOPRHD'
    REPLACE ALL CITMMAJOR WITH PADR(CITMMAJOR,8) FOR lfvCnt()

ENDCASE
IF llOpnded
  USE IN (lcKCFile)
ENDIF
RETURN
