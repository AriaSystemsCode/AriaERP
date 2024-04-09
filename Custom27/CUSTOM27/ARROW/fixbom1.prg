lcDataPath = GETDIR('','Select Data Dir.')
IF EMPTY(lcDataPath)
  RETURN
ENDIF
WAIT WINDOW 'Opening Data Files...' NOWAIT

IF !USED('STYLE')
  USE (lcDataPath) + 'STYLE.DBF' IN 0 SHARED
ENDIF

IF !USED('BOM')
  USE (lcDataPath) + 'BOM.DBF' IN 0  SHARED
ENDIF

IF !USED('SETUPS')
  USE (lcDataPath) + 'SETUPS.DBF' IN 0  SHARED
ENDIF

CREATE TABLE (lcDataPath+'FIXEDST.DBF') (Style C(19) , cStyMajor C(19))
IF !USED('FIXEDST')
  USE (lcDataPath+'FIXEDST.DBF') IN 0 SHARED
ENDIF
SET SAFETY OFF
SELECT SETUPS
SET ORDER TO TAG VARNAME

WAIT WINDOW 'Collecting Cost Information...' NOWAIT

IF SEEK('M_CITYPE1 ')
  lcIType1 = ALLTRIM(MDATA_DEF)
ELSE
  WAIT WINDOW 'SETUPS ERROR'
  RETURN
ENDIF

IF SEEK('M_CITYPE2 ')
  lcIType2 = ALLTRIM(MDATA_DEF)
ELSE
  WAIT WINDOW 'SETUPS ERROR'
  RETURN
ENDIF

IF SEEK('M_CITYPE3 ')
  lcIType3 = ALLTRIM(MDATA_DEF)
ELSE
  WAIT WINDOW 'SETUPS ERROR'
  RETURN
ENDIF

IF SEEK('M_CITYPE4 ')
  lcIType4 = ALLTRIM(MDATA_DEF)
ELSE
  WAIT WINDOW 'SETUPS ERROR'
  RETURN
ENDIF

IF SEEK('M_CITYPE5 ')
  lcIType5 = ALLTRIM(MDATA_DEF)
ELSE
  WAIT WINDOW 'SETUPS ERROR'
  RETURN
ENDIF
  
IF SEEK('M_CMTYPE1 ')
  lcMType1 = ALLTRIM(MDATA_DEF)
ELSE
  WAIT WINDOW 'SETUPS ERROR'
  RETURN
ENDIF

IF SEEK('M_CMTYPE2 ')
  lcMType2 = ALLTRIM(MDATA_DEF)
ELSE
  WAIT WINDOW 'SETUPS ERROR'
  RETURN
ENDIF

IF SEEK('M_CMTYPE3 ')
  lcMType3 = ALLTRIM(MDATA_DEF)
ELSE
  WAIT WINDOW 'SETUPS ERROR'
  RETURN
ENDIF

IF SEEK('M_CMTYPE4 ')
  lcMType4 = ALLTRIM(MDATA_DEF)
ELSE
  WAIT WINDOW 'SETUPS ERROR'
  RETURN
ENDIF

IF SEEK('M_CMTYPE5 ')
  lcMType5 = ALLTRIM(MDATA_DEF)
ELSE
  WAIT WINDOW 'SETUPS ERROR'
  RETURN
ENDIF

SELECT BOM
SET ORDER TO TAG Bom

SELECT STYLE
SCAN FOR !LDETCOST
  WAIT WINDOW 'Checking style : '+STYLE.Style NOWAIT

  IF STYLE.MAKE
    REPLACE STYLE.TotCost WITH STYLE.nmCost1+STYLE.nmCost2+STYLE.nmCost3+STYLE.nmCost4+STYLE.nmCost5
    lnCostVal1 = STYLE.nmCost1
    lnCostVal2 = STYLE.nmCost2
    lnCostVal3 = STYLE.nmCost3
    lnCostVal4 = STYLE.nmCost4
    lnCostVal5 = STYLE.nmCost5
  ELSE
    REPLACE STYLE.niCost1 WITH STYLE.Gros_Price * (1-(STYLE.Disc_Pcnt/100))
    REPLACE STYLE.TotCost WITH STYLE.niCost1+STYLE.niCost2+STYLE.niCost3+STYLE.niCost4+STYLE.niCost5
    lnCostVal1 = STYLE.niCost1
    lnCostVal2 = STYLE.niCost2
    lnCostVal3 = STYLE.niCost3
    lnCostVal4 = STYLE.niCost4
    lnCostVal5 = STYLE.niCost5
  ENDIF

  FOR LNI = 1 TO 5
    LCZ = STR(LNI,1)
    IF lnCostVal&LCZ = 0
      LOOP
    ENDIF  

    IF SEEK(STYLE.cStyMajor+LCZ,'BOM') AND SUBSTR(BOM.CITMMASK,14,6)='******'
      SELECT BOM
      DELETE 
    ENDIF
  
    IF SEEK(STYLE.cStyMajor+LCZ+STYLE.Style,'BOM')
      *--UPDATE 
      SELECT BOM
      =RLOCK()
      REPLACE MFGCODE    WITH '*'+LCZ,;
              UOM        WITH 'EAC',;
              UntCost    WITH lnCostVal&LCZ ,;
              nBomTotQty WITH 1,;
              TotCost    WITH UntCost,;
              cCatgTyp   WITH IIF(!STYLE.MAKE , lcIType&LCZ,lcMType&LCZ)
      UNLOCK
    ELSE
      *--APPEND
      SELECT BOM
      APPEND BLANK
      REPLACE cItmMajor  WITH STYLE.CSTYMAJOR,;
              Typ        WITH LCZ,;
              cItmMask   WITH STYLE.STYLE,;
              MFGCODE    WITH '*'+LCZ,;
              UOM        WITH 'EAC',;
              UntCost    WITH lnCostVal&LCZ ,;
              nBomTotQty WITH 1,;
              TotCost    WITH UntCost,;
              cCatgTyp   WITH IIF(!STYLE.MAKE , lcIType&LCZ,lcMType&LCZ)
      
      SELECT FIXEDST
      APPEN BLANK
      REPLACE STYLE     WITH STYLE.STYLE,;
              CSTYMAJOR WITH STYLE.CSTYMAJOR

    ENDIF
  ENDFOR
  
ENDSCAN

WAIT CLEAR
@ 0,0 SAY 'Fixed styles found in ' + lcDataPath +'fixedst.DBF'
CLEA ALL

