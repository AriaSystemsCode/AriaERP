*:***************************************************************************
*: Program file  : POSTYPBL                        (C102325)
*: Program desc. : CUSTOM PO bill of Material
*: For Report    : MFPRCSBL.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Purchase Order (PO)
*: Developer     : MOHAMED SHOKRY
*:***************************************************************************
*! Modifications :
*! B604776,1 AMH 08/07/2001 Fix some bugs in Bom form. Such as printing the
*! B604776,1                vendor name instead of the vendor code, 
*! B604776,1                remove the black boxes, printing the long desc
*! B604776,1                of the color of the fabric instead of the color 
*! B604776,1                of the style.
*:***************************************************************************

IF llrPrtCs 
  *--to get color length
  STORE 0 TO lnClrPo,lnColorLen
  STORE '' TO lcColor,lcCLrDesc
  *-- To get the color long name
  DIMENSION laClrLName[1,2]
  laClrLName[1,1] = 'CLRLNAME'      && Array to get the Color long name
  laClrLName[1,2] = 'lcCLrDesc'

  *--call function to get no major segment (color)
  =lfNonMaj()

  *--create another indx
  lcCostF2 = gfTempName()
  SELECT (lcCostF)
  ZAP
  *--add variable to check if file is opened
  llIsOpen = .F. 
  IF !USED('BOMLINE')
    llIsOpen = .T. 

   *B604776,1 AMH Change the bomline order to to collect records of type 1 only [Start]
   *=gfOpenFile(gcDataDir+'BOMLINE','MfgOpr','SH')
   =gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')
   *B604776,1 AMH [End]

  ENDIF
  *--add some fields to use in FRX
  SELECT (lcCostF)
  =AFIELDS(laMlnTmp)
  lnNewFld = ALEN(laMlnTmp,1)

  *B604776,1 AMH Add two new fields [Start]
  *DIMENSION laMlnTmp[lnNewFld+7,4]
  DIMENSION laMlnTmp[lnNewFld+9,4]
  *B604776,1 AMH [End]
  
  laMlnTmp[lnNewFld + 1,1] = 'cStyle'
  laMlnTmp[lnNewFld + 1,2] = 'C'
  laMlnTmp[lnNewFld + 1,3] = 19
  laMlnTmp[lnNewFld + 1,4] = 0

  laMlnTmp[lnNewFld + 2,1] = 'cColDesc'
  laMlnTmp[lnNewFld + 2,2] = 'C'
  laMlnTmp[lnNewFld + 2,3] = 60
  laMlnTmp[lnNewFld + 2,4] = 0

  laMlnTmp[lnNewFld + 3,1] = 'cStyleDes'
  laMlnTmp[lnNewFld + 3,2] = 'C'
  laMlnTmp[lnNewFld + 3,3] = 60
  laMlnTmp[lnNewFld + 3,4] = 0

  laMlnTmp[lnNewFld + 4,1] = 'nYield'
  laMlnTmp[lnNewFld + 4,2] = 'N'
  laMlnTmp[lnNewFld + 4,3] = 7
  laMlnTmp[lnNewFld + 4,4] = 3

  laMlnTmp[lnNewFld + 5,1] = 'cPattern'
  laMlnTmp[lnNewFld + 5,2] = 'C'
  laMlnTmp[lnNewFld + 5,3] = 10
  laMlnTmp[lnNewFld + 5,4] = 0

  laMlnTmp[lnNewFld + 6,1] = 'cContent'
  laMlnTmp[lnNewFld + 6,2] = 'C'
  laMlnTmp[lnNewFld + 6,3] = 60
  laMlnTmp[lnNewFld + 6,4] = 0

  laMlnTmp[lnNewFld + 7,1] = 'cMDesc'
  laMlnTmp[lnNewFld + 7,2] = 'C'
  laMlnTmp[lnNewFld + 7,3] = 20
  laMlnTmp[lnNewFld + 7,4] = 0

  *B604776,1 AMH Add two new fields [Start]
  laMlnTmp[lnNewFld + 8,1] = 'cPoVen'
  laMlnTmp[lnNewFld + 8,2] = 'C'
  laMlnTmp[lnNewFld + 8,3] = 30
  laMlnTmp[lnNewFld + 8,4] = 0

  laMlnTmp[lnNewFld + 9,1] = 'cFabVen'
  laMlnTmp[lnNewFld + 9,2] = 'C'
  laMlnTmp[lnNewFld + 9,3] = 30
  laMlnTmp[lnNewFld + 9,4] = 0
  *B604776,1 AMH [End]
  
  CREATE DBF (gcWorkDir+lcCostF) FROM ARRAY laMlnTmp
  INDEX ON CUTTKT+cCatgTyp+Typ+Item+IClr TAG (lcCostF2)
  SELECT poshdr
  lcOldSkip = SET('SKIP')
  SET SKIP TO
  SCAN FOR &lcRpExp 
    *-- If we will print cost sheet , insert cost sheet records in the temprorary file.
     IF SEEK('I'+POSHDR.PO,'CTKTBOM')
      SELECT CTKTBOM
      SCAN WHILE cImTyp+CutTkt+Typ+Item+Iclr+MfgCode+Dyelot = 'I'+POSHDR.PO FOR cCatgTyp $ 'FT'
        =SEEK(PADR(ITEM,7)+Iclr,'FABRIC')        
        SCATTER MEMVAR
        m.cPattern = FABRIC.pattern
        m.cContent = FABRIC.Content
        m.cMDesc   = FABRIC.DESC
        
        *B604776,1 AMH get the vendor name from apvendor [Start]
        m.cPoVen   = IIF(SEEK(poshdr.vendor,'APVENDOR'),APVENDOR.CVENCOMP,'')
        m.cFabVen  = IIF(SEEK(Fabric.vendor,'APVENDOR'),APVENDOR.CVENCOMP,'')
        *B604776,1 AMH [End]
        
        INSERT INTO (lcCostF) FROM MEMVAR 
        *--add variable to check first varible
        llChckFrst = .T.
        
        *B604776,1 AMH find the first record for the Po with type 1 [Start]
        *IF SEEK('I'+cTktBom.CutTkt ,'BOMLINE')
        IF SEEK('I'+'1'+cTktBom.CutTkt ,'BOMLINE')        
        *B604776,1 AMH [End]
        
          SELECT BOMLINE
          
          *B604776,1 AMH Scan for type 1 only [Start]
          *SCAN REST WHILE cImTyp+cTktNo+cOprCode+cType+Style+SClr+IIF(cCatgTyp$"MDP",PADR(mfgcode,12),item)+iclr = 'I' +CTKTBOM.CutTkt;
                          FOR item+iclr = CTKTBOM.item + CTKTBOM.iclr
          SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+;
                          SClr+Item+IClr+MfgCode = 'I' +'1' + CTKTBOM.CutTkt;
                          FOR Item+IClr = CTKTBOM.Item + CTKTBOM.IClr
          *B604776,1 AMH [End]
          
            IF llChckFrst 

              *B604776,1 AMH get the desc. of item color [Start]
              *lcColor = SUBSTR(Style,lnClrPo,lnColorLen)
              *=gfRltFld(lcColor, @laClrLName ,'COLOR     ')
              =gfRltFld(iclr, @laClrLName ,'COLOR     ')
              *B604776,1 AMH [End]

              =SEEK(STYLE,'STYLE')
             
              REPLACE &lcCostF..cStyle    WITH STYLE,;
                      &lcCostF..cColDesc  WITH lcCLrDesc,;
                      &lcCostF..cStyleDes WITH STYLE.DESC1,;
                      &lcCostF..nYield    WITH UnitQty
            ELSE
              =gfRltFld(iclr, @laClrLName ,'COLOR     ')
              =SEEK(STYLE,'STYLE')
              m.cStyleDes = STYLE.DESC1
              m.cStyle    = STYLE
              m.cColDesc  = lcCLrDesc
              m.nYield    = UnitQty
              INSERT INTO (lcCostF) FROM MEMVAR 
            ENDIF
            llChckFrst = .F.
          ENDSCAN
        ENDIF     
      ENDSCAN
    ENDIF
  ENDSCAN
  *--used in FRX to group by
  lcInGroup = 'CUTTKT+TYP + ITEM + ICLR'

  *--close opend file
  IF llIsOpen 
    SELECT BOMLINE
    USE 
  ENDIF

  SELECT poshdr
  SET SKIP TO &lcOldSkip
  LOCATE
ENDIF

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Mohamed Shokry (MHM)
*! Date      : 06/25/2001
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnNonMajPo = 0
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      llStopConc = .T.
    ENDIF
  ENDIF
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
RETURN ''
*--End lfNonMaj