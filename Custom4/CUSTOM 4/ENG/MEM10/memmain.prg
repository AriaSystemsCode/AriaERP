*!***************************************************************************************************************************************
*! Name      : MEMMAIN.PRG
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/04/2008
*! Purpose   : MEMO Main Program (C201063){T20080908.0001}
*!***************************************************************************************************************************************
*! Parameters: loFormSet -> FormSet object
*!       lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!***************************************************************************************************************************************
*! Returns   : Logical value.     
*!***************************************************************************************************************************************
*! Modifcations :
*! B609413,1 MMT 09/21/2010 pick carton screen at Memo hangs on partial allocations.[T20100914.0012]
*!***************************************************************************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars  


lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!*************************************************************
*! Name      : lfSAVCARTON
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Save Cartons
*!*************************************************************
FUNCTION lfSAVCARTON
IF !(loFormSet.activemode $ 'AE')
  RETURN 
ENDIF 


IF loFormSet.activemode = 'A'
  IF gfModalGen('QRM00000B34001',.F.,.F.,.F.,'Update Carton Details') = 2
    RETURN
  ENDIF

  SELECT (loFormSet.lcShpLineForUpdate)
  LOCATE 
  IF EOF()
    RETURN 
  ENDIF 

  DO FORM (oAriaApplication.ScreenHome+"\po\pocrtinf.scx") WITH loFormSet
ELSE
  *Edit Mode Saving.
  lcMasterScrFile =  IIF(loFormSet.ActiveMode $ 'EV',loFormSet.lcShpLine,loFormSet.lcShpLineForUpdate) 
  IF !USED('POCRTNMF')
    =gfOpenTable('POCRTNMF','pocrtnmf')
  ENDIF
  IF !USED('Style_A')
    =gfOpenTable('Style','Style','SH','Style_A')
  ENDIF 
  lcTempShp = loFormSet.lcTempShp
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
  IF !USED(lcTempShp)
    RETURN 
  ENDIF 
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
  
  SELECT (lcTempShp)
  SET KEY TO 
  SUM NCrtTot FOR  UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' TO lnTotCrt
  LOCATE 
  IF lnTotCrt > 0
    loFormSet.lntotshpcrt  = EVALUATE(loFormSet.lcShpHdrForUpdate+'.NGRSWGHT')/lnTotCrt
  ELSE
    loFormSet.lntotshpcrt = 0
  ENDIF 
  lcDele = SET("Deleted")
  SET DELETED OFF 
  SCAN FOR UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' AND !EMPTY(Cstatus) AND TOTQty > 0 
    IF DELETED() AND CSTATUS = 'D' && Deleted 
      SELECT 'pocrtnmf'
      gfSetOrder('pocrtshp')
      =gfSeek(EVALUATE(loFormSet.lcShpHdrForUpdate+'.Shipno')+&lcTempShp..po)
      SCAN REST WHILE CSHIPNO+PO+Str(NLINENO,6)+STYLE+CCARTONTYP =;
        EVALUATE(loFormSet.lcShpHdrForUpdate+'.Shipno')+&lcTempShp..po FOR NLINENO = &lcTempShp..LineNo AND Style = ALLTRIM(&lcTempShp..style) 
        =gfDelete()
      ENDSCAN 
      SELECT 'pocrtnmf'
      gfSetOrder('pocrtnmf')
    ELSE
      IF Cstatus = 'N' && NEw 
        m.nLINENO  = LINENO 
        lcCurrPO = PO
        lcCurrStyleMaj = Style
        m.CstyMajor  = SUBSTR(lcCurrStyleMaj,1,loFormSet.lnMajLen)
        m.PO = lcCurrPO 
        m.CShipNo = EVALUATE(loFormSet.lcShpHdrForUpdate+'.Shipno')
        m.Invoice = ''
        m.Piktkt = ''
        m.weight = loFormSet.lntotshpcrt 
        FOR lnL = 1 TO &lcTempShp..NCrtTot
          m.ccartonNo = gfsequence('CCARTONNO')
          =gfSeek(ALLTRIM(lcCurrStyleMaj) ,'Style_A','Style')
          SELECT 'Style_A'
          lnStyCnt = 1
          lnLastCrt = 1
          SCAN REST WHILE Style =ALLTRIM(lcCurrStyleMaj)
             m.Rec_no = ''
             m.cedit_user = ''
             m.dedit_date = {}
             m.cedit_time = ''
             m.cedt_Ver = ''
            IF SEEK(lcCurrPO +'0001'+Style_A.Style,lcMasterScrFile) AND !DELETED(lcMasterScrFile)
              =gfSeek("S"+Style_A.Scale,'Scale')    
              m.Style = Style_A.Style
              m.cwarecode = &lcMasterScrFile..cwarecode 
              lnLastCrt = lnStyCnt 
              m.ccartontyp = &lcTempShp..CCRTNVLTYP 
              DIMENSION laCartRelated[3,2]
              laCartRelated[1,1] = 'NCRTLENGTH'
              laCartRelated[1,2] = 'lnLength'
              laCartRelated[2,1] = 'NCRTWIDTH'
              laCartRelated[2,2] = 'lnWIDTH'
              laCartRelated[3,1] = 'NCRTHEIGHT'
              laCartRelated[3,2] = 'lnHEIGHT'
              STORE 0 TO lnHEIGHT,lnWIDTH,lnLength
              llNoThing = gfRltFld(&lcTempShp..CCRTNVLTYP, @laCartRelated, "CCRTNVLTYP")
              m.NDEPTH = lnHEIGHT   
              m.NLENGTH   = lnLength
              m.WIDTH  =   STR(lnWIDTH,6,2)
              m.TOTQTY = 0
              FOR lnT = 1 TO 8
                lcT = STR(lnT ,1)
                STORE 0 TO m.Qty&lcT. 
               ENDFOR  
               STORE '' TO m.Cadd_user,m.cadd_time 
              FOR lnJ = 1 TO Scale.Cnt
                lcJ = STR(lnJ,1)
                lcStyCnt =ALLTRIM(STR(lnStyCnt))
                IF &lcTempShp..QTY&lcStyCnt. > 0
                  m.QTY&lcJ. = &lcTempShp..QTY&lcStyCnt. 
                  m.TOTQTY = m.TOTQTY +  m.QTY&lcJ.
                ENDIF 
                lnStyCnt = lnStyCnt + 1
              ENDFOR  
              IF m.TOTQTY <> 0 
                 INSERT INTO 'POCRTNMF' FROM MEMVAR 
                 =gfAdd_Info('POCRTNMF')
                 SELECT  'POCRTNMF' 
                 gfReplace("")
              ENDIF   
              SELECT 'Style_A'
            ENDIF 
          ENDSCAN
        ENDFOR 
      ELSE && Modified 
        SELECT 'pocrtnmf'
        gfSetOrder('pocrtshp')
        =gfSeek(EVALUATE(loFormSet.lcShpHdrForUpdate+'.Shipno')+&lcTempShp..po)
        SELECT Distinct cCartonNo FROM pocrtnmf WHERE;
              CSHIPNO+PO+Str(NLINENO,6)+STYLE+CCARTONTYP= EVALUATE(loFormSet.lcShpHdrForUpdate+'.Shipno')+&lcTempShp..po AND NLINENO =&lcTempShp..LineNo AND Style =  ALLTRIM(&lcTempShp..style)  ;
              INTO CURSOR "CartonS"
        SELECT 'pocrtnmf'
        =gfSeek(EVALUATE(loFormSet.lcShpHdrForUpdate+'.Shipno')+&lcTempShp..po)
        SCAN REST WHILE CSHIPNO+PO+Str(NLINENO,6)+STYLE+CCARTONTYP= EVALUATE(loFormSet.lcShpHdrForUpdate+'.Shipno')+&lcTempShp..po FOR NLINENO =&lcTempShp..LineNo AND Style = ALLTRIM(&lcTempShp..style) 
          =gfDelete()
        ENDSCAN 

        IF RECCOUNT("CartonS") <> &lcTempShp..NCrtTot
          IF RECCOUNT("CartonS") < &lcTempShp..NCrtTot && LEss
             lnDiff = &lcTempShp..NCrtTot - RECCOUNT("CartonS") 
             SELECT 'CartonS'
             SCAN 
               m.ccartonNo = CartonS.ccartonNo 
               lnStyCnt = 1
               lnLastCrt = 1
               SELECT 'Style_A'
                m.nLINENO  = &lcTempShp..LINENO 
                lcCurrPO = &lcTempShp..PO
                lcCurrStyleMaj = &lcTempShp..Style
                m.CstyMajor  = SUBSTR(lcCurrStyleMaj,1,loFormSet.lnMajLen)
                m.PO = lcCurrPO 
                m.CShipNo = EVALUATE(loFormSet.lcShpHdrForUpdate+'.Shipno')
                m.Invoice = ''
                m.Piktkt = ''
                m.weight = loFormSet.lntotshpcrt 
               =gfSeek(ALLTRIM(&lcTempShp..style) ,'Style_A','Style')
               SELECT Style_A
               lnStyCnt = 1
               SCAN REST WHILE Style =ALLTRIM(&lcTempShp..style)
                 m.Rec_no = ''
                 m.cedit_user = ''
                 m.dedit_date = {}
                 m.cedit_time = ''
                 m.cedt_Ver = ''
                 IF SEEK(&lcTempShp..PO+'0001'+Style_A.Style,lcMasterScrFile) AND !DELETED(lcMasterScrFile)
                   =gfSeek("S"+Style_A.Scale,'Scale')    
                   m.Style = Style_A.Style
                   m.cwarecode = &lcMasterScrFile..cwarecode 
                   lnLastCrt = lnStyCnt 
                   m.ccartontyp = &lcTempShp..CCRTNVLTYP 
                   DIMENSION laCartRelated[3,2]
                   laCartRelated[1,1] = 'NCRTLENGTH'
                   laCartRelated[1,2] = 'lnLength'
                   laCartRelated[2,1] = 'NCRTWIDTH'
                   laCartRelated[2,2] = 'lnWIDTH'
                   laCartRelated[3,1] = 'NCRTHEIGHT'
                   laCartRelated[3,2] = 'lnHEIGHT'
                   STORE 0 TO lnHEIGHT,lnWIDTH,lnLength
                   llNoThing = gfRltFld(&lcTempShp..CCRTNVLTYP, @laCartRelated, "CCRTNVLTYP")
                   m.NDEPTH = lnHEIGHT   
                   m.NLENGTH   = lnLength
                   m.WIDTH  =   STR(lnWIDTH,6,2)
                   m.TOTQTY = 0
                   FOR lnT = 1 TO 8
                     lcT = STR(lnT ,1)
                     STORE 0 TO m.Qty&lcT. 
                    ENDFOR  
                    STORE '' TO m.Cadd_user,m.cadd_time 
                   FOR lnJ = 1 TO Scale.Cnt
                     lcJ = STR(lnJ,1)
                     lcStyCnt =ALLTRIM(STR(lnStyCnt))
                     IF &lcTempShp..QTY&lcStyCnt. > 0
                       m.QTY&lcJ. = &lcTempShp..QTY&lcStyCnt. 
                       m.TOTQTY = m.TOTQTY +  m.QTY&lcJ.
                     ENDIF 
                     lnStyCnt = lnStyCnt + 1
                   ENDFOR  
                   IF m.TOTQTY <> 0 
                     INSERT INTO 'POCRTNMF' FROM MEMVAR 
                     =gfAdd_Info('POCRTNMF')
                     SELECT  'POCRTNMF' 
                     gfReplace("")
                   ENDIF   
                   SELECT 'Style_A'
  
                 ENDIF 
               ENDSCAN 
             ENDSCAN     
             FOR lnK = 1 TO lnDiff 
               m.ccartonNo = gfsequence('CCARTONNO')
               lnStyCnt = 1
               lnLastCrt = 1
                m.nLINENO  = &lcTempShp..LINENO 
                lcCurrPO = &lcTempShp..PO
                lcCurrStyleMaj = &lcTempShp..Style
                m.CstyMajor  = SUBSTR(lcCurrStyleMaj,1,loFormSet.lnMajLen)
                m.PO = lcCurrPO 
                m.CShipNo = EVALUATE(loFormSet.lcShpHdrForUpdate+'.Shipno')
                m.Invoice = ''
                m.Piktkt = ''
                m.weight = loFormSet.lntotshpcrt 
               SELECT 'Style_A'
               lnStyCnt = 1
               =gfSeek(ALLTRIM(&lcTempShp..style) ,'Style_A','Style')
               SCAN REST WHILE Style =ALLTRIM(&lcTempShp..style)
                 m.Rec_no = ''
                 m.cedit_user = ''
                 m.dedit_date = {}
                 m.cedit_time = ''
                 m.cedt_Ver = ''
                 IF SEEK(&lcTempShp..PO+'0001'+Style_A.Style,lcMasterScrFile) AND !DELETED(lcMasterScrFile)
                   =gfSeek("S"+Style_A.Scale,'Scale')    
                   m.Style = Style_A.Style
                   m.cwarecode = &lcMasterScrFile..cwarecode 
                   lnLastCrt = lnStyCnt 
                   m.ccartontyp = &lcTempShp..CCRTNVLTYP 
                   DIMENSION laCartRelated[3,2]
                   laCartRelated[1,1] = 'NCRTLENGTH'
                   laCartRelated[1,2] = 'lnLength'
                   laCartRelated[2,1] = 'NCRTWIDTH'
                   laCartRelated[2,2] = 'lnWIDTH'
                   laCartRelated[3,1] = 'NCRTHEIGHT'
                   laCartRelated[3,2] = 'lnHEIGHT'
                   STORE 0 TO lnHEIGHT,lnWIDTH,lnLength
                   llNoThing = gfRltFld(&lcTempShp..CCRTNVLTYP, @laCartRelated, "CCRTNVLTYP")
                   m.NDEPTH = lnHEIGHT   
                   m.NLENGTH   = lnLength
                   m.WIDTH  =   STR(lnWIDTH,6,2)
                   m.TOTQTY = 0
                   FOR lnT = 1 TO 8
                     lcT = STR(lnT ,1)
                     STORE 0 TO m.Qty&lcT. 
                    ENDFOR  
                    STORE '' TO m.Cadd_user,m.cadd_time 
                   FOR lnJ = 1 TO Scale.Cnt
                     lcJ = STR(lnJ,1)
                     lcStyCnt =ALLTRIM(STR(lnStyCnt))
                     IF &lcTempShp..QTY&lcStyCnt. > 0
                       m.QTY&lcJ. = &lcTempShp..QTY&lcStyCnt. 
                       m.TOTQTY = m.TOTQTY +  m.QTY&lcJ.
                     ENDIF 
                     lnStyCnt = lnStyCnt + 1
                   ENDFOR  
                   IF m.TOTQTY <> 0 
                     INSERT INTO 'POCRTNMF' FROM MEMVAR 
                     =gfAdd_Info('POCRTNMF')
                     SELECT  'POCRTNMF' 
                     gfReplace("")
                   ENDIF   
                   SELECT 'Style_A'
                 ENDIF 
               ENDSCAN 
             ENDFOR 
          ELSE && More
            lnDiff = &lcTempShp..NCrtTot - RECCOUNT("CartonS") && Required to to be deleted 
            SELECT 'CartonS'
            lnCrCnt  = &lcTempShp..NCrtTot
            SCAN FOR lnCrCnt <> 0
              m.ccartonNo = CartonS.ccartonNo 
              lnCrCnt = lnCrCnt - 1
              lnStyCnt = 1
              lnLastCrt = 1
               m.nLINENO  = &lcTempShp..LINENO 
                lcCurrPO = &lcTempShp..PO
                lcCurrStyleMaj = &lcTempShp..Style
                m.CstyMajor  = SUBSTR(lcCurrStyleMaj,1,loFormSet.lnMajLen)
                m.PO = lcCurrPO 
                m.CShipNo = EVALUATE(loFormSet.lcShpHdrForUpdate+'.Shipno')
                m.Invoice = ''
                m.Piktkt = ''
                m.weight = loFormSet.lntotshpcrt 
              SELECT 'Style_A'
              lnStyCnt = 1
              =gfSeek(ALLTRIM(&lcTempShp..style) ,'Style_A','Style')
              SCAN REST WHILE Style =ALLTRIM(&lcTempShp..style)
                m.Rec_no = ''
                m.cedit_user = ''
                m.dedit_date = {}
                m.cedit_time = ''
                m.cedt_Ver = ''
                IF SEEK(lcCurrPO +'0001'+Style_A.Style,lcMasterScrFile) AND !DELETED(lcMasterScrFile)
                  =gfSeek("S"+Style_A.Scale,'Scale')    
                  m.Style = Style_A.Style
                  m.cwarecode = &lcMasterScrFile..cwarecode 
                  lnLastCrt = lnStyCnt 
                  m.ccartontyp = &lcTempShp..CCRTNVLTYP 
                  DIMENSION laCartRelated[3,2]
                  laCartRelated[1,1] = 'NCRTLENGTH'
                  laCartRelated[1,2] = 'lnLength'
                  laCartRelated[2,1] = 'NCRTWIDTH'
                  laCartRelated[2,2] = 'lnWIDTH'
                  laCartRelated[3,1] = 'NCRTHEIGHT'
                  laCartRelated[3,2] = 'lnHEIGHT'
                  STORE 0 TO lnHEIGHT,lnWIDTH,lnLength
                  llNoThing = gfRltFld(&lcTempShp..CCRTNVLTYP, @laCartRelated, "CCRTNVLTYP")
                  m.NDEPTH = lnHEIGHT   
                  m.NLENGTH   = lnLength
                  m.WIDTH  =   STR(lnWIDTH,6,2)
                  m.TOTQTY = 0
                  FOR lnT = 1 TO 8
                    lcT = STR(lnT ,1)
                    STORE 0 TO m.Qty&lcT. 
                  ENDFOR  
                  STORE '' TO m.Cadd_user,m.cadd_time 
                  FOR lnJ = 1 TO Scale.Cnt
                    lcJ = STR(lnJ,1)
                    lcStyCnt =ALLTRIM(STR(lnStyCnt))
                    IF &lcTempShp..QTY&lcStyCnt. > 0
                      m.QTY&lcJ. = &lcTempShp..QTY&lcStyCnt. 
                      m.TOTQTY = m.TOTQTY +  m.QTY&lcJ.
                    ENDIF 
                    lnStyCnt = lnStyCnt + 1
                  ENDFOR  
                  IF m.TOTQTY <> 0 
                    INSERT INTO 'POCRTNMF' FROM MEMVAR 
                    =gfAdd_Info('POCRTNMF')
                    SELECT  'POCRTNMF' 
                    gfReplace("")
                  ENDIF   
                  SELECT 'Style_A'
                ENDIF 
               ENDSCAN 
             ENDSCAN     
          ENDIF   
        ELSE && Same Number of Cartons 
            SELECT 'CartonS'
           SCAN 
               m.ccartonNo = CartonS.ccartonNo 
               lnStyCnt = 1
               lnLastCrt = 1
               SELECT 'Style_A'
                m.nLINENO  = &lcTempShp..LINENO 
                lcCurrPO = &lcTempShp..PO
                lcCurrStyleMaj = &lcTempShp..Style
                m.CstyMajor  = SUBSTR(lcCurrStyleMaj,1,loFormSet.lnMajLen)
                m.PO = lcCurrPO 
                m.CShipNo = EVALUATE(loFormSet.lcShpHdrForUpdate+'.Shipno')
                m.Invoice = ''
                m.Piktkt = ''
                m.weight = loFormSet.lntotshpcrt 
                =gfSeek(ALLTRIM(&lcTempShp..style) ,'Style_A','Style')
                SELECT 'Style_A'
                lnStyCnt = 1
                SCAN REST WHILE Style =ALLTRIM(&lcTempShp..style)
                 m.Rec_no = ''
                 m.cedit_user = ''
                 m.dedit_date = {}
                 m.cedit_time = ''
                 m.cedt_Ver = ''
                 IF SEEK(&lcTempShp..PO+'0001'+Style_A.Style,lcMasterScrFile) AND !DELETED(lcMasterScrFile)
                   =gfSeek("S"+Style_A.Scale,'Scale')    
                   m.Style = Style_A.Style
                   m.cwarecode = &lcMasterScrFile..cwarecode 
                   lnLastCrt = lnStyCnt 
                   m.ccartontyp = &lcTempShp..CCRTNVLTYP 
                   DIMENSION laCartRelated[3,2]
                   laCartRelated[1,1] = 'NCRTLENGTH'
                   laCartRelated[1,2] = 'lnLength'
                   laCartRelated[2,1] = 'NCRTWIDTH'
                   laCartRelated[2,2] = 'lnWIDTH'
                   laCartRelated[3,1] = 'NCRTHEIGHT'
                   laCartRelated[3,2] = 'lnHEIGHT'
                   STORE 0 TO lnHEIGHT,lnWIDTH,lnLength
                   llNoThing = gfRltFld(&lcTempShp..CCRTNVLTYP, @laCartRelated, "CCRTNVLTYP")
                   m.NDEPTH = lnHEIGHT   
                   m.NLENGTH   = lnLength
                   m.WIDTH  =   STR(lnWIDTH,6,2)
                   m.TOTQTY = 0
                   FOR lnT = 1 TO 8
                     lcT = STR(lnT ,1)
                     STORE 0 TO m.Qty&lcT. 
                    ENDFOR  
                    STORE '' TO m.Cadd_user,m.cadd_time 
                   FOR lnJ = 1 TO Scale.Cnt
                     lcJ = STR(lnJ,1)
                     lcStyCnt =ALLTRIM(STR(lnStyCnt))
                     IF &lcTempShp..QTY&lcStyCnt. > 0
                       m.QTY&lcJ. = &lcTempShp..QTY&lcStyCnt. 
                       m.TOTQTY = m.TOTQTY +  m.QTY&lcJ.
                     ENDIF 
                     lnStyCnt = lnStyCnt + 1
                   ENDFOR  
                   IF m.TOTQTY <> 0 
                     INSERT INTO 'POCRTNMF' FROM MEMVAR 
                     =gfAdd_Info('POCRTNMF')
                     SELECT  'POCRTNMF' 
                     gfReplace("")
                   ENDIF   
                   SELECT 'Style_A'
                 ENDIF 
               ENDSCAN 
             ENDSCAN  
        ENDIF   
        SELECT 'pocrtnmf'
        gfSetOrder('pocrtnmf')
      ENDIF 
    ENDIF 
  ENDSCAN 
  SELECT 'POCRTNMF' 
  gfTableUpdate()
  SET DELETED &lcDele 
ENDIF   
*!*************************************************************
*! Name      : lfRMVSHIPMT
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : remove cutom file lines when delete shipment
*!*************************************************************
FUNCTION lfRMVSHIPMT
lnSlct = SELECT()
IF !USED('POCRTNMF')
  =gfOpenTable('POCRTNMF','POCRTNMF')
ENDIF
SELECT 'POCRTNMF'
=gfSetOrder('pocrtshp')
lcShip = loFormSet.ariaform1.kBShipNo.keytextbox.Value 
IF gfSEEK(lcShip,'POCRTNMF')
  SELECT POCRTNMF
  SCAN REST WHILE cshipno+ po+ STR(nlineno,6)+ style+ ccartontyp= lcShip
    gfDELETE()
  ENDSCAN 
  gfTableUpdate()
ENDIF
SELECT (lnSlct)
*!*************************************************************
*! Name      : lfADDCRTMNU
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Add Menu Bar to PO Shipment screen
*!*************************************************************
FUNCTION lfADDCRTMNU
lcHostFormName = '[' + loFormSet.cHostFormName + ']'

DEFINE BAR 4 OF _INQURYPOP PROMPT 'Carton Details' SKIP FOR  (gfFormIsActive(&lcHostFormName) .AND. (_screen.ActiveForm.Parent.activemode $ 'SA')) 

*!*************************************************************
*! Name      : lfOPNCRTSCR
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Opens Carton Screen from PO Shipment screen
*!*************************************************************
FUNCTION lfOPNCRTSCR

IF BAR() = 4

  IF EVALUATE(loFormSet.lcshphdr+'.Status') $ 'XC'
    RETURN 
  ENDIF 
  DO FORM (oAriaApplication.ScreenHome+"\po\pocrtinf.scx") WITH loFormSet
ENDIF 

*!*************************************************************
*! Name      : lfGetShipInfor
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Get Carton info. from SQL
*!*************************************************************
FUNCTION lfGetShipInfor
PARAMETERS loShipForm
lcMasterScrFile = IIF(loShipForm.loparentform.ActiveMode $ 'EV',loShipForm.loparentform.lcShpLine,loShipForm.loparentform.lcShpLineForUpdate)
lnMajorLen = loShipForm.loparentform.lnMajLen
lnClrLen  = loShipForm.loparentform.lnClrlen 
lnClrPos  =loShipForm.loparentform.lnClrStrt
loShipForm.loparentform.lnCrtCnt = 0
lnStyClrLen  = lnClrLen  +lnClrPos-1  
lcPOStyle = loShipForm.loparentform.lcPOStyle 
lcScaleFile = loShipForm.loparentform.lcScaleFile
loShipForm.lcPOStyle = lcPOStyle
loShipForm.lcScaleFile = lcScaleFile
lcTempShp=loShipForm.loParentForm.lcTempShp
loShipForm.lcTempShp= lcTempShp



DIMENSION laTempFileStr[9,4]
laTempFileStr[1,1] = 'Style'
laTempFileStr[1,2] = 'C'
laTempFileStr[1,3] = lnStyClrLen
laTempFileStr[1,4] = 0

laTempFileStr[2,1] = 'PO'
laTempFileStr[2,2] = 'C'
laTempFileStr[2,3] = 6
laTempFileStr[2,4] = 0

laTempFileStr[3,1] = 'nSclCnt'
laTempFileStr[3,2] = 'N'
laTempFileStr[3,3] = 5
laTempFileStr[3,4] = 0


laTempFileStr[4,1] = 'DESC1'
laTempFileStr[4,2] = 'C'
laTempFileStr[4,3] = 60
laTempFileStr[4,4] = 0

laTempFileStr[5,1] = 'nCrtTyp'
laTempFileStr[5,2] = 'N'
laTempFileStr[5,3] = 5
laTempFileStr[5,4] = 0

laTempFileStr[6,1] = 'nTotQty'
laTempFileStr[6,2] = 'N'
laTempFileStr[6,3] = 7
laTempFileStr[6,4] = 0

laTempFileStr[7,1] = 'nCrtQTY'
laTempFileStr[7,2] = 'N'
laTempFileStr[7,3] = 7
laTempFileStr[7,4] = 0

laTempFileStr[8,1] = 'nTotalQ'
laTempFileStr[8,2] = 'N'
laTempFileStr[8,3] = 9
laTempFileStr[8,4] = 0

laTempFileStr[9,1] = 'nLastLn'
laTempFileStr[9,2] = 'N'
laTempFileStr[9,3] = 6
laTempFileStr[9,4] = 0

=gfCrtTmp(lcPOStyle ,@laTempFileStr,[Style+PO],lcPOStyle)



SELECT (lcPOStyle)
INDEX on PO+Style TAG 'POSTYLE'
SET ORDER TO (lcPOStyle)
SELECT (lcMasterScrFile)
SCAN
  IF !SEEK(SUBSTR(Style,1,lnStyClrLen)+PO,lcPOStyle)  
    m.Style = SUBSTR(Style,1,lnStyClrLen)
    m.PO = Po
    INSERT INTO (lcPOStyle) FROM MEMVAR 
  ENDIF 
ENDSCAN 

lnMaxScl = 0
IF !USED('Style_A')
  =gfOpenTable('Style','Style','SH','Style_A')
ENDIF 
IF !USED('Scale')
  =gfOpenTable('Scale','Scale')
ENDIF 

*Scan to collect upper grid info. and get Max scale count used.
SELECT (lcPOStyle)
SET ORDER TO 'POSTYLE'
LOCATE 
lnPOCnt = 0
SCAN 
  lnPOCnt = 0
  lcCurrStyleMaj = Style
  lcCurrPO = PO
  =gfSeek(ALLTRIM(lcCurrStyleMaj) ,'Style_A','Style')
  SELECT 'Style_A'
  REPLACE DESC1 WITH Style_A.Desc1 IN (lcPOStyle)
  SCAN REST WHILE Style =ALLTRIM(lcCurrStyleMaj)
    IF SEEK(lcCurrPO +'0001'+Style_A.Style,lcMasterScrFile) 
      =gfSeek("S"+Style_A.Scale,'Scale')    
      lnPOCnt = lnPOCnt + Scale.cnt 
      REPLACE  nTotQty WITH nTotQty + &lcMasterScrFile..totqty IN (lcPOStyle)
    ENDIF 
  ENDSCAN
  REPLACE nSclCnt WITH lnPOCnt in (lcPOStyle)
   
  IF lnPOCnt  > lnMaxScl
    lnMaxScl = lnPOCnt  
  ENDIF 
ENDSCAN 


*Scan to get the scale sizes used for each style color PO
IF lnMaxScl = 0
  RETURN .F.
ENDIF 
DIMENSION laSclFile[lnMaxScl +2 ,4]

laSclFile[1,1] = 'Style'
laSclFile[1,2] = 'C'
laSclFile[1,3] = lnStyClrLen
laSclFile[1,4] = 0

laSclFile[2,1] = 'PO'
laSclFile[2,2] = 'C'
laSclFile[2,3] = 6
laSclFile[2,4] = 0

lnCnt = 3

FOR lnI = 1 TO lnMaxScl 
  laSclFile[lnCnt ,1] = 'Scl'+ALLTRIM(STR(lnI))
  laSclFile[lnCnt ,2] = 'C'
  laSclFile[lnCnt ,3] = 5
  laSclFile[lnCnt ,4] = 0
  lnCnt = lnCnt + 1
ENDFOR 

=gfCrtTmp(lcScaleFile ,@laSclFile,[PO+Style],lcScaleFile )

SELECT (lcPOStyle)
SET ORDER TO 'POSTYLE'
LOCATE 
SCAN 
  IF SEEK(Style+PO,lcScaleFile )
    LOOP 
  ENDIF 
  lcCurrStyleMaj = Style
  lcCurrPO = PO
  =gfSeek(ALLTRIM(lcCurrStyleMaj) ,'Style_A','Style')
  SELECT 'Style_A'
  m.Style  = lcCurrStyleMaj
  m.PO = lcCurrPO
  lnCnter = 1
  
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
  FOR lnO = 1 TO lnMaxScl 
    lcO = ALLTRIM(STR(lnO))
    STORE '' TO m.Scl&lcO
  ENDFOR 
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
  
  SCAN REST WHILE Style =ALLTRIM(lcCurrStyleMaj)
    IF SEEK(lcCurrPO +'0001'+Style_A.Style,lcMasterScrFile) 
      =gfSeek("S"+Style_A.Scale,'Scale')    
      FOR lnI = 1 TO Scale.cnt 
        lcI = ALLTRIM(STR(lnI))
        lcCnter = ALLTRIM(STR(lnCnter))
        m.Scl&lcCnter  = Scale.Sz&lcI. 
        lnCnter = lnCnter + 1
      ENDFOR 
    ENDIF 
    *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
*!*	    IF !SEEK(m.PO+m.Style,lcScaleFile )
*!*	      INSERT  INTO (lcScaleFile ) FROM MEMVAR 
*!*	    ELSE
*!*	      SELECT(lcScaleFile)
*!*	      GATHER MEMO MEMVAR  
*!*	    ENDIF 
    *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
  ENDSCAN
  IF !SEEK(m.PO+m.Style,lcScaleFile )
      INSERT  INTO (lcScaleFile ) FROM MEMVAR 
    ELSE
      SELECT(lcScaleFile)
      GATHER MEMO MEMVAR  
    ENDIF 
ENDSCAN 




SELECT (lcPOStyle)
LOCATE 


lcShip = loShipForm.loParentForm.ariaform1.kBShipNo.keytextbox.Value 
lfAddCntrlSrc(loShipForm)


IF loShipForm.loparentform.ActiveMode = 'E' AND USED(loShipForm.lcTempShp) AND recc(loShipForm.lcTempShp)>0
  SELECT (lcPOStyle)
  SCAN 
    SELECT (lcTempShp)
    SET KEY TO 
    =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
    SCAN REST WHILE PO+Style+STR(LINENO,6) =&lcPOStyle..PO+&lcPOStyle..Style
      lfCalcQty(loShipForm,.T.)
    ENDSCAN 
  ENDSCAN 
  SELECT (lcPOStyle)
  LOCATE   
  RETURN 
ENDIF 

lfCrtTempFl()


IF loShipForm.loparentform.ActiveMode $ 'EV'
  *Fill Temp File from SQL Table
  IF !USED('POCRTNMF')
    =gfOpenTable('POCRTNMF','pocrtnmf')
  ENDIF 
  IF !USED('STYLE_A')
    =gfOpenTable('STYLE','STYLE','SH','STYLE_A')
  ENDIF 
  SELECT 'POCRTNMF'
  gfSetOrder('pocrtshp')
  =gfSeek(lcShip,'POCRTNMF','pocrtshp')
  SELECT (lcPOStyle)
  SCAN 
    SELECT 'POCRTNMF'
    m.sTYLE = &lcPOStyle..Style
    M.po = &lcPOStyle..po
    IF SEEK(lcShip+&lcPOStyle..po)
      SELECT Distinct CCARTONTYP FROM 'POCRTNMF' WHERE CSHIPNO+PO+Str(NLINENO,6)+STYLE+CCARTONTYP= lcShip+&lcPOStyle..PO AND Style =ALLTRIM(&lcPOStyle..Style)   INTO CURSOR 'TempCrt'
      lnInvCrtCnt = RECCOUNT('TempCrt')
      REPLACE nCrtTyp WITH lnInvCrtCnt  IN (lcPOStyle)
      
      SELECT 'POCRTNMF'
      =SEEK(lcShip+&lcPOStyle..po)
      lnCurrLnNo = -1
      DIMENSION laCartons[1]
      STORE '' TO laCartons
      SCAN REST WHILE CSHIPNO+PO+Str(NLINENO,6)+STYLE+CCARTONTYP= lcShip+ &lcPOStyle..PO FOR Style =ALLTRIM(&lcPOStyle..Style) 
        IF lnCurrLnNo = nlineno
          LOOP 
        ELSE
          DIMENSION laCartons[1]
          STORE '' TO laCartons
        ENDIF 
        lnCurrLnNo = nlineno
        lnCurrRec = RECNO()
        =SEEK(lcShip+&lcPOStyle..PO)
        lnCrtCnt = 0 
        lcCurrCrt = '  '
        lnSclCnt = 1
        FOR lnI = 1 TO lnMaxScl 
          lcI = ALLTRIM(STR(lnI))
          STORE 0 TO m.Qty&lcI.
        ENDFOR 
        STORE 0 TO m.TOTQTY
        lcStyle = POCRTNMF.Style
        SCAN REST WHILE CSHIPNO+PO+Str(NLINENO,6)+STYLE+CCARTONTYP= lcShip+&lcPOStyle..PO FOR nLINENO = lnCurrLnNo  AND sTYLE =ALLTRIM(&lcPOStyle..Style) 
          IF lcStyle = POCRTNMF.Style 
            lnSclCnt = 1
          ENDIF 
          m.CCRTNVLTYP = POCRTNMF.CCARTONTYP
          IF ASCAN(laCartons,CCARTONNo ,1) = 0
          *IF lcCurrCrt <> CCARTONNo 
            lnCrtCnt = lnCrtCnt + 1
          ENDIF 
          m.LineNo = POCRTNMF.NlineNo
          =gfSeek(POCRTNMF.Style,'STYLE_A')
          =gfSeek('S'+Style_A.Scale,'Scale') 
          SELECT Style_A
          =gfSeek(ALLTRIM(&lcPOStyle..Style),'STYLE_A')
          lnSclCnt = 1
          SCAN REST WHILE Style = ALLTRIM(&lcPOStyle..Style) FOR SEEK(&lcPOStyle..PO+'0001'+Style_A.Style,lcMasterScrFile) 
            =gfSeek('S'+Style_A.Scale,'Scale') 
            IF Style_A.Style = POCRTNMF.Style
              FOR lnZ = 1 TO Scale.cnt
                lcSclCnt =ALLTRIM(STR(lnSclCnt))
                lcZ = STR(lnZ,1)
                m.Qty&lcSclCnt. =  POCRTNMF.Qty&lcZ.
                m.TOTQTY = m.TOTQTY + m.Qty&lcSclCnt. 
                lnSclCnt = lnSclCnt +  1 
              ENDFOR 
              EXIT 
            ELSE
              lnSclCnt = lnSclCnt + Scale.cnt
            ENDIF  
          ENDSCAN  
          SELECT 'POCRTNMF' 
          IF EMPTY(laCartons[1])
            laCartons[1] = CCARTONNo 
          ELSE
            DIMENSION laCartons[ALEN(laCartons,1)+1]
            laCartons[ALEN(laCartons,1)] = CCARTONNo 
          ENDIF
          IF &lcPOStyle..nLastLn < POCRTNMF.NlineNo
            REPLACE nLastLn WITH POCRTNMF.NlineNo IN (lcPOStyle)
          ENDIF 
          lcStyle = POCRTNMF.Style 
        ENDSCAN 
        m.NCrtTot = lnCrtCnt 
        IF !SEEK(m.PO+m.Style+m.CCRTNVLTYP+STR(m.LINENO,6),lcTempShp)
          INSERT INTO (lcTempShp) FROM MEMVAR 
        ENDIF 
        SELECT 'POCRTNMF'
        IF BETWEEN(lnCurrRec,1,recc())
          GO RECORD lnCurrRec
        ENDIF 
      ENDSCAN 
      

    ENDIF 
  ENDSCAN 

  SELECT (lcPOStyle)
  SCAN 
    SELECT (lcTempShp)
    SET KEY TO 
    =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
    SCAN REST WHILE PO+Style+STR(LINENO,6) =&lcPOStyle..PO+&lcPOStyle..Style
      lfCalcQty(loShipForm,.T.)
    ENDSCAN 
  ENDSCAN 
  SELECT (lcPOStyle)
  LOCATE   
  loShipForm.ariaForm1.grdCrttype.refresh()
  SELECT (lcTempShp)
  IF loShipForm.loparentform.ActiveMode  = 'V' 
    loShipForm.AriaForm1.cmdRemove.enabled = .F.
    loShipForm.AriaForm1.CmdNew.enabled = .F.
    loShipForm.ariaForm1.grdCrttype.Readonly = .T.
    loShipForm.ariaForm1.grdCrttype.Column1.AriaCodes1.Enabled = .F.
  ENDIF 
ENDIF   


*!*************************************************************
*! Name      : lfCrtTempFl
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Create Temp. File
*!*************************************************************
FUNCTION lfCrtTempFl
DIMENSION laFilStru[lnMaxScl +7,4]
laFilStru[1,1] = 'PO'
laFilStru[1,2] = 'C'
laFilStru[1,3] = 6
laFilStru[1,4] = 0

laFilStru[2,1] = 'Style'
laFilStru[2,2] = 'C'
laFilStru[2,3] = lnStyClrLen
laFilStru[2,4] = 0

laFilStru[3,1] = 'CCRTNVLTYP'
laFilStru[3,2] = 'C'
laFilStru[3,3] = 6
laFilStru[3,4] = 0

laFilStru[4,1] = 'NCrtTot'
laFilStru[4,2] = 'N'
laFilStru[4,3] = 6
laFilStru[4,4] = 0

laFilStru[5,1] = 'TOTQty'
laFilStru[5,2] = 'N'
laFilStru[5,3] = 9
laFilStru[5,4] = 0

laFilStru[6,1] = 'LINENO'
laFilStru[6,2] = 'N'
laFilStru[6,3] = 6
laFilStru[6,4] = 0

laFilStru[7,1] = 'Cstatus'
laFilStru[7,2] = 'C'
laFilStru[7,3] = 1
laFilStru[7,4] = 0

lnCnt = 8

FOR lnI = 1 TO lnMaxScl 
  laFilStru[lnCnt ,1] = 'Qty'+ALLTRIM(STR(lnI))
  laFilStru[lnCnt ,2] = 'N'
  laFilStru[lnCnt ,3] = 7
  laFilStru[lnCnt ,4] = 0
  lnCnt = lnCnt + 1
ENDFOR 
=gfCrtTmp(lcTempShp,@laFilStru,[PO+Style+STR(LINENO,6)],lcTempShp)  


*!*************************************************************
*! Name      : lfADDFRMPROP
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Add Properties to Shipemnt screen
*!*************************************************************
FUNCTION lfADDFRMPROP
loformset.AddProperty ('lnMajLen',0)
loformset.AddProperty ('lnClrlen',0)
loformset.AddProperty ('lnClrStrt',0)
loformset.AddProperty ('lnCrtCnt',0)
loformset.AddProperty ('lntotshpcrt',0)

loformset.AddProperty ('lcTempShp',gfTempName())
loformset.AddProperty ('lcScaleFile',gfTempName())
loformset.AddProperty ('lcPOStyle',gfTempName())



STORE 0 TO lnClrLen,lnClrPos,lnSclLen ,lnSclPos 
  
DIMENSION laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])    
     
  CASE  laItemSeg[lnCount,1]='S'
    lnSclLen = LEN(laItemSeg[lnCount,3])
    lnSclPos = laItemSeg[lnCount,4]
    
  ENDCASE  
ENDFOR

loformset.lnMajLen = LEN(gfItemMask("PM","",'0001'))
loformset.lnClrlen =lnClrLen 
loformset.lnClrStrt=lnClrPos 


*!*************************************************************
*! Name      : lfAddCntrlSrc
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Add Grids control source
*!*************************************************************
FUNCTION lfAddCntrlSrc
PARAMETERS loChldForm
lcMasterScrFile = IIF(loChldForm.loparentform.ActiveMode $ 'EV',loChldForm.loparentform.lcShpLine,loChldForm.loparentform.lcShpLineForUpdate)
loChldForm.ariaForm1.grdPohead.RecordSource = lcPOStyle
loChldForm.ariaForm1.grdPohead.Column1.controlsource = lcPOStyle+'.PO'
loChldForm.ariaForm1.grdPohead.Column2.controlsource = lcPOStyle+'.Style'
loChldForm.ariaForm1.grdPohead.Column3.controlsource = lcPOStyle+'.nCrtTyp'
loChldForm.ariaForm1.grdPohead.Column4.controlsource = lcPOStyle+'.nTotQty'
loChldForm.ariaForm1.grdPohead.Column5.controlsource = lcPOStyle+'.nCrtQTY'
loChldForm.ariaForm1.grdPohead.readonly = .T.

*!*************************************************************
*! Name      : lfAftrPOChngd
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : After row column change of PO grid
*!*************************************************************
FUNCTION lfAftrPOChngd
PARAMETERS loChldForm



lcTempShp = loChldForm.lcTempShp
lcScaleFile = loChldForm.lcScaleFile
lcPOStyle = loChldForm.lcPOStyle

IF loChldForm.lntotshpcrt > 0 
  loChldForm.ariaForm1.txtCrtWght.Value  =  IIF(loChldForm.loParentForm.ActiveMode = 'A',EVALUATE(loChldForm.loParentForm.lcShpHdrForUpdate+'.NGRSWGHT'),EVALUATE(loChldForm.loParentForm.lcShpHdr+'.NGRSWGHT'))/loChldForm.lntotshpcrt 
ELSE
  loChldForm.ariaForm1.txtCrtWght.Value  =  0
ENDIF   
loChldForm.loParentForm.lntotshpcrt =loChldForm.ariaForm1.txtCrtWght.Value


SELECT (loChldForm.lcpostyle)
lnReCNum = RECNO()
IF loChldForm.lnCurrRec <> lnReCNum 
  IF BETWEEN(loChldForm.lnCurrRec ,1,RECCOUNT())
    GO RECORD loChldForm.lnCurrRec 
    IF &lcPOStyle..nCrtQTY <> &lcPOStyle..nTotQty
    ELSE
      IF BETWEEN(loChldForm.lnCurrRec ,1,RECCOUNT())
        GO RECORD lnReCNum 
      ENDIF 
    ENDIF 
  ENDIF 
ENDIF   





loChldForm.ariaForm1.kbPO.keytextbox.Value = &lcPOStyle..PO
loChldForm.ariaForm1.kbstyle.keytextbox.Value  = &lcPOStyle..Style
loChldForm.ariaForm1.txtStyDesc.Value  =&lcPOStyle..DESC1
loChldForm.ariaForm1.txtAlloc.Value =&lcPOStyle..nCrtQTY
loChldForm.ariaForm1.txtRcv.Value   =&lcPOStyle..nTotQty




SET KEY TO &lcPOStyle..PO+&lcPOStyle..Style IN (lcScaleFile)
SET KEY TO &lcPOStyle..PO+&lcPOStyle..Style IN (lcTempShp)

*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
SELECT(lcScaleFile)
LOCATE 
*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]


SELECT (lcTempShp)
LOCATE 
IF EOF() OR loChldForm.loparentform.ActiveMode = 'V'
  loChldForm.ariaForm1.cmdRemove.Enabled = .F.
ELSE
  loChldForm.ariaForm1.cmdRemove.Enabled = .T.
ENDIF 


loChldForm.ariaForm1.grdCrttype.RecordSource = ''
loChldForm.ariaForm1.grdCrttype.RecordSource = lcTempShp
loChldForm.ariaForm1.grdCrttype.ColumnCount = 1
loChldForm.ariaForm1.grdCrttype.ColumnCount = &lcPOStyle..nSclCnt + 3
loChldForm.ariaForm1.grdCrttype.Column1.sparse = .F.
loChldForm.ariaForm1.grdCrttype.Column1.dynamiccurrentcontrol= "IIF(UPPER(ALLTRIM(EVALUATE(ThisFormset.lcTempShp+'.CCRTNVLTYP')))='TOTALS','Text1','AriaCodes1')"
loChldForm.ariaForm1.grdCrttype.Column1.controlsource = lcTempShp+'.CCRTNVLTYP'
loChldForm.ariaForm1.grdCrttype.Column1.ReadOnly = IIF(loChldForm.loparentform.ActiveMode $ "EA",.F.,.T.)
loChldForm.ariaForm1.grdCrttype.Column1.Header1.caption = 'Carton Type'
loChldForm.ariaForm1.grdCrttype.Column1.Text1.enabled = .F.
loChldForm.ariaForm1.grdCrttype.Column1.Text1.DisabledForeColor = RGB(0,0,0)


lnCnter = 2
FOR lnI = 1 TO &lcPOStyle..nSclCnt
  lcCntr =ALLTRIM(STR(lnCnter))
  lcI = ALLTRIM(STR(lnI))
  
  loChldForm.ariaForm1.grdCrttype.Column&lcCntr..controlsource = lcTempShp+'.Qty'+lcI 
  loChldForm.ariaForm1.grdCrttype.Column&lcCntr..Header1.caption = &lcScaleFile..Scl&lcI.
  loChldForm.ariaForm1.grdCrttype.Column&lcCntr..Readonly = IIF(loChldForm.loparentform.ActiveMode $ "EA",.F.,.T.)
  
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
  loChldForm.ariaForm1.grdCrttype.Column&lcCntr..wIDTH = 50
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
  
  lnCnter = lnCnter + 1
  *IF loChldForm.loparentform.ActiveMode $ "EA"
    BINDEVENT(loChldForm.ariaForm1.grdCrttype.Column&lcCntr..Text1,'LostFocus',loChldForm,'lfCalcQty')
    BINDEVENT(loChldForm.ariaForm1.grdCrttype.Column&lcCntr..Text1,'GotFocus',loChldForm,'lfChkFld')
  *ENDIF 
ENDFOR 

lcCntr =ALLTRIM(STR(lnCnter))
loChldForm.ariaForm1.grdCrttype.Column&lcCntr..controlsource = lcTempShp+'.NCrtTot'
loChldForm.ariaForm1.grdCrttype.Column&lcCntr..Header1.caption = 'No. of Cartons'
loChldForm.ariaForm1.grdCrttype.Column&lcCntr..Readonly = IIF(loChldForm.loparentform.ActiveMode $ "EA",.F.,.T.)
*IF loChldForm.loparentform.ActiveMode $ "EA"
  BINDEVENT(loChldForm.ariaForm1.grdCrttype.Column&lcCntr..Text1,'LostFocus',loChldForm,'lfCalcQty')
  BINDEVENT(loChldForm.ariaForm1.grdCrttype.Column&lcCntr..Text1,'GotFocus',loChldForm,'lfChkFld')
*ENDIF 
lnCnter = lnCnter + 1
lcCntr =ALLTRIM(STR(lnCnter))
loChldForm.ariaForm1.grdCrttype.Column&lcCntr..controlsource = lcTempShp+'.TOTQty'
loChldForm.ariaForm1.grdCrttype.Column&lcCntr..Header1.caption = 'Total Quantity'
loChldForm.ariaForm1.grdCrttype.Column&lcCntr..Readonly = .T.

*!*************************************************************
*! Name      : lfAddNew
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Add new line
*!*************************************************************
FUNCTION lfAddNew
PARAMETERS loChldFormSet

lcPOStyle = loChldFormSet.lcPOStyle
lcTempShp = loChldFormSet.lcTempShp


SELECT(lcTempShp)
lnLastLine = 0
SCAN FOR UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS'
  IF lnLastLine < LinenO
    lnLastLine = LinenO
  ENDIF 
ENDSCAN 
REPLACE nLastLn WITH lnLastLine  IN (lcPOStyle )


SELECT(lcTempShp)
APPEND BLANK 

REPLACE CCRTNVLTYP WITH loChldFormSet.ariaForm1.grdCrttype.Column1.AriaCodes1.codedefaultvalue,;
            Style  WITH &lcPOStyle..Style,;
            PO     WITH &lcPOStyle..PO,;
            LINENO WITH &lcPOStyle..nLastLn+1 ,;
            Cstatus WITH 'N'
*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
IF !EMPTY(loChldFormSet.lcLastType)
  REPLACE CCRTNVLTYP WITH loChldFormSet.lcLastType
ENDIF 
*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]

REPLACE nLastLn WITH nLastLn+1 IN (lcPOStyle)

lnRecNo= RECNO()
LOCATE FOR PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND UPPER(ALLTRIM(CCRTNVLTYP)) = 'TOTALS'
IF !FOUND()
  LOCATE 
  =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  SUM TOTQty,NCrtTot REST WHILE PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style FOR UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' TO lnTotQty,lnTotCrt
  =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  
  SELECT  distinct CCRTNVLTYP FROM (lcTempShp) WHERE  PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND  UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' INTO ARRAY  laInvCnt
  lnCrtTyp  = _tally  
  APPEND BLANK  
  REPLACE CCRTNVLTYP WITH 'Totals',;
            Style    WITH &lcPOStyle..Style,;
            PO       WITH &lcPOStyle..PO,;
            NCrtTot  WITH lnTotCrt,;
            TOTQty   WITH lnTotQty,;
            Lineno   WITH 99999
            
  
  

  REPLACE nCrtQTY WITH lnTotQty,;
          nCrtTyp WITH lnCrtTyp IN (lcPOStyle) 
  loChldFormSet.ariaForm1.txtAlloc.Value =&lcPOStyle..nCrtQTY

            
  FOR lnI = 1 TO &lcPOStyle..nSclCnt
    lcI = ALLTRIM(STR(lnI))
    REPLACE QTY&lcI. WITH 0 
  ENDFOR              
ELSE
  LOCATE 
  =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  SUM TOTQty ,NCrtTot REST WHILE PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style FOR UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' TO lnTotQty,lnTotCrt
  
  =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  
  SELECT  distinct CCRTNVLTYP FROM (lcTempShp) WHERE  PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND  UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' INTO ARRAY  laInvCnt
  lnCrtTyp  = _tally  


  
  LOCATE FOR PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND UPPER(ALLTRIM(CCRTNVLTYP)) = 'TOTALS'
  IF FOUND()
    REPLACE NCrtTot WITH lnTotCrt,;
            TOTQty with lnTotQty

   REPLACE nCrtQTY WITH lnTotQty ,;
           nCrtTyp WITH lnCrtTyp  IN (lcPOStyle) 
   loChldFormSet.ariaForm1.txtAlloc.Value =&lcPOStyle..nCrtQTY

            
    FOR lnI = 1 TO &lcPOStyle..nSclCnt
       lcI = ALLTRIM(STR(lnI))
      REPLACE QTY&lcI. WITH 0 
    ENDFOR              
  ENDIF   
ENDIF 
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO RECORD lnRecNo 
ENDIF 
loChldFormSet.ariaForm1.grdPohead.refresh
loChldFormSet.ariaForm1.grdCrttype.refresh

*!*************************************************************
*! Name      : lfCalcQty
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Calc. QTY
*!*************************************************************
FUNCTION lfCalcQty
PARAMETERS loChldForm,llFromPrg
lcPOStyle = loChldForm.lcPOStyle
lcTempShp = loChldForm.lcTempShp
lnTotal = 0
FOR lnI = 1 TO &lcPOStyle..nSclCnt
  lcI = ALLTRIM(STR(lnI))
  lnTotal = lnTotal +  EVALUATE(lcTempShp+'.Qty'+lcI)
ENDFOR 
REPLACE TOTQty WITH lnTotal * NCrtTot IN (lcTempShp)

SELECT(lcTempShp)
IF !llFromPrg
  REPLACE Cstatus WITH IIF(Cstatus <> 'N','M',Cstatus)
ENDIF   
lnRecNo= RECNO()
LOCATE FOR PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND UPPER(ALLTRIM(CCRTNVLTYP)) = 'TOTALS'
IF !FOUND()
  LOCATE 
  =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  SUM TOTQty ,NCrtTot REST WHILE PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style FOR UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' TO lnTotQty,lnTotCrt
  
  =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  SELECT  distinct CCRTNVLTYP FROM (lcTempShp) WHERE  PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND  UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' INTO ARRAY  laInvCnt
  lnCrtTyp  = _tally  


  
  
  APPEND BLANK  
  REPLACE CCRTNVLTYP WITH 'Totals',;
            Style  WITH &lcPOStyle..Style,;
            PO     WITH &lcPOStyle..PO,;
            NCrtTot WITH lnTotCrt,;
            TOTQty with lnTotQty,;
            linenO WITH 99999

  REPLACE nCrtQTY WITH lnTotQty,;
          nCrtTyp WITH lnCrtTyp IN (lcPOStyle) 
  loChldForm.ariaForm1.txtAlloc.Value =&lcPOStyle..nCrtQTY

          
  FOR lnI = 1 TO &lcPOStyle..nSclCnt
    lcI = ALLTRIM(STR(lnI))
    REPLACE QTY&lcI. WITH 0 
  ENDFOR              
ELSE
  LOCATE 
  =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  SUM TOTQty ,NCrtTot REST WHILE PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style FOR UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' TO lnTotQty,lnTotCrt
  
  =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  SELECT  distinct CCRTNVLTYP FROM (lcTempShp) WHERE  PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND  UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' inTO ARRAY laInvCnt
  lnCrtTyp  = _tally  

  
  LOCATE FOR PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND UPPER(ALLTRIM(CCRTNVLTYP)) = 'TOTALS'
  IF FOUND()
    REPLACE NCrtTot WITH lnTotCrt,;
            TOTQty  WITH lnTotQty
            
    REPLACE nCrtQTY WITH lnTotQty ,;
            nCrtTyp WITH lnCrtTyp IN (lcPOStyle) 
            
    loChldForm.ariaForm1.txtAlloc.Value =&lcPOStyle..nCrtQTY

         
    FOR lnI = 1 TO &lcPOStyle..nSclCnt
       lcI = ALLTRIM(STR(lnI))
      REPLACE QTY&lcI. WITH 0 
    ENDFOR              
  ENDIF   
ENDIF 

  SELECT (lcTempShp)
  SET KEY TO 
  SUM NCrtTot FOR  UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' TO loChldForm.lntotshpcrt 
  IF loChldForm.lntotshpcrt > 0 
    loChldForm.ariaForm1.txtCrtWght.Value  =  EVALUATE(loChldForm.loParentForm.lcShpHdrForUpdate+'.NGRSWGHT')/loChldForm.lntotshpcrt 
  ELSE
    loChldForm.ariaForm1.txtCrtWght.Value  =  0
  ENDIF   
  loChldForm.loParentForm.lntotshpcrt =loChldForm.ariaForm1.txtCrtWght.Value
  SET KEY TO &lcPOStyle..PO+&lcPOStyle..Style IN (lcTempShp)


IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO RECORD lnRecNo 
ENDIF 

loChldForm.ariaForm1.grdCrttype.refresh
loChldForm.ariaForm1.grdPohead.refresh

*!*************************************************************
*! Name      : lfChkFld
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Check if field is editable
*!*************************************************************
FUNCTION lfChkFld
PARAMETERS loChldForm
lcTempShp = loChldForm.lcTempShp
IF UPPER(ALLTRIM(&lcTempShp..CCRTNVLTYP)) = 'TOTALS' AND loChldForm.loparentform.ActiveMode $ 'EA'
  KEYBOARD '{TAB}'   
ENDIF 
*!*************************************************************
*! Name      : lfLinRef
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Refesh lines
*!*************************************************************
FUNCTION lfLinRef
PARAMETERS loChldForm
lcPOStyle = loChldForm.lcPOStyle
lcTempShp = loChldForm.lcTempShp

IF !used(lcTempShp)
  RETURN 
ENDIF 

SELECT(lcTempShp)
lnRecNo= RECNO()
lnTotCnt = 0
=SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
COUNT REST WHILE PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' to lnTotCnt 

IF lnTotCnt > 0 AND !(loChldForm.loparentform.ActiveMode = 'V')
  loChldForm.ariaForm1.cmdRemove.Enabled = .T.
ELSE
  loChldForm.ariaForm1.cmdRemove.Enabled = .F.
ENDIF 

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO RECORD lnRecNo 
ENDIF 
*!*************************************************************
*! Name      : lfRemove
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : remove lines
*!*************************************************************
FUNCTION lfRemove
PARAMETERS  loChldForm

lcPOStyle = loChldForm.lcPOStyle
lcTempShp = loChldForm.lcTempShp
SELECT(lcTempShp)
IF UPPER(ALLTRIM(CCRTNVLTYP)) <>'TOTALS'
  lnCurCrtTot = &lcPOStyle..nCrtQTY
  REPLACE nCrtQTY WITH lnCurCrtTot - &lcTempShp..TOTQty IN (lcPOStyle) 
  loChldForm.ariaForm1.txtAlloc.Value =&lcPOStyle..nCrtQTY
  REPLACE Cstatus WITH IIF(Cstatus = 'N',"",'D')
  DELETE 
  IF EOF()
    LOCATE 
  ENDIF 
ENDIF   
SELECT(lcTempShp)
lnRecNo= RECNO()


SELECT (lcTempShp)
SET KEY TO 
SUM NCrtTot FOR  UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' TO loChldForm.lntotshpcrt 
IF loChldForm.lntotshpcrt > 0 
  loChldForm.ariaForm1.txtCrtWght.Value  =  EVALUATE(loChldForm.loParentForm.lcShpHdrForUpdate+'.NGRSWGHT')/loChldForm.lntotshpcrt 
ELSE
  loChldForm.ariaForm1.txtCrtWght.Value  =  0
ENDIF 
loChldForm.loParentForm.lntotshpcrt =loChldForm.ariaForm1.txtCrtWght.Value
SET KEY TO &lcPOStyle..PO+&lcPOStyle..Style IN (lcTempShp)



lnTotCnt = 0
=SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
COUNT REST WHILE PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' to lnTotCnt 

IF lnTotCnt = 0
  LOCATE FOR PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND UPPER(ALLTRIM(CCRTNVLTYP)) = 'TOTALS'
  IF FOUND()
    DELETE 
    loChldForm.ariaForm1.grdCrttype.refresh
    RETURN 
  ENDIF 
ENDIF
LOCATE FOR PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND UPPER(ALLTRIM(CCRTNVLTYP)) = 'TOTALS'
IF !FOUND()
  LOCATE 
  =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  SUM TOTQty ,NCrtTot REST WHILE PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style FOR UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' TO lnTotQty,lnTotCrt
  
   =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  SELECT  distinct CCRTNVLTYP FROM (lcTempShp) WHERE  PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND  UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' INTO ARRAY  laInvCnt
  lnCrtTyp  = _tally  

  
  
  APPEND BLANK  
  REPLACE CCRTNVLTYP WITH 'Totals',;
            Style  WITH &lcPOStyle..Style,;
            PO     WITH &lcPOStyle..PO,;
            NCrtTot WITH lnTotCrt,;
            TOTQty with lnTotQty,;
            linenO WITH 99999

  REPLACE nCrtQTY WITH lnTotQty,;
          nCrtTyp WITH lnCrtTyp IN (lcPOStyle) 
          
  loChldForm.ariaForm1.txtAlloc.Value =&lcPOStyle..nCrtQTY

          
  FOR lnI = 1 TO &lcPOStyle..nSclCnt
    lcI = ALLTRIM(STR(lnI))
    REPLACE QTY&lcI. WITH 0 
  ENDFOR              
ELSE
  LOCATE 
  =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  SUM TOTQty ,NCrtTot REST WHILE PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style FOR UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' TO lnTotQty,lnTotCrt
  
  =SEEK(&lcPOStyle..PO+&lcPOStyle..Style)
  SELECT  distinct CCRTNVLTYP FROM (lcTempShp) WHERE  PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND  UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' INTO ARRAY  laInvCnt
  lnCrtTyp  = _tally  

  
  LOCATE FOR PO+Style+STR(LINENO,6) = &lcPOStyle..PO+&lcPOStyle..Style AND UPPER(ALLTRIM(CCRTNVLTYP)) = 'TOTALS'
  IF FOUND()
    REPLACE NCrtTot WITH lnTotCrt,;
            TOTQty  WITH lnTotQty

   REPLACE nCrtQTY WITH lnTotQty,;
           nCrtTyp WITH lnCrtTyp  IN (lcPOStyle) 

   loChldForm.ariaForm1.txtAlloc.Value =&lcPOStyle..nCrtQTY

       
    FOR lnI = 1 TO &lcPOStyle..nSclCnt
       lcI = ALLTRIM(STR(lnI))
      REPLACE QTY&lcI. WITH 0 
    ENDFOR              
  ENDIF   
ENDIF 
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO RECORD lnRecNo 
ENDIF 
loChldForm.ariaForm1.grdCrttype.refresh
loChldForm.ariaForm1.grdPohead.refresh

*!*************************************************************
*! Name      : lfOk
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : OK Button function
*!*************************************************************
FUNCTION lfOk
PARAMETERS loChldForm
lcPOStyle = loChldForm.lcPOStyle
lcTempShp = loChldForm.lcTempShp
IF loChldForm.loParentForm.ActiveMode $ "EA"
  SELECT (lcPOStyle)
  LOCATE FOR nCrtQTY <> nTotQty
  IF FOUND()
    = gfModalGen('QRM00000B00000',.F.,.F.,.F.,'Total Quantity Received and Total Quantity Allocated  to Cartons are Not equal.') 
    loChldForm.ariaForm1.grdPohead.refresh
    loChldForm.ariaForm1.grdPohead.SetFocus
    RETURN  
  ENDIF 
ENDIF 


IF loChldForm.loParentForm.ActiveMode = "A"

  IF !USED('POCRTNMF')
    =gfOpenTable('POCRTNMF','pocrtnmf')
  ENDIF 

  lcMasterScrFile =  IIF(loChldForm.loparentform.ActiveMode $ 'EV',loChldForm.loparentform.lcShpLine,loChldForm.loparentform.lcShpLineForUpdate)
	SELECT(lcTempShp)
	SET KEY TO 
	
	*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
	COUNT FOR !DELETED() AND UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' AND TOTQty > 0 TO lnCrtRec
	LOCATE 
	opross = CREATEOBJECT('ariaprogressbar')  
  oPross.TotalProgress  =  lnCrtRec
  oPross.lblFirstLabel.Caption = 'Updating Cartons Info.'
  lnRecCounter = 0
  oPross.AutoCenter = .T.
  oPross.Show()
	*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
  
	SCAN FOR !DELETED() AND UPPER(ALLTRIM(CCRTNVLTYP)) <> 'TOTALS' AND TOTQty > 0
	
    *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
  	lnRecCounter = lnRecCounter + 1 
  	oPross.CurrentProgress(lnRecCounter )
	  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
    
    m.nLINENO  = LINENO 
    lcCurrPO = PO
    lcCurrStyleMaj = Style
    m.CstyMajor  = SUBSTR(lcCurrStyleMaj,1,loChldForm.loParentForm.lnMajLen)
    m.PO = lcCurrPO 
	  m.CShipNo = EVALUATE(loChldForm.loParentForm.lcShpHdrForUpdate+'.Shipno')
	  m.Invoice = ''
	  m.Piktkt = ''
	  m.weight = loChldForm.ariaForm1.txtCrtWght.Value
	  
    FOR lnL = 1 TO &lcTempShp..NCrtTot
      
      m.ccartonNo = gfsequence('CCARTONNO')
      =gfSeek(ALLTRIM(lcCurrStyleMaj) ,'Style_A','Style')
      SELECT 'Style_A'
      lnStyCnt = 1
      lnLastCrt = 1
	    SCAN REST WHILE Style =ALLTRIM(lcCurrStyleMaj)
	       m.Rec_no = ''
	       m.cedit_user = ''
	       m.dedit_date = {}
	       m.cedit_time = ''
	       m.cedt_Ver = ''
		    IF SEEK(lcCurrPO +'0001'+Style_A.Style,lcMasterScrFile) 
		      =gfSeek("S"+Style_A.Scale,'Scale')    
		      m.Style = Style_A.Style
		      m.cwarecode = &lcMasterScrFile..cwarecode 
  		    lnLastCrt = lnStyCnt 
		      m.ccartontyp = &lcTempShp..CCRTNVLTYP 
		      DIMENSION laCartRelated[3,2]
		      laCartRelated[1,1] = 'NCRTLENGTH'
		      laCartRelated[1,2] = 'lnLength'
		      laCartRelated[2,1] = 'NCRTWIDTH'
		      laCartRelated[2,2] = 'lnWIDTH'
		      laCartRelated[3,1] = 'NCRTHEIGHT'
		      laCartRelated[3,2] = 'lnHEIGHT'
		      STORE 0 TO lnHEIGHT,lnWIDTH,lnLength
		      llNoThing = gfRltFld(&lcTempShp..CCRTNVLTYP, @laCartRelated, "CCRTNVLTYP")
		      m.NDEPTH = lnHEIGHT   
		      m.NLENGTH   = lnLength
		      m.WIDTH  =   STR(lnWIDTH,6,2)
		      m.TOTQTY = 0
		      FOR lnT = 1 TO 8
		        lcT = STR(lnT ,1)
		        STORE 0 TO m.Qty&lcT. 
 	        ENDFOR  
 	        STORE '' TO m.Cadd_user,m.cadd_time 
		      FOR lnJ = 1 TO Scale.Cnt
  		      lcJ = STR(lnJ,1)
  		      lcStyCnt =ALLTRIM(STR(lnStyCnt))
  		      IF &lcTempShp..QTY&lcStyCnt. > 0
  		        m.QTY&lcJ. = &lcTempShp..QTY&lcStyCnt. 
  		        m.TOTQTY = m.TOTQTY +  m.QTY&lcJ.
  		      ENDIF 
            
  		      lnStyCnt = lnStyCnt + 1
   		    ENDFOR  
          IF m.TOTQTY <> 0 
     		    INSERT INTO 'POCRTNMF' FROM MEMVAR 
   	  	    =gfAdd_Info('POCRTNMF')
   		      SELECT  'POCRTNMF' 
   		      gfReplace("")
          ENDIF   
 		      SELECT 'Style_A'
   		  ENDIF 
	    ENDSCAN
      IF lnL  <> &lcTempShp..NCrtTot
        lnStyCnt = lnLastCrt  
      ENDIF   
    ENDFOR 
	ENDSCAN 
	*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
	oPross.Hide()
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
  SELECT 'POCRTNMF' 
  gfTableUpdate()
ENDIF 
loChldForm.Release()

*!*************************************************************
*! Name      : lfCheckPOQTY
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Check Po Qty
*!*************************************************************
FUNCTION lfCheckPOQTY
PARAMETERS loChldForm
lcPOStyle = loChldForm.lcPOStyle

IF &lcPOStyle..nCrtQTY <> &lcPOStyle..nTotQty
  RETURN .F.
ENDIF  

*!*************************************************************
*! Name      : lfLstRecSt
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Check Last Po Line QTY
*!*************************************************************
FUNCTION lfLstRecSt
PARAMETERS loChldForm
lcPOStyle = loChldForm.lcPOStyle
SELECT (lcPOStyle)

lnReCNum = RECNO()
IF loChldForm.lnCurrRec <> lnReCNum 
  IF BETWEEN(loChldForm.lnCurrRec ,1,RECCOUNT())
	GO RECORD loChldForm.lnCurrRec 
	IF &lcPOStyle..nCrtQTY <> &lcPOStyle..nTotQty
	  *= gfModalGen('QRM00000B00000',.F.,.F.,.F.,'Total Quantity Received and Total Quantity Allocated  to Cartons are Not equal.') 
	  RETURN .F.
	ELSE
	  IF BETWEEN(loChldForm.lnCurrRec ,1,RECCOUNT())
	    GO RECORD lnReCNum 
	  ENDIF 
	ENDIF 
  eNDIF 
endif 

*!*************************************************************
*! Name      : lfCHNGMODE  
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Change mode
*!*************************************************************
FUNCTION lfCHNGMODE  

IF loFormSet.ActiveMode $ 'AV'
 IF USED(loformset.lcTempShp)
   USE IN (loformset.lcTempShp)
 ENDIF 

 IF USED(loformset.lcScaleFile)
   USE IN (loformset.lcScaleFile)
 ENDIF 

 IF USED(loformset.lcPOStyle)
   USE IN (loformset.lcPOStyle)
 ENDIF 
  
ENDif   

*!*************************************************************
*! Name      : lfADBRPICK
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/11/2008
*! Purpose   : Add New Menu Bar in Allocation screen
*!*************************************************************
FUNCTION lfADBRPICK
lcHostFormName = '[' + loFormSet.cHostFormName + ']'

DEFINE BAR _SCREEN.ActiveForm.Parent.lnShavBar + 1 OF _INQURYPOP PROMPT '\<Pick Cartons' SKIP FOR  (gfFormIsActive(&lcHostFormName) .AND. (_screen.ActiveForm.Parent.activemode $ 'S' OR !_screen.ActiveForm.Parent.llgenpktk))

*!*************************************************************
*! Name      : lfALCARTON
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/11/2008
*! Purpose   : New Menu Bar in Allocation screen function
*!*************************************************************
FUNCTION lfALCARTON



IF BAR() = loFormSet.lnShavBar + 1 OR (TYPE('llFromSave') = 'L' AND llFromSave)
  IF !loFormSet.llgenpktk AND !(TYPE('llFromSave') = 'L' AND llFromSave)
    RETURN 
  ENDIF
  SELECT (loformset.lcStylePik)
  COUNT FOR !DELETED() TO lnNotDel
  IF lnNotDel = 0
    lfGETCRTINF()
  ENDIF 
*!*    IF loFormSet.ActiveMode = "E"
*!*      lcOrdln = loFormSet.lc_tmpordl
*!*      SELECT(lcOrdln)
*!*      lnCurrentRec = RECNO()

*!*      *-- Case New Pitkt is just generated 
*!*      IF !USED('POCRTNMF_A')
*!*        =gfOpenTable('POCRTNMF','POCRTNMF','SH','POCRTNMF_A')
*!*      ENDIF  
*!*       
*!*       
*!*      SELECT POCRTNMF
*!*      gfSetOrder('POCRTSTYLE')
*!*      SELECT (lcOrdln)
*!*      SCAN FOR picked &&OR ((TYPE('llFromSave') = 'L' AND llFromSave) AND CStatus = 'R')
*!*        m.PIKTKT = PIKTKT
*!*        m.nQtyPik = TOTPIK
*!*        m.Style = SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1)
*!*        m.cwarecode = &lcOrdln..cwarecode
*!*        m.CStyMajor = SUBSTR(&lcOrdln..Style,1,loFormSet.lnMajLen)
*!*        IF gfSeek(&lcOrdln..Style,'POCRTNMF')
*!*          SELECT POCRTNMF
*!*          SCAN REST WHILE STYLE+CCARTONTYP = &lcOrdln..Style FOR cwarecode  = &lcOrdln..cwarecode &&IIF(!EMPTY(m.PIKTKT),PIKTKT = m.PIKTKT,EMPTY(PIKTKT))
*!*            IF SEEK(m.PIKTKT+PADR(m.Style,19)+POCRTNMF.CCARTONTYP,loformset.lcStylePik)
*!*              SELECT (loformset.lcStylePik)
*!*              llFound = .F.
*!*              SCAN REST WHILE Piktkt+Style+CCARTONTYP  = m.PIKTKT+PADR(m.Style,19)+POCRTNMF.CCARTONTYP FOR QtyPerCrt = POCRTNMF.TOTQTY 
*!*                llFound = .T.
*!*                EXIT 
*!*              ENDSCAN
*!*     
*!*              IF llFound AND !SEEK(PADR(m.Style,19)+POCRTNMF.CCARTONNO+&lcOrdln..Style ,loformset.lcCartTemp)
*!*                IF !EMPTY(POCRTNMF.PIKTKT) AND m.PIKTKT=POCRTNMF.PIKTKT
*!*                  REPLACE nCartAll WITH nCartAll + 1 ,;
*!*                          nQtyAll WITH QtyPerCrt * nCartAll ,;
*!*                          nOldAll WITH nOldAll + 1 ,;
*!*                          nQty1Crt WITH nQty1Crt+POCRTNMF.QTY1,;
*!*                          nQty2Crt WITH nQty2Crt+POCRTNMF.QTY2,;
*!*                          nQty3Crt WITH nQty3Crt+POCRTNMF.QTY3,;
*!*                          nQty4Crt WITH nQty4Crt+POCRTNMF.QTY4,;
*!*                          nQty5Crt WITH nQty5Crt+POCRTNMF.QTY5,;
*!*                          nQty6Crt WITH nQty6Crt+POCRTNMF.QTY6,;
*!*                          nQty7Crt WITH nQty7Crt+POCRTNMF.QTY7,;
*!*                          nQty8Crt WITH nQty8Crt+POCRTNMF.QTY8;                            
*!*                          IN (loformset.lcStylePik)
*!*                ELSE
*!*                  IF EMPTY(POCRTNMF.PIKTKT)
*!*                    REPLACE nCartAvl WITH nCartAvl + 1 ,;
*!*                            nQty1Crt WITH  POCRTNMF.QTY1,;
*!*                            nQty2Crt WITH  POCRTNMF.QTY2,;
*!*                            nQty3Crt WITH  POCRTNMF.QTY3,;
*!*                            nQty4Crt WITH  POCRTNMF.QTY4,;
*!*                            nQty5Crt WITH  POCRTNMF.QTY5,;
*!*                            nQty6Crt WITH  POCRTNMF.QTY6,;
*!*                            nQty7Crt WITH  POCRTNMF.QTY7,;
*!*                            nQty8Crt WITH  POCRTNMF.QTY8; 
*!*                            IN (loformset.lcStylePik)
*!*                  ELSE
*!*                    LOOP           
*!*                  ENDIF   
*!*                ENDIF 
*!*                m.QtyPerCrt = POCRTNMF.TOTQTY
*!*                m.CCARTONNO = POCRTNMF.CCARTONNO
*!*                m.NQtyPerCrn =  POCRTNMF.TOTQTY
*!*                m.HStyle =&lcOrdln..Style 
*!*                INSERT INTO (loformset.lcCartTemp) FROM MEMVAR 
*!*              ELSE
*!*                IF !SEEK(PADR(m.Style,19)+POCRTNMF.CCARTONNO+ &lcOrdln..Style ,loformset.lcCartTemp)
*!*                  IF SEEK(PADR(m.Style,19)+POCRTNMF.CCARTONNO,loformset.lcCartTemp)
*!*                    IF SEEK(m.PIKTKT+PADR(m.Style,19)+POCRTNMF.CCARTONTYP,loformset.lcStylePik)
*!*                      *IF gfSeek(POCRTNMF.CCARTONTYP+POCRTNMF.CCARTONNO+PADR(SUBSTR(EVALUATE(loformset.lcStylePik+'.Style'),1,loformset.lnMajLen),19)+ALLTRIM(EVALUATE(loformset.lcStylePik+'.Style')),'POCRTNMF_A')
*!*                        *LOCATE FOR Style = loformset.lcCartTemp.HSTYLE
*!*                        lnTOTCrtSty = 0
*!*                        SELECT (loformset.lcCartTemp)
*!*                        SUM NQtyPerCrn REST WHILE Style+CCARTONNO+HStyle =PADR(m.Style,19)+POCRTNMF.CCARTONNO TO lnTOTCrtSty
*!*                        
*!*                        *m.NQtyPerCrn = = POCRTNMF.TOTQTY
*!*                        lnQTyTOtal = POCRTNMF_A.TOTQTY
*!*                      *ENDIF   
*!*                      SELECT (loformset.lcStylePik)
*!*                      SCAN REST WHILE Piktkt+Style+CCARTONTYP  = m.PIKTKT+PADR(m.Style,19)+POCRTNMF.CCARTONTYP FOR QtyPerCrt = lnTOTCrtSty
*!*                        REPLACE QtyPerCrt WITH QtyPerCrt +POCRTNMF.TOTQTY,;
*!*                        nQty1Crt WITH nQty1Crt + POCRTNMF.QTY1,;
*!*                        nQty2Crt with nQty2Crt + POCRTNMF.QTY2,;
*!*                        nQty3Crt with nQty3Crt + POCRTNMF.QTY3,;
*!*                        nQty4Crt with nQty4Crt + POCRTNMF.QTY4,;
*!*                        nQty5Crt with nQty5Crt + POCRTNMF.QTY5,;
*!*                        nQty6Crt with nQty6Crt + POCRTNMF.QTY6,;
*!*                        nQty7Crt with nQty7Crt +POCRTNMF.QTY7,;
*!*                        nQty8Crt with nQty8Crt + POCRTNMF.QTY8 
*!*                        EXIT      
*!*                      ENDSCAN
*!*                      m.CCARTONNO = POCRTNMF.CCARTONNO
*!*                      m.HStyle = &lcOrdln..Style 
*!*                      m.NQtyPerCrn =  POCRTNMF.TOTQTY
*!*                      m.CCARTONTYP = POCRTNMF.CCARTONTYP
*!*                      INSERT INTO (loformset.lcCartTemp) FROM MEMVAR 
*!*                      LOOP  
*!*                    ENDIF 
*!*                  ENDIF 
*!*                  
*!*                  SELECT (loformset.lcStylePik)  
*!*                  m.CCARTONTYP = POCRTNMF.CCARTONTYP
*!*                  m.QtyPerCrt = POCRTNMF.TOTQTY
*!*                  m.nQty1Crt = POCRTNMF.QTY1
*!*                  m.nQty2Crt = POCRTNMF.QTY2
*!*                  m.nQty3Crt = POCRTNMF.QTY3
*!*                  m.nQty4Crt = POCRTNMF.QTY4
*!*                  m.nQty5Crt = POCRTNMF.QTY5
*!*                  m.nQty6Crt = POCRTNMF.QTY6
*!*                  m.nQty7Crt = POCRTNMF.QTY7
*!*                  m.nQty8Crt = POCRTNMF.QTY8                            
*!*                  
*!*                  IF !EMPTY(POCRTNMF.PIKTKT) AND m.PIKTKT=POCRTNMF.PIKTKT
*!*                     m.nCartAll = 1
*!*                     m.nOldAll = 1
*!*                     m.nCartAvl = 0
*!*                     m.nQtyAll = m.QtyPerCrt
*!*                  ELSE
*!*                    IF EMPTY(POCRTNMF.PIKTKT)
*!*                      m.nCartAvl = 1 
*!*                      m.nCartAll = 0
*!*                      m.nOldAll  = 0
*!*                      m.nQtyAll  = 0
*!*  *!*                      m.nQty1Crt = 0
*!*  *!*                      m.nQty2Crt = 0
*!*  *!*                      m.nQty3Crt = 0
*!*  *!*                      m.nQty4Crt = 0
*!*  *!*                      m.nQty5Crt = 0
*!*  *!*                      m.nQty6Crt = 0
*!*  *!*                      m.nQty7Crt = 0
*!*  *!*                      m.nQty8Crt = 0
*!*                    ELSE
*!*                      LOOP 
*!*                    ENDIF   
*!*                  ENDIF   
*!*                  IF !SEEK(m.PIKTKT+PADR(m.Style,19),loformset.lcStylePik)
*!*                    m.cColor = gfCodDes(SUBSTR(&lcOrdln..Style,loFormSet.lnClrStrt,loFormSet.lnClrlen), 'COLOR     ')
*!*                  ELSE
*!*                    m.cColor =  ''
*!*                  ENDIF    
*!*                  APPEND BLANK 
*!*                  GATHER MEMO MEMVAR   
*!*                  m.CCARTONNO = POCRTNMF.CCARTONNO
*!*                  m.QtyPerCrt = POCRTNMF.TOTQTY
*!*                  m.HStyle =&lcOrdln..Style 
*!*                  m.NQtyPerCrn = POCRTNMF.TOTQTY
*!*                  INSERT INTO (loformset.lcCartTemp) FROM MEMVAR 
*!*                ENDIF   
*!*              ENDIF   
*!*            ELSE
*!*              IF !SEEK(PADR(m.Style,19)+POCRTNMF.CCARTONNO+&lcOrdln..Style ,loformset.lcCartTemp)
*!*                SELECT (loformset.lcStylePik)  
*!*                m.CCARTONTYP = POCRTNMF.CCARTONTYP
*!*                m.QtyPerCrt = POCRTNMF.TOTQTY
*!*                m.nQty1Crt = POCRTNMF.QTY1
*!*                m.nQty2Crt = POCRTNMF.QTY2
*!*                m.nQty3Crt = POCRTNMF.QTY3
*!*                m.nQty4Crt = POCRTNMF.QTY4
*!*                m.nQty5Crt = POCRTNMF.QTY5
*!*                m.nQty6Crt = POCRTNMF.QTY6
*!*                m.nQty7Crt = POCRTNMF.QTY7
*!*                m.nQty8Crt = POCRTNMF.QTY8                            
*!*                IF !EMPTY(POCRTNMF.PIKTKT) AND m.PIKTKT=POCRTNMF.PIKTKT
*!*                  m.nCartAll = 1
*!*                  m.nOldAll = 1
*!*                  m.nCartAvl = 0
*!*                  m.nQtyAll = m.QtyPerCrt
*!*                ELSE
*!*                  IF EMPTY(POCRTNMF.PIKTKT)
*!*                    m.nCartAvl = 1 
*!*                    m.nCartAll = 0
*!*                    m.nOldAll = 0
*!*                    m.nQtyAll = 0
*!*  *!*                    m.nQty1Crt = 0
*!*  *!*                    m.nQty2Crt = 0
*!*  *!*                    m.nQty3Crt = 0
*!*  *!*                    m.nQty4Crt = 0
*!*  *!*                    m.nQty5Crt = 0
*!*  *!*                    m.nQty6Crt = 0
*!*  *!*                    m.nQty7Crt = 0
*!*  *!*                    m.nQty8Crt = 0
*!*                  ELSE
*!*                    LOOP 
*!*                  ENDIF   
*!*                ENDIF   
*!*                IF !SEEK(m.PIKTKT+PADR(m.Style,19),loformset.lcStylePik)
*!*                  m.cColor = gfCodDes(SUBSTR(&lcOrdln..Style,loFormSet.lnClrStrt,loFormSet.lnClrlen), 'COLOR     ')
*!*                ELSE
*!*                  m.cColor =  ''
*!*                ENDIF    
*!*                APPEND BLANK 
*!*                GATHER MEMO MEMVAR  
*!*                m.CCARTONNO = POCRTNMF.CCARTONNO
*!*                m.QtyPerCrt = POCRTNMF.TOTQTY
*!*                m.HStyle =&lcOrdln..Style 
*!*                m.NQtyPerCrn = POCRTNMF.TOTQTY
*!*                INSERT INTO (loformset.lcCartTemp) FROM MEMVAR  
*!*              ENDIF   
*!*            ENDIF 
*!*          ENDSCAN  
*!*        ENDIF   
*!*      ENDSCAN  
*!*      lcStyPik = loformset.lcStylePik
*!*      SELECT (lcStyPik) 
*!*      SCAN 
*!*        REPLACE nCurrAll WITH nCartAll
*!*        lnCurrRec = RECNO()
*!*        lcPiktkt = &lcStyPik..PIKTKT
*!*        lcStyleClr = &lcStyPik..Style
*!*        SELECT(lcOrdln)
*!*        SUM TOTPIK FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED
*!*        SUM PIK1 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED1
*!*        SUM PIK2 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED2
*!*        SUM PIK3 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED3
*!*        SUM PIK4 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED4
*!*        SUM PIK5 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED5
*!*        SUM PIK6 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED6
*!*        SUM PIK7 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED7
*!*        SUM PIK8 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED8
*!*        SELECT (lcStyPik)
*!*        REPLACE nQtyPik WITH lnTOTPIKED,;
*!*                nQty1Pik WITH lnTOTPIKED1,;
*!*                nQty2Pik WITH lnTOTPIKED2,;
*!*                nQty3Pik WITH lnTOTPIKED3,;
*!*                nQty4Pik WITH lnTOTPIKED4,;
*!*                nQty5Pik WITH lnTOTPIKED5,;
*!*                nQty6Pik WITH lnTOTPIKED6,;
*!*                nQty7Pik WITH lnTOTPIKED7,;
*!*                nQty8Pik WITH lnTOTPIKED8;
*!*                 FOR Style = lcStyleClr AND PIKTKT =lcPiktkt 
*!*                
*!*        IF BETWEEN( lnCurrRec ,1,RECCOUNT())
*!*          GO RECORD  lnCurrRec 
*!*        ENDIF 
*!*      ENDSCAN 
*!*      LOCATE  
*!*      ENDIF 
    *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
    IF loFormSet.ActiveMode = 'E'
     lcOrdln = loFormSet.lc_tmpordl
     llReCol = .F.
     SELECT(lcOrdln)
     SCAN FOR cSTatus <> 'R' AND picked 
        lnRecNum = RECNO()
        *IF SEEK("O"+&lcOrdln..Order+STR(&lcOrdln..LINENo,6),'ORDLINE','ORDLINE') 
          *IF !EMPTY(ORDLINE.PIKTKT)
            lcStyle = SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1) 
            lcPIKTKT = &lcOrdln..PIKTKT
         
            SELECT(lcOrdln)
            *=SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
    
            SUM PIK1,PIK2,PIK3,PIK4,PIK5,PIK6,PIK7,PIK8,TOTPIK REST WHILE ;
            CORDTYPE+ORDER+STR(LINENO,6)= "O"+&lcOrdln..Order FOR Style = ALLTRIM(lcStyle) AND PICKED AND  (PIKTKT = lcPIKTKT OR EMPTY(PIKTKT )) TO ;
            lnPIK1,lnPIK2,lnPIK3,lnPIK4,lnPIK5,lnPIK6,lnPIK7,lnPIK8,lnTOTPIK
         
         
           *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
            SELECT Style_Carton
            =gfSeek(ALLTRIM(lcStyle),'Style_Carton')
            lnSclCnt = 1
            SCAN REST WHILE Style = lcStyle 
              =gfSeek('S'+Style_Carton.Scale,'Scale_Carton') 
              IF Style_Carton.Style = ALLTRIM(&lcOrdln..Style)
                 EXIT 
               ELSE
               lnSclCnt = lnSclCnt + Scale_carton.cnt
              ENDIF  
            ENDSCAN 
        
            =SEEK("O"+&lcOrdln..Order+STR(&lcOrdln..LINENo,6),'ORDLINE','ORDLINE')  
            lnAllQty = 0
            SELECT (loFormSet.lcStylePik)
            FOR lnR = lnSclCnt TO lnSclCnt+Scale_carton.cnt - 1
               LOCATE 
               lcR = ALLTRIM(STR(lnR))
               SUM nCartAll * nQty&lcR.Crt FOR ;
             Style = ALLTRIM(SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1)) ;
               AND (EMPTY(PIKTKT) OR PIKTKT = ORDLINE.PIKTKT );
               TO lnAllo&lcR.
               lnAllQty = lnAllQty + lnAllo&lcR.
            ENDFOR

             SELECT (lcOrdln)   
            IF BETWEEN(lnRecNum ,1,RECCOUNT())      
              GO RECORD lnRecNum 
            ENDIF 

            lnNormCnt = 1
            FOR lnR = lnSclCnt TO lnSclCnt+Scale_carton.cnt - 1
              IF (lnAllQty <> lnTOTPIK) 
                llReCol = .T.
                EXIT 
              ENDIF 
              lcNormCnt = ALLTRIM(STR(lnNormCnt))
              lcR = ALLTRIM(STR(lnR))
              IF lnAllo&lcR. <> lnPIK&lcNormCnt.
                llReCol = .T.
                EXIT 
              ENDIF 
              lnNormCnt = lnNormCnt + 1 
            ENDFOR

             
            *=SEEK("O"+&lcOrdln..Order+STR(&lcOrdln..LINENo,6),'ORDLINE','ORDLINE')  
*!*              SELECT (loFormSet.lcStylePik)
*!*              SUM nQtyAll,nCartAll * nQty1Crt,;
*!*                nCartAll * nQty2Crt,nCartAll * nQty3Crt,;
*!*                nCartAll * nQty4Crt ,nCartAll * nQty5Crt ,;
*!*                nCartAll * nQty6Crt ,nCartAll * nQty7Crt ,;
*!*                nCartAll * nQty8Crt FOR ;
*!*                Style = ALLTRIM(SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1)) ;
*!*                 AND (EMPTY(PIKTKT) OR PIKTKT = ORDLINE.PIKTKT );
*!*                 TO lnAllQty,lnAllo1,lnAllo2,lnAllo3,lnAllo4,lnAllo5,lnAllo6,lnAllo7,lnAllo8
*!*      
*!*              SELECT (lcOrdln)   
*!*              IF BETWEEN(lnRecNum ,1,RECCOUNT())      
*!*                GO RECORD lnRecNum 
*!*              ENDIF 
*!*              IF (lnAllQty <> lnTOTPIK) OR (lnPIK1 <> lnAllo1);
*!*                 OR (lnPIK2 <> lnAllo2)OR (lnPIK3 <> lnAllo3);
*!*                 OR (lnPIK4 <> lnAllo4)OR (lnPIK5 <> lnAllo5);
*!*                 OR (lnPIK6 <> lnAllo6)OR (lnPIK7 <> lnAllo7);
*!*                 OR (lnPIK8 <> lnAllo8)
*!*                 llReCol = .T.
*!*                 EXIT 
*!*              ENDIF
         * ENDIF   
        *ENDIF 
        *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
     ENDSCAN 
     IF llReCol 
       IF TYPE('loFormSet.lcStylePik')<> 'U' AND USED(loFormSet.lcStylePik)
          SELECT (loFormSet.lcStylePik)
          DELETE ALL
        ENDIF 

        IF TYPE('loFormSet.lcCartTemp')<> 'U' AND  USED(loFormSet.lcCartTemp)
          SELECT (loFormSet.lcCartTemp)
          DELETE ALL
        ENDIF 
        lfGETCRTINF()
     ENDIF   
   ENDIF     
    *MMMT2221
    SELECT (loformset.lcStylePik)
    LOCATE 
    DO FORM (oAriaApplication.ScreenHome+"\AL\alcrtyp.scx") WITH loFormSet 
   * SELECT(lcOrdln)
*!*      IF BETWEEN(lnCurrentRec,1,RECCOUNT())
*!*        GO RECORD lnCurrentRec
*!*      ENDIF 
    llFromSave = .F. 
ENDIF   

*!*************************************************************
*! Name      : lfALOPNFLS  
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/11/2008
*! Purpose   : function to Open POCRTNMF Table and create Temp. Files
*!*************************************************************
FUNCTION lfALOPNFLS  
PARAMETERS llCreateTemp
IF !USED('POCRTNMF')
  =gfOpenTable('POCRTNMF','pocrtnmf')
ENDIF


*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
IF !USED('Scale_Carton')
  =gfOpenTable('scale','scale','SH','Scale_Carton')
ENDIF
IF !USED('Style_Carton')
  =gfOpenTable('Style','Style','SH','Style_Carton')
ENDIF
*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]

IF !llCreateTemp
  loformset.AddProperty ('lcStylePik',gfTempName())
  loformset.AddProperty ('lnMajLen',0)
  loformset.AddProperty ('lnClrlen',0)
  loformset.AddProperty ('lnClrStrt',0)
  loformset.AddProperty ('lcCartTemp',gfTempName())
  loformset.AddProperty ('lcOrdTemp',gfTempName())
  loformset.AddProperty ('lcSclTemp',gfTempName())
  loformset.AddProperty ('lnMaxSiz',0)
  
  
  
  STORE 0 TO lnClrLen,lnClrPos,lnSclLen ,lnSclPos 
  DIMENSION laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    DO CASE
      CASE laItemSeg[lnCount,1]='C'
        lnClrLen = LEN(laItemSeg[lnCount,3])
        lnClrPos = laItemSeg[lnCount,4]
        lcClrSpr = ALLT(laItemSeg[lnCount,6])    
     
      CASE  laItemSeg[lnCount,1]='S'
        lnSclLen = LEN(laItemSeg[lnCount,3])
        lnSclPos = laItemSeg[lnCount,4]
    ENDCASE  
  ENDFOR

  loformset.lnMajLen = LEN(gfItemMask("PM","",'0001'))
  loformset.lnClrlen =lnClrLen 
  loformset.lnClrStrt=lnClrPos 
ENDIF 

SELECT (loformset.lc_tmpordl) 
AFIELDS(laOrdLSt)
=gfCrtTmp(loformset.lcOrdTemp, @laOrdLSt,"cOrdType + ORDER  + STR(LineNo,6)",loformset.lcOrdTemp) 


*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
loformset.lnMaxSiz = 0
SELECT (loformset.lc_tmpordl) 
LOCATE 
lcStyleMajor = SUBSTR(Style,1,loformset.lnClrlen+loformset.lnClrStrt-1)
=gfSEEK(lcStyleMajor ,'Style_Carton')
SELECT 'Style_Carton'
SCAN REST WHILE Style = lcStyleMajor
  =gfSeek('S' +  Style_Carton.Scale,'Scale_Carton')
  FOR lnI = 1 TO Scale_Carton.cnt
    loformset.lnMaxSiz = loformset.lnMaxSiz + 1
  ENDFOR 
ENDSCAN
*
IF loformset.lnMaxSiz > 0
  DIMENSION laSclDime[loformset.lnMaxSiz,4]
  FOR lnI = 1 TO loformset.lnMaxSiz
    lcI = ALLTRIM(STR(lnI ))
    laSclDime[lnI,1] = 'sz'+lcI
    laSclDime[lnI,2] = 'C'
    laSclDime[lnI,3] = 5
    laSclDime[lnI,4] = 0
  ENDFOR 
  =gfCrtTmp(loformset.lcSclTemp, @laSclDime) 
  =gfSEEK(lcStyleMajor ,'Style_Carton')
  SELECT 'Style_Carton'
  lncntr = 1
  SCAN REST WHILE Style = lcStyleMajor
    =gfSeek('S' +  Style_Carton.Scale,'Scale_Carton')
    FOR lnI = 1 TO Scale_Carton.cnt
      lcI = ALLTRIM(STR(lnI ))
      lccntr = ALLTRIM(STR(lncntr))
      m.sz&lccntr. = Scale_Carton.sz&lcI
      lncntr = lncntr+ 1
    ENDFOR 
  ENDSCAN
  INSERT INTO (loformset.lcSclTemp) FROM MEMVAR 
ENDIF   
*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]


DIMENSION laFileStru[15+2*loformset.lnMaxSiz ,4]

laFileStru[1,1] = 'Piktkt'
laFileStru[1,2] = 'C'
laFileStru[1,3] = '6'
laFileStru[1,4] = 0

laFileStru[2,1] = 'CStyMajor'
laFileStru[2,2] = 'C'
laFileStru[2,3] = loformset.lnMajLen 
laFileStru[2,4] = 0

laFileStru[3,1] = 'Style'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 19
laFileStru[3,4] = 0

laFileStru[4,1] = 'CCARTONTYP'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 6
laFileStru[4,4] = 0

laFileStru[5,1] = 'QtyPerCrt'
laFileStru[5,2] = 'N'
laFileStru[5,3] = 7
laFileStru[5,4] = 0

laFileStru[6,1] = 'nCartAvl'
laFileStru[6,2] = 'N'
laFileStru[6,3] = 7
laFileStru[6,4] = 0

laFileStru[7,1] = 'nCartAll'
laFileStru[7,2] = 'N'
laFileStru[7,3] = 7
laFileStru[7,4] = 0

laFileStru[8,1] = 'nQtyAll'
laFileStru[8,2] = 'N'
laFileStru[8,3] = 9
laFileStru[8,4] = 0

laFileStru[9,1] = 'nQtyPik'
laFileStru[9,2] = 'N'
laFileStru[9,3] = 9
laFileStru[9,4] = 0

laFileStru[10,1] = 'cColor'
laFileStru[10,2] = 'C'
laFileStru[10,3] = 30
laFileStru[10,4] = 0

laFileStru[11,1] = 'nOldAll'
laFileStru[11,2] = 'N'
laFileStru[11,3] = 7
laFileStru[11,4] = 0

laFileStru[12,1] = 'cwarecode'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 6
laFileStru[12,4] = 0


laFileStru[13,1] = 'nCurrAll'
laFileStru[13,2] = 'N'
laFileStru[13,3] = 7
laFileStru[13,4] = 0

laFileStru[15,1] = 'MCartons'
laFileStru[15,2] = 'M'
laFileStru[15,3] = 10
laFileStru[15,4] = 0


lnStrCnt = 16

FOR lnT =1 TO loformset.lnMaxSiz 
  lcT = ALLTRIM(STR(lnT))
  lcStrCnt  = ALLTRIM(STR(lnStrCnt))
  laFileStru[lnStrCnt,1] = 'nQty'+lcT+'Pik' 
  laFileStru[lnStrCnt,2] = 'N' 
  laFileStru[lnStrCnt,3] = 7
  laFileStru[lnStrCnt,4] = 0
  lnStrCnt  = lnStrCnt + 1
ENDFOR 

FOR lnT =1 TO loformset.lnMaxSiz 
  lcT = ALLTRIM(STR(lnT))
  lcStrCnt  = ALLTRIM(STR(lnStrCnt))
  laFileStru[lnStrCnt,1] = 'nQty'+lcT+'Crt' 
  laFileStru[lnStrCnt,2] = 'N' 
  laFileStru[lnStrCnt,3] = 7
  laFileStru[lnStrCnt,4] = 0
  lnStrCnt  = lnStrCnt + 1
ENDFOR 


*!*	laFileStru[14,1] = 'nQty1Pik'
*!*	laFileStru[14,2] = 'N'
*!*	laFileStru[14,3] = 7
*!*	laFileStru[14,4] = 0

*!*	laFileStru[15,1] = 'nQty2Pik'
*!*	laFileStru[15,2] = 'N'
*!*	laFileStru[15,3] = 7
*!*	laFileStru[15,4] = 0

*!*	laFileStru[16,1] = 'nQty3Pik'
*!*	laFileStru[16,2] = 'N'
*!*	laFileStru[16,3] = 7
*!*	laFileStru[16,4] = 0

*!*	laFileStru[17,1] = 'nQty4Pik'
*!*	laFileStru[17,2] = 'N'
*!*	laFileStru[17,3] = 7
*!*	laFileStru[17,4] = 0

*!*	laFileStru[18,1] = 'nQty5Pik'
*!*	laFileStru[18,2] = 'N'
*!*	laFileStru[18,3] = 7
*!*	laFileStru[18,4] = 0

*!*	laFileStru[19,1] = 'nQty6Pik'
*!*	laFileStru[19,2] = 'N'
*!*	laFileStru[19,3] = 7
*!*	laFileStru[19,4] = 0

*!*	laFileStru[20,1] = 'nQty7Pik'
*!*	laFileStru[20,2] = 'N'
*!*	laFileStru[20,3] = 7
*!*	laFileStru[20,4] = 0

*!*	laFileStru[21,1] = 'nQty8Pik'
*!*	laFileStru[21,2] = 'N'
*!*	laFileStru[21,3] = 7
*!*	laFileStru[21,4] = 0


*!*	laFileStru[22,1] = 'nQty1Crt'
*!*	laFileStru[22,2] = 'N'
*!*	laFileStru[22,3] = 7
*!*	laFileStru[22,4] = 0

*!*	laFileStru[23,1] = 'nQty2Crt'
*!*	laFileStru[23,2] = 'N'
*!*	laFileStru[23,3] = 7
*!*	laFileStru[23,4] = 0

*!*	laFileStru[24,1] = 'nQty3Crt'
*!*	laFileStru[24,2] = 'N'
*!*	laFileStru[24,3] = 7
*!*	laFileStru[24,4] = 0

*!*	laFileStru[25,1] = 'nQty4Crt'
*!*	laFileStru[25,2] = 'N'
*!*	laFileStru[25,3] = 7
*!*	laFileStru[25,4] = 0

*!*	laFileStru[26,1] = 'nQty5Crt'
*!*	laFileStru[26,2] = 'N'
*!*	laFileStru[26,3] = 7
*!*	laFileStru[26,4] = 0

*!*	laFileStru[27,1] = 'nQty6Crt'
*!*	laFileStru[27,2] = 'N'
*!*	laFileStru[27,3] = 7
*!*	laFileStru[27,4] = 0

*!*	laFileStru[28,1] = 'nQty7Crt'
*!*	laFileStru[28,2] = 'N'
*!*	laFileStru[28,3] = 7
*!*	laFileStru[28,4] = 0

*!*	laFileStru[29,1] = 'nQty8Crt'
*!*	laFileStru[29,2] = 'N'
*!*	laFileStru[29,3] = 7
*!*	laFileStru[29,4] = 0

laFileStru[14,1] = 'FStyle'
laFileStru[14,2] = 'C'
laFileStru[14,3] = 19
laFileStru[14,4] = 0

=gfCrtTmp(loformset.lcStylePik,@laFileStru,[Piktkt+Style+CCARTONTYP],loformset.lcStylePik)


DIMENSION laCrtTemp[4,4]
laCrtTemp[1,1] = 'Style'
laCrtTemp[1,2] = 'C'
laCrtTemp[1,3] = 19
laCrtTemp[1,4] = 0

laCrtTemp[2,1] = 'CcartonNo'
laCrtTemp[2,2] = 'C'
laCrtTemp[2,3] = 6
laCrtTemp[2,4] = 0

laCrtTemp[3,1] = 'HStyle'
laCrtTemp[3,2] = 'C'
laCrtTemp[3,3] = 19
laCrtTemp[3,4] = 0

laCrtTemp[4,1] = 'NQtyPerCrn'
laCrtTemp[4,2] = 'N'
laCrtTemp[4,3] = 7
laCrtTemp[4,4] = 0
 =gfCrtTmp(loformset.lcCartTemp ,@laCrtTemp,[Style+CCARTONNO+HStyle],loformset.lcCartTemp )
*!*************************************************************
*! Name      : lfAddGrdCntSrc
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/11/2008
*! Purpose   : function to grid control source
*!*************************************************************
FUNCTION lfAddGrdCntSrc
PARAMETERS loChldForm
lcSrcFile = loChldForm.loParentForm.lcStylePik
WITH loChldForm.ariaForm1.grdClrCrt
  .RecordSource = ''
  .RecordSource = lcSrcFile 
  .ColumnCount = .ColumnCount + loChldForm.loParentForm.lnMaxSiz 

  .Column1.controlsource = lcSrcFile+'.cColor'
  .Column1.Readonly = .T.
   
  lnC = 7
  lnO = 2
  FOR lnR = 1 TO loChldForm.loParentForm.lnMaxSiz 
    lcR = ALLTRIM(STR(lnR))
    lcC = ALLTRIM(STR(lnC))
    .Column&lcC..controlsource = lcSrcFile+'.nQty'+ lcR +'Crt'
    .Column&lcC..Header1.Caption = EVALUATE(loChldForm.loParentForm.lcSclTemp+'.sz'+ lcR )
    .Column&lcC..Columnorder= lnO
    .Column&lcC..width= 50
    .Column&lcC..Readonly = .T.
    lnO = lnO + 1
    lnC = lnC + 1
  ENDFOR  
  
  .Column2.controlsource = lcSrcFile+'.CCARTONTYP'
  .Column2.Readonly = .T.
  .Column2.Columnorder= lnO
  lnO = lnO + 1
  
  .Column3.controlsource = lcSrcFile+'.QtyPerCrt'
  .Column3.Readonly = .T.
  .Column3.Columnorder= lnO
  lnO = lnO + 1
 
  .Column4.controlsource = lcSrcFile+'.nCartAvl'
  .Column4.Readonly = .T.
  .Column4.Columnorder= lnO
  lnO = lnO + 1

  .Column5.controlsource = lcSrcFile+'.nCartAll'
  .Column5.Readonly = (loChldForm.loParentForm.ActiveMode = 'V') 
  .Column5.Columnorder= lnO
  lnO = lnO + 1

  BINDEVENT(.Column5.Text1,'LostFocus',loChldForm,'lfCalcTot')
  BINDEVENT(.Column5.Text1,'GotFocus',loChldForm,'lfChkTot')
  .Column6.controlsource = lcSrcFile+'.nQtyAll'
  .Column6.Readonly = .T.
  .Column6.Columnorder= lnO
  lnO = lnO + 1

ENDWITH 
*!*************************************************************
*! Name      : lfHeadrInfo
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/11/2008
*! Purpose   : function to refersh screen Header
*!*************************************************************
FUNCTION lfHeadrInfo
PARAMETERS loChldFormSet
lcSrcFile =loChldFormSet.loParentForm.lcStylePik

loChldFormSet.ariaForm1.kbStyle.KeyTextBox.Value = &lcSrcFile..cStyMAjor
loChldFormSet.ariaForm1.KBPIK.KeyTextBox.Value = IIF('******' $ &lcSrcFile..PIKTKT ,'',&lcSrcFile..PIKTKT)
loChldFormSet.ariaForm1.txtTotPik.Value = &lcSrcFile..nQtyPik
loChldFormSet.ariaForm1.txtCartTot.Value = &lcSrcFile..nQtyAll

SELECT (lcSrcFile)
lcCurrStyle = Style
lcCurrPik = PIKTKT
lnRecNo= RECNO()
SUM nQtyAll FOR Style = lcCurrStyle AND PIKTKT = lcCurrPik  TO lnTOTAll
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO RECORD lnRecNo
ENDIF 
loChldFormSet.ariaForm1.txtCartTot.Value = lnTOTAll

*!*************************************************************
*! Name      : lfCalcTot
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/11/2008
*! Purpose   : function to calc. TotQty
*!*************************************************************
FUNCTION lfCalcTot
PARAMETERS loChldFormSet

lcSrcFile =loChldFormSet.loParentForm.lcStylePik
SELECT(lcSrcFile)

IF nCartAll <> loChldFormSet.lnOldValCrt AND nCartAll > nCartAvl+loChldFormSet.lnOldValCrt
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Allocated cartons can not be greater than Available cartons') 
  SELECT(lcSrcFile)
  REPLACE nCartAll WITH loChldFormSet.lnOldValCrt ,;
          nQtyAll WITH nCartAll*QtyPerCrt
  RETURN
ENDIF 

IF nCartAll < 0 
  = gfModalGen('TRM42000B40011','DIALOG')
  SELECT(lcSrcFile)
  REPLACE nCartAll WITH loChldFormSet.lnOldValCrt ,;
          nQtyAll WITH nCartAll*QtyPerCrt
  RETURN
ENDIF


REPLACE  nQtyAll WITH nCartAll*QtyPerCrt ,;
         nCartAvl WITH nCartAvl - nCartAll + loChldFormSet.lnOldValCrt IN (lcSrcFile)

*!*************************************************************
*! Name      : lfALCRTSAV
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/11/2008
*! Purpose   : function to allocation cartons
*!*************************************************************
FUNCTION lfALCRTSAV
*user does not open the cartons screen at all
*--if generated PIKtkt 
*-- if did not generate piktkt at all
*-- release exsiting one 


lcOrdln = loFormSet.lc_tmpordl

IF !USED('PIKTKT_A')
  gfOpenTable('PIKTKT',"ORDPIK",'SH','PIKTKT_A')
ENDIF 
lnCountDel = 0
IF USED(loFormSet.lcStylePik)
  SELECT (loFormSet.lcStylePik)
  LOCATE 
  COUNT FOR !DELETED() TO lnCountDel 
ENDIF   

IF USED(loFormSet.lcStylePik) AND lnCountDel  = 0
  llMsgGen = .F.
  SELECT (lcOrdln)
  SCAN 
    IF SEEK("O"+&lcOrdln..Order+STR(LINENo,6),'ORDLINE','ORDLINE') 
     IF !EMPTY(ORDLINE.PIKTKT)
       IF !gfSeek(ORDLINE.PIKTKT,'POCRTNMF','POPIKTKT')
         llMsgGen = .T.
         EXIT 
       ELSE
         
         lcStyle = SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1) 
         lcPIKTKT = ORDLINE.PIKTKT
         SELECT ORDLINE 
         
         =SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
         SUM PIK1,PIK2,PIK3,PIK4,PIK5,PIK6,PIK7,PIK8,TOTPIK REST WHILE ;
             CORDTYPE+ORDER+STR(LINENO,6)= "O"+&lcOrdln..Order FOR Style = &lcOrdln..Style AND  PIKTKT = lcPIKTKT TO ;
             lnPIK1,lnPIK2,lnPIK3,lnPIK4,lnPIK5,lnPIK6,lnPIK7,lnPIK8,lnTOTPIK
             
         SELECT POCRTNMF    
         SUM Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TOTQTY REST WHILE  PIKTKT = lcPIKTKT FOR Style = &lcOrdln..Style TO ;
         lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTOTPIKCR
         
         IF (lnTOTPIK <> lnTOTPIKCR) OR (lnQty1 <> lnPIK1);
            OR (lnQty2 <> lnPIK2) OR (lnQty3 <> lnPIK3) OR (lnQty4 <> lnPIK4) OR (lnQty5 <> lnPIK5);
            OR (lnQty6 <> lnPIK6) OR (lnQty7 <> lnPIK7) OR (lnQty8 <> lnPIK8) 
           llMsgGen = .T.
         ELSE
             
         ENDIF 
       ENDIF 
     ELSE
       
       IF gfSeek(&lcOrdln..Order,'PIKTKT_A')      
         SELECT PIKTKT_A
         SCAN REST WHILE ORDER+PIKTKT = &lcOrdln..Order FOR status = 'X'
           IF gfSeek(PIKTKT_A.PIKTKT,'POCRTNMF','POPIKTKT')
             SELECT POCRTNMF
             SCAN REST WHILE PIKTKT = PIKTKT_A.PIKTKT
               gfReplace("PIKTKT with ''")
             ENDSCAN  
             gfTableUpdate()
           ENDIF 
         ENDSCAN 
       ENDIF
             
     ENDIF 
    ENDIF 
  ENDSCAN 
*!*    IF llMsgGen 
*!*      IF gfModalGen('QRM00000B34001',.F.,.F.,.F.,'Update Carton Details') = 2
*!*        RETURN
*!*      ENDIF
*!*      llFromSave = .T.
*!*      lfALCARTON()
*!*    ELSE
    lfSaveCrtn()
*!*    ENDIF   
ELSE
  *-- User Opened the cartons alloc. screen 
  *- Does not alloc. any line although there is a piktkt
  *- alloc. cartons but not equal to picked qty
  IF USED(loFormSet.lcStylePik) AND lnCountDel  > 0
    SELECT (loFormSet.lcStylePik)
    llMsgGen = .F.
    LOCATE FOR nCartAll > 0
    IF FOUND()
      SELECT (lcOrdln)
      SCAN FOR cSTatus = 'R'
        IF SEEK(&lcOrdln..cOrdType + &lcOrdln..ORDER  + STR(&lcOrdln..LineNo,6),loformset.lcOrdTemp) AND !EMPTY(EVALUATE(loformset.lcOrdTemp+'.PIKTKT'))
          lcStyle = SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1) 
          IF gfSeek(EVALUATE(loformset.lcOrdTemp+'.PIKTKT'),'POCRTNMF','POPIKTKT')
            SELECT POCRTNMF
            SCAN REST WHILE PIKTKT = EVALUATE(loformset.lcOrdTemp+'.PIKTKT') FOR style =&lcOrdln..Style
              gfReplace("PIKTKT with ''")
              SELECT (loFormSet.lcStylePik)
              *MIII
              SCAN FOR nCartAll > 0 AND Style = lcStyle AND PIKTKT = EVALUATE(loformset.lcOrdTemp+'.PIKTKT') AND CCARTONTYP = POCRTNMF.CCARTONTYP 
                * nQty1Crt WITH nQty1Crt - POCRTNMF.Qty1,;
                        nQty2Crt WITH nQty2Crt - POCRTNMF.Qty2,;
                        nQty3Crt WITH nQty3Crt - POCRTNMF.Qty3,;
                        nQty4Crt WITH nQty4Crt - POCRTNMF.Qty4,;
                        nQty5Crt WITH nQty5Crt - POCRTNMF.Qty5,;
                        nQty6Crt WITH nQty6Crt - POCRTNMF.Qty6,;
                        nQty7Crt WITH nQty7Crt - POCRTNMF.Qty7,;
                        nQty8Crt WITH nQty8Crt - POCRTNMF.Qty8,;
                        QtyPerCrt WITH QtyPerCrt - POCRTNMF.TOTQTY
                        
                  REPLACE nCartAll WITH 0,;
                          nQtyAll   WITH nCartAll*QtyPerCrt
              ENDSCAN 
              SELECT POCRTNMF
            ENDSCAN  
            gfTableUpdate()
            
            
            
          ENDIF 
        ENDIF 
      ENDSCAN 
      
      SELECT (lcOrdln)      
      SCAN FOR cSTatus <> 'R'
        lnRecNum = RECNO()
        IF SEEK("O"+&lcOrdln..Order+STR(&lcOrdln..LINENo,6),'ORDLINE','ORDLINE') 
          IF !EMPTY(ORDLINE.PIKTKT)
            lcStyle = SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1) 
            lcPIKTKT = ORDLINE.PIKTKT
         
         *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
         	SELECT Style_Carton
    			=gfSeek(ALLTRIM(lcStyle),'Style_Carton')
		    	lnSclCnt = 1
    			SCAN REST WHILE Style = lcStyle 
    			  =gfSeek('S'+Style_Carton.Scale,'Scale_Carton') 
    			  IF Style_Carton.Style = ALLTRIM(&lcOrdln..Style)
    		 	    EXIT 
     			  ELSE
    			   lnSclCnt = lnSclCnt + Scale_carton.cnt
    			  ENDIF  
    			ENDSCAN 
        *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
         
         
            SELECT Ordline 
            =SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
     	   	 SUM PIK1,PIK2,PIK3,PIK4,PIK5,PIK6,PIK7,PIK8,TOTPIK REST WHILE ;
            CORDTYPE+ORDER+STR(LINENO,6)= "O"+&lcOrdln..Order FOR Style = &lcOrdln..Style AND  PIKTKT = lcPIKTKT TO ;
            lnPIK1,lnPIK2,lnPIK3,lnPIK4,lnPIK5,lnPIK6,lnPIK7,lnPIK8,lnTOTPIK
         
             
            =SEEK("O"+&lcOrdln..Order+STR(&lcOrdln..LINENo,6),'ORDLINE','ORDLINE')  
            lnAllQty = 0
            SELECT (loFormSet.lcStylePik)
            FOR lnR = lnSclCnt TO lnSclCnt+Scale_carton.cnt - 1
               LOCATE 
               lcR = ALLTRIM(STR(lnR))
               SUM nCartAll * nQty&lcR.Crt FOR ;
	           Style = ALLTRIM(SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1)) ;
               AND (EMPTY(PIKTKT) OR PIKTKT = ORDLINE.PIKTKT );
               TO lnAllo&lcR.
               lnAllQty = lnAllQty + lnAllo&lcR.
            ENDFOR

            
            
            *SUM nQtyAll,nCartAll * nQty1Crt,;
              nCartAll * nQty2Crt,nCartAll * nQty3Crt,;
              nCartAll * nQty4Crt ,nCartAll * nQty5Crt ,;
              nCartAll * nQty6Crt ,nCartAll * nQty7Crt ,;
              nCartAll * nQty8Crt FOR ;
              Style = ALLTRIM(SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1)) ;
               AND (EMPTY(PIKTKT) OR PIKTKT = ORDLINE.PIKTKT );
               TO lnAllQty,lnAllo1,lnAllo2,lnAllo3,lnAllo4,lnAllo5,lnAllo6,lnAllo7,lnAllo8
 
          *SUM nQtyAll FOR Style = ALLTRIM(SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1)) ;
              AND (EMPTY(PIKTKT) OR PIKTKT = ORDLINE.PIKTKT )  TO lnAllQty
    
            SELECT (lcOrdln)   
            IF BETWEEN(lnRecNum ,1,RECCOUNT())      
              GO RECORD lnRecNum 
            ENDIF 
            lnNormCnt = 1
            FOR lnR = lnSclCnt TO lnSclCnt+Scale_carton.cnt - 1
              IF (lnAllQty <> lnTOTPIK) 
                llMsgGen = .T.
                EXIT 
              ENDIF 
              lcNormCnt = ALLTRIM(STR(lnNormCnt))
              lcR = ALLTRIM(STR(lnR))
              IF lnAllo&lcR. <> lnPIK&lcNormCnt.
                llMsgGen = .T.
                EXIT 
              ENDIF 
              lnNormCnt = lnNormCnt + 1 
            ENDFOR
            
            *IF (lnAllQty <> lnTOTPIK) OR (lnPIK1 <> lnAllo1);
               OR (lnPIK2 <> lnAllo2)OR (lnPIK3 <> lnAllo3);
               OR (lnPIK4 <> lnAllo4)OR (lnPIK5 <> lnAllo5);
               OR (lnPIK6 <> lnAllo6)OR (lnPIK7 <> lnAllo7);
               OR (lnPIK8 <> lnAllo8)
            *  llMsgGen = .T.
            *ENDIF
          ELSE
            IF &lcOrdln..cSTatus = 'R' AND SEEK(&lcOrdln..cOrdType + &lcOrdln..ORDER  + STR(&lcOrdln..LineNo,6),loformset.lcOrdTemp) AND !EMPTY(EVALUATE(loformset.lcOrdTemp+'.PIKTKT'))
              IF gfSeek(EVALUATE(loformset.lcOrdTemp+'.PIKTKT'),'POCRTNMF','POPIKTKT')
                SELECT POCRTNMF
                SCAN REST WHILE PIKTKT = EVALUATE(loformset.lcOrdTemp+'.PIKTKT') FOR style =&lcOrdln..Style
                  gfReplace("PIKTKT with ''")
                ENDSCAN  
                gfTableUpdate()
              ENDIF 
            ENDIF 
          ENDIF   
        ENDIF 
      ENDSCAN 
*!*        IF llMsgGen 
*!*          *IF gfModalGen('QRM00000B34001',.F.,.F.,.F.,'Update Carton Details') = 2
*!*            RETURN
*!*         * ENDIF
*!*          IF TYPE('loFormSet.lcStylePik')<> 'U' AND USED(loFormSet.lcStylePik)
*!*            SELECT (loFormSet.lcStylePik)
*!*            DELETE ALL
*!*          ENDIF 

*!*          IF TYPE('loFormSet.lcCartTemp')<> 'U' AND  USED(loFormSet.lcCartTemp)
*!*            SELECT (loFormSet.lcCartTemp)
*!*            DELETE ALL
*!*          ENDIF 
*!*          lfGETCRTINF()
*!*          llFromSave = .T.
*!*          lfALCARTON()
*!*        ELSE
        lfSaveCrtn()
*!*        ENDIF         
    ELSE
      llMsgGen = .F.
      SELECT (lcOrdln)
      SCAN 
        IF SEEK("O"+Order+STR(LINENo,6),'ORDLINE','ORDLINE') AND !EMPTY(ORDLINE.PIKTKT)
          IF !gfSeek(ORDLINE.PIKTKT,'POCRTNMF','POPIKTKT')
            llMsgGen = .T.
            EXIT 
          ENDIF   
        ENDIF
      ENDSCAN 
*!*        IF llMsgGen 
*!*          *IF gfModalGen('QRM00000B34001',.F.,.F.,.F.,'Update Carton Details') = 2
*!*            RETURN
*!*          *ENDIF
*!*          llFromSave = .T.
*!*          lfALCARTON()
*!*        ELSE
        lfSaveCrtn()
*!*        ENDIF         
    ENDIF 
  ENDIF 
ENDIF 
*!*************************************************************
*! Name      : lfClose
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/11/2008
*! Purpose   : function of the screen close button
*!*************************************************************
FUNCTION lfClose
PARAMETERS loChldFormSet
lcSrcFile =loChldFormSet.loParentForm.lcStylePik




llNotEqual = .F.
IF loChldFormSet.loParentForm.ActiveMode = 'E'
*!*	  SELECT(lcSrcFile)
*!*	  llNotEqual = .F.
*!*	  SCAN 
*!*	    lcCurrStyle = Style
*!*	    lcCurrPik = PIKTKT
*!*	    lnRecNo= RECNO()
*!*	    SUM nQtyAll FOR Style = lcCurrStyle AND ((PIKTKT = lcCurrPik) OR EMPTY(PIKTKT)) TO lnTOTAll
*!*	    IF BETWEEN(lnRecNo,1,RECCOUNT())
*!*	      GO RECORD lnRecNo
*!*	    ENDIF 
*!*	    SUM nCartAll * nQty1Crt ,nCartAll * nQty2Crt ,;
*!*	    nCartAll * nQty3Crt,nCartAll * nQty4Crt,;
*!*	    nCartAll * nQty5Crt,nCartAll * nQty6Crt,;
*!*	    nCartAll * nQty7Crt,nCartAll * nQty8Crt;
*!*	    FOR Style = lcCurrStyle AND ((PIKTKT = lcCurrPik) OR EMPTY(PIKTKT)) TO ;
*!*	    lnTOTAll1,lnTOTAll2,lnTOTAll3,lnTOTAll4,lnTOTAll5,lnTOTAll6,lnTOTAll7,lnTOTAll8
*!*	    IF BETWEEN(lnRecNo,1,RECCOUNT())
*!*	      GO RECORD lnRecNo
*!*	    ENDIF 
*!*	    llSizes = lfChkAlSiz()
*!*	    IF BETWEEN(lnRecNo,1,RECCOUNT())
*!*	      GO RECORD lnRecNo
*!*	    ENDIF 
*!*	    IF (lnTOTAll <> nQtyPik ) OR !llSizes
*!*	      llNotEqual = .T.
*!*	      EXIT 
*!*	    ENDIF 
*!*	  ENDSCAN 
SELECT(lcSrcFile)
LOCATE 
IF !EOF()
 
SELECT (lcOrdln)
SCAN FOR cSTatus <> 'R' AND Picked 
  lnRecNum = RECNO()
  lcPIKTKT = Piktkt
  lcOrderCur = &lcOrdln..Order
  lcStyCurr = &lcOrdln..Style 
  SUM PIK1,PIK2,PIK3,PIK4,PIK5,PIK6,PIK7,PIK8,TOTPIK FOR ;
      CORDTYPE+ORDER+STR(LINENO,6)= "O"+lcOrderCur  AND Style =lcStyCurr AND  PIKTKT = lcPIKTKT TO ;
      lnPIK1,lnPIK2,lnPIK3,lnPIK4,lnPIK5,lnPIK6,lnPIK7,lnPIK8,lnTOTPIK
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
  SELECT Style_Carton
  lcStyle = SUBSTR(lcStyCurr,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1) 
  =gfSeek(ALLTRIM(lcStyle),'Style_Carton')
  lnSclCnt = 1
  SCAN REST WHILE Style = lcStyle 
	=gfSeek('S'+Style_Carton.Scale,'Scale_Carton') 
	IF Style_Carton.Style = ALLTRIM(lcStyCurr)
	  EXIT 
    ELSE
	  lnSclCnt = lnSclCnt + Scale_carton.cnt
    ENDIF  
  ENDSCAN 
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
  lnAllQty = 0
  SELECT (loFormSet.lcStylePik)
  FOR lnR = lnSclCnt TO lnSclCnt+Scale_carton.cnt - 1
    LOCATE 
    lcR = ALLTRIM(STR(lnR))
    SUM nCartAll * nQty&lcR.Crt FOR ;
	Style = ALLTRIM(SUBSTR(lcStyCurr,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1)) ;
    AND (EMPTY(PIKTKT) OR PIKTKT = ORDLINE.PIKTKT );
    TO lnAllo&lcR.
    
    lnAllQty = lnAllQty + lnAllo&lcR.
  ENDFOR

  SELECT (lcOrdln)   
  IF BETWEEN(lnRecNum ,1,RECCOUNT())      
    GO RECORD lnRecNum 
  ENDIF 
  lnNormCnt = 1
  FOR lnR = lnSclCnt TO lnSclCnt+Scale_carton.cnt - 1
    IF (lnAllQty <> lnTOTPIK) 
      llNotEqual = .T.
      EXIT 
    ENDIF 
    lcNormCnt = ALLTRIM(STR(lnNormCnt))
    lcR = ALLTRIM(STR(lnR))
    IF lnAllo&lcR. <> lnPIK&lcNormCnt.
      llNotEqual = .T.
      EXIT 
    ENDIF 
    lnNormCnt = lnNormCnt + 1 
  ENDFOR
ENDSCAN  
ENDIF  
  IF llNotEqual 
    lnAnswer=gfModalGen("TRM00000B44017","DIALOG",.F.,.F.,'Picked quantity is different from the total cartoned  quantity.') 
    DO CASE 
      CASE lnAnswer = 1
        =lfUpPkdQty()
        lfSaveCrtn()
      CASE lnAnswer = 2
        RETURN
      CASE lnAnswer = 3
        llCSave = .F.
        IF TYPE('llFromSave') = 'U' OR (TYPE('llFromSave') = 'L' AND !llFromSave)
          REPLACE ALL nCartAll WITH nCurrAll,;
                      nQtyAll WITH QtyPerCrt * nCartAll IN (lcSrcFile)
        ENDIF   
        loChldFormSet.release
      ENDCASE
  ELSE
    IF TYPE('llFromSave') = 'L' AND llFromSave
      lfSaveCrtn()
      loChldFormSet.release  
    ELSE
      loChldFormSet.release  
    ENDIF   
  ENDIF 
ELSE
  loChldFormSet.release     
ENDIF   
*!*************************************************************
*! Name      : lfUpPkdQty  
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/12/2008
*! Purpose   : function to update picked qty
*!*************************************************************
FUNCTION lfUpPkdQty  
lcSrcFile = loFormSet.lcStylePik
lcOrdln = loFormSet.lc_tmpordl



IF TYPE('llFromSave') = 'L' AND llFromSave
   SELECT POCRTNMF
  =gfSetOrder('POCRTSTYLE')
  SELECT (lcOrdln)
  SCAN FOR Picked
    STORE 0 TO m.PIk1,m.PIk2,m.PIk3,m.PIk4,m.PIk5,m.PIk6,m.PIk7,m.PIk8,m.TOTPIck
    lcAllStyle = &lcOrdln..Style
    lcStyle = SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1)
    
    
    
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
 	SELECT Style_Carton
	=gfSeek(ALLTRIM(lcStyle),'Style_Carton')
	lnSclCnt = 1
	SCAN REST WHILE Style = lcStyle 
	  =gfSeek('S'+Style_Carton.Scale,'Scale_Carton') 
	  IF Style_Carton.Style = ALLTRIM(lcAllStyle)
 	    EXIT 
	  ELSE
 	    lnSclCnt = lnSclCnt + Scale_carton.cnt
	  ENDIF  
	ENDSCAN 
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
         

    
    
    =SEEK("O"+&lcOrdln..Order+STR(&lcOrdln..LINENo,6),'ORDLINE','ORDLINE') 
    SELECT (lcSrcFile) 
    LOCATE 
    SCAN FOR Style = lcStyle  ;
              AND (EMPTY(PIKTKT) OR PIKTKT = ORDLINE.PIKTKT) AND nCartAll > 0
	  lnQtyAcc = 0
      lnCart = nCartAll   
      DIMENSION laStyCrt[1]
      STORE '' TO laStyCrt
      SELECT POCRTNMF        
      =gfSeek(lcStyle)
      SCAN REST WHILE STYLE+CCARTONTYP =  lcStyle FOR style = lcAllStyle AND ;
         CCARTONTYP = &lcSrcFile..CCARTONTYP  AND  cwarecode = &lcSrcFile..cwarecode AND ;
         (EMPTY(PIKTKT) OR PIKTKT =ORDLINE.PIKTKT) &&AND IIF(&lcSrcFile..FStyle = '*',TOTQTY <= &lcSrcFile..QtyPerCrt,TOTQTY = &lcSrcFile..QtyPerCrt)
         
        llSameSzs = .F.
        lnNormCnt = 1
        FOR lnC = lnSclCnt TO lnSclCnt+  Scale_carton.cnt-1
          lcC = ALLTRIM(STR(lnC))
          lcNormCnt = ALLTRIM(STR(lnNormCnt))
          IF POCRTNMF.Qty&lcNormCnt. = &lcSrcFile..nQty&lcC.Crt
            llSameSzs = .T.
          ELSE
            llSameSzs = .F.  
            EXIT 
          ENDIF 
          lnNormCnt = lnNormCnt +  1
        ENDFOR 
         
        IF !llSameSzs 
          LOOP 
        ENDIF 
         
        IF ASCAN(laStyCrt,CCARTONNO) = 0
          lnCart = lnCart - 1
          IF EMPTY(laStyCrt[1])
            laStyCrt[1] = CCARTONNO
          ELSE
            DIMENSION laStyCrt[ALEN(laStyCrt,1)+1]
            laStyCrt[ALEN(laStyCrt,1)] = CCARTONNO
          ENDIF 
        ENDIF 
        m.PIK1 = m.PIK1 + Qty1 
        m.PIK2 = m.PIK2 + Qty2 
        m.PIK3 = m.PIK3 + Qty3 
        m.PIK4 = m.PIK4 + Qty4
        m.PIK5 = m.PIK5 + Qty5
        m.PIK6 = m.PIK6  + Qty6
        m.PIK7 = m.PIK7 + Qty7
        m.PIK8 = m.PIK8 + Qty8    
        m.TOTPIck =m.PIK1 + m.PIK2 +m.PIK3 +m.PIK4 +m.PIK5 +m.PIK6 +m.PIK7 +m.PIK8 
        
        IF lnCart = 0 
          EXIT 
        ENDIF 
      ENDSCAN 
    ENDSCAN
    SELECT (lcOrdln)
    m.OldPIK1  = &lcOrdln..PIK1
    m.OldPIK2  = &lcOrdln..PIK2
    m.OldPIK3  = &lcOrdln..PIK3
    m.OldPIK4  = &lcOrdln..PIK4
    m.OldPIK5  = &lcOrdln..PIK5
    m.OldPIK6  = &lcOrdln..PIK6
    m.OldPIK7  = &lcOrdln..PIK7
    m.OldPIK8  = &lcOrdln..PIK8
    m.OldTotPik= &lcOrdln..TOTPIK 
    
    REPLACE PIK1 WITH m.PIK1,;
            PIK2 WITH m.PIK2,;
            PIK3 WITH m.PIK3,;
            PIK4 WITH m.PIK4,;
            PIK5 WITH m.PIK5,;
            PIK6 WITH m.PIK6,;
            PIK7 WITH m.PIK7,;
            PIK8 WITH m.PIK8,;
            TOTPIk WITH m.TOTPIck,;
            BOOK1 WITH IIF(BOOK1 < m.PIK1,m.PIK1,BOOK1),;
            BOOK2 WITH IIF(BOOK2 < m.PIK2,m.PIK2,BOOK2),;
            BOOK3 WITH IIF(BOOK3 < m.PIK3,m.PIK3,BOOK3),;
            BOOK4 WITH IIF(BOOK4 < m.PIK4,m.PIK4,BOOK4),;
            BOOK5 WITH IIF(BOOK5 < m.PIK5,m.PIK5,BOOK5),;
            BOOK6 WITH IIF(BOOK6 < m.PIK6,m.PIK6,BOOK6),;
            BOOK7 WITH IIF(BOOK7 < m.PIK7,m.PIK7,BOOK7),;
            BOOK8 WITH IIF(BOOK8 < m.PIK8,m.PIK8,BOOK8),;
            TOTBOOK WITH BOOK1 +BOOK2 +BOOK3 +BOOK4 +BOOK5 +BOOK6 +BOOK7 +BOOK8 ,;
            Qty1 WITH IIF(Qty1 < m.PIK1,m.PIK1,Qty1),;
            Qty2 WITH IIF(Qty2 < m.PIK2,m.PIK2,Qty2),;
            Qty3 WITH IIF(Qty3 < m.PIK3,m.PIK3,Qty3),;
            Qty4 WITH IIF(Qty4 < m.PIK4,m.PIK4,Qty4),;
            Qty5 WITH IIF(Qty5 < m.PIK5,m.PIK5,Qty5),;
            Qty6 WITH IIF(Qty6 < m.PIK6,m.PIK6,Qty6),;
            Qty7 WITH IIF(Qty7 < m.PIK7,m.PIK7,Qty7),;
            Qty8 WITH IIF(Qty8 < m.PIK8,m.PIK8,Qty8),;
            TOTQTY WITH Qty1 +Qty2 +Qty3 +Qty4+Qty5 +Qty6 +Qty7 +Qty8
    SCATTER MEMO MEMVAR         
    IF SEEK("O"+&lcOrdln..Order+STR(&lcOrdln..LINENo,6),'Ordline')          
      SELECT Ordline 
      lnOldTotBoOk = TOTBOOK
      lnOldTotQty = TOTQty
      GATHER MEMO MEMVAR FIELDS PIK1 ,PIK2 ,PIK3 ,PIK4 ,PIK5 ,PIK6 ,PIK7 ,;
                                PIK8 ,TOTPIk ,BOOK1 ,BOOK2 ,BOOK3 ,BOOK4 ,BOOK5 ,BOOK6 ,BOOK7 ,BOOK8 ,TOTBOOK ,;
                                QTY1,Qty2 ,Qty3 ,Qty4 ,Qty5 ,Qty6 ,Qty7 ,Qty8 ,TOTQTY
      IF lnOldTotBoOk  <> m.TOTBOOK OR  lnOldTotQty <> m.TOTQty                         
        IF SEEK('O'+&lcOrdln..Order,'OrdHDR')    
          REPLACE BOOK WITH BOOK - lnOldTotBoOk  +m.TOTBOOK,;
                  OPen WITH OPen -lnOldTotQty +m. TOTQty IN Ordhdr
        ENDIF     
      ENDIF 
      IF SEEK(&lcOrdln..Style,'Style','Style')
        SELECT Style 
        REPLACE alo1 WITH alo1 - m.OldPIK1 + m.PIK1 ,;
                alo2 WITH alo2 - m.OldPIK2 + m.PIK2 ,;
                alo3 WITH alo3 - m.OldPIK3 + m.PIK3 ,;
                alo4 WITH alo4 - m.OldPIK4 + m.PIK4 ,;
                alo5 WITH alo5 - m.OldPIK5 + m.PIK5 ,;
                alo6 WITH alo6 - m.OldPIK6 + m.PIK6 ,;
                alo7 WITH alo7 - m.OldPIK7 + m.PIK7 ,;
                alo8 WITH alo8 - m.OldPIK8 + m.PIK8 ,;
                totalo WITH  totalo - m.OldTotPik + m.TOTPIk 
      ENDIF 
      
      IF SEEK(&lcOrdln..Style+&lcOrdln..cwarecode,'StyDYE','StyDYE')
        SELECT Stydye 
        REPLACE alo1 WITH alo1 - m.OldPIK1 + m.PIK1 ,;
                alo2 WITH alo2 - m.OldPIK2 + m.PIK2 ,;
                alo3 WITH alo3 - m.OldPIK3 + m.PIK3 ,;
                alo4 WITH alo4 - m.OldPIK4 + m.PIK4 ,;
                alo5 WITH alo5 - m.OldPIK5 + m.PIK5 ,;
                alo6 WITH alo6 - m.OldPIK6 + m.PIK6 ,;
                alo7 WITH alo7 - m.OldPIK7 + m.PIK7 ,;
                alo8 WITH alo8 - m.OldPIK8 + m.PIK8 ,;
                totalo WITH  totalo - m.OldTotPik + m.TOTPIk 
      ENDIF 

    ENDIF 
  ENDSCAN 

ELSE

  SELECT POCRTNMF
  =gfSetOrder('POCRTSTYLE')
  SELECT (lcOrdln)
  SCAN FOR Picked
    
    STORE 0 TO m.PIk1,m.PIk2,m.PIk3,m.PIk4,m.PIk5,m.PIk6,m.PIk7,m.PIk8,m.TOTPIck
    lcAllStyle = &lcOrdln..Style
    lcStyle = ALLTRIM(SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1)) 
    SELECT (lcSrcFile) 
*!*	    llnew = .F.
    SCAN FOR Style = lcStyle  ;
              AND (EMPTY(PIKTKT) OR PIKTKT = &lcOrdln..PIKTKT) AND nCartAll > 0 
        
      lnCart = nCartAll        
      
 *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
 	SELECT Style_Carton
	=gfSeek(ALLTRIM(lcStyle),'Style_Carton')
	lnSclCnt = 1
	SCAN REST WHILE Style = lcStyle 
	  =gfSeek('S'+Style_Carton.Scale,'Scale_Carton') 
	  IF Style_Carton.Style = ALLTRIM(lcAllStyle)
 	    EXIT 
	  ELSE
 	    lnSclCnt = lnSclCnt + Scale_carton.cnt
	  ENDIF  
	ENDSCAN 
 	*C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
      DIMENSION laStyCrt[1]
      STORE '' TO laStyCrt 
      SELECT POCRTNMF        
      =gfSeek(lcStyle)
      SCAN REST WHILE STYLE+CCARTONTYP =  lcStyle FOR style = lcAllStyle AND ;
         CCARTONTYP = &lcSrcFile..CCARTONTYP  AND  cwarecode = &lcSrcFile..cwarecode AND;
          (EMPTY(PIKTKT) OR PIKTKT =&lcOrdln..PIKTKT) &&AND IIF(&lcSrcFile..FStyle = '*',TOTQTY <= &lcSrcFile..QtyPerCrt,TOTQTY = &lcSrcFile..QtyPerCrt)

        llSameSzs = .F.
        lnNormCnt = 1
        FOR lnC = lnSclCnt TO lnSclCnt+  Scale_carton.cnt-1
          lcC = ALLTRIM(STR(lnC))
          lcNormCnt = ALLTRIM(STR(lnNormCnt))
          IF POCRTNMF.Qty&lcNormCnt. = &lcSrcFile..nQty&lcC.Crt
            llSameSzs = .T.
          ELSE
            llSameSzs = .F.  
            EXIT 
          ENDIF 
          lnNormCnt = lnNormCnt +  1
        ENDFOR 
         
        IF !llSameSzs 
          LOOP 
        ENDIF 

        IF ASCAN(laStyCrt,CCARTONNO) = 0
          lnCart = lnCart - 1
          IF EMPTY(laStyCrt[1])
            laStyCrt[1] = CCARTONNO
          ELSE
            DIMENSION laStyCrt[ALEN(laStyCrt,1)+1]
            laStyCrt[ALEN(laStyCrt,1)] = CCARTONNO
          ENDIF 
        ENDIF 
        m.PIK1 = m.PIK1 + Qty1 
        m.PIK2 = m.PIK2 + Qty2 
        m.PIK3 = m.PIK3 + Qty3 
        m.PIK4 = m.PIK4 + Qty4
        m.PIK5 = m.PIK5 + Qty5
        m.PIK6 = m.PIK6  + Qty6
        m.PIK7 = m.PIK7 + Qty7
        m.PIK8 = m.PIK8 + Qty8    
        m.TOTPIck =m.PIK1 + m.PIK2 +m.PIK3 +m.PIK4 +m.PIK5 +m.PIK6 +m.PIK7 +m.PIK8 
*!*	        IF POCRTNMF.TOTQTY = EVALUATE(lcSrcFile+'.QtyPerCrt')
*!*	          lnCart = lnCart - 1
*!*	        ELSE
*!*	          lnQtyAcc = lnQtyAcc +  POCRTNMF.TOTQTY
*!*	          IF lnQtyAcc  = EVALUATE(lcSrcFile+'.QtyPerCrt')
*!*	            lnCart = lnCart - 1
*!*	            lnQtyAcc = 0
*!*	          ENDIF 
*!*	        EnDIF 
        *lnCrt = lnCrt - 1
         *ENDIF 
        IF lnCart = 0 
          EXIT 
        ENDIF 
      ENDSCAN 
*!*	      llnew = .F.
    ENDSCAN
    SELECT (lcOrdln)
    REPLACE PIK1 WITH m.PIK1,;
            PIK2 WITH m.PIK2,;
            PIK3 WITH m.PIK3,;
            PIK4 WITH m.PIK4,;
            PIK5 WITH m.PIK5,;
            PIK6 WITH m.PIK6,;
            PIK7 WITH m.PIK7,;
            PIK8 WITH m.PIK8,;
            TOTPIk WITH m.TOTPIck,;
            BOOK1 WITH IIF(BOOK1 < m.PIK1,m.PIK1,BOOK1),;
            BOOK2 WITH IIF(BOOK2 < m.PIK2,m.PIK2,BOOK2),;
            BOOK3 WITH IIF(BOOK3 < m.PIK3,m.PIK3,BOOK3),;
            BOOK4 WITH IIF(BOOK4 < m.PIK4,m.PIK4,BOOK4),;
            BOOK5 WITH IIF(BOOK5 < m.PIK5,m.PIK5,BOOK5),;
            BOOK6 WITH IIF(BOOK6 < m.PIK6,m.PIK6,BOOK6),;
            BOOK7 WITH IIF(BOOK7 < m.PIK7,m.PIK7,BOOK7),;
            BOOK8 WITH IIF(BOOK8 < m.PIK8,m.PIK8,BOOK8),;
            TOTBOOK WITH BOOK1 +BOOK2 +BOOK3 +BOOK4 +BOOK5 +BOOK6 +BOOK7 +BOOK8 ,;
            Qty1 WITH IIF(Qty1 < m.PIK1,m.PIK1,Qty1),;
            Qty2 WITH IIF(Qty2 < m.PIK2,m.PIK2,Qty2),;
            Qty3 WITH IIF(Qty3 < m.PIK3,m.PIK3,Qty3),;
            Qty4 WITH IIF(Qty4 < m.PIK4,m.PIK4,Qty4),;
            Qty5 WITH IIF(Qty5 < m.PIK5,m.PIK5,Qty5),;
            Qty6 WITH IIF(Qty6 < m.PIK6,m.PIK6,Qty6),;
            Qty7 WITH IIF(Qty7 < m.PIK7,m.PIK7,Qty7),;
            Qty8 WITH IIF(Qty8 < m.PIK8,m.PIK8,Qty8),;
            TOTQTY WITH Qty1 +Qty2 +Qty3 +Qty4+Qty5 +Qty6 +Qty7 +Qty8

            *cStatus WITH "M"
        
            
  ENDSCAN 
ENDIF 
loChldFormSet.release
*RETURN (nQty1Pik = nCartAll * nQty1Crt AND nQty2Pik =nCartAll * nQty2Crt AND nQty3Pik = nCartAll * nQty3Crt AND nQty4Pik =nCartAll * nQty4Crt AND ;
        nQty5Pik = nCartAll * nQty5Crt AND nQty6Pik =nCartAll * nQty6Crt AND nQty7Pik =nCartAll * nQty7Crt AND nQty8Pik =nCartAll * nQty8Crt )

*!*************************************************************
*! Name      : lfSaveCrtn
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/11/2008
*! Purpose   : function to save allocated cartons to POCRTNMF File
*!*************************************************************
FUNCTION lfSaveCrtn


SELECT POCRTNMF
*gfSetOrder('POCRTSTYLE')
=gfSetOrder('POCRTNMF')
lcOrdln = loFormSet.lc_tmpordl
SELECT (loformset.lcStylePik)

lcStyPik = loformset.lcStylePik

*!*	opross = CREATEOBJECT('ariaprogressbar')  
*!*	oPross.TotalProgress  =  RECCOUNT()
*!*	oPross.lblFirstLabel.Caption = 'Updating Cartons Info.'
*!*	lnRecCounter = 0
*!*	oPross.AutoCenter = .T.
*!*	oPross.Show()


SCAN FOR nCartAll > 0 OR nOldAll > 0
  lnQtyAcc = 0
  m.Style = ALLTRIM(Style)
*!*	  lnRecCounter = lnRecCounter + 1 
*!*	  oPross.CurrentProgress(lnRecCounter )
  
  lnCart = IIF(nCartAll = 0,nOldAll ,nCartAll)
  *! B609413,1 MMT 09/21/2010 pick carton screen at Memo hangs on partial allocations.[Start]
  *IF gfSeek(EVALUATE(loformset.lcStylePik+'.CCARTONTYP'),'POCRTNMF')
  SELECT POCRTNMF
  if gfSqlRun("Select * from POCRTNMF where CCARTONTYP = '"+;
  			EVALUATE(loformset.lcStylePik+'.CCARTONTYP')+"' AND STYLE LIKE '"+m.Style+"%' AND cwarecode ='"+EVALUATE(loformset.lcStylePik+'.cwarecode')+"'",'POCRTNMF')
  *! B609413,1 MMT 09/21/2010 pick carton screen at Memo hangs on partial allocations.[End]
    SELECT POCRTNMF
    *=gfSetOrder('POCRTNMF')
    SCAN REST WHILE CCARTONTYP+CCARTONNO+CSTYMAJOR+STYLE = EVALUATE(loformset.lcStylePik+'.CCARTONTYP') FOR Style = m.Style AND;
                    cwarecode = EVALUATE(loformset.lcStylePik+'.cwarecode') 
                    &&AND IIF(&lcStyPik..FStyle = '*',POCRTNMF.TOTQTY <= &lcStyPik..QtyPerCrt,POCRTNMF.TOTQTY = &lcStyPik..QtyPerCrt)
                    
    *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
    
      SELECT Style_Carton
	  =gfSeek(m.Style,'Style_Carton')
	  lnSclCnt = 1
	  SCAN REST WHILE Style = m.Style
		=gfSeek('S'+Style_Carton.Scale,'Scale_Carton') 
		IF Style_Carton.Style = ALLTRIM(POCRTNMF.Style)
		   EXIT 
 		ELSE
		  lnSclCnt = lnSclCnt + Scale_carton.cnt
		ENDIF  
	  ENDSCAN 
	  
	  llSameSzs = .F.
      lnNormCnt = 1
      FOR lnC = lnSclCnt TO lnSclCnt+  Scale_carton.cnt-1
        lcC = ALLTRIM(STR(lnC))
        lcNormCnt = ALLTRIM(STR(lnNormCnt))
        IF POCRTNMF.Qty&lcNormCnt. = &lcStyPik..nQty&lcC.Crt
            llSameSzs = .T.
        ELSE
          llSameSzs = .F.  
          EXIT 
        ENDIF 
        lnNormCnt = lnNormCnt +  1
      ENDFOR 
         
      IF !llSameSzs 
        LOOP 
      ENDIF 
	  
      *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
      
                    
      SELECT (lcOrdln)
      LOCATE FOR Style =POCRTNMF.STYLE
      IF FOUND()
        IF SEEK("O"+Order+STR(LINENo,6),'ORDLINE','ORDLINE')
          m.PIKTKT = ORDLINE.PIKTKT
        ENDIF
        IF EVALUATE(loformset.lcStylePik+'.nCartAll') > 0    
          IF (EMPTY(m.PIKTKT) AND POCRTNMF.PIKTKT = EVALUATE(loformset.lcStylePik+'.PIKTKT')) OR;
             (!EMPTY(m.PIKTKT) AND EMPTY(POCRTNMF.PIKTKT)) OR ;
             (!EMPTY(m.PIKTKT) AND POCRTNMF.PIKTKT = EVALUATE(loformset.lcStylePik+'.PIKTKT'))
            SELECT POCRTNMF
            *=gfSetOrder('POCRTSTYLE')
            =gfReplace('PIKTKT WITH m.PIKTKT')
            *=gfSetOrder('POCRTNMF')
            IF POCRTNMF.TOTQTY = EVALUATE(loformset.lcStylePik+'.QtyPerCrt')
              lnCart = lnCart - 1
            ELSE
              lnQtyAcc = lnQtyAcc +  POCRTNMF.TOTQTY
              IF lnQtyAcc  = EVALUATE(loformset.lcStylePik+'.QtyPerCrt')
                lnCart = lnCart - 1
                lnQtyAcc = 0
              ENDIF 
            ENDIF 
          ENDIF   
        ELSE
           SELECT POCRTNMF
           *=gfSetOrder('POCRTSTYLE')
           =gfReplace("PIKTKT WITH ''")
           *=gfSetOrder('POCRTNMF')
           IF POCRTNMF.TOTQTY = EVALUATE(loformset.lcStylePik+'.QtyPerCrt')
             lnCart = lnCart - 1
           ELSE
             lnQtyAcc = lnQtyAcc +  POCRTNMF.TOTQTY
             IF lnQtyAcc  = EVALUATE(loformset.lcStylePik+'.QtyPerCrt')
               lnCart = lnCart - 1
               lnQtyAcc = 0
             ENDIF 
           ENDIF 
        ENDIF   
      ENDIF
      IF lnCart = 0 &&OR (lnQtyAcc = EVALUATE(loformset.lcStylePik+'.QtyPerCrt'))
        EXIT 
      ENDIF 
    ENDSCAN 
    SELECT POCRTNMF
    *=gfSetOrder('POCRTSTYLE')
  ENDIF 
ENDSCAN       
*!*	oPross.Hide()
SELECT POCRTNMF
gfTableUpdate()
*!*************************************************************
*! Name      : lfChkAlSiz
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/12/2008
*! Purpose   : function to Check all sizes qty in cartons to be allocated
*!*************************************************************
FUNCTION lfChkAlSiz

RETURN (nQty1Pik = lnTOTAll1 AND nQty2Pik =lnTOTAll2 AND nQty3Pik = lnTOTAll3 AND nQty4Pik =lnTOTAll4 AND ;
        nQty5Pik = lnTOTAll5 AND nQty6Pik =lnTOTAll6 AND nQty7Pik =lnTOTAll7 AND nQty8Pik =lnTOTAll8)


*!*************************************************************
*! Name      : lfZPCRTDET  
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/12/2008
*! Purpose   : function to Empty Temp. Files
*!*************************************************************
FUNCTION lfZPCRTDET  

IF loFormSet.ActiveMode = "V"
  IF TYPE('loFormSet.lcStylePik')<> 'U' AND USED(loFormSet.lcStylePik)
    SELECT (loFormSet.lcStylePik)
    DELETE ALL
  ENDIF 

  IF TYPE('loFormSet.lcCartTemp')<> 'U' AND  USED(loFormSet.lcCartTemp)
    SELECT (loFormSet.lcCartTemp)
    DELETE ALL
  ENDIF 

  lfGETCRTINF()
ELSE
  RETURN   
ENDIF 

*!*************************************************************
*! Name      : lfGETCRTINF
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/24/2008
*! Purpose   : function to get Cartons Info.
*!*************************************************************
FUNCTION lfGETCRTINF


  lfALOPNFLS  (.T.)
  lcOrdln = loFormSet.lc_tmpordl
  SELECT(lcOrdln)
  lnCurrentRec = RECNO()

    *-- Case New Pitkt is just generated 
    IF !USED('POCRTNMF_A')
      =gfOpenTable('POCRTNMF','POCRTNMF','SH','POCRTNMF_A')
    ENDIF  
     
     
    SELECT POCRTNMF
    gfSetOrder('POCRTSTYLE')
    SELECT (lcOrdln)
    SCAN FOR picked &&OR ((TYPE('llFromSave') = 'L' AND llFromSave) AND CStatus = 'R')
      IF !SEEK(&lcOrdln..cOrdType + &lcOrdln..ORDER  + STR(&lcOrdln..LineNo,6),loformset.lcOrdTemp)
        SCATTER MEMO MEMVAR 
        INSERT INTO  (loformset.lcOrdTemp) FROM MEMVAR 
      ENDIF 
      
      m.PIKTKT = PIKTKT
      IF EMPTY(PIKTKT) and SEEK("O"+&lcOrdln..Order+STR(LINENo,6),'ORDLINE','ORDLINE') AND !EMPTY(ORDLINE.PIKTKT)
        m.PIKTKT = Ordline.PIKTKT
      ENDIF 
      

      FOR lnT =1 TO loformset.lnMaxSiz 
    		lcT = ALLTRIM(STR(lnT))
		    STORE 0 TO m.nQty&lcT.Crt
      ENDFOR  

      
      m.nQtyPik = TOTPIK
      m.Style = SUBSTR(&lcOrdln..Style,1,loFormSet.lnClrlen+loFormSet.lnClrStrt-1)
      m.cwarecode = &lcOrdln..cwarecode
      m.CStyMajor = SUBSTR(&lcOrdln..Style,1,loFormSet.lnMajLen)
      IF gfSeek(&lcOrdln..Style,'POCRTNMF')
        SELECT Style_Carton
        =gfSeek(ALLTRIM(m.Style),'Style_Carton')
        lnSclCnt = 1
        SCAN REST WHILE Style = m.Style
          =gfSeek('S'+Style_Carton.Scale,'Scale_Carton') 
          IF Style_Carton.Style = ALLTRIM(POCRTNMF.Style)
            EXIT 
          ELSE
            lnSclCnt = lnSclCnt + Scale_carton.cnt
          ENDIF  
        ENDSCAN 
        SELECT POCRTNMF
        SCAN REST WHILE STYLE+CCARTONTYP = &lcOrdln..Style FOR cwarecode  = &lcOrdln..cwarecode AND IIF(!EMPTY(m.PIKTKT) AND m.piktkt <> '******',PIKTKT = m.PIKTKT,EMPTY(PIKTKT))
          IF SEEK(m.PIKTKT+PADR(m.Style,19)+POCRTNMF.CCARTONTYP,loformset.lcStylePik)
            SELECT (loformset.lcStylePik)
            llFound = .F.
            SCAN REST WHILE Piktkt+Style+CCARTONTYP  = m.PIKTKT+PADR(m.Style,19)+POCRTNMF.CCARTONTYP &&FOR QtyPerCrt = POCRTNMF.TOTQTY 
              *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
              
               
               lnI = 1 
               FOR lnY = lnSclCnt TO lnSclCnt+ Scale_carton.cnt-1
                 lcI = ALLTRIM(STR(lnI))
                 lcY = ALLTRIM(STR(lnY))
                 IF nQty&lcY.Crt <> POCRTNMF.qty&lcI. 
                   llFound = .F.
                   EXIT 
                 ELSE
                   llFound = .T.  
          			 ENDIF 
                 lnI = lnI +  1
               ENDFOR 
               IF llFound 
                 EXIT 
               ENDIF   
             ENDSCAN
   *+&lcOrdln..Style 
            IF llFound AND !SEEK(PADR(m.Style,19)+POCRTNMF.CCARTONNO,loformset.lcCartTemp)
              IF !EMPTY(POCRTNMF.PIKTKT) AND m.PIKTKT=POCRTNMF.PIKTKT
                IF !EMPTY(EVALUATE(loformset.lcStylePik+'.FStyle')) and &lcOrdln..Style <> EVALUATE(loformset.lcStylePik+'.FStyle')
                  REPLACE QtyPerCrt WITH  QtyPerCrt + POCRTNMF.totqty ,;
                          MCartons WITH  MCartons +","+ POCRTNMF.CCARTONNO  IN (loformset.lcStylePik)
                  
                  lnI = 1 
                  FOR lnY = lnSclCnt TO lnSclCnt+ Scale_carton.cnt-1
                    lcI = ALLTRIM(STR(lnI))
                    lcY = ALLTRIM(STR(lnY))
                    REPLACE  nQty&lcY.Crt WITH nQty&lcY.Crt + POCRTNMF.qty&lcI. IN (loformset.lcStylePik)
                    lnI = lnI +  1
                  ENDFOR  
                  
                  *        nQty1Crt WITH nQty1Crt+POCRTNMF.QTY1,;
                          nQty2Crt WITH nQty2Crt+POCRTNMF.QTY2,;
                          nQty3Crt WITH nQty3Crt+POCRTNMF.QTY3,;
                          nQty4Crt WITH nQty4Crt+POCRTNMF.QTY4,;
                          nQty5Crt WITH nQty5Crt+POCRTNMF.QTY5,;
                          nQty6Crt WITH nQty6Crt+POCRTNMF.QTY6,;
                          nQty7Crt WITH nQty7Crt+POCRTNMF.QTY7,;
                          nQty8Crt WITH nQty8Crt+POCRTNMF.QTY8 IN (loformset.lcStylePik)
                ENDIF   
                
                              
                
                REPLACE nCartAll WITH nCartAll + 1 ,;
                        nQtyAll WITH QtyPerCrt * nCartAll ,;
                        nOldAll WITH nOldAll + 1 ,;
                        MCartons WITH  MCartons +","+ POCRTNMF.CCARTONNO  ,;
                        FStyle   With IIF(EMPTY(FStyle),&lcOrdln..Style,IIF(FStyle <> &lcOrdln..Style,'*',FStyle));                            
                        IN (loformset.lcStylePik)
                        
                        
                        * nQty1Crt WITH nQty1Crt+POCRTNMF.QTY1,;
                        nQty2Crt WITH nQty2Crt+POCRTNMF.QTY2,;
                        nQty3Crt WITH nQty3Crt+POCRTNMF.QTY3,;
                        nQty4Crt WITH nQty4Crt+POCRTNMF.QTY4,;
                        nQty5Crt WITH nQty5Crt+POCRTNMF.QTY5,;
                        nQty6Crt WITH nQty6Crt+POCRTNMF.QTY6,;
                        nQty7Crt WITH nQty7Crt+POCRTNMF.QTY7,;
                        nQty8Crt WITH nQty8Crt+POCRTNMF.QTY8
                        
              ELSE
                IF EMPTY(POCRTNMF.PIKTKT)
                  REPLACE nCartAvl WITH nCartAvl + 1 ,;
                          MCartons WITH  MCartons +","+ POCRTNMF.CCARTONNO  ,;
                          FStyle   With IIF(EMPTY(FStyle),&lcOrdln..Style,IIF(FStyle <> &lcOrdln..Style,'*',FStyle));             
                          IN (loformset.lcStylePik)
                  lnI = 1 
                  FOR lnY = lnSclCnt TO lnSclCnt+ Scale_carton.cnt-1
                    lcI = ALLTRIM(STR(lnI))
                    lcY = ALLTRIM(STR(lnY))
                    REPLACE  nQty&lcY.Crt WITH  POCRTNMF.qty&lcI. IN (loformset.lcStylePik)
                    lnI = lnI +  1
                  ENDFOR  
                  *        nQty1Crt WITH  POCRTNMF.QTY1,;
                          nQty2Crt WITH  POCRTNMF.QTY2,;
                          nQty3Crt WITH  POCRTNMF.QTY3,;
                          nQty4Crt WITH  POCRTNMF.QTY4,;
                          nQty5Crt WITH  POCRTNMF.QTY5,;
                          nQty6Crt WITH  POCRTNMF.QTY6,;
                          nQty7Crt WITH  POCRTNMF.QTY7,;
                          nQty8Crt WITH  POCRTNMF.QTY8,; 
                          FStyle   With IIF(EMPTY(FStyle),&lcOrdln..Style,IIF(FStyle <> &lcOrdln..Style,'*',FStyle));             
                          IN (loformset.lcStylePik)
                ELSE
                  LOOP           
                ENDIF   
              ENDIF 
              m.QtyPerCrt = POCRTNMF.TOTQTY
              m.CCARTONNO = POCRTNMF.CCARTONNO
              m.NQtyPerCrn =  POCRTNMF.TOTQTY
              m.HStyle =&lcOrdln..Style 
              INSERT INTO (loformset.lcCartTemp) FROM MEMVAR 
            ELSE
              IF !SEEK(PADR(m.Style,19)+POCRTNMF.CCARTONNO+ &lcOrdln..Style ,loformset.lcCartTemp)
                IF SEEK(PADR(m.Style,19)+POCRTNMF.CCARTONNO,loformset.lcCartTemp)
                  IF SEEK(m.PIKTKT+PADR(m.Style,19)+POCRTNMF.CCARTONTYP,loformset.lcStylePik)
                    *IF gfSeek(POCRTNMF.CCARTONTYP+POCRTNMF.CCARTONNO+PADR(SUBSTR(EVALUATE(loformset.lcStylePik+'.Style'),1,loformset.lnMajLen),19)+ALLTRIM(EVALUATE(loformset.lcStylePik+'.Style')),'POCRTNMF_A')
                      *LOCATE FOR Style = loformset.lcCartTemp.HSTYLE
                      lnTOTCrtSty = 0
                      SELECT (loformset.lcCartTemp)
                      SUM NQtyPerCrn REST WHILE Style+CCARTONNO+HStyle =PADR(m.Style,19)+POCRTNMF.CCARTONNO TO lnTOTCrtSty
                      
                      *m.NQtyPerCrn = = POCRTNMF.TOTQTY
                      *lnQTyTOtal = POCRTNMF_A.TOTQTY
                    *ENDIF   
                    SELECT (loformset.lcStylePik)
                    SCAN REST WHILE Piktkt+Style+CCARTONTYP  = m.PIKTKT+PADR(m.Style,19)+POCRTNMF.CCARTONTYP ;
                                    FOR QtyPerCrt = lnTOTCrtSty AND POCRTNMF.CCARTONNO $ MCartons  
*!*                        llRecord = .F.
*!*                        lnI = 1 
*!*                        FOR lnY = lnSclCnt TO lnSclCnt+ Scale_carton.cnt-1
*!*                            lcI = ALLTRIM(STR(lnI))
*!*                            lcY = ALLTRIM(STR(lnY))
*!*                            IF  nQty&lcY.Crt <>  POCRTNMF.qty&lcI. 
*!*                              llRecord = .F.
*!*                              EXIT 
*!*                            ELSE
*!*                              llRecord = .T.
*!*                            ENDIF 
*!*                            lnI = lnI +  1
*!*                         ENDFOR  
*!*                         IF !llRecord 
*!*                           LOOP 
*!*                         ENDIF 
                       
                      IF !EMPTY(EVALUATE(loformset.lcStylePik+'.FStyle')) and &lcOrdln..Style <> EVALUATE(loformset.lcStylePik+'.FStyle')
                        REPLACE QtyPerCrt WITH QtyPerCrt +POCRTNMF.totqty IN (loformset.lcStylePik)

                        lnI = 1 
                        FOR lnY = lnSclCnt TO lnSclCnt+ Scale_carton.cnt-1
                          lcI = ALLTRIM(STR(lnI))
                          lcY = ALLTRIM(STR(lnY))
                          REPLACE  nQty&lcY.Crt WITH nQty&lcY.Crt +POCRTNMF.qty&lcI. IN (loformset.lcStylePik)
                          lnI = lnI +  1
                        ENDFOR  
                        *        nQty1Crt WITH nQty1Crt + POCRTNMF.QTY1,;
                                nQty2Crt with nQty2Crt + POCRTNMF.QTY2,;
                                nQty3Crt with nQty3Crt + POCRTNMF.QTY3,;
                                nQty4Crt with nQty4Crt + POCRTNMF.QTY4,;
                                nQty5Crt with nQty5Crt + POCRTNMF.QTY5,;
                                nQty6Crt with nQty6Crt + POCRTNMF.QTY6,;
                                nQty7Crt with nQty7Crt + POCRTNMF.QTY7,;
                                nQty8Crt with nQty8Crt + POCRTNMF.QTY8 IN (loformset.lcStylePik)
                      ENDIF   
                      
                      REPLACE nQtyAll WITH QtyPerCrt * nCartAll ,;
                              MCartons WITH  MCartons +","+ POCRTNMF.CCARTONNO  ,;
                              FStyle   With IIF(EMPTY(FStyle),&lcOrdln..Style,IIF(FStyle <> &lcOrdln..Style,'*',FStyle))             
                              
                              * 
                      EXIT      
                    ENDSCAN
                    m.CCARTONNO = POCRTNMF.CCARTONNO
                    m.HStyle = &lcOrdln..Style 
                    m.NQtyPerCrn =  POCRTNMF.TOTQTY
                    m.CCARTONTYP = POCRTNMF.CCARTONTYP
                    INSERT INTO (loformset.lcCartTemp) FROM MEMVAR 
                    LOOP  
                  ENDIF 
                ENDIF 
                
                SELECT (loformset.lcStylePik)  
                m.CCARTONTYP = POCRTNMF.CCARTONTYP
                m.QtyPerCrt = POCRTNMF.TOTQTY
                lnI = 1 
                FOR lnY = lnSclCnt TO lnSclCnt+ Scale_carton.cnt-1
                  lcI = ALLTRIM(STR(lnI))
                  lcY = ALLTRIM(STR(lnY))
                  m.nQty&lcY.Crt =  POCRTNMF.qty&lcI. 
                  lnI = lnI +  1
                ENDFOR  
*!*                  
*!*                  m.nQty1Crt = POCRTNMF.QTY1
*!*                  m.nQty2Crt = POCRTNMF.QTY2
*!*                  m.nQty3Crt = POCRTNMF.QTY3
*!*                  m.nQty4Crt = POCRTNMF.QTY4
*!*                  m.nQty5Crt = POCRTNMF.QTY5
*!*                  m.nQty6Crt = POCRTNMF.QTY6
*!*                  m.nQty7Crt = POCRTNMF.QTY7
*!*                  m.nQty8Crt = POCRTNMF.QTY8                            
                m.FStyle   = &lcOrdln..Style
                IF !EMPTY(POCRTNMF.PIKTKT) AND m.PIKTKT=POCRTNMF.PIKTKT
                   m.nCartAll = 1
                   m.nOldAll = 1
                   m.nCartAvl = 0
                   m.nQtyAll = m.QtyPerCrt
                ELSE
                  IF EMPTY(POCRTNMF.PIKTKT)
                    m.nCartAvl = 1 
                    m.nCartAll = 0
                    m.nOldAll  = 0
                    m.nQtyAll  = 0
*!*                      m.nQty1Crt = 0
*!*                      m.nQty2Crt = 0
*!*                      m.nQty3Crt = 0
*!*                      m.nQty4Crt = 0
*!*                      m.nQty5Crt = 0
*!*                      m.nQty6Crt = 0
*!*                      m.nQty7Crt = 0
*!*                      m.nQty8Crt = 0
                  ELSE
                    LOOP 
                  ENDIF   
                ENDIF   
                IF !SEEK(m.PIKTKT+PADR(m.Style,19),loformset.lcStylePik)
                  m.cColor = gfCodDes(SUBSTR(&lcOrdln..Style,loFormSet.lnClrStrt,loFormSet.lnClrlen), 'COLOR     ')
                ELSE
                  m.cColor =  ''
                ENDIF    
                m.MCartons = POCRTNMF.CCARTONNO
                APPEND BLANK 
                GATHER MEMO MEMVAR   
                m.CCARTONNO = POCRTNMF.CCARTONNO
                m.QtyPerCrt = POCRTNMF.TOTQTY
                m.HStyle =&lcOrdln..Style 
                m.NQtyPerCrn = POCRTNMF.TOTQTY
                INSERT INTO (loformset.lcCartTemp) FROM MEMVAR 
              ENDIF   
            ENDIF   
          ELSE
            IF !SEEK(PADR(m.Style,19)+POCRTNMF.CCARTONNO+&lcOrdln..Style ,loformset.lcCartTemp)
              SELECT (loformset.lcStylePik)  
              m.CCARTONTYP = POCRTNMF.CCARTONTYP
              m.QtyPerCrt = POCRTNMF.TOTQTY
              lnI = 1 
              FOR lnY = lnSclCnt TO lnSclCnt+ Scale_carton.cnt-1
                lcI = ALLTRIM(STR(lnI))
                lcY = ALLTRIM(STR(lnY))
                m.nQty&lcY.Crt =  POCRTNMF.qty&lcI. 
                lnI = lnI +  1
              ENDFOR 
*!*                m.nQty1Crt = POCRTNMF.QTY1
*!*                m.nQty2Crt = POCRTNMF.QTY2
*!*                m.nQty3Crt = POCRTNMF.QTY3
*!*                m.nQty4Crt = POCRTNMF.QTY4
*!*                m.nQty5Crt = POCRTNMF.QTY5
*!*                m.nQty6Crt = POCRTNMF.QTY6
*!*                m.nQty7Crt = POCRTNMF.QTY7
*!*                m.nQty8Crt = POCRTNMF.QTY8    
              m.FStyle   = &lcOrdln..Style                        
              IF !EMPTY(POCRTNMF.PIKTKT) AND m.PIKTKT=POCRTNMF.PIKTKT
                m.nCartAll = 1
                m.nOldAll = 1
                m.nCartAvl = 0
                m.nQtyAll = m.QtyPerCrt
              ELSE
                IF EMPTY(POCRTNMF.PIKTKT)
                  m.nCartAvl = 1 
                  m.nCartAll = 0
                  m.nOldAll = 0
                  m.nQtyAll = 0
*!*                    m.nQty1Crt = 0
*!*                    m.nQty2Crt = 0
*!*                    m.nQty3Crt = 0
*!*                    m.nQty4Crt = 0
*!*                    m.nQty5Crt = 0
*!*                    m.nQty6Crt = 0
*!*                    m.nQty7Crt = 0
*!*                    m.nQty8Crt = 0
                ELSE
                  LOOP 
                ENDIF   
              ENDIF   
              IF !SEEK(m.PIKTKT+PADR(m.Style,19),loformset.lcStylePik)
                m.cColor = gfCodDes(SUBSTR(&lcOrdln..Style,loFormSet.lnClrStrt,loFormSet.lnClrlen), 'COLOR     ')
              ELSE
                m.cColor =  ''
              ENDIF   
              M.MCartons = POCRTNMF.CCARTONNO 
              APPEND BLANK 
              GATHER MEMO MEMVAR  
              m.CCARTONNO = POCRTNMF.CCARTONNO
              m.QtyPerCrt = POCRTNMF.TOTQTY
              m.HStyle =&lcOrdln..Style 
              m.NQtyPerCrn = POCRTNMF.TOTQTY
              INSERT INTO (loformset.lcCartTemp) FROM MEMVAR  
            ENDIF   
          ENDIF 
        ENDSCAN  
      ENDIF   
    ENDSCAN  
 
    
    lcStyPik = loformset.lcStylePik
    SELECT (lcStyPik) 
    SCAN 
      REPLACE nCurrAll WITH nCartAll
      lnCurrRec = RECNO()
      lcPiktkt = &lcStyPik..PIKTKT
      lcStyleClr = &lcStyPik..Style
      SELECT(lcOrdln)
      LOCATE 
      IF (TYPE('llFromSave') = 'L' AND llFromSave)
        =SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
        SELECT Ordline 
        SUM TOTPIK REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+&lcOrdln..Order FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED
      ELSE
        SUM TOTPIK FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED  
      ENDIF   
      

*!*        IF (TYPE('llFromSave') = 'L' AND llFromSave)
*!*          =SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
*!*          SELECT Ordline 
*!*          SUM PIK1 REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+&lcOrdln..Order FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED1
*!*        ELSE
*!*          SUM PIK1 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED1        
*!*        ENDIF   
*!*        
*!*        

*!*        IF (TYPE('llFromSave') = 'L' AND llFromSave)
*!*          =SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
*!*          SELECT Ordline 
*!*          SUM PIK2 REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+&lcOrdln..Order FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED2        
*!*        ELSE
*!*          SUM PIK2 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED2        
*!*        ENDIF   


*!*        
*!*        IF (TYPE('llFromSave') = 'L' AND llFromSave)
*!*          =SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
*!*          SELECT Ordline 
*!*          SUM PIK3 REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+&lcOrdln..Order FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED3
*!*        ELSE
*!*          SUM PIK3 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED3  
*!*        ENDIF   

*!*        
*!*        
*!*        IF (TYPE('llFromSave') = 'L' AND llFromSave)
*!*          =SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
*!*          SELECT Ordline 
*!*          SUM PIK4 REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+&lcOrdln..Order FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED4
*!*        ELSE
*!*          SUM PIK4 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED4  
*!*        ENDIF   
*!*        
*!*        

*!*        IF (TYPE('llFromSave') = 'L' AND llFromSave)
*!*          =SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
*!*          SELECT Ordline 
*!*          SUM PIK5 REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+&lcOrdln..Order FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED5
*!*        ELSE
*!*          SUM PIK5 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED5  
*!*        ENDIF   
*!*        


*!*        IF (TYPE('llFromSave') = 'L' AND llFromSave)
*!*          =SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
*!*          SELECT Ordline 
*!*          SUM PIK6 REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+&lcOrdln..Order FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED6
*!*        ELSE
*!*          SUM PIK6 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED6  
*!*        ENDIF   
*!*        
*!*        
*!*        
*!*        IF (TYPE('llFromSave') = 'L' AND llFromSave)
*!*          =SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
*!*          SELECT Ordline 
*!*          SUM PIK7 REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+&lcOrdln..Order FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED7
*!*        else
*!*          SUM PIK7 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED7
*!*        ENDIF   


*!*        
*!*        IF (TYPE('llFromSave') = 'L' AND llFromSave)
*!*          =SEEK("O"+&lcOrdln..Order,'ORDLINE','ORDLINE') 
*!*          SELECT Ordline 
*!*          SUM PIK8 REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O"+&lcOrdln..Order FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED8
*!*        ELSE
*!*          SUM PIK8 FOR Style = ALLTRIM(lcStyleClr) AND PIKTKT =lcPiktkt  TO lnTOTPIKED8  
*!*        ENDIF   
*!*        
      
      
      SELECT (lcStyPik)
      REPLACE nQtyPik WITH lnTOTPIKED;
               FOR Style = lcStyleClr AND PIKTKT =lcPiktkt 
     
     * nQty1Pik WITH lnTOTPIKED1,;
              nQty2Pik WITH lnTOTPIKED2,;
              nQty3Pik WITH lnTOTPIKED3,;
              nQty4Pik WITH lnTOTPIKED4,;
              nQty5Pik WITH lnTOTPIKED5,;
              nQty6Pik WITH lnTOTPIKED6,;
              nQty7Pik WITH lnTOTPIKED7,;
              nQty8Pik WITH lnTOTPIKED8
     
              
      IF BETWEEN( lnCurrRec ,1,RECCOUNT())
        GO RECORD  lnCurrRec 
      ENDIF 
    ENDSCAN 
    LOCATE  
    sELECT(lcOrdln)
    IF BETWEEN(lnCurrentRec,1,RECCOUNT())
      GO RECORD lnCurrentRec
    ENDIF 
*!*************************************************************
*! Name      : lfChkTot
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/24/2008
*! Purpose   : function to Save Old Cartons value
*!*************************************************************
FUNCTION lfChkTot
PARAMETERS loChldForm
loChldForm.lnOldValCrt = loChldForm.ariaForm1.grdClrCrt.Column5.Text1.value

*!*************************************************************
*! Name      : lfCRTNDTL
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/25/2008
*! Purpose   : function to add option to option menu of style screen
*!*************************************************************
FUNCTION lfCRTNDTL
LOCAL lnCntBar
lnCntBar = CNTBAR('_lPopOpt')+1
DEFINE BAR lnCntBar OF _lPopOpt PROMPT '\<View Carton Detail' SKIP FOR ;
    (_Screen.ActiveForm.Parent.ActiveMode $ 'SA') OR  (_Screen.ActiveForm.Parent.ariaForm1.pgfStyleInfo.ActivePage <> 2)

ON SELECTION BAR lnCntBar OF _lPopOpt DO lfOpnCrtn
*!*************************************************************
*! Name      : lfOpnCrtn
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/25/2008
*! Purpose   : function to Open Cartons table
*!*************************************************************
FUNCTION lfOpnCrtn

lcAlias = SELECT(0)
loParentForm = _screen.ActiveForm.Parent

STORE 0 TO lnClrLen,LNCLRSTRT,lnSclLen ,lnSclPos 
  
DIMENSION laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    LNCLRSTRT = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])    
     
  CASE  laItemSeg[lnCount,1]='S'
    lnSclLen = LEN(laItemSeg[lnCount,3])
    lnSclPos = laItemSeg[lnCount,4]
    
  ENDCASE  
ENDFOR
lnMajLen = LEN(gfItemMask("PM","",'0001'))
lnStyClrLen = LNCLRSTRT +  lnClrLen -1


IF !USED('POCRTNMF')
  =gfOpenTable('POCRTNMF','POCRTSTYLE')
ENDIF


IF !USED('Style_Crt')
  =gfOpenTable('Style','Style','Sh','Style_Crt')
ENDIF

IF !USED('Scale_Crt')
  =gfOpenTable('Scale','Scale','Sh','Scale_Crt')
ENDIF

lnMaxSzNo = 0
SELECT Style_Crt
=gfSeek(IIF(loParentForm.llAllColors,ALLTRIM(loParentForm.lcStyMajor),loParentForm.lcStyleKey))
lcFrstClr = SUBSTR(Style_Crt.Style,1,LNCLRSTRT+lnClrLen -1)
SCAN REST WHILE Style = IIF(loParentForm.llAllColors,ALLTRIM(loParentForm.lcStyMajor),loParentForm.lcStyleKey) ;
     FOR IIF(loParentForm.llAllColors,SUBSTR(Style_Crt.Style,1,LNCLRSTRT+lnClrLen -1) =lcFrstClr ,.T.)
  =gfSeek('S'+Style_crt.Scale,'Scale_Crt')
  lnMaxSzNo = lnMaxSzNo + Scale_Crt.cnt
ENDSCAN 

SELECT 'POCRTNMF'
IF loParentForm.llAllColors
  =gfSeek(loParentForm.lcStyMajor)
ELSE
  =gfSeek(loParentForm.lcStyleKey)
ENDIF


lcTempCrt = gfTempName()
lcScaleTable  = gfTempName()
lfCrtTmpFile()
lcStyleType = ' '
DIMENSION laCartons[1]
SELECT POCRTNMF
SCAN REST WHILE STYLE+CCARTONTYP = IIF(loParentForm.llAllColors,loParentForm.lcStyMajor,loParentForm.lcStyleKey) FOR EMPTY(Invoice)    
  m.cTyp = ''
  IF STYLE+CCARTONTYP <> lcStyleType
    DIMENSION laCartons[1]
  ENDIF 
  SCATTER MEMO MEMVAR    
  m.NCRTN = 0
  m.Total = TotQty
  m.ClrScale = SUBSTR(m.Style,LNCLRSTRT)  
  SELECT Style_Crt
  =gfSeek(IIF(loParentForm.llAllColors,ALLTRIM(loParentForm.lcStyMajor),loParentForm.lcStyleKey))
  =gfSeek('S'+Style_crt.Scale,'Scale_Crt')
  FOR lnC = 1 TO lnMaxSzNo 
    lcC = ALLTRIM(STR(lnC))
    STORE 0 TO m.Qty&lcC.
  ENDFOR       
  IF loParentForm.llAllColors                                                                                           
    lnCnting = 1
    SCAN REST WHILE Style = IIF(loParentForm.llAllColors,ALLTRIM(loParentForm.lcStyMajor),loParentForm.lcStyleKey) FOR IIF(loParentForm.llAllColors,Style=ALLTRIM(SUBSTR(POCRTNMF.Style,1,LNCLRSTRT+lnClrLen -1)),.T.)
      =gfSeek('S'+Style_crt.Scale,'Scale_Crt')
      IF POCRTNMF.Style <> Style_Crt.Style 
        lnCnting = lnCnting + Scale_Crt.cnt
      ELSE
        FOR lnE = 1 TO Scale_Crt.cnt
          lcE = ALLTRIM(STR(lnE))
          lcCnting = ALLTRIM(STR(lnCnting))
          m.Qty&lcCnting = POCRTNMF.Qty&lcE
          IF !SEEK(SUBSTR(m.Style,1,lnStyClrLen),lcScaleTable)
            m.Scl&lcCnting = Scale_Crt.SZ&lcE
            INSERT INTO (lcScaleTable) FROM MEMVAR 
            REPLACE Style WITH SUBSTR(m.Style,1,lnStyClrLen) IN (lcScaleTable) 
          ELSE
            *SELECT (lcScaleTable)
            REPLACE Scl&lcCnting  WITH Scale_Crt.SZ&lcE IN (lcScaleTable) 
          ENDIF 
          lnCnting = lnCnting + 1
        ENDFOR
        EXIT 
      ENDIF 
    ENDSCAN 
  ELSE
    IF !SEEK(SUBSTR(m.Style,1,lnStyClrLen),lcScaleTable)
      FOR lnE = 1 TO Scale_Crt.cnt
        lcE=ALLTRIM(STR(lnE))
        m.Scl&lcE = Scale_Crt.SZ&lcE
      ENDFOR  
      INSERT INTO (lcScaleTable) FROM MEMVAR 
      REPLACE Style WITH SUBSTR(m.Style,1,lnStyClrLen) IN (lcScaleTable)        
    ENDIF   
  ENDIF 
  m.cTypeCrt = m.CCARTONTYP
  
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
  llCartonNotFound = .F.
  IF SEEK(m.CCARTONTYP+m.Style+' '+m.cShipNo+m.PIKTKT,lcTempCrt)  
    SELECT (lcTempCrt)
    SCAN REST WHILE CCARTONTYP+Style+cTyp+cShipNo+PIKTKT = m.CCARTONTYP+m.Style+' '+m.cShipNo+m.PIKTKT 
      FOR lnC = 1 TO lnMaxSzNo 
        lcC = ALLTRIM(STR(lnC))
        IF QTY&lcC. <> m.Qty&lcC.
          llCartonNotFound = .F.
          EXIT 
        ELSE
          llCartonNotFound = .T.
        ENDIF 
      ENDFOR   
      IF llCartonNotFound 
        EXIT 
      ENDIF 
    ENDSCAN 
  ENDIF
  SELECT POCRTNMF
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
  
  
  IF !llCartonNotFound &&!SEEK(m.CCARTONTYP+m.Style+' '+m.cShipNo+m.PIKTKT,lcTempCrt)  
    m.NCRTN = 1
    INSERT INTO (lcTempCrt) from MEMVAR 
    SELECT(lcTempCrt)
    =SEEK(m.CCARTONTYP+m.Style)
    LOCATE REST WHILE CCARTONTYP+Style+cTyp+cShipNo+PIKTKT = m.CCARTONTYP+m.Style FOR cTyp = 'X'
    IF !FOUND()
      *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
      FOR lnC = 1 TO lnMaxSzNo 
        lcC = ALLTRIM(STR(lnC))
        STORE 0 TO m.QTY&lcC.
        SELECT(lcTempCrt)
        =SEEK(m.CCARTONTYP+m.Style)      
        SUM QTY&lcC.,TOTAL REST WHILE CCARTONTYP+Style+cTyp+cShipNo+PIKTKT = m.CCARTONTYP+m.Style FOR cTyp <> 'X' TO m.QTY&lcC.,m.Total
      ENDFOR 
      *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
    
      m.Piktkt = ''
      m.cShipNo = ''
      m.cTyp = 'X'
      m.NCRTN = 1
      m.cTypeCrt = ''
      INSERT INTO (lcTempCrt) from MEMVAR 
      IF EMPTY(laCartons[1])
        laCartons[1] = m.cCartonNo
      ELSE
        DIMENSION laCartons[ALEN(laCartons,1)+1]  
        laCartons[ALEN(laCartons,1)]  = m.cCartonNo
      ENDIF
    ELSE
      IF ASCAN(laCartons,m.cCartonNo,1) = 0
        *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
        FOR lnC = 1 TO lnMaxSzNo 
          lcC = ALLTRIM(STR(lnC))
          STORE 0 TO m.QTY&lcC.
          SELECT(lcTempCrt)
          =SEEK(m.CCARTONTYP+m.Style)      
          SUM QTY&lcC.,TOTAL REST WHILE CCARTONTYP+Style+cTyp+cShipNo+PIKTKT = m.CCARTONTYP+m.Style FOR cTyp <> 'X' TO m.QTY&lcC.,m.TOTAL
        ENDFOR 
        SELECT(lcTempCrt)
        =SEEK(m.CCARTONTYP+m.Style)      
        LOCATE REST WHILE CCARTONTYP+Style+cTyp+cShipNo+PIKTKT = m.CCARTONTYP+m.Style FOR cTyp = 'X'
        FOR lnC = 1 TO lnMaxSzNo 
          lcC = ALLTRIM(STR(lnC))
          REPLACE QTY&lcC. WITH m.QTY&lcC.
        ENDFOR 
        REPLACE Total WITH m.Total
        *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
        REPLACE NCRTN WITH NCRTN + 1 
        IF EMPTY(laCartons[1])
	 	  laCartons[1] = m.cCartonNo
	    ELSE
    	  DIMENSION laCartons[ALEN(laCartons,1)+1]  
	      laCartons[ALEN(laCartons,1)]  = m.cCartonNo
	    ENDIF
      ENDIF 
    ENDIF   
  ELSE
    SELECT(lcTempCrt)
    REPLACE NCRTN WITH NCRTN + 1
    IF ASCAN(laCartons,m.cCartonNo,1) = 0
	    =SEEK(m.CCARTONTYP+m.Style)
      LOCATE REST WHILE CCARTONTYP+Style+cTyp+cShipNo+PIKTKT = m.CCARTONTYP+m.Style FOR cTyp = 'X'
      IF FOUND()
        *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
        FOR lnC = 1 TO lnMaxSzNo 
          lcC = ALLTRIM(STR(lnC))
          STORE 0 TO m.QTY&lcC.
          SELECT(lcTempCrt)
          =SEEK(m.CCARTONTYP+m.Style)      
          SUM QTY&lcC.,TOTAL REST WHILE CCARTONTYP+Style+cTyp+cShipNo+PIKTKT = m.CCARTONTYP+m.Style FOR cTyp <> 'X' TO m.QTY&lcC.,m.total
        ENDFOR 
        SELECT(lcTempCrt)
        =SEEK(m.CCARTONTYP+m.Style)      
        LOCATE REST WHILE CCARTONTYP+Style+cTyp+cShipNo+PIKTKT = m.CCARTONTYP+m.Style FOR cTyp = 'X'
        FOR lnC = 1 TO lnMaxSzNo 
          lcC = ALLTRIM(STR(lnC))
          REPLACE QTY&lcC. WITH m.QTY&lcC.
        ENDFOR 
        REPLACE Total WITH m.Total
       *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
        REPLACE NCRTN WITH NCRTN + 1
      ENDIF 
      SELECT 'POCRTNMF'
      
      IF EMPTY(laCartons[1])
	    laCartons[1] = m.cCartonNo
	  ELSE
    	DIMENSION laCartons[ALEN(laCartons,1)+1]  
	    laCartons[ALEN(laCartons,1)]  = m.cCartonNo
	  ENDIF
    ENDIF   
  ENDIF 
  lcStyleType = STYLE+CCARTONTYP 
ENDSCAN 
SELECT (lcTempCrt)
LOCATE 
lcBrFields = "cTypeCrt:H= 'Carton Type':R,ClrScale:H= 'Col-Sc':R,"
FOR lnR = 1 TO lnMaxSzNo 
  lcR = ALLTRIM(STR(lnR))
  lcBrFields = lcBrFields +  "Qty"+lcR +":H= '"+&lcScaleTable..Scl&lcR.+"':R,"
ENDFOR   
lcBrFields = lcBrFields +  "Total:H= 'Total':R,NCRTN:H= 'No. Of Ctns':R,cShipNo:H= 'Ship No.':R,PIKTKT:H= 'PIKTKT':R,"+;
						   "nLength:H= 'Length':R,Width:H= 'Width':R,nDepth:H= 'Height':R"	

llReturn = ariabrow('','Carton Details', .F., .F., .F., .F.,'','','Style','laTemp')            && call ariabrow to select or cancel
SELECT (lcAlias )

*!*************************************************************
*! Name      : lfARINVSAV  
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/25/2008
*! Purpose   : function to Update Invoice Field when invoice created 
*!*************************************************************
FUNCTION lfARINVSAV  
IF !OrdLine.Picked 
  RETURN 
ENDIF 
lcAlias = SELECT(0)
IF !USED('POCRTNMF')
  =gfOpenTable('POCRTNMF','POPIKTKT')
ENDIF
SELECT POCRTNMF
=gfSeek(Ordline.piktkt)
SCAN REST WHILE PIKTKT = Ordline.piktkt FOR Style = Ordline.Style AND EMPTY(Invoice)
  =gfReplace('Invoice with lcInvNo')
ENDSCAN
gfTableUpdate()
SELECT(lcAlias )

*!*************************************************************
*! Name      : lfCrtTmpFile
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/26/2008
*! Purpose   : function to Create Tmp File to browse from in Style screen
*!*************************************************************
FUNCTION lfCrtTmpFile
DIMENSION laFileStruct[lnMaxSzNo +13,4] 

laFileStruct[1,1]  = 'CCARTONTYP '
laFileStruct[1,2]  = 'C'
laFileStruct[1,3]  = 6
laFileStruct[1,4]  = 0

laFileStruct[2,1]  = 'ClrScale'
laFileStruct[2,2]  = 'C'
laFileStruct[2,3]  = 19
laFileStruct[2,4]  = 0

laFileStruct[3,1]  = 'nLength'
laFileStruct[3,2]  = 'N'
laFileStruct[3,3]  = 8
laFileStruct[3,4]  = 2

laFileStruct[4,1]  = 'ndepth'
laFileStruct[4,2]  = 'N'
laFileStruct[4,3]  = 8
laFileStruct[4,4]  = 2


laFileStruct[5,1]  = 'width'
laFileStruct[5,2]  = 'C'
laFileStruct[5,3]  = 6
laFileStruct[5,4]  = 0

laFileStruct[6,1]  = 'Weight'
laFileStruct[6,2]  = 'N'
laFileStruct[6,3]  = 8
laFileStruct[6,4]  = 2

laFileStruct[7,1]  = 'TOTAL'
laFileStruct[7,2]  = 'N'
laFileStruct[7,3]  = 10
laFileStruct[7,4]  = 0


laFileStruct[8,1]  = 'NCRTN'
laFileStruct[8,2]  = 'N'
laFileStruct[8,3]  = 8
laFileStruct[8,4]  = 0

laFileStruct[9,1]  = 'CShipNO'
laFileStruct[9,2]  = 'C'
laFileStruct[9,3]  = 6
laFileStruct[9,4]  = 0

laFileStruct[10,1]  = 'PIKTKT'
laFileStruct[10,2]  = 'C'
laFileStruct[10,3]  = 6
laFileStruct[10,4]  = 0

laFileStruct[11,1]  = 'Style'
laFileStruct[11,2]  = 'C'
laFileStruct[11,3]  = 19
laFileStruct[11,4]  = 0

laFileStruct[12,1]  = 'cTyp'
laFileStruct[12,2]  = 'C'
laFileStruct[12,3]  = 1
laFileStruct[12,4]  = 0


laFileStruct[13,1]  = 'cTypeCrt'
laFileStruct[13,2]  = 'C'
laFileStruct[13,3]  = 6
laFileStruct[13,4]  = 0

lnT = 14
FOR lnC = 1 TO lnMaxSzNo 
  lcC = ALLTRIM(STR(lnC))
  laFileStruct[lnT,1]  = 'Qty'+lcC 
  laFileStruct[lnT,2]  = 'N'
  laFileStruct[lnT,3]  = 9
  laFileStruct[lnT,4]  = 0
  lnT = lnT + 1
ENDFOR 
=gfCrtTmp(lcTempCrt  ,@laFileStruct,[CCARTONTYP+Style+cTyp+cShipNo+PIKTKT],lcTempCrt)


DIMENSION laSclFile[lnMaxSzNo +2 ,4]

laSclFile[1,1] = 'Style'
laSclFile[1,2] = 'C'
laSclFile[1,3] = lnStyClrLen
laSclFile[1,4] = 0

laSclFile[2,1] = 'PO'
laSclFile[2,2] = 'C'
laSclFile[2,3] = 6
laSclFile[2,4] = 0

lnCnt = 3

FOR lnI = 1 TO lnMaxSzNo 
  laSclFile[lnCnt ,1] = 'Scl'+ALLTRIM(STR(lnI))
  laSclFile[lnCnt ,2] = 'C'
  laSclFile[lnCnt ,3] = 5
  laSclFile[lnCnt ,4] = 0
  lnCnt = lnCnt + 1
ENDFOR 

=gfCrtTmp(lcScaleTable,@laSclFile,[Style],lcScaleTable  )

*!*************************************************************
*! Name      : lfalloAll
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/26/2008
*! Purpose   : function to Allocate all
*!*************************************************************
FUNCTION lfalloAll
PARAMETERS loChldForm
lcSrcFile =loChldForm.loParentForm.lcStylePik
lnRecnum = RECNO(lcSrcFile)

REPLACE ALL nCartAll WITH nCartAll + nCartAvl,;
			nQtyAll  WITH nCartAll * QtyPerCrt ,;
	        nCartAvl WITH nCartAvl - nCartAll FOR nCartAvl > 0 IN (lcSrcFile)
	        
IF BETWEEN(lnRecnum ,1,RECCOUNT(lcSrcFile ))
  GO RECORD lnRecnum IN (lcSrcFile )
ENDIF 