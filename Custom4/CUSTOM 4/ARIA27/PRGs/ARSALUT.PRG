*:***************************************************************************
*: Program file  : ARSALUT
*: Program desc. : Salutation program
*: For screen    : ARSALUT.SCX
*:        System : Aria Advantage Series.
*:        Module : Accounts Receiviable/Payable (AR/AP)
*:        Date   : 10/29/1999
*:     Developer : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Parameters    : Base File name
*:***************************************************************************
*: Example : DO ARSALUT -- E301119,1 Customer/Vendor Letters
*:***************************************************************************
*: Modifications :
*: B607389,1 08/05/2003 Fix Bugs related to E302174
*:***************************************************************************
PARAMETERS lcUpdtFile
PRIVATE lnFnAlis,lcUpdtFile,lcSalCase
PRIVATE lcIdStat,lcRestStat,lcMastStat
PRIVATE laTopSalut,laBotSalut,m.cSalutId,m.cSalDefa,m.cSalDesc,m.csalutpos,lcTmpSalut,lcStatTag
PRIVATE lnTopSalut,lnBotSalut,llOkPresed,llSalDefa
lnFnAlis = SELECT(0)
IF TYPE("lcUpdtFile") $ "UL"
  lcUpdtFile = IIF(gcAct_Appl="AR","CUSTOMER","APVENDOR")
ENDIF  
=gfOpenFile(gcDataDir+"SALUTES","Saluteid","SH")  && Open Salutation table.
*B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
lcIdType = IIF(lcUpdtFile = "APVENDOR","V","C")
*B607389,1 ALB Fix Bugs Relate to E302174 [END]


DIMENSION laTopSalut[1] , laBotSalut[1]
STORE '' TO laTopSalut,laBotSalut,m.cSalutId,m.cSalDesc,m.cSalDefa,;
            lcTmpSalut,m.csalutpos,lcSalCase

STORE "DISABLE" TO lcIdStat,lcRestStat
STORE 1 TO lnTopSalut,lnBotSalut

lcTmpSalut = gfTempName()
lcStatTag  = gfTempName()
lcSalDefa  = gfTempName()

*B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
*SELECT *,"S" AS cSalStatu FROM SALUTES ;
  ORDER BY csalutpos, csalutid;
  INTO TABLE (gcWorkDir+lcTmpSalut)

SELECT *,"S" AS cSalStatu FROM SALUTES ;
  WHERE CIDentify = lcIdType OR IIF(lcIdType="C",EMPTY(CIDentify),.T.);
  ORDER BY csalutpos, csalutid;
  INTO TABLE (gcWorkDir+lcTmpSalut)
*B607389,1 ALB Fix Bugs Relate to E302174 [END]

INDEX ON cSalStatu TAG (lcStatTag) ;
   OF (gcWorkDir+lcTmpSalut+".CDX") FOR cSalStatu <> "S"

*B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
*INDEX ON cSalDefa+csalutpos+csalutid TAG (lcSalDefa) ;
   OF (gcWorkDir+lcTmpSalut+".CDX") FOR (!DELETED() AND cSalStatu<> "D")
*INDEX ON csalutpos+csalutid TAG (lcTmpSalut) ;
   OF (gcWorkDir+lcTmpSalut+".CDX") FOR (!DELETED() AND cSalStatu<> "D")

INDEX ON cSalDefa+csalutpos+CIDentify+csalutid TAG (lcSalDefa) ;
   OF (gcWorkDir+lcTmpSalut+".CDX") FOR (!DELETED() AND cSalStatu<> "D")
INDEX ON csalutpos+CIDentify+csalutid TAG (lcTmpSalut) ;
   OF (gcWorkDir+lcTmpSalut+".CDX") FOR (!DELETED() AND cSalStatu<> "D")
*B607389,1 ALB Fix Bugs Relate to E302174 [END]

SET ORDER TO (lcTmpSalut) IN (lcTmpSalut)

*B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
*SELECT DISTINCT csalutid + ' - ' + cSalDesc;
  FROM SALUTES;
  WHERE csalutpos = "T";
  ORDER BY csalutid;
  INTO ARRAY laTopSalut
*SELECT DISTINCT csalutid + ' - ' + cSalDesc;
  FROM SALUTES;
  WHERE csalutpos = "B";
  ORDER BY csalutid;
  INTO ARRAY laBotSalut

SELECT DISTINCT csalutid + ' - ' + cSalDesc;
  FROM SALUTES;
  WHERE csalutpos = "T" AND (CIDentify = lcIdType OR IIF(lcIdType="C",EMPTY(CIDentify),.T.));
  ORDER BY csalutid;
  INTO ARRAY laTopSalut

SELECT DISTINCT csalutid + ' - ' + cSalDesc;
  FROM SALUTES;
  WHERE csalutpos = "B" AND (CIDentify = lcIdType OR IIF(lcIdType="C",EMPTY(CIDentify),.T.));
  ORDER BY csalutid;
  INTO ARRAY laBotSalut
*B607389,1 ALB Fix Bugs Relate to E302174 [END]

IF EMPTY(laTopSalut[1])
  DIMENSION laTopSalut[2]
ELSE
  DIMENSION laTopSalut[ALEN(laTopSalut,1) + 2]
  =AINS(laTopSalut,1)
ENDIF
=AINS(laTopSalut,1)

IF EMPTY(laBotSalut[1])
  DIMENSION laBotSalut[2]
ELSE
  DIMENSION laBotSalut[ALEN(laBotSalut,1) + 2]
  =AINS(laBotSalut,1)
ENDIF
=AINS(laBotSalut,1)

STORE "       Use Default Salutation" TO laTopSalut[1] , laBotSalut[1]
STORE "       Edit Salutation" TO laTopSalut[2] , laBotSalut[2]

*-- Default values to previouse selected saluataions.
lnTopSalut = ASCAN(laTopSalut,&lcUpdtFile..cTopSalut)
lnBotSalut = ASCAN(laBotSalut,&lcUpdtFile..cBotSalut)

lnTopSalut = IIF(lnTopSalut=0,1,lnTopSalut)
lnBotSalut = IIF(lnBotSalut=0,1,lnBotSalut)

STORE .F. TO llOkPresed , llSalDefa

lcMastStat = IIF(laScrMode[2],"DIS","EN")+"ABLE"
DO (gcScrDir+"ARSALUT.SPX")

IF USED(lcTmpSalut)
  =llOkPresed AND lfSavSalut()   && Saving process.
  USE IN (lcTmpSalut)
  ERASE (gcWorkDir+lcTmpSalut+".DBF")
  ERASE (gcWorkDir+lcTmpSalut+".CDX")
ENDIF

SELECT (lnFnAlis)  && Restore alias.
*-- end of Main calling salutation function.

*!*************************************************************
*! Name      : lfvNewSal
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 10/30/1999
*! Purpose   : Add new salutation.
*!*************************************************************
*
FUNCTION lfvNewSal
SELECT (lcTmpSalut)
IF !SEEK(lcSalCase+SPACE(6))
  APPEND BLANK
  *B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
  *REPLACE csalutpos WITH lcSalCase, cSalStatu WITH "A"
  REPLACE csalutpos WITH lcSalCase, cSalStatu WITH "A", CIDentify WITH lcIdType
  *B607389,1 ALB Fix Bugs Relate to E302174 [END]
ENDIF
SCATTER MEMVAR

SET ORDER TO (lcSalDefa)
*B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
*llSalDefa = !SEEK("D"+lcSalCase)
llSalDefa = !SEEK("D"+lcSalCase+lcIdType)
*B607389,1 ALB Fix Bugs Relate to E302174 [END]

SET ORDER TO (lcTmpSalut)

*B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
*=SEEK(lcSalCase+SPACE(6))
=SEEK(lcSalCase+lcIdType+SPACE(6))
*B607389,1 ALB Fix Bugs Relate to E302174 [END]

REPLACE cSalDefa WITH IIF(llSalDefa,"D"," ")

IF !EMPTY(laSaluts[1])
  DIMENSION laSaluts[ALEN(laSaluts)+1]
ENDIF

laSaluts[ALEN(laSaluts)] = SPACE(6)
=ASORT(laSaluts)
lnSaluts = ASCAN(laSaluts,m.cSalutId)

=lfEditStat()  && Add new salutation.
*-- end of lfvNewSal.

*!*************************************************************
*! Name      : lfvRemSal
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 10/30/1999
*! Purpose   : Remove a salutation.
*!*************************************************************
*
FUNCTION lfvRemSal
PRIVATE lcMesgPos
lcMesgPos = IIF(lcSalCase="T","Opening","Concluding")
*Message : 40165 => You are about to remove XXX salutation : YYY , are you sure?
*Button  : <Yes> <No>
IF gfModalGen('QRM40165B40000','Dialog',lcMesgPos+"|"+ALLTRIM(m.cSalDesc)) = 1
  SELECT (lcTmpSalut)
  IF !EMPTY(cSalutId)
    lnToErase = lnSaluts + 2
    
    IF ALEN(laSaluts,1) = 1
      laSaluts = SPACE(6)
    ELSE
      =ADEL(laSaluts,lnSaluts)
      IF TYPE("laSaluts[lnSaluts]") = "L"
        lnSaluts = lnSaluts - 1
      ENDIF
      DIMENSION laSaluts[ALEN(laSaluts)-1]
    ENDIF

    lcArryName = IIF(csalutpos = "T","laTopSalut","laBotSalut")
    =ADEL(&lcArryName,lnToErase)
    DIMENSION &lcArryName[ALEN(&lcArryName)-1]
  ENDIF

  IF cSalStatu = "A"
    DELETE
  ELSE
    REPLACE cSalStatu WITH "D"
  ENDIF
  
  =lfSelSal()  && Select new salutation

ENDIF  
*-- end of lfvRemSal.

*!*************************************************************
*! Name      : lfvSalut
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 10/30/1999
*! Purpose   : Salutation validation.
*!*************************************************************
*
FUNCTION lfvSalut
IF MDOWN()
  RETURN
ENDIF

IF SEEK(m.csalutpos+m.cSalutId,lcTmpSalut)
  *Message : "Salutation " + m.cSalutId + " is found in data file"
  *Button  : Ok
  =gfModalGen('TRM40041B00000','Dialog',"Salutation  "+": "+m.cSalutId)  
  m.cSalutId = SPACE(6)
  SHOW GET m.cSalutId
  _CUROBJ = _CUROBJ
  RETURN
ELSE
  SELECT (lcTmpSalut)
  SET ORDER TO (lcTmpSalut)
  *B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
  *=SEEK(m.csalutpos+SPACE(6))
  =SEEK(m.csalutpos+lcIdType+SPACE(6))
  *B607389,1 ALB Fix Bugs Relate to E302174 [END]
  REPLACE cSalutId WITH PADR(m.cSalutId,6)

ENDIF

laSaluts[1] = m.cSalutId + ' - '
=ASORT(laSaluts)
lnSaluts = ASCAN(laSaluts,m.cSalutId)

IF m.csalutpos = "T"
  DIMENSION laTopSalut[ALEN(laTopSalut)+1]
  lnTopSalut = lnSaluts + 2
  =AINS(laTopSalut,lnTopSalut)
  laTopSalut[lnTopSalut] = laSaluts[lnSaluts]
ELSE
  DIMENSION laBotSalut[ALEN(laBotSalut)+1]
  lnBotSalut = lnSaluts + 2
  =AINS(laBotSalut,lnBotSalut)
  laBotSalut[lnBotSalut] = laSaluts[lnSaluts]
ENDIF

=lfEditStat(.T.)
_CUROBJ = OBJNUM(m.cSalDesc)
*-- end of lfvSalut.

*!*************************************************************
*! Name      : lfvSalDesc
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 10/30/1999
*! Purpose   : Description validation.
*!*************************************************************
*
FUNCTION lfvSalDesc
SELECT (lcTmpSalut)
REPLACE cSalDesc WITH m.cSalDesc , cSalStatu WITH IIF(cSalStatu="A",cSalStatu,"M")

laSaluts[lnSaluts] = PADR(laSaluts[lnSaluts],9) + m.cSalDesc
SHOW GET lnSaluts

IF m.csalutpos = "T"
  laTopSalut[lnTopSalut] = laSaluts[lnSaluts]
ELSE
  laBotSalut[lnBotSalut] = laSaluts[lnSaluts]
ENDIF
*-- end of lfvSalDesc.

*!*************************************************************
*! Name      : lfSavSalut
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 10/30/1999
*! Purpose   : Save Salutaions.
*!*************************************************************
*
FUNCTION lfSavSalut
*-- update salutation file.
SELECT (lcTmpSalut)
SET ORDER TO (lcStatTag)
SCAN

  IF cSalStatu = "D"

    IF SEEK(cSalutId + csalutpos , "SALUTES")
      SELECT SALUTES
      =RLOCK()
      BLANK
      DELETE
      UNLOCK
    ENDIF

  ELSE

    IF !EMPTY(cSalutId)
      SCATTER MEMVAR
      IF SEEK(cSalutId + csalutpos , "SALUTES")
        SELECT SALUTES
        =RLOCK()
        GATHER MEMVAR
        UNLOCK
      ELSE
        INSERT INTO SALUTES FROM MEMVAR
      ENDIF
      =gfAdd_Info("SALUTES")
    ENDIF

  ENDIF  
ENDSCAN

*-- update Transaction file.
SELECT (lcUpdtFile)
=RLOCK(lcUpdtFile)
REPLACE cTopSalut WITH PADR(laTopSalut[lnTopSalut],6),;
        cBotSalut WITH PADR(laBotSalut[lnBotSalut],6)
UNLOCK
*-- end of lfSavSalut.

*!*************************************************************
*! Name      : lfvSalPop
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 10/30/1999
*! Purpose   : Salutaions POPUP Validation.
*!*************************************************************
*
FUNCTION lfvSalPop
PRIVATE lcWhatSal
lcWhatSal = SYS(18)

*-- if user want to edit salutation
IF EVALUATE(lcWhatSal) = 2
  PRIVATE lcMastAray
  lcMastAray = "la" + SUBSTR(lcWhatSal,3)
  lcSalTitle = IIF("TOP"$lcWhatSal," Opening " , " Concluding ") + "Salutation "
  lnSaluts   = 1
  IF ALEN(&lcMastAray,1) = 2
    DIMENSION laSaluts[1]
    laSaluts = ""
  ELSE
    DIMENSION laSaluts[ALEN(&lcMastAray,1)-2]
    =ACOPY(&lcMastAray,laSaluts,3,-1,1)
  ENDIF  
  =lfDefaSal()
  IF !EMPTY(m.cSalutId)
    lcRestStat = "DISAB"
  ENDIF  
  =lfEditStat(.T.)
  DO (gcScrDir+"AREDSAL.SPX")
  
  IF lcSalCase = "T"
    SHOW GET lnTopSalut
  ELSE
    SHOW GET lnBotSalut
  ENDIF

ENDIF
*-- end of lfvSalPop.

*!*************************************************************
*! Name      : lfDefaSal
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 10/30/1999
*! Purpose   : Refresh salutation objects
*!*************************************************************
*
FUNCTION lfDefaSal
lcSalCase = IIF("TOP"$lcWhatSal,"T","B")

SELECT (lcTmpSalut)
SET ORDER TO (lcSalDefa)
*B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
*llSalDefa = SEEK("D"+lcSalCase)
llSalDefa = SEEK("D"+lcSalCase+lcIdType)
*B607389,1 ALB Fix Bugs Relate to E302174 [END]

SET ORDER TO (lcTmpSalut)
*-- if you find default value.
IF !llSalDefa
  *B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
  *= SEEK(lcSalCase)
  = SEEK(lcSalCase+lcIdType)
  *B607389,1 ALB Fix Bugs Relate to E302174 [END]
  
ENDIF
SCATTER MEMVAR
lnSaluts = ASCAN(laSaluts,m.cSalutId)
*-- end of lfDefaSal.

*!*************************************************************
*! Name      : lfEditStat
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 10/30/1999
*! Purpose   : Refresh salutation objects
*!*************************************************************
*
FUNCTION lfEditStat
PARAMETERS llEdit  && .T. to edit, else to Add new
STORE "ABLE" TO lcIdStat,lcRestStat
IF llEdit
  lcIdStat = "DIS" + lcIdStat
  lcRestStat = IIF(EMPTY(m.cSalutId),"DIS","EN") + lcRestStat
ELSE
  lcIdStat = "EN" + lcIdStat
  lcRestStat = "DIS" + lcRestStat
  _CUROBJ = OBJNUM(m.cSalutId)
ENDIF
SHOW GET m.cSalutId &lcIdStat
SHOW GET m.cSalDesc &lcRestStat
SHOW GET llSalDefa  &lcRestStat
SHOW GET lnSaluts   &lcRestStat
SHOW GET pbRemSal   &lcRestStat
*-- end of lfEditStat.

*!*************************************************************
*! Name      : lfSelSal
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 10/30/1999
*! Purpose   : Select salutation popup validation.
*!*************************************************************
*
FUNCTION lfSelSal
*B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
*=SEEK(lcSalCase+PADR(laSaluts[lnSaluts],6))
=SEEK(lcSalCase+lcIdType+PADR(laSaluts[lnSaluts],6))
*B607389,1 ALB Fix Bugs Relate to E302174 [END]
SCATTER MEMVAR
llSalDefa = (m.cSalDefa = "D")
=lfEditStat(.T.)
IF lcSalCase = "T"
  lnTopSalut = IIF(EMPTY(laSaluts),1,lnSaluts + 2)
ELSE
  lnBotSalut = IIF(EMPTY(laSaluts),1,lnSaluts + 2)
ENDIF
*-- end of lfSelSal.

*!*************************************************************
*! Name      : lfAsDefa
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 10/30/1999
*! Purpose   : Set default salutation.
*!*************************************************************
*
FUNCTION lfAsDefa

IF llSalDefa
  PRIVATE lcCurrOrd , lcCurrKey , lcMyKey
  SELECT (lcTmpSalut)
  lcCurrOrd = ORDER()
  lcCurrKey = EVALUATE(KEY())

  SET ORDER TO (lcSalDefa)
  lcMyKey = EVALUATE(KEY())

  IF SEEK("D"+lcSalCase) AND lcMyKey <> EVALUATE(KEY())
    *Message : 40166 => XXX was set as default, Would you want to set active YYY as default?
    *Button  : <Yes> <No>
    lcDefaMsg = IIF(lcSalCase="T","Opening ", "Concluding ") + "salutation (" + ALLTRIM(cSalutId) + ")"
    IF gfModalGen('QRM40166B40000','Dialog',lcDefaMsg+"|" + "salutation") = 1
      REPLACE cSalDefa WITH " " , cSalStatu WITH IIF(cSalStatu="A",cSalStatu,"M")
    ELSE
      llSalDefa = .F.
      SHOW GET llSalDefa
    ENDIF
  ENDIF

  SET ORDER TO &lcCurrOrd
  =SEEK(lcCurrKey)

ENDIF
REPLACE cSalDefa WITH IIF(llSalDefa,"D"," ") , cSalStatu WITH IIF(cSalStatu="A",cSalStatu,"M")
*-- end of lfAsDefa.