*:***************************************************************************
*: Program file  : RMCMEMFG
*: Program desc. : Credit Memo form FOR KAZROUNI(F&G)
*! Date          : 12/19/1999
*: System        : Aria Advantage Series.
*: Module        : Return Merchandise (RM)
*: Developer     : Ramy Mabrouk (RAMY)
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO RMCMEMFG
*:***************************************************************************
*: This Report Program is due to ...C200108
*:***************************************************************************
*: Modificatin:
*B603594,1 RAMY 23/04/2000 the price does not printed if the price has multible colors
*:***************************************************************************
*-- Cursor to hold the logo BitMap

llUseTax = .F.
*-- Creat the main temp file
CREATE CURSOR (lclines)  (CrMemo C(6) , Account C(5)  , STYLE C(6)    ,Scale C(1)     ,;
   						  Qty1 N(6)    , Qty2 N(6)     , Qty3 N(6)     , Qty4 N(6)     ,;
						  Qty5 N(6)    , Qty6 N(6)     , Qty7 N(6)     , Qty8 N(6)     ,;
						  Qty9 N(6)    , Qty10 N(6)    , Qty11 N(6)    , ;
						  Price1 N(9,2), Price2 N(9,2) , Price3 N(9,2) , Price4 N(9,2) ,;
						  Price5 N(9,2), Price6 N(9,2) , Price7 N(9,2) , Price8 N(9,2) ,;
						  Price9 N(9,2), Price10 N(9,2), Price11 N(9,2), STORE C(8)    ,;
						  DESC C(20)    , TotQty N(9) ,  lChange L(1)  , TotP N(9,2) )
INDEX ON CrMemo + Scale + STYLE TAG CrMemo OF (gcWorkDir+lclines)
ZAP
CREATE CURSOR (lcScale) (scale C(1) ,SZ1 C(5), SZ2 C(5), SZ3 C(5), SZ4 C(5), SZ5 C(5),;
                           SZ6 C(5), SZ7 C(5), SZ8 C(5), SZ9 C(5), SZ10 C(5), SZ11 C(5) )
INDEX ON SCALE TAG(lcScale) OF (gcWorkDir+lcScale)
DIMENSION laSizes[12]

*-- Set Some Relation
SELECT RETLINE
SET RELATION TO STYLE INTO STYLE

*-- Start the data collection
SELECT RETHDR
SET RELATION TO
lcScanExp = IIF(UPPER(lcRpExp)==".T.", "", "FOR " + lcRpExp)
SCAN &lcScanExp
  IF SEEK(RETHDR.CrMemo ,'RETLINE')
    SELECT RETLINE
    SCAN WHILE CrMemo = RETHDR.CrMemo FOR Cret_TrnCd = '2'
      *-- check if scale changed, insert new record in scale file
      IF !SEEK(LEFT(STYLE.SCALE,1) ,lcScale)
        laSizes = SPACE(0)
        laSizes[1] = LEFT(STYLE.SCALE,1)
        lnSzCnt = 2
        SELECT SCALE
        =SEEK('S'+LEFT(STYLE.SCALE,1))
        *-- Scale is 11 sizes in 11 record in the master scale file, get them in one record in the temporary scale file
        SCAN WHILE TYPE+LEFT(SCALE,1) = 'S'+LEFT(STYLE.SCALE,1) .AND. lnSzCnt <=12
          FOR lnC = 1 TO SCALE.CNT
            lcC = ALLTRIM(STR(lnC))
            lnPos = ASCAN(laSizes,SCALE.SZ&lcC)
            IF lnPos = 0
              laSizes[lnSzCnt] = SCALE.SZ&lcC
              lnSzCnt = lnSzCnt + 1
            ENDIF
          ENDFOR
        ENDSCAN
        INSERT INTO (lcScale) FROM ARRAY laSizes
      ELSE
        SELECT (lcScale)
        SCATTER FIELDS SCALE,SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,SZ9,SZ10,SZ11 TO laSizes
      ENDIF
      
      *-- Add one record per main scale and get its other scales in the same record
      IF !EOF('STYLE') .AND. !SEEK( RETLINE.CrMemo+LEFT(Style.scale,1)+LEFT(STYLE.Style,6) , lcLines )
        INSERT INTO (lcLines) (CrMemo , Scale , STYLE,Account , lChange , DESC ) VALUES ;
        ( RETLINE.CrMemo, laSizes[1] , LEFT(STYLE.STYLE,6),RetHdr.account , .F. , STYLE.DESC )
      ENDIF
      =SEEK('S'+STYLE.SCALE,'SCALE')
      FOR lnCnt = 1 TO SCALE.Cnt
        lcCnt = STR(lnCnt,1)
        lnPos = ASCAN(laSizes,SCALE.Sz&lcCnt)
        IF lnPos # 0
          lnPos = lnPos - 1
          lcPos = ALLTRIM(STR(lnPos))
          SELECT (lcLines)
          *B603594,1 RAMY [start] the price is not printed if the price has multible colors
          *REPLACE Qty&lcPos    WITH Qty&lcPos + RetLine.Qty&lcCnt ,;
                  Price&lcPos  WITH IIF(EMPTY(RetLine.Qty&lcCnt) , 0 , RETLINE.Price)

          *REPLACE Qty&lcPos   WITH Qty&lcPos + RetLine.Qty&lcCnt ,;
                  Price&lcPos WITH IIF(EMPTY(RetLine.Qty&lcCnt) , IIF(!EMPTY(Price&lcPos) ,Price&lcPos , 0) , RETLINE.Price)

          REPLACE Qty&lcPos   WITH Qty&lcPos + RetLine.Qty&lcCnt
          IF !EMPTY(RetLine.Qty&lcCnt)
            REPLACE Price&lcPos WITH RETLINE.Price
          ENDIF
          *TotQty      WITH TotQty + Qty&lcPos ;
          *TotP        WITH TotP   + Price&lcPos
           
          *B603594,1 RAMY [end]

        ENDIF
      ENDFOR
    ENDSCAN
  ENDIF
ENDSCAN

SELECT (lcLines)
SCAN
  STORE 0 TO lnTotQty , lnTotP
  FOR lnC = 1 to 11
    lnC = ALLTRIM(STR(lnC))
    lnTotQty = lnTotQty + Qty&lnC
    lnTotP   = lnTotP   + (Qty&lnC * Price&lnC)
    lnC = VAL(lnC)
  ENDFOR
  REPLACE TotQty WITH lnTotQty , TotP WITH lnTotP
ENDSCAN

GO TOP
SET RELATION TO SCALE INTO (lcScale)

SELECT RETHDR
SET RELATION TO CrMemo INTO (lclines)
SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
SET SKIP TO (lclines)

IF SET('CENTURY') = 'OFF'
  SET CENTURY ON
ENDIF


*!*************************************************************
*! Name      : lfPrntUsr
*! Developer : RAMY MABROUK (RAMY)
*! Date      : 12/19/1999
*! Purpose   : Get the user name
*!*************************************************************
*! Called from : RMCMEMFG.FRX
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfPrntUsr
PARAMETERS lcDumy
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT SYUUSER
=SEEK(gcUser_Id , 'SYUUSER')
lcUsrDesc = ALLTRIM(SYUUSER.CUSR_NAME)

SELECT (lcAlias)
RETURN !EMPTY(lcUsrDesc)

*!*************************************************************
*! Name      : lfSetCnt
*! Developer : RAMY MABROUK (RAMY)
*! Date      : 12/19/1999
*! Purpose   : Sets the century setting to of after closing the report
*!*************************************************************
*! Called from : RMCMEMFG.FRX
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************

FUNCTION lfSetCnt
PARAMETERS lcDumy

IF SET('CENTURY') = 'ON'
  SET CENTURY OFF
ENDIF





*!*************************************************************
*! Name      : lfArabic
*! Developer : RAMY MABROUK (RAMY)
*! Date      : 24/1/20000
*! Purpose   : Function to get the descreption of the total in arbic words.
*!*************************************************************
*! Called from : RMCMEMFG.FRX
*!*************************************************************
*! Calls       : lfGetNO , lfPower
*!*************************************************************
*! Passed Parameters : NO  = the number will be change
*!                   : sex = the sex of the number. [Must have number]
*!                   :      1 > Male   , 2 > Female.
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfArabic
PARAMETERS lcDumy

PRIVATE lcAlias,lcLength,LnCOUNT,TempLength,lcString,lcAfter,lcTxt,lcGetNO




*-- NEW

*The DIMENSIONS of the arabic function and the initial values of it.[START]
DIMENSION First  [9,2]    && Hold "Ê«Õœ ","«À‰«‰ " ...
DIMENSION First1 [2,2]    && Hold "√Õœ ","«À‰« "
DIMENSION Secnd  [9,2]    && Hold "⁄‘—… ","⁄‘—Ê‰ "
DIMENSION Third  [9,1]    && Hold "„«∆… ", "„«∆ «‰ "
DIMENSION Parts  [4]
DIMENSION PartStr[5]

*Hold from 1 to 9 Male
First[1, 1] = "Ê«Õœ "
First[2, 1] = "«À‰«‰ "
First[3, 1] = "À·«À… "
First[4, 1] = "√—»⁄… "
First[5, 1] = "Œ„”… "
First[6, 1] = "” … "
First[7, 1] = "”»⁄… "
First[8, 1] = "À„«‰Ì… "
First[9, 1] = " ”⁄… "

*Hold from 1 to 9 Female
First[1, 2] = "Ê«Õœ… "
First[2, 2] = "«À‰ «‰ "
First[3, 2] = "À·«À "
First[4, 2] = "√—»⁄ "
First[5, 2] = "Œ„” "
First[6, 2] = "”  "
First[7, 2] = "”»⁄ "
First[8, 2] = "À„«‰ "
First[9, 2] = " ”⁄ "

*Hold from 1 to 2 Male
First1[1, 1] = "√Õœ "
First1[2, 1] = "«À‰« "


*Hold from 1 to 2 Female
First1[1, 2] = "≈ÕœÏ "
First1[2, 2] = "«À‰ « "

*Hold from 10,20,30 ... Male
Secnd[1, 1] = "⁄‘—… "
Secnd[2, 1] = "⁄‘—Ê‰ "
Secnd[3, 1] = "À·«ÀÊ‰ "
Secnd[4, 1] = "√—»⁄Ê‰ "
Secnd[5, 1] = "Œ„”Ê‰ "
Secnd[6, 1] = "” Ê‰ "
Secnd[7, 1] = "”»⁄Ê‰ "
Secnd[8, 1] = "À„«‰Ê‰ "
Secnd[9, 1] = " ”⁄Ê‰ "

*Hold from 10,20,30 ... Female
Secnd[1, 2] = "⁄‘— "
Secnd[2, 2] = "⁄‘—Ê‰ "
Secnd[3, 2] = "À·«ÀÊ‰ "
Secnd[4, 2] = "√—»⁄Ê‰ "
Secnd[5, 2] = "Œ„”Ê‰ "
Secnd[6, 2] = "” Ê‰ "
Secnd[7, 2] = "”»⁄Ê‰ "
Secnd[8, 2] = "À„«‰Ê‰ "
Secnd[9, 2] = " ”⁄Ê‰ "

*Hold from 100,200,300 ... Male , there is NO Female
Third[1, 1] = "„«∆… "
Third[2, 1] = "„«∆ «‰ "
Third[3, 1] = "À·«À„«∆… "
Third[4, 1] = "√—»⁄„«∆… "
Third[5, 1] = "Œ„”„«∆… "
Third[6, 1] = "” „«∆… "
Third[7, 1] = "”»⁄„«∆… "
Third[8, 1] = "À„«‰„«∆… "
Third[9, 1] = " ”⁄„«∆… "
*The DIMENSIONS of the arabic function and the initial values of it.[END]


*-- NEW END




lcAlias = ALIAS()
SELECT RetHdr


IF Gross_amt = 0              && Check if the value if [0] or not
  lcGetNO = "’›—"
  RETURN lcGetNO
ENDIF

lcString = ALLTRIM(STR(TotCredit,10,2))
STORE SPACE(0) TO Parts,PartStr,lcGetNO,lcAfter,CURRENCY
Sex     = 1
lcTxt   = ''
lnCOUNT = 0

lnLength = LEN(lcString)        && get the length of the number.
lnPos = ATC(".",lcString)       && assign the place of the dot in a variable

*«ﬁ”„ ”·”·… «·⁄œœ≈·Ï „«ﬁ»· «·›«’·… Ê„«»⁄œ «·›«’·…
IF lnPos > 0
  lcAfter     = RIGHT(lcString, lnLength - lnPos)
  lcString    = LEFT(lcString, lnPos - 1)
  lnLength    = LEN(lcString)
  lnpos_After = LEN(lcAfter)
ENDIF

*Ã“¡ «·⁄œœ ≈·Ï ”·«”· Õ—›Ì… „ƒ·›… „‰ À·«À Œ«‰«  ⁄‘—Ì… √Ê √ﬁ·
lnTempLength = lnLength
Parts[1]     = lcString

DO WHILE lnTempLength >= 3
  lnTempLength   = lnTempLength - 3
  lnCount        = lnCount + 1
  Parts[lnCount] = RIGHT(lcString, 3)
  lcString       = LEFT(lcString , lnTempLength)
ENDDO

Parts[lnCount + 1] = lcString

*«” œ⁄ «· «»⁄ «·›—⁄Ì Ê«Õ›Ÿ «·‰ «∆Ã ›Ì «·„’›Ê›…
FOR lnCount = 1 TO 4
  IF LEN(Parts[lnCount]) > 0
    PartStr[lnCount] = lfGetNO(Parts[lnCount], sex, lnCount , @First,@First1,@Secnd,@Third)
  ELSE
    EXIT
  ENDIF
ENDFOR

*Get the sub words in a one statment.
FOR lnCount = 5 TO 1 STEP -1
  IF LEN(PartStr[lnCount]) > 0
    IF lnCount > 1 .AND. LEN(PartStr[lnCount - 1]) > 0
      lcTxt = lcTxt + " " + PartStr[lnCount] + "Ê"
    ELSE
      IF ATC(".",lcString) + ATC(",",lcString) = 0
        IF LEN(lcString) >= 3
          IF VAL(RIGHT(lcString, 2)) > 10
            CURRENCY = "Ã‰ÌÂ« „’—Ì« "
          ELSE
            IF VAL(RIGHT(lcString, 2)) = 10
              CURRENCY = "Ã‰ÌÂ«  „’—Ì… "
            ELSE
              IF VAL(RIGHT(lcString, 2)) < 10 AND VAL(RIGHT(lcString, 2)) > 2
                CURRENCY = "Ã‰ÌÂ«  „’—Ì… "
              ELSE
                IF VAL(RIGHT(lcString, 2)) = 2
                  CURRENCY = "Ã‰ÌÂ« „’—Ì« "
                ELSE
                  IF VAL(RIGHT(lcString, 2)) = 1
                    CURRENCY = "Ã‰ÌÂ „’—Ì "
                  ELSE
                    IF LEN(lcString) = 2 AND VAL(lcString) > 10
                      CURRENCY = "Ã‰ÌÂ« „’—Ì« "
                    ELSE
                      IF LEN(lcString) = 2 AND VAL(lcString) = 10
                        CURRENCY = "Ã‰ÌÂ«  „’—Ì… "
                      ELSE
                        IF LEN(lcString) = 1 AND VAL(lcString) < 10 AND VAL(lcString) > 2
                          CURRENCY = "Ã‰ÌÂ«  „’—Ì… "
                        ELSE
                          IF LEN(lcString) = 1 AND VAL(lcString) = 2
                            CURRENCY = "Ã‰ÌÂ« „’—Ì« "
                          ELSE
                            IF LEN(lcString) = 1 AND VAL(lcString) = 1
                              CURRENCY = "Ã‰ÌÂ „’—Ì "
                            ENDIF
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      lcTxt = "›ﬁÿ" + lcTxt + " " + PartStr[LNCOUNT] + " " + "Ã‰Ì…" + " " +CURRENCY
      lcDub = OCCURS("Ã‰Ì",lcTxt)    &&for not dublicate the word "Ã‰Ì…"
      IF lcDub > 1
        lcTxt =STRTRAN(lcTxt  , "Ã‰Ì…" , "" )
        IF !OCCURS("Ã‰Ì" ,lcTxt) = 1
          lcTxt = lcTxt +"Ã‰Ì…" + " " +CURRENCY
        ENDIF
      ENDIF

    ENDIF
  ENDIF
ENDFOR

IF VAL(lcAfter) > 0
  IF !EMPTY(PARTS[1])
    lcTxt = lcTxt + "Ê" + lfGetNO(lcAfter, sex,0,@First,@First1,@Secnd,@Third)
  ELSE
    lcTxt = "›ﬁÿ" + " " + lcTxt +  lfGetNO(lcAfter, sex,0,@First,@First1,@Secnd,@Third)
  ENDIF
ENDIF
lcArabic = lcTxt + "·« €Ì—"         && Variable hold the description
SELECT (lcAlias)
RETURN !EMPTY(lcArabic)
************************************************************************
FUNCTION lfGetNO
PARAMETERS ns,Sex,Power,Frst,frst1,scnd,thrd

PRIVATE Lngth,InvSex
DIMENSION Indx[3]
DIMENSION TmpArray[3]

STORE SPACE(0) TO TmpArray
STORE 0 TO Indx

IF sex = 1
  InvSex = 2
ELSE
  InvSex = 1
ENDIF

*Solution for three numbers only.
Lngth = LEN(ns)

*the {1}
Indx[1] = VAL(SUBSTR(ns,Lngth,1))
IF Indx[1] = 0
  TmpArray[1] = ""
ELSE
  TmpArray[1] = frst(Indx[1],sex)
ENDIF

Lngth = Lngth - 1
IF Lngth > 0
  *the {10}
  Indx[2] = VAL(SUBSTR(ns, Lngth, 1))
  IF !EMPTY(TmpArray[1])
    IF Indx[2]    = 0
      TmpArray[2] = ""
    ELSE
      IF !EMPTY(PARTS[2])
        TmpArray[2] = scnd(Indx[2], InvSex)
      ELSE
        TmpArray[2] = scnd(Indx[2], InvSex) + "Ã‰ÌÂ«"
      ENDIF
    ENDIF
  ELSE
    IF Indx[2]    = 0
      TmpArray[2] = ""
    ELSE
      IF Indx[2] = 1
        IF !EMPTY(PARTS[2])
          TmpArray[2] = scnd(Indx[2], sex)
        ELSE
          TmpArray[2] = scnd(Indx[2], sex) + "Ã‰ÌÂ« "
        ENDIF
      ELSE
        IF !EMPTY(PARTS[2])
          TmpArray[2] = scnd(Indx[2], sex)
        ELSE
          TmpArray[2] = scnd(Indx[2], sex) + "Ã‰ÌÂ«"
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF Indx[2] > 1 AND !EMPTY(TmpArray[1])    &&«·⁄‘—«  „‰ 1 ≈·Ï  ”⁄…
    TmpArray[1] = TmpArray[1] + " Ê"
  ELSE
    IF (Indx[1] = 1) AND (Indx[2] = 1)        &&  '√Õœ ⁄‘—
      TmpArray[1] = frst1(1, sex)
    ELSE
      IF (Indx[1] = 2) AND (Indx[2] = 1)      &&' «À‰« ⁄‘—
        TmpArray[1] = frst1(2, sex)
      ENDIF
    ENDIF
  ENDIF
  Lngth = Lngth - 1
  IF Lngth > 0
    *the {100}
    Indx[3] = VAL(SUBSTR(ns, Lngth, 1))
    IF Indx[3]    = 0
      TmpArray[3] = ""
    ELSE
      IF !EMPTY(Indx[2])  AND EMPTY(Indx[1])
        TmpArray[3] = thrd(Indx[3]) + "Ê"
      ELSE
        IF EMPTY(Indx[2])  AND EMPTY(Indx[1])
          TmpArray[3] = thrd(Indx[3]) + "Ã‰Ì…"
        ELSE
          TmpArray[3] = thrd(Indx[3])
        ENDIF
      ENDIF
    ENDIF
    IF (Indx[3] > 0) AND (!EMPTY(TmpArray[1]) OR !EMPTY(TmpArray[1]))
      TmpArray[3] = TmpArray[3] + " Ê"
    ENDIF
  ELSE
    lcGetNO = lfPower()
    RETURN lcGetNO
  ENDIF
ELSE
  lcGetNO = lfPower()
  RETURN lcGetNO
ENDIF
*≈÷«›… ﬂ·„… «·„— »…(„∆…,√·›,...)Õ”» „— »… «·√—ﬁ«„
lcGetNO = lfPower()
RETURN lcGetNO

**********************************************************
FUNCTION lfPower

DO CASE
CASE Power = 0
  IF !EMPTY(TmpArray[1]) AND Indx[1] = 1 AND EMPTY(TmpArray[2]) AND lnpos_After = 1
    lcNO = scnd(Indx[1], sex) + "ﬁ—Ê‘ "
  ELSE
    IF !EMPTY(TmpArray[1]) AND Indx[1] > 1 AND EMPTY(TmpArray[2]) AND lnpos_After = 1
      lcNO = scnd(Indx[1], sex) + "ﬁ—‘« "
    ELSE
      IF EMPTY(TmpArray[1]) AND !EMPTY(TmpArray[2]) AND Indx[1] = 0 AND Indx[2] = 1 AND lnpos_After = 2
        lcNO = scnd(Indx[2], sex) + "ﬁ—Ê‘ "
      ELSE
        IF EMPTY(TmpArray[1]) AND !EMPTY(TmpArray[2]) AND Indx[2] = 0 AND Indx[2] > 1 AND lnpos_After = 2
          lcNO = scnd(Indx[2], sex) + "ﬁ—‘« "
        ELSE
          IF EMPTY(TmpArray[1]) AND !EMPTY(TmpArray[2]) AND Indx[1] = 0 AND Indx[2] > 1 AND lnpos_After = 2
            lcNO = scnd(Indx[2], sex) + "ﬁ—‘« "
          ELSE
            IF !EMPTY(TmpArray[1]) AND EMPTY(TmpArray[2]) AND Indx[1] = 1 AND Indx[2] = 0 AND lnpos_After = 2
              lcNO = frst(Indx[1], sex) + "ﬁ—‘ "
            ELSE
              IF !EMPTY(TmpArray[1]) AND EMPTY(TmpArray[2]) AND Indx[1] = 2 AND Indx[2] = 0 AND lnpos_After = 2
                lcNO = frst(Indx[1], sex) + "ﬁ—‘« "
              ELSE
                IF !EMPTY(TmpArray[1]) AND EMPTY(TmpArray[2]) AND Indx[1] > 2 AND Indx[1] <= 9 AND Indx[2] = 0 AND lnpos_After = 2
                  lcNO = frst(Indx[1], sex) + "ﬁ—Ê‘ "
                ELSE
                  IF !EMPTY(TmpArray[1]) AND !EMPTY(TmpArray[2]) AND Indx[1] = 1 AND Indx[2] = 1 AND lnpos_After = 2 ;
                      OR !EMPTY(TmpArray[1]) AND !EMPTY(TmpArray[2]) AND Indx[1] = 2 AND Indx[2] = 1 AND lnpos_After = 2
                    lcNO = frst1(Indx[1], sex) + scnd(Indx[2], InvSex) + "ﬁ—‘« "
                  ELSE
                    IF !EMPTY(TmpArray[1]) AND !EMPTY(TmpArray[2]) AND Indx[1] > 0 AND Indx[2] >= 2 AND lnpos_After = 2
                      lcNO = frst(Indx[1], sex) + "Ê " + scnd(Indx[2], InvSex) + "ﬁ—‘« "
                    ELSE
                      IF !EMPTY(TmpArray[1]) AND !EMPTY(TmpArray[2]) AND Indx[1] > 0 AND Indx[2] = 1 AND lnpos_After = 2
                        lcNO = frst(Indx[1], sex) + scnd(Indx[2], InvSex) + "ﬁ—‘« "
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF

CASE Power = 1
  lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2]
  IF !EMPTY(TmpArray[3]) AND !EMPTY(TmpArray[1]) AND EMPTY(TmpArray[2]) AND Indx[1] <= 2
    lcNO =    TmpArray[3] + TmpArray[1] + "Ã‰Ì…"
  ELSE
    IF !EMPTY(TmpArray[3]) AND !EMPTY(TmpArray[1]) AND EMPTY(TmpArray[2]) AND Indx[1] > 2
      lcNO =    TmpArray[3] + TmpArray[1] + "Ã‰ÌÂ« "
    ENDIF
  ENDIF
  IF LEN(Parts[2]) = 0 .AND. EMPTY(TmpArray[2]) .AND. EMPTY(TmpArray[3])
    lcNO =STRTRAN(lcNO  , "Ê«Õœ " , "Ã‰Ì… Ê«Õœ ")
    lcNO =STRTRAN(lcNO  , "«À‰«‰ ", "Ã‰ÌÂ«‰")
  ENDIF
  IF Indx[1] > 2 .AND. EMPTY(TmpArray[2]) .AND. EMPTY(TmpArray[3])
    lcNO = lcNO + "Ã‰ÌÂ« "
  ENDIF

CASE Power = 2
  IF (Indx[1] = 1) AND (Indx[2] = 0) AND (Indx[3] = 0)
    lcNO = " √·› "
  ELSE
    IF (Indx[1] = 2) AND (Indx[2] = 0) AND (Indx[3] = 0)
      lcNO = " √·›«‰ "
    ELSE
      IF (Indx[1] > 2) AND (Indx[2] = 0) AND (Indx[3] = 0)
        lcNO = TmpArray[1] + " ¬·«› "
      ELSE
        IF (Indx[1] = 0) AND (Indx[2] = 1) AND (Indx[3] = 0)
          lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " ¬·«› "
        ELSE
          IF (Indx[1] = 0) AND (Indx[2] = 0) AND (Indx[3] = 0)
            lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2]
          ELSE
            lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " √·› "
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF

CASE Power = 3
  IF (Indx[1] = 1) AND (Indx[2] = 0) AND (Indx[3] = 0)
    lcNO = " „·ÌÊ‰ "
  ELSE
    IF (Indx[1] = 2) AND (Indx[2] = 0) AND (Indx[3] = 0)
      lcNO = " „·ÌÊ‰«‰ "
    ELSE
      IF (Indx[1] > 2) AND (Indx[2] = 0) AND (Indx[3] = 0)
        lcNO = TmpArray[1] + " „·«ÌÌ‰ "
      ELSE
        IF (Indx[1] = 0) AND (Indx[2] = 1) AND (Indx[3] = 0)
          lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " „·«ÌÌ‰ "
        ELSE
          IF (Indx[1] = 0) AND (Indx[2] = 0) AND (Indx[3] = 0)
            lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2]
          ELSE
            lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " „·ÌÊ‰ "
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF

CASE Power = 4
  IF (Indx[1] = 1) AND (Indx[2] = 0) AND (Indx[3] = 0)
    lcNO = " „·Ì«— "
  ELSE
    IF (Indx[1] = 2) AND (Indx[2] = 0) AND (Indx[3] = 0)
      lcNO = " „·Ì«—«‰ "
    ELSE
      IF (Indx[1] > 2) AND (Indx[2] = 0) AND (Indx[3] = 0)
        lcNO = TmpArray[1] + " „·Ì«—«  "
      ELSE
        IF (Indx[1] = 0) AND (Indx[2] = 1) AND (Indx[3] = 0)
          lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " „·Ì«—«  "
        ELSE
          lcNO = TmpArray[3] + TmpArray[1] + TmpArray[2] + " „·Ì«— "
        ENDIF
      ENDIF
    ENDIF
  ENDIF

ENDCASE
RETURN lcNO


*!*************************************************************
*! Name      : lfFillLogo
*! Developer : RAMY MABROUK (RAMY)
*! Date      : 24/1/2000
*! Purpose   : Fill cursor with the company logo
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfFillLogo

PARAMETER lcDummy , llWhat
CREATE CURSOR (lcLogBmp) (gPic G)
APPEND BLANK
APPEND GENERAL gPIC FROM ( gcBmpHome+'FGLOGO.BMP')


*!*************************************************************
FUNCTION LFCHECK
PARAMETERS lcReturn , llWhat

lcReturn = ''
IF llWhat
  llCheck = .T.
ELSE
  llCheck = .F.
ENDIF
